"! <p class="shorttext synchronized" lang="en">AWS Bedrock Converse API Client</p>
CLASS zcl_llm_client_aws DEFINITION
  PUBLIC
  INHERITING FROM zcl_llm_client_base FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS get_client
      IMPORTING client_config   TYPE zllm_clnt_config
                provider_config TYPE zllm_providers
      RETURNING VALUE(result)   TYPE REF TO zif_llm_client_int
      RAISING   zcx_llm_validation
                zcx_llm_authorization.

    METHODS constructor
      IMPORTING client_config   TYPE zllm_clnt_config
                provider_config TYPE zllm_providers
      RAISING   zcx_llm_validation
                zcx_llm_authorization.

    METHODS zif_llm_client_int~chat REDEFINITION.

  PROTECTED SECTION.
    METHODS: get_http_client REDEFINITION,
      set_auth REDEFINITION,
      get_chat_endpoint REDEFINITION,
      build_request_json REDEFINITION,
      parse_message REDEFINITION,
      handle_http_response REDEFINITION.
  PRIVATE SECTION.
    TYPES: BEGIN OF aws_tool_use,
             input     TYPE /ui2/cl_json=>json,
             name      TYPE string,
             tooluseid TYPE string,
           END OF aws_tool_use.

    TYPES: BEGIN OF aws_content,
             text    TYPE string,
             tooluse TYPE aws_tool_use,
           END OF aws_content,
           aws_contents TYPE STANDARD TABLE OF aws_content WITH EMPTY KEY.

    TYPES: BEGIN OF aws_message,
             role    TYPE string,
             content TYPE aws_contents,
           END OF aws_message.

    TYPES: BEGIN OF aws_output,
             message TYPE aws_message,
           END OF aws_output.

    TYPES: BEGIN OF aws_usage,
             inputtokens  TYPE i,
             outputtokens TYPE i,
             totaltokens  TYPE i,
           END OF aws_usage.

    TYPES: BEGIN OF aws_response,
             stopreason TYPE string,
             output     TYPE aws_output,
             usage      TYPE aws_usage,
           END OF aws_response.

    DATA auth TYPE REF TO zcl_llm_client_aws_sigv4.
    DATA host TYPE string.
ENDCLASS.


CLASS zcl_llm_client_aws IMPLEMENTATION.
  METHOD constructor.
    super->constructor( client_config   = client_config
                        provider_config = provider_config ).
    initialize( ).
  ENDMETHOD.

  METHOD get_client.
    result = NEW zcl_llm_client_aws( client_config   = client_config
                                     provider_config = provider_config ).
  ENDMETHOD.

  METHOD get_chat_endpoint.
    result = |/{ client_config-provider_model }/converse|.
  ENDMETHOD.

  METHOD get_http_client.
    client = zcl_llm_http_client_wrapper=>get_client( client_config   = client_config
                                                       provider_config = provider_config ).
  ENDMETHOD.

  METHOD set_auth.
    DATA auth_value TYPE string.

    IF provider_config-auth_encrypted IS NOT INITIAL.
      DATA(llm_badi) = zcl_llm_common=>get_llm_badi( ).
      CALL BADI llm_badi->get_encryption_impl
        RECEIVING result = DATA(enc_class).
      auth_value = enc_class->decrypt( provider_config-auth_encrypted ).
    ENDIF.

    SPLIT auth_value AT ',' INTO DATA(access_key) DATA(secret_key) host DATA(region).
    " We cannot set any auth header now, we need to get an active token
    " right before every call instead. We just initialize the class.
    DATA credentials TYPE zcl_llm_client_aws_sigv4=>auth_credentials.
    credentials-access_key = access_key.
    credentials-secret_key = secret_key.
    credentials-region     = region.
    credentials-service    = 'bedrock'.
    auth = NEW zcl_llm_client_aws_sigv4( credentials = credentials ).
  ENDMETHOD.

  METHOD zif_llm_client_int~chat.
    " Set the aws headers including auth, rest handled by base class
    DATA(uri) = |/model/{ escape( val    = client_config-provider_model
                                  format = cl_abap_format=>e_url_full ) }/converse|.
    DATA request_time TYPE timestamp.
    GET TIME STAMP FIELD request_time.
    DATA(helper) = CONV string( request_time ).
    DATA(timestamp) = |{ helper(8) }T{ helper+8(6) }Z|.
    client->set_header( name  = 'x-amz-date'
                        value = timestamp ).
    client->set_header( name  = 'host'
                        value = host ).
    DATA(headers) = client->get_req_headers( ).
    LOOP AT headers ASSIGNING FIELD-SYMBOL(<header>).
      IF <header>-name <> 'x-amz-date' AND <header>-name <> 'host'.
        DELETE headers INDEX sy-tabix.
      ENDIF.
    ENDLOOP.

    DATA(json) = build_request_json( request->get_internal_request( ) ).
    DATA(token) = auth->get_auth_header( request_method = 'POST'
                                         request_uri    = uri
                                         headers        = headers
                                         payload        = json
                                         request_time   = request_time ).
    client->set_header( name  = 'Authorization'
                        value = token ) ##NO_TEXT.
    TRY.
        msg = msg + 1.
        client->set_url( get_chat_endpoint( ) ).
        GET TIME STAMP FIELD begin_request.
        DATA(resp) = client->communicate( request    = json
                                          session_id = id
                                          msg        = msg ).
        GET TIME STAMP FIELD end_request.
        response = handle_http_response( http_response = resp
                                         request       = request->get_internal_request( ) ).
        statistics->add( VALUE #( call_date     = sy-datum
                                  call_time     = sy-uzeit
                                  duration      = end_request - begin_request
                                  model         = client_config-model
                                  tokens_prompt = response-usage-prompt_tokens
                                  tokens_resp   = response-usage-completion_tokens
                                  tokens_total  = response-usage-total_tokens
                                  id            = id
                                  msg           = msg ) ).
      CATCH zcx_llm_http_error INTO DATA(http_error).
        response-error-error_text = http_error->if_message~get_text( ).
        response-error-retrieable = abap_false.
      CLEANUP.
        IF client IS BOUND.
          client->close_client( ).
        ENDIF.
    ENDTRY.
  ENDMETHOD.

  METHOD build_request_json.
    " Open content
    result = |\{"messages":[|.
    DATA first_line          TYPE abap_bool VALUE abap_true.

    " AWS handles system messages differently
    DATA non_system_messages TYPE zllm_msgs.
    DATA system_messages     TYPE zllm_msgs.

    LOOP AT request-messages ASSIGNING FIELD-SYMBOL(<message>).
      IF <message>-role = 'system'.
        APPEND <message> TO system_messages.
      ELSE.
        APPEND <message> TO non_system_messages.
      ENDIF.
    ENDLOOP.

    " Add messages
    LOOP AT non_system_messages INTO DATA(message).
      IF first_line = abap_true.
        result = |{ result }{ parse_message( message ) }|.
        first_line = abap_false.
      ELSE.
        result = |{ result },{ parse_message( message ) }|.
      ENDIF.
    ENDLOOP.
    result = |{ result }]|.

    " Add system messages
    IF lines( system_messages ) > 0.
      first_line = abap_true.
      result = |{ result },"system":[|.
      LOOP AT system_messages INTO DATA(system_message).
        IF first_line = abap_true.
          result = |{ result }\{"text":"{ system_message-content }"\}|.
          first_line = abap_false.
        ELSE.
          result = |{ result },\{"text":"{ system_message-content }"\}|.
        ENDIF.
      ENDLOOP.
      result = |{ result }]\}|.
    ENDIF.

    " Add tools
    IF lines( request-tools ) > 0.
      result = |{ result },"toolConfig":\{"tools":[|.
      first_line = abap_true.
      LOOP AT request-tools ASSIGNING FIELD-SYMBOL(<tool>).
        DATA(details) = <tool>->get_tool_details( ).
        IF first_line = abap_true.
          result = |{ result }\{"toolSpec":\{"name":"{ details-name }"|
                && |,"description":"{ details-description }","inputSchema":\{"json":|
                && tool_parser->parse( data_desc    = details-parameters-data_desc
                                       descriptions = details-parameters-descriptions )
                && |\}\}\}|.
          first_line = abap_false.
        ELSE.
          result = |{ result },\{"toolSpec":\{"name":"{ details-name }"|
                && |,"description":"{ details-description }","inputSchema":\{"json":|
                && tool_parser->parse( data_desc    = details-parameters-data_desc
                                       descriptions = details-parameters-descriptions )
                && |\}\}\}|.
        ENDIF.
      ENDLOOP.

      result = |{ result }]|.

      " Tool choice
      IF request-tool_choice <> zif_llm_chat_request=>tool_choice_none.
        CASE request-tool_choice.
          WHEN zif_llm_chat_request=>tool_choice_auto OR zif_llm_chat_request=>tool_choice_required.
            " The majority of models seem to not support any currently, therefore setting auto
            result = |{ result },"toolChoice":\{"auto":\{\}\}|.
          WHEN OTHERS.
            result = |{ result },"toolChoice":\{"tool":\{"name":"{ request-tool_choice }"\}\}|.
        ENDCASE.
      ENDIF.

      result = |{ result }\}|.
    ENDIF.

    " Add options if available
    DATA(option_parameters) = request-options->get_paramters( ).
    IF lines( option_parameters ) > 0.
      result = |{ result },"inferenceConfig":\{|.
      first_line = abap_true.
      LOOP AT option_parameters INTO DATA(parameter).
        IF first_line = abap_true.
          result = |{ result }"{ parameter-key }":{ parameter-value }|.
          first_line = abap_false.
        ELSE.
          result = |{ result },"{ parameter-key }":{ parameter-value }|.
        ENDIF.
      ENDLOOP.
      result = |{ result }\}|.
    ENDIF.

    result = |{ result }\}|.
  ENDMETHOD.

  METHOD parse_message.
    IF lines( message-tool_calls ) > 0.
      result = |\{"role":"{ zif_llm_client_int=>role_assistant }","content":[|.
      LOOP AT message-tool_calls ASSIGNING FIELD-SYMBOL(<tool_call>).
        IF sy-tabix <> 1.
          result = |{ result },|.
        ENDIF.
        result = |{ result }\{"toolUse":\{"toolUseId":"{ <tool_call>-id }","name":"{ <tool_call>-function-name }",|
              && |"input":{ <tool_call>-function-json_response }\}\}|.
      ENDLOOP.
      result = |{ result }]\}|.
    ELSE.
      IF message-role = zif_llm_client_int=>role_tool.
        result = |\{"role":"{ zif_llm_client_int=>role_user }","content":[\{"toolResult":\{|
              && |"content":[\{"json":{ message-content }\}]|.
        result = |{ result },"toolUseId":"{ message-tool_call_id }"\}\}]\}|.
      ELSE.
        result = |\{"role":"{ message-role }","content":[\{"text":"{
                 escape( val    = message-content
                         format = cl_abap_format=>e_json_string ) }"|.
        result = |{ result }\}]\}|.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD handle_http_response.
    IF http_response-code >= 400.
      result-error-http_code  = http_response-code.
      result-error-error_text = http_response-message.
      IF    http_response-code = status-too_many_requests
         OR http_response-code = status-timeout.
        result-error-retrieable = abap_true.
      ENDIF.
      RETURN.
    ENDIF.

    DATA response TYPE aws_response.

    zcl_llm_common=>from_json( EXPORTING json = http_response-response
                               CHANGING  data = response ).

    " There should always be at least one entry (may be multiple with tools)
    IF lines( response-output-message-content ) < 1.
      result-success = abap_false.
      result-error-error_text = 'No model response received!' ##NO_TEXT.
      RETURN.
    ENDIF.

    " First one is either the text output or "thinking" for tool uses
    DATA(response_choice) = response-output-message-content[ 1 ].
    IF response_choice-text IS NOT INITIAL.
      result-choice = VALUE #( finish_reason = response-stopreason
                               message       = VALUE #( role    = response-output-message-role
                                                        content = response_choice-text ) ).
    ENDIF.

    result-usage = VALUE #( completion_tokens = response-usage-outputtokens
                            prompt_tokens     = response-usage-inputtokens
                            total_tokens      = response-usage-totaltokens ).

    " Handle tool calls
    IF request-tool_choice <> zif_llm_chat_request=>tool_choice_none.
      " Map to common choice format
      DATA base_choice TYPE base_choice.
      LOOP AT response-output-message-content ASSIGNING FIELD-SYMBOL(<content>).
        " There might be a "thinking" text which was then mapped above already
        IF <content>-tooluse IS INITIAL.
          CONTINUE.
        ENDIF.
        DATA tool_call TYPE tool_call.
        tool_call-type = 'function'.
        tool_call-id   = <content>-tooluse-tooluseid.
        tool_call-function-name      = <content>-tooluse-name.
        tool_call-function-arguments = <content>-tooluse-input.
        INSERT tool_call INTO TABLE base_choice-message-tool_calls.
      ENDLOOP.
      TRY.
          handle_tool_calls( EXPORTING response_choice = base_choice
                                       request         = request
                             CHANGING  result          = result ).
        CATCH cx_root INTO DATA(error).
          result-success = abap_false.
          result-error   = VALUE #( tool_parse_error = abap_true
                                    error_text       = error->get_text( ) ).
          RETURN.
      ENDTRY.
    ENDIF.

    result-success = abap_true.
  ENDMETHOD.

ENDCLASS.
