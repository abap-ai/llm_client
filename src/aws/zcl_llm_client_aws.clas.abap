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
           aws_contents TYPE STANDARD TABLE OF aws_content WITH DEFAULT KEY.

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
    CREATE OBJECT result TYPE zcl_llm_client_aws EXPORTING client_config = client_config provider_config = provider_config.
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
    DATA access_key TYPE string.
    DATA secret_key TYPE string.
    DATA region TYPE string.
    DATA credentials TYPE zcl_llm_client_aws_sigv4=>auth_credentials.

    IF provider_config-auth_encrypted IS NOT INITIAL.
      DATA(llm_badi) = zcl_llm_common=>get_llm_badi( ).
      CALL BADI llm_badi->get_encryption_impl
        RECEIVING result = DATA(enc_class).
      auth_value = enc_class->decrypt( provider_config-auth_encrypted ).
    ENDIF.

    
    
    
    SPLIT auth_value AT ',' INTO access_key secret_key host region.
    " We cannot set any auth header now, we need to get an active token
    " right before every call instead. We just initialize the class.
    
    credentials-access_key = access_key.
    credentials-secret_key = secret_key.
    credentials-region     = region.
    credentials-service    = 'bedrock'.
    CREATE OBJECT auth TYPE zcl_llm_client_aws_sigv4 EXPORTING credentials = credentials.
  ENDMETHOD.

  METHOD zif_llm_client_int~chat.
    " Set the aws headers including auth, rest handled by base class
    DATA uri TYPE string.
    DATA request_time TYPE timestamp.
    DATA temp1 TYPE string.
    DATA helper LIKE temp1.
    DATA timestamp TYPE string.
    DATA headers TYPE tihttpnvp.
    FIELD-SYMBOLS <header> LIKE LINE OF headers.
    DATA json TYPE string.
    DATA token TYPE string.
        DATA resp TYPE zif_llm_http_client_wrapper=>response.
        DATA temp2 TYPE zllm_statistics.
        DATA http_error TYPE REF TO zcx_llm_http_error.
    uri = |/model/{ escape( val    = client_config-provider_model
                                  format = cl_abap_format=>e_url_full ) }/converse|.
    
    GET TIME STAMP FIELD request_time.
    
    temp1 = request_time.
    
    helper = temp1.
    
    timestamp = |{ helper(8) }T{ helper+8(6) }Z|.
    client->set_header( name  = 'x-amz-date'
                        value = timestamp ).
    client->set_header( name  = 'host'
                        value = host ).
    
    headers = client->get_req_headers( ).
    
    LOOP AT headers ASSIGNING <header>.
      IF <header>-name <> 'x-amz-date' AND <header>-name <> 'host'.
        DELETE headers INDEX sy-tabix.
      ENDIF.
    ENDLOOP.

    
    json = build_request_json( request->get_internal_request( ) ).
    
    token = auth->get_auth_header( request_method = 'POST'
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
        
        resp = client->communicate( request    = json
                                          session_id = id
                                          msg        = msg ).
        GET TIME STAMP FIELD end_request.
        response = handle_http_response( http_response = resp
                                         request       = request->get_internal_request( ) ).
        
        CLEAR temp2.
        temp2-call_date = sy-datum.
        temp2-call_time = sy-uzeit.
        temp2-duration = end_request - begin_request.
        temp2-model = client_config-model.
        temp2-tokens_prompt = response-usage-prompt_tokens.
        temp2-tokens_resp = response-usage-completion_tokens.
        temp2-tokens_total = response-usage-total_tokens.
        temp2-id = id.
        temp2-msg = msg.
        statistics->add( temp2 ).
        
      CATCH zcx_llm_http_error INTO http_error.
        response-error-error_text = http_error->if_message~get_text( ).
        response-error-retrieable = abap_false.
      CLEANUP.
        IF client IS BOUND.
          client->close_client( ).
        ENDIF.
    ENDTRY.
  ENDMETHOD.

  METHOD build_request_json.
    DATA first_line TYPE abap_bool VALUE abap_true.
    DATA non_system_messages TYPE zllm_msgs.
    DATA system_messages TYPE zllm_msgs.
    FIELD-SYMBOLS <message> LIKE LINE OF request-messages.
    DATA message LIKE LINE OF non_system_messages.
      DATA system_message LIKE LINE OF system_messages.
      FIELD-SYMBOLS <tool> LIKE LINE OF request-tools.
        DATA details TYPE zif_llm_tool=>tool_details.
    DATA option_parameters TYPE zllm_keyvalues.
      DATA parameter LIKE LINE OF option_parameters.
    " Open content
    result = |\{"messages":[|.
    

    " AWS handles system messages differently
    
    

    
    LOOP AT request-messages ASSIGNING <message>.
      IF <message>-role = 'system'.
        APPEND <message> TO system_messages.
      ELSE.
        APPEND <message> TO non_system_messages.
      ENDIF.
    ENDLOOP.

    " Add messages
    
    LOOP AT non_system_messages INTO message.
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
      
      LOOP AT system_messages INTO system_message.
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
      
      LOOP AT request-tools ASSIGNING <tool>.
        
        details = <tool>->get_tool_details( ).
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
    
    option_parameters = request-options->get_paramters( ).
    IF lines( option_parameters ) > 0.
      result = |{ result },"inferenceConfig":\{|.
      first_line = abap_true.
      
      LOOP AT option_parameters INTO parameter.
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
      FIELD-SYMBOLS <tool_call> LIKE LINE OF message-tool_calls.
    IF lines( message-tool_calls ) > 0.
      result = |\{"role":"{ zif_llm_client_int=>role_assistant }","content":[|.
      
      LOOP AT message-tool_calls ASSIGNING <tool_call>.
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
    DATA response TYPE aws_response.
    DATA response_choice TYPE zcl_llm_client_aws=>aws_content.
    DATA temp1 LIKE LINE OF response-output-message-content.
    DATA temp2 LIKE sy-tabix.
      DATA base_choice TYPE base_choice.
      FIELD-SYMBOLS <content> LIKE LINE OF response-output-message-content.
        DATA tool_call TYPE tool_call.
          DATA error TYPE REF TO cx_root.
    IF http_response-code >= 400.
      result-error-http_code  = http_response-code.
      result-error-error_text = http_response-message.
      IF    http_response-code = status-too_many_requests
         OR http_response-code = status-timeout.
        result-error-retrieable = abap_true.
      ENDIF.
      RETURN.
    ENDIF.

    

    zcl_llm_common=>from_json( EXPORTING json = http_response-response
                               CHANGING  data = response ).

    " There should always be at least one entry (may be multiple with tools)
    IF lines( response-output-message-content ) < 1.
      result-success = abap_false.
      result-error-error_text = 'No model response received!' ##NO_TEXT.
      RETURN.
    ENDIF.

    " First one is either the text output or "thinking" for tool uses
    
    
    
    temp2 = sy-tabix.
    READ TABLE response-output-message-content INDEX 1 INTO temp1.
    sy-tabix = temp2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    response_choice = temp1.
    IF response_choice-text IS NOT INITIAL.
      CLEAR result-choice.
      result-choice-finish_reason = response-stopreason.
      CLEAR result-choice-message.
      result-choice-message-role = response-output-message-role.
      result-choice-message-content = response_choice-text.
    ENDIF.

    CLEAR result-usage.
    result-usage-completion_tokens = response-usage-outputtokens.
    result-usage-prompt_tokens = response-usage-inputtokens.
    result-usage-total_tokens = response-usage-totaltokens.

    " Handle tool calls
    IF request-tool_choice <> zif_llm_chat_request=>tool_choice_none.
      " Map to common choice format
      
      
      LOOP AT response-output-message-content ASSIGNING <content>.
        " There might be a "thinking" text which was then mapped above already
        IF <content>-tooluse IS INITIAL.
          CONTINUE.
        ENDIF.
        
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
          
        CATCH cx_root INTO error.
          result-success = abap_false.
          CLEAR result-error.
          result-error-tool_parse_error = abap_true.
          result-error-error_text = error->get_text( ).
          RETURN.
      ENDTRY.
    ENDIF.

    result-success = abap_true.
  ENDMETHOD.

ENDCLASS.
