"! <p class="shorttext synchronized" lang="en">Anthropic LLM Client</p>
CLASS zcl_llm_client_anthropic DEFINITION
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

    METHODS zif_llm_client_int~new_request REDEFINITION.

  PROTECTED SECTION.
    METHODS get_http_client      REDEFINITION.
    METHODS set_auth             REDEFINITION.
    METHODS get_chat_endpoint    REDEFINITION.
    METHODS build_request_json   REDEFINITION.
    METHODS parse_message        REDEFINITION.
    METHODS handle_http_response REDEFINITION.

  PRIVATE SECTION.
    " Anthropic message are in a content array
    TYPES: BEGIN OF anthropic_message,
             type  TYPE string,
             text  TYPE string,
             id    TYPE string,
             name  TYPE string,
             " We need this as pure json content in order to parse it later
             input TYPE /ui2/cl_json=>json,
           END OF anthropic_message,
           anthropic_messages TYPE STANDARD TABLE OF anthropic_message WITH EMPTY KEY.

    TYPES: BEGIN OF anthropic_usage,
             input_tokens  TYPE i,
             output_tokens TYPE i,
           END OF anthropic_usage.

    TYPES: BEGIN OF anthropic_response,
             content     TYPE anthropic_messages,
             stop_reason TYPE string,
             role        TYPE string,
             usage       TYPE anthropic_usage,
           END OF anthropic_response.
ENDCLASS.


CLASS zcl_llm_client_anthropic IMPLEMENTATION.
  METHOD constructor.
    super->constructor( client_config   = client_config
                        provider_config = provider_config ).
    initialize( ).
  ENDMETHOD.

  METHOD get_client.
    result = NEW zcl_llm_client_anthropic( client_config   = client_config
                                           provider_config = provider_config ).
  ENDMETHOD.

  METHOD get_chat_endpoint.
    result = '/messages'.
  ENDMETHOD.

  METHOD get_http_client.
    client = zcl_llm_http_client_wrapper=>get_client( client_config   = client_config
                                                      provider_config = provider_config ).
    client->set_header( name  = 'anthropic-version'
                        value = '2023-06-01' ).
  ENDMETHOD.

  METHOD set_auth.
    DATA auth_value TYPE string.

    IF provider_config-auth_encrypted IS NOT INITIAL.
      DATA(llm_badi) = zcl_llm_common=>get_llm_badi( ).
      CALL BADI llm_badi->get_encryption_impl
        RECEIVING result = DATA(enc_class).
      auth_value = enc_class->decrypt( provider_config-auth_encrypted ).
    ENDIF.
    client->set_header( name  = 'x-api-key'
                        value = auth_value ).
  ENDMETHOD.

  METHOD zif_llm_client_int~new_request.
    response = super->zif_llm_client_int~new_request( ).
    " max_tokens is mandatory for anthropic models. The current ones support 8192.
    response->options( )->set_max_tokens( 8192 ).
    " Structured output is not supported but for now we just leave the default
    " implementation and simply ignore it in later stages. At a later point in time
    " we might try to solve this via a mandatory tool call instead. However I hope this is
    " a default feature with the next model generation anyhow.
    " Initialize tool parser
    tool_parser = create_tool_parser( ).
  ENDMETHOD.

  METHOD build_request_json.
    " Open content
    result = |\{"model":"{ client_config-provider_model }","messages":[|.
    DATA first_line          TYPE abap_bool VALUE abap_true.

    " Anthropic handles system messages differently
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
          result = |{ result }\{"text":"{ system_message-content }","type":"text"\}|.
          first_line = abap_false.
        ELSE.
          result = |{ result },\{"text":"{ system_message-content }","type":"text"\}|.
        ENDIF.
      ENDLOOP.
      result = |{ result }]|.
    ENDIF.

    " Anthropic requires tool definitions of tools used before.
    DATA(tool_definition_required) = abap_false.
    IF lines( request-tools ) > 0 AND request-tool_choice = zif_llm_chat_request=>tool_choice_none.
      LOOP AT request-messages ASSIGNING <message>.
        IF lines( <message>-tool_calls ) > 0.
          tool_definition_required = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF lines( request-tools ) > 0 AND ( tool_definition_required = abap_true OR request-tool_choice <> zif_llm_chat_request=>tool_choice_none ).
      result = |{ result },"tools":[|.
      first_line = abap_true.
      LOOP AT request-tools ASSIGNING FIELD-SYMBOL(<tool>).
        DATA(details) = <tool>->get_tool_details( ).
        IF first_line = abap_true.
          result = |{ result }\{"name":"{ details-name }"|
                && |,"description":"{ details-description }","input_schema":|
                && tool_parser->parse( data_desc    = details-parameters-data_desc
                                       descriptions = details-parameters-descriptions )
                && |\}|.
          first_line = abap_false.
        ELSE.
          result = |{ result },\{"name":"{ details-name }"|
                && |,"description":"{ details-description }","input_schema":|
                && tool_parser->parse( data_desc    = details-parameters-data_desc
                                       descriptions = details-parameters-descriptions )
                && |\}|.
        ENDIF.
      ENDLOOP.

      result = |{ result }]|.

      " Tool choice
      IF request-tool_choice <> zif_llm_chat_request=>tool_choice_none.
        CASE request-tool_choice.
          WHEN zif_llm_chat_request=>tool_choice_auto.
            result = |{ result },"tool_choice":\{"type":"auto"\}|.
          WHEN zif_llm_chat_request=>tool_choice_required.
            result = |{ result },"tool_choice":\{"type":"any"\}|.
          WHEN OTHERS.
            result = |{ result },"tool_choice":\{"type":"tool","name":"{ request-tool_choice }"\}|.
        ENDCASE.
      ENDIF.
    ENDIF.

    " Add options if available
    DATA(option_parameters) = request-options->get_paramters( ).
    LOOP AT option_parameters INTO DATA(parameter).
      result = |{ result },"{ parameter-key }":{ parameter-value }|.
    ENDLOOP.

    result = |{ result }\}|.
  ENDMETHOD.

  METHOD parse_message.
    IF lines( message-tool_calls ) > 0.
      " Add a dummy assistant message if the output from the last call in not available
      DATA(content) = message-content.
      IF content IS INITIAL.
        content = `tool call.` ##NO_TEXT.
      ENDIF.

      result = |\{"role":"{ message-role }","content":[|
            && |\{"type":"text","text":"{ content }"\}|.
      LOOP AT message-tool_calls ASSIGNING FIELD-SYMBOL(<tool_call>).

        result = |{ result },\{"id":"{ <tool_call>-id }","type":"tool_use",|
              && |"name":"{ <tool_call>-function-name }",|
              && |"input":{ <tool_call>-function-json_response }\}|.
      ENDLOOP.
      result = |{ result }]\}|.
    ELSE.

      " Need to differentiate between user and tool message
      IF message-role = zif_llm_client_int=>role_tool.
        result = |\{"role":"user","content":[\{"type":"tool_result",|
              && |"tool_use_id":"{ message-tool_call_id }","content":"|
              && |{ escape( val    = message-content
                            format = cl_abap_format=>e_json_string ) }"\}]\}|.
      ELSE.
        result = |\{"role":"{ message-role }","content":"{
                 escape( val    = message-content
                         format = cl_abap_format=>e_json_string ) }"\}|.
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

    DATA response TYPE anthropic_response.

    zcl_llm_common=>from_json( EXPORTING json = http_response-response
                               CHANGING  data = response ).

    " We should only have one text response but there might be multiple tool_use entries
    DATA tool_calls         TYPE anthropic_messages.
    DATA assistant_response TYPE string.
    LOOP AT response-content ASSIGNING FIELD-SYMBOL(<content>).
      IF <content>-type = 'text'.
        IF assistant_response IS NOT INITIAL.
          result-success = abap_false.
          result-error-error_text = 'More than one text response entry not supported' ##NO_TEXT.
          RETURN.
        ENDIF.
        assistant_response = <content>-text.
      ELSE.
        APPEND <content> TO tool_calls.
      ENDIF.
    ENDLOOP.

    result-choice = VALUE #( finish_reason = response-stop_reason
                             message       = VALUE #( role    = response-role
                                                      content = assistant_response ) ).

    result-usage  = VALUE #( completion_tokens = response-usage-output_tokens
                             prompt_tokens     = response-usage-input_tokens
                             total_tokens      = ( response-usage-input_tokens + response-usage-output_tokens ) ).

    " Handle tool calls
    " To minimize effort we just transfer the anthropic response structure to our internal one
    DATA(response_choice) = VALUE base_choice( finish_reason = response-stop_reason
                                               message       = VALUE #( role    = zif_llm_client_int=>role_assistant
                                                                        content = assistant_response ) ).
    LOOP AT tool_calls ASSIGNING FIELD-SYMBOL(<tool_call>).
      APPEND VALUE #( id       = <tool_call>-id
                      type     = 'function'
                      function = VALUE #( name      = <tool_call>-name
                                          arguments = <tool_call>-input ) ) TO response_choice-message-tool_calls.
    ENDLOOP.

    IF request-tool_choice <> zif_llm_chat_request=>tool_choice_none.
      TRY.
          handle_tool_calls( EXPORTING response_choice = response_choice
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
