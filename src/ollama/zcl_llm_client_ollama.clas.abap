"! <p class="shorttext synchronized" lang="en">Ollama Client</p>
CLASS zcl_llm_client_ollama DEFINITION
  PUBLIC
  INHERITING FROM zcl_llm_client_base
  CREATE PRIVATE.

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

  PROTECTED SECTION.
    METHODS get_http_client      REDEFINITION.
    METHODS set_auth             REDEFINITION.
    METHODS get_chat_endpoint    REDEFINITION.
    METHODS build_request_json   REDEFINITION.
    METHODS handle_http_response REDEFINITION.
    METHODS parse_message        REDEFINITION.

  PRIVATE SECTION.
    TYPES: BEGIN OF ollama_function,
             name      TYPE string,
             arguments TYPE /ui2/cl_json=>json,
           END OF ollama_function.

    TYPES: BEGIN OF ollama_tool_call,
             id       TYPE string,
             type     TYPE string,
             function TYPE ollama_function,
           END OF ollama_tool_call.

    TYPES: BEGIN OF ollama_message,
             role       TYPE string,
             content    TYPE string,
             tool_calls TYPE STANDARD TABLE OF ollama_tool_call WITH EMPTY KEY,
           END OF ollama_message.

    TYPES: BEGIN OF ollama_response,
             prompt_eval_count TYPE i,
             eval_count        TYPE i,
             message           TYPE ollama_message,
             done_reason       TYPE string,
           END OF ollama_response.

ENDCLASS.

CLASS zcl_llm_client_ollama IMPLEMENTATION.
  METHOD constructor.
    super->constructor( client_config   = client_config
                        provider_config = provider_config ).
    initialize( ).
  ENDMETHOD.

  METHOD get_client.
    result = NEW zcl_llm_client_ollama( client_config   = client_config
                                        provider_config = provider_config ).
  ENDMETHOD.

  METHOD get_http_client.
    client = zcl_llm_http_client_wrapper=>get_client( client_config   = client_config
                                                      provider_config = provider_config ).
  ENDMETHOD.

  METHOD set_auth.
    " OLLAMA supports optional API Key via custom headers
    " Format is HeaderName:ApiKey
    DATA auth_value TYPE string.

    IF provider_config-auth_encrypted IS NOT INITIAL.
      DATA(llm_badi) = zcl_llm_common=>get_llm_badi( ).
      CALL BADI llm_badi->get_encryption_impl
        RECEIVING result = DATA(enc_class).
      auth_value = enc_class->decrypt( provider_config-auth_encrypted ).
    ENDIF.
    IF auth_value IS NOT INITIAL.
      SPLIT auth_value AT ':' INTO DATA(api_header) DATA(api_key).
      client->set_header( name  = api_header
                          value = api_key ).
    ENDIF.
  ENDMETHOD.

  METHOD build_request_json.
    " Handle Ollama-specific structured output
    IF request-use_structured_output = abap_true.
      " We need to modify the request before passing it to the base implementation
      DATA(modified_request) = request.
      " Turn off structured output flag so base implementation doesn't add response_format
      modified_request-use_structured_output = abap_false.
      DATA(last_line) = lines( modified_request-messages ).
      IF last_line > 0.
        ASSIGN modified_request-messages[ last_line ] TO FIELD-SYMBOL(<message>).
        <message>-content = |{ <message>-content } Respond in JSON based on the following schema: |
                         && |{ request-structured_output->get_schema( ) }| ##NO_TEXT.
      ENDIF.
    ELSE.
      modified_request = request.
    ENDIF.

    " Call base implementation with potentially modified request
    DATA(json) = super->build_request_json( modified_request ).

    " Remove the last closing brace
    REPLACE REGEX '\}$' IN json WITH ''.

    " Add stream parameter
    json = |{ json },"stream":false|.

    " Add Ollama-specific format parameter for structured output
    IF request-use_structured_output = abap_true.
      json = |{ json },"format":{ request-structured_output->get_schema( ) }|.
    ENDIF.

    " Add options in Ollama format if available
    DATA(option_parameters) = request-options->get_paramters( ).
    IF lines( option_parameters ) > 0.
      json = |{ json },"options":\{|.
      DATA(first_line) = abap_true.

      LOOP AT option_parameters INTO DATA(parameter).
        IF first_line = abap_true.
          json = |{ json }"{ parameter-key }":{ parameter-value }|.
          first_line = abap_false.
        ELSE.
          json = |{ json },"{ parameter-key }":{ parameter-value }|.
        ENDIF.
      ENDLOOP.

      json = |{ json }\}|.
    ENDIF.

    " Close JSON
    json = |{ json }\}|.

    result = json.
  ENDMETHOD.


  METHOD get_chat_endpoint.
    result = '/chat'.
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

    DATA response TYPE ollama_response.

    zcl_llm_common=>from_json( EXPORTING json = http_response-response
                               CHANGING  data = response ).

    result-choice = VALUE #( finish_reason = response-done_reason
                             message       = VALUE #( role    = response-message-role
                                                      content = response-message-content ) ).

    result-usage  = VALUE #( completion_tokens = response-eval_count
                             prompt_tokens     = response-prompt_eval_count
                             total_tokens      = response-eval_count + response-prompt_eval_count ).

    IF request-use_structured_output = abap_true.
      parse_structured_output( EXPORTING content  = response-message-content
                                         request  = request
                               CHANGING  response = result ).
    ENDIF.

    " There can be multiple tool calls, we need to map them properly to the available tools.
    IF request-tool_choice <> zif_llm_chat_request=>tool_choice_none.
      LOOP AT request-tools ASSIGNING FIELD-SYMBOL(<tool>).
        DATA(details) = <tool>->get_tool_details( ).

        LOOP AT response-message-tool_calls ASSIGNING FIELD-SYMBOL(<tool_call>) WHERE function-name = details-name.
          TRY.
              DATA func_result TYPE REF TO data.
              CREATE DATA func_result TYPE HANDLE details-parameters-data_desc.

              " Parse the JSON arguments
              zcl_llm_common=>from_json( EXPORTING json = <tool_call>-function-arguments
                                         CHANGING  data = func_result ).

              " Add tool call to response
              APPEND VALUE #( id       = ``
                              type     = zif_llm_tool=>type_function
                              function = VALUE #( name          = details-name
                                                  arguments     = func_result
                                                  json_response = <tool_call>-function-arguments ) )
                     TO result-choice-tool_calls.

              " For Ollama, we need to use the unescaped JSON directly
              result-choice-message = VALUE #( BASE result-choice-message
                                               role    = zif_llm_client_int=>role_tool
                                               name    = details-name
                                               content = <tool_call>-function-arguments ).

            CATCH cx_root.
              result-success = abap_false.
              MESSAGE ID 'ZLLM_CLIENT' TYPE 'E' NUMBER 016 WITH <tool_call>-function-name INTO DATA(message_text).
              result-error = VALUE #( tool_parse_error = abap_true
                                      error_text       = message_text ).
              RETURN.
          ENDTRY.
        ENDLOOP.

        " This is only an issue if tool call is required
        IF sy-subrc <> 0 AND request-tool_choice = zif_llm_chat_request=>tool_choice_required.
          result-success = abap_false.
          MESSAGE ID 'ZLLM_CLIENT' TYPE 'E' NUMBER 017 WITH details-name INTO message_text.
          result-error = VALUE #( tool_parse_error = abap_true
                                  error_text       = message_text ).
          RETURN.
        ENDIF.
      ENDLOOP.
    ENDIF.

    result-success = abap_true.
  ENDMETHOD.

  METHOD parse_message.
    IF lines( message-tool_calls ) > 0.
      result = |\{"role":"{ message-role }","tool_calls":[|.
      LOOP AT message-tool_calls ASSIGNING FIELD-SYMBOL(<tool_call>).
        IF sy-tabix <> 1.
          result = |{ result },|.
        ENDIF.
        result = |{ result }\{"id":"{ <tool_call>-id }","type":"{ <tool_call>-type }",|
              && |"{ <tool_call>-type }":\{"name":"{ <tool_call>-function-name }",|
              && |"arguments":{ <tool_call>-function-json_response }\}\}|.
      ENDLOOP.
      result = |{ result }]\}|.
    ELSE.
      result = |\{"role":"{ message-role }","content":"{
               escape( val    = message-content
                       format = cl_abap_format=>e_json_string ) }"|.
      IF message-name IS NOT INITIAL.
        result = |{ result },"name":"{ message-name }"|.
      ENDIF.
      IF message-tool_call_id IS NOT INITIAL.
        result = |{ result },"tool_call_id":"{ message-tool_call_id }"\}|.
      ELSE.
        result = |{ result }\}|.
      ENDIF.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
