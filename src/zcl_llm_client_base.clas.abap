CLASS zcl_llm_client_base DEFINITION
  PUBLIC
  ABSTRACT.

  PUBLIC SECTION.
    INTERFACES zif_llm_client_int.

    METHODS constructor
      IMPORTING client_config   TYPE zllm_clnt_config
                provider_config TYPE zllm_providers.

  PROTECTED SECTION.
    CONSTANTS:
      BEGIN OF status,
        ok                TYPE i VALUE 200,
        too_many_requests TYPE i VALUE 429,
        timeout           TYPE i VALUE 408,
      END OF status.

    " Common type definitions for LLM responses
    TYPES: BEGIN OF function,
             name      TYPE string,
             arguments TYPE string,
           END OF function.

    TYPES: BEGIN OF tool_call,
             id       TYPE string,
             type     TYPE string,
             function TYPE function,
           END OF tool_call.

    TYPES: BEGIN OF base_message,
             role       TYPE string,
             content    TYPE string,
             tool_calls TYPE STANDARD TABLE OF tool_call WITH EMPTY KEY,
           END OF base_message.

    TYPES: BEGIN OF base_choice,
             finish_reason TYPE string,
             message       TYPE base_message,
           END OF base_choice.

    TYPES: BEGIN OF base_usage,
             prompt_tokens     TYPE i,
             completion_tokens TYPE i,
             total_tokens      TYPE i,
           END OF base_usage.

    TYPES: BEGIN OF base_response,
             id      TYPE string,
             choices TYPE STANDARD TABLE OF base_choice WITH EMPTY KEY,
             usage   TYPE base_usage,
             model   TYPE string,
           END OF base_response.

    DATA client          TYPE REF TO zif_llm_http_client_wrapper.
    DATA client_config   TYPE zllm_clnt_config.
    DATA provider_config TYPE zllm_providers.
    DATA tool_parser     TYPE REF TO zif_llm_tool_parser.
    DATA statistics      TYPE REF TO zif_llm_statistics.
    DATA begin_request   TYPE timestamp.
    DATA end_request     TYPE timestamp.
    DATA id              TYPE uuid.
    DATA msg             TYPE i.

    " Abstract methods that must be implemented by providers
    METHODS get_http_client ABSTRACT
      RAISING zcx_llm_validation.

    METHODS set_auth ABSTRACT
      RAISING zcx_llm_authorization.

    METHODS get_chat_endpoint ABSTRACT
      RETURNING VALUE(result) TYPE string.

    " Common methods with default implementations that can be overridden
    METHODS build_request_json
      IMPORTING !request      TYPE zllm_request
      RETURNING VALUE(result) TYPE string.

    METHODS handle_http_response
      IMPORTING !request      TYPE zllm_request
                http_response TYPE zif_llm_http_client_wrapper=>response
      RETURNING VALUE(result) TYPE zllm_response.

    METHODS parse_message
      IMPORTING !message      TYPE zllm_msg
      RETURNING VALUE(result) TYPE string.

    METHODS create_structured_output
      RETURNING VALUE(result) TYPE REF TO zif_llm_so
      RAISING   zcx_llm_validation.

    METHODS parse_structured_output
      IMPORTING content   TYPE string
                !request  TYPE zllm_request
      CHANGING  !response TYPE zllm_response.

    " New helper methods for common functionality
    METHODS handle_tool_calls
      IMPORTING response_choice TYPE base_choice
                !request        TYPE zllm_request
      CHANGING  !result         TYPE zllm_response
      RAISING   cx_static_check.

    METHODS initialize
      RAISING zcx_llm_validation
              zcx_llm_authorization.

    METHODS create_tool_parser RETURNING VALUE(result) TYPE REF TO zif_llm_tool_parser.

  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_llm_client_base IMPLEMENTATION.
  METHOD constructor.
    me->client_config   = client_config.
    me->provider_config = provider_config.
    TRY.
        id = cl_system_uuid=>create_uuid_x16_static( ).
      CATCH cx_uuid_error.
        DATA help_timestamp TYPE timestampl.
        GET TIME STAMP FIELD help_timestamp.
        id = help_timestamp.
    ENDTRY.
    DATA stat_handler TYPE REF TO zllm_implementation.
    GET BADI stat_handler.
    CALL BADI stat_handler->get_statistics_impl
      RECEIVING
        result = statistics.
  ENDMETHOD.

  METHOD build_request_json.
    " Open content
    result = |\{"model":"{ client_config-provider_model }","messages":[|.
    DATA first_line TYPE abap_bool VALUE abap_true.

    DATA(messages) = request-messages.

    " Add messages
    LOOP AT messages INTO DATA(message).
      IF first_line = abap_true.
        result = |{ result }{ parse_message( message ) }|.
        first_line = abap_false.
      ELSE.
        result = |{ result },{ parse_message( message ) }|.
      ENDIF.
    ENDLOOP.
    result = |{ result }]|.

    " Add structured output if available and format
    IF request-use_structured_output = abap_true.
      result = |{ result },"response_format":\{"type":"json_schema","json_schema":{ request-structured_output->get_schema( ) }\}|.
    ENDIF.

    " Add tools if available and active
    IF lines( request-tools ) > 0 AND request-tool_choice <> zif_llm_chat_request=>tool_choice_none.
      result = |{ result },"tools":[|.
      first_line = abap_true.
      LOOP AT request-tools ASSIGNING FIELD-SYMBOL(<tool>).
        DATA(details) = <tool>->get_tool_details( ).
        IF first_line = abap_true.
          result = |{ result }\{"type":"{ details-type }","{ details-type }":\{"name":"{ details-name }"|
                && |,"description":"{ details-description }","parameters":|
                && tool_parser->parse( data_desc    = details-parameters-data_desc
                                       descriptions = details-parameters-descriptions )
                && |,"strict":true\}\}|.
          first_line = abap_false.
        ELSE.
          result = |{ result },\{"type":"{ details-type }","{ details-type }":\{"name":"{ details-name }"|
                && |,"description":"{ details-description }","parameters":|
                && tool_parser->parse( data_desc    = details-parameters-data_desc
                                       descriptions = details-parameters-descriptions )
                && |,"strict":true\}\}|.
        ENDIF.
      ENDLOOP.

      result = |{ result }]|.

      " Tool choice
      IF     request-tool_choice <> zif_llm_chat_request=>tool_choice_auto
         AND request-tool_choice <> zif_llm_chat_request=>tool_choice_required.
        result = |{ result },"tool_choice":"\{"type":"function","function":\{"name":"{ request-tool_choice }"\}\}|.
      ELSE.
        result = |{ result },"tool_choice":"{ request-tool_choice }"|.
      ENDIF.
    ENDIF.

    " Add options if available
    DATA(option_parameters) = request-options->get_paramters( ).
    LOOP AT option_parameters INTO DATA(parameter).
      result = |{ result },"{ parameter-key }":{ parameter-value }|.
    ENDLOOP.

    result = |{ result }\}|.
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

    DATA response TYPE base_response.

    zcl_llm_common=>from_json( EXPORTING json = http_response-response
                               CHANGING  data = response ).

    " There should always be only one response without streaming or none in case of errors
    IF lines( response-choices ) <> 1.
      result-success = abap_false.
      result-error-error_text = 'Wrong number of choices!' ##NO_TEXT.
      RETURN.
    ENDIF.

    DATA(response_choice) = response-choices[ 1 ].
    result-choice = VALUE #( finish_reason = response_choice-finish_reason
                             message       = VALUE #( role    = response_choice-message-role
                                                      content = response_choice-message-content ) ).

    result-usage  = VALUE #( completion_tokens = response-usage-completion_tokens
                             prompt_tokens     = response-usage-prompt_tokens
                             total_tokens      = response-usage-total_tokens ).

    IF request-use_structured_output = abap_true.
      parse_structured_output( EXPORTING content  = response_choice-message-content
                                         request  = request
                               CHANGING  response = result ).
    ENDIF.

    " Handle tool calls
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

  METHOD handle_tool_calls.
    LOOP AT request-tools ASSIGNING FIELD-SYMBOL(<tool>).
      DATA(details) = <tool>->get_tool_details( ).

      LOOP AT response_choice-message-tool_calls ASSIGNING FIELD-SYMBOL(<tool_call>)
           WHERE function-name = details-name.
        TRY.
            DATA func_result TYPE REF TO data.
            CREATE DATA func_result TYPE HANDLE details-parameters-data_desc.

            zcl_llm_common=>from_json( EXPORTING json = <tool_call>-function-arguments
                                       CHANGING  data = func_result ).

            APPEND VALUE #( id       = <tool_call>-id
                            type     = zif_llm_tool=>type_function
                            function = VALUE #( name          = details-name
                                                arguments     = func_result
                                                json_response = <tool_call>-function-arguments ) )
                   TO result-choice-tool_calls.
          CATCH cx_root.
            MESSAGE ID 'ZLLM_CLIENT' TYPE 'E' NUMBER 016
                    WITH <tool_call>-function-name INTO DATA(message_text) ##NEEDED.
        ENDTRY.
      ENDLOOP.

      " This is only an issue if tool call is required
      IF sy-subrc <> 0 AND request-tool_choice = zif_llm_chat_request=>tool_choice_required.
        MESSAGE ID 'ZLLM_CLIENT' TYPE 'E' NUMBER 017
                WITH details-name INTO message_text.
      ENDIF.
    ENDLOOP.
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
              && |"arguments":"{ escape( val    = <tool_call>-function-json_response
                                         format = cl_abap_format=>e_json_string ) }"\}\}|.
      ENDLOOP.
      result = |{ result }]\}|.
    ELSE.
      result = |\{"role":"{ message-role }","content":"{
               escape( val    = message-content
                       format = cl_abap_format=>e_json_string ) }"|.
      IF message-name IS NOT INITIAL.
        result =  |{ result },"name":"{ message-name }"|.
      ENDIF.
      IF message-tool_call_id IS NOT INITIAL.
        result = |{ result },"tool_call_id":"{ message-tool_call_id }"\}|.
      ELSE.
        result = |{ result }\}|.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD create_structured_output.
    result = NEW zcl_llm_so_js( ).
  ENDMETHOD.

  METHOD initialize.
    get_http_client( ).
    set_auth( ).
  ENDMETHOD.

  METHOD parse_structured_output.
    DATA(data_desc) = request-structured_output->get_datatype( ).

    CREATE DATA response-choice-structured_output TYPE HANDLE data_desc.

    zcl_llm_common=>from_json( EXPORTING json = content
                               CHANGING  data = response-choice-structured_output ).
  ENDMETHOD.

  METHOD zif_llm_client_int~chat.
    TRY.
        msg = msg + 1.
        client->set_url( get_chat_endpoint( ) ).
        GET TIME STAMP FIELD begin_request.
        DATA(resp) = client->communicate( request    = build_request_json( request->get_internal_request( ) )
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

  METHOD zif_llm_client_int~new_request.
    DATA request TYPE zllm_request.

    " Initialize options
    request-options           = NEW zcl_llm_options( ).

    " Initialize structured output using provider-specific implementation
    request-structured_output = create_structured_output( ).
    " Same for tool parser
    tool_parser = create_tool_parser( ).

    " Get configured default parameters and set them
    IF client_config-default_op IS NOT INITIAL.
      SPLIT client_config-default_op AT ';' INTO TABLE DATA(options).
      DATA parameters TYPE zllm_keyvalues.

      LOOP AT options INTO DATA(option).
        SPLIT option AT ':' INTO DATA(key) DATA(value).
        INSERT VALUE #( key   = key
                        value = value )
               INTO TABLE parameters.
      ENDLOOP.

      request-options->set_custom_parameters( parameters ).
    ENDIF.

    " Create and return chat request object
    response = NEW zcl_llm_chat_request( request ).
  ENDMETHOD.

  METHOD zif_llm_client_int~get_client.
    CALL METHOD (provider_config-provider_class)=>get_client
      EXPORTING
        client_config   = client_config
        provider_config = provider_config
      RECEIVING
        result          = response.
  ENDMETHOD.

  METHOD create_tool_parser.
    result = NEW zcl_llm_tool_parser(  ).
  ENDMETHOD.

ENDCLASS.
