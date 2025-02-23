"! <p class="shorttext synchronized" lang="en">Vertex AI LLM Client</p>
CLASS zcl_llm_client_vertexai DEFINITION
  PUBLIC
  INHERITING FROM zcl_llm_client_base
  CREATE PUBLIC .

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
    METHODS zif_llm_client_int~new_request REDEFINITION.
  PROTECTED SECTION.
    METHODS: get_http_client REDEFINITION,
      set_auth REDEFINITION,
      get_chat_endpoint REDEFINITION,
      build_request_json REDEFINITION,
      parse_message REDEFINITION,
      handle_http_response REDEFINITION.
  PRIVATE SECTION.
    DATA auth TYPE REF TO zcl_llm_client_vertex_auth.

    " VertexAI messages
    TYPES: BEGIN OF vertexai_function,
             name TYPE string,
             " We need this as pure json content in order to parse it later
             args TYPE /ui2/cl_json=>json,
           END OF vertexai_function,
           vertexai_functions TYPE STANDARD TABLE OF vertexai_function WITH EMPTY KEY.

    TYPES: BEGIN OF vertexai_part,
             text         TYPE string,
             functioncall TYPE vertexai_function,
           END OF vertexai_part,
           vertexai_parts TYPE STANDARD TABLE OF vertexai_part WITH EMPTY KEY.

    TYPES: BEGIN OF vertexai_usage,
             prompttokencount     TYPE i,
             candidatestokencount TYPE i,
             totaltokencount      TYPE i,
           END OF vertexai_usage.

    TYPES: BEGIN OF vertexai_content,
             role  TYPE string,
             parts TYPE vertexai_parts,
           END OF vertexai_content.

    TYPES: BEGIN OF vertexai_candidate,
             content      TYPE vertexai_content,
             finishreason TYPE string,
           END OF vertexai_candidate,
           vertexai_candidates TYPE STANDARD TABLE OF vertexai_candidate WITH EMPTY KEY.

    TYPES: BEGIN OF vertexai_response,
             candidates    TYPE vertexai_candidates,
             usagemetadata TYPE vertexai_usage,
           END OF vertexai_response.
ENDCLASS.



CLASS zcl_llm_client_vertexai IMPLEMENTATION.
  METHOD constructor.
    super->constructor( client_config   = client_config
                        provider_config = provider_config ).
    initialize( ).
  ENDMETHOD.

  METHOD get_client.
    result = NEW zcl_llm_client_vertexai( client_config   = client_config
                                            provider_config = provider_config ).
  ENDMETHOD.

  METHOD get_chat_endpoint.
    result = |{ client_config-provider_model }:generateContent|.
  ENDMETHOD.

  METHOD get_http_client.
    client = zcl_llm_http_client_wrapper=>get_client( client_config   = client_config
                                                       provider_config = provider_config ).
  ENDMETHOD.

  METHOD set_auth.
    " We cannot set any auth header now, we need to get an active token
    " right before every call instead. We just initialize the class.
    auth = NEW zcl_llm_client_vertex_auth( ).
  ENDMETHOD.

  METHOD zif_llm_client_int~chat.
    " Set the auth header, everything else will be handled by the base class
    TRY.
        DATA(token) = auth->get_token( provider_config ).
        client->set_header( name  = 'Authorization'
                            value = |Bearer { token-content }| ) ##NO_TEXT.
        response = super->zif_llm_client_int~chat( request ).
        " We use two different catch entries due to downport issues
      CATCH zcx_llm_http_error INTO DATA(http_error).
        response-success = abap_false.
        response-error-error_text = http_error->get_text( ).
        response-error-retrieable = abap_false.
      CATCH zcx_llm_authorization INTO DATA(auth_error).
        response-success = abap_false.
        response-error-error_text = auth_error->get_text( ).
        response-error-retrieable = abap_false.
    ENDTRY.
  ENDMETHOD.

  METHOD build_request_json.
    " Open content
    result = |\{"contents":[|.
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
      result = |{ result },"systemInstruction":\{"parts":[|.
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
    IF lines( request-tools ) > 0 AND request-tool_choice <> zif_llm_chat_request=>tool_choice_none.
      result = |{ result },"tools":[\{"function_declarations":[|.
      first_line = abap_true.
      LOOP AT request-tools ASSIGNING FIELD-SYMBOL(<tool>).
        DATA(details) = <tool>->get_tool_details( ).
        IF first_line = abap_true.
          result = |{ result }\{"name":"{ details-name }"|
                && |,"description":"{ details-description }","parameters":|
                && tool_parser->parse( data_desc    = details-parameters-data_desc
                                       descriptions = details-parameters-descriptions )
                && |\}|.
          first_line = abap_false.
        ELSE.
          result = |{ result },\{"name":"{ details-name }"|
                && |,"description":"{ details-description }","parameters":|
                && tool_parser->parse( data_desc    = details-parameters-data_desc
                                       descriptions = details-parameters-descriptions )
                && |\}|.
        ENDIF.
      ENDLOOP.

      result = |{ result }]\}]|.

      " Tool choice
      IF request-tool_choice <> zif_llm_chat_request=>tool_choice_none.
        CASE request-tool_choice.
          WHEN zif_llm_chat_request=>tool_choice_auto.
            result = |{ result },"toolConfig":\{"functionCallingConfig":\{"mode":"AUTO"\}\}|.
          WHEN zif_llm_chat_request=>tool_choice_required.
            result = |{ result },"toolConfig":\{"functionCallingConfig":\{"mode":"ANY"\}\}|.
          WHEN OTHERS.
            result = |{ result },"toolConfig":\{"functionCallingConfig":\{"mode":"ANY","allowedFunctionNames":["{ request-tool_choice }"]\}\}|.
        ENDCASE.
      ENDIF.
    ENDIF.

    " Add options if available
    DATA(option_parameters) = request-options->get_paramters( ).
    IF lines( option_parameters ) > 0.
      result = |{ result },"generationConfig":\{|.
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

    " Structured output is part of generationConfig, so we have to check if this exists
    IF request-use_structured_output = abap_true.
      first_line = abap_true.
      IF lines( option_parameters ) > 0.
        DATA(res_length) = strlen( result ) - 1.
        result = result(res_length).
        first_line = abap_false.
      ELSE.
        result = |{ result },"generationConfig":\{|.
      ENDIF.
      IF first_line = abap_false.
        result = |{ result },|.
      ENDIF.
      result = |{ result }"responseMimeType":"application/json"|.
      result = |{ result },"responseSchema": { request-structured_output->get_schema( ) }\}|.
    ENDIF.

    result = |{ result }\}|.
  ENDMETHOD.

  METHOD parse_message.
    IF lines( message-tool_calls ) > 0.
      result = |\{"parts":[|.
      LOOP AT message-tool_calls ASSIGNING FIELD-SYMBOL(<tool_call>).
        IF sy-tabix <> 1.
          result = |{ result },|.
        ENDIF.
        result = |{ result }\{"function_call":\{|
              && |"name":"{ <tool_call>-function-name }",|
              && |"args":{ <tool_call>-function-json_response }\}\}|.
      ENDLOOP.
      result = |{ result }]\}|.
    ELSE.
      DATA role TYPE string.
      CASE message-role.
        WHEN zif_llm_client_int=>role_user.
          role = zif_llm_client_int=>role_user.
        WHEN zif_llm_client_int=>role_assistant OR zif_llm_client_int=>role_tool.
          role = 'model'.
      ENDCASE.
      result = |\{"role":"{ role }","parts":[\{"text":"{
               escape( val    = message-content
                       format = cl_abap_format=>e_json_string ) }"\}]\}|.
    ENDIF.
  ENDMETHOD.

  METHOD zif_llm_client_int~new_request.
    DATA request TYPE zllm_request.

    " Initialize options
    request-options           = NEW zcl_llm_options_vertexai( ).
    " Same for tool parser
    tool_parser = create_tool_parser( ).

    " Initialize structured output using provider-specific implementation
    request-structured_output = create_structured_output( ).

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

    DATA response TYPE vertexai_response.

    zcl_llm_common=>from_json( EXPORTING json = http_response-response
                               CHANGING  data = response ).

    " We should have only one candidate
    IF lines( response-candidates ) <> 1.
      result-success = abap_false.
      result-error-error_text = 'More than one candidate response entry not supported' ##NO_TEXT.
      RETURN.
    ENDIF.

    DATA(candidate) = response-candidates[ 1 ].

    " There can be multiple parts in case of function calls, for text we expect one only
    DATA messages  TYPE string_table.
    DATA functions TYPE vertexai_functions.
    LOOP AT candidate-content-parts ASSIGNING FIELD-SYMBOL(<part>).
      IF <part>-text IS NOT INITIAL.
        APPEND <part>-text TO messages.
      ENDIF.
      IF <part>-functioncall IS NOT INITIAL.
        APPEND <part>-functioncall TO functions.
      ENDIF.
    ENDLOOP.

    result-choice = VALUE #( finish_reason = candidate-finishreason
                             message       = VALUE #( role    = zif_llm_client_int=>role_assistant
                                                      content = concat_lines_of( table = messages
                                                                                 sep   = `\n` ) ) ).

    result-usage  = VALUE #( completion_tokens = response-usagemetadata-candidatestokencount
                             prompt_tokens     = response-usagemetadata-prompttokencount
                             total_tokens      = response-usagemetadata-totaltokencount ).

    " Structured output currently not supported
    IF request-use_structured_output = abap_true.
      parse_structured_output( EXPORTING content  = result-choice-message-content
                                         request  = request
                               CHANGING  response = result ).
    ENDIF.

    " Handle tool calls
    IF request-tool_choice <> zif_llm_chat_request=>tool_choice_none.
      LOOP AT request-tools ASSIGNING FIELD-SYMBOL(<tool>).
        DATA(details) = <tool>->get_tool_details( ).

        LOOP AT functions ASSIGNING FIELD-SYMBOL(<tool_call>) WHERE name = details-name.
          TRY.
              DATA func_result TYPE REF TO data.
              CREATE DATA func_result TYPE HANDLE details-parameters-data_desc.

              " Parse the JSON arguments
              zcl_llm_common=>from_json( EXPORTING json = <tool_call>-args
                                         CHANGING  data = func_result ).

              " Add tool call to response
              APPEND VALUE #( id       = ``
                              type     = zif_llm_tool=>type_function
                              function = VALUE #( name          = details-name
                                                  arguments     = func_result
                                                  json_response = <tool_call>-args ) )
                     TO result-choice-tool_calls.

              result-choice-message = VALUE #( BASE result-choice-message
                                               role    = zif_llm_client_int=>role_tool
                                               name    = details-name
                                               content = <tool_call>-args ).

            CATCH cx_root.
              result-success = abap_false.
              MESSAGE ID 'ZLLM_CLIENT' TYPE 'E' NUMBER 016 WITH <tool_call>-name INTO DATA(message_text).
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

ENDCLASS.
