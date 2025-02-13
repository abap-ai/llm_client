"! <p class="shorttext synchronized" lang="en">Gemini Client</p>
"! This client uses mostly the vertex logic with a few changes
CLASS zcl_llm_client_gemini DEFINITION
 PUBLIC
  INHERITING FROM zcl_llm_client_base
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS get_client
      IMPORTING client_config   TYPE zllm_clnt_config
                provider_config TYPE zllm_providers
      RETURNING VALUE(result)   TYPE REF TO zif_llm_client
      RAISING   zcx_llm_validation
                zcx_llm_authorization.

    METHODS constructor
      IMPORTING client_config   TYPE zllm_clnt_config
                provider_config TYPE zllm_providers
      RAISING   zcx_llm_validation
                zcx_llm_authorization.
    METHODS zif_llm_client~new_request REDEFINITION.
    METHODS zif_llm_client~chat REDEFINITION.
  PROTECTED SECTION.
    METHODS: get_http_client REDEFINITION,
      set_auth REDEFINITION,
      get_chat_endpoint REDEFINITION,
      build_request_json REDEFINITION,
      parse_message REDEFINITION,
      handle_http_response REDEFINITION,
      create_structured_output REDEFINITION,
      create_tool_parser REDEFINITION.
  PRIVATE SECTION.

    " VertexAI messages
    TYPES: BEGIN OF vertexai_function,
             name TYPE string,
             " We need this as pure json content in order to parse it later
             args TYPE /ui2/cl_json=>json,
           END OF vertexai_function,
           vertexai_functions TYPE STANDARD TABLE OF vertexai_function WITH DEFAULT KEY.

    TYPES: BEGIN OF vertexai_part,
             text         TYPE string,
             functioncall TYPE vertexai_function,
           END OF vertexai_part,
           vertexai_parts TYPE STANDARD TABLE OF vertexai_part WITH DEFAULT KEY.

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
           vertexai_candidates TYPE STANDARD TABLE OF vertexai_candidate WITH DEFAULT KEY.

    TYPES: BEGIN OF vertexai_response,
             candidates    TYPE vertexai_candidates,
             usagemetadata TYPE vertexai_usage,
           END OF vertexai_response.

    DATA api_key TYPE string.

ENDCLASS.

CLASS zcl_llm_client_gemini IMPLEMENTATION.
  METHOD constructor.
    super->constructor( client_config   = client_config
                        provider_config = provider_config ).
    initialize( ).
  ENDMETHOD.

  METHOD create_structured_output.
    CREATE OBJECT result TYPE zcl_llm_so_ge.
  ENDMETHOD.

  METHOD get_client.
    CREATE OBJECT result TYPE zcl_llm_client_gemini EXPORTING client_config = client_config provider_config = provider_config.
  ENDMETHOD.

  METHOD get_chat_endpoint.
    result = |/{ client_config-provider_model }:generateContent|.
  ENDMETHOD.

  METHOD get_http_client.
    client = zcl_llm_http_client_wrapper=>get_client( client_config   = client_config
                                                       provider_config = provider_config ).
  ENDMETHOD.

  METHOD set_auth.
    " Not handled here, need to do it on every call
  ENDMETHOD.

  METHOD zif_llm_client~chat.
      DATA error TYPE REF TO ZCX_LLM_HTTP_ERROR.
    " Set the auth parameter, everything else will be handled by the base class
    TRY.
        IF api_key IS INITIAL.
          IF provider_config-auth_encrypted IS NOT INITIAL.
            DATA(llm_badi) = zcl_llm_common=>get_llm_badi( ).
            CALL BADI llm_badi->get_encryption_impl
              RECEIVING
                result = DATA(enc_class).
            api_key = enc_class->decrypt( provider_config-auth_encrypted ).
          ENDIF.
        ENDIF.
        client->set_parmeter( name = 'key' value = api_key ).
        response = super->zif_llm_client~chat( request ).
      
      CATCH zcx_llm_http_error
            zcx_llm_authorization INTO error.
        response-success = abap_false.
        response-error-error_text = error->get_text( ).
        response-error-retrieable = abap_false.
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
        DATA res_length TYPE i.
    " Open content
    result = |\{"contents":[|.
    

    " Anthropic handles system messages differently
    
    

    
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
      result = |{ result },"systemInstruction":\{"parts":[|.
      
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
    IF lines( request-tools ) > 0 AND request-tool_choice <> zif_llm_chat_request=>tool_choice_none.
      result = |{ result },"tools":[\{"function_declarations":[|.
      first_line = abap_true.
      
      LOOP AT request-tools ASSIGNING <tool>.
        
        details = <tool>->get_tool_details( ).
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
    
    option_parameters = request-options->get_paramters( ).
    IF lines( option_parameters ) > 0.
      result = |{ result },"generationConfig":\{|.
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

    " Structured output is part of generationConfig, so we have to check if this exists
    IF request-use_structured_output = abap_true.
      first_line = abap_true.
      IF lines( option_parameters ) > 0.
        
        res_length = strlen( result ) - 1.
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
      FIELD-SYMBOLS <tool_call> LIKE LINE OF message-tool_calls.
      DATA role TYPE string.
    IF lines( message-tool_calls ) > 0.
      result = |\{"parts":[|.
      
      LOOP AT message-tool_calls ASSIGNING <tool_call>.
        IF sy-tabix <> 1.
          result = |{ result },|.
        ENDIF.
        result = |{ result }\{"function_call":\{|
              && |"name":"{ <tool_call>-function-name }",|
              && |"args":{ <tool_call>-function-json_response }\}\}|.
      ENDLOOP.
      result = |{ result }]\}|.
    ELSE.
      
      CASE message-role.
        WHEN zif_llm_client=>role_user.
          role = zif_llm_client=>role_user.
        WHEN zif_llm_client=>role_assistant OR zif_llm_client=>role_tool.
          role = 'model'.
      ENDCASE.
      result = |\{"role":"{ role }","parts":[\{"text":"{
               escape( val    = message-content
                       format = cl_abap_format=>e_json_string ) }"\}]\}|.
    ENDIF.
  ENDMETHOD.

  METHOD zif_llm_client~new_request.
    DATA request TYPE zllm_request.
      TYPES temp2 TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA options TYPE temp2.
      DATA parameters TYPE zllm_keyvalues.
      DATA option LIKE LINE OF options.
        DATA key TYPE string.
        DATA value TYPE string.
        DATA temp1 TYPE zllm_keyvalue.

    " Initialize options
    CREATE OBJECT request-options TYPE zcl_llm_options_vertexai.

    " Initialize structured output using provider-specific implementation
    request-structured_output = create_structured_output( ).
    " Same for tool parser
    tool_parser = create_tool_parser( ).

    " Get configured default parameters and set them
    IF client_config-default_op IS NOT INITIAL.
      

      SPLIT client_config-default_op AT ';' INTO TABLE options.
      

      
      LOOP AT options INTO option.
        
        
        SPLIT option AT ':' INTO key value.
        
        CLEAR temp1.
        temp1-key = key.
        temp1-value = value.
        INSERT temp1
               INTO TABLE parameters.
      ENDLOOP.

      request-options->set_custom_parameters( parameters ).
    ENDIF.

    " Create and return chat request object
    CREATE OBJECT response TYPE zcl_llm_chat_request EXPORTING REQUEST = request.
  ENDMETHOD.

  METHOD handle_http_response.
    DATA response TYPE vertexai_response.
    DATA candidate TYPE zcl_llm_client_gemini=>vertexai_candidate.
    DATA temp1 LIKE LINE OF response-candidates.
    DATA temp4 LIKE sy-tabix.
    DATA messages TYPE string_table.
    DATA functions TYPE vertexai_functions.
    FIELD-SYMBOLS <part> LIKE LINE OF candidate-content-parts.
      FIELD-SYMBOLS <tool> LIKE LINE OF request-tools.
        DATA details TYPE zif_llm_tool=>tool_details.
        FIELD-SYMBOLS <tool_call> LIKE LINE OF functions.
              DATA func_result TYPE REF TO data.
              DATA temp2 TYPE zllm_tool_call.
              DATA temp3 TYPE zllm_msg.
              DATA message_text TYPE string.
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

    " We should have only one candidate
    IF lines( response-candidates ) <> 1.
      result-success = abap_false.
      result-error-error_text = 'More than one candidate response entry not supported' ##NO_TEXT.
      RETURN.
    ENDIF.

    
    
    
    temp4 = sy-tabix.
    READ TABLE response-candidates INDEX 1 INTO temp1.
    sy-tabix = temp4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    candidate = temp1.

    " There can be multiple parts in case of function calls, for text we expect one only
    
    
    
    LOOP AT candidate-content-parts ASSIGNING <part>.
      IF <part>-text IS NOT INITIAL.
        APPEND <part>-text TO messages.
      ENDIF.
      IF <part>-functioncall IS NOT INITIAL.
        APPEND <part>-functioncall TO functions.
      ENDIF.
    ENDLOOP.

    CLEAR result-choice.
    result-choice-finish_reason = candidate-finishreason.
    CLEAR result-choice-message.
    result-choice-message-role = zif_llm_client=>role_assistant.
    result-choice-message-content = concat_lines_of( table = messages
sep = `\n` ).

    CLEAR result-usage.
    result-usage-completion_tokens = response-usagemetadata-candidatestokencount.
    result-usage-prompt_tokens = response-usagemetadata-prompttokencount.
    result-usage-total_tokens = response-usagemetadata-totaltokencount.

    " Structured output currently not supported
    IF request-use_structured_output = abap_true.
      parse_structured_output( EXPORTING content  = result-choice-message-content
                                         request  = request
                               CHANGING  response = result ).
    ENDIF.

    " Handle tool calls
    IF request-tool_choice <> zif_llm_chat_request=>tool_choice_none.
      
      LOOP AT request-tools ASSIGNING <tool>.
        
        details = <tool>->get_tool_details( ).

        
        LOOP AT functions ASSIGNING <tool_call> WHERE name = details-name.
          TRY.
              
              CREATE DATA func_result TYPE HANDLE details-parameters-data_desc.

              " Parse the JSON arguments
              zcl_llm_common=>from_json( EXPORTING json = <tool_call>-args
                                         CHANGING  data = func_result ).

              " Add tool call to response
              
              CLEAR temp2.
              temp2-id = ``.
              temp2-type = zif_llm_tool=>type_function.
              CLEAR temp2-function.
              temp2-function-name = details-name.
              temp2-function-arguments = func_result.
              temp2-function-json_response = <tool_call>-args.
              APPEND temp2
                     TO result-choice-tool_calls.

              
              CLEAR temp3.
              temp3 = result-choice-message.
              temp3-role = zif_llm_client=>role_tool.
              temp3-name = details-name.
              temp3-content = <tool_call>-args.
              result-choice-message = temp3.

            CATCH cx_root.
              result-success = abap_false.
              
              MESSAGE ID 'ZLLM_CLIENT' TYPE 'E' NUMBER 016 WITH <tool_call>-name INTO message_text.
              CLEAR result-error.
              result-error-tool_parse_error = abap_true.
              result-error-error_text = message_text.
              RETURN.
          ENDTRY.
        ENDLOOP.

        " This is only an issue if tool call is required
        IF sy-subrc <> 0 AND request-tool_choice = zif_llm_chat_request=>tool_choice_required.
          result-success = abap_false.
          MESSAGE ID 'ZLLM_CLIENT' TYPE 'E' NUMBER 017 WITH details-name INTO message_text.
          CLEAR result-error.
          result-error-tool_parse_error = abap_true.
          result-error-error_text = message_text.
          RETURN.
        ENDIF.
      ENDLOOP.
    ENDIF.

    result-success = abap_true.
  ENDMETHOD.

  METHOD create_tool_parser.
    CREATE OBJECT result TYPE zcl_llm_tool_parser_gemini.
  ENDMETHOD.

ENDCLASS.
