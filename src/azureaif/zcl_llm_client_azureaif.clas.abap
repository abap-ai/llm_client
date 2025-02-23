"! <p class="shorttext synchronized" lang="en">Azure AI Foundry Client</p>
CLASS zcl_llm_client_azureaif DEFINITION
  PUBLIC
  INHERITING FROM zcl_llm_client_base
  CREATE PRIVATE.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Get client instance</p>
    "! Factory method to get a new Azure OpenAI client instance
    "! @parameter client_config         | <p class="shorttext synchronized">Client configuration</p>
    "! @parameter provider_config       | <p class="shorttext synchronized">Provider configuration</p>
    "! @parameter result                | <p class="shorttext synchronized">Client instance</p>
    "! @raising   zcx_llm_validation    | <p class="shorttext synchronized">Client validation error</p>
    "! @raising   zcx_llm_authorization | <p class="shorttext synchronized">Authorization error</p>
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
    CONSTANTS api_version TYPE string VALUE '2024-05-01-preview'.

    METHODS get_http_client          REDEFINITION.
    METHODS set_auth                 REDEFINITION.
    METHODS create_structured_output REDEFINITION.
    METHODS get_chat_endpoint        REDEFINITION.
    METHODS build_request_json       REDEFINITION.

  PRIVATE SECTION.

ENDCLASS.

CLASS zcl_llm_client_azureaif IMPLEMENTATION.
  METHOD constructor.
    super->constructor( client_config   = client_config
                        provider_config = provider_config ).
    initialize( ).
  ENDMETHOD.

  METHOD get_client.
    result = NEW zcl_llm_client_azureaif( client_config   = client_config
                                          provider_config = provider_config ).
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
    client->set_header( name  = 'api-key'
                        value = auth_value ).
  ENDMETHOD.

  METHOD create_structured_output.
    result = NEW zcl_llm_so_js_azureoai( ).
  ENDMETHOD.

  METHOD get_chat_endpoint.
    result = |/chat/completions?api-version={ api_version }|.
  ENDMETHOD.

  METHOD build_request_json.
    " Open content
    result = |\{"model":"{ client_config-provider_model }","messages":[|.
    DATA first_line TYPE abap_bool VALUE abap_true.

    DATA(messages) = request-messages.

    " Add messages
    " Note that Azure AI Foundry does not support defining the structured output in the API.
    " We need to set it to json mode and in addition specify the format in the prompt.
    DATA(lines) = lines( messages ).
    DATA current_line TYPE i.
    LOOP AT messages INTO DATA(message).
      current_line = current_line + 1.
      IF current_line = lines AND request-use_structured_output = abap_true.
        " As mentioned above add the json format to the prompt.
        message-content = |{ message-content } Respond in JSON based on the following schema: |
                         && |{ request-structured_output->get_schema( ) }| ##NO_TEXT.
      ENDIF.
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
      result = |{ result },"response_format":\{"type":"json_object"\}|.
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
                && |\}\}|.
          first_line = abap_false.
        ELSE.
          result = |{ result },\{"type":"{ details-type }","{ details-type }":\{"name":"{ details-name }"|
                && |,"description":"{ details-description }","parameters":|
                && tool_parser->parse( data_desc    = details-parameters-data_desc
                                       descriptions = details-parameters-descriptions )
                && |\}\}|.
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

ENDCLASS.
