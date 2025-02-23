CLASS zcl_llm_client_azureoai DEFINITION
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
    CONSTANTS api_version TYPE string VALUE '2024-10-21'.

    METHODS get_http_client          REDEFINITION.
    METHODS set_auth                 REDEFINITION.
    METHODS create_structured_output REDEFINITION.
    METHODS get_chat_endpoint        REDEFINITION.

  PRIVATE SECTION.

ENDCLASS.

CLASS zcl_llm_client_azureoai IMPLEMENTATION.
  METHOD constructor.
    super->constructor( client_config   = client_config
                        provider_config = provider_config ).
    initialize( ).
  ENDMETHOD.

  METHOD get_client.
    result = NEW zcl_llm_client_azureoai( client_config   = client_config
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
    result = |/{ client_config-provider_model }/chat/completions?api-version={ api_version }|.
  ENDMETHOD.

ENDCLASS.
