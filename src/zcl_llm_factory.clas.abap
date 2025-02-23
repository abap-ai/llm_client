"! <p class="shorttext synchronized" lang="en">LLM Factory</p>
CLASS zcl_llm_factory DEFINITION
  PUBLIC
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_llm_factory.

    ALIASES get_client     FOR zif_llm_factory~get_client.
    ALIASES get_client_int FOR zif_llm_factory~get_client_int.

    CLASS-METHODS class_constructor.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CLASS-DATA auth_class TYPE REF TO zif_llm_auth.
ENDCLASS.

CLASS zcl_llm_factory IMPLEMENTATION.
  METHOD zif_llm_factory~get_client.
    DATA(int_client) = zif_llm_factory~get_client_int( model ).
    response = NEW zcl_llm_client( int_client ).
  ENDMETHOD.

  METHOD class_constructor.
    DATA(llm_badi) = zcl_llm_common=>get_llm_badi( ).
    CALL BADI llm_badi->get_authorization_impl
      RECEIVING
        result = auth_class.
  ENDMETHOD.

  METHOD zif_llm_factory~get_client_int.
    auth_class->check_get_client( model ).
    SELECT SINGLE * FROM zllm_clnt_config WHERE model = @model INTO @DATA(client_configuration).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_llm_validation( textid = zcx_llm_validation=>model_does_not_exist
                                              attr1  = CONV string( model ) ).
    ENDIF.

    SELECT SINGLE * FROM zllm_providers
      WHERE provider_name = @client_configuration-provider_name
      INTO @DATA(provider_configuration).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_llm_validation( textid = zcx_llm_validation=>provider_does_not_exist
                                              attr1  = CONV string( client_configuration-provider_name ) ).
    ENDIF.

    CALL METHOD (provider_configuration-provider_class)=>('ZIF_LLM_CLIENT_INT~GET_CLIENT')
      EXPORTING
        client_config   = client_configuration
        provider_config = provider_configuration
      RECEIVING
        response        = response.
  ENDMETHOD.

ENDCLASS.
