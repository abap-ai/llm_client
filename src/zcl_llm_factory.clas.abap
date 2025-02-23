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
    DATA int_client TYPE REF TO zif_llm_client_int.
    int_client = zif_llm_factory~get_client_int( model ).
    CREATE OBJECT response TYPE zcl_llm_client EXPORTING INT_CLIENT = int_client.
  ENDMETHOD.

  METHOD class_constructor.
    DATA(llm_badi) = zcl_llm_common=>get_llm_badi( ).
    CALL BADI llm_badi->get_authorization_impl
      RECEIVING
        result = auth_class.
  ENDMETHOD.

  METHOD zif_llm_factory~get_client_int.
    DATA client_configuration TYPE zllm_clnt_config.
      DATA temp1 TYPE string.
      DATA temp3 TYPE REF TO zcx_llm_validation.
    DATA provider_configuration TYPE zllm_providers.
      DATA temp2 TYPE string.
      DATA temp4 TYPE REF TO zcx_llm_validation.
    auth_class->check_get_client( model ).
    
    SELECT SINGLE * FROM zllm_clnt_config INTO client_configuration WHERE model = model .
    IF sy-subrc <> 0.
      
      temp1 = model.
      
      CREATE OBJECT temp3 TYPE zcx_llm_validation EXPORTING textid = zcx_llm_validation=>model_does_not_exist attr1 = temp1.
      RAISE EXCEPTION temp3.
    ENDIF.

    
    SELECT SINGLE * FROM zllm_providers INTO provider_configuration
      WHERE provider_name = client_configuration-provider_name
      .
    IF sy-subrc <> 0.
      
      temp2 = client_configuration-provider_name.
      
      CREATE OBJECT temp4 TYPE zcx_llm_validation EXPORTING textid = zcx_llm_validation=>provider_does_not_exist attr1 = temp2.
      RAISE EXCEPTION temp4.
    ENDIF.

    CALL METHOD (provider_configuration-provider_class)=>('ZIF_LLM_CLIENT_INT~GET_CLIENT')
      EXPORTING
        client_config   = client_configuration
        provider_config = provider_configuration
      RECEIVING
        response        = response.
  ENDMETHOD.

ENDCLASS.
