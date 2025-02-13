CLASS ltcl_llm_factory DEFINITION
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS:
      valid_model   TYPE zllm_model VALUE 'VALID_MODEL',
      invalid_model TYPE zllm_model VALUE 'INVALID_MODEL',
      provider_name TYPE string VALUE 'LTCL_MOCK_LLM_CLIENT'.

    CLASS-DATA:
      sql_test_double TYPE REF TO if_osql_test_environment.

    METHODS:
      get_client_valid_model FOR TESTING,
      get_client_invalid_model FOR TESTING,
      get_client_invalid_provider FOR TESTING.

    CLASS-METHODS:
      class_setup,
      class_teardown.

ENDCLASS.

CLASS ltcl_llm_factory IMPLEMENTATION.

  METHOD class_setup.
    " Create SQL test double environment for both tables
    DATA temp3 TYPE if_osql_test_environment=>ty_t_sobjnames.
    DATA clnt_config TYPE STANDARD TABLE OF zllm_clnt_config.
    DATA temp5 LIKE clnt_config.
    DATA temp6 LIKE LINE OF temp5.
    DATA temp7 TYPE zllm_clnt_config.
    DATA client_config_data LIKE clnt_config.
    DATA provider_config TYPE STANDARD TABLE OF zllm_providers.
    DATA temp8 LIKE provider_config.
    DATA temp9 LIKE LINE OF temp8.
    DATA provider_config_data LIKE provider_config.
    CLEAR temp3.
    INSERT 'ZLLM_CLNT_CONFIG' INTO TABLE temp3.
    INSERT 'ZLLM_PROVIDERS' INTO TABLE temp3.
    sql_test_double = cl_osql_test_environment=>create(
        temp3 ).

    " Prepare test data for client configuration
    
    
    CLEAR temp5.
    
    temp6-model = valid_model.
    temp6-provider_name = provider_name.
    INSERT temp6 INTO TABLE temp5.
    clnt_config = temp5.
    
    CLEAR temp7.
    temp7-model = invalid_model.
    temp7-provider_name = 'INVALID_PROVIDER'.
    APPEND temp7 TO clnt_config.
    
    client_config_data = clnt_config.

    " Prepare test data for provider configuration
    
    
    CLEAR temp8.
    
    temp9-provider_name = provider_name.
    temp9-provider_class = provider_name.
    INSERT temp9 INTO TABLE temp8.
    provider_config = temp8.
    
    provider_config_data = provider_config.

    " Mock the database tables content
    sql_test_double->insert_test_data( client_config_data ).
    sql_test_double->insert_test_data( provider_config_data ).
  ENDMETHOD.

  METHOD class_teardown.
    sql_test_double->destroy( ).
  ENDMETHOD.

  METHOD get_client_valid_model.
    DATA client TYPE REF TO zif_llm_client.
        DATA error TYPE REF TO zcx_llm_validation.
        DATA auth_error TYPE REF TO zcx_llm_authorization.

    TRY.
        client = zcl_llm_factory=>zif_llm_factory~get_client( valid_model ).

        cl_abap_unit_assert=>assert_bound(
          act = client
          msg = 'Client should be created for valid model' ).

        
      CATCH zcx_llm_validation INTO error.
        cl_abap_unit_assert=>fail( 'Unexpected validation error' ).
        
      CATCH zcx_llm_authorization INTO auth_error.
        cl_abap_unit_assert=>fail( 'Unexpected authorization error' ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_client_invalid_model.
    DATA client TYPE REF TO zif_llm_client.
        DATA error TYPE REF TO zcx_llm_validation.
        DATA auth_error TYPE REF TO zcx_llm_authorization.

    TRY.
        client = zcl_llm_factory=>zif_llm_factory~get_client( invalid_model ).
        cl_abap_unit_assert=>fail( 'Expected validation error not raised' ).

        
      CATCH zcx_llm_validation INTO error.
        cl_abap_unit_assert=>assert_equals(
          exp = zcx_llm_validation=>model_does_not_exist
          act = error->model_does_not_exist
          msg = 'Wrong exception raised for invalid model' ).
        
      CATCH zcx_llm_authorization INTO auth_error.
        cl_abap_unit_assert=>fail( 'Unexpected authorization error' ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_client_invalid_provider.
        DATA client TYPE REF TO zif_llm_client.
        DATA error TYPE REF TO zcx_llm_validation.
        DATA auth_error TYPE REF TO zcx_llm_authorization.

    TRY.
        
        client = zcl_llm_factory=>zif_llm_factory~get_client( invalid_model ).
        cl_abap_unit_assert=>fail( 'Expected validation error not raised' ).

        
      CATCH zcx_llm_validation INTO error.
        cl_abap_unit_assert=>assert_equals(
          exp = zcx_llm_validation=>provider_does_not_exist
          act = error->provider_does_not_exist
          msg = 'Wrong exception raised for invalid provider' ).
        
      CATCH zcx_llm_authorization INTO auth_error.
        cl_abap_unit_assert=>fail( 'Unexpected authorization error' ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_mock_llm_client DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES zif_llm_client.
ENDCLASS.

CLASS ltcl_mock_llm_client IMPLEMENTATION.
  METHOD zif_llm_client~chat.
  ENDMETHOD. "#EC EMPTY_PROCEDURE

  METHOD zif_llm_client~new_request.
  ENDMETHOD. "#EC EMPTY_PROCEDURE

  METHOD zif_llm_client~get_client.
    CREATE OBJECT response TYPE ltcl_mock_llm_client.
  ENDMETHOD.
ENDCLASS.
