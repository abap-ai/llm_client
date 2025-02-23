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
    sql_test_double = cl_osql_test_environment=>create(
        VALUE #( ( 'ZLLM_CLNT_CONFIG' )
                ( 'ZLLM_PROVIDERS' ) ) ).

    " Prepare test data for client configuration
    DATA clnt_config TYPE STANDARD TABLE OF zllm_clnt_config.
    clnt_config = VALUE #(
      ( model = valid_model provider_name = provider_name ) ).
    APPEND VALUE #( model = invalid_model provider_name = 'INVALID_PROVIDER' ) TO clnt_config.
    DATA(client_config_data) = clnt_config.

    " Prepare test data for provider configuration
    DATA provider_config TYPE STANDARD TABLE OF zllm_providers.
    provider_config = VALUE #(
      ( provider_name = provider_name
        provider_class = provider_name )
    ).
    DATA(provider_config_data) = provider_config.

    " Mock the database tables content
    sql_test_double->insert_test_data( client_config_data ).
    sql_test_double->insert_test_data( provider_config_data ).
  ENDMETHOD.

  METHOD class_teardown.
    sql_test_double->destroy( ).
  ENDMETHOD.

  METHOD get_client_valid_model.
    DATA client TYPE REF TO zif_llm_client_int.

    TRY.
        client = zcl_llm_factory=>zif_llm_factory~get_client_int( valid_model ).

        cl_abap_unit_assert=>assert_bound(
          act = client
          msg = 'Client should be created for valid model' ).

      CATCH zcx_llm_validation INTO DATA(error).
        cl_abap_unit_assert=>fail( 'Unexpected validation error' ).
      CATCH zcx_llm_authorization INTO DATA(auth_error).
        cl_abap_unit_assert=>fail( 'Unexpected authorization error' ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_client_invalid_model.
    DATA client TYPE REF TO zif_llm_client_int.

    TRY.
        client = zcl_llm_factory=>zif_llm_factory~get_client_int( invalid_model ).
        cl_abap_unit_assert=>fail( 'Expected validation error not raised' ).

      CATCH zcx_llm_validation INTO DATA(error).
        cl_abap_unit_assert=>assert_equals(
          exp = zcx_llm_validation=>model_does_not_exist
          act = error->model_does_not_exist
          msg = 'Wrong exception raised for invalid model' ).
      CATCH zcx_llm_authorization INTO DATA(auth_error).
        cl_abap_unit_assert=>fail( 'Unexpected authorization error' ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_client_invalid_provider.

    TRY.
        DATA(client) = zcl_llm_factory=>zif_llm_factory~get_client_int( invalid_model ).
        cl_abap_unit_assert=>fail( 'Expected validation error not raised' ).

      CATCH zcx_llm_validation INTO DATA(error).
        cl_abap_unit_assert=>assert_equals(
          exp = zcx_llm_validation=>provider_does_not_exist
          act = error->provider_does_not_exist
          msg = 'Wrong exception raised for invalid provider' ).
      CATCH zcx_llm_authorization INTO DATA(auth_error).
        cl_abap_unit_assert=>fail( 'Unexpected authorization error' ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_mock_llm_client DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES zif_llm_client_int.
ENDCLASS.

CLASS ltcl_mock_llm_client IMPLEMENTATION.
  METHOD zif_llm_client_int~chat.
  ENDMETHOD.                                       "#EC EMPTY_PROCEDURE

  METHOD zif_llm_client_int~new_request.
  ENDMETHOD.                                       "#EC EMPTY_PROCEDURE

  METHOD zif_llm_client_int~get_client.
    response = NEW ltcl_mock_llm_client( ).
  ENDMETHOD.
ENDCLASS.
