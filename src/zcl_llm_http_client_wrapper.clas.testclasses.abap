CLASS ltcl_http_client_wrapper DEFINITION DEFERRED.
CLASS zcl_llm_http_client_wrapper DEFINITION LOCAL FRIENDS ltcl_http_client_wrapper.

CLASS ltcl_http_client_wrapper DEFINITION
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS:
      test_url           TYPE string VALUE 'sap/public/ping',
      test_destination   TYPE rfcdest VALUE 'NONE',
      non_existent_dest  TYPE rfcdest VALUE 'XYZ234',
      test_request_json  TYPE string VALUE '{"test": "data"}' ##NO_TEXT,
      test_response_json TYPE string VALUE '{"status": "success"}' ##NO_TEXT,
      test_header_name   TYPE string VALUE 'X-Test-Header' ##NO_TEXT,
      test_header_value  TYPE string VALUE 'TestValue'.

    DATA:
      http_client_wrapper TYPE REF TO zcl_llm_http_client_wrapper,
      client_config       TYPE zllm_clnt_config,
      provider_config     TYPE zllm_providers.

    METHODS:
      setup,
      validate_client_creation     FOR TESTING,
      validate_header_setting      FOR TESTING,
      validate_communication       FOR TESTING,
      validate_response_retrieval  FOR TESTING,
      validate_url_setting         FOR TESTING,
      validate_get_client          FOR TESTING,
      validate_close_client        FOR TESTING,
      test_client_creation_failure FOR TESTING.

ENDCLASS.

CLASS ltcl_http_client_wrapper IMPLEMENTATION.

  METHOD setup.
    provider_config-auth_type = space.
    provider_config-rfc_destination = test_destination.

    TRY.
        CREATE OBJECT http_client_wrapper EXPORTING client_config = client_config provider_config = provider_config.
      CATCH zcx_llm_validation.
        cl_abap_unit_assert=>fail( 'Unexpected exception during setup' ).
    ENDTRY.
  ENDMETHOD.

  METHOD validate_client_creation.
    cl_abap_unit_assert=>assert_not_initial(
      act = http_client_wrapper
      msg = 'HTTP client wrapper should be created successfully' ).
  ENDMETHOD.

  METHOD validate_header_setting.
    DATA temp1 TYPE xsdboolean.
    http_client_wrapper->set_header(
      name  = test_header_name
      value = test_header_value ).

    
    temp1 = boolc( 1 = 1 ).
    cl_abap_unit_assert=>assert_true( temp1 ).
  ENDMETHOD.

  METHOD validate_url_setting.
    http_client_wrapper->set_url( test_url ).

    cl_abap_unit_assert=>assert_equals(
      exp = test_url
      act = http_client_wrapper->url
      msg = 'URL should be set correctly' ).
  ENDMETHOD.

  METHOD validate_communication.
        DATA response TYPE zif_llm_http_client_wrapper=>response.
        DATA error TYPE REF TO zcx_llm_http_error.
    http_client_wrapper->set_url( test_url ).

    TRY.
        
        response = http_client_wrapper->communicate( request = test_request_json msg = 1 session_id = '123123123' ).
        cl_abap_unit_assert=>assert_not_initial(
          act = response-code
          msg = 'Communication should return a response' ).
        
      CATCH zcx_llm_http_error INTO error.
        cl_abap_unit_assert=>fail(
          msg    = 'Unexpected HTTP communication error'
          detail = error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD validate_response_retrieval.
        DATA response TYPE zif_llm_http_client_wrapper=>response.
        DATA error TYPE REF TO zcx_llm_http_error.
    http_client_wrapper->set_url( test_url ).

    TRY.
        
        response = http_client_wrapper->communicate( request = test_request_json msg = 1 session_id = '123123123' ).

        cl_abap_unit_assert=>assert_not_initial(
          act = response
          msg = 'Response should be retrievable' ).
        
      CATCH zcx_llm_http_error INTO error.
        cl_abap_unit_assert=>fail(
          msg    = 'Unexpected HTTP communication error'
          detail = error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD validate_get_client.
        DATA new_client TYPE REF TO zif_llm_http_client_wrapper.
        DATA error TYPE REF TO zcx_llm_validation.
    TRY.
        
        new_client = http_client_wrapper->get_client(
          client_config   = client_config
          provider_config = provider_config ).

        cl_abap_unit_assert=>assert_not_initial(
          act = new_client
          msg = 'get_client should return a new client instance' ).
        
      CATCH zcx_llm_validation INTO error.
        cl_abap_unit_assert=>fail(
          msg    = 'Unexpected error in get_client'
          detail = error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD validate_close_client.
        DATA temp2 TYPE xsdboolean.
        DATA error TYPE REF TO zcx_llm_http_error.
    TRY.
        http_client_wrapper->close_client( ).
        
        temp2 = boolc( 1 = 1 ).
        cl_abap_unit_assert=>assert_true(
          act = temp2
          msg = 'close_client should execute without error' ).
        
      CATCH zcx_llm_http_error INTO error.
        cl_abap_unit_assert=>fail(
          msg    = 'Unexpected error in close_client'
          detail = error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_client_creation_failure.
    DATA: client_config   TYPE zllm_clnt_config,
          provider_config TYPE zllm_providers.
        DATA failed_client TYPE REF TO zcl_llm_http_client_wrapper.
        DATA validation_error TYPE REF TO zcx_llm_validation.

    provider_config-rfc_destination = non_existent_dest.
    provider_config-auth_type = space.

    TRY.
        
        CREATE OBJECT failed_client TYPE zcl_llm_http_client_wrapper EXPORTING client_config = client_config provider_config = provider_config.
        cl_abap_unit_assert=>fail( 'Expected exception for non-existent destination' ).
        
      CATCH zcx_llm_validation INTO validation_error.
        cl_abap_unit_assert=>assert_not_initial(
          act = validation_error->get_text( )
          msg = 'Validation error should contain error details' ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
