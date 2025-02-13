CLASS ltcl_llm_auth_disabled DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      auth_checker TYPE REF TO zif_llm_auth.

    METHODS:
      setup,
      test_check_decrypt FOR TESTING,
      test_check_encrypt FOR TESTING,
      test_check_get_client FOR TESTING,
      test_check_provider_maint FOR TESTING.
ENDCLASS.


CLASS ltcl_llm_auth_disabled IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT auth_checker TYPE zcl_llm_auth_disabled.
  ENDMETHOD.

  METHOD test_check_decrypt.
        DATA auth_error TYPE REF TO zcx_llm_authorization.
    TRY.
        auth_checker->check_decrypt( ).

        cl_abap_unit_assert=>assert_true(
          act = abap_true
          msg = 'Check decrypt should not raise any exception' ).

        
      CATCH zcx_llm_authorization INTO auth_error.
        cl_abap_unit_assert=>fail( |Unexpected authorization exception: { auth_error->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_check_encrypt.
        DATA auth_error TYPE REF TO zcx_llm_authorization.
    TRY.
        auth_checker->check_encrypt( ).

        cl_abap_unit_assert=>assert_true(
          act = abap_true
          msg = 'Check encrypt should not raise any exception' ).

        
      CATCH zcx_llm_authorization INTO auth_error.
        cl_abap_unit_assert=>fail( |Unexpected authorization exception: { auth_error->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_check_get_client.
        DATA auth_error TYPE REF TO zcx_llm_authorization.
    TRY.
        auth_checker->check_get_client( 'TEST' ).

        cl_abap_unit_assert=>assert_true(
          act = abap_true
          msg = 'Check get client should not raise any exception' ).

        
      CATCH zcx_llm_authorization INTO auth_error.
        cl_abap_unit_assert=>fail( |Unexpected authorization exception: { auth_error->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_check_provider_maint.
        DATA auth_error TYPE REF TO zcx_llm_authorization.
    TRY.
        auth_checker->check_provider_maintenance( ).

        cl_abap_unit_assert=>assert_true(
          act = abap_true
          msg = 'Check provider maintenance should not raise any exception' ).

        
      CATCH zcx_llm_authorization INTO auth_error.
        cl_abap_unit_assert=>fail( |Unexpected authorization exception: { auth_error->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
