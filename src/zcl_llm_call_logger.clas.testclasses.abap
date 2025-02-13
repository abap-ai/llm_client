CLASS ltcl_llm_call_logger DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      cut        TYPE REF TO zcl_llm_call_logger,
      test_entry TYPE zllm_call_log.

    CLASS-DATA:
      environment TYPE REF TO if_osql_test_environment.

    CLASS-METHODS:
      class_setup,
      class_teardown.

    METHODS:
      setup,
      teardown,
      given_system_settings IMPORTING active       TYPE sap_bool
                                      filter_uname TYPE syuname,
      when_adding_log_entry,
      then_entry_is_logged IMPORTING expected_count TYPE i,
      add_when_active FOR TESTING,
      add_when_inactive FOR TESTING,
      add_with_user_filter FOR TESTING.
ENDCLASS.


CLASS ltcl_llm_call_logger IMPLEMENTATION.

  METHOD class_setup.
    DATA temp1 TYPE if_osql_test_environment=>ty_t_sobjnames.
    CLEAR temp1.
    INSERT 'ZLLM_SYSTEM' INTO TABLE temp1.
    INSERT 'ZLLM_CALL_LOG' INTO TABLE temp1.
    environment = cl_osql_test_environment=>create( temp1 ).
  ENDMETHOD.

  METHOD class_teardown.
    environment->destroy( ).
  ENDMETHOD.

  METHOD setup.
    CLEAR test_entry.
    test_entry-request = 'TEST_PROMPT'.
    test_entry-response = 'TEST_RESPONSE'.
    test_entry-timestamp = sy-datum.
    test_entry-uname = sy-uname.

    environment->clear_doubles( ).
  ENDMETHOD.

  METHOD teardown.
    FREE cut.
  ENDMETHOD.

  METHOD given_system_settings.
    DATA test_data TYPE STANDARD TABLE OF zllm_system.
    DATA temp3 TYPE zllm_system.
    CLEAR temp3.
    temp3-save_calls = active.
    temp3-call_filter_uname = filter_uname.
    APPEND temp3 TO test_data.

    environment->insert_test_data( test_data ).
  ENDMETHOD.

  METHOD when_adding_log_entry.
    CREATE OBJECT cut.
    cut->zif_llm_call_logger~add( test_entry ).
  ENDMETHOD.

  METHOD then_entry_is_logged.
    DATA actual_count TYPE i.
    SELECT COUNT( * ) FROM zllm_call_log INTO actual_count. "#EC CI_NOWHERE
    cl_abap_unit_assert=>assert_equals(
      exp = expected_count
      act = actual_count ).
  ENDMETHOD.

  METHOD add_when_active.
    " Test: Entry should be logged when system is active
    given_system_settings(
      active = abap_true
      filter_uname = '*' ).

    when_adding_log_entry( ).

    then_entry_is_logged( 1 ).
  ENDMETHOD.

  METHOD add_when_inactive.
    " Test: Entry should not be logged when system is inactive
    given_system_settings(
      active = abap_false
      filter_uname = '*' ).

    when_adding_log_entry( ).

    then_entry_is_logged( 0 ).
  ENDMETHOD.

  METHOD add_with_user_filter.
    " Test: Entry should be logged only for specific user
    given_system_settings(
      active = abap_true
      filter_uname = sy-uname ).

    when_adding_log_entry( ).

    then_entry_is_logged( 1 ).
  ENDMETHOD.

ENDCLASS.
