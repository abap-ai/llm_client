CLASS zcl_llm_call_logger DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_llm_call_logger.

    METHODS constructor.

  PROTECTED SECTION.
    DATA active TYPE sap_bool.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_llm_call_logger IMPLEMENTATION.
  METHOD zif_llm_call_logger~add.
    IF active = abap_false.
      RETURN.
    ENDIF.
    INSERT zllm_call_log FROM entry ##SUBRC_OK.
  ENDMETHOD.

  METHOD constructor.
    DATA system TYPE zllm_system.
    SELECT SINGLE * INTO system FROM zllm_system.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    IF     system-save_calls = abap_true
       AND ( system-call_filter_uname = '*' OR system-call_filter_uname = sy-uname ).
      active = abap_true.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
