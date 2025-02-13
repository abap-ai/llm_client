"! <p class="shorttext synchronized" lang="en">LLM Statistics</p>
CLASS zcl_llm_statistics DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_llm_statistics.

    ALIASES add FOR zif_llm_statistics~add.

    METHODS constructor.

  PROTECTED SECTION.
    DATA active TYPE sap_bool.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_llm_statistics IMPLEMENTATION.
  METHOD zif_llm_statistics~add.
    IF active = abap_false.
      RETURN.
    ENDIF.
    INSERT zllm_statistics FROM record ##SUBRC_OK.
  ENDMETHOD.

  METHOD constructor.
    SELECT SINGLE stat_active INTO active FROM zllm_system ##SUBRC_OK.
  ENDMETHOD.

ENDCLASS.
