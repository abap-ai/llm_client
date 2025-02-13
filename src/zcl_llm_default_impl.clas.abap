CLASS zcl_llm_default_impl DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES zif_llm_default_impl .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA:
      enc_class  TYPE REF TO zif_llm_encryption,
      stat_class TYPE REF TO zif_llm_statistics,
      log_class  TYPE REF TO zif_llm_call_logger,
      auth_class TYPE REF TO zif_llm_auth.
ENDCLASS.



CLASS zcl_llm_default_impl IMPLEMENTATION.
  METHOD zif_llm_default_impl~get_encryption_impl.
    IF enc_class IS NOT BOUND.
      CREATE OBJECT enc_class TYPE zcl_llm_encryption.
    ENDIF.
    result = enc_class.
  ENDMETHOD.

  METHOD zif_llm_default_impl~get_json_impl.
    result = '/UI2/CL_JSON'.
  ENDMETHOD.

  METHOD zif_llm_default_impl~get_call_logger_impl.
    IF log_class IS NOT BOUND.
      CREATE OBJECT log_class TYPE zcl_llm_call_logger.
    ENDIF.
    result = log_class.
  ENDMETHOD.

  METHOD zif_llm_default_impl~get_statistics_impl.
    IF stat_class IS NOT BOUND.
      CREATE OBJECT stat_class TYPE zcl_llm_statistics.
    ENDIF.
    result = stat_class.
  ENDMETHOD.

  METHOD zif_llm_default_impl~get_authorization_impl.
    IF auth_class IS NOT BOUND.
      CREATE OBJECT auth_class TYPE zcl_llm_auth_disabled.
    ENDIF.
    result = auth_class.
  ENDMETHOD.

ENDCLASS.
