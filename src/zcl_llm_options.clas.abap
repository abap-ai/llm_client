CLASS zcl_llm_options DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_llm_options.

    ALIASES get_paramters         FOR zif_llm_options~get_paramters.
    ALIASES set_custom_parameters FOR zif_llm_options~set_custom_parameters.
    ALIASES set_frequency_penalty FOR zif_llm_options~set_frequency_penalty.
    ALIASES set_min_p             FOR zif_llm_options~set_min_p.
    ALIASES set_presence_penalty  FOR zif_llm_options~set_presence_penalty.
    ALIASES set_seed              FOR zif_llm_options~set_seed.
    ALIASES set_temperature       FOR zif_llm_options~set_temperature.
    ALIASES set_top_a             FOR zif_llm_options~set_top_a.
    ALIASES set_top_k             FOR zif_llm_options~set_top_k.
    ALIASES set_top_p             FOR zif_llm_options~set_top_p.
    ALIASES set_max_tokens        FOR zif_llm_options~set_max_tokens.

  PROTECTED SECTION.
    DATA int_parameters TYPE zllm_keyvalues.

    METHODS validate_range_float
      IMPORTING !value TYPE decfloat16
                !min   TYPE decfloat16
                !max   TYPE decfloat16
      RAISING   zcx_llm_validation.

    METHODS validate_range_int
      IMPORTING !value TYPE i
                !min   TYPE i
                !max   TYPE i OPTIONAL
      RAISING   zcx_llm_validation.

    METHODS set_parameter
      IMPORTING !key   TYPE string
                !value TYPE string.

  PRIVATE SECTION.

ENDCLASS.

CLASS zcl_llm_options IMPLEMENTATION.
  METHOD set_parameter.
    DATA temp1 TYPE zllm_keyvalue.
    " If parameter already exists, replace it
    DELETE int_parameters WHERE key = key.

    " Add new parameter
    
    CLEAR temp1.
    temp1-key = key.
    temp1-value = value.
    INSERT temp1
           INTO TABLE int_parameters.
  ENDMETHOD.

  METHOD validate_range_float.
      DATA temp2 TYPE REF TO zcx_llm_validation.
    IF value < min OR value > max.
      
      CREATE OBJECT temp2 TYPE zcx_llm_validation EXPORTING textid = zcx_llm_validation=>value_out_of_range attr1 = |Value { value } is out of range [{ min }, { max }]|.
      RAISE EXCEPTION temp2 ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD validate_range_int.
      DATA temp3 TYPE REF TO zcx_llm_validation.
      DATA temp4 TYPE REF TO zcx_llm_validation.
    IF value < min.
      
      CREATE OBJECT temp3 TYPE zcx_llm_validation EXPORTING textid = zcx_llm_validation=>value_out_of_range attr1 = |Value { value } is below minimum { min }|.
      RAISE EXCEPTION temp3 ##NO_TEXT.
    ENDIF.

    IF max IS NOT INITIAL AND value > max.
      
      CREATE OBJECT temp4 TYPE zcx_llm_validation EXPORTING textid = zcx_llm_validation=>value_out_of_range attr1 = |Value { value } is above maximum { max }|.
      RAISE EXCEPTION temp4 ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD get_paramters.
    parameters = int_parameters.
  ENDMETHOD.

  METHOD zif_llm_options~set_custom_parameters.
    DATA par LIKE LINE OF parameters.
      FIELD-SYMBOLS <par> TYPE zllm_keyvalue.
    LOOP AT parameters INTO par.
      
      READ TABLE int_parameters WITH KEY key = par-key ASSIGNING <par>.
      IF sy-subrc = 0.
        <par>-value = par-value.
      ELSE.
        INSERT par INTO TABLE int_parameters.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_llm_options~set_frequency_penalty.
    validate_range_float( value = frequency_penalty
                          min   = -2
                          max   = 2 ).
    set_parameter( key   = 'frequency_penalty'
                   value = |{ frequency_penalty }| ).
  ENDMETHOD.

  METHOD zif_llm_options~set_min_p.
    validate_range_float( value = min_p
                          min   = 0
                          max   = 1 ).
    set_parameter( key   = 'min_p'
                   value = |{ min_p }| ).
  ENDMETHOD.

  METHOD zif_llm_options~set_presence_penalty.
    validate_range_float( value = presence_penalty
                          min   = -2
                          max   = 2 ).
    set_parameter( key   = 'presence_penalty'
                   value = |{ presence_penalty }| ).
  ENDMETHOD.

  METHOD zif_llm_options~set_seed.
    validate_range_int( value = seed
                        min   = 0 ).
    set_parameter( key   = 'seed'
                   value = |{ seed }| ).
  ENDMETHOD.

  METHOD zif_llm_options~set_temperature.
    validate_range_float( value = temperature
                          min   = 0
                          max   = 2 ).
    set_parameter( key   = 'temperature'
                   value = |{ temperature }| ).
  ENDMETHOD.

  METHOD zif_llm_options~set_top_a.
    validate_range_float( value = top_a
                          min   = 0
                          max   = 1 ).
    set_parameter( key   = 'top_a'
                   value = |{ top_a }| ).
  ENDMETHOD.

  METHOD zif_llm_options~set_top_k.
    validate_range_int( value = top_k
                        min   = 1 ).
    set_parameter( key   = 'top_k'
                   value = |{ top_k }| ).
  ENDMETHOD.

  METHOD zif_llm_options~set_top_p.
    validate_range_float( value = top_p
                          min   = 0
                          max   = 1 ).
    set_parameter( key   = 'top_p'
                   value = |{ top_p }| ).
  ENDMETHOD.

  METHOD zif_llm_options~set_max_tokens.
    validate_range_int( value = tokens
                        min   = 0 ).
    set_parameter( key   = 'max_tokens'
                   value = |{ tokens }| ).
  ENDMETHOD.

ENDCLASS.
