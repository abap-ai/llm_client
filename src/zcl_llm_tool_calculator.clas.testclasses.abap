CLASS ltcl_calculator_tool DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA calculator TYPE REF TO zcl_llm_tool_calculator.

    METHODS setup.
    METHODS addition                     FOR TESTING.
    METHODS subtraction                  FOR TESTING.
    METHODS multiplication               FOR TESTING.
    METHODS division                     FOR TESTING.
    METHODS power                        FOR TESTING.
    METHODS modulo                       FOR TESTING.
    METHODS complex_expression           FOR TESTING.
    METHODS spaces_in_expression         FOR TESTING.
    METHODS division_by_zero             FOR TESTING.
    METHODS invalid_expression           FOR TESTING.
    METHODS check_tool_details           FOR TESTING.
    METHODS complex_expressions_brackets FOR TESTING.
    METHODS power_operations             FOR TESTING.
    METHODS power_operation_errors       FOR TESTING.
ENDCLASS.

CLASS ltcl_calculator_tool IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT calculator.
  ENDMETHOD.

  METHOD addition.
    DATA temp36 TYPE zcl_llm_tool_calculator=>calculation_input.
    DATA input LIKE temp36.
    DATA data LIKE REF TO input.
    DATA result TYPE zif_llm_tool=>tool_result.
    DATA output TYPE zcl_llm_tool_calculator=>calculation_output.
    FIELD-SYMBOLS <output> TYPE data.
    CLEAR temp36.
    temp36-expression = '2 + 2'.
    
    input = temp36.

    
    GET REFERENCE OF input INTO data.
    
    result = calculator->zif_llm_tool~execute( data         = data
                                                     tool_call_id = 'test_id' ).

    
    
    ASSIGN result-data->* TO <output>.
    output = <output>.

    cl_abap_unit_assert=>assert_equals( exp = '4'
                                        act = output-result ).
  ENDMETHOD.

  METHOD subtraction.
    DATA temp37 TYPE zcl_llm_tool_calculator=>calculation_input.
    DATA input LIKE temp37.
    DATA data LIKE REF TO input.
    DATA result TYPE zif_llm_tool=>tool_result.
    DATA output TYPE zcl_llm_tool_calculator=>calculation_output.
    FIELD-SYMBOLS <output> TYPE data.
    CLEAR temp37.
    temp37-expression = '5 - 3'.
    
    input = temp37.

    
    GET REFERENCE OF input INTO data.
    
    result = calculator->zif_llm_tool~execute( data         = data
                                                     tool_call_id = 'test_id' ).

    
    
    ASSIGN result-data->* TO <output>.
    output = <output>.

    cl_abap_unit_assert=>assert_equals( exp = '2'
                                        act = output-result ).
  ENDMETHOD.

  METHOD multiplication.
    DATA temp38 TYPE zcl_llm_tool_calculator=>calculation_input.
    DATA input LIKE temp38.
    DATA data LIKE REF TO input.
    DATA result TYPE zif_llm_tool=>tool_result.
    DATA output TYPE zcl_llm_tool_calculator=>calculation_output.
    FIELD-SYMBOLS <output> TYPE data.
    CLEAR temp38.
    temp38-expression = '4 * 3'.
    
    input = temp38.

    
    GET REFERENCE OF input INTO data.
    
    result = calculator->zif_llm_tool~execute( data         = data
                                                     tool_call_id = 'test_id' ).

    
    
    ASSIGN result-data->* TO <output>.
    output = <output>.

    cl_abap_unit_assert=>assert_equals( exp = '12'
                                        act = output-result ).
  ENDMETHOD.

  METHOD division.
    DATA temp39 TYPE zcl_llm_tool_calculator=>calculation_input.
    DATA input LIKE temp39.
    DATA data LIKE REF TO input.
    DATA result TYPE zif_llm_tool=>tool_result.
    DATA output TYPE zcl_llm_tool_calculator=>calculation_output.
    FIELD-SYMBOLS <output> TYPE data.
    CLEAR temp39.
    temp39-expression = '10 / 2'.
    
    input = temp39.

    
    GET REFERENCE OF input INTO data.
    
    result = calculator->zif_llm_tool~execute( data         = data
                                                     tool_call_id = 'test_id' ).

    
    
    ASSIGN result-data->* TO <output>.
    output = <output>.

    cl_abap_unit_assert=>assert_equals( exp = '5'
                                        act = output-result ).
  ENDMETHOD.

  METHOD power.
    DATA temp40 TYPE zcl_llm_tool_calculator=>calculation_input.
    DATA input LIKE temp40.
    DATA data LIKE REF TO input.
    DATA result TYPE zif_llm_tool=>tool_result.
    DATA output TYPE zcl_llm_tool_calculator=>calculation_output.
    FIELD-SYMBOLS <output> TYPE data.
    CLEAR temp40.
    temp40-expression = '2 ** 3'.
    
    input = temp40.

    
    GET REFERENCE OF input INTO data.
    
    result = calculator->zif_llm_tool~execute( data         = data
                                                     tool_call_id = 'test_id' ).

    
    
    ASSIGN result-data->* TO <output>.
    output = <output>.

    cl_abap_unit_assert=>assert_equals( exp = '8'
                                        act = output-result ).
  ENDMETHOD.

  METHOD modulo.
    DATA temp41 TYPE zcl_llm_tool_calculator=>calculation_input.
    DATA input LIKE temp41.
    DATA data LIKE REF TO input.
    DATA result TYPE zif_llm_tool=>tool_result.
    DATA output TYPE zcl_llm_tool_calculator=>calculation_output.
    FIELD-SYMBOLS <output> TYPE data.
    CLEAR temp41.
    temp41-expression = '10 MOD 3'.
    
    input = temp41.

    
    GET REFERENCE OF input INTO data.
    
    result = calculator->zif_llm_tool~execute( data         = data
                                                     tool_call_id = 'test_id' ).

    
    
    ASSIGN result-data->* TO <output>.
    output = <output>.

    cl_abap_unit_assert=>assert_equals( exp = '1'
                                        act = output-result ).
  ENDMETHOD.

  METHOD complex_expression.
    DATA temp42 TYPE zcl_llm_tool_calculator=>calculation_input.
    DATA input LIKE temp42.
    DATA data LIKE REF TO input.
    DATA result TYPE zif_llm_tool=>tool_result.
    DATA output TYPE zcl_llm_tool_calculator=>calculation_output.
    FIELD-SYMBOLS <output> TYPE data.
    CLEAR temp42.
    temp42-expression = '(5 + 3) * 2 - 4'.
    
    input = temp42.

    
    GET REFERENCE OF input INTO data.
    
    result = calculator->zif_llm_tool~execute( data         = data
                                                     tool_call_id = 'test_id' ).

    
    
    ASSIGN result-data->* TO <output>.
    output = <output>.

    cl_abap_unit_assert=>assert_equals( exp = '12'
                                        act = output-result ).
  ENDMETHOD.

  METHOD spaces_in_expression.
    DATA temp43 TYPE zcl_llm_tool_calculator=>calculation_input.
    DATA input LIKE temp43.
    DATA data LIKE REF TO input.
    DATA result TYPE zif_llm_tool=>tool_result.
    DATA output TYPE zcl_llm_tool_calculator=>calculation_output.
    FIELD-SYMBOLS <output> TYPE data.
    CLEAR temp43.
    temp43-expression = '  2   +    2  '.
    
    input = temp43.

    
    GET REFERENCE OF input INTO data.
    
    result = calculator->zif_llm_tool~execute( data         = data
                                                     tool_call_id = 'test_id' ).

    
    
    ASSIGN result-data->* TO <output>.
    output = <output>.

    cl_abap_unit_assert=>assert_equals( exp = '4'
                                        act = output-result ).
  ENDMETHOD.

  METHOD division_by_zero.
    DATA temp44 TYPE zcl_llm_tool_calculator=>calculation_input.
    DATA input LIKE temp44.
    DATA data LIKE REF TO input.
    DATA result TYPE zif_llm_tool=>tool_result.
    DATA output TYPE zcl_llm_tool_calculator=>calculation_output.
    FIELD-SYMBOLS <output> TYPE data.
    CLEAR temp44.
    temp44-expression = '1 / 0'.
    
    input = temp44.

    
    GET REFERENCE OF input INTO data.
    
    result = calculator->zif_llm_tool~execute( data         = data
                                                     tool_call_id = 'test_id' ).

    
    
    ASSIGN result-data->* TO <output>.
    output = <output>.

    cl_abap_unit_assert=>assert_char_cp( exp = 'Error:*'
                                         act = output-result ).
  ENDMETHOD.

  METHOD invalid_expression.
    DATA temp45 TYPE zcl_llm_tool_calculator=>calculation_input.
    DATA input LIKE temp45.
    DATA data LIKE REF TO input.
    DATA result TYPE zif_llm_tool=>tool_result.
    DATA output TYPE zcl_llm_tool_calculator=>calculation_output.
    FIELD-SYMBOLS <output> TYPE data.
    CLEAR temp45.
    temp45-expression = 'abc + 2'.
    
    input = temp45.

    
    GET REFERENCE OF input INTO data.
    
    result = calculator->zif_llm_tool~execute( data         = data
                                                    tool_call_id = 'test_id' ).

    
    
    ASSIGN result-data->* TO <output>.
    output = <output>.

    " The error message from cx_sy_conversion_no_number
    cl_abap_unit_assert=>assert_char_cp(
      exp = 'Error: Invalid expression: abc + 2'
      act = output-result ).
  ENDMETHOD.

  METHOD check_tool_details.
    DATA tool_details TYPE zif_llm_tool=>tool_details.
    tool_details = calculator->zif_llm_tool~get_tool_details( ).

    cl_abap_unit_assert=>assert_equals( exp = 'calculator'
                                        act = tool_details-name ).

    cl_abap_unit_assert=>assert_equals( exp = zif_llm_tool=>type_function
                                        act = tool_details-type ).

    cl_abap_unit_assert=>assert_not_initial( tool_details-description ).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( tool_details-parameters-descriptions ) ).
  ENDMETHOD.

  METHOD complex_expressions_brackets.
    " Structure for test cases
    TYPES: BEGIN OF test_case,
             expression TYPE string,
             expected   TYPE string,
           END OF test_case,
           test_cases TYPE STANDARD TABLE OF test_case WITH DEFAULT KEY.

    DATA temp46 TYPE test_cases.
    DATA temp47 LIKE LINE OF temp46.
    DATA test_cases LIKE temp46.
    DATA test_case LIKE LINE OF test_cases.
      DATA temp48 TYPE zcl_llm_tool_calculator=>calculation_input.
      DATA input LIKE temp48.
      DATA data LIKE REF TO input.
      DATA result TYPE zif_llm_tool=>tool_result.
      DATA output TYPE zcl_llm_tool_calculator=>calculation_output.
      FIELD-SYMBOLS <output> TYPE data.
    CLEAR temp46.
    
    temp47-expression = '(5 + 3) * 2'.
    temp47-expected = '16'.
    INSERT temp47 INTO TABLE temp46.
    temp47-expression = '2 + 3 * 4'.
    temp47-expected = '14'.
    INSERT temp47 INTO TABLE temp46.
    temp47-expression = '(2 + 3) * 4'.
    temp47-expected = '20'.
    INSERT temp47 INTO TABLE temp46.
    temp47-expression = '2 ** (2 + 1)'.
    temp47-expected = '8'.
    INSERT temp47 INTO TABLE temp46.
    temp47-expression = '((4 + 2) * 3)'.
    temp47-expected = '18'.
    INSERT temp47 INTO TABLE temp46.
    temp47-expression = '10 - (2 + 3)'.
    temp47-expected = '5'.
    INSERT temp47 INTO TABLE temp46.
    
    test_cases = temp46.

    
    LOOP AT test_cases INTO test_case.
      
      CLEAR temp48.
      temp48-expression = test_case-expression.
      
      input = temp48.
      
      GET REFERENCE OF input INTO data.
      
      result = calculator->zif_llm_tool~execute( data         = data
                                                       tool_call_id = 'test_id' ).
      
      
      ASSIGN result-data->* TO <output>.
      output = <output>.
      cl_abap_unit_assert=>assert_equals( exp = test_case-expected
                                          act = output-result
                                          msg = |Expression: { test_case-expression }| ).
    ENDLOOP.
  ENDMETHOD.

  METHOD power_operations.
    TYPES: BEGIN OF test_case,
             expression TYPE string,
             expected   TYPE string,
           END OF test_case,
           test_cases TYPE STANDARD TABLE OF test_case WITH DEFAULT KEY.

    DATA temp49 TYPE test_cases.
    DATA temp50 LIKE LINE OF temp49.
    DATA test_cases LIKE temp49.
    DATA test_case LIKE LINE OF test_cases.
      DATA temp51 TYPE zcl_llm_tool_calculator=>calculation_input.
      DATA input LIKE temp51.
      DATA data LIKE REF TO input.
      DATA result TYPE zif_llm_tool=>tool_result.
      DATA output TYPE zcl_llm_tool_calculator=>calculation_output.
      FIELD-SYMBOLS <output> TYPE data.
    CLEAR temp49.
    
    temp50-expression = '4 ** 0.5'.
    temp50-expected = '2'.
    INSERT temp50 INTO TABLE temp49.
    temp50-expression = '2 ** 0'.
    temp50-expected = '1'.
    INSERT temp50 INTO TABLE temp49.
    temp50-expression = '2.5 ** 2'.
    temp50-expected = '6,25'.
    INSERT temp50 INTO TABLE temp49.
    temp50-expression = '3 ** 2 + 4'.
    temp50-expected = '13'.
    INSERT temp50 INTO TABLE temp49.
    temp50-expression = '2 ** (3 ** 2)'.
    temp50-expected = '512'.
    INSERT temp50 INTO TABLE temp49.
    temp50-expression = '(2 ** 3) ** 2'.
    temp50-expected = '64'.
    INSERT temp50 INTO TABLE temp49.
    temp50-expression = '2 ** -2'.
    temp50-expected = '0,25'.
    INSERT temp50 INTO TABLE temp49.
    temp50-expression = '2 ** -3'.
    temp50-expected = '0,125'.
    INSERT temp50 INTO TABLE temp49.
    temp50-expression = '4 ** -0.5'.
    temp50-expected = '0,5'.
    INSERT temp50 INTO TABLE temp49.
    
    test_cases = temp49.    " Negative fractional exponent

    
    LOOP AT test_cases INTO test_case.
      
      CLEAR temp51.
      temp51-expression = test_case-expression.
      
      input = temp51.

      
      GET REFERENCE OF input INTO data.
      
      result = calculator->zif_llm_tool~execute( data         = data
                                                       tool_call_id = 'test_id' ).

      
      
      ASSIGN result-data->* TO <output>.
      output = <output>.

      cl_abap_unit_assert=>assert_equals( exp = test_case-expected
                                          act = output-result
                                          msg = |Expression: { test_case-expression }| ).
    ENDLOOP.
  ENDMETHOD.

  METHOD power_operation_errors.
    TYPES: BEGIN OF test_case,
             expression TYPE string,
             exp_error  TYPE abap_bool,
           END OF test_case,
           test_cases TYPE STANDARD TABLE OF test_case WITH DEFAULT KEY.

    DATA temp52 TYPE test_cases.
    DATA temp53 LIKE LINE OF temp52.
    DATA test_cases LIKE temp52.
    DATA test_case LIKE LINE OF test_cases.
      DATA temp54 TYPE zcl_llm_tool_calculator=>calculation_input.
      DATA input LIKE temp54.
      DATA data LIKE REF TO input.
      DATA result TYPE zif_llm_tool=>tool_result.
      DATA output TYPE zcl_llm_tool_calculator=>calculation_output.
      FIELD-SYMBOLS <output> TYPE data.
    CLEAR temp52.
    
    temp53-expression = '2 ** a'.
    temp53-exp_error = abap_true.
    INSERT temp53 INTO TABLE temp52.
    temp53-expression = '2 ** '.
    temp53-exp_error = abap_true.
    INSERT temp53 INTO TABLE temp52.
    temp53-expression = '** 2'.
    temp53-exp_error = abap_true.
    INSERT temp53 INTO TABLE temp52.
    temp53-expression = '0 ** -1'.
    temp53-exp_error = abap_true.
    INSERT temp53 INTO TABLE temp52.
    
    test_cases = temp52.

    
    LOOP AT test_cases INTO test_case.
      
      CLEAR temp54.
      temp54-expression = test_case-expression.
      
      input = temp54.

      
      GET REFERENCE OF input INTO data.
      
      result = calculator->zif_llm_tool~execute(
        data         = data
        tool_call_id = 'test_id'
      ).

      
      
      ASSIGN result-data->* TO <output>.
      output = <output>.

      IF test_case-exp_error = abap_true.
        cl_abap_unit_assert=>assert_char_cp(
          exp = 'Error:*'
          act = output-result
          msg = |Expression: { test_case-expression }|
        ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
