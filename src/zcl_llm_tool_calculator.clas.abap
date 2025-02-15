CLASS zcl_llm_tool_calculator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_llm_tool.

    TYPES: BEGIN OF calculation_input,
             expression TYPE string,
           END OF calculation_input.

    TYPES: BEGIN OF calculation_output,
             result TYPE string,
           END OF calculation_output.

  PRIVATE SECTION.
    TYPES: BEGIN OF token,
             value     TYPE string,
             is_number TYPE abap_bool,
           END OF token.
    TYPES tokens TYPE STANDARD TABLE OF token WITH DEFAULT KEY.

    METHODS evaluate_expression
      IMPORTING expression    TYPE string
      RETURNING VALUE(result) TYPE string
      RAISING   cx_sy_zerodivide
                cx_sy_arithmetic_error
                cx_sy_conversion_no_number.

    METHODS tokenize
      IMPORTING expression    TYPE string
      RETURNING VALUE(result) TYPE tokens
      RAISING   cx_sy_conversion_no_number.

    METHODS parse_number
      IMPORTING number_string TYPE string
      RETURNING VALUE(result) TYPE decfloat34
      RAISING   cx_sy_conversion_no_number.

    METHODS get_operator_precedence
      IMPORTING operator      TYPE string
      RETURNING VALUE(result) TYPE i.

    METHODS evaluate_tokens
      IMPORTING !tokens       TYPE tokens
      RETURNING VALUE(result) TYPE decfloat34
      RAISING   cx_sy_zerodivide
                cx_sy_arithmetic_error
                cx_sy_conversion_no_number.

    METHODS apply_operator
      IMPORTING operator      TYPE string
                operand1      TYPE decfloat34
                operand2      TYPE decfloat34
      RETURNING VALUE(result) TYPE decfloat34
      RAISING   cx_sy_zerodivide
                cx_sy_arithmetic_error
                cx_sy_conversion_no_number.

    METHODS process_operator
      IMPORTING operator       TYPE string
      CHANGING  operator_stack TYPE string_table
                output_queue   TYPE tokens.

    METHODS evaluate_rpn
      IMPORTING !tokens       TYPE tokens
      RETURNING VALUE(result) TYPE decfloat34
      RAISING   cx_sy_zerodivide cx_sy_arithmetic_error
                cx_sy_conversion_no_number.

    METHODS pop_from_stack
      EXPORTING result TYPE string
      CHANGING  !stack TYPE string_table.

    METHODS peek_stack
      EXPORTING result TYPE string
      CHANGING  !stack TYPE string_table.

    DATA output       TYPE calculation_output.
    DATA tool_call_id TYPE string.

ENDCLASS.


CLASS zcl_llm_tool_calculator IMPLEMENTATION.
  METHOD zif_llm_tool~get_tool_details.
    DATA parameters TYPE zif_llm_tool=>tool_parameters.
    DATA temp1 TYPE zif_llm_tool_parser=>def_descriptions.
    DATA temp2 LIKE LINE OF temp1.

    parameters-data_desc    ?= cl_abap_typedescr=>describe_by_name( 'CALCULATION_INPUT' ).

    
    CLEAR temp1.
    
    temp2-fieldname = 'EXPRESSION'.
    temp2-description = 'Mathematical expression to evaluate. Supports +, -, *, /, **, MOD and parentheses'.
    INSERT temp2 INTO TABLE temp1.
    parameters-descriptions  = temp1 ##NO_TEXT.

    CLEAR result.
    result-name = 'calculator'.
    result-description = 'Evaluates mathematical expressions. Supports +, -, *, /, **, MOD and parentheses'.
    result-type = zif_llm_tool=>type_function.
    result-parameters = parameters.
  ENDMETHOD.

  METHOD zif_llm_tool~execute.
    DATA input TYPE calculation_input.

    FIELD-SYMBOLS <data> TYPE data.
    ASSIGN data->* TO <data>.
    input = <data>.

    TRY.
        output-result = evaluate_expression( input-expression ).
      CATCH cx_sy_zerodivide.
        output-result = |Error: Division by zero| ##NO_TEXT.
      CATCH cx_sy_conversion_no_number cx_sy_arithmetic_error.
        output-result = |Error: Invalid expression: { input-expression }| ##NO_TEXT.
    ENDTRY.

    me->tool_call_id = tool_call_id.
    GET REFERENCE OF output INTO result-data.
    result-name         = `calculator`.
    result-tool_call_id = tool_call_id.
  ENDMETHOD.

  METHOD zif_llm_tool~get_result.
    CLEAR result.
    GET REFERENCE OF output INTO result-data.
    result-tool_call_id = tool_call_id.
    result-name = `calculator`.
  ENDMETHOD.

  METHOD evaluate_expression.
      DATA temp3 TYPE REF TO cx_sy_conversion_no_number.
    DATA cleaned_expr TYPE string.
    DATA tokens TYPE zcl_llm_tool_calculator=>tokens.
    DATA calc_result TYPE decfloat34.
    IF expression IS INITIAL.
      
      CREATE OBJECT temp3 TYPE cx_sy_conversion_no_number.
      RAISE EXCEPTION temp3.
    ENDIF.

    
    cleaned_expr = replace( val   = expression
                                  regex = `\s`
                                  with  = ``
                                  occ   = 0 ).
    
    tokens = tokenize( cleaned_expr ).
    
    calc_result = evaluate_tokens( tokens ).
    result = |{ calc_result NUMBER = USER }|.
  ENDMETHOD.

  METHOD tokenize.
    DATA current_token TYPE string.
    DATA current_char  TYPE c LENGTH 1.
    DATA length        TYPE i.
    DATA index         TYPE i VALUE 0.
    DATA next_pos      TYPE i.
          DATA temp4 TYPE zcl_llm_tool_calculator=>token.
            DATA temp5 TYPE zcl_llm_tool_calculator=>token.
              DATA temp6 TYPE zcl_llm_tool_calculator=>token.
              DATA temp7 TYPE zcl_llm_tool_calculator=>token.
            DATA temp8 TYPE zcl_llm_tool_calculator=>token.
          DATA temp9 LIKE LINE OF result.
          DATA temp10 LIKE sy-tabix.
          DATA temp1 LIKE LINE OF result.
          DATA temp2 LIKE sy-tabix.
          DATA temp16 LIKE LINE OF result.
          DATA temp17 LIKE sy-tabix.
          DATA temp18 LIKE LINE OF result.
          DATA temp19 LIKE sy-tabix.
          DATA temp20 LIKE LINE OF result.
          DATA temp21 LIKE sy-tabix.
          DATA temp22 LIKE LINE OF result.
          DATA temp23 LIKE sy-tabix.
          DATA temp24 LIKE LINE OF result.
          DATA temp25 LIKE sy-tabix.
              DATA temp11 TYPE zcl_llm_tool_calculator=>token.
              DATA temp12 TYPE zcl_llm_tool_calculator=>token.
            DATA temp13 TYPE zcl_llm_tool_calculator=>token.
          DATA temp14 TYPE undefined.
          DATA temp3 TYPE REF TO cx_sy_conversion_no_number.
      DATA temp15 TYPE REF TO cx_sy_conversion_no_number.

    length = strlen( expression ) - 1.

    WHILE index <= length.
      current_char = expression+index(1).

      CASE current_char.
        WHEN '0' OR '1' OR '2' OR '3' OR '4' OR '5' OR '6' OR '7' OR '8' OR '9' OR '.'.
          CLEAR current_token.
          WHILE index <= length AND expression+index(1) CA '0123456789.'.
            current_token = current_token && expression+index(1).
            index = index + 1.
          ENDWHILE.
          index = index - 1.
          
          CLEAR temp4.
          temp4-value = current_token.
          temp4-is_number = abap_true.
          APPEND temp4 TO result.

        WHEN '(' OR ')' OR '+' OR '*' OR '/' OR 'M'.
          " Check for MOD operator
          IF     current_char         = 'M'
             AND index + 2           <= length
             AND expression+index(3)  = 'MOD'.
            
            CLEAR temp5.
            temp5-value = 'MOD'.
            temp5-is_number = abap_false.
            APPEND temp5 TO result.
            index = index + 2.
            " Check for ** operator
          ELSEIF     current_char = '*'
                 AND index        < length.
            next_pos = index + 1.
            IF expression+next_pos(1) = '*'.
              
              CLEAR temp6.
              temp6-value = '**'.
              temp6-is_number = abap_false.
              APPEND temp6 TO result.
              index = index + 1.
            ELSE.
              
              CLEAR temp7.
              temp7-value = current_char.
              temp7-is_number = abap_false.
              APPEND temp7 TO result.
            ENDIF.
          ELSE.
            
            CLEAR temp8.
            temp8-value = current_char.
            temp8-is_number = abap_false.
            APPEND temp8 TO result.
          ENDIF.

        WHEN '-'.
          " Check if this is a negative number (when it's the first token or follows an operator or opening parenthesis)
          
          
          temp10 = sy-tabix.
          READ TABLE result INDEX lines( result ) INTO temp9.
          sy-tabix = temp10.
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
          ENDIF.
          
          
          temp2 = sy-tabix.
          READ TABLE result INDEX lines( result ) INTO temp1.
          sy-tabix = temp2.
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
          ENDIF.
          
          
          temp17 = sy-tabix.
          READ TABLE result INDEX lines( result ) INTO temp16.
          sy-tabix = temp17.
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
          ENDIF.
          
          
          temp19 = sy-tabix.
          READ TABLE result INDEX lines( result ) INTO temp18.
          sy-tabix = temp19.
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
          ENDIF.
          
          
          temp21 = sy-tabix.
          READ TABLE result INDEX lines( result ) INTO temp20.
          sy-tabix = temp21.
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
          ENDIF.
          
          
          temp23 = sy-tabix.
          READ TABLE result INDEX lines( result ) INTO temp22.
          sy-tabix = temp23.
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
          ENDIF.
          
          
          temp25 = sy-tabix.
          READ TABLE result INDEX lines( result ) INTO temp24.
          sy-tabix = temp25.
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
          ENDIF.
          IF    result IS INITIAL
             OR (     lines( result ) > 0
                  AND (    temp9-value = '('
                        OR temp1-value = '+'
                        OR temp16-value = '-'
                        OR temp18-value = '*'
                        OR temp20-value = '/'
                        OR temp22-value = '**'
                        OR temp24-value = 'MOD' ) ).
            " This is a negative number - read the number part
            next_pos = index + 1.
            IF next_pos <= length AND expression+next_pos(1) CA '0123456789.'.
              current_token = '-'.
              index = index + 1.
              WHILE index <= length AND expression+index(1) CA '0123456789.'.
                current_token = current_token && expression+index(1).
                index = index + 1.
              ENDWHILE.
              index = index - 1.
              
              CLEAR temp11.
              temp11-value = current_token.
              temp11-is_number = abap_true.
              APPEND temp11 TO result.
            ELSE.
              " Just a minus operator
              
              CLEAR temp12.
              temp12-value = current_char.
              temp12-is_number = abap_false.
              APPEND temp12 TO result.
            ENDIF.
          ELSE.
            " Just a minus operator
            
            CLEAR temp13.
            temp13-value = current_char.
            temp13-is_number = abap_false.
            APPEND temp13 TO result.
          ENDIF.

        WHEN space.
          " Ignore spaces
          CONTINUE.

        WHEN OTHERS.
          " Raise exception for invalid characters
          
          temp14 = current_char.
          
          CREATE OBJECT temp3 TYPE cx_sy_conversion_no_number EXPORTING value = temp14.
          RAISE EXCEPTION temp3.
      ENDCASE.
      index = index + 1.
    ENDWHILE.

    " If no tokens were created, raise an exception
    IF result IS INITIAL.
      
      CREATE OBJECT temp15 TYPE cx_sy_conversion_no_number.
      RAISE EXCEPTION temp15.
    ENDIF.
  ENDMETHOD.

  METHOD get_operator_precedence.
    CASE operator.
      WHEN '(' OR ')'.
        result = 0.
      WHEN '+' OR '-'.
        result = 1.
      WHEN '*' OR '/' OR 'MOD'.
        result = 2.
      WHEN '**'.
        result = 3.
    ENDCASE.
  ENDMETHOD.

  METHOD evaluate_tokens.
    DATA output_queue   TYPE tokens.
    DATA operator_stack TYPE string_table.
    DATA peeked_operator TYPE string.  " Store result of peek_stack
    DATA popped_operator TYPE string. "For pop_from_stack

    " Shunting yard algorithm
    DATA token LIKE LINE OF tokens.
          DATA temp16 TYPE zcl_llm_tool_calculator=>token.
      DATA temp17 TYPE zcl_llm_tool_calculator=>token.
    LOOP AT tokens INTO token.
      IF token-is_number = abap_true.
        APPEND token TO output_queue.
      ELSEIF token-value = '('.
        APPEND token-value TO operator_stack.
      ELSEIF token-value = ')'.
        peek_stack( IMPORTING result = peeked_operator CHANGING stack = operator_stack  ). "Get Top of stack
        WHILE operator_stack IS NOT INITIAL AND peeked_operator <> '('.
          pop_from_stack( IMPORTING result = popped_operator CHANGING stack = operator_stack ).
          
          CLEAR temp16.
          temp16-value = popped_operator.
          temp16-is_number = abap_false.
          APPEND temp16
                 TO output_queue.
          peek_stack( IMPORTING result = peeked_operator CHANGING stack = operator_stack ). "Check Top of stack again
        ENDWHILE.
        IF operator_stack IS NOT INITIAL.
          pop_from_stack( IMPORTING result = popped_operator CHANGING stack = operator_stack ). " Remove '('
        ENDIF.
      ELSE.
        process_operator( EXPORTING operator       = token-value
                          CHANGING  operator_stack = operator_stack
                                    output_queue   = output_queue ).
      ENDIF.
    ENDLOOP.

    " Move remaining operators to output
    WHILE operator_stack IS NOT INITIAL.
      pop_from_stack( IMPORTING result = popped_operator CHANGING stack = operator_stack ).
      
      CLEAR temp17.
      temp17-value = popped_operator.
      temp17-is_number = abap_false.
      APPEND temp17
             TO output_queue.
    ENDWHILE.

    " Evaluate RPN
    result = evaluate_rpn( output_queue ).
  ENDMETHOD.

  METHOD process_operator.
    DATA peeked_operator TYPE string.
    DATA popped_operator TYPE string.
      DATA temp18 TYPE zcl_llm_tool_calculator=>token.

    peek_stack( IMPORTING result = peeked_operator CHANGING  stack = operator_stack ).
    WHILE     operator_stack IS NOT INITIAL
          AND peeked_operator <> '('
          AND get_operator_precedence( peeked_operator ) >=
              get_operator_precedence( operator ).
      pop_from_stack( IMPORTING result = popped_operator CHANGING stack = operator_stack ).
      
      CLEAR temp18.
      temp18-value = popped_operator.
      temp18-is_number = abap_false.
      APPEND temp18
             TO output_queue.
      peek_stack( IMPORTING result = peeked_operator CHANGING  stack = operator_stack ). "Check for next iteration
    ENDWHILE.
    APPEND operator TO operator_stack.
  ENDMETHOD.

  METHOD evaluate_rpn.
    DATA value_stack TYPE STANDARD TABLE OF decfloat34.
    DATA operand1    TYPE decfloat34.
    DATA operand2    TYPE decfloat34.

    DATA token LIKE LINE OF tokens.
          DATA temp19 TYPE REF TO cx_sy_conversion_no_number.
        DATA temp20 LIKE LINE OF value_stack.
        DATA temp21 LIKE sy-tabix.
        DATA temp22 LIKE LINE OF value_stack.
        DATA temp23 LIKE sy-tabix.
      DATA temp24 LIKE LINE OF value_stack.
      DATA temp25 LIKE sy-tabix.
      DATA temp26 TYPE REF TO cx_sy_conversion_no_number.
    LOOP AT tokens INTO token.
      IF token-is_number = abap_true.
        APPEND parse_number( token-value ) TO value_stack.
      ELSE.
        IF lines( value_stack ) < 2.
          
          CREATE OBJECT temp19 TYPE cx_sy_conversion_no_number.
          RAISE EXCEPTION temp19.
        ENDIF.

        
        
        temp21 = sy-tabix.
        READ TABLE value_stack INDEX lines( value_stack ) INTO temp20.
        sy-tabix = temp21.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
        ENDIF.
        operand2 = temp20.
        DELETE value_stack INDEX lines( value_stack ).
        
        
        temp23 = sy-tabix.
        READ TABLE value_stack INDEX lines( value_stack ) INTO temp22.
        sy-tabix = temp23.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
        ENDIF.
        operand1 = temp22.
        DELETE value_stack INDEX lines( value_stack ).

        APPEND apply_operator( operator = token-value
                               operand1 = operand1
                               operand2 = operand2 )
               TO value_stack.
      ENDIF.
    ENDLOOP.

    IF lines( value_stack ) = 1.
      
      
      temp25 = sy-tabix.
      READ TABLE value_stack INDEX 1 INTO temp24.
      sy-tabix = temp25.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
      ENDIF.
      result = temp24.
    ELSE.
      
      CREATE OBJECT temp26 TYPE cx_sy_conversion_no_number.
      RAISE EXCEPTION temp26.
    ENDIF.
  ENDMETHOD.

  METHOD pop_from_stack.
    DATA last_index TYPE i.
      DATA temp27 LIKE LINE OF stack.
      DATA temp28 LIKE sy-tabix.
    last_index = lines( stack ).
    IF last_index > 0.
      
      
      temp28 = sy-tabix.
      READ TABLE stack INDEX last_index INTO temp27.
      sy-tabix = temp28.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
      ENDIF.
      result = temp27.  " Assign to the EXPORTING parameter
      DELETE stack INDEX last_index.
    ELSE.
      CLEAR result. "Clear in case of empty stack
    ENDIF.
  ENDMETHOD.

  METHOD peek_stack.

    DATA last_index TYPE i.
      DATA temp29 LIKE LINE OF stack.
      DATA temp30 LIKE sy-tabix.
    last_index = lines( stack ).
    IF last_index > 0.
      
      
      temp30 = sy-tabix.
      READ TABLE stack INDEX last_index INTO temp29.
      sy-tabix = temp30.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
      ENDIF.
      result = temp29. " Assign to the EXPORTING parameter
    ELSE.
      CLEAR result. "Clear Result
    ENDIF.
  ENDMETHOD.

  METHOD parse_number.
    DATA temp31 TYPE decfloat34.
    temp31 = number_string.
    result = temp31.
  ENDMETHOD.

  METHOD apply_operator.
          DATA temp32 TYPE REF TO cx_sy_zerodivide.
            DATA temp33 TYPE REF TO cx_sy_zerodivide.
          DATA temp34 TYPE REF TO cx_sy_zerodivide.
        DATA temp35 TYPE REF TO cx_sy_conversion_no_number.
    CASE operator.
      WHEN '+'.
        result = operand1 + operand2.
      WHEN '-'.
        result = operand1 - operand2.
      WHEN '*'.
        result = operand1 * operand2.
      WHEN '/'.
        IF operand2 = 0.
          
          CREATE OBJECT temp32 TYPE cx_sy_zerodivide.
          RAISE EXCEPTION temp32.
        ENDIF.
        result = operand1 / operand2.
      WHEN '**'.
        " Handle negative exponents
        IF operand2 < 0.
          IF operand1 = 0.
            
            CREATE OBJECT temp33 TYPE cx_sy_zerodivide.
            RAISE EXCEPTION temp33.
          ENDIF.
          result = 1 / ( operand1 ** abs( operand2 ) ).
        ELSE.
          result = operand1 ** operand2.
        ENDIF.
      WHEN 'MOD'.
        IF operand2 = 0.
          
          CREATE OBJECT temp34 TYPE cx_sy_zerodivide.
          RAISE EXCEPTION temp34.
        ENDIF.
        result = operand1 - ( operand2 * trunc( operand1 / operand2 ) ).
      WHEN OTHERS.
        
        CREATE OBJECT temp35 TYPE cx_sy_conversion_no_number.
        RAISE EXCEPTION temp35.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.

