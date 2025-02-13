CLASS ltcl_llm_options DEFINITION
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO zcl_llm_options_vertexai.

    METHODS setup.

    METHODS set_temperature_valid FOR TESTING.
    METHODS set_temperature_invalid FOR TESTING.

    METHODS set_top_p_valid FOR TESTING.
    METHODS set_top_p_invalid FOR TESTING.

    METHODS set_top_k_valid FOR TESTING.
    METHODS set_top_k_invalid FOR TESTING.

    METHODS set_seed_valid FOR TESTING.
    METHODS set_seed_invalid FOR TESTING.

    METHODS set_frequency_penalty_valid FOR TESTING.
    METHODS set_frequency_penalty_invalid FOR TESTING.

    METHODS set_presence_penalty_valid FOR TESTING.
    METHODS set_presence_penalty_invalid FOR TESTING.

    METHODS set_custom_parameters FOR TESTING.
    METHODS get_parameters FOR TESTING.

    METHODS set_custom_parameters_ovrwrte FOR TESTING.
ENDCLASS.                                           "#EC NUMBER_METHODS

CLASS ltcl_llm_options IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT cut.
  ENDMETHOD.

  METHOD set_temperature_valid.
    DATA temp5 TYPE decfloat16.
    DATA temp6 TYPE decfloat16.
    DATA temp7 TYPE decfloat16.
    temp5 = '1.0'.
    cut->set_temperature( temp5 ).
    
    temp6 = '0.0'.
    cut->set_temperature( temp6 ).
    
    temp7 = '2.0'.
    cut->set_temperature( temp7 ).
  ENDMETHOD.

  METHOD set_temperature_invalid.
        DATA temp8 TYPE decfloat16.
        DATA temp9 TYPE decfloat16.
    TRY.
        
        temp8 = '-0.1'.
        cut->set_temperature( temp8 ).
        cl_abap_unit_assert=>fail( 'Expected exception for temperature below 0' ).
      CATCH zcx_llm_validation.                        "#EC EMPTY_CATCH
    ENDTRY.

    TRY.
        
        temp9 = '2.1'.
        cut->set_temperature( temp9 ).
        cl_abap_unit_assert=>fail( 'Expected exception for temperature above 2' ).
      CATCH zcx_llm_validation.                        "#EC EMPTY_CATCH
    ENDTRY.
  ENDMETHOD.

  METHOD set_top_p_valid.
    DATA temp10 TYPE decfloat16.
    DATA temp11 TYPE decfloat16.
    DATA temp12 TYPE decfloat16.
    temp10 = '0.0'.
    cut->set_top_p( temp10 ).
    
    temp11 = '0.5'.
    cut->set_top_p( temp11 ).
    
    temp12 = '1.0'.
    cut->set_top_p( temp12 ).
  ENDMETHOD.

  METHOD set_top_p_invalid.
        DATA temp13 TYPE decfloat16.
        DATA temp14 TYPE decfloat16.
    TRY.
        
        temp13 = '-0.1'.
        cut->set_top_p( temp13 ).
        cl_abap_unit_assert=>fail( 'Expected exception for top_p below 0' ).
      CATCH zcx_llm_validation.                        "#EC EMPTY_CATCH
    ENDTRY.

    TRY.
        
        temp14 = '1.1'.
        cut->set_top_p( temp14 ).
        cl_abap_unit_assert=>fail( 'Expected exception for top_p above 1' ).
      CATCH zcx_llm_validation.                        "#EC EMPTY_CATCH
    ENDTRY.
  ENDMETHOD.

  METHOD set_top_k_valid.
    cut->set_top_k( 1 ).
    cut->set_top_k( 10 ).
    cut->set_top_k( 100 ).
  ENDMETHOD.

  METHOD set_top_k_invalid.
    TRY.
        cut->set_top_k( 0 ).
        cl_abap_unit_assert=>fail( 'Expected exception for top_k below 1' ).
      CATCH zcx_llm_validation.                        "#EC EMPTY_CATCH
    ENDTRY.
  ENDMETHOD.

  METHOD set_seed_valid.
    cut->set_seed( 0 ).
    cut->set_seed( 42 ).
    cut->set_seed( 100 ).
  ENDMETHOD.

  METHOD set_seed_invalid.
    TRY.
        cut->set_seed( -1 ).
        cl_abap_unit_assert=>fail( 'Expected exception for seed below 0' ).
      CATCH zcx_llm_validation.                        "#EC EMPTY_CATCH
    ENDTRY.
  ENDMETHOD.

  METHOD set_frequency_penalty_valid.
    DATA temp15 TYPE decfloat16.
    DATA temp16 TYPE decfloat16.
    DATA temp17 TYPE decfloat16.
    temp15 = '-2.0'.
    cut->set_frequency_penalty( temp15 ).
    
    temp16 = '0.0'.
    cut->set_frequency_penalty( temp16 ).
    
    temp17 = '2.0'.
    cut->set_frequency_penalty( temp17 ).
  ENDMETHOD.

  METHOD set_frequency_penalty_invalid.
        DATA temp18 TYPE decfloat16.
        DATA temp19 TYPE decfloat16.
    TRY.
        
        temp18 = '-2.1'.
        cut->set_frequency_penalty( temp18 ).
        cl_abap_unit_assert=>fail( 'Expected exception for frequency_penalty below -2' ).
      CATCH zcx_llm_validation.                        "#EC EMPTY_CATCH
    ENDTRY.

    TRY.
        
        temp19 = '2.1'.
        cut->set_frequency_penalty( temp19 ).
        cl_abap_unit_assert=>fail( 'Expected exception for frequency_penalty above 2' ).
      CATCH zcx_llm_validation.                        "#EC EMPTY_CATCH
    ENDTRY.
  ENDMETHOD.

  METHOD set_presence_penalty_valid.
    DATA temp20 TYPE decfloat16.
    DATA temp21 TYPE decfloat16.
    DATA temp22 TYPE decfloat16.
    temp20 = '-2.0'.
    cut->set_presence_penalty( temp20 ).
    
    temp21 = '0.0'.
    cut->set_presence_penalty( temp21 ).
    
    temp22 = '2.0'.
    cut->set_presence_penalty( temp22 ).
  ENDMETHOD.

  METHOD set_presence_penalty_invalid.
        DATA temp23 TYPE decfloat16.
        DATA temp24 TYPE decfloat16.
    TRY.
        
        temp23 = '-2.1'.
        cut->set_presence_penalty( temp23 ).
        cl_abap_unit_assert=>fail( 'Expected exception for presence_penalty below -2' ).
      CATCH zcx_llm_validation.                        "#EC EMPTY_CATCH
    ENDTRY.

    TRY.
        
        temp24 = '2.1'.
        cut->set_presence_penalty( temp24 ).
        cl_abap_unit_assert=>fail( 'Expected exception for presence_penalty above 2' ).
      CATCH zcx_llm_validation.                        "#EC EMPTY_CATCH
    ENDTRY.
  ENDMETHOD.

  METHOD set_custom_parameters.
    DATA parameters TYPE zllm_keyvalues.
    DATA temp25 TYPE zllm_keyvalue.
    DATA temp26 TYPE zllm_keyvalue.
    DATA result TYPE zllm_keyvalues.
    CLEAR temp25.
    temp25-key = 'custom_param1'.
    temp25-value = 'value1'.
    INSERT temp25 INTO TABLE parameters.
    
    CLEAR temp26.
    temp26-key = 'custom_param2'.
    temp26-value = 'value2'.
    INSERT temp26 INTO TABLE parameters.

    cut->set_custom_parameters( parameters ).

    
    result = cut->get_paramters( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 2
      act = lines( result )
    ).
  ENDMETHOD.

  METHOD get_parameters.
    DATA temp27 TYPE decfloat16.
    DATA result TYPE zllm_keyvalues.
    FIELD-SYMBOLS <param> LIKE LINE OF result.
    temp27 = '0.7'.
    cut->set_temperature( temp27 ).

    
    result = cut->get_paramters( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lines( result )
    ).

    
    LOOP AT result ASSIGNING <param>.
      cl_abap_unit_assert=>assert_equals(
        exp = 'temperature'
        act = <param>-key
      ).

      cl_abap_unit_assert=>assert_equals(
        exp = '0.7'
        act = <param>-value
      ).
    ENDLOOP.
  ENDMETHOD.

  METHOD set_custom_parameters_ovrwrte.
    " First set a temperature
    DATA temp28 TYPE decfloat16.
    DATA parameters TYPE zllm_keyvalues.
    DATA temp29 TYPE zllm_keyvalue.
    DATA result TYPE zllm_keyvalues.
    FIELD-SYMBOLS <param> LIKE LINE OF result.
    temp28 = '0.7'.
    cut->set_temperature( temp28 ).

    " Prepare custom parameters with a different temperature
    
    
    CLEAR temp29.
    temp29-key = 'temperature'.
    temp29-value = '0.5'.
    INSERT temp29 INTO TABLE parameters.

    " Set custom parameters
    cut->set_custom_parameters( parameters ).

    " Retrieve parameters
    
    result = cut->get_paramters( ).

    " Assert only one parameter exists
    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lines( result )
    ).

    " Check that the value has been overwritten
    
    LOOP AT result ASSIGNING <param>.
      cl_abap_unit_assert=>assert_equals(
        exp = 'temperature'
        act = <param>-key
      ).

      cl_abap_unit_assert=>assert_equals(
        exp = '0.5'
        act = <param>-value
      ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
