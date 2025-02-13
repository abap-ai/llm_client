CLASS ltcl_llm_so_default DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.
    DATA: cut TYPE REF TO zcl_llm_tool_parser.

    METHODS:
      setup,
      test_single_element FOR TESTING,
      test_simple_structure FOR TESTING,
      test_nested_structure FOR TESTING,
      test_table FOR TESTING,
      test_table_of_structs FOR TESTING,
      test_invalid_type FOR TESTING,
      test_boolean_field FOR TESTING,
      test_with_descriptions FOR TESTING,
      test_with_enum FOR TESTING,
      test_char1_nonbool FOR TESTING,
      test_invalid_element FOR TESTING,
      test_deep_nesting FOR TESTING,
      test_nested_path_descriptions FOR TESTING RAISING cx_static_check,
      test_multiple_tables_ordering FOR TESTING RAISING cx_static_check,
      test_empty_description FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_llm_so_default IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT cut.
  ENDMETHOD.

  METHOD test_single_element.
    TYPES: BEGIN OF element_type,
             field TYPE string,
           END OF element_type.

    DATA temp13 TYPE REF TO cl_abap_datadescr.
    DATA schema TYPE string.
    temp13 ?= cl_abap_datadescr=>describe_by_name( 'ELEMENT_TYPE' ).
    
    schema = cut->zif_llm_tool_parser~parse( temp13 ).

    cl_abap_unit_assert=>assert_char_cp(
      act = schema
      exp = `*"field":{"title":"Field","type":"string"*` ).
  ENDMETHOD.

  METHOD test_simple_structure.
    TYPES: BEGIN OF simple_type,
             id     TYPE i,
             name   TYPE string,
             amount TYPE decfloat34,
           END OF simple_type.

    DATA temp14 TYPE REF TO cl_abap_datadescr.
    DATA schema TYPE string.
    temp14 ?= cl_abap_datadescr=>describe_by_name( 'SIMPLE_TYPE' ).
    
    schema = cut->zif_llm_tool_parser~parse( temp14 ).

    cl_abap_unit_assert=>assert_char_cp(
      act = schema
      exp = `*"type":"object"*"properties"*"id"*"name"*"amount"*` ).
  ENDMETHOD.

  METHOD test_nested_structure.
    TYPES: BEGIN OF address_type,
             street TYPE string,
             city   TYPE string,
           END OF address_type.

    TYPES: BEGIN OF person_type,
             id      TYPE i,
             address TYPE address_type,
           END OF person_type.

    DATA temp15 TYPE REF TO cl_abap_datadescr.
    DATA schema TYPE string.
    temp15 ?= cl_abap_datadescr=>describe_by_name( 'PERSON_TYPE' ).
    
    schema = cut->zif_llm_tool_parser~parse( temp15 ).

    cl_abap_unit_assert=>assert_char_cp(
      act = schema
      exp = `*"address":{"title":"Address"*"properties"*"street"*"city"*` ).
  ENDMETHOD.

  METHOD test_table.
    TYPES: BEGIN OF test_type,
             items TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
           END OF test_type.

    DATA temp16 TYPE REF TO cl_abap_datadescr.
    DATA schema TYPE string.
    temp16 ?= cl_abap_datadescr=>describe_by_name( 'TEST_TYPE' ).
    
    schema = cut->zif_llm_tool_parser~parse( temp16 ).

    cl_abap_unit_assert=>assert_char_cp(
      act = schema
      exp = `*"items":{"type":"array"*` ).
  ENDMETHOD.

  METHOD test_table_of_structs.
    TYPES: BEGIN OF line_type,
             id   TYPE i,
             name TYPE string,
           END OF line_type.

    TYPES: BEGIN OF test_type,
             items TYPE STANDARD TABLE OF line_type WITH DEFAULT KEY,
           END OF test_type.

    DATA temp17 TYPE REF TO cl_abap_datadescr.
    DATA schema TYPE string.
    temp17 ?= cl_abap_datadescr=>describe_by_name( 'TEST_TYPE' ).
    
    schema = cut->zif_llm_tool_parser~parse( temp17 ).

    cl_abap_unit_assert=>assert_char_cp(
      act = schema
      exp = `*"items":{"type":"array"*"properties"*"id"*"name"*` ).
  ENDMETHOD.

  METHOD test_invalid_type.
    DATA test TYPE REF TO data.
        DATA temp18 TYPE REF TO cl_abap_datadescr.

    TRY.
        
        temp18 ?= cl_abap_datadescr=>describe_by_data( test ).
        cut->zif_llm_tool_parser~parse( temp18 ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_llm_validation.                        "#EC EMPTY_CATCH
        "Expected exception
    ENDTRY.
  ENDMETHOD.

  METHOD test_boolean_field.
    TYPES: BEGIN OF test_type,
             flag TYPE abap_bool,
           END OF test_type.

    DATA temp19 TYPE REF TO cl_abap_datadescr.
    DATA schema TYPE string.
    temp19 ?= cl_abap_datadescr=>describe_by_name( 'TEST_TYPE' ).
    
    schema = cut->zif_llm_tool_parser~parse( temp19 ).

    cl_abap_unit_assert=>assert_char_cp(
      act = schema
      exp = `*"flag":*"type":"boolean"*` ).
  ENDMETHOD.

  METHOD test_with_descriptions.
    TYPES: BEGIN OF test_type,
             id   TYPE i,
             name TYPE string,
           END OF test_type.

    DATA descriptions TYPE zif_llm_so=>def_descriptions.

    DATA temp20 TYPE zif_llm_so=>def_descriptions.
    DATA temp21 LIKE LINE OF temp20.
    DATA temp22 TYPE REF TO cl_abap_datadescr.
    DATA schema TYPE string.
    CLEAR temp20.
    
    temp21-fieldname = 'id'.
    temp21-description = 'Identifier'.
    INSERT temp21 INTO TABLE temp20.
    temp21-fieldname = 'name'.
    temp21-description = 'Full Name'.
    INSERT temp21 INTO TABLE temp20.
    descriptions = temp20.

    
    temp22 ?= cl_abap_datadescr=>describe_by_name( 'TEST_TYPE' ).
    
    schema = cut->zif_llm_tool_parser~parse(
        data_desc = temp22
        descriptions = descriptions ).

    cl_abap_unit_assert=>assert_char_cp(
      act = schema
      exp = `*"description":"Identifier"*"description":"Full Name"*` ).
  ENDMETHOD.

  METHOD test_with_enum.
    TYPES: BEGIN OF test_type,
             status TYPE string,
           END OF test_type.

    DATA descriptions TYPE zif_llm_so=>def_descriptions.

    DATA temp23 TYPE zif_llm_so=>def_descriptions.
    DATA temp24 LIKE LINE OF temp23.
    DATA temp8 TYPE string_table.
    DATA temp25 TYPE REF TO cl_abap_datadescr.
    DATA schema TYPE string.
    CLEAR temp23.
    
    temp24-fieldname = 'status'.
    temp24-description = 'Current Status'.
    
    CLEAR temp8.
    INSERT `NEW` INTO TABLE temp8.
    INSERT `IN_PROGRESS` INTO TABLE temp8.
    INSERT `DONE` INTO TABLE temp8.
    temp24-enum_values = temp8.
    INSERT temp24 INTO TABLE temp23.
    descriptions = temp23.

    
    temp25 ?= cl_abap_datadescr=>describe_by_name( 'TEST_TYPE' ).
    
    schema = cut->zif_llm_tool_parser~parse(
            data_desc = temp25
            descriptions = descriptions ).

    cl_abap_unit_assert=>assert_char_cp(
      act = schema
      exp = `*"status"*"enum":["NEW","IN_PROGRESS","DONE"]*` ).
  ENDMETHOD.

  METHOD test_char1_nonbool.
    TYPES: BEGIN OF test_type,
             category TYPE c LENGTH 1,
           END OF test_type.
        DATA temp26 TYPE REF TO cl_abap_datadescr.
        DATA ex TYPE REF TO zcx_llm_validation.

    TRY.
        
        temp26 ?= cl_abap_datadescr=>describe_by_name( 'TEST_TYPE' ).
        cut->zif_llm_tool_parser~parse( temp26 ).
        cl_abap_unit_assert=>fail( 'Should raise exception for non-boolean CHAR1' ).
        
      CATCH zcx_llm_validation INTO ex.          "#EC EMPTY_CATCH
    ENDTRY.
  ENDMETHOD.

  METHOD test_invalid_element.
    TYPES: BEGIN OF test_type,
             "Date type is not supported
             date TYPE d,
           END OF test_type.
        DATA temp27 TYPE REF TO cl_abap_datadescr.
        DATA ex TYPE REF TO zcx_llm_validation.

    TRY.
        
        temp27 ?= cl_abap_datadescr=>describe_by_name( 'TEST_TYPE' ).
        cut->zif_llm_tool_parser~parse( temp27 ).
        cl_abap_unit_assert=>fail( 'Should raise exception for unsupported type' ).
        
      CATCH zcx_llm_validation INTO ex.          "#EC EMPTY_CATCH
    ENDTRY.
  ENDMETHOD.

  METHOD test_deep_nesting.
    TYPES: BEGIN OF detail_type,
             code       TYPE string,
             quantity   TYPE i,
             unit_price TYPE decfloat34,
           END OF detail_type.

    TYPES: BEGIN OF item_type,
             item_id TYPE i,
             details TYPE STANDARD TABLE OF detail_type WITH DEFAULT KEY,
           END OF item_type.

    TYPES: BEGIN OF order_type,
             id       TYPE i,
             customer TYPE string,
             items    TYPE STANDARD TABLE OF item_type WITH DEFAULT KEY,
             total    TYPE decfloat34,
           END OF order_type.

    TYPES: BEGIN OF root_type,
             orders TYPE STANDARD TABLE OF order_type WITH DEFAULT KEY,
             meta   TYPE string,
           END OF root_type.

    DATA descriptions TYPE zif_llm_so=>def_descriptions.

    DATA temp28 TYPE zif_llm_so=>def_descriptions.
    DATA temp29 LIKE LINE OF temp28.
    DATA temp30 TYPE REF TO cl_abap_datadescr.
    DATA schema TYPE string.
    CLEAR temp28.
    
    temp29-fieldname = 'orders'.
    temp29-description = 'Order List'.
    INSERT temp29 INTO TABLE temp28.
    temp29-fieldname = 'orders-items'.
    temp29-description = 'Order Items'.
    INSERT temp29 INTO TABLE temp28.
    temp29-fieldname = 'orders-items-details'.
    temp29-description = 'Item Details'.
    INSERT temp29 INTO TABLE temp28.
    descriptions = temp28.

    
    temp30 ?= cl_abap_datadescr=>describe_by_name( 'ROOT_TYPE' ).
    
    schema = cut->zif_llm_tool_parser~parse(
        data_desc = temp30
        descriptions = descriptions ).

    "Check structure hierarchy
    cl_abap_unit_assert=>assert_char_cp(
      act = schema
      exp = `*"orders":{"type":"array"*` ).

    cl_abap_unit_assert=>assert_char_cp(
      act = schema
      exp = `*"items":{"type":"array"*` ).

    cl_abap_unit_assert=>assert_char_cp(
      act = schema
      exp = `*"details":{"type":"array"*` ).

    cl_abap_unit_assert=>assert_char_cp(
      act = schema
      exp = `*"item_id"*"code"*"quantity"*"unit_price"*` ).

    "Check descriptions
    cl_abap_unit_assert=>assert_char_cp(
      act = schema
      exp = `*"description":"Order List"*` ).

    cl_abap_unit_assert=>assert_char_cp(
      act = schema
      exp = `*"description":"Item Details"*` ).

    "Additional assertions in test_deep_nesting
    cl_abap_unit_assert=>assert_char_cp(
      act = schema
      exp = `*"orders":{"type":"array"*,"meta":*` ).

    cl_abap_unit_assert=>assert_char_cp(
      act = schema
      exp = `*"items":{"type":"array"*,"total":*` ).
  ENDMETHOD.

  METHOD test_nested_path_descriptions.
    "Test correct path handling for descriptions
    TYPES: BEGIN OF detail_type,
             code TYPE string,
           END OF detail_type.

    TYPES: BEGIN OF test_type,
             items TYPE STANDARD TABLE OF detail_type WITH DEFAULT KEY,
           END OF test_type.

    DATA descriptions TYPE zif_llm_so=>def_descriptions.

    DATA temp31 TYPE zif_llm_so=>def_descriptions.
    DATA temp32 LIKE LINE OF temp31.
    DATA temp33 TYPE REF TO cl_abap_datadescr.
    DATA schema TYPE string.
    CLEAR temp31.
    
    temp32-fieldname = 'items'.
    temp32-description = 'Items List'.
    INSERT temp32 INTO TABLE temp31.
    temp32-fieldname = 'items-code'.
    temp32-description = 'Item Code'.
    INSERT temp32 INTO TABLE temp31.
    descriptions = temp31.

    
    temp33 ?= cl_abap_datadescr=>describe_by_name( 'TEST_TYPE' ).
    
    schema = cut->zif_llm_tool_parser~parse(
        data_desc = temp33
        descriptions = descriptions ).

    cl_abap_unit_assert=>assert_char_cp(
      act = schema
      exp = `*"description":"Items List"*"description":"Item Code"*` ).
  ENDMETHOD.

  METHOD test_multiple_tables_ordering.
    "Test correct comma placement between multiple tables
    TYPES: BEGIN OF test_type,
             table1 TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
             field  TYPE string,
             table2 TYPE STANDARD TABLE OF i WITH DEFAULT KEY,
           END OF test_type.

    DATA temp34 TYPE REF TO cl_abap_datadescr.
    DATA schema TYPE string.
    temp34 ?= cl_abap_datadescr=>describe_by_name( 'TEST_TYPE' ).
    
    schema = cut->zif_llm_tool_parser~parse( temp34 ).

    cl_abap_unit_assert=>assert_char_cp(
      act = schema
      exp = `*"table1":{"type":"array"*,"field":*,"table2":{"type":"array"*` ).
  ENDMETHOD.

  METHOD test_empty_description.
    "Test handling of empty/missing descriptions
    TYPES: BEGIN OF test_type,
             field TYPE string,
           END OF test_type.

    DATA descriptions TYPE zif_llm_so=>def_descriptions.

    DATA temp35 TYPE REF TO cl_abap_datadescr.
    DATA schema TYPE string.
    temp35 ?= cl_abap_datadescr=>describe_by_name( 'TEST_TYPE' ).
    
    schema = cut->zif_llm_tool_parser~parse(
        data_desc = temp35
        descriptions = descriptions ).

    cl_abap_unit_assert=>assert_char_cp(
      act = schema
      exp = `*"field":{"title":"Field","type":"string"*` ).
  ENDMETHOD.

ENDCLASS.
