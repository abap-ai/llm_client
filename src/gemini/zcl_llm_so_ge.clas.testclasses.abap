CLASS ltcl_llm_so_default DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: cut TYPE REF TO zcl_llm_so_ge.

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
    cut = NEW #( ).
  ENDMETHOD.

  METHOD test_single_element.
    TYPES: BEGIN OF element_type,
             field TYPE string,
           END OF element_type.

    cut->zif_llm_so~set_schema( CAST #( cl_abap_datadescr=>describe_by_name( 'ELEMENT_TYPE' ) ) ).
    DATA(schema) = cut->zif_llm_so~get_schema( ).

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

    cut->zif_llm_so~set_schema( CAST #( cl_abap_datadescr=>describe_by_name( 'SIMPLE_TYPE' ) ) ).
    DATA(schema) = cut->zif_llm_so~get_schema( ).

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

    cut->zif_llm_so~set_schema( CAST #( cl_abap_datadescr=>describe_by_name( 'PERSON_TYPE' ) ) ).
    DATA(schema) = cut->zif_llm_so~get_schema( ).

    cl_abap_unit_assert=>assert_char_cp(
      act = schema
      exp = `*"address":{"title":"Address"*"properties"*"street"*"city"*` ).
  ENDMETHOD.

  METHOD test_table.
    TYPES: BEGIN OF test_type,
             items TYPE STANDARD TABLE OF string WITH EMPTY KEY,
           END OF test_type.

    cut->zif_llm_so~set_schema( CAST #( cl_abap_datadescr=>describe_by_name( 'TEST_TYPE' ) ) ).
    DATA(schema) = cut->zif_llm_so~get_schema( ).

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
             items TYPE STANDARD TABLE OF line_type WITH EMPTY KEY,
           END OF test_type.

    cut->zif_llm_so~set_schema( CAST #( cl_abap_datadescr=>describe_by_name( 'TEST_TYPE' ) ) ).
    DATA(schema) = cut->zif_llm_so~get_schema( ).

    cl_abap_unit_assert=>assert_char_cp(
      act = schema
      exp = `*"items":{"type":"array"*"properties"*"id"*"name"*` ).
  ENDMETHOD.

  METHOD test_invalid_type.
    DATA test TYPE REF TO data.

    TRY.
        cut->zif_llm_so~set_schema( CAST #( cl_abap_datadescr=>describe_by_data( test ) ) ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_llm_validation.                        "#EC EMPTY_CATCH
        "Expected exception
    ENDTRY.
  ENDMETHOD.

  METHOD test_boolean_field.
    TYPES: BEGIN OF test_type,
             flag TYPE abap_bool,
           END OF test_type.

    cut->zif_llm_so~set_schema( CAST #( cl_abap_datadescr=>describe_by_name( 'TEST_TYPE' ) ) ).
    DATA(schema) = cut->zif_llm_so~get_schema( ).

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

    descriptions = VALUE #(
      ( fieldname = 'id' description = 'Identifier' )
      ( fieldname = 'name' description = 'Full Name' ) ).

    cut->zif_llm_so~set_schema(
          data_desc = CAST #( cl_abap_datadescr=>describe_by_name( 'TEST_TYPE' ) )
          description = descriptions ).
    DATA(schema) = cut->zif_llm_so~get_schema( ).

    cl_abap_unit_assert=>assert_char_cp(
      act = schema
      exp = `*"description":"Identifier"*"description":"Full Name"*` ).
  ENDMETHOD.

  METHOD test_with_enum.
    TYPES: BEGIN OF test_type,
             status TYPE string,
           END OF test_type.

    DATA descriptions TYPE zif_llm_so=>def_descriptions.

    descriptions = VALUE #(
      ( fieldname = 'status'
        description = 'Current Status'
        enum_values = VALUE #( ( `NEW` ) ( `IN_PROGRESS` ) ( `DONE` ) ) ) ).

    cut->zif_llm_so~set_schema(
          data_desc = CAST #( cl_abap_datadescr=>describe_by_name( 'TEST_TYPE' ) )
          description = descriptions ).
    DATA(schema) = cut->zif_llm_so~get_schema( ).

    cl_abap_unit_assert=>assert_char_cp(
      act = schema
      exp = `*"status"*"enum":["NEW","IN_PROGRESS","DONE"]*` ).
  ENDMETHOD.

  METHOD test_char1_nonbool.
    TYPES: BEGIN OF test_type,
             category TYPE c LENGTH 1,
           END OF test_type.

    TRY.
        cut->zif_llm_so~set_schema( CAST #( cl_abap_datadescr=>describe_by_name( 'TEST_TYPE' ) ) ).
        cl_abap_unit_assert=>fail( 'Should raise exception for non-boolean CHAR1' ).
      CATCH zcx_llm_validation INTO DATA(ex).          "#EC EMPTY_CATCH
    ENDTRY.
  ENDMETHOD.

  METHOD test_invalid_element.
    TYPES: BEGIN OF test_type,
             "Date type is not supported
             date TYPE d,
           END OF test_type.

    TRY.
        cut->zif_llm_so~set_schema( CAST #( cl_abap_datadescr=>describe_by_name( 'TEST_TYPE' ) ) ).
        cl_abap_unit_assert=>fail( 'Should raise exception for unsupported type' ).
      CATCH zcx_llm_validation INTO DATA(ex).          "#EC EMPTY_CATCH
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
             details TYPE STANDARD TABLE OF detail_type WITH EMPTY KEY,
           END OF item_type.

    TYPES: BEGIN OF order_type,
             id       TYPE i,
             customer TYPE string,
             items    TYPE STANDARD TABLE OF item_type WITH EMPTY KEY,
             total    TYPE decfloat34,
           END OF order_type.

    TYPES: BEGIN OF root_type,
             orders TYPE STANDARD TABLE OF order_type WITH EMPTY KEY,
             meta   TYPE string,
           END OF root_type.

    DATA descriptions TYPE zif_llm_so=>def_descriptions.

    descriptions = VALUE #(
      ( fieldname = 'orders' description = 'Order List' )
      ( fieldname = 'orders-items' description = 'Order Items' )
      ( fieldname = 'orders-items-details' description = 'Item Details' ) ).

    cut->zif_llm_so~set_schema(
      data_desc = CAST #( cl_abap_datadescr=>describe_by_name( 'ROOT_TYPE' ) )
      description = descriptions ).
    DATA(schema) = cut->zif_llm_so~get_schema( ).

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
             items TYPE STANDARD TABLE OF detail_type WITH EMPTY KEY,
           END OF test_type.

    DATA descriptions TYPE zif_llm_so=>def_descriptions.

    descriptions = VALUE #(
      ( fieldname = 'items' description = 'Items List' )
      ( fieldname = 'items-code' description = 'Item Code' ) ).

    cut->zif_llm_so~set_schema(
      data_desc = CAST #( cl_abap_datadescr=>describe_by_name( 'TEST_TYPE' ) )
      description = descriptions ).
    DATA(schema) = cut->zif_llm_so~get_schema( ).

    cl_abap_unit_assert=>assert_char_cp(
      act = schema
      exp = `*"description":"Items List"*"description":"Item Code"*` ).
  ENDMETHOD.

  METHOD test_multiple_tables_ordering.
    "Test correct comma placement between multiple tables
    TYPES: BEGIN OF test_type,
             table1 TYPE STANDARD TABLE OF string WITH EMPTY KEY,
             field  TYPE string,
             table2 TYPE STANDARD TABLE OF i WITH EMPTY KEY,
           END OF test_type.

    cut->zif_llm_so~set_schema( CAST #( cl_abap_datadescr=>describe_by_name( 'TEST_TYPE' ) ) ).
    DATA(schema) = cut->zif_llm_so~get_schema( ).

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

    cut->zif_llm_so~set_schema(
          data_desc = CAST #( cl_abap_datadescr=>describe_by_name( 'TEST_TYPE' ) )
          description = descriptions ).

    DATA(schema) = cut->zif_llm_so~get_schema( ).

    cl_abap_unit_assert=>assert_char_cp(
      act = schema
      exp = `*"field":{"title":"Field","type":"string"*` ).
  ENDMETHOD.

ENDCLASS.
