"! <p class="shorttext synchronized" lang="en">Gemini Tool Parser using JSON Schema</p>
"! title and additionalattributes are not supported
CLASS zcl_llm_tool_parser_gemini DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_llm_tool_parser.

  PROTECTED SECTION.
    TYPES:
      BEGIN OF field_info,
        name        TYPE string,
        path        TYPE string,
        description TYPE zif_llm_tool_parser=>def_description,
      END OF field_info.

    DATA descriptions TYPE zif_llm_tool_parser=>def_descriptions.
    DATA schema       TYPE string.

    METHODS pre_schema.
    METHODS post_schema.

    METHODS pre_object
      IMPORTING !field TYPE field_info
      RAISING   zcx_llm_validation ##NEEDED.

    METHODS post_object
      IMPORTING !field TYPE field_info
      RAISING   zcx_llm_validation ##NEEDED.

    METHODS pre_array.
    METHODS post_array.

    METHODS process_type
      IMPORTING type_descriptor TYPE REF TO cl_abap_typedescr
                !field          TYPE field_info
      RAISING   zcx_llm_validation.

    METHODS process_structure
      IMPORTING structure_descriptor TYPE REF TO cl_abap_structdescr
                !field               TYPE field_info
      RAISING   zcx_llm_validation.

    METHODS process_table
      IMPORTING table_descriptor TYPE REF TO cl_abap_tabledescr
                !field           TYPE field_info
      RAISING   zcx_llm_validation.

    METHODS process_element
      IMPORTING element_descriptor TYPE REF TO cl_abap_elemdescr
                !field             TYPE field_info
      RAISING   zcx_llm_validation.

    METHODS get_field_info
      IMPORTING !name         TYPE string OPTIONAL
                !path         TYPE string OPTIONAL
      RETURNING VALUE(result) TYPE field_info.

    METHODS append_to_schema
      IMPORTING content TYPE string.

    METHODS get_path
      IMPORTING current_path  TYPE string OPTIONAL
                field_name    TYPE string OPTIONAL
      RETURNING VALUE(result) TYPE string.

    METHODS get_enum_values
      IMPORTING !description  TYPE zif_llm_tool_parser=>def_description
      RETURNING VALUE(result) TYPE string.

    CONSTANTS typekind_int8 TYPE abap_typekind VALUE '8' ##NO_TEXT.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_llm_tool_parser_gemini IMPLEMENTATION.
  METHOD append_to_schema.
    schema = schema && content.
  ENDMETHOD.

  METHOD get_enum_values.
    DATA temp1 TYPE string.
    DATA temp TYPE string.
    DATA value LIKE LINE OF description-enum_values.
      DATA idx LIKE sy-tabix.
      DATA temp2 TYPE string.
    temp = ``.
    
    LOOP AT description-enum_values INTO value.
      
      idx = sy-tabix.
      
      IF idx = 1.
        temp2 = |"{ value }"|.
      ELSE.
        temp2 = |{ temp },"{ value }"|.
      ENDIF.
      temp = temp2.
    ENDLOOP.
    temp1 = temp.
    result = temp1.
  ENDMETHOD.

  METHOD get_field_info.
    DATA temp3 TYPE zif_llm_tool_parser=>def_description.
    DATA temp4 TYPE zif_llm_tool_parser=>def_description.
    CLEAR result.
    result-name = name.
    result-path = path.
    
    CLEAR temp3.
    
    READ TABLE descriptions INTO temp4 WITH KEY fieldname = path.
    IF sy-subrc = 0.
      temp3 = temp4.
    ENDIF.
    result-description = temp3.
  ENDMETHOD.

  METHOD get_path.
    DATA temp2 TYPE string.
    IF current_path IS INITIAL.
      temp2 = to_lower( field_name ).
    ELSE.
      temp2 = |{ current_path }-{ to_lower( field_name ) }|.
    ENDIF.
    result = temp2.
  ENDMETHOD.


  METHOD post_array.
  ENDMETHOD.                                       "#EC EMPTY_PROCEDURE

  METHOD post_object.
  ENDMETHOD.                                       "#EC EMPTY_PROCEDURE

  METHOD post_schema.
  ENDMETHOD.                                       "#EC EMPTY_PROCEDURE

  METHOD pre_array.
  ENDMETHOD.                                       "#EC EMPTY_PROCEDURE

  METHOD pre_object.
  ENDMETHOD.                                       "#EC EMPTY_PROCEDURE

  METHOD pre_schema.
  ENDMETHOD.                                       "#EC EMPTY_PROCEDURE

  METHOD process_element.
          DATA temp3 TYPE REF TO zcx_llm_validation.
        DATA temp4 TYPE REF TO zcx_llm_validation.
    IF field-name IS NOT INITIAL.
      append_to_schema( |"{ field-name }":\{| ).
    ENDIF.

    CASE element_descriptor->type_kind.
      WHEN cl_abap_typedescr=>typekind_int OR typekind_int8.
        append_to_schema( |"type":"integer"| ).
      WHEN cl_abap_typedescr=>typekind_decfloat16 OR cl_abap_typedescr=>typekind_decfloat34.
        append_to_schema( |"type":"number"| ).
      WHEN cl_abap_typedescr=>typekind_string.
        append_to_schema( |"type":"string"| ).
      WHEN cl_abap_typedescr=>typekind_char.
        IF     element_descriptor->output_length = 1
           AND '\TYPE-POOL=ABAP\TYPE=ABAP_BOOL\TYPE=BOOLEAN\TYPE=BOOLE_D\TYPE=XFELD' CS element_descriptor->absolute_name.
          append_to_schema( |"type":"boolean"| ).
        ELSE.
          
          CREATE OBJECT temp3 TYPE zcx_llm_validation EXPORTING textid = zcx_llm_validation=>unsupported_type attr1 = |Unsupported elementary type: { element_descriptor->type_kind }|.
          RAISE EXCEPTION temp3 ##NO_TEXT.
        ENDIF.
      WHEN OTHERS.
        
        CREATE OBJECT temp4 TYPE zcx_llm_validation EXPORTING textid = zcx_llm_validation=>unsupported_type attr1 = |Unsupported elementary type: { element_descriptor->type_kind }|.
        RAISE EXCEPTION temp4 ##NO_TEXT.
    ENDCASE.

    IF field-description-description IS NOT INITIAL.
      append_to_schema( |,"description":"{ escape( val    = field-description-description
                                                   format = cl_abap_format=>e_json_string ) }"| ).
    ENDIF.

    IF lines( field-description-enum_values ) > 0.
      append_to_schema( |,"enum":[{ get_enum_values( field-description ) }]| ).
    ENDIF.
    IF field-name IS NOT INITIAL.
      append_to_schema( |\}| ).
    ENDIF.
  ENDMETHOD.

  METHOD process_structure.
    DATA components TYPE abap_component_tab.
    DATA needs_comma TYPE abap_bool.
    DATA temp5 LIKE LINE OF components.
    DATA component LIKE REF TO temp5.
      DATA child_field TYPE zcl_llm_tool_parser_gemini=>field_info.
    DATA temp6 TYPE string.
    DATA result TYPE string.
    DATA comp LIKE LINE OF components.
      DATA idx LIKE sy-tabix.
      DATA temp7 TYPE string.
    IF field-name IS NOT INITIAL.
      append_to_schema( |,"{ field-name }":\{| ).
      IF field-description-description IS NOT INITIAL.
        append_to_schema( |"description":"{ escape( val    = field-description-description
                                                    format = cl_abap_format=>e_json_string ) }"| ).
      ENDIF.
    ENDIF.

    pre_object( field ).

    IF field-name IS NOT INITIAL.
      append_to_schema( |,"type":"object","properties":\{| ).
    ELSE.
      append_to_schema( |"type":"object","properties":\{| ).
    ENDIF.

    
    components = structure_descriptor->get_components( ).
    

    
    
    LOOP AT components REFERENCE INTO component.
      " Add comma if needed from previous iteration
      IF needs_comma = abap_true.
        append_to_schema( |,| ).
      ENDIF.
      " Default to true for next iteration
      needs_comma = abap_true.

      
      child_field = get_field_info( name = to_lower( component->name )
                                          path = get_path( current_path = field-path
                                                           field_name   = component->name ) ).

      process_type( type_descriptor = component->type
                    field           = child_field ).
    ENDLOOP.

    append_to_schema( |\},"required":[| ).
    
    
    result = ``.
    
    LOOP AT components INTO comp.
      
      idx = sy-tabix.
      
      IF idx = 1.
        temp7 = |"{ to_lower( comp-name ) }"|.
      ELSE.
        temp7 = |{ result },"{ to_lower( comp-name ) }"|.
      ENDIF.
      result = temp7.
    ENDLOOP.
    temp6 = result.
    append_to_schema( temp6 ).
    append_to_schema( |]| ).

    post_object( field ).

    IF field-name IS NOT INITIAL.
      append_to_schema( |\}| ).
    ENDIF.
  ENDMETHOD.

  METHOD process_table.
    DATA temp7 TYPE string.
    DATA child_field TYPE zcl_llm_tool_parser_gemini=>field_info.
    DATA line_type TYPE REF TO cl_abap_datadescr.
    IF field-name IS NOT INITIAL.
      append_to_schema( |"{ field-name }":\{"type":"array"| ).
    ENDIF.
    pre_array( ).

    IF field-description-description IS NOT INITIAL.
      append_to_schema( |,"description":"{ escape( val    = field-description-description
                                                   format = cl_abap_format=>e_json_string ) }"| ).
    ENDIF.
    append_to_schema( |,"items":\{| ).

    
    IF field-path IS INITIAL AND field-name IS INITIAL.
      temp7 = ''.
    ELSEIF field-path IS INITIAL.
      temp7 = field-name.
    ELSE.
      temp7 = field-path.
    ENDIF.
    
    child_field = get_field_info( path = temp7 ).

    
    line_type = table_descriptor->get_table_line_type( ).
    process_type( type_descriptor = line_type
                  field           = child_field ).

    append_to_schema( |\}| ).
    post_array( ).
    IF field-name IS NOT INITIAL.
      append_to_schema( |\}| ).
    ENDIF.
  ENDMETHOD.

  METHOD process_type.
        DATA temp8 TYPE REF TO cl_abap_structdescr.
        DATA temp9 TYPE REF TO cl_abap_tabledescr.
        DATA temp10 TYPE REF TO cl_abap_elemdescr.
        DATA temp11 TYPE REF TO zcx_llm_validation.
    CASE type_descriptor->kind.
      WHEN cl_abap_typedescr=>kind_struct.
        
        temp8 ?= type_descriptor.
        process_structure( structure_descriptor = temp8
                           field                = field ).
      WHEN cl_abap_typedescr=>kind_table.
        
        temp9 ?= type_descriptor.
        process_table( table_descriptor = temp9
                       field            = field ).
      WHEN cl_abap_typedescr=>kind_elem.
        
        temp10 ?= type_descriptor.
        process_element( element_descriptor = temp10
                         field              = field ).
      WHEN OTHERS.
        
        CREATE OBJECT temp11 TYPE zcx_llm_validation EXPORTING textid = zcx_llm_validation=>unsupported_type attr1 = |Unsupported type: { type_descriptor->kind }|.
        RAISE EXCEPTION temp11 ##NO_TEXT.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_llm_tool_parser~parse.
        DATA temp12 TYPE REF TO zcx_llm_validation.
    " Check if the schema is already available, then just return it (we are called twice)
    IF schema IS NOT INITIAL.
      result = schema.
      RETURN.
    ENDIF.

    me->descriptions = descriptions.

    append_to_schema( |\{| ).
    pre_schema( ).

    CASE data_desc->kind.
      WHEN cl_abap_typedescr=>kind_struct.
        process_type( type_descriptor = data_desc
                      field           = get_field_info( ) ).
      WHEN OTHERS.
        
        CREATE OBJECT temp12 TYPE zcx_llm_validation EXPORTING textid = zcx_llm_validation=>unsupported_type attr1 = |Unsupported type: { data_desc->kind }|.
        RAISE EXCEPTION temp12 ##NO_TEXT.
    ENDCASE.

    post_schema( ).
    append_to_schema( |\}| ).
    result = schema.
  ENDMETHOD.
ENDCLASS.
