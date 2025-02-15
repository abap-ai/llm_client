"! <p class="shorttext synchronized" lang="en">Template parser based on limited Jinja2</p>
CLASS zcl_llm_template_parser DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF token_type,
        type    TYPE string,
        content TYPE string,
      END OF token_type,
      tokens_type TYPE STANDARD TABLE OF token_type WITH EMPTY KEY.

    TYPES:
      BEGIN OF template_type,
        name    TYPE string,
        content TYPE string,
        tokens  TYPE tokens_type,
      END OF template_type,
      templates_type TYPE SORTED TABLE OF template_type WITH UNIQUE KEY name.

    METHODS constructor.

    "! <p class="shorttext synchronized">Add or replace a template</p>
    "! @parameter name                    | <p class="shorttext synchronized">Name of the template</p>
    "! @parameter content                 | <p class="shorttext synchronized">Content of the template</p>
    "! @parameter replace                 | <p class="shorttext synchronized">Replace template with identical name</p>
    "! @raising   zcx_llm_template_parser | <p class="shorttext synchronized">If template parsing fails</p>
    METHODS add_template
      IMPORTING !name    TYPE string
                content  TYPE string
                !replace TYPE sap_bool DEFAULT abap_true
      RAISING   zcx_llm_template_parser.

    "! <p class="shorttext synchronized">Render a template with given context</p>
    "! @parameter template_name           | <p class="shorttext synchronized">Name of the template to render</p>
    "! @parameter context                 | <p class="shorttext synchronized">Data context for variable resolution</p>
    "! @parameter result                  | <p class="shorttext synchronized">Rendered template string</p>
    "! @raising   zcx_llm_template_parser | <p class="shorttext synchronized">If rendering fails</p>
    METHODS render
      IMPORTING template_name TYPE string
                !context      TYPE REF TO data
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_llm_template_parser.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF token_types,
        text     TYPE string VALUE 'TEXT',
        variable TYPE string VALUE 'VARIABLE',
        control  TYPE string VALUE 'CONTROL',
        comment  TYPE string VALUE 'COMMENT',
      END OF token_types.

    TYPES:
      BEGIN OF loop_meta_type,
        index TYPE i,
        first TYPE abap_bool,
        last  TYPE abap_bool,
      END OF loop_meta_type.

    TYPES:
      BEGIN OF control_stack_type,
        type              TYPE string,
        condition_met     TYPE abap_bool,
        any_condition_met TYPE abap_bool,
        tokens            TYPE tokens_type,
        loop_var          TYPE string,
        collection        TYPE REF TO data,
        loop_index        TYPE i,
        loop_tokens       TYPE tokens_type,
      END OF control_stack_type.

    TYPES control_stack_types TYPE STANDARD TABLE OF control_stack_type.

    DATA templates TYPE templates_type.

    "! <p class="shorttext synchronized">Tokenize template string into sequence of tokens</p>
    "! @parameter template                | <p class="shorttext synchronized">Template string to tokenize</p>
    "! @parameter tokens                  | <p class="shorttext synchronized">Resulting sequence of tokens</p>
    "! @raising   zcx_llm_template_parser | <p class="shorttext synchronized">If tokenization fails</p>
    METHODS tokenize
      IMPORTING template      TYPE string
      RETURNING VALUE(tokens) TYPE tokens_type
      RAISING   zcx_llm_template_parser.

    "! <p class="shorttext synchronized">Parse tokens and replace variables with values</p>
    "! @parameter tokens                  | <p class="shorttext synchronized">Tokens to parse</p>
    "! @parameter context                 | <p class="shorttext synchronized">Data context for variable resolution</p>
    "! @parameter result                  | <p class="shorttext synchronized">Resulting parsed string</p>
    "! @raising   zcx_llm_template_parser | <p class="shorttext synchronized">If parsing fails</p>
    METHODS parse_tokens
      IMPORTING !tokens       TYPE tokens_type
                !context      TYPE REF TO data
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_llm_template_parser.

    "! <p class="shorttext synchronized">Resolve variable path to its value</p>
    "! @parameter variable_path           | <p class="shorttext synchronized">Path to the variable</p>
    "! @parameter context                 | <p class="shorttext synchronized">Data context</p>
    "! @parameter control_stack           | <p class="shorttext synchronized">Current control structure stack</p>
    "! @parameter result                  | <p class="shorttext synchronized">Resolved variable value as string</p>
    "! @raising   zcx_llm_template_parser | <p class="shorttext synchronized">If variable resolution fails</p>
    METHODS resolve_variable
      IMPORTING variable_path TYPE string
                !context      TYPE REF TO data
                control_stack TYPE control_stack_types OPTIONAL
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_llm_template_parser.

    "! <p class="shorttext synchronized">Get template by its name</p>
    "! @parameter name                    | <p class="shorttext synchronized">Template name</p>
    "! @parameter content                 | <p class="shorttext synchronized">Template content reference</p>
    "! @raising   zcx_llm_template_parser | <p class="shorttext synchronized">If template not found</p>
    METHODS get_template_by_name
      IMPORTING !name          TYPE string
      RETURNING VALUE(content) TYPE REF TO template_type
      RAISING   zcx_llm_template_parser.

    "! <p class="shorttext synchronized">Evaluate if condition is true</p>
    "! @parameter condition               | <p class="shorttext synchronized">Condition to evaluate</p>
    "! @parameter context                 | <p class="shorttext synchronized">Data context</p>
    "! @parameter result                  | <p class="shorttext synchronized">True if condition is met</p>
    "! @raising   zcx_llm_template_parser | <p class="shorttext synchronized">If evaluation fails</p>
    METHODS evaluate_condition_true
      IMPORTING !condition    TYPE string
                !context      TYPE REF TO data
      RETURNING VALUE(result) TYPE abap_bool
      RAISING   zcx_llm_template_parser.

    "! <p class="shorttext synchronized">Apply filter to value</p>
    "! @parameter value                   | <p class="shorttext synchronized">Value to filter</p>
    "! @parameter filter                  | <p class="shorttext synchronized">Filter to apply</p>
    "! @parameter param                   | <p class="shorttext synchronized">Optional filter parameter</p>
    "! @parameter result                  | <p class="shorttext synchronized">Filtered value</p>
    "! @raising   zcx_llm_template_parser | <p class="shorttext synchronized">If filter application fails</p>
    METHODS apply_filter
      IMPORTING !value        TYPE string
                !filter       TYPE string
                param         TYPE string OPTIONAL
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_llm_template_parser.

    "! <p class="shorttext synchronized">Resolve variable path to data reference</p>
    "! @parameter variable_path           | <p class="shorttext synchronized">Path to the variable</p>
    "! @parameter context                 | <p class="shorttext synchronized">Data context</p>
    "! @parameter result                  | <p class="shorttext synchronized">Reference to resolved data</p>
    "! @raising   zcx_llm_template_parser | <p class="shorttext synchronized">If resolution fails</p>
    METHODS resolve_variable_ref
      IMPORTING variable_path TYPE string
                !context      TYPE REF TO data
      RETURNING VALUE(result) TYPE REF TO data
      RAISING   zcx_llm_template_parser.

    "! <p class="shorttext synchronized">Process loop content with given context</p>
    "! @parameter tokens                  | <p class="shorttext synchronized">Tokens in loop body</p>
    "! @parameter context                 | <p class="shorttext synchronized">Data context</p>
    "! @parameter loop_var                | <p class="shorttext synchronized">Loop variable name</p>
    "! @parameter collection              | <p class="shorttext synchronized">Collection to iterate</p>
    "! @parameter result                  | <p class="shorttext synchronized">Processed loop content</p>
    "! @raising   zcx_llm_template_parser | <p class="shorttext synchronized">If processing fails</p>
    METHODS process_loop_content
      IMPORTING !tokens       TYPE tokens_type
                !context      TYPE REF TO data
                loop_var      TYPE string
                !collection   TYPE REF TO data
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_llm_template_parser.

    "! <p class="shorttext synchronized">Format table as string</p>
    "! @parameter table                   | <p class="shorttext synchronized">Table to format</p>
    "! @parameter result                  | <p class="shorttext synchronized">Formatted string</p>
    "! @raising   zcx_llm_template_parser | <p class="shorttext synchronized">If formatting fails</p>
    METHODS format_table
      IMPORTING !table        TYPE ANY TABLE
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_llm_template_parser.

    "! <p class="shorttext synchronized">Handle nested for loop tokens</p>
    "! @parameter token             | <p class="shorttext synchronized">Current token</p>
    "! @parameter control_stack     | <p class="shorttext synchronized">Control structure stack</p>
    "! @parameter for_nesting_level | <p class="shorttext synchronized">Current nesting level</p>
    "! @parameter output_buffer     | <p class="shorttext synchronized">Output buffer</p>
    "! @parameter result            | <p class="shorttext synchronized">True if token was handled</p>
    METHODS handle_nested_for_loop
      IMPORTING token             TYPE token_type
      EXPORTING result            TYPE abap_bool
      CHANGING  control_stack     TYPE control_stack_types
                for_nesting_level TYPE i
                output_buffer     TYPE string. "'EC NEEDED

    "! <p class="shorttext synchronized">Handle conditional control structures</p>
    "! @parameter control_content         | <p class="shorttext synchronized">Control structure content</p>
    "! @parameter context                 | <p class="shorttext synchronized">Data context</p>
    "! @parameter control_stack           | <p class="shorttext synchronized">Control structure stack</p>
    "! @raising   zcx_llm_template_parser | <p class="shorttext synchronized">If handling fails</p>
    METHODS handle_conditional
      IMPORTING control_content TYPE string
                !context        TYPE REF TO data
      CHANGING  control_stack   TYPE control_stack_types
      RAISING   zcx_llm_template_parser.

    "! <p class="shorttext synchronized">Handle endif control structure</p>
    "! @parameter control_stack           | <p class="shorttext synchronized">Control structure stack</p>
    "! @raising   zcx_llm_template_parser | <p class="shorttext synchronized">If handling fails</p>
    METHODS handle_endif
      CHANGING control_stack TYPE control_stack_types
      RAISING  zcx_llm_template_parser.

    "! <p class="shorttext synchronized">Handle for loop control structure</p>
    "! @parameter control_content         | <p class="shorttext synchronized">Loop content</p>
    "! @parameter context                 | <p class="shorttext synchronized">Data context</p>
    "! @parameter control_stack           | <p class="shorttext synchronized">Control structure stack</p>
    "! @raising   zcx_llm_template_parser | <p class="shorttext synchronized">If handling fails</p>
    METHODS handle_for_loop
      IMPORTING control_content TYPE string
                !context        TYPE REF TO data
      CHANGING  control_stack   TYPE control_stack_types
      RAISING   zcx_llm_template_parser.

    "! <p class="shorttext synchronized">Handle endfor control structure</p>
    "! @parameter context                 | <p class="shorttext synchronized">Data context</p>
    "! @parameter control_stack           | <p class="shorttext synchronized">Control structure stack</p>
    "! @parameter output_buffer           | <p class="shorttext synchronized">Output buffer to append to</p>
    "! @raising   zcx_llm_template_parser | <p class="shorttext synchronized">If handling fails</p>
    METHODS handle_endfor
      IMPORTING !context      TYPE REF TO data
      CHANGING  control_stack TYPE control_stack_types
                output_buffer TYPE string
      RAISING   zcx_llm_template_parser.

    "! <p class="shorttext synchronized">Process individual token</p>
    "! @parameter token                   | <p class="shorttext synchronized">Token to process</p>
    "! @parameter context                 | <p class="shorttext synchronized">Data context</p>
    "! @parameter control_stack           | <p class="shorttext synchronized">Control structure stack</p>
    "! @parameter output_buffer           | <p class="shorttext synchronized">Output buffer to append to</p>
    "! @raising   zcx_llm_template_parser | <p class="shorttext synchronized">If processing fails</p>
    METHODS process_token
      IMPORTING token         TYPE token_type
                !context      TYPE REF TO data
                control_stack TYPE control_stack_types
      CHANGING  output_buffer TYPE string
      RAISING   zcx_llm_template_parser.

    "! <p class="shorttext synchronized">Check if all conditions in control stack are met</p>
    "! @parameter control_stack | <p class="shorttext synchronized">Control structure stack</p>
    "! @parameter result        | <p class="shorttext synchronized">True if all conditions are met</p>
    METHODS check_control_stack_conditions
      IMPORTING control_stack TYPE control_stack_types
      RETURNING VALUE(result) TYPE abap_bool.
ENDCLASS.

CLASS zcl_llm_template_parser IMPLEMENTATION.
  METHOD constructor.
    templates = VALUE #( ).
  ENDMETHOD.

  METHOD add_template.
    " Check if we already have this template, if yes replace it
    ASSIGN templates[ name = name ] TO FIELD-SYMBOL(<template>).
    IF sy-subrc = 0 AND replace = abap_true.
      <template>-content = content.
      " When replacing the content we need to clear the cache
      CLEAR <template>-tokens.
    ELSE.
      INSERT VALUE #( name    = name
                      content = content ) INTO TABLE templates.
    ENDIF.
  ENDMETHOD.

  METHOD render.
    DATA(template) = get_template_by_name( template_name ).
    FIELD-SYMBOLS <template> TYPE template_type.
    ASSIGN template->* TO <template>.
    IF lines( <template>-tokens ) = 0.
      <template>-tokens = tokenize( <template>-content ).
    ENDIF.
    result = parse_tokens( tokens  = <template>-tokens
                           context = context ).
  ENDMETHOD.

  METHOD tokenize.
    CONSTANTS variable_start TYPE string VALUE '{{'.
    CONSTANTS comment_regex  TYPE string VALUE '\{#[^}]*#\}'.

    " Remove all comments in one step
    DATA(template_content) = template.
    REPLACE ALL OCCURRENCES OF REGEX comment_regex
            IN template_content WITH ``.

    " Initialize positions
    DATA(current_pos) = 0.
    DATA(content_length) = strlen( template_content ).

    WHILE current_pos < content_length.
      DATA(remaining) = content_length - current_pos.
      DATA(current_chunk) = template_content+current_pos(remaining).

      " Look for token start
      FIND FIRST OCCURRENCE OF REGEX `\{\{|\{%`
           IN current_chunk
           MATCH OFFSET DATA(token_start).

      IF sy-subrc <> 0.
        " No more tokens, add remaining text
        IF current_chunk IS NOT INITIAL.
          APPEND VALUE #( type    = token_types-text
                          content = current_chunk ) TO tokens.
        ENDIF.
        EXIT.
      ENDIF.

      " Add text before token if exists
      IF token_start > 0.
        DATA(text) = current_chunk(token_start).
        IF text IS NOT INITIAL.
          APPEND VALUE #( type    = token_types-text
                          content = text ) TO tokens.
        ENDIF.
      ENDIF.

      " Determine token type based on opening delimiter
      DATA(token_type) = COND #( WHEN current_chunk+token_start(2) = variable_start
                                 THEN token_types-variable
                                 ELSE token_types-control ).

      " Find matching end token based on type
      DATA(expected_end) = COND #( WHEN token_type = token_types-variable
                                   THEN '\}\}'
                                   ELSE '%\}' ).

      FIND FIRST OCCURRENCE OF REGEX expected_end
           IN current_chunk+token_start
           MATCH OFFSET DATA(token_end_pos)
           MATCH LENGTH DATA(token_end_length).

      " Raise exception for unclosed tokens
      IF sy-subrc <> 0.
        RAISE EXCEPTION NEW zcx_llm_template_parser( textid = zcx_llm_template_parser=>unclosed_token
                                                     msgv1  = COND #( WHEN token_type = token_types-variable
                                                                      THEN 'Variable'
                                                                      ELSE substring( val = current_chunk+token_start
                                                                                      off = 2
                                                                                      len = 20 ) ) ) ##NO_TEXT.
      ENDIF.

      " Extract and process token content
      DATA(token_length) = token_end_pos + token_end_length.
      DATA(token_start_corrected) = token_start + 2.
      DATA(token_end_corrected) = token_end_pos - 2.
      DATA(token_content) = current_chunk+token_start_corrected(token_end_corrected).

      " Control tokens need to be trimmed
      IF token_type = token_types-control.
        token_content = condense( token_content ).
      ENDIF.

      APPEND VALUE #( type    = token_type
                      content = token_content ) TO tokens.

      " Move position after current token
      current_pos = current_pos + token_start + token_length.
    ENDWHILE.

    " Process escape sequences in text tokens
    LOOP AT tokens ASSIGNING FIELD-SYMBOL(<token>) WHERE type = token_types-text.
      REPLACE ALL OCCURRENCES OF
        '\n'  IN <token>-content WITH cl_abap_char_utilities=>newline.
      REPLACE ALL OCCURRENCES OF
        '\t'  IN <token>-content WITH cl_abap_char_utilities=>horizontal_tab.
      REPLACE ALL OCCURRENCES OF
        '\{'  IN <token>-content WITH '{'.
      REPLACE ALL OCCURRENCES OF
        '\}'  IN <token>-content WITH '}'.
      REPLACE ALL OCCURRENCES OF
        '\\' IN <token>-content WITH '\'.
    ENDLOOP.
  ENDMETHOD.

  METHOD parse_tokens.
    DATA control_stack     TYPE control_stack_types.
    DATA output_buffer     TYPE string.
    DATA for_nesting_level TYPE i VALUE 0.

    LOOP AT tokens ASSIGNING FIELD-SYMBOL(<token>).
      " Handle nested for loops first
      DATA nested TYPE sap_bool.
      handle_nested_for_loop(    EXPORTING token             = <token>
                                 IMPORTING result = nested
                                 CHANGING  control_stack     = control_stack
                                           for_nesting_level = for_nesting_level
                                           output_buffer     = output_buffer ).
      IF nested = abap_true.
        CONTINUE.
      ENDIF.

      " Process different token types
      CASE <token>-type.
        WHEN token_types-control.
          DATA(control_content) = condense( <token>-content ).

          " Handle different control structures
          CASE control_content.
            WHEN 'endif'.
              handle_endif( CHANGING control_stack = control_stack ).

            WHEN 'endfor'.
              handle_endfor( EXPORTING context       = context
                             CHANGING  control_stack = control_stack
                                       output_buffer = output_buffer ).

            WHEN OTHERS.
              " Handle if/elif/else conditions
              IF    control_content CP 'if *'
                 OR control_content CP 'elif *'
                 OR control_content  = 'else'.

                handle_conditional( EXPORTING control_content = control_content
                                              context         = context
                                    CHANGING  control_stack   = control_stack ).

                " Handle for loops
              ELSEIF control_content CP 'for *' ##NO_TEXT.
                handle_for_loop( EXPORTING control_content = control_content
                                           context         = context
                                 CHANGING  control_stack   = control_stack ).
              ENDIF.

          ENDCASE.

        WHEN OTHERS.
          " Process text and variable tokens
          process_token( EXPORTING token         = <token>
                                   context       = context
                                   control_stack = control_stack
                         CHANGING  output_buffer = output_buffer ).

      ENDCASE.
    ENDLOOP.

    " Check for unclosed control structures
    IF lines( control_stack ) > 0.
      DATA(unclosed_control) = control_stack[ 1 ].
      RAISE EXCEPTION NEW zcx_llm_template_parser( textid = zcx_llm_template_parser=>unclosed_token
                                                   msgv1  = CONV #( unclosed_control-type ) ).
    ENDIF.

    result = output_buffer.
  ENDMETHOD.

  METHOD resolve_variable.
    DATA path_segments TYPE string_table.
    DATA current_ref   TYPE REF TO data.
    DATA filter_name   TYPE string.
    DATA filter_param  TYPE string.

    " Split variable path and filter if present
    SPLIT variable_path AT '|' INTO TABLE DATA(parts).
    DATA(var_path) = condense( parts[ 1 ] ).

    IF lines( parts ) > 1.
      DATA(filter_part) = condense( parts[ 2 ] ).
      FIND FIRST OCCURRENCE OF REGEX '(\w+)(?:\((.*)\))?'
           IN filter_part
           SUBMATCHES filter_name filter_param ##SUBRC_OK.
    ENDIF.

    " Split path into segments
    SPLIT var_path AT '.' INTO TABLE path_segments.

    " Check if first segment is current loop variable
    IF     lines( control_stack ) > 0
       AND control_stack[ lines( control_stack ) ]-type = 'FOR'
       AND path_segments[ 1 ] = control_stack[ lines( control_stack ) ]-loop_var.
      " We're accessing a loop variable property
      " Remove the loop variable name from path segments
      DELETE path_segments INDEX 1.

      " Get current item from the loop
      DATA loop_ref TYPE REF TO data.
      loop_ref = control_stack[ lines( control_stack ) ]-collection.
      FIELD-SYMBOLS <loop_table> TYPE STANDARD TABLE.
      ASSIGN loop_ref->* TO <loop_table>.

      ASSIGN <loop_table>[ control_stack[ lines( control_stack ) ]-loop_index ] TO FIELD-SYMBOL(<current_item>).

      " Create reference to current item
      CREATE DATA current_ref LIKE <current_item>.
      ASSIGN current_ref->* TO FIELD-SYMBOL(<new_value>).
      <new_value> = <current_item>.
    ELSE.
      " Start with the context
      current_ref = context.
    ENDIF.

    " Navigate through the path
    LOOP AT path_segments INTO DATA(segment).
      " Handle array access notation (e.g., table[1])
      DATA component TYPE string.
      DATA idx       TYPE string.

      FIND FIRST OCCURRENCE OF REGEX '(\w+)(?:\[(\d+)\])?'
           IN segment
           SUBMATCHES component idx ##SUBRC_OK.

      TRY.
          DATA(descr) = cl_abap_typedescr=>describe_by_data_ref( current_ref ).

          CASE descr->kind.
            WHEN cl_abap_typedescr=>kind_struct.
              FIELD-SYMBOLS <struct> TYPE any.
              ASSIGN current_ref->* TO <struct>.

              ASSIGN COMPONENT component OF STRUCTURE <struct> TO FIELD-SYMBOL(<component>).
              IF sy-subrc <> 0.
                RAISE EXCEPTION NEW zcx_llm_template_parser( textid = zcx_llm_template_parser=>invalid_variable_path
                                                             msgv1  = CONV #( component ) ).
              ENDIF.

              CREATE DATA current_ref LIKE <component>.
              ASSIGN current_ref->* TO <new_value>.
              <new_value> = <component>.

              " If we have an array index, process it immediately after getting the component
              IF idx IS NOT INITIAL.
                DATA(table_idx) = CONV i( idx ).
                FIELD-SYMBOLS <idx_table> TYPE STANDARD TABLE.
                ASSIGN current_ref->* TO <idx_table>.

                IF table_idx <= 0 OR table_idx > lines( <idx_table> ).
                  RAISE EXCEPTION NEW zcx_llm_template_parser( textid = zcx_llm_template_parser=>invalid_table_index ).
                ENDIF.

                ASSIGN <idx_table>[ table_idx ] TO FIELD-SYMBOL(<table_entry>).
                IF sy-subrc <> 0.
                  RAISE EXCEPTION NEW zcx_llm_template_parser( textid = zcx_llm_template_parser=>invalid_table_index ).
                ENDIF.

                CREATE DATA current_ref LIKE <table_entry>.
                ASSIGN current_ref->* TO <new_value>.
                <new_value> = <table_entry>.
              ENDIF.

            WHEN cl_abap_typedescr=>kind_table.

              FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
              ASSIGN current_ref->* TO <table>.

              IF idx IS NOT INITIAL.
                table_idx = CONV i( idx ).

                IF table_idx <= 0 OR table_idx > lines( <table> ).
                  RAISE EXCEPTION NEW zcx_llm_template_parser( textid = zcx_llm_template_parser=>invalid_table_index ).
                ENDIF.

                ASSIGN <table>[ table_idx ] TO <table_entry>.
                IF sy-subrc <> 0.
                  RAISE EXCEPTION NEW zcx_llm_template_parser( textid = zcx_llm_template_parser=>invalid_table_index ).
                ENDIF.

                CREATE DATA current_ref LIKE <table_entry>.
                ASSIGN current_ref->* TO <new_value>.
                <new_value> = <table_entry>.
              ELSE.
                " If this is the final segment and no index specified, format table
                IF sy-tabix = lines( path_segments ).
                  result = format_table( <table> ).
                  RETURN.
                ELSE.
                  RAISE EXCEPTION NEW zcx_llm_template_parser( textid = zcx_llm_template_parser=>invalid_variable_path
                                                               msgv1  = 'Cannot access table without index in path' ) ##NO_TEXT.
                ENDIF.
              ENDIF.

            WHEN OTHERS.
              RAISE EXCEPTION NEW zcx_llm_template_parser(
                                      textid = zcx_llm_template_parser=>unsupported_variable_type ).
          ENDCASE.

        CATCH cx_root INTO DATA(ex).
          RAISE EXCEPTION NEW zcx_llm_template_parser( textid   = zcx_llm_template_parser=>variable_resolution_error
                                                       previous = ex ).
      ENDTRY.
    ENDLOOP.

    " Convert final value to string
    ASSIGN current_ref->* TO FIELD-SYMBOL(<final_value>).
    IF <final_value> IS ASSIGNED.
      DATA(final_descr) = cl_abap_typedescr=>describe_by_data( <final_value> ).

      CASE final_descr->kind.
        WHEN cl_abap_typedescr=>kind_table.
          FIELD-SYMBOLS <final_table> TYPE ANY TABLE.
          ASSIGN <final_value> TO <final_table>.
          result = format_table( <final_table> ).

        WHEN OTHERS.
          TRY.
              CASE final_descr->type_kind.
                WHEN cl_abap_typedescr=>typekind_char.
                  IF    final_descr->absolute_name = '\TYPE=ABAP_BOOL'
                     OR (     final_descr->length = 1
                          AND ( <final_value> = abap_true OR <final_value> = abap_false ) ).
                    result = COND #( WHEN <final_value> = 'X'
                                     THEN 'true'
                                     ELSE 'false' ).
                  ELSE.
                    result = |{ <final_value> }|.
                  ENDIF.
                WHEN cl_abap_typedescr=>typekind_date.
                  DATA(date) = CONV d( <final_value> ).
                  result = |{ date DATE = USER }|.
                WHEN cl_abap_typedescr=>typekind_time.
                  DATA(time) = CONV t( <final_value> ).
                  result = |{ time TIME = USER }|.
                WHEN OTHERS.
                  result = |{ <final_value> }|.
              ENDCASE.
            CATCH cx_sy_conversion_error INTO DATA(exception).
              RAISE EXCEPTION NEW zcx_llm_template_parser( textid   = zcx_llm_template_parser=>variable_resolution_error
                                                           previous = exception ).
          ENDTRY.
      ENDCASE.
    ELSE.
      result = ''.
    ENDIF.

    " Apply filter if present
    IF filter_name IS NOT INITIAL.
      result = apply_filter( value  = result
                             filter = filter_name
                             param  = filter_param ).
    ENDIF.
  ENDMETHOD.

  METHOD format_table.
    DATA result_table TYPE string_table.

    DATA(line_type) = CAST cl_abap_tabledescr(
        cl_abap_typedescr=>describe_by_data( table ) )->get_table_line_type( ).

    CASE line_type->kind.
      WHEN cl_abap_typedescr=>kind_elem.
        " For elementary types (string, integer etc), join with comma
        LOOP AT table ASSIGNING FIELD-SYMBOL(<line>).
          DATA(elem_str) = |{ <line> }|.  " Convert to string first
          APPEND elem_str TO result_table.
        ENDLOOP.
        result = concat_lines_of( table = result_table sep = `, ` ).

      WHEN cl_abap_typedescr=>kind_struct.
        " For structures, create a list of key-value pairs
        LOOP AT table ASSIGNING <line>.
          DATA(struct_descr) = CAST cl_abap_structdescr( line_type ).
          DATA(components) = struct_descr->get_components( ).
          DATA line_result TYPE string.

          LOOP AT components INTO DATA(component).
            ASSIGN COMPONENT component-name OF STRUCTURE <line> TO FIELD-SYMBOL(<field>).
            IF sy-subrc <> 0.
              CONTINUE.
            ENDIF.

            DATA field_str TYPE string.

            " Handle different component types
            CASE component-type->kind.
              WHEN cl_abap_typedescr=>kind_table.
                field_str = '[NESTED TABLE]'.
              WHEN cl_abap_typedescr=>kind_elem.
                IF component-type->absolute_name = '\TYPE=ABAP_BOOL'.
                  field_str = COND #( WHEN <field> = abap_true THEN 'X' ELSE ` ` ).
                ELSE.
                  field_str = |{ <field> }|.
                ENDIF.
              WHEN OTHERS.
                field_str = '[COMPLEX TYPE]'.
            ENDCASE.

            line_result = |{ line_result }{ to_upper( component-name ) }: { field_str }, |.
          ENDLOOP.

          " Remove trailing comma and space
          IF strlen( line_result ) >= 2.
            line_result = substring( val = line_result
                                     len = strlen( line_result ) - 2 ).
          ENDIF.

          APPEND line_result TO result_table.
        ENDLOOP.

        DATA(joined_results) = concat_lines_of( table = result_table
                                                sep   = `; ` ).
        result = |[{ joined_results }]|.

      WHEN cl_abap_typedescr=>kind_table.
        " For nested tables, return placeholder
        result = '[NESTED TABLE]'.

      WHEN OTHERS.
        result = '[COMPLEX TABLE]'.
    ENDCASE.
  ENDMETHOD.

  METHOD get_template_by_name.
    TRY.
        content = REF #( templates[ name = name ] ).
      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION NEW zcx_llm_template_parser(
            textid = zcx_llm_template_parser=>zcx_llm_template_parser ).
    ENDTRY.
  ENDMETHOD.

  METHOD apply_filter.
    CASE to_upper( filter ).
      WHEN 'UPPER'.
        result = to_upper( value ).

      WHEN 'LOWER'.
        result = to_lower( value ).

      WHEN 'CAPITALIZE'.
        IF strlen( value ) > 0.
          result = to_upper( value(1) ) && to_lower( substring( val = value
                                                                off = 1 ) ).
        ENDIF.

      WHEN 'DEFAULT'.
        IF value IS INITIAL AND param IS NOT INITIAL.
          " Remove quotes if present
          result = COND #( WHEN param CS '"' OR param CS ''''
                           THEN substring( val = param
                                           off = 1
                                           len = strlen( param ) - 2 )
                           ELSE param ).
        ELSE.
          result = value.
        ENDIF.

      WHEN OTHERS.
        RAISE EXCEPTION NEW zcx_llm_template_parser( textid = zcx_llm_template_parser=>unknown_filter ).
    ENDCASE.
  ENDMETHOD.

  METHOD evaluate_condition_true.
    " Split condition into parts
    DATA operator     TYPE string.
    DATA left_value   TYPE string.
    DATA right_value  TYPE string.
    DATA left_result  TYPE string.
    DATA right_result TYPE string.

    " Handle NOT operator
    DATA(condition_text) = condense( condition ).
    DATA(is_negated) = xsdbool( strlen( condition_text ) >= 4 AND substring( val = condition_text
                                                                             len = 4 ) = `not ` ).
    IF is_negated = abap_true.
      condition_text = condense( substring( val = condition_text
                                            off = 4 ) ).
    ENDIF.

    " Check for logical operators
    IF condition_text CS ' and '.
      SPLIT condition_text AT ' and ' INTO TABLE DATA(and_conditions).
      result = abap_true.
      LOOP AT and_conditions INTO DATA(sub_condition).
        sub_condition = condense( sub_condition ).
        IF evaluate_condition_true( condition = sub_condition
                                    context   = context ) = abap_false.
          result = abap_false.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF is_negated = abap_true.
        result = xsdbool( result = abap_false ).
      ENDIF.
      RETURN.
    ENDIF.

    IF condition_text CS ' or '.
      SPLIT condition_text AT ' or ' INTO TABLE DATA(or_conditions).
      result = abap_false.
      LOOP AT or_conditions INTO DATA(or_sub_condition).
        or_sub_condition = condense( or_sub_condition ).
        IF evaluate_condition_true( condition = or_sub_condition
                                    context   = context ) = abap_true.
          result = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF is_negated = abap_true.
        result = xsdbool( result = abap_false ).
      ENDIF.
      RETURN.
    ENDIF.

    " Handle comparison operators
    FIND FIRST OCCURRENCE OF REGEX '([^<>=!]+)\s*(==|!=|>=|<=|>|<)\s*([^<>=!]+)'
         IN condition_text
         SUBMATCHES left_value operator right_value.

    IF sy-subrc = 0.
      " Clean up values
      left_value = condense( left_value ).
      right_value = condense( right_value ).

      " Handle left value
      TRY.
          IF left_value CS '"' OR left_value CS ''''.
            left_result = substring( val = left_value
                                     off = 1
                                     len = strlen( left_value ) - 2 ).
          ELSE.
            left_result = resolve_variable( variable_path = left_value
                                            context       = context ).
          ENDIF.
        CATCH zcx_llm_template_parser.
          left_result = left_value.
      ENDTRY.

      " Handle right value
      TRY.
          IF right_value CS '"' OR right_value CS ''''.
            right_result = substring( val = right_value
                                      off = 1
                                      len = strlen( right_value ) - 2 ).
          ELSE.
            right_result = resolve_variable( variable_path = right_value
                                             context       = context ).
          ENDIF.
        CATCH zcx_llm_template_parser.
          right_result = right_value.
      ENDTRY.

      " Try numeric comparison
      TRY.
          DATA left_num  TYPE i.
          DATA right_num TYPE i.

          " Check if both values can be converted to numbers
          left_num = CONV i( left_result ).
          right_num = CONV i( right_result ).

          " Perform numeric comparison
          CASE operator.
            WHEN '=='.
              result = xsdbool( left_num = right_num ).
            WHEN '!='.
              result = xsdbool( left_num <> right_num ).
            WHEN '>'.
              result = xsdbool( left_num > right_num ).
            WHEN '<'.
              result = xsdbool( left_num < right_num ).
            WHEN '>='.
              result = xsdbool( left_num >= right_num ).
            WHEN '<='.
              result = xsdbool( left_num <= right_num ).
          ENDCASE.
        CATCH cx_root.
          " If numeric comparison fails, do string comparison
          CASE operator.
            WHEN '=='.
              result = xsdbool( left_result = right_result ).
            WHEN '!='.
              result = xsdbool( left_result <> right_result ).
            WHEN '>'.
              result = xsdbool( left_result > right_result ).
            WHEN '<'.
              result = xsdbool( left_result < right_result ).
            WHEN '>='.
              result = xsdbool( left_result >= right_result ).
            WHEN '<='.
              result = xsdbool( left_result <= right_result ).
          ENDCASE.
      ENDTRY.

    ELSE.
      " Handle boolean value
      TRY.
          left_result = resolve_variable( variable_path = condition_text
                                          context       = context ).
          result = xsdbool(    left_result = 'true'
                            OR left_result = '1'
                            OR left_result = 'X'
                            OR left_result = 'x' ).
        CATCH zcx_llm_template_parser.
          result = xsdbool(    condition_text = 'true'
                            OR condition_text = '1'
                            OR condition_text = 'X'
                            OR condition_text = 'x' ).
      ENDTRY.
    ENDIF.

    IF is_negated = abap_true.
      result = xsdbool( result = abap_false ).
    ENDIF.
  ENDMETHOD.

  METHOD resolve_variable_ref.
    DATA path_segments TYPE string_table.
    DATA current_ref   TYPE REF TO data.

    IF variable_path IS INITIAL OR context IS INITIAL.
      RAISE EXCEPTION NEW zcx_llm_template_parser( textid = zcx_llm_template_parser=>invalid_variable_path ).
    ENDIF.

    " Split path into segments
    SPLIT variable_path AT '.' INTO TABLE path_segments.

    " Start with the context
    current_ref = context.
    ASSIGN current_ref->* TO FIELD-SYMBOL(<current_data>).

    " Navigate through the path
    LOOP AT path_segments INTO DATA(segment).
      TRY.
          DATA(descr) = cl_abap_typedescr=>describe_by_data( <current_data> ).

          IF descr->kind = cl_abap_typedescr=>kind_struct.
            " Handle structure components
            ASSIGN COMPONENT segment OF STRUCTURE <current_data>
                   TO FIELD-SYMBOL(<component>).
            IF sy-subrc <> 0.
              RAISE EXCEPTION NEW zcx_llm_template_parser( textid = zcx_llm_template_parser=>invalid_variable_path ).
            ENDIF.

            CREATE DATA current_ref LIKE <component>.
            ASSIGN current_ref->* TO <current_data>.
            <current_data> = <component>.
          ELSE.
            RAISE EXCEPTION NEW zcx_llm_template_parser( textid = zcx_llm_template_parser=>unsupported_variable_type ).
          ENDIF.

        CATCH cx_root INTO DATA(ex).
          RAISE EXCEPTION NEW zcx_llm_template_parser( textid   = zcx_llm_template_parser=>variable_resolution_error
                                                       previous = ex ).
      ENDTRY.
    ENDLOOP.

    result = current_ref.
  ENDMETHOD.


  METHOD process_loop_content.
    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.

    ASSIGN collection->* TO <table>.
    DATA(total_items) = lines( <table> ).

    " Get the structure of the original context
    DATA(orig_type) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data_ref( context ) ).
    DATA(components) = orig_type->get_components( ).

    " Get table type and line type
    DATA(table_descr) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data_ref( collection ) ).
    DATA(line_type) = table_descr->get_table_line_type( ).

    " Add the loop variable component - first check if it exists
    IF NOT line_exists( components[ name = loop_var ] ).
      DATA component TYPE abap_componentdescr.
      component-name  = loop_var.
      component-type ?= line_type.
      INSERT component INTO TABLE components.
    ENDIF.

    " Add loop metadata component only if it doesn't exist
    IF NOT line_exists( components[ name = 'LOOP' ] ).
      component-name  = 'LOOP'.
      component-type ?= CAST cl_abap_typedescr(
        cl_abap_structdescr=>describe_by_data( VALUE loop_meta_type( ) ) ).
      INSERT component INTO TABLE components.
    ENDIF.

    " Create new structure type
    DATA(new_struct_type) = cl_abap_structdescr=>create( components ).

    DO total_items TIMES.
      DATA(current_index) = sy-index.

      " Create new context for this iteration
      DATA new_context TYPE REF TO data.
      CREATE DATA new_context TYPE HANDLE new_struct_type.
      ASSIGN new_context->* TO FIELD-SYMBOL(<new_ctx>).

      " Copy original context data
      ASSIGN context->* TO FIELD-SYMBOL(<ctx_data>).
      <new_ctx> = CORRESPONDING #( <ctx_data> ).

      " Set loop variable
      ASSIGN <table>[ current_index ] TO FIELD-SYMBOL(<current_item>).
      ASSIGN COMPONENT loop_var OF STRUCTURE <new_ctx> TO FIELD-SYMBOL(<loop_var>).
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
      <loop_var> = <current_item>.

      " Set loop metadata
      ASSIGN COMPONENT 'LOOP' OF STRUCTURE <new_ctx> TO FIELD-SYMBOL(<loop_meta>).
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
      <loop_meta> = VALUE loop_meta_type( index = current_index
                                          first = xsdbool( current_index = 1 )
                                          last  = xsdbool( current_index = total_items ) ).

      " Process tokens with new context
      result = result && parse_tokens( tokens  = tokens
                                       context = new_context ).
    ENDDO.
  ENDMETHOD.

  METHOD handle_nested_for_loop.
    result = abap_false.

    ASSIGN control_stack[ lines( control_stack ) ] TO FIELD-SYMBOL(<current_stack>).
    IF NOT ( sy-subrc = 0 AND <current_stack>-type = 'FOR' ).
      RETURN.
    ENDIF.

    " Check if this is another nested for loop
    IF token-type = token_types-control AND condense( token-content ) CP 'for *'.
      for_nesting_level = for_nesting_level + 1.
    ENDIF.

    " Check if this is an endfor
    IF token-type = token_types-control AND condense( token-content ) = 'endfor'.
      IF for_nesting_level > 0.
        for_nesting_level = for_nesting_level - 1.
        APPEND token TO <current_stack>-loop_tokens.
        result = abap_true.
      ENDIF.
    ELSE.
      " If we're in a nested loop or not at the endfor, collect tokens
      APPEND token TO <current_stack>-loop_tokens.
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD handle_conditional.
    FIELD-SYMBOLS <current_stack> TYPE control_stack_type.
    DATA condition TYPE string.

    IF control_content CP 'if *'.
      " Start new if block
      APPEND INITIAL LINE TO control_stack ASSIGNING <current_stack>.
      <current_stack>-type              = 'IF'.
      <current_stack>-any_condition_met = abap_false.

      TRY.
          condition = substring_after( val = control_content
                                       sub = 'if ' ).
          <current_stack>-condition_met = evaluate_condition_true( condition = condition
                                                                   context   = context ).
          IF <current_stack>-condition_met = abap_true.
            <current_stack>-any_condition_met = abap_true.
          ENDIF.
        CATCH cx_root INTO DATA(exception).
          RAISE EXCEPTION NEW zcx_llm_template_parser( textid   = zcx_llm_template_parser=>condition_evaluation_error
                                                       msgv1    = CONV #( condition )
                                                       previous = exception ).
      ENDTRY.

    ELSEIF control_content CP 'elif *' ##NO_TEXT.
      " Handle elif (else if)
      IF lines( control_stack ) = 0 OR control_stack[ lines( control_stack ) ]-type <> 'IF'.
        RAISE EXCEPTION NEW zcx_llm_template_parser( textid = zcx_llm_template_parser=>unexpected_elif ).
      ENDIF.

      ASSIGN control_stack[ lines( control_stack ) ] TO <current_stack>.
      " Only evaluate elif if no previous condition was met
      IF <current_stack>-any_condition_met = abap_false.
        TRY.
            condition = substring_after( val = control_content
                                         sub = 'elif ' ).
            <current_stack>-condition_met = evaluate_condition_true( condition = condition
                                                                     context   = context ).
            IF <current_stack>-condition_met = abap_true.
              <current_stack>-any_condition_met = abap_true.
            ENDIF.
          CATCH cx_root INTO exception.
            RAISE EXCEPTION NEW zcx_llm_template_parser( textid   = zcx_llm_template_parser=>condition_evaluation_error
                                                         msgv1    = CONV #( condition )
                                                         previous = exception ).
        ENDTRY.
      ELSE.
        <current_stack>-condition_met = abap_false.
      ENDIF.

    ELSEIF control_content = 'else'.
      " Handle else
      IF lines( control_stack ) = 0 OR control_stack[ lines( control_stack ) ]-type <> 'IF'.
        RAISE EXCEPTION NEW zcx_llm_template_parser( textid = zcx_llm_template_parser=>unexpected_else ).
      ENDIF.

      ASSIGN control_stack[ lines( control_stack ) ] TO <current_stack>.
      " Only execute else if no previous condition was met
      <current_stack>-condition_met = xsdbool( <current_stack>-any_condition_met = abap_false ).
    ENDIF.
  ENDMETHOD.

  METHOD handle_endif.
    IF lines( control_stack ) = 0 OR control_stack[ lines( control_stack ) ]-type <> 'IF'.
      RAISE EXCEPTION NEW zcx_llm_template_parser( textid = zcx_llm_template_parser=>unexpected_endif ).
    ENDIF.

    DELETE control_stack INDEX lines( control_stack ).
  ENDMETHOD.

  METHOD handle_for_loop.
    FIELD-SYMBOLS <current_stack> TYPE control_stack_type.

    APPEND INITIAL LINE TO control_stack ASSIGNING <current_stack>.
    <current_stack>-type        = 'FOR'.
    <current_stack>-loop_tokens = VALUE #( ).

    TRY.
        " Parse for loop syntax: "for item in collection"
        DATA(loop_content) = condense( control_content ).
        SPLIT loop_content AT ' ' INTO TABLE DATA(loop_parts).
        IF lines( loop_parts ) <> 4 OR loop_parts[ 3 ] <> 'in'.
          RAISE EXCEPTION NEW zcx_llm_template_parser( textid = zcx_llm_template_parser=>invalid_loop_syntax ).
        ENDIF.

        <current_stack>-loop_var = loop_parts[ 2 ].
        DATA(collection_path) = loop_parts[ 4 ].

        " Clean up collection path
        collection_path = replace( val  = collection_path
                                   sub  = '{{'
                                   with = '' ).
        collection_path = replace( val  = collection_path
                                   sub  = '}}'
                                   with = '' ).
        collection_path = condense( collection_path ).

        <current_stack>-collection = resolve_variable_ref( variable_path = collection_path
                                                           context       = context ).

      CATCH cx_root INTO DATA(exception).
        RAISE EXCEPTION NEW zcx_llm_template_parser( textid   = zcx_llm_template_parser=>loop_initialization_error
                                                     previous = exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD handle_endfor.
    IF lines( control_stack ) = 0 OR control_stack[ lines( control_stack ) ]-type <> 'FOR'.
      RAISE EXCEPTION NEW zcx_llm_template_parser( textid = zcx_llm_template_parser=>unexpected_endfor ).
    ENDIF.

    DATA(loop_stack) = control_stack[ lines( control_stack ) ].
    output_buffer = output_buffer && process_loop_content( tokens     = loop_stack-loop_tokens
                                                           context    = context
                                                           loop_var   = loop_stack-loop_var
                                                           collection = loop_stack-collection ).

    DELETE control_stack INDEX lines( control_stack ).
  ENDMETHOD.

  METHOD process_token.
    CASE token-type.
      WHEN token_types-text.
        IF check_control_stack_conditions( control_stack ) = abap_true.
          output_buffer = output_buffer && token-content.
        ENDIF.

      WHEN token_types-variable.
        IF check_control_stack_conditions( control_stack ) = abap_true.
          TRY.
              output_buffer = output_buffer && resolve_variable( variable_path = token-content
                                                                 context       = context
                                                                 control_stack = control_stack ).
            CATCH zcx_llm_template_parser INTO DATA(exception).
              RAISE EXCEPTION NEW zcx_llm_template_parser( textid   = zcx_llm_template_parser=>variable_resolution_error
                                                           msgv1    = CONV #( token-content )
                                                           previous = exception ).
          ENDTRY.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD check_control_stack_conditions.
    result = abap_true.

    LOOP AT control_stack ASSIGNING FIELD-SYMBOL(<stack_entry>).
      IF <stack_entry>-condition_met = abap_false.
        result = abap_false.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
