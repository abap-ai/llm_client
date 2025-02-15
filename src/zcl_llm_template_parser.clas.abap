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
      tokens_type TYPE STANDARD TABLE OF token_type WITH DEFAULT KEY.

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
    DATA temp1 TYPE zcl_llm_template_parser=>templates_type.
    CLEAR temp1.
    templates = temp1.
  ENDMETHOD.

  METHOD add_template.
    " Check if we already have this template, if yes replace it
    FIELD-SYMBOLS <template> TYPE zcl_llm_template_parser=>template_type.
      DATA temp2 TYPE zcl_llm_template_parser=>template_type.
    READ TABLE templates WITH KEY name = name ASSIGNING <template>.
    IF sy-subrc = 0 AND replace = abap_true.
      <template>-content = content.
      " When replacing the content we need to clear the cache
      CLEAR <template>-tokens.
    ELSE.
      
      CLEAR temp2.
      temp2-name = name.
      temp2-content = content.
      INSERT temp2 INTO TABLE templates.
    ENDIF.
  ENDMETHOD.

  METHOD render.
    DATA template TYPE REF TO zcl_llm_template_parser=>template_type.
    FIELD-SYMBOLS <template> TYPE template_type.
    template = get_template_by_name( template_name ).
    
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
    DATA template_content LIKE template.
    DATA current_pos TYPE i.
    DATA content_length TYPE i.
      DATA remaining TYPE i.
      DATA current_chunk TYPE string.
      DATA token_start TYPE i.
          DATA temp3 TYPE zcl_llm_template_parser=>token_type.
        DATA text TYPE string.
          DATA temp4 TYPE zcl_llm_template_parser=>token_type.
      DATA temp5 TYPE string.
      DATA token_type LIKE temp5.
      DATA expected_end TYPE c LENGTH 4.
      DATA temp1 LIKE expected_end.
      DATA token_end_pos TYPE i.
      DATA token_end_length TYPE i.
        DATA temp7 TYPE symsgv.
        DATA temp2 TYPE REF TO zcx_llm_template_parser.
      DATA token_length TYPE i.
      DATA token_start_corrected TYPE i.
      DATA token_end_corrected TYPE i.
      DATA token_content TYPE string.
      DATA temp8 TYPE zcl_llm_template_parser=>token_type.
    FIELD-SYMBOLS <token> LIKE LINE OF tokens.
    template_content = template.
    REPLACE ALL OCCURRENCES OF REGEX comment_regex
            IN template_content WITH ``.

    " Initialize positions
    
    current_pos = 0.
    
    content_length = strlen( template_content ).

    WHILE current_pos < content_length.
      
      remaining = content_length - current_pos.
      
      current_chunk = template_content+current_pos(remaining).

      " Look for token start
      
      FIND FIRST OCCURRENCE OF REGEX `\{\{|\{%`
           IN current_chunk
           MATCH OFFSET token_start.

      IF sy-subrc <> 0.
        " No more tokens, add remaining text
        IF current_chunk IS NOT INITIAL.
          
          CLEAR temp3.
          temp3-type = token_types-text.
          temp3-content = current_chunk.
          APPEND temp3 TO tokens.
        ENDIF.
        EXIT.
      ENDIF.

      " Add text before token if exists
      IF token_start > 0.
        
        text = current_chunk(token_start).
        IF text IS NOT INITIAL.
          
          CLEAR temp4.
          temp4-type = token_types-text.
          temp4-content = text.
          APPEND temp4 TO tokens.
        ENDIF.
      ENDIF.

      " Determine token type based on opening delimiter
      
      IF current_chunk+token_start(2) = variable_start.
        temp5 = token_types-variable.
      ELSE.
        temp5 = token_types-control.
      ENDIF.
      
      token_type = temp5.

      " Find matching end token based on type
      
      
      IF token_type = token_types-variable.
        temp1 = '\}\}'.
      ELSE.
        temp1 = '%\}'.
      ENDIF.
      expected_end = temp1.

      
      
      FIND FIRST OCCURRENCE OF REGEX expected_end
           IN current_chunk+token_start
           MATCH OFFSET token_end_pos
           MATCH LENGTH token_end_length.

      " Raise exception for unclosed tokens
      IF sy-subrc <> 0.
        
        IF token_type = token_types-variable.
          temp7 = 'Variable'.
        ELSE.
          temp7 = substring( val = current_chunk+token_start off = 2 len = 20 ).
        ENDIF.
        
        CREATE OBJECT temp2 TYPE zcx_llm_template_parser EXPORTING textid = zcx_llm_template_parser=>unclosed_token msgv1 = temp7.
        RAISE EXCEPTION temp2 ##NO_TEXT.
      ENDIF.

      " Extract and process token content
      
      token_length = token_end_pos + token_end_length.
      
      token_start_corrected = token_start + 2.
      
      token_end_corrected = token_end_pos - 2.
      
      token_content = current_chunk+token_start_corrected(token_end_corrected).

      " Control tokens need to be trimmed
      IF token_type = token_types-control.
        token_content = condense( token_content ).
      ENDIF.

      
      CLEAR temp8.
      temp8-type = token_type.
      temp8-content = token_content.
      APPEND temp8 TO tokens.

      " Move position after current token
      current_pos = current_pos + token_start + token_length.
    ENDWHILE.

    " Process escape sequences in text tokens
    
    LOOP AT tokens ASSIGNING <token> WHERE type = token_types-text.
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

    FIELD-SYMBOLS <token> LIKE LINE OF tokens.
      DATA nested TYPE sap_bool.
          DATA control_content TYPE string.
      DATA unclosed_control LIKE LINE OF control_stack.
      DATA temp3 LIKE LINE OF control_stack.
      DATA temp4 LIKE sy-tabix.
      DATA temp9 TYPE symsgv.
      DATA temp5 TYPE REF TO zcx_llm_template_parser.
    LOOP AT tokens ASSIGNING <token>.
      " Handle nested for loops first
      
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
          
          control_content = condense( <token>-content ).

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
      
      
      
      temp4 = sy-tabix.
      READ TABLE control_stack INDEX 1 INTO temp3.
      sy-tabix = temp4.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
      ENDIF.
      unclosed_control = temp3.
      
      temp9 = unclosed_control-type.
      
      CREATE OBJECT temp5 TYPE zcx_llm_template_parser EXPORTING textid = zcx_llm_template_parser=>unclosed_token msgv1 = temp9.
      RAISE EXCEPTION temp5.
    ENDIF.

    result = output_buffer.
  ENDMETHOD.

  METHOD resolve_variable.
    DATA path_segments TYPE string_table.
    DATA current_ref   TYPE REF TO data.
    DATA filter_name   TYPE string.
    DATA filter_param  TYPE string.

    " Split variable path and filter if present
    TYPES temp5 TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA parts TYPE temp5.
    DATA var_path TYPE string.
    DATA temp6 LIKE LINE OF parts.
    DATA temp7 LIKE sy-tabix.
      DATA filter_part TYPE string.
      DATA temp8 LIKE LINE OF parts.
      DATA temp9 LIKE sy-tabix.
    DATA temp10 LIKE LINE OF control_stack.
    DATA temp11 LIKE sy-tabix.
    DATA temp28 LIKE LINE OF path_segments.
    DATA temp29 LIKE sy-tabix.
    DATA temp1 LIKE LINE OF control_stack.
    DATA temp2 LIKE sy-tabix.
      DATA loop_ref TYPE REF TO data.
      DATA temp12 LIKE LINE OF control_stack.
      DATA temp13 LIKE sy-tabix.
      FIELD-SYMBOLS <loop_table> TYPE STANDARD TABLE.
      FIELD-SYMBOLS <current_item> TYPE any.
      DATA temp3 LIKE LINE OF control_stack.
      DATA temp4 LIKE sy-tabix.
      FIELD-SYMBOLS <new_value> TYPE data.
    DATA segment LIKE LINE OF path_segments.
      DATA component TYPE string.
      DATA idx TYPE string.
          DATA descr TYPE REF TO cl_abap_typedescr.
              FIELD-SYMBOLS <struct> TYPE any.
              FIELD-SYMBOLS <component> TYPE any.
                DATA temp14 TYPE symsgv.
                DATA temp30 TYPE REF TO zcx_llm_template_parser.
                DATA temp15 TYPE i.
                DATA table_idx LIKE temp15.
                FIELD-SYMBOLS <idx_table> TYPE STANDARD TABLE.
                  DATA temp16 TYPE REF TO zcx_llm_template_parser.
                FIELD-SYMBOLS <table_entry> TYPE any.
                  DATA temp17 TYPE REF TO zcx_llm_template_parser.
              FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
                DATA temp18 TYPE i.
                  DATA temp19 TYPE REF TO zcx_llm_template_parser.
                  DATA temp20 TYPE REF TO zcx_llm_template_parser.
                  DATA temp21 TYPE REF TO zcx_llm_template_parser.
              DATA temp22 TYPE REF TO zcx_llm_template_parser.
          DATA ex TYPE REF TO cx_root.
          DATA temp23 TYPE REF TO zcx_llm_template_parser.
    FIELD-SYMBOLS <final_value> TYPE data.
      DATA final_descr TYPE REF TO cl_abap_typedescr.
          FIELD-SYMBOLS <final_table> TYPE ANY TABLE.
                    DATA temp24 TYPE string.
                  DATA temp25 TYPE d.
                  DATA date LIKE temp25.
                  DATA temp26 TYPE t.
                  DATA time LIKE temp26.
              DATA exception TYPE REF TO cx_sy_conversion_error.
              DATA temp27 TYPE REF TO zcx_llm_template_parser.
    SPLIT variable_path AT '|' INTO TABLE parts.
    
    
    
    temp7 = sy-tabix.
    READ TABLE parts INDEX 1 INTO temp6.
    sy-tabix = temp7.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    var_path = condense( temp6 ).

    IF lines( parts ) > 1.
      
      
      
      temp9 = sy-tabix.
      READ TABLE parts INDEX 2 INTO temp8.
      sy-tabix = temp9.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
      ENDIF.
      filter_part = condense( temp8 ).
      FIND FIRST OCCURRENCE OF REGEX '(\w+)(?:\((.*)\))?'
           IN filter_part
           SUBMATCHES filter_name filter_param ##SUBRC_OK.
    ENDIF.

    " Split path into segments
    SPLIT var_path AT '.' INTO TABLE path_segments.

    " Check if first segment is current loop variable
    
    
    temp11 = sy-tabix.
    READ TABLE control_stack INDEX lines( control_stack ) INTO temp10.
    sy-tabix = temp11.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    
    
    temp29 = sy-tabix.
    READ TABLE path_segments INDEX 1 INTO temp28.
    sy-tabix = temp29.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    
    
    temp2 = sy-tabix.
    READ TABLE control_stack INDEX lines( control_stack ) INTO temp1.
    sy-tabix = temp2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    IF     lines( control_stack ) > 0
       AND temp10-type = 'FOR'
       AND temp28 = temp1-loop_var.
      " We're accessing a loop variable property
      " Remove the loop variable name from path segments
      DELETE path_segments INDEX 1.

      " Get current item from the loop
      
      
      
      temp13 = sy-tabix.
      READ TABLE control_stack INDEX lines( control_stack ) INTO temp12.
      sy-tabix = temp13.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
      ENDIF.
      loop_ref = temp12-collection.
      
      ASSIGN loop_ref->* TO <loop_table>.

      
      
      
      temp4 = sy-tabix.
      READ TABLE control_stack INDEX lines( control_stack ) INTO temp3.
      sy-tabix = temp4.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
      ENDIF.
      READ TABLE <loop_table> INDEX temp3-loop_index ASSIGNING <current_item>.

      " Create reference to current item
      CREATE DATA current_ref LIKE <current_item>.
      
      ASSIGN current_ref->* TO <new_value>.
      <new_value> = <current_item>.
    ELSE.
      " Start with the context
      current_ref = context.
    ENDIF.

    " Navigate through the path
    
    LOOP AT path_segments INTO segment.
      " Handle array access notation (e.g., table[1])
      
      

      FIND FIRST OCCURRENCE OF REGEX '(\w+)(?:\[(\d+)\])?'
           IN segment
           SUBMATCHES component idx ##SUBRC_OK.

      TRY.
          
          descr = cl_abap_typedescr=>describe_by_data_ref( current_ref ).

          CASE descr->kind.
            WHEN cl_abap_typedescr=>kind_struct.
              
              ASSIGN current_ref->* TO <struct>.

              
              ASSIGN COMPONENT component OF STRUCTURE <struct> TO <component>.
              IF sy-subrc <> 0.
                
                temp14 = component.
                
                CREATE OBJECT temp30 TYPE zcx_llm_template_parser EXPORTING textid = zcx_llm_template_parser=>invalid_variable_path msgv1 = temp14.
                RAISE EXCEPTION temp30.
              ENDIF.

              CREATE DATA current_ref LIKE <component>.
              ASSIGN current_ref->* TO <new_value>.
              <new_value> = <component>.

              " If we have an array index, process it immediately after getting the component
              IF idx IS NOT INITIAL.
                
                temp15 = idx.
                
                table_idx = temp15.
                
                ASSIGN current_ref->* TO <idx_table>.

                IF table_idx <= 0 OR table_idx > lines( <idx_table> ).
                  
                  CREATE OBJECT temp16 TYPE zcx_llm_template_parser EXPORTING textid = zcx_llm_template_parser=>invalid_table_index.
                  RAISE EXCEPTION temp16.
                ENDIF.

                
                READ TABLE <idx_table> INDEX table_idx ASSIGNING <table_entry>.
                IF sy-subrc <> 0.
                  
                  CREATE OBJECT temp17 TYPE zcx_llm_template_parser EXPORTING textid = zcx_llm_template_parser=>invalid_table_index.
                  RAISE EXCEPTION temp17.
                ENDIF.

                CREATE DATA current_ref LIKE <table_entry>.
                ASSIGN current_ref->* TO <new_value>.
                <new_value> = <table_entry>.
              ENDIF.

            WHEN cl_abap_typedescr=>kind_table.

              
              ASSIGN current_ref->* TO <table>.

              IF idx IS NOT INITIAL.
                
                temp18 = idx.
                table_idx = temp18.

                IF table_idx <= 0 OR table_idx > lines( <table> ).
                  
                  CREATE OBJECT temp19 TYPE zcx_llm_template_parser EXPORTING textid = zcx_llm_template_parser=>invalid_table_index.
                  RAISE EXCEPTION temp19.
                ENDIF.

                READ TABLE <table> INDEX table_idx ASSIGNING <table_entry>.
                IF sy-subrc <> 0.
                  
                  CREATE OBJECT temp20 TYPE zcx_llm_template_parser EXPORTING textid = zcx_llm_template_parser=>invalid_table_index.
                  RAISE EXCEPTION temp20.
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
                  
                  CREATE OBJECT temp21 TYPE zcx_llm_template_parser EXPORTING textid = zcx_llm_template_parser=>invalid_variable_path msgv1 = 'Cannot access table without index in path'.
                  RAISE EXCEPTION temp21 ##NO_TEXT.
                ENDIF.
              ENDIF.

            WHEN OTHERS.
              
              CREATE OBJECT temp22 TYPE zcx_llm_template_parser EXPORTING textid = zcx_llm_template_parser=>unsupported_variable_type.
              RAISE EXCEPTION temp22.
          ENDCASE.

          
        CATCH cx_root INTO ex.
          
          CREATE OBJECT temp23 TYPE zcx_llm_template_parser EXPORTING textid = zcx_llm_template_parser=>variable_resolution_error previous = ex.
          RAISE EXCEPTION temp23.
      ENDTRY.
    ENDLOOP.

    " Convert final value to string
    
    ASSIGN current_ref->* TO <final_value>.
    IF <final_value> IS ASSIGNED.
      
      final_descr = cl_abap_typedescr=>describe_by_data( <final_value> ).

      CASE final_descr->kind.
        WHEN cl_abap_typedescr=>kind_table.
          
          ASSIGN <final_value> TO <final_table>.
          result = format_table( <final_table> ).

        WHEN OTHERS.
          TRY.
              CASE final_descr->type_kind.
                WHEN cl_abap_typedescr=>typekind_char.
                  IF    final_descr->absolute_name = '\TYPE=ABAP_BOOL'
                     OR (     final_descr->length = 1
                          AND ( <final_value> = abap_true OR <final_value> = abap_false ) ).
                    
                    IF <final_value> = 'X'.
                      temp24 = 'true'.
                    ELSE.
                      temp24 = 'false'.
                    ENDIF.
                    result = temp24.
                  ELSE.
                    result = |{ <final_value> }|.
                  ENDIF.
                WHEN cl_abap_typedescr=>typekind_date.
                  
                  temp25 = <final_value>.
                  
                  date = temp25.
                  result = |{ date DATE = USER }|.
                WHEN cl_abap_typedescr=>typekind_time.
                  
                  temp26 = <final_value>.
                  
                  time = temp26.
                  result = |{ time TIME = USER }|.
                WHEN OTHERS.
                  result = |{ <final_value> }|.
              ENDCASE.
              
            CATCH cx_sy_conversion_error INTO exception.
              
              CREATE OBJECT temp27 TYPE zcx_llm_template_parser EXPORTING textid = zcx_llm_template_parser=>variable_resolution_error previous = exception.
              RAISE EXCEPTION temp27.
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

    DATA temp28 TYPE REF TO cl_abap_tabledescr.
    DATA line_type TYPE REF TO cl_abap_datadescr.
        FIELD-SYMBOLS <line> TYPE ANY.
          DATA elem_str TYPE string.
          DATA temp29 TYPE REF TO cl_abap_structdescr.
          DATA struct_descr LIKE temp29.
          DATA components TYPE abap_component_tab.
          DATA line_result TYPE string.
          DATA component LIKE LINE OF components.
            FIELD-SYMBOLS <field> TYPE any.
            DATA field_str TYPE string.
                  DATA temp30 TYPE string.
        DATA joined_results TYPE string.
    temp28 ?= cl_abap_typedescr=>describe_by_data( table ).
    
    line_type = temp28->get_table_line_type( ).

    CASE line_type->kind.
      WHEN cl_abap_typedescr=>kind_elem.
        " For elementary types (string, integer etc), join with comma
        
        LOOP AT table ASSIGNING <line>.
          
          elem_str = |{ <line> }|.  " Convert to string first
          APPEND elem_str TO result_table.
        ENDLOOP.
        result = concat_lines_of( table = result_table sep = `, ` ).

      WHEN cl_abap_typedescr=>kind_struct.
        " For structures, create a list of key-value pairs
        LOOP AT table ASSIGNING <line>.
          
          temp29 ?= line_type.
          
          struct_descr = temp29.
          
          components = struct_descr->get_components( ).
          

          
          LOOP AT components INTO component.
            
            ASSIGN COMPONENT component-name OF STRUCTURE <line> TO <field>.
            IF sy-subrc <> 0.
              CONTINUE.
            ENDIF.

            

            " Handle different component types
            CASE component-type->kind.
              WHEN cl_abap_typedescr=>kind_table.
                field_str = '[NESTED TABLE]'.
              WHEN cl_abap_typedescr=>kind_elem.
                IF component-type->absolute_name = '\TYPE=ABAP_BOOL'.
                  
                  IF <field> = abap_true.
                    temp30 = 'X'.
                  ELSE.
                    temp30 = ` `.
                  ENDIF.
                  field_str = temp30.
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

        
        joined_results = concat_lines_of( table = result_table
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
        FIELD-SYMBOLS <temp31> TYPE zcl_llm_template_parser=>template_type.
        DATA temp32 TYPE REF TO zcx_llm_template_parser.
    TRY.
        
        READ TABLE templates WITH KEY name = name ASSIGNING <temp31>.
IF sy-subrc <> 0.
  RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
ENDIF.
GET REFERENCE OF <temp31> INTO content.
      CATCH cx_sy_itab_line_not_found.
        
        CREATE OBJECT temp32 TYPE zcx_llm_template_parser EXPORTING textid = zcx_llm_template_parser=>zcx_llm_template_parser.
        RAISE EXCEPTION temp32.
    ENDTRY.
  ENDMETHOD.

  METHOD apply_filter.
          DATA temp33 TYPE string.
        DATA temp34 TYPE REF TO zcx_llm_template_parser.
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
          
          IF param CS '"' OR param CS ''''.
            temp33 = substring( val = param off = 1 len = strlen( param ) - 2 ).
          ELSE.
            temp33 = param.
          ENDIF.
          result = temp33.
        ELSE.
          result = value.
        ENDIF.

      WHEN OTHERS.
        
        CREATE OBJECT temp34 TYPE zcx_llm_template_parser EXPORTING textid = zcx_llm_template_parser=>unknown_filter.
        RAISE EXCEPTION temp34.
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
    DATA condition_text TYPE string.
    DATA is_negated TYPE abap_bool.
    DATA temp1 TYPE xsdboolean.
      TYPES temp6 TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA and_conditions TYPE temp6.
      DATA sub_condition LIKE LINE OF and_conditions.
        DATA temp2 TYPE xsdboolean.
      TYPES temp7 TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA or_conditions TYPE temp7.
      DATA or_sub_condition LIKE LINE OF or_conditions.
        DATA temp3 TYPE xsdboolean.
          DATA left_num TYPE i.
          DATA right_num TYPE i.
          DATA temp35 TYPE i.
          DATA temp36 TYPE i.
              DATA temp4 TYPE xsdboolean.
              DATA temp5 TYPE xsdboolean.
              DATA temp8 TYPE xsdboolean.
              DATA temp9 TYPE xsdboolean.
              DATA temp10 TYPE xsdboolean.
              DATA temp11 TYPE xsdboolean.
              DATA temp12 TYPE xsdboolean.
              DATA temp13 TYPE xsdboolean.
              DATA temp14 TYPE xsdboolean.
              DATA temp15 TYPE xsdboolean.
              DATA temp16 TYPE xsdboolean.
              DATA temp17 TYPE xsdboolean.
          DATA temp18 TYPE xsdboolean.
          DATA temp19 TYPE xsdboolean.
      DATA temp20 TYPE xsdboolean.
    condition_text = condense( condition ).
    
    
    temp1 = boolc( strlen( condition_text ) >= 4 AND substring( val = condition_text len = 4 ) = `not ` ).
    is_negated = temp1.
    IF is_negated = abap_true.
      condition_text = condense( substring( val = condition_text
                                            off = 4 ) ).
    ENDIF.

    " Check for logical operators
    IF condition_text CS ' and '.
      

      SPLIT condition_text AT ' and ' INTO TABLE and_conditions.
      result = abap_true.
      
      LOOP AT and_conditions INTO sub_condition.
        sub_condition = condense( sub_condition ).
        IF evaluate_condition_true( condition = sub_condition
                                    context   = context ) = abap_false.
          result = abap_false.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF is_negated = abap_true.
        
        temp2 = boolc( result = abap_false ).
        result = temp2.
      ENDIF.
      RETURN.
    ENDIF.

    IF condition_text CS ' or '.
      

      SPLIT condition_text AT ' or ' INTO TABLE or_conditions.
      result = abap_false.
      
      LOOP AT or_conditions INTO or_sub_condition.
        or_sub_condition = condense( or_sub_condition ).
        IF evaluate_condition_true( condition = or_sub_condition
                                    context   = context ) = abap_true.
          result = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF is_negated = abap_true.
        
        temp3 = boolc( result = abap_false ).
        result = temp3.
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
          
          

          " Check if both values can be converted to numbers
          
          temp35 = left_result.
          left_num = temp35.
          
          temp36 = right_result.
          right_num = temp36.

          " Perform numeric comparison
          CASE operator.
            WHEN '=='.
              
              temp4 = boolc( left_num = right_num ).
              result = temp4.
            WHEN '!='.
              
              temp5 = boolc( left_num <> right_num ).
              result = temp5.
            WHEN '>'.
              
              temp8 = boolc( left_num > right_num ).
              result = temp8.
            WHEN '<'.
              
              temp9 = boolc( left_num < right_num ).
              result = temp9.
            WHEN '>='.
              
              temp10 = boolc( left_num >= right_num ).
              result = temp10.
            WHEN '<='.
              
              temp11 = boolc( left_num <= right_num ).
              result = temp11.
          ENDCASE.
        CATCH cx_root.
          " If numeric comparison fails, do string comparison
          CASE operator.
            WHEN '=='.
              
              temp12 = boolc( left_result = right_result ).
              result = temp12.
            WHEN '!='.
              
              temp13 = boolc( left_result <> right_result ).
              result = temp13.
            WHEN '>'.
              
              temp14 = boolc( left_result > right_result ).
              result = temp14.
            WHEN '<'.
              
              temp15 = boolc( left_result < right_result ).
              result = temp15.
            WHEN '>='.
              
              temp16 = boolc( left_result >= right_result ).
              result = temp16.
            WHEN '<='.
              
              temp17 = boolc( left_result <= right_result ).
              result = temp17.
          ENDCASE.
      ENDTRY.

    ELSE.
      " Handle boolean value
      TRY.
          left_result = resolve_variable( variable_path = condition_text
                                          context       = context ).
          
          temp18 = boolc( left_result = 'true' OR left_result = '1' OR left_result = 'X' OR left_result = 'x' ).
          result = temp18.
        CATCH zcx_llm_template_parser.
          
          temp19 = boolc( condition_text = 'true' OR condition_text = '1' OR condition_text = 'X' OR condition_text = 'x' ).
          result = temp19.
      ENDTRY.
    ENDIF.

    IF is_negated = abap_true.
      
      temp20 = boolc( result = abap_false ).
      result = temp20.
    ENDIF.
  ENDMETHOD.

  METHOD resolve_variable_ref.
    DATA path_segments TYPE string_table.
    DATA current_ref   TYPE REF TO data.
      DATA temp37 TYPE REF TO zcx_llm_template_parser.
    FIELD-SYMBOLS <current_data> TYPE data.
    DATA segment LIKE LINE OF path_segments.
          DATA descr TYPE REF TO cl_abap_typedescr.
            FIELD-SYMBOLS <component> TYPE any.
              DATA temp38 TYPE REF TO zcx_llm_template_parser.
            DATA temp39 TYPE REF TO zcx_llm_template_parser.
          DATA ex TYPE REF TO cx_root.
          DATA temp40 TYPE REF TO zcx_llm_template_parser.

    IF variable_path IS INITIAL OR context IS INITIAL.
      
      CREATE OBJECT temp37 TYPE zcx_llm_template_parser EXPORTING textid = zcx_llm_template_parser=>invalid_variable_path.
      RAISE EXCEPTION temp37.
    ENDIF.

    " Split path into segments
    SPLIT variable_path AT '.' INTO TABLE path_segments.

    " Start with the context
    current_ref = context.
    
    ASSIGN current_ref->* TO <current_data>.

    " Navigate through the path
    
    LOOP AT path_segments INTO segment.
      TRY.
          
          descr = cl_abap_typedescr=>describe_by_data( <current_data> ).

          IF descr->kind = cl_abap_typedescr=>kind_struct.
            " Handle structure components
            
            ASSIGN COMPONENT segment OF STRUCTURE <current_data>
                   TO <component>.
            IF sy-subrc <> 0.
              
              CREATE OBJECT temp38 TYPE zcx_llm_template_parser EXPORTING textid = zcx_llm_template_parser=>invalid_variable_path.
              RAISE EXCEPTION temp38.
            ENDIF.

            CREATE DATA current_ref LIKE <component>.
            ASSIGN current_ref->* TO <current_data>.
            <current_data> = <component>.
          ELSE.
            
            CREATE OBJECT temp39 TYPE zcx_llm_template_parser EXPORTING textid = zcx_llm_template_parser=>unsupported_variable_type.
            RAISE EXCEPTION temp39.
          ENDIF.

          
        CATCH cx_root INTO ex.
          
          CREATE OBJECT temp40 TYPE zcx_llm_template_parser EXPORTING textid = zcx_llm_template_parser=>variable_resolution_error previous = ex.
          RAISE EXCEPTION temp40.
      ENDTRY.
    ENDLOOP.

    result = current_ref.
  ENDMETHOD.


  METHOD process_loop_content.
    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    DATA total_items TYPE i.
    DATA temp41 TYPE REF TO cl_abap_structdescr.
    DATA orig_type LIKE temp41.
    DATA components TYPE abap_component_tab.
    DATA temp42 TYPE REF TO cl_abap_tabledescr.
    DATA table_descr LIKE temp42.
    DATA line_type TYPE REF TO cl_abap_datadescr.
    DATA temp43 LIKE sy-subrc.
      DATA component TYPE abap_componentdescr.
    DATA temp44 LIKE sy-subrc.
      DATA temp45 TYPE loop_meta_type.
      DATA temp31 TYPE REF TO cl_abap_typedescr.
    DATA new_struct_type TYPE REF TO cl_abap_structdescr.
      DATA current_index LIKE sy-index.
      DATA new_context TYPE REF TO data.
      FIELD-SYMBOLS <new_ctx> TYPE data.
      FIELD-SYMBOLS <ctx_data> TYPE data.
      FIELD-SYMBOLS <current_item> TYPE any.
      FIELD-SYMBOLS <loop_var> TYPE any.
      FIELD-SYMBOLS <loop_meta> TYPE any.
      DATA temp46 TYPE loop_meta_type.
      DATA temp21 TYPE xsdboolean.
      DATA temp22 TYPE xsdboolean.

    ASSIGN collection->* TO <table>.
    
    total_items = lines( <table> ).

    " Get the structure of the original context
    
    temp41 ?= cl_abap_typedescr=>describe_by_data_ref( context ).
    
    orig_type = temp41.
    
    components = orig_type->get_components( ).

    " Get table type and line type
    
    temp42 ?= cl_abap_typedescr=>describe_by_data_ref( collection ).
    
    table_descr = temp42.
    
    line_type = table_descr->get_table_line_type( ).

    " Add the loop variable component - first check if it exists
    
    READ TABLE components WITH KEY name = loop_var TRANSPORTING NO FIELDS.
    temp43 = sy-subrc.
    IF NOT temp43 = 0.
      
      component-name  = loop_var.
      component-type ?= line_type.
      INSERT component INTO TABLE components.
    ENDIF.

    " Add loop metadata component only if it doesn't exist
    
    READ TABLE components WITH KEY name = 'LOOP' TRANSPORTING NO FIELDS.
    temp44 = sy-subrc.
    IF NOT temp44 = 0.
      component-name  = 'LOOP'.
      
      CLEAR temp45.
      
      temp31 ?= cl_abap_structdescr=>describe_by_data( temp45 ).
      component-type ?= temp31.
      INSERT component INTO TABLE components.
    ENDIF.

    " Create new structure type
    
    new_struct_type = cl_abap_structdescr=>create( components ).

    DO total_items TIMES.
      
      current_index = sy-index.

      " Create new context for this iteration
      
      CREATE DATA new_context TYPE HANDLE new_struct_type.
      
      ASSIGN new_context->* TO <new_ctx>.

      " Copy original context data
      
      ASSIGN context->* TO <ctx_data>.
      MOVE-CORRESPONDING <ctx_data> TO <new_ctx>.

      " Set loop variable
      
      READ TABLE <table> INDEX current_index ASSIGNING <current_item>.
      
      ASSIGN COMPONENT loop_var OF STRUCTURE <new_ctx> TO <loop_var>.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
      <loop_var> = <current_item>.

      " Set loop metadata
      
      ASSIGN COMPONENT 'LOOP' OF STRUCTURE <new_ctx> TO <loop_meta>.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
      
      CLEAR temp46.
      temp46-index = current_index.
      
      temp21 = boolc( current_index = 1 ).
      temp46-first = temp21.
      
      temp22 = boolc( current_index = total_items ).
      temp46-last = temp22.
      <loop_meta> = temp46.

      " Process tokens with new context
      result = result && parse_tokens( tokens  = tokens
                                       context = new_context ).
    ENDDO.
  ENDMETHOD.

  METHOD handle_nested_for_loop.
    FIELD-SYMBOLS <current_stack> TYPE zcl_llm_template_parser=>control_stack_type.
    result = abap_false.

    
    READ TABLE control_stack INDEX lines( control_stack ) ASSIGNING <current_stack>.
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
          DATA exception TYPE REF TO cx_root.
          DATA temp47 TYPE symsgv.
          DATA temp32 TYPE REF TO zcx_llm_template_parser.
      DATA temp48 LIKE LINE OF control_stack.
      DATA temp49 LIKE sy-tabix.
        DATA temp50 TYPE REF TO zcx_llm_template_parser.
            DATA temp51 TYPE symsgv.
            DATA temp33 TYPE REF TO zcx_llm_template_parser.
      DATA temp52 LIKE LINE OF control_stack.
      DATA temp53 LIKE sy-tabix.
        DATA temp54 TYPE REF TO zcx_llm_template_parser.
      DATA temp23 TYPE xsdboolean.

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
          
        CATCH cx_root INTO exception.
          
          temp47 = condition.
          
          CREATE OBJECT temp32 TYPE zcx_llm_template_parser EXPORTING textid = zcx_llm_template_parser=>condition_evaluation_error msgv1 = temp47 previous = exception.
          RAISE EXCEPTION temp32.
      ENDTRY.

    ELSEIF control_content CP 'elif *' ##NO_TEXT.
      " Handle elif (else if)
      
      
      temp49 = sy-tabix.
      READ TABLE control_stack INDEX lines( control_stack ) INTO temp48.
      sy-tabix = temp49.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
      ENDIF.
      IF lines( control_stack ) = 0 OR temp48-type <> 'IF'.
        
        CREATE OBJECT temp50 TYPE zcx_llm_template_parser EXPORTING textid = zcx_llm_template_parser=>unexpected_elif.
        RAISE EXCEPTION temp50.
      ENDIF.

      READ TABLE control_stack INDEX lines( control_stack ) ASSIGNING <current_stack>.
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
            
            temp51 = condition.
            
            CREATE OBJECT temp33 TYPE zcx_llm_template_parser EXPORTING textid = zcx_llm_template_parser=>condition_evaluation_error msgv1 = temp51 previous = exception.
            RAISE EXCEPTION temp33.
        ENDTRY.
      ELSE.
        <current_stack>-condition_met = abap_false.
      ENDIF.

    ELSEIF control_content = 'else'.
      " Handle else
      
      
      temp53 = sy-tabix.
      READ TABLE control_stack INDEX lines( control_stack ) INTO temp52.
      sy-tabix = temp53.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
      ENDIF.
      IF lines( control_stack ) = 0 OR temp52-type <> 'IF'.
        
        CREATE OBJECT temp54 TYPE zcx_llm_template_parser EXPORTING textid = zcx_llm_template_parser=>unexpected_else.
        RAISE EXCEPTION temp54.
      ENDIF.

      READ TABLE control_stack INDEX lines( control_stack ) ASSIGNING <current_stack>.
      " Only execute else if no previous condition was met
      
      temp23 = boolc( <current_stack>-any_condition_met = abap_false ).
      <current_stack>-condition_met = temp23.
    ENDIF.
  ENDMETHOD.

  METHOD handle_endif.
    DATA temp55 LIKE LINE OF control_stack.
    DATA temp56 LIKE sy-tabix.
      DATA temp57 TYPE REF TO zcx_llm_template_parser.
    temp56 = sy-tabix.
    READ TABLE control_stack INDEX lines( control_stack ) INTO temp55.
    sy-tabix = temp56.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    IF lines( control_stack ) = 0 OR temp55-type <> 'IF'.
      
      CREATE OBJECT temp57 TYPE zcx_llm_template_parser EXPORTING textid = zcx_llm_template_parser=>unexpected_endif.
      RAISE EXCEPTION temp57.
    ENDIF.

    DELETE control_stack INDEX lines( control_stack ).
  ENDMETHOD.

  METHOD handle_for_loop.
    FIELD-SYMBOLS <current_stack> TYPE control_stack_type.
    DATA temp58 TYPE zcl_llm_template_parser=>tokens_type.
        DATA loop_content TYPE string.
        TYPES temp8 TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA loop_parts TYPE temp8.
        DATA temp59 LIKE LINE OF loop_parts.
        DATA temp60 LIKE sy-tabix.
          DATA temp61 TYPE REF TO zcx_llm_template_parser.
        DATA temp62 LIKE LINE OF loop_parts.
        DATA temp63 LIKE sy-tabix.
        DATA collection_path LIKE LINE OF loop_parts.
        DATA temp34 LIKE LINE OF loop_parts.
        DATA temp35 LIKE sy-tabix.
        DATA exception TYPE REF TO cx_root.
        DATA temp64 TYPE REF TO zcx_llm_template_parser.

    APPEND INITIAL LINE TO control_stack ASSIGNING <current_stack>.
    <current_stack>-type        = 'FOR'.
    
    CLEAR temp58.
    <current_stack>-loop_tokens = temp58.

    TRY.
        " Parse for loop syntax: "for item in collection"
        
        loop_content = condense( control_content ).
        

        SPLIT loop_content AT ' ' INTO TABLE loop_parts.
        
        
        temp60 = sy-tabix.
        READ TABLE loop_parts INDEX 3 INTO temp59.
        sy-tabix = temp60.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
        ENDIF.
        IF lines( loop_parts ) <> 4 OR temp59 <> 'in'.
          
          CREATE OBJECT temp61 TYPE zcx_llm_template_parser EXPORTING textid = zcx_llm_template_parser=>invalid_loop_syntax.
          RAISE EXCEPTION temp61.
        ENDIF.

        
        
        temp63 = sy-tabix.
        READ TABLE loop_parts INDEX 2 INTO temp62.
        sy-tabix = temp63.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
        ENDIF.
        <current_stack>-loop_var = temp62.
        
        
        
        temp35 = sy-tabix.
        READ TABLE loop_parts INDEX 4 INTO temp34.
        sy-tabix = temp35.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
        ENDIF.
        collection_path = temp34.

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

        
      CATCH cx_root INTO exception.
        
        CREATE OBJECT temp64 TYPE zcx_llm_template_parser EXPORTING textid = zcx_llm_template_parser=>loop_initialization_error previous = exception.
        RAISE EXCEPTION temp64.
    ENDTRY.
  ENDMETHOD.

  METHOD handle_endfor.
    DATA temp65 LIKE LINE OF control_stack.
    DATA temp66 LIKE sy-tabix.
      DATA temp67 TYPE REF TO zcx_llm_template_parser.
    DATA loop_stack LIKE LINE OF control_stack.
    DATA temp36 LIKE LINE OF control_stack.
    DATA temp37 LIKE sy-tabix.
    temp66 = sy-tabix.
    READ TABLE control_stack INDEX lines( control_stack ) INTO temp65.
    sy-tabix = temp66.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    IF lines( control_stack ) = 0 OR temp65-type <> 'FOR'.
      
      CREATE OBJECT temp67 TYPE zcx_llm_template_parser EXPORTING textid = zcx_llm_template_parser=>unexpected_endfor.
      RAISE EXCEPTION temp67.
    ENDIF.

    
    
    
    temp37 = sy-tabix.
    READ TABLE control_stack INDEX lines( control_stack ) INTO temp36.
    sy-tabix = temp37.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    loop_stack = temp36.
    output_buffer = output_buffer && process_loop_content( tokens     = loop_stack-loop_tokens
                                                           context    = context
                                                           loop_var   = loop_stack-loop_var
                                                           collection = loop_stack-collection ).

    DELETE control_stack INDEX lines( control_stack ).
  ENDMETHOD.

  METHOD process_token.
              DATA exception TYPE REF TO zcx_llm_template_parser.
              DATA temp68 TYPE symsgv.
              DATA temp38 TYPE REF TO zcx_llm_template_parser.
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
              
            CATCH zcx_llm_template_parser INTO exception.
              
              temp68 = token-content.
              
              CREATE OBJECT temp38 TYPE zcx_llm_template_parser EXPORTING textid = zcx_llm_template_parser=>variable_resolution_error msgv1 = temp68 previous = exception.
              RAISE EXCEPTION temp38.
          ENDTRY.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD check_control_stack_conditions.
    FIELD-SYMBOLS <stack_entry> LIKE LINE OF control_stack.
    result = abap_true.

    
    LOOP AT control_stack ASSIGNING <stack_entry>.
      IF <stack_entry>-condition_met = abap_false.
        result = abap_false.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
