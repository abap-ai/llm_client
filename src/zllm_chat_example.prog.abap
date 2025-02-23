*&---------------------------------------------------------------------*
*& Report ZLLM_CHAT_EXAMPLE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zllm_chat_example.
DATA model        TYPE zllm_model.
DATA user_message TYPE string.

CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    METHODS display.
    METHODS handle_user_command IMPORTING ucomm TYPE sy-ucomm.
    METHODS initialize_text_editors.

  PRIVATE SECTION.
    DATA: text_editor_input    TYPE REF TO cl_gui_textedit,
          text_editor_output   TYPE REF TO cl_gui_textedit,
          custom_container_in  TYPE REF TO cl_gui_custom_container,
          custom_container_out TYPE REF TO cl_gui_custom_container.


    METHODS get_response.
    METHODS set_output_text IMPORTING text TYPE string.
    METHODS cleanup_controls.
ENDCLASS.

CLASS lcl_app IMPLEMENTATION.
  METHOD display.
    CALL SCREEN 100.
  ENDMETHOD.

  METHOD handle_user_command.
    CASE ucomm.
      WHEN 'SEND'.
        get_response( ).
      WHEN 'BACK' OR 'CANCEL' OR 'EXIT'.  " Consistent handling
        cleanup_controls( ).
        LEAVE TO SCREEN 0. " Exit the program
    ENDCASE.
  ENDMETHOD.

  METHOD initialize_text_editors.
    IF text_editor_input IS NOT BOUND.  " Only initialize if not already done
      custom_container_in = NEW #( container_name = 'INPUT_CONTAINER' ).
      text_editor_input = NEW #( parent = custom_container_in ).

      " Settings for input editor
      text_editor_input->set_toolbar_mode( cl_gui_textedit=>false ).
      text_editor_input->set_statusbar_mode( cl_gui_textedit=>false ).
      text_editor_input->set_wordwrap_behavior( wordwrap_mode     = cl_gui_textedit=>wordwrap_at_fixed_position
                                                wordwrap_position = 100 ).
    ENDIF.

    IF text_editor_output IS NOT BOUND.
      custom_container_out = NEW #( container_name = 'OUTPUT_CONTAINER' ).
      text_editor_output = NEW #( parent = custom_container_out ).

      " Settings for output editor
      text_editor_output->set_toolbar_mode( cl_gui_textedit=>false ).
      text_editor_output->set_statusbar_mode( cl_gui_textedit=>false ).
      text_editor_output->set_wordwrap_behavior( wordwrap_mode     = cl_gui_textedit=>wordwrap_at_fixed_position
                                                 wordwrap_position = 100 ).
      text_editor_output->set_readonly_mode( 1 ).  " Make output read-only
    ENDIF.

    " Flush operations so controls are sent to the frontend immediately!
    cl_gui_cfw=>flush( ).
  ENDMETHOD.

  METHOD get_response.
    " Error Handling first!
    IF model IS INITIAL.
      MESSAGE 'Please select a model.'(002) TYPE 'I' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    " Get user input text
    DATA texts TYPE STANDARD TABLE OF char255.

    text_editor_input->get_text_as_stream( IMPORTING text = texts ).

    user_message = concat_lines_of( table = texts
                                    sep   = cl_abap_char_utilities=>cr_lf ).

    IF user_message IS INITIAL.
      MESSAGE 'Please enter a message.'(003) TYPE 'I' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    TRY.
        DATA(client) = zcl_llm_factory=>get_client( model ).
        DATA(response) = client->execute( user_message = user_message ).
        set_output_text( response-choice-message-content ).

      CATCH zcx_llm_authorization.
        MESSAGE 'Not authorized.'(005) TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD set_output_text.
    DATA texts TYPE STANDARD TABLE OF char255.

    " Convert the string to table of fixed-length strings
    DATA(remaining_text) = text.
    WHILE strlen( remaining_text ) > 0.
      DATA(line) = substring( val = remaining_text
                              len = COND #(
                              WHEN strlen( remaining_text ) > 255
                              THEN 255
                              ELSE strlen( remaining_text ) ) ).
      APPEND line TO texts.
      remaining_text = substring( val = remaining_text
                                  off = strlen( line )
                                  len = strlen( remaining_text ) - strlen( line ) ).
    ENDWHILE.

    text_editor_output->set_text_as_stream( texts ).
    cl_gui_cfw=>flush( ).
  ENDMETHOD.

  METHOD cleanup_controls.
    IF text_editor_input IS BOUND.
      text_editor_input->free( ).
    ENDIF.
    IF text_editor_output IS BOUND.
      text_editor_output->free( ).
    ENDIF.

    IF custom_container_in IS BOUND.
      custom_container_in->free( ).
    ENDIF.
    IF custom_container_out IS BOUND.
      custom_container_out->free( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

DATA app TYPE REF TO lcl_app.

INITIALIZATION.
  app = NEW #( ).

START-OF-SELECTION.
  app->display( ).

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR '0100'.
  app->initialize_text_editors( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  app->handle_user_command( sy-ucomm ).
ENDMODULE.
