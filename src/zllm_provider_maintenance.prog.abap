REPORT zllm_provider_maintenance.

CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF provider_config,
        provider_name        TYPE zllm_provider_name,
        provider_class       TYPE zllm_provider,
        rfc_destination      TYPE rfcdest,
        auth_rfc_destination TYPE rfcdest,
        auth_value           TYPE zllm_auth_value,
        auth_encrypted       TYPE xstring,
      END OF provider_config,
      provider_configs TYPE STANDARD TABLE OF provider_config WITH KEY provider_name.

    METHODS constructor. " Initialize application
    METHODS display_providers. " Display the providers in ALV

    METHODS handle_action_add.    " Handle adding a new provider
    METHODS handle_action_change. " Handle updating a provider
    METHODS handle_action_delete. " Handle deleting a provider

  PRIVATE SECTION.
    DATA providers TYPE provider_configs.
    DATA grid      TYPE REF TO cl_gui_alv_grid.
    DATA enc_class TYPE REF TO zif_llm_encryption.

    METHODS load_providers. " Load the provider list from the database
    METHODS save_provider IMPORTING config TYPE provider_config. " Save a provider to the database

    METHODS encrypt_auth_value IMPORTING plain         TYPE zllm_auth_value
                               RETURNING VALUE(result) TYPE xstring. " Encrypt a value

    METHODS decrypt_auth_value IMPORTING encrypted     TYPE zllm_auth_enc
                               RETURNING VALUE(result) TYPE zllm_auth_value.

    METHODS build_field_catalog RETURNING VALUE(fieldcat) TYPE lvc_t_fcat. " Build ALV field catalog
    METHODS refresh_display. " Refresh displayed ALV data

    METHODS show_confirm_popup IMPORTING !title        TYPE string
                                         !text         TYPE string
                               RETURNING VALUE(result) TYPE abap_bool. " Show confirm popup
ENDCLASS.

CLASS lcl_popup_screen DEFINITION.
  PUBLIC SECTION.
    METHODS constructor IMPORTING provider TYPE lcl_app=>provider_config.
    METHODS show        IMPORTING !title   TYPE string.

    METHODS pai         IMPORTING ucomm    TYPE sy-ucomm.

    METHODS pbo.

    DATA cancelled TYPE abap_bool.
    DATA result    TYPE lcl_app=>provider_config.
    DATA title     TYPE string.

  PRIVATE SECTION.
    TYPES: BEGIN OF textline,
             line TYPE c LENGTH 255,
           END OF textline,
           textlines TYPE STANDARD TABLE OF textline WITH EMPTY KEY.

    DATA provider_name        TYPE zllm_provider_name.
    DATA provider_class       TYPE zllm_provider.
    DATA rfc_destination      TYPE rfcdest.
    DATA auth_rfc_destination TYPE rfcdest.
    DATA text_editor          TYPE REF TO cl_gui_textedit.
    DATA custom_container     TYPE REF TO cl_gui_custom_container.

    METHODS initialize_text_editor.
    METHODS set_text_editor_content.
    METHODS cleanup_controls.
ENDCLASS.

DATA: popup_screen TYPE REF TO lcl_popup_screen.
DATA: BEGIN OF screen_fields,
        provider_name        TYPE zllm_provider_name,
        provider_class       TYPE zllm_provider,
        rfc_destination      TYPE rfcdest,
        auth_rfc_destination TYPE rfcdest,
      END OF screen_fields.

CLASS lcl_app IMPLEMENTATION.
  METHOD constructor.
    load_providers( ).
    DATA(llm_badi) = zcl_llm_common=>get_llm_badi( ).
    CALL BADI llm_badi->get_encryption_impl
      RECEIVING result = enc_class.
  ENDMETHOD.

  METHOD load_providers.
    SELECT * FROM zllm_providers INTO CORRESPONDING FIELDS OF TABLE @providers ORDER BY provider_name ##SUBRC_OK. "#EC CI_BYPASS "#EC CI_GENBUFF
  ENDMETHOD.

  METHOD display_providers.
    IF grid IS NOT BOUND.
      grid = NEW #( i_parent = cl_gui_container=>default_screen ).

      " Build field catalog with reusable logic
      DATA(fieldcat) = build_field_catalog( ).

      " Configure ALV layout
      DATA(layout) = VALUE lvc_s_layo( sel_mode   = 'A'
                                       zebra      = abap_true
                                       col_opt    = abap_true
                                       cwidth_opt = abap_true
                                       no_toolbar = abap_true ).

      " Set ALV for display
      grid->set_table_for_first_display( EXPORTING is_layout       = layout
                                                   i_save          = 'A'
                                                   i_default       = 'X'
                                         CHANGING  it_outtab       = providers
                                                   it_fieldcatalog = fieldcat ).
    ELSE.
      refresh_display( ).
    ENDIF.
  ENDMETHOD.

  METHOD handle_action_add.
    DATA title TYPE string.

    title = 'Add Provider Configuration'(006).

    popup_screen = NEW #( VALUE #( ) ).
    popup_screen->result = VALUE #( ). " Clear any previous values
    popup_screen->show( title ).

    IF popup_screen->cancelled = abap_false.
      popup_screen->result-auth_encrypted = encrypt_auth_value( popup_screen->result-auth_value ).
      save_provider( popup_screen->result ).
      load_providers( ).
    ENDIF.
  ENDMETHOD.

  METHOD handle_action_change.
    DATA sel_rows TYPE lvc_t_row.

    grid->get_selected_rows( IMPORTING et_index_rows = sel_rows ).
    IF lines( sel_rows ) <> 1.
      MESSAGE 'Select one row'(014) TYPE 'E'.
    ENDIF.

    DATA(selected_provider) = providers[ sel_rows[ 1 ]-index ].
    selected_provider-auth_value = decrypt_auth_value( selected_provider-auth_encrypted ).

    DATA title TYPE string.
    title = 'Change Provider Configuration'(007).

    popup_screen = NEW #( selected_provider ).
    popup_screen->result = selected_provider. " Set the current values
    popup_screen->show( title ).

    IF popup_screen->cancelled = abap_false.
      popup_screen->result-auth_encrypted = encrypt_auth_value( popup_screen->result-auth_value ).
      save_provider( popup_screen->result ).
      load_providers( ).
    ENDIF.
  ENDMETHOD.

  METHOD handle_action_delete.
    DATA sel_rows TYPE lvc_t_row.

    grid->get_selected_rows( IMPORTING et_index_rows = sel_rows ).
    IF lines( sel_rows ) <> 1.
      MESSAGE 'Select one row'(014) TYPE 'E'.
    ENDIF.

    DATA(selected_provider) = providers[ sel_rows[ 1 ]-index ].
    DATA title TYPE string.
    DATA text  TYPE string.
    title = 'Confirm Deletion'(010).
    text = 'Delete provider'(011).
    DATA(confirmed) = show_confirm_popup( title = title
                                          text  = |{ text } { selected_provider-provider_name }?| ).

    IF confirmed = abap_true.
      DELETE FROM zllm_providers WHERE provider_name = @selected_provider-provider_name.
      IF sy-subrc = 0.
        load_providers( ).
        MESSAGE 'Provider deleted successfully'(008) TYPE 'S'.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD save_provider.
    " Using a two step corresponding to avoid abaplint donwport issues
    DATA provider TYPE zllm_providers.

    provider = CORRESPONDING #( config ).
    MODIFY zllm_providers FROM @provider.
    IF sy-subrc = 0.
      MESSAGE 'Provider configuration saved successfully'(009) TYPE 'S'.
    ENDIF.
  ENDMETHOD.

  METHOD encrypt_auth_value.
    TRY.
        result = enc_class->encrypt( plain ).
      CATCH zcx_llm_authorization.
        MESSAGE 'No Authorization to encrypt!'(012) TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD build_field_catalog.
    " This definition ensures I do not accidentially delete it as unused again ;-)
    DATA ensure_ddic_ref TYPE zllm_provider_disp ##NEEDED.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING i_structure_name = 'ZLLM_PROVIDER_DISP'
      CHANGING  ct_fieldcat      = fieldcat.

    DELETE fieldcat WHERE fieldname = 'AUTH_VALUE'.

    LOOP AT fieldcat ASSIGNING FIELD-SYMBOL(<fieldcat>).
      IF <fieldcat>-fieldname <> 'PROVIDER_NAME'.
        <fieldcat>-edit = abap_true.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD refresh_display.
    grid->refresh_table_display( ).
  ENDMETHOD.

  METHOD show_confirm_popup.
    DATA(answer) = ''.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING titlebar      = title
                text_question = text
                text_button_1 = 'Yes'
                text_button_2 = 'No'
      IMPORTING answer        = answer.

    result = xsdbool( answer = '1' ).
  ENDMETHOD.

  METHOD decrypt_auth_value.
    TRY.
        result = enc_class->decrypt( encrypted ).
      CATCH zcx_llm_authorization.
        MESSAGE 'No Authorization to decrypt!'(013) TYPE 'E'.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_screen DEFINITION.
  PUBLIC SECTION.
    METHODS start.

    METHODS pai IMPORTING ucomm TYPE sy-ucomm.

    METHODS pbo.

  PRIVATE SECTION.
    DATA app TYPE REF TO lcl_app.
ENDCLASS.

CLASS lcl_screen IMPLEMENTATION.
  METHOD start.
    app = NEW #( ).
    CALL SCREEN 100.
  ENDMETHOD.

  METHOD pai.
    CASE ucomm.
      WHEN 'BACK' OR 'CANCEL' OR 'EXIT'.
        LEAVE TO SCREEN 0.
      WHEN 'ADD'.
        app->handle_action_add( ).
      WHEN 'CHANGE'.
        app->handle_action_change( ).
      WHEN 'DELETE'.
        app->handle_action_delete( ).
      WHEN 'REFRESH'.
        app->display_providers( ).
    ENDCASE.
  ENDMETHOD.

  METHOD pbo.
    SET PF-STATUS 'MAIN100'.
    SET TITLEBAR 'TITLE100'.
    app->display_providers( ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_popup_screen IMPLEMENTATION.
  METHOD constructor.
    result = provider.
    provider_name = provider-provider_name.
    provider_class = provider-provider_class.
    rfc_destination = provider-rfc_destination.
    auth_rfc_destination = provider-auth_rfc_destination.
  ENDMETHOD.

  METHOD show.
    me->title = title.
    cleanup_controls( ). " Ensure clean state before showing
    CALL SCREEN 200 STARTING AT 10 3 ENDING AT 140 28.
  ENDMETHOD.

  METHOD initialize_text_editor.
    IF text_editor IS NOT BOUND.
      custom_container = NEW #( container_name = 'CUSTOM_CONTROL' ).
      text_editor = NEW #( parent            = custom_container
                           wordwrap_mode     = 2
                           wordwrap_position = 110 ).
      text_editor->set_toolbar_mode( 0 ).
      text_editor->set_statusbar_mode( 0 ).
      text_editor->set_readonly_mode( 0 ).
      text_editor->set_visible( abap_true ).
      cl_gui_cfw=>flush( ).
    ENDIF.
  ENDMETHOD.

  METHOD set_text_editor_content.
    DATA auth_value  TYPE string.
    DATA line_length TYPE i VALUE 110.
    DATA remaining   TYPE string.
    DATA current_len TYPE i.
    DATA split_pos   TYPE i.
    DATA textlines   TYPE textlines.
    DATA textline    TYPE textline.

    auth_value = result-auth_value.
    remaining = auth_value.

    WHILE remaining IS NOT INITIAL.
      current_len = strlen( remaining ).

      IF current_len <= line_length.
        textline-line = remaining.
        APPEND textline TO textlines.
        CLEAR remaining.
      ELSE.
        split_pos = line_length.
        FIND `\s` IN remaining(line_length) RESPECTING CASE MATCH OFFSET split_pos.
        IF sy-subrc <> 0.
          split_pos = line_length.
        ENDIF.

        textline-line = remaining(split_pos).
        APPEND textline TO textlines.
        remaining = remaining+split_pos.
        remaining = shift_left( remaining ).
      ENDIF.
    ENDWHILE.

    text_editor->delete_text( ).
    text_editor->set_text_as_r3table( textlines ).
    text_editor->set_focus( text_editor ).
  ENDMETHOD.

  METHOD pbo.
    SET PF-STATUS 'POPUP200'.
    SET TITLEBAR 'TITLE200' WITH title.

    " Fill screen fields
    screen_fields-provider_name        = provider_name.
    screen_fields-provider_class       = provider_class.
    screen_fields-rfc_destination      = rfc_destination.
    screen_fields-auth_rfc_destination = auth_rfc_destination.

    " Always initialize and set content
    initialize_text_editor( ).
    set_text_editor_content( ).
  ENDMETHOD.

  METHOD cleanup_controls.
    IF text_editor IS BOUND.
      text_editor->free( ).
    ENDIF.
    IF custom_container IS BOUND.
      custom_container->free( ).
    ENDIF.
  ENDMETHOD.

  METHOD pai.
    CASE ucomm.
      WHEN 'OK'.
        DATA text TYPE textlines.
        text_editor->get_text_as_r3table( IMPORTING table = text ).

        result-provider_name        = screen_fields-provider_name.
        result-provider_class       = screen_fields-provider_class.
        result-rfc_destination      = screen_fields-rfc_destination.
        result-auth_rfc_destination = screen_fields-auth_rfc_destination.
        result-auth_value           = concat_lines_of( text ).
        cancelled = abap_false.
        cleanup_controls( ).
        LEAVE TO SCREEN 0.
      WHEN 'CANCEL'.
        cancelled = abap_true.
        cleanup_controls( ).
        LEAVE TO SCREEN 0.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

INITIALIZATION.
  DATA(screen) = NEW lcl_screen( ).

START-OF-SELECTION.
  screen->start( ).

*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  screen->pbo( ).
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module USER_COMMAND_0100 INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  screen->pai( sy-ucomm ).
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  popup_screen->pbo( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
" -----------------------------------------------------------------------
MODULE user_command_0200 INPUT.
  popup_screen->pai( sy-ucomm ).
ENDMODULE.
