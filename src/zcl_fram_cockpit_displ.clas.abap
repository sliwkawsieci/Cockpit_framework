CLASS zcl_fram_cockpit_displ DEFINITION ABSTRACT
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_fram_cockpit_display.
    ALIASES: set_information_popup FOR zif_fram_cockpit_display~set_information_popup,
             refresh_data FOR zif_fram_cockpit_display~refresh_data,
             prepare_alv FOR zif_fram_cockpit_display~prepare_alv,
             get_alv FOR zif_fram_cockpit_display~get_alv.
    METHODS constructor IMPORTING io_parrent     TYPE REF TO zif_fram_cockpit
                                  iv_struct_name TYPE lvc_tname
                                  io_applog      TYPE any OPTIONAL.


  PROTECTED SECTION.
    CONSTANTS:


    con_action_col TYPE lvc_fname VALUE 'ACTION_BTN' ##NO_TEXT.


    DATA:
      mo_parrent     TYPE REF TO zif_fram_cockpit,
      mo_table       TYPE REF TO data,
      mo_alv         TYPE REF TO cl_salv_table,
      mo_layout      TYPE REF TO cl_salv_layout,
      mo_applog      TYPE REF TO zif_logger,
      mo_cols        TYPE REF TO cl_salv_columns,
      mv_struct_name TYPE lvc_tname.
    METHODS: prepare_view,
      on_link_click
                  FOR EVENT link_click OF cl_salv_events_table
        IMPORTING row column ,
      on_added_function FOR EVENT if_salv_events_functions~added_function
                  OF cl_salv_events_table
        IMPORTING e_salv_function,
      set_confirmation_popup
        IMPORTING iv_title      TYPE char30
                  iv_question   TYPE char80
        RETURNING
                  VALUE(rv_ans) TYPE char1,
      set_column_layout ,
      set_layout,
      set_row_color,
      handle_step_action
        IMPORTING
          iv_row TYPE i,
      set_columns_dictionary_ref,
      set_key_column
        IMPORTING
          iv_column TYPE lvc_fname ,
      set_hotspot_column
        IMPORTING
          iv_column TYPE lvc_fname,
      handle_custom_click
        IMPORTING
          iv_row    TYPE i
          iv_column TYPE salv_de_column ,
      jump_fb03
        IMPORTING
          iv_belnr TYPE belnr
          i_row    TYPE i
          iv_buk   TYPE bukrs
          iv_year  TYPE gjahr ,
      set_column_with_and_centred
        IMPORTING
          iv_fname TYPE lvc_fname
          iv_with  TYPE lvc_outlen  OPTIONAL.
  PRIVATE SECTION.
    CONSTANTS con_basic_column_log TYPE lvc_fname VALUE 'LOG' ##NO_TEXT.

    DATA: mv_show_log_icon TYPE sap_bool.



ENDCLASS.



CLASS zcl_fram_cockpit_displ IMPLEMENTATION.

  METHOD set_column_with_and_centred.
    DATA: lv_width TYPE lvc_outlen .

    lv_width = 10.
    IF iv_with IS NOT INITIAL.
      lv_width = iv_with.
    ENDIF.
    mo_cols->get_column( iv_fname )->set_output_length( lv_width ).
    mo_cols->get_column( iv_fname )->set_alignment( if_salv_c_alignment=>centered ).
  ENDMETHOD.

  METHOD jump_fb03.
    DATA: lt_bcdata TYPE TABLE OF bdcdata,
          lv_year   TYPE char4,
          lt_mess   TYPE TABLE OF bdcmsgcoll.

    lv_year = iv_year.

    APPEND INITIAL LINE TO lt_bcdata ASSIGNING FIELD-SYMBOL(<ls_bcdata>).
    <ls_bcdata>-program = 'SAPMF05L'.
    <ls_bcdata>-dynpro = '0100'.
    <ls_bcdata>-dynbegin = 'X'.

    APPEND INITIAL LINE TO lt_bcdata ASSIGNING <ls_bcdata>.
    <ls_bcdata>-fnam = 'RF05L-BELNR'.
    <ls_bcdata>-fval = iv_belnr.

    APPEND INITIAL LINE TO lt_bcdata ASSIGNING <ls_bcdata>.
    <ls_bcdata>-fnam = 'RF05L-BUKRS'.
    <ls_bcdata>-fval = iv_buk.

    APPEND INITIAL LINE TO lt_bcdata ASSIGNING <ls_bcdata>.
    <ls_bcdata>-fnam = 'RF05L-GJAHR'.
    <ls_bcdata>-fval = lv_year.

    CALL TRANSACTION 'FB03' USING lt_bcdata
                       MODE   'E'
                       UPDATE 'S'.

  ENDMETHOD.

  METHOD set_hotspot_column.
    DATA lo_column TYPE REF TO cl_salv_column_table.

    lo_column ?= mo_cols->get_column( iv_column ).
    lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
  ENDMETHOD.

  METHOD set_key_column.
    DATA lo_column TYPE REF TO cl_salv_column_table.


    lo_column ?= mo_cols->get_column( iv_column ).
    lo_column->set_key( abap_true ).
  ENDMETHOD.

  METHOD handle_step_action.
    TRY.
        mo_parrent->next_step( iv_row ).
*        me->set_information_popup( iv_title = 'Success!'(004) iv_text1 = 'All steps were processed correctly'(006) ).
        mo_parrent->get_applog( )->add_text( iv_txt = 'All steps were processed correctly'(006) iv_type = 'S' ).
      CATCH  zcx_fram_cockpit_steps.
*        me->set_information_popup( iv_title = 'Error!'(005) iv_text1 = 'Not all steps were processed correctly'(007) ).
        mo_parrent->get_applog( )->add_text( iv_txt = 'Not all steps were processed correctly'(007) iv_type = 'E' ).
    ENDTRY.

    mo_parrent->get_applog( )->display( ).
    mo_parrent->get_applog( )->delete_logs( ).
    mo_parrent->refresh_data( ).
  ENDMETHOD.

  METHOD set_row_color.

    DATA: lo_columns TYPE REF TO cl_salv_columns_table.
*
    lo_columns = mo_alv->get_columns( ).
    lo_columns->set_color_column( value = zif_fram_cockpit_display~con_color_column ).

  ENDMETHOD.

  METHOD constructor.
    IF mo_applog IS INITIAL.

      mo_applog = NEW zcl_zhr_appl_logger(
            iv_object    = zif_fram_cockpit_display=>con_slg0-object
            iv_subobject = zif_fram_cockpit_display=>con_slg0-subobject_cockpit
            iv_extnumber = zif_fram_cockpit_display=>con_slg0-extnumber ).
    ELSE.
      mo_applog =  io_applog.
    ENDIF.

    mv_struct_name = iv_struct_name.
    mo_parrent = io_parrent.

  ENDMETHOD.

  METHOD prepare_alv.
    DATA: lr_events  TYPE REF TO cl_salv_events_table.
    FIELD-SYMBOLS: <lt_data> TYPE table.

    TRY.
        mo_table = io_data.
        ASSIGN mo_table->* TO <lt_data>.

        cl_salv_table=>factory(
        IMPORTING
          r_salv_table = mo_alv
        CHANGING
          t_table      = <lt_data> ).

        lr_events = mo_alv->get_event( ).

        SET HANDLER on_link_click FOR lr_events.
        SET HANDLER on_added_function FOR lr_events.


        prepare_view( ).

        set_row_color( ).

        set_column_layout( ).
      CATCH cx_salv_msg INTO DATA(lx_error).
        DATA lx_fra TYPE REF TO cx_static_check.
        lx_fra ?= lx_error.

        mo_applog->add_static_exception( lx_fra ).
      CATCH zcx_fram_cockpit_no_data.

*        mo_applog->display( iv_standard_disp = abap_true ).
    ENDTRY.

  ENDMETHOD.

  METHOD refresh_data.

    ASSIGN mo_table->* TO FIELD-SYMBOL(<ls_alv_tab>).
    ASSIGN io_table->* TO FIELD-SYMBOL(<ls_import_tabl>).
    CLEAR <ls_alv_tab>.

    MOVE-CORRESPONDING <ls_import_tabl> TO <ls_alv_tab>.

    mo_alv->refresh( ).

  ENDMETHOD.

  METHOD set_layout.

  ENDMETHOD.

  METHOD set_confirmation_popup.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = iv_title
        text_question         = iv_question
        icon_button_1         = 'ICON_CHECKED'
        icon_button_2         = 'ICON_CANCEL'
        display_cancel_button = abap_false
        popup_type            = 'ICON_MESSAGE_ERROR'
      IMPORTING
        answer                = rv_ans.

  ENDMETHOD.

  METHOD set_information_popup.

    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = iv_title    " Title line of dialog box
        txt1  = iv_text1
        txt2  = iv_text2
*       txt3  = SPACE
*       txt4  = SPACE
      .

  ENDMETHOD.

  METHOD prepare_view.

    mo_cols = mo_alv->get_columns( ).
    set_columns_dictionary_ref( ).
    mo_alv->get_columns( )->set_key_fixation( ).
    mo_layout = mo_alv->get_layout( ).
    mo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

    DATA(lo_functions) = mo_alv->get_functions( ).
    lo_functions->set_all( abap_true ).

*---------- LAYOUT ----------------

  ENDMETHOD.


  METHOD set_columns_dictionary_ref.

    DATA: ls_tab_struc_des TYPE REF TO cl_abap_tabledescr.
    DATA: lr_struct_descr TYPE REF TO cl_abap_structdescr,
          lv_field        TYPE lvc_fname,
          lv_table        TYPE lvc_tname.
    ls_tab_struc_des ?= cl_abap_tabledescr=>describe_by_data_ref( mo_table ).

    lr_struct_descr ?= ls_tab_struc_des->get_table_line_type( ).
    DATA(lt_componenets) = lr_struct_descr->get_components( ).

    LOOP AT lt_componenets ASSIGNING FIELD-SYMBOL(<ls_comp>) WHERE name <> 'COLOR' AND name <> 'LOG' AND name <> 'LOG_KEY'. "TODO more dynamicly(some where table etc.)

      lv_field =  <ls_comp>-name .
      lv_table = <ls_comp>-type->get_relative_name( ).
*      LOOP AT <ls_comp>-type->get_relative_name( )->* ASSIGNING FIELD-SYMBOL(<ls_comp_strline>) WHERE as_include = abap_true.
*      mo_cols->get_column( <ls_comp_strline>-type->get_relative_name( ) )->set_ddic_reference( VALUE salv_s_ddic_reference( table = mv_struct_name field = <ls_comp_strline> ) ).
*
*      ENDLOOP.
      CHECK lv_field IS NOT INITIAL or <ls_comp>-as_include = abap_false.
      mo_cols->get_column( lv_field )->set_ddic_reference( VALUE salv_s_ddic_reference( table = mv_struct_name field = lv_field ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD on_link_click.
**********************************************************************
    "" Method for handlin action in ALV.
    "" Predefined action for column Āction_BTN'is here.
    "" If you have other action for the same name of column than use refefinition
**********************************************************************
    CASE column.
      WHEN con_action_col.

        mo_parrent->handle_step_action( row ).
        when con_basic_column_log.

        mo_parrent->handle_log_action( row ).

      WHEN OTHERS.
        handle_custom_click( iv_row = row iv_column = column ).
    ENDCASE.
  ENDMETHOD.

  METHOD zif_fram_cockpit_display~display_alv.
    FIELD-SYMBOLS: <lt_data> TYPE table.
    FIELD-SYMBOLS: <lt_incoming_data> TYPE table.

    ASSIGN io_table->* TO <lt_incoming_data>.
    ASSIGN mo_table->* TO <lt_data>.

    MOVE-CORRESPONDING <lt_incoming_data> TO <lt_data>.

    mo_alv->display( ).
  ENDMETHOD.

   METHOD get_alv.
    r_result = mo_alv.
   ENDMETHOD.

  METHOD on_added_function.

  ENDMETHOD.


  METHOD handle_custom_click.

  ENDMETHOD.

  METHOD set_column_layout.

    "TODO add check if log is available
    CHECK mo_parrent->mv_show_log_icon = abap_true.

    set_key_column(  con_basic_column_log ).
*
    set_hotspot_column( con_basic_column_log ).

    set_column_with_and_centred( iv_fname = con_basic_column_log  iv_with = 5 ).

    mo_cols->set_column_position( columnname = con_basic_column_log position = 1 ).

  ENDMETHOD.

ENDCLASS.
