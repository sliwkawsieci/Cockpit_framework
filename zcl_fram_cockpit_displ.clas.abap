CLASS zcl_fram_cockpit_displ DEFINITION ABSTRACT
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_fram_cockpit_display.
    ALIASES: set_information_popup FOR zif_fram_cockpit_display~set_information_popup,
             refresh_data FOR zif_fram_cockpit_display~refresh_data,
             prepare_alv FOR zif_fram_cockpit_display~prepare_alv.
    METHODS constructor IMPORTING iv_zadhoc_pay TYPE string io_parrent TYPE REF TO zif_fram_cockpit
                                                            io_applog TYPE any OPTIONAL .


  PROTECTED SECTION.
    CONSTANTS:


    con_action_col TYPE lvc_fname VALUE 'ACTION_BTN' ##NO_TEXT.


    DATA:
      mo_parrent TYPE REF TO zif_fram_cockpit,
      mo_table   TYPE REF TO data,
      mo_alv     TYPE REF TO cl_salv_table,
      mo_layout  TYPE REF TO cl_salv_layout,
      mo_applog  TYPE REF TO zif_logger,
      mo_cols    TYPE REF TO cl_salv_columns.
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
      set_column_layout ABSTRACT,
      set_layout,
      set_row_color,
      handle_step_action
        IMPORTING
          iv_row TYPE i.
  PRIVATE SECTION.
    METHODS add_color_str_to_table.


ENDCLASS.



CLASS zcl_fram_cockpit_displ IMPLEMENTATION.

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

    DATA: ls_color   TYPE lvc_s_scol,
          lo_columns TYPE REF TO cl_salv_columns_table.
*
    lo_columns = mo_alv->get_columns( ).
    lo_columns->set_color_column( value = zif_fram_cockpit_display~con_color_column ).

  ENDMETHOD.

  METHOD add_color_str_to_table.
**********************************************************************
    " adding columns for row coloring to the table
    " supporting only dictoray based tables

**********************************************************************

    DATA: ls_tab_struc_des TYPE REF TO cl_abap_tabledescr.
    DATA: lr_struct_descr  TYPE REF TO cl_abap_structdescr,
          lo_data_table    TYPE REF TO data,
          lr_colorstr_type TYPE REF TO cl_abap_datadescr,
          lt_new_table     TYPE REF TO data.
    FIELD-SYMBOLS: <lt_datatable> TYPE ANY TABLE,
                   <lt_final>     TYPE STANDARD TABLE,
                   <lt_alv>       TYPE STANDARD TABLE.
    lo_data_table = mo_table.
    ASSIGN  lo_data_table->* TO <lt_datatable>.
    ls_tab_struc_des ?= cl_abap_tabledescr=>describe_by_data_ref( mo_table ).

    lr_struct_descr ?= ls_tab_struc_des->get_table_line_type( ).
    DATA(lt_componenets) = lr_struct_descr->get_components( ).

    lr_colorstr_type ?= cl_abap_tabledescr=>describe_by_name( 'LVC_T_SCOL' ).

    INSERT VALUE abap_componentdescr( name = zif_fram_cockpit_display=>con_color_column type = lr_colorstr_type  ) INTO TABLE lt_componenets.

    DATA(lo_strucdescr) = cl_abap_structdescr=>create( p_components = lt_componenets[] ).

    DATA(lo_tabledescr) = cl_abap_tabledescr=>create( p_line_type = lo_strucdescr ).
*
    ASSIGN lt_new_table->* TO <lt_final>.

    CREATE DATA mo_table TYPE HANDLE lo_tabledescr.

    ASSIGN mo_table->* TO <lt_alv>.
    <lt_alv> = CORRESPONDING #( <lt_datatable> ).

  ENDMETHOD.

  METHOD constructor.
    DATA: lr_events  TYPE REF TO cl_salv_events_table.
    FIELD-SYMBOLS: <lt_data> TYPE table.



    IF mo_applog IS INITIAL.

      mo_applog = NEW zcl_zhr_appl_logger(
            iv_object    = zif_fram_cockpit_display=>con_slg0-object
            iv_subobject = zif_fram_cockpit_display=>con_slg0-subobject_cockpit
            iv_extnumber = zif_fram_cockpit_display=>con_slg0-extnumber ).
    ELSE.
      mo_applog =  io_applog.
    ENDIF.


    mo_parrent = io_parrent.

    CREATE DATA mo_table TYPE TABLE OF (iv_zadhoc_pay).



  ENDMETHOD.

  METHOD prepare_alv.
    DATA: lr_events  TYPE REF TO cl_salv_events_table.
    FIELD-SYMBOLS: <lt_data> TYPE table.

    TRY.
        mo_table = mo_parrent->get_data( ).
*        add_color_str_to_table( ).

*        ASSIGN lo_table->* TO <lt_data>.
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

*        mo_applog->display( iv_standard_disp = abap_true ).
    ENDTRY.

  ENDMETHOD.

  METHOD refresh_data.
    mo_table = io_table.
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


*        lv_variant = '/'.
*        lo_layout->set_initial_layout( lv_variant ).
    mo_alv->get_columns( )->set_key_fixation( ).
    mo_cols = mo_alv->get_columns( ).
    mo_layout = mo_alv->get_layout( ).
    mo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

    DATA(lo_functions) = mo_alv->get_functions( ).
    lo_functions->set_all( abap_true ).

*---------- LAYOUT ----------------

  ENDMETHOD.

  METHOD on_link_click.
**********************************************************************
    "" Method for handlin action in ALV.
    "" Predefined action for column Āction_BTN'is here.
    "" If you have other action for the same name of column than use refefinition
**********************************************************************
    CASE column.
      WHEN con_action_col.

        handle_step_action( row ).

      WHEN OTHERS.
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


  METHOD on_added_function.

  ENDMETHOD.

ENDCLASS.
