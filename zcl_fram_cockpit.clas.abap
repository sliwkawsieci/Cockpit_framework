CLASS zcl_fram_cockpit DEFINITION ABSTRACT
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_fram_cockpit.

    ALIASES: display FOR zif_fram_cockpit~display,
             close_ticket FOR zif_fram_cockpit~close_ticket,
             create_message_log FOR zif_fram_cockpit~create_message_log,
             run_steps FOR zif_fram_cockpit~run_steps,
             create_ticket FOR zif_fram_cockpit~create_ticket,
             if_no_c4c FOR zif_fram_cockpit~if_no_c4c,
             next_step FOR zif_fram_cockpit~next_step ,
             run_backgroung_job FOR zif_fram_cockpit~run_backgroung_job,
             get_applog FOR zif_fram_cockpit~get_applog,
             refresh_data FOR zif_fram_cockpit~refresh_data.

    METHODS: constructor
      IMPORTING
        i_view           TYPE REF TO zif_fram_cockpit_display
        i_data_provider  TYPE REF TO zif_fram_cockpit_data_provider
        is_logger_config TYPE zif_fram_cockpit~ty_logger_config.


  PROTECTED SECTION.
    CONSTANTS: BEGIN OF con_step_id,
                 open       TYPE zde_fram_cockpit_step_id VALUE 'O',
                 posted     TYPE zde_fram_cockpit_step_id VALUE 'P',
                 reporcesed TYPE zde_fram_cockpit_step_id VALUE 'R',
                 rejected   TYPE zde_fram_cockpit_step_id VALUE 'X',
                 error      TYPE zde_fram_cockpit_step_id VALUE 'E',
                 approved   TYPE zde_fram_cockpit_step_id VALUE 'A',
               END OF con_step_id.

    TYPES: ty_step_id TYPE char2.
    DATA:
      mt_steps            TYPE TABLE OF zif_fram_cockpit=>ty_step_table,
      mo_data_provider    TYPE REF TO zif_fram_cockpit_data_provider,
      mo_view             TYPE REF TO zif_fram_cockpit_display,
      mo_data_table_alv   TYPE REF TO data,
      mv_no_data_selected TYPE abap_bool,
      mo_applog           TYPE REF TO zcl_fram_logger.
    METHODS:
      fill_step_table ABSTRACT,
      get_next_step_id
        IMPORTING
          iv_id           TYPE zde_fram_cockpit_step_id
        RETURNING
          VALUE(r_result) TYPE zde_fram_cockpit_step_id.

  PRIVATE SECTION.
    METHODS add_color_str_to_table.

ENDCLASS.



CLASS zcl_fram_cockpit IMPLEMENTATION.

  METHOD get_next_step_id.

    ASSIGN mt_steps[ id = iv_id ] TO FIELD-SYMBOL(<ls_steps>).
    IF sy-subrc = 0.
      r_result = <ls_steps>-next_id.
    ELSE.
      "TODO
    ENDIF.

  ENDMETHOD.

  METHOD constructor.

    me->mo_data_provider = i_data_provider.
    mo_applog = NEW zcl_fram_logger(
        iv_object       = is_logger_config-object
        iv_subobject    = is_logger_config-subobject
        iv_extnumber    = is_logger_config-extnumber    ).
    TRY.

*        mo_data_table_alv = me->zif_fram_cockpit~get_data( ).

      CATCH cx_no_data_found INTO DATA(lx_datanotfound).
        mv_no_data_selected = abap_true.
    ENDTRY.
    me->mo_view = i_view.
  ENDMETHOD.

  METHOD close_ticket.

  ENDMETHOD.

  METHOD create_ticket.

  ENDMETHOD.

  METHOD create_message_log.

  ENDMETHOD.

  METHOD run_steps.
*    mo_data_provider->change_status( iv_id = '' iv_status = '' ).
  ENDMETHOD.

  METHOD if_no_c4c.

  ENDMETHOD.


  METHOD display.
    TRY.
        CHECK mo_data_table_alv IS NOT INITIAL.
        mo_view->display_alv( mo_data_table_alv ).
      CATCH cx_no_data_found.

        MESSAGE i001(zfram_cockpit).

    ENDTRY.

  ENDMETHOD.

  METHOD zif_fram_cockpit~get_data.
    mo_data_table_alv = mo_data_provider->get_data( ).

    add_color_str_to_table( ).

    r_result = mo_data_table_alv.
  ENDMETHOD.

  METHOD zif_fram_cockpit~get_step_name.

  ENDMETHOD.

  METHOD zif_fram_cockpit~next_step.

  ENDMETHOD.


  METHOD zif_fram_cockpit~run_backgroung_job.

  ENDMETHOD.

  METHOD zif_fram_cockpit~get_applog.
    r_result = mo_applog.
  ENDMETHOD.

  METHOD refresh_data.
    mo_data_table_alv = me->zif_fram_cockpit~get_data( ).

    me->zif_fram_cockpit~update_table_settings( ).
    me->mo_view->refresh_data(  mo_data_table_alv ).
  ENDMETHOD.

  METHOD add_color_str_to_table.
**********************************************************************
    " adding columns for row coloring to the table
    " supporting only dictoray based tables

**********************************************************************

    DATA: ls_tab_struc_des TYPE REF TO cl_abap_tabledescr.
    DATA: lr_struct_descr  TYPE REF TO cl_abap_structdescr,
          lr_colorstr_type TYPE REF TO cl_abap_datadescr,
          lo_data_table    TYPE REF TO data,
          lt_new_table     TYPE REF TO data.
    FIELD-SYMBOLS: <lt_datatable> TYPE ANY TABLE,
                   <lt_final>     TYPE STANDARD TABLE,
                   <lt_tmc>     TYPE STANDARD TABLE,
                   <lt_alv>       TYPE STANDARD TABLE.


    ASSIGN  mo_data_table_alv->* TO <lt_datatable>.
    lo_data_table = mo_data_table_alv.
    ASSIGN lo_data_table->* to <lt_tmc>.
    MOVE-CORRESPONDING <lt_datatable> to <lt_tmc>.

    ls_tab_struc_des ?= cl_abap_tabledescr=>describe_by_data_ref( mo_data_table_alv ).

    lr_struct_descr ?= ls_tab_struc_des->get_table_line_type( ).
    DATA(lt_componenets) = lr_struct_descr->get_components( ).

    lr_colorstr_type ?= cl_abap_tabledescr=>describe_by_name( 'LVC_T_SCOL' ).

    INSERT VALUE abap_componentdescr( name = CONV #( zif_fram_cockpit_display=>con_color_column ) type = lr_colorstr_type  ) INTO TABLE lt_componenets.

    DATA(lo_strucdescr) = cl_abap_structdescr=>create( p_components = lt_componenets[] ).

    DATA(lo_tabledescr) = cl_abap_tabledescr=>create( p_line_type = lo_strucdescr ).
*
    ASSIGN lt_new_table->* TO <lt_final>.

    CREATE DATA mo_data_table_alv TYPE HANDLE lo_tabledescr.

    ASSIGN mo_data_table_alv->* TO <lt_alv>.
    <lt_alv> = CORRESPONDING #( <lt_tmc> ).

  ENDMETHOD.

  METHOD zif_fram_cockpit~update_table_settings.

  ENDMETHOD.

ENDCLASS.
