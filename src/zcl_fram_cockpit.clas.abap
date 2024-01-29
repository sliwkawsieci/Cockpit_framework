CLASS zcl_fram_cockpit DEFINITION

 ABSTRACT

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
             refresh_data FOR zif_fram_cockpit~refresh_data,
             mv_show_log_icon FOR zif_fram_cockpit~mv_show_log_icon,
             handle_step_action FOR zif_fram_cockpit~handle_step_action,
             handle_log_action FOR zif_fram_cockpit~handle_log_action,
             show_applogs FOR zif_fram_cockpit~show_applogs.


    METHODS: constructor
      IMPORTING
        iv_show_log_icon TYPE sap_bool OPTIONAL
        i_view           TYPE REF TO zif_fram_cockpit_display
        i_data_provider  TYPE REF TO zif_fram_cockpit_data_provider
        is_logger_config TYPE zif_fram_cockpit~ty_logger_config.


  PROTECTED SECTION.
    CONSTANTS:
      BEGIN OF con_log_status,
        success  TYPE char1 VALUE 'S',
        error    TYPE char1 VALUE 'E',
        warrning TYPE char1 VALUE 'W',
      END OF con_log_status,
      BEGIN OF con_step_id,
        open       TYPE zde_fram_cockpit_step_id VALUE 'O',
        posted     TYPE zde_fram_cockpit_step_id VALUE 'P',
        reporcesed TYPE zde_fram_cockpit_step_id VALUE 'R',
        rejected   TYPE zde_fram_cockpit_step_id VALUE 'X',
        error      TYPE zde_fram_cockpit_step_id VALUE 'E',
        approved   TYPE zde_fram_cockpit_step_id VALUE 'A',
      END OF con_step_id.

    TYPES: ty_step_id TYPE char2.
    DATA:
      mt_steps          TYPE TABLE OF zif_fram_cockpit=>ty_step_table,
      mo_data_provider  TYPE REF TO zif_fram_cockpit_data_provider,
      mo_view           TYPE REF TO zif_fram_cockpit_display,
      mo_data_table_alv TYPE REF TO data,
      mo_applog         TYPE REF TO zcl_fram_logger.
    METHODS:
      fill_step_table ABSTRACT,
      get_table_structurt RETURNING VALUE(r_result) TYPE REF TO cl_abap_structdescr ,
      get_next_step_id
        IMPORTING
          iv_id           TYPE zde_fram_cockpit_step_id
        RETURNING
          VALUE(r_result) TYPE zde_fram_cockpit_step_id,
      get_log_messages
        IMPORTING
          iv_extnum       TYPE balnrext
        RETURNING
          VALUE(r_result) TYPE balm_t,
      update_log_icon
        IMPORTING
          is_alv_line     TYPE REF TO data
        RETURNING
          VALUE(r_result) TYPE REF TO data.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF con_generic_columns,
        log     TYPE string VALUE 'LOG',
        log_key TYPE string VALUE 'LOG_KEY',
      END OF con_generic_columns.
    METHODS add_color_str_to_table.
    METHODS: add_log_columns,
      add_additional_columns,
      update_table_data,
      get_log_status
        IMPORTING
                  iv_external_id  TYPE balnrext
        RETURNING VALUE(r_result) TYPE char1.

ENDCLASS.



CLASS zcl_fram_cockpit IMPLEMENTATION.
  METHOD show_applogs.
    LOOP AT get_log_messages( iv_external_id ) ASSIGNING FIELD-SYMBOL(<ls_message>).

      mo_applog->add_message(
     EXPORTING
       iv_type = <ls_message>-msgty
       is_t100msg  = VALUE #( msgno = <ls_message>-msgno msgid = <ls_message>-msgid attr1 = <ls_message>-msgv1 attr2 = <ls_message>-msgv2 attr3 = <ls_message>-msgv3 attr4 = <ls_message>-msgv4 )  ).
    ENDLOOP.
    mo_applog->display( ).
  ENDMETHOD.

  METHOD handle_log_action.
    FIELD-SYMBOLS <lt_data> TYPE STANDARD TABLE.


    ASSIGN mo_data_table_alv->* TO <lt_data>.
    ASSIGN <lt_data>[ iv_row ] TO FIELD-SYMBOL(<ls_line>).

    ASSIGN COMPONENT con_generic_columns-log_key OF STRUCTURE <ls_line> TO FIELD-SYMBOL(<lv_key>).

    show_applogs( <lv_key> ).

  ENDMETHOD.

  METHOD handle_step_action.
    TRY.
        next_step( iv_row ).
*        me->set_information_popup( iv_title = 'Success!'(004) iv_text1 = 'All steps were processed correctly'(006) ).
        get_applog( )->add_text( iv_txt = 'All steps were processed correctly'(006) iv_type = 'S' ).
      CATCH  zcx_fram_cockpit_steps.
*        me->set_information_popup( iv_title = 'Error!'(005) iv_text1 = 'Not all steps were processed correctly'(007) ).
        get_applog( )->add_text( iv_txt = 'Not all steps were processed correctly'(007) iv_type = 'E' ).
    ENDTRY.

    get_applog( )->display( ).
    get_applog( )->delete_logs( ).
    refresh_data( ).
  ENDMETHOD.

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

    me->mo_view = i_view.

    mv_show_log_icon = iv_show_log_icon.

    mo_data_table_alv = mo_data_provider->get_data( ).

    add_additional_columns( ).

    me->zif_fram_cockpit~update_table_settings( ).

    update_table_data( ).

*    refresh_data( ).

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
*        mo_view->display_alv( mo_data_table_alv ).
        mo_view->get_alv( )->display( ).
      CATCH cx_no_data_found.

        MESSAGE i001(zfram_cockpit).

    ENDTRY.

  ENDMETHOD.

  METHOD zif_fram_cockpit~get_data.

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
    update_table_data( ).
    me->mo_view->refresh_data( mo_data_table_alv ).
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
                   <lt_tmc>       TYPE STANDARD TABLE,
                   <lt_alv>       TYPE STANDARD TABLE.


    ASSIGN  mo_data_table_alv->* TO <lt_datatable>.
    lo_data_table = mo_data_table_alv.
    ASSIGN lo_data_table->* TO <lt_tmc>.
    MOVE-CORRESPONDING <lt_datatable> TO <lt_tmc>.

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

  METHOD get_log_messages.

    r_result = mo_applog->get_loggs4extnum( iv_external_number = iv_extnum ).

  ENDMETHOD.

  METHOD zif_fram_cockpit~update_table_settings.
    FIELD-SYMBOLS: <lt_alv> TYPE STANDARD TABLE.

*    CHECK mo_data_table_alv IS NOT INITIAL.
*
*    ASSIGN mo_data_table_alv->* TO <lt_alv>." <lt_data_alv>.

  ENDMETHOD.


  METHOD add_log_columns.
**********************************************************************
    " adding columns for applog to the table

**********************************************************************

    DATA: ls_tab_struc_des TYPE REF TO cl_abap_tabledescr.
    DATA: lr_struct_descr   TYPE REF TO cl_abap_structdescr,
          lr_colorstr_type  TYPE REF TO cl_abap_datadescr,
          lo_data_table     TYPE REF TO data,
          lt_new_table      TYPE REF TO data,
          lr_data_descr     TYPE REF TO cl_abap_datadescr,
          lr_data_descr_key TYPE REF TO cl_abap_datadescr.
    FIELD-SYMBOLS: <lt_datatable> TYPE ANY TABLE,
                   <lt_final>     TYPE STANDARD TABLE,
                   <lt_tmc>       TYPE STANDARD TABLE,
                   <lt_alv>       TYPE STANDARD TABLE.


    ASSIGN  mo_data_table_alv->* TO <lt_datatable>.
    lo_data_table = mo_data_table_alv.
    ASSIGN lo_data_table->* TO <lt_tmc>.
    MOVE-CORRESPONDING <lt_datatable> TO <lt_tmc>.

    ls_tab_struc_des ?= cl_abap_tabledescr=>describe_by_data_ref( mo_data_table_alv ).

    lr_struct_descr ?= ls_tab_struc_des->get_table_line_type( ).
    DATA(lt_componenets) = lr_struct_descr->get_components( ).

    lr_data_descr ?= cl_abap_datadescr=>describe_by_name( 'ICON_D' ).
    lr_data_descr_key ?= cl_abap_datadescr=>describe_by_name( 'balnrext ' ).

    INSERT VALUE abap_componentdescr( name = con_generic_columns-log type = lr_data_descr  ) INTO TABLE lt_componenets.

    INSERT VALUE abap_componentdescr( name = con_generic_columns-log_key type = lr_data_descr_key  ) INTO TABLE lt_componenets.

    DATA(lo_strucdescr) = cl_abap_structdescr=>create( p_components = lt_componenets[] ).

    DATA(lo_tabledescr) = cl_abap_tabledescr=>create( p_line_type = lo_strucdescr ).
*
    ASSIGN lt_new_table->* TO <lt_final>.

    CREATE DATA mo_data_table_alv TYPE HANDLE lo_tabledescr.

    ASSIGN mo_data_table_alv->* TO <lt_alv>.
    <lt_alv> = CORRESPONDING #( <lt_tmc> ).




  ENDMETHOD.

  METHOD get_table_structurt.
    DATA: ls_tab_struc_des TYPE REF TO cl_abap_tabledescr.
    DATA: lr_struct_descr  TYPE REF TO cl_abap_structdescr,
          lr_colorstr_type TYPE REF TO cl_abap_datadescr,
          lo_data_table    TYPE REF TO data,
          lt_new_table     TYPE REF TO data.
    FIELD-SYMBOLS: <lt_datatable> TYPE ANY TABLE,
                   <lt_final>     TYPE STANDARD TABLE,
                   <lt_tmc>       TYPE STANDARD TABLE,
                   <lt_alv>       TYPE STANDARD TABLE.


    ASSIGN  mo_data_table_alv->* TO <lt_datatable>.
    lo_data_table = mo_data_table_alv.
    ASSIGN lo_data_table->* TO <lt_tmc>.
    MOVE-CORRESPONDING <lt_datatable> TO <lt_tmc>.

    ls_tab_struc_des ?= cl_abap_tabledescr=>describe_by_data_ref( mo_data_table_alv ).

    r_result ?= ls_tab_struc_des->get_table_line_type( ).

  ENDMETHOD.


  METHOD add_additional_columns.


    add_color_str_to_table( ).

    IF mv_show_log_icon = abap_true.
      add_log_columns( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_log_status.
    DATA:
            lv_extnum             TYPE balnrext.

    lv_extnum =  iv_external_id.
    DATA(lt_loogs) = mo_applog->get_loggs4extnum( iv_external_number = lv_extnum ).

    ASSIGN lt_loogs[ msgty = con_log_status-error ] TO FIELD-SYMBOL(<ls_log_error>).
    IF sy-subrc = 0.
      r_result = con_log_status-error.
    ELSE.
      ASSIGN lt_loogs[ msgty = con_log_status-warrning ] TO FIELD-SYMBOL(<ls_log_warrning>).
      IF sy-subrc = 0.
        r_result = con_log_status-warrning.
      ELSE.
        ASSIGN lt_loogs[ msgty = con_log_status-success ] TO FIELD-SYMBOL(<ls_log_success>).
        IF sy-subrc = 0.
          r_result = con_log_status-success.
        ENDIF.
      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD update_table_data.
    DATA: lo_alv_line TYPE REF TO data.
    FIELD-SYMBOLS: <lt_alv>     TYPE STANDARD TABLE,
                   <ls_alv_tmp> TYPE data.

    CHECK mo_data_table_alv IS NOT INITIAL.

    ASSIGN mo_data_table_alv->* TO <lt_alv>."



    LOOP AT <lt_alv> ASSIGNING FIELD-SYMBOL(<ls_alv_line>).

      CREATE DATA lo_alv_line LIKE <ls_alv_line>.
      ASSIGN lo_alv_line->* TO <ls_alv_tmp>.

      <ls_alv_tmp> = <ls_alv_line>.
      lo_alv_line = update_log_icon( lo_alv_line ).

      ASSIGN lo_alv_line->* TO <ls_alv_tmp>.
      <ls_alv_line> = <ls_alv_tmp>.

    ENDLOOP.
  ENDMETHOD.


  METHOD update_log_icon.
*    "incoming is_alv_line and returning the same but updated
**********************************************************************
    FIELD-SYMBOLS: <ls_result> TYPE data.
    DATA: lv_logkey TYPE balnrext.

    r_result = is_alv_line.
    ASSIGN r_result->* TO <ls_result>.

    ASSIGN COMPONENT con_generic_columns-log OF STRUCTURE <ls_result> TO FIELD-SYMBOL(<lv_log>).
    IF sy-subrc = 0.
      ASSIGN COMPONENT con_generic_columns-log_key OF STRUCTURE <ls_result> TO FIELD-SYMBOL(<lv_key>).
      CHECK sy-subrc = 0.
      lv_logkey = <lv_key>.
      IF get_log_status( lv_logkey ) = con_log_status-error.
        <lv_log> = icon_error_protocol.
      ENDIF.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
