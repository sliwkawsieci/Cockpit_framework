*"* use this source file for your ABAP unit test classes




CLASS ltcl_main_logic DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: mo_dp_null TYPE REF TO zif_fram_cockpit_data_provider,
          mo_applog  TYPE REF TO zcl_fram_logger_null.
    METHODS:
      create_subject ,
      first_test FOR TESTING RAISING cx_static_check,
      log_key_as_technical FOR TESTING RAISING cx_static_check,
      log_key_with_data FOR TESTING RAISING cx_static_check,
      overwrite_col_positon FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_main_logic IMPLEMENTATION.

  METHOD first_test.
    FIELD-SYMBOLS:
    <lt_tabdata>      TYPE STANDARD TABLE.

    create_subject( ).

    DATA(lo_main) = NEW lcl_main_obj(
        io_logger = mo_applog
        iv_show_log_icon = abap_true
        i_data_provider  = mo_dp_null           ).



    DATA(lo_datatab) = lo_main->zif_fram_cockpit~get_data( ).

    lo_main->zif_fram_cockpit~update_table_settings( ).

    ASSIGN lo_datatab->* TO <lt_tabdata>.

    ASSIGN <lt_tabdata>[ 1 ] TO  FIELD-SYMBOL(<ls_tabdata>).
    IF sy-subrc = 0.
      ASSIGN COMPONENT 'LOG'  OF STRUCTURE <ls_tabdata> TO FIELD-SYMBOL(<lv_fieldval>).
      cl_abap_unit_assert=>assert_subrc( msg = 'Missing LOG column' ).
      cl_abap_unit_assert=>assert_equals( act = lo_main->get_first_row_view_log_column( ) exp = icon_error_protocol msg = 'LOG has different icon' ).
    ELSE.

      cl_abap_unit_assert=>fail( 'Table should not be empty' ).
    ENDIF.


  ENDMETHOD.

  METHOD create_subject.
    DATA : lo_tabledata TYPE REF TO data.
    DATA: lt_pa0001        TYPE TABLE OF pa0001.
    SELECT SINGLE *
    FROM pa0001 INTO @DATA(ls_pa0001).

    INSERT ls_pa0001 INTO TABLE lt_pa0001.

    mo_dp_null  ?= cl_abap_testdouble=>create( 'zif_fram_cockpit_data_provider' ).
    CREATE DATA lo_tabledata LIKE lt_pa0001.
    ASSIGN lo_tabledata->* TO FIELD-SYMBOL(<ls_tabledata>).
    <ls_tabledata> = lt_pa0001.

    cl_abap_testdouble=>configure_call( mo_dp_null )->returning( lo_tabledata ).
    mo_dp_null->get_data( ).

    mo_applog  ?= cl_abap_testdouble=>create( 'zcl_fram_logger_null' ).
    cl_abap_testdouble=>configure_call( mo_applog )->returning( VALUE balm_t( ( msgty = 'E' ) ) ).
    mo_applog->get_loggs4extnum( '' ).
  ENDMETHOD.

  METHOD overwrite_col_positon.
    "Log is by default on 1 position and her it will be on 3.
    create_subject( ).
    DATA(lo_main) = NEW lcl_main_obj(
        io_logger = mo_applog
        iv_show_log_icon = abap_true
        i_data_provider  = mo_dp_null           ).

    lo_main->set_log_column_possition( 3 ) .

*    cl_abap_unit_assert=>assert_equals(
*      EXPORTING
*        act                  =      " Data object with current value
*        exp                  = 3    " Data object with expected type
*).
    cl_abap_unit_assert=>fail( 'NOT READY' ).
  ENDMETHOD.

  METHOD log_key_as_technical.
    cl_abap_unit_assert=>fail( 'NOT READY' ).
  ENDMETHOD.

  METHOD log_key_with_data.
    cl_abap_unit_assert=>fail( 'NOT READY' ).
  ENDMETHOD.

ENDCLASS.
