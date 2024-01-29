*"* use this source file for your ABAP unit test classes


CLASS lcl_main_obj DEFINITION
INHERITING FROM zcl_fram_cockpit
 CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        io_applog        TYPE REF TO zcl_fram_logger
        iv_show_log_icon TYPE sap_bool OPTIONAL
        i_view           TYPE REF TO zif_fram_cockpit_display
        i_data_provider  TYPE REF TO zif_fram_cockpit_data_provider
        is_logger_config TYPE zif_fram_cockpit~ty_logger_config.
    METHODS: get_table_components RETURNING VALUE(r_result) TYPE cl_abap_structdescr=>component_table  .
  PROTECTED SECTION.
    METHODS: fill_step_table REDEFINITION.
  PRIVATE SECTION.

ENDCLASS.


CLASS lcl_main_obj IMPLEMENTATION.

  METHOD fill_step_table.

  ENDMETHOD.

  METHOD get_table_components.

    r_result = me->get_table_structurt( )->get_components( ).
  ENDMETHOD.

  METHOD constructor.

    super->constructor( iv_show_log_icon = iv_show_log_icon i_view = i_view i_data_provider = i_data_provider is_logger_config = is_logger_config ).

    mo_applog = io_applog.
  ENDMETHOD.

ENDCLASS.


CLASS ltcl_basic_tests DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      check_default_icons FOR TESTING RAISING cx_static_check,
      check_color_column_exists FOR TESTING RAISING cx_static_check,
      check_log_column_not_exists FOR TESTING RAISING cx_static_check,
      check_log_column_exists FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_basic_tests IMPLEMENTATION.

  METHOD check_log_column_exists.
    DATA: lo_view_null     TYPE REF TO zif_fram_cockpit_display,
          lt_pa0001        TYPE TABLE OF pa0001,
          ls_logger_config TYPE zif_fram_cockpit=>ty_logger_config,
          lo_dp_null       TYPE REF TO zif_fram_cockpit_data_provider,
          lo_tabledata     TYPE REF TO data.


    lo_dp_null  ?= cl_abap_testdouble=>create( 'zif_fram_cockpit_data_provider' ).
    CREATE DATA lo_tabledata LIKE lt_pa0001.
    cl_abap_testdouble=>configure_call( lo_dp_null )->returning( lo_tabledata ).
    lo_dp_null->get_data( ).


    DATA(lo_main) = NEW lcl_main_obj(
        io_applog = NEW zcl_fram_logger_null( )
        iv_show_log_icon = abap_true
        i_view           = lo_view_null
        i_data_provider  = lo_dp_null
        is_logger_config = ls_logger_config
    ).

    lo_main->zif_fram_cockpit~get_data( ).
    DATA(lt_comp) =  lo_main->get_table_components( ).

    ASSIGN lt_comp[ name = 'LOG' ] TO FIELD-SYMBOL(<ls_compline>).
    cl_abap_unit_assert=>assert_subrc( exp = 0 msg = 'Missing log table' ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = 'E'   " Data object with current value
        exp                  = <ls_compline>-type->kind
    ).

  ENDMETHOD.

  METHOD check_log_column_not_exists.
    DATA: lo_view_null     TYPE REF TO zif_fram_cockpit_display,
          lt_pa0001        TYPE TABLE OF pa0001,
          ls_logger_config TYPE zif_fram_cockpit=>ty_logger_config,
          lo_dp_null       TYPE REF TO zif_fram_cockpit_data_provider,
          lo_tabledata     TYPE REF TO data.


    lo_dp_null  ?= cl_abap_testdouble=>create( 'zif_fram_cockpit_data_provider' ).
    CREATE DATA lo_tabledata LIKE lt_pa0001.
    cl_abap_testdouble=>configure_call( lo_dp_null )->returning( lo_tabledata ).
    lo_dp_null->get_data( ).

    DATA(lo_main) = NEW lcl_main_obj(
    io_applog = NEW zcl_fram_logger_null( )
        iv_show_log_icon = abap_false
        i_view           = lo_view_null
        i_data_provider  = lo_dp_null
        is_logger_config = ls_logger_config
    ).

    lo_main->zif_fram_cockpit~get_data( ).
    DATA(lt_comp) =  lo_main->get_table_components( ).

    ASSIGN lt_comp[ name = 'LOG' ] TO FIELD-SYMBOL(<ls_compline>).
    IF sy-subrc = 0.
      cl_abap_unit_assert=>fail( 'LOG column shouls not be implemented' ).

    ENDIF.


  ENDMETHOD.

  METHOD check_color_column_exists.
    DATA: lo_view_null     TYPE REF TO zif_fram_cockpit_display,
          lt_pa0001        TYPE TABLE OF pa0001,
          ls_logger_config TYPE zif_fram_cockpit=>ty_logger_config,
          lo_dp_null       TYPE REF TO zif_fram_cockpit_data_provider,
          lo_tabledata     TYPE REF TO data.


    lo_dp_null  ?= cl_abap_testdouble=>create( 'zif_fram_cockpit_data_provider' ).
    CREATE DATA lo_tabledata LIKE lt_pa0001.
    cl_abap_testdouble=>configure_call( lo_dp_null )->returning( lo_tabledata ).
    lo_dp_null->get_data( ).

    DATA(lo_main) = NEW lcl_main_obj(
        io_applog = new zcl_fram_logger_null( )
        iv_show_log_icon = abap_false
        i_view           = lo_view_null
        i_data_provider  = lo_dp_null
        is_logger_config = ls_logger_config
    ).

    lo_main->zif_fram_cockpit~get_data( ).
    DATA(lt_comp) =  lo_main->get_table_components( ).

    ASSIGN lt_comp[ name = 'COLOR' ] TO FIELD-SYMBOL(<ls_compline>).
    IF sy-subrc <> 0.
      cl_abap_unit_assert=>fail( 'Color setting column should be implemented' ).
    ENDIF.

  ENDMETHOD.

  METHOD check_default_icons.
    DATA: lo_view_null     TYPE REF TO zif_fram_cockpit_display,
          lt_pa0001        TYPE TABLE OF pa0001,
          ls_logger_config TYPE zif_fram_cockpit=>ty_logger_config,
          lo_dp_null       TYPE REF TO zif_fram_cockpit_data_provider,
          lo_applog        TYPE REF TO zcl_fram_logger_null,
          lo_tabledata     TYPE REF TO data,
          lo_tabledata2    TYPE REF TO data.

    FIELD-SYMBOLS:
    <lt_tabdata>      TYPE STANDARD TABLE.

    SELECT SINGLE *
    FROM pa0001 INTO @DATA(ls_pa0001).

    INSERT ls_pa0001 INTO TABLE lt_pa0001.

    lo_dp_null  ?= cl_abap_testdouble=>create( 'zif_fram_cockpit_data_provider' ).
    CREATE DATA lo_tabledata LIKE lt_pa0001.
    ASSIGN lo_tabledata->* TO FIELD-SYMBOL(<ls_tabledata>).
    <ls_tabledata> = lt_pa0001.

    cl_abap_testdouble=>configure_call( lo_dp_null )->returning( lo_tabledata ).
    lo_dp_null->get_data( ).

    lo_applog  ?= cl_abap_testdouble=>create( 'zcl_fram_logger_null' ).
    cl_abap_testdouble=>configure_call( lo_applog )->returning( VALUE balm_t( ( msgty = 'E' ) ) ).
    lo_applog->get_loggs4extnum( '' ).

    DATA(lo_main) = NEW lcl_main_obj(
        io_applog = lo_applog
        iv_show_log_icon = abap_true
        i_view           = lo_view_null
        i_data_provider  = lo_dp_null
        is_logger_config = ls_logger_config    ).

    DATA(lo_datatab) = lo_main->zif_fram_cockpit~get_data( ).

    lo_main->zif_fram_cockpit~update_table_settings( ).

    ASSIGN lo_datatab->* TO <lt_tabdata>.

    ASSIGN <lt_tabdata>[ 1 ] TO  FIELD-SYMBOL(<ls_tabdata>).
    IF sy-subrc = 0.
      ASSIGN COMPONENT 'LOG' OF STRUCTURE <ls_tabdata> TO FIELD-SYMBOL(<lv_fieldval>).
      cl_abap_unit_assert=>assert_subrc( msg = 'Missing LOG column' ).
      cl_abap_unit_assert=>assert_equals( act = <lv_fieldval> exp = icon_error_protocol msg = 'LOG has different icon' ).
    ELSE.

      cl_abap_unit_assert=>fail( 'Table should not be empty' ).
    ENDIF.


  ENDMETHOD.

ENDCLASS.
