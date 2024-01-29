*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


CLASS lcl_view IMPLEMENTATION.

  METHOD get_first_row_log_column.
    FIELD-SYMBOLS:<lt_tabpa0001> TYPE STANDARD TABLE .

    ASSIGN me->mo_table->* TO <lt_tabpa0001>.

    ASSIGN COMPONENT 'LOG' OF STRUCTURE <lt_tabpa0001>[ 1 ] TO FIELD-SYMBOL(<lv_log_icon>).

    r_result = <lv_log_icon>.


  ENDMETHOD.


  METHOD get_log_col_possition.
    DATA: lo_col TYPE REF TO cl_salv_column.
    lo_col ?= mo_alv->get_columns( )->get_column( 'LOG' ).

  ENDMETHOD.

  METHOD set_log_column_possition.

    mo_cols->set_column_position( columnname = 'LOG' position = iv_position ).
    mo_alv->refresh( ).
  ENDMETHOD.

ENDCLASS.


CLASS lcl_main_obj IMPLEMENTATION.

  METHOD fill_step_table.

  ENDMETHOD.

  METHOD constructor.
    DATA: lo_view          TYPE REF TO zif_fram_cockpit_display,
          ls_logger_config TYPE zif_fram_cockpit=>ty_logger_config.

    super->constructor( iv_show_log_icon = iv_show_log_icon i_view = NEW lcl_view( io_parrent = me iv_struct_name = 'PA0001' ) i_data_provider = i_data_provider is_logger_config = ls_logger_config ).
    TRY.
        mo_applog = io_logger.

        mo_view->prepare_alv( zif_fram_cockpit~get_data( )  ).
        mo_test_view ?= mo_view.

      CATCH zcx_fram_cockpit_no_data.

        me->mo_view->set_information_popup( EXPORTING iv_title = 'No data' iv_text1 = 'No data has been selected.' ).
        EXIT.
    ENDTRY.
  ENDMETHOD.

  METHOD get_first_row_view_log_column.
    DATA: lo_view TYPE REF TO lcl_view.

    lo_view ?= mo_view.

    r_result = lo_view->get_first_row_log_column( ).
  ENDMETHOD.


  METHOD set_log_column_possition.
    mo_test_view->set_log_column_possition( iv_col ).
  ENDMETHOD.


  METHOD get_log_col_possition.
    r_result = mo_test_view->get_log_col_possition( ).
  ENDMETHOD.

ENDCLASS.
