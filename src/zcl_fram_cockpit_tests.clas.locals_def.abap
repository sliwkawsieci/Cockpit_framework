*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CLASS lcl_view DEFINITION
INHERITING FROM zcl_fram_cockpit_displ
CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: get_log_col_possition RETURNING VALUE(r_result) TYPE char1,
      set_log_column_possition
        IMPORTING
          iv_position TYPE i.
    METHODS: get_first_row_log_column RETURNING VALUE(r_result) TYPE icon_d.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_main_obj DEFINITION
INHERITING FROM zcl_fram_cockpit
 CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS:
      constructor
        IMPORTING
          io_logger        TYPE REF TO zcl_fram_logger_null
          iv_show_log_icon TYPE sap_bool
          i_data_provider  TYPE REF TO zif_fram_cockpit_data_provider,
      get_first_row_view_log_column RETURNING VALUE(r_result) TYPE icon_d,
      set_log_column_possition
        IMPORTING
          iv_col TYPE i,
      get_log_col_possition
        RETURNING
          value(r_result) TYPE i.

  PROTECTED SECTION.
    METHODS: fill_step_table REDEFINITION.
  PRIVATE SECTION.
  data mo_test_view TYPE REF TO lcl_view.

ENDCLASS.
