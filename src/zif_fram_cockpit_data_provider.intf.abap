INTERFACE zif_fram_cockpit_data_provider
  PUBLIC .
  METHODS get_data RETURNING VALUE(ro_result) TYPE REF TO data
                   RAISING
                             zcx_fram_cockpit_no_data.
  METHODS change_status
    IMPORTING
      iv_id     TYPE zadhoc_paymid
      iv_status TYPE zadhoc_status.
  METHODS update_table_line
    IMPORTING
              io_table_line TYPE REF TO data
    RAISING   cx_smt_update_error.
  METHODS get_lgart
    IMPORTING
              iv_current_date TYPE d
              iv_pernr        TYPE pernr_d
    RETURNING VALUE(r_result) TYPE lgart.



ENDINTERFACE.
