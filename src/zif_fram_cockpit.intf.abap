INTERFACE zif_fram_cockpit
  PUBLIC .
  TYPES: BEGIN OF ty_step_table,
           id      TYPE zde_fram_cockpit_step_id,
           next_id TYPE zde_fram_cockpit_step_id,
*           name    TYPE char30,
         END OF ty_step_table,
         tt_step_table TYPE SORTED TABLE OF ty_step_table WITH DEFAULT KEY,
         BEGIN OF ty_logger_config,
             object    TYPE balobj_d,
             subobject TYPE balsubobj,
             extnumber TYPE balnrext,
           END OF ty_logger_config.
  DATA mv_show_log_icon TYPE sap_bool READ-ONLY.

  METHODS: display,
    close_ticket,
    create_message_log,
    run_steps,
    create_ticket,
    get_step_name,
    if_no_c4c,
    next_step
      IMPORTING i_row TYPE i
      RAISING   zcx_fram_cockpit_steps,
           get_data
             RETURNING
               value(r_result) TYPE REF TO data
               RAISING zcx_fram_cockpit_no_data         ,
           run_backgroung_job ,
           get_applog RETURNING value(r_result) TYPE REF TO zcl_fram_logger,
           refresh_data,
           update_table_settings,
           handle_step_action
               IMPORTING
                 iv_row TYPE i,
           handle_log_action
               IMPORTING
                 iv_row TYPE i,
           show_applogs
               IMPORTING
                 iv_external_id TYPE balnrext .

ENDINTERFACE.
