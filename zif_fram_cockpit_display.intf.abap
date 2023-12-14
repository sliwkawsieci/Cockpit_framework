INTERFACE zif_fram_cockpit_display
  PUBLIC .


  CONSTANTS: con_color_column TYPE lvc_fname  VALUE 'COLOR'.

  METHODS: display_alv IMPORTING io_table TYPE REF TO data      ,
    set_information_popup   IMPORTING
                              iv_title TYPE char30
                              iv_text1 TYPE char90
                              iv_text2 TYPE char90  OPTIONAL   ,
           refresh_data
             IMPORTING
               io_table TYPE any OPTIONAL,
           prepare_alv.
  CONSTANTS:
    BEGIN OF con_slg0,
      object            TYPE balobj_d VALUE 'ZFRAM',
      subobject         TYPE balsubobj VALUE 'BASIC',
      subobject_cockpit TYPE balsubobj VALUE 'COCKPIT',
      extnumber         TYPE balnrext VALUE '',
    END OF con_slg0 .
ENDINTERFACE.
