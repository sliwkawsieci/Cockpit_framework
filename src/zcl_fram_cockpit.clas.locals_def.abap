*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section



CLASS lcl_main_view DEFINITION INHERITING FROM zcl_fram_cockpit_displ
CREATE PRIVATE.

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS: set_column_layout REDEFINITION.
  PRIVATE SECTION.

ENDCLASS.
