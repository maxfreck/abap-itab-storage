INTERFACE zif_itab
  PUBLIC .


  CLASS-METHODS from_itab
    CHANGING
      !ct_itab      TYPE ANY TABLE
    RETURNING
      VALUE(ro_ret) TYPE REF TO zif_itab .
  CLASS-METHODS from_xstring
    IMPORTING
      !iv_xstring   TYPE xstring
    RETURNING
      VALUE(ro_ret) TYPE REF TO zif_itab .
  METHODS get_xstring
    RETURNING
      VALUE(rv_xstring) TYPE xstring .
  METHODS move_to_itab
    CHANGING
      !ct_itab TYPE STANDARD TABLE .

  CLASS-METHODS get_extension
    RETURNING VALUE(rv_ext) TYPE string.
ENDINTERFACE.
