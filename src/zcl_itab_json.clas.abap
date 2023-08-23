CLASS zcl_itab_json DEFINITION PUBLIC CREATE PROTECTED.
  PUBLIC SECTION.
    INTERFACES zif_itab.
    ALIASES:
      from_itab FOR zif_itab~from_itab,
      from_xstring FOR zif_itab~from_xstring,
      get_xstring FOR zif_itab~get_xstring,
      move_to_itab FOR zif_itab~move_to_itab,
      get_extension FOR zif_itab~get_extension.

  PROTECTED SECTION.
    DATA mv_json TYPE xstring.

    METHODS set_itab
      CHANGING
        ct_itab TYPE ANY TABLE.
ENDCLASS.



CLASS ZCL_ITAB_JSON IMPLEMENTATION.


  METHOD set_itab.
    DATA(lv_json) = /ui2/cl_json=>serialize( data = ct_itab compress = abap_true ).
    mv_json =  cl_abap_codepage=>convert_to( lv_json ).
  ENDMETHOD.


  METHOD zif_itab~from_itab.
    DATA(lo_json) = NEW zcl_itab_json( ).
    lo_json->set_itab( CHANGING ct_itab = ct_itab ).
    ro_ret = lo_json.
  ENDMETHOD.


  METHOD zif_itab~from_xstring.
    DATA(lo_json) = NEW zcl_itab_json( ).
    lo_json->mv_json = iv_xstring.
    ro_ret = lo_json.
  ENDMETHOD.


  METHOD zif_itab~get_extension.
    rv_ext = '.json'.
  ENDMETHOD.


  METHOD zif_itab~get_xstring.
    rv_xstring = mv_json.
  ENDMETHOD.


  METHOD zif_itab~move_to_itab.
    CALL METHOD /ui2/cl_json=>deserialize
      EXPORTING
        jsonx = mv_json
      CHANGING
        data  = ct_itab.
  ENDMETHOD.
ENDCLASS.
