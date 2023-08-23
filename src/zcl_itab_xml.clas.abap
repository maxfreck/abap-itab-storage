CLASS zcl_itab_xml DEFINITION PUBLIC CREATE PROTECTED.
  PUBLIC SECTION.
    INTERFACES zif_itab.
    ALIASES:
      from_itab FOR zif_itab~from_itab,
      from_xstring FOR zif_itab~from_xstring,
      get_xstring FOR zif_itab~get_xstring,
      move_to_itab FOR zif_itab~move_to_itab,
      get_extension FOR zif_itab~get_extension.

  PROTECTED SECTION.
    DATA mv_xml TYPE xstring.

    METHODS set_itab
      CHANGING
        ct_itab TYPE ANY TABLE.
ENDCLASS.



CLASS ZCL_ITAB_XML IMPLEMENTATION.


  METHOD set_itab.
    CALL TRANSFORMATION id SOURCE model = ct_itab RESULT XML DATA(lv_xml).
    mv_xml =  lv_xml."cl_abap_codepage=>convert_to( lv_xml ).
  ENDMETHOD.


  METHOD zif_itab~from_itab.
    DATA(lo_xml) = NEW zcl_itab_xml( ).
    lo_xml->set_itab( CHANGING ct_itab = ct_itab ).
    ro_ret = lo_xml.
  ENDMETHOD.


  METHOD zif_itab~from_xstring.
    DATA(lo_xml) = NEW zcl_itab_xml( ).
    lo_xml->mv_xml = iv_xstring.
    ro_ret = lo_xml.
  ENDMETHOD.


  METHOD zif_itab~get_extension.
    rv_ext = '.xml'.
  ENDMETHOD.


  METHOD zif_itab~get_xstring.
    rv_xstring = mv_xml.
  ENDMETHOD.


  METHOD zif_itab~move_to_itab.
    CALL TRANSFORMATION id SOURCE XML mv_xml RESULT model = ct_itab.
  ENDMETHOD.
ENDCLASS.
