CLASS zcl_itab_cluster DEFINITION PUBLIC CREATE PROTECTED.
  PUBLIC SECTION.
    INTERFACES zif_itab.
    ALIASES:
      from_itab FOR zif_itab~from_itab,
      from_xstring FOR zif_itab~from_xstring,
      get_xstring FOR zif_itab~get_xstring,
      move_to_itab FOR zif_itab~move_to_itab,
      get_extension FOR zif_itab~get_extension.

  PROTECTED SECTION.
    DATA mv_bin TYPE xstring.

    METHODS set_itab
      CHANGING
        ct_itab TYPE ANY TABLE.
ENDCLASS.



CLASS zcl_itab_cluster IMPLEMENTATION.

  METHOD set_itab.
    EXPORT data = ct_itab TO DATA BUFFER mv_bin COMPRESSION ON.
  ENDMETHOD.


  METHOD zif_itab~from_itab.
    DATA(lo_cluster) = NEW zcl_itab_cluster( ).
    lo_cluster->set_itab( CHANGING ct_itab = ct_itab ).
    ro_ret = lo_cluster.
  ENDMETHOD.


  METHOD zif_itab~from_xstring.
    DATA(lo_cluster) = NEW zcl_itab_cluster( ).
    lo_cluster->mv_bin = iv_xstring.
    ro_ret = lo_cluster.
  ENDMETHOD.


  METHOD zif_itab~get_extension.
    rv_ext = '.bin'.
  ENDMETHOD.


  METHOD zif_itab~get_xstring.
    rv_xstring = mv_bin.
  ENDMETHOD.


  METHOD zif_itab~move_to_itab.
    IMPORT data = ct_itab FROM DATA BUFFER mv_bin.
  ENDMETHOD.
ENDCLASS.
