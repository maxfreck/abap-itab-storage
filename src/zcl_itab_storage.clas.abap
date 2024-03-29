CLASS zcl_itab_storage DEFINITION PUBLIC ABSTRACT CREATE PUBLIC .
  PUBLIC SECTION.
    INTERFACES zif_itab_storage.
    ALIASES:
      get_next_file_name FOR zif_itab_storage~get_next_file_name,
      store FOR zif_itab_storage~store,
      delete FOR zif_itab_storage~delete,
      download FOR zif_itab_storage~download,
      download_to_itab FOR zif_itab_storage~download_to_itab.

    METHODS constructor.

  PROTECTED SECTION.
    TYPES:
      BEGIN OF mty_s_itab,
        extension TYPE string,
        class     TYPE seoclsname,
      END OF mty_s_itab,
      mty_t_itab TYPE SORTED TABLE OF mty_s_itab WITH UNIQUE KEY extension.

    DATA mt_itab_by_ext TYPE mty_t_itab.

    METHODS load_itab_implementations.

    METHODS get_file_extension
      IMPORTING
        iv_file_name  TYPE string
      RETURNING
        VALUE(rv_ext) TYPE string.

    METHODS create_itab_by_ext
      IMPORTING
        iv_ext         TYPE string
        iv_buffer      TYPE xstring
      RETURNING
        VALUE(ro_itab) TYPE REF TO zif_itab.

private section.
ENDCLASS.



CLASS ZCL_ITAB_STORAGE IMPLEMENTATION.


  METHOD constructor.
    load_itab_implementations( ).
  ENDMETHOD.


  METHOD create_itab_by_ext.
    ASSIGN mt_itab_by_ext[ extension = iv_ext ] TO FIELD-SYMBOL(<ls_itab>).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL METHOD (<ls_itab>-class)=>from_xstring
      EXPORTING
        iv_xstring = iv_buffer
      RECEIVING
        ro_ret     = ro_itab.
  ENDMETHOD.


  METHOD get_file_extension.
    DATA lv_l TYPE i.
    rv_ext = iv_file_name.
    lv_l = strlen( rv_ext ) - 1.
    WHILE lv_l >= 0 AND rv_ext+lv_l(1) <> '.'.
      lv_l = lv_l - 1.
    ENDWHILE.
    IF lv_l < 1.
      rv_ext = ''.   " '.blah' retuns ''
    ELSE.
      rv_ext = rv_ext+lv_l(*).       " 'foo.bar1.bar2' returns 'bar2'
    ENDIF.

    rv_ext = to_upper( rv_ext ).
  ENDMETHOD.


  METHOD load_itab_implementations.
    TRY.
        DATA(lo_oo_intf) = CAST cl_oo_interface( cl_oo_interface=>get_instance( 'ZIF_ITAB' ) ).
        DATA(lt_classes) = lo_oo_intf->get_implementing_classes( ).
        DATA lv_ext TYPE string.
      CATCH cx_root.
        "#TODO: error handling
        RETURN.
    ENDTRY.

    LOOP AT lt_classes ASSIGNING FIELD-SYMBOL(<ls_class>).
      CLEAR lv_ext.
      CALL METHOD (<ls_class>-clsname)=>get_extension RECEIVING rv_ext = lv_ext.
      IF lv_ext IS INITIAL.
        CONTINUE.
      ENDIF.
      lv_ext = to_upper( lv_ext ).
      INSERT VALUE #( extension = lv_ext
                      class = <ls_class>-clsname ) INTO TABLE mt_itab_by_ext.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_itab_storage~download.
  ENDMETHOD.


  METHOD zif_itab_storage~download_to_itab.
  ENDMETHOD.


  METHOD zif_itab_storage~get_next_file_name.
  ENDMETHOD.


  METHOD zif_itab_storage~store.
  ENDMETHOD.

  METHOD zif_itab_storage~delete.
  ENDMETHOD.
ENDCLASS.
