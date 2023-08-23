CLASS zcl_itab_ps_storage DEFINITION PUBLIC CREATE PUBLIC INHERITING FROM zcl_itab_storage.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        iv_prefix TYPE string.

    METHODS zif_itab_storage~get_next_file_name REDEFINITION.
    METHODS zif_itab_storage~store REDEFINITION.
    METHODS zif_itab_storage~download REDEFINITION.
    METHODS zif_itab_storage~download_to_itab REDEFINITION.

  PROTECTED SECTION.
    DATA:
      mv_prefix    TYPE string,
      mv_file_name TYPE string.

    METHODS file_open_dialog
      RETURNING
        VALUE(rv_file_name) TYPE string.

    METHODS file_save_dialog
      IMPORTING
        iv_extension        TYPE string DEFAULT ''
      RETURNING
        VALUE(rv_file_name) TYPE string.

ENDCLASS.



CLASS ZCL_ITAB_PS_STORAGE IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    mv_prefix = iv_prefix.
    get_next_file_name( ).
  ENDMETHOD.


  METHOD file_open_dialog.
    DATA:
      lt_filetable TYPE filetable,
      lv_rc        TYPE i,
      lv_uaction   TYPE i.

    cl_gui_frontend_services=>file_open_dialog(
      CHANGING
        file_table              = lt_filetable
        rc                      = lv_rc
        user_action             = lv_uaction
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5 ).

    rv_file_name = VALUE #( lt_filetable[ 1 ]-filename DEFAULT '' ).
  ENDMETHOD.


  METHOD file_save_dialog.
    DATA lv_path TYPE string.
    DATA lv_filename TYPE string.

    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        file_filter = iv_extension
        default_file_name = get_next_file_name( ) && iv_extension
      CHANGING
        path              = lv_path
        filename          = lv_filename
        fullpath          = rv_file_name
      EXCEPTIONS
        OTHERS            = 0 ).
  ENDMETHOD.


  METHOD zif_itab_storage~download.
    MESSAGE e410(00).
  ENDMETHOD.


  METHOD zif_itab_storage~download_to_itab.
    DATA lv_buffer TYPE xstring.
    DATA lt_filetab TYPE w3mimetabtype.
    DATA lv_filesize TYPE i.


    DATA(lv_file_name) = file_open_dialog( ).
    IF lv_file_name IS INITIAL.
      RETURN.
    ENDIF.

    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename   = lv_file_name
        filetype   = 'BIN'
      IMPORTING
        filelength = lv_filesize
      CHANGING
        data_tab   = lt_filetab
      EXCEPTIONS
        OTHERS     = 4.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lv_buffer = cl_bcs_convert=>solix_to_xstring( it_solix = lt_filetab ).
    DATA(lv_extension) = get_file_extension( lv_file_name ).

    ro_itab = create_itab_by_ext( iv_ext = lv_extension
                                  iv_buffer = lv_buffer ).
  ENDMETHOD.


  METHOD zif_itab_storage~get_next_file_name.
    IF mv_file_name IS INITIAL
    OR iv_renew = abap_true.
      mv_file_name = |{ mv_prefix }_{ sy-uname }_{ sy-datum }_{ sy-uzeit }|.
    ENDIF.

    rv_file_name = mv_file_name.
  ENDMETHOD.


  METHOD zif_itab_storage~store.
    DATA lv_size TYPE i.
    DATA lv_bintab TYPE solix_tab.

    DATA(lv_filename) = file_save_dialog( io_itab->get_extension( ) ).
    IF lv_filename IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_buffer) = io_itab->get_xstring( ).

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = lv_buffer
      IMPORTING
        output_length = lv_size
      TABLES
        binary_tab    = lv_bintab.

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize              = lv_size
        filename                  = lv_filename
        filetype                  = 'BIN'
      CHANGING
        data_tab                  = lv_bintab
      EXCEPTIONS
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        not_supported_by_gui      = 22
        error_no_gui              = 23
        OTHERS                    = 24 ).
  ENDMETHOD.
ENDCLASS.
