CLASS zcl_itab_cluster_storage  DEFINITION PUBLIC CREATE PUBLIC INHERITING FROM zcl_itab_storage.
  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        iv_prefix TYPE char3.

    METHODS zif_itab_storage~get_next_file_name REDEFINITION.
    METHODS zif_itab_storage~store REDEFINITION.
    METHODS zif_itab_storage~delete REDEFINITION.
    METHODS zif_itab_storage~download REDEFINITION.
    METHODS zif_itab_storage~download_to_itab REDEFINITION.

  PROTECTED SECTION.

    DATA mv_prefix TYPE string .
    DATA mv_file_name TYPE string .

    METHODS cluster_file_name_dialog
      RETURNING
        VALUE(rv_file_name) TYPE string .
ENDCLASS.



CLASS zcl_itab_cluster_storage IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_prefix = iv_prefix.
    get_next_file_name( ).
  ENDMETHOD.

  METHOD cluster_file_name_dialog.
    DATA lt_files TYPE STANDARD TABLE OF epsfili.
    DATA lv_choise TYPE sy-tabix.
    DATA lv_canceled TYPE abap_bool.
    DATA(lv_prefix) = mv_prefix && '%'.

    SELECT
      srtfd AS name,
      clustr AS size
      FROM indx
     WHERE relid = 'ZI'
       AND srtfd LIKE @lv_prefix
      INTO CORRESPONDING FIELDS OF TABLE @lt_files.

    CALL FUNCTION 'SRM_SELECTION_POPUP_WITH_ALV'
      EXPORTING
        im_title            = TEXT-001
        im_tabname          = 'EPSFILI'
        im_startx           = 5
        im_stopx            = 100
        im_starty           = 1
        im_stopy            = 10
      IMPORTING
        ex_selected_line    = lv_choise
        ex_canceled_by_user = lv_canceled
      CHANGING
        lt_outtab           = lt_files.
    IF lv_choise IS INITIAL
    OR lv_canceled = abap_true.
      RETURN.
    ENDIF.

    rv_file_name = lt_files[ lv_choise ]-name.
  ENDMETHOD.


  METHOD zif_itab_storage~download.

    DATA:
      lv_filename    TYPE string,
      lv_path        TYPE string,
      lv_fullpath    TYPE string,
      lv_user_action TYPE i,
      lv_size        TYPE i,
      lv_buffer      TYPE xstring,
      lt_content     TYPE STANDARD TABLE OF soli.

    DATA(lv_cluster_file_name) = CONV char22( cluster_file_name_dialog( ) ).
    IF lv_cluster_file_name IS INITIAL.
      RETURN.
    ENDIF.

    CALL METHOD cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        default_extension = 'xlsx'
        default_file_name = CONV string( lv_cluster_file_name )
      CHANGING
        filename          = lv_filename
        path              = lv_path
        fullpath          = lv_fullpath
        user_action       = lv_user_action ).
    IF lv_user_action <> 0.
      RETURN.
    ENDIF.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IMPORT buffer = lv_buffer FROM DATABASE indx(zi) ID lv_cluster_file_name.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = lv_buffer
      IMPORTING
        output_length = lv_size
      TABLES
        binary_tab    = lt_content.

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        filetype     = 'BIN'
        bin_filesize = lv_size
        filename     = lv_fullpath
      CHANGING
        data_tab     = lt_content ).
  ENDMETHOD.


  METHOD zif_itab_storage~download_to_itab.
    DATA lv_buffer TYPE xstring.

    DATA(lv_cluster_file_name) = CONV char22( cluster_file_name_dialog( ) ).
    IF lv_cluster_file_name IS INITIAL.
      RETURN.
    ENDIF.

    IMPORT buffer = lv_buffer FROM DATABASE indx(zi) ID lv_cluster_file_name.

    DATA(lv_extension) = get_file_extension( CONV string( lv_cluster_file_name ) ).

    ro_itab = create_itab_by_ext( iv_ext = lv_extension
                                  iv_buffer = lv_buffer ).
  ENDMETHOD.


  METHOD zif_itab_storage~get_next_file_name.
    IF mv_file_name IS INITIAL
    OR iv_renew = abap_true.
      mv_file_name = |{ mv_prefix }{ sy-datum }{ sy-uzeit }|.
    ENDIF.

    rv_file_name = mv_file_name.
  ENDMETHOD.


  METHOD zif_itab_storage~store.
    DATA(lv_filename) = CONV char22( get_next_file_name( ) && io_itab->get_extension( ) ).

    DATA(lv_buffer) = io_itab->get_xstring( ).

    EXPORT buffer = lv_buffer TO DATABASE indx(zi) ID lv_filename.
  ENDMETHOD.

  METHOD zif_itab_storage~delete.
    DATA(lv_cluster_file_name) = CONV char22( cluster_file_name_dialog( ) ).
    IF lv_cluster_file_name IS INITIAL.
      RETURN.
    ENDIF.

    DELETE FROM DATABASE indx(zi) ID lv_cluster_file_name.
  ENDMETHOD.
ENDCLASS.
