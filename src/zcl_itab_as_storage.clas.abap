CLASS zcl_itab_as_storage DEFINITION PUBLIC CREATE PUBLIC INHERITING FROM zcl_itab_storage.
  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        iv_prefix TYPE string.

    METHODS zif_itab_storage~get_next_file_name REDEFINITION.
    METHODS zif_itab_storage~store REDEFINITION.
    METHODS zif_itab_storage~download REDEFINITION.
    METHODS zif_itab_storage~download_to_itab REDEFINITION.

  PROTECTED SECTION.

    DATA mv_prefix TYPE string .
    DATA mv_file_name TYPE string .

    METHODS get_as_dir
      RETURNING
        VALUE(rv_dir) TYPE string .

    METHODS server_file_name_dialog
      RETURNING
        VALUE(rv_file_name) TYPE string .
ENDCLASS.



CLASS ZCL_ITAB_AS_STORAGE IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    mv_prefix = iv_prefix.
    get_next_file_name( ).
  ENDMETHOD.


  METHOD get_as_dir.
    DATA lv_dirname TYPE dirname_al11.

    CALL 'C_SAPGPARAM' ID 'NAME'  FIELD 'DIR_TEMP'        "#EC CI_CCALL
                       ID 'VALUE' FIELD lv_dirname.

    IF lv_dirname IS INITIAL.
      lv_dirname = '/temp'.
    ENDIF.

    rv_dir = lv_dirname && `/`.
  ENDMETHOD.


  METHOD server_file_name_dialog.
    DATA lt_files TYPE STANDARD TABLE OF epsfili.
    DATA lv_choise TYPE sy-tabix.
    DATA lv_canceled TYPE abap_bool.

    CALL FUNCTION 'EPS_GET_DIRECTORY_LISTING'
      EXPORTING
        dir_name               = CONV epsf-epsdirnam( get_as_dir( ) )
        file_mask              = CONV epsf-epsfilnam( |{ mv_prefix }*| )
      TABLES
        dir_list               = lt_files
      EXCEPTIONS
        invalid_eps_subdir     = 1
        sapgparam_failed       = 2
        build_directory_failed = 3
        no_authorization       = 4
        read_directory_failed  = 5
        too_many_read_errors   = 6
        empty_directory_list   = 7
        OTHERS                 = 99.
    IF sy-subrc <> 0
    OR lt_files IS INITIAL.
      RETURN.
    ENDIF.

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
      lv_user_action TYPE i.

    DATA(lv_server_file_name) = server_file_name_dialog( ).
    IF lv_server_file_name IS INITIAL.
      RETURN.
    ENDIF.

    CALL METHOD cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        default_extension = 'xlsx'
        default_file_name = lv_server_file_name
      CHANGING
        filename          = lv_filename
        path              = lv_path
        fullpath          = lv_fullpath
        user_action       = lv_user_action ).
    IF lv_user_action <> 0.
      RETURN.
    ENDIF.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA:
      lv_buffer  TYPE xstring,
      lt_content TYPE STANDARD TABLE OF soli,
      lv_size    TYPE i.

    DATA(lv_full_server_filename) = get_as_dir( ) && lv_server_file_name.

    OPEN DATASET lv_full_server_filename FOR INPUT IN BINARY MODE.
    READ DATASET lv_full_server_filename INTO lv_buffer.
    CLOSE DATASET lv_full_server_filename.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = lv_buffer
      IMPORTING
        output_length = lv_size
      TABLES
        binary_tab    = lt_content.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filetype     = 'BIN'
        bin_filesize = lv_size
        filename     = lv_fullpath
      TABLES
        data_tab     = lt_content.
  ENDMETHOD.


  METHOD zif_itab_storage~download_to_itab.
    DATA lv_buffer TYPE xstring.

    DATA(lv_server_file_name) = server_file_name_dialog( ).
    IF lv_server_file_name IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_full_server_filename) = get_as_dir( ) && lv_server_file_name.

    OPEN DATASET lv_full_server_filename FOR INPUT IN BINARY MODE.
    READ DATASET lv_full_server_filename INTO lv_buffer.
    CLOSE DATASET lv_full_server_filename.

    DATA(lv_extension) = get_file_extension( lv_server_file_name ).

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
    DATA(lv_filename) = get_as_dir( ) && get_next_file_name( ) && io_itab->get_extension( ).

    DATA(lv_buffer) = io_itab->get_xstring( ).

    OPEN DATASET lv_filename  FOR OUTPUT IN BINARY MODE.
    TRANSFER lv_buffer TO lv_filename.
    CLOSE DATASET lv_filename.
  ENDMETHOD.
ENDCLASS.
