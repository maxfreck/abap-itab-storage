CLASS zcl_itab_xlsx DEFINITION PUBLIC CREATE PROTECTED.
  PUBLIC SECTION.
    INTERFACES zif_itab.
    ALIASES:
      from_itab FOR zif_itab~from_itab,
      from_xstring FOR zif_itab~from_xstring,
      get_xstring FOR zif_itab~get_xstring,
      move_to_itab FOR zif_itab~move_to_itab,
      get_extension FOR zif_itab~get_extension.

  PROTECTED SECTION.
    DATA mv_xlsx TYPE xstring.

    METHODS set_itab
      CHANGING
        ct_itab TYPE ANY TABLE.
private section.
ENDCLASS.



CLASS ZCL_ITAB_XLSX IMPLEMENTATION.


  METHOD set_itab.
    DATA lt_fcat TYPE lvc_t_fcat.
    DATA ls_layout TYPE lvc_s_layo.
    DATA lv_version TYPE string.
    DATA lo_result_data TYPE REF TO cl_salv_ex_result_data_table.
    DATA lo_columns  TYPE REF TO cl_salv_columns_table.
    DATA lo_aggreg   TYPE REF TO cl_salv_aggregations.
    DATA lo_salv_table  TYPE REF TO cl_salv_table.

    DATA(lt_data) = REF #( ct_itab ).

    TRY.
        cl_salv_table=>factory(
          EXPORTING
            list_display = abap_false
          IMPORTING
            r_salv_table = lo_salv_table
          CHANGING
            t_table      = ct_itab ).
      CATCH cx_salv_msg.
        "#TODO: error handling
    ENDTRY.

    lo_columns  = lo_salv_table->get_columns( ).
    lo_aggreg   = lo_salv_table->get_aggregations( ).
    lt_fcat     = cl_salv_controller_metadata=>get_lvc_fieldcatalog( r_columns = lo_columns
                                                                     r_aggregations = lo_aggreg ).


    IF  cl_salv_bs_a_xml_base=>get_version( ) <> if_salv_bs_xml=>version_25
    AND cl_salv_bs_a_xml_base=>get_version( ) <> if_salv_bs_xml=>version_26.
      "#TODO: error handling
      RETURN.
    ENDIF.
    lo_result_data
      = cl_salv_ex_util=>factory_result_data_table( r_data         = lt_data
                                                    s_layout       = ls_layout
                                                    t_fieldcatalog = lt_fcat ).
    CASE cl_salv_bs_a_xml_base=>get_version( ).
      WHEN if_salv_bs_xml=>version_25.
        lv_version = if_salv_bs_xml=>version_25.
      WHEN if_salv_bs_xml=>version_26.
        lv_version = if_salv_bs_xml=>version_26.
    ENDCASE.

    CALL METHOD cl_salv_bs_tt_util=>if_salv_bs_tt_util~transform
      EXPORTING
        xml_type      = if_salv_bs_xml=>c_type_xlsx
        xml_version   = lv_version
        r_result_data = lo_result_data
        xml_flavour   = if_salv_bs_c_tt=>c_tt_xml_flavour_export
        gui_type      = if_salv_bs_xml=>c_gui_type_gui
      IMPORTING
        xml           = mv_xlsx.
  ENDMETHOD.


  METHOD zif_itab~from_itab.
    DATA(lo_xlsx) = NEW zcl_itab_xlsx( ).
    lo_xlsx->set_itab( CHANGING ct_itab = ct_itab ).
    ro_ret = lo_xlsx.
  ENDMETHOD.


  METHOD zif_itab~from_xstring.
    DATA(lo_xlsx) = NEW zcl_itab_xlsx( ).
    lo_xlsx->mv_xlsx = iv_xstring.
    ro_ret = lo_xlsx.
  ENDMETHOD.


  METHOD zif_itab~get_extension.
    rv_ext = '.xlsx'.
  ENDMETHOD.


  METHOD zif_itab~get_xstring.
    rv_xstring = mv_xlsx.
  ENDMETHOD.


  METHOD zif_itab~move_to_itab.
    DATA(lo_excel) =
      NEW cl_fdt_xl_spreadsheet( document_name = ''
                                 xdocument     = mv_xlsx ).

    lo_excel->if_fdt_doc_spreadsheet~get_worksheet_names(
      IMPORTING worksheet_names = DATA(lt_worksheets) ).

    READ TABLE lt_worksheets INDEX 1 INTO DATA(lv_name).
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    DATA(lt_excel) = lo_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet( lv_name ).

    FIELD-SYMBOLS <lt_excel> TYPE STANDARD TABLE.

    "Столбцы мэпятся 1 к 1 по очереди, первая строка с заголовками удаляется
    ASSIGN lt_excel->* TO <lt_excel>.
    DELETE <lt_excel> INDEX 1.

    DATA(lt_dst_comp) = CAST cl_abap_structdescr(
                          CAST cl_abap_tabledescr(
                            cl_abap_typedescr=>describe_by_data( ct_itab )
                          )->get_table_line_type( )
                        )->components.

    DATA(lt_src_comp) = CAST cl_abap_structdescr(
                          CAST cl_abap_tabledescr(
                            cl_abap_typedescr=>describe_by_data( <lt_excel> )
                          )->get_table_line_type( )
                        )->components.

    LOOP AT <lt_excel> ASSIGNING FIELD-SYMBOL(<ls_src_line>).
      APPEND INITIAL LINE TO ct_itab ASSIGNING FIELD-SYMBOL(<ls_dst_line>).
      LOOP AT lt_dst_comp ASSIGNING FIELD-SYMBOL(<ls_dst_comp>).            "#EC CI_NESTED
        ASSIGN lt_src_comp[ sy-tabix ] TO FIELD-SYMBOL(<ls_src_comp>).
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        ASSIGN COMPONENT <ls_dst_comp>-name OF STRUCTURE <ls_dst_line> TO FIELD-SYMBOL(<lv_dst>).
        IF sy-subrc <> 0.
          "#TODO: error handling
          CONTINUE.
        ENDIF.
        ASSIGN COMPONENT <ls_src_comp>-name OF STRUCTURE <ls_src_line> TO FIELD-SYMBOL(<lv_src>).
        IF sy-subrc <> 0.
          "#TODO: error handling
          CONTINUE.
        ENDIF.

        <lv_dst> = <lv_src>.

      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
