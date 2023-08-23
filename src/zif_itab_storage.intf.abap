INTERFACE zif_itab_storage
  PUBLIC .


  METHODS get_next_file_name
    IMPORTING
      !iv_renew           TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(rv_file_name) TYPE string .

  METHODS store
    IMPORTING
      !io_itab TYPE REF TO zif_itab .

  METHODS download.

  METHODS download_to_itab
    RETURNING
      VALUE(ro_itab) TYPE REF TO zif_itab.

ENDINTERFACE.
