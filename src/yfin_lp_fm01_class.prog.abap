*&---------------------------------------------------------------------*
*&  Include           YFIN_LP_FM01_CLASS
*&---------------------------------------------------------------------*
CLASS lcl_alv_handle DEFINITION.

  PUBLIC SECTION.
    METHODS:
      hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING
            e_column_id
            es_row_no,
      call_transaction
        IMPORTING
          im_parameter TYPE tt_params
          im_tcode     TYPE sy-tcode.

ENDCLASS.
CLASS lcl_alv_handle IMPLEMENTATION.
  METHOD hotspot_click.

    CASE e_column_id.
      WHEN 'BELNR'.
        READ TABLE _fm01dat-t_alvdat_tab1 REFERENCE INTO DATA(_tab1) INDEX es_row_no-row_id.
        IF sy-subrc IS INITIAL.
          me->call_transaction(
             EXPORTING
               im_tcode     = CONV #('FB03')
               im_parameter = CONV #( VALUE #( ( param_id = 'BLN' value = _tab1->belnr )
                                               ( param_id = 'BUK' value = _tab1->bukrs )
                                               ( param_id = 'GJR' value = _tab1->gjahr ) ) ) ).
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.                    "hotspot_click
  METHOD call_transaction.

    LOOP AT im_parameter INTO DATA(wa_param).
      SET PARAMETER ID wa_param-param_id FIELD wa_param-value.
    ENDLOOP.
    IF sy-subrc IS INITIAL AND
       im_tcode IS NOT INITIAL.
      CALL TRANSACTION im_tcode AND SKIP FIRST SCREEN.
    ENDIF.

  ENDMETHOD.                    "call_transaction
ENDCLASS.
