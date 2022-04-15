*&---------------------------------------------------------------------*
*&  Include           YFIN_LP_FM01_CLASS
*&---------------------------------------------------------------------*
CLASS lcl_alv_handle DEFINITION.

  PUBLIC SECTION.
    METHODS:
      handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING
            e_column_id
            es_row_no,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
            e_ucomm,
      handle_toolbar_set FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
            e_object
            e_interactive,
      call_transaction
        IMPORTING
          im_parameter TYPE tt_params
          im_tcode     TYPE sy-tcode.

ENDCLASS.
CLASS lcl_alv_handle IMPLEMENTATION.
  METHOD handle_hotspot_click.

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

  ENDMETHOD.                    "handle_hotspot_click
  METHOD handle_user_command.

    CASE e_ucomm.
      WHEN '&RECALL'.
        DATA: _answer TYPE char1.
        PERFORM popup_confirm USING TEXT-t01 TEXT-t02 CHANGING _answer.
        IF _answer EQ '1'.
          _fm01dat-ev_action = e_ucomm.
          SET SCREEN 0.
          LEAVE SCREEN.
        ENDIF.
    ENDCASE.

  ENDMETHOD.                    "handle_user_command
  METHOD handle_toolbar_set.

    CASE _pressed_tab.
      WHEN '9200'.
        IF _fm01dat-iv_tunit EQ 'D' AND _fm01dat-t_alvdat_tab2[] IS NOT INITIAL.
          APPEND LINES OF VALUE ttb_button( ( function = '&RECALL'
                                              icon = icon_status_reverse
                                              text = 'Geri Çek'
                                              quickinfo = 'Geri Çek' ) ) TO e_object->mt_toolbar.
        ENDIF.
    ENDCASE.

  ENDMETHOD.                    "handle_toolbar_set
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
