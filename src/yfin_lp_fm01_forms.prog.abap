*&---------------------------------------------------------------------*
*&  Include           YFIN_LP_FM01_FORMS
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F9001_OBJECTS_CREATE
*&---------------------------------------------------------------------*
FORM f_objects_create.

  CREATE OBJECT custom_container1
    EXPORTING
      container_name              = cont_for_cognos1
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CREATE OBJECT custom_container2
    EXPORTING
      container_name              = cont_for_cognos2
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  f_build_field_catalog
*&---------------------------------------------------------------------*
FORM f_build_field_catalog TABLES p_fieldcat STRUCTURE lvc_s_fcat
                            USING VALUE(p_structure).

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = p_structure
    CHANGING
      ct_fieldcat            = p_fieldcat[]
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF NOT sy-subrc IS INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  LOOP AT p_fieldcat REFERENCE INTO DATA(_fcat).
    CASE _fcat->fieldname.
      WHEN 'BELNR'.
        _fcat->hotspot = abap_true.
    ENDCASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  f_set_layout
*&---------------------------------------------------------------------*
FORM f_set_layout USING VALUE(ptitle)
                        VALUE(pzebra)
                        VALUE(pmode)
                        VALUE(pwidth).

  gv_layout-grid_title = ptitle.
  gv_layout-zebra = pzebra.
  gv_layout-sel_mode = pmode.
  gv_layout-cwidth_opt = pwidth.
  gv_variant-report = sy-repid.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  popup_confirm
*&---------------------------------------------------------------------*
FORM popup_confirm USING pv_titlebar pv_question CHANGING cv_answer TYPE char1.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = pv_titlebar
      text_question         = pv_question
      text_button_1         = TEXT-d01
      text_button_2         = TEXT-d02
      popup_type            = 'ICON_MESSAGE_CRITICAL'
      default_button        = '2'
      display_cancel_button = abap_true
    IMPORTING
      answer                = cv_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

ENDFORM.
