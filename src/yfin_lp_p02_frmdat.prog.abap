*&---------------------------------------------------------------------*
*& Include          YFIN_LP_P02_FRMDAT
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
FORM user_command USING v_ucomm  LIKE sy-ucomm
                        v_selfld TYPE slis_selfield.

  CASE v_ucomm.
    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN '&RUNDAT'.
      _controller->display_logdat(
        EXPORTING
          iv_logdat = _controller->rundat_session( ) ).
    WHEN '&IC1'.
      CASE v_selfld-fieldname.
        WHEN 'MSGSHW'.
          READ TABLE _controller->mo_model->mt_outdat REFERENCE INTO DATA(_outdat) INDEX v_selfld-tabindex.
          IF sy-subrc IS INITIAL.
            _controller->display_logdat(
              EXPORTING
                iv_logdat = _outdat->msgdat ).
          ENDIF.
      ENDCASE.
  ENDCASE.

  v_selfld-refresh = abap_true.
  v_selfld-row_stable = abap_true.

ENDFORM .                    "user_command
*&---------------------------------------------------------------------*
*&      Form  SET_PF_STATUS
*&---------------------------------------------------------------------*
FORM set_pf_status USING p_status.
  SET PF-STATUS 'STANDARD' EXCLUDING _view->mt_exdat.
ENDFORM .                    "set_pf_status
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM top_of_page.

  DATA: d_heading    TYPE slis_t_listheader.

  SELECT SINGLE name_text FROM v_usr_name INTO @DATA(name_text) WHERE bname = @sy-uname.

  d_heading = VALUE #( ( typ = 'H' info = sy-title )
                       ( typ = 'S' info = |Kulanıcı Adı : { name_text }| )
                       ( typ = 'S' info = |Tarih / Saat : { sy-datum DATE = USER } / { sy-uzeit TIME = USER }| )
                       ( typ = 'S' info = |Kayıt Sayısı : { lines( _model->mt_outdat ) }| ) ).

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = d_heading.

ENDFORM .                    "top_of_page
