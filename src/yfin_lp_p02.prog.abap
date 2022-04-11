*&---------------------------------------------------------------------*
*& Program YFIN_LP_P02
*&---------------------------------------------------------------------*
*& @author Abdullah ATAN <abdullah.atan@dataliva.com.tr>
*&---------------------------------------------------------------------*
PROGRAM yfin_lp_p02.

INCLUDE yfin_lp_p02_clsdat.
INCLUDE yfin_lp_p02_frmdat.

LOAD-OF-PROGRAM.
  _controller = NEW lcl_controller( )->instantiate_app(
    EXPORTING
      iv_model = lcl_controller=>mc_model
      iv_view  = lcl_controller=>mc_view ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  _controller->at_selection_screen_request(
    EXPORTING
      iv_fldnam = _controller->mc_cons-p_fname ).

START-OF-SELECTION.
  _controller->mo_model->retrieve_dat(
    EXPORTING
      iv_bukrs = p_bukrs
      iv_fname = p_fname ).

END-OF-SELECTION.
  _controller->alv_session(
    EXCEPTIONS
      contains_error = 1
      OTHERS         = 2 ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
