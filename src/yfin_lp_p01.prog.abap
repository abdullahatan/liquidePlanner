*&---------------------------------------------------------------------*
*& Program YFIN_LP_P01
*&---------------------------------------------------------------------*
*& @author Abdullah ATAN <abdullah.atan@dataliva.com.tr>
*&---------------------------------------------------------------------*
PROGRAM yfin_lp_p01.

INCLUDE yfin_lp_p01_topdat.
INCLUDE yfin_lp_p01_clsdat.
INCLUDE yfin_lp_p01_scrdat.

LOAD-OF-PROGRAM.
  _controller = NEW lcl_mvc_controller( )->instantiate_app(
    EXPORTING
      iv_model = lcl_mvc_controller=>mc_model
      iv_view  = lcl_mvc_controller=>mc_view ).

INITIALIZATION.
  _controller->initialization( ).

AT SELECTION-SCREEN.
  _controller->at_selection_screen( ).

START-OF-SELECTION.
  _controller->start_of_selection( ).
