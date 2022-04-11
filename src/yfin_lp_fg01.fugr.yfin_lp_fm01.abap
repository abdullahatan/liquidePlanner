FUNCTION yfin_lp_fm01.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_STRNAME_TAB1) TYPE  TABNAME DEFAULT 'YFIN_LP_S01'
*"     REFERENCE(IV_STRNAME_TAB2) TYPE  TABNAME DEFAULT 'YFIN_LP_S02'
*"  TABLES
*"      T_ALVDAT_TAB1 TYPE  YFIN_LP_TT01
*"      T_ALVDAT_TAB2 TYPE  YFIN_LP_TT02
*"----------------------------------------------------------------------

  FREE: _fm01dat.
  _fm01dat = VALUE #( iv_strname_tab1 = iv_strname_tab1 iv_strname_tab2 = iv_strname_tab2 t_alvdat_tab1 = t_alvdat_tab1[] t_alvdat_tab2 = t_alvdat_tab2[] ).
  CALL SCREEN 9001 STARTING AT 20 8 ENDING AT 162 34.


ENDFUNCTION.
