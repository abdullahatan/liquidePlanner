FUNCTION-POOL yfin_lp_fg01.                 "MESSAGE-ID ..

* INCLUDE LYFIN_LP_FG01D...                  " Local class definition

TYPES: BEGIN OF ty_fm01dat,
         iv_strname_tab1 TYPE tabname,
         iv_strname_tab2 TYPE tabname,
         t_alvdat_tab1   TYPE yfin_lp_tt01,
         t_alvdat_tab2   TYPE yfin_lp_tt02,
       END OF ty_fm01dat.

DATA: _fm01dat TYPE ty_fm01dat.
