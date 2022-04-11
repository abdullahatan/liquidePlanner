*&---------------------------------------------------------------------*
*&  Include           YFIN_LP_P01_TOPDAT
*&---------------------------------------------------------------------*
TYPE-POOLS: slis, icon.

TABLES: sscrfields, bkpf.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-s01.
PARAMETERS:
  p_bukrs TYPE t001-bukrs OBLIGATORY DEFAULT '1073'.
SELECT-OPTIONS:
  s_keydat FOR bkpf-budat NO-EXTENSION OBLIGATORY.
PARAMETERS:
  p_ldays TYPE int1 DEFAULT '120'.
SELECTION-SCREEN SKIP 1.
PARAMETERS:
  p_tunit TYPE char1 AS LISTBOX VISIBLE LENGTH 10 DEFAULT 'D' OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

TYPES:
  BEGIN OF ty_param,
    param_id TYPE memoryid,
    value    TYPE char100,
  END  OF ty_param,
  tt_params TYPE TABLE OF ty_param.

TYPES: tt_keydat_rng TYPE RANGE OF datum.

CLASS lcl_mvc_model DEFINITION DEFERRED.
CLASS lcl_mvc_view DEFINITION DEFERRED.
CLASS lcl_mvc_controller DEFINITION DEFERRED.

DATA _controller TYPE REF TO lcl_mvc_controller.
