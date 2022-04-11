*&---------------------------------------------------------------------*
*&  Include           YFIN_LP_FM01_DATA
*&---------------------------------------------------------------------*

*---------------------------------------------------------------------*
* Variables->
*---------------------------------------------------------------------*

TYPES: BEGIN OF ty_param,
         param_id TYPE memoryid,
         value    TYPE char100,
       END  OF ty_param,
       tt_params TYPE TABLE OF ty_param WITH DEFAULT KEY.

DATA : t_fieldcat_tab1   TYPE lvc_t_fcat,
       t_fieldcat_tab2   TYPE lvc_t_fcat,
       o_alvgrid1        TYPE REF TO cl_gui_alv_grid,
       o_alvgrid2        TYPE REF TO cl_gui_alv_grid,
       cont_for_cognos1  TYPE scrfname VALUE 'BCALC_GRID_01_9100',
       cont_for_cognos2  TYPE scrfname VALUE 'BCALC_GRID_01_9200',
       custom_container1 TYPE REF TO cl_gui_custom_container,
       custom_container2 TYPE REF TO cl_gui_custom_container,
       gv_layout         TYPE lvc_s_layo,
       gv_variant        TYPE disvariant.

*---------------------------------------------------------------------*
* Constants->
*---------------------------------------------------------------------*
CONSTANTS : c_lay(1) TYPE c VALUE 'A'.
CONSTANTS: BEGIN OF c_main_tab,
             tab1 LIKE sy-ucomm VALUE 'MAIN_TAB_FC1', "
             tab2 LIKE sy-ucomm VALUE 'MAIN_TAB_FC2', "
           END OF c_main_tab.
