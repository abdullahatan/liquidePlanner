*&---------------------------------------------------------------------*
*&  Include           YFIN_LP_FM01_SCREEN
*&---------------------------------------------------------------------*
CONTROLS: main_tab TYPE TABSTRIP.
DATA: BEGIN OF i_main_tab,
        subscreen   LIKE sy-dynnr,
        prog        LIKE sy-repid VALUE sy-repid,
        pressed_tab LIKE sy-ucomm VALUE c_main_tab-tab1,
      END OF i_main_tab.

DATA: lo_handle TYPE REF TO lcl_alv_handle.

*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_9001 OUTPUT.

  SET PF-STATUS 'ZSTATUS'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CREATE_ALVDAT  OUTPUT
*&---------------------------------------------------------------------*
MODULE create_alvdat OUTPUT.

  IF custom_container1 IS INITIAL.
    PERFORM f_objects_create.
    PERFORM f_build_field_catalog TABLES t_fieldcat_tab1 USING _fm01dat-iv_strname_tab1.
    PERFORM f_build_field_catalog TABLES t_fieldcat_tab2 USING _fm01dat-iv_strname_tab2.
    PERFORM f_set_layout USING sy-title 'X' 'B' 'X'.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  MAIN_TAB_ACTIVE_TAB_SET  OUTPUT
*&---------------------------------------------------------------------*
MODULE main_tab_active_tab_set OUTPUT.

  main_tab-activetab = i_main_tab-pressed_tab.
  CASE i_main_tab-pressed_tab.
    WHEN c_main_tab-tab1.
      i_main_tab-subscreen = '9100'.

      IF o_alvgrid1 IS NOT BOUND.

        CREATE OBJECT o_alvgrid1
          EXPORTING
            i_parent = custom_container1.

        lo_handle = NEW lcl_alv_handle( ).
        SET HANDLER lo_handle->hotspot_click FOR o_alvgrid1.

        CLEAR: gv_variant.
        gv_variant-report = sy-repid.
        gv_variant-handle = '9100'.
        CALL METHOD o_alvgrid1->set_table_for_first_display
          EXPORTING
            is_variant                    = gv_variant
            i_save                        = c_lay
            is_layout                     = gv_layout
          CHANGING
            it_outtab                     = _fm01dat-t_alvdat_tab1
            it_fieldcatalog               = t_fieldcat_tab1
          EXCEPTIONS
            invalid_parameter_combination = 1
            program_error                 = 2
            too_many_lines                = 3
            OTHERS                        = 4.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      ELSE.
        o_alvgrid1->refresh_table_display( ).
      ENDIF.

    WHEN c_main_tab-tab2.

      i_main_tab-subscreen = '9200'.
      IF o_alvgrid2 IS NOT BOUND.
        CREATE OBJECT o_alvgrid2
          EXPORTING
            i_parent = custom_container2.

        lo_handle = NEW lcl_alv_handle( ).
        SET HANDLER lo_handle->hotspot_click FOR o_alvgrid2.

        CLEAR: gv_variant.
        gv_variant-report = sy-repid.
        gv_variant-handle = '9200'.
        CALL METHOD o_alvgrid2->set_table_for_first_display
          EXPORTING
            is_variant                    = gv_variant
            i_save                        = c_lay
            is_layout                     = gv_layout
          CHANGING
            it_outtab                     = _fm01dat-t_alvdat_tab2
            it_fieldcatalog               = t_fieldcat_tab2
          EXCEPTIONS
            invalid_parameter_combination = 1
            program_error                 = 2
            too_many_lines                = 3
            OTHERS                        = 4.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      ELSE.
        o_alvgrid2->refresh_table_display( ).
      ENDIF.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9001 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      SET SCREEN '0'.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  MAIN_TAB_ACTIVE_TAB_GET  INPUT
*&---------------------------------------------------------------------*
MODULE main_tab_active_tab_get INPUT.
  CASE sy-ucomm.
    WHEN c_main_tab-tab1.
      i_main_tab-pressed_tab = c_main_tab-tab1.
    WHEN c_main_tab-tab2.
      i_main_tab-pressed_tab = c_main_tab-tab2.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  MAIN_TAB_ACTIVE_TAB_SET  INPUT
*&---------------------------------------------------------------------*
MODULE main_tab_active_tab_set INPUT.

  main_tab-activetab = i_main_tab-pressed_tab.
  CASE i_main_tab-pressed_tab.
    WHEN c_main_tab-tab1.
      i_main_tab-subscreen = '9100'.
    WHEN c_main_tab-tab2.
      i_main_tab-subscreen = '9200'.
  ENDCASE.

ENDMODULE.
