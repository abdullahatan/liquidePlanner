*&---------------------------------------------------------------------*
*& Include          YFIN_LP_P02_CLSDAT
*&---------------------------------------------------------------------*
TABLES: flqlpos.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-s01.
PARAMETERS:
  p_bukrs TYPE bukrs OBLIGATORY DEFAULT '1073',
  p_fname TYPE localfile OBLIGATORY DEFAULT 'C:\'.
SELECTION-SCREEN END OF BLOCK b1.

CLASS: lcl_controller DEFINITION DEFERRED,
       lcl_model DEFINITION DEFERRED,
       lcl_view DEFINITION DEFERRED.

DATA: _controller TYPE REF TO lcl_controller,
      _model      TYPE REF TO lcl_model,
      _view       TYPE REF TO lcl_view.

*----------------------------------------------------------------------*
* CLASS lcl_model DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_model DEFINITION.

  PUBLIC SECTION.

    TYPES:
      tt_outdat TYPE STANDARD TABLE OF yfin_lp_s03 WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_exceldat,
        lqpos TYPE c LENGTH 50,
        budat TYPE c LENGTH 50,
        wrbtr TYPE c LENGTH 50,
        waers TYPE c LENGTH 50,
        name1 TYPE c LENGTH 50,
      END OF ty_exceldat,
      tt_exceldat TYPE STANDARD TABLE OF ty_exceldat WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_maindat,
        bukrs TYPE yfin_lp_t02-bukrs,
        waers TYPE yfin_lp_t02-waers,
        lqpos TYPE yfin_lp_t02-lqpos,
        ebene TYPE yfin_lp_t02-ebene,
        overw TYPE yfin_lp_t02-overw,
      END OF ty_maindat,
      tt_maindat TYPE STANDARD TABLE OF ty_maindat WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_fieldval,
        value TYPE REF TO data,
        text  TYPE symsgv,
      END OF ty_fieldval,
      tt_fieldval TYPE TABLE OF ty_fieldval WITH DEFAULT KEY.

    CONSTANTS:
      BEGIN OF mc_msg,
        id      TYPE symsgid    VALUE 'YFIN_LP',
        success TYPE bapi_mtype VALUE 'S',
        error   TYPE bapi_mtype VALUE 'E',
        warning TYPE bapi_mtype VALUE 'W',
        info    TYPE bapi_mtype VALUE 'I',
        abort   TYPE bapi_mtype VALUE 'A',
      END OF mc_msg.

    CONSTANTS:
      BEGIN OF mc_bdc,
        tcode    TYPE tcode VALUE 'FF63',
        dismod_p TYPE ctu_mode VALUE 'P',
        dismod_n TYPE ctu_mode VALUE 'N',
        dismod_e TYPE ctu_mode VALUE 'E',
        dismod_a TYPE ctu_mode VALUE 'A',
        updmod_a TYPE ctu_update VALUE 'A',
        updmod_s TYPE ctu_update VALUE 'S',
        updmod_l TYPE ctu_update VALUE 'L',
      END OF mc_bdc.

    DATA:
      mt_outdat   TYPE tt_outdat,
      mt_exceldat TYPE tt_exceldat,
      mt_maindat  TYPE tt_maindat.

    METHODS:
      upload_exceldat
        IMPORTING
          !iv_fname    TYPE rlgrap-filename
        EXPORTING
          !et_exceldat TYPE tt_exceldat
        EXCEPTIONS
          contains_error
          not_found_record,
      retrieve_dat
        IMPORTING
          iv_bukrs TYPE bukrs
          iv_fname TYPE localfile
        EXCEPTIONS
          contains_error,
      _conv_price_to_sap
        CHANGING
          cv_price TYPE clike
        EXCEPTIONS
          formating_error,
      _conv_datum_to_sap
        CHANGING
          cv_datum TYPE char50
        EXCEPTIONS
          formating_error,
      _required_value
        IMPORTING
          iv_fieldval      TYPE tt_fieldval
        RETURNING
          VALUE(rt_msgdat) TYPE bapiret2_tab,
      rundat_ff63
        RETURNING
          VALUE(rt_msgdat) TYPE bapiret2_tab
        EXCEPTIONS
          contains_error,
      rundat_canceled
        EXCEPTIONS
          contains_error.

ENDCLASS.
*----------------------------------------------------------------------*
* CLASS lcl_model IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_model IMPLEMENTATION .

  METHOD upload_exceldat.

    DATA : lv_start_col TYPE i VALUE '1',
           lv_start_row TYPE i VALUE '2',
           lv_end_col   TYPE i VALUE '256',
           lv_end_row   TYPE i VALUE '65536',
           lv_index     TYPE i,
           ls_exceldat  TYPE ty_exceldat.

    DATA: lt_excel_file TYPE TABLE OF alsmex_tabline,
          ls_excel_file TYPE alsmex_tabline.

    FIELD-SYMBOLS: <fs> TYPE any.

    CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
      EXPORTING
        filename                = iv_fname
        i_begin_col             = lv_start_col
        i_begin_row             = lv_start_row
        i_end_col               = lv_end_col
        i_end_row               = lv_end_row
      TABLES
        intern                  = lt_excel_file
      EXCEPTIONS
        inconsistent_parameters = 1
        upload_ole              = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.
      MESSAGE s003(yfin_lp) RAISING contains_error.
    ENDIF.

    SORT lt_excel_file BY row col.
    LOOP AT lt_excel_file INTO ls_excel_file.

      MOVE ls_excel_file-col TO lv_index.
      ASSIGN COMPONENT lv_index OF STRUCTURE ls_exceldat TO <fs>.
      MOVE ls_excel_file-value TO <fs>.
      AT END OF row.
        APPEND ls_exceldat TO et_exceldat. CLEAR ls_exceldat.
      ENDAT.
    ENDLOOP.
    IF et_exceldat IS INITIAL.
      MESSAGE s004(yfin_lp) RAISING not_found_record.
    ENDIF.

  ENDMETHOD.
  METHOD retrieve_dat.

    DATA: t_msgdat TYPE bapiret2_tab,
          _ebene   TYPE yfin_lp_t02-ebene.

    FREE: mt_exceldat.
    upload_exceldat(
      EXPORTING
        iv_fname         = iv_fname
      IMPORTING
        et_exceldat      = mt_exceldat
      EXCEPTIONS
        contains_error   = 1
        not_found_record = 2
        OTHERS           = 3 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
    ENDIF.

    SELECT DISTINCT flqlpos~lqpos, flqlpost~s_text, flqlpost~l_text
      FROM flqlpos
        INNER JOIN flqlpost ON flqlpost~lqpos = flqlpos~lqpos AND flqlpost~spras = @sy-langu
          INTO TABLE @DATA(t_lqposdat)
          FOR ALL ENTRIES IN @mt_exceldat
          WHERE flqlpos~lqpos = @mt_exceldat-lqpos(16).

    SELECT DISTINCT bukrs, waers, lqpos, ebene, overw
      FROM yfin_lp_t02
        INTO TABLE @mt_maindat
        WHERE bukrs = @p_bukrs.

    LOOP AT mt_exceldat REFERENCE INTO DATA(_exceldat).
      FREE: t_msgdat, _ebene.
      _conv_datum_to_sap(
        CHANGING
          cv_datum        = _exceldat->budat
        EXCEPTIONS
          formating_error = 1
          OTHERS          = 2 ).
      IF sy-subrc <> 0.
        APPEND VALUE #( id = sy-msgid type = sy-msgty number = sy-msgno message_v1 = sy-msgv1 ) TO t_msgdat.  CLEAR: _exceldat->budat.
      ENDIF.

      _conv_price_to_sap(
        CHANGING
          cv_price        = _exceldat->wrbtr
        EXCEPTIONS
          formating_error = 1
          OTHERS          = 2 ).
      IF sy-subrc <> 0.
        APPEND VALUE #( id = sy-msgid type = sy-msgty number = sy-msgno message_v1 = sy-msgv1 ) TO t_msgdat.  CLEAR: _exceldat->wrbtr.
      ENDIF.

      READ TABLE t_lqposdat WITH KEY lqpos = _exceldat->lqpos TRANSPORTING NO FIELDS.
      IF sy-subrc IS NOT INITIAL.
        APPEND VALUE #( id = mc_msg-id type = mc_msg-error number = '007' message_v1 = _exceldat->lqpos ) TO t_msgdat.
      ELSE.
        READ TABLE mt_maindat REFERENCE INTO DATA(_maindat) WITH KEY bukrs = p_bukrs lqpos = _exceldat->lqpos.
        IF sy-subrc IS NOT INITIAL.
          APPEND VALUE #( id = mc_msg-id type = mc_msg-error number = '008' message_v1 = p_bukrs message_v2 = _exceldat->waers message_v3 = _exceldat->lqpos ) TO t_msgdat.
        ELSE.
          _ebene = _maindat->ebene.
        ENDIF.
      ENDIF.

      DATA(t_required) = _required_value(
        EXPORTING
          iv_fieldval = VALUE #( ( value = REF #( _exceldat->lqpos ) text = TEXT-c01 )
                                 ( value = REF #( _exceldat->budat ) text = TEXT-c02 )
                                 ( value = REF #( _exceldat->wrbtr ) text = TEXT-c03 )
                                 ( value = REF #( _exceldat->waers ) text = TEXT-c04 )
                                 ( value = REF #( _exceldat->name1 ) text = TEXT-c05 ) ) ).
      APPEND LINES OF t_required TO t_msgdat.

      APPEND INITIAL LINE TO mt_outdat REFERENCE INTO DATA(_outdat).
      _outdat->* = CORRESPONDING #( _exceldat->* ).
      _outdat->* = VALUE #( BASE  _outdat->* light = COND #( WHEN t_msgdat[] IS INITIAL THEN icon_yellow_light ELSE icon_led_red ) bukrs = p_bukrs ebene = _ebene msgshw = icon_message_faulty_orphan msgdat = t_msgdat[] ).
    ENDLOOP.
    SORT mt_outdat BY ebene.

  ENDMETHOD.                    "retrieve_data
  METHOD _conv_price_to_sap.

    DEFINE conv_price.
      TRANSLATE &1 USING '. '.
      TRANSLATE &1 USING ',.'.
      CONDENSE &1 NO-GAPS.
    END-OF-DEFINITION.
    DATA: _price TYPE wrbtr.

    conv_price: cv_price.
    CATCH SYSTEM-EXCEPTIONS conversion_errors = 1.
      MOVE cv_price TO _price.
    ENDCATCH.
    IF sy-subrc <> 0.
      MESSAGE e006(yfin_lp) WITH cv_price RAISING formating_error.
    ENDIF.

  ENDMETHOD.                    "_conv_price_to_sap
  METHOD _conv_datum_to_sap.

    DATA: l_error(1),
          l_datum TYPE datum.
    TRY .
        CALL FUNCTION 'CONVERT_DATE_TO_INTERN_FORMAT'
          EXPORTING
            datum = cv_datum
            dtype = 'DATS'
          IMPORTING
            error = l_error
            idate = cv_datum.
        IF l_error <> space.
          MESSAGE e005(yfin_lp) WITH cv_datum RAISING formating_error.
        ENDIF.
      CATCH cx_root INTO DATA(lo_root).
        MESSAGE e005(yfin_lp) WITH cv_datum RAISING formating_error.
    ENDTRY.

  ENDMETHOD.                    "_conv_datum_to_sap
  METHOD _required_value.

    DATA: lr_fieldval TYPE REF TO ty_fieldval,
          lr_msgdat   TYPE REF TO bapiret2.

    FIELD-SYMBOLS <fs_value> TYPE any.

    LOOP AT iv_fieldval REFERENCE INTO lr_fieldval.
      ASSIGN lr_fieldval->value->* TO <fs_value>.
      CHECK <fs_value> IS INITIAL.
      APPEND INITIAL LINE TO rt_msgdat REFERENCE INTO lr_msgdat.
      lr_msgdat->type = mc_msg-error.
      lr_msgdat->id = mc_msg-id.
      lr_msgdat->number = '010'.
      lr_msgdat->message_v1 = lr_fieldval->text.
    ENDLOOP.

  ENDMETHOD.                    "_required_value
  METHOD rundat_ff63.

    DATA: lo_bdctkit TYPE REF TO yfin_lp_bdc_toolkit,
          t_return   TYPE bapiret2_tab.

*-&Create singleton session;
    lo_bdctkit = yfin_lp_bdc_toolkit=>bdc_instance(
      EXPORTING
        im_tcode    = mc_bdc-tcode
        im_upd_mode = mc_bdc-updmod_a
        im_bdc_mode = mc_bdc-dismod_n
        im_racommit = abap_true ).

    LOOP AT mt_outdat REFERENCE INTO DATA(_outdat) WHERE ( light = icon_yellow_light OR light = icon_red_light ).

*-&Fill BDC Dat->
      lo_bdctkit->bdc_add_screen( im_repid = 'SAPMF40E' im_dynnr = '0100' ).
      lo_bdctkit->bdc_add_field( im_fld = 'BDC_CURSOR'  im_val = 'FDES-DSART' ).
      lo_bdctkit->bdc_add_field( im_fld = 'BDC_OKCODE'  im_val = '=ONE' ).
      lo_bdctkit->bdc_add_field( im_fld = 'FDES-BUKRS'  im_val = _outdat->bukrs ).
      lo_bdctkit->bdc_add_field( im_fld = 'FDES-DSART'  im_val = _outdat->ebene ).

      lo_bdctkit->bdc_add_screen( im_repid = 'SAPMF40E' im_dynnr = '0102' ).
      lo_bdctkit->bdc_add_field( im_fld = 'BDC_CURSOR'  im_val = 'FDES-DATUM' ).
      lo_bdctkit->bdc_add_field( im_fld = 'BDC_OKCODE'  im_val = '=UPD' ).
      lo_bdctkit->bdc_add_field( im_fld = 'FDES-DATUM'  im_val = |{ _outdat->budat DATE = USER }| ).
      lo_bdctkit->bdc_add_field( im_fld = 'FDES-GRUPP'  im_val = _outdat->ebene ).
      lo_bdctkit->bdc_add_field( im_fld = 'FDES-WRSHB'  im_val = |{ _outdat->wrbtr CURRENCY = _outdat->waers NUMBER = USER }| ).
      lo_bdctkit->bdc_add_field( im_fld = 'FDES-DISPW'  im_val = _outdat->waers ).
      lo_bdctkit->bdc_add_field( im_fld = 'FDES-SGTXT'  im_val = _outdat->name1 ).

      lo_bdctkit->bdc_add_screen( im_repid = 'SAPMF40E' im_dynnr = '0102' ).
      lo_bdctkit->bdc_add_field( im_fld = 'BDC_OKCODE'  im_val = '/EBCK' ).
      lo_bdctkit->bdc_add_field( im_fld = 'BDC_CURSOR'  im_val = 'FDES-DATUM' ).

      lo_bdctkit->bdc_add_screen( im_repid = 'SAPMF40E' im_dynnr = '0100' ).
      lo_bdctkit->bdc_add_field( im_fld = 'BDC_OKCODE'  im_val = '/EBCK' ).
      lo_bdctkit->bdc_add_field( im_fld = 'BDC_CURSOR'  im_val = 'FDES-BUKRS' ).

*-&Run bdc data;
      FREE: _outdat->msgdat.
      lo_bdctkit->bdc_process(
        IMPORTING
          ev_subrc    = DATA(l_subrc)
          ev_messages = DATA(t_messages) ).
      SORT t_messages. DELETE ADJACENT DUPLICATES FROM t_messages COMPARING ALL FIELDS.
      INSERT LINES OF t_messages INTO TABLE rt_msgdat.
      READ TABLE t_messages ASSIGNING FIELD-SYMBOL(<fs_msg>) WITH KEY id = 'RQ' number = '070'.
      IF sy-subrc IS INITIAL.
        _outdat->light = icon_green_light.
        _outdat->idenr = <fs_msg>-message_v1.
      ELSE.
        _outdat->light = icon_red_light.
        _outdat->msgdat = t_messages.
      ENDIF.
    ENDLOOP.
    IF sy-subrc <> 0.
      MESSAGE e009(yfin_lp) RAISING contains_error.
    ENDIF.

  ENDMETHOD.                    "rundat_ff63
  METHOD rundat_canceled.

    DATA: t_idenr_rng TYPE RANGE OF fdes-idenr.
    LOOP AT mt_outdat INTO DATA(s_outdat) GROUP BY ( bukrs = s_outdat-bukrs lqpos = s_outdat-lqpos ebene = s_outdat-ebene )
                                          ASSIGNING FIELD-SYMBOL(<group_dat>).

      READ TABLE mt_maindat REFERENCE INTO DATA(_maindat) WITH KEY bukrs = <group_dat>-bukrs lqpos = <group_dat>-lqpos ebene = <group_dat>-ebene overw = abap_true.
      IF sy-subrc IS INITIAL.


        DATA(t_members) = VALUE tt_outdat( ).
        LOOP AT GROUP <group_dat> ASSIGNING FIELD-SYMBOL(<member>).
          t_members = VALUE #( BASE t_members ( <member> ) ).
        ENDLOOP.

        IF NOT line_exists( t_members[ idenr = space ] ).
          FREE: t_idenr_rng.
          t_idenr_rng = VALUE #( FOR wa_member IN t_members ( sign = 'E' option = 'EQ' low = wa_member-idenr ) ).

          SELECT archk, bukrs, bnkko, grupp, ebene, dispw, datum, idenr, gsber
            FROM fdes
              INTO TABLE @DATA(t_fdesdat)
              WHERE bukrs EQ @<group_dat>-bukrs
                AND ebene EQ @<group_dat>-ebene
                AND merkm NE 'CANCEL'
                AND idenr IN @t_idenr_rng.

          LOOP AT t_fdesdat REFERENCE INTO DATA(_fdesdat) .
            UPDATE fdes SET merkm = 'CANCEL'
             WHERE archk = _fdesdat->archk
               AND bukrs = _fdesdat->bukrs
               AND bnkko = _fdesdat->bnkko
               AND grupp = _fdesdat->grupp
               AND ebene = _fdesdat->ebene
               AND dispw = _fdesdat->dispw
               AND datum = _fdesdat->datum
               AND idenr = _fdesdat->idenr
               AND gsber = _fdesdat->gsber.
            COMMIT WORK.
          ENDLOOP.

        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "rundat_canceled
ENDCLASS.
*----------------------------------------------------------------------*
* CLASS lcl_view DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_view DEFINITION FINAL.

  PUBLIC SECTION.
    CONSTANTS:
      mc_strname TYPE tabname VALUE 'YFIN_LP_S03'.

    DATA:
      mt_fcat  TYPE lvc_t_fcat,
      mt_exdat TYPE slis_t_extab.

    METHODS:
      prepare_fcatdat
        IMPORTING
          !im_strname TYPE tabname
        EXCEPTIONS
          contains_error,
      excluding_alvdat
        EXCEPTIONS
          contains_error,
      display_alvdat
        EXCEPTIONS
          contains_error.

ENDCLASS.
*----------------------------------------------------------------------*
* CLASS lcl_view IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_view IMPLEMENTATION .

  METHOD prepare_fcatdat.

    DATA: _text(40) TYPE c.

    FREE: me->mt_fcat.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = im_strname
      CHANGING
        ct_fieldcat            = me->mt_fcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      MESSAGE s001(00) WITH 'Fiedcatalog oluşturma sırasında hatalar oluştu!' RAISING contains_error.
    ENDIF.

    LOOP AT me->mt_fcat REFERENCE INTO DATA(r_fcat).
      CLEAR: _text.
      CASE r_fcat->fieldname .
        WHEN 'LIGHT'.
          r_fcat->just = 'C'.
          _text = TEXT-f01.
        WHEN 'MSGSHW'.
          r_fcat->just = 'C'.
          r_fcat->hotspot = abap_true.
          _text = TEXT-f02.
      ENDCASE.
      IF _text <> space.
        MOVE _text TO: r_fcat->scrtext_l, r_fcat->scrtext_m, r_fcat->scrtext_s, r_fcat->reptext.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD excluding_alvdat.

    IF line_exists( _model->mt_outdat[ light = icon_led_red ] ).
      APPEND VALUE #( fcode = '&RUNDAT' ) TO mt_exdat.
    ENDIF.

  ENDMETHOD.
  METHOD display_alvdat.

    excluding_alvdat(
      EXCEPTIONS
        contains_error = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
    ENDIF.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
        i_callback_program       = sy-repid
        i_callback_pf_status_set = 'SET_PF_STATUS'
        i_callback_user_command  = 'USER_COMMAND'
        i_callback_top_of_page   = 'TOP_OF_PAGE'
        is_layout_lvc            = VALUE lvc_s_layo( col_opt = abap_true cwidth_opt = abap_true zebra = abap_true )
        it_fieldcat_lvc          = me->mt_fcat
        it_excluding             = mt_exdat
        i_default                = abap_true
        i_save                   = abap_true
      TABLES
        t_outtab                 = _model->mt_outdat
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0 .
      MESSAGE s001(00) WITH 'ALV gösterimi sırasında hatalar oluştu!' RAISING contains_error.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
*----------------------------------------------------------------------*
* CLASS lcl_controller DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_controller DEFINITION FINAL.

  PUBLIC SECTION.

    CONSTANTS:
      mc_model TYPE seoclsname VALUE 'LCL_MODEL',
      mc_view  TYPE seoclsname VALUE 'LCL_VIEW'.

    CONSTANTS:
      BEGIN OF mc_cons,
        p_fname TYPE char10 VALUE 'P_FNAME',
      END OF mc_cons.

    DATA:
      mo_model TYPE REF TO lcl_model,
      mo_view  TYPE REF TO lcl_view.

    METHODS:
      instantiate_app
        IMPORTING
          !iv_model            TYPE seoclsname
          !iv_view             TYPE seoclsname
        RETURNING
          VALUE(ro_controller) TYPE REF TO lcl_controller,
      at_selection_screen_request
        IMPORTING
          !iv_fldnam TYPE char10,
      search_file_path
        CHANGING
          cv_fname TYPE localfile,
      alv_session
        EXCEPTIONS
          contains_error,
      rundat_session
        RETURNING
          VALUE(rt_msgdat) TYPE bapiret2_tab,
      popup_confirm
        IMPORTING
          im_titlebar      TYPE clike
          im_question      TYPE clike
        RETURNING
          VALUE(rv_answer) TYPE char1,
      display_logdat
        IMPORTING
          !iv_logdat TYPE bapiret2_tab.

ENDCLASS.
*----------------------------------------------------------------------*
* CLASS lcl_controller IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_controller IMPLEMENTATION.

  METHOD instantiate_app.

    DATA: lo_object TYPE REF TO object.

    ro_controller = NEW lcl_controller( ).

    FREE: lo_object.
    CREATE OBJECT lo_object TYPE (iv_model).
    IF lo_object IS BOUND.
      ro_controller->mo_model ?= lo_object.
    ENDIF.

    FREE: lo_object.
    CREATE OBJECT lo_object TYPE (iv_view).
    IF lo_object IS BOUND.
      ro_controller->mo_view ?= lo_object.
    ENDIF.

    IF ro_controller->mo_model IS BOUND AND ro_controller->mo_view IS BOUND.
      _model ?= ro_controller->mo_model.
      _view  ?= ro_controller->mo_view.
    ENDIF.

  ENDMETHOD.
  METHOD at_selection_screen_request.

    CASE iv_fldnam.
      WHEN mc_cons-p_fname.
        search_file_path(
          CHANGING
            cv_fname = p_fname ).
    ENDCASE.

  ENDMETHOD.
  METHOD search_file_path.

    DATA : lv_subrc LIKE sy-subrc,
           lt_path  TYPE filetable,
           lr_path  TYPE REF TO file_table.

    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      EXPORTING
        window_title      = 'Select Source Excel File'
        default_extension = 'XLSX'
        initial_directory = 'C:\'
        multiselection    = abap_false
      CHANGING
        file_table        = lt_path
        rc                = lv_subrc.

    READ TABLE lt_path REFERENCE INTO lr_path INDEX 1.
    IF sy-subrc IS INITIAL.
      MOVE lr_path->filename TO cv_fname.
    ENDIF.

  ENDMETHOD.
  METHOD alv_session.

    CHECK lines( _controller->mo_model->mt_outdat ) IS NOT INITIAL.

    _controller->mo_view->prepare_fcatdat(
      EXPORTING
        im_strname     = _controller->mo_view->mc_strname
      EXCEPTIONS
        contains_error = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
    ENDIF.

    _controller->mo_view->display_alvdat(
      EXCEPTIONS
        contains_error = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
    ENDIF.

  ENDMETHOD.
  METHOD rundat_session.

    CHECK popup_confirm(
      EXPORTING
        im_titlebar = CONV #( TEXT-t01 )
        im_question = CONV #( TEXT-t02 ) ) EQ '1'.

    mo_model->rundat_ff63(
      RECEIVING
        rt_msgdat      = rt_msgdat
      EXCEPTIONS
        contains_error = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      mo_model->rundat_canceled(
        EXCEPTIONS
          contains_error = 1
          OTHERS         = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD popup_confirm.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = im_titlebar
        text_question         = im_question
        text_button_1         = 'Evet'
        text_button_2         = 'Hayır'
        default_button        = '2'
        display_cancel_button = abap_false
      IMPORTING
        answer                = rv_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

  ENDMETHOD.
  METHOD display_logdat.

    CHECK NOT iv_logdat IS INITIAL.
    CALL FUNCTION 'ISH_BAPIRET2_DISPLAY'
      TABLES
        ss_bapiret2 = iv_logdat.

  ENDMETHOD.

ENDCLASS.
