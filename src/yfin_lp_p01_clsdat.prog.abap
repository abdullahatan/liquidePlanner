*&---------------------------------------------------------------------*
*&  Include           YFIN_LP_P01_CLSDAT
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&  Class           LCL_MVC_MODEL           Definition
*&---------------------------------------------------------------------*
CLASS lcl_mvc_model DEFINITION.
  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_rootdat,
        _docdat TYPE STANDARD TABLE OF yfin_lp_s01 WITH DEFAULT KEY,
        _logdat TYPE STANDARD TABLE OF yfin_lp_t03 WITH DEFAULT KEY,
      END OF ty_rootdat.

    TYPES:
      BEGIN OF ty_appdat,
        bukrs    TYPE yfin_lp_t01-bukrs,
        pkind    TYPE yfin_lp_t01-pkind,
        waers    TYPE yfin_lp_t01-waers,
        base_tab TYPE REF TO data,
        line_tab TYPE REF TO data,
        root_dat TYPE ty_rootdat,
      END OF ty_appdat,
      tt_appdat TYPE STANDARD TABLE OF ty_appdat WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_runparams,
        _update_mod TYPE char1,
      END OF ty_runparams.

    DATA:
      mt_alvdat    TYPE REF TO data,
      mt_keydat    TYPE yfin_lp_cl01=>tt_valdat,
      mt_messtab   TYPE bapiret2_tab,
      mt_appdat    TYPE tt_appdat,
      mv_appdat    TYPE ty_appdat,
      mt_actualdat TYPE yfin_lp_tt01,
      mt_manueldat TYPE yfin_lp_tt02,
      mv_runparams TYPE ty_runparams.

    METHODS:
      constructor,
      retrieve_dat
        IMPORTING
          !im_bukrs TYPE bukrs
          !im_tunit TYPE char1
          !im_keyda TYPE tt_keydat_rng
          !im_ldays TYPE int1
        EXCEPTIONS
          contains_error.

ENDCLASS.
*&---------------------------------------------------------------------*
*&  Class           LCL_MVC_VIEW           Definition
*&---------------------------------------------------------------------*
CLASS lcl_mvc_view DEFINITION.
  PUBLIC SECTION.

    DATA:
      mo_grid        TYPE REF TO cl_gui_alv_grid,
      mo_dyndoc_id   TYPE REF TO cl_dd_document,
      mo_splitter    TYPE REF TO cl_gui_splitter_container,
      mo_parent_grid TYPE REF TO cl_gui_container,
      mo_parent_top  TYPE REF TO cl_gui_container,
      mo_html_cntrl  TYPE REF TO cl_gui_html_viewer,
      mt_fieldcat    TYPE lvc_t_fcat.

    METHODS:
      constructor
        IMPORTING
          io_model      TYPE REF TO lcl_mvc_model
          io_controller TYPE REF TO lcl_mvc_controller,
      display_alvdat
        EXCEPTIONS
          contains_error.

  PRIVATE SECTION.

    DATA:
      mo_model      TYPE REF TO lcl_mvc_model,
      mo_controller TYPE REF TO lcl_mvc_controller.

    METHODS:
      create_fieldcat
        IMPORTING
          !im_strname TYPE tabname
        EXCEPTIONS
          contains_error,
      update_fieldcat
        EXCEPTIONS
          contains_error,
      set_layout_dat
        RETURNING
          VALUE(rv_layoutdat) TYPE lvc_s_layo,
      set_exclude_dat
        RETURNING
          VALUE(rv_excludedat) TYPE ui_functions,
      attach_handlers
        IMPORTING
          VALUE(im_grid) TYPE REF TO cl_gui_alv_grid
        EXCEPTIONS
          contains_error,
      refresh_alv
        EXCEPTIONS
          contains_error.


ENDCLASS.                    "lcl_mvc_view DEFINITION
*&---------------------------------------------------------------------*
*&  Class           LCL_MVC_CONTROLLER           Definition
*&---------------------------------------------------------------------*
CLASS lcl_mvc_controller DEFINITION.
  PUBLIC SECTION.

    TYPES: ty_lqday_rng TYPE RANGE OF datum.

    TYPES: BEGIN OF ty_keydat,
             bukrs TYPE bukrs,
             pkind TYPE yfin_lp_e001,
             waers TYPE waers,
             lqpos TYPE flqpos,
             lqday TYPE ty_lqday_rng,
             ebene TYPE fdlev,
             ltype TYPE char1,
           END OF ty_keydat.

    CONSTANTS:
      mc_model TYPE seoclsname VALUE 'LCL_MVC_MODEL',
      mc_view  TYPE seoclsname VALUE 'LCL_MVC_VIEW'.

    METHODS:
      constructor,
      instantiate_app
        IMPORTING
          iv_model             TYPE seoclsname
          iv_view              TYPE seoclsname
        RETURNING
          VALUE(ro_controller) TYPE REF TO lcl_mvc_controller,
      initialization,
      at_selection_screen,
      user_commmand_selscr
        CHANGING
          cv_ucomm TYPE sy-ucomm,
      start_of_selection,
      listbox_build,
      alv_session
        EXCEPTIONS
          contains_error,
      display_linedat
        IMPORTING
          iv_actualdat TYPE yfin_lp_tt01
          iv_manueldat TYPE yfin_lp_tt02
        EXPORTING
          ev_action    TYPE syst_ucomm
        EXCEPTIONS
          contains_error,
      reject_manueldat
        IMPORTING
          iv_keydat    TYPE ty_keydat
          iv_column_id TYPE lvc_s_col
          iv_row_no    TYPE lvc_s_roid
        EXCEPTIONS
          contains_error,
      save_logdat
        EXCEPTIONS
          contains_error,
      get_icon
        IMPORTING
          iv_type        TYPE char1
        RETURNING
          VALUE(rv_icon) TYPE text40,
      get_long_date
        IMPORTING
          !im_dat         TYPE datum
        RETURNING
          VALUE(r_dattxt) TYPE text40,
      _expand_all_alvdat
        RETURNING
          VALUE(rv_refresh) TYPE abap_bool,
      _collapse_all_alvdat
        RETURNING
          VALUE(rv_refresh) TYPE abap_bool,
      _editable_alvdat
        RETURNING
          VALUE(rv_refresh) TYPE abap_bool,
      handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING
            e_column_id
            es_row_no,
      handle_toolbar_set FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
            e_object
            e_interactive,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
            e_ucomm,
      handle_data_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING
            e_modified
            et_good_cells,
      handle_top_of_page FOR EVENT top_of_page OF cl_gui_alv_grid
        IMPORTING
            e_dyndoc_id,
      event_top_of_page
        CHANGING
          dg_dyndoc_id TYPE REF TO cl_dd_document,
      display_top_of_page
        IMPORTING
          dg_dyndoc_id TYPE REF TO cl_dd_document.

  PRIVATE SECTION.
    DATA:
      mo_model TYPE REF TO lcl_mvc_model,
      mo_view  TYPE REF TO lcl_mvc_view.

ENDCLASS.
*&---------------------------------------------------------------------*
*&  Class           LCL_MVC_VIEW           Implementation
*&---------------------------------------------------------------------*
CLASS lcl_mvc_model IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.                    "constructor
  METHOD retrieve_dat.
    FIELD-SYMBOLS: <ft_alvdat>  TYPE STANDARD TABLE,
                   <ft_basedat> TYPE STANDARD TABLE,
                   <ft_linedat> TYPE STANDARD TABLE,
                   <fs_basedat> TYPE any,
                   <fs_linedat> TYPE any,
                   <fs_value>   TYPE any.

    TYPES: BEGIN OF ty_bkpfdat,
             bukrs TYPE bkpf-bukrs,
             belnr TYPE bkpf-belnr,
             gjahr TYPE bkpf-gjahr,
             waers TYPE bkpf-waers,
           END OF ty_bkpfdat,
           tt_bkpfdat TYPE HASHED TABLE OF ty_bkpfdat WITH UNIQUE KEY bukrs belnr gjahr.

    TYPES: tt_logdat TYPE STANDARD TABLE OF yfin_lp_t03 WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_lfa1dat,
             lifnr TYPE lfa1-lifnr,
             name1 TYPE lfa1-name1,
             name2 TYPE lfa1-name2,
           END OF ty_lfa1dat,
           tt_lfa1dat TYPE HASHED TABLE OF ty_lfa1dat WITH UNIQUE KEY lifnr.

    TYPES: BEGIN OF ty_kna1dat,
             kunnr TYPE kna1-kunnr,
             name1 TYPE kna1-name1,
             name2 TYPE kna1-name2,
           END OF ty_kna1dat,
           tt_kna1dat TYPE HASHED TABLE OF ty_kna1dat WITH UNIQUE KEY kunnr.

    TYPES: BEGIN OF ty_skatdat,
             saknr TYPE skat-saknr,
             txt50 TYPE skat-txt50,
           END OF ty_skatdat,
           tt_skatdat TYPE HASHED TABLE OF ty_skatdat WITH UNIQUE KEY saknr.

    DATA: t_celldat   TYPE lvc_t_styl,
          t_cellcolor TYPE lvc_t_scol,
          r_basedat   TYPE REF TO data,
          r_linedat   TYPE REF TO data,
          _lqday_rng  TYPE RANGE OF datum,
          t_basicdat  TYPE TABLE OF rgsbv,
          t_budat_rng TYPE RANGE OF datum,
          t_fdlev_rng TYPE RANGE OF bseg-fdlev,
          t_blart_rng TYPE RANGE OF blart,
          t_actualdat TYPE STANDARD TABLE OF yfin_lp_s01,
          t_bkpfdat   TYPE tt_bkpfdat,
          t_plandat   TYPE STANDARD TABLE OF yfin_lp_s01,
          t_lfa1dat   TYPE tt_lfa1dat,
          t_kna1dat   TYPE tt_kna1dat,
          t_skatdat   TYPE tt_skatdat.

    SELECT bukrs, pkind, waers, hname
      FROM yfin_lp_t01
        INTO TABLE @DATA(t_basedat)
          WHERE bukrs = @p_bukrs
          ORDER BY rowno.

    SELECT bukrs, pkind, waers, rowno, hname, lqpos, ebene
      FROM yfin_lp_t02
        INTO TABLE @DATA(t_linedat)
          WHERE bukrs = @p_bukrs
          ORDER BY bukrs, waers, rowno.

*--------------------------------------------------------------------*
*-&Planlama Verilerinin Çekilmesi->
*--------------------------------------------------------------------*
    FREE: t_basicdat.
    CALL FUNCTION 'G_SET_FETCH'
      EXPORTING
        class              = '0000'
        no_authority_check = 'X'
        setnr              = 'LP_BELGE_TURU'
      TABLES
        set_lines_basic    = t_basicdat
      EXCEPTIONS
        no_authority       = 1
        set_is_broken      = 2
        set_not_found      = 3
        OTHERS             = 4.

    FREE: t_blart_rng.
    LOOP AT t_basicdat REFERENCE INTO DATA(_basicdat).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = _basicdat->from ) TO t_blart_rng.
    ENDLOOP.

    FREE: t_budat_rng.
    t_budat_rng[] = im_keyda[].
    ASSIGN t_budat_rng[ 1 ] TO FIELD-SYMBOL(<fs_budat_rng>).
    IF sy-subrc IS INITIAL.
      <fs_budat_rng>-low = <fs_budat_rng>-low - im_ldays.
    ENDIF.

    FREE: t_bkpfdat.
    SELECT bukrs, belnr, gjahr, waers
      FROM bkpf
        INTO CORRESPONDING FIELDS OF TABLE @t_bkpfdat
          WHERE bukrs EQ @p_bukrs
            AND budat IN @t_budat_rng
            AND blart IN @t_blart_rng.

    IF NOT t_bkpfdat[] IS INITIAL.
      FREE: t_fdlev_rng.
      SELECT DISTINCT 'I' AS sign, 'EQ' AS option, a~ebene AS low
        FROM yfin_lp_t02 AS a
          INTO CORRESPONDING FIELDS OF TABLE @t_fdlev_rng
            WHERE a~bukrs = @p_bukrs.

      SELECT bukrs, belnr, gjahr, buzei, gsber, kunnr, lifnr, zfbdt AS lqday, zbd1t, zbd2t, zbd3t, fdlev AS ebene, wrbtr, zuonr, sgtxt
        FROM bseg
          INTO TABLE @DATA(t_bsegdat)
            FOR ALL ENTRIES IN @t_bkpfdat
              WHERE bukrs = @t_bkpfdat-bukrs
                AND gjahr = @t_bkpfdat-gjahr
                AND belnr = @t_bkpfdat-belnr
                AND koart IN ('D', 'K')
                AND fdlev IN @t_fdlev_rng.

      FREE: t_lfa1dat, t_kna1dat.
      IF NOT t_bsegdat[] IS INITIAL.
        SELECT DISTINCT lifnr, name1, name2
          FROM lfa1
            INTO TABLE @t_lfa1dat
              FOR ALL ENTRIES IN @t_bsegdat
                WHERE lifnr = @t_bsegdat-lifnr.

        SELECT DISTINCT kunnr, name1, name2
          FROM kna1
            INTO TABLE @t_kna1dat
              FOR ALL ENTRIES IN @t_bsegdat
                WHERE kunnr = @t_bsegdat-kunnr.
      ENDIF.

      LOOP AT t_bsegdat REFERENCE INTO DATA(_bsegdat).
        DATA(_index) = sy-tabix.
        _bsegdat->lqday = _bsegdat->lqday + _bsegdat->zbd1t + _bsegdat->zbd2t + _bsegdat->zbd3t.
        IF _bsegdat->lqday NOT IN im_keyda.
          DELETE t_bsegdat INDEX _index. CONTINUE.
        ENDIF.
        APPEND INITIAL LINE TO t_plandat REFERENCE INTO DATA(_plandat).
        _plandat->* = CORRESPONDING #( _bsegdat->* ).
        _plandat->lqpos = VALUE #( t_linedat[ ebene = _plandat->ebene ]-lqpos OPTIONAL ).
        _plandat->ltype = 'P'.
        READ TABLE t_bkpfdat REFERENCE INTO DATA(_bkpfdat) WITH TABLE KEY bukrs = _bsegdat->bukrs belnr = _bsegdat->belnr gjahr = _bsegdat->gjahr.
        IF sy-subrc IS INITIAL.
          _plandat->twaer = _bkpfdat->waers.
        ENDIF.
        IF _bsegdat->lifnr <> space.
          READ TABLE t_lfa1dat REFERENCE INTO DATA(_lfa1dat) WITH TABLE KEY lifnr = _bsegdat->lifnr.
          IF sy-subrc IS INITIAL.
            _plandat->name1 = |{ _lfa1dat->lifnr ALPHA = IN }/{ _lfa1dat->name1 } { _lfa1dat->name2 }|.
          ENDIF.
        ENDIF.
        IF _bsegdat->kunnr <> space.
          READ TABLE t_kna1dat REFERENCE INTO DATA(_kna1dat) WITH TABLE KEY kunnr = _bsegdat->kunnr.
          IF sy-subrc IS INITIAL.
            _plandat->name1 = |{ _kna1dat->kunnr ALPHA = IN }/{ _kna1dat->name1 } { _kna1dat->name2 }|.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    SELECT DISTINCT 'P' AS ltype, a~zbukr AS bukrs, a~belnr, a~gjahr, a~buzei, a~gsber, b~lqpos, b~ebene, a~lqday, a~twaer, a~wrbtr, a~pkoart, a~partner
      FROM flqitemfi_fc AS a
        INNER JOIN yfin_lp_t02 AS b ON b~bukrs = a~bukrs AND b~lqpos = a~lqpos AND b~waers = a~twaer
          INTO TABLE @DATA(t_flqdat_fc)
            WHERE a~zbukr EQ @p_bukrs
              AND a~lqday IN @s_keydat.

    FREE: t_lfa1dat, t_kna1dat, t_skatdat.
    IF NOT t_flqdat_fc[] IS INITIAL.
      SELECT DISTINCT lifnr, name1, name2
        FROM lfa1
          INTO TABLE @t_lfa1dat
            FOR ALL ENTRIES IN @t_flqdat_fc
              WHERE lifnr = @t_flqdat_fc-partner.

      SELECT DISTINCT kunnr, name1, name2
        FROM kna1
          INTO TABLE @t_kna1dat
            FOR ALL ENTRIES IN @t_flqdat_fc
              WHERE kunnr = @t_flqdat_fc-partner.

      SELECT DISTINCT saknr, txt50
        FROM skat
          INTO TABLE @t_skatdat
            FOR ALL ENTRIES IN @t_flqdat_fc
              WHERE saknr = @t_flqdat_fc-partner
                AND spras = @sy-langu.
    ENDIF.

    LOOP AT t_flqdat_fc REFERENCE INTO DATA(_flqdat_fc).
      APPEND INITIAL LINE TO t_plandat REFERENCE INTO _plandat.
      _plandat->* = CORRESPONDING #( _flqdat_fc->* ).
      CASE _flqdat_fc->pkoart.
        WHEN 'D'.
          READ TABLE t_kna1dat REFERENCE INTO _kna1dat WITH TABLE KEY kunnr = _flqdat_fc->partner.
          IF sy-subrc IS INITIAL.
            _plandat->name1 = |{ _kna1dat->kunnr ALPHA = IN }/{ _kna1dat->name1 } { _kna1dat->name2 }|.
          ENDIF.
        WHEN 'K'.
          READ TABLE t_lfa1dat REFERENCE INTO _lfa1dat WITH TABLE KEY lifnr = _flqdat_fc->partner.
          IF sy-subrc IS INITIAL.
            _plandat->name1 = |{ _lfa1dat->lifnr ALPHA = IN }/{ _lfa1dat->name1 } { _lfa1dat->name2 }|.
          ENDIF.
        WHEN 'S'.
          READ TABLE t_skatdat REFERENCE INTO DATA(_skatdat) WITH TABLE KEY saknr = _flqdat_fc->partner.
          IF sy-subrc IS INITIAL.
            _plandat->name1 = |{ _skatdat->saknr ALPHA = IN }/{ _skatdat->txt50 }|.
          ENDIF.
      ENDCASE.
    ENDLOOP.

    SELECT DISTINCT 'P' AS ltype, a~bukrs, a~idenr, a~gsber, a~ebene, b~lqpos, a~datum AS lqday, a~dispw AS twaer, a~wrshb AS wrbtr, a~zuonr, a~sgtxt
      FROM fdes AS a
        INNER JOIN yfin_lp_t02 AS b ON b~bukrs = a~bukrs AND b~ebene = a~ebene AND b~waers = a~dispw
          INTO TABLE @DATA(t_fdesdat)
            WHERE a~bukrs EQ @p_bukrs
              AND a~datum IN @im_keyda
              AND a~merkm NE 'CANCEL'.

    LOOP AT t_fdesdat REFERENCE INTO DATA(_fdesdat).
      APPEND INITIAL LINE TO t_plandat REFERENCE INTO _plandat.
      _plandat->* = CORRESPONDING #( _fdesdat->* ).
    ENDLOOP.

*--------------------------------------------------------------------*
*-&Fiili Verilerin Çekilmesi->
*--------------------------------------------------------------------*
    SELECT DISTINCT 'A' AS ltype, a~zbukr AS bukrs, a~belnr, a~gjahr, a~buzei, a~gsber, a~lqpos, b~ebene, a~lqday, a~twaer, a~wrbtr, a~pkoart, a~partner
      FROM flqitemfi AS a
        INNER JOIN yfin_lp_t02 AS b ON b~bukrs = a~bukrs AND b~lqpos = a~lqpos AND b~waers = a~twaer
          INTO TABLE @DATA(t_flqdat_fi)
            WHERE a~zbukr EQ @p_bukrs
              AND a~lqday IN @s_keydat
              AND a~xdelete EQ @space.

    FREE: t_lfa1dat, t_kna1dat, t_skatdat.
    IF NOT t_flqdat_fi[] IS INITIAL.
      SELECT DISTINCT lifnr, name1, name2
        FROM lfa1
          INTO TABLE @t_lfa1dat
            FOR ALL ENTRIES IN @t_flqdat_fi
              WHERE lifnr = @t_flqdat_fi-partner.

      SELECT DISTINCT kunnr, name1, name2
        FROM kna1
          INTO TABLE @t_kna1dat
            FOR ALL ENTRIES IN @t_flqdat_fi
              WHERE kunnr = @t_flqdat_fi-partner.

      SELECT DISTINCT saknr, txt50
        FROM skat
          INTO TABLE @t_skatdat
            FOR ALL ENTRIES IN @t_flqdat_fi
              WHERE saknr = @t_flqdat_fi-partner
                AND spras = @sy-langu.
    ENDIF.

    LOOP AT t_flqdat_fi REFERENCE INTO DATA(_flqdat_fi).
      APPEND INITIAL LINE TO t_actualdat REFERENCE INTO DATA(_actualdat).
      _actualdat->* = CORRESPONDING #( _flqdat_fi->* ).
      CASE _flqdat_fi->pkoart.
        WHEN 'D'.
          READ TABLE t_kna1dat REFERENCE INTO _kna1dat WITH TABLE KEY kunnr = _flqdat_fi->partner.
          IF sy-subrc IS INITIAL.
            _actualdat->name1 = |{ _kna1dat->kunnr ALPHA = IN }/{ _kna1dat->name1 } { _kna1dat->name2 }|.
          ENDIF.
        WHEN 'K'.
          READ TABLE t_lfa1dat REFERENCE INTO _lfa1dat WITH TABLE KEY lifnr = _flqdat_fi->partner.
          IF sy-subrc IS INITIAL.
            _actualdat->name1 = |{ _lfa1dat->lifnr ALPHA = IN }/{ _lfa1dat->name1 } { _lfa1dat->name2 }|.
          ENDIF.
        WHEN 'S'.
          READ TABLE t_skatdat REFERENCE INTO _skatdat WITH TABLE KEY saknr = _flqdat_fi->partner.
          IF sy-subrc IS INITIAL.
            _actualdat->name1 = |{ _skatdat->saknr ALPHA = IN }/{ _skatdat->txt50 }|.
          ENDIF.
      ENDCASE.
    ENDLOOP.

*--------------------------------------------------------------------*
*-&Manuel Düzeltme Kayıtları->
*--------------------------------------------------------------------*
    SELECT a~bukrs, a~pkind, a~twaer, a~lqpos, a~ebene, a~lqday, a~ltype, a~prsid, a~wrbtr, a~erdat, a~erzet, a~ernam
      FROM yfin_lp_t03 AS a
        INTO TABLE @DATA(t_logdat)
          WHERE bukrs EQ @p_bukrs
            AND lqday IN @s_keydat
            AND xdele EQ @space.

    LOOP AT t_basedat REFERENCE INTO DATA(_basedat).
      FREE: mv_appdat.
      mv_appdat = CORRESPONDING #( _basedat->* ).

      DATA(_basealv) = NEW yfin_lp_cl01( )->dynamic_alvdat(
        EXPORTING
          iv_tmunit = p_tunit
          iv_begdat = s_keydat-low
          iv_enddat = s_keydat-high
          iv_celdat = abap_true ).

      mv_appdat-base_tab ?= _basealv-alvoutput.
      ASSIGN mv_appdat-base_tab->* TO <ft_basedat>.
      CREATE DATA r_basedat LIKE LINE OF <ft_basedat>. ASSIGN r_basedat->* TO <fs_basedat>.

      DATA(_linealv) = NEW yfin_lp_cl01( )->dynamic_alvdat(
        EXPORTING
          iv_tmunit = p_tunit
          iv_begdat = s_keydat-low
          iv_enddat = s_keydat-high
          iv_celdat = abap_true ).

      mv_appdat-line_tab ?= _linealv-alvoutput.
      ASSIGN mv_appdat-line_tab->* TO <ft_linedat>.
      CREATE DATA r_linedat LIKE LINE OF <ft_linedat>. ASSIGN r_linedat->* TO <fs_linedat>.

      FREE: <ft_basedat>, <ft_linedat>.
      LOOP AT t_linedat REFERENCE INTO DATA(_linedat)
                        WHERE bukrs = _basedat->bukrs AND
                              waers = _basedat->waers AND
                              pkind = _basedat->pkind.

        FREE: <fs_linedat>, <fs_basedat>.
        ASSIGN COMPONENT 'BUKRS' OF STRUCTURE <fs_linedat> TO <fs_value>.
        IF sy-subrc IS INITIAL.
          <fs_value> = _linedat->bukrs.
        ENDIF.
        ASSIGN COMPONENT 'PKIND' OF STRUCTURE <fs_linedat> TO <fs_value>.
        IF sy-subrc IS INITIAL.
          <fs_value> = _linedat->pkind.
        ENDIF.
        ASSIGN COMPONENT 'WAERS' OF STRUCTURE <fs_linedat> TO <fs_value>.
        IF sy-subrc IS INITIAL.
          <fs_value> = _linedat->waers.
        ENDIF.
        ASSIGN COMPONENT 'HEADER' OF STRUCTURE <fs_linedat> TO <fs_value>.
        IF sy-subrc IS INITIAL.
          <fs_value> = _linedat->hname.
        ENDIF.
        ASSIGN COMPONENT 'LQPOS' OF STRUCTURE <fs_linedat> TO <fs_value>.
        IF sy-subrc IS INITIAL.
          <fs_value> = _linedat->lqpos.
        ENDIF.
        ASSIGN COMPONENT 'EBENE' OF STRUCTURE <fs_linedat> TO <fs_value>.
        IF sy-subrc IS INITIAL.
          <fs_value> = _linedat->ebene.
        ENDIF.
*--------------------------------------------------------------------*
*-& Actual Dat->
*--------------------------------------------------------------------*
        LOOP AT mt_keydat REFERENCE INTO DATA(_keydat).

          FREE: _lqday_rng.
          LOOP AT t_logdat REFERENCE INTO DATA(_logdat) WHERE bukrs = _linedat->bukrs AND pkind = _linedat->pkind AND twaer = _linedat->waers AND lqpos = _linedat->lqpos AND ltype = 'A'.
            APPEND VALUE #( sign = 'I' option = 'EQ' low = _logdat->lqday ) TO _lqday_rng.
          ENDLOOP.

          ASSIGN COMPONENT |F_{ _keydat->value }| OF STRUCTURE <fs_linedat> TO <fs_value>.
          IF sy-subrc IS INITIAL.

            IF _lqday_rng[] IS INITIAL.
              <fs_value> = REDUCE wrbtr( INIT val TYPE wrbtr FOR wa_doc IN t_actualdat
                                         WHERE ( lqpos EQ _linedat->lqpos AND twaer EQ _linedat->waers AND lqday IN _keydat->range ) NEXT val = val + wa_doc-wrbtr ).
            ELSE.
              DATA(_amtdoc) = REDUCE wrbtr( INIT val TYPE wrbtr FOR wa_doc IN t_actualdat
                                            WHERE ( lqpos EQ _linedat->lqpos AND twaer EQ _linedat->waers AND lqday IN _keydat->range AND lqday NOT IN _lqday_rng ) NEXT val = val + wa_doc-wrbtr ).

              DATA(_amtlog) = REDUCE wrbtr( INIT val TYPE wrbtr FOR wa_log IN t_logdat
                                            WHERE ( pkind = _linedat->pkind AND twaer = _linedat->waers AND lqpos = _linedat->lqpos AND ltype = 'A' AND lqday IN _keydat->range ) NEXT val = val + wa_log-wrbtr ).

              <fs_value> = _amtdoc + _amtlog.
            ENDIF.

            APPEND LINES OF VALUE yfin_lp_tt01( FOR ls_flqdat IN t_actualdat
              WHERE ( bukrs EQ _linedat->bukrs AND twaer EQ _linedat->waers AND lqpos EQ _linedat->lqpos AND lqday IN _keydat->range )
                    ( CORRESPONDING #( ls_flqdat ) ) ) TO mv_appdat-root_dat-_docdat.

            APPEND LINES OF VALUE tt_logdat( FOR ls_logdat IN t_logdat
              WHERE ( bukrs EQ _linedat->bukrs AND pkind EQ _linedat->pkind AND twaer EQ _linedat->waers AND lqpos EQ _linedat->lqpos AND ltype = 'A' AND lqday IN _keydat->range )
                    ( CORRESPONDING #( ls_logdat ) ) ) TO mv_appdat-root_dat-_logdat.
          ENDIF.

*--------------------------------------------------------------------*
*-& Planner Dat->
*--------------------------------------------------------------------*
          FREE: _lqday_rng.
          LOOP AT t_logdat REFERENCE INTO _logdat WHERE bukrs = _linedat->bukrs AND pkind = _linedat->pkind AND twaer = _linedat->waers AND ebene = _linedat->ebene AND ltype = 'P'.
            APPEND VALUE #( sign = 'I' option = 'EQ' low = _logdat->lqday ) TO _lqday_rng.
          ENDLOOP.

          ASSIGN COMPONENT |P_{ _keydat->value }| OF STRUCTURE <fs_linedat> TO <fs_value>.
          IF sy-subrc IS INITIAL.

            IF _lqday_rng[] IS INITIAL.
              <fs_value> = REDUCE wrbtr( INIT val TYPE wrbtr FOR wa_plandat IN t_plandat
                                         WHERE ( ebene = _linedat->ebene AND twaer EQ _linedat->waers AND lqday IN _keydat->range ) NEXT val = val + wa_plandat-wrbtr ).
            ELSE.
              CLEAR: _amtdoc.
              _amtdoc = REDUCE wrbtr( INIT val TYPE wrbtr FOR wa_plandat IN t_plandat
                                       WHERE ( ebene = _linedat->ebene AND twaer EQ _linedat->waers AND lqday IN _keydat->range AND lqday NOT IN _lqday_rng ) NEXT val = val + wa_plandat-wrbtr ).

              CLEAR: _amtlog.
              _amtlog = REDUCE wrbtr( INIT val TYPE wrbtr FOR wa_log IN t_logdat
                                      WHERE ( pkind = _linedat->pkind AND twaer = _linedat->waers AND ebene = _linedat->ebene AND ltype = 'P' AND lqday IN _keydat->range ) NEXT val = val + wa_log-wrbtr ).

              <fs_value> = _amtdoc + _amtlog.
            ENDIF.

            APPEND LINES OF VALUE yfin_lp_tt01( FOR ls_plandat IN t_plandat
              WHERE ( bukrs EQ _linedat->bukrs AND twaer EQ _linedat->waers AND ebene = _linedat->ebene AND lqday IN _keydat->range )
                    ( CORRESPONDING #( ls_plandat ) ) ) TO mv_appdat-root_dat-_docdat.

            APPEND LINES OF VALUE tt_logdat( FOR ls_logdat IN t_logdat
              WHERE ( bukrs EQ _linedat->bukrs AND pkind EQ _linedat->pkind AND twaer EQ _linedat->waers AND ebene = _linedat->ebene AND ltype = 'P' AND lqday IN _keydat->range )
                    ( CORRESPONDING #( ls_logdat ) ) ) TO mv_appdat-root_dat-_logdat.
          ENDIF.

        ENDLOOP.

        MOVE-CORRESPONDING <fs_linedat> TO <fs_basedat>.
        ASSIGN COMPONENT 'HEADER' OF STRUCTURE <fs_basedat> TO <fs_value>.
        IF sy-subrc IS INITIAL.
          <fs_value> = _basedat->hname.
        ENDIF.
        ASSIGN COMPONENT 'LQPOS' OF STRUCTURE <fs_basedat> TO <fs_value>.
        IF sy-subrc IS INITIAL.
          CLEAR: <fs_value>.
        ENDIF.
        ASSIGN COMPONENT 'EBENE' OF STRUCTURE <fs_basedat> TO <fs_value>.
        IF sy-subrc IS INITIAL.
          CLEAR: <fs_value>.
        ENDIF.
        ASSIGN COMPONENT 'EXPAND' OF STRUCTURE <fs_basedat> TO <fs_value>.
        IF sy-subrc IS INITIAL.
          <fs_value> = _controller->get_icon( iv_type =  'E' ).
        ENDIF.
        COLLECT <fs_basedat> INTO <ft_basedat>.
        APPEND <fs_linedat> TO <ft_linedat>.
      ENDLOOP.
      APPEND mv_appdat TO mt_appdat.
    ENDLOOP.

    ASSIGN mt_alvdat->* TO <ft_alvdat>.
    LOOP AT mt_appdat REFERENCE INTO DATA(_appdat).
      UNASSIGN: <ft_basedat>.
      ASSIGN _appdat->base_tab->* TO <ft_basedat>.
      LOOP AT <ft_basedat> ASSIGNING <fs_basedat>.
        APPEND INITIAL LINE TO <ft_alvdat> ASSIGNING FIELD-SYMBOL(<fs_alvdat>).
        <fs_alvdat> = CORRESPONDING #( <fs_basedat> ).
        ASSIGN COMPONENT 'CELLTAB' OF STRUCTURE <fs_alvdat> TO FIELD-SYMBOL(<fs_celltab>).
        IF sy-subrc IS INITIAL.
          FREE: t_celldat.
          t_celldat = VALUE #( ( fieldname = 'EXPAND' style = cl_gui_alv_grid=>mc_style_hotspot )
                               ( fieldname = 'HEADER' style = '00000020' ) ).
          LOOP AT mt_keydat REFERENCE INTO _keydat.
            INSERT VALUE #(  fieldname = |F_{ _keydat->value }| style = '00000020' ) INTO TABLE t_celldat.
            INSERT VALUE #(  fieldname = |P_{ _keydat->value }| style = '00000020' ) INTO TABLE t_celldat.
          ENDLOOP.
          <fs_celltab> = CORRESPONDING #( t_celldat ).
        ENDIF.
        ASSIGN COMPONENT 'CELLCOLOR' OF STRUCTURE <fs_alvdat> TO FIELD-SYMBOL(<fs_cellcolor>).
        IF sy-subrc IS INITIAL.
          FREE: t_celldat.
          t_cellcolor = VALUE #( ( fname  = 'EXPAND' color = VALUE #( col = 3 int = 0 inv = 0 ) )
                                 ( fname  = 'HEADER' color = VALUE #( col = 3 int = 0 inv = 0 ) ) ).
          LOOP AT mt_keydat REFERENCE INTO _keydat.
            INSERT VALUE #(  fname = |F_{ _keydat->value }| color = VALUE #( col = 3 int = 0 inv = 0 ) ) INTO TABLE t_cellcolor.
            INSERT VALUE #(  fname = |P_{ _keydat->value }| color = VALUE #( col = 3 int = 0 inv = 0 ) ) INTO TABLE t_cellcolor.
          ENDLOOP.
          <fs_cellcolor> = CORRESPONDING #( t_cellcolor ).
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.                    "lcl_mvc_model IMPLEMENTATION
*&---------------------------------------------------------------------*
*&  Class           LCL_MVC_VIEW           Implementation
*&---------------------------------------------------------------------*
CLASS lcl_mvc_view IMPLEMENTATION.

  METHOD constructor.
    mo_model = io_model.
    mo_controller = io_controller.
  ENDMETHOD.                    "constructor
  METHOD display_alvdat.

    IF mo_grid IS NOT BOUND.
*-&Create TOP-Document
      CREATE OBJECT mo_dyndoc_id
        EXPORTING
          style = 'ALV_GRID'.

      CREATE OBJECT mo_splitter
        EXPORTING
          parent  = cl_gui_custom_container=>screen0
          rows    = 2
          columns = 1.

      CALL METHOD mo_splitter->get_container
        EXPORTING
          row       = 1
          column    = 1
        RECEIVING
          container = mo_parent_top.

      CALL METHOD mo_splitter->get_container
        EXPORTING
          row       = 2
          column    = 1
        RECEIVING
          container = mo_parent_grid.

      CALL METHOD mo_splitter->set_row_height
        EXPORTING
          id     = 1
          height = 12.

      CREATE OBJECT mo_grid
        EXPORTING
          i_parent          = mo_parent_grid
          i_lifetime        = cl_gui_alv_grid=>lifetime_dynpro
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
      ENDIF.

*      create_fieldcat(
*        EXPORTING
*          im_strname     = mc_strname
*        EXCEPTIONS
*          contains_error = 1
*          OTHERS         = 2 ).
*      IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
*      ENDIF.

      update_fieldcat(
        EXCEPTIONS
          contains_error = 1
          OTHERS         = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
      ENDIF.

      attach_handlers(
        EXPORTING
          im_grid = mo_grid
        EXCEPTIONS
          contains_error = 1
          OTHERS         = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
      ENDIF.

      FIELD-SYMBOLS: <ft_alvdat> TYPE STANDARD TABLE.
      ASSIGN mo_model->mt_alvdat->* TO <ft_alvdat>.

      mo_grid->set_table_for_first_display(
      EXPORTING
        i_buffer_active               = space
        i_bypassing_buffer            = abap_true
        is_layout                     = set_layout_dat( )
        it_toolbar_excluding          = CONV #( set_exclude_dat( ) )
      CHANGING
        it_fieldcatalog               = mt_fieldcat
        it_outtab                     = <ft_alvdat>
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
      OTHERS                          = 4 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
      ENDIF.

      mo_grid->set_ready_for_input(
        EXPORTING
          i_ready_for_input = 1 ).

      CALL METHOD mo_dyndoc_id->initialize_document
        EXPORTING
          background_color = cl_dd_area=>col_textarea.

      CALL METHOD mo_grid->list_processing_events
        EXPORTING
          i_event_name = 'TOP_OF_PAGE'
          i_dyndoc_id  = mo_dyndoc_id.
    ELSE.
      refresh_alv(
        EXCEPTIONS
          contains_error = 1
          OTHERS         = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "display
  METHOD create_fieldcat.

    FREE: mt_fieldcat.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = im_strname
      CHANGING
        ct_fieldcat            = mt_fieldcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
    ENDIF.

  ENDMETHOD.                    "create_fieldcat
  METHOD update_fieldcat.

    DATA: _text(40) TYPE c.
    LOOP AT mt_fieldcat REFERENCE INTO DATA(r_fieldcat).
      CLEAR: _text.
      CASE r_fieldcat->fieldname .
        WHEN 'EXPAND'.
          r_fieldcat->just = 'C'.
        WHEN 'BUKRS' OR 'PKIND' OR 'WAERS' OR 'LQPOS' OR 'EBENE'.
          r_fieldcat->no_out = abap_true.
      ENDCASE.
      IF _text <> space.
        MOVE _text TO: r_fieldcat->scrtext_l, r_fieldcat->scrtext_m, r_fieldcat->scrtext_s, r_fieldcat->reptext.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "update_fieldcat
  METHOD set_layout_dat.

    rv_layoutdat = VALUE lvc_s_layo( col_opt = abap_true cwidth_opt = abap_true zebra = abap_true stylefname = 'CELLTAB' ctab_fname = 'CELLCOLOR' ).

  ENDMETHOD.                    "set_layout_dat
  METHOD set_exclude_dat.
    rv_excludedat = VALUE #( ( cl_gui_alv_grid=>mc_fc_f4 )      ( cl_gui_alv_grid=>mc_fc_view_lotus ) ( cl_gui_alv_grid=>mc_fc_current_variant  )
                             ( cl_gui_alv_grid=>mc_fc_auf )     ( cl_gui_alv_grid=>mc_fc_pc_file )    ( cl_gui_alv_grid=>mc_fc_call_report )
                             ( cl_gui_alv_grid=>mc_fc_sum )     ( cl_gui_alv_grid=>mc_fc_refresh )    ( cl_gui_alv_grid=>mc_fc_fix_columns )
                             ( cl_gui_alv_grid=>mc_mb_sum )     ( cl_gui_alv_grid=>mc_mb_variant )    ( cl_gui_alv_grid=>mc_fc_to_rep_tree )
                             ( cl_gui_alv_grid=>mc_fc_find )    ( cl_gui_alv_grid=>mc_fc_call_abc )   ( cl_gui_alv_grid=>mc_fc_back_classic )
                             ( cl_gui_alv_grid=>mc_fc_help )    ( cl_gui_alv_grid=>mc_fc_maximum )    ( cl_gui_alv_grid=>mc_fc_call_crbatch )
                             ( cl_gui_alv_grid=>mc_fc_info )    ( cl_gui_alv_grid=>mc_fc_loc_copy )   ( cl_gui_alv_grid=>mc_fc_deselect_all )
                             ( cl_gui_alv_grid=>mc_fc_send )    ( cl_gui_alv_grid=>mc_fc_loc_undo )   ( cl_gui_alv_grid=>mc_fc_load_variant )
                             ( cl_gui_alv_grid=>mc_fc_sort )    ( cl_gui_alv_grid=>mc_fc_sort_asc )   ( cl_gui_alv_grid=>mc_fc_loc_copy_row )
                             ( cl_gui_alv_grid=>mc_mb_view )    ( cl_gui_alv_grid=>mc_fc_sort_dsc )   ( cl_gui_alv_grid=>mc_fc_loc_move_row )
                             ( cl_gui_alv_grid=>mc_fc_check )   ( cl_gui_alv_grid=>mc_fc_call_more )  ( cl_gui_alv_grid=>mc_fc_save_variant )
                             ( cl_gui_alv_grid=>mc_fc_count )   ( cl_gui_alv_grid=>mc_fc_call_xint )  ( cl_gui_alv_grid=>mc_fc_view_crystal )
                             ( cl_gui_alv_grid=>mc_fc_graph )   ( cl_gui_alv_grid=>mc_fc_data_save )  ( cl_gui_alv_grid=>mc_fc_col_invisible )
                             ( cl_gui_alv_grid=>mc_fc_print )   ( cl_gui_alv_grid=>mc_fc_expcrdata )  ( cl_gui_alv_grid=>mc_fc_delete_filter )
                             ( cl_gui_alv_grid=>mc_fc_views )   ( cl_gui_alv_grid=>mc_fc_find_more )  ( cl_gui_alv_grid=>mc_fc_unfix_columns )
                             ( cl_gui_alv_grid=>mc_mb_paste )   ( cl_gui_alv_grid=>mc_fc_loc_paste )  ( cl_gui_alv_grid=>mc_fc_variant_admin )
                             ( cl_gui_alv_grid=>mc_fc_detail )  ( cl_gui_alv_grid=>mc_fc_separator )  ( cl_gui_alv_grid=>mc_fc_call_lineitems )
                             ( cl_gui_alv_grid=>mc_fc_expmdb )  ( cl_gui_alv_grid=>mc_fc_to_office )  ( cl_gui_alv_grid=>mc_fc_loc_append_row )
                             ( cl_gui_alv_grid=>mc_fc_extend )  ( cl_gui_alv_grid=>mc_fc_view_grid )  ( cl_gui_alv_grid=>mc_fc_loc_delete_row )
                             ( cl_gui_alv_grid=>mc_fc_filter )  ( cl_gui_alv_grid=>mc_fc_call_chain ) ( cl_gui_alv_grid=>mc_fc_loc_insert_row )
                             ( cl_gui_alv_grid=>mc_fc_reprep )  ( cl_gui_alv_grid=>mc_fc_call_crweb ) ( cl_gui_alv_grid=>mc_fc_word_processor )
                             ( cl_gui_alv_grid=>mc_fc_subtot )  ( cl_gui_alv_grid=>mc_fc_expcrdesig ) ( cl_gui_alv_grid=>mc_fc_call_xml_export )
                             ( cl_gui_alv_grid=>mc_mb_filter )  ( cl_gui_alv_grid=>mc_fc_print_back ) ( cl_gui_alv_grid=>mc_fc_call_master_data )
                             ( cl_gui_alv_grid=>mc_mb_subtot )  ( cl_gui_alv_grid=>mc_fc_print_prev ) ( cl_gui_alv_grid=>mc_fc_maintain_variant )
                             ( cl_gui_alv_grid=>mc_fc_average ) ( cl_gui_alv_grid=>mc_fc_select_all ) ( cl_gui_alv_grid=>mc_fc_loc_paste_new_row )
                             ( cl_gui_alv_grid=>mc_fc_loc_cut ) ( cl_gui_alv_grid=>mc_fc_view_excel ) ( cl_gui_alv_grid=>mc_fc_url_copy_to_clipboard )
                             ( cl_gui_alv_grid=>mc_fc_minimum ) ( cl_gui_alv_grid=>mc_fc_expcrtempl ) ( cl_gui_alv_grid=>mc_fc_col_optimize ) ).
  ENDMETHOD.                    "set_exclude_dat
  METHOD attach_handlers.

    CALL METHOD im_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    SET HANDLER mo_controller->handle_hotspot_click FOR im_grid.
    SET HANDLER mo_controller->handle_toolbar_set   FOR im_grid.
    SET HANDLER mo_controller->handle_user_command  FOR im_grid.
    SET HANDLER mo_controller->handle_data_finished FOR im_grid.
    SET HANDLER mo_controller->handle_top_of_page   FOR im_grid.
  ENDMETHOD.                    "attach_handlers
  METHOD refresh_alv.

    mo_grid->refresh_table_display(
      EXPORTING
        is_stable      = VALUE #( row = abap_true col = abap_true )
        i_soft_refresh = abap_true
      EXCEPTIONS
        finished       = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
    ENDIF.

  ENDMETHOD.                    "refresh_alv
ENDCLASS.                    "lcl_mvc_view IMPLEMENTATION
*&---------------------------------------------------------------------*
*&  Class           LCL_MVC_CONTROLLER           Implementation
*&---------------------------------------------------------------------*
CLASS lcl_mvc_controller IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.                    "constructor
  METHOD instantiate_app.

    DATA: lo_object TYPE REF TO object.

    ro_controller = NEW lcl_mvc_controller( ).

    FREE: lo_object.
    CREATE OBJECT lo_object TYPE (iv_model).
    IF lo_object IS BOUND.
      mo_model ?= lo_object.
      ro_controller->mo_model ?= mo_model.
    ENDIF.

    FREE: lo_object.
    CREATE OBJECT lo_object TYPE (iv_view)
      EXPORTING
        io_model      = ro_controller->mo_model
        io_controller = me.
    IF lo_object IS BOUND.
      mo_view ?= lo_object.
      ro_controller->mo_view ?= mo_view.
    ENDIF.

  ENDMETHOD.                    "instantiate_app
  METHOD initialization.

    FREE: s_keydat[].
    s_keydat[] = VALUE #( sign = 'I' option = 'BT' ( low = sy-datum high = sy-datum ) ).
    listbox_build( ).

  ENDMETHOD.                    "initialization
  METHOD at_selection_screen.
    user_commmand_selscr(
      CHANGING
          cv_ucomm = sy-ucomm ).
  ENDMETHOD.                    "at_selection_screen
  METHOD user_commmand_selscr.
    CASE cv_ucomm.
      WHEN 'ONLI'.
        NEW yfin_lp_cl01( )->dynamic_alvdat(
          EXPORTING
            iv_tmunit      = p_tunit
            iv_begdat      = s_keydat-low
            iv_enddat      = s_keydat-high
          RECEIVING
            rv_alvdat      = DATA(_alvdat)
          EXCEPTIONS
            contains_error = 1
            OTHERS         = 2 ).
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ELSE.
          mo_model->mt_alvdat ?= _alvdat-alvoutput.
          mo_model->mt_keydat = _alvdat-keydate.
          mo_view->mt_fieldcat = _alvdat-fieldcat.
        ENDIF.
    ENDCASE.
  ENDMETHOD.                    "user_commmand_selscr
  METHOD start_of_selection.

    mo_model->retrieve_dat(
      EXPORTING
        im_bukrs       = p_bukrs
        im_tunit       = p_tunit
        im_keyda       = s_keydat[]
        im_ldays       = p_ldays
      EXCEPTIONS
        contains_error = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4. RETURN.
    ENDIF.

    CALL SCREEN 100.

  ENDMETHOD.                    "start_of_selection
  METHOD listbox_build.

    DATA: _values TYPE vrm_values,
          _id     TYPE vrm_id.

    FREE: _values, _id.
    _id = 'P_TUNIT'.
    _values = VALUE #( BASE _values ( key = 'D' text = 'Günlük' )
                                    ( key = 'W' text = 'Haftalık' )
                                    ( key = 'M' text = 'Aylık' )
                                    ( key = 'Y' text = 'Yıllık' ) ).
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = _id
        values = _values.

  ENDMETHOD.                    "listbox_build
  METHOD alv_session.

    IF NOT mo_model->mt_alvdat IS INITIAL.
      mo_view->display_alvdat(
        EXCEPTIONS
          contains_error = 1
          OTHERS         = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "alv_session
  METHOD display_linedat.
    CALL FUNCTION 'YFIN_LP_FM01'
      EXPORTING
        iv_tunit        = p_tunit
        iv_strname_tab1 = 'YFIN_LP_S01'
        iv_strname_tab2 = 'YFIN_LP_S02'
      IMPORTING
        ev_action       = ev_action
      TABLES
        t_alvdat_tab1   = iv_actualdat
        t_alvdat_tab2   = iv_manueldat.
  ENDMETHOD.                    "display_linedat
  METHOD reject_manueldat.

    TYPES: BEGIN OF ty_keycolmn,
             bukrs  TYPE yfin_lp_t01-bukrs,
             pkind  TYPE yfin_lp_t01-pkind,
             waers  TYPE yfin_lp_t01-waers,
             expand TYPE text40,
             lqpos  TYPE yfin_lp_t02-lqpos,
             ebene  TYPE yfin_lp_t02-ebene,
           END OF ty_keycolmn.

    DATA: _keycolmn TYPE ty_keycolmn,
          _total    TYPE wrbtr.

    FIELD-SYMBOLS: <ft_alvdat>  TYPE STANDARD TABLE,
                   <ft_linedat> TYPE STANDARD TABLE.

    READ TABLE mo_model->mt_appdat REFERENCE INTO DATA(_appdat) WITH KEY bukrs = iv_keydat-bukrs waers = iv_keydat-waers pkind = iv_keydat-pkind.
    IF sy-subrc IS INITIAL.

      GET TIME.
      UPDATE yfin_lp_t03 SET xdele = abap_true erdat = sy-datum erzet = sy-uzeit ernam = sy-uname
        WHERE bukrs = iv_keydat-bukrs AND pkind = iv_keydat-pkind AND twaer = iv_keydat-waers AND lqpos = iv_keydat-lqpos AND lqday = iv_column_id-fieldname+2(27) AND ltype = iv_keydat-ltype.
      IF sy-subrc IS INITIAL.
        DELETE _appdat->root_dat-_logdat WHERE bukrs = iv_keydat-bukrs AND
                                               pkind = iv_keydat-pkind AND
                                               twaer = iv_keydat-waers AND
                                               lqpos = iv_keydat-lqpos AND
                                               lqday = iv_column_id-fieldname+2(27) AND
                                               ltype = iv_keydat-ltype.

        ASSIGN mo_model->mt_alvdat->* TO <ft_alvdat>.
        ASSIGN <ft_alvdat>[ iv_row_no-row_id ] TO FIELD-SYMBOL(<fs_alvdat>).

        ASSIGN COMPONENT iv_column_id-fieldname OF STRUCTURE <fs_alvdat> TO FIELD-SYMBOL(<amount>).
        <amount> = REDUCE wrbtr( INIT val TYPE wrbtr FOR wa_docdat IN _appdat->root_dat-_docdat
                    WHERE ( bukrs = iv_keydat-bukrs AND twaer = iv_keydat-waers AND lqpos = iv_keydat-lqpos AND ltype = iv_keydat-ltype AND lqday IN iv_keydat-lqday[] ) NEXT val = val + wa_docdat-wrbtr ).

        UNASSIGN: <ft_linedat>.
        ASSIGN _appdat->line_tab->* TO <ft_linedat>.
        LOOP AT <ft_linedat> ASSIGNING FIELD-SYMBOL(<fs_linedat>).
          ASSIGN COMPONENT iv_column_id-fieldname OF STRUCTURE <fs_linedat> TO FIELD-SYMBOL(<fs_lineval>).
          IF <fs_lineval> IS ASSIGNED.
            CLEAR: _keycolmn.
            _keycolmn = CORRESPONDING #( <fs_linedat> ).
            IF _keycolmn-bukrs EQ iv_keydat-bukrs AND _keycolmn-pkind = iv_keydat-pkind AND _keycolmn-waers = iv_keydat-waers AND _keycolmn-lqpos = iv_keydat-lqpos.
              <fs_lineval> = <amount>.
            ENDIF.
            _total = _total + <fs_lineval>.
          ENDIF.
        ENDLOOP.
        FIELD-SYMBOLS: <ft_basedat> TYPE STANDARD TABLE.

        LOOP AT <ft_alvdat> ASSIGNING <fs_alvdat>.
          CLEAR: _keycolmn.
          _keycolmn = CORRESPONDING #( <fs_alvdat> ).
          CHECK _keycolmn-bukrs = iv_keydat-bukrs AND _keycolmn-pkind = iv_keydat-pkind AND _keycolmn-waers = iv_keydat-waers AND _keycolmn-expand <> space.
          UNASSIGN: <amount>.
          ASSIGN COMPONENT iv_column_id-fieldname OF STRUCTURE <fs_alvdat> TO <amount>.
          IF <amount> IS ASSIGNED.
            <amount> = _total. EXIT.
          ENDIF.
        ENDLOOP.

        UNASSIGN: <ft_basedat>.
        ASSIGN _appdat->base_tab->* TO <ft_basedat>.
        READ TABLE <ft_basedat> ASSIGNING FIELD-SYMBOL(<fs_basedat>) INDEX 1.
        IF sy-subrc IS INITIAL.
          ASSIGN COMPONENT iv_column_id-fieldname OF STRUCTURE <fs_basedat> TO FIELD-SYMBOL(<fs_baseval>).
          IF <fs_baseval> IS ASSIGNED.
            <fs_baseval> = _total.
          ENDIF.
        ENDIF.
      ELSE.
        MESSAGE e011(yfin_lp) RAISING contains_error.
      ENDIF.
    ELSE.
      MESSAGE e011(yfin_lp) RAISING contains_error.
    ENDIF.

  ENDMETHOD.                    "reject_manueldat
  METHOD save_logdat.

    LOOP AT mo_model->mt_appdat INTO mo_model->mv_appdat.
      LOOP AT mo_model->mv_appdat-root_dat-_logdat REFERENCE INTO DATA(_logdat).
        UPDATE yfin_lp_t03 SET xdele = abap_true erdat = sy-datum erzet = sy-uzeit ernam = sy-uname
          WHERE bukrs = _logdat->bukrs
            AND pkind = _logdat->pkind
            AND twaer = _logdat->twaer
            AND lqpos = _logdat->lqpos
            AND ebene = _logdat->ebene
            AND lqday = _logdat->lqday
            AND ltype = _logdat->ltype.
        COMMIT WORK.

        MODIFY yfin_lp_t03 FROM _logdat->*.
        COMMIT WORK.

      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.                    "save_logdat
  METHOD get_icon.
    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name                  = SWITCH #( iv_type WHEN 'E' THEN icon_expand
                                                  WHEN 'C' THEN icon_collapse )
        info                  = SWITCH text40( iv_type WHEN 'E' THEN 'Expand Details'(e02)
                                                       WHEN 'C' THEN 'Collapse Details'(c02) )
        add_stdinf            = ' '
      IMPORTING
        result                = rv_icon
      EXCEPTIONS
        icon_not_found        = 0
        outputfield_too_short = 0
        OTHERS                = 0.
  ENDMETHOD.                    "get_icon
  METHOD get_long_date.

    DATA: t_month_names TYPE TABLE OF t247,
          r_month_names TYPE REF TO t247,
          l_daytxt      TYPE hrvsched-daytxt,
          l_year(4)     TYPE c,
          l_month(2)    TYPE c,
          l_day(2)      TYPE c.

    FREE: t_month_names.
    CALL FUNCTION 'MONTH_NAMES_GET'
      EXPORTING
        language    = sy-langu
      TABLES
        month_names = t_month_names.

    l_year = im_dat+0(4).
    l_month = im_dat+4(2).
    l_day = im_dat+6(2).

    CLEAR: l_daytxt.
    CALL FUNCTION 'RH_GET_DATE_DAYNAME'
      EXPORTING
        langu               = sy-langu
        date                = im_dat
      IMPORTING
        daytxt              = l_daytxt
      EXCEPTIONS
        no_langu            = 1
        no_date             = 2
        no_daytxt_for_langu = 3
        invalid_date        = 4
        OTHERS              = 5.

    READ TABLE t_month_names REFERENCE INTO r_month_names INDEX l_month.
    IF sy-subrc IS INITIAL.
      CONCATENATE l_day r_month_names->ltx l_year l_daytxt INTO r_dattxt SEPARATED BY space.
    ENDIF.

  ENDMETHOD.              "get_long_date
  METHOD _expand_all_alvdat.

    DATA: t_celldat TYPE lvc_t_styl.
    FIELD-SYMBOLS: <ft_alvdat> TYPE STANDARD TABLE.

    ASSIGN mo_model->mt_alvdat->* TO <ft_alvdat>.
    LOOP AT <ft_alvdat> ASSIGNING FIELD-SYMBOL(<fs_alvdat>).
      DATA(_tabix) = sy-tabix.
      ASSIGN COMPONENT 'EXPAND' OF STRUCTURE <fs_alvdat> TO FIELD-SYMBOL(<expand>).
      IF <expand>(3) EQ icon_expand(3).
        rv_refresh = abap_true.
        <expand> = get_icon( iv_type = 'C' ).
        ASSIGN COMPONENT 'BUKRS' OF STRUCTURE <fs_alvdat> TO FIELD-SYMBOL(<bukrs>).
        ASSIGN COMPONENT 'PKIND' OF STRUCTURE <fs_alvdat> TO FIELD-SYMBOL(<pkind>).
        ASSIGN COMPONENT 'WAERS' OF STRUCTURE <fs_alvdat> TO FIELD-SYMBOL(<waers>).
        FREE: mo_model->mv_appdat.
        READ TABLE mo_model->mt_appdat INTO mo_model->mv_appdat WITH KEY bukrs = <bukrs> pkind = <pkind> waers = <waers>.
        IF sy-subrc IS INITIAL.
          FIELD-SYMBOLS: <ft_linedat> TYPE STANDARD TABLE.
          DATA(lv_add_subrows_index) = _tabix + 1.
          ASSIGN mo_model->mv_appdat-line_tab->* TO <ft_linedat>.
          LOOP AT <ft_linedat> ASSIGNING FIELD-SYMBOL(<fs_linedat>).
            INSERT INITIAL LINE INTO <ft_alvdat> ASSIGNING FIELD-SYMBOL(<alvdat>) INDEX lv_add_subrows_index.
            <alvdat> = CORRESPONDING #( <fs_linedat> ).
            ASSIGN COMPONENT 'CELLTAB' OF STRUCTURE <alvdat> TO FIELD-SYMBOL(<fs_celltab>).
            IF sy-subrc IS INITIAL.
              FREE: t_celldat.
              LOOP AT mo_model->mt_keydat REFERENCE INTO DATA(_keydat).
                t_celldat = VALUE #( BASE t_celldat ( fieldname = |F_{ _keydat->value }| style = cl_gui_alv_grid=>mc_style_hotspot )
                                                    ( fieldname = |P_{ _keydat->value }| style = cl_gui_alv_grid=>mc_style_hotspot ) ).
              ENDLOOP.
              <fs_celltab> = CORRESPONDING #( t_celldat ).
            ENDIF.
            ADD 1 TO lv_add_subrows_index.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.              "_expand_all_alvdat
  METHOD _collapse_all_alvdat.

    TYPES:
      BEGIN OF ty_deldat,
        bukrs  TYPE yfin_lp_t01-bukrs,
        pkind  TYPE yfin_lp_t01-pkind,
        waers  TYPE yfin_lp_t01-waers,
        expand TYPE text40,
      END OF ty_deldat.

    DATA: _deldat   TYPE ty_deldat.

    FIELD-SYMBOLS: <ft_alvdat> TYPE STANDARD TABLE.

    ASSIGN mo_model->mt_alvdat->* TO <ft_alvdat>.
    LOOP AT <ft_alvdat> ASSIGNING FIELD-SYMBOL(<fs_alvdat>).
      ASSIGN COMPONENT 'EXPAND' OF STRUCTURE <fs_alvdat> TO FIELD-SYMBOL(<expand>).
      IF <expand>(3) EQ icon_collapse(3).
        rv_refresh = abap_true.
        <expand> = get_icon( iv_type = 'E' ).
        ASSIGN COMPONENT 'BUKRS' OF STRUCTURE <fs_alvdat> TO FIELD-SYMBOL(<bukrs>).
        ASSIGN COMPONENT 'PKIND' OF STRUCTURE <fs_alvdat> TO FIELD-SYMBOL(<pkind>).
        ASSIGN COMPONENT 'WAERS' OF STRUCTURE <fs_alvdat> TO FIELD-SYMBOL(<waers>).
        LOOP AT <ft_alvdat> ASSIGNING FIELD-SYMBOL(<fs_deldat>).
          DATA(_index) = sy-tabix.
          _deldat = CORRESPONDING #( <fs_deldat> ).
          IF _deldat-bukrs EQ <bukrs> AND _deldat-pkind EQ <pkind> AND _deldat-waers EQ <waers> AND _deldat-expand IS INITIAL.
            DELETE <ft_alvdat> INDEX _index.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.              "_collapse_all_alvdat
  METHOD _editable_alvdat.

    DATA: t_celldat TYPE lvc_t_styl.

    FIELD-SYMBOLS: <ft_alvdat> TYPE STANDARD TABLE.

    ASSIGN mo_model->mt_alvdat->* TO <ft_alvdat>.
    LOOP AT <ft_alvdat> ASSIGNING FIELD-SYMBOL(<fs_alvdat>).
      ASSIGN COMPONENT 'EXPAND' OF STRUCTURE <fs_alvdat> TO FIELD-SYMBOL(<expand>).
      IF <expand> IS INITIAL.
        ASSIGN COMPONENT 'CELLTAB' OF STRUCTURE <fs_alvdat> TO FIELD-SYMBOL(<celltab>).
        IF sy-subrc IS INITIAL.
          FREE: t_celldat.
          LOOP AT mo_model->mt_keydat REFERENCE INTO DATA(_keydat).
            t_celldat = VALUE #( BASE t_celldat ( fieldname = |F_{ _keydat->value }| style = cl_gui_alv_grid=>mc_style_enabled )
                                                ( fieldname = |P_{ _keydat->value }| style = cl_gui_alv_grid=>mc_style_enabled ) ).
          ENDLOOP.
          <celltab> = CORRESPONDING #( t_celldat ).
          rv_refresh = abap_true.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.              "_editable_alvdat
  METHOD handle_hotspot_click.

    FIELD-SYMBOLS: <ft_alvdat> TYPE STANDARD TABLE.

    TYPES: BEGIN OF ty_delcolmn,
             bukrs  TYPE yfin_lp_t01-bukrs,
             pkind  TYPE yfin_lp_t01-pkind,
             waers  TYPE yfin_lp_t01-waers,
             expand TYPE text40,
           END OF ty_delcolmn.

    TYPES: BEGIN OF ty_keycolmn,
             bukrs  TYPE yfin_lp_t01-bukrs,
             pkind  TYPE yfin_lp_t01-pkind,
             waers  TYPE yfin_lp_t01-waers,
             expand TYPE text40,
             lqpos  TYPE yfin_lp_t02-lqpos,
             ebene  TYPE yfin_lp_t02-ebene,
           END OF ty_keycolmn.

    DATA: _delcolmn TYPE ty_delcolmn,
          _keycolmn TYPE ty_keycolmn,
          _total    TYPE wrbtr,
          t_celldat TYPE lvc_t_styl.

    CASE e_column_id.
      WHEN 'EXPAND'.
        ASSIGN mo_model->mt_alvdat->* TO <ft_alvdat>.
        ASSIGN <ft_alvdat>[ es_row_no-row_id ] TO FIELD-SYMBOL(<fs_alvdat>).

        ASSIGN COMPONENT 'BUKRS' OF STRUCTURE <fs_alvdat> TO FIELD-SYMBOL(<bukrs>).
        ASSIGN COMPONENT 'PKIND' OF STRUCTURE <fs_alvdat> TO FIELD-SYMBOL(<pkind>).
        ASSIGN COMPONENT 'WAERS' OF STRUCTURE <fs_alvdat> TO FIELD-SYMBOL(<waers>).
        ASSIGN COMPONENT e_column_id OF STRUCTURE <fs_alvdat> TO FIELD-SYMBOL(<expand>).
        IF <expand>(3) EQ icon_expand(3).
          <expand> = get_icon( iv_type = 'C' ).
          FREE: mo_model->mv_appdat.
          READ TABLE mo_model->mt_appdat INTO mo_model->mv_appdat WITH KEY bukrs = <bukrs> pkind = <pkind> waers = <waers>.
          IF sy-subrc IS INITIAL.
            FIELD-SYMBOLS: <ft_linedat> TYPE STANDARD TABLE.
            DATA(lv_add_subrows_index) = es_row_no-row_id + 1.
            ASSIGN mo_model->mv_appdat-line_tab->* TO <ft_linedat>.
            LOOP AT <ft_linedat> ASSIGNING FIELD-SYMBOL(<fs_linedat>).
              INSERT INITIAL LINE INTO <ft_alvdat> ASSIGNING FIELD-SYMBOL(<alvdat>) INDEX lv_add_subrows_index.
              <alvdat> = CORRESPONDING #( <fs_linedat> ).
              ASSIGN COMPONENT 'CELLTAB' OF STRUCTURE <alvdat> TO FIELD-SYMBOL(<fs_celltab>).
              IF sy-subrc IS INITIAL.
                FREE: t_celldat.
                LOOP AT mo_model->mt_keydat REFERENCE INTO DATA(_keydat).
                  t_celldat = VALUE #( BASE t_celldat ( fieldname = |F_{ _keydat->value }| style = cl_gui_alv_grid=>mc_style_hotspot )
                                                      ( fieldname = |P_{ _keydat->value }| style = cl_gui_alv_grid=>mc_style_hotspot ) ).
                ENDLOOP.
                <fs_celltab> = CORRESPONDING #( t_celldat ).
              ENDIF.
              ADD 1 TO lv_add_subrows_index.
            ENDLOOP.
          ENDIF.
        ELSE.
          <expand> = get_icon( iv_type = 'E' ).
          LOOP AT <ft_alvdat> ASSIGNING FIELD-SYMBOL(<fs_deldat>).
            DATA(_index) = sy-tabix.
            _delcolmn = CORRESPONDING #( <fs_deldat> ).
            IF _delcolmn-bukrs EQ <bukrs> AND _delcolmn-pkind EQ <pkind> AND _delcolmn-waers EQ <waers> AND _delcolmn-expand IS INITIAL.
              DELETE <ft_alvdat> INDEX _index.
            ENDIF.
          ENDLOOP.
        ENDIF.
      WHEN OTHERS.
        CASE e_column_id(2).
          WHEN 'F_'.
            ASSIGN mo_model->mt_alvdat->* TO <ft_alvdat>.
            ASSIGN <ft_alvdat>[ es_row_no-row_id ] TO <fs_alvdat>.
            ASSIGN COMPONENT 'BUKRS' OF STRUCTURE <fs_alvdat> TO <bukrs>.
            ASSIGN COMPONENT 'WAERS' OF STRUCTURE <fs_alvdat> TO <waers>.
            ASSIGN COMPONENT 'PKIND' OF STRUCTURE <fs_alvdat> TO <pkind>.
            ASSIGN COMPONENT 'LQPOS' OF STRUCTURE <fs_alvdat> TO FIELD-SYMBOL(<lqpos>).
            ASSIGN COMPONENT 'EBENE' OF STRUCTURE <fs_alvdat> TO FIELD-SYMBOL(<ebene>).
            READ TABLE mo_model->mt_keydat REFERENCE INTO _keydat WITH KEY value = e_column_id-fieldname+2(27).
            IF sy-subrc IS INITIAL.

              READ TABLE mo_model->mt_appdat REFERENCE INTO DATA(_appdat) WITH KEY bukrs = <bukrs> waers = <waers> pkind = <pkind>.
              IF sy-subrc IS INITIAL.
                FREE: mo_model->mt_actualdat, mo_model->mt_manueldat.
                APPEND LINES OF VALUE yfin_lp_tt01( FOR ls_docdat IN _appdat->root_dat-_docdat WHERE ( ltype = 'A' AND lqday IN _keydat->range AND lqpos = <lqpos> ) ( CORRESPONDING #( ls_docdat ) ) ) TO mo_model->mt_actualdat.
                APPEND LINES OF VALUE yfin_lp_tt02( FOR ls_logdat IN _appdat->root_dat-_logdat WHERE ( ltype = 'A' AND lqday IN _keydat->range AND lqpos = <lqpos> ) ( CORRESPONDING #( ls_logdat ) ) ) TO mo_model->mt_manueldat.
                me->display_linedat(
                  EXPORTING
                    iv_actualdat = mo_model->mt_actualdat
                    iv_manueldat = mo_model->mt_manueldat
                  IMPORTING
                    ev_action = DATA(_action)
                  EXCEPTIONS
                    contains_error = 1
                    OTHERS         = 2 ).
                IF sy-subrc <> 0.
                  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
                ELSE.
                  CASE _action.
                    WHEN '&RECALL'.
                      reject_manueldat(
                        EXPORTING
                          iv_keydat    = VALUE #( bukrs = <bukrs> pkind = <pkind> waers = <waers> lqpos = <lqpos> ebene = <ebene> lqday = _keydat->range ltype = 'A' )
                          iv_column_id = e_column_id
                          iv_row_no    = es_row_no
                       EXCEPTIONS
                         contains_error = 1
                         OTHERS         = 2 ).
                      IF sy-subrc <> 0.
                        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
                      ENDIF.
                  ENDCASE.
                ENDIF.
              ENDIF.
            ENDIF.
          WHEN 'P_'.
            ASSIGN mo_model->mt_alvdat->* TO <ft_alvdat>.
            ASSIGN <ft_alvdat>[ es_row_no-row_id ] TO <fs_alvdat>.
            ASSIGN COMPONENT 'BUKRS' OF STRUCTURE <fs_alvdat> TO <bukrs>.
            ASSIGN COMPONENT 'WAERS' OF STRUCTURE <fs_alvdat> TO <waers>.
            ASSIGN COMPONENT 'PKIND' OF STRUCTURE <fs_alvdat> TO <pkind>.
            ASSIGN COMPONENT 'LQPOS' OF STRUCTURE <fs_alvdat> TO <lqpos>.
            ASSIGN COMPONENT 'EBENE' OF STRUCTURE <fs_alvdat> TO <ebene>.
            READ TABLE mo_model->mt_keydat REFERENCE INTO _keydat WITH KEY value = e_column_id-fieldname+2(27).
            IF sy-subrc IS INITIAL.

              READ TABLE mo_model->mt_appdat REFERENCE INTO _appdat WITH KEY bukrs = <bukrs> waers = <waers> pkind = <pkind>.
              IF sy-subrc IS INITIAL.
                FREE: mo_model->mt_actualdat, mo_model->mt_manueldat.
                APPEND LINES OF VALUE yfin_lp_tt01( FOR ls_docdat IN _appdat->root_dat-_docdat WHERE ( ltype = 'P' AND lqday IN _keydat->range AND ebene = <ebene> ) ( CORRESPONDING #( ls_docdat ) ) ) TO mo_model->mt_actualdat.
                APPEND LINES OF VALUE yfin_lp_tt02( FOR ls_logdat IN _appdat->root_dat-_logdat WHERE ( ltype = 'P' AND lqday IN _keydat->range AND ebene = <ebene> ) ( CORRESPONDING #( ls_logdat ) ) ) TO mo_model->mt_manueldat.
                me->display_linedat(
                  EXPORTING
                    iv_actualdat = mo_model->mt_actualdat
                    iv_manueldat = mo_model->mt_manueldat
                  IMPORTING
                    ev_action = _action
                  EXCEPTIONS
                    contains_error = 1
                    OTHERS         = 2 ).
                IF sy-subrc <> 0.
                  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
                ELSE.
                  CASE _action.
                    WHEN '&RECALL'.
                      reject_manueldat(
                        EXPORTING
                          iv_keydat    = VALUE #( bukrs = <bukrs> pkind = <pkind> waers = <waers> lqpos = <lqpos> ebene = <ebene> lqday = _keydat->range ltype = 'P' )
                          iv_column_id = e_column_id
                          iv_row_no    = es_row_no
                       EXCEPTIONS
                         contains_error = 1
                         OTHERS         = 2 ).
                      IF sy-subrc <> 0.
                        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
                      ENDIF.
                  ENDCASE.
                ENDIF.
              ENDIF.
            ENDIF.
        ENDCASE.
    ENDCASE.

    mo_view->mo_grid->refresh_table_display(
      EXPORTING
        is_stable      = VALUE #( row = abap_true col = abap_true )
        i_soft_refresh = abap_true
      EXCEPTIONS
        finished       = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.              "handle_hotspot_click
  METHOD handle_toolbar_set.

    DELETE e_object->mt_toolbar WHERE function = '&LOCAL&COPY_ROW'
                                   OR function = '&LOCAL&APPEND'
                                   OR function = '&LOCAL&INSERT_ROW'
                                   OR function = '&LOCAL&DELETE_ROW'
                                   OR function = '&LOCAL&CUT'
                                   OR function = '&LOCAL&COPY'
                                   OR function = '&LOCAL&PASTE'.

    APPEND LINES OF VALUE ttb_button( ( function = 'EXPALL'
                                        icon = icon_expand_all
                                        quickinfo = 'Tümünü Aç' )
                                      ( function = 'COLALL'
                                        icon = icon_collapse_all
                                        quickinfo = 'Tümünü Kapat' ) ) TO e_object->mt_toolbar.
    IF p_tunit EQ 'D'.
      APPEND VALUE #( function  = '&EDITDAT'
                      icon      = icon_budget_update
                      text      = 'Güncelle'
                      quickinfo = 'Güncelleme işlemleri' ) TO e_object->mt_toolbar.
    ENDIF.

  ENDMETHOD.                    "handle_toolbar
  METHOD handle_user_command.

    CASE e_ucomm.
      WHEN 'EXPALL'.
        IF _expand_all_alvdat( ) EQ abap_true.
          mo_view->mo_grid->refresh_table_display(
            EXPORTING
              is_stable      = VALUE #( row = abap_true col = abap_true )
              i_soft_refresh = abap_true
            EXCEPTIONS
              finished       = 1
              OTHERS         = 2 ).
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDIF.
      WHEN 'COLALL'.
        IF _collapse_all_alvdat( ) EQ abap_true.
          mo_view->mo_grid->refresh_table_display(
            EXPORTING
              is_stable      = VALUE #( row = abap_true col = abap_true )
              i_soft_refresh = abap_true
            EXCEPTIONS
              finished       = 1
              OTHERS         = 2 ).
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDIF.

      WHEN '&EDITDAT'.
        IF _editable_alvdat( ) EQ abap_true.
          mo_view->mo_grid->refresh_table_display(
            EXPORTING
              is_stable      = VALUE #( row = abap_true col = abap_true )
              i_soft_refresh = abap_true
            EXCEPTIONS
              finished       = 1
              OTHERS         = 2 ).
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDIF.
    ENDCASE.

  ENDMETHOD.                    "handle_user_command
  METHOD handle_data_finished.

    TYPES: BEGIN OF ty_keydat,
             bukrs  TYPE yfin_lp_t01-bukrs,
             pkind  TYPE yfin_lp_t01-pkind,
             waers  TYPE yfin_lp_t01-waers,
             lqpos  TYPE yfin_lp_t02-lqpos,
             ebene  TYPE yfin_lp_t02-ebene,
             expand TYPE text40,
           END OF ty_keydat.

    DATA: _keydat TYPE ty_keydat,
          _cntdat TYPE ty_keydat,
          _wrbtr  TYPE wrbtr,
          _prsid  TYPE timestamp.

    FIELD-SYMBOLS: <ft_alvdat> TYPE STANDARD TABLE.

    ASSIGN mo_model->mt_alvdat->* TO <ft_alvdat>.
    LOOP AT et_good_cells ASSIGNING FIELD-SYMBOL(<fs_good_cells>).

      READ TABLE <ft_alvdat> ASSIGNING FIELD-SYMBOL(<fs_alvdat>) INDEX <fs_good_cells>-row_id.
      IF sy-subrc IS INITIAL.
        CLEAR: _keydat.
        _keydat = CORRESPONDING #( <fs_alvdat> ).
        LOOP AT <ft_alvdat> ASSIGNING <fs_alvdat>.
          DATA(_index) = sy-tabix.
          _cntdat = CORRESPONDING #( <fs_alvdat> ).
          IF _cntdat-bukrs = _keydat-bukrs AND _cntdat-pkind = _keydat-pkind AND _cntdat-waers = _keydat-waers AND _cntdat-expand IS NOT INITIAL.
            EXIT.
          ENDIF.
        ENDLOOP.
        ASSIGN <ft_alvdat>[ _index ] TO <fs_alvdat>.
        IF sy-subrc IS INITIAL.
          ASSIGN COMPONENT <fs_good_cells>-fieldname OF STRUCTURE <fs_alvdat> TO FIELD-SYMBOL(<value>).
          IF sy-subrc IS INITIAL.
            DATA(l_change) = abap_true.
            CLEAR: _wrbtr.
            LOOP AT <ft_alvdat> ASSIGNING FIELD-SYMBOL(<fs_sumdat>).
              ASSIGN COMPONENT <fs_good_cells>-fieldname OF STRUCTURE <fs_sumdat> TO FIELD-SYMBOL(<amount>).
              IF sy-subrc IS INITIAL.
                _cntdat = CORRESPONDING #( <fs_sumdat> ).
                IF _cntdat-bukrs = _keydat-bukrs AND _cntdat-pkind = _keydat-pkind AND _cntdat-waers = _keydat-waers AND _cntdat-expand IS INITIAL.
                  _wrbtr = _wrbtr + <amount>.
                ENDIF.
              ENDIF.
            ENDLOOP.
            <value> = _wrbtr.

            READ TABLE mo_model->mt_appdat REFERENCE INTO DATA(_appdat) WITH KEY bukrs = _keydat-bukrs pkind = _keydat-pkind waers = _keydat-waers.
            IF sy-subrc IS INITIAL.
              FIELD-SYMBOLS: <ft_linedat> TYPE STANDARD TABLE,
                             <fs_linedat> TYPE any.
              DATA: r_linedat TYPE REF TO data.

              ASSIGN _appdat->line_tab->* TO <ft_linedat>.
              IF <ft_linedat> IS ASSIGNED.
                FREE: <ft_linedat>.
                CREATE DATA r_linedat LIKE LINE OF <ft_linedat>. ASSIGN r_linedat->* TO <fs_linedat>.
                LOOP AT <ft_alvdat> ASSIGNING <fs_alvdat>.
                  _cntdat = CORRESPONDING #( <fs_alvdat> ).
                  IF _cntdat-bukrs = _keydat-bukrs AND _cntdat-pkind = _keydat-pkind AND _cntdat-waers = _keydat-waers AND _cntdat-expand IS INITIAL.
                    APPEND INITIAL LINE TO <ft_linedat> ASSIGNING <fs_linedat>.
                    <fs_linedat> = CORRESPONDING #( <fs_alvdat> ).
                  ENDIF.
                ENDLOOP.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
*--------------------------------------------------------------------*
*-& Insert Log Dat->
*--------------------------------------------------------------------*
        READ TABLE mo_model->mt_appdat REFERENCE INTO _appdat WITH KEY bukrs = _keydat-bukrs pkind = _keydat-pkind waers = _keydat-waers.
        IF sy-subrc IS INITIAL.
          DATA(_ltype) = COND #( WHEN <fs_good_cells>-fieldname(2) EQ 'F_' THEN 'A' WHEN <fs_good_cells>-fieldname(2) EQ 'P_' THEN 'P' ELSE space ).
          IF _ltype <> space.
            CLEAR: _prsid.
            GET TIME STAMP FIELD _prsid.
            DATA(_lqday) = <fs_good_cells>-fieldname+2(8).
            DELETE _appdat->root_dat-_logdat WHERE bukrs = _keydat-bukrs AND pkind = _keydat-pkind AND twaer = _keydat-waers AND lqpos = _keydat-lqpos AND lqday = _lqday AND ltype = _ltype.
            APPEND VALUE #( bukrs = _keydat-bukrs
                            pkind = _keydat-pkind
                            twaer = _keydat-waers
                            lqpos = _keydat-lqpos
                            ebene = _keydat-ebene
                            lqday = _lqday
                            ltype = _ltype
                            prsid = _prsid
                            wrbtr = <fs_good_cells>-value
                            erdat = sy-datum
                            erzet = sy-uzeit
                            ernam = sy-uname ) TO _appdat->root_dat-_logdat.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF l_change <> abap_false.
      mo_view->mo_grid->refresh_table_display(
        EXPORTING
          is_stable      = VALUE #( row = abap_true col = abap_true )
          i_soft_refresh = abap_true
        EXCEPTIONS
          finished       = 1
          OTHERS         = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "handle_data_finished
  METHOD handle_top_of_page.
    event_top_of_page(
      CHANGING
        dg_dyndoc_id = e_dyndoc_id ).
  ENDMETHOD.                    "handle_top_of_page
  METHOD event_top_of_page.

    DATA : dl_text(255) TYPE c,
           v_name_last  TYPE adrp-name_last,
           v_name_first TYPE adrp-name_first.

    CONSTANTS: c_date_from TYPE adrp-date_from VALUE '00010101'.

*-&Free class and create top of page document;
    FREE dg_dyndoc_id.
    CREATE OBJECT dg_dyndoc_id
      EXPORTING
        style = 'ALV_GRID'.

*--------------------------------------------------------------------*
*-&HEADER ->
*--------------------------------------------------------------------*
    CLEAR: dl_text.
    CONCATENATE 'Başlık:' TEXT-h01 INTO dl_text.
    CALL METHOD dg_dyndoc_id->add_text
      EXPORTING
        text         = dl_text
        sap_style    = cl_dd_area=>heading
        sap_fontsize = cl_dd_area=>standard
        sap_color    = cl_dd_area=>list_heading_int.

    CLEAR: dl_text.
    dl_text = TEXT-h02.
    CALL METHOD dg_dyndoc_id->new_line.
    CALL METHOD dg_dyndoc_id->add_text
      EXPORTING
        text         = dl_text
        sap_style    = cl_dd_area=>heading
        sap_fontsize = cl_dd_area=>large
        sap_emphasis = cl_dd_area=>strong.

    CLEAR: dl_text.
    dl_text = 'Kullanıcı Adı :'.
    CALL METHOD dg_dyndoc_id->new_line.
    CALL METHOD dg_dyndoc_id->add_text
      EXPORTING
        text         = dl_text
        sap_emphasis = cl_dd_area=>strong
        sap_color    = cl_dd_area=>list_heading_int.

    CLEAR: dl_text, v_name_last, v_name_first.
    SELECT SINGLE name_last name_first
      FROM usr21 JOIN adrp ON adrp~persnumber = usr21~persnumber AND
                              adrp~date_from  = c_date_from AND
                              adrp~nation     = space
        INTO (v_name_last, v_name_first)
        WHERE usr21~bname = sy-uname.
    CONCATENATE v_name_first v_name_last INTO dl_text SEPARATED BY space.

    CALL METHOD dg_dyndoc_id->add_text
      EXPORTING
        text         = dl_text
        sap_emphasis = cl_dd_area=>heading
        sap_color    = cl_dd_area=>list_negative_inv.

    CLEAR: dl_text.
    dl_text = 'Tarih :'.
    CALL METHOD dg_dyndoc_id->new_line.
    CALL METHOD dg_dyndoc_id->add_text
      EXPORTING
        text         = dl_text
        sap_emphasis = cl_dd_area=>strong
        sap_color    = cl_dd_area=>list_heading_int.

    CLEAR dl_text.
    dl_text = get_long_date( im_dat = sy-datum ).
    CALL METHOD dg_dyndoc_id->add_text
      EXPORTING
        text         = dl_text
        sap_emphasis = cl_dd_area=>heading
        sap_color    = cl_dd_area=>list_negative_inv.

    display_top_of_page(
      EXPORTING
        dg_dyndoc_id = dg_dyndoc_id ).

  ENDMETHOD.              "event_top_of_page
  METHOD display_top_of_page.

    IF mo_view->mo_html_cntrl IS INITIAL.
      CREATE OBJECT mo_view->mo_html_cntrl
        EXPORTING
          parent = mo_view->mo_parent_top.
    ENDIF.
    CALL METHOD dg_dyndoc_id->merge_document.
    dg_dyndoc_id->html_control = mo_view->mo_html_cntrl.

    CALL METHOD dg_dyndoc_id->display_document
      EXPORTING
        reuse_control      = abap_true
        parent             = mo_view->mo_parent_top
      EXCEPTIONS
        html_display_error = 1.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.              "display_top_of_page

ENDCLASS.                    "lcl_mvc_controller IMPLEMENTATION
