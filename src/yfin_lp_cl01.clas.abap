CLASS yfin_lp_cl01 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_valdat,
        value TYPE char30,
        range TYPE RANGE OF datum,
      END OF ty_valdat .
    TYPES:
      tt_valdat TYPE STANDARD TABLE OF ty_valdat WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_alvdat,
        fieldcat  TYPE lvc_t_fcat,
        alvoutput TYPE REF TO data,
        keydate   TYPE tt_valdat,
      END OF ty_alvdat .

    METHODS dynamic_alvdat
      IMPORTING
        VALUE(iv_tmunit) TYPE char1 DEFAULT 'D'
        VALUE(iv_begdat) TYPE datum
        VALUE(iv_enddat) TYPE datum
        !iv_celdat       TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rv_alvdat) TYPE ty_alvdat
      EXCEPTIONS
        contains_error .
    CLASS-METHODS _timeunit_valdat
      IMPORTING
        !iv_tmunit       TYPE char1
        !iv_begdat       TYPE datum
        !iv_enddat       TYPE datum
      RETURNING
        VALUE(rt_valdat) TYPE tt_valdat
      EXCEPTIONS
        invalid_timeunit
        invalid_date .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS YFIN_LP_CL01 IMPLEMENTATION.


  METHOD dynamic_alvdat.

    DATA: _colpos TYPE lvc_colpos,
          _coltxt TYPE lvc_txtcol.


    DATA : lt_celltab   TYPE lvc_t_styl,
           lt_cellcolor TYPE lvc_t_scol.
    DATA : lo_table TYPE REF TO cl_abap_tabledescr,
           lo_line  TYPE REF TO cl_abap_structdescr.

    DATA : gdo_handle    TYPE REF TO data,
           go_sdescr     TYPE REF TO cl_abap_structdescr,
           go_sdescr_new TYPE REF TO cl_abap_structdescr,
           go_tdescr     TYPE REF TO cl_abap_tabledescr,
           gs_comp       TYPE abap_componentdescr,
           gt_components TYPE abap_component_tab.

    _timeunit_valdat(
      EXPORTING
        iv_tmunit = iv_tmunit
        iv_begdat = iv_begdat
        iv_enddat = iv_enddat
      RECEIVING
        rt_valdat        = rv_alvdat-keydate
      EXCEPTIONS
        invalid_timeunit = 1
        invalid_date     = 2
        OTHERS           = 3  ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
    ENDIF.

    CLEAR: _colpos, _coltxt.
    ADD 1 TO _colpos.
    _coltxt = |ŞK|.
    APPEND VALUE #( col_pos = _colpos fieldname = |BUKRS| datatype = 'CHAR' intlen = '4' domname = 'BUKRS' coltext = _coltxt scrtext_l = _coltxt scrtext_m = _coltxt scrtext_s = _coltxt ) TO rv_alvdat-fieldcat.

    CLEAR: _coltxt.
    ADD 1 TO _colpos.
    _coltxt = |İşlem türü|.
    APPEND VALUE #( col_pos = _colpos fieldname = |PKIND| datatype = 'CHAR' intlen = '10' domname = 'YFIN_LP_D001' coltext = _coltxt scrtext_l = _coltxt scrtext_m = _coltxt scrtext_s = _coltxt ) TO rv_alvdat-fieldcat.

    CLEAR: _coltxt.
    ADD 1 TO _colpos.
    _coltxt = |PB|.
    APPEND VALUE #( col_pos = _colpos fieldname = |WAERS| datatype = 'CUKY' intlen = '5' domname = 'WAERS' coltext = _coltxt scrtext_l = _coltxt scrtext_m = _coltxt scrtext_s = _coltxt ) TO rv_alvdat-fieldcat.

    CLEAR: _coltxt.
    ADD 1 TO _colpos.
    _coltxt = |Exp/Col Row|.
    APPEND VALUE #( col_pos = _colpos fieldname = |EXPAND| datatype = 'CHAR' intlen = '40' domname = 'TEXT40' coltext = _coltxt scrtext_l = _coltxt scrtext_m = _coltxt scrtext_s = _coltxt ) TO rv_alvdat-fieldcat.

    CLEAR: _coltxt.
    ADD 1 TO _colpos.
    _coltxt = |Başlık|.
    APPEND VALUE #( col_pos = _colpos fieldname = |HEADER| datatype = 'CHAR' intlen = '40' domname = 'TEXT40' coltext = _coltxt scrtext_l = _coltxt scrtext_m = _coltxt scrtext_s = _coltxt ) TO rv_alvdat-fieldcat.

    LOOP AT  rv_alvdat-keydate REFERENCE INTO DATA(_keydate).
      CLEAR: _coltxt.
      ADD 1 TO _colpos.
      _coltxt = |Fiili { COND #( WHEN iv_tmunit = 'D' THEN |{ _keydate->value+6(2) }.{ _keydate->value+4(2) }.{  _keydate->value+0(4) }|
                                 WHEN iv_tmunit = 'W' THEN |{ _keydate->value+4(3) }.{ _keydate->value+0(4) }|
                                 WHEN iv_tmunit = 'M' THEN |{ _keydate->value+4(2) }.{ _keydate->value+0(4) }|
                                 ELSE _keydate->value ) }|.
      APPEND VALUE #( col_pos = _colpos fieldname = |F_{ _keydate->value }| datatype = 'CURR' intlen = '13' domname = 'WRBTR' coltext = _coltxt scrtext_l = _coltxt scrtext_m = _coltxt scrtext_s = _coltxt ) TO rv_alvdat-fieldcat.

      CLEAR: _coltxt.
      ADD 1 TO _colpos.
      _coltxt = |Plan { COND #( WHEN iv_tmunit = 'D' THEN |{ _keydate->value+6(2) }.{ _keydate->value+4(2) }.{  _keydate->value+0(4) }|
                                WHEN iv_tmunit = 'W' THEN |{ _keydate->value+4(3) }.{ _keydate->value+0(4) }|
                                WHEN iv_tmunit = 'M' THEN |{ _keydate->value+4(2) }.{ _keydate->value+0(4) }|
                                ELSE _keydate->value ) }|.
      APPEND VALUE #( col_pos = _colpos fieldname = |P_{ _keydate->value }| datatype = 'CURR' intlen = '13' domname = 'WRBTR' coltext = _coltxt scrtext_l = _coltxt scrtext_m = _coltxt scrtext_s = _coltxt ) TO rv_alvdat-fieldcat.
    ENDLOOP.

    CLEAR: _coltxt.
    ADD 1 TO _colpos.
    _coltxt = |Likidite kalemi|.
    APPEND VALUE #( col_pos = _colpos fieldname = |LQPOS| datatype = 'CHAR' intlen = '16' domname = 'FLQPOS' coltext = _coltxt scrtext_l = _coltxt scrtext_m = _coltxt scrtext_s = _coltxt ) TO rv_alvdat-fieldcat.

    CLEAR: _coltxt.
    ADD 1 TO _colpos.
    _coltxt = |Planlama düzeyi|.
    APPEND VALUE #( col_pos = _colpos fieldname = |EBENE| datatype = 'CHAR' intlen = '2' domname = 'FDLEV' coltext = _coltxt scrtext_l = _coltxt scrtext_m = _coltxt scrtext_s = _coltxt ) TO rv_alvdat-fieldcat.

    FIELD-SYMBOLS: <table> TYPE STANDARD TABLE,
                   <wa>    TYPE any.

    DATA: gt_table TYPE REF TO data.

    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog           = rv_alvdat-fieldcat
      IMPORTING
        ep_table                  = gt_table
      EXCEPTIONS
        generate_subpool_dir_full = 1
        OTHERS                    = 2.

    IF iv_celdat EQ abap_true.
      rv_alvdat-alvoutput ?= gt_table.
    ELSE.
      UNASSIGN: <table>.
      ASSIGN gt_table->* TO <table>.

      lo_table ?= cl_abap_typedescr=>describe_by_data( <table> ).
      lo_line  ?= lo_table->get_table_line_type( ).
      gt_components[]  = lo_line->get_components( ).

      CLEAR gs_comp.
      gs_comp-type ?= cl_abap_typedescr=>describe_by_data( lt_celltab ).
      gs_comp-name = 'CELLTAB'.
      APPEND gs_comp TO gt_components.

      CLEAR gs_comp.
      gs_comp-type ?= cl_abap_typedescr=>describe_by_data( lt_cellcolor ).
      gs_comp-name = 'CELLCOLOR'.
      APPEND gs_comp TO gt_components.

      go_sdescr  = cl_abap_structdescr=>create(
                      p_components = gt_components[]
                      p_strict     = cl_abap_structdescr=>false ).
      go_tdescr  = cl_abap_tabledescr=>create( go_sdescr ).
      CREATE DATA rv_alvdat-alvoutput TYPE HANDLE go_tdescr.

    ENDIF.
  ENDMETHOD.


  METHOD _timeunit_valdat.

    TYPES: BEGIN OF ty_basedat,
             datum TYPE datum,
           END OF ty_basedat,
           tt_basedat TYPE STANDARD TABLE OF ty_basedat WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_yeardat,
             gjahr TYPE gjahr,
           END OF ty_yeardat,
           tt_yeardat TYPE STANDARD TABLE OF ty_yeardat WITH DEFAULT KEY.

    DATA: _basedat_tab  TYPE tt_basedat,
          _basedat      TYPE ty_basedat,
          _weekdat_tab  TYPE tt_basedat,
          _weekdat      TYPE ty_basedat,
          _monthdat_tab TYPE tt_basedat,
          _monthdat     TYPE ty_basedat,
          _week         TYPE scal-week,
          _monday       TYPE sy-datum,
          _sunday       TYPE sy-datum,
          _month_bdate  TYPE datum,
          _month_edate  TYPE datum,
          _year_bdate   TYPE datum,
          _year_edate   TYPE datum,
          _yeardat_tab  TYPE tt_yeardat,
          _yeardat      TYPE ty_yeardat.

    IF iv_begdat GT iv_enddat.
      MESSAGE e002(yfin_lp) RAISING invalid_date.
    ENDIF.

    CLEAR: _basedat.
    _basedat-datum = iv_begdat.
    APPEND _basedat TO _basedat_tab.
    DO.
      IF _basedat-datum  = iv_enddat.
        EXIT.
      ENDIF.
      _basedat-datum =  _basedat-datum + 1.
      APPEND _basedat TO _basedat_tab.
    ENDDO.

    CASE iv_tmunit.
*--------------------------------------------------------------------*
*-&Daily Dat->
*--------------------------------------------------------------------*
      WHEN 'D'.
        LOOP AT _basedat_tab INTO _basedat.
          APPEND INITIAL LINE TO rt_valdat REFERENCE INTO DATA(_valdat).
          _valdat->value = _basedat-datum.
          _valdat->range = VALUE #( ( sign = 'I' option = 'EQ' low = _basedat-datum ) ).
        ENDLOOP.

*--------------------------------------------------------------------*
*-&Weekly Dat->
*--------------------------------------------------------------------*
      WHEN 'W'.
        LOOP AT _basedat_tab INTO _basedat.
          CLEAR: _week, _monday, _sunday.
          CALL FUNCTION 'GET_WEEK_INFO_BASED_ON_DATE'
            EXPORTING
              date   = _basedat-datum
            IMPORTING
              week   = _week
              monday = _monday
              sunday = _sunday.

          FREE: _weekdat_tab, _weekdat.
          IF NOT line_exists( rt_valdat[ value = _week ] ).
            APPEND INITIAL LINE TO rt_valdat REFERENCE INTO _valdat.
            _valdat->value = _week.

            _weekdat-datum = _monday.
            APPEND _weekdat TO _weekdat_tab.
            DO.
              IF _weekdat-datum  = _sunday.
                EXIT.
              ENDIF.
              _weekdat-datum =  _weekdat-datum + 1.
              APPEND _weekdat TO _weekdat_tab.
            ENDDO.
            LOOP AT _weekdat_tab INTO _weekdat WHERE datum GE iv_begdat AND datum LE iv_enddat.
              _valdat->range = VALUE #( BASE _valdat->range ( sign = 'I' option = 'EQ' low = _weekdat-datum ) ).
            ENDLOOP.
          ENDIF.
        ENDLOOP.

*--------------------------------------------------------------------*
*-&Monthly Dat->
*--------------------------------------------------------------------*
      WHEN 'M'. "*-&Monthly
        LOOP AT _basedat_tab INTO _basedat.
          FREE: _monthdat_tab, _monthdat.
          IF NOT line_exists( rt_valdat[ value = _basedat-datum(6) ] ).
            CLEAR: _month_bdate, _month_edate.
            CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
              EXPORTING
                iv_date             = _basedat-datum
              IMPORTING
                ev_month_begin_date = _month_bdate
                ev_month_end_date   = _month_edate.

            APPEND INITIAL LINE TO rt_valdat REFERENCE INTO _valdat.
            _valdat->value = _basedat-datum(6).

            _monthdat-datum = _month_bdate.
            APPEND _monthdat TO _monthdat_tab.
            DO.
              IF _monthdat-datum  = _month_edate.
                EXIT.
              ENDIF.
              _monthdat-datum =  _monthdat-datum + 1.
              APPEND _monthdat TO _monthdat_tab.
            ENDDO.
            LOOP AT _monthdat_tab INTO _monthdat WHERE datum GE iv_begdat AND datum LE iv_enddat.
              _valdat->range = VALUE #( BASE _valdat->range ( sign = 'I' option = 'EQ' low = _monthdat-datum ) ).
            ENDLOOP.
          ENDIF.
        ENDLOOP.

*--------------------------------------------------------------------*
*-&Yearly Dat->
*--------------------------------------------------------------------*
      WHEN 'Y'. "*-&Yearly
        FREE: _yeardat_tab, _yeardat.
        _yeardat-gjahr = iv_begdat(4).
        APPEND _yeardat TO _yeardat_tab.
        DO.
          IF _yeardat-gjahr  = iv_enddat(4).
            EXIT.
          ENDIF.
          _yeardat-gjahr =  _yeardat-gjahr + 1.
          APPEND _yeardat TO _yeardat_tab.
        ENDDO.

        LOOP AT _yeardat_tab INTO _yeardat.
          APPEND INITIAL LINE TO rt_valdat REFERENCE INTO _valdat.
          _valdat->value = _yeardat.

          CLEAR: _year_bdate, _year_edate.
          _year_bdate = |{ _yeardat-gjahr }0101|.
          _year_edate = |{ _yeardat-gjahr }1231|.
          LOOP AT _basedat_tab INTO _basedat WHERE datum GE _year_bdate AND datum LE _year_edate.
            _valdat->range = VALUE #( BASE _valdat->range ( sign = 'I' option = 'EQ' low = _basedat-datum ) ).
          ENDLOOP.
        ENDLOOP.

      WHEN OTHERS.
        MESSAGE e001(yfin_lp) RAISING invalid_timeunit.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
