CLASS yfin_lp_bdc_toolkit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS co_msgtyp_info TYPE bapi_mtype VALUE 'I' ##NO_TEXT.
    CONSTANTS co_msgtyp_success TYPE bapi_mtype VALUE 'S' ##NO_TEXT.
    CONSTANTS co_msgtyp_error TYPE bapi_mtype VALUE 'E' ##NO_TEXT.
    CONSTANTS co_msgtyp_warning TYPE bapi_mtype VALUE 'W' ##NO_TEXT.
    CONSTANTS co_msgtyp_abend TYPE bapi_mtype VALUE 'A' ##NO_TEXT.

    METHODS constructor
      IMPORTING
        !im_tcode    TYPE tcode
        !im_bdc_mode TYPE ctu_mode
        !im_upd_mode TYPE ctu_update
        !im_racommit TYPE xfeld OPTIONAL .
    CLASS-METHODS bdc_instance
      IMPORTING
        !im_tcode     TYPE tcode
        !im_bdc_mode  TYPE ctu_mode DEFAULT 'N'
        !im_upd_mode  TYPE ctu_update DEFAULT 'S'
        !im_racommit  TYPE xfeld OPTIONAL
      RETURNING
        VALUE(re_bdc) TYPE REF TO yfin_lp_bdc_toolkit .
    CLASS-METHODS bdc_instance_free .
    METHODS bdc_add_field
      IMPORTING
        !im_fld  TYPE any
        !im_val  TYPE any
        !im_conv TYPE char1 OPTIONAL .
    METHODS bdc_add_screen
      IMPORTING
        !im_repid TYPE any
        !im_dynnr TYPE any .
    METHODS bdc_process
      EXPORTING
        !ev_subrc    TYPE sy-subrc
        !ev_messages TYPE bapiret2_tab .
    METHODS bdc_data_clear .
    METHODS call_transaction
      EXPORTING
        !ev_subrc    TYPE sy-subrc
        !ev_messages TYPE bapiret2_tab .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA go_bdc TYPE REF TO yfin_lp_bdc_toolkit .
    DATA bdcdata TYPE bdcdata_tab .
    DATA tcode TYPE tcode .
    DATA ct_bdc_mode TYPE ctu_mode .
    DATA ct_upd_mode TYPE ctu_update .
    DATA ct_racommit TYPE xfeld .
ENDCLASS.



CLASS YFIN_LP_BDC_TOOLKIT IMPLEMENTATION.


  METHOD bdc_add_field.

*----------------------------------------------------------------------*
*-& Insert field in BDC screen
*-& IV_CONV ->
*-& D - Convert date from YYYYMMDD format to output format
*-& C - Convert currency from XX.XX format to output format
*-& S - Allow initial values to be populated in BDC
*----------------------------------------------------------------------*
    DATA: bdcdata LIKE LINE OF me->bdcdata.

    IF im_val IS INITIAL AND im_conv <> 'S'.
      RETURN.
    ENDIF.

    CASE im_conv.
      WHEN 'D'. "*-&Date format;
        CALL FUNCTION 'CONVERSION_EXIT_MODAT_OUTPUT'
          EXPORTING
            input  = im_val
          IMPORTING
            output = bdcdata-fval.

      WHEN 'C'.
        bdcdata-fval = im_val.
        TRANSLATE bdcdata-fval USING '.,'.
        CONDENSE bdcdata-fval.

      WHEN OTHERS.
        bdcdata-fval = im_val.
        CONDENSE bdcdata-fval.

    ENDCASE.

    bdcdata-fnam = im_fld.
    APPEND bdcdata TO me->bdcdata.

  ENDMETHOD.


  METHOD bdc_add_screen.
*----------------------------------------------------------------------*
*-& Start new BDC screen
*----------------------------------------------------------------------*
    DATA: bdcdata LIKE LINE OF me->bdcdata.

    bdcdata-program  = im_repid.
    bdcdata-dynpro   = im_dynnr.
    bdcdata-dynbegin = 'X'.
    APPEND bdcdata TO me->bdcdata.

  ENDMETHOD.


  METHOD bdc_data_clear.
*----------------------------------------------------------------------*
*-& Clear BDC Data;
*----------------------------------------------------------------------*

    CLEAR: me->bdcdata.
  ENDMETHOD.


  METHOD bdc_instance.

    yfin_lp_bdc_toolkit=>bdc_instance_free( ).
    IF yfin_lp_bdc_toolkit=>go_bdc IS NOT BOUND.
      ASSERT FIELDS im_tcode CONDITION im_tcode IS NOT INITIAL.
      CREATE OBJECT yfin_lp_bdc_toolkit=>go_bdc
        EXPORTING
          im_tcode    = im_tcode
          im_bdc_mode = im_bdc_mode
          im_upd_mode = im_upd_mode
          im_racommit = im_racommit.
    ENDIF.
    re_bdc = yfin_lp_bdc_toolkit=>go_bdc.

  ENDMETHOD.


  METHOD bdc_instance_free.

    FREE yfin_lp_bdc_toolkit=>go_bdc.

  ENDMETHOD.


  METHOD bdc_process.
*-----------------------------------------------------------------------
* Process BDC
*-----------------------------------------------------------------------
    CLEAR: ev_subrc,
           ev_messages.
    me->call_transaction( IMPORTING ev_subrc    = ev_subrc
                                    ev_messages = ev_messages ).

  ENDMETHOD.


  METHOD call_transaction.
*-----------------------------------------------------------------------
*-& Run BDC
*-----------------------------------------------------------------------
    DATA: bdc_subrc  LIKE sy-subrc,
          ls_options TYPE ctu_params,
          t_messages TYPE tab_bdcmsgcoll.

    CLEAR: ev_subrc,
           ev_messages,
           t_messages,
           ls_options.

*-& Call transaction ->
    ls_options-racommit = me->ct_racommit.
    ls_options-dismode  = me->ct_bdc_mode.
    ls_options-updmode  = me->ct_upd_mode.
    CALL TRANSACTION me->tcode
                     USING  me->bdcdata
                     OPTIONS FROM ls_options
                     MESSAGES INTO t_messages.
    bdc_subrc = sy-subrc.
*-&Check errors ->
    LOOP AT t_messages TRANSPORTING NO FIELDS WHERE msgtyp = co_msgtyp_error OR msgtyp = co_msgtyp_abend.
      EXIT.
    ENDLOOP.
    IF sy-subrc IS INITIAL OR bdc_subrc <> 0.
      ev_subrc = 4.
    ENDIF.

*-&Converting message->
    CALL FUNCTION 'CONVERT_BDCMSGCOLL_TO_BAPIRET2'
      TABLES
        imt_bdcmsgcoll = t_messages
        ext_return     = ev_messages.

    me->bdc_data_clear( ).

  ENDMETHOD.


  METHOD constructor.

    me->tcode       = im_tcode.
    me->ct_bdc_mode = im_bdc_mode.
    me->ct_upd_mode = im_upd_mode.
    me->ct_racommit = im_racommit.

  ENDMETHOD.
ENDCLASS.
