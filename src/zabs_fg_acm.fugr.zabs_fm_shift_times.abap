FUNCTION zabs_fm_shift_times.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_SDATE) TYPE  D OPTIONAL
*"     REFERENCE(IV_EDATE) TYPE  D OPTIONAL
*"     REFERENCE(IV_STIME) TYPE  T OPTIONAL
*"     REFERENCE(IV_ETIME) TYPE  T OPTIONAL
*"  EXPORTING
*"     REFERENCE(EV_STIME) TYPE  TZONREF-TSTAMPS
*"     REFERENCE(EV_ETIME) TYPE  TZONREF-TSTAMPS
*"     REFERENCE(EV_STDUR) TYPE  ZABS_DEL_STDUR
*"----------------------------------------------------------------------

*-- Local Declarations
  DATA: lv_available(10) TYPE p DECIMALS 4,
        lv_sumparada(10) TYPE p DECIMALS 4.
  DATA: lv_achit_int_h  TYPE int4,
        lv_mint_aux(10) TYPE p DECIMALS 4,
        lv_achft_int_h  TYPE int4,
        lv_achft_int_m  TYPE int4,
        lv_achit_int_m  TYPE int4.

  CALL FUNCTION 'IB_CONVERT_INTO_TIMESTAMP'
    EXPORTING
      i_datlo     = iv_sdate
      i_timlo     = iv_stime
    IMPORTING
      e_timestamp = ev_stime.

  CALL FUNCTION 'IB_CONVERT_INTO_TIMESTAMP'
    EXPORTING
      i_datlo     = iv_edate
      i_timlo     = iv_etime
    IMPORTING
      e_timestamp = ev_etime.

*-- Calculate the total duration between shift start and end time
  IF ev_stdur IS REQUESTED.
*--- Conver hour
    lv_achit_int_h = iv_stime(2). " *Initially  hour
    lv_achft_int_h = iv_etime(2). " *Finally    hour
    lv_achit_int_m = iv_stime+2(2). " *Finally  minute
    lv_achft_int_m = iv_etime+2(2). " *Finally  minute

*-- Available total calculate
    IF lv_achft_int_h < lv_achit_int_h.
      lv_achft_int_h = lv_achft_int_h + 24.
    ENDIF.

    lv_mint_aux  = ( abs( lv_achit_int_m - lv_achft_int_m ) / 60 ) .
    lv_available = abs( ( lv_achit_int_h - lv_achft_int_h ) ).
    lv_available = lv_available + lv_mint_aux.
    ev_stdur = lv_available.
  ENDIF.

ENDFUNCTION.
