*----------------------------------------------------------------------*
*   INCLUDE M0C10F01                                                   *
*----------------------------------------------------------------------*
*---------------------------------------------------------------------*
*       FORM 0C10_SET_TABLE_ACCESS                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM turn_calculate.

  DATA:
    tot_wa(1000)         TYPE c,
    tot_wa_paradas(1000) TYPE c.
  DATA:
  eflag        TYPE vcl_flag_type.

  DATA BEGIN OF ls_wa.
  INCLUDE STRUCTURE zvfmacwork_shift.
  DATA action(1) TYPE c.
  DATA END OF ls_wa.

  DATA BEGIN OF ls_wa_paradas.
  INCLUDE STRUCTURE zvfmacsche_break.
  DATA action(1) TYPE c.
  DATA END OF ls_wa_paradas.

  DATA: lv_available(10) TYPE p DECIMALS 4,
        lv_sumparada(10) TYPE p DECIMALS 4.
  DATA: lv_achit_int_h  TYPE int4,
        lv_mint_aux(10) TYPE p DECIMALS 4,
        lv_achft_int_h  TYPE int4,
        lv_achft_int_m  TYPE int4,
        lv_achit_int_m  TYPE int4.

  FIELD-SYMBOLS:
    <tot_wa_struc>    LIKE ls_wa,
    <tot_wa_paradas>  LIKE ls_wa_paradas,
    <tot_wa_area>     LIKE ls_wa,
    <lv_actut>        TYPE any,
    <lv_acocp>        TYPE any,
    <lv_accpd>        TYPE any,
    <lv_action>       TYPE any,
    <lv_acnci>        TYPE any,
    <lv_achit>        TYPE any,
    <lv_achft>        TYPE any,
    <lv_acppg_parada> TYPE any,
    <lv_acprt>        TYPE any,
    <lv_acppg>        TYPE any,
    <lv_mandt>        TYPE any,
    <lv_actrn>        TYPE any,
    <lv_acdtr>        TYPE any,
*    <lv_acvtr>        TYPE any,
*    <lv_acvat>        TYPE any,
    <lv_period>       TYPE any,
    <lv_acmei>        TYPE any,
    <lv_werks>        TYPE any,
    <lv_arbpl>        TYPE any,
    <lv_fabkl>        TYPE any,
    <choose_area>     LIKE ls_wa,
    <tot_wax>         TYPE x.

  DATA: lt_zfmactrns   TYPE zt_fmacwork_shift,
        lt_zfmactrnsbd TYPE TABLE OF zfmacwork_shift,
        ls_zfmactrns   TYPE zfmacwork_shift.

  DATA: lv_acdtr TYPE zfmacdtr.

  DATA:
    lt_e071k         TYPE TABLE OF e071k,
    ltdba_vimsellist TYPE TABLE OF vimsellist,
    ltdpl_vimsellist TYPE TABLE OF vimsellist,
    lt_vimexclfun    TYPE TABLE OF vimexclfun,
    lt_hader         TYPE TABLE OF vimdesc,
    lt_namtab        TYPE TABLE OF vimnamtab.

  CHECK sy-msgid EQ 'SV'
    AND sy-msgty  NE 'I'
    AND sy-msgno NE '011'
  AND sy-ucomm = 'SAVE'.

  PERFORM vcl_set_table_access_for_obj USING 'ZVFMACSCHE_BREAK'
                                      CHANGING eflag.

  ASSIGN <vcl_extract>   TO <vcl_extract_v_paradas>.

  PERFORM vcl_set_table_access_for_obj USING 'ZVFMACWORK_SHIFT'
                                       CHANGING eflag.

  ASSIGN <vcl_extract> TO <vcl_extract_v_tc37a>.
  ASSIGN <vcl_total> TO <vcl_total_v_tc37a>.

*--- Conversion
  ASSIGN: tot_wa         TO <tot_wa_struc> CASTING,
          tot_wa         TO <tot_wax> CASTING,
          tot_wa_paradas TO <tot_wa_paradas> CASTING.

  LOOP AT <vcl_extract_v_tc37a> INTO tot_wa.

    ASSIGN COMPONENT 'ACTUT' OF STRUCTURE <tot_wa_struc> TO <lv_actut>.
    ASSIGN COMPONENT 'ACNCI' OF STRUCTURE <tot_wa_struc> TO <lv_acnci>.
    ASSIGN COMPONENT 'ACCPD' OF STRUCTURE <tot_wa_struc> TO <lv_accpd>.
    ls_zfmactrns-accpd = <lv_accpd>.
    IF <lv_actut> IS ASSIGNED
      AND <lv_acnci> IS ASSIGNED
      AND <lv_accpd> IS ASSIGNED.
*-- available capacity calculate
      <lv_accpd> = abs( <lv_actut> - <lv_acnci> ).
      ls_zfmactrns-accpd =  <lv_accpd>.
    ENDIF.

    ASSIGN COMPONENT 'ACHIT' OF STRUCTURE <tot_wa_struc> TO <lv_achit>.
    ls_zfmactrns-achit = <lv_achit>.
    ASSIGN COMPONENT 'ACHFT' OF STRUCTURE <tot_wa_struc> TO <lv_achft>.

    ls_zfmactrns-achft = <lv_achft>.
    IF <lv_achit> IS ASSIGNED
      AND <lv_achft> IS ASSIGNED.
*--- Conver hour
      lv_achit_int_h = <lv_achit>(2). " *Initially  hour
      lv_achft_int_h = <lv_achft>(2). " *Finally    hour
      lv_achit_int_m = <lv_achit>+2(2). " *Finally  minute
      lv_achft_int_m = <lv_achft>+2(2). " *Finally  minute

*-- available total calculate
      IF  lv_achft_int_h < lv_achit_int_h.
        lv_achft_int_h = lv_achft_int_h + 24.
      ENDIF.
      lv_mint_aux    = ( abs( lv_achit_int_m - lv_achft_int_m ) / 60 ) .
      lv_available   = abs( ( lv_achit_int_h - lv_achft_int_h ) ).
      lv_available   = lv_available + lv_mint_aux.

      CLEAR:  lv_achit_int_h,
              lv_achft_int_h,
              lv_achit_int_m,
              lv_achft_int_m.

      ASSIGN COMPONENT 'ACPPG' OF STRUCTURE <tot_wa_struc> TO <lv_acppg>.
      ASSIGN COMPONENT 'PERIOD' OF STRUCTURE <tot_wa_struc> TO FIELD-SYMBOL(<lv_periodx>).
      IF <lv_acppg> IS ASSIGNED
      AND <lv_periodx> IS ASSIGNED.
        ls_zfmactrns-acppg = <lv_acppg>.
        LOOP AT <vcl_extract_v_paradas> INTO tot_wa_paradas.
          ASSIGN COMPONENT 'ACPPG' OF STRUCTURE <tot_wa_paradas> TO <lv_acppg_parada>.
          ASSIGN COMPONENT 'PERIOD' OF STRUCTURE <tot_wa_paradas> TO FIELD-SYMBOL(<lv_periodx_parada>).
          ASSIGN COMPONENT 'ACPRT' OF STRUCTURE <tot_wa_paradas> TO <lv_acprt>.
          IF <lv_acppg_parada> IS NOT ASSIGNED
          OR <lv_periodx_parada> IS NOT ASSIGNED
          OR <lv_acprt> IS NOT ASSIGNED.
            EXIT.
          ENDIF.
          IF <lv_acppg> = <lv_acppg_parada>
          AND <lv_periodx> = <lv_periodx_parada>.
            IF <lv_acprt> IS ASSIGNED.
              lv_sumparada = lv_sumparada + <lv_acprt>.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
      ASSIGN COMPONENT 'ACTUT' OF STRUCTURE <tot_wa_struc> TO <lv_actut>.
** Used time calculate
      <lv_actut> = abs( lv_available - (  lv_available * ( lv_sumparada / 100 ) ) ) .
      ls_zfmactrns-actut = <lv_actut>.
      CLEAR: lv_sumparada.

      ASSIGN COMPONENT 'ACOCP' OF STRUCTURE <tot_wa_struc> TO <lv_acocp>.
      ls_zfmactrns-acocp = <lv_acocp>.
      IF  <lv_acocp> IS ASSIGNED AND
        <lv_actut> IS ASSIGNED.
*** Ocupation calculate
        IF lv_available IS NOT INITIAL.
          <lv_acocp> = abs( ( <lv_actut> / lv_available ) * 100 ).
        ENDIF.
        ls_zfmactrns-acocp =  <lv_acocp>.
      ENDIF.

      ASSIGN COMPONENT 'ACOCP' OF STRUCTURE <tot_wa_struc> TO <lv_acocp>.
      ASSIGN COMPONENT 'ACNCI' OF STRUCTURE <tot_wa_struc> TO <lv_acnci>.
      ls_zfmactrns-acnci = <lv_acnci>.
      IF  <lv_acnci> IS ASSIGNED AND
        <lv_actut> IS ASSIGNED AND
        <lv_accpd> IS ASSIGNED.
***  available capacity
        <lv_accpd> = ( <lv_actut> * <lv_acnci> ).
        ls_zfmactrns-accpd = <lv_accpd>.
      ENDIF.
      CLEAR: lv_available.
    ENDIF.

*--- Fill table for update
    ASSIGN COMPONENT 'ACTRN' OF STRUCTURE <tot_wa_struc> TO <lv_actrn>.
    IF <lv_actrn> IS ASSIGNED.
      ls_zfmactrns-actrn = <lv_actrn>.
    ENDIF.
    ASSIGN COMPONENT 'ACDTR' OF STRUCTURE <tot_wa_struc> TO <lv_acdtr>.
    IF <lv_acdtr> IS ASSIGNED.
      ls_zfmactrns-acdtr = <lv_acdtr>.
    ENDIF.
*    ASSIGN COMPONENT 'ACVTR' OF STRUCTURE <tot_wa_struc> TO <lv_acvtr>.
*    ls_zfmactrns-acvtr = <lv_acvtr>.
*    ASSIGN COMPONENT 'ACVAT' OF STRUCTURE <tot_wa_struc> TO <lv_acvat>.
*    ls_zfmactrns-acvat = <lv_acvat>.
    ASSIGN COMPONENT 'PERIOD' OF STRUCTURE <tot_wa_struc> TO <lv_period>.
    IF <lv_period> IS ASSIGNED.
      ls_zfmactrns-period = <lv_period>.
    ENDIF.

*    IF <lv_acvtr> GT <lv_acvat>.
*      MESSAGE ID 'DB' TYPE 'S' NUMBER '650' DISPLAY LIKE 'E'.
*      SET CURSOR FIELD 'ZVFMACWORK_SHIFT-ACVAT'.
*      EXIT.
*    ENDIF.

    ASSIGN COMPONENT 'ACMEI' OF STRUCTURE <tot_wa_struc> TO <lv_acmei>.
    IF <lv_acmei> IS ASSIGNED.
      ls_zfmactrns-acmei = <lv_acmei> = 'H'.
    ENDIF.
    ASSIGN COMPONENT 'WERKS' OF STRUCTURE <tot_wa_struc> TO <lv_werks>.
    IF <lv_werks> IS ASSIGNED.
      ls_zfmactrns-werks = <lv_werks>.
    ENDIF.
    ASSIGN COMPONENT 'ARBPL' OF STRUCTURE <tot_wa_struc> TO <lv_arbpl>.
    IF <lv_arbpl> IS ASSIGNED.
      ls_zfmactrns-arbpl = <lv_arbpl>.
    ENDIF.
    ASSIGN COMPONENT 'FABKL' OF STRUCTURE <tot_wa_struc> TO <lv_fabkl>.
    IF <lv_fabkl> IS ASSIGNED.
      ls_zfmactrns-fabkl = <lv_fabkl>.
    ENDIF.
    ASSIGN COMPONENT 'MANDT' OF STRUCTURE <tot_wa_struc> TO <lv_mandt>.
    IF <lv_mandt> IS ASSIGNED.
      ls_zfmactrns-mandt = <lv_mandt>.
    ENDIF.
*--- Update for show in field-symbol
    MODIFY <vcl_extract_v_tc37a>  FROM tot_wa INDEX sy-tabix.
    IF sy-subrc EQ 0.
      APPEND ls_zfmactrns TO lt_zfmactrns.
    ENDIF.
  ENDLOOP.
  PERFORM view_update USING lt_zfmactrns.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form VIEW_UPDATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_ZFMACTRNS
*&---------------------------------------------------------------------*
FORM view_update  CHANGING lt_zfmactrns TYPE zt_fmacwork_shift.

  DATA: lt_zfmactrnsbd    TYPE zt_fmacwork_shift,
        ls_zfmatrn_update TYPE zfmacwork_shift,
        lwa_zfmactrnsbd   TYPE zsc_fmacwork_shift.

  FIELD-SYMBOLS: <lwa_zfmactrns> TYPE zsc_fmacwork_shift .

  DELETE lt_zfmactrns WHERE actrn IS INITIAL.

  IF lt_zfmactrns[] IS NOT INITIAL.
    SELECT * FROM zfmacwork_shift
      INTO CORRESPONDING FIELDS OF TABLE lt_zfmactrnsbd
      FOR ALL ENTRIES IN lt_zfmactrns
     WHERE werks  = lt_zfmactrns-werks
       AND arbpl  = lt_zfmactrns-arbpl
       AND fabkl  = lt_zfmactrns-fabkl
       AND actrn  = lt_zfmactrns-actrn
       AND acppg  = lt_zfmactrns-acppg
       AND period = lt_zfmactrns-period.
    SORT lt_zfmactrnsbd BY werks arbpl fabkl actrn acppg period.
    LOOP AT lt_zfmactrns ASSIGNING <lwa_zfmactrns>.
      READ TABLE lt_zfmactrnsbd INTO lwa_zfmactrnsbd
        WITH KEY werks  = <lwa_zfmactrns>-werks
                 arbpl  = <lwa_zfmactrns>-arbpl
                 fabkl  = <lwa_zfmactrns>-fabkl
                 actrn  = <lwa_zfmactrns>-actrn
                 acppg  = <lwa_zfmactrns>-acppg
                 period = <lwa_zfmactrns>-period BINARY SEARCH.
      IF <lwa_zfmactrns> NE lwa_zfmactrnsbd.
        <lwa_zfmactrns>-updkz = 'U'.
      ENDIF.
    ENDLOOP.
  ENDIF.
  LOOP AT lt_zfmactrns ASSIGNING <lwa_zfmactrns>
                               WHERE updkz EQ 'U'.
    MOVE-CORRESPONDING <lwa_zfmactrns> TO ls_zfmatrn_update.
    MODIFY zfmacwork_shift FROM ls_zfmatrn_update.
  ENDLOOP.
*--- Update Data Base
  READ TABLE lt_zfmactrns WITH KEY updkz = 'U'
                          TRANSPORTING NO FIELDS.
  IF sy-subrc EQ 0.
    PERFORM commit_work.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form COMMIT_WORK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM commit_work .

  COMMIT WORK.

ENDFORM.
