*&---------------------------------------------------------------------*
*& Include          LZFG_ACMF0Y
*&---------------------------------------------------------------------*
FORM yield_get  USING    lv_aufnr
                         lv_matnr
                CHANGING lv_yield.

  DATA: lt_aufm     TYPE STANDARD TABLE OF aufm,
        lwa_aufm    TYPE aufm,
        lt_mseg     TYPE /agri/t_fmac_mseg,
        lt_mseg_tem TYPE /agri/t_fmac_mseg,
        lt_fmprdoc  TYPE /agri/t_fmac_pr,
        lr_budat    TYPE RANGE OF budat,
        lwa_budat   LIKE LINE OF lr_budat.

  FIELD-SYMBOLS:  <lwa_mseg>        TYPE /agri/s_fmac_mseg.

  CLEAR lv_yield.

  IF gs_tfmactyp-acapp EQ c_accom_appli-prnum.

    SELECT * INTO TABLE lt_aufm FROM aufm            "#EC CI_ALL_FIELDS_NEEDED   "#EC CI_NOORDER
*    WHERE budat BETWEEN /agri/s_fmachdr-strtdat AND  /agri/s_fmachdr-findat
    WHERE bwart BETWEEN c_movements-mov_101 AND c_movements-mov_102
    AND matnr = lv_matnr
    AND werks = /agri/s_fmachdr-werks
    AND aufnr = lv_aufnr.
*
    lwa_budat-option = c_operator_word-between.
    lwa_budat-sign   = c_sign-include.
    lwa_budat-low    = /agri/s_fmachdr-strtdat.
    lwa_budat-high   = /agri/s_fmachdr-findat.
    APPEND lwa_budat TO lr_budat.

    SELECT a~prnum a~gjahr a~bukrs a~budat
          b~dokar b~pdokar1
     FROM /agri/fmprhdr AS a
     INNER JOIN /agri/fmprdoc AS b
     ON a~prnum = b~prnum
     INTO TABLE lt_fmprdoc
     WHERE a~budat IN lr_budat
     AND   a~status = 'C'.

    SORT lt_fmprdoc BY pdokar1.

    IF lt_fmprdoc IS NOT INITIAL.

      SELECT mblnr aufnr bwart smbln matbf
        FROM mseg
        INTO TABLE lt_mseg
       FOR ALL ENTRIES IN lt_fmprdoc
         WHERE mblnr = lt_fmprdoc-pdokar1
         AND matbf = lv_matnr
         AND bwart = '101'.


      SELECT mblnr aufnr bwart smbln matbf
         FROM mseg
         INTO TABLE lt_mseg_tem
         WHERE aufnr = lv_aufnr
          AND matbf = lv_matnr
          AND bwart = '102'.

      IF lt_mseg_tem IS NOT INITIAL.
        LOOP AT lt_mseg_tem ASSIGNING <lwa_mseg> WHERE  bwart = c_movements-mov_102 .
          DELETE lt_mseg WHERE mblnr EQ <lwa_mseg>-smbln.
          DELETE lt_aufm WHERE mblnr EQ <lwa_mseg>-smbln.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ELSE.

    SELECT * INTO TABLE lt_aufm FROM aufm            "#EC CI_ALL_FIELDS_NEEDED        "#EC CI_NOORDER
    WHERE budat BETWEEN /agri/s_fmachdr-strtdat AND  /agri/s_fmachdr-findat
    AND bwart = c_movements-mov_101
    AND matnr = lv_matnr
    AND werks = /agri/s_fmachdr-werks
    AND aufnr = lv_aufnr.

    SELECT mblnr aufnr bwart smbln matbf
         FROM mseg
         INTO TABLE lt_mseg_tem
         WHERE aufnr = lv_aufnr
          AND matbf = lv_matnr
          AND bwart = '102'.

    IF lt_mseg_tem IS NOT INITIAL.
      LOOP AT lt_mseg_tem ASSIGNING <lwa_mseg> WHERE  bwart = c_movements-mov_102 .
        DELETE lt_aufm WHERE mblnr EQ <lwa_mseg>-smbln.
      ENDLOOP.
    ENDIF.

  ENDIF.

  LOOP AT lt_aufm INTO lwa_aufm. "#EC CI_ALL_FIELDS_NEEDED       "#EC CI_NOORDER
    IF lt_mseg[] IS NOT INITIAL.
      READ TABLE lt_mseg TRANSPORTING NO FIELDS WITH KEY mblnr = lwa_aufm-mblnr. "#EC CI_NOORDER
      IF sy-subrc EQ 0.
        lv_yield = lv_yield + lwa_aufm-menge.  "#EC CI_ALL_FIELDS_NEEDED      "#EC CI_NOORDER
      ENDIF.
    ELSE.
*      CASE lwa_aufm-bwart.
*        WHEN c_movements-mov_102.
*          MULTIPLY lwa_aufm-menge BY -1.
*      ENDCASE.
      lv_yield = lv_yield + lwa_aufm-menge.  "#EC CI_ALL_FIELDS_NEEDED      "#EC CI_NOORDER

    ENDIF.
  ENDLOOP.

ENDFORM.                    " YIELD_GET
