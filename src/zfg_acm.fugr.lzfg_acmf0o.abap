*&---------------------------------------------------------------------*
*& Include          LZFG_ACMF0O
*&---------------------------------------------------------------------*

FORM owner_text_get  USING lv_owrol lv_owner
                  CHANGING lv_owner_txt.

  DATA: lv_nrart LIKE tpar-nrart.

  CHECK NOT lv_owrol IS INITIAL AND NOT lv_owner IS INITIAL.

  SELECT SINGLE nrart FROM tpar INTO lv_nrart
                         WHERE parvw = lv_owrol.

  partner_name_get lv_nrart
                   lv_owner
                   space
                   space
                   lv_owner_txt.

ENDFORM.                    "owner_text_get
*&---------------------------------------------------------------------*
*&      Form  OCNUM_GET
*&---------------------------------------------------------------------*
FORM ocnum_get CHANGING lt_ocnum   TYPE /agri/t_fmocnum
                        lt_fmprdoc TYPE /agri/t_fmac_pr.

  DATA: lt_fmactsk  TYPE TABLE OF /agri/fmactsk,
        lt_fmactar  TYPE TABLE OF /agri/tfmactar,
        lt_afwi     TYPE TABLE OF afwi,
        lwa_afwi    TYPE afwi,
        lr_budat    TYPE RANGE OF budat,
        lt_mseg     TYPE /agri/t_fmac_mseg,
        lt_mara     TYPE /agri/t_fmac_mara,
        lwa_mara    TYPE /agri/s_fmac_mara,
        lwa_fmactar TYPE /agri/tfmactar,
        lwa_budat   LIKE LINE OF lr_budat,
        lwa_fmactsk TYPE /agri/fmactsk.

  RANGES: lr_matnr    FOR mara-matnr,
          lr_mblnr    FOR afwi-mblnr,
          lr_rueck    FOR afwi-rueck,
          lr_rmzhl    FOR afwi-rmzhl.

  FIELD-SYMBOLS: <lwa_mseg>   TYPE /agri/s_fmac_mseg,
                 <lwa_aufnr>  LIKE LINE OF gr_aufnr.

  lwa_budat-option = c_operator_word-between.
  lwa_budat-sign   = c_sign-include.
  lwa_budat-low    = /agri/s_fmachdr-strtdat.
  lwa_budat-high   = /agri/s_fmachdr-findat.
  APPEND lwa_budat TO lr_budat.

  IF gs_tfmactyp-acapp EQ c_accom_appli-wonum.

    SELECT ocnum FROM /agri/fmochdr
      INTO TABLE lt_ocnum
     WHERE wonum = gs_acdoc_infocus-x-achdr-wonum
       AND erdat IN lr_budat.

  ELSEIF gs_tfmactyp-acapp EQ c_accom_appli-aufnr.

    SELECT * FROM /agri/fmactsk
      INTO TABLE lt_fmactsk
      WHERE accom = gs_acdoc_infocus-accom.

    IF lt_fmactsk IS NOT INITIAL.
*      SELECT ocnum FROM /agri/fmocopr
*        INTO TABLE lt_ocnum
*        FOR ALL ENTRIES IN lt_fmactsk
*       WHERE aufnr = lt_fmactsk-aufnr.

      SELECT mblnr aufnr bwart smbln matbf
      FROM mseg
      INTO TABLE lt_mseg
      FOR ALL ENTRIES IN lt_fmactsk  "#EC CI_NO_TRANSFORM
      WHERE aufnr EQ lt_fmactsk-aufnr
*          WHERE aufnr in gr_aufnr[]
        AND bwart      EQ '101'
        AND budat_mkpf IN lr_budat.
***  13.05.2016 If the task is new.
    ELSEIF gr_aufnr[] IS NOT INITIAL.

*      LOOP AT gr_aufnr[] ASSIGNING <lwa_aufnr>.
*        MOVE <lwa_aufnr>-low TO lwa_fmactsk-aufnr.
*        APPEND lwa_fmactsk TO lt_fmactsk.
*      ENDLOOP.

*      IF lt_fmactsk IS NOT INITIAL.

      SELECT mblnr aufnr bwart smbln matbf
      FROM mseg
      INTO TABLE lt_mseg
*        FOR ALL ENTRIES IN lt_fmactsk
*        WHERE aufnr EQ lt_fmactsk-aufnr
        WHERE aufnr IN gr_aufnr[]
        AND bwart      EQ '101'
        AND budat_mkpf IN lr_budat.

*      ENDIF.
***        13.05.2016
    ENDIF.
  ELSEIF gs_tfmactyp-acapp EQ c_accom_appli-prnum.

    SELECT a~prnum a~gjahr a~bukrs a~budat
           b~dokar b~pdokar1
      FROM /agri/fmprhdr AS a
      INNER JOIN /agri/fmprdoc AS b
      ON a~prnum = b~prnum
      INTO TABLE lt_fmprdoc
      WHERE a~budat IN lr_budat
      AND   a~status = 'C'.
*      AND   b~loekz = space. " Produce Receipt close

    SORT lt_fmprdoc BY pdokar1.

    IF lt_fmprdoc IS NOT INITIAL.
      SELECT mblnr aufnr bwart smbln matbf
         FROM mseg
         INTO TABLE lt_mseg
         FOR ALL ENTRIES IN lt_fmprdoc
         WHERE mblnr      EQ lt_fmprdoc-pdokar1
*           AND budat_mkpf IN lr_budat
           AND bwart      EQ '101'.
    ENDIF.
  ENDIF.

  IF lt_mseg IS NOT INITIAL.
    SELECT * FROM /agri/tfmactar
        INTO TABLE lt_fmactar
        WHERE actyp EQ gs_tfmactyp-actyp.
    SORT lt_fmactar BY mtart.
    IF lt_fmactar IS NOT INITIAL.
      SELECT matnr mtart
         FROM mara
         INTO TABLE lt_mara
         FOR ALL ENTRIES IN lt_mseg  "#EC CI_NO_TRANSFORM
          WHERE matnr EQ lt_mseg-matbf.
      SORT lt_mara BY matnr.
    ENDIF.
    IF lt_mara IS NOT INITIAL.
      LOOP AT lt_mseg ASSIGNING <lwa_mseg>.
        READ TABLE lt_mara INTO lwa_mara
                            WITH KEY matnr = <lwa_mseg>-matbf
                            BINARY SEARCH.
        CHECK sy-subrc EQ 0.
        READ TABLE lt_fmactar INTO lwa_fmactar
                              WITH KEY mtart = lwa_mara-mtart
                              BINARY SEARCH.
        IF sy-subrc NE 0.
          lr_matnr-option = c_operator_word-equalto.
          lr_matnr-sign   = c_sign-include.
          lr_matnr-low    = lwa_mara-matnr.
          APPEND lr_matnr TO lr_matnr[].
        ENDIF.
      ENDLOOP.
      IF lt_mseg IS NOT INITIAL.
        IF lr_matnr[] IS NOT INITIAL.
          DELETE lt_mseg WHERE matbf IN lr_matnr[].
        ENDIF.
        IF lt_mseg IS NOT INITIAL.
****29/08/2016
          SELECT *
          FROM afwi
          INTO CORRESPONDING FIELDS OF TABLE lt_afwi
          FOR ALL ENTRIES IN lt_mseg
          WHERE mblnr EQ lt_mseg-mblnr.

          IF  lt_afwi IS NOT INITIAL.

            LOOP AT lt_afwi INTO lwa_afwi.

              lr_mblnr-option = c_operator_word-equalto.
              lr_mblnr-sign   = c_sign-include.
              lr_mblnr-low    = lwa_afwi-mblnr.
              APPEND lr_mblnr TO lr_mblnr[].

              lr_rueck-option = c_operator_word-equalto.
              lr_rueck-sign   = c_sign-include.
              lr_rueck-low    = lwa_afwi-rueck.
              APPEND lr_rueck TO lr_rueck[].

              lr_rmzhl-option = c_operator_word-equalto.
              lr_rmzhl-sign   = c_sign-include.
              lr_rmzhl-low    = lwa_afwi-rmzhl.
              APPEND lr_rmzhl TO lr_rmzhl[].

            ENDLOOP.

            DELETE lt_mseg WHERE mblnr NOT IN lr_mblnr[].

            IF lt_mseg IS NOT INITIAL.

              IF  gs_tfmactyp-acapp EQ c_accom_appli-aufnr.

                SELECT ocnum FROM /agri/fmocopr
                INTO TABLE lt_ocnum
                FOR ALL ENTRIES IN lt_mseg
                WHERE aufnr = lt_mseg-aufnr
                AND rueck IN lr_rueck
                AND rmzhl IN lr_rmzhl
                AND budat IN lr_budat.

              ELSE.

                SELECT ocnum FROM /agri/fmocopr
                INTO TABLE lt_ocnum
                FOR ALL ENTRIES IN lt_mseg
                WHERE aufnr = lt_mseg-aufnr
                AND rueck IN lr_rueck
                AND rmzhl IN lr_rmzhl.

              ENDIF.

            ENDIF.

          ENDIF.
        ENDIF.
*****
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " OCNUM_GET
