FUNCTION zfmac_data_crea .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_ACDES) TYPE  CHAR40
*"     REFERENCE(I_WERKS) TYPE  WERKS_D
*"     REFERENCE(I_CMNUM) TYPE  /AGRI/GLCMNUM
*"     REFERENCE(I_DATAB) TYPE  /AGRI/GLDATAB
*"     REFERENCE(I_DATBI) TYPE  /AGRI/GLDATBI
*"  EXPORTING
*"     REFERENCE(ET_ACITM) TYPE  ZT_FMACITM
*"     REFERENCE(ES_ACHDR) TYPE  ZSC_FMACHDR
*"     REFERENCE(ET_ACVLC) TYPE  ZT_FMACVLCL
*"  EXCEPTIONS
*"      DATA_NO_EXITS
*"      DESCRIPTION_NOT_EXITS
*"      PLANT_NOT_EXITS
*"      CROP_NOT_EXITS
*"      INITAL_DATE_NOT_EXITS
*"      FINISH_DATE_NOT_EXITS
*"      VARIETY_NOT_EXITS
*"----------------------------------------------------------------------


  REFRESH: et_acitm[].

  IF i_acdes IS INITIAL.
    RAISE description_not_exits.
  ENDIF.

  IF i_werks IS INITIAL.
    RAISE plant_not_exits.
  ENDIF.
  IF i_cmnum IS INITIAL.
    RAISE crop_not_exits.
  ENDIF.
  IF i_datab IS INITIAL.
    RAISE inital_date_not_exits.
  ENDIF.
  IF i_datbi IS INITIAL.
    RAISE finish_date_not_exits.
  ENDIF.

**--- Header
  es_achdr-acdes  = i_acdes.
  es_achdr-acnum  = c_upkdz_local.
  es_achdr-cmnum  = i_cmnum.
  es_achdr-datab  = i_datab.
  es_achdr-datbi  = i_datbi.
  es_achdr-werks  = i_werks.
  es_achdr-updkz  = c_updkz_new.

  SELECT * FROM /agri/glflcma AS a INNER JOIN /agri/glflot AS t
                ON a~tplnr_fl EQ t~tplnr_fl
               INTO CORRESPONDING FIELDS OF TABLE et_acitm
           WHERE t~iwerk = i_werks
            AND a~cmnum =  i_cmnum
               AND ( a~datab >= i_datab
           AND a~datbi =< i_datbi )
        AND a~loevm EQ space.

  IF sy-subrc NE 0.
    RAISE data_no_exits.
  ELSE.
    PERFORM attribute_get CHANGING et_acitm.
  ENDIF.

ENDFUNCTION.
