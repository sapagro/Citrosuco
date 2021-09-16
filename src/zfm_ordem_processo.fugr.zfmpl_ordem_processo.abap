FUNCTION zfmpl_ordem_processo.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IT_PLITM) TYPE  ZT_FMPLITM OPTIONAL
*"  EXPORTING
*"     VALUE(ET_LOG) TYPE  /AGRI/T_GPROLOG
*"----------------------------------------------------------------------
  DATA c_true TYPE c VALUE ''.

  DATA: lt_cskey    TYPE /agri/t_glcs_key,
        lwa_csdoc   TYPE /agri/s_glcs_doc,
        lwa_cskey   TYPE /agri/s_glcs_key,
        lwa_plitm   TYPE  zsc_fmplitm,
        lt_glflcma  TYPE  /agri/t_glflcma,
        lt_glcsprs  TYPE  /agri/t_glcsprs,
        lt_glcsprso TYPE  /agri/t_glcsprso,
        lt_glcsprst TYPE  /agri/t_glcsprst,
        lt_glcsdfl  TYPE  /agri/t_glcsdfl,
        lt_log      TYPE /agri/t_gprolog.

  DATA: lwa_glflcma  TYPE  /agri/s_glflcma,
        lwa_glcsprs  TYPE  /agri/s_glcsprs,
        lwa_glcsprso TYPE  /agri/s_glcsprso,
        lwa_glcsprst TYPE  /agri/s_glcsprst,
        lwa_glcsdfl  TYPE  /agri/s_glcsdfl,
        lwa_message  TYPE  /agri/s_gprolog.


  CHECK it_plitm[] IS NOT INITIAL.
  LOOP AT it_plitm INTO lwa_plitm.
    lwa_cskey-tplnr_fl = lwa_plitm-tplnr_fl.
    lwa_cskey-contr = lwa_plitm-contr.
    APPEND lwa_cskey TO lt_cskey.
  ENDLOOP.

  CALL FUNCTION '/AGRI/GLCS_READ'
    EXPORTING
      it_cskey       = lt_cskey
    IMPORTING
      et_flcma       = lt_glflcma
      et_csprs       = lt_glcsprs
      et_csprso      = lt_glcsprso
      et_csprst      = lt_glcsprst
      et_csdfl       = lt_glcsdfl
    EXCEPTIONS
      no_data_exists = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  SORT: lt_glflcma  ASCENDING BY tplnr_fl contr,
        lt_glcsprs  ASCENDING BY tplnr_fl contr cpros,
        lt_glcsprso ASCENDING BY tplnr_fl contr,
        lt_glcsprst ASCENDING BY tplnr_fl contr,
        lt_glcsdfl  ASCENDING BY tplnr_fl contr.

  LOOP AT it_plitm INTO lwa_plitm.
    REFRESH: lt_cskey.
    CLEAR: lwa_cskey.
    lwa_cskey-tplnr_fl = lwa_plitm-tplnr_fl.
    lwa_cskey-contr    = lwa_plitm-contr.
    APPEND lwa_cskey TO lt_cskey.

    READ TABLE lt_glcsprs INTO lwa_glcsprs
      WITH KEY tplnr_fl = lwa_plitm-tplnr_fl
               contr    = lwa_plitm-contr
               cpros    = lwa_plitm-cpros BINARY SEARCH.
    IF sy-subrc EQ 0.
      lwa_glcsprs-ppros = lwa_glcsprs-cpros.
      CALL FUNCTION '/AGRI/FMFP_ORDER_CREATE'
        EXPORTING
          i_save_messages       = c_true
          i_commit_work         = c_true
          i_cpros               = lwa_glcsprs-ppros
          i_gstrp               = lwa_plitm-pldil
          i_release             = 'X'
          it_cskey              = lt_cskey
        IMPORTING
          et_messages           = lt_log
        EXCEPTIONS
          inconsistent_data     = 1
          no_valid_crop_seasons = 2
          OTHERS                = 3.
      APPEND LINES OF lt_log TO et_log.
    ENDIF.
  ENDLOOP.

ENDFUNCTION.
