
*&---------------------------------------------------------------------*
*& Form PROCESS_ORDER_CREATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM process_order_create USING lt_plitm TYPE zt_fmplitm
                          CHANGING lt_messages TYPE /agri/t_gprolog.

  DATA: lt_cskey    TYPE /agri/t_glcs_key,
        lwa_csdoc   TYPE /agri/s_glcs_doc,
        lwa_cskey   TYPE /agri/s_glcs_key,
        lwa_plitm   TYPE  zsc_fmplitm,
        lt_glflcma  TYPE  /agri/t_glflcma,
        lt_glcsprs  TYPE  /agri/t_glcsprs,
        lt_glcsprso TYPE  /agri/t_glcsprso,
        lt_glcsprst TYPE  /agri/t_glcsprst,
        lt_glcsdfl  TYPE  /agri/t_glcsdfl.

  DATA: lwa_glflcma  TYPE  /agri/s_glflcma,
        lwa_glcsprs  TYPE  /agri/s_glcsprs,
        lwa_glcsprso TYPE  /agri/s_glcsprso,
        lwa_glcsprst TYPE  /agri/s_glcsprst,
        lwa_glcsdfl  TYPE  /agri/s_glcsdfl,
        lwa_message  TYPE  /agri/s_gprolog.


  CHECK lt_plitm[] IS NOT INITIAL.
  LOOP AT lt_plitm INTO lwa_plitm.
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
        lt_glcsprs  ASCENDING BY tplnr_fl contr,
        lt_glcsprso ASCENDING BY tplnr_fl contr,
        lt_glcsprst ASCENDING BY tplnr_fl contr,
        lt_glcsdfl  ASCENDING BY tplnr_fl contr.


  REFRESH: lt_cskey.
  CLEAR: lwa_cskey.
  LOOP AT lt_plitm INTO lwa_plitm.

    lwa_cskey-tplnr_fl = lwa_plitm-tplnr_fl.
    lwa_cskey-contr = lwa_plitm-contr.
    APPEND lwa_cskey TO lt_cskey.

    READ TABLE lt_glcsprs INTO lwa_glcsprs WITH KEY
                                             tplnr_fl = lwa_plitm-tplnr_fl
                                             contr   = lwa_plitm-contr
                                             CPROS   = lwa_plitm-cpros
                                             BINARY SEARCH.
    IF sy-subrc EQ 0.
      CALL FUNCTION '/AGRI/FMFP_ORDER_CREATE'
        EXPORTING
          i_save_messages       = c_true
          i_commit_work         = c_true
          i_cpros               = lwa_glcsprs-ppros
          i_gstrp               = lwa_plitm-pldil
          i_release             = 'X'
          it_cskey              = lt_cskey
        IMPORTING
          et_messages           = lt_messages
        EXCEPTIONS
          inconsistent_data     = 1
          no_valid_crop_seasons = 2
          OTHERS                = 3.
    ENDIF.
  ENDLOOP.

ENDFORM.
