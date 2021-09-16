*&---------------------------------------------------------------------*
*& Include ZABS_INC_ITEMS_DISP
*&---------------------------------------------------------------------*

TYPES: BEGIN OF ty_mchb,
         matnr  TYPE matnr,
         werks  TYPE werks_d,
         lgort  TYPE lgort_d,
         charg  TYPE charg_d,
         clabs  TYPE labst,
       END OF ty_mchb.

DATA: lt_mchb     TYPE STANDARD TABLE OF ty_mchb,
      ls_mchb     TYPE ty_mchb,
      ls_fpcom    TYPE /agri/s_fmfpcom,
      lt_fpcom    TYPE /agri/t_fmfpcom,
      lv_contr    TYPE /agri/fmccontr,
      lv_lmnga    TYPE /agri/fmlmnga,
      ls_fpcom_in TYPE /agri/s_fmfpcom.

 IF ref_grid_confirmations IS INITIAL.
  LOOP AT gt_fpcnf_fcat INTO lwa_fpcnf_fcat.
    LOOP AT gs_fpdoc_infocus-x-fpcom INTO ls_fpcom
                    WHERE aufnr EQ lwa_fpcnf_fcat-aufnr
                      AND posnr EQ lwa_fpcnf_fcat-posnr
                      AND flgch EQ 'X'.
      CLEAR ls_mchb.
      ls_mchb-matnr = ls_fpcom-matnr.
      ls_mchb-werks = ls_fpcom-werks.
      ls_mchb-lgort = ls_fpcom-lgort.
      COLLECT ls_mchb INTO lt_mchb.
    ENDLOOP.
  ENDLOOP.

  IF lt_mchb IS NOT INITIAL.
    SELECT matnr werks lgort charg clabs
    FROM mchb
    INTO TABLE lt_mchb
    FOR ALL ENTRIES IN lt_mchb
    WHERE matnr EQ lt_mchb-matnr
    AND werks EQ lt_mchb-werks
    AND lgort EQ lt_mchb-lgort.
    IF sy-subrc EQ 0.
      SORT lt_mchb BY matnr werks lgort.
    ENDIF.
  ENDIF.

  REFRESH lt_fpcom.
  LOOP AT gs_fpdoc_infocus-x-fpcom INTO ls_fpcom
                                   WHERE aufnr EQ /agri/s_fmfp_cnf-aufnr
                                   AND   posnr EQ /agri/s_fmfp_cnf-posnr.
  READ TABLE <gs_fpdoc_infocus>-x-fpcom INTO ls_fpcom_in
                                          WITH KEY aufnr = ls_fpcom-aufnr
                                                posnr = ls_fpcom-posnr
                                                matnr = ls_fpcom-matnr.
    IF sy-subrc EQ 0.
      APPEND ls_fpcom_in TO lt_fpcom.
    ENDIF.
  ENDLOOP.

   CLEAR <gs_fpdoc_infocus>-x-fpcom.
   LOOP AT gt_fpcnf_fcat INTO lwa_fpcnf_fcat.
    CLEAR lv_contr.
   LOOP AT lt_fpcom INTO ls_fpcom
                    WHERE aufnr EQ lwa_fpcnf_fcat-aufnr
                    AND   posnr EQ lwa_fpcnf_fcat-posnr.
      IF ls_fpcom-flgch IS INITIAL.
        lv_contr = lv_contr + 1.
        ls_fpcom-contr = lv_contr.
        APPEND ls_fpcom TO <gs_fpdoc_infocus>-x-fpcom.
      ELSE.
        lv_lmnga = lwa_fpcnf_fcat-lmnga.
        LOOP AT lt_mchb INTO ls_mchb WHERE matnr = ls_fpcom-matnr
                                     AND werks = ls_fpcom-werks
                                     AND lgort = ls_fpcom-lgort.
          ls_fpcom-charg = ls_mchb-charg.
          IF lv_lmnga > ls_mchb-clabs.
            ls_fpcom-lmnga = ls_mchb-clabs.
            lv_lmnga = lv_lmnga - ls_mchb-clabs.
          ELSE.
            ls_fpcom-lmnga = lv_lmnga.
            lv_lmnga = 0.
          ENDIF.
          lv_contr = lv_contr + 1.
          ls_fpcom-contr = lv_contr.
          APPEND ls_fpcom TO <gs_fpdoc_infocus>-x-fpcom.
        ENDLOOP.
        IF sy-subrc NE 0.
          lv_contr = lv_contr + 1.
          ls_fpcom-contr = lv_contr.
          APPEND ls_fpcom TO <gs_fpdoc_infocus>-x-fpcom.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDIF.
