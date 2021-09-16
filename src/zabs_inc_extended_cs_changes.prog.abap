************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Enhancement Name  :  ZABS_IENH_CROP_SEASONS                          *
* Form Name         :  EXTEND_CROP_SEASONS                             *
* Include Name      :  ZABS_INC_EXTENDED_CS_CHANGES                    *
* Created By        :  Jetendra Mantena                                *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  09.17.2019                                      *
* TR                :  C4DK901784                                      *
* Version           :  001                                             *
* Description       :  In Crop Season, when user clicks on Extend CS   *
*                      give a pop up to enter season and year for the  *
*                      Extended CS.                                    *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*--To change Season for Extended Crop season.
READ TABLE lt_csdoc INTO DATA(lwa_csdoc_temp) INDEX 1.
IF lwa_csdoc_temp-x-cshdr-class = zcl_abs_abap_maintain=>c_cs_farming_appl.

*--Workarea declaration
  DATA: lwa_view_data_n TYPE /agri/s_glcsview_flds,
        lwa_cshdr_n     TYPE /agri/s_glflcma,
        lwa_csdoc_n     TYPE /agri/s_glcs_doc,

*--Internal table declaration
        lt_cshdr_n      TYPE /agri/t_glflcma,

*--Local variable declaration
        lv_subrc_n      TYPE sy-subrc,
        lv_season       TYPE /agri/gl_season,
        lv_year         TYPE gjahr.

*--SOC by Jetendra M 17 Sep 2019
*--Calling Pop Up FM to get season and year
  CALL FUNCTION 'ZABS_FM_CS_POPUP_INPUT'
    EXPORTING
      is_csdoc = lwa_csdoc_temp
    IMPORTING
      e_season = lv_season
      e_year   = lv_year.

  IF lv_season IS INITIAL OR lv_year IS INITIAL.
    REFRESH lt_csdoc.
    RETURN.
  ENDIF.
*--EOC by Jetendra M 17 sep 2019

  SELECT * FROM /agri/glflcma                 "#EC CI_ALL_FIELDS_NEEDED
    INTO CORRESPONDING FIELDS OF TABLE lt_cshdr_n
     FOR ALL ENTRIES IN lt_csdoc
   WHERE tplnr_fl EQ lt_csdoc-tplnr_fl.
  IF sy-subrc EQ 0.
    SORT lt_cshdr_n BY tplnr_fl contr DESCENDING.
  ENDIF.

  LOOP AT lt_csdoc INTO lwa_csdoc_n.

    CLEAR: /agri/s_glflcma.
    MOVE-CORRESPONDING lwa_csdoc_n-x-cshdr TO /agri/s_glflcma.

    IF /agri/s_glflcma-datab_ref IS INITIAL.
      /agri/s_glflcma-datab_ref = /agri/s_glflcma-datab.
    ENDIF.
    /agri/s_glflcma-datab       = /agri/s_glflcma-datbi + 1.
*--SOC by Jetendra M 17 Sep 2019
*--Overwritting season and year for CS based on Pop Up Screen
    IF lv_season IS NOT INITIAL.
      /agri/s_glflcma-season      = lv_season.
    ENDIF.
    IF lv_year IS NOT INITIAL.
      /agri/s_glcsscrfields-gyear = lv_year.
    ELSE.
      /agri/s_glcsscrfields-gyear = /agri/s_glflcma-datab(4).
    ENDIF.
*--EOC by Jetendra M 17 Sep 2019
    /agri/s_glflcma-contr_ref   = /agri/s_glflcma-contr.
    /agri/s_glflcma-gncnt       = /agri/s_glflcma-gncnt + 1.

    PERFORM cs_prepare_check CHANGING lv_subrc_n.
    CHECK lv_subrc_n IS INITIAL.

    CLEAR: gs_csdoc_infocus,
           /agri/s_glflcma-contr, /agri/s_glflcma-datbi,
           /agri/s_glflcma-proid, /agri/s_glflcma-varia,
           /agri/s_glflcma-rtnid, /agri/s_glflcma-aenam,
           /agri/s_glflcma-aedat, /agri/s_glflcma-aezet.

    PERFORM cs_default_values_fill.

    MOVE-CORRESPONDING /agri/s_glflcma TO gs_csdoc_infocus-x-cshdr.
    gs_csdoc_infocus-tplnr_fl = gs_csdoc_infocus-x-cshdr-tplnr_fl.
    gs_csdoc_infocus-x-cshdr-updkz = c_updkz_new.

    PERFORM cs_control_read USING gs_csdoc_infocus-x-cshdr-tplkz
                                  gs_csdoc_infocus-x-cshdr-tplvl
                         CHANGING lv_subrc_n.
    IF lv_subrc_n IS NOT INITIAL.
      PERFORM document_infocus_unlock
            USING gs_csdoc_infocus-x-cshdr-tplnr_fl.
      CLEAR: lv_subrc_n, /agri/s_glflcma, gs_csdoc_infocus.
      CONTINUE.
    ENDIF.
    MOVE-CORRESPONDING gs_tglcsatl TO gs_csdoc_infocus-x-cshdr.

    CLEAR: lwa_cshdr_n.
    READ TABLE lt_cshdr_n INTO lwa_cshdr_n
                        WITH KEY tplnr_fl = gs_csdoc_infocus-x-cshdr-tplnr_fl
                      BINARY SEARCH.
    LOOP AT gt_csdoc_infocus INTO lwa_csdoc_n
                            WHERE tplnr_fl EQ gs_csdoc_infocus-x-cshdr-tplnr_fl.
      IF lwa_csdoc_n-contr GT lwa_cshdr_n-contr.
        lwa_cshdr_n-contr = lwa_csdoc_n-contr.
      ENDIF.
    ENDLOOP.
    gs_csdoc_infocus-contr = gs_csdoc_infocus-x-cshdr-contr
                           = lwa_cshdr_n-contr + 1.

    PERFORM authority_check USING gs_csdoc_infocus-x-cshdr
                                  c_authorization_activity-create
                                  c_msg_type-info
                         CHANGING lv_subrc_n.
    IF lv_subrc_n IS NOT INITIAL.
      CLEAR: lv_subrc_n, /agri/s_glflcma, gs_csdoc_infocus.
      CONTINUE.
    ENDIF.
    PERFORM cs_processes_prepare.

    PERFORM cs_dates_prepare.
    PERFORM user_fields_complete USING '2'
                                       gs_csdoc_infocus-x-cshdr-tplkz
                                       gs_csdoc_infocus-x-cshdr-tplvl
                              CHANGING gs_csdoc_infocus-x-cshdr
                                       lwa_view_data_n.
    MOVE-CORRESPONDING lwa_view_data_n TO gt_view_data.

    APPEND gs_csdoc_infocus TO gt_csdoc_infocus.
    gt_view_data-tplnr_fl = gs_csdoc_infocus-x-cshdr-tplnr_fl.
    gt_view_data-contr = gs_csdoc_infocus-x-cshdr-contr.
    APPEND gt_view_data.
    CLEAR: gs_csdoc_infocus, gt_view_data, lwa_view_data_n.

  ENDLOOP.

  SORT: gt_csdoc_infocus BY tplnr_fl contr,
        gt_view_data     BY tplnr_fl contr.

  RETURN.

ENDIF.
