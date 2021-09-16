************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Enhancement Name  :  ZABS_IENH_CROP_SEASONS                          *
* Form Name         :  MASS_INFOCUS_CHECK                              *
* Include Name      :  ZABS_INC_MASS_INFOCUS_CHECK                     *
* Created By        :  Jetendra Mantena                                *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  09.20.2019                                      *
* TR                :  C4DK901784                                      *
* Version           :  001                                             *
* Description       :  Do not consider the Replanting Crop Season      *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*--Local variable declaration
 DATA: lv_subrc_new      TYPE sy-subrc,
       lv_tabix_new      TYPE sy-tabix,
       lv_view_tabix_new TYPE sy-tabix.

  LOOP AT gt_csdoc_infocus INTO gs_csdoc_infocus.

    lv_tabix_new = sy-tabix.
    IF gs_csdoc_infocus-x-cshdr-updkz IS INITIAL.
      LOOP AT gs_csdoc_infocus-x-csprs TRANSPORTING NO FIELDS
                                 WHERE updkz IS NOT INITIAL.
        EXIT.
      ENDLOOP.
      CHECK sy-subrc EQ 0.
    ENDIF.
    PERFORM messages_context_set USING gs_csdoc_infocus-x-cshdr.

    CLEAR: gs_csdoc_infocus-x-cshdr-error.
    MOVE-CORRESPONDING gs_csdoc_infocus-x-cshdr TO /agri/s_glflcma.

    READ TABLE gt_view_data WITH KEY tplnr_fl = /agri/s_glflcma-tplnr_fl
                                     contr    = /agri/s_glflcma-contr
                          BINARY SEARCH.
    IF sy-subrc EQ 0.
      lv_view_tabix_new = sy-tabix.
      MOVE-CORRESPONDING gt_view_data TO /agri/s_glcsview_flds.
    ENDIF.

    PERFORM document_infocus_check CHANGING lv_subrc_new.
    IF lv_subrc_new IS INITIAL.
      LOOP AT gt_csdoc_infocus TRANSPORTING NO FIELDS
                      WHERE x-cshdr-tplnr_fl  EQ /agri/s_glflcma-tplnr_fl
                        AND x-cshdr-cmnum     EQ /agri/s_glflcma-cmnum
                        AND x-cshdr-datbi     GE /agri/s_glflcma-datab
                        AND x-cshdr-datab     LE /agri/s_glflcma-datbi
                        AND x-cshdr-loevm     EQ space
                        AND x-cshdr-contr     NE /agri/s_glflcma-contr
*-- Do not consider the replanning crop season
                        AND x-cshdr-zzreplant NE abap_true.
*-- Do not consider the replanning crop season
        EXIT.
      ENDLOOP.
      IF sy-subrc EQ 0.
        lv_subrc_new = 4.
        MESSAGE e038(/agri/glcs) INTO sy-msgli.
        message_simple space.
      ENDIF.
    ENDIF.

    IF lv_subrc_new IS NOT INITIAL.
      gs_csdoc_infocus-x-cshdr-error = c_true.
      gs_variables-errors = c_true.
      CLEAR lv_subrc_new.
    ENDIF.

    MODIFY gt_csdoc_infocus FROM gs_csdoc_infocus INDEX lv_tabix_new.

    MOVE-CORRESPONDING /agri/s_glcsview_flds TO gt_view_data.
    MODIFY gt_view_data INDEX lv_view_tabix_new.
    CLEAR: /agri/s_glcsview_flds.

  ENDLOOP.

  RETURN.
