************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Enhancement Name  :  ZABS_IENH_CROP_SEASONS                          *
* Form Name         :  CS_HEADER_DATA_CHECK                            *
* Include Name      :  ZABS_INC_HEADER_DATA_CHECK                      *
* Created By        :  Jetendra Mantena                                *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  09.18.2019                                      *
* TR                :  C4DK901784                                      *
* Version           :  001                                             *
* Description       :  Not considering the Crop Seasons which are      *
*                      created for replanting                          *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*--Types declaration
 TYPES: BEGIN OF ty_flcma,
          tplnr_fl  TYPE /agri/gltplnr_fl,
          contr     TYPE /agri/gcontr,
*-- BOC T_T.KONNO-04.05.21
          cmnum     TYPE /agri/glcmnum,
          varia     TYPE /agri/glvaria,
          season    TYPE /agri/gl_season,
          datab     TYPE /agri/gldatab,
          datbi     TYPE /agri/gldatbi,
*-- EOC T_T.KONNO-04.05.21
          aarea     TYPE /agri/glaarea,
          garea     TYPE /agri/glgarea,
          zzreplant TYPE zabs_del_replant,
        END OF ty_flcma.

*--Internal table declaration
 DATA: lt_cskey_new TYPE /agri/t_glcs_key,
       lt_flcma     TYPE STANDARD TABLE OF ty_flcma,
       lt_flcma_tmp TYPE STANDARD TABLE OF ty_flcma,
       ls_flcma     TYPE ty_flcma,

*--Local Variable declaration
       lv_werks_new TYPE werks_d,
       lv_parea     TYPE /agri/glparea.

 IF lwa_cshdr-datab IS INITIAL OR
    lwa_cshdr-datbi IS INITIAL.
   lv_subrc = 4.
   MESSAGE e002(/agri/glcs) INTO sy-msgli.
   message_simple space.
   EXIT.
 ENDIF.

 IF lwa_cshdr-datab GT lwa_cshdr-datbi.
   lv_subrc = 4.
   MESSAGE e005(/agri/glcs) INTO sy-msgli.
   message_simple space.
   EXIT.
 ENDIF.

 IF lwa_cshdr_old-datab NE lwa_cshdr-datab OR
    lwa_cshdr_old-datbi NE lwa_cshdr-datbi.
*-- BOC T_T.KONNO-04.05.21
*   SELECT tplnr_fl contr aarea garea zzreplant
*     FROM /agri/glflcma
*     INTO CORRESPONDING FIELDS OF TABLE lt_flcma
*      AND contr     NE lwa_cshdr-contr
*      AND cmnum     EQ lwa_cshdr-cmnum
*      AND datbi     GE lwa_cshdr-datab
*      AND datab     LE lwa_cshdr-datbi
*      AND loevm     EQ space.
***-- SOC - Don't consider the crop seasons which are created for replanting
**       AND zzreplant NE abap_true.
***-- EOC - Don't consider the crop seasons which are created for replanting
   SELECT tplnr_fl, contr, cmnum, varia, season, datab,
          datbi, aarea, garea, zzreplant
     FROM /agri/glflcma
     INTO TABLE @lt_flcma
    WHERE tplnr_fl  EQ @lwa_cshdr-tplnr_fl
      AND contr     NE @lwa_cshdr-contr
      AND cmnum     EQ @lwa_cshdr-cmnum
      AND datbi     GE @lwa_cshdr-datab
      AND datab     LE @lwa_cshdr-datbi
      AND loevm     EQ @space.

   LOOP AT lt_flcma INTO ls_flcma.
     DATA(lv_flcma_tabix) = sy-tabix.
     READ TABLE gt_csdoc_infocus INTO DATA(ls_csdoc_ref)
       WITH KEY tplnr_fl = ls_flcma-tplnr_fl
                contr    = ls_flcma-contr.
     IF sy-subrc EQ 0.
       IF NOT ( ls_csdoc_ref-x-cshdr-datbi GE lwa_cshdr-datab AND
                ls_csdoc_ref-x-cshdr-datab LE lwa_cshdr-datbi ).
         DELETE lt_flcma INDEX lv_flcma_tabix.
       ENDIF.
     ENDIF.
   ENDLOOP.
*-- EOC T_T.KONNO-04.05.21

*-- EOC T_T.KONNO-04.05.21
*   IF sy-subrc EQ 0.
   IF lt_flcma[] IS NOT INITIAL.
*-- EOC T_T.KONNO-04.05.21
*--SOC by Chandrakanth
     lt_flcma_tmp = lt_flcma.

     DELETE lt_flcma_tmp WHERE zzreplant EQ abap_true.

     LOOP AT lt_flcma_tmp TRANSPORTING NO FIELDS
                      WHERE contr NE lwa_cshdr-contr.
       EXIT.
     ENDLOOP.
     IF sy-subrc EQ 0.
       lv_subrc = 4.
       MESSAGE e038(/agri/glcs) INTO sy-msgli.
       message_simple space.
       EXIT.
     ENDIF.

     lv_parea = ( lwa_cshdr-aarea / lwa_cshdr-garea ) * 100.

     LOOP AT lt_flcma INTO ls_flcma
                      WHERE contr NE lwa_cshdr-contr.
       lv_parea = lv_parea + ( ( ls_flcma-aarea / ls_flcma-garea ) * 100 ).
     ENDLOOP.

     IF lv_parea > 100.
       lv_subrc = 4.
       MESSAGE e078(zabs_msgcls) INTO sy-msgli.
       message_simple space.
       EXIT.
     ENDIF.
   ENDIF.
*--EOC by Chandrakanth

*--Fetching plant from crop master table to check validity
   SELECT SINGLE werks INTO lv_werks_new
     FROM /agri/glcmwrk
    WHERE cmnum EQ lwa_cshdr-cmnum
      AND werks EQ lwa_cshdr-iwerk
      AND datbi GE lwa_cshdr-datbi
      AND datab LE lwa_cshdr-datab.
   IF sy-subrc NE 0.
     MESSAGE e033(/agri/glcs) WITH lwa_cshdr-iwerk lwa_cshdr-cmnum
                             INTO sy-msgli.
     message_simple space.
     lv_subrc = 4.
     EXIT.
   ENDIF.
 ENDIF.

 IF lwa_cshdr-astat IS INITIAL.
   lv_subrc = 4.
   MESSAGE e051(/agri/glcs) INTO sy-msgli.
   message_simple space.
   EXIT.
 ENDIF.

 RETURN.
