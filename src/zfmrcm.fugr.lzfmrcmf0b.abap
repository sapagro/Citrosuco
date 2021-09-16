*&---------------------------------------------------------------------*
*& Form BAPI_COMMIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM bapi_commit .

  DATA: lt_return TYPE bapiret2.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait   = c_true
    IMPORTING
      return = lt_return.

ENDFORM.

FORM bom_update.

  DATA: lt_bom_itm       TYPE TABLE OF stpo_api03.
  DATA: ls_bom_itm       TYPE stpo_api03,
        ls_bom_hdr       TYPE stko_api01,
        ls_bom_hdr_expot TYPE stko_api02,
        ls_stpo_read     TYPE stpo_api02,
        lt_stko_read     TYPE TABLE OF stko_api02,
        fl_warnin        LIKE  capiflag-flwarning,
        lt_stpo          TYPE tty_stpo,
        lt_stas          TYPE TABLE OF stas,
        lwa_stpo         TYPE stpo,
        lwa_stas         TYPE stpo,
        lv_datum         TYPE csap_mbom-datuv,
        lv_bomnumber     TYPE stko_api02-bom_no..

*  IF sy-uname EQ 'T_T.KONNO'.
*    BREAK-POINT.
*  ENDIF.

  PERFORM messages_initialize USING gs_variables-initiator
                                   c_log_subobject-create
                                   gs_rcdoc_infocus-x-rchdr.
  PERFORM item_lista_tecnica_get CHANGING lt_stpo.
* Create
  LOOP AT gs_rcdoc_infocus-x-rclst INTO DATA(lwa_rclst)
                                   WHERE updkz = c_updkz_new.
    READ TABLE lt_stpo WITH KEY   idnrk  = lwa_rclst-matnr_ins
                                  stlty  = 'M'
                                  stlnr  = zsc_fmrchdr-stlnr
                                  datuv =  zsc_fmrchdr-datuv
                                  TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
      ls_bom_itm-item_categ  =  'L'.
      ls_bom_itm-item_no     =  lwa_rclst-posnr+2(4).
      ls_bom_itm-component   =  lwa_rclst-matnr_ins.
      IF zsc_fmrchdr-ausme = 'HC'.
        ls_bom_itm-comp_qty =  lwa_rclst-rcdos.
*...Vistex-11.01.2019/Begin
        REPLACE ALL OCCURRENCES OF '.' IN ls_bom_itm-comp_qty WITH ','.
*...Vistex-11.01.2019/End
      ELSEIF zsc_fmrchdr-ausme = 'VC'.
        ls_bom_itm-comp_qty    =  1.
      ENDIF.
      ls_bom_itm-comp_unit   =  lwa_rclst-units.

      CHECK  ls_bom_itm IS NOT INITIAL.
      APPEND ls_bom_itm TO lt_bom_itm.
      CLEAR: ls_bom_itm.
    ENDIF.
  ENDLOOP.

  CLEAR lwa_rclst.

*Update
  LOOP AT gs_rcdoc_infocus-x-rclst INTO DATA(lwa_rclst_u)
                                   WHERE updkz = c_updkz_update .
    READ TABLE lt_stpo  INTO DATA(lwa_stapo_u)
                        WITH KEY idnrk  = lwa_rclst_u-matnr_ins
                                 stlty  = 'M'
                                 postp  = 'L'
                                 posnr  = lwa_rclst_u-posnr+2(4)
                                 stlnr  = zsc_fmrchdr-stlnr
                                 datuv =  zsc_fmrchdr-datuv.
    IF sy-subrc EQ 0.
      ls_bom_itm-item_categ  =  'L'.
      ls_bom_itm-item_no     =  lwa_rclst_u-posnr+2(4).
      ls_bom_itm-component   =  lwa_rclst_u-matnr_ins.
      ls_bom_itm-item_node   =  lwa_stapo_u-stlkn.
      ls_bom_itm-item_count  =  lwa_stapo_u-stpoz.
      ls_bom_itm-item_node   =  lwa_stapo_u-stlkn.
      ls_bom_itm-item_count  =  lwa_stapo_u-stpoz.
      IF zsc_fmrchdr-ausme = 'HC'.
        ls_bom_itm-comp_qty = lwa_rclst_u-rcdos.
*...Vistex-11.01.2019/Begin
        REPLACE ALL OCCURRENCES OF '.' IN ls_bom_itm-comp_qty WITH ','.
*...Vistex-11.01.2019/End
      ELSEIF zsc_fmrchdr-ausme = 'VC'.
        ls_bom_itm-comp_qty    =  1.
      ENDIF.
      ls_bom_itm-comp_unit   =  lwa_rclst_u-units.

      CHECK  ls_bom_itm IS NOT INITIAL.
      APPEND ls_bom_itm TO lt_bom_itm.
      CLEAR: ls_bom_itm.
    ENDIF.
  ENDLOOP.


  MOVE zsc_fmrchdr-datuv TO lv_datum.
  date_formt_ddmmyyyy lv_datum lv_datum.

  READ TABLE gs_rcdoc_infocus-y-rclst
   WITH KEY updkz = c_updkz_delete TRANSPORTING NO FIELDS.
  CHECK sy-subrc EQ 0 OR
  lt_bom_itm[] IS NOT INITIAL.

*---Enquee Exceptions
  CALL FUNCTION 'CALO_INIT_API'
    EXCEPTIONS
      log_object_not_found     = 1
      log_sub_object_not_found = 2
      OTHERS                   = 3.
  IF sy-subrc <> 0
       AND NOT sy-msgid IS INITIAL
       AND NOT sy-msgty IS INITIAL
       AND NOT sy-msgno IS INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO sy-msgli.
    message_simple space.
    PERFORM messages_display USING gs_variables-initiator.
    EXIT.
  ENDIF.

  CALL FUNCTION 'CSAP_MAT_BOM_OPEN'
    EXPORTING
      material    = zsc_fmrchdr-matnr
      plant       = zsc_fmrchdr-werks
      bom_usage   = '1'
      alternative = zsc_fmrchdr-stlal
      valid_from  = lv_datum
    IMPORTING
      o_stko      = ls_bom_hdr_expot
      fl_warning  = fl_warnin
*    TABLES
*     t_stpo      = lt_bom_itm
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.
  IF sy-subrc <> 0
      AND NOT sy-msgid IS INITIAL
      AND NOT sy-msgty IS INITIAL
      AND NOT sy-msgno IS INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO sy-msgli.
    message_simple space.
    PERFORM messages_display USING gs_variables-initiator.
    EXIT.
  ENDIF.
*Delete
  LOOP AT gs_rcdoc_infocus-y-rclst INTO DATA(lwa_rclst_y)
                                 WHERE updkz = c_updkz_delete.
    READ TABLE lt_stpo  INTO DATA(lwa_stapo)
                        WITH KEY idnrk  = lwa_rclst_y-matnr_ins
                                 stlty  = 'M'
                                 postp  = 'L'
                                 posnr  = lwa_rclst_y-posnr+2(4)
                                 stlnr  = zsc_fmrchdr-stlnr
                                 datuv =  zsc_fmrchdr-datuv.
    IF sy-subrc EQ 0.
      ls_stpo_read-item_categ  = 'L'.
      ls_stpo_read-change_no   =  sy-datum.
      ls_stpo_read-changed_by  =  sy-uname.
      ls_stpo_read-valid_from  =  zsc_fmrchdr-datuv.
      ls_stpo_read-item_no     =  lwa_rclst_y-posnr+2(4).
      ls_stpo_read-component   =  lwa_rclst_y-matnr_ins.
      ls_stpo_read-comp_unit   =  lwa_rclst_y-units.
      ls_stpo_read-fldelete    =  c_true.
      ls_stpo_read-item_node   =  lwa_stapo-stlkn.
      ls_stpo_read-item_count  =  lwa_stapo-stpoz.
    ENDIF.
    IF ls_stpo_read IS NOT INITIAL.
    ELSE.
      CONTINUE.
    ENDIF.
    CALL FUNCTION 'CSAP_BOM_ITEM_MAINTAIN'
      EXPORTING
        i_stpo = ls_stpo_read
      EXCEPTIONS
        error  = 1
        OTHERS = 2.
    IF sy-subrc EQ 0.
      COMMIT WORK.
      CLEAR ls_stpo_read.
    ELSE.
      IF sy-subrc <> 0
      AND NOT sy-msgid IS INITIAL
      AND NOT sy-msgty IS INITIAL
      AND NOT sy-msgno IS INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                INTO sy-msgli.
        message_simple space.
        PERFORM messages_display USING gs_variables-initiator.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF lt_bom_itm[] IS NOT INITIAL.
    CALL FUNCTION 'CSAP_MAT_BOM_MAINTAIN'
      EXPORTING
        material           = zsc_fmrchdr-matnr
        plant              = zsc_fmrchdr-werks
        bom_usage          = '1'
        alternative        = zsc_fmrchdr-stlal
        valid_from         = lv_datum
        i_stko             = ls_bom_hdr
        fl_commit_and_wait = 'X'
        fl_bom_create      = 'X'
        fl_new_item        = 'X'
      IMPORTING
        fl_warning         = fl_warnin
      TABLES
        t_stpo             = lt_bom_itm.

    IF sy-subrc EQ 0.
      COMMIT WORK.
    ELSE.
      IF sy-subrc <> 0
      AND NOT sy-msgid IS INITIAL
      AND NOT sy-msgty IS INITIAL
      AND NOT sy-msgno IS INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                INTO sy-msgli.
        message_simple space.
        PERFORM messages_display USING gs_variables-initiator.
      ENDIF.
    ENDIF.
  ENDIF.
*Close BOM
  CALL FUNCTION 'CSAP_MAT_BOM_CLOSE'
    EXPORTING
      fl_commit_and_wait = 'X'
    IMPORTING
      fl_warning         = fl_warnin
    EXCEPTIONS
      error              = 1
      OTHERS             = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BOM_BYALTER_CREATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_RCDOC_INFOCUS_X_RCHDR_STLNR
*&      --> GS_RCDOC_INFOCUS_X_RCHDR_STLAL
*&---------------------------------------------------------------------*
FORM bom_byalter_create  USING  lv_stlnr TYPE stnum
                                lv_stlal TYPE stalt.

  DATA: ls_rcbom TYPE zsc_fmrcbom.

  FIELD-SYMBOLS:<lwa_rclst> TYPE zsc_fmrclst.
  CHECK lv_stlnr IS NOT INITIAL AND
        lv_stlal IS NOT INITIAL.
  REFRESH: gs_rcdoc_infocus-x-rcbom.
  LOOP AT gs_rcdoc_infocus-x-rclst INTO DATA(lwa_rclst).
    MOVE-CORRESPONDING lwa_rclst TO ls_rcbom.
    MOVE:
          lv_stlnr TO ls_rcbom-stlnr,
          lv_stlal TO ls_rcbom-stlal,
          c_updkz_new TO ls_rcbom-updkz.
    APPEND ls_rcbom TO gs_rcdoc_infocus-x-rcbom.
    CLEAR ls_rcbom.
  ENDLOOP.

  LOOP AT gs_rcdoc_infocus-x-rclst ASSIGNING <lwa_rclst>.
    MOVE: lv_stlal    TO <lwa_rclst>-stlal,
          c_updkz_new TO <lwa_rclst>-updkz.
  ENDLOOP.


  gs_variables-refresh_bom_grid = c_true.
  gs_variables-refresh_dose_grid = c_true.

  PERFORM document_infocus_save USING c_true.

ENDFORM.
