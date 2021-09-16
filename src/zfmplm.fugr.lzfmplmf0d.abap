*----------------------------------------------------------------------*
***INCLUDE /AGRI/LGLacMNF0D .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_INFOCUS_UNLOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM document_infocus_unlock USING lv_acnum TYPE zfmacnum
                                   lv_ajahr TYPE ajahr.

  CHECK lv_acnum IS NOT INITIAL
         AND lv_ajahr IS NOT INITIAL.

  CALL FUNCTION 'DEQUEUE_EZ_FMAC'
    EXPORTING
*     mode_zfmachdr = 'X'
      mandt = sy-mandt
      acnum = lv_acnum
      ajahr = lv_ajahr.

ENDFORM.                    " DOCUMENT_INFOCUS_UNLOCK
*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_DATA_INITIALIZE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM document_data_initialize  USING lv_refresh_messages.

  object_refresh_all.
  CLEAR: gs_acdoc_infocus,
         gs_variables-document_mode,
         gs_variables-data_changed,
         gs_variables-manual_changes,
         gs_variables-errors.

  REFRESH: gt_achdr,
           gt_fmacitm_fcat.

  IF NOT lv_refresh_messages IS INITIAL.
    messages_init.
  ENDIF.

  IF ref_text IS NOT INITIAL.
    CALL METHOD ref_text->init.
    FREE ref_text.
  ENDIF.

  object_refresh_all.

ENDFORM.                    " DOCUMENT_DATA_INITIALIZE
*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_INFOCUS_CLEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM document_infocus_clear  CHANGING lv_continue.

  DATA: lv_answer(1).

  IF gs_variables-exit_after_save IS NOT INITIAL.

    PERFORM document_infocus_save USING space.

    IF gs_variables-exit_after_save EQ c_true.
      CLEAR gs_variables-exit_after_save.
      SET SCREEN 0.
      LEAVE SCREEN.
    ELSEIF gs_variables-exit_after_save EQ 'C'.
      CLEAR: gs_variables-exit_after_save.
      PERFORM document_data_initialize USING c_true.
      gs_variables-document_mode = c_mode_display.
    ENDIF.
  ELSE.
    PERFORM changes_confirm   CHANGING lv_answer.
    IF lv_answer EQ 'A'.
      IF ok_code = c_fcode-save.
        PERFORM document_infocus_save USING space.
        PERFORM document_data_initialize USING c_true.
        CLEAR ok_code.
      ELSE.
        CLEAR ok_code.
        lv_continue = c_false.
        EXIT.
      ENDIF.
    ENDIF.
    PERFORM document_infocus_unlock USING gs_acdoc_infocus-acnum
                                          gs_acdoc_infocus-x-achdr-ajahr.
    PERFORM document_data_initialize USING c_true.
    gs_variables-document_mode = c_mode_display.
  ENDIF.

ENDFORM.                    " DOCUMENT_INFOCUS_CLEAR
*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_INFOCUS_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM document_infocus_save USING lv_set_infocus.

  DATA: lt_acdoc TYPE zt_fmac_doc,
        lv_subrc TYPE sysubrc.

  CLEAR: gs_variables-errors.
  gs_variables-initiator = c_log_initiator-save.
  PERFORM messages_initialize USING gs_variables-initiator
                                    c_log_subobject-save
                                    gs_acdoc_infocus-x-achdr.

  PERFORM acdoc_infocus_save CHANGING lv_subrc
                                      lt_acdoc.

  PERFORM messages_display USING gs_variables-initiator.
  CHECK lv_subrc IS INITIAL.
  LOOP AT lt_acdoc INTO gs_acdoc_infocus.
    PERFORM worklist_update USING gs_acdoc_infocus.
  ENDLOOP.
  IF lv_set_infocus IS NOT INITIAL.
    PERFORM document_infocus_set USING gs_acdoc_infocus-x-achdr-acnum
                              CHANGING lv_subrc.
  ENDIF.

ENDFORM.                    " DOCUMENT_INFOCUS_SAVE
*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_INFOCUS_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM document_infocus_set  USING VALUE(lv_acnum) TYPE zfmacnum
                          CHANGING    lv_subrc.

  DATA: lv_activity(2) TYPE c,
**** ESP6 Task #30035 - Global Text Engine Integration
*        lv_txtgr  TYPE txtgr.
        lv_txtgr       TYPE /agri/gtxtgr.
****

  PERFORM document_data_initialize USING c_true.

  CHECK lv_acnum IS NOT INITIAL.

  PERFORM document_infocus_read USING lv_acnum.
  CHECK gs_acdoc_infocus IS NOT INITIAL.

  gs_variables-document_mode = gs_variables-overview_mode.

  IF gs_variables-overview_mode NE c_mode_display.
    MOVE c_authorization_activity-change TO lv_activity.
  ELSEIF gs_variables-overview_mode EQ c_mode_display.
    MOVE c_authorization_activity-display TO lv_activity.
  ENDIF.

  PERFORM authority_check USING gs_acdoc_infocus-x-achdr
                                lv_activity
                                c_msg_type-info
                       CHANGING lv_subrc.
  IF lv_subrc NE 0.
    IF lv_activity EQ c_authorization_activity-change.
      gs_variables-overview_mode = gs_variables-document_mode
                                 = c_mode_display.
    ELSE.
      gs_variables-errors = c_true.
      PERFORM document_data_initialize USING c_true.
      EXIT.
    ENDIF.
  ENDIF.

  IF gs_variables-document_mode = c_mode_change.
    PERFORM document_infocus_lock USING lv_acnum
                               CHANGING lv_subrc.
    IF lv_subrc NE 0.
      gs_variables-document_mode =
      gs_variables-overview_mode = c_mode_display.
    ENDIF.
  ENDIF.

  PERFORM acdoc_type_control_read USING gs_acdoc_infocus-x-achdr-actyp
                              CHANGING lv_subrc.
  IF lv_subrc IS NOT INITIAL.
    CLEAR: gs_variables-document_mode,
           gs_acdoc_infocus, zsc_fmachdr.
    EXIT.
  ENDIF.

  PERFORM tabstrip_build.
  IF gs_acdoc_infocus-x-achdr-txtgr IS NOT INITIAL.
    lv_txtgr = gs_acdoc_infocus-x-achdr-txtgr.
  ELSE.
    lv_txtgr = gs_fmactyp-txtgr.
  ENDIF.

  object_refresh_all.
  object_publish c_object-bor gs_acdoc_infocus-acnum.

  PERFORM text_maintain USING lv_txtgr
                              c_object-text_object
                     CHANGING gs_variables-data_changed.

  PERFORM notes_refresh USING gs_acdoc_infocus-x-achdr-acnum.

  gs_variables-refresh_items_grid = c_true.

ENDFORM.                    " DOCUMENT_INFOCUS_SET
*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_INFOCUS_LOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM document_infocus_lock  USING lv_acnum TYPE zfmacnum
                         CHANGING lv_subrc.

  DATA: lv_msgv1 LIKE sy-msgv1,
        lv_msgli TYPE sy-msgli.

  CHECK NOT lv_acnum IS INITIAL.

  CALL FUNCTION 'ENQUEUE_EZ_FMAC'
    EXPORTING
*     MODE_/AGRI/GLachdr = 'X'
*     MANDT          = SY-MANDT
      acnum          = lv_acnum
*     X_acnum        = ' '
*     _SCOPE         = '2'
*     _WAIT          = ' '
*     _COLLECT       = ' '
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
    lv_subrc = sy-subrc.
    lv_msgv1 = sy-msgv1.
****Document &1 is locked by User &2.
    MESSAGE i005(/agri/glac) WITH lv_acnum lv_msgv1 INTO lv_msgli.
    sy-msgli = lv_msgli.
    message_simple space.
  ENDIF.

ENDFORM.                    " DOCUMENT_INFOCUS_LOCK
*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_INFOCUS_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM document_infocus_read  USING lv_acnum.

  DATA: lt_acnum  TYPE zt_fmacnum,
        lt_ac_doc TYPE zt_fmac_doc.

  APPEND lv_acnum TO lt_acnum.

  CALL FUNCTION 'ZFMAC_VIEW'
    EXPORTING
      it_acnum       = lt_acnum
    IMPORTING
      et_acdoc       = lt_ac_doc
    EXCEPTIONS
      no_data_exists = 1
      OTHERS         = 2.
  IF sy-subrc EQ 0.
    READ TABLE lt_ac_doc INTO gs_acdoc_infocus INDEX 1.
  ENDIF.

ENDFORM.                    " DOCUMENT_INFOCUS_READ
*&---------------------------------------------------------------------*
*&      Form  DROPDOWN_VALUES_FILL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM dropdown_values_fill .


ENDFORM.                    " DROPDOWN_VALUES_FILL
*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_INFOCUS_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM document_infocus_prepare .

  DATA: lv_error,
        lv_subrc     TYPE sy-subrc,
        lv_subrc_tmp TYPE sy-subrc.

  CHECK gs_variables-document_mode EQ c_mode_create.

  gs_acdoc_infocus-updkz = c_updkz_new.
  IF gs_acdoc_infocus-x-achdr-acnum IS INITIAL.
    gs_acdoc_infocus-acnum = TEXT-046.
    gs_acdoc_infocus-x-achdr-acnum = TEXT-046.
  ELSE.
    gs_acdoc_infocus-acnum = gs_acdoc_infocus-x-achdr-acnum.
  ENDIF.

*--Call Authority Check
  PERFORM authority_check USING gs_acdoc_infocus-x-achdr
                                c_authorization_activity-create
                                c_msg_type-info
                       CHANGING lv_subrc.
  IF lv_subrc NE 0.
    CLEAR: gs_variables-document_mode,
           gs_acdoc_infocus, zsc_fmachdr.
    gs_variables-errors = c_true.
    EXIT.
  ENDIF.

****Crop Type
  CLEAR gs_variables-actyp_in_focus.
  PERFORM acdoc_type_control_read USING gs_acdoc_infocus-x-achdr-actyp
                              CHANGING lv_subrc.
  IF lv_subrc IS NOT INITIAL.
    CLEAR: gs_variables-document_mode,
           gs_acdoc_infocus, zsc_fmachdr.
    EXIT.
  ENDIF.
  MOVE-CORRESPONDING gs_fmactyp TO gs_acdoc_infocus-x-achdr.

  IF ref_text IS NOT INITIAL.
    CALL METHOD ref_text->init.
    FREE ref_text.
  ENDIF.

  PERFORM text_maintain USING gs_fmactyp-txtgr
                              c_object-text_object
                     CHANGING gs_variables-data_changed.

  PERFORM notes_refresh USING gs_acdoc_infocus-x-achdr-acnum.
  CLEAR: ts_items-activetab.

ENDFORM.                    " DOCUMENT_INFOCUS_PREPARE
*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM document_check  USING    lv_before_save
                     CHANGING lv_subrc TYPE sy-subrc.
ENDFORM.                    " DOCUMENT_CHECK
*&---------------------------------------------------------------------*
*&      Form  DATA_UPDATE_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM data_update_all  CHANGING lt_data TYPE STANDARD TABLE
                               lv_subrc TYPE sy-subrc.
ENDFORM.                    " DATA_UPDATE_ALL
*&---------------------------------------------------------------------*
*&      Form  DESCRIPTIONS_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM descriptions_display .

  DATA: lv_subrc  TYPE sy-subrc,
        lwa_dd07v TYPE dd07v.

  STATICS: lt_dd07v TYPE STANDARD TABLE OF dd07v.

  CASE sy-dynnr.
    WHEN c_screen-quick_info.
      IF gs_fmactyp IS INITIAL AND
         gs_acdoc_infocus-x-achdr-actyp IS NOT INITIAL.
        PERFORM acdoc_type_control_read USING gs_acdoc_infocus-x-achdr-actyp
                                    CHANGING lv_subrc.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " DESCRIPTIONS_DISPLAY
