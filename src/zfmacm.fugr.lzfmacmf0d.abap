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
         zsc_fmachdr,
         gs_variables-data_changed,
         gs_variables-manual_changes,
         gs_variables-errors,
         gs_variables-copy.

  REFRESH: gt_achdr,
*-- BOC T_T.KONNO 04.07.21
           gt_glflcma,
*-- EOC T_T.KONNO 04.07.21
           gt_fmacitm_fcat,
           gt_fmacvlc_fcat.

  IF NOT lv_refresh_messages IS INITIAL.
    messages_init.
  ENDIF.

  IF ref_text IS NOT INITIAL.
    CALL METHOD ref_text->init.
    FREE ref_text.
  ENDIF.

  object_refresh_all.
  gs_variables-refresh_items_grid = c_true.
  gs_variables-refresh_vlc_grid = c_true.

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
    PERFORM document_infocus_unlock USING gs_acdoc_infocus-x-achdr-acnum
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

*...BOC-T_T.KONNO
  APPEND gs_acdoc_infocus TO lt_acdoc.
*...EOC-T_T.KONNO

  PERFORM acdoc_infocus_save CHANGING lv_subrc
                                      lt_acdoc.
  IF lv_subrc EQ 0.
    PERFORM messages_display USING gs_variables-initiator.
    PERFORM worklist_update USING gs_acdoc_infocus.
    IF lv_set_infocus IS NOT INITIAL.
      PERFORM document_infocus_set USING gs_acdoc_infocus-x-achdr-acnum
                                         gs_acdoc_infocus-x-achdr-ajahr
                                CHANGING lv_subrc.
    ENDIF.
  ENDIF.

ENDFORM.                    " DOCUMENT_INFOCUS_SAVE
*&---------------------------------------------------------------------*
*&      Form  SET_DOCUMENT_MODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_document_mode USING lv_mode.

  gs_variables-document_mode = lv_mode.

ENDFORM.                    " SET_DOCUMENT_MODE
*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_INFOCUS_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM document_infocus_set  USING VALUE(lv_acnum) TYPE zfmacnum
                                 VALUE(lv_ajahr) TYPE ajahr
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
                                        lv_ajahr
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
                                  lv_ajahr TYPE ajahr
                         CHANGING lv_subrc.

  DATA: lv_msgv1 LIKE sy-msgv1,
        lv_msgli TYPE sy-msgli.

  CHECK NOT lv_acnum IS INITIAL
    AND NOT lv_ajahr IS INITIAL.

  CALL FUNCTION 'ENQUEUE_EZ_FMAC'
    EXPORTING
*     MODE_ZFMACHDR        = 'X'
      mandt = sy-mandt
      acnum = lv_acnum
      ajahr = lv_ajahr
*     X_ACNUM              = ' '
*     X_AJAHR              = ' '
*     _SCOPE               = '2'
*     _WAIT = ' '
*     _COLLECT             = ' '
* EXCEPTIONS
*     FOREIGN_LOCK         = 1
*     SYSTEM_FAILURE       = 2
*     OTHERS               = 3
    .
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
    IF sy-subrc EQ 0.
      gs_acdoc_infocus-acnum = gs_acdoc_infocus-x-achdr-acnum.
*...BOC-T_T.KONNO
      p_datab = gs_acdoc_infocus-x-achdr-datab.
      p_datbi = gs_acdoc_infocus-x-achdr-datbi.
      LOOP AT gs_acdoc_infocus-x-acitm INTO DATA(lwa_acitm).
        INSERT INITIAL LINE INTO TABLE so_werks
          ASSIGNING FIELD-SYMBOL(<lwa_werks>).
        IF sy-subrc EQ 0.
          <lwa_werks> = 'IEQ'.
          <lwa_werks>-low = lwa_acitm-iwerk.
        ENDIF.
      ENDLOOP.
*...EOC-T_T.KONNO
    ENDIF.
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
  MOVE-CORRESPONDING gs_tfmactyp TO gs_acdoc_infocus-x-achdr.

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

*...BOC-T_T.KONNO
*  DATA: lwa_acitma        TYPE zsc_fmacitm_fcat,
*        lwa_selected_rows TYPE lvc_s_row.
*
*  IF gs_variables-document_mode EQ c_mode_create.
*    IF ref_grid_items IS NOT INITIAL.
*      CALL METHOD ref_grid_items->get_selected_rows
*        IMPORTING
*          et_index_rows = gt_selected_rows.
*      IF NOT gt_selected_rows IS INITIAL.
*        LOOP AT gt_selected_rows INTO lwa_selected_rows.
*          READ TABLE gt_fmacitm_fcat INTO lwa_acitma INDEX lwa_selected_rows-index.
*          IF sy-subrc EQ 0.
*            IF lwa_acitma IS INITIAL.
**.............Atenção: linhas vazias não devem ser selecionadas!
*              MESSAGE ID 'ZFMAC' TYPE c_msg_type-error NUMBER '076'.
*              message_simple space.
*              MOVE 4 TO lv_subrc.
*              PERFORM messages_display USING gs_variables-initiator.
*            ENDIF.
*          ENDIF.
*        ENDLOOP.
*      ELSEIF gt_selected_rows IS INITIAL.
*****Please select a row
*        MESSAGE ID '/AGRI/GLOBAL' TYPE 'E' NUMBER '321' INTO sy-msgli.
*        message_simple space.
*        MOVE 4 TO lv_subrc.
*        PERFORM messages_display USING gs_variables-initiator.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*...EOC-T_T.KONNO

*--- Update total data on header.
  PERFORM update_total.
*  PERFORM fcode_reno.

ENDFORM.                    " DOCUMENT_CHECK
*...BOC-T_T.KONNO
**&---------------------------------------------------------------------*
**&      Form  DATA_UPDATE_ALL
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*FORM data_update_all  CHANGING lt_data TYPE STANDARD TABLE
*                               lv_subrc TYPE sy-subrc.
*  PERFORM quantities_totalize.
*ENDFORM.                    " DATA_UPDATE_ALL
*...EOC-T_T.KONNO
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
