*----------------------------------------------------------------------*
***INCLUDE /AGRI/LGLFLMF0U .
*----------------------------------------------------------------------*
**&---------------------------------------------------------------------*
**&      Form  UPDATE_FL_CLASS
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*FORM update_fl_class .
*
*  DATA: lv_clint  TYPE clint,
*        lwa_atgrp TYPE /agri/glagha.
*
*  FIELD-SYMBOLS: <lwa_flatv> TYPE /agri/s_glflatv.
*
*  gs_variables-refresh_attributes = c_true.
*  gs_fldoc_infocus-x-flhdr-class = /agri/s_glflot-class.
*  IF /agri/s_glflot-class IS INITIAL.
*    LOOP AT gs_fldoc_infocus-y-flatv ASSIGNING <lwa_flatv>.
*      <lwa_flatv>-updkz = c_updkz_delete.
*    ENDLOOP.
*    REFRESH: gs_fldoc_infocus-x-flatv, gt_athdr.
*    EXIT.
*  ENDIF.
*
*  IF /agri/s_glflot-class EQ 'ALL'.
*    LOOP AT gt_atgrp INTO lwa_atgrp
*                    WHERE class NE /agri/s_glflot-class.
*      PERFORM class_attributes_read USING c_agtyp-functional_location
*                                    lv_clint
*                                    lwa_atgrp-class
*                           CHANGING gt_athdr.
*    ENDLOOP.
*  ELSE.
*    LOOP AT gs_fldoc_infocus-y-flatv ASSIGNING <lwa_flatv>.
*      <lwa_flatv>-updkz = c_updkz_delete.
*    ENDLOOP.
*    REFRESH: gs_fldoc_infocus-x-flatv, gt_athdr.
*    PERFORM class_attributes_read USING c_agtyp-functional_location
*                                  lv_clint
*                                  /agri/s_glflot-class
*                         CHANGING gt_athdr.
*  ENDIF.
*
*ENDFORM.                    " UPDATE_FL_CLASS
*&---------------------------------------------------------------------*
*&      Form  USER_ADDITIONAL_DATA_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM user_additional_data_update .

  DATA: lv_modified,
        lv_valid,
        lwa_flhdr   TYPE /agri/s_glflot,
        lwa_xflhdr  TYPE /agri/s_glflot.

  CHECK NOT gs_variables-user_structure IS INITIAL AND
            gs_variables-overview_mode NE c_mode_display.

  lv_modified = ref_additional_data_grid->data_modified_check( ).

  IF lv_modified EQ c_true OR gs_variables-manual_changes EQ c_true.

    CALL METHOD ref_additional_data_grid->check_changed_data
      IMPORTING
        e_valid = lv_valid.

    IF lv_valid IS INITIAL.
      CLEAR ok_code.
      EXIT.
    ENDIF.

    MOVE-CORRESPONDING gs_fldoc_infocus-x-flhdr TO lwa_flhdr.
    PERFORM field_value_conversions USING '2'.
    MOVE-CORRESPONDING gs_fldoc_infocus-x-flhdr TO lwa_xflhdr.
    IF lwa_xflhdr NE lwa_flhdr.
      IF gs_fldoc_infocus-x-flhdr-updkz NE c_updkz_new.
        gs_fldoc_infocus-x-flhdr-updkz = c_updkz_update.
      ENDIF.
      gs_variables-data_changed = c_true.
    ENDIF.
    gs_variables-refresh_additional_data_grid = c_true.

  ENDIF.

ENDFORM.                    " USER_ADDITIONAL_DATA_UPDATE
*&---------------------------------------------------------------------*
*&      Form  USER_STATUS_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM user_status_update  USING    lv_tplnr    TYPE /agri/gltplnr_fl
                                                 VALUE(lv_user_status)
                                  lv_set_inactive
                                  lwa_outcome TYPE /agri/s_gacoutcome
                                  lv_collect_message
                         CHANGING ls_flhdr    TYPE /agri/s_glflot
                                  lv_subrc.

  DATA: lt_messages TYPE /agri/t_gprolog,
        lwa_message TYPE /agri/s_gprolog.
  IF lwa_outcome IS INITIAL.
    IF lv_user_status(1) = 'E'.
      ls_flhdr-ustat = lv_user_status.
    ELSE.
      SELECT SINGLE estat FROM tj30t INTO lv_user_status
                   WHERE stsma EQ ls_flhdr-stsma
                     AND spras EQ sy-langu
                     AND txt04 EQ lv_user_status.
      lv_subrc = sy-subrc.
    ENDIF.
    CHECK lv_subrc EQ 0 AND ls_flhdr-objnr IS NOT INITIAL.
  ELSE.
    CHECK ls_flhdr-objnr NE space.
  ENDIF.

  IF ref_status_handler IS INITIAL.
    CREATE OBJECT ref_status_handler.
  ENDIF.

  CALL FUNCTION '/AGRI/G_STATUS_CHANGE_EXTERNAL'
    EXPORTING
      i_object_number       = ls_flhdr-objnr
      i_user_status         = ls_flhdr-ustat
      i_set_inactive        = lv_set_inactive
*     I_UPDATE_CHANGE_DOCS  =
      i_objkey              = lv_tplnr
      is_outcome            = lwa_outcome
      iref_object           = ref_status_handler
    IMPORTING
      et_messages           = lt_messages
    EXCEPTIONS
      object_not_found      = 1
      status_inconsistent   = 2
      status_not_allowed    = 3
      outcome_cannot_be_set = 4
      OTHERS                = 5.
  lv_subrc = sy-subrc.
  IF lv_subrc <> 0.
    IF lv_collect_message EQ c_true.
      IF NOT sy-msgid IS INITIAL  AND NOT sy-msgty IS INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
        message_simple space.
      ENDIF.
    ENDIF.
  ENDIF.
  IF lt_messages IS NOT INITIAL.
    lv_subrc = 4.
    IF lv_collect_message EQ c_true.
      LOOP AT lt_messages INTO lwa_message.
        MESSAGE ID lwa_message-msgid TYPE lwa_message-msgty
        NUMBER lwa_message-msgno WITH lwa_message-msgv1
        lwa_message-msgv2 lwa_message-msgv3 lwa_message-msgv4
        INTO sy-msgli.
        message_simple space.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " USER_STATUS_UPDATE

*&---------------------------------------------------------------------*
*&      Form  USER_STATUS_TEXT_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM user_status_text_get  USING  lv_ustat
                                  lv_stsma
                        CHANGING  lv_ustat_txt.

  DATA: ls_tj30t LIKE tj30t.

  CHECK NOT lv_stsma IS INITIAL.

  ls_tj30t-stsma = lv_stsma.
  ls_tj30t-estat = lv_ustat.
  key_text_get 'TJ30T' 'ESTAT' ls_tj30t-estat
                  ls_tj30t lv_ustat_txt.

ENDFORM.                    " USER_STATUS_TEXT_GET
*&---------------------------------------------------------------------*
*& Form UPDATE_TERRAIN_LABEL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM update_terrain_label .

  DATA: ls_flos  TYPE /agri/s_glflos,
        lv_versn TYPE /agri/glversn.

  CLEAR: gs_variables-new_label.
  CHECK */agri/s_glflot-strno NE /agri/s_glflot-strno.

  LOOP AT gs_fldoc_infocus-x-flos INTO ls_flos.
    IF ls_flos-versn GT lv_versn.
      lv_versn = ls_flos-versn.
    ENDIF.
  ENDLOOP.

  CLEAR ls_flos.
  MOVE-CORRESPONDING /agri/s_glflot TO ls_flos.
  ls_flos-versn = lv_versn + 1.
  ls_flos-erdat = sy-datum.
  ls_flos-ernam = sy-uname.
  ls_flos-updkz = c_updkz_new.
  APPEND ls_flos TO gs_fldoc_infocus-x-flos.
  gs_fldoc_infocus-x-flhdr-strno = /agri/s_glflot-strno.
  gs_fldoc_infocus-x-flhdr-tplma = /agri/s_glflot-tplma.
  IF gs_fldoc_infocus-x-flhdr-updkz NE c_updkz_new.
    gs_fldoc_infocus-x-flhdr-updkz = c_updkz_update.
  ENDIF.

ENDFORM.
