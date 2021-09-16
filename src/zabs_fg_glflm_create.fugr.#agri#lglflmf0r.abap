*----------------------------------------------------------------------*
***INCLUDE /AGRI/LGLFLMF0R .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  REFRESH_OWNERS_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM refresh_owners_grid .

  CLEAR: gs_variables-refresh_owners_grid.
  CALL METHOD ref_owners_grid->refresh_table_display
    EXCEPTIONS
      finished = 1
      OTHERS   = 2.
  IF sy-subrc <> 0.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " REFRESH_OWNERS_GRID

*&---------------------------------------------------------------------*
*&      Form  RELEASE_STATUS_DETERMINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM release_status_determine  USING  lv_user_status
                            CHANGING  lv_kfrst.

  DATA: lt_tj31 TYPE TABLE OF tj31,
        ls_tj31 TYPE tj31.

  SELECT * FROM tj31 INTO TABLE lt_tj31
            WHERE stsma EQ gs_fldoc_infocus-x-flhdr-stsma
              AND estat EQ lv_user_status.
  CHECK sy-subrc EQ 0.

  READ TABLE lt_tj31 INTO ls_tj31
       WITH KEY vrgng = c_status_transaction-blocked.
  IF sy-subrc EQ 0 AND ls_tj31-modkz CA '12'.
    lv_kfrst = 'A'.
  ENDIF.

  READ TABLE lt_tj31 INTO ls_tj31
       WITH KEY vrgng = c_status_transaction-released.
  IF sy-subrc EQ 0 AND ls_tj31-modkz CA '12'.
    lv_kfrst = 'R'.
  ENDIF.

ENDFORM.                    " RELEASE_STATUS_DETERMINE
*&---------------------------------------------------------------------*
*&      Form  release_status_allowed_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM release_status_allowed_check USING lwa_fphdr TYPE /agri/s_glflot
                                        lv_release_status
                               CHANGING lv_not_allowed.

  DATA: lv_business_transaction LIKE tj01-vrgng.

  CHECK NOT lwa_fphdr-stsma IS INITIAL.

  CASE lv_release_status.
    WHEN space OR 'R'.
      lv_business_transaction = c_status_transaction-released.
    WHEN 'A'.
      lv_business_transaction = c_status_transaction-blocked.
  ENDCASE.

  CALL FUNCTION '/AGRI/G_STATUS_PROCESS_CHECK'
    EXPORTING
      i_objnr              = lwa_fphdr-objnr
      i_vrgng              = lv_business_transaction
    EXCEPTIONS
      allowed_with_warning = 1
      not_allowed          = 2
      object_not_found     = 3
      OTHERS               = 4.
  IF sy-subrc <> 0.
    IF sy-subrc NE 1.
      lv_not_allowed = c_true.
    ENDIF.
    IF NOT sy-msgid IS INITIAL
    AND NOT sy-msgno IS INITIAL
    AND NOT sy-msgty IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
      message_simple space.
    ENDIF.
  ENDIF.

ENDFORM.                    "release_status_allowed_check
*&---------------------------------------------------------------------*
*&      Form  release_status_update
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM release_status_update .

  DATA: lv_subrc TYPE sysubrc,
        lv_answer.

  IF /agri/s_glflot-kfrst NE gs_fldoc_infocus-x-flhdr-kfrst.
    popup_to_confirm text-007 text-018 c_true lv_answer.
    IF lv_answer EQ '1'.

      PERFORM rel_status_check_n_update USING /agri/s_glflot-kfrst space
                                     CHANGING lv_subrc.
      IF lv_subrc EQ 0.
        gs_variables-data_changed = c_true.
      ENDIF.
    ELSE.
      /agri/s_glflot-kfrst = gs_fldoc_infocus-x-flhdr-kfrst.
    ENDIF.

  ENDIF.

ENDFORM.                    "release_status_update
*&---------------------------------------------------------------------*
*&      Form  rel_status_check_n_update
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM rel_status_check_n_update  USING    lv_kfrst
                                         lv_show_message
                                CHANGING lv_subrc.

  PERFORM release_status_check USING lv_kfrst lv_show_message
                            CHANGING lv_subrc.

  CHECK lv_subrc EQ 0.

  /agri/s_glflot-kfrst = lv_kfrst.
  gs_fldoc_infocus-x-flhdr-kfrst = lv_kfrst.
  IF gs_fldoc_infocus-x-flhdr-updkz NE c_updkz_new.
    gs_fldoc_infocus-x-flhdr-updkz = c_updkz_update.
  ENDIF.
  gs_variables-data_changed = c_true.

ENDFORM.                    "rel_status_check_n_update
*&---------------------------------------------------------------------*
*&      Form  release_status_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM release_status_check  USING    lv_kfrst
                                    lv_show_message
                           CHANGING lv_subrc.

  DATA: lv_not_allowed,
        lv_status_check,
        lwa_fphdr TYPE /agri/s_glflot.

  MOVE-CORRESPONDING gs_fldoc_infocus-x-flhdr TO lwa_fphdr.
  lwa_fphdr-kfrst = lv_kfrst.


  PERFORM authority_check USING lwa_fphdr
                                c_authorization_activity-release
                                c_msg_type-info
                       CHANGING lv_subrc.
  IF lv_subrc NE 0.
    lv_subrc = 4.
    EXIT.
  ENDIF.

  PERFORM release_status_allowed_check USING gs_fldoc_infocus-x-flhdr
                                             lv_kfrst
                                    CHANGING lv_not_allowed.
  IF lv_not_allowed EQ c_true.
    lv_subrc = 4.
    EXIT.
  ENDIF.

ENDFORM.                    "release_status_check
