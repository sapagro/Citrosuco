*----------------------------------------------------------------------*
***INCLUDE /AGRI/LFMACMNF0F .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FIELD_CATALOG_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM field_catalog_prepare USING lv_structure_name
                        CHANGING lt_fcat TYPE lvc_t_fcat.

  FIELD-SYMBOLS : <lwa_fcat>       LIKE LINE OF lt_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
*     I_BUFFER_ACTIVE        =
      i_structure_name       = lv_structure_name
*     I_CLIENT_NEVER_DISPLAY = 'X'
*     I_BYPASSING_BUFFER     =
*     I_INTERNAL_TABNAME     =
    CHANGING
      ct_fieldcat            = lt_fcat
    EXCEPTIONS ##FM_SUBRC_OK
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  CASE lv_structure_name.
****WorkList Header
    WHEN c_structure_name-work_list_header.

      READ TABLE lt_fcat ASSIGNING <lwa_fcat> WITH KEY fieldname = 'ACNUM'.
      IF <lwa_fcat> IS ASSIGNED.
        <lwa_fcat>-hotspot = c_true.
        <lwa_fcat>-key     = c_true.
      ENDIF.
    WHEN OTHERS.

  ENDCASE.

ENDFORM.                    " FIELD_CATALOG_PREPARE
*&---------------------------------------------------------------------*
*&      Form  fcode_/agri/max
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_/agri/max .

****Maximize Worklist(Full Screen)
  DATA : lv_extension TYPE i VALUE 2000.

  CHECK gs_variables-worklist_is_visible EQ c_true.

  CALL METHOD ref_worklist_container->set_extension
    EXPORTING
      extension  = lv_extension
    EXCEPTIONS
      cntl_error = 1
      OTHERS     = 2.

ENDFORM.                    "fcode_max
*&---------------------------------------------------------------------*
*&      Form  fcode_min
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_/agri/min .

  DATA : lv_extension TYPE i VALUE 300.
  CHECK gs_variables-worklist_is_visible EQ c_true.
  CALL METHOD ref_worklist_container->set_extension
    EXPORTING
      extension  = lv_extension
    EXCEPTIONS
      cntl_error = 1
      OTHERS     = 2.

ENDFORM.                    "fcode_min
*&---------------------------------------------------------------------*
*&      Form  fcode_dhn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_/agri/dhn .

****Display/Hide Navigator
  IF gs_variables-worklist_is_visible EQ c_true.
    CLEAR gs_variables-worklist_is_visible.
  ELSE.
    gs_variables-worklist_is_visible = c_true.
  ENDIF.

  CALL METHOD ref_worklist_container->set_visible
    EXPORTING
      visible           = gs_variables-worklist_is_visible
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    "fcode_dhn
*&---------------------------------------------------------------------*
*&      Form  FCODE_PROCESSING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_processing .

  DATA: lv_routine(30) TYPE c VALUE 'FCODE_'.

  IF ok_code IS NOT INITIAL.
    fcode = ok_code.
    CLEAR ok_code.

    IF fcode(2) EQ 'T\'.
      PERFORM fcode_processing_tabstrip.
    ELSE.
      CONCATENATE lv_routine fcode INTO lv_routine.
      PERFORM (lv_routine) IN PROGRAM (c_program_acm)
                           IF FOUND.
    ENDIF.
  ENDIF.

ENDFORM.                    " FCODE_PROCESSING
*&---------------------------------------------------------------------*
*&      Form  FCODE_PROCESSING_TABSTRIP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_processing_tabstrip .

  DATA: ls_tabstrip TYPE /agri/s_gtabstrip.

****Check Rules
  READ TABLE gt_tabstrip_fcodes INTO ls_tabstrip WITH KEY ts_fcode = fcode.
  CASE ls_tabstrip-local_fcode.
    WHEN c_fcode-tab_texts.
      CALL FUNCTION '/AGRI/G_WORD_PROCESSING_INIT'.
    WHEN OTHERS.
  ENDCASE.

  ts_items-activetab = fcode.

ENDFORM.                    " FCODE_PROCESSING_TABSTRIP
*&---------------------------------------------------------------------*
*&      Form  fcode_srch
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_srch.

  DATA: lt_search_hdr_more LIKE gt_search_header,
        lv_continue        TYPE c VALUE c_true.

  PERFORM document_infocus_clear CHANGING lv_continue.
  CHECK lv_continue EQ c_true.

  PERFORM worklist_search CHANGING lt_search_hdr_more.

ENDFORM.                    "fcode_srch
*&---------------------------------------------------------------------*
*&      Form  fcode_srch_more
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_srch_more.

  DATA: lt_search_hdr_more LIKE gt_search_header,
        lv_continue        TYPE c VALUE c_true.

  PERFORM document_infocus_clear CHANGING lv_continue.
  CHECK lv_continue EQ c_true.

  lt_search_hdr_more = gt_search_header.

  PERFORM worklist_search CHANGING lt_search_hdr_more.

ENDFORM.                    "fcode_srch_more
*&---------------------------------------------------------------------*
*&      Form  fcode_wlhc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_wlhc.

****Hotspot click - Worklist
  DATA: lv_answer,
        lv_subrc         TYPE sy-subrc,
        lv_dummy,
        lwa_selected_doc LIKE LINE OF gt_selected_docs.

  READ TABLE gt_selected_docs INTO lwa_selected_doc INDEX 1.
  CHECK sy-subrc EQ 0.

****Confirm Changes made to the document
  PERFORM changes_confirm CHANGING lv_answer.

  IF lv_answer EQ 'A'.

    IF ok_code EQ c_fcode-save.
****Save - Yes
      CLEAR ok_code.
****Save the infocus document
      PERFORM document_infocus_save USING space.

    ELSE.
****Save - Cancel
****Changes were not saved
      MESSAGE ID 'ZFMAC' TYPE c_msg_type-success NUMBER '007'
      INTO sy-msgli.
      message_simple space.
      PERFORM messages_display USING gs_variables-initiator.
      EXIT.
    ENDIF.
  ENDIF.

****Dequeue the document infocus
  PERFORM document_infocus_unlock USING gs_acdoc_infocus-acnum
                                        gs_acdoc_infocus-x-achdr-ajahr.
  PERFORM document_infocus_set USING    lwa_selected_doc-acnum
                               CHANGING lv_subrc.

ENDFORM.                    "fcode_wlhc
*&---------------------------------------------------------------------*
*&      Form  fcode_dich
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_dich.

  DATA: lv_answer,
        lv_subrc TYPE sy-subrc.

  IF gs_variables-overview_mode EQ c_mode_change.

    PERFORM changes_confirm CHANGING lv_answer.
    IF lv_answer NE 'A'.

      gs_variables-overview_mode = c_mode_display.

      IF gs_variables-document_mode NE c_mode_display.
        PERFORM document_infocus_unlock USING gs_acdoc_infocus-acnum
                                              gs_acdoc_infocus-x-achdr-ajahr.

        PERFORM document_infocus_set USING gs_acdoc_infocus-acnum
                                  CHANGING lv_subrc.

        gs_variables-document_mode = c_mode_display.
        gs_variables-overview_mode = c_mode_display.
      ENDIF.

    ELSEIF ok_code EQ c_fcode-save.

      CLEAR: lv_answer, ok_code.
      PERFORM document_infocus_save USING space.
      PERFORM document_infocus_unlock USING gs_acdoc_infocus-acnum
                                            gs_acdoc_infocus-x-achdr-ajahr.
      gs_variables-overview_mode = c_mode_display.
      gs_variables-document_mode = c_mode_display.
      PERFORM document_infocus_set USING gs_acdoc_infocus-x-achdr-acnum
                                CHANGING lv_subrc.

    ENDIF.
    gs_variables-refresh_worklist = c_true.
  ELSE.

    gs_variables-overview_mode = c_mode_change.
    PERFORM document_infocus_set USING    gs_acdoc_infocus-acnum
                                 CHANGING lv_subrc.

  ENDIF.

ENDFORM.                    "fcode_dich
*&---------------------------------------------------------------------*
*&      Form  fcode_save
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_save.

  IF gs_variables-mass_action IS NOT INITIAL.
*    PERFORM mass_save.
    gs_variables-refresh_attributes_mass_grid = c_true.
    EXIT.
  ENDIF.

  IF gs_variables-external_dialog IS INITIAL.
    PERFORM document_infocus_save USING c_true.
  ELSE.
    PERFORM fcode_save_dialog.
  ENDIF.

ENDFORM.                    "fcode_save
*&---------------------------------------------------------------------*
*&      Form  fcode_back
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_back.

  DATA: lv_continue.

  IF gs_variables-mass_action NE c_true.
    PERFORM document_infocus_clear USING lv_continue.
  ELSE.
*    PERFORM mass_changes_confirm.
  ENDIF.

ENDFORM.                    "fcode_back
*&---------------------------------------------------------------------*
*&      Form  fcode_crea
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_crea.
  PERFORM measurement_create.
ENDFORM.                    "fcode_crea
*&---------------------------------------------------------------------*
*&      Form  fcode_cont
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_cont.
  SET SCREEN 0.
  LEAVE SCREEN.
ENDFORM.                    "fcode_cont
*&---------------------------------------------------------------------*
*&      Form  fcode_cdoc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_cdoc.

  DATA: lt_objid  TYPE /agri/t_gcdobjid,
        lwa_objid TYPE /agri/s_gcdobjid.

  CHECK: NOT gs_acdoc_infocus-acnum IS INITIAL.

  lwa_objid-objid  = gs_acdoc_infocus-acnum.
  APPEND lwa_objid TO lt_objid.
  CALL FUNCTION '/AGRI/GCD_DISPLAY'
    EXPORTING
      i_object           = c_object-change_documents
*     I_TITLE            =
      i_html_view        = c_true
*     T_CDHDR            =
      t_objid            = lt_objid
*     IREF_GCD_PROCESS   =
    EXCEPTIONS
      no_documents_found = 1
      no_object_selected = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    "fcode_cdoc
*&---------------------------------------------------------------------*
*&      Form  fcode_seag
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_seag.

*  PERFORM mass_data_prepare.
  gs_variables-refresh_output_table = c_true.

ENDFORM.                    "fcode_seag
*&---------------------------------------------------------------------*
*&      Form  fcode_sncp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_sncp.

ENDFORM.                    "fcode_sncp
*&---------------------------------------------------------------------*
*&      Form  fcode_RFSH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_rfsh.

ENDFORM.                    "fcode_RFSH
*&---------------------------------------------------------------------*
*&      Form  fcode_eqtr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_eqtr.

ENDFORM.                    "fcode_eqtr
*&---------------------------------------------------------------------*
*&      Form  fcode_excludes_prepare
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_excludes_prepare  CHANGING lt_fcode_excludes TYPE ui_functions.

  CASE sy-dynnr.
****Screen 100
    WHEN c_screen-main_screen.
      IF gs_variables-overview_mode EQ c_mode_display
      OR NOT gs_acdoc_infocus IS INITIAL.
        APPEND c_fcode-create TO lt_fcode_excludes.
      ENDIF.

      IF gs_acdoc_infocus IS INITIAL.
        APPEND c_fcode-back            TO lt_fcode_excludes.
        APPEND c_fcode-cancel          TO lt_fcode_excludes.
        APPEND c_fcode-save            TO lt_fcode_excludes.
        APPEND c_fcode-delete          TO lt_fcode_excludes.
        APPEND c_fcode-where_used_list TO lt_fcode_excludes.
      ENDIF.

      IF gs_variables-document_mode EQ c_mode_display.
        APPEND c_fcode-save   TO lt_fcode_excludes.
        APPEND c_fcode-delete TO lt_fcode_excludes.
      ENDIF.

      IF gs_variables-document_mode EQ c_mode_create.
        APPEND c_fcode-delete          TO lt_fcode_excludes.
        APPEND c_fcode-where_used_list TO lt_fcode_excludes.
      ENDIF.

****External Call
      IF gs_variables-display_only EQ c_true.
        APPEND c_fcode-display_change_toggle TO lt_fcode_excludes.
      ENDIF.
      IF gs_variables-external EQ c_true.
        APPEND c_fcode-display_change_toggle TO lt_fcode_excludes.
      ENDIF.
****User Functions
      APPEND c_fcode-where_used_list TO lt_fcode_excludes.
      APPEND c_fcode-user_function1 TO lt_fcode_excludes.
      APPEND c_fcode-user_function2 TO lt_fcode_excludes.
      APPEND c_fcode-user_function3 TO lt_fcode_excludes.
      APPEND c_fcode-user_function4 TO lt_fcode_excludes.
      APPEND c_fcode-user_function5 TO lt_fcode_excludes.

    WHEN c_screen-create_mass_dialog.

    WHEN c_screen-create_dialog.
      APPEND c_fcode-refresh_objects TO lt_fcode_excludes.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    "fcode_excludes_prepare
*&---------------------------------------------------------------------*
*&      Form  FCODE_MASS_DOC_DELETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_mass_doc_delete .
ENDFORM.                    " FCODE_MASS_DOC_DELETE
*&---------------------------------------------------------------------*
*&      Form  fcode_dele
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_dele.
ENDFORM.                    "fcode_dele

*&---------------------------------------------------------------------*
*&      Form  fcode_pick
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_pick.
ENDFORM.                    "fcode_pick
*&---------------------------------------------------------------------*
*&      Form  FCODE_SAVE_DIALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_save_dialog .
ENDFORM.                    " FCODE_SAVE_DIALOG
*&---------------------------------------------------------------------*
*&      Form  fcode_dcvl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_dcvl.
ENDFORM.                    "fcode_dcvl
