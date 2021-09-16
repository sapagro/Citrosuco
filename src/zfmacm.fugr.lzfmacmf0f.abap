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
    WHEN c_structure_name-items.
      LOOP AT lt_fcat ASSIGNING <lwa_fcat>.
        CASE <lwa_fcat>-fieldname.
          WHEN 'TPLNR_FL'.
            <lwa_fcat>-edit = c_true.
            <lwa_fcat>-f4availabl = c_true.
          WHEN 'ACPOS' OR 'CMNUM' OR 'SEASON' OR 'DATAB' OR 'DATBI' OR 'CLASS'
            OR 'ASTAT' OR 'AAREA' OR 'MSEHI'  OR 'MSEHI' OR 'ADQPL' OR 'UNQPL'
            OR 'ADVLC' OR 'ACETM' OR 'IWERK'.
          WHEN OTHERS.
            <lwa_fcat>-tech = c_true.
        ENDCASE.
      ENDLOOP.

    WHEN c_structure_name-vlcl.
      LOOP AT lt_fcat ASSIGNING <lwa_fcat>.
        CASE <lwa_fcat>-fieldname.
          WHEN 'MATNR' OR 'MAKTX' OR 'VORNR' OR 'POSNR' OR 'ACVCL' OR 'TPLNR_FL'
                OR 'ACUQB' OR 'ACQTB' OR 'ACARX' OR 'UNARX'.
          WHEN 'ACIDT' OR 'ACDIS' OR 'ACFCB' OR 'ACREN'
            OR 'UNREN' OR 'ACPEX'.
            <lwa_fcat>-edit = c_true.
            <lwa_fcat>-f4availabl = c_true.
          WHEN OTHERS.
            <lwa_fcat>-tech = c_true.
        ENDCASE.

      ENDLOOP.
    WHEN OTHERS.
      LOOP AT lt_fcat ASSIGNING <lwa_fcat>.
        CASE <lwa_fcat>-fieldname.
          WHEN 'FIELDNAME'.
            <lwa_fcat>-no_out = c_true.
          WHEN 'FIELDVAL'.
            <lwa_fcat>-edit = c_true.
            <lwa_fcat>-f4availabl = c_true.
            <lwa_fcat>-outputlen = 10.
****
            <lwa_fcat>-lowercase = c_true.
          WHEN 'FIELDDSCR'.
            <lwa_fcat>-outputlen = 40.
          WHEN 'FIELDTXT'.
            <lwa_fcat>-outputlen = 15.

          WHEN 'FLDTY' OR 'VTEXT' OR 'NAME1'
            OR 'BUTXT' OR 'DESCR' OR 'CMNUM'
            OR 'ROWCOLOR' OR 'FIELDTYP' OR 'OUTPUTLEN' OR 'CONVEXIT' OR 'TABNAME'.
            <lwa_fcat>-no_out = <lwa_fcat>-tech = c_true.
        ENDCASE.
      ENDLOOP.
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
        lv_subrc          TYPE sy-subrc,
        lwa_search_header TYPE zsc_fmachdr,
        lv_dummy,
        lwa_selected_doc  LIKE LINE OF gt_selected_docs.

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

**Dequeue the document infocus
  IF gs_acdoc_infocus-x-achdr-acnum IS INITIAL.
    READ TABLE gt_search_header INTO lwa_search_header INDEX 1.
    MOVE lwa_search_header-acnum TO  gs_acdoc_infocus-x-achdr-acnum.
    MOVE lwa_search_header-ajahr TO  gs_acdoc_infocus-x-achdr-ajahr.
  ENDIF.
  PERFORM document_infocus_unlock USING gs_acdoc_infocus-x-achdr-acnum
                                          gs_acdoc_infocus-x-achdr-ajahr.
  PERFORM document_infocus_set USING    lwa_selected_doc-acnum
                                        gs_acdoc_infocus-x-achdr-ajahr
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
        PERFORM document_infocus_unlock USING gs_acdoc_infocus-x-achdr-acnum
                                          gs_acdoc_infocus-x-achdr-ajahr.

        PERFORM document_infocus_set USING gs_acdoc_infocus-acnum
                                           gs_acdoc_infocus-x-achdr-ajahr
                                  CHANGING lv_subrc.

        gs_variables-document_mode = c_mode_display.
        gs_variables-overview_mode = c_mode_display.
      ENDIF.

    ELSEIF ok_code EQ c_fcode-save.

      CLEAR: lv_answer, ok_code.
      PERFORM document_infocus_save USING space.
      PERFORM document_infocus_unlock USING gs_acdoc_infocus-x-achdr-acnum
                                          gs_acdoc_infocus-x-achdr-ajahr.
      gs_variables-overview_mode = c_mode_display.
      gs_variables-document_mode = c_mode_display.
      PERFORM document_infocus_set USING gs_acdoc_infocus-x-achdr-acnum
                                         gs_acdoc_infocus-x-achdr-ajahr
                                CHANGING lv_subrc.
    ENDIF.
    gs_variables-refresh_worklist = c_true.
  ELSE.
    gs_variables-overview_mode = c_mode_change.
    PERFORM document_infocus_unlock USING gs_acdoc_infocus-x-achdr-acnum
                                            gs_acdoc_infocus-x-achdr-ajahr.
    PERFORM document_infocus_set USING    gs_acdoc_infocus-x-achdr-acnum
                                          gs_acdoc_infocus-x-achdr-ajahr
                                 CHANGING lv_subrc.
  ENDIF.

ENDFORM.                    "fcode_dich
*&---------------------------------------------------------------------*
*&      Form  fcode_save
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_save.
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
  ENDIF.

ENDFORM.                    "fcode_back
*&---------------------------------------------------------------------*
*&      Form  fcode_crea
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_crea.

  inputparameter_refresh_all.
  CALL SELECTION-SCREEN 0010 STARTING AT 5 5.

ENDFORM.                    "fcode_crea
***&---------------------------------------------------------------------*
***&      Form  fcode_cont
***&---------------------------------------------------------------------*
***       text
***----------------------------------------------------------------------*
**FORM fcode_cont.
**  SET SCREEN 0.
**  LEAVE SCREEN.
**ENDFORM.                    "fcode_cont
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
        APPEND c_fcode-copy            TO lt_fcode_excludes.
        APPEND c_fcode-reno            TO lt_fcode_excludes.
        APPEND c_fcode-save            TO lt_fcode_excludes.
        APPEND c_fcode-delete          TO lt_fcode_excludes.
        APPEND c_fcode-where_used_list TO lt_fcode_excludes.
      ENDIF.

      IF gs_variables-document_mode EQ c_mode_display.
        APPEND c_fcode-save   TO lt_fcode_excludes.
        APPEND c_fcode-delete TO lt_fcode_excludes.
        APPEND c_fcode-copy TO lt_fcode_excludes.
        APPEND c_fcode-reno TO lt_fcode_excludes.
      ENDIF.

      IF gs_variables-document_mode EQ c_mode_create.
        APPEND c_fcode-delete          TO lt_fcode_excludes.
        APPEND c_fcode-copy            TO lt_fcode_excludes.
        APPEND c_fcode-reno            TO lt_fcode_excludes.
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

  DATA: lv_answer   TYPE c,
        lv_view     TYPE i,
        lv_subrc    TYPE sy-subrc,
        lv_text(70) TYPE c,
        lv_acnum    TYPE zfmacnum.

  IF zsc_fmachdr-statu = space.

    lv_text = TEXT-015.
    lv_acnum = gs_acdoc_infocus-acnum.
    REPLACE '&1' WITH lv_acnum INTO lv_text.
    popup_to_confirm TEXT-010 lv_text c_true lv_answer.

    CHECK lv_answer EQ '1'.

    gs_variables-initiator =  c_log_initiator-check.
    PERFORM messages_initialize USING  gs_variables-initiator
                                       c_log_subobject-check
                                       gs_acdoc_infocus-x-achdr.

*--Call Authority Check
    PERFORM authority_check
                      USING gs_acdoc_infocus-x-achdr
                            c_authorization_activity-delete
                            c_msg_type-error
                   CHANGING lv_subrc.
    IF lv_subrc NE 0.
      PERFORM messages_display USING gs_variables-initiator.
      EXIT.
    ENDIF.

    CALL FUNCTION 'ZFMAC_DELETE'
      CHANGING
        cs_acdoc = gs_acdoc_infocus.

    lv_view = ref_worklist->view_in_focus_get( ).

    IF lv_view EQ '1'.
      DELETE gt_worklist_header WHERE acnum = gs_acdoc_infocus-acnum.
    ELSEIF lv_view EQ '2'.
      DELETE gt_search_header WHERE acnum = gs_acdoc_infocus-acnum.
    ENDIF.

    CLEAR: gs_variables-document_mode, ok_code.
    gs_variables-refresh_worklist = c_true.
    PERFORM messages_display USING gs_variables-initiator.
    PERFORM document_data_initialize USING c_true.

  ELSE.
    MESSAGE ID 'ZFMAC' TYPE c_msg_type-error NUMBER '071'
             WITH zsc_fmachdr-acnum INTO sy-msgli.
    message_simple space.
    PERFORM messages_display USING gs_variables-initiator.
  ENDIF.

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

FORM fcode_crar.

  DATA: lt_rows          TYPE lvc_t_row,
        lwa_row          TYPE lvc_s_row,
        lv_answer        TYPE c,
        lwa_acitm_layout LIKE LINE OF gt_fmacitm_fcat.

  DATA: lwa_acitm       TYPE zsc_fmacitm,
        lwa_item_layout LIKE LINE OF gt_fmacitm_fcat,
        lwa_mod_row     TYPE lvc_s_modi,
        lv_typ          TYPE char2 VALUE '02',
        lv_tabix        TYPE sy-tabix,
        lv_modified,
        lv_subrc        TYPE int4,
        lv_valid.

  FIELD-SYMBOLS: <lwa_acitm>       TYPE zsc_fmacitm,
                 <lwa_item_layout> TYPE zsc_fmacitm_fcat.

  CALL METHOD ref_grid_items->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.

*...BOC-T_T.KONNO
*  CHECK lt_rows IS NOT INITIAL.
*...EOC-T_T.KONNO

  CLEAR: gs_acdoc_infocus.

* Create header.
  MOVE-CORRESPONDING zsc_fmachdr TO gs_acdoc_infocus-x-achdr.
  gs_acdoc_infocus-acnum = TEXT-004.
  gs_acdoc_infocus-x-achdr-updkz = gs_acdoc_infocus-updkz = c_updkz_new.

  gs_variables-refresh_items_grid = c_true.

*...BOC-T_T.KONNO
*  LOOP AT lt_rows INTO lwa_row.
*    lv_tabix = sy-tabix.
*    READ TABLE gt_fmacitm_fcat INTO lwa_item_layout INDEX lwa_row-index.
*    CHECK lwa_item_layout IS NOT INITIAL.
*
*    MOVE-CORRESPONDING lwa_item_layout TO lwa_acitm.
*    READ TABLE gs_acdoc_infocus-x-acitm ASSIGNING <lwa_acitm>
*                                        WITH KEY acnum = lwa_item_layout-acnum
*                                                 acpos = lwa_item_layout-acpos.
*    IF sy-subrc EQ 0.
*      IF lwa_acitm NE <lwa_acitm>.
*        gs_variables-document_changed = c_true.
*        IF <lwa_acitm>-updkz NE c_updkz_new.
*          MOVE lwa_acitm TO <lwa_acitm>.
*          <lwa_acitm>-updkz = c_updkz_update.
*        ELSE.
*          MOVE lwa_acitm TO <lwa_acitm>.
*          <lwa_acitm>-updkz = c_updkz_new.
*        ENDIF.
*      ENDIF.
*    ELSE.
*      gs_variables-refresh_items_grid = c_true.
*      gs_variables-document_changed = c_true.
*      lwa_item_layout-acnum = gs_acdoc_infocus-acnum.
*      lwa_item_layout-updkz = c_updkz_new.
*      MOVE-CORRESPONDING lwa_item_layout TO lwa_acitm.
*      APPEND lwa_acitm TO gs_acdoc_infocus-x-acitm.
*    ENDIF.
*    DELETE gt_items_modi INDEX lv_tabix.
*  ENDLOOP.

  LOOP AT gt_fmacitm_fcat INTO lwa_item_layout.
    lv_tabix = sy-tabix.
*...BOC-T_T.KONNO
    IF lwa_item_layout IS INITIAL.
      DELETE gt_fmacitm_fcat INDEX lv_tabix.
      CONTINUE.
    ENDIF.
*...EOC-T_T.KONNO
    MOVE-CORRESPONDING lwa_item_layout TO lwa_acitm.
    READ TABLE gs_acdoc_infocus-x-acitm ASSIGNING <lwa_acitm>
      WITH KEY acnum = lwa_item_layout-acnum
               acpos = lwa_item_layout-acpos.
    IF sy-subrc EQ 0.
      IF lwa_acitm NE <lwa_acitm>.
        gs_variables-document_changed = c_true.
        IF <lwa_acitm>-updkz NE c_updkz_new.
          MOVE lwa_acitm TO <lwa_acitm>.
          <lwa_acitm>-updkz = c_updkz_update.
        ELSE.
          MOVE lwa_acitm TO <lwa_acitm>.
          <lwa_acitm>-updkz = c_updkz_new.
        ENDIF.
      ENDIF.
    ELSE.
      gs_variables-refresh_items_grid = c_true.
      gs_variables-document_changed = c_true.
      lwa_item_layout-acnum = gs_acdoc_infocus-acnum.
      lwa_item_layout-updkz = c_updkz_new.
      MOVE-CORRESPONDING lwa_item_layout TO lwa_acitm.
      APPEND lwa_acitm TO gs_acdoc_infocus-x-acitm.
    ENDIF.
    DELETE gt_items_modi INDEX lv_tabix.
  ENDLOOP.
*...EOC-T_T.KONNO

ENDFORM.

FORM fcode_copy.

  DATA : lv_acnum  TYPE zfmacnum,
         lv_answer TYPE c,
         lv_subrc  TYPE sy-subrc,
         lv_dummy.

  FIELD-SYMBOLS :
    <lwa_acitm> TYPE  zsc_fmacitm,
    <lwa_acvlc> TYPE  zsc_fmacvlcl.

  PERFORM changes_confirm CHANGING lv_answer.
  IF lv_answer NE 'A'.
    PERFORM document_infocus_unlock USING gs_acdoc_infocus-x-achdr-acnum
                                           gs_acdoc_infocus-x-achdr-ajahr.
  ELSEIF lv_answer EQ 'A'.
    IF ok_code EQ c_fcode-save.
      CLEAR: lv_answer, ok_code, gs_variables-errors.
      PERFORM document_infocus_save USING c_true.
      CHECK gs_variables-errors IS INITIAL.
    ELSE.
      EXIT.
    ENDIF.
  ENDIF.

  lv_acnum = gs_acdoc_infocus-acnum.
  gs_variables-copy = c_true.
  gs_variables-document_mode = c_mode_create.
  gs_acdoc_infocus-x-achdr-acnum_ref = gs_acdoc_infocus-x-achdr-acnum.
  zsc_fmachdr-acnum_ref = gs_acdoc_infocus-x-achdr-acnum.

  CLEAR: gs_acdoc_infocus-y,
         gs_acdoc_infocus-x-achdr-ernam,
         gs_acdoc_infocus-x-achdr-erdat,
         gs_acdoc_infocus-x-achdr-erzet,
         gs_acdoc_infocus-x-achdr-aenam,
         gs_acdoc_infocus-x-achdr-aedat,
         gs_acdoc_infocus-x-achdr-aezet.

*** Number range object
  CLEAR: gs_acdoc_infocus-acnum, gs_acdoc_infocus-x-achdr-acnum,
         zsc_fmachdr-acnum.

  CALL SCREEN 202 STARTING AT 35 5.

  IF sy-ucomm EQ 'CANC'.
    LEAVE TO TRANSACTION 'ZFMACM'.
  ENDIF.

  gs_acdoc_infocus-acnum = TEXT-004.
  gs_acdoc_infocus-x-achdr-acnum = TEXT-004.

  IF gs_acdoc_infocus IS NOT INITIAL.

    LOOP AT gs_acdoc_infocus-x-acitm ASSIGNING FIELD-SYMBOL(<lwa_actim>).
      <lwa_actim>-acnum = gs_acdoc_infocus-x-achdr-acnum.
      <lwa_actim>-updkz = c_updkz_new.
    ENDLOOP.

    LOOP AT gs_acdoc_infocus-x-acvlc ASSIGNING <lwa_acvlc>.
      <lwa_acvlc>-acnum = gs_acdoc_infocus-x-achdr-acnum.
      <lwa_acvlc>-updkz = c_updkz_new.
    ENDLOOP.

    PERFORM document_infocus_prepare.

    gs_variables-refresh_items_grid  = c_true.
  ELSE.
    PERFORM document_infocus_set USING lv_acnum
                                       gs_acdoc_infocus-x-achdr-ajahr
                              CHANGING lv_subrc.
  ENDIF.

*  CLEAR: gs_variables-copy.

ENDFORM.

FORM fcode_reno.

  TYPES: BEGIN OF ly_terrain,
           input  TYPE /agri/gltplnr_fl,
           output TYPE /agri/gltplnr_fl,
           season TYPE /agri/gl_season,
         END OF ly_terrain.

  DATA: lt_terrain      TYPE STANDARD TABLE OF ly_terrain INITIAL SIZE 0,
        lt_sorted       LIKE gs_acdoc_infocus-x-acitm,
        lt_acvlc_del    TYPE STANDARD TABLE OF zfmacvlcl INITIAL SIZE 0,
        lt_acvlc_ins    TYPE STANDARD TABLE OF zfmacvlcl INITIAL SIZE 0,
        lt_acvlc        TYPE zt_fmacvlcl,
        lt_acitm_del    TYPE STANDARD TABLE OF zfmaitm INITIAL SIZE 0,
        lt_acitm_ins    TYPE STANDARD TABLE OF zfmaitm INITIAL SIZE 0,
        lt_acitm        TYPE zt_fmacitm,
        lt_safras       TYPE type_safras_tab,
        lv_sumqtyplants TYPE zfmacqpl,
        lv_sumabsolarea TYPE /agri/glaarea,
        lv_sumavolumen  TYPE zfmacvlctl,
        lv_subrc        TYPE sy-subrc,
        lv_titlebar     TYPE itex132,
        lv_answer.

*-- Atenção! Os Itens e os dados de Volume de Calda serão atualizados!
*-- Atenção! Itens e Volume de Calda serão atualizados! Continuar com a atualização?
  MESSAGE i315(zfmfp) INTO lv_titlebar.
  PERFORM popup_to_confirm USING lv_titlebar
                                 TEXT-t02
                                 abap_true
                        CHANGING lv_answer.

  IF lv_answer NE '1'.
*-- Operação cancelada.
    MESSAGE s302(zfmfp).
    RETURN.
  ENDIF.

*-- ITEMS: Check for updated entries for table ZFMAITM
  PERFORM items_update CHANGING lt_acitm.

  LOOP AT lt_acitm INTO DATA(lwa_acitm).
    INSERT INITIAL LINE INTO TABLE lt_terrain
      ASSIGNING FIELD-SYMBOL(<lwa_terrain>).
    IF sy-subrc EQ 0.
      <lwa_terrain>-input = lwa_acitm-tplnr_fl.
      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
        EXPORTING
          input  = lwa_acitm-tplnr_fl
        IMPORTING
          output = <lwa_terrain>-output.
      <lwa_terrain>-season = lwa_acitm-season.
    ENDIF.
  ENDLOOP.

  SORT lt_terrain BY output season.

  LOOP AT lt_terrain INTO DATA(lwa_terrain).
    READ TABLE lt_acitm INTO lwa_acitm
      WITH KEY tplnr_fl = lwa_terrain-input
               season   = lwa_terrain-season.
    IF sy-subrc EQ 0.
      INSERT INITIAL LINE INTO TABLE lt_sorted
        ASSIGNING FIELD-SYMBOL(<lwa_sorted>).
      IF sy-subrc EQ 0.
        <lwa_sorted> = lwa_acitm.
      ENDIF.
    ENDIF.
  ENDLOOP.

  lt_acitm[] = lt_sorted[].

*-- DELETE OLD ENTRIES FROM TABLE ZFMAITM
  DELETE FROM zfmaitm WHERE acnum = gs_acdoc_infocus-x-achdr-acnum.
  IF sy-subrc EQ 0.
    COMMIT WORK AND WAIT.
  ENDIF.

  REFRESH: gs_acdoc_infocus-x-acitm,
*-- BOC T_T.KONNO 04.07.21
           gt_glflcma,
*-- EOC T_T.KONNO 04.07.21
           gt_fmacitm_fcat.

*-- INSERT NEW ENTRIES INTO TABLE ZFMAITM
  LOOP AT lt_acitm INTO lwa_acitm .
    lwa_acitm-acnum = gs_acdoc_infocus-x-achdr-acnum.
    lwa_acitm-acpos = sy-tabix.
    lwa_acitm-updkz = c_updkz_update.
    APPEND lwa_acitm TO gs_acdoc_infocus-x-acitm.
  ENDLOOP.

  lt_acitm_ins[] = CORRESPONDING #( gs_acdoc_infocus-x-acitm[] ).
  IF lt_acitm_ins[] IS NOT INITIAL.
    MODIFY zfmaitm FROM TABLE lt_acitm_ins.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

  PERFORM terrain_attribute_get CHANGING gs_acdoc_infocus-x-acitm[]
                                         lt_safras.

  LOOP AT gs_acdoc_infocus-x-acitm INTO lwa_acitm.
    ADD lwa_acitm-aarea TO lv_sumabsolarea.
    ADD lwa_acitm-adqpl TO lv_sumqtyplants.
    ADD lwa_acitm-advlc TO lv_sumavolumen.
  ENDLOOP.

*-- CABEÇALHO: Check for changes for table ZFMACHDR
  MOVE lv_sumqtyplants TO zsc_fmachdr-qtplt.
  MOVE c_unit_of_measurement-unit TO zsc_fmachdr-unplt.
  MOVE lv_sumabsolarea TO zsc_fmachdr-arabs.
  MOVE c_unit_of_measurement-hectare TO zsc_fmachdr-unabs.
  MOVE lv_sumavolumen TO zsc_fmachdr-vlctl.
  gs_acdoc_infocus-x-achdr-updkz = c_updkz_update.
  gs_acdoc_infocus-x-achdr-qtplt = zsc_fmachdr-qtplt.
  gs_acdoc_infocus-x-achdr-arabs = zsc_fmachdr-arabs.
  gs_acdoc_infocus-x-achdr-vlctl = zsc_fmachdr-vlctl.

  UPDATE zfmachdr SET qtplt = zsc_fmachdr-qtplt
                      arabs = zsc_fmachdr-arabs
                      vlctl = zsc_fmachdr-vlctl
                WHERE acnum = gs_acdoc_infocus-x-achdr-acnum
                  AND ajahr = gs_acdoc_infocus-x-achdr-ajahr
                  AND actyp = gs_acdoc_infocus-x-achdr-actyp.
  IF sy-subrc EQ 0.
    COMMIT WORK AND WAIT.
  ENDIF.

*-- VOLUME DE CALDA: Check for updated entries for table ZFMACVLCL
*-- DELETE OLD ENTRIES FROM TABLE ZFMACVLCL
  SELECT * FROM zfmacvlcl
    INTO CORRESPONDING FIELDS OF TABLE @lt_acvlc_del
   WHERE acnum = @gs_acdoc_infocus-x-achdr-acnum.

  SORT lt_acvlc_del BY matnr tplnr_fl.

  DELETE FROM zfmacvlcl WHERE acnum = gs_acdoc_infocus-x-achdr-acnum.
  IF sy-subrc EQ 0.
    COMMIT WORK AND WAIT.
  ENDIF.

*-- INSERT NEW ENTRIES INTO TABLE ZFMACVLCL
  REFRESH gs_acdoc_infocus-x-acvlc.
  PERFORM volumen_calda_create.
  LOOP AT gs_acdoc_infocus-x-acvlc ASSIGNING FIELD-SYMBOL(<lwa_acvlc_new>).
    <lwa_acvlc_new>-acnum = gs_acdoc_infocus-x-achdr-acnum.
    READ TABLE lt_acvlc_del INTO DATA(lwa_acvlc_old)
      WITH KEY matnr    = <lwa_acvlc_new>-matnr
               tplnr_fl = <lwa_acvlc_new>-tplnr_fl BINARY SEARCH.
    IF sy-subrc EQ 0.
      <lwa_acvlc_new>-acidt = lwa_acvlc_old-acidt.
      <lwa_acvlc_new>-acdis = lwa_acvlc_old-acdis.
      <lwa_acvlc_new>-acvcl = lwa_acvlc_old-acvcl.
      <lwa_acvlc_new>-acfcb = lwa_acvlc_old-acfcb.
      <lwa_acvlc_new>-acqtb = lwa_acvlc_old-acqtb.
      <lwa_acvlc_new>-acuqb = lwa_acvlc_old-acuqb.
      <lwa_acvlc_new>-acren = lwa_acvlc_old-acren.
      <lwa_acvlc_new>-unren = lwa_acvlc_old-unren.
      <lwa_acvlc_new>-acpex = lwa_acvlc_old-acpex.
      <lwa_acvlc_new>-acarx = lwa_acvlc_old-acarx.
      <lwa_acvlc_new>-unarx = lwa_acvlc_old-unarx.
    ENDIF.
    <lwa_acvlc_new>-updkz = c_updkz_update.
  ENDLOOP.

  gs_acdoc_infocus-updkz = c_updkz_update.
  gs_variables-refresh_items_grid = c_true.
  gs_variables-refresh_vlc_grid = c_true.
  gs_variables-manual_changes = c_true.

  PERFORM controls_display_0203.
  PERFORM vlcl_grid_update.

*-- INSERT NEW ENTRIES INTO TABLE ZFMACVLCL
  lt_acvlc_ins[] = CORRESPONDING #( gs_acdoc_infocus-x-acvlc[] ).
  IF lt_acvlc_ins[] IS NOT INITIAL.
    MODIFY zfmacvlcl FROM TABLE lt_acvlc_ins.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

ENDFORM.

FORM fcode_inrw.


ENDFORM.

FORM fcode_dlrw.

  DATA: lt_rows          TYPE lvc_t_row,
        lwa_row          TYPE lvc_s_row,
        lv_answer        TYPE c,
        lwa_acitm_layout LIKE LINE OF gt_fmacitm_fcat.

  FIELD-SYMBOLS: <lwa_items_modi> TYPE lvc_s_modi,
                 <lwa_acitm>      TYPE zsc_fmacitm,
                 <lwa_acvlc>      TYPE zsc_fmacvlcl.

  CALL METHOD ref_grid_items->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.

  CHECK lt_rows IS NOT INITIAL.
  gs_acdoc_infocus-y-acitm[] =  gs_acdoc_infocus-x-acitm[].
  popup_to_confirm TEXT-007 TEXT-014 c_true lv_answer.
  IF lv_answer EQ '1'  .
    gs_variables-refresh_items_grid = c_true.
    SORT lt_rows BY index DESCENDING.

    LOOP AT lt_rows INTO lwa_row.
      LOOP AT gt_items_modi ASSIGNING <lwa_items_modi> WHERE row_id GE lwa_row-index.
        IF <lwa_items_modi>-row_id NE lwa_row-index.
          <lwa_items_modi>-row_id = <lwa_items_modi>-row_id - 1.
        ELSE.
          DELETE gt_items_modi WHERE row_id EQ <lwa_items_modi>-row_id.
        ENDIF.
      ENDLOOP.
      READ TABLE gt_fmacitm_fcat INTO lwa_acitm_layout
                                     INDEX lwa_row-index.
      IF sy-subrc EQ 0.
        gs_acdoc_infocus-y-acvlc[] = gs_acdoc_infocus-x-acvlc[].
        LOOP AT gs_acdoc_infocus-y-acvlc ASSIGNING <lwa_acvlc>
                                         WHERE tplnr_fl EQ lwa_acitm_layout-tplnr_fl.
          <lwa_acvlc>-updkz = c_updkz_delete.
        ENDLOOP.
      ENDIF.
*...BOC-T_T.KONNO
      READ TABLE gs_acdoc_infocus-x-acitm
        INTO DATA(lwa_acitm_del) INDEX lwa_row-index.
      IF sy-subrc EQ 0.
        DELETE gs_acdoc_infocus-x-acvlc
          WHERE tplnr_fl EQ lwa_acitm_del-tplnr_fl.
      ENDIF.
*...EOC-T_T.KONNO
      DELETE gs_acdoc_infocus-x-acitm INDEX lwa_row-index.
      READ TABLE gs_acdoc_infocus-y-acitm ASSIGNING <lwa_acitm>
      INDEX lwa_row-index.
      IF sy-subrc EQ 0.
        <lwa_acitm>-updkz = c_updkz_delete.
      ENDIF.
      DELETE gt_fmacitm_fcat INDEX lwa_row-index.
      UNASSIGN: <lwa_acitm>.
    ENDLOOP.
  ENDIF.


ENDFORM.

FORM fcode_upms.
  PERFORM call_modal_screen.
ENDFORM.

FORM fcode_cont.


  CASE  sy-dynnr.
    WHEN c_screen-update_mass.
      PERFORM volumen_calda_massupdate.
    WHEN OTHERS.
  ENDCASE.

  SET SCREEN 0.
  LEAVE SCREEN.

ENDFORM.

FORM fcode_conm.

  SET SCREEN 0.
  LEAVE SCREEN.

ENDFORM.

FORM fcode_volr.

  FIELD-SYMBOLS: <lwa_acvlc> TYPE zsc_fmacvlcl.
  FIELD-SYMBOLS: <lwa_acitm> TYPE zsc_fmacitm.

  LOOP AT gs_acdoc_infocus-x-acitm  ASSIGNING <lwa_acitm>
                                    WHERE tplnr_fl IS NOT INITIAL.
    LOOP AT gs_acdoc_infocus-x-acvlc ASSIGNING  <lwa_acvlc>
                                    WHERE tplnr_fl = <lwa_acitm>-tplnr_fl.
      ADD <lwa_acvlc>-acvcl TO <lwa_acitm>-advlc.
    ENDLOOP.
    <lwa_acitm>-updkz = c_updkz_update.
  ENDLOOP.

  gs_variables-refresh_items_grid = c_true.

ENDFORM.

*...BOC-T_T.KONNO
*&---------------------------------------------------------------------*
*& Form UPDATE_VLCL_VOLUME_TABLE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GS_ACDOC_INFOCUS_X_ACVLC[]
*&---------------------------------------------------------------------*
FORM update_vlcl_volume_table CHANGING lt_acvlc TYPE zt_fmacvlcl.

  DATA: lt_tplnr         TYPE /agri/t_gltplnr,
        lv_tplnr         TYPE /agri/gltplnr_fl,
        ls_fldoc_infocus TYPE /agri/s_glfl_doc,
        lv_internal      TYPE atinn,
        lt_fl_doc        TYPE /agri/t_glfl_doc.

  LOOP AT lt_acvlc INTO DATA(lwa_acvlc).
    MOVE lwa_acvlc-tplnr_fl TO lv_tplnr.
    APPEND lv_tplnr TO lt_tplnr.
  ENDLOOP.

  SORT lt_tplnr ASCENDING BY tplnr_fl.
  DELETE ADJACENT DUPLICATES FROM lt_tplnr COMPARING ALL FIELDS.

  CALL FUNCTION '/AGRI/GLFL_VIEW'
    EXPORTING
      it_tplnr       = lt_tplnr
    IMPORTING
      et_fldoc       = lt_fl_doc
    EXCEPTIONS
      no_data_exists = 1
      OTHERS         = 2.

  LOOP AT lt_acvlc ASSIGNING FIELD-SYMBOL(<lwa_acvlc>).
    READ TABLE lt_fl_doc ASSIGNING FIELD-SYMBOL(<ls_fldoc_infocus>)
      WITH KEY tplnr_fl = <lwa_acvlc>-tplnr_fl.
    IF sy-subrc EQ 0.
      IF <lwa_acvlc>-updkz IS INITIAL.
        IF gs_variables-document_mode = c_mode_create.
          <lwa_acvlc>-updkz = c_updkz_new.
        ELSEIF gs_variables-document_mode = c_mode_change.
          <lwa_acvlc>-updkz = c_updkz_update.
        ENDIF.
      ENDIF.
      gs_variables-refresh_vlc_grid = c_true.
    ENDIF.
  ENDLOOP.

ENDFORM.
*...EOC-T_T.KONNO

*&---------------------------------------------------------------------*
*& Form ITEMS_UPDATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_ACITM
*&---------------------------------------------------------------------*
FORM items_update CHANGING lt_acitm TYPE zt_fmacitm.

  DATA: lt_acitm_aux TYPE zt_fmacitm.

*-- BOC T_T.KONNO 04.07.21
*  SELECT * FROM /agri/glflcma AS a
*    INNER JOIN /agri/glflot AS t
*    ON a~tplnr_fl EQ t~tplnr_fl
*    INTO CORRESPONDING FIELDS OF TABLE @lt_acitm
*   WHERE t~iwerk IN @so_werks[]
*     AND a~cmnum EQ 'CITROS'
*     AND a~loevm NE @abap_true
*     AND a~astat EQ @c_crop_season_status-active.
*
**-- BOC T_T.KONNO-11/10/20
*  SELECT * FROM /agri/glflcma AS a
*    INNER JOIN /agri/glflot AS t
*    ON a~tplnr_fl EQ t~tplnr_fl
*    APPENDING CORRESPONDING FIELDS OF TABLE @lt_acitm
*   WHERE t~iwerk      IN @so_werks[]
*     AND a~cmnum      EQ 'CITROS'
*     AND a~loevm      NE @abap_true
*     AND a~zzprevisto EQ @abap_true.
**-- EOC T_T.KONNO-11/10/20
  SELECT * FROM /agri/glflcma AS a
    INNER JOIN /agri/glflot AS t
    ON a~tplnr_fl EQ t~tplnr_fl
    INTO CORRESPONDING FIELDS OF TABLE @gt_glflcma
   WHERE t~iwerk      IN @so_werks[]
     AND a~cmnum      EQ 'CITROS'
     AND a~loevm      NE @abap_true
     AND a~astat      EQ @c_crop_season_status-active
     AND a~zzprevisto EQ @abap_false.

  SELECT * FROM /agri/glflcma AS a
    INNER JOIN /agri/glflot AS t
    ON a~tplnr_fl EQ t~tplnr_fl
    APPENDING CORRESPONDING FIELDS OF TABLE @gt_glflcma
   WHERE t~iwerk      IN @so_werks[]
     AND a~cmnum      EQ 'CITROS'
     AND a~loevm      NE @abap_true
     AND a~zzprevisto EQ @abap_true.
*-- EOC T_T.KONNO 04.07.21

*-- BOC T_T.KONNO 04.07.21
*  SORT lt_acitm BY tplnr_fl ASCENDING contr DESCENDING.
*  DELETE ADJACENT DUPLICATES FROM lt_acitm COMPARING tplnr_fl contr.
*
*  DELETE lt_acitm WHERE NOT ( datab BETWEEN gs_acdoc_infocus-x-achdr-datab
*                                        AND gs_acdoc_infocus-x-achdr-datbi )
*                    AND NOT ( datbi BETWEEN gs_acdoc_infocus-x-achdr-datab
*                                        AND gs_acdoc_infocus-x-achdr-datbi )
*                    AND NOT ( datab LE p_datab AND
*                              datbi GE p_datbi ).
*  SORT gt_glflcma BY tplnr_fl ASCENDING contr DESCENDING.
  SORT gt_glflcma BY tplnr_fl contr.
  DELETE ADJACENT DUPLICATES FROM gt_glflcma COMPARING tplnr_fl contr.

  DELETE gt_glflcma WHERE NOT ( datab BETWEEN gs_acdoc_infocus-x-achdr-datab
                                          AND gs_acdoc_infocus-x-achdr-datbi )
                      AND NOT ( datbi BETWEEN gs_acdoc_infocus-x-achdr-datab
                                        AND gs_acdoc_infocus-x-achdr-datbi )
                      AND NOT ( datab LE p_datab AND
                                datbi GE p_datbi ).

*-------------------------------------------------------------------*
*-- BOC T_T.KONNO 04.10.21
* lt_acitm = CORRESPONDING #( gt_glflcma ).
  DATA(lt_glflcma_1) = gt_glflcma[].
  DATA(lt_glflcma_2) = gt_glflcma[].
*-- 1)NÃO PREVISTO: [ZZPREVISTO = ABAP_FALSE] ou
*-- [ZZPREVISTO = ABAP_TRUE e ZZPREV_PLANTIO = VAZIO]
  DELETE lt_glflcma_1 WHERE zzprevisto EQ abap_true
                        AND zzprev_plantio IS NOT INITIAL.
  lt_acitm = CORRESPONDING #( lt_glflcma_1 ).
*-- 2) PREVISTO: [ZZPREVISTO = ABAP_TRUE e ZZPREV_PLANTIO <> VAZIO]
  DELETE lt_glflcma_2 WHERE zzprevisto EQ abap_false
                         OR zzprev_plantio IS INITIAL.
  lt_acitm_aux = CORRESPONDING #( lt_glflcma_2 MAPPING aarea = zzarea_talhao ).
  APPEND LINES OF lt_acitm_aux TO lt_acitm.
*-- EOC T_T.KONNO 04.10.21
*-------------------------------------------------------------------*
*-- EOC T_T.KONNO 04.07.21

ENDFORM.

*&---------------------------------------------------------------------*
*& Form UPDATE_TOTAL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM update_total .

  DATA: lt_safras TYPE type_safras_tab.

  PERFORM terrain_attribute_get CHANGING gs_acdoc_infocus-x-acitm[]
                                         lt_safras.

  PERFORM quantities_totalize.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> TEXT_006
*&      --> TEXT_007
*&      --> ABAP_TRUE
*&      <-- LV_ANSWER
*&---------------------------------------------------------------------*
FORM popup_to_confirm USING lv_titlebar
                            lv_question
                            lv_display_cancel
                   CHANGING lv_answer.

  CLEAR lv_answer.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = lv_titlebar
      text_question         = lv_question
      display_cancel_button = lv_display_cancel
    IMPORTING
      answer                = lv_answer
    EXCEPTIONS ##fm_subrc_ok
      text_not_found        = 1
      OTHERS                = 2.

ENDFORM.
