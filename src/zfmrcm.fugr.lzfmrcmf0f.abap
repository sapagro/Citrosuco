*----------------------------------------------------------------------*
***INCLUDE /AGRI/LFMRCMNF0F .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FIELD_CATALOG_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM field_catalog_prepare USING lv_structure_name
                        CHANGING lt_fcat TYPE lvc_t_fcat.

  FIELD-SYMBOLS : <lwa_fcat> LIKE LINE OF lt_fcat.

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

      READ TABLE lt_fcat ASSIGNING <lwa_fcat> WITH KEY fieldname = 'RCNUM'.
      IF <lwa_fcat> IS ASSIGNED.
        <lwa_fcat>-hotspot = c_true.
        <lwa_fcat>-key     = c_true.
      ENDIF.
    WHEN c_structure_name-doce.
      LOOP AT lt_fcat ASSIGNING <lwa_fcat>.
        CASE <lwa_fcat>-fieldname.
          WHEN 'RCDTA' OR 'RCDOS'.
            <lwa_fcat>-edit = c_true.
          WHEN 'MAKTX' OR 'UNITS' OR 'POSNR'.
          WHEN 'MATNR_INS'.
            <lwa_fcat>-edit = c_true.
            <lwa_fcat>-f4availabl = c_true.
          WHEN 'RCINP'.
*...Vistex-11.01.2019/Begin
            <lwa_fcat>-icon = c_true.
            <lwa_fcat>-hotspot = abap_true.
*            <lwa_fcat>-edit = abap_true.
*            <lwa_fcat>-checkbox = abap_true.
*...Vistex-11.01.2019/End
          WHEN OTHERS.
            <lwa_fcat>-tech = c_true.
        ENDCASE.
      ENDLOOP.
    WHEN c_structure_name-ver.
      LOOP AT lt_fcat ASSIGNING <lwa_fcat>.
        CASE <lwa_fcat>-fieldname.
          WHEN     'STLAL' OR 'PLNNR' OR 'TEXT1' OR 'PLNAL' OR 'VERID' .
            <lwa_fcat>-edit = c_true.
            <lwa_fcat>-f4availabl = c_true.
          WHEN  'POSNR' OR 'PLNTY'  OR 'ADATU' OR 'BDATU'.
          WHEN OTHERS.
            <lwa_fcat>-tech = c_true.
        ENDCASE.
      ENDLOOP.

    WHEN c_structure_name-bom.
      LOOP AT lt_fcat ASSIGNING <lwa_fcat>.
        CASE <lwa_fcat>-fieldname.
          WHEN  'STLNR' OR  'STLAL' OR  'MATNR_INS' OR  'RCDOS' OR  'UNITS'.
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
      PERFORM (lv_routine) IN PROGRAM (c_program_rcm)
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
        lwa_search_header TYPE zsc_fmrchdr,
        lv_dummy,
        lwa_selected_doc  LIKE LINE OF gt_selected_docs.

  CLEAR: gs_rckey,
         gs_rcdoc_infocus,
         gs_rcdoc_infocus-x-rchdr,
         gs_rcdoc_infocus-y-rchdr.

*...Vistex-11.06.2019/Begin
  REFRESH gt_similares.
*...Vistex-11.06.2019/End

  SORT gt_search_header ASCENDING BY rcnum.

  READ TABLE gt_selected_docs INTO lwa_selected_doc INDEX 1.
  CHECK sy-subrc EQ 0.
*...Vistex-11.06.2019/Begin
  MOVE-CORRESPONDING lwa_selected_doc TO zsc_fmrchdr.
  PERFORM set_grid_lines .
*...Vistex-11.06.2019/End

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
      MESSAGE ID 'ZFMRC' TYPE c_msg_type-success NUMBER '007'
      INTO sy-msgli.
      message_simple space.
      PERFORM messages_display USING gs_variables-initiator.
      EXIT.
    ENDIF.
  ENDIF.

**Dequeue the document infocus
  IF gs_rcdoc_infocus-x-rchdr-rcnum IS INITIAL.
    READ TABLE gt_search_header INTO lwa_search_header
                                WITH  KEY  rcnum = lwa_selected_doc-rcnum
                                BINARY SEARCH.
    MOVE: lwa_search_header-rcnum TO gs_rcdoc_infocus-rcnum,
          lwa_search_header-werks TO gs_rcdoc_infocus-werks,
          lwa_search_header-matnr TO gs_rcdoc_infocus-matnr.
    MOVE-CORRESPONDING : gs_rcdoc_infocus TO gs_rckey.
  ENDIF.
  PERFORM document_infocus_unlock USING gs_rckey.
  PERFORM document_infocus_set USING    gs_rckey
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
        PERFORM document_infocus_unlock USING gs_rckey.
        PERFORM document_infocus_set USING gs_rckey
                                     CHANGING lv_subrc.
        gs_variables-document_mode = c_mode_display.
        gs_variables-overview_mode = c_mode_display.
      ENDIF.

    ELSEIF ok_code EQ c_fcode-save.

      CLEAR: lv_answer, ok_code.
      PERFORM document_infocus_save USING space.
      PERFORM document_infocus_unlock USING gs_rckey.
      gs_variables-overview_mode = c_mode_display.
      gs_variables-document_mode = c_mode_display.
      PERFORM document_infocus_set USING gs_rckey
                                CHANGING lv_subrc.
    ENDIF.
    gs_variables-refresh_worklist = c_true.
  ELSE.
    gs_variables-overview_mode = c_mode_change.
    PERFORM document_infocus_unlock USING gs_rckey.
    PERFORM document_infocus_set USING    gs_rckey
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
  PERFORM recipe_create.
ENDFORM.                    "fcode_crea
***&---------------------------------------------------------------------*
***&      Form  fcode_cont
***&---------------------------------------------------------------------*
***       text
***----------------------------------------------------------------------*
FORM fcode_cont.

  CASE sy-dynnr.
    WHEN c_screen-popup_version_modify.
      PERFORM modify_mass.
    WHEN c_screen-popup_recipe_create.
    WHEN c_screen-popup_version_create.
      PERFORM version_data_prepared.
    WHEN OTHERS.
  ENDCASE.
  CASE sy-dynnr.
    WHEN    c_screen-popup_version_modify
         OR c_screen-popup_version_create.
      PERFORM free_memory_screen USING 'ZSC_FMRCVRS'.
    WHEN c_screen-popup_recipe_create.
    WHEN OTHERS.
  ENDCASE.
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

  CHECK: NOT gs_rcdoc_infocus-rcnum IS INITIAL.

  lwa_objid-objid  = gs_rcdoc_infocus-rcnum.
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
*  gs_variables-refresh_output_table = c_true.

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
      OR NOT gs_rcdoc_infocus IS INITIAL.
        APPEND c_fcode-create TO lt_fcode_excludes.
      ENDIF.

      IF gs_rcdoc_infocus IS INITIAL.
        APPEND c_fcode-back            TO lt_fcode_excludes.
        APPEND c_fcode-cancel          TO lt_fcode_excludes.
        APPEND c_fcode-copy            TO lt_fcode_excludes.
        APPEND c_fcode-reno            TO lt_fcode_excludes.
        APPEND c_fcode-save            TO lt_fcode_excludes.
        APPEND c_fcode-delete          TO lt_fcode_excludes.
        APPEND c_fcode-delete_alternative          TO lt_fcode_excludes.
        APPEND c_fcode-where_used_list TO lt_fcode_excludes.
      ENDIF.

      IF gs_variables-document_mode EQ c_mode_display.
        APPEND c_fcode-save   TO lt_fcode_excludes.
        APPEND c_fcode-delete TO lt_fcode_excludes.
        APPEND c_fcode-delete_alternative TO lt_fcode_excludes.
        APPEND c_fcode-copy TO lt_fcode_excludes.
        APPEND c_fcode-reno TO lt_fcode_excludes.
      ENDIF.

      IF gs_variables-document_mode EQ c_mode_create.
        APPEND c_fcode-delete          TO lt_fcode_excludes.
        APPEND c_fcode-delete_alternative TO lt_fcode_excludes.
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
        lv_rcnum    TYPE zfmrcnum.

*  IF zsc_fmrchdr-statu = space.

  lv_text = TEXT-015.
  lv_rcnum = gs_rcdoc_infocus-rcnum.
  REPLACE '&1' WITH lv_rcnum INTO lv_text.
  popup_to_confirm TEXT-010 lv_text c_true lv_answer.

  CHECK lv_answer EQ '1'.

  gs_variables-initiator =  c_log_initiator-check.
  PERFORM messages_initialize USING  gs_variables-initiator
                                     c_log_subobject-check
                                     gs_rcdoc_infocus-x-rchdr.

*--Call Authority Check
  PERFORM authority_check
                    USING gs_rcdoc_infocus-x-rchdr
                          c_authorization_activity-delete
                          c_msg_type-error
                 CHANGING lv_subrc.
  IF lv_subrc NE 0.
    PERFORM messages_display USING gs_variables-initiator.
    EXIT.
  ENDIF.
  PERFORM delete_all.
  CALL FUNCTION 'ZFMRC_DELETE'
    CHANGING
      cs_rcdoc = gs_rcdoc_infocus.

  lv_view = ref_worklist->view_in_focus_get( ).

  IF lv_view EQ '1'.
    DELETE gt_worklist_header WHERE rcnum = gs_rcdoc_infocus-rcnum.
  ELSEIF lv_view EQ '2'.
    DELETE gt_search_header WHERE rcnum = gs_rcdoc_infocus-rcnum.
  ENDIF.

  CLEAR: gs_variables-document_mode, ok_code.
  gs_variables-refresh_worklist = c_true.
  PERFORM messages_display USING gs_variables-initiator.
  PERFORM document_data_initialize USING c_true.



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



ENDFORM.


FORM fcode_copy.

  DATA : lv_rcnum  TYPE zfmrcnum,
         lv_answer TYPE c,
         lv_subrc  TYPE sy-subrc,
         lv_dummy.

  FIELD-SYMBOLS :
    <lwa_rcnum> TYPE  zsc_fmrcnum,
    <lwa_rcbom> TYPE  zsc_fmrcbom,
    <lwa_rclst> TYPE  zsc_fmrclst.

  lv_rcnum                   = gs_rcdoc_infocus-rcnum.
  gs_variables-copy          = c_true.
  gs_variables-document_mode = c_mode_create.

  gs_rcdoc_infocus-x-rchdr-rcnum_ref = gs_rcdoc_infocus-x-rchdr-rcnum.
  zsc_fmrchdr-rcnum_ref = gs_rcdoc_infocus-x-rchdr-rcnum.
  CLEAR  gs_rcdoc_infocus-y.
  CLEAR : gs_rcdoc_infocus-x-rchdr-ernam,
          gs_rcdoc_infocus-x-rchdr-erdat,
          gs_rcdoc_infocus-x-rchdr-erzet,
          gs_rcdoc_infocus-x-rchdr-aenam,
          gs_rcdoc_infocus-x-rchdr-aedat,
          gs_rcdoc_infocus-x-rchdr-aezet.

*** Number range object
  CLEAR : gs_rcdoc_infocus-rcnum, gs_rcdoc_infocus-x-rchdr-rcnum,
          zsc_fmrchdr-rcnum.
  CALL SCREEN 204 STARTING AT 35 5.
  CLEAR : gs_rcdoc_infocus-x-rchdr-stlnr,
          gs_rcdoc_infocus-x-rchdr-stlal,
          gs_rcdoc_infocus-x-rcbom[],
          gs_rcdoc_infocus-x-rcvrs.

  gs_rcdoc_infocus-rcnum = gs_rcdoc_infocus-x-rchdr-rcnum
                         = TEXT-004.
*  gs_rcdoc_infocus-x-rchdr-acdes = zsc_fmrchdr-acdes.
  IF gs_rcdoc_infocus IS NOT INITIAL.
    LOOP AT gs_rcdoc_infocus-x-rclst ASSIGNING <lwa_rclst>.
      CLEAR : <lwa_rclst>-mandt.
      <lwa_rclst>-werks = gs_rcdoc_infocus-x-rchdr-werks.
      <lwa_rclst>-rcnum = gs_rcdoc_infocus-x-rchdr-rcnum.
      <lwa_rclst>-stlal = space.
      <lwa_rclst>-updkz = c_updkz_new.
    ENDLOOP.

    LOOP AT gs_rcdoc_infocus-x-rcbom ASSIGNING <lwa_rcbom>.
      <lwa_rcbom>-werks = gs_rcdoc_infocus-x-rchdr-werks.
    ENDLOOP.

    LOOP AT gs_rcdoc_infocus-x-rcvrs ASSIGNING FIELD-SYMBOL(<lwa_rcvrs>).
      <lwa_rcvrs>-werks = gs_rcdoc_infocus-x-rchdr-werks.
    ENDLOOP.

    gs_rcdoc_infocus-x-rchdr-updkz = c_updkz_new.
    PERFORM document_infocus_prepare.
*--Refresh ALV's
    gs_variables-refresh_dose_grid  = c_true.
  ELSE.
    PERFORM document_infocus_set USING gs_rckey
                              CHANGING lv_subrc.
  ENDIF.
  CLEAR: gs_variables-copy.
ENDFORM.

FORM fcode_reno.
ENDFORM.

FORM fcode_inrw.


ENDFORM.

FORM fcode_dlrw.

  DATA: lt_rows          TYPE lvc_t_row,
        lrt_matnr        TYPE RANGE OF matnr,
        lwa_row          TYPE lvc_s_row,
        lv_answer        TYPE c,
        lv_matnr         TYPE matnr,
        lwa_rclst_layout LIKE LINE OF gt_fmrclst_fcat.


  FIELD-SYMBOLS: <lwa_dose_modi> TYPE lvc_s_modi,
                 <lwa_rcbom>     TYPE zsc_fmrcbom,
                 <lwa_rclst>     TYPE zsc_fmrclst.

  CALL METHOD ref_grid_rclst->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.

  CHECK lt_rows IS NOT INITIAL.
*--BOC-T_T.KONNO-09.10.20
  DATA(lt_insumos) = gt_fmrclst_fcat[].
  DELETE lt_insumos WHERE matnr_ins IS INITIAL.
  IF lt_insumos[] IS NOT INITIAL
  AND zsc_fmrchdr-rctyp IS NOT INITIAL.
    IF zsc_fmrchdr-rctyp EQ 'ZORC'.
      SELECT acnum, extwg, matkl, rcnum,
             matnr, period, fazenda, produtos
        FROM zabs_orcamento
        INTO TABLE @DATA(lt_receita_zorc)
        FOR ALL ENTRIES IN @lt_insumos
       WHERE rcnum = @zsc_fmrchdr-rcnum
         AND matnr = @lt_insumos-matnr_ins.

      DELETE lt_receita_zorc WHERE produtos IS INITIAL.
      SORT lt_receita_zorc BY matnr.
      DELETE ADJACENT DUPLICATES FROM lt_receita_zorc COMPARING matnr.
    ELSEIF zsc_fmrchdr-rctyp EQ 'ZREC'.
      SELECT werks, acnum, extwg, matkl,
             tplnr_fl, data, rcnum, matnr_ins
        FROM zfmrchis
        INTO TABLE @DATA(lt_receita_zrec)
        FOR ALL ENTRIES IN @lt_insumos
       WHERE rcnum     = @zsc_fmrchdr-rcnum
         AND matnr_ins = @lt_insumos-matnr_ins.

      SORT lt_receita_zrec BY matnr_ins.
      DELETE ADJACENT DUPLICATES FROM lt_receita_zrec COMPARING matnr_ins.
    ENDIF.
  ENDIF.

  LOOP AT lt_rows INTO lwa_row.
    READ TABLE gt_fmrclst_fcat INTO DATA(lwa_insumo) INDEX lwa_row-index.
    IF sy-subrc EQ 0.
      DATA(lv_not_allowed) = abap_false.
      IF zsc_fmrchdr-rctyp EQ 'ZORC'.
        READ TABLE lt_receita_zorc INTO DATA(lwa_receita_zorc)
          WITH KEY matnr = lwa_insumo-matnr_ins BINARY SEARCH.
        IF sy-subrc EQ 0.
          lv_not_allowed = abap_true.
        ENDIF.
      ELSEIF zsc_fmrchdr-rctyp EQ 'ZREC'.
        READ TABLE lt_receita_zrec INTO DATA(lwa_receita_zrec)
          WITH KEY matnr_ins = lwa_insumo-matnr_ins BINARY SEARCH.
        IF sy-subrc EQ 0.
          lv_not_allowed = abap_true.
        ENDIF.
      ENDIF.

      IF lv_not_allowed = abap_true.
        CLEAR lv_matnr.
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
          EXPORTING
            input  = lwa_insumo-matnr_ins
          IMPORTING
            output = lv_matnr.
*-- O insumo &1 está sendo utilizado no Agriplan. Não é possível eliminá-lo.
        MESSAGE i269(zfmfp) WITH lv_matnr.
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.
*--EOC-T_T.KONNO-09.10.20

  IF lv_not_allowed EQ abap_false.
    gs_rcdoc_infocus-y =  gs_rcdoc_infocus-x.
*...Vistex-11.06.2019/Begin
*  popup_to_confirm TEXT-007 TEXT-014 c_true lv_answer.
*...Eliminar o insumo selecionado e o produto similar correspondente?
    popup_to_confirm TEXT-007 TEXT-055 c_true lv_answer.
*...Vistex-11.06.2019/End
    IF lv_answer EQ '1'  .
      gs_variables-refresh_dose_grid = c_true.
      SORT lt_rows BY index DESCENDING.

      LOOP AT lt_rows INTO lwa_row.
*...Vistex-11.01.2019/Begin
*      LOOP AT gt_dose_modi ASSIGNING <lwa_dose_modi> WHERE row_id GE lwa_row-index.
*        IF <lwa_dose_modi>-row_id NE lwa_row-index.
*          <lwa_dose_modi>-row_id = <lwa_dose_modi>-row_id - 1.
*        ELSE.
*          DELETE gt_dose_modi WHERE row_id EQ <lwa_dose_modi>-row_id.
*        ENDIF.
*      ENDLOOP.
*      DELETE gs_rcdoc_infocus-x-rclst INDEX lwa_row-index.
*      READ TABLE gs_rcdoc_infocus-y-rclst ASSIGNING <lwa_rclst>
*      INDEX lwa_row-index.
*      IF sy-subrc EQ 0.
*        <lwa_rclst>-updkz = c_updkz_delete.
*        READ TABLE gs_rcdoc_infocus-y-rcbom ASSIGNING <lwa_rcbom>
*                                             WITH KEY stlal = <lwa_rclst>-stlal
*                                                   stlnr = zsc_fmrchdr-stlnr
*                                                   matnr_ins = <lwa_rclst>-matnr_ins
*                                                   rcnum  = <lwa_rclst>-rcnum.
*        IF sy-subrc EQ 0.
*          MOVE c_updkz_delete TO <lwa_rcbom>-updkz.
*        ENDIF.
*      ENDIF.
*      DELETE gt_fmrclst_fcat INDEX lwa_row-index.
*      UNASSIGN: <lwa_rclst>.
        READ TABLE gt_fmrclst_fcat INTO DATA(lwa_fmrclst) INDEX lwa_row-index.
        IF sy-subrc EQ 0.
          DELETE FROM zfmrcsim WHERE rcnum = lwa_fmrclst-rcnum
                                 AND stlal = lwa_fmrclst-stlal
                                 AND matnr_ins = lwa_fmrclst-matnr_ins.
          IF sy-subrc EQ 0.
            COMMIT WORK AND WAIT.
          ENDIF.

          DELETE gt_similares WHERE matnr_ins = lwa_fmrclst-matnr_ins.

*...Vistex-11.07.2019/Begin
          DELETE gs_rcdoc_infocus-x-rclst
            WHERE rcnum     = lwa_fmrclst-rcnum
              AND stlal     = lwa_fmrclst-stlal
              AND matnr_ins = lwa_fmrclst-matnr_ins.
*...Vistex-11.07.2019/End

          READ TABLE gs_rcdoc_infocus-y-rclst ASSIGNING FIELD-SYMBOL(<lwa_rclst_old>)
            WITH KEY rcnum     = lwa_fmrclst-rcnum
                     stlal     = lwa_fmrclst-stlal
                     matnr_ins = lwa_fmrclst-matnr_ins.

          IF sy-subrc EQ 0.
            <lwa_rclst_old>-updkz = c_updkz_delete.
            READ TABLE gs_rcdoc_infocus-y-rcbom ASSIGNING FIELD-SYMBOL(<lwa_rcbom_old>)
              WITH KEY rcnum     = <lwa_rclst_old>-rcnum
                       stlal     = <lwa_rclst_old>-stlal
                       stlnr     = zsc_fmrchdr-stlnr
                       matnr_ins = <lwa_rclst_old>-matnr_ins.
            IF sy-subrc EQ 0.
              <lwa_rcbom_old>-updkz = c_updkz_delete.
            ENDIF.
          ENDIF.
          DELETE gt_fmrclst_fcat INDEX lwa_row-index.
        ENDIF.
*...Vistex-11.01.2019/End
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.

FORM fcode_conm.

  SET SCREEN 0.
  LEAVE SCREEN.

ENDFORM.


FORM fcode_cbom.

  DATA:
    it_bomgroup    LIKE bapi1080_bgr_c OCCURS 0 WITH HEADER LINE,
    it_variants    LIKE bapi1080_bom_c OCCURS 0 WITH HEADER LINE,
    it_items       LIKE bapi1080_itm_c OCCURS 0 WITH HEADER LINE,
    it_matrel      LIKE bapi1080_mbm_c OCCURS 0 WITH HEADER LINE,
    it_itemas      LIKE bapi1080_rel_itm_bom_c OCCURS 0 WITH HEADER LINE,
    it_return      TYPE TABLE OF bapiret2,
    it_subitems    LIKE bapi1080_sui_c OCCURS 0 WITH HEADER LINE,
    lv_alter_hight TYPE stalt,
    lv_alter_numc  TYPE numc2,
    lv_subrc       TYPE int4,
    it_subitemas   LIKE bapi1080_rel_sui_itm_c OCCURS 0 WITH HEADER LINE.

  PERFORM messages_initialize USING gs_variables-initiator
                                     c_log_subobject-create
                                     gs_rcdoc_infocus-x-rchdr.
*fill the data
*material bom group header data
  CLEAR it_bomgroup.
  it_bomgroup-bom_group_identification = 'BAPI_SMP_COL1'.
  it_bomgroup-object_type = 'BGR'.
  it_bomgroup-object_id = 'SIMPLE1'.
  it_bomgroup-bom_usage = '1'.
  it_bomgroup-created_in_plant = zsc_fmrchdr-werks.

  it_bomgroup-ltxt_lang = sy-langu.
  it_bomgroup-technical_type = ' '.
  it_bomgroup-bom_text = zsc_fmrchdr-rcnum.
  APPEND it_bomgroup.

  PERFORM alternative_max_get CHANGING lv_alter_hight
                                       lv_subrc.
  IF  lv_subrc NE 0.
    MOVE '01' TO  lv_alter_hight.
  ELSE.
    MOVE lv_alter_hight TO lv_alter_numc.
    lv_alter_numc = lv_alter_numc + 1.
    lv_alter_hight = lv_alter_numc.
  ENDIF.

*header details of the different variants
  CLEAR it_variants.
  it_variants-bom_group_identification = 'BAPI_SMP_COL1'.
  it_variants-object_type = 'BOM'.
  it_variants-object_id = 'SIMPLE1'.
  it_variants-alternative_bom = lv_alter_hight.
  it_variants-bom_status = '01'.
  it_variants-base_qty = '1.000'.
  it_variants-valid_from_date = zsc_fmrchdr-datuv.
  it_variants-function = 'NEW'.
  APPEND it_variants.

*details of the materials of the different variants
  CLEAR it_matrel.
  it_matrel-bom_group_identification = 'BAPI_SMP_COL1'.
  it_matrel-material = zsc_fmrchdr-matnr.
  it_matrel-plant = zsc_fmrchdr-werks.
  it_matrel-bom_usage = '1'.
  it_matrel-alternative_bom = lv_alter_hight.
  APPEND it_matrel.

*linking subitems to the corresponding variants
  CLEAR it_subitemas.
  it_subitemas-bom_group_identification = 'BAPI_SMP_COL1'.
  it_subitemas-sub_object_type = 'SUI'.
  it_subitemas-sub_object_id = 'SIM1'.
  it_subitemas-super_object_type = 'ITM'.
  it_subitemas-super_object_id = 'SIMPLE1'.
  APPEND it_subitemas.


*linking items to the corresponding variants
  CLEAR it_itemas.
  it_itemas-bom_group_identification = 'BAPI_SMP_COL1'.
  it_itemas-sub_object_type = 'ITM'.
  it_itemas-sub_object_id = 'SIMPLE1'.
  it_itemas-super_object_type = 'BOM'.
  it_itemas-super_object_id = 'SIMPLE1'.
  it_itemas-valid_from_date = zsc_fmrchdr-datuv.
  it_itemas-function = 'NEW'.
  APPEND it_itemas.

  LOOP AT gs_rcdoc_infocus-x-rclst INTO DATA(lwa_rclst).

*details of the items of the variants
    CLEAR it_items.
    it_items-bom_group_identification = 'BAPI_SMP_COL1'.
    it_items-object_type = 'ITM'.
    it_items-object_id = 'SIMPLE1'.
    it_items-item_no = lwa_rclst-posnr+2(4).
    it_items-item_cat = 'L'.
    it_items-component = lwa_rclst-matnr_ins.
    IF zsc_fmrchdr-ausme = 'HC'.
      it_items-comp_qty = lwa_rclst-rcdos.
    ELSEIF zsc_fmrchdr-ausme = 'VC'.
      it_items-comp_qty = '1'.
    ENDIF.
    it_items-valid_from_date = zsc_fmrchdr-datuv.
    APPEND it_items.

  ENDLOOP.

  CALL FUNCTION 'BAPI_MATERIAL_BOM_GROUP_CREATE'
    EXPORTING
      all_error          = 'X'
    TABLES
      bomgroup           = it_bomgroup
      variants           = it_variants
      items              = it_items
      materialrelations  = it_matrel
      itemassignments    = it_itemas
      subitems           = it_subitems
      subitemassignments = it_subitemas
      return             = it_return.

  LOOP AT it_return INTO DATA(lwa_return) WHERE type = 'E'.
    MESSAGE ID lwa_return-id
            TYPE lwa_return-type
            NUMBER lwa_return-number
            WITH   lwa_return-message_v1
                   lwa_return-message_v2
            lwa_return-message_v3
            lwa_return-message_v4
    INTO sy-msgli.
    message_simple space.
  ENDLOOP.
  IF sy-subrc NE 0.
    READ TABLE it_return INTO DATA(lwa_return_aux) INDEX 1.
    IF sy-subrc EQ 0.
      PERFORM bapi_commit.
      IF zsc_fmrchdr-stlnr IS INITIAL.
        MOVE lwa_return_aux-message_v2+0(8) TO gs_rcdoc_infocus-x-rchdr-stlnr.
      ENDIF.
      " Last alternative
      MOVE lv_alter_hight TO gs_rcdoc_infocus-x-rchdr-stlal.
      MOVE c_updkz_update TO gs_rcdoc_infocus-x-rchdr-updkz.
*...Vistex-11.07.2019/Begin
      zsc_fmrchdr-stlal = lv_alter_hight.
*...Vistex-11.07.2019/End
      MESSAGE ID 'ZFMRC' TYPE c_msg_type-success NUMBER '090'
                         WITH   lv_alter_hight
                                lwa_return_aux-message_v2+0(8)
                          INTO sy-msgli.
      message_simple space.
    ENDIF.
    CHECK gs_rcdoc_infocus-x-rchdr-stlnr IS NOT INITIAL
          AND gs_rcdoc_infocus-x-rchdr-stlal IS NOT INITIAL.
    PERFORM bom_byalter_create USING gs_rcdoc_infocus-x-rchdr-stlnr
                                     gs_rcdoc_infocus-x-rchdr-stlal.
  ENDIF.

  PERFORM messages_display USING gs_variables-initiator.

ENDFORM.

FORM fcode_vers.
  CALL SCREEN 202 STARTING AT 35 5.
ENDFORM.

FORM fcode_verm.
  CALL SCREEN 206 STARTING AT 35 5.
ENDFORM.

FORM fcode_dlvr.

  DATA: lt_rows          TYPE lvc_t_row,
        lwa_row          TYPE lvc_s_row,
        lv_answer        TYPE c,
        lwa_rcvrs_layout LIKE LINE OF gt_fmrcvrs_fcat.

  FIELD-SYMBOLS: <lwa_version_modi> TYPE lvc_s_modi,
                 <lwa_rcvrs>        TYPE zsc_fmrcvrs.


  CALL METHOD ref_grid_rcvrs->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.

  CHECK lt_rows IS NOT INITIAL.
  gs_rcdoc_infocus-y-rcvrs[] =  gs_rcdoc_infocus-x-rcvrs[].
  popup_to_confirm TEXT-052 TEXT-014 c_true lv_answer.
  IF lv_answer EQ '1'  .
    gs_variables-refresh_vrs_grid = c_true.
    SORT lt_rows BY index DESCENDING.

    LOOP AT lt_rows INTO lwa_row.
      LOOP AT gt_rcvrs_modi ASSIGNING <lwa_version_modi> WHERE row_id GE lwa_row-index.
        IF <lwa_version_modi>-row_id NE lwa_row-index.
          <lwa_version_modi>-row_id = <lwa_version_modi>-row_id - 1.
        ELSE.
          DELETE gt_rcvrs_modi WHERE row_id EQ <lwa_version_modi>-row_id.
        ENDIF.
      ENDLOOP.
      DELETE gs_rcdoc_infocus-x-rcvrs INDEX lwa_row-index.
      READ TABLE gs_rcdoc_infocus-y-rcvrs ASSIGNING <lwa_rcvrs>
      INDEX lwa_row-index.
      IF sy-subrc EQ 0.
        <lwa_rcvrs>-updkz = c_updkz_delete.
      ENDIF.
      DELETE gt_fmrcvrs_fcat INDEX lwa_row-index.
      UNASSIGN: <lwa_rcvrs>.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FREE_MEMORY_SCREEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM free_memory_screen USING VALUE(lv_struct) .

  FIELD-SYMBOLS: <struct> TYPE any.
  ASSIGN (lv_struct) TO <struct>.
  IF <struct> IS ASSIGNED.
    CLEAR: <struct>.
  ENDIF.
ENDFORM.

FORM fcode_chalt.
  PERFORM alternative_change.
ENDFORM.

FORM fcode_dela.

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
        lv_subrc         TYPE int4,
        lv_lines         TYPE systepl VALUE 1,
        lwa_stas         TYPE stpo,
        lv_datum         TYPE csap_mbom-datuv,
        ls_rcdoc         TYPE zsc_fmrc_doc,
        lv_bomnumber     TYPE stko_api02-bom_no.

  FIELD-SYMBOLS: <lwa_rclst> TYPE zsc_fmrclst.
  FIELD-SYMBOLS: <lwa_rcbom> TYPE zsc_fmrcbom.

  DATA: lv_text      TYPE char100,
        lv_answer    TYPE c,
        ls_csap_mbom TYPE csap_mbom.

  PERFORM messages_initialize USING gs_variables-initiator
                                   c_log_subobject-create
                                   gs_rcdoc_infocus-x-rchdr.

  CONCATENATE TEXT-054 ' ' zsc_fmrchdr-stlal ' ' '?' INTO lv_text RESPECTING BLANKS.

  popup_to_confirm TEXT-007 lv_text c_true lv_answer.
  IF lv_answer EQ '1'  .

    MOVE-CORRESPONDING zsc_fmrchdr TO ls_csap_mbom.
    MOVE zsc_fmrchdr-datuv TO lv_datum.
    date_formt_ddmmyyyy lv_datum lv_datum.
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
*       t_stpo      = lt_bom_itm
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


    CALL FUNCTION 'CSAP_MAT_BOM_DELETE'
      EXPORTING
        material           = ls_csap_mbom-matnr
        plant              = ls_csap_mbom-werks
        bom_usage          = '1'
        alternative        = ls_csap_mbom-stlal
        valid_from         = lv_datum
        fl_commit_and_wait = 'X'
*    IMPORTING
*       fl_warning         =
      EXCEPTIONS
        error              = 1
        OTHERS             = 2.
    IF sy-subrc EQ 0.
      COMMIT WORK.
      CLEAR: ls_csap_mbom.
      MOVE: c_updkz_update TO gs_rcdoc_infocus-x-rchdr-updkz,
            space TO gs_rcdoc_infocus-x-rchdr-stlal.
      LOOP AT gs_rcdoc_infocus-x-rclst ASSIGNING <lwa_rclst> WHERE rcnum IS NOT INITIAL.
        MOVE c_updkz_delete TO <lwa_rclst>-updkz.
      ENDLOOP.
      LOOP AT gs_rcdoc_infocus-x-rcbom ASSIGNING <lwa_rcbom> WHERE rcnum IS NOT INITIAL
                                                             AND stlal EQ zsc_fmrchdr-stlal.
        MOVE c_updkz_delete TO <lwa_rcbom>-updkz.
      ENDLOOP.
      gs_rcdoc_infocus-y = CORRESPONDING #( gs_rcdoc_infocus-x ).
      PERFORM document_infocus_save USING c_true.
      CALL FUNCTION 'VRM_REFRESH_VALUES'.
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
