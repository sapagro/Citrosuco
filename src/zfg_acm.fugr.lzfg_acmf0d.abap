*&---------------------------------------------------------------------*
*& Include          LZFG_ACMF0D
*&---------------------------------------------------------------------*
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
    PERFORM document_infocus_unlock USING gs_acdoc_infocus-accom.
    PERFORM document_data_initialize USING c_true.
    gs_variables-document_mode = c_mode_display.
  ENDIF.
ENDFORM.                    " DOCUMENT_INFOCUS_CLEAR
*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_INFOCUS_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM document_infocus_save  USING   lv_set_infocus.

  DATA: lt_acdoc     TYPE /agri/t_fmacs_doc,
        lt_messages  TYPE /agri/t_gprolog,
        ls_message   TYPE /agri/s_gprolog,
        ls_variant   TYPE disvariant,
        lv_subrc     TYPE sysubrc,
        lv_stop_save.

  CHECK gs_variables-errors IS INITIAL.

  gs_variables-initiator = c_log_initiator-save.
  PERFORM messages_initialize USING gs_variables-initiator
                                    c_log_subobject-save
                                    gs_acdoc_infocus-x-achdr.

  PERFORM field_value_conversions USING '2'.

*--call badi Agreement check
  PERFORM badi_document_check USING gs_acdoc_infocus
                              CHANGING lv_stop_save.

**  PERFORM status_outcome_set USING gs_irdoc_infocus-x-irhdr-objnr.

  IF lv_stop_save IS NOT INITIAL.
    gs_variables-errors = abap_true.
    PERFORM messages_display USING gs_variables-initiator.
    EXIT.
  ENDIF.

  APPEND gs_acdoc_infocus TO lt_acdoc.
  CALL FUNCTION '/AGRI/FMAC_SAVE'
    EXPORTING
*     I_SET_UPDATE_TASK = 'X'
*     I_COMMIT_WORK     = 'X'
      iref_text   = ref_text
    CHANGING
      ct_acdoc    = lt_acdoc
      ct_messages = lt_messages
    EXCEPTIONS
      no_change   = 1
      OTHERS      = 2.

  lv_subrc  = sy-subrc.
  LOOP AT lt_messages INTO ls_message.
    MESSAGE ID ls_message-msgid TYPE ls_message-msgty
       NUMBER ls_message-msgno WITH ls_message-msgv1
       ls_message-msgv2 ls_message-msgv3 ls_message-msgv4
                  INTO sy-msgli.
    message_simple space.
  ENDLOOP.

  IF lv_subrc NE 0.
    IF lv_subrc EQ 1.
      MESSAGE ID '/AGRI/GLOBAL' TYPE c_msg_type-success NUMBER '322'
                                                        INTO sy-msgli.
      message_simple space.
    ELSE.
***Extended Additional Syntax Check ATC  1709 PQ
      MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error NUMBER '001'
                                                        INTO sy-msgli. "#EC MG_MISSING
****
      message_simple space.
    ENDIF.
  ELSE.
    READ TABLE lt_acdoc INTO gs_acdoc_infocus INDEX 1.
    PERFORM accom_tskord_save.
    MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-success NUMBER '007'
                            WITH gs_acdoc_infocus-x-achdr-accom INTO sy-msgli.
    message_simple space.
  ENDIF.

  IF gs_variables-external IS INITIAL.
    PERFORM messages_display USING gs_variables-initiator.
  ENDIF.

  PERFORM worklist_update.
  CLEAR gs_variables-data_changed.

  IF lv_set_infocus EQ c_true.
    PERFORM document_infocus_set USING    gs_acdoc_infocus-x-achdr-accom
                                 CHANGING lv_subrc.
  ENDIF.

ENDFORM.                    " DOCUMENT_INFOCUS_SAVE
*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_INFOCUS_SET
*&---------------------------------------------------------------------*
FORM document_infocus_set USING VALUE(lv_accom) TYPE /agri/fmaccom
                          CHANGING    lv_subrc.

  DATA: lv_activity(2) TYPE c,
**** ESP6 Task #30035 - Global Text Engine Integration
*        lv_txtgr  TYPE txtgr,
        lv_txtgr       TYPE /agri/gtxtgr,
****
        ls_fmactyp     TYPE /agri/tfmactyp.

  CLEAR lv_subrc.
  PERFORM document_data_initialize USING c_true.

  CHECK  lv_accom IS NOT INITIAL.

  PERFORM document_infocus_read USING lv_accom.

  gs_variables-document_mode = gs_variables-overview_mode.

  IF gs_variables-document_mode = c_mode_display.
    lv_activity = c_authorization_activity-display.
  ELSEIF
    lv_activity = c_authorization_activity-change.
  ENDIF.

  PERFORM authority_check USING gs_acdoc_infocus-x-achdr
                                lv_activity
                                c_true
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
    PERFORM document_infocus_lock USING lv_accom
                               CHANGING lv_subrc.
    IF lv_subrc NE 0.
      gs_variables-document_mode =
      gs_variables-overview_mode = c_mode_display.
    ENDIF.
  ENDIF.

  CLEAR gs_variables-actyp_in_focus.
  PERFORM accom_type_control_read
                  USING gs_acdoc_infocus-x-achdr-actyp.
  IF gs_variables-errors IS NOT INITIAL.
    CLEAR: gs_variables-document_mode,
           gs_acdoc_infocus, /agri/s_fmachdr.
    EXIT.
  ENDIF.

  IF gs_tfmactyp-txtgr IS NOT INITIAL.
    lv_txtgr = gs_tfmactyp-txtgr.
  ENDIF.

  IF ref_text IS NOT INITIAL.
    CALL METHOD ref_text->init.
    FREE ref_text.
  ENDIF.

  PERFORM text_maintain USING lv_txtgr
                              c_object-text_object
                     CHANGING gs_variables-document_changed.
**
  PERFORM notes_refresh USING gs_acdoc_infocus-x-achdr-accom.

  IF gs_acdoc_infocus-x-achdr-status EQ c_process_status-del.
    gs_variables-document_mode = c_mode_display.
    gs_variables-overview_mode = c_mode_display.
  ENDIF.


  gs_variables-refresh_grid_desc = c_true.
  gs_variables-refresh_items_grid = c_true.
  gs_variables-refresh_postings_grid = c_true.

ENDFORM.                    " DOCUMENT_INFOCUS_SET
*&---------------------------------------------------------------------*
*&      Form  document_data_initialize
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LV_REFRESH_MESSAGES  text
*----------------------------------------------------------------------*
FORM document_data_initialize  USING lv_refresh_messages.

  object_refresh_all.
  CLEAR: gs_acdoc_infocus,
         gs_variables-document_mode,
         gs_variables-document_changed,
         gs_variables-manual_changes,
         gs_variables-errors,
         gs_variables-copy,
         gs_tabstrip_captions,
         gs_tfmactyp,
         /agri/s_fmachdr,
         p_actyp,
         p_accom,
         p_bukrs,
         p_werks,
         p_strdat,
         p_findat,
         p_wonum,
         so_aufnr,
         so_aufnr[],
         p_descr.

  REFRESH: gt_items_layout,
           gt_details,
           gt_items_layout,
           gt_ac_desc_layout,
           gr_aufnrtmp[],
           gr_aufnr[].


  IF NOT lv_refresh_messages IS INITIAL.
    messages_init.
  ENDIF.

*--Refresh ALV's
  gs_variables-refresh_items_grid           = c_true.
  gs_variables-refresh_postings_grid        = c_true.
  gs_variables-refresh_additional_data_grid = c_true.
  gs_variables-refresh_grid_desc            = c_true.

ENDFORM.                    " document_data_initialize
*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_INFOCUS_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_ACCOM  text
*----------------------------------------------------------------------*
FORM document_infocus_read  USING    lv_accom.

  DATA: lt_accon  TYPE /agri/t_fmacom,
        lt_ac_doc TYPE /agri/t_fmacs_doc.

  DATA:
    lwa_acm_ord TYPE zabs_acm_ord,
    lt_acm_ord  TYPE STANDARD TABLE OF zabs_acm_ord,
    lwa_details TYPE ty_details.

  APPEND lv_accom TO lt_accon.

  CALL FUNCTION '/AGRI/FMAC_VIEW'
    EXPORTING
      it_accom       = lt_accon
    IMPORTING
      et_acdoc       = lt_ac_doc
    EXCEPTIONS
      no_data_exists = 1
      OTHERS         = 2.
  IF sy-subrc EQ 0.
    READ TABLE lt_ac_doc INTO gs_acdoc_infocus INDEX 1.

    CALL FUNCTION 'ZABS_FM_ORD_GET'
      EXPORTING
        iv_accom  = gs_acdoc_infocus-accom
      TABLES
        t_acm_ord = lt_acm_ord.

    LOOP AT lt_acm_ord INTO lwa_acm_ord.
      CLEAR lwa_details.
      MOVE-CORRESPONDING lwa_acm_ord TO lwa_details.
      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
        EXPORTING
          input  = lwa_details-tplnr_fl
        IMPORTING
          output = lwa_details-tplnr_fl.
      APPEND lwa_details TO gt_details.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " DOCUMENT_INFOCUS_READ
*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_INFOCUS_LOCK
*&---------------------------------------------------------------------*
FORM document_infocus_lock  USING    lv_accom TYPE /agri/fmaccom
                            CHANGING lv_subrc.
  DATA: lv_msgv1 LIKE sy-msgv1,
        lv_msgli TYPE sy-msgli.

  CHECK NOT lv_accom IS INITIAL.

  CALL FUNCTION 'ENQUEUE_/AGRI/EZ_FMAC'
    EXPORTING
      accom          = lv_accom
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
    lv_subrc = sy-subrc.
    lv_msgv1 = sy-msgv1.
****Document &1 is locked by User &2.
    SET CURSOR FIELD 'P_ACCOM'.
    MESSAGE i017(/agri/fmac) WITH lv_accom lv_msgv1 INTO lv_msgli.
    sy-msgli = lv_msgli.
    message_simple space.
  ENDIF.

ENDFORM.                    " DOCUMENT_INFOCUS_LOCK
*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_INFOCUS_LOCK
*&---------------------------------------------------------------------*
FORM document_infocus_lock_ac  USING    lv_accom TYPE /agri/fmaccom
                            CHANGING lv_subrc.
  DATA: lv_msgv1 LIKE sy-msgv1,
        lv_msgli TYPE sy-msgli.

  CHECK NOT lv_accom IS INITIAL.

  CALL FUNCTION 'ENQUEUE_/AGRI/EZ_FMAC'
    EXPORTING
      accom          = lv_accom
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
    lv_subrc = sy-subrc.
    lv_msgv1 = sy-msgv1.
****Document &1 is locked by User &2.

    MESSAGE e017(/agri/fmac) WITH lv_accom lv_msgv1 INTO lv_msgli.
    sy-msgli = lv_msgli.
    message_simple space.
  ENDIF.

ENDFORM.                    " DOCUMENT_INFOCUS_LOCK
*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_INFOCUS_UNLOCK
*&---------------------------------------------------------------------*
FORM document_infocus_unlock  USING lv_accom.
  CHECK lv_accom IS NOT INITIAL.

  CALL FUNCTION 'DEQUEUE_/AGRI/EZ_FMAC'
    EXPORTING
      accom = lv_accom.

ENDFORM.                    " DOCUMENT_INFOCUS_UNLOCK
*&---------------------------------------------------------------------*
*&      Form  DESCRIPTIONS_DISPLAY
*&---------------------------------------------------------------------*
FORM descriptions_display  USING  lv_screen TYPE sydynnr.

  IF lv_screen IS INITIAL.
    lv_screen = sy-dynnr.
  ENDIF.

*-- Fetch the Turma Description
  IF /agri/s_fmachdr-zzturma IS NOT INITIAL.
    SELECT SINGLE turma_text
      FROM zfmfpgrouphdr
      INTO zabs_s_ac_scr_fields-turma_desc
     WHERE turma_id EQ /agri/s_fmachdr-zzturma.
  ENDIF.

ENDFORM.                    " DESCRIPTIONS_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  DESC_DATA_FILL
*&---------------------------------------------------------------------*
FORM desc_data_fill .
  FIELD-SYMBOLS: <lwa_acdesc> TYPE /agri/s_fmachdrt.
  DATA: lwa_acdesc TYPE /agri/s_fmachdrt.

  CHECK gs_acdoc_infocus IS NOT INITIAL.

  gs_variables-refresh_grid_desc = c_true.

  IF /agri/s_fmachdr-descr IS INITIAL.
    READ TABLE gs_acdoc_infocus-y-acdes ASSIGNING <lwa_acdesc>
                                      WITH KEY spras = sy-langu
                                               accom = gs_acdoc_infocus-accom.
    IF sy-subrc EQ 0.
      <lwa_acdesc>-updkz = c_updkz_delete.
    ENDIF.
    DELETE gs_acdoc_infocus-x-acdes WHERE spras EQ sy-langu.
  ELSE.

    READ TABLE gs_acdoc_infocus-x-acdes ASSIGNING <lwa_acdesc>
                                        WITH KEY spras = sy-langu.

    IF sy-subrc EQ 0.
      <lwa_acdesc>-descr = /agri/s_fmachdr-descr.
      IF <lwa_acdesc>-updkz NE c_updkz_new.
        <lwa_acdesc>-updkz = c_updkz_update.
      ENDIF.
    ELSE.
      lwa_acdesc-spras = sy-langu.
      lwa_acdesc-descr = /agri/s_fmachdr-descr.
      lwa_acdesc-accom = /agri/s_fmachdr-accom.
      lwa_acdesc-updkz = c_updkz_new.
      APPEND lwa_acdesc TO gs_acdoc_infocus-x-acdes.
    ENDIF.
  ENDIF.

  gs_variables-refresh_grid_desc = c_true.
ENDFORM.                    " DESC_DATA_FILL
*&---------------------------------------------------------------------*
*&      Form  desc_data_fill_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM desc_data_fill_1.
  FIELD-SYMBOLS: <lwa_acdesc> TYPE /agri/s_fmachdrt.
  DATA: lwa_acdesc TYPE /agri/s_fmachdrt.

  CHECK gs_acdoc_infocus IS NOT INITIAL.

  gs_variables-refresh_grid_desc = c_true.

  IF p_descr IS INITIAL.
    READ TABLE gs_acdoc_infocus-y-acdes ASSIGNING <lwa_acdesc>
                                      WITH KEY spras = sy-langu
                                               accom = gs_acdoc_infocus-accom.
    IF sy-subrc EQ 0.
      <lwa_acdesc>-updkz = c_updkz_delete.
    ENDIF.
    DELETE gs_acdoc_infocus-x-acdes WHERE spras EQ sy-langu.
  ELSE.

    READ TABLE gs_acdoc_infocus-x-acdes ASSIGNING <lwa_acdesc>
                                        WITH KEY spras = sy-langu.

    IF sy-subrc EQ 0.
      <lwa_acdesc>-descr = p_descr.
      IF <lwa_acdesc>-updkz NE c_updkz_new.
        <lwa_acdesc>-updkz = c_updkz_update.
      ENDIF.
    ELSE.
      lwa_acdesc-spras = sy-langu.
      lwa_acdesc-descr = p_descr.
      lwa_acdesc-accom = p_accom.
      lwa_acdesc-updkz = c_updkz_new.
      APPEND lwa_acdesc TO gs_acdoc_infocus-x-acdes.
    ENDIF.
  ENDIF.

  gs_variables-refresh_grid_desc = c_true.
ENDFORM.                    " DESC_DATA_FILL
*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_INFOCUS_PREPARE
*&---------------------------------------------------------------------*
FORM document_infocus_prepare.

  DATA: lv_subrc TYPE sysubrc,
        lv_txtgr TYPE txtgr.

  CHECK gs_variables-document_mode EQ c_mode_create.
  gs_acdoc_infocus-updkz = c_updkz_new.
  IF gs_acdoc_infocus-x-achdr-accom IS INITIAL.
    gs_acdoc_infocus-accom = TEXT-046.
    gs_acdoc_infocus-x-achdr-accom = TEXT-046.
  ELSE.
    gs_acdoc_infocus-accom = gs_acdoc_infocus-x-achdr-accom.
  ENDIF.

*--Call Authority Check
  PERFORM authority_check USING gs_acdoc_infocus-x-achdr
                                c_authorization_activity-create
                                c_true
                       CHANGING lv_subrc.

  IF lv_subrc NE 0.
    CLEAR: gs_variables-document_mode,
           gs_acdoc_infocus, /agri/s_fmachdr.
    gs_variables-errors = c_true.
    EXIT.
  ENDIF.

****Accomplishment
  CLEAR gs_variables-actyp_in_focus.
  PERFORM accom_type_control_read USING /agri/s_fmachdr-actyp.
  IF gs_variables-errors IS NOT INITIAL.
    CLEAR: gs_variables-document_mode,
           gs_acdoc_infocus, /agri/s_fmachdr.
    EXIT.
  ENDIF.
  gs_acdoc_infocus-x-achdr-updkz = c_updkz_new.
  gs_acdoc_infocus-x-achdr-stsma = gs_tfmactyp-stsma.

  IF ref_text IS NOT INITIAL.
    CALL METHOD ref_text->init.
    FREE ref_text.
  ENDIF.

  PERFORM text_maintain USING gs_tfmactyp-txtgr
                              c_object-text_object
                     CHANGING gs_variables-document_changed.

  PERFORM notes_refresh USING gs_acdoc_infocus-x-achdr-accom.
**
******Status object
**  gs_acdoc_infocus-x-achdr-stsma = gs_tfmirtyp-stsma.
**  PERFORM status_object_create
**                         USING gs_acdoc_infocus-x-achdr-stsma
**                      CHANGING   gs_acdoc_infocus-x-achdr-objnr.
**  PERFORM status_update.

  CLEAR ts_items-activetab.

ENDFORM.                    " DOCUMENT_INFOCUS_PREPARE
*&---------------------------------------------------------------------*
*&      Form  DISTRIBUTED_INFOCUS_SAVE
*&---------------------------------------------------------------------*
FORM distributed_infocus_save   USING lv_post
                                CHANGING lv_subrc TYPE sy-subrc.

  DATA: lt_acdoc    TYPE /agri/t_fmacs_doc,
        lt_messages TYPE /agri/t_gprolog,
        ls_message  TYPE /agri/s_gprolog.

  PERFORM field_value_conversions USING '2'.
  CHECK gs_variables-errors IS INITIAL.
  IF lv_post IS NOT INITIAL.
    PERFORM distributed_process CHANGING lv_subrc.
    IF lv_subrc IS NOT INITIAL.
      EXIT.
    ENDIF.
  ENDIF.

  APPEND gs_acdoc_infocus TO lt_acdoc.

  CALL FUNCTION '/AGRI/FMAC_SAVE'
    EXPORTING
*     I_SET_UPDATE_TASK = 'X'
*     I_COMMIT_WORK     = 'X'
      iref_text   = ref_text
    CHANGING
      ct_acdoc    = lt_acdoc
      ct_messages = lt_messages
    EXCEPTIONS
      no_change   = 1
      OTHERS      = 2.

  lv_subrc  = sy-subrc.
  LOOP AT lt_messages INTO ls_message.
    MESSAGE ID ls_message-msgid TYPE ls_message-msgty
       NUMBER ls_message-msgno WITH ls_message-msgv1
       ls_message-msgv2 ls_message-msgv3 ls_message-msgv4
                  INTO sy-msgli.
    message_simple space.
  ENDLOOP.

  READ TABLE lt_acdoc INTO gs_acdoc_infocus INDEX 1.
  IF lv_subrc NE 0.
    IF lv_subrc EQ 1.
      MESSAGE ID '/AGRI/GLOBAL' TYPE c_msg_type-success NUMBER '322'
                                                        INTO sy-msgli.
      message_simple space.
    ELSE.
      MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error NUMBER '011'
                                                      INTO sy-msgli.
      message_simple space.
    ENDIF.
  ELSE.
    MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-success NUMBER '054'
                            WITH gs_acdoc_infocus-x-achdr-accom INTO
                            sy-msgli.
    message_simple space.
  ENDIF.

ENDFORM.                    " DISTRIBUTED_INFOCUS_SAVE
*&---------------------------------------------------------------------*
*&      Form  DISTRIBUTED_PROCESS
*&---------------------------------------------------------------------*
FORM distributed_process  CHANGING lv_subrc.

  DATA: lt_activity  TYPE TABLE OF /agri/fmacact,
        lwa_activity TYPE /agri/fmacact,
        lwa_distr    TYPE /agri/s_fmac_disres,
        lt_distr     TYPE /agri/t_fmac_disres,
        lwa_diseqr   TYPE /agri/s_fmac_disres,
        lt_diseqr    TYPE /agri/t_fmac_disres,
        lwa_message  TYPE /agri/s_gprolog,
        lt_messages  TYPE /agri/t_gprolog,
        lwa_context  TYPE /agri/s_fmfp_context,
        lv_msg_dummy TYPE c LENGTH 1,
        lt_acitm_tmp TYPE /agri/t_fmacitm,
        lv_tabix     TYPE sytabix,
        lv_dif       TYPE /agri/fmdistr,
        lv_yield     TYPE /agri/fmyield.
*        lv_subrc2    TYPE sy-subrc VALUE 0.

  FIELD-SYMBOLS: <lt_detfct>      LIKE gt_details_fcat,
                 <lt_itmtmp>      TYPE /agri/t_fmacitm,
                 <lwa_acitm>      TYPE /agri/s_fmacitm,
                 <lwa_acitm_tmp>  TYPE /agri/s_fmacitm,
                 <lwa_distr>      TYPE /agri/s_fmac_disres,
                 <lwa_diseqr>     TYPE /agri/s_fmac_disres,
                 <lwa_distr2>     TYPE /agri/s_fmac_disres,
                 <lwa_diseqr2>    TYPE /agri/s_fmac_disres,
                 <lwa_detail>     TYPE ty_details,
                 <lwa_detail_tmp> TYPE /agri/s_fmoc_details.

  IF gs_acdoc_infocus-x-acitm IS NOT INITIAL.
    ASSIGN gs_acdoc_infocus-x-acitm TO <lt_itmtmp>.
    SORT <lt_itmtmp> BY aufnr.
  ELSE.
    lv_subrc = 4.
  ENDIF.

  IF <lt_itmtmp> IS ASSIGNED.
    SELECT * FROM /agri/fmacact               "#EC CI_FAE_LINES_ENSURED
      INTO TABLE lt_activity
      FOR ALL ENTRIES IN <lt_itmtmp>
      WHERE idactv = <lt_itmtmp>-idactvl
         OR idactv = <lt_itmtmp>-idactve.
    SORT lt_activity BY idactv.
  ENDIF.

  IF gt_details IS NOT INITIAL.
    ASSIGN gt_details TO <lt_detfct>.
    SORT <lt_detfct> BY aufnr.
    DELETE ADJACENT DUPLICATES FROM <lt_detfct>
                          COMPARING aufnr.
  ELSE.
    lv_subrc = 4.
  ENDIF.

  IF lv_subrc EQ 0.

    PERFORM distribution_check TABLES <lt_itmtmp> <lt_detfct>
                             CHANGING lv_subrc.
  ENDIF.

  IF lv_subrc EQ 0.
    LOOP AT <lt_itmtmp> ASSIGNING <lwa_acitm>.
      CLEAR lv_yield.
      LOOP AT gt_details_tmp ASSIGNING <lwa_detail_tmp> WHERE aufnr = <lwa_acitm>-aufnr.
        lv_yield = lv_yield + <lwa_detail_tmp>-menge.
      ENDLOOP.
      <lwa_acitm>-yield = lv_yield.
      READ TABLE <lt_detfct> TRANSPORTING NO FIELDS
                      WITH KEY aufnr = <lwa_acitm>-aufnr
                      BINARY SEARCH.
      IF sy-subrc EQ 0.
        lv_tabix = sy-tabix.
        IF <lwa_acitm>-status NE c_process_status-cnf.
          lv_subrc = 4.
          MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                               NUMBER 025 INTO sy-msgli.
          message_simple space.
        ELSE.
          MOVE-CORRESPONDING <lwa_acitm> TO lwa_distr.
          MOVE-CORRESPONDING <lwa_acitm> TO lwa_diseqr.
          CLEAR:  lwa_distr-idactve.
          CLEAR:  lwa_diseqr-idactvl.
          COLLECT lwa_distr  INTO lt_distr.
          COLLECT lwa_diseqr INTO lt_diseqr.
        ENDIF.
      ELSE.
        lv_subrc = 4.
        MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                             NUMBER 025 INTO sy-msgli.
        message_simple space.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lv_subrc EQ 0.
      gs_acdoc_infocus-x-achdr-status = c_process_status-dis .
      IF gs_acdoc_infocus-x-achdr-updkz NE c_updkz_new.
        gs_acdoc_infocus-x-achdr-updkz = c_updkz_update.
      ENDIF.
      LOOP AT gs_acdoc_infocus-x-acitm ASSIGNING <lwa_acitm>.
        CLEAR lwa_activity.
        IF <lwa_acitm>-idactvl IS NOT INITIAL.
          READ TABLE lt_activity INTO lwa_activity
                      WITH KEY idactv = <lwa_acitm>-idactvl
                      BINARY SEARCH.
        ELSE.
          READ TABLE lt_activity INTO lwa_activity
                    WITH KEY idactv = <lwa_acitm>-idactve
                    BINARY SEARCH.
        ENDIF.
        IF lwa_activity-bill EQ 'YES'.
          <lwa_acitm>-status = c_process_status-dis.
          IF <lwa_acitm>-idresource IS NOT INITIAL.
            UNASSIGN <lwa_distr>.
            READ TABLE lt_distr ASSIGNING <lwa_distr>
                                WITH KEY aufnr   = <lwa_acitm>-aufnr
                                         idactvl = <lwa_acitm>-idactvl.

            IF sy-subrc EQ 0 AND <lwa_distr>-menge > 0.

              <lwa_acitm>-distr = ( <lwa_acitm>-yield * <lwa_acitm>-menge ) / <lwa_distr>-menge.
              <lwa_distr>-distr = <lwa_distr>-distr + <lwa_acitm>-distr.

            ENDIF.
          ELSE.
            IF <lwa_acitm>-equnr IS NOT INITIAL.
              UNASSIGN <lwa_diseqr>.
              READ TABLE lt_diseqr ASSIGNING <lwa_diseqr>
                                  WITH KEY aufnr   = <lwa_acitm>-aufnr
                                           idactvl = space
                                           idactve = <lwa_acitm>-idactve.

              IF sy-subrc EQ 0 AND <lwa_diseqr>-menge > 0.

                <lwa_acitm>-distr = ( <lwa_acitm>-yield * <lwa_acitm>-menge ) / <lwa_diseqr>-menge.
                <lwa_diseqr>-distr = <lwa_diseqr>-distr + <lwa_acitm>-distr.

              ENDIF.
            ENDIF.
          ENDIF.
        ELSE.
          <lwa_acitm>-status = c_process_status-dis.
        ENDIF.
        IF <lwa_acitm>-updkz NE c_updkz_new.
          <lwa_acitm>-updkz = c_updkz_update.
        ENDIF.
      ENDLOOP.

***    16/05/2016 distribution diference.
      lt_acitm_tmp = gs_acdoc_infocus-x-acitm.
      SORT lt_acitm_tmp DESCENDING BY idactvl menge.
      DELETE ADJACENT DUPLICATES FROM lt_acitm_tmp COMPARING idactvl idresource equnr.

      LOOP AT lt_acitm_tmp ASSIGNING <lwa_acitm_tmp>.

        IF <lwa_acitm_tmp>-idactvl IS NOT INITIAL.
          READ TABLE lt_activity INTO lwa_activity
                      WITH KEY idactv = <lwa_acitm_tmp>-idactvl
                      BINARY SEARCH.
        ELSE.
          READ TABLE lt_activity INTO lwa_activity
                    WITH KEY idactv = <lwa_acitm_tmp>-idactve
                    BINARY SEARCH.
        ENDIF.
        IF lwa_activity-bill EQ 'YES'.

          IF <lwa_acitm_tmp>-idresource IS NOT INITIAL.
            CLEAR lwa_distr.
            READ TABLE gs_acdoc_infocus-x-acitm ASSIGNING <lwa_acitm>
                        WITH KEY posnr = <lwa_acitm_tmp>-posnr.

            IF sy-subrc EQ 0.

              READ TABLE lt_distr INTO lwa_distr
                                WITH KEY aufnr   = <lwa_acitm_tmp>-aufnr
                                         idactvl = <lwa_acitm_tmp>-idactvl.

              IF lwa_distr-distr > <lwa_acitm_tmp>-yield.

                lv_dif = lwa_distr-distr - <lwa_acitm_tmp>-yield.
                <lwa_acitm>-distr = <lwa_acitm>-distr - lv_dif.

              ELSEIF lwa_distr-distr < <lwa_acitm_tmp>-yield.

                lv_dif = <lwa_acitm_tmp>-yield - lwa_distr-distr.
                <lwa_acitm>-distr = <lwa_acitm>-distr + lv_dif.

              ENDIF.
            ENDIF.
          ELSEIF <lwa_acitm_tmp>-equnr IS NOT INITIAL.

            CLEAR lwa_diseqr.
            READ TABLE gs_acdoc_infocus-x-acitm ASSIGNING <lwa_acitm>
                        WITH KEY posnr = <lwa_acitm_tmp>-posnr.

            IF sy-subrc EQ 0.

              READ TABLE lt_diseqr INTO lwa_diseqr
                                 WITH KEY aufnr   = <lwa_acitm_tmp>-aufnr
                                          idactvl = space
                                          idactve = <lwa_acitm_tmp>-idactve.

              IF lwa_diseqr-distr > <lwa_acitm_tmp>-yield.

                lv_dif = lwa_diseqr-distr - <lwa_acitm_tmp>-yield.
                <lwa_acitm>-distr = <lwa_acitm>-distr - lv_dif.

              ELSEIF lwa_diseqr-distr < <lwa_acitm_tmp>-yield.

                lv_dif = <lwa_acitm_tmp>-yield - lwa_diseqr-distr.
                <lwa_acitm>-distr = <lwa_acitm>-distr + lv_dif.

              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
***
    ENDIF.
  ELSE.
    MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                           NUMBER 059 INTO sy-msgli.
    message_simple space.

  ENDIF.

ENDFORM.                    " DISTRIBUTED_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DETAILS_DATA_DISPLAY
*&---------------------------------------------------------------------*
FORM details_data_display .
  DATA: lwa_details      TYPE ty_details,
        lwa_details_fcat TYPE ty_details,
        lwa_makt         TYPE makt.

  CHECK ref_grid_details IS INITIAL OR
        gs_variables-refresh_postings_grid IS NOT INITIAL.
  REFRESH: gt_details_fcat.

  LOOP AT gt_details INTO lwa_details.
    MOVE-CORRESPONDING lwa_details TO lwa_details_fcat.
    key_text_get 'MAKT' 'MATNR' lwa_details_fcat-matnr
                 lwa_makt lwa_details_fcat-maktx.
    APPEND lwa_details_fcat TO gt_details_fcat.
  ENDLOOP.
ENDFORM.                    " DETAILS_DATA_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  DESC_STYLES_PREPARE
*&---------------------------------------------------------------------*
FORM desc_styles_prepare CHANGING lwa_acdesc_layout
                             TYPE /agri/s_fmachdrt_layout_fcat.

  DATA: lwa_style TYPE lvc_s_styl.

  lwa_style-fieldname = 'SPRAS'.
  lwa_style-style = cl_gui_alv_grid=>mc_style_disabled.
  INSERT lwa_style INTO TABLE lwa_acdesc_layout-styles.

ENDFORM.                    " DESC_STYLES_PREPARE
*&---------------------------------------------------------------------*
*&      Form  DESC_GRID_UPDATE
*&---------------------------------------------------------------------*
FORM desc_grid_update.

  DATA: lwa_cmdesc        TYPE /agri/s_fmachdrt,
        lwa_cmdesc_layout TYPE /agri/s_fmachdrt_layout_fcat,
        lwa_mod_row       TYPE lvc_s_modi,
        lv_modified,
        lv_valid.

  FIELD-SYMBOLS: <lwa_cmdesc> TYPE /agri/s_fmachdrt.

  CHECK gs_variables-document_mode NE c_mode_display.
  CLEAR gs_variables-errors.

  IF ok_code EQ c_fcode-desc_delete.
    PERFORM fcode_desc_delete.
    CLEAR ok_code.
  ENDIF.

  lv_modified = ref_multi_lang_desc_grid->data_modified_check( ).
  IF lv_modified EQ c_true.
    CALL METHOD ref_multi_lang_desc_grid->check_changed_data
      IMPORTING
        e_valid = lv_valid.
    IF lv_valid IS INITIAL.
      CLEAR ok_code.
      EXIT.
    ELSE.
      gs_variables-refresh_grid_desc = c_true.
    ENDIF.

  ENDIF.

  PERFORM desc_update.

ENDFORM.                    " DESC_GRID_UPDATE
*&---------------------------------------------------------------------*
*&      Form  DESC_UPDATE
*&---------------------------------------------------------------------*
FORM desc_update.

  DATA: lwa_acdesc        TYPE /agri/s_fmachdrt,
        lwa_acdesc_layout TYPE /agri/s_fmachdrt_layout_fcat,
        lwa_mod_row       TYPE lvc_s_modi,
        lv_subrc.

  FIELD-SYMBOLS: <lwa_acdesc> TYPE /agri/s_fmachdrt.

  LOOP AT gt_desc_mod_rows INTO lwa_mod_row.

    READ TABLE gt_ac_desc_layout INTO lwa_acdesc_layout
                                INDEX lwa_mod_row-row_id.
    CHECK lwa_acdesc_layout IS NOT INITIAL.

    PERFORM desc_duplicates_check USING lwa_mod_row-row_id
                                        lwa_acdesc_layout
                                        lv_subrc.

    IF lv_subrc NE 0.
      gs_variables-errors = c_true.
      CONTINUE.
    ENDIF.

    MOVE-CORRESPONDING lwa_acdesc_layout TO lwa_acdesc.

    READ TABLE gs_acdoc_infocus-x-acdes ASSIGNING <lwa_acdesc>
                                         WITH KEY spras = lwa_acdesc_layout-spras
                                                  accom = lwa_acdesc_layout-accom.
    IF sy-subrc EQ 0.
      IF lwa_acdesc NE <lwa_acdesc>.
        MOVE lwa_acdesc TO <lwa_acdesc>.
        IF <lwa_acdesc>-updkz NE c_updkz_new.
          gs_variables-data_changed = c_true.
          <lwa_acdesc>-updkz = c_updkz_update.
        ENDIF.
**** updating header simultaneously
        IF sy-langu = lwa_acdesc-spras.
          gs_acdoc_infocus-x-achdr-descr = lwa_acdesc-descr.
        ENDIF.
      ENDIF.
    ELSE.
**** updating header simultaneously
      IF sy-langu = lwa_acdesc-spras.
        gs_acdoc_infocus-x-achdr-descr = lwa_acdesc-descr.
      ENDIF.
      gs_variables-refresh_grid_desc = c_true.
      lwa_acdesc_layout-accom = gs_acdoc_infocus-accom.
      lwa_acdesc_layout-updkz = c_updkz_new.
      MOVE-CORRESPONDING lwa_acdesc_layout TO lwa_acdesc.
      APPEND lwa_acdesc TO gs_acdoc_infocus-x-acdes.
    ENDIF.
  ENDLOOP.

  IF gs_variables-errors IS NOT INITIAL.
    CLEAR: ok_code, gs_variables-errors.
  ENDIF.

ENDFORM.                    " DESC_UPDATE
*&---------------------------------------------------------------------*
*&      Form  DESC_DUPLICATES_CHECK
*&---------------------------------------------------------------------*
FORM desc_duplicates_check USING lv_tabix          TYPE any
                                 lwa_acdesc_layout TYPE
                                            /agri/s_fmachdrt_layout_fcat
                                 lv_subrc.

  DATA: lwa_acdesc_tmp TYPE /agri/s_fmachdrt_layout_fcat,
        lv_cnt         TYPE i,
        lv_error.

  CLEAR lv_subrc.
  IF lwa_acdesc_layout-spras IS INITIAL.
    lv_error = c_true.
    MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                            NUMBER '008' INTO sy-msgli.
    message_simple space.
  ENDIF.

  LOOP AT gt_ac_desc_layout INTO lwa_acdesc_tmp
                            WHERE spras EQ lwa_acdesc_layout-spras.
    lv_cnt = lv_cnt + 1.
    IF lv_cnt GT 1.
      lv_error = c_true.
      MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                              NUMBER '009' INTO sy-msgli.
      message_simple space.
    ENDIF.
  ENDLOOP.

  IF lv_error IS INITIAL.
    DELETE gt_desc_mod_rows WHERE row_id EQ lv_tabix.
  ELSE.
    lv_subrc = 4.
  ENDIF.

ENDFORM.                    " DESC_DUPLICATES_CHECK
*&---------------------------------------------------------------------*
*&      Form  DISTRIBUTION_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LT_ITMTMP>  text
*      -->P_<LT_DETFCT>  text
*      <--P_LV_SUBRC  text
*----------------------------------------------------------------------*
FORM distribution_check  TABLES lt_itmtmp TYPE /agri/t_fmacitm
                                lt_detfct LIKE gt_details_fcat
                         CHANGING lv_subrc.

  FIELD-SYMBOLS: <lwa_itmtmp> TYPE /agri/s_fmacitm,
                 <lwa_detfct> TYPE ty_details.

  LOOP AT lt_detfct ASSIGNING <lwa_detfct>.
    LOOP AT lt_itmtmp ASSIGNING <lwa_itmtmp> WHERE aufnr = <lwa_detfct>-aufnr.
      IF <lwa_itmtmp>-status NE c_process_status-cnf.
        lv_subrc = 4.
        RETURN.
      ENDIF.
    ENDLOOP.
    IF sy-subrc NE 0.
      lv_subrc = 4.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " DISTRIBUTION_CHECK
*&---------------------------------------------------------------------*
*&      Form  DESCRIPTION_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LWA_ITEMS_LAYOUT  text
*----------------------------------------------------------------------*
FORM description_get CHANGING lwa_item_layout  TYPE /agri/s_fmacitm_layout.

  SELECT SINGLE pltxt
  FROM /agri/glflot
  INTO lwa_item_layout-pltxt
  WHERE tplnr_fl = lwa_item_layout-tplnr.

  SELECT SINGLE maktx
  FROM makt
  INTO lwa_item_layout-maktx
  WHERE matnr = lwa_item_layout-tmatnr.                 "#EC CI_NOORDER

  SELECT SINGLE ktext
  FROM crtx AS t1                                      "#EC CI_BUFFJOIN
  INNER JOIN crhd AS t2                                "#EC CI_BUFFJOIN
  ON t1~objid = t2~objid
  INTO lwa_item_layout-ktext
  WHERE t2~arbpl = lwa_item_layout-arbpl.               "#EC CI_NOORDER

  SELECT SINGLE t2~description
  INTO  lwa_item_layout-empdes
  FROM /agri/fmacact AS t1 INNER JOIN /agri/fmacactt AS t2 "#EC CI_BUFFJOIN
  ON t1~idactv = t2~idactv
  WHERE t1~idactv = lwa_item_layout-idactvl             "#EC CI_NOORDER
  AND t2~spras = sy-langu.

  SELECT SINGLE t2~description
  INTO  lwa_item_layout-eqmdes
  FROM /agri/fmacact AS t1 INNER JOIN /agri/fmacactt AS t2 "#EC CI_BUFFJOIN
  ON t1~idactv = t2~idactv
  WHERE t1~idactv = lwa_item_layout-idactve             "#EC CI_NOORDER
  AND t2~spras = sy-langu.

  SELECT SINGLE ejdesc
    INTO lwa_item_layout-zzejdesc
    FROM zabs_event_just
   WHERE ejust EQ lwa_item_layout-zzjust.

ENDFORM.                    " DESCRIPTION_GET

*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_AGREEMENT_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_FMACRES  text
*----------------------------------------------------------------------*
FORM document_agreement_get CHANGING lt_toagroup TYPE /agri/t_fmacoll.

  DATA: lwa_acitm         TYPE /agri/s_fmacitm,
        lt_agreem         TYPE /agri/t_fmacagr,
        lt_agrnum         TYPE /agri/t_fmknuma_ag,
        lwa_agree         TYPE /agri/s_fmacagr,
        lt_kona           TYPE  /agri/t_gkona,
        lt_agreements     TYPE  /agri/t_fmaghdr,
        lwa_agreements    TYPE  /agri/s_fmaghdr,
        lt_partners       TYPE  /agri/t_fmagptr,
        lt_task_materials TYPE  /agri/t_fmagtm,
        lt_func_locs      TYPE  /agri/t_fmagfl,
        lt_owners         TYPE  /agri/t_fmagown,
        lt_qualt_cond     TYPE  /agri/t_fmagqc,
        lt_ekkn           TYPE ty_ekkn,
        lwa_ekkn          TYPE  ekkn,
        lt_purch_order    TYPE  /agri/t_fmagxpo,
        lwa_purch_order   TYPE  /agri/s_fmagxpo,
        lt_sched_head     TYPE  /agri/t_fmagsch,
        lt_sched_date     TYPE  /agri/t_fmagscd,
        lv_subrc          TYPE  int4,
        lt_resb           TYPE  resb_t,
        lwa_resb          TYPE  resb,
        lwa_toagroup      TYPE  /agri/s_fmacoll,
        lt_sched_claim    TYPE  /agri/t_fmagsccr.


  PERFORM material_services_get CHANGING gs_acdoc_infocus-x-acitm
                                 lt_resb.
  LOOP AT gs_acdoc_infocus-x-acitm INTO lwa_acitm WHERE status EQ 'CTD'
                                                  AND intext EQ c_true.

    MOVE lwa_acitm-lifnr TO lwa_agree-parnr.

    READ TABLE lt_resb INTO lwa_resb WITH KEY aufnr = lwa_acitm-aufnr.
    IF sy-subrc EQ 0.
      MOVE lwa_resb-matnr  TO lwa_agree-matnr.
    ENDIF.
    APPEND lwa_agree       TO lt_agreem.
  ENDLOOP.

  CHECK lt_agreem IS NOT INITIAL
  AND lv_subrc EQ 0.
  PERFORM agreement_number_get USING lt_agreem
                               CHANGING lt_agrnum
                                        lv_subrc.
  CHECK lt_agrnum IS NOT INITIAL.
  CALL FUNCTION '/AGRI/FMAG_READ'
    EXPORTING
      it_knuma_ag       = lt_agrnum
    IMPORTING
      et_kona           = lt_kona
      et_agreements     = lt_agreements
      et_partners       = lt_partners
      et_task_materials = lt_task_materials
      et_func_locs      = lt_func_locs
      et_owners         = lt_owners
      et_qualt_cond     = lt_qualt_cond
      et_sched_head     = lt_sched_head
      et_sched_date     = lt_sched_date
      et_sched_claim    = lt_sched_claim
      et_purch_order    = lt_purch_order
    EXCEPTIONS
      no_data_exists    = 1
      OTHERS            = 2.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              INTO sy-msgli.
    message_simple space.
    EXIT.
  ELSE.
    CHECK lt_purch_order IS NOT INITIAL.
    PERFORM assign_account_get USING lt_purch_order
                               CHANGING lt_ekkn.

    LOOP AT gs_acdoc_infocus-x-acitm INTO lwa_acitm.

*--- Validate Period Agreements by line
      LOOP AT lt_agreements INTO lwa_agreements
          WHERE ( datab GE lwa_acitm-strtdat OR
                 datab LE lwa_acitm-findat )  AND
                ( datbi GE lwa_acitm-strtdat OR
                  datbi LE lwa_acitm-findat )
          AND  owner EQ lwa_acitm-lifnr
         AND bukrs EQ /agri/s_fmachdr-bukrs.
        MOVE-CORRESPONDING lwa_acitm TO lwa_toagroup.
        lwa_toagroup-bldat = lwa_toagroup-budat = lwa_acitm-strtdat.

        READ TABLE lt_resb INTO lwa_resb WITH KEY aufnr = lwa_acitm-aufnr.
        IF sy-subrc EQ 0.
          LOOP AT lt_purch_order INTO lwa_purch_order WHERE  knuma_ag = lwa_agreements-knuma_ag
                                                      AND    matnr    = lwa_resb-matnr.

            READ TABLE lt_ekkn INTO lwa_ekkn WITH KEY aufnr = lwa_acitm-aufnr
                                                      ebeln = lwa_purch_order-ebeln.
            CHECK sy-subrc EQ 0.
            MOVE lwa_ekkn-ebelp TO lwa_toagroup-ebelp.
            MOVE lwa_purch_order-ebeln TO lwa_toagroup-ebeln.
            MOVE lwa_resb-matnr        TO lwa_toagroup-matnr.
            MOVE lwa_agreements-knuma_ag TO lwa_toagroup-knuma_ag.
            COLLECT lwa_toagroup INTO lt_toagroup.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDIF.
ENDFORM.                    "document_agreement_get
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_PRODUCTION_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_/AGRI/S_FMFPHDR_AUFNR  text
*----------------------------------------------------------------------*
FORM display_production_order  USING  lv_aufnr TYPE aufnr.

  DATA: lwa_aufnr TYPE /agri/s_fmaufnr,
        lt_aufnr  TYPE /agri/t_fmaufnr.

  CHECK lv_aufnr IS NOT INITIAL.
  lwa_aufnr-aufnr = lv_aufnr.

  APPEND lwa_aufnr TO lt_aufnr.
  CALL FUNCTION '/AGRI/FMCO_PROCESS'
    EXPORTING
*     I_MODE                        = 'A'
*     I_DISPLAY_ONLY                = 'X'
      i_save                        = space
      it_aufnr                      = lt_aufnr
*     IT_FILTER                     =
*     IS_WSLINK                     =
    EXCEPTIONS
      enter_order_number            = 1
      invalid_parameter_combination = 2
      OTHERS                        = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  gs_variables-refresh_postings_grid = c_true.

ENDFORM.                    "display_production_order
