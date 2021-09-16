*----------------------------------------------------------------------*
***INCLUDE /AGRI/LFMACMNF0M .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  acdoc_DATA_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM acdoc_data_set .

  DATA: lwa_search_header TYPE zsc_fmachdr,
        lv_subrc          TYPE sy-subrc.

  SORT gt_search_header BY acnum.
  DELETE ADJACENT DUPLICATES FROM gt_search_header.
  gs_variables-refresh_worklist = c_true.
  DESCRIBE TABLE gt_search_header LINES sy-tfill.

  IF sy-tfill  EQ 1.
    READ TABLE gt_search_header INTO lwa_search_header INDEX sy-tfill
    TRANSPORTING acnum.
    PERFORM document_infocus_set USING    lwa_search_header-acnum
                                 CHANGING lv_subrc.
  ENDIF.

ENDFORM.                    " acdoc_DATA_SET
*&---------------------------------------------------------------------*
*&      Form  MESSAGES_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM messages_display  USING  lv_initiator TYPE /agri/gdescr.

  DATA: ls_variant TYPE disvariant.
  ls_variant-report = c_program_acm.
  ls_variant-handle = lv_initiator.

  messages_display lv_initiator c_true space space ls_variant.
  messages_init.
  CALL METHOD ref_process_log->context_data_set
    EXPORTING
      is_context_data = gs_context_data.
  CLEAR: gs_variables-initiator.

ENDFORM.                    " MESSAGES_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  MDOC_TYPE_CONTROL_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM acdoc_type_control_read  USING lv_actyp
                          CHANGING lv_subrc.

  CHECK gs_variables-actyp_in_focus NE lv_actyp.

  SELECT SINGLE * FROM ztfmactyp INTO gs_tfmactyp
          WHERE actyp EQ lv_actyp.
  IF sy-subrc NE 0.
    lv_subrc = 4.
    MESSAGE ID 'ZFMAC' TYPE 'E' NUMBER '035' WITH lv_actyp
                                                  INTO sy-msgli.
    message_simple space.
    gs_variables-errors = c_true.
    EXIT.
  ENDIF.
ENDFORM.                    " MDOC_TYPE_CONTROL_READ
*&---------------------------------------------------------------------*
*&      Form  MEASUREMENT_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM measurement_create .

  DATA: lv_txtgr TYPE txtgr.

  PERFORM document_data_initialize USING c_true.
  gs_variables-document_mode = c_mode_create.
  gs_acdoc_infocus-x-achdr-updkz = c_updkz_new.
  gs_acdoc_infocus-acnum = TEXT-046.
  gs_acdoc_infocus-x-achdr-acnum = TEXT-046.
  CLEAR ts_items-activetab.
  gs_variables-refresh_items_grid = c_true.
*  CLEAR: /agri/s_glflcma.

ENDFORM.                    " MEASUREMENT_CREATE
*&---------------------------------------------------------------------*
*&      Form  acdoc_INFOCUS_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM acdoc_infocus_save CHANGING lv_subrc TYPE sy-subrc
                                 lt_acdoc TYPE zt_fmac_doc.

  DATA: lt_messages TYPE /agri/t_gprolog,
        ls_message  TYPE /agri/s_gprolog.

  PERFORM document_check USING c_true
                      CHANGING lv_subrc.
  IF lv_subrc NE 0.
    EXIT.
  ENDIF.

  IF gs_variables-document_mode EQ c_mode_create.
*    PERFORM split_measurements_for_terrain CHANGING lt_acdoc.
    IF lt_acdoc IS INITIAL.
      lv_subrc = 4.
      MESSAGE ID '/AGRI/FMAC' TYPE 'E' NUMBER '054'
                                         INTO sy-msgli.
      message_simple space.
      EXIT.
    ENDIF.
  ELSE.
  ENDIF.

  CALL FUNCTION 'ZFMAC_SAVE_SINGLE'
    EXPORTING
*     I_SET_UPDATE_TASK = 'X'
*     I_COMMIT_WORK     = 'X'
      iref_text   = ref_text
    CHANGING
      cs_acdoc    = gs_acdoc_infocus
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
      MESSAGE ID 'ZFMAC' TYPE c_msg_type-error NUMBER '011'
                              WITH gs_acdoc_infocus-x-achdr-acnum
                              INTO sy-msgli.
      message_simple space.
    ENDIF.
  ELSE.
    LOOP AT lt_acdoc INTO gs_acdoc_infocus.
      PERFORM message_context_set USING gs_acdoc_infocus-x-achdr.
      MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-success NUMBER '006'
         WITH gs_acdoc_infocus-x-achdr-acnum INTO sy-msgli.
      message_simple space.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " acdoc_INFOCUS_SAVE
*&---------------------------------------------------------------------*
*&      Form  MESSAGES_INITIALIZE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM messages_initialize  USING lv_initiator TYPE /agri/gdescr
                                lv_subobject TYPE balsubobj
                                zsc_fmachdr   TYPE zsc_fmachdr.

  DATA: ls_context TYPE /agri/s_fmac_context.

  messages_init.
  messages_collect_all.
  messages_initiator_set lv_initiator c_object-log lv_subobject.

****Set the context for Process Log
  IF NOT zsc_fmachdr IS INITIAL.
    MOVE-CORRESPONDING zsc_fmachdr TO ls_context.
    messages_context_data_set zsc_fmachdr-acnum space space
                              'ZCS_FMAC_CONTEXT' ls_context.
  ENDIF.

ENDFORM.                    " MESSAGES_INITIALIZE
*&---------------------------------------------------------------------*
*&      Form  message_context_set
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM message_context_set USING lwa_achdr TYPE zsc_fmachdr.

  DATA: lwa_context TYPE /agri/s_fmac_context.

  IF lwa_achdr IS NOT INITIAL.
    MOVE-CORRESPONDING lwa_achdr TO lwa_context.
    messages_context_data_set lwa_achdr-acnum
                                  space space
                                  'ZSC_FMAC_CONTEXT' lwa_context.
  ENDIF.

ENDFORM.                    "message_context_set
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  MD_HEADER_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ac_header_update  USING ls_old_header TYPE zsc_fmachdr
                       CHANGING ls_header  TYPE zsc_fmachdr
                                lv_subrc   TYPE sy-subrc.

  IF ls_old_header-actyp NE ls_header-actyp.
    PERFORM authority_check USING ls_header
                                  c_authorization_activity-create
                                  c_msg_type-error
                         CHANGING lv_subrc.
    IF lv_subrc <> 0.
      CLEAR gs_variables-document_mode.
      EXIT.
    ENDIF.

    PERFORM acdoc_type_control_read USING ls_header-actyp
                                CHANGING lv_subrc.
    IF lv_subrc NE 0.
      EXIT.
    ENDIF.
    MOVE-CORRESPONDING gs_fmactyp TO ls_header.
    PERFORM text_maintain USING ls_header-txtgr
                                c_object-text_object
                       CHANGING gs_variables-data_changed.
  ENDIF.

ENDFORM.                    " MD_HEADER_UPDATE
*&---------------------------------------------------------------------*
*&      Form  MD_HEADER_DATA_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ac_header_data_check CHANGING ls_achdr TYPE zsc_fmachdr
                                   lv_subrc TYPE sy-subrc.

ENDFORM.                    " MD_HEADER_DATA_CHECK
*&---------------------------------------------------------------------*
*&      Form  md_fcode_processing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ac_fcode_processing.

  DATA: lv_prefix TYPE char50.

  CHECK sy-ucomm CS 'ZFMAC_'.
  SPLIT sy-ucomm AT '_' INTO lv_prefix sy-ucomm.

  fcode_execute sy-ucomm /agri/saplfmacm.
  CLEAR: ok_code, sy-ucomm.

ENDFORM.                    "md_fcode_processing
