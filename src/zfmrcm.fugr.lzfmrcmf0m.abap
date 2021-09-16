*----------------------------------------------------------------------*
***INCLUDE /AGRI/LFMRCMNF0M .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  RCDOC_DATA_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM rcdoc_data_set .

  DATA: lwa_search_header TYPE zsc_fmrchdr,
        lv_subrc          TYPE sy-subrc.

  SORT gt_search_header BY rcnum.
  DELETE ADJACENT DUPLICATES FROM gt_search_header.
  gs_variables-refresh_worklist = c_true.
  DESCRIBE TABLE gt_search_header LINES sy-tfill.

  IF sy-tfill  EQ 1.
    READ TABLE gt_search_header INTO lwa_search_header INDEX sy-tfill
    TRANSPORTING rcnum.
    PERFORM document_infocus_set USING  gs_rckey
                                 CHANGING lv_subrc.
  ENDIF.

ENDFORM.                    " RCDOC_DATA_SET
*&---------------------------------------------------------------------*
*&      Form  MESSAGES_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM messages_display  USING  lv_initiator TYPE /agri/gdescr.

  DATA: ls_variant TYPE disvariant.
  ls_variant-report = c_program_rcm.
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
FORM rcdoc_type_control_read  USING lv_rctyp
                           CHANGING lv_subrc.

  CHECK gs_variables-rctyp_in_focus NE lv_rctyp.

  SELECT SINGLE *
    FROM ztfmrctyp
    INTO @gs_tfmrctyp
   WHERE rctyp EQ @lv_rctyp.

  IF sy-subrc NE 0.
    lv_subrc = 4.
    MESSAGE ID 'ZFMRC' TYPE 'E' NUMBER '035'
      WITH lv_rctyp INTO sy-msgli.
    message_simple space.
    gs_variables-errors = c_true.
    EXIT.
  ENDIF.

ENDFORM.                    " MDOC_TYPE_CONTROL_READ
*&---------------------------------------------------------------------*
*&      Form  RCDOC_INFOCUS_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM rcdoc_infocus_save CHANGING lv_subrc TYPE sy-subrc
                                 lt_rcdoc TYPE zt_fmrc_doc.

  DATA: lt_messages TYPE /agri/t_gprolog,
        ls_message  TYPE /agri/s_gprolog.

  gs_variables-initiator =  c_log_initiator-check.
  PERFORM messages_initialize USING  gs_variables-initiator
                                     c_log_subobject-save
                                     gs_rcdoc_infocus-x-rchdr.
  PERFORM document_check USING c_true
                      CHANGING lv_subrc.
  IF lv_subrc NE 0.
    EXIT.
  ENDIF.

  CALL FUNCTION 'ZFMRC_SAVE_SINGLE'
    EXPORTING
*     I_SET_UPDATE_TASK = 'X'
*     I_COMMIT_WORK     = 'X'
      iref_text   = ref_text
    CHANGING
      cs_rcdoc    = gs_rcdoc_infocus
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
      MESSAGE ID 'ZFMRC' TYPE c_msg_type-error NUMBER '011'
                              WITH gs_rcdoc_infocus-x-rchdr-rcnum
                              INTO sy-msgli.
      message_simple space.
    ENDIF.
  ELSE.
    PERFORM message_context_set USING gs_rcdoc_infocus-x-rchdr.
    MESSAGE ID 'ZFMRC' TYPE c_msg_type-success NUMBER '006'
       WITH gs_rcdoc_infocus-x-rchdr-rcnum INTO sy-msgli.
    message_simple space.
*...Vistex-11.07.2019/Begin
    READ TABLE lt_rcdoc ASSIGNING FIELD-SYMBOL(<lwa_rcdoc>) INDEX 1.
    IF sy-subrc EQ 0.
      <lwa_rcdoc>-rcnum = gs_rcdoc_infocus-rcnum.
    ENDIF.
*...Vistex-11.07.2019/End
  ENDIF.

ENDFORM.                    " RCDOC_INFOCUS_SAVE
*&---------------------------------------------------------------------*
*&      Form  MESSAGES_INITIALIZE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM messages_initialize  USING lv_initiator TYPE /agri/gdescr
                                lv_subobject TYPE balsubobj
                                zsc_fmrchdr   TYPE zsc_fmrchdr.

  DATA: ls_context TYPE zsc_fmrc_context.

  messages_init.
  messages_collect_all.
  messages_initiator_set lv_initiator c_object-log lv_subobject.

****Set the context for Process Log
  IF NOT zsc_fmrchdr IS INITIAL.
    MOVE-CORRESPONDING zsc_fmrchdr TO ls_context.
    messages_context_data_set zsc_fmrchdr-rcnum space space
                              'ZSC_FMRC_CONTEXT' ls_context.
  ENDIF.

ENDFORM.                    " MESSAGES_INITIALIZE
*&---------------------------------------------------------------------*
*&      Form  message_context_set
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM message_context_set USING lwa_rchdr TYPE zsc_fmrchdr.

  DATA: lwa_context TYPE zsc_fmrc_context.

  IF lwa_rchdr IS NOT INITIAL.
    MOVE-CORRESPONDING lwa_rchdr TO lwa_context.
    messages_context_data_set lwa_rchdr-rcnum
                                  space space
                                  'ZSC_FMRC_CONTEXT' lwa_context.
  ENDIF.

ENDFORM.                    "message_context_set
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  MD_HEADER_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ac_header_update  USING ls_old_header TYPE zsc_fmrchdr
                       CHANGING ls_header  TYPE zsc_fmrchdr
                                lv_subrc   TYPE sy-subrc.

  IF ls_old_header-rctyp NE ls_header-rctyp.
    PERFORM authority_check USING ls_header
                                  c_authorization_activity-create
                                  c_msg_type-error
                         CHANGING lv_subrc.
    IF lv_subrc <> 0.
      CLEAR gs_variables-document_mode.
      EXIT.
    ENDIF.

    PERFORM rcdoc_type_control_read USING ls_header-rctyp
                                CHANGING lv_subrc.
    IF lv_subrc NE 0.
      EXIT.
    ENDIF.
    MOVE-CORRESPONDING gs_fmrctyp TO ls_header.
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
FORM ac_header_data_check CHANGING ls_rchdr TYPE zsc_fmrchdr
                                   lv_subrc TYPE sy-subrc.

ENDFORM.                    " MD_HEADER_DATA_CHECK
*&---------------------------------------------------------------------*
*&      Form  md_fcode_processing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ac_fcode_processing.

  DATA: lv_prefix TYPE char50.

  CHECK sy-ucomm CS 'ZFMRC_'.
  SPLIT sy-ucomm AT '_' INTO lv_prefix sy-ucomm.

  fcode_execute sy-ucomm /agri/saplfmrcm.
  CLEAR: ok_code, sy-ucomm.

ENDFORM.                    "md_fcode_processing
*&---------------------------------------------------------------------*
*& Form MANDATORY_FIELD_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM mandatory_field_check .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MATERIAL_DUPLICATE_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_MOD_ROW_VALUE
*&      <-- LV_SUBRC
*&---------------------------------------------------------------------*
FORM material_duplicate_check  USING VALUE(lv_material)
                               CHANGING VALUE(lv_subrc).

  DATA: lv_timesnumbers TYPE int4.

  CLEAR: lv_subrc.
  LOOP AT gt_fmrclst_fcat INTO DATA(lwa_rclst)
                          WHERE matnr_ins IS NOT INITIAL.
    IF lv_material EQ lwa_rclst-matnr_ins.
      ADD 1 TO lv_timesnumbers.
      IF lv_timesnumbers GT 1.
        MESSAGE ID 'ZFMRC' TYPE 'E' NUMBER '069' INTO sy-msgli.
        MOVE 4 TO lv_subrc.
        MOVE c_true TO gs_variables-errors.
        message_simple c_false.
        EXIT.
      ELSE.
        CONTINUE.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form MATERIAL_PRINCIPAL_UPDATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_MOD_ROW_ROW_ID
*&---------------------------------------------------------------------*
FORM material_principal_update  USING VALUE(lv_row_id)
                                CHANGING ls_rclst TYPE zsc_fmrclst_fcat.

  FIELD-SYMBOLS: <lwa_rclst> TYPE zsc_fmrclst.

  LOOP AT gs_rcdoc_infocus-x-rclst ASSIGNING <lwa_rclst>
                    WHERE rcinp_check = c_true.
    MOVE: space TO <lwa_rclst>-rcinp_check,
          icon_wd_radio_button_empty  TO <lwa_rclst>-rcinp,
          c_updkz_update TO <lwa_rclst>-updkz.
  ENDLOOP.

  READ TABLE gt_fmrclst_fcat INTO DATA(lwa_rclst)
                               INDEX lv_row_id.
  IF sy-subrc EQ 0.
    MOVE: c_true TO ls_rclst-rcinp_check,
          icon_wd_radio_button TO ls_rclst-rcinp.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form MATERIAL_F4_VALUES_GET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_DISPLAY
*&      <-- LV_VALUE
*&      <-- LV_CHANGE
*&      <-- LV_SUBRC
*&---------------------------------------------------------------------*
FORM material_f4_values_get  USING     lv_display
                              CHANGING lv_value TYPE matnr
                                       lv_change
                                       lv_subrc TYPE sy-subrc.


  DATA:
    lt_matnr_tab      TYPE zt_fmrcmatnr_tab,
    lwa_matnr_data    LIKE LINE OF lt_matnr_tab,
    lwa_field_mapping TYPE dselc,
    lt_field_mapping  TYPE TABLE OF dselc.

  DATA: lv_title(80),
       lv_fieldname TYPE fieldname.

  DATA: lt_return_values TYPE TABLE OF ddshretval INITIAL SIZE 0,
        lwa_return_value TYPE ddshretval.

  FIELD-SYMBOLS: <lt_matnr_tab> TYPE STANDARD TABLE.

  PERFORM material_read CHANGING lt_matnr_tab
                                 lv_subrc.
  CHECK lv_subrc EQ 0.
  CHECK lt_matnr_tab[] IS NOT INITIAL.
  lv_title = TEXT-019.
  CONDENSE lv_title.
  ASSIGN lt_matnr_tab[] TO <lt_matnr_tab>.
  lv_fieldname = 'MATNR'.

  CHECK <lt_matnr_tab> IS ASSIGNED.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = lv_fieldname
      window_title    = lv_title
      value_org       = 'S'
      display         = lv_display
    TABLES
      value_tab       = <lt_matnr_tab>[]
      return_tab      = lt_return_values
      dynpfld_mapping = lt_field_mapping
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  READ TABLE lt_return_values INTO lwa_return_value
                INDEX 1.
  IF sy-subrc EQ 0.
    lv_change = c_true.
    lv_value = lwa_return_value-fieldval.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKT_READ
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ZSC_FMRCHDR_WERKS
*&      <-- LT_MAKT_TAB
*&---------------------------------------------------------------------*
FORM material_read CHANGING lt_material_tab TYPE zt_fmrcmatnr_tab
                            lv_subrc        TYPE sysubrc.

  DATA: lrt_mtart TYPE RANGE OF mtart.

  DO 2 TIMES.
    DATA(lv_index) = sy-index.
    INSERT INITIAL LINE INTO TABLE lrt_mtart
      ASSIGNING FIELD-SYMBOL(<lrs_mtart>).
    IF sy-subrc EQ 0.
      <lrs_mtart> = 'IEQ'.
      CASE lv_index.
        WHEN 1.
          <lrs_mtart>-low = gs_tfmrctyp-mtart.
        WHEN 2.
          <lrs_mtart>-low = 'ZPFZ'.
      ENDCASE.
    ENDIF.
  ENDDO.

  SELECT * FROM mara AS a
    INNER JOIN  makt AS t
    ON a~matnr = t~matnr
    INNER JOIN marc AS c
    ON a~matnr = c~matnr
    INTO CORRESPONDING FIELDS OF TABLE @lt_material_tab
   WHERE c~werks EQ @zsc_fmrchdr-werks
*     AND a~mtart = gs_tfmrctyp-mtart
     AND a~mtart IN @lrt_mtart[]
     AND t~spras EQ @sy-langu.

  IF sy-subrc EQ 0.
    MOVE 0 TO lv_subrc.
  ELSE.
    MOVE 4 TO lv_subrc.
  ENDIF.

  SORT lt_material_tab BY matnr.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MATNR_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM matnr_check .

  DATA: lv_matnr TYPE matnr.
  DATA: lv_ausme TYPE meins.

  CHECK zsc_fmrchdr-werks IS NOT INITIAL
        AND zsc_fmrchdr-matnr IS NOT INITIAL.
  SELECT SINGLE matnr ausme FROM marc
               INTO (lv_matnr, lv_ausme)
         WHERE matnr = zsc_fmrchdr-matnr
              AND werks = zsc_fmrchdr-werks.

  IF sy-subrc NE 0.
    SET CURSOR FIELD 'ZSC_FMRCHDR-MATNR'.
    MESSAGE ID 'ZFMRC' TYPE c_msg_type-error
                       NUMBER '091'
                       WITH zsc_fmrchdr-matnr
                            zsc_fmrchdr-werks.
  ENDIF.
  IF  lv_ausme IS INITIAL.
    SET CURSOR FIELD 'ZSC_FMRCHDR-MATNR'.
    MESSAGE ID 'ZFMRC' TYPE c_msg_type-error
                       NUMBER '103'
                       WITH zsc_fmrchdr-matnr
                            zsc_fmrchdr-werks.
  ENDIF.

ENDFORM.

FORM modify_mass.


  DATA: lt_rows          TYPE lvc_t_row,
        lwa_row          TYPE lvc_s_row,
        lv_answer        TYPE c,
        lwa_rclst_layout LIKE LINE OF gt_fmrclst_fcat.

  FIELD-SYMBOLS:<lwa_rcvrs>     TYPE zsc_fmrcvrs.
  CALL METHOD ref_grid_rcvrs->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.

  gs_variables-refresh_vrs_grid = c_true.
  SORT lt_rows BY index DESCENDING.
  LOOP AT lt_rows INTO lwa_row.
    READ TABLE gs_rcdoc_infocus-x-rcvrs ASSIGNING <lwa_rcvrs>
                                        INDEX lwa_row-index.
    IF sy-subrc EQ 0.
      IF zsc_fmrcvrs-adatu IS NOT INITIAL.
        MOVE zsc_fmrcvrs-adatu TO <lwa_rcvrs>-adatu.
      ENDIF.
      IF zsc_fmrcvrs-bdatu IS NOT INITIAL.
        MOVE zsc_fmrcvrs-bdatu TO <lwa_rcvrs>-bdatu.
      ENDIF.
      IF zsc_fmrcvrs-stlal IS NOT INITIAL.
        MOVE zsc_fmrcvrs-stlal TO <lwa_rcvrs>-stlal.
      ENDIF.
      IF zsc_fmrcvrs-plnnr IS NOT INITIAL.
        MOVE zsc_fmrcvrs-plnnr  TO <lwa_rcvrs>-plnnr.
      ENDIF.
      IF zsc_fmrcvrs-text1 IS NOT INITIAL.
        MOVE zsc_fmrcvrs-text1 TO <lwa_rcvrs>-text1.
      ENDIF.
    ENDIF.

    IF zsc_fmrcvrs-adatu IS NOT INITIAL OR
       zsc_fmrcvrs-bdatu IS NOT INITIAL OR
       zsc_fmrcvrs-stlal IS NOT INITIAL OR
       zsc_fmrcvrs-plnnr IS NOT INITIAL OR
       zsc_fmrcvrs-text1 IS NOT INITIAL.
      MOVE: c_updkz_update TO <lwa_rcvrs>-updkz,
            c_true      TO gs_variables-refresh_vrs_grid.
    ENDIF.
  ENDLOOP.
ENDFORM.
