*&---------------------------------------------------------------------*
*& Include          LZFG_ACMF0G
*&---------------------------------------------------------------------*
FORM good_receipt_create  USING
                                lv_commit_work
                          CHANGING lv_subrc  TYPE sy-subrc
                            lt_toagroup TYPE  /agri/t_fmacoll.

  DATA:
    lt_items_gm    TYPE TABLE OF bapi2017_gm_item_create,
    lwa_items_gm   TYPE bapi2017_gm_item_create,
    lwa_toagroup   TYPE  /agri/s_fmacoll,
    lt_po_items    TYPE icl_t_bapiekpoc, "bapiekpoc_tp,
    lwa_po_items   TYPE bapiekpoc,
    ls_comprfir    TYPE /agri/s_fmacomp,
    ls_comprseg    TYPE /agri/s_fmacomp,
    lt_return      TYPE TABLE OF bapiret2,
    lv_mvt_code    TYPE bapi2017_gm_code,
    lwa_mvt_header TYPE bapi2017_gm_head_01,
    lv_test_run    TYPE bapi2017_gm_gen-testrun,
    lwa_return     TYPE bapiret2,
    lwa_grheader   TYPE bapi2017_gm_head_ret.

  FIELD-SYMBOLS: <lwa_fmacitm> TYPE /agri/s_fmacitm.

*--- Header
  MOVE c_grmm-mvt_code TO  lv_mvt_code.
  MOVE gs_acdoc_infocus-accom TO lwa_mvt_header-ref_doc_no.

*--- Sort for purchase order
  SORT lt_toagroup ASCENDING BY aufnr.
*--- Items

  LOOP AT lt_toagroup INTO lwa_toagroup .
    CLEAR:ls_comprfir.
    MOVE-CORRESPONDING lwa_toagroup TO ls_comprfir.

    PERFORM purchase_items_get USING lwa_toagroup-ebeln
                               CHANGING lt_po_items.
    CHECK lt_po_items[] IS NOT INITIAL.
    READ TABLE lt_po_items INTO lwa_po_items
                           WITH KEY pur_mat = lwa_toagroup-matnr"material_long = lwa_toagroup-matnr
                                    po_number  = lwa_toagroup-ebeln
                                    po_item  = lwa_toagroup-ebelp.
    CHECK sy-subrc EQ 0.
    MOVE-CORRESPONDING lwa_po_items TO lwa_items_gm.
    MOVE lwa_toagroup-qmein TO lwa_items_gm-entry_uom.
    MOVE lwa_toagroup-menge TO lwa_items_gm-entry_qnt.
    MOVE lwa_toagroup-budat TO  lwa_mvt_header-pstng_date.
    MOVE lwa_toagroup-bldat TO  lwa_mvt_header-doc_date.
    MOVE c_grmm-move_type   TO lwa_items_gm-move_type.
    MOVE c_grmm-mvt_ind     TO lwa_items_gm-mvt_ind.
    MOVE c_grmm-stck_type   TO lwa_items_gm-stck_type.
    APPEND lwa_items_gm TO lt_items_gm.

*    AT END OF aufnr.

    IF lv_commit_work IS INITIAL.
      lv_test_run = c_true.
    ELSE.
      SET UPDATE TASK LOCAL.
    ENDIF.

    CHECK lt_items_gm[] IS NOT INITIAL.
    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
      EXPORTING
        goodsmvt_header       = lwa_mvt_header
        goodsmvt_code         = lv_mvt_code
        testrun               = lv_test_run
*       GOODSMVT_REF_EWM      =
      IMPORTING
        goodsmvt_headret      = lwa_grheader
*       MATERIALDOCUMENT      =
*       MATDOCUMENTYEAR       =
      TABLES
        goodsmvt_item         = lt_items_gm[]
*       GOODSMVT_SERIALNUMBER =
        return                = lt_return.
*     GOODSMVT_SERV_PART_DATA =
*     EXTENSIONIN      =

    REFRESH: lt_items_gm.

    LOOP AT lt_return INTO lwa_return
                      WHERE type EQ c_msg_type-error
                         OR type EQ c_msg_type-abend.
      MESSAGE ID lwa_return-id TYPE c_msg_type-error
      NUMBER lwa_return-number
      WITH lwa_return-message_v1 lwa_return-message_v2
           lwa_return-message_v3 lwa_return-message_v4
      INTO sy-msgli.
      message_simple space.
      EXIT.
    ENDLOOP.
    IF sy-subrc EQ 0.
      lv_subrc = 4.
      EXIT.
    ELSEIF lv_commit_work IS NOT INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = c_true.
      SORT gs_acdoc_infocus-x-acitm ASCENDING BY aufnr.
      gs_acdoc_infocus-x-achdr-status = c_process_status-cnf.
      LOOP AT gs_acdoc_infocus-x-acitm ASSIGNING <lwa_fmacitm>
                                       WHERE status   = c_process_status-ctd
                                       AND intext     = c_true.

        READ TABLE lt_toagroup INTO lwa_toagroup WITH KEY
                                                   lifnr      = <lwa_fmacitm>-lifnr
                                                   idactvl    = <lwa_fmacitm>-idactvl
                                                  idresource = <lwa_fmacitm>-idresource
                                                   aufnr     = <lwa_fmacitm>-aufnr.
        MOVE sy-subrc TO lv_subrc.
        MOVE-CORRESPONDING lwa_toagroup TO ls_comprseg.
        CHECK ls_comprfir EQ ls_comprseg.
        IF lv_subrc EQ 0.
          MOVE lwa_toagroup-ebeln    TO <lwa_fmacitm>-ebeln.
          MOVE lwa_toagroup-ebelp    TO <lwa_fmacitm>-ebelp.
          MOVE lwa_toagroup-knuma_ag    TO <lwa_fmacitm>-knuma_ag.
          MOVE lwa_grheader-mat_doc  TO <lwa_fmacitm>-mblnr.
          MOVE lwa_grheader-doc_year TO <lwa_fmacitm>-mjahr.
          MOVE c_process_status-cnf  TO  <lwa_fmacitm>-status.
          IF <lwa_fmacitm>-updkz NE c_updkz_new.
            <lwa_fmacitm>-updkz = c_updkz_update.
          ENDIF.
        ENDIF.
        CLEAR:lv_subrc.
      ENDLOOP.
    ENDIF.
    MESSAGE ID '/AGRI/FMAC'
        TYPE c_msg_type-success
        NUMBER '064'
        WITH lwa_grheader-mat_doc
        INTO sy-msgli.
    message_simple space.
*    ENDAT.
  ENDLOOP.

  PERFORM messages_display USING gs_variables-initiator.
****Save Accomplishment
  PERFORM document_infocus_save USING c_true.

ENDFORM.                    "good_receipt_create

*&---------------------------------------------------------------------*
*&      Form  good_receipt_reverse
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM good_receipt_reverse .

  DATA:
    lt_fmbck_doc      TYPE /agri/t_fmbck_doc,
    lwa_fmbck_doc     TYPE /agri/s_fmbck_doc,
    lwa_items         TYPE /agri/s_fmacitm_layout,
    lwa_acitm         TYPE /agri/s_fmacitm,
    lwa_bckdfl        TYPE /agri/s_fmbckdfl,
    lwa_selected_rows TYPE lvc_s_row,
    lv_mblnr          TYPE mblnr,
    ls_bckdfl         TYPE /agri/s_fmbckdfl,
    lt_acitm          TYPE /agri/t_fmacitm.

  PERFORM flow_documents_check CHANGING lt_fmbck_doc.
  CALL METHOD ref_grid_items->get_selected_rows
    IMPORTING
      et_index_rows = gt_selected_rows.

  CHECK gt_selected_rows IS NOT INITIAL.
  LOOP AT gt_selected_rows INTO lwa_selected_rows.
    READ TABLE gt_items_layout INTO lwa_items
    INDEX lwa_selected_rows-index.

*---Check type resources
    CHECK lwa_items-intext NE c_false.

    CHECK sy-subrc EQ 0 AND
    lwa_items-status EQ c_process_status-cnf
                      AND lwa_items-intext EQ c_true
                      AND lwa_items-indca EQ c_false  " Confirmation
                      AND lwa_items-mblnr NE lv_mblnr.
    MOVE lwa_items-mblnr TO lv_mblnr.
    READ TABLE lt_fmbck_doc INTO lwa_fmbck_doc INDEX 1.
    CHECK sy-subrc EQ 0.
    READ TABLE lwa_fmbck_doc-x-bckdfl INTO lwa_bckdfl
                                     WITH KEY vbeln = lwa_items-mblnr
                                              vbtyp_n = 'GR'.
    IF sy-subrc EQ 0.
      READ TABLE lwa_fmbck_doc-x-bckdfl INTO ls_bckdfl
                                       WITH KEY bcknum = lwa_bckdfl-bcknum
                                                bckitm = lwa_bckdfl-bckitm
                                                vbtyp_v = 'IR'.
      IF sy-subrc EQ 0.
        CLEAR: lwa_bckdfl.
        READ TABLE lwa_fmbck_doc-x-bckdfl INTO lwa_bckdfl
                                         WITH KEY bcknum = ls_bckdfl-bcknum
                                                  bckitm = ls_bckdfl-bckitm
                                                  vbtyp_v = 'RI'.
        IF sy-subrc EQ 0.
          MOVE-CORRESPONDING lwa_items TO lwa_acitm.
          APPEND lwa_acitm TO lt_acitm.
        ELSE.
          MESSAGE ID '/AGRI/FMAC'
                  TYPE c_msg_type-success
                  NUMBER '065'
                  WITH lwa_bckdfl-bcknum
                  INTO sy-msgli.
          message_simple space.
          EXIT.
        ENDIF.
      ELSE.
        MOVE-CORRESPONDING lwa_items TO lwa_acitm.
        APPEND lwa_acitm TO lt_acitm.
      ENDIF.
    ENDIF.
  ENDLOOP.

****  If IR is done then only we can reverse IR
  CHECK lt_acitm[] IS NOT INITIAL.
  PERFORM good_receipt_cancel USING lt_acitm[]
                                    lt_fmbck_doc.

  gs_variables-refresh_items_grid = c_true.
  PERFORM messages_display USING gs_variables-initiator.
****Save Accomplishment
  PERFORM document_infocus_save USING c_true.

ENDFORM.                    "good_receipt_reverse
*&---------------------------------------------------------------------*
*&      Form  GOOD_RECEIPT_CANCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ACITM[]  text
*----------------------------------------------------------------------*
FORM good_receipt_cancel  USING lt_acitm TYPE /agri/t_fmacitm
                                lt_fmbck_doc TYPE /agri/t_fmbck_doc.


  DATA: lwa_fmacitm        TYPE /agri/s_fmacitm,
        lwa_bckdfl         TYPE /agri/s_fmbckdfl,
        lv_set_update_task,
        lv_commit_work,
        lt_messages_save   TYPE /agri/t_gprolog,
        lwa_messages_save  TYPE /agri/s_gprolog,
        lwa_bckdoc         TYPE /agri/s_fmbck_doc,
        lt_fmbckdfl        TYPE /agri/t_fmbckdfl,
        ls_docanc          TYPE bapi2017_gm_head_ret,
        lwa_return         TYPE bapiret2,
        lv_reftext         TYPE bktxt,

        lt_return          TYPE TABLE OF bapiret2.

  FIELD-SYMBOLS: <lwa_fmacitm>   TYPE /agri/s_fmacitm,
                 <lwa_fmbck_doc> TYPE /agri/s_fmbck_doc,
                 <lwa_bckitm>    TYPE /agri/s_fmbckitm.

  SORT lt_acitm ASCENDING BY mblnr.
  DELETE ADJACENT DUPLICATES FROM lt_acitm COMPARING mblnr.

  MOVE gs_acdoc_infocus-accom  TO lv_reftext.
  LOOP AT lt_acitm INTO lwa_fmacitm .
    CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
      EXPORTING
        materialdocument    = lwa_fmacitm-mblnr
        matdocumentyear     = lwa_fmacitm-mjahr
*       GOODSMVT_PSTNG_DATE =
*       GOODSMVT_PR_UNAME   =
*       documentheader_text = lv_reftext
      IMPORTING
        goodsmvt_headret    = ls_docanc
      TABLES
        return              = lt_return.
*       GOODSMVT_MATDOCITEM =

    LOOP AT lt_return INTO lwa_return
                       WHERE type EQ c_msg_type-error
                       OR type EQ c_msg_type-abend.
      MESSAGE ID lwa_return-id TYPE c_msg_type-error
      NUMBER lwa_return-number
      WITH lwa_return-message_v1 lwa_return-message_v2
           lwa_return-message_v3 lwa_return-message_v4
      INTO sy-msgli.
      message_simple space.
      EXIT.
    ENDLOOP.
    IF lt_return[] IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = c_true.

      MESSAGE ID '/AGRI/FMAC'
              TYPE c_msg_type-success
              NUMBER '066'
              WITH lwa_fmacitm-mblnr
              INTO sy-msgli.
      message_simple space.
*--- Update Bucket
      READ TABLE lt_fmbck_doc ASSIGNING <lwa_fmbck_doc> INDEX 1.
      CHECK sy-subrc EQ 0.
      READ TABLE <lwa_fmbck_doc>-x-bckdfl INTO lwa_bckdfl
                      WITH KEY vbeln = lwa_fmacitm-mblnr
                      vbtyp_n = 'GR'.
      IF sy-subrc EQ 0.
        READ TABLE <lwa_fmbck_doc>-x-bckitm ASSIGNING <lwa_bckitm>
                                            WITH KEY bcknum = lwa_bckdfl-bcknum
                                                     bckitm = lwa_bckdfl-bckitm.
        IF sy-subrc EQ 0.

          <lwa_bckitm>-status = '6'.
          <lwa_fmbck_doc>-updkz = c_updkz_update.
          <lwa_bckitm>-updkz = c_updkz_update.

          CALL FUNCTION '/AGRI/FMBCK_DOCFLOW_ADD'
            EXPORTING
              i_vbeln_p   = <lwa_bckitm>-bcknum
              i_posnr_p   = <lwa_bckitm>-bckitm
              i_vbeln_s   = ls_docanc-mat_doc
*             i_posnr_s   =
              i_vbtyp_p   = 'BK'
              i_vbtyp_s   = 'RG'
*             I_AWTYP     =
*             I_AWREF     =
*             I_AWORG     =
*             I_AWSYS     =
*             I_WRBTR     =
*             i_waers     = lwa_bckitm-waers
*             IS_BKPF     = ls_bkpf
            IMPORTING
              et_fmbckdfl = lt_fmbckdfl.
          APPEND LINES OF lt_fmbckdfl TO <lwa_fmbck_doc>-x-bckdfl.


          CHECK lt_fmbck_doc IS NOT INITIAL.
          CALL FUNCTION '/AGRI/FMBCK_SAVE'
            EXPORTING
              i_set_update_task = lv_set_update_task
              i_commit          = lv_commit_work
            CHANGING
              ct_bckdoc         = lt_fmbck_doc
              ct_messages       = lt_messages_save
            EXCEPTIONS
              no_change         = 1
              OTHERS            = 2.

          LOOP AT lt_fmbck_doc INTO lwa_bckdoc.
            MESSAGE ID    '/AGRI/FMAC'
                    TYPE   c_msg_type-success
                    NUMBER '068'
                    WITH lwa_bckdoc-bcknum
            INTO sy-msgli.
            message_simple space.
            EXIT.
          ENDLOOP.

          REFRESH: lt_messages_save.
          CLEAR: lwa_bckdoc.
        ENDIF.
      ENDIF.

      LOOP AT gs_acdoc_infocus-x-acitm ASSIGNING <lwa_fmacitm>
                                          WHERE accom = lwa_fmacitm-accom
                                               AND mblnr = lwa_fmacitm-mblnr
                                               AND mjahr = lwa_fmacitm-mjahr.
        MOVE c_true TO lwa_fmacitm-indca.
        IF lwa_fmacitm NE <lwa_fmacitm>.
          MOVE c_true               TO <lwa_fmacitm>-indca.
          MOVE c_process_status-rev TO <lwa_fmacitm>-status.
          IF <lwa_fmacitm>-updkz NE c_updkz_new.
            <lwa_fmacitm>-updkz = c_updkz_update.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "good_receipt_cancel
