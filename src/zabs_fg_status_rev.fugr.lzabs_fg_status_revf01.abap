*----------------------------------------------------------------------*
***INCLUDE LZABS_FG_STATUS_REVF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form REVERSE_CONFIRMATIONS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> C_TRUE
*&      <-- LWA_FMOC_DOC
*&      <-- LV_SUBRC
*&---------------------------------------------------------------------*
FORM reverse_confirmations  USING    lv_commit_work
                            CHANGING lwa_fmoc_doc TYPE /agri/s_fmoc_doc
                                     lv_subrc     TYPE sy-subrc.

*--Local declarations
  DATA: lwa_fmoc_doc_infocus TYPE /agri/s_fmoc_doc,
        lwa_ocopr            TYPE /agri/s_fmocopr,
        lwa_occom            TYPE /agri/s_fmoccom,
        lwa_ocindx           TYPE /agri/s_fmocindx,
        lt_fmoc_doc          TYPE /agri/t_fmoc_doc,
        lt_aufnr             TYPE /agri/t_fmaufnr,
        lt_fpdoc             TYPE /agri/t_fmfp_doc,
        lwa_aufnr            TYPE /agri/s_fmaufnr,
        lv_ocnum             TYPE /agri/fmocnum,
        lt_cancel            TYPE TABLE OF conf_cancel,
        lt_docnum            TYPE TABLE OF imseg,
        lt_return            TYPE TABLE OF bapi_coru_return,
        lt_mseg              TYPE TABLE OF imseg,
        lwa_docnum           TYPE imseg,
        lwa_cancel           TYPE conf_cancel,
        lwa_return           TYPE bapi_coru_return,
        lv_failed,
*---Replace Unreleased Interfaces
        lwa_breturn          TYPE bapiret1,
        lv_locked	           TYPE bapi_coru_param-locked,
        lv_conf_no           TYPE bapi_pi_conf_key-conf_no,
        lv_conf_count        TYPE bapi_pi_conf_key-conf_cnt.

*----Field-symbol declaration
  FIELD-SYMBOLS: <lwa_fmfp_doc> TYPE /agri/s_fmfp_doc,
                 <lwa_fpitm>    TYPE /agri/s_fmfpitm,
                 <lwa_fpcom>    TYPE /agri/s_fmfpcom.

  PERFORM messages_context_set USING lwa_fmoc_doc-x-ochdr.
*--lock task orders
  LOOP AT lwa_fmoc_doc-x-ocindx INTO lwa_ocindx.
    PERFORM order_infocus_lock USING lwa_ocindx-aufnr
                            CHANGING lv_subrc.
    IF lv_subrc IS NOT INITIAL.
      EXIT.
    ENDIF.

    lwa_aufnr-aufnr = lwa_ocindx-aufnr.
    APPEND lwa_aufnr TO lt_aufnr.
    CLEAR lwa_ocindx.
    lwa_ocindx-aufnr = lwa_aufnr-aufnr.
    lwa_ocindx-updkz = zcl_abs_abap_maintain=>c_updkz_insert. "'I'
    APPEND lwa_ocindx TO lwa_fmoc_doc_infocus-x-ocindx.
  ENDLOOP.
  IF lv_subrc IS NOT INITIAL OR
     lt_aufnr IS INITIAL.
    EXIT.
  ENDIF.

  PERFORM production_order_read USING lt_aufnr
                             CHANGING lt_fpdoc.

  LOOP AT lwa_fmoc_doc-x-ocopr INTO lwa_ocopr.
    READ TABLE lt_fpdoc ASSIGNING <lwa_fmfp_doc>
                         WITH KEY aufnr = lwa_ocopr-aufnr
                         BINARY SEARCH.
    IF sy-subrc EQ 0.
      IF <lwa_fmfp_doc>-x-fphdr-tecom IS INITIAL.
        lwa_cancel-conf_no    = lwa_ocopr-rueck.
        lwa_cancel-conf_count = lwa_ocopr-rmzhl.
        APPEND lwa_cancel TO lt_cancel.
      ELSE.
        MESSAGE e051(/agri/fmfp) WITH lwa_ocopr-aufnr INTO sy-msgli.
        message_simple space.
        EXIT.
      ENDIF.
      READ TABLE <lwa_fmfp_doc>-x-fpitm ASSIGNING <lwa_fpitm>
                                       WITH KEY aufnr = lwa_ocopr-aufnr
                                                posnr = lwa_ocopr-posnr
                                       BINARY SEARCH.
      IF sy-subrc EQ 0.
        <lwa_fpitm>-gwemg = <lwa_fpitm>-gwemg - lwa_ocopr-lmnga.
        IF <lwa_fpitm>-gwemg IS INITIAL.
          CLEAR <lwa_fpitm>-cstat.
        ELSEIF <lwa_fpitm>-gamng GT <lwa_fpitm>-gwemg.
          <lwa_fpitm>-cstat = zcl_abs_abap_maintain=>c_po_pcnf. "'PCNF'
        ENDIF.
        IF <lwa_fpitm>-grcre IS NOT INITIAL.
          <lwa_fmfp_doc>-x-fphdr-cstat = <lwa_fpitm>-cstat.
          READ TABLE lwa_fmoc_doc-x-ocindx INTO lwa_ocindx
                                       WITH KEY aufnr = <lwa_fpitm>-aufnr
                                       BINARY SEARCH.
          IF sy-subrc EQ 0.
            <lwa_fmfp_doc>-x-fphdr-gwemg = <lwa_fmfp_doc>-x-fphdr-gwemg - lwa_ocindx-gwemg.
          ENDIF.
          <lwa_fmfp_doc>-x-fphdr-updkz = zcl_abs_abap_maintain=>c_updkz_update. "'U'
        ENDIF.
        <lwa_fpitm>-updkz = zcl_abs_abap_maintain=>c_updkz_update. "'U'
        LOOP AT lwa_fmoc_doc-x-occom INTO lwa_occom WHERE rueck = lwa_ocopr-rueck
                                                      AND rmzhl = lwa_ocopr-rmzhl.
          READ TABLE  <lwa_fmfp_doc>-x-fpcom ASSIGNING <lwa_fpcom>
                                              WITH KEY aufnr = <lwa_fpitm>-aufnr
                                                       posnr = <lwa_fpitm>-posnr
                                                       contr = lwa_occom-contr
                                              BINARY SEARCH.
          IF sy-subrc EQ 0.
            <lwa_fpcom>-comng = <lwa_fpcom>-comng - lwa_occom-lmnga.
            <lwa_fpcom>-updkz = zcl_abs_abap_maintain=>c_updkz_update. "'U'
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDLOOP.
  CHECK lt_cancel IS NOT INITIAL.
*--Reverse Confirmations
  PERFORM call_confirmation_reverse TABLES lt_cancel
                                           lt_docnum
                                           lt_return
                                     USING sy-datum.
  LOOP AT lt_return INTO lwa_return
                   WHERE type EQ zcl_abs_abap_maintain=>c_msgty_error. "'E'
    lv_subrc = 4.
    MESSAGE ID lwa_return-id TYPE lwa_return-type
                           NUMBER lwa_return-number
                             WITH lwa_return-message_v1
                                  lwa_return-message_v2
                                  lwa_return-message_v3
                                  lwa_return-message_v4
                             INTO sy-msgli.
    message_simple space.
  ENDLOOP.
  IF lv_subrc IS NOT INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    EXIT.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
  ENDIF.

  LOOP AT lt_return INTO lwa_return.
    READ TABLE lt_cancel INTO lwa_cancel WITH KEY conf_no = lwa_return-conf_no. "INDEX lwa_return-row.
    IF sy-subrc EQ 0.
      CLEAR:lwa_ocopr.
      READ TABLE lwa_fmoc_doc-x-ocopr INTO lwa_ocopr WITH KEY rueck = lwa_cancel-conf_no
                                                              rmzhl = lwa_cancel-conf_count.
      IF sy-subrc EQ 0.
        lwa_ocopr-rueck_ref = lwa_ocopr-rueck."lwa_cancel-conf_no.
        lwa_ocopr-rmzhl_ref = lwa_ocopr-rmzhl."lwa_cancel-conf_count.
        lwa_ocopr-rueck     = lwa_return-conf_no.
        lwa_ocopr-rmzhl     = lwa_return-conf_cnt.
        lwa_ocopr-updkz     = zcl_abs_abap_maintain=>c_updkz_insert. "'I'
        READ TABLE lt_docnum INTO lwa_docnum INDEX sy-tabix.
        IF sy-subrc EQ 0.
          lwa_ocopr-aufnr = lwa_docnum-aufnr.
        ENDIF.
        APPEND lwa_ocopr TO lwa_fmoc_doc_infocus-x-ocopr.
        LOOP AT lwa_fmoc_doc-x-occom INTO lwa_occom WHERE rueck = lwa_cancel-conf_no
                                                      AND rmzhl = lwa_cancel-conf_count.
          CLEAR: lwa_occom-ocnum,lwa_occom-bwart.
          lwa_occom-rueck = lwa_return-conf_no.
          lwa_occom-rmzhl = lwa_return-conf_cnt.
          lwa_occom-updkz = zcl_abs_abap_maintain=>c_updkz_insert. "'I'
          APPEND lwa_occom TO lwa_fmoc_doc_infocus-x-occom.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF lv_subrc NE 0.
    EXIT.
  ENDIF.

*-- Generate new confirmatino number for reversal
  PERFORM oc_number_generate CHANGING lv_ocnum lv_failed.
  IF lv_failed IS INITIAL.
    PERFORM oc_enqueue USING lv_ocnum
                             lv_subrc.
    IF lv_subrc IS NOT INITIAL.
      EXIT.
    ENDIF.

    MOVE-CORRESPONDING lwa_fmoc_doc-x-ochdr TO lwa_fmoc_doc_infocus-x-ochdr.
    lwa_fmoc_doc_infocus-ocnum             =
    lwa_fmoc_doc_infocus-x-ochdr-ocnum     = lv_ocnum.
    lwa_fmoc_doc_infocus-x-ochdr-ocnum_ref = lwa_fmoc_doc-x-ochdr-ocnum.
    lwa_fmoc_doc_infocus-x-ochdr-updkz     = zcl_abs_abap_maintain=>c_updkz_insert. "'I'
    APPEND lwa_fmoc_doc_infocus TO lt_fmoc_doc.
  ENDIF.
*--Save confirmation
  PERFORM document_infocus_save USING lv_commit_work
                             CHANGING lt_fmoc_doc
                                      lv_subrc.
  IF lv_subrc IS INITIAL.
    MESSAGE s000(/agri/fmoc) INTO sy-msgli.
    message_simple space.
    MESSAGE s001(/agri/fmoc) WITH lv_ocnum INTO sy-msgli.
    message_simple space.
    CLEAR: lwa_fmoc_doc.
    READ TABLE lt_fmoc_doc INTO lwa_fmoc_doc INDEX 1.
  ELSE.
    lv_subrc = 4.
    EXIT.
  ENDIF.

*--Save Process order
  PERFORM production_order_save USING lv_commit_work
                                      lt_fpdoc.
  LOOP AT lt_aufnr INTO lwa_aufnr.
    PERFORM order_infocus_unlock USING lwa_aufnr-aufnr.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALL_CONFIRMATION_REVERSE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_CANCEL
*&      --> LT_DOCNUM
*&      --> LT_RETURN
*&      --> SY_DATUM
*&---------------------------------------------------------------------*
FORM call_confirmation_reverse  TABLES   lt_cancel STRUCTURE conf_cancel
                                         lt_docnum STRUCTURE imseg
                                         lt_return STRUCTURE bapi_coru_return
                                USING    lv_budat TYPE budat.

*---Replace Unreleased Interfaces
  DATA: lwa_cancel         TYPE conf_cancel,
        lwa_breturn        TYPE bapi_coru_return,
*{   REPLACE        SS8K900016                                        3
*\       lv_conf_no         TYPE BAPI_PI_CONF_KEY-CONF_NO,
*\       lv_conf_coUnt      TYPE BAPI_PI_CONF_KEY-CONF_CNT,
*\       lv_POSTG_DATE      TYPE BAPI_PI_CONFIRM-POSTG_DATE,
*\       lv_CONF_TEXT       TYPE BAPI_PI_CONFIRM-CONF_TEXT,
        lv_conf_no         TYPE bapi_pp_conf_key-conf_no,
        lv_conf_count      TYPE bapi_pp_conf_key-conf_cnt,
        lv_postg_date      TYPE bapi_pp_confirm-postg_date,
        lv_conf_text       TYPE bapi_pp_confirm-conf_text,
*}   REPLACE
        lwa_return         TYPE bapiret1,
        lv_locked          TYPE bapi_coru_param-locked,
*{   REPLACE        SS8K900016                                        4
*\       lv_CRTD_CONF_NO    TYPE BAPI_PI_CONF_KEY-CONF_NO,
*\       lv_CRTD_CONF_COUNT TYPE BAPI_PI_CONF_KEY-CONF_CNT,
        lv_crtd_conf_no    TYPE bapi_pp_conf_key-conf_no,
        lv_crtd_conf_count TYPE bapi_pp_conf_key-conf_cnt,
*}   REPLACE
        lv_flg_error(1)    TYPE c.

*  CALL FUNCTION 'CO_RI_CONF_CANCEL'
*   EXPORTING
**     ORDER_CATEGORY           = 0
**     CONF_NO                  =
**     CONF_COUNT               =
*     postg_date               = lv_budat
**     CONF_TEXT                =
**     POST_WRONG_ENTRIES       = '0'
**     EX_IDENT                 =
**     I_NO_DATA_RESET          =
**     I_NO_CONF_POST           =
**   IMPORTING
**     RETURN                   =
**     LOCKED                   =
**     CREATED_CONF_NO          =
**     CREATED_CONF_COUNT       =
*   TABLES
*     it_cancel_conf           = lt_cancel[]
*     goodsmovements           = lt_docnum[]
*     detail_return            = lt_return[].

  LOOP AT lt_cancel INTO lwa_cancel.

    CLEAR: lv_conf_no, lv_conf_count, lv_postg_date, lv_conf_text,
           lwa_return, lv_locked, lv_crtd_conf_no, lv_crtd_conf_count,
           lv_flg_error, lwa_breturn.

    lv_conf_no    = lwa_cancel-conf_no.
    lv_conf_count = lwa_cancel-conf_count.

*{   DELETE         SS8K900016                                        1
*\     CALL FUNCTION 'BAPI_PROCORDCONF_CANCEL'
*\       EXPORTING
*\         confirmation              = lv_conf_no
*\         confirmationcounter       = lv_conf_count
*\         POSTG_DATE                = lv_budat
*\*         CONF_TEXT                 =
*\       IMPORTING
*\         RETURN                    = lwa_return
*\         LOCKED                    = lv_locked
*\         CREATED_CONF_NO           = lv_crtd_conf_no
*\         CREATED_CONF_COUNT        = lv_crtd_conf_count.
*}   DELETE
*{   INSERT         SS8K900016                                        2
    CALL FUNCTION 'BAPI_PRODORDCONF_CANCEL'
      EXPORTING
        confirmation        = lv_conf_no
        confirmationcounter = lv_conf_count
        postg_date          = lv_budat
*       CONF_TEXT           =
      IMPORTING
        return              = lwa_return
        locked              = lv_locked
        created_conf_no     = lv_crtd_conf_no
        created_conf_count  = lv_crtd_conf_count.
    IF lwa_return-type NE zcl_abs_abap_maintain=>c_msgty_error. "'E'
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.
*}   INSERT

    MOVE-CORRESPONDING lwa_return TO lwa_breturn.
    lwa_breturn-flg_locked = lv_locked.
    lwa_breturn-conf_no    = lv_crtd_conf_no.
    lwa_breturn-conf_cnt   = lv_crtd_conf_count.
    APPEND lwa_breturn TO lt_return.
  ENDLOOP.
*---


ENDFORM.
*&---------------------------------------------------------------------*
*& Form DOCUMENT_INFOCUS_SAVE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_COMMIT_WORK
*&      <-- LT_FMOC_DOC
*&      <-- LV_SUBRC
*&---------------------------------------------------------------------*
FORM document_infocus_save  USING    lv_commit_work
                            CHANGING lt_fmoc_doc
                                     lv_subrc TYPE sy-subrc.

  CALL FUNCTION '/AGRI/FMFP_OC_SAVE'
    EXPORTING
      i_commit_work = lv_commit_work
    CHANGING
      ct_ocdoc      = lt_fmoc_doc
    EXCEPTIONS
      OTHERS        = 1.
  lv_subrc  = sy-subrc.
  IF sy-subrc IS NOT INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MESSAGES_CONTEXT_SET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_FMOC_DOC_X_OCHDR
*&---------------------------------------------------------------------*
FORM messages_context_set  USING  lwa_ochdr TYPE /agri/s_fmochdr.

  DATA: lwa_context TYPE /agri/s_fmoc_context.

  IF lwa_ochdr IS NOT INITIAL.
    MOVE-CORRESPONDING lwa_ochdr TO lwa_context.
    messages_context_data_set_new lwa_ochdr-ocnum
                                  space space
                                  '/AGRI/S_FMOC_CONTEXT' lwa_context.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form OC_ENQUEUE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_OCNUM
*&      --> LV_SUBRC
*&---------------------------------------------------------------------*
FORM oc_enqueue  USING   lwa_ocdoc
                         lv_subrc.

  CALL FUNCTION 'ENQUEUE_/AGRI/EZ_FMOC'
    EXPORTING
      ocnum          = lwa_ocdoc
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
    lv_subrc = sy-subrc.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form OC_NUMBER_GENERATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_OCNUM
*&      <-- LV_FAILED
*&---------------------------------------------------------------------*
FORM oc_number_generate  CHANGING lv_ocnum
                                  lv_failed.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = zcl_abs_abap_maintain=>c_obj_fmoc  "'/AGRI/FMOC'
    IMPORTING
      number                  = lv_ocnum
    EXCEPTIONS
      interval_not_found      = 01
      number_range_not_intern = 02
      object_not_found        = 03.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
    message_simple space.
    lv_failed = abap_true.
    EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ORDER_INFOCUS_LOCK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_OCINDX_AUFNR
*&      <-- LV_SUBRC
*&---------------------------------------------------------------------*
FORM order_infocus_lock  USING    lv_aufnr TYPE /agri/fmfpnum
                         CHANGING lv_subrc TYPE sy-subrc.

  DATA: lv_msgv1 TYPE sy-msgv1.

  CALL FUNCTION 'ENQUEUE_ESORDER'
    EXPORTING
      aufnr          = lv_aufnr
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc IS NOT INITIAL.
    lv_subrc = sy-subrc.
    lv_msgv1 = sy-msgv1.
    MESSAGE i037(/agri/fmfp) INTO sy-msgli
                             WITH lv_aufnr lv_msgv1.
    message_simple space.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ORDER_INFOCUS_UNLOCK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_AUFNR_AUFNR
*&---------------------------------------------------------------------*
FORM order_infocus_unlock  USING    lv_aufnr TYPE /agri/fmfpnum.

  CALL FUNCTION 'DEQUEUE_ESORDER'
    EXPORTING
      aufnr = lv_aufnr.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PRODUCTION_ORDER_READ
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_AUFNR
*&      <-- LT_FPDOC
*&---------------------------------------------------------------------*
FORM production_order_read  USING    lt_aufnr TYPE /agri/t_fmaufnr
                            CHANGING lt_fpdoc TYPE /agri/t_fmfp_doc.

  CALL FUNCTION '/AGRI/FMFP_VIEW'
    EXPORTING
      it_aufnr       = lt_aufnr
    IMPORTING
      et_fpdoc       = lt_fpdoc
    EXCEPTIONS ##FM_SUBRC_OK
      no_data_exists = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form PRODUCTION_ORDER_SAVE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_COMMIT_WORK
*&      --> LT_FPDOC
*&---------------------------------------------------------------------*
FORM production_order_save  USING    lv_commit_work
                                     lt_fpdoc.

  CALL FUNCTION '/AGRI/FMFP_SAVE'
    EXPORTING
      i_commit_work = lv_commit_work
    CHANGING
      ct_fpdoc      = lt_fpdoc
    EXCEPTIONS
      no_change     = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form MESSAGES_INITIALIZE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> P_
*&---------------------------------------------------------------------*
FORM messages_initialize  USING  lv_initiator TYPE /agri/gdescr
                                 lv_subobject TYPE balsubobj.
messages_init.

  CHECK lv_initiator IS NOT INITIAL.
  messages_collect_all.
  messages_initiator_set lv_initiator '/AGRI/FMOC' lv_subobject.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form REV_MESSAGES_GET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
*FORM rev_messages_get  USING  lv_initiator TYPE /agri/gdescr
*                     CHANGING ct_messages  TYPE /agri/t_gprolog.
*
*
*    messages_get lv_initiator  ct_messages.
*    messages_init.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form MESSAGES_GET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_INITIATOR
*&      <-- ET_MESSAGES
*&---------------------------------------------------------------------*
FORM messages_get  USING    lv_initiator TYPE /agri/gdescr
                   CHANGING ct_messages  TYPE /agri/t_gprolog.

  messages_get lv_initiator ct_messages.
  messages_init.

ENDFORM.
