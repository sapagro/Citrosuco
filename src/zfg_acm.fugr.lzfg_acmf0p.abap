*&---------------------------------------------------------------------*
*& Include          LZFG_ACMF0P
*&---------------------------------------------------------------------*
FORM postings_confirmations CHANGING lv_subrc TYPE sy-subrc.

  DATA: lt_activity       TYPE TABLE OF /agri/fmacact,
        lt_fmfpcnf        TYPE /agri/t_fmfp_cnf,
        lt_fmfpcom        TYPE /agri/t_fmfpcom,
        lwa_fmfpcnf       TYPE /agri/s_fmfp_cnf,
        lt_fpdoc          TYPE /agri/t_fmfp_doc,
        lwa_fpdoc_infocus TYPE /agri/s_fmfp_doc,
        lt_fpitm          TYPE /agri/t_fmfpitm,
        lwa_fpitm         TYPE /agri/s_fmfpitm,
        lwa_message       TYPE /agri/s_gprolog,
        lt_messages       TYPE /agri/t_gprolog,
        lwa_context       TYPE /agri/s_fmfp_context,
        lt_aufnr          TYPE /agri/t_fmaufnr,
        lwa_activity      TYPE /agri/fmacact,
        lv_subrc2         TYPE sy-subrc,
        lt_fcat           TYPE lvc_t_fcat,
        lv_fieldname      TYPE lvc_fname,
        lt_acowner        TYPE /agri/t_fmacitm,
        lwa_acowner       TYPE /agri/s_fmacitm,
        lt_acthird        TYPE /agri/t_fmacitm,
        lwa_acthird       TYPE /agri/s_fmacitm.

  FIELD-SYMBOLS: <lwa_acitm> TYPE /agri/s_fmacitm,
                 <lwa_fcat>  TYPE lvc_s_fcat.

*  IF gt_items_layout IS INITIAL.
  IF gs_acdoc_infocus-x-acitm IS INITIAL.
    lv_subrc = 4.
    MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                                 NUMBER '028'
                                 INTO sy-msgli.
    RETURN.
  ENDIF.

  PERFORM conf_time_validate CHANGING lv_subrc.
  CHECK lv_subrc EQ 0.

  SELECT * INTO TABLE lt_activity FROM /agri/fmacact.   "#EC CI_NOWHERE

  PERFORM purchase_order_get.

  LOOP AT gs_acdoc_infocus-x-acitm ASSIGNING <lwa_acitm>
                                   WHERE status = c_process_status-ctd
                                     AND intext EQ c_false
                                     AND zzconfm IS INITIAL.
    "    CHECK lv_subrc EQ 0.
    READ TABLE lt_activity INTO lwa_activity WITH KEY
                                              idactv = <lwa_acitm>-idactvl
                                              rstype = c_rstype-labor.
    IF sy-subrc NE 0.
      READ TABLE lt_activity INTO lwa_activity WITH KEY
                                              idactv = <lwa_acitm>-idactve
                                              rstype = c_rstype-equnr.
    ENDIF.
    IF lwa_activity-bill = c_no.
      <lwa_acitm>-status = c_process_status-cnf.
      IF <lwa_acitm>-updkz NE c_updkz_new.
        <lwa_acitm>-updkz = c_updkz_update.
      ENDIF.
      CONTINUE.
    ENDIF.
    CLEAR: lt_aufnr[], lt_fpdoc[], lt_fpitm[].
    APPEND <lwa_acitm>-aufnr TO lt_aufnr.
    PERFORM aufnr_infocus_read  USING lt_aufnr
                                CHANGING lt_fpdoc.
    READ TABLE lt_fpdoc INTO lwa_fpdoc_infocus INDEX 1.
    lt_fpitm[] = lwa_fpdoc_infocus-x-fpitm[].
    READ TABLE lt_fpitm INTO lwa_fpitm WITH KEY arbpl = <lwa_acitm>-arbpl.

    lwa_fmfpcnf-aufnr = <lwa_acitm>-aufnr.
**    lwa_fmfpcnf-lmnga = <lwa_acitm>-menge.
**    lwa_fmfpcnf-meinh = <lwa_acitm>-qmein.
*    IF <lwa_acitm>-idactvl IS NOT INITIAL.
*      lwa_fmfpcnf-ism01 = <lwa_acitm>-menge.
*      lwa_fmfpcnf-leinh1 = <lwa_acitm>-qmein.
*    ENDIF.
*    IF <lwa_acitm>-idactve IS NOT INITIAL.
*      lwa_fmfpcnf-ism02 = <lwa_acitm>-menge.
*      lwa_fmfpcnf-leinh2 = <lwa_acitm>-qmein.
*    ENDIF.
**    lwa_fmfpcnf-pernr = lwa_acitm-lifnr.

****16/09/2016
    PERFORM confirmation_parameter_change USING <lwa_acitm>
                                       CHANGING  lwa_fmfpcnf.
****
    lwa_fmfpcnf-arbpl = <lwa_acitm>-arbpl.
    lwa_fmfpcnf-isdd  = <lwa_acitm>-strtdat.
*    gv_posting_date   = <lwa_acitm>-strtdat.
    gv_posting_date   = <lwa_acitm>-zzbudat.
    lwa_fmfpcnf-isdz  = <lwa_acitm>-strttim.
    lwa_fmfpcnf-iedd  = <lwa_acitm>-findat.
    lwa_fmfpcnf-iedz  = <lwa_acitm>-fintim.
    lwa_fmfpcnf-vornr = lwa_fpitm-vornr.
    lwa_fmfpcnf-ltxa1 = lwa_fpitm-ltxa1.
    lwa_fmfpcnf-budat = sy-datlo.
    APPEND lwa_fmfpcnf TO lt_fmfpcnf.

    CALL FUNCTION '/AGRI/FMFP_ORDER_CONFIRM'
      EXPORTING
*       i_commit_work     = space
        i_commit_work     = c_true
        it_fmfpcnf        = lt_fmfpcnf
        it_fmfpcom        = lt_fmfpcom
      IMPORTING
        e_subrc           = lv_subrc
        et_fpdoc          = lt_fpdoc
        et_messages       = lt_messages
      EXCEPTIONS ##FM_SUBRC_OK
        inconsistent_data = 1
        OTHERS            = 2.

    IF lv_subrc EQ 4.
      READ TABLE lt_messages TRANSPORTING NO FIELDS
        WITH KEY initiator = 'SAVE'
                 msgid     = 'M7'
                 msgno     = '022'
                 msgty     = 'E'
                 aplobj    = '/AGRI/FMFP'
                 subobj    = 'SAVE'.
      IF sy-subrc EQ 0.
        READ TABLE lt_fmfpcnf ASSIGNING FIELD-SYMBOL(<ls_fmfpcnf>) INDEX 1.
        IF sy-subrc EQ 0
        AND <ls_fmfpcnf>-aufnr IS NOT INITIAL.
          SELECT SINGLE igmng
            FROM afko
            INTO @DATA(lv_igmng)
           WHERE aufnr = @<ls_fmfpcnf>-aufnr.
          IF sy-subrc EQ 0
          AND lv_igmng IS NOT INITIAL.
            REFRESH lt_messages.
            CLEAR: <ls_fmfpcnf>-lmnga, lv_subrc.
            CALL FUNCTION '/AGRI/FMFP_ORDER_CONFIRM'
              EXPORTING
*               i_commit_work     = space
                i_commit_work     = c_true
                it_fmfpcnf        = lt_fmfpcnf
                it_fmfpcom        = lt_fmfpcom
              IMPORTING
                e_subrc           = lv_subrc
                et_fpdoc          = lt_fpdoc
                et_messages       = lt_messages
              EXCEPTIONS ##FM_SUBRC_OK
                inconsistent_data = 1
                OTHERS            = 2.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF lv_subrc = 0.
      WAIT UP TO 2 SECONDS.
      CLEAR: lt_fpdoc[], lt_fpitm[].
      PERFORM aufnr_infocus_read  USING lt_aufnr
                                  CHANGING lt_fpdoc.
      READ TABLE lt_fpdoc INTO lwa_fpdoc_infocus INDEX 1.
      lt_fpitm[] = lwa_fpdoc_infocus-x-fpitm[].
      READ TABLE lt_fpitm INTO lwa_fpitm WITH KEY arbpl = <lwa_acitm>-arbpl.
      <lwa_acitm>-rueck = lwa_fpitm-rueck.
*      SELECT SINGLE MAX( rmzhl ) rueck INTO (<lwa_acitm>-rmzhl, <lwa_acitm>-rueck)
*        FROM afru WHERE aufnr = <lwa_acitm>-aufnr
*        GROUP BY rueck.
      SELECT SINGLE MAX( rmzhl ) INTO <lwa_acitm>-rmzhl "#EC CI_SEL_NESTED
        FROM afru WHERE rueck =  <lwa_acitm>-rueck.
      <lwa_acitm>-status = c_process_status-cnf.
      IF <lwa_acitm>-updkz NE c_updkz_new.
        <lwa_acitm>-updkz = c_updkz_update.
      ENDIF.
    ELSE.
      lv_subrc2 = lv_subrc.
    ENDIF.


    LOOP AT lt_messages INTO lwa_message.
      MOVE lwa_message-context-context-value TO lwa_context.
      messages_context_data_set_new lwa_context-aufnr
                                    lwa_context-posnr space
                                    '/AGRI/S_FMFP_CONTEXT' lwa_context.
      MESSAGE ID lwa_message-msgid TYPE lwa_message-msgty
                                 NUMBER lwa_message-msgno
      WITH lwa_message-msgv1 lwa_message-msgv2 lwa_message-msgv3
      lwa_message-msgv4 INTO sy-msgli.
      message_simple space.
    ENDLOOP.
    CLEAR: lt_fmfpcnf[], lt_fmfpcom[], lt_fpdoc[],
           lt_messages[], lv_subrc, lwa_fmfpcnf.
  ENDLOOP.

  IF lv_subrc2 EQ 0.
    gs_acdoc_infocus-x-achdr-status = c_process_status-cnf .
    IF gs_acdoc_infocus-x-achdr-updkz NE c_updkz_new.
      gs_acdoc_infocus-x-achdr-updkz = c_updkz_update.
    ENDIF.
  ENDIF.

ENDFORM.                    " POSTINGS_CONFIRMATIONS

*&---------------------------------------------------------------------*
*&      Form  purchase_order_get
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM purchase_order_get.

  DATA: lt_items  TYPE /agri/t_fmacitm,
        lwa_items TYPE /agri/s_fmacitm.

  CHECK gs_acdoc_infocus-x-acitm[] IS NOT INITIAL.
  PERFORM agreements_po_confirm .
  gs_variables-refresh_items_grid = c_true.

ENDFORM.                    "purchase_order_get
*&---------------------------------------------------------------------*
*&      Form  PURCHASE_ITEMS_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LWA_TOAGROUP_EBELN  text
*      <--P_LT_PO_ITEMS  text
*      <--P_CALL  text
*      <--P_FUNCTION  text
*      <--P_'BAPI_PO_GETITEMS'  text
*      <--P_EXPORTING  text
*      <--P_PURCHASEORDER  text
*      <--P_=  text
*      <--P_LWA_TOAGROUP_EBELN  text
*      -->P_PO_ITEMS  text
*      -->P_=  text
*      -->P_LT_PO_ITEMS  text
*----------------------------------------------------------------------*
FORM purchase_items_get  USING    lv_po TYPE ebeln
                         CHANGING lt_po_items TYPE icl_t_bapiekpoc."bapiekpoc_tp.

  DATA: ls_return TYPE bapireturn,
        lt_return TYPE fmfg_t_bapireturn.

  CALL FUNCTION 'BAPI_PO_GETITEMS'
    EXPORTING
      purchaseorder = lv_po
    TABLES
      po_items      = lt_po_items
      return        = lt_return.

  LOOP AT lt_return INTO ls_return WHERE type EQ 'E'.
    MESSAGE ID ls_return-message
            TYPE ls_return-type
            NUMBER ls_return-log_msg_no
    WITH ls_return-message_v1 ls_return-message_v2
         ls_return-message_v3 ls_return-message_v4
            INTO sy-msgli.
    message_simple space.
  ENDLOOP.

  PERFORM messages_display USING gs_variables-initiator.
ENDFORM.                    "purchase_items_get
