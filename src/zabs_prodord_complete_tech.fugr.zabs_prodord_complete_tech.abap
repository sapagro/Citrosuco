FUNCTION zabs_prodord_complete_tech.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(SCOPE_COMPL_TECH) TYPE
*"        BAPI_ORDER_FUNC_CNTRL-SCOPE_COMPL_TECH DEFAULT '1'
*"     VALUE(WORK_PROCESS_GROUP) TYPE
*"        BAPI_ORDER_CNTRL_PARAM-WORK_PROC_GROUP DEFAULT 'COWORK_BAPI'
*"     VALUE(WORK_PROCESS_MAX) TYPE
*"        BAPI_ORDER_CNTRL_PARAM-WORK_PROC_MAX DEFAULT 99
*"  EXPORTING
*"     VALUE(RETURN) TYPE  BAPIRET2
*"  TABLES
*"      ORDERS STRUCTURE  BAPI_ORDER_KEY
*"      DETAIL_RETURN STRUCTURE  BAPI_ORDER_RETURN OPTIONAL
*"      APPLICATION_LOG STRUCTURE  BAPI_ORDER_APPLICATION_LOG OPTIONAL
*"----------------------------------------------------------------------

  DATA: lt_fpdoc   TYPE /agri/t_fmfp_doc,
        lt_fphdr   TYPE /agri/t_fmfphdr,
        lwa_fphdr  TYPE /agri/s_fmfphdr,
        lwa_orders TYPE bapi_order_key,
        lt_orders  TYPE TABLE OF bapi_order_key,
        lwa_status TYPE bapiret2,
        lwa_return TYPE bapi_coru_return,
        lt_return  TYPE TABLE OF bapi_coru_return,
        lwa_fpdoc  TYPE /agri/s_fmfp_doc,
        lt_aufnr   TYPE /agri/t_fmaufnr,
        lwa_aufnr  TYPE /agri/s_fmaufnr,
        lv_msgv1   TYPE sy-msgv1,
        lv_subrc   TYPE sy-subrc.

  READ TABLE orders INTO DATA(lwa_order) INDEX 1.
  IF sy-subrc EQ 0.
    lwa_aufnr-aufnr = lwa_order-order_number.
    APPEND lwa_aufnr TO lt_aufnr.

    CALL FUNCTION '/AGRI/FMFP_READ'
      EXPORTING
        it_aufnr       = lt_aufnr
      IMPORTING
        et_fmfphdr     = lt_fphdr
      EXCEPTIONS ##FM_SUBRC_OK
        no_data_exists = 1
        OTHERS         = 2.

    IF sy-subrc EQ 0.
      CALL FUNCTION 'ENQUEUE_ESORDER'
        EXPORTING
          aufnr          = lwa_aufnr-aufnr
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      lv_subrc = sy-subrc.

      IF lv_subrc IS NOT INITIAL.
        lv_msgv1 = sy-msgv1.
        INSERT INITIAL LINE INTO TABLE lt_return
          ASSIGNING FIELD-SYMBOL(<lwa_return>).
        IF sy-subrc EQ 0.
          <lwa_return>-type       = 'E'.
          <lwa_return>-id	        = '/AGRI/FMFP'.
          <lwa_return>-number     = 37.
          <lwa_return>-message_v1 = lwa_aufnr-aufnr.
          <lwa_return>-message_v2 = lv_msgv1.
        ENDIF.
        EXIT.
      ENDIF.

      IF lv_subrc EQ 0.
        PERFORM zorders_technical_complete IN PROGRAM /agri/saplfmfpm
                                             CHANGING lt_fphdr
                                                      lt_fpdoc
                                                      lt_return.
      ENDIF.

      CALL FUNCTION 'DEQUEUE_ESORDER'
        EXPORTING
          aufnr     = lwa_aufnr-aufnr
          _synchron = abap_true.
    ELSE.
      INSERT INITIAL LINE INTO TABLE lt_return
        ASSIGNING <lwa_return>.
      IF sy-subrc EQ 0.
        <lwa_return>-type = 'E'.
        <lwa_return>-id	 = '/AGRI/GLOBAL'.
        <lwa_return>-number = 751.
        MESSAGE ID <lwa_return>-id TYPE <lwa_return>-type
       NUMBER <lwa_return>-number INTO <lwa_return>-message.
      ENDIF.
    ENDIF.
  ENDIF.

  orders[] = lt_orders[].
  detail_return[] = lt_return[].

ENDFUNCTION.
