FUNCTION zfmfp_task_order_create.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(STRNO) TYPE  /AGRI/GLSTRNO OPTIONAL
*"     VALUE(MATKL) TYPE  MATKL OPTIONAL
*"     VALUE(EXTWG) TYPE  EXTWG OPTIONAL
*"     VALUE(MATNR) TYPE  MATNR OPTIONAL
*"     VALUE(PMATNR) TYPE  MATNR OPTIONAL
*"     VALUE(ACNUM) TYPE  ZFMACNUM OPTIONAL
*"     VALUE(AJAHR) TYPE  AJAHR OPTIONAL
*"     VALUE(ACTYP) TYPE  ZFMACTYP OPTIONAL
*"     VALUE(DATA) TYPE  BEGDA OPTIONAL
*"  EXCEPTIONS
*"      INCONSISTENT_DATA
*"      NO_DATA
*"      NO_MATERIAL
*"      NO_ORDER
*"      ORDER_CANNOT_BE_LOCKED
*"----------------------------------------------------------------------

  DATA: lv_matkl TYPE matkl,
        lv_extwg TYPE extwg,
        lv_strno TYPE /agri/glstrno,
        lv_acnum TYPE zfmacnum,
        lv_ajahr TYPE ajahr,
        lv_data  TYPE begda,
        lv_actyp TYPE ZFMACTYP.

  lv_strno = strno.
  lv_acnum = acnum.
  lv_ajahr = ajahr.
  lv_actyp = actyp.
  lv_matkl = matkl.
  lv_extwg = extwg.
  lv_data  = data.

  PERFORM document_data_initialize
    IN PROGRAM zfmfp_task_order_create USING abap_true IF FOUND.

  PERFORM messages_initialize
    IN PROGRAM zfmfp_task_order_create USING c_log_initiator-create
                                             c_log_subobject-create IF FOUND.

  PERFORM check_material
    IN PROGRAM zfmfp_task_order_create USING lv_strno
                                             lv_acnum
                                             lv_actyp
                                             lv_ajahr
                                             lv_data
                                             lv_matkl
                                             lv_extwg
                                    CHANGING gv_process_material
                                             gv_subrc IF FOUND.

  PERFORM read_order
    IN PROGRAM zfmfp_task_order_create USING lv_strno
                                             gv_process_material
                                    CHANGING gt_fmfphdr
                                             gt_fpdoc
                                             gv_subrc IF FOUND.

  IF gv_subrc EQ 4.
    RAISE no_order.
  ENDIF.

  READ TABLE gt_fpdoc ASSIGNING FIELD-SYMBOL(<lwa_fpdoc>) INDEX 1.
  IF sy-subrc EQ 0.
    PERFORM document_infocus_lock
      IN PROGRAM zfmfp_task_order_create USING <lwa_fpdoc>-aufnr
                                      CHANGING gv_subrc IF FOUND.
    IF gv_subrc IS NOT INITIAL.
      RAISE order_cannot_be_locked.
    ENDIF.

    IF sy-uname EQ 'T_T.KONNO'.
      BREAK-POINT.
    ENDIF.
    PERFORM create_task_orders
      IN PROGRAM zfmfp_task_order_create USING lv_matkl
                                               gt_fmfphdr
                                      CHANGING gt_fpdoc IF FOUND.

    PERFORM document_infocus_unlock
      IN PROGRAM zfmfp_task_order_create USING <lwa_fpdoc>-aufnr
                                               <lwa_fpdoc>-x-fphdr-rsnum IF FOUND.

    PERFORM messages_display
      IN PROGRAM zfmfp_task_order_create USING c_log_initiator-create IF FOUND.
  ENDIF.

ENDFUNCTION.
