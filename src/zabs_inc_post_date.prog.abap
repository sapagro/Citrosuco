*&---------------------------------------------------------------------*
*& Include ZABS_INC_POST_DATE
*&---------------------------------------------------------------------*

IF lv_mvt_code = '02'.

  DATA: lv_objectkey       TYPE bapi1003_key-object,
        lv_classnum        TYPE bapi1003_key-classnum,
        lt_allocvaluesnum  TYPE tt_bapi1003_alloc_values_num,
        lt_allocvalueschar TYPE tt_bapi1003_alloc_values_char,
        lt_allocvaluescurr TYPE tt_bapi1003_alloc_values_curr,
        ls_fmfpcnf         TYPE /agri/s_fmfp_cnf,
        ls_allocvaluesnum  TYPE bapi1003_alloc_values_num,
        lv_float           TYPE cawn-atflv,
        lv_atwrt           TYPE cawn-atwrt.

  lv_classnum = zcl_abs_abap_maintain=>c_classnum.

  CONCATENATE gs_fpdoc_infocus-x-fphdr-matnr gs_variables-batch_infocus
  INTO lv_objectkey RESPECTING BLANKS.

  CLEAR ls_fmfpcnf.
  READ TABLE gt_fmfpcnf INTO ls_fmfpcnf INDEX 1.
  IF sy-subrc EQ 0 AND ls_fmfpcnf-budat IS NOT INITIAL.
    CLEAR ls_allocvaluesnum.
    ls_allocvaluesnum-charact = zcl_abs_abap_maintain=>c_charact_psdat.
    lv_atwrt = ls_fmfpcnf-budat.
    CALL FUNCTION 'CTCV_CONVERT_DATE_TO_FLOAT'
      EXPORTING
        date  = lv_atwrt
      IMPORTING
        float = lv_float.
    ls_allocvaluesnum-value_from = lv_float.
    APPEND ls_allocvaluesnum TO lt_allocvaluesnum.
  ENDIF.

  CALL FUNCTION 'BAPI_OBJCL_CHANGE'
    EXPORTING
      objectkey          = lv_objectkey
      objecttable        = 'MCH1'
      classnum           = lv_classnum
      classtype          = '023'
    TABLES
      allocvaluesnumnew  = lt_allocvaluesnum
      allocvaluescharnew = lt_allocvalueschar
      allocvaluescurrnew = lt_allocvaluescurr
      return             = lt_return.

  LOOP AT lt_return INTO lwa_return
                   WHERE type EQ c_msg_type-error.
    MESSAGE ID lwa_return-id
       TYPE lwa_return-type
     NUMBER lwa_return-number
       WITH lwa_return-message_v1 lwa_return-message_v2
            lwa_return-message_v3 lwa_return-message_v4
       INTO sy-msgli.
    message_simple space.
  ENDLOOP.

ENDIF.
