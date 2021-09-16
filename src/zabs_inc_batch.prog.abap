************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Enhancement Name  :  ZABS_IENH_NURSERY_PROCESS                       *
* Form Name         :  FCODE_CBCH                                      *
* Include Name      :  ZABS_INC_BATCH                                  *
* Created By        :  Chandrakanth Karanam                            *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  06.21.2019                                      *
* TR                :  C4DK901784                                      *
* Version           :  001                                             *
* Description       :  Auto batch number generation and popup to enter *
*                      the quantity while creating the batch           *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

DATA: ls_str_batch TYPE zabs_str_batch,  "LOC for batch creation
      ls_batch     TYPE /agri/s_fmfpbch,
      ls_fpbch     TYPE /agri/s_fmfpbch,
      lv_subrc_new TYPE sy-subrc,
      lv_error     TYPE c,
      lv_canc      TYPE abap_bool VALUE abap_false.

CLEAR: /agri/s_fmfpscrfields-charg.

SET PARAMETER ID 'MAT' FIELD /agri/s_fmfphdr-matnr.

SELECT SINGLE meins
  FROM mara
  INTO @DATA(lv_uom)
  WHERE matnr EQ @/agri/s_fmfphdr-matnr.

*----SOC Creating batch using custom button---------*
* CALL SCREEN c_screen-create_batch STARTING AT 25 5.
ls_str_batch-matnr = /agri/s_fmfphdr-matnr.
ls_str_batch-iwerk = /agri/s_fmfphdr-iwerk.
ls_str_batch-meins = lv_uom.
ls_str_batch-date = sy-datum.

CALL FUNCTION 'ZABS_FM_BATCH'
  IMPORTING
    ev_canceled = lv_canc
  CHANGING
    cs_batch    = ls_str_batch
    cv_error    = lv_error.

IF lv_canc EQ abap_false.
  IF lv_error IS NOT INITIAL.
*-- Lote não criado
    MESSAGE s155(zfmfp)
    DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_error. "'E'
    RETURN.
  ENDIF.

  /agri/s_fmfpscrfields-charg = ls_str_batch-charg.
  /agri/s_fmfpscrfields-gmein = ls_str_batch-meins.
*----EOC Creating batch using custom button---------*

  CHECK /agri/s_fmfpscrfields-charg IS NOT INITIAL.

  gs_variables-initiator = c_log_initiator-mass.
  PERFORM messages_initialize USING gs_variables-initiator
                                    c_log_subobject-create.

  PERFORM create_batch USING gs_fpdoc_infocus-x-fphdr
                             /agri/s_fmfpscrfields-charg
                    CHANGING lv_subrc_new.
  IF lv_subrc_new EQ 0.
    gs_variables-refresh_batches_grid = c_true.
    CLEAR: ls_batch.
    ls_batch-aufnr = gs_fpdoc_infocus-x-fphdr-aufnr.
    LOOP AT gs_fpdoc_infocus-x-fpbch INTO ls_fpbch.
      IF ls_batch-contr LT ls_fpbch-contr.
        ls_batch-contr = ls_fpbch-contr.
      ENDIF.
    ENDLOOP.
    ls_batch-contr   = ls_batch-contr + 1.
    ls_batch-matnr   = gs_fpdoc_infocus-x-fphdr-matnr.
    ls_batch-charg   = /agri/s_fmfpscrfields-charg.
    ls_batch-erfmg   = ls_str_batch-erfmg. "gs_fpdoc_infocus-x-fphdr-gamng.
    ls_batch-erfme   = /agri/s_fmfpscrfields-gmein.
*   ls_batch-erdat   = ls_str_batch-date.
    ls_batch-erdat   = sy-datum.
    ls_batch-budat   = ls_str_batch-date.
    ls_batch-zzbudat = ls_str_batch-date.
    ls_batch-updkz   = c_updkz_new.
    ls_batch-zzmdocm = ls_str_batch-mdocm.
    APPEND ls_batch TO gs_fpdoc_infocus-x-fpbch.
    gs_variables-document_changed = c_true.
    PERFORM document_infocus_save USING c_true
                      CHANGING gs_fpdoc_infocus
                               lv_subrc_new.
  ENDIF.

  PERFORM messages_display USING gs_variables-initiator.
  IF lv_subrc_new EQ 0.
    PERFORM document_infocus_set USING gs_fpdoc_infocus-aufnr.
  ENDIF.
ENDIF.

RETURN.
