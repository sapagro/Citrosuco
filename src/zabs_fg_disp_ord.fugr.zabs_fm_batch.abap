FUNCTION zabs_fm_batch.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  EXPORTING
*"     REFERENCE(EV_CANCELED) TYPE  BOOLE_D
*"  CHANGING
*"     REFERENCE(CS_BATCH) TYPE  ZABS_STR_BATCH
*"     REFERENCE(CV_ERROR) TYPE  C
*"----------------------------------------------------------------------
************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* FM Name           :  ZABS_FM_BATCH                                   *
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

*--Workarea declarations
  DATA : ls_var TYPE ty_var.

  CLEAR: zabs_str_batch, gv_canc.
  zabs_str_batch-matnr = cs_batch-matnr.
  zabs_str_batch-iwerk = cs_batch-iwerk.
  zabs_str_batch-meins = cs_batch-meins.
  zabs_str_batch-date  = cs_batch-date.

*--Perform Number Range
  PERFORM number_range CHANGING ls_var.

  CONCATENATE ls_var-ctext ls_var-next_no INTO cs_batch-charg.  "'FAZ'

  zabs_str_batch-charg = cs_batch-charg.

  CLEAR gs_variables-error.
*--Calling screen to create batch
  CALL SCREEN 0201 STARTING AT 25 5.

  cs_batch-charg = zabs_str_batch-charg.
  cs_batch-erfmg = zabs_str_batch-erfmg.
  cs_batch-meins = zabs_str_batch-meins.
  cs_batch-date  = zabs_str_batch-date.
*  cs_batch-mdocm = zabs_str_batch-mdocm.
  cv_error       = gs_variables-error.
  ev_canceled    = gv_canc.

ENDFUNCTION.
