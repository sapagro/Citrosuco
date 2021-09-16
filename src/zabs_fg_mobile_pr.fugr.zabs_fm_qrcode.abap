FUNCTION zabs_fm_qrcode.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(CV_QRCODE) TYPE  ZABS_DEL_QRCODE
*"----------------------------------------------------------------------
************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* FM Name           :  ZABS_FM_QRCOD                                   *
* Created By        :  Chandrakanth Karanam                            *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  04.06.2020                                      *
* TR                :  C4DK916492                                      *
* Version           :  001                                             *
* Description       :  Reading QRCODE data                             *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

  CLEAR gv_qrcode.

  zzqrcode = cv_qrcode.
  gv_qrcode = cv_qrcode.

*--Calling screen to read QRCODE
  CALL SCREEN 0100 STARTING AT 25 5.

  cv_qrcode = zzqrcode.

*  gs_variables-refresh_items_grid = c_true.

*  REFRESH ref_grid_items

ENDFUNCTION.
