FUNCTION zabs_fm_get_grid_refresh .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     REFERENCE(EV_GRID_REFRESH) TYPE  XFELD
*"----------------------------------------------------------------------
************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* FM Name           :  ZABS_FM_GET_GRID_REFRESH                        *
* Created By        :  Chandrakanth Karanam                            *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  06.26.2019                                      *
* TR                :  C4DK901784                                      *
* Version           :  001                                             *
* Description       :  grid refresh                                    *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

  ev_grid_refresh = gv_grid_refresh.

ENDFUNCTION.
