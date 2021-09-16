************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name    : ZABS_REP_WEIGHBRIDGE                                *
* Tcode          : ZABS_BALANCA                                        *
* Created By     : Helio Kababe                                        *
* Requested by   : Ricardo Genovez                                     *
* Created on     : 09.03.2020                                          *
* TR             : C4DK909167                                          *
* Version        : 001                                                 *
* Description    : Weighbridge data declaration and Selection Screen   *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*
REPORT zabs_rep_weighbridge.

INCLUDE zabs_rep_weighbridge_top.

INCLUDE zabs_rep_weighbridge_sub.

*&--------------------------------------------------------------------*
*& START-OF-SELECTION
*&--------------------------------------------------------------------*
START-OF-SELECTION.

*--Selection Screen Validations
  PERFORM selection_validations.

*--Initializing Global Data
  PERFORM initialize_global_data.

*--Preparing Weighbridge Data
  PERFORM get_weighbridge_data.

*&--------------------------------------------------------------------*
*& END-OF-SELECTION
*&--------------------------------------------------------------------*
*& Display Resource Data
*&--------------------------------------------------------------------*
END-OF-SELECTION.

*--Display Weighbridge Data
  PERFORM display_weighbridge_data.
