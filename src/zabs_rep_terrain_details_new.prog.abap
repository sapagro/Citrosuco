************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name       :  ZABS_REP_TERRAIN_DETAILS                        *
* Tcode             :  ZABS_TRN_TERRAIN_DTL                            *
* Created By        :  Chandrakanth Karanam                            *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  11.06.2019                                      *
* TR                :  C4DK903782                                      *
* Version           :  002                                             *
* Description       :  Terrain Details                                 *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*
REPORT zabs_rep_terrain_details_new.

*--Global data declarations
INCLUDE zabs_rep_terrain_det_new_top.
*INCLUDE zabs_rep_terrain_details_top.

*--Processing data
INCLUDE zabs_rep_terrain_det_new_sub.
*INCLUDE zabs_rep_terrain_details_sub.

*&--------------------------------------------------------------------*
*&    AT SELECTION-SCREEN
*&--------------------------------------------------------------------*
*& Selection Screen Validations
*&--------------------------------------------------------------------*
AT SELECTION-SCREEN.

*--Selection Screen Validations
  PERFORM selection_validations.

*&--------------------------------------------------------------------*
*&    AT SELECTION-SCREEN
*&--------------------------------------------------------------------*
*& SELECTION-SCREEN value requests
*&--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_region.
*--Search Help for Period
  PERFORM f4_for_region.

*&--------------------------------------------------------------------*
*&    AT SELECTION-SCREEN
*&--------------------------------------------------------------------*
*& SELECTION-SCREEN value requests
*&--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
*--Search Help for Path
  PERFORM file_path.

*&--------------------------------------------------------------------*
*&    START-OF-SELECTION
*&--------------------------------------------------------------------*
*& Preparing Terrain Data
*&--------------------------------------------------------------------*
START-OF-SELECTION.

*--Initializing Global Data
  PERFORM initialize_global_data.

*--Preparing Terrain Data
  PERFORM build_terrain_data.

*--Getting Adobe Form Function Module
  PERFORM terrains_data_sfp.

*--Dowmloading Pdf
  PERFORM dowmload_pdf.

**--Sending Adobe form as an attachment in a mail
*  PERFORM mail_attachment.
