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
REPORT zabs_rep_terrain_details.

*--Global data declarations
INCLUDE zabs_rep_terrain_details_top.

*--Processing data
INCLUDE zabs_rep_terrain_details_sub.

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
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_region-low. " p_region.
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

*-- BOC - 12/03/2020 By T_T.KONNO
*--Parameters Validations
  PERFORM parameters_validations.
*-- EOC - 12/03/2020 By T_T.KONNO

*--Preparing Terrain Data
  PERFORM build_terrain_data.

*-- BOC - 12/03/2020 By T_T.KONNO
**--Getting Adobe Form Function Module
***  PERFORM terrains_data_sfp. " Commented in Wave 3 changes
*  PERFORM terrains_data_sfp_copy. " Added in Wave 3 changes
**  PERFORM terrains_data_sfp_reg. " Added in Wave 3 changes
*
**--Dowmloading Pdf
*  PERFORM dowmload_pdf.
*
***--Sending Adobe form as an attachment in a mail
**  PERFORM mail_attachment.
  IF p_print EQ abap_true.
*--Getting Adobe Form Function Module
    PERFORM terrains_data_sfp_copy. " Added in Wave 3 changes
*--Dowmloading Pdf
    PERFORM dowmload_pdf.
  ELSE.
    EXPORT gt_farm_subtot TO MEMORY ID 'ZVTX_FARM_SUBTOT'.
    EXPORT gt_terrain_dtls TO MEMORY ID 'ZVTX_TERRAIN_DTLS'.
    EXPORT gv_print TO MEMORY ID 'ZVTX_PRINT'.

    PERFORM terrains_data_sfp_copy.
    EXPORT gt_att_content_hex from gt_att_content_hex to MEMORY ID 'ZVTX_ATT_HEX'.
  ENDIF.
*-- EOC - 12/03/2020 By T_T.KONNO
