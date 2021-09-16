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
REPORT zabs_rep_detalhes_laudo.

*--Global data declarations
INCLUDE zabs_rep_detalhes_laudo_top.

*--Processing data
INCLUDE zabs_rep_detalhes_laudo_sub.

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
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_region-low.
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

*-- BOC-T_T.KONNO
*--Parameters Validations
  PERFORM parameters_validations CHANGING gv_subrc.
*-- EOC-T_T.KONNO

*--Preparing Terrain Data
  PERFORM build_terrain_data CHANGING gv_subrc.

*--BOC- T_T.KONNO-05.27.21
**--Getting Adobe Form Function Module
*  PERFORM terrains_data_sfp_reg. " Added in Wave 3 changes
**--Downloading Pdf
*  PERFORM download_pdf.
  IF p_print EQ abap_true.
*--Getting Adobe Form Function Module
    PERFORM terrains_data_sfp_reg. " Added in Wave 3 changes
*--Downloading PDF
    PERFORM download_pdf.
  ELSE.
**--Getting Adobe Form Function Module
*    PERFORM terrains_data_sfp_reg. " Added in Wave 3 changes
**--Creating a PDF File
*    PERFORM create_pdf.
    EXPORT gt_farm_subtot TO MEMORY ID 'ZVTX_FARM_SUBTOT'.
    EXPORT gt_terrain_dtls TO MEMORY ID 'ZVTX_TERRAIN_DTLS'.
  ENDIF.
*--EOC- T_T.KONNO-05.27.21
