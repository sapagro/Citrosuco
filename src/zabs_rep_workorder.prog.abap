************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name       :  ZABS_REP_WORKORDER                              *
* Tcode             :  ZABS_TRN_IORD_CRT                               *
* Created By        :  Helio Kitagami Kababe                           *
* Requested by      :  Daniele Jane                                    *
* Created on        :  05.31.2021                                      *
* TR                :                                                  *
* Version           :  001                                             *
* Description       :  WorkOrder                                       *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*
REPORT zabs_rep_workorder.

*-- Global data declarations
INCLUDE zabs_rep_workorder_top.

*-- Processing data
INCLUDE zabs_rep_workorder_sub.

*&--------------------------------------------------------------------*
*&    START-OF-SELECTION
*&--------------------------------------------------------------------*
*& Preparing WorkOrder Data
*&--------------------------------------------------------------------*
START-OF-SELECTION.
  IF sy-uname EQ 'T_H.KABABE'.
    BREAK-POINT.
  ENDIF.

*-- Initializing Global Data
  PERFORM initialize_global_data.
*-- Read irrigation and equipments data
  PERFORM fetch_irrigation_data.

*&---------------------------------------------------------------------*
*& END-OF-SELECTION
*& Create WorkOrder
*&---------------------------------------------------------------------*
END-OF-SELECTION.
  IF sy-uname EQ 'T_H.KABABE'.
    BREAK-POINT.
  ENDIF.

*-- Create WorkOrder
  PERFORM create_workorder.

  IF gt_message[] IS NOT INITIAL.
    DELETE ADJACENT DUPLICATES FROM gt_message COMPARING ALL FIELDS.
    IF gs_variables-initiator IS INITIAL.
      gs_variables-initiator = c_log_initiator-save.
      PERFORM messages_initialize USING gs_variables-initiator
                                        c_log_subobject-save.
      PERFORM message_add_table.
      PERFORM display_messages USING gs_variables-initiator.
    ENDIF.
  ENDIF.
