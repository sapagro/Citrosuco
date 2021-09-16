*&---------------------------------------------------------------------*
*& Report ZABS_REP_MEASUREM_DOC_CREATE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabs_rep_measurem_doc_create.

INCLUDE zabs_rep_measurem_doc_creattop.
INCLUDE zabs_rep_measurem_doc_creatf01.

*&--------------------------------------------------------------------*
*&    SELECTION-SCREEN
*&--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_per TYPE sy-datum OBLIGATORY.
SELECT-OPTIONS: s_region FOR /agri/glflatv-atwrt NO-DISPLAY.

SELECTION-SCREEN  BEGIN OF LINE.
PARAMETERS: rb_lep RADIOBUTTON GROUP rb1 USER-COMMAND xy DEFAULT 'X'.
SELECTION-SCREEN COMMENT 3(50) TEXT-014.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: rb_lpp RADIOBUTTON GROUP rb1.
SELECTION-SCREEN COMMENT 3(50) TEXT-015.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.

*&--------------------------------------------------------------------*
*&    START-OF-SELECTION
*&--------------------------------------------------------------------*
*& Check Measurement Document's Data
*&--------------------------------------------------------------------*
START-OF-SELECTION.
*-- Initializing Global Data
  PERFORM initialize_global_data.
*-- Fill Header Data from the Measurement Document
  PERFORM data_get USING p_per
                         rb_lep
                         rb_lpp
                CHANGING gt_sheet
                         s_region[].

*&--------------------------------------------------------------------*
*& END-OF-SELECTION
*&--------------------------------------------------------------------*
*& Create Measurement Document
*&--------------------------------------------------------------------*
END-OF-SELECTION.
*-- Create Measurement Document
  PERFORM measurement_doc_create USING gt_sheet.

  IF gt_message[] IS NOT INITIAL.
    IF gs_variables-initiator IS INITIAL.
      gs_variables-initiator = c_log_initiator-save.
      PERFORM messages_initialize USING gs_variables-initiator
                                        c_log_subobject-save.
      PERFORM message_add_table.
      PERFORM messages_display USING gs_variables-initiator.

    ENDIF.
  ENDIF.
