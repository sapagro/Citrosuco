*&---------------------------------------------------------------------*
*& Report ZFMFP_TASK_JOB_PROC
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfmfp_task_job_proc.

INCLUDE zfmfp_inc_task_upload.


DATA lt_seltexts LIKE STANDARD TABLE OF rsseltexts.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME.
SELECT-OPTIONS:  s_ernam     FOR zfmfp_task_sheet-ernam,
                 s_erdat     FOR zfmfp_task_sheet-erdat,
                 s_zrow    FOR zfmfp_task_sheet-zrow,
                 s_erzet     FOR zfmfp_task_sheet-erzet,
                 s_aenam     FOR zfmfp_task_sheet-aenam,
                 s_aedat     FOR zfmfp_task_sheet-aedat,
                 s_aezet     FOR zfmfp_task_sheet-aezet,
                 p_proc      FOR zfmfp_task_sheet-processed.
SELECTION-SCREEN END OF BLOCK blk1.

SELECTION-SCREEN FUNCTION KEY 1.
SELECTION-SCREEN FUNCTION KEY 2.
SELECTION-SCREEN FUNCTION KEY 3.

INITIALIZATION.
  PERFORM screen_update.

AT SELECTION-SCREEN OUTPUT.
  PERFORM screen_update.

START-OF-SELECTION.
  PERFORM read_sheet_to_db CHANGING gt_excel
                                    gt_msgs
                                    s_ernam[]
                                    s_erdat[]
                                    s_zrow[]
                                    s_erzet[]
                                    s_aenam[]
                                    s_aedat[]
                                    s_aezet[]
                                    p_proc[].

  PERFORM tasks_create_via_submit USING gt_excel
                               CHANGING gt_msgs.

  IF gs_variables-initiator IS INITIAL.
    gs_variables-initiator = c_log_initiator-save.
    PERFORM messages_initialize USING gs_variables-initiator
                                      c_log_subobject-save.
    DATA: lwa_message LIKE LINE OF gt_message.
    LOOP AT gt_bdc_messages INTO DATA(lwa_bdc_message).
      DATA(lwa_bdc_msg) = lwa_bdc_message.
      AT NEW row.
        INSERT INITIAL LINE INTO TABLE gt_message
          ASSIGNING FIELD-SYMBOL(<lwa_message>).
        IF sy-subrc EQ 0.
*"        Linha Excel &1:
          <lwa_message>-msgid = 'ZFMFP'.
          <lwa_message>-msgno = '003'.
          <lwa_message>-msgty = lwa_bdc_msg-msgtyp.
          <lwa_message>-msgv1 = lwa_bdc_msg-row.
        ENDIF.
      ENDAT.
      READ TABLE gt_message TRANSPORTING NO FIELDS
        WITH KEY msgid = lwa_bdc_msg-msgid
                 msgno = lwa_bdc_msg-msgnr
                 msgty = lwa_bdc_msg-msgtyp
                 msgv1 = lwa_bdc_msg-msgv1
                 msgv2 = lwa_bdc_msg-msgv2
                 msgv3 = lwa_bdc_msg-msgv3
                 msgv4 = lwa_bdc_msg-msgv4.
      IF sy-subrc NE 0.
        INSERT INITIAL LINE INTO TABLE gt_message
          ASSIGNING <lwa_message>.
        IF sy-subrc EQ 0.
          <lwa_message>-msgid = lwa_bdc_msg-msgid.
          <lwa_message>-msgno = lwa_bdc_msg-msgnr.
          <lwa_message>-msgty = lwa_bdc_msg-msgtyp.
          <lwa_message>-msgv1 = lwa_bdc_msg-msgv1.
          <lwa_message>-msgv2 = lwa_bdc_msg-msgv2.
          <lwa_message>-msgv3 = lwa_bdc_msg-msgv3.
          <lwa_message>-msgv4 = lwa_bdc_msg-msgv4.
        ENDIF.
      ENDIF.
    ENDLOOP.

    PERFORM message_add_table.
    PERFORM messages_display USING gs_variables-initiator.
  ENDIF.
