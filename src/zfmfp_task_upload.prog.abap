*&---------------------------------------------------------------------*
*& Report ZFMFP_TASK_UPLOAD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfmfp_task_upload.

*DATA: BEGIN OF gt_verid OCCURS 0,
*        verid TYPE verid,
*      END OF gt_verid.
*
*DATA: gt_taskorder      TYPE /agri/t_fmfp_uptask_mass,
*      gt_taskorder_fcat TYPE /agri/t_fmfp_uptask_mass_fcat,
*      gt_csdoc_infocus  TYPE /agri/t_glcs_doc,
*      gt_flcma          TYPE /agri/t_glflcma,
*      gt_tplnr          TYPE /agri/t_gltplnr.

INCLUDE zfmfp_inc_task_upload.

**"Informações do Arquivo/File Info
SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_fname TYPE char255.
SELECTION-SCREEN END OF BLOCK blk2.

SELECTION-SCREEN FUNCTION KEY 1.
SELECTION-SCREEN FUNCTION KEY 2.
SELECTION-SCREEN FUNCTION KEY 3.

INITIALIZATION.
  PERFORM screen_update.

AT SELECTION-SCREEN OUTPUT.
  PERFORM screen_update.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  PERFORM file_path_get CHANGING p_fname.

START-OF-SELECTION.
  IF p_fname IS INITIAL.
    MESSAGE i055(00) DISPLAY LIKE 'E'.
  ELSE.
    PERFORM file_extension_get USING p_fname
                            CHANGING gv_file_extension
                                     gv_subrc.

    PERFORM excel_data_import USING p_fname
                                    gv_file_extension
                           CHANGING gt_excel
                                    gt_bdc_messages
                                    gv_subrc.

    IF gs_variables-initiator IS INITIAL.
      gs_variables-initiator = c_log_initiator-save.
      PERFORM messages_initialize USING gs_variables-initiator
                                        c_log_subobject-save.
*    DATA: lwa_message LIKE LINE OF gt_message.
*    LOOP AT gt_bdc_messages INTO DATA(lwa_bdc_message).
*      DATA(lwa_bdc_msg) = lwa_bdc_message.
*      AT NEW row.
*        INSERT INITIAL LINE INTO TABLE gt_message
*          ASSIGNING FIELD-SYMBOL(<lwa_message>).
*        IF sy-subrc EQ 0.
**"        Linha Excel &1:
*          <lwa_message>-msgid = 'ZFMFP'.
*          <lwa_message>-msgno = '003'.
*          <lwa_message>-msgty = lwa_bdc_msg-msgtyp.
*          <lwa_message>-msgv1 = lwa_bdc_msg-row.
*        ENDIF.
*      ENDAT.
*      READ TABLE gt_message TRANSPORTING NO FIELDS
*        WITH KEY msgid = lwa_bdc_msg-msgid
*                 msgno = lwa_bdc_msg-msgnr
*                 msgty = lwa_bdc_msg-msgtyp
*                 msgv1 = lwa_bdc_msg-msgv1
*                 msgv2 = lwa_bdc_msg-msgv2
*                 msgv3 = lwa_bdc_msg-msgv3
*                 msgv4 = lwa_bdc_msg-msgv4.
*      IF sy-subrc NE 0.
*        INSERT INITIAL LINE INTO TABLE gt_message
*          ASSIGNING <lwa_message>.
*        IF sy-subrc EQ 0.
*          <lwa_message>-msgid = lwa_bdc_msg-msgid.
*          <lwa_message>-msgno = lwa_bdc_msg-msgnr.
*          <lwa_message>-msgty = lwa_bdc_msg-msgtyp.
*          <lwa_message>-msgv1 = lwa_bdc_msg-msgv1.
*          <lwa_message>-msgv2 = lwa_bdc_msg-msgv2.
*          <lwa_message>-msgv3 = lwa_bdc_msg-msgv3.
*          <lwa_message>-msgv4 = lwa_bdc_msg-msgv4.
*        ENDIF.
*      ENDIF.
*   ENDLOOP.

      PERFORM message_add_table.
      PERFORM messages_display USING gs_variables-initiator.
    ENDIF.
  ENDIF.
