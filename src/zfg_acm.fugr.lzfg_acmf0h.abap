*&---------------------------------------------------------------------*
*& Include          LZFG_ACMF0H
*&---------------------------------------------------------------------*
FORM header_data_display .
  DATA: lwa_acdes TYPE /agri/s_fmachdrt.
  CLEAR: /agri/s_fmachdr.
  MOVE-CORRESPONDING: gs_acdoc_infocus-x-achdr TO /agri/s_fmachdr.
ENDFORM.                    " HEADER_DATA_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  HEADER_DATA_UPDATE
*&---------------------------------------------------------------------*
FORM header_data_update_1 .

  DATA: ls_fmachdr     TYPE /agri/fmachdr,
        lv_activity(2) TYPE c,
        lv_subrc       TYPE sy-subrc..

  CHECK gs_variables-document_mode NE c_mode_display
    AND gs_variables-document_mode NE space.

  IF gs_variables-document_mode EQ c_mode_create.
    MOVE c_authorization_activity-create TO lv_activity.
  ELSEIF gs_variables-document_mode EQ c_mode_change.
    MOVE c_authorization_activity-change TO lv_activity.
  ENDIF.

*--Call Authority Check
  PERFORM authority_check USING gs_acdoc_infocus-x-achdr
                                lv_activity
                                c_true
                       CHANGING lv_subrc.
  IF lv_subrc NE 0.

    IF lv_activity EQ c_authorization_activity-change.
      gs_variables-document_mode = c_mode_display.
      EXIT.
    ELSE.

      CLEAR: gs_variables-document_mode,
             gs_acdoc_infocus, /agri/s_fmachdr.
      gs_variables-errors = c_true.
      EXIT.
    ENDIF.
  ENDIF.

  MOVE-CORRESPONDING gs_acdoc_infocus-x-achdr TO ls_fmachdr.

  /agri/s_fmachdr-actyp =  p_actyp.
  /agri/s_fmachdr-accom =  p_accom.
  /agri/s_fmachdr-bukrs =  p_bukrs.
  /agri/s_fmachdr-werks =  p_werks.
  /agri/s_fmachdr-strtdat =  p_strdat.
  /agri/s_fmachdr-findat =  p_findat.
  /agri/s_fmachdr-wonum =  p_wonum.
  /agri/s_fmachdr-descr =  p_descr.
  /agri/s_fmachdr-gjahr =  p_strdat+0(4).
  /agri/s_fmachdr-zzturma = p_turma.

  IF ls_fmachdr NE /agri/s_fmachdr.

    IF gs_variables-document_mode = c_mode_create.
      /agri/s_fmachdr-status = c_process_status-ctd.
      IF gs_acdoc_infocus-x-achdr-actyp NE p_actyp.
        PERFORM header_defaults_fill_1.
        IF gs_tfmactyp-acapp EQ c_accom_appli-prnum.
          CLEAR: /agri/s_fmachdr-accom.
        ENDIF.
      ENDIF.
    ENDIF.

    MOVE-CORRESPONDING /agri/s_fmachdr TO gs_acdoc_infocus-x-achdr.
    IF gs_acdoc_infocus-x-achdr-updkz NE c_updkz_new.
      gs_acdoc_infocus-x-achdr-updkz = c_updkz_update.
    ENDIF.

    gs_variables-data_changed = c_true.
    gs_acdoc_infocus-accom = gs_acdoc_infocus-x-achdr-accom.
  ENDIF.
ENDFORM.                    " HEADER_DATA_UPDATE
*&---------------------------------------------------------------------*
*&      Form  header_data_update
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM header_data_update .

  DATA: ls_fmachdr     TYPE /agri/fmachdr,
        lv_activity(2) TYPE c,
        lv_subrc       TYPE sy-subrc..

  CHECK gs_variables-document_mode NE c_mode_display
    AND gs_variables-document_mode NE space.

  IF gs_variables-document_mode EQ c_mode_create.
    MOVE c_authorization_activity-create TO lv_activity.
  ELSEIF gs_variables-document_mode EQ c_mode_change.
    MOVE c_authorization_activity-change TO lv_activity.
  ENDIF.

*--Call Authority Check
  PERFORM authority_check USING gs_acdoc_infocus-x-achdr
                                lv_activity
                                c_true
                       CHANGING lv_subrc.
  IF lv_subrc NE 0.

    IF lv_activity EQ c_authorization_activity-change.
      gs_variables-document_mode = c_mode_display.
      EXIT.
    ELSE.

      CLEAR: gs_variables-document_mode,
             gs_acdoc_infocus, /agri/s_fmachdr.
      gs_variables-errors = c_true.
      EXIT.
    ENDIF.
  ENDIF.

  MOVE-CORRESPONDING gs_acdoc_infocus-x-achdr TO ls_fmachdr.

  IF ls_fmachdr NE /agri/s_fmachdr.

    IF gs_variables-document_mode = c_mode_create.
      /agri/s_fmachdr-status = c_process_status-ctd.
      IF gs_acdoc_infocus-x-achdr-actyp NE /agri/s_fmachdr-actyp.
        CLEAR: /agri/s_fmachdr-accom.
        PERFORM header_defaults_fill.
      ENDIF.
    ENDIF.

    MOVE-CORRESPONDING /agri/s_fmachdr TO gs_acdoc_infocus-x-achdr.
    IF gs_acdoc_infocus-x-achdr-updkz NE c_updkz_new.
      gs_acdoc_infocus-x-achdr-updkz = c_updkz_update.
    ENDIF.

    gs_variables-data_changed = c_true.
    gs_acdoc_infocus-accom = gs_acdoc_infocus-x-achdr-accom.

  ENDIF.
ENDFORM.                    " HEADER_DATA_UPDATE
*&---------------------------------------------------------------------*
*&      Form  HEADER_DEFAULTS_FILL
*&---------------------------------------------------------------------*
FORM header_defaults_fill.

  CHECK /agri/s_fmachdr-actyp IS NOT INITIAL.

  SELECT SINGLE * FROM /agri/tfmactyp INTO gs_tfmactyp
          WHERE actyp = /agri/s_fmachdr-actyp.
  IF sy-subrc NE 0.
***Extended Additional Syntax Check ATC  1709 PQ
    MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error NUMBER '003'
                            WITH /agri/s_fmachdr-actyp INTO sy-msgli. "#EC MG_MISSING
*****
    message_simple space.
    gs_variables-errors = c_true.
    EXIT.
  ENDIF.
  PERFORM badi_header_defaults_fill.
ENDFORM.                    " HEADER_DEFAULTS_FILL
*&---------------------------------------------------------------------*
*&      Form  HEADER_DEFAULTS_FILL
*&---------------------------------------------------------------------*
FORM header_defaults_fill_1.

  CHECK p_actyp IS NOT INITIAL.

  SELECT SINGLE * FROM /agri/tfmactyp INTO gs_tfmactyp
          WHERE actyp = p_actyp.
  IF sy-subrc NE 0.
***Extended Additional Syntax Check ATC  1709 PQ
    MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error NUMBER '003'
                            WITH p_actyp INTO sy-msgli. "#EC MG_MISSING
****
    message_simple space.
    gs_variables-errors = c_true.
    EXIT.
  ENDIF.
  PERFORM badi_header_defaults_fill.
ENDFORM.                    " HEADER_DEFAULTS_FILL
*&---------------------------------------------------------------------*
*&      Form  HEADER_DATA_CHECK
*&---------------------------------------------------------------------*
FORM header_data_check .

  DATA: lv_subrc TYPE sy-subrc,
        lv_accom TYPE /agri/fmaccom,
        lr_aufnr LIKE gr_aufnr.

  IF /agri/s_fmachdr-actyp NE gs_tfmactyp-actyp.
    CLEAR gs_tfmactyp.
  ENDIF.

  IF gs_tfmactyp-acapp NE c_accom_appli-aufnr.
    CLEAR aufk-aufnr.
  ENDIF.

  IF /agri/s_fmachdr-actyp IS INITIAL.
    SET CURSOR FIELD '/AGRI/S_FMACHDR-ACTYP'.
    MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                       NUMBER '002' INTO sy-msgli.
    gs_variables-errors = c_true.
    message_simple space.
  ENDIF.

  IF /agri/s_fmachdr-accom IS INITIAL
    AND gs_tfmactyp-numki IS INITIAL
    AND ok_code EQ c_fcode-continue.
    SET CURSOR FIELD '/AGRI/S_FMACHDR-ACCOM'.
    MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                       NUMBER '019' INTO sy-msgli.
    gs_variables-errors = c_true.
    message_simple space.
  ENDIF.
  IF /agri/s_fmachdr-actyp IS NOT INITIAL
   AND gs_tfmactyp-numki IS INITIAL
   AND /agri/s_fmachdr-accom IS NOT INITIAL.
    SELECT SINGLE accom
             INTO lv_accom
             FROM /agri/fmachdr
            WHERE accom = /agri/s_fmachdr-accom.
    IF sy-subrc = 0.
      SET CURSOR FIELD '/AGRI/S_FMACHDR-ACCOM'.
      MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                              NUMBER '004'
                              WITH lv_accom
                              INTO sy-msgli.
      gs_variables-errors = c_true.
      message_simple space.
    ENDIF.
  ENDIF.
  IF /agri/s_fmachdr-bukrs IS INITIAL
    AND ok_code EQ c_fcode-continue.
    SET CURSOR FIELD '/AGRI/S_FMACHDR-BUKRS'.
    MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                                NUMBER '040'
                                INTO   sy-msgli.
    gs_variables-errors = c_true.
    message_simple space.
  ENDIF.
  IF /agri/s_fmachdr-werks IS INITIAL
    AND ok_code EQ c_fcode-continue.
    SET CURSOR FIELD '/AGRI/S_FMACHDR-WERKS'.
    MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                                NUMBER '041'
                                INTO   sy-msgli.
    gs_variables-errors = c_true.
    message_simple space.
  ENDIF.
  IF /agri/s_fmachdr-strtdat IS INITIAL
  AND ok_code EQ c_fcode-continue.
    SET CURSOR FIELD '/AGRI/S_FMACHDR-STRTDAT'.
    MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                                NUMBER '038'
                                INTO   sy-msgli.
    gs_variables-errors = c_true.
    message_simple space.
  ELSEIF /agri/s_fmachdr-findat IS INITIAL
    AND ok_code EQ c_fcode-continue.
    SET CURSOR FIELD '/AGRI/S_FMACHDR-FINDAT'.
    MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                                NUMBER '039'
                                INTO   sy-msgli.
    gs_variables-errors = c_true.
    message_simple space.
  ELSEIF /agri/s_fmachdr-strtdat IS NOT INITIAL
    AND /agri/s_fmachdr-findat IS NOT INITIAL.
    IF /agri/s_fmachdr-findat LT /agri/s_fmachdr-strtdat.
      SET CURSOR FIELD '/AGRI/S_FMACHDR-FINDAT'.
      MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                                  NUMBER '018'
                                  WITH /agri/s_fmachdr-findat
                                       /agri/s_fmachdr-strtdat
                                  INTO   sy-msgli.
      gs_variables-errors = c_true.
      message_simple space.
    ENDIF.
  ENDIF.
  IF gs_tfmactyp-acapp EQ c_accom_appli-aufnr.
    IF aufk-aufnr IS INITIAL
      AND ok_code EQ c_fcode-continue.
      SET CURSOR FIELD 'AUFK-AUFNR'.
      MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                                NUMBER '037'
                                INTO   sy-msgli.
      gs_variables-errors = c_true.
      message_simple space.
    ELSEIF aufk-aufnr IS NOT INITIAL
      AND ok_code EQ c_fcode-continue.
      lr_aufnr-option = c_operator_word-equalto.
      lr_aufnr-sign   = c_sign-include.
      lr_aufnr-low    = aufk-aufnr.
      APPEND lr_aufnr TO gr_aufnr.
      PERFORM aufnr_data_check.
      CLEAR lr_aufnr.
    ENDIF.
  ENDIF.
  IF gs_tfmactyp-acapp EQ c_accom_appli-wonum
      AND ok_code EQ c_fcode-continue.
    IF /agri/s_fmachdr-wonum IS INITIAL.
      SET CURSOR FIELD '/AGRI/S_FMACHDR-WONUM'.
      MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                                  NUMBER '042'
                                  INTO   sy-msgli.
      gs_variables-errors = c_true.
      message_simple space.
    ENDIF.
  ENDIF.
  CLEAR gs_variables-errors.
  IF gs_variables-errors NE c_true.
    PERFORM document_infocus_lock USING /agri/s_fmachdr-accom
                               CHANGING lv_subrc.
    IF lv_subrc IS NOT INITIAL.
      gs_variables-overview_mode = gs_variables-document_mode
                                 = c_mode_display.
    ENDIF.
  ENDIF.

ENDFORM.                    " HEADER_DATA_CHECK
*&---------------------------------------------------------------------*
*&      Form  header_data_check_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM header_data_check_1.

  DATA: lv_subrc TYPE sy-subrc,
        lv_accom TYPE /agri/fmaccom,
        lr_aufnr LIKE gr_aufnr,
        lv_bukrs TYPE bukrs,
        lv_werks TYPE /agri/fm_werks.

  IF p_actyp NE gs_tfmactyp-actyp.
    CLEAR gs_tfmactyp.
  ENDIF.

  IF gs_tfmactyp-acapp NE c_accom_appli-aufnr.
    CLEAR aufk-aufnr.
  ENDIF.

  IF p_actyp IS INITIAL.
    SET CURSOR FIELD 'P_ACTYP'.
    MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                       NUMBER '002' INTO sy-msgli.
    gs_variables-errors = c_true.
    message_simple space.
  ENDIF.

  IF p_accom IS INITIAL
    AND gs_tfmactyp-numki IS INITIAL
    AND ( ok_code EQ c_fcode-crea OR ok_code EQ c_fcode-none ).
    SET CURSOR FIELD 'P_ACCOM'.
    MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                       NUMBER '019' INTO sy-msgli.
    gs_variables-errors = c_true.
    message_simple space.
  ENDIF.
  IF p_actyp IS NOT INITIAL
   AND gs_tfmactyp-numki IS INITIAL
   AND p_accom IS NOT INITIAL.
    SELECT SINGLE accom
             INTO lv_accom
             FROM /agri/fmachdr
            WHERE accom = p_accom.
    IF sy-subrc = 0.
      SET CURSOR FIELD '/AGRI/S_FMACHDR-ACCOM'.
      MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                              NUMBER '004'
                              WITH lv_accom
                              INTO sy-msgli.
      gs_variables-errors = c_true.
      message_simple space.
    ENDIF.
  ENDIF.
  IF p_bukrs IS INITIAL
    AND ( ok_code EQ c_fcode-crea OR ok_code EQ c_fcode-none ).
    SET CURSOR FIELD 'P_BUKRS'.
    MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                                NUMBER '040'
                                INTO   sy-msgli.
    gs_variables-errors = c_true.
    message_simple space.
  ELSE.
    IF ok_code EQ c_fcode-crea.
      SELECT SINGLE bukrs
      INTO lv_bukrs
      FROM t001
      WHERE bukrs = p_bukrs.
      IF sy-subrc NE 0.
        SET CURSOR FIELD 'P_BUKRS'.
        MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                                    NUMBER '060'
                                    WITH p_bukrs
                                    INTO   sy-msgli.
        gs_variables-errors = c_true.
        message_simple space.
      ENDIF.
    ENDIF.
  ENDIF.
  IF p_mat IS INITIAL
     AND  ok_code EQ c_fcode-crea.
    SET CURSOR FIELD 'P_MAT'.
    MESSAGE ID 'ZABS_MSGCLS' TYPE c_msg_type-error
                                NUMBER '051'
                                INTO   sy-msgli.

    gs_variables-errors = c_true.
    message_simple space.
  ENDIF.
  IF p_werks IS INITIAL
    AND ( ok_code EQ c_fcode-crea OR ok_code EQ c_fcode-none ).
    SET CURSOR FIELD 'P_WERKS'.
    MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                                NUMBER '041'
                                INTO   sy-msgli.
    gs_variables-errors = c_true.
    message_simple space.
  ELSE.
    IF ok_code EQ c_fcode-crea.
      SELECT SINGLE werks
        INTO lv_werks
        FROM t001w
        WHERE werks = p_werks.
      IF sy-subrc NE 0.
        SET CURSOR FIELD 'P_WERKS'.
        MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                                    NUMBER '061'
                                    WITH p_werks
                                    INTO   sy-msgli.
        gs_variables-errors = c_true.
        message_simple space.
      ENDIF.
    ENDIF.
  ENDIF.
  IF p_strdat IS INITIAL
  AND ( ok_code EQ c_fcode-crea OR ok_code EQ c_fcode-none ).
    SET CURSOR FIELD 'P_STRDAT'.
    MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                                NUMBER '038'
                                INTO   sy-msgli.
    gs_variables-errors = c_true.
    message_simple space.
  ELSEIF p_findat IS INITIAL
    AND ( ok_code EQ c_fcode-crea OR ok_code EQ c_fcode-none ).
    SET CURSOR FIELD 'P_FINDAT'.
    MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                                NUMBER '039'
                                INTO   sy-msgli.
    gs_variables-errors = c_true.
    message_simple space.
  ELSEIF p_strdat IS NOT INITIAL
    AND p_findat IS NOT INITIAL.
    IF p_findat < p_strdat.
      SET CURSOR FIELD 'P_FINDAT'.
      MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                                  NUMBER '018'
                                  WITH p_findat
                                       p_strdat
                                  INTO   sy-msgli.
      gs_variables-errors = c_true.
      message_simple space.
    ENDIF.
  ENDIF.
  IF gs_tfmactyp-acapp EQ c_accom_appli-aufnr.
***    IF so_aufnr[] IS INITIAL
***      AND ( ok_code EQ c_fcode-crea or ok_code EQ c_fcode-none ).
***      SET CURSOR FIELD 'SO_AUFNR'.
***      MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
***                                NUMBER '037'
***                                INTO   sy-msgli.
***      gs_variables-errors = c_true.
***      message_simple space.
***    ELSE

    IF so_aufnr[] IS INITIAL AND ok_code EQ c_fcode-crea.
*      APPEND LINES OF so_aufnr[] to gr_aufnr[].
      PERFORM aufnr_data_check.
      CLEAR lr_aufnr.
    ENDIF.
  ENDIF.
  IF gs_tfmactyp-acapp EQ c_accom_appli-wonum
      AND ( ok_code EQ c_fcode-crea OR ok_code EQ c_fcode-none ).
    IF p_wonum IS INITIAL.
      SET CURSOR FIELD 'P_WONUM'.
      MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                                  NUMBER '042'
                                  INTO   sy-msgli.
      gs_variables-errors = c_true.
      message_simple space.
    ENDIF.
  ENDIF.

*-- Validate Turma
  IF p_turma IS NOT INITIAL.
    SELECT SINGLE turma_id
      FROM zfmfpgrouphdr
      INTO @DATA(lv_turma)
     WHERE turma_id EQ @p_turma.
    IF sy-subrc NE 0.
      SET CURSOR FIELD 'P_TURMA'.
      MESSAGE ID 'ZABS_MSGCLS' TYPE c_msg_type-error
                                  NUMBER '084'
                                  INTO   sy-msgli.

      gs_variables-errors = c_true.
      message_simple space.
    ENDIF.
  ENDIF.

  CLEAR gs_variables-errors.
  IF gs_variables-errors NE c_true.
    PERFORM document_infocus_lock_ac USING p_accom
                               CHANGING lv_subrc.
*    IF lv_subrc IS NOT INITIAL.
*      gs_variables-overview_mode = gs_variables-document_mode
*                                 = c_mode_display.
*    ENDIF.
  ENDIF.

ENDFORM.                    " HEADER_DATA_CHECK
