FUNCTION zabs_fm_cit_estimativa.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  CHANGING
*"     REFERENCE(CS_MDHDR) TYPE  /AGRI/S_GLMDHDR
*"     REFERENCE(CT_MDITM) TYPE  /AGRI/T_GLMDITM
*"     REFERENCE(CT_MDATV) TYPE  /AGRI/T_GLMDATV
*"----------------------------------------------------------------------
  DATA: lwa_attributes TYPE zabs_str_attr_forml,
        lt_attributes  TYPE zabs_tty_attr_forml,
        lv_subrc       TYPE sysubrc,
        lv_message     TYPE char80,
        lv_value       TYPE atflv.

  SELECT SINGLE clint
    FROM klah
    INTO @DATA(lv_clint)
    WHERE class = @cs_mdhdr-mpgrp.

  IF sy-subrc = 0.
    SELECT *
      FROM zabst_ksml
      INTO TABLE @DATA(lt_ksml)
     WHERE clint = @lv_clint.
    IF sy-subrc = 0.
      SELECT atinn, adzhl, atnam,
             atfor, anzst, anzdz
        FROM cabn
        INTO TABLE @DATA(lt_cabn)
        FOR ALL ENTRIES IN @lt_ksml
       WHERE atnam = @lt_ksml-atnam.
    ENDIF.
  ENDIF.

  LOOP AT ct_mdatv INTO DATA(lwa_mdatv).
    READ TABLE lt_cabn INTO DATA(lwa_cabn)
      WITH KEY atinn = lwa_mdatv-atinn.
    IF sy-subrc EQ 0.
      READ TABLE lt_ksml INTO DATA(lwa_ksml)
        WITH KEY atnam = lwa_cabn-atnam.
      IF sy-subrc = 0.
        DATA(lv_data) = lwa_ksml-ident(1).
        lwa_attributes-atflv = lwa_mdatv-atflv.
        lwa_attributes-atnam = lwa_ksml-atnam.
        lwa_attributes-ident = lwa_ksml-ident.
        APPEND lwa_attributes TO lt_attributes.
      ENDIF.
    ENDIF.
    CLEAR: lwa_attributes, lwa_ksml, lwa_cabn, lwa_mdatv.
  ENDLOOP.

  IF lt_attributes[] IS INITIAL.
    RETURN.
  ENDIF.

  LOOP AT lt_ksml INTO lwa_ksml WHERE formula IS NOT INITIAL.
    CLEAR: lwa_cabn.
    READ TABLE lt_cabn INTO lwa_cabn
      WITH KEY atnam = lwa_ksml-atnam.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.
    READ TABLE ct_mdatv ASSIGNING FIELD-SYMBOL(<fs_mdatv>) INDEX 1.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.
    lwa_mdatv = <fs_mdatv>.
    lwa_mdatv-atinn = lwa_cabn-atinn.

    CALL FUNCTION 'ZABS_FM_EVAL_FORMULA'
      EXPORTING
        iv_formula    = lwa_ksml-formula
        it_attributes = lt_attributes
      IMPORTING
        ev_subrc      = lv_subrc
        ev_message    = lv_message
        ev_value      = lv_value.
    IF lv_subrc IS INITIAL.
      lwa_mdatv-atflv = lv_value.
      APPEND lwa_mdatv TO ct_mdatv.
    ENDIF.
  ENDLOOP.

ENDFUNCTION.
