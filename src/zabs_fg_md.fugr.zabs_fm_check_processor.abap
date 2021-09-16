FUNCTION zabs_fm_check_processor.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IS_STSFL) TYPE  /AGRI/GSTSFL
*"     REFERENCE(IS_FLSTP) TYPE  /AGRI/GSFFLSTP
*"     REFERENCE(IS_SOUTC) TYPE  /AGRI/GSFSOC
*"     REFERENCE(IT_STEP_OUTCOMES) TYPE  /AGRI/T_GSTATAS
*"  CHANGING
*"     REFERENCE(CT_MESSAGES) TYPE  /AGRI/T_GPROLOG
*"  EXCEPTIONS
*"      OUTCOME_NOT_SET
*"      NOT_A_PROCESSOR
*"----------------------------------------------------------------------
* validates if the "user" processor is as a processor
* and compare with sy-uname (user processing approval)
* check processor in container list table 'RL'
* SOUTC: When blank, no Outcome has been set.
*"----------------------------------------------------------------------

  DATA: lt_step_outcomes TYPE /agri/t_gstatas,
        ls_step_outcomes TYPE /agri/s_gstatas,
        lt_gacprs        TYPE /agri/t_gacprs,
        ls_gacprs        TYPE /agri/s_gacprs,
        ls_gstataa       TYPE /agri/gstataa,
        ls_messages      TYPE /agri/s_gprolog,
        lt_tgsfrcp       TYPE /agri/t_tgsfrcp,
        ls_tgsfrcp       TYPE /agri/s_tgsfrcp,
        ls_tgrcprs       TYPE /agri/tgrcprs,
        lv_rclst         TYPE /agri/grclst,
        lv_found         TYPE char1.

  CHECK is_soutc IS NOT INITIAL.

  lt_step_outcomes = it_step_outcomes.

*--Active step reading
  READ TABLE lt_step_outcomes
        INTO ls_step_outcomes
        WITH KEY stpst = 'A'.                  "Step Ativo

*--Activity Selection
  SELECT SINGLE *
           FROM /agri/gstataa
           INTO ls_gstataa
          WHERE objnr  = ls_step_outcomes-objnr
            AND fcontr = ls_step_outcomes-fcontr
            AND scontr = ls_step_outcomes-scontr.

  IF sy-subrc NE 0.
*--More than one status may be
* being released without saving the document.
*    ls_messages-msgid = '/AGRI/GACM'.
*    ls_messages-msgno = '097'.
*    ls_messages-msgty = 'E'.
*    ls_messages-msgv1 = space.
*    ls_messages-msgv2 = space.
*    ls_messages-msgv3 = space.
*    ls_messages-msgv4 = space.
*    APPEND ls_messages TO ct_messages.

    MESSAGE i097(/agri/gacm) DISPLAY LIKE 'E'.
    IF is_soutc NE space.
      RAISE outcome_not_set.
    ENDIF.
  ENDIF.

*--Activity Processor Selection
  SELECT *
    FROM /agri/gacprs
    INTO TABLE lt_gacprs
   WHERE acnum = ls_gstataa-acnum.

  READ TABLE lt_gacprs
        INTO ls_gacprs
        WITH KEY procr = sy-uname.
  IF sy-subrc NE 0.

*--Check processors in processor list
    LOOP AT lt_gacprs INTO ls_gacprs WHERE prtyp = 'RL'.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ls_gacprs-procr
        IMPORTING
          output = lv_rclst.

      SELECT SINGLE *
        FROM /agri/tgrcprs
        INTO ls_tgrcprs
       WHERE rclst = lv_rclst
         AND procr = sy-uname.
      IF sy-subrc IS INITIAL.
        lv_found = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lv_found IS INITIAL.
      ls_messages-msgid = '/AGRI/GACM'.
      ls_messages-msgno = '044'.
      ls_messages-msgty = 'E'.
      ls_messages-msgv1 = 'U/sr.'.
      ls_messages-msgv2 = sy-uname.
      ls_messages-msgv3 = space.
      ls_messages-msgv4 = space.
      APPEND ls_messages TO ct_messages.
      RAISE not_a_processor.
*      MESSAGE i044(/agri/gacm) WITH 'U/sr.' sy-uname DISPLAY LIKE 'E'.
*      RAISE not_a_processor.
    ENDIF.
  ENDIF.

ENDFUNCTION.
