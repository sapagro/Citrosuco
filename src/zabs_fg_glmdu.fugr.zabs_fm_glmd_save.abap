FUNCTION zabs_fm_glmd_save.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_SET_UPDATE_TASK) TYPE  RV56A-SELKZ DEFAULT 'X'
*"     REFERENCE(I_COMMIT_WORK) TYPE  RV56A-SELKZ DEFAULT 'X'
*"     REFERENCE(IREF_TEXT) TYPE REF TO  /AGRI/CL_GTEXT_PROCESS
*"       OPTIONAL
*"     REFERENCE(IT_MDDOC_REF) TYPE  /AGRI/T_GLMD_DOC OPTIONAL
*"     REFERENCE(I_MDOCM) TYPE  /AGRI/GLMDOCM OPTIONAL
*"  CHANGING
*"     REFERENCE(CT_MDDOC) TYPE  /AGRI/T_GLMD_DOC
*"     REFERENCE(CT_MESSAGES) TYPE  /AGRI/T_GPROLOG OPTIONAL
*"  EXCEPTIONS
*"      NO_CHANGE
*"----------------------------------------------------------------------

  DATA: BEGIN OF ls_atinn,
          data_encerra_in    TYPE atinn,
          florada_in         TYPE atinn,
          prod_real_in       TYPE atinn,
          safra_in           TYPE atinn,
          estimativa_in      TYPE atinn,
          motivo_encerra_in  TYPE atinn,
          data_encerra_out   TYPE atnam VALUE 'CIT-DATA-ENCERRA',
          florada_out        TYPE atnam VALUE 'CIT-FLORADA',
          prod_real_out      TYPE atnam VALUE 'CIT-PROD-REAL',
          safra_out          TYPE atnam VALUE 'CIT-SAFRA',
          estimativa_out     TYPE atnam VALUE 'CIT-ESTIMATIVA',
          motivo_encerra_out TYPE atnam VALUE 'CIT-MOT-EST',
        END OF ls_atinn.

  DATA: lt_mdatv         TYPE /agri/t_glmdatv,
        lt_messages      TYPE /agri/t_gprolog,
        lrt_atinn        TYPE RANGE OF atinn,
        ls_mddoc_infocus TYPE /agri/s_glmd_doc,
        lv_i             TYPE i,
        lv_estimativa    TYPE char20,
        lv_menge         TYPE menge_d,
        lv_menge_char    TYPE char16,
        lv_sum_menge     TYPE i.

  FIELD-SYMBOLS: <ls_mddoc_infocus> TYPE /agri/s_glmd_doc.

  CONSTANTS: BEGIN OF c_measurement_level,
               terrain      TYPE /agri/glaslvl VALUE 'T',
               crop_seasons TYPE /agri/glaslvl VALUE 'A',
               harvest      TYPE /agri/glaslvl VALUE 'H',
               irrigation   TYPE /agri/glaslvl VALUE 'I',
             END OF c_measurement_level.

****Ownership
  CONSTANTS : BEGIN OF c_ownership,
                own         TYPE /agri/glownshp VALUE 'OW',
                third_party TYPE /agri/glownshp VALUE 'TP',
              END OF c_ownership.

****Document Modes
  CONSTANTS: c_mode_create(1)  TYPE c VALUE 'H',
             c_mode_change(1)  TYPE c VALUE 'V',
             c_mode_display(1) TYPE c VALUE 'A',
             c_mode_copy(1)    TYPE c VALUE 'C',
             c_mode_delete(1)  TYPE c VALUE 'D'.

  ASSIGN ('(/AGRI/SAPLGLMDM)GS_VARIABLES-DOCUMENT_MODE') TO FIELD-SYMBOL(<lv_mode>).
  ASSIGN ('(/AGRI/SAPLGLMDM)GS_MDDOC_INFOCUS') TO <ls_mddoc_infocus>.

  IF <lv_mode> IS ASSIGNED
  AND <lv_mode> EQ c_mode_create
  AND <ls_mddoc_infocus> IS ASSIGNED
  AND <ls_mddoc_infocus>-x-mdhdr-aslvl EQ c_measurement_level-crop_seasons
  AND <ls_mddoc_infocus>-x-mdhdr-mdtyp EQ 'ZPTA'
  AND <ls_mddoc_infocus>-x-mdhdr-mpgrp EQ 'CIT-ESTIMATIVA'.
    READ TABLE ct_mddoc ASSIGNING FIELD-SYMBOL(<ls_mddoc>) INDEX 1.
    IF sy-subrc EQ 0.
      DO 6 TIMES.
        DATA(lv_index) = sy-index.
        INSERT INITIAL LINE INTO TABLE lrt_atinn
          ASSIGNING FIELD-SYMBOL(<lrs_atinn>).
        IF sy-subrc EQ 0.
          <lrs_atinn> = 'IEQ'.
          CASE lv_index.
            WHEN 1.
              CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
                EXPORTING
                  input  = ls_atinn-data_encerra_out
                IMPORTING
                  output = ls_atinn-data_encerra_in.
              <lrs_atinn>-low = ls_atinn-data_encerra_in.
            WHEN 2.
              CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
                EXPORTING
                  input  = ls_atinn-florada_out
                IMPORTING
                  output = ls_atinn-florada_in.
              <lrs_atinn>-low = ls_atinn-florada_in.
            WHEN 3.
              CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
                EXPORTING
                  input  = ls_atinn-prod_real_out
                IMPORTING
                  output = ls_atinn-prod_real_in.
              <lrs_atinn>-low = ls_atinn-prod_real_in.
            WHEN 4.
              CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
                EXPORTING
                  input  = ls_atinn-safra_out
                IMPORTING
                  output = ls_atinn-safra_in.
              <lrs_atinn>-low = ls_atinn-safra_in.
            WHEN 5.
              CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
                EXPORTING
                  input  = ls_atinn-estimativa_out
                IMPORTING
                  output = ls_atinn-estimativa_in.
              <lrs_atinn>-low = ls_atinn-estimativa_in.
            WHEN 6.
              CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
                EXPORTING
                  input  = ls_atinn-motivo_encerra_out
                IMPORTING
                  output = ls_atinn-motivo_encerra_in.
              <lrs_atinn>-low = ls_atinn-motivo_encerra_in.
          ENDCASE.
        ENDIF.
      ENDDO.

      SELECT atinn, adzhl, atnam,
             atfor, anzst, anzdz
        FROM cabn
        INTO TABLE @DATA(lt_cabn)
       WHERE atinn IN @lrt_atinn[].

      lt_mdatv[] = <ls_mddoc>-x-mdatv[].
      DELETE <ls_mddoc>-x-mdatv WHERE atinn NE ls_atinn-safra_in
                                  AND atinn NE ls_atinn-florada_in
                                  AND atinn NE ls_atinn-prod_real_in.

      READ TABLE <ls_mddoc>-x-mdatv ASSIGNING FIELD-SYMBOL(<ls_mdatv>)
        WITH KEY atinn = ls_atinn-prod_real_in.
      IF sy-subrc EQ 0.
        <ls_mdatv>-atinn = ls_atinn-estimativa_in.
      ENDIF.

*--BOC-T_T.KONNO-07.15.21
      IF i_mdocm IS NOT INITIAL.
        SELECT SINGLE h~mdocm, h~mdtyp, h~aslvl, h~tplnr_fl,
                      h~contr, h~cmnum, h~equnr, h~mpgrp,
                      h~mdate, h~mtime, v~atzhl, v~atwrt, v~atflv,
                      c~atinn, q~value AS atinn_out, c~atnam
          INTO @DATA(ls_glmdatv)
          FROM /agri/glmdhdr AS h
          INNER JOIN /agri/glmdatv AS v
          ON v~mdocm EQ h~mdocm
          INNER JOIN cabn AS c
          ON c~atinn EQ v~atinn
          LEFT OUTER JOIN /agri/fm_qmsmpl AS q
          ON q~datum EQ c~adatu "creates field atinn_out char40
         WHERE h~mdocm EQ @i_mdocm
           AND v~atinn EQ @ls_atinn-prod_real_in.

        IF sy-subrc EQ 0.
          lv_sum_menge = ls_glmdatv-atflv.
        ELSE.
          IMPORT lv_menge_char FROM MEMORY ID 'ZVTX_PRODU_REAL'.
          FREE MEMORY ID 'ZVTX_PRODU_REAL'.
          lv_sum_menge = lv_menge_char.
        ENDIF.
      ELSE.
*--EOC-T_T.KONNO-07.15.21
        READ TABLE it_mddoc_ref INTO DATA(ls_mddoc_ref) INDEX 1.
        IF sy-subrc EQ 0.
          READ TABLE ls_mddoc_ref-x-mdatv INTO DATA(ls_mdatv)
            WITH KEY atinn = ls_atinn-prod_real_in.
          IF sy-subrc EQ 0.
            IF ls_mdatv-atflv IS NOT INITIAL.
              CALL FUNCTION 'CEVA_CONVERT_FLOAT_TO_CHAR'
                EXPORTING
                  float_imp  = ls_mdatv-atflv
                  format_imp = lv_i
                IMPORTING
                  char_exp   = lv_estimativa.
              MOVE lv_estimativa TO lv_menge.
              lv_sum_menge = lv_sum_menge + lv_menge.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      READ TABLE <ls_mddoc>-x-mdatv INTO DATA(ls_mdatv_ref)
        WITH KEY atinn = ls_atinn-safra_in.
      IF sy-subrc EQ 0.
        READ TABLE <ls_mddoc>-x-mdatv ASSIGNING FIELD-SYMBOL(<ls_mdatv_new>)
          WITH KEY atinn = ls_atinn-estimativa_in.
        IF sy-subrc NE 0.
          INSERT INITIAL LINE INTO TABLE <ls_mddoc>-x-mdatv
            ASSIGNING <ls_mdatv_new>.
        ENDIF.

        IF <ls_mdatv_new> IS ASSIGNED.
          <ls_mdatv_new> = ls_mdatv_ref.
          <ls_mdatv_new>-atinn = ls_atinn-estimativa_in.
          <ls_mdatv_new>-atwrt = lv_sum_menge.
          CONDENSE <ls_mdatv_new>-atwrt.
        ENDIF.

        UNASSIGN <ls_mdatv_new>.
        READ TABLE <ls_mddoc>-x-mdatv INTO <ls_mdatv_new>
          WITH KEY atinn = ls_atinn-motivo_encerra_in.
        IF sy-subrc NE 0.
          INSERT INITIAL LINE INTO TABLE <ls_mddoc>-x-mdatv
            ASSIGNING <ls_mdatv_new>.
        ENDIF.

        IF <ls_mdatv_new> IS ASSIGNED.
          <ls_mdatv_new> = ls_mdatv_ref.
          <ls_mdatv_new>-atinn = ls_atinn-motivo_encerra_in.
          <ls_mdatv_new>-atwrt = '5'.
        ENDIF.
      ENDIF.

      CALL FUNCTION '/AGRI/GLMD_SAVE'
        EXPORTING
          i_commit_work = 'X'
          iref_text     = iref_text
        CHANGING
          ct_mddoc      = ct_mddoc
          ct_messages   = lt_messages
        EXCEPTIONS
          no_change     = 1
          OTHERS        = 2.

      ct_messages[] = lt_messages[].
    ENDIF.
  ENDIF.

ENDFUNCTION.
