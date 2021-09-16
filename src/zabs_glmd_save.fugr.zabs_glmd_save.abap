FUNCTION zabs_glmd_save.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IV_DOCUMENT_TYPE) TYPE  /AGRI/S_GLMDHDR-MDTYP
*"     VALUE(IV_TERRAIN) TYPE  /AGRI/S_GLMDHDR-TPLNR_FL
*"     VALUE(IV_MEASUREMENT_GROUP) TYPE  /AGRI/S_GLMDHDR-MPGRP
*"     VALUE(IT_ATTRIBUTES) TYPE  ZABS_TTY_ATTRIBUTES
*"  EXPORTING
*"     VALUE(ET_MESSAGES) TYPE  ZABS_TTY_MESSAGES
*"----------------------------------------------------------------------
  DATA: lt_messages  TYPE /agri/t_gprolog,
        ls_message   LIKE LINE OF lt_messages,
        ls_vtx_msg   LIKE LINE OF et_messages,
        lt_mddoc     TYPE /agri/t_glmd_doc,
        ls_attr_val  TYPE auspdata,
        ls_mdatv_tmp TYPE /agri/s_glmdatv,
        lv_subrc     TYPE sysubrc,
        lv_terrain   TYPE /agri/s_glmdhdr-tplnr_fl,
        lv_data_ref  TYPE sydatum,
        lv_valid.

****Global Infocus Document Structure
  FIELD-SYMBOLS: <lt_attr_values>   TYPE /agri/t_glmdatv_fcat,
                 <ls_glmdhdr>       TYPE /agri/s_glmdhdr,
                 <ls_mddoc_infocus> TYPE /agri/s_glmd_doc.

  CONSTANTS: BEGIN OF c_agtyp,
               terrain      TYPE /agri/gagtyp VALUE '003',
               measurements TYPE /agri/gagtyp VALUE 'X90',
             END OF c_agtyp.

****Document Modes
  CONSTANTS: c_mode_create(1)  TYPE c VALUE 'H',
             c_mode_change(1)  TYPE c VALUE 'V',
             c_mode_display(1) TYPE c VALUE 'A',
             c_mode_copy(1)    TYPE c VALUE 'C',
             c_mode_delete(1)  TYPE c VALUE 'D'.

****Message Types
  CONSTANTS : BEGIN OF c_msg_type,
                info    LIKE sy-msgty VALUE 'I',
                warning LIKE sy-msgty VALUE 'W',
                error   LIKE sy-msgty VALUE 'E',
                abend   LIKE sy-msgty VALUE 'A',
                success LIKE sy-msgty VALUE 'S',
                x       LIKE sy-msgty VALUE 'X',
              END   OF c_msg_type.

  CONSTANTS: BEGIN OF c_crop_season_status,
               active   TYPE /agri/glastat VALUE 'A',
               inactive TYPE /agri/glastat VALUE 'I',
               closed   TYPE /agri/glastat VALUE 'C',
             END OF c_crop_season_status.

  CONSTANTS: BEGIN OF c_tipo_safra,
               contabil TYPE zfmtpsafra VALUE 'C',
               tecnica  TYPE zfmtpsafra VALUE 'T',
             END OF c_tipo_safra.

  CONSTANTS: BEGIN OF c_caracteristica,
               safra TYPE atnam VALUE 'CIT-SAFRA',
               data  TYPE atnam VALUE 'CIT-DATA-APLIC',
             END OF c_caracteristica.

****Update Indicators
  CONSTANTS: c_updkz_new(1)     TYPE c VALUE 'I',
             c_updkz_update(1)  TYPE c VALUE 'U',
             c_updkz_delete(1)  TYPE c VALUE 'D',
             c_updkz_old(1)     TYPE c VALUE ' ',
****Rel 60E SP6
             c_updkz_newrow     TYPE c VALUE 'N',
****
             c_updkz_propose(1) TYPE c VALUE 'P'. "ESP5 11129 - Generic Fiori app changes,

  DEFINE d_add_message.
    ls_vtx_msg-msgno = sy-msgno.
    ls_vtx_msg-msgty = sy-msgty.
    ls_vtx_msg-msgv1 = sy-msgv1.
    ls_vtx_msg-msgv2 = sy-msgv2.
    ls_vtx_msg-msgv3 = sy-msgv3.
    ls_vtx_msg-msgv4 = sy-msgv4.
    ls_vtx_msg-msgli = sy-msgli.
    APPEND ls_vtx_msg TO et_messages.
  END-OF-DEFINITION.

  lv_terrain = iv_terrain.

  CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
    EXPORTING
      input      = lv_terrain
    IMPORTING
      output     = lv_terrain
    EXCEPTIONS
      not_found  = 1
      not_active = 2
      OTHERS     = 3.

  PERFORM measurement_create IN PROGRAM /agri/saplglmdm IF FOUND.

  ASSIGN ('(/AGRI/SAPLGLMDM)GS_MDDOC_INFOCUS') TO <ls_mddoc_infocus>.
  IF sy-subrc EQ 0.
    IF iv_document_type IS INITIAL.
*-- Inserir tipo de documento de medição
      MESSAGE e027(/agri/glmd) INTO sy-msgli.
      lv_subrc = 4.
      d_add_message.
      EXIT.
    ELSE.
      SELECT SINGLE mdtyp
        FROM /agri/tglmdtyp
        INTO <ls_mddoc_infocus>-x-mdhdr-mdtyp
       WHERE mdtyp = iv_document_type.
      IF sy-subrc NE 0.
*-- Documento de medição &1 inválido
        MESSAGE e001(zglmd) WITH iv_document_type INTO sy-msgli.
        lv_subrc = 4.
        d_add_message.
        EXIT.
      ENDIF.
    ENDIF.

    IF lv_terrain IS INITIAL.
*-- Inserir terreno
      MESSAGE e008(/agri/glmd) INTO sy-msgli.
      lv_subrc = 4.
      d_add_message.
      EXIT.
    ELSE.
      SELECT SINGLE tplnr_fl
        FROM /agri/glflot
        INTO <ls_mddoc_infocus>-x-mdhdr-tplnr_fl
       WHERE tplnr_fl = lv_terrain.
      <ls_mddoc_infocus>-x-mdhdr-tplnr_fl = lv_terrain.
      IF sy-subrc NE 0.
*-- Terreno &1 inválido
        MESSAGE e002(zglmd) WITH iv_terrain INTO sy-msgli.
        lv_subrc = 4.
        d_add_message.
        EXIT.
      ENDIF.
    ENDIF.

    IF iv_measurement_group IS INITIAL.
*-- Inserir grupo de medições
      MESSAGE e009(/agri/glmd) INTO sy-msgli.
      lv_subrc = 4.
      d_add_message.
      EXIT.
    ELSE.
      <ls_mddoc_infocus>-x-mdhdr-mpgrp = iv_measurement_group.
    ENDIF.

    SELECT *
      FROM /agri/glflcma
      INTO TABLE @DATA(lt_glflcma)
     WHERE tplnr_fl  EQ @lv_terrain
       AND loevm     EQ @abap_false
       AND astat     EQ @c_crop_season_status-active
*-- SOC - Don't consider the replanting crop season while create the measurement document
       AND zzreplant NE @abap_true.
*-- EOC - Don't consider the replanting crop season while create the measurement document

    SORT lt_glflcma BY contr DESCENDING.
    READ TABLE lt_glflcma INTO DATA(ls_glflcma) INDEX 1.

    IF sy-subrc NE 0.
*-- Safra inexistente para talhão informado.
      MESSAGE e111(zfmfp) INTO sy-msgli.
      lv_subrc = 4.
      d_add_message.
      EXIT.
    ENDIF.

    SELECT SINGLE aslvl
      FROM /agri/tglmdtyp
      INTO <ls_mddoc_infocus>-x-mdhdr-aslvl
     WHERE mdtyp EQ <ls_mddoc_infocus>-x-mdhdr-mdtyp.
    IF sy-subrc NE 0
    OR <ls_mddoc_infocus>-x-mdhdr-aslvl IS INITIAL.
*-- Nível não atualizado para tipo de documento
      MESSAGE e028(/agri/glmd) INTO sy-msgli.
      lv_subrc = 4.
      d_add_message.
      EXIT.
    ELSE.
      ASSIGN: ('(/AGRI/SAPLGLMDM)/AGRI/S_GLFLCMA') TO FIELD-SYMBOL(<ls_glflcma>).
      IF sy-subrc EQ 0.
        <ls_glflcma> = ls_glflcma.
        ASSIGN COMPONENT 'DATAB' OF STRUCTURE <ls_glflcma> TO FIELD-SYMBOL(<lv_datab>).
        IF sy-subrc EQ 0.
          <lv_datab> = sy-datum.
        ENDIF.
      ENDIF.
      ASSIGN: ('(/AGRI/SAPLGLMDM)/AGRI/S_GLMDHDR') TO <ls_glmdhdr>.
      IF <ls_glmdhdr> IS ASSIGNED.
        <ls_glmdhdr>-mdtyp = <ls_mddoc_infocus>-x-mdhdr-mdtyp.
        <ls_glmdhdr>-tplnr_fl = <ls_mddoc_infocus>-x-mdhdr-tplnr_fl.
        <ls_glmdhdr>-mpgrp = <ls_mddoc_infocus>-x-mdhdr-mpgrp.
        <ls_glmdhdr>-aslvl = <ls_mddoc_infocus>-x-mdhdr-aslvl.
        <ls_mddoc_infocus>-x-mdhdr-cmnum = <ls_glmdhdr>-cmnum = ls_glflcma-cmnum.
        <ls_mddoc_infocus>-x-mdhdr-datab = <ls_glmdhdr>-datab = ls_glflcma-datab.
        <ls_mddoc_infocus>-x-mdhdr-datbi = <ls_glmdhdr>-datbi = ls_glflcma-datbi.
        <ls_mddoc_infocus>-x-mdhdr-contr = <ls_glmdhdr>-contr = ls_glflcma-contr.
        <ls_glmdhdr>-kfrst = <ls_mddoc_infocus>-x-mdhdr-kfrst.
*        <ls_mddoc_infocus>-x-mdhdr-kfrst = <ls_glmdhdr>-kfrst = 'A'.
        PERFORM attr_group_check IN PROGRAM /agri/saplglmdm IF FOUND.
        PERFORM zabs_attributes_prepare IN PROGRAM /agri/saplglmdm IF FOUND.
        ASSIGN ('(/AGRI/SAPLGLMDM)GT_ATTR_VALUES') TO <lt_attr_values>.
        IF sy-subrc EQ 0.
          LOOP AT <lt_attr_values> ASSIGNING FIELD-SYMBOL(<lwa_attr_value>).
            MOVE-CORRESPONDING <lwa_attr_value> TO ls_mdatv_tmp.
            ls_mdatv_tmp-mdocm = <ls_mddoc_infocus>-x-mdhdr-mdocm.
            ls_mdatv_tmp-updkz = c_updkz_new.
            INSERT ls_mdatv_tmp INTO TABLE <ls_mddoc_infocus>-x-mdatv.
          ENDLOOP.
        ENDIF.

        IF <ls_glmdhdr>-muser IS INITIAL.
          <ls_mddoc_infocus>-x-mdhdr-muser = <ls_glmdhdr>-muser = sy-uname.
        ENDIF.
        IF <ls_glmdhdr>-mdate IS INITIAL.
          <ls_mddoc_infocus>-x-mdhdr-mdate = <ls_glmdhdr>-mdate = sy-datum.
        ENDIF.
        IF <ls_glmdhdr>-mtime IS INITIAL.
          <ls_mddoc_infocus>-x-mdhdr-mtime = <ls_glmdhdr>-mtime = sy-uzeit.
        ENDIF.

        ASSIGN ('(/AGRI/SAPLGLMDM)GS_VARIABLES-DOCUMENT_MODE') TO FIELD-SYMBOL(<lv_doc_mode>).
        IF sy-subrc EQ 0.
          <lv_doc_mode> = c_mode_create.
          <ls_mddoc_infocus>-x-mdhdr-updkz = <ls_glmdhdr>-updkz = c_updkz_new.
          PERFORM md_header_update IN PROGRAM /agri/saplglmdm
                                        USING <ls_mddoc_infocus>-x-mdhdr
                                     CHANGING <ls_glmdhdr> lv_subrc IF FOUND.
          <ls_mddoc_infocus>-x-mdhdr-stsma = <ls_glmdhdr>-stsma.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
    lv_subrc = 4.
    EXIT.
  ENDIF.

  SELECT class FROM /agri/glagha
    INTO TABLE @DATA(lt_agha)
   WHERE klart EQ @c_agtyp-measurements
     AND mdtyp EQ @<ls_mddoc_infocus>-x-mdhdr-mdtyp
     AND aslvl EQ @<ls_mddoc_infocus>-x-mdhdr-aslvl.
  IF sy-subrc EQ 0.
    SORT lt_agha BY class.
  ENDIF.

  READ TABLE lt_agha INTO DATA(lwa_agha)
    WITH KEY class = iv_measurement_group BINARY SEARCH.
  IF sy-subrc NE 0.
    IF sy-subrc NE 0.
*-- Grupo de medições &1 inválido
      MESSAGE e003(zglmd) WITH iv_measurement_group INTO sy-msgli.
      lv_subrc = 4.
      d_add_message.
      EXIT.
    ENDIF.
  ENDIF.

  IF it_attributes[] IS NOT INITIAL.
    SELECT atinn, adzhl, atnam,
           atfor, anzst, anzdz
      FROM cabn
      INTO TABLE @DATA(lt_cabn)
      FOR ALL ENTRIES IN @it_attributes
     WHERE atnam EQ @it_attributes-atnam.

    IF sy-subrc EQ 0.
      SORT lt_cabn BY atnam atinn.

      SELECT *
        FROM zfmfpsafras
        INTO TABLE @DATA(lt_safras)
       WHERE tipo_safra = @c_tipo_safra-tecnica.

      SORT lt_safras BY ano_safra.

      DATA(lt_attributes) = it_attributes[].
      READ TABLE lt_attributes ASSIGNING FIELD-SYMBOL(<ls_data_ref>)
        WITH KEY atnam = c_caracteristica-data.
      IF sy-subrc EQ 0
      AND <ls_data_ref>-atwrt IS NOT INITIAL.
        lv_data_ref = <ls_data_ref>-atwrt+6(4) && <ls_data_ref>-atwrt+3(2)
                      && <ls_data_ref>-atwrt+0(2).
        READ TABLE lt_attributes ASSIGNING FIELD-SYMBOL(<ls_safra>)
          WITH KEY atnam = c_caracteristica-safra.
        IF sy-subrc NE 0.
          INSERT INITIAL LINE INTO TABLE lt_attributes ASSIGNING <ls_safra>.
          IF sy-subrc EQ 0.
            <ls_safra>-atnam = c_caracteristica-safra.
          ENDIF.
        ENDIF.
        IF sy-subrc EQ 0.
          LOOP AT lt_safras INTO DATA(lwa_safra).
            IF lv_data_ref BETWEEN lwa_safra-inicio_safra
                               AND lwa_safra-fim_safra.
              <ls_safra>-atwrt = lwa_safra-ano_safra.
              EXIT.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.

      LOOP AT lt_attributes INTO DATA(ls_attribute).
        READ TABLE lt_cabn INTO DATA(ls_cabn)
          WITH KEY atnam = ls_attribute-atnam BINARY SEARCH.
        IF sy-subrc EQ 0
        AND ls_attribute-atwrt IS NOT INITIAL.
          READ TABLE <ls_mddoc_infocus>-x-mdatv
            ASSIGNING FIELD-SYMBOL(<ls_mdatv>) WITH KEY atinn = ls_cabn-atinn.
          IF sy-subrc EQ 0.
            REFRESH lt_messages.
            CALL METHOD /agri/cl_gattr_utils=>attribute_value_check
              EXPORTING
                i_agtyp     = c_agtyp-measurements
                i_atinn     = <ls_mdatv>-atinn
                i_atwrt     = ls_attribute-atwrt
              IMPORTING
                et_messages = lt_messages[]
              CHANGING
                cs_attr_val = ls_attr_val
                c_valid     = lv_valid.

            IF lv_valid EQ abap_true.
              MOVE-CORRESPONDING ls_attr_val TO <ls_mdatv>.
            ELSE.
              LOOP AT lt_messages INTO ls_message.
                MESSAGE ID ls_message-msgid TYPE ls_message-msgty
                   NUMBER ls_message-msgno WITH ls_message-msgv1
                   ls_message-msgv2 ls_message-msgv3 ls_message-msgv4
                              INTO sy-msgli.
                d_add_message.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF <ls_mddoc_infocus>-x-mdatv[] IS INITIAL.
      lv_subrc = 4.
      MESSAGE ID '/AGRI/GLMD' TYPE c_msg_type-error NUMBER '061'
                              INTO sy-msgli.
      d_add_message.
      EXIT.
    ENDIF.

    REFRESH: lt_mddoc.
    PERFORM zabs_mddoc_infocus_save IN PROGRAM /agri/saplglmdm
                                      CHANGING lt_messages
                                               lv_subrc
                                               lt_mddoc IF FOUND.

    LOOP AT lt_messages INTO ls_message.
      MESSAGE ID ls_message-msgid TYPE ls_message-msgty
         NUMBER ls_message-msgno WITH ls_message-msgv1
         ls_message-msgv2 ls_message-msgv3 ls_message-msgv4
                    INTO sy-msgli.
      d_add_message.
    ENDLOOP.

    IF lv_subrc NE 0.
      IF lv_subrc EQ 1.
        MESSAGE ID '/AGRI/GLOBAL' TYPE c_msg_type-success NUMBER '322'
                                                          INTO sy-msgli.
        d_add_message.
      ELSE.
        MESSAGE ID '/AGRI/GLMD' TYPE c_msg_type-error NUMBER '011'
                                WITH <ls_mddoc_infocus>-x-mdhdr-mdocm
                                INTO sy-msgli.
        d_add_message.
      ENDIF.
    ELSE.
      LOOP AT lt_mddoc INTO DATA(lwa_mddoc_infocus).
        MESSAGE ID '/AGRI/GLMD' TYPE c_msg_type-success NUMBER '006'
           WITH lwa_mddoc_infocus-x-mdhdr-mdocm INTO sy-msgli.
        d_add_message.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFUNCTION.
