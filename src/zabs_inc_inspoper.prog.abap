*&---------------------------------------------------------------------*
*& Include ZABS_INC_INSPOPER
*&---------------------------------------------------------------------*

  DATA: lt_sample_results TYPE STANDARD TABLE OF bapi2045d3 INITIAL SIZE 0,
        lt_single_results TYPE STANDARD TABLE OF bapi2045d4 INITIAL SIZE 0,
        lwa_single_result LIKE LINE OF lt_single_results,
        lwa_sample_result LIKE LINE OF lt_sample_results,
        lt_char_results   TYPE rplm_tt_bapi2045d2,
        lwa_char_result   LIKE LINE OF lt_char_results,
        lt_return         TYPE STANDARD TABLE OF bapiret2 INITIAL SIZE 0,
        lt_messages       TYPE /agri/t_gprolog,
        lwa_requirements  TYPE bapi2045d5,
        lwa_return        TYPE bapireturn1,
        lwa_tq79          TYPE tq79,
        lwa_tq79t         TYPE tq79t,
        lwa_insppoint     TYPE bapi2045l4,
        lwa_record_ret    TYPE bapiret2,
        lwa_qals1         TYPE qals,
        lwa_qapo2         TYPE qapo,
        lwa_qals2         TYPE qals,
        lv_insplot        TYPE qals-prueflos,
        lv_terreno        TYPE /agri/gltplnr_fl,
        lv_char100        TYPE char100,
        lv_inspoper       TYPE qapo-vornr VALUE '0010'.

  DATA: lt_insppoints     TYPE /agri/t_fmbapi2045l4,
        lt_char_req       TYPE co_mes_bapi2045d1_t,
        lt_char_res       TYPE rplm_tt_bapi2045d2,
        lt_samples        TYPE rplm_tt_bapi2045d3,
        lt_single         TYPE rplm_tt_bapi2045d4,
        lwa_qapo          TYPE qapo,
        lwa_qals          TYPE qals,
        lwa_operation     TYPE bapi2045l2,
        lwa_insppoint_req TYPE bapi2045d5,
        lwa_insppoint_ret TYPE bapiret2.

  FIELD-SYMBOLS: <lwa_fmfphdr>    TYPE /agri/fmfphdr,
                 <lt_messtab>     TYPE ANY TABLE,
                 <lwa_parameters> TYPE zsc_fmfp_task_upload.

  CONSTANTS: BEGIN OF c_ponto_controle,
               amostra_terrenos TYPE qslwbez VALUE 'Z28',
               selecao_pomar    TYPE qslwbez VALUE 'Z29',
             END OF c_ponto_controle,

             lc_qm01 TYPE plpo-steus VALUE 'QM01'.

  IF ordernumber IS NOT INITIAL.
    WAIT UP TO 2 SECONDS.
    ASSIGN ('(SAPLQEEV)QALS-PRUEFLOS') TO FIELD-SYMBOL(<lv_insplot>).
*--------------------------------------------------------------------*
*      Inspection Lot Created
*--------------------------------------------------------------------*
    IF <lv_insplot> IS ASSIGNED
    AND <lv_insplot> IS NOT INITIAL.
      lv_insplot = <lv_insplot>.

      SELECT SINGLE aufnr, plnty, plnnr
        FROM afko
        INTO @DATA(lwa_afko)
       WHERE aufnr = @ordernumber.

      IF sy-subrc EQ 0.
        SELECT plnty, plnnr, plnkn, vornr, steus
          FROM plpo UP TO 1 ROWS
          INTO @DATA(lwa_plpo)
         WHERE plnty = @lwa_afko-plnty
           AND plnnr = @lwa_afko-plnnr
           AND steus = @lc_qm01.
        ENDSELECT.

        IF sy-subrc EQ 0.
          lv_inspoper = lwa_plpo-vornr.
        ENDIF.
      ENDIF.

*--------------------------------------------------------------------*
*      Inspection Lot Data Read
*--------------------------------------------------------------------*
      IF sy-subrc EQ 0.
        CALL FUNCTION 'BAPI_INSPOPER_GETDETAIL'
          EXPORTING
            insplot                = lv_insplot
            inspoper               = lv_inspoper
            read_insppoints        = 'X'
            read_char_requirements = 'X'
            read_char_results      = 'X'
            read_sample_results    = 'X'
            read_single_results    = 'X'
          IMPORTING
            operation              = lwa_operation
            insppoint_requirements = lwa_insppoint_req
            return                 = lwa_insppoint_ret
          TABLES
            insppoints             = lt_insppoints
            char_requirements      = lt_char_req
            char_results           = lt_char_res
            sample_results         = lt_samples
            single_results         = lt_single.

*get unit text and unit iso code
        CALL FUNCTION 'QIBP_GET_OPERATION'
          EXPORTING
            i_insp_lot           = lv_insplot
            i_insp_lot_operation = lv_inspoper
          IMPORTING
            e_qapo               = lwa_qapo
            e_qals               = lwa_qals
          EXCEPTIONS
            wrong_inspection_lot = 1
            wrong_operation      = 2
            operation_not_unique = 3
            OTHERS               = 4.

        lwa_insppoint-insppoint = lwa_insppoint_req-insppoint.
        lwa_insppoint-insplot   = lwa_insppoint_req-insplot.
        lwa_insppoint-inspoper  = lwa_insppoint_req-inspoper.

        SELECT prueflos,
               probenr
          FROM qapp
          INTO TABLE @DATA(lt_qapp)
         WHERE prueflos = @lv_insplot.

        IF sy-subrc EQ 0.
          SORT lt_qapp BY prueflos probenr DESCENDING.
          READ TABLE lt_qapp INTO DATA(lwa_qapp) INDEX 1.
          IF sy-subrc EQ 0.
            lwa_insppoint-insppoint = lwa_qapp-probenr.
          ENDIF.
          lwa_insppoint-insppoint = lwa_insppoint-insppoint + 1.
        ENDIF.

*...Z28/Z29:Variedade: / *Z28:Ano de Plantio: / *Z29:Caixa Estimada:
        ASSIGN ('(ZFMFP_UNPLANNED_TASKORDER)WA_FMFPHDR') TO <lwa_fmfphdr>.
        IF <lwa_fmfphdr> IS ASSIGNED.
          SELECT tplnr_fl, contr, cmnum, eston, ymatnr, zzfazplantio
            INTO TABLE @DATA(t_glflcma)
            FROM /agri/glflcma
           WHERE tplnr_fl EQ @<lwa_fmfphdr>-tplnr_fl
*-- BOC-T_T.KONNO-05.14.21
             AND contr    EQ @<lwa_fmfphdr>-contr
             AND cmnum    EQ @<lwa_fmfphdr>-cmnum
             AND varia    EQ @<lwa_fmfphdr>-varia
             AND class    EQ @<lwa_fmfphdr>-class
             AND astat    EQ 'A'
*-- EOC-T_T.KONNO-05.14.21
             AND loevm    EQ @abap_false
           ORDER BY contr DESCENDING.
          IF sy-subrc EQ 0.
            READ TABLE t_glflcma INTO DATA(lwa_glflcma) INDEX 1.
            IF sy-subrc EQ 0
            AND lwa_glflcma-ymatnr IS NOT INITIAL.
              SELECT SINGLE maktx
                INTO @DATA(lv_maktx)
                FROM makt
               WHERE matnr = @lwa_glflcma-ymatnr
                 AND spras = @sy-langu.
              IF sy-subrc EQ 0.
                TRANSLATE lv_maktx USING '@ £ § ! # / ~ } ] { [ ´ ` = + - _ & * '.
                TRANSLATE lv_maktx USING '( ) & ¨ % $ # @ ! " , . ; : \ | '.
*                CONDENSE lv_maktx NO-GAPS.
              ENDIF.
            ENDIF.
          ENDIF.
          CLEAR: lv_terreno.
          CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
            EXPORTING
              input  = <lwa_fmfphdr>-tplnr_fl
            IMPORTING
              output = lv_terreno.
          CASE lwa_insppoint_req-ident_key.
            WHEN c_ponto_controle-amostra_terrenos. "Z28
*Terreno:/Variedade:/Plantio:
              lwa_insppoint-userc1 = lv_terreno.
              lwa_insppoint-userc2 = lv_maktx+8(10).
              lwa_insppoint-usern1 = lwa_glflcma-zzfazplantio+0(4).
              lwa_insppoint-userd1 = sy-datum.
              lwa_insppoint-usert1 = sy-uzeit.
              UNPACK lwa_insppoint-usern1 TO lwa_insppoint-usern1.
            WHEN c_ponto_controle-selecao_pomar. "Z29
*Terreno:/Variedade:/Caixa Estimada:
              lwa_insppoint-userc1 = lv_terreno.
              lwa_insppoint-userc2 = lv_maktx+8(10).
              IF lwa_glflcma-eston LT 9999999999.
                lwa_insppoint-usern1 = lwa_glflcma-eston.
              ENDIF.
          ENDCASE.
        ENDIF.

        IF lt_char_req[] IS NOT INITIAL.
*-- Fecth Inspection plan characteristics
          SELECT plnty, plnnr, plnkn, kzeinstell,
                 merknr, zaehl, qpmk_zaehl, verwmerkm,
                 mkversion, steuerkz
            FROM plmk
            INTO TABLE @DATA(lt_plmk)
           WHERE plnty EQ @lwa_plpo-plnty
             AND plnnr EQ @lwa_plpo-plnnr
             AND plnkn EQ @lwa_plpo-plnkn
             AND loekz EQ @abap_false.

*-- Fecth Inspection characteristic master
          SELECT zaehler, werks, mkmnr, version, steuerkz
            FROM qpmk
            INTO TABLE @DATA(lt_qpmk)
            FOR ALL ENTRIES IN @lt_char_req
           WHERE zaehler = @lt_char_req-pmstr_char
             AND mkmnr   = @lt_char_req-mstr_char
             AND version = @lt_char_req-vmstr_char.

          SORT lt_qpmk BY zaehler mkmnr version.

*-- Fecth Inspection Characteristic's code group
          SELECT zaehler, mkmnr, version,
                 auswmenge1, katalgart1
            INTO TABLE @DATA(lt_qpmz)
            FROM qpmz
            FOR ALL ENTRIES IN @lt_char_req
           WHERE zaehler = @lt_char_req-pmstr_char
             AND mkmnr   = @lt_char_req-mstr_char
             AND version = @lt_char_req-vmstr_char.

          SORT lt_qpmz BY zaehler mkmnr version.
        ENDIF.

        ASSIGN ('(ZFMFP_UNPLANNED_TASKORDER)WA_PARAMETERS') TO <lwa_parameters>.
        LOOP AT lt_char_req INTO DATA(lwa_char_req).
          DATA(lv_tabix) = sy-tabix.
          CLEAR lwa_sample_result.
          MOVE-CORRESPONDING lwa_char_req TO lwa_sample_result.
          CASE lv_tabix.
            WHEN 1.
              lwa_sample_result-mean_value = <lwa_parameters>-result1.
              lwa_sample_result-last_smpl  = abap_true.
            WHEN 2.
              lwa_sample_result-mean_value = <lwa_parameters>-result2.
              lwa_sample_result-last_smpl  = abap_true.
            WHEN 3.
              lwa_sample_result-mean_value = <lwa_parameters>-result3.
              lwa_sample_result-last_smpl  = abap_true.
            WHEN 4.
              lwa_sample_result-mean_value = <lwa_parameters>-result4.
              lwa_sample_result-last_smpl = abap_true.
            WHEN 5.
              lwa_sample_result-mean_value = <lwa_parameters>-result5.
              lwa_sample_result-last_smpl = abap_true.
            WHEN 6.
              lwa_sample_result-mean_value = <lwa_parameters>-result6.
              lwa_sample_result-last_smpl  = abap_true.
            WHEN 7.
              lwa_sample_result-mean_value = <lwa_parameters>-result7.
              lwa_sample_result-last_smpl = abap_true.
            WHEN 8.
              lwa_sample_result-mean_value = <lwa_parameters>-result8.
              lwa_sample_result-last_smpl  = abap_true.
            WHEN 9.
              lwa_sample_result-mean_value = <lwa_parameters>-result9.
              lwa_sample_result-last_smpl  = abap_true.
            WHEN 10.
              lwa_sample_result-mean_value = <lwa_parameters>-result10.
              lwa_sample_result-last_smpl  = abap_true.
          ENDCASE.

          IF lwa_insppoint_req-ident_key EQ 'ZLP'.
            lwa_sample_result-mean_value = '1'.
          ENDIF.

          IF lwa_char_req-char_type EQ '02'.
            READ TABLE lt_qpmz INTO DATA(lwa_qpmz)
              WITH KEY zaehler = lwa_char_req-pmstr_char
                       mkmnr   = lwa_char_req-mstr_char
                       version = lwa_char_req-vmstr_char BINARY SEARCH.
            IF sy-subrc NE 0.
              CLEAR lwa_qpmz.
              DATA(lv_fill_code) = abap_false.
            ELSE.
              lv_fill_code = abap_true.
            ENDIF.
          ENDIF.

*          lwa_sample_result-evaluated      = abap_true.
*          lwa_sample_result-evaluation     = 'A'. "Aceitação
*          lwa_sample_result-closed         = 'X'.
          lwa_sample_result-start_date     = sy-datlo.
          lwa_sample_result-start_time     = sy-timlo.
          lwa_sample_result-inspector      = sy-uname.
          lwa_sample_result-original_input = lwa_sample_result-mean_value.
          IF lv_fill_code EQ abap_true.
            lwa_sample_result-code1     = lwa_sample_result-mean_value.
            lwa_sample_result-code_grp1 = lwa_qpmz-auswmenge1.
          ENDIF.
          APPEND lwa_sample_result TO lt_sample_results.

          lwa_single_result-insplot    = lwa_sample_result-insplot.
          lwa_single_result-inspoper   = lwa_sample_result-inspoper.
          lwa_single_result-inspchar   = lwa_sample_result-inspchar.
          lwa_single_result-inspsample = lwa_sample_result-inspsample.
          lwa_single_result-insp_date  = lwa_sample_result-start_date.
          lwa_single_result-insp_time  = lwa_sample_result-start_time.
          lwa_single_result-res_value  = lwa_sample_result-mean_value.
          lwa_single_result-code1      = lwa_sample_result-code1.
          lwa_single_result-code_grp1  = lwa_sample_result-code_grp1.
          APPEND lwa_single_result TO lt_single_results.

          lwa_char_result-insplot         = lwa_sample_result-insplot.
          lwa_char_result-inspoper        = lwa_sample_result-inspoper.
          lwa_char_result-inspchar        = lwa_sample_result-inspchar.
          lwa_char_result-closed          = lwa_sample_result-closed.
          lwa_char_result-evaluated       = lwa_sample_result-evaluated.
          lwa_char_result-evaluation      = lwa_sample_result-evaluation.
          lwa_char_result-mean_value      = lwa_sample_result-mean_value.
          lwa_char_result-original_input  = lwa_sample_result-mean_value.
          lwa_char_result-start_date      = sy-datum.
          lwa_char_result-start_time      = sy-uzeit.
          lwa_char_result-inspector       = sy-uname.
          lwa_char_result-code1           = lwa_sample_result-code1.
          lwa_char_result-code_grp1       = lwa_sample_result-code_grp1.
          APPEND lwa_char_result TO lt_char_results.

        ENDLOOP.
*--------------------------------------------------------------------*
*             Inspection Lot
*--------------------------------------------------------------------*
* Tabelle CHARRESULTS ist nach Vorgängen sortiert
* Der Aufruf pro Merkmal und evtl. mit Einzelwerten
        CALL FUNCTION 'BAPI_INSPOPER_RECORDRESULTS'
          EXPORTING
            insplot        = lv_insplot
            inspoper       = lv_inspoper
            insppointdata  = lwa_insppoint
          IMPORTING
            return         = lwa_record_ret
          TABLES
**              char_results   = lt_char_results
            sample_results = lt_sample_results
**             single_results = lt_single_results
            returntable    = lt_return.

        IF lt_return[] IS INITIAL.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.
        ENDIF.

        IF lv_insplot IS NOT INITIAL.
*...Lote de Controle &1 criado com sucesso.
          INSERT INITIAL LINE INTO TABLE et_messtab
            ASSIGNING FIELD-SYMBOL(<lwa_message>).
          IF sy-subrc EQ 0.
            <lwa_message>-msgid  = 'ZFMFP'.
            <lwa_message>-msgnr  = 031.
            <lwa_message>-msgtyp = 'S'.
            <lwa_message>-msgv1  = lv_insplot.
          ENDIF.
        ENDIF.

        LOOP AT lt_return INTO DATA(lwa_return2).
          MESSAGE ID lwa_return2-id TYPE lwa_return2-type
            NUMBER lwa_return2-number
            WITH lwa_return2-message_v1 lwa_return2-message_v2
            lwa_return2-message_v3 lwa_return2-message_v4 INTO sy-msgli.
          INSERT INITIAL LINE INTO TABLE et_messtab
            ASSIGNING <lwa_message>.
          IF sy-subrc EQ 0.
            <lwa_message>-msgid  = lwa_return2-id.
            <lwa_message>-msgnr  = lwa_return2-number.
            <lwa_message>-msgtyp = lwa_return2-type.
            <lwa_message>-msgv1  = lwa_return2-message_v1.
            <lwa_message>-msgv2  = lwa_return2-message_v2.
            <lwa_message>-msgv3  = lwa_return2-message_v3.
            <lwa_message>-msgv4  = lwa_return2-message_v4.
            IF lwa_return2-id EQ 'QT'
            AND lwa_return2-number EQ 406.
              INSERT INITIAL LINE INTO TABLE et_messtab
                ASSIGNING FIELD-SYMBOL(<lwa_message_>).
              IF sy-subrc EQ 0.
*...Informação necessária para Ponto de Controle: Campo '&1' Valor '&2'
                <lwa_message_>-msgid  = 'ZFMFP'.
                <lwa_message_>-msgnr  = 030.
                <lwa_message_>-msgtyp = 'E'.
                IF lwa_insppoint-userc1 IS INITIAL.
                  <lwa_message_>-msgv1 = lwa_insppoint_req-userc1_txt.
                  <lwa_message_>-msgv2 = lwa_insppoint-userc1.
                ELSEIF lwa_insppoint-userc2 IS INITIAL.
                  <lwa_message_>-msgv1 = lwa_insppoint_req-userc2_txt.
                  <lwa_message_>-msgv2 = lwa_insppoint-userc2.
                ELSEIF lwa_insppoint-usern1 IS INITIAL.
                  <lwa_message_>-msgv1 = lwa_insppoint_req-usern1_txt.
                  <lwa_message_>-msgv2 = lwa_insppoint-usern1.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
*        ASSIGN ('(ZFMFP_UNPLANNED_TASKORDER)GT_MESSTAB[]') TO <lt_messtab>.
*        IF sy-subrc EQ 0.
*          <lt_messtab>[] = et_messtab[].
*        ENDIF.
      ENDIF.
    ENDIF.
    WAIT UP TO 2 SECONDS.
    SELECT SINGLE aufnr, prueflos
      INTO @DATA(lwa_afkox)
      FROM afko
     WHERE aufnr = @ordernumber.
    IF sy-subrc EQ 0.
      SELECT prueflos, vorglfnr, probenr, ppsortkey
        INTO @DATA(lwa_qappx) UP TO 1 ROWS
        FROM qapp
       WHERE prueflos = @lwa_afkox-prueflos.
      ENDSELECT.
      IF sy-subrc EQ 0
      AND lwa_qappx-ppsortkey IS NOT INITIAL.
*...Lote de Controle &1 criado com sucesso.
        INSERT INITIAL LINE INTO TABLE et_messtab
          ASSIGNING <lwa_message>.
        IF sy-subrc EQ 0.
          <lwa_message>-msgid  = 'ZFMFP'.
          <lwa_message>-msgnr  = 032.
          <lwa_message>-msgtyp = 'S'.
          <lwa_message>-msgv1  = lwa_qappx-ppsortkey.
        ENDIF.
      ENDIF.
    ENDIF.
    ASSIGN ('(ZFMFP_UNPLANNED_TASKORDER)GT_MESSTAB[]') TO <lt_messtab>.
    IF sy-subrc EQ 0.
      <lt_messtab>[] = et_messtab[].
    ENDIF.
  ENDIF.
