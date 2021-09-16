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
        lt_messages2      TYPE /agri/t_gprolog,
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
        ordernumber       TYPE bapi_order_key-order_number,
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
                 <lwa_parameters> TYPE zsc_fmfp_task_upload.

  CONSTANTS: BEGIN OF c_ponto_controle,
               amostra_terrenos TYPE qslwbez VALUE 'Z28',
               selecao_pomar    TYPE qslwbez VALUE 'Z29',
             END OF c_ponto_controle,

             lc_qm01 TYPE plpo-steus VALUE 'QM01'.

  IF <lwa_taskorder>-aufnr  IS ASSIGNED
  AND <lwa_taskorder>-aufnr IS NOT INITIAL.
    ordernumber = <lwa_taskorder>-aufnr.
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
              SHIFT lwa_insppoint-usern1 LEFT DELETING LEADING '0'.
            WHEN c_ponto_controle-selecao_pomar. "Z29
*Terreno:/Variedade:/Caixa Estimada:
              lwa_insppoint-userc1 = lv_terreno.
              lwa_insppoint-userc2 = lv_maktx+8(10).
              IF lwa_glflcma-eston GT 9999999999.
                lwa_insppoint-usern1 = lwa_glflcma-eston.
              ENDIF.
            WHEN OTHERS.
              lwa_insppoint-userc1 = 22.
              lwa_insppoint-userc2 = 22.
              lwa_insppoint-usern1 = 22.
              lwa_insppoint-usern2 = 22.
              lwa_insppoint-userd1 = sy-datum.
              lwa_insppoint-usert1 = sy-uzeit.
          ENDCASE.
        ENDIF.

        ASSIGN ('(ZFMFP_UNPLANNED_TASKORDER)WA_PARAMETERS') TO <lwa_parameters>.
        LOOP AT lt_char_req INTO DATA(lwa_char_req).
          DATA(lv_tabix) = sy-tabix.
          MOVE-CORRESPONDING lwa_char_req TO lwa_sample_result.
          CASE lv_tabix.
            WHEN 1.
              lwa_sample_result-mean_value = <lwa_parameters>-result1.
            WHEN 2.
              lwa_sample_result-mean_value = <lwa_parameters>-result2.
            WHEN 3.
              lwa_sample_result-mean_value = <lwa_parameters>-result3.
            WHEN 4.
              lwa_sample_result-mean_value = <lwa_parameters>-result4.
            WHEN 5.
              lwa_sample_result-mean_value = <lwa_parameters>-result5.
            WHEN 6.
              lwa_sample_result-mean_value = <lwa_parameters>-result6.
            WHEN 7.
              lwa_sample_result-mean_value = <lwa_parameters>-result7.
            WHEN 8.
              lwa_sample_result-mean_value = <lwa_parameters>-result8.
            WHEN 9.
              lwa_sample_result-mean_value = <lwa_parameters>-result9.
            WHEN 10.
              lwa_sample_result-mean_value = <lwa_parameters>-result10.
          ENDCASE.

          IF lwa_insppoint_req-ident_key EQ 'ZLP'.
            lwa_sample_result-mean_value = '1'.
          ENDIF.

          lwa_sample_result-evaluated      = abap_true.
          lwa_sample_result-evaluation     = 'A'. "Aceitação
          lwa_sample_result-start_date     = sy-datlo.
          lwa_sample_result-start_time     = sy-timlo.
          lwa_sample_result-inspector      = sy-uname.
          lwa_sample_result-original_input = lwa_sample_result-mean_value.
          lwa_sample_result-closed         = 'X'.
          APPEND lwa_sample_result TO lt_sample_results.

          lwa_single_result-insplot    = lwa_sample_result-insplot.
          lwa_single_result-inspoper   = lwa_sample_result-inspoper.
          lwa_single_result-inspchar   = lwa_sample_result-inspchar.
          lwa_single_result-inspsample = lwa_sample_result-inspsample.
          lwa_single_result-insp_date  = lwa_sample_result-start_date.
          lwa_single_result-insp_time  = lwa_sample_result-start_time.
          lwa_single_result-res_value  = lwa_sample_result-mean_value.
          APPEND lwa_single_result TO lt_single_results.


          lwa_char_result-insplot         = lwa_sample_result-insplot.
          lwa_char_result-inspoper        = lwa_sample_result-inspoper.
          lwa_char_result-inspchar        = lwa_sample_result-inspchar.
          lwa_char_result-closed          = 'X'.
          lwa_char_result-evaluated       = lwa_sample_result-evaluated.
          lwa_char_result-evaluation      = lwa_sample_result-evaluation.
          lwa_char_result-mean_value      = lwa_sample_result-mean_value.
          lwa_char_result-original_input  = lwa_sample_result-mean_value.
          lwa_char_result-start_date      = sy-datum.
          lwa_char_result-start_time      = sy-uzeit.
          lwa_char_result-inspector       = sy-uname.
          APPEND lwa_char_result TO lt_char_results.

        ENDLOOP.
*--------------------------------------------------------------------*
*             Inspection Lot
*--------------------------------------------------------------------*
* Tabelle CHARRESULTS ist nach Vorgängen sortiert
* Der Aufruf pro Merkmal und evtl. mit Einzelwerten
*        CALL FUNCTION 'BAPI_INSPOPER_RECORDRESULTS'
        CALL FUNCTION 'ZFMFP_INSPOPER_RECORDRESULTS'
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

        INSERT INITIAL LINE INTO TABLE lt_messages2
          ASSIGNING FIELD-SYMBOL(<lwa_message>).
        IF sy-subrc EQ 0.
          READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'E'.
          IF sy-subrc NE 0.
            COMMIT WORK AND WAIT.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = abap_false.
            <lwa_message>-msgty = 'S'.
            <lwa_message>-msgv1 = 'Result Recorded Successfully'. "#EC NOTEXT
            <lwa_message>-msgid = '00'.
            <lwa_message>-msgno = '208'.
          ELSE.
            <lwa_message>-msgty = 'E'.
            <lwa_message>-msgv1 = 'Defects occured during confirmation'. "#EC NOTEXT
            <lwa_message>-msgid = '00'.
            <lwa_message>-msgno = '208'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
