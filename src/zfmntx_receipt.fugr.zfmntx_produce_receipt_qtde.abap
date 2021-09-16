FUNCTION zfmntx_produce_receipt_qtde.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_TICKET) TYPE  /AGRI/FMPRNUM
*"     VALUE(I_EXERCICIO) TYPE  GJAHR
*"     VALUE(I_PESO_LIQUIDO) TYPE  NTGEW
*"     VALUE(I_PESO_BRUTO) TYPE  BRGEW
*"     VALUE(I_PESO_TARA) TYPE  EWEIGHT_TARE
*"     VALUE(I_PESO_REFUGO) TYPE  ZFMPRISW
*"     VALUE(I_DEVOL_TOTAL) TYPE  ZFMPR_DEV_TOT OPTIONAL
*"     VALUE(I_DATA_TURNO_FAB) TYPE  SYST_DATUM
*"     VALUE(T_LOTES) TYPE  ZT_BATCHES
*"  EXPORTING
*"     VALUE(T_RETORNO) TYPE  /AGRI/T_GPROLOG
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ly_rateio,
           lote            TYPE charg_d,
           caixas_liquidas TYPE zabs_del_cxliq,
           caixas_refugo   TYPE zabs_del_cxref,
           pritm           TYPE /agri/fmpritem,
           cx_liq_new      TYPE zabs_del_cxliq,
           cx_ref_new      TYPE zabs_del_cxref,
           zzsboxq         TYPE zabs_del_sboxq,
         END OF ly_rateio.

  DATA: t_msgs             TYPE zabs_tty_sls_msgs,
        lt_prdoc           TYPE /agri/t_fmpr_doc,
        lt_rateio          TYPE STANDARD TABLE OF ly_rateio INITIAL SIZE 0,
        lt_sum             LIKE lt_rateio,
        ls_retorno         LIKE LINE OF t_retorno,
        lt_cmnum           TYPE /agri/t_glcmnum,
        lt_cmqch           TYPE /agri/t_glcmqch,
        lt_prhrq           TYPE /agri/t_fmprhrq,
        lt_mscl01          TYPE /agri/t_fmscl01,
        lt_messages_close  TYPE /agri/t_gprolog,
        ls_message         TYPE /agri/s_gprolog,
        lt_qualt_cond	     TYPE /agri/t_gmagqc,
        lt_prqlt           TYPE /agri/t_fmprqlt,
        lt_return_log      TYPE bapiret2_t,
        ls_return_log      LIKE LINE OF lt_return_log,
        ls_sum             LIKE LINE OF lt_sum,
        ls_fmpritm         TYPE /agri/s_fmpritm,
        ls_mscl01          TYPE /agri/fmscl01,
        ls_prhdr           TYPE /agri/fmprhdr,
        ls_prref           TYPE /agri/s_fmprref,
        ls_aghdr           TYPE /agri/s_gmaghdr,
        lref_op_controller TYPE REF TO /agri/cl_fmpr_op_controller,
        lref_own_produce   TYPE REF TO /agri/cl_fmpr_own_produce,
        lv_subrc           TYPE sysubrc,
        lv_arrendado       TYPE abap_bool VALUE abap_false,
        lv_stop_save       TYPE xfeld,
        lv_result_liq      TYPE f,
        lv_aux             TYPE f,
        lv_integer_liq     TYPE zabs_del_cxliq,
        lv_total_liq_x     TYPE zabs_del_cxliq,
        lv_diferenca       TYPE zabs_del_cxliq,
        lv_calwgth         TYPE /agri/fmcalwgth,
        lv_decimal_liq     TYPE f,
        lv_sobra_liq       TYPE f,
        lv_total_liq       TYPE f,
        lv_result_ref      TYPE f,
        lv_integer_ref     TYPE zabs_del_cxref,
        lv_decimal_ref     TYPE f,
        lv_sobra_ref       TYPE f,
        lv_total_ref       TYPE f,
        lv_proporcao       TYPE f,
        lv_matdoc          TYPE menge_d,
        lv_cxliq           TYPE zabs_del_cxliq,
        lv_log_id          TYPE balnrext,
        lv_caixas_refugo   TYPE zabs_del_cxref,
        dummy.

****Update Indicators
  CONSTANTS: c_updkz_new(1)     TYPE c VALUE 'I',
             c_updkz_update(1)  TYPE c VALUE 'U',
             c_updkz_delete(1)  TYPE c VALUE 'D',
             c_updkz_old(1)     TYPE c VALUE ' ',
****Rel 60E SP6
             c_updkz_newrow     TYPE c VALUE 'N',
****
             c_updkz_propose(1) TYPE c VALUE 'P'. "ESP5 11129 - Generic Fiori app changes,

****Ticket Status
  CONSTANTS : BEGIN OF c_ticket_status,
                closed    TYPE /agri/gleloek VALUE 'C',
                inac_rvsl TYPE /agri/gleloek VALUE 'X',
                saved     TYPE /agri/gleloek VALUE 'S',
                error     TYPE /agri/gleloek VALUE 'E',
              END OF c_ticket_status.

****Message Types
  CONSTANTS : BEGIN OF c_msg_type,
                info    LIKE sy-msgty VALUE 'I',
                warning LIKE sy-msgty VALUE 'W',
                error   LIKE sy-msgty VALUE 'E',
                abend   LIKE sy-msgty VALUE 'A',
                success LIKE sy-msgty VALUE 'S',
                x       LIKE sy-msgty VALUE 'X',
              END OF c_msg_type,

              BEGIN OF c_appl_log,
                object    TYPE balobj_d  VALUE 'ZREF',
                subobject TYPE balsubobj VALUE 'ZREF-SMQ2',
              END OF c_appl_log.

****System Message Add
  DEFINE system_message_add.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.

    IF sy-msgty IS NOT INITIAL
    AND sy-msgid IS NOT INITIAL
    AND sy-msgno IS NOT INITIAL.
      ls_return_log-type       = sy-msgty.
      ls_return_log-id         = sy-msgid.
      ls_return_log-number     = sy-msgno.
      ls_return_log-message    = sy-msgli.
      ls_return_log-message_v1 = sy-msgv1.
      ls_return_log-message_v2 = sy-msgv2.
      ls_return_log-message_v3 = sy-msgv3.
      ls_return_log-message_v4 = sy-msgv4.
      APPEND ls_return_log TO lt_return_log.
    ENDIF.
  END-OF-DEFINITION.

***Return Message Add
  DEFINE return_message_add.
    LOOP AT &1 INTO ls_message.
    MESSAGE ID ls_message-msgid TYPE ls_message-msgty
      NUMBER ls_message-msgno
      WITH ls_message-msgv1 ls_message-msgv2
      ls_message-msgv3 ls_message-msgv4 INTO sy-msgli.

      ls_return_log-type       = ls_message-msgty.
      ls_return_log-id         = ls_message-msgid.
      ls_return_log-number     = ls_message-msgno.
      ls_return_log-message    = sy-msgli.
      ls_return_log-message_v1 = ls_message-msgv1.
      ls_return_log-message_v2 = ls_message-msgv2.
      ls_return_log-message_v3 = ls_message-msgv3.
      ls_return_log-message_v4 = ls_message-msgv4.
      APPEND ls_return_log TO lt_return_log.
    ENDLOOP.
  END-OF-DEFINITION.

  IF i_ticket NOT BETWEEN '6000000000'
                      AND '6999999999'.
*-- Número de Ticket & inválido!
    MESSAGE ID 'ZFMFP' TYPE 'E'
      NUMBER 146 WITH i_ticket INTO sy-msgli.
    MESSAGE ID 'ZFMFP' TYPE 'I'
      NUMBER 146 WITH i_ticket INTO dummy.
    lv_stop_save = abap_true.
  ENDIF.

  DATA(lo_appl_log) = NEW ys4_appl_log( ).
  lo_appl_log->log_clear( ).
  lv_log_id = |{ i_ticket }| && '-' && |{ i_exercicio }|.

  lo_appl_log->log_initialize(
  EXPORTING
    object      = c_appl_log-object
    subobject   = c_appl_log-subobject
    extnumber   = lv_log_id
  EXCEPTIONS
    init_error = 1
    OTHERS     = 2 ).

  PERFORM document_infocus_lock USING i_ticket
                                      i_exercicio
                             CHANGING lv_subrc.

  IF lv_subrc EQ 0.
    CALL FUNCTION '/AGRI/FMPR_VIEW_SINGLE'
      EXPORTING
        i_prnum        = i_ticket
        i_gjahr        = i_exercicio
      IMPORTING
        es_prdoc       = gs_prdoc_infocus
      EXCEPTIONS ##FM_SUBRC_OK
        no_data_exists = 1
        OTHERS         = 2.

    IF sy-subrc EQ 0.
      LOOP AT gs_prdoc_infocus-x-pritm INTO DATA(ls_pritm).
        READ TABLE t_lotes INTO DATA(ls_lote)
          WITH KEY lote = ls_pritm-charg.
        IF sy-subrc NE 0.
*-- Batch &1 incorreto! Enviar batches originados na fazenda!
          MESSAGE ID 'ZFMFP' TYPE 'E'
            NUMBER 189 INTO sy-msgli.
          MESSAGE ID 'ZFMFP' TYPE 'E'
            NUMBER 189 INTO dummy.
          lv_stop_save = abap_true.
        ENDIF.
      ENDLOOP.

      IF lv_stop_save EQ abap_false.
        IF gs_prdoc_infocus-x-prhdr-status EQ c_ticket_status-closed.
          IF gs_prdoc_infocus-x-pritm[] IS NOT INITIAL.
            SELECT key1, key2, key3, key4, key5, key6,
                   werks, menge, matnr, charg
              FROM matdoc
              INTO TABLE @DATA(lt_matdoc_db)
              FOR ALL ENTRIES IN @gs_prdoc_infocus-x-pritm
             WHERE werks = @gs_prdoc_infocus-x-prhdr-werks
               AND matnr = @gs_prdoc_infocus-x-pritm-plnbez
               AND charg = @gs_prdoc_infocus-x-pritm-charg.

            CLEAR lv_cxliq.
            LOOP AT gs_prdoc_infocus-x-pritm INTO DATA(ls_fmpritm_aux).
              lv_cxliq = lv_cxliq + ls_fmpritm_aux-zzcxliq.
            ENDLOOP.

            CLEAR lv_matdoc.
            LOOP AT lt_matdoc_db INTO DATA(ls_matdoc_db).
              lv_matdoc = lv_matdoc + ls_matdoc_db-menge.
            ENDLOOP.

            IF gs_prdoc_infocus-x-prhdr-zarrend NE abap_true
            AND gs_prdoc_infocus-x-prhdr-zdevol_total NE abap_true.
              IF lv_matdoc LT lv_cxliq.
                UPDATE /agri/fmprhdr SET status = c_ticket_status-saved
                  WHERE prnum = gs_prdoc_infocus-x-prhdr-prnum
                    AND gjahr = gs_prdoc_infocus-x-prhdr-gjahr.
                IF sy-subrc EQ 0.
                  COMMIT WORK AND WAIT.
                  WAIT UP TO 1 SECONDS.
                  gs_prdoc_infocus-x-prhdr-status = c_ticket_status-saved.
                ENDIF.
              ELSE.
*-- Viagem salva com sucesso
                INSERT INITIAL LINE INTO TABLE t_retorno
                  ASSIGNING FIELD-SYMBOL(<ls_retorno>).
                IF sy-subrc EQ 0.
                  <ls_retorno>-msgid = 'ZFMFP'.
                  <ls_retorno>-msgno = 266.
                  <ls_retorno>-msgty = 'S'.
                  <ls_retorno>-msgv1 = i_ticket.
                ENDIF.
              ENDIF.
            ELSE.
*-- Viagem salva com sucesso
              INSERT INITIAL LINE INTO TABLE t_retorno
                ASSIGNING <ls_retorno>.
              IF sy-subrc EQ 0.
                <ls_retorno>-msgid = 'ZFMFP'.
                <ls_retorno>-msgno = 266.
                <ls_retorno>-msgty = 'S'.
                <ls_retorno>-msgv1 = i_ticket.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      IF lv_stop_save EQ abap_false.
        gs_prdoc_infocus-updkz = c_updkz_update.
        gs_prdoc_infocus-x-prhdr-updkz = c_updkz_update.
        gs_prdoc_infocus-x-prhdr-zdevol_total = i_devol_total.
        gs_prdoc_infocus-x-prhdr-budat = i_data_turno_fab.

        IF i_devol_total EQ abap_false.
          gs_prdoc_infocus-x-prhdr-ntgew = i_peso_liquido.
          gs_prdoc_infocus-x-prhdr-brgew = i_peso_bruto.
          gs_prdoc_infocus-x-prhdr-obgew = i_peso_tara.
          gs_prdoc_infocus-x-prhdr-zfmprisw = i_peso_refugo.

          LOOP AT gs_prdoc_infocus-x-pritm INTO ls_pritm.
            ls_sum-lote = ls_pritm-charg.
            ls_sum-zzsboxq = ls_pritm-zzsboxq.
            COLLECT ls_sum INTO lt_sum.
          ENDLOOP.

          SORT lt_sum BY lote.
          DATA(lt_lotes_doc) = gs_prdoc_infocus-x-pritm[].
          DELETE lt_lotes_doc WHERE charg IS INITIAL.
          DATA(lv_lotes_doc) = lines( lt_lotes_doc ).
          DATA(lv_lotes_param) = lines( t_lotes ).

          LOOP AT t_lotes INTO DATA(ls_lote_param).
            CLEAR: lv_result_liq, lv_integer_liq, lv_decimal_liq,
                   lv_result_ref, lv_integer_ref, lv_decimal_ref,
                   lv_sobra_liq, lv_sobra_ref, lv_total_liq,
                   lv_total_ref.
            lt_lotes_doc[] = gs_prdoc_infocus-x-pritm[].
            DELETE lt_lotes_doc WHERE charg NE ls_lote_param-lote.
            SORT lt_lotes_doc BY charg pritm.
            lv_lotes_doc = lines( lt_lotes_doc ).
            IF lv_lotes_doc EQ 1.
              READ TABLE gs_prdoc_infocus-x-pritm ASSIGNING FIELD-SYMBOL(<ls_pritm>)
                WITH KEY charg = ls_lote_param-lote.
              IF sy-subrc EQ 0.
                <ls_pritm>-updkz = c_updkz_update.
                <ls_pritm>-dirwgth = <ls_pritm>-zzcxliq = ls_lote_param-caixas_liquidas.
                MOVE <ls_pritm>-dirwgth TO <ls_pritm>-calwgth.
                lv_calwgth = lv_calwgth + <ls_pritm>-calwgth.
                <ls_pritm>-zzcxref = ls_lote_param-caixas_refugo.
              ENDIF.
            ELSE.
              DATA(lv_proc) = 0.
              CLEAR lv_total_liq_x.
              READ TABLE lt_lotes_doc INTO DATA(ls_lote_doc)
                WITH KEY charg = ls_lote_param-lote BINARY SEARCH.
              WHILE sy-subrc EQ 0.
                DATA(lv_tabix) = sy-tabix + 1.
                ADD 1 TO lv_proc.
                READ TABLE lt_sum INTO ls_sum
                  WITH KEY lote = ls_lote_param-lote BINARY SEARCH.
                IF sy-subrc EQ 0.
                  IF ls_sum-zzsboxq IS NOT INITIAL.
                    lv_proporcao = ( ls_lote_doc-zzsboxq * 100 ) / ls_sum-zzsboxq.
                    READ TABLE gs_prdoc_infocus-x-pritm ASSIGNING <ls_pritm>
                      WITH KEY pritm = ls_lote_doc-pritm.
                    IF sy-subrc EQ 0.
                      <ls_pritm>-updkz = c_updkz_update.
                      lv_aux = ls_lote_param-caixas_liquidas.
                      lv_result_liq = ( lv_aux * lv_proporcao ) / 100.
                      lv_integer_liq = lv_result_liq.
                      lv_decimal_liq = lv_result_liq - lv_integer_liq.
                      lv_sobra_liq = lv_sobra_liq + lv_decimal_liq.
                      lv_total_liq = lv_total_liq + lv_integer_liq.
                      lv_total_liq_x = lv_total_liq_x + lv_integer_liq.
                      <ls_pritm>-dirwgth = <ls_pritm>-zzcxliq = lv_integer_liq.
                      MOVE <ls_pritm>-dirwgth TO <ls_pritm>-calwgth.
                      lv_calwgth = lv_calwgth + <ls_pritm>-calwgth.
                      lv_aux = ls_lote_param-caixas_refugo.
                      lv_result_ref = ( lv_aux * lv_proporcao ) / 100.
                      lv_integer_ref = lv_result_ref.
                      lv_decimal_ref = lv_result_ref - lv_integer_ref.
                      lv_sobra_ref = lv_sobra_ref + lv_decimal_ref.
                      <ls_pritm>-zzcxref = lv_integer_ref.
                      lv_total_ref = lv_total_ref + lv_integer_ref.
                    ENDIF.
                  ENDIF.
                ENDIF.
                READ TABLE lt_lotes_doc INTO ls_lote_doc
                  INDEX lv_tabix COMPARING charg.
              ENDWHILE.
*-- Verificação Adicional
              lv_caixas_refugo = lv_total_ref.
              IF lv_caixas_refugo NE ls_lote_param-caixas_refugo.
                IF lv_caixas_refugo GT ls_lote_param-caixas_refugo.
                  DATA(lv_subtract_ref) = abap_true.
                ELSEIF ls_lote_param-caixas_refugo GT lv_total_ref.
                  lv_subtract_ref = abap_false.
                ENDIF.

                lv_diferenca = abs( lv_caixas_refugo - ls_lote_param-caixas_refugo ).

                READ TABLE gs_prdoc_infocus-x-pritm ASSIGNING <ls_pritm>
                  WITH KEY charg = ls_lote_param-lote.
                IF sy-subrc EQ 0.
                  IF lv_subtract_ref EQ abap_true.
                    <ls_pritm>-zzcxref = <ls_pritm>-zzcxref - lv_diferenca.
                  ELSEIF lv_subtract_ref EQ abap_false.
                    <ls_pritm>-zzcxref = <ls_pritm>-zzcxref + lv_diferenca.
                  ENDIF.
                ENDIF.
              ENDIF.

              IF lv_total_liq_x NE ls_lote_param-caixas_liquidas.
                IF lv_total_liq_x GT ls_lote_param-caixas_liquidas.
                  DATA(lv_subtract_liq) = abap_true.
                ELSEIF ls_lote_param-caixas_liquidas GT lv_total_liq_x.
                  lv_subtract_liq = abap_false.
                ENDIF.

                lv_diferenca = abs( lv_total_liq_x - ls_lote_param-caixas_liquidas ).

                READ TABLE gs_prdoc_infocus-x-pritm ASSIGNING <ls_pritm>
                  WITH KEY charg = ls_lote_param-lote.
                IF sy-subrc EQ 0.
                  IF lv_subtract_liq EQ abap_true.
                    <ls_pritm>-dirwgth = <ls_pritm>-dirwgth - lv_diferenca.
                    <ls_pritm>-zzcxliq = <ls_pritm>-zzcxliq - lv_diferenca.
                    MOVE <ls_pritm>-dirwgth TO <ls_pritm>-calwgth.
                    lv_calwgth = lv_calwgth - lv_diferenca.
                  ELSEIF lv_subtract_liq EQ abap_false.
                    <ls_pritm>-dirwgth = <ls_pritm>-dirwgth + lv_diferenca.
                    <ls_pritm>-zzcxliq = <ls_pritm>-zzcxliq + lv_diferenca.
                    MOVE <ls_pritm>-dirwgth TO <ls_pritm>-calwgth.
                    lv_calwgth = lv_calwgth + lv_diferenca.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDLOOP.

          gs_prdoc_infocus-x-prhdr-calwgth = lv_calwgth.
        ELSEIF i_devol_total EQ abap_true.
          LOOP AT gs_prdoc_infocus-x-pritm ASSIGNING <ls_pritm>.
            <ls_pritm>-zzcxref = <ls_pritm>-zzsboxq.
            <ls_pritm>-updkz = c_updkz_update.
          ENDLOOP.
        ENDIF.

*-- Call Badi Document Check
        PERFORM badi_document_check IN PROGRAM /agri/saplfmprm
                                         USING gs_prdoc_infocus
                                      CHANGING lv_stop_save IF FOUND.
      ENDIF.

      IF lv_stop_save EQ abap_false.
        APPEND gs_prdoc_infocus TO lt_prdoc.
        CALL FUNCTION '/AGRI/FMPR_SAVE'
          EXPORTING
            i_commit_work = 'X'
          CHANGING
            ct_prdoc      = lt_prdoc
            ct_messages   = t_retorno
          EXCEPTIONS
            no_change     = 1
            OTHERS        = 2.

        lv_subrc = sy-subrc.
        IF lv_subrc NE 0.
          system_message_add.
        ENDIF.

        READ TABLE t_retorno WITH KEY msgty = 'E'
           TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          IF lv_subrc NE 1.
            lv_subrc = 4.
          ENDIF.
        ELSE.
          IF gs_prdoc_infocus-x-prhdr-zarrend IS INITIAL
          AND gs_prdoc_infocus-x-prhdr-zdevol_total IS INITIAL.
            CREATE OBJECT lref_op_controller.

            sy-tfill = lines( gs_prdoc_infocus-x-pritm ).

            IF sy-tfill EQ 1. " Without generate tickets
*-- Closure: Own Produce
              CALL METHOD lref_op_controller->close_ticket_008
                EXPORTING
                  i_prnum = gs_prdoc_infocus-x-prhdr-prnum
                  i_gjahr = gs_prdoc_infocus-x-prhdr-gjahr.
            ELSE. " Generates multiple tickets
*-- Generate multiple tickets
              CALL METHOD /agri/cl_fmpr_op_controller=>generate_ticket_001
                EXPORTING
                  i_prnum  = gs_prdoc_infocus-x-prhdr-prnum
                  i_gjahr  = gs_prdoc_infocus-x-prhdr-gjahr
                IMPORTING
                  et_prhrq = lt_prhrq.

              LOOP AT lt_prhrq ASSIGNING FIELD-SYMBOL(<ls_prhrq>).
*--  Closure: Own Produce
                CALL METHOD lref_op_controller->close_ticket_008
                  EXPORTING
                    i_prnum = <ls_prhrq>-r_prnum
                    i_gjahr = <ls_prhrq>-r_gjahr.
              ENDLOOP.

*-- Get process versions
              CREATE OBJECT lref_own_produce
                EXPORTING
                  i_prnum          = <ls_prhrq>-r_prnum
                  i_gjahr          = <ls_prhrq>-r_gjahr
                EXCEPTIONS
                  ticket_not_found = 1
                  OTHERS           = 2.

              IF sy-subrc = 0.
                lref_own_produce->get_version_process( IMPORTING et_scl01 =  lt_mscl01 ).
                READ TABLE lt_mscl01 INTO ls_mscl01 INDEX 1.
              ELSE.
                system_message_add.
              ENDIF.

*-- Update generated tickets in view items
              LOOP AT lt_prhrq ASSIGNING <ls_prhrq>.
*-- Get reference
                CALL METHOD /agri/cl_fm_master_ticket=>get_details
                  EXPORTING
                    i_prnum          = <ls_prhrq>-r_prnum
                    i_gjahr          = <ls_prhrq>-r_gjahr
                  IMPORTING
                    es_prhdr         = ls_prhdr
                    es_prref         = ls_prref
                  EXCEPTIONS
                    ticket_not_found = 1
                    OTHERS           = 2.

                IF sy-subrc NE 0.
                  system_message_add.
                ENDIF.

                IF <ls_prhrq>-posnr IS NOT INITIAL.
                  READ TABLE gs_prdoc_infocus-x-pritm INTO ls_fmpritm
                    WITH KEY pritm = <ls_prhrq>-posnr.
                  IF sy-subrc = 0.
                    ls_fmpritm-r_prnum = <ls_prhrq>-r_prnum.
                    ls_fmpritm-charg   = ls_prref-charg.
                    MODIFY gs_prdoc_infocus-x-pritm FROM ls_fmpritm INDEX sy-tabix
                       TRANSPORTING r_prnum charg.
                  ENDIF.
                ENDIF.

*-- Update status at log level
                REFRESH lt_messages_close.
                CALL METHOD lref_own_produce->update_ticket_status_at_log
                  EXPORTING
                    is_prhdr    = ls_prhdr
                    i_setproc   = ls_mscl01-setproc
                    i_idproc    = ls_mscl01-idproc
                  IMPORTING
                    et_messages = lt_messages_close.

                return_message_add lt_messages_close.
              ENDLOOP.

            ENDIF.
          ELSE.
            IF gs_prdoc_infocus-x-prhdr-zarrend EQ abap_true.
*-- Atenção: Imóvel arrendado!
              MESSAGE ID 'ZFMFP' TYPE 'I'
                NUMBER 080 INTO sy-msgli.
              MESSAGE ID 'ZFMFP' TYPE 'I'
                NUMBER 080 INTO dummy.
              system_message_add.
            ELSEIF gs_prdoc_infocus-x-prhdr-zdevol_total EQ abap_true.
*-- Atenção: Devolução Total!
              MESSAGE ID 'ZFMFP' TYPE 'I'
                NUMBER 087 INTO sy-msgli.
              MESSAGE ID 'ZFMFP' TYPE 'I'
                NUMBER 087 INTO dummy.
              system_message_add.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        lv_subrc = 4.
        system_message_add.
      ENDIF.

      IF lv_subrc NE 0.
        IF lv_subrc EQ 1.
          MESSAGE ID '/AGRI/GLOBAL' TYPE c_msg_type-success
            NUMBER '322' INTO dummy.
          system_message_add.
        ELSE.
          MESSAGE ID '/AGRI/FMPR' TYPE c_msg_type-error
            NUMBER '009' INTO dummy.
          system_message_add.
        ENDIF.
      ELSE.
        MESSAGE ID '/AGRI/FMPR' TYPE c_msg_type-success
          NUMBER '007' WITH gs_prdoc_infocus-x-prhdr-prnum INTO dummy.
        system_message_add.
      ENDIF.
    ENDIF.
  ENDIF.

  PERFORM document_infocus_unlock USING i_ticket
                                        i_exercicio.

*-- Recuperar mensagens processamento
  CALL FUNCTION 'ZABS_FM_MEMORY_MSGS'
    IMPORTING
      et_messages = t_msgs[].

*-- Ticket Balança &1 não finalizado. Reenviar função.
  SELECT SINGLE *
    FROM /agri/fmprhdr
    INTO @DATA(ls_fmprhdr_db)
   WHERE prnum = @gs_prdoc_infocus-x-prhdr-prnum
     AND gjahr = @gs_prdoc_infocus-x-prhdr-gjahr.

  IF sy-subrc EQ 0.
    SELECT prnum, gjahr, pritm, plnbez, charg, zzcxliq
      FROM /agri/fmpritm
      INTO TABLE @DATA(lt_fmpritm_db)
     WHERE prnum = @gs_prdoc_infocus-x-prhdr-prnum
       AND gjahr = @gs_prdoc_infocus-x-prhdr-gjahr.

    IF sy-subrc EQ 0.
      REFRESH lt_matdoc_db.
      SELECT key1, key2, key3, key4, key5, key6,
             werks, menge, matnr, charg
        FROM matdoc
        INTO TABLE @lt_matdoc_db
        FOR ALL ENTRIES IN @lt_fmpritm_db
       WHERE werks = @gs_prdoc_infocus-x-prhdr-werks
         AND matnr = @lt_fmpritm_db-plnbez
         AND charg = @lt_fmpritm_db-charg.
    ENDIF.

    CLEAR lv_cxliq.
    LOOP AT lt_fmpritm_db INTO DATA(ls_fmpritm_db).
      lv_cxliq = lv_cxliq + ls_fmpritm_db-zzcxliq.
    ENDLOOP.

    CLEAR lv_matdoc.
    LOOP AT lt_matdoc_db INTO ls_matdoc_db.
      lv_matdoc = lv_matdoc + ls_matdoc_db-menge.
    ENDLOOP.

    IF ls_fmprhdr_db-zarrend EQ abap_true
    OR ls_fmprhdr_db-zdevol_total EQ abap_true.
      UPDATE /agri/fmprhdr SET status = c_ticket_status-closed
        WHERE prnum = ls_fmprhdr_db-prnum
          AND gjahr = ls_fmprhdr_db-gjahr.
    ELSE.
      IF lv_matdoc LT lv_cxliq.
        INSERT INITIAL LINE INTO TABLE lt_return_log
          ASSIGNING FIELD-SYMBOL(<ls_return_log>).
        IF sy-subrc EQ 0.
*-- Viagem não finalizada. Reenviar.
          <ls_return_log>-type   = c_msg_type-error.
          <ls_return_log>-id     = 'ZFMFP'.
          <ls_return_log>-number = 246.
        ENDIF.

        UPDATE /agri/fmprhdr SET status = c_ticket_status-error
          WHERE prnum = ls_fmprhdr_db-prnum
            AND gjahr = ls_fmprhdr_db-gjahr.
      ELSEIF lv_matdoc EQ lv_cxliq.
        IF ls_fmprhdr_db-status NE c_ticket_status-closed.
          UPDATE /agri/fmprhdr SET status = c_ticket_status-closed
            WHERE prnum = ls_fmprhdr_db-prnum
              AND gjahr = ls_fmprhdr_db-gjahr.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  LOOP AT t_msgs ASSIGNING FIELD-SYMBOL(<ls_msg>).
    IF <ls_msg>-msgty  = 'I'
    AND <ls_msg>-lineno = '999999'.
      <ls_msg>-msgty  = 'E'.
      INSERT INITIAL LINE INTO TABLE lt_return_log
        ASSIGNING <ls_return_log>.
      IF sy-subrc EQ 0.
        <ls_return_log>-type       = <ls_msg>-msgty.
        <ls_return_log>-id         = <ls_msg>-msgid.
        <ls_return_log>-number     = <ls_msg>-msgno.
        <ls_return_log>-message_v1 = <ls_msg>-msgv1.
        <ls_return_log>-message_v2 = <ls_msg>-msgv2.
        <ls_return_log>-message_v3 = <ls_msg>-msgv3.
        <ls_return_log>-message_v4 = <ls_msg>-msgv4.
      ENDIF.
    ENDIF.
  ENDLOOP.

*-- Output Message Add
  LOOP AT lt_return_log INTO ls_return_log.
    CLEAR ls_retorno.
    READ TABLE t_retorno TRANSPORTING NO FIELDS
      WITH KEY msgty = ls_return_log-type
               msgid = ls_return_log-id
               msgno = ls_return_log-number.
    IF sy-subrc NE 0.
      ls_retorno-msgty = ls_return_log-type.
      ls_retorno-msgid = ls_return_log-id.
      ls_retorno-msgno = ls_return_log-number.
      ls_retorno-msgv1 = ls_return_log-message_v1.
      ls_retorno-msgv2 = ls_return_log-message_v2.
      ls_retorno-msgv3 = ls_return_log-message_v3.
      ls_retorno-msgv4 = ls_return_log-message_v4.
      APPEND ls_retorno TO t_retorno.
    ENDIF.
  ENDLOOP.

  lo_appl_log->log_write_bapiret(
    EXPORTING
      bapiret2        = lt_return_log
      object          = 'ZREF'
      subobject       = 'ZREF-SMQ2'
      extnumber       = lv_log_id
    EXCEPTIONS
      bapiret_initial = 1
      write_error     = 2
      OTHERS          = 3 ).

  IF sy-subrc EQ 0.
    lo_appl_log->log_db_save(
      EXCEPTIONS
        log_not_initialized = 1
        db_save_error       = 2
        OTHERS              = 3 ).

    lo_appl_log->log_clear( ).

    IF line_exists( lt_return_log[ type = 'E' ] ).
      DATA(ls_return) = lt_return_log[ type = 'E' ].

      MESSAGE ID ls_return-id
            TYPE ls_return-type
          NUMBER ls_return-number
            WITH ls_return-message_v1
                 ls_return-message_v2
                 ls_return-message_v3
                 ls_return-message_v4
         RAISING posting_error.
    ENDIF.
  ENDIF.

ENDFUNCTION.
