class ZCL_ABS_BADI_FMPR_ALL definition
  public
  final
  create public .

public section.

  interfaces /AGRI/IF_EX_BADI_FMPR_ALL .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ABS_BADI_FMPR_ALL IMPLEMENTATION.


  METHOD /agri/if_ex_badi_fmpr_all~after_save.

    TYPES: BEGIN OF ly_pritm,
             pritm          TYPE /agri/fmpritem,
             charg          TYPE charg_d,
             zzsboxq        TYPE zabs_del_sboxq,
             zzcarimbo      TYPE zabs_del_carimbo,
             carimbos_dif   TYPE abap_bool,
             fruta_problema TYPE abap_bool,
             counter        TYPE i,
             updkz          TYPE updkz_d,
           END OF ly_pritm.

    DATA: lt_stack              TYPE cl_abap_get_call_stack=>call_stack_internal,
          lt_formatted_stack    TYPE cl_abap_get_call_stack=>formatted_entry_stack,
          wa_prhdr              TYPE /agri/s_fmprhdr,
          t_pritm	              TYPE /agri/t_fmpritm,
          t_relevancia          TYPE STANDARD TABLE OF ly_pritm INITIAL SIZE 0,
          t_msgs                TYPE zabs_tty_sls_msgs,
          t_allocvaluesnum      TYPE tt_bapi1003_alloc_values_num,
          t_service_type        TYPE STANDARD TABLE OF ys4s_service_type INITIAL SIZE 0,
          t_service_return      TYPE STANDARD TABLE OF ys4s_os_return INITIAL SIZE 0,
          wa_allocvaluesnum     LIKE LINE OF t_allocvaluesnum,
          lr_cuobn              TYPE RANGE OF cuobn,
          lwa_cuobn             LIKE LINE OF lr_cuobn,
          t_allocvalueschar     TYPE tt_bapi1003_alloc_values_char,
          wa_allocvalueschar    LIKE LINE OF t_allocvalueschar,
          t_allocvaluescurr     TYPE tt_bapi1003_alloc_values_curr,
          wa_allocvaluescurr    LIKE LINE OF t_allocvaluescurr,
          t_return              TYPE bapiret2_t,
          t_char_batch          TYPE STANDARD TABLE OF clbatch INITIAL SIZE 0,
          t_char                TYPE STANDARD TABLE OF api_char INITIAL SIZE 0,
          t_alloclist_old       TYPE TABLE OF bapi1003_alloc_list,
          t_allocvaluescurr_old TYPE tt_bapi1003_alloc_values_curr,
          t_allocvaluesnum_old  TYPE tt_bapi1003_alloc_values_num,
          t_allocvalueschar_old TYPE tt_bapi1003_alloc_values_char,
          t_messages_old        TYPE bapiret2_tt,
          t_retorno_balanca     TYPE bapirettab,
          wa_ymcha              TYPE mcha,
          wa_yupdmcha           TYPE updmcha,
          v_documento           TYPE /scmtms/tor_id,
          v_planta              TYPE werks_d,
          v_carimbo_c           TYPE zabs_del_carimbo,
          v_carimbo_n           TYPE yotpclht_d,
          v_fmname              TYPE funcname,
          v_order_id            TYPE /scmtms/tor_id,
          v_objid               TYPE zabs_del_objid VALUE 'CARI',
          v_klval               TYPE zabs_del_k1val VALUE 'FRUTA_FCOJ',
          v_fcoj                TYPE zabs_del_carimbo,
          v_farm_id             TYPE werks_d,
          v_direction           TYPE ys4e_bal_direction,
          v_peso_total          TYPE /scmtms/qua_gro_wei_val,
          v_un_medida           TYPE /scmtms/qua_gro_wei_uni,
          v_date                TYPE ys4e_baldate,
          v_time                TYPE ys4e_baltime,
          v_peso_bruto          TYPE /scmtms/qua_gro_wei_val,
          v_um_bruto            TYPE /scmtms/qua_gro_wei_uni,
          v_peso_tara           TYPE /scmtms/qua_pack_unl_wei_val,
          v_um_tara             TYPE /scmtms/qua_pack_unl_wei_uni,
          v_class               TYPE klah-class,
          v_objectkey           TYPE bapi1003_key-object,
          v_objectkey_old       TYPE bapi1003_key-object_long,
          v_objecttable         TYPE bapi1003_key-objecttable VALUE 'MCH1',
          v_classnum            TYPE bapi1003_key-classnum VALUE 'Z_FRUTA_MP',
          v_classtype           TYPE bapi1003_key-classtype VALUE '023',
          v_classif_status      TYPE clstatus,
          v_fieldname           TYPE fieldname,
          v_float               TYPE cawn-atflv,
          v_atwrt               TYPE cawn-atwrt,
          v_tor_load            TYPE /scmtms/tor_id,
          v_farm_load           TYPE werks_d,
          v_event_load          TYPE /saptrx/ev_evtid,
          v_date_load           TYPE ys4e_lu_date,
          v_atflv               TYPE zabs_del_inlat,
          v_time_load           TYPE ys4e_lu_time,
          v_cod_imovel          TYPE /agri/glstrno,
          v_talhao              TYPE /agri/glstrno,
          v_caixa_default       TYPE zabs_del_cxliq VALUE '1.5'.

    DATA: lt_retorno_load TYPE bapiret2_tab,
          lt_batches_load TYPE ys4st_batches_farm_trl.

    CONSTANTS: BEGIN OF c_caracteristica,
                 ticket          TYPE atnam VALUE 'ABS_CL_TICKET_COLHEITA',
                 safra           TYPE atnam VALUE 'RC_FR_ANO_SAFRA',
                 imovel          TYPE atnam VALUE 'RC_FR_COD_IM',
                 placa_cv        TYPE atnam VALUE 'RC_FR_PLACA_CR',
                 placa_cm        TYPE atnam VALUE 'RC_FR_PLACA_CM',
                 placa_mov_int   TYPE atnam VALUE 'ABS_PLACA_SERV_INT',
                 lote_logistico  TYPE atnam VALUE 'ABS_BATCH_LOG',
                 remessa         TYPE atnam VALUE 'REC_FRU_NUMERO_AR',
                 talhao          TYPE atnam VALUE 'RC_FR_TALH',
                 carregamento    TYPE atnam VALUE 'ABS_MECANIZADO',
                 movimentacao    TYPE atnam VALUE 'ABS_CL_MOVINT',
                 lider           TYPE atnam VALUE 'ABS_CL_LIDER',
                 turma           TYPE atnam VALUE 'ABS_CL_TURMA',
                 ordem_colheita  TYPE atnam VALUE 'ABS_CL_ORDEM',
                 inicio_lat      TYPE atnam VALUE 'ABS_MECANIZADO_ILAT',
                 inicio_long     TYPE atnam VALUE 'ABS_MECANIZADO_ILONG',
                 fim_lat         TYPE atnam VALUE 'ABS_MECANIZADO_FLAT',
                 fim_long        TYPE atnam VALUE 'ABS_MECANIZADO_FLONG',
                 caixas_liquidas TYPE atnam VALUE 'RC_FR_CAIX_LIQ',
                 caixas_refugo   TYPE atnam VALUE 'ABS_REF_DEVOL',
                 caixinhas       TYPE atnam VALUE 'ABS_CL_CAIXINHA_RATEIO',
                 lote_colheita   TYPE atnam VALUE 'ABS_CL_LOTE_COLHEITA',
                 carimbo         TYPE atnam VALUE 'ABS_CL_CARIMBO',
                 bin             TYPE atnam VALUE 'ABS_BIN',
               END OF c_caracteristica.

****Ticket Status
    CONSTANTS : BEGIN OF c_ticket_status,
                  closed    TYPE /agri/gleloek VALUE 'C',
                  inac_rvsl TYPE /agri/gleloek VALUE 'X',
                  saved     TYPE /agri/gleloek VALUE 'S',
                END OF c_ticket_status.

    CONSTANTS: BEGIN OF c_tipo_safra,
                 contabil TYPE zfmtpsafra VALUE 'C',
                 tecnica  TYPE zfmtpsafra VALUE 'T',
               END OF c_tipo_safra.

    CONSTANTS: BEGIN OF c_tipo_servico,
                 carreg_mecan TYPE ys4e_serv_type VALUE 'CM',
                 mov_interna  TYPE ys4e_serv_type VALUE 'TI',
               END OF c_tipo_servico.

    IF is_prhdr-status NE c_ticket_status-closed.
      wa_prhdr = is_prhdr.
      t_pritm[] = it_pritm[].

      SELECT *
        FROM zfmfpsafras
        INTO TABLE @DATA(lt_safras)
       WHERE tipo_safra = @c_tipo_safra-tecnica.

      SORT lt_safras BY ano_safra.

      SELECT tpclht
        INTO TABLE @DATA(lt_fruta_problema)
        FROM yotpclht
       WHERE problema EQ @abap_true.

      IF sy-subrc EQ 0.
        READ TABLE lt_fruta_problema INTO DATA(lwa_fruta_problema) INDEX 1.
        IF sy-subrc EQ 0.
          DATA(lv_problema_found) = abap_true.
        ELSE.
          lv_problema_found = abap_false.
        ENDIF.
      ENDIF.

*-- Verifica Fruta FCOJ
      CALL METHOD zcl_abs_get_variants=>get_constant_single
        EXPORTING
          iv_objid  = v_objid
          iv_k1val  = v_klval
        IMPORTING
          ev_cnval1 = v_fcoj.

      LOOP AT t_pritm INTO DATA(lwa_pritm).
        READ TABLE t_relevancia ASSIGNING FIELD-SYMBOL(<lwa_mesmo_lote>)
          WITH KEY charg = lwa_pritm-charg.
        IF sy-subrc NE 0.
          INSERT INITIAL LINE INTO TABLE t_relevancia
            ASSIGNING FIELD-SYMBOL(<lwa_relevancia>).
          IF sy-subrc EQ 0.
            <lwa_relevancia>-pritm     = lwa_pritm-pritm.
            <lwa_relevancia>-charg     = lwa_pritm-charg.
            <lwa_relevancia>-zzsboxq   = lwa_pritm-zzsboxq.
            <lwa_relevancia>-zzcarimbo = lwa_pritm-zzcarimbo.
            <lwa_relevancia>-counter   = 1.
            IF lv_problema_found EQ abap_true.
              UNPACK lwa_pritm-zzcarimbo TO v_carimbo_n.
              IF v_carimbo_n EQ lwa_fruta_problema-tpclht.
                <lwa_relevancia>-fruta_problema = abap_true.
              ENDIF.
            ENDIF.
          ENDIF.
        ELSE.
          IF lwa_pritm-zzsboxq GT <lwa_mesmo_lote>-zzsboxq.
            <lwa_mesmo_lote>-pritm   = lwa_pritm-pritm.
            <lwa_mesmo_lote>-zzsboxq = lwa_pritm-zzsboxq.
          ENDIF.
          IF lwa_pritm-zzcarimbo NE <lwa_mesmo_lote>-zzcarimbo.
            <lwa_mesmo_lote>-carimbos_dif = abap_true.
          ENDIF.
          IF lv_problema_found EQ abap_true.
            UNPACK lwa_pritm-zzcarimbo TO v_carimbo_n.
            IF v_carimbo_n EQ lwa_fruta_problema-tpclht.
              <lwa_mesmo_lote>-fruta_problema = abap_true.
              <lwa_mesmo_lote>-zzcarimbo = lwa_pritm-zzcarimbo.
            ENDIF.
          ENDIF.
          ADD 1 TO <lwa_mesmo_lote>-counter.
        ENDIF.
      ENDLOOP.

      SORT t_relevancia BY charg.

      LOOP AT t_pritm INTO lwa_pritm.
*-- Verifica relevância (Caixinhas)
        READ TABLE t_relevancia ASSIGNING <lwa_relevancia>
          WITH KEY charg = lwa_pritm-charg BINARY SEARCH.
        IF sy-subrc EQ 0.
          IF <lwa_relevancia>-updkz EQ abap_true.
            CONTINUE.
          ELSE.
            READ TABLE t_pritm INTO lwa_pritm
              WITH KEY pritm = <lwa_relevancia>-pritm.
            <lwa_relevancia>-updkz = abap_true.
          ENDIF.
        ENDIF.

        v_objectkey(18) = lwa_pritm-plnbez.
        v_objectkey+18(10) = lwa_pritm-charg.

        CONCATENATE lwa_pritm-plnbez lwa_pritm-charg
          INTO v_objectkey_old RESPECTING BLANKS.

*-- Recupera as características preenchidas do batch
        CALL FUNCTION 'VB_BATCH_GET_DETAIL'
          EXPORTING
            matnr              = lwa_pritm-plnbez
            charg              = lwa_pritm-charg
            get_classification = abap_true
          TABLES
            char_of_batch      = t_char_batch.

        SORT t_char_batch BY atnam.

        CALL FUNCTION 'BAPI_OBJCL_GETCLASSES'
          EXPORTING
            objecttable_imp    = 'MCH1'
            classtype_imp      = '023'
            read_valuations    = 'X'
            objectkey_imp_long = v_objectkey_old
          TABLES
            alloclist          = t_alloclist_old
            allocvalueschar    = t_allocvalueschar_old
            allocvaluescurr    = t_allocvaluescurr_old
            allocvaluesnum     = t_allocvaluesnum_old
            return             = t_messages_old.

        SORT: t_allocvalueschar_old BY charact,
              t_allocvaluescurr_old BY charact,
              t_allocvaluesnum_old BY charact.

*...Recupera todas as características possíveis do batch
        CALL FUNCTION 'VBWS_MARA_CLASSIFICATION_GET'
          EXPORTING
            i_matnr              = lwa_pritm-plnbez
          IMPORTING
            e_class              = v_classnum
          TABLES
            e_cl_char            = t_char
          EXCEPTIONS
            classtype_not_found  = 1
            classtype_not_active = 2
            class_not_found      = 3
            no_allocations       = 4
            characters_not_found = 5
            OTHERS               = 6.

        IF sy-subrc EQ 0.
          SORT t_char BY atnam.

          SELECT atnam,
                 atfor
            FROM cabn
            INTO TABLE @DATA(t_cabn)
            FOR ALL ENTRIES IN @t_char
            WHERE atinn = @t_char-atinn.
          IF sy-subrc = 0.
            SORT t_cabn BY atnam.
          ENDIF.

          LOOP AT t_char INTO DATA(wa_char).
            CLEAR v_fieldname.
            DATA(v_header_data) = abap_true.
            READ TABLE t_cabn INTO DATA(wa_cabn)
              WITH KEY atnam = wa_char-atnam BINARY SEARCH.
            IF sy-subrc NE 0.
              CONTINUE.
            ENDIF.
*--Verifica a existência de valores nos atributos
            DATA(lv_old_value) = abap_false.
            READ TABLE t_char_batch INTO DATA(lwa_char_batch)
              WITH KEY atnam = wa_char-atnam BINARY SEARCH.
            IF sy-subrc EQ 0.
              lv_old_value = abap_true.
              CASE wa_cabn-atfor.
                WHEN 'CHAR'.
                  READ TABLE t_allocvalueschar_old INTO DATA(lwa_char_old)
                    WITH KEY charact = wa_char-atnam BINARY SEARCH.
                  IF sy-subrc EQ 0.
                    wa_allocvalueschar-charact = wa_cabn-atnam.
                    wa_allocvalueschar-value_char = lwa_char_old-value_char.
                    APPEND wa_allocvalueschar TO t_allocvalueschar.
                  ENDIF.
                WHEN 'CURR'.
                  READ TABLE t_allocvaluescurr_old INTO DATA(lwa_curr_old)
                    WITH KEY charact = wa_char-atnam BINARY SEARCH.
                  IF sy-subrc EQ 0.
                    wa_allocvaluescurr-charact = wa_cabn-atnam.
                    wa_allocvaluescurr-value_from = lwa_curr_old-value_from.
                    APPEND wa_allocvaluescurr TO t_allocvaluescurr.
                  ENDIF.
                WHEN 'DATE'
                  OR 'TIME'
                  OR 'NUM'.
                  READ TABLE t_allocvaluesnum_old INTO DATA(lwa_num_old)
                    WITH KEY charact = wa_char-atnam BINARY SEARCH.
                  IF sy-subrc EQ 0.
                    wa_allocvaluesnum-charact = wa_cabn-atnam.
                    wa_allocvaluesnum-value_from = lwa_num_old-value_from.
                    APPEND wa_allocvaluesnum TO t_allocvaluesnum.
                  ENDIF.
              ENDCASE.
              CLEAR: wa_allocvalueschar, wa_allocvaluescurr, wa_allocvaluesnum.
            ENDIF.

            IF lv_old_value EQ abap_true.
              CONTINUE.
            ENDIF.

            CASE wa_char-atnam.
              WHEN c_caracteristica-ticket.
                v_header_data = abap_true.
                v_fieldname = 'PRNUM'.
              WHEN c_caracteristica-safra.
                v_header_data = abap_true.
                v_fieldname = 'BUDAT'.
              WHEN c_caracteristica-imovel.
                v_header_data = abap_false.
                v_fieldname = 'TPLNR'.
              WHEN c_caracteristica-placa_cv.
                v_header_data = abap_false.
                v_fieldname = 'ZZSREBO1'.
              WHEN c_caracteristica-placa_cm.
                v_header_data = abap_true.
                v_fieldname = 'LIC_PLATE'.
              WHEN c_caracteristica-remessa.
                v_header_data = abap_true.
                v_fieldname = 'ZREMS'.
              WHEN c_caracteristica-talhao.
                v_header_data = abap_false.
                v_fieldname = 'TPLNR'.
              WHEN c_caracteristica-carimbo.
                v_header_data = abap_false.
                v_fieldname = 'ZZCARIMBO'.
              WHEN OTHERS.
                CONTINUE.
            ENDCASE.

            IF v_header_data EQ abap_true.
              ASSIGN COMPONENT v_fieldname OF STRUCTURE wa_prhdr
                TO FIELD-SYMBOL(<lv_fieldvalue>).
              IF sy-subrc NE 0.
                CONTINUE.
              ENDIF.
            ELSE.
              ASSIGN COMPONENT v_fieldname OF STRUCTURE lwa_pritm
                TO <lv_fieldvalue>.
              IF sy-subrc NE 0.
                CONTINUE.
              ENDIF.
            ENDIF.

            IF wa_char-atnam EQ c_caracteristica-safra.
              DATA(lv_season_found) = abap_false.
              LOOP AT lt_safras INTO DATA(lwa_safra).
                IF <lv_fieldvalue> BETWEEN lwa_safra-inicio_safra
                                       AND lwa_safra-fim_safra.
                  lv_season_found = abap_true.
                  EXIT.
                ENDIF.
              ENDLOOP.
              IF lv_season_found EQ abap_false.
                CLEAR lwa_safra.
              ENDIF.
            ELSEIF wa_char-atnam EQ c_caracteristica-imovel.
              DATA(lv_strlen) = strlen( <lv_fieldvalue> ).
              IF lv_strlen GE 6.
                CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
                  EXPORTING
                    input  = <lv_fieldvalue>
                  IMPORTING
                    output = v_cod_imovel.
                v_cod_imovel = v_cod_imovel+0(6).
              ENDIF.
            ELSEIF wa_char-atnam EQ c_caracteristica-talhao.
              lv_strlen = strlen( <lv_fieldvalue> ).
              IF lv_strlen GE 12.
                CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
                  EXPORTING
                    input  = <lv_fieldvalue>
                  IMPORTING
                    output = v_talhao.
                v_talhao = v_talhao+7(5).
              ENDIF.
            ELSEIF wa_char-atnam EQ c_caracteristica-carimbo.
              CLEAR v_carimbo_c.
              READ TABLE t_relevancia ASSIGNING <lwa_relevancia>
                WITH KEY charg = lwa_pritm-charg BINARY SEARCH.
              IF sy-subrc EQ 0.
                IF <lwa_relevancia>-carimbos_dif EQ abap_false.
                  v_carimbo_c = <lwa_relevancia>-zzcarimbo.
                ELSEIF <lwa_relevancia>-carimbos_dif EQ abap_true.
                  IF <lwa_relevancia>-fruta_problema EQ abap_true.
                    v_carimbo_c = <lwa_relevancia>-zzcarimbo.
                  ELSE.
                    IF v_fcoj IS NOT INITIAL.
                      UNPACK v_fcoj TO v_carimbo_c.
                    ELSE.
                      v_carimbo_c = '00023'.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.

            CASE wa_cabn-atfor.
              WHEN 'CHAR'.
                wa_allocvalueschar-charact = wa_cabn-atnam.
                IF wa_char-atnam EQ c_caracteristica-placa_cm
                OR wa_char-atnam EQ c_caracteristica-placa_cv.
                  TRANSLATE <lv_fieldvalue> USING '- '.
                  CONDENSE <lv_fieldvalue> NO-GAPS.
                  wa_allocvalueschar-value_char = <lv_fieldvalue>.
                ELSEIF wa_char-atnam EQ c_caracteristica-imovel.
                  wa_allocvalueschar-value_char = v_cod_imovel.
                ELSEIF wa_char-atnam EQ c_caracteristica-talhao.
                  wa_allocvalueschar-value_char = v_talhao.
                ELSEIF wa_char-atnam EQ c_caracteristica-bin.
                  wa_allocvalueschar-value_char = <lv_fieldvalue>.
                ELSE.
                  wa_allocvalueschar-value_char = <lv_fieldvalue>.
                ENDIF.
                APPEND wa_allocvalueschar TO t_allocvalueschar.
              WHEN 'CURR'.
                wa_allocvaluescurr-charact = wa_cabn-atnam.
                wa_allocvaluescurr-value_from = <lv_fieldvalue>.
                APPEND wa_allocvaluescurr TO t_allocvaluescurr.
              WHEN 'DATE'.
                CALL FUNCTION 'CTCV_CONVERT_DATE_TO_FLOAT'
                  EXPORTING
                    date  = <lv_fieldvalue>
                  IMPORTING
                    float = v_float.
                wa_allocvaluesnum-value_from = v_float.
                wa_allocvaluesnum-charact = wa_cabn-atnam.
                APPEND wa_allocvaluesnum TO t_allocvaluesnum.
              WHEN 'TIME'
                OR 'NUM '.
                wa_allocvaluesnum-value_from = <lv_fieldvalue>.
                wa_allocvaluesnum-charact = wa_cabn-atnam.
                IF wa_char-atnam EQ c_caracteristica-safra.
                  wa_allocvaluesnum-value_from = lwa_safra-ano_safra.
                ELSEIF wa_char-atnam EQ c_caracteristica-carimbo.
                  wa_allocvaluesnum-value_from = v_carimbo_c.
                ELSE.
                  wa_allocvaluesnum-value_from = <lv_fieldvalue>.
                ENDIF.
                APPEND wa_allocvaluesnum TO t_allocvaluesnum.
              WHEN OTHERS.
                wa_allocvaluesnum-value_from = <lv_fieldvalue>.
                wa_allocvaluesnum-charact = wa_cabn-atnam.
                IF wa_char-atnam EQ c_caracteristica-safra.
                  wa_allocvaluesnum-value_from = lwa_safra-ano_safra.
                ELSEIF wa_char-atnam EQ c_caracteristica-imovel.
                  wa_allocvaluesnum-value_from = v_cod_imovel.
                ELSEIF wa_char-atnam EQ c_caracteristica-talhao.
                  wa_allocvaluesnum-value_from = v_talhao.
                ELSE.
                  wa_allocvaluesnum-value_from = <lv_fieldvalue>.
                ENDIF.
                APPEND wa_allocvaluesnum TO t_allocvaluesnum.
            ENDCASE.

            CLEAR: wa_allocvalueschar, wa_allocvaluescurr, wa_allocvaluesnum.
          ENDLOOP.
        ENDIF.

        CALL FUNCTION 'BAPI_OBJCL_CHANGE'
          EXPORTING
            objectkey          = v_objectkey
            objecttable        = v_objecttable
            classnum           = v_classnum
            classtype          = v_classtype
          IMPORTING
            classif_status     = v_classif_status
          TABLES
            allocvaluesnumnew  = t_allocvaluesnum
            allocvaluescharnew = t_allocvalueschar
            allocvaluescurrnew = t_allocvaluescurr
            return             = t_return.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

        MOVE : v_objectkey(18)    TO wa_ymcha-matnr,
               v_objectkey+18(10) TO wa_ymcha-charg,
               sy-uname           TO wa_ymcha-aenam,
               sy-datum           TO wa_ymcha-laeda,
               'X'                TO wa_yupdmcha-aenam,
               'X'                TO wa_yupdmcha-laeda.

*.......Grava log de modificação
        CALL FUNCTION 'VB_CHANGE_BATCH'
          EXPORTING
            ymcha                       = wa_ymcha
            yupdmcha                    = wa_yupdmcha
          EXCEPTIONS
            no_material                 = 1
            no_batch                    = 2
            no_plant                    = 3
            material_not_found          = 4
            plant_not_found             = 5
            lock_on_material            = 6
            lock_on_plant               = 7
            lock_on_batch               = 8
            lock_system_error           = 9
            no_authority                = 10
            batch_not_exist             = 11
            no_class                    = 12
            error_in_classification     = 13
            error_in_valuation_change   = 14
            error_in_status_change      = 15
            region_of_origin_not_found  = 16
            country_of_origin_not_found = 17
            vendor_error                = 18
            OTHERS                      = 19.
      ENDLOOP.
    ELSEIF is_prhdr-status EQ c_ticket_status-closed.
      SELECT * FROM /agri/fmpritm
        INTO TABLE @DATA(lt_pritm_db)
       WHERE prnum EQ @is_prhdr-prnum
         AND gjahr EQ @is_prhdr-gjahr.

      LOOP AT lt_pritm_db INTO DATA(lwa_pritm_db).
        ASSIGN COMPONENT 'ZZLLIFNR' OF STRUCTURE lwa_pritm_db TO FIELD-SYMBOL(<lv_carreg_mecan>).
        IF sy-subrc EQ 0
        AND <lv_carreg_mecan> IS NOT INITIAL.
          INSERT INITIAL LINE INTO TABLE t_service_type ASSIGNING FIELD-SYMBOL(<lwa_service_type>).
          IF sy-subrc EQ 0.
            <lwa_service_type>-serv_type = c_tipo_servico-carreg_mecan.
            <lwa_service_type>-serv_provider = <lv_carreg_mecan>.
            <lwa_service_type>-serv_qty = lwa_pritm_db-zzcxliq.
            <lwa_service_type>-serv_qty_uom = 'CX'.
            <lwa_service_type>-serv_date_ini = is_prhdr-budat.
            <lwa_service_type>-serv_time_ini = sy-uzeit.
            <lwa_service_type>-serv_date_end = is_prhdr-budat.
            <lwa_service_type>-serv_time_end = <lwa_service_type>-serv_time_ini.
          ENDIF.
        ENDIF.

        IF lwa_pritm_db-zztransbordo EQ abap_false.
          ASSIGN COMPONENT 'ZZTLIFNR' OF STRUCTURE lwa_pritm_db TO FIELD-SYMBOL(<lv_mov_interna>).
          IF sy-subrc EQ 0
          AND <lv_mov_interna> IS NOT INITIAL.
            INSERT INITIAL LINE INTO TABLE t_service_type ASSIGNING <lwa_service_type>.
            IF sy-subrc EQ 0.
              <lwa_service_type>-serv_type = c_tipo_servico-mov_interna.
              <lwa_service_type>-serv_provider = <lv_mov_interna>.
              <lwa_service_type>-serv_qty = lwa_pritm_db-zzcxliq.
              <lwa_service_type>-serv_qty_uom = 'CX'.
              <lwa_service_type>-serv_date_ini = is_prhdr-budat.
              <lwa_service_type>-serv_time_ini = sy-uzeit.
              <lwa_service_type>-serv_date_end = is_prhdr-budat.
              <lwa_service_type>-serv_time_end = <lwa_service_type>-serv_time_ini.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

      IF t_service_type[] IS NOT INITIAL
      AND is_prhdr-fruta_mercado EQ abap_false.
        v_documento = is_prhdr-prnum.
        v_planta  = is_prhdr-werks.

        v_fmname = 'Y_S4TM_YARD_VISTEX_OS'.
        CALL FUNCTION 'FUNCTION_EXISTS'
          EXPORTING
            funcname           = v_fmname
          EXCEPTIONS
            function_not_exist = 1
            OTHERS             = 2.

        IF sy-subrc EQ 0.
          CALL FUNCTION 'Y_S4TM_YARD_VISTEX_OS'
            EXPORTING
              i_tor_id            = v_documento
              i_farm_id           = v_planta
            TABLES
              imp_tpserv_provider = t_service_type[]
              ext_return          = t_service_return[].
        ENDIF.
      ENDIF.
    ENDIF.

    IF is_prhdr-status NE c_ticket_status-closed
    AND is_prhdr-fruta_mercado EQ abap_false.
      v_tor_load   = is_prhdr-prnum.
      v_farm_load  = is_prhdr-werks.
      v_date_load  = sy-datum.
      v_time_load  = sy-uzeit.
      DATA(lt_pritm) = it_pritm[].
      DELETE lt_pritm WHERE charg IS INITIAL.
      LOOP AT lt_pritm INTO lwa_pritm.
        INSERT INITIAL LINE INTO TABLE lt_batches_load
          ASSIGNING FIELD-SYMBOL(<lwa_batch>).
        IF sy-subrc EQ 0.
          <lwa_batch>-batch_number = lwa_pritm-charg.
          <lwa_batch>-matnr = lwa_pritm-plnbez.
          <lwa_batch>-lic_plate = lwa_pritm-zzsrebo1.
          <lwa_batch>-uom = lwa_pritm-uomunit.
        ENDIF.
      ENDLOOP.

      SORT lt_batches_load BY batch_number.
      DELETE ADJACENT DUPLICATES FROM lt_batches_load COMPARING batch_number.

      DATA(lv_exec) = abap_false.
*Peso Bruto da Fazenda
      IF is_prhdr-zfmprfgw IS NOT INITIAL.
        v_event_load = 'LE'.
        lv_exec = abap_true.
      ELSE.
*Peso Tara da Fazenda
        IF is_prhdr-zfmprftw IS NOT INITIAL.
          v_event_load = 'LB'.
          lv_exec = abap_true.
        ELSE.
          IF lt_batches_load[] IS NOT INITIAL.
            DO 2 TIMES.
              IF sy-index EQ 1.
                v_event_load = 'LB'.
              ELSEIF sy-index EQ 2.
                v_event_load = 'LE'.
              ENDIF.

              CALL FUNCTION 'Y_S4TM_YARD_VISTEX_LOADING'
                EXPORTING
                  i_tor_id        = v_tor_load
                  i_farm_id       = v_farm_load
                  i_event_id      = v_event_load
                  i_date          = v_date_load
                  i_time          = v_time_load
                TABLES
                  ext_bapiret2    = lt_retorno_load
                  imp_batches_trl = lt_batches_load.
            ENDDO.
          ENDIF.
        ENDIF.
      ENDIF.

      IF v_event_load IS NOT INITIAL
      AND lv_exec EQ abap_true.
        CALL FUNCTION 'Y_S4TM_YARD_VISTEX_LOADING'
          EXPORTING
            i_tor_id        = v_tor_load
            i_farm_id       = v_farm_load
            i_event_id      = v_event_load
            i_date          = v_date_load
            i_time          = v_time_load
          TABLES
            ext_bapiret2    = lt_retorno_load
            imp_batches_trl = lt_batches_load.
      ENDIF.

      IF is_prhdr-zfmprfgw IS NOT INITIAL.
        v_direction = 'B'.
      ELSE.
        IF is_prhdr-zfmprftw IS NOT INITIAL.
          v_direction = 'T'.
        ENDIF.
      ENDIF.

      IF v_direction IS NOT INITIAL.
        v_order_id = is_prhdr-prnum.
        v_farm_id = is_prhdr-werks.
        IF is_prhdr-zfmprfgw IS NOT INITIAL.
          v_peso_total = is_prhdr-zfmprfgw.
        ELSE.
          v_peso_total = is_prhdr-zfmprftw.
        ENDIF.
*        v_un_medida = is_prhdr-zfmprftw_un.
        v_un_medida = 'KG'.
        v_date = sy-datum.
        v_time = sy-uzeit.

        v_fmname = 'Y_S4TM_YARD_VISTEX_BALANCA'.
        CALL FUNCTION 'FUNCTION_EXISTS'
          EXPORTING
            funcname           = v_fmname
          EXCEPTIONS
            function_not_exist = 1
            OTHERS             = 2.

        IF sy-subrc EQ 0.
          CALL FUNCTION 'Y_S4TM_YARD_VISTEX_BALANCA'
            EXPORTING
              i_tor_id             = v_order_id
              i_farm_id            = v_farm_id
              i_bal_type_direction = v_direction
              i_val_peso_total     = v_peso_total
              i_un_medida          = v_un_medida
              i_date               = v_date
              i_time               = v_time
            IMPORTING
              e_tara_pln           = v_peso_tara
              e_tara_pln_uom       = v_um_tara
              e_bruto_pln          = v_peso_bruto
              e_bruto_pln_uom      = v_um_bruto
            TABLES
              ext_bapiret2         = t_retorno_balanca.
        ENDIF.
      ENDIF.
    ENDIF.

    lt_stack = cl_abap_get_call_stack=>get_call_stack( ).
    lt_formatted_stack = cl_abap_get_call_stack=>format_call_stack_with_struct( lt_stack ).

    DATA(lv_monitor) = abap_false.
    READ TABLE lt_formatted_stack TRANSPORTING NO FIELDS
      WITH KEY kind        = 'FUNCTION'
               progname    = 'SAPLZFMNTX_RECEIPT'
               includename = 'LZFMNTX_RECEIPTU01'
               event       = 'ZFMNTX_PRODUCE_RECEIPT_QTDE'.
    IF sy-subrc EQ 0.
      lv_monitor = abap_true.
    ENDIF.

    IF lv_monitor EQ abap_false.
      CALL FUNCTION 'ZABS_FM_MEMORY_MSGS'
        IMPORTING
          et_messages = t_msgs[].

      IF t_msgs[] IS NOT INITIAL.
        CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
          TABLES
            i_message_tab = t_msgs[].
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD /agri/if_ex_badi_fmpr_all~authority_check.



  ENDMETHOD.


METHOD /agri/if_ex_badi_fmpr_all~before_save.

  DATA: lt_alloclist       TYPE TABLE OF bapi1003_alloc_list,
        lt_allocvalueschar TYPE tt_bapi1003_alloc_values_char,
        lt_allocvaluescurr TYPE tt_bapi1003_alloc_values_curr,
        lt_allocvaluesnum  TYPE tt_bapi1003_alloc_values_num,
        lt_char            TYPE STANDARD TABLE OF api_char,
        lt_messages        TYPE bapiret2_tt,
        lt_collect         LIKE ct_pritm,
        ls_collect         LIKE LINE OF lt_collect,
        lref_date          TYPE REF TO data,
        lv_objectkey       TYPE bapi1003_key-object_long,
        lv_objectkey2      TYPE bapi1003_key-object,
        lv_objecttable2    TYPE bapi1003_key-objecttable VALUE 'MCH1',
        lv_classnum2       TYPE bapi1003_key-classnum VALUE 'Z_FRUTA_MP',
        lv_classtype2      TYPE bapi1003_key-classtype VALUE '023',
        lv_classif_status2 TYPE clstatus.

  FIELD-SYMBOLS: <lv_date> TYPE any.

  IF cs_prhdr-zzmobile EQ 'N'
  OR cs_prhdr-zzmobile EQ 'Q'.
    WAIT UP TO 4 SECONDS.
    CALL FUNCTION 'ZABS_FM_UPDATE_WGHBRIDGE'
      IN UPDATE TASK
      EXPORTING
        is_prhdr = cs_prhdr
        it_pritm = ct_pritm.

    cs_prhdr-zzmobile = 'U'.
  ENDIF.

*--BOC- T_T.KONNO-05.26.21
  IF ( ( i_updkz EQ 'I' OR
         i_updkz EQ 'U' )
  AND  (  cs_prhdr-prnum BETWEEN '9000000000'
                             AND '9999999999' ) ).
    LOOP AT ct_pritm INTO DATA(ls_pritm).
      IF sy-tabix EQ 1.
*-- Recupera todas as características possíveis do lote
        CALL FUNCTION 'VBWS_MARA_CLASSIFICATION_GET'
          EXPORTING
            i_matnr              = ls_pritm-plnbez
          IMPORTING
            e_class              = lv_classnum2
          TABLES
            e_cl_char            = lt_char
          EXCEPTIONS
            classtype_not_found  = 1
            classtype_not_active = 2
            class_not_found      = 3
            no_allocations       = 4
            characters_not_found = 5
            OTHERS               = 6.

        IF sy-subrc EQ 0.
          SORT lt_char BY atnam.

          SELECT atnam, atfor, anzst, anzdz
            FROM cabn
            INTO TABLE @DATA(lt_cabn)
            FOR ALL ENTRIES IN @lt_char
           WHERE atinn = @lt_char-atinn.
          IF sy-subrc EQ 0.
            SORT lt_cabn BY atnam.
          ENDIF.
        ENDIF.
      ENDIF.

      ls_collect-charg = ls_pritm-charg.
      ls_collect-zzcxliq = ls_pritm-zzcxliq.
      COLLECT ls_collect INTO lt_collect.
    ENDLOOP.

    DELETE lt_collect WHERE zzcxliq IS INITIAL
                         OR charg IS INITIAL.

    LOOP AT lt_collect INTO ls_collect.
      REFRESH: lt_alloclist, lt_allocvalueschar,
               lt_allocvaluescurr, lt_allocvaluesnum,
               lt_messages, lt_char.

      CLEAR: lv_objectkey, lv_classif_status2.

      READ TABLE ct_pritm INTO ls_pritm
        WITH KEY charg = ls_collect-charg.
      IF sy-subrc EQ 0
      AND ls_pritm-plnbez IS NOT INITIAL.
        CONCATENATE ls_pritm-plnbez ls_pritm-charg
          INTO lv_objectkey RESPECTING BLANKS.

        CALL FUNCTION 'BAPI_OBJCL_GETCLASSES'
          EXPORTING
            objecttable_imp    = 'MCH1'
            classtype_imp      = '023'
            read_valuations    = 'X'
            objectkey_imp_long = lv_objectkey
          TABLES
            alloclist          = lt_alloclist
            allocvalueschar    = lt_allocvalueschar
            allocvaluescurr    = lt_allocvaluescurr
            allocvaluesnum     = lt_allocvaluesnum
            return             = lt_messages.

        lv_objectkey2(18) = ls_pritm-plnbez.
        lv_objectkey2+18(10) = ls_pritm-charg.

        READ TABLE lt_cabn INTO DATA(ls_cabn)
          WITH KEY atnam = 'RC_FR_CAIX_LIQ' BINARY SEARCH.
        IF sy-subrc EQ 0.
          CASE ls_cabn-atfor.
            WHEN 'CHAR'. "lt_allocvalueschar
            WHEN 'CURR'. "lt_allocvaluescurr
            WHEN 'DATE'. "lt_allocvaluesnum
            WHEN 'TIME' "lt_allocvaluesnum
              OR 'NUM'.
              READ TABLE lt_allocvaluesnum
                ASSIGNING FIELD-SYMBOL(<ls_allocvaluesnum>)
                WITH KEY charact = 'RC_FR_CAIX_LIQ'.
              IF sy-subrc NE 0.
                INSERT INITIAL LINE INTO TABLE lt_allocvaluesnum
                  ASSIGNING <ls_allocvaluesnum>.
              ENDIF.
              IF sy-subrc EQ 0.
                CREATE DATA lref_date TYPE p LENGTH ls_cabn-anzst DECIMALS ls_cabn-anzdz.
                ASSIGN lref_date->* TO <lv_date>.
                <lv_date> = ls_collect-zzcxliq.
                <ls_allocvaluesnum>-value_from = <lv_date>.
                <ls_allocvaluesnum>-charact = 'RC_FR_CAIX_LIQ'.
              ENDIF.
          ENDCASE.
        ENDIF.

        CALL FUNCTION 'BAPI_OBJCL_CHANGE'
          EXPORTING
            objectkey          = lv_objectkey2
            objecttable        = lv_objecttable2
            classnum           = lv_classnum2
            classtype          = lv_classtype2
          IMPORTING
            classif_status     = lv_classif_status2
          TABLES
            allocvaluesnumnew  = lt_allocvaluesnum
            allocvaluescharnew = lt_allocvalueschar
            allocvaluescurrnew = lt_allocvaluescurr
            return             = lt_messages.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
      ENDIF.
    ENDLOOP.
  ENDIF.
*--EOC- T_T.KONNO-05.26.21

ENDMETHOD.


  METHOD /agri/if_ex_badi_fmpr_all~document_check.

    DATA: lt_header     TYPE STANDARD TABLE OF bapi_order_header1 INITIAL SIZE 0,
          lt_position   TYPE STANDARD TABLE OF bapi_order_item INITIAL SIZE 0,
          lt_sequence   TYPE STANDARD TABLE OF bapi_order_sequence INITIAL SIZE 0,
          lt_operation  TYPE STANDARD TABLE OF bapi_order_operation1 INITIAL SIZE 0,
          lr_lifnr      TYPE RANGE OF lifnr,
          lr_atnam      TYPE RANGE OF atnam,
          lr_bin        TYPE RANGE OF zabs_del_bin,
          lwa_return    TYPE bapiret2,
          lwa_ordobjs   TYPE bapi_pp_order_objects,
          lv_rateio_max TYPE zabs_del_sboxq VALUE 1000,
          lv_terreno    TYPE /agri/gltplnr_fl.

    DATA(lt_pritm) = it_pritm[].
    DELETE lt_pritm WHERE zzturma IS NOT INITIAL.
    IF lt_pritm[] IS NOT INITIAL.
      INSERT INITIAL LINE INTO TABLE ct_messages
        ASSIGNING FIELD-SYMBOL(<lwa_message>).
      IF sy-subrc EQ 0.
*-- O Código da Turma é de preenchimento obrigatório!
        <lwa_message>-msgid = 'ZFMFP'.
        <lwa_message>-msgno = '280'.
        <lwa_message>-msgty = 'E'.
        c_stop_save = abap_true.
      ENDIF.
    ENDIF.

    lt_pritm[] = it_pritm[].
    DELETE lt_pritm WHERE plnbez IS INITIAL.
    SORT lt_pritm BY plnbez.
    DELETE ADJACENT DUPLICATES FROM lt_pritm COMPARING plnbez.
    IF lines( lt_pritm ) GT 1.
      INSERT INITIAL LINE INTO TABLE ct_messages
        ASSIGNING <lwa_message>.
      IF sy-subrc EQ 0.
*-- Ticket com diversas variedades não permitido!
        <lwa_message>-msgid = 'ZFMFP'.
        <lwa_message>-msgno = '296'.
        <lwa_message>-msgty = 'E'.
        c_stop_save = abap_true.
      ENDIF.
    ENDIF.

*-- Validar Preenchimento Caixas Rateio
    lt_pritm[] = it_pritm[].
    DELETE lt_pritm WHERE zzsboxq IS NOT INITIAL.
    IF lt_pritm[] IS NOT INITIAL.
      INSERT INITIAL LINE INTO TABLE ct_messages
        ASSIGNING <lwa_message>.
      IF sy-subrc EQ 0.
*-- Coluna 'Caixas Rateio' deve ser preenchida para todos os itens!
        <lwa_message>-msgid = 'ZFMFP'.
        <lwa_message>-msgno = '267'.
        <lwa_message>-msgty = 'E'.
        c_stop_save = abap_true.
      ENDIF.
    ELSE.
      LOOP AT it_pritm INTO DATA(ls_pritm).
        IF ls_pritm-zzsboxq GT lv_rateio_max.
          INSERT INITIAL LINE INTO TABLE ct_messages ASSIGNING <lwa_message>.
          IF sy-subrc EQ 0.
*-- Valor máximo permitido para Caixas Rateio é 1000.
            <lwa_message>-msgid = 'ZFMFP'.
            <lwa_message>-msgno = '279'.
            <lwa_message>-msgty = 'E'.
            c_stop_save = abap_true.
            EXIT.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

*-- Validação centro produtivo Ticket x Imóvel itens
    lt_pritm[] = it_pritm[].
    DELETE lt_pritm WHERE tplnr IS INITIAL.
    IF lt_pritm[] IS NOT INITIAL
    AND is_prhdr-werks IS NOT INITIAL.
      SELECT tplnr_fl, iwerk, swerk
        FROM /agri/glflot
        INTO TABLE @DATA(lt_glflot)
        FOR ALL ENTRIES IN @lt_pritm
       WHERE tplnr_fl EQ @lt_pritm-tplnr.

      SORT lt_glflot BY tplnr_fl.
      LOOP AT lt_pritm INTO ls_pritm.
        READ TABLE lt_glflot INTO DATA(ls_glflot)
          WITH KEY tplnr_fl = ls_pritm-tplnr BINARY SEARCH.
        IF sy-subrc EQ 0.
          IF ls_glflot-swerk NE is_prhdr-werks.
            c_stop_save = abap_true.
            INSERT INITIAL LINE INTO TABLE ct_messages
              ASSIGNING <lwa_message>.
            IF sy-subrc EQ 0.
              CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
                EXPORTING
                  input  = ls_pritm-tplnr
                IMPORTING
                  output = lv_terreno.

*-- Talhão &1 não pertence a fazenda dessa viagem.
              <lwa_message>-msgid = 'ZFMFP'.
              <lwa_message>-msgno = '268'.
              <lwa_message>-msgty = 'E'.
              <lwa_message>-msgv1 = lv_terreno.
              c_stop_save = abap_true.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

*-- Talhão não permitido para colheita (Ordem Pátio).
    lt_pritm[] = it_pritm[].
    DELETE lt_pritm WHERE aufnr IS NOT INITIAL.
    IF lt_pritm[] IS NOT INITIAL.
      INSERT INITIAL LINE INTO TABLE ct_messages
        ASSIGNING <lwa_message>.
      IF sy-subrc EQ 0.
*-- Talhão não permitido para colheita (Ordem Pátio).
        <lwa_message>-msgid = 'ZFMFP'.
        <lwa_message>-msgno = '78'.
        <lwa_message>-msgty = 'E'.
        c_stop_save = abap_true.
      ENDIF.
    ELSE.
*-- Get operation details
      lwa_ordobjs-header     = abap_true.
      lwa_ordobjs-positions  = abap_true.
      lwa_ordobjs-sequences  = abap_true.
      lwa_ordobjs-operations = abap_true.

      LOOP AT it_pritm INTO DATA(lwa_pritm).
        IF lwa_pritm-zzcxliq IS INITIAL.
          CONTINUE.
        ENDIF.

        IF lwa_pritm-aufnr IS NOT INITIAL.
          DATA(lv_check_aval) = abap_true.
          SELECT uebtk UP TO 1 ROWS
            FROM afpo
            INTO @DATA(lv_uebtk)
           WHERE aufnr = @lwa_pritm-aufnr.
          ENDSELECT.

          IF sy-subrc EQ 0
          AND lv_uebtk EQ abap_true.
            lv_check_aval = abap_false.
          ENDIF.

          REFRESH: lt_header, lt_position, lt_sequence, lt_operation.
          CLEAR lwa_return.
          CALL FUNCTION 'BAPI_PRODORD_GET_DETAIL'
            EXPORTING
              number        = lwa_pritm-aufnr
              order_objects = lwa_ordobjs
            IMPORTING
              return        = lwa_return
            TABLES
              header        = lt_header
              position      = lt_position
              sequence      = lt_sequence
              operation     = lt_operation.

          IF lv_check_aval EQ abap_true.
            READ TABLE lt_position INTO DATA(lwa_position) INDEX 1.
            IF sy-subrc EQ 0.
              DATA(lv_available) = lwa_position-quantity - lwa_position-delivered_quantity.
              IF lwa_pritm-zzcxliq GT lv_available.
                INSERT INITIAL LINE INTO TABLE ct_messages ASSIGNING <lwa_message>.
                IF sy-subrc EQ 0.
*-- Saldo insuficiente na Ordem Pátio &1!
                  <lwa_message>-msgid = 'ZFMFP'.
                  <lwa_message>-msgno = '79'.
                  <lwa_message>-msgty = 'E'.
                  <lwa_message>-msgv1 = lwa_pritm-aufnr.
                  c_stop_save = abap_true.
                  EXIT.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    lt_pritm[] = it_pritm[].

    IF is_prhdr-zzwerks_dest IS INITIAL
    AND is_prhdr-fruta_mercado EQ abap_false.
      INSERT INITIAL LINE INTO TABLE ct_messages ASSIGNING <lwa_message>.
      IF sy-subrc EQ 0.
*-- Fábrica de destino não informada. Favor verificar.
        <lwa_message>-msgid = 'ZFMFP'.
        <lwa_message>-msgno = '60'.
        <lwa_message>-msgty = 'E'.
        c_stop_save = abap_true.
      ENDIF.
    ENDIF.

    INSERT INITIAL LINE INTO TABLE lr_atnam ASSIGNING FIELD-SYMBOL(<lwa_atnam>).
    IF sy-subrc EQ 0.
      <lwa_atnam> = 'IEQ'.
      <lwa_atnam>-low = 'ABS_BIN'.
    ENDIF.

    SELECT atinn, atnam
      FROM cabn
      INTO TABLE @DATA(lt_cabn)
     WHERE atnam IN @lr_atnam.
    IF sy-subrc EQ 0.
      SELECT atinn, atwrt
        FROM cawn
        INTO TABLE @DATA(lt_cawn)
        FOR ALL ENTRIES IN @lt_cabn
       WHERE atinn EQ @lt_cabn-atinn.

      LOOP AT lt_cawn ASSIGNING FIELD-SYMBOL(<lwa_cawn>).
        INSERT INITIAL LINE INTO TABLE lr_bin
          ASSIGNING FIELD-SYMBOL(<lwa_bin>).
        IF sy-subrc EQ 0.
          <lwa_bin> = 'IEQ'.
          <lwa_bin>-low = <lwa_cawn>-atwrt(20).
        ENDIF.
      ENDLOOP.

      LOOP AT it_pritm INTO lwa_pritm.
        IF lwa_pritm-zzbin IS NOT INITIAL
        AND lwa_pritm-zzbin NOT IN lr_bin[].
          INSERT INITIAL LINE INTO TABLE ct_messages
            ASSIGNING <lwa_message>.
          IF sy-subrc EQ 0.
*-- Bin &1 inexistente. Favor verificar.
            <lwa_message>-msgid = 'ZFMFP'.
            <lwa_message>-msgno = '59'.
            <lwa_message>-msgty = 'E'.
            <lwa_message>-msgv1 = lwa_pritm-zzbin.
          ENDIF.
          c_stop_save = abap_true.
        ENDIF.

        IF lwa_pritm-zzcxliq LT 0.
          INSERT INITIAL LINE INTO TABLE ct_messages
            ASSIGNING <lwa_message>.
          IF sy-subrc EQ 0.
*-- Item &1. Não é permitido valor negativo &2 para Caixas Líquidas.
            <lwa_message>-msgid = 'ZFMFP'.
            <lwa_message>-msgno = '71'.
            <lwa_message>-msgty = 'E'.
            <lwa_message>-msgv1 = lwa_pritm-pritm.
            <lwa_message>-msgv2 = lwa_pritm-zzcxliq.
          ENDIF.
          c_stop_save = abap_true.
        ENDIF.

        IF lwa_pritm-zzcxref LT 0.
          INSERT INITIAL LINE INTO TABLE ct_messages
            ASSIGNING <lwa_message>.
          IF sy-subrc EQ 0.
*-- Item &1. Não é permitido valor negativo &2 para Caixas Refugo.
            <lwa_message>-msgid = 'ZFMFP'.
            <lwa_message>-msgno = '72'.
            <lwa_message>-msgty = 'E'.
            <lwa_message>-msgv1 = lwa_pritm-pritm.
            <lwa_message>-msgv2 = lwa_pritm-zzbin.
          ENDIF.
          c_stop_save = abap_true.
        ENDIF.
      ENDLOOP.
    ENDIF.

    DELETE lt_pritm WHERE zzllifnr IS INITIAL
                      AND zztlifnr IS INITIAL.

    LOOP AT lt_pritm ASSIGNING FIELD-SYMBOL(<lwa_pritm>).
      IF <lwa_pritm>-zzllifnr IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <lwa_pritm>-zzllifnr
          IMPORTING
            output = <lwa_pritm>-zzllifnr.
        INSERT INITIAL LINE INTO TABLE lr_lifnr
          ASSIGNING FIELD-SYMBOL(<lwa_lifnr>).
        IF sy-subrc EQ 0.
          <lwa_lifnr> = 'IEQ'.
          <lwa_lifnr>-low = <lwa_pritm>-zzllifnr.
        ENDIF.
      ENDIF.
      IF <lwa_pritm>-zztlifnr IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <lwa_pritm>-zztlifnr
          IMPORTING
            output = <lwa_pritm>-zztlifnr.
        INSERT INITIAL LINE INTO TABLE lr_lifnr
          ASSIGNING <lwa_lifnr>.
        IF sy-subrc EQ 0.
          <lwa_lifnr> = 'IEQ'.
          <lwa_lifnr>-low = <lwa_pritm>-zztlifnr.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF lr_lifnr[] IS NOT INITIAL.
      SELECT lifnr
        FROM lfa1
        INTO TABLE @DATA(lt_lfa1)
       WHERE lifnr IN @lr_lifnr[].

      SORT lt_lfa1 BY lifnr.

      LOOP AT lt_pritm ASSIGNING <lwa_pritm>.
        READ TABLE lt_lfa1 TRANSPORTING NO FIELDS
          WITH KEY lifnr = <lwa_pritm>-zzllifnr BINARY SEARCH.
        IF sy-subrc NE 0.
          READ TABLE it_pritm INTO lwa_pritm
            WITH KEY pritm = <lwa_pritm>-pritm.
          IF sy-subrc EQ 0.
            INSERT INITIAL LINE INTO TABLE ct_messages
              ASSIGNING <lwa_message>.
            IF sy-subrc EQ 0.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                EXPORTING
                  input  = <lwa_pritm>-zzllifnr
                IMPORTING
                  output = <lwa_pritm>-zzllifnr.
*-- Carregamento mecanizado &1 inexistente. Favor verificar.
              <lwa_message>-msgid = 'ZFMFP'.
              <lwa_message>-msgno = '56'.
              <lwa_message>-msgty = 'E'.
              <lwa_message>-msgv1 = <lwa_pritm>-zzllifnr.
            ENDIF.
            c_stop_save = abap_true.
          ENDIF.
        ENDIF.
        READ TABLE lt_lfa1 TRANSPORTING NO FIELDS
          WITH KEY lifnr = <lwa_pritm>-zztlifnr BINARY SEARCH.
        IF sy-subrc NE 0.
          READ TABLE it_pritm INTO lwa_pritm
            WITH KEY pritm = <lwa_pritm>-pritm.
          IF sy-subrc EQ 0.
            INSERT INITIAL LINE INTO TABLE ct_messages
              ASSIGNING <lwa_message>.
            IF sy-subrc EQ 0.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                EXPORTING
                  input  = <lwa_pritm>-zztlifnr
                IMPORTING
                  output = <lwa_pritm>-zztlifnr.
*-- Movimentação interna &1 inexistente. Favor verificar.
              <lwa_message>-msgid = 'ZFMFP'.
              <lwa_message>-msgno = '57'.
              <lwa_message>-msgty = 'E'.
              <lwa_message>-msgv1 = <lwa_pritm>-zztlifnr.
            ENDIF.
            c_stop_save = abap_true.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    lt_pritm[] = it_pritm[].

    DELETE lt_pritm WHERE zzhaufnr IS INITIAL.

    IF lt_pritm[] IS NOT INITIAL.
      LOOP AT lt_pritm ASSIGNING <lwa_pritm>.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <lwa_pritm>-zzhaufnr
          IMPORTING
            output = <lwa_pritm>-zzhaufnr.
      ENDLOOP.

      SELECT aufnr
        FROM aufk
        INTO TABLE @DATA(lt_aufk)
        FOR ALL ENTRIES IN @lt_pritm
       WHERE aufnr = @lt_pritm-zzhaufnr.

      SORT lt_aufk BY aufnr.

      LOOP AT lt_pritm ASSIGNING <lwa_pritm>.
        READ TABLE lt_aufk TRANSPORTING NO FIELDS
          WITH KEY aufnr = <lwa_pritm>-zzhaufnr BINARY SEARCH.
        IF sy-subrc NE 0.
          READ TABLE it_pritm INTO lwa_pritm
            WITH KEY pritm = <lwa_pritm>-pritm.

          IF sy-subrc EQ 0.
            INSERT INITIAL LINE INTO TABLE ct_messages
              ASSIGNING <lwa_message>.

            IF sy-subrc EQ 0.
*-- As modificações não foram gravadas
              <lwa_message>-msgid = 'ZFMFP'.
              <lwa_message>-msgno = '54'.
              <lwa_message>-msgty = 'E'.
            ENDIF.

            INSERT INITIAL LINE INTO TABLE ct_messages
              ASSIGNING <lwa_message>.
            IF sy-subrc EQ 0.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                EXPORTING
                  input  = <lwa_pritm>-zzhaufnr
                IMPORTING
                  output = <lwa_pritm>-zzhaufnr.
*-- Nº ordem * não existente em AUFK (verificar a entrada)
              <lwa_message>-msgid = 'ZFMFP'.
              <lwa_message>-msgno = '53'.
              <lwa_message>-msgty = 'E'.
              <lwa_message>-msgv1 = <lwa_pritm>-zzhaufnr.
            ENDIF.
            c_stop_save = abap_true.
          ENDIF.
          EXIT.
        ELSE.
          IF <lwa_pritm>-zzhbatch IS NOT INITIAL
          AND <lwa_pritm>-zzsrebo1 IS INITIAL.
            INSERT INITIAL LINE INTO TABLE ct_messages
              ASSIGNING <lwa_message>.
            IF sy-subrc EQ 0.
*-- As modificações não foram gravadas
              <lwa_message>-msgid = 'ZFMFP'.
              <lwa_message>-msgno = '54'.
              <lwa_message>-msgty = 'E'.
            ENDIF.

            INSERT INITIAL LINE INTO TABLE ct_messages
              ASSIGNING <lwa_message>.
            IF sy-subrc EQ 0.
*-- Item &1: Inserir Placa Semi-Reboque
              <lwa_message>-msgid = 'ZFMFP'.
              <lwa_message>-msgno = '75'.
              <lwa_message>-msgty = 'E'.
              <lwa_message>-msgv1 = <lwa_pritm>-pritm.
            ENDIF.
            c_stop_save = abap_true.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD /agri/if_ex_badi_fmpr_all~fieldinfo_get.



  ENDMETHOD.


  METHOD /agri/if_ex_badi_fmpr_all~field_catalog_prepare.



  ENDMETHOD.


  METHOD /agri/if_ex_badi_fmpr_all~get_batch_number.


  ENDMETHOD.


  METHOD /agri/if_ex_badi_fmpr_all~get_classification_values.

    DATA: lt_pritm        TYPE /agri/t_fmpritm,
          lwa_pritm       TYPE /agri/s_fmpritm,
          ls_prhdr        TYPE /agri/fmprhdr,
          lwa_allocvalues TYPE /agri/s_fmallocvalues.


* Get Ticket Item
    CALL METHOD /agri/cl_fm_master_ticket=>get_details
      EXPORTING
        i_prnum          = i_prnum
        i_gjahr          = i_gjahr
      IMPORTING
        es_prhdr         = ls_prhdr
        et_pritm         = lt_pritm
      EXCEPTIONS
        ticket_not_found = 1
        OTHERS           = 2.
    IF sy-subrc = 0.


      READ TABLE lt_pritm INTO lwa_pritm INDEX 1.
      IF sy-subrc = 0.
        lwa_allocvalues-atnam = 'ABS_CL_ORDEM'.
        lwa_allocvalues-atflv = lwa_pritm-zzhaufnr.
        APPEND lwa_allocvalues TO ct_allocvalues.

        lwa_allocvalues-atnam = 'ABS_CL_CARIMBO'.
        lwa_allocvalues-atflv = lwa_pritm-zzcarimbo.
        APPEND lwa_allocvalues TO ct_allocvalues.

        IF lwa_pritm-zzcarimbo IS NOT INITIAL.
          lwa_allocvalues-atnam = 'RC_FR_DESC_TIP_COLH'.
          SELECT SINGLE toclhtt FROM yotpclht INTO lwa_allocvalues-atwrt
                                 WHERE tpclht = lwa_pritm-zzcarimbo.
          APPEND lwa_allocvalues TO ct_allocvalues.
        ENDIF.

        lwa_allocvalues-atnam = 'ABS_CL_TICKET_COLHEITA'.
        lwa_allocvalues-atwrt = lwa_pritm-zzhbatch.
        APPEND lwa_allocvalues TO ct_allocvalues.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD /agri/if_ex_badi_fmpr_all~get_logo.



  ENDMETHOD.


  METHOD /agri/if_ex_badi_fmpr_all~get_text_descriptions.



  ENDMETHOD.


  METHOD /agri/if_ex_badi_fmpr_all~header_data_fill.



  ENDMETHOD.


  METHOD /agri/if_ex_badi_fmpr_all~header_defaults_fill.



  ENDMETHOD.


  METHOD /agri/if_ex_badi_fmpr_all~material_receiver_get.



  ENDMETHOD.


  METHOD /agri/if_ex_badi_fmpr_all~material_sub_product_get.



  ENDMETHOD.


  METHOD /agri/if_ex_badi_fmpr_all~weight_check.



  ENDMETHOD.


  METHOD /agri/if_ex_badi_fmpr_all~weight_format.



  ENDMETHOD.
ENDCLASS.
