*&---------------------------------------------------------------------*
*& Include ZFMFP_INC_FILL_ITEMS
*&---------------------------------------------------------------------*

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
               transbordo      TYPE atnam VALUE 'ABS_TRANSBORDO',
               data_colheita   TYPE atnam VALUE 'ABS_DATA_COLHEITA',
             END OF c_caracteristica.

  CONSTANTS: BEGIN OF c_tipo_safra,
               contabil TYPE zfmtpsafra VALUE 'C',
               tecnica  TYPE zfmtpsafra VALUE 'T',
             END OF c_tipo_safra.

  TYPES: BEGIN OF ty_qrcode,
           baggp             TYPE zabs_del_baggp,
           zzbagcn(6)        TYPE c,
           tot_boxes(6)      TYPE c,
           avg_cxn(16)       TYPE c,
           ibag_no           TYPE zabs_del_bagno,
           fbag_no           TYPE zabs_del_fbagno,
           ldcde             TYPE zabs_del_ldcde,
           zzturma           TYPE zfmturma_id,
           lider_turma       TYPE persno,
           tplnr             TYPE /agri/gltplnr_fl,
           tmatnr            TYPE /agri/gltmatnr,
           int_tr_prov       TYPE zabs_del_trprov,
           in_licplate       TYPE zabs_del_licplate,
           charg             TYPE charg_d,
           aufnr             TYPE aufnr,
           lat_frstbag(20)   TYPE c,
           long_frstbag(20)  TYPE c,
           lat_lst_bag(20)   TYPE c,
           long_last_bag(20) TYPE c,
           bcarimbo(5)       TYPE n,
           btoclhtt          TYPE yodesc,
           ymatnr            TYPE /agri/glymatnr,
           maktx             TYPE maktx,
           ersda             TYPE ersda,
           zztransbordo      TYPE zabs_del_transbordo,
         END OF ty_qrcode,

         BEGIN OF ty_carimbo,
           tplnr   TYPE /agri/gltplnr_fl,
           imov    TYPE yoimov,
           talhao  TYPE yotalhao,
           carimbo TYPE yotpclht_d,
           toclhtt TYPE yodesc,
         END OF ty_carimbo .

  FORM fill_items_citro USING lv_msgid  TYPE any
                              lv_msgtyp TYPE any
                              lv_msgnro TYPE any.

    DATA: lt_stack           TYPE cl_abap_get_call_stack=>call_stack_internal,
          lt_formatted_stack TYPE cl_abap_get_call_stack=>formatted_entry_stack,
          lt_batch_read      TYPE bapiret2_tab,
          lt_batch_create    TYPE bapiret2_tab,
          lt_ausp            TYPE tt_ausp,
          lt_charts          TYPE zt_fmpr_charact,
          lt_header          TYPE STANDARD TABLE OF bapi_order_header1 INITIAL SIZE 0,
          lt_position        TYPE STANDARD TABLE OF bapi_order_item INITIAL SIZE 0,
          lt_sequence        TYPE STANDARD TABLE OF bapi_order_sequence INITIAL SIZE 0,
          lt_operation       TYPE STANDARD TABLE OF bapi_order_operation1 INITIAL SIZE 0,
          lwa_ordobjs        TYPE bapi_pp_order_objects,
          lwa_return         TYPE bapiret2,
          lwa_pritm          TYPE /agri/s_fmpritm_fcat,
          lwa_pritm_a        TYPE /agri/s_fmpritm,
          lwa_afpod          TYPE afpod,
          lwa_caufvd         TYPE caufvd,
          lv_aufnr           TYPE caufvd-aufnr,
          lv_vbeln           TYPE vbeln_vl,
          lv_verurx          TYPE verur_vl,
          lv_batchx          TYPE bapibatchkey-batch,
          lv_werks_tox       TYPE bapibatchkey-plant,
          lv_lgortx          TYPE bapibatchstoloc-stge_loc,
          lv_matnrx          TYPE bapibatchkey-material,
          lv_matnr_check     TYPE matnr18,
          lv_matnr_out       TYPE matnr18,
          lv_batch_check     TYPE charg_d,
          lv_plant_check     TYPE bapibatchkey-plant,
          lv_batch_created   TYPE bapibatchkey-batch,
          lv_werks_fromx     TYPE bapibatchkey-plant,
          lv_lev1_terr       TYPE char6,
          lv_lev2_terr       TYPE numc5,
          lv_int4_field      TYPE int4,
          lv_matnr           TYPE matnr,
          lr_variedade       TYPE RANGE OF matnr,
          lv_terreno         TYPE /agri/gltplnr_fl,
          lv_date            TYPE sy-datum,
          lv_week            TYPE kweek,
          lv_werks           TYPE werks_d,
          lv_variedade       TYPE matnr,
          lv_imovel          TYPE yoimov,
          lv_partner         TYPE bu_partner,
          lv_ind_sector      TYPE bu_ind_sector VALUE '2000',
          lv_talhao          TYPE yocotaseg-talhao,
          lv_semana          TYPE yocotaseg-semana,
          lv_carimbo         TYPE zabs_del_carimbo,
          ls_qrcode          TYPE ty_qrcode.

    CHECK gs_prdoc_infocus IS NOT INITIAL.

    IF sy-tcode EQ '/AGRI/FMPRM'
    AND gs_variables-document_mode NE c_mode_display
    AND sy-dynnr EQ c_screen-items.
      lt_stack = cl_abap_get_call_stack=>get_call_stack( ).
      lt_formatted_stack = cl_abap_get_call_stack=>format_call_stack_with_struct( lt_stack ).
      IF gt_items_mod_rows[] IS NOT INITIAL.
        READ TABLE gt_items_mod_rows TRANSPORTING NO FIELDS
          WITH KEY fieldname = 'ZZQRCODE'.
        IF sy-subrc EQ 0.
*--Atenção: Utilize o botão "Ler QR Code" para a leitura do QR Code!
          MESSAGE i164(zfmfp).
          RETURN.
        ENDIF.
      ENDIF.
    ENDIF.

    IF gs_variables-document_mode EQ c_mode_change
    AND gs_prdoc_infocus IS NOT INITIAL.
*-- Get current item
      READ TABLE gt_pritm ASSIGNING FIELD-SYMBOL(<lwa_pritm_changed>)
        WITH KEY pritm = gs_variables-item_infocus.
      IF sy-subrc EQ 0.
*-- Carregamento Mecanizado
        IF <lwa_pritm_changed>-zzllifnr IS NOT INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = <lwa_pritm_changed>-zzllifnr
            IMPORTING
              output = lv_partner.

          SELECT partner, istype, ind_sector UP TO 1 ROWS
            FROM but0is
            INTO @DATA(lwa_but0is)
           WHERE partner    = @lv_partner
             AND ind_sector = @lv_ind_sector.
          ENDSELECT.

          IF sy-subrc NE 0.
*-- Verificar CM/MI do BP!
            CLEAR <lwa_pritm_changed>-zzllifnr.
            MESSAGE i187(zfmfp).
            RETURN.
          ELSE.
            SELECT SINGLE partner, xblck
              FROM but000
              INTO @DATA(lwa_but000)
             WHERE partner = @lv_partner.

            IF sy-subrc NE 0.
*-- CM/MI bloqueado no BP!
              CLEAR <lwa_pritm_changed>-zzllifnr.
              MESSAGE i188(zfmfp).
              RETURN.
            ENDIF.
          ENDIF.
        ENDIF.

*-- Movimentação Interna
        IF <lwa_pritm_changed>-zztlifnr IS NOT INITIAL.
          CLEAR: lwa_but0is, lv_partner.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = <lwa_pritm_changed>-zztlifnr
            IMPORTING
              output = lv_partner.

          SELECT partner, istype, ind_sector UP TO 1 ROWS
            FROM but0is
            INTO @lwa_but0is
           WHERE partner    = @lv_partner
             AND ind_sector = @lv_ind_sector.
          ENDSELECT.

          IF sy-subrc NE 0.
*-- Verificar CM/MI Setor Industrial do BP!
            CLEAR <lwa_pritm_changed>-zztlifnr.
            MESSAGE i187(zfmfp).
            RETURN.
          ELSE.
            SELECT SINGLE partner, xblck
              FROM but000
              INTO @lwa_but000
             WHERE partner = @lv_partner.

            IF sy-subrc NE 0.
*-- CM/MI bloqueado no BP!
              CLEAR <lwa_pritm_changed>-zztlifnr.
              MESSAGE i188(zfmfp).
              RETURN.
            ENDIF.
          ENDIF.
        ENDIF.

        IF <lwa_pritm_changed>-zzsrebo1 IS NOT INITIAL
        AND gs_prdoc_infocus-x-prhdr-semireb1 NE <lwa_pritm_changed>-zzsrebo1
        AND gs_prdoc_infocus-x-prhdr-semireb2 NE <lwa_pritm_changed>-zzsrebo1.
*-- Verificar SRs permitidos nos dados adicionais!
          CLEAR <lwa_pritm_changed>-zzsrebo1.
          MESSAGE i170(zfmfp).
          RETURN.
        ENDIF.

        IF <lwa_pritm_changed>-zzhbatch IS NOT INITIAL.
          SELECT *
            FROM /agri/fmpritm
            INTO TABLE @DATA(lt_fmpritm_bd)
           WHERE gjahr    = @gs_prdoc_infocus-gjahr
             AND zzhbatch = @<lwa_pritm_changed>-zzhbatch.

          IF sy-subrc EQ 0.
            SELECT *
              FROM /agri/fmprhrq
              INTO TABLE @DATA(lt_fmprhrq_bd)
              FOR ALL ENTRIES IN @lt_fmpritm_bd
             WHERE prnum = @lt_fmpritm_bd-prnum
               AND gjahr = @lt_fmpritm_bd-gjahr.

            IF sy-subrc EQ 0.
              SORT lt_fmprhrq_bd BY prnum gjahr.
            ENDIF.
            DATA(lt_fmpritm_aux) = lt_fmpritm_bd[].
            DELETE lt_fmpritm_aux WHERE prnum EQ gs_prdoc_infocus-prnum.
            READ TABLE lt_fmpritm_aux INTO DATA(lwa_fmpritm_aux) INDEX 1.
            IF sy-subrc EQ 0.
              READ TABLE lt_fmprhrq_bd INTO DATA(lwa_fmprhrq_bd)
                WITH KEY prnum = lwa_fmpritm_aux-prnum
                         gjahr = lwa_fmpritm_aux-gjahr.
              IF sy-subrc EQ 0.
                IF lwa_fmprhrq_bd-r_prnum NE gs_prdoc_infocus-prnum.
                  CLEAR <lwa_pritm_changed>-zzhbatch.
*--O Lote Colheita &1 está atribuído ao ticket &2 e será apagado!
                  MESSAGE i148(zfmfp) WITH lwa_fmprhrq_bd-r_prnum.
                  RETURN.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

*-- Get current item
    READ TABLE gt_pritm ASSIGNING FIELD-SYMBOL(<lwa_pritm_qr>)
      WITH KEY pritm = gs_variables-item_infocus.
    IF sy-subrc EQ 0.
      READ TABLE gs_prdoc_infocus-y-pritm INTO DATA(ls_pritm)
            WITH KEY pritm = gs_variables-item_infocus.
      IF ( <lwa_pritm_qr>-zzqrcode IS NOT INITIAL AND
           <lwa_pritm_qr>-zzhbatch IS INITIAL ) OR
         ( <lwa_pritm_qr>-zzqrcode IS NOT INITIAL AND
           ls_pritm IS NOT INITIAL AND
           <lwa_pritm_qr>-zzqrcode <> ls_pritm-zzqrcode ).

        SPLIT <lwa_pritm_qr>-zzqrcode AT '$'
                                    INTO TABLE DATA(lt_string).
        IF sy-subrc EQ 0.
          LOOP AT lt_string INTO DATA(ls_string).
            DATA(lv_tabix) = sy-tabix.
            ASSIGN COMPONENT lv_tabix OF STRUCTURE ls_qrcode
                                      TO FIELD-SYMBOL(<fs_value>).
            IF sy-subrc EQ 0 AND <fs_value> IS ASSIGNED.
              IF lv_tabix EQ 24.
                CONCATENATE ls_string+6(4)
                            ls_string+3(2)
                            ls_string(2)
                       INTO DATA(lv_sdate).
                ls_string = lv_sdate.
              ENDIF.

              <fs_value> = ls_string.
              UNASSIGN <fs_value>.
            ENDIF.
          ENDLOOP.

          <lwa_pritm_qr>-zzturma         = ls_qrcode-zzturma.
          <lwa_pritm_qr>-zzldlifnr       = ls_qrcode-lider_turma.
          <lwa_pritm_qr>-zzhaufnr        = ls_qrcode-aufnr.
          <lwa_pritm_qr>-zzllifnr        = ls_qrcode-ldcde.
          <lwa_pritm_qr>-zzinlat         = ls_qrcode-lat_frstbag.
          <lwa_pritm_qr>-zzinlong        = ls_qrcode-long_frstbag.
          <lwa_pritm_qr>-zzfilat         = ls_qrcode-lat_lst_bag.
          <lwa_pritm_qr>-zzfilong        = ls_qrcode-long_last_bag.
          <lwa_pritm_qr>-zztlifnr        = ls_qrcode-int_tr_prov.
          <lwa_pritm_qr>-zzsrebo2        = ls_qrcode-in_licplate.
          <lwa_pritm_qr>-zzhbatch        = ls_qrcode-charg.
*-- BOC for the INC0031293
          <lwa_pritm_qr>-zzsboxq         = ls_qrcode-avg_cxn.
*-- EOC
          <lwa_pritm_qr>-zztransbordo    = ls_qrcode-zztransbordo.
          <lwa_pritm_qr>-zzcarimbo       = ls_qrcode-bcarimbo.
          <lwa_pritm_qr>-zzdata_colheita = ls_qrcode-ersda.
          <lwa_pritm_qr>-tplnr           = ls_qrcode-tplnr.

          CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
            EXPORTING
              input      = <lwa_pritm_qr>-tplnr
            IMPORTING
              output     = <lwa_pritm_qr>-tplnr
            EXCEPTIONS
              not_found  = 1
              not_active = 2
              OTHERS     = 3.
        ENDIF.
      ENDIF.
    ENDIF.

    IF gs_variables-document_mode NE c_mode_display.
      IF sy-ucomm EQ 'SAVE'.
        DATA(lv_save) = abap_true.
      ELSE.
        lv_save = abap_false.
      ENDIF.

      IF lv_save EQ abap_true.
        LOOP AT gt_pritm ASSIGNING FIELD-SYMBOL(<lwa_pritm>).
          lv_tabix = sy-tabix.
          PERFORM check_batch USING lv_tabix
                           CHANGING <lwa_pritm>
                                    gs_prdoc_infocus-x-prhdr.
        ENDLOOP.
      ELSE.
*-- Get current item
        READ TABLE gt_pritm ASSIGNING <lwa_pritm>
          WITH KEY pritm = gs_variables-item_infocus.
        IF sy-subrc = 0.
          lv_tabix = sy-tabix.
          PERFORM check_batch USING lv_tabix
                           CHANGING <lwa_pritm>
                                    gs_prdoc_infocus-x-prhdr.
        ENDIF.
      ENDIF.
    ENDIF.

    REFRESH: lt_batch_read, lt_batch_create, lt_ausp, lt_charts.

    CLEAR: lwa_pritm, lwa_pritm_a, lv_vbeln, lv_verurx, lv_batchx,
           lv_werks_tox, lv_lgortx,lv_matnrx, lv_matnr_check,
           lv_matnr_out, lv_batch_check, lv_plant_check,
           lv_batch_created, lv_werks_fromx, lv_lev1_terr,
           lv_lev2_terr, lv_int4_field, lv_matnr.

    gt_pritm_copy[] = gt_pritm[].

  ENDFORM.

  FORM class_batch_get USING lv_klass
                    CHANGING lt_ksml TYPE zt_fmpr_charact.

    SELECT *
      FROM klah
      INTO TABLE @DATA(lt_klah)
     WHERE class EQ @lv_klass.

    IF sy-subrc EQ 0.
      SELECT *
        FROM ksml AS l
        INNER JOIN cabn AS n
        ON  l~imerk EQ n~atinn
        INTO CORRESPONDING FIELDS OF TABLE lt_ksml
        FOR ALL ENTRIES IN lt_klah
      WHERE l~clint EQ lt_klah-clint
        AND l~klart EQ '023'.
    ENDIF.

  ENDFORM.

  FORM batch_details_get USING lt_charts TYPE zt_fmpr_charact
                               lv_batch  TYPE charg_d
                      CHANGING lt_ausp  TYPE tt_ausp
                               lv_matnr TYPE matnr.

    TYPES: ltype_cuobn TYPE RANGE OF cuobn.
    DATA: lr_cuobn           TYPE ltype_cuobn,
          lwa_cuobn          LIKE LINE OF lr_cuobn,
          lv_objectkey       TYPE bapi1003_key-object_long,
          lv_classnum        TYPE bapi1003_key-classnum,

          lt_alloclist       TYPE TABLE OF bapi1003_alloc_list,
          lt_allocvaluescurr TYPE tt_bapi1003_alloc_values_curr,
          lt_allocvaluesnum  TYPE tt_bapi1003_alloc_values_num,
          lt_allocvalueschar TYPE tt_bapi1003_alloc_values_char,
          lt_messages        TYPE bapiret2_tt,
          lwa_ausp           TYPE ausp.

    REFRESH lt_ausp.

    SELECT * FROM mcha
      INTO TABLE @DATA(lt_mcha)
      WHERE werks = @/agri/s_fmprhdr-werks
        AND charg = @lv_batch.

    IF sy-subrc EQ 0.
      READ TABLE lt_mcha INTO DATA(lwa_mcha) INDEX 1.
      IF sy-subrc EQ 0.
        MOVE lwa_mcha-matnr TO lv_matnr.
      ENDIF.

      SELECT * FROM mch1
        INTO TABLE @DATA(lt_mch1)
        FOR ALL ENTRIES IN @lt_mcha[]
       WHERE matnr = @lt_mcha-matnr
         AND charg = @lt_mcha-charg.

      IF sy-subrc EQ 0.
        LOOP AT lt_mch1 INTO DATA(lwa_mch1).
          lwa_cuobn-sign   = 'I'.
          lwa_cuobn-option = 'EQ'.
          lwa_cuobn-low    = lwa_mch1-cuobj_bm.
          APPEND lwa_cuobn TO lr_cuobn.
        ENDLOOP.

        SELECT *
          FROM ausp
          INTO TABLE lt_ausp
         WHERE objek IN lr_cuobn[]
           AND klart = '023'.
      ENDIF.

      IF lt_ausp IS INITIAL.
        CONCATENATE lv_matnr lv_batch
        INTO lv_objectkey RESPECTING BLANKS.

        CALL FUNCTION 'BAPI_OBJCL_GETCLASSES'
          EXPORTING
*           OBJECTKEY_IMP      =
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

        LOOP AT lt_allocvalueschar INTO DATA(lwa_allocvalueschar).
          READ TABLE lt_charts INTO DATA(lwa_charts)
          WITH KEY atnam = lwa_allocvalueschar-charact
          BINARY SEARCH.
          IF sy-subrc = 0.
            lwa_ausp-atinn = lwa_charts-imerk.
            lwa_ausp-atwrt = lwa_allocvalueschar-value_neutral."value_char.
            APPEND lwa_ausp TO lt_ausp.
            CLEAR lwa_ausp.
          ENDIF.
        ENDLOOP.

        LOOP AT lt_allocvaluesnum INTO DATA(lwa_allocvaluesnum).
          READ TABLE lt_charts INTO lwa_charts
          WITH KEY atnam = lwa_allocvaluesnum-charact
          BINARY SEARCH.
          IF sy-subrc = 0.
            lwa_ausp-atinn = lwa_charts-imerk.
            lwa_ausp-atflv = lwa_allocvaluesnum-value_from.
            APPEND lwa_ausp TO lt_ausp.
            CLEAR lwa_ausp.
          ENDIF.
        ENDLOOP.

        LOOP AT lt_allocvaluescurr INTO DATA(lwa_allocvaluescurr).
          READ TABLE lt_charts INTO lwa_charts
          WITH KEY atnam = lwa_allocvaluescurr-charact
          BINARY SEARCH.
          IF sy-subrc = 0.
            lwa_ausp-atinn = lwa_charts-imerk.
            lwa_ausp-atflv = lwa_allocvaluescurr-value_from.
            APPEND lwa_ausp TO lt_ausp.
            CLEAR lwa_ausp.
          ENDIF.
        ENDLOOP.
      ENDIF.

    ENDIF.

  ENDFORM.

  FORM batch_update USING lwa_pritm TYPE /agri/s_fmpritm_fcat
                          lwa_prhdr TYPE /agri/s_fmprhdr.

    DATA: wa_prhdr           TYPE /agri/s_fmprhdr,
          t_pritm	           TYPE /agri/t_fmpritm,
          t_allocvaluesnum   TYPE tt_bapi1003_alloc_values_num,
          t_msgs             TYPE zabs_tty_sls_msgs,
          wa_allocvaluesnum  LIKE LINE OF t_allocvaluesnum,
          t_allocvalueschar  TYPE tt_bapi1003_alloc_values_char,
          wa_allocvalueschar LIKE LINE OF t_allocvalueschar,
          t_allocvaluescurr  TYPE tt_bapi1003_alloc_values_curr,
          wa_allocvaluescurr LIKE LINE OF t_allocvaluescurr,
          t_return           TYPE bapiret2_t,
          t_char_batch       TYPE STANDARD TABLE OF clbatch,
          t_char             TYPE STANDARD TABLE OF api_char,
          t_retorno_balanca  TYPE bapirettab,
          wa_ymcha           TYPE mcha,
          wa_yupdmcha        TYPE updmcha,
          v_fmname           TYPE funcname,
          v_order_id         TYPE /scmtms/tor_id,
          v_farm_id          TYPE werks_d,
          v_data             TYPE atwrt,
          v_direction        TYPE ys4e_bal_direction,
          v_peso_total       TYPE /scmtms/qua_gro_wei_val,
          v_un_medida        TYPE /scmtms/qua_gro_wei_uni,
          v_date             TYPE ys4e_baldate,
          v_time             TYPE ys4e_baltime,
          v_peso_bruto       TYPE /scmtms/qua_gro_wei_val,
          v_um_bruto         TYPE /scmtms/qua_gro_wei_uni,
          v_peso_tara        TYPE /scmtms/qua_pack_unl_wei_val,
          v_um_tara          TYPE /scmtms/qua_pack_unl_wei_uni,
          v_class            TYPE klah-class,
          v_objectkey        TYPE bapi1003_key-object,
          v_objecttable      TYPE bapi1003_key-objecttable VALUE 'MCH1',
          v_classnum         TYPE bapi1003_key-classnum VALUE 'Z_FRUTA_MP',
          v_classtype        TYPE bapi1003_key-classtype VALUE '023',
          v_matnr_check      TYPE matnr,
          v_classif_status   TYPE clstatus,
          v_fieldname        TYPE fieldname,
          v_float            TYPE cawn-atflv,
          v_atwrt            TYPE cawn-atwrt,
          v_tor_load         TYPE /scmtms/tor_id,
          v_farm_load        TYPE werks_d,
          v_event_load       TYPE /saptrx/ev_evtid,
          v_date_load        TYPE ys4e_lu_date,
          v_atflv            TYPE zabs_del_inlat,
          v_time_load        TYPE ys4e_lu_time,
          v_cod_imovel       TYPE /agri/glstrno,
          v_talhao           TYPE /agri/glstrno.

    FIELD-SYMBOLS: <lv_fieldvalue> TYPE any.

    SELECT *
      FROM zfmfpsafras
      INTO TABLE @DATA(lt_safras)
     WHERE tipo_safra = @c_tipo_safra-tecnica.

    SORT lt_safras BY ano_safra.

*    v_objectkey(18) = lwa_pritm-plnbez.
    v_matnr_check = 'TCOL0001'.

    CALL FUNCTION 'CONVERSION_EXIT_MATN5_INPUT'
      EXPORTING
        input        = v_matnr_check
      IMPORTING
        output       = v_matnr_check
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

    v_objectkey(18) = v_matnr_check.
    v_objectkey+18(10) = lwa_pritm-zzhbatch.

*...Recupera as características preenchidas do batch
    CALL FUNCTION 'VB_BATCH_GET_DETAIL'
      EXPORTING
        matnr              = v_matnr_check "lwa_pritm-plnbez
        charg              = lwa_pritm-zzhbatch
        get_classification = abap_true
      TABLES
        char_of_batch      = t_char_batch.

*...Recupera todas as características possíveis do batch
    CALL FUNCTION 'VBWS_MARA_CLASSIFICATION_GET'
      EXPORTING
        i_matnr              = v_matnr_check "lwa_pritm-plnbez
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
*****************ZABS_INC_BATCH_UPDATE
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
          WHEN c_caracteristica-carregamento.
            v_header_data = abap_false.
            v_fieldname = 'ZZLLIFNR'.
          WHEN c_caracteristica-movimentacao.
            v_header_data = abap_false.
            v_fieldname = 'ZZTLIFNR'.
          WHEN c_caracteristica-lider.
            v_header_data = abap_false.
            v_fieldname = 'ZZLDLIFNR'.
          WHEN c_caracteristica-turma.
            v_header_data = abap_false.
            v_fieldname = 'ZZTURMA'.
          WHEN c_caracteristica-ordem_colheita.
            v_header_data = abap_false.
            v_fieldname = 'ZZHAUFNR'.
          WHEN c_caracteristica-inicio_lat.
            v_header_data = abap_false.
            v_fieldname = 'ZZINLAT'.
          WHEN c_caracteristica-placa_mov_int.
            v_header_data = abap_false.
            v_fieldname = 'ZZSREBO2'.
          WHEN c_caracteristica-lote_logistico.
            v_header_data = abap_false.
            v_fieldname = 'CHARG'.
          WHEN c_caracteristica-inicio_long.
            v_header_data = abap_false.
            v_fieldname = 'ZZINLONG'.
          WHEN c_caracteristica-fim_lat.
            v_header_data = abap_false.
            v_fieldname = 'ZZFILAT'.
          WHEN c_caracteristica-fim_long.
            v_header_data = abap_false.
            v_fieldname = 'ZZFILONG'.
          WHEN c_caracteristica-caixas_liquidas.
            v_header_data = abap_false.
            v_fieldname = 'ZZCXLIQ'.
          WHEN c_caracteristica-caixas_refugo.
            v_header_data = abap_false.
            v_fieldname = 'ZZCXREF'.
          WHEN c_caracteristica-caixinhas.
            v_header_data = abap_false.
            v_fieldname = 'ZZSBOXQ'.
          WHEN c_caracteristica-lote_colheita.
            v_header_data = abap_false.
            v_fieldname = 'ZZHBATCH'.
          WHEN c_caracteristica-carimbo.
            v_header_data = abap_false.
            v_fieldname = 'ZZCARIMBO'.
          WHEN c_caracteristica-bin.
            v_header_data = abap_false.
            v_fieldname = 'ZZBIN'.
          WHEN c_caracteristica-transbordo.
            v_header_data = abap_false.
            v_fieldname = 'ZZTRANSBORDO'.
          WHEN c_caracteristica-data_colheita.
            v_header_data = abap_false.
            v_fieldname = 'ZZDATA_COLHEITA'.
          WHEN OTHERS.
            CONTINUE.
        ENDCASE.

        UNASSIGN <lv_fieldvalue>.
        IF v_header_data EQ abap_true.
          ASSIGN COMPONENT v_fieldname OF STRUCTURE lwa_prhdr
            TO <lv_fieldvalue>.
          IF sy-subrc NE 0.
            CONTINUE.
          ENDIF.
        ELSE.
          ASSIGN COMPONENT v_fieldname OF STRUCTURE lwa_pritm
            TO <lv_fieldvalue>.
          IF sy-subrc NE 0.
            CONTINUE.
          ELSE.
            IF <lv_fieldvalue> IS NOT INITIAL.
              IF wa_char-atnam EQ c_caracteristica-movimentacao
              OR wa_char-atnam EQ c_caracteristica-carregamento.
                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                  EXPORTING
                    input  = <lv_fieldvalue>
                  IMPORTING
                    output = <lv_fieldvalue>.
              ENDIF.
            ENDIF.
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
            ELSEIF wa_char-atnam EQ c_caracteristica-transbordo.
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
            v_data = <lv_fieldvalue>.
            CALL FUNCTION 'CTCV_CONVERT_DATE_TO_FLOAT'
              EXPORTING
                date  = v_data
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

    DATA(lv_tabix) = 0.
    DATA(t_error) = t_return[].
    DELETE t_error WHERE type NE 'E'.
    LOOP AT t_error INTO DATA(lwa_return).
      ADD 1 TO lv_tabix.
      INSERT INITIAL LINE INTO TABLE t_msgs ASSIGNING FIELD-SYMBOL(<lwa_msg>).
      IF sy-subrc EQ 0.
        <lwa_msg>-msgid = lwa_return-id.
        <lwa_msg>-msgty = lwa_return-type.
        <lwa_msg>-msgno = lwa_return-number.
        <lwa_msg>-msgv1 = lwa_return-message_v1.
        <lwa_msg>-msgv2 = lwa_return-message_v2.
        <lwa_msg>-msgv3 = lwa_return-message_v3.
        <lwa_msg>-msgv4 = lwa_return-message_v4.
      ENDIF.
    ENDLOOP.

    IF t_msgs[] IS NOT INITIAL.
      CALL FUNCTION 'ZABS_FM_MEMORY_MSGS'
        EXPORTING
          it_messages = t_msgs.
    ENDIF.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

    MOVE : v_objectkey(18)    TO wa_ymcha-matnr,
           v_objectkey+18(10) TO wa_ymcha-charg,
           sy-uname           TO wa_ymcha-aenam,
           sy-datum           TO wa_ymcha-laeda,
           'X'                TO wa_yupdmcha-aenam,
           'X'                TO wa_yupdmcha-laeda.

*...Grava log de modificação
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

  ENDFORM.

  FORM check_batch USING lv_tabix  TYPE sytabix
                CHANGING lwa_pritm TYPE /agri/s_fmpritm_fcat
                         lwa_prhdr TYPE /agri/s_fmprhdr.

    DATA: lt_batch_create  TYPE bapiret2_tab,
          lt_charts        TYPE zt_fmpr_charact,
          lt_ausp          TYPE tt_ausp,
          lt_header        TYPE STANDARD TABLE OF bapi_order_header1 INITIAL SIZE 0,
          lt_position      TYPE STANDARD TABLE OF bapi_order_item INITIAL SIZE 0,
          lt_sequence      TYPE STANDARD TABLE OF bapi_order_sequence INITIAL SIZE 0,
          lt_operation     TYPE STANDARD TABLE OF bapi_order_operation1 INITIAL SIZE 0,
          lrt_data         TYPE RANGE OF zabs_del_dt_colheita,
          lwa_ordobjs      TYPE bapi_pp_order_objects,
          lwa_return       TYPE bapiret2,
          lwa_pritm_a      TYPE /agri/s_fmpritm,
          lr_variedade     TYPE RANGE OF matnr,
          lv_matnr_out     TYPE matnr,
          lv_lev1_terr     TYPE char6,
          lv_lev2_terr     TYPE numc5,
          lv_int4_field    TYPE int4,
          lv_aufnr         TYPE caufvd-aufnr,
          lv_matnr         TYPE matnr,
          lv_vbeln         TYPE vbeln_vl,
          lv_verurx        TYPE verur_vl,
          lv_batchx        TYPE bapibatchkey-batch,
          lv_werks_tox     TYPE bapibatchkey-plant,
          lv_lgortx        TYPE bapibatchstoloc-stge_loc,
          lv_matnrx        TYPE bapibatchkey-material,
          lv_batch_check   TYPE charg_d,
          lv_matnr_check   TYPE matnr18,
          lv_plant_check   TYPE bapibatchkey-plant,
          lv_batch_created TYPE bapibatchkey-batch,
          lv_werks_fromx   TYPE bapibatchkey-plant,
          lv_terreno       TYPE /agri/gltplnr_fl,
          lv_werks         TYPE werks_d,
          lv_variedade     TYPE matnr,
          lv_imovel        TYPE yoimov,
          lv_talhao        TYPE yocotaseg-talhao,
          lv_charg         TYPE charg_d VALUE 'ZZ%',
          lv_next_charg    TYPE charg_d,
          lv_carimbo       TYPE zabs_del_carimbo,
          lwa_fetch        TYPE /agri/s_gltplnr,
          lt_tplnr         TYPE /agri/t_gltplnr,
          lt_carimbo       TYPE TABLE OF ty_carimbo,
          cl_acm           TYPE REF TO zcl_zabs_mobile_acm_dpc_ext.

    CHECK lwa_pritm-pritm IS NOT INITIAL.

    DATA(lv_batch_exists) = abap_false.
    DATA(lv_new_batch) = abap_false.
    DATA(lv_error) = abap_false.
    lv_batch_check = lwa_pritm-zzhbatch.
    lv_matnr_check = 'TCOL0001'.
    lv_plant_check = gs_prdoc_infocus-x-prhdr-werks.
    messages_collect_all.

    IF gs_variables-document_mode EQ c_mode_change.
*--BOC-T_T.KONNO-05.27.21
      IF gs_prdoc_infocus-x-prhdr-cltckt NE 'I'.
        DATA(lt_pritm_aux) = gt_pritm[].
        DELETE lt_pritm_aux WHERE charg IS INITIAL.
        IF lt_pritm_aux[] IS NOT INITIAL.
          SELECT prnum, gjahr, pritm, charg, zzhbatch
            FROM /agri/fmpritm
            INTO TABLE @DATA(lt_batches)
            FOR ALL ENTRIES IN @lt_pritm_aux
           WHERE prnum NOT LIKE '0%'
             AND charg EQ @lt_pritm_aux-charg.
          IF sy-subrc EQ 0.
            DELETE lt_batches WHERE prnum EQ gs_prdoc_infocus-x-prhdr-prnum
                                AND gjahr EQ gs_prdoc_infocus-x-prhdr-gjahr.
            READ TABLE lt_batches INTO DATA(ls_batch) INDEX 1.
            IF sy-subrc EQ 0.
*--Lote logístico &1 já atribuído ao ticket &2.
              IF ls_batch-charg EQ lwa_pritm-charg.
                MESSAGE i404(zfmfp) WITH ls_batch-charg ls_batch-prnum.
                CLEAR lwa_pritm-charg.
              ENDIF.
              RETURN.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDIF.


*--EOC-T_T.KONNO-05.27.21

      IF lwa_pritm-tplnr IS NOT INITIAL
      AND lwa_pritm-zzsrebo1 IS NOT INITIAL
      AND lwa_pritm-zzsrebo2 IS NOT INITIAL.
        SELECT SINGLE tplma
          FROM /agri/glflot
          INTO @DATA(lv_tplma)
         WHERE tplnr_fl EQ @lwa_pritm-tplnr.

        IF sy-subrc EQ 0.
          SELECT * UP TO 1 ROWS
            FROM zabst_tran_typ
            INTO @DATA(ls_tran_typ)
           WHERE farm        = @lv_tplma
             AND in_licplate = @lwa_pritm-zzsrebo2.
          ENDSELECT.

          IF sy-subrc EQ 0.
            lwa_pritm-zztransbordo = ls_tran_typ-trans_bord.
          ELSE.
            CLEAR lwa_pritm-zzsrebo1.
*-- Cadastrar Mov.Interna na tabela ZABSV_TRAN_TYP!
            MESSAGE i177(zfmfp).
            RETURN.
          ENDIF.
        ENDIF.
      ENDIF.

      IF lwa_pritm-zztransbordo NE abap_true
      AND lwa_pritm-zztransbordo NE abap_false.
*--A coluna Transbordo pode ser preenchida com o valores 'X' ou vazio!
        MESSAGE i176(zfmfp).
      ENDIF.

      IF gs_prdoc_infocus IS NOT INITIAL
      AND lwa_pritm-zzhbatch IS INITIAL
      AND lwa_pritm-zzsrebo1 IS NOT INITIAL.
        SELECT charg
          FROM mch1
          INTO TABLE @DATA(lt_next_charg)
         WHERE matnr EQ @lv_matnr_check
           AND charg LIKE @lv_charg.

        IF sy-subrc EQ 0.
          DELETE lt_next_charg WHERE charg+2(8) CN '0123456789'.
        ENDIF.

        SORT lt_next_charg BY charg DESCENDING.
        READ TABLE lt_next_charg INTO DATA(ls_next_charg) INDEX 1.
        IF sy-subrc EQ 0.
          lv_next_charg = ls_next_charg.
        ENDIF.

        IF lv_next_charg IS INITIAL.
          lv_next_charg = 'ZZ'.
        ENDIF.

        DATA(lv_number) = lv_next_charg+2(8).
        IF lv_number CO ' 0123456789'.
          ADD 1 TO lv_number.
          UNPACK lv_number TO lv_number.
          lwa_pritm-zzhbatch = 'ZZ' && lv_number.
        ENDIF.
      ENDIF.
    ENDIF.

    IF gs_variables-document_mode NE c_mode_display.
      lt_pritm_aux = gt_pritm[].
      DELETE lt_pritm_aux WHERE zzhbatch IS INITIAL.
      IF lt_pritm_aux[] IS NOT INITIAL.

        SELECT prnum gjahr pritm charg zzhbatch
          FROM /agri/fmpritm
          INTO TABLE lt_batches
          FOR ALL ENTRIES IN lt_pritm_aux
         WHERE prnum NOT LIKE '0%'
           AND zzhbatch EQ lt_pritm_aux-zzhbatch.
        IF sy-subrc EQ 0.

          DELETE lt_batches WHERE prnum EQ gs_prdoc_infocus-x-prhdr-prnum
                              AND gjahr EQ gs_prdoc_infocus-x-prhdr-gjahr.

          LOOP AT lt_batches INTO ls_batch.

            IF ls_batch-zzhbatch EQ lwa_pritm-zzhbatch.
            MESSAGE e000(zfmfp)
             WITH 'Lote colheita' && ls_batch-zzhbatch && 'já atribuído ao ticket' ls_batch-prnum
               INTO sy-msgli.

              message_simple c_false.
              gs_variables-errors = c_true.
              "CLEAR lwa_pritm-zzhbatch.
              lv_error = abap_true.
              RETURN.
            ENDIF.

          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.

    IF ( lv_batch_check IS NOT INITIAL AND
         lv_matnr_check IS NOT INITIAL ) OR
       ( lv_matnr_check IS NOT INITIAL AND
         lwa_pritm-zzqrcode IS NOT INITIAL AND
         lv_batch_check IS INITIAL ).
      CALL FUNCTION 'CONVERSION_EXIT_MATN5_INPUT'
        EXPORTING
          input        = lv_matnr_check
        IMPORTING
          output       = lv_matnr_check
        EXCEPTIONS
          length_error = 1
          OTHERS       = 2.
      IF sy-subrc EQ 0.
        SELECT matnr, werks, charg
          FROM mcha
          INTO TABLE @DATA(lt_mcha)
        WHERE werks = @lv_plant_check
          AND charg = @lv_batch_check
          ORDER BY matnr.

        READ TABLE lt_mcha INTO DATA(lwa_mcha)
          WITH KEY matnr = lv_matnr_check BINARY SEARCH.
        IF sy-subrc EQ 0.
          lv_batch_exists = abap_true.
        ELSE.
          READ TABLE lt_mcha INTO lwa_mcha INDEX 1.
          IF sy-subrc EQ 0.
            CALL FUNCTION 'CONVERSION_EXIT_MATN5_OUTPUT'
              EXPORTING
                input  = lwa_mcha-matnr
              IMPORTING
                output = lv_matnr_out.
            CLEAR lwa_pritm-zzhbatch.
*--O Lote &1 está atribuído ao material &2 e será apagado do item &3!
            MESSAGE i043(zfmfp) WITH lwa_mcha-charg lv_matnr_out lwa_pritm-pritm .
          ELSE.
            CALL FUNCTION 'BAPI_BATCH_CREATE'
              EXPORTING
                material = lv_matnr_check
                batch    = lv_batch_check
                plant    = lv_plant_check
              IMPORTING
                batch    = lv_batch_created
              TABLES
                return   = lt_batch_create.

*--Lote &1 é criado
            READ TABLE lt_batch_create TRANSPORTING NO FIELDS
              WITH KEY type = 'S'
                         id = '12'
                     number = '128'.

            IF sy-subrc = 0.
              lv_new_batch = abap_true.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = abap_true.

              IF lwa_pritm-zzhbatch IS INITIAL.
                lwa_pritm-zzhbatch = lv_batch_created.
              ENDIF.

              lv_batch_exists = abap_true.

              DO 5 TIMES.
                SELECT SINGLE COUNT( * )
                  FROM mcha
                 WHERE matnr = lv_matnr_check
                   AND werks = lv_plant_check
                   AND charg = lv_batch_check.

                IF sy-subrc = 0.
                  EXIT.
                ELSE.
                  WAIT UP TO 1 SECONDS.
                ENDIF.
              ENDDO.
            ELSE.
              CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF lv_batch_exists = abap_true.
      IF gs_variables-document_mode NE c_mode_display
        AND lwa_pritm-pritm IS NOT INITIAL.
        READ TABLE gt_pritm_copy INTO DATA(lwa_pritm_copy)
          WITH KEY prnum = lwa_pritm-prnum
                   gjahr = lwa_pritm-gjahr
                   pritm = lwa_pritm-pritm.
        IF sy-subrc EQ 0.
          IF lwa_pritm_copy-zzhbatch NE lwa_pritm-zzhbatch.
            DATA(lv_move_values) = abap_true.
          ELSE.
            lv_move_values = abap_false.
          ENDIF.
        ELSE.
          lv_move_values = abap_true.
        ENDIF.
      ELSE.
        lv_move_values = abap_false.
      ENDIF.
      IF sy-ucomm EQ 'SAVE'.
        PERFORM batch_update IN PROGRAM /agri/saplfmprm
                                  USING lwa_pritm
                                        gs_prdoc_infocus-x-prhdr IF FOUND.
      ENDIF.

      PERFORM class_batch_get IN PROGRAM /agri/saplfmprm
                                   USING 'Z_FRUTA_MP'
                                CHANGING lt_charts IF FOUND.

      SORT lt_charts BY atnam.

      PERFORM batch_details_get IN PROGRAM /agri/saplfmprm
                                     USING lt_charts
                                           lwa_pritm-zzhbatch
                                  CHANGING lt_ausp
                                           lv_matnr IF FOUND.

      SORT lt_ausp BY atinn.

      lwa_pritm-cmnum = 'CITROS'.

      IF lv_move_values EQ abap_true.
        READ TABLE lt_charts INTO DATA(lwa_charts)
          WITH KEY atnam = 'RC_FR_TALH' BINARY SEARCH.
        IF sy-subrc EQ 0.
          READ TABLE lt_ausp INTO DATA(lwa_ausp)
            WITH KEY atinn = lwa_charts-imerk BINARY SEARCH.
          IF sy-subrc EQ 0.
            MOVE lwa_ausp-atwrt TO lv_int4_field.
            lv_lev2_terr = lv_int4_field.
          ENDIF.
        ENDIF.

        READ TABLE lt_charts INTO lwa_charts
          WITH KEY atnam = 'ABS_TRANSBORDO' BINARY SEARCH.
        IF sy-subrc EQ 0.
          READ TABLE lt_ausp INTO lwa_ausp
            WITH KEY atinn = lwa_charts-imerk BINARY SEARCH.
          IF sy-subrc EQ 0.
            lwa_pritm-zztransbordo = lwa_ausp-atwrt.
          ENDIF.
        ENDIF.

        CLEAR lwa_charts.
        READ TABLE lt_charts INTO lwa_charts
          WITH KEY atnam = 'RC_FR_COD_IM' BINARY SEARCH.
        IF sy-subrc EQ 0.
          READ TABLE lt_ausp INTO lwa_ausp
            WITH KEY atinn = lwa_charts-imerk BINARY SEARCH.
          IF sy-subrc EQ 0.
            MOVE lwa_ausp-atwrt TO lv_int4_field.
            lv_lev1_terr = lv_int4_field.
          ENDIF.
        ENDIF.

        IF lwa_pritm-tplnr IS INITIAL
        AND lv_lev1_terr IS NOT INITIAL
        AND lv_lev2_terr IS NOT INITIAL.
          CONCATENATE lv_lev1_terr '-' lv_lev2_terr INTO lwa_pritm-tplnr.
          CONDENSE lwa_pritm-tplnr NO-GAPS.

          CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
            EXPORTING
              input      = lwa_pritm-tplnr
            IMPORTING
              output     = lwa_pritm-tplnr
            EXCEPTIONS
              not_found  = 1
              not_active = 2
              OTHERS     = 3.
        ENDIF.

        READ TABLE lt_charts INTO lwa_charts
          WITH KEY atnam = 'ABS_CL_ORDEM' BINARY SEARCH.
        IF sy-subrc EQ 0.
          READ TABLE lt_ausp INTO lwa_ausp
            WITH KEY atinn = lwa_charts-imerk BINARY SEARCH.
          IF sy-subrc EQ 0.
            MOVE lwa_ausp-atwrt TO lv_int4_field.
            MOVE lv_int4_field TO lwa_pritm-zzhaufnr.
          ENDIF.
        ENDIF.

        READ TABLE lt_charts INTO lwa_charts
          WITH KEY atnam = 'ABS_PLACA_SERV_INT' BINARY SEARCH.
        IF sy-subrc EQ 0.
          READ TABLE lt_ausp INTO lwa_ausp
            WITH KEY atinn = lwa_charts-imerk BINARY SEARCH.
          IF sy-subrc EQ 0.
            MOVE lwa_ausp-atwrt TO lwa_pritm-zzsrebo2.
          ENDIF.
        ENDIF.

        READ TABLE lt_charts INTO lwa_charts
          WITH KEY atnam = 'RC_FR_PLACA_CR' BINARY SEARCH.
        IF sy-subrc EQ 0.
          READ TABLE lt_ausp INTO lwa_ausp
            WITH KEY atinn = lwa_charts-imerk BINARY SEARCH.
          IF sy-subrc EQ 0.
            MOVE lwa_ausp-atwrt TO lwa_pritm-zzsrebo1.
          ENDIF.
        ENDIF.

        READ TABLE lt_charts INTO lwa_charts
          WITH KEY atnam = 'ABS_MECANIZADO' BINARY SEARCH.
        IF sy-subrc EQ 0.
          READ TABLE lt_ausp INTO lwa_ausp
            WITH KEY atinn = lwa_charts-imerk BINARY SEARCH.
          IF sy-subrc EQ 0.
            MOVE: lwa_ausp-atwrt TO lv_int4_field,
                  lv_int4_field TO lwa_pritm-zzllifnr.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = lwa_pritm-zzllifnr
              IMPORTING
                output = lwa_pritm-zzllifnr.
          ENDIF.
        ENDIF.

        READ TABLE lt_charts INTO lwa_charts
          WITH KEY atnam = 'ABS_CL_MOVINT' BINARY SEARCH.
        IF sy-subrc EQ 0.
          READ TABLE lt_ausp INTO lwa_ausp
            WITH KEY atinn = lwa_charts-imerk BINARY SEARCH.
          IF sy-subrc EQ 0.
            MOVE: lwa_ausp-atwrt TO lv_int4_field,
                  lv_int4_field TO lwa_pritm-zztlifnr.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = lwa_pritm-zztlifnr
              IMPORTING
                output = lwa_pritm-zztlifnr.
          ENDIF.
        ENDIF.

        READ TABLE lt_charts INTO lwa_charts
          WITH KEY atnam = 'ABS_CL_LIDER' BINARY SEARCH.
        IF sy-subrc EQ 0.
          READ TABLE lt_ausp INTO lwa_ausp
            WITH KEY atinn = lwa_charts-imerk BINARY SEARCH.
          IF sy-subrc EQ 0.
            MOVE: lwa_ausp-atflv TO lv_int4_field,
                  lv_int4_field TO lwa_pritm-zzldlifnr.
          ENDIF.
        ENDIF.

        READ TABLE lt_charts INTO lwa_charts
          WITH KEY atnam = 'ABS_CL_TURMA' BINARY SEARCH.
        IF sy-subrc EQ 0.
          READ TABLE lt_ausp INTO lwa_ausp
            WITH KEY atinn = lwa_charts-imerk BINARY SEARCH.
          IF sy-subrc EQ 0.
            MOVE: lwa_ausp-atwrt TO lv_int4_field,
                  lv_int4_field TO lwa_pritm-zzturma.
            UNPACK lwa_pritm-zzturma TO lwa_pritm-zzturma.
          ENDIF.
        ENDIF.

        READ TABLE lt_charts INTO lwa_charts
          WITH KEY atnam = 'ABS_CL_CAIXINHA_RATEIO' BINARY SEARCH.
        IF sy-subrc EQ 0.
          READ TABLE lt_ausp INTO lwa_ausp
            WITH KEY atinn = lwa_charts-imerk BINARY SEARCH.
          IF sy-subrc EQ 0.
            MOVE: lwa_ausp-atflv TO lv_int4_field,
                  lv_int4_field TO lwa_pritm-zzsboxq.
          ENDIF.
        ENDIF.

        READ TABLE lt_charts INTO lwa_charts
          WITH KEY atnam = 'ABS_DATA_COLHEITA' BINARY SEARCH.
        IF sy-subrc EQ 0.
          READ TABLE lt_ausp INTO lwa_ausp
            WITH KEY atinn = lwa_charts-imerk BINARY SEARCH.
          IF sy-subrc EQ 0.
            MOVE: lwa_ausp-atflv TO lv_int4_field.
            WRITE lv_int4_field TO lwa_pritm-zzdata_colheita.
          ENDIF.
        ENDIF.

        READ TABLE lt_charts INTO lwa_charts
          WITH KEY atnam = 'ABS_MECANIZADO_ILAT' BINARY SEARCH.
        IF sy-subrc EQ 0.
          READ TABLE lt_ausp INTO lwa_ausp
            WITH KEY atinn = lwa_charts-imerk BINARY SEARCH.
          IF sy-subrc EQ 0.
            MOVE lwa_ausp-atflv TO lwa_pritm-zzinlat.
          ENDIF.
        ENDIF.

        READ TABLE lt_charts INTO lwa_charts
          WITH KEY atnam = 'ABS_MECANIZADO_ILONG' BINARY SEARCH.
        IF sy-subrc EQ 0.
          READ TABLE lt_ausp INTO lwa_ausp
            WITH KEY atinn = lwa_charts-imerk BINARY SEARCH.
          IF sy-subrc EQ 0.
            MOVE lwa_ausp-atflv TO lwa_pritm-zzinlong.
          ENDIF.
        ENDIF.

        READ TABLE lt_charts INTO lwa_charts
           WITH KEY atnam = 'ABS_MECANIZADO_FLONG' BINARY SEARCH.
        IF sy-subrc EQ 0.
          READ TABLE lt_ausp INTO lwa_ausp
            WITH KEY atinn = lwa_charts-imerk BINARY SEARCH.
          IF sy-subrc EQ 0.
            MOVE lwa_ausp-atflv TO lwa_pritm-zzfilong.
          ENDIF.
        ENDIF.

        READ TABLE lt_charts INTO lwa_charts
          WITH KEY atnam = 'ABS_MECANIZADO_FLAT' BINARY SEARCH.
        IF sy-subrc EQ 0.
          READ TABLE lt_ausp INTO lwa_ausp
            WITH KEY atinn = lwa_charts-imerk BINARY SEARCH.
          IF sy-subrc EQ 0.
            MOVE lwa_ausp-atflv TO lwa_pritm-zzfilat.
          ENDIF.
        ENDIF.

*-- Populate BIN
        READ TABLE lt_charts INTO lwa_charts
          WITH KEY atnam = 'ABS_BIN' BINARY SEARCH.
        IF sy-subrc EQ 0.
          READ TABLE lt_ausp INTO lwa_ausp
            WITH KEY atinn = lwa_charts-imerk BINARY SEARCH.
          IF sy-subrc EQ 0.
            MOVE lwa_ausp-atwrt TO lwa_pritm-zzbin.
          ENDIF.
        ENDIF.
      ENDIF.

      IF lwa_pritm-zzcxliq GE 0.
        lwa_pritm-dirwgth = lwa_pritm-zzcxliq.
      ENDIF.

      IF gs_prdoc_infocus-x-prhdr-zrems(10) IS NOT INITIAL
      AND lwa_prhdr-fruta_mercado EQ abap_false.
        lv_vbeln = gs_prdoc_infocus-x-prhdr-zrems(10).
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lv_vbeln
          IMPORTING
            output = lv_vbeln.

        SELECT SINGLE vbeln, vbtyp, kunnr, verur
          FROM likp
          INTO @DATA(lwa_likp)
         WHERE vbeln = @lv_vbeln.

        IF sy-subrc EQ 0
        AND lwa_likp-vbtyp EQ '7'.
          SELECT vbeln, werks UP TO 1 ROWS
           FROM lips
           INTO @DATA(lwa_lips)
          WHERE vbeln = @lv_vbeln.
          ENDSELECT.

          IF sy-subrc EQ 0.
            lwa_likp-kunnr = lwa_lips-werks.
          ENDIF.
        ENDIF.

        lv_verurx = lwa_likp-verur.
        SHIFT lwa_likp-kunnr LEFT DELETING LEADING '0'.
        lv_werks_tox = lwa_likp-kunnr.
        lv_werks_fromx = gs_prdoc_infocus-x-prhdr-werks.
        lv_lgortx = 'DLAR'.
        lv_matnrx = lwa_pritm-plnbez.

        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            input        = lv_matnrx
          IMPORTING
            output       = lv_matnrx
          EXCEPTIONS
            length_error = 1
            OTHERS       = 2.

* ZZHBATCH: Lote Colheita
* CHARG: Lote Logístico
        DATA(lv_create_charg) = abap_true.
        IF lwa_pritm-zzsrebo1 IS NOT INITIAL.
          LOOP AT gt_pritm INTO DATA(lwa_pritm_check).
            IF lwa_pritm_check-pritm = gs_variables-item_infocus
            OR lwa_pritm_check-pritm IS INITIAL.
              CONTINUE.
            ELSE.
              IF lwa_pritm_check-zzsrebo1 EQ lwa_pritm-zzsrebo1
              AND lwa_pritm_check-charg IS NOT INITIAL.
                lv_create_charg = abap_false.
                IF lwa_pritm_check-plnbez NE lv_matnrx.
                  IF lv_matnrx IS NOT INITIAL.
                    CALL FUNCTION 'CONVERSION_EXIT_MATN5_OUTPUT'
                      EXPORTING
                        input  = lwa_pritm_check-plnbez
                      IMPORTING
                        output = lv_matnr_out.
*-- Utilizar material &1 nos itens referentes à placa de semi reboque &2!
                    MESSAGE i052(zfmfp) WITH lv_matnr_out lwa_pritm_check-zzsrebo1.
                    CLEAR lwa_pritm-zzhbatch.
                  ENDIF.
                ELSE.
                  lwa_pritm-charg = lwa_pritm_check-charg.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ELSE.
          lv_create_charg = abap_false.
        ENDIF.

        IF  lv_werks_tox IS NOT INITIAL
        AND lv_lgortx IS NOT INITIAL
        AND lv_matnrx IS NOT INITIAL
        AND lv_create_charg EQ abap_true
        AND lv_werks_fromx IS NOT INITIAL
        AND ( sy-ucomm EQ 'ENTR' OR sy-ucomm EQ 'USF1' )
        AND gs_variables-document_mode EQ c_mode_change
        AND sy-cprog EQ '/AGRI/SAPLFMPRM'
        AND lwa_pritm-zzhbatch IS NOT INITIAL
        AND lwa_pritm-charg IS INITIAL.
          CALL FUNCTION 'YOMM_CRIACAO_LOTE'
            EXPORTING
              imp_verur         = lv_verurx
              imp_werks_destino = lv_werks_tox
              imp_lgort         = lv_lgortx
              imp_matnr         = lv_matnrx
              imp_werks_origem  = lv_werks_fromx
              imp_remessa       = lwa_likp-vbeln
            IMPORTING
              exp_batch         = lv_batchx.

          lwa_pritm-charg = lv_batchx.
        ENDIF.
      ENDIF.
    ENDIF.

    IF lwa_prhdr-fruta_mercado EQ abap_true
    AND gs_variables-document_mode EQ c_mode_change
    AND sy-cprog EQ '/AGRI/SAPLFMPRM'.
      lv_matnrx = lwa_pritm-plnbez.
* ZZHBATCH: Lote Colheita
* CHARG: Lote Logístico
      lv_create_charg = abap_true.
      IF lwa_pritm-zzsrebo1 IS NOT INITIAL.
        LOOP AT gt_pritm INTO lwa_pritm_check.
          IF lwa_pritm_check-pritm = gs_variables-item_infocus.
            CONTINUE.
          ELSE.
            IF lwa_pritm_check-zzsrebo1 EQ lwa_pritm-zzsrebo1
            AND lwa_pritm_check-charg IS NOT INITIAL.
              lv_create_charg = abap_false.
              IF lwa_pritm_check-plnbez NE lv_matnrx.
                IF lv_matnrx IS NOT INITIAL.
                  CALL FUNCTION 'CONVERSION_EXIT_MATN5_OUTPUT'
                    EXPORTING
                      input  = lwa_pritm_check-plnbez
                    IMPORTING
                      output = lv_matnr_out.
*-- Utilizar material &1 nos itens referentes à placa de semi reboque &2!
                  MESSAGE i052(zfmfp) WITH lv_matnr_out lwa_pritm_check-zzsrebo1.
                  CLEAR lwa_pritm-zzhbatch.
                ENDIF.
              ELSE.
                lwa_pritm-charg = lwa_pritm_check-charg.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ELSE.
        lv_create_charg = abap_false.
      ENDIF.

      IF lv_create_charg EQ abap_true
      AND lwa_pritm-zzhbatch IS NOT INITIAL
      AND lwa_pritm-charg IS INITIAL.
        lv_matnrx = lwa_pritm-plnbez.

        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            input        = lv_matnrx
          IMPORTING
            output       = lv_matnrx
          EXCEPTIONS
            length_error = 1
            OTHERS       = 2.

        lv_charg = 'FZ%'.
        REFRESH lt_next_charg.

*        SELECT charg
*          FROM mch1
*          INTO TABLE @lt_next_charg
*         WHERE matnr EQ   @lv_matnrx
*           AND charg LIKE @lv_charg.
        SELECT charg
          FROM mch1
          INTO TABLE @lt_next_charg
         WHERE charg LIKE @lv_charg.

        IF sy-subrc EQ 0.
          DELETE lt_next_charg WHERE charg+2(8) CN '0123456789'.
        ENDIF.

        SORT lt_next_charg BY charg DESCENDING.
        CLEAR: ls_next_charg, lv_next_charg, lv_number.
        READ TABLE lt_next_charg INTO ls_next_charg INDEX 1.
        IF sy-subrc EQ 0.
          lv_next_charg = ls_next_charg.
        ENDIF.

        IF lv_next_charg IS INITIAL.
          lv_next_charg = 'FZ'.
        ENDIF.

        lv_number = lv_next_charg+2(8).
        IF lv_number CO ' 0123456789'.
          ADD 1 TO lv_number.
          UNPACK lv_number TO lv_number.
          lv_batch_check = 'FZ' && lv_number.
        ENDIF.

        REFRESH: lt_batch_create.
        CLEAR: lv_batch_created.
        CALL FUNCTION 'BAPI_BATCH_CREATE'
          EXPORTING
            material = lv_matnrx
            batch    = lv_batch_check
            plant    = lv_plant_check
          IMPORTING
            batch    = lv_batch_created
          TABLES
            return   = lt_batch_create.

*--Lote &1 é criado
        READ TABLE lt_batch_create TRANSPORTING NO FIELDS
          WITH KEY type = 'S'
                     id = '12'
                 number = '128'.

        IF sy-subrc = 0.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.

          IF lwa_pritm-charg IS INITIAL.
            lwa_pritm-charg = lv_batch_created.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
      EXPORTING
        input  = lwa_pritm-tplnr
      IMPORTING
        output = lv_terreno.

    lv_imovel = lv_terreno+0(6).
    lv_talhao = lv_terreno+7(5).
    lv_werks = gs_prdoc_infocus-x-prhdr-werks.
    lv_variedade = lwa_pritm-plnbez.
    DO 2 TIMES.
      DATA(lv_index) = sy-index.
      INSERT INITIAL LINE INTO TABLE lr_variedade
        ASSIGNING FIELD-SYMBOL(<lwa_variedade>).
      IF sy-subrc EQ 0.
        <lwa_variedade> = 'IEQ'.
        CASE lv_index.
          WHEN 1.
            <lwa_variedade>-low = lv_variedade.
          WHEN 2.
            CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
              EXPORTING
                input  = lv_variedade
              IMPORTING
                output = <lwa_variedade>-low.
        ENDCASE.
      ENDIF.
    ENDDO.

    IF lwa_pritm-zzdata_colheita IS NOT INITIAL
    AND ( gs_variables-document_mode EQ c_mode_change
    OR gs_variables-document_mode EQ c_mode_change ).
      INSERT INITIAL LINE INTO TABLE lrt_data
        ASSIGNING FIELD-SYMBOL(<lrs_data>).
      IF sy-subrc EQ 0.
        <lrs_data> = 'IBT'.
        <lrs_data>-low = lwa_pritm-zzdata_colheita.
        <lrs_data>-high = lwa_pritm-zzdata_colheita + 20.
      ENDIF.

      SELECT *
        INTO TABLE @DATA(lt_cotaseg)
        FROM yocotaseg
       WHERE dtvlcota  IN @lrt_data[]
         AND imov      EQ @lv_imovel
         AND talhao    EQ @lv_talhao.
      IF sy-subrc = 0.
        SORT lt_cotaseg BY dtvlcota ASCENDING.
        READ TABLE lt_cotaseg INTO DATA(lwa_cotaseg) INDEX 1.
        IF sy-subrc EQ 0.
          lwa_pritm-zzcarimbo = lwa_cotaseg-carimbo.
        ENDIF.
      ENDIF.

*-----------------------------------------------Wave 3
      IF lwa_pritm-zzcarimbo IS INITIAL.

        CLEAR lwa_fetch. REFRESH: lt_tplnr, lt_carimbo.
        lwa_fetch-tplnr_fl = lwa_pritm-tplnr.
        APPEND lwa_fetch TO lt_tplnr.
        CLEAR lwa_fetch.

        CREATE OBJECT cl_acm.

        CALL METHOD cl_acm->fetch_carimbo
          EXPORTING
            it_tplnr   = lt_tplnr
          CHANGING
            ct_carimbo = lt_carimbo.

        IF lt_carimbo IS NOT INITIAL.
          READ TABLE lt_carimbo INTO DATA(lwa_carimbo) INDEX 1.
          IF sy-subrc EQ 0.
            lwa_pritm-zzcarimbo = lwa_carimbo-carimbo.
          ENDIF.
        ENDIF.
      ENDIF.
*-----------------------------------------------Wave 3

    ENDIF.

    IF gs_variables-document_mode NE c_mode_display
    AND lwa_pritm-zzhaufnr IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lwa_pritm-zzhaufnr
        IMPORTING
          output = lv_aufnr.

*-- Get operation details
      lwa_ordobjs-header     = abap_true.
      lwa_ordobjs-positions  = abap_true.
      lwa_ordobjs-sequences  = abap_true.
      lwa_ordobjs-operations = abap_true.

      CALL FUNCTION 'BAPI_PRODORD_GET_DETAIL'
        EXPORTING
          number        = lv_aufnr
          order_objects = lwa_ordobjs
        IMPORTING
          return        = lwa_return
        TABLES
          header        = lt_header
          position      = lt_position
          sequence      = lt_sequence
          operation     = lt_operation.

      LOOP AT lt_position INTO DATA(lwa_position) WHERE material NE lv_matnr_check.

        MESSAGE e000(zfmfp) WITH 'Não existe Mat: TCOL001 na Ordem' && lwa_pritm-zzhaufnr
            INTO sy-msgli.
        message_simple c_false.
        gs_variables-errors = c_true.
        RETURN.
      ENDLOOP.
    ENDIF.

    IF gs_variables-document_mode EQ c_mode_change
    AND lwa_pritm-zzhaufnr IS NOT INITIAL
    AND lwa_pritm-zzcxliq IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lwa_pritm-zzhaufnr
        IMPORTING
          output = lv_aufnr.

*-- Get operation details
      lwa_ordobjs-header     = abap_true.
      lwa_ordobjs-positions  = abap_true.
      lwa_ordobjs-sequences  = abap_true.
      lwa_ordobjs-operations = abap_true.

      CALL FUNCTION 'BAPI_PRODORD_GET_DETAIL'
        EXPORTING
          number        = lv_aufnr
          order_objects = lwa_ordobjs
        IMPORTING
          return        = lwa_return
        TABLES
          header        = lt_header
          position      = lt_position
          sequence      = lt_sequence
          operation     = lt_operation.

      READ TABLE lt_position INTO lwa_position INDEX 1.
      IF sy-subrc EQ 0.
        DATA(lv_available) = lwa_position-quantity - lwa_position-delivered_quantity.
        IF lwa_pritm-zzcxliq GT lv_available.
          READ TABLE gs_prdoc_infocus-x-pritm ASSIGNING FIELD-SYMBOL(<lwa_infocus_itm>)
            WITH KEY pritm = lwa_pritm-pritm.
          IF sy-subrc EQ 0.
            CLEAR: <lwa_infocus_itm>-zzcxliq,
                   lwa_pritm-zzcxliq.
*-- Saldo insuficiente na Ordem Colheita &1!
            MESSAGE i073(zfmfp) WITH lwa_pritm-zzhaufnr.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF gs_variables-document_mode EQ c_mode_change
    AND lwa_pritm-aufnr IS NOT INITIAL
    AND lwa_pritm-zzcxliq IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lwa_pritm-aufnr
        IMPORTING
          output = lv_aufnr.

*-- Get operation details
      lwa_ordobjs-header     = abap_true.
      lwa_ordobjs-positions  = abap_true.
      lwa_ordobjs-sequences  = abap_true.
      lwa_ordobjs-operations = abap_true.

      REFRESH: lt_header, lt_position, lt_sequence, lt_operation.
      CLEAR lwa_return.
      CALL FUNCTION 'BAPI_PRODORD_GET_DETAIL'
        EXPORTING
          number        = lv_aufnr
          order_objects = lwa_ordobjs
        IMPORTING
          return        = lwa_return
        TABLES
          header        = lt_header
          position      = lt_position
          sequence      = lt_sequence
          operation     = lt_operation.

      READ TABLE lt_position INTO lwa_position INDEX 1.
      IF sy-subrc EQ 0.
        lv_available = lwa_position-quantity - lwa_position-delivered_quantity.
        IF lwa_pritm-zzcxliq GT lv_available.
          READ TABLE gs_prdoc_infocus-x-pritm ASSIGNING <lwa_infocus_itm>
            WITH KEY pritm = lwa_pritm-pritm.
          IF sy-subrc EQ 0.
*-- Saldo insuficiente na Ordem Pátio &1!
            MESSAGE i079(zfmfp) WITH lwa_pritm-aufnr.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    MOVE-CORRESPONDING lwa_pritm TO lwa_pritm_a.
    MODIFY gs_prdoc_infocus-x-pritm FROM lwa_pritm_a INDEX lv_tabix.

  ENDFORM.
