class ZCL_ZFARM_AGRIPLAN_DPC_EXT definition
  public
  inheriting from ZCL_ZFARM_AGRIPLAN_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_BEGIN
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_END
    redefinition .
protected section.

  methods CHECKSAVEDRECIPE_GET_ENTITYSET
    redefinition .
  methods CHECKTERRAINSET_GET_ENTITYSET
    redefinition .
  methods CONSULTARESTOQUE_GET_ENTITYSET
    redefinition .
  methods CREATEORDERPROCE_GET_ENTITYSET
    redefinition .
  methods CREATEORDERTASKS_GET_ENTITYSET
    redefinition .
  methods CUSTORECEITASET_GET_ENTITYSET
    redefinition .
  methods GERAEXCELSET_GET_ENTITYSET
    redefinition .
  methods GETAREACULTIVOSE_GET_ENTITYSET
    redefinition .
  methods GETCENTROSET_GET_ENTITYSET
    redefinition .
  methods GETDETAIL1DATASE_GET_ENTITYSET
    redefinition .
  methods GETHORASEFETSET_GET_ENTITYSET
    redefinition .
  methods GETMONTHDATESET_GET_ENTITY
    redefinition .
  methods GETMONTHDATESET_GET_ENTITYSET
    redefinition .
  methods GETPLANNINGSET_CREATE_ENTITY
    redefinition .
  methods GETPLANNINGSET_GET_ENTITY
    redefinition .
  methods GETPLANNINGSET_GET_ENTITYSET
    redefinition .
  methods GETPLANNINGSET_UPDATE_ENTITY
    redefinition .
  methods GETPLANTAREFASSE_GET_ENTITYSET
    redefinition .
  methods GETPROCESSOSET_GET_ENTITYSET
    redefinition .
  methods GETPRODUTOSSET_CREATE_ENTITY
    redefinition .
  methods GETPRODUTOSSET_GET_ENTITYSET
    redefinition .
  methods GETRECEITALIST01_GET_ENTITYSET
    redefinition .
  methods GETRECEITALISTAS_GET_ENTITYSET
    redefinition .
  methods GETRECEITASSET_GET_ENTITYSET
    redefinition .
  methods GETRESUMOORCSET_GET_ENTITYSET
    redefinition .
  methods GETSAFRASET_GET_ENTITYSET
    redefinition .
  methods GETSTORAGELOCATI_GET_ENTITYSET
    redefinition .
  methods GETTALHAOIDADESE_GET_ENTITYSET
    redefinition .
  methods GETTALHAOSET_GET_ENTITYSET
    redefinition .
  methods GETTAREFASORCSET_GET_ENTITYSET
    redefinition .
  methods GETTASKINSUMOSET_GET_ENTITYSET
    redefinition .
  methods GETTERRENOSET_GET_ENTITYSET
    redefinition .
  methods GETVOLUMECALDASE_GET_ENTITYSET
    redefinition .
  methods GETWEEKDATESET_GET_ENTITY
    redefinition .
  methods GETWEEKDATESET_GET_ENTITYSET
    redefinition .
  methods GETWORKCENTERPLA_CREATE_ENTITY
    redefinition .
  methods GETWORKCENTERPLA_GET_ENTITYSET
    redefinition .
  methods GETWORKCENTERSET_CREATE_ENTITY
    redefinition .
  methods GETWORKCENTERSET_GET_ENTITYSET
    redefinition .
  methods GETYEARMONTHSSET_GET_ENTITYSET
    redefinition .
  methods HEADERAREACULTIV_GET_ENTITY
    redefinition .
  methods HEADERAREACULTIV_GET_ENTITYSET
    redefinition .
  methods SETPRODUTOSIMILA_CREATE_ENTITY
    redefinition .
  methods SETPROGTALHAOSET_CREATE_ENTITY
    redefinition .
  methods SETPROGTALHAOSET_GET_ENTITYSET
    redefinition .
  methods SETRECEITASSET_CREATE_ENTITY
    redefinition .
  methods SETRECEITASSET_GET_ENTITYSET
    redefinition .
  methods SETRECEITASSET_UPDATE_ENTITY
    redefinition .
  methods SETTECHCOMPLETES_CREATE_ENTITY
    redefinition .
  methods SETTECHCOMPLETES_GET_ENTITYSET
    redefinition .
  methods SUGGEST_TERRAINS_GET_ENTITYSET
    redefinition .
  methods GETVERSIONSET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZFARM_AGRIPLAN_DPC_EXT IMPLEMENTATION.


  method /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_BEGIN.
**TRY.
*CALL METHOD SUPER->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_BEGIN
*  EXPORTING
*    IT_OPERATION_INFO =
**  CHANGING
**    cv_defer_mode     =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
  endmethod.


  method /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_END.
**TRY.
*CALL METHOD SUPER->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_END
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
  endmethod.


METHOD checksavedrecipe_get_entityset.

*-- Local Declarations
  DATA : lt_filter     TYPE /iwbep/t_mgw_select_option,
         ls_filter     TYPE /iwbep/s_mgw_select_option,
         lt_orc_full   TYPE STANDARD TABLE OF zabs_orcamento INITIAL SIZE 0,
         ls_orc_full   LIKE LINE OF lt_orc_full,
         lv_filter_str TYPE string,
         lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
         lv_rcnum      TYPE zfmrcnum,
         lv_tipo       TYPE char10,
         lt_dates      TYPE /scwm/tt_lm_dates,
         lt_period     TYPE STANDARD TABLE OF period INITIAL SIZE 0,
         ls_period     LIKE LINE OF lt_period,
         ls_entityset  LIKE LINE OF et_entityset,
         lrt_period    TYPE RANGE OF tvarvc-low,
         lrs_period    LIKE LINE OF lrt_period,
         lrt_matnr     TYPE RANGE OF zfmacmatnr,
         lv_versao     TYPE zabs_del_ver_orc,
         lv_matnr_x    TYPE matnr,
         lv_total      TYPE char10,
         lv_recipe     TYPE fieldname,
         lv_field_md   TYPE fieldname,
         lv_field_mp   TYPE fieldname,
         lv_field_p    TYPE fieldname,
         lv_tabix_c    TYPE char2,
         lv_period     TYPE char6,
         lv_matnr      TYPE matnr,
         lv_werks      TYPE werks_d,
         lv_acnum      TYPE zfmacnum,
         lv_extwg      TYPE extwg,
         lv_matkl      TYPE matkl,
         lv_begda      TYPE begda,
         lv_endda      TYPE begda.

  CONSTANTS: BEGIN OF lc_tipo,
               formacao   LIKE lv_tipo VALUE 'FORMAÇÃO',
               manutencao LIKE lv_tipo VALUE 'MANUTENÇÃO',
             END OF lc_tipo.

  lo_filter = io_tech_request_context->get_filter( ).
  lt_filter = lo_filter->get_filter_select_options( ).
  lv_filter_str = lo_filter->get_filter_string( ).

*--Maps filter table lines to function module parameters
  LOOP AT lt_filter INTO ls_filter.
    CASE ls_filter-property.
      WHEN 'MATNR'.
        lv_matnr = ls_filter-select_options[ 1 ]-low.
      WHEN 'BEGDA'.
        lv_begda = ls_filter-select_options[ 1 ]-low.
      WHEN 'ENDDA'.
        lv_endda = ls_filter-select_options[ 1 ]-low.
      WHEN 'WERKS'.
        lv_werks = ls_filter-select_options[ 1 ]-low.
      WHEN 'ACNUM'.
        lv_acnum = ls_filter-select_options[ 1 ]-low.
      WHEN 'EXTWG'.
        lv_extwg = ls_filter-select_options[ 1 ]-low.
      WHEN 'MATKL'.
        lv_matkl = ls_filter-select_options[ 1 ]-low.
      WHEN 'VERSAO'.
        lv_versao = ls_filter-select_options[ 1 ]-low.
    ENDCASE.
  ENDLOOP.

  IF lv_begda IS NOT INITIAL.
    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                      = lv_begda
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS                    = 2.

    IF sy-subrc <> 0.
      CLEAR lv_begda.
    ENDIF.
  ENDIF.

  IF lv_endda IS NOT INITIAL.
    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                      = lv_endda
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS                    = 2.

    IF sy-subrc <> 0.
      CLEAR lv_endda.
    ENDIF.
  ENDIF.

  IF lv_begda IS INITIAL
  OR lv_endda IS INITIAL.
    DATA(lv_error) = abap_true.
  ELSE.
    lv_error = abap_false.
  ENDIF.

  IF lv_error EQ abap_false.
    CALL FUNCTION '/SCWM/DATES_BETWEEN_TWO_DATES'
      EXPORTING
        iv_begda = lv_begda
        iv_endda = lv_endda
      IMPORTING
        et_dates = lt_dates.

    lrs_period = 'IEQ'.
    LOOP AT lt_dates INTO DATA(ls_date).
      IF lv_period NE ls_date(6).
        lrs_period-low = ls_period = lv_period = ls_date(6).
        APPEND: ls_period TO lt_period,
                lrs_period TO lrt_period.
      ENDIF.
    ENDLOOP.

    SELECT rctyp, orcamento
      INTO TABLE @DATA(lt_rctyp)
      FROM ztfmrctyp
     WHERE orcamento EQ @abap_true.

    IF sy-subrc EQ 0.
      lv_matnr_x = 'TMAN' && lv_matnr+4(36).
      INSERT INITIAL LINE INTO TABLE lrt_matnr
        ASSIGNING FIELD-SYMBOL(<lrs_matnr>).
      IF sy-subrc EQ 0.
        <lrs_matnr> = 'IEQ'.
        <lrs_matnr>-low = lv_matnr_x.
      ENDIF.

      lv_matnr_x = 'TFOR' && lv_matnr+4(36).
      INSERT INITIAL LINE INTO TABLE lrt_matnr ASSIGNING <lrs_matnr>.
      IF sy-subrc EQ 0.
        <lrs_matnr> = 'IEQ'.
        <lrs_matnr>-low = lv_matnr_x.
      ENDIF.

      lv_matnr_x = 'TIMP' && lv_matnr+4(36).
      INSERT INITIAL LINE INTO TABLE lrt_matnr ASSIGNING <lrs_matnr>.
      IF sy-subrc EQ 0.
        <lrs_matnr> = 'IEQ'.
        <lrs_matnr>-low = lv_matnr_x.
      ENDIF.

      SELECT *
        INTO TABLE @DATA(lt_rchdr)
        FROM zfmrchdr
        FOR ALL ENTRIES IN @lt_rctyp
       WHERE werks EQ @lv_werks
         AND matnr IN @lrt_matnr[]
         AND datuv GE @lv_begda
         AND rctyp EQ @lt_rctyp-rctyp
         AND datbi LE @lv_endda.

      IF sy-subrc EQ 0.
        SELECT matnr, matkl
          FROM mara
          INTO TABLE @DATA(lt_mara)
          FOR ALL ENTRIES IN @lt_rchdr
         WHERE matnr = @lt_rchdr-matnr
           AND matkl = @lv_matkl.

        SORT lt_mara BY matnr.

        DELETE lt_rchdr WHERE matnr(4) NE 'TFOR'
                          AND matnr(4) NE 'TMAN'
                          AND matnr(4) NE 'TIMP'.

        IF lt_rchdr[] IS NOT INITIAL.
          SORT lt_rchdr BY matnr rcnum.

          LOOP AT lt_rchdr ASSIGNING FIELD-SYMBOL(<ls_rchdr>).
            IF <ls_rchdr>-matnr(4) EQ 'TFOR'
            OR <ls_rchdr>-matnr(4) EQ 'TIMP'.
              <ls_rchdr>-text1 = lc_tipo-formacao.
            ELSEIF <ls_rchdr>-matnr(4) EQ 'TMAN'.
              <ls_rchdr>-text1 = lc_tipo-manutencao.
            ENDIF.
          ENDLOOP.

          SELECT *
            INTO TABLE @DATA(lt_orcamento)
            FROM zabs_orcamento
            FOR ALL ENTRIES IN @lt_rchdr
           WHERE acnum  EQ @lv_acnum
             AND extwg  EQ @lv_extwg
             AND matkl  EQ @lv_matkl
             AND rcnum  EQ @lt_rchdr-rcnum
             AND period IN @lrt_period[]
             AND versao EQ @lv_versao.

          SORT lt_orcamento BY rcnum period matnr.

          SELECT *
            INTO TABLE @DATA(lt_zfmrclst)
            FROM zfmrclst
            FOR ALL ENTRIES IN @lt_rchdr
           WHERE rcnum EQ @lt_rchdr-rcnum
             AND werks EQ @lt_rchdr-werks
             AND matnr EQ @lt_rchdr-matnr.

          IF sy-subrc EQ 0.
            SORT lt_zfmrclst BY rcnum werks matnr posnr.

            DATA(lt_rchdrx) = lt_rchdr[].
            LOOP AT lt_rchdr INTO DATA(ls_rchdr).
              READ TABLE lt_mara TRANSPORTING NO FIELDS
                WITH KEY matnr = ls_rchdr-matnr BINARY SEARCH.
              IF sy-subrc NE 0.
                CONTINUE.
              ENDIF.

              READ TABLE lt_zfmrclst TRANSPORTING NO FIELDS
                WITH KEY rcnum = ls_rchdr-rcnum
                         werks = ls_rchdr-werks
                         matnr = ls_rchdr-matnr BINARY SEARCH.

              IF sy-subrc EQ 0.
                LOOP AT lt_zfmrclst INTO DATA(ls_zfmrclst) FROM sy-tabix.
                  IF ls_zfmrclst-rcnum NE ls_rchdr-rcnum
                  OR ls_zfmrclst-werks NE ls_rchdr-werks
                  OR ls_zfmrclst-matnr NE ls_rchdr-matnr.
                    EXIT.
                  ENDIF.

                  INSERT INITIAL LINE INTO TABLE et_entityset
                    ASSIGNING FIELD-SYMBOL(<fs_entityset>).
                  IF sy-subrc EQ 0.
                    IF ls_rchdr-matnr(4) EQ 'TFOR'
                    OR ls_rchdr-matnr(4) EQ 'TIMP'.
                      <fs_entityset>-tipo = lc_tipo-formacao.
                    ELSEIF ls_rchdr-matnr(4) EQ 'TMAN'.
                      <fs_entityset>-tipo = lc_tipo-manutencao.
                    ENDIF.

                    <fs_entityset>-rcnum1 = ls_rchdr-rcnum.

                    LOOP AT lt_period INTO ls_period.
                      DATA(lv_tabix) = sy-tabix.
                      lv_tabix_c = lv_tabix.

                      READ TABLE lt_orcamento INTO DATA(ls_orcamento)
                        WITH KEY rcnum  = ls_rchdr-rcnum
                                 period = ls_period
                                 matnr  = ls_zfmrclst-matnr_ins BINARY SEARCH.
                      IF sy-subrc NE 0.
                        CLEAR ls_orcamento.
                      ENDIF.

                      CONCATENATE: 'M' lv_tabix_c 'D' INTO lv_field_md,
                                   'M' lv_tabix_c 'P' INTO lv_field_mp,
                                   'P' lv_tabix_c INTO lv_field_p.

                      DATA(lv_field_exists) = abap_true.
                      ASSIGN COMPONENT lv_field_md OF STRUCTURE <fs_entityset> TO FIELD-SYMBOL(<lv_field_md>).
                      IF sy-subrc NE 0.
                        lv_field_exists = abap_false.
                      ENDIF.

                      ASSIGN COMPONENT lv_field_mp OF STRUCTURE <fs_entityset> TO FIELD-SYMBOL(<lv_field_mp>).
                      IF sy-subrc NE 0.
                        lv_field_exists = abap_false.
                      ENDIF.

                      ASSIGN COMPONENT lv_field_p OF STRUCTURE <fs_entityset> TO FIELD-SYMBOL(<lv_field_p>).
                      IF sy-subrc NE 0.
                        lv_field_exists = abap_false.
                      ENDIF.

                      IF lv_field_exists = abap_false.
                        EXIT.
                      ENDIF.

                      IF ls_orcamento-rcdos GT 0.
                        <lv_field_md> = ls_orcamento-rcdos.
                      ELSE.
                        <lv_field_md> = ls_zfmrclst-rcdos.
                      ENDIF.

                      <lv_field_mp> = ls_orcamento-passadas.
                      <lv_field_p> = ls_orcamento-produtos.
                    ENDLOOP.

                    <fs_entityset>-maktx     = ls_zfmrclst-maktx.
                    <fs_entityset>-matnrins  = ls_zfmrclst-matnr_ins.
                    <fs_entityset>-matnr     = ls_zfmrclst-matnr.
                    <fs_entityset>-principal = ls_zfmrclst-rcinp_check.
                  ENDIF.
                ENDLOOP.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMETHOD.


  METHOD checkterrainset_get_entityset.

*-- Local Declarations
    DATA: lrt_tplnr_fl   TYPE /iwbep/t_cod_select_options,
          lrt_terrains   LIKE lrt_tplnr_fl,
          lrs_tplnr_fl   LIKE LINE OF lrt_tplnr_fl,
          lv_terreno     TYPE /agri/gltplnr_fl,
          ls_prog_talhao TYPE zfmprog_talhao,
          lv_acnum       TYPE zfmacnum,
          lv_extwg       TYPE extwg,
          lv_begda       TYPE begda,
          lv_data        TYPE sy-datum,
          lv_rcnum       TYPE string,
          lv_endda       TYPE begda,
          lv_matkl       TYPE matkl.

    LOOP AT it_filter_select_options INTO DATA(ls_filters).
      CASE ls_filters-property.
        WHEN 'Acnum'.
          lv_acnum = ls_filters-select_options[ 1 ]-low.
        WHEN 'Matkl'.
          lv_matkl = ls_filters-select_options[ 1 ]-low.
        WHEN 'Extwg'.
          lv_extwg = ls_filters-select_options[ 1 ]-low.
        WHEN 'Rcnum'.
          lv_rcnum = ls_filters-select_options[ 1 ]-low.
        WHEN 'Data'.
          lv_data = ls_filters-select_options[ 1 ]-low.
        WHEN 'TplnrFl'.
          lrt_tplnr_fl = ls_filters-select_options.
      ENDCASE.
    ENDLOOP.

    lrt_terrains[] = lrt_tplnr_fl[].
    DATA(lv_lines) = lines( lrt_terrains ).
    DO lv_lines TIMES.
      READ TABLE lrt_terrains INTO DATA(lrs_terrain) INDEX sy-index.
      IF sy-subrc EQ 0.
        lrs_terrain-sign   = lrs_terrain-sign.
        lrs_terrain-option = lrs_terrain-option.
        lv_terreno          = lrs_terrain-low.
        CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
          EXPORTING
            input      = lv_terreno
          IMPORTING
            output     = lv_terreno
          EXCEPTIONS
            not_found  = 1
            not_active = 2
            OTHERS     = 3.
        lrs_terrain-low = lv_terreno.
        APPEND lrs_terrain TO lrt_terrains.
      ENDIF.
    ENDDO.

    SELECT *
      INTO TABLE @DATA(lt_prog_talhao)
      FROM zfmprog_talhao
     WHERE acnum    EQ @lv_acnum
       AND extwg    EQ @lv_extwg
       AND matkl    EQ @lv_matkl
       AND tplnr_fl IN @lrt_terrains[]
       AND data     EQ @lv_data.

    DELETE lt_prog_talhao WHERE aufnr IS NOT INITIAL.
    SORT lt_prog_talhao BY tplnr_fl.

    LOOP AT lrt_tplnr_fl INTO lrs_tplnr_fl.
      READ TABLE lt_prog_talhao INTO ls_prog_talhao
        WITH KEY tplnr_fl = lrs_tplnr_fl-low BINARY SEARCH.
      IF sy-subrc NE 0.
        INSERT INITIAL LINE INTO TABLE et_entityset
          ASSIGNING FIELD-SYMBOL(<ls_entityset>).
        IF sy-subrc EQ 0.
*-- Grave o terreno &1 na tela anterior.
          MESSAGE ID 'ZFMFP' TYPE 'E' NUMBER '109'
            WITH lrs_tplnr_fl-low INTO sy-msgli.
          <ls_entityset>-descr = sy-msgli.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF et_entityset[] IS INITIAL.
      INSERT INITIAL LINE INTO TABLE et_entityset
        ASSIGNING <ls_entityset>.
    ENDIF.

  ENDMETHOD.


  METHOD consultarestoque_get_entityset.

    DATA : lv_rcnum         TYPE zfmrcnum,
           ls_entityset     LIKE LINE OF et_entityset,
           ls_list          TYPE bapi_mrp_list,
           ls_control_param TYPE bapi_mrp_control_param,
           ls_stock_detail  TYPE bapi_mrp_stock_detail,
           ls_return        TYPE bapiret2,
           lv_material      TYPE bapi_mrp_mat_param-material,
           lv_plant         TYPE bapi_mrp_mat_param-plant,
           lv_tabix         TYPE sy-tabix,
           lv_werks         TYPE werks.


    DATA: r_werks TYPE /iwbep/t_cod_select_options.

    LOOP AT it_filter_select_options INTO DATA(ls_filters).
      CASE ls_filters-property.
        WHEN 'Rcnum'.
          lv_rcnum = ls_filters-select_options[ 1 ]-low.
        WHEN 'Werks'.
          r_werks = ls_filters-select_options.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

    TYPES: BEGIN OF ty_produtos,
             werks TYPE werks,
             matnr TYPE matnr,
             maktx TYPE maktx,
             stock TYPE sum01w,
           END OF   ty_produtos.

    DATA: ls_produtos TYPE ty_produtos,
          lt_produtos TYPE TABLE OF ty_produtos.

    SELECT *
      INTO TABLE @DATA(lt_lista)
      FROM zfmrclst
      WHERE rcnum EQ @lv_rcnum.
    IF sy-subrc EQ 0.
      LOOP AT lt_lista INTO DATA(ls_lista).
        ls_produtos-matnr = ls_lista-matnr_ins.
        ls_produtos-maktx = ls_lista-maktx.
        APPEND ls_produtos TO lt_produtos.
        CLEAR: ls_produtos.
      ENDLOOP.
    ENDIF.

    SELECT *
      INTO TABLE @DATA(lt_similar)
      FROM zfmrcsim
      WHERE rcnum EQ @lv_rcnum.
    IF sy-subrc EQ 0.
      LOOP AT lt_similar INTO DATA(ls_similar).
        ls_produtos-matnr = ls_similar-matnr_sim.
        ls_produtos-maktx = ls_similar-maktx_sim.
        APPEND ls_produtos TO lt_produtos.
        CLEAR: ls_produtos.
      ENDLOOP.
    ENDIF.

    LOOP AT lt_produtos INTO ls_produtos.
      LOOP AT r_werks INTO DATA(s_werks).

        lv_material = ls_produtos-matnr.
        lv_plant    = s_werks-low.

        CALL FUNCTION 'BAPI_MATERIAL_STOCK_REQ_LIST'
          EXPORTING
            material          = lv_material
            plant             = lv_plant
          IMPORTING
            mrp_list          = ls_list
            mrp_control_param = ls_control_param
            mrp_stock_detail  = ls_stock_detail
            return            = ls_return.

        ls_entityset-plnt_stock = ls_list-plnt_stock.
        ls_entityset-matnr      = ls_produtos-matnr.
        ls_entityset-maktx_ins  = ls_produtos-maktx.
        ls_entityset-werks      = s_werks-low.
        APPEND ls_entityset TO et_entityset.
        CLEAR: ls_entityset.
      ENDLOOP.
    ENDLOOP.

    SORT et_entityset BY werks matnr_ins.

  ENDMETHOD.


  METHOD createorderproce_get_entityset.

    DATA: lt_fmplitm      TYPE zt_fmplitm,
          lt_doc          TYPE /agri/t_fmfp_doc,
          lt_cskey        TYPE /agri/t_glcs_key,
          lt_cskeyx       TYPE /agri/t_glcs_key,
          lt_csdocx       TYPE /agri/t_glcs_doc,
          lt_fpdocx       TYPE /agri/t_fmfp_doc,
          lt_messagesx    TYPE /agri/t_gprolog,
          lt_messtabx     TYPE tab_bdcmsgcoll,
          lt_messages     TYPE /agri/t_gprolog,
          lwa_order_commx TYPE /agri/s_glpocomm,
          lwa_cskey       TYPE /agri/s_glcs_key,
          lwa_cskeyx      TYPE /agri/s_glcs_key,
          lwa_entityset   LIKE LINE OF et_entityset,
          lwa_csdocx      TYPE /agri/s_glcs_doc,
          lwa_messagex    TYPE /agri/s_gprolog,
          lwa_fmfpcordx   TYPE /agri/s_fmfpcord,
          lv_acnum        TYPE zfmacnum,
          lv_matkl        TYPE matkl,
          lv_ajahr        TYPE ajahr,
          lv_actyp        TYPE zfmactyp,
          lv_extwg        TYPE extwg,
          lv_subrcx       TYPE sy-subrc,
          lv_terreno      TYPE /agri/gltplnr_fl,
          lv_ordem        TYPE /agri/fmfpnum,
          lv_commit_workx TYPE abap_bool VALUE abap_true,
          lv_msgx         TYPE string.

    CONSTANTS: BEGIN OF c_document_category,
                 production_order TYPE /agri/gl_autyp VALUE 'AO',
                 task_order       TYPE /agri/gl_autyp VALUE 'TO',
                 produce_reciept  TYPE /agri/gl_autyp VALUE 'PR',
                 work_order       TYPE /agri/gl_autyp VALUE 'WO',
                 purchase_order   TYPE /agri/gl_autyp VALUE 'PO',
                 confirmation     TYPE /agri/gl_autyp VALUE 'OC',
                 reversals        TYPE /agri/gl_autyp VALUE 'CR',
                 measurements     TYPE /agri/gl_autyp VALUE 'MD',
               END OF c_document_category.

    CONSTANTS: BEGIN OF c_msg_type,
                 info    LIKE sy-msgty VALUE 'I',
                 warning LIKE sy-msgty VALUE 'W',
                 error   LIKE sy-msgty VALUE 'E',
                 abend   LIKE sy-msgty VALUE 'A',
                 success LIKE sy-msgty VALUE 'S',
                 x       LIKE sy-msgty VALUE 'X',
               END OF c_msg_type.

    LOOP AT it_filter_select_options INTO DATA(ls_filters).
      CASE ls_filters-property.
        WHEN 'Acnum'.
          lv_acnum = ls_filters-select_options[ 1 ]-low.
        WHEN 'Matkl'.
          lv_matkl = ls_filters-select_options[ 1 ]-low.
        WHEN 'Ajahr'.
          lv_ajahr = ls_filters-select_options[ 1 ]-low.
        WHEN 'Extwg'.
          lv_extwg = ls_filters-select_options[ 1 ]-low.
        WHEN 'Actyp'.
          lv_actyp = ls_filters-select_options[ 1 ]-low.
      ENDCASE.
    ENDLOOP.

    SELECT *
      INTO TABLE @DATA(lt_zfmachdr)
      FROM zfmachdr
      WHERE acnum EQ @lv_acnum
        AND ajahr EQ @lv_ajahr
        AND actyp EQ @lv_actyp.

    IF sy-subrc EQ 0.
      SELECT *
        INTO TABLE @DATA(lt_zfmaitm)
        FROM zfmaitm
        FOR ALL ENTRIES IN @lt_zfmachdr
       WHERE acnum EQ @lt_zfmachdr-acnum.

      IF sy-subrc EQ 0.
        SELECT *
          INTO TABLE @DATA(lt_glflcma)
          FROM /agri/glflcma
          FOR ALL ENTRIES IN @lt_zfmaitm
         WHERE tplnr_fl EQ @lt_zfmaitm-tplnr_fl
           AND loevm    NE @abap_true
           AND astat    EQ 'A'.

        IF lt_glflcma[] IS NOT INITIAL.
*-- Fetch Farm Process Order Item Details for Tasks
          SELECT *
            INTO TABLE @DATA(lt_glcsprs)
            FROM /agri/glcsprs
            FOR ALL ENTRIES IN @lt_glflcma
           WHERE tplnr_fl EQ @lt_glflcma-tplnr_fl
             AND contr    EQ @lt_glflcma-contr
             AND prprs    EQ @abap_false.

          IF sy-subrc EQ 0.
            SORT: lt_zfmachdr BY acnum actyp ajahr,
                  lt_zfmaitm  BY tplnr_fl,
                  lt_glflcma  BY tplnr_fl contr,
                  lt_glcsprs  BY tplnr_fl contr cpros.

            LOOP AT lt_glcsprs INTO DATA(ls_glcsprs).
              INSERT INITIAL LINE INTO TABLE lt_fmplitm
                ASSIGNING FIELD-SYMBOL(<ls_fmplitm>).
              IF sy-subrc EQ 0.
                MOVE-CORRESPONDING ls_glcsprs TO <ls_fmplitm>.
                READ TABLE lt_zfmaitm INTO DATA(ls_zfmaitm)
                  WITH KEY tplnr_fl = ls_glcsprs-tplnr_fl BINARY SEARCH.
                IF sy-subrc EQ 0.
                  <ls_fmplitm>-garea = ls_zfmaitm-aarea.
                  <ls_fmplitm>-pldil = ls_zfmaitm-datab.
                ENDIF.
              ENDIF.
            ENDLOOP.

            LOOP AT lt_fmplitm INTO DATA(lwa_fmplitm).

              REFRESH: lt_cskey, lt_messages.

              lwa_cskey-contr    = lwa_fmplitm-contr.
              lwa_cskey-tplnr_fl = lwa_fmplitm-tplnr_fl.
              APPEND lwa_cskey TO lt_cskey.

              CALL FUNCTION '/AGRI/FMFP_ORDER_CREATE'
                EXPORTING
                  i_save_messages       = abap_true
                  i_commit_work         = abap_true
                  i_cpros               = lwa_fmplitm-cpros
                  i_gstrp               = sy-datum
                  it_cskey              = lt_cskey
                IMPORTING
                  et_messages           = lt_messages
                EXCEPTIONS
                  inconsistent_data     = 1
                  no_valid_crop_seasons = 2
                  OTHERS                = 3.

              LOOP AT lt_messages INTO lwa_messagex.
                CLEAR: lwa_entityset, lv_msgx.
                CALL FUNCTION 'FORMAT_MESSAGE'
                  EXPORTING
                    id        = lwa_messagex-msgid
                    no        = lwa_messagex-msgno
                    v1        = lwa_messagex-msgv1
                    v2        = lwa_messagex-msgv2
                    v3        = lwa_messagex-msgv3
                    v4        = lwa_messagex-msgv4
                  IMPORTING
                    msg       = lv_msgx
                  EXCEPTIONS
                    not_found = 1
                    OTHERS    = 2.

                CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
                  EXPORTING
                    input  = lwa_fmplitm-tplnr_fl
                  IMPORTING
                    output = lwa_entityset-tplnrfl.

                CASE lwa_messagex-msgty.
                  WHEN 'E'.
                    lwa_entityset-status = lwa_messagex-msgty.
                  WHEN 'S'.
                    IF lwa_messagex-msgno EQ '009'.
                      lwa_entityset-status = 'W'.
                    ELSE.
                      lwa_entityset-status = 'S'.
                    ENDIF.
                  WHEN OTHERS.
                ENDCASE.

                lwa_entityset-descr  = lv_msgx.

                IF lwa_entityset-status EQ c_msg_type-success.
                  lwa_entityset-aufnr   = lwa_messagex-msgv1.
                ELSEIF lwa_entityset-status EQ c_msg_type-warning.
                  lwa_entityset-aufnr   = lwa_messagex-msgv3.
                ELSEIF   lwa_entityset-status EQ c_msg_type-error.

                ENDIF.
                APPEND lwa_entityset TO et_entityset.
              ENDLOOP.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD createordertasks_get_entityset.

    TYPES: BEGIN OF ty_resb_bt.
        INCLUDE TYPE resbb.
    TYPES: indold     TYPE syst_tabix.
    TYPES: no_req_upd TYPE syst_datar.
    TYPES: END OF ty_resb_bt.

    TYPES tt_resb_bt TYPE TABLE OF ty_resb_bt.

    TYPES: BEGIN OF ty_receita,
             werks     TYPE werks_d,
             acnum     TYPE zfmacnum,
             extwg     TYPE extwg,
             matkl     TYPE matkl,
             tplnr_fl  TYPE /agri/gltplnr_fl,
             data      TYPE begda,
             rcnum     TYPE zfmrcnum,
             matnr_ins TYPE zfmrcmatnr,
             matnr     TYPE matnr,
             objek     TYPE cuobn,
           END OF ty_receita,

           BEGIN OF ty_talhoes,
             tplnr_fl TYPE /agri/gltplnr_fl,
             imovel   TYPE yocotaseg-imov,
             talhao   TYPE yocotaseg-talhao,
           END OF ty_talhoes,


           BEGIN OF ty_ausp,
             objek          TYPE ausp-objek,
             atinn          TYPE ausp-atinn,
             atzhl          TYPE ausp-atzhl,
             mafid          TYPE ausp-mafid,
             klart          TYPE ausp-klart,
             adzhl          TYPE ausp-adzhl,
             dec_value_from TYPE ausp-dec_value_from, "
             dec_value_to   TYPE ausp-dec_value_to,
             data_aplic     TYPE sydatum,
             fim_carencia   TYPE sydatum,
           END OF ty_ausp.



    TYPES: BEGIN OF ty_cotaseg.
        INCLUDE TYPE yocotaseg.
    TYPES: tplnr_fl TYPE /agri/gltplnr_fl,
           END OF ty_cotaseg.

    FIELD-SYMBOLS: <ft_resb_bt> TYPE tt_resb_bt,
                   <fs_resb_bt> TYPE ty_resb_bt.

*-- Tables
    DATA: ref_upload           TYPE REF TO zcl_abs_glupload_master_data,
          lt_talhoes           TYPE STANDARD TABLE OF ty_talhoes INITIAL SIZE 0,
          lt_message           TYPE /agri/t_gprolog,
          lrt_tplnr_fl         TYPE /iwbep/t_cod_select_options,
          lrt_tplnr_aux        TYPE RANGE OF /agri/gltplnr_fl,
          lrt_rcnum            TYPE /iwbep/t_cod_select_options,
          lrt_imovel           TYPE RANGE OF yoimov,
          lrt_talhao           TYPE RANGE OF yotalhao,
          lrt_periodo          TYPE RANGE OF sydatum,
          lrt_data             TYPE /iwbep/t_cod_select_options,
          lt_ausp              TYPE TABLE OF ty_ausp INITIAL SIZE 0,
          lt_receita           TYPE TABLE OF ty_receita INITIAL SIZE 0,
          lt_mara              TYPE TABLE OF mara,
          lt_msg               TYPE /agri/t_gprolog,
          lt_msg_task_order    TYPE tab_bdcmsgcoll,
          lt_fpitm             TYPE /agri/t_fmfpitm,
          lt_fpdoc             TYPE /agri/t_fmfp_doc,
          lt_resbkeys          TYPE coxt_t_resbdel,
          lt_return            TYPE STANDARD TABLE OF bapiret2,
          lt_cotaseg           TYPE STANDARD TABLE OF ty_cotaseg,
          lt_aufnr             TYPE /agri/t_fmaufnr,
          lt_fmfpcom           TYPE TABLE OF /agri/fmfpcom,
          lrt_maktx            TYPE RANGE OF tvarvc-low,
          lrt_ltxa1            TYPE RANGE OF tvarvc-low,
          lrs_ltxa1            LIKE LINE OF lrt_ltxa1,
*-- Structures
          lwa_taskorder        TYPE /agri/s_fmfp_uptask_mass,
          lwa_order_comm       TYPE /agri/s_glpocomm,
          ls_receita           LIKE LINE OF lt_receita,
          ls_return            TYPE coxt_bapireturn,
          ls_csdoc             TYPE /agri/s_glcs_doc,
          ls_fpcord            TYPE /agri/s_fmfpcord,
          ls_fpitm             TYPE /agri/s_fmfpitm,
          ls_aufnr             LIKE LINE OF lt_aufnr,
          ls_ordens            TYPE zabs_ordens,
          ls_fmfpcom           LIKE LINE OF lt_fmfpcom,
          ls_storage_location  TYPE coxt_s_storage_location,
          ls_storage_locationx TYPE coxt_s_storage_locationx,
          ls_requ_quan         TYPE coxt_s_quantity,
*-- Variables
          lv_data_aplicacao    TYPE sydatum,
          lv_fim_carencia      TYPE sydatum,
*          lv_carencia          TYPE atinn,
          lv_dias_carencia     TYPE i,
          lv_imovel            TYPE yoimov,
          lv_talhao            TYPE yotalhao,
          lv_date              TYPE scdatum,
          lv_begda             TYPE scdatum,
          lv_endda             TYPE scdatum,
          lv_week              TYPE kweek,
          lv_week_aux          TYPE scal-week,
          lv_first_day         TYPE scal-date,
          lv_last_day          TYPE scal-date,
          lv_semana_cota       TYPE yosemana_cota,
          lv_error             TYPE flag,
          lv_rcnum             TYPE string,
          lv_acnum             TYPE zfmacnum,
          lv_matkl             TYPE matkl,
          lv_new_posnr         TYPE co_posnr,
          lv_ajahr             TYPE ajahr,
          lv_actyp             TYPE zfmactyp,
          lv_extwg             TYPE extwg,
          lv_data              TYPE begda,
          lv_string            TYPE string,
          lv_matnr             TYPE matnr,
          lv_message           TYPE string,
          lv_subrc             TYPE sysubrc,
          lv_tplnr_fl          TYPE string,
          lv_tplnr_in          TYPE /agri/gltplnr_fl,
          lv_operation         TYPE co_aplzl,
          lv_batch             TYPE coxt_batch,
          lv_batchx            TYPE coxt_batchx,
          lv_postp             TYPE postp,
          lv_sequence          TYPE plnfolge,
          lv_material          TYPE matnr,
          lv_aufnr             TYPE aufnr,
          lv_positionno        TYPE positionno,
          lv_numc              TYPE numc4,
          lv_rcnum_error       TYPE abap_bool VALUE abap_false,
          lv_posnr             TYPE /agri/fmfpcom-posnr.

    CONSTANTS: BEGIN OF c_order_level,
                 process      TYPE /agri/glordlv VALUE 'A',
                 process_task TYPE /agri/glordlv VALUE 'B',
               END OF c_order_level,

               BEGIN OF c_crop_season_status,
                 active   TYPE /agri/glastat VALUE 'A',
                 inactive TYPE /agri/glastat VALUE 'I',
                 closed   TYPE /agri/glastat VALUE 'C',
               END OF c_crop_season_status,

               BEGIN OF c_document_category,
                 production_order TYPE /agri/gl_autyp VALUE 'AO',
                 task_order       TYPE /agri/gl_autyp VALUE 'TO',
                 produce_reciept  TYPE /agri/gl_autyp VALUE 'PR',
                 work_order       TYPE /agri/gl_autyp VALUE 'WO',
                 purchase_order   TYPE /agri/gl_autyp VALUE 'PO',
                 confirmation     TYPE /agri/gl_autyp VALUE 'OC',
                 reversals        TYPE /agri/gl_autyp VALUE 'CR',
                 measurements     TYPE /agri/gl_autyp VALUE 'MD',
               END OF c_document_category,

               BEGIN OF c_msg_type,
                 info    TYPE sy-msgty VALUE 'I',
                 warning TYPE sy-msgty VALUE 'W',
                 error   TYPE sy-msgty VALUE 'E',
                 abend   TYPE sy-msgty VALUE 'A',
                 success TYPE sy-msgty VALUE 'S',
                 x       TYPE sy-msgty VALUE 'X',
               END OF c_msg_type,

               c_cl_material TYPE klassenart VALUE '001'.

    LOOP AT it_filter_select_options INTO DATA(ls_filters).
      CASE ls_filters-property.
        WHEN 'Acnum'.
          lv_acnum = ls_filters-select_options[ 1 ]-low.
        WHEN 'Matkl'.
          lv_matkl = ls_filters-select_options[ 1 ]-low.
        WHEN 'Ajahr'.
          lv_ajahr = ls_filters-select_options[ 1 ]-low.
        WHEN 'Extwg'.
          lv_extwg = ls_filters-select_options[ 1 ]-low.
        WHEN 'Actyp'.
          lv_actyp = ls_filters-select_options[ 1 ]-low.
        WHEN 'Rcnum'.
          lrt_rcnum = ls_filters-select_options.
        WHEN 'Data'.
          lrt_data = ls_filters-select_options.
        WHEN 'TplnrFl'.
          lrt_tplnr_fl = ls_filters-select_options.
      ENDCASE.
    ENDLOOP.

    LOOP AT lrt_rcnum ASSIGNING FIELD-SYMBOL(<lrs_rcnum>).
      IF <lrs_rcnum>-low EQ 'null'.
        CLEAR <lrs_rcnum>-low.
      ENDIF.
    ENDLOOP.

    IF lrt_tplnr_fl[] IS NOT INITIAL.
      INSERT INITIAL LINE INTO TABLE lrt_maktx
        ASSIGNING FIELD-SYMBOL(<lrs_matkx>).
      IF sy-subrc EQ 0.
        <lrs_matkx>-sign = 'I'.
        <lrs_matkx>-option = 'CP'.
        <lrs_matkx>-low = lv_matkl && '*'.
      ENDIF.

      LOOP AT lrt_tplnr_fl INTO DATA(lrs_tplnr_fl).
*--BOC-T_T.KONNO-07.27.21
        IF lrs_tplnr_fl-low IS NOT INITIAL.
          INSERT INITIAL LINE INTO TABLE lt_talhoes
            ASSIGNING FIELD-SYMBOL(<ls_talhao>).
          IF sy-subrc EQ 0.
            CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
              EXPORTING
                input      = lrs_tplnr_fl-low
              IMPORTING
                output     = <ls_talhao>-tplnr_fl
              EXCEPTIONS
                not_found  = 1
                not_active = 2
                OTHERS     = 3.

            <ls_talhao>-imovel = lrs_tplnr_fl-low(6).
            <ls_talhao>-talhao = lrs_tplnr_fl-low+7(5).
          ENDIF.
        ENDIF.
*--EOC-T_T.KONNO-07.27.21

        INSERT INITIAL LINE INTO TABLE lrt_imovel
          ASSIGNING FIELD-SYMBOL(<lrs_imovel>).
        IF sy-subrc EQ 0.
          <lrs_imovel> = 'IEQ'.
          <lrs_imovel>-low = lrs_tplnr_fl-low(6).
        ENDIF.

        INSERT INITIAL LINE INTO TABLE lrt_talhao
          ASSIGNING FIELD-SYMBOL(<lrs_talhao>).
        IF sy-subrc EQ 0.
          <lrs_talhao> = 'IEQ'.
          <lrs_talhao>-low = lrs_tplnr_fl-low+7(5).
        ENDIF.

        INSERT INITIAL LINE INTO TABLE lrt_tplnr_aux
          ASSIGNING FIELD-SYMBOL(<lrs_tplnr_aux>).
        IF sy-subrc EQ 0.
          <lrs_tplnr_aux> = 'IEQ'.
          <lrs_tplnr_aux>-low = lrs_tplnr_fl-low.
        ENDIF.

        INSERT INITIAL LINE INTO TABLE lrt_tplnr_aux
          ASSIGNING <lrs_tplnr_aux>.
        IF sy-subrc EQ 0.
          <lrs_tplnr_aux> = 'IEQ'.
          CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
            EXPORTING
              input      = lrs_tplnr_fl-low
            IMPORTING
              output     = <lrs_tplnr_aux>-low
            EXCEPTIONS
              not_found  = 1
              not_active = 2
              OTHERS     = 3.
        ENDIF.
      ENDLOOP.

      DATA(lrt_prescription) = lrt_rcnum[].
      DELETE lrt_prescription WHERE low IS INITIAL.

      IF lrt_prescription[] IS INITIAL.
        SELECT *
          INTO TABLE @DATA(lt_rec_hist)
          FROM zfmrchis
         WHERE acnum    EQ @lv_acnum
           AND extwg    EQ @lv_extwg
           AND matkl    EQ @lv_matkl
           AND tplnr_fl IN @lrt_tplnr_aux[]
           AND data     IN @lrt_data[].
      ELSE.
        SELECT *
          INTO TABLE @lt_rec_hist
          FROM zfmrchis
         WHERE acnum    EQ @lv_acnum
           AND extwg    EQ @lv_extwg
           AND matkl    EQ @lv_matkl
           AND tplnr_fl IN @lrt_tplnr_aux[]
           AND data     IN @lrt_data[]
           AND rcnum    IN @lrt_rcnum[].
      ENDIF.

      IF lt_rec_hist[] IS NOT INITIAL.
        SORT lt_rec_hist BY rcnum werks tplnr_fl.

        SELECT *
          FROM zfmrclst
          INTO TABLE @DATA(lt_rec_list)
          FOR ALL ENTRIES IN @lt_rec_hist
         WHERE rcnum EQ @lt_rec_hist-rcnum.

        SORT lt_rec_list BY rcnum.
      ENDIF.

      SELECT *
        INTO TABLE @DATA(lt_talhao_prog)
        FROM zfmprog_talhao
       WHERE acnum    EQ @lv_acnum
         AND extwg    EQ @lv_extwg
         AND matkl    EQ @lv_matkl
         AND tplnr_fl IN @lrt_tplnr_aux[]
         AND data     IN @lrt_data[]
         AND rcnum    IN @lrt_rcnum[].

      DATA(lt_talhao_aux) = lt_talhao_prog[].
      DELETE lt_talhao_prog WHERE aufnr IS NOT INITIAL.
      SORT lt_talhao_prog BY rcnum tplnr_fl data.

      SELECT matnr, matkl, extwg
        INTO TABLE @DATA(gt_mara)
        FROM mara
       WHERE matkl EQ @lv_matkl
         AND extwg EQ @lv_extwg
        ORDER BY matnr, matkl, extwg.

      IF sy-subrc EQ 0.
        SELECT *
          INTO TABLE @DATA(lt_volume)
          FROM zfmacvlcl
          FOR ALL ENTRIES IN @gt_mara
         WHERE acnum    EQ @lv_acnum
           AND matnr    EQ @gt_mara-matnr
           AND tplnr_fl IN @lrt_tplnr_aux[].
      ENDIF.

      IF lt_talhao_prog[] IS NOT INITIAL.
        SORT lt_volume BY tplnr_fl matnr.

        lv_data = lrt_data[ 1 ]-low.

        SELECT *
          INTO TABLE @DATA(lt_zfmachdr)
          FROM zfmachdr
         WHERE acnum EQ @lv_acnum
           AND ajahr EQ @lv_ajahr
           AND actyp EQ @lv_actyp.

        DATA(lv_rcnum_required) = abap_false.
        IF lt_zfmachdr[] IS NOT INITIAL.
          READ TABLE lt_volume INTO DATA(ls_volume) INDEX 1.
          IF sy-subrc EQ 0.
            READ TABLE lt_zfmachdr INTO DATA(ls_zfmachdr) INDEX 1.
            IF sy-subrc EQ 0.
              SELECT SINGLE matnr, maktx
                FROM zabst_rec_obrig
                INTO @DATA(ls_rec_obrig)
               WHERE matnr = @ls_volume-matnr.
              IF sy-subrc EQ 0.
                lv_rcnum_required = abap_true.
                IF lrt_prescription[] IS INITIAL.
                  INSERT INITIAL LINE INTO TABLE et_entityset
                    ASSIGNING FIELD-SYMBOL(<ls_entityset>).
                  IF sy-subrc EQ 0.
                    <ls_entityset>-status = 'E'.
*-- A operação &1 exige receita!
                    MESSAGE ID 'ZFMFP' TYPE c_msg_type-error NUMBER '248'
                      INTO sy-msgli WITH ls_volume-matnr.
                    <ls_entityset>-descr = sy-msgli.
                    lv_rcnum_error = abap_true.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.

          CHECK lv_rcnum_error = abap_false.

          SELECT *
            INTO TABLE @DATA(lt_zfmaitm)
            FROM zfmaitm
            FOR ALL ENTRIES IN @lt_zfmachdr
           WHERE acnum    EQ @lt_zfmachdr-acnum
             AND tplnr_fl IN @lrt_tplnr_aux[].

          IF lt_zfmaitm[] IS NOT INITIAL.
            SELECT *
              INTO TABLE @DATA(lt_glflcma)
              FROM /agri/glflcma
              FOR ALL ENTRIES IN @lt_zfmaitm
             WHERE tplnr_fl EQ @lt_zfmaitm-tplnr_fl
               AND contr    EQ @lt_zfmaitm-contr
               AND cmnum    EQ @lt_zfmaitm-cmnum
               AND astat    EQ @c_crop_season_status-active
               AND loevm    NE @abap_true.

*-- If there are several seasons for a terrain, consider the last one created
            SORT lt_glflcma BY tplnr_fl ASCENDING
                               contr    DESCENDING.

            DELETE ADJACENT DUPLICATES FROM lt_glflcma COMPARING tplnr_fl.

            IF lt_glflcma[] IS NOT INITIAL.
              SORT lt_glflcma BY tplnr_fl.
*-- Fetch Farm Process Order Item Details for Tasks
              SELECT *
                INTO TABLE @DATA(lt_glcsprs)
                FROM /agri/glcsprs
                FOR ALL ENTRIES IN @lt_glflcma
               WHERE tplnr_fl EQ @lt_glflcma-tplnr_fl
                 AND contr    EQ @lt_glflcma-contr
                 AND strdt    LE @lv_data
                 AND enddt    GE @lv_data
                 AND prprs    EQ @abap_false.

              IF sy-subrc EQ 0.
                SORT lt_glcsprs BY tplnr_fl contr cpros.
              ELSE.
                REFRESH lt_glcsprs.
              ENDIF.
            ELSE.
              INSERT INITIAL LINE INTO TABLE et_entityset ASSIGNING <ls_entityset>.
              IF sy-subrc EQ 0.
                READ TABLE lrt_tplnr_fl INTO lrs_tplnr_fl INDEX 1.
                IF sy-subrc EQ 0.
*-- Não existe safra válida para o talhão &1!
                  <ls_entityset>-status = 'E'.
                  MESSAGE ID 'ZFMFP' TYPE c_msg_type-error
                    NUMBER '249' WITH lrs_tplnr_fl-low INTO sy-msgli.
                  <ls_entityset>-descr = sy-msgli.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

        LOOP AT lt_rec_hist INTO DATA(ls_rec_hist).
          MOVE-CORRESPONDING ls_rec_hist TO ls_receita.
          CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
            EXPORTING
              input        = ls_receita-matnr_ins
            IMPORTING
              output       = ls_receita-matnr
            EXCEPTIONS
              length_error = 1
              OTHERS       = 2.

          ls_receita-objek = ls_receita-matnr.
          APPEND ls_receita TO lt_receita.
        ENDLOOP.

*-- BOC 10.16.20-T_T.KONNO
        READ TABLE lrt_data INTO DATA(lrs_data) INDEX 1.
        IF sy-subrc EQ 0.
          lv_date = lrs_data-low.
          CALL FUNCTION 'DATE_GET_WEEK'
            EXPORTING
              date         = lv_date
            IMPORTING
              week         = lv_week
            EXCEPTIONS
              date_invalid = 1
              OTHERS       = 2.

          IF sy-subrc EQ 0.
            lv_semana_cota = lv_week.

            SELECT *
              INTO TABLE lt_cotaseg
              FROM yocotaseg
             WHERE imov   IN lrt_imovel[]
               AND talhao IN lrt_talhao[]
               AND semana EQ lv_semana_cota.

            IF sy-subrc EQ 0.
* Adonis - 26.07.2021 - INC0030158
              lv_week_aux  = lv_semana_cota.

              CALL FUNCTION 'WEEK_GET_FIRST_DAY'
                EXPORTING
                  week         = lv_week_aux
                IMPORTING
                  date         = lv_first_day
                EXCEPTIONS
                  week_invalid = 1
                  OTHERS       = 2.
              IF sy-subrc EQ 0.


                LOOP AT lt_cotaseg ASSIGNING FIELD-SYMBOL(<ls_cotaseg_key>).

                  lv_imovel = <ls_cotaseg_key>-imov.
                  SHIFT lv_imovel LEFT DELETING LEADING ' '.
                  SHIFT lv_imovel LEFT DELETING LEADING '0'.

                  lv_talhao = <ls_cotaseg_key>-talhao.
                  SHIFT lv_talhao RIGHT DELETING TRAILING ' '.
                  "REPLACE ALL OCCURRENCES OF ' ' IN lv_talhao WITH '0'.

                  <ls_cotaseg_key>-tplnr_fl = lv_imovel && '-' && lv_talhao.
                  CONDENSE <ls_cotaseg_key>-tplnr_fl NO-GAPS.

                  CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
                    EXPORTING
                      input      = <ls_cotaseg_key>-tplnr_fl
                    IMPORTING
                      output     = <ls_cotaseg_key>-tplnr_fl
                    EXCEPTIONS
                      not_found  = 1
                      not_active = 2
                      OTHERS     = 3.


                ENDLOOP.


                lv_last_day = lv_first_day + 7.

                SELECT tplnr_fl, matnr , gstrp , tecom
                  INTO TABLE @DATA(lt_fmfphdr)
                  FROM /agri/fmfphdr
                  FOR ALL ENTRIES IN @lt_cotaseg
                 WHERE tplnr_fl  EQ @lt_cotaseg-tplnr_fl
                   AND matnr     EQ 'TCOL0001'
                   AND auart     EQ 'ZH10'
                   AND gstrp     BETWEEN @lv_first_day AND @lv_last_day.
                   "AND tecom     NE 'X'.

                IF sy-subrc NE 0.
                  "REFRESH lt_cotaseg.
                ELSE.
                  SORT lt_fmfphdr BY tplnr_fl matnr.
                ENDIF.

                LOOP AT lt_cotaseg INTO DATA(ls_cotaseg).

                  DATA(lv_index) = sy-tabix.

                  READ TABLE lt_fmfphdr into data(ls_fmfphdr) WITH KEY tplnr_fl  = ls_cotaseg-tplnr_fl.
                  IF sy-subrc = 0 AND ls_fmfphdr-tecom = 'X'.

                    DELETE lt_cotaseg INDEX lv_index.

                  ENDIF.

                ENDLOOP.

              ENDIF.
            ENDIF.

            SORT lt_cotaseg BY imov talhao.
          ENDIF.
        ENDIF.
*-- EOC 10.16.20-T_T.KONNO

*--BOC-T_T.KONNO-06.01.21
        DATA(lt_prescription) = lt_rec_hist[].
        DELETE lt_prescription WHERE loevm EQ abap_true.
*--EOC-T_T.KONNO-06.01.21

        LOOP AT lrt_tplnr_fl INTO lrs_tplnr_fl.
          DATA(lv_tabix) = sy-tabix.
*-- BOC 10.16.20-T_T.KONNO
          CLEAR lv_rcnum.
          READ TABLE lrt_rcnum INTO DATA(lrs_rcnum) INDEX lv_tabix.
          IF sy-subrc EQ 0.
            lv_rcnum = lrs_rcnum-low.
          ENDIF.

          lv_imovel = lrs_tplnr_fl-low(6).
          lv_talhao = lrs_tplnr_fl-low+7(5).
          IF lv_rcnum_required EQ abap_true.
*--BOC-T_T.KONNO-06.01.21
            IF lv_rcnum IS NOT INITIAL.
              READ TABLE lt_prescription INTO DATA(ls_prescription)
                WITH KEY acnum    = lv_acnum
                         tplnr_fl = lrs_tplnr_fl-low
                         rcnum    = lv_rcnum.
              IF sy-subrc NE 0.
                INSERT INITIAL LINE INTO TABLE et_entityset
                  ASSIGNING <ls_entityset>.
                IF sy-subrc EQ 0.
*-- Sem insumos na receita &1 talhão &2!
                  MESSAGE ID 'ZFMFP' TYPE c_msg_type-error NUMBER '414'
                    INTO sy-msgli WITH lv_rcnum lrs_tplnr_fl-low.
                  <ls_entityset>-tplnr_fl = lrs_tplnr_fl-low.
                  <ls_entityset>-data = lv_data+6(2) && '.' && lv_data+4(2) && '.' && lv_data(4).
                  <ls_entityset>-status = 'E'.
                  <ls_entityset>-descr = sy-msgli.
                  CONTINUE.
                ENDIF.
              ENDIF.
            ENDIF.
*--EOC-T_T.KONNO-06.01.21
            READ TABLE lt_cotaseg TRANSPORTING NO FIELDS
              WITH KEY imov   = lv_imovel
                       talhao = lv_talhao BINARY SEARCH.
            IF sy-subrc EQ 0.
              INSERT INITIAL LINE INTO TABLE et_entityset ASSIGNING <ls_entityset>.
              IF sy-subrc EQ 0.
*-- O talhão &1 está programado para colheita nessa semana!
                <ls_entityset>-status = 'E'.
                MESSAGE ID 'ZFMFP' TYPE c_msg_type-error
                  NUMBER '274' WITH lrs_tplnr_fl-low INTO sy-msgli.
                <ls_entityset>-descr = sy-msgli.
                CONTINUE.
              ENDIF.
            ENDIF.
          ENDIF.
*-- EOC 10.16.20-T_T.KONNO

          CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
            EXPORTING
              input      = lrs_tplnr_fl-low
            IMPORTING
              output     = lv_tplnr_in
            EXCEPTIONS
              not_found  = 1
              not_active = 2
              OTHERS     = 3.

*-- Production Order
          READ TABLE lt_glcsprs INTO DATA(ls_glcsprs)
            WITH KEY tplnr_fl = lv_tplnr_in.
          IF sy-subrc EQ 0.
            SELECT *
              FROM /agri/glcsdfl
              INTO TABLE @DATA(lt_dflow)
             WHERE tplnr_fl EQ @ls_glcsprs-tplnr_fl
               AND contr    EQ @ls_glcsprs-contr
               AND autyp    EQ @c_document_category-production_order.

            IF sy-subrc EQ 0.
              SELECT * FROM /agri/fmfphdr
               INTO TABLE @DATA(lt_fphdr)
               FOR ALL ENTRIES IN @lt_dflow
              WHERE aufnr EQ @lt_dflow-aufnr
                AND cpros EQ @ls_glcsprs-cpros
                AND tecom EQ @space.

              IF sy-subrc EQ 0.
                DATA(lv_ord) = abap_true.
              ELSE.
                SELECT * FROM /agri/fmfphdr
                  INTO TABLE lt_fphdr
                  FOR ALL ENTRIES IN lt_dflow
                 WHERE aufnr EQ lt_dflow-aufnr
                   AND tecom EQ space.
                IF sy-subrc EQ 0.
                  lv_ord = abap_true.
                  DATA(lv_crk) = abap_true.
                ENDIF.
              ENDIF.

              IF lv_ord EQ abap_true.
                SORT lt_fphdr BY aufnr DESCENDING.
                READ TABLE lt_fphdr INTO DATA(ls_fphdr) INDEX 1.
                IF sy-subrc EQ 0.
                  lv_string = lv_matkl && '*'.
                  INSERT INITIAL LINE INTO TABLE lrt_ltxa1
                    ASSIGNING FIELD-SYMBOL(<lrs_ltxa1>).
                  IF sy-subrc EQ 0.
                    <lrs_ltxa1>-sign = 'I'.
                    <lrs_ltxa1>-option = 'CP'.
                    <lrs_ltxa1>-low = lv_string.
                  ENDIF.

                  SELECT *
                    INTO TABLE @DATA(lt_items)
                    FROM /agri/fmfpitm
                   WHERE aufnr EQ @ls_fphdr-aufnr
                     AND ltxa1 IN @lrt_ltxa1[].

                  IF sy-subrc EQ 0.
                    READ TABLE lt_items INTO DATA(ls_items) INDEX 1.
                  ENDIF.
                ENDIF.
              ELSE.
                INSERT INITIAL LINE INTO TABLE et_entityset
                  ASSIGNING <ls_entityset>.
                IF sy-subrc EQ 0.
                  MESSAGE ID '/AGRI/FMFP'  TYPE c_msg_type-error
                    NUMBER '072' WITH ls_glcsprs-cpros INTO sy-msgli.

                  CLEAR lv_message.
                  CALL FUNCTION 'FORMAT_MESSAGE'
                    EXPORTING
                      id        = '/AGRI/FMFP'
                      no        = '072'
                      v1        = ls_glcsprs-cpros
                    IMPORTING
                      msg       = lv_message
                    EXCEPTIONS
                      not_found = 1
                      OTHERS    = 2.

                  CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
                    EXPORTING
                      input  = ls_glcsprs-tplnr_fl
                    IMPORTING
                      output = <ls_entityset>-tplnr_fl.

                  <ls_entityset>-data   = lv_data+6(2) && '.' && lv_data+4(2) && '.' && lv_data(4).
                  <ls_entityset>-status = 'E'.
                  <ls_entityset>-descr  = lv_message.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.

          READ TABLE lt_glflcma INTO DATA(ls_glflcma)
            WITH KEY tplnr_fl = ls_glcsprs-tplnr_fl BINARY SEARCH.

          IF sy-subrc EQ 0.
**Modify -- Added Start INC0023214 on 18/09/2020 T_C.KARANAM
            IF lv_crk = abap_true.
              READ TABLE lt_glcsprs INTO DATA(ls_glcsprs_1)
                WITH KEY tplnr_fl = lv_tplnr_in
                         cpros = ls_fphdr-cpros.
              lv_matnr = 'T' && ls_glcsprs_1-matnr+1(3) && '*'.
**Modify -- Added End INC0023214 on 18/09/2020 T_C.KARANAM
            ELSE.
              lv_matnr = 'T' && ls_glcsprs-matnr+1(3) && '*'.
            ENDIF.
            LOOP AT gt_mara INTO DATA(ls_mara) WHERE matnr CP lv_matnr.
              EXIT.
            ENDLOOP.

            REFRESH: lt_fpitm, lt_msg.
            ls_fpitm-aufnr = ls_fphdr-aufnr.
            IF ls_items-vornr IS NOT INITIAL.
              ls_fpitm-vornr = ls_items-vornr.
            ENDIF.

            READ TABLE lt_volume INTO ls_volume
              WITH KEY tplnr_fl = ls_glcsprs-tplnr_fl
                       matnr    = ls_mara-matnr BINARY SEARCH.
            IF sy-subrc EQ 0.
              IF ls_volume-acarx IS NOT INITIAL AND ls_volume-acarx <> 0.
                ls_fpitm-tomng = ls_volume-acarx.
                ls_fpitm-gamng = ls_volume-acarx.
              ELSE.
                ls_fpitm-tomng = ls_volume-acqtb.
                ls_fpitm-gamng = ls_volume-acqtb.
              ENDIF.
            ENDIF.

            ls_fpitm-matnr    = ls_mara-matnr.
            ls_fpitm-verid    = '0001'.
            ls_fpitm-actdt    = lv_data.
            ls_fpitm-schdt    = lv_data.
            ls_fpitm-updkz    = 'I'.
            ls_fpitm-schdl    = abap_true.
            ls_fpitm-meinh    = ls_glflcma-msehi.
            ls_fpitm-confm    = abap_true.
            APPEND ls_fpitm TO lt_fpitm.

            REFRESH: lt_aufnr, lt_fpdoc, lt_msg_task_order.
            ls_aufnr-aufnr = ls_fpitm-aufnr.
            APPEND ls_aufnr TO lt_aufnr.

            CALL FUNCTION '/AGRI/FMFP_VIEW'
              EXPORTING
                it_aufnr       = lt_aufnr
              IMPORTING
                et_fpdoc       = lt_fpdoc
              EXCEPTIONS
                no_data_exists = 1
                OTHERS         = 2.

            READ TABLE lt_fpdoc INTO DATA(ls_fpdoc) INDEX 1.
            IF sy-subrc EQ 0.
              lv_tabix = sy-tabix.
              DATA(lv_lines) = lines( ls_fpdoc-x-fpitm ).
              lv_lines = lv_lines + 1.
              lv_string = lv_matkl && '*'.

              lv_new_posnr = lv_lines.
              DO 20 TIMES. "DO 5 TIMES.
                lv_new_posnr = lv_lines.
                READ TABLE ls_fpdoc-x-fpcom TRANSPORTING NO FIELDS
                  WITH KEY posnr = lv_new_posnr.
                IF sy-subrc EQ 0.
                  ADD 1 TO lv_lines.
                  CONTINUE.
                ENDIF.

                READ TABLE ls_fpdoc-x-fpitm TRANSPORTING NO FIELDS
                  WITH KEY posnr = lv_new_posnr.
                IF sy-subrc EQ 0.
                  ADD 1 TO lv_lines.
                  CONTINUE.
                ENDIF.
                EXIT.
              ENDDO.

              READ TABLE ls_fpdoc-x-fpcom INTO DATA(ls_fpcom)
                WITH KEY matnr = ls_mara-matnr.
              IF sy-subrc EQ 0.
                DATA(lv_posnr_x) = ls_fpcom-posnr.
                ls_fpcom-posnr = lv_lines.

                DATA(lv_itm_found) = abap_false.
                READ TABLE ls_fpdoc-x-fpitm INTO DATA(ls_fpitmx)
                  WITH KEY posnr = lv_posnr_x.
                IF sy-subrc EQ 0.
                  IF ls_fpitmx-ltxa1 CP lv_string.
                    lv_itm_found = abap_true.
                  ENDIF.
                ENDIF.
                IF lv_itm_found EQ abap_false.
                  LOOP AT ls_fpdoc-x-fpitm INTO ls_fpitmx
                    WHERE ltxa1 CP lv_string.
                    lv_itm_found = abap_true.
                    EXIT.
                  ENDLOOP.
                ENDIF.
                IF lv_itm_found = abap_true.
                  IF lv_lines LE 999.
                    ls_fpitm-vornr = lv_lines * 10.
                  ELSE.
                    ls_fpitm-vornr = lv_lines.
                  ENDIF.
                  ls_fpcom-vornr = ls_fpitm-vornr.
                  ls_fpitm-ltxa1 = ls_fpitmx-ltxa1.
                  ls_fpitm-posnr = lv_lines.
                ENDIF.
              ENDIF.

              MOVE-CORRESPONDING ls_fpitm TO ls_fpcord.
              ls_fpcord-matnr = ls_fpcom-matnr.
              ls_fpcord-gamng = ls_fpitm-tomng.
              ls_fpcord-gmein = ls_fpitm-meinh.
              ls_fpcord-gstrp = ls_fpitm-actdt.
              ls_fpcord-grcre = ls_fpdoc-x-fphdr-grcre.
              ls_fpcord-ordlv = c_order_level-process.

              CLEAR: ls_fpcord-aufnr, ls_fpcord-schta.

              ls_fpcord-autyp = c_document_category-task_order.

              MOVE-CORRESPONDING ls_fpcord TO lwa_order_comm.
              CLEAR: lwa_order_comm-auart.

              lwa_order_comm-iwerk    = ls_fphdr-iwerk.
              lwa_order_comm-tplnr_fl = ls_fphdr-tplnr_fl.
              lwa_order_comm-datab    = lv_data.
              lwa_order_comm-cmnum    = ls_fphdr-cmnum.

              CALL METHOD /agri/cl_glco_process=>order_create
                EXPORTING
                  is_order_comm = lwa_order_comm
                IMPORTING
                  e_aufnr       = ls_fpcord-aufnr
                  et_messages   = lt_msg_task_order.

              IF ls_fpcord-aufnr IS NOT INITIAL.
                ls_fpitm-aufnr_to = ls_fpcord-aufnr.
                ls_fpcom-updkz    = 'I'.
                APPEND ls_fpitm TO ls_fpdoc-x-fpitm.
                APPEND ls_fpcom TO ls_fpdoc-x-fpcom.
                MODIFY lt_fpdoc FROM ls_fpdoc INDEX lv_tabix.

                CALL FUNCTION '/AGRI/FMFP_SAVE'
                  EXPORTING
                    i_commit_work = 'X'
                  CHANGING
                    ct_fpdoc      = lt_fpdoc
                    ct_messages   = lt_msg
                  EXCEPTIONS
                    no_change     = 1
                    OTHERS        = 2.

                CLEAR: lt_fpdoc.
                MOVE-CORRESPONDING ls_fpdoc-x-fphdr TO ls_fpcord.
                ls_fpcord-ordlv = 'A'.
                ls_fpcord-aufnr = ls_fpitm-aufnr_to.
                ls_fpcord-autyp = c_document_category-task_order.
                PERFORM document_infocus_prepare
                  IN PROGRAM /agri/saplfmfpm USING ls_fpcord
                                                   ls_csdoc
                                          CHANGING lt_fpdoc IF FOUND.

                READ TABLE lt_fpdoc INTO DATA(ls_fpdoc_to) INDEX 1.
                IF sy-subrc EQ 0.
                  ls_fpdoc_to-x-fphdr-posnr_v = ls_fpitm-posnr.
                  ls_fpdoc_to-x-fphdr-equnr   = ls_fpitm-equnr.
                  ls_fpdoc_to-x-fphdr-gangcd  = ls_fpitm-gangcd.
                  IF ls_fpitm-unpln IS NOT INITIAL.
                    ls_fpdoc_to-x-fphdr-gamng = ls_fpcord-gamng.
                  ENDIF.

                  PERFORM document_infocus_save IN PROGRAM /agri/saplfmfpm
                                                     USING abap_true
                                                  CHANGING ls_fpdoc_to
                                                           lv_subrc IF FOUND.
                ENDIF.

                IF lt_msg[] IS INITIAL.
                  UNASSIGN <ls_entityset>.
                  INSERT INITIAL LINE INTO TABLE et_entityset ASSIGNING <ls_entityset>.
                  IF sy-subrc EQ 0.
                    CALL FUNCTION 'FORMAT_MESSAGE'
                      EXPORTING
                        id        = 'CO'
                        no        = '100'
                        v1        = ls_fpcord-aufnr
                      IMPORTING
                        msg       = lv_message
                      EXCEPTIONS
                        not_found = 1
                        OTHERS    = 2.

                    CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
                      EXPORTING
                        input  = ls_glcsprs-tplnr_fl
                      IMPORTING
                        output = <ls_entityset>-tplnr_fl.

                    <ls_entityset>-data   = lv_data+6(2) && '.' && lv_data+4(2) && '.' && lv_data(4).
                    <ls_entityset>-status = 'S'.
                    <ls_entityset>-descr  = lv_message.
                    <ls_entityset>-aufnr  = ls_fpcord-aufnr.
                  ENDIF.

                  READ TABLE lt_talhao_prog INTO DATA(ls_prog)
                    WITH KEY rcnum    = lv_rcnum
                             tplnr_fl = lrs_tplnr_fl-low
                             data     = lv_data BINARY SEARCH.

                  IF sy-subrc EQ 0.
                    ls_prog-aufnr = ls_fpcord-aufnr.
                    UPDATE zfmprog_talhao SET aufnr = ls_prog-aufnr
                      WHERE acnum    = ls_prog-acnum
                        AND extwg    = ls_prog-extwg
                        AND matkl    = ls_prog-matkl
                        AND tplnr_fl = ls_prog-tplnr_fl
                        AND data     = ls_prog-data
                        AND aufnr    = lv_aufnr.
                  ENDIF.

                  IF <ls_entityset> IS ASSIGNED.
                    lv_tplnr_fl = <ls_entityset>-tplnr_fl.
                  ENDIF.
                  ls_ordens-aufnr    = ls_fpcord-aufnr.
                  ls_ordens-tplnr_fl = ls_glcsprs-tplnr_fl.
                  ls_ordens-data     = lv_data.
                  MODIFY zabs_ordens FROM ls_ordens.

                  CREATE OBJECT ref_upload.
                  READ TABLE lt_aufnr INTO DATA(ls_aufnr_x) INDEX 1.
                  IF sy-subrc EQ 0
                  AND ls_aufnr_x-aufnr IS NOT INITIAL.
                    CALL METHOD ref_upload->calculate_cost_order
                      EXPORTING
                        iv_aufnr    = ls_aufnr_x-aufnr
                      IMPORTING
                        et_messages = lt_message.

                    READ TABLE lt_message INTO DATA(ls_message)
                      WITH KEY msgty = 'E'.
                    IF sy-subrc EQ 0.
                      INSERT INITIAL LINE INTO TABLE et_entityset ASSIGNING <ls_entityset>.
                      IF sy-subrc EQ 0.
                        CLEAR lv_message.
                        CALL FUNCTION 'FORMAT_MESSAGE'
                          EXPORTING
                            id        = ls_message-msgid
                            no        = ls_message-msgno
                            v1        = ls_message-msgv1
                            v2        = ls_message-msgv2
                            v3        = ls_message-msgv3
                            v4        = ls_message-msgv4
                          IMPORTING
                            msg       = lv_message
                          EXCEPTIONS
                            not_found = 1
                            OTHERS    = 2.

                        <ls_entityset>-status = ls_message-msgty.
                        <ls_entityset>-descr  = lv_message.
                      ENDIF.
                    ENDIF.
                  ENDIF.

**********************************************************************
* REMOVER COMPONENTES DA ORDEM TAREFA CRIADA
**********************************************************************
*-- Deleting Components from Production Order
                  SELECT *
                    INTO TABLE lt_fmfpcom
                    FROM /agri/fmfpcom
                   WHERE aufnr EQ ls_fpcord-aufnr.
                  IF sy-subrc EQ 0.
                    READ TABLE lt_fmfpcom INTO ls_fmfpcom INDEX 1.
                    IF sy-subrc EQ 0.
                      lv_posnr = ls_fmfpcom-posnr.
                    ENDIF.
                    DELETE /agri/fmfpcom FROM TABLE lt_fmfpcom.
                  ELSE.
                    SELECT *
                      INTO TABLE @DATA(lt_afpo)
                      FROM afpo
                     WHERE aufnr EQ @ls_fpcord-aufnr.
                    IF sy-subrc EQ 0.
                      READ TABLE lt_afpo INTO DATA(ls_afpo) INDEX 1.
                      IF sy-subrc EQ 0.
                        lv_posnr = ls_afpo-posnr.
                      ENDIF.
                    ENDIF.
                  ENDIF.

*-- Fetch existing components of given Production Order
                  SELECT rsnum, rspos
                    INTO TABLE @DATA(lt_resb)
                    FROM resb
                   WHERE aufnr = @ls_fpcord-aufnr.  " Previously created order
                  IF sy-subrc EQ 0.
                    lt_resbkeys = CORRESPONDING #( lt_resb ).
                  ENDIF.

                  IF NOT lt_resbkeys[] IS INITIAL.
*-- BAPI to delete the components of Production Order
                    CALL FUNCTION 'CO_XT_COMPONENTS_DELETE'
                      EXPORTING
                        it_resbkeys_to_delete = lt_resbkeys
                      IMPORTING
                        e_error_occurred      = lv_error
                      TABLES
                        ct_bapireturn         = lt_return
                      EXCEPTIONS
                        delete_failed         = 1
                        OTHERS                = 2.

                    IF lv_error = space.
                      CALL FUNCTION 'CO_XT_ORDER_PREPARE_COMMIT'
                        IMPORTING
                          es_bapireturn    = ls_return
                          e_error_occurred = lv_error.
                      IF ( ls_return-type = 'S' OR
                           ls_return-type = 'W' OR
                           ls_return-type = 'I' ) OR
                           ls_return IS INITIAL.
                        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
                        CALL FUNCTION 'CO_XT_ORDER_INITIALIZE'.
                      ELSE.
                        CLEAR: lv_error, ls_return.
                        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
                      ENDIF.
                    ELSE.
                      CLEAR lv_error.
                      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
                    ENDIF.
                  ENDIF.

**********************************************************************
* INCLUIR INGREDIENTES DA RECEITA NA ORDEM TAREFA
**********************************************************************
                  READ TABLE lt_rec_hist TRANSPORTING NO FIELDS
                    WITH KEY rcnum    = lv_rcnum
                             werks    = ls_fpcord-iwerk
                             tplnr_fl = lv_tplnr_fl BINARY SEARCH.
                  IF sy-subrc EQ 0.
*-- Adding components to Production Order
                    LOOP AT lt_rec_hist INTO ls_rec_hist FROM sy-tabix.
                      IF ls_rec_hist-rcnum    NE lv_rcnum
                      OR ls_rec_hist-werks    NE ls_fpcord-iwerk
                      OR ls_rec_hist-tplnr_fl NE lv_tplnr_fl.
                        EXIT.
                      ENDIF.

                      IF ls_rec_hist-rcdos_ins IS INITIAL
                      OR ls_rec_hist-loevm EQ abap_true.
                        CONTINUE.
                      ENDIF.

                      ls_storage_location-werks = ls_fpcord-iwerk.
                      ls_storage_locationx-werks = 'X'.
                      ls_storage_location-lgort = ls_rec_hist-lgort_ins.
                      ls_storage_locationx-lgort = 'X'.

                      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
                        EXPORTING
                          input      = lv_tplnr_fl
                        IMPORTING
                          output     = lv_tplnr_in
                        EXCEPTIONS
                          not_found  = 1
                          not_active = 2
                          OTHERS     = 3.

                      READ TABLE lt_volume INTO ls_volume
                        WITH KEY tplnr_fl = lv_tplnr_in
                                 matnr    = ls_mara-matnr BINARY SEARCH.
                      IF sy-subrc EQ 0.

                        IF ls_volume-acarx IS NOT INITIAL AND ls_volume-acarx <> 0.
                          ls_requ_quan-quantity = ls_rec_hist-rcdos_ins * ls_volume-acarx.
                        ELSE.
                          ls_requ_quan-quantity = ls_rec_hist-rcdos_ins * ls_volume-acqtb.
                        ENDIF.

                      ELSE.
                        ls_requ_quan-quantity = ls_rec_hist-rcdos_ins.
                      ENDIF.

                      READ TABLE lt_rec_list INTO DATA(ls_rec_list)
                        WITH KEY rcnum     = ls_rec_hist-rcnum
                                 matnr_ins = ls_rec_hist-matnr_ins.
                      IF sy-subrc EQ 0.
                        CALL FUNCTION 'UNIT_OF_MEASURE_ISO_TO_SAP'
                          EXPORTING
                            iso_code  = ls_rec_list-units
                          IMPORTING
                            sap_code  = ls_requ_quan-uom
                          EXCEPTIONS
                            not_found = 1
                            OTHERS    = 2.
                      ENDIF.

                      lv_positionno = '0010'.
                      lv_postp = 'L'.
                      lv_material = ls_rec_hist-matnr_ins.

                      SELECT SINGLE aufnr, aufpl
                        INTO @DATA(ls_afko)
                        FROM afko
                       WHERE aufnr = @ls_fpcord-aufnr.
                      IF sy-subrc EQ 0.
*-- Fetch operation to which it has to be assigned
                        SELECT SINGLE aufpl, aplzl, plnfl
                          INTO @DATA(ls_afvc)
                          FROM afvc
                         WHERE aufpl = @ls_afko-aufpl.
                        IF sy-subrc EQ 0.
                          lv_operation = ls_afvc-aplzl.
                          lv_sequence = ls_afvc-plnfl.
                        ENDIF.
                      ENDIF.

*-- BAPI to add components to Production Order
                      CALL FUNCTION 'CO_XT_COMPONENT_ADD'
                        EXPORTING
                          is_order_key         = ls_fpcord-aufnr
                          i_material           = lv_material
                          is_requ_quan         = ls_requ_quan
                          i_operation          = lv_operation
                          i_sequence           = lv_sequence
                          is_storage_location  = ls_storage_location
                          is_storage_locationx = ls_storage_locationx
*                         i_batch              = lv_batch
*                         i_batchx             = lv_batchx
                          i_postp              = lv_postp
                          i_posno              = lv_positionno
                        IMPORTING
                          es_bapireturn        = ls_return
                          e_error_occurred     = lv_error.
                      IF lv_error = space.
                        CLEAR: lv_numc,
                               ls_return.
*-- Modify POSNR via ASSIGN before DB update to correct the blank
*-- item number in Components due to incompatible types of I_POSNO
*-- (type CIF_R3RES-POSITIONNO) and RESB-POSNR
                        ASSIGN ('(SAPLCOBC)RESB_BT[]') TO <ft_resb_bt>.
                        LOOP AT <ft_resb_bt> ASSIGNING <fs_resb_bt>.
                          lv_numc = sy-tabix * 10.
                          <fs_resb_bt>-posnr = lv_numc.
                          CLEAR lv_numc.
                        ENDLOOP.

*-- Commit transaction
                        CALL FUNCTION 'CO_XT_ORDER_PREPARE_COMMIT'
                          IMPORTING
                            es_bapireturn    = ls_return
                            e_error_occurred = lv_error.

                        IF ( ls_return-type = 'S' OR
                           ls_return-type = 'W' OR
                           ls_return-type = 'I' ) OR
                           ls_return IS INITIAL.
                          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
                          CALL FUNCTION 'CO_XT_ORDER_INITIALIZE'.
                          WAIT UP TO 1 SECONDS.
                        ELSE.
                          CLEAR: lv_error, ls_return.
                          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
                        ENDIF.
                      ELSE.
                        IF ls_return-id IS NOT INITIAL
                        AND ls_return-number IS NOT INITIAL.
                          INSERT INITIAL LINE INTO TABLE et_entityset ASSIGNING <ls_entityset>.
                          IF sy-subrc EQ 0.
                            <ls_entityset>-status = 'E'.
                            MESSAGE ID ls_return-id TYPE c_msg_type-error NUMBER ls_return-number
                              WITH ls_return-message_v1 ls_return-message_v2
                                   ls_return-message_v3 ls_return-message_v4 INTO sy-msgli.
                            <ls_entityset>-descr = sy-msgli.
                          ENDIF.
                        ENDIF.
                        CLEAR: lv_error, ls_return.
                        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
                      ENDIF.
                    ENDLOOP.
                  ENDIF.

                  SELECT *
                    INTO TABLE @DATA(lt_resb_aux)
                    FROM resb
                   WHERE aufnr EQ @ls_fpcord-aufnr
                     AND xloek EQ @space.

                  IF sy-subrc EQ 0.
                    DATA(lv_count) = 0.
                    LOOP AT lt_resb_aux INTO DATA(ls_resb).
                      CLEAR ls_fmfpcom.
                      MOVE-CORRESPONDING ls_resb TO ls_fmfpcom.
                      ls_fmfpcom-posnr = lv_posnr.
                      ADD 1 TO lv_count.
                      ls_fmfpcom-contr = lv_count.
                      READ TABLE lt_rec_hist INTO DATA(ls_rec_hist_x)
                        WITH KEY matnr_ins = ls_resb-matnr.
                      IF sy-subrc EQ 0.
                        ls_fmfpcom-rcdos_ins = ls_rec_hist_x-rcdos_ins.
                      ENDIF.
                      MODIFY /agri/fmfpcom FROM ls_fmfpcom.
                    ENDLOOP.
                  ENDIF.
                ELSE.
                  LOOP AT lt_msg INTO DATA(ls_msg).
                    INSERT INITIAL LINE INTO TABLE et_entityset
                      ASSIGNING <ls_entityset>.
                    IF sy-subrc EQ 0.
                      CLEAR lv_message.
                      CALL FUNCTION 'FORMAT_MESSAGE'
                        EXPORTING
                          id        = ls_msg-msgid
                          no        = ls_msg-msgno
                          v1        = ls_msg-msgv1
                          v2        = ls_msg-msgv2
                          v3        = ls_msg-msgv3
                          v4        = ls_msg-msgv4
                        IMPORTING
                          msg       = lv_message
                        EXCEPTIONS
                          not_found = 1
                          OTHERS    = 2.

                      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
                        EXPORTING
                          input  = ls_glcsprs-tplnr_fl
                        IMPORTING
                          output = <ls_entityset>-tplnr_fl.

                      IF ls_msg-msgty EQ 'S'.
                        <ls_entityset>-aufnr = ls_msg-msgv1.
                      ENDIF.

                      <ls_entityset>-data   = lv_data+6(2) && '.' && lv_data+4(2) && '.' && lv_data(4).
                      <ls_entityset>-status = ls_msg-msgty.
                      <ls_entityset>-descr  = lv_message.
                    ENDIF.
                  ENDLOOP.
                ENDIF.
              ELSE.
                LOOP AT lt_msg_task_order INTO DATA(ls_msg_task_order).
                  INSERT INITIAL LINE INTO TABLE et_entityset ASSIGNING <ls_entityset>.
                  IF sy-subrc EQ 0.
                    CLEAR lv_message.
                    CALL FUNCTION 'FORMAT_MESSAGE'
                      EXPORTING
                        id        = ls_msg_task_order-msgid
                        no        = ls_msg_task_order-msgnr
                        v1        = ls_msg_task_order-msgv1
                        v2        = ls_msg_task_order-msgv2
                        v3        = ls_msg_task_order-msgv3
                        v4        = ls_msg_task_order-msgv4
                      IMPORTING
                        msg       = lv_message
                      EXCEPTIONS
                        not_found = 1
                        OTHERS    = 2.

                    CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
                      EXPORTING
                        input  = ls_glcsprs-tplnr_fl
                      IMPORTING
                        output = <ls_entityset>-tplnr_fl.

                    IF ls_msg_task_order-msgtyp EQ 'S'.
                      <ls_entityset>-aufnr = ls_msg_task_order-msgv1.
                    ENDIF.
                    <ls_entityset>-data   = lv_data+6(2) && '.' && lv_data+4(2) && '.' && lv_data(4).
                    <ls_entityset>-status = ls_msg_task_order-msgtyp.
                    <ls_entityset>-descr  = lv_message.
                  ENDIF.
                ENDLOOP.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ELSE.
        IF lt_talhao_aux[] IS NOT INITIAL.
          INSERT INITIAL LINE INTO TABLE et_entityset ASSIGNING <ls_entityset>.
          IF sy-subrc EQ 0.
*-- Linha selecionada já possui Ordem Tarefa!
            <ls_entityset>-status = 'E'.
            MESSAGE ID 'ZFMFP' TYPE c_msg_type-error NUMBER '097' INTO sy-msgli.
            <ls_entityset>-descr = sy-msgli.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      INSERT INITIAL LINE INTO TABLE et_entityset ASSIGNING <ls_entityset>.
      IF sy-subrc EQ 0.
*-- Selecione ao menos um Terreno!
        <ls_entityset>-status = 'E'.
        MESSAGE ID 'ZFMFP' TYPE c_msg_type-error NUMBER '076' INTO sy-msgli.
        <ls_entityset>-descr = sy-msgli.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD custoreceitaset_get_entityset.

*-- Local Types
    TYPES: BEGIN OF ty_collect_vc,
             period     TYPE spmon,
             aareaform  TYPE zfmacqtb,
             aareamanu  TYPE zfmacqtb,
             bombasform TYPE zfmacqtb,
             bombasmanu TYPE zfmacqtb,
             tipo       TYPE char10,
           END OF ty_collect_vc,

           BEGIN OF ty_period,
             period TYPE spmon,
           END OF ty_period.

*-- Local Declarations
    DATA: lt_filter     TYPE /iwbep/t_mgw_select_option,
          ls_filter     TYPE /iwbep/s_mgw_select_option,
          lt_collect_vc TYPE TABLE OF ty_collect_vc,
          ls_collect_vc TYPE ty_collect_vc,
          lv_filter_str TYPE string,
          lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
          lv_rcnum      TYPE zfmrcnum,
          lv_tfor       TYPE string,
          lv_tman       TYPE string,
          lv_string     TYPE string,
          lv_begda_bis  TYPE begda,
          lv_months     TYPE string,
          lv_matnr      TYPE matnr,
          lrt_matnr     TYPE RANGE OF zfmacmatnr,
          lv_matnr_x    TYPE zfmacmatnr,
          lv_tabixc     TYPE char2,
          lv_field      TYPE fieldname,
          lv_total      TYPE char18,
          lv_period     TYPE char6,
          lt_months     TYPE TABLE OF t247,
          lt_dates      TYPE /scwm/tt_lm_dates,
          lt_period     TYPE TABLE OF ty_period,
          ls_period     TYPE ty_period,
          lrt_period    TYPE RANGE OF tvarvc-low,
          lrt_werks     TYPE RANGE OF werks_d,
          lrs_werks     LIKE LINE OF lrt_werks,
          lv_versao     TYPE zabs_del_ver_orc,
          lv_begda      TYPE begda,
          lv_endda      TYPE endda,
          lv_werks      TYPE werks_d,
          lv_acnum      TYPE zfmacnum,
          lv_extwg      TYPE extwg,
          lv_matkl      TYPE matkl,
          lv_tipo       TYPE char10.

    TYPES: BEGIN OF ty_collect,
             period TYPE spmon,
             custo  TYPE zabs_del_custo,
             tipo   LIKE lv_tipo,
           END OF ty_collect.

    DATA: lt_collect TYPE STANDARD TABLE OF ty_collect INITIAL SIZE 0,
          ls_collect LIKE LINE OF lt_collect.

*-- Local Constants
    CONSTANTS: BEGIN OF c_crop_season_status,
                 active   TYPE /agri/glastat VALUE 'A',
                 inactive TYPE /agri/glastat VALUE 'I',
                 closed   TYPE /agri/glastat VALUE 'C',
               END OF c_crop_season_status.

*-- Constants
    CONSTANTS: BEGIN OF lc_tipo,
                 formacao   LIKE lv_tipo VALUE 'FORMAÇÃO',
                 manutencao LIKE lv_tipo VALUE 'MANUTENÇÃO',
               END OF lc_tipo.

    lo_filter = io_tech_request_context->get_filter( ).
    lt_filter = lo_filter->get_filter_select_options( ).
    lv_filter_str = lo_filter->get_filter_string( ).

*--Maps filter table lines to function module parameters
    LOOP AT lt_filter INTO ls_filter.
      CASE ls_filter-property.
        WHEN 'MATNR'.
          lv_matnr = ls_filter-select_options[ 1 ]-low.
        WHEN 'BEGDA'.
          lv_begda = ls_filter-select_options[ 1 ]-low.
        WHEN 'ENDDA'.
          lv_endda = ls_filter-select_options[ 1 ]-low.
        WHEN 'WERKS'.
          lv_werks = ls_filter-select_options[ 1 ]-low.
        WHEN 'ACNUM'.
          lv_acnum = ls_filter-select_options[ 1 ]-low.
        WHEN 'EXTWG'.
          lv_extwg = ls_filter-select_options[ 1 ]-low.
        WHEN 'MATKL'.
          lv_matkl = ls_filter-select_options[ 1 ]-low.
        WHEN 'VERSAO'.
          lv_versao = ls_filter-select_options[ 1 ]-low.
      ENDCASE.
    ENDLOOP.

    IF lv_acnum IS NOT INITIAL.
      SELECT iwerk AS low
        FROM zfmaitm
        INTO CORRESPONDING FIELDS OF TABLE @lrt_werks
       WHERE acnum = @lv_acnum.

      lrs_werks-sign = 'I'.
      lrs_werks-option = 'EQ'.
      MODIFY lrt_werks FROM lrs_werks TRANSPORTING sign option WHERE low <> ''.

      lrs_werks = 'IEQ'.
      lrs_werks-low = lv_werks.
      APPEND lrs_werks TO lrt_werks.

      SORT lrt_werks BY low.
      DELETE ADJACENT DUPLICATES FROM lrt_werks COMPARING ALL FIELDS.
    ENDIF.

    IF lv_begda IS NOT INITIAL.
      CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
        EXPORTING
          date                      = lv_begda
        EXCEPTIONS
          plausibility_check_failed = 1
          OTHERS                    = 2.

      IF sy-subrc <> 0.
        CLEAR lv_begda.
      ENDIF.
    ENDIF.

    IF lv_endda IS NOT INITIAL.
      CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
        EXPORTING
          date                      = lv_endda
        EXCEPTIONS
          plausibility_check_failed = 1
          OTHERS                    = 2.

      IF sy-subrc <> 0.
        CLEAR lv_endda.
      ENDIF.
    ENDIF.

    IF lv_begda IS INITIAL
    OR lv_endda IS INITIAL.
      DATA(lv_error) = abap_true.
    ELSE.
      lv_error = abap_false.
    ENDIF.

    IF lv_error EQ abap_false.
      CALL FUNCTION '/SCWM/DATES_BETWEEN_TWO_DATES'
        EXPORTING
          iv_begda = lv_begda
          iv_endda = lv_endda
        IMPORTING
          et_dates = lt_dates.

      LOOP AT lt_dates INTO DATA(ls_dates).
        IF lv_period NE ls_dates(6).
          INSERT INITIAL LINE INTO TABLE lrt_period
           ASSIGNING FIELD-SYMBOL(<lrs_period>).
          IF sy-subrc EQ 0.
            <lrs_period> = 'IEQ'.
            <lrs_period>-low = lv_period = ls_dates(6).
          ENDIF.

          ls_period-period = ls_dates(6).
          APPEND ls_period TO lt_period.
        ENDIF.
      ENDLOOP.

      SELECT rctyp, orcamento
        INTO TABLE @DATA(lt_rctyp)
        FROM ztfmrctyp
       WHERE orcamento EQ @abap_true.

      IF sy-subrc EQ 0.
        lv_matnr_x = 'TMAN' && lv_matnr+4(36).
        INSERT INITIAL LINE INTO TABLE lrt_matnr
          ASSIGNING FIELD-SYMBOL(<lrs_matnr>).
        IF sy-subrc EQ 0.
          <lrs_matnr> = 'IEQ'.
          <lrs_matnr>-low = lv_matnr_x.
        ENDIF.

        lv_matnr_x = 'TFOR' && lv_matnr+4(36).
        INSERT INITIAL LINE INTO TABLE lrt_matnr ASSIGNING <lrs_matnr>.
        IF sy-subrc EQ 0.
          <lrs_matnr> = 'IEQ'.
          <lrs_matnr>-low = lv_matnr_x.
        ENDIF.

        lv_matnr_x = 'TIMP' && lv_matnr+4(36).
        INSERT INITIAL LINE INTO TABLE lrt_matnr ASSIGNING <lrs_matnr>.
        IF sy-subrc EQ 0.
          <lrs_matnr> = 'IEQ'.
          <lrs_matnr>-low = lv_matnr_x.
        ENDIF.

        SELECT *
          INTO TABLE @DATA(lt_rchdr)
          FROM zfmrchdr
          FOR ALL ENTRIES IN @lt_rctyp
*        WHERE werks EQ @lv_werks
         WHERE werks IN @lrt_werks[]
           AND matnr IN @lrt_matnr[]
           AND datuv GE @lv_begda
           AND rctyp EQ @lt_rctyp-rctyp
           AND datbi LE @lv_endda.

        DELETE lt_rchdr WHERE matnr(4) NE 'TFOR'
                          AND matnr(4) NE 'TMAN'
                          AND matnr(4) NE 'TIMP'.

        IF lt_rchdr[] IS NOT INITIAL.
          SORT lt_rchdr BY rcnum.

          LOOP AT lt_rchdr ASSIGNING FIELD-SYMBOL(<ls_rchdr>).
            IF <ls_rchdr>-matnr(4) EQ 'TFOR'
            OR <ls_rchdr>-matnr(4) EQ 'TIMP'.
              <ls_rchdr>-text1 = lc_tipo-formacao.
            ELSEIF <ls_rchdr>-matnr(4) EQ 'TMAN'.
              <ls_rchdr>-text1 = lc_tipo-manutencao.
            ENDIF.
          ENDLOOP.

          SELECT *
            INTO TABLE @DATA(lt_orcamento)
            FROM zabs_orcamento
            FOR ALL ENTRIES IN @lt_rchdr
           WHERE rcnum  EQ @lt_rchdr-rcnum
             AND period IN @lrt_period[]
             AND versao EQ @lv_versao.

          SORT lt_orcamento BY acnum rcnum period.
        ENDIF.
      ENDIF.

      IF lt_orcamento[] IS NOT INITIAL
      AND lt_rchdr[] IS NOT INITIAL.
        IF lv_acnum IS NOT INITIAL.
          SELECT *
            INTO TABLE @DATA(lt_zfmacvlcl)
            FROM zfmacvlcl
           WHERE acnum EQ @lv_acnum.

          SORT lt_zfmacvlcl BY tplnr_fl matnr.

          IF lt_zfmacvlcl[] IS NOT INITIAL.
            SELECT *
              INTO TABLE @DATA(lt_glflcma)
              FROM /agri/glflcma
              FOR ALL ENTRIES IN @lt_zfmacvlcl
             WHERE tplnr_fl EQ @lt_zfmacvlcl-tplnr_fl
               AND loevm    NE @abap_true
               AND astat    EQ @c_crop_season_status-active.

*-- BOC T_T.KONNO-11/10/20
*            SELECT *
*              APPENDING TABLE @lt_glflcma
*              FROM /agri/glflcma
*              FOR ALL ENTRIES IN @lt_zfmacvlcl
*             WHERE tplnr_fl   EQ @lt_zfmacvlcl-tplnr_fl
*               AND loevm      NE @abap_true
*               AND zzprevisto EQ @abap_true.
*-- EOC T_T.KONNO-11/10/20
          ENDIF.

          IF lt_glflcma[] IS NOT INITIAL.
            SORT lt_glflcma BY tplnr_fl ASCENDING
                               contr    DESCENDING.

            DELETE ADJACENT DUPLICATES FROM lt_glflcma COMPARING tplnr_fl.

            lv_tman = 'TMAN' && lv_matnr+4(36).
            lv_tfor = 'TFOR' && lv_matnr+4(36).

            LOOP AT lt_period INTO ls_period.
              CONCATENATE ls_period '01' INTO lv_begda_bis.

              LOOP AT lt_glflcma INTO DATA(ls_glflcma).
                CLEAR lv_months.
                IF ls_glflcma-datab_ref IS NOT INITIAL.
                  CALL FUNCTION 'MONTHS_BETWEEN_TWO_DATES'
                    EXPORTING
                      i_datum_bis = lv_begda_bis
                      i_datum_von = ls_glflcma-datab_ref
                    IMPORTING
                      e_monate    = lv_months.

                  CONDENSE lv_months NO-GAPS.
                ENDIF.

                lv_string = lv_tman.
                READ TABLE lt_zfmacvlcl INTO DATA(ls_zfmacvlcl)
                  WITH KEY tplnr_fl = ls_glflcma-tplnr_fl
                           matnr    = lv_string BINARY SEARCH.
                IF sy-subrc EQ 0.
                  ls_collect_vc-aareamanu  = ls_zfmacvlcl-acarx.
                  ls_collect_vc-bombasmanu = ls_zfmacvlcl-acqtb.
                ENDIF.

                lv_string = lv_tfor.
                READ TABLE lt_zfmacvlcl INTO ls_zfmacvlcl
                  WITH KEY tplnr_fl = ls_glflcma-tplnr_fl
                           matnr    = lv_string BINARY SEARCH.
                IF sy-subrc EQ 0.
                  ls_collect_vc-aareaform  = ls_zfmacvlcl-acarx.
                  ls_collect_vc-bombasform = ls_zfmacvlcl-acqtb.
                ENDIF.

                ls_collect_vc-period = ls_period-period.
                COLLECT ls_collect_vc INTO lt_collect_vc.
                CLEAR ls_collect_vc.
              ENDLOOP.
            ENDLOOP.
            SORT lt_collect_vc BY period.
          ENDIF.
        ENDIF.

        LOOP AT lt_orcamento ASSIGNING FIELD-SYMBOL(<ls_orcamento>).
          READ TABLE lt_rchdr INTO DATA(ls_rchdr)
            WITH KEY rcnum = <ls_orcamento>-rcnum BINARY SEARCH.
          IF sy-subrc EQ 0.
            ls_collect-period = <ls_orcamento>-period.
            IF ls_rchdr-matnr(4) EQ 'TFOR'.
              ls_collect-tipo = lc_tipo-formacao.
              READ TABLE lt_collect_vc INTO ls_collect_vc
                WITH KEY period = <ls_orcamento>-period BINARY SEARCH.
              IF sy-subrc EQ 0.
                CLEAR <ls_orcamento>-custo.
              ENDIF.
            ELSEIF ls_rchdr-matnr(4) EQ 'TMAN'.
              ls_collect-tipo = lc_tipo-manutencao.
            ENDIF.
            ls_collect-custo  = <ls_orcamento>-custo.
            COLLECT ls_collect INTO lt_collect.
          ENDIF.
        ENDLOOP.

        SORT lt_collect BY period tipo.
      ENDIF.

      DO 2 TIMES.
        CASE sy-index.
          WHEN 1.
            lv_tipo = lc_tipo-formacao.
          WHEN 2.
            lv_tipo = lc_tipo-manutencao.
        ENDCASE.

        INSERT INITIAL LINE INTO TABLE et_entityset
          ASSIGNING FIELD-SYMBOL(<ls_entityset>).
        IF sy-subrc EQ 0.
          LOOP AT lrt_period INTO DATA(lrs_period).
            DATA(lv_tabix) = sy-tabix.
            lv_tabixc = lv_tabix.
            CLEAR lv_total.
            READ TABLE lt_collect INTO ls_collect
              WITH KEY period = lrs_period-low
                       tipo   = lv_tipo BINARY SEARCH.
            IF sy-subrc EQ 0.
              lv_total = ls_collect-custo.
            ENDIF.
            CONCATENATE 'M' lv_tabixc INTO lv_field.
            ASSIGN COMPONENT lv_field OF STRUCTURE <ls_entityset>
              TO FIELD-SYMBOL(<lv_field>).
            IF sy-subrc EQ 0.
              <lv_field> = lv_total.
            ENDIF.
          ENDLOOP.
          <ls_entityset>-custo = lv_tipo.
        ENDIF.
      ENDDO.
    ENDIF.

  ENDMETHOD.


METHOD geraexcelset_get_entityset.

*-- Local Declarations
  DATA: lt_filter        TYPE /iwbep/t_mgw_select_option,
        ls_filter        TYPE /iwbep/s_mgw_select_option,
        ls_entityset     LIKE LINE OF et_entityset,
        lv_filter_str    TYPE string,
        lo_filter        TYPE REF TO /iwbep/if_mgw_req_filter,
        lv_rcnum         TYPE zfmrcnum,
        lrt_rcnum        TYPE RANGE OF zfmrcnum,
        lv_matnr         TYPE matnr,
        lrt_matnr        TYPE RANGE OF matnr,
        lv_tplnr_fl      TYPE /agri/gltplnr_fl,
        lrt_tplnr_fl     LIKE RANGE OF lv_tplnr_fl,
        lrt_matkl        TYPE RANGE OF matkl,
        lv_werks         TYPE werks_d,
        lrt_werks        TYPE RANGE OF werks_d,
        lv_acnum         TYPE zfmacnum,
        lrt_acnum        TYPE RANGE OF zfmacnum,
        lv_extwg         TYPE extwg,
        lrt_extwg        TYPE RANGE OF extwg,
        lv_matkl         TYPE matkl,
        lv_begda         TYPE begda,
        lv_p_begda       TYPE spmon,
        lv_p_endda       TYPE spmon,
        lv_endda         TYPE begda,
        lv_versao        TYPE zabs_del_ver_orc,
        lrt_versao       TYPE RANGE OF zabs_del_ver_orc,
        lv_versao_custo  TYPE zabs_del_ver_orc,
        lrt_versao_custo TYPE RANGE OF zabs_del_ver_orc,
        lv_lgort         TYPE lgort_d,
        lrt_lgort        TYPE RANGE OF lgort_d.

  CONSTANTS: lc_status TYPE /agri/glastat VALUE 'A'.

  lo_filter = io_tech_request_context->get_filter( ).
  lt_filter = lo_filter->get_filter_select_options( ).
  lv_filter_str = lo_filter->get_filter_string( ).

*--Maps filter table lines to function module parameters
  LOOP AT lt_filter INTO ls_filter.
    CASE ls_filter-property.
      WHEN 'RCNUM'.
        lv_rcnum = ls_filter-select_options[ 1 ]-low.

        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_rcnum ).


      WHEN 'MATNR'.
        lv_matnr = ls_filter-select_options[ 1 ]-low.

        lo_filter->convert_select_option(
        EXPORTING
          is_select_option = ls_filter
        IMPORTING
          et_select_option = lrt_matnr ).

      WHEN 'DATAB'.
        lv_begda = ls_filter-select_options[ 1 ]-low.

        lv_p_begda = lv_begda(6).
      WHEN 'DATBI'.
        lv_endda = ls_filter-select_options[ 1 ]-low.

        lv_p_endda = lv_endda(6).
      WHEN 'WERKS'.
        lv_werks = ls_filter-select_options[ 1 ]-low.

        lo_filter->convert_select_option(
        EXPORTING
          is_select_option = ls_filter
        IMPORTING
          et_select_option = lrt_werks ).

      WHEN 'ACNUM'.
        lv_acnum = ls_filter-select_options[ 1 ]-low.

        lo_filter->convert_select_option(
        EXPORTING
          is_select_option = ls_filter
        IMPORTING
          et_select_option = lrt_acnum ).

      WHEN 'EXTWG'.
        lv_extwg = ls_filter-select_options[ 1 ]-low.


        lo_filter->convert_select_option(
        EXPORTING
          is_select_option = ls_filter
        IMPORTING
          et_select_option = lrt_extwg ).

      WHEN 'MATKL'.
        lv_matkl = ls_filter-select_options[ 1 ]-low.


        lo_filter->convert_select_option(
        EXPORTING
          is_select_option = ls_filter
        IMPORTING
          et_select_option = lrt_matkl ).

      WHEN 'LGORT'.
        lv_lgort = ls_filter-select_options[ 1 ]-low.


        lo_filter->convert_select_option(
        EXPORTING
          is_select_option = ls_filter
        IMPORTING
          et_select_option = lrt_lgort ).

      WHEN 'VERSAO'.
        lv_versao = ls_filter-select_options[ 1 ]-low.

        lo_filter->convert_select_option(
        EXPORTING
          is_select_option = ls_filter
        IMPORTING
          et_select_option = lrt_versao ).

      WHEN 'VERSAOCUSTO'.
        lv_versao_custo = ls_filter-select_options[ 1 ]-low.

        lo_filter->convert_select_option(
        EXPORTING
          is_select_option = ls_filter
        IMPORTING
          et_select_option = lrt_versao_custo ).

      WHEN 'TPLNR_FL'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_tplnr_fl ).
    ENDCASE.
  ENDLOOP.

***modify begin On 19.05.2021 INC0029665 Adonis Rossato

*  SELECT *
*    INTO TABLE @DATA(lt_t025t)
*    FROM t025t
*    WHERE spras EQ @sy-langu.
*
*  IF sy-subrc EQ 0.
*    SELECT *
*      INTO TABLE @DATA(lt_orcamento)
*      FROM zabs_orcamento
*     WHERE acnum  EQ @lv_acnum
*       AND versao EQ @lv_versao.
*
*    IF sy-subrc EQ 0.
*      SELECT *
*        INTO TABLE @DATA(lt_zfmachdr)
*        FROM zfmachdr
*        FOR ALL ENTRIES IN @lt_orcamento
*       WHERE acnum EQ @lt_orcamento-acnum.
*
*      IF sy-subrc EQ 0.
*        SELECT acnum, acpos, tplnr_fl, iwerk
*          INTO TABLE @DATA(lt_zfmacitm)
*          FROM zfmaitm
*          FOR ALL ENTRIES IN @lt_zfmachdr
*         WHERE acnum EQ @lt_zfmachdr-acnum.
*
*        SELECT *
*          INTO TABLE @DATA(lt_mbew)
*          FROM mbew
*          FOR ALL ENTRIES IN @lt_orcamento
*         WHERE matnr EQ @lt_orcamento-matnr.
*
**-- Consider only active seasons and not eliminated
*        SELECT tplnr_fl, contr, anlnr, kostl
*          INTO TABLE @DATA(lt_glflcma)
*          FROM /agri/glflcma
*          FOR ALL ENTRIES IN @lt_orcamento
*         WHERE tplnr_fl EQ @lt_orcamento-fazenda
*           AND astat    EQ @lc_status
*           AND loevm    EQ @abap_false.
*
**-- BOC T_T.KONNO-11/10/20
**        SELECT tplnr_fl, contr, anlnr, kostl
**          APPENDING TABLE @lt_glflcma
**          FROM /agri/glflcma
**          FOR ALL ENTRIES IN @lt_orcamento
**         WHERE tplnr_fl   EQ @lt_orcamento-fazenda
**           AND loevm      EQ @abap_false
**           AND zzprevisto EQ @abap_true.
**-- EOC T_T.KONNO-11/10/20
*
**-- If there are several seasons for a terrain, consider the last one created
*        SORT lt_glflcma BY tplnr_fl ASCENDING
*                           contr    DESCENDING.
*
*        DELETE ADJACENT DUPLICATES FROM lt_glflcma COMPARING tplnr_fl.
*
*        SORT: lt_zfmachdr BY acnum,
*              lt_zfmacitm BY acnum iwerk,
*              lt_mbew     BY matnr bwkey,
*              lt_t025t    BY bklas,
*              lt_glflcma  BY tplnr_fl.


*--Preparing Budget's Data
  SELECT o~acnum, o~extwg, o~matkl, o~rcnum,
         o~matnr, o~period, o~fazenda,
         o~kostl, o~aarea_form, o~aarea_manu, o~passadas,
         o~rcdos, o~custo, o~produtos, r~werks,
         m~bklas, t~bkbez
    FROM zabs_orcamento AS o
    INNER JOIN zfmrchdr AS r
    ON o~rcnum EQ r~rcnum
    LEFT OUTER JOIN mbew AS m
    ON o~matnr EQ m~matnr
    AND r~werks EQ m~bwkey
    LEFT OUTER JOIN t025t AS t
    ON t~spras EQ @sy-langu
    AND m~bklas EQ t~bklas
    INTO TABLE @DATA(lt_orcamento)
   WHERE o~acnum   IN @lrt_acnum[]
     AND o~extwg   IN @lrt_extwg[]
     AND o~matkl   IN @lrt_matkl[]
     AND o~rcnum   IN @lrt_rcnum[]
     AND o~matnr   IN @lrt_matnr[]
     AND o~period  BETWEEN @lv_p_begda AND @lv_p_endda
     AND o~fazenda IN @lrt_tplnr_fl[]
     AND o~versao  EQ @lv_versao
     AND r~werks in @lrt_werks.


  DATA(lt_budget) = lt_orcamento[].
  SORT lt_budget BY period ASCENDING.
  READ TABLE lt_budget INTO DATA(ls_budget) INDEX 1.
  IF sy-subrc EQ 0.
    DATA(lv_low) = ls_budget-period(4).
    DATA(lv_high) = lv_low + 1.
  ENDIF.
***modify End On 19.05.2021 INC0029665 Adonis Rossato


  LOOP AT lt_orcamento INTO DATA(ls_orcamento).

***modify Begin On 19.05.2021 INC0029665 Adonis Rossato

*          READ TABLE lt_zfmachdr INTO DATA(ls_zfmachdr)
*            WITH KEY acnum = ls_orcamento-acnum BINARY SEARCH.
*
*          IF sy-subrc EQ 0.
*            READ TABLE lt_mbew INTO DATA(ls_mbew)
*              WITH KEY  matnr = ls_orcamento-matnr
*                        bwkey = lv_werks BINARY SEARCH.
*
*            IF sy-subrc EQ 0.
*              READ TABLE lt_t025t INTO DATA(ls_t025t)
*                WITH KEY bklas = ls_mbew-bklas BINARY SEARCH.
*              IF sy-subrc EQ 0.
*                CLEAR ls_entityset.
*                ls_entityset-classe      = ls_t025t-bkbez.
**                  ls_entityset-centrocusto = ls_orcamento-kostl.
***modify Start On 15.09.2020 INC0023160 T_C.KARANAM
***                  IF ls_orcamento-kostl IS NOT INITIAL.
***                    ls_entityset-centrocusto = ls_orcamento-kostl.
***                  ELSE.
*                READ TABLE lt_glflcma INTO DATA(ls_glflcma)
*                  WITH KEY tplnr_fl = ls_orcamento-fazenda
*                    BINARY SEARCH.
*                IF ls_glflcma-kostl IS NOT INITIAL.
*                  ls_entityset-centrocusto = ls_glflcma-kostl.
*                ELSEIF ls_glflcma-anlnr IS NOT INITIAL.
*                  ls_entityset-centrocusto = ls_glflcma-anlnr.
*                ENDIF.
***                  ENDIF.
***modify End On 15.09.2020 INC0023160 T_C.KARANAM
*                ls_entityset-anomes      = ls_orcamento-period.
*                ls_entityset-material    = ls_orcamento-matnr.
*                ls_entityset-quantidade  = ls_orcamento-produtos.
*                ls_entityset-versao      = lv_versao_custo.
*                ls_entityset-budget      = lv_begda+0(4) && lv_endda+0(4).



    CLEAR ls_entityset.
    ls_entityset-classe      = ls_orcamento-bkbez.
    ls_entityset-centrocusto = ls_orcamento-kostl.
    ls_entityset-anomes      = ls_orcamento-period.
    ls_entityset-material    = ls_orcamento-matnr.
    ls_entityset-quantidade  = ls_orcamento-produtos.
    ls_entityset-versao      = lv_versao_custo.
    ls_entityset-budget      = lv_low && lv_high.

***modify End On 19.05.2021 INC0029665 Adonis Rossato

    append ls_entityset TO et_entityset.
    CLEAR ls_entityset.
*  ENDIF.
*ENDIF.
*ENDIF.
ENDLOOP.
*ENDIF.
*ENDIF.
*ENDIF.

SORT et_entityset BY anomes material.

ENDMETHOD.


  METHOD getareacultivose_get_entityset.

    DATA: lt_achdr  TYPE  zt_fmachdr,
          lt_acitm  TYPE  zt_fmacitm,
          lrt_werks TYPE /iwbep/t_cod_select_options.

    LOOP AT it_filter_select_options INTO DATA(ls_filters).
      CASE ls_filters-property.
        WHEN 'Werks'.
          lrt_werks = ls_filters-select_options.
      ENDCASE.
    ENDLOOP.

    CALL FUNCTION 'ZFMAC_READ'
      IMPORTING
        et_achdr       = lt_achdr
        et_acitm       = lt_acitm
      EXCEPTIONS
        no_data_exists = 1
        OTHERS         = 2.

    IF sy-subrc EQ 0.
      IF lrt_werks[] IS NOT INITIAL.
        DELETE lt_achdr WHERE werks NOT IN lrt_werks[].
      ENDIF.

      LOOP AT lt_achdr INTO DATA(ls_achdr).
        INSERT INITIAL LINE INTO TABLE et_entityset
        ASSIGNING FIELD-SYMBOL(<ls_entityset>).
        IF sy-subrc EQ 0.
          MOVE-CORRESPONDING ls_achdr TO <ls_entityset>.
          <ls_entityset>-key   = ls_achdr-acnum.
          <ls_entityset>-descr = ls_achdr-acdes.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD getcentroset_get_entityset.

    DATA: ls_entityset LIKE LINE OF et_entityset.

    SELECT werks, name1
      INTO TABLE @DATA(lt_t001w)
      FROM /agri/vd_tgl001w.

    LOOP AT lt_t001w INTO DATA(ls_t001w).
      INSERT INITIAL LINE INTO TABLE et_entityset
      ASSIGNING FIELD-SYMBOL(<ls_entityset>).
      IF sy-subrc EQ 0.
        <ls_entityset>-key = ls_t001w-werks.
        TRANSLATE ls_t001w-name1 TO UPPER CASE.
        <ls_entityset>-descr = ls_t001w-name1.
      ENDIF.
    ENDLOOP.

    SORT et_entityset BY key.

  ENDMETHOD.


  METHOD getdetail1datase_get_entityset.

    TYPES: BEGIN OF ty_matkl,
             matnr   TYPE matnr,
             matkl   TYPE matkl,
             wgbez   TYPE wgbez,
             wgbez60 TYPE wgbez60,
           END OF ty_matkl.

    DATA: lt_matkl       TYPE TABLE OF ty_matkl,
          lt_mara        TYPE TABLE OF ty_matkl,
          lt_week        TYPE TABLE OF zfmplprsemanal,
          lt_acnum       TYPE zt_fm_acnum,
          lt_achdr       TYPE zt_fmachdr,
          lt_acitm       TYPE zt_fmacitm,
          lt_dates       TYPE /scwm/tt_lm_dates,
          lrt_matnr      TYPE RANGE OF matnr,
          lrt_mtart      TYPE /iwbep/t_cod_select_options,
          ls_entityset   LIKE LINE OF et_entityset,
          ls_dates       LIKE LINE OF lt_dates,
          ls_acnum       LIKE LINE OF lt_acnum,
          lrs_matnr      LIKE LINE OF lrt_matnr,
          lrs_mtart      LIKE LINE OF lrt_mtart,
          lv_pltyp       TYPE zfmplann,
          lv_acnum       TYPE zfmacnum,
          lv_begda       TYPE zfmdatab,
          lv_begda_aux   TYPE char10,
          lv_endda       TYPE zfmdatbi,
          lv_extwg       TYPE extwg,
          lv_total       TYPE zfmacren,
          lv_begda_month TYPE begda,
          lv_endda_month TYPE endda,
          lv_month       TYPE c LENGTH 2,
          lv_tot_prog    TYPE zfmplapr,
          lv_calc        TYPE zfmplapr,
          lv_count       TYPE i.

    LOOP AT it_filter_select_options INTO DATA(ls_filters).
      CASE ls_filters-property.
        WHEN 'Acnum'.
          lv_acnum = ls_filters-select_options[ 1 ]-low.
        WHEN 'Actyp'.
          lv_pltyp = ls_filters-select_options[ 1 ]-low.
        WHEN 'Begda'.
          lv_begda = ls_filters-select_options[ 1 ]-low.
        WHEN 'Begda_aux'.
          lv_begda_aux = ls_filters-select_options[ 1 ]-low.
        WHEN 'Endda'.
          lv_endda = ls_filters-select_options[ 1 ]-low.
        WHEN 'Extwg'.
          lv_extwg = ls_filters-select_options[ 1 ]-low.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

    IF lv_begda IS INITIAL.
      IF lv_begda_aux IS NOT INITIAL.
        lv_begda = lv_begda_aux.
      ELSE.
        lv_begda = sy-datum.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                      = lv_begda
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS                    = 2.

    IF sy-subrc <> 0.
      DATA(lv_invalid_date) = abap_true.
    ELSE.
      lv_invalid_date = abap_false.
    ENDIF.

    CHECK lv_invalid_date EQ abap_false.

*-- Selecionar área de planejamento mensal
    SELECT *
      INTO TABLE @DATA(lt_zfmplmens)
      FROM zfmplmens
     WHERE acnum   EQ @lv_acnum
       AND extwg   EQ @lv_extwg
       AND periodo EQ @lv_begda(6).

    IF sy-subrc EQ 0.
      SORT lt_zfmplmens BY matkl.
    ENDIF.

    CALL FUNCTION 'FIAPPL_ADD_DAYS_TO_DATE'
      EXPORTING
        i_date      = lv_begda
        i_days      = '4'
        signum      = '+'
      IMPORTING
        e_calc_date = lv_endda.

    CALL FUNCTION '/SCWM/DATES_BETWEEN_TWO_DATES'
      EXPORTING
        iv_begda = lv_begda
        iv_endda = lv_endda
      IMPORTING
        et_dates = lt_dates.

    IF lv_begda IS INITIAL.
      lv_begda = sy-datum.
    ENDIF.

    IF lv_endda IS INITIAL.
      lv_endda = sy-datum.
    ENDIF.

    SELECT *
      INTO TABLE @DATA(lt_zfmacvlcl)
      FROM zfmacvlcl
     WHERE acnum EQ @lv_acnum.

    ls_acnum-acnum = lv_acnum.
    APPEND ls_acnum TO lt_acnum.

    CALL FUNCTION 'ZFMAC_READ'
      EXPORTING
        it_acnum       = lt_acnum
      IMPORTING
        et_achdr       = lt_achdr
        et_acitm       = lt_acitm
      EXCEPTIONS
        no_data_exists = 1
        OTHERS         = 2.

    IF lt_acitm[] IS NOT INITIAL.
      SELECT *
        INTO TABLE @lt_week
        FROM zfmplprsemanal
        FOR ALL ENTRIES IN @lt_acitm
       WHERE acnum EQ @lt_acitm-acnum.
    ENDIF.

    lrt_mtart = zcl_agri_utilities=>zm_get_tvarvc( iv_name = 'ZAGRI_PROCESSO' ).
    lrs_mtart-sign = 'I'.
    lrs_mtart-option = 'EQ'.
    MODIFY lrt_mtart FROM lrs_mtart TRANSPORTING sign option WHERE low <> ''.

    SELECT *
      INTO TABLE @DATA(lt_t023t)
      FROM t023t
     WHERE spras EQ @sy-langu.

    IF sy-subrc EQ 0.
      SORT lt_t023t BY matkl.
    ENDIF.

    CASE lv_extwg.
      WHEN 'GIM'.
        lrs_matnr = 'ICP'.
        lrs_matnr-low = 'TIM*'.
        APPEND lrs_matnr TO lrt_matnr.
      WHEN 'GTC'.
        lrs_matnr = 'ICP'.
        lrs_matnr-low = 'TMAN*'.
        APPEND lrs_matnr TO lrt_matnr.
        lrs_matnr-low = 'TFOR*'.
        APPEND lrs_matnr TO lrt_matnr.
      WHEN 'GCO'.
        lrs_matnr = 'ICP'.
        lrs_matnr-low = 'TCOL*'.
        APPEND lrs_matnr TO lrt_matnr.
      WHEN 'GIR'.
        lrs_matnr = 'ICP'.
        lrs_matnr-low = 'TMAN*'.
        APPEND lrs_matnr TO lrt_matnr.
    ENDCASE.

    SELECT matnr, matkl
      INTO CORRESPONDING FIELDS OF TABLE @lt_matkl
      FROM mara
     WHERE matnr IN @lrt_matnr[]
       AND mtart IN @lrt_mtart[]
       AND extwg EQ @lv_extwg
       AND matkl NE @space.

    IF sy-subrc EQ 0.
      SORT lt_matkl BY matkl.
      lt_mara[] = lt_matkl[].
      DELETE ADJACENT DUPLICATES FROM lt_matkl COMPARING matkl.

      LOOP AT lt_matkl INTO DATA(ls_matkl).
        READ TABLE lt_t023t INTO DATA(ls_t023t)
          WITH KEY matkl = ls_matkl-matkl BINARY SEARCH.

        IF sy-subrc EQ 0.
          CLEAR lv_calc.
*-- Ler dados de planejamento mensal de acordo com o material
          READ TABLE lt_zfmplmens INTO DATA(ls_zfmplmens)
            WITH KEY matkl = ls_matkl-matkl BINARY SEARCH.

          IF sy-subrc EQ 0.
            CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
              EXPORTING
                input    = ls_zfmplmens-unplm
                language = sy-langu
              IMPORTING
                output   = ls_zfmplmens-unplm.

            IF ls_zfmplmens-qtplm > 0.
              ls_entityset-planejado = ls_zfmplmens-qtplm.
            ENDIF.
          ENDIF.

*-- Retornar total programado para material no mês
          lv_begda_month = lv_begda(6) && '01'.
          CALL FUNCTION '/AGRI/G_LAST_DAY_OF_MONTHS'
            EXPORTING
              i_day_in            = lv_begda_month
            IMPORTING
              e_last_day_of_month = lv_endda_month.

          lv_tot_prog = zcl_agri_utilities=>zm_get_tot_matnr(
              EXPORTING
                it_acitm = lt_acitm                 " Área de cultivo
                iv_begda = lv_begda_month           " Início da validade
                iv_endda = lv_endda_month           " Fim da validade
                iv_matkl = ls_matkl-matkl           " Nº do material
                it_week  = lt_week ).

          IF lv_tot_prog > 0.
            ls_entityset-programado = lv_tot_prog.
          ENDIF.

*-- Calcula saldo
          lv_calc = ls_zfmplmens-qtplm - lv_tot_prog.
          IF lv_calc > 0.
            ls_entityset-saldo = lv_calc.
          ELSE.
            lv_calc = lv_calc * ( -1 ).
            IF lv_calc > 0.
              ls_entityset-saldo = '-' && lv_calc.
            ENDIF.
          ENDIF.

          IF ls_entityset-planejado IS INITIAL
          AND ls_entityset-programado IS INITIAL.
            CLEAR ls_entityset-saldo.
          ENDIF.

*-- Realizar cálculo por dia
          LOOP AT lt_dates INTO ls_dates.
            DATA(lv_tabix) = sy-tabix.
            LOOP AT lt_week INTO DATA(ls_week)
              WHERE matkl EQ ls_matkl-matkl
                AND plday EQ ls_dates.
              CASE lv_tabix.
                WHEN '01'.
                  ls_entityset-d1 = ls_entityset-d1 + ls_week-plapr.
                WHEN '02'.
                  ls_entityset-d2 = ls_entityset-d2 + ls_week-plapr.
                WHEN '03'.
                  ls_entityset-d3 = ls_entityset-d3 + ls_week-plapr.
                WHEN '04'.
                  ls_entityset-d4 = ls_entityset-d4 + ls_week-plapr.
                WHEN '05'.
                  ls_entityset-d5 = ls_entityset-d5 + ls_week-plapr.
              ENDCASE.
            ENDLOOP.
          ENDLOOP.

*-- Material
          ls_entityset-matkl = ls_matkl-matkl.
          ls_entityset-matnr = ls_matkl-matnr.
          TRANSLATE ls_t023t-wgbez TO UPPER CASE.
          ls_entityset-wgbez   = ls_t023t-wgbez.
          ls_entityset-wgbez60 = ls_t023t-wgbez60.
          ls_entityset-acnum = lv_acnum.
          APPEND ls_entityset TO et_entityset.
          CLEAR: ls_entityset.
        ENDIF.
      ENDLOOP.
    ENDIF.

    SORT et_entityset BY extwg.

    IF et_entityset IS NOT INITIAL.
      IF lt_mara[] IS NOT INITIAL.
        SELECT *
          INTO TABLE @lt_zfmacvlcl
          FROM zfmacvlcl
          FOR ALL ENTRIES IN @lt_mara
         WHERE acnum EQ @lv_acnum
           AND matnr EQ @lt_mara-matnr.

        IF sy-subrc EQ 0.
          LOOP AT et_entityset INTO ls_entityset.
            lv_tabix = sy-tabix.
            LOOP AT lt_mara INTO DATA(ls_mara) WHERE matkl EQ ls_entityset-matkl.
              IF ls_mara-matnr EQ 'TMANGEN'
              OR ls_mara-matnr EQ 'TFORGEN'
              OR ls_mara-matnr EQ 'TIMGEN'
              OR ls_mara-matnr EQ 'TCOLGEN'.
                CONTINUE.
              ENDIF.
              LOOP AT lt_zfmacvlcl INTO DATA(ls_rend) WHERE matnr EQ ls_mara-matnr.
                ls_entityset-acarx = ls_rend-acarx.
                lv_total = lv_total + ls_rend-acren.
                ADD 1 TO lv_count.
              ENDLOOP.
            ENDLOOP.

            IF lv_count IS NOT INITIAL.
              ls_entityset-media_rend = lv_total / lv_count.
            ENDIF.

            MODIFY et_entityset FROM ls_entityset INDEX lv_tabix.
            CLEAR: ls_entityset, lv_total, lv_count.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.

    LOOP AT et_entityset ASSIGNING FIELD-SYMBOL(<ls_entityset>).
       <ls_entityset>-wgbez = <ls_entityset>-matkl(4) && '-' && <ls_entityset>-wgbez(35).
    ENDLOOP.

    SORT et_entityset BY wgbez ASCENDING.

  ENDMETHOD.


  METHOD gethorasefetset_get_entityset.

    DATA : lv_werks  TYPE werks_d,
           lv_period TYPE period,
           lv_total  TYPE zfmacwork_shift-actut,
           r_arbpl   TYPE /iwbep/t_cod_select_options,
           lv_media  TYPE zfmacwork_shift-actut.

    LOOP AT it_filter_select_options INTO DATA(ls_filters).
      CASE ls_filters-property.
        WHEN 'Werks'.
          lv_werks = ls_filters-select_options[ 1 ]-low.
        WHEN 'Period'.
          lv_period = ls_filters-select_options[ 1 ]-low.
        WHEN 'Arbpl'.
          r_arbpl = ls_filters-select_options.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE et_entityset
      FROM zfmacwork_shift
      WHERE werks  EQ lv_werks
        AND period EQ lv_period
        AND arbpl  IN r_arbpl.

    IF sy-subrc EQ 0.
      LOOP AT et_entityset INTO DATA(ls_entityset).
        lv_total = lv_total + ls_entityset-actut.
      ENDLOOP.
    ENDIF.

    DATA(lv_lines) = lines( et_entityset ).
    IF lv_lines IS NOT INITIAL.
      lv_media = lv_total / lv_lines.
    ENDIF.

    READ TABLE et_entityset INTO ls_entityset INDEX 1.
    IF sy-subrc EQ 0.
      ls_entityset-media = lv_media.
      MODIFY et_entityset FROM ls_entityset INDEX 1.
    ENDIF.

  ENDMETHOD.


  METHOD getmonthdateset_get_entity.

    TYPES: BEGIN OF ty_period,
            period type period,
          end of ty_period.

    DATA: lt_period TYPE TABLE OF ty_period,
          ls_period TYPE ty_period.

    DATA: lv_begda TYPE begda,
          lv_endda TYPE begda,
          lv_month TYPE c LENGTH 2,
          lv_tabix TYPE sy-tabix,
          lv_descr TYPE string,
          lv_char  TYPE c LENGTH 15,
          lv_date  TYPE c LENGTH 10.

    DATA: lt_months TYPE TABLE OF t247,
          lt_dates TYPE /scwm/tt_lm_dates,
          ls_dates LIKE LINE OF lt_dates.


    READ TABLE it_key_tab INTO DATA(ls_key) INDEX 1.
    IF sy-subrc EQ 0.
*      lv_begda = ls_key-value+6(4) && ls_key-value+3(2) && ls_key-value(2).
      lv_begda = ls_key-value.
    ENDIF.

*    CALL FUNCTION '/CPD/ADD_MONTH_TO_DATE'
*      EXPORTING
*        months        = '12'
*        olddate       = lv_begda
*      IMPORTING
*        newdate       = lv_endda.

     lv_endda = lv_begda(4) && '12' && '31'.

     CALL FUNCTION '/SCWM/DATES_BETWEEN_TWO_DATES'
       EXPORTING
         iv_begda       = lv_begda
         iv_endda       = lv_endda
       IMPORTING
         et_dates       = lt_dates.

     LOOP AT lt_dates INTO ls_dates.
       ls_period-period = ls_dates(6).
       APPEND ls_period TO lt_period.
     ENDLOOP.

     DELETE ADJACENT DUPLICATES FROM lt_period.

     LOOP AT lt_period INTO ls_period.
       lv_tabix = sy-tabix.
       CASE lv_tabix.
       	WHEN '01'.
          er_entity-d1 = ls_period-period+4(2) && '.' && ls_period-period(4) .
       	WHEN '02'.
          er_entity-d2 = ls_period-period+4(2) && '.' && ls_period-period(4) .
       	WHEN '03'.
          er_entity-d3 = ls_period-period+4(2) && '.' && ls_period-period(4) .
       	WHEN '04'.
          er_entity-d4 = ls_period-period+4(2) && '.' && ls_period-period(4) .
       	WHEN '05'.
          er_entity-d5 = ls_period-period+4(2) && '.' && ls_period-period(4) .
       	WHEN '06'.
          er_entity-d6 = ls_period-period+4(2) && '.' && ls_period-period(4) .
       	WHEN '07'.
          er_entity-d7 = ls_period-period+4(2) && '.' && ls_period-period(4) .
       	WHEN '08'.
          er_entity-d8 = ls_period-period+4(2) && '.' && ls_period-period(4) .
       	WHEN '09'.
          er_entity-d9 = ls_period-period+4(2) && '.' && ls_period-period(4) .
       	WHEN '10'.
          er_entity-d10 = ls_period-period+4(2) && '.' && ls_period-period(4) .
       	WHEN '11'.
          er_entity-d11 = ls_period-period+4(2) && '.' && ls_period-period(4) .
       	WHEN '12'.
          er_entity-d12 = ls_period-period+4(2) && '.' && ls_period-period(4) .
       	WHEN OTHERS.
       ENDCASE.
     ENDLOOP.

     er_entity-begda = lv_begda.

  ENDMETHOD.


  METHOD getmonthdateset_get_entityset.

    TYPES: BEGIN OF ty_period,
            period TYPE period,
          END OF ty_period.

    DATA: lt_period TYPE TABLE OF ty_period,
          ls_period TYPE ty_period.

    DATA: lv_begda TYPE begda,
          lv_endda TYPE begda,
          lv_month TYPE c LENGTH 2,
          lv_tabix TYPE sy-tabix,
          lv_descr TYPE string,
          lv_char  TYPE c LENGTH 15,
          lv_date  TYPE c LENGTH 10.

    DATA: lt_months TYPE TABLE OF t247,
          lt_dates TYPE /scwm/tt_lm_dates,
          lv_data TYPE begda,
          lv_safra TYPE string,
          lv_acnum TYPE string,
          lv_mes TYPE string,
          lv_endda_ano TYPE endda,
          lv_begda_ano TYPE endda,
          ls_dates LIKE LINE OF lt_dates.

    DATA: ls_entityset LIKE LINE OF et_entityset.

*    READ TABLE it_key_tab INTO DATA(ls_key) INDEX 1.
*    IF sy-subrc EQ 0.
**      lv_begda = ls_key-value+6(4) && ls_key-value+3(2) && ls_key-value(2).
*      lv_begda = ls_key-value.
*    ENDIF.

    LOOP AT it_filter_select_options INTO DATA(ls_filters).
      CASE ls_filters-property.
        WHEN 'Begda'.
          lv_begda = ls_filters-select_options[ 1 ]-low.
        WHEN 'Endda'.
          lv_endda = ls_filters-select_options[ 1 ]-low.
        WHEN 'Safra'.
          lv_safra = ls_filters-select_options[ 1 ]-low.
        WHEN 'Mes'.
          lv_mes = ls_filters-select_options[ 1 ]-low.
        WHEN 'Acnum'.
          lv_acnum = ls_filters-select_options[ 1 ]-low.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

    DATA: lt_area TYPE TABLE OF zfmachdr.
    DATA: lt_mensal TYPE TABLE OF zfmplmens,
          ls_mensal LIKE LINE OF lt_mensal.

    DATA: lv_ajahr TYPE ajahr.

*********************************************************************
    SELECT *
      INTO TABLE lt_area
      FROM zfmachdr
      WHERE acnum EQ lv_acnum AND
            ajahr EQ lv_safra.
      IF sy-subrc EQ 0.
        SORT lt_area BY acnum ajahr.
        READ TABLE lt_area INTO DATA(ls_area) INDEX 1.
        IF sy-subrc EQ 0.
          lv_begda_ano = ls_area-datab.
          lv_endda_ano = ls_area-datbi.
        ENDIF.
      ENDIF.
**********************************************************************


    IF lv_begda IS NOT INITIAL.
      lv_begda = lv_mes+3(4) && lv_mes(2) && '01'.
      CALL FUNCTION 'HR_CL_SUBTRACT_MONTH_TO_DATE'
        EXPORTING
          p_date             = lv_begda
          p_backmonths       = '4'
        IMPORTING
          p_newdate          = lv_begda.

      CALL FUNCTION '/CPD/ADD_MONTH_TO_DATE'
        EXPORTING
          months        = '4'
          olddate       = lv_begda
        IMPORTING
          newdate       = lv_endda.
    ELSE.

    IF lv_endda IS NOT INITIAL.
      lv_begda = lv_mes+3(4) && lv_mes(2) && '01'.
      CALL FUNCTION '/CPD/ADD_MONTH_TO_DATE'
        EXPORTING
          months        = '4'
          olddate       = lv_begda
        IMPORTING
          newdate       = lv_endda.

    ENDIF.

    ENDIF.

    IF lv_begda IS INITIAL.
      lv_begda = lv_begda_ano.
    ELSE.
      IF lv_begda LT lv_begda_ano.
        lv_begda = lv_begda_ano.
      ENDIF.
      ENDIF.
      IF lv_endda IS INITIAL.
       lv_endda = lv_endda_ano.
       ELSE.
      IF lv_endda GT lv_endda_ano.
        lv_endda = lv_endda_ano.
      ENDIF.
      ENDIF.
     CALL FUNCTION '/SCWM/DATES_BETWEEN_TWO_DATES'
       EXPORTING
         iv_begda       = lv_begda
         iv_endda       = lv_endda
       IMPORTING
         et_dates       = lt_dates.

     LOOP AT lt_dates INTO ls_dates.
       ls_period-period = ls_dates(6).
       APPEND ls_period TO lt_period.
     ENDLOOP.

     DELETE ADJACENT DUPLICATES FROM lt_period.

     LOOP AT lt_period INTO ls_period.
       lv_tabix = sy-tabix.
       CASE lv_tabix.
       	WHEN '01'.
          ls_entityset-d1 = ls_period-period+4(2) && '.' && ls_period-period(4) .
       	WHEN '02'.
          ls_entityset-d2 = ls_period-period+4(2) && '.' && ls_period-period(4) .
       	WHEN '03'.
          ls_entityset-d3 = ls_period-period+4(2) && '.' && ls_period-period(4) .
       	WHEN '04'.
          ls_entityset-d4 = ls_period-period+4(2) && '.' && ls_period-period(4) .
       	WHEN '05'.
          ls_entityset-d5 = ls_period-period+4(2) && '.' && ls_period-period(4) .
*         WHEN '06'.
*          ls_entityset-d6 = ls_period-period+4(2) && '.' && ls_period-period(4) .
*         WHEN '07'.
*          er_entity-d7 = ls_period-period+4(2) && '.' && ls_period-period(4) .
*         WHEN '08'.
*          er_entity-d8 = ls_period-period+4(2) && '.' && ls_period-period(4) .
*         WHEN '09'.
*          er_entity-d9 = ls_period-period+4(2) && '.' && ls_period-period(4) .
*         WHEN '10'.
*          er_entity-d10 = ls_period-period+4(2) && '.' && ls_period-period(4) .
*         WHEN '11'.
*          er_entity-d11 = ls_period-period+4(2) && '.' && ls_period-period(4) .
*         WHEN '12'.
*          er_entity-d12 = ls_period-period+4(2) && '.' && ls_period-period(4) .
       	WHEN OTHERS.
       ENDCASE.
     ENDLOOP.
     APPEND ls_entityset TO et_entityset.
*     er_entity-begda = lv_begda.

  ENDMETHOD.


  METHOD getplanningset_create_entity.

    DATA: lwa_entity           TYPE zcl_zfarm_agriplan_mpc=>ts_getplanning,
          ls_mensal            TYPE zfmplmens,
          lt_mensal            TYPE TABLE OF zfmplmens,
          lt_dates             TYPE /scwm/tt_lm_dates,
          lr_acnum             TYPE RANGE OF zfmacnum,
          lr_matnr             TYPE RANGE OF zfmaplmatnr,
          lr_periodo           TYPE RANGE OF spmon,
          ls_periodo           LIKE LINE OF lr_periodo,
          lv_tabix             TYPE sy-tabix,
          lv_osql_where_clause TYPE string,
          lv_top               TYPE i,
          lv_skip              TYPE i,
          lv_max_index         TYPE i,
          lv_endda             TYPE endda,
          lv_begda             TYPE begda,
          l_n                  TYPE i,
          lv_acnum             TYPE zfmacnum,
          lv_matkl             TYPE matkl,
          lv_begda_ano         TYPE begda,
          lv_endda_ano         TYPE endda,
          lv_extwg             TYPE extwg,
          lv_ajahr             TYPE ajahr,
          lt_area              TYPE TABLE OF zfmachdr.

    io_data_provider->read_entry_data( IMPORTING es_data = er_entity ).
    lv_acnum = er_entity-acnum.
    lv_ajahr = er_entity-ajahr.

    SELECT *
      INTO TABLE lt_area
      FROM zfmachdr
      WHERE acnum EQ lv_acnum AND
            ajahr EQ lv_ajahr.
    IF sy-subrc EQ 0.
      SORT lt_area BY acnum ajahr.
      READ TABLE lt_area INTO DATA(ls_area) INDEX 1.
      IF sy-subrc EQ 0.
        lv_begda_ano = ls_area-datab.
        lv_endda_ano = ls_area-datbi.
      ENDIF.
    ENDIF.

    ls_periodo-sign = 'I'.
    ls_periodo-option = 'EQ'.

    ls_periodo-low = er_entity-periodo1_txt+3(4) && er_entity-periodo1_txt(2).
    APPEND ls_periodo TO lr_periodo.
    ls_periodo-low = er_entity-periodo2_txt+3(4) && er_entity-periodo2_txt(2).
    APPEND ls_periodo TO lr_periodo.
    ls_periodo-low = er_entity-periodo3_txt+3(4) && er_entity-periodo3_txt(2).
    APPEND ls_periodo TO lr_periodo.
    ls_periodo-low = er_entity-periodo4_txt+3(4) && er_entity-periodo4_txt(2).
    APPEND ls_periodo TO lr_periodo.
    ls_periodo-low = er_entity-periodo5_txt+3(4) && er_entity-periodo5_txt(2).
    APPEND ls_periodo TO lr_periodo.

    DELETE ADJACENT DUPLICATES FROM lr_periodo COMPARING low.

    SELECT *
      INTO TABLE lt_mensal
      FROM zfmplmens
     WHERE acnum EQ er_entity-acnum
       AND extwg EQ er_entity-extwg
       AND matkl EQ er_entity-matkl
       AND periodo IN lr_periodo.

    IF sy-subrc EQ 0.
      SORT lt_mensal BY acnum extwg matkl periodo.
    ENDIF.

    LOOP AT lr_periodo INTO ls_periodo.
      lv_tabix = sy-tabix.

      ls_mensal-acnum = er_entity-acnum.
      ls_mensal-extwg = er_entity-extwg.
      ls_mensal-matkl = er_entity-matkl.
      ls_mensal-periodo = ls_periodo-low.

      READ TABLE lt_mensal INTO DATA(ls_mens)
        WITH KEY acnum = ls_mensal-acnum
                 extwg = ls_mensal-extwg
                 matkl = ls_mensal-matkl
                 periodo = ls_mensal-periodo BINARY SEARCH.

      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING ls_mens TO ls_mensal.
      ENDIF.

      CASE lv_tabix.
        WHEN '1'.
          IF er_entity-caracteristica EQ 'Área'.
            ls_mensal-qtplm = er_entity-valor1.
          ELSE.
            ls_mensal-qtd_passadas = er_entity-valor1.
          ENDIF.
        WHEN '2'.
          IF er_entity-caracteristica EQ 'Área'.
            ls_mensal-qtplm = er_entity-valor2.
          ELSE.
            ls_mensal-qtd_passadas = er_entity-valor2.
          ENDIF.
        WHEN '3'.
          IF er_entity-caracteristica EQ 'Área'.
            ls_mensal-qtplm = er_entity-valor3.
          ELSE.
            ls_mensal-qtd_passadas = er_entity-valor3.
          ENDIF.
        WHEN '4'.
          IF er_entity-caracteristica EQ 'Área'.
            ls_mensal-qtplm = er_entity-valor4.
          ELSE.
            ls_mensal-qtd_passadas = er_entity-valor4.
          ENDIF.
        WHEN '5'.
          IF er_entity-caracteristica EQ 'Área'.
            ls_mensal-qtplm = er_entity-valor5.
          ELSE.
            ls_mensal-qtd_passadas = er_entity-valor5.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.

      MODIFY zfmplmens FROM ls_mensal.
    ENDLOOP.

  ENDMETHOD.


  METHOD getplanningset_get_entity.

    DATA: lv_acnum   TYPE zfmacnum,
          lv_matnr   TYPE zfmaplmatnr,
          lv_periodo TYPE spmon.

    DATA: lwa_key_values TYPE zcl_zfarm_agriplan_mpc=>ts_getplanning.

*    IF sy-uname EQ 'T_T.KONNO'.
*      BREAK-POINT.
*    ENDIF.

    CALL METHOD io_tech_request_context->get_converted_keys
      IMPORTING
        es_key_values = lwa_key_values.

    SELECT SINGLE *
      FROM zfmplmens
      INTO CORRESPONDING FIELDS OF @er_entity
     WHERE acnum   EQ @lwa_key_values-acnum
       AND extwg   EQ @lwa_key_values-extwg
       AND matkl   EQ @lwa_key_values-matkl
       AND periodo EQ @lwa_key_values-periodo.

  ENDMETHOD.


  METHOD getplanningset_get_entityset.

    DATA: lr_acnum   TYPE RANGE OF zfmacnum,
          lr_matnr   TYPE RANGE OF zfmaplmatnr,
          lr_periodo TYPE RANGE OF spmon,
          ls_periodo LIKE LINE OF lr_periodo.

    DATA: lv_osql_where_clause TYPE string,
          lv_top               TYPE i,
          lv_skip              TYPE i,
          lv_max_index         TYPE i,
          lv_endda             TYPE endda,
          lv_begda             TYPE begda,
          l_n                  TYPE i.

    DATA: lt_dates TYPE /scwm/tt_lm_dates.

    DATA: ls_entityset LIKE LINE OF et_entityset.

    DATA: lv_acnum TYPE zfmacnum,
          lv_matkl TYPE matkl,
          lv_tabix TYPE sy-tabix,
          lv_begda_ano TYPE begda,
          lv_endda_ano TYPE endda,
          lv_extwg TYPE extwg.

    DATA: lv_ajahr TYPE ajahr.
    DATA: lt_area TYPE TABLE OF zfmachdr.
    DATA: lt_mensal TYPE TABLE OF zfmplmens,
          ls_mensal LIKE LINE OF lt_mensal.

    LOOP AT it_filter_select_options INTO DATA(ls_filters).
      CASE ls_filters-property.
        WHEN 'Acnum'.
          lv_acnum = ls_filters-select_options[ 1 ]-low.
        WHEN 'Matkl'.
          lv_matkl = ls_filters-select_options[ 1 ]-low.
        WHEN 'Begda'.
          lv_begda = ls_filters-select_options[ 1 ]-low.
        WHEN 'Endda'.
          lv_endda = ls_filters-select_options[ 1 ]-low.
        WHEN 'Extwg'.
          lv_extwg = ls_filters-select_options[ 1 ]-low.
        WHEN 'Ajahr'.
          lv_ajahr = ls_filters-select_options[ 1 ]-low.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

*********************************************************************
    SELECT *
      INTO TABLE lt_area
      FROM zfmachdr
      WHERE acnum EQ lv_acnum AND
            ajahr EQ lv_ajahr.
      IF sy-subrc EQ 0.
        SORT lt_area BY acnum ajahr.
        READ TABLE lt_area INTO DATA(ls_area) INDEX 1.
        IF sy-subrc EQ 0.
          lv_begda_ano = ls_area-datab.
          lv_endda_ano = ls_area-datbi.
        ENDIF.
      ENDIF.
**********************************************************************

    IF lv_endda IS NOT INITIAL.
      lv_begda = lv_endda.
      CLEAR: lv_endda.
    CALL FUNCTION '/CPD/ADD_MONTH_TO_DATE'
      EXPORTING
        months        = '4'
        olddate       = lv_begda
      IMPORTING
        newdate       = lv_endda.

    ELSE.

    IF lv_begda IS NOT INITIAL.
      lv_begda = lv_begda.

      CALL FUNCTION 'HR_CL_SUBTRACT_MONTH_TO_DATE'
        EXPORTING
          p_date             = lv_begda
          p_backmonths       = '4'
        IMPORTING
          p_newdate          = lv_begda.

      CALL FUNCTION '/CPD/ADD_MONTH_TO_DATE'
        EXPORTING
          months        = '4'
          olddate       = lv_begda
        IMPORTING
          newdate       = lv_endda.

    ELSE.
      lv_begda = lv_begda_ano.
      CALL FUNCTION '/CPD/ADD_MONTH_TO_DATE'
        EXPORTING
          months        = '4'
          olddate       = lv_begda
        IMPORTING
          newdate       = lv_endda.
    ENDIF.

       ENDIF.

     IF lv_begda LT lv_begda_ano.
       lv_begda = lv_begda_ano.
     ENDIF.
     IF lv_endda GT lv_endda_ano.
       lv_endda = lv_endda_ano.
     ENDIF.

    CALL FUNCTION '/SCWM/DATES_BETWEEN_TWO_DATES'
      EXPORTING
        iv_begda       = lv_begda
        iv_endda       = lv_endda
      IMPORTING
        et_dates       = lt_dates.

    LOOP AT lt_dates INTO DATA(ls_dates).
      ls_periodo-sign = 'I'.
      ls_periodo-option = 'EQ'.
      ls_periodo-low = ls_dates(6).
      APPEND ls_periodo TO lr_periodo.
    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM lr_periodo COMPARING low.

    SELECT *
      FROM zfmplmens
      INTO TABLE lt_mensal
      WHERE acnum EQ lv_acnum AND
            matkl EQ lv_matkl AND
            extwg EQ lv_extwg AND
            periodo IN lr_periodo.
      IF sy-subrc EQ 0.
        SORT lt_mensal BY acnum extwg matkl periodo.
      ENDIF.

* Área
      LOOP AT lr_periodo INTO ls_periodo.
        lv_tabix = sy-tabix.
        LOOP AT lt_mensal INTO ls_mensal WHERE periodo EQ ls_periodo-low.
          CASE lv_tabix.
            WHEN '1'.
              ls_entityset-valor1 = ls_mensal-qtplm.
            WHEN '2'.
              ls_entityset-valor2 = ls_mensal-qtplm.
            WHEN '3'.
              ls_entityset-valor3 = ls_mensal-qtplm.
            WHEN '4'.
              ls_entityset-valor4 = ls_mensal-qtplm.
            WHEN '5'.
              ls_entityset-valor5 = ls_mensal-qtplm.
            WHEN OTHERS.
          ENDCASE.
        ENDLOOP.

        CASE lv_tabix.
          WHEN '1'.
              ls_entityset-periodo1_txt = ls_periodo-low+4(2) && '.' && ls_periodo-low(4).
          WHEN '2'.
              ls_entityset-periodo2_txt = ls_periodo-low+4(2) && '.' && ls_periodo-low(4).
          WHEN '3'.
              ls_entityset-periodo3_txt = ls_periodo-low+4(2) && '.' && ls_periodo-low(4).
          WHEN '4'.
              ls_entityset-periodo4_txt = ls_periodo-low+4(2) && '.' && ls_periodo-low(4).
          WHEN '5'.
              ls_entityset-periodo5_txt = ls_periodo-low+4(2) && '.' && ls_periodo-low(4).
          WHEN OTHERS.
        ENDCASE.

      ENDLOOP.

      ls_entityset-caracteristica = 'Área'.
      ls_entityset-acnum = lv_acnum.
      ls_entityset-extwg = lv_extwg.
      ls_entityset-matkl = lv_matkl.
      APPEND ls_entityset TO et_entityset.
      CLEAR: ls_entityset.
* Passadas

      LOOP AT lr_periodo INTO ls_periodo.
        lv_tabix = sy-tabix.
        LOOP AT lt_mensal INTO ls_mensal WHERE periodo EQ ls_periodo-low.
          CASE lv_tabix.
            WHEN '1'.
              ls_entityset-valor1 = ls_mensal-qtd_passadas.
            WHEN '2'.
              ls_entityset-valor2 = ls_mensal-qtd_passadas.
            WHEN '3'.
              ls_entityset-valor3 = ls_mensal-qtd_passadas.
            WHEN '4'.
              ls_entityset-valor4 = ls_mensal-qtd_passadas.
            WHEN '5'.
              ls_entityset-valor5 = ls_mensal-qtd_passadas.
            WHEN OTHERS.
          ENDCASE.
        ENDLOOP.
      ENDLOOP.

      ls_entityset-caracteristica = 'Passadas'.
      ls_entityset-acnum = lv_acnum.
      ls_entityset-extwg = lv_extwg.
      ls_entityset-matkl = lv_matkl.
      APPEND ls_entityset TO et_entityset.
      CLEAR: ls_entityset.

  ENDMETHOD.


  METHOD getplanningset_update_entity.

    DATA: lwa_entity    TYPE zcl_zfarm_agriplan_mpc=>ts_getplanning,
          lwa_zfmplmens TYPE zfmplmens.

*    IF sy-uname EQ 'T_T.KONNO'.
*      BREAK-POINT.
*    ENDIF.

    io_data_provider->read_entry_data( IMPORTING es_data = lwa_entity ).

    lwa_zfmplmens = CORRESPONDING #( lwa_entity ).

    lwa_zfmplmens-mandt = sy-mandt.
    UPDATE zfmplmens FROM lwa_zfmplmens.

    IF sy-subrc EQ 0
    OR sy-subrc EQ 4.
      er_entity = lwa_entity.
    ENDIF.

  ENDMETHOD.


  METHOD getplantarefasse_get_entityset.


    TYPES: BEGIN OF ty_matkl,
             matnr   TYPE matnr,
             matkl   TYPE matkl,
             wgbez   TYPE wgbez,
             wgbez60 TYPE wgbez60,
           END OF   ty_matkl.

    TYPES: BEGIN OF ty_vcalda,
             tplnr_fl TYPE  /agri/gltplnr_fl,
             acnum    TYPE  zfmacnum,
             vornr    TYPE vornr,
             matnr    TYPE zfmacmatnr,
             posnr    TYPE zfmacposnr,
             maktx    TYPE zfmacmaktx,
             acidt    TYPE zfmacidt,
             acdis    TYPE zfmacdis,
             acvcl    TYPE zfmacvcl,
             acfcb    TYPE zfmacfcb,
             acqtb    TYPE zfmacqtb,
             acuqb    TYPE zfmacuqb,
             acren    TYPE zfmacren,
             unren    TYPE zfmacunren,
             acpex    TYPE zfmacpex,
             acarx    TYPE zfmacarx,
             unarx    TYPE zfmacunarx,
           END OF   ty_vcalda.

    DATA: lt_plpo              TYPE TABLE OF plpo,
          lt_fmfpcom           TYPE TABLE OF /agri/s_fmfpcom,
          lt_fmfpitm           TYPE TABLE OF /agri/s_fmfpitm,
          lt_makt              TYPE TABLE OF makt,
          lt_zfmacvlcl         TYPE TABLE OF ty_vcalda,
          lt_vcalda            TYPE TABLE OF zfmacvlcl,
          lt_matkl             TYPE TABLE OF ty_matkl,
          lt_mara              TYPE TABLE OF ty_matkl,
          lt_t023t             TYPE TABLE OF t023t,
          lv_total             TYPE zfmacren,
          lt_zfmplmens         TYPE TABLE OF zfmplmens,
          lt_acnum             TYPE zt_fm_acnum,
          lt_achdr             TYPE zt_fmachdr,
          lt_acitm             TYPE zt_fmacitm,
          lt_week              TYPE TABLE OF zfmplprsemanal,
          r_mtart              TYPE /iwbep/t_cod_select_options,
          wa_mtart             LIKE LINE OF r_mtart,
          ls_entityset         LIKE LINE OF et_entityset,
          lv_pltyp             TYPE  zfmplann,
          lv_acnum             TYPE  zfmacnum,
          lv_begda             TYPE  zfmdatab,
          lv_begda_aux         TYPE char10,
          lv_endda_aux         TYPE  zfmdatbi,
          lv_extwg             TYPE  extwg,
          lv_month             TYPE c LENGTH 2,
          lv_tabix             TYPE sy-tabix,
          lv_descr             TYPE string,
          lv_media             TYPE zfmacren,
          lv_char              TYPE c LENGTH 15,
          lv_endda             TYPE endda,
          lv_string            TYPE string,
          lv_index             TYPE sy-tabix,
          lv_tot_prog          TYPE zfmplapr,
          lv_calc              TYPE zfmplapr,
          lv_date              TYPE c LENGTH 10,
          lr_acnum             TYPE RANGE OF zfmacnum,
          lr_matnr             TYPE RANGE OF zfmaplmatnr,
          lr_periodo           TYPE RANGE OF spmon,
          ls_periodo           LIKE LINE OF lr_periodo,
          lv_osql_where_clause TYPE string,
          lv_top               TYPE i,
          lv_skip              TYPE i,
          lv_max_index         TYPE i,
          lv_matkl             TYPE matkl,
          lv_count             TYPE i,
          l_n                  TYPE i,
          r_matnr              TYPE RANGE OF matnr,
          s_matnr              LIKE LINE OF r_matnr,
          lt_dates             TYPE /scwm/tt_lm_dates,
          lv_begda_ano         TYPE begda,
          lv_endda_ano         TYPE endda,
          lt_area              TYPE TABLE OF zfmachdr,
          lt_mensal            TYPE TABLE OF zfmplmens,
          ls_mensal            LIKE LINE OF lt_mensal,
          lv_ajahr             TYPE ajahr.

    LOOP AT it_filter_select_options INTO DATA(ls_filters).
      CASE ls_filters-property.
        WHEN 'Acnum'.
          lv_acnum = ls_filters-select_options[ 1 ]-low.
        WHEN 'Matkl'.
          lv_matkl = ls_filters-select_options[ 1 ]-low.
        WHEN 'Begda'.
          lv_begda = ls_filters-select_options[ 1 ]-low.
        WHEN 'Endda'.
          lv_endda = ls_filters-select_options[ 1 ]-low.
        WHEN 'Extwg'.
          lv_extwg = ls_filters-select_options[ 1 ]-low.
        WHEN 'Ajahr'.
          lv_ajahr = ls_filters-select_options[ 1 ]-low.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

    SELECT *
      INTO TABLE lt_area
      FROM zfmachdr
     WHERE acnum EQ lv_acnum
       AND ajahr EQ lv_ajahr.

    IF sy-subrc EQ 0.
      SORT lt_area BY acnum ajahr.
      READ TABLE lt_area INTO DATA(ls_area) INDEX 1.
      IF sy-subrc EQ 0.
        lv_begda_ano = ls_area-datab.
        lv_endda_ano = ls_area-datbi.
      ENDIF.
    ENDIF.

    IF lv_endda IS NOT INITIAL.
      lv_begda = lv_endda.
      CLEAR: lv_endda.
      CALL FUNCTION '/CPD/ADD_MONTH_TO_DATE'
        EXPORTING
          months  = '4'
          olddate = lv_begda
        IMPORTING
          newdate = lv_endda.
    ELSE.
      IF lv_begda IS NOT INITIAL.
        lv_begda = lv_begda.

        CALL FUNCTION 'HR_CL_SUBTRACT_MONTH_TO_DATE'
          EXPORTING
            p_date       = lv_begda
            p_backmonths = '4'
          IMPORTING
            p_newdate    = lv_begda.

        CALL FUNCTION '/CPD/ADD_MONTH_TO_DATE'
          EXPORTING
            months  = '4'
            olddate = lv_begda
          IMPORTING
            newdate = lv_endda.
      ELSE.
        lv_begda = lv_begda_ano.
        CALL FUNCTION '/CPD/ADD_MONTH_TO_DATE'
          EXPORTING
            months  = '4'
            olddate = lv_begda
          IMPORTING
            newdate = lv_endda.
      ENDIF.
    ENDIF.

    IF lv_begda LT lv_begda_ano.
      lv_begda = lv_begda_ano.
    ENDIF.
    IF lv_endda GT lv_endda_ano.
      lv_endda = lv_endda_ano.
    ENDIF.

    CALL FUNCTION '/SCWM/DATES_BETWEEN_TWO_DATES'
      EXPORTING
        iv_begda = lv_begda
        iv_endda = lv_endda
      IMPORTING
        et_dates = lt_dates.

    LOOP AT lt_dates INTO DATA(ls_dates).
      ls_periodo-sign = 'I'.
      ls_periodo-option = 'EQ'.
      ls_periodo-low = ls_dates(6).
      APPEND ls_periodo TO lr_periodo.
    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM lr_periodo COMPARING low.

    SELECT *
      FROM zfmplmens
      INTO TABLE lt_mensal
     WHERE acnum   EQ lv_acnum
       AND extwg   EQ lv_extwg
       AND periodo IN lr_periodo.
    IF sy-subrc EQ 0.
      SORT lt_mensal BY acnum extwg matkl periodo.
    ENDIF.

    r_mtart = zcl_agri_utilities=>zm_get_tvarvc( iv_name = 'ZAGRI_PROCESSO' ).

    wa_mtart-sign = 'I'.
    wa_mtart-option = 'EQ'.
    MODIFY r_mtart FROM wa_mtart TRANSPORTING sign option WHERE low <> ''.

    SELECT *
      INTO TABLE lt_t023t
      FROM t023t
      WHERE spras EQ sy-langu.

    IF sy-subrc EQ 0.
      SORT lt_t023t BY matkl.
    ENDIF.

    CASE lv_extwg.
      WHEN 'GIM'.
        s_matnr-sign = 'I'.
        s_matnr-option = 'CP'.
        s_matnr-low = 'TIM*'.
        APPEND s_matnr TO r_matnr.
      WHEN 'GTC'.
        s_matnr-sign = 'I'.
        s_matnr-option = 'CP'.
        s_matnr-low = 'TMAN*'.
        APPEND s_matnr TO r_matnr.
        s_matnr-low = 'TFOR*'.
        APPEND s_matnr TO r_matnr.
      WHEN 'GCO'.
        s_matnr-sign = 'I'.
        s_matnr-option = 'CP'.
        s_matnr-low = 'TCOL*'.
        APPEND s_matnr TO r_matnr.
      WHEN 'GIR'.
        s_matnr-sign = 'I'.
        s_matnr-option = 'CP'.
        s_matnr-low = 'TMAN*'.
        APPEND s_matnr TO r_matnr.
      WHEN OTHERS.
    ENDCASE.

    SELECT matnr matkl
      INTO CORRESPONDING FIELDS OF TABLE lt_matkl
      FROM mara
     WHERE matnr IN r_matnr
       AND mtart IN r_mtart
       AND extwg EQ lv_extwg
       AND matkl NE space.
    IF sy-subrc EQ 0.
      SORT lt_matkl BY matkl.
      lt_mara[] = lt_matkl[].

      DELETE ADJACENT DUPLICATES FROM lt_matkl COMPARING matkl.

      LOOP AT lt_matkl INTO DATA(ls_matkl).
        READ TABLE lt_t023t INTO DATA(ls_t023t)
          WITH KEY matkl = ls_matkl-matkl.
        IF sy-subrc EQ 0.
*-- Realizar cálculo por dia
          LOOP AT lr_periodo INTO ls_periodo.
            lv_tabix = sy-tabix.
*-- Ler programação mensal
            READ TABLE lt_mensal INTO ls_mensal
              WITH KEY matkl   = ls_matkl-matkl
                       periodo = ls_periodo-low.
            IF sy-subrc EQ 0.

              CASE lv_tabix.
                WHEN '1'.
                  ls_entityset-d1 = ls_mensal-qtplm.
                WHEN '2'.
                  ls_entityset-d2 = ls_mensal-qtplm.
                WHEN '3'.
                  ls_entityset-d3 = ls_mensal-qtplm.
                WHEN '4'.
                  ls_entityset-d4 = ls_mensal-qtplm.
                WHEN '5'.
                  ls_entityset-d5 = ls_mensal-qtplm.
                WHEN OTHERS.
              ENDCASE.
            ENDIF.
          ENDLOOP.

          ls_entityset-matnr = ls_matkl-matnr.
          ls_entityset-matkl = ls_matkl-matkl.
          ls_entityset-wgbez = ls_t023t-wgbez.
          TRANSLATE ls_entityset-wgbez TO UPPER CASE.
          APPEND ls_entityset TO et_entityset.
          CLEAR: ls_entityset.
        ENDIF.
      ENDLOOP.
    ENDIF.

    SORT et_entityset BY matkl.

    IF et_entityset[] IS NOT INITIAL.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE lt_zfmacvlcl
        FROM zfmacvlcl
        FOR ALL ENTRIES IN lt_mara
       WHERE acnum EQ lv_acnum
         AND matnr EQ lt_mara-matnr.

      IF sy-subrc EQ 0.
        LOOP AT et_entityset INTO ls_entityset.
          lv_index = sy-tabix.
          LOOP AT lt_mara INTO DATA(ls_mara) WHERE matkl EQ ls_entityset-matkl.
            LOOP AT lt_zfmacvlcl INTO DATA(ls_rend) WHERE matnr EQ ls_mara-matnr.
              lv_total = lv_total + ls_rend-acren.
              ADD 1 TO lv_count.
            ENDLOOP.
          ENDLOOP.
          IF lv_count IS NOT INITIAL.
            ls_entityset-media_rend = lv_total / lv_count.
          ENDIF.
          MODIFY et_entityset FROM ls_entityset INDEX lv_index.
          CLEAR: ls_entityset, lv_total, lv_count.
        ENDLOOP.
      ENDIF.
    ENDIF.

    SORT et_entityset BY wgbez ASCENDING.
    DELETE ADJACENT DUPLICATES FROM et_entityset COMPARING ALL FIELDS.

    LOOP AT et_entityset ASSIGNING FIELD-SYMBOL(<ls_entityset>).
       <ls_entityset>-wgbez = <ls_entityset>-matkl(4) && '-' && <ls_entityset>-wgbez(35).
    ENDLOOP.

  ENDMETHOD.


  METHOD getprocessoset_get_entityset.

    DATA: lt_grp_mara TYPE zct_grp_mara,
          ls_entityset LIKE LINE OF et_entityset.

    lt_grp_mara = zcl_agri_utilities=>zm_get_processos( ).

    LOOP AT lt_grp_mara INTO DATA(ls_grp_mara).
      ls_entityset-key   = ls_grp_mara-extwg.
      ls_entityset-descr = ls_grp_mara-ewbez.
      APPEND ls_entityset TO et_entityset.
      CLEAR: ls_entityset.
    ENDLOOP.

  ENDMETHOD.


METHOD getprodutosset_create_entity.

*-- Local Types
  TYPES: BEGIN OF ty_total,
           fazenda    TYPE string,
           kostl_form TYPE kostl,
           kostl_manu TYPE kostl,
           period     TYPE spmon,
           aareaform  TYPE zfmacqtb,
           aareamanu  TYPE zfmacqtb,
           bombasform TYPE zfmacqtb,
           bombasmanu TYPE zfmacqtb,
           tarefa     TYPE matnr,
           matnr      TYPE matnr,
           iwerk      TYPE werks_d,
           valor      TYPE verpr,
         END OF ty_total,

         BEGIN OF ty_prod_periodo,
           period   TYPE spmon,
           produtos TYPE zabs_del_qtd_tot,
         END OF ty_prod_periodo,

         BEGIN OF ly_mbew,
           matnr TYPE matnr,
           bwkey TYPE bwkey,
           bwtar TYPE bwtar_d,
           verpr TYPE verpr,
         END OF ly_mbew,

         BEGIN OF ty_fazenda,
           fazenda   TYPE /agri/gltplnr_fl,
           fazenda_d TYPE string,
           terreno   TYPE string,
           talhao    TYPE string,
           tplnr_fl  TYPE /agri/gltplnr_fl,
           kostlform TYPE kostl,
           kostlmanu TYPE kostl,
         END OF ty_fazenda,

         BEGIN OF ty_imp,
           tplnr_fl       TYPE /agri/gltplnr_fl,
           contr          TYPE /agri/gcontr,
           cmnum          TYPE /agri/glcmnum,
           varia          TYPE /agri/glvaria,
           season         TYPE /agri/gl_season,
           datab          TYPE /agri/gldatab,
           datbi          TYPE /agri/gldatbi,
           astat          TYPE /agri/glastat,
           loevm          TYPE loevm,
           datab_ref      TYPE /agri/gldatab,
           zzprevisto     TYPE zabs_del_previsto,
           zzprev_plantio TYPE zabs_del_prev_plantio,
           zzprev_errad   TYPE zabs_del_prev_errad,
         END OF ty_imp,

         BEGIN OF ty_period,
           period TYPE spmon,
         END OF ty_period.

*-- Local Declarations
*-- Tabelas Internas
  DATA: lt_consolidated   TYPE TABLE OF ty_total INITIAL SIZE 0,
        lt_imp            TYPE STANDARD TABLE OF ty_imp INITIAL SIZE 0,
        lt_tman           TYPE STANDARD TABLE OF ty_imp INITIAL SIZE 0,
        lt_prod_periodo   TYPE STANDARD TABLE OF ty_prod_periodo INITIAL SIZE 0,
        lt_detailed       TYPE TABLE OF ty_total INITIAL SIZE 0,
        lt_zfmacvlcl_1    TYPE TABLE OF zfmacvlcl INITIAL SIZE 0,
        lt_zfmacvlcl_2    TYPE TABLE OF zfmacvlcl INITIAL SIZE 0,
        lt_fazenda        TYPE TABLE OF ty_fazenda INITIAL SIZE 0,
        lt_log            TYPE STANDARD TABLE OF zabs_yorcamento,
        lt_dates          TYPE /scwm/tt_lm_dates,
        lt_period         TYPE TABLE OF ty_period INITIAL SIZE 0,
        lt_orcamento      TYPE TABLE OF zabs_orcamento INITIAL SIZE 0,
        lt_orc_aux        TYPE TABLE OF zabs_orcamento INITIAL SIZE 0,
        lt_orc_collect    TYPE TABLE OF zabs_orcamento INITIAL SIZE 0,
        lrt_bwkey         TYPE RANGE OF bwkey,
        lrt_matnr         TYPE RANGE OF zfmacmatnr,
        lrt_tplnr_fl      TYPE RANGE OF /agri/gltplnr_fl,
*-- Estruturas
        ls_consolidated   TYPE ty_total,
        ls_orc_collect    LIKE LINE OF lt_orc_collect,
        ls_orcamento      TYPE zabs_orcamento,
        ls_prod_periodo   LIKE LINE OF lt_prod_periodo, atnam,
        ls_detailed       TYPE ty_total,
        ls_zfmacvlcl      TYPE zfmacvlcl,
        ls_fazenda        TYPE ty_fazenda,
        lrs_tplnr_fl      LIKE LINE OF lrt_tplnr_fl,
*-- Variáveis
        lv_begda_month    TYPE begda,
        lv_endda_month    TYPE endda,
        lv_acnum          TYPE zfmacnum,
        lv_period         TYPE char6,
        lv_matkl          TYPE matkl,
        lv_begda          TYPE begda,
        lv_endda          TYPE endda,
        lv_tfor           TYPE matnr,
        lv_tman           TYPE matnr,
        lv_timp           TYPE matnr,
        lv_season         TYPE /agri/gl_season,
        lv_prod_dif       TYPE zabs_del_qtd_tot,
        lv_period_x       TYPE /agri/gldatab,
        lv_date_x         TYPE sydatum,
        lv_tol_min        TYPE zabs_del_qtd_tot VALUE '0.000',
        lv_tol_max        TYPE zabs_del_qtd_tot VALUE '10.000',
        lv_rcnum          TYPE zfmrcnum,
        lv_auxiliar       TYPE f,
        lv_passadas       TYPE f,
        lv_produto        TYPE f,
        lv_tabix_c        TYPE char2,
        lv_field_md       TYPE fieldname,
        lv_field_mp       TYPE fieldname,
        lv_field_p        TYPE fieldname,
        lv_versao         TYPE zabs_del_ver_orc,
        lv_werks          TYPE werks_d,
        lv_mes_processado TYPE char7.

*-- Local Constants
  CONSTANTS: BEGIN OF c_crop_season_status,
               active   TYPE /agri/glastat VALUE 'A',
               inactive TYPE /agri/glastat VALUE 'I',
               closed   TYPE /agri/glastat VALUE 'C',
             END OF c_crop_season_status.

  io_data_provider->read_entry_data( IMPORTING es_data = er_entity ).
  lv_versao = er_entity-versao.
  lv_begda  = er_entity-begda.
  lv_endda  = er_entity-endda.
  lv_rcnum  = er_entity-rcnum1.
  lv_mes_processado = er_entity-period.
  lv_tman = 'TMAN' && er_entity-matnr+4(36).
  lv_tfor = 'TFOR' && er_entity-matnr+4(36).
  lv_timp = 'TIMP' && er_entity-matnr+4(36).

  CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
    EXPORTING
      date                      = lv_begda
    EXCEPTIONS
      plausibility_check_failed = 1
      OTHERS                    = 2.

  IF sy-subrc <> 0.
    DATA(lv_invalid_date) = abap_true.
  ELSE.
    lv_invalid_date = abap_false.
  ENDIF.

  IF lv_invalid_date = abap_false.
    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                      = lv_endda
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS                    = 2.

    IF sy-subrc <> 0.
      lv_invalid_date = abap_true.
    ELSE.
      lv_invalid_date = abap_false.
    ENDIF.
  ENDIF.

  IF lv_begda GT lv_endda.
    lv_invalid_date = abap_true.
  ENDIF.

  IF lv_invalid_date EQ abap_false.
    CALL FUNCTION '/SCWM/DATES_BETWEEN_TWO_DATES'
      EXPORTING
        iv_begda = lv_begda
        iv_endda = lv_endda
      IMPORTING
        et_dates = lt_dates.

    LOOP AT lt_dates INTO DATA(ls_date).
      DATA(lv_tabix) = sy-tabix.
      IF lv_period NE ls_date(6).
        INSERT INITIAL LINE INTO TABLE lt_period
          ASSIGNING FIELD-SYMBOL(<ls_period>).
        IF sy-subrc EQ 0.
          <ls_period>-period = lv_period = ls_date(6).
        ENDIF.
      ELSE.
        DELETE lt_dates INDEX lv_tabix.
      ENDIF.
    ENDLOOP.

    INSERT INITIAL LINE INTO TABLE lrt_matnr
     ASSIGNING FIELD-SYMBOL(<lrs_matnr>).
    IF sy-subrc EQ 0.
      <lrs_matnr> = 'IEQ'.
      <lrs_matnr>-low = er_entity-matnr.
    ENDIF.

*-- Fetch the Recipe Types used for Budget (Orçamento)
    SELECT rctyp, orcamento
      INTO TABLE @DATA(lt_rctyp)
      FROM ztfmrctyp
     WHERE orcamento EQ @abap_true.

    IF sy-subrc EQ 0.
      SELECT * UP TO 1 ROWS
        INTO @DATA(ls_receita)
        FROM zfmrchdr
       WHERE rcnum = @lv_rcnum.
      ENDSELECT.

      lv_werks = ls_receita-werks.
    ENDIF.

    IF er_entity-matnr(4) EQ 'TFOR'
    OR er_entity-matnr(4) EQ 'TIMP'.
      lv_season = 'SAFRA PLAN'.
    ELSEIF er_entity-matnr(4) EQ 'TMAN'.
      lv_season = 'SAFRA PROD'.
    ENDIF.
  ENDIF.

  IF lv_invalid_date EQ abap_false.
    LOOP AT lt_dates INTO DATA(ls_date_x).
      DATA(lv_meses) = sy-tabix.
*-- lv_mes_processado = 08.2020 [MM.AAAA]
*-- ls_date_x = 20200701 [AAAA.MM.DD]
      IF er_entity-massa EQ abap_true.
        lv_mes_processado = ls_date_x+4(2)  && '.' && ls_date_x(4).
      ELSE.
        lv_mes_processado = er_entity-period.
        IF lv_meses NE 1.
          EXIT.
        ENDIF.
      ENDIF.

      lv_date_x = lv_mes_processado+3(4) && lv_mes_processado(2) && '01'.
      lv_period_x = lv_mes_processado+3(4) && lv_mes_processado(2).

      REFRESH: lt_consolidated, lt_prod_periodo, lt_detailed,
               lt_zfmacvlcl_1, lt_fazenda, lt_log, lt_orcamento,
               lt_orc_aux, lt_orc_collect, lrt_bwkey, lrt_tplnr_fl.

      CLEAR: ls_consolidated, ls_orc_collect, ls_orcamento,
             ls_prod_periodo, ls_detailed, ls_zfmacvlcl,
             ls_fazenda, lrs_tplnr_fl.

      CLEAR: lv_acnum, lv_period, lv_matkl, lv_prod_dif, lv_auxiliar,
             lv_passadas, lv_produto, lv_tabix_c, lv_field_md,
             lv_field_mp, lv_field_p.


      SELECT s~tplnr_fl, s~contr, s~cmnum, s~varia,
             s~season, s~datab, s~datbi, s~loevm,
             s~zzprevisto, s~zzprev_plantio, s~zzprev_errad,
             v~matnr
        INTO TABLE @DATA(lt_tplnr_fl)
        FROM zfmaitm AS i
        INNER JOIN zfmacvlcl AS v
        ON  i~acnum    EQ v~acnum
        AND i~tplnr_fl EQ v~tplnr_fl
        INNER JOIN /agri/glflcma AS s
        ON s~tplnr_fl EQ v~tplnr_fl
       WHERE i~acnum  EQ @er_entity-acnum
         AND i~iwerk  EQ @lv_werks
         AND v~matnr  EQ @er_entity-matnr
*-- BOC T_T.KONNO-03.18.21
         AND s~cmnum  EQ 'CITROS'
*-- EOC T_T.KONNO-03.18.21
         AND s~season EQ @lv_season
         AND s~datab  LE @lv_date_x
         AND s~datbi  GE @lv_date_x
         AND s~astat  EQ @c_crop_season_status-active
         AND s~loevm  NE @abap_true.
      IF sy-subrc NE 0.
        REFRESH lt_tplnr_fl.
      ENDIF.

      SELECT s~tplnr_fl, s~contr, s~cmnum, s~varia,
             s~season, s~datab, s~datbi, s~loevm,
             s~zzprevisto, s~zzprev_plantio, s~zzprev_errad,
             v~matnr
        APPENDING TABLE @lt_tplnr_fl
        FROM zfmaitm AS i
        INNER JOIN zfmacvlcl AS v
        ON  i~acnum    EQ v~acnum
        AND i~tplnr_fl EQ v~tplnr_fl
        INNER JOIN /agri/glflcma AS s
        ON s~tplnr_fl EQ v~tplnr_fl
       WHERE i~acnum      EQ @er_entity-acnum
         AND i~iwerk      EQ @lv_werks
         AND v~matnr      EQ @er_entity-matnr
*-- BOC T_T.KONNO-03.18.21
         AND s~cmnum      EQ 'CITROS'
*-- EOC T_T.KONNO-03.18.21
         AND s~season     EQ @lv_season
         AND s~datab      LE @lv_date_x
         AND s~datbi      GE @lv_date_x
         AND s~loevm      NE @abap_true
         AND s~zzprevisto EQ @abap_true.

      DELETE ADJACENT DUPLICATES FROM lt_tplnr_fl COMPARING ALL FIELDS.

      IF lt_tplnr_fl[] IS NOT INITIAL.
        REFRESH lrt_tplnr_fl.
        LOOP AT lt_tplnr_fl INTO DATA(ls_tplnr_fl).
          lrs_tplnr_fl-sign = 'I'.
          lrs_tplnr_fl-option = 'EQ'.
          lrs_tplnr_fl-low = ls_tplnr_fl-tplnr_fl.
          APPEND lrs_tplnr_fl TO lrt_tplnr_fl.
        ENDLOOP.

*-- Fetch the items from the crop area to check the branchs
        SELECT i~acnum, i~acpos, i~tplnr_fl,
               i~iwerk, i~contr, i~cmnum,
               i~season, i~datab, i~datbi
          FROM zfmaitm AS i
          INNER JOIN zfmachdr AS h
          ON i~acnum EQ h~acnum
          INNER JOIN zfmacvlcl AS v
          ON  i~acnum    EQ v~acnum
          AND i~tplnr_fl EQ v~tplnr_fl
          INTO TABLE @DATA(lt_zfmacitm)
         WHERE h~acnum    EQ @er_entity-acnum
           AND i~tplnr_fl IN @lrt_tplnr_fl[]
           AND i~iwerk    EQ @lv_werks
           AND v~matnr    EQ @er_entity-matnr.

        IF sy-subrc EQ 0.
          DATA(lt_fmac_aux) = lt_zfmacitm[].
          SORT: lt_zfmacitm BY tplnr_fl,
                lt_fmac_aux BY iwerk.
          DELETE ADJACENT DUPLICATES FROM lt_fmac_aux COMPARING iwerk.

          REFRESH lrt_bwkey.
          LOOP AT lt_fmac_aux INTO DATA(ls_fmac_aux).
            INSERT INITIAL LINE INTO TABLE lrt_bwkey
              ASSIGNING FIELD-SYMBOL(<ls_bwkey>).
            IF sy-subrc EQ 0.
              <ls_bwkey> = 'IEQ'.
              <ls_bwkey>-low = ls_fmac_aux-iwerk.
            ENDIF.
          ENDLOOP.
        ELSE.
          REFRESH lt_zfmacitm.
        ENDIF.

        SELECT matnr, bwkey, bwtar, verpr
          FROM mbew
          INTO TABLE @DATA(lt_mbew)
         WHERE matnr EQ @er_entity-matnrins
           AND bwkey IN @lrt_bwkey[].
        IF sy-subrc EQ 0.
          SORT lt_mbew BY matnr bwkey.
        ELSE.
          REFRESH lt_mbew.
        ENDIF.

        SELECT verpr UP TO 1 ROWS
          INTO @DATA(lv_valor)
          FROM mbew
         WHERE matnr EQ @er_entity-matnrins
           AND bwkey EQ @lv_werks.
        ENDSELECT.
        IF sy-subrc NE 0.
          CLEAR lv_valor.
        ENDIF.

        SELECT *
          INTO TABLE @DATA(lt_zfmacvlcl)
          FROM zfmacvlcl
         WHERE acnum EQ @er_entity-acnum
           AND matnr IN @lrt_matnr[]
           AND tplnr_fl IN @lrt_tplnr_fl[].

        IF sy-subrc EQ 0.
          SORT lt_zfmacvlcl BY tplnr_fl matnr.

          SELECT s~tplnr_fl, s~contr, s~season, s~datab, s~datbi,
                 s~astat, s~anlnr, s~kostl, s~rtnid, s~loevm,
                 s~datab_ref, s~zzfazplantio,
                 s~zzprevisto, s~zzprev_plantio, s~zzprev_errad,
                 v~acnum, v~vornr, v~matnr, v~acqtb, v~acarx
            INTO TABLE @DATA(lt_glflcma)
            FROM /agri/glflcma AS s
            INNER JOIN zfmacvlcl AS v
            ON s~tplnr_fl = v~tplnr_fl
           WHERE v~acnum    EQ @er_entity-acnum
             AND v~matnr    IN @lrt_matnr[]
             AND v~tplnr_fl IN @lrt_tplnr_fl[]
*-- BOC T_T.KONNO-03.18.21
             AND s~cmnum    EQ 'CITROS'
*-- EOC T_T.KONNO-03.18.21
             AND s~astat    EQ @c_crop_season_status-active
             AND s~loevm    NE @abap_true.

*-- BOC T_T.KONNO-11/10/20
          SELECT s~tplnr_fl, s~contr, s~season, s~datab, s~datbi,
                 s~astat, s~anlnr, s~kostl, s~rtnid, s~loevm,
                 s~datab_ref, s~zzfazplantio,
                 s~zzprevisto, s~zzprev_plantio, s~zzprev_errad,
                 v~acnum, v~vornr, v~matnr, v~acqtb, v~acarx
            APPENDING TABLE @lt_glflcma
            FROM /agri/glflcma AS s
            INNER JOIN zfmacvlcl AS v
            ON s~tplnr_fl = v~tplnr_fl
           WHERE v~acnum      EQ @er_entity-acnum
             AND v~matnr      IN @lrt_matnr[]
             AND v~tplnr_fl   IN @lrt_tplnr_fl[]
*-- BOC T_T.KONNO-03.18.21
             AND s~cmnum      EQ 'CITROS'
*-- EOC T_T.KONNO-03.18.21
             AND s~loevm      NE @abap_true
             AND s~zzprevisto EQ @abap_true.

          IF er_entity-matnr(4) EQ 'TFOR'
          OR er_entity-matnr(4) EQ 'TIMP'.
            DELETE lt_glflcma WHERE season NE 'SAFRA PLAN'.
          ELSEIF er_entity-matnr(4) EQ 'TMAN'.
            DELETE lt_glflcma WHERE season NE 'SAFRA PROD'.
          ENDIF.
          DELETE lt_glflcma WHERE matnr NE er_entity-matnr.
*-- EOC T_T.KONNO-11/10/20

          IF lt_glflcma[] IS NOT INITIAL.
**-- If there are several seasons for a terrain, consider the last one created
            SORT lt_glflcma BY matnr    ASCENDING
                               tplnr_fl ASCENDING
                               contr    DESCENDING.

            DELETE ADJACENT DUPLICATES FROM lt_glflcma COMPARING matnr tplnr_fl contr.

*-- BOC T_T.KONNO - 03.19.20
            IF lv_date_x IS NOT INITIAL.
              CALL FUNCTION '/AGRI/G_LAST_DAY_OF_MONTHS'
                EXPORTING
                  i_day_in            = lv_date_x
                IMPORTING
                  e_last_day_of_month = lv_endda_month.
            ENDIF.
*-- EOC T_T.KONNO - 03.19.20

            LOOP AT lt_glflcma INTO DATA(ls_glflcma).
              DATA(lv_glflcma_tabix) = sy-tabix.

              IF lv_date_x IS NOT INITIAL.
                IF lv_date_x NOT BETWEEN ls_glflcma-datab
                                     AND ls_glflcma-datbi.
                  DELETE lt_glflcma INDEX lv_glflcma_tabix.
                  CONTINUE.
                ENDIF.
              ENDIF.

*-- BOC T_T.KONNO - 03.19.20
*-- Ao programar FORMAÇÃO, verificar se talhão não está no período de IMPLANTAÇÃO
*-- Épocas: 'SAFRA PLAN' e 'SAFRA PROD'
*-- Obs.: TFOR* e TIMP* contidos em Época (/AGRI/GLFLCMA-SEASON) 'SAFRA PLAN'
              IF er_entity-matnr(4) EQ 'TFOR'.
                IF ls_glflcma-zzprevisto EQ abap_true.
                  IF ls_glflcma-zzprev_plantio IS INITIAL.
                    INSERT INITIAL LINE INTO TABLE lt_imp
                      ASSIGNING FIELD-SYMBOL(<ls_imp>).
                    IF sy-subrc EQ 0.
                      MOVE-CORRESPONDING ls_glflcma TO <ls_imp>.
                    ENDIF.
                    DELETE lt_glflcma INDEX lv_glflcma_tabix.
                    CONTINUE.
                  ELSE.
                    IF lv_endda_month IS NOT INITIAL.
                      IF lv_endda_month LE ls_glflcma-zzprev_plantio.
                        INSERT INITIAL LINE INTO TABLE lt_imp
                          ASSIGNING <ls_imp>.
                        IF sy-subrc EQ 0.
                          MOVE-CORRESPONDING ls_glflcma TO <ls_imp>.
                        ENDIF.
                        DELETE lt_glflcma INDEX lv_glflcma_tabix.
                        CONTINUE.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ELSEIF er_entity-matnr(4) EQ 'TMAN'.
                IF ls_glflcma-zzprev_errad IS NOT INITIAL.
                  IF lv_date_x GT ls_glflcma-datbi.
                    INSERT INITIAL LINE INTO TABLE lt_tman
                      ASSIGNING FIELD-SYMBOL(<ls_tman>).
                    IF sy-subrc EQ 0.
                      MOVE-CORRESPONDING ls_glflcma TO <ls_tman>.
                    ENDIF.
                    DELETE lt_glflcma INDEX lv_glflcma_tabix.
                    CONTINUE.
                  ENDIF.
                ENDIF.
              ENDIF.
*-- EOC T_T.KONNO - 03.19.20

              INSERT INITIAL LINE INTO TABLE lt_fazenda
                ASSIGNING FIELD-SYMBOL(<ls_fazenda>).
              IF sy-subrc EQ 0.
                IF ls_glflcma-kostl IS NOT INITIAL.
                  <ls_fazenda>-kostlform = ls_glflcma-kostl.
                  <ls_fazenda>-kostlmanu = ls_glflcma-kostl.
                ELSEIF ls_glflcma-anlnr IS NOT INITIAL.
                  <ls_fazenda>-kostlform = ls_glflcma-anlnr.
                  <ls_fazenda>-kostlmanu = ls_glflcma-anlnr.
                ENDIF.

                CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
                  EXPORTING
                    input  = ls_glflcma-tplnr_fl
                  IMPORTING
                    output = <ls_fazenda>-terreno.

                <ls_fazenda>-tplnr_fl = ls_glflcma-tplnr_fl.
                SPLIT <ls_fazenda>-terreno AT '-'
                  INTO <ls_fazenda>-fazenda_d <ls_fazenda>-talhao.

                CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
                  EXPORTING
                    input  = <ls_fazenda>-fazenda_d
                  IMPORTING
                    output = <ls_fazenda>-fazenda.
              ENDIF.
            ENDLOOP.

            SORT lt_fazenda BY tplnr_fl.

*-- BOC T_T.KONNO - 03.19.20
            SORT lt_imp BY tplnr_fl contr.
            SORT lt_tman BY tplnr_fl contr.
*-- EOC T_T.KONNO - 03.19.20

            LOOP AT lt_dates INTO ls_date.
              IF ls_date NE lv_date_x.
                CONTINUE.
              ENDIF.
              READ TABLE lt_glflcma INTO ls_glflcma
                WITH KEY matnr = er_entity-matnr BINARY SEARCH.
              WHILE sy-subrc EQ 0.
                lv_tabix = sy-tabix + 1.

                CLEAR: ls_consolidated, ls_detailed, ls_fazenda.
                IF ls_date GE ls_glflcma-datab
                AND ls_date LE ls_glflcma-datbi.
                  IF ls_glflcma-matnr = lv_tman.
                    ls_consolidated-aareamanu  = ls_glflcma-acarx.
                    ls_consolidated-bombasmanu = ls_glflcma-acqtb.
                  ELSEIF ls_glflcma-matnr = lv_tfor
                      OR ls_glflcma-matnr = lv_timp.
                    ls_consolidated-aareaform  = ls_glflcma-acarx.
                    ls_consolidated-bombasform = ls_glflcma-acqtb.
                  ENDIF.
                ENDIF.

                READ TABLE lt_zfmacitm INTO DATA(ls_zfmacitm)
                  WITH KEY tplnr_fl = ls_glflcma-tplnr_fl BINARY SEARCH.
                IF sy-subrc NE 0.
                  CLEAR ls_zfmacitm.
                ENDIF.

                ls_consolidated-period = ls_date(6).
                ls_consolidated-tarefa = er_entity-matnr.
                COLLECT ls_consolidated INTO lt_consolidated.

                ls_detailed = ls_consolidated.
                READ TABLE lt_fazenda INTO ls_fazenda
                  WITH KEY tplnr_fl = ls_glflcma-tplnr_fl BINARY SEARCH.
                IF sy-subrc EQ 0.
                  ls_detailed-tarefa     = er_entity-matnr.
                  ls_detailed-fazenda    = ls_fazenda-tplnr_fl.
                  ls_detailed-matnr      = er_entity-matnrins.
                  ls_detailed-iwerk      = ls_zfmacitm-iwerk.
                  ls_detailed-kostl_form = ls_fazenda-kostlform.
                  ls_detailed-kostl_manu = ls_fazenda-kostlmanu.
                ENDIF.
                COLLECT ls_detailed INTO lt_detailed.

                READ TABLE lt_glflcma INTO ls_glflcma
                  INDEX lv_tabix COMPARING matnr.
              ENDWHILE.
            ENDLOOP.
          ENDIF.
        ENDIF.

        SORT: lt_consolidated BY period matnr,
              lt_detailed BY period tarefa.

        LOOP AT lt_detailed ASSIGNING FIELD-SYMBOL(<ls_detailed>).
          READ TABLE lt_mbew INTO DATA(ls_mbew)
            WITH KEY matnr = er_entity-matnrins
                     bwkey = <ls_detailed>-iwerk BINARY SEARCH.
          IF sy-subrc EQ 0.
            <ls_detailed>-valor = ls_mbew-verpr.
          ENDIF.
        ENDLOOP.

        REFRESH: lt_zfmacvlcl_1, lt_zfmacvlcl_2.
        lt_zfmacvlcl_1[] = lt_zfmacvlcl[].
        SORT lt_zfmacvlcl_1 BY matnr tplnr_fl.
        DELETE lt_zfmacvlcl_1 WHERE matnr NE er_entity-matnr.
        IF sy-subrc IS INITIAL.
          lt_zfmacvlcl_2[] = lt_zfmacvlcl_1[].
        ENDIF.

        READ TABLE lt_period INTO DATA(ls_period)
          WITH KEY period = lv_period_x.
        IF sy-subrc EQ 0.
          lv_tabix = sy-tabix.
          lv_tabix_c = lv_tabix.

          IF lt_zfmacvlcl_1[] IS INITIAL
          AND lt_zfmacvlcl_2[] IS NOT INITIAL.
            lt_zfmacvlcl_1[] = lt_zfmacvlcl_2[].
          ENDIF.

          READ TABLE lt_detailed INTO ls_detailed
            WITH KEY period = ls_period
                     tarefa = er_entity-matnr BINARY SEARCH.
          WHILE sy-subrc EQ 0.
            lv_tabix = sy-tabix + 1.

            READ TABLE lt_consolidated INTO ls_consolidated
              WITH KEY period = ls_period BINARY SEARCH.

            CLEAR ls_orcamento.
            ls_orcamento-acnum = er_entity-acnum.
            ls_orcamento-extwg = er_entity-extwg.
            ls_orcamento-matkl = er_entity-matkl.
            ls_orcamento-rcnum = lv_rcnum.
            ls_orcamento-matnr = er_entity-matnrins.

            CONCATENATE: 'M' lv_tabix_c 'D' INTO lv_field_md,
                         'M' lv_tabix_c 'P' INTO lv_field_mp,
                         'P' lv_tabix_c INTO lv_field_p.

            DATA(lv_field_exists) = abap_true.
            ASSIGN COMPONENT lv_field_md OF STRUCTURE er_entity
              TO FIELD-SYMBOL(<lv_field_md>).
            IF sy-subrc NE 0.
              lv_field_exists = abap_false.
            ENDIF.

            ASSIGN COMPONENT lv_field_mp OF STRUCTURE er_entity
              TO FIELD-SYMBOL(<lv_field_mp>).
            IF sy-subrc NE 0.
              lv_field_exists = abap_false.
            ENDIF.

            ASSIGN COMPONENT lv_field_p OF STRUCTURE er_entity
              TO FIELD-SYMBOL(<lv_field_p>).
            IF sy-subrc NE 0.
              lv_field_exists = abap_false.
            ENDIF.

            IF lv_field_exists = abap_false.
              EXIT.
            ELSE.
              ls_orcamento-matnr = er_entity-matnrins.
              ls_orcamento-period = ls_period-period.

              IF ls_detailed-kostl_manu IS NOT INITIAL.
                ls_orcamento-kostl = ls_detailed-kostl_manu.
              ELSEIF ls_detailed-kostl_form IS NOT INITIAL.
                ls_orcamento-kostl = ls_detailed-kostl_form.
              ENDIF.

              CLEAR: lv_auxiliar, lv_passadas.
*-- Orçamento via quantidade de produtos
              IF er_entity-produtos EQ 'X'.
                IF er_entity-tipo EQ 'FORMAÇÃO'.
                  IF ls_receita-ausme EQ 'VC'.
                    IF <lv_field_md> IS NOT INITIAL.
                      lv_auxiliar = <lv_field_p> / <lv_field_md>.
                      IF ls_consolidated-bombasform IS NOT INITIAL.
                        lv_passadas = lv_auxiliar / ls_consolidated-bombasform.
                        ls_orcamento-passadas = lv_passadas.
                      ENDIF.
                    ENDIF.

                    READ TABLE lt_zfmacvlcl_1 INTO ls_zfmacvlcl
                      WITH KEY tplnr_fl = ls_detailed-fazenda.
                    IF sy-subrc IS INITIAL.
                      IF ls_consolidated-bombasform IS NOT INITIAL.
                        ls_orcamento-produtos = ( ls_zfmacvlcl-acqtb /
                           ls_consolidated-bombasform ) * <lv_field_p>.
                      ENDIF.
                      ls_orcamento-fazenda = ls_zfmacvlcl-tplnr_fl.
                    ENDIF.
                    CLEAR ls_zfmacvlcl.
                  ELSEIF ls_receita-ausme EQ 'HC'.
                    READ TABLE lt_zfmacvlcl_1 INTO ls_zfmacvlcl
                      WITH KEY tplnr_fl = ls_detailed-fazenda.
                    IF sy-subrc IS INITIAL.
                      IF ls_consolidated-aareaform IS NOT INITIAL.
                        ls_orcamento-produtos = ( <lv_field_p> / ls_consolidated-aareaform )
                                                * ls_zfmacvlcl-acarx.
                      ENDIF.
                      ls_orcamento-fazenda = ls_zfmacvlcl-tplnr_fl.
                    ENDIF.

                    IF <lv_field_md> IS NOT INITIAL.
                      lv_auxiliar = ls_orcamento-produtos / <lv_field_md>.
                      IF ls_zfmacvlcl-acarx IS NOT INITIAL.
                        ls_orcamento-passadas = lv_auxiliar / ls_zfmacvlcl-acarx.
                      ENDIF.
                    ENDIF.
                    CLEAR ls_zfmacvlcl.
                  ENDIF.

                  ls_orcamento-fazenda = ls_detailed-fazenda.

                  IF er_entity-principal EQ 'X'.
                    ls_orcamento-aarea_form = ls_orcamento-passadas * ls_detailed-aareaform.
                  ENDIF.
                ELSEIF er_entity-tipo EQ 'MANUTENÇÃO'.
                  IF ls_receita-ausme EQ 'VC'.
                    IF <lv_field_md> IS NOT INITIAL.
                      lv_auxiliar = <lv_field_p> / <lv_field_md>.
                      IF ls_consolidated-bombasmanu IS NOT INITIAL.
                        lv_passadas = lv_auxiliar / ls_consolidated-bombasmanu.
                        ls_orcamento-passadas = lv_passadas.
                      ENDIF.
                    ENDIF.

                    READ TABLE lt_zfmacvlcl_1 INTO ls_zfmacvlcl
                      WITH KEY tplnr_fl = ls_detailed-fazenda.
                    IF sy-subrc IS INITIAL.
                      IF ls_consolidated-bombasmanu IS NOT INITIAL.
                        ls_orcamento-produtos = ( ls_zfmacvlcl-acqtb /
                           ls_consolidated-bombasmanu ) * <lv_field_p>.
                      ENDIF.
                      ls_orcamento-fazenda = ls_zfmacvlcl-tplnr_fl.
                    ENDIF.
                    CLEAR ls_zfmacvlcl.
                  ELSEIF ls_receita-ausme EQ 'HC'.
                    READ TABLE lt_zfmacvlcl_1 INTO ls_zfmacvlcl
                      WITH KEY tplnr_fl = ls_detailed-fazenda.
                    IF sy-subrc IS INITIAL.
                      IF ls_consolidated-aareamanu IS NOT INITIAL.
                        ls_orcamento-produtos = ( <lv_field_p> / ls_consolidated-aareamanu )
                                                * ls_zfmacvlcl-acarx.
                      ENDIF.
                      ls_orcamento-fazenda = ls_zfmacvlcl-tplnr_fl.
                    ENDIF.

                    IF <lv_field_md> IS NOT INITIAL.
                      lv_auxiliar = ls_orcamento-produtos / <lv_field_md>.
                      IF ls_zfmacvlcl-acarx IS NOT INITIAL.
                        ls_orcamento-passadas = lv_auxiliar / ls_zfmacvlcl-acarx.
                      ENDIF.
                    ENDIF.
                    CLEAR ls_zfmacvlcl.
                  ENDIF.

                  ls_orcamento-fazenda = ls_detailed-fazenda.

                  IF er_entity-principal EQ 'X'.
                    ls_orcamento-aarea_manu = ls_orcamento-passadas * ls_detailed-aareamanu.
                  ENDIF.
                ENDIF.

                ls_orcamento-custo = ls_orcamento-produtos * ls_detailed-valor.
                ls_orcamento-rcdos = <lv_field_md>.

                IF <lv_field_p> IS ASSIGNED.
                  ls_prod_periodo-period   = ls_period.
                  ls_prod_periodo-produtos = <lv_field_p>.
                  APPEND ls_prod_periodo TO lt_prod_periodo.
                ENDIF.
              ELSE.
                CLEAR lv_produto.

                ls_orcamento-fazenda  = ls_detailed-fazenda.
                ls_orcamento-passadas = <lv_field_mp>.
                ls_orcamento-rcdos    = <lv_field_md>.

                IF er_entity-tipo EQ 'FORMAÇÃO'.
                  ls_orcamento-aarea_form = ls_detailed-aareaform * <lv_field_mp>.
*-- Calcula Quantidade de Produto
                  IF ls_receita-ausme EQ 'VC'.
                    lv_produto = <lv_field_mp> * ls_detailed-bombasform * <lv_field_md>.
                  ELSEIF ls_receita-ausme EQ 'HC'.
                    lv_produto = <lv_field_mp> * ls_detailed-aareaform * <lv_field_md>.
                  ENDIF.
                ELSEIF er_entity-tipo EQ 'MANUTENÇÃO'.
                  ls_orcamento-aarea_manu = ls_detailed-aareamanu * <lv_field_mp>.
*-- Calcula Quantidade de Produto
                  IF ls_receita-ausme EQ 'VC'.
                    lv_produto = <lv_field_mp> * ls_detailed-bombasmanu * <lv_field_md>.
                  ELSEIF ls_receita-ausme EQ 'HC'.
                    lv_produto = <lv_field_mp> * ls_detailed-aareamanu * <lv_field_md>.
                  ENDIF.
                ENDIF.

                IF er_entity-principal NE 'X'.
                  CLEAR: ls_orcamento-aarea_manu, ls_orcamento-aarea_form.
                ENDIF.

                ls_orcamento-produtos = lv_produto.
                ls_orcamento-custo = ls_orcamento-produtos * ls_detailed-valor.
              ENDIF.
            ENDIF.

            IF ls_orcamento-produtos IS NOT INITIAL.
              ls_orcamento-versao = lv_versao.
              APPEND ls_orcamento TO lt_orc_aux.
              ls_orc_collect-period = ls_orcamento-period.
              ls_orc_collect-produtos = ls_orcamento-produtos.
              COLLECT ls_orc_collect INTO lt_orc_collect.
            ENDIF.

            ls_orcamento-versao = lv_versao.
            APPEND ls_orcamento TO lt_orcamento.

            READ TABLE lt_detailed INTO ls_detailed
              INDEX lv_tabix COMPARING period tarefa.
          ENDWHILE.
        ENDIF.

        IF lv_period_x IS NOT INITIAL.
          DELETE lt_orcamento WHERE period NE lv_period_x.
        ENDIF.

        SORT: lt_orc_aux      BY period ASCENDING produtos DESCENDING,
              lt_orc_collect  BY period,
              lt_prod_periodo BY period ASCENDING.

        DELETE ADJACENT DUPLICATES FROM lt_prod_periodo COMPARING period.

        LOOP AT lt_prod_periodo INTO ls_prod_periodo.
          READ TABLE lt_orc_collect INTO ls_orc_collect
            WITH KEY period = ls_prod_periodo-period BINARY SEARCH.
          IF sy-subrc EQ 0
          AND ls_orc_collect-produtos NE ls_prod_periodo-produtos.
            READ TABLE lt_orc_aux ASSIGNING FIELD-SYMBOL(<ls_orc_aux>)
              WITH KEY period = ls_prod_periodo-period BINARY SEARCH.
            IF sy-subrc EQ 0
            AND ls_orc_collect-produtos IS NOT INITIAL
            AND ls_prod_periodo-produtos IS NOT INITIAL
            AND ls_orc_collect-produtos NE ls_prod_periodo-produtos.
              lv_prod_dif = abs( ls_orc_collect-produtos - ls_prod_periodo-produtos ).

              IF ls_orc_collect-produtos LT ls_prod_periodo-produtos.
                DATA(lv_add) = abap_true.
              ELSE.
                lv_add = abap_false.
              ENDIF.

              IF lv_prod_dif BETWEEN lv_tol_min AND lv_tol_max.
                IF lv_add EQ abap_true.
                  ADD lv_prod_dif TO <ls_orc_aux>-produtos.
                ELSE.
                  SUBTRACT lv_prod_dif FROM <ls_orc_aux>-produtos.
                ENDIF.

                READ TABLE lt_orcamento ASSIGNING FIELD-SYMBOL(<ls_orcamento>)
                  WITH KEY acnum   = <ls_orc_aux>-acnum
                           extwg   = <ls_orc_aux>-extwg
                           matkl   = <ls_orc_aux>-matkl
                           rcnum   = <ls_orc_aux>-rcnum
                           matnr   = <ls_orc_aux>-matnr
                           period  = <ls_orc_aux>-period
                           fazenda = <ls_orc_aux>-fazenda
                           versao  = <ls_orc_aux>-versao.
                IF sy-subrc EQ 0.
                  <ls_orcamento>-produtos = <ls_orc_aux>-produtos.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.

        DATA(lv_uzeit) = sy-uzeit.

*-- Atualiza Log de Modificações [ZABS_ORCAMENTO -> ZABS_YORCAMENTO]
*-- Selecionar todos os talhões. Podem existir talhões inválidos.
        IF lt_orcamento[] IS NOT INITIAL.
          SELECT *
            FROM zabs_orcamento
            INTO TABLE @DATA(lt_old_entries)
            FOR ALL ENTRIES IN @lt_orcamento
           WHERE acnum  = @lt_orcamento-acnum
             AND extwg  = @lt_orcamento-extwg
             AND matkl  = @lt_orcamento-matkl
             AND rcnum  = @lt_orcamento-rcnum
             AND matnr  = @lt_orcamento-matnr
             AND period = @lt_orcamento-period
             AND versao = @lt_orcamento-versao.

          IF sy-subrc EQ 0.
            LOOP AT lt_old_entries INTO DATA(ls_old_entry).
              INSERT INITIAL LINE INTO TABLE lt_log
                ASSIGNING FIELD-SYMBOL(<ls_log>).
              IF sy-subrc EQ 0.
                MOVE-CORRESPONDING ls_old_entry TO <ls_log>.
                CLEAR <ls_log>-tcode.
                <ls_log>-acnum = ls_old_entry-acnum.
                <ls_log>-rcnum = ls_old_entry-rcnum.
                <ls_log>-matnr = ls_old_entry-matnr.
                <ls_log>-period = ls_old_entry-period.
                <ls_log>-fazenda = ls_old_entry-fazenda.
                <ls_log>-versao = er_entity-versao.
                <ls_log>-extwg = ls_old_entry-extwg.
                <ls_log>-matkl = ls_old_entry-matkl.
                <ls_log>-kostl = ls_old_entry-kostl.
                <ls_log>-aarea_form = ls_old_entry-aarea_form.
                <ls_log>-aarea_manu = ls_old_entry-aarea_manu.
                <ls_log>-passadas = ls_old_entry-passadas.
                <ls_log>-rcdos = ls_old_entry-rcdos.
                <ls_log>-custo = ls_old_entry-custo.
                <ls_log>-produtos = ls_old_entry-produtos.
                <ls_log>-versao_origem = er_entity-versao.
                <ls_log>-usuario = sy-uname.
              ENDIF.
            ENDLOOP.

*-- Atualiza Log de Modificações [ZABS_ORCAMENTO -> ZABS_YORCAMENTO]
            READ TABLE lt_log INTO DATA(ls_log) INDEX 1.
            IF sy-subrc EQ 0.
              ls_log-data_log = sy-datum.
              ls_log-hora = lv_uzeit.
              ls_log-tcode = 'AGRIPLAN'.
              MODIFY lt_log FROM ls_log
                TRANSPORTING data_log hora tcode WHERE tcode IS INITIAL.
              MODIFY zabs_yorcamento FROM TABLE lt_log[].
            ENDIF.
*-- Elimina Registros Antigos da Tabela ZABS_ORCAMENTO
            DELETE zabs_orcamento FROM TABLE lt_old_entries.
          ENDIF.
        ENDIF.

        READ TABLE lt_orcamento INTO ls_orcamento INDEX 1.
        IF sy-subrc EQ 0.
          ls_orcamento-usuario = sy-uname.
          ls_orcamento-data  = sy-datum.
          ls_orcamento-hora  = lv_uzeit.
          ls_orcamento-tcode = 'AGRIPLAN'.
          ls_orcamento-versao = er_entity-versao.
          MODIFY lt_orcamento FROM ls_orcamento
            TRANSPORTING usuario data hora tcode versao WHERE tcode IS INITIAL.
*-- Insere Registros Atualizados na Tabela ZABS_ORCAMENTO
          MODIFY zabs_orcamento FROM TABLE lt_orcamento.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMETHOD.


METHOD getprodutosset_get_entityset.

  TYPES: BEGIN OF ty_period,
           period TYPE period,
         END OF ty_period.

*-- Local Declarations
  DATA : lt_filter     TYPE /iwbep/t_mgw_select_option,
         ls_filter     TYPE /iwbep/s_mgw_select_option,
         lt_orc_full   TYPE STANDARD TABLE OF zabs_orcamento INITIAL SIZE 0,
         ls_orc_full   LIKE LINE OF lt_orc_full,
         lv_filter_str TYPE string,
         lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
         lt_period     TYPE STANDARD TABLE OF ty_period INITIAL SIZE 0,
         lrt_matnr     TYPE RANGE OF zfmacmatnr,
         lv_matnr_x    TYPE zfmacmatnr,
         lv_rcnum      TYPE zfmrcnum,
         lv_matnr      TYPE matnr,
         lrt_period    TYPE RANGE OF tvarvc-low,
         lrs_period    LIKE LINE OF lrt_period,
         ls_period     TYPE ty_period,
         lv_period     TYPE char6,
         lv_tipo       TYPE char10,
         lv_begda      TYPE begda,
         lv_endda      TYPE begda,
         lv_month      TYPE c LENGTH 2,
         lv_tabix_c    TYPE char2,
         lv_recipe     TYPE fieldname,
         lv_descr      TYPE string,
         lv_char       TYPE c LENGTH 15,
         lv_date       TYPE c LENGTH 10,
         lt_months     TYPE TABLE OF t247,
         lt_dates      TYPE /scwm/tt_lm_dates,
         lrt_matkl     TYPE RANGE OF tvarvc-low,
         lrs_matkl     LIKE LINE OF lrt_matkl,
         lrt_werks     TYPE RANGE OF werks_d,
         lrs_werks     LIKE LINE OF lrt_werks,
         ls_entityset  LIKE LINE OF et_entityset,
         lv_versao     TYPE zabs_del_ver_orc,
         lv_werks      TYPE werks_d,
         lv_acnum      TYPE zfmacnum,
         lv_rcnum1     TYPE zfmrcnum,
         lv_extwg      TYPE extwg,
         lv_matkl      TYPE matkl.

*-- Constants
  CONSTANTS: BEGIN OF lc_tipo,
               formacao   LIKE lv_tipo VALUE 'FORMAÇÃO',
               manutencao LIKE lv_tipo VALUE 'MANUTENÇÃO',
             END OF lc_tipo.

  lo_filter = io_tech_request_context->get_filter( ).
  lt_filter = lo_filter->get_filter_select_options( ).
  lv_filter_str = lo_filter->get_filter_string( ).

*--Maps filter table lines to function module parameters
  LOOP AT lt_filter INTO ls_filter.
    CASE ls_filter-property.
      WHEN 'MATNR'.
        lv_matnr = ls_filter-select_options[ 1 ]-low.
      WHEN 'BEGDA'.
        lv_begda = ls_filter-select_options[ 1 ]-low.
      WHEN 'ENDDA'.
        lv_endda = ls_filter-select_options[ 1 ]-low.
      WHEN 'WERKS'.
        lv_werks = ls_filter-select_options[ 1 ]-low.
      WHEN 'ACNUM'.
        lv_acnum = ls_filter-select_options[ 1 ]-low.
      WHEN 'EXTWG'.
        lv_extwg = ls_filter-select_options[ 1 ]-low.
      WHEN 'MATKL'.
        lv_matkl = ls_filter-select_options[ 1 ]-low.
      WHEN 'VERSAO'.
        lv_versao = ls_filter-select_options[ 1 ]-low.
    ENDCASE.
  ENDLOOP.

  IF lv_acnum IS NOT INITIAL.
    SELECT iwerk AS low
      FROM zfmaitm
      INTO CORRESPONDING FIELDS OF TABLE @lrt_werks
     WHERE acnum = @lv_acnum.

    lrs_werks-sign = 'I'.
    lrs_werks-option = 'EQ'.
    MODIFY lrt_werks FROM lrs_werks TRANSPORTING sign option WHERE low <> ''.

    lrs_werks = 'IEQ'.
    lrs_werks-low = lv_werks.
    APPEND lrs_werks TO lrt_werks.

    SORT lrt_werks BY low.
    DELETE ADJACENT DUPLICATES FROM lrt_werks COMPARING ALL FIELDS.
  ENDIF.

  IF lv_begda IS NOT INITIAL.
    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                      = lv_begda
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS                    = 2.

    IF sy-subrc <> 0.
      CLEAR lv_begda.
    ENDIF.
  ENDIF.

  IF lv_endda IS NOT INITIAL.
    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                      = lv_endda
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS                    = 2.

    IF sy-subrc <> 0.
      CLEAR lv_endda.
    ENDIF.
  ENDIF.

  IF lv_begda IS INITIAL
  OR lv_endda IS INITIAL.
    DATA(lv_error) = abap_true.
  ELSE.
    lv_error = abap_false.
  ENDIF.

  IF lv_error EQ abap_false.
    CALL FUNCTION '/SCWM/DATES_BETWEEN_TWO_DATES'
      EXPORTING
        iv_begda = lv_begda
        iv_endda = lv_endda
      IMPORTING
        et_dates = lt_dates.

    lrs_period = 'IEQ'.
    LOOP AT lt_dates INTO DATA(ls_dates).
      IF lv_period NE ls_dates(6).
        lrs_period-low =  ls_period-period = lv_period = ls_dates(6).
        APPEND: ls_period  TO lt_period,
                lrs_period TO lrt_period.
      ENDIF.
    ENDLOOP.

    lrs_matkl = 'ICP'.
    lrs_matkl-low = lv_matkl && '*'.
    APPEND lrs_matkl TO lrt_matkl.

    SELECT rctyp, orcamento
      INTO TABLE @DATA(lt_rctyp)
      FROM ztfmrctyp
     WHERE orcamento EQ @abap_true.

    IF sy-subrc EQ 0.
      lv_matnr_x = 'TMAN' && lv_matnr+4(36).
      INSERT INITIAL LINE INTO TABLE lrt_matnr
        ASSIGNING FIELD-SYMBOL(<lrs_matnr>).
      IF sy-subrc EQ 0.
        <lrs_matnr> = 'IEQ'.
        <lrs_matnr>-low = lv_matnr_x.
      ENDIF.

      lv_matnr_x = 'TFOR' && lv_matnr+4(36).
      INSERT INITIAL LINE INTO TABLE lrt_matnr ASSIGNING <lrs_matnr>.
      IF sy-subrc EQ 0.
        <lrs_matnr> = 'IEQ'.
        <lrs_matnr>-low = lv_matnr_x.
      ENDIF.

      lv_matnr_x = 'TIMP' && lv_matnr+4(36).
      INSERT INITIAL LINE INTO TABLE lrt_matnr ASSIGNING <lrs_matnr>.
      IF sy-subrc EQ 0.
        <lrs_matnr> = 'IEQ'.
        <lrs_matnr>-low = lv_matnr_x.
      ENDIF.

      SELECT *
        INTO TABLE @DATA(lt_rchdr)
        FROM zfmrchdr
        FOR ALL ENTRIES IN @lt_rctyp
*      WHERE werks EQ @lv_werks
       WHERE werks IN @lrt_werks[]
         AND matnr IN @lrt_matnr[]
         AND datuv GE @lv_begda
         AND rctyp EQ @lt_rctyp-rctyp
         AND datbi LE @lv_endda.

      IF sy-subrc EQ 0.
        SELECT matnr, matkl
          FROM mara
          INTO TABLE @DATA(lt_mara)
          FOR ALL ENTRIES IN @lt_rchdr
         WHERE matnr = @lt_rchdr-matnr
           AND matkl = @lv_matkl.

        SORT lt_mara BY matnr.

        DELETE lt_rchdr WHERE matnr(4) NE 'TFOR'
                          AND matnr(4) NE 'TMAN'
                          AND matnr(4) NE 'TIMP'.

        IF lt_rchdr[] IS NOT INITIAL.
          SORT lt_rchdr BY matnr rcnum.

          LOOP AT lt_rchdr ASSIGNING FIELD-SYMBOL(<ls_rchdr>).
            IF <ls_rchdr>-matnr(4) EQ 'TFOR'
            OR <ls_rchdr>-matnr(4) EQ 'TIMP'.
              <ls_rchdr>-text1 = lc_tipo-formacao.
            ELSEIF <ls_rchdr>-matnr(4) EQ 'TMAN'.
              <ls_rchdr>-text1 = lc_tipo-manutencao.
            ENDIF.
          ENDLOOP.

          SELECT *
            INTO TABLE @DATA(lt_orcamento)
            FROM zabs_orcamento
            FOR ALL ENTRIES IN @lt_rchdr
           WHERE acnum  EQ @lv_acnum
             AND extwg  EQ @lv_extwg
             AND matkl  EQ @lv_matkl
             AND rcnum  EQ @lt_rchdr-rcnum
             AND period IN @lrt_period[]
             AND versao EQ @lv_versao.

          LOOP AT lt_orcamento INTO DATA(ls_orcamento).
            ls_orc_full = ls_orcamento.
            CLEAR: ls_orc_full-fazenda,
*-- BOC-T_T.KONNO-04.13.21
                   ls_orc_full-usuario,
                   ls_orc_full-data,
                   ls_orc_full-hora,
                   ls_orc_full-tcode,
*-- EOC-T_T.KONNO-04.13.21
                   ls_orc_full-kostl.
            COLLECT ls_orc_full INTO lt_orc_full.
          ENDLOOP.

          SORT: lt_orc_full BY rcnum period matnr,
                lt_orcamento BY rcnum period matnr.

          DELETE: lt_orcamento WHERE fazenda  = space
                                  OR produtos = space.

          SELECT *
            INTO TABLE @DATA(lt_zfmrclst)
            FROM zfmrclst
            FOR ALL ENTRIES IN @lt_rchdr
           WHERE rcnum EQ @lt_rchdr-rcnum
             AND werks EQ @lt_rchdr-werks
             AND matnr EQ @lt_rchdr-matnr.

          IF sy-subrc EQ 0.
            SORT lt_zfmrclst BY rcnum werks matnr.

            CLEAR lv_rcnum1.
            DATA(lt_rchdrx) = lt_rchdr[].
            LOOP AT lt_rchdr INTO DATA(ls_rchdr).
              READ TABLE lt_zfmrclst TRANSPORTING NO FIELDS
                WITH KEY rcnum = ls_rchdr-rcnum
                         werks = ls_rchdr-werks
                         matnr = ls_rchdr-matnr BINARY SEARCH.

              IF sy-subrc EQ 0.
                LOOP AT lt_zfmrclst INTO DATA(ls_zfmrclst) FROM sy-tabix.
                  IF ls_zfmrclst-rcnum NE ls_rchdr-rcnum
                  OR ls_zfmrclst-werks NE ls_rchdr-werks
                  OR ls_zfmrclst-matnr NE ls_rchdr-matnr.
                    EXIT.
                  ENDIF.

                  CASE ls_zfmrclst-matnr(4).
*--BOC-T_T.KONNO-05.28.21
*                    WHEN 'TFOR'.
                    WHEN 'TFOR'
                      OR 'TIMP'.
*--EOC-T_T.KONNO-05.28.21
                      ls_entityset-tipo = lc_tipo-formacao.
                    WHEN 'TMAN'.
                      ls_entityset-tipo = lc_tipo-manutencao.
                  ENDCASE.

                  ls_entityset-rcnum1 = ls_rchdr-rcnum.

                  LOOP AT lt_period INTO ls_period.
                    DATA(lv_tabix) = sy-tabix.

                    READ TABLE lt_orcamento INTO ls_orcamento
                      WITH KEY rcnum  = ls_rchdr-rcnum
                               period = ls_period
                               matnr  = ls_zfmrclst-matnr_ins BINARY SEARCH.
                    IF sy-subrc NE 0.
                      CLEAR ls_orcamento.
                    ENDIF.

                    READ TABLE lt_orc_full INTO ls_orc_full
                      WITH KEY rcnum  = ls_rchdr-rcnum
                               period = ls_period
                               matnr  = ls_zfmrclst-matnr_ins BINARY SEARCH.
                    IF sy-subrc NE 0.
                      CLEAR ls_orc_full.
                    ENDIF.

                    CASE lv_tabix.
                      WHEN '1'.
                        IF ls_orcamento-rcdos GT 0.
                          ls_entityset-m1d = ls_orcamento-rcdos.
                        ELSE.
                          ls_entityset-m1d = ls_zfmrclst-rcdos.
                        ENDIF.
                        ls_entityset-m1p = ls_orcamento-passadas.
                        ls_entityset-p1 = ls_orc_full-produtos.
                      WHEN '2'.
                        IF ls_orcamento-rcdos GT 0.
                          ls_entityset-m2d = ls_orcamento-rcdos.
                        ELSE.
                          ls_entityset-m2d = ls_zfmrclst-rcdos.
                        ENDIF.
                        ls_entityset-m2p = ls_orcamento-passadas.
                        ls_entityset-p2 = ls_orc_full-produtos.
                      WHEN '3'.
                        IF ls_orcamento-rcdos GT 0.
                          ls_entityset-m3d = ls_orcamento-rcdos.
                        ELSE.
                          ls_entityset-m3d = ls_zfmrclst-rcdos.
                        ENDIF.
                        ls_entityset-m3p = ls_orcamento-passadas.
                        ls_entityset-p3 = ls_orc_full-produtos.
                      WHEN '4'.
                        IF ls_orcamento-rcdos GT 0.
                          ls_entityset-m4d = ls_orcamento-rcdos.
                        ELSE.
                          ls_entityset-m4d = ls_zfmrclst-rcdos.
                        ENDIF.
                        ls_entityset-m4p = ls_orcamento-passadas.
                        ls_entityset-p4 = ls_orc_full-produtos.
                      WHEN '5'.
                        IF ls_orcamento-rcdos GT 0.
                          ls_entityset-m5d = ls_orcamento-rcdos.
                        ELSE.
                          ls_entityset-m5d = ls_zfmrclst-rcdos.
                        ENDIF.
                        ls_entityset-m5p = ls_orcamento-passadas.
                        ls_entityset-p5 = ls_orc_full-produtos.
                      WHEN '6'.
                        IF ls_orcamento-rcdos GT 0.
                          ls_entityset-m6d = ls_orcamento-rcdos.
                        ELSE.
                          ls_entityset-m6d = ls_zfmrclst-rcdos.
                        ENDIF.
                        ls_entityset-m6p = ls_orcamento-passadas.
                        ls_entityset-p6 = ls_orc_full-produtos.
                      WHEN '7'.
                        IF ls_orcamento-rcdos GT 0.
                          ls_entityset-m7d = ls_orcamento-rcdos.
                        ELSE.
                          ls_entityset-m7d = ls_zfmrclst-rcdos.
                        ENDIF.
                        ls_entityset-m7p = ls_orcamento-passadas.
                        ls_entityset-p7 = ls_orc_full-produtos.
                      WHEN '8'.
                        IF ls_orcamento-rcdos GT 0.
                          ls_entityset-m8d = ls_orcamento-rcdos.
                        ELSE.
                          ls_entityset-m8d = ls_zfmrclst-rcdos.
                        ENDIF.
                        ls_entityset-m8p = ls_orcamento-passadas.
                        ls_entityset-p8 = ls_orc_full-produtos.
                      WHEN '9'.
                        IF ls_orcamento-rcdos GT 0.
                          ls_entityset-m9d = ls_orcamento-rcdos.
                        ELSE.
                          ls_entityset-m9d = ls_zfmrclst-rcdos.
                        ENDIF.
                        ls_entityset-m9p = ls_orcamento-passadas.
                        ls_entityset-p9 = ls_orc_full-produtos.
                      WHEN '10'.
                        IF ls_orcamento-rcdos GT 0.
                          ls_entityset-m10d = ls_orcamento-rcdos.
                        ELSE.
                          ls_entityset-m10d = ls_zfmrclst-rcdos.
                        ENDIF.
                        ls_entityset-m10p = ls_orcamento-passadas.
                        ls_entityset-p10 = ls_orc_full-produtos.
                      WHEN '11'.
                        IF ls_orcamento-rcdos GT 0.
                          ls_entityset-m11d = ls_orcamento-rcdos.
                        ELSE.
                          ls_entityset-m11d = ls_zfmrclst-rcdos.
                        ENDIF.
                        ls_entityset-m11p = ls_orcamento-passadas.
                        ls_entityset-p11 = ls_orc_full-produtos.
                      WHEN '12'.
                        IF ls_orcamento-rcdos GT 0.
                          ls_entityset-m12d = ls_orcamento-rcdos.
                        ELSE.
                          ls_entityset-m12d = ls_zfmrclst-rcdos.
                        ENDIF.
                        ls_entityset-m12p = ls_orcamento-passadas.
                        ls_entityset-p12 = ls_orc_full-produtos.
                    ENDCASE.
                  ENDLOOP.

                  ls_entityset-maktx     = ls_zfmrclst-maktx.
                  ls_entityset-matnrins  = ls_zfmrclst-matnr_ins.
                  ls_entityset-werks     = ls_rchdr-werks.
                  ls_entityset-matnr     = ls_zfmrclst-matnr.
                  ls_entityset-principal = ls_zfmrclst-rcinp_check.
                  APPEND ls_entityset TO et_entityset.
                  CLEAR ls_entityset.
                ENDLOOP.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMETHOD.


  METHOD getreceitalist01_get_entityset.

*-- Local Types
    TYPES: BEGIN OF ly_mard,
             matnr TYPE matnr,
             werks TYPE werks_d,
             lgort TYPE lgort_d,
             lvorm TYPE lvolg,
             labst TYPE labst,
           END OF ly_mard.

*-- Local Declarations
    DATA: lt_filter        TYPE /iwbep/t_mgw_select_option,
          lt_mard          TYPE STANDARD TABLE OF ly_mard INITIAL SIZE 0,
          lt_mard_collect  LIKE lt_mard,
          lrt_tplnr_fl     TYPE RANGE OF /agri/gltplnr_fl,
          lrt_matkl        TYPE RANGE OF matkl,
          ls_filter        TYPE /iwbep/s_mgw_select_option,
          ls_mard_collect  LIKE LINE OF lt_mard_collect,
          ls_entityset     LIKE LINE OF et_entityset,
          ls_list          TYPE bapi_mrp_list,
          ls_control_param TYPE bapi_mrp_control_param,
          ls_stock_detail  TYPE bapi_mrp_stock_detail,
          ls_return        TYPE bapiret2,
          lo_filter        TYPE REF TO /iwbep/if_mgw_req_filter,
          lv_filter_str    TYPE string,
          lv_tplnr_fl      TYPE /agri/gltplnr_fl,
          lv_rcnum         TYPE zfmrcnum,
          lv_matnr         TYPE matnr,
          lv_werks         TYPE werks_d,
          lv_acnum         TYPE zfmacnum,
          lv_extwg         TYPE extwg,
          lv_matkl         TYPE matkl,
          lv_data          TYPE begda,
          lv_lgort         TYPE lgort_d,
          lv_material_ins  TYPE matnr,
          lv_material_sim  TYPE matnr,
          lv_plant         TYPE werks.

    lo_filter = io_tech_request_context->get_filter( ).
    lt_filter = lo_filter->get_filter_select_options( ).
    lv_filter_str = lo_filter->get_filter_string( ).

*--Maps filter table lines to function module parameters
    LOOP AT lt_filter INTO ls_filter.
      CASE ls_filter-property.
        WHEN 'RCNUM'.
          lv_rcnum = ls_filter-select_options[ 1 ]-low.
        WHEN 'MATNR'.
          lv_matnr = ls_filter-select_options[ 1 ]-low.
        WHEN 'DATA'.
          lv_data = ls_filter-select_options[ 1 ]-low.
        WHEN 'WERKS'.
          lv_werks = ls_filter-select_options[ 1 ]-low.
        WHEN 'ACNUM'.
          lv_acnum = ls_filter-select_options[ 1 ]-low.
        WHEN 'EXTWG'.
          lv_extwg = ls_filter-select_options[ 1 ]-low.
        WHEN 'MATKL'.
          lv_matkl = ls_filter-select_options[ 1 ]-low.
        WHEN 'LGORT'.
          lv_lgort = ls_filter-select_options[ 1 ]-low.
        WHEN 'TPLNR_FL'.
          lo_filter->convert_select_option(
            EXPORTING
              is_select_option = ls_filter
            IMPORTING
              et_select_option = lrt_tplnr_fl ).
      ENDCASE.
    ENDLOOP.

    IF lv_rcnum IS NOT INITIAL.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE @et_entityset
        FROM zfmrcsim
       WHERE rcnum EQ @lv_rcnum.

      LOOP AT et_entityset ASSIGNING FIELD-SYMBOL(<entityset>).
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            input        = <entityset>-matnr_sim
          IMPORTING
            output       = <entityset>-matnr_sim
          EXCEPTIONS
            length_error = 1
            OTHERS       = 2.
      ENDLOOP.

      SELECT *
        INTO TABLE @DATA(lt_recipes_changed)
        FROM zfmrchis
       WHERE werks    EQ @lv_werks
         AND acnum    EQ @lv_acnum
         AND matkl    IN @lrt_matkl[]
         AND tplnr_fl IN @lrt_tplnr_fl[]
         AND data     EQ @lv_data
         AND rcnum    EQ @lv_rcnum.

      SORT: et_entityset BY matnr_ins,
            lt_recipes_changed BY werks matnr_ins tplnr_fl.

      DATA(lt_entityset) = et_entityset[].
      DELETE: lt_entityset WHERE matnr_sim IS INITIAL,
              lt_recipes_changed WHERE matnr_sim IS INITIAL
                                    OR loevm EQ abap_true.

      IF lt_entityset[] IS NOT INITIAL.
        SELECT matnr, werks, lgort, lvorm, labst
          FROM mard
          INTO TABLE @lt_mard
          FOR ALL ENTRIES IN @lt_entityset
         WHERE matnr EQ @lt_entityset-matnr_sim
           AND werks EQ @lt_entityset-werks
           AND lvorm EQ @abap_false.

        SORT lt_mard BY matnr werks lgort.

        LOOP AT lt_mard INTO DATA(ls_mard).
          ls_mard_collect-matnr = ls_mard-matnr.
          ls_mard_collect-werks = ls_mard-werks.
          ls_mard_collect-labst = ls_mard-labst.
          COLLECT ls_mard_collect INTO lt_mard_collect.
        ENDLOOP.

        SORT lt_mard_collect BY matnr werks.
      ENDIF.

      LOOP AT lrt_tplnr_fl ASSIGNING FIELD-SYMBOL(<lrs_tplnr_fl>).
        IF sy-tabix EQ 1.
          DATA(et_entityset_copy) = et_entityset[].
          READ TABLE et_entityset INTO ls_entityset INDEX 1.
          IF sy-subrc EQ 0.
            ls_entityset-tplnr_fl = <lrs_tplnr_fl>-low.
            MODIFY et_entityset FROM ls_entityset TRANSPORTING tplnr_fl WHERE tplnr_fl = ' '.
          ENDIF.
        ELSE.
          APPEND LINES OF et_entityset_copy TO et_entityset.
          ls_entityset-tplnr_fl = <lrs_tplnr_fl>-low.
          MODIFY et_entityset FROM ls_entityset TRANSPORTING tplnr_fl WHERE tplnr_fl = ' '.
        ENDIF.
      ENDLOOP.

      LOOP AT et_entityset ASSIGNING FIELD-SYMBOL(<ls_entityset>).
        lv_material_ins = <ls_entityset>-matnr_ins.
        lv_material_sim = <ls_entityset>-matnr_sim.
        lv_plant = <ls_entityset>-werks.

        READ TABLE lt_recipes_changed INTO DATA(lwa_recipe_changed)
          WITH KEY werks     = lv_plant
                   matnr_ins = lv_material_ins
                   tplnr_fl  = <ls_entityset>-tplnr_fl BINARY SEARCH.
        IF sy-subrc NE 0.
          CLEAR lwa_recipe_changed.
        ELSE.
          <ls_entityset>-rcdos_sim = lwa_recipe_changed-rcdos_sim.
          IF lwa_recipe_changed-lgort_sim IS NOT INITIAL.
            lv_lgort = <ls_entityset>-lgort = lwa_recipe_changed-lgort_sim.
          ENDIF.
        ENDIF.

        IF lv_lgort IS INITIAL.
          READ TABLE lt_mard_collect INTO ls_mard_collect
            WITH KEY matnr = lv_material_sim
                     werks = lv_plant BINARY SEARCH.
          IF sy-subrc EQ 0.
            <ls_entityset>-plnt_stock = ls_mard_collect-labst.
          ENDIF.
        ELSE.
          READ TABLE lt_mard INTO ls_mard
            WITH KEY matnr = lv_material_sim
                     werks = lv_plant BINARY SEARCH.
          IF sy-subrc EQ 0.
            <ls_entityset>-plnt_stock = ls_mard-labst.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


METHOD getreceitalistas_get_entityset.

*-- Local Types
  TYPES: BEGIN OF ly_mard,
           matnr TYPE matnr,
           werks TYPE werks_d,
           lgort TYPE lgort_d,
           lvorm TYPE lvolg,
           labst TYPE labst,
         END OF ly_mard.

*-- Local Declarations
  DATA: lt_filter        TYPE /iwbep/t_mgw_select_option,
        lt_tplnr         TYPE /agri/t_gltplnr,
        lt_mard          TYPE STANDARD TABLE OF ly_mard INITIAL SIZE 0,
        lt_mard_sim      LIKE lt_mard,
        lt_mard_collect  LIKE lt_mard,
        lt_flhdr         TYPE /agri/t_glflot,
        lt_iflotx        TYPE /agri/t_gliflotx,
        lt_adrc          TYPE /agri/t_gladrc,
        lt_ihpa          TYPE /agri/t_glihpa,
        lt_flppl         TYPE /agri/t_glflppl,
        lt_flatg         TYPE /agri/t_glflatg,
        lt_flatv         TYPE /agri/t_glflatv,
        lt_flown         TYPE /agri/t_glflown,
        lt_flcma         TYPE /agri/t_glflcma,
        lt_flos          TYPE /agri/t_glflos,
        lrt_tplnr_in     TYPE RANGE OF /agri/gltplnr_fl,
        lrt_matkl        TYPE RANGE OF matkl,
        lrt_tplnr_fl     TYPE RANGE OF /agri/gltplnr_fl,
        ls_mard_collect  LIKE LINE OF lt_mard_collect,
        ls_filter        TYPE /iwbep/s_mgw_select_option,
        ls_entityset     LIKE LINE OF et_entityset,
        lo_filter        TYPE REF TO /iwbep/if_mgw_req_filter,
        lv_filter_str    TYPE string,
        lv_rcnum         TYPE zfmrcnum,
        lv_atinn_lgort   TYPE atinn,
        lv_matnr         TYPE matnr,
        lv_tplnr_fl      TYPE /agri/gltplnr_fl,
        lv_lgort         TYPE lgort_d,
        lv_werks         TYPE werks_d,
        lv_acnum         TYPE zfmacnum,
        lv_extwg         TYPE extwg,
        lv_matkl         TYPE matkl,
        lv_terrain_in    TYPE /agri/gltplnr_fl,
        lv_data          TYPE begda,
        lv_terrain_lgort TYPE lgort_d,
        lv_material      TYPE matnr,
        lv_plant         TYPE werks.

  lo_filter = io_tech_request_context->get_filter( ).
  lt_filter = lo_filter->get_filter_select_options( ).
  lv_filter_str = lo_filter->get_filter_string( ).

*--Maps filter table lines to function module parameters
  LOOP AT lt_filter INTO ls_filter.
    CASE ls_filter-property.
      WHEN 'RCNUM'.
        lv_rcnum = ls_filter-select_options[ 1 ]-low.
      WHEN 'MATNR'.
        lv_matnr = ls_filter-select_options[ 1 ]-low.
      WHEN 'DATA'.
        lv_data = ls_filter-select_options[ 1 ]-low.
      WHEN 'WERKS'.
        lv_werks = ls_filter-select_options[ 1 ]-low.
      WHEN 'ACNUM'.
        lv_acnum = ls_filter-select_options[ 1 ]-low.
      WHEN 'EXTWG'.
        lv_extwg = ls_filter-select_options[ 1 ]-low.
      WHEN 'MATKL'.
        lv_matkl = ls_filter-select_options[ 1 ]-low.
      WHEN 'LGORT'.
        lv_lgort = ls_filter-select_options[ 1 ]-low.
      WHEN 'TPLNR_FL'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_tplnr_fl ).
    ENDCASE.
  ENDLOOP.

  IF lv_rcnum IS NOT INITIAL.
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE @et_entityset
      FROM zfmrclst
     WHERE rcnum EQ @lv_rcnum.

    IF et_entityset[] IS NOT INITIAL.
      LOOP AT lrt_tplnr_fl INTO DATA(lrs_tplnr_fl).
        INSERT INITIAL LINE INTO TABLE lrt_tplnr_in
          ASSIGNING FIELD-SYMBOL(<lrs_tplnr_in>).
        IF sy-subrc EQ 0.
          <lrs_tplnr_in> = 'IEQ'.
          <lrs_tplnr_in>-low = lrs_tplnr_fl-low.

          CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
            EXPORTING
              input      = <lrs_tplnr_in>-low
            IMPORTING
              output     = <lrs_tplnr_in>-low
            EXCEPTIONS
              not_found  = 1
              not_active = 2
              OTHERS     = 3.
        ENDIF.
      ENDLOOP.

      IF lrt_tplnr_in[] IS NOT INITIAL.
*-- Read terrain data from Terrain Header table
        SELECT tplnr_fl
          FROM /agri/glflot
          INTO TABLE lt_tplnr
         WHERE tplnr_fl IN lrt_tplnr_in[].

        IF sy-subrc EQ 0.
*-- Getting all the terrain records
          CALL FUNCTION '/AGRI/GLFL_READ'
            EXPORTING
              it_tplnr       = lt_tplnr
            IMPORTING
*             et_flhdr       = lt_flhdr
*             et_iflotx      = lt_iflotx
*             et_adrc        = lt_adrc
*             et_ihpa        = lt_ihpa
*             et_flppl       = lt_flppl
              et_flatg       = lt_flatg
              et_flatv       = lt_flatv
*             et_flown       = lt_flown
*             et_flcma       = lt_flcma
*             et_flos        = lt_flos
            EXCEPTIONS
              no_data_exists = 1
              OTHERS         = 2.
        ENDIF.

        CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
          EXPORTING
            input  = 'DEP_CONSUMO'
          IMPORTING
            output = lv_atinn_lgort.
      ENDIF.

      DATA(lt_reference) = et_entityset[].
      SORT lt_reference BY posnr DESCENDING.

      SELECT *
        INTO TABLE @DATA(lt_similares)
        FROM zfmrcsim
       WHERE rcnum EQ @lv_rcnum.

      LOOP AT lt_similares ASSIGNING FIELD-SYMBOL(<ls_similar>).
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            input        = <ls_similar>-matnr_sim
          IMPORTING
            output       = <ls_similar>-matnr_sim
          EXCEPTIONS
            length_error = 1
            OTHERS       = 2.
      ENDLOOP.

      SORT lt_similares BY matnr_sim werks.

      IF lt_similares[] IS NOT INITIAL.
        SELECT matnr, werks, lgort, lvorm, labst
          FROM mard
          INTO TABLE @lt_mard_sim
          FOR ALL ENTRIES IN @lt_similares
         WHERE matnr EQ @lt_similares-matnr_sim
           AND werks EQ @lt_similares-werks
           AND lvorm EQ @abap_false.

        SORT lt_mard_sim BY matnr werks lgort.

        LOOP AT lt_mard_sim INTO DATA(ls_mard_sim).
          ls_mard_collect-matnr = ls_mard_sim-matnr.
          ls_mard_collect-werks = ls_mard_sim-werks.
          ls_mard_collect-labst = ls_mard_sim-labst.
          COLLECT ls_mard_collect INTO lt_mard_collect.
        ENDLOOP.

        SORT lt_mard_collect BY matnr werks.
      ENDIF.

      INSERT INITIAL LINE INTO TABLE lrt_matkl
        ASSIGNING FIELD-SYMBOL(<lrs_matkl>).
      IF sy-subrc EQ 0.
        <lrs_matkl> = 'IEQ'.
        <lrs_matkl>-low = lv_matkl.
      ENDIF.

      SELECT *
        INTO TABLE @DATA(lt_recipes_changed)
        FROM zfmrchis
       WHERE werks    EQ @lv_werks
         AND acnum    EQ @lv_acnum
         AND matkl    IN @lrt_matkl[]
         AND tplnr_fl IN @lrt_tplnr_fl[]
         AND data     EQ @lv_data
         AND rcnum    EQ @lv_rcnum.

      SORT: et_entityset BY matnr_ins,
            lt_recipes_changed BY werks matnr_ins tplnr_fl.
      DELETE ADJACENT DUPLICATES FROM et_entityset COMPARING matnr_ins.
      SORT et_entityset BY werks matnr_ins tplnr_fl.

      IF et_entityset[] IS NOT INITIAL.
        SELECT matnr, werks, lgort, lvorm, labst
          FROM mard
          INTO TABLE @lt_mard
          FOR ALL ENTRIES IN @et_entityset
         WHERE matnr EQ @et_entityset-matnr_ins
           AND werks EQ @et_entityset-werks
           AND lvorm EQ @abap_false.

        SORT lt_mard BY matnr werks lgort.

        LOOP AT lt_mard INTO DATA(ls_mard).
          ls_mard_collect-matnr = ls_mard-matnr.
          ls_mard_collect-werks = ls_mard-werks.
          ls_mard_collect-labst = ls_mard-labst.
          COLLECT ls_mard_collect INTO lt_mard_collect.
        ENDLOOP.

        SORT lt_mard_collect BY matnr werks.
      ENDIF.

      LOOP AT lrt_tplnr_fl ASSIGNING FIELD-SYMBOL(<lrs_tplnr_fl>).
        IF sy-tabix EQ 1.
          DATA(et_entityset_copy) = et_entityset[].
          READ TABLE et_entityset INTO ls_entityset INDEX 1.
          IF sy-subrc EQ 0.
            ls_entityset-tplnr_fl = <lrs_tplnr_fl>-low.
            MODIFY et_entityset FROM ls_entityset TRANSPORTING tplnr_fl WHERE tplnr_fl = ' '.
          ENDIF.
        ELSE.
          APPEND LINES OF et_entityset_copy TO et_entityset.
          ls_entityset-tplnr_fl = <lrs_tplnr_fl>-low.
          MODIFY et_entityset FROM ls_entityset TRANSPORTING tplnr_fl WHERE tplnr_fl = ' '.
        ENDIF.
      ENDLOOP.

      LOOP AT et_entityset ASSIGNING FIELD-SYMBOL(<ls_entityset>).
        IF <ls_entityset>-tplnr_fl IS INITIAL.
          CONTINUE.
        ENDIF.

        CLEAR lv_terrain_lgort.
        lv_tplnr_fl = <ls_entityset>-tplnr_fl.
        CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
          EXPORTING
            input      = lv_tplnr_fl
          IMPORTING
            output     = lv_tplnr_fl
          EXCEPTIONS
            not_found  = 1
            not_active = 2
            OTHERS     = 3.

        READ TABLE lt_flatv INTO DATA(ls_flatv)
          WITH KEY tplnr_fl = lv_tplnr_fl
                   atinn    = lv_atinn_lgort
                   class    = 'CIT-TALHAO'.
        IF sy-subrc EQ 0.
          IF ls_flatv-atwrt IS NOT INITIAL.
            lv_terrain_lgort = ls_flatv-atwrt(4).
          ENDIF.
        ENDIF.

        <ls_entityset>-lgort = lv_terrain_lgort.
      ENDLOOP.

      LOOP AT et_entityset ASSIGNING <ls_entityset>.
        DATA(lv_tabix) = sy-tabix.
        lv_material = <ls_entityset>-matnr_ins.
        lv_plant = <ls_entityset>-werks.

        READ TABLE lt_recipes_changed INTO DATA(lwa_recipe_changed)
          WITH KEY werks     = lv_plant
                   matnr_ins = lv_material
                   tplnr_fl  = <ls_entityset>-tplnr_fl BINARY SEARCH.
        IF sy-subrc NE 0.
          CLEAR lwa_recipe_changed.
        ELSE.
          IF lwa_recipe_changed-loevm EQ abap_true.
            DELETE et_entityset INDEX lv_tabix.
            CONTINUE.
          ELSE.
            <ls_entityset>-rcdos = lwa_recipe_changed-rcdos_ins.
            IF lwa_recipe_changed-lgort_ins IS NOT INITIAL.
              lv_lgort = <ls_entityset>-lgort = lwa_recipe_changed-lgort_ins.
            ENDIF.
          ENDIF.
        ENDIF.

        IF lv_lgort IS INITIAL.
          READ TABLE lt_mard_collect INTO ls_mard_collect
            WITH KEY matnr = lv_material
                     werks = lv_plant BINARY SEARCH.
          IF sy-subrc EQ 0.
            <ls_entityset>-plnt_stock = ls_mard_collect-labst.
          ENDIF.
        ELSE.
          READ TABLE lt_mard INTO ls_mard
            WITH KEY matnr = lv_material
                     werks = lv_plant BINARY SEARCH.
          IF sy-subrc EQ 0.
            <ls_entityset>-plnt_stock = ls_mard-labst.
          ENDIF.
        ENDIF.
      ENDLOOP.

*-- Verificação Adicional: move insumos incluídos pelo usuário na receita
      LOOP AT lt_recipes_changed INTO lwa_recipe_changed.
        IF lwa_recipe_changed-loevm EQ abap_true.
          CONTINUE.
        ENDIF.
        READ TABLE et_entityset TRANSPORTING NO FIELDS
          WITH KEY werks     = lwa_recipe_changed-werks
                   matnr_ins = lwa_recipe_changed-matnr_ins
                   tplnr_fl  = lwa_recipe_changed-tplnr_fl.
        IF sy-subrc NE 0.
          READ TABLE lt_reference INTO DATA(lwa_reference) INDEX 1.
          IF sy-subrc EQ 0.
            INSERT INITIAL LINE INTO TABLE et_entityset
              ASSIGNING <ls_entityset>.
            IF sy-subrc EQ 0.
              READ TABLE lt_mard_collect INTO ls_mard_collect
                WITH KEY matnr = lwa_recipe_changed-matnr_ins
                         werks = lwa_recipe_changed-werks BINARY SEARCH.
              IF sy-subrc EQ 0.
                <ls_entityset>-plnt_stock = ls_mard_collect-labst.
              ENDIF.

              READ TABLE lt_similares INTO DATA(ls_similar)
                WITH KEY matnr_sim = lwa_recipe_changed-matnr_ins
                         werks     = lwa_recipe_changed-werks BINARY SEARCH.
              IF sy-subrc EQ 0.
                <ls_entityset>-maktx = ls_similar-maktx_sim.
                <ls_entityset>-rcdos = ls_similar-rcdos_sim.
                <ls_entityset>-units = ls_similar-units_sim.
              ENDIF.

              MOVE-CORRESPONDING lwa_recipe_changed TO <ls_entityset>.
              <ls_entityset>-matnr = lwa_reference-matnr.
              <ls_entityset>-rcinp = icon_wd_radio_button_empty.
              <ls_entityset>-posnr = lwa_reference-posnr + 10.
              UNPACK <ls_entityset>-posnr TO <ls_entityset>-posnr.

              READ TABLE lt_recipes_changed INTO DATA(lwa_similar_changed)
                WITH KEY werks     = <ls_entityset>-werks
                         matnr_ins = <ls_entityset>-matnr_ins
                         tplnr_fl  = <ls_entityset>-tplnr_fl.
              IF sy-subrc EQ 0.
                <ls_entityset>-rcdos = lwa_similar_changed-rcdos_ins.
                IF lwa_similar_changed-lgort_ins IS NOT INITIAL.
                  <ls_entityset>-lgort = lwa_similar_changed-lgort_ins.
                ENDIF.
              ELSE.
                READ TABLE lt_recipes_changed INTO lwa_similar_changed
                  WITH KEY werks     = <ls_entityset>-werks
                           matnr_sim = <ls_entityset>-matnr_ins
                           tplnr_fl  = <ls_entityset>-tplnr_fl.
                IF sy-subrc EQ 0.
                  <ls_entityset>-rcdos = lwa_similar_changed-rcdos_sim.
                  IF lwa_similar_changed-lgort_sim IS NOT INITIAL.
                    <ls_entityset>-lgort = lwa_similar_changed-lgort_sim.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD getreceitasset_get_entityset.

*-- Local Declarations
  DATA: lt_filter     TYPE /iwbep/t_mgw_select_option,
        lt_acnum      TYPE zt_fm_acnum,
        lt_achdr      TYPE zt_fmachdr,
        lt_acitm      TYPE zt_fmacitm,
        lrt_tplnr_fl  TYPE RANGE OF /agri/gltplnr_fl,
        lrt_matnr     TYPE RANGE OF matnr,
        lrt_matkl     TYPE RANGE OF matkl,
        lrt_werks     TYPE RANGE OF werks_d,
        ls_filter     TYPE /iwbep/s_mgw_select_option,
        lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
        lv_filter_str TYPE string,
        lv_rcnum      TYPE zfmrcnum,
        lv_matnr      TYPE matnr,
        lv_tplnr_fl   TYPE /agri/gltplnr_fl,
        lv_werks      TYPE werks_d,
        lv_acnum      TYPE zfmacnum,
        lv_matkl      TYPE matkl.

  CONSTANTS: BEGIN OF c_msg_type,
               info    LIKE sy-msgty VALUE 'I',
               warning LIKE sy-msgty VALUE 'W',
               error   LIKE sy-msgty VALUE 'E',
               abend   LIKE sy-msgty VALUE 'A',
               success LIKE sy-msgty VALUE 'S',
               x       LIKE sy-msgty VALUE 'X',
             END OF c_msg_type .

  lo_filter = io_tech_request_context->get_filter( ).
  lt_filter = lo_filter->get_filter_select_options( ).
  lv_filter_str = lo_filter->get_filter_string( ).

*--Maps filter table lines to function module parameters
  LOOP AT lt_filter INTO ls_filter.
    CASE ls_filter-property.
      WHEN 'MATNR'.
        lv_matnr = ls_filter-select_options[ 1 ]-low.
      WHEN 'WERKS'.
        lv_werks = ls_filter-select_options[ 1 ]-low.
      WHEN 'ACNUM'.
        lv_acnum = ls_filter-select_options[ 1 ]-low.
      WHEN 'MATKL'.
        lv_matkl = ls_filter-select_options[ 1 ]-low.
      WHEN 'TPLNR_FL'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_tplnr_fl ).
    ENDCASE.
  ENDLOOP.

  IF lv_acnum IS NOT INITIAL.
    INSERT INITIAL LINE INTO TABLE lt_acnum
     ASSIGNING FIELD-SYMBOL(<ls_acnum>).
    IF sy-subrc EQ 0.
      <ls_acnum>-acnum = lv_acnum.
      CALL FUNCTION 'ZFMAC_READ'
        EXPORTING
          it_acnum       = lt_acnum
        IMPORTING
          et_achdr       = lt_achdr
          et_acitm       = lt_acitm
        EXCEPTIONS
          no_data_exists = 1
          OTHERS         = 2.

      DATA(lt_acitm_aux) = lt_acitm[].
      SORT lt_acitm_aux BY iwerk.
      DELETE ADJACENT DUPLICATES FROM lt_acitm_aux COMPARING iwerk.
      LOOP AT lt_acitm_aux INTO DATA(ls_acitm_aux).
        INSERT INITIAL LINE INTO TABLE lrt_werks
          ASSIGNING FIELD-SYMBOL(<lrs_werks>).
        IF sy-subrc EQ 0.
          <lrs_werks> = 'IEQ'.
          <lrs_werks>-low = ls_acitm_aux-iwerk.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

  INSERT INITIAL LINE INTO TABLE lrt_matnr
    ASSIGNING FIELD-SYMBOL(<lrs_matnr>).
  IF sy-subrc EQ 0.
    <lrs_matnr> = 'ICP'.
    <lrs_matnr>-low = '*' && lv_matkl.
  ENDIF.

  INSERT INITIAL LINE INTO TABLE lrt_matkl
    ASSIGNING FIELD-SYMBOL(<lrs_matkl>).
  IF sy-subrc EQ 0.
    <lrs_matkl> = 'ICP'.
    <lrs_matkl>-low = lv_matkl && '*'.
  ENDIF.

  READ TABLE lrt_werks TRANSPORTING NO FIELDS
    WITH KEY low = lv_werks.
  IF sy-subrc NE 0.
    INSERT INITIAL LINE INTO TABLE lrt_werks
      ASSIGNING <lrs_werks>.
    IF sy-subrc EQ 0.
      <lrs_werks> = 'IEQ'.
      <lrs_werks>-low = lv_werks.
    ENDIF.
  ENDIF.

*-- Fetch the Recipe Types
  SELECT h~rcnum, h~werks, h~matnr,
         h~rctex, h~rctyp, h~maktx,
         h~ausme
    INTO TABLE @DATA(lt_recipes)
    FROM zfmrchdr AS h
    INNER JOIN ztfmrctyp AS t
    ON h~rctyp EQ t~rctyp
   WHERE h~werks     IN @lrt_werks[]
     AND h~matnr     IN @lrt_matnr[]
*      AND h~maktx     IN @lrt_matkl[]
     AND t~orcamento EQ @abap_false.

  IF sy-subrc EQ 0.
    SORT lt_recipes BY rcnum rctex.
    DELETE lt_recipes WHERE matnr(4) NE 'TFOR'
                        AND matnr(4) NE 'TMAN'
                        AND matnr(4) NE 'TIMP'.
  ENDIF.

  LOOP AT lt_recipes INTO DATA(ls_recipe).
    INSERT INITIAL LINE INTO TABLE et_entityset
      ASSIGNING FIELD-SYMBOL(<ls_entityset>).
    IF sy-subrc EQ 0.
      <ls_entityset>-rcnum = ls_recipe-rcnum.
      <ls_entityset>-werks = ls_recipe-werks.
      <ls_entityset>-matnr = ls_recipe-matnr.
      <ls_entityset>-rctex = ls_recipe-rctex.
      <ls_entityset>-rctyp = ls_recipe-rctyp.
      <ls_entityset>-maktx = ls_recipe-maktx.
      <ls_entityset>-ausme = ls_recipe-ausme.
    ENDIF.
  ENDLOOP.

  LOOP AT et_entityset ASSIGNING <ls_entityset>.
    CASE <ls_entityset>-matnr(4).
      WHEN 'TFOR'
        OR 'TIMP'.
        <ls_entityset>-tipo = 'FORMAÇÃO'.
      WHEN 'TMAN'.
        <ls_entityset>-tipo = 'MANUTENÇÃO'.
    ENDCASE.
  ENDLOOP.

  IF et_entityset[] IS INITIAL.
*-- Give error message if no Recipe's Type maintained
*-- VERIFICAR TIPOS DE RECEITA (ZFMRCTYP)
    INSERT INITIAL LINE INTO TABLE et_entityset
      ASSIGNING <ls_entityset>.
    IF sy-subrc EQ 0.
      MESSAGE ID 'ZFMFP' TYPE c_msg_type-error
        NUMBER '077' INTO sy-msgli.
      <ls_entityset>-rctex = sy-msgli.
      <ls_entityset>-matnr = 'ERRO!'.
    ENDIF.
  ENDIF.

ENDMETHOD.


  METHOD getresumoorcset_get_entityset.

    TYPES: BEGIN OF ty_period,
             period TYPE period,
           END OF ty_period.

    DATA: lt_period    TYPE zabstc_orca_period,
          ls_period    LIKE LINE OF lt_period,
          lt_orcamento TYPE TABLE OF zabs_orcamento INITIAL SIZE 0,
          lt_months    TYPE STANDARD TABLE OF t247 INITIAL SIZE 0,
          lt_dates     TYPE /scwm/tt_lm_dates,
          lv_period    TYPE char6,
          ls_entityset LIKE LINE OF et_entityset,
          lv_ajahr     TYPE ajahr,
          lv_acnum     TYPE c LENGTH 40,
          lv_extwg     TYPE extwg,
          lv_matkl     TYPE matkl,
          lv_begda     TYPE begda,
          lv_endda     TYPE endda,
          lv_versao    TYPE zabs_del_ver_orc,
          lt_zfmaitm   TYPE zt_fmacitm,
          r_period     TYPE RANGE OF tvarvc-low,
          s_period     LIKE LINE OF r_period.

    LOOP AT it_filter_select_options INTO DATA(ls_filters).
      CASE ls_filters-property.
        WHEN 'Acnum'.
          lv_acnum = ls_filters-select_options[ 1 ]-low.
        WHEN 'Matkl'.
          lv_matkl = ls_filters-select_options[ 1 ]-low.
        WHEN 'Ajahr'.
          lv_ajahr = ls_filters-select_options[ 1 ]-low.
        WHEN 'Extwg'.
          lv_extwg = ls_filters-select_options[ 1 ]-low.
        WHEN 'Begda'.
          lv_begda = ls_filters-select_options[ 1 ]-low.
        WHEN 'Endda'.
          lv_endda = ls_filters-select_options[ 1 ]-low.
        WHEN 'Versao'.
          lv_versao = ls_filters-select_options[ 1 ]-low.
      ENDCASE.
    ENDLOOP.

    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                      = lv_begda
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS                    = 2.

    IF sy-subrc <> 0.
      DATA(lv_invalid_date) = abap_true.
    ELSE.
      lv_invalid_date = abap_false.
    ENDIF.

    IF lv_invalid_date = abap_false.
      CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
        EXPORTING
          date                      = lv_endda
        EXCEPTIONS
          plausibility_check_failed = 1
          OTHERS                    = 2.

      IF sy-subrc <> 0.
        lv_invalid_date = abap_true.
      ELSE.
        lv_invalid_date = abap_false.
      ENDIF.
    ENDIF.

    IF lv_invalid_date EQ abap_false.
      CALL FUNCTION '/SCWM/DATES_BETWEEN_TWO_DATES'
        EXPORTING
          iv_begda = lv_begda
          iv_endda = lv_endda
        IMPORTING
          et_dates = lt_dates.

      s_period = 'IEQ'.

      LOOP AT lt_dates INTO DATA(ls_dates).
        IF lv_period NE ls_dates(6).
          s_period-low =  ls_period = lv_period = ls_dates(6).
          APPEND: ls_period TO lt_period,
                  s_period  TO r_period.
        ENDIF.
      ENDLOOP.

      SELECT *
        INTO TABLE @lt_orcamento
        FROM zabs_orcamento
       WHERE acnum  EQ @lv_acnum
         AND extwg  EQ @lv_extwg
         AND period IN @r_period
         AND versao EQ @lv_versao.

      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE lt_zfmaitm
        FROM zfmaitm
       WHERE acnum EQ lv_acnum.

      IF sy-subrc EQ 0.
        SORT lt_zfmaitm BY acnum acpos tplnr_fl.
      ENDIF.

      DO 3 TIMES.
        CASE sy-index.
          WHEN 1.
            ls_entityset = zcl_vistex_agriplan=>zm_calc_line_resumo( EXPORTING im_index     = sy-index
                                                                               im_ajahr     = lv_ajahr
                                                                               im_period    = lt_period
                                                                               im_orcamento = lt_orcamento
                                                                               im_zfmaitm   = lt_zfmaitm ).
            ls_entityset-resumo = 'Área Formação'.
          WHEN 2.
            ls_entityset = zcl_vistex_agriplan=>zm_calc_line_resumo( EXPORTING im_index     = sy-index
                                                                               im_ajahr     = lv_ajahr
                                                                               im_period    = lt_period
                                                                               im_orcamento = lt_orcamento
                                                                               im_zfmaitm   = lt_zfmaitm ).
            ls_entityset-resumo = 'Área Manutenção'.
          WHEN 3.
            ls_entityset = zcl_vistex_agriplan=>zm_calc_line_resumo( EXPORTING im_index     = sy-index
                                                                               im_ajahr     = lv_ajahr
                                                                               im_period    = lt_period
                                                                               im_orcamento = lt_orcamento
                                                                               im_zfmaitm   = lt_zfmaitm ).
            ls_entityset-resumo = 'Custo Total'.
          WHEN OTHERS.
        ENDCASE.

        APPEND ls_entityset TO et_entityset.
        CLEAR ls_entityset.
      ENDDO.
    ENDIF.

  ENDMETHOD.


  METHOD getsafraset_get_entityset.

    DATA: lt_achdr     TYPE  zt_fmachdr,
          lt_acitm     TYPE  zt_fmacitm,
          lr_werks     TYPE /iwbep/t_cod_select_options,
          lr_acnum     TYPE /iwbep/t_cod_select_options,
          ls_entityset LIKE LINE OF et_entityset.

    LOOP AT it_filter_select_options INTO DATA(ls_filters).
      CASE ls_filters-property.
        WHEN 'Werks'.
          lr_werks = ls_filters-select_options.
        WHEN 'Acnum'.
          lr_acnum = ls_filters-select_options.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

    CALL FUNCTION 'ZFMAC_READ'
      IMPORTING
        et_achdr       = lt_achdr
        et_acitm       = lt_acitm
      EXCEPTIONS
        no_data_exists = 1
        OTHERS         = 2.

    IF sy-subrc EQ 0.
      LOOP AT lt_achdr INTO DATA(ls_achdr).
        MOVE-CORRESPONDING ls_achdr TO ls_entityset.
        ls_entityset-key   = ls_achdr-ajahr.
        ls_entityset-descr = ls_achdr-ajahr.
        APPEND ls_entityset TO et_entityset.
        CLEAR ls_entityset.
      ENDLOOP.
    ENDIF.

    IF lr_werks[] IS NOT INITIAL.
      DELETE et_entityset WHERE werks NOT IN lr_werks.
    ENDIF.

    IF lr_acnum[] IS NOT INITIAL.
      DELETE et_entityset WHERE acnum NOT IN lr_acnum.
    ENDIF.

    SORT et_entityset BY ajahr ASCENDING.
    DELETE ADJACENT DUPLICATES FROM et_entityset COMPARING ajahr.

  ENDMETHOD.


  METHOD getstoragelocati_get_entityset.

*-- Local Types
    TYPES: BEGIN OF ly_mard,
             matnr TYPE matnr,
             werks TYPE werks_d,
             lgort TYPE lgort_d,
             lvorm TYPE lvolg,
             labst TYPE labst,
           END OF ly_mard.

*-- Local Declarations
    DATA: lt_filter       TYPE /iwbep/t_mgw_select_option,
          ls_filter       TYPE /iwbep/s_mgw_select_option,
          lt_mard         TYPE STANDARD TABLE OF ly_mard INITIAL SIZE 0,
          lt_mard_collect LIKE lt_mard,
          ls_mard_collect LIKE LINE OF lt_mard_collect,
          lv_filter_str   TYPE string,
          lo_filter       TYPE REF TO /iwbep/if_mgw_req_filter,
          lrt_matnr       TYPE /iwbep/t_cod_select_options,
          lv_matnr        TYPE matnr,
          lv_werks        TYPE werks_d,
          lv_lgort        TYPE lgort_d,
          ls_entityset    LIKE LINE OF et_entityset.

    lo_filter = io_tech_request_context->get_filter( ).
    lt_filter = lo_filter->get_filter_select_options( ).
    lv_filter_str = lo_filter->get_filter_string( ).

*--Maps filter table lines to function module parameters
    LOOP AT lt_filter INTO ls_filter.
      CASE ls_filter-property.
        WHEN 'MATNR_INS'.
          lrt_matnr = ls_filter-select_options.
        WHEN 'WERKS'.
          lv_werks = ls_filter-select_options[ 1 ]-low.
        WHEN 'LGORT'.
          lv_lgort = ls_filter-select_options[ 1 ]-low.
      ENDCASE.
    ENDLOOP.

    SELECT matnr, werks, lgort, lvorm, labst
      FROM mard
      INTO TABLE @lt_mard
     WHERE matnr IN @lrt_matnr[]
       AND werks EQ @lv_werks
       AND lvorm EQ @abap_false.

    IF sy-subrc EQ 0.
      DATA(lt_storage_location) = lt_mard[].
      SORT lt_storage_location BY lgort.
      DELETE ADJACENT DUPLICATES FROM lt_storage_location COMPARING lgort.

      SORT lt_mard BY lgort matnr.
      LOOP AT lt_storage_location INTO DATA(ls_sloc).
        DATA(lv_tabix) = sy-tabix.
        DATA(lv_sloc) = abap_true.
        LOOP AT lrt_matnr INTO DATA(lrs_matnr).
          lv_matnr = lrs_matnr-low.
          READ TABLE lt_mard INTO DATA(ls_mard)
            WITH KEY lgort = ls_sloc-lgort
                     matnr = lv_matnr BINARY SEARCH.
          IF sy-subrc NE 0.
            lv_sloc = abap_false.
            EXIT.
          ENDIF.
        ENDLOOP.
        IF lv_sloc EQ abap_false.
          DELETE lt_mard WHERE lgort = ls_sloc-lgort.
        ENDIF.
      ENDLOOP.

      SORT lt_mard BY werks lgort.
      DELETE ADJACENT DUPLICATES FROM lt_mard
        COMPARING werks lgort.

      SELECT werks, lgort, lgobe
        FROM t001l
        INTO TABLE @DATA(lt_t001l)
        FOR ALL ENTRIES IN @lt_mard
       WHERE werks = @lt_mard-werks
         AND lgort = @lt_mard-lgort.

      SORT lt_t001l BY werks lgort.
    ENDIF.

    LOOP AT lt_mard INTO ls_sloc.
      READ TABLE lt_t001l INTO DATA(ls_t001l)
        WITH KEY werks = ls_sloc-werks
                 lgort = ls_sloc-lgort BINARY SEARCH.
      IF sy-subrc NE 0.
        CLEAR ls_t001l.
      ENDIF.
      INSERT INITIAL LINE INTO TABLE et_entityset
       ASSIGNING FIELD-SYMBOL(<ls_entityset>).
      IF sy-subrc EQ 0.
        <ls_entityset>-matnr_ins = ls_sloc-matnr.
        <ls_entityset>-werks     = ls_sloc-werks.
        <ls_entityset>-lgort     = ls_sloc-lgort.
        <ls_entityset>-labst     = ls_sloc-labst.
        <ls_entityset>-lgobe     = ls_t001l-lgobe.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD gettalhaoidadese_get_entityset.

    TYPES: BEGIN OF ty_period,
             period TYPE period,
           END OF ty_period,

           BEGIN OF ty_collect,
*             tplnr_fl   TYPE string,
             period    TYPE spmon,
             aareaform TYPE zfmacqtb,
             aareamanu TYPE zfmacqtb,
           END OF   ty_collect.

    DATA: lt_period  TYPE TABLE OF ty_period,
          lt_collect TYPE TABLE OF ty_collect,
          ls_period  TYPE ty_period,
          ls_collect TYPE ty_collect.

    DATA: lt_months    TYPE TABLE OF t247,
          lt_dates     TYPE /scwm/tt_lm_dates,
          ls_dates     LIKE LINE OF lt_dates,
          ls_entityset LIKE LINE OF et_entityset.

    DATA: lv_acnum  TYPE zfmacnum,
          lv_matkl  TYPE matkl,
          lv_begda  TYPE begda,
          lv_endda  TYPE endda,
          lv_extwg  TYPE extwg,
          lv_string TYPE string.

    DATA: r_matkl TYPE RANGE OF tvarvc-low,
          s_matkl LIKE LINE OF r_matkl.
    DATA: lv_begda_bis TYPE begda,
          lv_months    TYPE string.


    LOOP AT it_filter_select_options INTO DATA(ls_filters).
      CASE ls_filters-property.
        WHEN 'Acnum'.
          lv_acnum = ls_filters-select_options[ 1 ]-low.
        WHEN 'Matkl'.
          lv_matkl = ls_filters-select_options[ 1 ]-low.
        WHEN 'Begda'.
          lv_begda = ls_filters-select_options[ 1 ]-low.
        WHEN 'Endda'.
          lv_endda = ls_filters-select_options[ 1 ]-low.
        WHEN 'Extwg'.
          lv_extwg = ls_filters-select_options[ 1 ]-low.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

    CALL FUNCTION '/SCWM/DATES_BETWEEN_TWO_DATES'
      EXPORTING
        iv_begda = lv_begda
        iv_endda = lv_endda
      IMPORTING
        et_dates = lt_dates.

    LOOP AT lt_dates INTO ls_dates.
      ls_period-period = ls_dates(6).
      APPEND ls_period TO lt_period.
    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM lt_period.


    s_matkl-sign = 'I'.
    s_matkl-option = 'CP'.
    s_matkl-low = lv_matkl && '*'.
    APPEND s_matkl TO r_matkl.

    SELECT *
      INTO TABLE @DATA(lt_zfmacvlcl)
      FROM zfmacvlcl
      WHERE acnum EQ @lv_acnum
        AND maktx IN @r_matkl.

    IF sy-subrc EQ 0.
      SELECT *
        INTO TABLE @DATA(lt_glflcma)
        FROM /agri/glflcma
        FOR ALL ENTRIES IN @lt_zfmacvlcl
       WHERE tplnr_fl EQ @lt_zfmacvlcl-tplnr_fl
         AND loevm    NE @abap_true
         AND astat    EQ 'A'.

      IF lt_glflcma[] IS NOT INITIAL.
        SORT lt_glflcma BY tplnr_fl contr.
        DELETE ADJACENT DUPLICATES FROM lt_glflcma COMPARING tplnr_fl.

        LOOP AT lt_period INTO ls_period.

          CLEAR: lv_begda_bis.

          CONCATENATE ls_period '01' INTO lv_begda_bis.

          LOOP AT lt_glflcma INTO DATA(ls_glflcma).

            CALL FUNCTION 'MONTHS_BETWEEN_TWO_DATES'
              EXPORTING
                i_datum_bis = lv_begda_bis
                i_datum_von = ls_glflcma-zzfazplantio
*               I_KZ_INCL_BIS       = ' '
              IMPORTING
                e_monate    = lv_months.

            CONDENSE lv_months NO-GAPS.
            ls_entityset-acnum = lv_acnum.
            ls_entityset-extwg = lv_extwg.
            ls_entityset-matkl = lv_matkl.
            ls_entityset-tplnrfl = ls_glflcma-tplnr_fl.

            CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
              EXPORTING
                input  = ls_glflcma-tplnr_fl
              IMPORTING
                output = ls_entityset-talhao.

            IF lv_months LT '36'.
              lv_string = 'TFOR' && lv_matkl.
            ELSE.
              lv_string = 'TMAN' && lv_matkl.
            ENDIF.

            READ TABLE lt_zfmacvlcl INTO DATA(ls_zfmacvlcl)
              WITH KEY tplnr_fl = ls_glflcma-tplnr_fl
                       matnr    = lv_string.
            IF sy-subrc EQ 0.
              IF lv_months LT '36'.
                ls_collect-aareaform = ls_zfmacvlcl-acarx.
              ELSE.
                ls_collect-aareamanu = ls_zfmacvlcl-acarx.
              ENDIF.
            ENDIF.

            ls_collect-period  = ls_period-period.
            COLLECT ls_collect INTO lt_collect.
            CLEAR: ls_collect, ls_entityset.
          ENDLOOP.
        ENDLOOP.
      ENDIF.
    ENDIF.

    LOOP AT lt_collect INTO ls_collect.
      MOVE-CORRESPONDING ls_collect TO ls_entityset.
      APPEND ls_entityset TO et_entityset.
      CLEAR: ls_entityset, ls_collect.
    ENDLOOP.

  ENDMETHOD.


  METHOD gettalhaoset_get_entityset.

    DATA: lt_achdr TYPE  zt_fmachdr,
          lt_acitm TYPE  zt_fmacitm,
          lt_acnum TYPE zt_fm_acnum,
          lt_string TYPE TABLE OF string,
          lt_glflot TYPE TABLE OF /AGRI/GLFLOT.

    DATA: r_acnum TYPE /iwbep/t_cod_select_options.

    DATA: ls_acnum TYPE zsc_fm_acnum,
          ls_entityset LIKE LINE OF et_entityset.

    DATA: lv_string TYPE string.

    LOOP AT it_filter_select_options INTO DATA(ls_filters).
      CASE ls_filters-property.
        WHEN 'Acnum'.
          r_acnum = ls_filters-select_options.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

    LOOP AT r_acnum INTO DATA(rs_acnum).
      ls_acnum-acnum = rs_acnum-low.
      APPEND ls_acnum TO lt_acnum.
      CLEAR: ls_acnum.
    ENDLOOP.

    CALL FUNCTION 'ZFMAC_READ'
     EXPORTING
       it_acnum             = lt_acnum
     IMPORTING
       et_achdr             = lt_achdr
       et_acitm             = lt_acitm
*       et_acvlc             =
     EXCEPTIONS
       no_data_exists       = 1
       OTHERS               = 2
              .
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    IF lt_acitm IS NOT INITIAL.
      SELECT *
        INTO TABLE lt_glflot
        FROM /agri/glflot
        FOR ALL ENTRIES IN lt_acitm
        WHERE TPLNR_FL EQ lt_acitm-tplnr_fl AND
              fltyp EQ '2'.
    ENDIF.

    LOOP AT lt_acitm INTO DATA(ls_item).

      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
        EXPORTING
          input         = ls_item-tplnr_fl
       IMPORTING
          output        = lv_string.

    split lv_string AT '-' INTO TABLE lt_string.
    READ TABLE lt_string INTO DATA(ls_string) INDEX 2.
    IF sy-subrc EQ 0.
      READ TABLE lt_glflot INTO DATA(ls_glflot) WITH KEY tplnr_fl = ls_item-tplnr_fl.
      IF sy-subrc EQ 0.
        ls_entityset-key = ls_string.
        TRANSLATE ls_glflot-pltxt TO UPPER CASE.
        ls_entityset-descr = ls_glflot-pltxt.
        APPEND ls_entityset TO et_entityset.
        CLEAR: ls_entityset.
      ENDIF.
    ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD gettarefasorcset_get_entityset.

    TYPES: BEGIN OF ty_matkl,
             matnr   TYPE matnr,
             matkl   TYPE matkl,
             wgbez   TYPE wgbez,
             wgbez60 TYPE wgbez60,
           END OF   ty_matkl.

    TYPES: BEGIN OF ty_vcalda,
             tplnr_fl TYPE  /agri/gltplnr_fl,
             acnum    TYPE  zfmacnum,
             vornr    TYPE vornr,
             matnr    TYPE zfmacmatnr,
             posnr    TYPE zfmacposnr,
             maktx    TYPE zfmacmaktx,
             acidt    TYPE zfmacidt,
             acdis    TYPE zfmacdis,
             acvcl    TYPE zfmacvcl,
             acfcb    TYPE zfmacfcb,
             acqtb    TYPE zfmacqtb,
             acuqb    TYPE zfmacuqb,
             acren    TYPE zfmacren,
             unren    TYPE zfmacunren,
             acpex    TYPE zfmacpex,
             acarx    TYPE zfmacarx,
             unarx    TYPE zfmacunarx,
           END OF ty_vcalda.

*Modify Start on 23.09.2020 INC0023166 T_C.KARANAM
    TYPES: BEGIN OF ty_orcamento,
             matkl      TYPE matkl,
             period     TYPE spmon,
             aarea_form TYPE zabs_del_qty_20,
             aarea_manu TYPE zabs_del_qty_20,
           END OF ty_orcamento,
           BEGIN OF ty_orc_tot,
             matkl     TYPE matkl,
             period    TYPE spmon,
             form_manu TYPE zabs_del_qty_20,
           END OF ty_orc_tot.
*Modify End on 23.09.2020 INC0023166 T_C.KARANAM

    DATA: lt_plpo      TYPE TABLE OF plpo,
          lt_fmfpcom   TYPE TABLE OF /agri/s_fmfpcom,
          lt_fmfpitm   TYPE TABLE OF /agri/s_fmfpitm,
          lt_makt      TYPE TABLE OF makt,
          lt_zfmacvlcl TYPE TABLE OF ty_vcalda,
          lt_vcalda    TYPE TABLE OF zfmacvlcl,
          lt_matkl     TYPE TABLE OF ty_matkl,
          lt_mara      TYPE TABLE OF ty_matkl,
          lt_t023t     TYPE TABLE OF t023t,
          lv_total     TYPE zabs_del_qty_20, " zfmacren,
          lt_zfmplmens TYPE TABLE OF zfmplmens,
          lt_acnum     TYPE zt_fm_acnum,
          lt_achdr     TYPE zt_fmachdr,
          lt_acitm     TYPE zt_fmacitm,
          lt_week      TYPE TABLE OF zfmplprsemanal,
          r_mtart      TYPE /iwbep/t_cod_select_options,
          ls_mtart     LIKE LINE OF r_mtart,
          ls_entityset LIKE LINE OF et_entityset,
          lv_pltyp     TYPE zfmplann,
          lv_acnum     TYPE zfmacnum,
          lv_begda     TYPE zfmdatab,
          lv_begda_aux TYPE char10,
          lv_endda_aux TYPE zfmdatbi,
          lv_extwg     TYPE extwg,
          lv_month     TYPE c LENGTH 2,
          r_matnr      TYPE RANGE OF matnr,
          s_matnr      LIKE LINE OF r_matnr,
          lv_tabix     TYPE sy-tabix,
          lv_descr     TYPE string,
          lv_media     TYPE zfmacren,
          lv_char      TYPE c LENGTH 15,
          lv_endda     TYPE endda,
          lv_string    TYPE string,
          lv_index     TYPE sy-tabix,
          lv_tot_prog  TYPE zfmplapr,
          lv_calc      TYPE zfmplapr,
          lv_date      TYPE c LENGTH 10,
          lr_acnum     TYPE RANGE OF zfmacnum,
          lr_matnr     TYPE RANGE OF zfmaplmatnr,
          lr_periodo   TYPE RANGE OF spmon,
          ls_periodo   LIKE LINE OF lr_periodo,
          lv_top       TYPE i,
          lv_skip      TYPE i,
          lv_max_index TYPE i,
          lv_matkl     TYPE matkl,
          l_n          TYPE i,
          lt_dates     TYPE /scwm/tt_lm_dates,
          lt_mensal    TYPE TABLE OF zfmplmens,
          ls_mensal    LIKE LINE OF lt_mensal,
          lv_versao    TYPE zabs_del_ver_orc,
          lv_period    TYPE char6,
          lv_ajahr     TYPE ajahr.

*Modify Start on 23.09.2020 INC0023166 T_C.KARANAM
    DATA: ls_orcamen TYPE ty_orcamento,
          lt_orcamen TYPE STANDARD TABLE OF ty_orcamento INITIAL SIZE 0,
          ls_orc_tot TYPE ty_orc_tot,
          lt_orc_tot TYPE STANDARD TABLE OF ty_orc_tot INITIAL SIZE 0.
*Modify End on 23.09.2020 INC0023166 T_C.KARANAM

    LOOP AT it_filter_select_options INTO DATA(ls_filters).
      CASE ls_filters-property.
        WHEN 'Acnum'.
          lv_acnum = ls_filters-select_options[ 1 ]-low.
        WHEN 'Matkl'.
          lv_matkl = ls_filters-select_options[ 1 ]-low.
        WHEN 'Begda'.
          lv_begda = ls_filters-select_options[ 1 ]-low.
        WHEN 'Endda'.
          lv_endda = ls_filters-select_options[ 1 ]-low.
        WHEN 'Extwg'.
          lv_extwg = ls_filters-select_options[ 1 ]-low.
        WHEN 'Ajahr'.
          lv_ajahr = ls_filters-select_options[ 1 ]-low.
        WHEN 'Versao'.
          lv_versao = ls_filters-select_options[ 1 ]-low.
      ENDCASE.
    ENDLOOP.

    SELECT *
      INTO TABLE @DATA(lt_area)
      FROM zfmachdr
     WHERE acnum EQ @lv_acnum
       AND ajahr EQ @lv_ajahr.

    IF sy-subrc EQ 0.
      SORT lt_area BY acnum ajahr.
      READ TABLE lt_area INTO DATA(ls_area) INDEX 1.
    ENDIF.

    CALL FUNCTION '/SCWM/DATES_BETWEEN_TWO_DATES'
      EXPORTING
        iv_begda = lv_begda
        iv_endda = lv_endda
      IMPORTING
        et_dates = lt_dates.

    ls_periodo = 'IEQ'.
    LOOP AT lt_dates INTO DATA(ls_date).
      IF lv_period NE ls_date(6).
        ls_periodo-low = lv_period = ls_date(6).
        APPEND ls_periodo TO lr_periodo.
      ENDIF.
    ENDLOOP.

    SELECT *
      FROM zfmplmens
      INTO TABLE lt_mensal
     WHERE acnum EQ lv_acnum
       AND extwg EQ lv_extwg
       AND periodo IN lr_periodo.

    IF sy-subrc EQ 0.
      SORT lt_mensal BY acnum extwg matkl periodo.
    ENDIF.

    r_mtart = zcl_agri_utilities=>zm_get_tvarvc( iv_name = 'ZAGRI_PROCESSO' ).

    ls_mtart-sign   = 'I'.
    ls_mtart-option = 'EQ'.
    MODIFY r_mtart FROM ls_mtart TRANSPORTING sign option WHERE low <> ''.

    SELECT *
      INTO TABLE lt_t023t
      FROM t023t
      WHERE spras EQ sy-langu.

    IF sy-subrc EQ 0.
      SORT lt_t023t BY matkl.
    ENDIF.

    CASE lv_extwg.
      WHEN 'GIM'.
        s_matnr-sign = 'I'.
        s_matnr-option = 'CP'.
        s_matnr-low = 'TIM*'.
        APPEND s_matnr TO r_matnr.
      WHEN 'GTC'.
        s_matnr-sign = 'I'.
        s_matnr-option = 'CP'.
        s_matnr-low = 'TMAN*'.
        APPEND s_matnr TO r_matnr.
        s_matnr-low = 'TFOR*'.
        APPEND s_matnr TO r_matnr.
      WHEN 'GCO'.
        s_matnr-sign = 'I'.
        s_matnr-option = 'CP'.
        s_matnr-low = 'TCOL*'.
        APPEND s_matnr TO r_matnr.
      WHEN 'GIR'.
        s_matnr-sign = 'I'.
        s_matnr-option = 'CP'.
        s_matnr-low = 'TMAN*'.
        APPEND s_matnr TO r_matnr.
      WHEN OTHERS.
    ENDCASE.

    SELECT *
      INTO TABLE @DATA(lt_orcamento)
      FROM zabs_orcamento
     WHERE acnum  EQ @lv_acnum
       AND extwg  EQ @lv_extwg
       AND versao EQ @lv_versao.

    SELECT matnr matkl
      INTO CORRESPONDING FIELDS OF TABLE lt_matkl
      FROM mara
     WHERE matnr IN r_matnr
       AND mtart IN r_mtart
       AND extwg EQ lv_extwg
       AND matkl NE space.

    IF sy-subrc EQ 0.
      SORT lt_matkl BY matkl.
      lt_mara[] = lt_matkl[].

      DELETE ADJACENT DUPLICATES FROM lt_matkl COMPARING matkl.
**Modify Start on 23.09.2020 INC0023166 T_C.KARANAM
      REFRESH: lt_orcamen, lt_orc_tot.
      CLEAR: lt_orcamen, lt_orcamen.
      LOOP AT lt_orcamento INTO DATA(ls_orcamento_1).
        ls_orcamen-matkl      = ls_orcamento_1-matkl.
        ls_orcamen-period     = ls_orcamento_1-period.
        ls_orcamen-aarea_form = ls_orcamento_1-aarea_form.
        ls_orcamen-aarea_manu = ls_orcamento_1-aarea_manu.
        COLLECT ls_orcamen INTO lt_orcamen.
        CLEAR ls_orcamento_1.
      ENDLOOP.
      LOOP AT lt_orcamen INTO ls_orcamen.
        ls_orc_tot-matkl     = ls_orcamen-matkl.
        ls_orc_tot-period    = ls_orcamen-period.
        ls_orc_tot-form_manu = ls_orcamen-aarea_form +
                               ls_orcamen-aarea_manu.
        APPEND ls_orc_tot TO lt_orc_tot.
        CLEAR: ls_orc_tot, ls_orcamen.
      ENDLOOP.
**Modify End on 23.09.2020 INC0023160 T_C.KARANAM

      LOOP AT lt_matkl INTO DATA(ls_matkl).
        READ TABLE lt_t023t INTO DATA(ls_t023t) WITH KEY matkl = ls_matkl-matkl.
        IF sy-subrc EQ 0.
          ls_entityset-matnr = ls_matkl-matnr.
          ls_entityset-matkl = ls_matkl-matkl.
          ls_entityset-wgbez = ls_t023t-wgbez.
          TRANSLATE ls_entityset-wgbez TO UPPER CASE.

          LOOP AT lr_periodo INTO ls_periodo.
            lv_tabix = sy-tabix.
            CLEAR lv_total.
**Modify Start on 23.09.2020 INC0023166 T_C.KARANAM
**            LOOP AT lt_orcamento INTO DATA(ls_orcamento)
**              WHERE period EQ ls_periodo-low
**                AND matkl  EQ ls_matkl-matkl.
**              lv_total = lv_total + ls_orcamento-aarea_form + ls_orcamento-aarea_manu.
**            ENDLOOP.
            READ TABLE lt_orc_tot INTO ls_orc_tot
              WITH KEY period = ls_periodo-low
                       matkl = ls_matkl-matkl.
            IF sy-subrc IS INITIAL .
              lv_total = ls_orc_tot-form_manu.
            ENDIF.
**Modify End on 23.09.2020 INC0023166 T_C.KARANAM
            CASE lv_tabix.
              WHEN '1'.
                ls_entityset-d1 = lv_total.
              WHEN '2'.
                ls_entityset-d2 = lv_total.
              WHEN '3'.
                ls_entityset-d3 = lv_total.
              WHEN '4'.
                ls_entityset-d4 = lv_total.
              WHEN '5'.
                ls_entityset-d5 = lv_total.
              WHEN '6'.
                ls_entityset-d6 = lv_total.
              WHEN '7'.
                ls_entityset-d7 = lv_total.
              WHEN '8'.
                ls_entityset-d8 = lv_total.
              WHEN '9'.
                ls_entityset-d9 = lv_total.
              WHEN '10'.
                ls_entityset-d10 = lv_total.
              WHEN '11'.
                ls_entityset-d11 = lv_total.
              WHEN '12'.
                ls_entityset-d12 = lv_total.
              WHEN OTHERS.
            ENDCASE.
          ENDLOOP.

          APPEND ls_entityset TO et_entityset.
          CLEAR: ls_entityset.
        ENDIF.
      ENDLOOP.
    ENDIF.

    SORT et_entityset BY wgbez ASCENDING.

    LOOP AT et_entityset ASSIGNING FIELD-SYMBOL(<ls_entityset>).
      <ls_entityset>-wgbez = <ls_entityset>-matkl(4) && '-' && <ls_entityset>-wgbez(35).
    ENDLOOP.

  ENDMETHOD.


  METHOD gettaskinsumoset_get_entityset.


    TYPES: BEGIN OF ty_matkl,
             matnr   TYPE matnr,
             matkl   TYPE matkl,
             wgbez   TYPE wgbez,
             wgbez60 TYPE wgbez60,
           END OF   ty_matkl.

    TYPES: BEGIN OF ty_vcalda,
             tplnr_fl TYPE  /agri/gltplnr_fl,
             acnum    TYPE  zfmacnum,
             vornr    TYPE vornr,
             matnr    TYPE zfmacmatnr,
             posnr    TYPE zfmacposnr,
             maktx    TYPE zfmacmaktx,
             acidt    TYPE zfmacidt,
             acdis    TYPE zfmacdis,
             acvcl    TYPE zfmacvcl,
             acfcb    TYPE zfmacfcb,
             acqtb    TYPE zfmacqtb,
             acuqb    TYPE zfmacuqb,
             acren    TYPE zfmacren,
             unren    TYPE zfmacunren,
             acpex    TYPE zfmacpex,
             acarx    TYPE zfmacarx,
             unarx    TYPE zfmacunarx,
           END OF   ty_vcalda.

    DATA: lt_plpo      TYPE TABLE OF plpo,
          lt_fmfpcom   TYPE TABLE OF /agri/s_fmfpcom,
          lt_fmfpitm   TYPE TABLE OF /agri/s_fmfpitm,
          lt_makt      TYPE TABLE OF makt,
          lt_zfmacvlcl TYPE TABLE OF ty_vcalda,
          lt_vcalda    TYPE TABLE OF zfmacvlcl,
          lt_matkl     TYPE TABLE OF ty_matkl,
          lt_t023t     TYPE TABLE OF t023t,
          lv_total     TYPE zfmacren,
          lt_zfmplmens TYPE TABLE OF zfmplmens.

    DATA: lt_acnum TYPE zt_fm_acnum,
          lt_achdr TYPE zt_fmachdr,
          lt_acitm TYPE zt_fmacitm,
          lt_week  TYPE TABLE OF zfmplprsemanal.

*-- BOC-T_T.KONNO
*    DATA: r_mtart TYPE /iwbep/t_cod_select_options.
    DATA: r_mtart  TYPE /iwbep/t_cod_select_options,
          wa_mtart LIKE LINE OF r_mtart.
*-- EOC-T_T.KONNO

    DATA: ls_entityset LIKE LINE OF et_entityset.

    DATA: lv_pltyp     TYPE  zfmplann,
          lv_acnum     TYPE  zfmacnum,
          lv_begda     TYPE  zfmdatab,
          lv_begda_aux TYPE char10,
          lv_endda     TYPE  zfmdatbi,
          lv_extwg     TYPE  extwg.

    DATA: lv_month    TYPE c LENGTH 2,
          lv_tabix    TYPE sy-tabix,
          lv_descr    TYPE string,
          lv_media    TYPE zfmacren,
          lv_char     TYPE c LENGTH 15,
          lv_string   TYPE string,
          lv_tot_prog TYPE zfmplapr,
          lv_calc     TYPE zfmplapr,
          lv_date     TYPE c LENGTH 10.

    DATA: lr_acnum   TYPE RANGE OF zfmacnum,
          lr_matnr   TYPE RANGE OF zfmaplmatnr,
          lr_periodo TYPE RANGE OF spmon,
          ls_periodo LIKE LINE OF lr_periodo.

    DATA: lv_osql_where_clause TYPE string,
          lv_top               TYPE i,
          lv_skip              TYPE i,
          lv_max_index         TYPE i,
          lv_matkl             TYPE matkl,
          l_n                  TYPE i,
          lt_mensal            TYPE TABLE OF zfmplmens,
          ls_mensal            LIKE LINE OF lt_mensal,
          lt_dates             TYPE /scwm/tt_lm_dates.

*    IF sy-uname EQ 'T_T.KONNO'.
*      BREAK-POINT.
*    ENDIF.

    lv_top = io_tech_request_context->get_top( ).
    lv_skip = io_tech_request_context->get_skip( ).

    IF lv_top IS NOT INITIAL.
      lv_max_index = lv_top + lv_skip.
    ENDIF.

    LOOP AT it_filter_select_options INTO DATA(ls_filters).
      CASE ls_filters-property.
        WHEN 'Acnum'.
          lv_acnum = ls_filters-select_options[ 1 ]-low.
        WHEN 'Matkl'.
          lv_matkl = ls_filters-select_options[ 1 ]-low.
        WHEN 'Begda'.
          lv_begda = ls_filters-select_options[ 1 ]-low.
        WHEN 'Endda'.
          lv_endda = ls_filters-select_options[ 1 ]-low.
        WHEN 'Extwg'.
          lv_extwg = ls_filters-select_options[ 1 ]-low.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

**********************************************************************

**********************************************************************

    lv_osql_where_clause = io_tech_request_context->get_osql_where_clause( ).


    IF lv_endda IS NOT INITIAL.
      lv_begda = lv_endda.
      CLEAR: lv_endda.
      CALL FUNCTION '/CPD/ADD_MONTH_TO_DATE'
        EXPORTING
          months  = '4'
          olddate = lv_begda
        IMPORTING
          newdate = lv_endda.

    ELSE.

      IF lv_begda IS NOT INITIAL.
        lv_begda = lv_begda.

        CALL FUNCTION 'HR_CL_SUBTRACT_MONTH_TO_DATE'
          EXPORTING
            p_date       = lv_begda
            p_backmonths = '4'
          IMPORTING
            p_newdate    = lv_begda.

        CALL FUNCTION '/CPD/ADD_MONTH_TO_DATE'
          EXPORTING
            months  = '4'
            olddate = lv_begda
          IMPORTING
            newdate = lv_endda.

      ELSE.
        lv_begda = sy-datum.
        CALL FUNCTION '/CPD/ADD_MONTH_TO_DATE'
          EXPORTING
            months  = '4'
            olddate = lv_begda
          IMPORTING
            newdate = lv_endda.
      ENDIF.

    ENDIF.


    CALL FUNCTION '/SCWM/DATES_BETWEEN_TWO_DATES'
      EXPORTING
        iv_begda = lv_begda
        iv_endda = lv_endda
      IMPORTING
        et_dates = lt_dates.

    LOOP AT lt_dates INTO DATA(ls_dates).
      ls_periodo-sign = 'I'.
      ls_periodo-option = 'EQ'.
      ls_periodo-low = ls_dates(6).
      APPEND ls_periodo TO lr_periodo.
    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM lr_periodo COMPARING low.

    SELECT *
      FROM zfmplmens
      INTO TABLE lt_mensal
      WHERE acnum EQ lv_acnum AND
*            matkl EQ lv_matkl AND
            extwg EQ lv_extwg AND
            periodo IN lr_periodo.
    IF sy-subrc EQ 0.
      SORT lt_mensal BY acnum extwg matkl periodo.
    ENDIF.

**********************************************************************


    DATA: lt_months TYPE TABLE OF t247.
*          lt_dates TYPE /scwm/tt_lm_dates,
*          ls_dates LIKE LINE OF lt_dates.

    DATA: ls_acnum LIKE LINE OF lt_acnum.
*    READ TABLE it_key_tab INTO DATA(ls_key) INDEX 1.
*    IF sy-subrc EQ 0.
*      lv_begda = ls_key-value+6(4) && ls_key-value+3(2) && ls_key-value(2).
*    ENDIF.
    IF lv_begda_aux IS NOT INITIAL.
      lv_begda = lv_begda_aux+6(4) && lv_begda_aux+3(2) && lv_begda_aux(2).
    ELSE.
      lv_begda = sy-datum.
    ENDIF.


* Selecionar area de planejamento mensal
    SELECT *
      INTO TABLE lt_zfmplmens
      FROM zfmplmens
      WHERE acnum EQ lv_acnum.
    IF sy-subrc EQ 0.
      SORT lt_zfmplmens BY matkl.
    ENDIF.

    CALL FUNCTION 'FIAPPL_ADD_DAYS_TO_DATE'
      EXPORTING
        i_date      = lv_begda
        i_days      = '5'
        signum      = '+'
      IMPORTING
        e_calc_date = lv_endda.

    lv_endda = lv_endda - 1.

    CALL FUNCTION 'MONTH_NAMES_GET'
      EXPORTING
        language              = sy-langu
*     IMPORTING
*       RETURN_CODE           =
      TABLES
        month_names           = lt_months
      EXCEPTIONS
        month_names_not_found = 1
        OTHERS                = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    CALL FUNCTION '/SCWM/DATES_BETWEEN_TWO_DATES'
      EXPORTING
        iv_begda = lv_begda
        iv_endda = lv_endda
      IMPORTING
        et_dates = lt_dates.

    IF lv_begda IS INITIAL.
      lv_begda = sy-datum.
    ENDIF.
    IF lv_endda IS INITIAL.
      lv_endda = sy-datum.
    ENDIF.

    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE lt_zfmacvlcl
      FROM zfmacvlcl
      WHERE acnum EQ lv_acnum.
    IF sy-subrc EQ 0.

    ENDIF.

    ls_acnum-acnum = lv_acnum.
    APPEND ls_acnum TO lt_acnum.

    CALL FUNCTION 'ZFMAC_READ'
      EXPORTING
        it_acnum       = lt_acnum
      IMPORTING
        et_achdr       = lt_achdr
        et_acitm       = lt_acitm
      EXCEPTIONS
        no_data_exists = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    r_mtart = zcl_agri_utilities=>zm_get_tvarvc( iv_name = 'ZAGRI_PROCESSO' ).

*-- BOC-T_T.KONNO
    wa_mtart-sign = 'I'.
    wa_mtart-option = 'EQ'.
    MODIFY r_mtart FROM wa_mtart TRANSPORTING sign option WHERE low <> ''.
*-- EOC-T_T.KONNO

    SELECT *
      INTO TABLE lt_t023t
      FROM t023t
      WHERE spras EQ sy-langu.
    IF sy-subrc EQ 0.
      SORT lt_t023t BY matkl.
    ENDIF.

    DATA: r_matnr TYPE RANGE OF matnr,
          s_matnr LIKE LINE OF r_matnr.

    CASE lv_extwg.
      WHEN 'GIM'.
        s_matnr-sign = 'I'.
        s_matnr-option = 'CP'.
        s_matnr-low = 'TIM*'.
        APPEND s_matnr TO r_matnr.
      WHEN 'GTC'.
        s_matnr-sign = 'I'.
        s_matnr-option = 'CP'.
        s_matnr-low = 'TMAN*'.
        APPEND s_matnr TO r_matnr.
        s_matnr-low = 'TFOR*'.
        APPEND s_matnr TO r_matnr.
      WHEN 'GCO'.
        s_matnr-sign = 'I'.
        s_matnr-option = 'CP'.
        s_matnr-low = 'TCOL*'.
        APPEND s_matnr TO r_matnr.
      WHEN 'GIR'.
        s_matnr-sign = 'I'.
        s_matnr-option = 'CP'.
        s_matnr-low = 'TMAN*'.
        APPEND s_matnr TO r_matnr.
      WHEN OTHERS.
    ENDCASE.

    SELECT matnr matkl
      INTO CORRESPONDING FIELDS OF TABLE lt_matkl
      FROM mara
      WHERE matnr IN r_matnr AND
            mtart IN r_mtart AND
            extwg EQ lv_extwg AND
            matkl NE space.
    IF sy-subrc EQ 0.
      SORT lt_matkl BY matkl.
      DELETE ADJACENT DUPLICATES FROM lt_matkl COMPARING matkl.

      LOOP AT lt_matkl INTO DATA(ls_matkl).
        READ TABLE lt_t023t INTO DATA(ls_t023t) WITH KEY matkl = ls_matkl-matkl.
        IF sy-subrc EQ 0.

* Realizar calculo por dia
*         LOOP AT lt_dates INTO ls_dates.
          LOOP AT lr_periodo INTO ls_periodo.

            lv_tabix = sy-tabix.

* Ler programação mensal
            READ TABLE lt_mensal INTO ls_mensal WITH KEY matkl = ls_matkl-matkl
                                                         periodo = ls_periodo-low.
            IF sy-subrc EQ 0.

              CASE lv_tabix.
                WHEN '1'.
                  ls_entityset-d1 = ls_mensal-qtplm.
                WHEN '2'.
                  ls_entityset-d2 = ls_mensal-qtplm.
                WHEN '3'.
                  ls_entityset-d3 = ls_mensal-qtplm.
                WHEN '4'.
                  ls_entityset-d4 = ls_mensal-qtplm.
                WHEN '5'.
                  ls_entityset-d5 = ls_mensal-qtplm.
                WHEN '6'.
                  ls_entityset-d6 = ls_mensal-qtplm.
                WHEN '7'.
                  ls_entityset-d7 = ls_mensal-qtplm.
                WHEN '8'.
                  ls_entityset-d8 = ls_mensal-qtplm.
                WHEN '9'.
                  ls_entityset-d9 = ls_mensal-qtplm.
                WHEN '10'.
                  ls_entityset-d10 = ls_mensal-qtplm.
                WHEN '11'.
                  ls_entityset-d11 = ls_mensal-qtplm.
                WHEN '12'.
                  ls_entityset-d12 = ls_mensal-qtplm.
                WHEN OTHERS.
              ENDCASE.
            ENDIF.
          ENDLOOP.

          ls_entityset-matkl = ls_matkl-matkl.
          ls_entityset-wgbez = ls_t023t-wgbez.
          TRANSLATE ls_entityset-wgbez TO UPPER CASE.
          APPEND ls_entityset TO et_entityset.
          CLEAR: ls_entityset.
        ENDIF.
      ENDLOOP.
    ENDIF.


  ENDMETHOD.


  METHOD getterrenoset_get_entityset.

    DATA: lt_achdr  TYPE  zt_fmachdr,
          lt_acitm  TYPE  zt_fmacitm,
          lt_acnum  TYPE zt_fm_acnum,
          lt_string TYPE TABLE OF string,
          lt_glflot TYPE TABLE OF /agri/glflot.

    DATA: r_acnum TYPE /iwbep/t_cod_select_options.

    DATA: ls_acnum     TYPE zsc_fm_acnum,
          ls_entityset LIKE LINE OF et_entityset.

    DATA: lv_string TYPE string.

    LOOP AT it_filter_select_options INTO DATA(ls_filters).
      CASE ls_filters-property.
        WHEN 'Acnum'.
          r_acnum = ls_filters-select_options.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

    LOOP AT r_acnum INTO DATA(rs_acnum).
      ls_acnum-acnum = rs_acnum-low.
      APPEND ls_acnum TO lt_acnum.
      CLEAR: ls_acnum.
    ENDLOOP.

    CALL FUNCTION 'ZFMAC_READ'
      EXPORTING
        it_acnum       = lt_acnum
      IMPORTING
        et_achdr       = lt_achdr
        et_acitm       = lt_acitm
      EXCEPTIONS
        no_data_exists = 1
        OTHERS         = 2.

    LOOP AT lt_acitm INTO DATA(ls_acitm).
      MOVE-CORRESPONDING ls_acitm TO ls_entityset.

      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
        EXPORTING
          input  = ls_acitm-tplnr_fl
        IMPORTING
          output = ls_entityset-talhao.

      SHIFT: ls_entityset-adqpl LEFT DELETING LEADING space,
             ls_entityset-aarea LEFT DELETING LEADING space.

      ls_entityset-tplnr_fl = ls_acitm-tplnr_fl.
      APPEND ls_entityset TO et_entityset.
      CLEAR: ls_entityset.
    ENDLOOP.

    SORT et_entityset BY talhao ASCENDING.

  ENDMETHOD.


METHOD getversionset_get_entityset.

  DATA: lv_versao_null TYPE zabs_del_ver_orc.

  DATA(lo_filter) = io_tech_request_context->get_filter( ).
  DATA(lt_filter) = lo_filter->get_filter_select_options( ).
  DATA(lv_filter_str) = lo_filter->get_filter_string( ).

*--Maps filter table lines to function module parameters
  LOOP AT lt_filter INTO DATA(ls_filter).
    CASE ls_filter-property.
      WHEN 'ACNUM'.
        DATA(lv_acnum) = ls_filter-select_options[ 1 ]-low.
      WHEN 'VERSAO'.
        DATA(lv_versao) = ls_filter-select_options[ 1 ]-low.
    ENDCASE.
  ENDLOOP.

*-- Fetch Budget's Versions
  SELECT acnum, versao
    FROM zabs_orcamento
    INTO TABLE @DATA(lt_versions)
   WHERE acnum  EQ @lv_acnum
     AND versao NE @lv_versao_null.

  SORT lt_versions BY versao.
  DELETE ADJACENT DUPLICATES FROM lt_versions COMPARING versao.

  LOOP AT lt_versions INTO DATA(ls_version).
    INSERT INITIAL LINE INTO TABLE et_entityset
      ASSIGNING FIELD-SYMBOL(<ls_entityset>).
    IF sy-subrc EQ 0.
      <ls_entityset>-acnum  = ls_version-acnum.
      <ls_entityset>-versao = ls_version-versao.
    ENDIF.
  ENDLOOP.

ENDMETHOD.


  METHOD getvolumecaldase_get_entityset.

*-- Local Types
    DATA: lt_filter     TYPE /iwbep/t_mgw_select_option,
          lrt_tplnr_fl  TYPE /iwbep/t_cod_select_options,
          lrt_tplnr_in  TYPE RANGE OF /agri/gltplnr_fl,
          lrt_matnr     TYPE RANGE OF matnr,
          lrt_maktx     TYPE RANGE OF tvarvc-low,
          ls_filter     TYPE /iwbep/s_mgw_select_option,
          lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
          lv_filter_str TYPE string,
          lv_matnr      TYPE matnr,
          lv_matnr_x    TYPE matnr,
          lv_rcnum      TYPE zfmrcnum,
          lv_werks      TYPE werks_d,
          lv_acnum      TYPE zfmacnum,
          lv_extwg      TYPE extwg,
          lv_matkl      TYPE matkl,
          lv_begda      TYPE begda,
          lv_endda      TYPE begda,
          lv_lgort      TYPE lgort_d,
          lv_string     TYPE string.

    lo_filter = io_tech_request_context->get_filter( ).
    lt_filter = lo_filter->get_filter_select_options( ).
    lv_filter_str = lo_filter->get_filter_string( ).

*--Maps filter table lines to function module parameters
    LOOP AT lt_filter INTO ls_filter.
      CASE ls_filter-property.
        WHEN 'RCNUM'.
          lv_rcnum = ls_filter-select_options[ 1 ]-low.
        WHEN 'MATNR'.
          lv_matnr = ls_filter-select_options[ 1 ]-low.
        WHEN 'BEGDA'.
          lv_begda = ls_filter-select_options[ 1 ]-low.
        WHEN 'ENDDA'.
          lv_endda = ls_filter-select_options[ 1 ]-low.
        WHEN 'WERKS'.
          lv_werks = ls_filter-select_options[ 1 ]-low.
        WHEN 'ACNUM'.
          lv_acnum = ls_filter-select_options[ 1 ]-low.
        WHEN 'EXTWG'.
          lv_extwg = ls_filter-select_options[ 1 ]-low.
        WHEN 'MATKL'.
          lv_matkl = ls_filter-select_options[ 1 ]-low.
        WHEN 'LGORT'.
          lv_lgort = ls_filter-select_options[ 1 ]-low.
        WHEN 'TPLNR_FL'.
          lo_filter->convert_select_option(
            EXPORTING
              is_select_option = ls_filter
            IMPORTING
              et_select_option = lrt_tplnr_fl ).
      ENDCASE.
    ENDLOOP.

    INSERT INITIAL LINE INTO TABLE lrt_maktx
      ASSIGNING FIELD-SYMBOL(<lrs_maktx>).
    IF sy-subrc EQ 0.
      lv_string = lv_matkl && '*'.
      <lrs_maktx>-sign = 'I'.
      <lrs_maktx>-option = 'CP'.
      <lrs_maktx>-low = lv_string.
    ENDIF.

    DELETE lrt_tplnr_fl WHERE low EQ 'null'.

    LOOP AT lrt_tplnr_fl INTO DATA(ls_tplnr_fl).
      INSERT INITIAL LINE INTO TABLE lrt_tplnr_in
        ASSIGNING FIELD-SYMBOL(<ls_tplnr_in>).
      IF sy-subrc EQ 0.
        <ls_tplnr_in> = 'IEQ'.
        CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
          EXPORTING
            input      = ls_tplnr_fl-low
          IMPORTING
            output     = <ls_tplnr_in>-low
          EXCEPTIONS
            not_found  = 1
            not_active = 2
            OTHERS     = 3.

        IF sy-subrc <> 0.
          <ls_tplnr_in>-low = ls_tplnr_fl-low.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF lv_acnum IS NOT INITIAL
    AND lv_matnr IS NOT INITIAL
    AND lrt_tplnr_fl[] IS NOT INITIAL.
      lv_matnr_x = 'TMAN' && lv_matnr+4(36).
      INSERT INITIAL LINE INTO TABLE lrt_matnr
        ASSIGNING FIELD-SYMBOL(<lrs_matnr>).
      IF sy-subrc EQ 0.
        <lrs_matnr> = 'IEQ'.
        <lrs_matnr>-low = lv_matnr_x.
      ENDIF.

      lv_matnr_x = 'TFOR' && lv_matnr+4(36).
      INSERT INITIAL LINE INTO TABLE lrt_matnr ASSIGNING <lrs_matnr>.
      IF sy-subrc EQ 0.
        <lrs_matnr> = 'IEQ'.
        <lrs_matnr>-low = lv_matnr_x.
      ENDIF.

      lv_matnr_x = 'TIMP' && lv_matnr+4(36).
      INSERT INITIAL LINE INTO TABLE lrt_matnr ASSIGNING <lrs_matnr>.
      IF sy-subrc EQ 0.
        <lrs_matnr> = 'IEQ'.
        <lrs_matnr>-low = lv_matnr_x.
      ENDIF.

      SELECT *
        INTO TABLE @DATA(lt_vol_calda)
        FROM zfmacvlcl
       WHERE acnum    EQ @lv_acnum
         AND matnr    IN @lrt_matnr[]
         AND tplnr_fl IN @lrt_tplnr_in[].
    ELSEIF lv_acnum IS NOT INITIAL.
      SELECT *
        INTO TABLE @lt_vol_calda
        FROM zfmacvlcl
       WHERE acnum    EQ @lv_acnum
         AND tplnr_fl IN @lrt_tplnr_fl[].
    ENDIF.

    DELETE lt_vol_calda WHERE matnr+0(4) NE 'TFOR'
                          AND matnr+0(4) NE 'TMAN'
                          AND matnr+0(4) NE 'TIMP'.

    LOOP AT lt_vol_calda INTO DATA(ls_vol_calda).
      INSERT INITIAL LINE INTO TABLE et_entityset
        ASSIGNING FIELD-SYMBOL(<ls_entityset>).
      IF sy-subrc EQ 0.
        CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
          EXPORTING
            input  = ls_vol_calda-tplnr_fl
          IMPORTING
            output = <ls_entityset>-talhao.
        MOVE-CORRESPONDING ls_vol_calda TO <ls_entityset>.
      ENDIF.
    ENDLOOP.

    SORT et_entityset BY talhao ASCENDING.

  ENDMETHOD.


  METHOD getweekdateset_get_entity.

    DATA: lv_begda TYPE begda,
          lv_endda TYPE begda,
          lv_month TYPE c LENGTH 2,
          lv_tabix TYPE sy-tabix,
          lv_descr TYPE string,
          lv_char  TYPE c LENGTH 15,
          lv_date  TYPE c LENGTH 10.

    DATA: lt_months TYPE TABLE OF t247,
          lt_dates  TYPE /scwm/tt_lm_dates,
          ls_dates  LIKE LINE OF lt_dates.

    READ TABLE it_key_tab INTO DATA(ls_key) INDEX 1.
    IF sy-subrc EQ 0.
      lv_begda = ls_key-value.
    ENDIF.

    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                      = lv_begda
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS                    = 2.

    IF sy-subrc <> 0.
      DATA(lv_invalid_date) = abap_true.
    ELSE.
      lv_invalid_date = abap_false.
    ENDIF.

    CHECK lv_invalid_date EQ abap_false.

    CALL FUNCTION 'FIAPPL_ADD_DAYS_TO_DATE'
      EXPORTING
        i_date      = lv_begda
        i_days      = '5'
        signum      = '+'
      IMPORTING
        e_calc_date = lv_endda.

    lv_endda = lv_endda - 1.

    CALL FUNCTION 'MONTH_NAMES_GET'
      EXPORTING
        language              = sy-langu
      TABLES
        month_names           = lt_months
      EXCEPTIONS
        month_names_not_found = 1
        OTHERS                = 2.

    CALL FUNCTION '/SCWM/DATES_BETWEEN_TWO_DATES'
      EXPORTING
        iv_begda = lv_begda
        iv_endda = lv_endda
      IMPORTING
        et_dates = lt_dates.

    LOOP AT lt_dates INTO ls_dates.
      lv_tabix = sy-tabix.
      lv_month = ls_dates+4(2).
      READ TABLE lt_months INTO DATA(ls_months) WITH KEY mnr = lv_month.
      IF sy-subrc EQ 0.
        lv_descr = ls_dates+6(2) && '.' && ls_months-ltx(3).
      ENDIF.

      CASE lv_tabix.
        WHEN '01'.
          er_entity-d1 = lv_descr.
        WHEN '02'.
          er_entity-d2 = lv_descr.
        WHEN '03'.
          er_entity-d3 = lv_descr.
        WHEN '04'.
          er_entity-d4 = lv_descr.
        WHEN '05'.
          er_entity-d5 = lv_descr.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

    er_entity-begda = lv_begda.

  ENDMETHOD.


  METHOD getweekdateset_get_entityset.

    DATA: lt_months    TYPE TABLE OF t247,
          lt_dates     TYPE /scwm/tt_lm_dates,
          ls_dates     LIKE LINE OF lt_dates,
          ls_entityset LIKE LINE OF et_entityset,
          lv_begda     TYPE begda,
          lv_endda     TYPE begda,
          lv_month     TYPE c LENGTH 2,
          lv_tabix     TYPE sy-tabix,
          lv_descr     TYPE string,
          lv_char      TYPE c LENGTH 15,
          lv_date      TYPE c LENGTH 10.

    LOOP AT it_filter_select_options INTO DATA(lwa_filter).
      CASE lwa_filter-property.
        WHEN 'Begda'.
          lv_begda = lwa_filter-select_options[ 1 ]-low.
      ENDCASE.
    ENDLOOP.

    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                      = lv_begda
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS                    = 2.

    IF sy-subrc <> 0.
      DATA(lv_invalid_date) = abap_true.
    ELSE.
      lv_invalid_date = abap_false.
    ENDIF.

    CHECK lv_invalid_date EQ abap_false.

    CALL FUNCTION 'FIAPPL_ADD_DAYS_TO_DATE'
      EXPORTING
        i_date      = lv_begda
        i_days      = '5'
        signum      = '+'
      IMPORTING
        e_calc_date = lv_endda.

    lv_endda = lv_endda - 1.

    CALL FUNCTION 'MONTH_NAMES_GET'
      EXPORTING
        language              = sy-langu
      TABLES
        month_names           = lt_months
      EXCEPTIONS
        month_names_not_found = 1
        OTHERS                = 2.

    IF sy-subrc EQ 0.
      CALL FUNCTION '/SCWM/DATES_BETWEEN_TWO_DATES'
        EXPORTING
          iv_begda = lv_begda
          iv_endda = lv_endda
        IMPORTING
          et_dates = lt_dates.

      SORT lt_months BY mnr.

      LOOP AT lt_dates INTO ls_dates.
        lv_tabix = sy-tabix.
        lv_month = ls_dates+4(2).

        CLEAR ls_entityset.
        READ TABLE lt_months INTO DATA(ls_months)
          WITH KEY mnr = lv_month BINARY SEARCH.

        IF sy-subrc EQ 0.
          lv_descr = ls_dates+6(2) && '.' && ls_months-ltx(3).

          CASE lv_tabix.
            WHEN '01'.
              ls_entityset-d1 = lv_descr.
            WHEN '02'.
              ls_entityset-d2 = lv_descr.
            WHEN '03'.
              ls_entityset-d3 = lv_descr.
            WHEN '04'.
              ls_entityset-d4 = lv_descr.
            WHEN '05'.
              ls_entityset-d5 = lv_descr.
            WHEN OTHERS.
          ENDCASE.

          ls_entityset-begda = lv_begda.
          APPEND ls_entityset TO et_entityset.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  method GETWORKCENTERPLA_CREATE_ENTITY.

    DATA: lt_work_center TYPE TABLE OF zabs_work_center,
          ls_work_center like LINE OF lt_work_center.

    io_data_provider->read_entry_data( IMPORTING es_data = er_entity ).

    MOVE-CORRESPONDING er_entity TO ls_work_center.
    ls_work_center-period = er_entity-period+3(4) && er_entity-period(2).

    MODIFY zabs_work_center FROM ls_work_center.

  endmethod.


  METHOD getworkcenterpla_get_entityset.

    TYPES: BEGIN OF ly_dias,
             werks  TYPE werks_d,
             arbpl  TYPE arbpl,
             begda  TYPE begda,
             endda  TYPE endda,
             period TYPE period,
             dias   TYPE i,
             cap_d  TYPE zfmplapr,
           END OF ly_dias.

    DATA: lt_dias      TYPE TABLE OF ly_dias,
          lt_dates     TYPE /scwm/tt_lm_dates,
          lrt_periodo  TYPE RANGE OF spmon,
          lrt_matnr    TYPE RANGE OF matnr,
          lrt_mtart    TYPE /iwbep/t_cod_select_options,
          lrs_mtart    LIKE LINE OF lrt_mtart,
          lrs_matnr    LIKE LINE OF lrt_matnr,
          ls_entityset LIKE LINE OF et_entityset,
          lv_total     TYPE zfmacren,
          lv_media     TYPE zfmacren,
          lv_tot       TYPE zfmacqtb,
          lv_extwg     TYPE  extwg,
          lv_matkl     TYPE matkl,
          lv_acnum     TYPE string,
          lv_begda     TYPE begda,
          lv_begda_ano TYPE begda,
          lv_endda_ano TYPE endda,
          lv_endda     TYPE endda,
          lv_periodo   TYPE c LENGTH 6,
          lv_string1   TYPE string,
          lv_string2   TYPE string,
          lv_werks     TYPE werks_d,
          lv_cap_prog  TYPE zfmacqtb,
          lv_period    TYPE char6,
          lv_days      TYPE i,
          lv_matnr     TYPE string,
          lv_ajahr     TYPE ajahr.

    LOOP AT it_filter_select_options INTO DATA(ls_filters).
      CASE ls_filters-property.
        WHEN 'Werks'.
          lv_werks = ls_filters-select_options[ 1 ]-low.
        WHEN 'Acnum'.
          lv_acnum = ls_filters-select_options[ 1 ]-low.
        WHEN 'Extwg'.
          lv_extwg = ls_filters-select_options[ 1 ]-low.
        WHEN 'Matkl'.
          lv_matkl = ls_filters-select_options[ 1 ]-low.
        WHEN 'Begda'.
          lv_begda = ls_filters-select_options[ 1 ]-low.
        WHEN 'Endda'.
          lv_endda = ls_filters-select_options[ 1 ]-low.
        WHEN 'Ajahr'.
          lv_ajahr = ls_filters-select_options[ 1 ]-low.
      ENDCASE.
    ENDLOOP.

    SELECT *
      INTO TABLE @DATA(lt_area)
      FROM zfmachdr
     WHERE acnum EQ @lv_acnum
       AND ajahr EQ @lv_ajahr.

    SORT lt_area BY acnum ajahr.
    READ TABLE lt_area INTO DATA(ls_area) INDEX 1.
    IF sy-subrc EQ 0.
      lv_begda_ano = ls_area-datab.
      lv_endda_ano = ls_area-datbi.
    ENDIF.

    IF lv_endda IS NOT INITIAL.
      lv_begda = lv_endda.
      CLEAR lv_endda.
      CALL FUNCTION '/CPD/ADD_MONTH_TO_DATE'
        EXPORTING
          months  = '4'
          olddate = lv_begda
        IMPORTING
          newdate = lv_endda.
    ELSE.
      IF lv_begda IS NOT INITIAL.
        lv_begda = lv_begda.
        CALL FUNCTION 'HR_CL_SUBTRACT_MONTH_TO_DATE'
          EXPORTING
            p_date       = lv_begda
            p_backmonths = '4'
          IMPORTING
            p_newdate    = lv_begda.

        CALL FUNCTION '/CPD/ADD_MONTH_TO_DATE'
          EXPORTING
            months  = '4'
            olddate = lv_begda
          IMPORTING
            newdate = lv_endda.
      ELSE.
        lv_begda = lv_begda_ano.
        CALL FUNCTION '/CPD/ADD_MONTH_TO_DATE'
          EXPORTING
            months  = '4'
            olddate = lv_begda
          IMPORTING
            newdate = lv_endda.
      ENDIF.
    ENDIF.

    IF lv_begda LT lv_begda_ano.
      lv_begda = lv_begda_ano.
    ENDIF.

    IF lv_endda GT lv_endda_ano.
      lv_endda = lv_endda_ano.
    ENDIF.

    CALL FUNCTION '/SCWM/DATES_BETWEEN_TWO_DATES'
      EXPORTING
        iv_begda = lv_begda
        iv_endda = lv_endda
      IMPORTING
        et_dates = lt_dates.

    LOOP AT lt_dates INTO DATA(ls_dates).
      IF lv_period NE ls_dates(6).
        INSERT INITIAL LINE INTO TABLE lrt_periodo
          ASSIGNING FIELD-SYMBOL(<ls_periodo>).
        IF sy-subrc EQ 0.
          <ls_periodo> = 'IEQ'.
          <ls_periodo>-low = lv_period = ls_dates(6).
        ENDIF.
      ENDIF.
    ENDLOOP.

    SORT lrt_periodo BY low.
    DELETE ADJACENT DUPLICATES FROM lrt_periodo COMPARING low.

    SELECT *
      INTO TABLE @DATA(lt_work_center)
      FROM zabs_work_center
     WHERE acnum  EQ @lv_acnum
       AND extwg  EQ @lv_extwg
       AND period IN @lrt_periodo[].

    IF lv_matkl IS NOT INITIAL.
      DELETE lt_work_center WHERE matkl NE lv_matkl.
    ENDIF.

    LOOP AT lrt_periodo INTO DATA(ls_periodo).
      CLEAR: lv_endda, lv_days.
      INSERT INITIAL LINE INTO TABLE lt_dias
        ASSIGNING FIELD-SYMBOL(<ls_dia>).
      IF sy-subrc EQ 0.
        <ls_dia>-werks  = lv_werks.
        <ls_dia>-period = ls_periodo-low.
        <ls_dia>-begda  = ls_periodo-low && '01'.

        CALL FUNCTION 'HR_HR_LAST_DAY_OF_MONTH'
          EXPORTING
            day_in            = <ls_dia>-begda
          IMPORTING
            last_day_of_month = lv_endda.

        CALL FUNCTION 'HR_RO_WORKDAYS_IN_INTERVAL'
          EXPORTING
            begda   = <ls_dia>-begda
            endda   = lv_endda
            mofid   = 'BR'
          CHANGING
            wrkdays = lv_days.

        <ls_dia>-endda = lv_endda.
        <ls_dia>-dias  = lv_days.
      ENDIF.
    ENDLOOP.

    SELECT *
      INTO TABLE @DATA(lt_cap)
      FROM zfmacwork_shift
     WHERE werks  EQ @lv_werks
       AND period IN @lrt_periodo[].

    lrt_mtart = zcl_agri_utilities=>zm_get_tvarvc( iv_name = 'ZAGRI_PROCESSO' ).
    IF lrt_mtart[] IS NOT INITIAL.
      lrs_mtart-sign = 'I'.
      lrs_mtart-option = 'EQ'.
      MODIFY lrt_mtart FROM lrs_mtart TRANSPORTING sign option WHERE low <> ''.
    ENDIF.

    CASE lv_extwg.
      WHEN 'GIM'.
        lrs_matnr = 'ICP'.
        lrs_matnr-low = 'TIM*'.
        APPEND lrs_matnr TO lrt_matnr.
      WHEN 'GTC'.
        lrs_matnr = 'ICP'.
        lrs_matnr-low = 'TMAN*'.
        APPEND lrs_matnr TO lrt_matnr.
        lrs_matnr-low = 'TFOR*'.
        APPEND lrs_matnr TO lrt_matnr.
      WHEN 'GCO'.
        lrs_matnr = 'ICP'.
        lrs_matnr-low = 'TCOL*'.
        APPEND lrs_matnr TO lrt_matnr.
      WHEN 'GIR'.
        lrs_matnr = 'ICP'.
        lrs_matnr-low = 'TMAN*'.
        APPEND lrs_matnr TO lrt_matnr.
    ENDCASE.

    SELECT matnr, matkl
      INTO TABLE @DATA(lt_matkl)
      FROM mara
     WHERE matnr IN @lrt_matnr[]
       AND mtart IN @lrt_mtart[]
       AND extwg EQ @lv_extwg
       AND matkl NE @space.

    IF lv_matkl IS NOT INITIAL.
      DELETE lt_matkl WHERE matkl NE lv_matkl.
    ENDIF.

    IF lt_matkl[] IS NOT INITIAL.
      SORT lt_matkl BY matkl.

      SELECT *
        INTO TABLE @DATA(lt_mapl)
        FROM mapl
        FOR ALL ENTRIES IN @lt_matkl
       WHERE matnr EQ @lt_matkl-matnr
         AND werks EQ @lv_werks.

      IF sy-subrc EQ 0.
        SELECT DISTINCT o~plnty, o~plnnr, plnal,
                        o~versn, plnfl, o~plnkn,
                        o~arbid, o~ltxa1
          FROM plpo AS o
          INNER JOIN plas AS s
          ON o~plnty = s~plnty
          AND o~plnnr = s~plnnr
          AND o~plnkn = s~plnkn
          AND o~versn = s~versn
          INTO TABLE @DATA(lt_plpo)
          FOR ALL ENTRIES IN @lt_mapl
         WHERE o~plnty = @lt_mapl-plnty
           AND o~plnnr = @lt_mapl-plnnr
           AND o~werks = @lv_werks
           AND s~loekz = @abap_false.

        IF sy-subrc EQ 0.
          SELECT *
            INTO TABLE @DATA(lt_crhd)
            FROM crhd
            FOR ALL ENTRIES IN @lt_plpo
           WHERE objid EQ @lt_plpo-arbid.

          IF sy-subrc EQ 0.
            SORT lt_crhd BY objid.

            SELECT *
              INTO TABLE @DATA(lt_crtx)
              FROM crtx
              FOR ALL ENTRIES IN @lt_crhd
             WHERE objid EQ @lt_crhd-objid.

            IF sy-subrc EQ 0.
              LOOP AT lt_crtx INTO DATA(ls_crtx).
                LOOP AT lt_dias INTO DATA(ls_dia).
                  DATA(lv_tabix) = sy-tabix.
                  READ TABLE lt_crhd INTO DATA(ls_crhd)
                    WITH KEY objid = ls_crtx-objid BINARY SEARCH.
                  IF sy-subrc EQ 0.
                    CLEAR lv_tot.

                    LOOP AT lt_cap INTO DATA(ls_cap)
                      WHERE werks EQ ls_crhd-werks
                        AND arbpl EQ ls_crhd-arbpl
                        AND period EQ ls_dia-period.
                      lv_tot = lv_tot + ls_cap-accpd.
                    ENDLOOP.

                    lv_tot = lv_tot * ls_dia-dias.
                    ls_entityset-ltxa1 = ls_crtx-ktext_up.
                    ls_entityset-arbpl = ls_crhd-arbpl.
                    ls_entityset-acnum = lv_acnum.


                    CLEAR: lv_cap_prog.
*-- Capacidade Programada
                    LOOP AT lt_work_center INTO DATA(ls_work_center)
                      WHERE arbpl  EQ ls_crhd-arbpl
                        AND period EQ ls_dia-period.
                      lv_cap_prog = lv_cap_prog + ls_work_center-aarea.
                    ENDLOOP.

                    lv_string1 = lv_cap_prog. lv_string2 = lv_tot.
                    CASE lv_tabix.
                      WHEN '1'.
                        CONCATENATE lv_string1 '/' lv_string2 INTO ls_entityset-d1 SEPARATED BY space.
                      WHEN '2'.
                        CONCATENATE lv_string1 '/' lv_string2 INTO ls_entityset-d2 SEPARATED BY space.
                      WHEN '3'.
                        CONCATENATE lv_string1 '/' lv_string2 INTO ls_entityset-d3 SEPARATED BY space.
                      WHEN '4'.
                        CONCATENATE lv_string1 '/' lv_string2 INTO ls_entityset-d4 SEPARATED BY space.
                      WHEN '5'.
                        CONCATENATE lv_string1 '/' lv_string2 INTO ls_entityset-d5 SEPARATED BY space.
                      WHEN OTHERS.
                    ENDCASE.
                    CLEAR: lv_string1, lv_string2.
                  ENDIF.
                ENDLOOP.
                ls_entityset-extwg = lv_extwg.
                ls_entityset-matkl = lv_matkl.
                APPEND ls_entityset TO et_entityset.
                CLEAR ls_entityset.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF lv_matkl IS NOT INITIAL.
      SELECT *
        INTO TABLE @DATA(lt_mara)
        FROM mara
       WHERE mtart IN @lrt_mtart[]
         AND matkl EQ @lv_matkl
         AND extwg EQ @lv_extwg.

      IF sy-subrc EQ 0.
        SELECT *
          INTO TABLE @DATA(lt_zfmacvlcl)
          FROM zfmacvlcl
          FOR ALL ENTRIES IN @lt_mara
         WHERE acnum EQ @lv_acnum
           AND matnr EQ @lt_mara-matnr.
      ENDIF.

      LOOP AT lt_zfmacvlcl INTO DATA(ls_zfmacvlcl).
        lv_total = lv_total + ls_zfmacvlcl-acren.
      ENDLOOP.

      DATA(lv_lines) = lines( lt_zfmacvlcl ).
      IF lv_lines IS NOT INITIAL.
        lv_media = lv_total / lv_lines.

        READ TABLE et_entityset ASSIGNING FIELD-SYMBOL(<ls_entityset>) INDEX 1.
        IF sy-subrc EQ 0.
          <ls_entityset>-mediarend = lv_media.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD getworkcenterset_create_entity.

    DATA: lt_work_center TYPE TABLE OF zabs_wcenter_sem,
          ls_work_center LIKE LINE OF lt_work_center.

    DATA: lt_dates TYPE /scwm/tt_lm_dates,
          ls_dates LIKE LINE OF lt_dates.

    DATA: lv_begda TYPE begda,
          lv_tabix TYPE sy-tabix,
          lv_endda TYPE endda.

    io_data_provider->read_entry_data( IMPORTING es_data = er_entity ).

    lv_begda = er_entity-data.

    CALL FUNCTION 'FIAPPL_ADD_DAYS_TO_DATE'
      EXPORTING
        i_date      = lv_begda
        i_days      = '4'
        signum      = '+'
      IMPORTING
        e_calc_date = lv_endda.

    CALL FUNCTION '/SCWM/DATES_BETWEEN_TWO_DATES'
      EXPORTING
        iv_begda = lv_begda
        iv_endda = lv_endda
      IMPORTING
        et_dates = lt_dates.

    MOVE-CORRESPONDING er_entity TO ls_work_center.

*-- BOC-T_T.KONNO-07.02.20
*    LOOP AT lt_dates INTO ls_dates.
*      lv_tabix = sy-tabix.
*
*      CASE lv_tabix.
*        WHEN '1'.
*          IF er_entity-d1 GT 0.
*            ls_work_center-aarea = er_entity-d1.
*            ls_work_center-data  = ls_dates.
*          ENDIF.
*        WHEN '2'.
*         IF er_entity-d2 GT 0.
*            ls_work_center-aarea = er_entity-d2.
*            ls_work_center-data  = ls_dates.
*          ENDIF.
*        WHEN '3'.
*          IF er_entity-d3 GT 0.
*            ls_work_center-aarea = er_entity-d3.
*            ls_work_center-data  = ls_dates.
*          ENDIF.
*        WHEN '4'.
*          IF er_entity-d4 GT 0.
*            ls_work_center-aarea = er_entity-d4.
*            ls_work_center-data  = ls_dates.
*          ENDIF.
*        WHEN '5'.
*          IF er_entity-d5 GT 0.
*            ls_work_center-aarea = er_entity-d5.
*            ls_work_center-data  = ls_dates.
*          ENDIF.
*        WHEN OTHERS.
*      ENDCASE.
*    ENDLOOP.
    LOOP AT lt_dates INTO ls_dates.
      lv_tabix = sy-tabix.
      CASE lv_tabix.
        WHEN '1'.
          IF er_entity-field = lv_tabix.
            ls_work_center-aarea = er_entity-d1.
            ls_work_center-data  = ls_dates.
          ENDIF.
        WHEN '2'.
          IF er_entity-field = lv_tabix.
            ls_work_center-aarea = er_entity-d2.
            ls_work_center-data  = ls_dates.
          ENDIF.
        WHEN '3'.
          IF er_entity-field = lv_tabix.
            ls_work_center-aarea = er_entity-d3.
            ls_work_center-data  = ls_dates.
          ENDIF.
        WHEN '4'.
          IF er_entity-field = lv_tabix.
            ls_work_center-aarea = er_entity-d4.
            ls_work_center-data  = ls_dates.
          ENDIF.
        WHEN '5'.
          IF er_entity-field = lv_tabix.
            ls_work_center-aarea = er_entity-d5.
            ls_work_center-data  = ls_dates.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.
*-- EOC-T_T.KONNO-07.02.20

    MODIFY zabs_wcenter_sem FROM ls_work_center.

  ENDMETHOD.


  METHOD getworkcenterset_get_entityset.

    TYPES: BEGIN OF ly_matkl,
             matnr   TYPE matnr,
             matkl   TYPE matkl,
             wgbez   TYPE wgbez,
             wgbez60 TYPE wgbez60,
           END OF ly_matkl.

    DATA: lt_plpo      TYPE TABLE OF plpo,
          lt_mapl      TYPE TABLE OF mapl,
          lt_crhd      TYPE TABLE OF crhd,
          lt_crtx      TYPE TABLE OF crtx,
          lt_fmfpcom   TYPE TABLE OF /agri/s_fmfpcom,
          lt_fmfpitm   TYPE TABLE OF /agri/s_fmfpitm,
          lt_makt      TYPE TABLE OF makt,
          lt_zfmacvlcl TYPE TABLE OF zfmacvlcl,
          lt_matkl     TYPE TABLE OF ly_matkl,
          lt_t023t     TYPE TABLE OF t023t,
          lt_months    TYPE TABLE OF t247,
          lt_dates     TYPE /scwm/tt_lm_dates,
          lt_cap       TYPE TABLE OF zfmacwork_shift,
          lr_matnr     TYPE RANGE OF matnr,
          lr_dates     TYPE RANGE OF tvarvc-low,
          lr_mtart     TYPE /iwbep/t_cod_select_options,
          lr_werks     TYPE /iwbep/t_cod_select_options,
          lr_acnum     TYPE /iwbep/t_cod_select_options,
          lr_talhao    TYPE /iwbep/t_cod_select_options,
          ls_mtart     LIKE LINE OF lr_mtart,
          ls_matnr     LIKE LINE OF lr_matnr,
          ls_dates     LIKE LINE OF lt_dates,
          ls_entityset LIKE LINE OF et_entityset,
          ls_date      LIKE LINE OF lr_dates,
          lv_tot       TYPE zfmacqtb,
          lv_extwg     TYPE extwg,
          lv_matkl     TYPE matkl,
          lv_acnum     TYPE string,
          lv_string1   TYPE string,
          lv_string2   TYPE string,
          lv_char10    TYPE char10,
          lv_matnr     TYPE string,
          lv_begda     TYPE begda,
          lv_cap_prog  TYPE zfmacqtb,
          lv_tabix     TYPE sy-tabix,
          lv_endda     TYPE endda,
          lv_periodo   TYPE c LENGTH 6,
          lv_werks     TYPE werks,
          lv_days      TYPE i,
          lv_plan      TYPE c.

    LOOP AT it_filter_select_options INTO DATA(ls_filters).
      CASE ls_filters-property.
        WHEN 'Werks'.
          lv_werks = ls_filters-select_options[ 1 ]-low.
        WHEN 'Acnum'.
          lv_acnum = ls_filters-select_options[ 1 ]-low.
        WHEN 'idTalhao'.
          lr_talhao = ls_filters-select_options.
        WHEN 'Extwg'.
          lv_extwg = ls_filters-select_options[ 1 ]-low.
        WHEN 'Matkl'.
          lv_matkl = ls_filters-select_options[ 1 ]-low.
        WHEN 'Begda'.
          lv_char10 = ls_filters-select_options[ 1 ]-low.
        WHEN 'Planejamento'.
          lv_plan = ls_filters-select_options[ 1 ]-low.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

    IF lv_char10 IS NOT INITIAL.
*      IF lv_char10+2(1) EQ '.'.
*        lv_begda = lv_char10+6(4) && lv_char10+3(2) && lv_char10(2).
*      ELSE.
*        lv_begda = lv_char10.
*      ENDIF.
      lv_begda = lv_char10.
    ELSE.
      lv_begda = sy-datum.
    ENDIF.

    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                      = lv_begda
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS                    = 2.

    IF sy-subrc <> 0.
      DATA(lv_invalid_date) = abap_true.
    ELSE.
      lv_invalid_date = abap_false.
    ENDIF.

    CHECK lv_invalid_date EQ abap_false.

    CALL FUNCTION 'FIAPPL_ADD_DAYS_TO_DATE'
      EXPORTING
        i_date      = lv_begda
        i_days      = '5'
        signum      = '+'
      IMPORTING
        e_calc_date = lv_endda.

    lv_endda = lv_endda - 1.

    CALL FUNCTION '/SCWM/DATES_BETWEEN_TWO_DATES'
      EXPORTING
        iv_begda = lv_begda
        iv_endda = lv_endda
      IMPORTING
        et_dates = lt_dates.

    LOOP AT lt_dates INTO ls_dates.
      ls_date = 'IEQ'.
      ls_date-low = ls_dates.
      APPEND ls_date TO lr_dates.
    ENDLOOP.

    SELECT *
      INTO TABLE @DATA(lt_work_center)
      FROM zabs_wcenter_sem
      WHERE acnum EQ @lv_acnum
        AND extwg EQ @lv_extwg
        AND data  IN @lr_dates.

    IF lv_matkl IS NOT INITIAL.
      DELETE lt_work_center WHERE matkl NE lv_matkl.
    ENDIF.

    SELECT *
      INTO TABLE lt_cap
      FROM zfmacwork_shift
     WHERE werks EQ lv_werks
       AND period EQ lv_begda(6).
    IF sy-subrc EQ 0.
      lv_periodo = lv_begda(6).
    ENDIF.

    CALL FUNCTION 'HR_RO_WORKDAYS_IN_INTERVAL'
      EXPORTING
        begda   = lv_begda
        endda   = lv_endda
        mofid   = 'BR'
      CHANGING
        wrkdays = lv_days.

    lr_mtart = zcl_agri_utilities=>zm_get_tvarvc( iv_name = 'ZAGRI_PROCESSO' ).
    ls_mtart-sign = 'I'.
    ls_mtart-option = 'EQ'.
    MODIFY lr_mtart FROM ls_mtart TRANSPORTING sign option WHERE low <> ''.

    ls_matnr = 'ICP'.
    CASE lv_extwg.
      WHEN 'GIM'.
        ls_matnr-low = 'TIM*'.
        APPEND ls_matnr TO lr_matnr.
      WHEN 'GTC'.
        ls_matnr-low = 'TMAN*'.
        APPEND ls_matnr TO lr_matnr.
        ls_matnr-low = 'TFOR*'.
        APPEND ls_matnr TO lr_matnr.
      WHEN 'GCO'.
        ls_matnr-low = 'TCOL*'.
        APPEND ls_matnr TO lr_matnr.
      WHEN 'GIR'.
        ls_matnr-low = 'TMAN*'.
        APPEND ls_matnr TO lr_matnr.
    ENDCASE.

    SELECT matnr matkl
      INTO CORRESPONDING FIELDS OF TABLE lt_matkl
      FROM mara
      WHERE matnr IN lr_matnr
        AND mtart IN lr_mtart
        AND extwg EQ lv_extwg
        AND matkl NE space.

    IF lv_matkl IS NOT INITIAL.
      DELETE lt_matkl WHERE matkl NE lv_matkl.
    ENDIF.

    IF lt_matkl[] IS NOT INITIAL.
      SORT lt_matkl BY matkl.

      SELECT *
        INTO TABLE lt_mapl
        FROM mapl
        FOR ALL ENTRIES IN lt_matkl
       WHERE matnr EQ lt_matkl-matnr
         AND werks EQ lv_werks.

      IF sy-subrc EQ 0.
        SELECT DISTINCT o~plnty, o~plnnr, plnal, o~versn,
                        plnfl, o~plnkn, o~arbid, o~ltxa1
          FROM plpo AS o INNER JOIN plas AS s
            ON o~plnty = s~plnty AND
               o~plnnr = s~plnnr AND
               o~plnkn = s~plnkn AND
               o~versn = s~versn
            INTO CORRESPONDING FIELDS OF TABLE @lt_plpo
            FOR ALL ENTRIES IN @lt_mapl
           WHERE o~plnty = @lt_mapl-plnty
             AND o~plnnr = @lt_mapl-plnnr
             AND s~loekz = @abap_false
             AND o~werks = @lv_werks.

        IF sy-subrc EQ 0.
          SELECT *
            INTO TABLE lt_crhd
            FROM crhd
            FOR ALL ENTRIES IN lt_plpo
           WHERE objid EQ lt_plpo-arbid.

          IF sy-subrc EQ 0.
            SELECT *
              INTO TABLE lt_crtx
              FROM crtx
              FOR ALL ENTRIES IN lt_crhd
             WHERE objid EQ lt_crhd-objid.

            IF sy-subrc EQ 0.
              LOOP AT lt_crtx INTO DATA(ls_crtx).
                READ TABLE lt_crhd INTO DATA(ls_crhd) WITH KEY objid = ls_crtx-objid.
                IF sy-subrc EQ 0.
                  LOOP AT lt_cap INTO DATA(ls_cap) WHERE werks EQ ls_crhd-werks AND
                                                         arbpl EQ ls_crhd-arbpl AND
                                                         period EQ lv_periodo.
                    lv_tot = lv_tot + ls_cap-accpd.
                  ENDLOOP.
                  IF lv_plan IS NOT INITIAL.
                    lv_tot = lv_tot * lv_days.
                  ENDIF.

                  LOOP AT lt_dates INTO ls_dates.
                    lv_tabix = sy-tabix.
                    CLEAR: lv_cap_prog.
                    " CALCULAR CAPACIDADE PROGRAMADA
                    LOOP AT lt_work_center INTO DATA(ls_work_center) WHERE arbpl EQ ls_crhd-arbpl AND
                                                                           data  EQ ls_dates.
                      lv_cap_prog = lv_cap_prog + ls_work_center-aarea.
                    ENDLOOP.

                    CASE lv_tabix.
                      WHEN '1'.
                        lv_string1 = lv_cap_prog. lv_string2 = lv_tot.
                        CONCATENATE lv_string1 '/' lv_string2 INTO ls_entityset-d1 SEPARATED BY space.
                        CLEAR: lv_string1, lv_string2.
                      WHEN '2'.
                        lv_string1 = lv_cap_prog. lv_string2 = lv_tot.
                        CONCATENATE lv_string1 '/' lv_string2 INTO ls_entityset-d2 SEPARATED BY space.
                        CLEAR: lv_string1, lv_string2.
                      WHEN '3'.
                        lv_string1 = lv_cap_prog. lv_string2 = lv_tot.
                        CONCATENATE lv_string1 '/' lv_string2 INTO ls_entityset-d3 SEPARATED BY space.
                        CLEAR: lv_string1, lv_string2.
                      WHEN '4'.
                        lv_string1 = lv_cap_prog. lv_string2 = lv_tot.
                        CONCATENATE lv_string1 '/' lv_string2 INTO ls_entityset-d4 SEPARATED BY space.
                        CLEAR: lv_string1, lv_string2.
                      WHEN '5'.
                        lv_string1 = lv_cap_prog. lv_string2 = lv_tot.
                        CONCATENATE lv_string1 '/' lv_string2 INTO ls_entityset-d5 SEPARATED BY space.
                        CLEAR: lv_string1, lv_string2.
                      WHEN OTHERS.
                    ENDCASE.
                  ENDLOOP.

                  ls_entityset-ltxa1 = ls_crtx-ktext_up.
                  ls_entityset-arbpl = ls_crhd-arbpl.
                  ls_entityset-acnum = lv_acnum.
                  ls_entityset-extwg = lv_extwg.
                  ls_entityset-matkl = lv_matkl.
                  APPEND ls_entityset TO et_entityset.
                  CLEAR: lv_tot, ls_entityset.
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD getyearmonthsset_get_entityset.

    TYPES: BEGIN OF ty_period,
             period TYPE period,
           END OF ty_period.

    DATA: lt_period TYPE TABLE OF ty_period,
          lv_begda  TYPE begda,
          lv_endda  TYPE begda,
          lv_period TYPE char6,
          lv_tabixc TYPE char2,
          lv_date   TYPE c LENGTH 10,
          lv_field  TYPE fieldname,
          lt_dates  TYPE /scwm/tt_lm_dates.

    LOOP AT it_filter_select_options INTO DATA(ls_filter).
      CASE ls_filter-property.
        WHEN 'Begda'.
          lv_begda = ls_filter-select_options[ 1 ]-low.
        WHEN 'Endda'.
          lv_endda = ls_filter-select_options[ 1 ]-low.
      ENDCASE.
    ENDLOOP.

    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                      = lv_begda
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS                    = 2.

    IF sy-subrc <> 0.
      DATA(lv_invalid_date) = abap_true.
    ELSE.
      lv_invalid_date = abap_false.
    ENDIF.

    IF lv_invalid_date = abap_false.
      CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
        EXPORTING
          date                      = lv_endda
        EXCEPTIONS
          plausibility_check_failed = 1
          OTHERS                    = 2.

      IF sy-subrc <> 0.
        lv_invalid_date = abap_true.
      ELSE.
        lv_invalid_date = abap_false.
      ENDIF.
    ENDIF.

    IF lv_invalid_date EQ abap_false.
      CALL FUNCTION '/SCWM/DATES_BETWEEN_TWO_DATES'
        EXPORTING
          iv_begda = lv_begda
          iv_endda = lv_endda
        IMPORTING
          et_dates = lt_dates.

      LOOP AT lt_dates INTO DATA(ls_date).
        IF lv_period NE ls_date(6).
          INSERT INITIAL LINE INTO TABLE lt_period
           ASSIGNING FIELD-SYMBOL(<ls_period>).
          IF sy-subrc EQ 0.
            <ls_period>-period = lv_period = ls_date(6).
          ENDIF.
        ENDIF.
      ENDLOOP.

      INSERT INITIAL LINE INTO TABLE et_entityset
        ASSIGNING FIELD-SYMBOL(<ls_entityset>).
      IF sy-subrc EQ 0.
        LOOP AT lt_period INTO data(ls_period).
          DATA(lv_tabix) = sy-tabix.
          lv_tabixc = lv_tabix.
          CONCATENATE 'M' lv_tabixc INTO lv_field.
          ASSIGN COMPONENT lv_field OF STRUCTURE <ls_entityset>
            TO FIELD-SYMBOL(<lv_field>).
          IF sy-subrc EQ 0.
            <lv_field> = ls_period-period+4(2) && '.' && ls_period-period(4) .
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD headerareacultiv_get_entity.

    DATA: lt_acnum TYPE zt_fm_acnum,
          lt_achdr TYPE zt_fmachdr,
          lt_acitm TYPE zt_fmacitm.

    DATA: ls_acnum TYPE zsc_fm_acnum.

    READ TABLE it_key_tab INTO DATA(ls_key_tab) INDEX 1.
    IF sy-subrc EQ 0.
      ls_acnum-acnum = ls_key_tab-value.
      APPEND ls_acnum TO lt_acnum.
      CLEAR: ls_acnum.
    ENDIF.

    CALL FUNCTION 'ZFMAC_READ'
      EXPORTING
        it_acnum             = lt_acnum
      IMPORTING
        et_achdr             = lt_achdr
        et_acitm             = lt_acitm
      EXCEPTIONS
        no_data_exists       = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    READ TABLE lt_achdr INTO DATA(ls_achdr) INDEX 1.
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING ls_achdr TO er_entity.
    ENDIF.

  ENDMETHOD.


  METHOD headerareacultiv_get_entityset.

    DATA: lt_acnum  TYPE zt_fm_acnum,
          lt_achdr  TYPE zt_fmachdr,
          lt_acitm  TYPE zt_fmacitm,
          lrt_werks TYPE /iwbep/t_cod_select_options,
          lrt_acnum TYPE /iwbep/t_cod_select_options,
          lrt_ajahr TYPE /iwbep/t_cod_select_options.

    LOOP AT it_filter_select_options INTO DATA(ls_filters).
      CASE ls_filters-property.
        WHEN 'Werks'.
          lrt_werks = ls_filters-select_options.
        WHEN 'Acnum'.
          lrt_acnum = ls_filters-select_options.
        WHEN 'Ajahr'.
          lrt_ajahr = ls_filters-select_options.
      ENDCASE.
    ENDLOOP.

    CALL FUNCTION 'ZFMAC_READ'
      EXPORTING
        it_acnum       = lt_acnum
      IMPORTING
        et_achdr       = lt_achdr
        et_acitm       = lt_acitm
      EXCEPTIONS
        no_data_exists = 1
        OTHERS         = 2.

    LOOP AT lt_achdr INTO DATA(ls_achdr).
      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
        EXPORTING
          input    = ls_achdr-unabs
          language = sy-langu
        IMPORTING
          output   = ls_achdr-unabs.

      INSERT INITIAL LINE INTO TABLE et_entityset
      ASSIGNING FIELD-SYMBOL(<ls_entityset>).
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING ls_achdr TO <ls_entityset>.
      ENDIF.
    ENDLOOP.

    IF lrt_werks[] IS NOT INITIAL.
      DELETE et_entityset WHERE werks NOT IN lrt_werks[].
    ENDIF.

    IF lrt_acnum[] IS NOT INITIAL.
      DELETE et_entityset WHERE acnum NOT IN lrt_acnum[].
    ENDIF.

    IF lrt_ajahr[] IS NOT INITIAL.
      DELETE et_entityset WHERE ajahr NOT IN lrt_ajahr[].
    ENDIF.

  ENDMETHOD.


  METHOD setprodutosimila_create_entity.

*-- Local Declarations
    DATA: ls_entity    LIKE er_entity,
          ls_zfmrchis  TYPE zfmrchis,
          lv_matnr_ins TYPE matnr,
          lv_matnr_sim TYPE matnr,
          lv_terrain   TYPE /agri/gltplnr_fl.

    io_data_provider->read_entry_data( IMPORTING es_data = ls_entity ).

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = ls_entity-matnr_ins
      IMPORTING
        output       = lv_matnr_ins
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

    IF sy-subrc <> 0.
      lv_matnr_ins = ls_entity-matnr_ins.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = ls_entity-matnr_sim
      IMPORTING
        output       = lv_matnr_sim
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

    IF sy-subrc <> 0.
      lv_matnr_sim = ls_entity-matnr_sim.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
      EXPORTING
        input      = ls_entity-tplnr_fl
      IMPORTING
        output     = lv_terrain
      EXCEPTIONS
        not_found  = 1
        not_active = 2
        OTHERS     = 3.

    IF sy-subrc <> 0.
      lv_terrain = ls_entity-tplnr_fl.
    ENDIF.

    SELECT SINGLE *
      FROM zfmrchis
      INTO @ls_zfmrchis
     WHERE werks     EQ @ls_entity-werks
       AND acnum     EQ @ls_entity-acnum
       AND extwg     EQ @ls_entity-extwg
       AND matkl     EQ @ls_entity-matkl
       AND tplnr_fl  EQ @ls_entity-tplnr_fl
       AND data      EQ @ls_entity-data
       AND rcnum     EQ @ls_entity-rcnum
       AND matnr_ins EQ @lv_matnr_ins.

    IF sy-subrc EQ 0.
      ls_zfmrchis-matnr_sim = ls_entity-matnr_sim.
      ls_zfmrchis-lgort_sim = ls_entity-lgort_sim.
      ls_zfmrchis-rcdos_sim = ls_entity-rcdos_sim.
      ls_zfmrchis-aenam     = sy-uname.
      ls_zfmrchis-aedat     = sy-datum.
      ls_zfmrchis-aezet     = sy-uzeit.
      MODIFY zfmrchis FROM ls_zfmrchis.
    ENDIF.

  ENDMETHOD.


  METHOD setprogtalhaoset_create_entity.

    DATA: ls_prog_talhao    TYPE zfmprog_talhao,
          ls_prog_talhao_bd TYPE zfmprog_talhao,
          lv_terrain        TYPE /agri/gltplnr_fl,
          lv_aufnr          TYPE aufnr.

    io_data_provider->read_entry_data( IMPORTING es_data = er_entity ).

    SHIFT er_entity-aarea LEFT DELETING LEADING space.
    MOVE-CORRESPONDING er_entity TO ls_prog_talhao.
    ls_prog_talhao-data = er_entity-data.

    IF er_entity-loevm IS INITIAL.
      SELECT SINGLE *
        FROM zfmprog_talhao
        INTO ls_prog_talhao_bd
       WHERE acnum    = er_entity-acnum
         AND extwg    = er_entity-extwg
         AND matkl    = er_entity-matkl
         AND tplnr_fl = er_entity-tplnr_fl
         AND data     = er_entity-data
         AND aufnr    = er_entity-aufnr.
      IF sy-subrc EQ 0.
        ls_prog_talhao-aufnr = ls_prog_talhao_bd-aufnr.
        ls_prog_talhao-rcnum = ls_prog_talhao_bd-rcnum.
      ENDIF.
      MODIFY zfmprog_talhao FROM ls_prog_talhao.
    ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
        EXPORTING
          input      = ls_prog_talhao-tplnr_fl
        IMPORTING
          output     = lv_terrain
        EXCEPTIONS
          not_found  = 1
          not_active = 2
          OTHERS     = 3.

      SELECT SINGLE *
        FROM zfmprog_talhao
        INTO @DATA(ls_deleted)
       WHERE acnum    = @ls_prog_talhao-acnum
         AND extwg    = @ls_prog_talhao-extwg
         AND matkl    = @ls_prog_talhao-matkl
         AND tplnr_fl = @ls_prog_talhao-tplnr_fl
         AND data     = @ls_prog_talhao-data
         AND aufnr    = @lv_aufnr.
        IF sy-subrc EQ 0.
          DELETE FROM zfmrchis WHERE acnum    = ls_prog_talhao-acnum
                                 AND extwg    = ls_prog_talhao-extwg
                                 AND matkl    = ls_prog_talhao-matkl
                                 AND tplnr_fl = ls_prog_talhao-tplnr_fl
                                 AND data     = ls_prog_talhao-data
                                 AND rcnum    = ls_deleted-rcnum.
        ENDIF.

      DELETE FROM zfmprog_talhao WHERE acnum    = ls_prog_talhao-acnum
                                   AND extwg    = ls_prog_talhao-extwg
                                   AND matkl    = ls_prog_talhao-matkl
                                   AND tplnr_fl = ls_prog_talhao-tplnr_fl
                                   AND data     = ls_prog_talhao-data
                                   AND aufnr    = lv_aufnr.
      IF sy-subrc NE 0.
        SELECT SINGLE *
          FROM zfmprog_talhao
          INTO @ls_deleted
         WHERE acnum    = @ls_prog_talhao-acnum
           AND extwg    = @ls_prog_talhao-extwg
           AND matkl    = @ls_prog_talhao-matkl
           AND tplnr_fl = @lv_terrain
           AND data     = @ls_prog_talhao-data
           AND aufnr    = @lv_aufnr.
        IF sy-subrc EQ 0.
          DELETE FROM zfmrchis WHERE acnum    = ls_prog_talhao-acnum
                                 AND extwg    = ls_prog_talhao-extwg
                                 AND matkl    = ls_prog_talhao-matkl
                                 AND tplnr_fl = lv_terrain
                                 AND data     = ls_prog_talhao-data
                                 AND rcnum    = ls_deleted-rcnum.
        ENDIF.

        DELETE FROM zfmprog_talhao WHERE acnum    = ls_prog_talhao-acnum
                                     AND extwg    = ls_prog_talhao-extwg
                                     AND matkl    = ls_prog_talhao-matkl
                                     AND tplnr_fl = lv_terrain
                                     AND data     = ls_prog_talhao-data
                                     AND aufnr    = lv_aufnr.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD setprogtalhaoset_get_entityset.

    DATA: lt_prog_talhao TYPE TABLE OF zfmprog_talhao,
          ls_entityset   LIKE LINE OF et_entityset,
          lv_acnum       TYPE zfmacnum,
          lv_extwg       TYPE extwg,
          lv_begda       TYPE begda,
          lv_data        TYPE sy-datum,
          lv_endda       TYPE begda,
          lv_matkl       TYPE matkl,
          lv_days        TYPE i,
          lv_years       TYPE i.

    CONSTANTS: BEGIN OF c_syst_stat,
                 open TYPE j_istat VALUE 'I0001',
                 rel  TYPE j_istat VALUE 'I0002',
                 teco TYPE j_istat VALUE 'I0045',
               END OF c_syst_stat.

    LOOP AT it_filter_select_options INTO DATA(ls_filters).
      CASE ls_filters-property.
        WHEN 'Acnum'.
          lv_acnum = ls_filters-select_options[ 1 ]-low.
        WHEN 'Matkl'.
          lv_matkl = ls_filters-select_options[ 1 ]-low.
        WHEN 'Extwg'.
          lv_extwg = ls_filters-select_options[ 1 ]-low.
        WHEN 'Begda'.
          lv_begda = ls_filters-select_options[ 1 ]-low.
        WHEN 'Data'.
          lv_data = ls_filters-select_options[ 1 ]-low.
      ENDCASE.
    ENDLOOP.

    lv_days = 4.

    WAIT UP TO 2 SECONDS.

    IF lv_begda IS NOT INITIAL.
      CALL FUNCTION 'HR_RO_COMPUTE_DATE_INTERVAL'
        CHANGING
          datum1 = lv_begda
          datum2 = lv_endda
          years  = lv_years
          days   = lv_days.
    ENDIF.

    IF lv_data IS INITIAL.
      SELECT *
        INTO TABLE lt_prog_talhao
        FROM zfmprog_talhao
       WHERE acnum EQ lv_acnum
         AND extwg EQ lv_extwg
         AND matkl EQ lv_matkl.
    ELSE.
      IF lv_begda IS NOT INITIAL
      AND lv_endda IS NOT INITIAL.
        SELECT *
          INTO TABLE lt_prog_talhao
          FROM zfmprog_talhao
         WHERE acnum EQ lv_acnum
           AND extwg EQ lv_extwg
           AND matkl EQ lv_matkl
           AND data  BETWEEN lv_begda
                         AND lv_endda.
      ELSE.
        SELECT *
          INTO TABLE lt_prog_talhao
          FROM zfmprog_talhao
         WHERE acnum EQ lv_acnum
           AND extwg EQ lv_extwg
           AND matkl EQ lv_matkl
           AND data  EQ lv_data.
      ENDIF.
    ENDIF.

    DATA(lt_aufnr_check) = lt_prog_talhao[].
    DELETE lt_aufnr_check WHERE aufnr IS INITIAL.
    IF lt_aufnr_check[] IS NOT INITIAL.
*--Check if next process is technically completed
      SELECT aufnr, tecom
        FROM /agri/fmfphdr
        INTO TABLE @DATA(lt_fmfphdr)
        FOR ALL ENTRIES IN @lt_aufnr_check
       WHERE aufnr = @lt_aufnr_check-aufnr.

      IF sy-subrc EQ 0.
        SORT lt_fmfphdr BY aufnr.
      ENDIF.
    ENDIF.

    LOOP AT lt_prog_talhao INTO DATA(ls_prog_talhao).
      CLEAR ls_entityset.
      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
        EXPORTING
          input  = ls_prog_talhao-tplnr_fl
        IMPORTING
          output = ls_prog_talhao-tplnr_fl.

      MOVE-CORRESPONDING ls_prog_talhao TO ls_entityset.
      SHIFT ls_entityset-aarea LEFT DELETING LEADING space.

      READ TABLE lt_fmfphdr INTO DATA(ls_fmfphdr)
        WITH KEY aufnr = ls_prog_talhao-aufnr BINARY SEARCH.
      IF sy-subrc EQ 0
      AND ls_fmfphdr-tecom EQ abap_true.
        ls_entityset-status = abap_true.
      ENDIF.
      APPEND ls_entityset TO et_entityset.
    ENDLOOP.

  ENDMETHOD.


  METHOD setreceitasset_create_entity.

*-- Local Declarations
    DATA: lt_acnum     TYPE zt_fm_acnum,
          lt_achdr     TYPE zt_fmachdr,
          lt_acitm     TYPE zt_fmacitm,
          ls_entity    TYPE zcl_zfarm_agriplan_mpc=>ts_setreceitas,
          ls_zfmrchis  TYPE zfmrchis,
          lv_tplnr_in  TYPE /agri/gltplnr_fl,
          lv_werks     TYPE iwerk,
          lv_matnr_ins TYPE matnr.

    io_data_provider->read_entry_data( IMPORTING es_data = ls_entity ).

    IF ls_entity-acnum IS NOT INITIAL.
      INSERT INITIAL LINE INTO TABLE lt_acnum
       ASSIGNING FIELD-SYMBOL(<ls_acnum>).
      IF sy-subrc EQ 0.
        <ls_acnum>-acnum = ls_entity-acnum.
        CALL FUNCTION 'ZFMAC_READ'
          EXPORTING
            it_acnum       = lt_acnum
          IMPORTING
            et_achdr       = lt_achdr
            et_acitm       = lt_acitm
          EXCEPTIONS
            no_data_exists = 1
            OTHERS         = 2.

        IF ls_entity-tplnr_fl IS NOT INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
            EXPORTING
              input      = ls_entity-tplnr_fl
            IMPORTING
              output     = lv_tplnr_in
            EXCEPTIONS
              not_found  = 1
              not_active = 2
              OTHERS     = 3.
        ENDIF.

        IF lv_tplnr_in IS INITIAL.
          lv_tplnr_in = ls_entity-tplnr_fl.
        ENDIF.
      ENDIF.
    ENDIF.

    lv_werks = ls_entity-werks.
    READ TABLE lt_acitm INTO DATA(ls_acitm)
      WITH KEY tplnr_fl = lv_tplnr_in.
    IF sy-subrc EQ 0.
      lv_werks = ls_acitm-iwerk.
    ENDIF.

    SELECT *
      INTO TABLE @DATA(lt_prog_talhao_bd)
      FROM zfmprog_talhao
     WHERE acnum    EQ @ls_entity-acnum
       AND extwg    EQ @ls_entity-extwg
       AND matkl    EQ @ls_entity-matkl
       AND tplnr_fl EQ @ls_entity-tplnr_fl
       AND data     EQ @ls_entity-data.

    DELETE lt_prog_talhao_bd WHERE aufnr IS NOT INITIAL.

    READ TABLE lt_prog_talhao_bd INTO DATA(ls_prog_talhao_bd) INDEX 1.

    IF sy-subrc EQ 0.
      DATA(lv_terrain_found) = abap_true.
      ls_prog_talhao_bd-rcnum = ls_entity-rcnum.
      MODIFY zfmprog_talhao FROM ls_prog_talhao_bd.
    ELSE.
      lv_terrain_found = abap_false.
*-- Atenção! Grave os terrenos antes de gravar a receita!
      MESSAGE ID 'ZFMFP' TYPE 'E' NUMBER '107' INTO sy-msgli.
      ls_entity-descr = sy-msgli.
    ENDIF.

    IF lv_terrain_found EQ abap_true.
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input        = ls_entity-matnr_ins
        IMPORTING
          output       = lv_matnr_ins
        EXCEPTIONS
          length_error = 1
          OTHERS       = 2.

      IF sy-subrc <> 0.
        lv_matnr_ins = ls_entity-matnr_ins.
      ENDIF.

*      ls_zfmrchis-werks     = ls_entity-werks.
      ls_zfmrchis-werks     = lv_werks.
      ls_zfmrchis-acnum     = ls_entity-acnum.
      ls_zfmrchis-extwg     = ls_entity-extwg.
      ls_zfmrchis-matkl     = ls_entity-matkl.
      ls_zfmrchis-tplnr_fl  = ls_entity-tplnr_fl.
      ls_zfmrchis-data      = ls_entity-data.
      ls_zfmrchis-rcnum     = ls_entity-rcnum.
      ls_zfmrchis-matnr_ins = lv_matnr_ins.
      ls_zfmrchis-data      = ls_entity-data.
      ls_zfmrchis-lgort_ins = ls_entity-lgort.
      ls_zfmrchis-rcdos_ins = ls_entity-rcdos_ins.
      ls_zfmrchis-loevm     = ls_entity-loevm.
      ls_zfmrchis-aenam     = sy-uname.
      ls_zfmrchis-aedat     = sy-datum.
      ls_zfmrchis-aezet     = sy-uzeit.
      MODIFY zfmrchis FROM ls_zfmrchis.
*--  A receita foi gravada com sucesso!
      MESSAGE ID 'ZFMFP' TYPE 'S' NUMBER '108' INTO sy-msgli.
      ls_entity-descr = sy-msgli.
    ENDIF.

  ENDMETHOD.


  METHOD setreceitasset_get_entityset.

*-- Local Declarations
    DATA: lv_acnum     TYPE c LENGTH 40,
          lv_extwg     TYPE extwg,
          lv_matkl     TYPE matkl,
          lv_rcnum     TYPE zfmrcnum,
          lv_werks     TYPE werks_d,
          lrt_tplnr_fl TYPE /iwbep/t_cod_select_options,
          lrt_matnr    TYPE /iwbep/t_cod_select_options.

    LOOP AT it_filter_select_options INTO DATA(ls_filters).
      CASE ls_filters-property.
        WHEN 'Werks'.
          lv_werks = ls_filters-select_options[ 1 ]-low.
        WHEN 'Rcnum'.
          lv_rcnum = ls_filters-select_options[ 1 ]-low.
        WHEN 'Acnum'.
          lv_acnum = ls_filters-select_options[ 1 ]-low.
        WHEN 'Extwg'.
          lv_extwg = ls_filters-select_options[ 1 ]-low.
        WHEN 'Matkl'.
          lv_matkl = ls_filters-select_options[ 1 ]-low.
        WHEN 'TplnrFl'.
          lrt_tplnr_fl = ls_filters-select_options.
        WHEN 'MatnrIns'.
          lrt_matnr = ls_filters-select_options.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

    SELECT *
      INTO TABLE @DATA(lt_receita)
      FROM zfmrchis
     WHERE acnum    EQ @lv_acnum
       AND extwg    EQ @lv_extwg
       AND matkl    EQ @lv_matkl
       AND tplnr_fl IN @lrt_tplnr_fl
       AND rcnum    EQ @lv_rcnum.

    IF sy-subrc EQ 0.
      DELETE zfmrchis FROM TABLE lt_receita.
    ENDIF.

  ENDMETHOD.


  METHOD setreceitasset_update_entity.

*-- Local Declarations
    DATA: ls_zfmrchis TYPE zfmrchis.

    IF er_entity-loevm EQ abap_true.
*-- Update data in database
      MOVE-CORRESPONDING er_entity TO ls_zfmrchis.
      ls_zfmrchis-loevm = abap_true.
      MODIFY zfmrchis FROM ls_zfmrchis.

*-- O produto foi eliminado com sucesso!
      MESSAGE ID 'ZFMFP' TYPE 'S' NUMBER '167' INTO sy-msgli.
      er_entity-descr = sy-msgli.
    ENDIF.

  ENDMETHOD.


  METHOD settechcompletes_create_entity.

    DATA: lt_order         TYPE TABLE OF bapi_order_key,
          lt_detail_return TYPE TABLE OF bapi_order_return,
          ls_prog_talhao   TYPE zfmprog_talhao,
          lv_message       TYPE string.

    CONSTANTS: BEGIN OF c_msg_type,
                 info    LIKE sy-msgty VALUE 'I',
                 warning LIKE sy-msgty VALUE 'W',
                 error   LIKE sy-msgty VALUE 'E',
                 abend   LIKE sy-msgty VALUE 'A',
                 success LIKE sy-msgty VALUE 'S',
                 x       LIKE sy-msgty VALUE 'X',
               END OF c_msg_type.

    io_data_provider->read_entry_data( IMPORTING es_data = er_entity ).
    SHIFT er_entity-aarea LEFT DELETING LEADING space.
    MOVE-CORRESPONDING er_entity TO ls_prog_talhao.

    IF ls_prog_talhao-tplnr_fl IS NOT INITIAL.
      SELECT SINGLE *
        FROM zfmprog_talhao
        INTO @ls_prog_talhao
       WHERE acnum    = @ls_prog_talhao-acnum
         AND extwg    = @ls_prog_talhao-extwg
         AND matkl    = @ls_prog_talhao-matkl
         AND tplnr_fl = @ls_prog_talhao-tplnr_fl
         AND data     = @ls_prog_talhao-data
         AND aufnr    = @ls_prog_talhao-aufnr.

      IF sy-subrc EQ 0
      AND ls_prog_talhao-aufnr IS NOT INITIAL.
        INSERT INITIAL LINE INTO TABLE lt_order
          ASSIGNING FIELD-SYMBOL(<ls_order>).
        IF sy-subrc EQ 0.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = ls_prog_talhao-aufnr
            IMPORTING
              output = <ls_order>-order_number.

*        CALL FUNCTION 'ZABS_PRODORD_COMPLETE_TECH'
          CALL FUNCTION 'ZABS_PRODORD_SET_TECO'
            TABLES
              orders        = lt_order
              detail_return = lt_detail_return.

          READ TABLE lt_detail_return INTO DATA(ls_return)
            WITH KEY type = c_msg_type-error.
          IF sy-subrc NE 0.
            READ TABLE lt_detail_return INTO ls_return
              WITH KEY type = c_msg_type-success.
          ENDIF.

          IF ls_return IS NOT INITIAL.
            CLEAR lv_message.

            CALL FUNCTION 'FORMAT_MESSAGE'
              EXPORTING
                id        = ls_return-id
                no        = ls_return-number
                v1        = ls_return-message_v1
                v2        = ls_return-message_v2
                v3        = ls_return-message_v3
                v4        = ls_return-message_v4
              IMPORTING
                msg       = lv_message
              EXCEPTIONS
                not_found = 1
                OTHERS    = 2.

            er_entity-descr  = lv_message.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      MESSAGE ID 'ZFMFP' TYPE c_msg_type-error
        NUMBER '105' INTO lv_message.
      er_entity-descr  = lv_message.
    ENDIF.

  ENDMETHOD.


  METHOD settechcompletes_get_entityset.

*-- Local Declarations
    DATA: lt_filter        TYPE /iwbep/t_mgw_select_option,
          lt_order         TYPE TABLE OF bapi_order_key,
          lt_detail_return TYPE TABLE OF bapi_order_return,
          lrt_tplnr_fl     TYPE RANGE OF /agri/gltplnr_fl,
          ls_filter        TYPE /iwbep/s_mgw_select_option,
          lt_prog_talhao   TYPE STANDARD TABLE OF zfmprog_talhao INITIAL SIZE 0,
          ls_prog_talhao   TYPE zfmprog_talhao,
          lo_filter        TYPE REF TO /iwbep/if_mgw_req_filter,
          lv_filter_str    TYPE string,
          lv_rcnum         TYPE zfmrcnum,
          lv_aufnr         TYPE aufnr,
          lv_data          TYPE begda,
          lv_extwg         TYPE extwg,
          lv_acnum         TYPE zfmacnum,
          lv_matkl         TYPE matkl,
          lv_message       TYPE string.

    CONSTANTS: BEGIN OF c_msg_type,
                 info    LIKE sy-msgty VALUE 'I',
                 warning LIKE sy-msgty VALUE 'W',
                 error   LIKE sy-msgty VALUE 'E',
                 abend   LIKE sy-msgty VALUE 'A',
                 success LIKE sy-msgty VALUE 'S',
                 x       LIKE sy-msgty VALUE 'X',
               END OF c_msg_type.

    lo_filter = io_tech_request_context->get_filter( ).
    lt_filter = lo_filter->get_filter_select_options( ).
    lv_filter_str = lo_filter->get_filter_string( ).

*--Maps filter table lines to function module parameters
    LOOP AT lt_filter INTO ls_filter.
      CASE ls_filter-property.
        WHEN 'RCNUM'.
          lv_rcnum = ls_filter-select_options[ 1 ]-low.
        WHEN 'AUFNR'.
          lv_aufnr = ls_filter-select_options[ 1 ]-low.
        WHEN 'DATA'.
          lv_data = ls_filter-select_options[ 1 ]-low.
        WHEN 'ACNUM'.
          lv_acnum = ls_filter-select_options[ 1 ]-low.
        WHEN 'EXTWG'.
          lv_extwg = ls_filter-select_options[ 1 ]-low.
        WHEN 'MATKL'.
          lv_matkl = ls_filter-select_options[ 1 ]-low.
        WHEN 'TPLNR_FL'.
          lo_filter->convert_select_option(
            EXPORTING
              is_select_option = ls_filter
            IMPORTING
              et_select_option = lrt_tplnr_fl ).
      ENDCASE.
    ENDLOOP.

    SELECT *
      FROM zfmprog_talhao
      INTO TABLE @lt_prog_talhao
     WHERE acnum    EQ @lv_acnum
       AND extwg    EQ @lv_extwg
       AND matkl    EQ @lv_matkl
       AND tplnr_fl IN @lrt_tplnr_fl[]
       AND data     EQ @lv_data
       AND aufnr    EQ @lv_aufnr.

    READ TABLE lt_prog_talhao INTO ls_prog_talhao INDEX 1.
    IF sy-subrc EQ 0
    AND ls_prog_talhao-aufnr IS NOT INITIAL.
      INSERT INITIAL LINE INTO TABLE lt_order
        ASSIGNING FIELD-SYMBOL(<ls_order>).
      IF sy-subrc EQ 0.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = ls_prog_talhao-aufnr
          IMPORTING
            output = <ls_order>-order_number.

*        CALL FUNCTION 'ZABS_PRODORD_COMPLETE_TECH'
        CALL FUNCTION 'ZABS_PRODORD_SET_TECO'
          TABLES
            orders        = lt_order
            detail_return = lt_detail_return.

        READ TABLE lt_detail_return INTO DATA(ls_return)
          WITH KEY type = c_msg_type-error.
        IF sy-subrc NE 0.
          READ TABLE lt_detail_return INTO ls_return
            WITH KEY type = c_msg_type-success.
          IF sy-subrc NE 0.
            READ TABLE lt_detail_return INTO ls_return
              WITH KEY type = c_msg_type-info.
          ENDIF.
        ENDIF.

        IF ls_return IS NOT INITIAL.
          CLEAR lv_message.

          CALL FUNCTION 'FORMAT_MESSAGE'
            EXPORTING
              id        = ls_return-id
              no        = ls_return-number
              v1        = ls_return-message_v1
              v2        = ls_return-message_v2
              v3        = ls_return-message_v3
              v4        = ls_return-message_v4
            IMPORTING
              msg       = lv_message
            EXCEPTIONS
              not_found = 1
              OTHERS    = 2.

          INSERT INITIAL LINE INTO TABLE et_entityset
            ASSIGNING FIELD-SYMBOL(<ls_entityset>).
          IF sy-subrc EQ 0.
            <ls_entityset>-descr    = lv_message.
            <ls_entityset>-rcnum    = ls_prog_talhao-rcnum.
            <ls_entityset>-aufnr    = ls_prog_talhao-aufnr.
            <ls_entityset>-data     = ls_prog_talhao-data.
            <ls_entityset>-acnum    = ls_prog_talhao-acnum.
            <ls_entityset>-extwg    = ls_prog_talhao-extwg.
            <ls_entityset>-matkl    = ls_prog_talhao-matkl.
            <ls_entityset>-tplnr_fl = ls_prog_talhao-tplnr_fl.
            IF ( ( ls_return-type   EQ 'I'  AND
                   ls_return-id     EQ 'CO' AND
                   ls_return-number EQ 889 )
              OR ( ls_return-type   EQ 'I' AND
                   ls_return-id     EQ 'ZFMFP' AND
                   ls_return-number EQ 098 ) ).
              <ls_entityset>-status = abap_true.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      READ TABLE lrt_tplnr_fl INTO DATA(lrs_tplnr_fl) INDEX 1.
      IF sy-subrc EQ 0.
        IF lrs_tplnr_fl-low EQ 'null'.
*-- Selecione ao menos um Terreno!
          MESSAGE ID 'ZFMFP' TYPE c_msg_type-error
            NUMBER '105' INTO lv_message.
          INSERT INITIAL LINE INTO TABLE et_entityset
            ASSIGNING <ls_entityset>.
          IF sy-subrc EQ 0.
            <ls_entityset>-descr = lv_message.
          ENDIF.
        ELSE.
*-- Não existe Ordem Tarefa para o Terreno selecionado!
          MESSAGE ID 'ZFMFP' TYPE c_msg_type-error
            NUMBER '106' INTO lv_message.
          INSERT INITIAL LINE INTO TABLE et_entityset
            ASSIGNING <ls_entityset>.
          IF sy-subrc EQ 0.
            <ls_entityset>-descr = lv_message.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


METHOD suggest_terrains_get_entityset.

  DATA: lt_dates        TYPE /scwm/tt_lm_dates,
        lwa_prog_talhao TYPE zfmprog_talhao,
        lr_periodo      TYPE RANGE OF tvarvc-low,
        lwa_periodo     LIKE LINE OF lr_periodo,
        lv_total1       TYPE zfmplapr,
        lv_total2       TYPE zfmplapr,
        lv_total3       TYPE zfmplapr,
        lv_total4       TYPE zfmplapr,
        lv_total5       TYPE zfmplapr,
        lv_scheduled    TYPE zfmplapr,
        lv_matkl        TYPE matkl,
        lv_terreno      TYPE /agri/gltplnr_fl,
        lv_aufnr        TYPE aufnr,
        lv_extwg        TYPE extwg,
        lv_begda        TYPE begda,
        lv_endda        TYPE endda,
        lv_acnum        TYPE zfmacnum.

  LOOP AT it_filter_select_options INTO DATA(lwa_filter).
    CASE lwa_filter-property.
      WHEN 'Total1'.
        lv_total1 = lwa_filter-select_options[ 1 ]-low.
      WHEN 'Total2'.
        lv_total2 = lwa_filter-select_options[ 1 ]-low.
      WHEN 'Total3'.
        lv_total3 = lwa_filter-select_options[ 1 ]-low.
      WHEN 'Total4'.
        lv_total4 = lwa_filter-select_options[ 1 ]-low.
      WHEN 'Total5'.
        lv_total5 = lwa_filter-select_options[ 1 ]-low.
      WHEN 'Matkl'.
        lv_matkl  = lwa_filter-select_options[ 1 ]-low.
      WHEN 'Processo'
        OR 'Extwg'.
        lv_extwg  = lwa_filter-select_options[ 1 ]-low.
      WHEN 'Begda'.
        lv_begda  = lwa_filter-select_options[ 1 ]-low.
      WHEN 'Acnum'.
        lv_acnum  = lwa_filter-select_options[ 1 ]-low.
    ENDCASE.
  ENDLOOP.

  CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
    EXPORTING
      date                      = lv_begda
    EXCEPTIONS
      plausibility_check_failed = 1
      OTHERS                    = 2.

  IF sy-subrc <> 0.
    DATA(lv_invalid_date) = abap_true.
  ELSE.
    lv_invalid_date = abap_false.
  ENDIF.

  CHECK lv_invalid_date EQ abap_false.

  CALL FUNCTION 'FIAPPL_ADD_DAYS_TO_DATE'
    EXPORTING
      i_date      = lv_begda
      i_days      = '5'
      signum      = '+'
    IMPORTING
      e_calc_date = lv_endda.

  lv_endda = lv_endda - 1.

  CALL FUNCTION '/SCWM/DATES_BETWEEN_TWO_DATES'
    EXPORTING
      iv_begda = lv_begda
      iv_endda = lv_endda
    IMPORTING
      et_dates = lt_dates.

  LOOP AT lt_dates INTO DATA(lwa_date).
    lwa_periodo = 'IEQ'.
    lwa_periodo-low = lwa_date.
    APPEND lwa_periodo TO lr_periodo.
  ENDLOOP.

  SELECT *
    FROM zfmprog_talhao
    INTO TABLE @DATA(lt_prog_talhao)
   WHERE acnum EQ @lv_acnum
     AND extwg EQ @lv_extwg
     AND matkl EQ @lv_matkl
     AND data  IN @lr_periodo[].

  IF sy-subrc EQ 0.
    LOOP AT lt_prog_talhao ASSIGNING FIELD-SYMBOL(<ls_prog_talhao>).
      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
        EXPORTING
          input  = <ls_prog_talhao>-tplnr_fl
        IMPORTING
          output = <ls_prog_talhao>-tplnr_fl.
    ENDLOOP.

    SORT lt_prog_talhao BY data  ASCENDING
                           aufnr DESCENDING.
  ENDIF.

  IF lt_prog_talhao[] IS NOT INITIAL.
    LOOP AT lr_periodo INTO lwa_periodo.
      CASE sy-tabix.
        WHEN 1.
          DATA(lv_total) = lv_total1.
        WHEN 2.
          lv_total = lv_total2.
        WHEN 3.
          lv_total = lv_total3.
        WHEN 4.
          lv_total = lv_total4.
        WHEN 5.
          lv_total = lv_total5.
      ENDCASE.

      CLEAR lv_scheduled.
*--Check if task order has already been created
      DATA(lv_task_order_exists) = abap_false.
      READ TABLE lt_prog_talhao INTO lwa_prog_talhao
        WITH KEY data = lwa_periodo-low BINARY SEARCH.
      IF sy-subrc EQ 0.
        DATA(lv_tabix) = sy-tabix.
        IF lwa_prog_talhao-aufnr IS NOT INITIAL.
          lv_task_order_exists = abap_true.
        ENDIF.
      ENDIF.

*-- If there is a task order, the terrains scheduled on
*-- that selected date cannot be changed.
      IF lv_task_order_exists EQ abap_true.
        LOOP AT lt_prog_talhao INTO lwa_prog_talhao FROM lv_tabix.
          IF lwa_prog_talhao-data NE lwa_periodo-low.
            EXIT.
          ENDIF.

          IF lwa_prog_talhao-aufnr IS INITIAL.
            EXIT.
          ELSE.
            CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
              EXPORTING
                input  = lwa_prog_talhao-tplnr_fl
              IMPORTING
                output = lv_terreno.
          ENDIF.

          INSERT INITIAL LINE INTO TABLE et_entityset
            ASSIGNING FIELD-SYMBOL(<lwa_entityset>).
          IF sy-subrc EQ 0.
            <lwa_entityset>-acnum     = lwa_prog_talhao-acnum.
            <lwa_entityset>-processo  = lwa_prog_talhao-extwg.
            <lwa_entityset>-matkl     = lwa_prog_talhao-matkl.
            <lwa_entityset>-begda     = lwa_prog_talhao-data.
            <lwa_entityset>-tplnrfl   = lv_terreno.
            <lwa_entityset>-extwg     = lwa_prog_talhao-extwg.
            <lwa_entityset>-to_exists = abap_true.
          ENDIF.
        ENDLOOP.
*-- Check the terrains of the cultivation area and suggest
*-- which terrains can be managed that day.
      ELSEIF lv_task_order_exists EQ abap_false.
        LOOP AT lt_prog_talhao INTO lwa_prog_talhao FROM lv_tabix.
          IF lwa_prog_talhao-data NE lwa_periodo-low.
            EXIT.
          ENDIF.

          CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
            EXPORTING
              input  = lwa_prog_talhao-tplnr_fl
            IMPORTING
              output = lv_terreno.

          INSERT INITIAL LINE INTO TABLE et_entityset
            ASSIGNING <lwa_entityset>.
          IF sy-subrc EQ 0.
            lv_scheduled = lv_scheduled + lwa_prog_talhao-aarea.
            <lwa_entityset>-acnum     = lwa_prog_talhao-acnum.
            <lwa_entityset>-processo  = lwa_prog_talhao-extwg.
            <lwa_entityset>-matkl     = lwa_prog_talhao-matkl.
            <lwa_entityset>-begda     = lwa_prog_talhao-data.
            <lwa_entityset>-tplnrfl   = lv_terreno.
            <lwa_entityset>-extwg     = lwa_prog_talhao-extwg.
            IF lv_scheduled LE lv_total.
              <lwa_entityset>-full     = abap_true.
            ELSE.
              <lwa_entityset>-partial  = abap_true.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDIF.

  SORT et_entityset BY acnum processo matkl begda tplnrfl.

ENDMETHOD.
ENDCLASS.
