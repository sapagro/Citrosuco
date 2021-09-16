class ZCL_ABS_GLUPLOAD_BUDGET definition
  public
  final
  create public .

public section.

*"* public components of class ZCL_ABS_GLUPLOAD_BUDGET
*"* do not include other source files here!!!
  methods ATTRIBUTE_GROUPS_CREATE
    importing
      !IT_TABLE type /AGRI/T_EXCEL_SHEET
    exporting
      !ET_MESSAGES type /AGRI/T_GPROLOG .
  methods MEASUREMENT_DOCUMENT_CREATE
    importing
      !IT_TABLE type /AGRI/T_EXCEL_SHEET
    exporting
      !ET_MESSAGES type /AGRI/T_GPROLOG .
  class-methods READ_EXCEL_FILE
    importing
      !I_ROUTE type CHAR0256
    exporting
      !ET_SHEET type /AGRI/T_EXCEL_SHEET .
  methods AGRIPLAN_BUDGET_UPDATE
    importing
      !IT_TABLE type /AGRI/T_EXCEL_SHEET
    exporting
      !ET_MESSAGES type /AGRI/T_GPROLOG .
protected section.
*"* protected components of class ZCL_ABS_GLUPLOAD_BUDGET
*"* do not include other source files here!!!
private section.

  types:
    tt_bdcdata TYPE STANDARD TABLE OF bdcdata .

  constants MC_TRUE type CHAR1 value 'X' ##NO_TEXT.
  constants MC_ERRMODE type CTU_MODE value 'E' ##NO_TEXT.
  constants MC_DISMODE type CTU_MODE value 'N' ##NO_TEXT.
  constants MC_NOBIEND type CTU_NOBEN value 'X' ##NO_TEXT.
  constants MC_RACOMMIT type CTU_RAFC value 'X' ##NO_TEXT.
  constants MC_NOBINPT type CTU_NOBIM value 'X' ##NO_TEXT.
  class-data MT_BDCDATA type TT_BDCDATA .
  class-data MS_BDCDATA type BDCDATA .

*"* private components of class ZCL_ABS_GLUPLOAD_BUDGET
*"* do not include other source files here!!!
  methods STRUCTURE_BUILD
    importing
      !I_STRUCTURE type TABNAME
      !I_SHEET type NUMC2
    exporting
      !ET_DD03L type THRPAD_ERD_DD03L
    changing
      !CT_TABLE type /AGRI/T_EXCEL_SHEET .
  class-methods DYNPRO_BDC
    importing
      !I_PROGRAM type BDC_PROG
      !I_DYNPRO type BDC_DYNR .
  class-methods DYNPRO_FILL
    importing
      !I_PROGRAM type BDC_PROG
      !I_DYNPRO type BDC_DYNR .
  class-methods FIELD_FILL
    importing
      !I_FNAM type FNAM_____4
      !I_FVAL type BDC_FVAL .
  class-methods FIELD_BDC
    importing
      !I_FNAM type FNAM_____4
      !I_FVAL type BDC_FVAL .
ENDCLASS.



CLASS ZCL_ABS_GLUPLOAD_BUDGET IMPLEMENTATION.


METHOD agriplan_budget_update.

  TYPES: BEGIN OF ty_total,
           acnum      TYPE zfmacnum,
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

         BEGIN OF ty_fazenda,
           acnum     TYPE zfmacnum,
           fazenda   TYPE /agri/gltplnr_fl,
           fazenda_d TYPE string,
           terreno   TYPE string,
           talhao    TYPE string,
           tplnr_fl  TYPE /agri/gltplnr_fl,
           kostlform TYPE kostl,
           kostlmanu TYPE kostl,
         END OF ty_fazenda.

  TYPES: BEGIN OF ty_orc_aux.
      INCLUDE TYPE zabs_orcamento.
  TYPES: tarefa TYPE matnr,
         total  TYPE zabs_del_qtd_tot,
         END OF ty_orc_aux.

  DATA: lt_table        TYPE /agri/t_excel_sheet,
        lt_dd03l        TYPE thrpad_erd_dd03l,
        lt_messages     TYPE /agri/t_gprolog,
        lt_mess_collect TYPE /agri/t_gprolog,
        lt_mess_change  LIKE lt_mess_collect,
        lt_budget_sheet TYPE STANDARD TABLE OF zabs_str_budget_sheet2 INITIAL SIZE 0,
        lt_orcamento    TYPE STANDARD TABLE OF zabs_orcamento INITIAL SIZE 0,
        lt_log          TYPE STANDARD TABLE OF zabs_yorcamento,
        lrt_matnr       TYPE RANGE OF zfmacmatnr,
        ls_orcamento    LIKE LINE OF lt_orcamento,
        ls_data         TYPE /agri/s_excel_sheet,
        ls_dd03l        TYPE dd03l,
        ls_message      TYPE /agri/s_gprolog,
        lv_dif          TYPE f,
        lv_data_c       TYPE char8,
        lv_data_doc     TYPE sydatum,
        lv_begda_dif    TYPE sydatum,
        lv_endda_dif    TYPE sydatum,
        lv_months       TYPE i,
        lv_msgv1        TYPE sy-msgv1,
        lv_msgli        TYPE sy-msgli,
        lv_subrc        TYPE sysubrc,
        lv_posnr        TYPE i,
        lv_tfor         TYPE matnr,
        lv_tman         TYPE matnr,
        lv_timp         TYPE matnr,
        lv_internal     TYPE n LENGTH 6,
        lv_auxiliar     TYPE f,
        lv_passadas     TYPE f,
        lv_minutes      TYPE i,
        lv_i            TYPE i,
        lv_h            TYPE n LENGTH 2,
        lv_m            TYPE n LENGTH 2,
        lv_s            TYPE n LENGTH 2,
        lv_period       TYPE sydatum,
        lv_time         TYPE sy-uzeit,
        lv_time_char    TYPE char0256,
        lv_num          TYPE float,
        lv_row          TYPE /agri/s_excel_sheet-row.

  CONSTANTS: BEGIN OF c_structures,
               bud_01  TYPE tabname  VALUE 'ZABS_STR_BUDGET_SHEET2',
               bud_in  TYPE updkz_d  VALUE 'I',
               bud_up  TYPE updkz_d  VALUE 'U',
               success TYPE sy-msgty VALUE 'S',
             END OF c_structures.

  CONSTANTS: c_updkz_new(1)     TYPE c VALUE 'I',
             c_updkz_update(1)  TYPE c VALUE 'U',
             c_updkz_delete(1)  TYPE c VALUE 'D',
             c_updkz_old(1)     TYPE c VALUE ' ',
             c_updkz_newrow     TYPE c VALUE 'N',
             c_updkz_propose(1) TYPE c VALUE 'P'.

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

  CONSTANTS: BEGIN OF lc_plantio,
               mdtyp TYPE /agri/glmdtyp VALUE 'ZPTA',
               mpgrp TYPE /agri/glmpgrp VALUE 'FAZ-PLANTIO',
             END OF lc_plantio.

  FIELD-SYMBOLS: <lwa_budget_sheet> TYPE zabs_str_budget_sheet2,
                 <lv_value>         TYPE any,
                 <lwa_acdoc>        TYPE /agri/s_fmacs_doc.

  DEFINE add_first_line.
*-- Verificar Linha &2.
    CLEAR ls_message.
    ls_message-msgid = 'ZFMFP'.
    ls_message-msgno = '163'.
    ls_message-msgty = 'I'.
    ls_message-msgv1 = &1.
    APPEND ls_message TO lt_messages.
  END-OF-DEFINITION.

  lt_table[] = it_table[].

  DELETE lt_table WHERE row = 1.
  DELETE lt_table WHERE sheet <> 1.

  "Sheet 1
  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-bud_01
      i_sheet     = 1
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  SORT lt_dd03l BY position.

  LOOP AT lt_table INTO ls_data WHERE sheet = 01.
    IF lv_row NE ls_data-row.
      APPEND INITIAL LINE TO lt_budget_sheet ASSIGNING <lwa_budget_sheet>.
      lv_row = ls_data-row.
    ENDIF.

    READ TABLE lt_dd03l TRANSPORTING NO FIELDS
      WITH KEY position = ls_data-column BINARY SEARCH.
    IF sy-subrc EQ 0.
      LOOP AT lt_dd03l INTO ls_dd03l FROM sy-tabix.
        IF ls_dd03l-position NE ls_data-column.
          EXIT.
        ENDIF.
        ASSIGN COMPONENT ls_data-fieldname
          OF STRUCTURE <lwa_budget_sheet> TO <lv_value>.
        IF sy-subrc EQ 0.
          IF ls_dd03l-domname  NE 'MENGV13'
          AND ls_dd03l-domname NE 'ZFMRCDOSE'
          AND ls_dd03l-domname NE 'ZFMACQTB'
          AND ls_dd03l-domname NE 'ZABS_DOM_GEN'.
            <lv_value> = ls_data-value.
          ENDIF.

          IF ls_dd03l-domname EQ 'DATUM'
          OR ls_dd03l-domname EQ 'DATS'.
            CALL FUNCTION 'KCD_EXCEL_DATE_CONVERT'
              EXPORTING
                excel_date  = ls_data-value
                date_format = 'TMJ'
              IMPORTING
                sap_date    = <lv_value>.
          ELSEIF ls_dd03l-domname EQ 'ZFMACNUM'.
            UNPACK <lv_value> TO <lv_value>.
          ELSEIF ls_dd03l-domname EQ 'MATNR'.
            IF <lv_value> IS NOT INITIAL.
              CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
                EXPORTING
                  input        = <lv_value>
                IMPORTING
                  output       = <lv_value>
                EXCEPTIONS
                  length_error = 1
                  OTHERS       = 2.
              IF ls_dd03l-fieldname EQ 'MATNR'.
                INSERT INITIAL LINE INTO TABLE lrt_matnr
                  ASSIGNING FIELD-SYMBOL(<lrs_matnr>).
                IF sy-subrc EQ 0.
                  <lrs_matnr> = 'IEQ'.
                  <lrs_matnr>-low = 'TMAN' && <lv_value>+4(36).
                ENDIF.

                INSERT INITIAL LINE INTO TABLE lrt_matnr ASSIGNING <lrs_matnr>.
                IF sy-subrc EQ 0.
                  <lrs_matnr> = 'IEQ'.
                  <lrs_matnr>-low = 'TFOR' && <lv_value>+4(36).
                ENDIF.

                INSERT INITIAL LINE INTO TABLE lrt_matnr ASSIGNING <lrs_matnr>.
                IF sy-subrc EQ 0.
                  <lrs_matnr> = 'IEQ'.
                  <lrs_matnr>-low = 'TIMP' && <lv_value>+4(36).
                ENDIF.
              ENDIF.
            ENDIF.
          ELSEIF ls_dd03l-domname EQ 'MENGV13'
              OR ls_dd03l-domname EQ 'ZFMRCDOSE'
              OR ls_dd03l-domname EQ 'ZFMACQTB'
              OR ls_dd03l-domname EQ 'ZABS_DOM_GEN'.
            TRANSLATE ls_data-value USING ',.'.
            <lv_value> = ls_data-value.
          ELSEIF ls_dd03l-domname EQ 'MCPERIOD'.
            CLEAR lv_internal.
            CALL FUNCTION 'CONVERSION_EXIT_PERI6_INPUT'
              EXPORTING
                input           = ls_data-value
              IMPORTING
                output          = lv_internal
              EXCEPTIONS
                input_not_valid = 1
                OTHERS          = 2.
            IF sy-subrc EQ 0.
              <lv_value> = lv_internal.
            ENDIF.
          ELSEIF ls_dd03l-domname EQ 'TIMS'.
            CLEAR lv_time.
            lv_time_char = ls_data-value.
            TRANSLATE lv_time_char USING ',.'.
            CLEAR: lv_num, lv_h, lv_m, lv_s.
            lv_num = lv_time_char.
            lv_num = lv_num * 24 .
            lv_h = floor( lv_num ).
            lv_num = lv_num - lv_h.
            lv_num = lv_num * 60.
            lv_m = floor( lv_num ).
            lv_num = lv_num - lv_m.
            lv_num = lv_num * 60.
            lv_s = lv_num.
            IF lv_s = 60.
              ADD 1 TO lv_m.
              CLEAR lv_s.
            ENDIF.
            IF lv_m = 60.
              ADD 1 TO lv_h.
              CLEAR lv_m.
            ENDIF.
            CONCATENATE lv_h lv_m lv_s INTO lv_time.
            <lv_value> = lv_time.
          ELSEIF ls_dd03l-domname EQ '/AGRI/GLTPLNR_FL'.
            CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
              EXPORTING
                input      = <lv_value>
              IMPORTING
                output     = <lv_value>
              EXCEPTIONS
                not_found  = 1
                not_active = 2
                OTHERS     = 3.
          ELSEIF ls_dd03l-domname EQ 'AUFNR'.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = <lv_value>
              IMPORTING
                output = <lv_value>.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    <lwa_budget_sheet>-excel_row = ls_data-row.
  ENDLOOP.

  IF lt_budget_sheet[] IS INITIAL.
*-- Arquivo Excel não contém dados.
    CLEAR ls_message.
    ls_message-msgid = 'ZFMFP'.
    ls_message-msgno = '122'.
    ls_message-msgty = 'E'.
    APPEND ls_message TO lt_messages.
    RETURN.
  ELSE.
    SELECT *
      FROM zabs_orcamento
      INTO TABLE @DATA(lt_old_entries)
      FOR ALL ENTRIES IN @lt_budget_sheet
     WHERE acnum   EQ @lt_budget_sheet-acnum
       AND extwg   EQ @lt_budget_sheet-extwg
       AND matkl   EQ @lt_budget_sheet-matkl
       AND rcnum   EQ @lt_budget_sheet-rcnum
       AND matnr   EQ @lt_budget_sheet-matnr
       AND period  EQ @lt_budget_sheet-period
       AND fazenda EQ @lt_budget_sheet-fazenda
       AND versao  EQ @lt_budget_sheet-versao.
  ENDIF.

  SORT lt_budget_sheet BY acnum extwg matkl rcnum matnr period fazenda versao.
  LOOP AT lt_old_entries INTO DATA(ls_old_entry).
    READ TABLE lt_budget_sheet INTO DATA(ls_budget_sheet)
      WITH KEY acnum   = ls_old_entry-acnum
               extwg   = ls_old_entry-extwg
               matkl   = ls_old_entry-matkl
               rcnum   = ls_old_entry-rcnum
               matnr   = ls_old_entry-matnr
               period  = ls_old_entry-period
               fazenda = ls_old_entry-fazenda
               versao  = ls_old_entry-versao BINARY SEARCH.
    IF sy-subrc EQ 0.
      INSERT INITIAL LINE INTO TABLE lt_orcamento
        ASSIGNING FIELD-SYMBOL(<ls_orcamento>).
      IF sy-subrc EQ 0.
        <ls_orcamento> = ls_old_entry.
        <ls_orcamento>-passadas = ls_budget_sheet-passadas.
        <ls_orcamento>-rcdos = ls_budget_sheet-rcdos.
      ENDIF.
    ENDIF.
*-- Atualiza Log [ZABS_ORCAMENTO -> ZABS_YORCAMENTO]
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
      <ls_log>-versao = ls_old_entry-versao.
      <ls_log>-extwg = ls_old_entry-extwg.
      <ls_log>-matkl = ls_old_entry-matkl.
      <ls_log>-kostl = ls_old_entry-kostl.
      <ls_log>-aarea_form = ls_old_entry-aarea_form.
      <ls_log>-aarea_manu = ls_old_entry-aarea_manu.
      <ls_log>-passadas = ls_old_entry-passadas.
      <ls_log>-rcdos = ls_old_entry-rcdos.
      <ls_log>-custo = ls_old_entry-custo.
      <ls_log>-produtos = ls_old_entry-produtos.
      <ls_log>-versao_origem = ls_old_entry-versao.
      <ls_log>-usuario = sy-uname.
    ENDIF.
  ENDLOOP.

  DATA(lv_datum) = sy-datum.
  DATA(lv_uzeit) = sy-uzeit.
*-- Atualiza Log [ZABS_ORCAMENTO -> ZABS_YORCAMENTO]
  READ TABLE lt_log INTO DATA(ls_log) INDEX 1.
  IF sy-subrc EQ 0.
    ls_log-data_log = lv_datum.
    ls_log-hora = lv_uzeit.
    ls_log-tcode = 'ZABS_UPLOAD'.
    MODIFY lt_log FROM ls_log
      TRANSPORTING data_log hora tcode WHERE tcode IS INITIAL.
    MODIFY zabs_yorcamento FROM TABLE lt_log[].
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

  READ TABLE lt_orcamento INTO ls_orcamento INDEX 1.
  IF sy-subrc EQ 0.
    ls_orcamento-usuario = sy-uname.
    ls_orcamento-data  = lv_datum.
    ls_orcamento-hora  = lv_uzeit.
    ls_orcamento-tcode = 'ZABS_UPLOAD'.
    MODIFY lt_orcamento FROM ls_orcamento
      TRANSPORTING usuario data hora tcode WHERE tcode IS INITIAL.
*-- Atualiza Registros da Tabela ZABS_ORCAMENTO
    MODIFY zabs_orcamento FROM TABLE lt_orcamento.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ENDIF.
*-- Dados de Orçamento atualizados com sucesso!
    CLEAR ls_message.
    ls_message-msgid = 'ZFMFP'.
    ls_message-msgno = '235'.
    ls_message-msgty = 'S'.
    APPEND ls_message TO lt_messages.
  ENDIF.

  et_messages[] = lt_messages[].

ENDMETHOD.


METHOD ATTRIBUTE_GROUPS_CREATE.
  DATA: lwa_data         TYPE /agri/s_excel_sheet,
        lwa_klah         TYPE /agri/s_gklah,
        lt_klah          TYPE /agri/t_gklah,
        lt_klah2         TYPE /agri/t_gklah,
        lwa_agha         TYPE /agri/s_glagha,
        lt_agha          TYPE /agri/t_glagha,
        lt_agha2         TYPE /agri/t_glagha,
        lwa_swor         TYPE /agri/s_gswor,
        lt_swor          TYPE /agri/t_gswor,
        lwa_ksml         TYPE /agri/s_gksml,
        lt_ksml          TYPE /agri/t_gksml,
        lt_ksml2         TYPE /agri/t_gksml,
        lt_klah_e        TYPE /agri/t_gklah,
        lt_messages      TYPE /agri/t_gprolog,
        lwa_message      TYPE /agri/s_gprolog,
        lt_mass_messages TYPE /agri/t_gprolog,
        lv_clint         TYPE clint,
        lv_posnr         TYPE kposnr,
        lt_table         TYPE /agri/t_excel_sheet,
        lt_dd03l         TYPE thrpad_erd_dd03l,  "ishmed_t_dd03l,
        lwa_dd03l        TYPE dd03l,
        lt_ag01          TYPE STANDARD TABLE OF /agri/s_ag_sheet1,
        lt_ag02          TYPE STANDARD TABLE OF /agri/s_ag_sheet2,
        lv_row           TYPE /agri/s_excel_sheet-row.

  FIELD-SYMBOLS: <lwa_ag01>  TYPE /agri/s_ag_sheet1,
                 <lwa_ag02>  TYPE /agri/s_ag_sheet2,
                 <lwa_gklah> TYPE /agri/s_gklah,
                 <lwa_ksml>  TYPE /agri/s_gksml,
                 <lwa_agha>  TYPE /agri/s_glagha,
                 <lv_value>  TYPE any.

  CONSTANTS: BEGIN OF c_structures,
    ag_01 TYPE tabname VALUE '/AGRI/S_AG_SHEET1',
    ag_02 TYPE tabname VALUE '/AGRI/S_AG_SHEET2',
    ag_in TYPE updkz_d VALUE 'I',
    END OF c_structures.


  lt_table[] = it_table[].
  DELETE lt_table WHERE row BETWEEN 1 AND 15          "#EC CI_STDSEQ
             OR column EQ 1.

  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-ag_01
      i_sheet     = 1
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  LOOP AT lt_table INTO lwa_data WHERE sheet = 1.   "#EC CI_STDSEQ
*    AT NEW row.
*      APPEND INITIAL LINE TO lt_ag01 ASSIGNING <lwa_ag01>.
*    ENDAT.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_ag01 ASSIGNING <lwa_ag01>.
      lv_row = lwa_data-row.
    ENDIF.
    LOOP AT lt_dd03l INTO lwa_dd03l
                      WHERE position = lwa_data-column.   "#EC CI_STDSEQ
      ASSIGN COMPONENT lwa_data-fieldname
                  OF STRUCTURE <lwa_ag01> TO <lv_value>.
      <lv_value> = lwa_data-value.

    ENDLOOP.
  ENDLOOP.
  CLEAR lv_row.

  LOOP AT lt_ag01 ASSIGNING <lwa_ag01>.
    lv_clint = sy-tabix.
    MOVE-CORRESPONDING <lwa_ag01> TO lwa_klah.
    lwa_klah-clint = lv_clint.
    lwa_klah-updkz = c_structures-ag_in.
    MOVE-CORRESPONDING <lwa_ag01> TO lwa_agha.
    lwa_agha-updkz = c_structures-ag_in.

    MOVE-CORRESPONDING <lwa_ag01> TO lwa_swor.
    lwa_swor-clint = lv_clint.
    lwa_swor-spras = sy-langu.
    lwa_swor-updkz = c_structures-ag_in.

    APPEND lwa_agha TO lt_agha. CLEAR lwa_agha.
    APPEND lwa_klah TO lt_klah. CLEAR lwa_klah.
    APPEND lwa_swor TO lt_swor. CLEAR lwa_swor.
  ENDLOOP.

  "Sheet 2
  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-ag_02
      i_sheet     = 2
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  LOOP AT lt_table INTO lwa_data WHERE sheet = 2.   "#EC CI_STDSEQ
*    AT NEW row.
*      APPEND INITIAL LINE TO lt_ag02 ASSIGNING <lwa_ag02>.
*    ENDAT.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_ag02 ASSIGNING <lwa_ag02>.
      lv_row = lwa_data-row.
    ENDIF.
    LOOP AT lt_dd03l INTO lwa_dd03l
                      WHERE position = lwa_data-column.  "#EC CI_STDSEQ
      ASSIGN COMPONENT lwa_data-fieldname
                  OF STRUCTURE <lwa_ag02> TO <lv_value>.
      <lv_value> = lwa_data-value.

    ENDLOOP.
  ENDLOOP.
  CLEAR lv_row.

  LOOP AT lt_ag02 ASSIGNING <lwa_ag02>.
    AT NEW class.
      CLEAR lv_posnr.
    ENDAT.
    ADD 1 TO lv_posnr.
    MOVE-CORRESPONDING <lwa_ag02> TO lwa_ksml.
    READ TABLE lt_klah INTO lwa_klah                   "#EC CI_STDSEQ
                        WITH KEY class = <lwa_ag02>-class.
    IF sy-subrc = 0.
      lwa_ksml-clint = lwa_klah-clint.
      lwa_ksml-posnr = lv_posnr.
      lwa_ksml-updkz = c_structures-ag_in.
      APPEND lwa_ksml TO lt_ksml. CLEAR lwa_ksml.
    ENDIF.


  ENDLOOP.

  CLEAR lt_swor[].

  LOOP AT lt_klah ASSIGNING <lwa_gklah>.
    APPEND <lwa_gklah> TO lt_klah2.
    READ TABLE lt_ag01 ASSIGNING <lwa_ag01> WITH KEY class = <lwa_gklah>-class.  "#EC CI_STDSEQ
    IF sy-subrc = 0.
      LOOP AT lt_ksml ASSIGNING <lwa_ksml> WHERE clint = <lwa_gklah>-clint.     "#EC CI_STDSEQ
        APPEND <lwa_ksml> TO lt_ksml2.
      ENDLOOP.

      LOOP AT lt_agha ASSIGNING <lwa_agha> WHERE class = <lwa_gklah>-class.     "#EC CI_STDSEQ
        APPEND <lwa_agha> TO lt_agha2.
      ENDLOOP.
*{   INSERT         SS8K900620                                        1
*****Don't Create attribute group without attributes
      IF lt_klah2 IS NOT INITIAL AND
         lt_ksml2 IS INITIAL.
        lwa_message-msgid = '/AGRI/GLAG'.
        lwa_message-msgno = '002'.
        lwa_message-msgty = 'E'.
        lwa_message-msgv1 = <lwa_gklah>-class.
        APPEND lwa_message TO lt_mass_messages.
        CONTINUE.
      ENDIF.
*}   INSERT

*ENHANCEMENT-POINT ATTRIBUTE_GROUPS_CREATE SPOTS /AGRI/ES_CL_UPLOAD_MASTER_DATA .

      CALL FUNCTION '/AGRI/GLAG_CREATE_MASS'
        EXPORTING
          i_agcat               = <lwa_ag01>-agcat "'MP'
*         I_MESSAGE_LOG_INIT    = 'X'
*         I_PLCRULE             =
*         I_COMMIT              = 'X'
*         I_SET_UPDATE_TASK     = 'X'
          it_klah               = lt_klah2
          it_swor               = lt_swor
          it_ksml               = lt_ksml2
*         IT_AGTLN              =
          it_agha               = lt_agha2
        IMPORTING
          et_klah               = lt_klah_e
          et_messages           = lt_messages
        EXCEPTIONS
          invalid_parameters    = 1
          incomplete_parameters = 2
          OTHERS                = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                INTO sy-msgli.
        lwa_message-msgid = sy-msgid.
        lwa_message-msgno = sy-msgno.
        lwa_message-msgty = sy-msgty.
        lwa_message-msgv1 = sy-msgv1.
        lwa_message-msgv2 = sy-msgv2.
        lwa_message-msgv3 = sy-msgv3.
        lwa_message-msgv4 = sy-msgv4.
        APPEND lwa_message TO lt_messages.
      ENDIF.

      APPEND LINES OF lt_messages TO lt_mass_messages.
    ENDIF.
    CLEAR: lt_klah2[],
           lt_ksml2[],
           lt_agha2[].
  ENDLOOP.

  et_messages = lt_mass_messages.

ENDMETHOD.


  METHOD DYNPRO_BDC.

    DATA: ls_bdcdata TYPE bdcdata.

    ls_bdcdata-program  = i_program.
    CONDENSE ls_bdcdata-program NO-GAPS.

    ls_bdcdata-dynpro   = i_dynpro.
    CONDENSE ls_bdcdata-dynpro NO-GAPS.

    ls_bdcdata-dynbegin = 'X'.

    APPEND ls_bdcdata TO mt_bdcdata.

  ENDMETHOD.


  METHOD DYNPRO_FILL.

    CLEAR: ms_bdcdata.
    ms_bdcdata-program = i_program.
    ms_bdcdata-dynpro  = i_dynpro.
    dynpro_bdc( i_program = ms_bdcdata-program
                i_dynpro  = ms_bdcdata-dynpro ).

  ENDMETHOD.


METHOD FIELD_BDC.

  DATA: ls_bdcdata TYPE bdcdata.

  IF i_fval <> '/'.

    ls_bdcdata-fnam = i_fnam.
    CONDENSE ls_bdcdata-fval NO-GAPS.

    ls_bdcdata-fval = i_fval.
    CONDENSE ls_bdcdata-fval NO-GAPS.

    APPEND ls_bdcdata TO mt_bdcdata.
  ENDIF.

ENDMETHOD.


  METHOD FIELD_FILL.

    CLEAR: ms_bdcdata.
    ms_bdcdata-fnam = i_fnam.
    ms_bdcdata-fval = i_fval.
    field_bdc( i_fnam = ms_bdcdata-fnam
               i_fval = ms_bdcdata-fval ).

  ENDMETHOD.


METHOD MEASUREMENT_DOCUMENT_CREATE.
  DATA: lt_md01     TYPE TABLE OF /agri/s_md_sheet1,
        lt_md02     TYPE TABLE OF /agri/s_md_sheet2,
        lt_mdtyp    TYPE TABLE OF /agri/tglmdtyp,
        ls_mdtyp    TYPE /agri/tglmdtyp,
        lwa_data    TYPE /agri/s_excel_sheet,
        lv_posnr    TYPE /agri/glmdocitm,
        lwa_mdhdr   TYPE /agri/s_glmdhdr,
        lt_mdhdr    TYPE /agri/t_glmdhdr,
        lwa_mditm   TYPE /agri/s_glmditm,
        lt_mditm    TYPE /agri/t_glmditm,
        lwa_mdatv   TYPE /agri/s_glmdatv,
        lt_mdatv    TYPE /agri/t_glmdatv,
        lt_mditx    TYPE /agri/t_glmditm,
        lwa_mddoc   TYPE /agri/s_glmd_doc,
        lt_mddoc    TYPE /agri/t_glmd_doc,
        lwa_message TYPE /agri/s_gprolog,
        lt_messages TYPE /agri/t_gprolog,
        lt_table    TYPE /agri/t_excel_sheet,
        lt_dd03l    TYPE thrpad_erd_dd03l,    "ishmed_t_dd03l,
        lwa_dd03l   TYPE dd03l,
        lv_row      TYPE /agri/s_excel_sheet-row.

  FIELD-SYMBOLS: <lwa_md01> TYPE /agri/s_md_sheet1,
                 <lwa_md02> TYPE /agri/s_md_sheet2,
                 <lv_value> TYPE any.

  CONSTANTS: BEGIN OF c_structures,
               md_01 TYPE tabname VALUE '/AGRI/S_MD_SHEET1',
               md_02 TYPE tabname VALUE '/AGRI/S_MD_SHEET2',
               md_in TYPE updkz_d VALUE 'I',
             END OF c_structures.

  REFRESH: et_messages.

  lt_table[] = it_table[].
  DELETE lt_table WHERE row BETWEEN 1 AND 15   "#EC CI_STDSEQ
             OR column EQ 1.

  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-md_01
      i_sheet     = 1
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  LOOP AT lt_table INTO lwa_data WHERE sheet = 1.  "#EC CI_STDSEQ
*    AT NEW row.
*      APPEND INITIAL LINE TO lt_md01 ASSIGNING <lwa_md01>.
*    ENDAT.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_md01 ASSIGNING <lwa_md01>.
      lv_row = lwa_data-row.
    ENDIF.
    LOOP AT lt_dd03l INTO lwa_dd03l
                      WHERE position = lwa_data-column.  "#EC CI_STDSEQ
      ASSIGN COMPONENT lwa_data-fieldname
                  OF STRUCTURE <lwa_md01> TO <lv_value>.
      <lv_value> = lwa_data-value.

    ENDLOOP.
  ENDLOOP.
  CLEAR lv_row.

  LOOP AT lt_md01 ASSIGNING <lwa_md01>.
    MOVE-CORRESPONDING <lwa_md01> TO lwa_mdhdr.
    lwa_mdhdr-updkz = c_structures-md_in.
*    CALL FUNCTION 'CONVERSION_EXIT_TPLNR_INPUT'
*      EXPORTING
*        input                      = lwa_mdhdr-tplnr_fl
*     IMPORTING
*       OUTPUT                     = lwa_mdhdr-tplnr_fl
*     EXCEPTIONS
*       NOT_FOUND                  = 1
*       OTHERS                     = 2
*              .
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*    ENDIF.
    CALL FUNCTION '/AGRI/G_CONV_EXIT_TPLNR_INPUT'
      EXPORTING
        i_input  = lwa_mdhdr-tplnr_fl
*       I_NO_MESSAGE       =
      IMPORTING
        e_output = lwa_mdhdr-tplnr_fl
*       ES_RETURN          =
      .

    APPEND lwa_mdhdr TO lt_mdhdr. CLEAR lwa_mdhdr.
  ENDLOOP.

  IF lt_mdhdr IS NOT INITIAL.
    SELECT * FROM /agri/tglmdtyp
   INTO TABLE lt_mdtyp
      FOR ALL ENTRIES IN lt_mdhdr
      WHERE mdtyp EQ lt_mdhdr-mdtyp.
    SORT lt_mdtyp BY mdtyp.
  ENDIF.

  "Sheet 2
  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-md_02
      i_sheet     = 2
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  LOOP AT lt_table INTO lwa_data WHERE sheet = 2.
*    AT NEW row.
*      APPEND INITIAL LINE TO lt_md02 ASSIGNING <lwa_md02>.
*    ENDAT.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_md02 ASSIGNING <lwa_md02>.
      lv_row = lwa_data-row.
    ENDIF.
    LOOP AT lt_dd03l INTO lwa_dd03l
                      WHERE position = lwa_data-column. "#EC CI_STDSEQ
      ASSIGN COMPONENT lwa_data-fieldname
                  OF STRUCTURE <lwa_md02> TO <lv_value>.
      <lv_value> = lwa_data-value.

    ENDLOOP.
  ENDLOOP.
  CLEAR lv_row.
  LOOP AT lt_md02 ASSIGNING <lwa_md02>.
    MOVE-CORRESPONDING <lwa_md02> TO lwa_mdatv.
    lwa_mdatv-updkz = c_structures-md_in.
    SELECT SINGLE c~atinn g~atbez_uc INTO (lwa_mdatv-atinn, lwa_mdatv-atbez)  "#EC CI_SEL_NESTED
      FROM cabn AS c INNER JOIN /agri/gatda AS g ON c~atinn = g~atinn
       WHERE c~atnam = <lwa_md02>-atnam AND g~spras = sy-langu."#EC CI_NOORDER
    APPEND lwa_mdatv TO lt_mdatv.
    CLEAR lwa_mdatv.
  ENDLOOP.


  LOOP AT lt_md02 ASSIGNING <lwa_md02>.
    MOVE-CORRESPONDING <lwa_md02> TO lwa_mditm.
    lwa_mditm-updkz = c_structures-md_in.
    SELECT SINGLE c~msehi g~atbez_uc INTO (lwa_mditm-cunit, lwa_mditm-atbez)  "#EC CI_SEL_NESTED
      FROM cabn AS c INNER JOIN /agri/gatda AS g ON c~atinn = g~atinn
       WHERE c~atnam = lwa_mditm-atnam AND g~spras = sy-langu."#EC CI_NOORDER
    APPEND lwa_mditm TO lt_mditm. CLEAR lwa_mditm.
  ENDLOOP.

*{   INSERT         SS8K900620                                        1
****Date check
  LOOP AT lt_mdhdr INTO DATA(ls_mdhdr).
    IF ls_mdhdr-mdate IS NOT INITIAL.
      CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
        EXPORTING
          date                           = ls_mdhdr-mdate
       EXCEPTIONS
         plausibility_check_failed       = 1
         OTHERS                          = 2.
      IF sy-subrc <> 0.
        lwa_message-msgid = '/AGRI/GLAG'.
        lwa_message-msgno = '005'.
        lwa_message-msgty = 'E'.
        lwa_message-msgv1 = ls_mdhdr-mdocm.
      APPEND lwa_message TO et_messages.
      DELETE lt_mdhdr WHERE mdocm = ls_mdhdr-mdocm.
      DELETE lt_mditm WHERE mdocm = ls_mdhdr-mdocm.
      ENDIF.
    ENDIF.
  ENDLOOP.
*}   INSERT

*ENHANCEMENT-POINT MEASUREMENT_DOCUMENT_CREATE SPOTS /AGRI/ES_CL_UPLOAD_MASTER_DATA .

  CALL FUNCTION '/AGRI/GLMD_CREATE_MASS'
    EXPORTING
*     I_MESSAGES_DISPLAY = ' '
*     I_REFRESH_MESSAGES = 'X'
*     I_SAVE_MESSAGES   = 'X'
      it_mdhdr          = lt_mdhdr
      it_mdatv          = lt_mdatv
    IMPORTING
      et_mddoc          = lt_mddoc
      et_messages       = lt_messages
*     E_LOG_NUMBER      =
    EXCEPTIONS
      inconsistent_data = 1
      OTHERS            = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO sy-msgli.
    lwa_message-msgid = sy-msgid.
    lwa_message-msgno = sy-msgno.
    lwa_message-msgty = sy-msgty.
    lwa_message-msgv1 = sy-msgv1.
    lwa_message-msgv2 = sy-msgv2.
    lwa_message-msgv3 = sy-msgv3.
    lwa_message-msgv4 = sy-msgv4.
    APPEND lwa_message TO lt_messages.
  ENDIF.

  APPEND LINES OF lt_messages TO et_messages.


*  LOOP AT lt_mdhdr INTO lwa_mdhdr.
*    lt_mditx[] = lt_mditm[].
*    DELETE lt_mditx WHERE mdocm NE lwa_mdhdr-mdocm.
*    READ TABLE lt_mdtyp INTO ls_mdtyp
*              WITH KEY mdtyp = lwa_mdhdr-mdtyp
*              BINARY SEARCH.
*    IF sy-subrc = 0.
*
*
*
*
*      CALL FUNCTION '/AGRI/GLMD_CREATE'
*        EXPORTING
**         i_messages_display = 'X'
**         i_dialog           = 'X'
**         i_refresh_messages = 'X'
**         i_save_messages    = 'X'
*          i_aslvl            = ls_mdtyp-aslvl
*          i_mpgrp            = lwa_mdhdr-mpgrp
*          is_mdhdr           = lwa_mdhdr
*          it_mditm           = lt_mditx
*        IMPORTING
*          es_mddoc           = lwa_mddoc
*          et_messages        = lt_messages
*        EXCEPTIONS
*          creation_failed    = 1
*          action_canceled    = 2
*          invalid_parameters = 3
*          inconsistent_data  = 4
*          OTHERS             = 5.
*
*      IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
*                INTO sy-msgli.
*        lwa_message-msgid = sy-msgid.
*        lwa_message-msgno = sy-msgno.
*        lwa_message-msgty = sy-msgty.
*        lwa_message-msgv1 = sy-msgv1.
*        lwa_message-msgv2 = sy-msgv2.
*        lwa_message-msgv3 = sy-msgv3.
*        lwa_message-msgv4 = sy-msgv4.
*        APPEND lwa_message TO lt_messages.
*      ENDIF.
*
*      APPEND lwa_mddoc TO lt_mddoc. CLEAR lwa_mddoc.
*      APPEND LINES OF lt_messages TO et_messages.
*      CLEAR: lt_messages[], lt_mditx[].
*      WAIT UP TO 2 SECONDS.
*    ENDIF.
*
*  ENDLOOP.

ENDMETHOD.


METHOD READ_EXCEL_FILE.

  DATA:
    ref_container   TYPE REF TO cl_gui_custom_container,
    ref_control     TYPE REF TO i_oi_container_control,
    ref_document    TYPE REF TO i_oi_document_proxy,
    ref_spreadsheet TYPE REF TO i_oi_spreadsheet,
    ref_error       TYPE REF TO i_oi_error,
    lt_sheets_out   TYPE  /agri/t_excel_sheet,
    ls_sheets_out   TYPE /agri/s_excel_sheet,

    ls_sheets TYPE soi_sheets_table,
    lt_data   TYPE soi_generic_table,
    ls_ranges TYPE soi_range_list,

    lv_rows TYPE i VALUE 3000, "Rows (Maximum 65536)
    lv_cols TYPE i VALUE 30, "Columns (Maximum 256)
    lv_subrc TYPE sy-tabix,
    lv_excel  TYPE char0256.

  FIELD-SYMBOLS: <lwa_sheet> TYPE soi_sheets,
                 <lwa_data> TYPE soi_generic_item.


  CALL METHOD c_oi_container_control_creator=>get_container_control
    IMPORTING
      control = ref_control
      error   = ref_error.

  IF ref_error->has_failed = 'X'.
    CALL METHOD ref_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.

  CREATE OBJECT ref_container
    EXPORTING
      container_name              = 'CONT'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.
  IF sy-subrc <> 0.
***Extended Additional Syntax Check ATC  1709 PQ
        MESSAGE e001(00) WITH 'Error while creating container'(001). "#EC MG_MISSING
****
        ENDIF.

  CALL METHOD ref_control->init_control
    EXPORTING
      inplace_enabled      = 'X'
      r3_application_name  = 'EXCEL CONTAINER'
      parent               = ref_container
    IMPORTING
      error                = ref_error
    EXCEPTIONS
      javabeannotsupported = 1
      OTHERS               = 2.
  IF ref_error->has_failed = 'X'.
    CALL METHOD ref_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.



  CALL METHOD ref_control->get_document_proxy
    EXPORTING
      document_type  = soi_doctype_excel_sheet
    IMPORTING
      document_proxy = ref_document
      error          = ref_error.

  IF ref_error->has_failed = 'X'.
    CALL METHOD ref_error->raise_message
      EXPORTING
        type = 'E'.
  ENDIF.


  CONCATENATE 'FILE://' i_route INTO lv_excel.

  CALL METHOD ref_document->open_document
    EXPORTING
      document_title = 'Excel'(002)
      document_url   = lv_excel
      open_inplace   = 'X'
    IMPORTING
      error          = ref_error.


  IF ref_error->has_failed = 'X'.
    CALL METHOD ref_error->raise_message
      EXPORTING
        type = 'I'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  CALL METHOD ref_document->get_spreadsheet_interface
    EXPORTING
      no_flush        = ' '
    IMPORTING
      error           = ref_error
      sheet_interface = ref_spreadsheet.

  IF ref_error->has_failed = 'X'.
    CALL METHOD ref_error->raise_message
      EXPORTING
        type = 'I'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  CALL METHOD ref_spreadsheet->get_sheets
    EXPORTING
      no_flush = ' '
    IMPORTING
      sheets   = ls_sheets
      error    = ref_error.

  IF ref_error->has_failed = 'X'.
    CALL METHOD ref_error->raise_message
      EXPORTING
        type = 'I'.
    LEAVE LIST-PROCESSING.
  ENDIF.


*loop for all exel sheets
  LOOP AT ls_sheets ASSIGNING <lwa_sheet>.
    lv_subrc = sy-tabix.

    CALL METHOD ref_spreadsheet->select_sheet
      EXPORTING
        name  = <lwa_sheet>-sheet_name
      IMPORTING
        error = ref_error.

    IF ref_error->has_failed = 'X'.
      EXIT.
    ENDIF.

    CALL METHOD ref_spreadsheet->set_selection
      EXPORTING
        top     = 1
        left    = 1
        rows    = lv_rows
        columns = lv_cols.

    CALL METHOD ref_spreadsheet->insert_range
      EXPORTING
        name     = 'Test'(003)
        rows     = lv_rows
        columns  = lv_cols
        no_flush = ''
      IMPORTING
        error    = ref_error.

    IF ref_error->has_failed = 'X'.
      EXIT.
    ENDIF.

    REFRESH lt_data.

    CALL METHOD ref_spreadsheet->get_ranges_data
      EXPORTING
        all      = 'X'
      IMPORTING
        contents = lt_data
        error    = ref_error
      CHANGING
        ranges   = ls_ranges.

* Remove ranges not to be processed else the data keeps on adding up
    CALL METHOD ref_spreadsheet->delete_ranges
      EXPORTING
        ranges = ls_ranges.

    DELETE lt_data WHERE value IS INITIAL. "OR value = space.


    LOOP AT lt_data ASSIGNING <lwa_data>.

      ls_sheets_out-sheet = lv_subrc.
      MOVE-CORRESPONDING <lwa_data> TO ls_sheets_out.
      APPEND ls_sheets_out TO lt_sheets_out.

    ENDLOOP.
  ENDLOOP.


  CALL METHOD ref_document->close_document
    IMPORTING
      error = ref_error.


  IF ref_error->has_failed = 'X'.
    CALL METHOD ref_error->raise_message
      EXPORTING
        type = 'I'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  CALL METHOD ref_document->release_document
*  EXPORTING
*    no_flush = ' '
     IMPORTING
       error    = ref_error.

  IF ref_error->has_failed = 'X'.
    CALL METHOD ref_error->raise_message
      EXPORTING
        type = 'I'.
    LEAVE LIST-PROCESSING.
  ENDIF.


  et_sheet = lt_sheets_out.

ENDMETHOD.


METHOD STRUCTURE_BUILD.

  DATA: lwa_dd03l   TYPE dd03l,
        lv_position TYPE position.

  FIELD-SYMBOLS: <lwa_data> TYPE /agri/s_excel_sheet.

  SELECT *
    INTO TABLE @DATA(lt_dd03l)
    FROM dd03l
   WHERE tabname = @i_structure.

  DELETE lt_dd03l WHERE fieldname = '.INCLUDE'
                     OR fieldname = 'DUMMY2'
                     OR fieldname = '.INCLU--AP'.

  LOOP AT lt_dd03l ASSIGNING FIELD-SYMBOL(<lwa_dd03l>).
    ADD 1 TO lv_position.
    <lwa_dd03l>-position = lv_position.
  ENDLOOP.

  SORT lt_dd03l BY position.
  et_dd03l[] = lt_dd03l[].

  LOOP AT ct_table ASSIGNING <lwa_data> WHERE sheet = i_sheet.
    READ TABLE et_dd03l INTO lwa_dd03l
      WITH KEY position = <lwa_data>-column BINARY SEARCH.
    IF sy-subrc EQ 0.
      <lwa_data>-fieldname = lwa_dd03l-fieldname.
    ENDIF.
  ENDLOOP.

ENDMETHOD.
ENDCLASS.
