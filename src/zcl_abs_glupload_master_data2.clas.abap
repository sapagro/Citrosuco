class ZCL_ABS_GLUPLOAD_MASTER_DATA2 definition
  public
  final
  create public .

public section.

*"* public components of class ZCL_ABS_GLUPLOAD_MASTER_DATA2
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
  methods TERRAINS_CREATE
    importing
      !IT_TABLE type /AGRI/T_EXCEL_SHEET
    exporting
      !ET_MESSAGES type /AGRI/T_GPROLOG .
  methods ATTRIBUTE_CREATE
    importing
      !IT_TABLE type /AGRI/T_EXCEL_SHEET
    exporting
      !ET_MESSAGES type /AGRI/T_GPROLOG .
  methods CROP_MASTER_CREATE
    importing
      !IT_TABLE type /AGRI/T_EXCEL_SHEET
    exporting
      !ES_GLCM_DOC type /AGRI/S_GLCM_DOC
      !ET_MESSAGES type /AGRI/T_GPROLOG .
  methods CROP_SEASON_CREATE
    importing
      !IT_TABLE type /AGRI/T_EXCEL_SHEET
    exporting
      !ET_MESSAGES type /AGRI/T_GPROLOG
      !ET_TABLE type /AGRI/T_EXCEL_SHEET .
  methods IRRIGATION_EQUIP_CREATE
    importing
      !IT_TABLE type /AGRI/T_EXCEL_SHEET
    exporting
      !ET_MESSAGES type /AGRI/T_GPROLOG .
  methods ADDITIONAL_FIELDS_POPULATE
    importing
      !IT_TABLE type /AGRI/T_EXCEL_SHEET
    exporting
      !ET_MESSAGES type /AGRI/T_GPROLOG .
  methods ROUTING_CHANGE
    importing
      !IT_TABLE type /AGRI/T_EXCEL_SHEET
    exporting
      !ET_MESSAGES type /AGRI/T_GPROLOG .
  methods RECIPES_CREATE
    importing
      !IT_TABLE type /AGRI/T_EXCEL_SHEET
    exporting
      !ET_MESSAGES type /AGRI/T_GPROLOG .
  methods UPDATE_TECHNICAL_INDEXES
    importing
      !IT_TABLE type /AGRI/T_EXCEL_SHEET
    exporting
      !ET_MESSAGES type /AGRI/T_GPROLOG .
  methods CROP_SEASON_CHANGE
    importing
      !IT_TABLE type /AGRI/T_EXCEL_SHEET
    exporting
      !ET_MESSAGES type /AGRI/T_GPROLOG
      !ET_TABLE type /AGRI/T_EXCEL_SHEET .
  methods ACCOMPLISHMENT_SHEET_CREATE
    importing
      !IT_TABLE type /AGRI/T_EXCEL_SHEET
    exporting
      !ET_MESSAGES type /AGRI/T_GPROLOG .
  methods CALCULATE_COST_ORDER
    importing
      !IV_AUFNR type AUFNR
    exporting
      !ET_MESSAGES type /AGRI/T_GPROLOG .
  methods PRODORD_CHANGE
    importing
      !IT_TABLE type /AGRI/T_EXCEL_SHEET
    exporting
      !ET_MESSAGES type /AGRI/T_GPROLOG .
  methods ROUTES_CHANGE
    importing
      !IT_TABLE type /AGRI/T_EXCEL_SHEET
    exporting
      !ET_MESSAGES type /AGRI/T_GPROLOG .
  methods AGRIPLAN_BUDGET_CREATE
    importing
      !IT_TABLE type /AGRI/T_EXCEL_SHEET
    exporting
      !ET_MESSAGES type /AGRI/T_GPROLOG .
  methods ACCOMPLISHMENT_SHEET_UPDATE
    importing
      !IT_TABLE type /AGRI/T_EXCEL_SHEET
    exporting
      !ET_MESSAGES type /AGRI/T_GPROLOG .
  methods STANDARD_STRUCTURE_BUILD
    importing
      !I_STRUCTURE type TABNAME
      !I_SHEET type NUMC2
    exporting
      !ET_DD03L type THRPAD_ERD_DD03L
    changing
      !CT_TABLE type /AGRI/T_EXCEL_SHEET .
  methods SOLIDOSHA_UPDATE
    importing
      !IT_TABLE type /AGRI/T_EXCEL_SHEET
    exporting
      !ET_MESSAGES type /AGRI/T_GPROLOG .
protected section.
*"* protected components of class ZCL_ABS_GLUPLOAD_MASTER_DATA2
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

*"* private components of class ZCL_ABS_GLUPLOAD_MASTER_DATA2
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



CLASS ZCL_ABS_GLUPLOAD_MASTER_DATA2 IMPLEMENTATION.


METHOD ACCOMPLISHMENT_SHEET_CREATE.

  DATA: lt_table         TYPE /agri/t_excel_sheet,
        lt_dd03l         TYPE thrpad_erd_dd03l,
        lt_messages      TYPE /agri/t_gprolog,
        lt_mess_collect  TYPE /agri/t_gprolog,
        lt_mess_change   LIKE lt_mess_collect,
        lt_acc_sheet     TYPE STANDARD TABLE OF zabs_str_accomplishment_sheet1 INITIAL SIZE 0,
        lt_acitm_new     TYPE /agri/t_fmfmacitm,
        lt_acitm_all     TYPE /agri/t_fmfmacitm,
        lt_activity      TYPE /agri/t_fmac_src_act,
        lt_bdcdata       TYPE bdcdata_tab,
        lt_bdc_messages  TYPE ettcd_msg_tabtype,
        lt_acdoc         TYPE /agri/t_fmacs_doc,
        lt_accom         TYPE /agri/t_fmacom,
        lt_fmacrsc       TYPE /agri/t_fmacrsc,
        lt_fmac_messages TYPE /agri/t_gprolog,
        lt_activity_x    TYPE STANDARD TABLE OF /agri/fmacact INITIAL SIZE 0,
        lwa_acitm_new    TYPE /agri/s_fmacitm,
        lwa_items_layout TYPE /agri/s_fmacitm_layout,
        lwa_activity     TYPE /agri/fmacact,
        lwa_fmacrsc      TYPE /agri/t_fmacrsc,
        lwa_accom        TYPE /agri/s_fmacom,
        lwa_bdc_options  TYPE ctu_params,
        lwa_data         TYPE /agri/s_excel_sheet,
        lwa_dd03l        TYPE dd03l,
        lwa_message      TYPE /agri/s_gprolog,
        lwa_acdoc_new    TYPE /agri/s_fmacs_doc,
        lv_begda         TYPE sydatum,
        lv_begda_c10     TYPE char10,
        lv_endda         TYPE sydatum,
        lv_endda_c10     TYPE char10,
        lv_accom         TYPE /agri/fmaccom,
        lv_msgv1         TYPE sy-msgv1,
        lv_msgli         TYPE sy-msgli,
        lv_subrc         TYPE sysubrc,
        lv_aufnr         TYPE aufnr,
        lv_tplnr         TYPE /agri/gltplnr_fl,
        lv_posnr         TYPE i,
        lv_minutes       TYPE i,
        lv_h             TYPE n LENGTH 2,
        lv_m             TYPE n LENGTH 2,
        lv_s             TYPE n LENGTH 2,
        lv_time          TYPE sy-uzeit,
        lv_time_char     TYPE char0256,
        lv_num           TYPE float,
        lv_actyp_bdc     TYPE bdc_fval,
        lv_bukrs_bdc     TYPE bdc_fval,
        lv_werks_bdc     TYPE bdc_fval,
        lv_strtdat_bdc   TYPE bdc_fval,
        lv_findat_bdc    TYPE bdc_fval,
        lv_matnr_bdc     TYPE bdc_fval,
        lv_aufnr_bdc     TYPE bdc_fval,
        lv_fmacm         TYPE sy-tcode VALUE 'ZABS_FMACM',
        lv_row           TYPE /agri/s_excel_sheet-row.

  CONSTANTS: BEGIN OF c_structures,
               bdo_01  TYPE tabname  VALUE 'ZABS_STR_ACCOMPLISHMENT_SHEET1',
               bdo_in  TYPE updkz_d  VALUE 'I',
               bdo_up  TYPE updkz_d  VALUE 'U',
               success TYPE sy-msgty VALUE 'S',
             END OF c_structures.

  CONSTANTS: c_updkz_new(1)     TYPE c VALUE 'I',
             c_updkz_update(1)  TYPE c VALUE 'U',
             c_updkz_delete(1)  TYPE c VALUE 'D',
             c_updkz_old(1)     TYPE c VALUE ' ',
             c_updkz_newrow     TYPE c VALUE 'N',
             c_updkz_propose(1) TYPE c VALUE 'P'.

  CONSTANTS: BEGIN OF c_process_status,
               ctd TYPE /agri/fm_status VALUE 'CTD',
               cnf TYPE /agri/fm_status VALUE 'CNF',
               dis TYPE /agri/fm_status VALUE 'DIS',
               cls TYPE /agri/fm_status VALUE 'CLS',
               rev TYPE /agri/fm_status VALUE 'REV',
               del TYPE /agri/fm_status VALUE 'DEL',
             END OF c_process_status.

  CONSTANTS: BEGIN OF c_measurement_level,
               terrain      TYPE /agri/glaslvl VALUE 'T',
               crop_seasons TYPE /agri/glaslvl VALUE 'A',
               harvest      TYPE /agri/glaslvl VALUE 'H',
               irrigation   TYPE /agri/glaslvl VALUE 'I',
             END OF c_measurement_level.

****Message Types
  CONSTANTS : BEGIN OF c_msg_type,
                info    LIKE sy-msgty VALUE 'I',
                warning LIKE sy-msgty VALUE 'W',
                error   LIKE sy-msgty VALUE 'E',
                abend   LIKE sy-msgty VALUE 'A',
                success LIKE sy-msgty VALUE 'S',
                x       LIKE sy-msgty VALUE 'X',
              END   OF c_msg_type.

  CONSTANTS: BEGIN OF c_conftype,
               partial    TYPE zabs_del_conftyp VALUE 'P',
               complement TYPE zabs_del_conftyp VALUE 'C',
               total      TYPE zabs_del_conftyp VALUE 'T',
             END OF c_conftype.

  CONSTANTS: BEGIN OF c_rstype,
               labor TYPE /agri/fmrstype VALUE 'A',
               equnr TYPE /agri/fmrstype VALUE 'B',
             END OF c_rstype.

  FIELD-SYMBOLS: <lwa_acc_sheet> TYPE zabs_str_accomplishment_sheet1,
                 <lv_value>      TYPE any,
                 <lwa_acdoc>     TYPE /agri/s_fmacs_doc.

  DEFINE add_first_line.
*-- Verificar Linha &2.
    CLEAR lwa_message.
    lwa_message-msgid = 'ZFMFP'.
    lwa_message-msgno = '163'.
    lwa_message-msgty = 'I'.
    lwa_message-msgv1 = &1.
    APPEND lwa_message TO lt_messages.
  END-OF-DEFINITION.

  lt_table[] = it_table[].

  DELETE lt_table WHERE row = 1.
  DELETE lt_table WHERE sheet <> 1.

  "Sheet 1
  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-bdo_01
      i_sheet     = 1
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  SORT lt_dd03l BY position.

  LOOP AT lt_table INTO lwa_data WHERE sheet = 01.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_acc_sheet ASSIGNING <lwa_acc_sheet>.
      lv_row = lwa_data-row.
    ENDIF.

    READ TABLE lt_dd03l TRANSPORTING NO FIELDS
      WITH KEY position = lwa_data-column.
    IF sy-subrc EQ 0.
      LOOP AT lt_dd03l INTO lwa_dd03l FROM sy-tabix.
        IF lwa_dd03l-position NE lwa_data-column.
          EXIT.
        ENDIF.
        ASSIGN COMPONENT lwa_data-fieldname
          OF STRUCTURE <lwa_acc_sheet> TO <lv_value>.
        IF sy-subrc EQ 0.
          IF lwa_dd03l-domname NE 'MENGV13'.
            <lv_value> = lwa_data-value.
          ENDIF.

          IF lwa_dd03l-domname EQ 'DATUM'
          OR lwa_dd03l-domname EQ 'DATS'.
            <lv_value> = |{ lwa_data-value+6(4) }| &&
                         |{ lwa_data-value+3(2) }| &&
                         |{ lwa_data-value(2) }|.
          ELSEIF lwa_dd03l-domname EQ 'MENGV13'.
            TRANSLATE lwa_data-value USING ',.'.
            <lv_value> = lwa_data-value.
          ELSEIF lwa_dd03l-domname EQ 'TIMS'.
            CLEAR lv_time.
            lv_time_char = lwa_data-value.
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
          ELSEIF lwa_dd03l-domname EQ '/AGRI/GLTPLNR_FL'.
            CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
              EXPORTING
                input      = <lv_value>
              IMPORTING
                output     = <lv_value>
              EXCEPTIONS
                not_found  = 1
                not_active = 2
                OTHERS     = 3.
          ELSEIF lwa_dd03l-domname EQ 'AUFNR'.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = <lv_value>
              IMPORTING
                output = <lv_value>.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    <lwa_acc_sheet>-excel_row = lwa_data-row.
    <lwa_acc_sheet>-excel_column = lwa_data-column.
  ENDLOOP.
  CLEAR lv_row.

  IF lt_acc_sheet[] IS INITIAL.
*-- Arquivo Excel não contém dados.
    CLEAR lwa_message.
    lwa_message-msgid = 'ZFMFP'.
    lwa_message-msgno = '122'.
    lwa_message-msgty = 'E'.
    APPEND lwa_message TO lt_messages.
    RETURN.
  ELSE.
    SELECT *
      FROM /agri/fmacres
      INTO TABLE @DATA(lt_fmacres)
      FOR ALL ENTRIES IN @lt_acc_sheet
     WHERE idresource = @lt_acc_sheet-idresource
       AND rstype     = @c_rstype-labor.

    SELECT *
      FROM /agri/fmacres
      APPENDING TABLE @lt_fmacres
      FOR ALL ENTRIES IN @lt_acc_sheet
     WHERE idresource = @lt_acc_sheet-equnr
       AND rstype     = @c_rstype-equnr.

    SORT lt_fmacres BY idresource rstype.

    SELECT *
      FROM /agri/glflot
      INTO TABLE @DATA(lt_glflot)
      FOR ALL ENTRIES IN @lt_acc_sheet
     WHERE tplnr_fl = @lt_acc_sheet-tplnr.

    SORT lt_glflot BY tplnr_fl.

    SELECT *
      FROM /agri/fmacact
      INTO TABLE @lt_activity_x
      FOR ALL ENTRIES IN @lt_acc_sheet
     WHERE idactv = @lt_acc_sheet-idactve.

    SORT lt_activity_x BY idactv.

    SELECT k~aufnr, k~auart, k~autyp, k~bukrs,
           k~werks, k~kokrs, k~waers, k~loekz,
           o~gasmg, o~gamng, o~gmein, o~plnbez,
           o~plnty, o~plnnr, o~plnal, o~stlty,
           o~stlbez, o~stlst, o~stlnr
      FROM aufk AS k
      INNER JOIN afko AS o
      ON k~aufnr = o~aufnr
      INTO TABLE @DATA(lt_aufk)
      FOR ALL ENTRIES IN @lt_acc_sheet
     WHERE k~aufnr = @lt_acc_sheet-aufnr.

    SORT lt_aufk BY aufnr.

    SELECT *
      FROM /agri/fmacitm
      INTO TABLE @DATA(lt_acitm)
      FOR ALL ENTRIES IN @lt_acc_sheet
     WHERE tplnr EQ @lt_acc_sheet-tplnr
       AND aufnr EQ @lt_acc_sheet-aufnr.

    SORT lt_acitm BY aufnr strtdat findat.

    IF lt_acitm[] IS NOT INITIAL.
      SELECT *
        FROM /agri/fmachdr
        INTO TABLE @DATA(lt_achdr)
        FOR ALL ENTRIES IN @lt_acitm
       WHERE accom = @lt_acitm-accom.

      SORT lt_achdr BY werks   ASCENDING
                       strtdat ASCENDING
                       findat  ASCENDING
                       accom   DESCENDING.

      SELECT *
        FROM /agri/fmacdet
        INTO TABLE @DATA(lt_acdet)
        FOR ALL ENTRIES IN @lt_acitm
       WHERE accom = @lt_acitm-accom.

      SORT lt_acdet BY accom.
    ENDIF.
  ENDIF.

  LOOP AT lt_acc_sheet ASSIGNING <lwa_acc_sheet>.
*    READ TABLE lt_fmacres INTO DATA(lwa_fmacres)
*      WITH KEY idresource = <lwa_acc_sheet>-idresource
*               rstype     = c_rstype-labor BINARY SEARCH.
*    IF sy-subrc NE 0.
*      IF <lwa_acc_sheet>-line_w_error EQ abap_false.
*        <lwa_acc_sheet>-line_w_error = abap_true.
*        add_first_line <lwa_acc_sheet>-excel_row.
*      ENDIF.
**-- O ID de recurso &1 não existe.
*      CLEAR lwa_message.
*      lwa_message-msgid = '/AGRI/FMAC'.
*      lwa_message-msgno = 048.
*      lwa_message-msgty = 'E'.
*      lwa_message-msgv1 = <lwa_acc_sheet>-idresource .
*      APPEND lwa_message TO lt_messages.
*    ENDIF.

    READ TABLE lt_fmacres INTO DATA(lwa_fmacres)
      WITH KEY idresource = <lwa_acc_sheet>-equnr
               rstype     = c_rstype-equnr BINARY SEARCH.
    IF sy-subrc NE 0.
      IF <lwa_acc_sheet>-line_w_error EQ abap_false.
        <lwa_acc_sheet>-line_w_error = abap_true.
        add_first_line <lwa_acc_sheet>-excel_row.
      ENDIF.
*-- O equipamento &1 não existe.
      CLEAR lwa_message.
      lwa_message-msgid = '/AGRI/FMAC'.
      lwa_message-msgno = 031.
      lwa_message-msgty = 'E'.
      lwa_message-msgv1 = <lwa_acc_sheet>-equnr.
      APPEND lwa_message TO lt_messages.
    ENDIF.

    READ TABLE lt_aufk INTO DATA(lwa_aufk)
      WITH KEY aufnr = <lwa_acc_sheet>-aufnr BINARY SEARCH.
    IF sy-subrc NE 0.
      IF <lwa_acc_sheet>-line_w_error EQ abap_false.
        <lwa_acc_sheet>-line_w_error = abap_true.
        add_first_line <lwa_acc_sheet>-excel_row.
      ENDIF.
      lv_aufnr = <lwa_acc_sheet>-aufnr.
      SHIFT lv_aufnr LEFT DELETING LEADING '0'.
*-- Ordem &1 inexistente.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZFMFP'.
      lwa_message-msgno = 156.
      lwa_message-msgty = 'E'.
      lwa_message-msgv1 = lv_aufnr.
      APPEND lwa_message TO lt_messages.
    ELSE.
      <lwa_acc_sheet>-werks = lwa_aufk-werks.
      IF lwa_aufk-loekz EQ abap_true.
        IF <lwa_acc_sheet>-line_w_error EQ abap_false.
          <lwa_acc_sheet>-line_w_error = abap_true.
          add_first_line <lwa_acc_sheet>-excel_row.
        ENDIF.
        lv_aufnr = <lwa_acc_sheet>-aufnr.
        SHIFT lv_aufnr LEFT DELETING LEADING '0'.
*-- Ordem &1 marcada para eliminação.
        CLEAR lwa_message.
        lwa_message-msgid = 'ZFMFP'.
        lwa_message-msgno = 157.
        lwa_message-msgty = 'E'.
        lwa_message-msgv1 = lv_aufnr.
        APPEND lwa_message TO lt_messages.
      ENDIF.
    ENDIF.

    READ TABLE lt_glflot TRANSPORTING NO FIELDS
      WITH KEY tplnr_fl = <lwa_acc_sheet>-tplnr BINARY SEARCH.
    IF sy-subrc NE 0.
      IF <lwa_acc_sheet>-line_w_error EQ abap_false.
        <lwa_acc_sheet>-line_w_error = abap_true.
        add_first_line <lwa_acc_sheet>-excel_row.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
        EXPORTING
          input  = <lwa_acc_sheet>-tplnr
        IMPORTING
          output = lv_tplnr.

*-- Terreno &1 inexistente.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZFMFP'.
      lwa_message-msgno = 158.
      lwa_message-msgty = 'E'.
      lwa_message-msgv1 = lv_tplnr.
      APPEND lwa_message TO lt_messages.
    ENDIF.

    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                      = <lwa_acc_sheet>-strtdat
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS                    = 2.

    IF sy-subrc NE 0.
      IF <lwa_acc_sheet>-line_w_error EQ abap_false.
        <lwa_acc_sheet>-line_w_error = abap_true.
        add_first_line <lwa_acc_sheet>-excel_row.
      ENDIF.
*-- Data Inicial inválida.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZFMFP'.
      lwa_message-msgno = 160.
      lwa_message-msgty = 'E'.
      APPEND lwa_message TO lt_messages.
    ENDIF.

    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                      = <lwa_acc_sheet>-findat
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS                    = 2.

    IF sy-subrc NE 0.
      IF <lwa_acc_sheet>-line_w_error EQ abap_false.
        <lwa_acc_sheet>-line_w_error = abap_true.
        add_first_line <lwa_acc_sheet>-excel_row.
      ENDIF.
*-- Data Final inválida.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZFMFP'.
      lwa_message-msgno = 161.
      lwa_message-msgty = 'E'.
      APPEND lwa_message TO lt_messages.
    ENDIF.

    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                      = <lwa_acc_sheet>-zzbudat
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS                    = 2.

    IF sy-subrc NE 0.
      IF <lwa_acc_sheet>-line_w_error EQ abap_false.
        <lwa_acc_sheet>-line_w_error = abap_true.
        add_first_line <lwa_acc_sheet>-excel_row.
      ENDIF.
*-- Data de Lançamento inválida.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZFMFP'.
      lwa_message-msgno = 186.
      lwa_message-msgty = 'E'.
      APPEND lwa_message TO lt_messages.
    ENDIF.

    IF <lwa_acc_sheet>-findat LT <lwa_acc_sheet>-strtdat.
      IF <lwa_acc_sheet>-line_w_error EQ abap_false.
        <lwa_acc_sheet>-line_w_error = abap_true.
        add_first_line <lwa_acc_sheet>-excel_row.
      ENDIF.
*-- Data Final anterior à Data Inicial.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZFMFP'.
      lwa_message-msgno = 159.
      lwa_message-msgty = 'E'.
      APPEND lwa_message TO lt_messages.
    ENDIF.
  ENDLOOP.

  SORT lt_acc_sheet BY werks strtdat findat.
  DATA(lt_acc_sheet_x) = lt_acc_sheet[].
  DELETE lt_acc_sheet_x WHERE line_w_error EQ abap_true.
  DELETE ADJACENT DUPLICATES FROM lt_acc_sheet_x
    COMPARING werks strtdat findat.

  IF lt_acc_sheet_x[] IS INITIAL.
*-- Não existem informações válidas no arquivo selecionado.
    CLEAR lwa_message.
    lwa_message-msgid = 'ZFMFP'.
    lwa_message-msgno = 162.
    lwa_message-msgty = 'E'.
    APPEND lwa_message TO lt_messages.
  ELSE.
    lwa_bdc_options-dismode  = 'N'.
    lwa_bdc_options-nobiend  = abap_false. "mc_true.
    lwa_bdc_options-updmode  = 'S'.
    lwa_bdc_options-nobinpt  = abap_false. "mc_true.

    LOOP AT lt_acc_sheet_x INTO DATA(lwa_acc_sheet_x).
      CLEAR: mt_bdcdata[], ms_bdcdata, lt_bdc_messages[],
             lv_begda_c10, lv_begda, lv_endda_c10, lv_endda.

      DATA(lv_create) = abap_true.
      READ TABLE lt_aufk INTO lwa_aufk
        WITH KEY aufnr = lwa_acc_sheet_x-aufnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        READ TABLE lt_achdr INTO DATA(lwa_achdr)
          WITH KEY werks   = lwa_aufk-werks
                   strtdat = lwa_acc_sheet_x-strtdat
                   findat  = lwa_acc_sheet_x-findat BINARY SEARCH.
        IF sy-subrc EQ 0.
          lv_create = abap_false.
        ELSE.
          CLEAR lwa_achdr.
        ENDIF.

        CLEAR lv_accom.
        IF lv_create EQ abap_true.
          dynpro_fill( i_program = 'SAPLZFG_ACM' i_dynpro  = '0100' ).
          field_fill( i_fnam = 'BDC_OKCODE'  i_fval = '=DICH' ).

          dynpro_fill( i_program = 'SAPLZFG_ACM' i_dynpro  = '0100' ).
          field_fill( i_fnam = 'BDC_OKCODE'  i_fval = '=CREA' ).

          lv_actyp_bdc = 'TAST'.
          dynpro_fill( i_program = 'SAPLZFG_ACM' i_dynpro  = '0010' ).
          field_fill( i_fnam = 'BDC_OKCODE'  i_fval = '=ENTR' ).
          field_fill( i_fnam = 'P_ACTYP'  i_fval = lv_actyp_bdc ).

          lv_bukrs_bdc   = lwa_aufk-bukrs.
          lv_werks_bdc   = lwa_aufk-werks.

*          CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
*            EXPORTING
*              date_internal            = lwa_acc_sheet_x-strtdat
*            IMPORTING
*              date_external            = lv_begda
*            EXCEPTIONS
*              date_internal_is_invalid = 1
*              OTHERS                   = 2.
          CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
            EXPORTING
              date_internal            = lwa_acc_sheet_x-strtdat
            IMPORTING
              date_external            = lv_begda_c10
            EXCEPTIONS
              date_internal_is_invalid = 1
              OTHERS                   = 2.

          lv_begda_c10+6(2) = lv_begda_c10+8(2).
          lv_begda = lv_begda_c10(8).

*          CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
*            EXPORTING
*              date_internal            = lwa_acc_sheet_x-findat
*            IMPORTING
*              date_external            = lv_endda
*            EXCEPTIONS
*              date_internal_is_invalid = 1
*              OTHERS                   = 2.
          CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
            EXPORTING
              date_internal            = lwa_acc_sheet_x-findat
            IMPORTING
              date_external            = lv_endda_c10
            EXCEPTIONS
              date_internal_is_invalid = 1
              OTHERS                   = 2.

          lv_endda_c10+6(2) = lv_endda_c10+8(2).
          lv_endda = lv_endda_c10(8).

          lv_strtdat_bdc = lv_begda.
          lv_findat_bdc  = lv_endda.
          lv_matnr_bdc   = lwa_aufk-plnbez.
          lv_aufnr_bdc   = lwa_aufk-aufnr.

          dynpro_fill( i_program = 'SAPLZFG_ACM' i_dynpro  = '0010' ).
          field_fill( i_fnam = 'BDC_OKCODE'   i_fval = '=NONE' ).
          field_fill( i_fnam = 'P_ACTYP'      i_fval = lv_actyp_bdc ).
          field_fill( i_fnam = 'P_BUKRS'      i_fval = lv_bukrs_bdc ).
          field_fill( i_fnam = 'P_WERKS'      i_fval = lv_werks_bdc ).
          field_fill( i_fnam = 'P_STRDAT'     i_fval = lv_strtdat_bdc ).
          field_fill( i_fnam = 'P_FINDAT'     i_fval = lv_findat_bdc ).
          field_fill( i_fnam = 'P_MAT'        i_fval = lv_matnr_bdc ).
          field_fill( i_fnam = 'SO_AUFNR-LOW' i_fval = lv_aufnr_bdc ).

          dynpro_fill( i_program = 'SAPLZFG_ACM' i_dynpro  = '0010' ).
          field_fill( i_fnam = 'BDC_OKCODE'   i_fval = '=CRET' ).
          field_fill( i_fnam = 'P_ACTYP'      i_fval = lv_actyp_bdc ).
          field_fill( i_fnam = 'P_BUKRS'      i_fval = lv_bukrs_bdc ).
          field_fill( i_fnam = 'P_WERKS'      i_fval = lv_werks_bdc ).
          field_fill( i_fnam = 'P_STRDAT'     i_fval = lv_strtdat_bdc ).
          field_fill( i_fnam = 'P_FINDAT'     i_fval = lv_findat_bdc ).
          field_fill( i_fnam = 'P_MAT'        i_fval = lv_matnr_bdc ).
          field_fill( i_fnam = 'SO_AUFNR-LOW' i_fval = lv_aufnr_bdc ).

          dynpro_fill( i_program = 'SAPLZFG_ACM' i_dynpro  = '0100' ).
          field_fill( i_fnam = 'BDC_OKCODE'   i_fval = '=SAVE' ).

          REFRESH: lt_bdc_messages[].
          CALL METHOD /agri/cl_global_services=>transaction_call_process
            EXPORTING
              i_tcode                = lv_fmacm
              i_use_bdc_data         = mc_true
              is_bdc_options         = lwa_bdc_options
              it_bdcdata             = mt_bdcdata[]
            IMPORTING
              et_bdc_messages        = lt_bdc_messages
            EXCEPTIONS
              invalid_parameters     = 1
              call_transaction_error = 2
              OTHERS                 = 3.

          LOOP AT lt_bdc_messages INTO DATA(lwa_bdc_msg).
            CLEAR lwa_message.
            lwa_message-msgty   = lwa_bdc_msg-msgtyp.
            lwa_message-msgid   = lwa_bdc_msg-msgid.
            lwa_message-msgno   = lwa_bdc_msg-msgnr.
            lwa_message-msgv1   = lwa_bdc_msg-msgv1.
            lwa_message-msgv2   = lwa_bdc_msg-msgv2.
            lwa_message-msgv3   = lwa_bdc_msg-msgv3.
            lwa_message-msgv4   = lwa_bdc_msg-msgv4.
            APPEND lwa_message TO lt_messages.
          ENDLOOP.

          READ TABLE lt_bdc_messages TRANSPORTING NO FIELDS
            WITH KEY msgtyp = c_msg_type-error.
          IF sy-subrc NE 0.
            READ TABLE lt_bdc_messages INTO lwa_bdc_msg
              WITH KEY msgtyp = c_msg_type-success.
            IF sy-subrc EQ 0.
              lv_accom = lwa_bdc_msg-msgv1(10).
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = lv_accom
                IMPORTING
                  output = lv_accom.
            ENDIF.
          ENDIF.
        ENDIF.

        IF lv_accom IS INITIAL.
          lv_accom = lwa_achdr-accom.
        ENDIF.

        IF lv_accom IS NOT INITIAL.
          CLEAR: lv_subrc, lv_msgv1, lv_msgli.
          CALL FUNCTION 'ENQUEUE_/AGRI/EZ_FMAC'
            EXPORTING
              accom          = lv_accom
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.

          IF sy-subrc <> 0.
            lv_subrc = sy-subrc.
            lv_msgv1 = sy-msgv1.
****Document &1 is locked by User &2.
            MESSAGE i017(/agri/fmac) WITH lv_accom lv_msgv1 INTO lv_msgli.
            sy-msgli = lv_msgli.
            CLEAR lwa_message.
            lwa_message-msgty   = 'E'.
            lwa_message-msgid   = '/AGRI/FMAC'.
            lwa_message-msgno   = 017.
            lwa_message-msgv1   = lv_accom.
            lwa_message-msgv2   = lv_msgv1.
            APPEND lwa_message TO lt_messages.
          ENDIF.

          REFRESH: lt_accom, lt_acitm_new, lt_fmac_messages.
          CLEAR: lwa_acdoc_new, lv_posnr, lv_minutes.
          lwa_accom = lv_accom.
          APPEND lwa_accom TO lt_accom.

*-- Fetch the Accomplishment data
          CALL FUNCTION '/AGRI/FMAC_VIEW'
            EXPORTING
              it_accom       = lt_accom
            IMPORTING
              et_acdoc       = lt_acdoc
            EXCEPTIONS
              no_data_exists = 1
              OTHERS         = 2.

          IF lt_acdoc[] IS NOT INITIAL.
            IF lv_create EQ abap_false.
              READ TABLE lt_acdoc INTO DATA(lwa_acdoc) INDEX 1.
              IF sy-subrc EQ 0.
                lt_acitm_new[] = lwa_acdoc-x-acitm[].
                DATA(lt_acitm_y) = lwa_acdoc-x-acitm[].
                SORT lt_acitm_y BY posnr DESCENDING.
                READ TABLE lt_acitm_y INTO DATA(lwa_acitm_y) INDEX 1.
                IF sy-subrc EQ 0.
                  lv_posnr = lwa_acitm_y-posnr.
                ENDIF.
              ENDIF.
            ENDIF.

            READ TABLE lt_acc_sheet INTO DATA(lwa_acc_sheet)
              WITH KEY werks   = lwa_acc_sheet_x-werks
                       strtdat = lwa_acc_sheet_x-strtdat
                       findat  = lwa_acc_sheet_x-findat BINARY SEARCH.

            WHILE sy-subrc EQ 0.
              DATA(lv_tabix) = sy-tabix + 1.
              CLEAR lwa_acitm_new.

              CLEAR lwa_aufk.
              READ TABLE lt_aufk INTO lwa_aufk
                WITH KEY aufnr = lwa_acc_sheet-aufnr BINARY SEARCH.
              ADD 1 TO lv_posnr.
              lwa_acitm_new-accom      = lv_accom.
              lwa_acitm_new-posnr      = lv_posnr.
              lwa_acitm_new-tplnr      = lwa_acc_sheet-tplnr.
              lwa_acitm_new-tmatnr     = lwa_aufk-plnbez.
              lwa_acitm_new-aufnr      = lwa_aufk-aufnr.
              lwa_acitm_new-equnr      = lwa_acc_sheet-equnr.
              lwa_acitm_new-idresource = lwa_acc_sheet-idresource.
              lwa_acitm_new-zzmcnim    = lwa_acc_sheet-zzmcnim.
              lwa_acitm_new-zzbudat    = lwa_acc_sheet-zzbudat.

              SELECT arbpl UP TO 1 ROWS
                FROM /agri/fmacres
                INTO lwa_acitm_new-arbpl
               WHERE idresource = lwa_acitm_new-equnr.
              ENDSELECT.

              lwa_acitm_new-strtdat = lwa_acc_sheet-strtdat.
              lwa_acitm_new-strttim = lwa_acc_sheet-strttim.
              lwa_acitm_new-findat  = lwa_acc_sheet-findat.
              lwa_acitm_new-fintim  = lwa_acc_sheet-fintim.

              CALL FUNCTION '/AGRI/G_DELTA_TIME_DAY_HOUR'
                EXPORTING
                  i_t1      = lwa_acitm_new-strttim
                  i_t2      = lwa_acitm_new-fintim
                  i_d1      = lwa_acitm_new-strtdat
                  i_d2      = lwa_acitm_new-findat
                IMPORTING
                  e_minutes = lv_minutes.

              lwa_acitm_new-duran     = lv_minutes / 60.
              lwa_acitm_new-meins     = 'STD'.
              lwa_acitm_new-idactve   = lwa_acc_sheet-idactve.
              lwa_acitm_new-zzconftyp = lwa_acc_sheet-zzconftyp.

              DATA(lv_processed) = abap_false.
              IF lwa_acitm_new-zzconftyp EQ c_conftype-complement
              OR lwa_acitm_new-zzconftyp EQ c_conftype-total.
                READ TABLE lt_acitm_all TRANSPORTING NO FIELDS
                  WITH KEY aufnr     = lwa_acitm_new-aufnr
                           zzconftyp = lwa_acitm_new-zzconftyp.
                IF sy-subrc EQ 0.
                  lv_processed = abap_true.
                ENDIF.
              ENDIF.

              READ TABLE lt_activity_x INTO DATA(lwa_activity_x)
                WITH KEY idactv = lwa_acitm_new-idactve BINARY SEARCH.
              IF sy-subrc EQ 0.
                IF lwa_activity_x-bill EQ 'YES'.
                  CASE lwa_acitm_new-zzconftyp.
                    WHEN c_conftype-partial.
                      CLEAR lwa_acitm_new-zzlmnga.
                      lwa_acitm_new-zzmeinh = lwa_aufk-gmein.
                    WHEN c_conftype-complement.
                      lwa_acitm_new-zzlmnga = lwa_aufk-gamng.
                      lwa_acitm_new-zzmeinh = lwa_aufk-gmein.
                    WHEN c_conftype-total.
                      lwa_acitm_new-zzlmnga = lwa_aufk-gamng.
                      lwa_acitm_new-zzmeinh = lwa_aufk-gmein.
                  ENDCASE.
                  IF lv_processed EQ abap_true.
                    CLEAR lwa_acitm_new-zzlmnga.
                  ENDIF.
                ENDIF.
              ENDIF.

              IF lwa_acitm_new-idactve IS NOT INITIAL.
                CLEAR lwa_items_layout.
*--- {Call without data to make memory areas available...
                PERFORM activity_check IN PROGRAM saplzfg_acm
                                            USING c_rstype-equnr
                                         CHANGING lwa_items_layout
                                                  lt_activity
                                                  lt_fmacrsc IF FOUND.
                READ TABLE lt_acdoc INTO lwa_acdoc INDEX 1.
                IF sy-subrc EQ 0.
                  ASSIGN ('(SAPLZFG_ACM)GS_ACDOC_INFOCUS') TO <lwa_acdoc>.
                  IF sy-subrc EQ 0.
                    <lwa_acdoc> = lwa_acdoc.
                  ENDIF.
                ENDIF.
*--- Call without data to make memory areas available...}
                MOVE-CORRESPONDING lwa_acitm_new TO lwa_items_layout.
                PERFORM activity_check IN PROGRAM saplzfg_acm
                                            USING c_rstype-equnr
                                         CHANGING lwa_items_layout
                                                  lt_activity
                                                  lt_fmacrsc IF FOUND.
                READ TABLE lt_activity ASSIGNING FIELD-SYMBOL(<lwa_activity>)
                  WITH KEY idactv = lwa_acitm_new-idactve
                           rstype = c_rstype-equnr.
                IF sy-subrc EQ 0.
                  PERFORM activity_mein_change IN PROGRAM saplzfg_acm
                                                    USING lt_fmacrsc
                                                 CHANGING lwa_items_layout
                                                         <lwa_activity> IF FOUND.

                  lwa_acitm_new-menge = lwa_items_layout-menge.
                  lwa_acitm_new-qmein = lwa_items_layout-qmein.
                ENDIF.
              ENDIF.

              lwa_acitm_new-updkz   = c_updkz_new.
              lwa_acitm_new-status  = c_process_status-ctd.
              lwa_acitm_new-zztplnr = lwa_acc_sheet-tplnr.
              lwa_acitm_new-status  = c_process_status-ctd.
              APPEND lwa_acitm_new TO lt_acitm_new.
              APPEND lwa_acitm_new TO lt_acitm_all.

              READ TABLE lt_acc_sheet INTO lwa_acc_sheet
                INDEX lv_tabix COMPARING werks strtdat findat.
            ENDWHILE.

            IF lt_acitm_new[] IS NOT INITIAL.
              READ TABLE lt_acdoc ASSIGNING <lwa_acdoc> INDEX 1.
              IF sy-subrc EQ 0.
                <lwa_acdoc>-updkz = c_updkz_update.
                <lwa_acdoc>-x-achdr-updkz = c_updkz_update.
                <lwa_acdoc>-x-acitm[] = lt_acitm_new[].

*-- Create the new Accomplishment sheet
                CALL FUNCTION 'ZABS_FMAC_CREATE'
                  EXPORTING
                    is_fmacdoc  = <lwa_acdoc>
                    iv_mcnf     = abap_true
                  IMPORTING
                    es_fmac_doc = lwa_acdoc_new
                    et_messages = lt_fmac_messages.

                LOOP AT lt_fmac_messages INTO DATA(lwa_fmac_msg).
                  CLEAR lwa_message.
                  lwa_message-msgty   = lwa_fmac_msg-msgty.
                  lwa_message-msgid   = lwa_fmac_msg-msgid.
                  lwa_message-msgno   = lwa_fmac_msg-msgno.
                  lwa_message-msgv1   = lwa_fmac_msg-msgv1.
                  lwa_message-msgv2   = lwa_fmac_msg-msgv2.
                  lwa_message-msgv3   = lwa_fmac_msg-msgv3.
                  lwa_message-msgv4   = lwa_fmac_msg-msgv4.
                  APPEND lwa_message TO lt_messages.
                ENDLOOP.

*                READ TABLE lt_fmac_messages INTO DATA(lwa_fmac_msg)
*                  WITH KEY msgty = 'E'.
*                IF sy-subrc EQ 0.
*                  LOOP AT lt_fmac_messages INTO lwa_fmac_msg.
*                    CLEAR lwa_message.
*                    lwa_message-msgty   = lwa_fmac_msg-msgty.
*                    lwa_message-msgid   = lwa_fmac_msg-msgid.
*                    lwa_message-msgno   = lwa_fmac_msg-msgno.
*                    lwa_message-msgv1   = lwa_fmac_msg-msgv1.
*                    lwa_message-msgv2   = lwa_fmac_msg-msgv2.
*                    lwa_message-msgv3   = lwa_fmac_msg-msgv3.
*                    lwa_message-msgv4   = lwa_fmac_msg-msgv4.
*                    APPEND lwa_message TO lt_messages.
*                  ENDLOOP.
*                ELSE.
*                  REFRESH lt_fmac_messages.
*                  <lwa_acdoc> = lwa_acdoc_new.
*                  CLEAR lwa_acdoc_new.
*                  CALL FUNCTION 'ZABS_FMAC_CONFIRMATION'
*                    EXPORTING
*                      is_fmacdoc  = <lwa_acdoc>
*                      iv_mcnf     = abap_true
*                    IMPORTING
*                      es_fmac_doc = lwa_acdoc_new
*                      et_messages = lt_fmac_messages.
*
*                  <lwa_acdoc> = lwa_acdoc_new.
*
*                  LOOP AT lt_fmac_messages INTO lwa_fmac_msg.
*                    CLEAR lwa_message.
*                    lwa_message-msgty   = lwa_fmac_msg-msgty.
*                    lwa_message-msgid   = lwa_fmac_msg-msgid.
*                    lwa_message-msgno   = lwa_fmac_msg-msgno.
*                    lwa_message-msgv1   = lwa_fmac_msg-msgv1.
*                    lwa_message-msgv2   = lwa_fmac_msg-msgv2.
*                    lwa_message-msgv3   = lwa_fmac_msg-msgv3.
*                    lwa_message-msgv4   = lwa_fmac_msg-msgv4.
*                    APPEND lwa_message TO lt_messages.
*                  ENDLOOP.
*                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.

          CALL FUNCTION 'DEQUEUE_/AGRI/EZ_FMAC'
            EXPORTING
              accom = lv_accom.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  et_messages[] = lt_messages[].

ENDMETHOD.


METHOD ACCOMPLISHMENT_SHEET_UPDATE.

  DATA: lt_table         TYPE /agri/t_excel_sheet,
        lt_dd03l         TYPE thrpad_erd_dd03l,
        lt_messages      TYPE /agri/t_gprolog,
        lt_acc_sheet     TYPE STANDARD TABLE OF zabs_str_accomplishment_sheet2 INITIAL SIZE 0,
        lt_acdoc         TYPE /agri/t_fmacs_doc,
        lt_accom         TYPE /agri/t_fmacom,
        lt_fmac_messages TYPE /agri/t_gprolog,
        lwa_accom        TYPE /agri/s_fmacom,
        lwa_data         TYPE /agri/s_excel_sheet,
        lwa_dd03l        TYPE dd03l,
        lwa_message      TYPE /agri/s_gprolog,
        lwa_acdoc_new    TYPE /agri/s_fmacs_doc,
        lv_accom         TYPE /agri/fmaccom,
        lv_msgv1         TYPE sy-msgv1,
        lv_msgli         TYPE sy-msgli,
        lv_subrc         TYPE sysubrc,
        lv_minutes       TYPE i,
        lv_h             TYPE n LENGTH 2,
        lv_m             TYPE n LENGTH 2,
        lv_s             TYPE n LENGTH 2,
        lv_time          TYPE sy-uzeit,
        lv_time_char     TYPE char0256,
        lv_num           TYPE float,
        lv_error         TYPE abap_bool,
        lv_fmacm         TYPE sy-tcode VALUE 'ZABS_FMACM',
        lv_row           TYPE /agri/s_excel_sheet-row.

  CONSTANTS: BEGIN OF c_structures,
               bdo_01  TYPE tabname  VALUE 'ZABS_STR_ACCOMPLISHMENT_SHEET2',
               bdo_in  TYPE updkz_d  VALUE 'I',
               bdo_up  TYPE updkz_d  VALUE 'U',
               success TYPE sy-msgty VALUE 'S',
             END OF c_structures.

  CONSTANTS: c_updkz_new(1)     TYPE c VALUE 'I',
             c_updkz_update(1)  TYPE c VALUE 'U',
             c_updkz_delete(1)  TYPE c VALUE 'D',
             c_updkz_old(1)     TYPE c VALUE ' ',
             c_updkz_newrow     TYPE c VALUE 'N',
             c_updkz_propose(1) TYPE c VALUE 'P'.

  CONSTANTS: BEGIN OF c_process_status,
               ctd TYPE /agri/fm_status VALUE 'CTD',
               cnf TYPE /agri/fm_status VALUE 'CNF',
               dis TYPE /agri/fm_status VALUE 'DIS',
               cls TYPE /agri/fm_status VALUE 'CLS',
               rev TYPE /agri/fm_status VALUE 'REV',
               del TYPE /agri/fm_status VALUE 'DEL',
             END OF c_process_status.

  CONSTANTS: BEGIN OF c_measurement_level,
               terrain      TYPE /agri/glaslvl VALUE 'T',
               crop_seasons TYPE /agri/glaslvl VALUE 'A',
               harvest      TYPE /agri/glaslvl VALUE 'H',
               irrigation   TYPE /agri/glaslvl VALUE 'I',
             END OF c_measurement_level.

****Message Types
  CONSTANTS : BEGIN OF c_msg_type,
                info    LIKE sy-msgty VALUE 'I',
                warning LIKE sy-msgty VALUE 'W',
                error   LIKE sy-msgty VALUE 'E',
                abend   LIKE sy-msgty VALUE 'A',
                success LIKE sy-msgty VALUE 'S',
                x       LIKE sy-msgty VALUE 'X',
              END   OF c_msg_type.

  CONSTANTS: BEGIN OF c_conftype,
               partial    TYPE zabs_del_conftyp VALUE 'P',
               complement TYPE zabs_del_conftyp VALUE 'C',
               total      TYPE zabs_del_conftyp VALUE 'T',
             END OF c_conftype.

  CONSTANTS: BEGIN OF c_rstype,
               labor TYPE /agri/fmrstype VALUE 'A',
               equnr TYPE /agri/fmrstype VALUE 'B',
             END OF c_rstype.

  FIELD-SYMBOLS: <lwa_acc_sheet> TYPE zabs_str_accomplishment_sheet2,
                 <lv_value>      TYPE any,
                 <lwa_acdoc>     TYPE /agri/s_fmacs_doc.

  DEFINE add_first_line.
*-- Verificar Linha &2.
    CLEAR lwa_message.
    lwa_message-msgid = 'ZFMFP'.
    lwa_message-msgno = '163'.
    lwa_message-msgty = 'I'.
    lwa_message-msgv1 = &1.
    APPEND lwa_message TO lt_messages.
  END-OF-DEFINITION.

  lt_table[] = it_table[].

  DELETE lt_table WHERE row = 1.
  DELETE lt_table WHERE sheet <> 1.

*-- Sheet 1
  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-bdo_01
      i_sheet     = 1
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  SORT lt_dd03l BY position.

  LOOP AT lt_table INTO lwa_data WHERE sheet = 01.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_acc_sheet ASSIGNING <lwa_acc_sheet>.
      lv_row = lwa_data-row.
    ENDIF.

    READ TABLE lt_dd03l TRANSPORTING NO FIELDS
      WITH KEY position = lwa_data-column.
    IF sy-subrc EQ 0.
      LOOP AT lt_dd03l INTO lwa_dd03l FROM sy-tabix.
        IF lwa_dd03l-position NE lwa_data-column.
          EXIT.
        ENDIF.
        ASSIGN COMPONENT lwa_data-fieldname
          OF STRUCTURE <lwa_acc_sheet> TO <lv_value>.
        IF sy-subrc EQ 0.
          IF lwa_dd03l-domname NE 'MENGV13'.
            <lv_value> = lwa_data-value.
          ENDIF.

          IF lwa_dd03l-domname EQ 'DATUM'
          OR lwa_dd03l-domname EQ 'DATS'.
            <lv_value> = |{ lwa_data-value+6(4) }| &&
                         |{ lwa_data-value+3(2) }| &&
                         |{ lwa_data-value(2) }|.
          ELSEIF lwa_dd03l-domname EQ 'MENGV13'.
            TRANSLATE lwa_data-value USING ',.'.
            <lv_value> = lwa_data-value.
          ELSEIF lwa_dd03l-domname EQ 'TIMS'.
            CLEAR lv_time.
            lv_time_char = lwa_data-value.
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
          ELSEIF lwa_dd03l-domname EQ '/AGRI/GLTPLNR_FL'.
            CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
              EXPORTING
                input      = <lv_value>
              IMPORTING
                output     = <lv_value>
              EXCEPTIONS
                not_found  = 1
                not_active = 2
                OTHERS     = 3.
          ELSEIF lwa_dd03l-domname EQ '/AGRI/FMACCOM'
              OR lwa_dd03l-domname EQ 'AUFNR'.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = <lv_value>
              IMPORTING
                output = <lv_value>.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    <lwa_acc_sheet>-excel_row = lwa_data-row.
    <lwa_acc_sheet>-excel_column = lwa_data-column.
  ENDLOOP.
  CLEAR lv_row.

  IF lt_acc_sheet[] IS INITIAL.
*-- Arquivo Excel não contém dados.
    CLEAR lwa_message.
    lwa_message-msgid = 'ZFMFP'.
    lwa_message-msgno = '122'.
    lwa_message-msgty = 'E'.
    APPEND lwa_message TO lt_messages.
    lv_error = abap_true.
  ELSE.
    SELECT *
      FROM /agri/fmacres
      INTO TABLE @DATA(lt_fmacres)
      FOR ALL ENTRIES IN @lt_acc_sheet
     WHERE idresource = @lt_acc_sheet-equnr
       AND rstype     = @c_rstype-equnr.

    IF sy-subrc NE 0.
*-- As frotas informadas são inválidas.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZFMFP'.
      lwa_message-msgno = '256'.
      lwa_message-msgty = 'E'.
      APPEND lwa_message TO lt_messages.
      lv_error = abap_true.
    ELSE.
      SORT lt_fmacres BY idresource rstype.

      SELECT *
        FROM /agri/fmachdr
        INTO TABLE @DATA(lt_achdr)
        FOR ALL ENTRIES IN @lt_acc_sheet
       WHERE accom = @lt_acc_sheet-accom.

      IF sy-subrc EQ 0.
        SORT lt_achdr BY accom.

        SELECT *
          FROM /agri/fmacitm
          INTO TABLE @DATA(lt_acitm)
          FOR ALL ENTRIES IN @lt_achdr
         WHERE accom EQ @lt_achdr-accom.

        IF sy-subrc EQ 0.
          SORT lt_acitm BY accom posnr.

          SELECT *
            FROM /agri/fmacres
            INTO TABLE @DATA(lt_centro)
            FOR ALL ENTRIES IN @lt_acitm
           WHERE arbpl  = @lt_acitm-arbpl
             AND rstype = @c_rstype-equnr.

          SORT lt_centro BY idresource arbpl.
        ELSE.
*-- As folhas BDO informadas não possuem itens.
          CLEAR lwa_message.
          lwa_message-msgid = 'ZFMFP'.
          lwa_message-msgno = '255'.
          lwa_message-msgty = 'E'.
          APPEND lwa_message TO lt_messages.
          lv_error = abap_true.
        ENDIF.
      ELSE.
*-- As folhas BDO informadas não existem.
        CLEAR lwa_message.
        lwa_message-msgid = 'ZFMFP'.
        lwa_message-msgno = '254'.
        lwa_message-msgty = 'E'.
        APPEND lwa_message TO lt_messages.
        lv_error = abap_true.
      ENDIF.
    ENDIF.
  ENDIF.

  IF lv_error EQ abap_false.
    LOOP AT lt_acc_sheet ASSIGNING <lwa_acc_sheet>.
      IF <lwa_acc_sheet>-accom IS INITIAL.
        IF <lwa_acc_sheet>-line_w_error EQ abap_false.
          <lwa_acc_sheet>-line_w_error = abap_true.
          add_first_line <lwa_acc_sheet>-excel_row.
        ENDIF.
*-- A folha de confirmação BDO deve ser informada.
        CLEAR lwa_message.
        lwa_message-msgid = 'ZFMFP'.
        lwa_message-msgno = 259.
        lwa_message-msgty = 'E'.
        APPEND lwa_message TO lt_messages.
      ELSE.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <lwa_acc_sheet>-accom
          IMPORTING
            output = <lwa_acc_sheet>-accom.

        READ TABLE lt_achdr INTO DATA(lwa_achdr)
          WITH KEY accom = <lwa_acc_sheet>-accom BINARY SEARCH.
        IF sy-subrc NE 0.
          IF <lwa_acc_sheet>-line_w_error EQ abap_false.
            <lwa_acc_sheet>-line_w_error = abap_true.
            add_first_line <lwa_acc_sheet>-excel_row.
          ENDIF.
*-- A folha de confirmação BDO &1 não existe.
          CLEAR lwa_message.
          lwa_message-msgid = 'ZFMFP'.
          lwa_message-msgno = 258.
          lwa_message-msgty = 'E'.
          lwa_message-msgv1 = <lwa_acc_sheet>-accom.
          APPEND lwa_message TO lt_messages.
        ENDIF.
      ENDIF.

      IF <lwa_acc_sheet>-posnr IS INITIAL.
        IF <lwa_acc_sheet>-line_w_error EQ abap_false.
          <lwa_acc_sheet>-line_w_error = abap_true.
          add_first_line <lwa_acc_sheet>-excel_row.
        ENDIF.
*-- A posição do item é de preenchimento obrigatório.
        CLEAR lwa_message.
        lwa_message-msgid = 'ZFMFP'.
        lwa_message-msgno = 260.
        lwa_message-msgty = 'E'.
        APPEND lwa_message TO lt_messages.
      ELSE.
        IF <lwa_acc_sheet>-accom IS NOT INITIAL.
          READ TABLE lt_acitm INTO DATA(lwa_acitm)
            WITH KEY accom = <lwa_acc_sheet>-accom
                     posnr = <lwa_acc_sheet>-posnr BINARY SEARCH.
          IF sy-subrc NE 0.
            IF <lwa_acc_sheet>-line_w_error EQ abap_false.
              <lwa_acc_sheet>-line_w_error = abap_true.
              add_first_line <lwa_acc_sheet>-excel_row.
            ENDIF.
*-- Folha BDO &1 não possui item &2.
            CLEAR lwa_message.
            lwa_message-msgid = 'ZFMFP'.
            lwa_message-msgno = 261.
            lwa_message-msgty = 'E'.
            lwa_message-msgv1 = <lwa_acc_sheet>-accom.
            lwa_message-msgv2 = <lwa_acc_sheet>-posnr.
            APPEND lwa_message TO lt_messages.
          ENDIF.
        ENDIF.
      ENDIF.

      READ TABLE lt_fmacres INTO DATA(lwa_fmacres)
        WITH KEY idresource = <lwa_acc_sheet>-equnr
                 rstype     = c_rstype-equnr BINARY SEARCH.
      IF sy-subrc NE 0.
        IF <lwa_acc_sheet>-line_w_error EQ abap_false.
          <lwa_acc_sheet>-line_w_error = abap_true.
          add_first_line <lwa_acc_sheet>-excel_row.
        ENDIF.
*-- A frota &1 não existe.
        CLEAR lwa_message.
        lwa_message-msgid = 'ZFMFP'.
        lwa_message-msgno = 257.
        lwa_message-msgty = 'E'.
        lwa_message-msgv1 = <lwa_acc_sheet>-equnr.
        APPEND lwa_message TO lt_messages.
      ENDIF.
    ENDLOOP.

    SORT lt_acc_sheet BY accom posnr.
    DATA(lt_acc_sheet_x) = lt_acc_sheet[].
    DELETE lt_acc_sheet_x WHERE line_w_error EQ abap_true.
    DELETE lt_acc_sheet WHERE line_w_error EQ abap_true.
    DELETE ADJACENT DUPLICATES FROM lt_acc_sheet_x COMPARING accom.

    IF lt_acc_sheet_x[] IS INITIAL.
*-- Não existem informações válidas no arquivo selecionado.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZFMFP'.
      lwa_message-msgno = 162.
      lwa_message-msgty = 'E'.
      APPEND lwa_message TO lt_messages.
    ELSE.
      LOOP AT lt_acc_sheet_x INTO DATA(lwa_acc_sheet_x).
        READ TABLE lt_achdr INTO lwa_achdr
          WITH KEY accom = lwa_acc_sheet_x-accom BINARY SEARCH.

        IF sy-subrc EQ 0.
          lv_accom = lwa_achdr-accom.

          CLEAR: lv_subrc, lv_msgv1, lv_msgli.
          CALL FUNCTION 'ENQUEUE_/AGRI/EZ_FMAC'
            EXPORTING
              accom          = lv_accom
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.

          IF sy-subrc <> 0.
            lv_subrc = sy-subrc.
            lv_msgv1 = sy-msgv1.
****Document &1 is locked by User &2.
            MESSAGE i017(/agri/fmac) WITH lv_accom lv_msgv1 INTO lv_msgli.
            sy-msgli = lv_msgli.
            CLEAR lwa_message.
            lwa_message-msgty   = 'E'.
            lwa_message-msgid   = '/AGRI/FMAC'.
            lwa_message-msgno   = 017.
            lwa_message-msgv1   = lv_accom.
            lwa_message-msgv2   = lv_msgv1.
            APPEND lwa_message TO lt_messages.
          ENDIF.

          REFRESH: lt_accom.
          CLEAR: lwa_acdoc_new.
          lwa_accom = lv_accom.
          APPEND lwa_accom TO lt_accom.

*-- Fetch the Accomplishment data
          CALL FUNCTION '/AGRI/FMAC_VIEW'
            EXPORTING
              it_accom       = lt_accom
            IMPORTING
              et_acdoc       = lt_acdoc
            EXCEPTIONS
              no_data_exists = 1
              OTHERS         = 2.

          DATA(lv_accom_upd) = abap_false.
          READ TABLE lt_acdoc ASSIGNING <lwa_acdoc> INDEX 1.
          IF sy-subrc EQ 0.
            READ TABLE lt_acc_sheet INTO DATA(lwa_acc_sheet)
              WITH KEY accom = lwa_acc_sheet_x-accom BINARY SEARCH.

            WHILE sy-subrc EQ 0.
              DATA(lv_tabix) = sy-tabix + 1.

              READ TABLE <lwa_acdoc>-x-acitm ASSIGNING FIELD-SYMBOL(<lwa_acitm_upd>)
                WITH KEY accom = lwa_acc_sheet-accom
                         posnr = lwa_acc_sheet-posnr.
              IF sy-subrc EQ 0.
                READ TABLE lt_centro INTO DATA(lwa_centro)
                  WITH KEY idresource = lwa_acc_sheet-equnr
                           arbpl      = <lwa_acitm_upd>-arbpl
                           rstype     = c_rstype-equnr BINARY SEARCH.
                IF sy-subrc NE 0.
                  IF lwa_acc_sheet-line_w_error EQ abap_false.
*                    lwa_acc_sheet-line_w_error = abap_true.
                    add_first_line lwa_acc_sheet-excel_row.
                  ENDIF.
*-- A frota &1 não existe no centro de trabalho &2.
                  CLEAR lwa_message.
                  lwa_message-msgid = 'ZFMFP'.
                  lwa_message-msgno = 264.
                  lwa_message-msgty = 'E'.
                  lwa_message-msgv1 = <lwa_acc_sheet>-equnr.
                  lwa_message-msgv1 = <lwa_acitm_upd>-arbpl.
                  APPEND lwa_message TO lt_messages.
                ELSE.
                  lv_accom_upd = abap_true.
                  <lwa_acitm_upd>-equnr = lwa_acc_sheet-equnr.
                  <lwa_acitm_upd>-updkz = c_updkz_update.
                ENDIF.
              ENDIF.

              READ TABLE lt_acc_sheet INTO lwa_acc_sheet
                INDEX lv_tabix COMPARING accom.
            ENDWHILE.

            IF lv_accom_upd = abap_true.
              <lwa_acdoc>-updkz = c_updkz_update.
              <lwa_acdoc>-x-achdr-updkz = c_updkz_update.

*-- Update Accomplishment Sheet
              CALL FUNCTION 'ZABS_FMAC_CREATE'
                EXPORTING
                  is_fmacdoc  = <lwa_acdoc>
                  iv_mcnf     = abap_true
                IMPORTING
                  es_fmac_doc = lwa_acdoc_new
                  et_messages = lt_fmac_messages.

              LOOP AT lt_fmac_messages INTO DATA(lwa_fmac_msg).
                CLEAR lwa_message.
                lwa_message-msgty   = lwa_fmac_msg-msgty.
                lwa_message-msgid   = lwa_fmac_msg-msgid.
                lwa_message-msgno   = lwa_fmac_msg-msgno.
                lwa_message-msgv1   = lwa_fmac_msg-msgv1.
                lwa_message-msgv2   = lwa_fmac_msg-msgv2.
                lwa_message-msgv3   = lwa_fmac_msg-msgv3.
                lwa_message-msgv4   = lwa_fmac_msg-msgv4.
                APPEND lwa_message TO lt_messages.
              ENDLOOP.
            ENDIF.
          ENDIF.

          CALL FUNCTION 'DEQUEUE_/AGRI/EZ_FMAC'
            EXPORTING
              accom = lv_accom.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

  et_messages[] = lt_messages[].

ENDMETHOD.


METHOD ADDITIONAL_FIELDS_POPULATE.

  DATA: lt_table        TYPE /agri/t_excel_sheet,
        lwa_data        TYPE /agri/s_excel_sheet,
        lt_dd03l        TYPE thrpad_erd_dd03l,
        lwa_dd03l       TYPE dd03l,
        lt_messages     TYPE /agri/t_gprolog,
        lwa_message     TYPE /agri/s_gprolog,
        lt_mess_collect TYPE /agri/t_gprolog,
        lt_mess_change  LIKE lt_mess_collect,
        lt_at01         TYPE TABLE OF zabs_str_af_glflotca INITIAL SIZE 0,
        lt_at02         TYPE TABLE OF /agri/s_at_sheet2 INITIAL SIZE 0,
        lt_at03         TYPE TABLE OF /agri/s_at_sheet3 INITIAL SIZE 0,
        lt_at04         TYPE TABLE OF /agri/s_at_sheet4 INITIAL SIZE 0,
        lt_at05         TYPE TABLE OF /agri/s_at_sheet5 INITIAL SIZE 0,
        lv_terrain      TYPE /agri/gltplnr_fl,
        lv_row          TYPE /agri/s_excel_sheet-row.

  FIELD-SYMBOLS: <lwa_at01> TYPE zabs_str_af_glflotca,
                 <lwa_at02> TYPE /agri/s_at_sheet2,
                 <lwa_at03> TYPE /agri/s_at_sheet3,
                 <lwa_at04> TYPE /agri/s_at_sheet4,
                 <lwa_at05> TYPE /agri/s_at_sheet5,
                 <lv_value> TYPE any.

  CONSTANTS: BEGIN OF c_structures,
               af_01   TYPE tabname    VALUE 'ZABS_STR_AF_GLFLOTCA',
               af_02   TYPE tabname    VALUE '/AGRI/S_AT_SHEET2',
               af_03   TYPE tabname    VALUE '/AGRI/S_AT_SHEET3',
               af_04   TYPE tabname    VALUE '/AGRI/S_AT_SHEET4',
               af_05   TYPE tabname    VALUE '/AGRI/S_AT_SHEET5',
               klart   TYPE klassenart VALUE 'X91',
               af_in   TYPE updkz_d    VALUE 'I',
               af_up   TYPE updkz_d    VALUE 'U',
               success TYPE sy-msgty   VALUE 'S',
             END OF c_structures.

  lt_table[] = it_table[].

  DELETE lt_table WHERE row = 1.
  DELETE lt_table WHERE sheet <> 2.

  "Sheet 2
  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-af_01
      i_sheet     = 02
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  SORT lt_dd03l BY position.

  LOOP AT lt_table INTO lwa_data WHERE sheet = 02.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_at01 ASSIGNING <lwa_at01>.
      lv_row = lwa_data-row.
    ENDIF.

    READ TABLE lt_dd03l TRANSPORTING NO FIELDS
      WITH KEY position = lwa_data-column.
    IF sy-subrc EQ 0.
      LOOP AT lt_dd03l INTO lwa_dd03l FROM sy-tabix.
        IF lwa_dd03l-position NE lwa_data-column.
          EXIT.
        ENDIF.
        ASSIGN COMPONENT lwa_data-fieldname
          OF STRUCTURE <lwa_at01> TO <lv_value>.
        IF sy-subrc EQ 0.
          <lv_value> = lwa_data-value.
        ENDIF.
      ENDLOOP.
    ENDIF.

    <lwa_at01>-excel_row = lwa_data-row.
    <lwa_at01>-excel_column = lwa_data-column.
  ENDLOOP.
  CLEAR lv_row.

  IF lt_at01[] IS NOT INITIAL.
    SORT lt_at01 BY tplnr_fl.

    DATA(lv_terrain_w_error) = abap_false.
    LOOP AT lt_at01 ASSIGNING <lwa_at01>.
      AT NEW tplnr_fl.
        CLEAR lv_terrain_w_error.
      ENDAT.

      IF lv_terrain_w_error EQ abap_true
      AND lv_terrain = <lwa_at01>-tplnr_fl.
        <lwa_at01>-line_w_error = abap_true.
        CONTINUE.
      ENDIF.

      IF <lwa_at01>-tplnr_fl IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
          EXPORTING
            input      = <lwa_at01>-tplnr_fl
          IMPORTING
            output     = <lwa_at01>-tplnr_fl_in
          EXCEPTIONS
            not_found  = 1
            not_active = 2
            OTHERS     = 3.

        IF sy-subrc <> 0.
          lv_terrain_w_error = <lwa_at01>-line_w_error = abap_true.
*-- Verificar Coluna &1 Linha &2.
          CLEAR lwa_message.
          lwa_message-msgid   = 'ZABS_MSGCLS'.
          lwa_message-msgno   = '143'.
          lwa_message-msgty   = 'E'.
          lwa_message-msgv1   = <lwa_at01>-excel_column.
          lwa_message-msgv2   = <lwa_at01>-excel_row.
          APPEND lwa_message TO lt_messages.
          CLEAR lwa_message.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
          lwa_message-msgid = sy-msgid.
          lwa_message-msgno = sy-msgno.
          lwa_message-msgty = sy-msgty.
          lwa_message-msgv1 = sy-msgv1.
          lwa_message-msgv2 = sy-msgv2.
          lwa_message-msgv3 = sy-msgv3.
          lwa_message-msgv4 = sy-msgv4.
          APPEND lwa_message TO lt_messages.
        ENDIF.
      ELSE.
        lv_terrain_w_error = <lwa_at01>-line_w_error = abap_true.
*-- Verificar Coluna &1 Linha &2 do Arquivo Excel.
        CLEAR lwa_message.
        lwa_message-msgid   = 'ZABS_MSGCLS'.
        lwa_message-msgno   = '143'.
        lwa_message-msgty   = 'E'.
        lwa_message-msgv1   = <lwa_at01>-excel_column.
        lwa_message-msgv2   = <lwa_at01>-excel_row.
        APPEND lwa_message TO lt_messages.
*-- Código do Terreno não informado.
        CLEAR lwa_message.
        lwa_message-msgid   = 'ZABS_MSGCLS'.
        lwa_message-msgno   = '144'.
        lwa_message-msgty   = 'E'.
        APPEND lwa_message TO lt_messages.
      ENDIF.

      lv_terrain = <lwa_at01>-tplnr_fl.
    ENDLOOP.
    APPEND LINES OF lt_messages TO lt_mess_collect.
  ENDIF.

  DATA(lt_tplnr) = lt_at01[].

  DELETE ADJACENT DUPLICATES: FROM lt_tplnr  COMPARING tplnr_fl.

  LOOP AT lt_tplnr ASSIGNING FIELD-SYMBOL(<lwa_tplnr>).
    READ TABLE lt_at01 INTO DATA(lwa_at01)
      WITH KEY tplnr_fl = <lwa_tplnr>-tplnr_fl BINARY SEARCH.
    IF sy-subrc EQ 0.
      IF lwa_at01-line_w_error EQ abap_true.
        CONTINUE.
      ENDIF.

      REFRESH lt_mess_change.
      CALL FUNCTION 'ZABS_FM_GLFLM_CHANGE'
        EXPORTING
          iv_terrain           = <lwa_tplnr>-tplnr_fl
          is_additional_fiedls = lwa_at01
          it_dd03l             = lt_dd03l
        IMPORTING
          et_messages          = lt_mess_change.

      APPEND LINES OF lt_mess_change TO lt_mess_collect.
    ENDIF.
  ENDLOOP.

  et_messages = lt_mess_collect.

ENDMETHOD.


METHOD AGRIPLAN_BUDGET_CREATE.

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
        lt_budget_sheet TYPE STANDARD TABLE OF zabs_str_budget_sheet1 INITIAL SIZE 0,
        lt_sheet_sum    LIKE lt_budget_sheet,
        lt_orcamento    TYPE STANDARD TABLE OF zabs_orcamento INITIAL SIZE 0,
        lt_orc_aux      TYPE STANDARD TABLE OF ty_orc_aux,
        lt_fazenda      TYPE TABLE OF ty_fazenda,
        lt_vol_collect  TYPE STANDARD TABLE OF zfmacvlcl INITIAL SIZE 0,
        lt_consolidated TYPE TABLE OF ty_total,
        lt_detailed     TYPE TABLE OF ty_total,
        lrt_bwkey       TYPE RANGE OF bwkey,
        lrt_matnr       TYPE RANGE OF zfmacmatnr,
        lrt_atnam       TYPE RANGE OF atnam,
        lrt_tplnr_fl    TYPE RANGE OF /agri/gltplnr_fl,
        lrs_tplnr_fl    LIKE LINE OF lrt_tplnr_fl,
        ls_sheet_sum    LIKE LINE OF lt_sheet_sum,
        ls_fazenda      LIKE LINE OF lt_fazenda,
        ls_consolidated TYPE ty_total,
        ls_detailed     TYPE ty_total,
        ls_orc_aux      LIKE LINE OF lt_orc_aux,
        ls_vol_collect  LIKE LINE OF lt_vol_collect,
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
        lv_auxiliar     TYPE f,
        lv_passadas     TYPE f,
        lv_minutes      TYPE i,
        lv_i            TYPE i,
        lv_h            TYPE n LENGTH 2,
        lv_m            TYPE n LENGTH 2,
        lv_s            TYPE n LENGTH 2,
        lv_period       TYPE sydatum,
        lv_time         TYPE sy-uzeit,
        lv_prod_calc    TYPE zabs_del_qtd_tot,
        lv_prod_dif     TYPE zabs_del_qtd_tot,
        lv_max_dif      TYPE zabs_del_qtd_tot VALUE '0.010',
        lv_versao       TYPE zabs_del_ver_orc,
        lv_werks        TYPE werks_d,
        lv_time_char    TYPE char0256,
        lv_num          TYPE float,
        lv_row          TYPE /agri/s_excel_sheet-row.

  CONSTANTS: BEGIN OF c_structures,
               bud_01  TYPE tabname  VALUE 'ZABS_STR_BUDGET_SHEET1',
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

  FIELD-SYMBOLS: <lwa_budget_sheet> TYPE zabs_str_budget_sheet1,
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
            lv_period  = ls_data-value.
            <lv_value> = lv_period(6).
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
  CLEAR lv_row.

  SORT lrt_matnr BY low.
  DELETE ADJACENT DUPLICATES FROM lrt_matnr COMPARING low.

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
      INTO TABLE @DATA(lt_zfmrchdr)
      FROM zfmrchdr
      FOR ALL ENTRIES IN @lt_budget_sheet
     WHERE rcnum = @lt_budget_sheet-rcnum.

    SELECT s~tplnr_fl, s~contr, s~datab, s~datbi,
           s~astat, s~anlnr, s~kostl, s~rtnid, s~loevm,
           s~datab_ref, s~zzfazplantio, v~acnum,
           v~vornr, v~matnr, v~acqtb, v~acarx, r~rcnum,
           r~werks
      INTO TABLE @DATA(lt_vol_calda)
      FROM /agri/glflcma AS s
      INNER JOIN zfmacvlcl AS v
      ON s~tplnr_fl = v~tplnr_fl
      INNER JOIN zfmaitm AS i
      ON v~acnum = i~acnum
      AND v~tplnr_fl = i~tplnr_fl
      INNER JOIN zfmrchdr AS r
      ON i~iwerk = r~werks
      FOR ALL ENTRIES IN @lt_budget_sheet
     WHERE v~acnum EQ @lt_budget_sheet-acnum
       AND v~matnr IN @lrt_matnr[]
       AND r~rcnum EQ @lt_budget_sheet-rcnum.

*-- Consider only active seasons and not eliminated
    DELETE lt_vol_calda WHERE astat NE c_crop_season_status-active
                           OR loevm EQ abap_true.

*-- If there are several seasons for a terrain, consider the last one created
    SORT lt_vol_calda BY matnr    ASCENDING
                         tplnr_fl ASCENDING
                         contr    DESCENDING.

    DELETE ADJACENT DUPLICATES FROM lt_vol_calda COMPARING matnr tplnr_fl.

    IF lt_vol_calda[] IS NOT INITIAL.
      LOOP AT lt_vol_calda INTO DATA(ls_vol_calda).
        ls_vol_collect-acarx = ls_vol_calda-acarx.
        ls_vol_collect-acqtb = ls_vol_calda-acqtb.
        ls_vol_collect-acnum = ls_vol_calda-acnum.
        ls_vol_collect-matnr = ls_vol_calda-matnr.
        COLLECT ls_vol_collect INTO lt_vol_collect.

        INSERT INITIAL LINE INTO TABLE lt_fazenda
          ASSIGNING FIELD-SYMBOL(<ls_fazenda>).
        IF sy-subrc EQ 0.
          IF ls_vol_calda-kostl IS NOT INITIAL.
            <ls_fazenda>-kostlform = ls_vol_calda-kostl.
            <ls_fazenda>-kostlmanu = ls_vol_calda-kostl.
          ELSEIF ls_vol_calda-anlnr IS NOT INITIAL.
            <ls_fazenda>-kostlform = ls_vol_calda-anlnr.
            <ls_fazenda>-kostlmanu = ls_vol_calda-anlnr.
          ENDIF.

          CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
            EXPORTING
              input  = ls_vol_calda-tplnr_fl
            IMPORTING
              output = <ls_fazenda>-terreno.

          <ls_fazenda>-tplnr_fl = ls_vol_calda-tplnr_fl.
          SPLIT <ls_fazenda>-terreno AT '-'
            INTO <ls_fazenda>-fazenda_d <ls_fazenda>-talhao.

          CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
            EXPORTING
              input  = <ls_fazenda>-fazenda_d
            IMPORTING
              output = <ls_fazenda>-fazenda.

          <ls_fazenda>-acnum = ls_vol_calda-acnum.
        ENDIF.
      ENDLOOP.

      SORT lt_fazenda BY acnum tplnr_fl.
    ENDIF.
  ENDIF.

  DELETE ADJACENT DUPLICATES FROM lt_fazenda COMPARING ALL FIELDS.

*-- Fetch the items from the crop area to check the branches
  SELECT i~acnum, i~acpos, i~tplnr_fl,
         i~iwerk, i~contr, i~cmnum,
         i~season, i~datab, i~datbi
    FROM zfmaitm AS i
    INNER JOIN zfmachdr AS h
    ON i~acnum EQ h~acnum
    INNER JOIN zfmrchdr AS r
    ON i~iwerk = r~werks
    INTO TABLE @DATA(lt_zfmacitm)
    FOR ALL ENTRIES IN @lt_budget_sheet
   WHERE h~acnum EQ @lt_budget_sheet-acnum
     AND r~rcnum EQ @lt_budget_sheet-rcnum.

  IF sy-subrc EQ 0.
    DATA(lt_fmac_aux) = lt_zfmacitm[].
    SORT: lt_zfmacitm BY tplnr_fl,
          lt_fmac_aux BY iwerk.
    DELETE ADJACENT DUPLICATES FROM lt_fmac_aux COMPARING iwerk.

    LOOP AT lt_fmac_aux INTO DATA(ls_fmac_aux).
      INSERT INITIAL LINE INTO TABLE lrt_bwkey
        ASSIGNING FIELD-SYMBOL(<ls_bwkey>).
      IF sy-subrc EQ 0.
        <ls_bwkey> = 'IEQ'.
        <ls_bwkey>-low = ls_fmac_aux-iwerk.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF lrt_bwkey[] IS NOT INITIAL.
    SELECT matnr, bwkey, bwtar, verpr
      FROM mbew
      INTO TABLE @DATA(lt_mbew)
      FOR ALL ENTRIES IN @lt_budget_sheet
     WHERE matnr EQ @lt_budget_sheet-matnr_ins
       AND bwkey IN @lrt_bwkey[].

    SORT lt_mbew BY matnr bwkey.
  ENDIF.

  SELECT matnr, matkl, extwg
    FROM mara
    INTO TABLE @DATA(lt_mara)
    FOR ALL ENTRIES IN @lt_budget_sheet
   WHERE matnr EQ @lt_budget_sheet-matnr.

  SELECT matnr, matkl, extwg
    FROM mara
    APPENDING TABLE @lt_mara
    FOR ALL ENTRIES IN @lt_budget_sheet
   WHERE matnr EQ @lt_budget_sheet-matnr_ins.

*-- Fetch the Recipe Types used for Budget (Orçamento)
  SELECT rctyp, orcamento
    INTO TABLE @DATA(lt_rctyp)
    FROM ztfmrctyp
   WHERE orcamento EQ @abap_true.

  SELECT *
    FROM zfmrclst
    INTO TABLE @DATA(lt_zfmrclst)
    FOR ALL ENTRIES IN @lt_zfmrchdr
   WHERE rcnum = @lt_zfmrchdr-rcnum
     AND werks = @lt_zfmrchdr-werks.

  SORT: lt_mara BY matnr,
        lt_rctyp BY rctyp,
        lt_zfmrchdr BY rcnum,
        lt_rctyp BY rctyp,
        lt_zfmacitm BY acnum,
        lt_zfmrclst BY rcnum matnr_ins.

  LOOP AT lt_budget_sheet ASSIGNING <lwa_budget_sheet>.
    UNPACK <lwa_budget_sheet>-acnum TO <lwa_budget_sheet>-acnum.

    IF <lwa_budget_sheet>-acnum IS INITIAL.
      IF <lwa_budget_sheet>-line_w_error EQ abap_false.
        <lwa_budget_sheet>-line_w_error = abap_true.
        add_first_line <lwa_budget_sheet>-excel_row.
      ENDIF.
*-- Área de Cultivo não informada!
      CLEAR ls_message.
      ls_message-msgid = 'ZFMFP'.
      ls_message-msgno = 238.
      ls_message-msgty = 'E'.
      APPEND ls_message TO lt_messages.
    ELSE.
      READ TABLE lt_zfmacitm INTO DATA(ls_zfmacitm)
        WITH KEY acnum = <lwa_budget_sheet>-acnum BINARY SEARCH.
      IF sy-subrc NE 0.
        IF <lwa_budget_sheet>-line_w_error EQ abap_false.
          <lwa_budget_sheet>-line_w_error = abap_true.
          add_first_line <lwa_budget_sheet>-excel_row.
        ENDIF.
*-- Área de Cultivo &1 não existe.
        CLEAR ls_message.
        ls_message-msgid = 'ZFMFP'.
        ls_message-msgno = 231.
        ls_message-msgty = 'E'.
        ls_message-msgv1 = <lwa_budget_sheet>-acnum.
        APPEND ls_message TO lt_messages.
      ENDIF.
    ENDIF.

    IF <lwa_budget_sheet>-matnr IS INITIAL.
      IF <lwa_budget_sheet>-line_w_error EQ abap_false.
        <lwa_budget_sheet>-line_w_error = abap_true.
        add_first_line <lwa_budget_sheet>-excel_row.
      ENDIF.
*-- Código de tarefa não informada!
      CLEAR ls_message.
      ls_message-msgid = 'ZFMFP'.
      ls_message-msgno = 239.
      ls_message-msgty = 'E'.
      APPEND ls_message TO lt_messages.
    ELSE.
      READ TABLE lt_mara INTO DATA(ls_mara)
        WITH KEY matnr = <lwa_budget_sheet>-matnr BINARY SEARCH.
      IF sy-subrc NE 0.
        IF <lwa_budget_sheet>-line_w_error EQ abap_false.
          <lwa_budget_sheet>-line_w_error = abap_true.
          add_first_line <lwa_budget_sheet>-excel_row.
        ENDIF.
*-- Código de tarefa &1 não existe.
        CLEAR ls_message.
        ls_message-msgid = 'ZFMFP'.
        ls_message-msgno = 232.
        ls_message-msgty = 'E'.
        ls_message-msgv1 = <lwa_budget_sheet>-matnr.
        APPEND ls_message TO lt_messages.
      ENDIF.
    ENDIF.

    IF <lwa_budget_sheet>-rcnum IS INITIAL.
      IF <lwa_budget_sheet>-line_w_error EQ abap_false.
        <lwa_budget_sheet>-line_w_error = abap_true.
        add_first_line <lwa_budget_sheet>-excel_row.
      ENDIF.
*-- Receita não informada!
      CLEAR ls_message.
      ls_message-msgid = 'ZFMFP'.
      ls_message-msgno = 240.
      ls_message-msgty = 'E'.
      APPEND ls_message TO lt_messages.
    ELSE.
      READ TABLE lt_zfmrchdr INTO DATA(ls_zfmrchdr)
        WITH KEY rcnum = <lwa_budget_sheet>-rcnum BINARY SEARCH.
      IF sy-subrc NE 0.
        IF <lwa_budget_sheet>-line_w_error EQ abap_false.
          <lwa_budget_sheet>-line_w_error = abap_true.
          add_first_line <lwa_budget_sheet>-excel_row.
        ENDIF.
*-- Receita &1 não existe.
        CLEAR ls_message.
        ls_message-msgid = 'ZFMFP'.
        ls_message-msgno = 230.
        ls_message-msgty = 'E'.
        ls_message-msgv1 = <lwa_budget_sheet>-rcnum.
        APPEND ls_message TO lt_messages.
      ELSE.
        READ TABLE lt_rctyp TRANSPORTING NO FIELDS
          WITH KEY rctyp = ls_zfmrchdr-rctyp BINARY SEARCH.
        IF sy-subrc NE 0.
          IF <lwa_budget_sheet>-line_w_error EQ abap_false.
            <lwa_budget_sheet>-line_w_error = abap_true.
            add_first_line <lwa_budget_sheet>-excel_row.
          ENDIF.
*-- Tipo &1 da Receita &2 não é de Orçamento!
          CLEAR ls_message.
          ls_message-msgid = 'ZFMFP'.
          ls_message-msgno = 236.
          ls_message-msgty = 'E'.
          ls_message-msgv1 = ls_zfmrchdr-rctyp.
          ls_message-msgv2 = <lwa_budget_sheet>-rcnum.
          APPEND ls_message TO lt_messages.
        ENDIF.

        READ TABLE lt_zfmrclst INTO DATA(ls_zfmrclst)
          WITH KEY rcnum = <lwa_budget_sheet>-rcnum
                   matnr_ins = <lwa_budget_sheet>-matnr_ins BINARY SEARCH.
        IF sy-subrc NE 0.
          IF <lwa_budget_sheet>-line_w_error EQ abap_false.
            <lwa_budget_sheet>-line_w_error = abap_true.
            add_first_line <lwa_budget_sheet>-excel_row.
          ENDIF.
*-- Receita &1 não possui insumo &2!
          CLEAR ls_message.
          ls_message-msgid = 'ZFMFP'.
          ls_message-msgno = 237.
          ls_message-msgty = 'E'.
          ls_message-msgv1 = <lwa_budget_sheet>-rcnum.
          ls_message-msgv2 = <lwa_budget_sheet>-matnr_ins.
          APPEND ls_message TO lt_messages.
        ENDIF.
      ENDIF.
    ENDIF.

    IF <lwa_budget_sheet>-matnr_ins IS INITIAL.
      IF <lwa_budget_sheet>-line_w_error EQ abap_false.
        <lwa_budget_sheet>-line_w_error = abap_true.
        add_first_line <lwa_budget_sheet>-excel_row.
      ENDIF.
*-- Material insumo não informado!
      CLEAR ls_message.
      ls_message-msgid = 'ZFMFP'.
      ls_message-msgno = 241.
      ls_message-msgty = 'E'.
      APPEND ls_message TO lt_messages.
    ELSE.
      READ TABLE lt_mara INTO ls_mara
        WITH KEY matnr = <lwa_budget_sheet>-matnr_ins BINARY SEARCH.
      IF sy-subrc NE 0.
        IF <lwa_budget_sheet>-line_w_error EQ abap_false.
          <lwa_budget_sheet>-line_w_error = abap_true.
          add_first_line <lwa_budget_sheet>-excel_row.
        ENDIF.
*-- Insumo &1 não existe.
        CLEAR ls_message.
        ls_message-msgid = 'ZFMFP'.
        ls_message-msgno = 233.
        ls_message-msgty = 'E'.
        ls_message-msgv1 = <lwa_budget_sheet>-matnr_ins.
        APPEND ls_message TO lt_messages.
      ENDIF.
    ENDIF.

    IF <lwa_budget_sheet>-data IS INITIAL.
*-- Período não informado!
      IF <lwa_budget_sheet>-line_w_error EQ abap_false.
        <lwa_budget_sheet>-line_w_error = abap_true.
        add_first_line <lwa_budget_sheet>-excel_row.
      ENDIF.
      CLEAR ls_message.
      ls_message-msgid = 'ZFMFP'.
      ls_message-msgno = 244.
      ls_message-msgty = 'E'.
      APPEND ls_message TO lt_messages.
    ELSE.
      CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
        EXPORTING
          date                      = <lwa_budget_sheet>-data
        EXCEPTIONS
          plausibility_check_failed = 1
          OTHERS                    = 2.

      IF sy-subrc NE 0.
        IF <lwa_budget_sheet>-line_w_error EQ abap_false.
          <lwa_budget_sheet>-line_w_error = abap_true.
          add_first_line <lwa_budget_sheet>-excel_row.
        ENDIF.
*-- Data & inválida.
        CLEAR ls_message.
        ls_message-msgid = 'ZFMFP'.
        ls_message-msgno = 234.
        ls_message-msgty = 'E'.
        ls_message-msgv1 = <lwa_budget_sheet>-data.
        APPEND ls_message TO lt_messages.
      ELSE.
        <lwa_budget_sheet>-period = <lwa_budget_sheet>-data(6).
      ENDIF.
    ENDIF.
  ENDLOOP.

  SORT lt_zfmacitm BY acnum tplnr_fl.
  SORT lt_budget_sheet BY acnum matnr rcnum period matnr_ins.
  DATA(lt_period) = lt_budget_sheet[].
  SORT lt_period BY acnum matnr rcnum period matnr_ins.
  DELETE ADJACENT DUPLICATES FROM lt_period COMPARING acnum matnr rcnum period.

  SORT: lt_vol_calda   BY acnum matnr,
        lt_vol_collect BY acnum matnr.

  LOOP AT lt_budget_sheet ASSIGNING <lwa_budget_sheet>.
    IF <lwa_budget_sheet>-line_w_error EQ abap_true.
      CONTINUE.
    ENDIF.

    READ TABLE lt_period TRANSPORTING NO FIELDS
      WITH KEY acnum = <lwa_budget_sheet>-acnum
               matnr = <lwa_budget_sheet>-matnr
               rcnum = <lwa_budget_sheet>-rcnum BINARY SEARCH.
    IF sy-subrc EQ 0.
      LOOP AT lt_period INTO DATA(ls_period) FROM sy-tabix.
        IF ls_period-acnum NE <lwa_budget_sheet>-acnum
        OR ls_period-matnr NE <lwa_budget_sheet>-matnr
        OR ls_period-rcnum NE <lwa_budget_sheet>-rcnum.
          EXIT.
        ENDIF.

        DATA(lv_inserted) = abap_false.
        READ TABLE lt_vol_calda INTO ls_vol_calda
          WITH KEY acnum = <lwa_budget_sheet>-acnum
                   matnr = <lwa_budget_sheet>-matnr BINARY SEARCH.
        WHILE sy-subrc EQ 0.
          DATA(lv_tabix) = sy-tabix + 1.

          lv_tman = 'TMAN' && ls_vol_calda-matnr+4(36).
          lv_tfor = 'TFOR' && ls_vol_calda-matnr+4(36).
          lv_timp = 'TIMP' && ls_vol_calda-matnr+4(36).

          READ TABLE lt_vol_collect INTO ls_vol_collect
            WITH KEY acnum = <lwa_budget_sheet>-acnum
                     matnr = <lwa_budget_sheet>-matnr BINARY SEARCH.
          IF sy-subrc NE 0.
            CONTINUE.
          ENDIF.

          CLEAR: ls_consolidated, ls_detailed.
          IF ls_period-data GE ls_vol_calda-datab
          AND ls_period-data LE ls_vol_calda-datbi.
            IF ls_vol_calda-matnr = lv_tman.
              ls_consolidated-aareamanu  = ls_vol_collect-acarx.
              ls_consolidated-bombasmanu = ls_vol_collect-acqtb.
            ELSEIF ls_vol_calda-matnr = lv_tfor
                OR ls_vol_calda-matnr = lv_timp.
              ls_consolidated-aareaform  = ls_vol_collect-acarx.
              ls_consolidated-bombasform = ls_vol_collect-acqtb.
            ENDIF.

            ls_consolidated-matnr  = <lwa_budget_sheet>-matnr.
            ls_consolidated-acnum  = ls_vol_calda-acnum.
            ls_consolidated-period = ls_period-period.
            IF lv_inserted EQ abap_false.
              lv_inserted = abap_true.
              APPEND ls_consolidated TO lt_consolidated.
            ENDIF.

            READ TABLE lt_zfmacitm INTO ls_zfmacitm
              WITH KEY acnum    = <lwa_budget_sheet>-acnum
                       tplnr_fl = ls_vol_calda-tplnr_fl BINARY SEARCH.
            IF sy-subrc NE 0.
              CLEAR ls_zfmacitm.
            ENDIF.

            ls_detailed = ls_consolidated.

            CLEAR ls_fazenda.
            READ TABLE lt_fazenda INTO ls_fazenda
              WITH KEY acnum    = ls_vol_calda-acnum
                       tplnr_fl = ls_vol_calda-tplnr_fl BINARY SEARCH.
            IF sy-subrc EQ 0.
              ls_detailed-acnum      = ls_fazenda-acnum.
              ls_detailed-fazenda    = ls_fazenda-tplnr_fl.
              ls_detailed-tarefa     = <lwa_budget_sheet>-matnr.
              ls_detailed-matnr      = <lwa_budget_sheet>-matnr_ins.
              ls_detailed-iwerk      = ls_zfmacitm-iwerk.
              ls_detailed-kostl_form = ls_fazenda-kostlform.
              ls_detailed-kostl_manu = ls_fazenda-kostlmanu.
              ls_detailed-aareaform  = ls_vol_calda-acarx.
              ls_detailed-bombasform = ls_vol_calda-acqtb.
            ENDIF.

            READ TABLE lt_detailed TRANSPORTING NO FIELDS
              WITH KEY acnum      = ls_detailed-acnum
                       fazenda    = ls_detailed-fazenda
                       tarefa     = ls_detailed-tarefa
                       matnr      = ls_detailed-matnr
                       iwerk      = ls_detailed-iwerk
                       kostl_form = ls_detailed-kostl_form
                       kostl_manu = ls_detailed-kostl_manu
                       period     = ls_period-period
                       aareaform  = ls_detailed-aareaform
                       bombasform = ls_detailed-bombasform.
            IF sy-subrc NE 0.
              APPEND ls_detailed TO lt_detailed.
            ENDIF.
          ENDIF.

          READ TABLE lt_vol_calda INTO ls_vol_calda
            INDEX lv_tabix COMPARING acnum matnr.
        ENDWHILE.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  SORT lt_mbew BY matnr bwkey.
  LOOP AT lt_detailed ASSIGNING FIELD-SYMBOL(<ls_detailed>).
    READ TABLE lt_mbew INTO DATA(ls_mbew)
      WITH KEY matnr = <ls_detailed>-matnr
               bwkey = <ls_detailed>-iwerk BINARY SEARCH.
    IF sy-subrc EQ 0.
      <ls_detailed>-valor = ls_mbew-verpr.
    ENDIF.
  ENDLOOP.

  SORT: lt_consolidated BY acnum period matnr,
        lt_detailed BY acnum period tarefa matnr,
        lt_zfmrchdr BY rcnum.

  DELETE ADJACENT DUPLICATES FROM lt_consolidated COMPARING ALL FIELDS.

  SORT lt_consolidated BY acnum period matnr.
  LOOP AT lt_budget_sheet ASSIGNING <lwa_budget_sheet>.
    IF <lwa_budget_sheet>-line_w_error EQ abap_true.
      CONTINUE.
    ELSE.
      DATA(lt_zfmacvlcl_1) = lt_vol_calda[].
      DELETE lt_zfmacvlcl_1 WHERE matnr NE <lwa_budget_sheet>-matnr.

      READ TABLE lt_zfmrchdr INTO DATA(ls_receita)
        WITH KEY rcnum = <lwa_budget_sheet>-rcnum BINARY SEARCH.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      READ TABLE lt_consolidated INTO ls_consolidated
        WITH KEY acnum  = <lwa_budget_sheet>-acnum
                 period = <lwa_budget_sheet>-period
                 matnr  = <lwa_budget_sheet>-matnr BINARY SEARCH.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.
    ENDIF.

    READ TABLE lt_detailed INTO ls_detailed
      WITH KEY acnum  = <lwa_budget_sheet>-acnum
               period = <lwa_budget_sheet>-period
               tarefa = <lwa_budget_sheet>-matnr
               matnr  = <lwa_budget_sheet>-matnr_ins BINARY SEARCH.

    WHILE sy-subrc EQ 0.
      lv_tabix = sy-tabix + 1.

      READ TABLE lt_zfmrclst INTO ls_zfmrclst
        WITH KEY rcnum     = <lwa_budget_sheet>-rcnum
                 matnr_ins = ls_detailed-matnr BINARY SEARCH.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      CLEAR: ls_orcamento, lv_passadas, lv_auxiliar.
      ls_orcamento-acnum = <lwa_budget_sheet>-acnum.
      READ TABLE lt_mara INTO ls_mara
        WITH KEY matnr = <lwa_budget_sheet>-matnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_orcamento-extwg = ls_mara-extwg.
        ls_orcamento-matkl = ls_mara-matkl.
      ENDIF.
      ls_orcamento-rcnum    = <lwa_budget_sheet>-rcnum.
      ls_orcamento-matnr    = ls_detailed-matnr.
      ls_orcamento-period   = <lwa_budget_sheet>-period.
      ls_orcamento-passadas = <lwa_budget_sheet>-passadas.

      lv_tman = 'TMAN' && <lwa_budget_sheet>-matnr+4(36).
      lv_tfor = 'TFOR' && <lwa_budget_sheet>-matnr+4(36).
      lv_timp = 'TIMP' && <lwa_budget_sheet>-matnr+4(36).

      IF <lwa_budget_sheet>-passadas IS NOT INITIAL.
        ls_orcamento-passadas = <lwa_budget_sheet>-passadas.
        IF lv_tman EQ <lwa_budget_sheet>-matnr.
          ls_orcamento-aarea_manu = <lwa_budget_sheet>-passadas * ls_detailed-aareamanu.
*-- Calcula Quantidade de Produto
          IF ls_receita-ausme EQ 'VC'.
            ls_orcamento-produtos = ( <lwa_budget_sheet>-passadas * ls_detailed-bombasmanu )
                                    * ls_zfmrclst-rcdos.
          ELSEIF ls_receita-ausme EQ 'HC'.
            ls_orcamento-produtos = ( <lwa_budget_sheet>-passadas * ls_detailed-aareamanu )
                                      * ls_zfmrclst-rcdos.
          ENDIF.

          ls_orcamento-custo = ls_orcamento-produtos * ls_detailed-valor.
        ELSEIF lv_tfor EQ <lwa_budget_sheet>-matnr
            OR lv_timp EQ <lwa_budget_sheet>-matnr.
          ls_orcamento-aarea_form = <lwa_budget_sheet>-passadas * ls_detailed-aareaform.
*-- Calcula Quantidade de Produto
          IF ls_receita-ausme EQ 'VC'.
            ls_orcamento-produtos = ( <lwa_budget_sheet>-passadas * ls_detailed-bombasform )
                                    * ls_zfmrclst-rcdos.
          ELSEIF ls_receita-ausme EQ 'HC'.
            ls_orcamento-produtos = ( <lwa_budget_sheet>-passadas * ls_detailed-aareaform )
                                    * ls_zfmrclst-rcdos.
          ENDIF.

          ls_orcamento-custo = ls_orcamento-produtos * ls_detailed-valor.
        ENDIF.

        IF ls_zfmrclst-rcinp_check NE abap_true.
          CLEAR: ls_orcamento-aarea_manu, ls_orcamento-aarea_form.
        ENDIF.
      ELSEIF <lwa_budget_sheet>-produtos IS NOT INITIAL.
        IF lv_tman EQ <lwa_budget_sheet>-matnr.
          IF ls_receita-ausme EQ 'VC'.
            IF ls_zfmrclst-rcdos IS NOT INITIAL.
              lv_auxiliar = <lwa_budget_sheet>-produtos / ls_zfmrclst-rcdos.
              IF ls_consolidated-bombasmanu IS NOT INITIAL.
                lv_passadas = lv_auxiliar / ls_consolidated-bombasmanu.
              ENDIF.
            ENDIF.
            ls_orcamento-passadas = lv_passadas.

            READ TABLE lt_zfmacvlcl_1 INTO DATA(ls_zfmacvlcl)
              WITH KEY tplnr_fl = ls_detailed-fazenda.
            IF sy-subrc EQ 0.
              IF lv_tman EQ <lwa_budget_sheet>-matnr.
                IF ls_consolidated-bombasmanu IS NOT INITIAL.
                  ls_orcamento-produtos = ( ls_zfmacvlcl-acqtb /
                     ls_consolidated-bombasmanu ) * <lwa_budget_sheet>-produtos.
                ENDIF.
              ELSE.
                IF ls_consolidated-bombasform IS NOT INITIAL.
                  ls_orcamento-produtos = ( ls_zfmacvlcl-acqtb /
                     ls_consolidated-bombasform ) * <lwa_budget_sheet>-produtos.
                ENDIF.
              ENDIF.
              ls_orcamento-fazenda = ls_zfmacvlcl-tplnr_fl.
            ENDIF.
            CLEAR ls_zfmacvlcl.
          ELSEIF ls_receita-ausme EQ 'HC'.
            READ TABLE lt_zfmacvlcl_1 INTO ls_zfmacvlcl
              WITH KEY tplnr_fl = ls_detailed-fazenda.
            IF sy-subrc EQ 0.
              IF lv_tman EQ <lwa_budget_sheet>-matnr.
                IF ls_consolidated-aareamanu IS NOT INITIAL.
                  ls_orcamento-produtos = ( <lwa_budget_sheet>-produtos / ls_consolidated-aareamanu )
                                           * ls_zfmacvlcl-acarx.
                ENDIF.
              ELSE.
                IF ls_consolidated-aareaform IS NOT INITIAL.
                  ls_orcamento-produtos = ( <lwa_budget_sheet>-produtos / ls_consolidated-aareaform )
                                           * ls_zfmacvlcl-acarx.
                ENDIF.
              ENDIF.
              ls_orcamento-fazenda = ls_zfmacvlcl-tplnr_fl.
            ENDIF.

            IF ls_zfmrclst-rcdos IS NOT INITIAL.
              lv_auxiliar = ls_orcamento-produtos / ls_zfmrclst-rcdos.
              IF ls_zfmacvlcl-acarx IS NOT INITIAL.
                ls_orcamento-passadas = lv_auxiliar / ls_zfmacvlcl-acarx.
              ENDIF.
            ENDIF.
            CLEAR ls_zfmacvlcl.
          ENDIF.

          ls_orcamento-custo = ls_orcamento-produtos * ls_detailed-valor.

          IF ls_zfmrclst-rcinp_check EQ abap_true.
            ls_orcamento-aarea_manu = ls_orcamento-passadas * ls_detailed-aareamanu.
          ENDIF.
        ELSEIF lv_tfor EQ <lwa_budget_sheet>-matnr
            OR lv_timp EQ <lwa_budget_sheet>-matnr.
          IF ls_receita-ausme EQ 'VC'.
            IF ls_zfmrclst-rcdos IS NOT INITIAL.
              lv_auxiliar = <lwa_budget_sheet>-produtos / ls_zfmrclst-rcdos.
              IF ls_consolidated-bombasform IS NOT INITIAL.
                lv_passadas = lv_auxiliar / ls_consolidated-bombasform.
              ENDIF.
            ENDIF.
            ls_orcamento-passadas = lv_passadas.

            READ TABLE lt_zfmacvlcl_1 INTO ls_zfmacvlcl
              WITH KEY tplnr_fl = ls_detailed-fazenda.
            IF sy-subrc EQ 0.
              IF lv_tman EQ <lwa_budget_sheet>-matnr.
                IF ls_consolidated-bombasmanu IS NOT INITIAL.
                  ls_orcamento-produtos = ( ls_zfmacvlcl-acqtb /
                     ls_consolidated-bombasmanu ) * <lwa_budget_sheet>-produtos.
                ENDIF.
              ELSE.
                IF ls_consolidated-bombasform IS NOT INITIAL.
                  ls_orcamento-produtos = ( ls_zfmacvlcl-acqtb /
                     ls_consolidated-bombasform ) * <lwa_budget_sheet>-produtos.
                ENDIF.
              ENDIF.
              ls_orcamento-fazenda = ls_zfmacvlcl-tplnr_fl.
            ENDIF.
            CLEAR ls_zfmacvlcl.
          ELSEIF ls_receita-ausme EQ 'HC'.
            READ TABLE lt_zfmacvlcl_1 INTO ls_zfmacvlcl
              WITH KEY tplnr_fl = ls_detailed-fazenda.
            IF sy-subrc EQ 0.
              IF lv_tman EQ <lwa_budget_sheet>-matnr.
                IF ls_consolidated-aareamanu IS NOT INITIAL.
                  ls_orcamento-produtos = ( <lwa_budget_sheet>-produtos / ls_consolidated-aareamanu )
                                           * ls_zfmacvlcl-acarx.
                ENDIF.
              ELSE.
                IF ls_consolidated-aareaform IS NOT INITIAL.
                  ls_orcamento-produtos = ( <lwa_budget_sheet>-produtos / ls_consolidated-aareaform )
                                           * ls_zfmacvlcl-acarx.
                ENDIF.
              ENDIF.
              ls_orcamento-fazenda = ls_zfmacvlcl-tplnr_fl.
            ENDIF.

            IF ls_zfmrclst-rcdos IS NOT INITIAL.
              lv_auxiliar = ls_orcamento-produtos / ls_zfmrclst-rcdos.
              IF ls_zfmacvlcl-acarx IS NOT INITIAL.
                ls_orcamento-passadas = lv_auxiliar / ls_zfmacvlcl-acarx.
              ENDIF.
            ENDIF.
            CLEAR ls_zfmacvlcl.
          ENDIF.

          ls_orcamento-custo = ls_orcamento-produtos * ls_detailed-valor.

          IF ls_zfmrclst-rcinp_check EQ abap_true.
            ls_orcamento-aarea_form = ls_orcamento-passadas * ls_detailed-aareaform.
          ENDIF.
        ENDIF.
      ENDIF.
      ls_orcamento-fazenda = ls_detailed-fazenda.
      ls_orcamento-rcdos   = ls_zfmrclst-rcdos.
      ls_orcamento-versao  = <lwa_budget_sheet>-versao.

      IF ls_detailed-kostl_manu IS NOT INITIAL.
        ls_orcamento-kostl = ls_detailed-kostl_manu.
      ELSEIF ls_detailed-kostl_form IS NOT INITIAL.
        ls_orcamento-kostl = ls_detailed-kostl_form.
      ENDIF.

      APPEND ls_orcamento TO lt_orcamento.

      READ TABLE lt_detailed INTO ls_detailed
        INDEX lv_tabix COMPARING acnum period tarefa matnr.
    ENDWHILE.
  ENDLOOP.

  IF lt_orcamento[] IS NOT INITIAL.
    READ TABLE lt_orcamento INTO ls_orcamento INDEX 1.
    IF sy-subrc EQ 0.
      ls_orcamento-usuario = sy-uname.
      ls_orcamento-data    = sy-datum.
      ls_orcamento-hora    = sy-uzeit.
      ls_orcamento-tcode   = 'ZABS_ORCAMENTO'.
      MODIFY lt_orcamento FROM ls_orcamento
        TRANSPORTING usuario data hora tcode WHERE data = ''.
    ENDIF.

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


METHOD ATTRIBUTE_CREATE.

  DATA: lt_table           TYPE /agri/t_excel_sheet,
        lwa_data           TYPE /agri/s_excel_sheet,
        lt_dd03l           TYPE thrpad_erd_dd03l,
        lwa_dd03l          TYPE dd03l,
        lt_gathdr          TYPE /agri/t_gathdr,
        lwa_gathdr         TYPE /agri/s_gathdr,
        lt_atda            TYPE /agri/t_gatda,
        lwa_atda           TYPE /agri/s_gatda,
        lt_cawn            TYPE /agri/t_gcawn,
        lwa_cawn           TYPE /agri/s_gcawn,
        lt_cawnt           TYPE /agri/t_gcawnt,
        lwa_cawnt          TYPE /agri/s_gcawnt,
        lwa_gcabnt         TYPE /agri/s_gcabnt,
        lt_gcabnt          TYPE /agri/t_gcabnt,
        lt_agcat           TYPE TABLE OF /agri/glagcat,
        lwa_agcat          TYPE  /agri/glagcat,
        lt_messages        TYPE /agri/t_gprolog,
        lwa_message        TYPE /agri/s_gprolog,
        lrt_atnam          TYPE RANGE OF atnam,
        lrt_class          TYPE RANGE OF klasse_d,
        lt_mess_collect    TYPE /agri/t_gprolog,
        lt_mess_change     LIKE lt_mess_collect,
        lt_flatg           TYPE /agri/t_glflatg,
        lt_flatv           TYPE /agri/t_glflatv,
        lt_at01            TYPE TABLE OF zabs_str_at_sheet1 INITIAL SIZE 0,
        lt_cabn            TYPE zabs_tty_cabn,
        lt_cabn_change     LIKE lt_cabn,
        lt_characteristics LIKE lt_at01,
        lt_at02            TYPE TABLE OF /agri/s_at_sheet2 INITIAL SIZE 0,
        lt_at03            TYPE TABLE OF /agri/s_at_sheet3 INITIAL SIZE 0,
        lt_at04            TYPE TABLE OF /agri/s_at_sheet4 INITIAL SIZE 0,
        lt_at05            TYPE TABLE OF /agri/s_at_sheet5 INITIAL SIZE 0,
        lv_terrain         TYPE /agri/gltplnr_fl,
        lv_row             TYPE /agri/s_excel_sheet-row.

  FIELD-SYMBOLS: <lwa_at01> TYPE zabs_str_at_sheet1,
                 <lwa_at02> TYPE /agri/s_at_sheet2,
                 <lwa_at03> TYPE /agri/s_at_sheet3,
                 <lwa_at04> TYPE /agri/s_at_sheet4,
                 <lwa_at05> TYPE /agri/s_at_sheet5,
                 <lv_value> TYPE any.

  CONSTANTS: BEGIN OF c_structures,
               at_01   TYPE tabname    VALUE 'ZABS_STR_AT_SHEET1',
               at_02   TYPE tabname    VALUE '/AGRI/S_AT_SHEET2',
               at_03   TYPE tabname    VALUE '/AGRI/S_AT_SHEET3',
               at_04   TYPE tabname    VALUE '/AGRI/S_AT_SHEET4',
               at_05   TYPE tabname    VALUE '/AGRI/S_AT_SHEET5',
               klart   TYPE klassenart VALUE 'X91',
               at_in   TYPE updkz_d    VALUE 'I',
               at_up   TYPE updkz_d    VALUE 'U',
               success TYPE sy-msgty   VALUE 'S',
             END OF c_structures.

  lt_table[] = it_table[].

  DELETE lt_table WHERE row = 1.
  DELETE lt_table WHERE sheet <> 1.

  "Sheet 1
  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-at_01
      i_sheet     = 1
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  SORT lt_dd03l BY position.

  LOOP AT lt_table INTO lwa_data WHERE sheet = 01.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_at01 ASSIGNING <lwa_at01>.
      lv_row = lwa_data-row.
    ENDIF.

    READ TABLE lt_dd03l TRANSPORTING NO FIELDS
      WITH KEY position = lwa_data-column.
    IF sy-subrc EQ 0.
      LOOP AT lt_dd03l INTO lwa_dd03l FROM sy-tabix.
        IF lwa_dd03l-position NE lwa_data-column.
          EXIT.
        ENDIF.
        ASSIGN COMPONENT lwa_data-fieldname
          OF STRUCTURE <lwa_at01> TO <lv_value>.
        IF sy-subrc EQ 0.
          <lv_value> = lwa_data-value.
        ENDIF.
      ENDLOOP.
    ENDIF.

    <lwa_at01>-excel_row = lwa_data-row.
    <lwa_at01>-excel_column = lwa_data-column.
  ENDLOOP.
  CLEAR lv_row.

  IF lt_at01[] IS NOT INITIAL.
    SORT lt_at01 BY tplnr_fl class atnam.

    DATA(lv_terrain_w_error) = abap_false.
    LOOP AT lt_at01 ASSIGNING <lwa_at01>.
      AT NEW tplnr_fl.
        CLEAR lv_terrain_w_error.
      ENDAT.

      IF lv_terrain_w_error EQ abap_true
      AND lv_terrain = <lwa_at01>-tplnr_fl.
        <lwa_at01>-line_w_error = abap_true.
        CONTINUE.
      ENDIF.

      IF <lwa_at01>-tplnr_fl IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
          EXPORTING
            input      = <lwa_at01>-tplnr_fl
          IMPORTING
            output     = <lwa_at01>-tplnr_fl_in
          EXCEPTIONS
            not_found  = 1
            not_active = 2
            OTHERS     = 3.

        IF sy-subrc <> 0.
          lv_terrain_w_error = <lwa_at01>-line_w_error = abap_true.
*-- Verificar Coluna &1 Linha &2.
          CLEAR lwa_message.
          lwa_message-msgid   = 'ZABS_MSGCLS'.
          lwa_message-msgno   = '143'.
          lwa_message-msgty   = 'I'.
          lwa_message-msgv1   = <lwa_at01>-excel_column.
          lwa_message-msgv2   = <lwa_at01>-excel_row.
          APPEND lwa_message TO lt_messages.
          CLEAR lwa_message.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
          lwa_message-msgid = sy-msgid.
          lwa_message-msgno = sy-msgno.
          lwa_message-msgty = sy-msgty.
          lwa_message-msgv1 = sy-msgv1.
          lwa_message-msgv2 = sy-msgv2.
          lwa_message-msgv3 = sy-msgv3.
          lwa_message-msgv4 = sy-msgv4.
          APPEND lwa_message TO lt_messages.
        ENDIF.
      ELSE.
        lv_terrain_w_error = <lwa_at01>-line_w_error = abap_true.
*-- Verificar Coluna &1 Linha &2 do Arquivo Excel.
        CLEAR lwa_message.
        lwa_message-msgid   = 'ZABS_MSGCLS'.
        lwa_message-msgno   = '143'.
        lwa_message-msgty   = 'I'.
        lwa_message-msgv1   = <lwa_at01>-excel_column.
        lwa_message-msgv2   = <lwa_at01>-excel_row.
        APPEND lwa_message TO lt_messages.
*-- Código do Terreno não informado.
        CLEAR lwa_message.
        lwa_message-msgid   = 'ZABS_MSGCLS'.
        lwa_message-msgno   = '144'.
        lwa_message-msgty   = 'I'.
        APPEND lwa_message TO lt_messages.
      ENDIF.

      lv_terrain = <lwa_at01>-tplnr_fl.

      IF lv_terrain_w_error EQ abap_false.
        INSERT INITIAL LINE INTO TABLE lrt_atnam
          ASSIGNING FIELD-SYMBOL(<lwa_atnam>).
        IF sy-subrc EQ 0.
          <lwa_atnam> = 'IEQ'.
          <lwa_atnam>-low = <lwa_at01>-atnam.
        ENDIF.

        INSERT INITIAL LINE INTO TABLE lrt_class
          ASSIGNING FIELD-SYMBOL(<lwa_class>).
        IF sy-subrc EQ 0.
          <lwa_class> = 'IEQ'.
          <lwa_class>-low = <lwa_at01>-class.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  DATA(lt_tplnr) = lt_at01[].

  SORT: lrt_class BY low,
        lrt_atnam BY low.

  DELETE ADJACENT DUPLICATES: FROM lt_tplnr  COMPARING tplnr_fl,
                              FROM lrt_class COMPARING low,
                              FROM lrt_atnam COMPARING low.

  SELECT h~clint, h~klart, h~class,
         h~statu, h~klagr,
         c~atinn, c~adzhl, c~atnam,
         c~atfor, c~anzst, c~anzdz,
         c~atsch, c~atkla, c~atmst
    FROM klah AS h
    INNER JOIN ksml AS k ON h~clint EQ k~clint
    INNER JOIN cabn AS c ON k~imerk EQ c~atinn
    INTO TABLE @lt_cabn
   WHERE h~class IN @lrt_class[]
     AND k~klart EQ @c_structures-klart
     AND c~atnam IN @lrt_atnam[].

  IF sy-subrc EQ 0.
    SORT lt_cabn BY class atnam atinn.
  ELSE.
*-- Verificar Grupo de Atributos.
    CLEAR lwa_message.
    lwa_message-msgid   = 'ZFMFP'.
    lwa_message-msgno   = '253'.
    lwa_message-msgty   = 'I'.
    APPEND lwa_message TO lt_messages.
    LEAVE LIST-PROCESSING.
  ENDIF.

  LOOP AT lt_tplnr ASSIGNING FIELD-SYMBOL(<lwa_tplnr>).
    IF <lwa_at01>-line_w_error EQ abap_true.
      CONTINUE.
    ENDIF.

    REFRESH lt_characteristics.
    READ TABLE lt_at01 TRANSPORTING NO FIELDS
      WITH KEY tplnr_fl = <lwa_tplnr>-tplnr_fl BINARY SEARCH.
    IF sy-subrc EQ 0.
      REFRESH lt_cabn_change.
      READ TABLE lt_at01 INTO DATA(lwa_at01) INDEX sy-tabix.
      WHILE sy-subrc EQ 0.
        DATA(lv_tabix) = sy-tabix + 1.

        READ TABLE lt_cabn INTO DATA(lwa_cabn)
          WITH KEY class = lwa_at01-class
                   atnam = lwa_at01-atnam BINARY SEARCH.
        IF sy-subrc EQ 0.
          lwa_at01-atinn = lwa_cabn-atinn.
          MODIFY lt_at01 FROM lwa_at01 INDEX sy-tabix TRANSPORTING atinn.
          APPEND lwa_at01 TO lt_characteristics.
          INSERT INITIAL LINE INTO TABLE lt_cabn_change
            ASSIGNING FIELD-SYMBOL(<lwa_cabn_change>).
          IF sy-subrc EQ 0.
            <lwa_cabn_change> = lwa_cabn.
          ENDIF.
        ENDIF.

        READ TABLE lt_at01 INTO lwa_at01 INDEX lv_tabix
          COMPARING tplnr_fl.
      ENDWHILE.

*-- CALL FUNCTION 'CLSE_SELECT_CAWN'

      IF lt_characteristics[] IS NOT INITIAL.
        REFRESH lt_mess_change.
        CALL FUNCTION 'ZABS_FM_GLFLM_CHANGE'
          EXPORTING
            iv_terrain         = <lwa_tplnr>-tplnr_fl
            it_characteristics = lt_characteristics
            it_cabn            = lt_cabn_change
          IMPORTING
            et_messages        = lt_mess_change.

        APPEND LINES OF lt_mess_change TO lt_mess_collect.
      ENDIF.
    ENDIF.
  ENDLOOP.

  et_messages = lt_mess_collect.

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


METHOD CALCULATE_COST_ORDER.

  DATA: lt_messages     TYPE /agri/t_gprolog,
        lt_bdcdata      TYPE bdcdata_tab,
        lt_bdc_messages TYPE ettcd_msg_tabtype,
        lwa_bdc_options TYPE ctu_params,
        lwa_message     TYPE /agri/s_gprolog,
        lv_aufnr_bdc    TYPE bdc_fval,
        lv_msgv1        TYPE sy-msgv1,
        lv_msgli        TYPE sy-msgli,
        lv_co02         TYPE sy-tcode VALUE 'CO02',
        lv_subrc        TYPE sysubrc.

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

  lwa_bdc_options-dismode  = 'N'.
  lwa_bdc_options-nobiend  = abap_false.
  lwa_bdc_options-updmode  = 'S'.
  lwa_bdc_options-nobinpt  = abap_false.

  CLEAR: mt_bdcdata[], ms_bdcdata, lt_bdc_messages[].

  lv_aufnr_bdc = iv_aufnr.
  dynpro_fill( i_program = 'SAPLCOKO1' i_dynpro  = '0110' ).
  field_fill( i_fnam = 'BDC_OKCODE'  i_fval = '=ENTK' ).
  field_fill( i_fnam = 'CAUFVD-AUFNR'  i_fval = lv_aufnr_bdc ).
  field_fill( i_fnam = 'R62CLORD-FLG_OVIEW'  i_fval = 'X' ).

  dynpro_fill( i_program = 'SAPLCOKO1' i_dynpro  = '0115' ).
  field_fill( i_fnam = 'BDC_OKCODE'  i_fval = '=KOER' ).

  dynpro_fill( i_program = 'SAPLCOKO1' i_dynpro  = '0115' ).
  field_fill( i_fnam = 'BDC_OKCODE'  i_fval = '=BU' ).

  REFRESH: lt_bdc_messages[].
  CALL METHOD /agri/cl_global_services=>transaction_call_process
    EXPORTING
      i_tcode                = lv_co02
      i_use_bdc_data         = mc_true
      is_bdc_options         = lwa_bdc_options
      it_bdcdata             = mt_bdcdata[]
    IMPORTING
      et_bdc_messages        = lt_bdc_messages
    EXCEPTIONS
      invalid_parameters     = 1
      call_transaction_error = 2
      OTHERS                 = 3.

  LOOP AT lt_bdc_messages INTO DATA(lwa_bdc_msg).
    CLEAR lwa_message.
    lwa_message-msgty   = lwa_bdc_msg-msgtyp.
    lwa_message-msgid   = lwa_bdc_msg-msgid.
    lwa_message-msgno   = lwa_bdc_msg-msgnr.
    lwa_message-msgv1   = lwa_bdc_msg-msgv1.
    lwa_message-msgv2   = lwa_bdc_msg-msgv2.
    lwa_message-msgv3   = lwa_bdc_msg-msgv3.
    lwa_message-msgv4   = lwa_bdc_msg-msgv4.
    APPEND lwa_message TO lt_messages.
  ENDLOOP.

  et_messages[] = lt_messages[].

ENDMETHOD.


METHOD CROP_MASTER_CREATE.

  CONSTANTS: BEGIN OF c_structures,
               cm_01 TYPE tabname VALUE '/AGRI/S_CM_SHEET1',
               cm_02 TYPE tabname VALUE '/AGRI/S_CM_SHEET2',
               cm_03 TYPE tabname VALUE '/AGRI/S_CM_SHEET3',
               cm_04 TYPE tabname VALUE '/AGRI/S_CM_SHEET4',
               cm_05 TYPE tabname VALUE '/AGRI/S_CM_SHEET5',
               cm_06 TYPE tabname VALUE '/AGRI/S_CM_SHEET6',
               cm_07 TYPE tabname VALUE '/AGRI/S_CM_SHEET7',
               cm_08 TYPE tabname VALUE '/AGRI/S_CM_SHEET8',
               cm_09 TYPE tabname VALUE '/AGRI/S_CM_SHEET9',
               cm_in TYPE updkz_d VALUE 'I',
             END OF c_structures.

  TYPES: BEGIN OF ls_fldty,
           fldty TYPE /agri/glcmtyp,
           numki TYPE numki,
         END OF ls_fldty.

  DATA: lt_table       TYPE /agri/t_excel_sheet,
        lwa_data       TYPE /agri/s_excel_sheet,
        lt_dd03l       TYPE thrpad_erd_dd03l,   "ishmed_t_dd03l,
        lwa_dd03l      TYPE dd03l,
        lv_balognr     TYPE balognr,
        lwa_message    TYPE /agri/s_gprolog,
        lt_messages    TYPE /agri/t_gprolog,
        lv_row         TYPE /agri/s_excel_sheet-row,

        lt_cm01        TYPE TABLE OF /agri/s_cm_sheet1,
        lt_cm02        TYPE TABLE OF /agri/s_cm_sheet2,
        lt_cm03        TYPE TABLE OF /agri/s_cm_sheet3,
        lt_cm04        TYPE TABLE OF /agri/s_cm_sheet4,
        lt_cm05        TYPE TABLE OF /agri/s_cm_sheet5,
        lt_cm06        TYPE TABLE OF /agri/s_cm_sheet6,
        lt_cm07        TYPE TABLE OF /agri/s_cm_sheet7,
        lt_cm08        TYPE TABLE OF /agri/s_cm_sheet8,
        lt_cm09        TYPE TABLE OF /agri/s_cm_sheet9,
        lt_tmpcmhdr    TYPE TABLE OF /agri/s_cm_sheet1,
        lt_tmpcmwrk    TYPE TABLE OF /agri/s_cm_sheet2,
        lt_tmpcmqch    TYPE TABLE OF /agri/s_cm_sheet3,
        lt_tmpcmvar    TYPE TABLE OF /agri/s_cm_sheet4,
        lt_tmpcmprs    TYPE TABLE OF /agri/s_cm_sheet5,
        lt_tmpcmpvr    TYPE TABLE OF /agri/s_cm_sheet6,
        lt_tmpcmprso   TYPE TABLE OF /agri/s_cm_sheet7,
        lt_tmpcmprst   TYPE TABLE OF /agri/s_cm_sheet8,
        lt_tmpcmdesc   TYPE TABLE OF /agri/s_cm_sheet9,
        lt_fldty       TYPE TABLE OF ls_fldty,
        ls_glcm_doc    TYPE /agri/s_glcm_doc,
        ls_tmpglcm_doc TYPE /agri/s_glcm_doc,
        lwa_cmhdr      TYPE /agri/s_glcmhdr,
        lwa_cmwrk      TYPE /agri/s_glcmwrk,
        lwa_cmqch      TYPE /agri/s_glcmqch,
        lwa_cmvar      TYPE /agri/s_glcmvar,
        lwa_cmprs      TYPE /agri/s_glcmprs,
        lwa_cmpvr      TYPE /agri/s_glcmpvr,
        lwa_cmprso     TYPE /agri/s_glcmprso,
        lwa_cmprst     TYPE /agri/s_glcmprst,
        lwa_cmdesc     TYPE /agri/s_glcmhdrt,
        lwa_fldty      TYPE ls_fldty.

  FIELD-SYMBOLS: <lwa_glcm_doc> TYPE /agri/s_glcm_doc,
                 <lwa_cmhdr>    TYPE /agri/s_glcmhdr,
                 <lwa_cm01>     TYPE /agri/s_cm_sheet1,
                 <lwa_cm02>     TYPE /agri/s_cm_sheet2,
                 <lwa_cm03>     TYPE /agri/s_cm_sheet3,
                 <lwa_cm04>     TYPE /agri/s_cm_sheet4,
                 <lwa_cm05>     TYPE /agri/s_cm_sheet5,
                 <lwa_cm06>     TYPE /agri/s_cm_sheet6,
                 <lwa_cm07>     TYPE /agri/s_cm_sheet7,
                 <lwa_cm08>     TYPE /agri/s_cm_sheet8,
                 <lwa_cm09>     TYPE /agri/s_cm_sheet9,
                 <lv_value>     TYPE any.

  lt_table[] = it_table[].

  DELETE lt_table WHERE row BETWEEN 1 AND 15
              OR column EQ 1.

  READ TABLE lt_table TRANSPORTING NO FIELDS
                      WITH KEY sheet = 01.
  CHECK sy-subrc EQ 0.
  MOVE c_structures-cm_in TO es_glcm_doc-updkz.

  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-cm_01
      i_sheet     = 1
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  LOOP AT lt_table INTO lwa_data WHERE sheet = 01.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_cm01 ASSIGNING <lwa_cm01>.
      lv_row = lwa_data-row.
    ENDIF.
    LOOP AT lt_dd03l INTO lwa_dd03l
                      WHERE position = lwa_data-column.
      ASSIGN COMPONENT lwa_data-fieldname
                  OF STRUCTURE <lwa_cm01> TO <lv_value>.
      <lv_value> = lwa_data-value.

    ENDLOOP.
  ENDLOOP.
  CLEAR lv_row.

  IF lt_cm01 IS NOT INITIAL.
    SELECT fldty numki
      FROM /agri/tglcmtyp
      INTO TABLE lt_fldty
      FOR ALL ENTRIES IN lt_cm01
       WHERE fldty EQ lt_cm01-fldty.
    SORT lt_cm01 BY fldty.
  ENDIF.

  LOOP AT lt_cm01 ASSIGNING <lwa_cm01>.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        input          = <lwa_cm01>-msehi
        language       = sy-langu
      IMPORTING
        output         = <lwa_cm01>-msehi
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.
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

    READ TABLE lt_fldty INTO lwa_fldty                   "#EC CI_SORTED
                WITH KEY fldty = <lwa_cm01>-fldty.
*                BINARY SEARCH.
    IF sy-subrc = 0.
      IF lwa_fldty-numki IS NOT INITIAL.
        CLEAR <lwa_cm01>-cmnum.
      ENDIF.
      <lwa_cm01>-updkz = c_structures-cm_in.
      APPEND <lwa_cm01> TO lt_tmpcmhdr.
    ELSE.
      lwa_message-msgid  = 'ZUPDMD'.
      lwa_message-msgno  = '000'.
      lwa_message-msgty  = 'E'.
      lwa_message-msgv1 = <lwa_cm01>-fldty.
      APPEND lwa_message TO lt_messages.
      APPEND LINES OF lt_messages TO et_messages.
    ENDIF.
  ENDLOOP.

  READ TABLE lt_table TRANSPORTING NO FIELDS
                       WITH KEY sheet = 02.
  IF sy-subrc EQ 0.
    CALL METHOD structure_build(
      EXPORTING
        i_structure = c_structures-cm_02
        i_sheet     = 2
      IMPORTING
        et_dd03l    = lt_dd03l
      CHANGING
        ct_table    = lt_table ).

    LOOP AT lt_table INTO lwa_data WHERE sheet = 02.
      IF lv_row NE lwa_data-row.
        APPEND INITIAL LINE TO lt_cm02 ASSIGNING <lwa_cm02>.
        lv_row = lwa_data-row.
      ENDIF.

      LOOP AT lt_dd03l INTO lwa_dd03l
                        WHERE position = lwa_data-column.
        ASSIGN COMPONENT lwa_data-fieldname
                    OF STRUCTURE <lwa_cm02> TO <lv_value>.
        <lv_value> = lwa_data-value.

      ENDLOOP.
    ENDLOOP.
    CLEAR lv_row.

    LOOP AT lt_cm02 ASSIGNING <lwa_cm02>.
      <lwa_cm02>-updkz = c_structures-cm_in.
      APPEND <lwa_cm02> TO lt_tmpcmwrk.
    ENDLOOP.
  ENDIF.

  READ TABLE lt_table TRANSPORTING NO FIELDS
                         WITH KEY sheet = 03.
  IF sy-subrc EQ 0.

    CALL METHOD structure_build(
      EXPORTING
        i_structure = c_structures-cm_03
        i_sheet     = 3
      IMPORTING
        et_dd03l    = lt_dd03l
      CHANGING
        ct_table    = lt_table ).

    LOOP AT lt_table INTO lwa_data WHERE sheet = 03.
      IF lv_row NE lwa_data-row.
        APPEND INITIAL LINE TO lt_cm03 ASSIGNING <lwa_cm03>.
        lv_row = lwa_data-row.
      ENDIF.
      LOOP AT lt_dd03l INTO lwa_dd03l
                        WHERE position = lwa_data-column.
        ASSIGN COMPONENT lwa_data-fieldname
                    OF STRUCTURE <lwa_cm03> TO <lv_value>.
        <lv_value> = lwa_data-value.

      ENDLOOP.
    ENDLOOP.
    CLEAR lv_row.

    LOOP AT lt_cm03 ASSIGNING <lwa_cm03>.
      <lwa_cm03>-updkz = c_structures-cm_in.
      APPEND <lwa_cm03> TO lt_tmpcmqch.
    ENDLOOP.
  ENDIF.

  READ TABLE lt_table TRANSPORTING NO FIELDS
                         WITH KEY sheet = 04.
  IF sy-subrc EQ 0.

    CALL METHOD structure_build(
      EXPORTING
        i_structure = c_structures-cm_04
        i_sheet     = 4
      IMPORTING
        et_dd03l    = lt_dd03l
      CHANGING
        ct_table    = lt_table ).

    LOOP AT lt_table INTO lwa_data WHERE sheet = 04.
      IF lv_row NE lwa_data-row.
        APPEND INITIAL LINE TO lt_cm04 ASSIGNING <lwa_cm04>.
        lv_row = lwa_data-row.
      ENDIF.
      LOOP AT lt_dd03l INTO lwa_dd03l
                        WHERE position = lwa_data-column.
        ASSIGN COMPONENT lwa_data-fieldname
                    OF STRUCTURE <lwa_cm04> TO <lv_value>.
        IF lwa_data-fieldname = 'VARIA'.
          TRANSLATE lwa_data-value TO UPPER CASE.
        ENDIF.
        <lv_value> = lwa_data-value.

      ENDLOOP.
    ENDLOOP.
    CLEAR lv_row.

    LOOP AT lt_cm04 ASSIGNING <lwa_cm04>.
      <lwa_cm04>-updkz = c_structures-cm_in.
      APPEND <lwa_cm04> TO lt_tmpcmvar.
    ENDLOOP.
  ENDIF.

  READ TABLE lt_table TRANSPORTING NO FIELDS
                         WITH KEY sheet = 05.
  IF sy-subrc EQ 0.

    CALL METHOD structure_build(
      EXPORTING
        i_structure = c_structures-cm_05
        i_sheet     = 5
      IMPORTING
        et_dd03l    = lt_dd03l
      CHANGING
        ct_table    = lt_table ).

    LOOP AT lt_table INTO lwa_data WHERE sheet = 05.
      IF lv_row NE lwa_data-row.
        APPEND INITIAL LINE TO lt_cm05 ASSIGNING <lwa_cm05>.
        lv_row = lwa_data-row.
      ENDIF.
      LOOP AT lt_dd03l INTO lwa_dd03l
                        WHERE position = lwa_data-column.
        ASSIGN COMPONENT lwa_data-fieldname
                    OF STRUCTURE <lwa_cm05> TO <lv_value>.
        IF lwa_data-fieldname = 'VARIA' OR lwa_data-fieldname = 'CPROS' OR lwa_data-fieldname = 'MATNR'.
          TRANSLATE lwa_data-value TO UPPER CASE.
        ELSEIF lwa_data-fieldname = 'MIUNT' OR lwa_data-fieldname = 'MAUNT'.
          CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
            EXPORTING
              input          = lwa_data-value
              language       = sy-langu
            IMPORTING
              output         = lwa_data-value
            EXCEPTIONS
              unit_not_found = 1
              OTHERS         = 2.
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
        ENDIF.
        <lv_value> = lwa_data-value.

      ENDLOOP.
    ENDLOOP.
    CLEAR lv_row.

    LOOP AT lt_cm05 ASSIGNING <lwa_cm05>.
      <lwa_cm05>-updkz = c_structures-cm_in.
      APPEND <lwa_cm05> TO lt_tmpcmprs.
    ENDLOOP.
  ENDIF.

  READ TABLE lt_table TRANSPORTING NO FIELDS
                         WITH KEY sheet = 06.
  IF sy-subrc EQ 0.

    CALL METHOD structure_build(
      EXPORTING
        i_structure = c_structures-cm_06
        i_sheet     = 6
      IMPORTING
        et_dd03l    = lt_dd03l
      CHANGING
        ct_table    = lt_table ).

    LOOP AT lt_table INTO lwa_data WHERE sheet = 06.
      IF lv_row NE lwa_data-row.
        APPEND INITIAL LINE TO lt_cm06 ASSIGNING <lwa_cm06>.
        lv_row = lwa_data-row.
      ENDIF.
      LOOP AT lt_dd03l INTO lwa_dd03l
                        WHERE position = lwa_data-column.
        ASSIGN COMPONENT lwa_data-fieldname
                    OF STRUCTURE <lwa_cm06> TO <lv_value>.
        IF lwa_data-fieldname = 'VARIA'.
          TRANSLATE lwa_data-value TO UPPER CASE.
        ENDIF.
        <lv_value> = lwa_data-value.

      ENDLOOP.
    ENDLOOP.
    CLEAR lv_row.

    LOOP AT lt_cm06 ASSIGNING <lwa_cm06>.
      <lwa_cm06>-updkz = c_structures-cm_in.
      APPEND <lwa_cm06> TO lt_tmpcmpvr.
    ENDLOOP.
  ENDIF.

  READ TABLE lt_table TRANSPORTING NO FIELDS
                         WITH KEY sheet = 07.
  IF sy-subrc EQ 0.
    CALL METHOD structure_build(
      EXPORTING
        i_structure = c_structures-cm_07
        i_sheet     = 7
      IMPORTING
        et_dd03l    = lt_dd03l
      CHANGING
        ct_table    = lt_table ).

    LOOP AT lt_table INTO lwa_data WHERE sheet = 07.
      IF lv_row NE lwa_data-row.
        APPEND INITIAL LINE TO lt_cm07 ASSIGNING <lwa_cm07>.
        lv_row = lwa_data-row.
      ENDIF.
      LOOP AT lt_dd03l INTO lwa_dd03l
                        WHERE position = lwa_data-column.
        ASSIGN COMPONENT lwa_data-fieldname
                    OF STRUCTURE <lwa_cm07> TO <lv_value>.
        IF lwa_data-fieldname = 'VARIA'.
          TRANSLATE lwa_data-value TO UPPER CASE.
        ENDIF.
        <lv_value> = lwa_data-value.

      ENDLOOP.
    ENDLOOP.
    CLEAR lv_row.

    LOOP AT lt_cm07 ASSIGNING <lwa_cm07>.
      <lwa_cm07>-updkz = c_structures-cm_in.
      APPEND <lwa_cm07> TO lt_tmpcmprso.
    ENDLOOP.
  ENDIF.

  READ TABLE lt_table TRANSPORTING NO FIELDS
                         WITH KEY sheet = 08.
  IF sy-subrc EQ 0.

    CALL METHOD structure_build(
      EXPORTING
        i_structure = c_structures-cm_08
        i_sheet     = 8
      IMPORTING
        et_dd03l    = lt_dd03l
      CHANGING
        ct_table    = lt_table ).

    LOOP AT lt_table INTO lwa_data WHERE sheet = 08.
      IF lv_row NE lwa_data-row.
        APPEND INITIAL LINE TO lt_cm08 ASSIGNING <lwa_cm08>.
        lv_row = lwa_data-row.
      ENDIF.
      LOOP AT lt_dd03l INTO lwa_dd03l
                        WHERE position = lwa_data-column.
        ASSIGN COMPONENT lwa_data-fieldname
                    OF STRUCTURE <lwa_cm08> TO <lv_value>.
        IF lwa_data-fieldname EQ 'SPRAS'.
          CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
            EXPORTING
              input  = lwa_data-value
            IMPORTING
              output = lwa_data-value.
        ENDIF.
        IF lwa_data-fieldname = 'VARIA'.
          TRANSLATE lwa_data-value TO UPPER CASE.
        ENDIF.
        <lv_value> = lwa_data-value.
      ENDLOOP.
    ENDLOOP.
    CLEAR lv_row.

    LOOP AT lt_cm08 ASSIGNING <lwa_cm08>.
      <lwa_cm08>-updkz = c_structures-cm_in.
      APPEND <lwa_cm08> TO lt_tmpcmprst.
    ENDLOOP.
  ENDIF.

  READ TABLE lt_table TRANSPORTING NO FIELDS
                         WITH KEY sheet = 09.
  IF sy-subrc EQ 0.

    CALL METHOD structure_build(
      EXPORTING
        i_structure = c_structures-cm_09
        i_sheet     = 9
      IMPORTING
        et_dd03l    = lt_dd03l
      CHANGING
        ct_table    = lt_table ).

    LOOP AT lt_table INTO lwa_data WHERE sheet = 09.
*      AT NEW row.
*        APPEND INITIAL LINE TO lt_cm09 ASSIGNING <lwa_cm09>.
*      ENDAT.
      IF lv_row NE lwa_data-row.
        APPEND INITIAL LINE TO lt_cm09 ASSIGNING <lwa_cm09>.
        lv_row = lwa_data-row.
      ENDIF.
      LOOP AT lt_dd03l INTO lwa_dd03l
                        WHERE position = lwa_data-column.
        ASSIGN COMPONENT lwa_data-fieldname
                    OF STRUCTURE <lwa_cm09> TO <lv_value>.
        IF lwa_data-fieldname EQ 'SPRAS'.
          CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
            EXPORTING
              input  = lwa_data-value
            IMPORTING
              output = lwa_data-value.
        ENDIF.
        <lv_value> = lwa_data-value.

      ENDLOOP.
    ENDLOOP.
    CLEAR lv_row.

    LOOP AT lt_cm09 ASSIGNING <lwa_cm09>.
      <lwa_cm09>-updkz = c_structures-cm_in.
      APPEND <lwa_cm09> TO lt_tmpcmdesc.
    ENDLOOP.
  ENDIF.

  CHECK es_glcm_doc IS NOT INITIAL.

  LOOP AT lt_tmpcmhdr ASSIGNING <lwa_cm01>.
    MOVE-CORRESPONDING <lwa_cm01> TO ls_glcm_doc-x-cmhdr.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ls_glcm_doc-x-cmhdr-cmnum
      IMPORTING
        output = ls_glcm_doc-x-cmhdr-cmnum.
    IF lt_tmpcmwrk IS NOT INITIAL.
      LOOP AT lt_tmpcmwrk ASSIGNING <lwa_cm02>
                        WHERE contr EQ <lwa_cm01>-contr.
        MOVE-CORRESPONDING <lwa_cm02> TO lwa_cmwrk.
        lwa_cmwrk-cmnum = ls_glcm_doc-x-cmhdr-cmnum.
        APPEND lwa_cmwrk TO ls_glcm_doc-x-cmwrk.
      ENDLOOP.
    ENDIF.
    IF lt_tmpcmqch IS NOT INITIAL.
      LOOP AT lt_tmpcmqch ASSIGNING <lwa_cm03>
                        WHERE contr EQ <lwa_cm01>-contr.
        MOVE-CORRESPONDING <lwa_cm03> TO lwa_cmqch.
        lwa_cmqch-cmnum = ls_glcm_doc-x-cmhdr-cmnum.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lwa_cmqch-version
          IMPORTING
            output = lwa_cmqch-version.
        APPEND lwa_cmqch TO ls_glcm_doc-x-cmqch.
      ENDLOOP.
    ENDIF.
    IF lt_tmpcmvar IS NOT INITIAL.
      LOOP AT lt_tmpcmvar ASSIGNING <lwa_cm04>
                        WHERE contr EQ <lwa_cm01>-contr.
        MOVE-CORRESPONDING <lwa_cm04> TO lwa_cmvar.
        lwa_cmvar-cmnum = ls_glcm_doc-x-cmhdr-cmnum.
        APPEND lwa_cmvar TO ls_glcm_doc-x-cmvar.
      ENDLOOP.
    ENDIF.
    IF lt_tmpcmprs IS NOT INITIAL.
      LOOP AT lt_tmpcmprs ASSIGNING <lwa_cm05>
                        WHERE contr EQ <lwa_cm01>-contr.
        MOVE-CORRESPONDING <lwa_cm05> TO lwa_cmprs.
        lwa_cmprs-cmnum = ls_glcm_doc-x-cmhdr-cmnum.
        APPEND lwa_cmprs TO ls_glcm_doc-x-cmprs.
      ENDLOOP.
    ENDIF.
    IF lt_tmpcmpvr IS NOT INITIAL.
      LOOP AT lt_tmpcmpvr ASSIGNING <lwa_cm06>
                        WHERE contr EQ <lwa_cm01>-contr.
        MOVE-CORRESPONDING <lwa_cm06> TO lwa_cmpvr.
        lwa_cmpvr-cmnum = ls_glcm_doc-x-cmhdr-cmnum.
        APPEND lwa_cmpvr TO ls_glcm_doc-x-cmpvr.
      ENDLOOP.
    ENDIF.
    IF lt_tmpcmprso IS NOT INITIAL.
      LOOP AT lt_tmpcmprso ASSIGNING <lwa_cm07>
                        WHERE contr EQ <lwa_cm01>-contr.
        MOVE-CORRESPONDING <lwa_cm07> TO lwa_cmprso.
        lwa_cmprso-cmnum = ls_glcm_doc-x-cmhdr-cmnum.
        APPEND lwa_cmprso TO ls_glcm_doc-x-cmprso.
      ENDLOOP.
    ENDIF.
    IF lt_tmpcmprst IS NOT INITIAL.
      LOOP AT lt_tmpcmprst ASSIGNING <lwa_cm08>
                        WHERE contr EQ <lwa_cm01>-contr.
        MOVE-CORRESPONDING <lwa_cm08> TO lwa_cmprst.
        lwa_cmprst-cmnum = ls_glcm_doc-x-cmhdr-cmnum.
        APPEND lwa_cmprst TO ls_glcm_doc-x-cmprst.
      ENDLOOP.
    ENDIF.
    IF lt_tmpcmdesc IS NOT INITIAL.
      LOOP AT lt_tmpcmdesc ASSIGNING <lwa_cm09>
                        WHERE contr EQ <lwa_cm01>-contr.
        MOVE-CORRESPONDING <lwa_cm09> TO lwa_cmdesc.
        lwa_cmdesc-cmnum = ls_glcm_doc-x-cmhdr-cmnum.
        APPEND lwa_cmdesc TO ls_glcm_doc-x-cmdesc.
      ENDLOOP.
    ENDIF.
*{   INSERT         SS8K900620                                        1
***Date check
    DATA: lv_subrc TYPE sy-subrc.

    LOOP AT ls_glcm_doc-x-cmwrk INTO DATA(ls_cmwrk).
      IF ls_cmwrk-datab IS NOT INITIAL.
        CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
          EXPORTING
            date                      = ls_cmwrk-datab
          EXCEPTIONS
            plausibility_check_failed = 1
            OTHERS                    = 2.
        IF sy-subrc <> 0.
          lwa_message-msgid = '/AGRI/GLAG'.
          lwa_message-msgno = '003'.
          lwa_message-msgty = 'E'.
          lwa_message-msgv1 = ls_glcm_doc-x-cmhdr-cmnum.
          lwa_message-msgv2 = ls_cmwrk-werks.
          APPEND lwa_message TO et_messages.
          lv_subrc = sy-subrc.
        ENDIF.
      ENDIF.

      IF ls_cmwrk-datbi IS NOT INITIAL.
        CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
          EXPORTING
            date                      = ls_cmwrk-datbi
          EXCEPTIONS
            plausibility_check_failed = 1
            OTHERS                    = 2.
        IF sy-subrc <> 0.
          lwa_message-msgid = '/AGRI/GLAG'.
          lwa_message-msgno = '004'.
          lwa_message-msgty = 'E'.
          lwa_message-msgv1 = ls_glcm_doc-x-cmhdr-cmnum.
          lwa_message-msgv2 = ls_cmwrk-werks.
          APPEND lwa_message TO et_messages.
          lv_subrc = sy-subrc.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF lv_subrc IS NOT INITIAL.
      CLEAR ls_glcm_doc.
      CONTINUE.
    ENDIF.
*}   INSERT

*ENHANCEMENT-POINT CROP_MASTER_CREATE SPOTS /AGRI/ES_CL_UPLOAD_MASTER_DATA .

    CALL FUNCTION '/AGRI/GLCM_CREATE'
      EXPORTING
*       i_messages_display      = ' '
*       i_save_messages         = ' '
        i_commit_work           = 'X'
        is_cmhdr                = ls_glcm_doc-x-cmhdr
        it_cmwrk                = ls_glcm_doc-x-cmwrk
        it_cmqch                = ls_glcm_doc-x-cmqch
        it_cmvar                = ls_glcm_doc-x-cmvar
        it_cmprs                = ls_glcm_doc-x-cmprs
        it_cmpvr                = ls_glcm_doc-x-cmpvr
        it_cmprso               = ls_glcm_doc-x-cmprso
        it_cmprst               = ls_glcm_doc-x-cmprst
        it_cmdesc               = ls_glcm_doc-x-cmdesc
      IMPORTING
        es_glcm_doc             = ls_tmpglcm_doc
        et_messages             = lt_messages
        e_log_number            = lv_balognr
      EXCEPTIONS
        no_documents_to_process = 1
        no_authorization        = 2
        creation_failed         = 3
        OTHERS                  = 4.

    IF sy-subrc <> 0.
      IF sy-msgid IS INITIAL.
        lwa_message-msgid  = 'ZUPDMD'.
        lwa_message-msgno  = '001'.
        lwa_message-msgty  = 'E'.
        APPEND lwa_message TO lt_messages.
      ELSE.
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
    ENDIF.


    APPEND LINES OF lt_messages TO et_messages.
    CLEAR: ls_glcm_doc,
           lwa_message.

  ENDLOOP.


ENDMETHOD.


METHOD CROP_SEASON_CHANGE.

  DATA: lt_table      TYPE /agri/t_excel_sheet,
        lt_cs         TYPE TABLE OF zabs_str_season,
        lt_flcma_chng TYPE /agri/t_glflcma,
        lt_csdoc      TYPE /agri/t_glcs_doc,
        lt_messages   TYPE /agri/t_gprolog,
        lt_dd03l      TYPE thrpad_erd_dd03l,
        lt_flcma_bd   TYPE STANDARD TABLE OF /agri/glflcma INITIAL SIZE 0,
        lwa_flcma_bd  TYPE /agri/glflcma,
        lwa_data      TYPE /agri/s_excel_sheet,
        lwa_dd03l     TYPE dd03l,
        lwa_message   TYPE /agri/s_gprolog,
        lv_log_number TYPE balognr,
        lv_row        TYPE /agri/s_excel_sheet-row.

  FIELD-SYMBOLS: <lwa_cs>   TYPE zabs_str_season,
                 <lv_value> TYPE any.

  CONSTANTS: BEGIN OF c_cons,
               cs    TYPE tabname VALUE 'ZABS_STR_SEASON',
               cs_in TYPE updkz   VALUE 'I',
             END OF c_cons,

             c_updkz_update(1) VALUE 'U',

             BEGIN OF c_crop_season_status,
               active   TYPE /agri/glastat VALUE 'A',
               inactive TYPE /agri/glastat VALUE 'I',
               closed   TYPE /agri/glastat VALUE 'C',
             END OF c_crop_season_status.

  DEFINE add_first_line.
*-- Verificar Coluna &1 Linha &2.
    CLEAR lwa_message.
    lwa_message-msgid = 'ZABS_MSGCLS'.
    lwa_message-msgno = '143'.
    lwa_message-msgty = 'I'.
    lwa_message-msgv1 = &1.
    lwa_message-msgv2 = &2.
    APPEND lwa_message TO lt_messages.
  END-OF-DEFINITION.

  lt_table[] = it_table[].

  DELETE lt_table WHERE row = 1.
  DELETE lt_table WHERE sheet <> 1.

  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_cons-cs
      i_sheet     = 1
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  LOOP AT lt_table INTO lwa_data WHERE sheet = 01.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_cs ASSIGNING <lwa_cs>.
      lv_row = lwa_data-row.
    ENDIF.

    READ TABLE lt_dd03l TRANSPORTING NO FIELDS
      WITH KEY position = lwa_data-column.
    IF sy-subrc EQ 0.
      LOOP AT lt_dd03l INTO lwa_dd03l FROM sy-tabix.
        IF lwa_dd03l-position NE lwa_data-column.
          EXIT.
        ENDIF.
        ASSIGN COMPONENT lwa_data-fieldname
          OF STRUCTURE <lwa_cs> TO <lv_value>.
        IF sy-subrc EQ 0.
          <lv_value> = lwa_data-value.
        ENDIF.
      ENDLOOP.
    ENDIF.

    <lwa_cs>-excel_row = lwa_data-row.
    <lwa_cs>-excel_column = lwa_data-column.
  ENDLOOP.
  CLEAR lv_row.

  LOOP AT lt_cs ASSIGNING <lwa_cs>.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <lwa_cs>-cmnum
      IMPORTING
        output = <lwa_cs>-cmnum.

    CALL FUNCTION '/AGRI/G_CONV_EXIT_TPLNR_INPUT'
      EXPORTING
        i_input  = <lwa_cs>-tplnr_fl
      IMPORTING
        e_output = <lwa_cs>-tplnr_fl
      EXCEPTIONS
        OTHERS   = 1.

    IF sy-subrc NE 0.
      <lwa_cs>-line_w_error = abap_true.
    ENDIF.
  ENDLOOP.

  IF lt_cs[] IS NOT INITIAL.
    SELECT *
      FROM /agri/glflcma
      INTO TABLE @lt_flcma_bd
      FOR ALL ENTRIES IN @lt_cs
     WHERE tplnr_fl = @lt_cs-tplnr_fl
       AND astat    = @c_crop_season_status-active
       AND loevm    = @abap_false.

    SORT lt_flcma_bd BY tplnr_fl ASCENDING
                        contr    DESCENDING.

    DELETE ADJACENT DUPLICATES FROM lt_flcma_bd COMPARING tplnr_fl.

    SORT lt_flcma_bd BY tplnr_fl cmnum varia season.
  ENDIF.

  LOOP AT lt_cs ASSIGNING <lwa_cs>.
    IF <lwa_cs>-line_w_error = abap_false.
      REFRESH lt_flcma_chng.
      READ TABLE lt_flcma_bd INTO lwa_flcma_bd
        WITH KEY tplnr_fl = <lwa_cs>-tplnr_fl
                 cmnum    = <lwa_cs>-cmnum
                 varia    = <lwa_cs>-varia
                 season   = <lwa_cs>-season BINARY SEARCH.
      IF sy-subrc EQ 0.
        INSERT INITIAL LINE INTO TABLE lt_flcma_chng
         ASSIGNING FIELD-SYMBOL(<ls_flcma_chng>).
        IF sy-subrc EQ 0.
          MOVE-CORRESPONDING lwa_flcma_bd TO <ls_flcma_chng>.
          <ls_flcma_chng>-anlnr = <lwa_cs>-anlnr.
          <ls_flcma_chng>-updkz = c_updkz_update.

          IF <ls_flcma_chng>-anlnr IS NOT INITIAL.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = <ls_flcma_chng>-anlnr
              IMPORTING
                output = <ls_flcma_chng>-anlnr.
          ENDIF.

          IF <ls_flcma_chng>-kostl IS NOT INITIAL.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = <ls_flcma_chng>-kostl
              IMPORTING
                output = <ls_flcma_chng>-kostl.
          ENDIF.

          CALL FUNCTION '/AGRI/GLCS_CHANGE'
            EXPORTING
              it_flcma                = lt_flcma_chng
            IMPORTING
              et_messages             = lt_messages
            CHANGING
              ct_csdoc                = lt_csdoc
            EXCEPTIONS
              no_documents_to_process = 1
              change_failed           = 2
              crop_locked             = 3
              OTHERS                  = 4.

          IF sy-subrc <> 0.
            IF <lwa_cs>-line_w_error EQ abap_false.
              <lwa_cs>-line_w_error = abap_true.
*-- Verificar Coluna &1 Linha &2.
              add_first_line <lwa_cs>-excel_column <lwa_cs>-excel_row.
            ENDIF.
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
            APPEND lwa_message TO et_messages.
          ENDIF.
        ENDIF.
      ELSE.
        IF <lwa_cs>-line_w_error EQ abap_false.
          <lwa_cs>-line_w_error = abap_true.
*-- Verificar Coluna &1 Linha &2.
          add_first_line <lwa_cs>-excel_column <lwa_cs>-excel_row.
        ENDIF.
*-- Terreno &1 Cultura &2 Variante &3 Safra &4 não encontrado
        CLEAR lwa_message.
        lwa_message-msgid = 'ZFMFP'.
        lwa_message-msgno = 150.
        lwa_message-msgty = 'E'.
        lwa_message-msgv1 = <lwa_cs>-tplnr_fl.
        lwa_message-msgv1 = <lwa_cs>-cmnum.
        lwa_message-msgv1 = <lwa_cs>-varia.
        lwa_message-msgv1 = <lwa_cs>-season.
        APPEND lwa_message TO lt_messages.
      ENDIF.
    ELSEIF <lwa_cs>-line_w_error = abap_true.
      IF <lwa_cs>-line_w_error EQ abap_false.
        <lwa_cs>-line_w_error = abap_true.
*-- Verificar Coluna &1 Linha &2.
        add_first_line <lwa_cs>-excel_column <lwa_cs>-excel_row.
      ENDIF.
*-- Terreno &1 inexiste.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZFMFP'.
      lwa_message-msgno = 149.
      lwa_message-msgty = 'E'.
      lwa_message-msgv1 = <lwa_cs>-tplnr_fl.
      APPEND lwa_message TO lt_messages.
    ENDIF.
  ENDLOOP.

  et_messages[] = lt_messages[].

ENDMETHOD.


METHOD CROP_SEASON_CREATE.

  DATA: lt_cs         TYPE TABLE OF /agri/s_cs_sheet,
        lt_flcma      TYPE /agri/t_glflcma,
        lt_csdoc      TYPE /agri/t_glcs_doc,
        lwa_flcma     TYPE /agri/s_glflcma,
        lv_log_number TYPE balognr,
        lwa_data      TYPE /agri/s_excel_sheet,
        lt_dd03l      TYPE thrpad_erd_dd03l,   "ishmed_t_dd03l,
        lwa_dd03l     TYPE dd03l,
        lwa_message   TYPE /agri/s_gprolog.

  FIELD-SYMBOLS: <lwa_cs>   TYPE /agri/s_cs_sheet,
                 <lv_value> TYPE any.

  CONSTANTS: BEGIN OF c_cons,
               cs    TYPE tabname VALUE '/AGRI/S_CS_SHEET',
               cs_in TYPE updkz   VALUE 'I',
             END OF c_cons.

  et_table[] = it_table[].
  DELETE et_table WHERE row BETWEEN 1 AND 15
             OR column EQ 1.

  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_cons-cs
      i_sheet     = 1
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = et_table ).

  LOOP AT et_table INTO lwa_data.
    AT NEW row.
      APPEND INITIAL LINE TO lt_cs ASSIGNING <lwa_cs>.
    ENDAT.
    LOOP AT lt_dd03l INTO lwa_dd03l
                      WHERE position = lwa_data-column.
      ASSIGN COMPONENT lwa_data-fieldname
                  OF STRUCTURE <lwa_cs> TO <lv_value>.
      <lv_value> = lwa_data-value.

    ENDLOOP.
  ENDLOOP.

  LOOP AT lt_cs ASSIGNING <lwa_cs>.
    MOVE-CORRESPONDING <lwa_cs> TO lwa_flcma.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lwa_flcma-cmnum
      IMPORTING
        output = lwa_flcma-cmnum.
*    CALL FUNCTION 'CONVERSION_EXIT_TPLNR_INPUT'
*      EXPORTING
*        input                = lwa_flcma-tplnr_fl
*        i_flg_check_internal = 'X'
*      IMPORTING
*        output               = lwa_flcma-tplnr_fl
*      EXCEPTIONS
*        not_found            = 1
*        OTHERS               = 2.
    CALL FUNCTION '/AGRI/G_CONV_EXIT_TPLNR_INPUT'
      EXPORTING
        i_input  = lwa_flcma-tplnr_fl
*       I_NO_MESSAGE       =
      IMPORTING
        e_output = lwa_flcma-tplnr_fl
*       ES_RETURN          =
      EXCEPTIONS
        OTHERS   = 1.

    IF sy-subrc EQ 0.
      lwa_flcma-datab(4) = <lwa_cs>-gyear.
      lwa_flcma-updkz      = c_cons-cs_in.
      APPEND lwa_flcma TO lt_flcma.
    ENDIF.
    CLEAR lwa_flcma.
  ENDLOOP.

*ENHANCEMENT-POINT CROP_SEASON_CREATE SPOTS /AGRI/ES_CL_UPLOAD_MASTER_DATA .

  CALL FUNCTION '/AGRI/GLCS_CREATE'
    EXPORTING
*     I_MESSAGES_DISPLAY      = ' '
*     I_SAVE_MESSAGES         = ' '
*     I_COMMIT_WORK           = 'X'
*     I_SYNCHRONOUS           = ' '
      it_flcma                = lt_flcma
    IMPORTING
      et_csdoc                = lt_csdoc
      et_messages             = et_messages
      e_log_number            = lv_log_number
    EXCEPTIONS
      no_documents_to_process = 1
      no_authorization        = 2
      creation_failed         = 3
      OTHERS                  = 4.
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
    APPEND lwa_message TO et_messages.
  ENDIF.


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


METHOD IRRIGATION_EQUIP_CREATE.

  CONSTANTS: BEGIN OF c_structures,
      ie_01 TYPE tabname VALUE '/AGRI/S_IE_SHEET1',
      ie_02 TYPE tabname VALUE '/AGRI/S_IE_SHEET2',
      ie_03 TYPE tabname VALUE '/AGRI/S_IE_SHEET3',
      ie_04 TYPE tabname VALUE '/AGRI/S_IE_SHEET4',
      ie_05 TYPE tabname VALUE '/AGRI/S_IE_SHEET5',
      ie_in TYPE updkz_d VALUE 'I',
      END OF c_structures.

  TYPES: BEGIN OF ls_irtyp,
           irtyp TYPE /agri/fmirtyp,
           numki TYPE numki,
         END OF ls_irtyp.

  DATA:   lt_table        TYPE /agri/t_excel_sheet,
          lwa_data        TYPE /agri/s_excel_sheet,
          lt_dd03l        TYPE thrpad_erd_dd03l,    "ishmed_t_dd03l,
          lwa_dd03l       TYPE dd03l,
          lv_balognr      TYPE balognr,
          lwa_message     TYPE /agri/s_gprolog,
          lt_messages     TYPE /agri/t_gprolog,
          lv_row          TYPE /agri/s_excel_sheet-row,

          lt_ie01         TYPE TABLE OF /agri/s_ie_sheet1,
          lt_ie02         TYPE TABLE OF /agri/s_ie_sheet2,
          lt_ie03         TYPE TABLE OF /agri/s_ie_sheet3,
          lt_ie04         TYPE TABLE OF /agri/s_ie_sheet4,
          lt_ie05         TYPE TABLE OF /agri/s_ie_sheet5,
          lt_tpmirhdr     TYPE TABLE OF /agri/s_ie_sheet1,
          lt_tmpirwrk     TYPE TABLE OF /agri/s_ie_sheet2,
          lt_tmpirflo     TYPE TABLE OF /agri/s_ie_sheet3,
          lt_tmpirmea     TYPE TABLE OF /agri/s_ie_sheet4,
          lt_tmpirhdrt    TYPE TABLE OF /agri/s_ie_sheet5,
          lt_klah         TYPE TABLE OF klah,
          lt_irtyp        TYPE TABLE OF ls_irtyp,
          ls_klah         TYPE klah,
          ls_fmir_doc     TYPE /agri/s_fmir_doc,
          ls_tmpfmir_doc  TYPE /agri/s_fmir_doc,
          lwa_irwrk       TYPE /agri/s_fmirwrk,
          lwa_irflo       TYPE /agri/s_fmirflo,
          lwa_irmea       TYPE /agri/s_fmirmea,
          lwa_irhdrt      TYPE /agri/s_fmirhdrt,
          lwa_irtyp       TYPE ls_irtyp.

  FIELD-SYMBOLS: <lwa_ie01>     TYPE /agri/s_ie_sheet1,
                 <lwa_ie02>     TYPE /agri/s_ie_sheet2,
                 <lwa_ie03>     TYPE /agri/s_ie_sheet3,
                 <lwa_ie04>     TYPE /agri/s_ie_sheet4,
                 <lwa_ie05>     TYPE /agri/s_ie_sheet5,
                 <lv_value>     TYPE any.

  lt_table[] = it_table[].

  DELETE lt_table WHERE row BETWEEN 1 AND 15   "#EC CI_STDSEQ
              OR column EQ 1.

  READ TABLE lt_table TRANSPORTING NO FIELDS   "#EC CI_STDSEQ
                      WITH KEY sheet = 01.
  IF sy-subrc EQ 0.
    CALL METHOD structure_build(
      EXPORTING
        i_structure = c_structures-ie_01
        i_sheet     = 1
      IMPORTING
        et_dd03l    = lt_dd03l
      CHANGING
        ct_table    = lt_table ).

    LOOP AT lt_table INTO lwa_data WHERE sheet = 01.  "#EC CI_STDSEQ
*      AT NEW row.
*        APPEND INITIAL LINE TO lt_ie01 ASSIGNING <lwa_ie01>.
*      ENDAT.
      IF lv_row NE lwa_data-row.
        APPEND INITIAL LINE TO lt_ie01 ASSIGNING <lwa_ie01>.
        lv_row = lwa_data-row.
      ENDIF.
      LOOP AT lt_dd03l INTO lwa_dd03l
                        WHERE position = lwa_data-column. "#EC CI_STDSEQ
        ASSIGN COMPONENT lwa_data-fieldname
                    OF STRUCTURE <lwa_ie01> TO <lv_value>.
        <lv_value> = lwa_data-value.

      ENDLOOP.
    ENDLOOP.
    CLEAR lv_row.

    IF lt_ie01 IS NOT INITIAL.
      SELECT irtyp numki
        FROM /agri/tfmirtyp
       INTO TABLE lt_irtyp
        FOR ALL ENTRIES IN lt_ie01
        WHERE irtyp EQ lt_ie01-irtyp.
      SORT lt_irtyp BY irtyp.
    ENDIF.

    LOOP AT lt_ie01 ASSIGNING <lwa_ie01>.
      READ TABLE lt_irtyp INTO lwa_irtyp
                     WITH KEY irtyp = <lwa_ie01>-irtyp
                     BINARY SEARCH.
      IF lwa_irtyp-numki IS NOT INITIAL.
        CLEAR <lwa_ie01>-equnr.
      ENDIF.
      <lwa_ie01>-updkz = c_structures-ie_in.
      APPEND <lwa_ie01> TO lt_tpmirhdr.
    ENDLOOP.
  ENDIF.

  READ TABLE lt_table TRANSPORTING NO FIELDS  "#EC CI_STDSEQ
                       WITH KEY sheet = 02.
  IF sy-subrc EQ 0.
    CALL METHOD structure_build(
      EXPORTING
        i_structure = c_structures-ie_02
        i_sheet     = 2
      IMPORTING
        et_dd03l    = lt_dd03l
      CHANGING
        ct_table    = lt_table ).

    LOOP AT lt_table INTO lwa_data WHERE sheet = 02.   "#EC CI_STDSEQ
*      AT NEW row.
*        APPEND INITIAL LINE TO lt_ie02 ASSIGNING <lwa_ie02>.
*      ENDAT.
      IF lv_row NE lwa_data-row.
        APPEND INITIAL LINE TO lt_ie02 ASSIGNING <lwa_ie02>.
        lv_row = lwa_data-row.
      ENDIF.
      LOOP AT lt_dd03l INTO lwa_dd03l
                        WHERE position = lwa_data-column.  "#EC CI_STDSEQ
        ASSIGN COMPONENT lwa_data-fieldname
                    OF STRUCTURE <lwa_ie02> TO <lv_value>.
        <lv_value> = lwa_data-value.

      ENDLOOP.
    ENDLOOP.
    CLEAR lv_row.

    LOOP AT lt_ie02 ASSIGNING <lwa_ie02>.
      <lwa_ie02>-updkz = c_structures-ie_in.
      APPEND <lwa_ie02> TO lt_tmpirwrk.
    ENDLOOP.
  ENDIF.

  READ TABLE lt_table TRANSPORTING NO FIELDS  "#EC CI_STDSEQ
                       WITH KEY sheet = 03.
  IF sy-subrc EQ 0.
    CALL METHOD structure_build(
      EXPORTING
        i_structure = c_structures-ie_03
        i_sheet     = 3
      IMPORTING
        et_dd03l    = lt_dd03l
      CHANGING
        ct_table    = lt_table ).

    LOOP AT lt_table INTO lwa_data WHERE sheet = 03.  "#EC CI_STDSEQ
*      AT NEW row.
*        APPEND INITIAL LINE TO lt_ie03 ASSIGNING <lwa_ie03>.
*      ENDAT.
      IF lv_row NE lwa_data-row.
        APPEND INITIAL LINE TO lt_ie03 ASSIGNING <lwa_ie03>.
        lv_row = lwa_data-row.
      ENDIF.
      LOOP AT lt_dd03l INTO lwa_dd03l
                        WHERE position = lwa_data-column. "#EC CI_STDSEQ
        ASSIGN COMPONENT lwa_data-fieldname
                    OF STRUCTURE <lwa_ie03> TO <lv_value>.
        <lv_value> = lwa_data-value.

      ENDLOOP.
    ENDLOOP.
    CLEAR lv_row.

    LOOP AT lt_ie03 ASSIGNING <lwa_ie03>.
      <lwa_ie03>-updkz = c_structures-ie_in.
      APPEND <lwa_ie03> TO lt_tmpirflo.
    ENDLOOP.
  ENDIF.

  READ TABLE lt_table TRANSPORTING NO FIELDS  "#EC CI_STDSEQ
                       WITH KEY sheet = 04.
  IF sy-subrc EQ 0.
    CALL METHOD structure_build(
      EXPORTING
        i_structure = c_structures-ie_04
        i_sheet     = 4
      IMPORTING
        et_dd03l    = lt_dd03l
      CHANGING
        ct_table    = lt_table ).

    LOOP AT lt_table INTO lwa_data WHERE sheet = 04. "#EC CI_STDSEQ
*      AT NEW row.
*        APPEND INITIAL LINE TO lt_ie04 ASSIGNING <lwa_ie04>.
*      ENDAT.
      IF lv_row NE lwa_data-row.
        APPEND INITIAL LINE TO lt_ie04 ASSIGNING <lwa_ie04>.
        lv_row = lwa_data-row.
      ENDIF.
      LOOP AT lt_dd03l INTO lwa_dd03l
                        WHERE position = lwa_data-column.  "#EC CI_STDSEQ
        ASSIGN COMPONENT lwa_data-fieldname
                    OF STRUCTURE <lwa_ie04> TO <lv_value>.
        <lv_value> = lwa_data-value.

      ENDLOOP.
    ENDLOOP.
    CLEAR lv_row.

    IF lt_ie04 IS NOT INITIAL.
      SELECT * FROM  klah "#EC CI_ALL_FIELDS_NEEDED
        INTO TABLE lt_klah
        FOR ALL ENTRIES IN lt_ie04
        WHERE class EQ lt_ie04-class.
      SORT lt_klah BY class.
    ENDIF.

    LOOP AT lt_ie04 ASSIGNING <lwa_ie04>.
      READ TABLE lt_klah INTO ls_klah
                    WITH KEY class = <lwa_ie04>-class
                    BINARY SEARCH.
      <lwa_ie04>-class = ls_klah-class.
      <lwa_ie04>-updkz = c_structures-ie_in.
      APPEND <lwa_ie04> TO lt_tmpirmea.
    ENDLOOP.
  ENDIF.

  READ TABLE lt_table TRANSPORTING NO FIELDS   "#EC CI_STDSEQ
                       WITH KEY sheet = 05.
  IF sy-subrc EQ 0.
    CALL METHOD structure_build(
      EXPORTING
        i_structure = c_structures-ie_05
        i_sheet     = 5
      IMPORTING
        et_dd03l    = lt_dd03l
      CHANGING
        ct_table    = lt_table ).

    LOOP AT lt_table INTO lwa_data WHERE sheet = 05.  "#EC CI_STDSEQ
*      AT NEW row.
*        APPEND INITIAL LINE TO lt_ie05 ASSIGNING <lwa_ie05>.
*      ENDAT.
      IF lv_row NE lwa_data-row.
        APPEND INITIAL LINE TO lt_ie05 ASSIGNING <lwa_ie05>.
        lv_row = lwa_data-row.
      ENDIF.
      LOOP AT lt_dd03l INTO lwa_dd03l
                        WHERE position = lwa_data-column.  "#EC CI_STDSEQ
        ASSIGN COMPONENT lwa_data-fieldname
                    OF STRUCTURE <lwa_ie05> TO <lv_value>.
        <lv_value> = lwa_data-value.

      ENDLOOP.
    ENDLOOP.
    CLEAR lv_row.

    LOOP AT lt_ie05 ASSIGNING <lwa_ie05> WHERE spras <> sy-langu. "#EC CI_STDSEQ
      <lwa_ie05>-updkz = c_structures-ie_in.
      APPEND <lwa_ie05> TO lt_tmpirhdrt.
    ENDLOOP.
  ENDIF.

  LOOP AT lt_tpmirhdr ASSIGNING <lwa_ie01>.
    MOVE-CORRESPONDING <lwa_ie01> TO ls_fmir_doc-x-irhdr.
    IF lt_tmpirwrk IS NOT INITIAL.
      LOOP AT lt_tmpirwrk ASSIGNING <lwa_ie02>
                        WHERE contr EQ <lwa_ie01>-contr.  "#EC CI_STDSEQ
        MOVE-CORRESPONDING <lwa_ie02> TO lwa_irwrk.
        APPEND lwa_irwrk TO ls_fmir_doc-x-irwrk.
      ENDLOOP.
    ENDIF.
    IF lt_tmpirflo IS NOT INITIAL.
      LOOP AT lt_tmpirflo ASSIGNING <lwa_ie03>
                        WHERE contr EQ <lwa_ie01>-contr. "#EC CI_STDSEQ
        MOVE-CORRESPONDING <lwa_ie03> TO lwa_irflo.
        APPEND lwa_irflo TO ls_fmir_doc-x-irflo.
      ENDLOOP.
    ENDIF.
    IF lt_tmpirmea IS NOT INITIAL.
      LOOP AT lt_tmpirmea ASSIGNING <lwa_ie04>
                        WHERE contr EQ <lwa_ie01>-contr. "#EC CI_STDSEQ
        MOVE-CORRESPONDING <lwa_ie04> TO lwa_irmea.
        APPEND lwa_irmea TO ls_fmir_doc-x-irmea.
      ENDLOOP.
    ENDIF.
    IF lt_tmpirhdrt IS NOT INITIAL.
      LOOP AT lt_tmpirhdrt ASSIGNING <lwa_ie05>
                        WHERE contr EQ <lwa_ie01>-contr.  "#EC CI_STDSEQ
        MOVE-CORRESPONDING <lwa_ie05> TO lwa_irhdrt.
        APPEND lwa_irhdrt TO ls_fmir_doc-x-irdes.
      ENDLOOP.
    ENDIF.

*ENHANCEMENT-POINT IRRIGATION_EQUIP_CREATE SPOTS /AGRI/ES_CL_UPLOAD_MASTER_DATA .

    CALL FUNCTION '/AGRI/FMIR_CREATE'
      EXPORTING
        i_messages_display      = ' '
        i_save_messages         = ' '
        i_commit_work           = 'X'
        is_irhdr                = ls_fmir_doc-x-irhdr
        it_irwrk                = ls_fmir_doc-x-irwrk
        it_irflo                = ls_fmir_doc-x-irflo
        it_irmea                = ls_fmir_doc-x-irmea
        it_irdes                = ls_fmir_doc-x-irdes
      IMPORTING
        es_fmir_doc             = ls_fmir_doc
        et_messages             = lt_messages
*       e_log_number            =
      EXCEPTIONS
        no_documents_to_process = 1
        no_authorization        = 2
        creation_failed         = 3
        OTHERS                  = 4.
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
    CLEAR: ls_fmir_doc-x-irhdr, ls_tmpfmir_doc, ls_fmir_doc.

  ENDLOOP.






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

  CONSTANTS: BEGIN OF c_atfor,
               char           TYPE atfor VALUE 'CHAR',
               date           TYPE atfor VALUE 'DATE',
               time           TYPE atfor VALUE 'TIME',
               currency       TYPE atfor VALUE 'CURR',
               numeric        TYPE atfor VALUE 'NUM',
               abstract       TYPE atfor VALUE 'ADT',
               long           TYPE atfor VALUE 'LONG',
               composite_attr TYPE atfor VALUE 'CATR',
               boolean        TYPE atfor VALUE 'BOOL'.
  CONSTANTS: END OF c_atfor.

  REFRESH: et_messages.

  lt_table[] = it_table[].
  DELETE lt_table WHERE row BETWEEN 1 AND 15             "#EC CI_STDSEQ
             OR column EQ 1.

  CALL METHOD standard_structure_build(
    EXPORTING
      i_structure = c_structures-md_01
      i_sheet     = 1
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  LOOP AT lt_table INTO lwa_data WHERE sheet = 1.        "#EC CI_STDSEQ
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
    CALL FUNCTION '/AGRI/G_CONV_EXIT_TPLNR_INPUT'
      EXPORTING
        i_input  = lwa_mdhdr-tplnr_fl
      IMPORTING
        e_output = lwa_mdhdr-tplnr_fl.

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
  CALL METHOD standard_structure_build(
    EXPORTING
      i_structure = c_structures-md_02
      i_sheet     = 2
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  LOOP AT lt_table INTO lwa_data WHERE sheet = 2.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_md02 ASSIGNING <lwa_md02>.
      lv_row = lwa_data-row.
    ENDIF.
    LOOP AT lt_dd03l INTO lwa_dd03l
      WHERE position = lwa_data-column.                  "#EC CI_STDSEQ
      ASSIGN COMPONENT lwa_data-fieldname
        OF STRUCTURE <lwa_md02> TO <lv_value>.
      <lv_value> = lwa_data-value.
    ENDLOOP.
  ENDLOOP.

  IF lt_md02[] IS NOT INITIAL.
    SELECT c~atinn, c~atnam, c~atfor, g~atbez_uc AS atbez
      INTO TABLE @DATA(lt_cabn)
      FROM cabn AS c
      INNER JOIN /agri/gatda AS g
      ON c~atinn = g~atinn
      FOR ALL ENTRIES IN @lt_md02
     WHERE c~atnam = @lt_md02-atnam
       AND g~spras = @sy-langu.
  ENDIF.

  CLEAR lv_row.
*  LOOP AT lt_md02 ASSIGNING <lwa_md02>.
*    MOVE-CORRESPONDING <lwa_md02> TO lwa_mdatv.
*    lwa_mdatv-updkz = c_structures-md_in.
*
*    READ TABLE lt_cabn INTO DATA(ls_cabn)
*      WITH KEY atnam = <lwa_md02>-atnam BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      lwa_mdatv-atinn = ls_cabn-atinn.
*      lwa_mdatv-atbez = ls_cabn-atbez.
*      IF ls_cabn-atfor EQ c_atfor-numeric.
*        MOVE lwa_mdatv-atwrt TO lwa_mdatv-atflv.
*        CLEAR lwa_mdatv-atwrt.
*      ENDIF.
*    ENDIF.
*    APPEND lwa_mdatv TO lt_mdatv.
*    CLEAR lwa_mdatv.
*  ENDLOOP.
  LOOP AT lt_md02 ASSIGNING <lwa_md02>.
    MOVE-CORRESPONDING <lwa_md02> TO lwa_mdatv.
    lwa_mdatv-updkz = c_structures-md_in.
    SELECT SINGLE c~atinn g~atbez_uc INTO (lwa_mdatv-atinn, lwa_mdatv-atbez) "#EC CI_SEL_NESTED
      FROM cabn AS c INNER JOIN /agri/gatda AS g ON c~atinn = g~atinn
       WHERE c~atnam = <lwa_md02>-atnam AND g~spras = sy-langu. "#EC CI_NOORDER
    APPEND lwa_mdatv TO lt_mdatv.
    CLEAR lwa_mdatv.
  ENDLOOP.

  LOOP AT lt_md02 ASSIGNING <lwa_md02>.
    MOVE-CORRESPONDING <lwa_md02> TO lwa_mditm.
    lwa_mditm-updkz = c_structures-md_in.
    SELECT SINGLE c~msehi g~atbez_uc INTO (lwa_mditm-cunit, lwa_mditm-atbez) "#EC CI_SEL_NESTED
      FROM cabn AS c INNER JOIN /agri/gatda AS g ON c~atinn = g~atinn
       WHERE c~atnam = lwa_mditm-atnam AND g~spras = sy-langu. "#EC CI_NOORDER
    APPEND lwa_mditm TO lt_mditm. CLEAR lwa_mditm.
  ENDLOOP.

*{   INSERT         SS8K900620                                        1
****Date check
  LOOP AT lt_mdhdr INTO DATA(ls_mdhdr).
    IF ls_mdhdr-mdate IS NOT INITIAL.
      CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
        EXPORTING
          date                      = ls_mdhdr-mdate
        EXCEPTIONS
          plausibility_check_failed = 1
          OTHERS                    = 2.
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


METHOD PRODORD_CHANGE.

  DATA: lt_table         TYPE /agri/t_excel_sheet,
        lt_dd03l         TYPE thrpad_erd_dd03l,
        lt_messages      TYPE /agri/t_gprolog,
        lt_prodord_sheet TYPE STANDARD TABLE OF zabs_str_prodord_sheet INITIAL SIZE 0,
        lt_bdcdata       TYPE bdcdata_tab,
        lt_bdc_messages  TYPE ettcd_msg_tabtype,
        lwa_bdc_options  TYPE ctu_params,
        lwa_data         TYPE /agri/s_excel_sheet,
        lwa_dd03l        TYPE dd03l,
        lwa_message      TYPE /agri/s_gprolog,
        lv_msgv1         TYPE sy-msgv1,
        lv_msgli         TYPE sy-msgli,
        lv_subrc         TYPE sysubrc,
        lv_aufnr         TYPE aufnr,
        lv_posnr         TYPE i,
        lv_minutes       TYPE i,
        lv_h             TYPE n LENGTH 2,
        lv_m             TYPE n LENGTH 2,
        lv_s             TYPE n LENGTH 2,
        lv_time          TYPE sy-uzeit,
        lv_time_char     TYPE char0256,
        lv_num           TYPE float,
        lv_empge_bdc     TYPE bdc_fval,
        lv_aufnr_bdc     TYPE bdc_fval,
        lv_co02          TYPE sy-tcode VALUE 'CO02',
        lv_row           TYPE /agri/s_excel_sheet-row.

  CONSTANTS: BEGIN OF c_structures,
               prodord_01 TYPE tabname  VALUE 'ZABS_STR_PRODORD_SHEET',
               prodord_in TYPE updkz_d  VALUE 'I',
               prodord_up TYPE updkz_d  VALUE 'U',
               success    TYPE sy-msgty VALUE 'S',
             END OF c_structures.

****Message Types
  CONSTANTS : BEGIN OF c_msg_type,
                info    LIKE sy-msgty VALUE 'I',
                warning LIKE sy-msgty VALUE 'W',
                error   LIKE sy-msgty VALUE 'E',
                abend   LIKE sy-msgty VALUE 'A',
                success LIKE sy-msgty VALUE 'S',
                x       LIKE sy-msgty VALUE 'X',
              END   OF c_msg_type.

  FIELD-SYMBOLS: <lwa_prodord_sheet> TYPE zabs_str_prodord_sheet,
                 <lv_value>          TYPE any.

  DEFINE add_first_line.
*-- Verificar Linha &2.
    CLEAR lwa_message.
    lwa_message-msgid = 'ZFMFP'.
    lwa_message-msgno = '163'.
    lwa_message-msgty = 'I'.
    lwa_message-msgv1 = &1.
    APPEND lwa_message TO lt_messages.
  END-OF-DEFINITION.

  lt_table[] = it_table[].

  DELETE lt_table WHERE row = 1.
  DELETE lt_table WHERE sheet <> 1.

  "Sheet 1
  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-prodord_01
      i_sheet     = 1
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  SORT lt_dd03l BY position.

  LOOP AT lt_table INTO lwa_data WHERE sheet = 01.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_prodord_sheet ASSIGNING <lwa_prodord_sheet>.
      lv_row = lwa_data-row.
    ENDIF.

    READ TABLE lt_dd03l TRANSPORTING NO FIELDS
      WITH KEY position = lwa_data-column.
    IF sy-subrc EQ 0.
      LOOP AT lt_dd03l INTO lwa_dd03l FROM sy-tabix.
        IF lwa_dd03l-position NE lwa_data-column.
          EXIT.
        ENDIF.
        ASSIGN COMPONENT lwa_data-fieldname
          OF STRUCTURE <lwa_prodord_sheet> TO <lv_value>.
        IF sy-subrc EQ 0.
          IF lwa_dd03l-domname NE 'MENGV13'.
            <lv_value> = lwa_data-value.
          ENDIF.

          IF lwa_dd03l-domname EQ 'DATUM'
          OR lwa_dd03l-domname EQ 'DATS'.
            <lv_value> = |{ lwa_data-value+6(4) }| &&
                         |{ lwa_data-value+3(2) }| &&
                         |{ lwa_data-value(2) }|.
          ELSEIF lwa_dd03l-domname EQ 'MENGV13'.
            TRANSLATE lwa_data-value USING ',.'.
            <lv_value> = lwa_data-value.
          ELSEIF lwa_dd03l-domname EQ 'TIMS'.
            CLEAR lv_time.
            lv_time_char = lwa_data-value.
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
          ELSEIF lwa_dd03l-domname EQ '/AGRI/GLTPLNR_FL'.
            CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
              EXPORTING
                input      = <lv_value>
              IMPORTING
                output     = <lv_value>
              EXCEPTIONS
                not_found  = 1
                not_active = 2
                OTHERS     = 3.
          ELSEIF lwa_dd03l-domname EQ 'AUFNR'.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = <lv_value>
              IMPORTING
                output = <lv_value>.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    <lwa_prodord_sheet>-excel_row = lwa_data-row.
    <lwa_prodord_sheet>-excel_column = lwa_data-column.
  ENDLOOP.
  CLEAR lv_row.

  IF lt_prodord_sheet[] IS INITIAL.
*-- Arquivo Excel não contém dados.
    CLEAR lwa_message.
    lwa_message-msgid = 'ZFMFP'.
    lwa_message-msgno = '122'.
    lwa_message-msgty = 'E'.
    APPEND lwa_message TO lt_messages.
    RETURN.
  ELSE.
    SELECT k~aufnr, k~auart, k~autyp, k~bukrs,
           k~werks, k~kokrs, k~waers, k~loekz,
           o~gasmg, o~gamng, o~gmein, o~plnbez,
           o~plnty, o~plnnr, o~plnal, o~stlty,
           o~stlbez, o~stlst, o~stlnr
      FROM aufk AS k
      INNER JOIN afko AS o
      ON k~aufnr = o~aufnr
      INTO TABLE @DATA(lt_aufk)
      FOR ALL ENTRIES IN @lt_prodord_sheet
     WHERE k~aufnr = @lt_prodord_sheet-aufnr.

    SORT lt_aufk BY aufnr.
  ENDIF.

  LOOP AT lt_prodord_sheet ASSIGNING <lwa_prodord_sheet>.
    READ TABLE lt_aufk INTO DATA(lwa_aufk)
      WITH KEY aufnr = <lwa_prodord_sheet>-aufnr BINARY SEARCH.
    IF sy-subrc NE 0.
      IF <lwa_prodord_sheet>-line_w_error EQ abap_false.
        <lwa_prodord_sheet>-line_w_error = abap_true.
        add_first_line <lwa_prodord_sheet>-excel_row.
      ENDIF.
      lv_aufnr = <lwa_prodord_sheet>-aufnr.
      SHIFT lv_aufnr LEFT DELETING LEADING '0'.
*-- Ordem &1 inexistente.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZFMFP'.
      lwa_message-msgno = 156.
      lwa_message-msgty = 'E'.
      lwa_message-msgv1 = lv_aufnr.
      APPEND lwa_message TO lt_messages.
    ELSE.
      IF lwa_aufk-loekz EQ abap_true.
        IF <lwa_prodord_sheet>-line_w_error EQ abap_false.
          <lwa_prodord_sheet>-line_w_error = abap_true.
          add_first_line <lwa_prodord_sheet>-excel_row.
        ENDIF.
        lv_aufnr = <lwa_prodord_sheet>-aufnr.
        SHIFT lv_aufnr LEFT DELETING LEADING '0'.
*-- Ordem &1 marcada para eliminação.
        CLEAR lwa_message.
        lwa_message-msgid = 'ZFMFP'.
        lwa_message-msgno = 157.
        lwa_message-msgty = 'E'.
        lwa_message-msgv1 = lv_aufnr.
        APPEND lwa_message TO lt_messages.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF lt_prodord_sheet[] IS INITIAL.
*-- Não existem informações válidas no arquivo selecionado.
    CLEAR lwa_message.
    lwa_message-msgid = 'ZFMFP'.
    lwa_message-msgno = 162.
    lwa_message-msgty = 'E'.
    APPEND lwa_message TO lt_messages.
  ELSE.
    lwa_bdc_options-dismode  = 'N'.
    lwa_bdc_options-nobiend  = abap_false. "mc_true.
    lwa_bdc_options-updmode  = 'S'.
    lwa_bdc_options-nobinpt  = abap_false. "mc_true.

    LOOP AT lt_prodord_sheet INTO DATA(lwa_prodord_sheet).
      CLEAR: mt_bdcdata[], ms_bdcdata, lt_bdc_messages[].
      READ TABLE lt_aufk INTO lwa_aufk
        WITH KEY aufnr = lwa_prodord_sheet-aufnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        lv_aufnr_bdc = lwa_aufk-aufnr.
        lv_empge_bdc = lwa_prodord_sheet-empge.

        dynpro_fill( i_program = 'SAPLCOKO1'      i_dynpro  = '0110' ).
        field_fill( i_fnam = 'BDC_OKCODE'         i_fval = '=ENTK' ).
        field_fill( i_fnam = 'CAUFVD-AUFNR'       i_fval = lv_aufnr_bdc ).
        field_fill( i_fnam = 'R62CLORD-FLG_OVIEW' i_fval = 'X' ).

        dynpro_fill( i_program = 'SAPLCOKO1'      i_dynpro  = '0115' ).
        field_fill( i_fnam = 'BDC_OKCODE'         i_fval = '=KOBK' ).

        dynpro_fill( i_program = 'SAPLKOBS'       i_dynpro  = '0130' ).
        field_fill( i_fnam = 'BDC_OKCODE'         i_fval = '/00' ).
        field_fill( i_fnam = 'DKOBR-EMPGE(01)'    i_fval = lv_empge_bdc ).

        dynpro_fill( i_program = 'SAPLKOBS'       i_dynpro  = '0130' ).
        field_fill( i_fnam = 'BDC_OKCODE'         i_fval = '=BACK' ).

        dynpro_fill( i_program = 'SAPLCOKO1'      i_dynpro  = '0115' ).
        field_fill( i_fnam = 'BDC_OKCODE'         i_fval = '=BU' ).

        REFRESH: lt_bdc_messages[].
        CALL METHOD /agri/cl_global_services=>transaction_call_process
          EXPORTING
            i_tcode                = lv_co02
            i_use_bdc_data         = mc_true
            is_bdc_options         = lwa_bdc_options
            it_bdcdata             = mt_bdcdata[]
          IMPORTING
            et_bdc_messages        = lt_bdc_messages
          EXCEPTIONS
            invalid_parameters     = 1
            call_transaction_error = 2
            OTHERS                 = 3.

        LOOP AT lt_bdc_messages INTO DATA(lwa_bdc_msg).
          CLEAR lwa_message.
          lwa_message-msgty   = lwa_bdc_msg-msgtyp.
          lwa_message-msgid   = lwa_bdc_msg-msgid.
          lwa_message-msgno   = lwa_bdc_msg-msgnr.
          lwa_message-msgv1   = lwa_bdc_msg-msgv1.
          lwa_message-msgv2   = lwa_bdc_msg-msgv2.
          lwa_message-msgv3   = lwa_bdc_msg-msgv3.
          lwa_message-msgv4   = lwa_bdc_msg-msgv4.
          APPEND lwa_message TO lt_messages.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDIF.

  et_messages[] = lt_messages[].

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


METHOD RECIPES_CREATE.

  DATA: lt_table        TYPE /agri/t_excel_sheet,
        lt_dd03l        TYPE thrpad_erd_dd03l,
        lt_messages     TYPE /agri/t_gprolog,
        lt_rchdr        TYPE STANDARD TABLE OF zfmrchdr INITIAL SIZE 0,
        lt_rclst        TYPE STANDARD TABLE OF zfmrclst INITIAL SIZE 0,
        lt_similares    TYPE STANDARD TABLE OF zfmrcsim INITIAL SIZE 0,
        lt_mess_collect TYPE /agri/t_gprolog,
        lt_mess_change  LIKE lt_mess_collect,
        lt_recipes      TYPE TABLE OF zabs_str_recipe1 INITIAL SIZE 0,
        lwa_data        TYPE /agri/s_excel_sheet,
        lwa_dd03l       TYPE dd03l,
        lwa_message     TYPE /agri/s_gprolog,
        lwa_rchdr       TYPE zfmrchdr,
        lwa_rclst       TYPE zfmrclst,
        lwa_similar     TYPE zfmrcsim,
        lv_data_changed TYPE abap_bool,
        lv_rcnum        TYPE zfmrcnum,
        lv_i            TYPE i,
        lv_posnr        TYPE zfmposnr,
        lv_row          TYPE /agri/s_excel_sheet-row.

  FIELD-SYMBOLS: <lwa_recipe> TYPE zabs_str_recipe1,
                 <lv_value>   TYPE any.

  CONSTANTS: BEGIN OF c_structures,
               rc_01   TYPE tabname  VALUE 'ZABS_STR_RECIPE1',
               rc_02   TYPE tabname  VALUE '/AGRI/S_AT_SHEET2',
               rc_03   TYPE tabname  VALUE '/AGRI/S_AT_SHEET3',
               rc_04   TYPE tabname  VALUE '/AGRI/S_AT_SHEET4',
               rc_05   TYPE tabname  VALUE '/AGRI/S_AT_SHEET5',
               at_in   TYPE updkz_d  VALUE 'I',
               at_up   TYPE updkz_d  VALUE 'U',
               success TYPE sy-msgty VALUE 'S',
             END OF c_structures.

  CONSTANTS: BEGIN OF c_object,
               bor              TYPE /agri/tgabo-object VALUE 'ZFMRC',
               log              TYPE balobj-object      VALUE 'ZFMRC',
               change_documents TYPE cdobjectcl         VALUE 'ZFMRC',
               esh_object       TYPE /agri/geshobjtp    VALUE 'ZFMRC',
               text_object      TYPE thead-tdobject     VALUE 'ZFMRC',
             END OF c_object.

  CONSTANTS: BEGIN OF c_number_range,
               rc TYPE inri-object VALUE 'ZRNARC',
             END OF c_number_range.

  DEFINE add_first_line.
*-- Verificar Coluna &1 Linha &2.
    CLEAR lwa_message.
    lwa_message-msgid = 'ZABS_MSGCLS'.
    lwa_message-msgno = '143'.
    lwa_message-msgty = 'I'.
    lwa_message-msgv1 = &1.
    lwa_message-msgv2 = &2.
    APPEND lwa_message TO lt_messages.
  END-OF-DEFINITION.

  lt_table[] = it_table[].

  DELETE lt_table WHERE row = 1.
  DELETE lt_table WHERE sheet <> 1.

  "Sheet 1
  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-rc_01
      i_sheet     = 1
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  SORT lt_dd03l BY position.

  LOOP AT lt_table INTO lwa_data WHERE sheet = 01.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_recipes ASSIGNING <lwa_recipe>.
      lv_row = lwa_data-row.
    ENDIF.

    READ TABLE lt_dd03l TRANSPORTING NO FIELDS
      WITH KEY position = lwa_data-column.
    IF sy-subrc EQ 0.
      LOOP AT lt_dd03l INTO lwa_dd03l FROM sy-tabix.
        IF lwa_dd03l-position NE lwa_data-column.
          EXIT.
        ENDIF.
        ASSIGN COMPONENT lwa_data-fieldname
          OF STRUCTURE <lwa_recipe> TO <lv_value>.
        IF sy-subrc EQ 0.
          IF lwa_dd03l-datatype EQ 'DATS'.
            <lv_value> = lwa_data-value+6(4) && lwa_data-value+3(2) && lwa_data-value(2).
          ELSEIF lwa_dd03l-domname EQ 'MEINS'.
            CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
              EXPORTING
                input          = lwa_data-value
                language       = sy-langu
              IMPORTING
                output         = <lv_value>
              EXCEPTIONS
                unit_not_found = 1
                OTHERS         = 2.
          ELSEIF lwa_dd03l-domname EQ 'MATNR'.
            CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
              EXPORTING
                input        = lwa_data-value
              IMPORTING
                output       = <lv_value>
              EXCEPTIONS
                length_error = 1
                OTHERS       = 2.
          ELSE.
            <lv_value> = lwa_data-value.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    <lwa_recipe>-excel_row = lwa_data-row.
    <lwa_recipe>-excel_column = lwa_data-column.
  ENDLOOP.
  CLEAR lv_row.

  IF lt_recipes[] IS INITIAL.
*-- Arquivo Excel não contém dados.
    CLEAR lwa_message.
    lwa_message-msgid = 'ZFMFP'.
    lwa_message-msgno = '122'.
    lwa_message-msgty = 'E'.
    APPEND lwa_message TO lt_messages.
    RETURN.
  ENDIF.

  SELECT *
    FROM ztfmrctyp
    INTO TABLE @DATA(lt_fmrctyp)
    FOR ALL ENTRIES IN @lt_recipes
   WHERE rctyp = @lt_recipes-rctyp.

  SORT lt_fmrctyp BY rctyp.

  SELECT werks, name1
    FROM t001w
    INTO TABLE @DATA(lt_t001w)
    FOR ALL ENTRIES IN @lt_recipes
   WHERE werks = @lt_recipes-werks.

  SORT lt_t001w BY werks.

  SELECT m~matnr, m~mtart, m~meins,
         t~maktx, c~werks, c~ausme
    FROM mara AS m
    INNER JOIN makt AS t
    ON m~matnr EQ t~matnr
    INNER JOIN marc AS c
    ON m~matnr EQ c~matnr
    INTO TABLE @DATA(lt_mara)
    FOR ALL ENTRIES IN @lt_recipes
   WHERE m~matnr = @lt_recipes-matnr
     AND c~werks = @lt_recipes-werks
     AND t~spras = @sy-langu.

  SELECT m~matnr, m~mtart, m~meins,
         t~maktx, c~werks, c~ausme
    FROM mara AS m
    INNER JOIN makt AS t
    ON m~matnr EQ t~matnr
    INNER JOIN marc AS c
    ON m~matnr EQ c~matnr
    APPENDING TABLE @lt_mara
    FOR ALL ENTRIES IN @lt_recipes
   WHERE m~matnr = @lt_recipes-insumo
     AND c~werks = @lt_recipes-werks
     AND t~spras = @sy-langu.

  SELECT m~matnr, m~mtart, m~meins,
         t~maktx, c~werks, c~ausme
    FROM mara AS m
    INNER JOIN makt AS t
    ON m~matnr EQ t~matnr
    INNER JOIN marc AS c
    ON m~matnr EQ c~matnr
    APPENDING TABLE @lt_mara
    FOR ALL ENTRIES IN @lt_recipes
   WHERE m~matnr = @lt_recipes-similar
     AND c~werks = @lt_recipes-werks
     AND t~spras = @sy-langu.

  SORT lt_mara BY matnr werks mtart.

  SELECT msehi
    FROM t006
    INTO TABLE @DATA(lt_t006)
    FOR ALL ENTRIES IN @lt_recipes
   WHERE msehi = @lt_recipes-meins_sim.

  SORT lt_t006 BY msehi.

  LOOP AT lt_recipes ASSIGNING <lwa_recipe>.
    READ TABLE lt_fmrctyp INTO DATA(lwa_fmrctyp)
      WITH KEY rctyp = <lwa_recipe>-rctyp BINARY SEARCH.
    IF sy-subrc NE 0.
      <lwa_recipe>-line_w_error = abap_true.
*-- Verificar Coluna &1 Linha &2.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZABS_MSGCLS'.
      lwa_message-msgno = '143'.
      lwa_message-msgty = 'I'.
      lwa_message-msgv1 = <lwa_recipe>-excel_column.
      lwa_message-msgv2 = <lwa_recipe>-excel_row.
      APPEND lwa_message TO lt_messages.
*-- Tipo de Receita &1 inexistente.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZFMFP'.
      lwa_message-msgno = 117.
      lwa_message-msgty = 'E'.
      lwa_message-msgv1 = <lwa_recipe>-rctyp.
      APPEND lwa_message TO lt_messages.
    ENDIF.

    READ TABLE lt_t001w TRANSPORTING NO FIELDS
      WITH KEY werks = <lwa_recipe>-werks BINARY SEARCH.
    IF sy-subrc NE 0.
      IF <lwa_recipe>-line_w_error EQ abap_false.
        <lwa_recipe>-line_w_error = abap_true.
*-- Verificar Coluna &1 Linha &2.
        CLEAR lwa_message.
        lwa_message-msgid = 'ZABS_MSGCLS'.
        lwa_message-msgno = '143'.
        lwa_message-msgty = 'I'.
        lwa_message-msgv1 = <lwa_recipe>-excel_column.
        lwa_message-msgv2 = <lwa_recipe>-excel_row.
        APPEND lwa_message TO lt_messages.
      ENDIF.
*-- Centro &1 inválido.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZFMFP'.
      lwa_message-msgno = 118.
      lwa_message-msgty = 'E'.
      lwa_message-msgv1 = <lwa_recipe>-werks.
      APPEND lwa_message TO lt_messages.
    ENDIF.

    DATA(lv_valid_date) = abap_true.
    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                      = <lwa_recipe>-datuv
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS                    = 2.

    IF sy-subrc NE 0.
      lv_valid_date = abap_false.
      IF <lwa_recipe>-line_w_error EQ abap_false.
        <lwa_recipe>-line_w_error = abap_true.
        add_first_line <lwa_recipe>-excel_column <lwa_recipe>-excel_row.
      ENDIF.
*-- Data Inicial de validade inválida.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZFMFP'.
      lwa_message-msgno = 119.
      lwa_message-msgty = 'E'.
      APPEND lwa_message TO lt_messages.
    ENDIF.

    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                      = <lwa_recipe>-datbi
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS                    = 2.

    IF sy-subrc NE 0.
      lv_valid_date = abap_false.
      IF <lwa_recipe>-line_w_error EQ abap_false.
        <lwa_recipe>-line_w_error = abap_true.
        add_first_line <lwa_recipe>-excel_column <lwa_recipe>-excel_row.
      ENDIF.
*-- Data Final de validade inválida.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZFMFP'.
      lwa_message-msgno = 120.
      lwa_message-msgty = 'E'.
      APPEND lwa_message TO lt_messages.
    ENDIF.

    IF lv_valid_date = abap_true.
      IF <lwa_recipe>-datuv GT <lwa_recipe>-datbi.
        IF <lwa_recipe>-line_w_error EQ abap_false.
          <lwa_recipe>-line_w_error = abap_true.
          add_first_line <lwa_recipe>-excel_column <lwa_recipe>-excel_row.
        ENDIF.
*-- Data Final anterior à Data Inicial!
        CLEAR lwa_message.
        lwa_message-msgid = 'ZFMFP'.
        lwa_message-msgno = 121.
        lwa_message-msgty = 'E'.
        APPEND lwa_message TO lt_messages.
      ENDIF.
    ENDIF.

    READ TABLE lt_mara TRANSPORTING NO FIELDS
      WITH KEY matnr = <lwa_recipe>-matnr BINARY SEARCH.
    IF sy-subrc NE 0.
      IF <lwa_recipe>-line_w_error EQ abap_false.
        <lwa_recipe>-line_w_error = abap_true.
        add_first_line <lwa_recipe>-excel_column <lwa_recipe>-excel_row.
      ENDIF.
*-- Tarefa &1 inexistente.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZFMFP'.
      lwa_message-msgno = 123.
      lwa_message-msgty = 'E'.
      lwa_message-msgv1 = <lwa_recipe>-matnr.
      APPEND lwa_message TO lt_messages.
    ENDIF.

    READ TABLE lt_mara TRANSPORTING NO FIELDS
      WITH KEY matnr = <lwa_recipe>-insumo BINARY SEARCH.
    IF sy-subrc NE 0.
      IF <lwa_recipe>-line_w_error EQ abap_false.
        <lwa_recipe>-line_w_error = abap_true.
        add_first_line <lwa_recipe>-excel_column <lwa_recipe>-excel_row.
      ENDIF.
*-- Insumo &1 inexistente.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZFMFP'.
      lwa_message-msgno = 126.
      lwa_message-msgty = 'E'.
      lwa_message-msgv1 = <lwa_recipe>-insumo.
      APPEND lwa_message TO lt_messages.
    ENDIF.

    IF <lwa_recipe>-insumo IS INITIAL.
      IF <lwa_recipe>-line_w_error EQ abap_false.
        <lwa_recipe>-line_w_error = abap_true.
        add_first_line <lwa_recipe>-excel_column <lwa_recipe>-excel_row.
      ENDIF.
*-- O Isumo é de preenchimento obrigatório.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZFMFP'.
      lwa_message-msgno = 128.
      lwa_message-msgty = 'E'.
      APPEND lwa_message TO lt_messages.
    ENDIF.

    IF <lwa_recipe>-rcdos IS INITIAL.
      IF <lwa_recipe>-line_w_error EQ abap_false.
        <lwa_recipe>-line_w_error = abap_true.
        add_first_line <lwa_recipe>-excel_column <lwa_recipe>-excel_row.
      ENDIF.
*-- A dose do Insumo deve ser informada.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZFMFP'.
      lwa_message-msgno = 129.
      lwa_message-msgty = 'E'.
      APPEND lwa_message TO lt_messages.
    ENDIF.

    READ TABLE lt_mara INTO DATA(lwa_mara)
      WITH KEY matnr = <lwa_recipe>-matnr
               werks = <lwa_recipe>-werks BINARY SEARCH.
    IF sy-subrc NE 0.
      IF <lwa_recipe>-line_w_error EQ abap_false.
        <lwa_recipe>-line_w_error = abap_true.
        add_first_line <lwa_recipe>-excel_column <lwa_recipe>-excel_row.
      ENDIF.
*-- Tarefa &1 inexistente no Centro &2.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZFMFP'.
      lwa_message-msgno = 124.
      lwa_message-msgty = 'E'.
      lwa_message-msgv1 = <lwa_recipe>-matnr.
      lwa_message-msgv2 = <lwa_recipe>-werks.
      APPEND lwa_message TO lt_messages.
    ELSE.
      IF lwa_mara-ausme IS INITIAL.
        IF <lwa_recipe>-line_w_error EQ abap_false.
          <lwa_recipe>-line_w_error = abap_true.
          add_first_line <lwa_recipe>-excel_column <lwa_recipe>-excel_row.
        ENDIF.
*-- Tarefa &1 Centro &2 não possui base de cálculo (MARC-AUSME)
        CLEAR lwa_message.
        lwa_message-msgid = 'ZFMFP'.
        lwa_message-msgno = 125.
        lwa_message-msgty = 'E'.
        lwa_message-msgv1 = <lwa_recipe>-matnr.
        lwa_message-msgv2 = <lwa_recipe>-werks.
        APPEND lwa_message TO lt_messages.
      ENDIF.
    ENDIF.

    READ TABLE lt_mara INTO lwa_mara
      WITH KEY matnr = <lwa_recipe>-insumo
               werks = <lwa_recipe>-werks BINARY SEARCH.
    IF sy-subrc NE 0.
      IF <lwa_recipe>-line_w_error EQ abap_false.
        <lwa_recipe>-line_w_error = abap_true.
        add_first_line <lwa_recipe>-excel_column <lwa_recipe>-excel_row.
      ENDIF.
*-- Insumo &1 inexistente no Centro &2.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZFMFP'.
      lwa_message-msgno = 137.
      lwa_message-msgty = 'E'.
      lwa_message-msgv1 = <lwa_recipe>-insumo.
      lwa_message-msgv2 = <lwa_recipe>-werks.
      APPEND lwa_message TO lt_messages.
    ELSE.
      READ TABLE lt_fmrctyp INTO lwa_fmrctyp
        WITH KEY rctyp = <lwa_recipe>-rctyp BINARY SEARCH.
      IF lwa_mara-mtart NE lwa_fmrctyp-mtart.
        IF <lwa_recipe>-line_w_error EQ abap_false.
          <lwa_recipe>-line_w_error = abap_true.
          add_first_line <lwa_recipe>-excel_column <lwa_recipe>-excel_row.
        ENDIF.
*-- MARA-MTART do Insumo &1 é diferente do param. no Tipo de Receita &2!
        CLEAR lwa_message.
        lwa_message-msgid = 'ZFMFP'.
        lwa_message-msgno = 139.
        lwa_message-msgty = 'E'.
        lwa_message-msgv1 = lwa_mara-mtart.
        lwa_message-msgv2 = lwa_fmrctyp-mtart.
        APPEND lwa_message TO lt_messages.
      ENDIF.
    ENDIF.

    IF <lwa_recipe>-similar IS NOT INITIAL.
      READ TABLE lt_mara TRANSPORTING NO FIELDS
        WITH KEY matnr = <lwa_recipe>-similar BINARY SEARCH.
      IF sy-subrc NE 0.
        IF <lwa_recipe>-line_w_error EQ abap_false.
          <lwa_recipe>-line_w_error = abap_true.
          add_first_line <lwa_recipe>-excel_column <lwa_recipe>-excel_row.
        ENDIF.
*-- Produto Similar &1 inexistente.
        CLEAR lwa_message.
        lwa_message-msgid = 'ZFMFP'.
        lwa_message-msgno = 127.
        lwa_message-msgty = 'E'.
        lwa_message-msgv1 = <lwa_recipe>-similar.
        APPEND lwa_message TO lt_messages.
      ENDIF.

      IF <lwa_recipe>-rcdos_sim IS INITIAL.
        IF <lwa_recipe>-line_w_error EQ abap_false.
          <lwa_recipe>-line_w_error = abap_true.
          add_first_line <lwa_recipe>-excel_column <lwa_recipe>-excel_row.
        ENDIF.
*-- A dose do Produto Similar deve ser informada.
        CLEAR lwa_message.
        lwa_message-msgid = 'ZFMFP'.
        lwa_message-msgno = 130.
        lwa_message-msgty = 'E'.
        APPEND lwa_message TO lt_messages.
      ENDIF.

      IF <lwa_recipe>-meins_sim IS INITIAL.
        IF <lwa_recipe>-line_w_error EQ abap_false.
          <lwa_recipe>-line_w_error = abap_true.
          add_first_line <lwa_recipe>-excel_column <lwa_recipe>-excel_row.
        ENDIF.
*-- A Unid. de Medida do Produto Similar deve ser informada.
        CLEAR lwa_message.
        lwa_message-msgid = 'ZFMFP'.
        lwa_message-msgno = 131.
        lwa_message-msgty = 'E'.
        APPEND lwa_message TO lt_messages.
      ENDIF.

      READ TABLE lt_mara INTO lwa_mara
        WITH KEY matnr = <lwa_recipe>-similar
                 werks = <lwa_recipe>-werks BINARY SEARCH.
      IF sy-subrc NE 0.
        IF <lwa_recipe>-line_w_error EQ abap_false.
          <lwa_recipe>-line_w_error = abap_true.
          add_first_line <lwa_recipe>-excel_column <lwa_recipe>-excel_row.
        ENDIF.
*-- Produto Similar &1 inexistente no Centro &2.
        CLEAR lwa_message.
        lwa_message-msgid = 'ZFMFP'.
        lwa_message-msgno = 138.
        lwa_message-msgty = 'E'.
        lwa_message-msgv1 = <lwa_recipe>-similar.
        lwa_message-msgv2 = <lwa_recipe>-werks.
        APPEND lwa_message TO lt_messages.
      ENDIF.

      READ TABLE lt_t006 TRANSPORTING NO FIELDS
        WITH KEY msehi = <lwa_recipe>-meins_sim BINARY SEARCH.
      IF sy-subrc NE 0.
        IF <lwa_recipe>-line_w_error EQ abap_false.
          <lwa_recipe>-line_w_error = abap_true.
          add_first_line <lwa_recipe>-excel_column <lwa_recipe>-excel_row.
        ENDIF.
*-- UM do Produto Similar inválida!
        CLEAR lwa_message.
        lwa_message-msgid = 'ZFMFP'.
        lwa_message-msgno = 132.
        lwa_message-msgty = 'E'.
        APPEND lwa_message TO lt_messages.
      ENDIF.
    ENDIF.
  ENDLOOP.

  SORT lt_recipes BY rcnum insumo.
  DELETE ADJACENT DUPLICATES FROM lt_recipes COMPARING rcnum insumo.
  SORT lt_recipes BY rcnum rcinp.
  DATA(lt_new_recipes) = lt_recipes[].
  DELETE ADJACENT DUPLICATES FROM lt_new_recipes COMPARING rcnum.

  LOOP AT lt_new_recipes INTO DATA(lwa_recipe_header).
    READ TABLE lt_recipes TRANSPORTING NO FIELDS
      WITH KEY line_w_error = abap_true.
    IF sy-subrc EQ 0.
      CONTINUE.
    ENDIF.
    READ TABLE lt_recipes ASSIGNING <lwa_recipe>
      WITH KEY rcnum = lwa_recipe_header-rcnum
               rcinp = abap_true BINARY SEARCH.
    IF sy-subrc NE 0.
      <lwa_recipe>-line_w_error = abap_true.
*-- A Receita &1 não possui Insumo Principal!
      CLEAR lwa_message.
      lwa_message-msgid = 'ZFMFP'.
      lwa_message-msgno = 136.
      lwa_message-msgty = 'E'.
      APPEND lwa_message TO lt_messages.
    ELSE.
      CLEAR lwa_rchdr.
      MOVE-CORRESPONDING lwa_recipe_header TO lwa_rchdr.
      READ TABLE lt_mara INTO lwa_mara
        WITH KEY matnr = lwa_recipe_header-matnr
                 werks = lwa_recipe_header-werks BINARY SEARCH.
      IF sy-subrc EQ 0.
        lwa_rchdr-ausme = lwa_mara-ausme.
        lwa_rchdr-maktx = lwa_mara-maktx.
      ENDIF.

      lwa_rchdr-ernam = sy-uname.
      lwa_rchdr-erdat = sy-datum.
      lwa_rchdr-erzet = sy-uzeit.

      READ TABLE lt_fmrctyp INTO lwa_fmrctyp
        WITH KEY rctyp = lwa_rchdr-rctyp BINARY SEARCH.
      IF sy-subrc EQ 0.
        lwa_rchdr-txtgr = lwa_fmrctyp-txtgr.

        CLEAR lv_rcnum.
        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr             = lwa_fmrctyp-numki
            object                  = c_number_range-rc
          IMPORTING
            number                  = lv_rcnum
          EXCEPTIONS
            interval_not_found      = 01
            number_range_not_intern = 02
            object_not_found        = 03.
        IF sy-subrc EQ 0.
*-- Cabeçalho Receita
          lwa_rchdr-rcnum = lv_rcnum.
          APPEND lwa_rchdr TO lt_rchdr.
*-- Receita &1 criada com sucesso!
          CLEAR lwa_message.
          lwa_message-msgid = 'ZFMFP'.
          lwa_message-msgno = 140.
          lwa_message-msgty = 'S'.
          lwa_message-msgv1 = lv_rcnum.
          APPEND lwa_message TO lt_messages.

          CLEAR: lv_posnr, lv_i.
          READ TABLE lt_recipes TRANSPORTING NO FIELDS
            WITH KEY rcnum = lwa_recipe_header-rcnum BINARY SEARCH.
          IF sy-subrc EQ 0.
*-- Insumos da Receita
            LOOP AT lt_recipes INTO DATA(lwa_recipe_item) FROM sy-tabix.
              IF lwa_recipe_item-rcnum NE lwa_recipe_header-rcnum.
                EXIT.
              ENDIF.
              READ TABLE lt_mara INTO lwa_mara
                WITH KEY matnr = lwa_recipe_item-insumo
                         werks = lwa_recipe_item-werks BINARY SEARCH.
              IF sy-subrc EQ 0.
                CLEAR lwa_rclst.
                ADD 1 TO lv_i.
                lv_posnr = lv_i * 10.
                CLEAR lwa_rclst.
                lwa_rclst-rcnum       = lv_rcnum.
                lwa_rclst-werks       = lwa_recipe_header-werks.
                lwa_rclst-matnr       = lwa_recipe_header-matnr.
                lwa_rclst-posnr       = lv_posnr.
                lwa_rclst-matnr_ins   = lwa_recipe_item-insumo.
                lwa_rclst-maktx       = lwa_mara-maktx.
                lwa_rclst-rcdos       = lwa_recipe_item-rcdos.
                lwa_rclst-units       = lwa_mara-meins.
                IF lwa_recipe_item-rcinp IS NOT INITIAL.
                  lwa_rclst-rcinp_check = abap_true.
                ELSE.
                  lwa_rclst-rcinp_check = abap_false.
                ENDIF.
                IF lwa_rclst-rcinp_check EQ abap_true.
                  lwa_rclst-rcinp = icon_wd_radio_button.
                ELSE.
                  lwa_rclst-rcinp = icon_wd_radio_button_empty.
                ENDIF.
                lwa_rclst-rcdta       = lwa_recipe_item-rcdta.
                APPEND lwa_rclst TO lt_rclst.

                IF lwa_recipe_item-similar IS NOT INITIAL.
                  CLEAR lwa_similar.
                  lwa_similar-rcnum     = lv_rcnum.
                  lwa_similar-matnr_ins = lwa_recipe_item-insumo.
                  lwa_similar-matnr_sim = lwa_recipe_item-similar.
                  lwa_similar-werks     = lwa_recipe_header-werks.
                  lwa_similar-matnr     = lwa_recipe_header-matnr.

                  READ TABLE lt_mara INTO DATA(lwa_mara_ins)
                    WITH KEY matnr = lwa_similar-matnr_ins
                             werks = lwa_similar-werks BINARY SEARCH.
                  IF sy-subrc EQ 0.
                    lwa_similar-maktx_ins = lwa_mara_ins-maktx.
                  ENDIF.

                  READ TABLE lt_mara INTO DATA(lwa_mara_sim)
                    WITH KEY matnr = lwa_similar-matnr_sim
                             werks = lwa_similar-werks BINARY SEARCH.
                  IF sy-subrc EQ 0.
                    lwa_similar-maktx_sim = lwa_mara_sim-maktx.
                  ENDIF.

                  lwa_similar-rcdos_sim = lwa_recipe_item-rcdos_sim.
                  IF lwa_recipe_item-meins_sim IS NOT INITIAL.
                    lwa_similar-units_sim = lwa_recipe_item-meins_sim.
                  ELSE.
                    lwa_similar-units_sim = lwa_mara_sim-meins.
                  ENDIF.
                  APPEND lwa_similar TO lt_similares.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  SORT lt_rchdr BY rcnum werks matnr datuv.
  DELETE ADJACENT DUPLICATES FROM lt_rchdr
  COMPARING rcnum werks matnr datuv.
  IF lt_rchdr[] IS NOT INITIAL.
    MODIFY zfmrchdr FROM TABLE lt_rchdr.
  ENDIF.

  SORT lt_rclst BY rcnum werks matnr posnr matnr_ins stlal.
  DELETE ADJACENT DUPLICATES FROM lt_rclst
    COMPARING rcnum werks matnr posnr matnr_ins stlal.
  IF lt_rclst[] IS NOT INITIAL.
    MODIFY zfmrclst FROM TABLE lt_rclst.
  ENDIF.

  SORT lt_similares BY rcnum stlal matnr_ins matnr_sim.
  DELETE ADJACENT DUPLICATES FROM lt_similares
    COMPARING rcnum stlal matnr_ins matnr_sim.
  IF lt_similares[] IS NOT INITIAL.
    MODIFY zfmrcsim FROM TABLE lt_similares.
  ENDIF.

  et_messages[] = lt_messages[].

ENDMETHOD.


METHOD ROUTES_CHANGE.

  DATA: lt_table        TYPE /agri/t_excel_sheet,
        lt_dd03l        TYPE thrpad_erd_dd03l,
        lt_rotas        TYPE /agri/t_glflppl,
        lt_messages     TYPE /agri/t_gprolog,
        lt_mess_collect TYPE /agri/t_gprolog,
        lt_ro01         TYPE TABLE OF zabs_str_ro_sheet1 INITIAL SIZE 0,
        lt_ro02         TYPE TABLE OF /agri/s_at_sheet2 INITIAL SIZE 0,
        lt_ro03         TYPE TABLE OF /agri/s_at_sheet3 INITIAL SIZE 0,
        lt_ro04         TYPE TABLE OF /agri/s_at_sheet4 INITIAL SIZE 0,
        lt_ro05         TYPE TABLE OF /agri/s_at_sheet5 INITIAL SIZE 0,
        lt_fl_doc       TYPE /agri/t_glfl_doc,
        lt_tplnr_read   TYPE /agri/t_gltplnr,
        lt_flhdr        TYPE /agri/t_glflot,
        lt_iflotx       TYPE /agri/t_gliflotx,
        lt_adrc         TYPE /agri/t_gladrc,
        lt_ihpa         TYPE /agri/t_glihpa,
        lt_flppl        TYPE /agri/t_glflppl,
        lt_flatg        TYPE /agri/t_glflatg,
        lt_flatv        TYPE /agri/t_glflatv,
        lt_flown        TYPE /agri/t_glflown,
        lt_flcma        TYPE /agri/t_glflcma,
        lt_flos         TYPE /agri/t_glflos,
        lwa_glflot      TYPE /agri/s_glflot,
        lwa_data        TYPE /agri/s_excel_sheet,
        lwa_dd03l       TYPE dd03l,
        lwa_message     TYPE /agri/s_gprolog,
        lv_char6        TYPE char6,
        lv_terrain      TYPE /agri/gltplnr_fl,
        lv_subrc        TYPE sysubrc,
        lv_row          TYPE /agri/s_excel_sheet-row.

  FIELD-SYMBOLS: <lwa_ro01> TYPE zabs_str_ro_sheet1,
                 <lwa_ro02> TYPE /agri/s_at_sheet2,
                 <lwa_ro03> TYPE /agri/s_at_sheet3,
                 <lwa_ro04> TYPE /agri/s_at_sheet4,
                 <lwa_ro05> TYPE /agri/s_at_sheet5,
                 <lwa_rota> TYPE /agri/s_glflppl,
                 <lv_value> TYPE any.

  CONSTANTS: BEGIN OF c_structures,
               ro_01   TYPE tabname    VALUE 'ZABS_STR_RO_SHEET1',
               ro_02   TYPE tabname    VALUE '/AGRI/S_AT_SHEET2',
               ro_03   TYPE tabname    VALUE '/AGRI/S_AT_SHEET3',
               ro_04   TYPE tabname    VALUE '/AGRI/S_AT_SHEET4',
               ro_05   TYPE tabname    VALUE '/AGRI/S_AT_SHEET5',
               ro_in   TYPE updkz_d    VALUE 'I',
               ro_up   TYPE updkz_d    VALUE 'U',
               success TYPE sy-msgty   VALUE 'S',
             END OF c_structures.

*-- Message Types
  CONSTANTS : BEGIN OF c_msg_type,
                info    LIKE sy-msgty VALUE 'I',
                warning LIKE sy-msgty VALUE 'W',
                error   LIKE sy-msgty VALUE 'E',
                abend   LIKE sy-msgty VALUE 'A',
                success LIKE sy-msgty VALUE 'S',
                x       LIKE sy-msgty VALUE 'X',
              END   OF c_msg_type.

  CONSTANTS: c_updkz_new(1)     TYPE c VALUE 'I',
             c_updkz_update(1)  TYPE c VALUE 'U',
             c_updkz_delete(1)  TYPE c VALUE 'D',
             c_updkz_old(1)     TYPE c VALUE ' ',
             c_updkz_newrow     TYPE c VALUE 'N',
             c_updkz_propose(1) TYPE c VALUE 'P'.

  lt_table[] = it_table[].

  DELETE lt_table WHERE row = 1.
  DELETE lt_table WHERE sheet <> 1.

  "Sheet 1
  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-ro_01
      i_sheet     = 1
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  SORT lt_dd03l BY position.

  LOOP AT lt_table INTO lwa_data WHERE sheet = 01.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_ro01 ASSIGNING <lwa_ro01>.
      lv_row = lwa_data-row.
    ENDIF.

    READ TABLE lt_dd03l TRANSPORTING NO FIELDS
      WITH KEY position = lwa_data-column.
    IF sy-subrc EQ 0.
      LOOP AT lt_dd03l INTO lwa_dd03l FROM sy-tabix.
        IF lwa_dd03l-position NE lwa_data-column.
          EXIT.
        ENDIF.
        ASSIGN COMPONENT lwa_data-fieldname
          OF STRUCTURE <lwa_ro01> TO <lv_value>.
        IF sy-subrc EQ 0.
          <lv_value> = lwa_data-value.
        ENDIF.
      ENDLOOP.
    ENDIF.

    <lwa_ro01>-excel_row = lwa_data-row.
    <lwa_ro01>-excel_column = lwa_data-column.
  ENDLOOP.
  CLEAR lv_row.

  IF lt_ro01[] IS NOT INITIAL.
    DATA(lt_pwerk) = lt_ro01[].
    SORT lt_pwerk BY pwerk.
    DELETE lt_pwerk WHERE pwerk IS INITIAL.
    IF lt_pwerk[] IS NOT INITIAL.
      SELECT werks, name1
        FROM t001w
        INTO TABLE @DATA(lt_t001w)
        FOR ALL ENTRIES IN @lt_pwerk[]
       WHERE werks = @lt_pwerk-pwerk.

      SORT lt_t001w BY werks.
    ENDIF.
  ENDIF.

  IF lt_ro01[] IS NOT INITIAL.
    DATA(lt_route) = lt_ro01[].
    SORT lt_route BY route.
    DELETE lt_route WHERE route IS INITIAL.
    IF lt_route[] IS NOT INITIAL.
      SELECT route, rttyp, txtgr
        FROM /agri/glrthdr
        INTO TABLE @DATA(lt_glrthdr)
        FOR ALL ENTRIES IN @lt_route[]
       WHERE route = @lt_route-route.

      SORT lt_glrthdr BY route.
    ENDIF.
  ENDIF.

  DATA(lv_terrain_w_error) = abap_false.
  LOOP AT lt_ro01 ASSIGNING <lwa_ro01>.
    AT NEW tplnr_fl.
      CLEAR lv_terrain_w_error.
    ENDAT.

    IF lv_terrain_w_error EQ abap_true
    AND lv_terrain = <lwa_ro01>-tplnr_fl.
      <lwa_ro01>-line_w_error = abap_true.
      CONTINUE.
    ENDIF.

*-- Terreno
    IF <lwa_ro01>-tplnr_fl IS NOT INITIAL.
      IF strlen( <lwa_ro01>-tplnr_fl ) LT 6.
        UNPACK <lwa_ro01>-tplnr_fl TO lv_char6.
        <lwa_ro01>-tplnr_fl = lv_char6.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
        EXPORTING
          input      = <lwa_ro01>-tplnr_fl
        IMPORTING
          output     = <lwa_ro01>-tplnr_fl_in
        EXCEPTIONS
          not_found  = 1
          not_active = 2
          OTHERS     = 3.

      IF sy-subrc <> 0.
        lv_terrain_w_error = <lwa_ro01>-line_w_error = abap_true.
*-- Verificar Coluna &1 Linha &2.
        CLEAR lwa_message.
        lwa_message-msgid = 'ZABS_MSGCLS'.
        lwa_message-msgno = '143'.
        lwa_message-msgty = 'I'.
        lwa_message-msgv1 = <lwa_ro01>-excel_column.
        lwa_message-msgv2 = <lwa_ro01>-excel_row.
        APPEND lwa_message TO lt_messages.
        CLEAR lwa_message.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
        lwa_message-msgid = sy-msgid.
        lwa_message-msgno = sy-msgno.
        lwa_message-msgty = sy-msgty.
        lwa_message-msgv1 = sy-msgv1.
        lwa_message-msgv2 = sy-msgv2.
        lwa_message-msgv3 = sy-msgv3.
        lwa_message-msgv4 = sy-msgv4.
        APPEND lwa_message TO lt_messages.
      ENDIF.
    ELSE.
      lv_terrain_w_error = <lwa_ro01>-line_w_error = abap_true.
*-- Verificar Coluna &1 Linha &2 do Arquivo Excel.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZABS_MSGCLS'.
      lwa_message-msgno = '143'.
      lwa_message-msgty = 'I'.
      lwa_message-msgv1 = <lwa_ro01>-excel_column.
      lwa_message-msgv2 = <lwa_ro01>-excel_row.
      APPEND lwa_message TO lt_messages.
*-- Código do Terreno não informado.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZABS_MSGCLS'.
      lwa_message-msgno = '144'.
      lwa_message-msgty = 'I'.
      APPEND lwa_message TO lt_messages.
    ENDIF.

*-- Centro de processamento
    IF <lwa_ro01>-pwerk IS NOT INITIAL.
      READ TABLE lt_t001w TRANSPORTING NO FIELDS
        WITH KEY werks = <lwa_ro01>-pwerk BINARY SEARCH.

      IF sy-subrc <> 0.
        lv_terrain_w_error = <lwa_ro01>-line_w_error = abap_true.
*-- Verificar Coluna &1 Linha &2 do Arquivo Excel.
        CLEAR lwa_message.
        lwa_message-msgid = 'ZABS_MSGCLS'.
        lwa_message-msgno = '143'.
        lwa_message-msgty = 'I'.
        lwa_message-msgv1 = <lwa_ro01>-excel_column.
        lwa_message-msgv2 = <lwa_ro01>-excel_row.
        APPEND lwa_message TO lt_messages.
*-- Centro de Processamento &1 inválido.
        CLEAR lwa_message.
        lwa_message-msgid = 'ZFMFP'.
        lwa_message-msgno = '202'.
        lwa_message-msgty = 'I'.
        lwa_message-msgv1 = <lwa_ro01>-pwerk.
        APPEND lwa_message TO lt_messages.
      ENDIF.
    ELSE.
      lv_terrain_w_error = <lwa_ro01>-line_w_error = abap_true.
*-- Verificar Coluna &1 Linha &2 do Arquivo Excel.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZABS_MSGCLS'.
      lwa_message-msgno = '143'.
      lwa_message-msgty = 'I'.
      lwa_message-msgv1 = <lwa_ro01>-excel_column.
      lwa_message-msgv2 = <lwa_ro01>-excel_row.
      APPEND lwa_message TO lt_messages.
*-- Centro de Processamento não informado.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZFMFP'.
      lwa_message-msgno = '201'.
      lwa_message-msgty = 'I'.
      APPEND lwa_message TO lt_messages.
    ENDIF.

*-- Rota
    IF <lwa_ro01>-route IS NOT INITIAL.
      READ TABLE lt_glrthdr TRANSPORTING NO FIELDS
        WITH KEY route = <lwa_ro01>-route BINARY SEARCH.

      IF sy-subrc <> 0.
        lv_terrain_w_error = <lwa_ro01>-line_w_error = abap_true.
*-- Verificar Coluna &1 Linha &2 do Arquivo Excel.
        CLEAR lwa_message.
        lwa_message-msgid = 'ZABS_MSGCLS'.
        lwa_message-msgno = '143'.
        lwa_message-msgty = 'I'.
        lwa_message-msgv1 = <lwa_ro01>-excel_column.
        lwa_message-msgv2 = <lwa_ro01>-excel_row.
        APPEND lwa_message TO lt_messages.
*-- Rota &1 inválida.
        CLEAR lwa_message.
        lwa_message-msgid = 'ZFMFP'.
        lwa_message-msgno = '204'.
        lwa_message-msgty = 'I'.
        lwa_message-msgv1 = <lwa_ro01>-route.
        APPEND lwa_message TO lt_messages.
      ENDIF.
    ELSE.
      lv_terrain_w_error = <lwa_ro01>-line_w_error = abap_true.
*-- Verificar Coluna &1 Linha &2 do Arquivo Excel.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZABS_MSGCLS'.
      lwa_message-msgno = '143'.
      lwa_message-msgty = 'I'.
      lwa_message-msgv1 = <lwa_ro01>-excel_column.
      lwa_message-msgv2 = <lwa_ro01>-excel_row.
      APPEND lwa_message TO lt_messages.
*-- Rota não informada.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZFMFP'.
      lwa_message-msgno = '205'.
      lwa_message-msgty = 'I'.
      APPEND lwa_message TO lt_messages.
    ENDIF.

*-- Distância
    IF <lwa_ro01>-dstnc IS INITIAL.
      lv_terrain_w_error = <lwa_ro01>-line_w_error = abap_true.
*-- Verificar Coluna &1 Linha &2 do Arquivo Excel.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZABS_MSGCLS'.
      lwa_message-msgno = '143'.
      lwa_message-msgty = 'I'.
      lwa_message-msgv1 = <lwa_ro01>-excel_column.
      lwa_message-msgv2 = <lwa_ro01>-excel_row.
      APPEND lwa_message TO lt_messages.
*-- Distância não informada.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZFMFP'.
      lwa_message-msgno = '203'.
      lwa_message-msgty = 'I'.
      APPEND lwa_message TO lt_messages.
    ENDIF.

*-- Unidade para a distância (quilômetros)
    IF <lwa_ro01>-dsunt IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
        EXPORTING
          input          = <lwa_ro01>-dsunt
        IMPORTING
          output         = <lwa_ro01>-dsunt
        EXCEPTIONS
          unit_not_found = 1
          OTHERS         = 2.

      IF sy-subrc <> 0.
        lv_terrain_w_error = <lwa_ro01>-line_w_error = abap_true.
*-- Verificar Coluna &1 Linha &2 do Arquivo Excel.
        CLEAR lwa_message.
        lwa_message-msgid = 'ZABS_MSGCLS'.
        lwa_message-msgno = '143'.
        lwa_message-msgty = 'I'.
        lwa_message-msgv1 = <lwa_ro01>-excel_column.
        lwa_message-msgv2 = <lwa_ro01>-excel_row.
        APPEND lwa_message TO lt_messages.
*-- Unidade de Distância &1 Inválida.
        CLEAR lwa_message.
        lwa_message-msgid = 'ZFMFP'.
        lwa_message-msgno = '199'.
        lwa_message-msgty = 'I'.
        lwa_message-msgv1 = <lwa_ro01>-dsunt.
        APPEND lwa_message TO lt_messages.
      ENDIF.
    ELSE.
      lv_terrain_w_error = <lwa_ro01>-line_w_error = abap_true.
*-- Verificar Coluna &1 Linha &2 do Arquivo Excel.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZABS_MSGCLS'.
      lwa_message-msgno = '143'.
      lwa_message-msgty = 'I'.
      lwa_message-msgv1 = <lwa_ro01>-excel_column.
      lwa_message-msgv2 = <lwa_ro01>-excel_row.
      APPEND lwa_message TO lt_messages.
*-- Unidade de Distância não informada.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZFMFP'.
      lwa_message-msgno = '200'.
      lwa_message-msgty = 'I'.
      APPEND lwa_message TO lt_messages.
    ENDIF.

    lv_terrain = <lwa_ro01>-tplnr_fl.
  ENDLOOP.

  APPEND LINES OF lt_messages TO lt_mess_collect.

  DATA(lt_tplnr) = lt_ro01[].
  DELETE ADJACENT DUPLICATES FROM lt_tplnr COMPARING tplnr_fl.

  SORT lt_ro01 BY tplnr_fl.
  LOOP AT lt_tplnr ASSIGNING FIELD-SYMBOL(<lwa_tplnr>).
    IF <lwa_tplnr>-line_w_error EQ abap_true.
      CONTINUE.
    ENDIF.

    REFRESH: lt_fl_doc, lt_messages.
    CLEAR: lwa_message, lwa_glflot, lv_subrc.
    READ TABLE lt_ro01 TRANSPORTING NO FIELDS
      WITH KEY tplnr_fl = <lwa_tplnr>-tplnr_fl BINARY SEARCH.
    IF sy-subrc EQ 0.
      REFRESH lt_tplnr_read.
      INSERT INITIAL LINE INTO TABLE lt_tplnr_read
        ASSIGNING FIELD-SYMBOL(<ls_tplnr_read>).
      IF sy-subrc EQ 0.
        <ls_tplnr_read>-tplnr_fl = <lwa_tplnr>-tplnr_fl_in.
      ENDIF.

*-- Getting all the terrain records
      CALL FUNCTION '/AGRI/GLFL_VIEW'
        EXPORTING
          it_tplnr       = lt_tplnr_read
        IMPORTING
          et_fldoc       = lt_fl_doc
        EXCEPTIONS
          no_data_exists = 1
          OTHERS         = 2.

      IF sy-subrc EQ 0.
        READ TABLE lt_fl_doc ASSIGNING FIELD-SYMBOL(<ls_fldoc_infocus>) INDEX 1.
        IF sy-subrc EQ 0.
          PERFORM document_infocus_lock IN PROGRAM saplzabs_fg_glflm
                                             USING <ls_fldoc_infocus>-x-flhdr-tplnr_fl
                                                   <ls_fldoc_infocus>-x-flhdr-strno
                                                   c_msg_type-info
                                          CHANGING lt_messages
                                                   lv_subrc.

          IF lv_subrc NE 0.
            APPEND LINES OF lt_messages TO lt_mess_collect.
            CONTINUE.
          ENDIF.

          lt_rotas[] = <ls_fldoc_infocus>-x-flppl[].

          DATA(lv_default) = 0.
          READ TABLE lt_ro01 INTO DATA(lwa_ro01)
            WITH KEY tplnr_fl = <lwa_tplnr>-tplnr_fl BINARY SEARCH.
          WHILE sy-subrc EQ 0.
            DATA(lv_tabix) = sy-tabix + 1.

            DATA(lv_updkz) = c_updkz_new.
            UNASSIGN <lwa_rota>.
            READ TABLE lt_rotas ASSIGNING <lwa_rota>
              WITH KEY tplnr_fl = lwa_ro01-tplnr_fl_in
                       pwerk    = lwa_ro01-pwerk.
            IF sy-subrc NE 0.
              INSERT INITIAL LINE INTO TABLE lt_rotas ASSIGNING <lwa_rota>.
            ELSE.
              lv_updkz = c_updkz_update.
            ENDIF.

            IF <lwa_rota> IS ASSIGNED.
              MOVE-CORRESPONDING lwa_ro01 TO <lwa_rota>.
              <lwa_rota>-tplnr_fl = lwa_ro01-tplnr_fl_in.
              <lwa_rota>-updkz = lv_updkz.
              IF lwa_ro01-dfult EQ abap_true.
                ADD 1 TO lv_default.
              ENDIF.
            ELSE.
              CONTINUE.
            ENDIF.

            READ TABLE lt_ro01 INTO lwa_ro01 INDEX lv_tabix
              COMPARING tplnr_fl.
          ENDWHILE.

          <ls_fldoc_infocus>-x-flppl[] = lt_rotas[].

          MOVE-CORRESPONDING <ls_fldoc_infocus>-x-flhdr  TO lwa_glflot.
          lwa_glflot-updkz = c_updkz_update.

          REFRESH lt_messages.
          CALL FUNCTION '/AGRI/GLFL_CHANGE'
            EXPORTING
              is_flhdr                = lwa_glflot
            IMPORTING
              et_messages             = lt_messages
            CHANGING
              cs_glfl_doc             = <ls_fldoc_infocus>
            EXCEPTIONS
              no_documents_to_process = 1
              no_authorization        = 2
              change_failed           = 3
              terrain_locked          = 4
              OTHERS                  = 5.

          APPEND LINES OF lt_messages TO lt_mess_collect.

          PERFORM document_infocus_unlock IN PROGRAM saplzabs_fg_glflm
                                               USING <ls_fldoc_infocus>-x-flhdr-tplnr_fl
                                                     <ls_fldoc_infocus>-x-flhdr-strno.
        ENDIF.
      ELSE.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
        lwa_message-msgid = sy-msgid.
        lwa_message-msgno = sy-msgno.
        lwa_message-msgty = sy-msgty.
        lwa_message-msgv1 = sy-msgv1.
        lwa_message-msgv2 = sy-msgv2.
        lwa_message-msgv3 = sy-msgv3.
        lwa_message-msgv4 = sy-msgv4.
        APPEND lwa_message TO lt_mess_collect.
        CONTINUE.
      ENDIF.

      APPEND LINES OF lt_messages TO lt_mess_collect.
    ENDIF.
  ENDLOOP.

  et_messages = lt_mess_collect.

ENDMETHOD.


METHOD ROUTING_CHANGE.

  DATA: lt_table        TYPE /agri/t_excel_sheet,
        lt_dd03l        TYPE thrpad_erd_dd03l,
        lt_messages     TYPE /agri/t_gprolog,
        lt_bdcdata      TYPE bdcdata_tab,
        lt_bdc_messages TYPE ettcd_msg_tabtype,
        lt_at01         TYPE STANDARD TABLE OF zabs_str_routing_sheet1 INITIAL SIZE 0,
        lt_at02         TYPE TABLE OF /agri/s_at_sheet2 INITIAL SIZE 0,
        lt_at03         TYPE TABLE OF /agri/s_at_sheet3 INITIAL SIZE 0,
        lt_at04         TYPE TABLE OF /agri/s_at_sheet4 INITIAL SIZE 0,
        lt_at05         TYPE TABLE OF /agri/s_at_sheet5 INITIAL SIZE 0,
        lrt_matnr       TYPE RANGE OF matnr,
        lrt_werks       TYPE RANGE OF werks_d,
        lwa_bdc_options TYPE ctu_params,
        lwa_aux         LIKE LINE OF lt_at01,
        lwa_matnr       LIKE LINE OF lrt_matnr,
        lwa_werks       LIKE LINE OF lrt_werks,
        lwa_data        TYPE /agri/s_excel_sheet,
        lwa_dd03l       TYPE dd03l,
        lwa_message     TYPE /agri/s_gprolog,
        lv_matnr_bdc    TYPE bdc_fval,
        lv_werks_bdc    TYPE bdc_fval,
        lv_posnr_bdc    TYPE bdc_fval,
        lv_vornr_bdc    TYPE bdc_fval,
        lv_sttag_bdc    TYPE bdc_fval,
        lv_matnr        TYPE matnr,
        lv_temp_row     TYPE i,
        lv_row          TYPE /agri/s_excel_sheet-row.

  FIELD-SYMBOLS: <lwa_at01> TYPE zabs_str_routing_sheet1,
                 <lwa_at02> TYPE /agri/s_at_sheet2,
                 <lwa_at03> TYPE /agri/s_at_sheet3,
                 <lwa_at04> TYPE /agri/s_at_sheet4,
                 <lwa_at05> TYPE /agri/s_at_sheet5,
                 <lv_value> TYPE any.

  CONSTANTS: BEGIN OF c_structures,
               at_01   TYPE tabname    VALUE 'ZABS_STR_ROUTING_SHEET1',
               at_02   TYPE tabname    VALUE '/AGRI/S_AT_SHEET2',
               at_03   TYPE tabname    VALUE '/AGRI/S_AT_SHEET3',
               at_04   TYPE tabname    VALUE '/AGRI/S_AT_SHEET4',
               at_05   TYPE tabname    VALUE '/AGRI/S_AT_SHEET5',
               klart   TYPE klassenart VALUE 'X91',
               at_in   TYPE updkz_d    VALUE 'I',
               at_up   TYPE updkz_d    VALUE 'U',
               success TYPE sy-msgty   VALUE 'S',
             END OF c_structures.

  CONSTANTS: lc_change_routing TYPE sy-tcode VALUE 'CA02'.

  lt_table[] = it_table[].

  DELETE lt_table WHERE row = 1.
  DELETE lt_table WHERE sheet <> 1.

  "Sheet 1
  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-at_01
      i_sheet     = 1
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  SORT lt_dd03l BY position.

  LOOP AT lt_table INTO lwa_data WHERE sheet = 01.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_at01 ASSIGNING <lwa_at01>.
      lv_row = lwa_data-row.
    ENDIF.

    READ TABLE lt_dd03l TRANSPORTING NO FIELDS
      WITH KEY position = lwa_data-column.
    IF sy-subrc EQ 0.
      LOOP AT lt_dd03l INTO lwa_dd03l FROM sy-tabix.
        IF lwa_dd03l-position NE lwa_data-column.
          EXIT.
        ENDIF.
        ASSIGN COMPONENT lwa_data-fieldname
          OF STRUCTURE <lwa_at01> TO <lv_value>.
        IF sy-subrc EQ 0.
          <lv_value> = lwa_data-value.
        ENDIF.
      ENDLOOP.
    ENDIF.

    <lwa_at01>-excel_row = lwa_data-row.
    <lwa_at01>-excel_column = lwa_data-column.
  ENDLOOP.
  CLEAR lv_row.

  IF lt_at01[] IS NOT INITIAL.
    DATA(lt_werks) = lt_at01[].
    DELETE lt_werks WHERE werks IS INITIAL.

    IF lt_werks[] IS NOT INITIAL.
      SELECT werks AS low
        INTO CORRESPONDING FIELDS OF TABLE @lrt_werks
        FROM t001w
        FOR ALL ENTRIES IN @lt_werks
       WHERE werks EQ @lt_werks-werks.

      lwa_werks-sign = 'I'.
      lwa_werks-option = 'EQ'.
      MODIFY lrt_werks FROM lwa_werks TRANSPORTING sign option WHERE low <> ''.
      SORT: lrt_werks BY low.
      DELETE ADJACENT DUPLICATES: FROM lrt_werks COMPARING low.
      REFRESH lt_werks.
    ENDIF.

    LOOP AT lt_at01 ASSIGNING <lwa_at01>.
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input        = <lwa_at01>-matnr
        IMPORTING
          output       = <lwa_at01>-matnr
        EXCEPTIONS
          length_error = 1
          OTHERS       = 2.
    ENDLOOP.

    DATA(lt_matnr) = lt_at01[].
    DELETE lt_matnr WHERE matnr IS INITIAL.

    IF lt_matnr[] IS NOT INITIAL.
      SELECT matnr AS low
        INTO CORRESPONDING FIELDS OF TABLE @lrt_matnr
        FROM mara
        FOR ALL ENTRIES IN @lt_matnr
       WHERE matnr EQ @lt_matnr-matnr.

      lwa_matnr-sign = 'I'.
      lwa_matnr-option = 'EQ'.
      MODIFY lrt_matnr FROM lwa_matnr TRANSPORTING sign option WHERE low <> ''.
      SORT: lrt_matnr BY low.
      DELETE ADJACENT DUPLICATES: FROM lrt_matnr COMPARING low.
      REFRESH lt_matnr.
    ENDIF.

    DATA(lv_invalid_material) = abap_false.
    LOOP AT lt_at01 ASSIGNING <lwa_at01>.
      AT NEW matnr.
        CLEAR lv_invalid_material.
      ENDAT.

      IF lv_invalid_material EQ abap_true
      AND lv_matnr = <lwa_at01>-matnr.
        <lwa_at01>-line_w_error = abap_true.
        CONTINUE.
      ENDIF.

*-- Check Material Code
      IF <lwa_at01>-matnr IS NOT INITIAL.
        READ TABLE lrt_matnr INTO lwa_matnr
          WITH KEY low = <lwa_at01>-matnr BINARY SEARCH.

        IF sy-subrc <> 0.
          lv_invalid_material = <lwa_at01>-line_w_error = abap_true.
*-- Verificar Material da Linha &1.
          CLEAR lwa_message.
          lwa_message-msgid   = 'ZABS_MSGCLS'.
          lwa_message-msgno   = '150'.
          lwa_message-msgty   = 'E'.
          lwa_message-msgv1   = <lwa_at01>-excel_row.
          APPEND lwa_message TO lt_messages.
          CLEAR lwa_message.
*-- Material &1 inexistente.
          CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
            EXPORTING
              input  = <lwa_at01>-matnr
            IMPORTING
              output = <lwa_at01>-matnr.

          CLEAR lwa_message.
          lwa_message-msgid   = 'ZABS_MSGCLS'.
          lwa_message-msgno   = '154'.
          lwa_message-msgty   = 'E'.
          lwa_message-msgv1   = <lwa_at01>-matnr.
          APPEND lwa_message TO lt_messages.
        ENDIF.
      ELSE.
        lv_invalid_material = <lwa_at01>-line_w_error = abap_true.
*-- Verificar Material da Linha &1.
        CLEAR lwa_message.
        lwa_message-msgid   = 'ZABS_MSGCLS'.
        lwa_message-msgno   = '150'.
        lwa_message-msgty   = 'E'.
        lwa_message-msgv1   = <lwa_at01>-excel_row.
        APPEND lwa_message TO lt_messages.
*-- Código do Material não informado.
        CLEAR lwa_message.
        lwa_message-msgid   = 'ZABS_MSGCLS'.
        lwa_message-msgno   = '149'.
        lwa_message-msgty   = 'E'.
        APPEND lwa_message TO lt_messages.
      ENDIF.

      lv_matnr = <lwa_at01>-matnr.

*-- Check Plant
      IF <lwa_at01>-werks IS NOT INITIAL.
        READ TABLE lrt_werks INTO lwa_werks
          WITH KEY low = <lwa_at01>-werks BINARY SEARCH.
        IF sy-subrc NE 0.
*-- Verificar Centro da Linha &1.
          CLEAR lwa_message.
          lwa_message-msgid   = 'ZABS_MSGCLS'.
          lwa_message-msgno   = '151'.
          lwa_message-msgty   = 'E'.
          lwa_message-msgv1   = <lwa_at01>-excel_row.
          APPEND lwa_message TO lt_messages.
*-- Centro &1 inexistente.
          CLEAR lwa_message.
          lwa_message-msgid   = 'ZABS_MSGCLS'.
          lwa_message-msgno   = '153'.
          lwa_message-msgty   = 'E'.
          lwa_message-msgv1   = <lwa_at01>-werks.
          APPEND lwa_message TO lt_messages.
        ENDIF.
      ELSE.
*-- Verificar Centro da Linha &1.
        CLEAR lwa_message.
        lwa_message-msgid   = 'ZABS_MSGCLS'.
        lwa_message-msgno   = '151'.
        lwa_message-msgty   = 'E'.
        lwa_message-msgv1   = <lwa_at01>-excel_row.
        APPEND lwa_message TO lt_messages.
*-- Centro não informado.
        CLEAR lwa_message.
        lwa_message-msgid   = 'ZABS_MSGCLS'.
        lwa_message-msgno   = '152'.
        lwa_message-msgty   = 'E'.
        APPEND lwa_message TO lt_messages.
      ENDIF.
    ENDLOOP.
  ENDIF.

  DELETE lt_at01 WHERE line_w_error = abap_true.
  SORT lt_at01 BY matnr werks sttag posnr.
  DATA(lt_routing) = lt_at01[].
  DELETE ADJACENT DUPLICATES FROM lt_routing COMPARING matnr werks sttag.

  CLEAR lwa_aux.
  DATA(lv_rows) = lines( lt_at01 ).
  LOOP AT lt_at01 ASSIGNING <lwa_at01>.
    DATA(lv_tabix) = sy-tabix.
    IF lwa_aux-matnr NE <lwa_at01>-matnr
    OR lwa_aux-werks NE <lwa_at01>-werks
    OR lwa_aux-sttag NE <lwa_at01>-sttag.
      DATA(lt_temp) = lt_at01[].
      DELETE lt_temp WHERE matnr NE <lwa_at01>-matnr
                        OR werks NE <lwa_at01>-werks
                        OR sttag NE <lwa_at01>-sttag.
      DATA(lv_temp) = lines( lt_temp ).
      CLEAR lv_temp_row.
    ENDIF.
    ADD 1 TO lv_temp_row.

    UNPACK <lwa_at01>-posnr TO <lwa_at01>-posnr.
    UNPACK <lwa_at01>-vornr TO <lwa_at01>-vornr.

    lwa_aux = <lwa_at01>.
    IF lv_rows = lv_tabix
    OR lv_temp_row = lv_temp.
      <lwa_at01>-last_row = abap_true.
    ENDIF.
  ENDLOOP.

  lwa_bdc_options-dismode  = 'N'.
  lwa_bdc_options-nobiend  = mc_true.
  lwa_bdc_options-updmode  = 'S'.
  lwa_bdc_options-nobinpt  = mc_true.

  CLEAR: mt_bdcdata[], ms_bdcdata.
  LOOP AT lt_routing INTO DATA(lwa_routing).
    DATA(lv_fill_dynpro) = abap_true.
    READ TABLE lt_at01 INTO DATA(ls_at01)
      WITH KEY matnr = lwa_routing-matnr
               werks = lwa_routing-werks
               sttag = lwa_routing-sttag BINARY SEARCH.
    WHILE sy-subrc EQ 0.
      lv_tabix = sy-tabix + 1.
      lv_matnr_bdc = ls_at01-matnr.
      lv_werks_bdc = ls_at01-werks.
      lv_posnr_bdc = ls_at01-posnr.
      lv_vornr_bdc = ls_at01-vornr.
      lv_sttag_bdc = ls_at01-sttag.

      IF lv_fill_dynpro EQ abap_true.
        lv_fill_dynpro = abap_false.
        dynpro_fill( i_program = 'SAPLCPDI' i_dynpro  = '1010' ).
        field_fill( i_fnam = 'BDC_OKCODE'  i_fval = '=XALU' ).
        field_fill( i_fnam = 'RC27M-MATNR' i_fval = lv_matnr_bdc ).
        field_fill( i_fnam = 'RC27M-WERKS' i_fval = lv_werks_bdc ).
        field_fill( i_fnam = 'RC271-STTAG' i_fval = lv_sttag_bdc ).

        dynpro_fill( i_program = 'SAPLCPDI' i_dynpro  = '1400' ).
        field_fill( i_fnam = 'BDC_OKCODE'  i_fval = '=MATA' ).
        field_fill( i_fnam = 'RC27X-ENTRY_ACT' i_fval = '1' ).
      ENDIF.

      dynpro_fill( i_program = 'SAPLCMDI' i_dynpro  = '1000' )."12
      field_fill( i_fnam = 'BDC_OKCODE'  i_fval = '=FIND' ).

      dynpro_fill( i_program = 'SAPLCM01' i_dynpro  = '1055' )."15
      field_fill( i_fnam = 'BDC_OKCODE'  i_fval = '=GOON' ).
      field_fill( i_fnam = 'SEARCH_BY-POSNR' i_fval = lv_posnr_bdc ).
      field_fill( i_fnam = 'FIRST_HIT' i_fval = 'X' ).

      dynpro_fill( i_program = 'SAPLCMDI' i_dynpro  = '1000' )."20
      field_fill( i_fnam = 'BDC_OKCODE'  i_fval = '=NEW' ).

      dynpro_fill( i_program = 'SAPLCM01' i_dynpro  = '1090' )."23
      field_fill( i_fnam = 'BDC_OKCODE'  i_fval = '=GOON' ).
      field_fill( i_fnam = 'RCM01-VORNR' i_fval = lv_vornr_bdc ).
      field_fill( i_fnam = 'RCM01-PLNFL' i_fval = '0' ).

      IF ls_at01-last_row EQ abap_false.
        dynpro_fill( i_program = 'SAPLCMDI' i_dynpro  = '1000' )."28
        field_fill( i_fnam = 'BDC_OKCODE'  i_fval = '=MARD' ).
      ELSE.
        dynpro_fill( i_program = 'SAPLCMDI' i_dynpro  = '1000' )."66
        field_fill( i_fnam = 'BDC_OKCODE'  i_fval = '=BU' ).
      ENDIF.

      READ TABLE lt_at01 INTO ls_at01 INDEX lv_tabix
        COMPARING matnr werks sttag.
    ENDWHILE.

    REFRESH: lt_bdc_messages[].
    CALL METHOD /agri/cl_global_services=>transaction_call_process
      EXPORTING
        i_tcode                = lc_change_routing
        i_use_bdc_data         = mc_true
        is_bdc_options         = lwa_bdc_options
        it_bdcdata             = mt_bdcdata[]
      IMPORTING
        et_bdc_messages        = lt_bdc_messages
      EXCEPTIONS
        invalid_parameters     = 1
        call_transaction_error = 2
        OTHERS                 = 3.

    LOOP AT lt_bdc_messages INTO DATA(lwa_bdc_msg).
      CLEAR lwa_message.
      lwa_message-msgty   = lwa_bdc_msg-msgtyp.
      lwa_message-msgid   = lwa_bdc_msg-msgid.
      lwa_message-msgno   = lwa_bdc_msg-msgnr.
      lwa_message-msgv1   = lwa_bdc_msg-msgv1.
      lwa_message-msgv2   = lwa_bdc_msg-msgv2.
      lwa_message-msgv3   = lwa_bdc_msg-msgv3.
      lwa_message-msgv4   = lwa_bdc_msg-msgv4.
      APPEND lwa_message TO lt_messages.
    ENDLOOP.
    CLEAR: mt_bdcdata[], ms_bdcdata.
  ENDLOOP.

  et_messages[] = lt_messages[].

ENDMETHOD.


METHOD solidosha_update.

  DATA: lt_table        TYPE /agri/t_excel_sheet,
        lt_solidosha    TYPE STANDARD TABLE OF zabst_solidosha INITIAL SIZE 0,
        lt_dd03l        TYPE thrpad_erd_dd03l,
        lt_rotas        TYPE /agri/t_glflppl,
        lt_messages     TYPE /agri/t_gprolog,
        lt_mess_collect TYPE /agri/t_gprolog,
        lt_ro01         TYPE TABLE OF zabs_str_solidosha_sheet1 INITIAL SIZE 0,
        lt_ro02         TYPE TABLE OF /agri/s_at_sheet2 INITIAL SIZE 0,
        lt_ro03         TYPE TABLE OF /agri/s_at_sheet3 INITIAL SIZE 0,
        lt_ro04         TYPE TABLE OF /agri/s_at_sheet4 INITIAL SIZE 0,
        lt_ro05         TYPE TABLE OF /agri/s_at_sheet5 INITIAL SIZE 0,
        lt_fl_doc       TYPE /agri/t_glfl_doc,
        lt_tplnr_read   TYPE /agri/t_gltplnr,
        lt_flhdr        TYPE /agri/t_glflot,
        lt_iflotx       TYPE /agri/t_gliflotx,
        lt_adrc         TYPE /agri/t_gladrc,
        lt_ihpa         TYPE /agri/t_glihpa,
        lt_flppl        TYPE /agri/t_glflppl,
        lt_flatg        TYPE /agri/t_glflatg,
        lt_flatv        TYPE /agri/t_glflatv,
        lt_flown        TYPE /agri/t_glflown,
        lt_flcma        TYPE /agri/t_glflcma,
        lt_flos         TYPE /agri/t_glflos,
        lwa_glflot      TYPE /agri/s_glflot,
        lwa_data        TYPE /agri/s_excel_sheet,
        lwa_dd03l       TYPE dd03l,
        lwa_message     TYPE /agri/s_gprolog,
        lv_char6        TYPE char6,
        lv_terrain      TYPE /agri/gltplnr_fl,
        lv_subrc        TYPE sysubrc,
        lv_row          TYPE /agri/s_excel_sheet-row.

  FIELD-SYMBOLS: <lwa_ro01> TYPE zabs_str_solidosha_sheet1,
                 <lwa_ro02> TYPE /agri/s_at_sheet2,
                 <lwa_ro03> TYPE /agri/s_at_sheet3,
                 <lwa_ro04> TYPE /agri/s_at_sheet4,
                 <lwa_ro05> TYPE /agri/s_at_sheet5,
                 <lwa_rota> TYPE /agri/s_glflppl,
                 <lv_value> TYPE any.

  CONSTANTS: BEGIN OF c_structures,
               ro_01   TYPE tabname    VALUE 'ZABS_STR_SOLIDOSHA_SHEET1',
               ro_02   TYPE tabname    VALUE '/AGRI/S_AT_SHEET2',
               ro_03   TYPE tabname    VALUE '/AGRI/S_AT_SHEET3',
               ro_04   TYPE tabname    VALUE '/AGRI/S_AT_SHEET4',
               ro_05   TYPE tabname    VALUE '/AGRI/S_AT_SHEET5',
               ro_in   TYPE updkz_d    VALUE 'I',
               ro_up   TYPE updkz_d    VALUE 'U',
               success TYPE sy-msgty   VALUE 'S',
             END OF c_structures.

*-- Message Types
  CONSTANTS : BEGIN OF c_msg_type,
                info    LIKE sy-msgty VALUE 'I',
                warning LIKE sy-msgty VALUE 'W',
                error   LIKE sy-msgty VALUE 'E',
                abend   LIKE sy-msgty VALUE 'A',
                success LIKE sy-msgty VALUE 'S',
                x       LIKE sy-msgty VALUE 'X',
              END   OF c_msg_type.

  CONSTANTS: c_updkz_new(1)     TYPE c VALUE 'I',
             c_updkz_update(1)  TYPE c VALUE 'U',
             c_updkz_delete(1)  TYPE c VALUE 'D',
             c_updkz_old(1)     TYPE c VALUE ' ',
             c_updkz_newrow     TYPE c VALUE 'N',
             c_updkz_propose(1) TYPE c VALUE 'P'.

  lt_table[] = it_table[].

  DELETE lt_table WHERE row = 1.
  DELETE lt_table WHERE sheet <> 1.

  "Sheet 1
  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-ro_01
      i_sheet     = 1
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  SORT lt_dd03l BY position.

  LOOP AT lt_table INTO lwa_data WHERE sheet = 01.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_ro01 ASSIGNING <lwa_ro01>.
      lv_row = lwa_data-row.
    ENDIF.

    READ TABLE lt_dd03l TRANSPORTING NO FIELDS
      WITH KEY position = lwa_data-column.
    IF sy-subrc EQ 0.
      LOOP AT lt_dd03l INTO lwa_dd03l FROM sy-tabix.
        IF lwa_dd03l-position NE lwa_data-column.
          EXIT.
        ENDIF.
        ASSIGN COMPONENT lwa_data-fieldname
          OF STRUCTURE <lwa_ro01> TO <lv_value>.
        IF sy-subrc EQ 0.
          IF lwa_data-fieldname EQ 'TAXA_VARIACAO'.
            TRANSLATE lwa_data-value USING ',.'.
          ELSEIF lwa_data-fieldname EQ 'SEMANA'.
            lwa_data-value = lwa_data-value+2(4) && lwa_data-value(2).
          ENDIF.
          <lv_value> = lwa_data-value.
        ENDIF.
      ENDLOOP.
    ENDIF.

    <lwa_ro01>-excel_row = lwa_data-row.
    <lwa_ro01>-excel_column = lwa_data-column.
  ENDLOOP.
  CLEAR lv_row.

  DATA(lv_terrain_w_error) = abap_false.
  LOOP AT lt_ro01 ASSIGNING <lwa_ro01>.
    AT NEW tplnr_fl.
      CLEAR lv_terrain_w_error.
    ENDAT.

    IF lv_terrain_w_error EQ abap_true
    AND lv_terrain = <lwa_ro01>-tplnr_fl.
      <lwa_ro01>-line_w_error = abap_true.
      CONTINUE.
    ENDIF.

    IF strlen( <lwa_ro01>-cod_imov ) LT 6.
      UNPACK <lwa_ro01>-cod_imov TO lv_char6.
      <lwa_ro01>-cod_imov = lv_char6.
    ENDIF.

*-- Terreno
    IF <lwa_ro01>-tplnr_fl IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
        EXPORTING
          input      = <lwa_ro01>-tplnr_fl
        IMPORTING
          output     = <lwa_ro01>-tplnr_fl_in
        EXCEPTIONS
          not_found  = 1
          not_active = 2
          OTHERS     = 3.

      IF sy-subrc <> 0.
        lv_terrain_w_error = <lwa_ro01>-line_w_error = abap_true.
*-- Verificar Coluna &1 Linha &2.
        CLEAR lwa_message.
        lwa_message-msgid = 'ZABS_MSGCLS'.
        lwa_message-msgno = '143'.
        lwa_message-msgty = 'I'.
        lwa_message-msgv1 = <lwa_ro01>-excel_column.
        lwa_message-msgv2 = <lwa_ro01>-excel_row.
        APPEND lwa_message TO lt_messages.
        CLEAR lwa_message.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
        lwa_message-msgid = sy-msgid.
        lwa_message-msgno = sy-msgno.
        lwa_message-msgty = sy-msgty.
        lwa_message-msgv1 = sy-msgv1.
        lwa_message-msgv2 = sy-msgv2.
        lwa_message-msgv3 = sy-msgv3.
        lwa_message-msgv4 = sy-msgv4.
        APPEND lwa_message TO lt_messages.
      ENDIF.
    ELSE.
      lv_terrain_w_error = <lwa_ro01>-line_w_error = abap_true.
*-- Verificar Coluna &1 Linha &2 do Arquivo Excel.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZABS_MSGCLS'.
      lwa_message-msgno = '143'.
      lwa_message-msgty = 'I'.
      lwa_message-msgv1 = <lwa_ro01>-excel_column.
      lwa_message-msgv2 = <lwa_ro01>-excel_row.
      APPEND lwa_message TO lt_messages.
*-- Código do Terreno não informado.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZABS_MSGCLS'.
      lwa_message-msgno = '144'.
      lwa_message-msgty = 'I'.
      APPEND lwa_message TO lt_messages.
    ENDIF.

*-- Semana/ano de calendário
    IF <lwa_ro01>-semana IS INITIAL.
      lv_terrain_w_error = <lwa_ro01>-line_w_error = abap_true.
*-- Verificar Coluna &1 Linha &2 do Arquivo Excel.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZABS_MSGCLS'.
      lwa_message-msgno = '143'.
      lwa_message-msgty = 'I'.
      lwa_message-msgv1 = <lwa_ro01>-excel_column.
      lwa_message-msgv2 = <lwa_ro01>-excel_row.
      APPEND lwa_message TO lt_messages.
*-- Semana/ano não informados.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZFMFP'.
      lwa_message-msgno = '335'.
      lwa_message-msgty = 'I'.
      APPEND lwa_message TO lt_messages.
    ENDIF.

*-- Código do Imóvel
    IF <lwa_ro01>-cod_imov IS NOT INITIAL.
      IF <lwa_ro01>-cod_imov CN ' 0123456789'.
        lv_terrain_w_error = <lwa_ro01>-line_w_error = abap_true.
*-- Verificar Coluna &1 Linha &2 do Arquivo Excel.
        CLEAR lwa_message.
        lwa_message-msgid = 'ZABS_MSGCLS'.
        lwa_message-msgno = '143'.
        lwa_message-msgty = 'I'.
        lwa_message-msgv1 = <lwa_ro01>-excel_column.
        lwa_message-msgv2 = <lwa_ro01>-excel_row.
        APPEND lwa_message TO lt_messages.
*-- Código do Imóvel &1 inválido.
        CLEAR lwa_message.
        lwa_message-msgid = 'ZFMFP'.
        lwa_message-msgno = '337'.
        lwa_message-msgty = 'I'.
        lwa_message-msgv1 = <lwa_ro01>-cod_imov.
        APPEND lwa_message TO lt_messages.
      ENDIF.
    ELSE.
      lv_terrain_w_error = <lwa_ro01>-line_w_error = abap_true.
*-- Verificar Coluna &1 Linha &2 do Arquivo Excel.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZABS_MSGCLS'.
      lwa_message-msgno = '143'.
      lwa_message-msgty = 'I'.
      lwa_message-msgv1 = <lwa_ro01>-excel_column.
      lwa_message-msgv2 = <lwa_ro01>-excel_row.
      APPEND lwa_message TO lt_messages.
*-- Código do Imóvel não informado.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZFMFP'.
      lwa_message-msgno = '336'.
      lwa_message-msgty = 'I'.
      APPEND lwa_message TO lt_messages.
    ENDIF.

    lv_terrain = <lwa_ro01>-tplnr_fl.
  ENDLOOP.

  APPEND LINES OF lt_messages TO lt_mess_collect.

  LOOP AT lt_ro01 ASSIGNING <lwa_ro01>.
    IF <lwa_ro01>-line_w_error EQ abap_false.
      INSERT INITIAL LINE INTO TABLE lt_solidosha
        ASSIGNING FIELD-SYMBOL(<lwa_solidosha>).
      IF sy-subrc EQ 0.
        <lwa_solidosha>-safra = <lwa_ro01>-safra.
        <lwa_solidosha>-tplnr_fl = <lwa_ro01>-tplnr_fl.
        <lwa_solidosha>-semana = <lwa_ro01>-semana.
        <lwa_solidosha>-cod_imov = <lwa_ro01>-cod_imov.
        <lwa_solidosha>-taxa_variacao = <lwa_ro01>-taxa_variacao.
      ENDIF.
    ENDIF.
  ENDLOOP.


  IF lt_solidosha[] IS NOT INITIAL.
    MODIFY zabst_solidosha FROM TABLE lt_solidosha.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
      DATA(lv_sucesso) = abap_true.
    ELSE.
      lv_sucesso = abap_false.
      ROLLBACK WORK.
    ENDIF.
  ELSE.
    lv_sucesso = abap_false.
  ENDIF.

  IF lv_sucesso = abap_true.
*-- Tabela ZABST_SOLIDOSHA atualizada com sucesso!
    CLEAR lwa_message.
    lwa_message-msgid = 'ZFMFP'.
    lwa_message-msgno = '338'.
    lwa_message-msgty = 'S'.
    APPEND lwa_message TO lt_mess_collect.
  ELSE.
*-- Não existem dados válidos a serem atualizados na tabela ZABST_SOLIDOSHA
    CLEAR lwa_message.
    lwa_message-msgid = 'ZFMFP'.
    lwa_message-msgno = '339'.
    lwa_message-msgty = 'I'.
    APPEND lwa_message TO lt_mess_collect.
  ENDIF.

  et_messages = lt_mess_collect.

ENDMETHOD.


METHOD STANDARD_STRUCTURE_BUILD.

  DATA: lwa_dd03l TYPE dd03l.

  FIELD-SYMBOLS: <lwa_data> TYPE /agri/s_excel_sheet.

  SELECT * INTO TABLE et_dd03l                "#EC CI_ALL_FIELDS_NEEDED
    FROM dd03l
    WHERE tabname = i_structure.

  CASE i_structure.
    WHEN '/AGRI/S_MD_SHEET1'.
      DELETE et_dd03l WHERE fieldname EQ '.INCLU--AP'.

      LOOP AT et_dd03l ASSIGNING FIELD-SYMBOL(<fs_dd031>).
        <fs_dd031>-position = sy-tabix.
      ENDLOOP.
    WHEN '/AGRI/S_CS_SHEET'.
      DELETE et_dd03l WHERE fieldname EQ '.INCLUDE'
                         OR fieldname EQ '.INCLU--AP'.
      SORT et_dd03l BY position.

      LOOP AT et_dd03l ASSIGNING <fs_dd031>.
        <fs_dd031>-position = sy-tabix.
      ENDLOOP.

    WHEN '/AGRI/S_IE_SHEET1'.
      DELETE et_dd03l WHERE fieldname EQ 'STSMA'
                         OR fieldname EQ 'OBJNR'
                         OR fieldname EQ 'KFRST'
                         OR fieldname EQ 'USTAT'
                         OR fieldname EQ 'UPDKZ'
                         OR fieldname EQ '.INCLUDE'
                         OR fieldname EQ '.INCLU--AP'.
      SORT et_dd03l BY position.

      LOOP AT et_dd03l ASSIGNING <fs_dd031>.
        <fs_dd031>-position = sy-tabix.
      ENDLOOP.

    WHEN '/AGRI/S_IE_SHEET3'.
      DELETE et_dd03l WHERE fieldname EQ 'UPDKZ'
                         OR fieldname EQ '.INCLUDE'
                         OR fieldname EQ '.INCLU--AP'.
      SORT et_dd03l BY position.

      LOOP AT et_dd03l ASSIGNING <fs_dd031>.
        <fs_dd031>-position = sy-tabix.
      ENDLOOP.

    WHEN OTHERS.
  ENDCASE.

  LOOP AT ct_table ASSIGNING <lwa_data> WHERE sheet = i_sheet. "#EC CI_STDSEQ
    <lwa_data>-column = <lwa_data>-column - 1.
    READ TABLE et_dd03l INTO lwa_dd03l
                        WITH KEY position = <lwa_data>-column. "#EC CI_STDSEQ
    IF sy-subrc EQ 0.
      <lwa_data>-fieldname = lwa_dd03l-fieldname.
    ENDIF.
  ENDLOOP.

  IF i_structure EQ '/AGRI/S_TR_SHEET1'.

    LOOP AT ct_table ASSIGNING <lwa_data> WHERE sheet = '1'
                                            AND fieldname = 'GAREA'. "#EC CI_STDSEQ

      TRANSLATE <lwa_data>-value USING ',.. '.
      CONDENSE <lwa_data>-value NO-GAPS.

    ENDLOOP.

  ENDIF.

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


METHOD TERRAINS_CREATE.

  DATA: lt_glflot       TYPE TABLE OF /agri/s_glflot,
        lt_klah         TYPE TABLE OF klah,
        lt_table        TYPE /agri/t_excel_sheet,
        lt_dd03l        TYPE thrpad_erd_dd03l,    "ishmed_t_dd03l,
        lwa_dd03l       TYPE dd03l,
        lwa_data        TYPE /agri/s_excel_sheet,
        lwa_glflot      TYPE /agri/s_glflot,
        lwa_klah        TYPE klah,
        lv_row          TYPE /agri/s_excel_sheet-row,

*        lt_gliflot      TYPE TABLE OF /agri/s_gliflot,
*        lwa_gliflot     TYPE /agri/s_gliflot,

        lt_bapiaddr1    TYPE TABLE OF bapiaddr1,
        lwa_bapiaddr1   TYPE  bapiaddr1,
        lwa_gladrc      TYPE /agri/s_gladrc,

*        lt_gliloa       TYPE TABLE OF /agri/s_gliloa,
*        lwa_gliloa      TYPE /agri/s_gliloa,

        lt_gliflotx2    TYPE TABLE OF /agri/glflotx,
        lwa_gliflotx    TYPE /agri/s_gliflotx,
        lt_gliflotx     TYPE  /agri/t_gliflotx,

        lwa_glflown     TYPE /agri/s_glflown,
        lt_glflown      TYPE /agri/t_glflown,
        ls_glfl_doc     TYPE /agri/s_glfl_doc,

        lwa_glihpa      TYPE /agri/s_glihpa,
        lt_glihpa       TYPE /agri/t_glihpa,
        lt_glflatg      TYPE /agri/t_glflatg,
        lwa_glflatg     TYPE /agri/s_glflatg,
        lt_glflatv      TYPE /agri/t_glflatv,
        lwa_glflatv     TYPE /agri/s_glflatv,
*        lt_glihpa2 TYPE /agri/t_glihpa,

*        lt_glfl_doc TYPE TABLE OF /agri/s_glfl_doc,
        lwa_message     TYPE /agri/s_gprolog,
        lt_messages     TYPE /agri/t_gprolog,
        lt_mess_collect TYPE /agri/t_gprolog,

        lt_tr01         TYPE TABLE OF /agri/s_tr_sheet1,
        lt_tr02         TYPE TABLE OF /agri/s_tr_sheet2,
        lt_tr03         TYPE TABLE OF /agri/s_tr_sheet3,
        lwa_tr03        TYPE /agri/s_tr_sheet3,
        lt_tr04         TYPE TABLE OF /agri/s_tr_sheet4,
        lt_tr05         TYPE TABLE OF /agri/s_tr_sheet5,
        lt_tr06         TYPE TABLE OF /agri/s_tr_sheet6,
        lt_tr07         TYPE TABLE OF /agri/s_tr_sheet7,
        lt_tr08         TYPE TABLE OF /agri/s_tr_sheet8,
        lt_tr09         TYPE TABLE OF /agri/s_tr_sheet9.


  FIELD-SYMBOLS: <lwa_tr01>   TYPE /agri/s_tr_sheet1,
                 <lwa_tr02>   TYPE /agri/s_tr_sheet2,
                 <lwa_tr03>   TYPE /agri/s_tr_sheet3,
                 <lwa_tr04>   TYPE /agri/s_tr_sheet4,
                 <lwa_tr05>   TYPE /agri/s_tr_sheet5,
                 <lwa_tr06>   TYPE /agri/s_tr_sheet6,
                 <lwa_iflotx> TYPE /agri/glflotx,
                 <lwa_tr07>   TYPE /agri/s_tr_sheet7,
                 <lwa_tr08>   TYPE /agri/s_tr_sheet8,
                 <lwa_tr09>   TYPE /agri/s_tr_sheet9,
                 <lv_value>   TYPE any.



  CONSTANTS: BEGIN OF c_structures,
               tr_01   TYPE tabname    VALUE '/AGRI/S_TR_SHEET1',
               tr_02   TYPE tabname    VALUE '/AGRI/S_TR_SHEET2',
               tr_03   TYPE tabname    VALUE '/AGRI/S_TR_SHEET3',
               tr_04   TYPE tabname    VALUE '/AGRI/S_TR_SHEET4',
               tr_05   TYPE tabname    VALUE '/AGRI/S_TR_SHEET5',
               tr_06   TYPE tabname    VALUE '/AGRI/S_TR_SHEET6',
               tr_07   TYPE tabname    VALUE '/AGRI/S_TR_SHEET7',
               tr_08   TYPE tabname    VALUE '/AGRI/S_TR_SHEET8',
               tr_09   TYPE tabname    VALUE '/AGRI/S_TR_SHEET9',
****Terrain Class Type Issue Fix
*              klart   TYPE klassenart VALUE '003',
               klart   TYPE klassenart VALUE 'X91',
****
               tr_in   TYPE updkz_d    VALUE 'I',
               tr_up   TYPE updkz_d    VALUE 'U',
               success LIKE sy-msgty   VALUE 'S',
             END OF c_structures.

  lt_table[] = it_table[].

  DELETE lt_table WHERE row BETWEEN 1 AND 15.  "#EC CI_STDSEQ
  DELETE lt_table WHERE column = 1.  "#EC CI_STDSEQ


  "Sheet 1
  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-tr_01
      i_sheet     = 1
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  LOOP AT lt_table INTO lwa_data WHERE sheet = 01.   "#EC CI_STDSEQ
*    AT NEW row.
*      APPEND INITIAL LINE TO lt_tr01 ASSIGNING <lwa_tr01>.
*    ENDAT.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_tr01 ASSIGNING <lwa_tr01>.
      lv_row = lwa_data-row.
    ENDIF.
    LOOP AT lt_dd03l INTO lwa_dd03l
                      WHERE position = lwa_data-column.   "#EC CI_STDSEQ
      ASSIGN COMPONENT lwa_data-fieldname
                  OF STRUCTURE <lwa_tr01> TO <lv_value>.

      IF lwa_data-fieldname = 'PSPNR' .
*        CALL FUNCTION '/AGRI/G_CONV_EXIT_KONPD_INPUT' "#EC ARG_OK
*          EXPORTING
*            i_input     = lwa_data-value
*          IMPORTING
*            e_output    = lwa_data-value
**           PROJWA    =
*          EXCEPTIONS
*            not_found = 1
*            OTHERS    = 2.
        CALL FUNCTION '/AGRI/G_CONV_EXIT_KONPD_INPUT'
          EXPORTING
            i_input            = lwa_data-value
*           I_NO_MESSAGE       =
          IMPORTING
            e_output           = lwa_data-value.
*           ES_RETURN          =
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
          APPEND lwa_message TO lt_mess_collect.
          CLEAR: lwa_message.
          EXIT.
        ENDIF.
      ENDIF.
      <lv_value> = lwa_data-value.

    ENDLOOP.
  ENDLOOP.
  CLEAR lv_row.

  UNASSIGN: <lwa_tr01>, <lv_value>.

  "Sheet 2
  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-tr_02
      i_sheet     = 2
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  LOOP AT lt_table INTO lwa_data WHERE sheet = 02.  "#EC CI_STDSEQ
*    AT NEW row.
*      APPEND INITIAL LINE TO lt_tr02 ASSIGNING <lwa_tr02>.
*    ENDAT.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_tr02 ASSIGNING <lwa_tr02>.
      lv_row = lwa_data-row.
    ENDIF.
    LOOP AT lt_dd03l INTO lwa_dd03l
                      WHERE position = lwa_data-column.  "#EC CI_STDSEQ
      ASSIGN COMPONENT lwa_data-fieldname
                  OF STRUCTURE <lwa_tr02> TO <lv_value>.
      <lv_value> = lwa_data-value.

    ENDLOOP.
  ENDLOOP.
  CLEAR lv_row.
  UNASSIGN: <lwa_tr02>, <lv_value>.

  "Sheet 3
  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-tr_03
      i_sheet     = 3
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  LOOP AT lt_table INTO lwa_data WHERE sheet = 03.  "#EC CI_STDSEQ
*    AT NEW row.
*      APPEND INITIAL LINE TO lt_tr03 ASSIGNING <lwa_tr03>.
*    ENDAT.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_tr03 ASSIGNING <lwa_tr03>.
      lv_row = lwa_data-row.
    ENDIF.
    LOOP AT lt_dd03l INTO lwa_dd03l
                      WHERE position = lwa_data-column.  "#EC CI_STDSEQ
      ASSIGN COMPONENT lwa_data-fieldname
                  OF STRUCTURE <lwa_tr03> TO <lv_value>.
      <lv_value> = lwa_data-value.

    ENDLOOP.
  ENDLOOP.
  CLEAR lv_row.
  UNASSIGN: <lwa_tr03>, <lv_value>.

  "Sheet 4
  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-tr_04
      i_sheet     = 4
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  LOOP AT lt_table INTO lwa_data WHERE sheet = 04.   "#EC CI_STDSEQ
*    AT NEW row.
*      APPEND INITIAL LINE TO lt_tr04 ASSIGNING <lwa_tr04>.
*    ENDAT.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_tr04 ASSIGNING <lwa_tr04>.
      lv_row = lwa_data-row.
    ENDIF.
    LOOP AT lt_dd03l INTO lwa_dd03l
                      WHERE position = lwa_data-column.   "#EC CI_STDSEQ
      ASSIGN COMPONENT lwa_data-fieldname
                  OF STRUCTURE <lwa_tr04> TO <lv_value>.
      <lv_value> = lwa_data-value.

    ENDLOOP.
  ENDLOOP.
  CLEAR lv_row.
  UNASSIGN: <lwa_tr04>, <lv_value>.

  "Sheet 5
  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-tr_05
      i_sheet     = 5
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  LOOP AT lt_table INTO lwa_data WHERE sheet = 05.   "#EC CI_STDSEQ
*    AT NEW row.
*      APPEND INITIAL LINE TO lt_tr05 ASSIGNING <lwa_tr05>.
*    ENDAT.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_tr05 ASSIGNING <lwa_tr05>.
      lv_row = lwa_data-row.
    ENDIF.
    LOOP AT lt_dd03l INTO lwa_dd03l
                      WHERE position = lwa_data-column.   "#EC CI_STDSEQ
      ASSIGN COMPONENT lwa_data-fieldname
                  OF STRUCTURE <lwa_tr05> TO <lv_value>.
      <lv_value> = lwa_data-value.

    ENDLOOP.
  ENDLOOP.
  CLEAR lv_row.
  UNASSIGN: <lwa_tr05>, <lv_value>.

  "Sheet 6
  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-tr_06
      i_sheet     = 6
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  LOOP AT lt_table INTO lwa_data WHERE sheet = 06.  "#EC CI_STDSEQ
*    AT NEW row.
*      APPEND INITIAL LINE TO lt_tr06 ASSIGNING <lwa_tr06>.
*    ENDAT.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_tr06 ASSIGNING <lwa_tr06>.
      lv_row = lwa_data-row.
    ENDIF.
    LOOP AT lt_dd03l INTO lwa_dd03l
                      WHERE position = lwa_data-column.   "#EC CI_STDSEQ
      ASSIGN COMPONENT lwa_data-fieldname
                  OF STRUCTURE <lwa_tr06> TO <lv_value>.
      <lv_value> = lwa_data-value.

    ENDLOOP.
  ENDLOOP.
  CLEAR lv_row.
  UNASSIGN: <lwa_tr06>, <lv_value>.

  "Sheet 7
  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-tr_07
      i_sheet     = 7
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  LOOP AT lt_table INTO lwa_data WHERE sheet = 07.    "#EC CI_STDSEQ
*    AT NEW row.
*      APPEND INITIAL LINE TO lt_tr07 ASSIGNING <lwa_tr07>.
*    ENDAT.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_tr07 ASSIGNING <lwa_tr07>.
      lv_row = lwa_data-row.
    ENDIF.
    LOOP AT lt_dd03l INTO lwa_dd03l
                      WHERE position = lwa_data-column.  "#EC CI_STDSEQ
      ASSIGN COMPONENT lwa_data-fieldname
                  OF STRUCTURE <lwa_tr07> TO <lv_value>.
      <lv_value> = lwa_data-value.

    ENDLOOP.
  ENDLOOP.
  CLEAR lv_row.
  UNASSIGN: <lwa_tr07>, <lv_value>.

  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-tr_08
      i_sheet     = 8
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  LOOP AT lt_table INTO lwa_data WHERE sheet = 08.  "#EC CI_STDSEQ
*    AT NEW row.
*      APPEND INITIAL LINE TO lt_tr08 ASSIGNING <lwa_tr08>.
*    ENDAT.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_tr08 ASSIGNING <lwa_tr08>.
      lv_row = lwa_data-row.
    ENDIF.
    LOOP AT lt_dd03l INTO lwa_dd03l
                      WHERE position = lwa_data-column.  "#EC CI_STDSEQ


      ASSIGN COMPONENT lwa_data-fieldname
                  OF STRUCTURE <lwa_tr08> TO <lv_value>.
      <lv_value> = lwa_data-value.

    ENDLOOP.
  ENDLOOP.
  CLEAR lv_row.
  UNASSIGN: <lwa_tr08>, <lv_value>.

  IF lt_tr08 IS NOT INITIAL.
    SELECT * FROM klah "#EC CI_ALL_FIELDS_NEEDED
      INTO TABLE lt_klah
      FOR ALL ENTRIES IN lt_tr08
      WHERE class EQ lt_tr08-class.
  ENDIF.

  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-tr_09
      i_sheet     = 9
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  LOOP AT lt_table INTO lwa_data WHERE sheet = 09.    "#EC CI_STDSEQ
*    AT NEW row.
*      APPEND INITIAL LINE TO lt_tr09 ASSIGNING <lwa_tr09>.
*    ENDAT.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_tr09 ASSIGNING <lwa_tr09>.
      lv_row = lwa_data-row.
    ENDIF.
    LOOP AT lt_dd03l INTO lwa_dd03l
                      WHERE position = lwa_data-column.  "#EC CI_STDSEQ
      ASSIGN COMPONENT lwa_data-fieldname
                  OF STRUCTURE <lwa_tr09> TO <lv_value>.
      IF lwa_data-fieldname EQ 'ATINN'.
*---Replace Unreleased Interfaces
*  call function 'CONVERSION_EXIT_ATINN_INPUT'
*    exporting
*      input  = lwa_data-value
*    importing
*      output = lwa_data-value.
        CALL FUNCTION '/AGRI/G_CONV_EXIT_ATINN_INPUT'
          EXPORTING
            i_input  = lwa_data-value
          IMPORTING
            o_output = lwa_data-value.
*---
      ENDIF.
      <lv_value> = lwa_data-value.
    ENDLOOP.
  ENDLOOP.
  CLEAR lv_row.
  UNASSIGN: <lwa_tr09>, <lv_value>.


  LOOP AT lt_tr01 ASSIGNING <lwa_tr01>.

    "Terrain Header
    MOVE-CORRESPONDING <lwa_tr01> TO lwa_glflot.
    lwa_glflot-updkz = c_structures-tr_in.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        input          = lwa_glflot-msehi
*       LANGUAGE       =
      IMPORTING
        output         = lwa_glflot-msehi
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
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
      APPEND LINES OF lt_messages TO lt_mess_collect.
      CLEAR: lwa_message, lt_messages.
    ENDIF.
*---Replace Unreleased Interface
*    CALL FUNCTION 'CONVERSION_EXIT_PARVW_INPUT'
*      EXPORTING
*        input  = lwa_glflot-owrol
*      IMPORTING
*        output = lwa_glflot-owrol.
    CALL FUNCTION '/AGRI/G_CONV_EXIT_PARVW_INPUT'
      EXPORTING
        i_parvw = lwa_glflot-owrol
      IMPORTING
        o_parvw = lwa_glflot-owrol.
*---


    "Terrain Table
*    READ TABLE lt_tr02 ASSIGNING <lwa_tr02> WITH KEY tplnr = <lwa_tr01>-tplnr_fl.
*    IF sy-subrc EQ 0.
*      MOVE-CORRESPONDING <lwa_tr02> TO lwa_gliflot.
*      lwa_gliflot-updkz = c_structures-tr_in.
*    ENDIF.

    "BAPI Reference Structure for Addresses
    READ TABLE lt_tr03 ASSIGNING <lwa_tr03> WITH KEY tplnr = <lwa_tr01>-tplnr_fl.  "#EC CI_STDSEQ
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING <lwa_tr03> TO lwa_bapiaddr1.
    ENDIF.

    "PM Object Location and Account Assignment
*    READ TABLE lt_tr04 ASSIGNING <lwa_tr04> WITH KEY tplnr = <lwa_tr01>-tplnr_fl.
*    IF sy-subrc EQ 0.
*      MOVE-CORRESPONDING <lwa_tr04> TO lwa_gliloa.
*      lwa_glflot-eqfnr = lwa_gliloa-eqfnr.
*      lwa_gliloa-updkz = c_structures-tr_in.
*    ENDIF.

    "Terrain: Short Texts
    LOOP AT lt_tr05 ASSIGNING <lwa_tr05> WHERE tplnr = <lwa_tr01>-tplnr_fl.  "#EC CI_STDSEQ
      MOVE-CORRESPONDING <lwa_tr05> TO lwa_gliflotx.
      lwa_gliflotx-updkz = c_structures-tr_in.
      APPEND lwa_gliflotx TO lt_gliflotx.
      CLEAR lwa_gliflotx.
    ENDLOOP.

    "Plant Maintenance: Partners
    LOOP AT lt_tr06 ASSIGNING <lwa_tr06> WHERE tplnr = <lwa_tr01>-tplnr_fl.  "#EC CI_STDSEQ
      MOVE-CORRESPONDING <lwa_tr06> TO lwa_glihpa.
*---Replace Unreleased Interface
*    CALL FUNCTION 'CONVERSION_EXIT_PARVW_INPUT'
*      EXPORTING
*        input  = lwa_glihpa-parvw
*      IMPORTING
*        output = lwa_glihpa-parvw.
    CALL FUNCTION '/AGRI/G_CONV_EXIT_PARVW_INPUT'
      EXPORTING
        i_parvw = lwa_glihpa-parvw
      IMPORTING
        o_parvw = lwa_glihpa-parvw.
*---

      CHECK lwa_glihpa-parvw IS NOT INITIAL.

      lwa_glihpa-updkz = c_structures-tr_in.
      APPEND lwa_glihpa TO lt_glihpa.
      CLEAR lwa_glihpa.
    ENDLOOP.

    "Terrain Owners
    LOOP AT lt_tr07 ASSIGNING <lwa_tr07> WHERE tplnr_fl = <lwa_tr01>-tplnr_fl.  "#EC CI_STDSEQ
      MOVE-CORRESPONDING <lwa_tr07> TO lwa_glflown.
*---Replace Unreleased Interface
*    CALL FUNCTION 'CONVERSION_EXIT_PARVW_INPUT'
*      EXPORTING
*        input  = lwa_glflot-owrol
*      IMPORTING
*        output = lwa_glflot-owrol.
    CALL FUNCTION '/AGRI/G_CONV_EXIT_PARVW_INPUT'
      EXPORTING
        i_parvw = lwa_glflown-owrol
      IMPORTING
        o_parvw = lwa_glflown-owrol.
*---
      CHECK lwa_glflown-owrol IS NOT INITIAL.

      lwa_glflown-updkz = c_structures-tr_in.
      APPEND lwa_glflown TO lt_glflown.
      CLEAR lwa_glflown.
    ENDLOOP.

    LOOP AT lt_tr08 ASSIGNING <lwa_tr08> WHERE tplnr_fl = <lwa_tr01>-tplnr_fl.  "#EC CI_STDSEQ
      READ TABLE lt_klah INTO lwa_klah
                WITH KEY klart = c_structures-klart
                         class = <lwa_tr08>-class.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING <lwa_tr08> TO lwa_glflatg.
        MOVE-CORRESPONDING lwa_klah   TO lwa_glflatg.
        lwa_glflatg-updkz = c_structures-tr_in.
        APPEND lwa_glflatg TO lt_glflatg.
      ENDIF.
      CLEAR lwa_glflatg.
    ENDLOOP.

    LOOP AT lt_tr09 ASSIGNING <lwa_tr09> WHERE tplnr_fl = <lwa_tr01>-tplnr_fl. "#EC CI_STDSEQ
      READ TABLE lt_klah INTO lwa_klah
                WITH KEY klart = c_structures-klart
                         class = <lwa_tr09>-class.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING <lwa_tr09> TO lwa_glflatv.
        lwa_glflatv-clint = lwa_klah-clint.
        lwa_glflatv-updkz = c_structures-tr_in.
        APPEND lwa_glflatv TO lt_glflatv.
      ENDIF.
      CLEAR lwa_glflatv.
    ENDLOOP.

*ENHANCEMENT-POINT TERRAINS_CREATE SPOTS /AGRI/ES_CL_UPLOAD_MASTER_DATA .

    CALL FUNCTION '/AGRI/GLFL_CREATE'
      EXPORTING
        i_messages_display      = ' '
        i_save_messages         = ' '
        i_commit_work           = 'X'
        i_check_address         = 'X'
        i_dialog                = ' '
        is_flhdr                = lwa_glflot
*        is_iflot                = lwa_gliflot
        is_adrc                 = lwa_bapiaddr1
*        is_iloa                 = lwa_gliloa
        it_iflotx               = lt_gliflotx
        it_ihpa                 = lt_glihpa
        it_flown                = lt_glflown
      IMPORTING
        es_glfl_doc             = ls_glfl_doc
        et_messages             = lt_messages
**   E_LOG_NUMBER                  =
      EXCEPTIONS
        no_documents_to_process = 1
        no_authorization        = 2
        creation_failed         = 3
        OTHERS                  = 4.
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

    APPEND LINES OF lt_messages TO lt_mess_collect.
    CLEAR lt_messages.

    IF sy-msgty EQ c_structures-success
      AND lt_glflatg[] IS NOT INITIAL.
      MOVE-CORRESPONDING: ls_glfl_doc-x-flhdr  TO lwa_glflot.
*      MOVE-CORRESPONDING: ls_glfl_doc-x-iflot TO lwa_gliflot.
*      MOVE-CORRESPONDING: ls_glfl_doc-x-iloa  TO lwa_gliloa.
      lwa_glflot-updkz    = c_structures-tr_up.
      CALL FUNCTION '/AGRI/GLFL_CHANGE'
        EXPORTING
*         I_MESSAGES_DISPLAY      = ' '
*         I_SAVE_MESSAGES         = ' '
          is_flhdr                = lwa_glflot
*          is_iflot                = lwa_gliflot
*          is_iloa                 = lwa_gliloa
          it_flatg                = lt_glflatg
          it_flatv                = lt_glflatv
        IMPORTING
*         es_glfl_doc             = ls_glfl_doc
          et_messages             = lt_messages
*         E_LOG_NUMBER            =
*       CHANGING
*         CS_GLFL_DOC             =
        EXCEPTIONS
          no_documents_to_process = 1
          no_authorization        = 2
          change_failed           = 3
          terrain_locked          = 4
          OTHERS                  = 5.
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
      APPEND LINES OF lt_messages TO lt_mess_collect.
      CLEAR lt_messages.
*      READ TABLE lt_messages WITH KEY msgty = c_structures-success
*                 TRANSPORTING NO FIELDS.
*      IF sy-subrc NE 0.
*        APPEND LINES OF lt_messages TO lt_mess_collect.
*      ENDIF.

    ENDIF.

    CLEAR: lwa_glflot,
*           lwa_gliflot,
           lwa_bapiaddr1,
           lwa_bapiaddr1,
           lt_gliflotx[],
           lt_glihpa[],
           lt_glflown[],
           lt_glflatg[],
           lt_glflatv[].

  ENDLOOP.

  et_messages = lt_mess_collect.

ENDMETHOD.


METHOD update_technical_indexes.

  DATA: lt_table        TYPE /agri/t_excel_sheet,
        lt_dd03l        TYPE thrpad_erd_dd03l,
        lt_messages     TYPE /agri/t_gprolog,
        lt_mess_collect TYPE /agri/t_gprolog,
        lt_mess_change  LIKE lt_mess_collect,
        lt_zfmaitm_db   TYPE STANDARD TABLE OF zfmaitm INITIAL SIZE 0,
        lt_fmacvlcl_db  TYPE STANDARD TABLE OF zfmacvlcl INITIAL SIZE 0,
        lt_fmachdr_db   TYPE STANDARD TABLE OF zfmachdr INITIAL SIZE 0,
        lt_tech_index   TYPE TABLE OF zabs_str_technical_indexes INITIAL SIZE 0,
        lwa_data        TYPE /agri/s_excel_sheet,
        lwa_fmacvlcl    TYPE zfmacvlcl,
        lwa_dd03l       TYPE dd03l,
        lwa_message     TYPE /agri/s_gprolog,
        ref_var         TYPE REF TO cx_sy_arithmetic_overflow,
        lv_float        TYPE f,
        lv_output(12)   TYPE p DECIMALS 3,
        lv_text         TYPE string,
        lv_msgv1        TYPE sy-msgv1,
        lv_row          TYPE /agri/s_excel_sheet-row.

  FIELD-SYMBOLS: <lwa_tech_index> TYPE zabs_str_technical_indexes,
                 <lv_value>       TYPE any.

  CONSTANTS: BEGIN OF c_structures,
               index_01 TYPE tabname  VALUE 'ZABS_STR_TECHNICAL_INDEXES',
               index_02 TYPE tabname  VALUE '/AGRI/S_AT_SHEET2',
               index_03 TYPE tabname  VALUE '/AGRI/S_AT_SHEET3',
               index_04 TYPE tabname  VALUE '/AGRI/S_AT_SHEET4',
               index_05 TYPE tabname  VALUE '/AGRI/S_AT_SHEET5',
               index_in TYPE updkz_d  VALUE 'I',
               index_up TYPE updkz_d  VALUE 'U',
               success  TYPE sy-msgty VALUE 'S',
             END OF c_structures.

  CONSTANTS: BEGIN OF c_measurement_level,
               terrain      TYPE /agri/glaslvl VALUE 'T',
               crop_seasons TYPE /agri/glaslvl VALUE 'A',
               harvest      TYPE /agri/glaslvl VALUE 'H',
               irrigation   TYPE /agri/glaslvl VALUE 'I',
             END OF c_measurement_level.

  CONSTANTS: BEGIN OF c_unit_of_measurement,
               unit    TYPE meins VALUE 'UN',
               hectare TYPE meins VALUE 'HAR',
             END OF c_unit_of_measurement.

  CONSTANTS: BEGIN OF c_crop_season_status,
               active   TYPE /agri/glastat VALUE 'A',
               inactive TYPE /agri/glastat VALUE 'I',
               closed   TYPE /agri/glastat VALUE 'C',
             END OF c_crop_season_status.

  CONSTANTS: BEGIN OF c_grupo_medicao,
               volume_total TYPE /agri/glmpgrp VALUE 'ZFAZ_VOL_TOTAL_PL',
               qtd_plantas  TYPE /agri/glmpgrp VALUE 'FAZ-INV-PLANTAS',
               volume_copa  TYPE /agri/glmpgrp VALUE 'FAZ-CUBICAGEM',
             END OF c_grupo_medicao.

  CONSTANTS: BEGIN OF c_document_type,
               terrain TYPE /agri/glmdtyp VALUE 'ZTYP',
               plant   TYPE /agri/glmdtyp VALUE 'ZARV',
             END OF c_document_type.

  DEFINE add_first_line.
*-- Verificar Coluna &1 Linha &2.
    CLEAR lwa_message.
    lwa_message-msgid = 'ZABS_MSGCLS'.
    lwa_message-msgno = '143'.
    lwa_message-msgty = 'I'.
    lwa_message-msgv1 = &1.
    lwa_message-msgv2 = &2.
    APPEND lwa_message TO lt_messages.
  END-OF-DEFINITION.

  lt_table[] = it_table[].

  DELETE lt_table WHERE row = 1.
  DELETE lt_table WHERE sheet <> 1.

  "Sheet 1
  CALL METHOD structure_build(
    EXPORTING
      i_structure = c_structures-index_01
      i_sheet     = 1
    IMPORTING
      et_dd03l    = lt_dd03l
    CHANGING
      ct_table    = lt_table ).

  SORT lt_dd03l BY position.

  LOOP AT lt_table INTO lwa_data WHERE sheet = 01.
    IF lv_row NE lwa_data-row.
      APPEND INITIAL LINE TO lt_tech_index ASSIGNING <lwa_tech_index>.
      lv_row = lwa_data-row.
    ENDIF.

    READ TABLE lt_dd03l TRANSPORTING NO FIELDS
      WITH KEY position = lwa_data-column.
    IF sy-subrc EQ 0.
      LOOP AT lt_dd03l INTO lwa_dd03l FROM sy-tabix.
        IF lwa_dd03l-position NE lwa_data-column.
          EXIT.
        ENDIF.
        ASSIGN COMPONENT lwa_data-fieldname
          OF STRUCTURE <lwa_tech_index> TO <lv_value>.
        IF sy-subrc EQ 0.
          <lv_value> = lwa_data-value.
          IF lwa_dd03l-domname EQ 'ZFMACNUM'.
            UNPACK <lv_value> TO <lv_value>.
          ELSEIF lwa_dd03l-domname EQ '/AGRI/GLTPLNR_FL'.
            CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
              EXPORTING
                input      = <lv_value>
              IMPORTING
                output     = <lv_value>
              EXCEPTIONS
                not_found  = 1
                not_active = 2
                OTHERS     = 3.
          ELSEIF lwa_dd03l-domname EQ 'MEINS'.
            CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
              EXPORTING
                input          = <lv_value>
                language       = sy-langu
              IMPORTING
                output         = <lv_value>
              EXCEPTIONS
                unit_not_found = 1
                OTHERS         = 2.
          ELSEIF lwa_dd03l-domname EQ 'MATNR'.
            CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
              EXPORTING
                input        = <lv_value>
              IMPORTING
                output       = <lv_value>
              EXCEPTIONS
                length_error = 1
                OTHERS       = 2.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    <lwa_tech_index>-excel_row = lwa_data-row.
    <lwa_tech_index>-excel_column = lwa_data-column.
  ENDLOOP.
  CLEAR lv_row.

  SORT lt_tech_index BY acnum matnr tplnr_fl.
  DELETE ADJACENT DUPLICATES FROM lt_tech_index COMPARING acnum matnr tplnr_fl.

  IF lt_tech_index[] IS INITIAL.
*-- Arquivo Excel não contém dados.
    CLEAR lwa_message.
    lwa_message-msgid = 'ZFMFP'.
    lwa_message-msgno = '122'.
    lwa_message-msgty = 'E'.
    APPEND lwa_message TO lt_messages.
    RETURN.
  ENDIF.

  SELECT *
    FROM zfmachdr
    INTO TABLE @lt_fmachdr_db
    FOR ALL ENTRIES IN @lt_tech_index
   WHERE acnum = @lt_tech_index-acnum.

  SORT lt_fmachdr_db BY acnum ajahr.

  SELECT m~matnr, m~mtart,
         m~meins, t~maktx
    FROM mara AS m
    INNER JOIN makt AS t
    ON m~matnr EQ t~matnr
    INTO TABLE @DATA(lt_mara)
    FOR ALL ENTRIES IN @lt_tech_index
   WHERE m~matnr = @lt_tech_index-matnr
     AND t~spras = @sy-langu.

  SORT lt_mara BY matnr.

  SELECT *
    FROM /agri/glflot
    INTO TABLE @DATA(lt_glflot)
    FOR ALL ENTRIES IN @lt_tech_index
   WHERE tplnr_fl = @lt_tech_index-tplnr_fl.

  SORT lt_glflot BY tplnr_fl.

  SELECT msehi
    FROM t006
    INTO TABLE @DATA(lt_t006)
    FOR ALL ENTRIES IN @lt_tech_index
   WHERE msehi = @lt_tech_index-unren.

  SORT lt_t006 BY msehi.

  DATA: lv_acnum    TYPE zfmacnum,
        lv_tplnr_fl TYPE /agri/gltplnr_fl.

  LOOP AT lt_tech_index ASSIGNING <lwa_tech_index>.
    READ TABLE lt_fmachdr_db TRANSPORTING NO FIELDS
      WITH KEY acnum = <lwa_tech_index>-acnum BINARY SEARCH.
    IF sy-subrc NE 0.
      IF <lwa_tech_index>-line_w_error EQ abap_false.
        <lwa_tech_index>-line_w_error = abap_true.
        add_first_line <lwa_tech_index>-excel_column <lwa_tech_index>-excel_row.
      ENDIF.
      lv_acnum = <lwa_tech_index>-acnum.
      SHIFT lv_acnum LEFT DELETING LEADING '0'.
*-- Área de Cultivo &1 inexistente.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZFMFP'.
      lwa_message-msgno = 141.
      lwa_message-msgty = 'E'.
      lwa_message-msgv1 = lv_acnum.
      APPEND lwa_message TO lt_messages.
    ENDIF.

    READ TABLE lt_mara TRANSPORTING NO FIELDS
      WITH KEY matnr = <lwa_tech_index>-matnr BINARY SEARCH.
    IF sy-subrc NE 0.
      IF <lwa_tech_index>-line_w_error EQ abap_false.
        <lwa_tech_index>-line_w_error = abap_true.
        add_first_line <lwa_tech_index>-excel_column <lwa_tech_index>-excel_row.
      ENDIF.
*-- Material Tarefa &1 inexistente.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZFMFP'.
      lwa_message-msgno = 142.
      lwa_message-msgty = 'E'.
      lwa_message-msgv1 = <lwa_tech_index>-matnr.
      APPEND lwa_message TO lt_messages.
    ENDIF.

    READ TABLE lt_glflot TRANSPORTING NO FIELDS
      WITH KEY tplnr_fl = <lwa_tech_index>-tplnr_fl BINARY SEARCH.
    IF sy-subrc NE 0.
      IF <lwa_tech_index>-line_w_error EQ abap_false.
        <lwa_tech_index>-line_w_error = abap_true.
        add_first_line <lwa_tech_index>-excel_column <lwa_tech_index>-excel_row.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
        EXPORTING
          input  = <lwa_tech_index>-tplnr_fl
        IMPORTING
          output = lv_tplnr_fl.

*-- Talhão &1 inexistente.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZFMFP'.
      lwa_message-msgno = 143.
      lwa_message-msgty = 'E'.
      lwa_message-msgv1 = lv_tplnr_fl.
      APPEND lwa_message TO lt_messages.
    ENDIF.

    READ TABLE lt_t006 TRANSPORTING NO FIELDS
      WITH KEY msehi = <lwa_tech_index>-unren BINARY SEARCH.
    IF sy-subrc NE 0.
      IF <lwa_tech_index>-line_w_error EQ abap_false.
        <lwa_tech_index>-line_w_error = abap_true.
        add_first_line <lwa_tech_index>-excel_column <lwa_tech_index>-excel_row.
      ENDIF.
*-- Unidade de Medida &1 inexistente.
      CLEAR lwa_message.
      lwa_message-msgid = 'ZFMFP'.
      lwa_message-msgno = 144.
      lwa_message-msgty = 'E'.
      lwa_message-msgv1 = <lwa_tech_index>-unren.
      APPEND lwa_message TO lt_messages.
    ENDIF.
  ENDLOOP.

  SORT lt_tech_index BY acnum matnr tplnr_fl.
  DATA(lt_cult_area) = lt_tech_index[].
  DELETE ADJACENT DUPLICATES FROM lt_cult_area COMPARING acnum.

  IF lt_tech_index[] IS NOT INITIAL.
    SELECT *
      FROM zfmaitm
      INTO TABLE @lt_zfmaitm_db
      FOR ALL ENTRIES IN @lt_tech_index
     WHERE acnum    EQ @lt_tech_index-acnum
       AND tplnr_fl EQ @lt_tech_index-tplnr_fl.

    IF sy-subrc EQ 0.
*-- BOC T_T.KONNO-04.08.21
*     SORT lt_zfmaitm_db BY acnum tplnr_fl.
      SORT lt_zfmaitm_db BY acnum tplnr_fl season.
*-- EOC T_T.KONNO-04.08.21

      SELECT h~mdocm, h~mdtyp, h~aslvl, h~tplnr_fl,
             h~contr, h~cmnum, h~muser, h~equnr,
             h~mpgrp, v~atzhl, v~atwrt, v~atflv,
             c~atinn, c~atnam, h~mdate, h~mtime
        INTO TABLE @DATA(lt_glmdhdr_join)
        FROM /agri/glmdhdr AS h
        INNER JOIN /agri/glmdatv AS v
        ON v~mdocm EQ h~mdocm
        INNER JOIN cabn AS c
        ON c~atinn EQ v~atinn
        FOR ALL ENTRIES IN @lt_zfmaitm_db
       WHERE h~mdtyp    EQ @c_document_type-terrain
         AND h~aslvl    EQ @c_measurement_level-terrain
         AND h~tplnr_fl EQ @lt_zfmaitm_db-tplnr_fl
         AND h~mpgrp    EQ @c_grupo_medicao-volume_total.

      SORT lt_glmdhdr_join BY tplnr_fl atnam.

      SELECT h~mdocm, h~mdtyp, h~aslvl, h~tplnr_fl,
             h~contr, h~cmnum, h~muser, h~equnr,
             h~mpgrp, v~atzhl, v~atwrt, v~atflv,
             c~atinn, c~atnam, h~mdate, h~mtime
        INTO TABLE @DATA(lt_glmdhdr_plants)
        FROM /agri/glmdhdr AS h
        INNER JOIN /agri/glmdatv AS v
        ON v~mdocm EQ h~mdocm
        INNER JOIN cabn AS c
        ON c~atinn EQ v~atinn
        FOR ALL ENTRIES IN @lt_zfmaitm_db
       WHERE h~mdtyp    EQ @c_document_type-plant
         AND h~aslvl    EQ @c_measurement_level-crop_seasons
         AND h~tplnr_fl EQ @lt_zfmaitm_db-tplnr_fl
         AND h~mpgrp    EQ @c_grupo_medicao-qtd_plantas.

      SORT lt_glmdhdr_plants BY tplnr_fl atnam.

      SELECT h~mdocm, h~mdtyp, h~aslvl, h~tplnr_fl,
             h~contr, h~cmnum, h~muser, h~equnr,
             h~mpgrp, v~atzhl, v~atwrt, v~atflv,
             c~atinn, c~atnam, h~mdate, h~mtime
        INTO TABLE @DATA(lt_glmdhdr_volume)
        FROM /agri/glmdhdr AS h
        INNER JOIN /agri/glmdatv AS v
        ON v~mdocm EQ h~mdocm
        INNER JOIN cabn AS c
        ON c~atinn EQ v~atinn
        FOR ALL ENTRIES IN @lt_zfmaitm_db
       WHERE h~mdtyp    EQ @c_document_type-terrain
         AND h~aslvl    EQ @c_measurement_level-terrain
         AND h~tplnr_fl EQ @lt_zfmaitm_db-tplnr_fl
         AND h~mpgrp    EQ @c_grupo_medicao-volume_copa.

      SORT lt_glmdhdr_volume BY tplnr_fl atnam.
    ENDIF.

    SELECT *
      FROM zfmacvlcl
      INTO TABLE @lt_fmacvlcl_db
      FOR ALL ENTRIES IN @lt_tech_index
     WHERE acnum    EQ @lt_tech_index-acnum
       AND matnr    EQ @lt_tech_index-matnr
       AND tplnr_fl EQ @lt_tech_index-tplnr_fl.

    SORT lt_fmacvlcl_db BY acnum matnr tplnr_fl.
  ENDIF.

  LOOP AT lt_cult_area INTO DATA(lwa_cult_area).
    DATA(lv_locked) = abap_false.
    READ TABLE lt_fmachdr_db ASSIGNING FIELD-SYMBOL(<lwa_fmachdr_db>)
      WITH KEY acnum = lwa_cult_area-acnum BINARY SEARCH.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'ENQUEUE_EZ_FMAC'
        EXPORTING
          mandt          = sy-mandt
          acnum          = <lwa_fmachdr_db>-acnum
          ajahr          = <lwa_fmachdr_db>-ajahr
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      IF sy-subrc EQ 0.
        lv_locked = abap_true.
*-- Área de Cultivo &1 atualizada com sucesso!
        CLEAR lwa_message.
        lwa_message-msgid = 'ZFMFP'.
        lwa_message-msgno = 145.
        lwa_message-msgty = 'S'.
        lwa_message-msgv1 = lwa_cult_area-acnum.
        APPEND lwa_message TO lt_messages.
        <lwa_fmachdr_db>-aenam = sy-uname.
        <lwa_fmachdr_db>-aedat = sy-datum.
        <lwa_fmachdr_db>-aezet = sy-uzeit.
      ELSE.
        lv_msgv1 = sy-msgv1.
        IF <lwa_tech_index>-line_w_error EQ abap_false.
          <lwa_tech_index>-line_w_error = abap_true.
          add_first_line <lwa_tech_index>-excel_column <lwa_tech_index>-excel_row.
        ENDIF.
*-- Document &1 is locked by User &2.
        CLEAR lwa_message.
        lwa_message-msgid = '/AGRI/GLAC'.
        lwa_message-msgno = 005.
        lwa_message-msgty = 'E'.
        lwa_message-msgv1 = <lwa_fmachdr_db>-acnum.
        lwa_message-msgv2 = lv_msgv1.
        APPEND lwa_message TO lt_messages.
      ENDIF.
    ENDIF.

    IF lv_locked EQ abap_false.
      CONTINUE.
    ENDIF.

    CLEAR lwa_fmacvlcl.
    READ TABLE lt_tech_index INTO DATA(lwa_tech_index)
      WITH KEY acnum = lwa_cult_area-acnum BINARY SEARCH.

    WHILE sy-subrc EQ 0.
      DATA(lv_tabix) = sy-tabix + 1.

      READ TABLE lt_fmacvlcl_db ASSIGNING FIELD-SYMBOL(<lwa_fmacvlcl_db>)
        WITH KEY acnum    = lwa_tech_index-acnum
                 matnr    = lwa_tech_index-matnr
                 tplnr_fl = lwa_tech_index-tplnr_fl BINARY SEARCH.
      IF sy-subrc EQ 0.
*-- Índice Técnico (ACIDT)
        <lwa_fmacvlcl_db>-acidt = lwa_tech_index-acidt.
*-- Capacidade da Bomba (ACDIS)
        <lwa_fmacvlcl_db>-acdis = lwa_tech_index-acdis.
*-- Rendimento (ACREN)
        <lwa_fmacvlcl_db>-acren = lwa_tech_index-acren.
*-- Unidade de Rendimento (UNREN)
        <lwa_fmacvlcl_db>-unren = lwa_tech_index-unren.
*-- % Execução (ACPEX)
        <lwa_fmacvlcl_db>-acpex = lwa_tech_index-acpex.

        CLEAR lv_text.
*-- BOC T_T.KONNO-04.08.21
*        READ TABLE lt_zfmaitm_db ASSIGNING FIELD-SYMBOL(<lwa_zfmaitm_db>)
*          WITH KEY acnum    = lwa_tech_index-acnum
*                   tplnr_fl = lwa_tech_index-tplnr_fl BINARY SEARCH.
        TRANSLATE lwa_tech_index-matnr TO UPPER CASE.

        CASE lwa_tech_index-matnr(4).
          WHEN 'TFOR'
            OR 'TIMP'.
            READ TABLE lt_zfmaitm_db ASSIGNING FIELD-SYMBOL(<lwa_zfmaitm_db>)
                      WITH KEY acnum    = lwa_tech_index-acnum
                               tplnr_fl = lwa_tech_index-tplnr_fl
                               season   = 'SAFRA PLAN' BINARY SEARCH.
          WHEN 'TMAN'
            OR 'TCOL'.
            READ TABLE lt_zfmaitm_db ASSIGNING <lwa_zfmaitm_db>
                      WITH KEY acnum    = lwa_tech_index-acnum
                               tplnr_fl = lwa_tech_index-tplnr_fl
                               season   = 'SAFRA PROD' BINARY SEARCH.
          WHEN OTHERS.
            READ TABLE lt_zfmaitm_db ASSIGNING <lwa_zfmaitm_db>
              WITH KEY acnum    = lwa_tech_index-acnum
                       tplnr_fl = lwa_tech_index-tplnr_fl BINARY SEARCH.
        ENDCASE.
*-- EOC T_T.KONNO-04.08.21
        IF sy-subrc EQ 0.
          READ TABLE lt_glmdhdr_plants INTO DATA(lwa_plants)
            WITH KEY tplnr_fl = <lwa_zfmaitm_db>-tplnr_fl
                     atnam    = 'INV-QTDADE-ARV' BINARY SEARCH.
          IF sy-subrc EQ 0.
            <lwa_zfmaitm_db>-adqpl = lwa_plants-atflv.
            <lwa_zfmaitm_db>-unqpl = 'PÉS'.

            CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
              EXPORTING
                input          = <lwa_zfmaitm_db>-unqpl
                language       = 'P'
              IMPORTING
                output         = <lwa_zfmaitm_db>-unqpl
              EXCEPTIONS
                unit_not_found = 1
                OTHERS         = 2.
          ELSE.
            CLEAR lwa_plants.
          ENDIF.

          READ TABLE lt_glmdhdr_volume INTO DATA(lwa_volume)
            WITH KEY tplnr_fl = <lwa_zfmaitm_db>-tplnr_fl
                     atnam    = 'FAZ_ALTURA_COPA' BINARY SEARCH.
          IF sy-subrc EQ 0.
            MOVE lwa_volume-atflv TO lv_float.
            DATA(lv_altura) = lv_float.
          ENDIF.

          READ TABLE lt_glmdhdr_volume INTO lwa_volume
            WITH KEY tplnr_fl = <lwa_zfmaitm_db>-tplnr_fl
                     atnam    = 'CIT-ESPACAMENTO-PES' BINARY SEARCH.
          IF sy-subrc EQ 0.
            MOVE lwa_volume-atflv TO lv_float.
            DATA(lv_espacamento) = lv_float.
          ENDIF.

          READ TABLE lt_glmdhdr_volume INTO lwa_volume
            WITH KEY tplnr_fl = <lwa_zfmaitm_db>-tplnr_fl
                     atnam    = 'FAZ_LARGURA_COPA' BINARY SEARCH.
          IF sy-subrc EQ 0.
            MOVE lwa_volume-atflv TO lv_float.
            DATA(lv_largura) = lv_float.
          ENDIF.

          READ TABLE lt_glmdhdr_volume INTO lwa_volume
            WITH KEY tplnr_fl = <lwa_zfmaitm_db>-tplnr_fl
                     atnam    = 'FAZ_PERCENTAG_FALHA' BINARY SEARCH.
          IF sy-subrc EQ 0.
            MOVE lwa_volume-atflv TO lv_float.
            DATA(lv_percentual) = lv_float.
          ENDIF.

*-- M³ Copa por Planta = (Altura da Copa) x (Largura da Copa) x (Espaçamento entre Plantas)
          DATA(lv_copa_planta) = lv_altura * lv_largura * lv_espacamento.
*-- Volume de Copa = (M³ da Copa por Planta) x (Nº de Plantas) x (% de Falha)
          DATA(lv_volume_copa) = lv_copa_planta * <lwa_zfmaitm_db>-adqpl  * ( 1 - ( lv_percentual / 100 ) ).
          lv_output = lv_volume_copa.
*-- Volume de Copa (ADVLC)
          <lwa_zfmaitm_db>-advlc = lv_output.
          CLEAR: lv_altura, lv_espacamento, lv_largura, lv_percentual, lv_output.

          IF <lwa_zfmaitm_db>-advlc IS NOT INITIAL.
            TRY.
*-- Volume de Calda (ACVCL) = Índice Técnico (ACIDT) x Volume de Copa (ADVLC)
                <lwa_fmacvlcl_db>-acvcl = <lwa_fmacvlcl_db>-acidt * <lwa_zfmaitm_db>-advlc.
              CATCH cx_sy_arithmetic_overflow INTO ref_var.
                lv_text = ref_var->get_text( ).
            ENDTRY.

            IF <lwa_fmacvlcl_db>-acvcl IS NOT INITIAL
            AND <lwa_fmacvlcl_db>-acdis IS NOT INITIAL.
*-- N° Bomba (ACQTB) = Volume de Calda (ACVCL) / Capacidade da Bomba (ACDIS)
              <lwa_fmacvlcl_db>-acqtb = <lwa_fmacvlcl_db>-acvcl / <lwa_fmacvlcl_db>-acdis.
              MOVE c_unit_of_measurement-unit TO <lwa_fmacvlcl_db>-acuqb.
            ENDIF.
          ENDIF.
        ENDIF.

*-- % Execução
        IF <lwa_fmacvlcl_db>-acpex  IS NOT INITIAL.
          READ TABLE lt_zfmaitm_db ASSIGNING <lwa_zfmaitm_db>
            WITH KEY acnum    = lwa_tech_index-acnum
                     tplnr_fl = lwa_tech_index-tplnr_fl BINARY SEARCH.
          IF sy-subrc EQ 0.
*-- Área a Executar = % Execução * Área de terreno absoluto
            <lwa_fmacvlcl_db>-acarx = ( ( <lwa_fmacvlcl_db>-acpex  * <lwa_zfmaitm_db>-aarea ) / 100 ).
            IF <lwa_fmacvlcl_db>-acarx IS NOT INITIAL.
              <lwa_fmacvlcl_db>-unarx = c_unit_of_measurement-hectare.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      READ TABLE lt_tech_index INTO lwa_tech_index INDEX lv_tabix
        COMPARING acnum.
    ENDWHILE.

    CALL FUNCTION 'DEQUEUE_EZ_FMAC'
      EXPORTING
        mandt = sy-mandt
        acnum = <lwa_fmachdr_db>-acnum
        ajahr = <lwa_fmachdr_db>-ajahr.
  ENDLOOP.

  IF lt_fmachdr_db[] IS NOT INITIAL.
    MODIFY zfmachdr FROM TABLE lt_fmachdr_db.
    COMMIT WORK AND WAIT.
  ENDIF.

  IF lt_fmacvlcl_db[] IS NOT INITIAL.
    MODIFY zfmacvlcl FROM TABLE lt_fmacvlcl_db.
    COMMIT WORK AND WAIT.
  ENDIF.

  IF lt_zfmaitm_db[] IS NOT INITIAL.
    MODIFY zfmaitm FROM TABLE lt_zfmaitm_db.
    COMMIT WORK AND WAIT.
  ENDIF.

  et_messages[] = lt_messages[].

ENDMETHOD.
ENDCLASS.
