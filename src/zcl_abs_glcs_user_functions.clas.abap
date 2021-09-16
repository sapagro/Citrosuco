class ZCL_ABS_GLCS_USER_FUNCTIONS definition
  public
  final
  create public .

public section.

  interfaces /AGRI/IF_GLCS_USER_FUNCTIONS .

  class-data V_DISPLAY type XFELD .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ABS_GLCS_USER_FUNCTIONS IMPLEMENTATION.


  method /AGRI/IF_GLCS_USER_FUNCTIONS~EXECUTE.
  endmethod.


METHOD /agri/if_glcs_user_functions~user_functions_define.
************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Method Name       :  ZABS_FM_GET_GRID_REFRESH                        *
* Created By        :  Jetendra Mantena                                *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  06.18.2019                                      *
* TR                :  C4DK901784                                      *
* Version           :  001                                             *
* Description       :  Aadding Dispatch order button to crop season    *
*                      toolbar                                         *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*


****Document Modes
  CONSTANTS: c_mode_create(1)  TYPE c VALUE 'H',
             c_mode_change(1)  TYPE c VALUE 'V',
             c_mode_display(1) TYPE c VALUE 'A',
             c_mode_copy(1)    TYPE c VALUE 'C',
             c_mode_delete(1)  TYPE c VALUE 'D'.

  IF  ( sy-tcode EQ '/AGRI/GLFLCA' )
  AND ( i_mode EQ c_mode_create OR
        i_mode EQ c_mode_change ).
    INSERT INITIAL LINE INTO TABLE et_user_functions
      ASSIGNING FIELD-SYMBOL(<ls_user_function>).
    IF sy-subrc EQ 0.
      <ls_user_function>-function = 'DATEX'.
*-- Modificar Data Início
      <ls_user_function>-icon = icon_change.
      <ls_user_function>-text = TEXT-t01.
      <ls_user_function>-quickinfo = TEXT-t01.
    ENDIF.
*--BOC- T_T.KONNO-05.28.21
    INSERT INITIAL LINE INTO TABLE et_user_functions
      ASSIGNING <ls_user_function>.
    IF sy-subrc EQ 0.
      <ls_user_function>-function = 'ZPREV'.
*-- Verificar Previsão
      <ls_user_function>-icon = icon_availability_check.
      <ls_user_function>-text = TEXT-t02.
      <ls_user_function>-quickinfo = TEXT-t02.
    ENDIF.
*--EOC- T_T.KONNO-05.28.21
  ENDIF.

ENDMETHOD.


METHOD /agri/if_glcs_user_functions~user_function_execute.
************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Method Name       :  ZABS_FM_GET_GRID_REFRESH                        *
* Created By        :  Jetendra Mantena                                *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  06.18.2019                                      *
* TR                :  C4DK901784                                      *
* Version           :  001                                             *
* Description       :  Creating Multiple dispatch orders for single    *
*                      crop seasons                                    *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*


*&---------------------------------------------------------------------*
*&    TYPES
*&---------------------------------------------------------------------*
  TYPES: BEGIN OF ly_fmfphdr,
           aufnr    TYPE /agri/fmfpnum,
           auart    TYPE aufart,
           autyp    TYPE /agri/gl_autyp,
           tplnr_fl TYPE /agri/gltplnr_fl,
           contr    TYPE /agri/gcontr,
           tplma    TYPE /agri/gltplma,
           cmnum    TYPE /agri/glcmnum,
           varia    TYPE /agri/glvaria,
           cpros    TYPE /agri/glcpros,
           iwerk    TYPE iwerk,
           tecom    TYPE /agri/fmtecom,
         END OF ly_fmfphdr,

         ly_fmfphdr_tab TYPE STANDARD TABLE OF ly_fmfphdr.
*&---------------------------------------------------------------------*
*&    TABLES AND VARIABLES
*&---------------------------------------------------------------------*
  DATA: lt_selected_rows  TYPE lvc_t_row,
        lt_fmfphdr        TYPE ly_fmfphdr_tab,
        lt_aufnr          TYPE /agri/t_fmaufnr,
        lt_msg_teco       TYPE /agri/t_gprolog,
        lt_orders         TYPE TABLE OF bapi_order_key,
        lt_return         TYPE cocf_t_bapi_return,
        ls_status         TYPE bapiret2,
        ls_duration       TYPE psen_duration,
        lv_subrc_new      TYPE sy-subrc,
        lv_tabix_new      TYPE sy-tabix,
        lv_view_tabix_new TYPE sy-tabix,
        lv_data_ref       TYPE sy-datum,
        lv_data_inicial   TYPE sy-datum,
        lv_data_final     TYPE sy-datum.
*&---------------------------------------------------------------------*
*&    FIELD-SYMBOLS
*&---------------------------------------------------------------------*
  FIELD-SYMBOLS: <lt_doc>            TYPE /agri/t_glcs_doc,
                 <lt_selected_rows>  TYPE lvc_t_row,
                 <lv_manual_changes> TYPE abap_bool,
                 <lv_data_changed>   TYPE abap_bool.
*&---------------------------------------------------------------------*
*&    CONSTANTS
*&---------------------------------------------------------------------*
*-- Update Indicators
  CONSTANTS: c_updkz_new(1)     TYPE c VALUE 'I',
             c_updkz_update(1)  TYPE c VALUE 'U',
             c_updkz_delete(1)  TYPE c VALUE 'D',
             c_updkz_old(1)     TYPE c VALUE ' ',
             c_updkz_newrow     TYPE c VALUE 'N',
             c_updkz_propose(1) TYPE c VALUE 'P'.

  CONSTANTS : BEGIN OF c_ownership,
                own         TYPE /agri/glownshp VALUE 'OW',
                third_party TYPE /agri/glownshp VALUE 'TP',
              END OF c_ownership,

              BEGIN OF c_crop_season,
                formacao   TYPE /agri/glvaria VALUE 'FORMAÇÃO',
                manutencao TYPE /agri/glvaria VALUE 'MANUT&COLHEITA',
              END OF c_crop_season,

              BEGIN OF c_crop_process,
                formacao    TYPE /agri/glcpros VALUE 'FORMAÇÃO',
                manutencao  TYPE /agri/glcpros VALUE 'MANUTEÇÃO',
                colheita    TYPE /agri/glcpros VALUE 'COLHEITA',
                implantacao TYPE /agri/glcpros VALUE 'IMPLANT',
                close_all   TYPE /agri/glcpros VALUE abap_true,
              END OF c_crop_process.

  ASSIGN: ('(/AGRI/SAPLGLCSM)GT_CSDOC_INFOCUS[]') TO <lt_doc>,
          ('(/AGRI/SAPLGLCSM)GS_VARIABLES-MANUAL_CHANGES') TO <lv_manual_changes>,
          ('(/AGRI/SAPLGLCSM)GS_VARIABLES-DATA_CHANGED') TO <lv_data_changed>.

  IF <lt_doc> IS ASSIGNED
  AND <lv_manual_changes> IS ASSIGNED
  AND <lv_data_changed> IS ASSIGNED.
    DATA(lv_sel_lines) = lines( it_selected_rows ).

    IF i_user_function EQ 'DATEX'.
      IF lv_sel_lines IS INITIAL.
*-- Nenhuma linha selecionada!
        MESSAGE i168(zfmfp).
      ELSEIF lv_sel_lines GT 1.
*-- Selecionar somente uma linha!
        MESSAGE i169(zfmfp).
      ELSEIF lv_sel_lines EQ 1.
        READ TABLE it_selected_rows INTO DATA(ls_selected_row) INDEX 1.
        IF sy-subrc EQ 0.
          READ TABLE <lt_doc> ASSIGNING FIELD-SYMBOL(<ls_doc>)
            INDEX ls_selected_row-index.
          IF sy-subrc EQ 0.
            CALL FUNCTION 'ZABS_FM_GLFLCA_DATAB'
              CHANGING
                cv_datab = <ls_doc>-x-cshdr-datab.
            <lv_manual_changes> = <lv_data_changed> = abap_true.
            IF <ls_doc>-x-cshdr-updkz IS INITIAL.
              <ls_doc>-x-cshdr-updkz = c_updkz_update.
            ENDIF.
            LOOP AT <ls_doc>-x-csprs ASSIGNING FIELD-SYMBOL(<ls_csprs>).
              <ls_csprs>-strdt = <ls_doc>-x-cshdr-datab.
              <ls_csprs>-enddt = <ls_doc>-x-cshdr-datbi.
              IF <ls_doc>-x-cshdr-updkz IS INITIAL.
                <ls_csprs>-updkz = c_updkz_update.
              ELSE.
                EXIT.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
*--BOC- T_T.KONNO-05.28.21
    ELSEIF i_user_function EQ 'ZPREV'.
      SELECT *
        FROM /agri/glseason
        INTO TABLE @DATA(lt_season).

      SORT lt_season BY season.

      LOOP AT <lt_doc> ASSIGNING FIELD-SYMBOL(<ls_csdoc_infocus>).
        DATA(lv_index) = sy-tabix.
*-- MANUT&COLHEITA (MANUTENÇÃO) --> Gestão Erradicação
*-- BOC-T_T.KONNO-07.12.21
*        IF <ls_csdoc_infocus>-x-cshdr-updkz IS INITIAL
*        OR <ls_csdoc_infocus>-x-cshdr-varia NE 'MANUT&COLHEITA'.
*          CONTINUE.
*        ENDIF.
        IF <ls_csdoc_infocus>-x-cshdr-varia EQ 'MANUT&COLHEITA'.
*-- EOC-T_T.KONNO-07.12.21

          IF <ls_csdoc_infocus>-x-cshdr-zzprev_errad IS NOT INITIAL.
            IF <ls_csdoc_infocus>-x-cshdr-zzprev_errad NE <ls_csdoc_infocus>-y-cshdr-zzprev_errad.
              <ls_csdoc_infocus>-x-cshdr-zzprevisto = abap_true.
*-- Previsão Erradicação -> Data Final Safra
              <ls_csdoc_infocus>-x-cshdr-datbi = <ls_csdoc_infocus>-x-cshdr-zzprev_errad.
*-- Previsão Erradicação -> Data Final (Processos de épocas de cultura)
              LOOP AT <ls_csdoc_infocus>-x-csprs ASSIGNING <ls_csprs>.
                <ls_csprs>-enddt = <ls_csdoc_infocus>-x-cshdr-zzprev_errad.
                <ls_csprs>-updkz = 'U'.
              ENDLOOP.
            ENDIF.
          ELSEIF <ls_csdoc_infocus>-x-cshdr-zzprev_errad IS INITIAL.
            IF <ls_csdoc_infocus>-y-cshdr-zzprev_errad IS NOT INITIAL.
*-- BOC-T_T.KONNO-07.12.21
*              INSERT INITIAL LINE INTO TABLE lt_selected_rows
*                ASSIGNING FIELD-SYMBOL(<ls_selected_row>).
*              IF sy-subrc EQ 0.
*                <ls_selected_row>-index = lv_index.
*              ENDIF.
*-- EOC-T_T.KONNO-07.12.21
*-- SAFRA PROD --> MANUTENÇÃO
              READ TABLE lt_season INTO DATA(ls_season_manu)
                WITH KEY season = 'SAFRA PROD'.
              IF sy-subrc EQ 0.
*-- Período Final da Época 20210403 [YYYYMMDD]
*-- Devemos utilizar como referência o registro anterior às modificações
                lv_data_ref = <ls_csdoc_infocus>-y-cshdr-datab.
                IF ls_season_manu-offst LE 999.
                  ls_duration-duryy = ls_season_manu-offst.
                  CALL FUNCTION 'HR_99S_DATE_ADD_SUB_DURATION'
                    EXPORTING
                      im_date     = lv_data_ref
                      im_operator = '+'
                      im_duration = ls_duration
                    IMPORTING
                      ex_date     = lv_data_final.

                  lv_data_final = lv_data_final(4) && ls_season_manu-eperd.

                  <ls_csdoc_infocus>-x-cshdr-datbi = lv_data_final.

                  IF <ls_csdoc_infocus>-x-cshdr-astat EQ 'A'.
*-- Apaga Previsto
                    CLEAR <ls_csdoc_infocus>-x-cshdr-zzprevisto.
                  ENDIF.

*-- Data Final Safra -> Data Final (Processos de épocas de cultura)
                  LOOP AT <ls_csdoc_infocus>-x-csprs ASSIGNING <ls_csprs>.
                    <ls_csprs>-enddt = lv_data_final.
                    <ls_csprs>-updkz = 'U'.
                  ENDLOOP.
                ENDIF.
              ENDIF.
*-- BOC-T_T.KONNO-07.12.21
**-- Verifica Linha Ativa Prevista de FORMAÇÃO
*            READ TABLE <lt_doc> ASSIGNING FIELD-SYMBOL(<ls_csdoc_form>)
*              WITH KEY x-cshdr-cmnum      = 'CITROS'
*                       x-cshdr-varia      = 'FORMAÇÃO'
*                       x-cshdr-astat      = 'A'
*                       x-cshdr-zzprevisto = abap_true.
*            IF sy-subrc NE 0.
*              READ TABLE <lt_doc> ASSIGNING <ls_csdoc_form>
*                WITH KEY x-cshdr-cmnum      = 'CITROS'
*                         x-cshdr-varia      = 'FORMAÇÃO'.
*            ENDIF.
*            IF sy-subrc EQ 0.
*              lv_index = sy-tabix.
*              INSERT INITIAL LINE INTO TABLE lt_selected_rows
*                ASSIGNING <ls_selected_row>.
*              IF sy-subrc EQ 0.
*                <ls_selected_row>-index = lv_index.
*              ENDIF.
*              <ls_csdoc_form>-updkz = 'U'.
**-- SAFRA PLAN --> FORMAÇÃO
*              READ TABLE lt_season INTO DATA(ls_season_form)
*                WITH KEY season = 'SAFRA PLAN'.
*              IF sy-subrc EQ 0.
**-- Devemos utilizar como referência o registro anterior às modificações
*                lv_data_ref = <ls_csdoc_form>-y-cshdr-datbi.
*                IF ls_season_manu-offst LE 999.
*                  ls_duration-duryy = ls_season_form-offst.
*                  CALL FUNCTION 'HR_99S_DATE_ADD_SUB_DURATION'
*                    EXPORTING
*                      im_date     = lv_data_ref
*                      im_operator = '-'
*                      im_duration = ls_duration
*                    IMPORTING
*                      ex_date     = lv_data_inicial.
*
*                  lv_data_inicial = lv_data_inicial(4) && ls_season_form-sperd.
*                ENDIF.
*              ENDIF.
**-- Apaga Data Previsão Plantio e Previsto
*              CLEAR: <ls_csdoc_form>-x-cshdr-zzprev_plantio,
*                     <ls_csdoc_form>-x-cshdr-zzprevisto.
**-- Fecha Safra FORMAÇÃO
*              <ls_csdoc_form>-x-cshdr-astat = 'C'.
**-- Data Final Safra -> Data Final (Processos de épocas de cultura)
*              LOOP AT <ls_csdoc_form>-x-csprs ASSIGNING <ls_csprs>.
*                <ls_csprs>-enddt = <ls_csdoc_form>-x-cshdr-datab = lv_data_inicial.
*                <ls_csprs>-updkz = 'U'.
*              ENDLOOP.
*-- EOC-T_T.KONNO-07.12.21
            ENDIF.
          ENDIF.
*-- BOC-T_T.KONNO-07.12.21
        ELSE.
          IF <ls_csdoc_infocus>-x-cshdr-zzprev_plantio IS INITIAL.
            IF <ls_csdoc_infocus>-y-cshdr-zzprev_plantio IS NOT INITIAL.
              <ls_csdoc_infocus>-x-cshdr-updkz = c_updkz_update.
              <ls_csdoc_infocus>-x-cshdr-loevm = 'X'.
*-------------------------------------------------------------------*
*-- Encerra todas as ordens [FORMAÇÃO]
*-------------------------------------------------------------------*
*-- Process orders
              REFRESH lt_fmfphdr.
              SELECT aufnr, auart, autyp, tplnr_fl, contr,
                     tplma, cmnum, varia, cpros, iwerk, tecom
                FROM /agri/fmfphdr
                INTO TABLE @lt_fmfphdr
               WHERE tplnr_fl EQ @<ls_csdoc_infocus>-x-cshdr-tplnr_fl
                 AND cmnum    EQ @<ls_csdoc_infocus>-x-cshdr-cmnum
                 AND varia    EQ @<ls_csdoc_infocus>-x-cshdr-varia
                 AND iwerk    EQ @<ls_csdoc_infocus>-x-cshdr-iwerk
                 AND tecom    EQ @abap_false.

              DELETE lt_fmfphdr WHERE varia NE c_crop_season-formacao
                                   OR tplnr_fl NE <ls_csdoc_infocus>-x-cshdr-tplnr_fl.

              DELETE lt_fmfphdr WHERE tplnr_fl NE <ls_csdoc_infocus>-x-cshdr-tplnr_fl
                                   OR contr    NE <ls_csdoc_infocus>-x-cshdr-contr
                                   OR cmnum    NE <ls_csdoc_infocus>-x-cshdr-cmnum
                                   OR varia    NE <ls_csdoc_infocus>-x-cshdr-varia
                                   OR iwerk    NE <ls_csdoc_infocus>-x-cshdr-iwerk
                                   OR tecom    EQ abap_true.

              IF lt_fmfphdr[] IS NOT INITIAL.
                REFRESH: lt_aufnr, lt_msg_teco.
                lt_aufnr = CORRESPONDING #( lt_fmfphdr ).
*-- Order Mass Technical Complete
                CALL FUNCTION 'ZABS_FM_ORDER_MASS_TECO'
                  EXPORTING
                    it_aufnr    = lt_aufnr
                  IMPORTING
                    et_messages = lt_msg_teco.
              ENDIF.
*-------------------------------------------------------------------*
*-- Encerra Ordem Pátio [FORMAÇÃO]
              IF <ls_csdoc_infocus>-x-cshdr-yaufnr IS NOT INITIAL.
                CLEAR ls_status.
                REFRESH: lt_orders, lt_return, lt_msg_teco.
                INSERT INITIAL LINE INTO TABLE lt_orders
                  ASSIGNING FIELD-SYMBOL(<ls_order>).
                IF sy-subrc EQ 0.
                  <ls_order>-order_number = <ls_csdoc_infocus>-x-cshdr-yaufnr.
                  CALL FUNCTION 'BAPI_PRODORD_COMPLETE_TECH'
                    IMPORTING
                      return        = ls_status
                    TABLES
                      orders        = lt_orders
                      detail_return = lt_return.

                  READ TABLE lt_return TRANSPORTING NO FIELDS
                    WITH KEY type = 'E'.
                  IF sy-subrc NE 0.
                    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                      EXPORTING
                        wait = 'X'.
*-- Ordem Pátio &1.
                    INSERT INITIAL LINE INTO TABLE lt_msg_teco
                      ASSIGNING FIELD-SYMBOL(<ls_message>).
                    IF sy-subrc EQ 0.
                      <ls_message>-msgid = 'ZFMFP'.
                      <ls_message>-msgno = '375'.
                      <ls_message>-msgty = 'S'.
                      <ls_message>-msgv1 = <ls_csdoc_infocus>-x-cshdr-yaufnr.
                    ENDIF.
                  ELSE.
                    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
                  ENDIF.

                  LOOP AT lt_return INTO DATA(ls_return).
                    INSERT INITIAL LINE INTO TABLE lt_msg_teco
                      ASSIGNING <ls_message>.
                    IF sy-subrc EQ 0.
                      <ls_message>-msgid = ls_return-id.
                      <ls_message>-msgno = ls_return-number.
                      <ls_message>-msgty = ls_return-type.
                      <ls_message>-msgv1 = ls_return-message_v1.
                      <ls_message>-msgv2 = ls_return-message_v2.
                      <ls_message>-msgv3 = ls_return-message_v3.
                      <ls_message>-msgv4 = ls_return-message_v4.
                    ENDIF.
                  ENDLOOP.
                ENDIF.
              ENDIF.
*-------------------------------------------------------------------*
            ENDIF.
          ENDIF.
*-- EOC-T_T.KONNO-07.12.21
        ENDIF.
      ENDLOOP.

      IF lt_selected_rows[] IS NOT INITIAL.
        ASSIGN ('(/AGRI/SAPLGLCSM)GT_SELECTED_ROWS') TO <lt_selected_rows>.
        IF sy-subrc EQ 0.
          <lt_selected_rows> = lt_selected_rows[].
          PERFORM fcode_mcldt IN PROGRAM /agri/saplglcsm IF FOUND.
        ENDIF.
      ENDIF.
*--EOC- T_T.KONNO-05.28.21
    ENDIF.

    "DELETE <lt_doc> WHERE updkz = c_updkz_delete.

    c_refresh_view = abap_true.
  ENDIF.

ENDMETHOD.
ENDCLASS.
