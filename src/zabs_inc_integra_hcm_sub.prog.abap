************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name    : ZABS_REP_HCM_INTEGRATION                            *
* Tcode          : ZABS_COL_HCM                                        *
* Created By     : Helio Kababe                                        *
* Requested by   : DAniele Janes                                       *
* Created on     : 16.04.2021                                          *
* TR             :                                                     *
* Version        : 001                                                 *
* Description    : Update productions and events into HCM Info type    *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*&---------------------------------------------------------------------*
*& Form INITIALIZE_GLOBAL_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM initialize_global_data.

*-- Refresh Global data
  REFRESH: gt_productions, gt_events, gt_output.
  CLEAR: gobj_cont, gobj_grid.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FETCH_PROD_EVENTS_FOR_HCM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM fetch_prod_events_for_hcm.

  TYPES: BEGIN OF ly_atributos,
           safra          TYPE atinn,
           data_encerra   TYPE atinn,
           estimativa     TYPE atinn,
           cxpl_estimada  TYPE atinn,
           prod_real      TYPE atinn,
           cxpl_real      TYPE atinn,
           cod_imovel     TYPE atinn,
           talhao         TYPE atinn,
           florada        TYPE atinn,
           estimativa_ini TYPE atinn,
           estimativa_fim TYPE atinn,
           variacao_perc  TYPE atinn,
         END OF ly_atributos,

         BEGIN OF ly_aux,
           pernr   TYPE pernr_d,
           hora    TYPE zhrst_ed_hora,
           coletor TYPE zhrst_ed_coletor,
           idactvl TYPE /agri/fmidactl,
           zzjust	 TYPE zabs_del_ejust,
         END OF ly_aux.

*-- Local Declarations
  DATA: lt_turmas      TYPE STANDARD TABLE OF ty_turmas,
        lt_resources   TYPE STANDARD TABLE OF ty_resources,
        lt_terrains    TYPE STANDARD TABLE OF ty_terrains,
        lt_dados_aux   TYPE STANDARD TABLE OF zabs_str_integra_hcm,
        lt_aux         TYPE STANDARD TABLE OF ly_aux,
        lrt_pernr      TYPE RANGE OF persno,
        ls_turmas      TYPE ty_turmas,
        ls_resources   TYPE ty_resources,
        ls_terrains    TYPE ty_terrains,
        lt_split       TYPE STANDARD TABLE OF string,
        lrt_zzhdate    TYPE RANGE OF zabs_del_hdate,
        ls_productions TYPE p9900,
        ls_events      TYPE p2001,
        ls_dados       TYPE zhrst_ocorr_aprov,
        lt_tplnr       TYPE /agri/t_gltplnr,
        ls_tplnr       TYPE /agri/s_gltplnr,
        lt_complex     TYPE zabs_t_terrain_complex,
        ls_return      TYPE bapireturn1,
        ls_atributo    TYPE ly_atributos,
        lv_strno       TYPE /agri/glstrno,
        lv_idresource  TYPE /agri/fmidrsc,
        lv_data_ref    TYPE sy-datum,
        lv_pernr       TYPE persno,
        lv_permo       TYPE permo,
        lv_zzcdate     TYPE erdat,
        lv_pabrj       TYPE pabrj,
        lv_pabrp       TYPE pabrp.

  READ TABLE so_date INTO DATA(ls_date) INDEX 1.
  IF sy-subrc EQ 0.
    DATA(lv_begda) = ls_date-low.
    DATA(lv_endda) = ls_date-high.
    IF lv_endda IS INITIAL.
      lv_endda = lv_begda.
    ENDIF.
  ENDIF.

  INSERT INITIAL LINE INTO TABLE lrt_zzhdate
    ASSIGNING FIELD-SYMBOL(<lrs_zzhdate>).
  IF sy-subrc EQ 0.
    <lrs_zzhdate> = 'IBT'.
    lv_data_ref = lv_begda - 5.
    <lrs_zzhdate>-low = lv_data_ref.
    <lrs_zzhdate>-high = lv_endda.
  ENDIF.

*-- Fetch the productions and events from accomplishment sheets
  SELECT a~accom, a~ernam, a~erdat, a~zzturma,
         a~zzactcg, b~posnr, b~tplnr, b~idresource,
         b~arbpl, b~strtdat, b~strttim, b~findat,
         b~fintim, b~idactvl, b~menge, b~zzjust,
         b~zzcdate, b~zzctime, b~zzconftyp, b~zzhdate
    FROM /agri/fmachdr AS a
    INNER JOIN /agri/fmacitm AS b
    ON b~accom EQ a~accom
    INTO TABLE @DATA(lt_prod_events)
   WHERE a~accom      IN @so_accom
     AND a~actyp      EQ @p_actyp
     AND b~tplnr      IN @so_tplnr[]
     AND b~aufnr      IN @so_aufnr[]
     AND b~idresource IN @so_id[]
     AND b~idactvl    IN @so_act[]
     AND b~zzcdate    IN @so_date[]
     AND b~zzctime    IN @so_time
     AND b~zzhdate    IN @lrt_zzhdate[].

  IF sy-subrc EQ 0.
*-- Delete the accomplishments which are not related to productions or
*-- events
    DELETE lt_prod_events WHERE zzturma IS INITIAL
                             OR zzactcg IS INITIAL.

    lv_permo = 27. "Parâmetros período
    lv_pabrj = sy-datum(4). "Ano flh.pgto.

    SELECT *
      FROM t549q
      INTO TABLE @DATA(lt_t549q)
     WHERE permo EQ @lv_permo
       AND pabrj EQ @lv_pabrj
       AND begda LE @lv_begda
       AND endda GE @lv_endda.

    IF sy-subrc EQ 0.
      LOOP AT lt_prod_events INTO DATA(ls_prod_event).
        DATA(lv_tabix_del) = sy-tabix.

        IF lv_zzcdate NE ls_prod_event-zzcdate.
          DATA(lv_t549q_found) = abap_false.
          LOOP AT lt_t549q INTO DATA(ls_t549q).
            IF ls_t549q-begda LE ls_prod_event-zzcdate
            AND ls_t549q-endda GT ls_prod_event-zzcdate.
              lv_t549q_found = abap_true.
*-- Tolerância de 3 dias no final da validade
              ADD 3 TO ls_t549q-endda.
              EXIT.
            ENDIF.
          ENDLOOP.
        ENDIF.

        IF lv_t549q_found EQ abap_true.
          IF ls_prod_event-zzhdate
          NOT BETWEEN ls_t549q-begda
                  AND ls_t549q-endda
                  OR ls_prod_event-zzcdate GT ls_t549q-endda.
            DELETE lt_prod_events INDEX lv_tabix_del.
          ENDIF.
        ENDIF.

        lv_zzcdate = ls_prod_event-zzcdate.
      ENDLOOP.
    ENDIF.
  ENDIF.

*-- Give a message if no productions or events found with the selections
  IF lt_prod_events[] IS INITIAL.
    MESSAGE TEXT-002
       TYPE zcl_abs_abap_maintain=>c_msgty_info.
    LEAVE LIST-PROCESSING.
  ENDIF.

  LOOP AT lt_prod_events ASSIGNING FIELD-SYMBOL(<fs_prod_events>).

*-- Collect Turmas
    CLEAR ls_turmas.
    ls_turmas-turma = <fs_prod_events>-zzturma.
    COLLECT ls_turmas INTO lt_turmas.

*-- Collect Resources
    CLEAR ls_resources.
    ls_resources-idresource = <fs_prod_events>-idresource.
    ls_resources-arbpl = <fs_prod_events>-arbpl.
    COLLECT ls_resources INTO lt_resources.

*-- Collect Terrains
    CLEAR ls_tplnr.
    ls_tplnr-tplnr_fl = <fs_prod_events>-tplnr.
    COLLECT ls_tplnr INTO lt_tplnr.

*-- Collect forms
    CLEAR ls_terrains.
    ls_terrains-tplnr_fl = <fs_prod_events>-tplnr.

    CLEAR lv_strno.
    CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
      EXPORTING
        input  = ls_terrains-tplnr_fl
      IMPORTING
        output = lv_strno.

    REFRESH lt_split.
    SPLIT lv_strno AT '-' INTO TABLE lt_split.

    READ TABLE lt_split INTO DATA(ls_split) INDEX 1.
    IF sy-subrc EQ 0.
      ls_terrains-form = ls_split.
    ENDIF.

    CLEAR ls_split.
    READ TABLE lt_split INTO ls_split INDEX 2.
    IF sy-subrc EQ 0.
      ls_terrains-terrain = ls_split.
    ENDIF.

    COLLECT ls_terrains INTO lt_terrains.
  ENDLOOP.

  IF lt_turmas[] IS NOT INITIAL.
*-- Fetch the HCM Turmas
    SELECT orgeh, turma
      FROM yoturmas
      INTO TABLE @DATA(lt_hcm_turm)
       FOR ALL ENTRIES IN @lt_turmas
     WHERE turma EQ @lt_turmas-turma.
    IF sy-subrc EQ 0.
      SORT lt_hcm_turm BY turma.
    ENDIF.
  ENDIF.

  DATA(lt_resources_aux) = lt_resources[].
  SORT lt_resources_aux BY idresource arbpl.
  IF lt_resources[] IS NOT INITIAL.
    LOOP AT lt_resources INTO DATA(ls_resource).
      IF ls_resource-idresource IS NOT INITIAL.
        PACK ls_resource-idresource TO ls_resource-idresource.
        CONDENSE ls_resource-idresource.
        APPEND VALUE #( low    = ls_resource-idresource(8)
                        sign   = 'I'
                        option = 'EQ' ) TO lrt_pernr.
      ENDIF.
    ENDLOOP.

    IF lrt_pernr[] IS NOT INITIAL.
      SELECT pernr, persg
        FROM pa0001
        INTO TABLE @DATA(lt_pa0001)
       WHERE pernr IN @lrt_pernr[]
         AND persg EQ 'S'.

      SORT lt_pa0001 BY pernr.
    ENDIF.

*-- Fetch the HCM Resources
    SELECT idresource arbpl pernr
      FROM /agri/fmacres
      INTO TABLE lt_resources
      FOR ALL ENTRIES IN lt_resources
     WHERE idresource EQ lt_resources-idresource
       AND arbpl      EQ lt_resources-arbpl.
    IF sy-subrc EQ 0.
      SORT lt_resources BY idresource arbpl.
    ENDIF.
  ENDIF.

  IF lt_resources[] IS NOT INITIAL.
    SELECT i~turma_id, i~pernr, i~idresource,
           t~usr_aprov
      FROM zfmfpgroupitm AS i
      INNER JOIN zhrst_tb_turma AS t
      ON i~turma_id EQ t~turma
      INTO TABLE @DATA(lt_groupitm)
      FOR ALL ENTRIES IN @lt_resources
     WHERE pernr EQ @lt_resources-pernr.

    SORT lt_groupitm BY pernr.
  ENDIF.

  DATA(lt_forms) = lt_terrains.
  SORT lt_forms BY form.
  DELETE ADJACENT DUPLICATES FROM lt_forms COMPARING form.

  IF lt_tplnr IS NOT INITIAL.
*-- Fetch the complexity for terrains
    CALL FUNCTION 'ZABS_FM_COMPLEXITY_GET'
      EXPORTING
        it_tplnr   = lt_tplnr
      IMPORTING
        et_complex = lt_complex.
  ENDIF.

  IF lt_forms IS NOT INITIAL.
    SELECT faz_btrtl, btrtl, descr, fazenda_farm
      FROM yofazendas
      INTO TABLE @DATA(lt_hcm_form)
      FOR ALL ENTRIES IN @lt_forms
     WHERE fazenda_farm EQ @lt_forms-form(6).
    IF sy-subrc EQ 0.
      SORT lt_hcm_form BY fazenda_farm.
    ENDIF.
  ENDIF.

  SORT lt_terrains BY tplnr_fl.

  DATA(lt_productions) = lt_prod_events[].
  DELETE lt_productions WHERE zzactcg NE zcl_abs_abap_maintain=>c_actcg_prod. "Productions

  REFRESH gt_inactive.
*-- Collect HCM Productions
  LOOP AT lt_productions ASSIGNING <fs_prod_events>.
*-- Read HCN Resource
    CLEAR: ls_productions, ls_resources, lv_pernr, lv_idresource.

    READ TABLE lt_resources INTO ls_resources
      WITH KEY idresource = <fs_prod_events>-idresource
               arbpl      = <fs_prod_events>-arbpl BINARY SEARCH.
    IF sy-subrc EQ 0.
      lv_pernr = ls_resources-pernr.
    ELSE.
      PACK <fs_prod_events>-idresource TO lv_idresource.
      CONDENSE lv_idresource.
      lv_pernr = lv_idresource(8).
      READ TABLE lt_pa0001 INTO DATA(ls_pa0001)
        WITH KEY pernr = lv_pernr BINARY SEARCH.
      IF sy-subrc NE 0.
        CLEAR lv_pernr.
        CONTINUE.
      ENDIF.
    ENDIF.

    DATA(lv_active) = abap_true.
    IF lv_pernr IS NOT INITIAL.
      CALL FUNCTION 'CATS_CHECK_EMPLOYEE_ACTIVE'
        EXPORTING
          pernr            = lv_pernr
          begda            = <fs_prod_events>-zzhdate
          endda            = <fs_prod_events>-zzhdate
        EXCEPTIONS
          pernr_not_found  = 1
          pernr_not_active = 2
          OTHERS           = 3.

      IF sy-subrc EQ 0.
        ls_productions-pernr = lv_pernr.
      ELSE.
        lv_active = abap_false.
      ENDIF.
    ENDIF.

    ls_productions-endda = <fs_prod_events>-zzhdate.
    ls_productions-begda = <fs_prod_events>-zzhdate.
    ls_productions-aedtm = <fs_prod_events>-erdat.

    READ TABLE lt_groupitm INTO DATA(ls_groupitm)
      WITH KEY pernr = ls_productions-pernr BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_productions-uname = ls_groupitm-usr_aprov.
    ENDIF.

    ls_productions-cx_colh = <fs_prod_events>-menge.

*-- Read HCM Turma
    READ TABLE lt_hcm_turm INTO DATA(ls_hcm_turm)
          WITH KEY turma = <fs_prod_events>-zzturma
        BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_productions-turma = ls_hcm_turm-orgeh.
    ENDIF.

*-- Read complex of the Terrain
    READ TABLE lt_complex INTO DATA(ls_complex)
          WITH KEY tplnr = <fs_prod_events>-tplnr
        BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_productions-complex = ls_complex-complex.
    ENDIF.

*-- Read HCM Form
    CLEAR ls_terrains.
    READ TABLE lt_terrains INTO ls_terrains
          WITH KEY tplnr_fl = <fs_prod_events>-tplnr
        BINARY SEARCH.
    IF sy-subrc EQ 0.
      READ TABLE lt_hcm_form INTO DATA(ls_hcm_form)
            WITH KEY fazenda_farm = ls_terrains-form BINARY SEARCH.
      IF sy-subrc EQ 0.
*       ls_productions-faz_btrtl = ls_hcm_form-btrtl.
        ls_productions-faz_btrtl = ls_hcm_form-fazenda_farm.
      ENDIF.

      ls_productions-talhao = ls_terrains-terrain.
    ENDIF.

    ls_productions-hora    = <fs_prod_events>-strttim.
    ls_productions-coletor = <fs_prod_events>-accom.

    IF lv_active = abap_true.
      APPEND ls_productions TO gt_productions.

      INSERT INITIAL LINE INTO TABLE lt_aux
        ASSIGNING FIELD-SYMBOL(<ls_aux>).
      IF sy-subrc EQ 0.
        <ls_aux>-pernr = lv_pernr.
        <ls_aux>-hora    = <fs_prod_events>-strttim.
        <ls_aux>-coletor = <fs_prod_events>-accom.
        <ls_aux>-idactvl = <fs_prod_events>-idactvl.
        <ls_aux>-zzjust  =  <fs_prod_events>-zzjust.
      ENDIF.
    ELSE.
      INSERT INITIAL LINE INTO TABLE gt_inactive
        ASSIGNING FIELD-SYMBOL(<ls_inactive>).
      IF sy-subrc EQ 0.
*-- N° pessoal &1 está inativo
        ls_return-type = 'E'.
        ls_return-id = 'ZFMFP'.
        ls_return-number = 265.
        ls_return-message_v1 = lv_pernr.
        MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
          WITH ls_return-message_v1 INTO ls_return-message.
        <ls_inactive>-pernr = lv_pernr.
        MOVE-CORRESPONDING ls_return TO <ls_inactive>.
      ENDIF.
    ENDIF.
  ENDLOOP.

  APPEND LINES OF gt_inactive TO gt_output.

  IF gt_productions[] IS NOT INITIAL.
    SELECT pernr, subty, objps, sprps,
           endda, begda, seqnr, aedtm,
           uname, cx_colh, faz_btrtl,
           talhao, hora, coletor
      FROM pa9900
      INTO TABLE @DATA(lt_pa9900)
      FOR ALL ENTRIES IN @gt_productions
     WHERE pernr     = @gt_productions-pernr     "2-IDRESOURCE
       AND endda     = @gt_productions-endda     "3-ZZHDATE
       AND begda     = @gt_productions-begda     "3-ZZHDATE
       AND cx_colh   = @gt_productions-cx_colh   "5-MENGE
       AND faz_btrtl = @gt_productions-faz_btrtl "6-FAZENDA
       AND talhao    = @gt_productions-talhao    "6-TALHÃO
       AND hora      = @gt_productions-hora      "4-STRTTIM
       AND coletor   = @gt_productions-coletor.  "1-ACOMM

    SORT: lt_pa9900 BY coletor faz_btrtl talhao pernr begda endda hora cx_colh,
          gt_productions BY coletor faz_btrtl talhao pernr begda endda hora cx_colh.

    DELETE ADJACENT DUPLICATES FROM gt_productions
      COMPARING coletor faz_btrtl talhao pernr begda endda hora cx_colh.

    SORT lt_aux BY pernr coletor hora.

*-- Check productions in HCM [PA9900]
    LOOP AT gt_productions INTO DATA(ls_production).
      INSERT INITIAL LINE INTO TABLE gt_output
        ASSIGNING FIELD-SYMBOL(<ls_output>).
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING ls_production TO <ls_output>.
        READ TABLE lt_aux INTO DATA(ls_aux)
          WITH KEY pernr   = ls_production-pernr
                   coletor = ls_production-coletor
                   hora    = ls_production-hora BINARY SEARCH.
        IF sy-subrc EQ 0.
          <ls_output>-awart  = ls_aux-idactvl.
          <ls_output>-zzjust = ls_aux-zzjust.
        ENDIF.

        READ TABLE lt_pa9900 INTO DATA(ls_pa9900)
          WITH KEY coletor   = ls_production-coletor   "Realização
                   faz_btrtl = ls_production-faz_btrtl "Fazenda
                   talhao    = ls_production-talhao    "Talhão
                   pernr     = ls_production-pernr     "ID Empregado
                   begda     = ls_production-begda     "Data Colheira
                   endda     = ls_production-endda     "Data Colheira
                   hora      = ls_production-hora      "Hora Início
                   cx_colh   = ls_production-cx_colh BINARY SEARCH. "Quantidade
        IF sy-subrc EQ 0.
          <ls_output>-integrado = abap_true.
        ELSE.
          <ls_output>-integrado = abap_false.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

*--BOC- T_T.KONNO-06.04.21
*  IF gt_dados[] IS NOT INITIAL.
*    SELECT *
*      FROM yoocorr_aprov
*      INTO TABLE @DATA(lt_ocorrencias)
*      FOR ALL ENTRIES IN @gt_dados
*     WHERE pernr     = @gt_dados-pernr
*       AND data_ocor = @gt_dados-data_ocor.
*
*    SORT lt_ocorrencias BY pernr data_ocor.
*
**-- Checks Occurrences
*    LOOP AT gt_dados INTO DATA(ls_dado).
*      INSERT INITIAL LINE INTO TABLE gt_output
*        ASSIGNING <ls_output>.
*      IF sy-subrc EQ 0.
*        MOVE-CORRESPONDING ls_dado TO <ls_output>.
*        READ TABLE lt_ocorrencias INTO DATA(ls_ocorrencia)
*          WITH KEY pernr     = ls_dado-pernr
*                   data_ocor = ls_dado-data_ocor BINARY SEARCH.
*        IF sy-subrc EQ 0.
*          <ls_output>-integrado = abap_true.
*        ELSE.
*          <ls_output>-integrado = abap_false.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.
*--EOC- T_T.KONNO-06.04.21

  DATA(lt_events) = lt_prod_events[].
  DELETE lt_events WHERE zzactcg NE zcl_abs_abap_maintain=>c_actcg_event. "Events

*-- Collect HCM Events
  LOOP AT lt_events ASSIGNING <fs_prod_events>.
*-- Read HCM Resource
    CLEAR: ls_events, ls_resources,ls_dados.
    READ TABLE lt_resources INTO ls_resources
      WITH KEY idresource = <fs_prod_events>-idresource
               arbpl      = <fs_prod_events>-arbpl BINARY SEARCH.

    IF sy-subrc EQ 0.
      CALL FUNCTION 'CATS_CHECK_EMPLOYEE_ACTIVE'
        EXPORTING
          pernr            = ls_resources-pernr
          begda            = <fs_prod_events>-zzhdate
          endda            = <fs_prod_events>-zzhdate
        EXCEPTIONS
          pernr_not_found  = 1
          pernr_not_active = 2
          OTHERS           = 3.

      IF sy-subrc EQ 0.
        ls_dados-pernr = ls_resources-pernr. " Matrícula (PERNR)
      ELSE.
        CONTINUE.
      ENDIF.
    ENDIF.

    ls_dados-data_ocor = <fs_prod_events>-zzhdate." Data da Ocorrência
    ls_dados-tipo_ocor = <fs_prod_events>-idactvl." Tipo de Ocorrência - Subtipo do 2001
    ls_dados-observacao = <fs_prod_events>-zzjust.

    READ TABLE lt_hcm_form INTO ls_hcm_form
      WITH KEY fazenda_farm = ls_terrains-form BINARY SEARCH.
    IF sy-subrc EQ 0.
      DATA(lv_faz_btrtl) = ls_hcm_form-btrtl.
    ENDIF.

    IF lv_faz_btrtl IS NOT INITIAL.
      ls_dados-fazenda = lv_faz_btrtl." Fazenda (ex: FA79)
    ENDIF.

    APPEND ls_dados TO gt_dados.

*--BOC- T_T.KONNO-06.04.21
    INSERT INITIAL LINE INTO TABLE lt_dados_aux
      ASSIGNING FIELD-SYMBOL(<ls_dados_aux>).
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING <fs_prod_events> TO <ls_dados_aux>.
      <ls_dados_aux>-pernr = <fs_prod_events>-idresource.
      <ls_dados_aux>-turma = <fs_prod_events>-zzturma.
*-- Read complex of the Terrain
      READ TABLE lt_complex INTO ls_complex
            WITH KEY tplnr = <fs_prod_events>-tplnr
          BINARY SEARCH.
      IF sy-subrc EQ 0.
        <ls_dados_aux>-complex = ls_complex-complex.
      ENDIF.
*-- Read HCM Form
      CLEAR ls_terrains.
      READ TABLE lt_terrains INTO ls_terrains
            WITH KEY tplnr_fl = <fs_prod_events>-tplnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        READ TABLE lt_hcm_form INTO ls_hcm_form
              WITH KEY fazenda_farm = ls_terrains-form BINARY SEARCH.
        IF sy-subrc EQ 0.
          <ls_dados_aux>-faz_btrtl = ls_hcm_form-fazenda_farm.
        ENDIF.
        <ls_dados_aux>-begda =  <fs_prod_events>-zzhdate.
        <ls_dados_aux>-endda =  <fs_prod_events>-zzhdate.
        <ls_dados_aux>-talhao  = ls_terrains-terrain.
        <ls_dados_aux>-hora    = <fs_prod_events>-strttim.
        <ls_dados_aux>-coletor = <fs_prod_events>-accom.
        <ls_dados_aux>-awart   = <fs_prod_events>-idactvl.
        <ls_dados_aux>-zzjust = <fs_prod_events>-zzjust.
      ENDIF.
    ENDIF.
*--EOC- T_T.KONNO-06.04.21
  ENDLOOP.

*--BOC- T_T.KONNO-06.04.21
  IF gt_dados[] IS NOT INITIAL.
    SELECT *
      FROM yoocorr_aprov
      INTO TABLE @DATA(lt_ocorrencias)
      FOR ALL ENTRIES IN @gt_dados
     WHERE pernr     = @gt_dados-pernr
       AND data_ocor = @gt_dados-data_ocor.

    SORT: lt_dados_aux BY pernr,
          lt_ocorrencias BY pernr data_ocor.

*-- Checks Occurrences
    LOOP AT gt_dados INTO DATA(ls_dado).
      INSERT INITIAL LINE INTO TABLE gt_output
        ASSIGNING <ls_output>.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING ls_dado TO <ls_output>.

        READ TABLE lt_dados_aux INTO DATA(ls_dados_aux)
          WITH KEY pernr = ls_dado-pernr BINARY SEARCH.
        IF sy-subrc EQ 0.
          MOVE-CORRESPONDING ls_dados_aux TO <ls_output>.
        ENDIF.

        READ TABLE lt_ocorrencias INTO DATA(ls_ocorrencia)
          WITH KEY pernr     = ls_dado-pernr
                   data_ocor = ls_dado-data_ocor BINARY SEARCH.
        IF sy-subrc EQ 0.
          <ls_output>-integrado = abap_true.
        ELSE.
          <ls_output>-integrado = abap_false.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
*--EOC- T_T.KONNO-06.04.21

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_OUTPUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM display_output.
  CALL SCREEN 100.
ENDFORM.

*&---------------------------------------------------------------------*
*& Module STATUS_SET OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_set OUTPUT.
  PERFORM status_set.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form STATUS_SET
*&---------------------------------------------------------------------*
*& STATUS_SET
*&---------------------------------------------------------------------*
FORM status_set.
  SET PF-STATUS 'S100'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Module TITLE_SET OUTPUT
*&---------------------------------------------------------------------*
*& TITLE_SET
*&---------------------------------------------------------------------*
MODULE title_set OUTPUT.
  PERFORM title_set.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form TITLE_SET
*&---------------------------------------------------------------------*
*& TITLE_SET
*&---------------------------------------------------------------------*
FORM title_set.
  SET TITLEBAR 'T100'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Module CONTROLS_DISPLAY OUTPUT
*&---------------------------------------------------------------------*
*& CONTROLS_DISPLAY
*&---------------------------------------------------------------------*
MODULE controls_display OUTPUT.
  PERFORM controls_display.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form CONTROLS_DISPLAY
*&---------------------------------------------------------------------*
*& CONTROLS_DISPLAY
*&---------------------------------------------------------------------*
FORM controls_display.

*-- Local Declarations
  DATA: lt_fcat    TYPE lvc_t_fcat,
        ls_variant TYPE disvariant,
        ls_layout  TYPE lvc_s_layo.

  IF cl_gui_alv_grid=>offline( ) IS INITIAL. "Online - ALV Display

*--Create Object For Custom Container
    CREATE OBJECT gobj_cont
      EXPORTING
        container_name = 'ZHCM_INTG_100CC'.

*--Create Object for ALV Grid
    CREATE OBJECT gobj_grid
      EXPORTING
        i_parent = gobj_cont.

  ELSE.                                     ""Offline - ALV in Spool

    CREATE OBJECT gobj_grid
      EXPORTING
        i_parent = gobj_cont.

  ENDIF.

*-- Field Catalog
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZABS_STR_INTEGRA_HCM'
    CHANGING
      ct_fieldcat            = lt_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*--Set ALV attributes FOR LAYOUT
  ls_layout-cwidth_opt = abap_true.
  ls_layout-zebra = abap_true.
  ls_layout-sel_mode = 'A'.

  ls_variant-report = sy-repid.

*--Displaying ALV Data
  IF gobj_grid IS NOT INITIAL.
    CALL METHOD gobj_grid->set_table_for_first_display
      EXPORTING
        is_variant                    = ls_variant
        i_save                        = 'A'
        is_layout                     = ls_layout
      CHANGING
        it_outtab                     = gt_output
        it_fieldcatalog               = lt_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*& USER_COMMAND_0100
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  IF sy-ucomm = 'BACK'.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*& Form FILL_DEFAULT_VALUES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fill_default_values .

  IF so_date[] IS INITIAL.
    INSERT INITIAL LINE INTO TABLE so_date
      ASSIGNING FIELD-SYMBOL(<ls_date>).
    IF sy-subrc EQ 0.
      <ls_date>-low = <ls_date>-high = sy-datum.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_OBLIGATORY_PARAMETERS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_obligatory_parameters .

  IF so_date[] IS INITIAL.
*-- O parâmetro Data é de preenchimento obrigatório!
    MESSAGE i329(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.
