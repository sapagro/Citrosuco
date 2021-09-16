*&---------------------------------------------------------------------*
*& Include ZABS_INC_MAPA_CARENCIA_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form INITIALIZE_GLOBAL_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM initialize_global_data .

  REFRESH: gt_farm, gt_glflot, gt_mara, gt_fmfphdr,
           gt_fmfpbch, gt_fmfpitm, gt_fmfpcom,
           gt_fmocindx, gt_fmocopr,  gt_fmoccom, gt_afwi,
           gt_matdoc, gt_output.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form VALIDATE_PARAMETERS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM validate_parameters .

  IF p_iwerk IS INITIAL.
*--Informe a Fazenda!
    MESSAGE i417(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF s_date[] IS INITIAL.
*--Informe a Data!
    MESSAGE i418(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_TERRAINS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_terrains .

  DATA: lt_mat_class TYPE TABLE OF bapi1003_alloc_list,
        lt_val_char  TYPE TABLE OF bapi1003_alloc_values_char,
        lt_val_curr  TYPE TABLE OF bapi1003_alloc_values_curr,
        lt_val_num   TYPE TABLE OF bapi1003_alloc_values_num,
        lt_return    TYPE TABLE OF bapiret2,
        lrt_matnr    TYPE RANGE OF matnr,
        lrs_matnr    LIKE LINE OF lrt_matnr,
        lrt_budat    TYPE gy_budat_range,
        lrt_matnr_ex TYPE gy_matnr_range,
        ls_fmfphdr   LIKE LINE OF gt_fmfphdr,
        ls_fmfpitm   LIKE LINE OF gt_fmfpitm,
        ls_glflot    LIKE LINE OF gt_glflot,
        ls_farm      LIKE LINE OF gt_farm.

  CONSTANTS: lc_obj_mara TYPE bapi1003_key-objecttable VALUE 'MARA',
             lc_typ_001  TYPE bapi1003_key-classtype VALUE '001'.

  IF sy-uname EQ 'T_H.KABABE'.
    BREAK-POINT.
  ENDIF.

*--Check material to exclude
  PERFORM matnr_excludes_prepare CHANGING lrt_matnr_ex.

  SELECT m~matnr, m~mtart, m~matkl, t~maktx
    FROM mara AS m
    INNER JOIN makt AS t
    ON m~matnr = t~matnr
    INTO TABLE @gt_mara
   WHERE m~mtart EQ 'ZA00'
     AND t~spras EQ 'P'.

  IF lrt_matnr_ex[] IS NOT INITIAL.
    DELETE gt_mara WHERE matnr IN lrt_matnr_ex[].
  ENDIF.

  IF gt_mara[] IS NOT INITIAL.
    SORT gt_mara BY matnr.
    lrt_matnr = CORRESPONDING #( gt_mara MAPPING low = matnr ).
    lrs_matnr = 'IEQ'.
    MODIFY lrt_matnr FROM lrs_matnr TRANSPORTING sign option WHERE low <> ''.
  ENDIF.

*--Get terrain
  SELECT tplnr_fl, pltxt, tplkz, tplvl, tplma, bukrs,
         iwerk, swerk, kokrs, anlnr, kostl, ownshp
    FROM /agri/glflot
    INTO TABLE @gt_farm
   WHERE iwerk EQ @p_iwerk
     AND tplkz EQ 'CROP'
     AND tplvl EQ '1'.
  IF sy-subrc NE 0.
*--Não existem fazendas para os centros informados!
    MESSAGE i419(zfmfp).
    LEAVE LIST-PROCESSING.
  ELSE.
    SORT gt_farm BY tplnr_fl.

    SELECT tplnr_fl, pltxt, tplkz, tplvl, tplma, bukrs,
           iwerk, swerk, kokrs, anlnr, kostl, ownshp
        FROM /agri/glflot
        INTO TABLE @gt_glflot
        FOR ALL ENTRIES IN @gt_farm
       WHERE iwerk EQ @gt_farm-iwerk
         AND tplma EQ @gt_farm-tplnr_fl
         AND tplkz EQ 'CROP'
         AND tplvl EQ '2'.

    IF s_tplnr[] IS NOT INITIAL.
      DELETE gt_glflot WHERE tplnr_fl NOT IN s_tplnr[].

      IF gt_glflot[] IS INITIAL.
*--Talhões informados inválidos!
        MESSAGE i420(zfmfp).
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.
  ENDIF.

*--Fetch data from farm process header
  IF gt_glflot[] IS NOT INITIAL.
    SORT gt_glflot BY tplma tplnr_fl.

    SELECT * FROM /agri/fmfphdr
      INTO TABLE @gt_fmfphdr
      FOR ALL ENTRIES IN @gt_glflot
     WHERE autyp    EQ @c_document_category-task_order
       AND tplnr_fl EQ @gt_glflot-tplnr_fl
       AND class    EQ @c_application-farming
       AND matnr    IN @lrt_matnr[]
       AND gstrp    IN @s_date[]
       AND iwerk    EQ @gt_glflot-iwerk
       AND tecom    EQ @abap_false.

    IF gt_fmfphdr[] IS NOT INITIAL.
      SORT gt_fmfphdr BY aufnr.

*--Fetch Farm Process Order Batches
      SELECT * FROM /agri/fmfpbch
        INTO TABLE @gt_fmfpbch
        FOR ALL ENTRIES IN @gt_fmfphdr
       WHERE aufnr = @gt_fmfphdr-aufnr.
      IF sy-subrc EQ 0.
        SORT gt_fmfpbch BY aufnr contr.
      ENDIF.

*--Fetch data from farm process items
      SELECT * FROM /agri/fmfpitm
        INTO TABLE @gt_fmfpitm
        FOR ALL ENTRIES IN @gt_fmfphdr
       WHERE aufnr EQ @gt_fmfphdr-aufnr.

      IF sy-subrc EQ 0.
        SORT gt_fmfpitm BY aufnr posnr.

*--Fecth Farm Process Order Components
        SELECT * FROM /agri/fmfpcom
          INTO TABLE @gt_fmfpcom
          FOR ALL ENTRIES IN @gt_fmfpitm
         WHERE aufnr EQ @gt_fmfpitm-aufnr.

        SORT gt_fmfpcom BY aufnr posnr.

*--Get order confirmation
        SELECT * FROM /agri/fmocindx
          INTO TABLE @gt_fmocindx
          FOR ALL ENTRIES IN @gt_fmfpitm
         WHERE aufnr EQ @gt_fmfpitm-aufnr.

        IF sy-subrc EQ 0.
          SORT gt_fmocindx BY aufnr.

*--Get operations based on confirmation
          SELECT ocnum, rueck, rmzhl, aufnr, posnr,
                 vornr, ltxa1, lmnga, meinh, budat
            FROM /agri/fmocopr
            INTO TABLE @gt_fmocopr
            FOR ALL ENTRIES IN @gt_fmocindx
           WHERE ocnum EQ @gt_fmocindx-ocnum
             AND budat IN @lrt_budat[].

          IF sy-subrc EQ 0.
            SORT gt_fmocopr BY ocnum aufnr.

            SELECT * FROM /agri/fmoccom
              INTO TABLE @gt_fmoccom
              FOR ALL ENTRIES IN @gt_fmocopr
             WHERE ocnum EQ @gt_fmocopr-ocnum
               AND rueck EQ @gt_fmocopr-rueck
               AND rmzhl EQ @gt_fmocopr-rmzhl.
            IF sy-subrc EQ 0.
              SORT gt_fmoccom BY ocnum rueck rmzhl contr.

              SELECT m~matnr, m~mtart, m~matkl, t~maktx
                FROM mara AS m
                INNER JOIN makt AS t
                ON m~matnr = t~matnr
                APPENDING TABLE @gt_mara
                FOR ALL ENTRIES IN @gt_fmoccom
               WHERE m~matnr EQ @gt_fmoccom-matnr
                 AND t~spras EQ 'P'.
              IF sy-subrc EQ 0.
                SORT gt_mara BY matnr.
                DELETE ADJACENT DUPLICATES FROM gt_mara COMPARING matnr.
              ENDIF.
            ENDIF.

*--Get posted goods data
            SELECT * FROM afwi
              INTO TABLE @gt_afwi
              FOR ALL ENTRIES IN @gt_fmocopr
             WHERE rueck EQ @gt_fmocopr-rueck
               AND rmzhl EQ @gt_fmocopr-rmzhl.

            IF sy-subrc EQ 0.
              SORT gt_afwi BY rueck rmzhl.

*--Get posting date
              SELECT budat, cpudt, cputm, mblnr, mjahr,
                     zeile, aufnr, anln1, anln2, bwart
                FROM matdoc
                INTO TABLE @gt_matdoc
                FOR ALL ENTRIES IN @gt_afwi
               WHERE mblnr      EQ @gt_afwi-mblnr
                 AND mjahr      EQ @gt_afwi-mjahr
                 AND zeile      EQ @gt_afwi-mblpo
                 AND cancelled  EQ @space
                 AND bwart      EQ @c_move_type-goods_receipt.

              SORT gt_matdoc BY aufnr.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  SORT: gt_glflot BY tplma tplnr_fl,
        gt_farm BY tplnr_fl,
        gt_fmfphdr BY tplnr_fl.

  LOOP AT gt_glflot INTO ls_glflot.
    CLEAR: ls_farm, ls_fmfphdr, ls_fmfpitm.

    DATA(lv_planned) = abap_false.
    READ TABLE gt_fmfphdr INTO ls_fmfphdr
      WITH KEY tplnr_fl = ls_glflot-tplnr_fl BINARY SEARCH.
    WHILE sy-subrc EQ 0.
      DATA(lv_tabix) = sy-tabix + 1.
      lv_planned = abap_true.

      INSERT INITIAL LINE INTO TABLE gt_output
        ASSIGNING FIELD-SYMBOL(<ls_output>).
      IF sy-subrc EQ 0.
        CLEAR ls_fmfpitm.
        READ TABLE gt_fmfpitm INTO ls_fmfpitm
          WITH KEY aufnr = ls_fmfphdr-aufnr BINARY SEARCH.
        IF sy-subrc EQ 0.
          <ls_output>-color  = c_color-planned.
          <ls_output>-status = c_status-planned.
        ENDIF.

        CLEAR ls_farm.
        READ TABLE gt_farm INTO ls_farm
          WITH KEY tplnr_fl = ls_glflot-tplma BINARY SEARCH.

        <ls_output>-tplnr_fl       = ls_glflot-tplnr_fl.
        <ls_output>-pltxt          = ls_glflot-pltxt.
        <ls_output>-tplkz          = ls_glflot-tplkz.
        <ls_output>-tplvl          = ls_glflot-tplvl.
        <ls_output>-tplma          = ls_glflot-tplma.
        <ls_output>-bukrs          = ls_glflot-bukrs.
        <ls_output>-iwerk          = ls_glflot-iwerk.
        <ls_output>-swerk          = ls_glflot-swerk.
        <ls_output>-kokrs          = ls_glflot-kokrs.
        <ls_output>-anlnr          = ls_glflot-anlnr.
        <ls_output>-kostl          = ls_glflot-kostl.
        <ls_output>-ownshp         = ls_glflot-ownshp.
        <ls_output>-tarefa         = ls_fmfphdr-matnr.
        <ls_output>-data_inicio    = ls_fmfphdr-gstrp.
        <ls_output>-data_fim       = ls_fmfphdr-gltrp.
        <ls_output>-aufnr_to       = ls_fmfpitm-aufnr.

        READ TABLE gt_mara INTO DATA(ls_mara)
          WITH KEY matnr = <ls_output>-tarefa BINARY SEARCH.
        IF sy-subrc EQ 0.
          <ls_output>-tarefa_txt = ls_mara-maktx.
        ENDIF.
      ENDIF.

      READ TABLE gt_fmfphdr INTO ls_fmfphdr
        INDEX lv_tabix COMPARING tplnr_fl.
    ENDWHILE.

    IF lv_planned EQ abap_false.
      INSERT INITIAL LINE INTO TABLE gt_output
        ASSIGNING <ls_output>.
      IF sy-subrc EQ 0.
        <ls_output>-tplnr_fl = ls_glflot-tplnr_fl.
        <ls_output>-pltxt    = ls_glflot-pltxt.
        <ls_output>-tplkz    = ls_glflot-tplkz.
        <ls_output>-tplvl    = ls_glflot-tplvl.
        <ls_output>-tplma    = ls_glflot-tplma.
        <ls_output>-bukrs    = ls_glflot-bukrs.
        <ls_output>-iwerk    = ls_glflot-iwerk.
        <ls_output>-swerk    = ls_glflot-swerk.
        <ls_output>-kokrs    = ls_glflot-kokrs.
        <ls_output>-anlnr    = ls_glflot-anlnr.
        <ls_output>-kostl    = ls_glflot-kostl.
        <ls_output>-ownshp   = ls_glflot-ownshp.
        <ls_output>-color    = c_color-not_planned.
        <ls_output>-status   = c_status-not_planned.
      ENDIF.
    ENDIF.
  ENDLOOP.

  SORT gt_output BY tplnr_fl aufnr_to tarefa.

  DELETE ADJACENT DUPLICATES FROM gt_output
    COMPARING tplnr_fl aufnr_to tarefa.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_data .

  IF p_alv EQ abap_true.
    CALL SCREEN 100.
  ELSEIF p_map EQ abap_true.
    CALL SCREEN 200.
  ENDIF.

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

  DATA: lt_fcode_excludes TYPE ui_functions.

  PERFORM fcode_excludes_prepare CHANGING lt_fcode_excludes.

  CASE sy-dynnr.
    WHEN c_screen-overview.
      SET PF-STATUS 'S100' EXCLUDING lt_fcode_excludes.
    WHEN c_screen-map.
      SET PF-STATUS 'S200' EXCLUDING lt_fcode_excludes.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Module TITLE_SET OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE title_set OUTPUT.
  PERFORM title_set.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form TITLE_SET
*&---------------------------------------------------------------------*
*& TITLE_SET
*&---------------------------------------------------------------------*
FORM title_set .

  CASE sy-dynnr.
    WHEN c_screen-overview.
      SET TITLEBAR 'T100'.
    WHEN c_screen-map.
      SET TITLEBAR 'T200'.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Module CONTROLS_DISPLAY OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE controls_display OUTPUT.
  PERFORM controls_display.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form CONTROLS_DISPLAY
*&---------------------------------------------------------------------*
*& Display Controls
*&---------------------------------------------------------------------*
FORM controls_display.

  DATA : lv_routine(21) VALUE 'CONTROLS_DISPLAY_'.

  CONCATENATE lv_routine sy-dynnr INTO lv_routine.
  PERFORM (lv_routine) IN PROGRAM zabs_rep_mapa_operacoes IF FOUND.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CONTROLS_DISPLAY_0100
*&---------------------------------------------------------------------*
*& Display Controls
*&---------------------------------------------------------------------*
FORM controls_display_0100.

*--Local table declaration
  DATA: lt_fcat    TYPE lvc_t_fcat,
*--Workarea declaration
        ls_variant TYPE disvariant,
        ls_stable  TYPE lvc_s_stbl,
        ls_layout  TYPE lvc_s_layo,
*--Variables
        lv_valid   TYPE char01,
        lv_input   TYPE i.

*--Building field catalog
  PERFORM field_catalog_prepare.

*--Set ALV attributes for layout
  ls_layout-cwidth_opt = abap_true.
  ls_layout-zebra      = abap_true.
  ls_layout-sel_mode   = zcl_abs_abap_maintain=>c_layout_sel_mode.  "'A'
  ls_layout-smalltitle = abap_true.

  IF gobj_alv IS NOT BOUND.
*--Create Object for custom container
    CREATE OBJECT gobj_cont
      EXPORTING
        container_name = 'C_OPERACOES_0100_CC'.

*--Create Object for ALV Grid
    CREATE OBJECT gobj_alv
      EXPORTING
        i_parent = gobj_cont.
  ENDIF.

*--Displaying ALV Data
  IF gobj_alv IS NOT INITIAL.
    IF gt_output[] IS NOT INITIAL.
      ls_variant-report = sy-repid.
      CALL METHOD gobj_alv->set_table_for_first_display
        EXPORTING
          is_variant                    = ls_variant
          i_save                        = 'A'
          is_layout                     = ls_layout
        CHANGING
          it_outtab                     = gt_output
          it_fieldcatalog               = gt_fcat
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.
      IF sy-subrc <> 0.
*--Combinação de parâmetros inválida para exibição
        MESSAGE i035(zabs_msgcls).
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CONTROLS_DISPLAY_0200
*&---------------------------------------------------------------------*
*& Display Controls
*&---------------------------------------------------------------------*
FORM controls_display_0200.

  IF sy-uname EQ 'T_H.KABABE'.
    BREAK-POINT.
  ENDIF.

  IF html_container IS INITIAL.
    CREATE OBJECT html_container
      EXPORTING
        container_name = 'HTML2'
      EXCEPTIONS
        OTHERS         = 1.

    IF sy-subrc NE 0.
      RAISE cntl_error.
    ENDIF.
  ENDIF.

  IF html_control IS INITIAL.
    ui_flag = cl_gui_html_viewer=>uiflag_no3dborder +
              cl_gui_html_viewer=>uiflag_noiemenu.
    cb_no3d = 'X'.
    cb_noiectx = 'X'.

    CREATE OBJECT html_control
      EXPORTING
        parent   = html_container
        saphtmlp = 'X'
        uiflag   = ui_flag
        lifetime = cl_gui_html_viewer=>lifetime_dynpro.

    IF sy-subrc NE 0.
      RAISE cntl_error.
    ENDIF.

    alignment = html_control->align_at_left +
                html_control->align_at_right +
                html_control->align_at_top +
                html_control->align_at_bottom.

    CALL METHOD html_control->set_alignment
      EXPORTING
        alignment = alignment.

    html_event-appl_event = 'X'.
    html_event-eventid = html_control->m_id_navigate_complete.
    APPEND html_event TO html_event_tab.

    html_event-eventid = html_control->m_id_ctxmenu_request.
    APPEND html_event TO html_event_tab.

    html_event-eventid = html_control->m_id_ctxmenu_selected.
    APPEND html_event TO html_event_tab.
    CALL METHOD html_control->set_registered_events
      EXPORTING
        events = html_event_tab.

    CREATE OBJECT evt_receiver.

    SET HANDLER evt_receiver->on_navigate_complete
                FOR html_control.

    SET HANDLER evt_receiver->on_ctxmenu_request
                FOR html_control.

    SET HANDLER evt_receiver->on_ctxmenu_selected
                FOR html_control.

    PERFORM load_map.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FIELD_CATALOG_PREPARE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM field_catalog_prepare .

  DATA: lr_tabdescr TYPE REF TO cl_abap_structdescr,
        lr_data     TYPE REF TO data,
        lt_dfies    TYPE ddfields.

  CREATE DATA lr_data LIKE LINE OF gt_output.
  lr_tabdescr ?= cl_abap_structdescr=>describe_by_data_ref( lr_data ).
  lt_dfies = cl_salv_data_descr=>read_structdescr( lr_tabdescr ).

  LOOP AT lt_dfies INTO DATA(ls_dfies).
    INSERT INITIAL LINE INTO TABLE gt_fcat
      ASSIGNING FIELD-SYMBOL(<ls_fcat>).
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING ls_dfies TO <ls_fcat>.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  FCODE_PROCESSING  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE fcode_processing INPUT.
  PERFORM fcode_processing.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form FCODE_PROCESSING
*&---------------------------------------------------------------------*
*& FCODE_PROCESSING
*&---------------------------------------------------------------------*
FORM fcode_processing.

  DATA : lv_subroutine(40) TYPE c VALUE 'FCODE_'.

  fcode = ok_code.
  CLEAR: ok_code.
  CONCATENATE lv_subroutine fcode INTO lv_subroutine.
  PERFORM (lv_subroutine) IN PROGRAM (sy-repid)
                          IF FOUND.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FCODE_BACK
*&---------------------------------------------------------------------*
*& For Back Button
*&---------------------------------------------------------------------*
FORM fcode_back.
  SET SCREEN 0.
  LEAVE SCREEN.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form FCODE_CANC
*&---------------------------------------------------------------------*
*& For Cancel Button
*&---------------------------------------------------------------------*
FORM fcode_canc.
  SET SCREEN 0.
  LEAVE SCREEN.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form FCODE_EXIT
*&---------------------------------------------------------------------*
*& For Exit Button
*&---------------------------------------------------------------------*
FORM fcode_exit.
  SET SCREEN 0.
  LEAVE SCREEN.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHANGE_SCREEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM change_screen .

*  LOOP AT SCREEN.
*    IF p_enh EQ abap_true.
*      IF screen-group1 = 'ID2'.
*        screen-invisible = 0.
*        screen-input     = 1.
*        screen-active    = 1.
*      ENDIF.
*    ELSEIF p_enh EQ abap_false.
*      IF screen-group1 = 'ID2'.
*        screen-invisible = 1.
*        screen-input     = 0.
*        screen-active    = 0.
*      ENDIF.
*    ENDIF.
*    MODIFY SCREEN.
*  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_GET_F4_ON_TERRAIN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- S_TPLNR_LOW
*&---------------------------------------------------------------------*
FORM f_get_f4_on_terrain CHANGING lv_tplnr.

  DATA: lt_dynpfields TYPE STANDARD TABLE OF dynpread,
        lt_return     TYPE STANDARD TABLE OF ddshretval,
        lv_iwerk      TYPE iwerk.

*--Read dynpro data for Farm
  INSERT INITIAL LINE INTO TABLE lt_dynpfields
    ASSIGNING FIELD-SYMBOL(<ls_dynpfield>).
  IF sy-subrc EQ 0.
    <ls_dynpfield>-fieldname = 'P_IWERK'.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname             = 'ZABS_REP_MAPA_OPERACOES'
        dynumb             = sy-dynnr
        translate_to_upper = 'X'
      TABLES
        dynpfields         = lt_dynpfields.

    IF sy-subrc EQ 0.
      READ TABLE lt_dynpfields INTO DATA(ls_dynpfield) INDEX 1.
      IF sy-subrc EQ 0.
        lv_iwerk = ls_dynpfield-fieldvalue.
      ENDIF.
    ENDIF.
  ENDIF.

*--For terrain, get the possible F4 ------
  IF lv_iwerk IS NOT INITIAL.
    SELECT tplnr_fl, pltxt, tplma, bukrs,
           iwerk, swerk, ownshp
      FROM /agri/glflot
      INTO TABLE @DATA(lt_farm)
     WHERE iwerk EQ @lv_iwerk
       AND tplkz EQ 'CROP'
       AND tplvl EQ '1'.
  ELSE.
    SELECT tplnr_fl, pltxt, tplma, bukrs,
           iwerk, swerk, ownshp
      FROM /agri/glflot
      INTO TABLE @lt_farm
     WHERE iwerk EQ @p_iwerk
       AND tplkz EQ 'CROP'
       AND tplvl EQ '1'.
  ENDIF.

  IF lt_farm[] IS NOT INITIAL.
    SELECT tplnr_fl, pltxt, tplma, bukrs,
           iwerk, swerk, ownshp
        FROM /agri/glflot
        INTO TABLE @DATA(lt_data_sh)
        FOR ALL ENTRIES IN @lt_farm
       WHERE iwerk EQ @lt_farm-iwerk
         AND tplma EQ @lt_farm-tplnr_fl
         AND tplkz EQ 'CROP'
         AND tplvl EQ '2'.
  ENDIF.

  IF lt_data_sh[] IS NOT INITIAL.
*--Show F4 with the prepared data
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'TPLNR_FL'
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        dynprofield     = 'S_TPLNR'
        value_org       = 'S'
      TABLES
        value_tab       = lt_data_sh
        return_tab      = lt_return
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
*--Update the internal table from the selected item from Pop up ------
      READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
      IF sy-subrc EQ 0.
        lv_tplnr = ls_return-fieldval.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*& Form MATNR_EXCLUDES_PREPARE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LRT_MATNR
*&---------------------------------------------------------------------*
FORM matnr_excludes_prepare CHANGING lrt_matnr TYPE gy_matnr_range.

  DATA: lt_constants TYPE zabs_tty_vkey_const,
        lv_objid     TYPE zabs_del_objid VALUE 'GIS',
        lv_k1val     TYPE zabs_del_k1val VALUE 'OPERACOES',
        lv_k2val     TYPE zabs_del_k2val VALUE 'EXCLUDE'.

  REFRESH lrt_matnr.

  CALL METHOD zcl_abs_get_variants=>get_constant_multiple
    EXPORTING
      iv_objid     = lv_objid "GIS
      iv_k1val     = lv_k1val "OPERACOES
      iv_k2val     = lv_k2val "EXCLUDE
    IMPORTING
      et_constants = lt_constants.

  LOOP AT lt_constants INTO DATA(ls_constant)
    WHERE cnval1 = '01'.
    CONDENSE ls_constant-cnval2 NO-GAPS.
    INSERT INITIAL LINE INTO TABLE lrt_matnr
      ASSIGNING FIELD-SYMBOL(<lrs_matnr>).
    IF sy-subrc EQ 0.
      <lrs_matnr> = 'IEQ'.
      <lrs_matnr>-low = ls_constant-cnval2.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FCODE_EXCLUDES_PREPARE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_FCODE_EXCLUDES
*&---------------------------------------------------------------------*
FORM fcode_excludes_prepare CHANGING lt_fcode_excludes TYPE ui_functions.



ENDFORM.

*&---------------------------------------------------------------------*
*& Form LOAD_MAP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM load_map .

  DATA: lt_map_data      TYPE STANDARD TABLE OF zabs_map_type1 INITIAL SIZE 0,
        lt_operacoes     TYPE STANDARD TABLE OF zabs_map_type2 INITIAL SIZE 0,
        lv_html          TYPE string,
        lv_html_header   TYPE string,
        lv_html_body     TYPE string,
        lv_html_task     TYPE string,
        lv_data_inicio   TYPE char10,
        lv_map_fdname    TYPE fieldname,
        lv_alv_fdname    TYPE fieldname,
        lv_conv_date(10),
        lv_url           TYPE string.

  IF sy-uname EQ 'T_H.KABABE'.
    BREAK-POINT.
  ENDIF.

*--Building field catalog
  PERFORM field_catalog_prepare.

  SORT gt_fcat BY fieldname.

  DATA(lt_terrains) = gt_output[].
  SORT lt_terrains BY tplnr_fl.
  DELETE ADJACENT DUPLICATES FROM lt_terrains COMPARING tplnr_fl.
  SORT gt_output BY tplnr_fl tarefa.

  LOOP AT lt_terrains INTO DATA(ls_terrain).
    DATA(lv_tplnr_fl) = ls_terrain-tplnr_fl.
    CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
      EXPORTING
        input  = ls_terrain-tplnr_fl
      IMPORTING
        output = ls_terrain-tplnr_fl.
    CONDENSE ls_terrain-tplnr_fl NO-GAPS.

    INSERT INITIAL LINE INTO TABLE lt_operacoes
      ASSIGNING FIELD-SYMBOL(<ls_operacao>).
    IF sy-subrc EQ 0.
      <ls_operacao>-tplnr_fl = ls_terrain-tplnr_fl.
      <ls_operacao>-color    = ls_terrain-color.

      CONCATENATE '<html><head><style>'
              'table, th, td {'
              '  border: 1px solid black;'
              '}'
              '</style></head><body style=''500px''>'
               '<h3 style="text-align: center;">Talhão%' ls_terrain-tplnr_fl '</h3>'
              '<table width=''500px'' style=''border-collapse:collapse; table-layout:fixed; max-width: 500px;''>'
              '<tr><th colspan=''2''><img src=''img/cit.png''/></th></tr>'
              '<tr><th>Tarefa</th><th>Data Ínicio</th></tr>' INTO lv_html_header.
      TRANSLATE lv_html_header USING '% '.

      CLEAR lv_html_body.
      READ TABLE gt_output INTO DATA(ls_output)
        WITH KEY tplnr_fl = lv_tplnr_fl BINARY SEARCH.
      WHILE sy-subrc EQ 0.
        DATA(lv_tabix) = sy-tabix + 1.
        IF ls_output-data_inicio IS NOT INITIAL.
          lv_data_inicio = |{ ls_output-data_inicio DATE = ENVIRONMENT }|.
        ELSE.
          CLEAR lv_data_inicio.
        ENDIF.

        CONCATENATE '<tr><td style="text-align: center;">' ls_output-tarefa_txt
          '</td><td style="text-align: center;">' lv_data_inicio '</td></tr>' INTO lv_html_task.
        lv_html_body = lv_html_body && lv_html_task.
        READ TABLE gt_output INTO ls_output
          INDEX lv_tabix COMPARING tplnr_fl.
      ENDWHILE.

      lv_html = lv_html_header && lv_html_body && '</table></body></html>'.
      <ls_operacao>-table = lv_html.
    ENDIF.
  ENDLOOP.

  IF lt_operacoes[] IS NOT INITIAL.
    READ TABLE gt_farm INTO DATA(ls_farm) INDEX 1.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
        EXPORTING
          input  = ls_farm-tplnr_fl
        IMPORTING
          output = ls_farm-tplnr_fl.

*--Loading map based on function
      CALL FUNCTION 'ZABS_MAP_API_SAVE'
        EXPORTING
          i_farm  = ls_farm-tplnr_fl
          i_type  = '2'
          i_table = lt_operacoes
        IMPORTING
          e_url   = lv_url
        TABLES
          t_tab   = lt_map_data.

      IF lv_url IS NOT INITIAL.
        edurl = lv_url.
        CALL METHOD html_control->show_url
          EXPORTING
            url = edurl.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  fcode = ok_code.
  CLEAR: ok_code.

  CASE fcode.
    WHEN 'BACK'
      OR 'CANC'
      OR 'EXIT'.
      IF NOT html_control IS INITIAL.
        CALL METHOD html_control->free.
        FREE html_control.
      ENDIF.

      IF NOT html_container IS INITIAL.
        CALL METHOD html_container->free
          EXCEPTIONS
            OTHERS = 1.

        IF sy-subrc NE 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        FREE html_container.
      ENDIF.

      SET SCREEN 0.
      LEAVE SCREEN.

    WHEN 'HHOM'.
      CALL METHOD html_control->go_home.

      CALL METHOD html_control->get_current_url
        IMPORTING
          url = edurl.

    WHEN 'HBAK'.
      CALL METHOD html_control->go_back.

      CALL METHOD html_control->get_current_url
        IMPORTING
          url = edurl.

    WHEN 'HFWD'.
      CALL METHOD html_control->go_forward.

      CALL METHOD html_control->get_current_url
        IMPORTING
          url = edurl.

    WHEN 'HRFR'.
      CALL METHOD html_control->do_refresh.

      CALL METHOD html_control->get_current_url
        IMPORTING
          url = edurl.

    WHEN 'NO3D'.
      PERFORM set_ui_flags.

    WHEN 'NOIECTX'.
      PERFORM set_ui_flags.

    WHEN OTHERS.
      CALL METHOD cl_gui_cfw=>dispatch.
  ENDCASE.

ENDMODULE.

*---------------------------------------------------------------------*
*       FORM set_ui_flags                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM set_ui_flags.

  ui_flag = 0.
  IF NOT cb_no3d IS INITIAL.
    ui_flag = cl_gui_html_viewer=>uiflag_no3dborder.
  ENDIF.

  IF NOT cb_noiectx IS INITIAL.
    ui_flag = ui_flag + cl_gui_html_viewer=>uiflag_noiemenu.
  ENDIF.

  CALL METHOD html_control->set_ui_flag
    EXPORTING
      uiflag = ui_flag.

ENDFORM.
