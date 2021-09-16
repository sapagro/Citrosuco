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

  REFRESH: gt_farm, gt_glflot, gt_mara, gt_fmfpitm, gt_fmfpcom,
           gt_fmocindx, gt_matdoc, gt_output.

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

  IF p_date IS INITIAL.
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

  TYPES: BEGIN OF ly_reentrada,
           matnr     TYPE matnr,
           reentrada TYPE i,
         END OF ly_reentrada.

  DATA: lt_mat_class TYPE TABLE OF bapi1003_alloc_list,
        lt_val_char  TYPE TABLE OF bapi1003_alloc_values_char,
        lt_val_curr  TYPE TABLE OF bapi1003_alloc_values_curr,
        lt_val_num   TYPE TABLE OF bapi1003_alloc_values_num,
        lt_reentrada TYPE STANDARD TABLE OF ly_reentrada INITIAL SIZE 0,
        lt_return    TYPE TABLE OF bapiret2,
        lrt_matnr    TYPE RANGE OF matnr,
        lrs_matnr    LIKE LINE OF lrt_matnr,
        lrt_budat    TYPE gy_budat_range,
        lrt_matnr_ex TYPE gy_matnr_range,
        ls_fmfpcom   LIKE LINE OF gt_fmfpcom,
        ls_glflot    LIKE LINE OF gt_glflot,
        ls_farm      LIKE LINE OF gt_farm,
        lv_reentrada TYPE i.

  CONSTANTS: lc_obj_mara TYPE bapi1003_key-objecttable VALUE 'MARA',
             lc_typ_001  TYPE bapi1003_key-classtype VALUE '001'.

  IF sy-uname EQ 'T_H.KABABE'.
    BREAK-POINT.
  ENDIF.

*--Check restriction period
  PERFORM check_restriction_period CHANGING lrt_budat.

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

*--Fetch data from farm process items and batches
    SELECT h~aufnr, h~auart, h~autyp, h~tplnr_fl, h~contr,
           h~tplma, h~cmnum, h~varia, h~cpros, h~class,
           h~matnr, h~iwerk, h~tecom, i~posnr, i~vornr,
           i~ltxa1, i~rueck, i~arbpl, i~aufnr_to,
           b~contr AS contr_bch, b~charg, b~pmatnr,
           b~erfmg, b~gwemg, b~erfme
      FROM /agri/fmfphdr AS h
      INNER JOIN /agri/fmfpitm AS i
      ON h~aufnr EQ i~aufnr
      LEFT OUTER JOIN /agri/fmfpbch AS b
      ON b~aufnr = i~aufnr
      INTO TABLE @gt_fmfpitm
      FOR ALL ENTRIES IN @gt_glflot
     WHERE h~autyp    EQ @c_document_category-task_order
       AND h~tplnr_fl EQ @gt_glflot-tplnr_fl
       AND h~class    EQ @c_application-farming
       AND h~matnr    IN @lrt_matnr[]
       AND h~iwerk    EQ @gt_glflot-iwerk
       AND h~tecom    EQ @abap_false.

    IF sy-subrc EQ 0.
      SORT gt_fmfpitm BY aufnr posnr.

*--Fecth Farm Process Order Components
      SELECT * FROM /agri/fmfpcom
        INTO TABLE @gt_fmfpcom
        FOR ALL ENTRIES IN @gt_fmfpitm
       WHERE aufnr EQ @gt_fmfpitm-aufnr.

      SORT gt_fmfpcom BY aufnr posnr.

      IF sy-uname EQ 'T_H.KABABE'.
        BREAK-POINT.
      ENDIF.

*--Get operations based on order confirmation
      SELECT h~tplnr_fl, i~aufnr, i~ocnum, i~gwemg, i~gmein,
             o~rueck, o~rmzhl, o~posnr, o~vornr, o~ltxa1,
             o~meinh, o~budat, c~contr, c~rspos, c~matnr,
             c~lmnga, c~erfme, c~lgort, c~bwart, c~charg
        INTO TABLE @gt_fmocindx
        FROM /agri/fmfphdr AS h
        INNER JOIN /agri/fmocindx AS i
        ON h~aufnr EQ i~aufnr
        INNER JOIN /agri/fmocopr AS o
        ON  o~ocnum EQ i~ocnum
        AND o~aufnr EQ i~aufnr
        INNER JOIN /agri/fmoccom AS c
        ON  c~ocnum EQ o~ocnum
        AND c~rueck EQ o~rueck
        AND c~rmzhl EQ o~rmzhl
        FOR ALL ENTRIES IN @gt_fmfpitm
       WHERE i~aufnr EQ @gt_fmfpitm-aufnr
         AND o~budat IN @lrt_budat[].

      IF sy-subrc EQ 0.
        SELECT m~matnr, a~atwrt, a~atflv
          FROM mara AS m
          LEFT OUTER JOIN ausp AS a
          ON a~objek = m~matnr
          AND a~klart = '001'
          INNER JOIN cabn AS c
          ON c~atnam = 'FAZ_DIAS_REENTRADA'
          AND a~atinn = c~atinn
          INTO TABLE @DATA(lt_ausp)
          FOR ALL ENTRIES IN @gt_fmocindx
         WHERE m~matnr = @gt_fmocindx-matnr.

        SORT lt_ausp BY matnr.

        SELECT m~matnr, m~mtart, m~matkl, t~maktx
          FROM mara AS m
          INNER JOIN makt AS t
          ON m~matnr = t~matnr
          APPENDING TABLE @gt_mara
          FOR ALL ENTRIES IN @gt_fmocindx
         WHERE m~matnr EQ @gt_fmocindx-matnr
           AND t~spras EQ 'P'.
        IF sy-subrc EQ 0.
          SORT gt_mara BY matnr.
          DELETE ADJACENT DUPLICATES FROM gt_mara COMPARING matnr.
        ENDIF.

*--Get posted goods data
        SELECT a~rueck, a~rmzhl, a~mblnr, a~mjahr, a~mblpo,
               a~xallp, m~budat, m~cpudt, m~cputm,
               m~mblnr AS mblnr_doc, m~mjahr AS mjahr_doc,
               m~zeile, m~aufnr, m~anln1, m~anln2, m~bwart
          FROM matdoc AS m
          LEFT OUTER JOIN afwi AS a
          ON m~mblnr  EQ a~mblnr
          AND m~mjahr EQ a~mjahr
          AND m~zeile EQ a~mblpo
          INTO TABLE @gt_matdoc
          FOR ALL ENTRIES IN @gt_fmocindx
         WHERE m~cancelled EQ @space
           AND m~bwart     EQ @c_move_type-goods_receipt
           AND a~rueck     EQ @gt_fmocindx-rueck
           AND a~rmzhl     EQ @gt_fmocindx-rmzhl.

        SORT gt_matdoc BY rueck rmzhl aufnr.
      ENDIF.
    ENDIF.
  ENDIF.

  IF sy-uname EQ 'T_H.KABABE'.
    BREAK-POINT.
  ENDIF.

  SORT: gt_glflot BY tplma tplnr_fl,
        gt_fmocindx BY tplnr_fl,
        gt_farm BY tplnr_fl.

  LOOP AT gt_glflot INTO ls_glflot.
    DATA(lv_locked) = abap_false.

    READ TABLE gt_fmocindx INTO DATA(ls_fmocindx)
      WITH KEY tplnr_fl = ls_glflot-tplnr_fl BINARY SEARCH.
    WHILE sy-subrc EQ 0.
      DATA(lv_tabix) = sy-tabix + 1.

      lv_locked = abap_true.
      CLEAR lv_reentrada.

      READ TABLE gt_fmfpitm INTO DATA(ls_fmfpitm)
        WITH KEY aufnr = ls_fmocindx-aufnr BINARY SEARCH.
      IF sy-subrc NE 0.
        CLEAR ls_fmfpitm.
      ENDIF.

*      IF ls_fmocindx-matnr IS NOT INITIAL.
*        READ TABLE lt_reentrada INTO DATA(ls_reentrada)
*          WITH KEY matnr = ls_fmocindx-matnr.
*        IF sy-subrc NE 0.
*          REFRESH: lt_mat_class, lt_val_char, lt_val_curr,
*                   lt_val_num, lt_return.
*
*          CALL FUNCTION 'BAPI_OBJCL_GETCLASSES'
*            EXPORTING
*              objectkey_imp   = CONV objnum( ls_fmocindx-matnr )
*              objecttable_imp = 'MARA'
*              classtype_imp   = '001'
*              read_valuations = 'X'
*            TABLES
*              alloclist       = lt_mat_class
*              allocvalueschar = lt_val_char
*              allocvaluescurr = lt_val_curr
*              allocvaluesnum  = lt_val_num
*              return          = lt_return.
*
*          IF line_exists( lt_return[ type = 'S' ] ).
*            READ TABLE lt_val_num INTO DATA(ls_val_num)
*              WITH KEY charact = 'FAZ_DIAS_REENTRADA'.
*            IF sy-subrc EQ 0.
*              IF ls_val_num-value_from IS NOT INITIAL.
*                lv_reentrada = ls_val_num-value_from.
*              ENDIF.
*            ENDIF.
*          ENDIF.
*        ELSE.
*          lv_reentrada = ls_reentrada-reentrada.
*        ENDIF.
*      ENDIF.
      CLEAR lv_reentrada.
      READ TABLE lt_ausp INTO DATA(ls_ausp)
        WITH KEY matnr = ls_fmocindx-matnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        lv_reentrada = ls_ausp-atflv.
      ENDIF.

      INSERT INITIAL LINE INTO TABLE gt_output
        ASSIGNING FIELD-SYMBOL(<ls_output>).
      IF sy-subrc EQ 0.
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
        <ls_output>-tarefa         = ls_fmfpitm-matnr.
        <ls_output>-insumo         = ls_fmocindx-matnr.
        <ls_output>-dias_reentrada = lv_reentrada.
        <ls_output>-data_aplicacao = ls_fmocindx-budat.
        IF lv_reentrada IS NOT INITIAL.
          <ls_output>-data_bloqueio = ls_fmocindx-budat + lv_reentrada.
        ENDIF.
        <ls_output>-aufnr_to       = ls_fmocindx-aufnr.

        READ TABLE gt_mara INTO DATA(ls_mara)
          WITH KEY matnr = <ls_output>-tarefa BINARY SEARCH.
        IF sy-subrc EQ 0.
          <ls_output>-tarefa_txt = ls_mara-maktx.
        ENDIF.

        READ TABLE gt_mara INTO ls_mara
          WITH KEY matnr = <ls_output>-insumo BINARY SEARCH.
        IF sy-subrc EQ 0.
          <ls_output>-insumo_txt = ls_mara-maktx.
        ENDIF.

        IF p_date LE <ls_output>-data_bloqueio.
          <ls_output>-status = c_status-blocked.
          <ls_output>-color = c_color-blocked.
        ELSE.
          <ls_output>-status = c_status-available.
          <ls_output>-color = c_color-available.
        ENDIF.
      ENDIF.

      READ TABLE gt_fmocindx INTO ls_fmocindx INDEX lv_tabix
        COMPARING tplnr_fl.
    ENDWHILE.

    IF lv_locked = abap_false.
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

        IF p_date LE <ls_output>-data_bloqueio.
          <ls_output>-status = c_status-blocked.
          <ls_output>-color = c_color-blocked.
        ELSE.
          <ls_output>-status = c_status-available.
          <ls_output>-color = c_color-available.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF sy-uname EQ 'T_H.KABABE'.
    BREAK-POINT.
  ENDIF.

  SORT gt_output BY iwerk ASCENDING
                    tplnr_fl ASCENDING
                    data_bloqueio DESCENDING.

  DELETE ADJACENT DUPLICATES FROM gt_output COMPARING iwerk tplnr_fl.

  DATA(lt_output) = gt_output[].

  LOOP AT gt_output ASSIGNING <ls_output>.
    IF p_date LE <ls_output>-data_bloqueio.
      <ls_output>-status = c_status-blocked.
      <ls_output>-color = c_color-blocked.
    ELSE.
      <ls_output>-status = c_status-available.
      <ls_output>-color = c_color-available.
    ENDIF.
  ENDLOOP.

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
*& Form CHECK_RESTRICTION_PERIOD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LRT_BUDAT
*&---------------------------------------------------------------------*
FORM check_restriction_period CHANGING lrt_budat TYPE gy_budat_range.

  DATA: lt_constants TYPE zabs_tty_vkey_const,
        lv_objid     TYPE zabs_del_objid VALUE 'GIS',
        lv_k1val     TYPE zabs_del_k1val VALUE 'REENTRADA',
        lv_k2val     TYPE zabs_del_k2val VALUE 'BUDAT',
        lv_reentrada TYPE i.

  REFRESH lrt_budat.

  CALL METHOD zcl_abs_get_variants=>get_constant_multiple
    EXPORTING
      iv_objid     = lv_objid "GIS
      iv_k1val     = lv_k1val "REENTRADA
      iv_k2val     = lv_k2val "BUDAT
    IMPORTING
      et_constants = lt_constants.

  READ TABLE lt_constants INTO DATA(ls_constant)
        WITH KEY cnval1 = '01'.
  IF sy-subrc EQ 0.
    IF ls_constant-cnval2 CO ' 0123456789'.
      CONDENSE ls_constant-cnval2 NO-GAPS.
      lv_reentrada = ls_constant-cnval2.
    ENDIF.
  ENDIF.

  INSERT INITIAL LINE INTO TABLE lrt_budat
    ASSIGNING FIELD-SYMBOL(<lrs_budat>).
  IF sy-subrc EQ 0.
    <lrs_budat> = 'IBT'.
    <lrs_budat>-low  = p_date.
    <lrs_budat>-high = p_date.
    IF lv_reentrada IS NOT INITIAL.
      <lrs_budat>-low = <lrs_budat>-low - lv_reentrada.
    ENDIF.
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
        container_name = 'C_CARENCIA_0100_CC'.

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
*& Form CONTROLS_DISPLAY
*&---------------------------------------------------------------------*
*& Display Controls
*&---------------------------------------------------------------------*
FORM controls_display.

  DATA : lv_routine(21) VALUE 'CONTROLS_DISPLAY_'.

  CONCATENATE lv_routine sy-dynnr INTO lv_routine.
  PERFORM (lv_routine) IN PROGRAM zabs_rep_mapa_reentrada IF FOUND.

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
        dyname             = 'ZABS_REP_MAPA_REENTRADA'
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

*&---------------------------------------------------------------------*
*& Form MATNR_EXCLUDES_PREPARE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LRT_MATNR
*&---------------------------------------------------------------------*
FORM matnr_excludes_prepare CHANGING lrt_matnr TYPE gy_matnr_range.

  DATA: lt_constants TYPE zabs_tty_vkey_const,
        lv_objid     TYPE zabs_del_objid VALUE 'GIS',
        lv_k1val     TYPE zabs_del_k1val VALUE 'REENTRADA',
        lv_k2val     TYPE zabs_del_k2val VALUE 'EXCLUDE'.

  REFRESH lrt_matnr.

  CALL METHOD zcl_abs_get_variants=>get_constant_multiple
    EXPORTING
      iv_objid     = lv_objid "GIS
      iv_k1val     = lv_k1val "REENTRADA
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

  LOOP AT gt_output INTO DATA(ls_output).
    INSERT INITIAL LINE INTO TABLE lt_map_data
      ASSIGNING FIELD-SYMBOL(<ls_map_data>).
    IF sy-subrc EQ 0.
      DO 3 TIMES.
        DATA(lv_index) = sy-index.
        CLEAR lv_alv_fdname.
        lv_map_fdname = 'FIELD' && lv_index.
        CASE lv_index.
          WHEN 1.
            lv_alv_fdname = 'TPLNR_FL'.
            CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
              EXPORTING
                input  = ls_output-tplnr_fl
              IMPORTING
                output = ls_output-tplnr_fl.
          WHEN 2.
            lv_alv_fdname = 'DATA_APLICACAO'.
          WHEN 3.
            lv_alv_fdname = 'DATA_BLOQUEIO'.
        ENDCASE.

        IF lv_alv_fdname IS INITIAL.
          EXIT.
        ENDIF.

        READ TABLE gt_fcat INTO DATA(ls_fcat)
          WITH KEY fieldname = lv_alv_fdname.
        IF sy-subrc EQ 0.
          <ls_map_data>-tplnr_fl = ls_output-tplnr_fl.
          <ls_map_data>-color = ls_output-color.
*--Map Field
          ASSIGN COMPONENT lv_map_fdname OF STRUCTURE <ls_map_data>
            TO FIELD-SYMBOL(<lv_map_fdval>).
          IF sy-subrc EQ 0.
            <lv_map_fdval> = lv_alv_fdname.
*--Map Field's Value
            ASSIGN COMPONENT lv_alv_fdname OF STRUCTURE ls_output
              TO FIELD-SYMBOL(<lv_alv_fdval>).
            IF sy-subrc EQ 0.
              IF lv_alv_fdname = 'DATA_APLICACAO'
              OR lv_alv_fdname = 'DATA_BLOQUEIO'.
                CLEAR lv_conv_date.
                IF <lv_alv_fdval> IS NOT INITIAL.
*--Convert date to screen acceptable format
                  CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
                    EXPORTING
                      input  = <lv_alv_fdval>
                    IMPORTING
                      output = lv_conv_date.
                ENDIF.

*                <lv_map_fdval> = lv_alv_fdname && '|' &&  lv_conv_date.
                <lv_map_fdval> = ls_fcat-scrtext_m && '|' &&  lv_conv_date.
              ELSE.
*                <lv_map_fdval> = lv_alv_fdname && '|' &&  <lv_alv_fdval>.
                <lv_map_fdval> = ls_fcat-scrtext_m && '|' &&  <lv_alv_fdval>.
              ENDIF.
            ENDIF.
*--Map Field's Color
            <lv_map_fdval> = <lv_map_fdval> && '|' &&  map_color-black.
          ENDIF.
        ENDIF.
      ENDDO.
    ENDIF.
  ENDLOOP.

  IF lt_map_data[] IS NOT INITIAL.
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
          i_farm = ls_farm-tplnr_fl
          i_type = '1'
        IMPORTING
          e_url  = lv_url
        TABLES
          t_tab  = lt_map_data.

      IF lv_url IS NOT INITIAL.
        edurl = lv_url.
        CALL METHOD html_control->show_url
          EXPORTING
            url = edurl.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
