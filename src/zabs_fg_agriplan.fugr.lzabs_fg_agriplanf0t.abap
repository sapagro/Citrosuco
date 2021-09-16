*----------------------------------------------------------------------*
***INCLUDE /AGRI/LFMACMNF0T .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  TRANSACTION_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM transaction_init  USING lv_mode.

  CHECK gs_variables-overview_mode IS INITIAL.

  gs_variables-worklist_is_visible = c_true.
  gs_variables-overview_mode       = lv_mode.
  gs_variables-document_mode       = lv_mode.

****Register the log events that are to be handled
  CREATE OBJECT ref_log_handler.
  message_log_event_handler_set ref_log_handler
                                on_log_display_profile.

ENDFORM.                    " TRANSACTION_INIT
*&---------------------------------------------------------------------*
*&      Form  TITLE_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM title_set .

  DATA: lv_acnum       TYPE zfmacnum,
        lv_object_type TYPE string,
        lv_description TYPE /agri/gdescr_40.

  lv_object_type = TEXT-001.
  lv_acnum = gs_acdoc_infocus-acnum.

  CASE sy-dynnr.

    WHEN c_screen-main_screen.

****If no document is in focus
      IF lv_acnum IS INITIAL.
        IF gs_variables-overview_mode EQ c_mode_change
           AND NOT gs_variables-document_mode EQ c_mode_create.
          SET TITLEBAR 'T100' WITH TEXT-005
                                   lv_object_type.
        ELSEIF gs_variables-overview_mode EQ c_mode_display.
          SET TITLEBAR 'T100' WITH TEXT-002
                                   gs_variables-object_text.
        ENDIF.
****If some document is in focus
      ELSEIF gs_variables-document_mode EQ c_mode_change.
        SET TITLEBAR 'T101' WITH TEXT-005
                                 gs_variables-object_text
                                 lv_acnum.

      ELSEIF gs_variables-document_mode EQ c_mode_display.
        SET TITLEBAR 'T101' WITH TEXT-002
                                 gs_variables-object_text
                                 lv_acnum.

      ELSEIF gs_variables-document_mode EQ c_mode_create.
        SET TITLEBAR 'T101' WITH TEXT-003
                                 gs_variables-object_text
                                 TEXT-004.
      ENDIF.
    WHEN c_screen-hierarchy_items.
      SET TITLEBAR 'T202'.
    WHEN c_screen-create_mass_dialog OR
         c_screen-create_dialog.
**      keytext_get_simple 'ZFMACHDR' 'ASLVL' /agri/1899SC_FMACHDR-aslvl
**                         lv_description.
      SET TITLEBAR 'T102' WITH lv_description.
    WHEN c_screen-update_mass.
      SET TITLEBAR 'T310'.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " TITLE_SET
*&---------------------------------------------------------------------*
*&      Form  TOOLBAR_EXCLUDES_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM toolbar_excludes_prepare
                          TABLES lt_toolbar_excludes TYPE ui_functions.

  APPEND /agri/cl_gui_alv_grid=>mc_fc_info  TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_graph TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_print TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_views TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_check TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_refresh
                                           TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_mb_paste TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_append_row
                                           TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_copy
                                           TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_copy_row
                                           TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_cut
                                           TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_delete_row
                                           TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_insert_row
                                           TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_move_row
                                           TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_paste
                                           TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_paste_new_row
                                           TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_undo
                                           TO lt_toolbar_excludes.

ENDFORM.                    " TOOLBAR_EXCLUDES_PREPARE
*&---------------------------------------------------------------------*
*&      Form  TABSTRIP_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tabstrip_build .

  DATA: lv_fcode TYPE syucomm,
        lv_exist.

  FIELD-SYMBOLS: <ls_tabstrip_fcode> TYPE /agri/s_gtabstrip.

  READ TABLE gt_tabstrip_fcodes ASSIGNING <ls_tabstrip_fcode>
                                 WITH KEY ts_fcode = ts_items-activetab.
  IF sy-subrc EQ 0.
    lv_fcode = <ls_tabstrip_fcode>-local_fcode.
  ENDIF.

  PERFORM tabstrip_initialize.

****Set back previous tab
  IF lv_fcode IS INITIAL.
    CLEAR ts_items-activetab.
  ELSE.
    READ TABLE gt_tabstrip_fcodes ASSIGNING <ls_tabstrip_fcode>
                                   WITH KEY local_fcode = lv_fcode.
    IF sy-subrc EQ 0.
      IF <ls_tabstrip_fcode>-invisible IS INITIAL.
        ts_items-activetab = <ls_tabstrip_fcode>-ts_fcode.
      ELSE.
        CLEAR ts_items-activetab.
      ENDIF.
    ELSE.
      CLEAR ts_items-activetab.
    ENDIF.
  ENDIF.

****Adjust scroll
  IF ts_items-activetab IS INITIAL.
    CLEAR ts_items-%_scrollposition.
  ENDIF.

ENDFORM.                    " TABSTRIP_BUILD
*&---------------------------------------------------------------------*
*&      Form  TABSTRIP_INITIALIZE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tabstrip_initialize .

  DATA: lv_domain             TYPE domname VALUE 'ZABS_DOM_BUDGET',
        lv_actual_tabname(32) TYPE c,
        lv_agtab(2)           TYPE c,
        lt_domain_values      TYPE TABLE OF dd07v,
        ls_domain_value       TYPE dd07v,
        ls_tabstrip_fcode     TYPE /agri/s_gtabstrip.

  CONSTANTS: c_prefix_caption(24)  TYPE c
                                  VALUE 'GS_TABSTRIP_CAPTIONS-TAB',
             c_prefix_tab_fcode(2) TYPE c VALUE 'T\'.

  FIELD-SYMBOLS:  <lv_actual_tab> TYPE text60.

  IF gs_tabstrip_captions IS INITIAL.
****Prepare default tabs
    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = lv_domain
        text           = c_true
        langu          = sy-langu
      TABLES
        dd07v_tab      = lt_domain_values
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.
    IF sy-subrc EQ 0.
      LOOP AT lt_domain_values INTO ls_domain_value.
        CLEAR ls_tabstrip_fcode.
        lv_agtab = ls_domain_value-valpos+2(2).

        CONCATENATE c_prefix_caption lv_agtab INTO lv_actual_tabname.
        ASSIGN (lv_actual_tabname) TO <lv_actual_tab>.
        IF sy-subrc EQ 0.
          ls_tabstrip_fcode-tscode = ls_domain_value-domvalue_l.

          CASE ls_domain_value-domvalue_l.
            WHEN 'DP'.
*-- Doses e Passadas
              WRITE icon_product_receipts AS ICON TO <lv_actual_tab>.
              CONCATENATE <lv_actual_tab> ls_domain_value-ddtext
                     INTO <lv_actual_tab>.

              PERFORM icon_text_prepare USING ls_domain_value-ddtext
                                     CHANGING <lv_actual_tab>.
****Store other info for screen processing
              REPLACE 'GS_' IN lv_actual_tabname WITH '/AGRI/S_G'.
              ls_tabstrip_fcode-tabname = lv_actual_tabname.
              CONCATENATE c_prefix_tab_fcode lv_agtab
                     INTO ls_tabstrip_fcode-ts_fcode.
              ls_tabstrip_fcode-local_fcode = c_fcode-tab_dose.
              APPEND ls_tabstrip_fcode TO gt_tabstrip_fcodes.
            WHEN 'QP'.
*-- Quantidade de Produtos
              WRITE icon_product_requirements AS ICON TO <lv_actual_tab>.
              CONCATENATE <lv_actual_tab> ls_domain_value-ddtext
                     INTO <lv_actual_tab>.

              PERFORM icon_text_prepare USING ls_domain_value-ddtext
                                     CHANGING <lv_actual_tab>.
****Store other info for screen processing
              REPLACE 'GS_' IN lv_actual_tabname WITH '/AGRI/S_G'.
              ls_tabstrip_fcode-tabname = lv_actual_tabname.
              CONCATENATE c_prefix_tab_fcode lv_agtab
                     INTO ls_tabstrip_fcode-ts_fcode.
              ls_tabstrip_fcode-local_fcode = c_fcode-tab_qtde.
              APPEND ls_tabstrip_fcode TO gt_tabstrip_fcodes.
            WHEN 'AD'.
*-- Dados Administrativos
              WRITE icon_administrative_data AS ICON TO <lv_actual_tab>.
              CONCATENATE <lv_actual_tab> ls_domain_value-ddtext
                     INTO <lv_actual_tab>.

              PERFORM icon_text_prepare USING ls_domain_value-ddtext
                                     CHANGING <lv_actual_tab>.

              REPLACE 'GS_' IN lv_actual_tabname WITH '/AGRI/S_G'.
              ls_tabstrip_fcode-tabname = lv_actual_tabname.
              CONCATENATE c_prefix_tab_fcode lv_agtab
                     INTO ls_tabstrip_fcode-ts_fcode.
              ls_tabstrip_fcode-local_fcode = c_fcode-tab_admin.
              APPEND ls_tabstrip_fcode TO gt_tabstrip_fcodes.
          ENDCASE.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

  gt_tabstrip_texts = lt_domain_values.
  /agri/s_gtabstrip_captions = gs_tabstrip_captions.

ENDFORM.                    " TABSTRIP_INITIALIZE
*&---------------------------------------------------------------------*
*&      Form  TEXT_MAINTAIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM text_maintain USING lv_txtgr
                         lv_text_object
                CHANGING lv_changed.

**** ESP6 Task #30035 - Global Text Engine Integration
*  DATA: lv_objval       TYPE tdobname,
  DATA: lv_objval       TYPE /agri/gtxobjval,
****
        lv_display_only.

*  IF ref_text IS INITIAL.
*    CREATE OBJECT ref_text
***** ESP6 Task #30035 - Global Text Engine Integration
*      EXPORTING
*        i_objtp =  c_switch_object_type.
*****
*  ENDIF.

  IF gs_variables-document_mode EQ c_mode_display.
    lv_display_only = c_true.
  ENDIF.

  CONCATENATE gs_acdoc_infocus-x-achdr-acnum '' INTO lv_objval.

  IF NOT ref_text IS INITIAL.
**** ESP6 Task #30035 - Global Text Engine Integration
    PERFORM text_control_initialize.
*    CALL FUNCTION '/AGRI/G_TEXT_INIT'.
****
**** ESP6 Task #30035 - Global Text Engine Integration
*    CALL METHOD ref_text->text_initialize
    CALL METHOD ref_text->text_infocus_set
****
      EXPORTING
        i_tdobject              = lv_text_object
        i_objval                = lv_objval
*       I_OBJDESCR              =
        i_display_only          = lv_display_only
        i_txtgr                 = lv_txtgr
        i_spras                 = '*'
*       i_mode                  = gs_variables-document_mode
      EXCEPTIONS
        object_not_found        = 1
        obj_procedure_not_found = 2
        text_ids_not_maintained = 3
        OTHERS                  = 4.

    CALL METHOD ref_text->text_maintain
      EXPORTING
        i_subscreen = c_true
        i_save      = c_false
      IMPORTING
        e_changed   = lv_changed.

  ENDIF.

ENDFORM.                    " TEXT_MAINTAIN
**** ESP6 Task #30035 - Global Text Engine Integration
FORM text_control_initialize.
  CALL FUNCTION '/AGRI/G_TEXT_INIT'.
ENDFORM.                    "text_control_initialize
****
*&---------------------------------------------------------------------*
*&      Form  TEXTS_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM texts_update .
  DATA: lv_texts_changed.
  CLEAR sy-subrc.

  IF NOT ref_text IS INITIAL.
    CALL METHOD ref_text->text_update
      IMPORTING
        e_data_changed = lv_texts_changed.
  ENDIF.

  IF sy-subrc EQ 0
 AND lv_texts_changed IS NOT INITIAL.

    gs_variables-data_changed = c_true.

    IF gs_acdoc_infocus-x-achdr-updkz IS INITIAL.
      gs_acdoc_infocus-x-achdr-updkz = c_updkz_update.
    ENDIF.
  ENDIF.
ENDFORM.                    " TEXTS_UPDATE

*&---------------------------------------------------------------------*
*& Form TERRAIN_ATTRIBUTE_GET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_ACITM
*&---------------------------------------------------------------------*
*...BOC-T_T.KONNO
*FORM terrain_attribute_get CHANGING lt_acitm TYPE zt_fmacitm.
FORM terrain_attribute_get CHANGING lt_acitm  TYPE zt_fmacitm
                                    lt_safras TYPE type_safras_tab.
*...EOC-T_T.KONNO

  DATA : lt_tplnr         TYPE /agri/t_gltplnr,
         lv_tplnr         TYPE /agri/gltplnr_fl,
         ls_fldoc_infocus TYPE /agri/s_glfl_doc,
         lv_internal      TYPE atinn,
*         lv_int4_field    TYPE int4,
*         lv_int4_field(12) TYPE p DECIMALS 3,
         lv_float         TYPE f,
         lv_output(12)    TYPE p DECIMALS 3,
         lt_fl_doc        TYPE /agri/t_glfl_doc.

  FIELD-SYMBOLS: <lwa_acitm>   TYPE zsc_fmacitm,
                 <lwa_glflatv> TYPE /agri/s_glflatv.

  CONSTANTS: BEGIN OF lc_grupo_medicao,
               volume_total TYPE /agri/glmpgrp VALUE 'ZFAZ_VOL_TOTAL_PL',
               qtd_plantas  TYPE /agri/glmpgrp VALUE 'FAZ-INV-PLANTAS',
               volume_copa  TYPE /agri/glmpgrp VALUE 'FAZ-CUBICAGEM',
             END OF lc_grupo_medicao.

  CONSTANTS: BEGIN OF lc_measurement_level,
               terrain      TYPE /agri/glaslvl VALUE 'T',
               crop_seasons TYPE /agri/glaslvl VALUE 'A',
               harvest      TYPE /agri/glaslvl VALUE 'H',
               irrigation   TYPE /agri/glaslvl VALUE 'I',
             END OF lc_measurement_level.

  CONSTANTS: BEGIN OF lc_document_type,
               terrain TYPE /agri/glmdtyp VALUE 'ZTYP',
               plant   TYPE /agri/glmdtyp VALUE 'ZARV',
             END OF lc_document_type.

  CONSTANTS: BEGIN OF lc_document_category,
               production_order TYPE /agri/gl_autyp VALUE 'AO',
               task_order       TYPE /agri/gl_autyp VALUE 'TO',
               produce_reciept  TYPE /agri/gl_autyp VALUE 'PR',
               work_order       TYPE /agri/gl_autyp VALUE 'WO',
               purchase_order   TYPE /agri/gl_autyp VALUE 'PO',
               confirmation     TYPE /agri/gl_autyp VALUE 'OC',
               reversals        TYPE /agri/gl_autyp VALUE 'CR',
               measurements     TYPE /agri/gl_autyp VALUE 'MD',
             END OF lc_document_category.

  LOOP AT lt_acitm ASSIGNING <lwa_acitm>.
    MOVE <lwa_acitm>-tplnr_fl TO lv_tplnr.
    APPEND lv_tplnr TO lt_tplnr.
    CLEAR: lv_tplnr.
  ENDLOOP.

  SORT lt_tplnr ASCENDING BY tplnr_fl.
  DELETE ADJACENT DUPLICATES FROM lt_tplnr COMPARING ALL FIELDS.

  CALL FUNCTION '/AGRI/GLFL_VIEW'
    EXPORTING
      it_tplnr       = lt_tplnr
    IMPORTING
      et_fldoc       = lt_fl_doc
    EXCEPTIONS
      no_data_exists = 1
      OTHERS         = 2.

  PERFORM attribute_intenalname_get USING 'QTYPLANT'
                                    CHANGING lv_internal.

*...BOC-T_T.KONNO aqui cálculos
*  LOOP AT lt_acitm ASSIGNING <lwa_acitm>.
*    READ TABLE lt_fl_doc INTO ls_fldoc_infocus
*      WITH KEY tplnr_fl = <lwa_acitm>-tplnr_fl.
*    IF sy-subrc EQ 0.
*      READ TABLE ls_fldoc_infocus-x-flatv ASSIGNING <lwa_glflatv>
*        WITH KEY tplnr_fl = <lwa_acitm>-tplnr_fl
*                 class    = 'GRUPO_ABAP'
*                 atinn    = lv_internal.
*      IF sy-subrc EQ 0.
*        IF gs_variables-document_mode = c_mode_change.
*          IF <lwa_acitm>-adqpl NE <lwa_glflatv>-atflv.
*            <lwa_acitm>-updkz = c_updkz_update.
*            gs_variables-refresh_items_grid = c_true.
*          ENDIF.
*        ENDIF.
*        <lwa_acitm>-adqpl = <lwa_glflatv>-atflv.
*        <lwa_acitm>-unqpl = 'UN'.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.

  IF lt_acitm[] IS NOT INITIAL.
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
      FOR ALL ENTRIES IN @lt_acitm
     WHERE h~mdtyp    EQ @lc_document_type-terrain
       AND h~aslvl    EQ @lc_measurement_level-terrain
       AND h~tplnr_fl EQ @lt_acitm-tplnr_fl
       AND h~mpgrp    EQ @lc_grupo_medicao-volume_total.

    SORT lt_glmdhdr_join BY tplnr_fl atnam.

*...BOC-T_T.KONNO
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
      FOR ALL ENTRIES IN @lt_acitm
     WHERE h~mdtyp    EQ @lc_document_type-plant
       AND h~aslvl    EQ @lc_measurement_level-crop_seasons
       AND h~tplnr_fl EQ @lt_acitm-tplnr_fl
       AND h~mpgrp    EQ @lc_grupo_medicao-qtd_plantas.

    SORT lt_glmdhdr_plants BY tplnr_fl atnam.

*-- 62: 227374-00028
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
      FOR ALL ENTRIES IN @lt_acitm
     WHERE h~mdtyp    EQ @lc_document_type-terrain
       AND h~aslvl    EQ @lc_measurement_level-terrain
       AND h~tplnr_fl EQ @lt_acitm-tplnr_fl
       AND h~mpgrp    EQ @lc_grupo_medicao-volume_copa.

    SORT lt_glmdhdr_volume BY tplnr_fl atnam.
*...EOC-T_T.KONNO
  ENDIF.

  IF lt_safras[] IS NOT INITIAL.
    SELECT * FROM /agri/glamhdr
      INTO TABLE @DATA(lt_glamhdr)
       FOR ALL ENTRIES IN @lt_safras
     WHERE tplnr_fl EQ @lt_safras-tplnr_fl.
    IF sy-subrc EQ 0.
      SORT lt_glamhdr BY tplnr_fl ivact.
    ENDIF.
  ENDIF.

  LOOP AT lt_acitm ASSIGNING <lwa_acitm>.
    IF <lwa_acitm>-updkz IS NOT INITIAL.
      CONTINUE.
    ENDIF.
    READ TABLE lt_fl_doc ASSIGNING FIELD-SYMBOL(<ls_fldoc_infocus>)
      WITH KEY tplnr_fl = <lwa_acitm>-tplnr_fl.
    IF sy-subrc EQ 0.
      IF gs_variables-document_mode = c_mode_create.
        <ls_fldoc_infocus>-updkz = c_updkz_new.
        <lwa_acitm>-updkz = c_updkz_new.
      ELSEIF gs_variables-document_mode = c_mode_change.
        <ls_fldoc_infocus>-updkz = c_updkz_update.
        <lwa_acitm>-updkz = c_updkz_update.
      ENDIF.

      READ TABLE lt_glmdhdr_plants INTO DATA(lwa_plants)
        WITH KEY tplnr_fl = <lwa_acitm>-tplnr_fl
                 atnam    = 'INV-QTDADE-ARV' BINARY SEARCH.
      IF sy-subrc EQ 0.
        <lwa_acitm>-adqpl = lwa_plants-atflv.
        <lwa_acitm>-unqpl = 'PÉS'.

        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
          EXPORTING
            input          = <lwa_acitm>-unqpl
            language       = 'P'
          IMPORTING
            output         = <lwa_acitm>-unqpl
          EXCEPTIONS
            unit_not_found = 1
            OTHERS         = 2.
      ELSE.
        CLEAR lwa_plants.
      ENDIF.

      READ TABLE lt_glmdhdr_volume INTO DATA(lwa_volume)
        WITH KEY tplnr_fl = <lwa_acitm>-tplnr_fl
                 atnam    = 'FAZ_ALTURA_COPA' BINARY SEARCH.
      IF sy-subrc EQ 0.
        MOVE lwa_volume-atflv TO lv_float.
        DATA(lv_altura) = lv_float.
      ENDIF.

      READ TABLE lt_glmdhdr_volume INTO lwa_volume
        WITH KEY tplnr_fl = <lwa_acitm>-tplnr_fl
                 atnam    = 'CIT-ESPACAMENTO-PES' BINARY SEARCH.
      IF sy-subrc EQ 0.
        MOVE lwa_volume-atflv TO lv_float.
        DATA(lv_espacamento) = lv_float.
      ENDIF.

      READ TABLE lt_glmdhdr_volume INTO lwa_volume
        WITH KEY tplnr_fl = <lwa_acitm>-tplnr_fl
                 atnam    = 'FAZ_LARGURA_COPA' BINARY SEARCH.
      IF sy-subrc EQ 0.
        MOVE lwa_volume-atflv TO lv_float.
        DATA(lv_largura) = lv_float.
      ENDIF.

      READ TABLE lt_glmdhdr_volume INTO lwa_volume
        WITH KEY tplnr_fl = <lwa_acitm>-tplnr_fl
                 atnam    = 'FAZ_PERCENTAG_FALHA' BINARY SEARCH.
      IF sy-subrc EQ 0.
        MOVE lwa_volume-atflv TO lv_float.
        DATA(lv_percentual) = lv_float.
      ENDIF.

      gs_variables-refresh_items_grid = c_true.

*M³ Copa por Planta = (Altura da Copa) x (Largura da Copa) x (Espaçamento entre Plantas)
      DATA(lv_copa_planta) = lv_altura * lv_largura * lv_espacamento.
*Volume de Copa = (M³ da Copa por Planta) x (Nº de Plantas) x (% de Falha)
      DATA(lv_volume_copa) = lv_copa_planta * <lwa_acitm>-adqpl  * ( 1 - ( lv_percentual / 100 ) ).
      lv_output = lv_volume_copa.
      <lwa_acitm>-advlc = lv_output.

      CLEAR: lv_altura, lv_espacamento, lv_largura, lv_percentual.
    ENDIF.
  ENDLOOP.
*...EOC-T_T.KONNO

ENDFORM.

FORM terrain_manual_get USING VALUE(lv_tplnr_fl)
                        CHANGING lwa_acitm_manual TYPE zsc_fmacitm_fcat.

  DATA: lt_acitm TYPE zt_fmacitm,
        lv_ajahr TYPE char5,
        lv_tplnr TYPE /agri/gltplnr_fl.


*...BOC-T_T.KONNO
  IF ok_code NE 'DLRW'.
*...EOC-T_T.KONNO
    gs_variables-initiator = c_log_initiator-check.
    PERFORM messages_initialize USING gs_variables-initiator
                                      c_log_subobject-check
                                      gs_acdoc_infocus-x-achdr.
    converse_terrains lv_tplnr_fl lv_tplnr.
    CONCATENATE  zsc_fmachdr-ajahr '%' INTO lv_ajahr.

*...BOC-T_T.KONNO
*  SELECT SINGLE * FROM /agri/glflcma AS a INNER JOIN /agri/glflot AS t
*              ON t~tplnr_fl EQ a~tplnr_fl
*              INTO CORRESPONDING FIELDS OF @ls_acitm_manual
*                    WHERE t~iwerk = @zsc_fmachdr-werks
*                    AND a~tplnr_fl = @lv_tplnr
*                    AND a~datab LIKE @lv_ajahr
*                          AND a~loevm NE 'X'
*                          AND a~astat = 'A'.
    SELECT * FROM /agri/glflcma AS a
      INNER JOIN /agri/glflot AS t
      ON a~tplnr_fl EQ t~tplnr_fl
      INTO CORRESPONDING FIELDS OF TABLE @lt_acitm
     WHERE t~iwerk    IN @so_werks[]
       AND a~tplnr_fl EQ @lv_tplnr
       AND a~loevm    NE @abap_true
       AND a~astat    EQ @c_crop_season_status-active.

*    DELETE lt_acitm WHERE datab GT p_datab
*                       OR datbi LT p_datbi.
    DELETE lt_acitm WHERE NOT ( datab BETWEEN p_datab AND p_datbi )
                      AND NOT ( datbi BETWEEN p_datab AND p_datbi )
                      AND NOT ( datab LE p_datab AND
                                datbi GE p_datbi ).

    READ TABLE lt_acitm INTO DATA(lwa_acitm) INDEX 1.
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING lwa_acitm TO lwa_acitm_manual.
    ENDIF.
*...EOC-T_T.KONNO

    IF sy-subrc NE 0.
      MESSAGE ID 'ZFMAC' TYPE c_msg_type-error
        NUMBER '072' WITH zsc_fmachdr-werks
                          lv_tplnr
                          zsc_fmachdr-ajahr INTO sy-msgli.
      message_simple c_false.
      gs_variables-errors = c_true.
      EXIT.
    ENDIF.
*...BOC-T_T.KONNO
  ENDIF.
*...EOC-T_T.KONNO

ENDFORM.
*&---------------------------------------------------------------------*
*& Form TERRAINS_F4_VALUES_GET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_VALUE
*&      <-- LV_CHANGE
*&      <-- LV_SUBRC
*&---------------------------------------------------------------------*
FORM terrains_f4_values_get USING lv_display
                         CHANGING lv_value TYPE /agri/gltplnr_fl
                                  lv_change
                                  lv_subrc TYPE sy-subrc.

  DATA: lt_terrains_tab   TYPE zt_fmacterr_tab,
        lwa_terrains_data LIKE LINE OF lt_terrains_tab,
        lwa_field_mapping TYPE dselc,
        lt_field_mapping  TYPE TABLE OF dselc,
        lv_fieldname      TYPE fieldname,
        lt_return_values  TYPE TABLE OF ddshretval INITIAL SIZE 0,
        lwa_return_value  TYPE ddshretval,
        lv_title(80).

  FIELD-SYMBOLS: <lt_terrains_tab> TYPE STANDARD TABLE.

  PERFORM terrains_read USING zsc_fmachdr-werks
                      CHANGING lt_terrains_tab.

  CHECK lt_terrains_tab[] IS NOT INITIAL.
  lv_title = TEXT-019.
  CONDENSE lv_title.
  ASSIGN lt_terrains_tab[] TO <lt_terrains_tab>.
  lv_fieldname = 'TPLNR_FL'.

  CHECK <lt_terrains_tab> IS ASSIGNED.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = lv_fieldname
      window_title    = lv_title
      value_org       = 'S'
      display         = lv_display
    TABLES
      value_tab       = <lt_terrains_tab>[]
      return_tab      = lt_return_values
      dynpfld_mapping = lt_field_mapping
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  READ TABLE lt_return_values INTO lwa_return_value
                INDEX 1.
  IF sy-subrc EQ 0.
    lv_change = c_true.
    lv_value = lwa_return_value-fieldval.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form TERRAINS_READ
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ZSC_FMACHDR_WERKS
*&      <-- LT_TERRAINS_TAB
*&---------------------------------------------------------------------*
FORM terrains_read  USING    lv_werks TYPE werks_d
                    CHANGING lt_terrains_tab TYPE zt_fmacterr_tab.

  SELECT * FROM /agri/glflot
           INTO CORRESPONDING FIELDS OF TABLE lt_terrains_tab
          WHERE iwerk = lv_werks.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form TERRAINS_DUPLICATE_CHEK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_SUBRC
*&---------------------------------------------------------------------*
FORM terrains_duplicate_chek USING ls_acitm TYPE zsc_fmacitm_fcat
                          CHANGING lv_subrc TYPE sy-subrc.

*...BOC-T_T.KONNO
*  DATA: lwa_acitm_tmp TYPE zsc_fmacitm_fcat,
*        lv_cnt        TYPE i.
*
*  CLEAR: lv_subrc.
*
*  LOOP AT gt_fmacitm_fcat INTO lwa_acitm_tmp
*  WHERE     tplnr_fl EQ ls_acitm-tplnr_fl.
*    lv_cnt = lv_cnt + 1.
*    IF lv_cnt GT 1.
*      lv_subrc = 4.
*      MESSAGE ID 'ZFMAC' TYPE 'E' NUMBER '073'
*                              WITH lwa_acitm_tmp-tplnr_fl.
*      CLEAR: lwa_acitm_tmp.
*    ENDIF.
*  ENDLOOP.
  DATA(lt_itens) = gt_fmacitm_fcat[].
  DELETE lt_itens WHERE tplnr_fl IS INITIAL.
  SORT lt_itens BY tplnr_fl.
  IF ls_acitm-tplnr_fl IS NOT INITIAL.
    DELETE lt_itens WHERE tplnr_fl NE ls_acitm-tplnr_fl.
    DATA(lv_identical) = lines( lt_itens ).
    IF lv_identical GT 1.
      lv_subrc = 4.
      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
        EXPORTING
          input  = ls_acitm-tplnr_fl
        IMPORTING
          output = ls_acitm-tplnr_fl.
**....Terreno &1 duplicado! Favor ajustar os dados informados.
      MESSAGE e093(zfmac) WITH ls_acitm-tplnr_fl.
    ENDIF.
  ENDIF.
*...EOC-T_T.KONNO

ENDFORM.

*&---------------------------------------------------------------------*
*& Form TERRAINS_ANABLED_SET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_AJAHR
*&      <-- LT_ACITM
*&---------------------------------------------------------------------*
*...BOC-T_T.KONNO
FORM terrains_anabled_set USING lv_typeq TYPE char1
                       CHANGING lt_acitm TYPE zt_fmacitm.
*...EOC-T_T.KONNO

  DATA: lt_acitm_aux TYPE zt_fmacitm.

  CASE lv_typeq .
    WHEN c_updkz_new.
*...BOC-T_T.KONNO
*      SELECT * FROM /agri/glflcma AS a
*        INNER JOIN /agri/glflot AS t
*        ON a~tplnr_fl EQ t~tplnr_fl
*        INTO CORRESPONDING FIELDS OF TABLE lt_acitm
*       WHERE t~iwerk IN so_werks
*         AND a~datab LIKE lv_ajahr
*         AND a~loevm NE 'X'
*         AND a~astat = 'A'.
      SELECT * FROM /agri/glflcma AS a
        INNER JOIN /agri/glflot AS t
        ON a~tplnr_fl EQ t~tplnr_fl
        INTO CORRESPONDING FIELDS OF TABLE @lt_acitm
       WHERE t~iwerk IN @so_werks[]
         AND a~loevm NE @abap_true
         AND a~astat EQ @c_crop_season_status-active.

*      DELETE lt_acitm WHERE datab GT p_datab
*                         OR datbi LT p_datbi.
      DELETE lt_acitm WHERE NOT ( datab BETWEEN p_datab AND p_datbi )
                        AND NOT ( datbi BETWEEN p_datab AND p_datbi )
                        AND NOT ( datab LE p_datab AND
                                  datbi GE p_datbi ).
*...EOC-T_T.KONNO
    WHEN c_updkz_update.
      lt_acitm_aux[] = lt_acitm.
      REFRESH: lt_acitm.
      IF lt_acitm_aux[] IS NOT INITIAL.
*...BOC-T_T.KONNO
*        SELECT * FROM /agri/glflcma AS a
*          INNER JOIN /agri/glflot AS t
*          ON a~tplnr_fl EQ t~tplnr_fl
*          INTO CORRESPONDING FIELDS OF TABLE lt_acitm
*          FOR ALL ENTRIES IN lt_acitm_aux[]
*         WHERE a~tplnr_fl = lt_acitm_aux-tplnr_fl
*           AND a~datab LIKE lv_ajahr
*           AND a~loevm NE 'X'
*           AND a~astat = 'A'.
        SELECT * FROM /agri/glflcma AS a
          INNER JOIN /agri/glflot AS t
          ON a~tplnr_fl EQ t~tplnr_fl
          INTO CORRESPONDING FIELDS OF TABLE @lt_acitm
          FOR ALL ENTRIES IN @lt_acitm_aux[]
         WHERE a~tplnr_fl EQ @lt_acitm_aux-tplnr_fl
           AND a~loevm    NE @abap_true
           AND a~astat    EQ @c_crop_season_status-active.

*        DELETE lt_acitm WHERE datab GT p_datab
*                           OR datbi LT p_datbi.
        DELETE lt_acitm WHERE NOT ( datab BETWEEN p_datab AND p_datbi )
                          AND NOT ( datbi BETWEEN p_datab AND p_datbi )
                          AND NOT ( datab LE p_datab AND
                                    datbi GE p_datbi ).
*...EOC-T_T.KONNO
      ENDIF.
  ENDCASE.

ENDFORM.

FORM task_material_get CHANGING lt_glflcma  TYPE /agri/t_glflcma
                                lt_glcsprs  TYPE /agri/t_glcsprs
                                lt_glcsprso TYPE /agri/t_glcsprso
                                lt_mast     TYPE /agri/t_gmast_so
                                lt_stpo     TYPE tt_ccm_bomitem.


  IF gs_acdoc_infocus-x-acitm[] IS NOT INITIAL.
    SELECT * FROM /agri/glcsprs
                INTO CORRESPONDING FIELDS OF TABLE lt_glcsprs
         FOR ALL ENTRIES IN gs_acdoc_infocus-x-acitm[]
                            WHERE tplnr_fl = gs_acdoc_infocus-x-acitm-tplnr_fl
                             AND contr     = gs_acdoc_infocus-x-acitm-contr.
  ENDIF.
  IF lt_glcsprs[] IS NOT INITIAL.
    SELECT * FROM /agri/glcsprso
                INTO CORRESPONDING FIELDS OF TABLE lt_glcsprso
         FOR ALL ENTRIES IN lt_glcsprs[]
                            WHERE tplnr_fl = lt_glcsprs-tplnr_fl
                             AND contr     = lt_glcsprs-contr.
  ENDIF.
  IF lt_glcsprs IS NOT INITIAL.
    SELECT * FROM mast
                 INTO CORRESPONDING FIELDS OF TABLE lt_mast
       FOR ALL ENTRIES IN lt_glcsprs
                          WHERE
  ( matnr EQ lt_glcsprs-matnr )
AND ( stlan EQ '1' ).

  ENDIF.

  IF lt_mast IS NOT INITIAL.
    SELECT * FROM stpo
                   INTO CORRESPONDING FIELDS OF TABLE lt_stpo
         FOR ALL ENTRIES IN lt_mast
                            WHERE
      ( stlnr    EQ lt_mast-stlnr )
  AND ( stlty    EQ 'M' ).

  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_TERRAIN_PLANT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_MOD_ROW_VALUE
*&---------------------------------------------------------------------*
FORM check_terrain_plant USING lv_tplnr_fl
*...BOC-T_T.KONNO
*                        CHANGING lv_subrc.
                      CHANGING lt_safras TYPE type_safras_tab
                               lv_subrc  TYPE sysubrc.
*...EOC-T_T.KONNO

  DATA: lt_acitm   TYPE zt_fmacitm,
        lv_terrain TYPE /agri/gltplnr_fl,
        dummy.

  IF ok_code NE 'DLRW'.
    gs_variables-initiator = c_log_initiator-check.
    PERFORM messages_initialize USING gs_variables-initiator
                                      c_log_subobject-check
                                      gs_acdoc_infocus-x-achdr.

    lv_terrain = lv_tplnr_fl.

    CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
      EXPORTING
        input      = lv_tplnr_fl
      IMPORTING
        output     = lv_terrain
      EXCEPTIONS
        not_found  = 1
        not_active = 2
        OTHERS     = 3.

    IF sy-subrc EQ 0.
*...BOC-T_T.KONNO
*      SELECT SINGLE tplnr_fl, iwerk, swerk
*        FROM /agri/glflot
*        INTO @DATA(lwa_glflot)
*       WHERE tplnr_fl EQ @lv_terrain.
*      IF sy-subrc NE 0
*      OR ( sy-subrc EQ 0 AND
*          gs_acdoc_infocus-x-achdr-werks NE lwa_glflot-iwerk ).
**...Talhão &1 não existe no Centro &2
*        MESSAGE e090(zfmac) WITH lv_tplnr_fl
*          gs_acdoc_infocus-x-achdr-werks INTO sy-msgli.
*        message_simple c_false.
*        gs_variables-errors = c_true.
*        lv_subrc = 4.
*      ENDIF.
      SELECT SINGLE tplnr_fl, iwerk, swerk
        FROM /agri/glflot
        INTO @DATA(lwa_glflot)
       WHERE tplnr_fl EQ @lv_terrain
         AND iwerk    IN @so_werks[].
      IF sy-subrc NE 0.
        READ TABLE so_werks INTO DATA(lwa_werks) INDEX 1.
        IF sy-subrc EQ 0.
          CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
            EXPORTING
              input  = lv_tplnr_fl
            IMPORTING
              output = lv_tplnr_fl.
**........Talhão &1 não existe no Centro &2
          MESSAGE e090(zfmac) WITH lv_tplnr_fl lwa_werks-low INTO sy-msgli.
          message_simple c_false.
          gs_variables-errors = abap_true.
          lv_subrc = 4.
        ENDIF.
      ELSE.
        SELECT tplnr_fl, contr, datab, datbi
          FROM /agri/glflcma
*          INTO TABLE @DATA(lt_safras)
          INTO TABLE @lt_safras
         WHERE tplnr_fl EQ @lv_terrain
           AND astat    EQ @c_crop_season_status-active
           AND loevm    NE @abap_true.

*        DELETE lt_safras WHERE datab GT p_datab
*                            OR datbi LT p_datbi.
        DELETE lt_safras WHERE NOT ( datab BETWEEN p_datab AND p_datbi )
                           AND NOT ( datbi BETWEEN p_datab AND p_datbi )
                           AND NOT ( datab LE p_datab AND
                                     datbi GE p_datbi ).

        IF lt_safras[] IS INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
            EXPORTING
              input  = lv_tplnr_fl
            IMPORTING
              output = lv_tplnr_fl.
**........Não existe safra válida para o Terreno &1 no período informado.
          MESSAGE e091(zfmac) WITH lv_tplnr_fl INTO sy-msgli.
          message_simple c_false.
          gs_variables-errors = abap_true.
          lv_subrc = 4.
        ENDIF.
      ENDIF.
*...EOC-T_T.KONNO
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_VALID_CROP_SEASON
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_MOD_ROW_VALUE
*&---------------------------------------------------------------------*
FORM check_valid_crop_season USING lv_tplnr_fl
                          CHANGING lt_new_line TYPE zt_fmacitm
                                   lt_seasons  TYPE type_seasons_tab
                                   lv_season   TYPE /agri/glflcma-season
                                   lv_subrc    TYPE sysubrc.

  TYPES: BEGIN OF ly_values,
           season TYPE /agri/glflcma-season,
         END OF ly_values.

  TYPES: BEGIN OF ly_field.
      INCLUDE STRUCTURE help_value.
  TYPES: END OF ly_field.

  DATA: lt_rmg_tplnr_fl     TYPE zfmac_terrain_range_t,
        lt_tempfmacitm      TYPE zt_fmacitm,
        lt_acitm            TYPE zt_fmacitm,
        lt_field            TYPE STANDARD TABLE OF ly_field INITIAL SIZE 0,
        lt_values           TYPE STANDARD TABLE OF ly_values INITIAL SIZE 0,
        lr_assigned_seasons TYPE RANGE OF /agri/gl_season,
        lv_terrain          TYPE /agri/gltplnr_fl,
        dummy.

  CONSTANTS: BEGIN OF c_status,
               criado    TYPE zfmacitst VALUE ' ',
               agendado  TYPE zfmacitst VALUE 'A',
               planejado TYPE zfmacitst VALUE 'B',
             END OF c_status.

  IF ok_code NE 'DLRW'.
    gs_variables-initiator = c_log_initiator-check.
    PERFORM messages_initialize USING gs_variables-initiator
                                      c_log_subobject-check
                                      gs_acdoc_infocus-x-achdr.

    lv_terrain = lv_tplnr_fl.

    CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
      EXPORTING
        input      = lv_tplnr_fl
      IMPORTING
        output     = lv_terrain
      EXCEPTIONS
        not_found  = 1
        not_active = 2
        OTHERS     = 3.

    IF sy-subrc EQ 0.
      IF lt_new_line[] IS NOT INITIAL.
        SELECT * FROM zfmaitm AS m
          INNER JOIN zfmachdr AS r
          ON m~acnum EQ r~acnum
          INTO CORRESPONDING FIELDS OF TABLE @lt_tempfmacitm
          FOR ALL ENTRIES IN @lt_new_line[]
         WHERE m~tplnr_fl EQ @lt_new_line-tplnr_fl
           AND m~acetm    NE @c_status-planejado.

        LOOP AT lt_tempfmacitm INTO DATA(lwa_item).
          INSERT INITIAL LINE INTO TABLE lr_assigned_seasons
            ASSIGNING FIELD-SYMBOL(<lwa_season>).
          IF sy-subrc EQ 0.
            <lwa_season> = 'IEQ'.
            <lwa_season>-low = lwa_item-season.
          ENDIF.
        ENDLOOP.
      ENDIF.

      SELECT SINGLE tplnr_fl, iwerk, swerk
        FROM /agri/glflot
        INTO @DATA(lwa_glflot)
       WHERE tplnr_fl EQ @lv_terrain
         AND iwerk    IN @so_werks[].
      IF sy-subrc NE 0.
        READ TABLE so_werks INTO DATA(lwa_werks) INDEX 1.
        IF sy-subrc EQ 0.
          CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
            EXPORTING
              input  = lv_tplnr_fl
            IMPORTING
              output = lv_tplnr_fl.
**........Talhão &1 não existe no Centro &2
          MESSAGE e090(zfmac) WITH lv_tplnr_fl lwa_werks-low INTO sy-msgli.
          message_simple c_false.
          gs_variables-errors = abap_true.
          lv_subrc = 4.
        ENDIF.
      ELSE.
        SELECT tplnr_fl, cmnum, varia, season, datab, datbi
          FROM /agri/glflcma
          INTO TABLE @lt_seasons
         WHERE tplnr_fl EQ @lv_terrain
           AND astat    EQ @c_crop_season_status-active
           AND loevm    NE @abap_true.

        DELETE lt_seasons WHERE NOT ( datab BETWEEN p_datab AND p_datbi )
                            AND NOT ( datbi BETWEEN p_datab AND p_datbi )
                            AND NOT ( datab LE p_datab AND
                                      datbi GE p_datbi ).

        DELETE lt_seasons WHERE season IN lr_assigned_seasons[].

        SORT lt_seasons BY tplnr_fl varia.
        DELETE ADJACENT DUPLICATES FROM lt_seasons COMPARING tplnr_fl season.

        IF lt_seasons[] IS INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
            EXPORTING
              input  = lv_tplnr_fl
            IMPORTING
              output = lv_tplnr_fl.
*-- Não existe safra válida para o Terreno &1 no período informado.
          MESSAGE e091(zfmac) WITH lv_tplnr_fl INTO sy-msgli.
          message_simple c_false.
          gs_variables-errors = abap_true.
          lv_subrc = 4.
        ELSE.
          IF lines( lt_seasons ) GT 1.
            INSERT INITIAL LINE INTO TABLE lt_field
              ASSIGNING FIELD-SYMBOL(<lwa_field>).
            IF sy-subrc EQ 0.
              <lwa_field>-tabname    = '/AGRI/GLFLCMA'.
              <lwa_field>-fieldname  = 'SEASON'.
              <lwa_field>-selectflag = abap_true.
            ENDIF.

            LOOP AT lt_seasons INTO DATA(lwa_season).
              INSERT INITIAL LINE INTO TABLE lt_values
                ASSIGNING FIELD-SYMBOL(<lwa_value>).
              IF sy-subrc EQ 0.
                <lwa_value>-season   = lwa_season-season.
              ENDIF.
            ENDLOOP.

            CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE'
              IMPORTING
                select_value              = lv_season
              TABLES
                fields                    = lt_field
                valuetab                  = lt_values
              EXCEPTIONS
                field_not_in_ddic         = 4
                more_then_one_selectfield = 4
                no_selectfield            = 4
                OTHERS                    = 99. "#EC FB_OLDED

            IF lv_season IS INITIAL.
*-- Não é permitida a atribuição de talhão sem selecionar a safra!
              MESSAGE e095(zfmac).
            ELSE.
              CLEAR lv_subrc.
            ENDIF.
          ELSE.
            READ TABLE lt_seasons INTO lwa_season INDEX 1.
            IF sy-subrc EQ 0.
              lv_season = lwa_season-season.
              CLEAR lv_subrc.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F4_TERRAINS_VALUES_GET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_DISPLAY
*&      <-- LV_VALUE
*&      <-- LV_CHANGE
*&      <-- LV_SUBRC
*&---------------------------------------------------------------------*
FORM f4_terrains_values_get USING lv_display
                         CHANGING lv_value TYPE /agri/gltplnr_fl
                                  lv_change
                                  lv_subrc TYPE sy-subrc.

  TYPES: BEGIN OF ly_acitm,
           tplnr_fl TYPE /agri/gltplnr_fl,
           pltxt    TYPE /agri/glpltxt,
           iwerk    TYPE iwerk,
           cmnum    TYPE /agri/glcmnum,
           varia    TYPE /agri/glvaria,
           season   TYPE /agri/gl_season,
           datab    TYPE /agri/gldatab,
           datbi    TYPE /agri/gldatbi,
         END OF ly_acitm.

  DATA: lt_return    TYPE TABLE OF ddshretval INITIAL SIZE 0,
        lt_acitm     TYPE TABLE OF ly_acitm,
        lt_terrains  TYPE zt_fmacterr_tab,
        lt_mapping   TYPE TABLE OF dselc,
        lv_fieldname TYPE fieldname,
        lv_title(80).

  SELECT t~tplnr_fl, t~pltxt, t~iwerk,
         a~cmnum, a~varia, a~season, a~datab, a~datbi
    FROM /agri/glflcma AS a
    INNER JOIN /agri/glflot AS t
    ON a~tplnr_fl EQ t~tplnr_fl
    INTO CORRESPONDING FIELDS OF TABLE @lt_acitm
   WHERE t~iwerk IN @so_werks[]
     AND a~loevm NE @abap_true
     AND a~astat EQ @c_crop_season_status-active.

  IF lt_acitm[] IS NOT INITIAL.
    lv_title = TEXT-019.
    CONDENSE lv_title.

    lv_fieldname = 'TPLNR_FL'.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = lv_fieldname
        window_title    = lv_title
        value_org       = 'S'
        display         = lv_display
      TABLES
        value_tab       = lt_acitm[]
        return_tab      = lt_return[]
        dynpfld_mapping = lt_mapping[]
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    IF sy-subrc EQ 0.
      READ TABLE lt_return INTO DATA(lwa_return) INDEX 1.
      IF sy-subrc EQ 0.
        lv_change = abap_true.
        lv_value = lwa_return-fieldval.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
