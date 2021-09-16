*----------------------------------------------------------------------*
***INCLUDE /AGRI/LFMACMNF0V .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form VLCL_GRID_DATA_PREPARE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM vlcl_grid_data_prepare.

  DATA:lwa_acvlc     TYPE zsc_fmacvlcl,
       lwa_vlcl_fcat LIKE LINE OF gt_fmacvlc_fcat.

  CHECK ref_grid_vlcl IS INITIAL
     OR gs_variables-refresh_vlc_grid EQ c_true.

  REFRESH: gt_fmacvlc_fcat.

*...BOC-T_T.KONNO
  SORT gs_acdoc_infocus-x-acvlc BY tplnr_fl matnr.
*...EOC-T_T.KONNO

  LOOP AT gs_acdoc_infocus-x-acvlc INTO lwa_acvlc.
    MOVE-CORRESPONDING lwa_acvlc TO lwa_vlcl_fcat.
    APPEND lwa_vlcl_fcat TO gt_fmacvlc_fcat.
    CLEAR lwa_vlcl_fcat.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form VLCL_GRID_UPDATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM vlcl_grid_update .

  DATA: lwa_vlcl       TYPE zsc_fmacvlcl,
        lwa_vcl_layout LIKE LINE OF gt_fmacvlc_fcat,
        ls_fmvlcl      TYPE zsc_fmacvlcl,
        lt_vlcl        TYPE zt_fmacvlcl,
        lwa_mod_row    TYPE lvc_s_modi,
        lv_typ         TYPE char2 VALUE '02',
        lv_tabix       TYPE sy-tabix,
        lv_modified,
        lv_subrc       TYPE int4,
        lv_valid.

  DATA:
    lt_glmdhdr TYPE /agri/t_glmdhdr,
    lt_glmdatv TYPE /agri/t_glmdatv.

  FIELD-SYMBOLS: <lwa_vlcl>       TYPE zsc_fmacvlcl,
                 <lwa_vcl_layout> TYPE zsc_fmacvlcl_fcat.

  FIELD-SYMBOLS:
    <lwa_glmdhdr> TYPE /agri/s_glmdhdr,
    <lwa_glmdatv> TYPE /agri/s_glmdatv.

  CHECK gs_variables-document_mode NE c_mode_display.
  CLEAR gs_variables-errors.
  gs_variables-initiator = c_log_initiator-check.
  PERFORM messages_initialize USING gs_variables-initiator
                                    c_log_subobject-check
                                    gs_acdoc_infocus-x-achdr.

  lv_modified = ref_grid_vlcl->data_modified_check( ).
  IF lv_modified EQ c_true OR
  gs_variables-manual_changes IS NOT INITIAL.
    CALL METHOD ref_grid_vlcl->check_changed_data
      IMPORTING
        e_valid = lv_valid.
    IF lv_valid IS INITIAL.
      CLEAR ok_code.
      EXIT.
    ELSE.
      gs_variables-refresh_vlc_grid = c_true.
    ENDIF.
  ENDIF.
*--- Measurement document
  PERFORM measurement_document_get CHANGING lt_glmdatv
                                            lt_glmdhdr.

  LOOP AT gt_items_modi_vlcl INTO lwa_mod_row.
    lv_tabix = sy-tabix.
    READ TABLE gt_fmacvlc_fcat INTO lwa_vcl_layout INDEX lwa_mod_row-row_id.
    IF lwa_mod_row-fieldname EQ 'ACIDT'.
*...BOC-T_T.KONNO
*      PERFORM volumen_calda_calculate USING lwa_vcl_layout-tplnr_fl
*                                            lwa_vcl_layout-acidt
      PERFORM volumen_calda_calculate USING lwa_vcl_layout
*...EOC-T_T.KONNO
                                            lt_glmdhdr[]
                                            lt_glmdatv[]
                                   CHANGING lwa_vcl_layout-acvcl.
    ENDIF.
*...BOC-T_T.KONNO
*    IF lwa_vcl_layout-acvcl IS NOT INITIAL
*    AND lwa_vcl_layout-acvcl GT 0.
*      lwa_vcl_layout-acqtb = ( lwa_vcl_layout-acdis / lwa_vcl_layout-acvcl ).
*      MOVE c_unit_of_measurement-unit TO lwa_vcl_layout-acuqb.
*      IF gs_acdoc_infocus-acnum IS INITIAL.
*        gs_acdoc_infocus-acnum = gs_acdoc_infocus-x-achdr-acnum.
*      ENDIF.
*    ENDIF.
*...Nro.Bomba = Volume de Calda/Capacidade de Bomba
    IF lwa_vcl_layout-acdis IS NOT INITIAL.
      lwa_vcl_layout-acqtb = lwa_vcl_layout-acvcl / lwa_vcl_layout-acdis.
      MOVE c_unit_of_measurement-unit TO lwa_vcl_layout-acuqb.
    ENDIF.
    IF gs_acdoc_infocus-acnum IS INITIAL.
      gs_acdoc_infocus-acnum = gs_acdoc_infocus-x-achdr-acnum.
    ENDIF.
*...EOC-T_T.KONNO

    IF lwa_mod_row-fieldname EQ 'ACPEX'.
      READ TABLE gs_acdoc_infocus-x-acitm INTO DATA(lwa_acitm)
                                          WITH KEY tplnr_fl = lwa_vcl_layout-tplnr_fl.
      IF sy-subrc EQ 0.
        CHECK lwa_vcl_layout-acpex IS NOT INITIAL.
        lwa_vcl_layout-acarx = ( ( lwa_vcl_layout-acpex * lwa_acitm-aarea ) / 100 ). " Area a executar
        IF lwa_vcl_layout-acarx IS NOT INITIAL.
          lwa_vcl_layout-unarx = c_unit_of_measurement-hectare. " Area a executar unidad
        ENDIF.
      ENDIF.
    ENDIF.
    CHECK lwa_vcl_layout IS NOT INITIAL.
    MOVE-CORRESPONDING lwa_vcl_layout TO lwa_vlcl.
    READ TABLE gs_acdoc_infocus-x-acvlc ASSIGNING <lwa_vlcl>
                                      WITH KEY acnum = lwa_vcl_layout-acnum
                                      vornr = lwa_vcl_layout-vornr
                                      matnr = lwa_vcl_layout-matnr
                                      posnr = lwa_vcl_layout-posnr
                                      tplnr_fl = lwa_vcl_layout-tplnr_fl.
    IF sy-subrc EQ 0.
      IF lwa_vlcl NE <lwa_vlcl>.
        gs_variables-document_changed = c_true.
        IF <lwa_vlcl>-updkz NE c_updkz_new.
          MOVE lwa_vlcl TO <lwa_vlcl>.
          <lwa_vlcl>-updkz = c_updkz_update.
        ELSE.
          MOVE lwa_vlcl TO <lwa_vlcl>.
          <lwa_vlcl>-updkz = c_updkz_new.
        ENDIF.
      ENDIF.
    ELSE.
      MOVE zsc_fmachdr-acnum TO gs_acdoc_infocus-acnum.
      gs_variables-refresh_items_grid = c_true.
      gs_variables-document_changed = c_true.
      PERFORM new_position_set USING lwa_vcl_layout-posnr.
      lwa_vcl_layout-acnum = gs_acdoc_infocus-acnum.
      lwa_vcl_layout-updkz = c_updkz_new.
      MOVE-CORRESPONDING lwa_vcl_layout TO lwa_vlcl.
      APPEND lwa_vlcl TO gs_acdoc_infocus-x-acvlc.
    ENDIF.
    DELETE gt_items_modi_vlcl INDEX lv_tabix.
  ENDLOOP.

  CHECK gs_variables-errors IS NOT INITIAL.
  PERFORM messages_display USING gs_variables-initiator.
  IF gs_variables-errors IS NOT INITIAL.
    CLEAR: ok_code, gs_variables-errors.
    IF gs_variables-refresh_vlc_grid IS NOT INITIAL.
      CLEAR gs_variables-refresh_vlc_grid.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form VOLUMEN_CALDA_CREATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM volumen_calda_create.
  DATA:
    lt_glflcma  TYPE /agri/t_glflcma,
    lt_glcsprs  TYPE /agri/t_glcsprs,
    lt_glcsprso TYPE /agri/t_glcsprso,
    lt_mast     TYPE /agri/t_gmast_so,
    lt_stpo     TYPE tt_ccm_bomitem.


  PERFORM task_material_get CHANGING lt_glflcma
                                     lt_glcsprs
                                     lt_glcsprso
                                     lt_mast
                                     lt_stpo.


  PERFORM volumen_calda_fill USING   lt_glflcma
                                     lt_glcsprs
                                     lt_glcsprso
                                     lt_mast
                                     lt_stpo.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form VOLUMEN_CALDA_FILL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_GLFLCMA
*&      --> LT_GLCSPRS
*&      --> LT_GLCSPRSO
*&      --> LT_MAST
*&      --> LT_STPO
*&---------------------------------------------------------------------*
FORM volumen_calda_fill  USING  lt_glflcma  TYPE /agri/t_glflcma
                                lt_glcsprs  TYPE /agri/t_glcsprs
                                lt_glcsprso TYPE /agri/t_glcsprso
                                lt_mast     TYPE /agri/t_gmast_so
                                lt_stpo     TYPE tt_ccm_bomitem.


  FIELD-SYMBOLS: <lwa_glflcma>  TYPE /agri/t_glflcma,
                 <lwa_glcsprs>  TYPE /agri/s_glcsprs,
                 <lwa_glcsprso> TYPE /agri/s_glcsprso,
                 <lwa_mast>     TYPE mast,
                 <lwa_stpo>     TYPE stpo.

  DATA: ls_acvlc TYPE zsc_fmacvlcl,
        lv_posnr TYPE int4.

  SORT: lt_glflcma  ASCENDING BY tplnr_fl,
        lt_glcsprs  ASCENDING BY tplnr_fl contr cpros,
        lt_glcsprso ASCENDING BY tplnr_fl contr cpros,
        lt_stpo     ASCENDING BY stlty stlnr posnr.

  LOOP AT lt_glcsprso ASSIGNING <lwa_glcsprso>.
    MOVE sy-tabix TO lv_posnr.
*...BOC-T_T.KONNO
*    ls_acvlc-acnum = TEXT-004.
    IF gs_acdoc_infocus-acnum IS NOT INITIAL.
      ls_acvlc-acnum = gs_acdoc_infocus-acnum.
    ELSE.
      ls_acvlc-acnum = TEXT-004.
    ENDIF.
*...EOC-T_T.KONNO
    ls_acvlc-updkz = c_updkz_new.
    ls_acvlc-vornr = <lwa_glcsprso>-vornr.
    ls_acvlc-maktx = <lwa_glcsprso>-ltxa1.
    ls_acvlc-tplnr_fl = <lwa_glcsprso>-tplnr_fl.
    ls_acvlc-acdis = c_program_constant-tank_capasity.
    ls_acvlc-posnr = ( lv_posnr * 10 ).
    READ TABLE lt_glcsprs  ASSIGNING <lwa_glcsprs>
      WITH KEY tplnr_fl = <lwa_glcsprso>-tplnr_fl
               contr    = <lwa_glcsprso>-contr
               cpros    = <lwa_glcsprso>-cpros BINARY SEARCH.
    IF sy-subrc EQ 0.
      READ TABLE lt_mast ASSIGNING <lwa_mast>
        WITH KEY matnr = <lwa_glcsprs>-matnr
                 werks = <lwa_glcsprso>-werks
                 stlan = '1' BINARY SEARCH.
      IF sy-subrc EQ 0.
        READ TABLE lt_stpo ASSIGNING <lwa_stpo>
          WITH KEY stlty = 'M'
                   stlnr = <lwa_mast>-stlnr
                   posnr = <lwa_glcsprso>-vornr BINARY SEARCH.
        IF sy-subrc EQ 0.
          MOVE <lwa_stpo>-idnrk TO ls_acvlc-matnr.
        ENDIF.
      ENDIF.
    ENDIF.
    APPEND ls_acvlc TO gs_acdoc_infocus-x-acvlc.
  ENDLOOP.

*...BOC-T_T.KONNO
  SORT gs_acdoc_infocus-x-acvlc BY tplnr_fl matnr.
*...EOC-T_T.KONNO

ENDFORM.
*&---------------------------------------------------------------------*
*& Form VOLUMEN_CALDA_CALCULATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ENDIF
*&---------------------------------------------------------------------*
*...BOC-T_T.KONNO
*FORM volumen_calda_calculate USING lv_tplnr   TYPE /agri/gltplnr_fl
*                                   lv_indic   TYPE zfmacidt
*                                   lt_glmdhdr TYPE /agri/t_glmdhdr
*                                   lt_glmdatv TYPE /agri/t_glmdatv
*                          CHANGING lv_volcal  TYPE zfmacvcl.
FORM volumen_calda_calculate USING lwa_vcl_layout  TYPE zsc_fmacvlcl_fcat
                                   lt_glmdhdr      TYPE /agri/t_glmdhdr
                                   lt_glmdatv      TYPE /agri/t_glmdatv
                          CHANGING lv_volume_calda TYPE zfmacvcl.

  DATA: ref_var TYPE REF TO cx_sy_arithmetic_overflow,
        lv_text TYPE string.
*...EOC-T_T.KONNO

*...BOC-T_T.KONNO
*  FIELD-SYMBOLS: <lwa_glmdhdr> TYPE /agri/s_glmdhdr,
*                 <lwa_glmdatv> TYPE /agri/s_glmdatv.
*
*  SORT: lt_glmdhdr ASCENDING BY tplnr_fl,
*        lt_glmdatv ASCENDING BY mdocm.
*
*  IF lt_glmdhdr[] IS NOT INITIAL.
*    READ TABLE lt_glmdhdr ASSIGNING <lwa_glmdhdr>
*                          WITH KEY tplnr_fl = lv_tplnr
*                          BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      READ TABLE lt_glmdatv  ASSIGNING <lwa_glmdatv>
*                            WITH KEY mdocm = <lwa_glmdhdr>-mdocm
*                            BINARY SEARCH.
*      IF sy-subrc EQ 0.
*        lv_volcal = ( lv_indic *  <lwa_glmdatv>-atflv ).
*      ENDIF.
*    ENDIF.
*  ENDIF.
  READ TABLE gs_acdoc_infocus-x-acitm INTO DATA(lwa_acitm)
    WITH KEY tplnr_fl = lwa_vcl_layout-tplnr_fl.
  IF sy-subrc EQ 0.
    IF lwa_acitm-advlc IS NOT INITIAL.
      TRY.
*Volume de Calda = Índice Técnico x Volume de Copa
          lv_volume_calda = lwa_vcl_layout-acidt * lwa_acitm-advlc.
        CATCH cx_sy_arithmetic_overflow INTO ref_var.
          lv_text = ref_var->get_text( ).
          MESSAGE lv_text TYPE 'I'.
      ENDTRY.
    ENDIF.
*Volume de Calda/HA =  Volume Total de Calda/Qtde. de hectares
    IF lwa_acitm-aarea IS NOT INITIAL.
      DATA(lv_calda_por_ha) = lv_volume_calda / lwa_acitm-aarea.
    ELSE.
      CLEAR lv_calda_por_ha.
    ENDIF.
    DATA(lv_limite) = 4000.
    IF lv_calda_por_ha GT lv_limite.
**....Para o Terreno &1/Tarefa &2, o Volume de Calda excede 4.000 m3/ha!
      MESSAGE e094(zfmac) WITH lwa_acitm-tplnr_fl lwa_vcl_layout-matnr.
    ENDIF.
  ENDIF.
*...EOC-T_T.KONNO

ENDFORM.

FORM individual_task_material_get USING lv_tplnr    TYPE /agri/gltplnr_fl
                                        lv_contr    TYPE /agri/gcontr
                               CHANGING lt_glflcma  TYPE /agri/t_glflcma
                                        lt_glcsprs  TYPE /agri/t_glcsprs
                                        lt_glcsprso TYPE /agri/t_glcsprso
                                        lt_mast     TYPE /agri/t_gmast_so
                                        lt_stpo     TYPE tt_ccm_bomitem.

  READ TABLE gt_fmacitm_fcat
    WITH KEY tplnr_fl = lv_tplnr TRANSPORTING NO FIELDS.

  IF sy-subrc EQ 0.
    SELECT * FROM /agri/glcsprs
      INTO CORRESPONDING FIELDS OF TABLE lt_glcsprs
     WHERE tplnr_fl = lv_tplnr
       AND contr    = lv_contr.
  ENDIF.

  IF lt_glcsprs[] IS NOT INITIAL.
    SELECT * FROM /agri/glcsprso
      INTO CORRESPONDING FIELDS OF TABLE lt_glcsprso
      FOR ALL ENTRIES IN lt_glcsprs[]
     WHERE tplnr_fl = lt_glcsprs-tplnr_fl
      AND contr     = lt_glcsprs-contr.
  ENDIF.

*...BOC-T_T.KONNO
  DELETE: lt_glcsprs  WHERE cpros EQ 'COLHEITA',
          lt_glcsprso WHERE cpros EQ 'COLHEITA'.
*...EOC-T_T.KONNO

  IF lt_glcsprs IS NOT INITIAL.
    SELECT * FROM mast
      INTO CORRESPONDING FIELDS OF TABLE lt_mast
      FOR ALL ENTRIES IN lt_glcsprs
     WHERE matnr EQ lt_glcsprs-matnr
       AND stlan EQ '1'.
  ENDIF.

  IF lt_mast[] IS NOT INITIAL.
    SELECT * FROM stpo
      INTO CORRESPONDING FIELDS OF TABLE lt_stpo
      FOR ALL ENTRIES IN lt_mast
     WHERE stlnr EQ lt_mast-stlnr
       AND stlty EQ 'M'.
  ENDIF.

ENDFORM.

FORM volumen_calda_massupdate.

  DATA: lt_rows          TYPE lvc_t_row,
        lwa_row          TYPE lvc_s_row,
        lv_answer        TYPE c,
        lwa_acitm_layout LIKE LINE OF gt_fmacitm_fcat,
        lt_glmdhdr       TYPE /agri/t_glmdhdr,
        lt_glmdatv       TYPE /agri/t_glmdatv.

  FIELD-SYMBOLS: <lwa_vlcl_modi> TYPE lvc_s_modi,
                 <lwa_vlcl>      TYPE zsc_fmacvlcl_fcat,
                 <lwa_glmdhdr>   TYPE /agri/s_glmdhdr,
                 <lwa_glmdatv>   TYPE /agri/s_glmdatv.

  CHECK sy-dynnr NE '0202'.
*--- Measurement document.
  PERFORM measurement_document_get CHANGING lt_glmdatv
                                            lt_glmdhdr.
  CALL METHOD ref_grid_vlcl->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.
  CHECK lt_rows IS NOT INITIAL.
  gs_variables-refresh_vlc_grid = c_true.
  SORT lt_rows BY index DESCENDING.
  LOOP AT lt_rows INTO lwa_row.
    READ TABLE  gt_fmacvlc_fcat ASSIGNING <lwa_vlcl>
                                INDEX lwa_row-index.
    IF zsc_fmacvlcl-acfcb IS NOT INITIAL.
      <lwa_vlcl>-acfcb = zsc_fmacvlcl-acfcb.  " Factor de bomba
    ENDIF.
    IF zsc_fmacvlcl-acidt IS NOT INITIAL.
      <lwa_vlcl>-acidt = zsc_fmacvlcl-acidt. " Indice tecnico
    ENDIF.
    IF zsc_fmacvlcl-acdis IS NOT INITIAL.
      <lwa_vlcl>-acdis = zsc_fmacvlcl-acdis. " Desemvolvido en litro
    ENDIF.
    IF zsc_fmacvlcl-acren IS NOT INITIAL.
      <lwa_vlcl>-acren = zsc_fmacvlcl-acren.  " Rendimento
    ENDIF.
    IF zsc_fmacvlcl-unren IS NOT INITIAL.
      <lwa_vlcl>-unren = zsc_fmacvlcl-unren.  " Rendimento Unidad
    ENDIF.
    IF zsc_fmacvlcl-acpex IS NOT INITIAL.
      <lwa_vlcl>-acpex = zsc_fmacvlcl-acpex. " % Execucao
    ENDIF.

    READ TABLE gs_acdoc_infocus-x-acitm INTO DATA(lwa_acitm)
                                        WITH KEY tplnr_fl = <lwa_vlcl>-tplnr_fl.
    IF sy-subrc EQ 0.
      <lwa_vlcl>-acarx = ( ( <lwa_vlcl>-acpex * lwa_acitm-aarea ) / 100 ). " Area a executar
      IF <lwa_vlcl>-acarx IS NOT INITIAL.
        <lwa_vlcl>-unarx = c_unit_of_measurement-hectare. " Area a executar unidad
      ENDIF.
    ENDIF.
    IF <lwa_vlcl>-acarx IS NOT INITIAL.
      <lwa_vlcl>-unarx = c_unit_of_measurement-hectare. " Area a executar unidad
    ENDIF.
*...BOC-T_T.KONNO
*    PERFORM volumen_calda_calculate USING <lwa_vlcl>-tplnr_fl
*                                          <lwa_vlcl>-acidt
    PERFORM volumen_calda_calculate USING <lwa_vlcl>
*...EOC-T_T.KONNO
                                          lt_glmdhdr[]
                                          lt_glmdatv[]
                                 CHANGING <lwa_vlcl>-acvcl.

*...BOC-T_T.KONNO
*    IF <lwa_vlcl>-acvcl IS NOT INITIAL
*    AND <lwa_vlcl>-acvcl GT 0.
*      <lwa_vlcl>-acqtb = ( <lwa_vlcl>-acdis / <lwa_vlcl>-acvcl  ).
*      MOVE c_unit_of_measurement-unit TO <lwa_vlcl>-acuqb.
*    ENDIF.
*...Nro.Bomba = Volume de Calda/Capacidade de Bomba
    IF <lwa_vlcl>-acdis IS NOT INITIAL.
      <lwa_vlcl>-acqtb = <lwa_vlcl>-acvcl / <lwa_vlcl>-acdis.
      MOVE c_unit_of_measurement-unit TO <lwa_vlcl>-acuqb.
    ENDIF.
*...EOC-T_T.KONNO
  ENDLOOP.

  PERFORM update_mass_volumen_calda.

  CLEAR:zsc_fmacvlcl.
ENDFORM.
