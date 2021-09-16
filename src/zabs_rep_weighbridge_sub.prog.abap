************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name    : ZABS_REP_WEIGHBRIDGE_SUB                            *
* Tcode          : ZABS_BALANCA                                        *
* Created By     : Helio Kababe                                        *
* Requested by   : Ricardo Genovez                                     *
* Created on     : 09.03.2020                                          *
* TR             : C4DK909167                                          *
* Version        : 001                                                 *
* Description    : Weighbridge data declaration and Selection Screen   *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*
*----------------------------------------------------------------------*
***INCLUDE ZABS_REP_WEIGHBRIDGE_SUB.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form INITIALIZE_GLOBAL_DATA
*&---------------------------------------------------------------------*
FORM initialize_global_data.

  REFRESH: gt_fieldcat, gt_weighbridge.

  CLEAR: gs_layout.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_WEIGHBRIDGE_DATA
*&---------------------------------------------------------------------*
FORM get_weighbridge_data.

  DATA: lrt_werks TYPE RANGE OF werks_d,
        lv_em_101 TYPE bwart VALUE '101'.

*--Fetching Weighbridge Data
  SELECT *
    FROM /agri/fmprhdr
    INTO TABLE @DATA(lt_fmprhdr)
   WHERE prnum           IN @s_prnum[]
     AND fldty           IN @s_fldty[]
     AND werks           IN @s_werks[]
     AND status          IN @s_status[]
     AND budat           IN @s_budat[]
     AND lic_plate       IN @s_plate[]
     AND zarrend         IN @s_arrend[]
     AND viagem_original IN @s_viagem[].

  IF p_merc EQ abap_true.
    DELETE lt_fmprhdr WHERE fruta_mercado IS INITIAL.
  ENDIF.

  IF p_tipo IS NOT INITIAL.
    DELETE lt_fmprhdr WHERE tipo_viagem NE p_tipo.
  ENDIF.

  IF lt_fmprhdr[] IS INITIAL.
*--Não existem dados para os parâmetros informados
    MESSAGE i061(zfmfp).
    LEAVE LIST-PROCESSING.
  ELSE.
    LOOP AT lt_fmprhdr INTO DATA(lwa_fmprhdr).
      IF lwa_fmprhdr-werks IS NOT INITIAL.
        READ TABLE lrt_werks TRANSPORTING NO FIELDS
          WITH KEY low = lwa_fmprhdr-werks.
        IF sy-subrc NE 0.
          INSERT INITIAL LINE INTO TABLE lrt_werks
            ASSIGNING FIELD-SYMBOL(<lrs_werks>).
          IF sy-subrc EQ 0.
            <lrs_werks> = 'IEQ'.
            <lrs_werks>-low = lwa_fmprhdr-werks.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

    SORT lrt_werks BY low.

    SELECT *
      FROM /agri/fmpritm
      INTO TABLE @DATA(lt_fmpritm)
      FOR ALL ENTRIES IN @lt_fmprhdr
     WHERE prnum     EQ @lt_fmprhdr-prnum
       AND gjahr     EQ @lt_fmprhdr-gjahr
       AND tplnr     IN @s_tplnr[]
       AND charg     IN @s_lote[]
       AND zzldlifnr IN @s_lider[]
       AND zzsrebo1  IN @s_rebo1[]
       AND zzhbatch  IN @s_lotec[].

    DATA(lt_lotes) = lt_fmpritm[].
    SORT lt_lotes BY charg.
    DELETE lt_lotes WHERE charg IS INITIAL.
    DELETE ADJACENT DUPLICATES FROM lt_lotes COMPARING charg.

    IF lt_lotes[] IS NOT INITIAL.
      SELECT meins, menge, budat, mblnr, mjahr,
             zeile, werks, bwart, matnr, lgort,
             charg, bwtar, wempf
        FROM matdoc
        INTO TABLE @DATA(lt_matdoc)
        FOR ALL ENTRIES IN @lt_lotes
       WHERE werks IN @lrt_werks[]
         AND bwart EQ @lv_em_101
         AND charg EQ @lt_lotes-charg.

      SORT lt_matdoc BY charg ASCENDING
                        mblnr DESCENDING
                        mjahr DESCENDING.
    ENDIF.

    SORT lt_fmpritm BY prnum gjahr pritm.

    LOOP AT lt_fmprhdr INTO lwa_fmprhdr.
      READ TABLE lt_fmpritm INTO DATA(lwa_fmpritm)
        WITH KEY prnum = lwa_fmprhdr-prnum
                 gjahr = lwa_fmprhdr-gjahr BINARY SEARCH.
      WHILE sy-subrc EQ 0.
        DATA(lv_tabix) = sy-tabix + 1.
        INSERT INITIAL LINE INTO TABLE gt_weighbridge
          ASSIGNING FIELD-SYMBOL(<lwa_weighbridge>).
        IF sy-subrc EQ 0.
          READ TABLE lt_matdoc INTO DATA(lwa_matdoc)
            WITH KEY charg = lwa_fmpritm-charg BINARY SEARCH.
          IF sy-subrc NE 0.
            CLEAR lwa_matdoc.
          ENDIF.
          MOVE-CORRESPONDING lwa_fmpritm TO <lwa_weighbridge>.
          CONDENSE lwa_fmpritm-zzdata_colheita.
          IF lwa_fmpritm-zzdata_colheita EQ '0'.
            CLEAR <lwa_weighbridge>-zzdata_colheita.
          ENDIF.
          MOVE-CORRESPONDING lwa_fmprhdr TO <lwa_weighbridge>.
          <lwa_weighbridge>-calwgth_hdr = lwa_fmprhdr-calwgth.
          <lwa_weighbridge>-tplnr = lwa_fmpritm-tplnr.
          <lwa_weighbridge>-data_balanca = lwa_matdoc-budat.
        ENDIF.
        READ TABLE lt_fmpritm INTO lwa_fmpritm INDEX lv_tabix
          COMPARING prnum gjahr.
      ENDWHILE.
    ENDLOOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_WEIGHBRIDGE_DATA
*&---------------------------------------------------------------------*
FORM display_weighbridge_data.

*--Calling screen to display wighbridge data
  IF gt_weighbridge[] IS NOT INITIAL.
    CALL SCREEN 100.
  ENDIF.

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
FORM fcode_processing .

  CALL METHOD cl_gui_cfw=>dispatch.
  CASE ok_code.
    WHEN 'ECAN'
      OR 'ENDE'
      OR 'E'.
      PERFORM exit_program.
  ENDCASE.
  CLEAR ok_code.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form EXIT_PROGRAM
*&---------------------------------------------------------------------*
FORM exit_program .

  SET SCREEN '0'.
  LEAVE SCREEN.

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
FORM status_set.

**--Internal table declaration
*  DATA lt_exclude TYPE STANDARD TABLE OF sy-ucomm.
*
**--Status set for screen 100
*  SET PF-STATUS 'S100' EXCLUDING lt_exclude.

ENDFORM.

*&---------------------------------------------------------------------*
*& Module TITLE_SET OUTPUT
*&---------------------------------------------------------------------*
MODULE title_set OUTPUT.

  PERFORM title_set.

ENDMODULE.

*&---------------------------------------------------------------------*
*& Form TITLE_SET
*&---------------------------------------------------------------------*
FORM title_set .

*--Local variable declaration
  DATA lv_text(10).

*--Title set for screen 100
  SET TITLEBAR 'T100' WITH lv_text.

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
FORM controls_display .

  DATA: lt_f4      TYPE lvc_t_f4,
        ls_f4      TYPE lvc_s_f4,
        ls_variant TYPE disvariant,
        lt_buttons TYPE ui_functions,
        ls_button  TYPE ui_func.

  PERFORM prepare_catalog.
  PERFORM prepare_layout.

  ls_variant-report = sy-repid.
  ls_variant-handle = 'GRID'.

  IF go_custom_container IS INITIAL.
    CREATE OBJECT go_custom_container
      EXPORTING
        container_name = go_container.

    IF go_grid IS INITIAL.
      CREATE OBJECT go_grid
        EXPORTING
          i_parent = go_custom_container.

      CALL METHOD go_grid->set_table_for_first_display
        EXPORTING
          is_variant                    = ls_variant
          i_save                        = 'A'
          i_default                     = abap_true
          is_layout                     = gs_layout
          it_toolbar_excluding          = lt_buttons
          i_structure_name              = 'ZABS_STR_WEIGHBRIDGE'
        CHANGING
          it_outtab                     = gt_weighbridge
          it_fieldcatalog               = gt_fieldcat[]
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.
    ELSE.
      CALL METHOD go_grid->get_frontend_layout
        IMPORTING
          es_layout = gs_layout.

      CALL METHOD go_grid->set_frontend_layout
        EXPORTING
          is_layout = gs_layout.

      CALL METHOD go_grid->refresh_table_display.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form PREPARE_CATALOG
*&---------------------------------------------------------------------*
FORM prepare_catalog.

  DATA: lt_fieldcat_slis TYPE slis_t_fieldcat_alv.

  REFRESH: gt_fieldcat.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZABS_STR_WEIGHBRIDGE'
    CHANGING
      ct_fieldcat            = lt_fieldcat_slis
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc EQ 0.
    CALL FUNCTION 'LVC_TRANSFER_FROM_SLIS'
      EXPORTING
        it_fieldcat_alv = lt_fieldcat_slis
      IMPORTING
        et_fieldcat_lvc = gt_fieldcat[]
      TABLES
        it_data         = gt_weighbridge[]
      EXCEPTIONS
        it_data_missing = 1
        OTHERS          = 2.

    LOOP AT gt_fieldcat ASSIGNING FIELD-SYMBOL(<lwa_fcat>).
      CASE <lwa_fcat>-fieldname.
        WHEN 'DUMMY1'
          OR 'ZZMOBILE'
          OR 'UPDKZ'.
          <lwa_fcat>-no_out = abap_true.
      ENDCASE.
    ENDLOOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form PREPARE_LAYOUT
*&---------------------------------------------------------------------*
FORM prepare_layout.

  gs_layout-zebra = abap_true.
  gs_layout-smalltitle = abap_true.
  gs_layout-cwidth_opt = abap_true.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SELECTION_VALIDATIONS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM selection_validations .

  DATA: lt_domain_values TYPE TABLE OF dd07v,
        lt_lista         TYPE vrm_values,
        lv_dropdown_id   TYPE vrm_id,
        lv_domain        TYPE domname VALUE 'ZABS_DOM_TIPO_VIAGEM'.

  IF p_tipo IS NOT INITIAL.
    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = lv_domain
        text           = abap_true
        langu          = sy-langu
      TABLES
        dd07v_tab      = lt_domain_values
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.

    READ TABLE lt_domain_values INTO DATA(ls_domain_value)
      WITH KEY domvalue_l = p_tipo.
    IF sy-subrc NE 0.
*-- Tipo de Viagem &1 inválido!
      MESSAGE i245(zfmfp) WITH p_tipo.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

ENDFORM.
