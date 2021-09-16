*&---------------------------------------------------------------------*
*& Include          LZFG_ACMF0F
*&---------------------------------------------------------------------*
FORM fcode_excludes_prepare  CHANGING lt_fcode_excludes TYPE ui_functions.

  CASE sy-dynnr.

    WHEN c_screen-main_screen.

      IF gs_variables-overview_mode EQ c_mode_display
      OR NOT gs_acdoc_infocus IS INITIAL.
        APPEND c_fcode-create TO lt_fcode_excludes.
      ENDIF.

      IF gs_acdoc_infocus IS INITIAL.
        APPEND c_fcode-back              TO lt_fcode_excludes.
        APPEND c_fcode-cancel            TO lt_fcode_excludes.
        APPEND c_fcode-save              TO lt_fcode_excludes.
        APPEND c_fcode-copy              TO lt_fcode_excludes.
        APPEND c_fcode-delete            TO lt_fcode_excludes.
        APPEND c_fcode-change_docs       TO lt_fcode_excludes.
        APPEND c_fcode-order_confirm     TO lt_fcode_excludes.
        APPEND c_fcode-order_distributed TO lt_fcode_excludes.
        APPEND c_fcode-order_closed      TO lt_fcode_excludes.
        APPEND c_fcode-order_reversed    TO lt_fcode_excludes.
      ENDIF.

      IF gs_variables-document_mode EQ c_mode_display.
        APPEND c_fcode-save              TO lt_fcode_excludes.
        APPEND c_fcode-delete            TO lt_fcode_excludes.
        APPEND c_fcode-copy              TO lt_fcode_excludes.
        APPEND c_fcode-order_confirm     TO lt_fcode_excludes.
        APPEND c_fcode-order_distributed TO lt_fcode_excludes.
        APPEND c_fcode-order_closed      TO lt_fcode_excludes.
        APPEND c_fcode-order_reversed    TO lt_fcode_excludes.
      ENDIF.

      IF gs_variables-document_mode EQ c_mode_create.
        APPEND c_fcode-delete            TO lt_fcode_excludes.
        APPEND c_fcode-copy              TO lt_fcode_excludes.
        APPEND c_fcode-order_confirm     TO lt_fcode_excludes.
        APPEND c_fcode-order_distributed TO lt_fcode_excludes.
        APPEND c_fcode-order_closed      TO lt_fcode_excludes.
        APPEND c_fcode-order_reversed    TO lt_fcode_excludes.
      ENDIF.

      IF gs_variables-document_mode EQ c_mode_change.
        CASE gs_acdoc_infocus-x-achdr-status.
          WHEN c_process_status-ctd.
            APPEND c_fcode-order_distributed TO lt_fcode_excludes.
            APPEND c_fcode-order_closed      TO lt_fcode_excludes.
            APPEND c_fcode-order_reversed    TO lt_fcode_excludes.

            READ TABLE gs_acdoc_infocus-x-acitm
                                            WITH KEY status = c_process_status-ctd
                                            TRANSPORTING NO FIELDS.
            IF sy-subrc NE 0.
*              APPEND c_fcode-delete TO lt_fcode_excludes.
              APPEND c_fcode-order_confirm     TO lt_fcode_excludes.
            ENDIF.

          WHEN c_process_status-cnf.
            APPEND c_fcode-order_closed      TO lt_fcode_excludes.
            APPEND c_fcode-order_reversed    TO lt_fcode_excludes.
            APPEND c_fcode-delete            TO lt_fcode_excludes.
            READ TABLE gs_acdoc_infocus-x-acitm
                                             WITH KEY status = c_process_status-ctd
                                             TRANSPORTING NO FIELDS.
            IF sy-subrc NE 0.
              APPEND c_fcode-order_confirm     TO lt_fcode_excludes.
            ENDIF.


          WHEN c_process_status-dis.
            APPEND c_fcode-order_confirm     TO lt_fcode_excludes.
            APPEND c_fcode-order_distributed TO lt_fcode_excludes.
*            READ TABLE gs_acdoc_infocus-x-acitm
*                                           WITH KEY status = c_process_status-cnf
*                                           TRANSPORTING NO FIELDS.
*            IF sy-subrc EQ 0.
            APPEND c_fcode-delete            TO lt_fcode_excludes.
*            ENDIF.
*            READ TABLE gs_acdoc_infocus-x-acitm
*                                            WITH KEY status = c_process_status-ctd
*                                            TRANSPORTING NO FIELDS.
*            IF sy-subrc EQ 0.
*              APPEND c_fcode-delete            TO lt_fcode_excludes.
*            ENDIF.

          WHEN  c_process_status-cls.
            APPEND c_fcode-order_confirm     TO lt_fcode_excludes.
            APPEND c_fcode-order_distributed TO lt_fcode_excludes.
            APPEND c_fcode-order_closed      TO lt_fcode_excludes.
            APPEND c_fcode-order_reversed    TO lt_fcode_excludes.
            APPEND c_fcode-delete            TO lt_fcode_excludes.

        ENDCASE.
        APPEND c_fcode-copy   TO lt_fcode_excludes.


*        APPEND c_fcode-delete TO lt_fcode_excludes.

      ENDIF.

      IF gs_variables-document_mode EQ c_mode_create.
        APPEND c_fcode-delete          TO lt_fcode_excludes.
        APPEND c_fcode-copy            TO lt_fcode_excludes.
        APPEND c_fcode-where_used_list TO lt_fcode_excludes.
      ENDIF.

***External Call
      IF gs_variables-display_only EQ c_true.
        APPEND c_fcode-display_change_toggle TO lt_fcode_excludes.
      ENDIF.
** 60E - User Functions
***User Functions
      APPEND c_fcode-user_function1  TO lt_fcode_excludes.
      APPEND c_fcode-user_function2  TO lt_fcode_excludes.
      APPEND c_fcode-user_function3  TO lt_fcode_excludes.
      APPEND c_fcode-user_function4  TO lt_fcode_excludes.
      APPEND c_fcode-user_function5  TO lt_fcode_excludes.
      APPEND c_fcode-where_used_list TO lt_fcode_excludes.
  ENDCASE.
ENDFORM.                    " FCODE_EXCLUDES_PREPARE
*&---------------------------------------------------------------------*
*&      Form  FCODE_PROCESSING
*&---------------------------------------------------------------------*
FORM fcode_processing .
  DATA: lv_routine(30) TYPE c VALUE 'FCODE_'.

  IF ok_code IS NOT INITIAL.

    fcode = ok_code.
    CLEAR ok_code.

    IF fcode(2) EQ 'T\'.
      PERFORM fcode_processing_tabstrip.
    ELSE.
      CONCATENATE lv_routine fcode INTO lv_routine.
      PERFORM (lv_routine) IN PROGRAM (c_program)
                           IF FOUND.
    ENDIF.

  ENDIF.

ENDFORM.                    " FCODE_PROCESSING
*&---------------------------------------------------------------------*
*&      Form  FCODE_PROCESSING_TABSTRIP
*&---------------------------------------------------------------------*
FORM fcode_processing_tabstrip .

  DATA: ls_tabstrip    TYPE /agri/s_gtabstrip,
        lv_perform(30) VALUE 'FCODE_'.

****Check Rules
  READ TABLE gt_tabstrip_fcodes INTO ls_tabstrip
                      WITH KEY ts_fcode = fcode.
  CASE ls_tabstrip-local_fcode.
    WHEN c_fcode-tab_texts.
****Release 60E-SO
      CALL FUNCTION '/AGRI/G_WORD_PROCESSING_INIT'.
****
      CONCATENATE lv_perform ls_tabstrip-local_fcode INTO lv_perform.
      PERFORM (lv_perform) IN PROGRAM /agri/saplfmacm IF FOUND.

    WHEN OTHERS.
  ENDCASE.

  ts_items-activetab = fcode.
ENDFORM.                    " FCODE_PROCESSING_TABSTRIP
*&---------------------------------------------------------------------*
*&      Form  fcode_desc
*&---------------------------------------------------------------------*
FORM fcode_desc.
  CALL SCREEN c_screen-multi_lang_desc
                           STARTING AT 30 2 ENDING AT 85 15.
ENDFORM.                    "fcode_desc
*&---------------------------------------------------------------------*
*&      Form  fcode_srch
*&---------------------------------------------------------------------*
FORM fcode_srch.

  DATA: lt_search_hdr_more LIKE gt_search_header,
        lv_continue        TYPE c VALUE c_true.

  PERFORM document_infocus_clear CHANGING lv_continue.
  CHECK lv_continue EQ c_true.

  PERFORM worklist_search CHANGING lt_search_hdr_more.

ENDFORM.                    "FCODE_SRCH
*&---------------------------------------------------------------------*
*&      Form  FIELD_CATALOG_PREPARE
*&---------------------------------------------------------------------*
FORM field_catalog_prepare  USING lv_structure_name.

  DATA: lv_pos    TYPE sy-tabix,
        lv_tabix  TYPE sy-tabix,
        lwa_fcat  LIKE LINE OF gt_fcat,
        lv_colpos TYPE lvc_colpos.

  FIELD-SYMBOLS : <lwa_fcat> LIKE LINE OF gt_fcat.

  IF gt_fcat IS NOT INITIAL.
    REFRESH gt_fcat.
  ENDIF.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = lv_structure_name
    CHANGING
      ct_fieldcat            = gt_fcat
    EXCEPTIONS ##FM_SUBRC_OK
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  CASE lv_structure_name.
****WorkList Header
    WHEN c_structure_name-worklist_header.
      READ TABLE gt_fcat ASSIGNING <lwa_fcat> WITH KEY fieldname = 'ACCOM'.
      IF <lwa_fcat> IS ASSIGNED.
        <lwa_fcat>-hotspot = c_true.
      ENDIF.
    WHEN c_structure_name-ac_items.
      LOOP AT gt_fcat ASSIGNING <lwa_fcat>.
*        CLEAR <lwa_fcat>-col_pos.
        CASE <lwa_fcat>-fieldname.
          WHEN 'ACCOM' OR 'UPDKZ' OR 'POSNR' OR 'TPLNR'.
            <lwa_fcat>-tech = c_true.
          WHEN 'IDRESOURCE' OR 'EQUNR' OR 'IDACTVL' OR 'IDACTVE' OR 'AUFNR' OR 'ZZACTRN' OR 'ZZTPLNR'.
            <lwa_fcat>-f4availabl = c_true.
            <lwa_fcat>-edit = c_true.
*          WHEN 'TPLNR'  OR 'TMATNR' OR 'STRTDAT' OR 'STRTTIM' OR "#EC WHEN_DOUBLE_OK
          WHEN 'STRTDAT' OR 'STRTTIM' OR            "#EC WHEN_DOUBLE_OK
               'FINDAT' OR 'FINTIM' OR 'MENGE' OR 'AUFNR' OR 'ZZACTRN' OR
               'ZZLMNGA' OR 'ZZMEINH'.
            <lwa_fcat>-edit = c_true.
*            <lwa_fcat>-f4availabl = 'X'.
*            <lwa_fcat>-ref_table = 'GT_DETAILS_FCAT'.
*            <lwa_fcat>-ref_field = 'TPLNR_FL'.

          WHEN 'RUECK' OR 'EBELN' OR 'MBLNR'.
            <lwa_fcat>-hotspot = c_true.
          WHEN 'STATUS'.
            <lwa_fcat>-ref_field = space.
            <lwa_fcat>-ref_table = space.


          WHEN OTHERS.

        ENDCASE.
      ENDLOOP.
    WHEN c_structure_name-details.
      IF gs_variables-display_details IS NOT INITIAL.
        LOOP AT gt_fcat ASSIGNING <lwa_fcat>.
          CASE <lwa_fcat>-fieldname.
            WHEN 'ERNAM' OR 'ERDAT' OR 'ERZET' OR
                 'AENAM' OR 'AEDAT' OR 'AEZET' OR
                 'UPDKZ' OR 'GWEMG' OR 'GMEIN' OR
                 'CMNUM'  OR "'MBLNR_GI' OR 'MJAHR_GI'
                 'ROWCOLOR' OR 'STYLES' OR 'BWART_GI' OR
*              added on 25.09.19
              'OCNUM' OR  'MBLNR' OR 'BWART' OR 'MJAHR' OR
               'BUDAT_MKPF' OR 'CGLST'.
              <lwa_fcat>-tech =  c_true.
*            WHEN 'CGLST'.
*              <lwa_fcat>-drdn_alias = c_true.
*              <lwa_fcat>-convexit   = c_grid_dropdown-convexit1.
*              <lwa_fcat>-drdn_hndl  = '1'.
*              <lwa_fcat>-outputlen  = 15.
            WHEN OTHERS.
          ENDCASE.
        ENDLOOP.
        READ TABLE gt_fcat INTO lwa_fcat WITH KEY fieldname = 'MATNR'.
        IF sy-subrc EQ 0.
          READ TABLE gt_fcat ASSIGNING <lwa_fcat> WITH KEY fieldname = 'MAKTX'.
          IF sy-subrc EQ 0.
            <lwa_fcat>-col_pos = lwa_fcat-col_pos.
          ENDIF.
        ENDIF.
      ELSE.
        LOOP AT gt_fcat ASSIGNING <lwa_fcat>.
          CASE <lwa_fcat>-fieldname.
            WHEN 'ERNAM'     OR 'ERZET'     OR 'AENAM'     OR
                 'AEDAT'     OR 'AEZET'     OR 'UPDKZ'     OR
                 'GWEMG'     OR 'GMEIN'     OR "'MATNR'     OR
                 'MAKTX'     OR 'VORNR'     OR 'RUECK'     OR
                 'RMZHL'     OR 'LTXA1'     OR 'CMNUM'     OR
                 'LGORT'     OR 'MBLNR_GI'  OR 'BWART_GI'  OR
                 'MJAHR_GI'  OR 'ERDAT'     OR "'AUFNR'     OR
                 'CHARG'     OR 'ROWCOLOR'  OR 'STYLES'    OR
                 'WONUM'     OR 'OCNUM_REF' OR 'CHARG_REF' OR
*              added on 25.09.19
                 'CONTR_REF' OR 'OCNUM' OR  'MBLNR' OR 'BWART' OR 'MJAHR' OR
               'BUDAT_MKPF' OR 'CGLST'.
              <lwa_fcat>-tech =  c_true.
            WHEN OTHERS.
          ENDCASE.
        ENDLOOP.
        IF gs_tfmactyp-acapp EQ c_accom_appli-prnum.
          CLEAR lwa_fcat.
          lwa_fcat-fieldname  = 'PRNUM'.
          lwa_fcat-rollname   = '/AGRI/FMPRNUM'.
          lwa_fcat-domname    = '/AGRI/FMPRNUM'.
          lwa_fcat-scrtext_l  = TEXT-050."'Ticket'.
          lwa_fcat-colddictxt = 'L'.
          lwa_fcat-col_pos    = '1'.
          lwa_fcat-tabname    = '1'.
          lwa_fcat-no_zero      = c_true.
          INSERT lwa_fcat INTO gt_fcat INDEX 1.
          LOOP AT gt_fcat ASSIGNING <lwa_fcat> FROM 2.
            <lwa_fcat>-col_pos = <lwa_fcat>-col_pos + 1.
          ENDLOOP.
        ENDIF.
        CLEAR lwa_fcat.
        lwa_fcat-fieldname  = 'TPLNR_FL'.
        lwa_fcat-datatype   = 'CHAR'.
        lwa_fcat-inttype    = 'C'.
        lwa_fcat-intlen     = '30'.
        lwa_fcat-scrtext_l  = TEXT-055."'Terrain'.
        lwa_fcat-colddictxt = 'L'.
        lwa_fcat-col_pos    = '4'.
        lwa_fcat-tabname    = '1'.
        INSERT lwa_fcat INTO gt_fcat INDEX 4.
        LOOP AT gt_fcat ASSIGNING <lwa_fcat> FROM 5.
          <lwa_fcat>-col_pos = <lwa_fcat>-col_pos + 1.
        ENDLOOP.
        READ TABLE gt_fcat ASSIGNING <lwa_fcat> WITH KEY fieldname = 'MBLNR_GI'.
        IF sy-subrc EQ 0.
          <lwa_fcat>-scrtext_s =
          <lwa_fcat>-scrtext_m =
          <lwa_fcat>-scrtext_l = TEXT-056."'Goods Issue'.
          <lwa_fcat>-colddictxt = 'L'.
        ENDIF.
        READ TABLE gt_fcat ASSIGNING <lwa_fcat> WITH KEY fieldname = 'MBLNR'.
        IF sy-subrc EQ 0.
          <lwa_fcat>-scrtext_s =
          <lwa_fcat>-scrtext_m =
          <lwa_fcat>-scrtext_l = TEXT-057."'Goods Receipt'.
          <lwa_fcat>-colddictxt = 'L'.
        ENDIF.
        READ TABLE gt_fcat INTO lwa_fcat WITH KEY fieldname = 'MJAHR_GI'.
        IF sy-subrc EQ 0.
          READ TABLE gt_fcat ASSIGNING <lwa_fcat> WITH KEY fieldname = 'BUDAT_MKPF'.
          IF sy-subrc EQ 0.
            <lwa_fcat>-col_pos = lwa_fcat-col_pos + 1.
          ENDIF.
        ENDIF.
      ENDIF.
    WHEN c_structure_name-ac_multi_lang_desc.
      LOOP AT gt_fcat ASSIGNING <lwa_fcat>.
        CASE <lwa_fcat>-fieldname.
          WHEN 'ACCOM' OR 'UPDKZ'.
            <lwa_fcat>-no_out = c_true.
          WHEN 'SPRAS'.
            <lwa_fcat>-outputlen = 8.
            <lwa_fcat>-edit = c_true.
          WHEN 'DESCR'.
            <lwa_fcat>-edit = c_true.
        ENDCASE.
      ENDLOOP.
    WHEN OTHERS.
      LOOP AT gt_fcat ASSIGNING <lwa_fcat>.
        CASE <lwa_fcat>-fieldname.
          WHEN 'FIELDNAME' OR 'UPDKZ'.
            <lwa_fcat>-no_out = c_true.
          WHEN 'FIELDVAL'.
            <lwa_fcat>-edit = c_true.
            <lwa_fcat>-f4availabl = c_true.
            <lwa_fcat>-outputlen = 10.
            <lwa_fcat>-lowercase = c_true.
          WHEN 'FIELDDSCR'.
            <lwa_fcat>-outputlen = 40.
          WHEN 'FIELDTXT'.
            <lwa_fcat>-outputlen = 15.
          WHEN 'ACTYP' OR 'VTEXT' OR 'NAME1' OR
               'BUTXT' OR 'ACCOM' OR 'ROWCOLOR' OR
               'FIELDTYP' OR 'OUTPUTLEN' OR 'CONVEXIT' OR
               'TABNAME'.
            <lwa_fcat>-no_out = <lwa_fcat>-tech = c_true.
        ENDCASE.
      ENDLOOP.
  ENDCASE.

ENDFORM.                    " FIELD_CATALOG_PREPARE
*&---------------------------------------------------------------------*
*&      Form  FCODE_/AGRI/MIN
*&---------------------------------------------------------------------*
FORM fcode_/agri/min .
  DATA: lv_extension TYPE i VALUE 300.
  CHECK gs_variables-worklist_is_visible EQ c_true.
  CALL METHOD ref_worklist_container->set_extension
    EXPORTING
      extension  = lv_extension
    EXCEPTIONS
      cntl_error = 1
      OTHERS     = 2.
ENDFORM.                    " FCODE_/AGRI/MIN
*&---------------------------------------------------------------------*
*&      Form  fcode_crea
*&---------------------------------------------------------------------*
FORM fcode_crea.
  PERFORM accom_create.
ENDFORM.                    "fcode_crea
*&---------------------------------------------------------------------*
*&      Form  fcode_cont
*&---------------------------------------------------------------------*
FORM fcode_cont.

  DATA: lv_irtyp TYPE /agri/fmirtyp.
  FIELD-SYMBOLS: <lwa_acdesc>   TYPE /agri/s_fmachdrt,
                 <lwa_fmachdrt> TYPE /agri/s_fmachdrt.

  IF sy-dynnr = c_screen-create_accom.

    LOOP AT gs_acdoc_infocus-x-acdes ASSIGNING <lwa_fmachdrt>.
      CHECK <lwa_fmachdrt>-descr IS INITIAL.
      READ TABLE gs_acdoc_infocus-y-acdes ASSIGNING <lwa_acdesc>
                                 WITH KEY spras = <lwa_fmachdrt>-spras
                                          accom = gs_acdoc_infocus-accom.
      IF sy-subrc EQ 0.
        <lwa_acdesc>-updkz = c_updkz_delete.
      ENDIF.
      DELETE gs_acdoc_infocus-x-acdes WHERE spras EQ <lwa_fmachdrt>-spras.
      gs_variables-refresh_grid_desc = c_true.
    ENDLOOP.

  ENDIF.
  SET SCREEN 0.
  LEAVE SCREEN.

ENDFORM.                    "fcode_cont
*&---------------------------------------------------------------------*
*&      Form  fcode_cret
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_cret.

  DATA: lv_irtyp TYPE /agri/fmirtyp.
  FIELD-SYMBOLS: <lwa_acdesc>   TYPE /agri/s_fmachdrt,
                 <lwa_fmachdrt> TYPE /agri/s_fmachdrt.

  PERFORM header_data_update_1.

*  IF sy-dynnr = c_screen-create.
  IF gs_acdoc_infocus-x-achdr IS NOT INITIAL.

    LOOP AT gs_acdoc_infocus-x-acdes ASSIGNING <lwa_fmachdrt>.
      CHECK <lwa_fmachdrt>-descr IS INITIAL.
      READ TABLE gs_acdoc_infocus-y-acdes ASSIGNING <lwa_acdesc>
                                 WITH KEY spras = <lwa_fmachdrt>-spras
                                          accom = gs_acdoc_infocus-accom.
      IF sy-subrc EQ 0.
        <lwa_acdesc>-updkz = c_updkz_delete.
      ENDIF.
      DELETE gs_acdoc_infocus-x-acdes WHERE spras EQ <lwa_fmachdrt>-spras.
      gs_variables-refresh_grid_desc = c_true.
    ENDLOOP.

  ENDIF.
*  SET SCREEN 0.
*  LEAVE SCREEN.

ENDFORM.                    "fcode_cret
*&---------------------------------------------------------------------*
*&      Form  fcode_ccan
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_ccan.
  CLEAR: gs_acdoc_infocus, /agri/s_fmachdr.
  PERFORM document_data_initialize USING c_true.
*  SET SCREEN 0.
*  LEAVE SCREEN.
ENDFORM.                    "fcode_ccan
*&---------------------------------------------------------------------*
*&      Form  fcode_/agri/dhn
*&---------------------------------------------------------------------*
FORM fcode_/agri/dhn .

  IF gs_variables-worklist_is_visible EQ c_true.
    CLEAR gs_variables-worklist_is_visible.
  ELSE.
    gs_variables-worklist_is_visible = c_true.
  ENDIF.

  CALL METHOD ref_worklist_container->set_visible
    EXPORTING
      visible           = gs_variables-worklist_is_visible
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
  ENDIF.

ENDFORM.                    "fcode_/agri/dhn
*&---------------------------------------------------------------------*
*&      Form  fcode_/agri/max
*&---------------------------------------------------------------------*
FORM fcode_/agri/max .
****Maximize Worklist(Full Screen)
  DATA: lv_extension TYPE i VALUE 2000.

  CHECK gs_variables-worklist_is_visible EQ c_true.

  CALL METHOD ref_worklist_container->set_extension
    EXPORTING
      extension  = lv_extension
    EXCEPTIONS
      cntl_error = 1
      OTHERS     = 2.
ENDFORM.                    "fcode_max
*&---------------------------------------------------------------------*
*&      Form  fcode_back
*&---------------------------------------------------------------------*
FORM fcode_back.

  DATA: lv_continue.

  PERFORM document_infocus_clear  CHANGING lv_continue.

ENDFORM.                    "fcode_back
*&---------------------------------------------------------------------*
*&      Form  fcode_cdoc
*&---------------------------------------------------------------------*
FORM fcode_cdoc.

  DATA: lt_objid  TYPE /agri/t_gcdobjid,
        lwa_objid TYPE /agri/s_gcdobjid.

  CHECK: NOT gs_acdoc_infocus-accom IS INITIAL.

  lwa_objid-objid  = gs_acdoc_infocus-accom.
  APPEND lwa_objid TO lt_objid.
  CALL FUNCTION '/AGRI/GCD_DISPLAY'
    EXPORTING
      i_object           = c_object-change_documents
      i_html_view        = c_true
      t_objid            = lt_objid
    EXCEPTIONS ##FM_SUBRC_OK
      no_documents_found = 1
      no_object_selected = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
  ENDIF.

ENDFORM.                    "fcode_cdoc
*&---------------------------------------------------------------------*
*&      Form  fcode_cmph
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_cmph.
  CLEAR gs_variables-header_display.
ENDFORM.                    "fcode_cmph
*&---------------------------------------------------------------------*
*&      Form  fcode_dich
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_dich.

  DATA: lv_answer,
        lv_subrc  TYPE sy-subrc,
        lv_dummy.

  IF gs_variables-overview_mode EQ c_mode_change.

    PERFORM changes_confirm CHANGING lv_answer.
    IF lv_answer NE 'A'.

      gs_variables-overview_mode = c_mode_display.

      IF gs_variables-document_mode NE c_mode_display.
        PERFORM document_infocus_unlock USING gs_acdoc_infocus-accom.
        PERFORM document_infocus_set USING gs_acdoc_infocus-accom
                                  CHANGING lv_subrc.

        gs_variables-document_mode = c_mode_display.
        gs_variables-overview_mode = c_mode_display.
      ENDIF.

    ELSE.

      CLEAR: lv_answer, ok_code.
      PERFORM document_infocus_save USING c_true.
      PERFORM document_infocus_unlock USING gs_acdoc_infocus-accom.

      gs_variables-overview_mode = c_mode_display.
      gs_variables-document_mode = c_mode_display.

    ENDIF.

  ELSE.

    gs_variables-overview_mode = c_mode_change.
    PERFORM document_infocus_set USING    gs_acdoc_infocus-accom
                                 CHANGING lv_subrc.

  ENDIF.

ENDFORM.                    "fcode_dich
*&---------------------------------------------------------------------*
*&      Form  fcode_exph
*&---------------------------------------------------------------------*
FORM fcode_exph.
  gs_variables-header_display = c_true.
ENDFORM.                    "fcode_exph
*&---------------------------------------------------------------------*
*&      Form  fcode_dins
*&---------------------------------------------------------------------*
FORM fcode_dins.

  APPEND INITIAL LINE TO gt_ac_desc_layout.
  CALL METHOD ref_multi_lang_desc_grid->refresh_table_display
    EXCEPTIONS
      finished = 1
      OTHERS   = 2.

  CLEAR gs_variables-refresh_grid_desc.

ENDFORM.                    "fcode_dins
*&---------------------------------------------------------------------*
*&      Form  iins
*&---------------------------------------------------------------------*
FORM fcode_iins.
  DATA: lwa_items_layout TYPE /agri/s_fmacitm_layout,
        lt_items_temp    TYPE /agri/t_fmacitm_layout,
        lwa_items_temp   TYPE /agri/s_fmacitm_layout,
        lv_minutes       TYPE i,
        lv_posnr         TYPE i,
        lwa_style        TYPE lvc_s_styl,
        lt_fcat          TYPE lvc_t_fcat,
        lwa_fcat         TYPE lvc_s_fcat,
        lwa_details      TYPE ty_details.

  FIELD-SYMBOLS: <lwa_acitm>      TYPE /agri/s_fmacitm,
                 <lwa_items_temp> TYPE /agri/s_fmacitm_layout.

****22/09/2016
  IF gt_items_layout IS NOT INITIAL.

    lt_items_temp[] = gt_items_layout[].

    SORT lt_items_temp BY posnr DESCENDING.
    READ TABLE lt_items_temp ASSIGNING <lwa_items_temp>
                                        INDEX 1.
    lv_posnr = <lwa_items_temp>-posnr.
    ADD 1 TO lv_posnr.
    SORT gs_acdoc_infocus-x-acitm BY posnr ASCENDING.
  ELSE.
    ADD 1 TO lv_posnr.
  ENDIF.

*  IF gs_acdoc_infocus-x-acitm IS NOT INITIAL.
*    SORT gs_acdoc_infocus-x-acitm BY posnr DESCENDING.
*    READ TABLE gs_acdoc_infocus-x-acitm ASSIGNING <lwa_acitm>
*                                        INDEX 1.
*    lv_posnr = <lwa_acitm>-posnr.
*    ADD 1 TO lv_posnr.
*    SORT gs_acdoc_infocus-x-acitm BY posnr ASCENDING.
*  ELSE.
*    ADD 1 TO lv_posnr.
*  ENDIF.
****

*  DESCRIBE TABLE gt_items_layout LINES lv_posnr.
*  ADD 1 TO lv_posnr.
  lwa_items_layout-accom    = /agri/s_fmachdr-accom.
*  lwa_items_layout-strtdat  = /agri/s_fmachdr-strtdat.
*  lwa_items_layout-strttim  = sy-uzeit.
*  lwa_items_layout-findat   = /agri/s_fmachdr-findat.
*  lwa_items_layout-fintim   = sy-uzeit.
  lwa_items_layout-posnr    = lv_posnr.
  lwa_items_layout-status   = c_process_status-ctd.
*--- Replaced Unreleased Interface
*  CALL FUNCTION 'DELTA_TIME_DAY_HOUR'
*    EXPORTING
*      t1      = lwa_items_layout-strttim
*      t2      = lwa_items_layout-fintim
*      d1      = lwa_items_layout-strtdat
*      d2      = lwa_items_layout-findat
*    IMPORTING
*      minutes = lv_minutes.
  CALL FUNCTION '/AGRI/G_DELTA_TIME_DAY_HOUR'
    EXPORTING
      i_t1      = lwa_items_layout-strttim
      i_t2      = lwa_items_layout-fintim
      i_d1      = lwa_items_layout-strtdat
      i_d2      = lwa_items_layout-findat
    IMPORTING
      e_minutes = lv_minutes.
*---
  lwa_items_layout-duran = lv_minutes / 60.
  lwa_items_layout-meins = 'STD'.

  CALL METHOD ref_grid_items->get_frontend_fieldcatalog
    IMPORTING
      et_fieldcatalog = lt_fcat.
  LOOP AT lt_fcat INTO lwa_fcat WHERE fieldname = 'IDACTVL' OR fieldname = 'IDACTVE'.
    lwa_style-fieldname = lwa_fcat-fieldname.
    lwa_style-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT lwa_style INTO TABLE lwa_items_layout-styles.
  ENDLOOP.

  READ TABLE gt_details INTO lwa_details INDEX 1.
  IF sy-subrc EQ 0.
    lwa_items_layout-tmatnr = lwa_details-matnr.
  ENDIF.

  APPEND lwa_items_layout TO gt_items_layout.
  CALL METHOD ref_grid_items->refresh_table_display
    EXCEPTIONS
      finished = 1
      OTHERS   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
*  SORT gt_items_layout by posnr.
  CLEAR gs_variables-refresh_items_grid.
ENDFORM.                    "iins
*&---------------------------------------------------------------------*
*&      Form  idel
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_item_delete CHANGING lv_var.
  DATA: lt_rows          TYPE lvc_t_row,
        lwa_row          TYPE lvc_s_row,
        lv_answer        TYPE c,
        lwa_flo_layout   LIKE LINE OF gt_items_layout,
        lwa_items_layout TYPE /agri/s_fmacitm_layout.

  FIELD-SYMBOLS: <lwa_items_modi> TYPE lvc_s_modi,
                 <lwa_item>       TYPE /agri/s_fmacitm.

  CALL METHOD ref_grid_items->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.

  CHECK lt_rows IS NOT INITIAL.

  popup_to_confirm TEXT-007 TEXT-048 c_true lv_answer.

  IF lv_answer EQ '1'  .

    gs_variables-refresh_items_grid = c_true.
    SORT lt_rows BY index DESCENDING.

    LOOP AT lt_rows INTO lwa_row.

      LOOP AT gt_items_mod_rows ASSIGNING <lwa_items_modi> WHERE row_id GE lwa_row-index.
        IF <lwa_items_modi>-row_id NE lwa_row-index.
          <lwa_items_modi>-row_id = <lwa_items_modi>-row_id - 1.
        ELSE.
          DELETE gt_items_mod_rows WHERE row_id EQ <lwa_items_modi>-row_id.
        ENDIF.
      ENDLOOP.

      READ TABLE gt_items_layout INTO lwa_items_layout INDEX lwa_row-index.
      IF lwa_items_layout-status NE c_process_status-ctd.
        "Crear Mensaje
        MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                                NUMBER '027'
                                INTO sy-msgli.
        message_simple space.
        CONTINUE.

      ENDIF.

      READ TABLE gs_acdoc_infocus-x-acitm ASSIGNING <lwa_item> WITH KEY posnr = lwa_items_layout-posnr.

      IF sy-subrc EQ 0.

        DELETE gs_acdoc_infocus-x-acitm WHERE posnr = lwa_items_layout-posnr.

        READ TABLE gs_acdoc_infocus-y-acitm ASSIGNING <lwa_item> WITH KEY posnr = lwa_items_layout-posnr.

        IF sy-subrc EQ 0.
          <lwa_item>-updkz = c_updkz_delete.
        ENDIF.

        DELETE gt_items_layout INDEX lwa_row-index.
        UNASSIGN: <lwa_item>.


      ENDIF.

    ENDLOOP.
  ELSE.
    lv_var = lv_answer.
  ENDIF.

ENDFORM.                    "idel
*&---------------------------------------------------------------------*
*&      Form  fcode_save
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_save.
  PERFORM document_infocus_save USING c_true.
ENDFORM.                    "fcode_save
*&---------------------------------------------------------------------*
*&      Form  fcode_wlhc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_wlhc.
****Hotspot click - Worklist
  DATA: lv_answer,
        lv_subrc         TYPE sy-subrc,
        lv_dummy,
        lwa_selected_doc LIKE LINE OF gt_selected_docs.

  READ TABLE gt_selected_docs INTO lwa_selected_doc INDEX 1.
  CHECK sy-subrc EQ 0.

****Confirm Changes made to the document
  PERFORM changes_confirm CHANGING lv_answer.

  IF lv_answer EQ 'A'.

    IF ok_code EQ c_fcode-save.
****Save - Yes
      CLEAR ok_code.
****Save the infocus document
      PERFORM document_infocus_save USING space.

    ELSE.
****Save - Cancel
****Changes were not saved
      MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-success NUMBER '016'
      INTO sy-msgli.
      message_simple space.
      PERFORM messages_display USING gs_variables-initiator.
      EXIT.
    ENDIF.
  ENDIF.

****Dequeue the document infocus
  PERFORM document_infocus_unlock USING gs_acdoc_infocus-accom.

  PERFORM document_infocus_set USING    lwa_selected_doc-accom
                               CHANGING lv_subrc.

ENDFORM.                    "fcode_wlhc
*&---------------------------------------------------------------------*
*&      Form  F4_IDRESOURCE_DISPLAY
*&---------------------------------------------------------------------*
FORM f4_resource_display  USING lv_idresource
                          CHANGING
                            VALUE(lt_returntab)
                            lv_row_no
                            lv_rstype
                            lv_fieldname.

  DATA: lwa_resource     TYPE /agri/s_fmac_src_res,
        lt_resource      TYPE /agri/t_fmac_src_res,
        lt_return        TYPE STANDARD TABLE OF ddshretval,
        lv_retfield      TYPE dfies-fieldname,
        lv_dynprofield   TYPE help_info-dynprofld,
        lt_arbpl         TYPE /agri/t_range_arbpl,
        lwa_items_layout TYPE /agri/s_fmacitm_layout,
        lwa_style        TYPE lvc_s_styl,
        lt_vgwts         TYPE /agri/t_vgwts,
        lwa_vgwts        TYPE /agri/s_vgwts,
        lt_parameters    TYPE /agri/t_parameters_tc21,
        lwa_parameters   TYPE /agri/s_parameters_tc21,
        lt_tc20          TYPE /agri/t_parameters_tc20,
        lwa_tc20         TYPE tc20,
        lwa_fmachdr      TYPE /agri/s_fmachdr,
        lt_fmacrsc       TYPE /agri/t_fmacrsc,
        lwa_fmacrsc      TYPE /agri/s_fmacrsc.

  READ TABLE gt_items_layout INTO lwa_items_layout INDEX lv_row_no.

****21/07/2016
  CHECK gs_variables-document_mode NE c_mode_display.
  CHECK lwa_items_layout-status EQ c_process_status-ctd.

  READ TABLE lwa_items_layout-styles INTO lwa_style WITH KEY
                                      fieldname = lv_fieldname.
  IF sy-subrc NE 0.
    CASE lv_fieldname.
      WHEN 'EQUNR'.
        READ TABLE lwa_items_layout-styles INTO lwa_style WITH KEY
                                     fieldname = 'IDRESOURCE'.
        IF lwa_style-style EQ cl_gui_alv_grid=>mc_style_enabled.
          EXIT.
        ENDIF.
      WHEN 'IDRESOURCE'.
        READ TABLE lwa_items_layout-styles INTO lwa_style WITH KEY
                                     fieldname = 'EQUNR'.
        IF lwa_style-style EQ cl_gui_alv_grid=>mc_style_enabled.
          EXIT.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
  ELSE.
    IF lwa_style-style NE cl_gui_alv_grid=>mc_style_enabled.
      EXIT.
    ENDIF.
  ENDIF.
*  CHECK sy-subrc NE 0. "AND lwa_style-style = cl_gui_alv_grid=>mc_style_enabled.
****
  lv_retfield = lv_dynprofield = lv_fieldname.
  IF lwa_items_layout-aufnr IS NOT INITIAL.
    PERFORM wonum_infocus_read USING /agri/s_fmachdr-wonum
                                     lwa_items_layout-aufnr
                               CHANGING lt_arbpl.
  ELSE.
*    MESSAGE ID 'ZFMACC' TYPE c_msg_type-info NUMBER '021'
*                                                INTO sy-msgli.
*    message_simple space.
    EXIT.
  ENDIF.
  IF lt_arbpl[] IS INITIAL.
*    MESSAGE ID 'ZFMACC' TYPE c_msg_type-info NUMBER '022'
*                                                INTO sy-msgli.
*    message_simple space.
    EXIT.
  ENDIF.


  SELECT idresource description arbpl intext lifnr
  INTO TABLE lt_resource
  FROM /agri/fmacres
  WHERE rstype = lv_rstype
    AND arbpl IN lt_arbpl.

*-- Filter the resources based on Turma
  IF lv_fieldname EQ 'IDRESOURCE' AND
     /agri/s_fmachdr-zzturma IS NOT INITIAL.
     REFRESH lt_resource.
     SELECT *
       FROM zfmfpgroupitm
       INTO TABLE @DATA(lt_turma)
      WHERE turma_id EQ @/agri/s_fmachdr-zzturma.
       IF sy-subrc EQ 0.
         SELECT idresource description arbpl intext lifnr
         INTO TABLE lt_resource
         FROM /agri/fmacres
          FOR ALL ENTRIES IN lt_turma
         WHERE idresource EQ lt_turma-idresource
           AND rstype EQ lv_rstype.
*           AND arbpl IN lt_arbpl.
       ENDIF.
  ENDIF.

****  13/09/2016
  SELECT arbpl vgwts
  FROM crhd
  INTO CORRESPONDING FIELDS OF TABLE lt_vgwts
  WHERE arbpl IN lt_arbpl
    AND werks = /agri/s_fmachdr-werks.

  DELETE ADJACENT DUPLICATES FROM lt_vgwts COMPARING ALL FIELDS. "#EC CI_SORTED
***Extended additional syntax check 1_2 ATC 1709 PQ
  IF lt_vgwts IS NOT INITIAL.
    SELECT vgwts par01 par02 par03 par04 par05 par06 "#EC CI_FAE_LINES_ENSURED
    INTO CORRESPONDING FIELDS OF TABLE lt_parameters
    FROM tc21
    FOR ALL ENTRIES IN lt_vgwts
    WHERE vgwts = lt_vgwts-vgwts.
  ENDIF.
***Extended additional syntax check 1_3 ATC 1709 PQ
  IF lt_parameters IS NOT INITIAL.
    SELECT parid                              "#EC CI_FAE_LINES_ENSURED
    INTO CORRESPONDING FIELDS OF TABLE lt_tc20
    FROM tc20
    FOR ALL ENTRIES IN lt_parameters
    WHERE parid = lt_parameters-par01
      OR  parid = lt_parameters-par02
      OR  parid = lt_parameters-par03
      OR  parid = lt_parameters-par04
      OR  parid = lt_parameters-par05
      OR  parid = lt_parameters-par06.
  ENDIF.
  READ TABLE gt_search_header INTO lwa_fmachdr WITH KEY accom = gs_acdoc_infocus-x-achdr-accom."lwa_item_layout-accom.
***Extended additional syntax check 1_3 ATC 1709 PQ
  IF lt_tc20 IS NOT INITIAL.

    SELECT actyp parid rstyp txtlg            "#EC CI_FAE_LINES_ENSURED
    FROM /agri/tfmacrsc
    INTO TABLE lt_fmacrsc
    FOR ALL ENTRIES IN lt_tc20
    WHERE parid = lt_tc20-parid
      AND actyp = lwa_fmachdr-actyp.

  ENDIF.
  CASE lv_fieldname.
    WHEN 'EQUNR'.
      DELETE lt_fmacrsc WHERE rstyp NE c_accom_id-equipment.
    WHEN 'IDRESOURCE'.
      DELETE lt_fmacrsc WHERE rstyp NE c_accom_id-employee.
  ENDCASE.

  CHECK lt_fmacrsc IS NOT INITIAL.
  LOOP AT lt_vgwts INTO lwa_vgwts.
    READ TABLE lt_parameters INTO lwa_parameters WITH KEY vgwts = lwa_vgwts-vgwts.
    IF sy-subrc EQ 0.
      LOOP AT lt_fmacrsc INTO lwa_fmacrsc WHERE parid = lwa_parameters-par01
                                                   OR parid = lwa_parameters-par02
                                                   OR parid = lwa_parameters-par03
                                                   OR parid = lwa_parameters-par04
                                                   OR parid = lwa_parameters-par05
                                                   OR parid = lwa_parameters-par06.
        CASE lwa_fmacrsc-rstyp.
          WHEN  c_accom_id-equipment.
            IF lv_fieldname NE 'EQUNR'.
              DELETE lt_resource WHERE arbpl = lwa_vgwts-arbpl.
            ENDIF.

          WHEN  c_accom_id-employee.
            IF lv_fieldname NE 'IDRESOURCE'.
              DELETE lt_resource WHERE arbpl = lwa_vgwts-arbpl.
            ENDIF.
          WHEN OTHERS.
        ENDCASE.

      ENDLOOP.

      IF sy-subrc NE 0.
        DELETE lt_resource WHERE arbpl = lwa_vgwts-arbpl.
      ENDIF.

    ENDIF.
  ENDLOOP.

  CHECK lt_resource IS NOT INITIAL.

****

  IF lwa_items_layout-arbpl IS NOT INITIAL
    AND lwa_items_layout-idresource IS NOT INITIAL
    AND lv_retfield NE 'IDRESOURCE'.
    DELETE lt_resource WHERE arbpl NE lwa_items_layout-arbpl.
  ELSEIF lwa_items_layout-arbpl IS NOT INITIAL
    AND  lwa_items_layout-equnr IS NOT INITIAL
     AND lv_retfield NE 'EQUNR'.
    DELETE lt_resource WHERE arbpl NE lwa_items_layout-arbpl.
  ENDIF.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'IDRESOURCE'
*     retfield        = lv_retfield
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'IDRESOURCE'
*     dynprofield     = lv_dynprofield
      stepl           = 1
      value           = 'X'
      value_org       = 'S'
    TABLES
      value_tab       = lt_resource
      return_tab      = lt_return
    EXCEPTIONS ##FM_SUBRC_OK
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
  ENDIF.
  lt_returntab = lt_return.

ENDFORM.                    " F4_IDRESOURCE_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  f4_activity_display
*&---------------------------------------------------------------------*
FORM f4_activity_display  USING     lv_activity
                            CHANGING
                            VALUE(lt_returntab)
                            lv_row_no
                            lv_rstype
                            lv_fieldname.

  DATA: lwa_activity    TYPE /agri/s_fmac_src_act,
        lt_activity     TYPE /agri/t_fmac_src_act,
        ls_items_layout TYPE /agri/s_fmacitm_layout,
        lt_return       TYPE TABLE OF ddshretval,
        lv_retfield     TYPE dfies-fieldname,
        lv_dynprofield  TYPE help_info-dynprofld,
        lv_aufpl        TYPE co_aufpl,
        lt_afvc         TYPE /agri/t_fmac_src_ord,
        ls_afvc         TYPE /agri/s_fmac_src_ord,
        lr_lstar        TYPE RANGE OF lstar,
        lwa_lstar       LIKE LINE OF lr_lstar,
        lv_crhd         TYPE crhd-vgwts,
        lt_vgwts        TYPE /agri/t_vgwts,
        ls_vgwts        TYPE /agri/s_vgwts,
        lwa_vgwts       TYPE /agri/s_vgwts,
        lt_parameters   TYPE /agri/t_parameters_tc21,
        ls_parameters   TYPE /agri/s_parameters_tc21,
        lwa_parameters  TYPE /agri/s_parameters_tc21,
        lt_tc20         TYPE /agri/t_parameters_tc20,
        lwa_tc20        TYPE tc20,
        lwa_fmachdr     TYPE /agri/s_fmachdr,
        lt_fmacrsc      TYPE /agri/t_fmacrsc,
        lwa_fmacrsc     TYPE /agri/s_fmacrsc.

  FIELD-SYMBOLS <lwa_afvc> TYPE /agri/s_fmac_src_ord.

  READ TABLE gt_items_layout INTO ls_items_layout INDEX lv_row_no.
  SELECT SINGLE aufpl
    FROM afko
    INTO lv_aufpl
    WHERE aufnr = ls_items_layout-aufnr.

  SELECT SINGLE vgwts
    FROM crhd
    INTO lv_crhd
    WHERE arbpl = ls_items_layout-arbpl.                "#EC CI_NOORDER

  SELECT SINGLE vgwts lar01 lar02 lar03 lar04 lar05 lar06
    INTO ls_afvc
    FROM afvc
    WHERE aufpl = lv_aufpl                              "#EC CI_NOORDER
    AND vgwts = lv_crhd.

****  14/09/2016
  SELECT SINGLE arbpl vgwts
  FROM crhd
  INTO ls_vgwts
  WHERE arbpl = ls_items_layout-arbpl.                  "#EC CI_NOORDER

  SELECT SINGLE vgwts par01 par02 par03 par04 par05 par06
  INTO ls_parameters
  FROM tc21
  WHERE vgwts = ls_vgwts-vgwts.

  SELECT parid
  INTO CORRESPONDING FIELDS OF TABLE lt_tc20
  FROM tc20
  WHERE parid = ls_parameters-par01
    OR  parid = ls_parameters-par02
    OR  parid = ls_parameters-par03
    OR  parid = ls_parameters-par04
    OR  parid = ls_parameters-par05
    OR  parid = ls_parameters-par06.

  READ TABLE gt_search_header INTO lwa_fmachdr WITH KEY accom = gs_acdoc_infocus-x-achdr-accom."lwa_item_layout-accom.
***Extended additional syntax check 1_3 ATC 1709 PQ
  IF lt_tc20 IS NOT INITIAL.
    IF lv_rstype EQ c_rstype-labor.
      SELECT actyp parid rstyp txtlg          "#EC CI_FAE_LINES_ENSURED
      FROM /agri/tfmacrsc
      INTO TABLE lt_fmacrsc
      FOR ALL ENTRIES IN lt_tc20
      WHERE parid = lt_tc20-parid
      AND rstyp = c_accom_id-employee
      AND actyp = lwa_fmachdr-actyp.

    ELSE.

      SELECT actyp parid rstyp txtlg          "#EC CI_FAE_LINES_ENSURED
      FROM /agri/tfmacrsc
      INTO TABLE lt_fmacrsc
      FOR ALL ENTRIES IN lt_tc20
      WHERE parid = lt_tc20-parid
      AND rstyp = c_accom_id-equipment
      AND actyp = lwa_fmachdr-actyp.

    ENDIF.
  ENDIF.

  IF ls_afvc-vgwts EQ ls_parameters-vgwts.

    LOOP AT lt_fmacrsc INTO lwa_fmacrsc.

      IF lwa_fmacrsc-parid = ls_parameters-par01
        AND ls_afvc-lar01 IS NOT INITIAL.
        lwa_lstar-sign = c_sign-include.
        lwa_lstar-option = c_operator_word-equalto.
        lwa_lstar-low = ls_afvc-lar01.
        APPEND lwa_lstar TO lr_lstar.
      ENDIF.
      IF lwa_fmacrsc-parid = ls_parameters-par02
        AND ls_afvc-lar02 IS NOT INITIAL.
        lwa_lstar-sign = c_sign-include.
        lwa_lstar-option = c_operator_word-equalto.
        lwa_lstar-low = ls_afvc-lar02.
        APPEND lwa_lstar TO lr_lstar.
      ENDIF.
      IF lwa_fmacrsc-parid = ls_parameters-par03
        AND ls_afvc-lar03 IS NOT INITIAL.
        lwa_lstar-sign = c_sign-include.
        lwa_lstar-option = c_operator_word-equalto.
        lwa_lstar-low = ls_afvc-lar03.
        APPEND lwa_lstar TO lr_lstar.
      ENDIF.
      IF lwa_fmacrsc-parid = ls_parameters-par04
        AND ls_afvc-lar04 IS NOT INITIAL.
        lwa_lstar-sign = c_sign-include.
        lwa_lstar-option = c_operator_word-equalto.
        lwa_lstar-low = ls_afvc-lar04.
        APPEND lwa_lstar TO lr_lstar.
      ENDIF.
      IF lwa_fmacrsc-parid = ls_parameters-par05
        AND ls_afvc-lar05 IS NOT INITIAL.
        lwa_lstar-sign = c_sign-include.
        lwa_lstar-option = c_operator_word-equalto.
        lwa_lstar-low = ls_afvc-lar05.
        APPEND lwa_lstar TO lr_lstar.
      ENDIF.
      IF lwa_fmacrsc-parid = ls_parameters-par06
        AND ls_afvc-lar06 IS NOT INITIAL.
        lwa_lstar-sign = c_sign-include.
        lwa_lstar-option = c_operator_word-equalto.
        lwa_lstar-low = ls_afvc-lar06.
        APPEND lwa_lstar TO lr_lstar.
      ENDIF.

    ENDLOOP.

  ENDIF.

****
  lv_retfield = lv_dynprofield = lv_fieldname.
  IF lr_lstar[] IS NOT INITIAL.

*    SELECT t1~idactv t1~rstype t1~bill t1~actype t2~description
*      INTO TABLE lt_activity
*      FROM /agri/fmacact AS t1 INNER JOIN /agri/fmacactt AS t2 "#EC CI_BUFFJOIN
*      ON t1~idactv = t2~idactv
*      WHERE t1~rstype = lv_rstype
*      AND t2~spras = sy-langu
*      AND t1~actype IN lr_lstar
*      OR  t1~bill = c_no.

    SELECT t1~idactv t1~rstype t1~bill t1~actype t2~description
      INTO TABLE lt_activity
      FROM /agri/fmacact AS t1 INNER JOIN /agri/fmacactt AS t2 "#EC CI_BUFFJOIN
      ON t2~idactv = t1~idactv
      WHERE t1~rstype = lv_rstype
      AND ( t1~actype IN lr_lstar
      OR  t1~bill = c_no )
      AND t2~spras = sy-langu.

  ELSE.

*    SELECT t1~idactv t1~rstype t1~bill t1~actype t2~description
*      INTO TABLE lt_activity
*      FROM /agri/fmacact AS t1 INNER JOIN /agri/fmacactt AS t2 "#EC CI_BUFFJOIN
*      ON t1~idactv = t2~idactv
*      WHERE t1~rstype = lv_rstype
*      AND t2~spras = sy-langu
*      AND  t1~bill = c_no.

    SELECT t1~idactv t1~rstype t1~bill t1~actype t2~description
      INTO TABLE lt_activity
      FROM /agri/fmacact AS t1 INNER JOIN /agri/fmacactt AS t2 "#EC CI_BUFFJOIN
      ON t2~idactv = t1~idactv
      WHERE t1~rstype = lv_rstype
      AND t1~bill = c_no
      AND t2~spras = sy-langu.
  ENDIF.


  IF lv_rstype EQ c_rstype-labor.
    DELETE lt_activity WHERE rstype NE c_rstype-labor.
  ELSE.
    DELETE lt_activity WHERE rstype NE c_rstype-equnr.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'IDACTV'
*     retfield        = lv_retfield
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'IDACTV'
*     dynprofield     = lv_dynprofield
      stepl           = 1
      value           = 'X'
      value_org       = 'S'
    TABLES
      value_tab       = lt_activity
      return_tab      = lt_return
    EXCEPTIONS ##FM_SUBRC_OK
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
  ENDIF.
  lt_returntab = lt_return.

ENDFORM.                    "f4_resource_display
*&---------------------------------------------------------------------*
*&      Form  fcode_conf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_conf.
  DATA: ls_variant TYPE disvariant,
        lv_subrc   TYPE sysubrc,
        lwa_achdr  TYPE /agri/s_fmachdr.

  CHECK /agri/s_fmachdr-status = c_process_status-ctd
   OR /agri/s_fmachdr-status = c_process_status-cnf.

  CLEAR: gs_variables-errors.
  gs_variables-initiator = c_log_initiator-post.
  PERFORM messages_initialize USING gs_variables-initiator
                                    c_log_subobject-save
                                    lwa_achdr.

  PERFORM confirmation_data_check CHANGING lv_subrc.

  PERFORM confirmation_infocus_save USING c_true
                                    CHANGING lv_subrc.

  PERFORM messages_display USING gs_variables-initiator.

  IF lv_subrc EQ 0.
    PERFORM worklist_update.
    CLEAR gs_variables-data_changed.
    PERFORM document_infocus_set USING    gs_acdoc_infocus-x-achdr-accom
                                 CHANGING lv_subrc.

    gs_variables-document_mode = c_mode_display.
    gs_variables-overview_mode = c_mode_display.
  ENDIF.
ENDFORM.                    "fcode_conf
*&---------------------------------------------------------------------*
*&      Form  FIELD_VALUE_CONVERSIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0128   text
*----------------------------------------------------------------------*
FORM field_value_conversions  USING lv_int_ext.

  DATA: lv_fieldname      TYPE fnam_____4,
        lv_tabix          TYPE sytabix,
        lv_msgid          TYPE symsgid,
        lv_msgty          TYPE symsgty,
        lv_msgno          TYPE symsgno,
        lv_msgv1          TYPE symsgv,
        lv_msgv2          TYPE symsgv,
        lv_msgv3          TYPE symsgv,
        lv_msgv4          TYPE symsgv,
        lv_int_value(128) TYPE c.

***lv_int_ext 1 = int-ext 2 = ext-int
  DATA: ls_table_field LIKE tabfield.

  FIELD-SYMBOLS: <lv_field_value>      TYPE any,
*                 <lv_value> TYPE any,
                 <lwa_additional_data> TYPE /agri/s_abgl_user_scrfields.

  LOOP AT gt_additional_data ASSIGNING <lwa_additional_data>.

    ASSIGN COMPONENT <lwa_additional_data>-fieldname
       OF STRUCTURE gs_acdoc_infocus-x-achdr TO <lv_field_value>.
    IF sy-subrc EQ 0.
      ls_table_field-fieldname = <lwa_additional_data>-fieldname.
***Single Field
      ls_table_field-tabname = <lwa_additional_data>-tabname.
      IF lv_int_ext EQ '2'.
        CHECK gs_variables-refresh_additional_data_grid IS INITIAL.
        CALL FUNCTION 'RS_CONV_EX_2_IN'
          EXPORTING
            input_external               = <lwa_additional_data>-fieldval
            table_field                  = ls_table_field
*           CURRENCY                     =
          IMPORTING
            output_internal              = <lv_field_value>
          EXCEPTIONS ##FM_SUBRC_OK
            input_not_numerical          = 1
            too_many_decimals            = 2
            more_than_one_sign           = 3
            ill_thousand_separator_dist  = 4
            too_many_digits              = 5
            sign_for_unsigned            = 6
            too_large                    = 7
            too_small                    = 8
            invalid_date_format          = 9
            invalid_date                 = 10
            invalid_time_format          = 11
            invalid_time                 = 12
            invalid_hex_digit            = 13
            unexpected_error             = 14
            invalid_fieldname            = 15
            field_and_descr_incompatible = 16
            input_too_long               = 17
            no_decimals                  = 18
            invalid_float                = 19
            conversion_exit_error        = 20
            OTHERS                       = 21.

        lv_fieldname = ls_table_field-fieldname.
        CALL FUNCTION 'DDUT_INPUT_CHECK'
          EXPORTING
            tabname       = ls_table_field-tabname
            fieldname     = lv_fieldname
            value         = <lv_field_value>
          IMPORTING
            msgid         = lv_msgid
            msgty         = lv_msgty
            msgno         = lv_msgno
            msgv1         = lv_msgv1
            msgv2         = lv_msgv2
            msgv3         = lv_msgv3
            msgv4         = lv_msgv4
          EXCEPTIONS ##FM_SUBRC_OK
            no_ddic_field = 1
            illegal_move  = 2
            OTHERS        = 3.
        IF NOT lv_msgid IS INITIAL.
          MESSAGE ID lv_msgid TYPE lv_msgty NUMBER lv_msgno
          WITH lv_msgv1 lv_msgv2 lv_msgv3 lv_msgv4 INTO sy-msgli.
          message_simple space.
***CI-IP #3313
**          IF sy-subrc NE 0 AND lv_msgty EQ c_msg_type-error.
          IF sy-subrc NE 0 OR lv_msgty EQ c_msg_type-error.
            gs_variables-errors = c_true.
          ENDIF.
        ENDIF.

*        <lwa_additional_data>-fieldval = <lv_field_value>.

      ELSEIF lv_int_ext EQ '1'.
        IF <lwa_additional_data>-fieldtyp EQ 'D'.
          IF <lv_field_value> EQ '00000000'.
            CLEAR <lwa_additional_data>-fieldval.
          ELSE.
            CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
              EXPORTING
                date_internal            = <lv_field_value>
              IMPORTING
                date_external            = <lwa_additional_data>-fieldval
              EXCEPTIONS
                date_internal_is_invalid = 1
                OTHERS                   = 2.
            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
              message_simple space.
            ENDIF.
          ENDIF.
        ELSE.
          lv_int_value = <lv_field_value>.
****DS_CONV fm delimts data which has lenght more than 45
          IF <lwa_additional_data>-outputlen LT 46.
            CALL FUNCTION 'RS_DS_CONV_IN_2_EX'
              EXPORTING
                input       = lv_int_value
*               DESCR       =
                table_field = ls_table_field
              IMPORTING
                output      = <lwa_additional_data>-fieldval.
          ELSE.
            CALL FUNCTION '/AGRI/G_CONVRT_FIELD_TO_EXTERN'
              EXPORTING
                i_conve = <lwa_additional_data>-convexit
                i_feld  = lv_int_value
              IMPORTING
                e_feld  = <lwa_additional_data>-fieldval.
****
          ENDIF.
        ENDIF.
      ENDIF.
      keytext_get_simple  ls_table_field-tabname
                         <lwa_additional_data>-fieldname
                         <lv_field_value>
                         <lwa_additional_data>-fielddscr.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " FIELD_VALUE_CONVERSIONS
*&---------------------------------------------------------------------*
*&      Form  fcode_dist
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_dist.
  DATA: ls_variant TYPE disvariant,
        lv_subrc   TYPE sysubrc,
        lwa_achdr  TYPE /agri/s_fmachdr.

  CHECK /agri/s_fmachdr-status = c_process_status-cnf.

  CLEAR: gs_variables-errors.
  gs_variables-initiator = c_log_initiator-post.
  PERFORM messages_initialize USING gs_variables-initiator
                                    c_log_subobject-save
                                    lwa_achdr.

  PERFORM distributed_infocus_save USING c_true
                                    CHANGING lv_subrc.

  PERFORM messages_display USING gs_variables-initiator.

  IF lv_subrc EQ 0.
    PERFORM worklist_update.
    CLEAR gs_variables-data_changed.
    PERFORM document_infocus_set USING    gs_acdoc_infocus-x-achdr-accom
                                 CHANGING lv_subrc.

    gs_variables-document_mode = c_mode_display.
    gs_variables-overview_mode = c_mode_display.
  ENDIF.
ENDFORM.                    "fcode_dist
*&---------------------------------------------------------------------*
*&      Form  fcode_reve
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_reve.
  DATA: ls_variant TYPE disvariant,
        lv_subrc   TYPE sysubrc,
        lwa_achdr  TYPE /agri/s_fmachdr.

  CHECK /agri/s_fmachdr-status = c_process_status-dis.

  CLEAR: gs_variables-errors.
  gs_variables-initiator = c_log_initiator-post.
  PERFORM messages_initialize USING gs_variables-initiator
                                    c_log_subobject-save
                                    lwa_achdr.

  PERFORM reversed_infocus_save USING c_true
                                CHANGING lv_subrc.

  PERFORM messages_display USING gs_variables-initiator.

  IF lv_subrc EQ 0.
    PERFORM worklist_update.
    CLEAR gs_variables-data_changed.
    PERFORM document_infocus_set USING    gs_acdoc_infocus-x-achdr-accom
                                 CHANGING lv_subrc.

    gs_variables-document_mode = c_mode_display.
    gs_variables-overview_mode = c_mode_display.
  ENDIF.
ENDFORM.                    "fcode_reve
*&---------------------------------------------------------------------*
*&      Form  fcode_clos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_clos.
  DATA: ls_variant TYPE disvariant,
        lv_subrc   TYPE sysubrc,
        lwa_achdr  TYPE /agri/s_fmachdr,
        lv_answer.

  CHECK /agri/s_fmachdr-status = c_process_status-dis.

  CLEAR: gs_variables-errors.
  gs_variables-initiator = c_log_initiator-post.
  PERFORM messages_initialize USING gs_variables-initiator
                                    c_log_subobject-save
                                    lwa_achdr.

  popup_to_confirm TEXT-013 TEXT-052 c_true lv_answer.

  CHECK lv_answer EQ '1'.

  PERFORM closed_infocus_save USING c_true
                                CHANGING lv_subrc.

  PERFORM messages_display USING gs_variables-initiator.

  IF lv_subrc EQ 0.
    PERFORM worklist_update.
    CLEAR gs_variables-data_changed.
    PERFORM document_infocus_set USING    gs_acdoc_infocus-x-achdr-accom
                                 CHANGING lv_subrc.

    gs_variables-document_mode = c_mode_display.
    gs_variables-overview_mode = c_mode_display.
  ENDIF.
ENDFORM.                    "fcode_clos
*&---------------------------------------------------------------------*
*&      Form  fcode_dele
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_dele.
  DATA: ls_variant TYPE disvariant,
        lv_subrc   TYPE sysubrc,
        lwa_achdr  TYPE /agri/s_fmachdr,
        lv_answer.

  CHECK /agri/s_fmachdr-status = c_process_status-dis OR
        /agri/s_fmachdr-status = c_process_status-ctd.

  CLEAR: gs_variables-errors.
  gs_variables-initiator = c_log_initiator-post.
  PERFORM messages_initialize USING gs_variables-initiator
                                    c_log_subobject-save
                                    lwa_achdr.

  popup_to_confirm TEXT-013 TEXT-051 c_true lv_answer.

  CHECK lv_answer EQ '1'.

  PERFORM deleted_infocus_save USING c_true
                                CHANGING lv_subrc.

  PERFORM messages_display USING gs_variables-initiator.

  IF lv_subrc EQ 0.
    PERFORM worklist_update.
    CLEAR gs_variables-data_changed.
    PERFORM document_infocus_set USING    gs_acdoc_infocus-x-achdr-accom
                                 CHANGING lv_subrc.

    gs_variables-document_mode = c_mode_display.
    gs_variables-overview_mode = c_mode_display.
  ENDIF.
ENDFORM.                    "fcode_clos

*&---------------------------------------------------------------------*
*&      Form  fcode_revc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_revc.
  DATA: ls_variant TYPE disvariant,
        lv_subrc   TYPE sysubrc,
        lwa_achdr  TYPE /agri/s_fmachdr.

  CHECK /agri/s_fmachdr-status = c_process_status-ctd
   OR /agri/s_fmachdr-status = c_process_status-cnf.

  CLEAR: gs_variables-errors.
  gs_variables-initiator = c_log_initiator-post.
  PERFORM messages_initialize USING gs_variables-initiator
                                    c_log_subobject-save
                                    lwa_achdr.

  PERFORM reverse_infocus_save USING c_true
                               CHANGING lv_subrc.

  PERFORM messages_display USING gs_variables-initiator.

  IF lv_subrc EQ 0.
    PERFORM worklist_update.
    CLEAR gs_variables-data_changed.
    PERFORM document_infocus_set USING    gs_acdoc_infocus-x-achdr-accom
                                 CHANGING lv_subrc.

    gs_variables-document_mode = c_mode_display.
    gs_variables-overview_mode = c_mode_display.
  ENDIF.

ENDFORM.                    "fcode_revc
*&---------------------------------------------------------------------*
*&      Form  F4_ORDERS_DISPLAY
*&---------------------------------------------------------------------*
FORM f4_orders_display CHANGING
                       VALUE(lt_returntab)
                            lv_row_no.
*  TYPES: BEGIN OF src_resource,
*      aufnr  TYPE aufnr,
*    END OF src_resource.

  DATA: lwa_orders  TYPE /agri/s_aufnr,
        lt_orders   TYPE /agri/t_aufnr,
        lt_return   TYPE STANDARD TABLE OF ddshretval,
        lt_wonum    TYPE /agri/t_fmwonum,
        lt_fmwo_doc TYPE /agri/t_fmwoc_doc,
        ls_fmwo_doc TYPE /agri/s_fmwoc_doc,
        lv_tplnr_fl TYPE /agri/gltplnr_fl.

  FIELD-SYMBOLS <lwa_fmwoitm_doc> TYPE /agri/s_fmwoitm.

*  IF /agri/s_fmachdr-wonum IS NOT INITIAL.
*    APPEND /agri/s_fmachdr-wonum TO lt_wonum.
*
*    CALL FUNCTION '/AGRI/FMWO_VIEW'
*      EXPORTING
*        it_wonum       = lt_wonum
*      IMPORTING
*        et_wodoc       = lt_fmwo_doc
*      EXCEPTIONS ##FM_SUBRC_OK
*        no_data_exists = 1
*        OTHERS         = 2.
*    READ TABLE lt_fmwo_doc INTO ls_fmwo_doc INDEX 1.
*
*  ENDIF.
*
*  LOOP AT ls_fmwo_doc-x-woitm ASSIGNING <lwa_fmwoitm_doc>.
*    MOVE <lwa_fmwoitm_doc>-aufnr TO lwa_orders-aufnr.
*    APPEND lwa_orders TO lt_orders.
*  ENDLOOP.

  READ TABLE gt_items_layout ASSIGNING FIELD-SYMBOL(<fs_items_layout>)
  INDEX lv_row_no.
  IF sy-subrc = 0.

    CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
      EXPORTING
        input  = <fs_items_layout>-tplnr
      IMPORTING
        output = lv_tplnr_fl.

    LOOP AT gt_details INTO DATA(lwa_details) WHERE tplnr_fl = lv_tplnr_fl
                                                AND matnr    = <fs_items_layout>-tmatnr.
      MOVE lwa_details-aufnr TO lwa_orders-aufnr.
      APPEND lwa_orders TO lt_orders.
    ENDLOOP.
    DESCRIBE TABLE lt_orders LINES DATA(lv_ord_lines).
    IF lv_ord_lines = 1.
      <fs_items_layout>-aufnr = lt_orders[ 1 ]-aufnr.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'AUFNR'
*     retfield        = lv_retfield
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'AUFNR'
*     dynprofield     = lv_dynprofield
      stepl           = 1
      value           = 'X'
      value_org       = 'S'
    TABLES
      value_tab       = lt_orders
      return_tab      = lt_return
    EXCEPTIONS ##FM_SUBRC_OK
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
  ENDIF.

  lt_returntab = lt_return.

ENDFORM.                    " F4_ORDERS_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  FCODE_DESC_DELETE
*&---------------------------------------------------------------------*
FORM fcode_desc_delete.

  DATA: lt_rows         TYPE lvc_t_row,
        lwa_row         TYPE lvc_s_row,
        lv_answer       TYPE c,
        lwa_desc_layout LIKE LINE OF gt_ac_desc_layout.

  FIELD-SYMBOLS: <lwa_desc_modi> TYPE lvc_s_modi,
                 <lwa_desc>      TYPE /agri/s_fmachdrt.

  popup_to_confirm TEXT-007 TEXT-032 c_true lv_answer.

  IF lv_answer EQ '1'  .

    CALL METHOD ref_multi_lang_desc_grid->get_selected_rows
      IMPORTING
        et_index_rows = lt_rows.

    CHECK lt_rows IS NOT INITIAL.
    gs_variables-refresh_grid_desc = c_true.
    SORT lt_rows BY index DESCENDING.

    LOOP AT lt_rows INTO lwa_row.

      LOOP AT gt_desc_mod_rows ASSIGNING <lwa_desc_modi> WHERE row_id GE lwa_row-index.
        IF <lwa_desc_modi>-row_id NE lwa_row-index.
          <lwa_desc_modi>-row_id = <lwa_desc_modi>-row_id - 1.
        ELSE.
          DELETE gt_desc_mod_rows WHERE row_id EQ <lwa_desc_modi>-row_id.
        ENDIF.
      ENDLOOP.

      READ TABLE gt_ac_desc_layout INTO lwa_desc_layout INDEX lwa_row-index.
      READ TABLE gs_acdoc_infocus-y-acdes ASSIGNING <lwa_desc>
                             WITH KEY spras = lwa_desc_layout-spras.

      IF sy-subrc EQ 0.
        <lwa_desc>-updkz = c_updkz_delete.
      ENDIF.

      DELETE gs_acdoc_infocus-x-acdes INDEX lwa_row-index.
      DELETE gt_ac_desc_layout INDEX lwa_row-index.

      IF lwa_desc_layout-spras = sy-langu.
        READ TABLE gs_acdoc_infocus-x-acdes TRANSPORTING NO FIELDS
                                  WITH KEY spras = sy-langu.
        IF sy-subrc NE 0.
          CLEAR gs_acdoc_infocus-x-achdr-descr.
        ENDIF.
        IF gs_acdoc_infocus-x-achdr-updkz NE c_updkz_new.
          gs_acdoc_infocus-x-achdr-updkz = c_updkz_update.
        ENDIF.
      ENDIF.

      UNASSIGN: <lwa_desc>.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " FCODE_DESC_DELETE
*&---------------------------------------------------------------------*
*&      Form  fcode_push
*&---------------------------------------------------------------------*
FORM fcode_push.
  DATA: lt_wo         TYPE /agri/t_fmac_wo,
        lt_fp         TYPE /agri/t_fmac_fp,
        lt_cmprs      TYPE /agri/t_fmac_cmprs,
        lt_fptmp      TYPE /agri/t_fmac_fp,
        lt_fpitm      TYPE /agri/t_fmac_fpitm,
        lwa_fp        TYPE /agri/s_fmac_fp,
        lwa_aufnr     LIKE gr_aufnr,
        lt_tab_field  TYPE TABLE OF rstabfield,
        lwa_tab_field TYPE rstabfield,
        lwa_exl_opt   TYPE rsoptions.

  FIELD-SYMBOLS: <lwa_wo>    TYPE /agri/s_fmac_wo,
                 <lwa_fpitm> TYPE /agri/s_fmac_fpitm,
                 <lwa_cmprs> TYPE /agri/s_fmac_cmprs.

  MOVE: 'AUFNR' TO lwa_tab_field-fieldname ,
         'AUFK' TO lwa_tab_field-tablename .
  APPEND lwa_tab_field TO lt_tab_field.

  MOVE: c_true TO lwa_exl_opt-bt ,
        c_true TO lwa_exl_opt-cp ,
        c_true TO lwa_exl_opt-ge ,
        c_true TO lwa_exl_opt-gt ,
        c_true TO lwa_exl_opt-le ,
        c_true TO lwa_exl_opt-lt ,
        c_true TO lwa_exl_opt-nb ,
        c_true TO lwa_exl_opt-np ,
        c_true TO lwa_exl_opt-ne .

  CLEAR gs_variables-errors.
  REFRESH: gr_aufnr[].

  IF aufk-aufnr IS NOT INITIAL.
    READ TABLE gr_aufnrtmp TRANSPORTING NO FIELDS
                          WITH KEY low = aufk-aufnr.
    IF sy-subrc NE 0.
      IF gr_aufnrtmp IS INITIAL.
        gr_aufnrtmp-option = c_operator_word-equalto.
        gr_aufnrtmp-sign   = c_sign-include.
        gr_aufnrtmp-low    = aufk-aufnr.
        APPEND gr_aufnrtmp TO gr_aufnrtmp[].
      ELSE.
        gr_aufnrtmp-option = c_operator_word-equalto.
        gr_aufnrtmp-sign   = c_sign-include.
        gr_aufnrtmp-low    = aufk-aufnr.
        APPEND gr_aufnrtmp TO gr_aufnrtmp[].
      ENDIF.
      APPEND gr_aufnrtmp TO gr_aufnrtmp[].
    ENDIF.
  ENDIF.

  IF gr_aufnrtmp[] IS NOT INITIAL.
    gr_aufnr[] = gr_aufnrtmp[].
  ENDIF.

  CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
    EXPORTING
      excluded_options  = lwa_exl_opt
      no_interval_check = c_true
      just_incl         = c_true
      help_field        = '/AGRI/FMFPHDR-AUFNR'
      tab_and_field     = lwa_tab_field
    TABLES
      range             = gr_aufnr
    EXCEPTIONS ##FM_SUBRC_OK
      no_range_tab      = 1
      cancelled         = 2
      internal_error    = 3
      invalid_fieldname = 4
      OTHERS            = 5.
  IF sy-subrc <> 0.
  ENDIF.

  gr_aufnrtmp[] = gr_aufnr[].
  PERFORM aufnr_data_check.
  REFRESH gr_aufnr[].

  IF gs_variables-errors NE c_true.
    CLEAR aufk-aufnr.
    gt_fphdr = lt_fp.
    READ TABLE gr_aufnrtmp[] INTO lwa_aufnr INDEX 1.
    aufk-aufnr = lwa_aufnr-low.
  ENDIF.

ENDFORM.                    "fcode_push
*&---------------------------------------------------------------------*
*&      Form  fcode_setv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_setv.

  DATA: lv_save,
        lv_where         TYPE string,
        lt_rows          TYPE lvc_t_row,
        lwa_selected_col TYPE lvc_s_col,
        lt_selected_col  TYPE lvc_t_col,
        lwa_selected_row TYPE lvc_s_row,
        lt_selected_rows TYPE lvc_t_row,
        lv_selected_rows TYPE boole_d,
        lv_valid.

  CALL METHOD ref_grid_items->get_selected_rows
    IMPORTING
      et_index_rows = lt_selected_rows.

  CALL METHOD ref_grid_items->get_selected_columns
    IMPORTING
      et_index_columns = lt_selected_col.

  IF lt_selected_rows IS INITIAL.
    popup_to_confirm TEXT-013 TEXT-054 c_true lv_save.
    IF lv_save EQ '1'.

    ELSEIF lv_save EQ '2'.
      READ TABLE lt_selected_col INTO lwa_selected_col INDEX 1.
      IF sy-subrc EQ 0 AND
         lwa_selected_col-fieldname IS NOT INITIAL.
        CONCATENATE lwa_selected_col-fieldname 'IS INITIAL'
               INTO lv_where SEPARATED BY space.
        LOOP AT <gt_fmac_fcat> TRANSPORTING NO FIELDS
                              WHERE (lv_where).
          lwa_selected_row-index = sy-tabix.
          APPEND lwa_selected_row TO lt_selected_rows.
        ENDLOOP.
      ENDIF.
      IF lt_selected_rows IS INITIAL.
        EXIT.
      ENDIF.
    ELSE.
      EXIT.
    ENDIF.
  ENDIF.

  lv_selected_rows = c_true.

  CALL METHOD ref_grid_items->select_and_compute
    EXPORTING
      i_only_selected_rows = lv_selected_rows
*     i_init_container     =
*     it_selected_columns  =
*     i_show_dialog        = 'X'
*     i_apply_all_records  =
*     it_conditions        =
*     it_condition_columns =
      it_selected_rows     = lt_selected_rows
*     is_select_n_compute  =
    RECEIVING
      rt_row_indexes       = lt_rows.

  PERFORM items_grid_update.



ENDFORM.                    "fcode_setv
*&---------------------------------------------------------------------*
*&      Form  FLOW_DOCUMENTS_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM flow_documents_check  CHANGING lt_fmbck_doc TYPE /agri/t_fmbck_doc.

  DATA: lt_fmbcknum TYPE /agri/t_fmbcknum.


  SELECT * FROM /agri/fmbckitm
        INTO CORRESPONDING FIELDS OF TABLE lt_fmbcknum
    WHERE addoc EQ gs_acdoc_infocus-accom
        AND bukrs EQ /agri/s_fmachdr-bukrs.

  SORT lt_fmbcknum ASCENDING BY bcknum.
  DELETE ADJACENT DUPLICATES FROM lt_fmbcknum.

**  SELECT * FROM zfmbckhdr
**         INTO CORRESPONDING FIELDS OF TABLE lt_fmbcknum
**      WHERE bckdsc EQ gs_acdoc_infocus-accom
**       AND   bukrs EQ zcs_fmachdr-bukrs.

  IF lt_fmbcknum[] IS NOT INITIAL.
    CALL FUNCTION '/AGRI/FMBCK_VIEW'
      EXPORTING
        it_bcknum      = lt_fmbcknum
      IMPORTING
        et_fmbckdoc    = lt_fmbck_doc
      EXCEPTIONS ##FM_SUBRC_OK
        no_data_exists = 1
        OTHERS         = 2.
  ENDIF.

ENDFORM.                    "flow_documents_check
