*----------------------------------------------------------------------*
***INCLUDE LZABS_FG_DISP_ORDO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITIALIZE_GLOBAL_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM initialize_global_data .
  CLEAR: gs_csdoc_infocus, gs_variables, gt_disp_ord_fcat, gt_csdoc_infocus.
ENDFORM.

*&---------------------------------------------------------------------*
*& Module STATUS_SET OUTPUT
*&---------------------------------------------------------------------*
*& Status Set
*&---------------------------------------------------------------------*
MODULE status_set OUTPUT.
  PERFORM status_set.
ENDMODULE.            "STATUS_SET OUTPUT

*&---------------------------------------------------------------------*
*& Module TITLE_SET OUTPUT
*&---------------------------------------------------------------------*
*& Title Set
*&---------------------------------------------------------------------*
MODULE title_set OUTPUT.
  PERFORM title_set.
ENDMODULE.            "TITLE_SET OUTPUT

*&---------------------------------------------------------------------*
*& Module CONTROLS_DISPLAY OUTPUT
*&---------------------------------------------------------------------*
*& Controls Display
*&---------------------------------------------------------------------*
MODULE controls_display OUTPUT.
  PERFORM controls_display.
ENDMODULE.            "CONTROLS_DISPLAY OUTPUT

*&---------------------------------------------------------------------*
*& Form TITLE_SET
*&---------------------------------------------------------------------*
*& Title Set
*&---------------------------------------------------------------------*
FORM title_set.

  CASE sy-dynnr.
    WHEN c_screen-disp_ord.
      SET TITLEBAR 'T100'.
    WHEN c_screen-create_batch.
      SET TITLEBAR 'T201'.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.            "TITLE_SET

*&---------------------------------------------------------------------*
*& Form STATUS_SET
*&---------------------------------------------------------------------*
*& Status Set
*&---------------------------------------------------------------------*
FORM status_set.

  CASE sy-dynnr.
    WHEN c_screen-disp_ord.
*--Status set for screen 100
      SET PF-STATUS 'S100'.
    WHEN c_screen-create_batch.
*--Status set for screen 201
      SET PF-STATUS 'S201'.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.            "STATUS_SET

*&---------------------------------------------------------------------*
*& Form CONTROLS_DISPLAY
*&---------------------------------------------------------------------*
*& Controls Display
*&---------------------------------------------------------------------*
FORM controls_display .

*--Local Variables
  DATA: lv_title 	          TYPE lvc_title,
        lv_itm_indx         TYPE i,
        lv_itm_indx_tmp(20) TYPE n,
        lv_tot_itms(20)     TYPE n,
        lv_input            TYPE i.

*--Internal Tables
  DATA: lt_fcat             TYPE lvc_t_fcat,
        lt_toolbar_excludes TYPE ui_functions.

*--Workareas
  DATA: ls_row_info           TYPE lvc_s_row,
        ls_col_info           TYPE lvc_s_col,
        ls_rowid_current_cell TYPE lvc_s_row,
        ls_colid_current_cell TYPE lvc_s_col,
        ls_environment        TYPE /agri/s_glvc_environment,
        ls_variant            TYPE disvariant,
        ls_layout             TYPE lvc_s_layo.

  IF ref_container_disp_ord IS INITIAL.

    CREATE OBJECT ref_container_disp_ord
      EXPORTING
        container_name              = '/AGRI/SAPLFMFPM_0100_CC'
        repid                       = sy-repid
        dynnr                       = '0100'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CHECK sy-subrc EQ 0.

    ls_environment-switchoff_performance = abap_true.
    CREATE OBJECT ref_grid_disp_ord
      EXPORTING
        i_parent           = ref_container_disp_ord
        is_lvc_environment = ls_environment
      EXCEPTIONS
        error_cntl_create  = 1
        error_cntl_init    = 2
        error_cntl_link    = 3
        error_dp_create    = 4
        OTHERS             = 5.

    CHECK sy-subrc EQ 0.

    PERFORM control_events_register.

*--Preparing Field Catalog
    PERFORM field_catalog_prepare CHANGING lt_fcat.

    ls_layout-info_fname = 'DDHDL'.
    ls_layout-stylefname = 'STYLES'.
    ls_layout-cwidth_opt = abap_true.
    ls_layout-sel_mode   = 'D'.
    ls_layout-smalltitle = abap_true.

    ls_variant-report = sy-repid.
    ls_variant-handle = '0100'.

    PERFORM toolbar_buttons_exclude TABLES lt_toolbar_excludes.

    CALL METHOD ref_grid_disp_ord->set_table_for_first_display
      EXPORTING
        is_variant                    = ls_variant
        i_save                        = 'A'
        is_layout                     = ls_layout
        it_toolbar_excluding          = lt_toolbar_excludes[]
      CHANGING
        it_outtab                     = gt_disp_ord_fcat
        it_fieldcatalog               = lt_fcat[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    CALL METHOD ref_grid_disp_ord->set_toolbar_interactive.

    CLEAR: gs_variables-refresh_disp_ord.

  ELSE.

    PERFORM field_catalog_prepare CHANGING lt_fcat.

    ref_grid_disp_ord->set_frontend_fieldcatalog( it_fieldcatalog = lt_fcat ).

    ls_layout-info_fname = 'DDHDL'.
    ls_layout-stylefname = 'STYLES'.
    ls_layout-cwidth_opt = abap_true.
    ls_layout-sel_mode   = 'D'.
    ls_layout-smalltitle = abap_true.

    ref_grid_disp_ord->set_frontend_layout( is_layout = ls_layout ).

    ref_grid_disp_ord->refresh_table_display( ).

  ENDIF.

  lv_input = 1.

  CALL METHOD ref_grid_disp_ord->set_ready_for_input
    EXPORTING
      i_ready_for_input = lv_input.

*---Register Edit Event For Grid
  CALL METHOD ref_grid_disp_ord->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

*    ref_grid_disp_ord->on_data_changed_grid( ).

ENDFORM.            "CONTROLS_DISPLAY

*&---------------------------------------------------------------------*
*& Module FCODE_PROCESSING INPUT
*&---------------------------------------------------------------------*
* Fcode Processing
*----------------------------------------------------------------------*
MODULE fcode_processing INPUT.
  PERFORM fcode_processing.
ENDMODULE.            "FCODE_PROCESSING INPUT

*&---------------------------------------------------------------------*
*& Form FCODE_PROCESSING
*&---------------------------------------------------------------------*
*& Fcode Processing
*&---------------------------------------------------------------------*
FORM fcode_processing.

*--Local Variable
  DATA: lt_stack           TYPE cl_abap_get_call_stack=>call_stack_internal,
        lt_formatted_stack TYPE cl_abap_get_call_stack=>formatted_entry_stack,
        lv_subroutine(40)  TYPE c VALUE 'FCODE_'.

  fcode = ok_code.
  CLEAR ok_code.

*--When the user presses ENTER directly, the program is generating batch
*--without validating the mandatory information.
  IF sy-dynnr EQ '0201'
  AND fcode IS INITIAL.
    lt_stack = cl_abap_get_call_stack=>get_call_stack( ).
    lt_formatted_stack = cl_abap_get_call_stack=>format_call_stack_with_struct( lt_stack ).

    READ TABLE lt_formatted_stack TRANSPORTING NO FIELDS
      WITH KEY kind        = 'FORM'
               progname    = '/AGRI/SAPLFMNSM'
               includename = 'ZABS_INC_BATCH'
               event       = 'FCODE_CBCH'.

    fcode = 'CONT'.
  ENDIF.

  CONCATENATE lv_subroutine fcode INTO lv_subroutine.
  PERFORM (lv_subroutine) IN PROGRAM (sy-repid)
                          IF FOUND.

ENDFORM.            "FCODE_PROCESSING

*&---------------------------------------------------------------------*
*& Form FCODE_CANC
*&---------------------------------------------------------------------*
*& Fcode Canc
*&---------------------------------------------------------------------*
FORM fcode_canc.
  gv_canc = abap_true.
  LEAVE TO SCREEN 0.
ENDFORM.            "FCODE_CANC

*&---------------------------------------------------------------------*
*& Form FCODE_CONT
*&---------------------------------------------------------------------*
*& Fcode Cont
*&---------------------------------------------------------------------*
FORM fcode_cont.

*--Workarea declarations
  DATA : ls_var TYPE ty_var.

  FIELD-SYMBOLS : <lfs_fpdoc> TYPE /agri/s_fmfphdr.

  CLEAR gs_variables-error.
  CASE sy-dynnr.
    WHEN c_screen-disp_ord.
      LEAVE TO SCREEN 0.
    WHEN c_screen-create_batch.

      IF zabs_str_batch-erfmg IS INITIAL.
        MESSAGE s001(zabs_msgcls)
        DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_error . "Enter Quantity
        gs_variables-error = abap_true.
        RETURN.
      ENDIF.

      IF zabs_str_batch-date IS INITIAL.
        MESSAGE s140(zabs_msgcls)
        DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_error . "Enter Date
        gs_variables-error = abap_true.
        RETURN.
      ENDIF.

      IF zabs_str_batch-meins IS INITIAL.
        MESSAGE s141(zabs_msgcls)
        DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_error . "Enter UOM
        gs_variables-error = abap_true.
        RETURN.
      ELSE.
*--Validating UOM
        SELECT meinh
          FROM marm
          INTO TABLE @DATA(lt_meinh)
         WHERE matnr EQ @zabs_str_batch-matnr
           AND meinh EQ @zabs_str_batch-meins.
        IF sy-subrc <> 0.
          MESSAGE s142(zabs_msgcls)
          DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_error . "Enter valid UOM
          gs_variables-error = abap_true.
          RETURN.
        ENDIF.

      ENDIF.

*-- BOC commenting as per request from Jonathan on 11.06.2021
*--Validating Measurement Document
*      IF zabs_str_batch-mdocm IS INITIAL.
*        MESSAGE s155(zabs_msgcls)
*        DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_error . "Enter MD
*        gs_variables-error = abap_true.
*        RETURN.
*      ELSE.
*        ASSIGN ('(/AGRI/SAPLFMNSM)GS_FPDOC_INFOCUS-X-FPHDR') TO <lfs_fpdoc>.
*
*        IF <lfs_fpdoc> IS ASSIGNED.
*          SELECT SINGLE zzmpgrp
*            FROM /agri/glcmhdr
*            INTO @DATA(lv_glcmhdr)
*           WHERE cmnum EQ @<lfs_fpdoc>-cmnum
*             AND loevm EQ @space.
*
*          IF sy-subrc = 0.
*            SELECT mdocm, tplnr_fl,
*                   contr, mpgrp,
*                   canceled
*              FROM /agri/glmdhdr
*              INTO @DATA(lwa_glmdhdr)
*             WHERE mdocm EQ @zabs_str_batch-mdocm
*               AND tplnr_fl EQ @<lfs_fpdoc>-tplnr_fl
*               AND mpgrp    EQ @lv_glcmhdr
*               AND kfrst    EQ @space.
*            ENDSELECT.
*
*            IF sy-subrc <> 0.
*              MESSAGE s156(zabs_msgcls)
*              DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_error.
*              gs_variables-error = abap_true.
*              RETURN.
*            ELSE.
*              IF lwa_glmdhdr-canceled EQ abap_true.
**-- Documento de Medição &1 está cancelado. Selecione documento válido.
*                MESSAGE s154(zfmfp) WITH lwa_glmdhdr-mdocm
*                DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_error.
*                gs_variables-error = abap_true.
*                RETURN.
*              ENDIF.
*            ENDIF.
*          ENDIF.
*
**--Validating Existence of Measurement Document
*          SELECT a~aufnr,
*                 b~charg,
*                 b~zzmdocm
*            INTO @DATA(lwa_mdocm)
*              UP TO 1 ROWS
*            FROM /agri/fmfphdr AS a
*           INNER JOIN /agri/fmfpbch AS b
*              ON b~aufnr EQ b~aufnr
*           WHERE a~auart    EQ @<lfs_fpdoc>-auart
*             AND a~autyp    EQ 'AO'
*             AND a~tplnr_fl EQ @<lfs_fpdoc>-tplnr_fl
*             AND a~contr    EQ @<lfs_fpdoc>-contr
*             AND a~tecom    EQ @space
*             AND b~zzloevm  EQ @space
*             AND b~zzmdocm  EQ @zabs_str_batch-mdocm.
*          ENDSELECT.
*
*          IF sy-subrc = 0.
*            MESSAGE s157(zabs_msgcls) DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_error
*            WITH lwa_mdocm-zzmdocm lwa_mdocm-aufnr lwa_mdocm-charg.
*            gs_variables-error = abap_true.
*            RETURN.
*          ENDIF.
*
*        ENDIF. "<lfs_fpdoc"
*      ENDIF.
*-- EOC

      PERFORM number_range CHANGING ls_var.
*--Calling Number Range FM to assign next free number
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr = ls_var-contr  "'01'
          object      = zcl_abs_abap_maintain=>c_obj_btch_nr. "'ZABS_BATCH'.
      SET SCREEN 0.
      LEAVE SCREEN.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.            "FCODE_CONT

*&---------------------------------------------------------------------*
*& Form FCODE_CRCA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM fcode_crca.

  DATA: lt_rows     TYPE lvc_t_row,
        lt_fpdoc    TYPE /agri/t_fmfp_doc,
        lt_fpitm    TYPE /agri/t_fmfpitm,
        lt_fcat     TYPE lvc_t_fcat,
        lwa_fpdoc   TYPE /agri/s_fmfp_doc,
        lwa_fpitm   TYPE /agri/s_fmfpitm,
        lt_messages TYPE /agri/t_gprolog,
        ls_message  TYPE /agri/s_gprolog,
        lwa_msg_log TYPE zabs_str_dispord_msg_log,
        lv_subrc    TYPE sy-subrc,
        lv_aufnr    TYPE aufnr,
        lv_valid,
        lv_answer.

  DATA: lt_tab_dispord TYPE tty_tab_dispord,
        lt_msg_log     TYPE zabs_tty_dispord_msg_log,
        ls_tab_dispord TYPE zabs_tab_dispord.

  DATA: lv_lines TYPE sy-tabix,
        lv_cont  TYPE sy-tabix.

  FIELD-SYMBOLS: <lwa_taskord_fcat>    TYPE /agri/s_fmfp_taskord_mass_fcat,
                 <lwa_fpdisp_ord_temp> TYPE zabs_str_dispord_fcat. "--23/03/2017...

  CALL METHOD ref_grid_disp_ord->check_changed_data
    IMPORTING
      e_valid = lv_valid.

  IF lv_valid IS INITIAL.
    CLEAR ok_code.
    EXIT.
  ENDIF.

  CALL METHOD ref_grid_disp_ord->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.

  IF lt_rows IS INITIAL.
    MESSAGE i004(zabs_msgcls) DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_info.
    RETURN.
  ELSE.
    DATA(lt_disp_ord_temp) = gt_disp_ord_fcat.
    LOOP AT lt_rows INTO DATA(lwa_rows).
      READ TABLE lt_disp_ord_temp ASSIGNING FIELD-SYMBOL(<fs_ord_temp>)
      INDEX lwa_rows-index.
      IF sy-subrc = 0.
        CLEAR <fs_ord_temp>-dmatnr.
      ENDIF.
      READ TABLE gt_disp_ord_fcat INTO DATA(lwa_disp_ord)
      INDEX lwa_rows-index.
      IF sy-subrc = 0.
        IF lwa_disp_ord-disord IS NOT INITIAL.
          MESSAGE e007(zabs_msgcls) INTO lwa_msg_log-msgtxt.
          lwa_msg_log-nustn  = lwa_disp_ord-nustn.
          lwa_msg_log-dmatnr = lwa_disp_ord-dmatnr.
          lwa_msg_log-status = 1.
          APPEND lwa_msg_log TO lt_msg_log.
        ENDIF.
        READ TABLE lt_disp_ord_temp TRANSPORTING NO FIELDS
          WITH KEY nustn  = lwa_disp_ord-nustn
                   contr  = lwa_disp_ord-contr
                   dmatnr = lwa_disp_ord-dmatnr.
        IF sy-subrc = 0.
          MESSAGE e008(zabs_msgcls) INTO lwa_msg_log-msgtxt.
          lwa_msg_log-nustn  = lwa_disp_ord-nustn.
          lwa_msg_log-dmatnr = lwa_disp_ord-dmatnr.
          lwa_msg_log-status = 1.
          APPEND lwa_msg_log TO lt_msg_log.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  popup_to_confirm TEXT-001 TEXT-002 abap_true lv_answer.
  IF lv_answer EQ '1'.

    gs_variables-initiator = c_log_initiator-check.
    PERFORM messages_initialize IN PROGRAM /agri/saplglcsm
                                IF FOUND USING gs_variables-initiator
                                               c_log_subobject-save.

    MOVE-CORRESPONDING gs_csdoc_infocus-x-cshdr TO /agri/s_glflcma.
    PERFORM messages_context_set IN PROGRAM /agri/saplglcsm
                                 IF FOUND USING gs_csdoc_infocus-x-cshdr.

    PERFORM object_create_check IN PROGRAM /agri/saplglcsm
                                IF FOUND CHANGING lv_subrc.
    IF NOT lv_subrc IS INITIAL.
      CLEAR lv_subrc.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM /agri/glflot
      INTO @DATA(lwa_glflot)
     WHERE tplnr_fl EQ @gs_csdoc_infocus-tplnr_fl
       AND loevm    EQ @space.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    LOOP AT lt_rows INTO lwa_rows.
      CLEAR: lv_aufnr, ls_tab_dispord, lv_subrc, lwa_msg_log.
      READ TABLE gt_disp_ord_fcat ASSIGNING FIELD-SYMBOL(<fs_disp_ord>)
      INDEX lwa_rows-index.
      IF sy-subrc = 0.
        gs_csdoc_infocus-x-cshdr-ymatnr = <fs_disp_ord>-dmatnr.
        gs_csdoc_infocus-x-cshdr-eston  = <fs_disp_ord>-eston.
        gs_csdoc_infocus-x-cshdr-esuom  = <fs_disp_ord>-esuom.
        gs_csdoc_infocus-x-cshdr-iwerk  = <fs_disp_ord>-nwerks.
        gs_csdoc_infocus-x-cshdr-yaufnr = <fs_disp_ord>-disord.
        IF <fs_disp_ord>-disord IS NOT INITIAL.
          CONTINUE.
        ENDIF.
        PERFORM create_yard_order USING lwa_glflot
                               CHANGING lv_subrc
                                        lv_aufnr
                                        lwa_msg_log.
        IF lv_subrc IS INITIAL.
          ls_tab_dispord-disord = <fs_disp_ord>-disord = lv_aufnr.
        ENDIF.
        MOVE-CORRESPONDING <fs_disp_ord> TO ls_tab_dispord.
        IF ls_tab_dispord-dmatnr IS NOT INITIAL.
          APPEND ls_tab_dispord TO lt_tab_dispord.
        ENDIF.
        APPEND lwa_msg_log TO lt_msg_log.
        IF lv_aufnr IS INITIAL.
          CLEAR lwa_msg_log.
          MESSAGE e011(zabs_msgcls) INTO lwa_msg_log-msgtxt.
          lwa_msg_log-nustn  = <fs_disp_ord>-nustn.
          lwa_msg_log-dmatnr = <fs_disp_ord>-dmatnr.
          lwa_msg_log-status = 1.
          APPEND lwa_msg_log TO lt_msg_log.
        ELSE.
          PERFORM disp_ord_styles_prepare CHANGING <fs_disp_ord>.
        ENDIF.
      ENDIF.
    ENDLOOP.

    PERFORM cs_mass_save IN PROGRAM /agri/saplglcsm
                         IF FOUND CHANGING lv_subrc.

    PERFORM disp_ord_db_update USING lt_tab_dispord.

    PERFORM messages_display USING lt_msg_log.

  ENDIF.

ENDFORM.            "FCODE_CRCA

*&---------------------------------------------------------------------*
*&      Form  CREATE_YARD_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_yard_order USING lwa_glflot TYPE /agri/glflot
                    CHANGING lv_subrc   TYPE sy-subrc
                             lv_aufnr   TYPE aufnr
                             lwa_msg_log TYPE zabs_str_dispord_msg_log.

  DATA: lwa_order_comm TYPE /agri/s_glpocomm,
        lwa_messtab    TYPE bdcmsgcoll,
        lt_messtab     TYPE tab_bdcmsgcoll.

  IF NOT gs_csdoc_infocus-x-cshdr-yaufnr IS INITIAL OR
     NOT gs_csdoc_infocus-x-cshdr-loevm IS INITIAL OR
     gs_csdoc_infocus-x-cshdr-astat NE c_crop_season_status-active.
    EXIT.
  ENDIF.

  MOVE-CORRESPONDING lwa_glflot TO lwa_order_comm.
  MOVE-CORRESPONDING gs_csdoc_infocus-x-cshdr TO lwa_order_comm.
  lwa_order_comm-matnr = gs_csdoc_infocus-x-cshdr-ymatnr.
  lwa_order_comm-gstrp = gs_csdoc_infocus-x-cshdr-datab.
  lwa_order_comm-gltrp = gs_csdoc_infocus-x-cshdr-datbi.
  lwa_order_comm-gamng = gs_csdoc_infocus-x-cshdr-eston.
  lwa_order_comm-gmein = gs_csdoc_infocus-x-cshdr-esuom.

  CALL METHOD /agri/cl_glco_process=>order_create
    EXPORTING
      i_commit_work = space
      is_order_comm = lwa_order_comm
    IMPORTING
      e_aufnr       = lv_aufnr
      et_messages   = lt_messtab.

  IF lv_aufnr IS INITIAL.
    lv_subrc = 4.
    LOOP AT lt_messtab INTO lwa_messtab
                      WHERE msgtyp EQ zcl_abs_abap_maintain=>c_msgty_error.
      MESSAGE ID lwa_messtab-msgid TYPE lwa_messtab-msgtyp
       NUMBER lwa_messtab-msgnr WITH lwa_messtab-msgv1
              lwa_messtab-msgv2 lwa_messtab-msgv3 lwa_messtab-msgv4
        INTO lwa_msg_log-msgtxt.
      lwa_msg_log-dmatnr = lwa_order_comm-matnr.
      lwa_msg_log-nustn  = lwa_order_comm-tplnr_fl.
      lwa_msg_log-status = 1.
    ENDLOOP.
    EXIT.
  ELSE.
    gs_csdoc_infocus-x-cshdr-yaufnr = lv_aufnr.
    IF gs_csdoc_infocus-x-cshdr-updkz NE c_updkz_new.
      gs_csdoc_infocus-x-cshdr-updkz = c_updkz_update.
    ENDIF.
    MESSAGE s012(zabs_msgcls) WITH lv_aufnr INTO lwa_msg_log-msgtxt.
    lwa_msg_log-status = 3.
    lwa_msg_log-dmatnr = lwa_order_comm-matnr.
    lwa_msg_log-nustn  = lwa_order_comm-tplnr_fl.
  ENDIF.

ENDFORM.            " CREATE_YARD_ORDER

*&---------------------------------------------------------------------*
*& Form FCODE_BINS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM fcode_bins.

  DATA: lwa_disp_ord_fcat TYPE zabs_str_dispord_fcat,
        lv_lines          TYPE sy-tabix.

  lwa_disp_ord_fcat-nustn = gs_csdoc_infocus-tplnr_fl.
  lwa_disp_ord_fcat-contr = gs_csdoc_infocus-contr.
  lwa_disp_ord_fcat-nustn_txt = gs_variables-nustn_txt.
  DESCRIBE TABLE gt_disp_ord_fcat LINES lv_lines.
  APPEND lwa_disp_ord_fcat TO gt_disp_ord_fcat.

  READ TABLE gt_disp_ord_fcat ASSIGNING FIELD-SYMBOL(<lwa_disord>) INDEX lv_lines.
  IF sy-subrc = 0.
    PERFORM disp_ord_styles_prepare CHANGING <lwa_disord>.
  ENDIF.

  CALL METHOD ref_grid_disp_ord->tables_display_refresh
    CHANGING
      it_outtab = gt_disp_ord_fcat.

  gs_variables-refresh_disp_ord = abap_true.

ENDFORM.            "fcode_bins

*&---------------------------------------------------------------------*
*& Form FCODE_DBEL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM fcode_bdel.

  DATA: lwa_row TYPE lvc_s_row,
        lt_rows TYPE lvc_t_row.

  DATA lwa_disp_ord  TYPE  zabs_tab_dispord.

  CALL METHOD ref_grid_disp_ord->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.

  IF lt_rows IS INITIAL.
    MESSAGE e003(zabs_msgcls) DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_info.
  ENDIF.

  LOOP AT lt_rows INTO lwa_row.
    READ TABLE gt_disp_ord_fcat ASSIGNING FIELD-SYMBOL(<fs_disp_ord_fcat>)
          INDEX lwa_row-index.
    IF sy-subrc = 0 AND <fs_disp_ord_fcat>-disord IS INITIAL.
      MOVE-CORRESPONDING <fs_disp_ord_fcat> TO lwa_disp_ord .
      CLEAR <fs_disp_ord_fcat>-nustn.
      DELETE zabs_tab_dispord FROM lwa_disp_ord.
    ELSEIF <fs_disp_ord_fcat>-disord IS NOT INITIAL.
      MESSAGE e013(zabs_msgcls) DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_info.
    ENDIF.
  ENDLOOP.

  DELETE gt_disp_ord_fcat WHERE nustn IS INITIAL.

  CALL METHOD ref_grid_disp_ord->tables_display_refresh
    CHANGING
      it_outtab = gt_disp_ord_fcat.

ENDFORM.            "fcode_bdel

*&---------------------------------------------------------------------*
*& Form DISP_ORD_STYLES_PREPARE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM disp_ord_styles_prepare  CHANGING lwa_fpdisp_ord TYPE zabs_str_dispord_fcat.

  DATA: lwa_style TYPE lvc_s_styl,
        lwa_edit  TYPE lvc_s_styl.

  REFRESH: lwa_fpdisp_ord-styles.

*  lwa_style-fieldname = 'NUSTN'.
*  lwa_style-style = cl_gui_alv_grid=>mc_style_disabled.
*  INSERT lwa_style INTO TABLE lwa_fpdisp_ord-styles.
*
*  lwa_style-fieldname = 'CONTR'.
*  lwa_style-style = cl_gui_alv_grid=>mc_style_disabled.
*  INSERT lwa_style INTO TABLE lwa_fpdisp_ord-styles.

  IF lwa_fpdisp_ord-disord IS NOT INITIAL.

    lwa_style-fieldname = 'DMATNR'.
    lwa_style-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT lwa_style INTO TABLE lwa_fpdisp_ord-styles.

    lwa_style-fieldname = 'ESTON'.
    lwa_style-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT lwa_style INTO TABLE lwa_fpdisp_ord-styles.

    lwa_style-fieldname = 'ESUOM'.
    lwa_style-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT lwa_style INTO TABLE lwa_fpdisp_ord-styles.

    lwa_style-fieldname = 'NWERKS'.
    lwa_style-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT lwa_style INTO TABLE lwa_fpdisp_ord-styles.

    lwa_style-fieldname = 'DWERKS'.
    lwa_style-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT lwa_style INTO TABLE lwa_fpdisp_ord-styles.

  ELSEIF lwa_fpdisp_ord-dmatnr IS NOT INITIAL.

    lwa_style-fieldname = 'DMATNR'.
    lwa_style-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT lwa_style INTO TABLE lwa_fpdisp_ord-styles.

  ENDIF.

ENDFORM.            "DISP_ORD_STYLES_PREPARE

*&---------------------------------------------------------------------*
*& Form CONTROL_EVENTS_REGISTER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM control_events_register .

  IF ref_event_handler IS INITIAL.
    CREATE OBJECT ref_event_handler.
  ENDIF.

  IF ref_grid_disp_ord IS NOT INITIAL.
    CALL METHOD ref_grid_disp_ord->register_edit_event
      EXPORTING
        i_event_id = /agri/cl_gui_alv_grid=>mc_evt_enter
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.
  ENDIF.

  SET HANDLER: ref_event_handler->on_toolbar_grid
               FOR ref_grid_disp_ord,
               ref_event_handler->on_user_command_grid
               FOR ref_grid_disp_ord,
               ref_event_handler->on_data_changed_grid
               FOR ref_grid_disp_ord.

ENDFORM.            "CONTROL_EVENTS_REGISTER_0406

*&---------------------------------------------------------------------*
*& Form FIELD_CATALOG_PREPARE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM field_catalog_prepare CHANGING lt_fcat TYPE lvc_t_fcat.

  FIELD-SYMBOLS : <lwa_fcat> TYPE lvc_s_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = c_str_dispord_fcat  "'ZABS_STR_DISPORD_FCAT'
    CHANGING
      ct_fieldcat            = lt_fcat
    EXCEPTIONS ##FM_SUBRC_OK
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF gs_variables-display_mode IS INITIAL.
    LOOP AT lt_fcat ASSIGNING <lwa_fcat>.
      CASE <lwa_fcat>-fieldname.
        WHEN 'DMATNR' OR 'ESTON' OR 'ESUOM'
          OR 'MSEHI' OR 'NWERKS' OR 'DWERKS'.
          <lwa_fcat>-edit = abap_true.
      ENDCASE.
    ENDLOOP.
  ENDIF.

  READ TABLE lt_fcat ASSIGNING <lwa_fcat>
    WITH KEY fieldname = 'CONTR'.
  IF sy-subrc = 0.
    <lwa_fcat>-tech = abap_true.
  ENDIF.

ENDFORM.            "FIELD_CATALOG_PREPEARE

*&---------------------------------------------------------------------*
*& Form TOOLBAR_BUTTONS_EXCLUDE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM toolbar_buttons_exclude  TABLES lt_toolbar_excludes TYPE ui_functions.

  APPEND /agri/cl_gui_alv_grid=>mc_fc_info      TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_graph     TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_print     TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_views     TO lt_toolbar_excludes.

  APPEND /agri/cl_gui_alv_grid=>mc_fc_print     TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_views     TO lt_toolbar_excludes.
****Editable Grid Buttons
  APPEND /agri/cl_gui_alv_grid=>mc_fc_info      TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_graph     TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_print     TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_views     TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_check     TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_refresh   TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_mb_paste     TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_append_row
                                               TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_copy  TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_copy_row
                                               TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_cut   TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_delete_row
                                               TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_insert_row
                                               TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_move_row
                                               TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_paste TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_paste_new_row
                                               TO lt_toolbar_excludes.
  APPEND /agri/cl_gui_alv_grid=>mc_fc_loc_undo  TO lt_toolbar_excludes.

ENDFORM.            "TOOLBAR_BUTTONS_EXCLUDE

*----------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD: on_toolbar_grid.

    DATA : lwa_button TYPE stb_button.

    IF gs_variables-display_mode IS INITIAL.
      toolbar_button_insert e_object->mt_toolbar lwa_button space
                            c_fcode-delete_disp_ord icon_delete_row
                            TEXT-011 space.
      toolbar_button_insert e_object->mt_toolbar lwa_button space
                            c_fcode-insert_disp_ord icon_insert_row
                            TEXT-010 space.
      toolbar_button_insert e_object->mt_toolbar lwa_button space
                            c_fcode-create_cat icon_create
                            TEXT-013 space.
    ENDIF.

  ENDMETHOD.                    "on_toolbar_grid

  METHOD on_user_command_grid.

    IF e_ucomm EQ c_fcode-change_colum_disp_ord.
      IF gs_variables-refresh_colum_disp_ord EQ abap_false.
        gs_variables-refresh_colum_disp_ord = abap_true.
      ELSE.
        CLEAR: gs_variables-refresh_colum_disp_ord.
      ENDIF.
      gs_variables-refresh_disp_ord = abap_true.
    ENDIF.

    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = e_ucomm.

  ENDMETHOD.                    "on_user_command
  METHOD on_data_changed_grid.

    IF NOT er_data_changed->mt_mod_cells[] IS INITIAL.
      PERFORM handle_data_changed USING er_data_changed.
    ENDIF.

    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = c_fcode-enter.

  ENDMETHOD.                    "on_data_changed_grid
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*&---------------------------------------------------------------------*
*& Form HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM handle_data_changed USING p_er_data_changed
                         TYPE REF TO cl_alv_changed_data_protocol.

  DATA: lt_imod      TYPE lvc_t_modi,
        lwa_mod_cell TYPE lvc_s_modi,
        lv_dmatnr    TYPE matnr,
        lv_werks     TYPE werks_d.

  FIELD-SYMBOLS: <fs_disp_ord> TYPE zabs_str_dispord_fcat.

  APPEND LINES OF p_er_data_changed->mt_mod_cells[] TO lt_imod.
  SORT lt_imod BY row_id.
  DELETE ADJACENT DUPLICATES FROM lt_imod COMPARING row_id.
  READ TABLE lt_imod INTO DATA(ls_imod) INDEX 1.
  IF sy-subrc NE 0 AND ls_imod-value IS INITIAL.
    RETURN.
  ENDIF.

  CASE ls_imod-fieldname.

    WHEN 'DMATNR'.
      lv_dmatnr = ls_imod-value.
      SELECT SINGLE matnr
        FROM mara
        INTO @DATA(lv_matnr)
        WHERE matnr = @lv_dmatnr.
      IF sy-subrc NE 0.
        MESSAGE i005(zabs_msgcls)
        DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_error.
        RETURN.
      ENDIF.

      SELECT SINGLE maktx
        FROM makt
        INTO @DATA(lv_maktx)
        WHERE matnr = @lv_dmatnr
        AND   spras = @sy-langu.
      IF sy-subrc NE 0.
        RETURN.
      ENDIF.
      READ TABLE gt_disp_ord_fcat ASSIGNING <fs_disp_ord>
                                      INDEX ls_imod-row_id.
      IF sy-subrc = 0.
        <fs_disp_ord>-maktx = lv_maktx.
      ELSE.
        CLEAR <fs_disp_ord>-maktx.
      ENDIF.

    WHEN 'NWERKS'.
      lv_werks = ls_imod-value.
      SELECT SINGLE name1
        FROM t001w
        INTO @DATA(lv_nwerks)
        WHERE werks = @lv_werks.
      IF sy-subrc NE 0.
        RETURN.
      ENDIF.
      READ TABLE gt_disp_ord_fcat ASSIGNING <fs_disp_ord>
                                      INDEX ls_imod-row_id.
      IF sy-subrc = 0.
        <fs_disp_ord>-name1 = lv_nwerks.
      ELSE.
        CLEAR <fs_disp_ord>-name1.
      ENDIF.

    WHEN 'DWERKS'.
      lv_werks = ls_imod-value.
      SELECT SINGLE name1
        FROM t001w
        INTO @DATA(lv_dwerks)
        WHERE werks = @lv_werks.
      IF sy-subrc NE 0.
        RETURN.
      ENDIF.
      READ TABLE gt_disp_ord_fcat ASSIGNING <fs_disp_ord>
                                      INDEX ls_imod-row_id.
      IF sy-subrc = 0.
        <fs_disp_ord>-name2 = lv_dwerks.
      ELSE.
        CLEAR <fs_disp_ord>-name2.
      ENDIF.
    WHEN OTHERS.

  ENDCASE.

ENDFORM.            "HANDLE_DATA_CHANGED

*&---------------------------------------------------------------------*
*& Form DISP_ORD_DB_UPDATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM disp_ord_db_update USING lt_tab_dispord TYPE tty_tab_dispord.

  MODIFY zabs_tab_dispord FROM TABLE lt_tab_dispord.

ENDFORM.            "DISP_ORD_DB_UPDATE

*&---------------------------------------------------------------------*
*& Form DISPLAY_DISPATCH_ORDERS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM display_dispatch_orders  CHANGING gt_disp_ord_fcat TYPE zabs_tty_dispord_fcat.

  SELECT * FROM zabs_tab_dispord
    INTO TABLE @DATA(lt_disp_ord)
    WHERE nustn = @gs_csdoc_infocus-tplnr_fl
    AND   contr = @gs_csdoc_infocus-contr.
  IF sy-subrc = 0.
    MOVE-CORRESPONDING lt_disp_ord TO gt_disp_ord_fcat.
  ELSEIF gs_variables-display_mode IS NOT INITIAL.
    MESSAGE e009(zabs_msgcls) DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_info.
  ENDIF.

  LOOP AT gt_disp_ord_fcat[] ASSIGNING FIELD-SYMBOL(<lwa_disord>).
    PERFORM disp_ord_styles_prepare CHANGING <lwa_disord>.
  ENDLOOP.

ENDFORM.            "DISPLAY_DISPATCH_ORDERS

*&---------------------------------------------------------------------*
*& Module  EXIT_PROCESSING INPUT
*&---------------------------------------------------------------------*
* Exit Processing
*----------------------------------------------------------------------*
MODULE exit_processing INPUT.
  PERFORM exit_processing.
ENDMODULE.            "EXIT_PROCESSING INPUT

*&---------------------------------------------------------------------*
*& Form EXIT_PROCESSING
*&---------------------------------------------------------------------*
*& Exit Processing
*&---------------------------------------------------------------------*
FORM exit_processing.

  DATA: lv_answer.

  CASE sy-dynnr.
    WHEN c_screen-create_batch.
      CLEAR: ok_code,
             zabs_str_batch-charg,
             zabs_str_batch-erfmg,
             zabs_str_batch-meins,
             zabs_str_batch-date.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN OTHERS.
      SET SCREEN 0.
      LEAVE SCREEN.
  ENDCASE.

ENDFORM.            "EXIT_PROCESSING

*&---------------------------------------------------------------------*
*& Module  CHECK_BATCH INPUT
*&---------------------------------------------------------------------*
* Check Batch
*----------------------------------------------------------------------*
MODULE check_batch INPUT.
  PERFORM check_batch.
ENDMODULE.            "CHECK_BATCH INPUT

*&---------------------------------------------------------------------*
*& Form CHECK_BATCH
*&---------------------------------------------------------------------*
*& Check Batch
*&---------------------------------------------------------------------*
FORM check_batch.

*--Local Variable
  DATA: lv_charg TYPE charg_d.

  CHECK zabs_str_batch-charg IS NOT INITIAL.

*--Fetching Batch data
  SELECT SINGLE charg
           FROM mch1
           INTO lv_charg
          WHERE matnr EQ zabs_str_batch-matnr
            AND charg EQ zabs_str_batch-charg.
  IF sy-subrc EQ 0.
    MESSAGE e002(zabs_msgcls) WITH zabs_str_batch-charg
                             INTO sy-msgli.   "Batch &1 Already Exists
    message_simple space.
    EXIT.
  ENDIF.

ENDFORM.            "CHECK_BATCH

*&---------------------------------------------------------------------*
*& Form MESSAGES_DISPLAY
*&---------------------------------------------------------------------*
*& messages_display
*&---------------------------------------------------------------------*
FORM messages_display  USING lt_msg_log.

  DATA: lr_table   TYPE REF TO cl_salv_table,
        lr_columns TYPE REF TO cl_salv_columns_table,
        lr_column  TYPE REF TO cl_salv_column_table.

  cl_salv_table=>factory(
       IMPORTING
         r_salv_table = lr_table
       CHANGING
         t_table      = lt_msg_log ).

  lr_columns = lr_table->get_columns( ).
  lr_columns->set_exception_column( value = 'STATUS' ).
  lr_columns->set_optimize('X').
*  TRY.
*      lr_column ?= lr_columns->get_column( 'TYP' ).
*      lr_column->set_medium_text( 'Status' ).
*      lr_column->set_long_text( 'Status' ).
*      lr_column->set_short_text( 'Status' ).
*    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
*  ENDTRY.
*
*
*  TRY.
*      lr_column ?= lr_columns->get_column( 'MSG' ).
*      lr_column->set_medium_text( 'Message Text' ).
*      lr_column->set_long_text( 'Message Text' ).
*      lr_column->set_short_text( 'TEXT' ).
*    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
*  ENDTRY.
  lr_table->set_screen_popup(
    start_column = 23
    end_column   = 100
    start_line   = 6
    end_line     = 10 ).

  lr_table->display( ).

ENDFORM.            "MESSAGES_DISPLAY

*&---------------------------------------------------------------------*
*& Form NUMBER_RANGE
*&---------------------------------------------------------------------*
*& Building number range using variant table
*&---------------------------------------------------------------------*
FORM number_range CHANGING  ls_var TYPE ty_var.

*--Local Declarations
  DATA: lt_constants TYPE zabs_tty_vkey_const,
        ls_constants TYPE zabs_str_vkey_const,
        lv_nrlevel   TYPE nrlevel,
        lv_charg     TYPE charg_d.

*--Get variant table data
  CALL METHOD zcl_abs_get_variants=>get_constant_multiple
    EXPORTING
      iv_mod       = space
      iv_objid     = zcl_abs_abap_maintain=>c_objid_nursery_wb   "'NSWB'
      iv_k1val     = zcl_abs_abap_maintain=>c_key_batch_nr   "'BNOR'
    IMPORTING
      et_constants = lt_constants.

  CLEAR ls_constants.
*--Getting constant value table record based on flag x
  READ TABLE lt_constants INTO ls_constants
    WITH KEY cnval3 = abap_true. "'X'

  IF sy-subrc = 0.
    ls_var-ctext   = ls_constants-cnval2.   "'FAZ'
    ls_var-contr   = ls_constants-cnval1.   "'01'

*--Fetching Number range status from Number Range Intervals Table
    SELECT SINGLE nrlevel
      FROM nriv
      INTO lv_nrlevel
     WHERE object    = zcl_abs_abap_maintain=>c_obj_btch_nr  "'ZABS_BATCH'
       AND nrrangenr = ls_var-contr.                     "'01'

    ls_var-next_no = lv_nrlevel.
    CONCATENATE ls_var-ctext ls_var-next_no INTO lv_charg.

    SELECT charg
      FROM mch1 UP TO 1 ROWS
      INTO @DATA(lv_charg_x)
     WHERE charg EQ @lv_charg.
    ENDSELECT.

*--Wrong number. The MCH1 batch and number range are not synchronized.
    IF sy-subrc EQ 0.
*--Calling Number Range FM to assign next free number
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr = ls_var-contr  "'01'
          object      = zcl_abs_abap_maintain=>c_obj_btch_nr. "'ZABS_BATCH'.

      ls_var-next_no = lv_nrlevel + 1.

      IF zabs_str_batch-matnr IS NOT INITIAL.
        CONCATENATE ls_var-ctext ls_var-next_no INTO lv_charg.

        DO.
*--Fetching Batch data
          SELECT charg
            FROM mch1 UP TO 1 ROWS
            INTO @lv_charg_x
           WHERE charg EQ @lv_charg.
          ENDSELECT.

          IF sy-subrc NE 0.
            EXIT.
          ELSE.
*--Calling Number Range FM to assign next free number
            CALL FUNCTION 'NUMBER_GET_NEXT'
              EXPORTING
                nr_range_nr = ls_var-contr  "'01'
                object      = zcl_abs_abap_maintain=>c_obj_btch_nr. "'ZABS_BATCH'.
            ls_var-next_no = ls_var-next_no + 1.
            CONCATENATE ls_var-ctext ls_var-next_no INTO lv_charg.
          ENDIF.
        ENDDO.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.            "NUMBER_RANGE
