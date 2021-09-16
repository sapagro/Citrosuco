************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name       :  ZABS_REP_TO_MONITOR                             *
* Tcode             :  ZABS_TRN_TOMON                                  *
* Include Name      :  ZABS_REP_TO_MONITOR_SUB                         *
* Created By        :  Jetendra Mantena                                *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  07.31.2019                                      *
* TR                :  C4DK901784                                      *
* Version           :  001                                             *
* Description       :  Task Order Monitor Data Preperation and Display *
*                      Data.                                           *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*&---------------------------------------------------------------------*
*& Form DATE_VALIDATIONS
*&---------------------------------------------------------------------*
*& Selection Screen Validations
*&---------------------------------------------------------------------*
FORM selection_validations.

*--Plant Validation
  SELECT SINGLE werks
    FROM t001w
    INTO p_werks
    WHERE werks = p_werks.
  IF sy-subrc <> 0.
    MESSAGE e033(zabs_msgcls).
  ENDIF.

*--Crop Validation
  IF so_cmnum IS NOT INITIAL.
    SELECT SINGLE cmnum
      FROM /agri/glcmhdr
      INTO so_cmnum-low
      WHERE cmnum EQ so_cmnum-low.
    IF sy-subrc <> 0.
      MESSAGE e034(zabs_msgcls).
    ENDIF.
  ENDIF.

*--Date lower limit initial check
  IF so_datum-low IS INITIAL.
    MESSAGE e031(zabs_msgcls).
  ENDIF.

*--Date higher limit initial check
  IF so_datum-high IS INITIAL.
    MESSAGE e032(zabs_msgcls).
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form INITIALIZE_GLOBAL_DATA
*&---------------------------------------------------------------------*
*& Initializing global data
*&---------------------------------------------------------------------*
FORM initialize_global_data.
  REFRESH gt_to_monitor.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form BUILD_TASK_ORDER_DATA
*&---------------------------------------------------------------------*
*& Preparing Task Order Monitor Data
*&---------------------------------------------------------------------*
FORM build_task_order_data.

*--Local structure declarations
  DATA: ls_to_monitor TYPE zabs_str_to_monitor_fcat.

*--Local table declarations
  DATA: lt_dd07v      TYPE TABLE OF dd07v.

*--Local variable declarations
  DATA: lv_domvalue   TYPE domvalue_l,
        lv_duration   TYPE i,
        lv_time_elpsd TYPE i.

*--Fetch data from farm process header
  SELECT aufnr,
         autyp,
         tplnr_fl,
         cmnum,
         class,
         matnr,
         gamng,
         gwemg,
         gmein,
         gstrp,
         gltrp
  FROM /agri/fmfphdr
  INTO TABLE @DATA(lt_fmfphdr)
  WHERE matnr     IN @so_matnr
    AND tplnr_fl  IN @so_tplnr
    AND cmnum     IN @so_cmnum
    AND ( ( gstrp GE @so_datum-low
    AND gstrp     LE @so_datum-high )
    OR  ( gltrp   GE @so_datum-low
    AND gltrp     LE @so_datum-high ) )
    AND iwerk     EQ @p_werks
    AND tecom     EQ @space.

  IF sy-subrc NE 0.
    MESSAGE i030(zabs_msgcls).
    LEAVE LIST-PROCESSING.
  ENDIF.

*--Get terrain description
  DATA(lt_tplnr_fl) = lt_fmfphdr.
  SORT lt_tplnr_fl BY tplnr_fl.
  DELETE ADJACENT DUPLICATES FROM lt_tplnr_fl COMPARING tplnr_fl.
  IF lt_tplnr_fl IS NOT INITIAL.
    SELECT tplnr_fl,
           pltxt
      FROM /agri/glflot
      INTO TABLE @DATA(lt_pltxt)
      FOR ALL ENTRIES IN @lt_tplnr_fl
      WHERE tplnr_fl EQ @lt_tplnr_fl-tplnr_fl.
    IF sy-subrc = 0.
      SORT lt_pltxt BY tplnr_fl.
    ENDIF.
  ENDIF.

*--Get document category description
  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = c_domname_doc_cat
    TABLES
      values_tab      = lt_dd07v
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.
  IF sy-subrc = 0.
    SORT lt_dd07v BY domvalue_l.
  ENDIF.

*--Get task material description
  DATA(lt_matnr) = lt_fmfphdr.
  SORT lt_matnr BY matnr.
  DELETE ADJACENT DUPLICATES FROM lt_matnr COMPARING matnr.
  IF lt_matnr IS NOT INITIAL.
    SELECT matnr,
           maktx
      FROM makt
      INTO TABLE @DATA(lt_makt)
      FOR ALL ENTRIES IN @lt_matnr
      WHERE matnr EQ @lt_matnr-matnr
        AND spras EQ @sy-langu.
    IF sy-subrc = 0.
      SORT lt_makt BY matnr.
    ENDIF.
  ENDIF.

*--Get application text
  SELECT class,
         descr
    FROM /agri/tabclst
    INTO TABLE @DATA(lt_apltxt)
    WHERE spras EQ @sy-langu.
  IF sy-subrc = 0.
    SORT lt_apltxt BY class.
  ENDIF.

*--Get order confirmation
  SELECT ocnum
    FROM /agri/fmocindx
    INTO TABLE @DATA(lt_ocnum)
    FOR ALL ENTRIES IN @lt_fmfphdr
    WHERE aufnr EQ @lt_fmfphdr-aufnr.
  IF lt_ocnum IS NOT INITIAL.
*--Get operations based on confirmation
    SELECT ocnum,
           rueck,
           rmzhl
      FROM /agri/fmocopr
      INTO TABLE @DATA(lt_ocopr)
      FOR ALL ENTRIES IN @lt_ocnum
      WHERE ocnum EQ @lt_ocnum-ocnum.

    IF lt_ocopr IS NOT INITIAL.
*--Get posted goods data
      SELECT *
        FROM afwi
        INTO TABLE @DATA(lt_afwi)
        FOR ALL ENTRIES IN @lt_ocopr
        WHERE rueck EQ @lt_ocopr-rueck
          AND rmzhl EQ @lt_ocopr-rmzhl.
      IF sy-subrc EQ 0.
*--Get posting date
        SELECT budat,
               cpudt,
               cputm,
               aufnr
          FROM matdoc
          INTO TABLE @DATA(lt_matdoc)
          FOR ALL ENTRIES IN @lt_afwi
          WHERE mblnr     EQ @lt_afwi-mblnr
            AND mjahr     EQ @lt_afwi-mjahr
            AND zeile     EQ @lt_afwi-mblpo
            AND cancelled EQ @space
            AND bwart     EQ @zcl_abs_abap_maintain=>c_move_type_grn_create.
        IF sy-subrc EQ 0.
          SORT lt_matdoc BY aufnr cpudt DESCENDING cputm DESCENDING.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

*--Processing farm process data to build final data.
  LOOP AT lt_fmfphdr INTO DATA(ls_fmfphdr).

    ls_to_monitor-tplnr_fl = ls_fmfphdr-tplnr_fl. "Terrain
    ls_to_monitor-cmnum    = ls_fmfphdr-cmnum.    "Crop
    ls_to_monitor-matnr    = ls_fmfphdr-matnr.    "Process Material
    ls_to_monitor-aufnr    = ls_fmfphdr-aufnr.    "Order
    ls_to_monitor-autyp    = ls_fmfphdr-autyp.    "Doc Category
    ls_to_monitor-class    = ls_fmfphdr-class.    "Application
    ls_to_monitor-gstrp    = ls_fmfphdr-gstrp.    "Basic Start Date
    ls_to_monitor-gltrp    = ls_fmfphdr-gltrp.    "Basic End Date
    ls_to_monitor-gamng    = ls_fmfphdr-gamng.    "Total Quantity
    ls_to_monitor-gmein    = ls_fmfphdr-gmein.    "UOM
    ls_to_monitor-gwemg    = ls_fmfphdr-gwemg.    "Delivered Quantity

*--Posting Date fill
    READ TABLE lt_matdoc INTO DATA(ls_matdoc)
      WITH KEY aufnr = ls_fmfphdr-aufnr
      BINARY SEARCH.
    IF sy-subrc = 0.
      ls_to_monitor-lp_date = ls_matdoc-budat.
    ENDIF.

*--Filling terrain description
    READ TABLE lt_pltxt INTO DATA(ls_pltxt)
      WITH KEY tplnr_fl = ls_fmfphdr-tplnr_fl
      BINARY SEARCH.
    IF sy-subrc = 0.
      ls_to_monitor-pltxt = ls_pltxt-pltxt.
    ENDIF.

*--Filling description for document category
    lv_domvalue = ls_fmfphdr-autyp.
    READ TABLE lt_dd07v INTO DATA(ls_dd07v)
      WITH KEY domvalue_l = lv_domvalue
      BINARY SEARCH.
    IF sy-subrc = 0.
      ls_to_monitor-autyp_desc = ls_dd07v-ddtext.
    ENDIF.

*--Filling description for process material
    READ TABLE lt_makt INTO DATA(ls_makt)
      WITH KEY matnr = ls_fmfphdr-matnr
      BINARY SEARCH.
    IF sy-subrc = 0.
      ls_to_monitor-maktx = ls_makt-maktx.
    ENDIF.

*--Filling Description for application
    READ TABLE lt_apltxt INTO DATA(ls_apltxt)
      WITH KEY class = ls_fmfphdr-class
      BINARY SEARCH.
    IF sy-subrc = 0.
      ls_to_monitor-class_desc = ls_apltxt-descr.
    ENDIF.

*--Calculating Duration
    CLEAR lv_duration.
    PERFORM calculation_for_duration USING ls_fmfphdr-gstrp
                                           ls_fmfphdr-gltrp
                                  CHANGING lv_duration.

    ls_to_monitor-duration  = lv_duration.
    ls_to_monitor-plan_qty  = ls_fmfphdr-gamng / lv_duration.
    ls_to_monitor-curr_date = sy-datum.

*--Calculating elapsed time from start date to current date.
    CLEAR lv_time_elpsd.
    PERFORM calculation_for_duration USING ls_fmfphdr-gstrp
                                           sy-datum
                                  CHANGING lv_time_elpsd.
    ls_to_monitor-time_elpsd = lv_time_elpsd.

    ls_to_monitor-telpsd_per = ls_to_monitor-time_elpsd * 100 / lv_duration.

    IF ls_to_monitor-time_elpsd GT ls_to_monitor-duration.
      ls_to_monitor-act_qty    = ls_fmfphdr-gwemg / ls_to_monitor-duration.
    ELSE.
      ls_to_monitor-act_qty    = ls_fmfphdr-gwemg / ls_to_monitor-time_elpsd.
    ENDIF.

    ls_to_monitor-advance  = ls_fmfphdr-gwemg * 100 / ls_fmfphdr-gamng.

    APPEND ls_to_monitor TO gt_to_monitor.
    CLEAR :ls_to_monitor,ls_matdoc,ls_pltxt,ls_dd07v,ls_makt,ls_apltxt.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CALCULATION_FOR_DURATION
*&---------------------------------------------------------------------*
*& Calculating number of days
*&---------------------------------------------------------------------*
FORM calculation_for_duration  USING    ps_start_date TYPE d
                                        ps_end_date   TYPE d
                               CHANGING pv_days       TYPE i.

*--Local data declaration
  DATA: lt_holidays TYPE TABLE OF iscal_day.

*--Calling Function to get holidays betwwn date range
  CALL FUNCTION 'HOLIDAY_GET'
    EXPORTING
      holiday_calendar           = zcl_abs_abap_maintain=>c_holi_cal_brazil
      factory_calendar           = zcl_abs_abap_maintain=>c_fact_cal_brazil
      date_from                  = ps_start_date
      date_to                    = ps_end_date
    TABLES
      holidays                   = lt_holidays
    EXCEPTIONS
      factory_calendar_not_found = 1
      holiday_calendar_not_found = 2
      date_has_invalid_format    = 3
      date_inconsistency         = 4
      OTHERS                     = 5.
  IF sy-subrc = 0.
*--Collecting number of holidays
    DESCRIBE TABLE lt_holidays LINES pv_days.
    pv_days = ( ps_end_date - ps_start_date ) - pv_days + 1.
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
*& text
*&---------------------------------------------------------------------*
FORM status_set.
  SET PF-STATUS 'S100'.
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
*& Title Bar
*&---------------------------------------------------------------------*
FORM title_set .
  SET TITLEBAR 'T100'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Module CONTROLS_DISPLAY OUTPUT
*&---------------------------------------------------------------------*
*& Display Controls
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

*--Local data declaration
  DATA : lt_toolbar_excludes TYPE ui_functions,
         ls_environment      TYPE /agri/s_glvc_environment,
         ls_layout           TYPE lvc_s_layo,
         lt_fcat             TYPE lvc_t_fcat,
         ls_variant          TYPE disvariant.

  IF ref_container_task_ord IS NOT INITIAL.

    PERFORM field_catalog_prepare CHANGING lt_fcat.

    ref_grid_task_ord->set_frontend_fieldcatalog( it_fieldcatalog = lt_fcat ).

    ls_layout-cwidth_opt = abap_true.
    ls_layout-sel_mode   = zcl_abs_abap_maintain=>c_sel_mode_cell.
    ls_layout-smalltitle = abap_true.

    ref_grid_task_ord->set_frontend_layout( is_layout = ls_layout ).

    ref_grid_task_ord->refresh_table_display( ).

  ELSE.

    CREATE OBJECT ref_container_task_ord
      EXPORTING
        container_name              = c_to_monitor_0100_cc
        repid                       = sy-repid
        dynnr                       = sy-dynnr
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

*--Creation of Grid
    ls_environment-switchoff_performance = abap_true.
    CREATE OBJECT ref_grid_task_ord
      EXPORTING
        i_parent           = ref_container_task_ord
        is_lvc_environment = ls_environment
      EXCEPTIONS
        error_cntl_create  = 1
        error_cntl_init    = 2
        error_cntl_link    = 3
        error_dp_create    = 4
        OTHERS             = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

*--Preparing Field Catalog
    PERFORM field_catalog_prepare CHANGING lt_fcat.

    ls_layout-cwidth_opt = abap_true.
    ls_layout-sel_mode   = zcl_abs_abap_maintain=>c_sel_mode_cell.
    ls_layout-smalltitle = abap_true.

    ls_variant-report = sy-repid.
    ls_variant-handle = sy-dynnr.

*--Exclude unwanted buttons
    PERFORM toolbar_buttons_exclude TABLES lt_toolbar_excludes.

*--Display Data
    CALL METHOD ref_grid_task_ord->set_table_for_first_display
      EXPORTING
        is_variant                    = ls_variant
        i_save                        = zcl_abs_abap_maintain=>c_mode_display
        is_layout                     = ls_layout
        it_toolbar_excluding          = lt_toolbar_excludes[]
      CHANGING
        it_outtab                     = gt_to_monitor
        it_fieldcatalog               = lt_fcat[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
      MESSAGE i035(zabs_msgcls).
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FIELD_CATALOG_PREPARE
*&---------------------------------------------------------------------*
*& Preparing Field Catalog
*&---------------------------------------------------------------------*
FORM field_catalog_prepare  CHANGING lt_fcat TYPE lvc_t_fcat.

*--Preparing field catalog
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = zcl_abs_abap_maintain=>c_to_monitor_fcat
    CHANGING
      ct_fieldcat            = lt_fcat
    EXCEPTIONS ##FM_SUBRC_OK
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form TOOLBAR_BUTTONS_EXCLUDE
*&---------------------------------------------------------------------*
*& Excluding Toolbar Buttons
*&---------------------------------------------------------------------*
FORM toolbar_buttons_exclude TABLES lt_toolbar_excludes TYPE ui_functions.

****Editable Grid Buttons
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

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_TASK_ORDER_DATA
*&---------------------------------------------------------------------*
*& Calling Screen for Display
*&---------------------------------------------------------------------*
FORM display_task_order_data.

  IF gt_to_monitor IS NOT INITIAL.
    CALL SCREEN 0100.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  FCODE_PROCESSING  INPUT
*&---------------------------------------------------------------------*
*       Fcode Processing
*----------------------------------------------------------------------*
MODULE fcode_processing INPUT.
  PERFORM fcode_processing.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form FCODE_PROCESSING
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM fcode_processing.

  DATA : lv_subroutine(40) TYPE c VALUE 'FCODE_'.

  fcode = ok_code.
  CLEAR ok_code.
  CONCATENATE lv_subroutine fcode INTO lv_subroutine.
  PERFORM (lv_subroutine) IN PROGRAM (sy-repid)
                          IF FOUND.

ENDFORM.            "FCODE_PROCESSING

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
*& Form FCODE_BACK
*&---------------------------------------------------------------------*
*& For Cancel Button
*&---------------------------------------------------------------------*
FORM fcode_canc.
  SET SCREEN 0.
  LEAVE SCREEN.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form FCODE_BACK
*&---------------------------------------------------------------------*
*& For Exit Button
*&---------------------------------------------------------------------*
FORM fcode_exit.
  SET SCREEN 0.
  LEAVE SCREEN.
ENDFORM.
