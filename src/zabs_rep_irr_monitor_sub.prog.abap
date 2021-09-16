************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name       :  ZABS_REP_IRRI_MONITOR_SUB                       *
* Tcode             :  ZABS_TRN_IRRMON                                 *
* Created By        :  Chandrakanth Karanam                            *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  01.24.2020                                      *
* TR                :                                                  *
* Version           :  001                                             *
* Description       :  Irrigation Monitor Data Preperation and Display *
*                      Data.                                           *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*&---------------------------------------------------------------------*
*& Form DISPLAY_DATA
*&---------------------------------------------------------------------*
*& DISPLAY_DATA
*&---------------------------------------------------------------------*
FORM display_data.
  CALL SCREEN 100.
ENDFORM.

*&---------------------------------------------------------------------*
*& Module STATUS_SET OUTPUT
*&---------------------------------------------------------------------*
*& STATUS_SET
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
*
**&---------------------------------------------------------------------*
**& Form CONTROLS_DISPLAY
**&---------------------------------------------------------------------*
**& CONTROLS_DISPLAY
**&---------------------------------------------------------------------*
FORM controls_display.

*--Local declarations
  DATA : lt_fcat     TYPE lvc_t_fcat,
         lwa_layout  TYPE lvc_s_layo,
         lwa_variant TYPE disvariant,
         o_cont      TYPE REF TO cl_gui_custom_container.

*--Create Object For Custom Container
  CREATE OBJECT o_cont
    EXPORTING
      container_name = c_irr_monitor_0100_cc.

*--Create Object for ALV Grid
  CREATE OBJECT o_alv
    EXPORTING
      i_parent = o_cont.

*--Preparing Field Catalog
  PERFORM field_catalog_prepare CHANGING lt_fcat.

*--Set ALV attributes FOR LAYOUT
  lwa_layout-cwidth_opt = abap_true.
  lwa_layout-zebra      = abap_true.
  lwa_layout-sel_mode   = 'A'.
  lwa_variant-handle    = lwa_layout.
  lwa_variant-report    = sy-repid.

*--Displaying ALV Data
  IF o_alv IS NOT INITIAL.
    CALL METHOD o_alv->set_table_for_first_display
      EXPORTING
        is_variant                    = lwa_variant
        i_save                        = 'A'
        is_layout                     = lwa_layout
      CHANGING
        it_outtab                     = gt_final
        it_fieldcatalog               = lt_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      CREATE OBJECT gr_event_handler.
      SET HANDLER:
        gr_event_handler->handle_user_command FOR o_alv,
        gr_event_handler->handle_toolbar FOR o_alv.

      CALL METHOD o_alv->set_toolbar_interactive.

      CALL METHOD o_alv->set_ready_for_input
        EXPORTING
          i_ready_for_input = 1.

      CALL METHOD o_alv->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_modified.

      CALL METHOD cl_gui_control=>set_focus EXPORTING control = o_alv.
    ENDIF.
  ENDIF.

  PERFORM fcode_save.

ENDFORM.

*&---------------------------------------------------------------------*
*& Module  FCODE_PROCESSING  INPUT
*&---------------------------------------------------------------------*
*& FCODE_PROCESSING
*----------------------------------------------------------------------*
MODULE fcode_processing INPUT.
  PERFORM fcode_processing.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form  FCODE_BACK
*&---------------------------------------------------------------------*
*& FCODE_BACK
*----------------------------------------------------------------------*
FORM fcode_back.
  LEAVE TO SCREEN 0.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form  FCODE_SAVE
*&---------------------------------------------------------------------*
*& FCODE_SAVE
*----------------------------------------------------------------------*
FORM fcode_save.

*--Local declarations
  DATA: lwa_irrmon_insert TYPE zabst_irrmon,
        lwa_irrmon_update TYPE zabst_irrmon,
        lt_irrmon_insert  TYPE TABLE OF zabst_irrmon,
        lt_irrmon_delete  TYPE TABLE OF zabst_irrmon,
        lt_irrmon_update  TYPE TABLE OF zabst_irrmon.

*--Fetching data custom table
  SELECT *
    FROM zabst_irrmon
    INTO TABLE @DATA(lt_irrmon)
     FOR ALL ENTRIES IN @gt_final
   WHERE ordno   EQ @gt_final-ordno
     AND werks   EQ @p_werks
     AND equnr   IN @so_irr[]
     AND irrdate EQ @gt_final-irrdate
     AND shift   IN @so_shift[].
  IF sy-subrc = 0.
    SORT lt_irrmon BY ordno werks equnr irrdate shift.
  ENDIF.

  DATA(lt_final) = gt_final[].
  SORT lt_final BY werks   ASCENDING
                   equnr   ASCENDING
                   shift   ASCENDING
                   irrdate ASCENDING
                   ordno   ASCENDING
                   stime   DESCENDING.

  DELETE ADJACENT DUPLICATES FROM lt_final
    COMPARING ordno werks equnr irrdate shift.

  LOOP AT lt_final INTO DATA(lwa_final).
    READ TABLE lt_irrmon INTO DATA(lwa_irr_mon)
      WITH KEY ordno   = lwa_final-ordno
               werks   = lwa_final-werks
               equnr   = lwa_final-equnr
               irrdate = lwa_final-irrdate
               shift   = lwa_final-shift BINARY SEARCH.
    IF sy-subrc = 0.
      lwa_final-updkz = zcl_abs_abap_maintain=>c_updkz_update. "'U'
      DATA(lwa_irrmon_db) = lwa_irr_mon.
      MOVE-CORRESPONDING lwa_final TO lwa_irrmon_update.
      lwa_irrmon_update-ernam = lwa_irrmon_db-ernam.
      lwa_irrmon_update-erdat = lwa_irrmon_db-erdat.
      lwa_irrmon_update-erzet = lwa_irrmon_db-erzet.
      lwa_irrmon_update-aenam = sy-uname.
      lwa_irrmon_update-aedat = sy-datum.
      lwa_irrmon_update-aezet = sy-uzeit.
      APPEND lwa_irrmon_update TO lt_irrmon_update.
      CLEAR lwa_irrmon_update.
    ELSE.
      lwa_final-updkz = zcl_abs_abap_maintain=>c_updkz_insert. "'I'
      MOVE-CORRESPONDING lwa_final TO lwa_irrmon_insert.
      lwa_irrmon_insert-ernam = sy-uname.
      lwa_irrmon_insert-erdat = sy-datum.
      lwa_irrmon_insert-erzet = sy-uzeit.
      APPEND lwa_irrmon_insert TO lt_irrmon_insert.
      CLEAR lwa_irrmon_insert.
    ENDIF.
  ENDLOOP.

  IF lt_irrmon_insert[] IS NOT INITIAL.
    INSERT zabst_irrmon FROM TABLE lt_irrmon_insert.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

  IF lt_irrmon_update[] IS NOT INITIAL.
    UPDATE zabst_irrmon FROM TABLE lt_irrmon_update.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

  SORT lt_final BY ordno werks equnr irrdate shift.

  LOOP AT lt_irrmon INTO lwa_irr_mon.
    READ TABLE lt_final INTO lwa_final
      WITH KEY ordno   = lwa_irr_mon-ordno
               werks   = lwa_irr_mon-werks
               equnr   = lwa_irr_mon-equnr
               irrdate = lwa_irr_mon-irrdate
               shift   = lwa_irr_mon-shift BINARY SEARCH.
    IF sy-subrc NE 0.
      APPEND lwa_irr_mon TO lt_irrmon_delete.
    ENDIF.
  ENDLOOP.

  IF lt_irrmon_delete[] IS NOT INITIAL.
    DELETE zabst_irrmon FROM TABLE lt_irrmon_delete.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

*--Registros gravados com sucesso!
  MESSAGE TEXT-006 TYPE 'I'.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FIELD_CATALOG_PREPARE
*&---------------------------------------------------------------------*
*& FIELD_CATALOG_PREPARE
*&---------------------------------------------------------------------*
FORM field_catalog_prepare CHANGING lt_fcat TYPE lvc_t_fcat.

*--Preparing field catalog
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = zcl_abs_abap_maintain=>c_irr_monitor_fcat
    CHANGING
      ct_fieldcat            = lt_fcat
    EXCEPTIONS ##FM_SUBRC_OK
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
    IF <fs_fcat>-fieldname = zcl_abs_abap_maintain=>c_fieldname_updkz "'UPDKZ'
    OR <fs_fcat>-fieldname = 'ERNAM'
    OR <fs_fcat>-fieldname = 'ERDAT'
    OR <fs_fcat>-fieldname = 'ERZET'
    OR <fs_fcat>-fieldname = 'AENAM'
    OR <fs_fcat>-fieldname = 'AEDAT'
    OR <fs_fcat>-fieldname = 'AEZET'.
      <fs_fcat>-tech = abap_true.
    ELSEIF <fs_fcat>-fieldname = 'COUNTER'
        OR <fs_fcat>-fieldname = 'MANDT'.
      <fs_fcat>-no_out = abap_true.
    ENDIF.
  ENDLOOP.

  READ TABLE lt_fcat INTO DATA(ls_fcat) WITH KEY fieldname = 'NONIRR_REASON'.
  IF sy-subrc EQ 0.
    READ TABLE lt_fcat ASSIGNING <fs_fcat> WITH KEY fieldname = 'NONIRRRSN_DESC'.
    IF sy-subrc EQ 0.
      <fs_fcat>-col_pos = ls_fcat-col_pos + 1.
      DATA(lv_col_pos) = <fs_fcat>-col_pos.
    ENDIF.
    SORT lt_fcat BY col_pos ASCENDING
                    fieldname DESCENDING.
    READ TABLE lt_fcat INTO ls_fcat
      WITH KEY fieldname = 'NONIRRRSN_DESC'.
    IF sy-subrc EQ 0.
      DATA(lv_tabix) = sy-tabix + 1.
      LOOP AT lt_fcat ASSIGNING <fs_fcat> FROM lv_tabix.
        <fs_fcat>-col_pos = ls_fcat-col_pos + 1.
      ENDLOOP.
    ENDIF.
  ENDIF.

  READ TABLE lt_fcat INTO ls_fcat WITH KEY fieldname = 'EQUNR'.
  IF sy-subrc EQ 0.
    READ TABLE lt_fcat ASSIGNING <fs_fcat> WITH KEY fieldname = 'VORN'.
    IF sy-subrc EQ 0.
      <fs_fcat>-col_pos = ls_fcat-col_pos + 1.
      lv_col_pos = <fs_fcat>-col_pos.
    ENDIF.
    SORT lt_fcat BY col_pos ASCENDING
                    fieldname DESCENDING.
    READ TABLE lt_fcat INTO ls_fcat
      WITH KEY fieldname = 'VORN'.
    IF sy-subrc EQ 0.
      lv_tabix = sy-tabix + 1.
      LOOP AT lt_fcat ASSIGNING <fs_fcat> FROM lv_tabix.
        <fs_fcat>-col_pos = ls_fcat-col_pos + 1.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form INITIALIZE_GLOBAL_DATA
*&---------------------------------------------------------------------*
*& INITIALIZE_GLOBAL_DATA
*&---------------------------------------------------------------------*
FORM initialize_global_data.
  REFRESH: gt_final.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form BUILD_DATA
*&---------------------------------------------------------------------*
*& BUILD_DATA
*&---------------------------------------------------------------------*
FORM build_data.

*--Local declarations
  DATA: lt_ordhdr   TYPE tty_ordhdr,
        lt_ordcnf   TYPE tty_ordcnf,
        lt_tplnr    TYPE tty_tplnr,
        lt_fmirhdr  TYPE tty_fmirhdr,
        lt_descr    TYPE tty_descr,
        lt_irrmon   TYPE tty_irrmon,
        lt_glmdhdr  TYPE tty_glmdhdr,
        lt_glmdatv  TYPE tty_glmdatv,
        lt_glflatv  TYPE tty_glflatv,
        lt_zfmacitm TYPE tty_zfmacitm,
        lt_glflot   TYPE tty_glflot.

*--Fetching data
  PERFORM fetching_data CHANGING lt_ordhdr
                                 lt_ordcnf
                                 lt_tplnr
                                 lt_zfmacitm
                                 lt_fmirhdr
                                 lt_descr
                                 lt_irrmon
                                 lt_glmdhdr
                                 lt_glmdatv
                                 lt_glflot
                                 lt_glflatv.

*--Processing above fetched data
  PERFORM data_prepare USING lt_ordhdr
                             lt_ordcnf
                             lt_tplnr
                             lt_zfmacitm
                             lt_fmirhdr
                             lt_descr
                             lt_irrmon
                             lt_glmdhdr
                             lt_glmdatv
                             lt_glflot
                             lt_glflatv.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form REFRESH_GLOBAL_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM refresh_global_data.
  REFRESH: gt_final.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form F4_FOR_SHIFT
*&---------------------------------------------------------------------*
*& F4_FOR_SHIFT
*&---------------------------------------------------------------------*
FORM f4_for_shift USING lv_low TYPE abap_bool.

*--Local declaration
  DATA: lt_return TYPE STANDARD TABLE OF ddshretval.

  IF gt_shift[] IS INITIAL.
    PERFORM shift_f4 CHANGING gt_shift.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'VORNR'
      dynprofield     = 'SO_SHIFT'
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      multiple_choice = ' ' "'X'
      value_org       = zcl_abs_abap_maintain=>c_f4_valorg_s "'S'
    TABLES
      value_tab       = gt_shift
      return_tab      = lt_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc = 0.
    IF lines( lt_return ) GT 1.
      REFRESH so_shift.
      CLEAR so_shift.
      so_shift = 'IEQ'.

      LOOP AT lt_return INTO DATA(lwa_return).
        so_shift-low = lwa_return-fieldval.
        APPEND so_shift.
      ENDLOOP.

      READ TABLE lt_return INTO lwa_return INDEX 1.
      IF sy-subrc EQ 0.
        so_shift-low  = lwa_return-fieldval.
      ENDIF.
    ELSE.
      READ TABLE lt_return INTO lwa_return INDEX 1.
      IF sy-subrc EQ 0.
        READ TABLE so_shift ASSIGNING FIELD-SYMBOL(<ls_shift>) INDEX 1.
        IF sy-subrc NE 0.
          INSERT INITIAL LINE INTO TABLE so_shift ASSIGNING <ls_shift>.
        ENDIF.
        IF <ls_shift> IS ASSIGNED.
          <ls_shift> = 'IBT'.
          IF lv_low EQ abap_true.
            so_shift-low = <ls_shift>-low = lwa_return-fieldval.
            IF so_shift-high IS NOT INITIAL.
              <ls_shift>-high = so_shift-high.
            ENDIF.
            IF <ls_shift>-high IS INITIAL.
              <ls_shift>-high = <ls_shift>-low.
            ENDIF.
          ELSE.
            so_shift-high = <ls_shift>-high = lwa_return-fieldval.
            IF so_shift-low IS NOT INITIAL.
              <ls_shift>-low = so_shift-low.
            ENDIF.
            IF <ls_shift>-low IS INITIAL.
              <ls_shift>-low = <ls_shift>-high.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FCODE_PROCESSING
*&---------------------------------------------------------------------*
*& FCODE_PROCESSING
*&---------------------------------------------------------------------*
FORM fcode_processing.

*--Local variable declarations
  DATA: lv_tcode       TYPE sy-ucomm,
        lv_routine(32) TYPE c VALUE 'FCODE_'.

  lv_tcode = sy-ucomm.
  CLEAR ok_code.
  CONCATENATE lv_routine lv_tcode INTO lv_routine.
  PERFORM (lv_routine) IN PROGRAM (sy-repid) IF FOUND.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SELECTION_VALIDATIONS
*&---------------------------------------------------------------------*
*& SELECTION_VALIDATIONS
*&---------------------------------------------------------------------*
FORM selection_validations.

  IF gt_shift IS INITIAL.
    PERFORM shift_f4 CHANGING gt_shift.
  ENDIF.

*--Validating Form
  IF p_werks IS NOT INITIAL.
*--Fetching Equipment header and Equipment master plants data
    SELECT a~equnr, b~werks
      FROM /agri/fmirhdr AS a
     INNER JOIN /agri/fmirwrk AS b
        ON a~equnr = b~equnr
      INTO TABLE @DATA(lt_fmirhdr)
     WHERE a~irtyp EQ @zcl_abs_abap_maintain=>c_irrtyp "'001'
       AND b~werks EQ @p_werks.
    IF sy-subrc <> 0.
      MESSAGE TEXT-008 TYPE zcl_abs_abap_maintain=>c_msgty_info.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

  IF p_werks IS INITIAL.
*-- Atenção: Informar a Fazenda! Parâmetro de preenchimento obrigatório!
    MESSAGE i135(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF so_irr[] IS INITIAL.
*-- Atenção: Informar o Projeto! Parâmetro de preenchimento obrigatório!
    MESSAGE i134(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF so_date[] IS INITIAL.
*-- Atenção: Informar a Data! Parâmetro de preenchimento obrigatório!
    MESSAGE i133(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

*--Validating Equipment
  REFRESH gt_irhdr.
  SELECT equnr, irtyp, txtgr, descr
    FROM /agri/fmirhdr
    INTO TABLE @gt_irhdr
   WHERE equnr IN @so_irr[].

  IF sy-subrc <> 0.
    MESSAGE TEXT-007 TYPE zcl_abs_abap_maintain=>c_msgty_info.
    LEAVE LIST-PROCESSING.
  ELSE.
    SORT gt_irhdr BY equnr.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SHIFT_F4
*&---------------------------------------------------------------------*
*& SHIFT_F4
*&---------------------------------------------------------------------*
FORM shift_f4 CHANGING ct_shift TYPE tty_shift.

*--Local declarations
  DATA: lwa_shift TYPE ty_shift,
        lv_datab  TYPE datum,
        lv_datbi  TYPE datum,
        lv_sdate  TYPE datum.

  IF so_date-low IS NOT INITIAL.
    PERFORM get_week_dates USING so_date-low CHANGING lv_datab lv_datbi.
  ENDIF.

  IF so_date-high IS NOT INITIAL.
    PERFORM get_week_dates USING so_date-high CHANGING lv_sdate lv_datbi.
  ENDIF.

*--Fetching Order Numbers
  SELECT SINGLE ordno
    FROM zabst_ordhdr
    INTO @DATA(lv_ordno)
   WHERE werks EQ @p_werks
     AND equnr IN @so_irr[]
     AND datab LE @lv_datab
     AND datbi GE @lv_datbi.

*--Fetching WO Numbers
  SELECT ordno, wonum
    FROM zabst_orditm
    INTO TABLE @DATA(lt_wonum)
   WHERE ordno EQ @lv_ordno.
  IF sy-subrc = 0.
    SORT lt_wonum BY wonum.
  ENDIF.

*--Fetching Operations(Shift numbers)
  SELECT wonum,
         vornr,
         ltxa1
    FROM /agri/fmwoopr
    INTO TABLE @DATA(lt_opr_temp)
    FOR ALL ENTRIES IN @lt_wonum
   WHERE wonum EQ @lt_wonum-wonum.
  IF sy-subrc = 0.
    SORT lt_opr_temp BY wonum.
  ENDIF.

  DATA(lt_opr) = lt_opr_temp.
  SORT lt_opr BY vornr.
  DELETE ADJACENT DUPLICATES FROM lt_opr COMPARING vornr.

  LOOP AT so_shift INTO DATA(ls_shift).
    READ TABLE lt_opr TRANSPORTING NO FIELDS
      WITH KEY vornr = ls_shift-low.
    IF sy-subrc <> 0.
*-- Shift &1 inválido!
      MESSAGE i112(zfmfp) WITH ls_shift-low.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDLOOP.

  IF so_shift[] IS NOT INITIAL.
    DELETE lt_opr WHERE vornr NOT IN so_shift[].
  ENDIF.

  LOOP AT lt_opr INTO DATA(lwa_opr).
    lwa_shift-vornr = lwa_opr-vornr.
    lwa_shift-ltxa1 = lwa_opr-ltxa1.
    APPEND lwa_shift TO ct_shift.
    CLEAR lwa_shift.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_WEEK_DATES
*&---------------------------------------------------------------------*
*& GET_WEEK_DATES
*&---------------------------------------------------------------------*
FORM get_week_dates USING iv_date  TYPE datum
                 CHANGING cv_datab TYPE datum
                          cv_datbi TYPE datum.

  CALL FUNCTION 'GET_WEEK_INFO_BASED_ON_DATE'
    EXPORTING
      date   = iv_date
    IMPORTING
      monday = cv_datab
      sunday = cv_datbi.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FETCHING_DATA
*&---------------------------------------------------------------------*
*& FETCHING_DATA
*&---------------------------------------------------------------------*
FORM fetching_data CHANGING ct_ordhdr   TYPE tty_ordhdr
                            ct_ordcnf   TYPE tty_ordcnf
                            ct_tplnr    TYPE tty_tplnr
                            ct_zfmacitm TYPE tty_zfmacitm
                            ct_fmirhdr  TYPE tty_fmirhdr
                            ct_descr    TYPE tty_descr
                            ct_irrmon   TYPE tty_irrmon
                            ct_glmdhdr  TYPE tty_glmdhdr
                            ct_glmdatv  TYPE tty_glmdatv
                            ct_glflot   TYPE tty_glflot
                            ct_glflatv  TYPE tty_glflatv.

*--Local declarations
  DATA: lrt_atnam     TYPE RANGE OF atnam,
        lv_datab      TYPE datum,
        lv_datbi      TYPE datum,
        lv_pdate      TYPE datum,
        lv_sdate      TYPE datum,
        lv_atnam      TYPE atnam,
        lv_cnval1     TYPE zabs_del_cnval,
        lt_equp_temp  TYPE tty_equp_tplnr,
        lt_equp_tplnr TYPE tty_equp_tplnr,
        lr_date       TYPE RANGE OF zabs_del_irrdate WITH HEADER LINE.

  IF so_date-low IS NOT INITIAL.
    PERFORM get_week_dates USING so_date-low CHANGING lv_datab lv_datbi.
  ENDIF.

  IF so_date-high IS NOT INITIAL.
    PERFORM get_week_dates USING so_date-high CHANGING lv_sdate lv_datbi.
  ENDIF.

*--Fetching Order Numbers
  SELECT ordno, werks,
         equnr, datab, datbi
    FROM zabst_ordhdr
    INTO TABLE @ct_ordhdr
   WHERE equnr   IN @so_irr[]
     AND werks   EQ @p_werks
     AND ( datab IN @so_date OR datbi IN @so_date[] ).

  IF sy-subrc NE 0.
    MESSAGE TEXT-001 TYPE zcl_abs_abap_maintain=>c_msgty_info.
    LEAVE LIST-PROCESSING.
  ELSE.
    SORT ct_ordhdr BY equnr ordno.
  ENDIF.

*--Fetching Work Order Confirmation data
  SELECT ordno, contr, vornr,
         cdate, idate, fdate,
         ihour, fhour, tot_iqty
    FROM zabst_ordcnf
    INTO TABLE @ct_ordcnf
     FOR ALL ENTRIES IN @ct_ordhdr
   WHERE ordno EQ @ct_ordhdr-ordno
     AND vornr IN @so_shift
     AND loevm EQ @space.
  IF sy-subrc = 0.
    SORT ct_ordcnf BY ordno cdate vornr.
  ENDIF.

*--Fetching Terrain Header data to get terrains
  SELECT equnr, tplnr_fl,
         zzcshift1, zzcshift2,
         zzcshift3, zzcshift4,
         zzcshift5, zzcshift6
    FROM /agri/fmirflo
    INTO TABLE @ct_tplnr
   WHERE equnr IN @so_irr[].
  IF sy-subrc <> 0.
    MESSAGE TEXT-002 TYPE zcl_abs_abap_maintain=>c_msgty_info.
    LEAVE LIST-PROCESSING.
  ENDIF.
  SORT ct_tplnr BY equnr tplnr_fl.

*--Fetching Equipments based on Terrains for types('002' and '003')
  SELECT equnr, tplnr_fl
    FROM /agri/fmirflo
    INTO TABLE @lt_equp_tplnr
    FOR ALL ENTRIES IN @ct_tplnr
   WHERE tplnr_fl EQ @ct_tplnr-tplnr_fl.

  IF sy-subrc = 0.
    DATA(lt_equp_tplnr_tmp) = lt_equp_tplnr.
    SORT lt_equp_tplnr_tmp BY equnr.
    DELETE ADJACENT DUPLICATES FROM lt_equp_tplnr_tmp COMPARING equnr.

*--Fetching Equipment header and Equipment master plants data
    SELECT equnr
           irtyp
           zzrefcty
      FROM /agri/fmirhdr
      INTO TABLE ct_fmirhdr
      FOR ALL ENTRIES IN lt_equp_tplnr_tmp
     WHERE equnr EQ lt_equp_tplnr_tmp-equnr
       AND irtyp IN ( zcl_abs_abap_maintain=>c_irrtyp_001 ,
                      zcl_abs_abap_maintain=>c_irrtyp_002 ,
                      zcl_abs_abap_maintain=>c_irrtyp_003 ).
    IF sy-subrc EQ 0.
      SORT ct_fmirhdr BY equnr.
    ENDIF.

    LOOP AT lt_equp_tplnr INTO DATA(lwa_equp_tplnr).
      READ TABLE ct_fmirhdr INTO DATA(lwa_fmirhdr)
        WITH KEY equnr = lwa_equp_tplnr-equnr BINARY SEARCH.
      IF sy-subrc = 0
      AND ( lwa_fmirhdr-irtyp = zcl_abs_abap_maintain=>c_irrtyp_001 OR
            lwa_fmirhdr-irtyp = zcl_abs_abap_maintain=>c_irrtyp_002 OR
            lwa_fmirhdr-irtyp = zcl_abs_abap_maintain=>c_irrtyp_003 ).
        APPEND lwa_equp_tplnr TO lt_equp_temp.
        CLEAR lwa_equp_tplnr.
      ENDIF.
    ENDLOOP.
  ENDIF.

*--Fetching Equipment header data
  SELECT equnr, descr
    FROM /agri/fmirhdrt
    INTO TABLE @ct_descr
    FOR ALL ENTRIES IN @ct_ordhdr
   WHERE equnr EQ @ct_ordhdr-equnr
     AND spras EQ @sy-langu.
  IF sy-subrc = 0.
    SORT ct_descr BY equnr.
  ENDIF.

*--Get variant table data
  CALL METHOD zcl_abs_get_variants=>get_constant_single
    EXPORTING
      iv_mod    = space
      iv_objid  = zcl_abs_abap_maintain=>c_objid_irr_wb "'IRWO'
      iv_k1val  = zcl_abs_abap_maintain=>c_key_irr_mnth "'MNTH'
    IMPORTING
      ev_cnval1 = lv_cnval1.

  lv_pdate = so_date-low - 1.
  lr_date = 'IBT'.
  lr_date-low = lv_pdate.
  IF so_date-high IS NOT INITIAL.
    lr_date-high = so_date-high.
  ELSE.
    lr_date-high = so_date-low.
  ENDIF.
  APPEND lr_date.

*--Fetching Custom table data(ZABST_IRRMON) to irrigation for previous day
  SELECT *
    FROM zabst_irrmon
    INTO TABLE @ct_irrmon
    FOR ALL ENTRIES IN @ct_ordhdr
   WHERE werks   EQ @ct_ordhdr-werks
     AND equnr   EQ @ct_ordhdr-equnr
     AND irrdate IN @lr_date[]
     AND shift   IN @so_shift[]
     AND loevm   EQ @abap_false.

  IF sy-subrc EQ 0.
    SORT ct_irrmon BY werks   ASCENDING
                      equnr   ASCENDING
                      shift   ASCENDING
                      irrdate ASCENDING
                      ordno   ASCENDING
                      stime   DESCENDING.
  ENDIF.

  IF lt_equp_temp[] IS NOT INITIAL.
*--Fetching Terrain Attribute Values to get Crop Coefficient value
    SELECT a~mdocm a~tplnr_fl a~equnr
           a~mdate a~mtime b~class
      INTO TABLE ct_glmdhdr
      FROM /agri/glmdhdr AS a
      INNER JOIN /agri/fmirmea AS b
      ON b~equnr = a~equnr
       FOR ALL ENTRIES IN lt_equp_temp
     WHERE a~tplnr_fl EQ lt_equp_temp-tplnr_fl
       AND a~equnr    EQ lt_equp_temp-equnr
       AND a~mdate    IN so_date
       AND a~kfrst    EQ space
       AND b~class    IN ( zcl_abs_abap_maintain=>c_class_evap ,      "'EVAPORATION'
                           zcl_abs_abap_maintain=>c_class_prec ,      "'PRECIPITACION'
                           zcl_abs_abap_maintain=>c_class_nirr_rsn ). "'MOTIVO_NAO_IRRIGAÇ'
    IF sy-subrc = 0.
      LOOP AT so_shift INTO DATA(ls_shift).
        CLEAR lv_atnam.
        CASE ls_shift-low.
          WHEN c_shift1. "'0010'
            lv_atnam = zcl_abs_abap_maintain=>c_charact_nirsn_sft1. "'SHIFT1_NAOIRRIGA'
          WHEN c_shift2. "'0020'
            lv_atnam = zcl_abs_abap_maintain=>c_charact_nirsn_sft2. "'SHIFT2_NAOIRRIGA'
          WHEN c_shift3. "'0030'
            lv_atnam = zcl_abs_abap_maintain=>c_charact_nirsn_sft3. "'SHIFT3_NAOIRRIGA'
          WHEN c_shift4. "'0040'
            lv_atnam = zcl_abs_abap_maintain=>c_charact_nirsn_sft4. "'SHIFT4_NAOIRRIGA'
          WHEN c_shift5. "'0050'
            lv_atnam = zcl_abs_abap_maintain=>c_charact_nirsn_sft5. "'SHIFT5_NAOIRRIGA'
          WHEN c_shift6. "'0060'
            lv_atnam = zcl_abs_abap_maintain=>c_charact_nirsn_sft6. "'SHIFT6_NAOIRRIGA'
        ENDCASE.

        IF lv_atnam IS NOT INITIAL.
          INSERT INITIAL LINE INTO TABLE lrt_atnam
            ASSIGNING FIELD-SYMBOL(<lrs_atnam>).
          IF sy-subrc EQ 0.
            <lrs_atnam> = 'IEQ'.
            <lrs_atnam>-low = lv_atnam.
          ENDIF.
        ENDIF.
      ENDLOOP.

      DO 2 TIMES.
        DATA(lv_index) = sy-index.
        INSERT INITIAL LINE INTO TABLE lrt_atnam ASSIGNING <lrs_atnam>.
        IF sy-subrc EQ 0.
          <lrs_atnam> = 'IEQ'.
          CASE lv_index.
            WHEN 1.
              <lrs_atnam>-low = zcl_abs_abap_maintain=>c_class_evap. "'EVAPORATION'
            WHEN 2.
              <lrs_atnam>-low = zcl_abs_abap_maintain=>c_class_prec. "'PRECIPITACION'
          ENDCASE.
        ENDIF.
      ENDDO.

      SORT: ct_glmdhdr BY class mdate mtime DESCENDING.
      DELETE ADJACENT DUPLICATES FROM ct_glmdhdr COMPARING class mdate.

      SELECT a~mdocm, a~atzhl, a~atwrt,
             a~atflv, b~atinn, b~atnam
        FROM /agri/glmdatv AS a
        JOIN cabn          AS b
          ON b~atinn = a~atinn
        INTO TABLE @ct_glmdatv
         FOR ALL ENTRIES IN @ct_glmdhdr
       WHERE a~mdocm EQ @ct_glmdhdr-mdocm
         AND b~atnam IN @lrt_atnam[].

      IF sy-subrc = 0.
        SORT ct_glmdatv BY mdocm atnam.

*--Fetching attribute value description
        SELECT a~atinn, a~atwrt, b~atwtb
          INTO TABLE @gt_atval_desc
          FROM cawn AS a
         INNER JOIN cawnt AS b
            ON b~atinn EQ a~atinn
           AND b~atzhl EQ a~atzhl
           AND b~adzhl EQ a~adzhl
           FOR ALL ENTRIES IN @ct_glmdatv
         WHERE a~atinn EQ @ct_glmdatv-atinn
           AND a~atwrt EQ @ct_glmdatv-atwrt
           AND b~spras EQ @sy-langu.
        IF sy-subrc EQ 0.
          SORT gt_atval_desc BY atinn atwrt.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

*--Fetching Terrain Header data to get gross terrain arae
  SELECT tplnr_fl, garea, strno
    FROM /agri/glflot
    INTO TABLE @ct_glflot
     FOR ALL ENTRIES IN @ct_tplnr
   WHERE tplnr_fl EQ @ct_tplnr-tplnr_fl
     AND iwerk    EQ @p_werks
     AND kfrst    EQ @space
     AND loevm    EQ @space.

  IF sy-subrc = 0.
    SORT ct_glflot BY tplnr_fl.

*--Fetching Terrain Attribute Values to get Crop Coefficient value
    SELECT a~tplnr_fl, a~atwrt,
           a~atflv, b~atinn
      FROM /agri/glflatv AS a
      JOIN cabn          AS b
        ON b~atinn = a~atinn
      INTO TABLE @ct_glflatv
       FOR ALL ENTRIES IN @ct_glflot
     WHERE a~tplnr_fl  EQ @ct_glflot-tplnr_fl
       AND a~class     EQ @zcl_abs_abap_maintain=>c_class_info "'INFO_PROJETO'
       AND a~deleted   EQ @space
       AND b~atnam     EQ @zcl_abs_abap_maintain=>c_charact_coef. "'COEF_CULTURA'
    IF sy-subrc EQ 0.
      SORT ct_glflatv BY tplnr_fl.
    ENDIF.
  ENDIF.

  IF ct_tplnr[] IS NOT INITIAL.
    SELECT i~acnum, i~acpos, i~tplnr_fl, i~iwerk,
           i~contr, i~cmnum, i~season, i~astat,
           i~aarea,  h~ajahr, h~actyp, h~acdes,
           h~werks, h~datab, h~datbi
      FROM zfmaitm AS i
      INNER JOIN zfmachdr AS h
      ON i~acnum = h~acnum
      INTO TABLE @ct_zfmacitm
      FOR ALL ENTRIES IN @ct_tplnr
     WHERE i~tplnr_fl EQ @ct_tplnr-tplnr_fl
       AND h~datab LE @sy-datum
       AND h~datbi GT @sy-datum.

    SORT ct_zfmacitm BY tplnr_fl.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DATA_PREPARE
*&---------------------------------------------------------------------*
*& DATA_PREPARE
*&---------------------------------------------------------------------*
FORM data_prepare USING pt_ordhdr   TYPE tty_ordhdr
                        pt_ordcnf   TYPE tty_ordcnf
                        pt_tplnr    TYPE tty_tplnr
                        pt_zfmacitm TYPE tty_zfmacitm
                        pt_fmirhdr  TYPE tty_fmirhdr
                        pt_descr    TYPE tty_descr
                        pt_irrmon   TYPE tty_irrmon
                        pt_glmdhdr  TYPE tty_glmdhdr
                        pt_glmdatv  TYPE tty_glmdatv
                        pt_glflot   TYPE tty_glflot
                        pt_glflatv  TYPE tty_glflatv.

*--Local declarations
  DATA: lwa_final       TYPE zabs_str_irr_monitor_fcat,
        lv_days         TYPE t5a4a-dlydy,
        lv_prevdate_x   TYPE p0001-begda,
        lv_days_x       TYPE t5a4a-dlydy,
        lv_zzrefcty_aux TYPE zabs_del_cfc,
        lv_shift        TYPE zabst_irrmon-shift,
        lv_nextdate     TYPE p0001-begda.

  SORT so_shift BY low ASCENDING.
  DELETE ADJACENT DUPLICATES FROM so_shift COMPARING low.

  IF sy-uname EQ 'T_H.KABABE'.
    BREAK-POINT.
  ENDIF.

  DATA(lv_times) = lines( gt_shift ).

  LOOP AT gt_irhdr INTO DATA(lwa_irhdr).
    gv_new_equnr = abap_true.
    DATA(lv_value_found) = abap_false.
    DO lv_times TIMES.
      READ TABLE gt_shift INTO DATA(lwa_shift) INDEX sy-index.
      IF sy-subrc EQ 0.
        DATA(lt_tplnr) = pt_tplnr[].
        DELETE lt_tplnr WHERE equnr NE lwa_irhdr-equnr.
        CASE lwa_shift-vornr.
          WHEN '0010'.
            DELETE lt_tplnr WHERE zzcshift1 IS INITIAL.
          WHEN '0020'.
            DELETE lt_tplnr WHERE zzcshift2 IS INITIAL.
          WHEN '0030'.
            DELETE lt_tplnr WHERE zzcshift3 IS INITIAL.
          WHEN '0040'.
            DELETE lt_tplnr WHERE zzcshift4 IS INITIAL.
          WHEN '0050'.
            DELETE lt_tplnr WHERE zzcshift5 IS INITIAL.
          WHEN '0060'.
            DELETE lt_tplnr WHERE zzcshift6 IS INITIAL.
        ENDCASE.

        IF lt_tplnr[] IS INITIAL.
          CONTINUE.
        ENDIF.

        gv_new_shift = abap_true.

        READ TABLE pt_ordhdr TRANSPORTING NO FIELDS
          WITH KEY equnr = lwa_irhdr-equnr BINARY SEARCH.
        IF sy-subrc EQ 0.
          LOOP AT pt_ordhdr INTO DATA(lwa_ordhdr) FROM sy-tabix.
            IF lwa_ordhdr-equnr NE lwa_irhdr-equnr.
              EXIT.
            ENDIF.
            CLEAR lwa_final.
*--Filling Order Header data
            lwa_final-ordno = lwa_ordhdr-ordno.
            lwa_final-equnr = lwa_ordhdr-equnr.
            lwa_final-werks = lwa_ordhdr-werks.
            lwa_final-shift = lwa_shift-vornr.

*--Euipment Description
            READ TABLE pt_descr INTO DATA(lwa_descr)
              WITH KEY equnr = lwa_ordhdr-equnr BINARY SEARCH.
            IF sy-subrc = 0.
              lwa_final-edescr  = lwa_descr-descr.
            ENDIF.

*--Filling Crop Coefficient and Shift Area
            PERFORM shift_area USING lwa_ordhdr
                                     pt_tplnr
                                     pt_zfmacitm
                                     pt_glflot
                                     pt_glflatv
                            CHANGING lwa_final.

            CLEAR lv_days.

*--Loop based on Dates
            DO.
              IF lwa_ordhdr-datab IS INITIAL.
                EXIT.
              ENDIF.

*--Validating Date
              CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
                EXPORTING
                  date      = lwa_ordhdr-datab
                  days      = lv_days
                  months    = 0
                  signum    = '+'
                  years     = 0
                IMPORTING
                  calc_date = lv_nextdate.

              IF lwa_ordhdr-datbi < lv_nextdate
              OR ( so_date-high < lv_nextdate
             AND so_date-high IS NOT INITIAL ).
                EXIT.
              ENDIF.

              lv_days = lv_days + 1.

              IF lv_nextdate < so_date-low.
                CONTINUE.
              ENDIF.
              IF lv_zzrefcty_aux IS INITIAL
              OR lwa_shift-vornr NE lv_shift
              OR lv_value_found EQ abap_false.
*--If record doesn't exist in current execution, Check for ZABST_IRRMON table
                READ TABLE pt_irrmon INTO DATA(lwa_irrmon)
                  WITH KEY werks   = lwa_final-werks
                           equnr   = lwa_final-equnr
                           shift   = lwa_final-shift BINARY SEARCH.
                IF sy-subrc NE 0.
*--Filling Equipment Header data
                  READ TABLE pt_fmirhdr INTO DATA(lwa_fmirhdr)
                    WITH KEY equnr = lwa_ordhdr-equnr
                             irtyp = zcl_abs_abap_maintain=>c_irrtyp_001 BINARY SEARCH.
                  IF sy-subrc = 0.
                    lv_zzrefcty_aux = lwa_fmirhdr-zzrefcty.
                  ENDIF.
                ELSE.
                  lv_days_x = 1.
                  CLEAR: lv_prevdate_x.
*--FM to get previous date of Irrigation(mm)
                  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
                    EXPORTING
                      date      = lv_nextdate
                      days      = lv_days_x
                      months    = 0
                      signum    = '-'
                      years     = 0
                    IMPORTING
                      calc_date = lv_prevdate_x.

*--Checking for previous date based on order number
                  DATA(lv_found) = abap_false.
                  READ TABLE pt_irrmon INTO lwa_irrmon
                    WITH KEY werks   = lwa_final-werks
                             equnr   = lwa_final-equnr
                             shift   = lwa_final-shift
                             irrdate = lv_prevdate_x
                             ordno   = lwa_final-ordno BINARY SEARCH.
                  IF sy-subrc <> 0.
*--Checking for previous date without order number
                    READ TABLE pt_irrmon INTO lwa_irrmon
                      WITH KEY werks   = lwa_final-werks
                               equnr   = lwa_final-equnr
                               shift   = lwa_final-shift
                               irrdate = lv_prevdate_x BINARY SEARCH.
                    IF sy-subrc EQ 0.
                      lv_value_found = lv_found = abap_true.
                    ENDIF.
                  ELSE.
                    lv_value_found = lv_found = abap_true.
                  ENDIF.

                  IF lv_found EQ abap_true.
                    lv_zzrefcty_aux = lwa_irrmon-cfield_cap.
                  ENDIF.
                ENDIF.
              ENDIF.

*--Processing Attributes data
              PERFORM attributes_data USING pt_ordcnf
                                            pt_irrmon
                                            pt_glmdhdr
                                            pt_glmdatv
                                            lwa_ordhdr
                                            lv_nextdate
                                            lv_found
                                   CHANGING lwa_final
                                            lv_zzrefcty_aux.

              IF so_date-high < lv_nextdate.
                EXIT.
              ENDIF.
              lv_shift = lwa_shift-vornr.
            ENDDO.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDLOOP.

  LOOP AT gt_final ASSIGNING FIELD-SYMBOL(<lwa_final>).
    CLEAR lwa_fmirhdr.
    READ TABLE pt_fmirhdr INTO lwa_fmirhdr
      WITH KEY equnr = <lwa_final>-equnr
               irtyp = zcl_abs_abap_maintain=>c_irrtyp_001 BINARY SEARCH.
    IF sy-subrc = 0.
      IF <lwa_final>-cfield_cap GT lwa_fmirhdr-zzrefcty.
        <lwa_final>-cfield_cap = lwa_fmirhdr-zzrefcty.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.

**&---------------------------------------------------------------------*
**& Form TERRAIN_GROSSAREA
**&---------------------------------------------------------------------*
**& TERRAIN_GROSSAREA
**&---------------------------------------------------------------------*
*FORM terrain_grossarea USING pwa_ordhdr TYPE ty_ordhdr
*                             pt_tplnr   TYPE tty_tplnr
*                             pt_glflot  TYPE tty_glflot
*                             pt_glflatv TYPE tty_glflatv
*                    CHANGING cwa_final  TYPE zabs_str_irr_monitor_fcat.
*
**--Local declarations
*  DATA : lv_garea  TYPE /agri/glgarea,
*         lv_atflv  TYPE p DECIMALS 3,
*         lt_dayint TYPE TABLE OF pwsdayint.
*
*  READ TABLE pt_tplnr TRANSPORTING NO FIELDS
*    WITH KEY equnr = pwa_ordhdr-equnr BINARY SEARCH.
*  IF sy-subrc = 0.
*    DATA(lv_tabix) = sy-tabix.
*    LOOP AT pt_tplnr INTO DATA(lwa_tplnr) FROM lv_tabix.
*      IF lwa_tplnr-equnr <> pwa_ordhdr-equnr.
*        EXIT.
*      ENDIF.
*
*      READ TABLE pt_glflot INTO DATA(lwa_glflot)
*        WITH KEY tplnr_fl = lwa_tplnr-tplnr_fl BINARY SEARCH.
*      IF sy-subrc <> 0.
*        CONTINUE.
*      ENDIF.
*
*      lv_garea = lv_garea + ( lwa_glflot-garea * lwa_tplnr-zzcshift1 ) / 100.
*
**-- Based on shifts
*      CASE cwa_final-shift.
*        WHEN c_shift1. "'0010'
*          lv_garea = lv_garea + ( lwa_glflot-garea * lwa_tplnr-zzcshift1 ) / 100.
*        WHEN c_shift2. "'0020'
*          lv_garea = lv_garea + ( lwa_glflot-garea * lwa_tplnr-zzcshift2 ) / 100.
*        WHEN c_shift3. "'0030'
*          lv_garea = lv_garea + ( lwa_glflot-garea * lwa_tplnr-zzcshift3 ) / 100.
*        WHEN c_shift4. "'0040'
*          lv_garea = lv_garea + ( lwa_glflot-garea * lwa_tplnr-zzcshift4 ) / 100.
*        WHEN c_shift5. "'0050
*          lv_garea = lv_garea + ( lwa_glflot-garea * lwa_tplnr-zzcshift5 ) / 100.
*        WHEN c_shift6. "'0060'
*          lv_garea = lv_garea + ( lwa_glflot-garea * lwa_tplnr-zzcshift6 ) / 100.
*      ENDCASE.
*
*      READ TABLE pt_glflatv INTO DATA(lwa_glflatv)
*        WITH KEY tplnr_fl = lwa_glflot-tplnr_fl BINARY SEARCH.
*      IF sy-subrc <> 0.
*        CONTINUE.
*      ENDIF.
*
*      IF lwa_glflatv-atwrt IS NOT INITIAL.
*        cwa_final-ccoef = lwa_glflatv-atwrt.
*      ELSEIF lwa_glflatv-atflv IS NOT INITIAL.
*        lv_atflv = lwa_glflatv-atflv.
*        cwa_final-ccoef =  lv_atflv.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.
*
*  cwa_final-garea = lv_garea.
*
*ENDFORM.

*&---------------------------------------------------------------------*
*& Form ATTRIBUTES_DATA
*&---------------------------------------------------------------------*
*& ATTRIBUTES_DATA
*&---------------------------------------------------------------------*
FORM attributes_data USING pt_ordcnf   TYPE tty_ordcnf
                           pt_irrmon   TYPE tty_irrmon
                           pt_glmdhdr  TYPE tty_glmdhdr
                           pt_glmdatv  TYPE tty_glmdatv
                           pwa_ordhdr  TYPE ty_ordhdr
                           pv_date     TYPE p0001-begda
                           pv_found    TYPE abap_bool
                  CHANGING cwa_final   TYPE zabs_str_irr_monitor_fcat
                           cv_zzrefcty TYPE zabs_del_cfc.

  DATA : BEGIN OF ls_duration,
           days    TYPE f,
           hours   TYPE f,
           minutes TYPE f,
           seconds TYPE i VALUE 0,
         END OF ls_duration.

*--Local declarations
  DATA : lt_interval   TYPE TABLE OF ptws_time_duration,
         lt_dayint     TYPE TABLE OF pwsdayint,
         lwa_final     TYPE zabs_str_irr_monitor_fcat,
         lv_stime      TYPE ptprg-beguz,
         lv_etime      TYPE ptprg-enduz,
         lv_tdiff      TYPE tvro-fahztd,
         lv_prevdate   TYPE p0001-begda,
         lv_garea      TYPE /agri/glgarea,
         lv_disp       TYPE xfeld,
         lv_hyd_bal    TYPE zabs_del_hbal,
         lv_timeamount TYPE i VALUE 0,
         lv_duration   TYPE sytabix,
         lv_atdesc     TYPE atwtb.

  MOVE-CORRESPONDING cwa_final TO lwa_final.
  READ TABLE pt_ordcnf TRANSPORTING NO FIELDS
    WITH KEY ordno = pwa_ordhdr-ordno
             cdate = pv_date
             vornr = cwa_final-shift BINARY SEARCH.
  IF sy-subrc = 0.
    DATA(lv_tabix) = sy-tabix.
    LOOP AT pt_ordcnf INTO DATA(lwa_ordcnf) FROM lv_tabix.
      IF lwa_ordcnf-ordno <> pwa_ordhdr-ordno
      OR lwa_ordcnf-cdate <> pv_date
      OR lwa_ordcnf-vornr <> cwa_final-shift.
        EXIT.
      ENDIF.

      lv_disp = abap_true.
      cwa_final-irrdate = pv_date.
      cwa_final-irr_m3  = cwa_final-irr_m3 + lwa_ordcnf-tot_iqty.
      cwa_final-stime   = lwa_ordcnf-ihour.
      cwa_final-etime   = lwa_ordcnf-fhour.
      cwa_final-idate   = lwa_ordcnf-idate.
      cwa_final-fdate   = lwa_ordcnf-fdate.

      lv_stime = cwa_final-stime.
      lv_etime = cwa_final-etime.

      CALL FUNCTION 'SWI_DURATION_DETERMINE'
        EXPORTING
          start_date = lwa_ordcnf-idate
          end_date   = cwa_final-fdate
          start_time = cwa_final-stime
          end_time   = cwa_final-etime
        IMPORTING
          duration   = lv_duration
        EXCEPTIONS
          OTHERS     = 1.

      IF sy-subrc EQ 0.
        ls_duration-seconds = lv_duration.
        ls_duration-minutes = ls_duration-seconds / 60.
        ls_duration-days    = ls_duration-seconds / 84600 .
        ls_duration-hours   = ls_duration-seconds / 3600.
        cwa_final-irr_hr    = ls_duration-hours.
      ENDIF.

*--Calculating Irrigation mm : Irrigation (m3) / Area (m2)] x 0,1
      CLEAR lv_garea.
      lv_garea = cwa_final-garea.

      IF lv_garea IS NOT INITIAL.
        cwa_final-irr_mm    = ( cwa_final-irr_m3 / lv_garea ) * '0.1'.
      ENDIF.

      IF cwa_final-irr_hr IS NOT INITIAL. "Start time – End time
*"Irrigation (m3) / Irrigation Hours (h)
        cwa_final-rate   = cwa_final-irr_m3 / cwa_final-irr_hr.
*--Irrigation per hour (mm/h)
*"Irrigation (mm) / Irrigated Time (h).
        cwa_final-mm_h   = cwa_final-irr_mm / cwa_final-irr_hr.
      ENDIF.

*--Filling Evaporation
      PERFORM measurement_attributes USING pv_date
                                           zcl_abs_abap_maintain=>c_class_evap "'EVAPORATION'
                                           pwa_ordhdr
                                           pt_glmdhdr
                                           pt_glmdatv
                                  CHANGING cwa_final-evap
                                           lv_disp
                                           lv_atdesc.

*--Filling Rain(Chuvas)
      PERFORM measurement_attributes USING pv_date
                                           zcl_abs_abap_maintain=>c_class_prec "'PRECIPITATION'
                                           pwa_ordhdr
                                           pt_glmdhdr
                                           pt_glmdatv
                                  CHANGING cwa_final-chuvas
                                           lv_disp
                                           lv_atdesc.

*--Filling Nonirrigation reason
      PERFORM measurement_attributes USING pv_date
                                           zcl_abs_abap_maintain=>c_class_nirr_rsn  "'MOTIVO_NAO_IRRIGAÇ'
                                           pwa_ordhdr
                                           pt_glmdhdr
                                           pt_glmdatv
                                  CHANGING cwa_final-nonirr_reason
                                           lv_disp
                                           lv_atdesc.

*--Filling Non Irrigation Reason description
      IF cwa_final-nonirr_reason IS NOT INITIAL.
        cwa_final-nonirrrsn_desc = lv_atdesc.
      ENDIF.

      cwa_final-evaptrans = cwa_final-evap * cwa_final-ccoef.

*--Calculating Effective Rain
      IF ( 1 LE cwa_final-chuvas )
      AND ( cwa_final-chuvas LE 10 ).
        cwa_final-effchuva = 0.
      ELSEIF ( '10.1' LE cwa_final-chuvas )
         AND ( cwa_final-chuvas LE 60 ).
        cwa_final-effchuva = cwa_final-chuvas * '0.7'.
      ELSEIF ( cwa_final-chuvas GT 60 ).
        cwa_final-effchuva = 45.
      ENDIF.

*--Calculating Hydric Balance
      PERFORM hydric_bal USING pv_date
                               pwa_ordhdr
                               pt_irrmon
                      CHANGING cwa_final.

*--Current Field Capacity: Reference Field Capacity (RFC) + Accum. Hydric Balance (mm)
      cv_zzrefcty = cv_zzrefcty + cwa_final-hyd_bal.
*--BOC-T_T.KONNO-102220
      IF cwa_final-acum_hydbal IS NOT INITIAL
      AND pv_found EQ abap_false.
        cwa_final-cfield_cap = cwa_final-acum_hydbal.
      ELSE.
        cwa_final-cfield_cap = cv_zzrefcty.
      ENDIF.
      IF gv_new_equnr EQ abap_true.
        IF cwa_final-hyd_bal IS INITIAL
        AND cwa_final-acum_hydbal IS INITIAL.
          CLEAR cwa_final-cfield_cap.
        ENDIF.
        gv_new_equnr = abap_false.
      ENDIF.
      IF gv_new_shift EQ abap_true.
        IF cwa_final-hyd_bal IS INITIAL
        AND cwa_final-acum_hydbal IS INITIAL.
          CLEAR cwa_final-cfield_cap.
        ENDIF.
        gv_new_shift = abap_false.
      ENDIF.
*--EOC-T_T.KONNO-102220
      IF lv_disp EQ abap_true.
        DATA(lv_counter) = lines( gt_final ).
        ADD 1 TO lv_counter.
        cwa_final-counter = lv_counter.
        APPEND cwa_final TO gt_final.
      ENDIF.

      CLEAR: cwa_final.
      MOVE-CORRESPONDING lwa_final TO cwa_final.

    ENDLOOP.
  ELSE.
    cwa_final-irrdate = pv_date.

*--Calculating Irrigation mm : Irrigation (m3) / Area (m2)] x 0,1
    CLEAR lv_garea.
    lv_garea = cwa_final-garea.

    IF lv_garea IS NOT INITIAL.
      cwa_final-irr_mm    = ( cwa_final-irr_m3 / lv_garea ) * '0.1'.
    ENDIF.

    IF cwa_final-irr_hr IS NOT INITIAL. "Start time – End time
      cwa_final-rate   = cwa_final-irr_m3 / cwa_final-irr_hr. "Irrigation (m3) / Irrigation Hours (h)
*--Irrigation per hour (mm/h)
      cwa_final-mm_h   = cwa_final-irr_mm / cwa_final-irr_hr. "Irrigation (mm) / Irrigated Time (h).
    ENDIF.

*--Filling Evaporation
    PERFORM measurement_attributes USING pv_date
                                         zcl_abs_abap_maintain=>c_class_evap "'EVAPORATION'
                                         pwa_ordhdr
                                         pt_glmdhdr
                                         pt_glmdatv
                                CHANGING cwa_final-evap
                                         lv_disp
                                         lv_atdesc.

*--Filling Rain(Chuvas)
    PERFORM measurement_attributes USING pv_date
                                         zcl_abs_abap_maintain=>c_class_prec "'PRECIPITATION'
                                         pwa_ordhdr
                                         pt_glmdhdr
                                         pt_glmdatv
                                CHANGING cwa_final-chuvas
                                         lv_disp
                                         lv_atdesc.

*--Filling Nonirrigation reason
    PERFORM measurement_attributes USING pv_date
                                         zcl_abs_abap_maintain=>c_class_nirr_rsn  "'MOTIVO_NAO_IRRIGAÇ'
                                         pwa_ordhdr
                                         pt_glmdhdr
                                         pt_glmdatv
                                CHANGING cwa_final-nonirr_reason
                                         lv_disp
                                         lv_atdesc.

*--Filling Non Irrigation Reason description
    IF cwa_final-nonirr_reason IS NOT INITIAL.
      cwa_final-nonirrrsn_desc = lv_atdesc.
    ENDIF.

    cwa_final-evaptrans = cwa_final-evap * cwa_final-ccoef.

*--Calculating Effective Rain
    IF ( 1 LE cwa_final-chuvas )
    AND ( cwa_final-chuvas LE 10 ).
      cwa_final-effchuva = 0.
    ELSEIF ( '10.1' LE cwa_final-chuvas )
       AND ( cwa_final-chuvas LE 60 ).
      cwa_final-effchuva = cwa_final-chuvas * '0.7'.
    ELSEIF ( cwa_final-chuvas GT 60 ).
      cwa_final-effchuva = 45.
    ENDIF.

*--Calculating Hydric Balance
    PERFORM hydric_bal USING pv_date
                             pwa_ordhdr
                             pt_irrmon
                    CHANGING cwa_final.

*--Current Field Capacity: Reference Field Capacity (RFC) + Accum. Hydric Balance (mm)
    cv_zzrefcty = cv_zzrefcty + cwa_final-hyd_bal.
*--BOC-T_T.KONNO-102220
    IF cwa_final-acum_hydbal IS NOT INITIAL
    AND pv_found EQ abap_false. "05.11.20
      cwa_final-cfield_cap = cwa_final-acum_hydbal.
    ELSE.
      cwa_final-cfield_cap = cv_zzrefcty.
    ENDIF.
    IF gv_new_equnr EQ abap_true.
      IF cwa_final-hyd_bal IS INITIAL
      AND cwa_final-acum_hydbal IS INITIAL.
        CLEAR cwa_final-cfield_cap.
      ENDIF.
      gv_new_equnr = abap_false.
    ENDIF.
    IF gv_new_shift EQ abap_true.
      IF cwa_final-hyd_bal IS INITIAL
      AND cwa_final-acum_hydbal IS INITIAL.
        CLEAR cwa_final-cfield_cap.
      ENDIF.
      gv_new_shift = abap_false.
    ENDIF.
*--EOC-T_T.KONNO-102220
    IF lv_disp EQ abap_true.
      lv_counter = lines( gt_final ).
      ADD 1 TO lv_counter.
      cwa_final-counter = lv_counter.
      APPEND cwa_final TO gt_final.
    ENDIF.

    CLEAR: cwa_final.
    MOVE-CORRESPONDING lwa_final TO cwa_final.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form MEASUREMENT_ATTRIBUTES
*&---------------------------------------------------------------------*
*& MEASUREMENT_ATTRIBUTES
*&---------------------------------------------------------------------*
FORM measurement_attributes  USING    pv_date    TYPE p0001-begda
                                      pv_class   TYPE /agri/gatgrp
                                      pwa_ordhdr TYPE ty_ordhdr
                                      pt_glmdhdr TYPE tty_glmdhdr
                                      pt_glmdatv TYPE tty_glmdatv
                             CHANGING cv_value
                                      cv_disp
                                      cv_atdesc  TYPE atwtb.

  DATA : lv_atflv TYPE p DECIMALS 3.

*--Evaporation values data
  READ TABLE pt_glmdhdr INTO DATA(lwa_glmdhdr)
                      WITH KEY class = pv_class
                               mdate = pv_date

                      BINARY SEARCH.
  IF sy-subrc = 0.

    DATA(lv_tabix) = sy-tabix.

    READ TABLE pt_glmdatv INTO DATA(lwa_glmdatv)
    WITH KEY mdocm = lwa_glmdhdr-mdocm.
    IF sy-subrc = 0.
      cv_disp = abap_true.

      IF lwa_glmdatv-atwrt IS NOT INITIAL.

        IF lwa_glmdatv-atnam EQ 'PRECIPITACION'
        OR lwa_glmdatv-atnam EQ 'EVAPORATION'
        OR lwa_glmdatv-atwrt EQ '0,000'.
          TRANSLATE lwa_glmdatv-atwrt USING ',.'.
        ENDIF.

        cv_value = lwa_glmdatv-atwrt.

      ELSEIF lwa_glmdatv-atflv IS NOT INITIAL.

        lv_atflv = lwa_glmdatv-atflv.
        cv_value =  lv_atflv.

      ENDIF.

      READ TABLE gt_atval_desc INTO DATA(lwa_atval_desc)
      WITH KEY atinn = lwa_glmdatv-atinn
               atwrt = lwa_glmdatv-atwrt
               BINARY SEARCH.
      IF sy-subrc = 0.
        cv_atdesc = lwa_atval_desc-atwtb.
      ENDIF. "gt_atval_desc

    ENDIF. "pt_glmdatv

  ENDIF. "pt_glmdhdr

ENDFORM.

*&---------------------------------------------------------------------*
*& Form HYDRIC_BAL
*&---------------------------------------------------------------------*
*& HYDRIC_BAL
*&---------------------------------------------------------------------*
FORM hydric_bal USING pv_date    TYPE p0001-begda
                      pwa_ordhdr TYPE ty_ordhdr
                      pt_irrmon  TYPE tty_irrmon
             CHANGING cwa_final  TYPE zabs_str_irr_monitor_fcat.

*--Local declarations
  DATA : lv_days     TYPE t5a4a-dlydy,
         lv_prevdate TYPE p0001-begda.

*--Calculating Hydric balance Rain effective (mm) + Irrigation (mm) - Evapotranspiration (mm)
  cwa_final-hyd_bal = cwa_final-effchuva + cwa_final-irr_mm - cwa_final-evaptrans.
  cwa_final-acum_hydbal = cwa_final-hyd_bal.

  DATA(lt_final_aux) = gt_final[].
  SORT lt_final_aux BY counter DESCENDING.

  READ TABLE lt_final_aux INTO DATA(lwa_final)
    WITH KEY werks = p_werks
             equnr = cwa_final-equnr
             shift = cwa_final-shift.

  IF sy-subrc EQ 0.
    cwa_final-acum_hydbal = lwa_final-acum_hydbal + cwa_final-hyd_bal.
  ELSE.
    IF lt_final_aux[] IS INITIAL.
      cwa_final-acum_hydbal = lwa_final-acum_hydbal + cwa_final-hyd_bal.
    ENDIF.
*--If record doesn't exist in current execution, Check for ZABST_IRRMON table
    READ TABLE pt_irrmon INTO DATA(lwa_irrmon)
      WITH KEY werks   = p_werks
               equnr   = cwa_final-equnr
               shift   = cwa_final-shift BINARY SEARCH.
    IF sy-subrc = 0.
      lv_days = lv_days + 1.

      CLEAR lv_prevdate.
*--FM to get previous date of Irrigation(mm)
      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          date      = pv_date
          days      = lv_days
          months    = 0
          signum    = '-'
          years     = 0
        IMPORTING
          calc_date = lv_prevdate.

*--Checking for previous date based on order number
      DATA(lv_found) = abap_false.
      READ TABLE pt_irrmon INTO lwa_irrmon
        WITH KEY werks   = p_werks
                 equnr   = cwa_final-equnr
                 shift   = cwa_final-shift
                 irrdate = lv_prevdate
                 ordno   = cwa_final-ordno BINARY SEARCH.
      IF sy-subrc <> 0.
*--Checking for previous date without order number
        READ TABLE pt_irrmon INTO lwa_irrmon
          WITH KEY werks   = p_werks
                   equnr   = cwa_final-equnr
                   shift   = cwa_final-shift
                   irrdate = lv_prevdate BINARY SEARCH.
        IF sy-subrc EQ 0.
          lv_found = abap_true.
        ENDIF.
      ELSE.
        lv_found = abap_true.
      ENDIF.

      IF lv_found EQ abap_true.
        cwa_final-acum_hydbal = lwa_irrmon-acum_hydbal + cwa_final-hyd_bal.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FETCH_IRRMON
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fetch_irrmon.

*--Local declarations
  DATA: lt_irrmon TYPE tty_irrmon,
        lwa_final TYPE zabs_str_irr_monitor_fcat.

*--Fetching Custom table data(ZABST_IRRMON)
  SELECT *
    FROM zabst_irrmon
    INTO TABLE @lt_irrmon
   WHERE werks   EQ @p_werks
     AND equnr   IN @so_irr[]
     AND irrdate IN @so_date[]
     AND shift   IN @so_shift[].

  REFRESH gt_final.
  LOOP AT lt_irrmon INTO DATA(ls_irrmon).
    MOVE-CORRESPONDING ls_irrmon TO lwa_final.
    APPEND lwa_final TO gt_final.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SHIFT_AREA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_ORDHDR
*&      --> PT_TPLNR
*&      --> PT_GLFLOT
*&      --> PT_GLFLATV
*&      <-- LWA_FINAL
*&---------------------------------------------------------------------*
FORM shift_area USING pwa_ordhdr  TYPE ty_ordhdr
                      pt_tplnr    TYPE tty_tplnr
                      pt_zfmacitm TYPE tty_zfmacitm
                      pt_glflot   TYPE tty_glflot
                      pt_glflatv  TYPE tty_glflatv
             CHANGING cwa_final   TYPE zabs_str_irr_monitor_fcat.

*--Local declarations
  DATA : lv_garea   TYPE /agri/glgarea,
         lv_atflv   TYPE p DECIMALS 3,
         lv_fldname TYPE fieldname,
         lv_char1   TYPE char1,
         lt_dayint  TYPE TABLE OF pwsdayint.

  CASE cwa_final-shift.
    WHEN '0010'.
      lv_char1 = '1'.
    WHEN '0020'.
      lv_char1 = '2'.
    WHEN '0030'.
      lv_char1 = '3'.
    WHEN '0040'.
      lv_char1 = '4'.
    WHEN '0050'.
      lv_char1 = '5'.
    WHEN '0060'.
      lv_char1 = '6'.
  ENDCASE.

  lv_fldname = 'ZZCSHIFT' && lv_char1.

  READ TABLE pt_tplnr TRANSPORTING NO FIELDS
    WITH KEY equnr = pwa_ordhdr-equnr BINARY SEARCH.
  IF sy-subrc = 0.
    DATA(lv_tabix) = sy-tabix.
    LOOP AT pt_tplnr INTO DATA(lwa_tplnr) FROM lv_tabix.
      IF lwa_tplnr-equnr <> pwa_ordhdr-equnr.
        EXIT.
      ENDIF.

      ASSIGN COMPONENT lv_fldname OF STRUCTURE lwa_tplnr
        TO FIELD-SYMBOL(<lv_fldval>).
      IF sy-subrc NE 0.
        EXIT.
      ELSE.
        IF <lv_fldval> IS INITIAL.
          CONTINUE.
        ENDIF.
      ENDIF.

      READ TABLE pt_glflot INTO DATA(lwa_glflot)
        WITH KEY tplnr_fl = lwa_tplnr-tplnr_fl BINARY SEARCH.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      READ TABLE pt_zfmacitm INTO DATA(lwa_zfmacitm)
        WITH KEY tplnr_fl = lwa_tplnr-tplnr_fl BINARY SEARCH.
      IF sy-subrc <> 0.
        CLEAR lwa_zfmacitm.
      ENDIF.

*     lv_garea = lv_garea + ( lwa_glflot-garea * <lv_fldval> ) / 100.
      lv_garea = lv_garea + ( lwa_zfmacitm-aarea * <lv_fldval> ) / 100.

*      lv_garea = lv_garea + ( lwa_glflot-garea * lwa_tplnr-zzcshift1 ) / 100.
*
**-- Based on shifts
*      CASE cwa_final-shift.
*        WHEN c_shift1. "'0010'
*          lv_garea = lv_garea + ( lwa_glflot-garea * lwa_tplnr-zzcshift1 ) / 100.
*        WHEN c_shift2. "'0020'
*          lv_garea = lv_garea + ( lwa_glflot-garea * lwa_tplnr-zzcshift2 ) / 100.
*        WHEN c_shift3. "'0030'
*          lv_garea = lv_garea + ( lwa_glflot-garea * lwa_tplnr-zzcshift3 ) / 100.
*        WHEN c_shift4. "'0040'
*          lv_garea = lv_garea + ( lwa_glflot-garea * lwa_tplnr-zzcshift4 ) / 100.
*        WHEN c_shift5. "'0050
*          lv_garea = lv_garea + ( lwa_glflot-garea * lwa_tplnr-zzcshift5 ) / 100.
*        WHEN c_shift6. "'0060'
*          lv_garea = lv_garea + ( lwa_glflot-garea * lwa_tplnr-zzcshift6 ) / 100.
*      ENDCASE.

      READ TABLE pt_glflatv INTO DATA(lwa_glflatv)
        WITH KEY tplnr_fl = lwa_glflot-tplnr_fl BINARY SEARCH.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      IF lwa_glflatv-atwrt IS NOT INITIAL.
        cwa_final-ccoef = lwa_glflatv-atwrt.
      ELSEIF lwa_glflatv-atflv IS NOT INITIAL.
        lv_atflv = lwa_glflatv-atflv.
        cwa_final-ccoef =  lv_atflv.
      ENDIF.
    ENDLOOP.
  ENDIF.

  cwa_final-garea = lv_garea.

ENDFORM.
