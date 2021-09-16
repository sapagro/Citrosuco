************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name       :  ZABS_REP_IRRORD_INFO_SUB                        *
* Tcode             :  ZABS_TRN_IRRORD_INFO                            *
* Created By        :  Chandrakanth Karanam                            *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  07.04.2020                                      *
* TR                :                                                  *
* Version           :  001                                             *
* Description       :  Irrigation Order Information                    *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*----------------------------------------------------------------------*
*& CLASS lcl_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
*& lcl_event_handler
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_hotspot_click.

    DATA: lwa_final LIKE LINE OF gt_final,
          lv_wonum  TYPE /agri/fmwonum.

    READ TABLE gt_final INTO lwa_final INDEX e_row_id-index.
    IF sy-subrc = 0 .
      IF ( ( e_column_id-fieldname = 'FOR_ORDER'
        AND lwa_final-for_order IS NOT  INITIAL )
      OR ( e_column_id-fieldname = 'MANT_ORDER'
        AND lwa_final-mant_order IS NOT  INITIAL ) ).

        IF lwa_final-for_order IS NOT INITIAL.
          lv_wonum = lwa_final-for_order.
        ELSE.
          lv_wonum = lwa_final-mant_order.
        ENDIF.

*--Work Order Workbentch Navigation
        PERFORM workorder_wb USING lv_wonum.

      ENDIF. "e_column_id-fieldname
    ENDIF. "gt_final

  ENDMETHOD.                    "on_hotspot_click

ENDCLASS.

*&---------------------------------------------------------------------*
*& Form  workorder_wb
*&---------------------------------------------------------------------*
*& workorder_wb
*----------------------------------------------------------------------*
FORM workorder_wb USING lv_wonum TYPE /agri/fmwonum.

*--Local declarations
  DATA: lt_wonum  TYPE /agri/t_fmwonum,
        lwa_wonum TYPE /agri/s_fmwonum.

  REFRESH: lt_wonum.

  lwa_wonum-wonum = lv_wonum.
  APPEND lwa_wonum TO lt_wonum.

  CALL FUNCTION '/AGRI/FMWO_PROCESS'
    EXPORTING
      it_wonum                      = lt_wonum
    EXCEPTIONS
      invalid_parameter_combination = 1
      enter_work_order              = 2
      no_documents_to_process       = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.                    " ORDER_DISPLAY

*&---------------------------------------------------------------------*
*& Form REFRESH_GLOBAL_DATA
*&---------------------------------------------------------------------*
*& REFRESH_GLOBAL_DATA
*&---------------------------------------------------------------------*
FORM refresh_global_data.
  REFRESH : gt_final,
            gt_shift.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form F4_FOR_SHIFT
*&---------------------------------------------------------------------*
*& F4_FOR_SHIFT
*&---------------------------------------------------------------------*
FORM f4_for_shift CHANGING cv_shift TYPE vornr.

  DATA : lt_return  TYPE STANDARD TABLE OF ddshretval.

  IF gt_shift IS INITIAL AND p_werks IS NOT INITIAL.
    PERFORM shift_f4 CHANGING gt_shift.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'VORNR'
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      value_org       = zcl_abs_abap_maintain=>c_f4_valorg_s "'S'
    TABLES
      value_tab       = gt_shift
      return_tab      = lt_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc = 0.
    READ TABLE lt_return INTO DATA(lwa_return) INDEX 1.
    IF sy-subrc = 0.
      cv_shift = lwa_return-fieldval.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SELECTION_VALIDATIONS
*&---------------------------------------------------------------------*
*& SELECTION_VALIDATIONS
*&---------------------------------------------------------------------*
FORM selection_validations.

  IF gt_shift IS INITIAL AND p_werks IS NOT INITIAL.
    PERFORM shift_f4 CHANGING gt_shift.
  ENDIF.

  IF so_shift-low IS NOT INITIAL.
*--Validating Shift field
    READ TABLE gt_shift INTO DATA(lwa_shift)
    WITH KEY vornr = so_shift-low.
    IF sy-subrc <> 0.
      MESSAGE TEXT-003 TYPE zcl_abs_abap_maintain=>c_msgty_error. "'E'
    ENDIF.
  ENDIF.

  IF so_shift-high IS NOT INITIAL.
    READ TABLE gt_shift INTO lwa_shift
    WITH KEY vornr = so_shift-high.
    IF sy-subrc <> 0.
      MESSAGE TEXT-006 TYPE zcl_abs_abap_maintain=>c_msgty_error. "'E'
    ENDIF.
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
      MESSAGE TEXT-001 TYPE zcl_abs_abap_maintain=>c_msgty_error. "'E'
    ENDIF.
  ENDIF.

*--Validating Equipment
  IF p_equnr IS NOT INITIAL.
    SELECT SINGLE equnr
      FROM /agri/fmirhdr
      INTO @DATA(lv_equnr)
     WHERE equnr = @p_equnr.
    IF sy-subrc <> 0.
      MESSAGE TEXT-002 TYPE zcl_abs_abap_maintain=>c_msgty_error. "'E'
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SHIFT_F4
*&---------------------------------------------------------------------*
*& SHIFT_F4
*&---------------------------------------------------------------------*
FORM shift_f4  CHANGING ct_shift TYPE tty_shift.

*--Local declarations
  DATA : lwa_shift  TYPE ty_shift,
         lrt_tmatnr TYPE RANGE OF matnr,
         lrs_tmatnr LIKE LINE  OF lrt_tmatnr.

*--Get Process/Task Materials from Variant table
  CALL METHOD zcl_abs_get_variants=>get_constant_multiple
    EXPORTING
      iv_mod       = space
      iv_objid     = zcl_abs_abap_maintain=>c_objid_irr_wb "'IRWO'
      iv_k1val     = zcl_abs_abap_maintain=>c_key_irr_po "'IRPO'
    IMPORTING
      et_constants = DATA(lt_constants).

  lrs_tmatnr-sign = zcl_abs_abap_maintain=>c_rsign_include. "'I'
  lrs_tmatnr-option = zcl_abs_abap_maintain=>c_ropt_equal. "'EQ'
  LOOP AT lt_constants INTO DATA(lwa_constants).
    lrs_tmatnr-low = lwa_constants-cnval2.
    APPEND lrs_tmatnr TO lrt_tmatnr.
  ENDLOOP.

*--Fetching Assignment of Task Lists to Materials data
  SELECT plnty,
         plnnr,
         plnal
    FROM mapl
    INTO TABLE @DATA(lt_mapl)
   WHERE matnr IN @lrt_tmatnr
     AND werks EQ @p_werks
     AND loekz EQ @space.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  SORT lt_mapl BY plnty plnnr plnal.

*--Fetching Operations data
  SELECT b~vornr, b~ltxa1
    INTO TABLE @DATA(lt_plpo)
    FROM plas AS a
   INNER JOIN plpo AS b
      ON b~plnty = a~plnty
     AND b~plnnr = a~plnnr
     AND b~plnkn = a~plnkn
     AND b~zaehl = a~zaehl
     FOR ALL ENTRIES IN @lt_mapl
   WHERE a~plnty EQ @lt_mapl-plnty
     AND a~plnnr EQ @lt_mapl-plnnr
     AND a~plnal EQ @lt_mapl-plnal
     AND a~loekz EQ @space.
  IF sy-subrc = 0.
    SORT lt_plpo BY vornr.
  ENDIF.

  LOOP AT lt_plpo INTO DATA(lwa_plpo).
    lwa_shift-vornr = lwa_plpo-vornr.
    lwa_shift-ltxa1 = lwa_plpo-ltxa1.
    APPEND lwa_shift TO ct_shift.
    CLEAR lwa_shift.
  ENDLOOP. "lt_plpo

  SORT ct_shift BY vornr.
  DELETE ADJACENT DUPLICATES FROM ct_shift COMPARING vornr.

**--Fetching Assignment of Task Lists to Materials data
*  SELECT werks,
*         plnty,
*         plnnr,
*         plnal
*    FROM mapl
*    INTO TABLE @DATA(lt_mapl)
*   WHERE matnr IN @lrt_tmatnr.
*  IF sy-subrc <> 0.
*    RETURN.
*  ENDIF.
*
*  SORT lt_mapl BY plnty plnnr plnal.
*
*  IF lt_mapl IS NOT INITIAL.
**--Fetching Operations data
*    SELECT a~plnty, a~plnnr,
*           a~plnal, b~vornr,
*           b~ltxa1
*      INTO TABLE @DATA(lt_plpo)
*      FROM plas AS a
*     INNER JOIN plpo AS b
*        ON b~plnty = a~plnty
*       AND b~plnnr = a~plnnr
*       AND b~plnkn = a~plnkn
*       AND b~zaehl = a~zaehl
*       FOR ALL ENTRIES IN @lt_mapl
*     WHERE a~plnty EQ @lt_mapl-plnty
*       AND a~plnnr EQ @lt_mapl-plnnr
*       AND a~plnal EQ @lt_mapl-plnal
*       AND a~loekz EQ @space.
*    IF sy-subrc = 0.
*      SORT lt_plpo BY plnty plnnr plnal vornr.
*    ENDIF.
*  ENDIF. "lt_mapl
*
*  LOOP AT lt_mapl INTO DATA(lwa_mapl).
*
*    READ TABLE lt_plpo TRANSPORTING NO FIELDS
*                       WITH KEY plnty = lwa_mapl-plnty
*                                plnnr = lwa_mapl-plnnr
*                                plnal = lwa_mapl-plnal
*                       BINARY SEARCH.
*    IF sy-subrc <> 0.
*      CONTINUE.
*    ENDIF.
*
*    DATA(lv_tabix) = sy-tabix.
*    LOOP AT lt_plpo INTO DATA(lwa_plpo) FROM lv_tabix.
*
*      IF lwa_plpo-plnty <> lwa_mapl-plnty
*      OR lwa_plpo-plnnr <> lwa_mapl-plnnr
*      OR lwa_plpo-plnal <> lwa_mapl-plnal.
*        EXIT.
*      ENDIF.
*
*      lwa_shift-vornr  = lwa_plpo-vornr.
*      lwa_shift-ltxa1  = lwa_plpo-ltxa1.
*      APPEND lwa_shift TO ct_shift.
*      CLEAR lwa_shift.
*
*    ENDLOOP. "lt_plpo
*  ENDLOOP. "lt_mapl

ENDFORM.

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
*& Module  FCODE_PROCESSING  INPUT
*&---------------------------------------------------------------------*
*& FCODE_PROCESSING
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

*--Local variable declarations
  DATA: lv_tcode       TYPE sy-ucomm,
        lv_routine(32) TYPE c VALUE 'FCODE_'.

  lv_tcode = sy-ucomm.
  CLEAR ok_code.
  CONCATENATE lv_routine lv_tcode INTO lv_routine.
  PERFORM (lv_routine) IN PROGRAM (sy-repid) IF FOUND.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form  FCODE_BACK
*&---------------------------------------------------------------------*
*& FCODE_BACK
*----------------------------------------------------------------------*
FORM fcode_back.
  LEAVE TO SCREEN 0.
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

*--Local declarations
  DATA : lt_fcat    TYPE lvc_t_fcat,
         lwa_layout TYPE lvc_s_layo.

*--Create Object For Custom Container
  CREATE OBJECT gobj_cont
    EXPORTING
      container_name = c_irr_info_0100_cc.

*--Create Object for ALV Grid
  CREATE OBJECT gobj_alv
    EXPORTING
      i_parent = gobj_cont.

*--Preparing Field Catalog
  PERFORM field_catalog_prepare CHANGING lt_fcat.

*--Control Events Register
  PERFORM control_events_register.

*--Set ALV attributes FOR LAYOUT
  lwa_layout-cwidth_opt = abap_true.
  lwa_layout-zebra      = abap_true.
  lwa_layout-sel_mode   = zcl_abs_abap_maintain=>c_sel_mode_cell. "'A'

*--Displaying ALV Data
  IF gobj_alv IS NOT INITIAL.
    CALL METHOD gobj_alv->set_table_for_first_display
      EXPORTING
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
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form  CONTROL_EVENTS_REGISTER
*&---------------------------------------------------------------------*
*& CONTROL_EVENTS_REGISTER
*----------------------------------------------------------------------*
FORM control_events_register.

  IF ref_event_handler IS INITIAL.
    CREATE OBJECT ref_event_handler.
  ENDIF.

  SET HANDLER : ref_event_handler->on_hotspot_click
                FOR gobj_alv.

ENDFORM.                    "CONTROL_EVENTS_REGISTER_0100

*&---------------------------------------------------------------------*
*& Form FIELD_CATALOG_PREPARE
*&---------------------------------------------------------------------*
*& FIELD_CATALOG_PREPARE
*&---------------------------------------------------------------------*
FORM field_catalog_prepare  CHANGING lt_fcat TYPE lvc_t_fcat.

*--Preparing field catalog
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = zcl_abs_abap_maintain=>c_irr_info_fcat
    CHANGING
      ct_fieldcat            = lt_fcat
    EXCEPTIONS ##FM_SUBRC_OK
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

*--Setting Hotspot
  LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<lfs_fcat>)
    WHERE fieldname = 'FOR_ORDER'
       OR fieldname = 'MANT_ORDER'.
    <lfs_fcat>-hotspot = abap_true.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form BUILD_DATA
*&---------------------------------------------------------------------*
*& BUILD_DATA
*&---------------------------------------------------------------------*
FORM build_data.

*--Local declarations
  DATA : lwa_final  TYPE zabs_str_irrord_info,
         lv_tabix   TYPE sy-tabix,
         lv_datab   TYPE datum,
         lv_datbi   TYPE datum,
         lrt_tmatnr TYPE RANGE OF matnr,
         lrs_tmatnr LIKE LINE  OF lrt_tmatnr,
         lrt_datab  TYPE RANGE OF datum,
         lrs_datab  LIKE LINE OF lrt_datab.

*--Get Process/Task Materials from Variant table
  CALL METHOD zcl_abs_get_variants=>get_constant_multiple
    EXPORTING
      iv_mod       = space
      iv_objid     = zcl_abs_abap_maintain=>c_objid_irr_wb "'IRWO'
      iv_k1val     = zcl_abs_abap_maintain=>c_key_irr_po "'IRPO'
    IMPORTING
      et_constants = DATA(lt_constants).

  lrs_tmatnr-sign = zcl_abs_abap_maintain=>c_rsign_include. "'I'
  lrs_tmatnr-option = zcl_abs_abap_maintain=>c_ropt_equal. "'EQ'
  LOOP AT lt_constants INTO DATA(lwa_constants).
    lrs_tmatnr-low = lwa_constants-cnval2.
    APPEND lrs_tmatnr TO lrt_tmatnr.
  ENDLOOP.

  IF so_date-low IS NOT INITIAL.
    PERFORM get_week_dates USING so_date-low CHANGING lv_datab lv_datbi.
    lrs_datab-low  = lv_datab.
    lrs_datab-high = lv_datbi.
  ENDIF.

  IF so_date-high IS NOT INITIAL.
    PERFORM get_week_dates USING so_date-high CHANGING lv_datab lv_datbi.
    lrs_datab-high = lv_datbi.
  ENDIF.

  lrs_datab-sign = zcl_abs_abap_maintain=>c_rsign_include. "'I'
  lrs_datab-option = zcl_abs_abap_maintain=>c_ropt_between. "'BT'
  APPEND lrs_datab TO lrt_datab.

*--Fetching Order Header data
  SELECT ordno,
         equnr
    FROM zabst_ordhdr
    INTO TABLE @DATA(lt_ordhdr)
   WHERE werks EQ @p_werks
     AND equnr EQ @p_equnr
     AND datab IN @lrt_datab.
  IF sy-subrc <> 0.
    MESSAGE TEXT-005 TYPE zcl_abs_abap_maintain=>c_msgty_info. "'I'
    LEAVE LIST-PROCESSING.
  ENDIF.

*--Fetching Equipment header data
  SELECT equnr,
         descr
    FROM /agri/fmirhdrt
    INTO TABLE @DATA(lt_descr)
     FOR ALL ENTRIES IN @lt_ordhdr
   WHERE equnr EQ @lt_ordhdr-equnr
     AND spras EQ @sy-langu.
  IF sy-subrc = 0.
    SORT lt_descr BY equnr.
  ENDIF.

*--Fetching Work Order Numbers
  SELECT ordno,
         wonum,
         matnr
    FROM zabst_orditm
    INTO TABLE @DATA(lt_orditm)
     FOR ALL ENTRIES IN @lt_ordhdr
   WHERE ordno EQ @lt_ordhdr-ordno
     AND matnr IN @lrt_tmatnr.
  IF sy-subrc = 0.
    SORT lt_orditm BY ordno wonum.
  ENDIF.

*--Fetching Order Confirmation data
  SELECT ordno,
         contr,
         vornr,
         cdate,
         gamng
    FROM zabst_ordcnf
    INTO TABLE @DATA(lt_ordcnf)
    FOR ALL ENTRIES IN @lt_ordhdr
     WHERE ordno EQ @lt_ordhdr-ordno
       AND vornr IN @so_shift
       AND cdate IN @so_date.
  IF sy-subrc = 0.
    SORT lt_ordcnf BY ordno vornr cdate
                      contr DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_ordcnf COMPARING ordno vornr cdate.
  ENDIF.

*--Processing above data
  LOOP AT lt_ordhdr INTO DATA(lwa_ordhdr).

    CLEAR : lwa_final.
*--Euipment Description
    READ TABLE lt_descr INTO DATA(lwa_descr)
                        WITH KEY equnr = lwa_ordhdr-equnr
                        BINARY SEARCH.
    IF sy-subrc = 0.
      lwa_final-edescr  = lwa_descr-descr.
    ENDIF. "lt_descr

*--Filling Work Order Numbers
    READ TABLE lt_orditm TRANSPORTING NO FIELDS
                          WITH KEY ordno = lwa_ordhdr-ordno
                          BINARY SEARCH.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    lv_tabix = sy-tabix.
    LOOP AT lt_orditm INTO DATA(lwa_orditm) FROM lv_tabix.

      IF lwa_orditm-ordno <> lwa_ordhdr-ordno.
        EXIT.
      ENDIF.

      lwa_final-ordno = lwa_ordhdr-ordno.
      lwa_final-equnr = lwa_ordhdr-equnr.

      IF lwa_orditm-matnr = zcl_abs_abap_maintain=>c_tmatnr_ford. "'TFOR0350'
        lwa_final-for_order = lwa_orditm-wonum.
      ELSE.
        lwa_final-mant_order = lwa_orditm-wonum.
      ENDIF.

    ENDLOOP. "lt_orditm

*--Filling Shift and Shift Area
    READ TABLE lt_ordcnf TRANSPORTING NO FIELDS
                          WITH KEY ordno = lwa_ordhdr-ordno
                          BINARY SEARCH.
    IF sy-subrc <> 0.
      APPEND lwa_final TO gt_final.
      CLEAR lwa_final.
*      CONTINUE.
    ENDIF.

    lv_tabix = sy-tabix.
    LOOP AT lt_ordcnf INTO DATA(lwa_ordcnf) FROM lv_tabix.

      IF lwa_ordcnf-ordno <> lwa_ordhdr-ordno.
        EXIT.
      ENDIF.

      lwa_final-shift = lwa_ordcnf-vornr.
      lwa_final-sarea = lwa_ordcnf-gamng.
      lwa_final-cdate = lwa_ordcnf-cdate.

      APPEND lwa_final TO gt_final.
      CLEAR : lwa_final-sarea,
              lwa_final-shift,
              lwa_final-cdate.

    ENDLOOP. "lt_ordcnf
  ENDLOOP. "lt_ordhdr

ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_WEEK_DATES
*&---------------------------------------------------------------------*
*& GET_WEEK_DATES
*&---------------------------------------------------------------------*
FORM get_week_dates  USING iv_date  TYPE datum
                  CHANGING cv_datab TYPE datum
                           cv_datbi TYPE datum.

  CALL FUNCTION 'GET_WEEK_INFO_BASED_ON_DATE'
    EXPORTING
      date   = iv_date
    IMPORTING
      monday = cv_datab
      sunday = cv_datbi.

ENDFORM.
