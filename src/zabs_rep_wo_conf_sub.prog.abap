************************************************************************
** Confidential property of Citrosuco                                  *
** All Rights Reserved                                                 *
************************************************************************
**Report Name       :  ZABS_REP_WO_CONF_SUB                            *
**Tcode             :  ZABS_TRN_IORD_CNF                               *
**Created By        :  Chandrakanth Karanam                            *
**Requested by      :  Mario Alfredo                                   *
**Created on        :  02.25.2020                                      *
**TR                :                                                  *
**Version           :  001                                             *
**Description       :  Irrigation Monitor Data Preperation and Display *
**                     Data.                                           *
**---------------------------------------------------------------------*
** Modification Log:                                                   *
**---------------------------------------------------------------------*
**MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
**                                                                     *
**&-------------------------------------------------------------------&*
*
CLASS lcl_event_handler IMPLEMENTATION.

*--Handle user command.
  METHOD handle_user_command.

    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = e_ucomm.

  ENDMETHOD.                    "handle_user_command

*--Handle Data Changed
  METHOD handle_data_changed.

*--Calling handle data changed subroutine
    PERFORM handle_data_changed USING er_data_changed.

*--Set manual changes flag
    IF NOT er_data_changed->mt_mod_cells[] IS INITIAL.
      gs_variables-manual_changes = abap_true.
    ELSE.
      gs_variables-manual_changes = abap_false.
    ENDIF.

    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = zcl_abs_abap_maintain=>c_ucomm_enter. "'ENTR'

  ENDMETHOD.                    "handle_data_changed

ENDCLASS.

*&---------------------------------------------------------------------*
*& Form Operations_data
*&---------------------------------------------------------------------*
*& Operations_data
*&---------------------------------------------------------------------*
FORM operations_data CHANGING ct_shift TYPE tty_shift.

*--Local data declarations
  DATA : lwa_shift  TYPE ty_shift,
         lwa_wo_con TYPE zabs_str_ocon,
         lt_opr     TYPE tty_opr.

  IF gwa_order-orditm IS NOT INITIAL.

*--Fetching Operations(Shift numbers)
    SELECT wonum vornr ltxa1
      FROM /agri/fmwoopr
      INTO TABLE lt_opr
       FOR ALL ENTRIES IN gwa_order-orditm
     WHERE wonum EQ gwa_order-orditm-wonum.
    IF sy-subrc <> 0.
      MESSAGE TEXT-009 TYPE zcl_abs_abap_maintain=>c_msgty_info. "'I'
      LEAVE LIST-PROCESSING.
    ENDIF.

    SORT lt_opr BY vornr.
    DATA(lt_opr_temp) = lt_opr.
    DELETE ADJACENT DUPLICATES FROM lt_opr_temp COMPARING vornr.

    LOOP AT lt_opr_temp INTO DATA(lwa_opr).
      lwa_shift-vornr = lwa_opr-vornr.
      lwa_shift-ltxa1 = lwa_opr-ltxa1.
      APPEND lwa_shift TO ct_shift.
      CLEAR lwa_shift.
    ENDLOOP. "lt_opr_temp

*--Fetching components details to get materials
    SELECT a~wonum,
           a~vornr,
           a~contr,
           a~matnr,
           b~maktx,
           a~esmng,
           a~erfme
      INTO TABLE @DATA(lt_supply)
      FROM /agri/fmwocom AS a
     INNER JOIN makt AS b
        ON b~matnr = a~matnr
       FOR ALL ENTRIES IN @gwa_order-orditm
     WHERE a~wonum EQ @gwa_order-orditm-wonum.
    IF sy-subrc = 0.

      LOOP AT lt_supply INTO DATA(lwa_sup).

        lwa_wo_con-vornr = lwa_sup-vornr.
        lwa_wo_con-contr = lwa_sup-contr.
        lwa_wo_con-wonum = lwa_sup-wonum.
        lwa_wo_con-matnr = lwa_sup-matnr.
        lwa_wo_con-maktx = lwa_sup-maktx.
        lwa_wo_con-esmng = lwa_sup-esmng.
        lwa_wo_con-erfme = lwa_sup-erfme.

        APPEND lwa_wo_con TO gt_supply.
        CLEAR lwa_wo_con.
      ENDLOOP. "lt_supply

      SORT gt_supply BY vornr matnr.

    ENDIF.

  ENDIF. "gwa_order-orditm

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SELECTION_VALIDATIONS
*&---------------------------------------------------------------------*
*& SELECTION_VALIDATIONS
*&---------------------------------------------------------------------*
FORM selection_validations.

  IF sy-ucomm EQ 'XY'.

    CLEAR : p_equnr,
            p_werks,
            p_date,
            p_shift.

  ELSEIF sy-ucomm EQ zcl_abs_abap_maintain=>c_ucomm_onli "'ONLI'
     AND r_conf IS  NOT INITIAL
     AND ( p_equnr IS INITIAL
      OR p_werks IS INITIAL
      OR p_date  IS INITIAL ).

    MESSAGE TEXT-013 TYPE zcl_abs_abap_maintain=>c_msgty_error.

  ELSEIF sy-ucomm EQ zcl_abs_abap_maintain=>c_ucomm_onli "'ONLI'
     AND r_rev IS  NOT INITIAL
     AND ( p_equnr IS INITIAL
      OR p_werks IS INITIAL
      OR p_date  IS INITIAL
      OR p_shift IS INITIAL ).

    MESSAGE TEXT-013 TYPE zcl_abs_abap_maintain=>c_msgty_error.

  ENDIF.

*--Validating Form
  IF p_werks IS NOT INITIAL.

    SELECT werks
      FROM t001w
      INTO TABLE @DATA(lt_werks)
      WHERE werks EQ @p_werks.
    IF sy-subrc <> 0.
      MESSAGE TEXT-001 TYPE zcl_abs_abap_maintain=>c_msgty_error. "'E'
      LEAVE LIST-PROCESSING.
    ENDIF.

**--Fetching Equipment header and Equipment master plants data
*    SELECT a~equnr, b~werks
*      FROM /agri/fmirhdr AS a
*     INNER JOIN /agri/fmirwrk AS b
*        ON a~equnr = b~equnr
*      INTO TABLE @DATA(lt_fmirhdr)
*     WHERE
**      a~equnr EQ @p_equnr
*           a~irtyp EQ @zcl_abs_abap_maintain=>c_irrtyp "'001'
*       AND b~werks EQ @p_werks.
*    IF sy-subrc <> 0.
*      MESSAGE TEXT-001 TYPE zcl_abs_abap_maintain=>c_msgty_info. "'I'
*      LEAVE LIST-PROCESSING.
*    ENDIF.
  ENDIF. "p_werks

*--Validating Equipment
  IF p_equnr IS NOT INITIAL.
    SELECT SINGLE equnr
      FROM /agri/fmirhdr
      INTO @DATA(lv_equnr)
     WHERE equnr = @p_equnr.
    IF sy-subrc <> 0.
      MESSAGE TEXT-002 TYPE zcl_abs_abap_maintain=>c_msgty_error. "'E'
    ENDIF.
  ENDIF. "p_equnr

*--Validating Shift
  IF p_shift IS NOT INITIAL.

    LOOP AT gt_shift INTO DATA(lwa_shift) WHERE werks = p_werks
                                            AND equnr = p_equnr.
      DATA(lv_exit) = abap_true.
      EXIT.
    ENDLOOP. "gt_shift

    IF lv_exit IS INITIAL.
*--Fetching Shift data
      PERFORM shift_data.
    ENDIF.

    READ TABLE gt_shift TRANSPORTING NO FIELDS
     WITH KEY werks = p_werks
              equnr = p_equnr
              vornr = p_shift
              BINARY SEARCH.
    IF sy-subrc <> 0.
      MESSAGE TEXT-014 TYPE zcl_abs_abap_maintain=>c_msgty_error. "'E'
    ENDIF.

  ENDIF. "p_shift

ENDFORM.

*&---------------------------------------------------------------------*
*& Form BUILD_DATA
*&---------------------------------------------------------------------*
*& BUILD_DATA
*&---------------------------------------------------------------------*
FORM build_data.

*--Local declaration
  DATA : lv_prevdate TYPE p0001-begda,
         lv_cnval1   TYPE zabs_del_cnval,
         lv_days     TYPE t5a4a-dlydy,
         lv_pv_strdt TYPE datum,
         lv_pv_enddt TYPE datum,
         lv_cu_strdt TYPE datum,
         lv_cu_enddt TYPE datum,
         lwa_wo_cnf  TYPE zabs_str_ocnf,
         lwa_ordhdr  TYPE zabs_str_ordhdr.

*--Get variant table data
  CALL METHOD zcl_abs_get_variants=>get_constant_single
    EXPORTING
      iv_mod    = space
      iv_objid  = zcl_abs_abap_maintain=>c_objid_irr_wb "'IRWO'
      iv_k1val  = zcl_abs_abap_maintain=>c_key_irr_days "'DAYS'
    IMPORTING
      ev_cnval1 = lv_cnval1.

  lv_days = lv_cnval1.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      date      = p_date
      days      = lv_days
      months    = 0
      signum    = '-'
      years     = 0
    IMPORTING
      calc_date = lv_prevdate.

  IF lv_prevdate IS NOT INITIAL.
    PERFORM get_week_dates USING lv_prevdate CHANGING lv_pv_strdt lv_pv_enddt.
  ENDIF.

  IF p_date IS NOT INITIAL.
    PERFORM get_week_dates USING p_date CHANGING lv_cu_strdt lv_cu_enddt.
  ENDIF.

*--Fetching Order Numbers
  SELECT ordno, werks,
         equnr, datab,
         datbi, erzet
    FROM zabst_ordhdr
*    INTO CORRESPONDING FIELDS OF TABLE gwa_order-ordhdr
    INTO TABLE @DATA(lt_ordhdr)
   WHERE werks EQ @p_werks
     AND equnr EQ @p_equnr
     AND datab GE @lv_pv_strdt
     AND datbi LE @lv_cu_enddt.
  IF sy-subrc <> 0.
    MESSAGE TEXT-003 TYPE zcl_abs_abap_maintain=>c_msgty_info. "'I'
    LEAVE LIST-PROCESSING.
  ENDIF.

  LOOP AT lt_ordhdr INTO DATA(lwa_ordhdr_temp).

    IF lwa_ordhdr_temp-datab LE p_date
   AND lwa_ordhdr_temp-datbi GE p_date.
      MOVE-CORRESPONDING lwa_ordhdr_temp TO lwa_ordhdr.
      APPEND lwa_ordhdr TO gwa_order-ordhdr.
    ENDIF.

  ENDLOOP.

  IF lt_ordhdr IS NOT INITIAL.
    SELECT ordno
           contr
           fcontr_hm
      FROM zabst_ordcnf
      INTO TABLE gt_ordcnf
       FOR ALL ENTRIES IN lt_ordhdr
     WHERE ordno EQ lt_ordhdr-ordno.
    IF sy-subrc = 0.
      SORT gt_ordcnf BY contr DESCENDING.
    ENDIF.
  ENDIF.

*--Fetching Work Order Numbers
  SELECT ordno wonum
    FROM zabst_orditm
    INTO CORRESPONDING FIELDS OF TABLE gwa_order-orditm
     FOR ALL ENTRIES IN gwa_order-ordhdr
   WHERE ordno EQ gwa_order-ordhdr-ordno.
  IF sy-subrc = 0.
    SORT gwa_order-orditm BY wonum.
  ENDIF.

  IF r_conf IS NOT INITIAL.

*--Filling ALV 1 display fields
    READ TABLE gwa_order-ordhdr INTO lwa_ordhdr INDEX 1.
    IF sy-subrc = 0.
      lwa_wo_cnf-ordno = lwa_ordhdr-ordno.
      lwa_wo_cnf-werks = lwa_ordhdr-werks.
      lwa_wo_cnf-equnr = lwa_ordhdr-equnr.
      lwa_wo_cnf-cdate = p_date. "lwa_ordhdr-odate.
      lwa_wo_cnf-idate = lwa_wo_cnf-cdate.
      lwa_wo_cnf-fdate = lwa_wo_cnf-cdate.
      lwa_wo_cnf-irrqty_uom = zcl_abs_abap_maintain=>c_uom. "'M3'
      APPEND lwa_wo_cnf TO gt_wo_cnf.
      CLEAR lwa_wo_cnf.
    ENDIF.

  ELSEIF r_rev IS NOT INITIAL.

    PERFORM build_revdata USING gwa_order.

  ENDIF. "r_conf

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_DATA
*&---------------------------------------------------------------------*
*& DISPLAY_DATA
*&---------------------------------------------------------------------*
FORM display_data.
  CALL SCREEN 0100.
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

  IF r_conf IS NOT INITIAL.
    SET PF-STATUS 'S100'.
  ELSEIF r_rev IS NOT INITIAL.
    SET PF-STATUS 'S200'.
  ENDIF.

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
*& Module FCODE_PROCESSING  INPUT
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
*& Form  FCODE_ENTER
*&---------------------------------------------------------------------*
*& FCODE_ENTER
*----------------------------------------------------------------------*
FORM fcode_entr.

*--Local declaartions
  DATA : lv_tabix     TYPE sy-tabix,
         lv_contr     TYPE i,
         lv_tqty      TYPE gamng,
         lv_sup_qty   TYPE /agri/fmlmnga,
         lv_cqty      TYPE /agri/fmlmnga,
         lwa_wo_con   TYPE zabs_str_ocon,
         lwa_supply   TYPE zabs_str_ocon,
         lwa_fmfpcom  TYPE /agri/s_fmfpcom,
         lwa_fmfpcnf  TYPE /agri/s_fmfp_cnf,
         lwa_post     TYPE ty_post,
         lwa_comsplit TYPE ty_comsplit,
         lwa_task     TYPE ty_task,
         lt_fmfpcom   TYPE /agri/t_fmfpcom,
         lt_fmfpcnf   TYPE /agri/t_fmfp_cnf.

  REFRESH : gt_task,
            gt_comsplit.

  IF gt_woitm   IS INITIAL
  OR gt_fmfphdr IS INITIAL
  OR gt_fmirflo IS INITIAL.
*-- Fetch global data to display on enter
    PERFORM glob_data_entr.
  ENDIF.

  IF gt_shift IS INITIAL.
*--Fetching Operations data
    PERFORM operations_data CHANGING gt_shift.
  ENDIF. "gt_shift

  READ TABLE gt_wo_cnf ASSIGNING FIELD-SYMBOL(<fs_wo_cnf>) INDEX 1.
  IF sy-subrc = 0.

    READ TABLE gt_supply TRANSPORTING NO FIELDS
                         WITH KEY vornr = <fs_wo_cnf>-vornr
                         BINARY SEARCH.
    IF sy-subrc = 0.
      lv_tabix = sy-tabix.

      READ TABLE gwa_order-orditm INTO DATA(lwa_ordhdr) INDEX 1.

      REFRESH gt_wo_con.
      LOOP AT gt_supply INTO lwa_supply FROM lv_tabix.
        IF lwa_supply-vornr <> <fs_wo_cnf>-vornr.
          EXIT.
        ENDIF.

        lwa_wo_con-vornr   = lwa_supply-vornr.
        lwa_wo_con-contr   = lwa_supply-contr.
        lwa_wo_con-ordno   = lwa_ordhdr-ordno.
        lwa_wo_con-matnr   = lwa_supply-matnr.
        lwa_wo_con-maktx   = lwa_supply-maktx.
        lwa_wo_con-erfme   = lwa_supply-erfme.

        COLLECT lwa_wo_con INTO gt_wo_con.
        CLEAR lwa_wo_con.

      ENDLOOP. " gt_supply

    ENDIF. "gt_supply

*--To fill Target Quantity based on Operation
    LOOP AT gwa_order-orditm INTO lwa_ordhdr.

      READ TABLE gt_woitm TRANSPORTING NO FIELDS
                          WITH KEY wonum = lwa_ordhdr-wonum
                          BINARY SEARCH.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      lv_tabix = sy-tabix.
      LOOP AT gt_woitm INTO DATA(lwa_woitm) FROM lv_tabix.

        IF lwa_woitm-wonum <> lwa_ordhdr-wonum.
          EXIT.
        ENDIF.

        READ TABLE gt_fmfphdr INTO DATA(lwa_fmfphdr)
        WITH KEY aufnr = lwa_woitm-aufnr
        BINARY SEARCH.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

*-- Calculate confirmation quantity
        READ TABLE gt_fmirflo INTO DATA(lwa_fmirflo)
                              WITH KEY equnr    = p_equnr
                                       tplnr_fl = lwa_woitm-tplnr_fl
                              BINARY SEARCH.
        IF sy-subrc IS NOT INITIAL.
          CONTINUE.
        ENDIF.

*-- Based on shifts
        CASE <fs_wo_cnf>-vornr.
          WHEN c_shift1. "'0010'
            CLEAR lv_cqty.
            lv_cqty =  lwa_fmfphdr-gamng * ( lwa_fmirflo-zzcshift1 / 100 ).
            lv_sup_qty =  lv_sup_qty + lv_cqty.
            lwa_task-shift = lwa_fmirflo-zzcshift1.

          WHEN c_shift2. "'0020'
            CLEAR lv_cqty.
            lv_cqty =  lwa_fmfphdr-gamng * ( lwa_fmirflo-zzcshift2 / 100 ).
            lv_sup_qty =  lv_sup_qty + lv_cqty.
            lwa_task-shift = lwa_fmirflo-zzcshift2.

          WHEN c_shift3. "'0030'
            CLEAR lv_cqty.
            lv_cqty =  lwa_fmfphdr-gamng * ( lwa_fmirflo-zzcshift3 / 100 ).
            lv_sup_qty =  lv_sup_qty + lv_cqty.
            lwa_task-shift = lwa_fmirflo-zzcshift3.

          WHEN c_shift4. "'0040'
            CLEAR lv_cqty.
            lv_cqty =  lwa_fmfphdr-gamng * ( lwa_fmirflo-zzcshift4 / 100 ).
            lv_sup_qty =  lv_sup_qty + lv_cqty.
            lwa_task-shift = lwa_fmirflo-zzcshift4.

          WHEN c_shift5. "'0050'
            CLEAR lv_cqty.
            lv_cqty =  lwa_fmfphdr-gamng * ( lwa_fmirflo-zzcshift5 / 100 ).
            lv_sup_qty =  lv_sup_qty + lv_cqty.
            lwa_task-shift = lwa_fmirflo-zzcshift5.

          WHEN c_shift6. "'0060'
            CLEAR lv_cqty.
            lv_cqty =  lwa_fmfphdr-gamng * ( lwa_fmirflo-zzcshift6 / 100 ).
            lv_sup_qty =  lv_sup_qty + lv_cqty.
*            <fs_wo_cnf>-lmnga = <fs_wo_cnf>-lmnga + lv_cqty.
            lwa_task-shift = lwa_fmirflo-zzcshift6.

        ENDCASE.

        IF lwa_task-shift IS NOT INITIAL.
          lv_tqty = lv_tqty + lwa_fmfphdr-gamng.
        ENDIF.

*-- Collect workorder level target and confirmation qty
        lwa_comsplit-wonum = lwa_ordhdr-wonum.
        lwa_comsplit-gamng = lwa_comsplit-gamng + lwa_fmfphdr-gamng.

*--Collecting based on Task Order
        lwa_task-wonum = lwa_ordhdr-wonum.
        lwa_task-aufnr = lwa_woitm-aufnr.
        lwa_task-gamng = lwa_fmfphdr-gamng.

        APPEND lwa_task TO gt_task.
        CLEAR lwa_task.

      ENDLOOP."lt_woitm

*-- Collecting individual work order data
      APPEND lwa_comsplit TO gt_comsplit.
      CLEAR lwa_comsplit.

    ENDLOOP."gwa_order-orditm

*--Filling Total Order Quantity
    <fs_wo_cnf>-gamng = lv_tqty.

*--Filling Confirmation Quantity
    IF <fs_wo_cnf>-lmnga IS INITIAL.
      <fs_wo_cnf>-lmnga = lv_sup_qty.
    ENDIF.

    <fs_wo_cnf>-gmein = lwa_fmfphdr-gmein.
    CLEAR lv_tqty.

*--Filling Initial Counter
    READ TABLE gt_ordcnf INTO DATA(lwa_ordcnf) INDEX 1.
    IF sy-subrc = 0 AND <fs_wo_cnf>-icontr_hm IS INITIAL.
      <fs_wo_cnf>-icontr_hm = lwa_ordcnf-fcontr_hm.
    ENDIF.

*--Calculate Total Irrigation Quantity
*--Fetching custom field for calculation
    SELECT equnr,
           irtyp,
           zzhydfac
      FROM /agri/fmirhdr
      INTO TABLE @DATA(lt_fmirhdr)
     WHERE equnr EQ @p_equnr
       AND irtyp EQ @zcl_abs_abap_maintain=>c_irrtyp. "'001'
    IF sy-subrc <> 0.
      MESSAGE TEXT-007 TYPE zcl_abs_abap_maintain=>c_msgty_info. "'I'
      LEAVE LIST-PROCESSING.
    ENDIF.

    READ TABLE lt_fmirhdr INTO DATA(lwa_fmirhdr)
    WITH KEY equnr =  <fs_wo_cnf>-equnr.
    IF sy-subrc = 0.
      <fs_wo_cnf>-tot_iqty = ( <fs_wo_cnf>-fcontr_hm - <fs_wo_cnf>-icontr_hm )
                              * lwa_fmirhdr-zzhydfac.
    ENDIF.

    READ TABLE gt_mod TRANSPORTING NO FIELDS WITH KEY row_id = 1
                                                      fieldname = 'LMNGA'.
    IF sy-subrc NE 0 OR <fs_wo_cnf>-lmnga IS INITIAL.
*      <fs_wo_cnf>-lmnga = lv_sup_qty.
    ENDIF.

    IF <fs_wo_cnf>-lmnga GT <fs_wo_cnf>-gamng.
      MESSAGE TEXT-010 TYPE zcl_abs_abap_maintain=>c_msgty_error. "'E'
    ENDIF.

  ENDIF. "gt_wo_cnf

*-- Update grid after entering data
  CALL METHOD gobj_alv_cnf->refresh_table_display.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form  FCODE_REVERSAL
*&---------------------------------------------------------------------*
*& FCODE_REVERSAL
*----------------------------------------------------------------------*
FORM fcode_reversal.

*--Local declaration
  DATA: lv_tabix  TYPE sy-tabix,
        lwa_ocnum TYPE /agri/s_fmocnum,
        lt_ocnum  TYPE /agri/t_fmocnum.

  CALL METHOD gobj_alv_cnf->get_selected_rows
    IMPORTING
      et_index_rows = DATA(lt_rows).

  IF lt_rows IS INITIAL.
    MESSAGE TEXT-012 TYPE zcl_abs_abap_maintain=>c_msgty_info.
    LEAVE LIST-PROCESSING.
  ENDIF.

  SORT gt_ordgrp BY ordno contr.

*--Processing selected rows for Reversal
  LOOP AT lt_rows INTO DATA(lwa_rows).

    READ TABLE gt_wo_cnf INTO DATA(lwa_wo_cnf) INDEX lwa_rows-index.
    IF sy-subrc = 0 AND lwa_wo_cnf-loevm EQ space.

      READ TABLE gt_ordgrp TRANSPORTING NO FIELDS
                            WITH KEY ordno = lwa_wo_cnf-ordno
                                     contr = lwa_wo_cnf-contr
                            BINARY SEARCH.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      lv_tabix = sy-tabix.
      LOOP AT gt_ordgrp INTO DATA(lwa_ordgrp) FROM lv_tabix.
        IF lwa_ordgrp-ordno <> lwa_ordgrp-ordno.
          EXIT.
        ENDIF.
        lwa_ocnum-ocnum = lwa_ordgrp-ocnum.
        APPEND lwa_ocnum TO lt_ocnum.
        CLEAR lwa_ocnum.
      ENDLOOP. "gt_ordgrp

    ENDIF. "gt_wo_cnf

  ENDLOOP. "lt_rows

  IF lt_ocnum IS NOT INITIAL.

    SORT lt_ocnum BY ocnum.
    DELETE ADJACENT DUPLICATES FROM lt_ocnum COMPARING ocnum.

*--Calling Subroutine for Reversal
    PERFORM reversal USING lt_ocnum
                  CHANGING gt_ordgrp
                           gt_wo_cnf.

  ENDIF. "lt_ocnum

*--Deleting Reversal records from ALV
  DELETE gt_wo_cnf WHERE loevm EQ abap_true.
  CALL METHOD gobj_alv_cnf->refresh_table_display.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form  FCODE_POST
*&---------------------------------------------------------------------*
*& FCODE_POST
*----------------------------------------------------------------------*
FORM fcode_post.

*--Local declarations
  DATA : lt_fpdoc        TYPE /agri/t_fmfp_doc,
         lt_messages     TYPE /agri/t_gprolog,
         lt_msgtab       TYPE esp1_message_tab_type,
         lt_fmfpcom      TYPE /agri/t_fmfpcom,
         lt_fmfpcnf      TYPE /agri/t_fmfp_cnf,
         lwa_fmfpcom     TYPE /agri/s_fmfpcom,
         lwa_fmfpcnf     TYPE /agri/s_fmfp_cnf,
         lwa_msgtab      TYPE esp1_message_wa_type,
         lwa_update      TYPE ty_update,
         lwa_cust_grp    TYPE zabs_str_ordgrp,
         lwa_cust_cnf    TYPE zabs_str_ordcnf,
         lwa_update_tmp  TYPE ty_update,
         lwa_cust_con    TYPE zabs_str_ordcon,
         lv_tabix        TYPE sy-tabix,
         lv_subrc        TYPE sy-subrc,
         lv_wtr_qty      TYPE ru_ismng,
         lv_contr        TYPE i,
         lv_per_quantity TYPE /agri/fmlmnga,
         lv_posnr        TYPE i.

  SORT gt_task BY wonum.

*-- Build tables for posting the confirmations
*--Splitting Usage quantity based on Shifts
  READ TABLE gt_wo_cnf INTO DATA(lwa_wo_cnf) INDEX 1.
  IF sy-subrc = 0.

    SELECT MAX( contr )
      FROM zabst_ordcnf
      INTO @DATA(lv_cont)
     WHERE ordno EQ @lwa_wo_cnf-ordno.
    IF sy-subrc = 0.
      lv_cont = lv_cont + 1.
    ENDIF.

*--Validating Selection Fields
    PERFORM validation USING lwa_wo_cnf
                    CHANGING lt_msgtab.

    IF lt_msgtab IS INITIAL.

      SORT : gt_comsplit BY wonum,
             gt_task BY wonum aufnr.

*--Calculating Confirmation Quantity by Task and Work Order
      LOOP AT gt_comsplit ASSIGNING FIELD-SYMBOL(<lfs_comsplit>).

        CLEAR <lfs_comsplit>-lmnga.
        READ TABLE gt_task TRANSPORTING NO FIELDS
                                WITH KEY wonum = <lfs_comsplit>-wonum
                                BINARY SEARCH.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
        DATA(lv_tabix_task) = sy-tabix.
        LOOP AT gt_task ASSIGNING FIELD-SYMBOL(<lfs_task>) FROM lv_tabix_task.
          IF <lfs_comsplit>-wonum <> <lfs_task>-wonum.
            EXIT.
          ENDIF.

          IF <lfs_task>-shift IS INITIAL.
            CONTINUE.
          ENDIF.

          CLEAR <lfs_task>-lmnga.
          IF lwa_wo_cnf-gamng IS NOT INITIAL.
*--Calculating Confirmation Quantity at Task Level
            <lfs_task>-lmnga = <lfs_task>-gamng * lwa_wo_cnf-lmnga / lwa_wo_cnf-gamng.
            <lfs_comsplit>-lmnga = <lfs_comsplit>-lmnga + <lfs_task>-lmnga.
          ENDIF.

        ENDLOOP. "gt_task

      ENDLOOP. "gt_comsplit

      CLEAR lwa_cust_cnf.
      lwa_cust_cnf-ordno = lwa_wo_cnf-ordno.
      lwa_cust_cnf-contr = lv_cont.
      lwa_cust_cnf-vornr = lwa_wo_cnf-vornr.
      lwa_cust_cnf-cdate = lwa_wo_cnf-cdate.
      lwa_cust_cnf-gamng = lwa_wo_cnf-gamng.
      lwa_cust_cnf-lmnga = lwa_wo_cnf-lmnga.
      lwa_cust_cnf-gmein = lwa_wo_cnf-gmein.
      lwa_cust_cnf-idate = lwa_wo_cnf-idate.
      lwa_cust_cnf-fdate = lwa_wo_cnf-fdate.
      lwa_cust_cnf-ihour = lwa_wo_cnf-ihour.
      lwa_cust_cnf-fhour = lwa_wo_cnf-fhour.
      lwa_cust_cnf-icontr_hm = lwa_wo_cnf-icontr_hm.
      lwa_cust_cnf-fcontr_hm = lwa_wo_cnf-fcontr_hm.
      lwa_cust_cnf-tot_iqty  = lwa_wo_cnf-tot_iqty.
      lwa_cust_cnf-uom       = lwa_wo_cnf-irrqty_uom.
      APPEND lwa_cust_cnf TO lwa_update-ordcnf.

      IF lwa_wo_cnf-lmnga GT lwa_wo_cnf-gamng.
        MESSAGE TEXT-010 TYPE zcl_abs_abap_maintain=>c_msgty_error. "'E'
      ENDIF.

      LOOP AT gt_comsplit INTO DATA(lwa_comsplit).

        READ TABLE gt_task TRANSPORTING NO FIELDS
                                WITH KEY wonum = lwa_comsplit-wonum
                                BINARY SEARCH.
        lv_tabix = sy-tabix.
        LOOP AT gt_task INTO DATA(lwa_task) FROM lv_tabix.
          IF lwa_task-wonum <> lwa_comsplit-wonum.
            EXIT.
          ENDIF.

          IF lwa_task-shift IS INITIAL.
            CONTINUE.
          ENDIF.

*--FMFPCNF Filling
          lwa_fmfpcnf-lmnga     = lwa_task-lmnga.
          lwa_fmfpcnf-meinh     = zcl_abs_abap_maintain=>c_meinh_har. "'HAR'   "Need to be changed
          lwa_fmfpcnf-arbpl     = zcl_abs_abap_maintain=>c_arbpl_agua. "'AGUA' "Need to be changed
          lwa_fmfpcnf-arbpl_ext = zcl_abs_abap_maintain=>c_arbpl_agua. "'AGUA' "Need to be changed
          lwa_fmfpcnf-umren     = zcl_abs_abap_maintain=>c_ordcnf_umren. "'1' "Need to be changed
          lwa_fmfpcnf-aufnr     = lwa_task-aufnr.
          lwa_fmfpcnf-vornr     = lwa_wo_cnf-vornr.
          lwa_fmfpcnf-budat     = p_date.
          lwa_fmfpcnf-gicre     = abap_true.

          CLEAR lv_wtr_qty.
          IF lwa_wo_cnf-lmnga IS NOT INITIAL.
            lv_wtr_qty = lwa_wo_cnf-tot_iqty * lwa_task-lmnga / lwa_wo_cnf-lmnga.
          ENDIF.

          lwa_fmfpcnf-ism01 = lv_wtr_qty.
          lwa_fmfpcnf-leinh1 = zcl_abs_abap_maintain=>c_uom. "'M3'

          CLEAR lwa_cust_grp.
          lv_posnr = lv_posnr + 1.
          lwa_cust_grp-ordno = lwa_wo_cnf-ordno.
          lwa_cust_grp-posnr = lv_posnr.
          lwa_cust_grp-contr = lv_cont.
          lwa_cust_grp-vornr = lwa_wo_cnf-vornr.
          lwa_cust_grp-wonum = lwa_comsplit-wonum.
          APPEND lwa_cust_grp TO lwa_update_tmp-ordgrp.

          APPEND lwa_fmfpcnf TO lt_fmfpcnf.
          CLEAR lwa_fmfpcnf.

          CLEAR lv_contr.
          READ TABLE gt_supply TRANSPORTING NO FIELDS
                              WITH KEY wonum = lwa_comsplit-wonum
                                       vornr = lwa_wo_cnf-vornr
                              BINARY SEARCH.
          lv_tabix = sy-tabix.
          LOOP AT gt_supply INTO DATA(lwa_supply) FROM lv_tabix.
            IF lwa_supply-wonum <> lwa_comsplit-wonum
            OR lwa_supply-vornr <> lwa_wo_cnf-vornr.
              EXIT.
            ENDIF.

            READ TABLE gt_wo_con INTO DATA(lwa_wo_con)
            WITH KEY matnr = lwa_supply-matnr.
            IF sy-subrc IS INITIAL
              AND lwa_wo_con-esmng IS INITIAL.
              CONTINUE.
            ENDIF.

*--FMFPCOM Filling
            lwa_fmfpcom-aufnr = lwa_task-aufnr.
            lwa_fmfpcom-contr = lwa_supply-contr.
            lwa_fmfpcom-vornr = lwa_supply-vornr.
            lwa_fmfpcom-matnr = lwa_supply-matnr.
            lwa_fmfpcom-erfmg = lwa_supply-esmng. "lwa_wo_con-esmng.
            lwa_fmfpcom-esmng = lwa_supply-esmng. "lwa_wo_con-esmng.
            IF lwa_wo_cnf-lmnga IS NOT INITIAL.
              lwa_fmfpcom-lmnga = ( lwa_task-lmnga * lwa_wo_con-esmng ) / lwa_wo_cnf-lmnga.
            ENDIF.

            lwa_fmfpcom-maktx = lwa_supply-maktx.
            lwa_fmfpcom-bwart = zcl_abs_abap_maintain=>c_movement_typ_gi_order. "'261'
            lwa_fmfpcom-werks = p_werks.
            lwa_fmfpcom-lgort = zcl_abs_abap_maintain=>c_verid_0001. "'0001'

            CLEAR lwa_cust_con.
            lwa_cust_con-ordno = lwa_wo_con-ordno.
            lwa_cust_con-contr = lv_cont.
            lwa_cust_con-supply = lwa_supply-matnr.
            lwa_cust_con-vornr = lwa_wo_con-vornr.
            lwa_cust_con-supply_qty = lwa_fmfpcom-lmnga.
            lwa_cust_con-uom = lwa_wo_con-erfme.
            COLLECT lwa_cust_con INTO lwa_update-ordcon ASSIGNING FIELD-SYMBOL(<lfs_con>).

            APPEND lwa_fmfpcom TO lt_fmfpcom.
            CLEAR lwa_fmfpcom.

          ENDLOOP.  "gt_supply
        ENDLOOP.  "gt_task

*--Calling FM for Posting
        CALL FUNCTION '/AGRI/FMFP_ORDER_CONFIRM'
          EXPORTING
            i_commit_work     = abap_true
            i_wonum           = lwa_comsplit-wonum
            it_fmfpcnf        = lt_fmfpcnf
            it_fmfpcom        = lt_fmfpcom
          IMPORTING
            e_subrc           = lv_subrc
            et_fpdoc          = lt_fpdoc
            et_messages       = lt_messages
          EXCEPTIONS
            inconsistent_data = 1
            OTHERS            = 2.
        IF lv_subrc = 0.
          ASSIGN ('(/AGRI/SAPLFMFPM)GS_FPOC_DOC-OCNUM') TO FIELD-SYMBOL(<fs_ocnum>).
          IF <fs_ocnum> IS ASSIGNED AND <fs_ocnum> IS NOT INITIAL.
            LOOP AT lwa_update_tmp-ordgrp ASSIGNING FIELD-SYMBOL(<fs_ordgrp>).
              <fs_ordgrp>-ocnum = <fs_ocnum>.
            ENDLOOP.
          ENDIF.

          APPEND LINES OF lwa_update_tmp-ordgrp TO lwa_update-ordgrp.

        ENDIF.

        LOOP AT lt_messages INTO DATA(lwa_messages).
          MOVE-CORRESPONDING lwa_messages TO lwa_msgtab.
          APPEND lwa_msgtab TO lt_msgtab.
          CLEAR lwa_msgtab.
        ENDLOOP.

        REFRESH : lt_fmfpcnf, lt_fmfpcom, lt_fpdoc, lt_messages.
        CLEAR : lv_subrc,
                lwa_update_tmp.

      ENDLOOP.  "gt_comsplit
    ENDIF.  "lt_msgtab

  ENDIF. "gt_wo_cnf

*--Updating Order Group table
  IF lwa_update-ordgrp IS NOT INITIAL.
    PERFORM table_update USING lwa_update.
  ENDIF.

  IF lt_msgtab IS NOT INITIAL.
*--Message display sub-routine
    PERFORM messages_display USING lt_msgtab.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form  table_update
*&---------------------------------------------------------------------*
*& table_update
*----------------------------------------------------------------------*
FORM table_update USING pwa_update TYPE ty_update.

*--Local declaration
  DATA : lt_messages TYPE esp1_message_tab_type,
         lt_msgtab   TYPE esp1_message_tab_type.

  IF pwa_update-ordcnf IS NOT INITIAL.

*--Fetching Order Confirmation data
    SELECT ordno,
           contr,
           vornr
      FROM zabst_ordcnf
      INTO TABLE @DATA(lt_cust_ordcnf)
       FOR ALL ENTRIES IN @pwa_update-ordcnf
     WHERE ordno EQ @pwa_update-ordcnf-ordno
       AND contr EQ @pwa_update-ordcnf-contr
       AND vornr EQ @pwa_update-ordcnf-vornr.
    IF sy-subrc = 0.
      SORT lt_cust_ordcnf BY ordno contr vornr.
    ENDIF.

  ENDIF.

  IF pwa_update-ordcon IS NOT INITIAL.

*--Fetching Order Components data
    SELECT ordno,
           contr,
           supply
      FROM zabst_ordcon
      INTO TABLE @DATA(lt_cust_ordcon)
       FOR ALL ENTRIES IN @pwa_update-ordcon
     WHERE ordno  EQ @pwa_update-ordcon-ordno
       AND contr  EQ @pwa_update-ordcon-contr
       AND supply EQ @pwa_update-ordcon-supply.
    IF sy-subrc = 0.
      SORT lt_cust_ordcon BY ordno contr supply.
    ENDIF.

  ENDIF.

  IF pwa_update-ordgrp IS NOT INITIAL.

*--Fetching Order Group Numbers
    SELECT ordno,
           contr,
           posnr
      FROM zabst_ordgrp
      INTO TABLE @DATA(lt_cust_ordgrp)
       FOR ALL ENTRIES IN @pwa_update-ordgrp
     WHERE ordno  EQ @pwa_update-ordgrp-ordno
       AND contr  EQ @pwa_update-ordgrp-contr
       AND posnr  EQ @pwa_update-ordgrp-posnr.
    IF sy-subrc = 0.
      SORT lt_cust_ordgrp BY ordno contr posnr.
    ENDIF.

  ENDIF.

  LOOP AT pwa_update-ordcnf ASSIGNING FIELD-SYMBOL(<lwa_update_ordcnf>).

    <lwa_update_ordcnf>-updkz = zcl_abs_abap_maintain=>c_updkz_insert. "'I'

    READ TABLE lt_cust_ordcnf INTO DATA(lwa_wo_cnf)
    WITH KEY ordno = <lwa_update_ordcnf>-ordno
             contr = <lwa_update_ordcnf>-contr
             vornr = <lwa_update_ordcnf>-vornr
      BINARY SEARCH.
    IF sy-subrc = 0.
      <lwa_update_ordcnf>-updkz = zcl_abs_abap_maintain=>c_updkz_update. "'U'
    ENDIF.

  ENDLOOP. "pwa_update-ordcnf

  LOOP AT pwa_update-ordcon ASSIGNING FIELD-SYMBOL(<lwa_update_ordcon>).

    <lwa_update_ordcon>-updkz = zcl_abs_abap_maintain=>c_updkz_insert. "'I'

    READ TABLE lt_cust_ordcon INTO DATA(lwa_wo_con)
    WITH KEY ordno  = <lwa_update_ordcon>-ordno
             contr  = <lwa_update_ordcon>-contr
             supply = <lwa_update_ordcon>-supply
      BINARY SEARCH.
    IF sy-subrc = 0.
      <lwa_update_ordcon>-updkz = zcl_abs_abap_maintain=>c_updkz_update. "'U'
    ENDIF.

  ENDLOOP. "pwa_update-ordcon

  LOOP AT pwa_update-ordgrp ASSIGNING FIELD-SYMBOL(<lwa_update_ordgrp>).

    <lwa_update_ordgrp>-updkz = zcl_abs_abap_maintain=>c_updkz_insert. "'I'

    READ TABLE lt_cust_ordgrp INTO DATA(lwa_wo_grp)
    WITH KEY ordno = <lwa_update_ordgrp>-ordno
             contr = <lwa_update_ordgrp>-contr
             posnr = <lwa_update_ordgrp>-posnr
      BINARY SEARCH.
    IF sy-subrc = 0.
      <lwa_update_ordgrp>-updkz = zcl_abs_abap_maintain=>c_updkz_update. "'U'
    ENDIF.

  ENDLOOP. "pwa_update-ordgrp

*--Calling FM to Update data into database table
  CALL FUNCTION 'ZABS_FM_ORDER_DBUPDATE'
    EXPORTING
      it_ordhdr   = gwa_order-ordhdr
      it_orditm   = gwa_order-orditm
      it_ordcnf   = pwa_update-ordcnf
      it_ordcon   = pwa_update-ordcon
      it_ordgrp   = pwa_update-ordgrp
    IMPORTING
      et_messages = lt_messages.

ENDFORM.

*&---------------------------------------------------------------------*
*& Module CONTROLS_DISPLAY OUTPUT
*&---------------------------------------------------------------------*
*& CONTROLS_DISPLAY
*&---------------------------------------------------------------------*
MODULE controls_display OUTPUT.
  PERFORM control_display.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form CONTROLS_EVENTS_REGISTER
*&---------------------------------------------------------------------*
*& CONTROLS_EVENTS_REGISTER
*&---------------------------------------------------------------------*
FORM controls_events_register.

*-- Local declarations
  DATA : lwa_dropdown TYPE lvc_s_dral,
         lt_dropdown  TYPE lvc_t_dral,
         lt_domainval TYPE TABLE OF dd07v.

  REFRESH lt_dropdown.

*-- Update drop down values from specific tables for certain
*   fields
*-- Vornr
  LOOP AT gt_shift ASSIGNING FIELD-SYMBOL(<lfs_shift>).
    CLEAR lwa_dropdown.
    lwa_dropdown-handle = zcl_abs_abap_maintain=>c_drpdown_handle1. "'1'
    CONCATENATE <lfs_shift>-vornr
                <lfs_shift>-ltxa1
           INTO lwa_dropdown-value
      SEPARATED BY space.

    lwa_dropdown-int_value = <lfs_shift>-vornr.
    APPEND lwa_dropdown TO lt_dropdown.
  ENDLOOP."ls_dropdown-vornr

*-- Set drop down to grid
  CALL METHOD gobj_alv_cnf->set_drop_down_table
    EXPORTING
      it_drop_down_alias = lt_dropdown.

*--Create an object for the local class
  CREATE OBJECT ref_event_handler.

  CALL METHOD gobj_alv_cnf->register_edit_event
    EXPORTING
      i_event_id = /irm/cl_gui_alv_grid=>mc_evt_enter
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.
  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

*-- Set handler for F4 help
  SET HANDLER : ref_event_handler->handle_user_command FOR gobj_alv_cnf,
                ref_event_handler->handle_data_changed FOR gobj_alv_cnf.
  IF gobj_alv_con IS NOT INITIAL.
    CALL METHOD gobj_alv_con->register_edit_event
      EXPORTING
        i_event_id = /irm/cl_gui_alv_grid=>mc_evt_enter
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    CALL METHOD gobj_alv_con->check_changed_data( ).

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FCAT_PREPARE
*&---------------------------------------------------------------------*
*& FCAT_PREPARE
*&---------------------------------------------------------------------*
FORM fcat_prepare  USING lv_structure
                CHANGING ct_fcat TYPE lvc_t_fcat.

  REFRESH ct_fcat.
*--Preparing field catalog
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = lv_structure
    CHANGING
      ct_fieldcat            = ct_fcat
    EXCEPTIONS ##FM_SUBRC_OK
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form ALV_DISPLAY
*&---------------------------------------------------------------------*
*& ALV_DISPLAY
*&---------------------------------------------------------------------*
FORM alv_display  USING    pwa_layout
                  CHANGING cobj_alv  TYPE REF TO cl_gui_alv_grid
                           ct_wo_cnf TYPE STANDARD TABLE
                           ct_fcat   TYPE lvc_t_fcat.

*--Local declaration
  DATA : ls_variant TYPE disvariant.

  ls_variant-handle = zcl_abs_abap_maintain=>c_variant_hndl_item. "'ITEM'
  CALL METHOD cobj_alv->set_table_for_first_display
    EXPORTING
      is_variant                    = ls_variant
      is_layout                     = pwa_layout
    CHANGING
      it_outtab                     = ct_wo_cnf
      it_fieldcatalog               = ct_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form GLOB_DATA_ENTR
*&---------------------------------------------------------------------*
FORM glob_data_entr.

  REFRESH : gt_woitm,
            gt_woitm,
            gt_fmirflo.

  IF gwa_order-orditm IS NOT INITIAL.

*--Fetching Work order items data to get task orders
    SELECT wonum
           aufnr
           tplnr_fl
      FROM /agri/fmwoitm
      INTO TABLE gt_woitm
       FOR ALL ENTRIES IN gwa_order-orditm
     WHERE wonum = gwa_order-orditm-wonum.
    IF sy-subrc <> 0.
      MESSAGE TEXT-005 TYPE zcl_abs_abap_maintain=>c_msgty_info. "'I'
      LEAVE LIST-PROCESSING.
    ENDIF.
    SORT gt_woitm BY wonum.

  ENDIF. "gwa_order-orditm

*--Fetching process order header data to get target quantity
  SELECT aufnr tplnr_fl
         gamng gmein
    FROM /agri/fmfphdr
    INTO TABLE gt_fmfphdr
     FOR ALL ENTRIES IN gt_woitm
   WHERE aufnr EQ gt_woitm-aufnr
     AND autyp EQ zcl_abs_abap_maintain=>c_autyp_tsk_ord. "'TO'
  IF sy-subrc <> 0.
    MESSAGE TEXT-006 TYPE zcl_abs_abap_maintain=>c_msgty_info. "'I'
    LEAVE LIST-PROCESSING.
  ENDIF.

  SORT gt_fmfphdr BY aufnr.
  DATA(lt_fmfphdr_temp) = gt_fmfphdr.
  SORT lt_fmfphdr_temp BY tplnr_fl.
  DELETE ADJACENT DUPLICATES FROM lt_fmfphdr_temp COMPARING tplnr_fl.

*--Fetching Terrain details to get area(Shifts)
  SELECT *
    FROM /agri/fmirflo
    INTO TABLE gt_fmirflo
     FOR ALL ENTRIES IN lt_fmfphdr_temp
   WHERE equnr    EQ p_equnr
     AND tplnr_fl EQ lt_fmfphdr_temp-tplnr_fl.
  IF sy-subrc <> 0.
    MESSAGE TEXT-007 TYPE zcl_abs_abap_maintain=>c_msgty_info. "'I'
    LEAVE LIST-PROCESSING.
  ENDIF.
  SORT gt_fmirflo BY equnr tplnr_fl.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form MESSAGES_DISPLAY
*&---------------------------------------------------------------------*
*& MESSAGES_DISPLAY
*&---------------------------------------------------------------------*
FORM messages_display  USING pt_msgtab TYPE esp1_message_tab_type.

  IF pt_msgtab IS NOT INITIAL.
*--Calling FM to get message pop-up
    CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
      TABLES
        i_message_tab = pt_msgtab.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CONTROL_DISPLAY
*&---------------------------------------------------------------------*
*& CONTROL_DISPLAY
*&---------------------------------------------------------------------*
FORM control_display.

*--Local declarations
  DATA : lt_fcat      TYPE lvc_t_fcat,
         lt_dropdown  TYPE lvc_t_dral,
         lwa_layout   TYPE lvc_s_layo,
         lwa_dropdown TYPE lvc_s_dral,
         lv_structure TYPE dd02l-tabname.

  IF gobj_cont_cnf IS INITIAL.
*--Create Object For Custom Container
    CREATE OBJECT gobj_cont_cnf
      EXPORTING
        container_name = 'WO_CNF_0100_CC'.

*--Create Object for ALV Grid
    CREATE OBJECT gobj_alv_cnf
      EXPORTING
        i_parent = gobj_cont_cnf.
  ENDIF.

  IF gobj_cont_con IS INITIAL AND sy-ucomm EQ zcl_abs_abap_maintain=>c_ucomm_enter. "'ENTR'
*--Create Object For Custom Container
    CREATE OBJECT gobj_cont_con
      EXPORTING
        container_name = 'WO_CON_0100_CC'.
*--Create Object for ALV Grid
    CREATE OBJECT gobj_alv_con
      EXPORTING
        i_parent = gobj_cont_con.
  ENDIF.

  IF gobj_alv_cnf IS NOT INITIAL AND sy-ucomm NE zcl_abs_abap_maintain=>c_ucomm_enter. "'ENTR'
    lv_structure = zcl_abs_abap_maintain=>c_str_irr_cnf. "'ZABS_STR_OCNF'
  ELSEIF gobj_alv_con IS NOT INITIAL AND sy-ucomm EQ zcl_abs_abap_maintain=>c_ucomm_enter. "'ENTR'
    lv_structure = zcl_abs_abap_maintain=>c_str_irr_con. "'ZABS_STR_OCON'.
  ENDIF.

*--Field Catalog
  PERFORM fcat_prepare USING lv_structure
                    CHANGING lt_fcat.

  CASE lv_structure.
    WHEN zcl_abs_abap_maintain=>c_str_irr_cnf. "'ZABS_STR_OCNF'

      IF r_conf IS NOT INITIAL.

        LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<lfs_fcat>).
          CASE <lfs_fcat>-fieldname.
            WHEN 'ORDNO'
              OR 'WERKS'
              OR 'EQUNR'
              OR 'ODATE'
              OR 'GAMNG'
              OR 'TOT_IQTY'
              OR 'IRRQTY_UOM'.
              <lfs_fcat>-edit = space.
            WHEN 'VORNR'.
              <lfs_fcat>-edit = abap_true.
*-- Drop down for fields set
              <lfs_fcat>-drdn_hndl = zcl_abs_abap_maintain=>c_drpdown_handle1. "'1'
              <lfs_fcat>-drdn_alias = abap_true.
            WHEN 'CONTR'
              OR 'LOEVM'
              OR 'GMEIN'.
              <lfs_fcat>-no_out = abap_true.
            WHEN OTHERS.
              <lfs_fcat>-edit = abap_true.
          ENDCASE.
        ENDLOOP.

      ELSEIF r_rev IS NOT INITIAL.

        LOOP AT lt_fcat ASSIGNING <lfs_fcat>.
          CASE <lfs_fcat>-fieldname.
            WHEN 'LOEVM'
              OR 'CONTR'
              OR 'GAMNG'
              OR 'GMEIN'.
              <lfs_fcat>-no_out = abap_true.
          ENDCASE.
        ENDLOOP.

      ENDIF.

    WHEN zcl_abs_abap_maintain=>c_str_irr_con. "'ZABS_STR_OCON'
      LOOP AT lt_fcat ASSIGNING <lfs_fcat>.
        CASE <lfs_fcat>-fieldname.
          WHEN 'ORDNO'
            OR 'MATNR'
            OR 'MAKTX'
            OR 'ERFME'.
            <lfs_fcat>-edit = space.
          WHEN 'VORNR'
            OR 'WONUM'
            OR 'CONTR'.
            <lfs_fcat>-tech = abap_true.
          WHEN OTHERS.
            <lfs_fcat>-edit = abap_true.
        ENDCASE.
      ENDLOOP.
  ENDCASE.

  "Set ALV attributes FOR LAYOUT
  lwa_layout-cwidth_opt = abap_true.
  lwa_layout-zebra      = abap_true.
  lwa_layout-col_opt    = abap_true.
  lwa_layout-no_rowmark = abap_true.
  IF r_rev IS NOT INITIAL.
    lwa_layout-sel_mode = zcl_abs_abap_maintain=>c_layout_sel_mode. "'A'
    lwa_layout-no_rowmark = space.
  ENDIF.

  IF gt_shift IS INITIAL.
    PERFORM operations_data CHANGING gt_shift.
  ENDIF.

  "Controls events register
  PERFORM controls_events_register.

  "Displaying ALV Data
  IF gobj_alv_cnf IS NOT INITIAL AND sy-ucomm NE zcl_abs_abap_maintain=>c_ucomm_enter. "'ENTR'

    PERFORM alv_display USING lwa_layout
                     CHANGING gobj_alv_cnf
                              gt_wo_cnf
                              lt_fcat.
  ELSEIF gobj_alv_con IS NOT INITIAL AND sy-ucomm EQ zcl_abs_abap_maintain=>c_ucomm_enter. "'ENTR'

    PERFORM alv_display USING lwa_layout
                     CHANGING gobj_alv_con
                              gt_wo_con
                              lt_fcat.
  ENDIF.

  CALL METHOD gobj_alv_cnf->refresh_table_display.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
*& HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
FORM handle_data_changed  USING p_er_data_changed
                    TYPE REF TO cl_alv_changed_data_protocol.

*--Local Data declarations.
  DATA :lwa_mod_cell TYPE lvc_s_modi,
        lwa_ins_rows TYPE lvc_s_moce,
        lwa_mod      TYPE ty_mod,

*--Internal table declaration
        lt_imod      TYPE TABLE OF ty_mod,
        lv_valid.

*--Field-symbol declaration
  FIELD-SYMBOLS: <ft_append_row> TYPE ANY TABLE.

  IF p_er_data_changed->mt_inserted_rows IS NOT INITIAL.
    ASSIGN p_er_data_changed->mp_mod_rows->* TO <ft_append_row>.
    IF <ft_append_row> IS ASSIGNED.
      APPEND LINES OF <ft_append_row> TO gt_wo_cnf.
    ENDIF.
  ENDIF.

  LOOP AT p_er_data_changed->mt_good_cells INTO lwa_mod_cell.
    lwa_mod-row  = lwa_mod_cell-row_id.
    APPEND lwa_mod TO lt_imod.
    IF lwa_mod_cell-fieldname = 'LMNGA'
    OR lwa_mod_cell-fieldname = 'FCONTR_HM'
    OR lwa_mod_cell-fieldname = 'ICONTR_HM'.
      APPEND lwa_mod_cell TO gt_mod.
    ELSEIF lwa_mod_cell-fieldname = 'VORNR'.
      READ TABLE gt_wo_cnf ASSIGNING FIELD-SYMBOL(<lfs_wo_cnf>) INDEX 1.
      IF sy-subrc = 0.
        CLEAR <lfs_wo_cnf>-lmnga.
      ENDIF.
    ENDIF.
  ENDLOOP.

  gs_variables-changes = abap_true.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form VALIDATION
*&---------------------------------------------------------------------*
*& VALIDATION
*&---------------------------------------------------------------------*
FORM validation  USING    pwa_wo_cnf  TYPE zabs_str_ocnf
                 CHANGING pt_msgtab TYPE esp1_message_tab_type.

*--Local declarations
  DATA : lwa_messages TYPE esp1_message_wa_type.

  IF pwa_wo_cnf-vornr IS INITIAL
  OR pwa_wo_cnf-lmnga IS INITIAL
  OR pwa_wo_cnf-ihour IS INITIAL
  OR pwa_wo_cnf-fhour IS INITIAL
  OR pwa_wo_cnf-idate IS INITIAL
  OR pwa_wo_cnf-fdate IS INITIAL
  OR pwa_wo_cnf-fdate IS INITIAL
  OR pwa_wo_cnf-icontr_hm IS INITIAL
  OR pwa_wo_cnf-fcontr_hm IS INITIAL.
    lwa_messages-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
    lwa_messages-msgno = 129.
    lwa_messages-msgty = zcl_abs_abap_maintain=>c_msgty_error.
    APPEND lwa_messages TO pt_msgtab.
    CLEAR lwa_messages.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SCREEN_MODIFY
*&---------------------------------------------------------------------*
*& screen_modify
*&---------------------------------------------------------------------*
FORM screen_modify.

*--Screen Validations
  LOOP AT SCREEN.
*--When Confirmations is active
    IF r_conf = abap_true.
      IF screen-group1 = 'K1'.
        screen-active = '0'.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form BUILD_REVDATA
*&---------------------------------------------------------------------*
*& BUILD_REVDATA
*&---------------------------------------------------------------------*
FORM build_revdata  USING pwa_order TYPE ty_order.

*--Local declaration
  DATA : lwa_wo_cnf TYPE zabs_str_ocnf.

  IF pwa_order-ordhdr IS NOT INITIAL.
*--Fetching Order Confirmations data
    SELECT *
      FROM zabst_ordcnf
      INTO TABLE gt_wo_dbcnf
       FOR ALL ENTRIES IN pwa_order-ordhdr
     WHERE ordno EQ pwa_order-ordhdr-ordno
       AND vornr EQ p_shift
       AND cdate EQ p_date
       AND loevm EQ space.
    IF sy-subrc <> 0.
      MESSAGE TEXT-011 TYPE zcl_abs_abap_maintain=>c_msgty_info. "'I'
      LEAVE LIST-PROCESSING.
    ENDIF.
    SORT gt_wo_dbcnf BY ordno contr.
  ENDIF. "pwa_order-ordhdr

*--Fetching Order Group data
  SELECT *
    FROM zabst_ordgrp
    INTO TABLE gt_ordgrp
     FOR ALL ENTRIES IN gt_wo_dbcnf
   WHERE ordno EQ gt_wo_dbcnf-ordno
     AND contr EQ gt_wo_dbcnf-contr.
  IF sy-subrc = 0.
    SORT gt_ordgrp BY ordno contr.
  ENDIF.

  LOOP AT gt_wo_dbcnf INTO DATA(lwa_wocnf).

    READ TABLE pwa_order-ordhdr INTO DATA(lwa_ordhdr)
    WITH KEY ordno = lwa_wocnf-ordno.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING lwa_wocnf TO lwa_wo_cnf.
      lwa_wo_cnf-werks = lwa_ordhdr-werks.
      lwa_wo_cnf-equnr = lwa_ordhdr-equnr.
*      lwa_wo_cnf-cdate = p_date. "lwa_ordhdr-odate.
      lwa_wo_cnf-irrqty_uom = lwa_wocnf-uom.
      APPEND lwa_wo_cnf TO gt_wo_cnf.
      CLEAR lwa_wo_cnf.
    ENDIF.

  ENDLOOP. "gt_wo_dbcnf

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F4_FOR_SHIFT
*&---------------------------------------------------------------------*
*& F4_FOR_SHIFT
*&---------------------------------------------------------------------*
FORM f4_for_shift.

*--Local declaration
  DATA: lt_return   TYPE STANDARD TABLE OF ddshretval,
        lt_shiftf4  TYPE tty_shift_f4,
        lwa_shiftf4 TYPE ty_shift_f4,
        lv_tabix    TYPE sy-tabix.

  LOOP AT gt_shift INTO DATA(lwa_shift) WHERE werks = p_werks
                                          AND equnr = p_equnr.
    DATA(lv_exit) = abap_true.
    EXIT.
  ENDLOOP. "gt_shift

  IF lv_exit IS INITIAL.
    PERFORM shift_data.
  ENDIF.

*--Collect Shift data for F4
  READ TABLE gt_shift TRANSPORTING NO FIELDS
  WITH KEY werks = p_werks
           equnr = p_equnr
           BINARY SEARCH.
  IF sy-subrc = 0.
    lv_tabix = sy-tabix.
    LOOP AT gt_shift INTO lwa_shift FROM lv_tabix.
      IF lwa_shift-werks <> p_werks
      OR lwa_shift-equnr <> p_equnr.
        EXIT.
      ENDIF.
      CLEAR lwa_shiftf4.
      lwa_shiftf4-vornr = lwa_shift-vornr.
      lwa_shiftf4-ltxa1 = lwa_shift-ltxa1.
      APPEND lwa_shiftf4 TO lt_shiftf4.
    ENDLOOP.

*--For F4 Help
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'VORNR'
        dynpprog        = sy-cprog
        dynpnr          = sy-dynnr
        value_org       = zcl_abs_abap_maintain=>c_f4_valorg_s "'S'
      TABLES
        value_tab       = lt_shiftf4
        return_tab      = lt_return
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    IF sy-subrc = 0.
      READ TABLE lt_return INTO DATA(lwa_return) INDEX 1.
      IF sy-subrc = 0.
        p_shift = lwa_return-fieldval.
      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form REVERSAL
*&---------------------------------------------------------------------*
*& REVERSAL
*&---------------------------------------------------------------------*
FORM reversal USING    pt_ocnum   TYPE /agri/t_fmocnum
              CHANGING ct_ordgrp  TYPE zabs_tty_ordgrp
                       ct_wo_cnf  TYPE tty_wo_cnf.

*--Local declarations
  DATA :lt_fmoc_doc TYPE /agri/t_fmoc_doc,
        lt_ordgrp   TYPE TABLE OF zabst_ordgrp,
        lt_wo_cnf   TYPE TABLE OF zabst_ordcnf,
        lv_tabix    TYPE sy-tabix,
        lv_subrc    TYPE sy-subrc.

  SORT  ct_ordgrp BY ocnum.

  CALL FUNCTION '/AGRI/FMOC_VIEW'
    EXPORTING
      it_ocnum       = pt_ocnum
    IMPORTING
      et_ocdoc       = lt_fmoc_doc
    EXCEPTIONS
      no_data_exists = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  PERFORM messages_initialize USING 'SAVE'
                                    'SAVE'.

  LOOP AT lt_fmoc_doc INTO DATA(lwa_fmoc_doc).

*--Reverse Confirmations
    PERFORM reverse_confirmations USING c_true
                               CHANGING lwa_fmoc_doc
                                        lv_subrc.
    IF lv_subrc IS INITIAL.

*--Populating Order Group table with Reverse order number
      READ TABLE ct_ordgrp TRANSPORTING NO FIELDS
      WITH KEY ocnum = lwa_fmoc_doc-x-ochdr-ocnum_ref
      BINARY SEARCH.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      lv_tabix = sy-tabix.
      LOOP AT ct_ordgrp ASSIGNING FIELD-SYMBOL(<lfs_ordgrp>) FROM lv_tabix.
        IF <lfs_ordgrp>-ocnum <> lwa_fmoc_doc-x-ochdr-ocnum_ref.
          EXIT.
        ENDIF.
        <lfs_ordgrp>-rev_ocnum = lwa_fmoc_doc-ocnum.
        <lfs_ordgrp>-reversed  = abap_true.
        APPEND <lfs_ordgrp> TO lt_ordgrp.

*--Filling Order Confirmations table with Loevm eq X
        READ TABLE ct_wo_cnf ASSIGNING FIELD-SYMBOL(<lfs_wo_cnf>)
        WITH KEY ordno = <lfs_ordgrp>-ordno
                 contr = <lfs_ordgrp>-contr
                 BINARY SEARCH.
        IF sy-subrc = 0.
          <lfs_wo_cnf>-loevm = abap_true.

          READ TABLE gt_wo_dbcnf ASSIGNING FIELD-SYMBOL(<lfs_wo_dbcnf>)
          WITH KEY ordno = <lfs_wo_cnf>-ordno
                   contr = <lfs_wo_cnf>-contr
                   BINARY SEARCH.
          IF sy-subrc = 0.
            <lfs_wo_dbcnf>-loevm = abap_true.
            APPEND <lfs_wo_dbcnf> TO lt_wo_cnf.
          ENDIF.

        ENDIF.

      ENDLOOP. "ct_ordgrp

    ENDIF. "lv_subrc
  ENDLOOP. "lt_fmoc_doc

  IF lt_wo_cnf IS NOT INITIAL.
    SORT lt_wo_cnf BY ordno contr vornr.
    DELETE ADJACENT DUPLICATES FROM lt_wo_cnf COMPARING ordno contr vornr.
    MODIFY zabst_ordcnf FROM TABLE lt_wo_cnf.
  ENDIF.

  IF lt_ordgrp IS NOT INITIAL.
    SORT lt_ordgrp BY ordno contr posnr.
    DELETE ADJACENT DUPLICATES FROM lt_ordgrp COMPARING ordno contr posnr.
    MODIFY zabst_ordgrp FROM TABLE lt_ordgrp.
  ENDIF.

  PERFORM rev_messages_display USING 'SAVE'.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form  MESSAGES_INITIALIZE
*&---------------------------------------------------------------------*
*& MESSAGES_INITIALIZE
*----------------------------------------------------------------------*
FORM messages_initialize  USING  lv_initiator TYPE /agri/gdescr
                                 lv_subobject TYPE balsubobj.

  messages_init.

  CHECK lv_initiator IS NOT INITIAL.
  messages_collect_all.
  messages_initiator_set lv_initiator '/AGRI/FMOC' lv_subobject.

ENDFORM.                    " MESSAGES_INITIALIZE

*&---------------------------------------------------------------------*
*& Form  REVERSE_CONFIRMATIONS
*&---------------------------------------------------------------------*
*& REVERSE_CONFIRMATIONS
*----------------------------------------------------------------------*
FORM reverse_confirmations USING lv_commit_work
                        CHANGING lwa_fmoc_doc TYPE /agri/s_fmoc_doc
                                 lv_subrc TYPE sy-subrc.

*--Local declarations
  DATA: lwa_fmoc_doc_infocus TYPE /agri/s_fmoc_doc,
        lwa_ocopr            TYPE /agri/s_fmocopr,
        lwa_occom            TYPE /agri/s_fmoccom,
        lwa_ocindx           TYPE /agri/s_fmocindx,
        lt_fmoc_doc          TYPE /agri/t_fmoc_doc,
        lt_aufnr             TYPE /agri/t_fmaufnr,
        lt_fpdoc             TYPE /agri/t_fmfp_doc,
        lwa_aufnr            TYPE /agri/s_fmaufnr,
        lv_ocnum             TYPE /agri/fmocnum,
        lt_cancel            TYPE TABLE OF conf_cancel,
        lt_docnum            TYPE TABLE OF imseg,
        lt_return            TYPE TABLE OF bapi_coru_return,
        lt_mseg              TYPE TABLE OF imseg,
        lwa_docnum           TYPE imseg,
        lwa_cancel           TYPE conf_cancel,
        lwa_return           TYPE bapi_coru_return,
        lv_failed,
*---Replace Unreleased Interfaces
        lwa_breturn          TYPE bapiret1,
        lv_locked	           TYPE bapi_coru_param-locked,
        lv_conf_no           TYPE bapi_pi_conf_key-conf_no,
        lv_conf_count        TYPE bapi_pi_conf_key-conf_cnt.

*----Field-symbol declaration
  FIELD-SYMBOLS: <lwa_fmfp_doc> TYPE /agri/s_fmfp_doc,
                 <lwa_fpitm>    TYPE /agri/s_fmfpitm,
                 <lwa_fpcom>    TYPE /agri/s_fmfpcom.

  PERFORM messages_context_set USING lwa_fmoc_doc-x-ochdr.
*--lock task orders
  LOOP AT lwa_fmoc_doc-x-ocindx INTO lwa_ocindx.
    PERFORM order_infocus_lock USING lwa_ocindx-aufnr
                            CHANGING lv_subrc.
    IF lv_subrc IS NOT INITIAL.
      EXIT.
    ENDIF.

    lwa_aufnr-aufnr = lwa_ocindx-aufnr.
    APPEND lwa_aufnr TO lt_aufnr.
    CLEAR lwa_ocindx.
    lwa_ocindx-aufnr = lwa_aufnr-aufnr.
    lwa_ocindx-updkz = zcl_abs_abap_maintain=>c_updkz_insert. "'I'
    APPEND lwa_ocindx TO lwa_fmoc_doc_infocus-x-ocindx.
  ENDLOOP.
  IF lv_subrc IS NOT INITIAL OR
     lt_aufnr IS INITIAL.
    EXIT.
  ENDIF.

  PERFORM production_order_read USING lt_aufnr
                             CHANGING lt_fpdoc.

  LOOP AT lwa_fmoc_doc-x-ocopr INTO lwa_ocopr.
    READ TABLE lt_fpdoc ASSIGNING <lwa_fmfp_doc>
                         WITH KEY aufnr = lwa_ocopr-aufnr
                         BINARY SEARCH.
    IF sy-subrc EQ 0.
      IF <lwa_fmfp_doc>-x-fphdr-tecom IS INITIAL.
        lwa_cancel-conf_no    = lwa_ocopr-rueck.
        lwa_cancel-conf_count = lwa_ocopr-rmzhl.
        APPEND lwa_cancel TO lt_cancel.
      ELSE.
        MESSAGE e051(/agri/fmfp) WITH lwa_ocopr-aufnr INTO sy-msgli.
        message_simple space.
        EXIT.
      ENDIF.
      READ TABLE <lwa_fmfp_doc>-x-fpitm ASSIGNING <lwa_fpitm>
                                       WITH KEY aufnr = lwa_ocopr-aufnr
                                                posnr = lwa_ocopr-posnr
                                       BINARY SEARCH.
      IF sy-subrc EQ 0.
        <lwa_fpitm>-gwemg = <lwa_fpitm>-gwemg - lwa_ocopr-lmnga.
        IF <lwa_fpitm>-gwemg IS INITIAL.
          CLEAR <lwa_fpitm>-cstat.
        ELSEIF <lwa_fpitm>-gamng GT <lwa_fpitm>-gwemg.
          <lwa_fpitm>-cstat = zcl_abs_abap_maintain=>c_po_pcnf. "'PCNF'
        ENDIF.
        IF <lwa_fpitm>-grcre IS NOT INITIAL.
          <lwa_fmfp_doc>-x-fphdr-cstat = <lwa_fpitm>-cstat.
          READ TABLE lwa_fmoc_doc-x-ocindx INTO lwa_ocindx
                                       WITH KEY aufnr = <lwa_fpitm>-aufnr
                                       BINARY SEARCH.
          IF sy-subrc EQ 0.
            <lwa_fmfp_doc>-x-fphdr-gwemg = <lwa_fmfp_doc>-x-fphdr-gwemg - lwa_ocindx-gwemg.
          ENDIF.
          <lwa_fmfp_doc>-x-fphdr-updkz = zcl_abs_abap_maintain=>c_updkz_update. "'U'
        ENDIF.
        <lwa_fpitm>-updkz = zcl_abs_abap_maintain=>c_updkz_update. "'U'
        LOOP AT lwa_fmoc_doc-x-occom INTO lwa_occom WHERE rueck = lwa_ocopr-rueck
                                                      AND rmzhl = lwa_ocopr-rmzhl.
          READ TABLE  <lwa_fmfp_doc>-x-fpcom ASSIGNING <lwa_fpcom>
                                              WITH KEY aufnr = <lwa_fpitm>-aufnr
                                                       posnr = <lwa_fpitm>-posnr
                                                       contr = lwa_occom-contr
                                              BINARY SEARCH.
          IF sy-subrc EQ 0.
            <lwa_fpcom>-comng = <lwa_fpcom>-comng - lwa_occom-lmnga.
            <lwa_fpcom>-updkz = zcl_abs_abap_maintain=>c_updkz_update. "'U'
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDLOOP.
  CHECK lt_cancel IS NOT INITIAL.
*--Reverse Confirmations
  PERFORM call_confirmation_reverse TABLES lt_cancel
                                           lt_docnum
                                           lt_return
                                     USING sy-datum.
  LOOP AT lt_return INTO lwa_return
                   WHERE type EQ zcl_abs_abap_maintain=>c_msgty_error. "'E'
    lv_subrc = 4.
    MESSAGE ID lwa_return-id TYPE lwa_return-type
                           NUMBER lwa_return-number
                             WITH lwa_return-message_v1
                                  lwa_return-message_v2
                                  lwa_return-message_v3
                                  lwa_return-message_v4
                             INTO sy-msgli.
    message_simple space.
  ENDLOOP.
  IF lv_subrc IS NOT INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    EXIT.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
  ENDIF.

  LOOP AT lt_return INTO lwa_return.
    READ TABLE lt_cancel INTO lwa_cancel WITH KEY conf_no = lwa_return-conf_no. "INDEX lwa_return-row.
    IF sy-subrc EQ 0.
      CLEAR:lwa_ocopr.
      READ TABLE lwa_fmoc_doc-x-ocopr INTO lwa_ocopr WITH KEY rueck = lwa_cancel-conf_no
                                                              rmzhl = lwa_cancel-conf_count.
      IF sy-subrc EQ 0.
        lwa_ocopr-rueck_ref = lwa_ocopr-rueck."lwa_cancel-conf_no.
        lwa_ocopr-rmzhl_ref = lwa_ocopr-rmzhl."lwa_cancel-conf_count.
        lwa_ocopr-rueck     = lwa_return-conf_no.
        lwa_ocopr-rmzhl     = lwa_return-conf_cnt.
        lwa_ocopr-updkz     = zcl_abs_abap_maintain=>c_updkz_insert. "'I'
        READ TABLE lt_docnum INTO lwa_docnum INDEX sy-tabix.
        IF sy-subrc EQ 0.
          lwa_ocopr-aufnr = lwa_docnum-aufnr.
        ENDIF.
        APPEND lwa_ocopr TO lwa_fmoc_doc_infocus-x-ocopr.
        LOOP AT lwa_fmoc_doc-x-occom INTO lwa_occom WHERE rueck = lwa_cancel-conf_no
                                                      AND rmzhl = lwa_cancel-conf_count.
          CLEAR: lwa_occom-ocnum,lwa_occom-bwart.
          lwa_occom-rueck = lwa_return-conf_no.
          lwa_occom-rmzhl = lwa_return-conf_cnt.
          lwa_occom-updkz = zcl_abs_abap_maintain=>c_updkz_insert. "'I'
          APPEND lwa_occom TO lwa_fmoc_doc_infocus-x-occom.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF lv_subrc NE 0.
    EXIT.
  ENDIF.

*-- Generate new confirmatino number for reversal
  PERFORM oc_number_generate CHANGING lv_ocnum lv_failed.
  IF lv_failed IS INITIAL.
    PERFORM oc_enqueue USING lv_ocnum
                             lv_subrc.
    IF lv_subrc IS NOT INITIAL.
      EXIT.
    ENDIF.

    MOVE-CORRESPONDING lwa_fmoc_doc-x-ochdr TO lwa_fmoc_doc_infocus-x-ochdr.
    lwa_fmoc_doc_infocus-ocnum             =
    lwa_fmoc_doc_infocus-x-ochdr-ocnum     = lv_ocnum.
    lwa_fmoc_doc_infocus-x-ochdr-ocnum_ref = lwa_fmoc_doc-x-ochdr-ocnum.
    lwa_fmoc_doc_infocus-x-ochdr-updkz     = zcl_abs_abap_maintain=>c_updkz_insert. "'I'
    APPEND lwa_fmoc_doc_infocus TO lt_fmoc_doc.
  ENDIF.
*--Save confirmation
  PERFORM document_infocus_save USING lv_commit_work
                             CHANGING lt_fmoc_doc
                                      lv_subrc.
  IF lv_subrc IS INITIAL.
    MESSAGE s000(/agri/fmoc) INTO sy-msgli.
    message_simple space.
    MESSAGE s001(/agri/fmoc) WITH lv_ocnum INTO sy-msgli.
    message_simple space.
    CLEAR: lwa_fmoc_doc.
    READ TABLE lt_fmoc_doc INTO lwa_fmoc_doc INDEX 1.
  ELSE.
    lv_subrc = 4.
    EXIT.
  ENDIF.

*--Save Process order
  PERFORM production_order_save USING lv_commit_work
                                      lt_fpdoc.
  LOOP AT lt_aufnr INTO lwa_aufnr.
    PERFORM order_infocus_unlock USING lwa_aufnr-aufnr.
  ENDLOOP.

ENDFORM.                    " REVERSE_CONFIRMATIONS

*&---------------------------------------------------------------------*
*&      Form  PRODUCTION_ORDER_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM production_order_read  USING lt_aufnr TYPE /agri/t_fmaufnr
                         CHANGING lt_fpdoc TYPE /agri/t_fmfp_doc.

  CALL FUNCTION '/AGRI/FMFP_VIEW'
    EXPORTING
      it_aufnr       = lt_aufnr
    IMPORTING
      et_fpdoc       = lt_fpdoc
    EXCEPTIONS ##FM_SUBRC_OK
      no_data_exists = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.

  ENDIF.

ENDFORM.                    " PRODUCTION_ORDER_READ

*&---------------------------------------------------------------------*
*&      Form  cancel_confirmation_reverse
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM call_confirmation_reverse
                        TABLES lt_cancel STRUCTURE conf_cancel
                               lt_docnum STRUCTURE imseg
                               lt_return STRUCTURE bapi_coru_return
                         USING lv_budat TYPE budat.
*---Replace Unreleased Interfaces
  DATA: lwa_cancel         TYPE conf_cancel,
        lwa_breturn        TYPE bapi_coru_return,
*{   REPLACE        SS8K900016                                        3
*\       lv_conf_no         TYPE BAPI_PI_CONF_KEY-CONF_NO,
*\       lv_conf_coUnt      TYPE BAPI_PI_CONF_KEY-CONF_CNT,
*\       lv_POSTG_DATE      TYPE BAPI_PI_CONFIRM-POSTG_DATE,
*\       lv_CONF_TEXT       TYPE BAPI_PI_CONFIRM-CONF_TEXT,
        lv_conf_no         TYPE bapi_pp_conf_key-conf_no,
        lv_conf_count      TYPE bapi_pp_conf_key-conf_cnt,
        lv_postg_date      TYPE bapi_pp_confirm-postg_date,
        lv_conf_text       TYPE bapi_pp_confirm-conf_text,
*}   REPLACE
        lwa_return         TYPE bapiret1,
        lv_locked          TYPE bapi_coru_param-locked,
*{   REPLACE        SS8K900016                                        4
*\       lv_CRTD_CONF_NO    TYPE BAPI_PI_CONF_KEY-CONF_NO,
*\       lv_CRTD_CONF_COUNT TYPE BAPI_PI_CONF_KEY-CONF_CNT,
        lv_crtd_conf_no    TYPE bapi_pp_conf_key-conf_no,
        lv_crtd_conf_count TYPE bapi_pp_conf_key-conf_cnt,
*}   REPLACE
        lv_flg_error(1)    TYPE c.

*  CALL FUNCTION 'CO_RI_CONF_CANCEL'
*   EXPORTING
**     ORDER_CATEGORY           = 0
**     CONF_NO                  =
**     CONF_COUNT               =
*     postg_date               = lv_budat
**     CONF_TEXT                =
**     POST_WRONG_ENTRIES       = '0'
**     EX_IDENT                 =
**     I_NO_DATA_RESET          =
**     I_NO_CONF_POST           =
**   IMPORTING
**     RETURN                   =
**     LOCKED                   =
**     CREATED_CONF_NO          =
**     CREATED_CONF_COUNT       =
*   TABLES
*     it_cancel_conf           = lt_cancel[]
*     goodsmovements           = lt_docnum[]
*     detail_return            = lt_return[].

  LOOP AT lt_cancel INTO lwa_cancel.

    CLEAR: lv_conf_no, lv_conf_count, lv_postg_date, lv_conf_text,
           lwa_return, lv_locked, lv_crtd_conf_no, lv_crtd_conf_count,
           lv_flg_error, lwa_breturn.

    lv_conf_no    = lwa_cancel-conf_no.
    lv_conf_count = lwa_cancel-conf_count.

*{   DELETE         SS8K900016                                        1
*\     CALL FUNCTION 'BAPI_PROCORDCONF_CANCEL'
*\       EXPORTING
*\         confirmation              = lv_conf_no
*\         confirmationcounter       = lv_conf_count
*\         POSTG_DATE                = lv_budat
*\*         CONF_TEXT                 =
*\       IMPORTING
*\         RETURN                    = lwa_return
*\         LOCKED                    = lv_locked
*\         CREATED_CONF_NO           = lv_crtd_conf_no
*\         CREATED_CONF_COUNT        = lv_crtd_conf_count.
*}   DELETE
*{   INSERT         SS8K900016                                        2
    CALL FUNCTION 'BAPI_PRODORDCONF_CANCEL'
      EXPORTING
        confirmation        = lv_conf_no
        confirmationcounter = lv_conf_count
        postg_date          = lv_budat
*       CONF_TEXT           =
      IMPORTING
        return              = lwa_return
        locked              = lv_locked
        created_conf_no     = lv_crtd_conf_no
        created_conf_count  = lv_crtd_conf_count.
    IF lwa_return-type NE zcl_abs_abap_maintain=>c_msgty_error. "'E'
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.
*}   INSERT

    MOVE-CORRESPONDING lwa_return TO lwa_breturn.
    lwa_breturn-flg_locked = lv_locked.
    lwa_breturn-conf_no    = lv_crtd_conf_no.
    lwa_breturn-conf_cnt   = lv_crtd_conf_count.
    APPEND lwa_breturn TO lt_return.
  ENDLOOP.
*---
ENDFORM.                    "cancel_confirmation_reverse

*&---------------------------------------------------------------------*
*& Form  OC_NUMBER_GENERATE
*&---------------------------------------------------------------------*
*& OC_NUMBER_GENERATE
*----------------------------------------------------------------------*
FORM oc_number_generate  CHANGING lv_ocnum
                                  lv_failed.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = zcl_abs_abap_maintain=>c_obj_fmoc  "'/AGRI/FMOC'
    IMPORTING
      number                  = lv_ocnum
    EXCEPTIONS
      interval_not_found      = 01
      number_range_not_intern = 02
      object_not_found        = 03.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
    message_simple space.
    lv_failed = c_true.
    EXIT.
  ENDIF.

ENDFORM.                    " OC_NUMBER_GENERATE

*&---------------------------------------------------------------------*
*& Form  OC_ENQUEUE
*&---------------------------------------------------------------------*
*& OC_ENQUEUE
*----------------------------------------------------------------------*
FORM oc_enqueue  USING  lwa_ocdoc
                        lv_subrc.

  CALL FUNCTION 'ENQUEUE_/AGRI/EZ_FMOC'
    EXPORTING
      ocnum          = lwa_ocdoc
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
    lv_subrc = sy-subrc.
  ENDIF.

ENDFORM.                    " OC_ENQUEUE

*&---------------------------------------------------------------------*
*& Form  DOCUMENT_INFOCUS_SAVE
*&---------------------------------------------------------------------*
*& DOCUMENT_INFOCUS_SAVE
*----------------------------------------------------------------------*
FORM document_infocus_save  USING lv_commit_work
                         CHANGING lt_fmoc_doc
                                  lv_subrc TYPE sy-subrc.

  CALL FUNCTION '/AGRI/FMFP_OC_SAVE'
    EXPORTING
      i_commit_work = lv_commit_work
    CHANGING
      ct_ocdoc      = lt_fmoc_doc
    EXCEPTIONS
      OTHERS        = 1.
  lv_subrc  = sy-subrc.
  IF sy-subrc IS NOT INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.

ENDFORM.                    " DOCUMENT_INFOCUS_SAVE

*&---------------------------------------------------------------------*
*& Form  MESSAGES_DISPLAY
*&---------------------------------------------------------------------*
*& MESSAGES_DISPLAY
*----------------------------------------------------------------------*
FORM rev_messages_display USING  lv_initiator TYPE /agri/gdescr.

  DATA: ls_variant TYPE disvariant.
  ls_variant-report = zcl_abs_abap_maintain=>c_rep_wo_cnf. "'ZABS_REP_WO_CONF'
  ls_variant-handle = lv_initiator.

  messages_display lv_initiator c_true space space ls_variant.
  messages_init.

ENDFORM.                    " MESSAGES_DISPLAY


*&---------------------------------------------------------------------*
*& Form SHIFT_DATA
*&---------------------------------------------------------------------*
*& SHIFT_DATA
*&---------------------------------------------------------------------*
FORM shift_data.

*--Local declaration
  DATA: lwa_shift TYPE ty_shift.

*--Fetching Order Numbers
  SELECT ordno, werks,
         equnr, datab, datbi, erzet
    FROM zabst_ordhdr
    INTO TABLE @DATA(lt_ordhdr)
   WHERE werks EQ @p_werks
     AND equnr EQ @p_equnr
     AND datab LE @p_date
     AND datbi GE @p_date.
  IF sy-subrc = 0.
    SORT lt_ordhdr BY ordno.
  ENDIF.

  READ TABLE lt_ordhdr INTO DATA(lwa_ordno) INDEX 1.
  IF sy-subrc = 0.

*--Fetching WO Numbers
    SELECT ordno, wonum
      FROM zabst_orditm
      INTO TABLE @DATA(lt_wonum)
     WHERE ordno EQ @lwa_ordno-ordno.
    IF sy-subrc = 0.
      SORT lt_wonum BY wonum.
    ENDIF.

*--Fetching Operations(Shift numbers)
    SELECT wonum, vornr, ltxa1
      FROM /agri/fmwoopr
      INTO TABLE @DATA(lt_opr_temp)
       FOR ALL ENTRIES IN @gwa_order-orditm
     WHERE wonum EQ @gwa_order-orditm-wonum.
    IF sy-subrc = 0.
      SORT lt_opr_temp BY wonum.
    ENDIF.

    DATA(lt_opr) = lt_opr_temp.
    SORT lt_opr BY vornr.
    DELETE ADJACENT DUPLICATES FROM lt_opr COMPARING vornr.

    LOOP AT lt_opr INTO DATA(lwa_opr).
      lwa_shift-werks = lwa_ordno-werks.
      lwa_shift-equnr = lwa_ordno-equnr.
      lwa_shift-vornr = lwa_opr-vornr.
      lwa_shift-ltxa1 = lwa_opr-ltxa1.
      APPEND lwa_shift TO gt_shift.
      CLEAR lwa_shift.
    ENDLOOP.

  ENDIF. "lt_ordhdr

  IF gt_shift IS NOT INITIAL.
    SORT gt_shift BY werks equnr odate vornr.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form REFRESH_GLOBAL_DATA
*&---------------------------------------------------------------------*
*& REFRESH_GLOBAL_DATA
*&---------------------------------------------------------------------*
FORM refresh_global_data.

  REFRESH : gt_wo_cnf,
            gt_wo_con,
            gt_shift,
            gt_supply,
            gt_post,
            gt_woitm,
            gt_fmfphdr,
            gt_fmirflo,
            gt_mod,
            gt_comsplit,
            gt_task,
            gt_ordgrp,
            gt_wo_dbcnf.
  CLEAR gwa_order.


ENDFORM.

*&---------------------------------------------------------------------*
*& Form  MESSAGES_CONTEXT_SET
*&---------------------------------------------------------------------*
*&  MESSAGES_CONTEXT_SET
*----------------------------------------------------------------------*
FORM messages_context_set  USING lwa_ochdr TYPE /agri/s_fmochdr.

  DATA: lwa_context TYPE /agri/s_fmoc_context.

  IF lwa_ochdr IS NOT INITIAL.
    MOVE-CORRESPONDING lwa_ochdr TO lwa_context.
    messages_context_data_set_new lwa_ochdr-ocnum
                                  space space
                                  '/AGRI/S_FMOC_CONTEXT' lwa_context.
  ENDIF.

ENDFORM.                    " MESSAGES_CONTEXT_SET

*&---------------------------------------------------------------------*
*& Form  ORDER_INFOCUS_LOCK
*&---------------------------------------------------------------------*
*& ORDER_INFOCUS_LOCK
*----------------------------------------------------------------------*
FORM order_infocus_lock  USING lv_aufnr TYPE /agri/fmfpnum
                      CHANGING lv_subrc TYPE sy-subrc.

  DATA: lv_msgv1 TYPE sy-msgv1.

  CALL FUNCTION 'ENQUEUE_ESORDER'
    EXPORTING
      aufnr          = lv_aufnr
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc IS NOT INITIAL.
    lv_subrc = sy-subrc.
    lv_msgv1 = sy-msgv1.
    MESSAGE i037(/agri/fmfp) INTO sy-msgli
                             WITH lv_aufnr lv_msgv1.
    message_simple space.
  ENDIF.

ENDFORM.                    " ORDER_INFOCUS_LOCK

*&---------------------------------------------------------------------*
*& Form  order_infocus_unlock
*&---------------------------------------------------------------------*
*& order_infocus_unlock
*----------------------------------------------------------------------*
FORM order_infocus_unlock USING lv_aufnr TYPE /agri/fmfpnum.

  CALL FUNCTION 'DEQUEUE_ESORDER'
    EXPORTING
      aufnr = lv_aufnr.

ENDFORM.                    "order_infocus_unlock

*&---------------------------------------------------------------------*
*& Form  PRODUCTION_ORDER_SAVE
*&---------------------------------------------------------------------*
*& PRODUCTION_ORDER_SAVE
*----------------------------------------------------------------------*
FORM production_order_save USING lv_commit_work
                                 lt_fpdoc.

  CALL FUNCTION '/AGRI/FMFP_SAVE'
    EXPORTING
      i_commit_work = lv_commit_work
    CHANGING
      ct_fpdoc      = lt_fpdoc
    EXCEPTIONS
      no_change     = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.                    " PRODUCTION_ORDER_SAVE

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

*&---------------------------------------------------------------------*
*& Form F4_FOR_EQUNR
*&---------------------------------------------------------------------*
*& F4_FOR_EQUNR
*&---------------------------------------------------------------------*
FORM f4_for_equnr.

  DATA: BEGIN OF lt_dynpfields OCCURS 0.
      INCLUDE STRUCTURE dynpread.
  DATA: END OF lt_dynpfields.

  DATA: lt_return      TYPE STANDARD TABLE OF ddshretval,
        lv_werks_field TYPE dynfnam VALUE 'P_WERKS'.

  lt_dynpfields-fieldname = lv_werks_field.
  APPEND lt_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = sy-cprog
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = lt_dynpfields
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      double_conversion    = 9
      stepl_not_found      = 10
      OTHERS               = 11.

  IF sy-subrc EQ 0.
    READ TABLE lt_dynpfields WITH KEY fieldname = lv_werks_field.
    IF sy-subrc EQ 0.
      p_werks = lt_dynpfields-fieldvalue.
    ENDIF.
  ENDIF.

  IF p_werks IS INITIAL.
    MESSAGE TEXT-018 TYPE 'I'.
    RETURN.
  ENDIF.

*--Fetching Equipment header and Equipment master plants data
  SELECT a~equnr, a~irtyp
    FROM /agri/fmirhdr AS a
   INNER JOIN /agri/fmirwrk AS b
      ON a~equnr = b~equnr
    INTO TABLE @DATA(lt_equnr_f4)
   WHERE a~irtyp EQ @zcl_abs_abap_maintain=>c_irrtyp "'001'
     AND b~werks EQ @p_werks.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'EQUNR'
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      value_org       = 'S'
    TABLES
      value_tab       = lt_equnr_f4
      return_tab      = lt_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc = 0.
    READ TABLE lt_return INTO DATA(lwa_return) INDEX 1.
    IF sy-subrc = 0.
      p_equnr = lwa_return-fieldval.
    ENDIF.
  ENDIF.

ENDFORM.
