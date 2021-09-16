class ZCL_ABS_WORKORDER_UPDATE definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_WORKORDER_UPDATE .
protected section.
PRIVATE SECTION.

  TYPES ty_cobra TYPE cobra .
  TYPES:
    BEGIN OF ty_cobra_buf.
      INCLUDE TYPE ty_cobra.
  TYPES: uflag TYPE dkobr-upd_flag,
         END OF ty_cobra_buf .
  TYPES:
    tt_cobra_buf TYPE STANDARD TABLE OF ty_cobra_buf .
  TYPES ty_cobrb TYPE cobrb .
  TYPES:
    BEGIN OF ty_cobrb_buf.
      INCLUDE TYPE ty_cobrb.
  TYPES: uflag TYPE dkobr-upd_flag,
         END OF ty_cobrb_buf .
  TYPES:
    tt_cobrb_buf TYPE STANDARD TABLE OF ty_cobrb_buf .
  TYPES:
    tt_status TYPE STANDARD TABLE OF jstat .
ENDCLASS.



CLASS ZCL_ABS_WORKORDER_UPDATE IMPLEMENTATION.


METHOD if_ex_workorder_update~archive_objects.
ENDMETHOD.


METHOD if_ex_workorder_update~at_deletion_from_database.
ENDMETHOD.


METHOD if_ex_workorder_update~at_release.
ENDMETHOD.


METHOD if_ex_workorder_update~at_save.

*-- Internal Tables
  DATA:
    lt_items_bom TYPE /agri/t_fmfpbom_fcat,
    lrt_auart    TYPE RANGE OF aufart.

*-- Workareas
  DATA:
    ls_prodord   TYPE /agri/s_glprodord,
    ls_cobra     TYPE cobra,
    ls_items_bom TYPE /agri/s_fmfpbom_fcat,
    ls_resbb     TYPE resbb,
    ls_resbd_ins TYPE resbd,
    ls_caufvd    TYPE caufvd,
    ls_cobra_buf TYPE ty_cobra_buf,
    ls_cobrb_buf TYPE ty_cobrb_buf.

*-- Variables
  DATA:
    lv_chemical   TYPE sy-datar,
    lv_objnr_stat TYPE jsto-objnr,
    lv_index_plko TYPE sy-tabix,
    lv_inter      TYPE int4,
    lv_obart      TYPE j_obart,
    lv_objnr      TYPE j_objnr.

*-- Field Symbols
  FIELD-SYMBOLS:
    <lt_resb_bt>   TYPE ANY TABLE,
    <ls_resb_bt>   TYPE any,
    <lt_cobra_buf> TYPE tt_cobra_buf,
    <lt_cobrb_buf> TYPE tt_cobrb_buf,
    <lt_range>     TYPE STANDARD TABLE.

*-- References
  DATA:
    lo_range       TYPE REF TO data.

*-- BOC-T_T.KONNO
  DATA: lt_abap_stack TYPE abap_callstack,
        lt_sys_stack  TYPE sys_callst.

  CALL FUNCTION 'SYSTEM_CALLSTACK'
    IMPORTING
      callstack    = lt_abap_stack
      et_callstack = lt_sys_stack.

  DATA(lv_vistex) = abap_false.
  LOOP AT lt_abap_stack INTO DATA(lwa_abap_stack).
    IF lwa_abap_stack-mainprogram CS '/AGRI/'.
      lv_vistex = abap_true.
    ENDIF.
  ENDLOOP.
*-- EOC-T_T.KONNO

  IF ( sy-tcode(6) EQ zcl_abs_abap_maintain=>c_agri_name_space   OR
       sy-cprog    EQ 'ZFMFP_UNPLANNED_TASKORDER' OR
       lv_vistex   EQ abap_true OR
       sy-cprog(6) EQ zcl_abs_abap_maintain=>c_agri_name_space ).

**-- Get Yard Order types from variant table
*    CREATE DATA lo_range LIKE lrt_auart.
*    IF lo_range IS BOUND.
**-- Fetch Variant Table Data
*      CALL METHOD zcl_abs_get_variants=>get_range_constants
*        EXPORTING
*          iv_mod   = zcl_abs_abap_maintain=>c_cus_mode
*          iv_objid = zcl_abs_abap_maintain=>c_objid_order_create
*          iv_k1val = zcl_abs_abap_maintain=>c_key_yard_ord_types
*        IMPORTING
*          eo_range = lo_range.

*      ASSIGN lo_range->* TO <lt_range>.
*      IF <lt_range> IS ASSIGNED.
*        lrt_auart = <lt_range>.
*      ENDIF.
*    ENDIF.
*
*    IF lrt_auart IS NOT INITIAL AND is_header_dialog-auart IN lrt_auart.
*      RETURN.
*    ENDIF.

    CHECK is_header_dialog-auart NE 'ZY00'.

*-- Read first item in the Order
    lv_index_plko = 1.
    PERFORM read_caufv_indx IN PROGRAM saplcobh IF FOUND
                                 USING lv_index_plko
                                       ls_caufvd
                                       sy-subrc.

    IMPORT it_items_bom TO lt_items_bom FROM MEMORY ID 'IT_ITEMS_BOM'.
    FREE MEMORY ID 'IT_ITEMS_BOM'.

    IMPORT i_chemical TO lv_chemical  FROM MEMORY ID 'I_CHEMICAL'.
    FREE MEMORY ID 'I_CHEMICAL'.

    ASSIGN ('(SAPLCOBC)RESB_BT[]') TO <lt_resb_bt>.
    IF <lt_resb_bt> IS ASSIGNED AND
       ( lt_items_bom IS NOT INITIAL OR lv_chemical IS NOT INITIAL ).
      CLEAR: <lt_resb_bt>.
    ENDIF.

    LOOP AT lt_items_bom INTO ls_items_bom.

      CLEAR lv_objnr_stat.
      CALL FUNCTION 'STATUS_OBJECT_CREATE'
        EXPORTING
          chgkz = abap_true
          obtyp = zcl_abs_abap_maintain=>c_object_typ_pp_pm
        IMPORTING
          objnr = lv_objnr_stat.

      CLEAR ls_resbb.
      MOVE-CORRESPONDING ls_caufvd TO ls_resbb.

      ls_resbb-mandt = sy-mandt.
      ls_resbb-bdart = zcl_abs_abap_maintain=>c_requirement_typ_ar.
      ls_resbb-xwaok = abap_true.
      ls_resbb-matnr = ls_items_bom-matnr.
      ls_resbb-werks = ls_items_bom-werks.
      ls_resbb-lgort = ls_items_bom-lgort.
      ls_resbb-charg = ls_items_bom-charg.
      ls_resbb-bdter = ls_caufvd-plauf.
      ls_resbb-bdmng = ls_items_bom-plan_qty.

      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
        EXPORTING
          input          = ls_items_bom-unit
        IMPORTING
          output         = ls_items_bom-unit
        EXCEPTIONS
          unit_not_found = 1
          OTHERS         = 2.
      IF sy-subrc EQ 0.
        ls_resbb-meins = ls_items_bom-unit.
      ENDIF.

      ls_resbb-shkzg = zcl_abs_abap_maintain=>c_indicator_credit.
      ls_resbb-erfmg = ls_items_bom-plan_qty.
      ls_resbb-erfme = ls_items_bom-unit.
      ls_resbb-bwart = zcl_abs_abap_maintain=>c_movement_typ_gi_order.
      ls_resbb-postp = zcl_abs_abap_maintain=>c_item_cat_stock.
      ls_resbb-sanka = zcl_abs_abap_maintain=>c_costing_rel_full.
      ls_resbb-umrez = 1.
      ls_resbb-umren = 1.
      ls_resbb-sbter = ls_caufvd-plauf.
      ls_resbb-vornr = zcl_abs_abap_maintain=>c_activity_num_initial.
      ls_resbb-aplzl = 1.
      ls_resbb-objnr = lv_objnr_stat.
      ls_resbb-esmng = ls_items_bom-plan_qty.
      ls_resbb-funct = zcl_abs_abap_maintain=>c_distribute_fun_disp_sdate.

      MOVE-CORRESPONDING ls_resbb TO ls_resbd_ins.

      CALL FUNCTION 'CO_DI_MAT_INSERT'
        EXPORTING
          resbd_ins         = ls_resbd_ins
          index_plko        = 1
          index_plfl        = 1
          index_plpo        = 1
          tca11_opr         = abap_true
          schedule_flag_imp = ' '
          no_new_rspos      = ' '
        IMPORTING
          resbd_exp         = ls_resbd_ins.
    ENDLOOP.

    IF <lt_resb_bt> IS ASSIGNED.
      LOOP AT <lt_resb_bt> ASSIGNING <ls_resb_bt>.
        CLEAR: ls_resbb.
        MOVE-CORRESPONDING <ls_resb_bt> TO ls_resbb.
        ls_resbb-rspos = sy-tabix.
        lv_inter = ls_resbb-rspos * 10.
        ls_resbb-posnr = lv_inter.
        CONDENSE ls_resbb-posnr.
        MOVE-CORRESPONDING ls_resbb TO <ls_resb_bt>.
      ENDLOOP.
    ENDIF.

    ls_cobra-objnr = is_header_dialog-objnr.

    PERFORM gt_cobra_buf_set IN PROGRAM saplkobs
                                  USING ls_cobra abap_true IF FOUND.

    IMPORT is_prodord TO ls_prodord FROM MEMORY ID 'IS_PRODORD'.

    CHECK ls_prodord-konty IS NOT INITIAL.

*-- Fecth the object type
    SELECT SINGLE obart
      INTO lv_obart
      FROM tbo01
     WHERE obart_ld EQ ls_prodord-konty.

    ASSIGN ('(SAPLKOBS)GT_COBRA_BUF[]') TO <lt_cobra_buf>.
    ASSIGN ('(SAPLKOBS)GT_COBRB_BUF[]') TO <lt_cobrb_buf>.

    IF <lt_cobra_buf> IS ASSIGNED.
      REFRESH <lt_cobra_buf>.

      IF <lt_cobra_buf> IS INITIAL.
        CLEAR ls_cobra_buf.
        ls_cobra_buf-objnr = is_header_dialog-objnr.
        ls_cobra_buf-ernam = ls_cobra_buf-aenam = sy-uname.
        ls_cobra_buf-erdat = ls_cobra_buf-aedat = sy-datlo.

*-- Fetching the settlement profile for the order type
        SELECT SINGLE aprof
          INTO ls_cobra_buf-aprof
          FROM t003o
         WHERE auart EQ is_header_dialog-auart.

*-- Fetch the default value for allocation Structure in settlement profile
        SELECT SINGLE absch
          INTO ls_cobra_buf-absch
          FROM tkb1a
         WHERE aprof EQ ls_cobra_buf-aprof.

        IF lv_obart EQ zcl_abs_abap_maintain=>c_obj_typ_order. "Yard Order
*-- Fetch the object number for the order
          SELECT SINGLE objnr
            INTO lv_objnr
            FROM caufv
           WHERE aufnr EQ ls_prodord-empge.
          IF sy-subrc EQ 0.
            ls_cobra_buf-sort = lv_objnr.
          ENDIF.
          ls_cobra_buf-hienr = zcl_abs_abap_maintain=>c_hierarchy_no_50.
        ENDIF.

        ls_cobra_buf-uflag = zcl_abs_abap_maintain=>c_updkz_insert.
        INSERT ls_cobra_buf INTO TABLE <lt_cobra_buf>.

        CLEAR ls_cobra_buf.
        IF lv_obart = zcl_abs_abap_maintain=>c_obj_typ_order.  "Yard Order
*-- Fetch the settlement rule header data
          SELECT SINGLE *
            INTO ls_cobra_buf
            FROM cobra
           WHERE objnr = lv_objnr.
          IF sy-subrc EQ 0.
            INSERT ls_cobra_buf INTO TABLE <lt_cobra_buf>.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF <lt_cobrb_buf> IS ASSIGNED AND <lt_cobrb_buf> IS INITIAL.

      CASE lv_obart.                                   "Object Type

        WHEN zcl_abs_abap_maintain=>c_obj_typ_order.      "Yard Order
*-- Populate Object No and Order
          ls_cobrb_buf-rec_objnr1 = lv_objnr.
          ls_cobrb_buf-werks = is_header_dialog-werks.
          ls_cobrb_buf-aufnr = ls_prodord-empge.

        WHEN zcl_abs_abap_maintain=>c_obj_typ_asset.      "Asset
*-- Populate Object No, Asset and Sub Asset
          ls_cobrb_buf-anln1 = ls_prodord-empge.
          SELECT SINGLE anln2 objnr
            INTO ( ls_cobrb_buf-anln2, ls_cobrb_buf-rec_objnr1 )
            FROM anla
           WHERE bukrs EQ is_header_dialog-bukrs
             AND anln1 EQ ls_prodord-empge.

        WHEN zcl_abs_abap_maintain=>c_obj_typ_cost_center.  "Cost Center
*-- Populate Cost Center and Object No
          ls_cobrb_buf-kostl = ls_prodord-empge.
          SELECT SINGLE objnr
            INTO ls_cobrb_buf-rec_objnr1
            FROM csks
           WHERE kokrs EQ is_header_dialog-kokrs
             AND kostl EQ ls_prodord-empge.

        WHEN zcl_abs_abap_maintain=>c_obj_typ_material.    "Material
*-- Populate Material
          ls_cobrb_buf-matnr = ls_prodord-empge.
          ls_cobrb_buf-rec_objnr1 =
                             zcl_abs_abap_maintain=>c_obj_typ_material.

        WHEN zcl_abs_abap_maintain=>c_obj_typ_wbs_element.  "WBS Element
*-- Populate WBS element
          ls_cobrb_buf-ps_psp_pnr = ls_prodord-empge.
          SELECT SINGLE objnr
            INTO ls_cobrb_buf-rec_objnr1
            FROM prps
           WHERE posid EQ ls_prodord-empge.

        WHEN zcl_abs_abap_maintain=>c_obj_typ_gl_account.  "G/L Account
*-- Populate G/L Account No
          ls_cobrb_buf-hkont = ls_prodord-empge.

        WHEN zcl_abs_abap_maintain=>c_obj_typ_sales_doc.  "Sales Order
*-- Populate Sales Order
          ls_cobrb_buf-prctr = ls_prodord-empge.
          ls_cobrb_buf-rec_objnr1 =
                            zcl_abs_abap_maintain=>c_obj_typ_sales_doc.

      ENDCASE.

      ls_cobrb_buf-objnr = is_header_dialog-objnr.
      ls_cobrb_buf-lfdnr = 1.
      ls_cobrb_buf-perbz = zcl_abs_abap_maintain=>c_settle_typ_periodic.
      ls_cobrb_buf-prozs = 100.
      ls_cobrb_buf-avorg = zcl_abs_abap_maintain=>c_settle_trans_actual.
      ls_cobrb_buf-konty = lv_obart.
      ls_cobrb_buf-kokrs = is_header_dialog-kokrs.
      ls_cobrb_buf-bukrs = is_header_dialog-bukrs.
      ls_cobrb_buf-extnr = 1.
      ls_cobrb_buf-uflag = zcl_abs_abap_maintain=>c_updkz_insert.
      INSERT ls_cobrb_buf INTO TABLE <lt_cobrb_buf>.

      CLEAR ls_cobrb_buf.
      IF lv_obart = zcl_abs_abap_maintain=>c_obj_typ_order. "Yard Order
*-- Populate settlement rule item
        SELECT SINGLE *
          INTO ls_cobrb_buf
          FROM cobrb
         WHERE objnr = lv_objnr.
        IF sy-subrc EQ 0.
          INSERT ls_cobrb_buf INTO TABLE <lt_cobrb_buf>.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD if_ex_workorder_update~before_update.

*-- Internal Tables
  DATA:
    lt_status TYPE tt_status.

*-- Workareas
  DATA:
    ls_prodord  TYPE /agri/s_glprodord,
    ls_stat_new TYPE cobai_s_status,
    ls_header   TYPE cobai_s_header,
    ls_status   TYPE jstat.

*-- Field Symbols
  FIELD-SYMBOLS:
    <lt_stat_new> TYPE cobai_t_status.

*-- BOC-T_T.KONNO
  DATA: lt_abap_stack TYPE abap_callstack,
        lt_sys_stack  TYPE sys_callst.

  CALL FUNCTION 'SYSTEM_CALLSTACK'
    IMPORTING
      callstack    = lt_abap_stack
      et_callstack = lt_sys_stack.

  DATA(lv_vistex) = abap_false.
  LOOP AT lt_abap_stack INTO DATA(lwa_abap_stack).
    IF lwa_abap_stack-mainprogram CS '/AGRI/'.
      lv_vistex = abap_true.
    ENDIF.
  ENDLOOP.
*-- EOC-T_T.KONNO

  IF ( sy-tcode(6) = zcl_abs_abap_maintain=>c_agri_name_space OR
       sy-cprog    = 'ZFMFP_UNPLANNED_TASKORDER' OR
       lv_vistex   = abap_true OR
       sy-cprog(6) = zcl_abs_abap_maintain=>c_agri_name_space ).

    IMPORT is_prodord TO ls_prodord FROM MEMORY ID 'IS_PRODORD'.
    FREE MEMORY ID 'IS_PRODORD'.

    IF ls_prodord-konty IS NOT INITIAL.
      CALL FUNCTION 'K_SETTLEMENT_RULE_SAVE_ALL'.
    ENDIF.

    READ TABLE it_header INTO ls_header INDEX 1.

    IF ls_prodord-konty IS NOT INITIAL.
      ASSIGN ('(SAPLCOBT)STAT_NEW[]') TO <lt_stat_new>.
      IF <lt_stat_new> IS ASSIGNED.
        CLEAR ls_status.
        REFRESH lt_status.
        ls_status-stat  = zcl_abs_abap_maintain=>c_staus.
        ls_status-inact = ''.
        INSERT ls_status INTO TABLE lt_status.

        CALL FUNCTION 'STATUS_CHANGE_INTERN'
          EXPORTING
            objnr            = ls_header-objnr
          TABLES
            status           = lt_status
          EXCEPTIONS
            object_not_found = 0.

        ls_stat_new-objnr = ls_header-objnr.
        ls_stat_new-stat  = zcl_abs_abap_maintain=>c_staus.
        ls_stat_new-chgnr = 1.
        INSERT ls_stat_new INTO TABLE <lt_stat_new>.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD if_ex_workorder_update~cmts_check.
ENDMETHOD.


METHOD if_ex_workorder_update~initialize.

*-- Internal Tables
  DATA:
     lrt_auart TYPE RANGE OF aufart.

*-- Workareas
  DATA:
    ls_prodord TYPE /agri/s_glprodord.

*-- Field Symbols
  FIELD-SYMBOLS:
    <ls_rc27s> TYPE rc27s,
    <lt_range> TYPE STANDARD TABLE.

*-- References
  DATA:
    lo_range   TYPE REF TO data.

*-- BOC-T_T.KONNO
  DATA: lt_abap_stack TYPE abap_callstack,
        lt_sys_stack  TYPE sys_callst.

  CALL FUNCTION 'SYSTEM_CALLSTACK'
    IMPORTING
      callstack    = lt_abap_stack
      et_callstack = lt_sys_stack.

  DATA(lv_vistex) = abap_false.
  LOOP AT lt_abap_stack INTO DATA(lwa_abap_stack).
    IF lwa_abap_stack-mainprogram CS '/AGRI/'.
      lv_vistex = abap_true.
    ENDIF.
  ENDLOOP.
*-- EOC-T_T.KONNO

  IF ( sy-tcode(6) EQ zcl_abs_abap_maintain=>c_agri_name_space   OR
       sy-cprog    EQ 'ZFMFP_UNPLANNED_TASKORDER' OR
       lv_vistex   EQ abap_true OR
       sy-cprog(6) EQ zcl_abs_abap_maintain=>c_agri_name_space ).

**-- Get Yard Order types from variant table
*    CREATE DATA lo_range LIKE lrt_auart.
*    IF lo_range IS BOUND.
**-- Fetch Variant Table Data
*      CALL METHOD zcl_abs_get_variants=>get_range_constants
*        EXPORTING
*          iv_mod   = zcl_abs_abap_maintain=>c_cus_mode
*          iv_objid = zcl_abs_abap_maintain=>c_objid_order_create
*          iv_k1val = zcl_abs_abap_maintain=>c_key_yard_ord_types
*        IMPORTING
*          eo_range = lo_range.

*      ASSIGN lo_range->* TO <lt_range>.
*      IF <lt_range> IS ASSIGNED.
*        lrt_auart = <lt_range>.
*      ENDIF.
*    ENDIF.

*    IF lrt_auart IS NOT INITIAL AND is_caufvdb-auart IN lrt_auart.
*      RETURN.
*    ENDIF.

    CHECK is_caufvdb-auart NE 'ZY00'.

    IMPORT is_prodord TO ls_prodord FROM MEMORY ID 'IS_PRODORD'.

    CHECK ls_prodord-konty IS NOT INITIAL.

    ASSIGN ('(SAPLCOKO1)RC27S') TO <ls_rc27s>.
    IF <ls_rc27s> IS ASSIGNED.
      <ls_rc27s>-flg_colord_expand = abap_true.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD if_ex_workorder_update~in_update.


ENDMETHOD.


METHOD if_ex_workorder_update~number_switch.
ENDMETHOD.


METHOD if_ex_workorder_update~reorg_status_activate.
ENDMETHOD.


METHOD if_ex_workorder_update~reorg_status_act_check.
ENDMETHOD.


METHOD if_ex_workorder_update~reorg_status_revoke.
ENDMETHOD.
ENDCLASS.
