*----------------------------------------------------------------------*
***INCLUDE LZABS_FG_FMWOMF01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form DB_UPDATE
*&---------------------------------------------------------------------*
*& DB_UPDATE
*&---------------------------------------------------------------------*
FORM db_update  TABLES  lt_insert
                        lt_update
                USING   lv_tablename
                        et_messages TYPE esp1_message_tab_type.

  DATA : lwa_messages TYPE esp1_message_wa_type.

*--Insert
  IF NOT lt_insert[] IS INITIAL.
    INSERT (lv_tablename) FROM TABLE lt_insert.          "#EC CI_DYNTAB
*    IF sy-subrc = 0.
*      lwa_messages-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
*      lwa_messages-msgno = 125.
*      lwa_messages-msgty = zcl_abs_abap_maintain=>c_msgty_success.
*      APPEND lwa_messages TO et_messages.
*      CLEAR lwa_messages.
*    ENDIF.
  ENDIF.

*--Update
  IF NOT lt_update[] IS INITIAL.
    UPDATE (lv_tablename) FROM TABLE lt_update.          "#EC CI_DYNTAB
*    IF sy-subrc = 0.
*      lwa_messages-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
*      lwa_messages-msgno = 125.
*      lwa_messages-msgty = zcl_abs_abap_maintain=>c_msgty_success.
*      APPEND lwa_messages TO et_messages.
*      CLEAR lwa_messages.
*    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form NUMBER_RANGE
*&---------------------------------------------------------------------*
*& NUMBER_RANGE
*&---------------------------------------------------------------------*
FORM number_range CHANGING lwa_var TYPE ty_var.

*--Local Declarations
  DATA : lt_constants  TYPE zabs_tty_vkey_const,
         lwa_constants TYPE zabs_str_vkey_const,
         lv_nrlevel    TYPE nrlevel.

*--Get variant table data
  CALL METHOD zcl_abs_get_variants=>get_constant_multiple
    EXPORTING
      iv_mod       = space
      iv_objid     = zcl_abs_abap_maintain=>c_objid_nursery_wb   "'NSWB'
      iv_k1val     = zcl_abs_abap_maintain=>c_key_batch_nr       "'BNOR'
    IMPORTING
      et_constants = lt_constants.

  CLEAR lwa_constants.
*--Getting constant value table record based on flag x
  READ TABLE lt_constants INTO lwa_constants
                          WITH KEY cnval3 = abap_true. "'X'
  IF sy-subrc = 0.

    lwa_var-contr   = lwa_constants-cnval1.   "'01'

*--Calling FM NUmber range: Assigns next free number
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = lwa_var-contr                        "'01'
        object                  = zcl_abs_abap_maintain=>c_obj_ord_nr  "'ZABS_ORDER'
      IMPORTING
        number                  = lwa_var-next_no
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form WO_CREATE
*&---------------------------------------------------------------------*
*& WO_CREATE
*&---------------------------------------------------------------------*
FORM wo_create USING lt_tplnr_new TYPE /agri/t_gltplnr
                     lt_fphdr_new TYPE /agri/t_fmfphdr
                     i_farm       TYPE iwerk
            CHANGING lt_messages  TYPE /agri/t_gprolog
                     lt_wrk_ord   TYPE zabs_tty_wrkord.

*--Local declarations
  DATA : lt_fpitm_new     TYPE /agri/t_fmfpitm,
         lt_messages_temp TYPE /agri/t_gprolog,
         lwa_messages     TYPE /agri/s_gprolog,
         lwa_woitm_new    TYPE /agri/s_fmwoitm,
         lwa_fmwoc_doc    TYPE /agri/s_fmwoc_doc,
         lwa_wrk_ord      TYPE zabs_str_wrkord,
         lv_tabix         TYPE sy-tabix.

  IF lt_tplnr_new IS NOT INITIAL.

*--Fetching Process order data
    SELECT * FROM /agri/fmfphdr
      INTO CORRESPONDING FIELDS OF TABLE lt_fphdr_new
       FOR ALL ENTRIES IN lt_tplnr_new             "#EC CI_NO_TRANSFORM
     WHERE aufnr    IN ltr_aufnr
       AND autyp    EQ zcl_abs_abap_maintain=>c_autyp_tsk_ord "'TO'
       AND matnr    IN ltr_tmatnr
       AND tplnr_fl EQ lt_tplnr_new-tplnr_fl
       AND class    EQ zcl_abs_abap_maintain=>c_cs_farming_appl "'1'
       AND iwerk    EQ i_farm
       AND tecom    EQ space.
    IF lt_fphdr_new IS INITIAL.
      lwa_messages-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
      lwa_messages-msgno = 122.
      lwa_messages-msgty = zcl_abs_abap_maintain=>c_msgty_error.
      APPEND lwa_messages TO lt_messages.
    ELSE.
*-- Select FMFPITM details
      SELECT *
        FROM /agri/fmfpitm
        INTO CORRESPONDING FIELDS OF
       TABLE lt_fpitm_new
         FOR ALL ENTRIES IN lt_fphdr_new
       WHERE aufnr = lt_fphdr_new-aufnr.
    ENDIF. "lt_fphdr_new

    SORT lt_fphdr_new BY matnr.
    DATA(lt_fphdr_new_temp) = lt_fphdr_new.
    DELETE ADJACENT DUPLICATES FROM lt_fphdr_new_temp COMPARING matnr.

    LOOP AT lt_fphdr_new_temp INTO DATA(lwa_fphdr_new_temp).
      READ TABLE lt_fphdr_new TRANSPORTING NO FIELDS
                              WITH KEY matnr = lwa_fphdr_new_temp-matnr
                              BINARY SEARCH.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      lv_tabix = sy-tabix.
      LOOP AT lt_fphdr_new INTO DATA(lwa_fphdr_new) FROM lv_tabix.
        IF lwa_fphdr_new-matnr <> lwa_fphdr_new_temp-matnr.
          EXIT.
        ENDIF.

        MOVE-CORRESPONDING lwa_fphdr_new TO lwa_woitm_new.
        lwa_woitm_new-wonum = TEXT-004.
        lwa_woitm_new-updkz = zcl_abs_abap_maintain=>c_updkz_insert. "'I'
        APPEND lwa_woitm_new TO lwa_fmwoc_doc-x-woitm.
      ENDLOOP.

      lwa_fmwoc_doc-wonum         = TEXT-004.
      lwa_fmwoc_doc-x-wohdr-wonum = TEXT-004.
      lwa_fmwoc_doc-x-wohdr-updkz = zcl_abs_abap_maintain=>c_updkz_insert. "'I'
      lwa_fmwoc_doc-x-wohdr-matnr = lwa_fphdr_new_temp-matnr. "'TFOR0350'."p_matnr
      lwa_fmwoc_doc-x-wohdr-wotyp = zcl_abs_abap_maintain=>c_wotyp_zwom."'ZWOM'"p_wotyp
      lwa_fmwoc_doc-x-wohdr-sappl = zcl_abs_abap_maintain=>c_wo_splappl_1. "'1'
      lwa_fmwoc_doc-x-wohdr-iwerk = i_farm.
      lwa_fmwoc_doc-x-wohdr-verid = zcl_abs_abap_maintain=>c_verid_0001. "'0001' "p_verid

*--Calling FM to create WorkOrders
      CALL FUNCTION '/AGRI/FMWO_CREATE'
        EXPORTING
          i_verid                 = zcl_abs_abap_maintain=>c_verid_0001 "'0001'
          is_wohdr                = lwa_fmwoc_doc-x-wohdr
          it_woitm                = lwa_fmwoc_doc-x-woitm
        IMPORTING
          es_fmwoc_doc            = lwa_fmwoc_doc
          et_messages             = lt_messages_temp
        EXCEPTIONS
          creation_failed         = 1
          no_documents_to_process = 2
          no_authorization        = 3
          OTHERS                  = 4.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      APPEND LINES OF lt_messages_temp TO lt_messages.

      lwa_wrk_ord-wonum = lwa_fmwoc_doc-wonum.
      lwa_wrk_ord-matnr = lwa_fphdr_new_temp-matnr.
      APPEND lwa_wrk_ord TO lt_wrk_ord.
      CLEAR lwa_wrk_ord.

    ENDLOOP. "lt_fphdr_new_temp
  ENDIF. "lt_tplnr_new

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
      date      = gwa_wo_cnf-cdate
      days      = lv_days
      months    = 0
      signum    = '-'
      years     = 0
    IMPORTING
      calc_date = lv_prevdate.

  IF lv_prevdate IS NOT INITIAL.
    PERFORM get_week_dates USING lv_prevdate CHANGING lv_pv_strdt lv_pv_enddt.
  ENDIF.

  IF gwa_wo_cnf-cdate IS NOT INITIAL.
    PERFORM get_week_dates USING gwa_wo_cnf-cdate CHANGING lv_cu_strdt lv_cu_enddt.
  ENDIF.

*--Fetching Order Numbers
  SELECT ordno, werks,
         equnr, datab,
         datbi, erzet
    FROM zabst_ordhdr
    INTO TABLE @DATA(lt_ordhdr)
   WHERE werks EQ @gwa_wo_cnf-werks
     AND equnr EQ @gwa_wo_cnf-equnr
     AND datab IN ( @lv_pv_strdt , @lv_cu_strdt )
     AND datbi IN ( @lv_pv_enddt , @lv_cu_enddt ).
  IF sy-subrc <> 0.
    PERFORM message_build USING '00' '001' zcl_abs_abap_maintain=>c_msgty_error TEXT-001. "'E'
    RETURN.
  ENDIF.

  LOOP AT lt_ordhdr INTO DATA(lwa_ordhdr_temp).

    IF lwa_ordhdr_temp-datab LE gwa_wo_cnf-cdate
   AND lwa_ordhdr_temp-datbi GE gwa_wo_cnf-cdate.
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

**--Filling basic display fields
*  READ TABLE gwa_order-ordhdr INTO lwa_ordhdr INDEX 1.
*  IF sy-subrc = 0.
*    is_woconf-ordno = lwa_ordhdr-ordno.
*    is_woconf-werks = lwa_ordhdr-werks.
*    is_woconf-equnr = lwa_ordhdr-equnr.
*    is_woconf-cdate = p_date. "lwa_ordhdr-odate.
*    is_woconf-idate = lwa_wo_cnf-cdate.
*    is_woconf-fdate = lwa_wo_cnf-cdate.
*    is_woconf-irrqty_uom = zcl_abs_abap_maintain=>c_uom. "'M3'
**      APPEND lwa_wo_cnf TO gt_wo_cnf.
**      CLEAR lwa_wo_cnf.
*  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form PROCESS_DATA
*&---------------------------------------------------------------------*
*& PROCESS_DATA
*&---------------------------------------------------------------------*
FORM process_data.

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
*         lwa_wo_con   TYPE zabst_wo_con,
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

  IF gt_messages IS NOT INITIAL.
    EXIT.
  ENDIF.

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

    SORT gt_supply BY wonum vornr matnr.
  ENDIF.

*  IF gt_shift IS INITIAL.
*--Fetching Operations data
*    PERFORM operations_data CHANGING gt_shift.
*  ENDIF. "gt_shift

*  READ TABLE gt_wo_cnf ASSIGNING FIELD-SYMBOL(<fs_wo_cnf>) INDEX 1.
*  IF sy-subrc = 0.
  IF gwa_wo_cnf IS NOT INITIAL.

*    READ TABLE gt_supply TRANSPORTING NO FIELDS
*                         WITH KEY vornr = gwa_wo_cnf-vornr
*                         BINARY SEARCH.
*    IF sy-subrc = 0.
*      lv_tabix = sy-tabix.
*
*      READ TABLE gwa_order-orditm INTO DATA(lwa_ordhdr) INDEX 1.
*
*      REFRESH gt_wo_con.
*      LOOP AT gt_supply INTO lwa_supply FROM lv_tabix.
*        IF lwa_supply-vornr <> gwa_wo_cnf-vornr.
*          EXIT.
*        ENDIF.
*
**        IT_WOCON-vornr   = lwa_supply-vornr.
**        IT_WOCON-contr   = lwa_supply-contr.
*        lwa_wocon-ordno   = lwa_ordhdr-ordno.
*        lwa_wocon-supply  = lwa_supply-matnr.
**        lwa_wocon-maktx   = lwa_supply-maktx.
*        lwa_wocon-uom     = lwa_supply-erfme.
*
*        COLLECT lwa_wocon INTO it_wocon.
*        CLEAR lwa_wo_con.
*
*      ENDLOOP. " gt_supply
*
*    ENDIF. "gt_supply

*--To fill Target Quantity based on Operation
    LOOP AT gwa_order-orditm INTO DATA(lwa_ordhdr).

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
                              WITH KEY equnr    = gwa_wo_cnf-equnr
                                       tplnr_fl = lwa_woitm-tplnr_fl
                              BINARY SEARCH.
        IF sy-subrc IS NOT INITIAL.
          CONTINUE.
        ENDIF.

*-- Based on shifts
        CASE gwa_wo_cnf-vornr.
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

      IF lwa_comsplit IS NOT INITIAL.
*-- Collecting individual work order data
        APPEND lwa_comsplit TO gt_comsplit.
        CLEAR lwa_comsplit.
      ENDIF.

    ENDLOOP."gwa_order-orditm

*--Filling Total Order Quantity
    IF gwa_wo_cnf-gamng IS INITIAL.
      gwa_wo_cnf-gamng = lv_tqty.
    ENDIF.

*--Filling Confirmation Quantity
    IF gwa_wo_cnf-lmnga IS INITIAL.
      gwa_wo_cnf-lmnga = lv_sup_qty.
    ENDIF.

    gwa_wo_cnf-gmein = lwa_fmfphdr-gmein.
    CLEAR lv_tqty.

*--Filling Initial Counter
    READ TABLE gt_ordcnf INTO DATA(lwa_ordcnf) INDEX 1.
    IF sy-subrc = 0 AND gwa_wo_cnf-icontr_hm IS INITIAL.
      gwa_wo_cnf-icontr_hm = lwa_ordcnf-fcontr_hm.
    ENDIF.

*--Calculate Total Irrigation Quantity
*--Fetching custom field for calculation
    SELECT equnr,
           irtyp,
           zzhydfac
      FROM /agri/fmirhdr
      INTO TABLE @DATA(lt_fmirhdr)
     WHERE equnr EQ @gwa_wo_cnf-equnr
       AND irtyp EQ @zcl_abs_abap_maintain=>c_irrtyp. "'001'
    IF sy-subrc <> 0.
      PERFORM message_build USING '00' '002' zcl_abs_abap_maintain=>c_msgty_error TEXT-002. "'E'
      RETURN.
    ENDIF.

*    READ TABLE lt_fmirhdr INTO DATA(lwa_fmirhdr)
*    WITH KEY equnr =  gwa_wo_cnf-equnr.
*    IF sy-subrc = 0.
*      gwa_wo_cnf-tot_iqty = ( gwa_wo_cnf-fcontr_hm - gwa_wo_cnf-icontr_hm )
*                              * lwa_fmirhdr-zzhydfac.
*    ENDIF.

*    READ TABLE gt_mod TRANSPORTING NO FIELDS WITH KEY row_id = 1
*                                                      fieldname = 'LMNGA'.
*    IF sy-subrc NE 0 OR gwa_wo_cnf-lmnga IS INITIAL.
**      <fs_wo_cnf>-lmnga = lv_sup_qty.
*    ENDIF.

    IF gwa_wo_cnf-lmnga GT gwa_wo_cnf-gamng.
*      MESSAGE TEXT-003 TYPE zcl_abs_abap_maintain=>c_msgty_error. "'E'
      PERFORM message_build USING '00' '003' zcl_abs_abap_maintain=>c_msgty_error TEXT-003. "'E'
      RETURN.
    ENDIF.

  ENDIF. "gt_wo_cnf

ENDFORM.

*&---------------------------------------------------------------------*
*& Form GLOB_DATA_ENTR
*&---------------------------------------------------------------------*
*& GLOB_DATA_ENTR
*&---------------------------------------------------------------------*
FORM glob_data_entr.

  DATA:lwa_messages     TYPE /agri/s_gprolog.
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
      PERFORM message_build USING '00' '005' zcl_abs_abap_maintain=>c_msgty_error TEXT-005. "'E'
      RETURN.
    ENDIF.
    SORT gt_woitm BY wonum.

  ENDIF. "gwa_order-orditm

*--Fetching process order header data to get target quantity
  SELECT aufnr tplnr_fl
         gamng gmein tecom
    FROM /agri/fmfphdr
    INTO TABLE gt_fmfphdr
     FOR ALL ENTRIES IN gt_woitm
   WHERE aufnr EQ gt_woitm-aufnr
     AND autyp EQ zcl_abs_abap_maintain=>c_autyp_tsk_ord. "'TO'
  IF sy-subrc <> 0.
    PERFORM message_build USING '00' '006' zcl_abs_abap_maintain=>c_msgty_error TEXT-006. "'E'
    RETURN.
  ENDIF.

  IF gt_fmfphdr IS NOT INITIAL.
    DATA(lt_fphdr) = gt_fmfphdr[].
    DELETE lt_fphdr WHERE tecom IS INITIAL.
    IF lt_fphdr IS NOT INITIAL.
      LOOP AT lt_fphdr INTO DATA(ls_fmfphd).
        lwa_messages-msgid = '/AGRI/FMFP'.
        lwa_messages-msgno = '051'.
        lwa_messages-msgty = 'E'.
        lwa_messages-msgv1 = ls_fmfphd-aufnr.
        APPEND lwa_messages TO gt_messages.
        CLEAR lwa_messages.
      ENDLOOP.
      RETURN.
    ENDIF.
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
   WHERE equnr    EQ gwa_wo_cnf-equnr
     AND tplnr_fl EQ lt_fmfphdr_temp-tplnr_fl.
  IF sy-subrc <> 0.
    PERFORM message_build USING '00' '007' zcl_abs_abap_maintain=>c_msgty_error TEXT-007. "'E'
    RETURN.
  ENDIF.
  SORT gt_fmirflo BY equnr tplnr_fl.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form POSTING_ORDERS
*&---------------------------------------------------------------------*
*& POSTING_ORDERS
*&---------------------------------------------------------------------*
FORM posting_orders.

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
         lv_commit_true  TYPE c,
         lv_wtr_qty      TYPE ru_ismng,
         lv_contr        TYPE i,
         lv_per_quantity TYPE /agri/fmlmnga,
         lv_posnr        TYPE i.

  SORT gt_task BY wonum.

*-- Build tables for posting the confirmations
*--Splitting Usage quantity based on Shifts
*  READ TABLE gt_wo_cnf INTO DATA(lwa_wo_cnf) INDEX 1.
*  IF sy-subrc = 0.

  IF gwa_wo_cnf IS NOT INITIAL.

    SELECT MAX( contr )
      FROM zabst_ordcnf
      INTO @DATA(lv_cont)
     WHERE ordno EQ @gwa_wo_cnf-ordno.
    IF sy-subrc = 0.
      lv_cont = lv_cont + 1.
    ENDIF.

**--Validating Selection Fields
*    PERFORM validation USING lwa_wo_cnf
*                    CHANGING lt_msgtab.

*    IF lt_msgtab IS INITIAL.

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
        IF gwa_wo_cnf-gamng IS NOT INITIAL.
*--Calculating Confirmation Quantity at Task Level
          <lfs_task>-lmnga = <lfs_task>-gamng * gwa_wo_cnf-lmnga / gwa_wo_cnf-gamng.
          <lfs_comsplit>-lmnga = <lfs_comsplit>-lmnga + <lfs_task>-lmnga.
        ENDIF.

      ENDLOOP. "gt_task

    ENDLOOP. "gt_comsplit

    CLEAR lwa_cust_cnf.
    lwa_cust_cnf-ordno = gwa_wo_cnf-ordno.
    lwa_cust_cnf-contr = lv_cont.
    lwa_cust_cnf-vornr = gwa_wo_cnf-vornr.
    lwa_cust_cnf-cdate = gwa_wo_cnf-cdate.
    lwa_cust_cnf-gamng = gwa_wo_cnf-gamng.
    lwa_cust_cnf-lmnga = gwa_wo_cnf-lmnga.
    lwa_cust_cnf-gmein = gwa_wo_cnf-gmein.
    lwa_cust_cnf-idate = gwa_wo_cnf-idate.
    lwa_cust_cnf-fdate = gwa_wo_cnf-fdate.
    lwa_cust_cnf-ihour = gwa_wo_cnf-ihour.
    lwa_cust_cnf-fhour = gwa_wo_cnf-fhour.
    lwa_cust_cnf-icontr_hm = gwa_wo_cnf-icontr_hm.
    lwa_cust_cnf-fcontr_hm = gwa_wo_cnf-fcontr_hm.
    lwa_cust_cnf-tot_iqty  = gwa_wo_cnf-tot_iqty.
    lwa_cust_cnf-uom       = gwa_wo_cnf-irrqty_uom.
    lwa_cust_cnf-appl      = gwa_wo_cnf-appl.
    lwa_cust_cnf-imei      = gwa_wo_cnf-imei.
    lwa_cust_cnf-badge     = gwa_wo_cnf-badge.
    APPEND lwa_cust_cnf TO lwa_update-ordcnf.

    IF gwa_wo_cnf-lmnga GT gwa_wo_cnf-gamng.
      PERFORM message_build USING '00' '003' zcl_abs_abap_maintain=>c_msgty_error TEXT-003. "'E'
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
        lwa_fmfpcnf-vornr     = gwa_wo_cnf-vornr.
        lwa_fmfpcnf-budat     = gwa_wo_cnf-cdate.
        lwa_fmfpcnf-gicre     = abap_true.

        CLEAR lv_wtr_qty.
        IF gwa_wo_cnf-lmnga IS NOT INITIAL.
          lv_wtr_qty = gwa_wo_cnf-tot_iqty * lwa_task-lmnga / gwa_wo_cnf-lmnga.
        ENDIF.

        lwa_fmfpcnf-ism01 = lv_wtr_qty.
        lwa_fmfpcnf-leinh1 = zcl_abs_abap_maintain=>c_uom. "'M3'

        CLEAR lwa_cust_grp.
        lv_posnr = lv_posnr + 1.
        lwa_cust_grp-ordno = gwa_wo_cnf-ordno.
        lwa_cust_grp-posnr = lv_posnr.
        lwa_cust_grp-contr = lv_cont.
        lwa_cust_grp-vornr = gwa_wo_cnf-vornr.
        lwa_cust_grp-wonum = lwa_comsplit-wonum.
        lwa_cust_grp-appl  = gwa_wo_cnf-appl.
        lwa_cust_grp-imei  = gwa_wo_cnf-imei.
        lwa_cust_grp-badge = gwa_wo_cnf-badge.
        APPEND lwa_cust_grp TO lwa_update_tmp-ordgrp.

        APPEND lwa_fmfpcnf TO lt_fmfpcnf.
        CLEAR lwa_fmfpcnf.

        CLEAR lv_contr.
        READ TABLE gt_supply TRANSPORTING NO FIELDS
                            WITH KEY wonum = lwa_comsplit-wonum
                                     vornr = gwa_wo_cnf-vornr
                            BINARY SEARCH.
        lv_tabix = sy-tabix.
        LOOP AT gt_supply INTO DATA(lwa_supply) FROM lv_tabix.
          IF lwa_supply-wonum <> lwa_comsplit-wonum
          OR lwa_supply-vornr <> gwa_wo_cnf-vornr.
            EXIT.
          ENDIF.

          READ TABLE gt_wocon_imp INTO DATA(lwa_wo_con)
          WITH KEY matnr = lwa_supply-matnr.
          IF sy-subrc IS NOT INITIAL
          OR lwa_wo_con-esmng IS INITIAL.
            CONTINUE.
          ENDIF.

*--FMFPCOM Filling
          lwa_fmfpcom-aufnr = lwa_task-aufnr.
          lwa_fmfpcom-contr = lwa_supply-contr.
          lwa_fmfpcom-vornr = lwa_supply-vornr.
          lwa_fmfpcom-matnr = lwa_supply-matnr.
          lwa_fmfpcom-erfmg = lwa_supply-esmng. "lwa_wo_con-esmng.
          lwa_fmfpcom-esmng = lwa_supply-esmng. "lwa_wo_con-esmng.
          IF gwa_wo_cnf-lmnga IS NOT INITIAL.
            lwa_fmfpcom-lmnga = ( lwa_task-lmnga * lwa_wo_con-esmng ) / gwa_wo_cnf-lmnga.
          ENDIF.

          lwa_fmfpcom-maktx = lwa_supply-maktx.
          lwa_fmfpcom-bwart = zcl_abs_abap_maintain=>c_movement_typ_gi_order. "'261'
          lwa_fmfpcom-werks = gwa_wo_cnf-werks.
          lwa_fmfpcom-lgort = zcl_abs_abap_maintain=>c_verid_0001. "'0001'

          CLEAR lwa_cust_con.
          lwa_cust_con-ordno = lwa_wo_con-ordno.
          lwa_cust_con-contr = lv_cont.
          lwa_cust_con-supply = lwa_supply-matnr.
          lwa_cust_con-vornr = lwa_wo_con-vornr.
          lwa_cust_con-supply_qty = lwa_fmfpcom-lmnga.
          lwa_cust_con-uom   = lwa_wo_con-erfme.
          lwa_cust_con-appl  = gwa_wo_cnf-appl.
          lwa_cust_con-imei  = gwa_wo_cnf-imei.
          lwa_cust_con-badge = gwa_wo_cnf-badge.
          COLLECT lwa_cust_con INTO lwa_update-ordcon ASSIGNING FIELD-SYMBOL(<lfs_con>).

          APPEND lwa_fmfpcom TO lt_fmfpcom.
          CLEAR lwa_fmfpcom.

        ENDLOOP.  "gt_supply
      ENDLOOP.  "gt_task

*--Calling FM for Posting

      CLEAR lv_commit_true.
      REFRESH lt_messages[].
      DO 5 TIMES.
        CHECK lv_commit_true IS INITIAL.

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

          WAIT UP TO 1 SECONDS.
          lv_commit_true = 'X'.
          EXIT.
        ENDIF.
        WAIT UP TO 1 SECONDS.

      ENDDO.

*      CALL FUNCTION '/AGRI/FMFP_ORDER_CONFIRM'
*        EXPORTING
*          i_commit_work     = abap_true
*          i_wonum           = lwa_comsplit-wonum
*          it_fmfpcnf        = lt_fmfpcnf
*          it_fmfpcom        = lt_fmfpcom
*        IMPORTING
*          e_subrc           = lv_subrc
*          et_fpdoc          = lt_fpdoc
*          et_messages       = lt_messages
*        EXCEPTIONS
*          inconsistent_data = 1
*          OTHERS            = 2.
*      IF lv_subrc = 0.
*        ASSIGN ('(/AGRI/SAPLFMFPM)GS_FPOC_DOC-OCNUM') TO FIELD-SYMBOL(<fs_ocnum>).
*        IF <fs_ocnum> IS ASSIGNED AND <fs_ocnum> IS NOT INITIAL.
*          LOOP AT lwa_update_tmp-ordgrp ASSIGNING FIELD-SYMBOL(<fs_ordgrp>).
*            <fs_ordgrp>-ocnum = <fs_ocnum>.
*          ENDLOOP.
*        ENDIF.
*
*        APPEND LINES OF lwa_update_tmp-ordgrp TO lwa_update-ordgrp.
*
*      ENDIF.

      LOOP AT lt_messages INTO DATA(lwa_messages).
        IF lwa_messages-msgid = '/AGRI/FMFP' AND
           lwa_messages-msgno = '051'.
          lwa_messages-msgty = 'E'.
        ENDIF.
*          MOVE-CORRESPONDING lwa_messages TO lwa_msgtab.
*          APPEND lwa_msgtab TO lt_msgtab.
*          CLEAR lwa_msgtab.
        APPEND lwa_messages TO gt_messages.
        CLEAR lwa_messages.
      ENDLOOP.

      REFRESH : lt_fmfpcnf, lt_fmfpcom, lt_fpdoc, lt_messages.
      CLEAR : lv_subrc,
              lwa_update_tmp.

    ENDLOOP.  "gt_comsplit
*    ENDIF.  "lt_msgtab

  ENDIF. "gt_wo_cnf

*--Updating Order Group table
  IF lwa_update-ordgrp IS NOT INITIAL.
    PERFORM table_update USING lwa_update.
  ENDIF.

*  IF lt_msgtab IS NOT INITIAL.
**--Message display sub-routine
*    PERFORM messages_display USING lt_msgtab.
*  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form TABLE_UPDATE
*&---------------------------------------------------------------------*
*& TABLE_UPDATE
*&---------------------------------------------------------------------*
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
*& Form REFRESH_GLOBAL_DATA
*&---------------------------------------------------------------------*
*& REFRESH_GLOBAL_DATA
*&---------------------------------------------------------------------*
FORM refresh_global_data.

  REFRESH   : gt_wo_cnf,
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
              gt_wo_dbcnf,
              gt_messages,
              gt_wocon_imp.
  CLEAR : gwa_order,
          gwa_wo_cnf.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form MESSAGE_BUILD
*&---------------------------------------------------------------------*
*& MESSAGE_BUILD
*&---------------------------------------------------------------------*
FORM message_build  USING  msgid
                           msgno
                           msgty
                           msgv1.

  DATA : lwa_messages TYPE /agri/s_gprolog.

  lwa_messages-msgid = msgid.
  lwa_messages-msgno = msgno.
  lwa_messages-msgty = msgty.
  lwa_messages-msgv1 = msgv1.

  APPEND lwa_messages TO gt_messages.
  CLEAR lwa_messages.

ENDFORM.
