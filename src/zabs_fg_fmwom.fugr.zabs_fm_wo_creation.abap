FUNCTION zabs_fm_wo_creation.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_FARM) TYPE  IWERK
*"     REFERENCE(I_EQUNR) TYPE  EQUNR
*"     REFERENCE(I_DATE) TYPE  DATUM DEFAULT SY-DATUM
*"  EXPORTING
*"     VALUE(ET_MESSAGES) TYPE  /AGRI/T_GPROLOG
*"     REFERENCE(EV_ORDNO) TYPE  /AGRI/FMFPNUM
*"----------------------------------------------------------------------

*--Local variable declarations
  DATA: lv_set_infocus,
        lv_tabix        TYPE sy-tabix,
        lv_tmatnr       TYPE matnr,
        lv_constant     TYPE zabs_del_cnval,
        lv_gangcd_new   TYPE /agri/s_fmfphdr-gangcd,
        lv_key          TYPE char1,
        lv_lines        TYPE i,
        lv_create_to    TYPE c,
        lv_unplan       TYPE c,
        lv_release      TYPE c,
        lv_contr        TYPE i,
        lv_new_posnr    TYPE co_posnr,
        lv_lines_x      TYPE sy-tabix,

        lwa_messages    TYPE /agri/s_gprolog,
        lwa_objects     TYPE bapi_pp_order_objects,
        lwa_fpdoc       TYPE /agri/s_fmfp_doc,
        lwa_vornr       TYPE ty_vornr,
        lwa_aufnr       TYPE /agri/s_fmaufnr,
        lwa_fphdr_new   TYPE /agri/s_fmfphdr,
        lwa_fpitm_new   TYPE /agri/s_fmfpitm,
        lwa_aufnr_to    TYPE /agri/s_fmaufnr,
        lwa_vauf        TYPE /agri/s_aufnr,
        lwa_oitm        TYPE /agri/s_fmfpitm,
        lwa_fpcom       TYPE /agri/s_fmfpcom,
        lwa_ordhdr      TYPE zabs_str_ordhdr,
        lwa_orditm      TYPE zabs_str_orditm,

        lt_wrk_ord      TYPE zabs_tty_wrkord,
        lt_messages     TYPE /agri/t_gprolog,
        lt_msg          TYPE esp1_message_tab_type,
        lt_aufnr        TYPE /agri/t_fmaufnr,
        lt_tskaufnr     TYPE /agri/t_fmaufnr,
        lt_fpdoc        TYPE /agri/t_fmfp_doc,
        lt_vornr        TYPE TABLE OF ty_vornr,
        lt_header       TYPE TABLE OF bapi_order_header1,
        lt_operations   TYPE TABLE OF bapi_order_operation1,
        lt_position     TYPE TABLE OF bapi_order_item,
        lt_resb         TYPE TABLE OF resbd,
        lt_tplnr_new    TYPE /agri/t_gltplnr,
        lt_fphdr_new    TYPE /agri/t_fmfphdr,
        lt_wodoc        TYPE /agri/t_fmwoc_doc,
        lt_order_header TYPE STANDARD TABLE OF bapi_order_header1,
        lt_aufnr_to     TYPE /agri/t_fmaufnr,
        lt_vauf         TYPE /agri/t_aufnr,
        lt_ordhdr       TYPE TABLE OF zabs_str_ordhdr,
        lt_orditm       TYPE TABLE OF zabs_str_orditm,
        lt_ordcnf       TYPE TABLE OF zabs_str_ordcnf,
        lt_ordcon       TYPE TABLE OF zabs_str_ordcon,
        lt_ordgrp       TYPE TABLE OF zabs_str_ordgrp.

*--Field-Symbols declaration
  FIELD-SYMBOLS: <ref_text>          TYPE REF TO /agri/cl_gtext_process,
                 <lfs_wodoc_infocus> TYPE /agri/s_fmwoc_doc.

*--Equipment Validation
  SELECT SINGLE equnr
    FROM /agri/fmirhdr
    INTO @DATA(lv_equnr)
   WHERE equnr EQ @i_equnr.
  IF sy-subrc <> 0.
    lwa_messages-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
    lwa_messages-msgno = 119.
    lwa_messages-msgv1 = i_equnr.
    lwa_messages-msgty = zcl_abs_abap_maintain=>c_msgty_error.
    APPEND lwa_messages TO et_messages.
    RETURN.
  ENDIF.

*--Terrain Validation
  SELECT a~tplnr_fl, b~strno
    FROM /agri/fmirflo AS a
    JOIN /agri/glflot  AS b
      ON a~tplnr_fl = b~tplnr_fl
    INTO TABLE @DATA(lt_tplnr)
   WHERE a~equnr EQ @i_equnr
     AND b~iwerk EQ @i_farm
     AND b~loevm EQ @space.
  IF sy-subrc <> 0.
    lwa_messages-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
    lwa_messages-msgno = 118.
    lwa_messages-msgv1 = i_equnr.
    lwa_messages-msgty = zcl_abs_abap_maintain=>c_msgty_error.
    APPEND lwa_messages TO et_messages.
    RETURN.
  ENDIF.

  lt_tplnr_new = lt_tplnr.

*--Crop Season Validation
  SELECT tplnr_fl,
         contr
    FROM /agri/glflcma
    INTO TABLE @DATA(lt_glflcma)
     FOR ALL ENTRIES IN @lt_tplnr
   WHERE tplnr_fl EQ @lt_tplnr-tplnr_fl
     AND ( datab  LE @i_date
     AND datbi    GE @i_date )
     AND loevm    EQ @space.
  IF sy-subrc <> 0.
    lwa_messages-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
    lwa_messages-msgno = 095.
    lwa_messages-msgty = zcl_abs_abap_maintain=>c_msgty_error.
    APPEND lwa_messages TO et_messages.
    RETURN.
  ENDIF.

*--Get Process/Task Materials from Variant table
  CALL METHOD zcl_abs_get_variants=>get_constant_multiple
    EXPORTING
      iv_mod       = space
      iv_objid     = zcl_abs_abap_maintain=>c_objid_irr_wb "'IRWO'
      iv_k1val     = zcl_abs_abap_maintain=>c_key_irr_po "'IRPO'
    IMPORTING
      et_constants = DATA(lt_constants).

  lr_tmatnr-sign = lr_pmatnr-sign = zcl_abs_abap_maintain=>c_rsign_include. "'I'
  lr_tmatnr-option = lr_pmatnr-option = zcl_abs_abap_maintain=>c_ropt_equal. "'EQ'
  LOOP AT lt_constants INTO DATA(lwa_constants).
    lr_pmatnr-low = lwa_constants-cnval1.
    APPEND lr_pmatnr TO ltr_pmatnr.
    lr_tmatnr-low = lwa_constants-cnval2.
    APPEND lr_tmatnr TO ltr_tmatnr.
  ENDLOOP.

*--Process Order Validtaion
  SELECT aufnr, matnr,
         rsnum, aufpl
    FROM /agri/fmfphdr
    INTO TABLE @DATA(lt_paufnr)
     FOR ALL ENTRIES IN @lt_glflcma
   WHERE autyp    EQ @zcl_abs_abap_maintain=>c_autyp_pro_ord "'AO'
     AND tplnr_fl EQ @lt_glflcma-tplnr_fl
     AND contr    EQ @lt_glflcma-contr
     AND matnr    IN @ltr_pmatnr
     AND tecom    EQ @space.
  IF sy-subrc <> 0.
    lwa_messages-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
    lwa_messages-msgno = 120.
    lwa_messages-msgv1 = i_equnr.
    lwa_messages-msgty = zcl_abs_abap_maintain=>c_msgty_error.
    APPEND lwa_messages TO et_messages.
    RETURN.
  ENDIF.

  LOOP AT lt_paufnr INTO DATA(lwa_auf).
    lwa_vauf-aufnr = lwa_auf-aufnr.
    APPEND lwa_vauf TO lt_vauf.
    CLEAR lwa_vauf.
  ENDLOOP.

*--Task Order Check
  SELECT a~aufnr, a~matnr,
         a~aufnr_v, b~vornr, a~tecom
    FROM /agri/fmfphdr AS a
    JOIN /agri/fmfpitm AS b
      ON a~aufnr = b~aufnr_to
    INTO TABLE @DATA(lt_taufnr)
     FOR ALL ENTRIES IN @lt_paufnr
   WHERE b~aufnr EQ @lt_paufnr-aufnr
     AND a~autyp EQ @zcl_abs_abap_maintain=>c_autyp_tsk_ord "'TO'
     AND a~matnr IN @ltr_tmatnr
     AND a~ostat EQ @space.
  IF sy-subrc = 0.
    lr_aufnr-sign   = zcl_abs_abap_maintain=>c_rsign_include. "'I'
    lr_aufnr-option = zcl_abs_abap_maintain=>c_ropt_equal. "'EQ'
    SORT lt_taufnr BY aufnr aufnr_v matnr vornr.
  ENDIF.

  SELECT aufnr, posnr,
         vornr, matnr
    FROM /agri/fmfpcom
    INTO TABLE @DATA(lt_fmfpcom)
     FOR ALL ENTRIES IN @lt_paufnr
   WHERE aufnr = @lt_paufnr-aufnr.
  IF sy-subrc = 0.
    SORT lt_fmfpcom BY aufnr posnr vornr.
  ENDIF.

*--Callinf FM for Farm Process Order View
  CALL FUNCTION '/AGRI/FMFP_VIEW'
    EXPORTING
      it_aufnr       = lt_vauf
    IMPORTING
      et_fpdoc       = lt_fpdoc
    EXCEPTIONS
      no_data_exists = 1
      OTHERS         = 2.

  IF ltr_tmatnr IS NOT INITIAL.
    DELETE lt_fmfpcom WHERE matnr NOT IN ltr_tmatnr.
  ENDIF.

  SORT lt_vornr BY aufnr.

  LOOP AT lt_fpdoc INTO lwa_fpdoc.

    CLEAR: lv_create_to,
           lv_release,
           lv_unplan,
           lwa_vornr.

    IF ltr_tmatnr IS NOT INITIAL.
      DELETE lwa_fpdoc-x-fpcom WHERE matnr NOT IN ltr_tmatnr.
    ENDIF.

    LOOP AT lwa_fpdoc-x-fpcom INTO lwa_fpcom.

      READ TABLE lwa_fpdoc-x-fpitm ASSIGNING FIELD-SYMBOL(<lwa_fpitm>)
                                   WITH KEY aufnr = lwa_fpcom-aufnr
                                            posnr = lwa_fpcom-posnr
                                            vornr = lwa_fpcom-vornr.
      IF sy-subrc EQ 0.
        READ TABLE lt_taufnr INTO DATA(lwa_task)
                             WITH KEY aufnr = <lwa_fpitm>-aufnr_to
                             BINARY SEARCH.
        IF sy-subrc EQ 0.
          IF lwa_task-tecom EQ abap_true.
            lv_unplan = abap_true.
            lv_create_to = abap_true.
          ELSE.
            lr_aufnr-sign   = zcl_abs_abap_maintain=>c_rsign_include. "'I'
            lr_aufnr-option = zcl_abs_abap_maintain=>c_ropt_equal. "'EQ'
            lr_aufnr-low    = <lwa_fpitm>-aufnr_to.
            APPEND lr_aufnr TO ltr_aufnr.
            lv_release = abap_true.
            CLEAR lv_create_to.
            lwa_vornr-vornr = <lwa_fpitm>-vornr.
            lwa_vornr-posnr = <lwa_fpitm>-posnr.
          ENDIF.
        ELSE.
          CLEAR lv_unplan.
          lv_create_to = abap_true.
          lwa_vornr-vornr = <lwa_fpitm>-vornr.
          lwa_vornr-posnr = <lwa_fpitm>-posnr.
        ENDIF. "lt_taufnr

      ENDIF. "lwa_fpdoc-x-fpitm
    ENDLOOP. "lwa_fpdoc-x-fpitm

    IF lv_create_to IS INITIAL
    OR lv_release IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    IF lv_unplan IS NOT INITIAL.

      DATA(lt_fpitm_del) = lwa_fpdoc-x-fpitm.
      DELETE lt_fpitm_del WHERE aufnr NE lwa_fpdoc-aufnr.
      DESCRIBE TABLE lt_fpitm_del LINES lv_lines.

      lwa_oitm-aufnr = <lwa_fpitm>-aufnr.
*--BOC T_T.KONNO-11.17.20
*     lwa_oitm-posnr = lv_lines + 1.
      lv_lines_x = lines( lt_fpitm_del ).
      lv_lines_x = lv_lines_x + 1.

      DO 5 TIMES.
        lv_new_posnr = lv_lines_x.
        READ TABLE lwa_fpdoc-x-fpcom TRANSPORTING NO FIELDS
          WITH KEY posnr = lv_new_posnr.
        IF sy-subrc EQ 0.
          ADD 1 TO lv_lines_x.
          CONTINUE.
        ENDIF.

        READ TABLE lt_fpitm_del TRANSPORTING NO FIELDS
          WITH KEY posnr = lv_new_posnr.
        IF sy-subrc EQ 0.
          ADD 1 TO lv_lines_x.
          CONTINUE.
        ENDIF.
        EXIT.
      ENDDO.
      lv_lines_x = lv_new_posnr.
      lwa_oitm-posnr = lv_lines_x.
*--EOC T_T.KONNO-11.17.20

      lwa_oitm-matnr = lwa_task-matnr.
      lwa_oitm-tomng = <lwa_fpitm>-tomng.
      lwa_oitm-meinh = <lwa_fpitm>-meinh.
      lwa_oitm-actdt = sy-datum.
      lwa_oitm-confm = abap_true.
      lwa_oitm-unpln = abap_true.
      lwa_oitm-updkz = zcl_abs_abap_maintain=>c_updkz_insert. "'I'

      APPEND lwa_oitm TO lwa_fpdoc-x-fpitm.
      MOVE-CORRESPONDING lwa_oitm TO lwa_fpcom.
      APPEND lwa_fpcom TO lwa_fpdoc-x-fpcom.

*--Calling sub-routine to create task orders
      PERFORM create_task_orders(/agri/saplfmfpm) CHANGING lwa_fpdoc
                                                           lv_set_infocus
                                                           IF FOUND.
      TRY.
          lr_aufnr-low = lwa_fpdoc-x-fpitm[ updkz = 'I' ]-aufnr_to.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

*--Success message if task order is created.
      IF lr_aufnr-low IS NOT INITIAL.
        lr_aufnr-sign   = zcl_abs_abap_maintain=>c_rsign_include. "'I'
        lr_aufnr-option = zcl_abs_abap_maintain=>c_ropt_equal. "'EQ'
        lwa_messages-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
        lwa_messages-msgno = 121.
        lwa_messages-msgv1 = lr_aufnr-low.
        lwa_messages-msgty = zcl_abs_abap_maintain=>c_msgty_success.
        APPEND lwa_messages TO et_messages.
        APPEND lr_aufnr TO ltr_aufnr.
      ENDIF.

    ELSE.

      <lwa_fpitm>-confm = abap_true.
      <lwa_fpitm>-schdl = abap_true.
      <lwa_fpitm>-schdt = sy-datum.
      <lwa_fpitm>-tomng = <lwa_fpitm>-gamng.

      CLEAR lwa_aufnr.

*--Calling sub-routine to create task orders
      PERFORM create_task_orders(/agri/saplfmfpm) CHANGING lwa_fpdoc
                                                           lv_set_infocus
                                                           IF FOUND.
      TRY.
          lr_aufnr-low = lwa_fpdoc-x-fpitm[ vornr = lwa_vornr-vornr
                                            posnr = lwa_vornr-posnr ]-aufnr_to.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

*--Success message if task order is created.
      IF lr_aufnr-low IS NOT INITIAL.
        lr_aufnr-sign   = zcl_abs_abap_maintain=>c_rsign_include. "'I'
        lr_aufnr-option = zcl_abs_abap_maintain=>c_ropt_equal. "'EQ'
        lwa_messages-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
        lwa_messages-msgno = 121.
        lwa_messages-msgv1 = lr_aufnr-low.
        lwa_messages-msgty = zcl_abs_abap_maintain=>c_msgty_success.
        APPEND lwa_messages TO et_messages.
        APPEND lr_aufnr TO ltr_aufnr.
      ENDIF.

    ENDIF. "lv_unplan

  ENDLOOP. "lt_fpdoc

*--Calling Sub-routine to create Work Orders
  PERFORM wo_create USING lt_tplnr_new
                          lt_fphdr_new
                          i_farm
                 CHANGING lt_messages
                          lt_wrk_ord.

  APPEND LINES OF lt_messages TO et_messages.

  READ TABLE lt_messages  TRANSPORTING NO FIELDS
                          WITH KEY msgty = zcl_abs_abap_maintain=>c_msgty_error. "'E'
  IF sy-subrc = 0.
    EXIT.
  ENDIF.

*--Populating Order Header table
  lwa_ordhdr-ordno = 1.
  lwa_ordhdr-werks = i_farm.
  lwa_ordhdr-equnr = i_equnr.

*--Date Convertion
  CALL FUNCTION 'GET_WEEK_INFO_BASED_ON_DATE'
    EXPORTING
      date   = i_date
    IMPORTING
      monday = lwa_ordhdr-datab
      sunday = lwa_ordhdr-datbi.

  lwa_ordhdr-updkz = zcl_abs_abap_maintain=>c_updkz_insert. "'I'

  APPEND lwa_ordhdr TO lt_ordhdr.
  CLEAR lwa_ordhdr.

*--Processing created workorders to fill order item table
  LOOP AT lt_wrk_ord INTO DATA(lwa_wrk_ord).

    lv_contr = lv_contr + 1.

    lwa_orditm-ordno = 1.
    lwa_orditm-posnr = lv_contr.
    lwa_orditm-wonum = lwa_wrk_ord-wonum.
    lwa_orditm-matnr = lwa_wrk_ord-matnr.
    lwa_orditm-updkz = zcl_abs_abap_maintain=>c_updkz_insert. "'I'
    APPEND lwa_orditm TO lt_orditm.
    CLEAR lwa_orditm.

  ENDLOOP. "lt_wrk_ord

  SORT : lt_ordhdr BY ordno,
         lt_orditm BY ordno.

*--Calling FM to update database tables
  CALL FUNCTION 'ZABS_FM_ORDER_DBUPDATE'
    EXPORTING
      it_ordhdr   = lt_ordhdr
      it_orditm   = lt_orditm
      it_ordcnf   = lt_ordcnf
      it_ordcon   = lt_ordcon
      it_ordgrp   = lt_ordgrp
    IMPORTING
      et_messages = lt_msg
    CHANGING
      ev_ordno    = ev_ordno.

  LOOP AT lt_msg INTO DATA(lwa_msg).
    lwa_messages-msgid = lwa_msg-msgid.
    lwa_messages-msgno = lwa_msg-msgno.
    lwa_messages-msgty = lwa_msg-msgty.
    lwa_messages-msgv1 = lwa_msg-msgv1.
    lwa_messages-msgv2 = lwa_msg-msgv2.
    lwa_messages-msgv3 = lwa_msg-msgv3.
    APPEND lwa_messages TO et_messages.
    CLEAR lwa_messages.
  ENDLOOP.

ENDFUNCTION.
