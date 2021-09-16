class ZCL_ZABS_MOB_NURSERY_DPC_EXT definition
  public
  inheriting from ZCL_ZABS_MOB_NURSERY_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_BEGIN
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_END
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_PROCESS
    redefinition .
protected section.

  methods NURAUDITREPORTSE_GET_ENTITYSET
    redefinition .
  methods NURBADGEROUTESET_GET_ENTITYSET
    redefinition .
  methods NURBTCHCNFMF4SET_GET_ENTITYSET
    redefinition .
  methods NURCHARSTICSF4SE_GET_ENTITYSET
    redefinition .
  methods NURCHARVALUEF4SE_GET_ENTITYSET
    redefinition .
  methods NURPROCESSORDSET_GET_ENTITYSET
    redefinition .
  methods NURTERRAINF4SET_GET_ENTITYSET
    redefinition .
  methods UOMF4SET_GET_ENTITYSET
    redefinition .
private section.

  methods GET_DELTA_TOKEN
    importing
      !IV_ENTITY_SET_NAME type STRING
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
    exporting
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
    changing
      !CT_ENTITYSET type STANDARD TABLE .
  methods GET_AUDIT_REPORT
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
    exporting
      !ET_AUDIT_REPORT type ZABS_TTY_NUR_AUDIT_REPT .
  methods GET_TERRAIN_F4
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
    exporting
      !ET_TERRAIN_F4 type ZABS_TTY_NUR_AUDIT_TRN_F4 .
  methods GET_BADGE_ROUTE
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_BADGE_ROUTE type ZABS_TTY_NUR_ROUTE_TERRAIN .
  methods GET_CHARACTERISTICS_F4
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
    exporting
      !ET_CHARSTICS type ZABS_TTY_NUR_CHARSTICS_F4 .
  methods ADD_MESSAGES_TO_MSG_CONTAINER
    importing
      !IV_ENTITY_NAME type STRING optional
      !IV_OPERATION type STRING optional
      !IV_MESSAGE_TEXT type BAPI_MSG optional
      !IV_EXCEPTION type XFELD optional
      !IS_MESSAGE type /AGRI/S_GPROLOG optional
      !IT_MESSAGES type /AGRI/T_GPROLOG optional
      !IT_BAPI_MESSAGES type BAPIRET2_T optional
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods GET_BTCHCNFM_F4
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
      !I_AUFNR type /AGRI/FMFPNUM optional
    exporting
      !ET_BTCHCNFM type ZABS_TTY_NUR_BATCH_CONFM_F4
      !ET_FMFPCOM type /AGRI/T_FMFPCOM
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods GET_CHAR_VALUE_F4
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
    exporting
      !ET_CHARVALUE type ZABS_TTY_NUR_CHARSTICS_F4 .
  methods GET_PROCESSORD
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
    exporting
      !ET_PROCESSORD type ZABS_TTY_NUR_ROUTE_TERRAIN .
  methods GET_BATCHCNFM_DATA
    importing
      !IT_OCNUM type ZABS_TTY_NUR_AGRI_FMOCINDX
      !I_LOWDATE type SY-DATUM
    exporting
      !ET_BTCHCNFM_DATA type /AGRI/T_FMOC_DETAILS .
  methods CREATE_BATCH
    importing
      !IS_BATCH_CRT type ZABS_S_NUR_BATCH_CREATE
    exporting
      !ES_BATCH_CRT type ZABS_S_NUR_BATCH_CREATE
      !ET_MESSAGES type /AGRI/T_GPROLOG
      !EV_SUBRC type SUBRC .
  methods UPDATE_CHARACTERISTICS
    importing
      !IS_BATCH_CRT type ZABS_S_NUR_BATCH_CREATE
      !IT_CHAR_UPD type ZABS_TTY_NUR_BATCH_CREATE
    exporting
      !ET_MESSAGES type /AGRI/T_GPROLOG .
  methods BATCH_CONFIRMATIONS
    importing
      !IS_BATCH_CRT type ZABS_S_NUR_BATCH_CREATE
      !IT_BATCH_CNF type ZABS_TTY_NUR_BATCH_CREATE
    exporting
      !ET_MESSAGES type /AGRI/T_GPROLOG .
ENDCLASS.



CLASS ZCL_ZABS_MOB_NURSERY_DPC_EXT IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~changeset_begin.

    CLEAR: cv_defer_mode.
    cv_defer_mode = abap_true.

*-- Looping Entityset name in the Operation Table
    LOOP AT it_operation_info ASSIGNING FIELD-SYMBOL(<fs_operation_info>).
      IF NOT ( <fs_operation_info>-entity_set EQ 'NurBatchCreateSet' ).
        cv_defer_mode = abap_false.
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~changeset_end.

    COMMIT WORK.

  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~changeset_process.

*-- Local Declarations
    DATA:
      lo_update_context      TYPE REF TO /iwbep/if_mgw_req_entity_u,
      lv_entity_type         TYPE string,
      ls_batch_crt           TYPE zcl_zabs_mob_nursery_mpc=>ts_nurbatchcreate,
      lt_batch_crt           TYPE zcl_zabs_mob_nursery_mpc=>tt_nurbatchcreate,
      ls_batch_crt_1         TYPE zcl_zabs_mob_nursery_mpc=>ts_nurbatchcreate,
      lt_char_upd            TYPE zcl_zabs_mob_nursery_mpc=>tt_nurbatchcreate,
      lt_batch_cnf           TYPE zcl_zabs_mob_nursery_mpc=>tt_nurbatchcreate,
      ls_message             TYPE /agri/s_gprolog,
      lt_messages            TYPE /agri/t_gprolog,
      lt_bapi_messages       TYPE bapiret2_tt,
      lv_operation_no        TYPE i,
      lwa_changeset_response TYPE /iwbep/if_mgw_appl_types=>ty_s_changeset_response.

    LOOP AT it_changeset_request ASSIGNING FIELD-SYMBOL(<fs_changeset_request>).
      CLEAR lv_entity_type.
      lo_update_context ?= <fs_changeset_request>-request_context.
      lv_entity_type = lo_update_context->get_entity_type_name( ).

      IF lv_entity_type NE 'NurBatchCreate'.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
          EXPORTING
            textid      = /iwbep/cx_mgw_tech_exception=>operation_not_supported
            operation   = 'UPDATE_ENTITY'
            entity_type = lv_entity_type.
      ENDIF.

      CASE lv_entity_type.

        WHEN 'NurBatchCreate'.
          <fs_changeset_request>-entry_provider->read_entry_data( "#EC CI_FLDEXT_OK
                             IMPORTING es_data = ls_batch_crt ).

          IF ls_batch_crt-matnr IS NOT INITIAL.
            APPEND ls_batch_crt TO lt_batch_crt.
          ENDIF.
          IF ls_batch_crt-atnam IS NOT INITIAL.
            APPEND ls_batch_crt TO lt_char_upd.
          ENDIF.
          IF ls_batch_crt-matnr_bc IS NOT INITIAL.
            APPEND ls_batch_crt TO lt_batch_cnf.
          ENDIF.

          lwa_changeset_response-operation_no = <fs_changeset_request>-operation_no.
          copy_data_to_ref( EXPORTING is_data = ls_batch_crt
               CHANGING cr_data = lwa_changeset_response-entity_data ).
          INSERT lwa_changeset_response INTO TABLE ct_changeset_response.

      ENDCASE.
    ENDLOOP.

    REFRESH lt_messages[].

    READ TABLE lt_batch_crt INTO ls_batch_crt_1 INDEX 1.
    IF ls_batch_crt_1-matnr IS NOT INITIAL.
      "     Batch Create
      CALL METHOD me->create_batch
        EXPORTING
          is_batch_crt = ls_batch_crt_1
        IMPORTING
          es_batch_crt = DATA(os_batch_crt)
          et_messages  = DATA(ot_btchcrt_msg)
          ev_subrc     = DATA(lv_subrc).

      IF ot_btchcrt_msg[] IS NOT INITIAL.
        APPEND LINES OF ot_btchcrt_msg TO lt_messages.
      ENDIF.

    ENDIF.

    IF lt_char_upd[] IS NOT INITIAL AND os_batch_crt IS NOT INITIAL AND
        lv_subrc IS INITIAL.
      "     Characteristics update
      CALL METHOD me->update_characteristics
        EXPORTING
          is_batch_crt = os_batch_crt
          it_char_upd  = lt_char_upd
        IMPORTING
          et_messages  = DATA(ot_charupd_msg).

      IF ot_charupd_msg[] IS NOT INITIAL.
        APPEND LINES OF ot_charupd_msg TO lt_messages.
      ENDIF.

    ENDIF.

*    IF lt_batch_cnf[] IS NOT INITIAL AND
      if os_batch_crt IS NOT INITIAL AND
        lv_subrc IS INITIAL.
      "     Batch Confirmations
      CALL METHOD me->batch_confirmations
        EXPORTING
          is_batch_crt = os_batch_crt
          it_batch_cnf = lt_batch_cnf
        IMPORTING
          et_messages  = DATA(ot_batchcnf_msg).

      IF ot_batchcnf_msg[] IS NOT INITIAL.
        APPEND LINES OF ot_batchcnf_msg TO lt_messages.
      ENDIF.

    ENDIF.

    add_messages_to_msg_container( iv_exception = abap_true
                                   it_messages = lt_messages ).


  ENDMETHOD.


  METHOD add_messages_to_msg_container.

*-- Local Declarations
    DATA: ls_bapi_messages TYPE bapiret2,
          lo_msg_container TYPE REF TO /iwbep/if_message_container,
          ls_message       TYPE /agri/s_gprolog,
          lv_msg_error     TYPE xfld,
          lv_msgno         TYPE symsgno.

    CONSTANTS: c_true TYPE xfld VALUE 'X'.

*** Get Message container object
    lo_msg_container = me->mo_context->get_message_container( ).

*-- From Agri Messages
    IF is_message IS NOT INITIAL.
      CLEAR lv_msgno.
      lv_msgno = is_message-msgno.
      lo_msg_container->add_message(
         EXPORTING iv_msg_type               = is_message-msgty
                   iv_msg_id                 = is_message-msgid
                   iv_msg_number             = lv_msgno
                   iv_msg_v1                 = is_message-msgv1
                   iv_msg_v2                 = is_message-msgv2
                   iv_msg_v3                 = is_message-msgv3
                   iv_msg_v4                 = is_message-msgv4
                   iv_msg_text               = iv_message_text
                   iv_entity_type            = iv_entity_name
                   iv_add_to_response_header = abap_true ).
      IF is_message-msgty = zcl_abs_abap_maintain=>c_msgty_error.
        lv_msg_error = c_true.
      ENDIF.
    ENDIF.

    IF it_messages IS NOT INITIAL.
      CLEAR lv_msg_error.
      LOOP AT it_messages INTO ls_message.

        IF ls_message-msgid IS INITIAL AND
           ls_message-msgno IS INITIAL AND
           ls_message-msgty IS INITIAL.
          CONTINUE.
        ENDIF.

        CLEAR lv_msgno.
        IF ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error.
          lv_msg_error = c_true.
        ENDIF.
        lv_msgno = ls_message-msgno.
        lo_msg_container->add_message(
           EXPORTING iv_msg_type               = ls_message-msgty
                     iv_msg_id                 = ls_message-msgid
                     iv_msg_number             = lv_msgno
                     iv_msg_v1                 = ls_message-msgv1
                     iv_msg_v2                 = ls_message-msgv2
                     iv_msg_v3                 = ls_message-msgv3
                     iv_msg_v4                 = ls_message-msgv4
                     iv_entity_type            = iv_entity_name
                     iv_add_to_response_header = abap_true ).
      ENDLOOP.
    ENDIF.

    IF iv_exception EQ abap_true AND lv_msg_error IS NOT INITIAL.
*    " Raise Exception
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          textid            = /iwbep/cx_mgw_busi_exception=>business_error
          entity_type       = iv_entity_name
          message_container = lo_msg_container.
    ENDIF.

*-- From Bapi Messages
    IF it_bapi_messages IS NOT INITIAL.
      READ TABLE it_bapi_messages INTO ls_bapi_messages
           WITH KEY type = zcl_abs_abap_maintain=>c_msgty_error.
      IF sy-subrc = 0.
        lv_msg_error = c_true.
      ENDIF.

      lo_msg_container->add_messages_from_bapi(
            it_bapi_messages         = it_bapi_messages
            iv_determine_leading_msg =
     /iwbep/if_message_container=>gcs_leading_msg_search_option-first
            iv_entity_type           = iv_entity_name
            iv_add_to_response_header = abap_true ).

      IF iv_exception EQ abap_true AND lv_msg_error IS NOT INITIAL.
        " Raise Exception
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            textid            =
                                /iwbep/cx_mgw_busi_exception=>business_error
*           message           = lv_text
            message_container = lo_msg_container.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD batch_confirmations.

*--Adding user function to add custom button
    DATA: user_function1(40),
          user_function2(40),
          cust_scr           TYPE sydynnr, " VALUE c_screen-batches,
          ordlv              TYPE /agri/glordlv.

    DATA: gt_csdoc           TYPE /agri/t_glcs_doc,
*      gs_fpoc_doc        TYPE /agri/s_fmoc_doc,
          gs_fpdoc_infocus   TYPE /agri/s_fmfp_doc,
          gs_todoc_infocus   TYPE /agri/s_fmfp_doc,
*      gs_nsdoc_infocus   TYPE /agri/s_fmfp_doc,
          gt_fpitm_fcat      TYPE /agri/t_fmfpitm_fcat,
*      gt_fpitm_conf      TYPE /agri/t_fmfpitm_fcat,
          gt_components      TYPE /agri/t_fmfpcom_fcat,
          gt_batches_fcat    TYPE /agri/t_fmfpbch_fcat,
          gt_activities_fcat TYPE /agri/t_fmfpact_fcat,
          gt_fpcord_fcat     TYPE /agri/t_fmfpcord_fcat,
          gt_fpcord          TYPE /agri/t_fmfpcord,
          gt_update_aufnr    TYPE /agri/t_fmaufnr,
          gt_next_orders     TYPE /agri/t_fmfpcnprs_ord,
*      gt_fpcnf_fcat      TYPE /agri/t_fmfpcnf_fcat,
          gt_fmfpcnf         TYPE /agri/t_fmfp_cnf,
          gt_fpcnf_fcat      TYPE /agri/t_fmfp_cnf_fcat,
          gt_dropdown        TYPE lvc_t_drop.

    FIELD-SYMBOLS: <gs_fpdoc_infocus> TYPE /agri/s_fmfp_doc.

    DATA: gt_search_header      TYPE TABLE OF /agri/s_fmfphdr,
          gt_search_next_header TYPE TABLE OF /agri/s_fmfphdr,
          gt_worklist_header    TYPE TABLE OF /agri/s_fmfphdr_wl.

    DATA: gt_selected_docs     TYPE /agri/t_fmaufnr,
          gt_selected_rows     TYPE lvc_t_row,
          gt_items_mod_rows    TYPE lvc_t_modi,
          gt_comp_mod_rows     TYPE lvc_t_modi,
          gt_acti_mod_rows     TYPE lvc_t_modi,
          gt_next_mod_rows     TYPE lvc_t_modi,
          gt_task_itm_mod_rows TYPE lvc_t_modi,
          gt_batches_mod_rows  TYPE lvc_t_modi.

    DATA: gs_tabstrip_captions TYPE /agri/s_gtabstrip_captions,
          gt_tabstrip_fcodes   TYPE /agri/t_gtabstrip.

****Tabstrip settings
    DATA: gt_tabstrip_defaults TYPE /agri/t_gtabstrip,
          gt_tabstrip_texts    TYPE TABLE OF dd07v.

    DATA: gs_fphdr_portal                TYPE /agri/s_fmfphdr.

    DATA: ref_badi_fmns_all      TYPE REF TO /agri/badi_fmns_all.

    DATA : ref_worklist_container      TYPE REF TO cl_gui_docking_container,
           ref_worklist                TYPE REF TO /agri/cl_worklist_container,
*           ref_log_handler             TYPE REF TO lcl_log_handler,
*           ref_event_handler           TYPE REF TO lcl_event_handler,
           ref_grid_items              TYPE REF TO /agri/cl_gui_alv_grid,
           ref_dock_container_task_itm TYPE REF TO cl_gui_docking_container,
           ref_dock_container_comp     TYPE REF TO cl_gui_docking_container,
           ref_dock_container_actv     TYPE REF TO cl_gui_docking_container,
           ref_items_container         TYPE REF TO cl_gui_custom_container,
           ref_batches_container       TYPE REF TO cl_gui_custom_container,
           ref_confirmations_container TYPE REF TO cl_gui_custom_container,
           ref_grid_components         TYPE REF TO /agri/cl_gui_alv_grid,
           ref_grid_activities         TYPE REF TO /agri/cl_gui_alv_grid,
           ref_grid_create_orders      TYPE REF TO /agri/cl_gui_alv_grid,
           ref_grid_next_orders        TYPE REF TO /agri/cl_gui_alv_grid,
           ref_grid_confirmations      TYPE REF TO /agri/cl_gui_alv_grid,
           ref_grid_batches            TYPE REF TO /agri/cl_gui_alv_grid,
           ref_container_next_orders   TYPE REF TO cl_gui_custom_container,
           ref_container_create_orders TYPE REF TO cl_gui_custom_container.

*
    DATA: "gt_prodord_mod_rows_x TYPE lvc_t_modi,
          "gt_batches_mod_rows_x TYPE lvc_t_modi,
          gs_batch_fcat         LIKE LINE OF gt_batches_fcat.

    DATA: lwa_row        TYPE lvc_s_row,
          lt_rows        TYPE lvc_t_row,
          lv_subrc       TYPE sy-subrc,
          lt_aufnr       TYPE /agri/t_fmaufnr,
          lt_fpdoc       TYPE /agri/t_fmfp_doc,
          lt_fpdoc_1     TYPE /agri/t_fmfp_doc,
          lwa_fpdoc      TYPE /agri/s_fmfp_doc,
*        lwa_fpitm TYPE /agri/s_fmfpitm,
          lwa_fmfpcnf    TYPE /agri/s_fmfp_cnf,
          lwa_fpcnf_fcat TYPE /agri/s_fmfp_cnf_fcat,
          lwa_fpbch      TYPE /agri/s_fmfpbch,
          lwa_fpbch_tmp  TYPE /agri/s_fmfpbch,
          lwa_linkbd     TYPE /agri/s_fmlink_batch_conf.

    DATA: lt_ticket  TYPE TABLE OF bapi_pp_timeticket,
          ls_ticket  TYPE bapi_pp_timeticket,
          lt_items   TYPE TABLE OF bapi2017_gm_item_create,
          lt_linkgm  TYPE TABLE OF bapi_link_conf_goodsmov,
          lt_linkbd  TYPE TABLE OF /agri/s_fmlink_batch_conf,
*          gt_fmfpcnf    TYPE /agri/t_fmfp_cnf,
*          gt_components TYPE /agri/t_fmfpcom_fcat,
          lt_fmfpcnf TYPE /agri/t_fmfp_cnf,
          lt_ocdoc   TYPE /agri/t_fmoc_doc,
          ls_message TYPE /agri/s_gprolog.

    CONSTANTS: c_true(1)       TYPE c VALUE 'X',
               c_false(1)      TYPE c VALUE ' ',
               c_zero(1)       TYPE c VALUE '0',
               c_star(1)       TYPE c VALUE '*',
               c_dollar(1)     TYPE c VALUE '$',
               c_colon(1)      TYPE c VALUE ':',
               c_slash(1)      TYPE c VALUE '/',
               c_underscore(1) TYPE c VALUE '_',
               c_ascending(1)  TYPE c VALUE 'A',
               c_descending(1) TYPE c VALUE 'D'.

    FIELD-SYMBOLS: <lwa_fpitm_fcat> TYPE /agri/s_fmfpitm_fcat,
                   <lwa_fpitm>      TYPE /agri/s_fmfpitm.

    CLEAR lwa_fpdoc.
    REFRESH lt_aufnr[].
    CHECK is_batch_crt-aufnr IS NOT INITIAL.
    APPEND is_batch_crt-aufnr TO lt_aufnr.

    REFRESH lt_fpdoc_1[].
    CALL FUNCTION '/AGRI/FMFP_VIEW'
      EXPORTING
        it_aufnr       = lt_aufnr
      IMPORTING
        et_fpdoc       = lt_fpdoc_1 " ls_fpdoc
      EXCEPTIONS
        no_data_exists = 1
        OTHERS         = 2.
    IF sy-subrc = 0.

      READ TABLE lt_fpdoc_1 INTO lwa_fpdoc INDEX 1.
    ENDIF.
    MOVE-CORRESPONDING lwa_fpdoc-x-fpitm TO gt_fmfpcnf.
    READ TABLE lwa_fpdoc-x-fpitm INTO DATA(lwa_fpitm_dt) INDEX 1.
    LOOP AT gt_fmfpcnf INTO lwa_fmfpcnf.
      lwa_fmfpcnf-lmnga = is_batch_crt-erfmg.
      lwa_fmfpcnf-budat = is_batch_crt-date.
      lwa_fmfpcnf-isdd  = lwa_fpitm_dt-actdt.
      lwa_fmfpcnf-iedd  = lwa_fpitm_dt-actdt.
      lwa_fmfpcnf-gicre = abap_true.
      MODIFY gt_fmfpcnf FROM lwa_fmfpcnf
                        TRANSPORTING lmnga budat gicre.
    ENDLOOP.
    lt_fmfpcnf = gt_fmfpcnf.
    SORT lt_fmfpcnf BY action ASCENDING.
    DELETE lt_fmfpcnf WHERE action IS NOT INITIAL.

    CALL METHOD me->get_btchcnfm_f4
      EXPORTING
        i_aufnr    = is_batch_crt-aufnr
      IMPORTING
        et_fmfpcom = DATA(it_com).

    IF it_com IS NOT INITIAL.
      MOVE-CORRESPONDING it_com TO gt_components.
    ENDIF.

    LOOP AT gt_components INTO DATA(lwa_components).
      READ TABLE it_batch_cnf INTO DATA(lwa_batch_cnf)
                              WITH KEY matnr_bc = lwa_components-matnr
                                       charg_bc = lwa_components-charg.
      IF sy-subrc = 0.
        lwa_components-vornr = '0010'.
        lwa_components-lmnga = lwa_batch_cnf-lmnga.
        lwa_components-flgch = abap_true.
        MODIFY gt_components FROM lwa_components TRANSPORTING vornr lmnga flgch.
      ELSE.
        lwa_components-vornr = '0010'.
        lwa_components-flgch = abap_true.
        MODIFY gt_components FROM lwa_components TRANSPORTING vornr flgch.
      ENDIF.

    ENDLOOP.


    DATA(lv_custom) = abap_false.
    lv_custom = abap_true.

    IF lv_custom EQ abap_true.

      DATA: lwa_tcoko       TYPE tcoko,
            lv_index        TYPE i,
            lv_index_msg    TYPE i,
            lv_activity_no  TYPE i,
            lv_tot_yield    TYPE gamng,
            lwa_item        TYPE bapi2017_gm_item_create,
            lwa_fpitm       TYPE /agri/s_fmfpitm,
*            lwa_fmfpcnf     TYPE /agri/s_fmfp_cnf,
            lwa_fpcom       TYPE /agri/s_fmfpcom,
            lwa_yfpcom      TYPE /agri/s_fmfpcom,
            lwa_activities  TYPE /agri/s_fmfpact,
            lwa_linkgm      TYPE bapi_link_conf_goodsmov,
            lwa_mvt_code    TYPE bapi2017_gm_code,
            lwa_grheader_gi TYPE bapi2017_gm_head_01,
            lwa_grheader_gr TYPE bapi2017_gm_head_01,
            lwa_return      TYPE bapiret2,
            lt_return       TYPE TABLE OF bapiret2,
            lt_items_gi     TYPE TABLE OF bapi2017_gm_item_create,
            lt_items_gr     TYPE TABLE OF bapi2017_gm_item_create.


      IF lwa_tcoko IS INITIAL.
        SELECT SINGLE * FROM tcoko INTO lwa_tcoko
                       WHERE arbge EQ 'CO'.

      ENDIF.

      CLEAR: lv_subrc.

      DESCRIBE TABLE lt_ticket LINES lwa_linkgm-index_confirm.
      DESCRIBE TABLE lt_items  LINES lwa_linkgm-index_goodsmov.
      lv_index = lwa_linkgm-index_goodsmov.

      LOOP AT lt_fmfpcnf INTO lwa_fmfpcnf.

        CLEAR: ls_ticket.
        lwa_linkgm-index_confirm  = lwa_linkgm-index_confirm + 1.
        lv_activity_no            = 1.
        ls_ticket-orderid         = lwa_fmfpcnf-aufnr.
        ls_ticket-operation       = lwa_fmfpcnf-vornr.
        ls_ticket-fin_conf        = '1'.
        ls_ticket-postg_date      = lwa_fmfpcnf-budat.
        ls_ticket-yield           = lwa_fmfpcnf-lmnga.
        ls_ticket-conf_quan_unit  = lwa_fmfpcnf-meinh.
        ls_ticket-work_cntr       = lwa_fmfpcnf-arbpl_ext.
        ls_ticket-no_of_employee  = lwa_fmfpcnf-anzms.
        ls_ticket-pers_no         = lwa_fmfpcnf-pernr.
        ls_ticket-exec_start_date = lwa_fmfpcnf-isdd.
        ls_ticket-exec_start_time = lwa_fmfpcnf-isdz.
        ls_ticket-exec_fin_date   = lwa_fmfpcnf-iedd.
        ls_ticket-exec_fin_time   = lwa_fmfpcnf-iedz.
        ls_ticket-conf_activity1  = lwa_fmfpcnf-ism01.
        ls_ticket-conf_acti_unit1 = lwa_fmfpcnf-leinh1.
        ls_ticket-conf_activity2  = lwa_fmfpcnf-ism02.
        ls_ticket-conf_acti_unit2 = lwa_fmfpcnf-leinh2.
        ls_ticket-conf_activity3  = lwa_fmfpcnf-ism03.
        ls_ticket-conf_acti_unit3 = lwa_fmfpcnf-leinh3.
        ls_ticket-conf_activity4  = lwa_fmfpcnf-ism04.
        ls_ticket-conf_acti_unit4 = lwa_fmfpcnf-leinh4.
        ls_ticket-conf_activity5  = lwa_fmfpcnf-ism05.
        ls_ticket-conf_acti_unit5 = lwa_fmfpcnf-leinh5.
        ls_ticket-conf_activity6  = lwa_fmfpcnf-ism06.
        ls_ticket-conf_acti_unit6 = lwa_fmfpcnf-leinh6.
        APPEND ls_ticket TO lt_ticket.

        IF lwa_fmfpcnf-gicre IS NOT INITIAL.

          LOOP AT gt_components INTO DATA(lwa_component)
        WHERE aufnr = lwa_fmfpcnf-aufnr
          AND posnr = lwa_fmfpcnf-posnr
          AND matnr IS NOT INITIAL
          AND xwaok IS NOT INITIAL.
            lwa_grheader_gi-pstng_date = lwa_fmfpcnf-budat.
            lwa_item-prod_date         = sy-datum.
            lwa_item-material_long     = lwa_component-matnr.
            lwa_item-batch             = lwa_component-charg.
            lwa_item-move_type         = lwa_tcoko-wa_bwart.
            IF lwa_component-bwart IS NOT INITIAL.
              lwa_item-move_type = lwa_component-bwart.
            ENDIF.
            lwa_item-stge_loc          = lwa_component-lgort.
            lwa_item-entry_qnt         = lwa_component-lmnga.
            lwa_item-entry_uom         = lwa_component-erfme.
            lwa_item-withdrawn         = c_true.
            lwa_item-activity          = lwa_fmfpcnf-vornr.
            lwa_item-orderid           = lwa_fmfpcnf-aufnr.
            lwa_item-plant             = lwa_component-werks.

            IF lwa_component-bwart IS NOT INITIAL.
              lwa_item-move_type = lwa_component-bwart.
            ENDIF.

            READ TABLE lwa_fpdoc-y-fpcom INTO lwa_yfpcom
              WITH KEY aufnr = lwa_component-aufnr
                       posnr = lwa_component-posnr
                       contr = lwa_component-contr BINARY SEARCH.
            IF sy-subrc EQ 0.
              lwa_item-reserv_no = lwa_fpdoc-x-fphdr-rsnum.
              lwa_item-res_item  = lwa_component-rspos.
            ENDIF.
            APPEND lwa_item TO lt_items_gi.
            CLEAR: lwa_item.
            lwa_linkgm-index_goodsmov = lwa_linkgm-index_goodsmov + 1.
            APPEND lwa_linkgm TO lt_linkgm.
          ENDLOOP.
        ENDIF.

        IF lwa_fmfpcnf-grcre IS NOT INITIAL AND
       lwa_fmfpcnf-lmnga IS NOT INITIAL.
          lwa_grheader_gr-pstng_date = lwa_fmfpcnf-budat.
          lwa_item-prod_date = sy-datum.
          lwa_item-material_long  = lwa_fpdoc-x-fphdr-matnr.


          lwa_item-move_type = lwa_tcoko-we_bwart.
*          lwa_item-entry_uom = lwa_fmfpcnf-meinh.
          lwa_item-entry_uom = is_batch_crt-msehi.
          lwa_item-mvt_ind   = 'F'.
          lwa_item-orderid   = lwa_fpdoc-x-fphdr-aufnr.
          lwa_item-plant     = lwa_fpdoc-x-fphdr-iwerk.
          IF lwa_fmfpcnf-umren IS NOT INITIAL.
            lwa_item-entry_qnt = lwa_fmfpcnf-lmnga / lwa_fmfpcnf-umren.
          ENDIF.
          IF lwa_fpdoc-x-fphdr-autyp EQ 'AO'. " c_document_category-production_order.
            lwa_item-batch = is_batch_crt-charg.
          ENDIF.

          READ TABLE gt_batches_fcat INTO DATA(ls_batch_fcat)
            WITH KEY aufnr = lwa_item-orderid
                     matnr = lwa_item-material_long
                     charg = lwa_item-batch.
          IF sy-subrc EQ 0.
            lwa_item-entry_uom = ls_batch_fcat-erfme.
          ENDIF.

          APPEND lwa_item TO lt_items_gr.
          CLEAR lwa_item.
          lwa_linkgm-index_goodsmov = lwa_linkgm-index_goodsmov + 1.
          APPEND lwa_linkgm TO lt_linkgm.
        ENDIF.

      ENDLOOP.

      IF lt_items_gi[] IS NOT INITIAL.
        lwa_mvt_code-gm_code = '03'.


        DATA: lv_test_run  TYPE bapi2017_gm_gen-testrun,
*              lwa_return   TYPE bapiret2,
              lwa_grheader TYPE bapi2017_gm_head_ret.

        CHECK lt_items_gi[] IS NOT INITIAL.
        CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
          EXPORTING
            goodsmvt_header  = lwa_grheader_gi
            goodsmvt_code    = lwa_mvt_code
            testrun          = c_true " lv_test_run
*           GOODSMVT_REF_EWM =
          IMPORTING
            goodsmvt_headret = lwa_grheader
*           MATERIALDOCUMENT =
*           MATDOCUMENTYEAR  =
          TABLES
            goodsmvt_item    = lt_items_gi[]
*           GOODSMVT_SERIALNUMBER   =
            return           = lt_return
*           GOODSMVT_SERV_PART_DATA =
*           EXTENSIONIN      =
          .

        LOOP AT lt_return INTO lwa_return
                         WHERE type EQ 'E' " c_msg_type-error
                            OR type EQ 'A'. " c_msg_type-abend.

          CLEAR ls_message.
          ls_message-msgty = lwa_return-type.
          ls_message-msgid = lwa_return-id.
          ls_message-msgno = lwa_return-number.

          APPEND ls_message TO et_messages.
          CLEAR ls_message.
          EXIT.
        ENDLOOP.

        IF sy-subrc EQ 0.
          lv_subrc = 4.
*          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*            EXPORTING
*              wait = c_true.
        ENDIF.

        IF lv_subrc NE 0.
          LOOP AT lt_fmfpcnf INTO lwa_fmfpcnf.

            IF lwa_fmfpcnf-gicre IS NOT INITIAL.
              LOOP AT lwa_fpdoc-x-fpcom INTO lwa_fpcom
                                       WHERE aufnr = lwa_fmfpcnf-aufnr
                                         AND posnr = lwa_fmfpcnf-posnr
                                         AND matnr IS NOT INITIAL
                                         AND xwaok IS NOT INITIAL.
                lv_index_msg = lv_index_msg + 1.
                LOOP AT lt_return INTO lwa_return
                                WHERE row EQ lv_index_msg.
                  CLEAR ls_message.
                  ls_message-msgty = lwa_return-type.
                  ls_message-msgid = lwa_return-id.
                  ls_message-msgno = lwa_return-number.

                  APPEND ls_message TO et_messages.
                  CLEAR ls_message.

                ENDLOOP.
              ENDLOOP.
            ENDIF.

          ENDLOOP.
        ENDIF.
      ENDIF.


      IF lt_items_gr[] IS NOT INITIAL.
        lwa_mvt_code-gm_code = '02'.


        CHECK lt_items_gr[] IS NOT INITIAL.
        CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
          EXPORTING
            goodsmvt_header  = lwa_grheader_gr
            goodsmvt_code    = lwa_mvt_code
            testrun          = c_true " lv_test_run
*           GOODSMVT_REF_EWM =
          IMPORTING
            goodsmvt_headret = lwa_grheader
*           MATERIALDOCUMENT =
*           MATDOCUMENTYEAR  =
          TABLES
            goodsmvt_item    = lt_items_gr[]
*           GOODSMVT_SERIALNUMBER   =
            return           = lt_return
*           GOODSMVT_SERV_PART_DATA =
*           EXTENSIONIN      =
          .

        LOOP AT lt_return INTO lwa_return
                         WHERE type EQ 'E'
                            OR type EQ 'A'.

          CLEAR ls_message.
          ls_message-msgty = lwa_return-type.
          ls_message-msgid = lwa_return-id.
          ls_message-msgno = lwa_return-number.

          APPEND ls_message TO et_messages.
          CLEAR ls_message.
          EXIT.
        ENDLOOP.
        IF sy-subrc EQ 0.
          lv_subrc = 4.
          EXIT.
*        ELSEIF lv_commit_work IS NOT INITIAL.
*          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*            EXPORTING
*              wait = c_true.
        ENDIF.

        IF lwa_fmfpcnf-grcre IS NOT INITIAL AND
               lwa_fmfpcnf-lmnga IS NOT INITIAL.
          lv_index_msg = lv_index_msg + 1.
          LOOP AT lt_return INTO lwa_return
                            WHERE row EQ lv_index_msg.

            CLEAR ls_message.
            ls_message-msgty = lwa_return-type.
            ls_message-msgid = lwa_return-id.
            ls_message-msgno = lwa_return-number.

            APPEND ls_message TO et_messages.
            CLEAR ls_message.


          ENDLOOP.
        ENDIF.
      ENDIF.

      APPEND LINES OF lt_items_gi[] TO lt_items[].
      APPEND LINES OF lt_items_gr[] TO lt_items[].

    ENDIF.


    lwa_fpdoc-x-fphdr-charg = is_batch_crt-charg.
*APPEND lwa_fpdoc-x-fphdr TO lt_fpdoc.
    APPEND lwa_fpdoc TO lt_fpdoc.
    lwa_linkbd-charg = is_batch_crt-charg.
    lwa_linkbd-contr = is_batch_crt-contr.
    lwa_linkbd-index_document = 1.
    APPEND lwa_linkbd TO lt_linkbd.

    DATA: lv_row       TYPE bapi_line,
          "lwa_fpdoc    TYPE /agri/s_fmfp_doc,
          lv_ccontr    TYPE /agri/fmccontr,
          lv_gcontr    TYPE /agri/gcontr,
          lv_quantity  TYPE /agri/fmlmnga,
          lwa_status   TYPE bapiret1,
          "lwa_fpbch    TYPE /agri/s_fmfpbch,
          "lwa_fpcom    TYPE /agri/s_fmfpcom,
          lwa_return_d TYPE bapi_coru_return,
          lwa_ocopr    TYPE /agri/s_fmocopr,
          lwa_occom    TYPE /agri/s_fmoccom,
          lwa_ocindx   TYPE /agri/s_fmocindx,
          "lwa_linkbd   TYPE /agri/s_fmlink_batch_conf,
          "lwa_fmfpcnf  TYPE /agri/s_fmfp_cnf,
          lwa_fpoc_doc TYPE /agri/s_fmoc_doc,
          lwa_bapiret  TYPE bapiret2,
          lt_return_d  TYPE TABLE OF bapi_coru_return,
          lt_fpbch_new TYPE /agri/t_fmfpbch.

    FIELD-SYMBOLS: <lwa_fpdoc> TYPE /agri/s_fmfp_doc,
                   "<lwa_fpitm> TYPE /agri/s_fmfpitm,
                   <lwa_fpbch> TYPE /agri/s_fmfpbch.

    IF lt_ticket[] IS INITIAL.
      lv_subrc = 4.
      EXIT.
    ENDIF.

    READ TABLE lt_items INTO DATA(lwa_itemsgm)
    WITH KEY move_type = zcl_abs_abap_maintain=>c_move_type_grn_create.
    IF sy-subrc EQ 0
    AND lwa_itemsgm-batch IS NOT INITIAL.
      LOOP AT lt_items ASSIGNING FIELD-SYMBOL(<lwa_itemsgm>).
        <lwa_itemsgm>-gr_rcpt = lwa_itemsgm-batch.
      ENDLOOP.
    ENDIF.

    SORT: lt_fpdoc BY aufnr.
    CALL FUNCTION 'BAPI_PRODORDCONF_CREATE_TT'
      IMPORTING
        return             = lwa_status
      TABLES
        timetickets        = lt_ticket[]
        goodsmovements     = lt_items
        link_conf_goodsmov = lt_linkgm
        detail_return      = lt_return_d.

    SORT lt_return_d BY type row id number.
    READ TABLE lt_return_d TRANSPORTING NO FIELDS
    WITH KEY type = 'E' BINARY SEARCH. " c_msg_type-error BINARY SEARCH.
    IF sy-subrc EQ 0.
*      LOOP AT lt_fmfpcnf INTO lwa_fmfpcnf.
      READ TABLE lt_fmfpcnf INTO lwa_fmfpcnf INDEX 1.
      READ TABLE lt_fpdoc INTO lwa_fpdoc
        WITH KEY aufnr = lwa_fmfpcnf-aufnr BINARY SEARCH.

*      gs_variables-item_infocus = lwa_fmfpcnf-posnr.

      lv_row = lv_row + 1.
      LOOP AT lt_return_d INTO lwa_return_d
                        WHERE row = lv_row
                          AND type EQ 'E'. " c_msg_type-error.
        CLEAR ls_message.
        ls_message-msgty = lwa_return_d-type.
        ls_message-msgid = lwa_return_d-id.
        ls_message-msgno = lwa_return_d-number.
        ls_message-msgv1 = lwa_return_d-message_v1.
        ls_message-msgv2 = lwa_return_d-message_v2.

        APPEND ls_message TO et_messages.
        CLEAR ls_message.
        EXIT.
      ENDLOOP.
      CLEAR: lwa_fpdoc.

*      ENDLOOP.
      lv_subrc = 4.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      EXIT.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait   = 'X'
        IMPORTING
          return = lwa_bapiret.
      WAIT UP TO 2 SECONDS.
      READ TABLE lt_items INTO DATA(lwa_item_x) INDEX 1.
      IF sy-subrc EQ 0.
        SELECT * UP TO 1 ROWS
          FROM matdoc
          INTO @DATA(lwa_matdoc)
         WHERE werks = @lwa_item_x-plant
           AND aufnr = @lwa_item_x-orderid
           AND bwart = @lwa_item_x-move_type
           AND matnr = @lwa_item_x-material
           AND charg = @lwa_item_x-batch.
        ENDSELECT.
        IF sy-subrc NE 0.
*-- Erro ao processar movimento &1.

          CLEAR ls_message.
          ls_message-msgty = 'I'.
          ls_message-msgid = 'ZFMFP'.
          ls_message-msgno = '089'.

          APPEND ls_message TO et_messages.
          CLEAR ls_message.

        ENDIF.
      ENDIF.
    ENDIF.

    LOOP AT lwa_fpdoc-x-fpbch INTO lwa_fpbch.
      IF lv_gcontr LT lwa_fpbch-contr.
        lv_gcontr = lwa_fpbch-contr.
      ENDIF.
    ENDLOOP.
    CLEAR: lwa_fpbch.

    LOOP AT lt_fpdoc INTO lwa_fpdoc.

*--Get batch and counter for current document
      READ TABLE lt_linkbd INTO lwa_linkbd
                           WITH KEY index_document = sy-tabix.
      CHECK sy-subrc EQ 0.
      READ TABLE lwa_fpdoc-x-fpbch ASSIGNING <lwa_fpbch>
        WITH KEY contr = lwa_linkbd-contr
                 charg = lwa_linkbd-charg BINARY SEARCH.
      CHECK sy-subrc EQ 0.

      MOVE-CORRESPONDING lwa_fpdoc-x-fphdr TO lwa_fpoc_doc-x-ochdr.
      CLEAR: lwa_fpoc_doc-x-ochdr-updkz,
             lwa_fpoc_doc-x-ochdr-matnr,
             lwa_fpoc_doc-x-ochdr-charg,
             lv_ccontr.
      lwa_fpoc_doc-x-ochdr-charg_ref = lwa_linkbd-charg.
      lwa_fpoc_doc-x-ochdr-contr_ref = lwa_linkbd-contr.

      LOOP AT lt_fmfpcnf INTO lwa_fmfpcnf
                        WHERE aufnr EQ lwa_fpdoc-aufnr.

        lv_row = lv_row + 1.
        READ TABLE lwa_fpdoc-x-fpitm ASSIGNING <lwa_fpitm>
          WITH KEY aufnr = lwa_fmfpcnf-aufnr
                   posnr = lwa_fmfpcnf-posnr BINARY SEARCH.
        CHECK sy-subrc EQ 0.

*      gs_variables-item_infocus = <lwa_fpitm>-posnr.


        CLEAR: lwa_return_d.
        READ TABLE lt_return_d INTO lwa_return_d
          WITH KEY type   = 'I' " c_msg_type-info
                   row    = lv_row
                   id     = 'RU'
                   number = '100' BINARY SEARCH.
        CHECK sy-subrc EQ 0.

        MOVE-CORRESPONDING <lwa_fpitm> TO lwa_ocopr.
        MOVE-CORRESPONDING lwa_fmfpcnf TO lwa_ocopr.
        lwa_ocopr-rueck = lwa_return_d-conf_no.
        lwa_ocopr-rmzhl = lwa_return_d-conf_cnt.
        lwa_ocopr-rsnum = lwa_fpdoc-x-fphdr-rsnum.
        lwa_ocopr-isdd  = lwa_fmfpcnf-isdd.
        lwa_ocopr-iedd  = sy-datum.
        lwa_ocopr-iedz  = sy-uzeit.
        lwa_ocopr-updkz = 'I'. " c_updkz_new.
        APPEND lwa_ocopr TO lwa_fpoc_doc-x-ocopr.

        IF lwa_fmfpcnf-gicre IS NOT INITIAL.
*          LOOP AT lwa_fpdoc-x-fpcom INTO lwa_fpcom
          LOOP AT gt_components INTO DATA(lwa_compo)
                                  WHERE aufnr EQ <lwa_fpitm>-aufnr
                                    AND posnr EQ <lwa_fpitm>-posnr
                                    AND lmnga IS NOT INITIAL.
*            MOVE-CORRESPONDING lwa_fpcom TO lwa_occom.
            MOVE-CORRESPONDING lwa_compo TO lwa_occom.
            lwa_occom-rueck = lwa_return_d-conf_no.
            lwa_occom-rmzhl = lwa_return_d-conf_cnt.
            lv_ccontr = lv_ccontr + 1.
            lwa_occom-contr = lv_ccontr.
            lwa_occom-updkz = 'I'. " c_updkz_new.
            APPEND lwa_occom TO lwa_fpoc_doc-x-occom.
          ENDLOOP.
          IF <lwa_fpbch>-codat IS INITIAL.
            <lwa_fpbch>-codat = lwa_fmfpcnf-budat.
            IF <lwa_fpbch>-updkz NE 'I'. " c_updkz_new.
              <lwa_fpbch>-updkz = 'U'. " c_updkz_update.
            ENDIF.
          ENDIF.
        ENDIF.

        IF <lwa_fpitm>-grcre IS NOT INITIAL.
          CLEAR: lv_quantity.
          IF <lwa_fpitm>-umren IS NOT INITIAL.
            lv_quantity = lwa_fmfpcnf-lmnga / <lwa_fpitm>-umren.
          ENDIF.
          IF lv_quantity IS NOT INITIAL.

            lwa_fpoc_doc-x-ochdr-menge = lv_quantity.
            lwa_fpoc_doc-x-ochdr-meins = lwa_fpdoc-x-fphdr-gmein.
            lwa_fpoc_doc-x-ochdr-matnr = lwa_fpdoc-x-fphdr-matnr.
            lwa_fpoc_doc-x-ochdr-charg = lwa_fpoc_doc-x-ochdr-charg_ref.
            lwa_fpdoc-x-fphdr-gwemg =
                        lwa_fpdoc-x-fphdr-gwemg + lv_quantity.
            IF lwa_fpdoc-x-fphdr-updkz NE 'I'. " c_updkz_new.
              lwa_fpdoc-x-fphdr-updkz = 'U'. " c_updkz_update.
            ENDIF.
            IF lv_quantity NE <lwa_fpbch>-erfmg.
              MOVE-CORRESPONDING <lwa_fpbch> TO lwa_fpbch.
              CLEAR: lwa_fpbch-codat.
              lwa_fpbch-erfmg = <lwa_fpbch>-erfmg - lv_quantity.
              IF lwa_fpbch-erfmg GT 0.
                lv_gcontr = lv_gcontr + 1.
                lwa_fpbch-contr = lv_gcontr.
                lwa_fpbch-updkz = 'I'. " c_updkz_new.
                APPEND lwa_fpbch TO lt_fpbch_new.
              ENDIF.
              CLEAR: lwa_fpbch.
            ENDIF.
            <lwa_fpbch>-grdat = lwa_fmfpcnf-budat.
            <lwa_fpbch>-gwemg = lv_quantity.
            <lwa_fpbch>-erfmg = <lwa_fpbch>-gwemg.
            IF <lwa_fpbch>-updkz NE 'I'. " c_updkz_new.
              <lwa_fpbch>-updkz = 'U'. " c_updkz_update.
            ENDIF.

          ENDIF.
        ENDIF.
***Extended Additional Syntax Check 1_3 ATC  1709 PQ

        CLEAR ls_message.
        ls_message-msgty = 'S'.
        ls_message-msgid = '/AGRI/FMFP'.
        ls_message-msgno = '013'.

        APPEND ls_message TO et_messages.
        CLEAR ls_message.
*****

      ENDLOOP.
      IF sy-subrc EQ 0.
        lwa_fpoc_doc-x-ochdr-updkz = 'I'. " c_updkz_new.
        lwa_ocindx-aufnr = lwa_fpdoc-aufnr.
        lwa_ocindx-gwemg = lwa_fpoc_doc-x-ochdr-menge.
        lwa_ocindx-gmein = lwa_fpoc_doc-x-ochdr-meins.
        lwa_ocindx-aufnr = lwa_fpdoc-aufnr.
        lwa_ocindx-updkz = 'I'. " c_updkz_new.
        APPEND lwa_ocindx TO lwa_fpoc_doc-x-ocindx.
        APPEND lwa_fpoc_doc TO lt_ocdoc.
        CLEAR: lwa_fpoc_doc, lwa_ocindx.
      ENDIF.

    ENDLOOP.

    IF lt_fpbch_new IS NOT INITIAL.
      APPEND LINES OF lt_fpbch_new TO lwa_fpdoc-x-fpbch.
    ENDIF.


    IF lv_subrc EQ 0.

      DATA: lwa_update_aufnr TYPE /agri/s_fmaufnr,
            lt_fpdoc_fin     TYPE /agri/t_fmfp_doc.

      CLEAR: lv_subrc.
      REFRESH lt_fpdoc_fin[].

*      READ TABLE lwa_fpdoc-x-fpbch INTO DATA(ls_batch)
*                                   WITH KEY contr = is_batch_crt-contr.
      LOOP AT lwa_fpdoc-x-fpbch INTO DATA(ls_batch)
                               WHERE contr = is_batch_crt-contr.
        ls_batch-gwemg    = ls_batch-erfmg.
        ls_batch-grdat    = sy-datum.
        ls_batch-zzconf   = abap_true. " Batch Confirmation
        IF ls_batch-codat IS INITIAL.
          ls_batch-codat    = sy-datum.
        ENDIF.
        ls_batch-updkz    = 'U'.
        MODIFY lwa_fpdoc-x-fpbch FROM ls_batch TRANSPORTING gwemg grdat zzconf codat.
        CLEAR ls_batch.
      ENDLOOP.

      APPEND lwa_fpdoc TO lt_fpdoc_fin.
      DATA lt_messages TYPE /agri/t_gprolog.
      CALL FUNCTION '/AGRI/FMFP_SAVE'
        EXPORTING
*         I_SET_UPDATE_TASK = 'X'
          i_commit_work = space " lv_commit_work
        CHANGING
          ct_fpdoc      = lt_fpdoc_fin
          ct_messages   = lt_messages
        EXCEPTIONS
          no_change     = 1
          OTHERS        = 2.
      WAIT UP TO 2 SECONDS.
      lv_subrc = sy-subrc.
      MOVE-CORRESPONDING lt_messages TO et_messages.
*      et_messages[] = lt_messages[].

      IF lv_subrc NE 0.
        IF lv_subrc EQ 1.

          CLEAR ls_message.
          ls_message-msgty = 'S'.
          ls_message-msgid = '/AGRI/GLOBAL'.
          ls_message-msgno = '322'. " Saving is not necessary; no changes were made

          APPEND ls_message TO et_messages.
          CLEAR ls_message.


        ELSE.

          CLEAR ls_message.
          ls_message-msgty = 'E'.
          ls_message-msgid = '/AGRI/FMFP'.
          ls_message-msgno = '008'. " Order creation failed

          APPEND ls_message TO et_messages.
          CLEAR ls_message.

        ENDIF.
      ELSE.
        IF lwa_fpdoc-x-fphdr-autyp EQ 'AO'. " c_document_category-production_order.
          lwa_update_aufnr-aufnr = lwa_fpdoc-x-fphdr-aufnr.
          APPEND lwa_update_aufnr TO gt_update_aufnr.
        ENDIF.

        CLEAR ls_message.
        ls_message-msgty = 'S'.
        ls_message-msgid = '/AGRI/FMFP'.
        ls_message-msgno = '007'. " Order &1 has been saved

        APPEND ls_message TO et_messages.
        CLEAR ls_message.
      ENDIF.

      IF lv_subrc EQ 0.

        CALL FUNCTION '/AGRI/FMFP_OC_SAVE'
          EXPORTING
*           I_SET_UPDATE_TASK = 'X'
            i_commit_work = c_true
          CHANGING
            ct_ocdoc      = lt_ocdoc[].
        WAIT UP TO 2 SECONDS.
      ELSE.

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD create_batch.

    TYPES: BEGIN OF ty_var,
             next_no TYPE  zabs_del_batch,
             ctext   TYPE  zabs_del_cnval,
             contr   TYPE  nrnr,
           END OF ty_var.

*--Local Declarations
    DATA: ls_var         TYPE ty_var,
          ls_str_batch   TYPE zabs_str_batch,  "LOC for batch creation
**          lwa_fphdr    TYPE /agri/s_fmfphdr,
          ls_constants   TYPE zabs_str_vkey_const,
          lt_constants   TYPE zabs_tty_vkey_const,
          lwa_return     TYPE bapiret2,
          lt_return      TYPE TABLE OF bapiret2,
          lv_nrlevel     TYPE nrlevel,
          lv_charg       TYPE charg_d,
          i_matnr        TYPE matnr,
          lv_batch       TYPE charg_d,

          lt_aufnr       TYPE /agri/t_fmaufnr,
          lt_fpdoc       TYPE /agri/t_fmfp_doc,
          lt_fpdoc_final TYPE /agri/t_fmfp_doc,
          ls_fpdoc       TYPE /agri/s_fmfp_doc,
          ls_batch       TYPE /agri/s_fmfpbch,
          lt_fpbch       TYPE TABLE OF /agri/fmfpbch, "/agri/t_fmfpbch,
          ls_fpbch       TYPE /agri/fmfpbch, "/agri/s_fmfpbch.
          lv_msgno       TYPE sy-msgno,
          lt_messages    TYPE /agri/t_gprolog,
          ls_messages    TYPE /agri/s_gprolog.

    MOVE-CORRESPONDING is_batch_crt TO ls_str_batch.

*--Validating UOM
    SELECT meinh
      FROM marm
      INTO TABLE @DATA(lt_meinh)
     WHERE matnr EQ @ls_str_batch-matnr
       AND meinh EQ @ls_str_batch-msehi.
    IF sy-subrc <> 0.
**      MESSAGE s142(zabs_msgcls)
**      DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_error. "Enter valid UOM
**      gs_variables-error = abap_true.
**      RETURN.

**        CLEAR : ls_message.
**        ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error.
**        ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
**        ls_message-msgno = .
**        add_messages_to_msg_container( is_message = ls_message ).
**        RETURN.

    ENDIF.

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
      CONCATENATE ls_var-ctext ls_var-next_no INTO lv_batch. "lv_charg.

      SELECT charg
        FROM mch1 UP TO 1 ROWS
        INTO @DATA(lv_batch_x) " (lv_charg_x)
       WHERE charg EQ @lv_batch. " @lv_charg.
      ENDSELECT.

*--Wrong number. The MCH1 batch and number range are not synchronized.
      IF sy-subrc EQ 0.
*--Calling Number Range FM to assign next free number
        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr = ls_var-contr  "'01'
            object      = zcl_abs_abap_maintain=>c_obj_btch_nr. "'ZABS_BATCH'.

        ls_var-next_no = lv_nrlevel + 1.

*        IF zabs_str_batch-matnr IS NOT INITIAL.
        IF i_matnr IS NOT INITIAL.
          CONCATENATE ls_var-ctext ls_var-next_no INTO lv_batch. " lv_charg.

          DO.
*--Fetching Batch data
            SELECT charg
              FROM mch1 UP TO 1 ROWS
              INTO @lv_batch_x " @lv_charg_x
             WHERE charg EQ @lv_batch. " @lv_charg.
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
              CONCATENATE ls_var-ctext ls_var-next_no INTO lv_batch. " lv_charg.
            ENDIF.
          ENDDO.
        ENDIF.
      ENDIF.
    ENDIF.

    "NUMBER_RANGE

*--Calling Number Range FM to assign next free number
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = ls_var-contr  "'01'
        object      = zcl_abs_abap_maintain=>c_obj_btch_nr. "'ZABS_BATCH'.


    CHECK lv_batch IS NOT INITIAL.

    CALL FUNCTION 'BAPI_BATCH_CREATE'
      EXPORTING
        batch         = lv_batch
        plant         = ls_str_batch-iwerk " lwa_fphdr-iwerk
        material_long = ls_str_batch-matnr " lwa_fphdr-matnr
      IMPORTING
        batch         = lv_charg
      TABLES
        return        = lt_return.

    IF lv_charg IS NOT INITIAL.
      MOVE-CORRESPONDING is_batch_crt TO es_batch_crt.
      es_batch_crt-charg = lv_charg.
    ENDIF.

**    LOOP AT lt_return INTO lwa_return
**                     WHERE type EQ c_msg_type-error.
**      MESSAGE ID lwa_return-id TYPE c_msg_type-error
**           NUMBER lwa_return-number
**             WITH lwa_return-message_v1 lwa_return-message_v2
**                  lwa_return-message_v3 lwa_return-message_v4
**             INTO sy-msgli.
**      message_simple space.
**    ENDLOOP.
**    IF sy-subrc NE 0.
**      MESSAGE s063(/agri/fmfp) WITH lv_charg
**                               INTO sy-msgli.
**      message_simple space.
**      WAIT UP TO 1 SECONDS.
**      PERFORM update_batch_characteristics USING lv_batch
**                                                 lwa_fphdr.
**    ELSE.
**      lv_subrc = 4.
**      EXIT.
**    ENDIF.

**ENDFORM.                    " CREATE_BATCH

    READ TABLE lt_return INTO lwa_return WITH KEY type = 'S'.
    IF sy-subrc = 0.


      CLEAR ls_fpdoc.
      REFRESH lt_aufnr[].
      CHECK is_batch_crt-aufnr IS NOT INITIAL.
      APPEND is_batch_crt-aufnr TO lt_aufnr.

      REFRESH lt_fpdoc[].
      CALL FUNCTION '/AGRI/FMFP_VIEW'
        EXPORTING
          it_aufnr       = lt_aufnr
        IMPORTING
          et_fpdoc       = lt_fpdoc " ls_fpdoc
        EXCEPTIONS
          no_data_exists = 1
          OTHERS         = 2.
      IF sy-subrc = 0.

        READ TABLE lt_fpdoc INTO ls_fpdoc INDEX 1.

        CLEAR: ls_batch.
        ls_batch-aufnr = ls_fpdoc-x-fphdr-aufnr.
        lt_fpbch[] = ls_fpdoc-x-fpbch[].
        SORT lt_fpbch BY contr DESCENDING.
        READ TABLE lt_fpbch INTO ls_fpbch INDEX 1.
        IF sy-subrc = 0.
          ls_batch-contr = ls_fpbch-contr.
        ENDIF.

        ls_batch-contr     = ls_batch-contr + 1.
        es_batch_crt-contr = ls_batch-contr.
        ls_batch-matnr     = ls_str_batch-matnr.
        ls_batch-charg     = lv_charg.
        ls_batch-erfmg     = ls_str_batch-erfmg. "ls_fpdoc-x-fphdr-gamng.
        ls_batch-gwemg     = ls_str_batch-erfmg. "ls_fpdoc-x-fphdr-gamng.
        ls_batch-erfme     = ls_str_batch-msehi.
**        ls_batch-zzconf    = abap_true. " Batch Confirmation
        ls_batch-codat     = ls_str_batch-date.
        ls_batch-erdat     = sy-datum.
        ls_batch-erzet     = sy-uzeit.
        ls_batch-grdat     = ls_str_batch-date. "sy-datum.
        ls_batch-budat     = ls_str_batch-date.
        ls_batch-zzbudat   = ls_str_batch-date.
        ls_batch-updkz     = 'I'.
**      ls_batch-zzmdocm = ls_str_batch-mdocm.
        APPEND ls_batch TO ls_fpdoc-x-fpbch.

        REFRESH lt_fpdoc_final[].
        APPEND ls_fpdoc TO lt_fpdoc_final.

        CALL FUNCTION 'ENQUEUE_ESORDER'
          EXPORTING
*           MODE_AUFK      = 'E'
*           MANDT          = SY-MANDT
            aufnr          = ls_fpdoc-aufnr
*           X_AUFNR        = ' '
*           _SCOPE         = '2'
*           _WAIT          = ' '
*           _COLLECT       = ' '
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.
        IF sy-subrc EQ 0.

          CALL FUNCTION 'ENQUEUE_EMRKPF'
            EXPORTING
*             MODE_RKPF      = 'E'
*             MANDT          = SY-MANDT
              rsnum          = ls_fpdoc-x-fphdr-rsnum
*             X_RSNUM        = ' '
*             _SCOPE         = '2'
*             _WAIT          = ' '
*             _COLLECT       = ' '
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.
        ENDIF.

        IF sy-subrc IS NOT INITIAL.
          lv_msgno = 037.
          ls_messages-msgid = '/AGRI/FMFP'.
          ls_messages-msgno = lv_msgno.
          ls_messages-msgty = 'E'.
          ls_messages-msgv2 =  sy-msgv1.
          ls_messages-msgv1 = ls_fpdoc-aufnr.
          APPEND ls_messages TO et_messages.
          ev_subrc = 4.
        ELSE.

          CALL FUNCTION '/AGRI/FMFP_SAVE'
            EXPORTING
*             I_SET_UPDATE_TASK = 'X'
              i_commit_work = abap_true
            CHANGING
              ct_fpdoc      = lt_fpdoc_final
              ct_messages   = et_messages
            EXCEPTIONS
              no_change     = 1
              OTHERS        = 2.

          IF sy-subrc NE 0.
            IF sy-subrc EQ 1.
              lv_msgno = 322.
              ls_messages-msgid = '/AGRI/GLOBAL'.
              ls_messages-msgno = lv_msgno.
              ls_messages-msgty = 'S'.
              APPEND ls_messages TO et_messages.
            ELSE.
              lv_msgno = 008.
              ls_messages-msgid = '/AGRI/FMFP'.
              ls_messages-msgno = lv_msgno.
              ls_messages-msgty = 'E'.
              ls_messages-msgv1 = ls_fpdoc-aufnr.
              APPEND ls_messages TO et_messages.

            ENDIF.
          ELSE.
            lv_msgno = 197.
            ls_messages-msgid = 'ZABS_MSGCLS'.
            ls_messages-msgno = lv_msgno.
            ls_messages-msgty = 'S'.
            ls_messages-msgv1 = lv_charg.
            APPEND ls_messages TO et_messages.
          ENDIF.

          CALL FUNCTION 'DEQUEUE_ESORDER'
            EXPORTING
*             MODE_AUFK = 'E'
*             MANDT     = SY-MANDT
              aufnr     = ls_fpdoc-aufnr
*             X_AUFNR   = ' '
*             _SCOPE    = '3'
              _synchron = 'X'
*             _COLLECT  = ' '
            .
          CALL FUNCTION 'DEQUEUE_EMRKPF'
            EXPORTING
*             MODE_RKPF = 'E'
*             MANDT     = SY-MANDT
              rsnum     = ls_fpdoc-x-fphdr-rsnum
*             X_RSNUM   = ' '
*             _SCOPE    = '3'
              _synchron = 'X'
*             _COLLECT  = ' '
            .
        ENDIF.
      ENDIF.
    ENDIF.

*&---------------------------------------------------------------------*


  ENDMETHOD.


  METHOD get_audit_report.

*-- Local Declarations
    DATA : lt_filter        TYPE /iwbep/t_mgw_select_option,
           ls_filter        TYPE /iwbep/s_mgw_select_option,
           lo_filter        TYPE REF TO /iwbep/if_mgw_req_filter,
           lv_filter_str    TYPE string,
           ls_message       TYPE /agri/s_gprolog,
           lv_cnval1        TYPE zabs_del_cnval,
           lv_days          TYPE t5a4a-dlydy,
           lv_lowdate       TYPE p0001-begda,

           ltr_tplnr_fl     TYPE RANGE OF /agri/gltplnr_fl,
           lsr_tplnr_fl     LIKE LINE OF ltr_tplnr_fl,
           ltr_date         TYPE RANGE OF /agri/fmcodat,
           lsr_date         LIKE LINE OF ltr_date,

           lwa_audit_report TYPE zabs_s_nur_audit_rept.

    CLEAR: lv_cnval1, lv_days, lv_lowdate.
*--Get variant table data
**    CALL METHOD zcl_abs_get_variants=>get_constant_single
**      EXPORTING
**        iv_mod    = zcl_abs_abap_maintain=>c_custom_mode
**        iv_objid  = zcl_abs_abap_maintain=>c_objid_mobl
**        iv_k1val  = zcl_abs_abap_maintain=>c_key_nur
**        iv_k2val  = zcl_abs_abap_maintain=>c_key_audit
**      IMPORTING
**        ev_cnval1 = lv_cnval1.
**
**    lv_days = lv_cnval1.
**
**    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
**      EXPORTING
**        date      = sy-datum
**        days      = lv_days
**        months    = 0
**        signum    = '-'
**        years     = 0
**      IMPORTING
**        calc_date = lv_lowdate.

    lo_filter     = io_tech_request_context->get_filter( ).
    lt_filter     = lo_filter->get_filter_select_options( ).
    lv_filter_str = lo_filter->get_filter_string( ).

    IF  lv_filter_str IS NOT INITIAL
    AND lt_filter[]   IS INITIAL.
      me->/iwbep/if_sb_dpc_comm_services~log_message(
      EXPORTING
        iv_msg_type   = zcl_abs_abap_maintain=>c_msgty_error "'E'
        iv_msg_id     = '/IWBEP/MC_SB_DPC_ADM'
        iv_msg_number = 025 ).
      " Raise Exception
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
        EXPORTING
          textid = /iwbep/cx_mgw_tech_exception=>internal_error.
    ENDIF.

*--Maps filter table lines to function module parameters
    LOOP AT lt_filter INTO ls_filter.
      CASE ls_filter-property.
        WHEN 'TPLNR_FL'. "--Terrain
          lo_filter->convert_select_option(
            EXPORTING
              is_select_option = ls_filter
            IMPORTING
              et_select_option = ltr_tplnr_fl ).
        WHEN 'ERDAT'. "--Date
          lo_filter->convert_select_option(
            EXPORTING
              is_select_option = ls_filter
            IMPORTING
              et_select_option = ltr_date ).
          READ TABLE ltr_date INTO lsr_date INDEX 1.
          IF sy-subrc = 0.
            lv_lowdate = lsr_date-low.
          ENDIF.

        WHEN OTHERS.
          " Log message in the application log
          me->/iwbep/if_sb_dpc_comm_services~log_message(
            EXPORTING
              iv_msg_type   = zcl_abs_abap_maintain=>c_msgty_error "'E'
              iv_msg_id     = '/IWBEP/MC_SB_DPC_ADM'
              iv_msg_number = 020
              iv_msg_v1     = ls_filter-property ).
          " Raise Exception
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
            EXPORTING
              textid = /iwbep/cx_mgw_tech_exception=>internal_error.
      ENDCASE.
    ENDLOOP.

*      READ TABLE ltr_tplnr_fl INTO lsr_tplnr_fl INDEX 1.
*      LOOP AT ltr_tplnr_fl INTO lsr_tplnr_fl.
*        CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
*          EXPORTING
*            input      = lsr_tplnr_fl-low
*          IMPORTING
*            output     = lsr_tplnr_fl-low
*          EXCEPTIONS
*            not_found  = 1
*            not_active = 2
*            OTHERS     = 3.
*        MODIFY ltr_tplnr_fl FROM lsr_tplnr_fl TRANSPORTING low.
*      ENDLOOP.

      SELECT * FROM /agri/fmfphdr
        INTO TABLE @DATA(lt_hdr)
       WHERE tplnr_fl IN @ltr_tplnr_fl. " eq @lsr_tplnr_fl-low.

      IF sy-subrc IS NOT INITIAL.
        CLEAR ls_message.
        ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error.
        ls_message-msgid = zcl_abs_abap_maintain=>c_msg_class_/agri/fmfp.
        ls_message-msgno = '056'.
        add_messages_to_msg_container( is_message   = ls_message ).
        CLEAR ls_message.
      ENDIF.

    IF lv_lowdate IS NOT INITIAL.
      SELECT * FROM /agri/fmfpbch
        INTO TABLE @DATA(lt_bch)
        FOR ALL ENTRIES IN @lt_hdr
        WHERE aufnr EQ @lt_hdr-aufnr
          AND erdat EQ @lv_lowdate.
    ELSE.
      SELECT * FROM /agri/fmfpbch
         INTO TABLE @lt_bch
         FOR ALL ENTRIES IN @lt_hdr
         WHERE aufnr EQ @lt_hdr-aufnr.
    ENDIF.


    IF sy-subrc EQ 0.
      SORT lt_bch BY aufnr contr.

*        DELETE FROM lt_hdr WHERE aufnr NE lt_bch-aufnr.

*      SELECT * FROM /agri/fmfphdr
*        INTO TABLE @DATA(lt_hdr)
*     FOR ALL ENTRIES IN @lt_bch
*       WHERE aufnr EQ @lt_bch-aufnr.
*      IF sy-subrc EQ 0.
      IF lt_hdr[] IS NOT INITIAL.
        SELECT tplnr_fl, pltxt
        FROM /agri/glflot
  INTO TABLE @DATA(lt_glflot)
     FOR ALL ENTRIES IN @lt_hdr
       WHERE tplnr_fl   = @lt_hdr-tplnr_fl
         AND kfrst      = @space
         AND loevm      = @space.
        IF sy-subrc EQ 0.
          SORT lt_glflot BY tplnr_fl.
        ENDIF.

        DATA(lt_hdr_1) = lt_hdr.
        SORT lt_hdr_1 BY matnr.
        DELETE ADJACENT DUPLICATES FROM lt_hdr_1.
        IF lt_hdr_1[] IS NOT INITIAL.
          SELECT matnr, maktx
            FROM makt INTO TABLE @DATA(lt_makt)
         FOR ALL ENTRIES IN @lt_hdr_1
           WHERE matnr EQ @lt_hdr_1-matnr.
          IF sy-subrc = 0.
            SORT lt_makt BY matnr.
          ENDIF.
        ENDIF.
      ENDIF.

      SELECT * FROM zabst_btchchr
        INTO TABLE @DATA(lt_btchchr)
     FOR ALL ENTRIES IN @lt_bch
       WHERE aufnr EQ @lt_bch-aufnr
         AND contr EQ @lt_bch-contr.
      IF sy-subrc EQ 0.
        SORT lt_btchchr BY aufnr contr.
      ENDIF.
*      endif.
      "------------------------------------------Start of Batch Confirmations
      DATA: lt_ocnum TYPE zabs_tty_nur_agri_fmocindx.
      SELECT ocnum FROM /agri/fmocindx
        INTO TABLE lt_ocnum
     FOR ALL ENTRIES IN lt_bch
       WHERE aufnr EQ lt_bch-aufnr.

      CALL METHOD me->get_batchcnfm_data
        EXPORTING
          it_ocnum         = lt_ocnum
          i_lowdate        = lv_lowdate
        IMPORTING
          et_btchcnfm_data = DATA(lt_batch_data).

      IF lt_batch_data[] IS NOT INITIAL.
        SELECT matnr, maktx
            FROM makt APPENDING TABLE @lt_makt
         FOR ALL ENTRIES IN @lt_batch_data
           WHERE matnr EQ @lt_batch_data-matnr.
        IF sy-subrc = 0.
          SORT lt_makt BY matnr.
        ENDIF.
      ENDIF.
      "------------------------------------------End of Batch Confirmations
    ELSEIF sy-subrc IS NOT INITIAL.
      CLEAR ls_message.
      ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error.
      ls_message-msgid = zcl_abs_abap_maintain=>c_msg_class_/agri/fmfp.
      ls_message-msgno = '056'.
      add_messages_to_msg_container( is_message   = ls_message ).
      CLEAR ls_message.

    ENDIF.

*    ELSEIF sy-subrc IS NOT INITIAL.
*      CLEAR ls_message.
*      ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error.
*      ls_message-msgid = zcl_abs_abap_maintain=>c_msg_class_/agri/fmfp.
*      ls_message-msgno = '056'.
*      add_messages_to_msg_container( is_message   = ls_message ).
*      CLEAR ls_message.
*
*    ENDIF.

    LOOP AT lt_hdr INTO DATA(lwa_hdr).
      READ TABLE lt_glflot INTO DATA(lwa_glflot)
                           WITH KEY tplnr_fl = lwa_hdr-tplnr_fl BINARY SEARCH.
      READ TABLE lt_makt INTO DATA(lwa_makt)
                               WITH KEY matnr = lwa_hdr-matnr BINARY SEARCH.
      LOOP AT lt_bch INTO DATA(lwa_bch) WHERE aufnr = lwa_hdr-aufnr.
        READ TABLE lt_batch_data INTO DATA(lwa_batch_data_t)
                                 WITH KEY aufnr = lwa_bch-aufnr
                                          contr_ref = lwa_bch-contr.
        LOOP AT lt_btchchr INTO DATA(lwa_btchchr) WHERE aufnr EQ lwa_bch-aufnr
                                                    AND contr EQ lwa_bch-contr
                                                    AND value IS NOT INITIAL.

          lwa_audit_report-aufnr    = lwa_hdr-aufnr.
          lwa_audit_report-contr    = lwa_bch-contr.
          lwa_audit_report-matnr    = lwa_hdr-matnr.
          lwa_audit_report-maktx    = lwa_makt-maktx.
          lwa_audit_report-tplnr_fl = lwa_hdr-tplnr_fl.
          lwa_audit_report-pltxt    = lwa_glflot-pltxt.
          lwa_audit_report-charg    = lwa_bch-charg.
          lwa_audit_report-erfmg    = lwa_bch-erfmg.
          lwa_audit_report-erfme    = lwa_bch-erfme.
*          lwa_audit_report-erzet    = lwa_batch_data_t-erzet.
          CLEAR: lwa_bch-erfmg, lwa_bch-erfme, lwa_batch_data_t-erzet.
          lwa_audit_report-erdat    = lwa_bch-erdat.
          lwa_audit_report-erzet    = lwa_bch-erzet.
          lwa_audit_report-codat    = lwa_bch-codat.
          lwa_audit_report-atbez    = lwa_btchchr-atbez.
          lwa_audit_report-value    = lwa_btchchr-value.
          lwa_audit_report-kurztext = lwa_btchchr-kurztext.
*          lwa_audit_report-MATNR_BC = lwa_batch_data-matnr.
*          lwa_audit_report-MAKTX_BC =
*          lwa_audit_report-CHARG_BC = lwa_batch_data-charg.
*          lwa_audit_report-LMNGA    = lwa_batch_data-menge.
*          lwa_audit_report-erfme_bc = lwa_batch_data-meins.

          APPEND lwa_audit_report TO et_audit_report.
          CLEAR lwa_audit_report.
        ENDLOOP.
      ENDLOOP.
      CLEAR: lwa_glflot, lwa_makt.
    ENDLOOP.

    LOOP AT lt_hdr INTO lwa_hdr.
      READ TABLE lt_glflot INTO lwa_glflot
                           WITH KEY tplnr_fl = lwa_hdr-tplnr_fl BINARY SEARCH.
      READ TABLE lt_makt INTO lwa_makt
                               WITH KEY matnr = lwa_hdr-matnr BINARY SEARCH.
      LOOP AT lt_bch INTO lwa_bch WHERE aufnr = lwa_hdr-aufnr.
        LOOP AT lt_batch_data INTO DATA(lwa_batch_data) WHERE aufnr EQ lwa_bch-aufnr
                                                         AND contr_ref EQ lwa_bch-contr.
          READ TABLE lt_makt INTO DATA(lwa_makt_bc)
                               WITH KEY matnr = lwa_batch_data-matnr BINARY SEARCH.

          lwa_audit_report-aufnr    = lwa_hdr-aufnr.
          lwa_audit_report-contr    = lwa_bch-contr.
          lwa_audit_report-matnr    = lwa_hdr-matnr.
          lwa_audit_report-maktx    = lwa_makt-maktx.
          lwa_audit_report-tplnr_fl = lwa_hdr-tplnr_fl.
          lwa_audit_report-pltxt    = lwa_glflot-pltxt.
          lwa_audit_report-charg    = lwa_bch-charg.
          lwa_audit_report-codat    = lwa_bch-codat.
          lwa_audit_report-erdat    = lwa_bch-erdat.
          lwa_audit_report-erzet    = lwa_bch-erzet.
          lwa_audit_report-matnr_bc = lwa_batch_data-matnr.
          lwa_audit_report-maktx_bc = lwa_makt_bc-maktx.
          lwa_audit_report-charg_bc = lwa_batch_data-charg.
          lwa_audit_report-lmnga    = lwa_batch_data-menge.
          lwa_audit_report-erfme_bc = lwa_batch_data-meins.

          APPEND lwa_audit_report TO et_audit_report.
          CLEAR: lwa_audit_report, lwa_makt_bc.
        ENDLOOP.
      ENDLOOP.
      CLEAR: lwa_glflot, lwa_makt.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_badge_route.

*-- Local Declarations
    DATA : lt_filter     TYPE /iwbep/t_mgw_select_option,
           ls_filter     TYPE /iwbep/s_mgw_select_option,
           ls_message    TYPE /agri/s_gprolog,
           lv_filter_str TYPE string,
           lv_cnval1     TYPE zabs_del_cnval,
           lv_days       TYPE t5a4a-dlydy,
           lv_days_1(5)  TYPE n,
           lv_prevdate   TYPE p0001-begda,
           lv_lowdate    TYPE p0001-begda,
           lv_highdate   TYPE p0001-begda,

           lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
           ltr_badge     TYPE RANGE OF persno,
           lsr_badge     LIKE LINE OF ltr_badge,

           ls_badge_rout TYPE zabs_s_nur_route_terrain.

    lo_filter     = io_tech_request_context->get_filter( ).
    lt_filter     = lo_filter->get_filter_select_options( ).
    lv_filter_str = lo_filter->get_filter_string( ).

    IF  lv_filter_str IS NOT INITIAL
    AND lt_filter[]   IS INITIAL.
      me->/iwbep/if_sb_dpc_comm_services~log_message(
      EXPORTING
        iv_msg_type   = zcl_abs_abap_maintain=>c_msgty_error "'E'
        iv_msg_id     = '/IWBEP/MC_SB_DPC_ADM'
        iv_msg_number = 025 ).
      " Raise Exception
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
        EXPORTING
          textid = /iwbep/cx_mgw_tech_exception=>internal_error.
    ENDIF.

*--Maps filter table lines to function module parameters
    LOOP AT lt_filter INTO ls_filter.
      CASE ls_filter-property.
        WHEN 'PERNR'. "--BADGE
          lo_filter->convert_select_option(
            EXPORTING
              is_select_option = ls_filter
            IMPORTING
              et_select_option = ltr_badge ).

        WHEN OTHERS.
          " Log message in the application log
          me->/iwbep/if_sb_dpc_comm_services~log_message(
            EXPORTING
              iv_msg_type   = zcl_abs_abap_maintain=>c_msgty_error "'E'
              iv_msg_id     = '/IWBEP/MC_SB_DPC_ADM'
              iv_msg_number = 020
              iv_msg_v1     = ls_filter-property ).
          " Raise Exception
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
            EXPORTING
              textid = /iwbep/cx_mgw_tech_exception=>internal_error.
      ENDCASE.
    ENDLOOP.

    IF ltr_badge[] IS NOT INITIAL.
      SELECT *
      FROM zabs_usrpernr
      INTO TABLE @DATA(lt_rtusr)
      WHERE pernr IN @ltr_badge.
      IF sy-subrc IS NOT INITIAL.
        CLEAR ls_message.
        ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error.
        ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
        ls_message-msgno = '163'.
        add_messages_to_msg_container( is_message   = ls_message ).
        CLEAR ls_message.
      ENDIF.
    ENDIF.

    IF lt_rtusr[] IS NOT INITIAL.
      SORT lt_rtusr BY route.
      SELECT *
        FROM /agri/glrthdrt
        INTO TABLE @DATA(lt_hdrt)
     FOR ALL ENTRIES IN @lt_rtusr
       WHERE route EQ @lt_rtusr-route
         AND spras EQ @sy-langu.

      IF lt_hdrt[] IS NOT INITIAL.
        SORT lt_hdrt BY route.
      ENDIF.

      SELECT *
        FROM /agri/glrtfla
        INTO TABLE @DATA(lt_rtfla)
     FOR ALL ENTRIES IN @lt_rtusr                  "#EC CI_NO_TRANSFORM
       WHERE route  = @lt_rtusr-route.
    ENDIF.

    IF lt_rtfla[] IS NOT INITIAL.
      SORT lt_rtfla.
      SELECT tplnr_fl, pltxt
        FROM /agri/glflot
  INTO TABLE @DATA(lt_glflot)
     FOR ALL ENTRIES IN @lt_rtfla
       WHERE tplnr_fl   = @lt_rtfla-tplnr_fl
         AND kfrst      = @space
         AND loevm      = @space. "abap_false.

      IF sy-subrc = 0.

        CLEAR: lv_cnval1, lv_days.
*--Get variant table data
        CALL METHOD zcl_abs_get_variants=>get_constant_single
          EXPORTING
            iv_mod    = zcl_abs_abap_maintain=>c_custom_mode "'C'
            iv_objid  = zcl_abs_abap_maintain=>c_objid_mobl "'MOBL'
            iv_k1val  = zcl_abs_abap_maintain=>c_key_irr_days "'DAYS'
          IMPORTING
            ev_cnval1 = lv_cnval1. "'60'

        lv_days = lv_cnval1.

        CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
          EXPORTING
            date      = sy-datum
            days      = lv_days
            months    = 0
            signum    = '-'
            years     = 0
          IMPORTING
            calc_date = lv_prevdate.

        SORT lt_glflot BY tplnr_fl.
        SELECT tplnr_fl,
               contr,
               cmnum,
               season,
               datab,
               datbi,
               aarea,
               msehi,
               exhad,
               eston,
               esuom,
               ernam,
               erdat,
               erzet,
               aenam,
               aedat,
               aezet
         FROM /agri/glflcma
               INTO TABLE @DATA(lt_flcma)
         FOR ALL ENTRIES IN @lt_glflot
              WHERE tplnr_fl = @lt_glflot-tplnr_fl
*                AND cmnum IN @lrt_cmnum
*                AND astat EQ @lc_astat
                AND datab LE @sy-datum
                AND datbi GE @lv_prevdate
***                AND datab <= @sy-datum
***                AND datbi >= @sy-datum
                 AND loevm  = @space.

      ENDIF. "lt_glflot
    ENDIF. "lt_rtfla

    CLEAR: lv_cnval1, lv_days.
*--Get variant table data
    CALL METHOD zcl_abs_get_variants=>get_constant_single
      EXPORTING
        iv_mod    = zcl_abs_abap_maintain=>c_custom_mode
        iv_objid  = zcl_abs_abap_maintain=>c_objid_mobl
        iv_k1val  = zcl_abs_abap_maintain=>c_key_inspop
        iv_k2val  = zcl_abs_abap_maintain=>c_key_lgstrp
        iv_k3val  = zcl_abs_abap_maintain=>c_key_13
      IMPORTING
        ev_cnval1 = lv_cnval1.

**    lv_days = lv_cnval1.
**
**    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
**      EXPORTING
**        date      = sy-datum
**        days      = lv_days
**        months    = 0
**        signum    = '-'
**        years     = 0
**      IMPORTING
**        calc_date = lv_lowdate.

    lv_lowdate = sy-datum - lv_cnval1.

    CLEAR: lv_cnval1, lv_days.
*--Get variant table data
    CALL METHOD zcl_abs_get_variants=>get_constant_single
      EXPORTING
        iv_mod    = zcl_abs_abap_maintain=>c_custom_mode
        iv_objid  = zcl_abs_abap_maintain=>c_objid_mobl
        iv_k1val  = zcl_abs_abap_maintain=>c_key_inspop
        iv_k2val  = zcl_abs_abap_maintain=>c_key_hgstrp
        iv_k3val  = zcl_abs_abap_maintain=>c_key_13
      IMPORTING
        ev_cnval1 = lv_cnval1.

    lv_days = lv_cnval1.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = sy-datum
        days      = lv_days
        months    = 0
        signum    = '+'
        years     = 0
      IMPORTING
        calc_date = lv_highdate.

    IF lt_flcma[] IS NOT INITIAL.
      SORT lt_flcma.
      SELECT *
        FROM /agri/fmfphdr
        INTO TABLE @DATA(lt_fphdr)
     FOR ALL ENTRIES IN @lt_flcma
       WHERE contr EQ @lt_flcma-contr
         AND cmnum EQ @lt_flcma-cmnum
         AND autyp EQ 'AO'
         AND class EQ '0'
         AND gstrp GE @lv_lowdate
         AND gstrp LE @lv_highdate
         AND tecom EQ @space.
      IF sy-subrc = 0.
        SORT lt_fphdr.

***        SELECT *
***          FROM /agri/fmfpbch
***          INTO TABLE @DATA(lt_fpbch)
***       FOR ALL ENTRIES IN @lt_fphdr
***         WHERE aufnr   EQ @lt_fphdr-aufnr
***           AND zzconf  EQ @abap_false
***           AND zzloevm EQ @abap_false.
***        IF sy-subrc = 0.
***          SORT lt_fpbch.
***        ENDIF.

        DATA(lt_fphdr_1) = lt_fphdr.
        SORT lt_fphdr_1 BY matnr.
        DELETE ADJACENT DUPLICATES FROM lt_fphdr_1.
        IF lt_fphdr_1[] IS NOT INITIAL.
          SELECT matnr, maktx
            FROM makt INTO TABLE @DATA(lt_makt)
         FOR ALL ENTRIES IN @lt_fphdr_1
           WHERE matnr EQ @lt_fphdr_1-matnr.
          IF sy-subrc = 0.
            SORT lt_makt BY matnr.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    CLEAR: lv_cnval1, lv_days, lv_lowdate.
*--Get variant table data
    CALL METHOD zcl_abs_get_variants=>get_constant_single
      EXPORTING
        iv_mod    = zcl_abs_abap_maintain=>c_custom_mode
        iv_objid  = zcl_abs_abap_maintain=>c_objid_mobl
        iv_k1val  = zcl_abs_abap_maintain=>c_key_inspop
        iv_k2val  = zcl_abs_abap_maintain=>c_key_vldays
      IMPORTING
        ev_cnval1 = lv_cnval1.

    lv_days = lv_cnval1.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = sy-datum
        days      = lv_days
        months    = 0
        signum    = '-'
        years     = 0
      IMPORTING
        calc_date = lv_lowdate.

    CLEAR: lv_cnval1, lv_days, lv_highdate.
*--Get variant table data
    CALL METHOD zcl_abs_get_variants=>get_constant_single
      EXPORTING
        iv_mod    = zcl_abs_abap_maintain=>c_custom_mode
        iv_objid  = zcl_abs_abap_maintain=>c_objid_mobl
        iv_k1val  = zcl_abs_abap_maintain=>c_key_inspop
        iv_k2val  = zcl_abs_abap_maintain=>c_key_vhdays
      IMPORTING
        ev_cnval1 = lv_cnval1.

    lv_days = lv_cnval1.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = sy-datum
        days      = lv_days
        months    = 0
        signum    = '+'
        years     = 0
      IMPORTING
        calc_date = lv_highdate.

    REFRESH et_badge_route[].

    LOOP AT lt_rtusr INTO DATA(ls_rtusr).
      READ TABLE lt_hdrt INTO DATA(ls_hdrt)
                         WITH KEY route = ls_rtusr-route
                       BINARY SEARCH.
      LOOP AT lt_rtfla INTO DATA(ls_rtfla) WHERE route = ls_rtusr-route.
**        LOOP AT lt_glflot INTO DATA(ls_glflot)
**                         WHERE tplnr_fl = ls_rtfla-tplnr_fl.
        READ TABLE lt_glflot INTO DATA(ls_glflot)
                             WITH KEY tplnr_fl = ls_rtfla-tplnr_fl
                           BINARY SEARCH.
        LOOP AT lt_flcma INTO DATA(ls_flcma)
                        WHERE tplnr_fl = ls_rtfla-tplnr_fl.
          LOOP AT lt_fphdr INTO DATA(ls_fphdr)
                          WHERE contr = ls_flcma-contr
                            AND cmnum = ls_flcma-cmnum.
**            LOOP AT lt_fpbch INTO DATA(ls_fpbch)
**                            WHERE aufnr = ls_fphdr-aufnr.
            READ TABLE lt_makt INTO DATA(ls_makt)
                               WITH KEY matnr = ls_fphdr-matnr
                             BINARY SEARCH.

            ls_badge_rout-pernr    = ls_rtusr-pernr.
            ls_badge_rout-route    = ls_rtusr-route.
            ls_badge_rout-routdesc = ls_hdrt-descr.
            ls_badge_rout-tplnr_fl = ls_fphdr-tplnr_fl.
            ls_badge_rout-pltxt    = ls_glflot-pltxt.
            ls_badge_rout-contr    = ls_fphdr-contr.
            ls_badge_rout-cmnum    = ls_fphdr-cmnum.
            ls_badge_rout-aufnr    = ls_fphdr-aufnr.
            ls_badge_rout-matnr    = ls_fphdr-matnr.
            ls_badge_rout-maktx    = ls_makt-maktx.
            ls_badge_rout-gstrp    = ls_fphdr-gstrp.
            ls_badge_rout-iwerk    = ls_fphdr-iwerk.
            ls_badge_rout-gamng    = ls_fphdr-gamng.
            ls_badge_rout-gwemg    = ls_fphdr-gwemg.
            ls_badge_rout-pndqty   = ls_fphdr-gamng - ls_fphdr-gwemg.
            ls_badge_rout-meins    = ls_fphdr-gmein.
            ls_badge_rout-lower_dt = lv_lowdate.
            ls_badge_rout-upper_dt = lv_highdate.
            APPEND ls_badge_rout TO et_badge_route.
**            ENDLOOP.
            CLEAR: ls_badge_rout, ls_fphdr, ls_makt.
          ENDLOOP.
        ENDLOOP.
      ENDLOOP.
      CLEAR: ls_flcma, ls_glflot, ls_rtfla, ls_rtusr, ls_hdrt.
    ENDLOOP.

    IF et_badge_route[] IS NOT INITIAL.
      SORT et_badge_route BY aufnr contr.
      DELETE ADJACENT DUPLICATES FROM et_badge_route
                       COMPARING aufnr contr.
      SORT et_badge_route BY pltxt matnr ASCENDING.
**      SORT et_badge_route BY route tplnr_fl matnr.
**      DELETE ADJACENT DUPLICATES FROM et_badge_route
**            COMPARING route tplnr_fl matnr.
    ENDIF.


  ENDMETHOD.


  METHOD get_batchcnfm_data.

    DATA: lt_afwi       TYPE TABLE OF afwi,
          lwa_afwi      TYPE afwi,
          lwa_afwi_ref  TYPE afwi,
          lwa_mseg      TYPE matdoc,
          lt_mseg       TYPE TABLE OF matdoc,
          lwa_affw      TYPE affw,
          lt_affw       TYPE TABLE OF affw,
          lwa_ochdr     TYPE /agri/s_fmochdr,
          lwa_ochdr_ref TYPE /agri/s_fmochdr,
          lt_ochdr      TYPE /agri/t_fmochdr,
          lwa_ocopr     TYPE /agri/s_fmocopr,
          lwa_ocopr_ref TYPE /agri/s_fmocopr,
          lt_ocopr      TYPE /agri/t_fmocopr,
          lwa_occom     TYPE /agri/s_fmoccom,
          lt_occom      TYPE /agri/t_fmoccom,
          lwa_details   TYPE /agri/s_fmoc_details,
          lv_mblnr      TYPE mblnr,
          lv_details    TYPE xfld VALUE 'X',
          lt_details    TYPE /agri/t_fmoc_details.

    CONSTANTS: BEGIN OF c_status,
                 confirmation   TYPE /agri/fmcglst VALUE 'A',
                 goods_movement TYPE /agri/fmcglst VALUE 'B',
                 cogi           TYPE /agri/fmcglst VALUE 'C',
               END OF c_status.

    FIELD-SYMBOLS: <lwa_afwi> LIKE LINE OF lt_afwi.

*    gs_variables-display_details = lv_details.
    CHECK it_ocnum IS NOT INITIAL.

    REFRESH: lt_details.
    SELECT * FROM /agri/fmochdr
             INTO CORRESPONDING FIELDS OF TABLE lt_ochdr
              FOR ALL ENTRIES IN it_ocnum
            WHERE ocnum EQ it_ocnum-ocnum.
    IF lt_ochdr IS NOT INITIAL.
      SORT lt_ochdr BY ocnum.
      SELECT * FROM /agri/fmocopr
        INTO CORRESPONDING FIELDS OF TABLE lt_ocopr
        FOR ALL ENTRIES IN lt_ochdr
        WHERE ocnum EQ lt_ochdr-ocnum.
      IF sy-subrc EQ 0.
        SORT lt_ocopr BY ocnum rueck rmzhl.
      ENDIF.
    ENDIF.

    IF lt_ocopr IS NOT INITIAL.
      SELECT * FROM /agri/fmoccom
        INTO CORRESPONDING FIELDS OF TABLE lt_occom
        FOR ALL ENTRIES IN lt_ocopr
        WHERE ocnum EQ lt_ocopr-ocnum
          AND rueck EQ lt_ocopr-rueck
          AND rmzhl EQ lt_ocopr-rmzhl.
      IF sy-subrc EQ 0.
        SORT lt_occom BY ocnum rueck rmzhl contr.
      ENDIF.
      SELECT *
              FROM afwi
              INTO CORRESPONDING FIELDS OF TABLE lt_afwi
               FOR ALL ENTRIES IN lt_ocopr
             WHERE rueck EQ lt_ocopr-rueck
               AND rmzhl EQ lt_ocopr-rmzhl.
      IF sy-subrc EQ 0.
        SORT lt_afwi BY rueck rmzhl mblnr mjahr mblpo.
        SELECT * FROM matdoc
                 INTO CORRESPONDING FIELDS OF TABLE lt_mseg
                  FOR ALL ENTRIES IN lt_afwi
                WHERE mblnr EQ lt_afwi-mblnr
                  AND mjahr EQ lt_afwi-mjahr
                  AND zeile EQ lt_afwi-mblpo.
        IF sy-subrc EQ 0.
          SORT lt_mseg BY shkzg rsnum rspos.
        ENDIF.
        SELECT * FROM affw
                 INTO CORRESPONDING FIELDS OF TABLE lt_affw
                  FOR ALL ENTRIES IN lt_afwi
                WHERE weblnr  EQ lt_afwi-mblnr.          "#EC CI_SORTED
*                AND weblpos EQ lt_afwi-mblpo.
      ENDIF.
    ENDIF.

    SORT lt_mseg BY mblnr mjahr zeile.
    SORT lt_ochdr BY ocnum.
    SORT lt_ocopr BY ocnum rueck rmzhl.
    SORT lt_afwi BY rueck rmzhl mblnr mjahr mblpo.

    LOOP AT lt_ochdr INTO lwa_ochdr.
      LOOP AT lt_ocopr INTO lwa_ocopr
                      WHERE ocnum EQ lwa_ochdr-ocnum.
        MOVE-CORRESPONDING lwa_ochdr TO lwa_details.
        CLEAR: lwa_details-matnr.
        MOVE-CORRESPONDING lwa_ocopr TO lwa_details.
        IF lv_details IS INITIAL.
          LOOP AT lt_afwi ASSIGNING <lwa_afwi> WHERE rueck EQ lwa_ocopr-rueck
                                                 AND rmzhl EQ lwa_ocopr-rmzhl.

            IF lv_mblnr NE <lwa_afwi>-mblnr.
              lv_mblnr = <lwa_afwi>-mblnr.
*          AT NEW mblnr.
              READ TABLE lt_mseg INTO lwa_mseg
                                 WITH KEY mblnr = <lwa_afwi>-mblnr
                                          mjahr = <lwa_afwi>-mjahr
                                 BINARY SEARCH.
              IF sy-subrc EQ 0.
                IF lwa_mseg-shkzg = 'H'.
                  lwa_details-mblnr_gi = lwa_mseg-mblnr.
                  lwa_details-mjahr_gi = lwa_mseg-mjahr.
                  lwa_details-bwart_gi = lwa_mseg-bwart.
                ELSEIF lwa_mseg-shkzg = 'S'.
                  lwa_details-mblnr = lwa_mseg-mblnr.
                  lwa_details-mjahr = lwa_mseg-mjahr.
                  lwa_details-bwart = lwa_mseg-bwart.
                ENDIF.
                lwa_details-budat_mkpf = lwa_mseg-budat.
              ENDIF.
            ENDIF.
            IF lwa_details-mblnr_gi IS NOT INITIAL AND
               lwa_details-mblnr IS NOT INITIAL.
              EXIT.
            ENDIF.
          ENDLOOP.
          IF lwa_details-mblnr_gi IS NOT INITIAL AND
             lwa_details-mblnr IS NOT INITIAL.
            EXIT.
          ENDIF.
        ELSE.
          CHECK lwa_ochdr-ocnum_ref IS INITIAL.
          READ TABLE lt_ochdr INTO lwa_ochdr_ref WITH KEY ocnum_ref = lwa_ochdr-ocnum
                                                 BINARY SEARCH.
          IF sy-subrc EQ 0.
            lwa_details-ocnum_ref = lwa_ochdr_ref-ocnum.
          ENDIF.
          lwa_details-menge = lwa_ocopr-lmnga.
          lwa_details-meins = lwa_ocopr-meinh.
          CLEAR: lwa_details-charg.
          lwa_details-cglst = c_status-confirmation.
          APPEND lwa_details TO lt_details.
          LOOP AT lt_afwi INTO lwa_afwi
                         WHERE rueck EQ lwa_ocopr-rueck
                           AND rmzhl EQ lwa_ocopr-rmzhl.
            READ TABLE lt_mseg INTO lwa_mseg
                           WITH KEY mblnr = lwa_afwi-mblnr
                                    mjahr = lwa_afwi-mjahr
                                    zeile = lwa_afwi-mblpo
                           BINARY SEARCH.
            IF sy-subrc EQ 0.
              MOVE-CORRESPONDING lwa_mseg TO lwa_details.
              lwa_details-cglst = c_status-goods_movement.
              lwa_details-budat_mkpf = lwa_mseg-budat.
            ELSE.
              READ TABLE lt_affw INTO lwa_affw WITH KEY weblnr = lwa_afwi-mblnr "#EC CI_SORTED
                                               BINARY SEARCH.
              IF sy-subrc EQ 0.
                MOVE-CORRESPONDING lwa_affw TO lwa_details.
                lwa_details-cglst = c_status-cogi.
                lwa_details-budat_mkpf = lwa_affw-budat.
                CLEAR: lwa_details-menge,
                       lwa_details-meins.
              ENDIF.
            ENDIF.

            READ TABLE lt_ocopr INTO lwa_ocopr_ref WITH KEY ocnum = lwa_details-ocnum_ref
                                                            vornr = lwa_details-vornr
                                                   BINARY SEARCH.
            IF sy-subrc EQ 0.
              READ TABLE lt_afwi INTO lwa_afwi_ref WITH KEY rueck = lwa_ocopr_ref-rueck
                                                                rmzhl = lwa_ocopr_ref-rmzhl
                                                       BINARY SEARCH.
              IF sy-subrc EQ 0.
                lwa_details-mblnr_gi = lwa_afwi_ref-mblnr.
                lwa_details-mjahr_gi = lwa_afwi_ref-mjahr.
                READ TABLE lt_mseg TRANSPORTING NO FIELDS
                                   WITH KEY mblnr = lwa_afwi_ref-mblnr
                                            mjahr = lwa_afwi_ref-mjahr
                                   BINARY SEARCH.
                IF sy-subrc NE 0.
                  lwa_details-cglst = c_status-cogi.
                ELSE.
                  lwa_details-cglst = c_status-goods_movement.
                ENDIF.
              ENDIF.
            ENDIF.
            APPEND lwa_details TO lt_details.
          ENDLOOP.
          CLEAR: lwa_details,lwa_afwi_ref,lwa_ocopr_ref,lwa_ochdr_ref.
        ENDIF.
      ENDLOOP.
      IF lv_details IS INITIAL.
        APPEND lwa_details TO lt_details.
      ENDIF.
      CLEAR lwa_details.
    ENDLOOP.

*    SORT lt_details by bwart budat_mkpf.
*    DELETE lt_details WHERE budat_mkpf LT i_lowdate.
    DELETE lt_details WHERE erdat LT i_lowdate.
    DELETE lt_details WHERE bwart NE '261'.

    et_btchcnfm_data[] = lt_details[].

  ENDMETHOD.


  METHOD get_btchcnfm_f4.

*--Types declaration
    TYPES: BEGIN OF ty_mchb,
             matnr TYPE matnr,
             charg TYPE charg_d,
             clabs TYPE labst,
             laeda TYPE dats,
             grcvr TYPE atwrt,
             grcgo TYPE atwrt,
             werks TYPE werks_d,
             lgort TYPE lgort_d,
           END OF ty_mchb.

    TYPES: BEGIN OF ty_char_value,
             atwrt      TYPE atwrt,
             codegruppe TYPE qcodegrp,
             code       TYPE qcode,
           END OF ty_char_value.

    TYPES: BEGIN OF ty_value,
             codegruppe TYPE qpk1ac-codegruppe,
             fillr(1)   TYPE c,
             code       TYPE qpk1ac-code,
           END OF ty_value.

*--Internal Tables
    DATA : lt_mchb       TYPE TABLE OF ty_mchb,
           lt_char_batch TYPE STANDARD TABLE OF clbatch,
           lt_fpcom      TYPE /agri/t_fmfpcom,
           lt_char_value TYPE STANDARD TABLE OF ty_char_value.

*--Workarea declarations
    DATA : ls_fpcom_in     TYPE /agri/s_fmfpcom,
           ls_task_itm_mod TYPE lvc_s_modi,
           ls_mchb         TYPE ty_mchb,
*           ls_fpcom        TYPE /agri/s_fmfpcom,
           ls_fpcnf_fcat   TYPE /agri/s_fmfp_cnf_fcat,
           ls_char_batch   TYPE clbatch,
           ls_char_value   TYPE ty_char_value,
           ls_value        TYPE ty_value.

*--Field-Symbols
    FIELD-SYMBOLS : <fs_mchb> TYPE ty_mchb.

*--Local Variables
    DATA : lv_date         TYPE string,
           lv_tabix        TYPE sy-tabix,
           lv_contr        TYPE /agri/fmccontr,
           lv_lmnga        TYPE /agri/fmlmnga,
           lv_grid_refresh.

    DATA: lv_confirm_qty_x    TYPE /agri/fmlmnga,
          lwa_fpcom_x         TYPE /agri/s_fmfpcom,
          lwa_fpcom_fcat_x    TYPE /agri/s_fmfpcom_fcat,
          lwa_fpdoc_infocus_x TYPE /agri/s_fmfp_doc,
          lwa_makt_x          TYPE makt,
          lwa_edit_x          TYPE lvc_s_styl.

*-- Local Declarations
    DATA : lt_filter     TYPE /iwbep/t_mgw_select_option,
           ls_filter     TYPE /iwbep/s_mgw_select_option,
           ls_message    TYPE /agri/s_gprolog,
           lv_filter_str TYPE string,
           lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,

           ltr_aufnr     TYPE RANGE OF /agri/fmfpnum,
           lsr_aufnr     LIKE LINE OF ltr_aufnr,
           ltr_contr     TYPE RANGE OF /agri/gcontr,
           lsr_contr     LIKE LINE OF ltr_contr,

           lt_aufnr      TYPE /agri/t_fmaufnr,
           lt_hdr        TYPE /agri/t_fmfphdr,
           lt_itm        TYPE /agri/t_fmfpitm,
           lt_com        TYPE /agri/t_fmfpcom,
*           LT_BCH  TYPE  /AGRI/T_FMFPBCH,

           ls_btchcnfm   TYPE zabs_s_nur_batch_confm_f4.

    IF i_aufnr IS INITIAL.
      lo_filter     = io_tech_request_context->get_filter( ).
      lt_filter     = lo_filter->get_filter_select_options( ).
      lv_filter_str = lo_filter->get_filter_string( ).

      IF  lv_filter_str IS NOT INITIAL
      AND lt_filter[]   IS INITIAL.
        me->/iwbep/if_sb_dpc_comm_services~log_message(
        EXPORTING
          iv_msg_type   = zcl_abs_abap_maintain=>c_msgty_error "'E'
          iv_msg_id     = '/IWBEP/MC_SB_DPC_ADM'
          iv_msg_number = 025 ).
        " Raise Exception
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
          EXPORTING
            textid = /iwbep/cx_mgw_tech_exception=>internal_error.
      ENDIF.

*--Maps filter table lines to function module parameters
      LOOP AT lt_filter INTO ls_filter.
        CASE ls_filter-property.
          WHEN 'AUFNR'. "--Order No.
            lo_filter->convert_select_option(
              EXPORTING
                is_select_option = ls_filter
              IMPORTING
                et_select_option = ltr_aufnr ).
          WHEN 'CONTR'. "--Counter
            lo_filter->convert_select_option(
              EXPORTING
                is_select_option = ls_filter
              IMPORTING
                et_select_option = ltr_contr ).

          WHEN OTHERS.
            " Log message in the application log
            me->/iwbep/if_sb_dpc_comm_services~log_message(
              EXPORTING
                iv_msg_type   = zcl_abs_abap_maintain=>c_msgty_error "'E'
                iv_msg_id     = '/IWBEP/MC_SB_DPC_ADM'
                iv_msg_number = 020
                iv_msg_v1     = ls_filter-property ).
            " Raise Exception
            RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
              EXPORTING
                textid = /iwbep/cx_mgw_tech_exception=>internal_error.
        ENDCASE.
      ENDLOOP.

      REFRESH lt_aufnr.
      IF ltr_aufnr[] IS NOT INITIAL.
        READ TABLE ltr_aufnr INTO lsr_aufnr INDEX 1.
        DATA(ls_aufnr) = lsr_aufnr-low.
        APPEND ls_aufnr TO lt_aufnr.
      ENDIF.
    ELSEIF i_aufnr IS NOT INITIAL.
      ls_aufnr = i_aufnr.
      APPEND ls_aufnr TO lt_aufnr.
    ENDIF.

    IF lt_aufnr[] IS NOT INITIAL.
      CALL FUNCTION '/AGRI/FMFP_READ'
        EXPORTING
          it_aufnr       = lt_aufnr
        IMPORTING
          et_fmfphdr     = lt_hdr
          et_fmfpitm     = lt_itm
          et_fmfpcom     = lt_com
*         ET_FMFPBCH     =
        EXCEPTIONS
          no_data_exists = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
**        CLEAR : ls_message.
**        ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error.
**        ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
**        ls_message-msgno = .
**        add_messages_to_msg_container( is_message = ls_message ).
**        RETURN.
      ENDIF.

      IF lt_hdr[] IS NOT INITIAL.
        SORT lt_hdr.
      ENDIF.
      IF lt_itm[] IS NOT INITIAL.
        SORT lt_itm.
      ENDIF.
      IF lt_com[] IS NOT INITIAL.
        SORT lt_com.
      ENDIF.

    ENDIF.


**    LOOP AT gt_fpcnf_fcat INTO ls_fpcnf_fcat.
    LOOP AT lt_itm INTO DATA(ls_itm).
*      LOOP AT gs_fpdoc_infocus-x-fpcom INTO DATA(ls_fpcom)
      LOOP AT lt_com INTO DATA(ls_fpcom)
                      WHERE aufnr EQ ls_itm-aufnr " ls_fpcnf_fcat-aufnr
                        AND posnr EQ ls_itm-posnr "ls_fpcnf_fcat-posnr
**                        AND flgch EQ zcl_abs_abap_maintain=>c_costing_rel_full "'X'
                        AND zztask IS INITIAL.
        CLEAR ls_mchb.
        ls_mchb-matnr = ls_fpcom-matnr.
        ls_mchb-werks = ls_fpcom-werks.
        ls_mchb-lgort = ls_fpcom-lgort.
        COLLECT ls_mchb INTO lt_mchb.
      ENDLOOP.
    ENDLOOP.

    IF lt_mchb[] IS NOT INITIAL.
*--Fetching Batch Stock data
      SELECT matnr charg
             clabs laeda
        FROM mchb
        INTO TABLE lt_mchb
        FOR ALL ENTRIES IN lt_mchb
       WHERE matnr EQ lt_mchb-matnr
         AND werks EQ lt_mchb-werks
         AND lgort EQ lt_mchb-lgort.

      IF sy-subrc EQ 0.
        SORT lt_mchb BY matnr laeda.
      ENDIF.
    ENDIF.

    "-----------------------------------------------------------

*--Getting Characteristic Value based on Batch and Material
    LOOP AT lt_mchb ASSIGNING <fs_mchb>.
      REFRESH lt_char_batch.
      CALL FUNCTION 'VB_BATCH_GET_DETAIL'
        EXPORTING
          matnr              = <fs_mchb>-matnr
          charg              = <fs_mchb>-charg
          get_classification = zcl_abs_abap_maintain=>c_costing_rel_full  "'X'
        TABLES
          char_of_batch      = lt_char_batch.

      CLEAR ls_char_batch.
      READ TABLE lt_char_batch INTO ls_char_batch
      WITH KEY atnam = zcl_abs_abap_maintain=>c_charact_psdat. "'ZLOBM_HSDAT'
      IF sy-subrc = 0.
*--calling FM to convert date format to SAP date format
        CALL FUNCTION 'CONVERSION_EXIT_SDATE_INPUT'
          EXPORTING
            input  = ls_char_batch-atwtb
          IMPORTING
            output = <fs_mchb>-laeda.
      ENDIF.

      CLEAR ls_char_batch.
      READ TABLE lt_char_batch INTO ls_char_batch
      WITH KEY atnam = 'PPMPV_0005'.
      IF sy-subrc = 0.
        <fs_mchb>-grcvr = ls_char_batch-atwtb.
        CLEAR ls_value.
        ls_value = ls_char_batch-atwtb.

        CLEAR ls_char_value.
        ls_char_value-atwrt = ls_char_batch-atwtb.
        ls_char_value-codegruppe = ls_value-codegruppe.
        ls_char_value-code = ls_value-code.
        APPEND ls_char_value TO lt_char_value.
      ENDIF.

      CLEAR ls_char_batch.
      READ TABLE lt_char_batch INTO ls_char_batch
      WITH KEY atnam = 'PPMPV_0002'.
      IF sy-subrc = 0.
        <fs_mchb>-grcgo = ls_char_batch-atwtb.
        CLEAR ls_value.
        ls_value = ls_char_batch-atwtb.

        CLEAR ls_char_value.
        ls_char_value-atwrt = ls_char_batch-atwtb.
        ls_char_value-codegruppe = ls_value-codegruppe.
        ls_char_value-code = ls_value-code.
        APPEND ls_char_value TO lt_char_value.
      ENDIF.
    ENDLOOP.

    SORT lt_char_value BY atwrt.
    DELETE ADJACENT DUPLICATES FROM lt_char_value COMPARING atwrt.

    IF lt_char_value IS NOT INITIAL.
      SELECT katalogart, codegruppe, code, kurztext
        FROM qpct
        INTO TABLE @DATA(lt_qpct)
         FOR ALL ENTRIES IN @lt_char_value
       WHERE codegruppe EQ @lt_char_value-codegruppe
         AND code       EQ @lt_char_value-code
         AND sprache    EQ @sy-langu.
      IF sy-subrc EQ 0.
        SORT lt_qpct BY codegruppe code.
      ENDIF.
    ENDIF.
    SORT lt_mchb BY matnr laeda.
    "--------------------------------------------------------------------------
    REFRESH lt_fpcom.
*    LOOP AT gs_fpdoc_infocus-x-fpcom INTO ls_fpcom
    LOOP AT lt_com INTO ls_fpcom
                                     WHERE zztask IS INITIAL.
**      READ TABLE <gs_fpdoc_infocus>-x-fpcom INTO ls_fpcom_in
      READ TABLE lt_com INTO ls_fpcom_in
        WITH KEY aufnr = ls_fpcom-aufnr
                 posnr = ls_fpcom-posnr
                 matnr = ls_fpcom-matnr.
      IF sy-subrc EQ 0.
        APPEND ls_fpcom_in TO lt_fpcom.
      ENDIF.
    ENDLOOP.

**    CLEAR <gs_fpdoc_infocus>-x-fpcom.
    LOOP AT lt_itm INTO ls_itm.
**    LOOP AT gt_fpcnf_fcat INTO ls_fpcnf_fcat.
      CLEAR lv_contr.

*--Splitting the Consumption Quantity as batch wise
      LOOP AT lt_fpcom INTO ls_fpcom
                       WHERE aufnr EQ ls_itm-aufnr " ls_fpcnf_fcat-aufnr
                         AND posnr EQ ls_itm-posnr. " ls_fpcnf_fcat-posnr.
*        IF ls_fpcom-flgch IS INITIAL.
        IF ls_fpcom-zztask IS NOT INITIAL.
          lv_contr = lv_contr + 1.
          ls_fpcom-contr = lv_contr.
          APPEND ls_fpcom TO lt_com. " <gs_fpdoc_infocus>-x-fpcom.
        ELSE.
**          lv_lmnga = ls_fpcnf_fcat-lmnga.

          READ TABLE lt_mchb ASSIGNING <fs_mchb>
          WITH KEY matnr = ls_fpcom-matnr BINARY SEARCH .
          IF sy-subrc = 0.
            lv_tabix = sy-tabix.
            LOOP AT lt_mchb ASSIGNING <fs_mchb> FROM lv_tabix.
              IF <fs_mchb>-matnr <> ls_fpcom-matnr.
                EXIT.
              ENDIF.
              ls_fpcom-charg = <fs_mchb>-charg.

              CLEAR ls_char_value.
              READ TABLE lt_char_value INTO ls_char_value
                    WITH KEY atwrt = <fs_mchb>-grcvr.
              IF sy-subrc EQ 0.
                READ TABLE lt_qpct INTO DATA(ls_qpct)
                      WITH KEY codegruppe = ls_char_value-codegruppe
                               code       = ls_char_value-code
                    BINARY SEARCH.
                IF sy-subrc EQ 0.
                  ls_fpcom-zzgrcvr = ls_qpct-kurztext.
                ENDIF.
              ENDIF.

              CLEAR ls_char_value.
              READ TABLE lt_char_value INTO ls_char_value
                    WITH KEY atwrt = <fs_mchb>-grcgo.
              IF sy-subrc EQ 0.
                CLEAR ls_qpct.
                READ TABLE lt_qpct INTO ls_qpct
                      WITH KEY codegruppe = ls_char_value-codegruppe
                               code       = ls_char_value-code
                    BINARY SEARCH.
                IF sy-subrc EQ 0.
                  ls_fpcom-zzgrcgo = ls_qpct-kurztext.
                ENDIF.
              ENDIF.

              IF lv_lmnga > <fs_mchb>-clabs.
                ls_fpcom-lmnga = <fs_mchb>-clabs.
                lv_lmnga = lv_lmnga - <fs_mchb>-clabs.
              ELSE.
                ls_fpcom-lmnga = lv_lmnga.
                lv_lmnga = 0.
              ENDIF.
              lv_contr = lv_contr + 1.
              ls_fpcom-contr = lv_contr.
              ls_fpcom-zzclabs = <fs_mchb>-clabs.
              APPEND ls_fpcom TO lt_com. " <gs_fpdoc_infocus>-x-fpcom.
            ENDLOOP.
            IF sy-subrc NE 0.
              lv_contr = lv_contr + 1.
              ls_fpcom-contr = lv_contr.
              APPEND ls_fpcom TO lt_com. " <gs_fpdoc_infocus>-x-fpcom.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

*    DELETE <gs_fpdoc_infocus>-x-fpcom WHERE zzclabs IS INITIAL.
    DELETE lt_com WHERE zzclabs IS INITIAL.
    REFRESH : et_btchcnfm[], et_fmfpcom[].
    MOVE-CORRESPONDING lt_com TO et_btchcnfm.
    et_fmfpcom[] = lt_com[].

  ENDMETHOD.


  METHOD get_characteristics_f4.

*-- Local Declarations
    DATA : lt_filter     TYPE /iwbep/t_mgw_select_option,
           ls_filter     TYPE /iwbep/s_mgw_select_option,
           ls_message    TYPE /agri/s_gprolog,
           lv_filter_str TYPE string,
           lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
           ltr_matnr     TYPE RANGE OF matnr,
           lsr_matnr     LIKE LINE OF ltr_matnr,

           lv_class      TYPE klasse_d, " klah-class,
           lt_char       TYPE STANDARD TABLE OF api_char,

           ls_charstics  TYPE zabs_s_nur_charstics_f4,
           lt_charstics  TYPE zabs_tty_nur_charstics_f4.

    lo_filter     = io_tech_request_context->get_filter( ).
    lt_filter     = lo_filter->get_filter_select_options( ).
    lv_filter_str = lo_filter->get_filter_string( ).

    IF  lv_filter_str IS NOT INITIAL
    AND lt_filter[]   IS INITIAL.
      me->/iwbep/if_sb_dpc_comm_services~log_message(
      EXPORTING
        iv_msg_type   = zcl_abs_abap_maintain=>c_msgty_error "'E'
        iv_msg_id     = '/IWBEP/MC_SB_DPC_ADM'
        iv_msg_number = 025 ).
      " Raise Exception
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
        EXPORTING
          textid = /iwbep/cx_mgw_tech_exception=>internal_error.
    ENDIF.

*--Maps filter table lines to function module parameters
    LOOP AT lt_filter INTO ls_filter.
      CASE ls_filter-property.
        WHEN 'MATNR'. "--Material No.
          lo_filter->convert_select_option(
            EXPORTING
              is_select_option = ls_filter
            IMPORTING
              et_select_option = ltr_matnr ).

        WHEN OTHERS.
          " Log message in the application log
          me->/iwbep/if_sb_dpc_comm_services~log_message(
            EXPORTING
              iv_msg_type   = zcl_abs_abap_maintain=>c_msgty_error "'E'
              iv_msg_id     = '/IWBEP/MC_SB_DPC_ADM'
              iv_msg_number = 020
              iv_msg_v1     = ls_filter-property ).
          " Raise Exception
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
            EXPORTING
              textid = /iwbep/cx_mgw_tech_exception=>internal_error.
      ENDCASE.
    ENDLOOP.

    IF ltr_matnr[] IS NOT INITIAL.
      READ TABLE ltr_matnr INTO lsr_matnr INDEX 1.
      REFRESH lt_char[].
      CALL FUNCTION 'VBWS_MARA_CLASSIFICATION_GET'
        EXPORTING
          i_matnr              = lsr_matnr-low
        IMPORTING
          e_class              = lv_class
        TABLES
          e_cl_char            = lt_char
        EXCEPTIONS
          classtype_not_found  = 1
          classtype_not_active = 2
          class_not_found      = 3
          no_allocations       = 4
          characters_not_found = 5
          OTHERS               = 6.
      IF lt_char[] IS INITIAL.
**        CLEAR : ls_message.
**        ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error.
**        ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
**        ls_message-msgno = .
**        add_messages_to_msg_container( is_message = ls_message ).
**        RETURN.
      ENDIF.
    ENDIF.

    IF lt_char[] IS NOT INITIAL.
      MOVE-CORRESPONDING lt_char TO et_charstics. "lt_charstics.
    ENDIF.

    SELECT atinn, atfor, anzst, anzdz
      FROM cabn
      INTO TABLE @DATA(lt_cabn)
   FOR ALL ENTRIES IN @lt_char
     WHERE atinn EQ @lt_char-atinn.
    IF lt_cabn[] IS NOT INITIAL.
      SORT lt_cabn BY atinn.
    ENDIF.

    IF lv_class IS NOT INITIAL.
      SELECT SINGLE clint, class
             FROM klah INTO @DATA(ls_klah)
            WHERE class EQ @lv_class.
      IF sy-subrc = 0.
        SELECT SINGLE clint, kschl
                 FROM swor INTO @DATA(ls_swor)
                WHERE clint EQ @ls_klah-clint
                  AND spras EQ 'P'. "@SY-LANGU
      ENDIF.
    ENDIF.

**    LOOP AT lt_charstics INTO ls_charstics.
    LOOP AT et_charstics INTO ls_charstics.
      READ TABLE lt_cabn INTO DATA(ls_cabn)
                         WITH KEY atinn = ls_charstics-atinn
                       BINARY SEARCH.
      ls_charstics-matnr = lsr_matnr-low.
      ls_charstics-class = lv_class.
      IF ls_swor-kschl IS NOT INITIAL.
        ls_charstics-kltxt = ls_swor-kschl.
      ENDIF.
      ls_charstics-atfor = ls_cabn-atfor.
      ls_charstics-anzst = ls_cabn-anzst.
      ls_charstics-anzdz = ls_cabn-anzdz.
      ls_charstics-atinn_c = ls_charstics-atinn.
**      MODIFY lt_charstics FROM ls_charstics TRANSPORTING matnr class.
      MODIFY et_charstics FROM ls_charstics
                  TRANSPORTING matnr class atfor anzst anzdz kltxt atinn_c.
      CLEAR ls_cabn.
    ENDLOOP.

**    SELECT atinn, atnam, auswahlmge
**      FROM cabn
**      INTO TABLE @DATA(lt_cabn)
**   FOR ALL ENTRIES IN @lt_char
**     WHERE atinn EQ @lt_char-atinn.
**
**    IF lt_cabn[] IS NOT INITIAL.
**      SORT lt_cabn BY atinn.
**      SELECT codegruppe, code, kurztext
**        FROM qpct
**        INTO TABLE @DATA(lt_qpct)
**     FOR ALL ENTRIES IN @lt_cabn
**       WHERE codegruppe EQ @lt_cabn-auswahlmge.
**      IF lt_qpct[] IS NOT INITIAL.
**        SORT lt_qpct BY codegruppe code.
**      ENDIF.
**    ENDIF.
**
**    LOOP AT lt_charstics INTO ls_charstics.
**      READ TABLE lt_cabn INTO DATA(ls_cabn)
**                         WITH KEY atinn = ls_charstics-atinn
**                         BINARY SEARCH.
**      LOOP AT lt_qpct INTO DATA(ls_qpct)
**                     WHERE codegruppe = ls_cabn-auswahlmge.
**        IF sy-subrc = 0.
**          ls_charstics-value    = ls_qpct-code.
**          ls_charstics-kurztext = ls_qpct-kurztext.
**          APPEND ls_charstics TO et_charstics.
**        ENDIF.
**      ENDLOOP.
**      CLEAR: ls_cabn, ls_charstics.
**    ENDLOOP.


  ENDMETHOD.


  METHOD get_char_value_f4.

*-- Local Declarations
    DATA : lt_filter     TYPE /iwbep/t_mgw_select_option,
           ls_filter     TYPE /iwbep/s_mgw_select_option,
           ls_message    TYPE /agri/s_gprolog,
           lv_filter_str TYPE string,
           lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
           ltr_atinn     TYPE RANGE OF atinn,
           lsr_atinn     LIKE LINE OF ltr_atinn,
           ltr_atnam     TYPE RANGE OF atnam,
           lsr_atnam     LIKE LINE OF ltr_atnam,

           lv_class      TYPE klah-class,
           lt_char       TYPE STANDARD TABLE OF api_char,

           ls_charstics  TYPE zabs_s_nur_charstics_f4.

    lo_filter     = io_tech_request_context->get_filter( ).
    lt_filter     = lo_filter->get_filter_select_options( ).
    lv_filter_str = lo_filter->get_filter_string( ).

    IF  lv_filter_str IS NOT INITIAL
    AND lt_filter[]   IS INITIAL.
      me->/iwbep/if_sb_dpc_comm_services~log_message(
      EXPORTING
        iv_msg_type   = zcl_abs_abap_maintain=>c_msgty_error "'E'
        iv_msg_id     = '/IWBEP/MC_SB_DPC_ADM'
        iv_msg_number = 025 ).
      " Raise Exception
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
        EXPORTING
          textid = /iwbep/cx_mgw_tech_exception=>internal_error.
    ENDIF.

*--Maps filter table lines to function module parameters
    LOOP AT lt_filter INTO ls_filter.
      CASE ls_filter-property.
        WHEN 'ATINN'. "--Internal characteristic
          lo_filter->convert_select_option(
            EXPORTING
              is_select_option = ls_filter
            IMPORTING
              et_select_option = ltr_atinn ).

        WHEN OTHERS.
          " Log message in the application log
          me->/iwbep/if_sb_dpc_comm_services~log_message(
            EXPORTING
              iv_msg_type   = zcl_abs_abap_maintain=>c_msgty_error "'E'
              iv_msg_id     = '/IWBEP/MC_SB_DPC_ADM'
              iv_msg_number = 020
              iv_msg_v1     = ls_filter-property ).
          " Raise Exception
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
            EXPORTING
              textid = /iwbep/cx_mgw_tech_exception=>internal_error.
      ENDCASE.
    ENDLOOP.

    IF ltr_atinn[] IS NOT INITIAL.
      SELECT atinn, atnam, auswahlmge
        FROM cabn
        INTO TABLE @DATA(lt_cabn)
     FOR ALL ENTRIES IN @ltr_atinn
       WHERE atinn EQ @ltr_atinn-low. " atnam EQ @ltr_atnam-low.
    ENDIF.

    IF lt_cabn[] IS NOT INITIAL.
      SELECT codegruppe, code, kurztext
        FROM qpct
        INTO TABLE @DATA(lt_qpct)
     FOR ALL ENTRIES IN @lt_cabn
       WHERE codegruppe EQ @lt_cabn-auswahlmge
         AND sprache    EQ 'P'. "@sy-langu.
      IF lt_qpct[] IS NOT INITIAL.
        SORT lt_qpct BY codegruppe code.
      ENDIF.
    ENDIF.

    LOOP AT lt_cabn INTO DATA(ls_cabn).
      LOOP AT lt_qpct INTO DATA(ls_qpct)
                     WHERE codegruppe = ls_cabn-auswahlmge.
        IF sy-subrc = 0.
          ls_charstics-atinn    = ls_cabn-atinn.
          ls_charstics-atnam    = ls_cabn-atnam.
*          ls_charstics-value    = ls_qpct-code.
          CONCATENATE ls_cabn-auswahlmge ls_qpct-code
                 INTO ls_charstics-value SEPARATED BY space.
          ls_charstics-kurztext = ls_qpct-kurztext.
          APPEND ls_charstics TO et_charvalue.
        ENDIF.
      ENDLOOP.
      CLEAR: ls_cabn, ls_charstics.
    ENDLOOP.


  ENDMETHOD.


  METHOD get_delta_token.
*-- LOCAL declarations
    DATA :
      lt_entityset   TYPE REF TO data,
      lv_delta_token TYPE string,
      lv_format      TYPE string,
      lo_dp_facade   TYPE REF TO /iwbep/if_mgw_dp_facade,
      lo_dp_facade_1 TYPE REF TO /iwbep/if_mgw_dp_fw_facade.

    TRY.
        lo_dp_facade = /iwbep/if_mgw_conv_srv_runtime~get_dp_facade( ).
      CATCH /iwbep/cx_mgw_tech_exception.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception.
    ENDTRY.

*-- call the delta token functionality
    IF lo_dp_facade IS BOUND.
      lo_dp_facade_1 ?= lo_dp_facade.

*-- getting the format type
*-- get delta token only for XML format
      CALL METHOD lo_dp_facade_1->get_format
        RECEIVING
          rv_format = lv_format.

*-- Passing all the data of entityset and get the delta token generated
      IF lv_format NE 'json'.
        TRY.
            CALL METHOD /iwbep/cl_query_result_log=>create_update_log_entry_hash
              EXPORTING
                io_tech_request_context  = io_tech_request_context
                io_dp_facade             = lo_dp_facade
                ir_service_document_name = mr_service_document_name
                ir_service_version       = mr_service_version
                it_entityset             = ct_entityset
              CHANGING
                ev_delta_token           = lv_delta_token.

          CATCH /iwbep/cx_qrl_locked.
            RAISE EXCEPTION TYPE /iwbep/cx_qrl_locked.
          CATCH /iwbep/cx_qrl_delta_unavailabl.
            RAISE EXCEPTION TYPE /iwbep/cx_qrl_delta_unavailabl.
        ENDTRY.

*-- Exporting the delta token
        es_response_context-deltatoken = lv_delta_token.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_processord.
*-- Local Declarations
    DATA : lt_filter     TYPE /iwbep/t_mgw_select_option,
           ls_filter     TYPE /iwbep/s_mgw_select_option,
           ls_message    TYPE /agri/s_gprolog,
           lv_filter_str TYPE string,
           lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
           ltr_tplnr     TYPE RANGE OF /agri/gltplnr_fl,
           lsr_tplnr     LIKE LINE OF ltr_tplnr,
           lv_tplnr      TYPE /agri/gltplnr_fl,
           ltr_matnr     TYPE RANGE OF /agri/glmatnr, "Process Material
           lsr_matnr     LIKE LINE OF ltr_matnr,

           ls_processord TYPE zabs_s_nur_route_terrain.

    lo_filter     = io_tech_request_context->get_filter( ).
    lt_filter     = lo_filter->get_filter_select_options( ).
    lv_filter_str = lo_filter->get_filter_string( ).

    IF  lv_filter_str IS NOT INITIAL
    AND lt_filter[]   IS INITIAL.
      me->/iwbep/if_sb_dpc_comm_services~log_message(
      EXPORTING
        iv_msg_type   = zcl_abs_abap_maintain=>c_msgty_error "'E'
        iv_msg_id     = '/IWBEP/MC_SB_DPC_ADM'
        iv_msg_number = 025 ).
      " Raise Exception
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
        EXPORTING
          textid = /iwbep/cx_mgw_tech_exception=>internal_error.
    ENDIF.

*--Maps filter table lines to function module parameters
    LOOP AT lt_filter INTO ls_filter.
      CASE ls_filter-property.
        WHEN 'TPLNR_FL'. "--Terrain
          lo_filter->convert_select_option(
            EXPORTING
              is_select_option = ls_filter
            IMPORTING
              et_select_option = ltr_tplnr ).

        WHEN 'MATNR'. "--Process Material
          lo_filter->convert_select_option(
            EXPORTING
              is_select_option = ls_filter
            IMPORTING
              et_select_option = ltr_matnr ).

        WHEN OTHERS.
          " Log message in the application log
          me->/iwbep/if_sb_dpc_comm_services~log_message(
            EXPORTING
              iv_msg_type   = zcl_abs_abap_maintain=>c_msgty_error "'E'
              iv_msg_id     = '/IWBEP/MC_SB_DPC_ADM'
              iv_msg_number = 020
              iv_msg_v1     = ls_filter-property ).
          " Raise Exception
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
            EXPORTING
              textid = /iwbep/cx_mgw_tech_exception=>internal_error.
      ENDCASE.
    ENDLOOP.

    IF ltr_tplnr[] IS NOT INITIAL.
      READ TABLE ltr_tplnr INTO lsr_tplnr INDEX 1.
      IF sy-subrc = 0.
        lv_tplnr = lsr_tplnr-low.
      ENDIF.
      SELECT tplnr_fl, pltxt
        FROM /agri/glflot
  INTO TABLE @DATA(lt_glflot)
       WHERE tplnr_fl   = @lv_tplnr " IN @ltr_tplnr
         AND kfrst      = @space
         AND loevm      = @space. "abap_false.

      IF sy-subrc = 0.
        SORT lt_glflot BY tplnr_fl.
        SELECT tplnr_fl,
               contr,
               cmnum,
               season,
               datab,
               datbi,
               aarea,
               msehi,
               exhad,
               eston,
               esuom,
               ernam,
               erdat,
               erzet,
               aenam,
               aedat,
               aezet
         FROM /agri/glflcma
               INTO TABLE @DATA(lt_flcma)
         FOR ALL ENTRIES IN @lt_glflot
              WHERE tplnr_fl = @lt_glflot-tplnr_fl
                AND loevm  = @space.

      ENDIF. "lt_glflot
    ENDIF. " lt_tplnr

    IF lt_flcma[] IS NOT INITIAL.
      SORT lt_flcma.
      READ TABLE ltr_matnr INTO lsr_matnr INDEX 1.
      SELECT *
        FROM /agri/fmfphdr
        INTO TABLE @DATA(lt_fphdr)
     FOR ALL ENTRIES IN @lt_flcma
       WHERE contr    EQ @lt_flcma-contr
         AND cmnum    EQ @lt_flcma-cmnum
         AND matnr    EQ @lsr_matnr-low
         AND tplnr_fl EQ @lv_tplnr
         AND autyp    EQ 'AO'
         AND class    EQ '0'
         AND tecom    EQ @space.
      IF sy-subrc = 0.
        SORT lt_fphdr.
        DATA(lt_fphdr_1) = lt_fphdr.
        SORT lt_fphdr_1 BY matnr.
        DELETE ADJACENT DUPLICATES FROM lt_fphdr_1.
        IF lt_fphdr_1[] IS NOT INITIAL.
          SELECT matnr, maktx
            FROM makt INTO TABLE @DATA(lt_makt)
         FOR ALL ENTRIES IN @lt_fphdr_1
           WHERE matnr EQ @lt_fphdr_1-matnr.
          IF sy-subrc = 0.
            SORT lt_makt BY matnr.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    REFRESH et_processord[].

*        READ TABLE lt_glflot INTO DATA(ls_glflot)
*                             WITH KEY tplnr_fl = ls_rtfla-tplnr_fl
*                           BINARY SEARCH.
    LOOP AT lt_glflot INTO DATA(ls_glflot).
      LOOP AT lt_flcma INTO DATA(ls_flcma)
                      WHERE tplnr_fl = ls_glflot-tplnr_fl.
        LOOP AT lt_fphdr INTO DATA(ls_fphdr)
                        WHERE contr = ls_flcma-contr
                          AND cmnum = ls_flcma-cmnum.
          READ TABLE lt_makt INTO DATA(ls_makt)
                             WITH KEY matnr = ls_fphdr-matnr.

          ls_processord-tplnr_fl = ls_fphdr-tplnr_fl.
          ls_processord-pltxt    = ls_glflot-pltxt.
          ls_processord-contr    = ls_fphdr-contr.
          ls_processord-cmnum    = ls_fphdr-cmnum.
          ls_processord-aufnr    = ls_fphdr-aufnr.
          ls_processord-matnr    = ls_fphdr-matnr.
          ls_processord-maktx    = ls_makt-maktx.
          ls_processord-gamng    = ls_fphdr-gamng.
          ls_processord-pndqty   = ls_fphdr-gamng - ls_fphdr-gwemg.
          ls_processord-lower_dt =  sy-datum.
          ls_processord-upper_dt =  sy-datum.
          APPEND ls_processord TO et_processord.
          CLEAR: ls_processord, ls_fphdr, ls_makt.
        ENDLOOP.
      ENDLOOP.
      CLEAR: ls_flcma, ls_glflot.
    ENDLOOP.

    IF et_processord[] IS NOT INITIAL.
      SORT et_processord BY aufnr contr.
      DELETE ADJACENT DUPLICATES FROM et_processord.
    ENDIF.

  ENDMETHOD.


  METHOD get_terrain_f4.

*-- Local Declarations
    DATA: lt_filter     TYPE /iwbep/t_mgw_select_option,
          ls_filter     TYPE /iwbep/s_mgw_select_option,
          lv_filter_str TYPE string,
          lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
          ltr_badge     TYPE RANGE OF persno,
          lsr_badge     LIKE LINE OF ltr_badge,
          ls_message    TYPE /agri/s_gprolog,
          ls_terrain_f4 TYPE zabs_s_nur_audit_trn_f4.

    lo_filter     = io_tech_request_context->get_filter( ).
    lt_filter     = lo_filter->get_filter_select_options( ).
    lv_filter_str = lo_filter->get_filter_string( ).

    IF  lv_filter_str IS NOT INITIAL
    AND lt_filter[]   IS INITIAL.
      me->/iwbep/if_sb_dpc_comm_services~log_message(
      EXPORTING
        iv_msg_type   = zcl_abs_abap_maintain=>c_msgty_error "'E'
        iv_msg_id     = '/IWBEP/MC_SB_DPC_ADM'
        iv_msg_number = 025 ).
      " Raise Exception
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
        EXPORTING
          textid = /iwbep/cx_mgw_tech_exception=>internal_error.
    ENDIF.

*--Maps filter table lines to function module parameters
    LOOP AT lt_filter INTO ls_filter.
      CASE ls_filter-property.
        WHEN 'PERNR'. "--BADGE
          lo_filter->convert_select_option(
            EXPORTING
              is_select_option = ls_filter
            IMPORTING
              et_select_option = ltr_badge ).

        WHEN OTHERS.
          " Log message in the application log
          me->/iwbep/if_sb_dpc_comm_services~log_message(
            EXPORTING
              iv_msg_type   = zcl_abs_abap_maintain=>c_msgty_error "'E'
              iv_msg_id     = '/IWBEP/MC_SB_DPC_ADM'
              iv_msg_number = 020
              iv_msg_v1     = ls_filter-property ).
          " Raise Exception
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
            EXPORTING
              textid = /iwbep/cx_mgw_tech_exception=>internal_error.
      ENDCASE.
    ENDLOOP.

    IF ltr_badge[] IS NOT INITIAL.
      SELECT *
      FROM zabs_usrpernr
      INTO TABLE @DATA(lt_rtusr)
      WHERE pernr IN @ltr_badge.
      IF sy-subrc IS NOT INITIAL.
        CLEAR ls_message.
        ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error.
        ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
        ls_message-msgno = '163'.
        add_messages_to_msg_container( is_message   = ls_message ).
        CLEAR ls_message.
      ENDIF.
    ENDIF.

    IF lt_rtusr[] IS NOT INITIAL.
      SORT lt_rtusr BY route.
      SELECT *
        FROM /agri/glrthdrt
        INTO TABLE @DATA(lt_hdrt)
     FOR ALL ENTRIES IN @lt_rtusr
       WHERE route EQ @lt_rtusr-route
         AND spras EQ @sy-langu.

      IF lt_hdrt[] IS NOT INITIAL.
        SORT lt_hdrt BY route.
      ENDIF.

      SELECT *
        FROM /agri/glrtfla
        INTO TABLE @DATA(lt_rtfla)
     FOR ALL ENTRIES IN @lt_rtusr                  "#EC CI_NO_TRANSFORM
       WHERE route  = @lt_rtusr-route.
    ENDIF.

    IF lt_rtfla[] IS NOT INITIAL.
      SORT lt_rtfla.
      SELECT tplnr_fl, pltxt
        FROM /agri/glflot
  INTO TABLE @DATA(lt_glflot)
     FOR ALL ENTRIES IN @lt_rtfla
       WHERE tplnr_fl   = @lt_rtfla-tplnr_fl
         AND kfrst      = @space
         AND loevm      = @space. "abap_false.

      IF sy-subrc = 0.
        READ TABLE ltr_badge INTO lsr_badge INDEX 1.
        LOOP AT lt_glflot INTO DATA(ls_glflot).
          ls_terrain_f4-pernr    = lsr_badge-low.
          ls_terrain_f4-tplnr_fl = ls_glflot-tplnr_fl.
          ls_terrain_f4-pltxt    = ls_glflot-pltxt.
          APPEND ls_terrain_f4 TO et_terrain_f4.

          CLEAR ls_terrain_f4.
        ENDLOOP.
*        et_terrain_f4[] = lt_glflot[].
*
*        LOOP AT et_terrain_f4 INTO ls_terrain_f4.
*          ls_terrain_f4-pernr = lsr_badge-low.
*          MODIFY et_terrain_f4 FROM ls_terrain_f4 TRANSPORTING pernr.
*          CLEAR ls_terrain_f4.
*        ENDLOOP.

      ELSEIF sy-subrc IS NOT INITIAL.
        CLEAR ls_message.
        ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error.
        ls_message-msgid = zcl_abs_abap_maintain=>c_msg_class_/agri/fmfp.
        ls_message-msgno = '000'.
        add_messages_to_msg_container( is_message   = ls_message ).
        CLEAR ls_message.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD nurauditreportse_get_entityset.

    CALL METHOD me->get_audit_report
      EXPORTING
        io_tech_request_context = io_tech_request_context
      IMPORTING
        et_audit_report         = et_entityset.

*** Inlinecount
    IF io_tech_request_context->has_inlinecount( ) = abap_true.
      DESCRIBE TABLE et_entityset LINES es_response_context-inlinecount.
    ELSE.
      CLEAR es_response_context-inlinecount.
    ENDIF.

*--------------------------------------------------------------------*
*----------- Delta Token Functionality    ---------------------------*
*--------------------------------------------------------------------*
    CALL METHOD me->get_delta_token
      EXPORTING
        iv_entity_set_name      = iv_entity_set_name
        io_tech_request_context = io_tech_request_context
      IMPORTING
        es_response_context     = es_response_context
      CHANGING
        ct_entityset            = et_entityset.

  ENDMETHOD.


  METHOD nurbadgerouteset_get_entityset.

    CALL METHOD me->get_badge_route
      EXPORTING
        io_tech_request_context = io_tech_request_context
      IMPORTING
        et_badge_route          = et_entityset.

*** Inlinecount
    IF io_tech_request_context->has_inlinecount( ) = abap_true.
      DESCRIBE TABLE et_entityset LINES es_response_context-inlinecount.
    ELSE.
      CLEAR es_response_context-inlinecount.
    ENDIF.

*--------------------------------------------------------------------*
*----------- Delta Token Functionality    ---------------------------*
*--------------------------------------------------------------------*
    CALL METHOD me->get_delta_token
      EXPORTING
        iv_entity_set_name      = iv_entity_set_name
        io_tech_request_context = io_tech_request_context
      IMPORTING
        es_response_context     = es_response_context
      CHANGING
        ct_entityset            = et_entityset.

  ENDMETHOD.


  METHOD nurbtchcnfmf4set_get_entityset.

    CALL METHOD me->get_btchcnfm_f4
      EXPORTING
        io_tech_request_context = io_tech_request_context
      IMPORTING
        et_btchcnfm             = et_entityset.

*** Inlinecount
    IF io_tech_request_context->has_inlinecount( ) = abap_true.
      DESCRIBE TABLE et_entityset LINES es_response_context-inlinecount.
    ELSE.
      CLEAR es_response_context-inlinecount.
    ENDIF.

*--------------------------------------------------------------------*
*----------- Delta Token Functionality    ---------------------------*
*--------------------------------------------------------------------*
    CALL METHOD me->get_delta_token
      EXPORTING
        iv_entity_set_name      = iv_entity_set_name
        io_tech_request_context = io_tech_request_context
      IMPORTING
        es_response_context     = es_response_context
      CHANGING
        ct_entityset            = et_entityset.

  ENDMETHOD.


  METHOD nurcharsticsf4se_get_entityset.

    CALL METHOD me->get_characteristics_f4
      EXPORTING
        io_tech_request_context = io_tech_request_context
      IMPORTING
        et_charstics            = et_entityset.

*** Inlinecount
    IF io_tech_request_context->has_inlinecount( ) = abap_true.
      DESCRIBE TABLE et_entityset LINES es_response_context-inlinecount.
    ELSE.
      CLEAR es_response_context-inlinecount.
    ENDIF.

*--------------------------------------------------------------------*
*----------- Delta Token Functionality    ---------------------------*
*--------------------------------------------------------------------*
    CALL METHOD me->get_delta_token
      EXPORTING
        iv_entity_set_name      = iv_entity_set_name
        io_tech_request_context = io_tech_request_context
      IMPORTING
        es_response_context     = es_response_context
      CHANGING
        ct_entityset            = et_entityset.

  ENDMETHOD.


  METHOD nurcharvaluef4se_get_entityset.

    CALL METHOD me->get_char_value_f4
      EXPORTING
        io_tech_request_context = io_tech_request_context
      IMPORTING
        et_charvalue            = et_entityset.

*** Inlinecount
    IF io_tech_request_context->has_inlinecount( ) = abap_true.
      DESCRIBE TABLE et_entityset LINES es_response_context-inlinecount.
    ELSE.
      CLEAR es_response_context-inlinecount.
    ENDIF.

*--------------------------------------------------------------------*
*----------- Delta Token Functionality    ---------------------------*
*--------------------------------------------------------------------*
    CALL METHOD me->get_delta_token
      EXPORTING
        iv_entity_set_name      = iv_entity_set_name
        io_tech_request_context = io_tech_request_context
      IMPORTING
        es_response_context     = es_response_context
      CHANGING
        ct_entityset            = et_entityset.

  ENDMETHOD.


  METHOD nurprocessordset_get_entityset.

    CALL METHOD me->get_processord
      EXPORTING
        io_tech_request_context = io_tech_request_context
      IMPORTING
        et_processord           = et_entityset.

*** Inlinecount
    IF io_tech_request_context->has_inlinecount( ) = abap_true.
      DESCRIBE TABLE et_entityset LINES es_response_context-inlinecount.
    ELSE.
      CLEAR es_response_context-inlinecount.
    ENDIF.

*--------------------------------------------------------------------*
*----------- Delta Token Functionality    ---------------------------*
*--------------------------------------------------------------------*
    CALL METHOD me->get_delta_token
      EXPORTING
        iv_entity_set_name      = iv_entity_set_name
        io_tech_request_context = io_tech_request_context
      IMPORTING
        es_response_context     = es_response_context
      CHANGING
        ct_entityset            = et_entityset.

  ENDMETHOD.


  METHOD nurterrainf4set_get_entityset.

    CALL METHOD me->get_terrain_f4
      EXPORTING
        io_tech_request_context = io_tech_request_context
      IMPORTING
        et_terrain_f4           = et_entityset.

*** Inlinecount
    IF io_tech_request_context->has_inlinecount( ) = abap_true.
      DESCRIBE TABLE et_entityset LINES es_response_context-inlinecount.
    ELSE.
      CLEAR es_response_context-inlinecount.
    ENDIF.

*--------------------------------------------------------------------*
*----------- Delta Token Functionality    ---------------------------*
*--------------------------------------------------------------------*
    CALL METHOD me->get_delta_token
      EXPORTING
        iv_entity_set_name      = iv_entity_set_name
        io_tech_request_context = io_tech_request_context
      IMPORTING
        es_response_context     = es_response_context
      CHANGING
        ct_entityset            = et_entityset.

  ENDMETHOD.


  METHOD uomf4set_get_entityset.

    DATA: lt_badge     TYPE zcl_zabs_mob_nursery_mpc=>tt_nurbadgeroute,
          ls_entityset LIKE LINE OF et_entityset.
    CALL METHOD me->get_badge_route
      EXPORTING
        io_tech_request_context = io_tech_request_context
      IMPORTING
        et_badge_route          = lt_badge.

    SORT lt_badge BY pernr matnr.
    DELETE ADJACENT DUPLICATES FROM lt_badge COMPARING pernr matnr.

    SELECT matnr, meinh
      FROM marm
      INTO TABLE @DATA(tl_marm)
      FOR ALL ENTRIES IN @lt_badge
      WHERE matnr EQ @lt_badge-matnr.

    LOOP AT tl_marm INTO DATA(ls_marm).
      ls_entityset-matnr = ls_marm-matnr.
      ls_entityset-msehi = ls_marm-meinh.
      READ TABLE lt_badge INTO DATA(ls_badge) WITH KEY matnr = ls_marm-matnr.
      IF sy-subrc EQ 0.
        ls_entityset-pernr = ls_badge-pernr.
      ENDIF.
      APPEND ls_entityset TO et_entityset.
      CLEAR:ls_marm,ls_badge,ls_entityset.
    ENDLOOP.

  ENDMETHOD.


  METHOD update_characteristics.

    DATA: lv_rmclf        TYPE rmclf,
          lt_char         TYPE STANDARD TABLE OF api_char,
          lv_class        TYPE klah-class,
          gt_batch_char_1 TYPE STANDARD TABLE OF zabst_btchchr,
          gt_batch_char   TYPE STANDARD TABLE OF zabst_btchchr,
          gt_char         TYPE STANDARD TABLE OF api_char,
          gv_classnum     TYPE bapi1003_key-classnum,
          ls_batch_char   TYPE zabst_btchchr,
          BEGIN OF gs_variables,
            data_change,
            data_saved,
            refresh_batch_char,
            refresh_colum_batch_char,
            overview_mode,
            aufnr                    TYPE aufnr,
          END OF gs_variables.

*--Local data declaration
    DATA: lrt_codegruppe TYPE RANGE OF qcodegrp,
          lrt_code       TYPE RANGE OF qcode,
          lv_codegruppe  TYPE string, "qcodegrp, "string,
          lv_code        TYPE string. "qcode. "string.

    TYPES: tty_batch_char TYPE STANDARD TABLE OF zabst_btchchr.

*--Local data declaration
    DATA: lt_batch_char_update TYPE tty_batch_char, " zabst_btchchr,
          lt_batch_char_insert TYPE tty_batch_char, " zabst_btchchr,
          lt_allocvaluescurr   TYPE tt_bapi1003_alloc_values_curr,
          lt_allocvaluesnum    TYPE tt_bapi1003_alloc_values_num,
          lt_allocvalueschar   TYPE tt_bapi1003_alloc_values_char,
          ls_allocvaluesnum    TYPE bapi1003_alloc_values_num,
          ls_allocvalueschar   TYPE bapi1003_alloc_values_char,
          ls_allocvaluescurr   TYPE bapi1003_alloc_values_curr,
          lt_return_x          TYPE bapiret2_tab,
          ls_message           TYPE /agri/s_gprolog.

*--Local Variables
    DATA : lv_objectkey TYPE bapi1003_key-object_long,
           lv_classnum  TYPE bapi1003_key-classnum,
           lv_float     TYPE cawn-atflv,
           lv_atwrt     TYPE cawn-atwrt,
           lv_matnr     TYPE matnr.

    FIELD-SYMBOLS: <lv_matnr> TYPE matnr.

****Message Types
    CONSTANTS : BEGIN OF c_msg_type,
                  info    LIKE sy-msgty VALUE 'I',
                  warning LIKE sy-msgty VALUE 'W',
                  error   LIKE sy-msgty VALUE 'E',
                  abend   LIKE sy-msgty VALUE 'A',
                  success LIKE sy-msgty VALUE 'S',
                  x       LIKE sy-msgty VALUE 'X',
                END   OF c_msg_type.

    SELECT *
       FROM zabst_btchchr
       INTO TABLE @DATA(lt_clschar)
       WHERE aufnr EQ @is_batch_crt-aufnr " @ls_fpbch-aufnr
         AND contr EQ @is_batch_crt-contr " @ls_fpbch-contr
         AND batch EQ @is_batch_crt-charg. " @ls_fpbch-charg.
    IF sy-subrc EQ 0.
      SORT lt_clschar BY atinn.
      READ TABLE lt_clschar INTO DATA(ls_clschar) INDEX 1.
      IF sy-subrc = 0.
        lv_rmclf-class = ls_clschar-class.
        lv_rmclf-kltxt = ls_clschar-kltxt.
      ENDIF.
*    IF iv_overview_mode EQ zcl_abs_abap_maintain=>c_mode_display.
*      gt_batch_char_1 = lt_clschar.
*      gs_variables-overview_mode = abap_true.
*    ELSE.
*      CLEAR gs_variables-overview_mode.
*    ENDIF.
*  ELSE.
*    IF iv_overview_mode EQ zcl_abs_abap_maintain=>c_mode_display.
*      MESSAGE i023(zabs_msgcls) WITH is_batch_crt-charg.
*      RETURN.
*    ENDIF.
    ENDIF.

**    IF iv_overview_mode NE zcl_abs_abap_maintain=>c_mode_display.
*--Get batch characteristics based on process material
    CALL FUNCTION 'VBWS_MARA_CLASSIFICATION_GET'
      EXPORTING
        i_matnr              = is_batch_crt-matnr " is_fpdoc_infocus-x-fphdr-matnr
      IMPORTING
        e_class              = lv_class
      TABLES
        e_cl_char            = lt_char
      EXCEPTIONS
        classtype_not_found  = 1
        classtype_not_active = 2
        class_not_found      = 3
        no_allocations       = 4
        characters_not_found = 5
        OTHERS               = 6.

    IF sy-subrc NE 0.
*      MESSAGE i027(zabs_msgcls).
      RETURN.
    ELSE.
      gt_char[] = lt_char[].
      gv_classnum = lv_class.
    ENDIF.

    IF lt_clschar[] IS NOT INITIAL.
*--Process batch characteristics to build and display characteristics
      LOOP AT lt_char INTO DATA(ls_char_temp).
        CLEAR ls_clschar.
        READ TABLE lt_clschar INTO ls_clschar
          WITH KEY atinn = ls_char_temp-atinn.
        IF sy-subrc = 0.
          APPEND ls_clschar TO gt_batch_char_1.
        ELSE.
          ls_clschar-aufnr = is_batch_crt-aufnr.
          ls_clschar-contr = is_batch_crt-contr.
          ls_clschar-batch = is_batch_crt-charg.
          ls_clschar-class = lv_rmclf-class.
          ls_clschar-kltxt = lv_rmclf-kltxt.
          ls_clschar-atinn = ls_char_temp-atinn.
          ls_clschar-atnam = ls_char_temp-atnam.
          ls_clschar-atbez = ls_char_temp-atbez.
          APPEND ls_clschar TO gt_batch_char_1.
        ENDIF.
      ENDLOOP.
    ELSE.
      SELECT SINGLE clint
       FROM klah
       INTO @DATA(lv_clint)
       WHERE class = @lv_class.
      IF sy-subrc = 0.
        lv_rmclf-class = lv_class.
        SELECT SINGLE kschl
          FROM swor
          INTO lv_rmclf-kltxt
          WHERE clint = lv_clint
            AND spras = sy-langu.
        IF sy-subrc <> 0.
          CLEAR lv_rmclf-kltxt.
        ENDIF.
      ENDIF.

      ls_batch_char-aufnr = is_batch_crt-aufnr.
      ls_batch_char-contr = is_batch_crt-contr.
      ls_batch_char-batch = is_batch_crt-charg.
      ls_batch_char-class = lv_rmclf-class.
      ls_batch_char-kltxt = lv_rmclf-kltxt.
      LOOP AT lt_char INTO DATA(ls_char).
        ls_batch_char-atinn = ls_char-atinn.
        ls_batch_char-atnam = ls_char-atnam.
        ls_batch_char-atbez = ls_char-atbez.
        APPEND ls_batch_char TO gt_batch_char_1.
      ENDLOOP.
    ENDIF.
**    ENDIF.

    DELETE gt_batch_char_1 WHERE atnam = zcl_abs_abap_maintain=>c_charact_psdat.
    CLEAR ls_batch_char.
    LOOP AT it_char_upd INTO DATA(ls_char_upd) WHERE value IS NOT INITIAL.
      READ TABLE gt_batch_char_1 INTO ls_batch_char
                           WITH KEY atinn = ls_char_upd-atinn
                                    atnam = ls_char_upd-atnam.
      ls_batch_char-value = ls_char_upd-value.
*      ls_batch_char-      = ls_char_upd-. " text

      APPEND ls_batch_char TO gt_batch_char.
      CLEAR ls_batch_char.
    ENDLOOP.

    "---------------------------Start of Value text(description)

    IF gt_batch_char[] IS NOT INITIAL.
      SELECT atinn, atnam, auswahlmge
        FROM cabn
        INTO TABLE @DATA(lt_cabn)
        FOR ALL ENTRIES IN @gt_batch_char
       WHERE atinn = @gt_batch_char-atinn.

      IF sy-subrc EQ 0.
        DELETE lt_cabn WHERE auswahlmge IS INITIAL.
        IF lt_cabn IS NOT INITIAL.
          LOOP AT lt_cabn INTO DATA(lwa_cabn).
            READ TABLE gt_batch_char ASSIGNING FIELD-SYMBOL(<lwa_batch_char>)
              WITH KEY atnam = lwa_cabn-atnam.
            IF sy-subrc EQ 0.
              CLEAR: lv_codegruppe, lv_code.
              DATA(lv_value) = <lwa_batch_char>-value.
              SPLIT lv_value AT space INTO lv_codegruppe lv_code.
              CONDENSE: lv_codegruppe, lv_code.
              IF lv_codegruppe IS NOT INITIAL.
                INSERT INITIAL LINE INTO TABLE lrt_codegruppe
                  ASSIGNING FIELD-SYMBOL(<lrs_codegruppe>).
                IF sy-subrc EQ 0.
                  <lrs_codegruppe> = 'IEQ'.
                  <lrs_codegruppe>-low = lv_codegruppe.
                ENDIF.
              ENDIF.
              IF lv_code IS NOT INITIAL.
                INSERT INITIAL LINE INTO TABLE lrt_code
                  ASSIGNING FIELD-SYMBOL(<lrs_code>).
                IF sy-subrc EQ 0.
                  <lrs_code> = 'IEQ'.
                  <lrs_code>-low = lv_code.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.
        DATA(lv_katalogart) = '1'.
        SELECT * FROM qpct
          INTO TABLE @DATA(lt_qpct)
         WHERE katalogart EQ @lv_katalogart
           AND codegruppe IN @lrt_codegruppe[]
           AND code       IN @lrt_code[]
           AND sprache    EQ @sy-langu.

        SORT lt_qpct BY codegruppe ASCENDING
                        code       ASCENDING
                        version    DESCENDING.

        LOOP AT lt_cabn INTO lwa_cabn.
          READ TABLE gt_batch_char ASSIGNING <lwa_batch_char>
            WITH KEY atnam = lwa_cabn-atnam.
          IF sy-subrc EQ 0.
            CLEAR: lv_codegruppe, lv_code.
            lv_value = <lwa_batch_char>-value.
            SPLIT lv_value AT space INTO lv_codegruppe lv_code.
            CONDENSE: lv_codegruppe, lv_code.
            READ TABLE lt_qpct INTO DATA(ls_qpct)
              WITH KEY codegruppe = lv_codegruppe
                       code       = lv_code BINARY SEARCH.
            IF sy-subrc EQ 0.
              <lwa_batch_char>-kurztext = ls_qpct-kurztext.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
    "---------------------------End of Value text(description)

    "----------------------------Start of Save Characteristics value

*--Get batch characteristics data from custom table
*  to check if data exists
    SELECT * FROM zabst_btchchr
      INTO TABLE @DATA(lt_btchchr)
      FOR ALL ENTRIES IN @gt_batch_char
     WHERE aufnr = @gt_batch_char-aufnr
       AND contr = @gt_batch_char-contr
       AND batch = @gt_batch_char-batch
       AND class = @gt_batch_char-class
       AND atinn = @gt_batch_char-atinn.
    IF sy-subrc = 0.
      SORT lt_btchchr BY aufnr contr batch class atinn.
    ENDIF.

    REFRESH: lt_batch_char_update, lt_batch_char_insert.
*--Processing data to update/insert data in custom database table.
    LOOP AT gt_batch_char ASSIGNING FIELD-SYMBOL(<ls_batch_char>).
      READ TABLE lt_btchchr INTO DATA(ls_btchchr)
      WITH KEY aufnr = <ls_batch_char>-aufnr
               contr = <ls_batch_char>-contr
               batch = <ls_batch_char>-batch
               class = <ls_batch_char>-class
               atinn = <ls_batch_char>-atinn
               BINARY SEARCH.
      IF sy-subrc = 0.
        IF <ls_batch_char>-value NE ls_btchchr-value.
          <ls_batch_char>-aenam = sy-uname.
          <ls_batch_char>-aedat = sy-datum.
          <ls_batch_char>-aezet = sy-uzeit.
          APPEND <ls_batch_char> TO lt_batch_char_update.
        ENDIF.
      ELSE.
        <ls_batch_char>-ernam = sy-uname.
        <ls_batch_char>-erdat = sy-datum.
        <ls_batch_char>-erzet = sy-uzeit.
        APPEND <ls_batch_char> TO lt_batch_char_insert.
      ENDIF.
    ENDLOOP.

    IF lt_batch_char_update IS NOT INITIAL.
      UPDATE zabst_btchchr FROM TABLE lt_batch_char_update.
    ENDIF.

    IF lt_batch_char_insert IS NOT INITIAL.
      INSERT zabst_btchchr FROM TABLE lt_batch_char_insert.
    ENDIF.

    IF gt_char[] IS NOT INITIAL.
      SELECT atinn, adzhl, atnam,
             atidn, atfor, auswahlmge
        FROM cabn
        INTO TABLE @DATA(lt_cabn_1)
        FOR ALL ENTRIES IN @gt_char
       WHERE atinn = @gt_char-atinn.

      IF sy-subrc EQ 0.
        SORT lt_cabn_1 BY atnam.
      ENDIF.

      DATA(lt_batch_char) = gt_batch_char.
      SORT lt_batch_char BY atnam.
      LOOP AT gt_char INTO DATA(ls_char_1).
        READ TABLE lt_cabn_1 INTO DATA(ls_cabn_1)
          WITH KEY atnam = ls_char_1-atnam BINARY SEARCH.
        IF sy-subrc NE 0.
          CONTINUE.
        ENDIF.

        READ TABLE lt_batch_char INTO DATA(ls_batch_char_1)
          WITH KEY atnam = ls_char_1-atnam BINARY SEARCH.
        IF sy-subrc EQ 0.
          IF ls_batch_char_1-value IS INITIAL.
            CONTINUE.
          ENDIF.
        ELSE.
          CONTINUE.
        ENDIF.

        CASE ls_cabn_1-atfor.
          WHEN zcl_abs_abap_maintain=>c_char_datatyp_char. "'CHAR'
            CLEAR ls_allocvalueschar.
            ls_allocvalueschar-charact    = ls_batch_char_1-atnam.
            ls_allocvalueschar-value_char = ls_batch_char_1-value.
            APPEND ls_allocvalueschar TO lt_allocvalueschar.
          WHEN zcl_abs_abap_maintain=>c_char_datatyp_curr. "'CURR'
            CLEAR ls_allocvaluescurr.
            ls_allocvaluescurr-charact    = ls_batch_char_1-atnam.
            ls_allocvaluescurr-value_from = ls_batch_char_1-value.
            APPEND ls_allocvaluescurr TO lt_allocvaluescurr.
          WHEN zcl_abs_abap_maintain=>c_char_datatyp_date. "'DATE'
*--Calling FM to convert date to float format
            CLEAR ls_allocvaluesnum.
            IF lv_atwrt IS INITIAL.
              CONCATENATE ls_batch_char_1-value+6(4)
                          ls_batch_char_1-value+3(2)
                          ls_batch_char_1-value(2)
                          INTO lv_atwrt.
            ENDIF.
            CALL FUNCTION 'CTCV_CONVERT_DATE_TO_FLOAT'
              EXPORTING
                date  = lv_atwrt
              IMPORTING
                float = lv_float.
            ls_allocvaluesnum-value_from = lv_float.
            ls_allocvaluesnum-charact    = ls_batch_char_1-atnam.
            APPEND ls_allocvaluesnum TO lt_allocvaluesnum.
            CLEAR lv_atwrt.
          WHEN OTHERS.
            CLEAR ls_allocvaluesnum.
            ls_allocvaluesnum-charact    = ls_batch_char_1-atnam.
            ls_allocvaluesnum-value_from = ls_batch_char_1-value.
            APPEND ls_allocvaluesnum TO lt_allocvaluesnum.
        ENDCASE.
      ENDLOOP.
    ENDIF.

*  ASSIGN ('(/AGRI/SAPLFMNSM)GS_FPDOC_INFOCUS-X-FPHDR-MATNR') TO <lv_matnr>.
    ASSIGN is_batch_crt-matnr TO <lv_matnr>.
    IF sy-subrc EQ 0.
      READ TABLE gt_batch_char INTO ls_batch_char_1 INDEX 1.
      IF sy-subrc EQ 0
      AND <lv_matnr> IS NOT INITIAL
      AND ls_batch_char_1-batch IS NOT INITIAL.
        CONCATENATE <lv_matnr> ls_batch_char_1-batch INTO lv_objectkey RESPECTING BLANKS.

        CALL FUNCTION 'BAPI_OBJCL_CHANGE'
          EXPORTING
            objecttable        = 'MCH1'
            classnum           = gv_classnum
            classtype          = '023'
            objectkey_long     = lv_objectkey
          TABLES
            allocvaluesnumnew  = lt_allocvaluesnum
            allocvaluescharnew = lt_allocvalueschar
            allocvaluescurrnew = lt_allocvaluescurr
            return             = lt_return_x.

        READ TABLE lt_return_x INTO DATA(lwa_return_x)
          WITH KEY type = c_msg_type-error.
        IF sy-subrc NE 0.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

          READ TABLE lt_return_x TRANSPORTING NO FIELDS
            WITH KEY type = c_msg_type-success.
          IF sy-subrc EQ 0.
*-- Características do Lote Salvas
*            MESSAGE i025(zabs_msgcls).
            CLEAR : ls_message.
            ls_message-msgty = 'I'. " lwa_return-type.
            ls_message-msgid = 'ZABS_MSGCLS'. " lwa_return-id.
            ls_message-msgno = '025'. " lwa_return-number.

            APPEND ls_message TO et_messages.
            CLEAR ls_message.

          ELSE.
*-- Erro ao Salvar as Características do Lote!
*            MESSAGE i327(zfmfp).
            CLEAR : ls_message.
            ls_message-msgty = 'I'. " lwa_return-type.
            ls_message-msgid = 'ZFMFP'. " lwa_return-id.
            ls_message-msgno = '327'. " lwa_return-number.

            APPEND ls_message TO et_messages.
            CLEAR ls_message.


          ENDIF.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

          IF lwa_return_x-id IS NOT INITIAL
          AND lwa_return_x-type IS NOT INITIAL
          AND lwa_return_x-number IS NOT INITIAL.
**            MESSAGE ID lwa_return_x-id TYPE lwa_return_x-type NUMBER lwa_return_x-number
**               WITH lwa_return_x-message_v1 lwa_return_x-message_v2
**                    lwa_return_x-message_v3 lwa_return_x-message_v4.
            CLEAR : ls_message.
            ls_message-msgty = lwa_return_x-type.
            ls_message-msgid = lwa_return_x-id.
            ls_message-msgno = lwa_return_x-number.

            APPEND ls_message TO et_messages.
            CLEAR ls_message.
          ELSE.
*-- Erro ao Salvar as Características do Lote!
**            MESSAGE i327(zfmfp).
            CLEAR : ls_message.
            ls_message-msgty = 'I'. " lwa_return-type.
            ls_message-msgid = 'ZFMFP'. " lwa_return-id.
            ls_message-msgno = '327'. " lwa_return-number.

            APPEND ls_message TO et_messages.
            CLEAR ls_message.

          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    "----------------------------End of Save Characteristics value

  ENDMETHOD.
ENDCLASS.
