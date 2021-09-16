class ZCL_ZABS_MOBILE_ACM_DPC_EXT definition
  public
  inheriting from ZCL_ZABS_MOBILE_ACM_DPC
  create public .

public section.

  types:
    BEGIN OF ty_accom_cnt,
        mackey TYPE zabs_del_mackey,
        posnr  TYPE /agri/glposnr,
      END OF ty_accom_cnt .
  types:
    BEGIN OF ty_btch,
        in_licplate TYPE zabs_del_licplate,
        charg       TYPE charg_d,
      END OF ty_btch .
  types:
    BEGIN OF ty_accom_oper,
        mackey       TYPE zabs_del_mackey,
        posnr        TYPE /agri/glposnr,
        operation_no TYPE i,
      END OF ty_accom_oper .
  types:
    BEGIN OF ty_batch_oper,
        charg TYPE charg_d,
        operation_no TYPE i,
       END OF ty_batch_oper .
  types:
    BEGIN OF ty_mackey,
        mackey TYPE zabs_del_mackey,
      END OF ty_mackey .
  types:
    BEGIN OF TY_CARIMBO,
       tplnr   TYPE /AGRI/GLTPLNR_FL,
       IMOV    TYPE YOIMOV,
       TALHAO  TYPE YOTALHAO,
       CARIMBO TYPE YOTPCLHT_D,
       TOCLHTT TYPE YODESC,
     END OF TY_CARIMBO .
  types:
    tt_carimbo TYPE STANDARD TABLE OF TY_CARIMBO .

  methods FETCH_CARIMBO
    importing
      !IT_TPLNR type /AGRI/T_GLTPLNR
    changing
      !CT_CARIMBO type TT_CARIMBO .

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_BEGIN
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_END
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_PROCESS
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~EXECUTE_ACTION
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITYSET_DELTA
    redefinition .
protected section.

  methods ACCBAGGRPCLOSEDS_CREATE_ENTITY
    redefinition .
  methods ACCBAGGRPCLOSEDS_GET_ENTITYSET
    redefinition .
  methods ACCBATCHSET_CREATE_ENTITY
    redefinition .
  methods ACCBATCHSET_GET_ENTITYSET
    redefinition .
  methods ACCBATCHUPDATESE_CREATE_ENTITY
    redefinition .
  methods ACCHEADERSET_GET_ENTITYSET
    redefinition .
  methods ACCITEMSSET_GET_ENTITYSET
    redefinition .
  methods ACTIVITIESSET_GET_ENTITYSET
    redefinition .
  methods BAGGROUPSET_CREATE_ENTITY
    redefinition .
  methods BAGGROUPSET_GET_ENTITYSET
    redefinition .
  methods BEXITHDRPLATESET_GET_ENTITYSET
    redefinition .
  methods BEXITTRAILPLATES_GET_ENTITYSET
    redefinition .
  methods BINEXTRACTSET_GET_ENTITYSET
    redefinition .
  methods BINF4SET_GET_ENTITYSET
    redefinition .
  methods EVNJUSTSET_GET_ENTITYSET
    redefinition .
  methods FARMTERRAINSSET_GET_ENTITYSET
    redefinition .
  methods INTRBATCHSET_CREATE_ENTITY
    redefinition .
  methods INTRBATCHSET_GET_ENTITYSET
    redefinition .
  methods INTRPLATEEXTRACT_GET_ENTITYSET
    redefinition .
  methods LEADFARMSSET_GET_ENTITYSET
    redefinition .
  methods LOADBAGGROUPSET_GET_ENTITYSET
    redefinition .
  methods LOADER_F4SET_GET_ENTITYSET
    redefinition .
  methods LOADFARMS_F4SET_GET_ENTITYSET
    redefinition .
  methods TERRAINCARIMBOSE_GET_ENTITYSET
    redefinition .
  methods TRANTYPSET_GET_ENTITYSET
    redefinition .
  methods TURMAHEADSET_GET_ENTITYSET
    redefinition .
  methods TURMAITMSET_GET_ENTITYSET
    redefinition .
  methods USERLOGINSET_GET_ENTITYSET
    redefinition .
  methods USERROLESET_GET_ENTITYSET
    redefinition .
  methods ACCITEMS_REPSET_GET_ENTITYSET
    redefinition .
private section.

  methods GET_LOADER_FARMS
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_LOADER_FARMS type ZABS_TTY_AC_LOADE_FARMS_F4 .
  methods GET_EMPLOYEE_ROLE
    importing
      !IV_USER type XUBNAME
    exporting
      !ES_USR_EMP type ZABS_USR_EMP
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods GET_TURMA_HEADER
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_TURMA_HEAD type ZCL_ZABS_MOBILE_ACM_MPC=>TT_TURMAHEAD
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods GET_TURMA_ITEMS
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_TURMA_ITEMS type ZCL_ZABS_MOBILE_ACM_MPC=>TT_TURMAITM
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods GET_LEADER_FARMS
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_LEADER_FARMS type ZCL_ZABS_MOBILE_ACM_MPC=>TT_LEADFARMS
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods GET_FARM_TERRAINS
    importing
      !IT_LEADER_FARMS type ZCL_ZABS_MOBILE_ACM_MPC=>TT_LEADFARMS
    exporting
      !ET_FARM_TERRAINS type ZCL_ZABS_MOBILE_ACM_MPC=>TT_FARMTERRAINS
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods GET_DELTA_TOKEN
    importing
      !IV_ENTITY_SET_NAME type STRING
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
    exporting
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
    changing
      !CT_ENTITYSET type STANDARD TABLE .
  methods GET_ACTIVITIES
    exporting
      !ET_ACTIVITIES type ZCL_ZABS_MOBILE_ACM_MPC=>TT_ACTIVITIES
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods GET_EVENT_JUSTIFICATIONS
    exporting
      !ET_EVEJUST type ZCL_ZABS_MOBILE_ACM_MPC=>TT_EVNJUST
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods CREATE_ACCOMPLISHMENT
    importing
      !IS_FMAC_DOC type /AGRI/S_FMACS_DOC
    exporting
      !ES_FMAC_DOC type /AGRI/S_FMACS_DOC
      !ET_MESSAGES type /AGRI/T_GPROLOG
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
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
  methods GET_ACCOMPLISH_DATA
    exporting
      !ES_AC_DATA type ZABS_S_AC_MOB_DATA
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods POPULATE_ACCOMPLISHMENT_DATA
    importing
      !IS_ACHDR type /AGRI/S_FMACHDR
      !IT_ACITM type /AGRI/T_FMACITM
    exporting
      !ES_ACDOC type /AGRI/S_FMACS_DOC .
  methods GET_ACCOMPLISHMENT_ITEMS
    importing
      !IT_LEADER_FARMS type ZCL_ZABS_MOBILE_ACM_MPC=>TT_LEADFARMS
      !IT_FARM_TERRAINS type ZCL_ZABS_MOBILE_ACM_MPC=>TT_FARMTERRAINS
    exporting
      !ET_ACCITEMS type ZCL_ZABS_MOBILE_ACM_MPC=>TT_ACCITEMS .
  methods BATCH_CREATE
    exporting
      !ET_MESSAGES type BAPIRET2_TT
    changing
      !CS_AC_BATCH type ZABS_S_AC_BATCH .
  methods BATCH_UPDATE
    importing
      !IT_AC_BATCH type ZABS_T_AC_BATCH
    exporting
      !ET_MESSAGES type BAPIRET2_TT .
  methods BATCH_EXTRACT
    importing
      !IV_CHARG type CHARG_D
      !IV_TMATNR type /AGRI/GLTMATNR
    exporting
      !ES_AC_BATCH type ZABS_S_AC_BATCH .
  methods INTERNAL_PLATE_EXTRACT
    importing
      !IV_PLATE type ZABS_DEL_LICPLATE optional
      !IV_FARM type /AGRI/GLTPLMA optional
    exporting
      !ET_AC_ILP type ZABS_T_AC_ILP .
  methods BIN_EXTRACT
    importing
      !IV_BIN type ZABS_DEL_BIN
      !IV_PLATE type ZABS_DEL_LICPLATE
    exporting
      !ET_AC_BIN type ZABS_T_AC_BIN .
  methods GET_INTRBATCH
    exporting
      !ET_INTR_BATCH type ZABS_TTY_AC_INTR_BATCH .
  methods GET_TRANTYP
    exporting
      !ET_TRANTYP type ZABS_TTY_AC_TRAN_TYP .
  methods GET_LOADBAGGROUP
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_LOADBAGGROUP type ZABS_TTY_AC_LDBAGGRP .
  methods GET_BAGGROUP
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_BAGGROUP type ZABS_TTY_AC_BGGRP .
  methods GET_ACCBATCH
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_ACCBATCH type ZABS_TTY_AC_BATCH .
  methods GET_USR_ROLE
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_USR_ROLE type ZABS_TTY_USR_ROLE .
  methods ACCOM_CREATE
    exporting
      !ET_MESSAGES type /AGRI/T_GPROLOG
    changing
      !CT_ACCOM type ZABS_TTY_BGP_ACCOM .
  methods GET_ACCOM_ITEMS
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
    exporting
      !ET_ACCITEMS type ZCL_ZABS_MOBILE_ACM_MPC=>TT_ACCITEMS .
  methods GET_ACCBAGGRPCLOSED
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_ACCBAGGRPCLOSED type ZABS_TTY_AC_BGCLOSE .
  methods GET_FARMS_LOADER
    importing
      !IT_LEADER_FARMS type ZCL_ZABS_MOBILE_ACM_MPC=>TT_LEADFARMS optional
    exporting
      !ET_FARMS_LOADER type ZABS_TTY_AC_LOADER_F4 .
  methods GET_ACCOM_ITEMS_REP
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
    exporting
      !ET_ACCITEMS type ZCL_ZABS_MOBILE_ACM_MPC=>TT_ACCITEMS .
ENDCLASS.



CLASS ZCL_ZABS_MOBILE_ACM_DPC_EXT IMPLEMENTATION.


METHOD /iwbep/if_mgw_appl_srv_runtime~changeset_begin.

  CLEAR: cv_defer_mode.
  cv_defer_mode = abap_true.

*-- Looping Entityset name in the Operation Table
  LOOP AT it_operation_info ASSIGNING FIELD-SYMBOL(<fs_operation_info>).
    IF NOT ( <fs_operation_info>-entity_set  EQ 'AccHeaderSet' OR
             <fs_operation_info>-entity_set  EQ 'AccItemsSet'  OR
             <fs_operation_info>-entity_set  EQ 'AccBatchUpdateSet' ).
      cv_defer_mode = abap_false.
      EXIT.
    ENDIF.
  ENDLOOP.

ENDMETHOD.


METHOD /iwbep/if_mgw_appl_srv_runtime~changeset_end.

  COMMIT WORK.

ENDMETHOD.


METHOD /iwbep/if_mgw_appl_srv_runtime~changeset_process.

  TYPES : BEGIN OF ty_acitm_posnr.
      INCLUDE TYPE /agri/s_fmacitm.
  TYPES : kposnr TYPE /agri/glposnr,
          END OF ty_acitm_posnr.

*-- Local Declarations
  DATA: lo_update_context      TYPE REF TO /iwbep/if_mgw_req_entity_u,
        lv_entity_type         TYPE string,
        ls_acc_header          TYPE zcl_zabs_mobile_acm_mpc=>ts_accheader,
        ls_acc_items           TYPE zcl_zabs_mobile_acm_mpc=>ts_accitems,
        ls_acc_batch_upd       TYPE zcl_zabs_mobile_acm_mpc=>ts_accbatchupdate,
        ls_acc_batch           TYPE zabs_s_ac_batch,
        lt_acc_batch           TYPE zabs_t_ac_batch,
        lt_acc_items           TYPE zcl_zabs_mobile_acm_mpc=>tt_accitems,
        ls_achdr               TYPE /agri/s_fmachdr,
        lt_achdr               TYPE /agri/t_fmachdr,
        ls_acitm               TYPE /agri/s_fmacitm,
        lt_acitm               TYPE /agri/t_fmfmacitm,
        lt_acitm_tmp           TYPE /agri/t_fmfmacitm,
        ls_fmac_doc            TYPE /agri/s_fmacs_doc,
        lt_messages            TYPE /agri/t_gprolog,
        ls_messages            TYPE /agri/s_gprolog,
        lt_bapi_messages       TYPE bapiret2_tt,
        lv_operation_no        TYPE i,
        lwa_changeset_response TYPE /iwbep/if_mgw_appl_types=>ty_s_changeset_response,
*        lt_accom               TYPE /agri/t_fmacom,
*        ls_accom               TYPE /agri/s_fmacom,
        lt_accom               TYPE TABLE OF zabs_bgp_accom,
        ls_accom               TYPE zabs_bgp_accom,
        lt_acdoc               TYPE /agri/t_fmacs_doc,
        lt_accom_cnt           TYPE SORTED TABLE OF ty_accom_cnt WITH UNIQUE KEY mackey,
        ls_accom_cnt           TYPE ty_accom_cnt,
        lv_cnt                 TYPE i,
        lt_accom_oper          TYPE STANDARD TABLE OF ty_accom_oper,
        lt_batch_oper          TYPE STANDARD TABLE OF ty_batch_oper,
        ls_accom_oper          TYPE ty_accom_oper,
        ls_batch_oper          TYPE ty_batch_oper,
        lt_mackey              TYPE STANDARD TABLE OF ty_mackey,
        ls_mackey              TYPE ty_mackey,
        lv_accom               TYPE /agri/fmaccom,
        lt_acitm_posnr         TYPE STANDARD TABLE OF ty_acitm_posnr,
        ls_acitm_posnr         TYPE ty_acitm_posnr,
        lv_create              TYPE xfeld.

*  LOOP AT it_changeset_request ASSIGNING FIELD-SYMBOL(<fs_changeset_request>).
*    CLEAR lv_entity_type.
*    lo_update_context ?= <fs_changeset_request>-request_context.
*    lv_entity_type = lo_update_context->get_entity_type_name( ).
*
*    IF lv_entity_type NE 'AccItems' AND lv_entity_type NE 'AccBatchUpdate'.
*      RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
*        EXPORTING
*          textid      = /iwbep/cx_mgw_tech_exception=>operation_not_supported
*          operation   = 'UPDATE_ENTITY'
*          entity_type = lv_entity_type.
*    ENDIF.
*
*    CASE lv_entity_type.
*
*      WHEN 'AccItems'.
*        <fs_changeset_request>-entry_provider->read_entry_data( "#EC CI_FLDEXT_OK
*                           IMPORTING es_data = ls_acc_items ).
*
**-- Collect Accomplishment Keys
*        CLEAR ls_mackey.
*        ls_mackey-mackey = ls_acc_items-zzmackey.
*        APPEND ls_mackey TO lt_mackey.
*
*      WHEN 'AccBatchUpdate'.
*        <fs_changeset_request>-entry_provider->read_entry_data( "#EC CI_FLDEXT_OK
*                           IMPORTING es_data = ls_acc_batch_upd ).
*
**-- Collect Accomplishment Keys
*        MOVE-CORRESPONDING ls_acc_batch_upd TO ls_acc_batch.
*        APPEND ls_acc_batch TO lt_acc_batch.
*        CLEAR ls_acc_batch.
*
*        CLEAR ls_batch_oper.
*        ls_batch_oper-charg = ls_acc_batch_upd-charg.
*        ls_batch_oper-operation_no = <fs_changeset_request>-operation_no.
*        APPEND ls_batch_oper TO lt_batch_oper.
*
*    ENDCASE.
*  ENDLOOP.
*
*  IF lt_mackey IS NOT INITIAL.
*    SORT lt_mackey.
*    DELETE ADJACENT DUPLICATES FROM lt_mackey COMPARING ALL FIELDS.
*
**-- Fetch the Accomplishment Numbers for the keys
*    SELECT accom, zzmackey
*      FROM /agri/fmachdr
*      INTO TABLE @DATA(lt_accom_key)
*       FOR ALL ENTRIES IN @lt_mackey
*     WHERE status   NE @zcl_abs_abap_maintain=>c_ac_status_deleted "'DEL'
*       AND zzmackey EQ @lt_mackey-mackey.
*    IF sy-subrc EQ 0.
*      lt_accom = lt_accom_key.
*      SORT lt_accom_key BY zzmackey.
*    ENDIF.
*
*    IF lt_accom IS NOT INITIAL.
*
**-- Fetch the Accomplishment data
*      CALL FUNCTION '/AGRI/FMAC_VIEW'
*        EXPORTING
*          it_accom       = lt_accom
*        IMPORTING
*          et_acdoc       = lt_acdoc
*        EXCEPTIONS
*          no_data_exists = 1
*          OTHERS         = 2.
*    ENDIF.
*
*    LOOP AT it_changeset_request ASSIGNING <fs_changeset_request>.
*      CLEAR lv_entity_type.
*      lo_update_context ?= <fs_changeset_request>-request_context.
*      lv_entity_type = lo_update_context->get_entity_type_name( ).
*
*      CASE lv_entity_type.
*        WHEN 'AccItems'.
*          <fs_changeset_request>-entry_provider->read_entry_data( "#EC CI_FLDEXT_OK
*                             IMPORTING es_data = ls_acc_items ).
*
**-- Populate the accomplishment number
*          READ TABLE lt_accom_key INTO DATA(ls_accom_key)
*                WITH KEY zzmackey = ls_acc_items-zzmackey
*              BINARY SEARCH.
*          IF sy-subrc EQ 0.
*            ls_acc_items-accom = ls_accom_key-accom.
*          ENDIF.
*
**-- Collect the Accomplishment headers
*          CLEAR ls_achdr.
*          MOVE-CORRESPONDING ls_acc_items TO ls_achdr.
*          CLEAR ls_achdr-updkz.
*          APPEND ls_achdr TO lt_achdr.
*
**-- Collect Accomplishment items
*          CLEAR ls_acitm.
*          MOVE-CORRESPONDING ls_acc_items TO ls_acitm.
*          ls_acitm-zztplnr = ls_acc_items-tplnr.
*          ls_acitm-zzcdate = sy-datum.
*          ls_acitm-zzctime = sy-uzeit.
*          ls_acitm-zzcrimei  = ls_acc_items-imei.
*          ls_acitm-zzcrbadge = ls_acc_items-badge.
*
*          APPEND ls_acitm TO lt_acitm.
*
**-- Collect the items coming from front end
*          APPEND ls_acc_items TO lt_acc_items.
*
**-- Collect the operations
*          CLEAR ls_accom_oper.
*          ls_accom_oper-mackey = ls_acitm-zzmackey.
*          ls_accom_oper-posnr  = ls_acitm-posnr.
**          ls_accom_oper-mackey = ls_acc_items-zzmackey.
**          ls_accom_oper-posnr  = ls_acc_items-kposnr.
*          ls_accom_oper-operation_no = <fs_changeset_request>-operation_no.
*          APPEND ls_accom_oper TO lt_accom_oper.
*
*      ENDCASE.
*    ENDLOOP.
*
*    SORT lt_achdr BY zzmackey.
*    DELETE ADJACENT DUPLICATES FROM lt_achdr COMPARING zzmackey.
*
*    SORT lt_acitm BY zzmackey posnr.
*    SORT lt_acc_items BY zzmackey posnr.
**    SORT lt_acc_items BY zzmackey kposnr.
*    SORT lt_accom_oper BY mackey posnr.
*
**-- For each accomplishment header
*    LOOP AT lt_achdr INTO ls_achdr.
*
*      REFRESH : lt_acitm_tmp,
*                lt_acitm_posnr.
*
*      CLEAR lv_create.
*
*      CLEAR ls_accom_key.
*      READ TABLE lt_accom_key INTO ls_accom_key
*            WITH KEY zzmackey = ls_achdr-zzmackey
*          BINARY SEARCH.
*      IF sy-subrc EQ 0.                  "If Accomplish already created
*
*        READ TABLE lt_acdoc ASSIGNING FIELD-SYMBOL(<fs_acdoc>)
*              WITH KEY accom = ls_accom_key-accom
*            BINARY SEARCH.
*        IF sy-subrc EQ 0.
*
*          DATA(lt_acdoc_temp) = <fs_acdoc>-x-acitm.
*          SORT lt_acdoc_temp BY posnr DESCENDING.
*
*          READ TABLE lt_acdoc_temp INTO DATA(ls_acdoc_temp) INDEX 1.
*          IF sy-subrc = 0.
*            DATA(lv_posnr) = ls_acdoc_temp-posnr.
*          ENDIF.
*
*          SORT lt_acdoc_temp BY idresource strtdat strttim.
*
*          READ TABLE lt_acitm TRANSPORTING NO FIELDS
*                WITH KEY zzmackey = ls_achdr-zzmackey
*              BINARY SEARCH.
*          IF sy-subrc EQ 0.
*            DATA(lv_tabix) = sy-tabix.
*            LOOP AT lt_acitm INTO ls_acitm FROM lv_tabix.
*              IF ls_acitm-zzmackey NE ls_achdr-zzmackey.
*                EXIT.
*              ENDIF.
*
*              CLEAR ls_acitm_posnr.
*              MOVE ls_acitm TO ls_acitm_posnr.
*              ls_acitm_posnr-kposnr = ls_acitm-posnr.
*
*              CASE ls_acitm-updkz.
*
*                WHEN zcl_abs_abap_maintain=>c_updkz_insert.
*
**                  READ TABLE <fs_acdoc>-x-acitm TRANSPORTING NO FIELDS
**                  WITH KEY accom = ls_acitm-accom
**                           posnr = ls_acitm-posnr.
**                  IF sy-subrc NE 0.
*
*                  READ TABLE lt_acdoc_temp TRANSPORTING NO FIELDS
*                        WITH KEY idresource = ls_acitm-idresource
*                                 strtdat    = ls_acitm-strtdat
*                                 strttim    = ls_acitm-strttim
*                      BINARY SEARCH.
*                  IF sy-subrc NE 0.
**-- To avoid the duplicate record
*                    lv_posnr = lv_posnr + 1.
*                    ls_acitm-posnr = lv_posnr.
*                    ls_acitm_posnr-posnr = lv_posnr.
*
**-- Add new item
*                    IF ls_achdr-zzactcg = zcl_abs_abap_maintain=>c_actcg_prod.
*                      ls_acitm-status = zcl_abs_abap_maintain=>c_ac_status_created.
*                      <fs_acdoc>-x-achdr-status = zcl_abs_abap_maintain=>c_ac_status_created.
*                    ELSE.
*                      ls_acitm-status = zcl_abs_abap_maintain=>c_ac_status_confirm.
*                    ENDIF.
*                    APPEND ls_acitm TO <fs_acdoc>-x-acitm.
*
*                    lv_create = abap_true.
*                  ENDIF.
*
**                  ENDIF.
*
**                WHEN zcl_abs_abap_maintain=>c_updkz_update.
**
***-- Update the existed item
**                  READ TABLE <fs_acdoc>-x-acitm ASSIGNING FIELD-SYMBOL(<fs_acitm>)
**                        WITH KEY accom = ls_acitm-accom
**                                 posnr = ls_acitm-posnr
**                      BINARY SEARCH.
**                  IF sy-subrc EQ 0.
**                    MOVE-CORRESPONDING ls_acitm TO <fs_acitm>.
**                  ENDIF.
**
**                WHEN zcl_abs_abap_maintain=>c_updkz_delete.
**
***-- Delete the existed item
**                  READ TABLE <fs_acdoc>-x-acitm TRANSPORTING NO FIELDS
**                        WITH KEY accom = ls_acitm-accom
**                                 posnr = ls_acitm-posnr
**                      BINARY SEARCH.
**                  IF sy-subrc EQ 0.
**                    DELETE <fs_acdoc>-x-acitm INDEX sy-tabix.
**                  ENDIF.
**
**                  READ TABLE <fs_acdoc>-y-acitm ASSIGNING <fs_acitm>
**                        WITH KEY accom = ls_acitm-accom
**                                 posnr = ls_acitm-posnr
**                      BINARY SEARCH.
**                  IF sy-subrc EQ 0.
**                    <fs_acitm>-updkz = ls_acitm-updkz.
**                  ENDIF.
*
*              ENDCASE.
*
**              APPEND ls_acitm TO lt_acitm_tmp.
*              APPEND ls_acitm_posnr TO lt_acitm_posnr.
*            ENDLOOP.
*          ENDIF.
*
*          IF lv_create EQ abap_true.
*            DATA(ls_acdoc) = <fs_acdoc>.
*          ENDIF.
*
*        ENDIF.
*
*      ELSE.                                    "No accomplishment created
*
*        READ TABLE lt_acitm TRANSPORTING NO FIELDS
*              WITH KEY zzmackey = ls_achdr-zzmackey
*            BINARY SEARCH.
*        IF sy-subrc EQ 0.
*          lv_tabix = sy-tabix.
*          LOOP AT lt_acitm INTO ls_acitm FROM lv_tabix.
*            IF ls_acitm-zzmackey NE ls_achdr-zzmackey.
*              EXIT.
*            ENDIF.
*            APPEND ls_acitm TO lt_acitm_tmp.
*            CLEAR ls_acitm_posnr.
*            MOVE ls_acitm TO ls_acitm_posnr.
*            ls_acitm_posnr-kposnr = ls_acitm-posnr.
*            APPEND ls_acitm_posnr TO lt_acitm_posnr.
*          ENDLOOP.
*        ENDIF.
*
*        IF lt_acitm_tmp IS NOT INITIAL.
**-- Populate Accomplishment data
*          CALL METHOD me->populate_accomplishment_data
*            EXPORTING
*              is_achdr = ls_achdr
*              it_acitm = lt_acitm_tmp
*            IMPORTING
*              es_acdoc = ls_acdoc.
*        ENDIF.
*      ENDIF.
*
*      IF ls_acdoc IS NOT INITIAL.
*        REFRESH lt_messages.
*        CLEAR ls_fmac_doc.
*
**-- Create/Update the Accomplishment
*        CALL METHOD me->create_accomplishment
*          EXPORTING
*            is_fmac_doc = ls_acdoc
*          IMPORTING
*            es_fmac_doc = ls_fmac_doc
*            et_messages = lt_messages.
*
**-- Populate Accomplishment Sheet number and Operation number
*        CLEAR lv_accom.
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*          EXPORTING
*            input  = ls_fmac_doc-accom
*          IMPORTING
*            output = lv_accom.
*
**        LOOP AT lt_acitm_tmp INTO ls_acitm.
**        READ TABLE lt_acc_items TRANSPORTING NO FIELDS
**                        WITH KEY zzmackey = ls_achdr-zzmackey
**                      BINARY SEARCH.
**        IF sy-subrc = 0.
**          lv_tabix = sy-tabix.
**          LOOP AT lt_acc_items ASSIGNING FIELD-SYMBOL(<fs_acc_items>) FROM lv_tabix.
**            IF <fs_acc_items>-zzmackey NE ls_achdr-zzmackey.
**              EXIT.
**            ENDIF.
**            <fs_acc_items>-accom = lv_accom.
**
**            CLEAR ls_accom_oper.
**            READ TABLE lt_accom_oper INTO ls_accom_oper
**                  WITH KEY mackey = <fs_acc_items>-zzmackey
**                           posnr  = <fs_acc_items>-kposnr
**                BINARY SEARCH.
**            IF sy-subrc EQ 0.
***-- Populate the operation number
**              lwa_changeset_response-operation_no = ls_accom_oper-operation_no.
**            ENDIF.
*
*        LOOP AT lt_acitm_posnr INTO ls_acitm_posnr.
*
*          CLEAR ls_accom_oper.
*          READ TABLE lt_accom_oper INTO ls_accom_oper
*                WITH KEY mackey = ls_acitm_posnr-zzmackey
*                         posnr  = ls_acitm_posnr-kposnr
*              BINARY SEARCH.
*          IF sy-subrc EQ 0.
**-- Populate the operation number
*            lwa_changeset_response-operation_no = ls_accom_oper-operation_no.
*          ENDIF.
*
*          CLEAR ls_acc_items.
*          READ TABLE lt_acc_items INTO ls_acc_items
*                WITH KEY zzmackey = ls_acitm_posnr-zzmackey
*                         posnr    = ls_acitm_posnr-kposnr
*              BINARY SEARCH.
*          IF sy-subrc EQ 0.
*            ls_acc_items-accom = lv_accom.
*            ls_acc_items-posnr = ls_acitm_posnr-posnr.
*          ENDIF.
*
*          copy_data_to_ref( EXPORTING is_data = ls_acc_items
*               CHANGING cr_data = lwa_changeset_response-entity_data ).
*          INSERT lwa_changeset_response INTO TABLE ct_changeset_response.
*
*          CLEAR ls_acdoc.
*        ENDLOOP.
*
**-- Add the messages to containersa
*        add_messages_to_msg_container( iv_exception = abap_true
*                                       it_messages = lt_messages ).
*
*      ENDIF.
*
*      CLEAR: lv_posnr, ls_acdoc.
*      REFRESH lt_acdoc_temp.
*    ENDLOOP.
*  ENDIF.
*
*  IF lt_acc_batch IS NOT INITIAL.
*    CALL METHOD me->batch_update
*      EXPORTING
*        it_ac_batch = lt_acc_batch
*      IMPORTING
*        et_messages = lt_bapi_messages.
*
*    LOOP AT lt_acc_batch INTO ls_acc_batch.
*      READ TABLE lt_batch_oper INTO ls_batch_oper
*            WITH KEY charg = ls_acc_batch-charg.
*      IF sy-subrc EQ 0.
*        lwa_changeset_response-operation_no = ls_batch_oper-operation_no.
*        copy_data_to_ref( EXPORTING is_data = ls_acc_batch
*             CHANGING cr_data = lwa_changeset_response-entity_data ).
*        INSERT lwa_changeset_response INTO TABLE ct_changeset_response.
*      ENDIF.
*    ENDLOOP.
*
**-- Add the messages to containersa
*    add_messages_to_msg_container( iv_exception = abap_true
*                                   it_bapi_messages = lt_bapi_messages ).
*  ENDIF.



  LOOP AT it_changeset_request ASSIGNING FIELD-SYMBOL(<fs_changeset_request>).
    CLEAR lv_entity_type.
    lo_update_context ?= <fs_changeset_request>-request_context.
    lv_entity_type = lo_update_context->get_entity_type_name( ).

    IF lv_entity_type NE 'AccItems' AND lv_entity_type NE 'AccBatchUpdate'.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
        EXPORTING
          textid      = /iwbep/cx_mgw_tech_exception=>operation_not_supported
          operation   = 'UPDATE_ENTITY'
          entity_type = lv_entity_type.
    ENDIF.

    CASE lv_entity_type.

      WHEN 'AccItems'.
        <fs_changeset_request>-entry_provider->read_entry_data( "#EC CI_FLDEXT_OK
                           IMPORTING es_data = ls_acc_items ).

        MOVE-CORRESPONDING ls_acc_items TO ls_accom.
        ls_accom-badge  = ls_acc_items-badge(8).
        ls_accom-pernr  = ls_accom-badge.
        ls_accom-ernam  = ls_accom-mobusr = sy-uname.
        ls_accom-erdat  = sy-datum.
        ls_accom-erzet  = sy-uzeit.
        APPEND ls_accom TO lt_accom.
        CLEAR : ls_accom.

        lwa_changeset_response-operation_no = <fs_changeset_request>-operation_no.
        copy_data_to_ref( EXPORTING is_data = ls_acc_items "#EC CI_FLDEXT_OK
           CHANGING cr_data = lwa_changeset_response-entity_data ).
        INSERT lwa_changeset_response INTO TABLE ct_changeset_response.

      WHEN 'AccBatchUpdate'.
        <fs_changeset_request>-entry_provider->read_entry_data( "#EC CI_FLDEXT_OK
                           IMPORTING es_data = ls_acc_batch_upd ).

*-- Collect Accomplishment Keys
        MOVE-CORRESPONDING ls_acc_batch_upd TO ls_acc_batch.
        APPEND ls_acc_batch TO lt_acc_batch.
        CLEAR ls_acc_batch.

        CLEAR ls_batch_oper.
        ls_batch_oper-charg = ls_acc_batch_upd-charg.
        ls_batch_oper-operation_no = <fs_changeset_request>-operation_no.
        APPEND ls_batch_oper TO lt_batch_oper.

    ENDCASE.
  ENDLOOP.

  IF lt_accom IS NOT INITIAL.

    CALL METHOD me->accom_create
      IMPORTING
        et_messages = lt_messages
      CHANGING
        ct_accom    = lt_accom.

*-- Add the messages to containersa
    add_messages_to_msg_container( iv_exception = abap_true
                                   it_messages = lt_messages ).
  ENDIF. "lt_acc_items


  IF lt_acc_batch IS NOT INITIAL.
    CALL METHOD me->batch_update
      EXPORTING
        it_ac_batch = lt_acc_batch
      IMPORTING
        et_messages = lt_bapi_messages.

    LOOP AT lt_acc_batch INTO ls_acc_batch.
      READ TABLE lt_batch_oper INTO ls_batch_oper
            WITH KEY charg = ls_acc_batch-charg.
      IF sy-subrc EQ 0.
        lwa_changeset_response-operation_no = ls_batch_oper-operation_no.
        copy_data_to_ref( EXPORTING is_data = ls_acc_batch
             CHANGING cr_data = lwa_changeset_response-entity_data ).
        INSERT lwa_changeset_response INTO TABLE ct_changeset_response.
      ENDIF.
    ENDLOOP.
*-- Add the messages to containersa
    add_messages_to_msg_container( iv_exception = abap_true
                                   it_bapi_messages = lt_bapi_messages ).
  ENDIF. "lt_acc_batch

ENDMETHOD.


METHOD /iwbep/if_mgw_appl_srv_runtime~execute_action.

*-- Local Decarations
  DATA: lv_bin      TYPE zabs_del_bin,
        lv_ymatnr   TYPE /agri/glymatnr,
        lv_carimbo  TYPE zabs_del_tpclht,
        lv_bcarimbo TYPE zabs_del_tpclht,
        lv_hdrplate TYPE zabs_del_hdrplt,
        lv_licplate TYPE zabs_del_licplate,
        ls_entity   TYPE zcl_zabs_mobile_acm_mpc=>ts_message,
        lt_entity   TYPE zcl_zabs_mobile_acm_mpc=>tt_message.

  CASE iv_action_name.
    WHEN 'ValidateCarimVar'.
      IF it_parameter IS NOT INITIAL.
        READ TABLE it_parameter INTO DATA(ls_parameter)
              WITH KEY name = 'BIN'. " 'Bin'.
        IF sy-subrc EQ 0.
          lv_bin = ls_parameter-value.
        ENDIF.

        CLEAR ls_parameter.
        READ TABLE it_parameter INTO ls_parameter
              WITH KEY name = 'BCARIMBO'. " 'Carimbo'.
        IF sy-subrc EQ 0.
          lv_bcarimbo = ls_parameter-value.
        ENDIF.

        CLEAR ls_parameter.
        READ TABLE it_parameter INTO ls_parameter
              WITH KEY name = 'YMATNR'. " 'Ymatnr'.
        IF sy-subrc EQ 0.
*          lv_ymatnr = ls_parameter-value.
          CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
            EXPORTING
              input        = ls_parameter-value
            IMPORTING
              output       = lv_ymatnr
            EXCEPTIONS
              length_error = 1
              OTHERS       = 2.
        ENDIF.

        SELECT in_licplate, charg, bin, ymatnr, bcarimbo
          FROM zabs_ilp_btch
          INTO TABLE @DATA(lt_ilp_btch_entr)
         WHERE weighbridge EQ @space
           AND bin         EQ @lv_bin
           AND loevm       EQ @space
           AND transbord   EQ @space.
        IF sy-subrc EQ 0.
          LOOP AT lt_ilp_btch_entr TRANSPORTING NO FIELDS
                                  WHERE bcarimbo NE lv_bcarimbo.
            EXIT.
          ENDLOOP.
          IF sy-subrc EQ 0.
            ls_entity-msgty = 'W'. " 'E'.
            ls_entity-msg = TEXT-002.
            APPEND ls_entity TO lt_entity.
            CLEAR ls_entity.
          ENDIF.

          LOOP AT lt_ilp_btch_entr TRANSPORTING NO FIELDS
                                  WHERE ymatnr NE lv_ymatnr.
            EXIT.
          ENDLOOP.
          IF sy-subrc EQ 0.
            ls_entity-msgty = 'E'. " 'W'.
            ls_entity-msg = TEXT-003.
            APPEND ls_entity TO lt_entity.
            CLEAR ls_entity.
          ENDIF.
        ENDIF.

**        IF ls_entity-msgty IS INITIAL.
        IF lt_entity[] IS INITIAL.
          ls_entity-msgty = 'S'.
          APPEND ls_entity TO lt_entity.
          CLEAR ls_entity.
        ENDIF.

        DESCRIBE TABLE lt_entity LINES DATA(lv_count).
        IF lv_count EQ 2.
          DELETE lt_entity WHERE msgty EQ 'W'.
        ENDIF.

*        CALL METHOD copy_data_to_ref and EXPORT entity set data
        copy_data_to_ref( EXPORTING is_data = lt_entity " ls_entity
                CHANGING cr_data = er_data ).

      ENDIF.

    WHEN 'ValidateCarimVarExit'.

      IF it_parameter IS NOT INITIAL.
        CLEAR ls_parameter.
        READ TABLE it_parameter INTO ls_parameter
              WITH KEY name = 'BIN'. " 'Bin'.
        IF sy-subrc EQ 0.
          lv_bin = ls_parameter-value.
        ENDIF.

        CLEAR ls_parameter.
        READ TABLE it_parameter INTO ls_parameter
              WITH KEY name = 'BCARIMBO'. " 'Carimbo'.
        IF sy-subrc EQ 0.
          lv_bcarimbo = ls_parameter-value.
        ENDIF.

        CLEAR ls_parameter.
        READ TABLE it_parameter INTO ls_parameter
              WITH KEY name = 'YMATNR'. " 'Ymatnr'.
        IF sy-subrc EQ 0.
*          lv_ymatnr = ls_parameter-value.
          CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
            EXPORTING
              input        = ls_parameter-value
            IMPORTING
              output       = lv_ymatnr
            EXCEPTIONS
              length_error = 1
              OTHERS       = 2.
        ENDIF.

        CLEAR ls_parameter.
        READ TABLE it_parameter INTO ls_parameter
              WITH KEY name = 'HDRPLATE'.
        IF sy-subrc EQ 0.
          lv_hdrplate = ls_parameter-value.
        ENDIF.

        SELECT in_licplate, charg, bin, ymatnr, bcarimbo, trl_plate
          FROM zabs_ilp_btch
          INTO TABLE @DATA(lt_ilp_btch_exit)
         WHERE in_licplate EQ @lv_hdrplate
           AND weighbridge EQ @abap_true
           AND bin         EQ @lv_bin
           AND loevm       EQ @space
           AND transbord   EQ @space.
        IF sy-subrc EQ 0.
          LOOP AT lt_ilp_btch_exit TRANSPORTING NO FIELDS
                                  WHERE bcarimbo NE lv_bcarimbo.
            EXIT.
          ENDLOOP.
          IF sy-subrc EQ 0.
            ls_entity-msgty = 'W'. " 'E'.
            ls_entity-msg = TEXT-002.
            APPEND ls_entity TO lt_entity.
            CLEAR ls_entity.
          ENDIF.

          LOOP AT lt_ilp_btch_exit TRANSPORTING NO FIELDS
                                  WHERE ymatnr NE lv_ymatnr.
            EXIT.
          ENDLOOP.
          IF sy-subrc EQ 0.
            ls_entity-msgty = 'E'. " 'W'.
            ls_entity-msg = TEXT-003.
            APPEND ls_entity TO lt_entity.
            CLEAR ls_entity.
          ENDIF.
        ENDIF.

*        IF ls_entity-msgty IS INITIAL.
        IF lt_entity[] IS INITIAL.
          ls_entity-msgty = 'S'.
          APPEND ls_entity TO lt_entity.
          CLEAR ls_entity.
        ENDIF.

        DESCRIBE TABLE lt_entity LINES lv_count.
        IF lv_count EQ 2.
          DELETE lt_entity WHERE msgty EQ 'W'.
        ENDIF.

* Call method copy_data_to_ref and export entity set data
        copy_data_to_ref( EXPORTING is_data = lt_entity " ls_entity
                CHANGING cr_data = er_data ).


      ENDIF.

    WHEN 'ValidateCarimVarLoader'.

      IF it_parameter IS NOT INITIAL.
        CLEAR ls_parameter.
        READ TABLE it_parameter INTO ls_parameter
              WITH KEY name = 'CARIMBO'. " 'Carimbo'.
        IF sy-subrc EQ 0.
          lv_carimbo = ls_parameter-value.
        ENDIF.

        CLEAR ls_parameter.
        READ TABLE it_parameter INTO ls_parameter
              WITH KEY name = 'YMATNR'. " 'Ymatnr'.
        IF sy-subrc EQ 0.
*          lv_ymatnr = ls_parameter-value.
          CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
            EXPORTING
              input        = ls_parameter-value
            IMPORTING
              output       = lv_ymatnr
            EXCEPTIONS
              length_error = 1
              OTHERS       = 2.
        ENDIF.

        CLEAR ls_parameter.
        READ TABLE it_parameter INTO ls_parameter
              WITH KEY name = 'LICPLATE'.
        IF sy-subrc EQ 0.
          lv_licplate = ls_parameter-value.
        ENDIF.

        SELECT in_licplate, charg, ymatnr, carimbo
          FROM zabs_ilp_btch
          INTO TABLE @DATA(lt_ilp_btch_load)
         WHERE in_licplate EQ @lv_licplate " @lv_hdrplate
           AND weighbridge EQ @space
           AND loevm       EQ @space
           AND transbord   EQ @abap_true.
        IF sy-subrc EQ 0.
          LOOP AT lt_ilp_btch_load TRANSPORTING NO FIELDS
                                  WHERE carimbo NE lv_carimbo.
            EXIT.
          ENDLOOP.
          IF sy-subrc EQ 0.
            ls_entity-msgty = 'W'. " 'E'.
            ls_entity-msg = TEXT-007.
            APPEND ls_entity TO lt_entity.
            CLEAR ls_entity.
          ENDIF.

          LOOP AT lt_ilp_btch_load TRANSPORTING NO FIELDS
                                  WHERE ymatnr NE lv_ymatnr.
            EXIT.
          ENDLOOP.
          IF sy-subrc EQ 0.
            ls_entity-msgty = 'E'. " 'W'.
            ls_entity-msg = TEXT-008.
            APPEND ls_entity TO lt_entity.
            CLEAR ls_entity.
          ENDIF.
        ENDIF.

*        IF ls_entity-msgty IS INITIAL.
        IF lt_entity[] IS INITIAL.
          ls_entity-msgty = 'S'.
          APPEND ls_entity TO lt_entity.
          CLEAR ls_entity.
        ENDIF.

        DESCRIBE TABLE lt_entity LINES lv_count.
        IF lv_count EQ 2.
          DELETE lt_entity WHERE msgty EQ 'W'.
        ENDIF.

* Call method copy_data_to_ref and export entity set data
        copy_data_to_ref( EXPORTING is_data = lt_entity " ls_entity
                CHANGING cr_data = er_data ).
      ENDIF.

  ENDCASE.

ENDMETHOD.


METHOD /iwbep/if_mgw_appl_srv_runtime~get_entityset_delta.

*-- Local Declarations
  DATA: lv_entity_set_name     TYPE string,
        lv_delta_token         TYPE string,
        lo_dp_facade           TYPE REF TO /iwbep/if_mgw_dp_facade,
        lt_turma_head          TYPE zcl_zabs_mobile_acm_mpc=>tt_turmahead,
        lt_turma_head_del      TYPE zcl_zabs_mobile_acm_mpc=>tt_turmahead,
        lt_turma_items         TYPE zcl_zabs_mobile_acm_mpc=>tt_turmaitm,
        lt_turma_items_del     TYPE zcl_zabs_mobile_acm_mpc=>tt_turmaitm,
        lt_leader_farms        TYPE zcl_zabs_mobile_acm_mpc=>tt_leadfarms,
        lt_leader_farms_del    TYPE zcl_zabs_mobile_acm_mpc=>tt_leadfarms,
        lt_farm_terrains       TYPE zcl_zabs_mobile_acm_mpc=>tt_farmterrains,
        lt_farm_terrains_del   TYPE zcl_zabs_mobile_acm_mpc=>tt_farmterrains,
        lt_activities          TYPE zcl_zabs_mobile_acm_mpc=>tt_activities,
        lt_activities_del      TYPE zcl_zabs_mobile_acm_mpc=>tt_activities,
        lt_evejust             TYPE zcl_zabs_mobile_acm_mpc=>tt_evnjust,
        lt_evejust_del         TYPE zcl_zabs_mobile_acm_mpc=>tt_evnjust,
        lt_userlogin           TYPE zcl_zabs_mobile_acm_mpc=>tt_userlogin,
        lt_userlogin_del       TYPE zcl_zabs_mobile_acm_mpc=>tt_userlogin,
*        lt_usrrole           TYPE zcl_zabs_mobile_acm_mpc=>tt_userrole,
*        lt_usrrole_del       TYPE zcl_zabs_mobile_acm_mpc=>tt_userrole,
        lt_intrbatch           TYPE zcl_zabs_mobile_acm_mpc=>tt_intrbatch,
        lt_intrbatch_del       TYPE zcl_zabs_mobile_acm_mpc=>tt_intrbatch,
        lt_accitems            TYPE zcl_zabs_mobile_acm_mpc=>tt_accitems,
        lt_accitems_del        TYPE zcl_zabs_mobile_acm_mpc=>tt_accitems,
        lt_trantyp             TYPE zcl_zabs_mobile_acm_mpc=>tt_trantyp,
        lt_trantyp_del         TYPE zcl_zabs_mobile_acm_mpc=>tt_trantyp,
        lt_baggroup            TYPE zcl_zabs_mobile_acm_mpc=>tt_baggroup,
        lt_baggroup_del        TYPE zcl_zabs_mobile_acm_mpc=>tt_baggroup,
        lt_accbatch            TYPE zcl_zabs_mobile_acm_mpc=>tt_accbatch,
        lt_accbatch_del        TYPE zcl_zabs_mobile_acm_mpc=>tt_accbatch,
*        lt_accrt_elog        TYPE zcl_zabs_mobile_acm_mpc=>tt_acccreateelog,
*        lt_accrt_elog_del    TYPE zcl_zabs_mobile_acm_mpc=>tt_acccreateelog,
*        lt_acconf_elog       TYPE zcl_zabs_mobile_acm_mpc=>tt_accconfelog,
*        lt_acconf_elog_del   TYPE zcl_zabs_mobile_acm_mpc=>tt_accconfelog,
*        lt_accbatch_del      TYPE zcl_zabs_mobile_acm_mpc=>tt_accbatch,
        lt_loadbaggroup        TYPE zcl_zabs_mobile_acm_mpc=>tt_loadbaggroup,
        lt_loadbaggroup_del    TYPE zcl_zabs_mobile_acm_mpc=>tt_loadbaggroup,
        lt_accbaggrpclosed     TYPE zcl_zabs_mobile_acm_mpc=>tt_accbaggrpclosed,
        lt_accbaggrpclosed_del TYPE zcl_zabs_mobile_acm_mpc=>tt_accbaggrpclosed,
        ls_usr_emp             TYPE zabs_usr_emp,
        ls_userlogin           LIKE LINE OF lt_userlogin,
        lt_loader_f4           TYPE zcl_zabs_mobile_acm_mpc=>tt_loader_f4,
        lt_loader_f4_del       TYPE zcl_zabs_mobile_acm_mpc=>tt_loader_f4.

*-- Getting the EntitySet name
  lv_entity_set_name   = io_tech_request_context->get_entity_set_name( ).

  CASE lv_entity_set_name.

    WHEN 'TurmaHeadSet'.

      CALL METHOD me->get_turma_header
        EXPORTING
          io_tech_request_context = io_tech_request_context
        IMPORTING
          et_turma_head           = lt_turma_head.

*      CALL METHOD me->get_turma_header
*        IMPORTING
*          et_turma_head = lt_turma_head.

*-- Get the data provider facade
      TRY.
          lo_dp_facade = /iwbep/if_mgw_conv_srv_runtime~get_dp_facade( ).
        CATCH /iwbep/cx_mgw_tech_exception.
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception.
      ENDTRY.

*-- Call the delta token functionality
      TRY.
          CALL METHOD /iwbep/cl_query_result_log=>create_update_log_entry_hash
            EXPORTING
              io_tech_request_context  = io_tech_request_context
              io_dp_facade             = lo_dp_facade
              ir_service_document_name = mr_service_document_name
              ir_service_version       = mr_service_version
              it_entityset             = lt_turma_head
            IMPORTING
              et_deleted_entityset     = lt_turma_head_del
              et_entityset             = lt_turma_head
            CHANGING
              ev_delta_token           = lv_delta_token.

        CATCH /iwbep/cx_qrl_locked.
          RAISE EXCEPTION TYPE /iwbep/cx_qrl_locked.
        CATCH /iwbep/cx_qrl_delta_unavailabl.
          RAISE EXCEPTION TYPE /iwbep/cx_qrl_delta_unavailabl.
      ENDTRY.

*-- Export the delta token
      es_response_context-deltatoken = lv_delta_token.

*-- Export the deleted entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_turma_head_del
                        CHANGING
                         cr_data = er_deleted_entityset ).

*-- Export the changed entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_turma_head
                        CHANGING
                         cr_data = er_entityset ).

    WHEN 'TurmaItmSet'.

      CALL METHOD me->get_turma_items
        EXPORTING
          io_tech_request_context = io_tech_request_context
        IMPORTING
          et_turma_items          = lt_turma_items.

*      CALL METHOD me->get_turma_items
*        IMPORTING
*          et_turma_items = lt_turma_items.

*-- Get the data provider facade
      TRY.
          lo_dp_facade = /iwbep/if_mgw_conv_srv_runtime~get_dp_facade( ).
        CATCH /iwbep/cx_mgw_tech_exception.
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception.
      ENDTRY.

*-- Call the delta token functionality
      TRY.
          CALL METHOD /iwbep/cl_query_result_log=>create_update_log_entry_hash
            EXPORTING
              io_tech_request_context  = io_tech_request_context
              io_dp_facade             = lo_dp_facade
              ir_service_document_name = mr_service_document_name
              ir_service_version       = mr_service_version
              it_entityset             = lt_turma_items
            IMPORTING
              et_deleted_entityset     = lt_turma_items_del
              et_entityset             = lt_turma_items
            CHANGING
              ev_delta_token           = lv_delta_token.

        CATCH /iwbep/cx_qrl_locked.
          RAISE EXCEPTION TYPE /iwbep/cx_qrl_locked.
        CATCH /iwbep/cx_qrl_delta_unavailabl.
          RAISE EXCEPTION TYPE /iwbep/cx_qrl_delta_unavailabl.
      ENDTRY.

*-- Export the delta token
      es_response_context-deltatoken = lv_delta_token.

*-- Export the deleted entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_turma_items_del
                        CHANGING
                         cr_data = er_deleted_entityset ).

*-- Export the changed entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_turma_items
                        CHANGING
                         cr_data = er_entityset ).

    WHEN 'LeadFarmsSet'.

      CALL METHOD me->get_leader_farms
        EXPORTING
          io_tech_request_context = io_tech_request_context
        IMPORTING
          et_leader_farms         = lt_leader_farms.

*      CALL METHOD me->get_leader_farms
*        IMPORTING
*          et_leader_farms = lt_leader_farms.

*-- Get the data provider facade
      TRY.
          lo_dp_facade = /iwbep/if_mgw_conv_srv_runtime~get_dp_facade( ).
        CATCH /iwbep/cx_mgw_tech_exception.
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception.
      ENDTRY.

*-- Call the delta token functionality
      TRY.
          CALL METHOD /iwbep/cl_query_result_log=>create_update_log_entry_hash
            EXPORTING
              io_tech_request_context  = io_tech_request_context
              io_dp_facade             = lo_dp_facade
              ir_service_document_name = mr_service_document_name
              ir_service_version       = mr_service_version
              it_entityset             = lt_leader_farms
            IMPORTING
              et_deleted_entityset     = lt_leader_farms_del
              et_entityset             = lt_leader_farms
            CHANGING
              ev_delta_token           = lv_delta_token.

        CATCH /iwbep/cx_qrl_locked.
          RAISE EXCEPTION TYPE /iwbep/cx_qrl_locked.
        CATCH /iwbep/cx_qrl_delta_unavailabl.
          RAISE EXCEPTION TYPE /iwbep/cx_qrl_delta_unavailabl.
      ENDTRY.

*-- Export the delta token
      es_response_context-deltatoken = lv_delta_token.

*-- Export the deleted entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_leader_farms_del
                        CHANGING
                         cr_data = er_deleted_entityset ).

*-- Export the changed entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_leader_farms
                        CHANGING
                         cr_data = er_entityset ).

    WHEN 'FarmTerrainsSet'.

      CALL METHOD me->get_leader_farms
        EXPORTING
          io_tech_request_context = io_tech_request_context
        IMPORTING
          et_leader_farms         = lt_leader_farms.

*      CALL METHOD me->get_leader_farms
*        IMPORTING
*          et_leader_farms = lt_leader_farms.

      CALL METHOD me->get_farm_terrains
        EXPORTING
          it_leader_farms  = lt_leader_farms
        IMPORTING
          et_farm_terrains = lt_farm_terrains.

*-- Get the data provider facade
      TRY.
          lo_dp_facade = /iwbep/if_mgw_conv_srv_runtime~get_dp_facade( ).
        CATCH /iwbep/cx_mgw_tech_exception.
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception.
      ENDTRY.

*-- Call the delta token functionality
      TRY.
          CALL METHOD /iwbep/cl_query_result_log=>create_update_log_entry_hash
            EXPORTING
              io_tech_request_context  = io_tech_request_context
              io_dp_facade             = lo_dp_facade
              ir_service_document_name = mr_service_document_name
              ir_service_version       = mr_service_version
              it_entityset             = lt_farm_terrains
            IMPORTING
              et_deleted_entityset     = lt_farm_terrains_del
              et_entityset             = lt_farm_terrains
            CHANGING
              ev_delta_token           = lv_delta_token.

        CATCH /iwbep/cx_qrl_locked.
          RAISE EXCEPTION TYPE /iwbep/cx_qrl_locked.
        CATCH /iwbep/cx_qrl_delta_unavailabl.
          RAISE EXCEPTION TYPE /iwbep/cx_qrl_delta_unavailabl.
      ENDTRY.

*-- Export the delta token
      es_response_context-deltatoken = lv_delta_token.

*-- Export the deleted entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_farm_terrains_del
                        CHANGING
                         cr_data = er_deleted_entityset ).

*-- Export the changed entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_farm_terrains
                        CHANGING
                         cr_data = er_entityset ).

    WHEN 'ActivitiesSet'.

      CALL METHOD me->get_activities
        IMPORTING
          et_activities = lt_activities.

*-- Get the data provider facade
      TRY.
          lo_dp_facade = /iwbep/if_mgw_conv_srv_runtime~get_dp_facade( ).
        CATCH /iwbep/cx_mgw_tech_exception.
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception.
      ENDTRY.

*-- Call the delta token functionality
      TRY.
          CALL METHOD /iwbep/cl_query_result_log=>create_update_log_entry_hash
            EXPORTING
              io_tech_request_context  = io_tech_request_context
              io_dp_facade             = lo_dp_facade
              ir_service_document_name = mr_service_document_name
              ir_service_version       = mr_service_version
              it_entityset             = lt_activities
            IMPORTING
              et_deleted_entityset     = lt_activities_del
              et_entityset             = lt_activities
            CHANGING
              ev_delta_token           = lv_delta_token.

        CATCH /iwbep/cx_qrl_locked.
          RAISE EXCEPTION TYPE /iwbep/cx_qrl_locked.
        CATCH /iwbep/cx_qrl_delta_unavailabl.
          RAISE EXCEPTION TYPE /iwbep/cx_qrl_delta_unavailabl.
      ENDTRY.

*-- Export the delta token
      es_response_context-deltatoken = lv_delta_token.

*-- Export the deleted entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_activities_del
                        CHANGING
                         cr_data = er_deleted_entityset ).

*-- Export the changed entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_activities
                        CHANGING
                         cr_data = er_entityset ).

    WHEN 'EvnJustSet'.

      CALL METHOD me->get_event_justifications
        IMPORTING
          et_evejust = lt_evejust.

*-- Get the data provider facade
      TRY.
          lo_dp_facade = /iwbep/if_mgw_conv_srv_runtime~get_dp_facade( ).
        CATCH /iwbep/cx_mgw_tech_exception.
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception.
      ENDTRY.

*-- Call the delta token functionality
      TRY.
          CALL METHOD /iwbep/cl_query_result_log=>create_update_log_entry_hash
            EXPORTING
              io_tech_request_context  = io_tech_request_context
              io_dp_facade             = lo_dp_facade
              ir_service_document_name = mr_service_document_name
              ir_service_version       = mr_service_version
              it_entityset             = lt_evejust
            IMPORTING
              et_deleted_entityset     = lt_evejust_del
              et_entityset             = lt_evejust
            CHANGING
              ev_delta_token           = lv_delta_token.

        CATCH /iwbep/cx_qrl_locked.
          RAISE EXCEPTION TYPE /iwbep/cx_qrl_locked.
        CATCH /iwbep/cx_qrl_delta_unavailabl.
          RAISE EXCEPTION TYPE /iwbep/cx_qrl_delta_unavailabl.
      ENDTRY.

*-- Export the delta token
      es_response_context-deltatoken = lv_delta_token.

*-- Export the deleted entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_evejust_del
                        CHANGING
                         cr_data = er_deleted_entityset ).

*-- Export the changed entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_evejust
                        CHANGING
                         cr_data = er_entityset ).

*    WHEN 'UserLoginSet'.
*
**-- Get the employee number and role
*      CALL METHOD me->get_employee_role
*        EXPORTING
*          iv_user    = sy-uname
*        IMPORTING
*          es_usr_emp = ls_usr_emp.
*
*      ls_userlogin-bname  = ls_usr_emp-bname.
*      ls_userlogin-urole  = ls_usr_emp-urole.
*      ls_userlogin-pevt   = ls_usr_emp-pevt.
*      ls_userlogin-farm   = ls_usr_emp-farm.
*      APPEND ls_userlogin TO lt_userlogin.
*      CLEAR ls_userlogin.
*
**-- Get the data provider facade
*      TRY.
*          lo_dp_facade = /iwbep/if_mgw_conv_srv_runtime~get_dp_facade( ).
*        CATCH /iwbep/cx_mgw_tech_exception.
*          RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception.
*      ENDTRY.
*
**-- Call the delta token functionality
*      TRY.
*          CALL METHOD /iwbep/cl_query_result_log=>create_update_log_entry_hash
*            EXPORTING
*              io_tech_request_context  = io_tech_request_context
*              io_dp_facade             = lo_dp_facade
*              ir_service_document_name = mr_service_document_name
*              ir_service_version       = mr_service_version
*              it_entityset             = lt_userlogin
*            IMPORTING
*              et_deleted_entityset     = lt_userlogin_del
*              et_entityset             = lt_userlogin
*            CHANGING
*              ev_delta_token           = lv_delta_token.
*
*        CATCH /iwbep/cx_qrl_locked.
*          RAISE EXCEPTION TYPE /iwbep/cx_qrl_locked.
*        CATCH /iwbep/cx_qrl_delta_unavailabl.
*          RAISE EXCEPTION TYPE /iwbep/cx_qrl_delta_unavailabl.
*      ENDTRY.
*
**-- Export the delta token
*      es_response_context-deltatoken = lv_delta_token.
*
**-- Export the deleted entity set
*      copy_data_to_ref( EXPORTING
*                         is_data = lt_userlogin_del
*                        CHANGING
*                         cr_data = er_deleted_entityset ).
*
**-- Export the changed entity set
*      copy_data_to_ref( EXPORTING
*                         is_data = lt_userlogin
*                        CHANGING
*                         cr_data = er_entityset ).

*    WHEN 'UserRoleSet'.
*
**-- Get the employee role
*      CALL METHOD me->get_usr_role
*        EXPORTING
*          io_tech_request_context = io_tech_request_context
*        IMPORTING
*          et_usr_role             = lt_usrrole.
*
**-- Get the data provider facade
*      TRY.
*          lo_dp_facade = /iwbep/if_mgw_conv_srv_runtime~get_dp_facade( ).
*        CATCH /iwbep/cx_mgw_tech_exception.
*          RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception.
*      ENDTRY.
*
**-- Call the delta token functionality
*      TRY.
*          CALL METHOD /iwbep/cl_query_result_log=>create_update_log_entry_hash
*            EXPORTING
*              io_tech_request_context  = io_tech_request_context
*              io_dp_facade             = lo_dp_facade
*              ir_service_document_name = mr_service_document_name
*              ir_service_version       = mr_service_version
*              it_entityset             = lt_usrrole
*            IMPORTING
*              et_deleted_entityset     = lt_usrrole_del
*              et_entityset             = lt_usrrole
*            CHANGING
*              ev_delta_token           = lv_delta_token.
*
*        CATCH /iwbep/cx_qrl_locked.
*          RAISE EXCEPTION TYPE /iwbep/cx_qrl_locked.
*        CATCH /iwbep/cx_qrl_delta_unavailabl.
*          RAISE EXCEPTION TYPE /iwbep/cx_qrl_delta_unavailabl.
*      ENDTRY.
*
**-- Export the delta token
*      es_response_context-deltatoken = lv_delta_token.
*
**-- Export the deleted entity set
*      copy_data_to_ref( EXPORTING
*                         is_data = lt_usrrole_del
*                        CHANGING
*                         cr_data = er_deleted_entityset ).
*
**-- Export the changed entity set
*      copy_data_to_ref( EXPORTING
*                         is_data = lt_usrrole
*                        CHANGING
*                         cr_data = er_entityset ).

    WHEN 'IntrBatchSet'.

*--Fetching Internal batch data
      CALL METHOD me->get_intrbatch
        IMPORTING
          et_intr_batch = lt_intrbatch.

*-- Get the data provider facade
      TRY.
          lo_dp_facade = /iwbep/if_mgw_conv_srv_runtime~get_dp_facade( ).
        CATCH /iwbep/cx_mgw_tech_exception.
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception.
      ENDTRY.

*-- Call the delta token functionality
      TRY.
          CALL METHOD /iwbep/cl_query_result_log=>create_update_log_entry_hash
            EXPORTING
              io_tech_request_context  = io_tech_request_context
              io_dp_facade             = lo_dp_facade
              ir_service_document_name = mr_service_document_name
              ir_service_version       = mr_service_version
              it_entityset             = lt_intrbatch
            IMPORTING
              et_deleted_entityset     = lt_intrbatch_del
              et_entityset             = lt_intrbatch
            CHANGING
              ev_delta_token           = lv_delta_token.

        CATCH /iwbep/cx_qrl_locked.
          RAISE EXCEPTION TYPE /iwbep/cx_qrl_locked.
        CATCH /iwbep/cx_qrl_delta_unavailabl.
          RAISE EXCEPTION TYPE /iwbep/cx_qrl_delta_unavailabl.
      ENDTRY.

*-- Export the delta token
      es_response_context-deltatoken = lv_delta_token.

*-- Export the deleted entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_intrbatch_del
                        CHANGING
                         cr_data = er_deleted_entityset ).

*-- Export the changed entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_intrbatch
                        CHANGING
                         cr_data = er_entityset ).

    WHEN 'AccItemsSet'.

*-- Fetch the farms under leaders
      CALL METHOD me->get_leader_farms
        EXPORTING
          io_tech_request_context = io_tech_request_context
        IMPORTING
          et_leader_farms         = lt_leader_farms.

*      CALL METHOD me->get_leader_farms
*        IMPORTING
*          et_leader_farms = lt_leader_farms.

*-- Fetch the terrains under the farms (1st level terrains)
      CALL METHOD me->get_farm_terrains
        EXPORTING
          it_leader_farms  = lt_leader_farms
        IMPORTING
          et_farm_terrains = lt_farm_terrains.

*-- Fetch the Accomplishment items
      CALL METHOD me->get_accomplishment_items
        EXPORTING
          it_leader_farms  = lt_leader_farms
          it_farm_terrains = lt_farm_terrains
        IMPORTING
          et_accitems      = lt_accitems.

*-- Get the data provider facade
      TRY.
          lo_dp_facade = /iwbep/if_mgw_conv_srv_runtime~get_dp_facade( ).
        CATCH /iwbep/cx_mgw_tech_exception.
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception.
      ENDTRY.

*-- Call the delta token functionality
      TRY.
          CALL METHOD /iwbep/cl_query_result_log=>create_update_log_entry_hash
            EXPORTING
              io_tech_request_context  = io_tech_request_context
              io_dp_facade             = lo_dp_facade
              ir_service_document_name = mr_service_document_name
              ir_service_version       = mr_service_version
              it_entityset             = lt_accitems
            IMPORTING
              et_deleted_entityset     = lt_accitems_del
              et_entityset             = lt_accitems
            CHANGING
              ev_delta_token           = lv_delta_token.

        CATCH /iwbep/cx_qrl_locked.
          RAISE EXCEPTION TYPE /iwbep/cx_qrl_locked.
        CATCH /iwbep/cx_qrl_delta_unavailabl.
          RAISE EXCEPTION TYPE /iwbep/cx_qrl_delta_unavailabl.
      ENDTRY.

*-- Export the delta token
      es_response_context-deltatoken = lv_delta_token.

*-- Export the deleted entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_accitems_del
                        CHANGING
                         cr_data = er_deleted_entityset ).

*-- Export the changed entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_accitems
                        CHANGING
                         cr_data = er_entityset ).

    WHEN 'BagGroupSet'.

      CALL METHOD me->get_baggroup
        EXPORTING
          io_tech_request_context = io_tech_request_context
        IMPORTING
          et_baggroup             = lt_baggroup.

*      CALL METHOD me->get_baggroup
*        IMPORTING
*          et_baggroup = lt_baggroup.

*-- Get the data provider facade
      TRY.
          lo_dp_facade = /iwbep/if_mgw_conv_srv_runtime~get_dp_facade( ).
        CATCH /iwbep/cx_mgw_tech_exception.
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception.
      ENDTRY.

*-- Call the delta token functionality
      TRY.
          CALL METHOD /iwbep/cl_query_result_log=>create_update_log_entry_hash
            EXPORTING
              io_tech_request_context  = io_tech_request_context
              io_dp_facade             = lo_dp_facade
              ir_service_document_name = mr_service_document_name
              ir_service_version       = mr_service_version
              it_entityset             = lt_baggroup
            IMPORTING
              et_deleted_entityset     = lt_baggroup_del
              et_entityset             = lt_baggroup
            CHANGING
              ev_delta_token           = lv_delta_token.

        CATCH /iwbep/cx_qrl_locked.
          RAISE EXCEPTION TYPE /iwbep/cx_qrl_locked.
        CATCH /iwbep/cx_qrl_delta_unavailabl.
          RAISE EXCEPTION TYPE /iwbep/cx_qrl_delta_unavailabl.
      ENDTRY.

*-- Export the delta token
      es_response_context-deltatoken = lv_delta_token.

*-- Export the deleted entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_baggroup_del
                        CHANGING
                         cr_data = er_deleted_entityset ).

*-- Export the changed entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_baggroup
                        CHANGING
                         cr_data = er_entityset ).

    WHEN 'AccBatchSet'.

      CALL METHOD me->get_accbatch
        EXPORTING
          io_tech_request_context = io_tech_request_context
        IMPORTING
          et_accbatch             = lt_accbatch.

*      CALL METHOD me->get_accbatch
*        IMPORTING
*          et_accbatch = lt_accbatch.

*-- Get the data provider facade
      TRY.
          lo_dp_facade = /iwbep/if_mgw_conv_srv_runtime~get_dp_facade( ).
        CATCH /iwbep/cx_mgw_tech_exception.
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception.
      ENDTRY.

*-- Call the delta token functionality
      TRY.
          CALL METHOD /iwbep/cl_query_result_log=>create_update_log_entry_hash
            EXPORTING
              io_tech_request_context  = io_tech_request_context
              io_dp_facade             = lo_dp_facade
              ir_service_document_name = mr_service_document_name
              ir_service_version       = mr_service_version
              it_entityset             = lt_accbatch
            IMPORTING
              et_deleted_entityset     = lt_accbatch_del
              et_entityset             = lt_accbatch
            CHANGING
              ev_delta_token           = lv_delta_token.

        CATCH /iwbep/cx_qrl_locked.
          RAISE EXCEPTION TYPE /iwbep/cx_qrl_locked.
        CATCH /iwbep/cx_qrl_delta_unavailabl.
          RAISE EXCEPTION TYPE /iwbep/cx_qrl_delta_unavailabl.
      ENDTRY.

*-- Export the delta token
      es_response_context-deltatoken = lv_delta_token.

*-- Export the deleted entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_accbatch_del
                        CHANGING
                         cr_data = er_deleted_entityset ).

*-- Export the changed entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_accbatch
                        CHANGING
                         cr_data = er_entityset ).
*
    WHEN 'LoadBagGroupSet'.

      CALL METHOD me->get_loadbaggroup
        EXPORTING
          io_tech_request_context = io_tech_request_context
        IMPORTING
          et_loadbaggroup         = lt_loadbaggroup.

*      CALL METHOD me->get_loadbaggroup
*        IMPORTING
*          et_loadbaggroup = lt_loadbaggroup.

*-- Get the data provider facade
      TRY.
          lo_dp_facade = /iwbep/if_mgw_conv_srv_runtime~get_dp_facade( ).
        CATCH /iwbep/cx_mgw_tech_exception.
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception.
      ENDTRY.

*-- Call the delta token functionality
      TRY.
          CALL METHOD /iwbep/cl_query_result_log=>create_update_log_entry_hash
            EXPORTING
              io_tech_request_context  = io_tech_request_context
              io_dp_facade             = lo_dp_facade
              ir_service_document_name = mr_service_document_name
              ir_service_version       = mr_service_version
              it_entityset             = lt_loadbaggroup
            IMPORTING
              et_deleted_entityset     = lt_loadbaggroup_del
              et_entityset             = lt_loadbaggroup
            CHANGING
              ev_delta_token           = lv_delta_token.

        CATCH /iwbep/cx_qrl_locked.
          RAISE EXCEPTION TYPE /iwbep/cx_qrl_locked.
        CATCH /iwbep/cx_qrl_delta_unavailabl.
          RAISE EXCEPTION TYPE /iwbep/cx_qrl_delta_unavailabl.
      ENDTRY.

*-- Export the delta token
      es_response_context-deltatoken = lv_delta_token.

*-- Export the deleted entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_loadbaggroup_del
                        CHANGING
                         cr_data = er_deleted_entityset ).

*-- Export the changed entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_loadbaggroup
                        CHANGING
                         cr_data = er_entityset ).

    WHEN 'AccBaggrpClosedSet'.

      CALL METHOD me->get_accbaggrpclosed
        EXPORTING
          io_tech_request_context = io_tech_request_context
        IMPORTING
          et_accbaggrpclosed      = lt_accbaggrpclosed.


*-- Get the data provider facade
      TRY.
          lo_dp_facade = /iwbep/if_mgw_conv_srv_runtime~get_dp_facade( ).
        CATCH /iwbep/cx_mgw_tech_exception.
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception.
      ENDTRY.

*-- Call the delta token functionality
      TRY.
          CALL METHOD /iwbep/cl_query_result_log=>create_update_log_entry_hash
            EXPORTING
              io_tech_request_context  = io_tech_request_context
              io_dp_facade             = lo_dp_facade
              ir_service_document_name = mr_service_document_name
              ir_service_version       = mr_service_version
              it_entityset             = lt_accbaggrpclosed
            IMPORTING
              et_deleted_entityset     = lt_accbaggrpclosed_del
              et_entityset             = lt_accbaggrpclosed
            CHANGING
              ev_delta_token           = lv_delta_token.

        CATCH /iwbep/cx_qrl_locked.
          RAISE EXCEPTION TYPE /iwbep/cx_qrl_locked.
        CATCH /iwbep/cx_qrl_delta_unavailabl.
          RAISE EXCEPTION TYPE /iwbep/cx_qrl_delta_unavailabl.
      ENDTRY.

*-- Export the delta token
      es_response_context-deltatoken = lv_delta_token.

*-- Export the deleted entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_accbaggrpclosed_del
                        CHANGING
                         cr_data = er_deleted_entityset ).

*-- Export the changed entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_accbaggrpclosed
                        CHANGING
                         cr_data = er_entityset ).

    WHEN 'TranTypSet'.

      CALL METHOD me->get_trantyp
        IMPORTING
          et_trantyp = lt_trantyp.

*-- Get the data provider facade
      TRY.
          lo_dp_facade = /iwbep/if_mgw_conv_srv_runtime~get_dp_facade( ).
        CATCH /iwbep/cx_mgw_tech_exception.
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception.
      ENDTRY.

*-- Call the delta token functionality
      TRY.
          CALL METHOD /iwbep/cl_query_result_log=>create_update_log_entry_hash
            EXPORTING
              io_tech_request_context  = io_tech_request_context
              io_dp_facade             = lo_dp_facade
              ir_service_document_name = mr_service_document_name
              ir_service_version       = mr_service_version
              it_entityset             = lt_trantyp
            IMPORTING
              et_deleted_entityset     = lt_trantyp_del
              et_entityset             = lt_trantyp
            CHANGING
              ev_delta_token           = lv_delta_token.

        CATCH /iwbep/cx_qrl_locked.
          RAISE EXCEPTION TYPE /iwbep/cx_qrl_locked.
        CATCH /iwbep/cx_qrl_delta_unavailabl.
          RAISE EXCEPTION TYPE /iwbep/cx_qrl_delta_unavailabl.
      ENDTRY.

*-- Export the delta token
      es_response_context-deltatoken = lv_delta_token.

*-- Export the deleted entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_trantyp_del
                        CHANGING
                         cr_data = er_deleted_entityset ).

*-- Export the changed entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_trantyp
                        CHANGING
                         cr_data = er_entityset ).

    WHEN 'Loader_F4Set'.
*local decleration

*-- Fetch the farms under the leaders
      CALL METHOD me->get_leader_farms
        EXPORTING
          io_tech_request_context = io_tech_request_context
        IMPORTING
          et_leader_farms         = lt_leader_farms.

*-- Fetch the Loader under the Farms
      CALL METHOD me->get_farms_loader
        EXPORTING
          it_leader_farms = lt_leader_farms
        IMPORTING
          et_farms_loader = lt_loader_f4. " et_entityset.

*-- Get the data provider facade
      TRY.
          lo_dp_facade = /iwbep/if_mgw_conv_srv_runtime~get_dp_facade( ).
        CATCH /iwbep/cx_mgw_tech_exception.
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception.
      ENDTRY.

*-- Call the delta token functionality
      TRY.
          CALL METHOD /iwbep/cl_query_result_log=>create_update_log_entry_hash
            EXPORTING
              io_tech_request_context  = io_tech_request_context
              io_dp_facade             = lo_dp_facade
              ir_service_document_name = mr_service_document_name
              ir_service_version       = mr_service_version
              it_entityset             = lt_loader_f4
            IMPORTING
              et_deleted_entityset     = lt_loader_f4_del
              et_entityset             = lt_loader_f4
            CHANGING
              ev_delta_token           = lv_delta_token.

        CATCH /iwbep/cx_qrl_locked.
          RAISE EXCEPTION TYPE /iwbep/cx_qrl_locked.
        CATCH /iwbep/cx_qrl_delta_unavailabl.
          RAISE EXCEPTION TYPE /iwbep/cx_qrl_delta_unavailabl.
      ENDTRY.

*-- Export the delta token
      es_response_context-deltatoken = lv_delta_token.

*-- Export the deleted entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_loader_f4_del
                        CHANGING
                         cr_data = er_deleted_entityset ).

*-- Export the changed entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_loader_f4
                        CHANGING
                         cr_data = er_entityset ).

  ENDCASE.

ENDMETHOD.


  METHOD accbaggrpcloseds_create_entity.

*--Workarea declaration
    DATA : ls_bggrp        TYPE zabs_ac_bggrp,
           ls_bggrp_entity TYPE zcl_zabs_mobile_acm_mpc=>ts_accbaggrpclosed,
           ls_message      TYPE /agri/s_gprolog.

    io_data_provider->read_entry_data( IMPORTING es_data = ls_bggrp_entity ).

    SELECT SINGLE *
      FROM zabs_ac_bggrp
      INTO ls_bggrp
     WHERE mackey = ls_bggrp_entity-zzmackey
       AND baggp  = ls_bggrp_entity-zzbaggp.
    IF sy-subrc = 0.
      ls_bggrp-loaded = 'X'.
      ls_bggrp-aenam  = sy-uname.
      ls_bggrp-aedat  = sy-datum.
      ls_bggrp-aezet  = sy-uzeit.
      ls_bggrp-chimei  = ls_bggrp_entity-imei.
      ls_bggrp-chbadge = ls_bggrp_entity-badge.
      UPDATE zabs_ac_bggrp FROM ls_bggrp.

**--Start of QR Code Scan 03.11.2020
    ELSE.
      ls_bggrp-mackey      = ls_bggrp_entity-zzmackey.
      ls_bggrp-baggp       = ls_bggrp_entity-zzbaggp.
*      ls_bggrp-ldcde       = ls_bggrp_entity-lifnr.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = ls_bggrp_entity-lifnr
        IMPORTING
          output = ls_bggrp-ldcde.
      ls_bggrp-loaded      = 'X'.
      ls_bggrp-ersda       = sy-datum.
      ls_bggrp-ertme       = sy-uzeit.
      ls_bggrp-ernam       = sy-uname.
      ls_bggrp-erdat       = sy-datum.
      ls_bggrp-erzet       = sy-uzeit.
      ls_bggrp-crimei      = ls_bggrp_entity-imei.
      ls_bggrp-crbadge     = ls_bggrp_entity-badge.
      ls_bggrp-status      = 'C'.

      CALL FUNCTION 'ENQUEUE_EZABS_AC_BGGRP'
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.
      IF sy-subrc EQ 0.
        INSERT zabs_ac_bggrp FROM ls_bggrp.
      ENDIF.
      CALL FUNCTION 'DEQUEUE_EZABS_AC_BGGRP'.

**-- End of QR Code Scan 03.11.2020

    ENDIF.

*    UPDATE zabs_ac_bggrp FROM ls_bggrp.

    er_entity-zzmackey = ls_bggrp-mackey.
    er_entity-zzbaggp  = ls_bggrp-baggp.

*-- Give Succes message if bag group is closed
    CLEAR ls_message.
    ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_success. "'S'
    ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class. "'ZABS_MSGCLS'
    ls_message-msgno = '116'.
    ls_message-msgv1 = ls_bggrp-baggp.
*-- Add the messages to containersa
    add_messages_to_msg_container( is_message = ls_message ).

  ENDMETHOD.


METHOD accbaggrpcloseds_get_entityset.

  CALL METHOD me->get_accbaggrpclosed
    EXPORTING
      io_tech_request_context = io_tech_request_context
    IMPORTING
      et_accbaggrpclosed      = et_entityset.

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


METHOD accbatchset_create_entity.

*-- Local Declarations
  DATA: lt_messages TYPE bapiret2_tt,
        lt_msg      TYPE /agri/t_gprolog,

*--Workarea declaration
        ls_accbatch TYPE zcl_zabs_mobile_acm_mpc=>ts_accbatch.

  DATA(lo_message_container) = mo_context->get_message_container( ).

  io_data_provider->read_entry_data( IMPORTING es_data = ls_accbatch ).

  IF ls_accbatch-intr_batch IS INITIAL.
*--Call Method for Batch Create
    CALL METHOD me->batch_create
      IMPORTING
        et_messages = lt_messages
      CHANGING
        cs_ac_batch = ls_accbatch.
  ENDIF.

  er_entity = ls_accbatch.

*-- Add the messages to containersa
  add_messages_to_msg_container( iv_exception = abap_true
                                 it_bapi_messages = lt_messages ).

ENDMETHOD.


METHOD accbatchset_get_entityset.

**--Workarea declaration
*  DATA : ls_usr_emp         TYPE zabs_usr_emp,
*         ls_ac_batch        TYPE zabs_s_ac_batch,
*
**--Internal table declaration
*         lt_messages        TYPE bapiret2_tt,
*         lt_allocvaluescurr TYPE tt_bapi1003_alloc_values_curr,
*         lt_allocvaluesnum  TYPE tt_bapi1003_alloc_values_num,
*         lt_allocvalueschar TYPE tt_bapi1003_alloc_values_char,
*         lt_alloclist       TYPE TABLE OF bapi1003_alloc_list,
*
**--Variable declaration
*         lv_objectkey       TYPE bapi1003_key-object_long,
**         lv_classnum        TYPE bapi1003_key-classnum,
*         lv_days            TYPE i.
*
**--Field-symbol declaration
*  FIELD-SYMBOLS : <fs_value>    TYPE any.
*
**--Calling method to get user details
*  CALL METHOD me->get_employee_role
*    EXPORTING
*      iv_user    = sy-uname
*    IMPORTING
*      es_usr_emp = ls_usr_emp.
*
*  IF ls_usr_emp-urole = zcl_abs_abap_maintain=>c_role_loader. "'LD'
*
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*      EXPORTING
*        input  = ls_usr_emp-lifnr
*      IMPORTING
*        output = ls_usr_emp-lifnr.
*
*    CALL METHOD zcl_abs_get_variants=>get_constant_single
*      EXPORTING
*        iv_objid  = zcl_abs_abap_maintain=>c_objid_mobile "'MOBC'
*        iv_k1val  = 'DAYS'
*        iv_k2val  = 'LDAY'
*      IMPORTING
*        ev_cnval1 = lv_days. "'3'
*
*    DATA(lv_bdate) = sy-datum - lv_days.
*
**--Fetching Internal Licence Plate data
*    SELECT *
*      FROM zabs_ilp_btch
*      INTO TABLE @DATA(lt_ilp_btch)
*     WHERE ldcde    EQ @ls_usr_emp-lifnr
*       AND ( ersda  GE @lv_bdate
*       AND   ersda  LE @sy-datum ).
*    IF sy-subrc <> 0.
*      RETURN.
*    ENDIF.
*
*    SORT lt_ilp_btch BY charg.
*    DELETE ADJACENT DUPLICATES FROM lt_ilp_btch COMPARING charg.
*
**--Fetching Batch data from custom table
*    SELECT *
*      FROM zabs_tab_cbch
*      INTO TABLE @DATA(lt_cust_btch)
*     WHERE class = @zcl_abs_abap_maintain=>c_atgrp_fruta_mp. "'Z_FRUTA_MP'
*    IF sy-subrc = 0.
*      SORT lt_cust_btch BY atnam.
*    ENDIF.
*  ENDIF.
*
*  LOOP AT lt_ilp_btch INTO DATA(ls_ilp_btch).
*
*    CLEAR : lv_objectkey.
*    REFRESH : lt_alloclist,
*              lt_allocvalueschar,
*              lt_allocvaluescurr,
*              lt_allocvaluesnum,
*              lt_messages.
*
*    CONCATENATE ls_ilp_btch-tmatnr ls_ilp_btch-charg
*          INTO lv_objectkey RESPECTING BLANKS.
*
**--Fetching Classification BAPI: Create Assignment data
*    CALL FUNCTION 'BAPI_OBJCL_GETCLASSES'
*      EXPORTING
*        objecttable_imp    = 'MCH1'
*        classtype_imp      = '023'
*        read_valuations    = 'X'
*        objectkey_imp_long = lv_objectkey
*      TABLES
*        alloclist          = lt_alloclist
*        allocvalueschar    = lt_allocvalueschar
*        allocvaluescurr    = lt_allocvaluescurr
*        allocvaluesnum     = lt_allocvaluesnum
*        return             = lt_messages.
*
*    LOOP AT lt_allocvalueschar INTO DATA(ls_allocvalueschar).
*      READ TABLE lt_cust_btch INTO DATA(ls_cust_btch)
*                              WITH KEY atnam = ls_allocvalueschar-charact.
*      IF sy-subrc EQ 0.
*        UNASSIGN <fs_value>.
*        ASSIGN COMPONENT ls_cust_btch-field_name OF STRUCTURE ls_ac_batch TO <fs_value>.
*        IF <fs_value> IS ASSIGNED.
*          <fs_value> = ls_allocvalueschar-value_char.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*
*    LOOP AT lt_allocvaluescurr INTO DATA(ls_allocvaluescurr).
*      CLEAR ls_cust_btch.
*      READ TABLE lt_cust_btch INTO ls_cust_btch
*                              WITH KEY atnam = ls_allocvaluescurr-charact.
*      IF sy-subrc EQ 0.
*        UNASSIGN <fs_value>.
*        ASSIGN COMPONENT ls_cust_btch-field_name OF STRUCTURE ls_ac_batch TO <fs_value>.
*        IF <fs_value> IS ASSIGNED.
*          <fs_value> = ls_allocvaluescurr-value_from.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*
*    LOOP AT lt_allocvaluesnum INTO DATA(ls_allocvaluesnum).
*      CLEAR ls_cust_btch.
*      READ TABLE lt_cust_btch INTO ls_cust_btch
*                              WITH KEY atnam = ls_allocvaluesnum-charact.
*      IF sy-subrc EQ 0.
*        UNASSIGN <fs_value>.
*        ASSIGN COMPONENT ls_cust_btch-field_name OF STRUCTURE ls_ac_batch TO <fs_value>.
*        IF <fs_value> IS ASSIGNED.
*          <fs_value> = ls_allocvaluesnum-value_from.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*
*    CONCATENATE ls_ac_batch-farm ls_ac_batch-ftplnr
*           INTO ls_ac_batch-tplnr SEPARATED BY '-'.
*    ls_ac_batch-charg    = ls_ilp_btch-charg.
*    ls_ac_batch-zzbaggp  = ls_ilp_btch-baggp.
*    ls_ac_batch-zzmackey = ls_ilp_btch-mackey.
*    ls_ac_batch-bdate    = ls_ilp_btch-ersda.
*    ls_ac_batch-tmatnr   = ls_ilp_btch-tmatnr.
*    ls_ac_batch-werks    = ls_ilp_btch-werks.
*    ls_ac_batch-ibag_no  = ls_ilp_btch-ibag_no.
*    ls_ac_batch-fbag_no  = ls_ilp_btch-fbag_no.
*    ls_ac_batch-tot_qty  = ls_ilp_btch-tot_qty.
*
*    APPEND ls_ac_batch TO et_entityset.
*    CLEAR ls_ac_batch.
*
*  ENDLOOP.

  CALL METHOD me->get_accbatch
    EXPORTING
      io_tech_request_context = io_tech_request_context
    IMPORTING
      et_accbatch             = et_entityset.

*  CALL METHOD me->get_accbatch
*    IMPORTING
*      et_accbatch = et_entityset.

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


METHOD accbatchupdatese_create_entity.
ENDMETHOD.


METHOD accheaderset_get_entityset.
ENDMETHOD.


METHOD accitemsset_get_entityset.

**-- Fetch the farms under leaders
*  CALL METHOD me->get_leader_farms
*    EXPORTING
*      io_tech_request_context = io_tech_request_context
*    IMPORTING
*      et_leader_farms         = DATA(lt_leader_farms).
*
**  CALL METHOD me->get_leader_farms
**    IMPORTING
**      et_leader_farms = DATA(lt_leader_farms).
*
**-- Fetch the terrains under the farms (1st level terrains)
*  CALL METHOD me->get_farm_terrains
*    EXPORTING
*      it_leader_farms  = lt_leader_farms
*    IMPORTING
*      et_farm_terrains = DATA(lt_farm_terrains).
*
**-- Fetch the Accomplishment items
*  CALL METHOD me->get_accomplishment_items
*    EXPORTING
*      it_leader_farms  = lt_leader_farms
*      it_farm_terrains = lt_farm_terrains
*    IMPORTING
*      et_accitems      = et_entityset.

CALL METHOD me->get_accom_items
  EXPORTING
    io_tech_request_context = io_tech_request_context
  IMPORTING
    et_accitems             = et_entityset.


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


  METHOD accitems_repset_get_entityset.

    CALL METHOD me->get_accom_items_rep
      EXPORTING
        io_tech_request_context = io_tech_request_context
      IMPORTING
        et_accitems             = et_entityset.


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


METHOD accom_create.

  DATA : ls_messages TYPE /agri/s_gprolog,
         ls_temp     TYPE zabs_bgp_accom,
         ls_acc_hdr  TYPE zabs_hdr_accom,
         ls_acc_itm  TYPE zabs_bgp_accom,
         lt_acc_itm  TYPE TABLE OF zabs_bgp_accom,
         lt_acc_hdr  TYPE TABLE OF zabs_hdr_accom,
         ls_hdr_tmp  TYPE zabs_hdr_accom.

  READ TABLE ct_accom INTO DATA(ls_accom_hdr) INDEX 1.
  IF sy-subrc EQ 0.
    SELECT SINGLE *
             FROM zabs_hdr_accom
             INTO ls_acc_hdr
            WHERE pernr    EQ ls_accom_hdr-pernr
              AND zzmackey EQ ls_accom_hdr-zzmackey.
  ENDIF.

*  DATA(lv_posnr) = ls_acc_hdr-lposnr.

  LOOP AT ct_accom INTO DATA(ls_accom).
*    lv_posnr = lv_posnr + 1.

    MOVE-CORRESPONDING ls_accom TO ls_acc_itm.
*    ls_acc_itm-fposnr = ls_accom-posnr.
*    ls_acc_itm-posnr = lv_posnr.
    ls_acc_itm-status = zcl_abs_abap_maintain=>c_status_create.
    APPEND ls_acc_itm TO lt_acc_itm.
  ENDLOOP.

  IF ls_acc_hdr IS INITIAL.
    ls_acc_hdr-pernr    = ls_accom_hdr-pernr.
    ls_acc_hdr-zzmackey = ls_accom_hdr-zzmackey.
    ls_acc_hdr-strtdat  = ls_accom_hdr-strtdat.
*    ls_acc_hdr-lposnr   = lv_posnr.
    ls_acc_hdr-ernam    = sy-uname.
    ls_acc_hdr-erdat    = sy-datum.
    ls_acc_hdr-erzet    = sy-uzeit.
    INSERT zabs_hdr_accom FROM ls_acc_hdr.
*  ELSE.
*    ls_acc_hdr-lposnr   = lv_posnr.
*    ls_acc_hdr-aenam    = sy-uname.
*    ls_acc_hdr-aedat    = sy-datum.
*    ls_acc_hdr-aezet    = sy-uzeit.
*    UPDATE zabs_hdr_accom FROM ls_acc_hdr.
  ENDIF.

  IF lt_acc_itm IS NOT INITIAL.
*    INSERT zabs_bgp_accom FROM TABLE lt_acc_itm.
    MODIFY zabs_bgp_accom FROM TABLE lt_acc_itm.

    IF sy-subrc = 0.
      ls_messages-msgty = zcl_abs_abap_maintain=>c_msgty_success. "'S''S'.
      ls_messages-msgv1 = TEXT-004. "'Accomplishment is scheduled for processing'.
      ls_messages-msgid = '00'.
      ls_messages-msgno = '208'.
      APPEND ls_messages TO et_messages.
    ELSE.
      ls_messages-msgty = zcl_abs_abap_maintain=>c_msgty_error. "'E'.
      ls_messages-msgv1 = TEXT-005. "'Error in Accomplishment processing'.
      ls_messages-msgid = '00'.
      ls_messages-msgno = '208'.
      APPEND ls_messages TO et_messages.
    ENDIF.
  ENDIF.

  COMMIT WORK AND WAIT.

ENDMETHOD.


METHOD activitiesset_get_entityset.

*-- Fetch the Activities for productions and events
  CALL METHOD me->get_activities
    IMPORTING
      et_activities = et_entityset.

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
*         message           = lv_text
          message_container = lo_msg_container.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD baggroupset_create_entity.

*-- Local Declarations
  DATA: ls_messages TYPE /agri/s_gprolog,
        lt_messages TYPE /agri/t_gprolog,
        ls_baggrp   TYPE zcl_zabs_mobile_acm_mpc=>ts_baggroup,
        lt_accom    TYPE /agri/t_fmacom,
        ls_accom    TYPE /agri/s_fmacom,
        lt_acdoc    TYPE /agri/t_fmacs_doc,
        lt_bggrp    TYPE TABLE OF zabs_ac_bggrp.

  DATA(lo_message_container) = mo_context->get_message_container( ).

  io_data_provider->read_entry_data( IMPORTING es_data = ls_baggrp ).

**-- Check loader code already exists for the bag group
*  SELECT SINGLE *
*    FROM zabs_ac_bggrp
*    INTO @DATA(ls_bggrp)
*   WHERE mackey EQ @ls_baggrp-zzmackey
*     AND baggp  EQ @ls_baggrp-zzbaggp.
*  IF sy-subrc EQ 0.
*    er_entity-zzmackey = ls_bggrp-mackey.
*    er_entity-zzbaggp  = ls_bggrp-baggp.
*    er_entity-ldcde    = ls_bggrp-ldcde.
*    CLEAR ls_messages.
*    ls_messages-msgty = zcl_abs_abap_maintain=>c_msgty_success.
*    ls_messages-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
*    ls_messages-msgno = '101'.
*    ls_messages-msgv1 = ls_bggrp-ldcde.
*    ls_messages-msgv2 = ls_bggrp-baggp.
*    add_messages_to_msg_container( iv_exception = abap_true is_message = ls_messages ).
*    RETURN.
*  ENDIF.
*
**-- Fetch the Accomplishment Numbers for the keys
*  SELECT SINGLE accom
*    FROM /agri/fmachdr
*    INTO @DATA(lv_accom)
*   WHERE status   NE @zcl_abs_abap_maintain=>c_ac_status_deleted "'DEL'
*     AND zzmackey EQ @ls_baggrp-zzmackey.
*  IF sy-subrc NE 0.
*    CLEAR ls_messages.
*    ls_messages-msgty = zcl_abs_abap_maintain=>c_msgty_error.
*    ls_messages-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
*    ls_messages-msgno = '103'.
*    ls_messages-msgv1 = ls_baggrp-zzmackey.
*    add_messages_to_msg_container( iv_exception = abap_true is_message = ls_messages ).
*    RETURN.
*  ENDIF.
*
*  CLEAR ls_accom.
*  ls_accom-accom = lv_accom.
*  APPEND ls_accom TO lt_accom.
*
*  IF lt_accom IS NOT INITIAL.
**-- Fetch the Accomplishment data
*    CALL FUNCTION '/AGRI/FMAC_VIEW'
*      EXPORTING
*        it_accom       = lt_accom
*      IMPORTING
*        et_acdoc       = lt_acdoc
*      EXCEPTIONS
*        no_data_exists = 1
*        OTHERS         = 2.
*  ENDIF.
*
*  READ TABLE lt_acdoc INTO DATA(ls_acdoc) INDEX 1.
*  IF sy-subrc EQ 0.
*    LOOP AT ls_acdoc-x-acitm ASSIGNING FIELD-SYMBOL(<fs_acitm>)
*                                 WHERE zzbaggp EQ ls_baggrp-zzbaggp
*                                   AND status  NE 'CNF'.
*      <fs_acitm>-zzchimei  = ls_baggrp-imei.
*      <fs_acitm>-zzchbadge = ls_baggrp-badge.
*    ENDLOOP.
*
*    CALL FUNCTION 'ZABS_FMAC_CONFIRMATION'
*      EXPORTING
*        iv_baggp    = ls_baggrp-zzbaggp
*        is_fmacdoc  = ls_acdoc
*      IMPORTING
*        et_messages = lt_messages.
*  ENDIF.
*
*  READ TABLE lt_messages TRANSPORTING NO FIELDS
*        WITH KEY msgty = zcl_abs_abap_maintain=>c_msgty_error.
*  IF sy-subrc NE 0.
*    ls_bggrp-mackey      = ls_baggrp-zzmackey.
*    ls_bggrp-baggp       = ls_baggrp-zzbaggp.
*    ls_bggrp-zzbagcn     = ls_baggrp-zzbagcn.
*    ls_bggrp-tot_boxes   = ls_baggrp-tot_boxes.
*    ls_bggrp-avg_cxn     = ls_baggrp-avg_cxn.
*    ls_bggrp-ibag_no     = ls_baggrp-ibag_no.
*    ls_bggrp-fbag_no     = ls_baggrp-fbag_no.
*    ls_bggrp-ldcde       = ls_baggrp-ldcde.
*    ls_bggrp-tmatnr      = ls_baggrp-tmatnr.
*    ls_bggrp-aufnr       = ls_baggrp-aufnr.
*    ls_bggrp-zzturma     = ls_baggrp-zzturma.
*    ls_bggrp-tplnr       = ls_baggrp-tplnr.
*    ls_bggrp-werks       = ls_baggrp-werks.
*    ls_bggrp-ersda       = ls_baggrp-ersda. "sy-datum.
*    ls_bggrp-ertme       = ls_baggrp-ertme. "sy-datum.
*    ls_bggrp-lider_turma = ls_baggrp-lider_turma.
*    ls_bggrp-maktx       = ls_baggrp-maktx.
*    ls_bggrp-ymatnr      = ls_baggrp-ymatnr.
*    ls_bggrp-carimbo     = ls_baggrp-carimbo.
*    ls_bggrp-toclhtt     = ls_baggrp-toclhtt.
*    ls_bggrp-ernam       = sy-uname.
*    ls_bggrp-erdat       = sy-datum.
*    ls_bggrp-erzet       = sy-uzeit.
*    ls_bggrp-crimei      = ls_baggrp-imei.
*    ls_bggrp-crbadge     = ls_baggrp-badge.
*
*    CALL FUNCTION 'ENQUEUE_EZABS_AC_BGGRP'
*      EXCEPTIONS
*        foreign_lock   = 1
*        system_failure = 2
*        OTHERS         = 3.
*    IF sy-subrc EQ 0.
*      INSERT zabs_ac_bggrp FROM ls_bggrp.
*      CALL FUNCTION 'DEQUEUE_EZABS_AC_BGGRP'.
*    ENDIF.
*
*    er_entity = ls_baggrp.
*
*    CLEAR ls_messages.
*    ls_messages-msgty = zcl_abs_abap_maintain=>c_msgty_success.
*    ls_messages-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
*    ls_messages-msgno = '102'.
*    ls_messages-msgv1 = ls_baggrp-ldcde.
*    ls_messages-msgv2 = ls_baggrp-zzbaggp.
*    APPEND ls_messages TO lt_messages.
*  ENDIF.
*
**-- Add the messages to containersa
*  add_messages_to_msg_container( iv_exception = abap_true
*                                 it_messages = lt_messages ).


*-- Check loader code already exists for the bag group
  SELECT SINGLE *
    FROM zabs_ac_bggrp
    INTO @DATA(ls_bggrp)
   WHERE mackey EQ @ls_baggrp-zzmackey
     AND baggp  EQ @ls_baggrp-zzbaggp.
  IF sy-subrc EQ 0.
    er_entity-zzmackey = ls_bggrp-mackey.
    er_entity-zzbaggp  = ls_bggrp-baggp.
    er_entity-ldcde    = ls_bggrp-ldcde.
    CLEAR ls_messages.
**    ls_messages-msgty = zcl_abs_abap_maintain=>c_msgty_success.
**    ls_messages-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
**    ls_messages-msgno = '101'.
**    ls_messages-msgv1 = ls_bggrp-ldcde.
**    ls_messages-msgv2 = ls_bggrp-baggp.
**    add_messages_to_msg_container( iv_exception = abap_true is_message = ls_messages ).

**-- Start QR Code Scan 03.11.2020
    ls_bggrp-zzbagcn     = ls_baggrp-zzbagcn.
    ls_bggrp-tot_boxes   = ls_baggrp-tot_boxes.
    ls_bggrp-avg_cxn     = ls_baggrp-avg_cxn.
    ls_bggrp-ibag_no     = ls_baggrp-ibag_no.
    ls_bggrp-fbag_no     = ls_baggrp-fbag_no.
    ls_bggrp-ldcde       = ls_baggrp-ldcde.
    ls_bggrp-tmatnr      = ls_baggrp-tmatnr.
    ls_bggrp-aufnr       = ls_baggrp-aufnr.
    ls_bggrp-zzturma     = ls_baggrp-zzturma.
    ls_bggrp-tplnr       = ls_baggrp-tplnr.
    ls_bggrp-werks       = ls_baggrp-werks.
    ls_bggrp-ersda       = ls_baggrp-ersda. "sy-datum.
    ls_bggrp-ertme       = ls_baggrp-ertme. "sy-datum.
    ls_bggrp-lider_turma = ls_baggrp-lider_turma.
    ls_bggrp-maktx       = ls_baggrp-maktx.
    ls_bggrp-ymatnr      = ls_baggrp-ymatnr.
    ls_bggrp-carimbo     = ls_baggrp-carimbo.
    ls_bggrp-toclhtt     = ls_baggrp-toclhtt.
    ls_bggrp-aenam       = sy-uname.
    ls_bggrp-aedat       = sy-datum.
    ls_bggrp-aezet       = sy-uzeit.
    ls_bggrp-chimei      = ls_baggrp-imei.
    ls_bggrp-chbadge     = ls_baggrp-badge.
*    ls_bggrp-status      = 'C'.
    UPDATE zabs_ac_bggrp FROM ls_bggrp.
    IF sy-subrc = 0.
      ls_messages-msgty = zcl_abs_abap_maintain=>c_msgty_success. "'S''S'.
      ls_messages-msgv1 = TEXT-004.                         "#EC NOTEXT
      ls_messages-msgid = '00'.
      ls_messages-msgno = '208'.
      APPEND ls_messages TO lt_messages.
    ELSE.
      ls_messages-msgty = zcl_abs_abap_maintain=>c_msgty_error. "'E'.
      ls_messages-msgv1 = TEXT-005.                         "#EC NOTEXT
      ls_messages-msgid = '00'.
      ls_messages-msgno = '208'.
      APPEND ls_messages TO lt_messages.
    ENDIF.
    add_messages_to_msg_container( iv_exception = abap_true it_messages = lt_messages ).

**--End QR Code Scan 03.11.2020

    RETURN.
  ENDIF.

  ls_bggrp-mackey      = ls_baggrp-zzmackey.
  ls_bggrp-baggp       = ls_baggrp-zzbaggp.
  ls_bggrp-zzbagcn     = ls_baggrp-zzbagcn.
  ls_bggrp-tot_boxes   = ls_baggrp-tot_boxes.
  ls_bggrp-avg_cxn     = ls_baggrp-avg_cxn.
  ls_bggrp-ibag_no     = ls_baggrp-ibag_no.
  ls_bggrp-fbag_no     = ls_baggrp-fbag_no.
  ls_bggrp-ldcde       = ls_baggrp-ldcde.
  ls_bggrp-tmatnr      = ls_baggrp-tmatnr.
  ls_bggrp-aufnr       = ls_baggrp-aufnr.
  ls_bggrp-zzturma     = ls_baggrp-zzturma.
  ls_bggrp-tplnr       = ls_baggrp-tplnr.
  ls_bggrp-werks       = ls_baggrp-werks.
  ls_bggrp-ersda       = ls_baggrp-ersda. "sy-datum.
  ls_bggrp-ertme       = ls_baggrp-ertme. "sy-datum.
  ls_bggrp-lider_turma = ls_baggrp-lider_turma.
  ls_bggrp-maktx       = ls_baggrp-maktx.
  ls_bggrp-ymatnr      = ls_baggrp-ymatnr.
  ls_bggrp-carimbo     = ls_baggrp-carimbo.
  ls_bggrp-toclhtt     = ls_baggrp-toclhtt.
  ls_bggrp-ernam       = sy-uname.
  ls_bggrp-erdat       = sy-datum.
  ls_bggrp-erzet       = sy-uzeit.
  ls_bggrp-crimei      = ls_baggrp-imei.
  ls_bggrp-crbadge     = ls_baggrp-badge.
  ls_bggrp-status      = 'C'.

  CALL FUNCTION 'ENQUEUE_EZABS_AC_BGGRP'
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc EQ 0.
    INSERT zabs_ac_bggrp FROM ls_bggrp.
    IF sy-subrc = 0.
      ls_messages-msgty = zcl_abs_abap_maintain=>c_msgty_success. "'S''S'.
      ls_messages-msgv1 = TEXT-004.                         "#EC NOTEXT
      ls_messages-msgid = '00'.
      ls_messages-msgno = '208'.
      APPEND ls_messages TO lt_messages.
    ELSE.
      ls_messages-msgty = zcl_abs_abap_maintain=>c_msgty_error. "'E'.
      ls_messages-msgv1 = TEXT-005.                         "#EC NOTEXT
      ls_messages-msgid = '00'.
      ls_messages-msgno = '208'.
      APPEND ls_messages TO lt_messages.
    ENDIF.
    CALL FUNCTION 'DEQUEUE_EZABS_AC_BGGRP'.
  ENDIF.

  add_messages_to_msg_container( iv_exception = abap_true it_messages = lt_messages ).

ENDMETHOD.


METHOD baggroupset_get_entityset.

*  DATA: lt_ac_bggrp TYPE STANDARD TABLE OF zabs_ac_bggrp,
*        ls_entity   TYPE zcl_zabs_mobile_acm_mpc=>ts_baggroup.
*
**-- Fetch the farms under leaders
*  CALL METHOD me->get_leader_farms
*    IMPORTING
*      et_leader_farms = DATA(lt_leader_farms).
*
**-- Fetch the terrains under the farms (1st level terrains)
*  CALL METHOD me->get_farm_terrains
*    EXPORTING
*      it_leader_farms  = lt_leader_farms
*    IMPORTING
*      et_farm_terrains = DATA(lt_farm_terrains).
*
**-- Fetch the Accomplishment items
*  CALL METHOD me->get_accomplishment_items
*    EXPORTING
*      it_leader_farms  = lt_leader_farms
*      it_farm_terrains = lt_farm_terrains
*    IMPORTING
*      et_accitems      = DATA(lt_accitems).
*
*  SORT lt_accitems BY zzmackey zzbaggp.
*  DELETE ADJACENT DUPLICATES FROM lt_accitems COMPARING zzmackey zzbaggp.
*
*  IF lt_accitems IS NOT INITIAL.
*    SELECT * "mackey baggp ldcde
*      FROM zabs_ac_bggrp
*      INTO TABLE lt_ac_bggrp "et_entityset
*       FOR ALL ENTRIES IN lt_accitems
*     WHERE mackey EQ lt_accitems-zzmackey
*       AND baggp  EQ lt_accitems-zzbaggp.
**       AND loaded EQ space.
*    IF sy-subrc EQ 0.
*      LOOP AT lt_ac_bggrp INTO DATA(ls_ac_bggrp).
*        CLEAR ls_entity.
*        MOVE-CORRESPONDING ls_ac_bggrp TO ls_entity.
*        ls_entity-zzmackey = ls_ac_bggrp-mackey.
*        ls_entity-zzbaggp = ls_ac_bggrp-baggp.
*        APPEND ls_entity TO et_entityset.
*      ENDLOOP.
*    ENDIF.
*  ENDIF.

  CALL METHOD me->get_baggroup
    EXPORTING
      io_tech_request_context = io_tech_request_context
    IMPORTING
      et_baggroup             = et_entityset.

*  CALL METHOD me->get_baggroup
*    IMPORTING
*      et_baggroup = et_entityset.

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


METHOD batch_create.

*--Local Variables
  DATA: lv_charg           TYPE charg_d,
        lv_subrc           TYPE sysubrc,
        lv_objectkey       TYPE bapi1003_key-object_long,
        lv_classnum        TYPE bapi1003_key-classnum,
        lv_float           TYPE cawn-atflv,
        lv_atwrt           TYPE cawn-atwrt,
        lv_charg_mask      TYPE charg_d VALUE 'ZZ%',
        lv_charg_next      TYPE charg_d,

*--Internal Tables
        lt_allocvaluescurr TYPE tt_bapi1003_alloc_values_curr,
        lt_allocvaluesnum  TYPE tt_bapi1003_alloc_values_num,
        lt_allocvalueschar TYPE tt_bapi1003_alloc_values_char,
        lt_ilp_btch        TYPE TABLE OF zabs_ilp_btch,
        lt_char            TYPE STANDARD TABLE OF api_char,
        lt_messages        TYPE bapiret2_tt,
        ls_bggrp           TYPE zabs_ac_bggrp,
        lt_tplnr           TYPE /agri/t_gltplnr,
        lt_carimbo         TYPE tt_carimbo,
        lt_update          TYPE TABLE OF zabs_ilp_btch,

*--Workarea declaration
        ls_messages        TYPE bapiret2,
        ls_ilp_btch        TYPE zabs_ilp_btch,
        ls_allocvaluesnum  TYPE bapi1003_alloc_values_num,
        ls_allocvalueschar TYPE bapi1003_alloc_values_char,
        ls_allocvaluescurr TYPE bapi1003_alloc_values_curr,
        ls_ac_batch        TYPE zabs_s_ac_batch,
        ls_tplnr           TYPE /agri/s_gltplnr.

*--Field-symbol declaration
  FIELD-SYMBOLS : <fv_value> TYPE any.

  ls_ac_batch = cs_ac_batch.

  "---------------------------------------Wave 3
  IF ls_ac_batch-werks IS INITIAL
 AND ls_ac_batch-tplnr IS NOT INITIAL.

    SELECT SINGLE swerk FROM /agri/glflot
      INTO ls_ac_batch-werks
     WHERE tplnr_fl = ls_ac_batch-tplnr.

  ENDIF.

*-- Fetch Carimbo and description
  CLEAR ls_tplnr. REFRESH lt_tplnr.
  ls_tplnr-tplnr_fl = ls_ac_batch-tplnr.
  APPEND ls_tplnr TO lt_tplnr.

  CALL METHOD me->fetch_carimbo
    EXPORTING
      it_tplnr   = lt_tplnr
    CHANGING
      ct_carimbo = lt_carimbo.

*-- Read Carimbo and description
  READ TABLE lt_carimbo INTO DATA(ls_carimbo)
        WITH KEY tplnr = ls_ac_batch-tplnr
      BINARY SEARCH.

  SELECT *
   FROM zabs_ilp_btch
   INTO TABLE @DATA(lt_ilp_btch_lod)
*     FOR ALL ENTRIES IN @it_ac_batch
  WHERE in_licplate EQ @ls_ac_batch-in_licplate
    AND bin         EQ @space " EQ @ls_ac_batch-bin
    AND weighbridge EQ @space
    AND loevm       EQ @space
    AND transbord   EQ @abap_true.
  IF sy-subrc EQ 0.
    SORT lt_ilp_btch_lod BY in_licplate carimbo.
  ENDIF.

  SELECT tpclht, toclhtt
    FROM yotpclht
    INTO TABLE @DATA(lt_yotpclht).
  IF sy-subrc = 0 .
    SORT lt_yotpclht BY tpclht.
  ENDIF.

  "---------------------------------------Wave 3

  CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
    EXPORTING
      input  = ls_ac_batch-tplnr
    IMPORTING
      output = ls_ac_batch-farm.

  SPLIT ls_ac_batch-farm AT '-' INTO ls_ac_batch-farm ls_ac_batch-ftplnr.

  SELECT charg
    FROM mch1
    INTO TABLE @DATA(lt_next_charg)
   WHERE charg LIKE @lv_charg_mask.

  IF sy-subrc EQ 0.
    DELETE lt_next_charg WHERE charg+2(8) CN '0123456789'.
  ENDIF.

  SORT lt_next_charg BY charg DESCENDING.
  READ TABLE lt_next_charg INTO DATA(ls_next_charg) INDEX 1.
  IF sy-subrc EQ 0.
    lv_charg_next = ls_next_charg.
  ENDIF.

  IF lv_charg_next IS INITIAL.
    lv_charg_next = 'ZZ'.
  ENDIF.

  DATA(lv_number) = lv_charg_next+2(8).
  IF lv_number CO ' 0123456789'.
    ADD 1 TO lv_number.
    UNPACK lv_number TO lv_number.
    lv_charg_next = 'ZZ' && lv_number.
  ENDIF.

*  CALL FUNCTION 'BAPI_BATCH_CREATE'
*    EXPORTING
*      batch         = ls_ac_batch-charg
*      plant         = ls_ac_batch-werks
*      material_long = ls_ac_batch-tmatnr
*    IMPORTING
*      batch         = lv_charg
*    TABLES
*      return        = lt_messages.
  IF ls_ac_batch-charg IS NOT INITIAL.
    CALL FUNCTION 'BAPI_BATCH_CREATE'
      EXPORTING
        batch         = ls_ac_batch-charg
        plant         = ls_ac_batch-werks
        material_long = ls_ac_batch-tmatnr
      IMPORTING
        batch         = lv_charg
      TABLES
        return        = lt_messages.
  ELSE.
    CALL FUNCTION 'BAPI_BATCH_CREATE'
      EXPORTING
        batch         = lv_charg_next
        plant         = ls_ac_batch-werks
        material_long = ls_ac_batch-tmatnr
      IMPORTING
        batch         = lv_charg
      TABLES
        return        = lt_messages.
  ENDIF.

  READ TABLE lt_messages INTO ls_messages
                         WITH KEY type = zcl_abs_abap_maintain=>c_msgty_error. "'E'
  IF sy-subrc EQ 0.
    APPEND LINES OF lt_messages TO et_messages.
    EXIT.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

    IF cs_ac_batch-charg IS INITIAL.
      cs_ac_batch-charg = lv_charg.
    ENDIF.

*--Calling FM to get Characteristics
    CALL FUNCTION 'VBWS_MARA_CLASSIFICATION_GET'
      EXPORTING
        i_matnr              = ls_ac_batch-tmatnr
      IMPORTING
        e_class              = lv_classnum
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
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF lt_char IS NOT INITIAL.

*--Fetching Characteristic data
      SELECT atnam,
             atfor
        FROM cabn
        INTO TABLE @DATA(lt_cabn)
         FOR ALL ENTRIES IN @lt_char
       WHERE atinn = @lt_char-atinn.
      IF sy-subrc = 0.
        SORT lt_cabn BY atnam.
      ENDIF.

*--Fetching Batch data from custom table
      SELECT *
        FROM zabs_tab_cbch
        INTO TABLE @DATA(lt_cust_btch)
         FOR ALL ENTRIES IN @lt_char
       WHERE class = @lv_classnum
         AND atnam = @lt_char-atnam.
      IF sy-subrc = 0.
        SORT lt_cust_btch BY atnam.
      ENDIF.

    ENDIF.

    CONCATENATE ls_ac_batch-tmatnr lv_charg "ls_ac_batch-charg
    INTO lv_objectkey RESPECTING BLANKS.

*--Filling Characteristic values based on data type
    LOOP AT lt_char INTO DATA(ls_char).

      READ TABLE lt_cust_btch INTO DATA(ls_cust_btch)
        WITH KEY atnam = ls_char-atnam
        BINARY SEARCH.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      READ TABLE lt_cabn INTO DATA(ls_cabn)
        WITH KEY atnam = ls_cust_btch-atnam
        BINARY SEARCH.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      ASSIGN COMPONENT ls_cust_btch-field_name OF STRUCTURE ls_ac_batch TO <fv_value>.
      IF <fv_value> IS ASSIGNED AND <fv_value> IS NOT INITIAL.
        CASE ls_cabn-atfor.
          WHEN zcl_abs_abap_maintain=>c_char_datatyp_char. "'CHAR'
            CLEAR ls_allocvalueschar.
            ls_allocvalueschar-charact    = ls_char-atnam.
            ls_allocvalueschar-value_char = <fv_value>.
            APPEND ls_allocvalueschar TO lt_allocvalueschar.
          WHEN zcl_abs_abap_maintain=>c_char_datatyp_curr. "'CURR'
            CLEAR ls_allocvaluescurr.
            ls_allocvaluescurr-charact    = ls_char-atnam.
            ls_allocvaluescurr-value_from = <fv_value>.
            APPEND ls_allocvaluescurr TO lt_allocvaluescurr.
          WHEN zcl_abs_abap_maintain=>c_char_datatyp_date. "'DATE'
*--Calling FM to convert date to float format
            CLEAR : ls_allocvaluesnum,
                    lv_atwrt.
            lv_atwrt = <fv_value>.
            CALL FUNCTION 'CTCV_CONVERT_DATE_TO_FLOAT'
              EXPORTING
*               date  = <fv_value>
                date  = lv_atwrt
              IMPORTING
                float = lv_float.
            ls_allocvaluesnum-value_from = lv_float.
            ls_allocvaluesnum-charact    = ls_char-atnam.
            APPEND ls_allocvaluesnum TO lt_allocvaluesnum.
          WHEN OTHERS.
            CLEAR ls_allocvaluesnum.
            ls_allocvaluesnum-charact    = ls_char-atnam.
            ls_allocvaluesnum-value_from = <fv_value>.
            APPEND ls_allocvaluesnum TO lt_allocvaluesnum.
        ENDCASE.
      ENDIF.
    ENDLOOP.

*--Fetching Classification BAPI: Create Assignment data
    REFRESH lt_messages.
    CALL FUNCTION 'BAPI_OBJCL_CREATE'
      EXPORTING
        objecttablenew    = 'MCH1'
        classnumnew       = lv_classnum
        classtypenew      = '023'
        objectkeynew_long = lv_objectkey
      TABLES
        allocvaluesnum    = lt_allocvaluesnum
        allocvalueschar   = lt_allocvalueschar
        allocvaluescurr   = lt_allocvaluescurr
        return            = lt_messages.

    CLEAR ls_messages.
    READ TABLE lt_messages INTO ls_messages
                          WITH KEY type = zcl_abs_abap_maintain=>c_msgty_error. "'E'
    IF sy-subrc EQ 0.
      APPEND LINES OF lt_messages TO et_messages.
      EXIT.
    ELSE.

      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
        EXPORTING
          input      = ls_ac_batch-farm
        IMPORTING
          output     = ls_ac_batch-farm
        EXCEPTIONS
          not_found  = 1
          not_active = 2
          OTHERS     = 3.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      ls_ilp_btch-in_licplate = ls_ac_batch-in_licplate.
      ls_ilp_btch-charg       = lv_charg.
      ls_ilp_btch-farm        = ls_ac_batch-farm.
      ls_ilp_btch-weighbridge = ''.
      ls_ilp_btch-transbord   = ls_ac_batch-trans_bord.
      ls_ilp_btch-tmatnr      = ls_ac_batch-tmatnr.
      ls_ilp_btch-ersda       = ls_ac_batch-ersda. "sy-datum.
      ls_ilp_btch-ertme       = ls_ac_batch-ertme. "sy-datum.
      ls_ilp_btch-zzturma     = ls_ac_batch-zzturma.
      ls_ilp_btch-tplnr       = ls_ac_batch-tplnr.
      ls_ilp_btch-bin         = ls_ac_batch-bin.
      ls_ilp_btch-baggp       = ls_ac_batch-zzbaggp.
      ls_ilp_btch-mackey      = ls_ac_batch-zzmackey.
      ls_ilp_btch-ldcde       = ls_ac_batch-ldcde.
      ls_ilp_btch-werks       = ls_ac_batch-werks.
      ls_ilp_btch-ibag_no     = ls_ac_batch-ibag_no.
      ls_ilp_btch-fbag_no     = ls_ac_batch-fbag_no.
      ls_ilp_btch-tot_qty     = ls_ac_batch-tot_qty.
      ls_ilp_btch-maktx       = ls_ac_batch-maktx.
      ls_ilp_btch-ymatnr      = ls_ac_batch-ymatnr.
      ls_ilp_btch-carimbo     = ls_ac_batch-carimbo.
      READ TABLE lt_yotpclht INTO DATA(ls_yotpclht)
                    WITH KEY tpclht = ls_ac_batch-carimbo BINARY SEARCH.
      IF sy-subrc = 0.
        ls_ilp_btch-toclhtt    = ls_yotpclht-toclhtt.
      ENDIF.

      ls_ilp_btch-bcarimbo    = ls_carimbo-carimbo. " ls_ac_batch-bcarimbo.
      CLEAR ls_yotpclht.
      READ TABLE lt_yotpclht INTO ls_yotpclht
                     WITH KEY tpclht = ls_carimbo-carimbo BINARY SEARCH.
      IF sy-subrc = 0.
        ls_ilp_btch-btoclhtt    = ls_yotpclht-toclhtt." ls_ac_batch-btoclhtt
      ENDIF.
      ls_ilp_btch-bdate       = ls_ac_batch-bdate.
      ls_ilp_btch-btime       = ls_ac_batch-btime.
      ls_ilp_btch-ernam       = sy-uname.
      ls_ilp_btch-erdat       = sy-datum.
      ls_ilp_btch-erzet       = sy-uzeit.
      ls_ilp_btch-crimei      = ls_ac_batch-imei.
      ls_ilp_btch-crbadge     = ls_ac_batch-badge.

      IF ls_ac_batch-bin IS NOT INITIAL.
*-- Batch is creating from BIN (Loader offline)
        ls_ilp_btch-bedate    = sy-datum.
        ls_ilp_btch-betime    = sy-uzeit.
        ls_ilp_btch-chimei    = ls_ac_batch-imei.
        ls_ilp_btch-chbadge   = ls_ac_batch-badge.
      ELSE.
        ls_ilp_btch-crimei    = ls_ac_batch-imei.
        ls_ilp_btch-crbadge   = ls_ac_batch-badge.
      ENDIF.

      APPEND ls_ilp_btch TO lt_ilp_btch.
      CLEAR ls_ilp_btch.

      IF lt_ilp_btch IS  NOT INITIAL.
        INSERT zabs_ilp_btch FROM TABLE lt_ilp_btch.
      ENDIF.

*----------------------------Wave 3
      REFRESH lt_update[].
      IF ls_ac_batch-carimbo = '23'.
        LOOP AT lt_ilp_btch_lod INTO DATA(ls_ilp_btch_lod)
                      WHERE in_licplate = ls_ac_batch-in_licplate
                        AND carimbo NE ls_ac_batch-carimbo.
          READ TABLE lt_yotpclht INTO ls_yotpclht
                   WITH KEY tpclht = ls_ac_batch-carimbo BINARY SEARCH.

          ls_ilp_btch_lod-carimbo = ls_ac_batch-carimbo.
          ls_ilp_btch_lod-toclhtt = ls_yotpclht-toclhtt. " ls_ac_batch-toclhtt.

          ls_ilp_btch_lod-aenam = sy-uname.
          ls_ilp_btch_lod-aedat = sy-datum.
          ls_ilp_btch_lod-aezet = sy-uzeit.
          ls_ilp_btch_lod-chimei  = ls_ac_batch-imei.
          ls_ilp_btch_lod-chbadge = ls_ac_batch-badge.
          APPEND ls_ilp_btch_lod TO lt_update.
          CLEAR: ls_ilp_btch_lod, ls_yotpclht.
        ENDLOOP.
      ENDIF.
      IF lt_update IS NOT INITIAL.
        UPDATE zabs_ilp_btch FROM TABLE lt_update.
      ENDIF.
*----------------------------Wave 3

*-- Update batch created flag when batch is created for that group
      SELECT SINGLE *
        INTO @DATA(ls_bggrp_btcrt)
        FROM zabs_ac_bggrp
       WHERE mackey EQ @ls_ac_batch-zzmackey
         AND baggp  EQ @ls_ac_batch-zzbaggp.
      IF sy-subrc EQ 0 AND ls_bggrp_btcrt-btcrt IS INITIAL.
        ls_bggrp_btcrt-btcrt = abap_true.
        ls_bggrp_btcrt-aenam = sy-uname.
        ls_bggrp_btcrt-aedat = sy-datum.
        ls_bggrp_btcrt-aezet = sy-uzeit.
        ls_bggrp_btcrt-chimei  = ls_ac_batch-imei.
        ls_bggrp_btcrt-chbadge = ls_ac_batch-badge.
        UPDATE zabs_ac_bggrp FROM ls_bggrp_btcrt.
      ENDIF.

      CLEAR ls_messages.
      ls_messages-id = zcl_abs_abap_maintain=>c_custom_msg_class.
      ls_messages-number = '113'.
      ls_messages-type = zcl_abs_abap_maintain=>c_msgty_success. "'S'
      APPEND ls_messages TO et_messages.

    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD batch_extract.

*--Internal table declaration
  DATA : lt_val_tab  TYPE TABLE OF api_vali,
         lt_att_tab  TYPE TABLE OF api_ch_att,

*--Local variable declaration
         lv_classnum TYPE bapi1003_key-classnum,

*--Workarea declaration
         ls_val_tab  TYPE api_vali,
         ls_att_tab  TYPE api_ch_att.
  DATA : lt_ac_btch TYPE TABLE OF zabs_s_ac_btch,
         ls_ac_btch TYPE zabs_s_ac_btch.

*--Field-symbol declaration
  FIELD-SYMBOLS : <fs_value> TYPE any.

*--Fetching batch characteristics data
  CALL FUNCTION 'QC01_BATCH_VALUES_READ'
    EXPORTING
      i_val_matnr    = iv_tmatnr
      i_val_charge   = iv_charg
    IMPORTING
      e_class        = lv_classnum
    TABLES
      t_val_tab      = lt_val_tab
      t_att_tab      = lt_att_tab
    EXCEPTIONS
      no_class       = 1
      internal_error = 2
      no_values      = 3
      no_chars       = 4
      OTHERS         = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    RETURN.
  ENDIF.

*--Fetching Batch data from custom table
  SELECT *
    FROM zabs_tab_cbch
    INTO TABLE @DATA(lt_cust_btch)
   WHERE class = @lv_classnum.
  IF sy-subrc = 0.
    SORT lt_cust_btch BY atnam.
  ENDIF.

  LOOP AT lt_val_tab INTO ls_val_tab.

    CLEAR ls_att_tab.
    READ TABLE lt_att_tab INTO ls_att_tab WITH KEY atnam = ls_val_tab-atnam.
    IF sy-subrc = 0.
      READ TABLE lt_cust_btch INTO DATA(ls_cust_btch)
            WITH KEY atnam = ls_val_tab-atnam.
      IF sy-subrc EQ 0.
        ASSIGN COMPONENT ls_cust_btch-field_name OF STRUCTURE es_ac_batch TO <fs_value>.
        IF <fs_value> IS ASSIGNED.
          IF ls_att_tab-atfor = zcl_abs_abap_maintain=>c_char_datatyp_char. "'CHAR'
            <fs_value> = ls_val_tab-atwrt.
          ELSE.
            <fs_value> = ls_val_tab-atflv.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDMETHOD.


METHOD batch_update.

*--Local Variables
  DATA: lv_charg           TYPE charg_d,
        lv_objectkey       TYPE bapi1003_key-object_long,
        lv_classnum        TYPE bapi1003_key-classnum,
        lv_float           TYPE cawn-atflv,

*--Internal Tables
        lt_allocvaluescurr TYPE tt_bapi1003_alloc_values_curr,
        lt_allocvaluesnum  TYPE tt_bapi1003_alloc_values_num,
        lt_allocvalueschar TYPE tt_bapi1003_alloc_values_char,
        lt_btch            TYPE TABLE OF ty_btch,
        lt_update          TYPE TABLE OF zabs_ilp_btch,
        lt_insert          TYPE TABLE OF zabs_ilp_btch,
        lt_ilp_btch        TYPE TABLE OF zabs_ilp_btch,
        lt_alloclist       TYPE TABLE OF bapi1003_alloc_list,

*--Workarea declaration
        ls_messages        TYPE bapiret2,
        ls_ac_batch        TYPE zabs_s_ac_batch,
        ls_btch            TYPE ty_btch,
        ls_ilp_btch        TYPE zabs_ilp_btch,
        ls_insert          TYPE zabs_ilp_btch,
        ls_update          TYPE zabs_ilp_btch,
        ls_allocvalueschar TYPE bapi1003_alloc_values_char,
        ls_allocvaluesnum  TYPE bapi1003_alloc_values_num,
        ls_allocvaluescurr TYPE bapi1003_alloc_values_curr,
        ls_alloclist       TYPE bapi1003_alloc_list.

*--Field-symbol declaration
  FIELD-SYMBOLS :    <fv_value> TYPE any.

  LOOP AT it_ac_batch INTO ls_ac_batch.
    ls_btch-in_licplate = ls_ac_batch-in_licplate.
    ls_btch-charg       = ls_ac_batch-charg.
    APPEND ls_btch TO lt_btch.
    CLEAR ls_btch.
    ls_btch-in_licplate = ls_ac_batch-hdr_plate.
    ls_btch-charg       = ls_ac_batch-charg.
    APPEND ls_btch TO lt_btch.
    CLEAR ls_btch.
  ENDLOOP.

  IF lt_btch IS NOT INITIAL.
    SELECT *
      FROM zabs_ilp_btch
      INTO TABLE  lt_ilp_btch
      FOR ALL ENTRIES IN lt_btch
    WHERE in_licplate EQ lt_btch-in_licplate
      AND charg       EQ lt_btch-charg
      AND weighbridge EQ space
      AND loevm       EQ space
      AND transbord   EQ space.
    IF sy-subrc EQ 0.
      SORT lt_ilp_btch BY in_licplate charg.
    ENDIF.
  ENDIF.

***--------------------------Wave 3 changes

  SELECT tpclht, toclhtt
    FROM yotpclht
    INTO TABLE @DATA(lt_yotpclht).
  IF sy-subrc = 0 .
    SORT lt_yotpclht BY tpclht.
  ENDIF.

  CLEAR ls_ac_batch.
  READ TABLE it_ac_batch INTO ls_ac_batch INDEX 1.
  IF ls_ac_batch-exit IS INITIAL. "--BIN Entrance

    SELECT *
      FROM zabs_ilp_btch
      INTO TABLE @DATA(lt_ilp_btch_ben)
       FOR ALL ENTRIES IN @it_ac_batch
     WHERE " in_licplate EQ @it_ac_batch-in_licplate AND
           bin         EQ @it_ac_batch-bin
       AND weighbridge EQ @space
       AND loevm       EQ @space
       AND transbord   EQ @space.
    IF sy-subrc EQ 0.
      SORT lt_ilp_btch_ben BY in_licplate bin bcarimbo.
    ENDIF.

  ELSEIF ls_ac_batch-exit IS NOT INITIAL. "--BIN Exit
    SELECT *
      FROM zabs_ilp_btch
      INTO TABLE @DATA(lt_ilp_btch_bex)
       FOR ALL ENTRIES IN @it_ac_batch
     WHERE in_licplate EQ @it_ac_batch-hdr_plate
       AND bin         NE @space
       AND weighbridge EQ @abap_true
       AND loevm       EQ @space
       AND transbord   EQ @space.
    IF sy-subrc EQ 0.
      SORT lt_ilp_btch_bex BY in_licplate bin bcarimbo.
    ENDIF.
  ENDIF.

***--------------------------Wave 3 changes

  CLEAR ls_ac_batch.
  LOOP AT it_ac_batch INTO ls_ac_batch.

*--Calling FM to get Characteristics
    CALL FUNCTION 'VBWS_MARA_CLASSIFICATION_GET'
      EXPORTING
        i_matnr              = ls_ac_batch-tmatnr
      IMPORTING
        e_class              = lv_classnum
      EXCEPTIONS
        classtype_not_found  = 1
        classtype_not_active = 2
        class_not_found      = 3
        no_allocations       = 4
        characters_not_found = 5
        OTHERS               = 6.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    CONCATENATE ls_ac_batch-tmatnr ls_ac_batch-charg
    INTO lv_objectkey RESPECTING BLANKS.

    CALL FUNCTION 'BAPI_OBJCL_GETCLASSES'
      EXPORTING
        objecttable_imp    = 'MCH1'
        classtype_imp      = '023'
        read_valuations    = 'X'
        objectkey_imp_long = lv_objectkey
      TABLES
        alloclist          = lt_alloclist
        allocvalueschar    = lt_allocvalueschar
        allocvaluescurr    = lt_allocvaluescurr
        allocvaluesnum     = lt_allocvaluesnum
        return             = et_messages.

    IF ls_ac_batch-exit IS INITIAL.
      IF ls_ac_batch-bin IS NOT INITIAL.
        CLEAR ls_allocvalueschar.
        ls_allocvalueschar-charact    = 'ABS_BIN'.
        ls_allocvalueschar-value_char = ls_ac_batch-bin.
        APPEND ls_allocvalueschar TO lt_allocvalueschar.
      ENDIF.

    ELSE.

*      IF ls_ac_batch-ext_tr_prov IS NOT INITIAL.
*        CLEAR ls_allocvalueschar.
*        ls_allocvalueschar-charact    = 'ABS_PLACA_SERV_INT'.
*        ls_allocvalueschar-value_char = ls_ac_batch-ext_tr_prov.
*        APPEND ls_allocvalueschar TO lt_allocvalueschar.
*      ENDIF.

      IF ls_ac_batch-hdr_plate IS NOT INITIAL.
        CLEAR ls_allocvalueschar.
        ls_allocvalueschar-charact    = 'RC_FR_PLACA_CM'.
        ls_allocvalueschar-value_char = ls_ac_batch-hdr_plate.
        APPEND ls_allocvalueschar TO lt_allocvalueschar.
      ENDIF.

      IF ls_ac_batch-trl_plate IS NOT INITIAL.
        CLEAR ls_allocvalueschar.
        ls_allocvalueschar-charact    = 'RC_FR_PLACA_CR'.
        ls_allocvalueschar-value_char = ls_ac_batch-trl_plate.
        APPEND ls_allocvalueschar TO lt_allocvalueschar.
      ENDIF.

    ENDIF.

*--Fetching Classification BAPI: Change Assignment data
    CALL FUNCTION 'BAPI_OBJCL_CHANGE'
      EXPORTING
        objecttable        = 'MCH1'
        classnum           = lv_classnum
        classtype          = '023'
        objectkey_long     = lv_objectkey
      TABLES
        allocvaluesnumnew  = lt_allocvaluesnum
        allocvaluescharnew = lt_allocvalueschar
        allocvaluescurrnew = lt_allocvaluescurr
        return             = et_messages.

    READ TABLE et_messages INTO ls_messages WITH KEY type = zcl_abs_abap_maintain=>c_msgty_error. "'E'
    IF sy-subrc = 0.
      EXIT.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

      REFRESH et_messages.

      CLEAR ls_messages.
      ls_messages-id = zcl_abs_abap_maintain=>c_custom_msg_class.
      ls_messages-number = '114'.
      ls_messages-type = zcl_abs_abap_maintain=>c_msgty_success. "'S'
      APPEND ls_messages TO et_messages.

      IF ls_ac_batch-exit IS INITIAL.

*--BIN Entrance
        CLEAR ls_ilp_btch.
        READ TABLE lt_ilp_btch INTO ls_ilp_btch
              WITH KEY in_licplate = ls_ac_batch-in_licplate
                        charg = ls_ac_batch-charg
              BINARY SEARCH.
        IF sy-subrc = 0.
          ls_ilp_btch-bin      = ls_ac_batch-bin.
          ls_ilp_btch-bcarimbo = ls_ac_batch-bcarimbo.
          READ TABLE lt_yotpclht INTO DATA(ls_yotpclht)
                   WITH KEY tpclht = ls_ac_batch-bcarimbo BINARY SEARCH.
          IF sy-subrc = 0.
            ls_ilp_btch-btoclhtt    = ls_yotpclht-toclhtt.
          ENDIF.

*-- Batch is updating from BIN (Loader online)
          ls_ilp_btch-bedate = sy-datum.
          ls_ilp_btch-betime = sy-uzeit.

          ls_ilp_btch-aenam = sy-uname.
          ls_ilp_btch-aedat = sy-datum.
          ls_ilp_btch-aezet = sy-uzeit.
          ls_ilp_btch-chimei  = ls_ac_batch-imei.
          ls_ilp_btch-chbadge = ls_ac_batch-badge.
          APPEND ls_ilp_btch TO lt_update.
          CLEAR ls_ilp_btch.

        ENDIF.

      ELSE.

*--BIN Exit
*--Updating when Transbord is x
        CLEAR ls_ilp_btch.
        READ TABLE lt_ilp_btch INTO ls_ilp_btch
             WITH KEY in_licplate = ls_ac_batch-in_licplate
                      charg = ls_ac_batch-charg
             BINARY SEARCH.
        IF sy-subrc = 0.
          ls_ilp_btch-aenam = sy-uname.
          ls_ilp_btch-aedat = sy-datum.
          ls_ilp_btch-aezet = sy-uzeit.
          ls_ilp_btch-loevm = 'X'.
          ls_ilp_btch-chimei  = ls_ac_batch-imei.
          ls_ilp_btch-chbadge = ls_ac_batch-badge.
          APPEND ls_ilp_btch TO lt_update.
**          CLEAR ls_ilp_btch.
        ENDIF.

        READ TABLE lt_ilp_btch TRANSPORTING NO FIELDS
              WITH KEY in_licplate = ls_ac_batch-hdr_plate
                       charg       = ls_ac_batch-charg
              BINARY SEARCH.
        IF sy-subrc NE 0.
*--Inserting when Transbord is x
          ls_insert-in_licplate = ls_ac_batch-hdr_plate.
          ls_insert-trl_plate   = ls_ac_batch-trl_plate.
          ls_insert-charg       = ls_ac_batch-charg.
**          ls_ilp_btch-farm      = ls_ac_batch-farm.
          ls_insert-bin         = ls_ac_batch-bin.
          ls_insert-tmatnr      = ls_ac_batch-tmatnr.
          ls_insert-carimbo     = ls_ilp_btch-carimbo. " ls_ac_batch-carimbo.
          CLEAR ls_yotpclht.
          READ TABLE lt_yotpclht INTO ls_yotpclht
                    WITH KEY tpclht = ls_ilp_btch-carimbo BINARY SEARCH.
          IF sy-subrc = 0.
            ls_insert-toclhtt    = ls_yotpclht-toclhtt.
          ENDIF.
          ls_insert-ymatnr      = ls_ilp_btch-ymatnr.
          ls_insert-maktx       = ls_ilp_btch-maktx.
          ls_insert-bcarimbo    = ls_ac_batch-bcarimbo. " ls_ilp_btch-bcarimbo.
          CLEAR ls_yotpclht.
          READ TABLE lt_yotpclht INTO ls_yotpclht
                   WITH KEY tpclht = ls_ac_batch-bcarimbo BINARY SEARCH.
          IF sy-subrc = 0.
            ls_insert-btoclhtt    = ls_yotpclht-toclhtt.
          ENDIF.

          ls_insert-farm        = ls_ilp_btch-farm.
          ls_insert-mackey      = ls_ilp_btch-mackey.
          ls_insert-ldcde       = ls_ilp_btch-ldcde.
          ls_insert-werks       = ls_ilp_btch-werks.

          ls_insert-zzturma     = ls_ac_batch-zzturma.
          ls_insert-tplnr       = ls_ac_batch-tplnr.
          ls_insert-weighbridge = 'X'.
          ls_insert-ersda       = ls_ac_batch-ersda. "sy-datum.
          ls_insert-ertme       = ls_ac_batch-ertme. "sy-datum.
          ls_insert-baggp       = ls_ac_batch-zzbaggp.
**          ls_insert-mackey      = ls_ac_batch-zzmackey.
**          ls_insert-ldcde       = ls_ac_batch-ldcde.
**          ls_insert-werks       = ls_ac_batch-werks.
          ls_insert-ernam       = sy-uname.
          ls_insert-erdat       = sy-datum.
          ls_insert-erzet       = sy-uzeit.
          ls_insert-crimei      = ls_ac_batch-imei.
          ls_insert-crbadge     = ls_ac_batch-badge.

          ls_ilp_btch-ibag_no   = ls_ac_batch-ibag_no.
          ls_ilp_btch-fbag_no   = ls_ac_batch-fbag_no.
          ls_ilp_btch-tot_qty   = ls_ac_batch-tot_qty.
          ls_ilp_btch-maktx     = ls_ac_batch-maktx.
          ls_ilp_btch-ymatnr    = ls_ac_batch-ymatnr.
          ls_ilp_btch-toclhtt   = ls_ac_batch-toclhtt.
          ls_ilp_btch-bcarimbo  = ls_ac_batch-bcarimbo.
          ls_ilp_btch-btoclhtt  = ls_ac_batch-btoclhtt.
          ls_ilp_btch-bdate     = ls_ac_batch-bdate.
          ls_ilp_btch-btime     = ls_ac_batch-btime.
          ls_ilp_btch-transbord = ls_ac_batch-trans_bord.
          APPEND ls_insert TO lt_insert.
          CLEAR ls_insert.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

*--------------------------Wave 3 changes
  CLEAR: ls_ac_batch, ls_ilp_btch, ls_yotpclht.
  READ TABLE it_ac_batch INTO ls_ac_batch INDEX 1.
  IF ls_ac_batch-bcarimbo = '23'
 AND ls_ac_batch-exit IS NOT INITIAL. "--BIN Exit
*    LOOP AT lt_ilp_btch INTO ls_ilp_btch
    LOOP AT lt_ilp_btch_bex INTO DATA(ls_ilp_btch_bex)
                      WHERE in_licplate = ls_ac_batch-hdr_plate
                        AND bcarimbo NE ls_ac_batch-bcarimbo.
      READ TABLE lt_yotpclht INTO ls_yotpclht
                  WITH KEY tpclht = ls_ac_batch-bcarimbo BINARY SEARCH.
      ls_ilp_btch_bex-bcarimbo = ls_ac_batch-bcarimbo.
      ls_ilp_btch_bex-btoclhtt = ls_yotpclht-toclhtt. " ls_ac_batch-btoclhtt.

      ls_ilp_btch_bex-aenam = sy-uname.
      ls_ilp_btch_bex-aedat = sy-datum.
      ls_ilp_btch_bex-aezet = sy-uzeit.
      ls_ilp_btch_bex-chimei  = ls_ac_batch-imei.
      ls_ilp_btch_bex-chbadge = ls_ac_batch-badge.
      APPEND ls_ilp_btch_bex TO lt_update.
      CLEAR: ls_ilp_btch_bex, ls_yotpclht.
    ENDLOOP.

  ELSEIF ls_ac_batch-bcarimbo = '23'
   AND ls_ac_batch-exit IS INITIAL. "--BIN Entrance

    LOOP AT lt_ilp_btch_ben INTO DATA(ls_ilp_btch_ben)
                  WHERE "in_licplate = ls_ac_batch-in_licplate AND
                        bin = ls_ac_batch-bin
                    AND bcarimbo NE ls_ac_batch-bcarimbo.
      READ TABLE lt_yotpclht INTO ls_yotpclht
                WITH KEY tpclht = ls_ac_batch-bcarimbo BINARY SEARCH.
      ls_ilp_btch_ben-bcarimbo = ls_ac_batch-bcarimbo.
      ls_ilp_btch_ben-btoclhtt = ls_yotpclht-toclhtt. " ls_ac_batch-btoclhtt.

      ls_ilp_btch_ben-aenam = sy-uname.
      ls_ilp_btch_ben-aedat = sy-datum.
      ls_ilp_btch_ben-aezet = sy-uzeit.
      ls_ilp_btch_ben-chimei  = ls_ac_batch-imei.
      ls_ilp_btch_ben-chbadge = ls_ac_batch-badge.
      APPEND ls_ilp_btch_ben TO lt_update.
      CLEAR: ls_ilp_btch_ben, ls_yotpclht.
    ENDLOOP.

  ENDIF.
*--------------------------Wave 3 changes

  IF lt_update IS NOT INITIAL.
    UPDATE zabs_ilp_btch FROM TABLE lt_update.
  ENDIF.

  IF lt_insert IS NOT INITIAL.
    INSERT zabs_ilp_btch FROM TABLE lt_insert.
  ENDIF.

  COMMIT WORK.

ENDMETHOD.


METHOD bexithdrplateset_get_entityset.

*-- Local Declaration
  DATA: ls_entityset  TYPE zcl_zabs_mobile_acm_mpc=>ts_bexithdrplate,
        lv_prevdate   TYPE p0001-begda,
        lv_days       TYPE t5a4a-dlydy,
        lv_cnval1     TYPE zabs_del_cnval,
        lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
        lt_filter     TYPE /iwbep/t_mgw_select_option,
        ls_filter     TYPE /iwbep/s_mgw_select_option,
        lv_filter_str TYPE string,
        ltr_werks     TYPE RANGE OF werks_d,
        lsr_werks     LIKE LINE OF ltr_werks.

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
      WHEN 'WERKS'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = ltr_werks ).
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

*--Get variant table data
  CALL METHOD zcl_abs_get_variants=>get_constant_single
    EXPORTING
      iv_mod    = space
      iv_objid  = zcl_abs_abap_maintain=>c_objid_mobile   "'MOBC'
      iv_k1val  = zcl_abs_abap_maintain=>c_key_mob_wbdays "'WBDAYS'
    IMPORTING
      ev_cnval1 = lv_cnval1. "'2'

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

*-- Fetch the header plate numbers at Bin Exit
  SELECT prnum, gjahr, lic_plate, semireb1, semireb2
    FROM /agri/fmprhdr
    INTO TABLE @DATA(lt_prhdr)
   WHERE gtart  EQ @zcl_abs_abap_maintain=>c_dclass_owpr    "'OWPR'
     AND werks  IN @ltr_werks
     AND status EQ @zcl_abs_abap_maintain=>c_dclass_status  "'S'
     AND erdat  BETWEEN @lv_prevdate AND @sy-datum.
  IF sy-subrc EQ 0.
    SELECT prnum, gjahr, pritm
      FROM /agri/fmpritm
      INTO TABLE @DATA(lt_pritm)
       FOR ALL ENTRIES IN @lt_prhdr
     WHERE prnum EQ @lt_prhdr-prnum.
    IF sy-subrc EQ 0.
      SORT lt_pritm BY prnum gjahr.
    ENDIF.
*    SORT lt_prhdr BY lic_plate.
*    DELETE ADJACENT DUPLICATES FROM lt_prhdr COMPARING lic_plate.
    DELETE lt_prhdr WHERE lic_plate IS INITIAL.
  ENDIF.

  LOOP AT lt_prhdr INTO DATA(ls_prhdr).
    READ TABLE lt_pritm TRANSPORTING NO FIELDS
          WITH KEY prnum = ls_prhdr-prnum
                   gjahr = ls_prhdr-gjahr
        BINARY SEARCH.
    IF sy-subrc NE 0.
      IF ls_prhdr-semireb1 IS NOT INITIAL.
        CLEAR ls_entityset.
        ls_entityset-hdr_plate = ls_prhdr-lic_plate.
        ls_entityset-trl_plate = ls_prhdr-semireb1.
        APPEND ls_entityset TO et_entityset.
      ENDIF.

      IF ls_prhdr-semireb2 IS NOT INITIAL.
        CLEAR ls_entityset.
        ls_entityset-hdr_plate = ls_prhdr-lic_plate.
        ls_entityset-trl_plate = ls_prhdr-semireb2.
        APPEND ls_entityset TO et_entityset.
      ENDIF.
    ENDIF.
  ENDLOOP.

  SORT et_entityset BY hdr_plate trl_plate.
  DELETE ADJACENT DUPLICATES FROM et_entityset
                        COMPARING hdr_plate trl_plate.

ENDMETHOD.


METHOD bexittrailplates_get_entityset.

*-- Local Declaration
  DATA: ls_entityset  TYPE zcl_zabs_mobile_acm_mpc=>ts_bexittrailplate,
        lv_prevdate   TYPE p0001-begda,
        lv_days       TYPE t5a4a-dlydy,
        lv_cnval1     TYPE zabs_del_cnval,
        lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
        lt_filter     TYPE /iwbep/t_mgw_select_option,
        ls_filter     TYPE /iwbep/s_mgw_select_option,
        lv_filter_str TYPE string,
        ltr_werks     TYPE RANGE OF werks_d,
        lsr_werks     LIKE LINE OF ltr_werks.
*        ltr_hplate    TYPE RANGE OF zabs_del_hdrplt,
*        lsr_hplate    LIKE LINE OF ltr_hplate.

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
      WHEN 'WERKS'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = ltr_werks ).
*      WHEN 'HDR_PLATE'.
*        lo_filter->convert_select_option(
*          EXPORTING
*            is_select_option = ls_filter
*          IMPORTING
*            et_select_option = ltr_hplate ).
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

*--Get variant table data
  CALL METHOD zcl_abs_get_variants=>get_constant_single
    EXPORTING
      iv_mod    = space
      iv_objid  = zcl_abs_abap_maintain=>c_objid_mobile   "'MOBC'
      iv_k1val  = zcl_abs_abap_maintain=>c_key_mob_wbdays "'WBDAYS'
    IMPORTING
      ev_cnval1 = lv_cnval1. "'2'

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

*-- Fetch the header plate numbers at Bin Exit
  SELECT prnum, gjahr, semireb1, semireb2
    FROM /agri/fmprhdr
    INTO TABLE @DATA(lt_prhdr)
   WHERE gtart     EQ @zcl_abs_abap_maintain=>c_dclass_owpr    "'OWPR'
     AND werks     IN @ltr_werks
     AND status    EQ @zcl_abs_abap_maintain=>c_dclass_status  "'S'
*     AND lic_plate IN @ltr_hplate
     AND erdat     BETWEEN @lv_prevdate AND @sy-datum.
  IF sy-subrc EQ 0.
    SELECT prnum, gjahr, pritm
      FROM /agri/fmpritm
      INTO TABLE @DATA(lt_pritm)
       FOR ALL ENTRIES IN @lt_prhdr
     WHERE prnum EQ @lt_prhdr-prnum.
    IF sy-subrc EQ 0.
      SORT lt_pritm BY prnum gjahr.
    ENDIF.

    LOOP AT lt_prhdr INTO DATA(ls_prhdr).
      READ TABLE lt_pritm TRANSPORTING NO FIELDS
            WITH KEY prnum = ls_prhdr-prnum
                     gjahr = ls_prhdr-gjahr
          BINARY SEARCH.
      IF sy-subrc NE 0.
        IF ls_prhdr-semireb1 IS NOT INITIAL.
          CLEAR ls_entityset.
          ls_entityset-trl_plate = ls_prhdr-semireb1.
          APPEND ls_entityset TO et_entityset.
        ENDIF.

        IF ls_prhdr-semireb2 IS NOT INITIAL.
          CLEAR ls_entityset.
          ls_entityset-trl_plate = ls_prhdr-semireb2.
          APPEND ls_entityset TO et_entityset.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  SORT et_entityset BY trl_plate.
  DELETE ADJACENT DUPLICATES FROM et_entityset COMPARING trl_plate.

ENDMETHOD.


METHOD binextractset_get_entityset.

*--Internal table declaration
  DATA : lt_ac_bin     TYPE TABLE OF zabs_s_ac_bin,
         lt_filter     TYPE /iwbep/t_mgw_select_option,

*--Workarea declaration
         ls_ac_bin     TYPE zabs_s_ac_bin,
         ls_entityset  TYPE zcl_zabs_mobile_acm_mpc=>ts_binextract,
         ls_filter     TYPE /iwbep/s_mgw_select_option,

*--Local variable declaration
         lv_filter_str TYPE string,

*--Object declaration
         lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,

*--Range table declaration
         lrt_bin       TYPE RANGE OF zabs_del_bin,
         lrs_bin       LIKE LINE OF lrt_bin,
         lrt_plate     TYPE RANGE OF zabs_del_licplate,
         lrs_plate     LIKE LINE OF lrt_plate.

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
      WHEN 'BIN'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_bin ).
      WHEN 'IN_LICPLATE'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_plate ).
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

  IF lrt_bin IS NOT INITIAL.
    READ TABLE lrt_plate INTO lrs_plate INDEX 1.
    READ TABLE lrt_bin INTO lrs_bin INDEX 1.

    IF sy-subrc EQ 0.
      CALL METHOD me->bin_extract
        EXPORTING
          iv_bin    = lrs_bin-low
          iv_plate  = lrs_plate-low
        IMPORTING
          et_ac_bin = lt_ac_bin.
    ENDIF.
  ENDIF.

  LOOP AT lt_ac_bin INTO ls_ac_bin.
    MOVE-CORRESPONDING ls_ac_bin TO ls_entityset.
    APPEND ls_entityset TO et_entityset.
    CLEAR ls_entityset.
  ENDLOOP.

ENDMETHOD.


METHOD binf4set_get_entityset.

*-- Local Declarations
  DATA: lt_values     TYPE STANDARD TABLE OF bapicharactvalueschar,
        lt_return     TYPE STANDARD TABLE OF bapiret2,
        ls_bin_f4     TYPE zcl_zabs_mobile_acm_mpc=>ts_binf4,
        lv_farm       TYPE zabs_del_farm,
        lv_bin        TYPE zabs_del_bin,
        lt_filter     TYPE /iwbep/t_mgw_select_option,
        ls_filter     TYPE /iwbep/s_mgw_select_option,
        lv_filter_str TYPE string,
        lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
        ltr_farm      TYPE RANGE OF /agri/gltplma,
        lsr_farm      LIKE LINE OF ltr_farm,
        ltr_bin       TYPE RANGE OF zabs_del_bin,
        lsr_bin       LIKE LINE OF ltr_bin,
        ltr_level     TYPE RANGE OF char1,
        lsr_level     LIKE LINE OF ltr_level,
        lv_level      TYPE i.

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
      WHEN 'BIN'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = ltr_bin ).
      WHEN 'FARM'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = ltr_farm ).
      WHEN 'LEVEL'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = ltr_level ).
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

  CLEAR lsr_level.
  READ TABLE ltr_level INTO lsr_level INDEX 1.
  IF sy-subrc EQ 0.
    IF lsr_level-low EQ '0'.  "Bin Exit

      CLEAR lsr_farm.
      READ TABLE ltr_farm INTO lsr_farm INDEX 1.
      IF sy-subrc EQ 0.
        SELECT farm, bin, werks
          FROM zabs_ilp_btch
          INTO TABLE @DATA(lt_ilp_btch)
         WHERE farm        EQ @lsr_farm-low
           AND weighbridge EQ @space
           AND bin         NE @space
           AND loevm       EQ @space
           AND transbord   EQ @space.
        IF sy-subrc EQ 0.
          CLEAR: lsr_bin, lv_bin.
          READ TABLE ltr_bin INTO lsr_bin INDEX 1.
          IF sy-subrc EQ 0.
            CONCATENATE lsr_bin-low '*' INTO lv_bin.
          ENDIF.

          LOOP AT lt_ilp_btch INTO DATA(ls_ilp_btch) WHERE bin CP lv_bin.
            CLEAR ls_bin_f4.
            ls_bin_f4-farm  = ls_ilp_btch-farm.
            ls_bin_f4-bin   = ls_ilp_btch-bin.
            ls_bin_f4-werks = ls_ilp_btch-werks.
            APPEND ls_bin_f4 TO et_entityset.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ELSE.

      CALL FUNCTION 'BAPI_CHARACT_GETDETAIL'
        EXPORTING
          charactname       = 'ABS_BIN'
          language          = sy-langu
        TABLES
          charactvalueschar = lt_values
          return            = lt_return.

      READ TABLE lt_return TRANSPORTING NO FIELDS
            WITH KEY type = zcl_abs_abap_maintain=>c_msgty_error.
      IF sy-subrc EQ 0.
        RETURN.
      ENDIF.

      CLEAR: lv_bin, lv_level.

      IF lsr_level-low EQ '1'.  "Bin enterence initial screen
        CLEAR lsr_farm.
        READ TABLE ltr_farm INTO lsr_farm INDEX 1.
        IF sy-subrc EQ 0.
          CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
            EXPORTING
              input  = lsr_farm-low
            IMPORTING
              output = lsr_farm-low.
          CONCATENATE lsr_farm-low '*' INTO lv_bin.
          lv_level = 2.
        ENDIF.
      ELSEIF lsr_level-low EQ '2'. "Bin enterence second screen
        CLEAR lsr_bin.
        READ TABLE ltr_bin INTO lsr_bin INDEX 1.
        IF sy-subrc EQ 0.
          CONCATENATE lsr_bin-low '*' INTO lv_bin.
          lv_level = 3.
        ENDIF.
      ENDIF.

      LOOP AT lt_values INTO DATA(ls_values) WHERE value_char CP lv_bin.
        SPLIT ls_values-value_char AT '-' INTO TABLE DATA(lt_string).
        IF sy-subrc EQ 0.
          CLEAR ls_bin_f4.
          LOOP AT lt_string INTO DATA(ls_string) FROM 1 TO lv_level.
            IF ls_bin_f4-bin IS INITIAL.
              ls_bin_f4-bin  = ls_string.
              ls_bin_f4-farm = ls_string.
            ELSE.
              CONCATENATE ls_bin_f4-bin ls_string INTO ls_bin_f4-bin
                                             SEPARATED BY '-'.
            ENDIF.
          ENDLOOP.
          ls_bin_f4-level = lv_level.
          APPEND ls_bin_f4 TO et_entityset.
          REFRESH lt_string.
        ENDIF.
      ENDLOOP.
    ENDIF.

    SORT et_entityset BY farm bin.
    DELETE ADJACENT DUPLICATES FROM et_entityset COMPARING farm bin.
  ENDIF.

ENDMETHOD.


METHOD bin_extract.

*-- Local Declarations
  DATA: ls_message     TYPE /agri/s_gprolog,
        lv_ext_carmibo TYPE zabs_del_tpclht,
        lv_mixed       TYPE zabs_del_mixed.

*--Internal table declaration
  DATA : lt_ilp_btch TYPE TABLE OF zabs_ilp_btch,
         ls_ilp_btch TYPE zabs_ilp_btch,
         ls_ac_batch TYPE zabs_s_ac_batch,
         ls_ac_bin   TYPE zabs_s_ac_bin.

*--Fetching Internal License Plate data from custom table
  SELECT *
    FROM zabs_ilp_btch
    INTO TABLE @DATA(lt_ilp_btch_p)
   WHERE in_licplate EQ @iv_plate
     AND weighbridge EQ @abap_true
     AND bin         NE @space " EQ @iv_bin
     AND loevm       EQ @space
     AND transbord   EQ @space.
  IF sy-subrc EQ 0.

    CLEAR ls_message.
    READ TABLE lt_ilp_btch_p INTO DATA(ls_ilp_btch_p) INDEX 1.

**    ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_info.
**    ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
**    ls_message-msgno = '196'.
**    ls_message-msgv1 = ls_ilp_btch_p-maktx.
**    add_messages_to_msg_container( is_message = ls_message ).

    SELECT *
      FROM zabs_ilp_btch
      INTO TABLE lt_ilp_btch
     WHERE ymatnr      EQ ls_ilp_btch_p-ymatnr
       AND weighbridge EQ space
       AND bin         EQ iv_bin
       AND loevm       EQ space
       AND transbord   EQ space.

    "Fill Exicted Carimbo & Mixed Field depending upon existing Carimbo data
    CLEAR: lv_ext_carmibo,lv_mixed.
**    IF ls_ilp_btch_p-bcarimbo = '23'.
**      lv_mixed = abap_true.
**    ELSE.
    DATA(lt_ilp_btc) = lt_ilp_btch_p.
    SORT lt_ilp_btc BY bcarimbo.
    DELETE ADJACENT DUPLICATES FROM lt_ilp_btc COMPARING bcarimbo.
    DESCRIBE TABLE lt_ilp_btc LINES DATA(lv_count).
    IF lv_count EQ 1.
      READ TABLE lt_ilp_btc INTO DATA(ls_ilp_btc) INDEX 1.
      lv_ext_carmibo = ls_ilp_btc-bcarimbo.
    ELSE.
      lv_mixed = abap_true.
    ENDIF.
**    ENDIF.

  ELSE.

    SELECT *
      FROM zabs_ilp_btch
      INTO TABLE lt_ilp_btch
     WHERE weighbridge EQ space
       AND bin         EQ iv_bin
       AND loevm       EQ space
       AND transbord   EQ space.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

  ENDIF.
**-- Fetch the active crop seasons in the current day
*    SELECT tplnr_fl, contr, zzfazvartecnia
*      FROM /agri/glflcma
*      INTO TABLE @DATA(lt_flcma)
*       FOR ALL ENTRIES IN @lt_ilp_btch
*     WHERE tplnr_fl EQ @lt_ilp_btch-tplnr
*       AND datab    LE @sy-datum
*       AND datbi    GE @sy-datum
*       AND astat    EQ @zcl_abs_abap_maintain=>c_cs_active
*       AND loevm    EQ @space.
*    IF sy-subrc EQ 0.
*      SORT lt_flcma BY tplnr_fl.
*    ENDIF.

  LOOP AT lt_ilp_btch INTO ls_ilp_btch.
    ls_ac_bin-bin         = ls_ilp_btch-bin. "ls_ac_batch-in_licplate.
    ls_ac_bin-in_licplate = ls_ilp_btch-in_licplate. "ls_ac_batch-in_licplate.
    ls_ac_bin-charg       = ls_ilp_btch-charg. "ls_ac_batch-charg.
    ls_ac_bin-tplnr       = ls_ilp_btch-tplnr. "ls_ac_batch-tplnr.
    ls_ac_bin-zzturma     = ls_ilp_btch-zzturma. "ls_ac_batch-tplnr.
    ls_ac_bin-tmatnr      = ls_ilp_btch-tmatnr. "ls_ac_batch-tplnr.
    ls_ac_bin-bedate      = ls_ilp_btch-bedate.
    ls_ac_bin-betime      = ls_ilp_btch-betime.
    ls_ac_bin-maktx       = ls_ilp_btch-maktx.
    ls_ac_bin-ymatnr      = ls_ilp_btch-ymatnr.
    ls_ac_bin-bcarimbo    = ls_ilp_btch-bcarimbo.
    ls_ac_bin-btoclhtt    = ls_ilp_btch-btoclhtt.
    ls_ac_bin-werks       = ls_ilp_btch-werks.
    ls_ac_bin-ersda       = ls_ilp_btch-ersda.
    ls_ac_bin-ertme       = ls_ilp_btch-ertme.
    ls_ac_bin-ext_carimbo = lv_ext_carmibo.
    ls_ac_bin-mixed       = lv_mixed.


*      READ TABLE lt_flcma INTO DATA(ls_flcma)
*            WITH KEY tplnr_fl = ls_ilp_btch-tplnr
*          BINARY SEARCH.
*      IF sy-subrc EQ 0.
*        ls_ac_bin-fazvartec = ls_flcma-zzfazvartecnia.
*      ENDIF.
    APPEND ls_ac_bin TO et_ac_bin.
    CLEAR ls_ac_bin.
  ENDLOOP.

ENDMETHOD.


METHOD create_accomplishment.

*-- Create the new Accomplishment sheet
  CALL FUNCTION 'ZABS_FMAC_CREATE'
    EXPORTING
      is_fmacdoc  = is_fmac_doc
    IMPORTING
      es_fmac_doc = es_fmac_doc
      et_messages = et_messages.

ENDMETHOD.


METHOD evnjustset_get_entityset.

*-- Fetch the Activity (Event) Justifications
  CALL METHOD me->get_event_justifications
    IMPORTING
      et_evejust = et_entityset.

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


METHOD farmterrainsset_get_entityset.

*-- Fetch the farms under leaders
  CALL METHOD me->get_leader_farms
    EXPORTING
      io_tech_request_context = io_tech_request_context
    IMPORTING
      et_leader_farms         = DATA(lt_leader_farms).

*  CALL METHOD me->get_leader_farms
*    IMPORTING
*      et_leader_farms = DATA(lt_leader_farms).

*-- Fetch the terrains under the farms (1st level terrains)
  CALL METHOD me->get_farm_terrains
    EXPORTING
      it_leader_farms  = lt_leader_farms
    IMPORTING
      et_farm_terrains = et_entityset.

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


METHOD fetch_carimbo.

  DATA: ls_carimbo TYPE ty_carimbo.

  IF it_tplnr IS NOT INITIAL.
    LOOP AT it_tplnr INTO DATA(ls_tplnr).
      CLEAR ls_carimbo.
      ls_carimbo-tplnr = ls_tplnr-tplnr_fl.
      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
        EXPORTING
          input  = ls_tplnr-tplnr_fl
        IMPORTING
          output = ls_tplnr-tplnr_fl.

      SPLIT ls_tplnr-tplnr_fl AT '-' INTO ls_carimbo-imov
                                         ls_carimbo-talhao.
      APPEND ls_carimbo TO ct_carimbo.
    ENDLOOP.

*-- Fetch the Carimbo and description for the farm and terrain
    SELECT a~imov, a~talhao, a~carimbo, b~toclhtt
      FROM yocotaseg AS a INNER JOIN yotpclht AS b
        ON b~tpclht EQ a~carimbo
      INTO TABLE @DATA(lt_yocotaseg)
       FOR ALL ENTRIES IN @ct_carimbo
     WHERE a~dtvlcota EQ @sy-datum
       AND a~imov     EQ @ct_carimbo-imov
       AND a~talhao   EQ @ct_carimbo-talhao.
    IF sy-subrc EQ 0.
      SORT lt_yocotaseg BY imov talhao.
    ENDIF.

**Start of Wave 3 changes

      SELECT SINGLE dtvlcota, semana
        FROM yocotaseg INTO @DATA(ls_week)
       WHERE dtvlcota EQ @sy-datum.

      SELECT a~dtvlcota, a~semana, a~imov, a~talhao, a~carimbo, b~toclhtt
        FROM yocotaseg AS a INNER JOIN yotpclht AS b
          ON b~tpclht EQ a~carimbo
        INTO TABLE @DATA(lt_yocotaseg_week)
         FOR ALL ENTRIES IN @ct_carimbo
       WHERE a~semana   EQ @ls_week-semana
         AND a~imov     EQ @ct_carimbo-imov
         AND a~talhao   EQ @ct_carimbo-talhao
         AND a~carimbo  NE @space.
      IF sy-subrc EQ 0.
        SORT lt_yocotaseg_week BY dtvlcota DESCENDING.
        SORT lt_yocotaseg_week BY imov talhao.
      ENDIF.

      SELECT *
        FROM yotpclht
        INTO TABLE @DATA(lt_yotpclht).
      IF sy-subrc EQ 0.
        SORT lt_yotpclht BY tpclht.
      ENDIF.

**End of Wave 3 changes

    LOOP AT ct_carimbo ASSIGNING FIELD-SYMBOL(<fs_carimbo>).
      READ TABLE lt_yocotaseg ASSIGNING FIELD-SYMBOL(<fs_yocotaseg>)
            WITH KEY imov   = <fs_carimbo>-imov
                     talhao = <fs_carimbo>-talhao
          BINARY SEARCH.
      IF sy-subrc EQ 0.
        <fs_carimbo>-carimbo = <fs_yocotaseg>-carimbo.
        <fs_carimbo>-toclhtt = <fs_yocotaseg>-toclhtt.
**Start of Wave 3 changes
      ELSE.
        READ TABLE lt_yocotaseg_week ASSIGNING FIELD-SYMBOL(<fs_yocotaseg_week>)
           WITH KEY imov   = <fs_carimbo>-imov
                    talhao = <fs_carimbo>-talhao
         BINARY SEARCH.
        IF sy-subrc EQ 0.
          <fs_carimbo>-carimbo = <fs_yocotaseg_week>-carimbo.
          <fs_carimbo>-toclhtt = <fs_yocotaseg_week>-toclhtt.
        ELSE.
          <fs_carimbo>-carimbo = '23'.
          READ TABLE lt_yotpclht INTO DATA(ls_yotpclht)
          WITH KEY tpclht = <fs_carimbo>-carimbo BINARY SEARCH.
          <fs_carimbo>-toclhtt = ls_yotpclht-toclhtt.
        ENDIF.
      ENDIF.
**End of Wave 3 changes
    ENDLOOP.

    SORT ct_carimbo BY tplnr.
  ENDIF.


ENDMETHOD.


  METHOD get_accbaggrpclosed.

*--workarea declaration
    DATA : ls_usr_emp    TYPE zabs_s_ac_usr_role,
           ls_bggrp      TYPE zabs_ac_bggrp,
           ls_bgclose    TYPE zabs_s_ac_bgclose,
           ls_entityset  TYPE zcl_zabs_mobile_acm_mpc=>ts_accbaggrpclosed,

*--Internal table declaration
           lt_bggrp      TYPE TABLE OF zabs_ac_bggrp,
           lt_bgclose    TYPE TABLE OF zabs_s_ac_bgclose,

*--Variable declaration
           lv_bdate      TYPE p0001-begda,
           lv_days       TYPE t5a4a-dlydy,
           lv_cnval      TYPE zabs_del_cnval,
           lt_filter     TYPE /iwbep/t_mgw_select_option,
           ls_filter     TYPE /iwbep/s_mgw_select_option,
           lv_filter_str TYPE string,
           lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
           ltr_urole     TYPE RANGE OF zabs_del_urole,
           lsr_urole     LIKE LINE OF ltr_urole,
           ltr_lifnr     TYPE RANGE OF lifnr,
           lsr_lifnr     LIKE LINE OF ltr_lifnr.

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
        WHEN 'UROLE'.
          lo_filter->convert_select_option(
            EXPORTING
              is_select_option = ls_filter
            IMPORTING
              et_select_option = ltr_urole ).

          READ TABLE ltr_urole INTO lsr_urole INDEX 1.
          IF sy-subrc = 0.
            ls_usr_emp-urole = lsr_urole-low.
          ENDIF.

        WHEN 'LIFNR'.
          lo_filter->convert_select_option(
            EXPORTING
              is_select_option = ls_filter
            IMPORTING
              et_select_option = ltr_lifnr ).

          READ TABLE ltr_lifnr INTO lsr_lifnr INDEX 1.
          IF sy-subrc = 0.
            ls_usr_emp-lifnr = lsr_lifnr-low.
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

    IF ls_usr_emp-urole = 'LD'.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = ls_usr_emp-lifnr
        IMPORTING
          output = ls_usr_emp-lifnr.

      CALL METHOD zcl_abs_get_variants=>get_constant_single
        EXPORTING
          iv_objid  = 'MOBC'
          iv_k1val  = 'DAYS'
          iv_k2val  = 'LDAY'
        IMPORTING
          ev_cnval1 = lv_cnval. "'3'

      lv_days = lv_cnval.

      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          date      = sy-datum
          days      = lv_days
          months    = 0
          signum    = '-'
          years     = 0
        IMPORTING
          calc_date = lv_bdate.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = ls_usr_emp-lifnr
        IMPORTING
          output = ls_usr_emp-lifnr.

*--Accomplishment Bag Groups
      SELECT *
        FROM zabs_ac_bggrp
        INTO TABLE lt_bggrp
       WHERE ldcde  EQ ls_usr_emp-lifnr
         AND loaded EQ abap_true
         AND ersda  BETWEEN lv_bdate AND sy-datum.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ENDIF.

    LOOP AT lt_bggrp INTO ls_bggrp.
      MOVE-CORRESPONDING ls_bggrp TO ls_bgclose.
      ls_bgclose-zzmackey = ls_bggrp-mackey.
      ls_bgclose-zzbaggp  = ls_bggrp-baggp.
      ls_bgclose-imei     = ls_bggrp-crimei.
      ls_bgclose-badge    = ls_bggrp-crbadge.
      APPEND ls_bgclose TO lt_bgclose.
      CLEAR ls_bgclose.
    ENDLOOP.

    LOOP AT lt_bgclose INTO ls_bgclose.
      MOVE-CORRESPONDING ls_bgclose TO ls_entityset.
      ls_entityset-urole    = ls_usr_emp-urole.
      ls_entityset-lifnr    = ls_usr_emp-lifnr.
      APPEND ls_entityset TO et_accbaggrpclosed.
      CLEAR ls_entityset.
    ENDLOOP.

  ENDMETHOD.


METHOD get_accbatch.

*--Workarea declaration
  DATA : ls_usr_emp         TYPE zabs_s_ac_usr_role,
*         ls_usr_emp         TYPE zabs_usr_emp,
         ls_ac_batch        TYPE zabs_s_ac_batch,

*--Internal table declaration
         lt_messages        TYPE bapiret2_tt,
         lt_allocvaluescurr TYPE tt_bapi1003_alloc_values_curr,
         lt_allocvaluesnum  TYPE tt_bapi1003_alloc_values_num,
         lt_allocvalueschar TYPE tt_bapi1003_alloc_values_char,
         lt_alloclist       TYPE TABLE OF bapi1003_alloc_list,

*--Variable declaration
         lv_objectkey       TYPE bapi1003_key-object_long,
*         lv_classnum        TYPE bapi1003_key-classnum,
         lv_bdate           TYPE p0001-begda,
         lv_days            TYPE t5a4a-dlydy,
         lv_cnval           TYPE zabs_del_cnval,

         lt_filter          TYPE /iwbep/t_mgw_select_option,
         ls_filter          TYPE /iwbep/s_mgw_select_option,
         lv_filter_str      TYPE string,
         lo_filter          TYPE REF TO /iwbep/if_mgw_req_filter,
         ltr_urole          TYPE RANGE OF zabs_del_urole,
         lsr_urole          LIKE LINE OF ltr_urole,
         ltr_lifnr          TYPE RANGE OF lifnr,
         lsr_lifnr          LIKE LINE OF ltr_lifnr.

*--Field-symbol declaration
  FIELD-SYMBOLS : <fs_value>    TYPE any.

**--Calling method to get user details
*  CALL METHOD me->get_employee_role
*    EXPORTING
*      iv_user    = sy-uname
*    IMPORTING
*      es_usr_emp = ls_usr_emp.

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
      WHEN 'UROLE'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = ltr_urole ).

        READ TABLE ltr_urole INTO lsr_urole INDEX 1.
        IF sy-subrc = 0.
          ls_usr_emp-urole = lsr_urole-low.
        ENDIF.

      WHEN 'LIFNR'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = ltr_lifnr ).

        READ TABLE ltr_lifnr INTO lsr_lifnr INDEX 1.
        IF sy-subrc = 0.
          ls_usr_emp-lifnr = lsr_lifnr-low.
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

  IF ls_usr_emp-urole = zcl_abs_abap_maintain=>c_role_loader. "'LD'

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = ls_usr_emp-lifnr
      IMPORTING
        output = ls_usr_emp-lifnr.

    CALL METHOD zcl_abs_get_variants=>get_constant_single
      EXPORTING
        iv_objid  = zcl_abs_abap_maintain=>c_objid_mobile "'MOBC'
        iv_k1val  = 'DAYS'
        iv_k2val  = 'LDAY'
      IMPORTING
        ev_cnval1 = lv_cnval. "'3'

    lv_days = lv_cnval.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = sy-datum
        days      = lv_days
        months    = 0
        signum    = '-'
        years     = 0
      IMPORTING
        calc_date = lv_bdate.

*--Fetching Internal Licence Plate data
    SELECT *
      FROM zabs_ilp_btch
      INTO TABLE @DATA(lt_ilp_btch)
     WHERE ldcde    EQ @ls_usr_emp-lifnr
*      AND ( ersda  GE @lv_bdate
*      AND   ersda  LE @sy-datum ).
       AND ( erdat  GE @lv_bdate
       AND   erdat  LE @sy-datum ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SORT lt_ilp_btch BY charg.
    DELETE ADJACENT DUPLICATES FROM lt_ilp_btch COMPARING charg.

*--Fetching Batch data from custom table
    SELECT *
      FROM zabs_tab_cbch
      INTO TABLE @DATA(lt_cust_btch)
     WHERE class = @zcl_abs_abap_maintain=>c_atgrp_fruta_mp. "'Z_FRUTA_MP'
    IF sy-subrc = 0.
      SORT lt_cust_btch BY atnam.
    ENDIF.
  ENDIF.

  LOOP AT lt_ilp_btch INTO DATA(ls_ilp_btch).

    CLEAR : lv_objectkey.
    REFRESH : lt_alloclist,
              lt_allocvalueschar,
              lt_allocvaluescurr,
              lt_allocvaluesnum,
              lt_messages.

    CONCATENATE ls_ilp_btch-tmatnr ls_ilp_btch-charg
          INTO lv_objectkey RESPECTING BLANKS.

*--Fetching Classification BAPI: Create Assignment data
    CALL FUNCTION 'BAPI_OBJCL_GETCLASSES'
      EXPORTING
        objecttable_imp    = 'MCH1'
        classtype_imp      = '023'
        read_valuations    = 'X'
        objectkey_imp_long = lv_objectkey
      TABLES
        alloclist          = lt_alloclist
        allocvalueschar    = lt_allocvalueschar
        allocvaluescurr    = lt_allocvaluescurr
        allocvaluesnum     = lt_allocvaluesnum
        return             = lt_messages.

    LOOP AT lt_allocvalueschar INTO DATA(ls_allocvalueschar).
      READ TABLE lt_cust_btch INTO DATA(ls_cust_btch)
                              WITH KEY atnam = ls_allocvalueschar-charact.
      IF sy-subrc EQ 0.
        UNASSIGN <fs_value>.
        ASSIGN COMPONENT ls_cust_btch-field_name OF STRUCTURE ls_ac_batch TO <fs_value>.
        IF <fs_value> IS ASSIGNED.
          <fs_value> = ls_allocvalueschar-value_char.
        ENDIF.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_allocvaluescurr INTO DATA(ls_allocvaluescurr).
      CLEAR ls_cust_btch.
      READ TABLE lt_cust_btch INTO ls_cust_btch
                              WITH KEY atnam = ls_allocvaluescurr-charact.
      IF sy-subrc EQ 0.
        UNASSIGN <fs_value>.
        ASSIGN COMPONENT ls_cust_btch-field_name OF STRUCTURE ls_ac_batch TO <fs_value>.
        IF <fs_value> IS ASSIGNED.
          <fs_value> = ls_allocvaluescurr-value_from.
        ENDIF.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_allocvaluesnum INTO DATA(ls_allocvaluesnum).
      CLEAR ls_cust_btch.
      READ TABLE lt_cust_btch INTO ls_cust_btch
                              WITH KEY atnam = ls_allocvaluesnum-charact.
      IF sy-subrc EQ 0.
        UNASSIGN <fs_value>.
        ASSIGN COMPONENT ls_cust_btch-field_name OF STRUCTURE ls_ac_batch TO <fs_value>.
        IF <fs_value> IS ASSIGNED.
          <fs_value> = ls_allocvaluesnum-value_from.
        ENDIF.
      ENDIF.
    ENDLOOP.

    CONCATENATE ls_ac_batch-farm ls_ac_batch-ftplnr
           INTO ls_ac_batch-tplnr SEPARATED BY '-'.
    ls_ac_batch-charg    = ls_ilp_btch-charg.
    ls_ac_batch-zzbaggp  = ls_ilp_btch-baggp.
    ls_ac_batch-zzmackey = ls_ilp_btch-mackey.
    ls_ac_batch-bdate    = ls_ilp_btch-bdate.
    ls_ac_batch-btime    = ls_ilp_btch-btime.
    ls_ac_batch-tmatnr   = ls_ilp_btch-tmatnr.
    ls_ac_batch-werks    = ls_ilp_btch-werks.
    ls_ac_batch-ibag_no  = ls_ilp_btch-ibag_no.
    ls_ac_batch-fbag_no  = ls_ilp_btch-fbag_no.
    ls_ac_batch-tot_qty  = ls_ilp_btch-tot_qty.
    ls_ac_batch-carimbo  = ls_ilp_btch-carimbo.
    ls_ac_batch-maktx    = ls_ilp_btch-maktx.
    ls_ac_batch-ymatnr   = ls_ilp_btch-ymatnr.
    ls_ac_batch-toclhtt  = ls_ilp_btch-toclhtt.
    ls_ac_batch-ersda    = ls_ilp_btch-ersda.
    ls_ac_batch-ertme    = ls_ilp_btch-ertme.
    ls_ac_batch-urole    = ls_usr_emp-urole.
    ls_ac_batch-lifnr    = ls_usr_emp-lifnr.

    APPEND ls_ac_batch TO et_accbatch.
    CLEAR ls_ac_batch.

  ENDLOOP.

ENDMETHOD.


METHOD get_accomplishment_items.
*
*  DATA : lv_prevdate TYPE p0001-begda,
*         lv_days     TYPE t5a4a-dlydy,
*         lv_cnval1   TYPE zabs_del_cnval,
*         ls_accitems TYPE zcl_zabs_mobile_acm_mpc=>ts_accitems.
*
*  TYPES: BEGIN OF ty_turm_terrain,
*           turma TYPE zfmturma_id,
*           tplnr TYPE /agri/gltplnr_fl,
*         END OF ty_turm_terrain.
*
**-- Local Declarations
*  DATA: lt_turma_terrain TYPE STANDARD TABLE OF ty_turm_terrain,
*        ls_turma_terrain TYPE ty_turm_terrain,
*        lv_date          TYPE sy-datum.
*
*  DATA(lt_farm_terrains) = it_farm_terrains.
*  SORT lt_farm_terrains BY farm.
*
**-- Colect the Turmas and Terrains
*  LOOP AT it_leader_farms INTO DATA(ls_leader_farms).
*    READ TABLE lt_farm_terrains TRANSPORTING NO FIELDS
*          WITH KEY farm = ls_leader_farms-farm
*        BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      DATA(lv_tabix) = sy-tabix.
*
*      LOOP AT lt_farm_terrains INTO DATA(ls_farm_terrains) FROM lv_tabix.
*        IF ls_farm_terrains-farm NE ls_leader_farms-farm.
*          EXIT.
*        ENDIF.
*
*        CLEAR ls_turma_terrain.
*        ls_turma_terrain-turma = ls_leader_farms-zzturma.
*        ls_turma_terrain-tplnr = ls_farm_terrains-tplnr.
*        APPEND ls_turma_terrain TO lt_turma_terrain.
*      ENDLOOP.
*    ENDIF.
*  ENDLOOP.
*
**--Get variant table data
*  CALL METHOD zcl_abs_get_variants=>get_constant_single
*    EXPORTING
*      iv_mod    = space
*      iv_objid  = zcl_abs_abap_maintain=>c_objid_mobile "'MOBC'
*      iv_k1val  = 'LEAD'
*      iv_k2val  = 'PDAYS'
*    IMPORTING
*      ev_cnval1 = lv_cnval1.
*
*  lv_days = lv_cnval1.
*  lv_days = lv_days + 1.
*
*  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
*    EXPORTING
*      date      = sy-datum
*      days      = lv_days
*      months    = 0
*      signum    = '-'
*      years     = 0
*    IMPORTING
*      calc_date = lv_date.
*
**  lv_date = sy-datum - lv_prevdate.
*
**-- Fetch the Accomplishments
*  IF lt_turma_terrain IS NOT INITIAL.
**    SELECT a~accom, a~werks, a~datum, a~zzturma, a~zzactcg, a~zzmackey,
**           b~posnr, b~tplnr, b~tmatnr, b~aufnr, b~idresource, b~strtdat,
**           b~strttim, b~findat, b~fintim, b~idactvl, b~menge, b~qmein,
**           b~zzimei1, b~zzimei2, b~zzappl, b~zzjust, b~zzbagno, b~zzlongitude,
**           b~zzlatitude, b~zzbaggp, b~zzcdate, b~zzctime
**      FROM /agri/fmachdr AS a INNER JOIN /agri/fmacitm AS b
**        ON b~accom EQ a~accom
**      INTO TABLE @DATA(lt_ac_hdr_itm) "et_accitems
**       FOR ALL ENTRIES IN @lt_turma_terrain
**     WHERE a~strtdat GE @lv_date
**       AND a~strtdat LE @sy-datum
**       AND a~status  NE @zcl_abs_abap_maintain=>c_ac_status_deleted "'DEL'
***     WHERE a~strtdat EQ sy-datum
**       AND a~zzturma EQ @lt_turma_terrain-turma
**       AND a~zztplnr EQ @lt_turma_terrain-tplnr.
*
*    SELECT zzmackey, posnr,accom, werks,datum, zzturma,
*           zzactcg, tplnr, tmatnr, aufnr, idresource, strtdat,
*           strttim, findat, fintim, idactvl, menge, qmein,
*           zzimei1, zzimei2, zzappl, zzjust, zzbagno, zzlongitude,
*           zzlatitude, zzbaggp, zzcdate, zzctime, recflag, error
*    FROM zabs_bgp_accom
*    INTO TABLE @DATA(lt_ac_hdr_itm) "et_accitems
*     FOR ALL ENTRIES IN @lt_turma_terrain
*   WHERE strtdat GE @lv_date
*     AND strtdat LE @sy-datum
**     AND status  NE @zcl_abs_abap_maintain=>c_ac_status_deleted "'DEL'
**     WHERE a~strtdat EQ sy-datum
*     AND zzturma EQ @lt_turma_terrain-turma
*     AND tplnr   EQ @lt_turma_terrain-tplnr.
**     AND recflag EQ @space.
*
*  ENDIF.
*
*  CLEAR ls_leader_farms.
*  READ TABLE it_leader_farms INTO ls_leader_farms INDEX 1.
*  IF sy-subrc EQ 0.
*    LOOP AT lt_ac_hdr_itm INTO DATA(ls_ac_hdr_itm).
*      CLEAR ls_accitems.
*      MOVE-CORRESPONDING ls_ac_hdr_itm TO ls_accitems.
*      ls_accitems-urole = ls_leader_farms-urole.
*      ls_accitems-pernr = ls_leader_farms-pernr.
*      APPEND ls_accitems TO et_accitems.
*    ENDLOOP.
*  ENDIF.
*
ENDMETHOD.


METHOD get_accomplish_data.

*-- Local Declarations
  DATA: ls_message TYPE /agri/s_gprolog.

*-- Get the acomplishment type and harvest task
  CALL METHOD zcl_abs_get_variants=>get_constant_single
    EXPORTING
      iv_objid  = zcl_abs_abap_maintain=>c_objid_mobile
      iv_k1val  = zcl_abs_abap_maintain=>c_key_accomplish_create
    IMPORTING
      ev_cnval1 = es_ac_data-matnr.

  IF es_ac_data-matnr IS INITIAL.
*-- Give error message if task not maintained in variant
    CLEAR ls_message.
    ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error.
    ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
    ls_message-msgno = '097'.
    add_messages_to_msg_container( is_message = ls_message ).
  ENDIF.

ENDMETHOD.


METHOD get_accom_items.

*-- Local Declarations
  DATA: ls_message    TYPE /agri/s_gprolog,
*        ls_usr_emp TYPE zabs_usr_emp.
        ls_usr_emp    TYPE zabs_s_ac_usr_role,
        lt_filter     TYPE /iwbep/t_mgw_select_option,
        ls_filter     TYPE /iwbep/s_mgw_select_option,
        lv_filter_str TYPE string,
        lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
        ltr_urole     TYPE RANGE OF zabs_del_urole,
        lsr_urole     LIKE LINE OF ltr_urole,
        ltr_pernr     TYPE RANGE OF persno,
        lsr_pernr     LIKE LINE OF ltr_pernr.

  DATA : lv_prevdate TYPE p0001-begda,
         lv_days     TYPE t5a4a-dlydy,
         lv_cnval1   TYPE zabs_del_cnval,
         ls_accitems TYPE zcl_zabs_mobile_acm_mpc=>ts_accitems,
         lv_date     TYPE sy-datum.

*-- Get the employee number and role
*  CALL METHOD me->get_employee_role
*    EXPORTING
*      iv_user    = sy-uname
*    IMPORTING
*      es_usr_emp = ls_usr_emp.

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
      WHEN 'UROLE'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = ltr_urole ).

        READ TABLE ltr_urole INTO lsr_urole INDEX 1.
        IF sy-subrc = 0.
          ls_usr_emp-urole = lsr_urole-low.
        ENDIF.

      WHEN 'PERNR'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = ltr_pernr ).

        READ TABLE ltr_pernr INTO lsr_pernr INDEX 1.
        IF sy-subrc = 0.
*          ls_usr_emp-pernr = lsr_pernr-low.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = lsr_pernr-low
            IMPORTING
              output = ls_usr_emp-pernr.

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

  IF ls_usr_emp-pernr IS NOT INITIAL.
*--Get variant table data
    CALL METHOD zcl_abs_get_variants=>get_constant_single
      EXPORTING
        iv_mod    = space
        iv_objid  = zcl_abs_abap_maintain=>c_objid_mobile "'MOBC'
        iv_k1val  = 'LEAD'
        iv_k2val  = 'PDAYS'
      IMPORTING
        ev_cnval1 = lv_cnval1.

    lv_days = lv_cnval1.
    lv_days = lv_days + 1.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = sy-datum
        days      = lv_days
        months    = 0
        signum    = '-'
        years     = 0
      IMPORTING
        calc_date = lv_date.

    SELECT zzmackey
      FROM zabs_hdr_accom
      INTO TABLE @DATA(lt_mackey)
     WHERE pernr   EQ @ls_usr_emp-pernr
       AND strtdat GE @lv_date     " zzhdate GE @lv_date
       AND strtdat LE @sy-datum.   " zzhdate LE @sy-datum.
    IF sy-subrc EQ 0.
      SELECT *
        FROM zabs_bgp_accom
        INTO TABLE @DATA(lt_acc_itm)
         FOR ALL ENTRIES IN @lt_mackey
        WHERE zzmackey EQ @lt_mackey-zzmackey.
*      IF sy-subrc EQ 0.
*        SORT lt_acc_itm BY zzmackey idresource zzhdate strttim.
*        DELETE ADJACENT DUPLICATES FROM lt_acc_itm
*                              COMPARING zzmackey idresource zzhdate strttim.
*      ENDIF.

      LOOP AT lt_acc_itm INTO DATA(ls_acc_itm).
        CLEAR ls_accitems.
        MOVE-CORRESPONDING ls_acc_itm TO ls_accitems.
        ls_accitems-urole = ls_usr_emp-urole.
        APPEND ls_accitems TO et_accitems.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDMETHOD.


  METHOD get_accom_items_rep.

*-- Local Declarations
    DATA: ls_message    TYPE /agri/s_gprolog,
*        ls_usr_emp TYPE zabs_usr_emp.
          ls_usr_emp    TYPE zabs_s_ac_usr_role,
          lt_filter     TYPE /iwbep/t_mgw_select_option,
          ls_filter     TYPE /iwbep/s_mgw_select_option,
          lv_filter_str TYPE string,
          lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
          ltr_urole     TYPE RANGE OF zabs_del_urole,
          lsr_urole     LIKE LINE OF ltr_urole,
          ltr_pernr     TYPE RANGE OF persno,
          lsr_pernr     LIKE LINE OF ltr_pernr.

    DATA : lv_prevdate TYPE p0001-begda,
           lv_days     TYPE t5a4a-dlydy,
           lv_cnval1   TYPE zabs_del_cnval,
           ls_accitems TYPE zcl_zabs_mobile_acm_mpc=>ts_accitems,
           lv_date     TYPE sy-datum.

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
        WHEN 'UROLE'.
          lo_filter->convert_select_option(
            EXPORTING
              is_select_option = ls_filter
            IMPORTING
              et_select_option = ltr_urole ).

          READ TABLE ltr_urole INTO lsr_urole INDEX 1.
          IF sy-subrc = 0.
            ls_usr_emp-urole = lsr_urole-low.
          ENDIF.

        WHEN 'PERNR'.
          lo_filter->convert_select_option(
            EXPORTING
              is_select_option = ls_filter
            IMPORTING
              et_select_option = ltr_pernr ).

          READ TABLE ltr_pernr INTO lsr_pernr INDEX 1.
          IF sy-subrc = 0.
*          ls_usr_emp-pernr = lsr_pernr-low.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = lsr_pernr-low
              IMPORTING
                output = ls_usr_emp-pernr.

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

    IF ls_usr_emp-pernr IS NOT INITIAL.
*--Get variant table data
      CALL METHOD zcl_abs_get_variants=>get_constant_single
        EXPORTING
          iv_mod    = space
          iv_objid  = zcl_abs_abap_maintain=>c_objid_mobile "'MOBC'
          iv_k1val  = 'LEAD'
          iv_k2val  = 'PDAYS'
        IMPORTING
          ev_cnval1 = lv_cnval1.

      lv_days = lv_cnval1.
      lv_days = lv_days + 1.

      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          date      = sy-datum
          days      = lv_days
          months    = 0
          signum    = '-'
          years     = 0
        IMPORTING
          calc_date = lv_date.

      SELECT *
        FROM zabs_hrv_rep "zabs_bgp_accom
        INTO TABLE @DATA(lt_acc_itm)
        WHERE pernr   EQ @ls_usr_emp-pernr
          AND zzhdate GE @lv_date
          AND zzhdate LE @sy-datum.

*      IF sy-subrc EQ 0.
*        SORT lt_acc_itm BY zzmackey idresource zzhdate strttim.
*        DELETE ADJACENT DUPLICATES FROM lt_acc_itm
*                              COMPARING zzmackey idresource zzhdate strttim.
*      ENDIF.

      LOOP AT lt_acc_itm INTO DATA(ls_acc_itm).
        CLEAR ls_accitems.
        MOVE-CORRESPONDING ls_acc_itm TO ls_accitems.
        ls_accitems-urole = ls_usr_emp-urole.
        APPEND ls_accitems TO et_accitems.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


METHOD get_activities.

*-- Local Declarations
  DATA: ls_message TYPE /agri/s_gprolog.

*-- Fetch the activities (productions and events) with descriptions
  SELECT idactv rstype bill actype zzactcg
    FROM /agri/fmacact
    INTO TABLE et_activities.
  IF sy-subrc EQ 0.
    DELETE et_activities WHERE zzactcg IS INITIAL.
  ENDIF.

  IF et_activities IS INITIAL.
*-- Give error message if no activities found
    CLEAR ls_message.
    ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error.
    ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
    ls_message-msgno = '098'.
    add_messages_to_msg_container( is_message = ls_message ).
    RETURN.
  ENDIF.

  SELECT idactv, description
    FROM /agri/fmacactt
    INTO TABLE @DATA(lt_fmactt)
     FOR ALL ENTRIES IN @et_activities
   WHERE idactv EQ @et_activities-idactvl
     AND spras  EQ @sy-langu.
  IF sy-subrc EQ 0.
    SORT lt_fmactt BY idactv.
  ENDIF.

  LOOP AT et_activities ASSIGNING FIELD-SYMBOL(<fs_activities>).
    READ TABLE lt_fmactt INTO DATA(ls_fmactt)
          WITH KEY idactv = <fs_activities>-idactvl
        BINARY SEARCH.
    IF sy-subrc EQ 0.
      <fs_activities>-description = ls_fmactt-description.
    ENDIF.
  ENDLOOP.

ENDMETHOD.


METHOD get_baggroup.

  DATA: lt_ac_bggrp TYPE STANDARD TABLE OF zabs_ac_bggrp,
        ls_entity   TYPE zcl_zabs_mobile_acm_mpc=>ts_baggroup.


**-- Fetch the Accomplishment items
*  CALL METHOD me->get_accomplishment_items" get_accom_itms
*    EXPORTING
*      it_leader_farms  = lt_leader_farms
*      it_farm_terrains = lt_farm_terrains
*    IMPORTING
*      et_accitems      = DATA(lt_accitems).

*-- Fetch the Accomplishment items
CALL METHOD me->get_accom_items
  EXPORTING
    io_tech_request_context = io_tech_request_context
  IMPORTING
    et_accitems             = DATA(lt_accitems).

  SORT lt_accitems BY zzmackey zzbaggp.
  DELETE ADJACENT DUPLICATES FROM lt_accitems COMPARING zzmackey zzbaggp.

  IF lt_accitems IS NOT INITIAL.
    SELECT * "mackey baggp ldcde
      FROM zabs_ac_bggrp
      INTO TABLE lt_ac_bggrp "et_entityset
       FOR ALL ENTRIES IN lt_accitems
     WHERE mackey EQ lt_accitems-zzmackey
       AND baggp  EQ lt_accitems-zzbaggp.
*       AND loaded EQ space.
    IF sy-subrc EQ 0.
      READ TABLE lt_accitems INTO DATA(ls_accitems) INDEX 1.

      LOOP AT lt_ac_bggrp INTO DATA(ls_ac_bggrp).
        CLEAR ls_entity.
        MOVE-CORRESPONDING ls_ac_bggrp TO ls_entity.
        ls_entity-zzmackey = ls_ac_bggrp-mackey.
        ls_entity-zzbaggp = ls_ac_bggrp-baggp.
        ls_entity-urole = ls_accitems-urole.
        ls_entity-pernr = ls_accitems-pernr.
        APPEND ls_entity TO et_baggroup.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD get_delta_token.

*-- Local Declarations
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


METHOD get_employee_role.

*-- Local Declarations
  DATA: lo_message_container TYPE REF TO /iwbep/if_message_container,
        ls_message           TYPE /agri/s_gprolog.

  CALL METHOD me->/iwbep/if_mgw_conv_srv_runtime~get_message_container
    RECEIVING
      ro_message_container = lo_message_container.

*-- Fetch the Employee No for the SAP User
  SELECT SINGLE * "pernr
    FROM zabs_usr_emp
    INTO es_usr_emp
   WHERE bname EQ iv_user.
  IF sy-subrc NE 0.
*-- Give error message if not mapping maintained
    CLEAR ls_message.
    ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error. "'E'
    ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class. "'ZABS_MSGCLS'
    ls_message-msgno = '086'.
    ls_message-msgv1 = iv_user.
    add_messages_to_msg_container( is_message = ls_message ).
  ENDIF.

ENDMETHOD.


METHOD get_event_justifications.

*-- Local Declarations
  DATA: ls_message TYPE /agri/s_gprolog.

*-- Fetch the event justifications with descriptions
  SELECT ejust ejdesc
    FROM zabs_event_just
    INTO TABLE et_evejust.
  IF sy-subrc NE 0.
*-- Give error message if no justifications found
    CLEAR ls_message.
    ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error.
    ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
    ls_message-msgno = '099'.
    add_messages_to_msg_container( is_message = ls_message ).
  ENDIF.

ENDMETHOD.


  METHOD get_farms_loader.

*Local decleration
    DATA: ls_farms_loader TYPE zabs_s_ac_loader_f4.

*-- Fetch the Loader from transport movement types table
    SELECT farm, int_tr_prov, loader
    FROM zabst_tran_typ
    INTO TABLE @DATA(lt_tran_typ)
    FOR ALL ENTRIES IN @it_leader_farms
    WHERE farm EQ @it_leader_farms-farm
      AND loader EQ @abap_true.

    IF sy-subrc IS INITIAL.
      SORT lt_tran_typ BY farm.

      DATA(lt_vendor) = lt_tran_typ.
      SORT lt_vendor BY int_tr_prov.
      DELETE ADJACENT DUPLICATES FROM lt_vendor COMPARING int_tr_prov.

*-- Fetch the Vendor name
      SELECT lifnr, name1
        FROM lfa1
        INTO TABLE @DATA(lt_lfa1)
         FOR ALL ENTRIES IN @lt_vendor
       WHERE lifnr EQ @lt_vendor-int_tr_prov.
      IF sy-subrc EQ 0.
        SORT lt_lfa1 BY lifnr.
      ENDIF.

      DATA(lt_farm) = lt_tran_typ.
      SORT lt_farm BY farm.
      DELETE ADJACENT DUPLICATES FROM lt_farm COMPARING farm.

*-- Fetch the farm description
      SELECT tplnr_fl, pltxt
        FROM /agri/glflot
        INTO TABLE @DATA(lt_glflot)
         FOR ALL ENTRIES IN @lt_farm
       WHERE tplnr_fl EQ @lt_farm-farm
         AND kfrst EQ @space
         AND loevm EQ @space.
      IF sy-subrc EQ 0.
        SORT lt_glflot BY tplnr_fl.
      ENDIF.
    ENDIF.

    LOOP AT it_leader_farms INTO DATA(ls_leader_farms).
**      READ TABLE lt_tran_typ INTO DATA(ls_tran_typ)
**      WITH KEY farm = ls_leader_farms-farm BINARY SEARCH.
      LOOP AT lt_tran_typ INTO DATA(ls_tran_typ)
        WHERE farm = ls_leader_farms-farm.
        IF ls_tran_typ-farm EQ ls_leader_farms-farm.
          READ TABLE lt_lfa1 INTO DATA(ls_lfa1)
          WITH KEY lifnr = ls_tran_typ-int_tr_prov BINARY SEARCH.
          READ TABLE lt_glflot INTO DATA(ls_glflot)
          WITH KEY tplnr_fl = ls_leader_farms-farm BINARY SEARCH.

          ls_farms_loader-pernr  = ls_leader_farms-pernr.
          ls_farms_loader-farm   = ls_leader_farms-farm.
          ls_farms_loader-pltxt  = ls_glflot-pltxt.
          ls_farms_loader-loader = ls_tran_typ-int_tr_prov.
          ls_farms_loader-name1  = ls_lfa1-name1.
          APPEND ls_farms_loader TO et_farms_loader.
          CLEAR: ls_farms_loader, ls_lfa1, ls_glflot.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    SORT et_farms_loader BY loader.
*    DELETE ADJACENT DUPLICATES FROM et_farms_loader COMPARING loader.

  ENDMETHOD.


METHOD get_farm_terrains.

*-- Local Declarations
  DATA:
*        lv_datab         TYPE datum,
*        lv_datbi         TYPE datum,
    ls_farm_terrains TYPE zcl_zabs_mobile_acm_mpc=>ts_farmterrains,
    ls_ac_data       TYPE zabs_s_ac_mob_data,
    ls_message       TYPE /agri/s_gprolog,
    lt_tplnr         TYPE /agri/t_gltplnr,
    lt_carimbo       TYPE tt_carimbo,
    lv_cnval3        TYPE zabs_del_cnval,
    lv_prevdate      TYPE p0001-begda,
    lv_days          TYPE t5a4a-dlydy,
    lv_low_gstrp     TYPE sy-datum,
    lv_high_gstrp    TYPE sy-datum,
    lv_cnval1_gstrp  TYPE zabs_del_cnval,
    ltr_gstrp        TYPE RANGE OF co_gstrp.

  IF it_leader_farms IS NOT INITIAL.
*-- Fetch the required accomplishment data to create
    CALL METHOD me->get_accomplish_data
      IMPORTING
        es_ac_data = ls_ac_data.

*-- Fetch the terrains under the farms
    SELECT tplnr_fl, tplma
      FROM /agri/glflot
      INTO TABLE @DATA(lt_flot)
       FOR ALL ENTRIES IN @it_leader_farms
     WHERE tplma EQ @it_leader_farms-farm
       AND kfrst EQ @space
       AND loevm EQ @space.
    IF sy-subrc NE 0.
*-- Give error message if no terrains found under the farms
      CLEAR ls_message.
      ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error.
      ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
      ls_message-msgno = '094'.
      add_messages_to_msg_container( is_message = ls_message ).
    ENDIF.

    SORT lt_flot BY tplnr_fl.

*-- Fetch Carimbo and description
    lt_tplnr = lt_flot.
    CALL METHOD me->fetch_carimbo
      EXPORTING
        it_tplnr   = lt_tplnr
      CHANGING
        ct_carimbo = lt_carimbo.

**-- Fetch the terrain description
*    SELECT tplnr_fl, pltxt
*      FROM /agri/glflotx
*      INTO TABLE @DATA(lt_flotx)
*       FOR ALL ENTRIES IN @lt_flot
*     WHERE tplnr_fl EQ @lt_flot-tplnr_fl
*       AND spras    EQ @sy-langu.
*    IF sy-subrc EQ 0.
*      SORT lt_flotx BY tplnr_fl.
*    ENDIF.

**-- Get the week start and end date of the current week
*    CALL FUNCTION 'GET_WEEK_INFO_BASED_ON_DATE'
*      EXPORTING
*        date   = sy-datum
*      IMPORTING
*        monday = lv_datab
*        sunday = lv_datbi.

*--Get variant table data
    CALL METHOD zcl_abs_get_variants=>get_constant_single
      EXPORTING
        iv_mod    = space
        iv_objid  = zcl_abs_abap_maintain=>c_objid_mobile "'MOBC'
        iv_k1val  = zcl_abs_abap_maintain=>c_key_irr_days "'DAYS'
      IMPORTING
        ev_cnval3 = lv_cnval3. "'60'

    lv_days = lv_cnval3.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = sy-datum
        days      = lv_days
        months    = 0
        signum    = '-'
        years     = 0
      IMPORTING
        calc_date = lv_prevdate.

*-- Fetch the active crop seasons in the current day
    SELECT tplnr_fl, contr, ymatnr
      FROM /agri/glflcma
      INTO TABLE @DATA(lt_flcma)
       FOR ALL ENTRIES IN @lt_flot
     WHERE tplnr_fl EQ @lt_flot-tplnr_fl
       AND datab    LE @sy-datum
       AND datbi    GE @lv_prevdate
       AND astat    EQ @zcl_abs_abap_maintain=>c_cs_active
       AND loevm    EQ @space.
    IF sy-subrc NE 0.
*-- Give the error message if no active crop seasons found
      CLEAR ls_message.
      ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error.
      ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
      ls_message-msgno = '095'.
      add_messages_to_msg_container( is_message = ls_message ).
    ENDIF.

    SORT lt_flcma BY tplnr_fl contr.

    DATA(lt_matnr) = lt_flcma.
    SORT lt_matnr BY ymatnr.
    DELETE lt_matnr WHERE ymatnr IS INITIAL.
    DELETE ADJACENT DUPLICATES FROM lt_matnr COMPARING ymatnr.

    IF lt_matnr IS NOT INITIAL.
*-- Fetch Yard material description
      SELECT matnr, maktx
        FROM makt
        INTO TABLE @DATA(lt_makt)
         FOR ALL ENTRIES IN @lt_matnr
       WHERE matnr EQ @lt_matnr-ymatnr
         AND spras EQ @sy-langu.
      IF sy-subrc EQ 0.
        SORT lt_makt BY matnr.
      ENDIF.
    ENDIF.

    CALL METHOD zcl_abs_get_variants=>get_constant_single
      EXPORTING
        iv_mod    = space
        iv_objid  = zcl_abs_abap_maintain=>c_objid_mobile "'MOBC'
        iv_k1val  = zcl_abs_abap_maintain=>c_key_lgstrp   "'LGSTRP'
      IMPORTING
        ev_cnval1 = lv_cnval1_gstrp. "'4'
    CONDENSE lv_cnval1_gstrp.
    lv_low_gstrp   = sy-datum - lv_cnval1_gstrp.

    CALL METHOD zcl_abs_get_variants=>get_constant_single
      EXPORTING
        iv_mod    = space
        iv_objid  = zcl_abs_abap_maintain=>c_objid_mobile "'MOBC'
        iv_k1val  = zcl_abs_abap_maintain=>c_key_hgstrp   "'HGSTRP'
      IMPORTING
        ev_cnval1 = lv_cnval1_gstrp. "'1'
    CONDENSE lv_cnval1_gstrp.
    lv_high_gstrp  = sy-datum + lv_cnval1_gstrp.

    ltr_gstrp = VALUE #( sign = zcl_abs_abap_maintain=>c_rsign_include
                       option = zcl_abs_abap_maintain=>c_ropt_between " 'BT'
                       ( low = lv_low_gstrp
                         high = lv_high_gstrp ) ).

*-- Fetch the harvest orders created for the terrains in current week
    SELECT aufnr, tplnr_fl, contr, tplma, matnr, gstrp
      FROM /agri/fmfphdr
      INTO TABLE @DATA(lt_fmfphdr)
       FOR ALL ENTRIES IN @lt_flcma
     WHERE autyp    EQ @zcl_abs_abap_maintain=>c_autyp_tsk_ord "'TO'
       AND tplnr_fl EQ @lt_flcma-tplnr_fl
       AND contr    EQ @lt_flcma-contr
       AND matnr    EQ @ls_ac_data-matnr
       AND gstrp    IN @ltr_gstrp
       AND tecom    EQ @space.
    IF sy-subrc NE 0.
*-- Give error message found if no harvest order created in the current week
      CLEAR ls_message.
      ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error.
      ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
      ls_message-msgno = '093'.
      add_messages_to_msg_container( is_message = ls_message ).
    ENDIF.

    SORT lt_fmfphdr BY tplnr_fl gstrp DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_fmfphdr COMPARING tplnr_fl.

    READ TABLE it_leader_farms INTO DATA(ls_leader_farms) INDEX 1.

    LOOP AT lt_fmfphdr ASSIGNING FIELD-SYMBOL(<fs_fmfphdr>).
*-- Populate the terrains with harvest orders
      CLEAR ls_farm_terrains.
      ls_farm_terrains-tplnr  = <fs_fmfphdr>-tplnr_fl.
*      READ TABLE lt_flotx INTO DATA(ls_flotx)
*            WITH KEY tplnr_fl = ls_farm_terrains-tplnr
*          BINARY SEARCH.
*      IF sy-subrc EQ 0.
**-- Populate the terrain description
*        ls_farm_terrains-tpltxt = ls_flotx-pltxt.
*      ENDIF.

      READ TABLE lt_flcma INTO DATA(ls_flcma)
            WITH KEY tplnr_fl = <fs_fmfphdr>-tplnr_fl
                     contr    = <fs_fmfphdr>-contr
          BINARY SEARCH.
      IF sy-subrc EQ 0.
        READ TABLE lt_makt INTO DATA(ls_makt)
              WITH KEY matnr = ls_flcma-ymatnr
            BINARY SEARCH.
        IF sy-subrc EQ 0.
*-- Populate yard material description
          ls_farm_terrains-ymatnr = ls_flcma-ymatnr.
          ls_farm_terrains-maktx  = ls_makt-maktx+7.
        ENDIF.
      ENDIF.

*-- Read Carimbo and description
      READ TABLE lt_carimbo INTO DATA(ls_carimbo)
            WITH KEY tplnr = <fs_fmfphdr>-tplnr_fl
          BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_farm_terrains-carimbo = ls_carimbo-carimbo.
        ls_farm_terrains-toclhtt = ls_carimbo-toclhtt.
      ENDIF.

      ls_farm_terrains-farm   = <fs_fmfphdr>-tplma.
      ls_farm_terrains-aufnr  = <fs_fmfphdr>-aufnr.
      ls_farm_terrains-tmatnr = <fs_fmfphdr>-matnr.
      ls_farm_terrains-urole  = ls_leader_farms-urole.
      ls_farm_terrains-pernr  = ls_leader_farms-pernr.
      APPEND ls_farm_terrains TO et_farm_terrains.
    ENDLOOP.
  ENDIF.

ENDMETHOD.


METHOD get_intrbatch.

*--Fetching Internal batch data
  SELECT *
    FROM zabs_intr_batch
    INTO TABLE @DATA(lt_intr_batch).

  MOVE-CORRESPONDING lt_intr_batch TO et_intr_batch.

ENDMETHOD.


METHOD get_leader_farms.

*-- Local Declarations
  DATA: lt_turma_head   TYPE zcl_zabs_mobile_acm_mpc=>tt_turmahead,
        ls_message      TYPE /agri/s_gprolog,
        ls_leader_forms TYPE zcl_zabs_mobile_acm_mpc=>ts_leadfarms.

*-- Fetch the turmas of leader or incharge
  CALL METHOD me->get_turma_header
    EXPORTING
      io_tech_request_context = io_tech_request_context
    IMPORTING
      et_turma_head           = lt_turma_head.

*  CALL METHOD me->get_turma_header
*    IMPORTING
*      et_turma_head = lt_turma_head.

  IF lt_turma_head IS NOT INITIAL.
    LOOP AT lt_turma_head INTO DATA(ls_turma_head).
      CLEAR ls_leader_forms.
      ls_leader_forms-lider_turma = ls_turma_head-lider_turma.
      ls_leader_forms-zzturma = ls_turma_head-zzturma.

      IF ls_turma_head-fazenda1 IS NOT INITIAL.
        ls_leader_forms-farm = ls_turma_head-fazenda1.
        APPEND ls_leader_forms TO et_leader_farms.
      ENDIF.

      IF ls_turma_head-fazenda2 IS NOT INITIAL.
        ls_leader_forms-farm = ls_turma_head-fazenda2.
        APPEND ls_leader_forms TO et_leader_farms.
      ENDIF.

      IF ls_turma_head-fazenda3 IS NOT INITIAL.
        ls_leader_forms-farm = ls_turma_head-fazenda3.
        APPEND ls_leader_forms TO et_leader_farms.
      ENDIF.

      IF ls_turma_head-fazenda4 IS NOT INITIAL.
        ls_leader_forms-farm = ls_turma_head-fazenda4.
        APPEND ls_leader_forms TO et_leader_farms.
      ENDIF.

      IF ls_turma_head-fazenda5 IS NOT INITIAL.
        ls_leader_forms-farm = ls_turma_head-fazenda5.
        APPEND ls_leader_forms TO et_leader_farms.
      ENDIF.
    ENDLOOP.

    IF et_leader_farms IS INITIAL.
*-- Give error message if no farms found under the leaders
      CLEAR ls_message.
      ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error.
      ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
      ls_message-msgno = '092'.
      add_messages_to_msg_container( is_message = ls_message ).
      RETURN.
    ENDIF.

    DATA(lt_tplnr) = et_leader_farms.
    SORT lt_tplnr BY farm.
    DELETE ADJACENT DUPLICATES FROM lt_tplnr COMPARING farm.

    IF lt_tplnr IS NOT INITIAL.
*-- Fetch the plant of the farm
      SELECT tplnr_fl, iwerk
        FROM /agri/glflot
        INTO TABLE @DATA(lt_flot)
         FOR ALL ENTRIES IN @lt_tplnr
       WHERE tplnr_fl EQ @lt_tplnr-farm
         AND kfrst EQ @space
         AND loevm EQ @space.
      IF sy-subrc EQ 0.
        SORT lt_flot BY tplnr_fl.
      ENDIF.

*-- Fetch the description of farms
      SELECT tplnr_fl, pltxt
        FROM /agri/glflotx
        INTO TABLE @DATA(lt_flotx)
         FOR ALL ENTRIES IN @lt_tplnr
       WHERE tplnr_fl EQ @lt_tplnr-farm
         AND spras    EQ @sy-langu.
      IF sy-subrc EQ 0.
        SORT lt_flotx BY tplnr_fl.
      ENDIF.
    ENDIF.

    CLEAR ls_turma_head.
    READ TABLE lt_turma_head INTO ls_turma_head INDEX 1.

    LOOP AT et_leader_farms ASSIGNING FIELD-SYMBOL(<fs_leader_farms>).
*-- Populate the plant of the farm
      READ TABLE lt_flot INTO DATA(ls_flot)
            WITH KEY tplnr_fl = <fs_leader_farms>-farm
          BINARY SEARCH.
      IF sy-subrc EQ 0.
        <fs_leader_farms>-werks = ls_flot-iwerk.
      ENDIF.

*-- Populate the farm descriptions
      READ TABLE lt_flotx INTO DATA(ls_flotx)
            WITH KEY tplnr_fl = <fs_leader_farms>-farm
          BINARY SEARCH.
      IF sy-subrc EQ 0.
        <fs_leader_farms>-fpltxt = ls_flotx-pltxt.
      ENDIF.

      <fs_leader_farms>-urole = ls_turma_head-urole.
      <fs_leader_farms>-pernr = ls_turma_head-pernr.
    ENDLOOP.
  ENDIF.

ENDMETHOD.


METHOD get_loadbaggroup.

*--workarea declaration
  DATA : ls_usr_emp    TYPE zabs_s_ac_usr_role,
*         ls_usr_emp   TYPE zabs_usr_emp,
         ls_bggrp      TYPE zabs_ac_bggrp,
         ls_ldbaggrp   TYPE zabs_s_ac_ldbaggrp,
         ls_entityset  TYPE zcl_zabs_mobile_acm_mpc=>ts_loadbaggroup,

*--Internal table declaration
         lt_bggrp      TYPE TABLE OF zabs_ac_bggrp,
         lt_ldbaggrp   TYPE TABLE OF zabs_s_ac_ldbaggrp,

*--Variable declaration
         lv_banca      TYPE zabs_del_banca,
         lv_bdate      TYPE p0001-begda,
         lv_days       TYPE t5a4a-dlydy,
         lv_cnval      TYPE zabs_del_cnval,
         lt_filter     TYPE /iwbep/t_mgw_select_option,
         ls_filter     TYPE /iwbep/s_mgw_select_option,
         lv_filter_str TYPE string,
         lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
         ltr_urole     TYPE RANGE OF zabs_del_urole,
         lsr_urole     LIKE LINE OF ltr_urole,
         ltr_lifnr     TYPE RANGE OF lifnr,
         lsr_lifnr     LIKE LINE OF ltr_lifnr,
         ltr_farm      TYPE RANGE OF /agri/gltplnr_fl,
         lsr_farm      LIKE LINE OF ltr_farm.

**--Calling method to get user details
*  CALL METHOD me->get_employee_role
*    EXPORTING
*      iv_user    = sy-uname
*    IMPORTING
*      es_usr_emp = ls_usr_emp.

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
      WHEN 'UROLE'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = ltr_urole ).

        READ TABLE ltr_urole INTO lsr_urole INDEX 1.
        IF sy-subrc = 0.
          ls_usr_emp-urole = lsr_urole-low.
        ENDIF.

      WHEN 'LIFNR'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = ltr_lifnr ).

        READ TABLE ltr_lifnr INTO lsr_lifnr INDEX 1.
        IF sy-subrc = 0.
          ls_usr_emp-lifnr = lsr_lifnr-low.
        ENDIF.

*-------Start of Wave 3 change 11.11.2020 T_C.KARANAM
      WHEN 'FARM'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = ltr_farm ).

        READ TABLE ltr_farm INTO lsr_farm INDEX 1.
        IF sy-subrc = 0.
          ls_usr_emp-farm = lsr_farm-low.
        ENDIF.
*-------End of Wave 3 change 11.11.2020 T_C.KARANAM

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

  IF ls_usr_emp-urole = 'LD'.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = ls_usr_emp-lifnr
      IMPORTING
        output = ls_usr_emp-lifnr.

    CALL METHOD zcl_abs_get_variants=>get_constant_single
      EXPORTING
        iv_objid  = 'MOBC'
        iv_k1val  = 'DAYS'
        iv_k2val  = 'LDAY'
      IMPORTING
        ev_cnval1 = lv_cnval. "'3'

    lv_days = lv_cnval.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = sy-datum
        days      = lv_days
        months    = 0
        signum    = '-'
        years     = 0
      IMPORTING
        calc_date = lv_bdate.

*-------Start of Wave 3 change 11.11.2020 T_C.KARANAM
    CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
      EXPORTING
        input      = ls_usr_emp-farm
      IMPORTING
        output     = ls_usr_emp-farm
      EXCEPTIONS
        not_found  = 1
        not_active = 2
        OTHERS     = 3.

    SELECT SINGLE tplnr_fl, swerk
      FROM /agri/glflot
      INTO @DATA(ls_glflot)
     WHERE tplnr_fl EQ @ls_usr_emp-farm
       AND swerk NE @space.

*-------End of Wave 3 change 11.11.2020 T_C.KARANAM

*--Accomplishment Bag Groups
    SELECT *
      FROM zabs_ac_bggrp
      INTO TABLE lt_bggrp
     WHERE ldcde  EQ ls_usr_emp-lifnr
       AND loaded EQ space
       AND ersda  BETWEEN lv_bdate AND sy-datum
       AND werks EQ ls_glflot-swerk." Wave 3 change 11.11.2020
*       AND ersda  LE sy-datum.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
  ENDIF.

  LOOP AT lt_bggrp INTO ls_bggrp.
    MOVE-CORRESPONDING ls_bggrp TO ls_ldbaggrp.
    ls_ldbaggrp-zzmackey = ls_bggrp-mackey.
    ls_ldbaggrp-zzbaggp  = ls_bggrp-baggp.
*      ls_ldbaggrp-zzbagcn  = ls_bggrp-zzbagcn.
*      ls_ldbaggrp-tot_boxes  = ls_bggrp-tot_boxes.
*      ls_ldbaggrp-avg_cxn  = ls_bggrp-avg_cxn.
*      ls_ldbaggrp-ibag_no  = ls_bggrp-ibag_no.
*      ls_ldbaggrp-fbag_no  = ls_bggrp-fbag_no.
*      ls_ldbaggrp-ldcde  = ls_bggrp-ldcde.
*      ls_ldbaggrp-loaded = ls_bggrp-loaded.
*      ls_ldbaggrp-zzturma = ls_bggrp-zzturma.
*      ls_ldbaggrp-tplnr   = ls_bggrp-tplnr.
*      ls_ldbaggrp-tmatnr = ls_bggrp-tmatnr.
*      ls_ldbaggrp-aufnr   = ls_bggrp-aufnr.
*      ls_ldbaggrp-werks   = ls_bggrp-werks.
*      ls_ldbaggrp-lider_turma = ls_bggrp-lider_turma.
    CONCATENATE ls_bggrp-ibag_no ls_bggrp-fbag_no INTO lv_banca
    SEPARATED BY '-'.
    ls_ldbaggrp-banca = lv_banca.

    APPEND ls_ldbaggrp TO lt_ldbaggrp.
    CLEAR ls_ldbaggrp.
  ENDLOOP.

  LOOP AT lt_ldbaggrp INTO ls_ldbaggrp.
    MOVE-CORRESPONDING ls_ldbaggrp TO ls_entityset.
    ls_entityset-urole = ls_usr_emp-urole.
    ls_entityset-lifnr = ls_usr_emp-lifnr.
    ls_entityset-farm  = ls_usr_emp-farm.
    APPEND ls_entityset TO et_loadbaggroup.
    CLEAR ls_entityset.
  ENDLOOP.

ENDMETHOD.


  METHOD get_loader_farms.

*--Local declaration
    DATA : ls_usr_emp    TYPE zabs_s_ac_usr_role,
           ls_entityset  TYPE zabs_s_ac_load_farms,

           lt_filter     TYPE /iwbep/t_mgw_select_option,
           ls_filter     TYPE /iwbep/s_mgw_select_option,
           lv_filter_str TYPE string,
           lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
           ltr_urole     TYPE RANGE OF zabs_del_urole,
           lsr_urole     LIKE LINE OF ltr_urole,
           ltr_lifnr     TYPE RANGE OF lifnr,
           lsr_lifnr     LIKE LINE OF ltr_lifnr.

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
**        WHEN 'UROLE'.
**          lo_filter->convert_select_option(
**            EXPORTING
**              is_select_option = ls_filter
**            IMPORTING
**              et_select_option = ltr_urole ).
**
**          READ TABLE ltr_urole INTO lsr_urole INDEX 1.
**          IF sy-subrc = 0.
**            ls_usr_emp-urole = lsr_urole-low.
**          ENDIF.

        WHEN 'LOADER'.
          lo_filter->convert_select_option(
            EXPORTING
              is_select_option = ls_filter
            IMPORTING
              et_select_option = ltr_lifnr ).

          READ TABLE ltr_lifnr INTO lsr_lifnr INDEX 1.
          IF sy-subrc = 0.
            ls_usr_emp-lifnr = lsr_lifnr-low.
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

*-- Fetch the Farms from transport movement types table
    SELECT farm, int_tr_prov, loader
    FROM zabst_tran_typ
    INTO TABLE @DATA(lt_tran_typ)
    WHERE int_tr_prov EQ @ls_usr_emp-lifnr
      AND loader EQ @abap_true.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF sy-subrc IS INITIAL.
      DATA(lt_vendor) = lt_tran_typ.
      SORT lt_vendor BY int_tr_prov.
      DELETE ADJACENT DUPLICATES FROM lt_vendor COMPARING int_tr_prov.

*-- Fetch the Vendor name
      SELECT lifnr, name1
        FROM lfa1
        INTO TABLE @DATA(lt_lfa1)
         FOR ALL ENTRIES IN @lt_vendor
       WHERE lifnr EQ @lt_vendor-int_tr_prov.
      IF sy-subrc EQ 0.
        SORT lt_lfa1 BY lifnr.
      ENDIF.

      DATA(lt_farm) = lt_tran_typ.
      SORT lt_farm BY farm.
      DELETE ADJACENT DUPLICATES FROM lt_farm COMPARING farm.

*-- Fetch the farm description
      SELECT tplnr_fl, pltxt
        FROM /agri/glflot
        INTO TABLE @DATA(lt_glflot)
         FOR ALL ENTRIES IN @lt_farm
       WHERE tplnr_fl EQ @lt_farm-farm
         AND kfrst EQ @space
         AND loevm EQ @space.
      IF sy-subrc EQ 0.
        SORT lt_glflot BY tplnr_fl.
      ENDIF.
**    ENDIF.
**
**    IF sy-subrc IS INITIAL.
      SORT lt_tran_typ BY farm.
      DELETE ADJACENT DUPLICATES FROM lt_tran_typ COMPARING farm.

      LOOP AT lt_tran_typ INTO DATA(ls_tran_typ).
        READ TABLE lt_lfa1 INTO DATA(ls_lfa1)
        WITH KEY lifnr = ls_tran_typ-int_tr_prov BINARY SEARCH.
        READ TABLE lt_glflot INTO DATA(ls_glflot)
        WITH KEY tplnr_fl = ls_tran_typ-farm BINARY SEARCH.

        ls_entityset-loader  = ls_tran_typ-int_tr_prov.
        ls_entityset-name1   = ls_lfa1-name1.
        ls_entityset-farm    = ls_tran_typ-farm.
        ls_entityset-pltxt   = ls_glflot-pltxt.

        APPEND ls_entityset TO et_loader_farms.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


METHOD get_trantyp.

*-- Fetch the transport movement types
  SELECT *
    FROM zabst_tran_typ
    INTO TABLE @DATA(lt_tran_typ)
*    WHERE loader EQ @abap_true.
    WHERE in_licplate NE @space.
  IF sy-subrc EQ 0.
    DATA(lt_farm) = lt_tran_typ.
    SORT lt_farm BY farm.
    DELETE ADJACENT DUPLICATES FROM lt_farm COMPARING farm.

*-- Fetch the farm description
    SELECT tplnr_fl, pltxt
      FROM /agri/glflot
      INTO TABLE @DATA(lt_glflot)
       FOR ALL ENTRIES IN @lt_farm
     WHERE tplnr_fl EQ @lt_farm-farm
       AND kfrst EQ @space
       AND loevm EQ @space.
    IF sy-subrc EQ 0.
      SORT lt_glflot BY tplnr_fl.
    ENDIF.

    DATA(lt_vendor) = lt_tran_typ.
    SORT lt_vendor BY int_tr_prov.
    DELETE ADJACENT DUPLICATES FROM lt_vendor COMPARING int_tr_prov.

*-- Fetch the Vendor name
    SELECT lifnr, name1
      FROM lfa1
      INTO TABLE @DATA(lt_lfa1)
       FOR ALL ENTRIES IN @lt_vendor
     WHERE lifnr EQ @lt_vendor-int_tr_prov.
    IF sy-subrc EQ 0.
      SORT lt_lfa1 BY lifnr.
    ENDIF.

    MOVE-CORRESPONDING lt_tran_typ TO et_trantyp.
  ENDIF.

  LOOP AT et_trantyp ASSIGNING FIELD-SYMBOL(<fs_entityset>).
    READ TABLE lt_glflot INTO DATA(ls_glflot)
          WITH KEY tplnr_fl = <fs_entityset>-farm
        BINARY SEARCH.
    IF sy-subrc EQ 0.
      <fs_entityset>-pltxt = ls_glflot-pltxt.
    ENDIF.

    READ TABLE lt_lfa1 INTO DATA(ls_lfa1)
          WITH KEY lifnr = <fs_entityset>-int_tr_prov
        BINARY SEARCH.
    IF sy-subrc EQ 0.
      <fs_entityset>-name1 = ls_lfa1-name1.
    ENDIF.
  ENDLOOP.

ENDMETHOD.


METHOD get_turma_header.

*-- Local Declarations
  DATA: ls_message    TYPE /agri/s_gprolog,
*        ls_usr_emp TYPE zabs_usr_emp.
        ls_usr_emp    TYPE zabs_s_ac_usr_role,
        lt_filter     TYPE /iwbep/t_mgw_select_option,
        ls_filter     TYPE /iwbep/s_mgw_select_option,
        lv_filter_str TYPE string,
        lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
        ltr_urole     TYPE RANGE OF zabs_del_urole,
        lsr_urole     LIKE LINE OF ltr_urole,
        "Begin of José Sequeira - 09.02.2021 14:25:26
        lt_hdr        TYPE TABLE OF zfmfpgrouphdr,
        ls_hdr        TYPE zfmfpgrouphdr,
        ls_turma_head TYPE zabs_s_ac_turma_head,
        "End of José Sequeira - 09.02.2021 14:25:26
        ltr_pernr     TYPE RANGE OF persno,
        lsr_pernr     LIKE LINE OF ltr_pernr.

*-- Get the employee number and role
*  CALL METHOD me->get_employee_role
*    EXPORTING
*      iv_user    = sy-uname
*    IMPORTING
*      es_usr_emp = ls_usr_emp.

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
      WHEN 'UROLE'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = ltr_urole ).

        READ TABLE ltr_urole INTO lsr_urole INDEX 1.
        IF sy-subrc = 0.
          ls_usr_emp-urole = lsr_urole-low.
        ENDIF.

      WHEN 'PERNR'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = ltr_pernr ).

        READ TABLE ltr_pernr INTO lsr_pernr INDEX 1.
        IF sy-subrc = 0.
*          ls_usr_emp-pernr = lsr_pernr-low.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = lsr_pernr-low
            IMPORTING
              output = ls_usr_emp-pernr.

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

  IF NOT ( ls_usr_emp-urole EQ zcl_abs_abap_maintain=>c_role_leader "'LE'
      OR   ls_usr_emp-urole EQ zcl_abs_abap_maintain=>c_role_incharge ). "'IN'
    RETURN.
  ENDIF.

  IF ls_usr_emp-urole EQ zcl_abs_abap_maintain=>c_role_leader. "'LE'
*-- Fetch the turma of the leader
    SELECT turma_id turma_text encarregado lider_turma
           fazenda1 fazenda2 fazenda3 fazenda4 fazenda5
      FROM zfmfpgrouphdr
      INTO TABLE et_turma_head
     WHERE status      EQ zcl_abs_abap_maintain=>c_turstat_active "'01'
       AND lider_turma EQ ls_usr_emp-pernr.
    IF sy-subrc NE 0.
*-- Give error message if no turma maintained for the leader
      CLEAR ls_message.
      ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error.
      ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
      ls_message-msgno = '089'.
      ls_message-msgv1 = ls_usr_emp-pernr.
      add_messages_to_msg_container( is_message = ls_message ).
    ENDIF.
  ELSEIF ls_usr_emp-urole EQ zcl_abs_abap_maintain=>c_role_incharge. "'IN'
*-- Fetch the turma of the incharge
    SELECT turma_id turma_text encarregado lider_turma
           fazenda1 fazenda2 fazenda3 fazenda4 fazenda5
      FROM zfmfpgrouphdr
      INTO TABLE et_turma_head
     WHERE status      EQ zcl_abs_abap_maintain=>c_turstat_active "'01'
       AND encarregado EQ ls_usr_emp-pernr.
    IF sy-subrc NE 0.
      "Begin of José Sequeira - 09.02.2021 14:18:38
      "First look for IA, before the error message...
      SELECT turma_id turma_text encarregado lider_turma
             fazenda1 fazenda2 fazenda3 fazenda4 fazenda5 encarregado_adm
        FROM zfmfpgrouphdr
        INTO CORRESPONDING FIELDS OF TABLE lt_hdr
       WHERE status           EQ zcl_abs_abap_maintain=>c_turstat_active "'01'
         AND encarregado_adm  EQ ls_usr_emp-pernr.
      IF sy-subrc NE 0.
*-- Give error message if no turma maintained for the incharge
        CLEAR ls_message.
        ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error. "'E'
        ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
        ls_message-msgno = '090'.
        ls_message-msgv1 =  ls_usr_emp-pernr.
        add_messages_to_msg_container( is_message = ls_message ).
      ELSE.
        LOOP AT lt_hdr INTO ls_hdr.
          ls_hdr-encarregado = ls_hdr-encarregado_adm."Move the In charge ADM to regular in charge...
          MOVE-CORRESPONDING ls_hdr TO ls_turma_head.
          ls_turma_head-zzturma = ls_hdr-turma_id.
          APPEND ls_turma_head TO et_turma_head.
        ENDLOOP.
      ENDIF.
      "End of José Sequeira - 09.02.2021 14:18:38
    ENDIF.
  ENDIF.

  SORT et_turma_head BY zzturma.

  LOOP AT et_turma_head ASSIGNING FIELD-SYMBOL(<fs_turma_head>).
    <fs_turma_head>-urole = ls_usr_emp-urole.
    <fs_turma_head>-pernr = ls_usr_emp-pernr.
  ENDLOOP.

ENDMETHOD.


METHOD get_turma_items.

*-- Local Declarations
  DATA: lt_turma_head TYPE zcl_zabs_mobile_acm_mpc=>tt_turmahead,
        ls_message    TYPE /agri/s_gprolog,
        lv_cnval1     TYPE zabs_del_cnval.

*-- Fetch the turmas of leader or incharge
  CALL METHOD me->get_turma_header
    EXPORTING
      io_tech_request_context = io_tech_request_context
    IMPORTING
      et_turma_head           = lt_turma_head.

*  CALL METHOD me->get_turma_header
*    IMPORTING
*      et_turma_head = lt_turma_head.

  IF lt_turma_head IS NOT INITIAL.
*-- Fetch the employees (harvesters) under the turmas
    SELECT turma_id pernr idresource
      FROM zfmfpgroupitm
      INTO TABLE et_turma_items
       FOR ALL ENTRIES IN lt_turma_head
     WHERE turma_id EQ lt_turma_head-zzturma.
    IF sy-subrc NE 0.
*-- Give error message if no employees found under turmas
      CLEAR ls_message.
      ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error.
      ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
      ls_message-msgno = '091'.
      add_messages_to_msg_container( is_message = ls_message ).
    ENDIF.

*--Get variant table data
    CALL METHOD zcl_abs_get_variants=>get_constant_single
      EXPORTING
        iv_mod    = space
        iv_objid  = zcl_abs_abap_maintain=>c_objid_hcm_wrkc "'HCM'
        iv_k1val  = zcl_abs_abap_maintain=>c_key_work_center "'WRKC'
      IMPORTING
        ev_cnval1 = lv_cnval1.

*-- Fetch the Resource description
    SELECT idresource, description
      FROM /agri/fmacres
      INTO TABLE @DATA(lt_fmacres)
       FOR ALL ENTRIES IN @et_turma_items
     WHERE idresource EQ @et_turma_items-idresource
       AND arbpl      EQ @lv_cnval1.
    IF sy-subrc EQ 0.
      SORT lt_fmacres BY idresource.
      LOOP AT et_turma_items ASSIGNING FIELD-SYMBOL(<fs_turma_items>).
        READ TABLE lt_fmacres INTO DATA(ls_fmacres)
              WITH KEY idresource = <fs_turma_items>-idresource
            BINARY SEARCH.
        IF sy-subrc EQ 0.
*-- Popilate resource description
          <fs_turma_items>-descr = ls_fmacres-description.
        ENDIF.
      ENDLOOP.
    ENDIF.

    READ TABLE lt_turma_head INTO DATA(ls_turma_head) INDEX 1.
    IF sy-subrc EQ 0.
      LOOP AT et_turma_items ASSIGNING <fs_turma_items>.
        <fs_turma_items>-urole = ls_turma_head-urole.
        <fs_turma_items>-pernr = ls_turma_head-pernr.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDMETHOD.


 METHOD get_usr_role.

*-- Local Declarations
   DATA : lt_filter     TYPE /iwbep/t_mgw_select_option,
          ls_filter     TYPE /iwbep/s_mgw_select_option,
          lv_filter_str TYPE string,
          lv_cnval1     TYPE zabs_del_cnval,
          lv_days       TYPE t5a4a-dlydy,
*          lv_date       TYPE sy-datum,
          lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
          ls_message    TYPE /agri/s_gprolog,
          ltr_badge     TYPE RANGE OF zabs_del_badge,
          lsr_badge     LIKE LINE OF ltr_badge,
          ltr_urole     TYPE RANGE OF zabs_del_urole,
          lsr_urole     LIKE LINE OF ltr_urole,
          ltr_pernr     TYPE RANGE OF persno,
          lsr_pernr     LIKE LINE OF ltr_pernr,
          ltr_lifnr     TYPE RANGE OF lifnr,
          lsr_lifnr     LIKE LINE OF ltr_lifnr,
          ltr_farm      TYPE RANGE OF zabs_del_farm,
          lsr_farm      LIKE LINE OF ltr_farm,
          ls_usr_role   TYPE zabs_s_ac_usr_role.

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
       WHEN 'BADGE'.
         lo_filter->convert_select_option(
           EXPORTING
             is_select_option = ls_filter
           IMPORTING
             et_select_option = ltr_badge ).

       WHEN 'UROLE'.
         lo_filter->convert_select_option(
           EXPORTING
             is_select_option = ls_filter
           IMPORTING
             et_select_option = ltr_urole ).

       WHEN 'PERNR'.
         lo_filter->convert_select_option(
           EXPORTING
             is_select_option = ls_filter
           IMPORTING
             et_select_option = ltr_pernr ).

       WHEN 'LIFNR'.
         lo_filter->convert_select_option(
           EXPORTING
             is_select_option = ls_filter
           IMPORTING
             et_select_option = ltr_lifnr ).

       WHEN 'FARM'.
         lo_filter->convert_select_option(
           EXPORTING
             is_select_option = ls_filter
           IMPORTING
             et_select_option = ltr_farm ).

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

*--Get variant table data
   CALL METHOD zcl_abs_get_variants=>get_constant_single
     EXPORTING
       iv_mod    = space
       iv_objid  = zcl_abs_abap_maintain=>c_objid_mobile "'MOBC'
       iv_k1val  = zcl_abs_abap_maintain=>c_key_leader "'LEAD'
       iv_k2val  = 'PDAYS'
     IMPORTING
       ev_cnval1 = lv_cnval1.

   lv_days = lv_cnval1.
   lv_days = lv_days + 1.

   ls_usr_role-no_days = lv_days.

   READ TABLE ltr_badge INTO lsr_badge INDEX 1.
   IF sy-subrc = 0.
     ls_usr_role-badge = lsr_badge-low.
   ENDIF.

   READ TABLE ltr_urole INTO lsr_urole INDEX 1.
   IF sy-subrc = 0.
     ls_usr_role-urole = lsr_urole-low.

     CASE lsr_urole-low.
       WHEN 'LE' OR 'IN'.
         READ TABLE ltr_pernr INTO lsr_pernr INDEX 1.
         IF sy-subrc = 0.

           CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
             EXPORTING
               input  = lsr_pernr-low
             IMPORTING
               output = ls_usr_role-pernr.

*--Validating Personnel Number
           SELECT SINGLE pernr
             FROM pa0000
             INTO @DATA(lv_pernr)
            WHERE pernr EQ @ls_usr_role-pernr
              AND endda GE @sy-datum
              AND begda LE @sy-datum
              AND stat2 EQ @zcl_abs_abap_maintain=>c_emp_status. "'3'
           IF sy-subrc NE 0.
             CLEAR : ls_message,
                     ls_usr_role-pernr.
             ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error.
             ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
             ls_message-msgno = '163'.
             add_messages_to_msg_container( is_message = ls_message ).
             RETURN.
           ENDIF.

           SELECT SINGLE pevt
             FROM zabs_emp_role
             INTO @DATA(lv_pevt)
            WHERE pernr EQ @ls_usr_role-pernr
              AND urole EQ @ls_usr_role-urole.
           IF sy-subrc = 0.
             ls_usr_role-pevt = lv_pevt.
             "Begin of José Sequeira - 09.02.2021 14:10:32
           ELSE.
             IF lsr_urole-low EQ 'IN'."In charge ADM
               SELECT SINGLE pevt
                 FROM zabs_emp_role
                 INTO lv_pevt
                WHERE pernr EQ ls_usr_role-pernr
                  AND urole EQ 'IA'."Look for IA (In charge ADM)...
               IF sy-subrc EQ 0.
                 ls_usr_role-pevt = lv_pevt.
               ELSE.
                 CLEAR : ls_message,
                         ls_usr_role-pernr.
                 ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error.
                 ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
                 ls_message-msgno = '163'.
                 add_messages_to_msg_container( is_message = ls_message ).
                 RETURN.
               ENDIF.
             ELSE.
               CLEAR : ls_message,
                       ls_usr_role-pernr.
               ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error.
               ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
               ls_message-msgno = '163'.
               add_messages_to_msg_container( is_message = ls_message ).
               RETURN.
             ENDIF.
             "End of José Sequeira - 09.02.2021 14:10:32
           ENDIF.
         ENDIF. "ltr_pernr

       WHEN 'LD'.

**--Get variant table data
*         CALL METHOD zcl_abs_get_variants=>get_constant_single
*           EXPORTING
*             iv_mod    = space
*             iv_objid  = zcl_abs_abap_maintain=>c_objid_mobile "'MOBC'
*             iv_k1val  = 'LEAD'
*             iv_k2val  = 'PDAYS'
*           IMPORTING
*             ev_cnval1 = lv_cnval1.
*
*         lv_days = lv_cnval1.
*         lv_days = lv_days + 1.
*
*         ls_usr_role-no_days = lv_days.

         READ TABLE ltr_lifnr INTO lsr_lifnr INDEX 1.
         IF sy-subrc = 0.

           CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
             EXPORTING
               input  = lsr_lifnr-low
             IMPORTING
               output = ls_usr_role-lifnr.

*--Validating Vendor
           SELECT SINGLE lifnr
             FROM lfa1
             INTO @DATA(lv_partner)
            WHERE lifnr EQ @ls_usr_role-lifnr
              AND loevm EQ @space.
           IF sy-subrc NE 0.
             CLEAR : ls_message,
                     ls_usr_role-lifnr.
             ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error.
             ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
             ls_message-msgno = '164'.
             add_messages_to_msg_container( is_message = ls_message ).
             RETURN.
           ENDIF.

         ENDIF.

       WHEN 'BN'.
         READ TABLE ltr_farm INTO lsr_farm INDEX 1.
         IF sy-subrc = 0.
           ls_usr_role-farm = lsr_farm-low.

*           CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
*             EXPORTING
*               input      = lsr_farm-low
*             IMPORTING
*               output     = ls_usr_role-farm
*             EXCEPTIONS
*               not_found  = 1
*               not_active = 2
*               OTHERS     = 3.
*           IF sy-subrc NE 0.
*             CLEAR : ls_message.
*             ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error.
*             ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
*             ls_message-msgno = '165'.
*             add_messages_to_msg_container( iv_exception = abap_true
*                                            is_message = ls_message ).
*             RETURN.
*           ENDIF.

*--Validating Terrain
           SELECT SINGLE tplnr_fl
             FROM /agri/glflot
             INTO @DATA(lt_tplnr)
            WHERE tplnr_fl EQ @ls_usr_role-farm
              AND loevm EQ @space
              AND kfrst EQ @space.
           IF sy-subrc NE 0.
*-- Give error message
             CLEAR : ls_message,
                     ls_usr_role-farm.
             ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error.
             ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
             ls_message-msgno = '165'.
             add_messages_to_msg_container( is_message = ls_message ).
             RETURN.
           ENDIF.

         ENDIF.
       WHEN OTHERS.
     ENDCASE.
   ENDIF. "ltr_urole

   "-----------------------------------Wave 3 changes
   CALL METHOD zcl_abs_get_variants=>get_constant_single
     EXPORTING
       iv_mod    = space
       iv_objid  = zcl_abs_abap_maintain=>c_objid_accomplish
       iv_k1val  = zcl_abs_abap_maintain=>c_key_mch_gtol
     IMPORTING
       ev_cnval1 = lv_cnval1.

   ls_usr_role-gtol = lv_cnval1.
   "-----------------------------------Wave 3 changes
   "Begin of José Sequeira - 30.03.2021 10:32:38
   "Mass Events?
   DATA: lv_emass TYPE zabs_del_cnval.
   CALL METHOD zcl_abs_get_variants=>get_constant_single
     EXPORTING
       iv_mod    = space
       iv_objid  = zcl_abs_abap_maintain=>c_objid_accomplish
       iv_k1val  = 'MASS' "Mass Events...
     IMPORTING
       ev_cnval1 = lv_emass.

   IF lv_emass IS NOT INITIAL.
     ls_usr_role-emass = lv_emass.
   ENDIF.
   "End of José Sequeira - 30.03.2021 10:32:38
**********************************************************************
   "Begin of José Sequeira - 21.05.2021 09:08:28
   "GPS Validation (GeoFence) disabled?
**********************************************************************
   "First check global setting...
   DATA: lv_gps TYPE zabs_del_cnval.
   CALL METHOD zcl_abs_get_variants=>get_constant_single
     EXPORTING
       iv_mod    = space
       iv_objid  = zcl_abs_abap_maintain=>c_objid_accomplish
       iv_k1val  = 'GPS' "GPS...
       iv_k2val  = '*'"Em branco, geral...
     IMPORTING
       ev_cnval1 = lv_gps.
   IF lv_gps IS NOT INITIAL.
     ls_usr_role-gps = lv_gps.
   ELSE.
     "Now check local user setting...
     CALL METHOD zcl_abs_get_variants=>get_constant_single
       EXPORTING
         iv_mod    = space
         iv_objid  = zcl_abs_abap_maintain=>c_objid_accomplish
         iv_k1val  = 'GPS' "GPS...
         iv_k2val  = ls_usr_role-pernr
       IMPORTING
         ev_cnval1 = lv_gps.
     IF lv_gps IS NOT INITIAL.
       ls_usr_role-gps = lv_gps.
     ENDIF.
   ENDIF.
**********************************************************************
   "End of José Sequeira - 21.05.2021 09:08:28

   APPEND ls_usr_role TO et_usr_role.
   CLEAR ls_usr_role.

 ENDMETHOD.


METHOD internal_plate_extract.

*--Internal table declaration
  DATA : lt_ilp_btch TYPE TABLE OF zabs_ilp_btch,
         ls_ilp_btch TYPE zabs_ilp_btch,
         ls_ac_batch TYPE zabs_s_ac_batch,
         ls_ac_ilp   TYPE zabs_s_ac_ilp,
         lt_tplnr    TYPE /agri/t_gltplnr,
         ls_tplnr    TYPE /agri/s_gltplnr,
         lt_carimbo  TYPE tt_carimbo.

  IF iv_plate IS NOT INITIAL.
*--Fetching Internal License Plate data
    SELECT *
      FROM zabs_ilp_btch
      INTO TABLE lt_ilp_btch
     WHERE in_licplate EQ iv_plate
       AND weighbridge EQ space
       AND bin         EQ space
       AND loevm       EQ space
       and transbord   EQ space.
    IF sy-subrc EQ 0.
      LOOP AT lt_ilp_btch INTO ls_ilp_btch.
        CLEAR ls_ac_ilp.
        MOVE-CORRESPONDING ls_ilp_btch TO ls_ac_ilp.
        APPEND ls_ac_ilp TO et_ac_ilp.
      ENDLOOP.
    ENDIF.
  ELSEIF iv_farm IS NOT INITIAL.
    SELECT *
      FROM zabs_ilp_btch
      INTO TABLE lt_ilp_btch
     WHERE farm        EQ iv_farm
       AND weighbridge EQ space
       AND bin         EQ space
       AND loevm       EQ space
       AND transbord   EQ space.
    IF sy-subrc EQ 0.
      LOOP AT lt_ilp_btch INTO ls_ilp_btch.
        CLEAR ls_tplnr.
        ls_tplnr-tplnr_fl = ls_ilp_btch-tplnr.
        COLLECT ls_tplnr INTO lt_tplnr.
      ENDLOOP.

*-- Fetch the Carimbos
      CALL METHOD me->fetch_carimbo
        EXPORTING
          it_tplnr   = lt_tplnr
        CHANGING
          ct_carimbo = lt_carimbo.

      LOOP AT lt_ilp_btch INTO ls_ilp_btch.
        CLEAR ls_ac_ilp.
        MOVE-CORRESPONDING ls_ilp_btch TO ls_ac_ilp.
        READ TABLE lt_carimbo INTO DATA(ls_carimbo)
              WITH KEY tplnr = ls_ilp_btch-tplnr
            BINARY SEARCH.
        IF sy-subrc EQ 0.
          ls_ac_ilp-bcarimbo = ls_carimbo-carimbo.
          ls_ac_ilp-btoclhtt = ls_carimbo-toclhtt.
        ENDIF.
        APPEND ls_ac_ilp TO et_ac_ilp.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDMETHOD.


  METHOD intrbatchset_create_entity.

*-- Workarea Declarations
    DATA: ls_messages  TYPE /agri/s_gprolog,
          ls_btch      TYPE zabs_intr_batch,
          ls_intrbatch TYPE zcl_zabs_mobile_acm_mpc=>ts_intrbatch,

*--Table declarations
          lt_messages  TYPE /agri/t_gprolog,
          lt_btch      TYPE TABLE OF zabs_intr_batch.

    DATA(lo_message_container) = mo_context->get_message_container( ).

    io_data_provider->read_entry_data( IMPORTING es_data = ls_intrbatch ).

*--Fetching gang to insert or update data into custom table
    SELECT SINGLE zzturma
      FROM zabs_intr_batch
      INTO @DATA(lv_turma)
     WHERE zzturma EQ @ls_intrbatch-zzturma.
    IF sy-subrc EQ 0.
      ls_btch-zzturma = ls_intrbatch-zzturma.
      ls_btch-posnr   = ls_intrbatch-posnr.
      UPDATE zabs_intr_batch FROM ls_btch.
      CLEAR ls_messages.
      ls_messages-msgty = zcl_abs_abap_maintain=>c_msgty_success. "'S'
      ls_messages-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
      ls_messages-msgno = '109'.
      ls_messages-msgv1 = ls_btch-zzturma.
      ls_messages-msgv2 = ls_btch-posnr.
    ELSE.
      ls_btch-zzturma = ls_intrbatch-zzturma.
      ls_btch-posnr   = ls_intrbatch-posnr.
      INSERT zabs_intr_batch FROM ls_btch." ls_btch.
      CLEAR ls_messages.
      ls_messages-msgty = zcl_abs_abap_maintain=>c_msgty_success. "'S'
      ls_messages-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
      ls_messages-msgno = '110'.
      ls_messages-msgv1 = ls_btch-zzturma.
    ENDIF.

    er_entity-zzturma = ls_btch-zzturma.
    er_entity-posnr   = ls_btch-posnr.

*-- Add the messages to containersa
    add_messages_to_msg_container( iv_exception = abap_true
                                   it_messages = lt_messages ).

  ENDMETHOD.


  METHOD intrbatchset_get_entityset.

**--Fetching Internal batch data
*    SELECT *
*      FROM zabs_intr_batch
*      INTO TABLE @DATA(lt_intr_batch).
*
*    MOVE-CORRESPONDING lt_intr_batch TO et_entityset.

    CALL METHOD me->get_intrbatch
      IMPORTING
        et_intr_batch = et_entityset.

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


METHOD intrplateextract_get_entityset.

*--Internal table declaration
  DATA : lt_ac_ilp     TYPE TABLE OF zabs_s_ac_ilp,
         lt_filter     TYPE /iwbep/t_mgw_select_option,
         ls_ac_ilp     TYPE zabs_s_ac_ilp,
         ls_entityset  TYPE zcl_zabs_mobile_acm_mpc=>ts_intrplateextract,
         ls_filter     TYPE /iwbep/s_mgw_select_option,
         lv_filter_str TYPE string,
         lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
         lrt_plate     TYPE RANGE OF zabs_del_licplate,
         lrs_plate     LIKE LINE OF lrt_plate,
         ltr_farm      TYPE RANGE OF /agri/gltplma,
         lsr_farm      LIKE LINE OF ltr_farm.

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
      WHEN 'IN_LICPLATE'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_plate ).
      WHEN 'FARM'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = ltr_farm ).
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

  IF lrt_plate IS NOT INITIAL.
    READ TABLE lrt_plate INTO lrs_plate INDEX 1.
    IF sy-subrc EQ 0.
      CALL METHOD me->internal_plate_extract
        EXPORTING
          iv_plate  = lrs_plate-low
        IMPORTING
          et_ac_ilp = lt_ac_ilp.
    ENDIF.
  ELSEIF ltr_farm IS NOT INITIAL.
    READ TABLE ltr_farm INTO lsr_farm INDEX 1.
    IF sy-subrc EQ 0.
      CALL METHOD me->internal_plate_extract
        EXPORTING
          iv_farm   = lsr_farm-low
        IMPORTING
          et_ac_ilp = lt_ac_ilp.
    ENDIF.
  ENDIF.

  LOOP AT lt_ac_ilp INTO ls_ac_ilp.
    MOVE-CORRESPONDING ls_ac_ilp TO ls_entityset.
    APPEND ls_entityset TO et_entityset.
    CLEAR ls_entityset.
  ENDLOOP.

ENDMETHOD.


METHOD leadfarmsset_get_entityset.

*-- Fetch the farms under the leaders
  CALL METHOD me->get_leader_farms
    EXPORTING
      io_tech_request_context = io_tech_request_context
    IMPORTING
      et_leader_farms         = et_entityset.

*  CALL METHOD me->get_leader_farms
*    IMPORTING
*      et_leader_farms = et_entityset.

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


METHOD loadbaggroupset_get_entityset.

**--Workarea declaration
*    DATA : ls_usr_emp   TYPE zabs_usr_emp,
*           ls_bggrp     TYPE zabs_ac_bggrp,
*           ls_ldbaggrp  TYPE zabs_s_ac_ldbaggrp,
*           ls_entityset TYPE zcl_zabs_mobile_acm_mpc=>ts_loadbaggroup,
*
**--Internal table declaration
*           lt_bggrp     TYPE TABLE OF zabs_ac_bggrp,
*           lt_ldbaggrp  TYPE TABLE OF zabs_s_ac_ldbaggrp,
*
**--Variable declaration
*           lv_banca     TYPE zabs_del_banca,
*           lv_days      TYPE i.
*
**--Calling method to get user details
*    CALL METHOD me->get_employee_role
*      EXPORTING
*        iv_user    = sy-uname
*      IMPORTING
*        es_usr_emp = ls_usr_emp.
*
*    IF ls_usr_emp-urole = 'LD'.
*
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*        EXPORTING
*          input  = ls_usr_emp-lifnr
*        IMPORTING
*          output = ls_usr_emp-lifnr.
*
*      CALL METHOD zcl_abs_get_variants=>get_constant_single
*        EXPORTING
*          iv_objid  = 'MOBC'
*          iv_k1val  = 'DAYS'
*          iv_k2val  = 'LDAY'
*        IMPORTING
*          ev_cnval1 = lv_days. "'3'
*
*      DATA(lv_bdate) = sy-datum - lv_days.
*
**--Accomplishment Bag Groups
*      SELECT *
*        FROM zabs_ac_bggrp
*        INTO TABLE lt_bggrp
*       WHERE ldcde  EQ ls_usr_emp-lifnr
*         AND loaded EQ space
*         AND ersda  GE lv_bdate
*         AND ersda  LE sy-datum.
*      IF sy-subrc <> 0.
*        RETURN.
*      ENDIF.
*    ENDIF.
*
*    LOOP AT lt_bggrp INTO ls_bggrp.
*      MOVE-CORRESPONDING ls_bggrp TO ls_ldbaggrp.
*      ls_ldbaggrp-zzmackey = ls_bggrp-mackey.
*      ls_ldbaggrp-zzbaggp  = ls_bggrp-baggp.
**      ls_ldbaggrp-zzbagcn  = ls_bggrp-zzbagcn.
**      ls_ldbaggrp-tot_boxes  = ls_bggrp-tot_boxes.
**      ls_ldbaggrp-avg_cxn  = ls_bggrp-avg_cxn.
**      ls_ldbaggrp-ibag_no  = ls_bggrp-ibag_no.
**      ls_ldbaggrp-fbag_no  = ls_bggrp-fbag_no.
**      ls_ldbaggrp-ldcde  = ls_bggrp-ldcde.
**      ls_ldbaggrp-loaded = ls_bggrp-loaded.
**      ls_ldbaggrp-zzturma = ls_bggrp-zzturma.
**      ls_ldbaggrp-tplnr   = ls_bggrp-tplnr.
**      ls_ldbaggrp-tmatnr = ls_bggrp-tmatnr.
**      ls_ldbaggrp-aufnr   = ls_bggrp-aufnr.
**      ls_ldbaggrp-werks   = ls_bggrp-werks.
**      ls_ldbaggrp-lider_turma = ls_bggrp-lider_turma.
*      CONCATENATE ls_bggrp-ibag_no ls_bggrp-fbag_no INTO lv_banca
*      SEPARATED BY '-'.
*      ls_ldbaggrp-banca = lv_banca.
*
*      APPEND ls_ldbaggrp TO lt_ldbaggrp.
*      CLEAR ls_ldbaggrp.
*    ENDLOOP.
*
*    LOOP AT lt_ldbaggrp INTO ls_ldbaggrp.
*      MOVE-CORRESPONDING ls_ldbaggrp TO ls_entityset.
*      APPEND ls_entityset TO et_entityset.
*      CLEAR ls_entityset.
*    ENDLOOP.
*

  CALL METHOD me->get_loadbaggroup
    EXPORTING
      io_tech_request_context = io_tech_request_context
    IMPORTING
      et_loadbaggroup         = et_entityset.

*  CALL METHOD me->get_loadbaggroup
*    IMPORTING
*      et_loadbaggroup = et_entityset.

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


  METHOD loader_f4set_get_entityset.

*local decleration
*    DATA: lt_leader_farms TYPE STANDARD TABLE OF zabs_s_ac_lead_farms.

*-- Fetch the farms under the leaders
    CALL METHOD me->get_leader_farms
      EXPORTING
        io_tech_request_context = io_tech_request_context
      IMPORTING
        et_leader_farms         = DATA(lt_leader_farms).

*-- Fetch the Loader under the Farms
    CALL METHOD me->get_farms_loader
      EXPORTING
        it_leader_farms = lt_leader_farms
      IMPORTING
        et_farms_loader = et_entityset.

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


  method LOADFARMS_F4SET_GET_ENTITYSET.

*-- Fetch the Farms under the Loader
    CALL METHOD me->get_loader_Farms
      EXPORTING
        io_tech_request_context = io_tech_request_context
      IMPORTING
        et_loader_farms = et_entityset.

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
  endmethod.


METHOD populate_accomplishment_data.

*-- Local Declarations
  DATA: lv_actyp TYPE /agri/fmactyp,
        lv_task  TYPE /agri/gltmatnr,
        lv_posnr TYPE /agri/glposnr.

  DATA(ls_achdr) = is_achdr.
  DATA(lt_acitm) = it_acitm.

*-- Read Accomplishment type and task
  CALL METHOD zcl_abs_get_variants=>get_constant_single
    EXPORTING
      iv_objid  = zcl_abs_abap_maintain=>c_objid_mobile
      iv_k1val  = zcl_abs_abap_maintain=>c_key_accomplish_type
      iv_k2val  = ls_achdr-zzactcg
    IMPORTING
      ev_cnval1 = lv_actyp.

*-- Populate Accomplishment header data
  ls_achdr-accom  = TEXT-001.
  ls_achdr-actyp  = lv_actyp.
  ls_achdr-gjahr  = ls_achdr-strtdat(4).
  IF ls_achdr-zzactcg = zcl_abs_abap_maintain=>c_actcg_prod.
    ls_achdr-status = zcl_abs_abap_maintain=>c_ac_status_created.
  ELSE.
    ls_achdr-status = zcl_abs_abap_maintain=>c_ac_status_confirm.
  ENDIF.
  ls_achdr-descr  = ls_achdr-zzmackey.
  ls_achdr-updkz  = zcl_abs_abap_maintain=>c_insert.

  READ TABLE lt_acitm INTO DATA(ls_acitm) INDEX 1.
  IF sy-subrc EQ 0.
    ls_achdr-zztplnr  = ls_acitm-tplnr.
  ENDIF.

*-- Fetch the company code for the plant
  SELECT SINGLE bukrs
    FROM t001k
    INTO ls_achdr-bukrs
   WHERE bwkey EQ ls_achdr-werks.

  LOOP AT lt_acitm ASSIGNING FIELD-SYMBOL(<fs_acitm>).
*-- Populate the Accomplishment items
    <fs_acitm>-accom = TEXT-001.
    <fs_acitm>-zztplnr = <fs_acitm>-tplnr.
    IF ls_achdr-zzactcg = zcl_abs_abap_maintain=>c_actcg_prod.
      <fs_acitm>-status = zcl_abs_abap_maintain=>c_ac_status_created.
    ELSE.
      <fs_acitm>-status = zcl_abs_abap_maintain=>c_ac_status_confirm.
    ENDIF.
    <fs_acitm>-updkz = zcl_abs_abap_maintain=>c_insert.
  ENDLOOP.

  es_acdoc-accom = TEXT-001.
  es_acdoc-x-achdr = ls_achdr.
  es_acdoc-x-acitm = lt_acitm.

ENDMETHOD.


METHOD terraincarimbose_get_entityset.

*-- Local Declarations
  DATA: lt_filter     TYPE /iwbep/t_mgw_select_option,
        ls_filter     TYPE /iwbep/s_mgw_select_option,
        lv_filter_str TYPE string,
        lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
        ltr_tplnr     TYPE RANGE OF /agri/gltplnr_fl,
        lsr_tplnr     LIKE LINE OF ltr_tplnr,
        lt_tplnr      TYPE /agri/t_gltplnr,
        ls_tplnr      TYPE /agri/s_gltplnr,
        lt_carimbo    TYPE tt_carimbo,
        ls_entityset  TYPE zcl_zabs_mobile_acm_mpc=>ts_terraincarimbo.

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
      WHEN 'TPLNR'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = ltr_tplnr ).
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

  CLEAR lsr_tplnr.
  READ TABLE ltr_tplnr INTO lsr_tplnr INDEX 1.
  IF sy-subrc EQ 0.
    CLEAR ls_tplnr.
    ls_tplnr-tplnr_fl = lsr_tplnr-low.
    APPEND ls_tplnr TO lt_tplnr.

    CALL METHOD me->fetch_carimbo
      EXPORTING
        it_tplnr   = lt_tplnr
      CHANGING
        ct_carimbo = lt_carimbo.

    LOOP AT lt_carimbo INTO DATA(ls_carimbo).
      CLEAR ls_entityset.
      ls_entityset-tplnr    = ls_carimbo-tplnr.
      ls_entityset-bcarimbo = ls_carimbo-carimbo.
      ls_entityset-btoclhtt = ls_carimbo-toclhtt.
      APPEND ls_entityset TO et_entityset.
    ENDLOOP.
  ENDIF.

ENDMETHOD.


METHOD trantypset_get_entityset.

**-- Fetch the transport movement types
*  SELECT *
*    FROM zabst_tran_typ
*    INTO TABLE @DATA(lt_tran_typ).
*  IF sy-subrc EQ 0.
*    DATA(lt_farm) = lt_tran_typ.
*    SORT lt_farm BY farm.
*    DELETE ADJACENT DUPLICATES FROM lt_farm COMPARING farm.
*
**-- Fetch the farm description
*    SELECT tplnr_fl, pltxt
*      FROM /agri/glflot
*      INTO TABLE @DATA(lt_glflot)
*       FOR ALL ENTRIES IN @lt_farm
*     WHERE tplnr_fl EQ @lt_farm-farm.
*    IF sy-subrc EQ 0.
*      SORT lt_glflot BY tplnr_fl.
*    ENDIF.
*
*    DATA(lt_vendor) = lt_tran_typ.
*    SORT lt_vendor BY int_tr_prov.
*    DELETE ADJACENT DUPLICATES FROM lt_vendor COMPARING int_tr_prov.
*
**-- Fetch the Vendor name
*    SELECT lifnr, name1
*      FROM lfa1
*      INTO TABLE @DATA(lt_lfa1)
*       FOR ALL ENTRIES IN @lt_vendor
*     WHERE lifnr EQ @lt_vendor-int_tr_prov.
*    IF sy-subrc EQ 0.
*      SORT lt_lfa1 BY lifnr.
*    ENDIF.
*
*    MOVE-CORRESPONDING lt_tran_typ TO et_entityset.
*  ENDIF.
*
*  LOOP AT et_entityset ASSIGNING FIELD-SYMBOL(<fs_entityset>).
*    READ TABLE lt_glflot INTO DATA(ls_glflot)
*          WITH KEY tplnr_fl = <fs_entityset>-farm
*        BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      <fs_entityset>-pltxt = ls_glflot-pltxt.
*    ENDIF.
*
*    READ TABLE lt_lfa1 INTO DATA(ls_lfa1)
*          WITH KEY lifnr = <fs_entityset>-int_tr_prov
*        BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      <fs_entityset>-name1 = ls_lfa1-name1.
*    ENDIF.
*  ENDLOOP.

  CALL METHOD me->get_trantyp
    IMPORTING
      et_trantyp = et_entityset.

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


METHOD turmaheadset_get_entityset.

*-- Fetch the turmas of login user (Leader/Incharge)

  CALL METHOD me->get_turma_header
    EXPORTING
      io_tech_request_context = io_tech_request_context
    IMPORTING
      et_turma_head           = et_entityset.

*  CALL METHOD me->get_turma_header
*    IMPORTING
*      et_turma_head = et_entityset.

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


METHOD turmaitmset_get_entityset.

*-- Fetch the employees (harvesters) under the turmas
  CALL METHOD me->get_turma_items
    EXPORTING
      io_tech_request_context = io_tech_request_context
    IMPORTING
      et_turma_items          = et_entityset.

*  CALL METHOD me->get_turma_items
*    IMPORTING
*      et_turma_items = et_entityset.

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


METHOD userloginset_get_entityset.

*  DATA : ls_usr_emp   TYPE zabs_usr_emp,
*         ls_entityset LIKE LINE OF et_entityset.
*
**-- Get the employee number and role
*  CALL METHOD me->get_employee_role
*    EXPORTING
*      iv_user    = sy-uname
*    IMPORTING
*      es_usr_emp = ls_usr_emp.
*
*  ls_entityset-bname  = ls_usr_emp-bname.
*  ls_entityset-urole  = ls_usr_emp-urole.
*  ls_entityset-pevt   = ls_usr_emp-pevt.
*  ls_entityset-farm   = ls_usr_emp-farm.
*  APPEND ls_entityset TO et_entityset.
*  CLEAR ls_entityset.
*
**** Inlinecount
*  IF io_tech_request_context->has_inlinecount( ) = abap_true.
*    DESCRIBE TABLE et_entityset LINES es_response_context-inlinecount.
*  ELSE.
*    CLEAR es_response_context-inlinecount.
*  ENDIF.
*
**--------------------------------------------------------------------*
**----------- Delta Token Functionality    ---------------------------*
**--------------------------------------------------------------------*
*  CALL METHOD me->get_delta_token
*    EXPORTING
*      iv_entity_set_name      = iv_entity_set_name
*      io_tech_request_context = io_tech_request_context
*    IMPORTING
*      es_response_context     = es_response_context
*    CHANGING
*      ct_entityset            = et_entityset.

ENDMETHOD.


METHOD userroleset_get_entityset.

  CALL METHOD me->get_usr_role
    EXPORTING
      io_tech_request_context = io_tech_request_context
    IMPORTING
      et_usr_role             = et_entityset.

**** Inlinecount
*  IF io_tech_request_context->has_inlinecount( ) = abap_true.
*    DESCRIBE TABLE et_entityset LINES es_response_context-inlinecount.
*  ELSE.
*    CLEAR es_response_context-inlinecount.
*  ENDIF.
*
**--------------------------------------------------------------------*
**----------- Delta Token Functionality    ---------------------------*
**--------------------------------------------------------------------*
*  CALL METHOD me->get_delta_token
*    EXPORTING
*      iv_entity_set_name      = iv_entity_set_name
*      io_tech_request_context = io_tech_request_context
*    IMPORTING
*      es_response_context     = es_response_context
*    CHANGING
*      ct_entityset            = et_entityset.

ENDMETHOD.
ENDCLASS.
