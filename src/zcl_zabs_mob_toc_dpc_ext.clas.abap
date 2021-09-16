class ZCL_ZABS_MOB_TOC_DPC_EXT definition
  public
  inheriting from ZCL_ZABS_MOB_TOC_DPC
  create public .

public section.

  methods REQUEST_HEADER .
  methods GET_SKIP_TOKEN
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
    exporting
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
      !EV_TOTRECORDS type I
    changing
      !CT_ENTITYSET type STANDARD TABLE .
  methods DELETE_FROM_DATABASE
    importing
      !TABNAME type CSEQUENCE default 'INDX'
      !CLIENT type MANDT optional
      !AREA type RELID default 'ID'
      !ID type CLIKE optional
      !GENERIC_KEY type ABAP_BOOL default ABAP_FALSE
      !CLIENT_SPECIFIED type ABAP_BOOL default ABAP_TRUE .
  methods GET_ROUTE_TERRAIN_DTLS
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_ENTITYSET type ZCL_ZABS_MOB_TOC_MPC=>TT_WOROUTETERRAIN
      !ET_RTTRN type ZCL_ZABS_MOB_TOC_MPC=>TT_WOROUTETERRAIN
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods GET_DELTA_TOKEN
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
      !MR_SERVICE_DOCUMENT_NAME type ref to STRING
      !MR_SERVICE_VERSION type ref to NUMC4
      !IT_ENTITYSET type STANDARD TABLE
    exporting
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
    raising
      /IWBEP/CX_QRL_BASE
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods GET_EMPLOYEE_ROLES
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_ENTITYSET type ZCL_ZABS_MOB_TOC_MPC=>TT_EMPLOYEE_ROLE
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods EXCEPTION_MESSAGES
    importing
      !IV_ENTITY_NAME type STRING optional
      !IV_OPERATION type STRING optional
      !IV_MESSAGE_TEXT type BAPI_MSG optional
      !IS_MESSAGE type /AGRI/S_GPROLOG optional
      !IT_MESSAGES type /AGRI/T_GPROLOG optional
      !IT_BAPI_MESSSAGES type BAPIRET2_T optional
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_BEGIN
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_END
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_PROCESS
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITYSET
    redefinition .
protected section.

  methods GET_TERRAINSET
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_ENTITYSET type ZCL_ZABS_AGRI_MOBILE_E_MPC=>TT_TERRAIN_EXT
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods GET_WOMATERIAL
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_ENTITYSET type ZCL_ZABS_MOB_TOC_MPC=>TT_WOMATERIAL
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods GET_WOTASKORDERS
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_ENTITYSET type ZCL_ZABS_MOB_TOC_MPC=>TT_WOTASKORDERS
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods GET_WOOPERATIONS
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_ENTITYSET type ZCL_ZABS_MOB_TOC_MPC=>TT_WOOPERATIONS
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods GET_WOCOMPONENT
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_ENTITYSET type ZCL_ZABS_MOB_TOC_MPC=>TT_WOCOMPONENTS
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods GET_REASONLIST
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_ENTITYSET type ZCL_ZABS_MOB_TOC_MPC=>TT_REASONLIST
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods TASK_ORDER_CONFIRM
    importing
      !IT_FPITM type /AGRI/T_FMFPITM optional
      !IT_FPCOM type ZCL_ZABS_MOB_TOC_MPC=>TT_WOCOMPONENTS
      !IT_WOACTIVITIES type ZCL_ZABS_MOB_TOC_MPC=>TT_WOACTIVITIES
      !IT_FPFP_CNF type /AGRI/T_FMFP_CNF
      !IV_TANK_CALC type ZCL_ZABS_MOB_TOC_MPC=>TS_WOOPERATIONS-NRTANKS optional
      !IS_OPERATION type ZCL_ZABS_MOB_TOC_MPC=>TS_WOOPERATIONS optional
    exporting
      !ET_MESSAGES type /AGRI/T_GPROLOG
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .

  methods EMPLOYEE_ROLESET_GET_ENTITYSET
    redefinition .
  methods REASONLISTSET_GET_ENTITYSET
    redefinition .
  methods TERRAINSET_GET_ENTITYSET
    redefinition .
  methods WOCOMPONENTSSET_GET_ENTITYSET
    redefinition .
  methods WOMATERIALSET_GET_ENTITYSET
    redefinition .
  methods WOOPERATIONSSET_GET_ENTITYSET
    redefinition .
  methods WOROUTESET_GET_ENTITYSET
    redefinition .
  methods WOROUTETERRAINSE_GET_ENTITYSET
    redefinition .
  methods WOTASKORDERSSET_GET_ENTITYSET
    redefinition .
private section.

  constants C_UPDKZ_DELETE type UPDKZ_D value 'D' ##NO_TEXT.
  constants C_SOURCE_QUALITY type CHAR2 value 'QL' ##NO_TEXT.
  constants C_SOURCE_TASKORDER type CHAR2 value 'TC' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_ZABS_MOB_TOC_DPC_EXT IMPLEMENTATION.


METHOD /iwbep/if_mgw_appl_srv_runtime~changeset_begin.

*-- Data Declarations
  DATA: ls_operation_info  TYPE /iwbep/s_mgw_operation_info.

  CLEAR: cv_defer_mode.
  cv_defer_mode = abap_true.

*-- Looping Entityset name in the Operation Table
  LOOP AT it_operation_info INTO ls_operation_info.
    IF NOT ( "ls_operation_info-entity_set  = 'WoActivities_extSet'   OR
             ls_operation_info-entity_set  = 'WoComponentsSet'   OR
             ls_operation_info-entity_set  = 'WoOperationsSet'   OR
             ls_operation_info-entity_set  = 'TerrainSet'
      ).

      cv_defer_mode = abap_false.
      EXIT.
    ENDIF.
  ENDLOOP.

ENDMETHOD.


METHOD /iwbep/if_mgw_appl_srv_runtime~changeset_end.
  COMMIT WORK.
ENDMETHOD.


METHOD /iwbep/if_mgw_appl_srv_runtime~changeset_process.

*-- Types
*  TYPES: BEGIN OF ls_qapp,
*           prueflos TYPE qapp-prueflos,
*           probenr  TYPE qapp-probenr,
*         END OF ls_qapp.

  CONSTANTS: lc_objtyp TYPE oj_name       VALUE '/AGRI/GLMD',
             lc_astat  TYPE /agri/glastat VALUE 'A'.

  DATA:
*-- Class Objects
    lref_msg_container     TYPE REF TO /iwbep/if_message_container,
    lo_update_context      TYPE REF TO /iwbep/if_mgw_req_entity_u,
*-- Tables
    lt_return              TYPE bapiret2_t,
    lt_fmfpcnf             TYPE /agri/t_fmfp_cnf,
    lt_fpcom               TYPE /agri/t_fmfpcom,
*    lt_woactivities        TYPE /agri/cl_mobile_mpc_ext=>tt_woactivities_ext,
    lt_woactivities        TYPE zcl_zabs_mob_toc_mpc=>tt_woactivities,
    lt_wocomp              TYPE zcl_zabs_mob_toc_mpc=>tt_wocomponents,
    lt_line                TYPE /irm/t_gline,
    lt_mdatv               TYPE /agri/t_glmdatv,
    lt_atinn               TYPE /agri/t_gatinn,
    lt_mddoc               TYPE /agri/t_glmd_doc,
    lt_messages            TYPE /agri/t_gprolog,
    lv_ldate               TYPE sy-datum,
    lv_hdate               TYPE sy-datum,
*-- Structures
    ls_fmfpcnf             TYPE /agri/s_fmfp_cnf,
    ls_fpcom               TYPE /agri/s_fmfpcom, "JOBREGON
    ls_wooperations        TYPE zcl_zabs_mob_toc_mpc=>ts_wooperations,
    ls_wocomponents        TYPE /agri/cl_mobile_mpc_ext=>ts_wocomponents,
    ls_wocomp              TYPE zcl_zabs_mob_toc_mpc=>ts_wocomponents,
*    ls_woactivities        TYPE /agri/cl_mobile_mpc_ext=>ts_woactivities,
*    ls_woactivities        TYPE zcl_zabs_agri_mobile_e_mpc=>ts_woactivities_ext,
    ls_changeset_request   TYPE /iwbep/if_mgw_appl_types=>ty_s_changeset_request,
    ls_messages            TYPE /agri/s_gprolog,
*    ls_mditm               TYPE /agri/s_glmditm,
    ls_mddoc               TYPE /agri/s_glmd_doc,
    ls_line                LIKE LINE OF lt_line,
    lwa_attribute          TYPE auspdata,
*-- Variables
    lv_tmpdate             TYPE string,
    lwa_changeset_response TYPE /iwbep/if_mgw_appl_types=>ty_s_changeset_response,
    lv_entity_type         TYPE string,
    lv_atinn               TYPE cabn-atinn,
    lv_sample              TYPE qibpprobe,
    lv_aslvl               TYPE /agri/glaslvl,
    lv_msgno               TYPE symsgno,
    lv_x                   TYPE xfeld VALUE abap_true,
    lv_objkey              TYPE /irm/gobjkey,
    lv_workorder,
    lv_upload.

*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
*-- Processing of batch posting
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------

  LOOP AT it_changeset_request INTO ls_changeset_request.

    CLEAR lv_entity_type.
    lo_update_context ?= ls_changeset_request-request_context.
    lv_entity_type = lo_update_context->get_entity_type_name( ).
    lwa_changeset_response-operation_no = ls_changeset_request-operation_no.

    IF lv_entity_type EQ 'WoOperations'.                    "#EC NOTEXT
      ls_changeset_request-entry_provider->read_entry_data(
      IMPORTING es_data = ls_wooperations ).

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ls_wooperations-aufnr
        IMPORTING
          output = ls_wooperations-aufnr.

      MOVE-CORRESPONDING ls_wooperations TO ls_fmfpcnf. "#EC CI_FLDEXT_OK
      ls_fmfpcnf-budat     = ls_wooperations-budatn.
      ls_fmfpcnf-isdd      = ls_wooperations-ru_isdd.
      ls_fmfpcnf-iedd      = ls_wooperations-ru_iedd.
      ls_fmfpcnf-isdz      = ls_wooperations-ru_isdz.
      ls_fmfpcnf-iedz      = ls_wooperations-ru_iedz.
      ls_fmfpcnf-zznrtank  = ls_wooperations-nrtanks.

      SELECT SINGLE umren                            "#EC CI_SEL_NESTED
        FROM /agri/fmfpitm
        INTO @ls_fmfpcnf-umren
       WHERE aufnr = @ls_wooperations-aufnr
         AND posnr = @ls_wooperations-posnr.

      APPEND ls_fmfpcnf TO lt_fmfpcnf.
      lv_workorder = abap_true.
      copy_data_to_ref( EXPORTING is_data = ls_wooperations
                   CHANGING cr_data = lwa_changeset_response-entity_data ).
      INSERT lwa_changeset_response INTO TABLE ct_changeset_response.
    ENDIF.

    IF lv_entity_type EQ 'WoComponents'.                    "#EC NOTEXT
      ls_changeset_request-entry_provider->read_entry_data(
          IMPORTING
            es_data = ls_wocomp ).

      APPEND ls_wocomp TO lt_wocomp.

      copy_data_to_ref( EXPORTING is_data = ls_wocomp
                   CHANGING cr_data = lwa_changeset_response-entity_data ).

      INSERT lwa_changeset_response INTO TABLE ct_changeset_response.
    ENDIF.

  ENDLOOP.

*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
*-- Calling of respective methods for data processing
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------

*--------------------------------------------------------------------*
*       Task Order Confirmation
*--------------------------------------------------------------------*

  IF lv_workorder IS NOT INITIAL.

    DELETE lt_fpcom WHERE updkz = c_updkz_delete.
*-- Task Order confirmation
*      CALL METHOD me->work_order_confirm_ext
*        EXPORTING
*          it_fpfp_cnf     = lt_fmfpcnf
*          it_fpcom        = lt_wocomp "lt_fpcom
*          it_woactivities = lt_woactivities
*          is_operation    = ls_wooperations
*          iv_tank_calc    = ls_wooperations-nrtanks
*        IMPORTING
*          et_messages     = lt_messages.

    CALL METHOD me->task_order_confirm
      EXPORTING
*       it_fpitm        =
        it_fpcom        = lt_wocomp "lt_fpcom
        it_woactivities = lt_woactivities
        it_fpfp_cnf     = lt_fmfpcnf
*       iv_tank_calc    =
        is_operation    = ls_wooperations
      IMPORTING
        et_messages     = lt_messages.

*-- Preparing the messages in the container
    IF lt_messages IS NOT INITIAL.
      exception_messages( EXPORTING it_messages = lt_messages ).
    ENDIF.

  ELSE.
    CALL METHOD super->/iwbep/if_mgw_appl_srv_runtime~changeset_process
      EXPORTING
        it_changeset_request  = it_changeset_request
      CHANGING
        ct_changeset_response = ct_changeset_response.

  ENDIF.

ENDMETHOD.


METHOD /iwbep/if_mgw_appl_srv_runtime~get_entityset.

  DATA worouteterrainse    TYPE zcl_zabs_mob_toc_mpc=>tt_worouteterrain.
  DATA employee_roleset    TYPE zcl_zabs_mob_toc_mpc=>tt_employee_role.
*    DATA wo_act_ext_get_entityset       TYPE zcl_zabs_agri_mobile_e_mpc=>tt_woactivities.
  DATA womaterialset       TYPE zcl_zabs_mob_toc_mpc=>tt_womaterial.   "JOBREGON
  DATA wooperationsset     TYPE zcl_zabs_mob_toc_mpc=>tt_wooperations. "JOBREGON
  DATA wotaskordersset     TYPE zcl_zabs_mob_toc_mpc=>tt_wotaskorders. "JOBREGON
  DATA terrainset          TYPE zcl_zabs_mob_toc_mpc=>tt_terrain.        "JOBREGON
  DATA wocomponentsset     TYPE zcl_zabs_mob_toc_mpc=>tt_wocomponents.   "JOBREGON
  DATA reasonlistset       TYPE zcl_zabs_mob_toc_mpc=>tt_reasonlist.     "JOBREGON
  DATA worouteset          TYPE zcl_zabs_mob_toc_mpc=>tt_woroute.

  TRY.
      CALL METHOD super->/iwbep/if_mgw_appl_srv_runtime~get_entityset
        EXPORTING
          iv_entity_name           = iv_entity_name
          iv_entity_set_name       = iv_entity_set_name
          iv_source_name           = iv_source_name
          it_filter_select_options = it_filter_select_options
          it_order                 = it_order
          is_paging                = is_paging
          it_navigation_path       = it_navigation_path
          it_key_tab               = it_key_tab
          iv_filter_string         = iv_filter_string
          iv_search_string         = iv_search_string
          io_tech_request_context  = io_tech_request_context
        IMPORTING
          er_entityset             = er_entityset
          es_response_context      = es_response_context.
    CATCH /iwbep/cx_mgw_busi_exception .
    CATCH /iwbep/cx_mgw_tech_exception .
  ENDTRY.

  DATA(lv_entityset_name) = io_tech_request_context->get_entity_set_name( ).

  CASE lv_entityset_name.
*-------------------------------------------------------------------------*
*             EntitySet -  InspectionPointSet
*-------------------------------------------------------------------------*
    WHEN 'WoRouteTerrainSet'.
*     Call the entity set generated method
      worouteterrainse_get_entityset(
        EXPORTING
         iv_entity_name = iv_entity_name
         iv_entity_set_name = iv_entity_set_name
         iv_source_name = iv_source_name
         it_filter_select_options = it_filter_select_options
         it_order = it_order
         is_paging = is_paging
         it_navigation_path = it_navigation_path
         it_key_tab = it_key_tab
         iv_filter_string = iv_filter_string
         iv_search_string = iv_search_string
         io_tech_request_context = io_tech_request_context
       IMPORTING
         et_entityset = worouteterrainse
         es_response_context = es_response_context
       ).
*     Send specific entity data to the caller interface
      copy_data_to_ref(
        EXPORTING
          is_data = worouteterrainse
        CHANGING
          cr_data = er_entityset
      ).

    WHEN 'Employee_RoleSet'.
      CALL METHOD me->employee_roleset_get_entityset
        EXPORTING
          iv_entity_name           = iv_entity_name
          iv_entity_set_name       = iv_entity_set_name
          iv_source_name           = iv_source_name
          it_filter_select_options = it_filter_select_options
          is_paging                = is_paging
          it_key_tab               = it_key_tab
          it_navigation_path       = it_navigation_path
          it_order                 = it_order
          iv_filter_string         = iv_filter_string
          iv_search_string         = iv_search_string
          io_tech_request_context  = io_tech_request_context
        IMPORTING
          et_entityset             = employee_roleset
          es_response_context      = es_response_context.
      copy_data_to_ref(
      EXPORTING
        is_data = employee_roleset
      CHANGING
        cr_data = er_entityset
    ).

*    WHEN 'WoActivities_extSet'.
*
*      CALL METHOD me->woact_ext_set_get_entityset
*        EXPORTING
*          iv_entity_name           = iv_entity_name
*          iv_entity_set_name       = iv_entity_set_name
*          iv_source_name           = iv_source_name
*          it_filter_select_options = it_filter_select_options
*          is_paging                = is_paging
*          it_key_tab               = it_key_tab
*          it_navigation_path       = it_navigation_path
*          it_order                 = it_order
*          iv_filter_string         = iv_filter_string
*          iv_search_string         = iv_search_string
*          io_tech_request_context  = io_tech_request_context
*        IMPORTING
*          et_entityset             = wo_act_ext_get_entityset
*          es_response_context      = es_response_context.
*      copy_data_to_ref(
*      EXPORTING
*        is_data = wo_act_ext_get_entityset
*      CHANGING
*        cr_data = er_entityset
*    ).

    WHEN 'WoMaterialSet'.
      CALL METHOD me->womaterialset_get_entityset
        EXPORTING
          iv_entity_name           = iv_entity_name
          iv_entity_set_name       = iv_entity_set_name
          iv_source_name           = iv_source_name
          it_filter_select_options = it_filter_select_options
          is_paging                = is_paging
          it_key_tab               = it_key_tab
          it_navigation_path       = it_navigation_path
          it_order                 = it_order
          iv_filter_string         = iv_filter_string
          iv_search_string         = iv_search_string
          io_tech_request_context  = io_tech_request_context
        IMPORTING
          et_entityset             = womaterialset
          es_response_context      = es_response_context.

      copy_data_to_ref(
       EXPORTING
         is_data = womaterialset
       CHANGING
         cr_data = er_entityset
     ).

    WHEN 'WoOperationsSet'.

      CALL METHOD me->wooperationsset_get_entityset
        EXPORTING
          iv_entity_name           = iv_entity_name
          iv_entity_set_name       = iv_entity_set_name
          iv_source_name           = iv_source_name
          it_filter_select_options = it_filter_select_options
          is_paging                = is_paging
          it_key_tab               = it_key_tab
          it_navigation_path       = it_navigation_path
          it_order                 = it_order
          iv_filter_string         = iv_filter_string
          iv_search_string         = iv_search_string
          io_tech_request_context  = io_tech_request_context
        IMPORTING
          et_entityset             = wooperationsset
          es_response_context      = es_response_context.


      copy_data_to_ref(
       EXPORTING
         is_data = wooperationsset
       CHANGING
         cr_data = er_entityset ).

    WHEN 'WoTaskOrdersSet'.

      CALL METHOD me->wotaskordersset_get_entityset
        EXPORTING
          iv_entity_name           = iv_entity_name
          iv_entity_set_name       = iv_entity_set_name
          iv_source_name           = iv_source_name
          it_filter_select_options = it_filter_select_options
          is_paging                = is_paging
          it_key_tab               = it_key_tab
          it_navigation_path       = it_navigation_path
          it_order                 = it_order
          iv_filter_string         = iv_filter_string
          iv_search_string         = iv_search_string
          io_tech_request_context  = io_tech_request_context
        IMPORTING
          et_entityset             = wotaskordersset
          es_response_context      = es_response_context.


      copy_data_to_ref(
         EXPORTING
           is_data = wotaskordersset
         CHANGING
           cr_data = er_entityset ).

*-------------------------------------------------------------------------*
*             EntitySet - Terrain_ext Set
*-------------------------------------------------------------------------*

    WHEN 'TerrainSet'.

      CALL METHOD me->terrainset_get_entityset
        EXPORTING
          iv_entity_name           = iv_entity_name
          iv_entity_set_name       = iv_entity_set_name
          iv_source_name           = iv_source_name
          it_filter_select_options = it_filter_select_options
          is_paging                = is_paging
          it_key_tab               = it_key_tab
          it_navigation_path       = it_navigation_path
          it_order                 = it_order
          iv_filter_string         = iv_filter_string
          iv_search_string         = iv_search_string
          io_tech_request_context  = io_tech_request_context
        IMPORTING
          et_entityset             = terrainset
          es_response_context      = es_response_context.


      copy_data_to_ref(
         EXPORTING
           is_data = terrainset
         CHANGING
           cr_data = er_entityset ).

*-------------------------------------------------------------------------*
*             EntitySet - WoComponents_ext Set
*-------------------------------------------------------------------------*

    WHEN 'WoComponentsSet'.

      CALL METHOD me->wocomponentsset_get_entityset
        EXPORTING
          iv_entity_name           = iv_entity_name
          iv_entity_set_name       = iv_entity_set_name
          iv_source_name           = iv_source_name
          it_filter_select_options = it_filter_select_options
          is_paging                = is_paging
          it_key_tab               = it_key_tab
          it_navigation_path       = it_navigation_path
          it_order                 = it_order
          iv_filter_string         = iv_filter_string
          iv_search_string         = iv_search_string
          io_tech_request_context  = io_tech_request_context
        IMPORTING
          et_entityset             = wocomponentsset
          es_response_context      = es_response_context.


      copy_data_to_ref(
         EXPORTING
           is_data = wocomponentsset
         CHANGING
           cr_data = er_entityset ).

*-------------------------------------------------------------------------*
*             EntitySet - ReasonList_ext Set
*-------------------------------------------------------------------------*

    WHEN 'ReasonListSet'.

      CALL METHOD me->reasonlistset_get_entityset
        EXPORTING
          iv_entity_name           = iv_entity_name
          iv_entity_set_name       = iv_entity_set_name
          iv_source_name           = iv_source_name
          it_filter_select_options = it_filter_select_options
          is_paging                = is_paging
          it_key_tab               = it_key_tab
          it_navigation_path       = it_navigation_path
          it_order                 = it_order
          iv_filter_string         = iv_filter_string
          iv_search_string         = iv_search_string
          io_tech_request_context  = io_tech_request_context
        IMPORTING
          et_entityset             = reasonlistset
          es_response_context      = es_response_context.


      copy_data_to_ref(
         EXPORTING
           is_data = reasonlistset
         CHANGING
           cr_data = er_entityset ).

    WHEN 'WoRouteSet'.

      CALL METHOD me->worouteset_get_entityset
        EXPORTING
          iv_entity_name           = iv_entity_name
          iv_entity_set_name       = iv_entity_set_name
          iv_source_name           = iv_source_name
          it_filter_select_options = it_filter_select_options
          is_paging                = is_paging
          it_key_tab               = it_key_tab
          it_navigation_path       = it_navigation_path
          it_order                 = it_order
          iv_filter_string         = iv_filter_string
          iv_search_string         = iv_search_string
          io_tech_request_context  = io_tech_request_context
        IMPORTING
          et_entityset             = worouteset
          es_response_context      = es_response_context.


      copy_data_to_ref(
         EXPORTING
           is_data = worouteset
         CHANGING
           cr_data = er_entityset ).

  ENDCASE.

ENDMETHOD.


METHOD delete_from_database.

  DATA : lr_expimp  TYPE REF TO cl_abap_expimp_db.
  CREATE OBJECT lr_expimp.

  TRY.
* DELETE FROM DATABASE INDX(ZZ) ID KEY.

      CALL METHOD lr_expimp->delete
        EXPORTING
          tabname          = tabname "‘INDX’
          client           = client "‘800’
          area             = area "‘ZZ’
          id               = id "key
*         GENERIC_KEY      = ABAP_FALSE
          client_specified = abap_true.
    CATCH cx_sy_client .
    CATCH cx_sy_generic_key .
    CATCH cx_sy_incorrect_key .
  ENDTRY.

ENDMETHOD.


  METHOD employee_roleset_get_entityset.
    "local declarations.
    DATA : lr_srtfd TYPE RANGE OF indx_srtfd.

    " request_header call.
    CALL METHOD me->request_header.

    lr_srtfd = VALUE #( sign = 'I' option = 'EQ' ( low = 'ZZWOROT')
                                                 ( low = 'ZZQAOPR')
                                                 ( low = 'ZZQACHR')
                                                 ( low = 'ZZMESDC')
                                                 ( low = 'ZZQACF4')
                                                 ( low = 'ZZMDATTRB')
                                                 ( low = 'ZZMDATRB')
                                                 ( low = 'ZZWOACT')
                                                 ( low = 'ZZWOMAT')
                                                 ( low = 'ZZWOOPE')
                                                 ( low = 'ZZWOTSK')
                                                 ( low = 'ZZATTF4')
                                                 ( low = 'ZZATTRB')
                                                 ( low = 'ZZGNTRR')
                                                 ( low = 'ZZQATRR')
                                                 ( low = 'ZZQATSK')
                                                 ( low = 'ZZQAFCH')
                                                 ( low = 'ZZWOCOM') ).

    get_employee_roles(
          EXPORTING
            io_tech_request_context = io_tech_request_context
          IMPORTING
            et_entityset            = et_entityset ).

*-------------------------------------------------------------
*-- Delta Token implemented for performance
*-------------------------------------------------------------

    CALL METHOD me->get_delta_token
      EXPORTING
        io_tech_request_context  = io_tech_request_context
        mr_service_document_name = mr_service_document_name
        mr_service_version       = mr_service_version
        it_entityset             = et_entityset
      IMPORTING
        es_response_context      = es_response_context.

    "clearing the INDX table entries.
    DELETE FROM indx WHERE relid EQ 'ID' AND srtfd IN lr_srtfd.

  ENDMETHOD.


METHOD exception_messages.

  DATA: lwa_bapi_messsages TYPE bapiret2,
        lo_msg_container   TYPE REF TO /iwbep/if_message_container,
        ls_message         TYPE /agri/s_gprolog,
        lv_msg_error       TYPE xfld,
        lv_msgno           TYPE symsgno.

****Authorizations - Activities
  CONSTANTS: BEGIN OF c_msg_type,
               info    LIKE sy-msgty VALUE 'I',             "#EC NOTEXT
               warning LIKE sy-msgty VALUE 'W',             "#EC NOTEXT
               error   LIKE sy-msgty VALUE 'E',             "#EC NOTEXT
               abend   LIKE sy-msgty VALUE 'A',             "#EC NOTEXT
               success LIKE sy-msgty VALUE 'S',             "#EC NOTEXT
               x       LIKE sy-msgty VALUE 'X',             "#EC NOTEXT
             END OF c_msg_type .
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
      IF ls_message-msgty = c_msg_type-error. "'E'.
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

    IF lv_msg_error IS NOT INITIAL.
*    " Raise Exception
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          textid            = /iwbep/cx_mgw_busi_exception=>business_error
          entity_type       = iv_entity_name
          message_container = lo_msg_container.
    ENDIF.
  ENDIF.

*-- From Bapi Messages
  IF it_bapi_messsages IS NOT INITIAL.
    READ TABLE it_bapi_messsages INTO lwa_bapi_messsages
         WITH KEY type = c_msg_type-error. " 'E'.
    IF sy-subrc = 0.
      lv_msg_error = c_true.
    ENDIF.

    lo_msg_container->add_messages_from_bapi(
          it_bapi_messages         = it_bapi_messsages
          iv_determine_leading_msg =
   /iwbep/if_message_container=>gcs_leading_msg_search_option-first
          iv_entity_type           = iv_entity_name ).

    IF lv_msg_error IS NOT INITIAL.
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


METHOD get_delta_token.
  DATA:lo_dp_facade   TYPE REF TO /iwbep/if_mgw_dp_facade,
       lo_dp_facade_1 TYPE REF TO /iwbep/if_mgw_dp_fw_facade,

       lv_format      TYPE string,
       lv_delta_token TYPE string.
** get the data provider facade
  TRY.
      lo_dp_facade = /iwbep/if_mgw_conv_srv_runtime~get_dp_facade( ).
    CATCH /iwbep/cx_mgw_tech_exception.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception.
  ENDTRY.

* call the delta token functionality
  IF lo_dp_facade IS BOUND.
    lo_dp_facade_1 ?= lo_dp_facade.
    CALL METHOD lo_dp_facade_1->get_format
      RECEIVING
        rv_format = lv_format.
    IF lv_format NE 'json'.
      TRY.
          CALL METHOD /iwbep/cl_query_result_log=>create_update_log_entry_hash
            EXPORTING
              io_tech_request_context  = io_tech_request_context
              io_dp_facade             = lo_dp_facade
              ir_service_document_name = mr_service_document_name
              ir_service_version       = mr_service_version
              it_entityset             = it_entityset
            CHANGING
              ev_delta_token           = lv_delta_token.
**                  es_response_context-deltatoken = lv_delta_token.
        CATCH /iwbep/cx_qrl_locked.
          RAISE EXCEPTION TYPE /iwbep/cx_qrl_locked.
        CATCH /iwbep/cx_qrl_delta_unavailabl.
          RAISE EXCEPTION TYPE /iwbep/cx_qrl_delta_unavailabl.
      ENDTRY.
* export the delta token
      es_response_context-deltatoken = lv_delta_token.
    ENDIF.
  ENDIF.
ENDMETHOD.


  METHOD get_employee_roles.
*----------------------------------------------------------------------*
* General Information
*----------------------------------------------------------------------*
* WRICEF ID    : Documento for standard app issues
* Class        : ZCL_ABS_AGRI_MOBILE_E_DPC_EXT
* Method       : EMPLOYEE_ROLE_GET_ENTITYSET
* Company      : Vistex
* Dev. Author  : Umakanth kumchala
* Create Date  : 12.03.2020
* Description  : New Entity Set checking the User+Employee based valiadtion
*----------------------------------------------------------------------*
* Modification log
*----------------------------------------------------------------------*
* Date         | TR#         | Author          | Modification
*----------------------------------------------------------------------*
*                                                                      *
*----------------------------------------------------------------------*
    "local declarations.
    DATA : lt_filter     TYPE /iwbep/t_mgw_select_option,
           ls_filter     TYPE /iwbep/s_mgw_select_option,
           lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
           lv_filter_str TYPE string,
           lt_dd07       TYPE TABLE OF dd07v,
           lrt_pernr     TYPE RANGE OF persno,
           lrs_pernr     LIKE LINE OF lrt_pernr,
           ls_entity_set LIKE LINE OF et_entityset.

    "message objects.
    DATA :lo_msg_container TYPE REF TO /iwbep/if_message_container,
          ls_messages      TYPE /agri/s_gprolog,
          lv_msgno_eh      TYPE symsgno.
*** Get Message container object
    lo_msg_container = mo_context->get_message_container( ).

    lo_filter     = io_tech_request_context->get_filter( ).
    lt_filter     = lo_filter->get_filter_select_options( ).
    lv_filter_str = lo_filter->get_filter_string( ).

* Maps filter table lines to function module parameters
    LOOP AT lt_filter INTO ls_filter.

      CASE ls_filter-property.
        WHEN 'PERNR'.
          lo_filter->convert_select_option(
            EXPORTING
              is_select_option = ls_filter
            IMPORTING
              et_select_option = lrt_pernr ).
        WHEN OTHERS.
          " Log message in the application log
          me->/iwbep/if_sb_dpc_comm_services~log_message(
            EXPORTING
              iv_msg_type   = 'E'
              iv_msg_id     = '/IWBEP/MC_SB_DPC_ADM'
              iv_msg_number = 020
              iv_msg_v1     = ls_filter-property ).
          " Raise Exception
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
            EXPORTING
              textid = /iwbep/cx_mgw_tech_exception=>internal_error.
      ENDCASE.
    ENDLOOP.

    SELECT pernr
      FROM pa0000
      INTO TABLE @DATA(lt_pa0000)
     WHERE pernr IN @lrt_pernr
       AND begda LE @sy-datum
       AND endda GE @sy-datum
       AND stat2 = 3.
    IF sy-subrc NE 0.
      lo_msg_container = mo_context->get_message_container( ).
      lo_msg_container->add_message(
         EXPORTING iv_msg_type               = 'E'"is_message-msgty
                   iv_msg_id                 = '00'"is_message-msgid
                   iv_msg_number             = '001'"lv_msgno
                   iv_msg_v1                 = text-001 "'Invalid user'"is_message-msgv1
*                 iv_msg_v2                 = is_message-msgv2
*                 iv_msg_v3                 = is_message-msgv3
*                 iv_msg_v4                 = is_message-msgv4
*                 iv_msg_text               = iv_message_text
                   iv_entity_type            = 'Employee_Role'"iv_entity_name
                   iv_add_to_response_header = abap_true ).

      EXIT.
    ENDIF.
    "change -> Login User based validation to User+Employee based valiadtion
    SELECT *
      FROM zabs_emp_role
      INTO TABLE @DATA(lt_usr_emp)
     WHERE pernr IN @lrt_pernr.

    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = 'ZABS_DOM_FPCNF'
        text           = 'X'
        langu          = sy-langu
*       BYPASS_BUFFER  = ' '
*     IMPORTING
*       RC             =
      TABLES
        dd07v_tab      = lt_dd07
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    LOOP AT lt_usr_emp INTO DATA(ls_usr_emp).
      MOVE-CORRESPONDING ls_usr_emp TO ls_entity_set.
*    READ TABLE lt_dd07 INTO DATA(ls_dd07) WITH KEY domvalue_l = ls_entity_set-fpcnf.
*    IF sy-subrc EQ 0.
*      ls_entity_set-ctext = ls_dd07-ddtext.
*    ENDIF.

      APPEND ls_entity_set TO et_entityset.
      CLEAR :ls_entity_set,
             ls_usr_emp.
    ENDLOOP.

  ENDMETHOD.


METHOD get_reasonlist.

  DATA: lrt_reason TYPE RANGE OF zabs_e_reason.

  DATA: lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
        lt_filter     TYPE /iwbep/t_mgw_select_option,
        ls_filter     TYPE /iwbep/s_mgw_select_option,
        lv_period     TYPE zfmacperiod,
        lv_filter_str TYPE string.

  lo_filter     = io_tech_request_context->get_filter( ).
  lt_filter     = lo_filter->get_filter_select_options( ).
  lv_filter_str = lo_filter->get_filter_string( ).

  LOOP AT lt_filter INTO ls_filter.

    CASE ls_filter-property.
      WHEN 'REASON'.
        TRY.
            lo_filter->convert_select_option(
              EXPORTING
                is_select_option = ls_filter
              IMPORTING
                et_select_option = lrt_reason ).
        ENDTRY.
      WHEN OTHERS.
*-- Log message in the application log
        me->/iwbep/if_sb_dpc_comm_services~log_message(
          EXPORTING
            iv_msg_type   = 'E'
            iv_msg_id     = '/IWBEP/MC_SB_DPC_ADM'
            iv_msg_number = 020
            iv_msg_v1     = ls_filter-property ).
        " Raise Exception
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
          EXPORTING
            textid = /iwbep/cx_mgw_tech_exception=>internal_error.
    ENDCASE.

  ENDLOOP.

  SELECT *
    FROM zabs_mcmpreason
    INTO TABLE @DATA(lt_mcmpreason)
    WHERE reason IN @lrt_reason.

  IF sy-subrc = 0.

    LOOP AT lt_mcmpreason ASSIGNING FIELD-SYMBOL(<fs_mcmpreason>).
      APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<fs_data>).
      MOVE-CORRESPONDING <fs_mcmpreason> TO <fs_data>.
    ENDLOOP.

  ENDIF.

ENDMETHOD.


METHOD get_route_terrain_dtls.
*----------------------------------------------------------------------*
* General Information
*----------------------------------------------------------------------*
* WRICEF ID    : Documento for standard app issues
* Class        : ZCL_ABS_AGRI_MOBILE_E_DPC_EXT
* Method       : WoRoute_GET_ENTITYSET
* Company      : Vistex
* Dev. Author  : Umakanth kumchala
* Create Date  : 12.03.2020
* Description  : New Entity Set for route terrains based on BADGE
*                Here BAdge can be PERNR Or LIFNR
*----------------------------------------------------------------------*
* Modification log
*----------------------------------------------------------------------*
* Date         | TR#         | Author          | Modification
*----------------------------------------------------------------------*
*                                                                      *
*----------------------------------------------------------------------*
  "local declarations.
  DATA:
    lt_rtfla             TYPE TABLE OF /agri/glrtfla,
    ls_rtfla             TYPE /agri/glrtfla,
    ls_entityset         LIKE LINE OF et_entityset,
*    lt_glflot            TYPE zcl_zabs_agri_mobile_e_mpc=>tt_worouteterrainextend,    "TABLE OF ltyp_flot, "/agri/glflot,
    lt_strno             TYPE TABLE OF /agri/glstrno,
    ls_strno             TYPE /agri/glstrno,
    lrt_tplnr_fl         TYPE RANGE OF /agri/gltplnr_fl,
    lrs_tplnr_fl         LIKE LINE OF lrt_tplnr_fl,
*    lt_rtusr             TYPE TABLE OF /agri/glrtusr,
    lrt_pernr            TYPE RANGE OF persno,
    lr_pernr             LIKE LINE OF lrt_pernr,
    lrt_lifnr            TYPE RANGE OF lifnr,
    lrt_cmnum            TYPE RANGE OF /agri/glcmnum,
    ls_cmnum             LIKE LINE OF lrt_cmnum,
    lr_lifnr             LIKE LINE OF lrt_lifnr,
    lt_rtusr             TYPE TABLE OF zabs_usrpernr,
    lo_message_container TYPE REF TO /iwbep/if_message_container,
    lv_tmstmp            TYPE ad_tstamp.
*    ls_glflot            TYPE zcl_zabs_agri_mobile_e_mpc=>ts_worouteterrainextend.  "ltyp_flot. "/agri/glflot.

  DATA : lrt_route         TYPE RANGE OF /agri/gl_route, "/agri/gl_route.
         lt_filter         TYPE /iwbep/t_mgw_select_option,
         ls_filter         TYPE /iwbep/s_mgw_select_option,
         ls_converted_keys LIKE LINE OF et_entityset,
         lo_filter         TYPE REF TO /iwbep/if_mgw_req_filter,
         lv_filter_str     TYPE string,
         lrt_aslvl         TYPE RANGE OF /agri/glaslvl,
         lv_persno         TYPE persno,
         lv_lifnr          TYPE lifnr,
         lv_cnval1         TYPE zabs_del_cnval,
         lv_prevdate       TYPE p0001-begda,
         lv_days           TYPE t5a4a-dlydy.

  "constants.
  CONSTANTS: lc_astat TYPE /agri/glastat  VALUE 'A'.

  lo_filter     = io_tech_request_context->get_filter( ).
  lt_filter     = lo_filter->get_filter_select_options( ).
  lv_filter_str = lo_filter->get_filter_string( ).


* Get key table information
  io_tech_request_context->get_converted_source_keys(
    IMPORTING
      es_key_values  = ls_converted_keys ).

* Maps filter table lines to function module parameters
  LOOP AT lt_filter INTO ls_filter.

    CASE ls_filter-property.
      WHEN 'TPLNR_FL'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_tplnr_fl ).
      WHEN 'CMNUM'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_cmnum ).
      WHEN 'ROUTE'.
        TRY.
            lo_filter->convert_select_option(
              EXPORTING
                is_select_option = ls_filter
              IMPORTING
                et_select_option = lrt_route ).
          CATCH /iwbep/cx_mgw_tech_exception. " is not caught or declared in

        ENDTRY.
      WHEN 'PERNR'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_pernr ).
        READ TABLE lrt_pernr INTO lr_pernr INDEX 1.
        IF sy-subrc EQ 0.
          lv_persno = lr_pernr-low.
        ENDIF.
      WHEN 'LIFNR'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_lifnr ).
        READ TABLE lrt_lifnr INTO lr_lifnr INDEX 1.
        IF sy-subrc EQ 0.
          lv_lifnr = lr_lifnr-low.
        ENDIF.
      WHEN OTHERS.
*          " Log message in the application log
*          me->/iwbep/if_sb_dpc_comm_services~log_message(
*            EXPORTING
*              iv_msg_type   = 'E'
*              iv_msg_id     = '/IWBEP/MC_SB_DPC_ADM'
*              iv_msg_number = 020
*              iv_msg_v1     = ls_filter-property ).
*          " Raise Exception
*          RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
*            EXPORTING
*              textid = /iwbep/cx_mgw_tech_exception=>internal_error.
    ENDCASE.
  ENDLOOP.

  IF lrt_tplnr_fl IS NOT INITIAL.
    LOOP AT lrt_tplnr_fl ASSIGNING FIELD-SYMBOL(<lrs_tplnr_fl>).
      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
        EXPORTING
          input      = <lrs_tplnr_fl>-low
        IMPORTING
          output     = <lrs_tplnr_fl>-low
        EXCEPTIONS
          not_found  = 1
          not_active = 2
          OTHERS     = 3.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
*      UNASSIGN <lrs_tplnr_fl>.
    ENDLOOP.
  ENDIF.
*  IF lrt_route IS NOT INITIAL.
*    SELECT * FROM /agri/glrtusr
*           INTO TABLE @lt_rtusr
*           WHERE route IN @lrt_route
*             AND bname  = @sy-uname.
*  ELSE.
*    SELECT * FROM /agri/glrtusr
*     INTO TABLE @lt_rtusr
*     WHERE bname = @sy-uname.
*  ENDIF.

  "change -> Login User based validation to User+Employee based valiadtion
*  IF lrt_route IS NOT INITIAL.
*    SELECT *
*      FROM ZABS_USRPERNR
*      INTO TABLE @lt_rtusr
*       WHERE route IN @lrt_route
*         AND pernr IN @lrt_pernr.
*  ELSE.
*--Sselect Data Based on PERNR(Employee) or LIFNR(Vendor)
  IF lrt_pernr IS NOT INITIAL.
    SELECT *
    FROM zabs_usrpernr
    INTO TABLE lt_rtusr
    WHERE route IN lrt_route
      AND pernr IN lrt_pernr.
  ELSEIF lrt_lifnr IS NOT INITIAL.
    SELECT *
    FROM zabs_usrpernr
    INTO TABLE lt_rtusr
    WHERE route IN lrt_route
      AND lifnr IN lrt_lifnr.
  ENDIF.
*  ENDIF.


*  IF lrt_route IS NOT INITIAL and
*     lt_rtusr IS NOT INITIAL.
*    SELECT * FROM /agri/glrtfla
*          INTO TABLE lt_rtfla
*      FOR ALL ENTRIES IN lt_rtusr
*          WHERE route EQ lt_rtusr-route.
*  ELSEif lt_rtusr IS NOT INITIAL.
*    SELECT * FROM /agri/glrtfla
*          INTO TABLE lt_rtfla
*      FOR ALL ENTRIES IN lt_rtusr
*          WHERE route EQ lt_rtusr-route.
*
*  ENDIF.
*---Fetching Route based Terrain
  IF lt_rtusr IS NOT INITIAL.
    SELECT * FROM /agri/glrtfla
          INTO TABLE @lt_rtfla
      FOR ALL ENTRIES IN @lt_rtusr                 "#EC CI_NO_TRANSFORM
          WHERE route  = @lt_rtusr-route
            AND tplnr_fl IN @lrt_tplnr_fl.
    SORT lt_rtfla BY tplnr_fl.
  ENDIF.

*--Get variant table data
  CALL METHOD zcl_abs_get_variants=>get_constant_single
    EXPORTING
      iv_mod    = 'C'
      iv_objid  = 'MOBL'
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

*--Check Terrain Deletion Indicator and Active Crop Season Status.
  IF lt_rtfla IS NOT INITIAL.
    SELECT tplnr_fl, pltxt, tplma,
*          iwerk,
           strno, aedat, erdat
      FROM /agri/glflot
*      INTO CORRESPONDING FIELDS OF TABLE @lt_glflot
      INTO TABLE @DATA(lt_glflot)
      FOR ALL ENTRIES IN @lt_rtfla                 "#EC CI_NO_TRANSFORM
      WHERE tplnr_fl   = @lt_rtfla-tplnr_fl
        AND kfrst      = @space
        AND loevm      = @space. "abap_false.
    IF sy-subrc = 0.
      SORT lt_glflot BY tplnr_fl.
*      IF sy-subrc = 0.
      SELECT  tplnr_fl,                                "#EC CI_DYNWHERE
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
      FOR ALL ENTRIES IN @lt_glflot                "#EC CI_NO_TRANSFORM
           WHERE tplnr_fl = @lt_glflot-tplnr_fl
              AND cmnum IN @lrt_cmnum
              AND astat EQ @lc_astat
              AND datab LE @sy-datum
              AND datbi GE @lv_prevdate
*                AND datab <= @sy-datum
*                AND datbi >= @sy-datum
              AND loevm  = @space.
*      ENDIF. "lt_glflot

*---BOC "28/07/2020
      SELECT tplnr_fl, tplma, rbnr1
        FROM /agri/glflot
        INTO TABLE @DATA(lt_glflota)
         FOR ALL ENTRIES IN @lt_glflot
       WHERE tplnr_fl = @lt_glflot-tplma
         AND loevm    = @space."abap_true.
      IF sy-subrc = 0.
        SORT lt_glflota BY tplnr_fl.
      ENDIF.
*---EOC "28/07/2020

    ENDIF. "lt_glflot
  ENDIF. "lt_rtfla

*  LOOP AT lt_rtfla INTO ls_rtfla.
*
*    READ TABLE lt_glflot INTO ls_glflot
*      WITH KEY tplnr_fl = ls_rtfla-tplnr_fl BINARY SEARCH.
*
*    IF sy-subrc = 0.
*      ls_entityset-strno = ls_glflot-strno.
*      ls_entityset-pltxt = ls_glflot-pltxt.
*    ENDIF.
*
*    MOVE-CORRESPONDING ls_rtfla TO ls_entityset.
*    CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
*      EXPORTING
*        input  = ls_entityset-tplnr_fl
*      IMPORTING
*        output = ls_entityset-tplnr_fl.
*    ls_entityset-pernr = lv_PERSNO.
*    APPEND ls_entityset TO et_entityset.
*    CLEAR ls_entityset.
*  ENDLOOP.

  LOOP AT lt_flcma ASSIGNING FIELD-SYMBOL(<fs_flcma>).
*    CLEAR: ls_glflot.
    READ TABLE lt_glflot INTO DATA(ls_glflot) WITH KEY
                                        tplnr_fl = <fs_flcma>-tplnr_fl
                                        BINARY SEARCH.
    IF sy-subrc = 0.
      ls_entityset-cmnum = <fs_flcma>-cmnum.
      ls_entityset-strno = ls_glflot-strno.
      ls_entityset-pltxt = ls_glflot-pltxt.
*      ls_entityset-iwerk = ls_glflot-iwerk.

      CALL FUNCTION 'ADDR_CONVERT_DATE_TO_TIMESTAMP'
        EXPORTING
          iv_date      = ls_glflot-erdat
*         IV_HIGH      = ' '
        IMPORTING
          ev_timestamp = lv_tmstmp.

      ls_entityset-erdat = lv_tmstmp.

      CALL FUNCTION 'ADDR_CONVERT_DATE_TO_TIMESTAMP'
        EXPORTING
          iv_date      = ls_glflot-aedat
*         IV_HIGH      = ' '
        IMPORTING
          ev_timestamp = lv_tmstmp.

      ls_entityset-aedat = lv_tmstmp.

*---BOC "28/07/2020
      READ TABLE lt_glflota INTO DATA(ls_glflota) WITH KEY
                                        tplnr_fl = ls_glflot-tplma
                                        BINARY SEARCH.
      IF sy-subrc = 0.
        ls_entityset-rbnr1 = ls_glflota-rbnr1.
      ENDIF.
*---EOC "28/07/2020

      CLEAR: ls_rtfla.
      READ TABLE lt_rtfla INTO ls_rtfla WITH KEY
                                        tplnr_fl = <fs_flcma>-tplnr_fl
                                        BINARY SEARCH.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING ls_rtfla TO ls_entityset.
        ls_entityset-tplnr = ls_entityset-tplnr_fl.
        CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
          EXPORTING
            input  = ls_entityset-tplnr_fl
          IMPORTING
            output = ls_entityset-tplnr_fl.

        IF lv_persno IS NOT INITIAL.
          ls_entityset-pernr = lv_persno.
        ELSEIF lv_lifnr IS NOT INITIAL.
          ls_entityset-lifnr = lv_lifnr.
        ENDIF.

        APPEND ls_entityset TO et_entityset.
        CLEAR ls_entityset.
      ENDIF.
    ENDIF.

  ENDLOOP.

  et_rttrn[] = et_entityset[].

ENDMETHOD.


METHOD get_skip_token.
************************************************************************
*  Confidential property of Dole                                       *
*  All Rights Reserved                                                 *
************************************************************************
*      Method Name  : GET_SKIP_TOKEN                                   *
*      TCode        : -NA-                                             *
*      Created By   : Zukumchala                                       *
*      Requested by : -NA-                                             *
*      Created on   : 24.02.2020                                       *
*      RICEF        : -NA-                                             *
*      PROJECT      : ABS                                              *
*      FSD Name     : -NA-                                             *
*      TSD Name     : -NA-                                             *
*      TR           : D4HK903543                                       *
*      Version      : 1                                                *
*      Description  : Applying the skip token                          *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
**--------------------------------------------------------------------**
* MOD#  | Date       |  Programmer  | CTS | Description ( CRF/TPR Name)*
*                                                                      *
*&--------------------------------------------------------------------&*
*                                                                      *
**--------------------------------------------------------------------**
  DATA:
*-- Variables's
    lv_top        TYPE int4,
    lv_skip       TYPE int4,
    lv_skiptoken  TYPE int4,
    lv_upto       TYPE int4,
    lv_table_size TYPE i,
    lv_page_size  TYPE i.

*--------------------------------------------------------------------*
*-----------  $Skip Token Functionality   ---------------------------*
*--------------------------------------------------------------------*
*** $skip token functionality
* Obtain the $skip token value if it exists in the URL
  lv_top       = io_tech_request_context->get_top( ).
  lv_skiptoken = io_tech_request_context->get_skiptoken( ).

  IF lv_top IS INITIAL.
*-- Get Skip token value from Variant Table
    CALL METHOD zcl_abs_get_variants=>get_constant_single       " JOBREGON: Start
      EXPORTING
        iv_mod    = abap_true
        iv_objid  = zcl_abs_odata_maintain=>c_mobl_objid
        iv_k1val  = zcl_abs_odata_maintain=>c_mobl_val
        iv_k2val  = zcl_abs_odata_maintain=>c_ctry
        iv_k3val  = zcl_abs_odata_maintain=>c_token_skip
      IMPORTING
        ev_cnval1 = lv_top.

    IF lv_top IS INITIAL.
      lv_top = 500.
    ENDIF.
  ENDIF.
  IF lv_top IS NOT INITIAL.      "Checking Top and Skip
    DESCRIBE TABLE ct_entityset LINES lv_table_size.
    es_response_context-inlinecount = lv_table_size.

    IF lv_skiptoken LE lv_table_size.
      IF lv_skiptoken IS INITIAL.
        lv_skiptoken = 1.
      ENDIF.
      "getting Top count from Psize
*--------->> Client Paging (top/skip)
      lv_skip = lv_skiptoken.
      IF lv_skip IS INITIAL.
        lv_upto = lv_skiptoken + lv_top.
      ELSE.
        lv_upto = lv_skiptoken + lv_top.
      ENDIF.

      lv_upto = lv_upto + 1.
      IF lv_skip = 1.
        DELETE ct_entityset FROM lv_upto.
      ELSE.
        lv_skip = lv_skip - 1.
        DELETE ct_entityset FROM lv_upto.
        DELETE ct_entityset FROM 1 TO lv_skip.
      ENDIF.
      lv_upto = lv_upto - 1.

      lv_skiptoken = lv_upto + 1.
**-- Next Link
      es_response_context-skiptoken = lv_skiptoken.
      CONDENSE es_response_context-skiptoken.
      IF lv_table_size <= lv_upto.
        ev_totrecords = lv_top + 1.
      ELSE.
        ev_totrecords = lv_top.
      ENDIF.
      RETURN.
    ELSE.
      CLEAR: lv_skiptoken.
      REFRESH ct_entityset.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD get_terrainset.
*----------------------------------------------------------------------*
* General Information
*----------------------------------------------------------------------*
* WRICEF ID    : Issue # 14
* Class        : ZCL_ABS_AGRI_MOBILE_E_DPC_EXT
* Method       : TERRAINSET_EXT_GET_ENTITYSET
* Company      : Vistex
* Dev. Author  : John Anthony Obregon Lanas
* Create Date  : 10.04.2020
* Description  : New Entity Set that filters terrains with task orders
*                associated.
*----------------------------------------------------------------------*
* Modification log
*----------------------------------------------------------------------*
* Date         | TR#         | Author          | Modification
*----------------------------------------------------------------------*
* 10.04.2020     C4DK909655    T_A.Harshe        Creation
*----------------------------------------------------------------------*

  CONSTANTS: lc_autyp TYPE /agri/gl_autyp VALUE 'TO',
             lc_cstat TYPE /agri/fmcstat  VALUE 'CNF'.

  DATA: lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
        lt_filter     TYPE /iwbep/t_mgw_select_option,
        ls_filter     TYPE /iwbep/s_mgw_select_option,
        lv_filter_str TYPE string.

  DATA: lt_flot      TYPE TABLE OF /agri/glflot,
        lt_flott     TYPE TABLE OF /agri/glflot,
        lt_rtfla     TYPE TABLE OF /agri/glrtfla,
        lrt_route    TYPE RANGE OF /agri/gl_route,
        lrt_pernr    TYPE RANGE OF persno,
        lrt_lifnr    TYPE RANGE OF lifnr,
        lr_pernr     LIKE LINE OF lrt_pernr,
        lv_persno    TYPE persno,
        lr_lifnr     LIKE LINE OF lrt_lifnr,
        lv_lifnr     TYPE lifnr,
        ls_flot      TYPE /agri/s_glflot,
        lt_rttrn     TYPE ZCL_ZABS_MOB_TOC_MPC=>tt_worouteterrain,
        lt_rttrn_tmp TYPE ZCL_ZABS_MOB_TOC_MPC=>tt_worouteterrain,
        ls_rttrn     TYPE ZCL_ZABS_MOB_TOC_MPC=>ts_worouteterrain,
        lt_entityset TYPE ZCL_ZABS_MOB_TOC_MPC=>tt_terrain,
        ls_entityset LIKE LINE OF et_entityset,
        lt_orderby   TYPE /iwbep/t_mgw_tech_order,
        ls_orderby   TYPE /iwbep/s_mgw_tech_order,
        lt_rtusr     TYPE TABLE OF zabs_usrpernr,
        lv_tmstmp    TYPE ad_tstamp,
        lv_autyp     TYPE /agri/gl_autyp,
        lv_urole     TYPE zabs_del_urole.

  lo_filter     = io_tech_request_context->get_filter( ).
  lt_filter     = lo_filter->get_filter_select_options( ).
  lv_filter_str = lo_filter->get_filter_string( ).

  LOOP AT lt_filter INTO ls_filter.

    CASE ls_filter-property.
      WHEN 'ROUTE'.
        TRY.
            lo_filter->convert_select_option(
              EXPORTING
                is_select_option = ls_filter
              IMPORTING
                et_select_option = lrt_route ).
        ENDTRY.
      WHEN 'PERNR'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_pernr ).
        READ TABLE lrt_pernr INTO lr_pernr INDEX 1.
        IF sy-subrc EQ 0.
          lv_persno = lr_pernr-low.
        ENDIF.
      WHEN 'LIFNR'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_lifnr ).
        READ TABLE lrt_lifnr INTO lr_lifnr INDEX 1.
        IF sy-subrc EQ 0.
          lv_lifnr = lr_lifnr-low.
        ENDIF.
      WHEN OTHERS.
*-- Log message in the application log
        me->/iwbep/if_sb_dpc_comm_services~log_message(
          EXPORTING
            iv_msg_type   = 'E'
            iv_msg_id     = '/IWBEP/MC_SB_DPC_ADM'
            iv_msg_number = 020
            iv_msg_v1     = ls_filter-property ).
        " Raise Exception
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
          EXPORTING
            textid = /iwbep/cx_mgw_tech_exception=>internal_error.
    ENDCASE.
  ENDLOOP.

*  IF lrt_route IS NOT INITIAL.
*    SELECT *
*      FROM /agri/glrtusr
*      INTO TABLE @lt_rtusr
*      WHERE route IN @lrt_route
*        AND bname  = @sy-uname.
*  ELSE.
*    SELECT *
*     FROM /agri/glrtusr
*     INTO TABLE @lt_rtusr
*     WHERE bname = @sy-uname.
*  ENDIF.

*Fetch PERNR/LIFNR based Terrains
  get_route_terrain_dtls(
      EXPORTING
        io_tech_request_context = io_tech_request_context
      IMPORTING
        et_rttrn                = lt_rttrn ).
  IF lt_rttrn[] IS INITIAL.
    RETURN.
  ENDIF.
*"change -> Login User based validation to User+Employee based valiadtion
*  IF lrt_route IS NOT INITIAL.
*    SELECT *
*      FROM ZABS_USRPERNR
*      INTO TABLE @lt_rtusr
*       WHERE route IN @lrt_route
*         AND pernr IN @lrt_pernr.
*  ELSE.
*    IF lrt_pernr is not INITIAL.
*        SELECT *
*        FROM ZABS_USRPERNR
*        INTO TABLE lt_rtusr
*        WHERE pernr IN lrt_pernr.
*    ELSEIF lrt_lifnr is not INITIAL.
*        SELECT *
*        FROM ZABS_USRPERNR
*        INTO TABLE lt_rtusr
*        WHERE lifnr IN lrt_lifnr.
*    ENDIF.
*  ENDIF.

*  IF lt_rtusr IS NOT INITIAL.
*    SELECT *
*    FROM /agri/glrtfla
*    INTO TABLE @lt_rtfla
*    FOR ALL ENTRIES IN @lt_rtusr                   "#EC CI_NO_TRANSFORM
*    WHERE route = @lt_rtusr-route.
*    SORT lt_rtfla BY tplnr_fl.
*  ENDIF.

*  IF lt_rttrn IS NOT INITIAL.
*    SELECT *
*      FROM /agri/glflot
*      INTO TABLE @lt_flott
*      FOR ALL ENTRIES IN @lt_rtfla                 "#EC CI_NO_TRANSFORM
*      WHERE tplnr_fl = @lt_rtfla-tplnr_fl
*        AND  loevm    = @abap_true.
**               AND tplvl IN ('1','2','5','6').
*    IF sy-subrc = 0.
*      SORT lt_flott BY tplnr_fl.

*      SELECT a~task, b~urole
*          FROM zabst_task_app AS a
*          INNER JOIN zabs_usr_emp AS b
*          ON a~urole = b~urole
*          INTO TABLE @DATA(lt_taskmat)
*          WHERE b~bname = @sy-uname.

  "change -> Login User based validation to User+Employee based valiadtion
  SELECT a~task, b~urole
      FROM zabst_task_app AS a
      INNER JOIN zabs_emp_role AS b
      ON a~urole = b~urole
      INTO TABLE @DATA(lt_taskmat)
      WHERE b~pernr IN @lrt_pernr
        AND a~stapp EQ @abap_true.

  IF sy-subrc NE 0.
    RETURN.
  ELSE.
    DATA lr_matnr TYPE RANGE OF matnr.lr_matnr =
    VALUE #( FOR ls_taskmat IN lt_taskmat (
    sign   = 'I'
    option = 'EQ'
    low    = ls_taskmat-task ) ).
    DELETE lr_matnr WHERE low IS INITIAL.
    SORT lr_matnr BY low.
    DELETE ADJACENT DUPLICATES FROM lr_matnr COMPARING low.

    "BOC - 27.06.2020
    READ TABLE lt_taskmat ASSIGNING FIELD-SYMBOL(<ls_taskmat>) INDEX 1.
    IF <ls_taskmat> IS ASSIGNED.
      lv_urole = <ls_taskmat>-urole.
    ENDIF.
  ENDIF.

*--Get variant table data
*  "BOC - 19.06.2020
*  DATA : lv_low_acdt    TYPE sy-datum,
*         lv_high_acdt   TYPE sy-datum,
*         lv_cnval1_acdt TYPE zabs_del_cnval,
*         lr_actdt       TYPE RANGE OF /agri/fmactdt.
*  CALL METHOD zcl_abs_get_variants=>get_constant_single
*    EXPORTING
*      iv_mod    = 'C'
*      iv_objid  = 'MOBL'
*      iv_k1val  = 'INSPOP'
*      iv_k2val  = 'LACTDT'
*      iv_k3val  = lv_urole
*    IMPORTING
*      ev_cnval1 = lv_cnval1_acdt.
*  CONDENSE lv_cnval1_acdt.
*  lv_low_acdt   = sy-datum - lv_cnval1_acdt.
*  CALL METHOD zcl_abs_get_variants=>get_constant_single
*    EXPORTING
*      iv_mod    = 'C'
*      iv_objid  = 'MOBL'
*      iv_k1val  = 'INSPOP'
*      iv_k2val  = 'HACTDT'
*      iv_k3val  = lv_urole
*    IMPORTING
*      ev_cnval1 = lv_cnval1_acdt.
*  CONDENSE lv_cnval1_acdt.
*  lv_high_acdt  = sy-datum + lv_cnval1_acdt.
*  lr_actdt = VALUE #( sign = 'I' option = 'BT' ( low = lv_low_acdt
*                                                 high = lv_high_acdt ) ).
*   SELECT *
*     FROM /agri/fmfphdr
*     INTO TABLE @DATA(lt_fphdr) "et_entityset
*        FOR ALL ENTRIES IN @lt_rttrn"lt_flott
*        WHERE autyp  = @lc_autyp
*        AND tplnr_fl = @lt_rttrn-tplnr"lt_flott-tplnr_fl
*        AND matnr    IN @lr_matnr
*        AND tecom    = @space.

  "BOC - 03.07.2020
  DATA : lv_low_gstrp    TYPE sy-datum,
         lv_high_gstrp   TYPE sy-datum,
         lv_cnval1_gstrp TYPE zabs_del_cnval,
         ltr_gstrp       TYPE RANGE OF co_gstrp.

  CALL METHOD zcl_abs_get_variants=>get_constant_single
    EXPORTING
      iv_mod    = 'C'
      iv_objid  = 'MOBL'
      iv_k1val  = 'INSPOP'
      iv_k2val  = zcl_abs_abap_maintain=>c_key_lgstrp   "'LGSTRP'
      iv_k3val  = lv_urole
    IMPORTING
      ev_cnval1 = lv_cnval1_gstrp. "'4'
  CONDENSE lv_cnval1_gstrp.
  lv_low_gstrp   = sy-datum - lv_cnval1_gstrp.

  CALL METHOD zcl_abs_get_variants=>get_constant_single
    EXPORTING
      iv_mod    = 'C'
      iv_objid  = 'MOBL'
      iv_k1val  = 'INSPOP'
      iv_k2val  = zcl_abs_abap_maintain=>c_key_hgstrp   "'HGSTRP'
      iv_k3val  = lv_urole
    IMPORTING
      ev_cnval1 = lv_cnval1_gstrp. "'1'
  CONDENSE lv_cnval1_gstrp.
  lv_high_gstrp  = sy-datum + lv_cnval1_gstrp.

  ltr_gstrp = VALUE #( sign = zcl_abs_abap_maintain=>c_rsign_include
                     option = zcl_abs_abap_maintain=>c_ropt_between " 'BT'
                     ( low  = lv_low_gstrp
                       high = lv_high_gstrp ) ).
  "EOC - 03.07.2020

  "BOC - 22.06.2020
  SELECT a~tplnr_fl
    FROM /agri/fmfphdr AS a
   INNER JOIN /agri/fmfpitm AS b
      ON a~aufnr = b~aufnr
    INTO TABLE @DATA(lt_fphdr) "et_entityset
     FOR ALL ENTRIES IN @lt_rttrn"lt_flott
   WHERE a~autyp    = @lc_autyp
     AND a~tplnr_fl = @lt_rttrn-tplnr"lt_flott-tplnr_fl
     AND a~matnr   IN @lr_matnr
     AND a~gstrp   IN @ltr_gstrp "BOC - 03.07.2020
     AND a~tecom    = @space.
*     AND a~cstat   <> @lc_cstat "BOC - 22.06.2020.
*     AND b~actdt   IN @lr_actdt."BOC - 22.06.2020.
  IF sy-subrc = 0.

*        LOOP AT lt_fphdr ASSIGNING FIELD-SYMBOL(<fs_fphdr>).
*
*          READ TABLE lt_rttrn ASSIGNING FIELD-SYMBOL(<fs_rttrn>)
*                              WITH KEY tplnr_fl = <fs_fphdr>-tplnr_fl
*                              BINARY SEARCH.
*          IF sy-subrc = 0.
*            APPEND <fs_rttrn> TO lt_rttrn_tmp.
*          ENDIF.
*
*        ENDLOOP.

  ENDIF.

*    ENDIF.
*  ENDIF.

  SORT: lt_rttrn BY tplnr_fl,
        lt_fphdr BY tplnr_fl.
*  SORT lt_flot BY tplnr_fl.
*  DELETE ADJACENT DUPLICATES FROM lt_flot COMPARING tplnr_fl.
  DELETE ADJACENT DUPLICATES FROM lt_rttrn COMPARING tplnr_fl.
  DELETE ADJACENT DUPLICATES FROM lt_fphdr COMPARING tplnr_fl.
*  SORT lt_flot BY strno.

  SORT: lt_rttrn BY tplnr_fl,
        lt_fphdr BY tplnr_fl.

  LOOP AT lt_rttrn INTO ls_rttrn.


    READ TABLE lt_fphdr ASSIGNING FIELD-SYMBOL(<fs_fphdr>)
    WITH KEY tplnr_fl = ls_rttrn-tplnr
    BINARY SEARCH.
    IF sy-subrc = 0.

*     READ TABLE lt_rttrn ASSIGNING FIELD-SYMBOL(<ls_rttrn>)
*                              WITH KEY tplnr_fl = <fs_fphdr>-tplnr_fl
*                              BINARY SEARCH.
*     IF sy-subrc = 0.
*        ls_entityset-route = <ls_rttrn>-route.
      MOVE-CORRESPONDING ls_rttrn TO ls_entityset.

      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
        EXPORTING
          input  = ls_entityset-tplnr_fl
        IMPORTING
          output = ls_entityset-tplnr_fl.

      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
        EXPORTING
          input          = ls_entityset-msehi
          language       = sy-langu
        IMPORTING
*         LONG_TEXT      =
          output         = ls_entityset-msehi
*         SHORT_TEXT     =
        EXCEPTIONS
          unit_not_found = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = ls_entityset-owner
        IMPORTING
          output = ls_entityset-owner.
*
*      ls_entityset-aedat = ls_rttrn-aedat.
*      ls_entityset-erdat = ls_rttrn-erdat.

      ls_entityset-pernr = lv_persno.
      ls_entityset-lifnr = lv_lifnr.

      CLEAR: ls_entityset-aedat, ls_entityset-erdat.
      APPEND ls_entityset TO lt_entityset.
      CLEAR: ls_entityset.

    ENDIF.
  ENDLOOP.

  lt_orderby = io_tech_request_context->get_orderby( ).
  READ TABLE lt_orderby INTO ls_orderby INDEX 1.
  IF sy-subrc EQ 0 AND ls_orderby-order EQ 'asc' AND
     ls_orderby-property = 'STRNO'.
    SORT lt_entityset BY strno ASCENDING.
    et_entityset = lt_entityset.
  ENDIF.

  IF et_entityset IS INITIAL.
    et_entityset = lt_entityset.
  ENDIF.


*    MOVE-CORRESPONDING ls_rttrn TO ls_entityset.
*
*    READ TABLE lt_rtfla ASSIGNING FIELD-SYMBOL(<ls_rtfla>)
*                        WITH KEY tplnr_fl = ls_rttrn-tplnr_fl
*                        BINARY SEARCH.
*    IF sy-subrc = 0.
*      ls_entityset-route = <ls_rtfla>-route.
*    ENDIF.


*    CALL FUNCTION 'ADDR_CONVERT_DATE_TO_TIMESTAMP'
*      EXPORTING
*        iv_date      = ls_flot-erdat
**       IV_HIGH      = ' '
*      IMPORTING
*        ev_timestamp = lv_tmstmp.
*
*    ls_entityset-erdat = lv_tmstmp.
*
*    CALL FUNCTION 'ADDR_CONVERT_DATE_TO_TIMESTAMP'
*      EXPORTING
*        iv_date      = ls_flot-aedat
**       IV_HIGH      = ' '
*      IMPORTING
*        ev_timestamp = lv_tmstmp.
*
*    ls_entityset-aedat = lv_tmstmp.

*    ls_entityset-pernr = lv_persno.
*    ls_entityset-lifnr = lv_lifnr.
*
*    APPEND ls_entityset TO lt_entityset.
*    CLEAR: ls_entityset.
*  ENDLOOP.

*  lt_orderby = io_tech_request_context->get_orderby( ).
*  READ TABLE lt_orderby INTO ls_orderby INDEX 1.
*  IF sy-subrc EQ 0 AND ls_orderby-order EQ 'asc' AND
*     ls_orderby-property = 'STRNO'.
*    SORT lt_entityset BY strno ASCENDING.
*    et_entityset = lt_entityset.
*  ENDIF.
*
*  IF et_entityset IS INITIAL.
*    et_entityset = lt_entityset.
*  ENDIF.

ENDMETHOD.


METHOD get_wocomponent.
*----------------------------------------------------------------------*
* General Information
*----------------------------------------------------------------------*
* WRICEF ID    : Issue # 16
* Class        : ZCL_ABS_AGRI_MOBILE_E_DPC_EXT
* Method       : WOCOMPONEXT_SET_GET_ENTITYSET
* Company      : Vistex
* Dev. Author  : John Anthony Obregon Lanas
* Create Date  : 16.04.2020
* Description  : New EntitySet created to include component dosage.
*----------------------------------------------------------------------*
* Modification log
*----------------------------------------------------------------------*
* Date         | TR#         | Author          | Modification
*----------------------------------------------------------------------*
* 16.04.2020     C4DK909655    T_A.Harshe        Creation
*----------------------------------------------------------------------*

  TYPES: BEGIN OF ltyp_fpcom,
           aufnr     TYPE /agri/fmfpcom-aufnr,
           posnr     TYPE /agri/fmfpcom-posnr,
           contr     TYPE /agri/fmfpcom-contr,
           vornr     TYPE /agri/fmfpcom-vornr,
           matnr     TYPE /agri/fmfpcom-matnr,
           erfmg     TYPE /agri/fmfpcom-erfmg,
           comng     TYPE /agri/fmcomng,
           erfme     TYPE /agri/fmfpcom-erfme,
           werks     TYPE /agri/fmfpcom-werks,
           lgort     TYPE /agri/fmfpcom-lgort,
           charg     TYPE /agri/fmfpcom-charg,
           rcdos_ins TYPE /agri/fmfpcom-rcdos_ins,
           objnr     TYPE /agri/fmfphdr-objnr,
         END OF ltyp_fpcom.

  TYPES: BEGIN OF lty_strno,
           strno TYPE /agri/glstrno,
         END OF lty_strno.

  CONSTANTS: lc_autyp TYPE /agri/gl_autyp VALUE 'TO',
             lc_cstat TYPE /agri/fmcstat  VALUE 'CNF'.

  DATA : lt_strno     TYPE TABLE OF lty_strno,
*         lt_fpcom     TYPE TABLE OF ltyp_fpcom, "/agri/t_fmfpcom,
         lt_makt      TYPE TABLE OF makt,
         ls_makt      TYPE makt,
         lt_rtfla     TYPE TABLE OF /agri/glrtfla,
*         ls_fpcom     TYPE ltyp_fpcom, "/agri/s_fmfpcom.
         ls_entityset LIKE LINE OF et_entityset,
*         lt_rtusr     TYPE TABLE OF /agri/glrtusr.
         lrt_pernr    TYPE RANGE OF persno,
         lrt_lifnr    TYPE RANGE OF lifnr,
         lr_pernr     LIKE LINE OF lrt_pernr,
         lv_persno    TYPE persno,
         lr_lifnr     LIKE LINE OF lrt_lifnr,
         lv_lifnr     TYPE lifnr,
         lt_rttrn     TYPE zcl_zabs_mob_toc_mpc=>tt_worouteterrain,
         lt_rtusr     TYPE TABLE OF zabs_usrpernr,
         lv_var       TYPE char1,
         lv_urole     TYPE zabs_del_urole.

  DATA : lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
         ls_filter     TYPE /iwbep/s_mgw_select_option,
         lrt_aufnr     TYPE RANGE OF /agri/fmfpnum,
         lrt_vornr     TYPE RANGE OF vornr,
         lt_constants  TYPE zabs_tty_vkey_const,
         lrt_posnr     TYPE RANGE OF posnr,
*  DATA : lrt_wonum     TYPE RANGE OF /agri/fmwonum.
         lt_filter     TYPE /iwbep/t_mgw_select_option,
***  DATA : ls_converted_keys LIKE LINE OF et_entityset.
         lv_filter_str TYPE string.

  lo_filter     = io_tech_request_context->get_filter( ).
  lt_filter     = lo_filter->get_filter_select_options( ).
  lv_filter_str = lo_filter->get_filter_string( ).

  IF  lv_filter_str IS NOT INITIAL
  AND lt_filter[]   IS INITIAL.

    me->/iwbep/if_sb_dpc_comm_services~log_message(
    EXPORTING
      iv_msg_type   = 'E'
      iv_msg_id     = '/IWBEP/MC_SB_DPC_ADM'
      iv_msg_number = 025 ).
    " Raise Exception
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
      EXPORTING
        textid = /iwbep/cx_mgw_tech_exception=>internal_error.

  ENDIF.

***  io_tech_request_context->get_converted_source_keys(
***    IMPORTING
***      es_key_values  = ls_converted_keys ).

* Maps filter table lines to function module parameters
  LOOP AT lt_filter INTO ls_filter.

    CASE ls_filter-property.
      WHEN 'AUFNR'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_aufnr ).
      WHEN 'POSNR'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_posnr ).
      WHEN 'VORNR'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_vornr ).
      WHEN 'PERNR'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_pernr ).
        READ TABLE lrt_pernr INTO lr_pernr INDEX 1.
        IF sy-subrc EQ 0.
          lv_persno = lr_pernr-low.
        ENDIF.
      WHEN 'LIFNR'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_lifnr ).
        READ TABLE lrt_lifnr INTO lr_lifnr INDEX 1.
        IF sy-subrc EQ 0.
          lv_lifnr = lr_lifnr-low.
        ENDIF.
      WHEN OTHERS.
        " Log message in the application log
        me->/iwbep/if_sb_dpc_comm_services~log_message(
          EXPORTING
            iv_msg_type   = 'E'
            iv_msg_id     = '/IWBEP/MC_SB_DPC_ADM'
            iv_msg_number = 020
            iv_msg_v1     = ls_filter-property ).
        " Raise Exception
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
          EXPORTING
            textid = /iwbep/cx_mgw_tech_exception=>internal_error.
    ENDCASE.

  ENDLOOP.

  LOOP AT lrt_aufnr ASSIGNING FIELD-SYMBOL(<fs_aufnr>).
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <fs_aufnr>-low
      IMPORTING
        output = <fs_aufnr>-low.
  ENDLOOP.
*  IF lt_filter IS INITIAL.

*Fetch PERNR/LIFNR based Terrains
  get_route_terrain_dtls(
      EXPORTING
        io_tech_request_context = io_tech_request_context
      IMPORTING
        et_rttrn                = lt_rttrn ).
  IF lt_rttrn[] IS INITIAL.
    RETURN.
  ENDIF.
*  SELECT *
*    FROM /agri/glrtusr
*    INTO TABLE @lt_rtusr
*    WHERE bname = @sy-uname.

*  "change -> Login User based validation to User+Employee based valiadtion
*  IF lrt_pernr IS NOT INITIAL.
*    SELECT *
*    FROM zabs_usrpernr
*    INTO TABLE lt_rtusr
*    WHERE pernr IN lrt_pernr.
*  ELSEIF lrt_lifnr IS NOT INITIAL.
*    SELECT *
*    FROM zabs_usrpernr
*    INTO TABLE lt_rtusr
*    WHERE lifnr IN lrt_lifnr.
*  ENDIF.
*
*  IF lt_rtusr IS NOT INITIAL.
*    SELECT *
*      FROM /agri/glrtfla
*      INTO TABLE @lt_rtfla
*      FOR ALL ENTRIES IN @lt_rtusr                 "#EC CI_NO_TRANSFORM
*      WHERE route = @lt_rtusr-route.
*  ENDIF.
  "BOC - 21.06.2020
*--Fetch Login User based - Task Material maitained in the System
  SELECT a~task, a~vornr, a~stapp, b~urole
  FROM zabst_task_app AS a
  INNER JOIN zabs_emp_role AS b
  ON a~urole = b~urole
  INTO TABLE @DATA(lt_task_app)
    WHERE b~pernr IN @lrt_pernr
      AND a~stapp EQ @abap_true.
  IF sy-subrc NE 0.
*- If user role is not having any Task maintained - then dont show any task orders
    RETURN.
  ENDIF.
  DATA lr_matnr TYPE RANGE OF matnr.
  lr_matnr =  VALUE #( FOR ls_task_app IN lt_task_app (
   sign   = 'I'
   option = 'EQ'
   low    = ls_task_app-task ) ).
  DELETE lr_matnr WHERE low IS INITIAL.
  SORT lr_matnr BY low.
  DELETE ADJACENT DUPLICATES FROM lr_matnr COMPARING low.

  "BOC - 27.06.2020
  READ TABLE lt_task_app ASSIGNING FIELD-SYMBOL(<ls_task_app>) INDEX 1.
  IF <ls_task_app> IS ASSIGNED.
    lv_urole = <ls_task_app>-urole.
  ENDIF.
*--Get variant table data
*  CALL METHOD zcl_abs_get_variants=>get_constant_multiple
*    EXPORTING
*      iv_mod       = 'C'
*      iv_objid     = 'MOBL'
*      iv_k1val     = 'INSPOP'
*      iv_k2val     = 'STEUS'
*    IMPORTING
*      et_constants = lt_constants.
*
*  DATA lr_steus TYPE RANGE OF steus.
*  lr_steus =  VALUE #( FOR ls_constant IN lt_constants (
*  sign   = 'I'
*  option = 'EQ'
*  low    = ls_constant-cnval1 ) ).
*  DELETE lr_steus WHERE low IS INITIAL.
*  SORT lr_steus BY low.
*  DELETE ADJACENT DUPLICATES FROM lr_steus COMPARING low.

*--Get variant table data
*  "BOC - 19.06.2020
*  DATA : lv_low_acdt    TYPE sy-datum,
*         lv_high_acdt   TYPE sy-datum,
*         lv_cnval1_acdt TYPE zabs_del_cnval,
*         lr_actdt       TYPE RANGE OF /agri/fmactdt.
*  CALL METHOD zcl_abs_get_variants=>get_constant_single
*    EXPORTING
*      iv_mod    = 'C'
*      iv_objid  = 'MOBL'
*      iv_k1val  = 'INSPOP'
*      iv_k2val  = 'LACTDT'
*      iv_k3val  = lv_urole
*    IMPORTING
*      ev_cnval1 = lv_cnval1_acdt.
*  CONDENSE lv_cnval1_acdt.
*  lv_low_acdt   = sy-datum - lv_cnval1_acdt.
*  CALL METHOD zcl_abs_get_variants=>get_constant_single
*    EXPORTING
*      iv_mod    = 'C'
*      iv_objid  = 'MOBL'
*      iv_k1val  = 'INSPOP'
*      iv_k2val  = 'HACTDT'
*      iv_k3val  = lv_urole
*    IMPORTING
*      ev_cnval1 = lv_cnval1_acdt.
*  CONDENSE lv_cnval1_acdt.
*  lv_high_acdt  = sy-datum + lv_cnval1_acdt.
*  lr_actdt = VALUE #( sign = 'I' option = 'BT' ( low = lv_low_acdt
*                                                 high = lv_high_acdt ) ).

  "BOC - 03.07.2020
  DATA : lv_low_gstrp    TYPE sy-datum,
         lv_high_gstrp   TYPE sy-datum,
         lv_cnval1_gstrp TYPE zabs_del_cnval,
         ltr_gstrp       TYPE RANGE OF co_gstrp.

  CALL METHOD zcl_abs_get_variants=>get_constant_single
    EXPORTING
      iv_mod    = 'C'
      iv_objid  = 'MOBL'
      iv_k1val  = 'INSPOP'
      iv_k2val  = zcl_abs_abap_maintain=>c_key_lgstrp   "'LGSTRP'
      iv_k3val  = lv_urole
    IMPORTING
      ev_cnval1 = lv_cnval1_gstrp. "'4'
  CONDENSE lv_cnval1_gstrp.
  lv_low_gstrp   = sy-datum - lv_cnval1_gstrp.

  CALL METHOD zcl_abs_get_variants=>get_constant_single
    EXPORTING
      iv_mod    = 'C'
      iv_objid  = 'MOBL'
      iv_k1val  = 'INSPOP'
      iv_k2val  = zcl_abs_abap_maintain=>c_key_hgstrp   "'HGSTRP'
      iv_k3val  = lv_urole
    IMPORTING
      ev_cnval1 = lv_cnval1_gstrp. "'1'
  CONDENSE lv_cnval1_gstrp.
  lv_high_gstrp  = sy-datum + lv_cnval1_gstrp.

  ltr_gstrp = VALUE #( sign = zcl_abs_abap_maintain=>c_rsign_include
                     option = zcl_abs_abap_maintain=>c_ropt_between " 'BT'
                     ( low  = lv_low_gstrp
                       high = lv_high_gstrp ) ).
  "EOC - 03.07.2020

*  IF lt_rtfla IS NOT INITIAL.

  SELECT a~aufnr,
         a~tplnr_fl
    FROM /agri/fmfphdr AS a
    INNER JOIN /agri/fmfpitm AS b
      ON a~aufnr = b~aufnr
    INTO TABLE @DATA(lt_fmfphdr)
     FOR ALL ENTRIES IN @lt_rttrn
   WHERE a~aufnr    IN @lrt_aufnr
     AND a~autyp     = @lc_autyp
     AND a~tplnr_fl  = @lt_rttrn-tplnr
     AND a~matnr    IN @lr_matnr
     AND a~gstrp    IN @ltr_gstrp "BOC - 03.07.2020
     AND a~tecom     = @space.
*       AND b~cstat   <> @lc_cstat
*       AND b~actdt    IN @lr_actdt.
*       AND b~steus    IN @lr_steus
  IF lt_fmfphdr IS NOT INITIAL.
    SELECT aufnr,
           posnr,
           contr,
           vornr,
           matnr,
           erfmg,
           comng,
           erfme,
           werks,
           lgort,
           charg,
           rcdos_ins
    FROM /agri/fmfpcom
    INTO TABLE @DATA(lt_fpcom)
     FOR ALL ENTRIES IN @lt_fmfphdr                "#EC CI_NO_TRANSFORM
   WHERE aufnr    = @lt_fmfphdr-aufnr"lrt_aufnr
     AND posnr    IN @lrt_posnr
     AND vornr    IN @lrt_vornr.
  ENDIF.
*    SELECT  a~aufnr,
*            a~posnr,
*            a~contr,
*            a~vornr,
*            a~matnr,
*            a~erfmg,
*            a~comng,
*            a~erfme,
*            a~werks,
*            a~lgort,
*            a~charg,
*            a~rcdos_ins,
*            b~objnr
*     FROM /agri/fmfpcom AS a
*      INNER JOIN /agri/fmfphdr AS b
*     ON a~aufnr EQ b~aufnr
*            INTO TABLE @lt_fpcom
*     FOR ALL ENTRIES IN @lt_rttrn                  "#EC CI_NO_TRANSFORM
*            WHERE b~aufnr    IN @lrt_aufnr
*              AND b~autyp    = @lc_autyp " 'TO'
*              AND b~tecom    <> @abap_true
*              AND a~posnr    IN @lrt_posnr
*              AND a~vornr    IN @lrt_vornr
*              AND b~tplnr_fl = @lt_rttrn-tplnr"@lt_rtfla-tplnr_fl
*              AND b~matnr      IN @lr_matnr
*              AND b~cstat    NE 'CNF'.
*             AND vornr IN lrt_vornr.
*    ENDIF.
*  ELSE.
*
*    SELECT a~aufnr,
*           a~posnr,
*           a~contr,
*           a~vornr,
*           a~matnr,
*           a~erfmg,
*           a~comng,
*           a~erfme,
*           a~werks,
*           a~lgort,
*           a~charg,
*           a~rcdos_ins,
*           b~objnr
*           FROM /agri/fmfpcom AS a
*           INNER JOIN /agri/fmfphdr AS b
*           ON a~aufnr = b~aufnr
*           INTO TABLE @lt_fpcom
*           WHERE b~aufnr IN @lrt_aufnr
*            AND b~autyp   = @lc_autyp
*            AND b~tecom  <> @abap_true
*            AND a~posnr  IN @lrt_posnr
*            AND a~vornr  IN @lrt_vornr
*            AND b~cstat  NE 'CNF'.
*  ENDIF.

*LOOP AT lt_fpcom ASSIGNING FIELD-SYMBOL(<fs_fpcom>).
*  CLEAR :lv_var.
*  CALL METHOD me->order_status
*    EXPORTING
*      iv_objnr  = <fs_fpcom>-objnr
*      iv_source = ZCL_ZABS_AGRI_MOBILE_E_DPC_EXT=>c_source_taskorder"'TC'
*    IMPORTING
*      ev_var    = lv_var.
*  IF lv_var EQ 'X'.
*    DELETE lt_fpcom INDEX sy-tabix.
*  ELSEIF lv_var NE 'X'.
*    CONTINUE.
*  ENDIF.
*ENDLOOP.
  IF lt_fpcom[] IS NOT INITIAL.
    SELECT * FROM makt
      INTO TABLE @lt_makt
      FOR ALL ENTRIES IN @lt_fpcom                 "#EC CI_NO_TRANSFORM
      WHERE matnr = @lt_fpcom-matnr
       AND spras  = @sy-langu.
    IF sy-subrc = 0.
      SORT lt_makt BY matnr.
    ENDIF.
*-- Batch indicator Data
    SELECT matnr,werks,xchar,xchpf
       FROM marc
       INTO TABLE @DATA(lt_marc)
       FOR ALL ENTRIES IN @lt_fpcom
       WHERE matnr EQ @lt_fpcom-matnr
         AND werks EQ @lt_fpcom-werks
         AND lvorm EQ @space.
    IF sy-subrc = 0.
      SORT: lt_marc BY matnr werks.
    ENDIF.

*-- Begin of change: JOBREGON
*-- Based on the task order number, we fetch all dosages available. AUFNR> ZFMPROG_TALHAO> ZFMRCLST
*    DATA lr_aufnr TYPE RANGE OF aufnr.lr_aufnr =
*    VALUE #( FOR ls_fpcomt IN lt_fpcom (
*    sign   = 'I'
*    option = 'EQ'
*    low    = ls_fpcomt-aufnr ) ).
*
*    DELETE lr_aufnr WHERE low IS INITIAL.
*    SORT lr_aufnr BY low.
*    DELETE ADJACENT DUPLICATES FROM lr_aufnr COMPARING low.
*
*    SELECT aufnr, acnum, rcnum, tplnr_fl
*      FROM zfmprog_talhao
*      INTO TABLE @DATA(lt_talhao)
*      WHERE aufnr IN @lr_aufnr.
*    IF sy-subrc = 0.
*
*      SORT lt_talhao BY aufnr.
*
*      DATA lr_rcnum TYPE RANGE OF zfmrcnum.lr_rcnum =
*      VALUE #( FOR ls_talhaot IN lt_talhao (
*      sign   = 'I'
*      option = 'EQ'
*      low    = ls_talhaot-rcnum ) ).
*      DELETE lr_rcnum WHERE low IS INITIAL.
*      SORT lr_rcnum BY low.
*      DELETE ADJACENT DUPLICATES FROM lr_rcnum COMPARING low.
*
*      IF lr_rcnum[] IS NOT INITIAL.
*        DATA lr_matnr TYPE RANGE OF matnr.lr_matnr =
*        VALUE #( FOR ls_fpcomt IN lt_fpcom (
*        sign   = 'I'
*        option = 'EQ'
*        low    = ls_fpcomt-matnr ) ).
*        DELETE lr_matnr WHERE low IS INITIAL.
*        SORT lr_matnr BY low.
*        DELETE ADJACENT DUPLICATES FROM lr_matnr COMPARING low.
*
*        DATA lr_werks TYPE RANGE OF werks_d.lr_werks =
*        VALUE #( FOR ls_fpcomt IN lt_fpcom (
*        sign   = 'I'
*        option = 'EQ'
*        low    = ls_fpcomt-werks ) ).
*        DELETE lr_werks WHERE low IS INITIAL.
*        SORT lr_werks BY low.
*        DELETE ADJACENT DUPLICATES FROM lr_werks COMPARING low.
*
*        SELECT *
*          FROM zfmrclst
*          INTO TABLE @DATA(lt_fmrclst)
*          WHERE rcnum     IN @lr_rcnum
*            AND matnr_ins IN @lr_matnr
*            AND werks     IN @lr_werks.
*        SORT lt_fmrclst BY rcnum matnr_ins.
*      ENDIF.
*    ENDIF.
*-- End of change: JOBREGON

  ENDIF.

  LOOP AT lt_fpcom INTO DATA(ls_fpcom).
    MOVE-CORRESPONDING ls_fpcom TO ls_entityset.      "#EC CI_FLDEXT_OK

    " Begin of change - 12.06.20, End of change - 12.06.20, Incident No- INC0015840.
    " rcdos directly fetched from fpcom.
    ls_entityset-rcdos = ls_fpcom-rcdos_ins.

    CLEAR ls_makt.
    IF ls_fpcom-matnr IS INITIAL.
      CONTINUE.
    ENDIF.
    READ TABLE lt_makt INTO ls_makt WITH KEY matnr = ls_fpcom-matnr
                                    BINARY SEARCH.
    IF sy-subrc = 0.
      ls_entityset-maktx = ls_makt-maktx.
    ENDIF.
*-- Check Batch indicator
    READ TABLE lt_marc INTO DATA(ls_marc)
         WITH KEY matnr = ls_fpcom-matnr
                  werks = ls_fpcom-werks BINARY SEARCH.
    IF sy-subrc = 0.
      ls_entityset-flgch = ls_marc-xchpf.
    ENDIF.

*code commented because of rcdos fetched directly from fmfpcom.
*-- Begin of change: JOBREGON
*-- We read the corresponding tables to validate if dosage data is availabale
*    READ TABLE lt_talhao ASSIGNING FIELD-SYMBOL(<fs_talhao>)
*                         WITH KEY aufnr = ls_fpcom-aufnr
*                         BINARY SEARCH.
*    IF sy-subrc = 0.
*      READ TABLE lt_fmrclst ASSIGNING FIELD-SYMBOL(<fs_fmrclst>)
*                            WITH KEY rcnum     = <fs_talhao>-rcnum
*                                     matnr_ins = ls_fpcom-matnr
*                            BINARY SEARCH.
*      IF sy-subrc = 0.
*        ls_entityset-rcdos = <fs_fmrclst>-rcdos.
*      ENDIF.
*    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = ls_entityset-matnr
      IMPORTING
        output = ls_entityset-matnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = ls_entityset-aufnr
      IMPORTING
        output = ls_entityset-aufnr.

*-- End of change: JOBREGON

    ls_entityset-pernr = lv_persno.
    ls_entityset-lifnr = lv_lifnr.

    APPEND ls_entityset TO et_entityset.
    CLEAR: ls_entityset,ls_marc.
  ENDLOOP.

ENDMETHOD.


METHOD get_womaterial.
*----------------------------------------------------------------------*
* General Information
*----------------------------------------------------------------------*
* WRICEF ID    : Issue # 14
* Class        : ZCL_ABS_AGRI_MOBILE_E_DPC_EXT
* Method       : WOMATERIAL_EXT_GET_ENTITYSET
* Company      : Vistex
* Dev. Author  : John Anthony Obregon Lanas
* Create Date  : 05.03.2020
* Description  : New Entity Set that enhances logic to assign TANK flag
*                so the Front End knows which materials are set for
*                Tank Confirmation,
*----------------------------------------------------------------------*
* Modification log
*----------------------------------------------------------------------*
* Date         | TR#         | Author          | Modification
*----------------------------------------------------------------------*
* 05.03.2020     C4DK909655    T_A.Harshe        Creation
*----------------------------------------------------------------------*

  DATA: lt_mara       TYPE TABLE OF mara,
        ls_mara       TYPE mara,
        lt_makt       TYPE TABLE OF makt,
        ls_makt       TYPE makt,
        ls_key_tab    TYPE /iwbep/s_mgw_name_value_pair,
        lv_tplnr_fl   TYPE /agri/gltplnr_fl,
        lr_tplnr_fl   TYPE RANGE OF /agri/gltplnr_fl,
        lv_tflag      TYPE flag,
        lr_tflag      TYPE RANGE OF flag,
        lr_ausme      TYPE RANGE OF ausme,
        lr_ausmeb     TYPE RANGE OF ausme,
        lrt_pernr     TYPE RANGE OF persno,
        lrt_lifnr     TYPE RANGE OF lifnr,
        lr_lifnr      LIKE LINE OF lrt_lifnr,
        lv_lifnr      TYPE lifnr,
        lr_pernr      LIKE LINE OF lrt_pernr,
        lv_persno     TYPE persno,
        ls_ausmer     LIKE LINE OF lr_ausme,
        lwa_tplnr_fl  LIKE LINE OF lr_tplnr_fl,
        lwa_tflag     LIKE LINE OF lr_tflag,
        lv_date       TYPE co_gstrp,
        lr_date       TYPE RANGE OF co_gstrp,
        lt_rtfla      TYPE TABLE OF /agri/glrtfla,
        lt_rttrn      TYPE zcl_zabs_mob_toc_mpc=>tt_worouteterrain,
        lt_filter     TYPE /iwbep/t_mgw_select_option,
        lwa_date      LIKE LINE OF lr_date,
        lt_constants  TYPE zabs_tty_vkey_const,
        lv_filter_str TYPE string,
        lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
        ls_filter     TYPE /iwbep/s_mgw_select_option,
        ls_entityset  LIKE LINE OF et_entityset,
*        lt_rtusr      TYPE TABLE OF /agri/glrtusr,
        lt_rtusr      TYPE TABLE OF zabs_usrpernr,
        lt_tank_uom   TYPE zabs_tty_vkey_const,
        lv_var        TYPE char1,
        lv_urole      TYPE zabs_del_urole.

*  DATA: BEGIN OF lwa_matnr,
*          tplnr_fl TYPE /agri/gltplnr_fl,
*          matnr    TYPE /agri/glmatnr,
*          iwerk    TYPE werks_d,
*          objnr    TYPE j_objnr,
*        END OF lwa_matnr,
*        lt_matnr LIKE TABLE OF lwa_matnr.

  DATA: BEGIN OF lwa_marc,
          matnr TYPE /agri/glmatnr,
          werks TYPE werks,
          ausme TYPE ausme,
        END OF lwa_marc.
  CONSTANTS: lc_autyp TYPE /agri/gl_autyp VALUE 'TO',
             lc_astat TYPE /agri/glastat  VALUE 'A',
             lc_cstat TYPE /agri/fmcstat  VALUE 'CNF'.

  lo_filter     = io_tech_request_context->get_filter( ).
  lt_filter     = lo_filter->get_filter_select_options( ).
  lv_filter_str = lo_filter->get_filter_string( ).

  IF  lv_filter_str IS NOT INITIAL
  AND lt_filter[]   IS INITIAL.

    me->/iwbep/if_sb_dpc_comm_services~log_message(
    EXPORTING
      iv_msg_type   = 'E'
      iv_msg_id     = '/IWBEP/MC_SB_DPC_ADM'
      iv_msg_number = 025 ).
    " Raise Exception
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
      EXPORTING
        textid = /iwbep/cx_mgw_tech_exception=>internal_error.
  ENDIF.

* Maps filter table lines to function module parameters
  LOOP AT lt_filter INTO ls_filter.
    CASE ls_filter-property.
      WHEN 'TPLNR_FL'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lr_tplnr_fl ).
        READ TABLE lr_tplnr_fl INTO lwa_tplnr_fl INDEX 1.
        lv_tplnr_fl = lwa_tplnr_fl-low.
*        WHEN 'TFLAG'.
*          lo_filter->convert_select_option(
*          EXPORTING
*            is_select_option = ls_filter
*          IMPORTING
*            et_select_option = lr_tflag ).
*          READ TABLE lr_tflag INTO lwa_tflag INDEX 1.
*          lv_tflag = lwa_tflag-low.
      WHEN 'PERNR'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_pernr ).
        READ TABLE lrt_pernr INTO lr_pernr INDEX 1.
        IF sy-subrc EQ 0.
          lv_persno = lr_pernr-low.
        ENDIF.
      WHEN 'LIFNR'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_lifnr ).
        READ TABLE lrt_lifnr INTO lr_lifnr INDEX 1.
        IF sy-subrc EQ 0.
          lv_lifnr = lr_lifnr-low.
        ENDIF.
      WHEN OTHERS.
        " Log message in the application log
        me->/iwbep/if_sb_dpc_comm_services~log_message(
          EXPORTING
            iv_msg_type   = 'E'
            iv_msg_id     = '/IWBEP/MC_SB_DPC_ADM'
            iv_msg_number = 020
            iv_msg_v1     = ls_filter-property ).
        " Raise Exception
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
          EXPORTING
            textid = /iwbep/cx_mgw_tech_exception=>internal_error.
    ENDCASE.
  ENDLOOP.

*-- JOBREGON: Start of modification
  CALL METHOD zcl_abs_get_variants=>get_constant_multiple
    EXPORTING
      iv_mod       = abap_true
      iv_objid     = zcl_abs_odata_maintain=>c_mobl_objid
      iv_k1val     = zcl_abs_odata_maintain=>c_mobl_val
      iv_k2val     = zcl_abs_odata_maintain=>c_ctry
      iv_k3val     = zcl_abs_odata_maintain=>c_tank_uom
    IMPORTING
      et_constants = lt_tank_uom.

*-- Create range to validate Unit for Tank Confirmation
  ls_ausmer-option = 'EQ'.
  ls_ausmer-sign   = 'I'.

  LOOP AT lt_tank_uom ASSIGNING FIELD-SYMBOL(<fs_tank_uom>).

    ls_ausmer-low    = <fs_tank_uom>-cnval1.
    APPEND ls_ausmer TO lr_ausme.

  ENDLOOP.

  REFRESH lt_tank_uom.
  CALL METHOD zcl_abs_get_variants=>get_constant_multiple
    EXPORTING
      iv_mod       = abap_true
      iv_objid     = zcl_abs_odata_maintain=>c_mobl_objid
      iv_k1val     = zcl_abs_odata_maintain=>c_mobl_val
      iv_k2val     = zcl_abs_odata_maintain=>c_ctry
      iv_k3val     = zcl_abs_odata_maintain=>c_bags_uom
    IMPORTING
      et_constants = lt_tank_uom.

*-- Create range to validate Unit for Tank Confirmation
  ls_ausmer-option = 'EQ'.
  ls_ausmer-sign   = 'I'.

  LOOP AT lt_tank_uom ASSIGNING <fs_tank_uom>.

    ls_ausmer-low    = <fs_tank_uom>-cnval1.
    APPEND ls_ausmer TO lr_ausmeb.

  ENDLOOP.

*-- JOBREGON: End of modification

  IF lv_tplnr_fl IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
      EXPORTING
        input      = lv_tplnr_fl
      IMPORTING
        output     = lv_tplnr_fl
      EXCEPTIONS ##FM_SUBRC_OK
        not_found  = 1
        not_active = 2
        OTHERS     = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  ENDIF.

*  SELECT * FROM /agri/glrtusr
*         INTO TABLE @lt_rtusr
*         WHERE bname = @sy-uname.

  "change -> Login User based validation to User+Employee based valiadtion
*  IF lrt_pernr IS NOT INITIAL.
*    SELECT *
*    FROM zabs_usrpernr
*    INTO TABLE lt_rtusr
*    WHERE pernr IN lrt_pernr.
*  ELSEIF lrt_lifnr IS NOT INITIAL.
*    SELECT *
*    FROM zabs_usrpernr
*    INTO TABLE lt_rtusr
*    WHERE lifnr IN lrt_lifnr.
*  ENDIF.
*
*  IF lt_rtusr IS NOT INITIAL AND
*     lv_tplnr_fl IS NOT INITIAL.
*    SELECT * FROM /agri/glrtfla
*             INTO TABLE @lt_rtfla
*             FOR ALL ENTRIES IN @lt_rtusr          "#EC CI_NO_TRANSFORM
*             WHERE route = @lt_rtusr-route
*               AND tplnr_fl = @lv_tplnr_fl.
*  ELSEIF lt_rtusr IS NOT INITIAL.
*    SELECT * FROM /agri/glrtfla
*            INTO TABLE @lt_rtfla
*            FOR ALL ENTRIES IN @lt_rtusr           "#EC CI_NO_TRANSFORM
*            WHERE route EQ @lt_rtusr-route.
*  ENDIF.

*Fetch PERNR/LIFNR based Terrains
  get_route_terrain_dtls(
      EXPORTING
        io_tech_request_context = io_tech_request_context
      IMPORTING
        et_rttrn                = lt_rttrn ).
  IF lt_rttrn[] IS INITIAL.
    RETURN.
  ENDIF.

*--Fetch Login User based - Task Material maitained in the System
  SELECT a~task, a~vornr, a~stapp, b~urole
  FROM zabst_task_app AS a
  INNER JOIN zabs_emp_role AS b
  ON a~urole = b~urole
  INTO TABLE @DATA(lt_task_app)
    WHERE b~pernr IN @lrt_pernr
      AND a~stapp EQ @abap_true.
  IF sy-subrc NE 0.
*- If user role is not having any Task maintained - then dont show any task orders
    RETURN.
  ENDIF.
  DATA lr_matnr TYPE RANGE OF matnr.
  lr_matnr =  VALUE #( FOR ls_task_app IN lt_task_app (
   sign   = 'I'
   option = 'EQ'
   low    = ls_task_app-task ) ).
  DELETE lr_matnr WHERE low IS INITIAL.
  SORT lr_matnr BY low.
  DELETE ADJACENT DUPLICATES FROM lr_matnr COMPARING low.

  "BOC - 27.06.2020
  READ TABLE lt_task_app ASSIGNING FIELD-SYMBOL(<ls_task_app>) INDEX 1.
  IF <ls_task_app> IS ASSIGNED.
    lv_urole = <ls_task_app>-urole.
  ENDIF.
**  IF lt_rttrn IS NOT INITIAL.
*    SELECT tplnr_fl
*           matnr
*           iwerk
*           objnr
*    INTO TABLE lt_matnr
*    FROM /agri/fmfphdr
**      FOR ALL ENTRIES IN lt_rtfla                  "#EC CI_NO_TRANSFORM
*      FOR ALL ENTRIES IN lt_rttrn                  "#EC CI_NO_TRANSFORM
*    WHERE tplnr_fl = lt_rttrn-tplnr "lt_rtfla-tplnr_fl
*      AND autyp    = lc_autyp "autyp = 'TO'    "Task Order
**        AND class = '1'     "Farming
*      AND matnr    IN lr_matnr
*      AND tecom    EQ space.

*--Get variant table data
*  CALL METHOD zcl_abs_get_variants=>get_constant_multiple
*    EXPORTING
*      iv_mod       = 'C'
*      iv_objid     = 'MOBL'
*      iv_k1val     = 'INSPOP'
*      iv_k2val     = 'STEUS'
*    IMPORTING
*      et_constants = lt_constants.
*
*  DATA lr_steus TYPE RANGE OF steus.
*  lr_steus =  VALUE #( FOR ls_constant IN lt_constants (
*  sign   = 'I'
*  option = 'EQ'
*  low    = ls_constant-cnval1 ) ).
*  DELETE lr_steus WHERE low IS INITIAL.
*  SORT lr_steus BY low.
*  DELETE ADJACENT DUPLICATES FROM lr_steus COMPARING low.

***--Get variant table data
*  "BOC - 19.06.2020
*  DATA : lv_low_acdt    TYPE sy-datum,
*         lv_high_acdt   TYPE sy-datum,
*         lv_cnval1_acdt TYPE zabs_del_cnval,
*         lr_actdt       TYPE RANGE OF /agri/fmactdt.
*  CALL METHOD zcl_abs_get_variants=>get_constant_single
*    EXPORTING
*      iv_mod    = 'C'
*      iv_objid  = 'MOBL'
*      iv_k1val  = 'INSPOP'
*      iv_k2val  = 'LACTDT'
*      iv_k3val  = lv_urole
*    IMPORTING
*      ev_cnval1 = lv_cnval1_acdt.
*  CONDENSE lv_cnval1_acdt.
*  lv_low_acdt   = sy-datum - lv_cnval1_acdt.
*  CALL METHOD zcl_abs_get_variants=>get_constant_single
*    EXPORTING
*      iv_mod    = 'C'
*      iv_objid  = 'MOBL'
*      iv_k1val  = 'INSPOP'
*      iv_k2val  = 'HACTDT'
*      iv_k3val  = lv_urole
*    IMPORTING
*      ev_cnval1 = lv_cnval1_acdt.
*  CONDENSE lv_cnval1_acdt.
*  lv_high_acdt  = sy-datum + lv_cnval1_acdt.
*  lr_actdt = VALUE #( sign = 'I' option = 'BT' ( low = lv_low_acdt
*                                                 high = lv_high_acdt ) ).

  "BOC - 03.07.2020
  DATA : lv_low_gstrp    TYPE sy-datum,
         lv_high_gstrp   TYPE sy-datum,
         lv_cnval1_gstrp TYPE zabs_del_cnval,
         ltr_gstrp       TYPE RANGE OF co_gstrp.

  CALL METHOD zcl_abs_get_variants=>get_constant_single
    EXPORTING
      iv_mod    = 'C'
      iv_objid  = 'MOBL'
      iv_k1val  = 'INSPOP'
      iv_k2val  = zcl_abs_abap_maintain=>c_key_lgstrp   "'LGSTRP'
      iv_k3val  = lv_urole
    IMPORTING
      ev_cnval1 = lv_cnval1_gstrp. "'4'
  CONDENSE lv_cnval1_gstrp.
  lv_low_gstrp   = sy-datum - lv_cnval1_gstrp.

  CALL METHOD zcl_abs_get_variants=>get_constant_single
    EXPORTING
      iv_mod    = 'C'
      iv_objid  = 'MOBL'
      iv_k1val  = 'INSPOP'
      iv_k2val  = zcl_abs_abap_maintain=>c_key_hgstrp   "'HGSTRP'
      iv_k3val  = lv_urole
    IMPORTING
      ev_cnval1 = lv_cnval1_gstrp. "'1'
  CONDENSE lv_cnval1_gstrp.
  lv_high_gstrp  = sy-datum + lv_cnval1_gstrp.

  ltr_gstrp = VALUE #( sign = zcl_abs_abap_maintain=>c_rsign_include
                     option = zcl_abs_abap_maintain=>c_ropt_between " 'BT'
                     ( low  = lv_low_gstrp
                       high = lv_high_gstrp ) ).
  "EOC - 03.07.2020

  SELECT a~tplnr_fl,
         a~matnr,
         a~iwerk,
         a~objnr
    FROM /agri/fmfphdr AS a
    INNER JOIN /agri/fmfpitm AS b
      ON a~aufnr = b~aufnr
    INTO TABLE @DATA(lt_matnr)
     FOR ALL ENTRIES IN @lt_rttrn
   WHERE a~autyp     = @lc_autyp
     AND a~tplnr_fl  = @lt_rttrn-tplnr
     AND a~matnr    IN @lr_matnr
     AND a~gstrp    IN @ltr_gstrp "BOC - 03.07.2020
     AND a~tecom     = @space.
*     AND b~cstat    <> @lc_cstat
*     AND b~actdt    IN @lr_actdt.
*     AND b~steus    IN @lr_steus
*    LOOP AT lt_matnr ASSIGNING FIELD-SYMBOL(<fs_matnr>).
*      CLEAR :lv_var.
*      CALL METHOD me->order_status
*        EXPORTING
*          iv_objnr  = <fs_matnr>-objnr
*          iv_source = zcl_zabs_agri_mobile_e_dpc_ext=>c_source_taskorder "'TC'
*        IMPORTING
*          ev_var    = lv_var.
*      IF lv_var EQ 'X'.
*        DELETE lt_matnr INDEX sy-tabix.
*      ELSEIF lv_var NE 'X'.
*        CONTINUE.
*      ENDIF.
*    ENDLOOP.

*  ENDIF.
*endif.
  SORT lt_matnr BY tplnr_fl matnr.
  DELETE ADJACENT DUPLICATES FROM lt_matnr COMPARING tplnr_fl matnr.
*  ELSE.

  IF lt_matnr IS NOT INITIAL.
    SELECT * FROM makt
             INTO TABLE @lt_makt
             FOR ALL ENTRIES IN @lt_matnr          "#EC CI_NO_TRANSFORM
             WHERE matnr EQ @lt_matnr-matnr
               AND spras EQ @sy-langu.
    IF sy-subrc EQ 0.
      SORT lt_makt BY matnr.
    ENDIF.

*-- JOBREGON: Start of modification
    SELECT matnr, werks, ausme
      FROM marc
      INTO TABLE @DATA(lt_marc)
      FOR ALL ENTRIES IN @lt_matnr
      WHERE matnr = @lt_matnr-matnr
        AND werks = @lt_matnr-iwerk.
    IF sy-subrc = 0.
      SORT lt_marc BY matnr werks.
    ENDIF.
*-- JOBREGON: End of modification

*    SELECT a~task, a~vornr, a~stapp, b~urole
*    FROM zabst_task_app AS a
*    INNER JOIN zabs_usr_emp AS b
*    ON a~urole = b~urole
*    INTO TABLE @DATA(lt_task_app)
*    FOR ALL ENTRIES IN @lt_matnr
*      WHERE a~task = @lt_matnr-matnr
*      AND b~bname  = @sy-uname.

    "change -> Login User based validation to User+Employee based valiadtion
*    SELECT a~task, a~vornr, a~stapp, b~urole
*    FROM zabst_task_app AS a
*    INNER JOIN zabs_emp_role AS b
*    ON a~urole = b~urole
*    INTO TABLE @DATA(lt_task_app)
*    FOR ALL ENTRIES IN @lt_matnr
*      WHERE a~task = @lt_matnr-matnr
*      AND b~pernr IN @lrt_pernr.


    IF sy-subrc = 0.
      SORT lt_task_app BY task.
    ENDIF.

    SELECT *
      FROM zabs_task_bags
      INTO TABLE @DATA(lt_taskbags)
      FOR ALL ENTRIES IN @lt_matnr
      WHERE task = @lt_matnr-matnr.
    SORT lt_taskbags BY task.
  ENDIF.

  LOOP AT lt_matnr INTO DATA(lwa_matnr).
    ls_entityset-matnr = lwa_matnr-matnr.             "#EC CI_FLDEXT_OK
    ls_entityset-tplnr_fl = lwa_matnr-tplnr_fl.
    CLEAR: ls_makt.
    READ TABLE lt_makt INTO ls_makt
      WITH KEY matnr = ls_entityset-matnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_entityset-maktx = ls_makt-maktx.
    ENDIF.

*    READ TABLE lt_task_app INTO data(ls_task) WITH KEY task = lwa_matnr-matnr
*                                                  BINARY SEARCH.
*    IF sy-subrc NE 0.
*      CONTINUE.
*    ELSE.
*      ls_entityset-defvornr = ls_task-vornr.
*      IF ls_task_app-stapp <> abap_true.
*        CONTINUE.
*      ENDIF.
*    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
      EXPORTING
        input  = ls_entityset-tplnr_fl
      IMPORTING
        output = ls_entityset-tplnr_fl.

*-- JOBREGON: Start of modification
    READ TABLE lt_marc INTO lwa_marc
                       WITH KEY matnr = ls_entityset-matnr
                                werks = lwa_matnr-iwerk
                       BINARY SEARCH.
    IF sy-subrc = 0.
      IF lwa_marc-ausme IN lr_ausme[].
        ls_entityset-tflag = abap_true.
      ELSEIF lwa_marc-ausme IN lr_ausmeb[].

        READ TABLE lt_taskbags ASSIGNING FIELD-SYMBOL(<fs_taskbags>)
                               WITH KEY task = ls_entityset-matnr
                               BINARY SEARCH.
        IF sy-subrc = 0.
          ls_entityset-tflag = abap_true.
          ls_entityset-bagsnum  = <fs_taskbags>-bags_num.
          ls_entityset-bagsunit = <fs_taskbags>-mengeh.
        ENDIF.

      ENDIF.
    ENDIF.
*-- JOBREGON: End of modification
    ls_entityset-pernr = lv_persno.
    ls_entityset-lifnr = lv_lifnr.
    APPEND ls_entityset TO et_entityset.
    CLEAR: ls_entityset,lwa_matnr.
  ENDLOOP.

ENDMETHOD.


METHOD get_wooperations.
*----------------------------------------------------------------------*
* General Information
*----------------------------------------------------------------------*
* WRICEF ID    : Issue # 14
* Class        : ZCL_ABS_AGRI_MOBILE_E_DPC_EXT
* Method       : WOOPERATIONS_EXT_GET_ENTITYSET
* Company      : Vistex
* Dev. Author  : John Anthony Obregon Lanas
* Create Date  : 05.03.2020
* Description  : New Entity Set For work order operations data.
*----------------------------------------------------------------------*
* Modification log
*----------------------------------------------------------------------*
* Date         | TR#         | Author          | Modification
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
  "Types.
  TYPES: BEGIN OF ltyp_fpitm,
           aufnr TYPE /agri/fmfpitm-aufnr,
           iwerk TYPE /agri/fmfphdr-iwerk,
           objnr TYPE /agri/fmfphdr-objnr,
           posnr TYPE /agri/fmfpitm-posnr,
           vornr TYPE /agri/fmfpitm-vornr,
           ltxa1 TYPE /agri/fmfpitm-ltxa1,
           arbpl TYPE /agri/fmfpitm-arbpl,
           gamng TYPE /agri/fmfpitm-gamng,
           gwemg TYPE /agri/fmfpitm-gwemg,
           meinh TYPE /agri/fmfpitm-meinh,
           grcre TYPE /agri/fmfpitm-grcre,
*           steus TYPE /agri/fmfpitm-steus,
         END OF ltyp_fpitm.
  "constants.
  CONSTANTS: lc_autyp TYPE /agri/gl_autyp VALUE 'TO',
             lc_cstat TYPE /agri/fmcstat  VALUE 'CNF'.

  DATA : lv_filter_str     TYPE string,
         lo_filter         TYPE REF TO /iwbep/if_mgw_req_filter,
         ls_filter         TYPE /iwbep/s_mgw_select_option,
         lrt_aufnr         TYPE RANGE OF /agri/fmfpnum,
         lrt_vornr         TYPE RANGE OF vornr,
         lrt_posnr         TYPE RANGE OF posnr,
         lrt_pernr         TYPE RANGE OF persno,
         lrt_lifnr         TYPE RANGE OF lifnr,
         lv_persno         TYPE persno,
         lr_lifnr          LIKE LINE OF lrt_lifnr,
         lv_lifnr          TYPE lifnr,
         lr_pernr          LIKE LINE OF lrt_pernr,
         lv_var            TYPE char1,
         lt_filter         TYPE /iwbep/t_mgw_select_option,
         ls_converted_keys LIKE LINE OF et_entityset,
         lt_rttrn          TYPE zcl_zabs_mob_toc_mpc=>tt_worouteterrain,
         lt_entityset      TYPE zcl_zabs_mob_toc_mpc=>tt_wooperations,
         lt_fpitm          TYPE TABLE OF ltyp_fpitm, "/agri/t_fmfpitm,
         lt_rtfla          TYPE TABLE OF /agri/glrtfla,
         ls_fpitm          TYPE ltyp_fpitm, "/agri/s_fmfpitm,
         lt_orderby        TYPE /iwbep/t_mgw_tech_order,
         ls_orderby        TYPE /iwbep/s_mgw_tech_order,
         ls_entityset      LIKE LINE OF et_entityset,
*         lt_rtusr        TYPE TABLE OF /agri/glrtusr.
         lt_rtusr          TYPE TABLE OF zabs_usrpernr.

  DATA :
*         lv_low_acdt    TYPE sy-datum,
*         lv_high_acdt   TYPE sy-datum,
*         lv_cnval1_acdt TYPE zabs_del_cnval,
*         lr_actdt       TYPE RANGE OF /agri/fmactdt,
         lr_grcre       TYPE RANGE OF /agri/glgrcre.

  FIELD-SYMBOLS:<lwa_aufnr> LIKE LINE OF lrt_aufnr.

  lo_filter     = io_tech_request_context->get_filter( ).
  lt_filter     = lo_filter->get_filter_select_options( ).
  lv_filter_str = lo_filter->get_filter_string( ).

  IF  lv_filter_str IS NOT INITIAL
 AND lt_filter[]   IS INITIAL.

    me->/iwbep/if_sb_dpc_comm_services~log_message(
    EXPORTING
      iv_msg_type   = 'E'
      iv_msg_id     = '/IWBEP/MC_SB_DPC_ADM'
      iv_msg_number = 025 ).
    " Raise Exception
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
      EXPORTING
        textid = /iwbep/cx_mgw_tech_exception=>internal_error.

  ENDIF.

  io_tech_request_context->get_converted_source_keys(
    IMPORTING
      es_key_values  = ls_converted_keys ).

* Maps filter table lines to function module parameters
  LOOP AT lt_filter INTO ls_filter.

    CASE ls_filter-property.
      WHEN 'AUFNR'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_aufnr ).
      WHEN 'POSNR'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_vornr ).
      WHEN 'VORNR'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_vornr ).
      WHEN 'PERNR'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_pernr ).
        READ TABLE lrt_pernr INTO lr_pernr INDEX 1.
        IF sy-subrc EQ 0.
          lv_persno = lr_pernr-low.
        ENDIF.
      WHEN 'LIFNR'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_lifnr ).
        READ TABLE lrt_lifnr INTO lr_lifnr INDEX 1.
        IF sy-subrc EQ 0.
          lv_lifnr = lr_lifnr-low.
        ENDIF.
      WHEN OTHERS.
        " Log message in the application log
        me->/iwbep/if_sb_dpc_comm_services~log_message(
          EXPORTING
            iv_msg_type   = 'E'
            iv_msg_id     = '/IWBEP/MC_SB_DPC_ADM'
            iv_msg_number = 020
            iv_msg_v1     = ls_filter-property ).
        " Raise Exception
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
          EXPORTING
            textid = /iwbep/cx_mgw_tech_exception=>internal_error.
    ENDCASE.

  ENDLOOP.
  IF lrt_aufnr IS NOT INITIAL.
    LOOP AT lrt_aufnr ASSIGNING <lwa_aufnr>.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <lwa_aufnr>-low
        IMPORTING
          output = <lwa_aufnr>-low.

    ENDLOOP.
  ENDIF.

*  SELECT *
*    FROM /agri/glrtusr
*    INTO TABLE @lt_rtusr
*    WHERE bname = @sy-uname.

*Fetch PERNR/LIFNR based Terrains
  get_route_terrain_dtls(
      EXPORTING
        io_tech_request_context = io_tech_request_context
      IMPORTING
        et_rttrn                = lt_rttrn ).
  IF lt_rttrn[] IS INITIAL.
    RETURN.
  ENDIF.

  "change -> Login User based validation to User+Employee based valiadtion
  SELECT SINGLE *
    FROM zabs_emp_role
    INTO @DATA(ls_usremp)
    WHERE pernr IN @lrt_pernr.
  IF ls_usremp-fpcnf = 'R'. " Activities and Goods REceipt Create
    lr_grcre = VALUE #( sign = 'I' option = 'EQ' ( low = abap_true ) ).
  ELSEIF ls_usremp-fpcnf = 'S'. "Supplies and No Goods Receipt Created.
    lr_grcre = VALUE #( sign = 'I' option = 'EQ' ( low = abap_false ) ).
  ENDIF.
*  else.
*     lr_grcre = VALUE #( sign = 'I' option = 'BT' (
*    endif.


*  "change -> Login User based validation to User+Employee based valiadtion
*  IF lrt_pernr IS NOT INITIAL.
*    SELECT *
*    FROM zabs_usrpernr
*    INTO TABLE lt_rtusr
*    WHERE pernr IN lrt_pernr.
*  ELSEIF lrt_lifnr IS NOT INITIAL.
*    SELECT *
*    FROM zabs_usrpernr
*    INTO TABLE lt_rtusr
*    WHERE lifnr IN lrt_lifnr.
*  ENDIF.
*
*  IF lt_rtusr IS NOT INITIAL.
*    SELECT *
*      FROM /agri/glrtfla                                "#EC CI_NOWHERE
*      INTO TABLE @lt_rtfla
*      FOR ALL ENTRIES IN @lt_rtusr                 "#EC CI_NO_TRANSFORM
*      WHERE route = @lt_rtusr-route.
*  ENDIF.

***--Get variant table data
*  "BOC - 19.06.2020
*
*  CALL METHOD zcl_abs_get_variants=>get_constant_single
*    EXPORTING
*      iv_mod    = 'C'
*      iv_objid  = 'MOBL'
*      iv_k1val  = 'INSPOP'
*      iv_k2val  = 'LACTDT'
*      iv_k3val  = ls_usremp-urole
*    IMPORTING
*      ev_cnval1 = lv_cnval1_acdt.
*  CONDENSE lv_cnval1_acdt.
*  lv_low_acdt   = sy-datum - lv_cnval1_acdt.
*  CALL METHOD zcl_abs_get_variants=>get_constant_single
*    EXPORTING
*      iv_mod    = 'C'
*      iv_objid  = 'MOBL'
*      iv_k1val  = 'INSPOP'
*      iv_k2val  = 'HACTDT'
*      iv_k3val  = ls_usremp-urole
*    IMPORTING
*      ev_cnval1 = lv_cnval1_acdt.
*  CONDENSE lv_cnval1_acdt.
*  lv_high_acdt  = sy-datum + lv_cnval1_acdt.
*  lr_actdt = VALUE #( sign = 'I' option = 'BT' ( low = lv_low_acdt
*                                                 high = lv_high_acdt ) ).
*
*  " BOC - 21.06.2020

  "BOC - 03.07.2020
  DATA : lv_low_gstrp    TYPE sy-datum,
         lv_high_gstrp   TYPE sy-datum,
         lv_cnval1_gstrp TYPE zabs_del_cnval,
         ltr_gstrp       TYPE RANGE OF co_gstrp.

  CALL METHOD zcl_abs_get_variants=>get_constant_single
    EXPORTING
      iv_mod    = 'C'
      iv_objid  = 'MOBL'
      iv_k1val  = 'INSPOP'
      iv_k2val  = zcl_abs_abap_maintain=>c_key_lgstrp   "'LGSTRP'
      iv_k3val  = ls_usremp-urole
    IMPORTING
      ev_cnval1 = lv_cnval1_gstrp. "'4'
  CONDENSE lv_cnval1_gstrp.
  lv_low_gstrp   = sy-datum - lv_cnval1_gstrp.

  CALL METHOD zcl_abs_get_variants=>get_constant_single
    EXPORTING
      iv_mod    = 'C'
      iv_objid  = 'MOBL'
      iv_k1val  = 'INSPOP'
      iv_k2val  = zcl_abs_abap_maintain=>c_key_hgstrp   "'HGSTRP'
      iv_k3val  = ls_usremp-urole
    IMPORTING
      ev_cnval1 = lv_cnval1_gstrp. "'1'
  CONDENSE lv_cnval1_gstrp.
  lv_high_gstrp  = sy-datum + lv_cnval1_gstrp.

  ltr_gstrp = VALUE #( sign = zcl_abs_abap_maintain=>c_rsign_include
                     option = zcl_abs_abap_maintain=>c_ropt_between " 'BT'
                     ( low  = lv_low_gstrp
                       high = lv_high_gstrp ) ).
  "EOC - 03.07.2020


*--Fetch Login User based - Task Material maitained in the System
  SELECT a~task, a~vornr, a~stapp, b~urole
  FROM zabst_task_app AS a
  INNER JOIN zabs_emp_role AS b
  ON a~urole = b~urole
  INTO TABLE @DATA(lt_task_app)
    WHERE b~pernr IN @lrt_pernr
      AND a~stapp EQ @abap_true.
  IF sy-subrc NE 0.
*- If user role is not having any Task maintained - then dont show any task orders
    RETURN.
  ENDIF.
  DATA lr_matnr TYPE RANGE OF matnr.
  lr_matnr =  VALUE #( FOR ls_task_app IN lt_task_app (
   sign   = 'I'
   option = 'EQ'
   low    = ls_task_app-task ) ).
  DELETE lr_matnr WHERE low IS INITIAL.
  SORT lr_matnr BY low.
  DELETE ADJACENT DUPLICATES FROM lr_matnr COMPARING low.
*  IF lt_rtfla IS NOT INITIAL.
*  IF lt_rttrn IS NOT INITIAL.
  SELECT  a~aufnr,
          a~posnr,
          a~vornr,
          a~ltxa1,
          a~arbpl,
          a~gamng,
          a~gwemg,
          a~meinh,
          a~grcre,
*            a~steus " FS-03: SCHALLA
          b~aufnr,
          b~tecom,
          b~iwerk,
          b~objnr
    FROM /agri/fmfpitm AS a
    INNER JOIN /agri/fmfphdr AS b
    ON a~aufnr EQ b~aufnr
           INTO CORRESPONDING FIELDS OF TABLE @lt_fpitm
           FOR ALL ENTRIES IN @lt_rttrn"lt_rtfla  "#EC CI_NO_TRANSFORM
           WHERE b~aufnr   IN @lrt_aufnr
             AND b~autyp    = @lc_autyp
             AND b~gstrp   IN @ltr_gstrp "BOC - 03.07.2020
             AND b~tecom   <> @abap_true
             AND a~posnr   IN @lrt_posnr
             AND a~vornr   IN @lrt_vornr
             AND a~arbpl   <> @space
*               AND a~cstat   <> @lc_cstat
*               AND a~actdt   IN @lr_actdt"BOC - 19.06.2020
             AND a~grcre   IN @lr_grcre " Value should vary based on User Role - R, S and Both
             AND b~tplnr_fl = @lt_rttrn-tplnr
             AND b~matnr IN @lr_matnr."@lt_rtfla-tplnr_fl.
*  ELSE.
*    SELECT a~aufnr,
*           a~iwerk,
*           a~objnr,
*           b~posnr,
*           b~vornr,
*           b~ltxa1,
*           b~arbpl,
*           b~gamng,
*           b~gwemg,
*           b~meinh,
*           b~grcre
**           b~steus " FS-03: SCHALLA
*        FROM /agri/fmfphdr AS a
*        INNER JOIN /agri/fmfpitm AS b
*        ON b~aufnr EQ a~aufnr
*        INTO CORRESPONDING FIELDS OF TABLE @lt_fpitm
*        WHERE a~aufnr IN @lrt_aufnr
*          AND a~autyp  = @lc_autyp
*          AND a~tecom <> @abap_true
*          AND b~posnr IN @lrt_posnr
*          AND b~vornr IN @lrt_vornr
*          AND b~cstat <> @lc_cstat
*          AND b~arbpl <> @space
*          AND b~actdt IN @lr_actdt."BOC - 19.06.2020.
*  ENDIF.

  IF lt_fpitm IS NOT INITIAL.

    SELECT arbpl,
           werks,
           verwe
      FROM crhd
      INTO TABLE @DATA(lt_crhd)
       FOR ALL ENTRIES IN @lt_fpitm
     WHERE arbpl EQ @lt_fpitm-arbpl
       AND werks EQ @lt_fpitm-iwerk.
    IF sy-subrc EQ 0.
      SORT lt_crhd BY arbpl
                      werks.
    ENDIF.
  ENDIF.

*  SELECT SINGLE *
*    FROM zabs_usr_emp
*    INTO @DATA(ls_usremp)
*    WHERE bname = @sy-uname.
  "BOC - 21.06.2020
  DATA : lv_ldays  TYPE t5a4a-dlydy,
         lv_hdays  TYPE t5a4a-dlydy,
         lv_cnval1 TYPE zabs_del_cnval.

  CALL METHOD zcl_abs_get_variants=>get_constant_single
    EXPORTING
      iv_mod    = 'C'
      iv_objid  = 'MOBL'
      iv_k1val  = 'INSPOP'
      iv_k2val  = 'VLDAYS'
    IMPORTING
      ev_cnval1 = lv_cnval1.
  CONDENSE lv_cnval1.
  lv_ldays   = lv_cnval1.
  CALL METHOD zcl_abs_get_variants=>get_constant_single
    EXPORTING
      iv_mod    = 'C'
      iv_objid  = 'MOBL'
      iv_k1val  = 'INSPOP'
      iv_k2val  = 'VHDAYS'
    IMPORTING
      ev_cnval1 = lv_cnval1.
  CONDENSE lv_cnval1.
  lv_hdays = lv_cnval1.

  LOOP AT lt_fpitm INTO ls_fpitm.

*    "BOC - 19.06.2020 status checking.
*    CLEAR :lv_var.
*    CALL METHOD me->order_status
*      EXPORTING
*        iv_objnr  = ls_fpitm-objnr
*        iv_source = zcl_zabs_agri_mobile_e_dpc_ext=>c_source_taskorder "'TC'
*      IMPORTING
*        ev_var    = lv_var.
*    IF lv_var EQ 'X'.
*      CONTINUE.
*    ENDIF.
*This condition was commented as asked by functional consultant.
*    READ TABLE lt_crhd INTO DATA(ls_crhd) WITH KEY arbpl = ls_fpitm-arbpl
*                                                   werks = ls_fpitm-iwerk
*                                          BINARY SEARCH.
*    IF sy-subrc EQ 0.
**      IF ls_crhd-verwe NE '0003'.  "JOBREGON:
**        CONTINUE.
**      ENDIF.
*    ENDIF.

    MOVE-CORRESPONDING ls_fpitm TO ls_entityset.
    ls_entityset-werks = ls_fpitm-iwerk.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input          = ls_entityset-meinh
      IMPORTING
        output         = ls_entityset-meinh
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = ls_entityset-aufnr
      IMPORTING
        output = ls_entityset-aufnr.

    ls_entityset-pernr = lv_persno.
    ls_entityset-lifnr = lv_lifnr.
    ls_entityset-vldays = lv_ldays."BOC - 21.06.2020
    ls_entityset-vhdays = lv_hdays."BOC - 21.06.2020
    APPEND ls_entityset TO lt_entityset.
    CLEAR ls_entityset.
  ENDLOOP.

  lt_orderby = io_tech_request_context->get_orderby( ).
  READ TABLE lt_orderby INTO ls_orderby INDEX 1.
  IF sy-subrc EQ 0 AND ls_orderby-order EQ 'asc' AND
     ls_orderby-property = 'POSNR'.
    SORT lt_entityset BY posnr ASCENDING.
    et_entityset = lt_entityset.
  ENDIF.

  IF et_entityset IS INITIAL.
    et_entityset = lt_entityset.
*
*    IF ls_usremp-fpcnf = 'R'.
*      DELETE et_entityset WHERE grcre <> abap_true.
*    ELSEIF ls_usremp-fpcnf = 'S'.
*      DELETE et_entityset WHERE grcre = abap_true.
*    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD get_wotaskorders.
*----------------------------------------------------------------------*
* General Information
*----------------------------------------------------------------------*
* WRICEF ID    : Document of standard app isuues
* Class        : ZCL_ABS_AGRI_MOBILE_E_DPC_EXT
* Method       : WOTASKORDERS_EXT_GET_ENTITYSET
* Company      : Vistex
* Dev. Author  : John Anthony Obregon Lanas
* Create Date  : 12.03.2020
* Description  : New Entity Set that displays all types of task orders.
*----------------------------------------------------------------------*
* Modification log
*----------------------------------------------------------------------*
* Date         | TR#         | Author          | Modification
*----------------------------------------------------------------------*
* 12.03.2020     C4DK909655    T_A.Harshe        Creation
*----------------------------------------------------------------------*
  "types.
  TYPES: BEGIN OF ltyp_fphdr,
           aufnr    TYPE /agri/fmfphdr-aufnr,
           tplnr_fl TYPE /agri/fmfphdr-tplnr_fl,
           cmnum    TYPE /agri/fmfphdr-cmnum,
           datab    TYPE /agri/fmfphdr-datab,
           datbi    TYPE /agri/fmfphdr-datbi,
           matnr    TYPE /agri/fmfphdr-matnr,
           gamng    TYPE /agri/fmfphdr-gamng,
           gwemg    TYPE /agri/fmfphdr-gwemg,
           gmein    TYPE /agri/fmfphdr-gmein,
           objnr    TYPE /agri/fmfphdr-objnr,
         END OF ltyp_fphdr.

  TYPES: BEGIN OF ltyp_makt,
           matnr TYPE makt-matnr,
           maktx TYPE makt-maktx,
         END OF ltyp_makt.

  "constants.
  CONSTANTS: lc_autyp TYPE /agri/gl_autyp VALUE 'TO',
             lc_astat TYPE /agri/glastat  VALUE 'A',
             lc_cstat TYPE /agri/fmcstat  VALUE 'CNF'.

  "local declarations.
  DATA : lv_filter_str TYPE string,
         lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
         ls_filter     TYPE /iwbep/s_mgw_select_option,
         lrt_aufnr     TYPE RANGE OF /agri/fmfpnum,
         lwa_aufnr     LIKE LINE OF lrt_aufnr,
         lrt_route     TYPE RANGE OF /agri/glroute, " /agri/gl_route,
         lrt_datab     TYPE RANGE OF /agri/gldatab,
         ls_route      LIKE LINE OF lrt_route,
         lrt_tplnr     TYPE RANGE OF /agri/gltplnr_fl,
         lrs_tplnr     LIKE LINE OF lrt_tplnr,
         ls_tplnr      LIKE LINE OF  lrt_tplnr,
         ls_datab      LIKE LINE OF lrt_datab,
         lrt_matnr     TYPE RANGE OF matnr,
         lrt_matnrt    TYPE RANGE OF matnr,
         lrt_pernr     TYPE RANGE OF persno,
         lrt_lifnr     TYPE RANGE OF lifnr,
         lr_pernr      LIKE LINE OF lrt_pernr,
         lv_persno     TYPE persno,
         lr_lifnr      LIKE LINE OF lrt_lifnr,
         lv_lifnr      TYPE lifnr,
         lt_defrtt     TYPE TABLE OF /agri/glrtfla,
         ls_defrtt     TYPE          /agri/glrtfla,
         lt_filter     TYPE /iwbep/t_mgw_select_option,
         lt_constants  TYPE zabs_tty_vkey_const,
         lt_rttrn      TYPE zcl_zabs_mob_toc_mpc=>tt_worouteterrain,
         ls_rttrn      TYPE zcl_zabs_mob_toc_mpc=>ts_worouteterrain,
***         ls_converted_keys LIKE LINE OF et_entityset,
*         lt_fphdr      TYPE TABLE OF ltyp_fphdr, "/agri/t_fmfphdr,
*         ls_fphdr      TYPE ltyp_fphdr, "/agri/s_fmfphdr,
         lt_maktx      TYPE TABLE OF ltyp_makt,
         ls_maktx      TYPE ltyp_makt,
*         lt_rtusr      TYPE TABLE OF /agri/glrtusr,
         lt_rtusr      TYPE TABLE OF zabs_usrpernr,
         lv_tmstmp     TYPE ad_tstamp, " JOBREGON
         ls_entityset  LIKE LINE OF et_entityset,
         lv_var        TYPE char1,
         lv_urole      TYPE zabs_del_urole.

  "field symbols.
  FIELD-SYMBOLS:<lwa_aufnr>    LIKE LINE OF lrt_aufnr,
                <lwa_tplnr_fl> LIKE LINE OF lrt_tplnr.

  lo_filter     = io_tech_request_context->get_filter( ).
  lt_filter     = lo_filter->get_filter_select_options( ).
  lv_filter_str = lo_filter->get_filter_string( ).

  IF  lv_filter_str IS NOT INITIAL
  AND lt_filter[]   IS INITIAL.

    me->/iwbep/if_sb_dpc_comm_services~log_message(
    EXPORTING
      iv_msg_type   = 'E'
      iv_msg_id     = '/IWBEP/MC_SB_DPC_ADM'
      iv_msg_number = 025 ).
    " Raise Exception
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
      EXPORTING
        textid = /iwbep/cx_mgw_tech_exception=>internal_error.

  ENDIF.

* Maps filter table lines to function module parameters
  LOOP AT lt_filter INTO ls_filter.

    CASE ls_filter-property.

      WHEN 'ROUTE'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_route ).

      WHEN 'AUFNR'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_aufnr ).
      WHEN 'TPLNR_FL'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_tplnr ).
      WHEN 'DATAB'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_datab ).
        READ TABLE lrt_datab INTO ls_datab INDEX 1.
      WHEN 'MATNR'.
        lo_filter->convert_select_option(             "#EC CI_FLDEXT_OK
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_matnr ).
      WHEN 'PERNR'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_pernr ).
        READ TABLE lrt_pernr INTO lr_pernr INDEX 1.
        IF sy-subrc EQ 0.
          lv_persno = lr_pernr-low.
        ENDIF.
      WHEN 'LIFNR'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_lifnr ).
        READ TABLE lrt_lifnr INTO lr_lifnr INDEX 1.
        IF sy-subrc EQ 0.
          lv_lifnr = lr_lifnr-low.
        ENDIF.
      WHEN OTHERS.
        " Log message in the application log
        me->/iwbep/if_sb_dpc_comm_services~log_message(
          EXPORTING
            iv_msg_type   = 'E'
            iv_msg_id     = '/IWBEP/MC_SB_DPC_ADM'
            iv_msg_number = 020
            iv_msg_v1     = ls_filter-property ).
        " Raise Exception
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
          EXPORTING
            textid = /iwbep/cx_mgw_tech_exception=>internal_error.
    ENDCASE.

  ENDLOOP.

*  IF lrt_route IS NOT INITIAL.
*    SELECT *
*      FROM /agri/glrtusr
*      INTO TABLE @lt_rtusr
*      WHERE route IN @lrt_route
*        AND bname = @sy-uname.
*  ELSE.
*    SELECT *
*     FROM /agri/glrtusr
*     INTO TABLE @lt_rtusr
*     WHERE bname = @sy-uname.
*  ENDIF.

  "change -> Login User based validation to User+Employee based valiadtion
*  IF lrt_route IS NOT INITIAL.
*    SELECT *
*      FROM zabs_usrpernr
*      INTO TABLE @lt_rtusr
*       WHERE route IN @lrt_route
*         AND pernr IN @lrt_pernr.
*  ELSE.
*    IF lrt_pernr IS NOT INITIAL.
*      SELECT *
*      FROM zabs_usrpernr
*      INTO TABLE lt_rtusr
*      WHERE pernr IN lrt_pernr.
*    ELSEIF lrt_lifnr IS NOT INITIAL.
*      SELECT *
*      FROM zabs_usrpernr
*      INTO TABLE lt_rtusr
*      WHERE lifnr IN lrt_lifnr.
*    ENDIF.
*  ENDIF.
*
**--Fetch Routes
*  IF lt_rtusr IS NOT INITIAL.
*    SELECT *
*      FROM /agri/glrtfla
*      INTO TABLE @lt_defrtt
*      FOR ALL ENTRIES IN @lt_rtusr                 "#EC CI_NO_TRANSFORM
*      WHERE route EQ @lt_rtusr-route.
*    IF sy-subrc = 0.
*      SORT lt_defrtt BY tplnr_fl.
*      SELECT  tplnr_fl,                                "#EC CI_DYNWHERE
*              contr,
*              cmnum,
*              season,
*              datab,
*              datbi,
*              aarea,
*              msehi,
*              exhad,
*              eston,
*              esuom,
*              ernam,
*              erdat,
*              erzet,
*              aenam,
*              aedat,
*              aezet
*        FROM /agri/glflcma
*             INTO TABLE @DATA(lt_flcma)
*        FOR ALL ENTRIES IN @lt_defrtt               "#EC CI_NO_TRANSFORM
*             WHERE tplnr_fl = @lt_defrtt-tplnr_fl
*                AND astat  = @lc_astat
*                AND datab <= @sy-datum
*                AND datbi >= @sy-datum
*                AND loevm  = @space.
*    ENDIF.
*  ENDIF.

*  LOOP AT lt_defrtt INTO ls_defrtt. "WHERE route = ls_route-low. "#EC CI_STDSEQ
*    ls_tplnr-sign   = 'I'.
*    ls_tplnr-option = 'EQ'.
*    ls_tplnr-low = ls_defrtt-tplnr_fl.
*    APPEND ls_tplnr TO lrt_tplnr.
*  ENDLOOP.
*
*  IF lrt_tplnr IS NOT INITIAL.
*    LOOP AT lrt_tplnr ASSIGNING <lwa_tplnr_fl>.
*
*      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
*        EXPORTING
*          input      = <lwa_tplnr_fl>-low
*        IMPORTING
*          output     = <lwa_tplnr_fl>-low
*        EXCEPTIONS
*          not_found  = 1
*          not_active = 2
*          OTHERS     = 3.
*      IF sy-subrc <> 0.
** Implement suitable error handling here
*      ENDIF.
*
*    ENDLOOP.
*  ENDIF.

  IF lrt_aufnr IS NOT INITIAL.
    LOOP AT lrt_aufnr ASSIGNING <lwa_aufnr>.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <lwa_aufnr>-low
        IMPORTING
          output = <lwa_aufnr>-low.
    ENDLOOP.
  ENDIF.
  IF ls_datab IS INITIAL.
    ls_datab-sign = 'I'. ls_datab-option = 'EQ'.
    ls_datab-low = sy-datum.
  ENDIF.

*Fetch PERNR/LIFNR based Terrains
  get_route_terrain_dtls(
      EXPORTING
        io_tech_request_context = io_tech_request_context
      IMPORTING
        et_rttrn                = lt_rttrn ).
  IF lt_rttrn[] IS INITIAL.
    RETURN.
  ENDIF.
  SORT lt_rttrn BY tplnr_fl.

*  "change -> Login User based validation to User+Employee based valiadtion
  SELECT a~task, b~urole
    FROM zabst_task_app AS a
    INNER JOIN zabs_emp_role AS b
    ON a~urole = b~urole
    INTO TABLE @DATA(lt_taskmat)
    WHERE b~pernr IN @lrt_pernr
      AND a~task  IN @lrt_matnr
      AND a~stapp EQ @abap_true.
*      AND a~task  = @lt_fphdr-matnr.
  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  SORT lt_taskmat BY task.
  DATA lr_matnr TYPE RANGE OF matnr.
  lr_matnr =  VALUE #( FOR ls_taskmat IN lt_taskmat (
   sign   = 'I'
   option = 'EQ'
   low    = ls_taskmat-task ) ).
  DELETE lr_matnr WHERE low IS INITIAL.
  SORT lr_matnr BY low.
  DELETE ADJACENT DUPLICATES FROM lr_matnr COMPARING low.

  "BOC - 27.06.2020
  READ TABLE lt_taskmat ASSIGNING FIELD-SYMBOL(<ls_taskmat>) INDEX 1.
  IF <ls_taskmat> IS ASSIGNED.
    lv_urole = <ls_taskmat>-urole.
  ENDIF.
*--Get variant table data
*  CALL METHOD zcl_abs_get_variants=>get_constant_multiple
*    EXPORTING
*      iv_mod       = 'C'
*      iv_objid     = 'MOBL'
*      iv_k1val     = 'INSPOP'
*      iv_k2val     = 'STEUS'
*    IMPORTING
*      et_constants = lt_constants.
*
*  DATA lr_steus TYPE RANGE OF steus.
*  lr_steus =  VALUE #( FOR ls_constant IN lt_constants (
*  sign   = 'I'
*  option = 'EQ'
*  low    = ls_constant-cnval1 ) ).
*  DELETE lr_steus WHERE low IS INITIAL.
*  SORT lr_steus BY low.
*  DELETE ADJACENT DUPLICATES FROM lr_steus COMPARING low.

*--Get variant table data
*  "BOC - 19.06.2020
*  DATA : lv_low_acdt    TYPE sy-datum,
*         lv_high_acdt   TYPE sy-datum,
*         lv_cnval1_acdt TYPE zabs_del_cnval,
*         lr_actdt       TYPE RANGE OF /agri/fmactdt.
*  CALL METHOD zcl_abs_get_variants=>get_constant_single
*    EXPORTING
*      iv_mod    = 'C'
*      iv_objid  = 'MOBL'
*      iv_k1val  = 'INSPOP'
*      iv_k2val  = 'LACTDT'
*      iv_k3val  = lv_urole
*    IMPORTING
*      ev_cnval1 = lv_cnval1_acdt.
*  CONDENSE lv_cnval1_acdt.
*  lv_low_acdt   = sy-datum - lv_cnval1_acdt.
*  CALL METHOD zcl_abs_get_variants=>get_constant_single
*    EXPORTING
*      iv_mod    = 'C'
*      iv_objid  = 'MOBL'
*      iv_k1val  = 'INSPOP'
*      iv_k2val  = 'HACTDT'
*      iv_k3val  = lv_urole
*    IMPORTING
*      ev_cnval1 = lv_cnval1_acdt.
*  CONDENSE lv_cnval1_acdt.
*  lv_high_acdt  = sy-datum + lv_cnval1_acdt.
*  lr_actdt = VALUE #( sign = 'I' option = 'BT' ( low = lv_low_acdt
*                                                 high = lv_high_acdt ) ).

  "BOC - 03.07.2020
  DATA : lv_low_gstrp    TYPE sy-datum,
         lv_high_gstrp   TYPE sy-datum,
         lv_cnval1_gstrp TYPE zabs_del_cnval,
         ltr_gstrp       TYPE RANGE OF co_gstrp.

  CALL METHOD zcl_abs_get_variants=>get_constant_single
    EXPORTING
      iv_mod    = 'C'
      iv_objid  = 'MOBL'
      iv_k1val  = 'INSPOP'
      iv_k2val  = zcl_abs_abap_maintain=>c_key_lgstrp   "'LGSTRP'
      iv_k3val  = lv_urole
    IMPORTING
      ev_cnval1 = lv_cnval1_gstrp. "'4'
  CONDENSE lv_cnval1_gstrp.
  lv_low_gstrp   = sy-datum - lv_cnval1_gstrp.

  CALL METHOD zcl_abs_get_variants=>get_constant_single
    EXPORTING
      iv_mod    = 'C'
      iv_objid  = 'MOBL'
      iv_k1val  = 'INSPOP'
      iv_k2val  = zcl_abs_abap_maintain=>c_key_hgstrp   "'HGSTRP'
      iv_k3val  = lv_urole
    IMPORTING
      ev_cnval1 = lv_cnval1_gstrp. "'1'
  CONDENSE lv_cnval1_gstrp.
  lv_high_gstrp  = sy-datum + lv_cnval1_gstrp.

  ltr_gstrp = VALUE #( sign = zcl_abs_abap_maintain=>c_rsign_include
                     option = zcl_abs_abap_maintain=>c_ropt_between " 'BT'
                     ( low  = lv_low_gstrp
                       high = lv_high_gstrp ) ).
  "EOC - 03.07.2020

*    SELECT aufnr,
*           tplnr_fl,
*           cmnum,
*           datab,
*           datbi,
*           matnr,
*           gamng,
*           gwemg,
*           gmein,
*           objnr
*       FROM /agri/fmfphdr
*       INTO TABLE @lt_fphdr
*       FOR ALL ENTRIES IN @lt_rttrn "@lt_flcma
*       WHERE aufnr    IN @lrt_aufnr
*         AND autyp     = @lc_autyp
*         AND tplnr_fl  = @lt_rttrn-tplnr"@lt_flcma-tplnr_fl
**        AND class EQ '1' " JOBREGON - This filter was taken out in order to allow Nursey Task Orders according to client's requirement.
*         AND datab    <= @ls_datab-low
*         AND datbi    >= @ls_datab-low
*         AND matnr    IN @lr_matnr "@lrt_matnr
*         AND tecom     = @space.

  SELECT a~aufnr,
         a~tplnr_fl,
         a~cmnum,
         a~datab,
         a~datbi,
         a~matnr,
         a~gamng,
         a~gwemg,
         a~gmein,
         a~objnr
    FROM /agri/fmfphdr AS a
    INNER JOIN /agri/fmfpitm AS b
      ON a~aufnr = b~aufnr
    INTO TABLE @DATA(lt_fphdr)
     FOR ALL ENTRIES IN @lt_rttrn
   WHERE a~autyp     = @lc_autyp
     AND a~aufnr    IN @lrt_aufnr
     AND a~tplnr_fl = @lt_rttrn-tplnr
*     AND a~datab    <= @ls_datab-low
*     AND a~datbi    >= @ls_datab-low
     AND a~matnr    IN @lr_matnr
     AND a~gstrp    IN @ltr_gstrp "BOC - 03.07.2020
     AND a~tecom     = @space.
*     AND a~cstat    <> @lc_cstat
*     AND b~actdt    IN @lr_actdt.
*         AND b~steus    IN @lr_steus

*  ELSE.
*
*    SELECT aufnr,
*           tplnr_fl,
*           cmnum,
*           datab,
*           datbi,
*           matnr,
*           gamng,
*           gwemg,
*           gmein,
*           objnr
*        FROM /agri/fmfphdr
*       INTO TABLE @lt_fphdr
*      FOR ALL ENTRIES IN @lt_flcma
*       WHERE aufnr     IN @lrt_aufnr
*          AND autyp     = @lc_autyp
*          AND tplnr_fl  = @lt_flcma-tplnr_fl
**         AND datab LE ls_datab-low
**         AND datbi GE ls_datab-low
**           AND class EQ '1' " JOBREGON - This filter was taken out in order to allow Nursey Task Orders according to client's requirement.
*         AND matnr     IN @lrt_matnr
*         AND tecom      = @space.
*  ENDIF.

  IF lt_fphdr IS NOT INITIAL.
    SELECT matnr, maktx
      FROM makt
      INTO TABLE @lt_maktx
      FOR ALL ENTRIES IN @lt_fphdr                 "#EC CI_NO_TRANSFORM
      WHERE matnr = @lt_fphdr-matnr
        AND spras = @sy-langu.
    IF sy-subrc = 0.
      SORT lt_maktx BY matnr.
    ENDIF.
  ENDIF.

*  SELECT a~task, b~urole
*    FROM zabst_task_app AS a
*    INNER JOIN zabs_usr_emp AS b
*    ON a~urole = b~urole
*    INTO TABLE @DATA(lt_taskmat)
*    FOR ALL ENTRIES IN @lt_fphdr
*    WHERE b~bname = @sy-uname
*      AND a~task  = @lt_fphdr-matnr.

*  "change -> Login User based validation to User+Employee based valiadtion
*  SELECT a~task, b~urole
*    FROM zabst_task_app AS a
*    INNER JOIN zabs_emp_role AS b
*    ON a~urole = b~urole
*    INTO TABLE @DATA(lt_taskmat)
*    FOR ALL ENTRIES IN @lt_fphdr
*    WHERE b~pernr IN @lrt_pernr
*      AND a~task  = @lt_fphdr-matnr.
*
*  IF sy-subrc = 0.
*    SORT lt_taskmat BY task.
*  ENDIF.

  LOOP AT lt_fphdr INTO DATA(ls_fphdr).

*    "BOC - 19.06.2020 status checking.
*    CLEAR :lv_var.
*    CALL METHOD me->order_status
*      EXPORTING
*        iv_objnr  = ls_fphdr-objnr
*        iv_source = zcl_zabs_agri_mobile_e_dpc_ext=>c_source_taskorder "'TC'
*      IMPORTING
*        ev_var    = lv_var.
*    IF lv_var EQ 'X'.
*      CONTINUE.
*    ENDIF.

    READ TABLE lt_taskmat TRANSPORTING NO FIELDS
                          WITH KEY task = ls_fphdr-matnr
                          BINARY SEARCH.

    CHECK sy-subrc = 0.

    MOVE-CORRESPONDING ls_fphdr TO ls_entityset.
*----------------------------------------------------------------
*    JOBREGON - Begin change
*----------------------------------------------------------------
*-- In order for the oData service to not generate any errors
*   for the TIMESTAMP format in the dates, I added the following
*   functions to make proper conversions.
    CALL FUNCTION 'ADDR_CONVERT_DATE_TO_TIMESTAMP'
      EXPORTING
        iv_date      = ls_fphdr-datab
*       IV_HIGH      = ' '
      IMPORTING
        ev_timestamp = lv_tmstmp.

    ls_entityset-datab = lv_tmstmp.

    CALL FUNCTION 'ADDR_CONVERT_DATE_TO_TIMESTAMP'
      EXPORTING
        iv_date      = ls_fphdr-datbi
*       IV_HIGH      = ' '
      IMPORTING
        ev_timestamp = lv_tmstmp.

    ls_entityset-datbi = lv_tmstmp.

*----------------------------------------------------------------
*    JOBREGON - End change
*----------------------------------------------------------------


    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input          = ls_entityset-gmein
      IMPORTING
        output         = ls_entityset-gmein
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
    ls_entityset-lmnga = ls_fphdr-gamng - ls_fphdr-gwemg.

*    READ TABLE lt_defrtt INTO ls_defrtt WITH KEY tplnr_fl = ls_fphdr-tplnr_fl BINARY SEARCH.
*    IF sy-subrc = 0.
*      ls_entityset-route = ls_defrtt-route.
*    ENDIF.
    CLEAR: ls_rttrn.
    READ TABLE lt_rttrn INTO ls_rttrn WITH KEY
                                      tplnr_fl = ls_fphdr-tplnr_fl
                                      BINARY SEARCH.
    IF sy-subrc = 0.
      ls_entityset-route = ls_rttrn-route.
    ENDIF.

    READ TABLE lt_maktx INTO ls_maktx WITH KEY matnr = ls_fphdr-matnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_entityset-maktx = ls_maktx-maktx.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
      EXPORTING
        input  = ls_entityset-tplnr_fl
      IMPORTING
        output = ls_entityset-tplnr_fl.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = ls_entityset-aufnr
      IMPORTING
        output = ls_entityset-aufnr.

    ls_entityset-pernr = lv_persno.
    ls_entityset-lifnr = lv_lifnr.

    APPEND ls_entityset TO et_entityset.
    CLEAR ls_entityset.
  ENDLOOP.

ENDMETHOD.


METHOD reasonlistset_get_entityset.

  " request_header call.
  CALL METHOD me->request_header.

  get_reasonlist(
     EXPORTING
       io_tech_request_context = io_tech_request_context
     IMPORTING
       et_entityset            = et_entityset ).

*-------------------------------------------------------------
*-- Delta Token implemented for performance
*-------------------------------------------------------------

  CALL METHOD me->get_delta_token
    EXPORTING
      io_tech_request_context  = io_tech_request_context
      mr_service_document_name = mr_service_document_name
      mr_service_version       = mr_service_version
      it_entityset             = et_entityset
    IMPORTING
      es_response_context      = es_response_context.

ENDMETHOD.


METHOD request_header.

*-- Service Request/Response Headers
  DATA: lwa_header  TYPE ihttpnvp.

*    lwa_header-name = 'Accept-Encoding' ##NO_TEXT .
*    lwa_header-value = 'gzip'.
*    /iwbep/if_mgw_conv_srv_runtime~set_header( lwa_header ).

  lwa_header-name = 'Content-Encoding' ##NO_TEXT .
  lwa_header-value = 'gzip,deflate'.
  /iwbep/if_mgw_conv_srv_runtime~set_header( lwa_header ).

ENDMETHOD.


METHOD task_order_confirm.

  DATA : lt_tocnf_hdr       TYPE TABLE OF zabs_tocnf_hdr,
         lt_tocnf_itm       TYPE TABLE OF zabs_tocnf_itm,
         ls_tocnf_hdr       TYPE zabs_tocnf_hdr,
         ls_tocnf_itm       TYPE zabs_tocnf_itm,
         lt_messages        TYPE /agri/t_gprolog,
         ls_messages        TYPE /agri/s_gprolog,
         lref_msg_container TYPE REF TO /iwbep/if_message_container.

  lref_msg_container  = mo_context->get_message_container( ).

  LOOP AT it_fpfp_cnf INTO DATA(ls_fpfp_cnf).
    ls_tocnf_hdr-ernam  = ls_tocnf_hdr-mobusr = sy-uname.
    ls_tocnf_hdr-erdat  = sy-datum.
    ls_tocnf_hdr-erzet  = sy-uzeit.
    ls_tocnf_hdr-werks  = is_operation-werks.
    MOVE-CORRESPONDING ls_fpfp_cnf TO ls_tocnf_hdr.
    APPEND ls_tocnf_hdr TO lt_tocnf_hdr.
    CLEAR : ls_tocnf_hdr.
  ENDLOOP.

  LOOP AT it_fpcom INTO DATA(ls_fpcom).
    MOVE-CORRESPONDING ls_fpcom TO ls_tocnf_itm.
    APPEND ls_tocnf_itm TO lt_tocnf_itm.
    CLEAR : ls_tocnf_itm.
  ENDLOOP.

  DATA : lv_subrc TYPE sy-subrc.

  IF lt_tocnf_hdr IS NOT INITIAL AND
     lt_tocnf_itm IS NOT INITIAL.
    INSERT zabs_tocnf_hdr FROM TABLE lt_tocnf_hdr.
    COMMIT WORK.
    lv_subrc = sy-subrc.
    INSERT zabs_tocnf_itm FROM TABLE lt_tocnf_itm.
    COMMIT WORK.

    IF sy-subrc IS INITIAL AND lv_subrc IS INITIAL.
      ls_messages-msgty = zcl_abs_abap_maintain=>c_msgty_success. "'S''S'.
      ls_messages-msgv1 = TEXT-003.                         "#EC NOTEXT
      ls_messages-msgid = '00'.
      ls_messages-msgno = '208'.
      APPEND ls_messages TO lt_messages.
      exception_messages( EXPORTING it_messages = lt_messages ).
    ELSE.
      ls_messages-msgty = zcl_abs_abap_maintain=>c_msgty_error. "'E'.
      ls_messages-msgv1 = TEXT-004.                         "#EC NOTEXT
      ls_messages-msgid = '00'.
      ls_messages-msgno = '208'.
      APPEND ls_messages TO lt_messages.
      exception_messages( EXPORTING it_messages = lt_messages ).
    ENDIF.

  ENDIF.

ENDMETHOD.


METHOD terrainset_get_entityset.
*----------------------------------------------------------------------*
* General Information
*----------------------------------------------------------------------*
* WRICEF ID    : Issue # 14
* Class        : ZCL_ABS_AGRI_MOBILE_E_DPC_EXT
* Method       : TERRAINSET_EXT_GET_ENTITYSET
* Company      : Vistex
* Dev. Author  : John Anthony Obregon Lanas
* Create Date  : 10.04.2020
* Description  : New Entity Set that filters terrains with task orders
*                associated.
*----------------------------------------------------------------------*
* Modification log
*----------------------------------------------------------------------*
* Date         | TR#         | Author          | Modification
*----------------------------------------------------------------------*
* 10.04.2020     C4DK909655    T_A.Harshe        Creation
*----------------------------------------------------------------------*

  CONSTANTS lc_memgntrr(7) TYPE c VALUE 'ZZGNTRR'.
  DATA: lv_totrec TYPE i.

  " request_header call.
  CALL METHOD me->request_header.

  IMPORT: lt_dummy TO et_entityset FROM DATABASE indx(id) ID lc_memgntrr.

  IF sy-subrc <> 0.

    get_terrainset(
       EXPORTING
         io_tech_request_context = io_tech_request_context
       IMPORTING
         et_entityset            = et_entityset ).

    IF et_entityset IS NOT INITIAL.
      EXPORT lt_dummy FROM et_entityset TO DATABASE indx(id) ID lc_memgntrr.
    ENDIF.

  ENDIF.


*-------------------------------------------------------------
*-- Delta Token implemented for performance
*-------------------------------------------------------------

*  CALL METHOD me->get_delta_token
*    EXPORTING
*      io_tech_request_context  = io_tech_request_context
*      mr_service_document_name = mr_service_document_name
*      mr_service_version       = mr_service_version
*      it_entityset             = et_entityset
*    IMPORTING
*      es_response_context      = es_response_context.

*-------------------------------------------------------------
*-- Skip Token implemented for performance
*-------------------------------------------------------------

  CALL METHOD me->get_skip_token
    EXPORTING
      io_tech_request_context = io_tech_request_context
    IMPORTING
      es_response_context     = es_response_context
      ev_totrecords           = lv_totrec
    CHANGING
      ct_entityset            = et_entityset.

  IF et_entityset IS INITIAL
      OR lines( et_entityset ) <= lv_totrec.
*    DELETE FROM DATABASE: indx(id) ID lc_memgntrr.

    CLEAR: es_response_context-skiptoken,
           es_response_context-inlinecount.

    CALL METHOD me->delete_from_database
      EXPORTING
        tabname          = 'INDX'
        client           = sy-mandt
        area             = 'ID'
        id               = lc_memgntrr
*       generic_key      = ABAP_FALSE
        client_specified = abap_false.

  ENDIF.

ENDMETHOD.


METHOD wocomponentsset_get_entityset.
*----------------------------------------------------------------------*
* General Information
*----------------------------------------------------------------------*
* WRICEF ID    : Issue # 16
* Class        : ZCL_ABS_AGRI_MOBILE_E_DPC_EXT
* Method       : WOCOMPONEXT_SET_GET_ENTITYSET
* Company      : Vistex
* Dev. Author  : John Anthony Obregon Lanas
* Create Date  : 16.04.2020
* Description  : New EntitySet created to include component dosage.
*----------------------------------------------------------------------*
* Modification log
*----------------------------------------------------------------------*
* Date         | TR#         | Author          | Modification
*----------------------------------------------------------------------*
* 16.04.2020     C4DK909655    T_A.Harshe        Creation
*----------------------------------------------------------------------*

  CONSTANTS lc_memwocom(7) TYPE c VALUE 'ZZWOCOM'.
  DATA: lv_totrec TYPE i.

  " request_header call.
  CALL METHOD me->request_header.

  IMPORT: lt_dummy TO et_entityset FROM DATABASE indx(id) ID lc_memwocom.

  IF sy-subrc <> 0.

    get_wocomponent(
       EXPORTING
         io_tech_request_context = io_tech_request_context
       IMPORTING
         et_entityset            = et_entityset ).

    IF et_entityset IS NOT INITIAL.
      EXPORT lt_dummy FROM et_entityset TO DATABASE indx(id) ID lc_memwocom.
    ENDIF.

  ENDIF.

*-------------------------------------------------------------
*-- Delta Token implemented for performance
*-------------------------------------------------------------

*  CALL METHOD me->get_delta_token
*    EXPORTING
*      io_tech_request_context  = io_tech_request_context
*      mr_service_document_name = mr_service_document_name
*      mr_service_version       = mr_service_version
*      it_entityset             = et_entityset
*    IMPORTING
*      es_response_context      = es_response_context.

*-------------------------------------------------------------
*-- Skip Token implemented for performance
*-------------------------------------------------------------

  CALL METHOD me->get_skip_token
    EXPORTING
      io_tech_request_context = io_tech_request_context
    IMPORTING
      es_response_context     = es_response_context
      ev_totrecords           = lv_totrec
    CHANGING
      ct_entityset            = et_entityset.

  IF et_entityset IS INITIAL
     OR lines( et_entityset ) <= lv_totrec.
*    DELETE FROM DATABASE: indx(id) ID lc_memwocom.

    CLEAR: es_response_context-skiptoken,
           es_response_context-inlinecount.

    CALL METHOD me->delete_from_database
      EXPORTING
        tabname          = 'INDX'
        client           = sy-mandt
        area             = 'ID'
        id               = lc_memwocom
*       generic_key      = ABAP_FALSE
        client_specified = abap_false.
  ENDIF.

ENDMETHOD.


METHOD womaterialset_get_entityset.
*----------------------------------------------------------------------*
* General Information
*----------------------------------------------------------------------*
* WRICEF ID    : Issue # 14
* Class        : ZCL_ABS_AGRI_MOBILE_E_DPC_EXT
* Method       : WOMATERIAL_EXT_GET_ENTITYSET
* Company      : Vistex
* Dev. Author  : John Anthony Obregon Lanas
* Create Date  : 05.03.2020
* Description  : New Entity Set that enhances logic to assign TANK flag
*                so the Front End knows which materials are set for
*                Tank Confirmation,
*----------------------------------------------------------------------*
* Modification log
*----------------------------------------------------------------------*
* Date         | TR#         | Author          | Modification
*----------------------------------------------------------------------*
* 05.03.2020     C4DK909655    T_A.Harshe        Creation
*----------------------------------------------------------------------*

  CONSTANTS lc_memwomat(7) TYPE c VALUE 'ZZWOMAT'.

  " request_header call.
  CALL METHOD me->request_header.

  IMPORT: lt_dummy TO et_entityset FROM DATABASE indx(id) ID lc_memwomat.

  DATA: lv_totrec TYPE i.

  IF sy-subrc <> 0.

    get_womaterial(
        EXPORTING
          io_tech_request_context = io_tech_request_context
        IMPORTING
          et_entityset            = et_entityset ).

    IF et_entityset IS NOT INITIAL.
      EXPORT lt_dummy FROM et_entityset TO DATABASE indx(id) ID lc_memwomat.
    ENDIF.

  ENDIF.

*-------------------------------------------------------------
*-- Delta Token implemented for performance
*-------------------------------------------------------------

*  CALL METHOD me->get_delta_token
*    EXPORTING
*      io_tech_request_context  = io_tech_request_context
*      mr_service_document_name = mr_service_document_name
*      mr_service_version       = mr_service_version
*      it_entityset             = et_entityset
*    IMPORTING
*      es_response_context      = es_response_context.

*-------------------------------------------------------------
*-- Skip Token implemented for performance
*-------------------------------------------------------------

  CALL METHOD me->get_skip_token
    EXPORTING
      io_tech_request_context = io_tech_request_context
    IMPORTING
      es_response_context     = es_response_context
      ev_totrecords           = lv_totrec
    CHANGING
      ct_entityset            = et_entityset.

  IF et_entityset IS INITIAL
    OR lines( et_entityset ) <= lv_totrec.
*    DELETE FROM DATABASE: indx(id) ID lc_memwomat.

    CLEAR: es_response_context-skiptoken,
           es_response_context-inlinecount.

    CALL METHOD me->delete_from_database
      EXPORTING
        tabname          = 'INDX'
        client           = sy-mandt
        area             = 'ID'
        id               = lc_memwomat
*       generic_key      = ABAP_FALSE
        client_specified = abap_false.

  ENDIF.

ENDMETHOD.


METHOD wooperationsset_get_entityset.
*----------------------------------------------------------------------*
* General Information
*----------------------------------------------------------------------*
* WRICEF ID    : Issue # 14
* Class        : ZCL_ABS_AGRI_MOBILE_E_DPC_EXT
* Method       : WOOPERATIONS_EXT_GET_ENTITYSET
* Company      : Vistex
* Dev. Author  : John Anthony Obregon Lanas
* Create Date  : 05.03.2020
* Description  : New Entity Set that enables tank confirmation.
*----------------------------------------------------------------------*
* Modification log
*----------------------------------------------------------------------*
* Date         | TR#         | Author          | Modification
*----------------------------------------------------------------------*
* 05.03.2020     C4DK909655    T_A.Harshe        Creation
*----------------------------------------------------------------------*

  CONSTANTS lc_memwoope(7) TYPE c VALUE 'ZZWOOPE'.
  DATA: lv_totrec TYPE i.

  " request_header call.
  CALL METHOD me->request_header.

  IMPORT: lt_dummy TO et_entityset FROM DATABASE indx(id) ID lc_memwoope.

  IF sy-subrc <> 0.

    get_wooperations(
      EXPORTING
        io_tech_request_context = io_tech_request_context
      IMPORTING
        et_entityset            = et_entityset ).

    IF et_entityset IS NOT INITIAL.
      EXPORT lt_dummy FROM et_entityset TO DATABASE indx(id) ID lc_memwoope.
    ENDIF.

  ENDIF.

*-------------------------------------------------------------
*-- Delta Token implemented for performance
*-------------------------------------------------------------

*  CALL METHOD me->get_delta_token
*    EXPORTING
*      io_tech_request_context  = io_tech_request_context
*      mr_service_document_name = mr_service_document_name
*      mr_service_version       = mr_service_version
*      it_entityset             = et_entityset
*    IMPORTING
*      es_response_context      = es_response_context.

*-------------------------------------------------------------
*-- Skip Token implemented for performance
*-------------------------------------------------------------

  CALL METHOD me->get_skip_token
    EXPORTING
      io_tech_request_context = io_tech_request_context
    IMPORTING
      es_response_context     = es_response_context
      ev_totrecords           = lv_totrec
    CHANGING
      ct_entityset            = et_entityset.

  IF et_entityset IS INITIAL
    OR lines( et_entityset ) <= lv_totrec.
*    DELETE FROM DATABASE: indx(id) ID lc_memworot.

    CLEAR: es_response_context-skiptoken,
           es_response_context-inlinecount.

    CALL METHOD me->delete_from_database
      EXPORTING
        tabname          = 'INDX'
        client           = sy-mandt
        area             = 'ID'
        id               = lc_memwoope
*       generic_key      = ABAP_FALSE
        client_specified = abap_false.
  ENDIF.

ENDMETHOD.


METHOD worouteset_get_entityset.
*
*  DATA:
*        lt_defrtt TYPE TABLE OF /agri/glrthdrt,"/agri/gldefrtt,
*        ls_defrtt TYPE /agri/glrthdrt,"/agri/gldefrtt,
*        ls_entityset LIKE LINE OF et_entityset,
*         lt_rtusr     TYPE TABLE OF /AGRI/GLRTUSR.
*
*ENHANCEMENT-POINT /agri/ep_mobile_odata_36 SPOTS /agri/es_gl_mobile_odata .
*
*SELECT * FROM /AGRI/GLRTUSR
*       INTO TABLE lt_rtusr
*       WHERE bname eq sy-uname.
*
*  IF lt_rtusr IS NOT INITIAL.
*  SELECT * FROM /agri/glrthdrt"/agri/gldefrtt
*           INTO TABLE lt_defrtt
*    FOR ALL ENTRIES IN lt_rtusr
*           WHERE route eq lt_rtusr-route
*             and spras EQ sy-langu.
*  ENDIF.
*
*ENHANCEMENT-POINT /agri/ep_mobile_odata_37 SPOTS /agri/es_gl_mobile_odata .
**$*$-Start: /AGRI/EP_MOBILE_ODATA_37------------------------------------------------------------$*$*
*ENHANCEMENT 6  ZABS_ENH_IMP_MOBILE_ODATA.    "active version
  DATA:
    lt_defrtt    TYPE TABLE OF /agri/glrthdrt, "/agri/gldefrtt,
    ls_defrtt    TYPE /agri/glrthdrt, "/agri/gldefrtt,
    ls_entityset LIKE LINE OF et_entityset,
*         lt_rtusr     TYPE TABLE OF /agri/glrtusr,
    lrt_pernr    TYPE RANGE OF persno,
    lrt_lifnr    TYPE RANGE OF lifnr,
    lr_pernr     LIKE LINE OF lrt_pernr,
    lv_persno    TYPE persno,
    lr_lifnr     LIKE LINE OF lrt_lifnr,
    lv_lifnr     TYPE lifnr,
    lt_rtusr     TYPE TABLE OF zabs_usrpernr,
    ls_rtusr     TYPE zabs_usrpernr.

  DATA : lt_filter     TYPE /iwbep/t_mgw_select_option,
         ls_filter     TYPE /iwbep/s_mgw_select_option,
         lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
         lv_filter_str TYPE string.

  " request_header call.
  CALL METHOD me->request_header.

  lo_filter     = io_tech_request_context->get_filter( ).
  lt_filter     = lo_filter->get_filter_select_options( ).
  lv_filter_str = lo_filter->get_filter_string( ).

* Maps filter table lines to function module parameters
  LOOP AT lt_filter INTO ls_filter.

    CASE ls_filter-property.
      WHEN 'PERNR'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_pernr ).
        READ TABLE lrt_pernr INTO lr_pernr INDEX 1.
        IF sy-subrc EQ 0.
          lv_persno = lr_pernr-low.
        ENDIF.
      WHEN 'LIFNR'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_lifnr ).
        READ TABLE lrt_lifnr INTO lr_lifnr INDEX 1.
        IF sy-subrc EQ 0.
          lv_lifnr = lr_lifnr-low.
        ENDIF.
      WHEN OTHERS.
        " Log message in the application log
        me->/iwbep/if_sb_dpc_comm_services~log_message(
          EXPORTING
            iv_msg_type   = 'E'
            iv_msg_id     = '/IWBEP/MC_SB_DPC_ADM'
            iv_msg_number = 020
            iv_msg_v1     = ls_filter-property ).
        " Raise Exception
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
          EXPORTING
            textid = /iwbep/cx_mgw_tech_exception=>internal_error.
    ENDCASE.
  ENDLOOP.
*SELECT * FROM /agri/glrtusr
*       INTO TABLE lt_rtusr
*       WHERE bname EQ sy-uname.

  "change -> Login User based validation to User+Employee based valiadtion
  IF lrt_pernr IS NOT INITIAL.
    SELECT *
    FROM zabs_usrpernr
    INTO TABLE @lt_rtusr
    WHERE pernr IN @lrt_pernr.
  ELSEIF lrt_lifnr IS NOT INITIAL.
    SELECT *
    FROM zabs_usrpernr
    INTO TABLE @lt_rtusr
    WHERE lifnr IN @lrt_lifnr.
  ENDIF.

  IF lt_rtusr IS NOT INITIAL.
    SELECT *
      FROM /agri/glrthdrt"/agri/gldefrtt
      INTO TABLE @lt_defrtt
       FOR ALL ENTRIES IN @lt_rtusr                "#EC CI_NO_TRANSFORM
     WHERE route EQ @lt_rtusr-route
       AND spras EQ @sy-langu.
  ENDIF.


  LOOP AT lt_rtusr INTO ls_rtusr.
    MOVE-CORRESPONDING ls_rtusr TO ls_entityset.
*    ls_entityset-route = ls_rtusr-route.
*    ls_entityset-pernr = ls_rtusr-pernr.
    READ TABLE lt_defrtt INTO ls_defrtt WITH KEY route = ls_rtusr-route
                                                 spras = sy-langu.
    IF sy-subrc EQ 0.
      ls_entityset-descr = ls_defrtt-descr.
    ENDIF.
*    MOVE-CORRESPONDING ls_defrtt TO ls_entityset.
    APPEND ls_entityset TO et_entityset.
    CLEAR ls_entityset.
  ENDLOOP.

*** Getting Delta token
  CALL METHOD me->get_delta_token
    EXPORTING
      io_tech_request_context  = io_tech_request_context
      mr_service_document_name = mr_service_document_name
      mr_service_version       = mr_service_version
      it_entityset             = et_entityset
    IMPORTING
      es_response_context      = es_response_context.

*  exit.
*ENDENHANCEMENT.
*$*$-End:   /AGRI/EP_MOBILE_ODATA_37------------------------------------------------------------$*$*
*
*  LOOP AT lt_defrtt INTO ls_defrtt.
*    MOVE-CORRESPONDING ls_defrtt TO ls_entityset.
*    APPEND ls_entityset TO et_entityset.
*    CLEAR ls_entityset.
*  ENDLOOP.
*
*CALL METHOD me->get_workorder_route
*  EXPORTING
*    io_tech_request_context = io_tech_request_context
*  IMPORTING
*    et_entityset            = et_entityset.
*
**** Getting Delta token
*  CALL METHOD me->get_delta_token
*    EXPORTING
*      io_tech_request_context  = io_tech_request_context
*      mr_service_document_name = mr_service_document_name
*      mr_service_version       = mr_service_version
*      it_entityset             = et_entityset
*    IMPORTING
*      es_response_context      = es_response_context.

*  DATA:lv_delta_token     TYPE string,
*       lo_dp_facade       TYPE REF TO /iwbep/if_mgw_dp_facade,
*       lo_dp_facade_1     TYPE REF TO /iwbep/if_mgw_dp_fw_facade.
*  FIELD-SYMBOLS:<et_entityset> TYPE ANY TABLE.
*  DATA:lv_format TYPE string.
*
** get the data provider facade
*  TRY.
*      lo_dp_facade = /iwbep/if_mgw_conv_srv_runtime~get_dp_facade( ).
*    CATCH /iwbep/cx_mgw_tech_exception.
*      RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception.
*  ENDTRY.
*
** call the delta token functionality
*  IF lo_dp_facade IS BOUND.
*    lo_dp_facade_1 ?= lo_dp_facade.
*    CALL METHOD lo_dp_facade_1->get_format
*      RECEIVING
*        rv_format = lv_format.
*    IF lv_format NE 'json'.
*      TRY.
*          CALL METHOD /iwbep/cl_query_result_log=>create_update_log_entry_hash
*            EXPORTING
*              io_tech_request_context  = io_tech_request_context
*              io_dp_facade             = lo_dp_facade
*              ir_service_document_name = mr_service_document_name
*              ir_service_version       = mr_service_version
*              it_entityset             = et_entityset
*            CHANGING
*              ev_delta_token           = lv_delta_token.
***                  es_response_context-deltatoken = lv_delta_token.
*        CATCH /iwbep/cx_qrl_locked.
*          RAISE EXCEPTION TYPE /iwbep/cx_qrl_locked.
*        CATCH /iwbep/cx_qrl_delta_unavailabl.
*          RAISE EXCEPTION TYPE /iwbep/cx_qrl_delta_unavailabl.
*      ENDTRY.
** export the delta token
*      es_response_context-deltatoken = lv_delta_token.
*    ENDIF.
*  ENDIF.

ENDMETHOD.


METHOD worouteterrainse_get_entityset.

  DATA: lv_totrec TYPE i.

  CONSTANTS lc_memworot(7) TYPE c VALUE 'ZZWOROT'.

  " request_header call.
  CALL METHOD me->request_header.

  IMPORT: lt_dummy TO et_entityset FROM DATABASE indx(id) ID lc_memworot.

  IF sy-subrc <> 0.

    get_route_terrain_dtls(
      EXPORTING
        io_tech_request_context = io_tech_request_context
      IMPORTING
        et_entityset            = et_entityset ).

    IF et_entityset IS NOT INITIAL.
      EXPORT lt_dummy FROM et_entityset TO DATABASE indx(id) ID lc_memworot.
    ENDIF.

  ENDIF.

*** Getting Delta token
*  get_delta_token_ext(
*    EXPORTING
*      io_tech_request_context  = io_tech_request_context
*      mr_service_document_name = mr_service_document_name
*      mr_service_version       = mr_service_version
*      it_entityset             = et_entityset
*    IMPORTING
*      es_response_context      = es_response_context ).

*-------------------------------------------------------------
*-- Skip Token implemented for performance
*-------------------------------------------------------------

  CALL METHOD me->get_skip_token
    EXPORTING
      io_tech_request_context = io_tech_request_context
    IMPORTING
      es_response_context     = es_response_context
      ev_totrecords           = lv_totrec
    CHANGING
      ct_entityset            = et_entityset.


  IF et_entityset IS INITIAL
    OR lines( et_entityset ) <= lv_totrec.
*    DELETE FROM DATABASE: indx(id) ID lc_memworot.

    CLEAR: es_response_context-skiptoken,
           es_response_context-inlinecount.

    CALL METHOD me->delete_from_database
      EXPORTING
        tabname          = 'INDX'
        client           = sy-mandt
        area             = 'ID'
        id               = lc_memworot
*       generic_key      = ABAP_FALSE
        client_specified = abap_false.
  ENDIF.

ENDMETHOD.


METHOD wotaskordersset_get_entityset.
*----------------------------------------------------------------------*
* General Information
*----------------------------------------------------------------------*
* WRICEF ID    : Document of standard app isuues
* Class        : ZCL_ABS_AGRI_MOBILE_E_DPC_EXT
* Method       : WOTASKORDERS_EXT_GET_ENTITYSET
* Company      : Vistex
* Dev. Author  : John Anthony Obregon Lanas
* Create Date  : 12.03.2020
* Description  : New Entity Set that displays all types of task orders.
*----------------------------------------------------------------------*
* Modification log
*----------------------------------------------------------------------*
* Date         | TR#         | Author          | Modification
*----------------------------------------------------------------------*
* 12.03.2020     C4DK909655    T_A.Harshe        Creation
*----------------------------------------------------------------------*

  CONSTANTS lc_memwotsk(7) TYPE c VALUE 'ZZWOTSK'.
  DATA: lv_totrec TYPE i.

  " request_header call.
  CALL METHOD me->request_header.

  IMPORT: lt_dummy TO et_entityset FROM DATABASE indx(id) ID lc_memwotsk.

  IF sy-subrc <> 0.
    get_wotaskorders(
       EXPORTING
         io_tech_request_context = io_tech_request_context
       IMPORTING
         et_entityset            = et_entityset ).
    IF et_entityset IS NOT INITIAL.
      EXPORT lt_dummy FROM et_entityset TO DATABASE indx(id) ID lc_memwotsk.
    ENDIF.

  ENDIF.

*-------------------------------------------------------------
*-- Delta Token implemented for performance
*-------------------------------------------------------------

*  CALL METHOD me->get_delta_token
*    EXPORTING
*      io_tech_request_context  = io_tech_request_context
*      mr_service_document_name = mr_service_document_name
*      mr_service_version       = mr_service_version
*      it_entityset             = et_entityset
*    IMPORTING
*      es_response_context      = es_response_context.

*-------------------------------------------------------------
*-- Skip Token implemented for performance
*-------------------------------------------------------------

  CALL METHOD me->get_skip_token
    EXPORTING
      io_tech_request_context = io_tech_request_context
    IMPORTING
      es_response_context     = es_response_context
      ev_totrecords           = lv_totrec
    CHANGING
      ct_entityset            = et_entityset.

  IF et_entityset IS INITIAL
    OR lines( et_entityset ) <= lv_totrec.
*    DELETE FROM DATABASE: indx(id) ID lc_memwotsk.

    CLEAR: es_response_context-skiptoken,
           es_response_context-inlinecount.

    CALL METHOD me->delete_from_database
      EXPORTING
        tabname          = 'INDX'
        client           = sy-mandt
        area             = 'ID'
        id               = lc_memwotsk
*       generic_key      = ABAP_FALSE
        client_specified = abap_false.
  ENDIF.

ENDMETHOD.
ENDCLASS.
