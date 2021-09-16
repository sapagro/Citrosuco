class ZCL_ZABS_GLTR_GIS_EXT_DPC_EXT definition
  public
  inheriting from ZCL_ZABS_GLTR_GIS_EXT_DPC
  create public .

public section.

  data:
    mr_datum TYPE RANGE OF datum .
  data:
    BEGIN OF gwa_glmd_data.
        INCLUDE TYPE zcl_zabs_gltr_gis_ext_mpc=>ts_measurementdocumentheader.
    DATA: measurementattributesset TYPE zcl_zabs_gltr_gis_ext_mpc=>tt_measurementattributes,
          END OF gwa_glmd_data .
  constants:
*-- Constants: Order Status
    BEGIN OF gc_order_status,
        cnf    TYPE j_txt04 VALUE 'CNF',                    "#EC NOTEXT
        pcnf   TYPE j_txt04 VALUE 'PCNF',                   "#EC NOTEXT
        closed TYPE j_txt04 VALUE 'CLSD',                   "#EC NOTEXT
        delete TYPE j_txt04 VALUE 'DLFL',                   "#EC NOTEXT
        techo  TYPE j_txt04 VALUE 'TECO',                   "#EC NOTEXT
      END OF gc_order_status .
  constants:
    BEGIN OF gc_application,
        nursery TYPE /agri/glab_class VALUE '0',            "#EC NOTEXT
        farming TYPE /agri/glab_class VALUE '1',            "#EC NOTEXT
        grower  TYPE /agri/glab_class VALUE '2',            "#EC NOTEXT
      END OF gc_application .
  constants:
    BEGIN OF gc_document_category,
        process_order   TYPE /agri/gl_autyp VALUE 'AO',     "#EC NOTEXT
        task_order      TYPE /agri/gl_autyp VALUE 'TO',     "#EC NOTEXT
        produce_reciept TYPE /agri/gl_autyp VALUE 'PR',     "#EC NOTEXT
        work_order      TYPE /agri/gl_autyp VALUE 'WO',     "#EC NOTEXT
        purchase_order  TYPE /agri/gl_autyp VALUE 'PO',     "#EC NOTEXT
        confirmation    TYPE /agri/gl_autyp VALUE 'OC',     "#EC NOTEXT
        reversals       TYPE /agri/gl_autyp VALUE 'CR',     "#EC NOTEXT
      END OF gc_document_category .
  constants:
    BEGIN OF gc_measurement_level,
        terrain      TYPE /agri/glaslvl VALUE 'T',
        crop_seasons TYPE /agri/glaslvl VALUE 'A',
        harvesting   TYPE /agri/glaslvl VALUE 'H',
        irrigation   TYPE /agri/glaslvl VALUE 'I',
      END OF gc_measurement_level .
  constants:
    BEGIN OF gc_agtyp,
        terrain      TYPE /agri/gagtyp VALUE '003',
        measurements TYPE /agri/gagtyp VALUE 'X90',
      END OF gc_agtyp .
  constants:
    BEGIN OF gc_attr_property,
        display_only TYPE /agri/gaprop VALUE '1',
        hide         TYPE /agri/gaprop VALUE '2',
        required     TYPE /agri/gaprop VALUE '3',
      END OF gc_attr_property .

  methods ADD_HEADERS .
  methods ADD_MESSAGES_TO_MSG_CONTAINER
    importing
      !IV_ENTITY_NAME type STRING optional
      !IV_OPERATION type STRING optional
      !IV_MSG_TEXT type BAPI_MSG optional
      !IS_MESSAGE type /AGRI/S_GPROLOG optional
      !IT_MESSAGES type /AGRI/T_GPROLOG optional
      !IT_BAPI_MESSAGES type BAPIRET2_T optional .
  methods ORDER_STATUS_CHECK
    importing
      !IV_AUFNR type AUFNR
      !IV_OBJNR type J_OBJNR
    exporting
      !EV_STTXT type J_STEXT
      !EV_CSTAT type /AGRI/FMCSTAT
      !EV_SUBRC type SY-SUBRC .
  methods GET_DATE_RANGES
    importing
      !IV_DATAB type DATAB
      !IV_DATBI type DATBI
    exporting
      !ER_DATAB like MR_DATUM
      !ER_DATBI like MR_DATUM .

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_BEGIN
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_PROCESS
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITYSET
    redefinition .
protected section.

  methods F4CROPVARIANTSSE_GET_ENTITYSET
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION
      !IS_PAGING type /IWBEP/S_MGW_PAGING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
      !IT_ORDER type /IWBEP/T_MGW_SORTING_ORDER
      !IV_FILTER_STRING type STRING
      !IV_SEARCH_STRING type STRING
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
    exporting
      !ET_ENTITYSET type ZCL_ZABS_GLTR_GIS_EXT_MPC=>TT_F4CROPVARIANTS
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods F4CROPPROCESSSET_GET_ENTITYSET
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION
      !IS_PAGING type /IWBEP/S_MGW_PAGING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
      !IT_ORDER type /IWBEP/T_MGW_SORTING_ORDER
      !IV_FILTER_STRING type STRING
      !IV_SEARCH_STRING type STRING
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_ENTITYSET type ZCL_ZABS_GLTR_GIS_EXT_MPC=>TT_F4CROPPROCESS
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods F4WORKORDERTYPES_GET_ENTITYSET
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION
      !IS_PAGING type /IWBEP/S_MGW_PAGING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
      !IT_ORDER type /IWBEP/T_MGW_SORTING_ORDER
      !IV_FILTER_STRING type STRING
      !IV_SEARCH_STRING type STRING
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_ENTITYSET type ZCL_ZABS_GLTR_GIS_EXT_MPC=>TT_F4WORKORDERTYPES
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods TASKORDERANALYTI_GET_ENTITYSET
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION
      !IS_PAGING type /IWBEP/S_MGW_PAGING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
      !IT_ORDER type /IWBEP/T_MGW_SORTING_ORDER
      !IV_FILTER_STRING type STRING
      !IV_SEARCH_STRING type STRING
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_ENTITYSET type ZCL_ZABS_GLTR_GIS_EXT_MPC=>TT_TASKORDERANALYTICS
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods CATMASTERDATASET_GET_ENTITYSET
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION
      !IS_PAGING type /IWBEP/S_MGW_PAGING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
      !IT_ORDER type /IWBEP/T_MGW_SORTING_ORDER
      !IV_FILTER_STRING type STRING
      !IV_SEARCH_STRING type STRING
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_ENTITYSET type ZCL_ZABS_GLTR_GIS_EXT_MPC=>TT_CATMASTERDATA
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods CATMASTER1SET_GET_ENTITYSET
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION
      !IS_PAGING type /IWBEP/S_MGW_PAGING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
      !IT_ORDER type /IWBEP/T_MGW_SORTING_ORDER
      !IV_FILTER_STRING type STRING
      !IV_SEARCH_STRING type STRING
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
    exporting
      !ET_ENTITYSET type ZCL_ZABS_GLTR_GIS_EXT_MPC=>TT_CATMASTER1
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods MEASUREMENTTYPES_GET_ENTITYSET
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION
      !IS_PAGING type /IWBEP/S_MGW_PAGING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
      !IT_ORDER type /IWBEP/T_MGW_SORTING_ORDER
      !IV_FILTER_STRING type STRING
      !IV_SEARCH_STRING type STRING
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_ENTITYSET type ZCL_ZABS_GLTR_GIS_EXT_MPC=>TT_MEASUREMENTTYPES
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods MEASUREMENTGROUP_GET_ENTITYSET
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION
      !IS_PAGING type /IWBEP/S_MGW_PAGING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
      !IT_ORDER type /IWBEP/T_MGW_SORTING_ORDER
      !IV_FILTER_STRING type STRING
      !IV_SEARCH_STRING type STRING
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_ENTITYSET type ZCL_ZABS_GLTR_GIS_EXT_MPC=>TT_MEASUREMENTGROUP
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods MEASUREMENTDOCUM_GET_ENTITYSET
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION
      !IS_PAGING type /IWBEP/S_MGW_PAGING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
      !IT_ORDER type /IWBEP/T_MGW_SORTING_ORDER
      !IV_FILTER_STRING type STRING
      !IV_SEARCH_STRING type STRING
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_ENTITYSET type ZCL_ZABS_GLTR_GIS_EXT_MPC=>TT_MEASUREMENTDOCUMENTHEADER
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods MEASUREMENTATTRI_GET_ENTITYSET
    importing
      !IV_ENTITY_NAME type STRING
      !IV_ENTITY_SET_NAME type STRING
      !IV_SOURCE_NAME type STRING
      !IT_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION
      !IS_PAGING type /IWBEP/S_MGW_PAGING
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH
      !IT_ORDER type /IWBEP/T_MGW_SORTING_ORDER
      !IV_FILTER_STRING type STRING
      !IV_SEARCH_STRING type STRING
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
    exporting
      !ET_ENTITYSET type ZCL_ZABS_GLTR_GIS_EXT_MPC=>TT_MEASUREMENTATTRIBUTES
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
private section.

  methods GET_ORDER_CONFIRMATIONS_DATA
    importing
      !I_DETAILS type XFLD optional
      !IT_FMOCNUM type /AGRI/T_FMOCNUM
    exporting
      !ET_FMOC_DATA type /AGRI/T_FMOC_DETAILS .
  methods ATTRIBUTES_DATA_PREPARE
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
    exporting
      !ET_ENTITYSET type ZCL_ZABS_GLTR_GIS_EXT_MPC=>TT_MEASUREMENTATTRIBUTES
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods GET_ATTRIBUTE_DISPLAY_DATA
    importing
      !IS_ATHDR type /AGRI/S_GATHDR
      !IS_MDATV type /AGRI/S_GLMDATV optional
      !IT_ATHDR type /AGRI/T_GATHDR
    changing
      !CS_ATTR_VALUES type /AGRI/S_GLMDATV_FCAT .
  methods MEASUREMENTS_DATA_PROCESS
    changing
      !CS_GLMD_DATA like GWA_GLMD_DATA .
ENDCLASS.



CLASS ZCL_ZABS_GLTR_GIS_EXT_DPC_EXT IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~changeset_begin.

*-- Data Declarations
    DATA: lwa_operation_info  TYPE /iwbep/s_mgw_operation_info.

    CLEAR: cv_defer_mode.
    cv_defer_mode = abap_true.

*-- Get Entitysets to Data Process
    LOOP AT it_operation_info INTO lwa_operation_info.
      IF NOT ( lwa_operation_info-entity_set  EQ 'MeasurementDocumentHeaderSet' ).
        cv_defer_mode = abap_false.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF cv_defer_mode = abap_false.
      CALL METHOD super->/iwbep/if_mgw_appl_srv_runtime~changeset_begin
        EXPORTING
          it_operation_info = it_operation_info
        CHANGING
          cv_defer_mode     = cv_defer_mode.

    ENDIF.

  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~changeset_process.

*-- Internal Tables
    DATA: lt_messages    TYPE /agri/t_gprolog.

*-- Work Areas
    DATA: lwa_changeset_request  TYPE /iwbep/if_mgw_appl_types=>ty_s_changeset_request,
          lwa_changeset_response TYPE /iwbep/if_mgw_appl_types=>ty_s_changeset_response,
          lwa_message            TYPE /agri/s_gprolog.

*--  Class References
    DATA:lref_update_context TYPE REF TO /iwbep/if_mgw_req_entity_u.

*-- Variables
    DATA: lv_super_call  TYPE abap_bool,
          lv_entity_type TYPE string.

*--  Field Symbols
    FIELD-SYMBOLS: <lwa_data>     TYPE any.

    " Collect all the requests and call List Update
    LOOP AT it_changeset_request INTO lwa_changeset_request.
      CLEAR lv_entity_type.
      lref_update_context ?= lwa_changeset_request-request_context.
      lv_entity_type = lref_update_context->get_entity_type_name( ).
      IF NOT ( lv_entity_type EQ 'MeasurementDocumentHeader' ).
        lv_super_call = abap_true.
        EXIT.
      ENDIF.
      " Operation Number in current changeset
      lwa_changeset_response-operation_no = lwa_changeset_request-operation_no.

      CASE lv_entity_type.
        WHEN 'MeasurementDocumentHeader'.
          CLEAR:  gwa_glmd_data.
          lwa_changeset_request-entry_provider->read_entry_data( IMPORTING es_data = gwa_glmd_data ).
          " Get and convert keys
          lref_update_context->get_converted_keys( IMPORTING es_key_values = gwa_glmd_data ).

          measurements_data_process( CHANGING cs_glmd_data = gwa_glmd_data ).
          ASSIGN gwa_glmd_data TO <lwa_data>.

      ENDCASE.

      copy_data_to_ref( EXPORTING is_data = <lwa_data>
                        CHANGING  cr_data = lwa_changeset_response-entity_data ).

      INSERT lwa_changeset_response INTO TABLE ct_changeset_response.

    ENDLOOP.

    IF lv_super_call = abap_true.
      CALL METHOD super->/iwbep/if_mgw_appl_srv_runtime~changeset_process
        EXPORTING
          it_changeset_request  = it_changeset_request
        CHANGING
          ct_changeset_response = ct_changeset_response.

      RETURN.
    ENDIF.


  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~get_entityset.

    DATA: f4cropvariantsset            TYPE zcl_zabs_gltr_gis_ext_mpc=>tt_f4cropvariants,
          f4cropprocessset             TYPE zcl_zabs_gltr_gis_ext_mpc=>tt_f4cropprocess,
          f4workordertypesset          TYPE zcl_zabs_gltr_gis_ext_mpc=>tt_f4workordertypes,
          taskorderanalyticsset        TYPE zcl_zabs_gltr_gis_ext_mpc=>tt_taskorderanalytics,
          catmasterdataset             TYPE zcl_zabs_gltr_gis_ext_mpc=>tt_catmasterdata,
          catmaster1set                TYPE zcl_zabs_gltr_gis_ext_mpc=>tt_catmaster1,
          measurementtypesset          TYPE zcl_zabs_gltr_gis_ext_mpc=>tt_measurementtypes,
          measurementgroupset          TYPE zcl_zabs_gltr_gis_ext_mpc=>tt_measurementgroup,
          measurementdocumentheaderset TYPE zcl_zabs_gltr_gis_ext_mpc=>tt_measurementdocumentheader,
          measurementattributesset     TYPE zcl_zabs_gltr_gis_ext_mpc=>tt_measurementattributes.

    DATA: lv_entityset_name TYPE string.

    lv_entityset_name = io_tech_request_context->get_entity_set_name( ).

    CASE lv_entityset_name.
*-------------------------------------------------------------------------*
*             EntitySet -  F4CropVariantsSet
*-------------------------------------------------------------------------*
      WHEN 'F4CropVariantsSet'.
*--  Call the entity set generated method
        f4cropvariantsse_get_entityset(
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
             et_entityset             = f4cropvariantsset
             es_response_context      = es_response_context
         ).

*     Send specific entity data to the caller interface
        copy_data_to_ref( EXPORTING is_data = f4cropvariantsset
                          CHANGING  cr_data = er_entityset ).

*-------------------------------------------------------------------------*
*             EntitySet -  F4CropProcessSet
*-------------------------------------------------------------------------*
      WHEN 'F4CropProcessSet'.
*     Call the entity set generated method
        f4cropprocessset_get_entityset(
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
            et_entityset             = f4cropprocessset
            es_response_context      = es_response_context
         ).

*     Send specific entity data to the caller interface
        copy_data_to_ref( EXPORTING is_data = f4cropprocessset
                          CHANGING  cr_data = er_entityset ).

*-------------------------------------------------------------------------*
*             EntitySet -  F4WorkOrderTypesSet
*-------------------------------------------------------------------------*
      WHEN 'F4WorkOrderTypesSet'.
*-- Call the entity set generated method
        f4workordertypes_get_entityset(
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
            et_entityset             = f4workordertypesset
            es_response_context      = es_response_context
         ).

*--     Send specific entity data to the caller interface
        copy_data_to_ref( EXPORTING is_data = f4workordertypesset
                          CHANGING  cr_data = er_entityset ).

      WHEN 'TaskOrderAnalyticsSet'.

*-- Call the entity set generated method
        taskorderanalyti_get_entityset(
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
             et_entityset             = taskorderanalyticsset
             es_response_context      = es_response_context
          ).

*--     Send specific entity data to the caller interface
        copy_data_to_ref( EXPORTING is_data = taskorderanalyticsset
                          CHANGING  cr_data = er_entityset ).

      WHEN 'CATMasterDataSet'.

        catmasterdataset_get_entityset(
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
            et_entityset             = catmasterdataset
            es_response_context      = es_response_context
        ).

*--     Send specific entity data to the caller interface
        copy_data_to_ref( EXPORTING is_data = catmasterdataset
                          CHANGING  cr_data = er_entityset ).

      WHEN 'CatMaster1Set'.

        catmaster1set_get_entityset(
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
            et_entityset             = catmaster1set
            es_response_context      = es_response_context
        ).

*--     Send specific entity data to the caller interface
        copy_data_to_ref( EXPORTING is_data = catmaster1set
                          CHANGING  cr_data = er_entityset ).

      WHEN 'MeasurementTypesSet'.

        measurementtypes_get_entityset(
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
            et_entityset             = measurementtypesset
            es_response_context      = es_response_context
        ).

*--     Send specific entity data to the caller interface
        copy_data_to_ref( EXPORTING is_data = measurementtypesset
                          CHANGING  cr_data = er_entityset ).

      WHEN 'MeasurementGroupSet'.

        measurementgroup_get_entityset(
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
            et_entityset             = measurementgroupset
            es_response_context      = es_response_context
        ).

*--     Send specific entity data to the caller interface
        copy_data_to_ref( EXPORTING is_data = measurementgroupset
                          CHANGING  cr_data = er_entityset ).

      WHEN 'MeasurementDocumentHeaderSet'.

        measurementdocum_get_entityset(
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
            et_entityset             = measurementdocumentheaderset
            es_response_context      = es_response_context
        ).

*--     Send specific entity data to the caller interface
        copy_data_to_ref( EXPORTING is_data = measurementdocumentheaderset
                          CHANGING  cr_data = er_entityset ).

      WHEN 'MeasurementAttributesSet'.

        measurementattri_get_entityset(
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
            et_entityset             = measurementattributesset
            es_response_context      = es_response_context
        ).

*--     Send specific entity data to the caller interface
        copy_data_to_ref( EXPORTING is_data = measurementattributesset
                          CHANGING  cr_data = er_entityset ).

      WHEN OTHERS.

        super->/iwbep/if_mgw_appl_srv_runtime~get_entityset(
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
             es_response_context      = es_response_context
          ).

    ENDCASE.

  ENDMETHOD.


  method ADD_HEADERS.
  endmethod.


  METHOD add_messages_to_msg_container.

    DATA: lwa_bapi_messsages TYPE bapiret2,
          lwa_message        TYPE /agri/s_gprolog,
          lref_msg_container TYPE REF TO /iwbep/if_message_container,
          lv_msgno           TYPE symsgno.

*-- Get Message container object
    lref_msg_container = me->mo_context->get_message_container( ).

*-- From Agri Messages
*-- Single Message Processing
    IF is_message IS NOT INITIAL.
      CLEAR lv_msgno.
      lv_msgno = is_message-msgno.
      lref_msg_container->add_message(
            EXPORTING iv_msg_type    = is_message-msgty
                      iv_msg_id      = is_message-msgid
                      iv_msg_number  = lv_msgno
                      iv_msg_v1      = is_message-msgv1
                      iv_msg_v2      = is_message-msgv2
                      iv_msg_v3      = is_message-msgv3
                      iv_msg_v4      = is_message-msgv4
                      iv_msg_text    = iv_msg_text
                      iv_entity_type = iv_entity_name
                      iv_add_to_response_header = abap_true ).
    ENDIF.

*-- Multiple Messages Processing
    IF it_messages IS NOT INITIAL.
      CLEAR lwa_message.
      LOOP AT it_messages INTO lwa_message.
        CLEAR lv_msgno.
        lv_msgno = lwa_message-msgno.
        lref_msg_container->add_message(
            EXPORTING iv_msg_type    = lwa_message-msgty
                      iv_msg_id      = lwa_message-msgid
                      iv_msg_number  = lv_msgno
                      iv_msg_v1      = lwa_message-msgv1
                      iv_msg_v2      = lwa_message-msgv2
                      iv_msg_v3      = lwa_message-msgv3
                      iv_msg_v4      = lwa_message-msgv4
                      iv_entity_type = iv_entity_name
                      iv_add_to_response_header = abap_true ).
      ENDLOOP.
    ENDIF.

*-- From Bapi Messages
    IF it_bapi_messages IS NOT INITIAL.
      lref_msg_container->add_messages_from_bapi(
        it_bapi_messages          = it_bapi_messages
        iv_determine_leading_msg  = /iwbep/if_message_container=>gcs_leading_msg_search_option-first
        iv_entity_type            = iv_entity_name
        iv_add_to_response_header = abap_true ).
    ENDIF.

  ENDMETHOD.


  METHOD attributes_data_prepare.

*-- Data Declarations
*-- Internal Tables
    DATA: lt_filter      TYPE /iwbep/t_mgw_select_option,
          lt_klah        TYPE TABLE OF klah,
          lt_athdr       TYPE /agri/t_gathdr,
          lt_attr_list   TYPE /agri/t_gathdr,
          lt_cawn        TYPE /agri/t_gcawn,
          lt_attr_groups TYPE /agri/t_gatg_attr,
          lt_atvrref     TYPE /agri/t_gatvrref.

*-- Work Areas
    DATA: lwa_filter      TYPE /iwbep/s_mgw_select_option,
          lwa_sel_option  TYPE /iwbep/s_cod_select_option,
          lwa_message     TYPE /agri/s_gprolog,
          lwa_glmdhdr     TYPE /agri/s_glmdhdr,
          lwa_klah        TYPE klah,
          lwa_athdr       TYPE /agri/s_gathdr,
          lwa_attr_list   TYPE /agri/s_gathdr,
          lwa_cawn        TYPE /agri/s_gcawn,
          lwa_attr_groups TYPE /agri/s_gatg_attr,
          lwa_atvrref     TYPE /agri/s_gatvrref,
          lwa_ataoa       TYPE /agri/s_gataoa,
          lwa_attr_values TYPE /agri/s_glmdatv_fcat,
          lwa_entityset   LIKE LINE OF et_entityset,
          lref_filter     TYPE REF TO /iwbep/if_mgw_req_filter.

*-- Variables
    DATA: lv_class TYPE /agri/gatgrp.

*-- Get Filter Values
    lref_filter     = io_tech_request_context->get_filter( ).
    lt_filter       = lref_filter->get_filter_select_options( ).

*-- Add Response Headers
    add_headers( ).

*-- Maps filter table lines to function module parameters
    LOOP AT lt_filter INTO lwa_filter.
      CASE lwa_filter-property.
        WHEN 'CLASS' OR 'MPGRP'.
          CLEAR: lwa_sel_option.
          READ TABLE lwa_filter-select_options INTO lwa_sel_option INDEX 1.
          IF sy-subrc = 0.
            lwa_glmdhdr-mpgrp = lwa_sel_option-low.
          ENDIF.
        WHEN 'MDTYP'.
          CLEAR: lwa_sel_option.
          READ TABLE lwa_filter-select_options INTO lwa_sel_option INDEX 1.
          IF sy-subrc = 0.
            lwa_glmdhdr-mdtyp = lwa_sel_option-low.
          ENDIF.
        WHEN 'ASLVL'.
          CLEAR: lwa_sel_option.
          READ TABLE lwa_filter-select_options INTO lwa_sel_option INDEX 1.
          IF sy-subrc = 0.
            lwa_glmdhdr-aslvl = lwa_sel_option-low.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

    CLEAR:lv_class.
    SELECT SINGLE class FROM /agri/glagha
             INTO lv_class
            WHERE class EQ lwa_glmdhdr-mpgrp
              AND aslvl EQ lwa_glmdhdr-aslvl.
    IF sy-subrc NE 0.
      CLEAR: lwa_message.
      lwa_message-msgty = c_msg_type-error.
      lwa_message-msgid = '/AGRI/GLMD'.
      lwa_message-msgno = '052'.
      lwa_message-msgv1 = lwa_glmdhdr-mpgrp.
      add_messages_to_msg_container( EXPORTING is_message = lwa_message ).
      RETURN.
    ENDIF.

    CLEAR:lwa_klah.
    lwa_klah-class = lv_class.
    lwa_klah-klart = gc_agtyp-measurements.
    APPEND lwa_klah TO lt_klah.

    CALL METHOD /agri/cl_gattr_utils=>attribute_groups_attr_read
      EXPORTING
        it_klah    = lt_klah
        i_agtyp    = gc_agtyp-measurements
      IMPORTING
        et_atgrp   = lt_attr_groups
*       et_cattr   =
        et_athdr   = lt_athdr
        et_atvrref = lt_atvrref
        et_cawn    = lt_cawn.

    SORT: lt_athdr BY atinn.

    IF lt_athdr IS INITIAL.
      CLEAR: lwa_message.
      lwa_message-msgty = c_msg_type-error.
      lwa_message-msgid = '/AGRI/GLMD'.
      lwa_message-msgno = '022'.
      lwa_message-msgv1 = lwa_glmdhdr-mpgrp.
      add_messages_to_msg_container( EXPORTING is_message = lwa_message ).
      RETURN.
    ENDIF.

    LOOP AT lt_attr_groups INTO lwa_attr_groups.
      CLEAR:lwa_ataoa.
      LOOP AT lwa_attr_groups-ataoa INTO lwa_ataoa.
        CLEAR:lwa_athdr.
        READ TABLE lt_athdr INTO lwa_athdr
                            WITH KEY atinn = lwa_ataoa-atinn
                            BINARY SEARCH.
        CHECK sy-subrc EQ 0 AND
              lwa_athdr-atvie IS INITIAL AND
              lwa_athdr-aprop NE gc_attr_property-hide.

*-- Attribute Display Data Prepare
        get_attribute_display_data(
           EXPORTING
             is_athdr       = lwa_athdr
             it_athdr       = lt_athdr
           CHANGING
             cs_attr_values = lwa_attr_values
          ).

        MOVE-CORRESPONDING lwa_attr_values TO lwa_entityset.
        lwa_entityset-atsch = lwa_athdr-atsch.
        lwa_entityset-atfor = lwa_athdr-atfor.
        lwa_entityset-ataw1 = lwa_athdr-msehi.
        lwa_entityset-atinp = lwa_athdr-atinp.

        APPEND lwa_entityset TO et_entityset.
        CLEAR: lwa_attr_values, lwa_entityset.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD catmaster1set_get_entityset.

*-- Data Declarations
*-- Internal Tables
    DATA: lt_filter   TYPE /iwbep/t_mgw_select_option,
          lt_tplnr    TYPE RANGE OF /agri/gltplnr_fl,
          lt_matnr    TYPE RANGE OF matnr,
          lt_stort    TYPE RANGE OF /agri/glstort,
          lt_cahdr    TYPE TABLE OF /agri/fmcahdr,
          lt_glflot   TYPE TABLE OF /agri/glflot,
          lt_fphdr    TYPE TABLE OF /agri/fmfphdr,
          lt_fpcom    TYPE TABLE OF /agri/fmfpcom,
          lt_fmocnum  TYPE /agri/t_fmocnum,
          lt_fmoc     TYPE /agri/t_fmoc_details,
          lt_messages TYPE /irm/t_gprolog,
          lt_stpo     TYPE TABLE OF stpo,
          lt_mast     TYPE TABLE OF mast.

*-- Work Areas
    DATA: lwa_filter     TYPE /iwbep/s_mgw_select_option,
          lwa_sel_option TYPE /iwbep/s_cod_select_option,
          lwa_tplnr      LIKE LINE OF lt_tplnr,
          lwa_matnr      LIKE LINE OF lt_matnr,
          lo_filter      TYPE REF TO /iwbep/if_mgw_req_filter,
          lwa_entityset  LIKE LINE OF et_entityset,
          lwa_cahdr      TYPE /agri/fmcahdr,
          lwa_glflot     TYPE /agri/glflot,
          lwa_fmoc       TYPE /agri/s_fmoc_details,
          lwa_fphdr      TYPE /agri/fmfphdr,
          lwa_message    TYPE /irm/s_gprolog,
          lwa_stpo       TYPE stpo,
          lwa_mast       TYPE mast.

*-- Variables
    DATA: lv_aufnr TYPE aufnr,
          lv_matnr TYPE matnr.

    DATA: lt_header         TYPE TABLE OF bapi_order_header1,
          lt_header_temp    TYPE TABLE OF bapi_order_header1,
          lt_component      TYPE TABLE OF bapi_order_component,
          lt_component_temp TYPE TABLE OF bapi_order_component,
          lwa_objects       TYPE bapi_pp_order_objects,
          lwa_header        TYPE bapi_order_header1,
          lwa_component     TYPE bapi_order_component.

*-- Constants
    CONSTANTS: c_true         TYPE /agri/fmschdl VALUE 'X',
               c_movement_typ TYPE bwart VALUE '261',
               c_item_cat     TYPE postp VALUE 'L'.


    lo_filter     = io_tech_request_context->get_filter( ).
    lt_filter     = lo_filter->get_filter_select_options( ).

*** Maps filter table lines to function module parameters
    LOOP AT lt_filter INTO lwa_filter.
      CASE lwa_filter-property .
        WHEN  'TPLNR_FL'.
          lo_filter->convert_select_option(
            EXPORTING
              is_select_option = lwa_filter
            IMPORTING
              et_select_option = lt_tplnr ).
        WHEN 'STORT'.
          lo_filter->convert_select_option(
            EXPORTING
              is_select_option = lwa_filter
            IMPORTING
              et_select_option = lt_stort ).
        WHEN 'MATNR'.
          lo_filter->convert_select_option(
            EXPORTING
              is_select_option = lwa_filter
            IMPORTING
              et_select_option = lt_matnr ).
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

    IF lt_matnr IS INITIAL.
      lwa_message-msgty =  'E'.
      lwa_message-msgid =  '/AGRI/FMPR'.
      lwa_message-msgno =  '058'.
      add_messages_to_msg_container( EXPORTING is_message = lwa_message ).
      RETURN.
    ENDIF.

    IF lt_matnr IS NOT INITIAL.
*-- Get BOM item data
      SELECT * FROM stpo
        INTO TABLE lt_stpo
        WHERE idnrk IN lt_matnr."IN lt_matnr.

*-- Get Material to BOM Link data
      IF lt_stpo IS NOT INITIAL.
        SELECT * FROM mast
          INTO TABLE lt_mast
          FOR ALL ENTRIES IN lt_stpo
           WHERE stlnr EQ lt_stpo-stlnr.
*           AND werks EQ lt_stpo-pswrk.
      ENDIF.
*-- Get task order data
      IF lt_mast IS NOT INITIAL.
        SELECT * FROM /agri/fmfphdr
          INTO TABLE lt_fphdr
          FOR ALL ENTRIES IN lt_mast
          WHERE autyp EQ 'TO'                           "#EC CI_NOFIELD
            AND datab LE sy-datum
            AND datbi GE sy-datum
            AND matnr EQ lt_mast-matnr
            AND iwerk EQ lt_mast-werks
            AND tecom EQ space.

        IF lt_fphdr IS NOT INITIAL.
          SORT:lt_fphdr BY aufnr.
        ENDIF.
      ENDIF.
    ENDIF.

    IF lt_fphdr IS NOT INITIAL.
      SELECT ocnum FROM /agri/fmocindx
             INTO TABLE lt_fmocnum
             FOR ALL ENTRIES IN lt_fphdr
             WHERE aufnr EQ lt_fphdr-aufnr.
    ENDIF.

*** Order Confirmations Data ( Postings Data )
    IF lt_fmocnum IS NOT INITIAL.
      CALL METHOD get_order_confirmations_data
        EXPORTING
          it_fmocnum   = lt_fmocnum
        IMPORTING
          et_fmoc_data = lt_fmoc.
    ENDIF.

*** Get order Component Overview data
*** Order Objects
    CLEAR:lwa_objects,lwa_fphdr.
    lwa_objects-header     = c_true.
    lwa_objects-components = c_true.

    LOOP AT lt_fphdr INTO lwa_fphdr.
      CLEAR:lv_aufnr.
      REFRESH: lt_header_temp,lt_component_temp.
      lv_aufnr = lwa_fphdr-aufnr.

      IF lv_aufnr IS NOT INITIAL.
        CALL FUNCTION 'BAPI_PRODORD_GET_DETAIL'
          EXPORTING
            number        = lv_aufnr
            order_objects = lwa_objects
          TABLES
            header        = lt_header_temp
            component     = lt_component_temp.

        IF lt_header_temp IS NOT INITIAL.
          APPEND LINES OF lt_header_temp TO lt_header.
        ENDIF.

        IF lt_component_temp IS NOT INITIAL.
          APPEND LINES OF lt_component_temp TO lt_component.
        ENDIF.
      ENDIF.
    ENDLOOP.

    CLEAR: lwa_header,lwa_fphdr,lwa_component.
    LOOP AT lt_header INTO lwa_header.

      IF lwa_header-system_status CS 'TECO'.
        CONTINUE."" EXIT.
      ENDIF.

      lwa_entityset-aufnr  = lwa_header-order_number.
      lwa_entityset-auart  = lwa_header-order_type.
      lwa_entityset-werks  = lwa_header-plan_plant.
      lwa_entityset-tmatnr = lwa_header-material.
      lwa_entityset-matxt  = lwa_header-material_text.
      lwa_entityset-sttxt  = lwa_header-system_status.

      READ TABLE lt_fphdr INTO lwa_fphdr WITH KEY aufnr = lwa_header-order_number
                                                    BINARY SEARCH.
      IF sy-subrc = 0.
        lwa_entityset-tplnr_fl = lwa_fphdr-tplnr_fl.
        lwa_entityset-cmnum    = lwa_fphdr-cmnum.
      ENDIF.

      LOOP AT lt_component INTO lwa_component WHERE material     EQ lv_matnr
                                               AND order_number  EQ lwa_header-order_number
                                               AND movement_type EQ c_movement_typ
                                               AND item_category EQ c_item_cat.

        lwa_entityset-matnr  = lwa_component-material.
        lwa_entityset-menge  = lwa_component-req_quan.
        lwa_entityset-meins  = lwa_component-base_uom.
        lwa_entityset-maktx  = lwa_component-material_description.

        CLEAR: lwa_entityset-lmnga,lwa_entityset-gmein,lwa_fmoc.
        LOOP AT lt_fmoc INTO lwa_fmoc WHERE aufnr EQ lwa_component-order_number
                                        AND matnr EQ lwa_component-material
                                        AND bwart EQ lwa_component-movement_type.
          IF sy-subrc = 0.
            lwa_entityset-lmnga = lwa_entityset-lmnga + lwa_fmoc-menge.
            lwa_entityset-gmein = lwa_fmoc-meins.
          ENDIF.
        ENDLOOP.

        APPEND lwa_entityset TO et_entityset.
        CLEAR: lwa_component.
      ENDLOOP.
      CLEAR:lwa_header,lwa_entityset.
    ENDLOOP.

    IF et_entityset IS INITIAL.
      lwa_message-msgty =  'E'.
      lwa_message-msgid =  '/IRM/GLOBAL'.
      lwa_message-msgno =  '751'.
      APPEND lwa_message TO lt_messages.
      CLEAR:lwa_message.
    ENDIF.

    IF lt_messages IS NOT INITIAL.
      add_messages_to_msg_container( EXPORTING it_messages = lt_messages ).
    ENDIF.

  ENDMETHOD.


  METHOD catmasterdataset_get_entityset.

*-- Data Declarations
    DATA: lt_filter   TYPE /iwbep/t_mgw_select_option,
          lt_cahdr    TYPE TABLE OF /agri/fmcahdr,
          lr_matnr    TYPE RANGE OF matnr,
          lr_stort    TYPE RANGE OF /agri/glstort,
          li_messages TYPE /agri/t_gprolog.

*-- Work Areas
    DATA: lwa_entityset  LIKE LINE OF et_entityset,
          lwa_filter     TYPE /iwbep/s_mgw_select_option,
          lwa_sel_option TYPE /iwbep/s_cod_select_option,
          lwa_message    TYPE /agri/s_gprolog,
          lwa_cahdr      TYPE /agri/fmcahdr,
          lref_filter    TYPE REF TO /iwbep/if_mgw_req_filter.

*-- Variables
    DATA: lv_subrc TYPE sy-subrc.

    lref_filter     = io_tech_request_context->get_filter( ).
    lt_filter       = lref_filter->get_filter_select_options( ).

*-- Add Headers
    add_headers( ).

    LOOP AT lt_filter INTO lwa_filter.
      CASE lwa_filter-property.
        WHEN 'MATNR'.
          lref_filter->convert_select_option(
            EXPORTING
              is_select_option = lwa_filter
            IMPORTING
              et_select_option = lr_matnr ).
        WHEN 'STORT'.
          lref_filter->convert_select_option(
            EXPORTING
              is_select_option = lwa_filter
            IMPORTING
              et_select_option = lr_stort ).
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

    SELECT * FROM /agri/fmcahdr
      INTO TABLE lt_cahdr
      WHERE matnr IN lr_matnr
        AND stort IN lr_stort.

    IF lt_cahdr IS NOT INITIAL.
      SELECT stort,spras,descr
        FROM /agri/tglloct
        INTO TABLE @DATA(lt_glloct)
        FOR ALL ENTRIES IN @lt_cahdr
        WHERE stort EQ @lt_cahdr-stort
          AND spras EQ @sy-langu.

      SELECT matnr,maktx
        FROM makt
        INTO TABLE @DATA(lt_makt)
        FOR ALL ENTRIES IN @lt_cahdr
        WHERE matnr EQ @lt_cahdr-matnr
          AND spras EQ @sy-langu.

    ENDIF.

    SORT: lt_glloct BY stort,
          lt_makt   BY matnr.

    LOOP AT lt_cahdr INTO lwa_cahdr.
      MOVE-CORRESPONDING lwa_cahdr TO lwa_entityset.

      READ TABLE lt_glloct INTO DATA(lwa_glloct)
           WITH KEY stort = lwa_cahdr-stort BINARY SEARCH.
      IF sy-subrc = 0.
        lwa_entityset-descr = lwa_glloct-descr.
      ENDIF.

      READ TABLE lt_makt INTO DATA(lwa_makt)
           WITH KEY matnr = lwa_cahdr-matnr BINARY SEARCH.
      IF sy-subrc = 0.
        lwa_entityset-maktx = lwa_makt-maktx.
      ENDIF.

      APPEND lwa_entityset TO et_entityset.
      CLEAR: lwa_entityset.
    ENDLOOP.

  ENDMETHOD.


  METHOD f4cropprocessset_get_entityset.

*-- Data Declarations
*-- Internal Tables
    DATA: li_filter TYPE /iwbep/t_mgw_select_option,
          lr_tplnr  TYPE RANGE OF /agri/gltplnr_fl,
          lr_cmnum  TYPE RANGE OF /agri/glcmnum,
          lr_varia  TYPE RANGE OF /agri/glvaria,
          lr_cpros  TYPE RANGE OF /agri/glcpros.

*-- Work Areas
    DATA: lwa_filter     TYPE /iwbep/s_mgw_select_option,
          lwa_sel_option TYPE /iwbep/s_cod_select_option,
          lwa_entityset  LIKE LINE OF et_entityset,
          lwa_message    TYPE /agri/s_gprolog,
          lref_filter    TYPE REF TO /iwbep/if_mgw_req_filter.

*-- Variables
    DATA: lv_filter_str   TYPE string.

*-- Get Filters defined
    lref_filter     = io_tech_request_context->get_filter( ).
    li_filter       = lref_filter->get_filter_select_options( ).
    lv_filter_str   = lref_filter->get_filter_string( ).

*-- Maps filter table lines to function module parameters
    LOOP AT li_filter INTO lwa_filter.
      CASE lwa_filter-property.
        WHEN 'TPLNR'.
          lref_filter->convert_select_option(
            EXPORTING
              is_select_option = lwa_filter
            IMPORTING
              et_select_option = lr_tplnr ).
        WHEN 'CMNUM'.
          lref_filter->convert_select_option(
            EXPORTING
              is_select_option = lwa_filter
            IMPORTING
              et_select_option = lr_cmnum ).
        WHEN 'VARIA'.
          lref_filter->convert_select_option(
            EXPORTING
              is_select_option = lwa_filter
            IMPORTING
              et_select_option = lr_varia ).
        WHEN 'CPROS'.
          lref_filter->convert_select_option(
            EXPORTING
              is_select_option = lwa_filter
            IMPORTING
              et_select_option = lr_cpros ).
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

    add_headers( ).

    SELECT tplnr_fl,cmnum,varia
      FROM /agri/glflcma
      INTO TABLE @DATA(li_glflcma)
      WHERE tplnr_fl IN @lr_tplnr
        AND cmnum    IN @lr_cmnum
        AND varia    IN @lr_varia.

    IF li_glflcma IS NOT INITIAL.
      SELECT cmnum,varia,cpros,ppros
        FROM /agri/glcmprs
        INTO TABLE @DATA(li_glcmprs)               "#EC CI_NO_TRANSFORM
        FOR ALL ENTRIES IN @li_glflcma
        WHERE cmnum EQ @li_glflcma-cmnum
          AND varia EQ @li_glflcma-varia.
    ELSE.
      SELECT cmnum,varia,cpros,ppros
        FROM /agri/glcmprs
        INTO TABLE @li_glcmprs
        WHERE cmnum IN @lr_cmnum
          AND varia IN @lr_varia
          AND cpros IN @lr_cpros.
    ENDIF.

    IF li_glcmprs IS NOT INITIAL.
      SELECT * FROM /agri/glcmprst
        INTO TABLE @DATA(li_glcmprst)
        FOR ALL ENTRIES IN @li_glcmprs             "#EC CI_NO_TRANSFORM
        WHERE cmnum EQ @li_glcmprs-cmnum
          AND varia EQ @li_glcmprs-varia
          AND cpros EQ @li_glcmprs-cpros
          AND spras EQ @sy-langu.
    ENDIF.

    SORT: li_glcmprst BY cmnum varia cpros.

    LOOP AT li_glcmprs INTO DATA(lwa_glcmprs).
      MOVE-CORRESPONDING lwa_glcmprs TO lwa_entityset.
      READ TABLE li_glcmprst INTO DATA(lwa_glcmprst)
           WITH KEY cmnum = lwa_glcmprs-cmnum
                    varia = lwa_glcmprs-varia
                    cpros = lwa_glcmprs-cpros BINARY SEARCH.
      IF sy-subrc = 0.
        lwa_entityset-descr = lwa_glcmprst-descr.
      ENDIF.

      APPEND lwa_entityset TO et_entityset.
      CLEAR: lwa_entityset,lwa_glcmprs.
    ENDLOOP.

    IF et_entityset IS INITIAL.
      CLEAR: lwa_message.
      lwa_message-msgty = c_msg_type-error.
      lwa_message-msgid = '/AGRI/GLOBAL'.
      lwa_message-msgno = '751'.
      add_messages_to_msg_container( EXPORTING is_message = lwa_message ).
      RETURN.
    ENDIF.

  ENDMETHOD.


  method F4CROPVARIANTSSE_GET_ENTITYSET.

*-- Data Declarations
*-- Internal Tables
    DATA: li_filter TYPE /iwbep/t_mgw_select_option,
          lr_cmnum  TYPE RANGE OF /agri/glcmnum,
          lr_varia  TYPE RANGE OF /agri/glvaria.

*-- Work Areas
    DATA: lwa_filter     TYPE /iwbep/s_mgw_select_option,
          lwa_sel_option TYPE /iwbep/s_cod_select_option,
          lwa_entityset  LIKE LINE OF et_entityset,
          lwa_message    TYPE /agri/s_gprolog,
          lref_filter    TYPE REF TO /iwbep/if_mgw_req_filter.

*-- Variables
    DATA: lv_filter_str   TYPE string.

*-- Get Filters defined
    lref_filter     = io_tech_request_context->get_filter( ).
    li_filter       = lref_filter->get_filter_select_options( ).
    lv_filter_str   = lref_filter->get_filter_string( ).

*-- Maps filter table lines to function module parameters
    LOOP AT li_filter INTO lwa_filter.
      CASE lwa_filter-property.
        WHEN 'CMNUM'.
          lref_filter->convert_select_option(
            EXPORTING
              is_select_option = lwa_filter
            IMPORTING
              et_select_option = lr_cmnum ).
        WHEN 'VARIA'.
          lref_filter->convert_select_option(
            EXPORTING
              is_select_option = lwa_filter
            IMPORTING
              et_select_option = lr_varia ).
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

    add_headers( ).

    SELECT * FROM /agri/glcmvar
      INTO TABLE @DATA(li_glcmvar)
      WHERE cmnum IN @lr_cmnum
        AND varia IN @lr_varia.

    LOOP AT li_glcmvar INTO DATA(lwa_glcmvar).
      MOVE-CORRESPONDING lwa_glcmvar TO lwa_entityset.
      APPEND lwa_entityset TO et_entityset.
      CLEAR: lwa_entityset,lwa_glcmvar.
    ENDLOOP.

    IF et_entityset IS INITIAL.
      CLEAR: lwa_message.
      lwa_message-msgty = c_msg_type-error.
      lwa_message-msgid = '/AGRI/GLOBAL'.
      lwa_message-msgno = '751'.
      add_messages_to_msg_container( EXPORTING is_message = lwa_message ).
      RETURN.
    ENDIF.

  endmethod.


  method F4WORKORDERTYPES_GET_ENTITYSET.

*-- Data Declarations
*-- Internal Tables
    DATA: li_filter TYPE /iwbep/t_mgw_select_option,
          lr_wotyp  TYPE RANGE OF /agri/fmwotyp.

*-- Work Areas
    DATA: lwa_filter     TYPE /iwbep/s_mgw_select_option,
          lwa_sel_option TYPE /iwbep/s_cod_select_option,
          lwa_entityset  LIKE LINE OF et_entityset,
          lwa_message    TYPE /agri/s_gprolog,
          lref_filter    TYPE REF TO /iwbep/if_mgw_req_filter.

*-- Variables
    DATA: lv_filter_str   TYPE string.

*-- Get Filters defined
    lref_filter     = io_tech_request_context->get_filter( ).
    li_filter       = lref_filter->get_filter_select_options( ).
    lv_filter_str   = lref_filter->get_filter_string( ).

*-- Maps filter table lines to function module parameters
    LOOP AT li_filter INTO lwa_filter.
      CASE lwa_filter-property.
        WHEN 'WOTYP'.
          lref_filter->convert_select_option(
            EXPORTING
              is_select_option = lwa_filter
            IMPORTING
              et_select_option = lr_wotyp ).
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

    add_headers( ).

    SELECT wotyp,class,sappl
      FROM /agri/tfmwot
      INTO TABLE @DATA(li_fmwot)
      WHERE wotyp IN @lr_wotyp.

    IF li_fmwot IS NOT INITIAL.
      SELECT * FROM /agri/tfmwott
        INTO TABLE @DATA(li_fmwott)
        FOR ALL ENTRIES IN @li_fmwot
        WHERE spras EQ @sy-langu
          AND crtyp EQ @li_fmwot-wotyp.
    ENDIF.

    SORT: li_fmwott BY crtyp.

    LOOP AT li_fmwot INTO DATA(lwa_fmwot).

      MOVE-CORRESPONDING lwa_fmwot TO lwa_entityset.

      READ TABLE li_fmwott INTO DATA(lwa_fmwott)
           WITH KEY crtyp = lwa_fmwot-wotyp BINARY SEARCH.
      IF sy-subrc = 0.
        lwa_entityset-descr = lwa_fmwott-descr.
      ENDIF.

      APPEND lwa_entityset TO et_entityset.
      CLEAR: lwa_entityset,lwa_fmwot.

    ENDLOOP.

    IF et_entityset IS INITIAL.
      CLEAR: lwa_message.
      lwa_message-msgty = c_msg_type-error.
      lwa_message-msgid = '/AGRI/GLOBAL'.
      lwa_message-msgno = '751'.
      add_messages_to_msg_container( EXPORTING is_message = lwa_message ).
      RETURN.
    ENDIF.

  endmethod.


  METHOD get_attribute_display_data.

    DATA: lwa_athdr     TYPE /agri/s_gathdr,
          lwa_athdr_tmp TYPE /agri/s_gathdr.

    DATA: lv_atwrt       TYPE /agri/gatwrt,
          lv_atflv       TYPE atflv,
          lv_subrc       TYPE sy-subrc,
          lv_value_descr.

    CLEAR: lwa_athdr,lwa_athdr_tmp.

    MOVE-CORRESPONDING is_athdr TO cs_attr_values.
    IF is_mdatv IS NOT INITIAL.
      MOVE-CORRESPONDING is_mdatv TO cs_attr_values.  "#EC CI_FLDEXT_OK
    ENDIF.

    IF NOT is_mdatv-atwrt IS INITIAL
    OR NOT is_mdatv-atflv IS INITIAL
    OR NOT is_athdr-inval IS INITIAL.

      lwa_athdr_tmp = is_athdr.
      CLEAR lwa_athdr_tmp-msehi.

      IF NOT is_mdatv-atwrt IS INITIAL
      OR NOT is_mdatv-atflv IS INITIAL
      OR NOT is_athdr-inval IS INITIAL.

        CALL METHOD /agri/cl_gattr_utils=>attr_value_for_display_prepare
          EXPORTING
            i_agtyp       = gc_agtyp-measurements
            i_value_descr = c_true
            is_athdr      = lwa_athdr_tmp
          CHANGING
            c_atwrt       = cs_attr_values-atwrt
            c_atflv       = cs_attr_values-atflv
            c_atwtb       = cs_attr_values-atwtb.

        IF NOT lwa_athdr_tmp-rcura IS INITIAL.
          IF lwa_athdr_tmp-msehi IS INITIAL.
            lwa_athdr_tmp-msehi = is_athdr-msehi.
          ENDIF.
        ELSE.
          lwa_athdr_tmp-msehi = is_athdr-msehi.
        ENDIF.

        IF NOT is_athdr-runta IS INITIAL.
          READ TABLE it_athdr INTO lwa_athdr
          WITH KEY atnam = is_athdr-runta.
          IF sy-subrc EQ 0.
            IF NOT lv_atwrt IS INITIAL.
              lwa_athdr_tmp-msehi = lv_atwrt.         "#EC CI_FLDEXT_OK
            ENDIF.
          ENDIF.
        ENDIF.

        IF NOT lwa_athdr_tmp-msehi IS INITIAL.
          CONCATENATE cs_attr_values-atwtb
                      lwa_athdr_tmp-msehi
                 INTO cs_attr_values-atwtb.
          CONDENSE cs_attr_values-atwtb NO-GAPS.
        ENDIF.
      ENDIF.
    ENDIF.

    cs_attr_values-atwrt_old = cs_attr_values-atwrt.
    cs_attr_values-datuv_old = cs_attr_values-datuv.
    IF cs_attr_values-atbez IS INITIAL.
      cs_attr_values-atbez = is_athdr-atbez.
    ENDIF.


  ENDMETHOD.


  METHOD get_date_ranges.

    DATA: lwa_datab LIKE LINE OF er_datab,
          lwa_datbi LIKE LINE OF er_datbi.


    IF iv_datab IS NOT INITIAL AND
       iv_datbi IS INITIAL.

      CLEAR: lwa_datab,lwa_datbi.
      lwa_datab-sign   = 'I'.
      lwa_datab-option = 'LE'.
      lwa_datab-low    = iv_datab.
      APPEND lwa_datab TO er_datab.

      lwa_datbi-sign   = 'I'.
      lwa_datbi-option = 'GE'.
      lwa_datbi-low    = iv_datab.
      APPEND lwa_datbi TO er_datbi.

    ELSEIF iv_datab IS INITIAL AND
           iv_datbi IS NOT INITIAL.

      CLEAR: lwa_datab,lwa_datbi.
      lwa_datab-sign   = 'I'.
      lwa_datab-option = 'LE'.
      lwa_datab-low    = iv_datbi.
      APPEND lwa_datab TO er_datab.

      lwa_datbi-sign   = 'I'.
      lwa_datbi-option = 'GE'.
      lwa_datbi-low    = iv_datbi.
      APPEND lwa_datbi TO er_datbi.

    ELSEIF iv_datab IS NOT INITIAL AND
           iv_datbi IS NOT INITIAL.

      CLEAR: lwa_datab,lwa_datbi.
      lwa_datab-sign   = 'I'.
      lwa_datab-option = 'LE'.
      lwa_datab-low    = iv_datab.
      APPEND lwa_datab TO er_datab.

      CLEAR: lwa_datab.
      lwa_datab-sign   = 'I'.
      lwa_datab-option = 'LE'.
      lwa_datab-low    = iv_datbi.
      APPEND lwa_datab TO er_datab.

      lwa_datbi-sign   = 'I'.
      lwa_datbi-option = 'GE'.
      lwa_datbi-low    = iv_datbi.
      APPEND lwa_datbi TO er_datbi.
    ENDIF.

  ENDMETHOD.


  METHOD get_order_confirmations_data.

    DATA: lt_afwi       TYPE TABLE OF afwi,
          lt_ocnum      TYPE /agri/t_fmocnum,
          lwa_afwi      TYPE afwi,
          lwa_afwi_ref  TYPE afwi,
          lwa_mseg      TYPE mseg,
          lt_mseg       TYPE TABLE OF mseg,
          lwa_affw      TYPE affw,
          lt_affw       TYPE TABLE OF affw,
          lwa_ochdr     TYPE /agri/s_fmochdr,
          lwa_ochdr_ref TYPE /agri/s_fmochdr,
          lt_ochdr      TYPE /agri/t_fmochdr,
          lwa_ocopr     TYPE /agri/s_fmocopr,
          lwa_ocopr_ref TYPE /agri/s_fmocopr,
          lt_ocopr      TYPE /agri/t_fmocopr,
          lt_occom      TYPE /agri/t_fmoccom,
          lwa_fmoc_data TYPE /agri/s_fmoc_details.


    FIELD-SYMBOLS:
           <lwa_afwi>   LIKE LINE OF lt_afwi.

    CONSTANTS:BEGIN OF c_status,
                confirmation   TYPE /agri/fmcglst VALUE 'A',
                goods_movement TYPE /agri/fmcglst VALUE 'B',
                cogi           TYPE /agri/fmcglst VALUE 'C',
              END OF c_status.

    lt_ocnum[] = it_fmocnum[].

    CHECK lt_ocnum IS NOT INITIAL.

    REFRESH: et_fmoc_data.
    SELECT * FROM /agri/fmochdr
             INTO CORRESPONDING FIELDS OF TABLE lt_ochdr
              FOR ALL ENTRIES IN lt_ocnum
            WHERE ocnum EQ lt_ocnum-ocnum.
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

      IF lt_ochdr IS NOT INITIAL.
        SELECT * FROM /agri/fmoccom
           APPENDING CORRESPONDING FIELDS OF TABLE lt_occom
           FOR ALL ENTRIES IN lt_ochdr
           WHERE ocnum EQ lt_ochdr-ocnum_ref.
        IF sy-subrc EQ 0.
          SORT lt_occom BY ocnum rueck rmzhl contr.
        ENDIF.
      ENDIF.

      SELECT *
              FROM afwi
              INTO CORRESPONDING FIELDS OF TABLE lt_afwi
               FOR ALL ENTRIES IN lt_ocopr
             WHERE rueck EQ lt_ocopr-rueck
               AND rmzhl EQ lt_ocopr-rmzhl.
      IF sy-subrc EQ 0.
        SORT lt_afwi BY rueck rmzhl mblnr mjahr mblpo.
        SELECT * FROM mseg
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
                WHERE weblnr  EQ lt_afwi-mblnr.
      ENDIF.
    ENDIF.

    SORT lt_mseg BY mblnr mjahr zeile.
    SORT lt_ochdr BY ocnum.
    SORT lt_ocopr BY ocnum rueck rmzhl.
    SORT lt_afwi BY rueck rmzhl mblnr mjahr mblpo.
    SORT lt_affw BY weblnr.

    LOOP AT lt_ochdr INTO lwa_ochdr.
      LOOP AT lt_ocopr INTO lwa_ocopr
                      WHERE ocnum EQ lwa_ochdr-ocnum.
        MOVE-CORRESPONDING lwa_ochdr TO lwa_fmoc_data.
        CLEAR: lwa_fmoc_data-erdat,lwa_fmoc_data-matnr.
        MOVE-CORRESPONDING lwa_ocopr TO lwa_fmoc_data.
        IF i_details IS INITIAL.
          LOOP AT lt_afwi ASSIGNING <lwa_afwi> WHERE rueck EQ lwa_ocopr-rueck
                                                 AND rmzhl EQ lwa_ocopr-rmzhl.
*          AT NEW mblnr.
            IF <lwa_afwi>-mblnr IS NOT INITIAL.
              READ TABLE lt_mseg INTO lwa_mseg
                                 WITH KEY mblnr = <lwa_afwi>-mblnr
                                          mjahr = <lwa_afwi>-mjahr
                                 BINARY SEARCH.
              IF sy-subrc EQ 0.
                IF lwa_mseg-shkzg = 'H'.
                  lwa_fmoc_data-mblnr_gi = lwa_mseg-mblnr.
                  lwa_fmoc_data-mjahr_gi = lwa_mseg-mjahr.
                  lwa_fmoc_data-bwart_gi = lwa_mseg-bwart.
                ELSEIF lwa_mseg-shkzg = 'S'.
                  lwa_fmoc_data-mblnr = lwa_mseg-mblnr.
                  lwa_fmoc_data-mjahr = lwa_mseg-mjahr.
                  lwa_fmoc_data-bwart = lwa_mseg-bwart.
                ENDIF.
                lwa_fmoc_data-budat_mkpf = lwa_mseg-budat_mkpf.
              ENDIF.
            ENDIF.
*          ENDAT.
            IF lwa_fmoc_data-mblnr_gi IS NOT INITIAL AND
               lwa_fmoc_data-mblnr IS NOT INITIAL.
              EXIT.
            ENDIF.
          ENDLOOP.
          IF lwa_fmoc_data-mblnr_gi IS NOT INITIAL AND
             lwa_fmoc_data-mblnr IS NOT INITIAL.
            EXIT.
          ENDIF.
        ELSE.
          CHECK lwa_ochdr-ocnum_ref IS INITIAL.
          READ TABLE lt_ochdr INTO lwa_ochdr_ref WITH KEY ocnum_ref = lwa_ochdr-ocnum
                                                 BINARY SEARCH.
          IF sy-subrc EQ 0.
            lwa_fmoc_data-ocnum_ref = lwa_ochdr_ref-ocnum.
          ENDIF.
          lwa_fmoc_data-menge = lwa_ocopr-lmnga.
          lwa_fmoc_data-meins = lwa_ocopr-meinh.
          CLEAR: lwa_fmoc_data-charg.
          lwa_fmoc_data-cglst = c_status-confirmation.
          APPEND lwa_fmoc_data TO et_fmoc_data.
          LOOP AT lt_afwi INTO lwa_afwi
                         WHERE rueck EQ lwa_ocopr-rueck
                           AND rmzhl EQ lwa_ocopr-rmzhl.
            READ TABLE lt_mseg INTO lwa_mseg
                           WITH KEY mblnr = lwa_afwi-mblnr
                                    mjahr = lwa_afwi-mjahr
                                    zeile = lwa_afwi-mblpo
                           BINARY SEARCH.
            IF sy-subrc EQ 0.
              MOVE-CORRESPONDING lwa_mseg TO lwa_fmoc_data.
              lwa_fmoc_data-cglst = c_status-goods_movement.
            ELSE.
              READ TABLE lt_affw INTO lwa_affw WITH KEY weblnr = lwa_afwi-mblnr
                                               BINARY SEARCH.
              IF sy-subrc EQ 0.
                MOVE-CORRESPONDING lwa_affw TO lwa_fmoc_data.
                lwa_fmoc_data-cglst = c_status-cogi.
                CLEAR: lwa_fmoc_data-menge,
                       lwa_fmoc_data-meins.
              ENDIF.
            ENDIF.

            READ TABLE lt_ocopr INTO lwa_ocopr_ref WITH KEY ocnum = lwa_fmoc_data-ocnum_ref
                                                            vornr = lwa_fmoc_data-vornr
                                                   BINARY SEARCH.
            IF sy-subrc EQ 0.
              READ TABLE lt_afwi INTO lwa_afwi_ref WITH KEY rueck = lwa_ocopr_ref-rueck
                                                            rmzhl = lwa_ocopr_ref-rmzhl
                                                       BINARY SEARCH.
              IF sy-subrc EQ 0.
                lwa_fmoc_data-mblnr_gi = lwa_afwi_ref-mblnr.
                lwa_fmoc_data-mjahr_gi = lwa_afwi_ref-mjahr.
                READ TABLE lt_mseg TRANSPORTING NO FIELDS
                                   WITH KEY mblnr = lwa_afwi_ref-mblnr
                                            mjahr = lwa_afwi_ref-mjahr
                                   BINARY SEARCH.
                IF sy-subrc NE 0.
                  lwa_fmoc_data-cglst = c_status-cogi.
                ELSE.
                  lwa_fmoc_data-cglst = c_status-goods_movement.
                ENDIF.
              ENDIF.
            ENDIF.
            APPEND lwa_fmoc_data TO et_fmoc_data.
          ENDLOOP.
          CLEAR: lwa_fmoc_data,lwa_afwi_ref,lwa_ocopr_ref,lwa_ochdr_ref.
        ENDIF.
      ENDLOOP.
      IF i_details IS INITIAL.
        APPEND lwa_fmoc_data TO et_fmoc_data.
      ENDIF.
      CLEAR lwa_fmoc_data.
    ENDLOOP.

  ENDMETHOD.


  METHOD measurementattri_get_entityset.

*-- Data Declarations
*-- Internal Tables
    DATA: lt_filter      TYPE /iwbep/t_mgw_select_option,
          lt_range_mdocm TYPE RANGE OF /agri/glmdocm.

*-- Work Areas
    DATA: lwa_filter     TYPE /iwbep/s_mgw_select_option,
          lwa_sel_option TYPE /iwbep/s_cod_select_option,
          lwa_message    TYPE /agri/s_gprolog,
          lwa_glmdhdr    TYPE /agri/s_glmdhdr,
          lwa_entityset  LIKE LINE OF et_entityset,
          lref_filter    TYPE REF TO /iwbep/if_mgw_req_filter.

*-- Variables
    DATA: lv_data TYPE char1.

*-- Get Filter Values
    lref_filter     = io_tech_request_context->get_filter( ).
    lt_filter       = lref_filter->get_filter_select_options( ).

*-- Add Response Headers
    add_headers( ).

*-- Maps filter table lines to function module parameters
    LOOP AT lt_filter INTO lwa_filter.
      CASE lwa_filter-property.
        WHEN 'MDOCM'.
          lref_filter->convert_select_option(
            EXPORTING
              is_select_option = lwa_filter
            IMPORTING
              et_select_option = lt_range_mdocm ).
        WHEN 'DATA'.
          CLEAR: lwa_sel_option.
          READ TABLE lwa_filter-select_options INTO lwa_sel_option INDEX 1.
          IF sy-subrc = 0.
            lv_data = lwa_sel_option-low.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

    IF lv_data = c_true.
      attributes_data_prepare(
        EXPORTING
          io_tech_request_context = io_tech_request_context
        IMPORTING
          et_entityset            = et_entityset
      ).

      RETURN.
    ENDIF.

    SELECT * FROM /agri/glmdatv
      INTO TABLE @DATA(lt_mdatv)
      WHERE mdocm IN @lt_range_mdocm.

    LOOP AT lt_mdatv INTO DATA(lwa_mdatv).
      MOVE-CORRESPONDING lwa_mdatv TO lwa_entityset.
      APPEND lwa_entityset TO et_entityset.
      CLEAR: lwa_entityset.
    ENDLOOP.

  ENDMETHOD.


  METHOD measurementdocum_get_entityset.

*-- Data Declarations
*-- Internal Tables
    DATA: lt_filter      TYPE /iwbep/t_mgw_select_option,
          lt_mdtyp       TYPE TABLE OF /agri/tglmdtyp,
          lt_mdtypt      TYPE TABLE OF /agri/tglmdtypt,
          lt_dd07v       TYPE dd07v_tab,
          lt_range_mdocm TYPE RANGE OF /agri/glmdocm,
          lt_range_mdtyp TYPE RANGE OF /agri/glmdtyp,
          lt_range_aslvl TYPE RANGE OF /agri/glaslvl.

*-- Work Areas
    DATA: lwa_mdtyp     TYPE /agri/tglmdtyp,
          lwa_mdtypt    TYPE /agri/tglmdtypt,
          lwa_dd07v     TYPE dd07v,
          lwa_filter    TYPE /iwbep/s_mgw_select_option,
          lwa_entityset LIKE LINE OF et_entityset,
          lref_filter   TYPE REF TO /iwbep/if_mgw_req_filter.

*-- Get Filter Values
    lref_filter     = io_tech_request_context->get_filter( ).
    lt_filter       = lref_filter->get_filter_select_options( ).

*-- Add Response Headers
    add_headers( ).

*-- Maps filter table lines to function module parameters
    LOOP AT lt_filter INTO lwa_filter.
      CASE lwa_filter-property.
        WHEN 'MDOCM'.
          lref_filter->convert_select_option(
            EXPORTING
              is_select_option = lwa_filter
            IMPORTING
              et_select_option = lt_range_mdocm ).
        WHEN 'MDTYP'.
          lref_filter->convert_select_option(
            EXPORTING
              is_select_option = lwa_filter
            IMPORTING
              et_select_option = lt_range_mdtyp ).
        WHEN 'ASLVL'.
          lref_filter->convert_select_option(
            EXPORTING
              is_select_option = lwa_filter
            IMPORTING
              et_select_option = lt_range_aslvl ).
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

    SELECT * FROM /agri/glmdhdr
      INTO TABLE @DATA(lt_mdhdr)
      WHERE mdocm IN @lt_range_mdocm
        AND mdtyp IN @lt_range_mdtyp
        AND aslvl IN @lt_range_aslvl.

    IF lt_mdhdr IS NOT INITIAL.
      SELECT * FROM /agri/tglmdtypt
        INTO TABLE lt_mdtypt
        FOR ALL ENTRIES IN lt_mdhdr
        WHERE spras EQ sy-langu
          AND mdtyp EQ lt_mdhdr-mdtyp.

      CALL FUNCTION 'DD_DOMVALUES_GET'
        EXPORTING
          domname        = '/AGRI/GLASLVL'
          text           = 'X'
        TABLES
          dd07v_tab      = lt_dd07v
        EXCEPTIONS
          wrong_textflag = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
    ENDIF.

    SORT: lt_mdtypt BY mdtyp,
          lt_dd07v  BY domvalue_l.

    LOOP AT lt_mdhdr INTO DATA(lwa_mdhdr).

      MOVE-CORRESPONDING lwa_mdhdr TO lwa_entityset.

*      READ TABLE lt_mdtypt INTO lwa_mdtypt
*           WITH KEY mdtyp = lwa_mdhdr-mdtyp BINARY SEARCH.
*      IF sy-subrc = 0.
*        lwa_entityset-descr = lwa_mdtypt-descr.
*      ENDIF.
*
*      READ TABLE lt_dd07v INTO lwa_dd07v
*           WITH KEY domvalue_l = lwa_mdtyp-aslvl BINARY SEARCH.
*      IF sy-subrc = 0.
*        lwa_entityset-ddtext = lwa_dd07v-ddtext.
*      ENDIF.

      APPEND lwa_entityset TO et_entityset.
*      CLEAR: lwa_entityset,lwa_mdtyp,lwa_mdtypt,lwa_dd07v.
    ENDLOOP.

  ENDMETHOD.


  method MEASUREMENTGROUP_GET_ENTITYSET.

*-- Data Declarations
*-- Internal Tables
    DATA: lt_filter      TYPE /iwbep/t_mgw_select_option,
          lt_glagha      TYPE TABLE OF /agri/glagha,
          lt_range_class TYPE RANGE OF /agri/gatgrp,
          lt_range_klart TYPE RANGE OF /agri/gagtyp,
          lt_range_agcat TYPE RANGE OF /agri/glagcat,
          lt_range_mdtyp TYPE RANGE OF /agri/glmdtyp,
          lt_range_aslvl TYPE RANGE OF /agri/glaslvl.

*-- Work Areas
    DATA: lwa_glagha    TYPE /agri/glagha,
          lwa_filter    TYPE /iwbep/s_mgw_select_option,
          lwa_entityset LIKE LINE OF et_entityset,
          lref_filter   TYPE REF TO /iwbep/if_mgw_req_filter.

*-- Get Filter Values
    lref_filter     = io_tech_request_context->get_filter( ).
    lt_filter       = lref_filter->get_filter_select_options( ).

*-- Add Response Headers
    add_headers( ).

*-- Maps filter table lines to function module parameters
    LOOP AT lt_filter INTO lwa_filter.
      CASE lwa_filter-property.
        WHEN 'CLASS'.
          lref_filter->convert_select_option(
            EXPORTING
              is_select_option = lwa_filter
            IMPORTING
              et_select_option = lt_range_class ).
        WHEN 'KLART'.
          lref_filter->convert_select_option(
            EXPORTING
              is_select_option = lwa_filter
            IMPORTING
              et_select_option = lt_range_klart ).
        WHEN 'AGCAT'.
          lref_filter->convert_select_option(
            EXPORTING
              is_select_option = lwa_filter
            IMPORTING
              et_select_option = lt_range_agcat ).
        WHEN 'MDTYP'.
          lref_filter->convert_select_option(
            EXPORTING
              is_select_option = lwa_filter
            IMPORTING
              et_select_option = lt_range_mdtyp ).
        WHEN 'ASLVL'.
          lref_filter->convert_select_option(
            EXPORTING
              is_select_option = lwa_filter
            IMPORTING
              et_select_option = lt_range_aslvl ).
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

*-- Attribute Groups Header Data
    SELECT * FROM /agri/glagha
      INTO TABLE lt_glagha
      WHERE class IN lt_range_class
        AND klart IN lt_range_klart
        AND agcat IN lt_range_agcat
        AND mdtyp IN lt_range_mdtyp
        AND aslvl IN lt_range_aslvl.

    LOOP AT lt_glagha INTO lwa_glagha.
      MOVE-CORRESPONDING lwa_glagha TO lwa_entityset.
      APPEND lwa_entityset TO et_entityset.
      CLEAR: lwa_entityset,lwa_glagha.
    ENDLOOP.

  endmethod.


  METHOD measurements_data_process.

*-- Data Declarations
*-- Internal Tables
    DATA: lt_mdhdr    TYPE /agri/t_glmdhdr,
          lt_mdatv    TYPE /agri/t_glmdatv,
          lt_mddoc    TYPE /agri/t_glmd_doc,
          lt_messages TYPE /agri/t_gprolog.
*-- Work Areas
    DATA: lwa_mdhdr      TYPE /agri/s_glmdhdr,
          lwa_mdatv      TYPE /agri/s_glmdatv,
          lwa_mddoc      TYPE /agri/s_glmd_doc,
          lwa_message    TYPE /agri/s_gprolog,
          lwa_mdatv_data TYPE zcl_zabs_gltr_gis_ext_mpc=>ts_measurementattributes.

    CHECK gwa_glmd_data IS NOT INITIAL.

    REFRESH: lt_mdhdr, lt_mdatv.
    CLEAR: lwa_mdhdr.

    MOVE-CORRESPONDING gwa_glmd_data TO lwa_mdhdr.

    IF lwa_mdhdr-mdate IS INITIAL.
      lwa_mdhdr-mdate = sy-datum.
    ENDIF.

    IF lwa_mdhdr-mtime IS INITIAL.
      lwa_mdhdr-mtime = sy-uzeit.
    ENDIF.

    IF lwa_mdhdr-muser IS INITIAL.
      lwa_mdhdr-muser = sy-uname.
    ENDIF.

    APPEND lwa_mdhdr TO lt_mdhdr.

    LOOP AT gwa_glmd_data-measurementattributesset INTO lwa_mdatv_data.

      CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
        EXPORTING
          input  = lwa_mdatv_data-atnam
        IMPORTING
          output = lwa_mdatv_data-atinn.

      MOVE-CORRESPONDING lwa_mdatv_data TO lwa_mdatv.
      IF lwa_mdatv-atwrt IS INITIAL.
        CLEAR: lwa_mdatv-updkz.
      ENDIF.
      APPEND lwa_mdatv TO lt_mdatv.
      CLEAR: lwa_mdatv,lwa_mdatv_data.
    ENDLOOP.

    CALL FUNCTION '/AGRI/GLMD_CREATE_MASS'
      EXPORTING
        it_mdhdr          = lt_mdhdr
        it_mdatv          = lt_mdatv
      IMPORTING
        et_mddoc          = lt_mddoc
        et_messages       = lt_messages
      EXCEPTIONS
        inconsistent_data = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
      CLEAR: lwa_message.
      lwa_message-msgty = c_msg_type-error.
      lwa_message-msgid = '00'.
      lwa_message-msgno = '001'.
      lwa_message-msgv1 = TEXT-001.
      APPEND lwa_message TO lt_messages.
    ENDIF.

    IF lt_mddoc IS NOT INITIAL.
      CLEAR: lwa_mddoc.
      READ TABLE lt_mddoc INTO lwa_mddoc INDEX 1.
      cs_glmd_data-mdocm = lwa_mddoc-x-mdhdr-mdocm.
    ENDIF.

    IF lt_messages IS NOT INITIAL.
      add_messages_to_msg_container( EXPORTING it_messages = lt_messages ).
    ENDIF.

  ENDMETHOD.


  METHOD measurementtypes_get_entityset.

*-- Data Declarations
*-- Internal Tables
    DATA: lt_filter      TYPE /iwbep/t_mgw_select_option,
          lt_mdtyp       TYPE TABLE OF /agri/tglmdtyp,
          lt_mdtypt      TYPE TABLE OF /agri/tglmdtypt,
          lt_dd07v       TYPE dd07v_tab,
          lt_range_mdtyp TYPE RANGE OF /agri/glmdtyp,
          lt_range_aslvl TYPE RANGE OF /agri/glaslvl.

*-- Work Areas
    DATA: lwa_mdtyp     TYPE /agri/tglmdtyp,
          lwa_mdtypt    TYPE /agri/tglmdtypt,
          lwa_dd07v     TYPE dd07v,
          lwa_filter    TYPE /iwbep/s_mgw_select_option,
          lwa_entityset LIKE LINE OF et_entityset,
          lref_filter   TYPE REF TO /iwbep/if_mgw_req_filter.

*-- Get Filter Values
    lref_filter     = io_tech_request_context->get_filter( ).
    lt_filter       = lref_filter->get_filter_select_options( ).

*-- Add Response Headers
    add_headers( ).

*-- Maps filter table lines to function module parameters
    LOOP AT lt_filter INTO lwa_filter.
      CASE lwa_filter-property.
        WHEN 'MDTYP'.
          lref_filter->convert_select_option(
            EXPORTING
              is_select_option = lwa_filter
            IMPORTING
              et_select_option = lt_range_mdtyp ).
        WHEN 'ASLVL'.
          lref_filter->convert_select_option(
            EXPORTING
              is_select_option = lwa_filter
            IMPORTING
              et_select_option = lt_range_aslvl ).
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

    SELECT * FROM /agri/tglmdtyp
      INTO TABLE lt_mdtyp
      WHERE mdtyp IN lt_range_mdtyp
        AND aslvl IN lt_range_aslvl.

    IF lt_mdtyp IS NOT INITIAL.
      SELECT * FROM /agri/tglmdtypt
        INTO TABLE lt_mdtypt
        FOR ALL ENTRIES IN lt_mdtyp
        WHERE spras EQ sy-langu
          AND mdtyp EQ lt_mdtyp-mdtyp.

      CALL FUNCTION 'DD_DOMVALUES_GET'
        EXPORTING
          domname        = '/AGRI/GLASLVL'
          text           = 'X'
        TABLES
          dd07v_tab      = lt_dd07v
        EXCEPTIONS
          wrong_textflag = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
    ENDIF.

    SORT: lt_mdtypt BY mdtyp,
          lt_dd07v BY domvalue_l.

    LOOP AT lt_mdtyp INTO lwa_mdtyp.
      MOVE-CORRESPONDING lwa_mdtyp TO lwa_entityset.

      READ TABLE lt_mdtypt INTO lwa_mdtypt
           WITH KEY mdtyp = lwa_mdtyp-mdtyp BINARY SEARCH.
      IF sy-subrc = 0.
        lwa_entityset-descr = lwa_mdtypt-descr.
      ENDIF.

      READ TABLE lt_dd07v INTO lwa_dd07v
           WITH KEY domvalue_l = lwa_mdtyp-aslvl BINARY SEARCH.
      IF sy-subrc = 0.
        lwa_entityset-ddtext = lwa_dd07v-ddtext.
      ENDIF.

      APPEND lwa_entityset TO et_entityset.
      CLEAR: lwa_entityset,lwa_mdtyp,lwa_mdtypt,lwa_dd07v.
    ENDLOOP.

  ENDMETHOD.


  METHOD order_status_check.

    TYPES: BEGIN OF lty_status,
             status TYPE j_txt04,
           END OF lty_status .

*-- Data Declarations
    DATA: li_status  TYPE STANDARD TABLE OF lty_status,
          lwa_status TYPE lty_status,
          lv_line    TYPE j_stext.

    CHECK iv_aufnr IS NOT INITIAL OR
          iv_objnr IS NOT INITIAL.

*-- Status Text Get
    CALL FUNCTION 'STATUS_TEXT_EDIT'
      EXPORTING
        client           = sy-mandt "System ID
        objnr            = iv_objnr "Order Object Number
        only_active      = c_true
        spras            = sy-langu "System Language
      IMPORTING
        line             = lv_line
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    ev_sttxt = lv_line.
    SPLIT lv_line AT space INTO TABLE li_status.
    DELETE li_status WHERE status = space.

*-- check Status for delete
    READ TABLE li_status INTO lwa_status
         WITH KEY status = gc_order_status-delete.
    IF sy-subrc = 0.
*      ev_subrc = 4.
      ev_cstat = gc_order_status-delete.
    ENDIF.

*-- Check Status for closed
    READ TABLE li_status INTO lwa_status
         WITH KEY status = gc_order_status-closed.
    IF sy-subrc = 0.
*      ev_subrc = 4.
      ev_cstat = gc_order_status-closed.
    ENDIF.

*-- Check Status for Techo
    READ TABLE li_status INTO lwa_status
         WITH KEY status = gc_order_status-techo.
    IF sy-subrc = 0.
*      ev_subrc = 4.
      ev_cstat = gc_order_status-techo.
    ENDIF.

*-- Check Confirmation Status
    READ TABLE li_status INTO lwa_status
         WITH KEY status = gc_order_status-cnf.
    IF sy-subrc = 0.
      ev_cstat = gc_order_status-cnf.
    ENDIF.

*-- Check partially complete Confirmation Status
    READ TABLE li_status INTO lwa_status
         WITH KEY status = gc_order_status-pcnf.
    IF sy-subrc = 0.
      ev_cstat = gc_order_status-pcnf.
    ENDIF.


  ENDMETHOD.


  METHOD taskorderanalyti_get_entityset.

*-- Data Declarations
    DATA: li_filter   TYPE /iwbep/t_mgw_select_option,
          li_dd07v    TYPE TABLE OF dd07v,
          lr_tplnr    TYPE RANGE OF /agri/gltplnr_fl,
          lr_cmnum    TYPE RANGE OF /agri/glcmnum,
          lr_varia    TYPE RANGE OF /agri/glvaria,
          lr_cpros    TYPE RANGE OF /agri/glcpros,
          lr_aufnr    TYPE RANGE OF aufnr,
          lr_matnr    TYPE RANGE OF matnr,
          lr_datab    TYPE RANGE OF datum,
          lr_datbi    TYPE RANGE OF datum,
          lr_tplkz    TYPE RANGE OF /agri/gltplkz,
          lr_tplvl    TYPE RANGE OF /agri/gltplvl,
          lr_tplma    TYPE RANGE OF /agri/gltplma,
          li_messages TYPE /agri/t_gprolog.

*-- Work Areas
    DATA: lwa_entityset  LIKE LINE OF et_entityset,
          lwa_filter     TYPE /iwbep/s_mgw_select_option,
          lwa_sel_option TYPE /iwbep/s_cod_select_option,
          lwa_message    TYPE /agri/s_gprolog,
          lwa_dd07v      TYPE dd07v,
          lwa_fphdr      TYPE /agri/s_fmfphdr,
          lref_filter    TYPE REF TO /iwbep/if_mgw_req_filter.

*-- Variables
    DATA: lv_datab TYPE datab,
          lv_datbi TYPE datbi,
          lv_subrc TYPE sy-subrc,
          lv_cstat TYPE /agri/fmcstat.

    lref_filter     = io_tech_request_context->get_filter( ).
    li_filter       = lref_filter->get_filter_select_options( ).

*-- Add Headers
    add_headers( ).

    LOOP AT li_filter INTO lwa_filter.
      CASE lwa_filter-property.
        WHEN 'TPLNR_FL'.
          lref_filter->convert_select_option(
            EXPORTING
              is_select_option = lwa_filter
            IMPORTING
              et_select_option = lr_tplnr ).
        WHEN 'CMNUM'.
          lref_filter->convert_select_option(
            EXPORTING
              is_select_option = lwa_filter
            IMPORTING
              et_select_option = lr_cmnum ).
        WHEN 'VARIA'.
          lref_filter->convert_select_option(
            EXPORTING
              is_select_option = lwa_filter
            IMPORTING
              et_select_option = lr_varia ).
        WHEN 'CPROS'.
          lref_filter->convert_select_option(
            EXPORTING
              is_select_option = lwa_filter
            IMPORTING
              et_select_option = lr_cpros ).
        WHEN 'AUFNR'.
          lref_filter->convert_select_option(
            EXPORTING
              is_select_option = lwa_filter
            IMPORTING
              et_select_option = lr_aufnr ).
        WHEN 'MATNR'.
          lref_filter->convert_select_option(
            EXPORTING
              is_select_option = lwa_filter
            IMPORTING
              et_select_option = lr_matnr ).
        WHEN 'DATAB'.
          READ TABLE lwa_filter-select_options INTO lwa_sel_option INDEX 1.
          IF sy-subrc = 0.
            lv_datab = lwa_sel_option-low.
            lv_datbi = lwa_sel_option-high.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

    IF lr_aufnr IS INITIAL.
***-- Get Terrain Structure Indicator Based on User ID
**      terrain_structure_indi_range( EXPORTING iv_username = sy-uname
**                                    IMPORTING er_tplkz    = lr_tplkz
**                                              er_tplvl    = lr_tplvl ).

*-- Get Terrain ID
      SELECT tplnr_fl FROM /agri/glflot
        INTO TABLE @DATA(li_glflot)
        WHERE tplnr_fl IN @lr_tplnr
          AND tplkz    IN @lr_tplkz
          AND tplvl    IN @lr_tplvl
          AND tplma    IN @lr_tplma.
*          AND loevm    EQ @space.

*-- Populate Date Ranges
      IF lv_datab IS NOT INITIAL.
        CALL METHOD me->get_date_ranges
          EXPORTING
            iv_datab = lv_datab
            iv_datbi = lv_datbi
          IMPORTING
            er_datab = lr_datab
            er_datbi = lr_datbi.
      ENDIF.

      IF li_glflot IS NOT INITIAL.
        SELECT tplnr_fl,contr,cmnum,varia,season,datab,datbi
          FROM /agri/glflcma
          INTO TABLE @DATA(li_glflca)
          FOR ALL ENTRIES IN @li_glflot            "#EC CI_NO_TRANSFORM
          WHERE tplnr_fl EQ @li_glflot-tplnr_fl
            AND cmnum    IN @lr_cmnum
            AND datab    IN @lr_datab
            AND datbi    IN @lr_datbi.
      ENDIF.

      IF li_glflca IS NOT INITIAL.
        SELECT aufnr FROM /agri/fmfphdr
          INTO TABLE @DATA(li_fphdr)
          FOR ALL ENTRIES IN @li_glflca            "#EC CI_NO_TRANSFORM
          WHERE aufnr    IN @lr_aufnr
            AND autyp    EQ @gc_document_category-process_order "'AO'
            AND tplnr_fl EQ @li_glflca-tplnr_fl
            AND contr    EQ @li_glflca-contr
            AND cmnum    EQ @li_glflca-cmnum
            AND varia    EQ @li_glflca-varia
            AND cpros    IN @lr_cpros
            AND class    EQ @gc_application-farming.
      ENDIF.

      IF li_fphdr IS NOT INITIAL.
*-- Fetch Process Order Item Details
        SELECT aufnr,aufnr_to,actdt,schdt
          FROM /agri/fmfpitm
          INTO TABLE @DATA(li_fpitm)
          FOR ALL ENTRIES IN @li_fphdr             "#EC CI_NO_TRANSFORM
          WHERE aufnr    EQ @li_fphdr-aufnr
            AND aufnr_to IN @lr_aufnr.
      ENDIF.

*-- if Task Order(AUFNR_TO) is empty then Delete Process Order Item data
      SORT: li_fpitm BY aufnr_to.
      DELETE li_fpitm WHERE aufnr_to EQ space.
    ENDIF.

    IF li_fpitm IS NOT INITIAL.
*-- Fetch Task Order Hader Data
      SELECT * FROM /agri/fmfphdr
        INTO TABLE @DATA(li_torder)
        FOR ALL ENTRIES IN @li_fpitm               "#EC CI_NO_TRANSFORM
        WHERE aufnr EQ @li_fpitm-aufnr_to
          AND autyp EQ @gc_document_category-task_order "'TO'
          AND matnr IN @lr_matnr.
    ELSEIF lr_aufnr IS NOT INITIAL.
*-- Fetch Task Order Hader Data
      SELECT * FROM /agri/fmfphdr
        INTO TABLE li_torder
        WHERE aufnr IN lr_aufnr
          AND matnr IN lr_matnr.
    ENDIF.

    IF li_torder IS NOT INITIAL.
*-- Fetch Task Order Object Number to check Order Status
      SELECT aufnr,
             objnr
        FROM aufk
        INTO TABLE @DATA(li_aufk)                  "#EC CI_NO_TRANSFORM
        FOR ALL ENTRIES IN @li_torder
        WHERE aufnr = @li_torder-aufnr.

*-- Fetch Task Order Item data
      SELECT aufnr,posnr,vornr,rueck,gamng,gwemg,tomng
        FROM /agri/fmfpitm
        INTO TABLE @DATA(li_toitm)
        FOR ALL ENTRIES IN @li_torder              "#EC CI_NO_TRANSFORM
        WHERE aufnr EQ @li_torder-aufnr.

*-- Fetch Material Text
      SELECT matnr,maktx
        FROM makt
        INTO TABLE @DATA(li_makt)
        FOR ALL ENTRIES IN @li_torder              "#EC CI_NO_TRANSFORM
        WHERE matnr EQ @li_torder-matnr.

*-- Get Order Status Fixed Values
      CALL FUNCTION 'DD_DOMVALUES_GET'
        EXPORTING
          domname        = '/AGRI/FMCSTAT'
          text           = 'X'
        TABLES
          dd07v_tab      = li_dd07v
        EXCEPTIONS
          wrong_textflag = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
    ENDIF.

    SORT: li_aufk BY aufnr,
          li_makt BY matnr.

    LOOP AT li_torder INTO DATA(lwa_torder).

      MOVE-CORRESPONDING lwa_torder TO lwa_fphdr.
      CLEAR: lv_subrc,lwa_message.
      CALL METHOD /agri/cl_odata_authorizations=>fmfp_authority_check
        EXPORTING
          iv_activity = /agri/cl_odata_authorizations=>c_authorization_activity-display
          is_fphdr    = lwa_fphdr
        IMPORTING
          ev_subrc    = lv_subrc
          es_message  = lwa_message.
      IF lv_subrc <> 0.
        APPEND lwa_message TO li_messages.
        CONTINUE.
      ENDIF.

*-- Check Order Deletion check
      READ TABLE li_aufk INTO DATA(lwa_aufk)
           WITH KEY aufnr = lwa_torder-aufnr BINARY SEARCH.
      IF sy-subrc <> 0.
        CONTINUE.
      ELSE.
        CLEAR: lv_subrc,lv_cstat.
*-- Check Order - Closed , Techo
        order_status_check( EXPORTING iv_aufnr = lwa_aufk-aufnr
                                      iv_objnr = lwa_aufk-objnr
                            IMPORTING ev_sttxt = lwa_entityset-sttxt
                                      ev_cstat = lv_cstat
                                      ev_subrc = lv_subrc ).
        IF lv_subrc <> 0.
          CONTINUE.
        ENDIF.
      ENDIF.

      MOVE-CORRESPONDING lwa_torder TO lwa_entityset.
      lwa_entityset-cstat = lv_cstat.

      CLEAR: lwa_dd07v.
      READ TABLE li_dd07v INTO lwa_dd07v
           WITH KEY domvalue_l = lwa_entityset-cstat BINARY SEARCH.
      IF sy-subrc = 0.
        lwa_entityset-cstxt = lwa_dd07v-ddtext.
      ENDIF.

      READ TABLE li_fpitm INTO DATA(lwa_fpitm)
           WITH KEY aufnr_to = lwa_torder-aufnr BINARY SEARCH.
      IF sy-subrc = 0.
        lwa_entityset-actdt = lwa_fpitm-actdt.
        lwa_entityset-schdt = lwa_fpitm-schdt.
      ENDIF.

      IF lwa_entityset-schdt IS NOT INITIAL.
        lwa_entityset-schdl = c_true.
      ENDIF.

      READ TABLE li_makt INTO DATA(lwa_makt)
           WITH KEY matnr = lwa_torder-matnr BINARY SEARCH.
      IF sy-subrc = 0.
        lwa_entityset-maktx = lwa_makt-maktx.
      ENDIF.

*-- Get Order Quantity's
      CLEAR: lwa_entityset-gamng,lwa_entityset-gwemg.
      LOOP AT li_toitm INTO DATA(lwa_toitm) WHERE aufnr = lwa_entityset-aufnr.
        lwa_entityset-gamng = lwa_entityset-gamng + lwa_toitm-gamng.
        lwa_entityset-gwemg = lwa_entityset-gwemg + lwa_toitm-gwemg.
      ENDLOOP.

      lwa_entityset-comng = lwa_entityset-gamng - lwa_entityset-gwemg.

      APPEND lwa_entityset TO et_entityset.
      CLEAR: lwa_entityset,lwa_makt,lwa_torder.
    ENDLOOP.

    IF et_entityset IS INITIAL.
      CLEAR: lwa_message.
      lwa_message-msgty = c_msg_type-error.
      lwa_message-msgid = '/AGRI/GLOBAL'.
      lwa_message-msgno = '751'.
      add_messages_to_msg_container( EXPORTING is_message = lwa_message ).
      RETURN.
    ENDIF.

    IF li_messages IS NOT INITIAL.
      SORT: li_messages BY msgid msgno.
      DELETE ADJACENT DUPLICATES FROM li_messages COMPARING msgid msgno.
      add_messages_to_msg_container( EXPORTING it_messages = li_messages ).
    ENDIF.

*-- Inlinecount
    IF io_tech_request_context->has_inlinecount( ) = abap_true.
      DESCRIBE TABLE et_entityset LINES es_response_context-inlinecount.
    ELSE.
      CLEAR es_response_context-inlinecount.
    ENDIF.

*-- The method for $top and $skip Query Options
    IF is_paging IS NOT INITIAL.
      CALL METHOD /iwbep/cl_mgw_data_util=>paging
        EXPORTING
          is_paging = is_paging
        CHANGING
          ct_data   = et_entityset.
    ENDIF.

*-- The method for Orderby condition
    IF it_order IS NOT INITIAL.
      CALL METHOD /iwbep/cl_mgw_data_util=>orderby
        EXPORTING
          it_order = it_order
        CHANGING
          ct_data  = et_entityset.
    ENDIF.


  ENDMETHOD.
ENDCLASS.
