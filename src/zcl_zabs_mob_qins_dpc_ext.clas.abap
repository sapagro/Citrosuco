class ZCL_ZABS_MOB_QINS_DPC_EXT definition
  public
  inheriting from ZCL_ZABS_MOB_QINS_DPC
  create public .

public section.

  methods CROPSEASON_EXT_GET
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
    exporting
      !ET_ENTITYSET type ZCL_ZABS_MOB_QINS_MPC=>TT_CROPSEASON_EXT .
  methods GET_DELTA_TOKEN
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
      !MR_SERVICE_DOCUMENT_NAME type ref to STRING
      !MR_SERVICE_VERSION type ref to NUMC4
      !IT_ENTITYSET type STANDARD TABLE
    exporting
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
    raising
      /IWBEP/CX_QRL_BASE
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods DELETE_FROM_DATABASE
    importing
      !TABNAME type CSEQUENCE default 'INDX'
      !CLIENT type MANDT optional
      !AREA type RELID default 'ID'
      !ID type CLIKE optional
      !GENERIC_KEY type ABAP_BOOL default ABAP_FALSE
      !CLIENT_SPECIFIED type ABAP_BOOL default ABAP_TRUE .
  methods GET_QUAL_CHAR_V1
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
    exporting
      !ET_ENTITYSET type ZCL_ZABS_MOB_QINS_MPC=>TT_QUALCHAREXT .
  methods GET_SKIP_TOKEN
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
    exporting
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
      !EV_TOTRECORDS type I
    changing
      !CT_ENTITYSET type STANDARD TABLE .
  methods GET_EMPLOYEE_ROLES
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_ENTITYSET type ZCL_ZABS_MOB_QINS_MPC=>TT_EMPLOYEE_ROLE .
  methods GET_EMPLOYEE_USERS
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_ENTITYSET type ZCL_ZABS_MOB_QINS_MPC=>TT_EMPLOYEE_USERS .
  methods GET_ROUTE_TERRAIN_DTLS
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
    exporting
      !ET_ENTITYSET type ZCL_ZABS_MOB_QINS_MPC=>TT_WOROUTETERRAINEXTEND
      !ET_RTTRN type ZCL_ZABS_MOB_QINS_MPC=>TT_WOROUTETERRAINEXTEND
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods GET_QUALCHARF4EXT
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
    exporting
      !ET_ENTITYSET type ZCL_ZABS_MOB_QINS_MPC=>TT_QULACHRF4_EXT
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION .
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
protected section.

  methods GET_QUALTASKORD
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_ENTITYSET type ZCL_ZABS_MOB_QINS_MPC=>TT_QUALTASKORD_EXT .
  methods GET_TERRQUASET
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
    exporting
      !ET_ENTITYSET type ZCL_ZABS_MOB_QINS_MPC=>TT_TERRAINQUA_EXT .
  methods INSPPOINTS_RECORDING_UPDATE
    importing
      !IM_T_QULACHRF4 type ZCL_ZABS_MOB_QINS_MPC=>TT_QULACHRF4_EXT
      !IM_T_QULACHR type ZCL_ZABS_MOB_QINS_MPC=>TT_QUALCHAREXT
    exporting
      !EX_T_RETTAB type BAPIRET2_T
    changing
      !CH_S_INSPPOINT type /AGRI/CL_MOBILE_MPC=>TS_INSPECTIONPOINT
      !CH_S_QUALCHAR type ZCL_ZABS_MOB_QINS_MPC=>TS_QUALCHAREXT .
  methods UPLOADING_IMGEXT_INSP_POINT
    importing
      !IM_S_IMGUPLOAD type ZCL_ZABS_MOB_QINS_MPC=>TS_UPLOAD_EXT
      !IM_S_QUACHAR type ZCL_ZABS_MOB_QINS_MPC=>TS_QUALCHAREXT
      !IM_S_INSPPOINT type /AGRI/CL_MOBILE_MPC=>TS_INSPECTIONPOINT
    exporting
      !ET_MESSAGES type /AGRI/T_GPROLOG .
  methods GET_QUALCHAR_FETCH
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
    exporting
      !ET_ENTITYSET type ZCL_ZABS_MOB_QINS_MPC=>TT_QUALCHARFETCH_EXT .

  methods CROPSEASON_EXTSE_GET_ENTITYSET
    redefinition .
  methods EMPLOYEE_ROLESET_GET_ENTITYSET
    redefinition .
  methods EMPLOYEE_USERSSE_GET_ENTITYSET
    redefinition .
  methods QUALCHAREXTSET_GET_ENTITYSET
    redefinition .
  methods QUALCHARFETCH_EX_GET_ENTITYSET
    redefinition .
  methods QUALOPREXTENDSET_GET_ENTITYSET
    redefinition .
  methods QUALTASKORD_EXTS_GET_ENTITYSET
    redefinition .
  methods QULACHRF4_EXTSET_GET_ENTITYSET
    redefinition .
  methods TERRAINQUA_EXTSE_GET_ENTITYSET
    redefinition .
  methods WOROUTETERRAINEX_GET_ENTITYSET
    redefinition .
  methods WOROUTE_EXTSET_GET_ENTITYSET
    redefinition .
private section.

  methods REQUEST_HEADER .
  methods GET_QUALITY_OPERATIONS_EXTEND
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
    exporting
      !ET_ENTITYSET type ZCL_ZABS_MOB_QINS_MPC=>TT_QUALOPREXTEND .
ENDCLASS.



CLASS ZCL_ZABS_MOB_QINS_DPC_EXT IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~changeset_begin.

*-- Data Declarations
    DATA: ls_operation_info  TYPE /iwbep/s_mgw_operation_info.

    CLEAR: cv_defer_mode.
    cv_defer_mode = abap_true.

*-- Looping Entityset name in the Operation Table
    LOOP AT it_operation_info INTO ls_operation_info.
      IF NOT ( "ls_operation_info-entity_set  = 'WoActivities_extSet'   OR
               "ls_operation_info-entity_set  = 'WoComponentsSet'       OR
               "ls_operation_info-entity_set  = 'WoOperations_extSet'   OR
               "ls_operation_info-entity_set  = 'HarvestOrderSet'       OR
               "ls_operation_info-entity_set  = 'HarvestOrderItemSet'   OR
               ls_operation_info-entity_set  = 'QualCharExtSet'        OR
               "ls_operation_info-entity_set  = 'MeasurementsSet'       OR
               "ls_operation_info-entity_set  = 'MeasurementsExtendSet' OR
               "ls_operation_info-entity_set  = 'QualChrF4Set'          OR
               "ls_operation_info-entity_set  = 'InspectionPointSet'    OR
               ls_operation_info-entity_set  = 'QulaChrF4_extSet'      OR
               "ls_operation_info-entity_set  = 'AttributesF4_extSet'   OR
               "ls_operation_info-entity_set  = 'Terrain_extSet'        OR
               "ls_operation_info-entity_set  = 'TerrainQua_extSet'     OR
               ls_operation_info-entity_set  = 'TerrainQua_extSet'     OR
               ls_operation_info-entity_set  = 'QualTaskOrd_extSet'    OR
               "ls_operation_info-entity_set  = 'QualCharFetch_extSet'  OR
               "ls_operation_info-entity_set  = 'WoComponents_extSet'   OR
               ls_operation_info-entity_set  = 'Upload_extSet'
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
  TYPES: BEGIN OF ls_qapp,
           prueflos TYPE qapp-prueflos,
           probenr  TYPE qapp-probenr,
         END OF ls_qapp.

*  CONSTANTS: lc_objtyp TYPE oj_name       VALUE '/AGRI/GLMD',
*             lc_astat  TYPE /agri/glastat VALUE 'A'.

  DATA:
*-- Class Objects
    lref_msg_container     TYPE REF TO /iwbep/if_message_container,
    lo_update_context      TYPE REF TO /iwbep/if_mgw_req_entity_u,
*-- Tables
    lt_return              TYPE bapiret2_t,
*    lt_measurement         TYPE zcl_zabs_agri_mobile_e_mpc=>tt_measurementsextend,
*    lt_qualchr             TYPE zcl_zabs_mob_qins_mpc=>tt_qualchar,  "mn
    lt_qualchrext          TYPE zcl_zabs_mob_qins_mpc=>tt_qualcharext,
    ls_qualchrfetch        TYPE zcl_zabs_mob_qins_mpc=>ts_qualcharfetch_ext,
    lt_qualchrf4           TYPE zcl_zabs_mob_qins_mpc=>tt_qulachrf4_ext,
    lt_upload_ext          TYPE zcl_zabs_mob_qins_mpc=>tt_upload_ext,
*    lt_mdhdr               TYPE /agri/t_glmdhdr,
*    mt_athdr_tmp           TYPE /agri/t_gathdr,
*    lt_line                 TYPE /irm/t_gline,
*    lt_mdatv               TYPE /agri/t_glmdatv,
*    lt_atinn               TYPE /agri/t_gatinn,
*    lt_mddoc               TYPE /agri/t_glmd_doc,
    lt_messages            TYPE /agri/t_gprolog,
    lv_ldate               TYPE sy-datum,
    lv_hdate               TYPE sy-datum,
*-- Structures
*    ls_measurement         TYPE zcl_zabs_mob_qins_mpc=>ts_measurementsextend, "mn
    ls_qualchrf4           TYPE zcl_zabs_mob_qins_mpc=>ts_qulachrf4_ext,
    ls_qualcharext         TYPE zcl_zabs_mob_qins_mpc=>ts_qualcharext,
*    ls_qualchar            TYPE zcl_zabs_mob_qins_mpc=>ts_qualchar, mn
    ls_inspectionpointset  TYPE /agri/cl_mobile_mpc=>ts_inspectionpoint,
    ls_upload_ext          TYPE zcl_zabs_mob_qins_mpc=>ts_upload_ext,
    ls_inspoper            TYPE qapo-vornr,
    ls_inspchar            TYPE qamv-merknr,
    ls_changeset_request   TYPE /iwbep/if_mgw_appl_types=>ty_s_changeset_request,
*    ls_mdhdr               TYPE /agri/s_glmdhdr,
*    ls_mdatv               TYPE /agri/s_glmdatv,
    ls_messages            TYPE /agri/s_gprolog,
*    ls_mditm               TYPE /agri/s_glmditm,
*    ls_mddoc               TYPE /agri/s_glmd_doc,
*    ls_line                LIKE LINE OF lt_line, "mn
*    lwa_attribute          TYPE auspdata,
*-- Variables
    lv_insplot             TYPE bapi2045l2-insplot,
    lv_inspoper            TYPE bapi2045l2-inspoper,
    lv_tmpdate             TYPE string,
    lwa_changeset_response TYPE /iwbep/if_mgw_appl_types=>ty_s_changeset_response,
    lv_entity_type         TYPE string,
    lv_atinn               TYPE cabn-atinn,
    lv_sample              TYPE qibpprobe,
    lv_aslvl               TYPE /agri/glaslvl,
    lv_msgno               TYPE symsgno,
    lv_x                   TYPE xfeld VALUE abap_true,
    lv_objkey	             TYPE /irm/gobjkey,
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

*    IF ( lv_entity_type EQ 'MeasurementsExtend' OR
*         lv_entity_type EQ 'Measurements' ).
*
*      " Operation Number in current changeset
**--------------------------------------------------------------------*
**        Measurement Document Creation
**--------------------------------------------------------------------*
**      WHEN 'Measurements'.
*      IF lv_entity_type EQ  'MeasurementsExtend'.           "#EC NOTEXT
*        ls_changeset_request-entry_provider->read_entry_data( "#EC CI_FLDEXT_OK
*                                   IMPORTING es_data = ls_measurement ).
*
*        copy_data_to_ref( EXPORTING is_data = ls_measurement "#EC CI_FLDEXT_OK
*             CHANGING cr_data = lwa_changeset_response-entity_data ).
*        INSERT lwa_changeset_response INTO TABLE ct_changeset_response.
*
*        APPEND ls_measurement TO lt_measurement.
*
*      ENDIF.
*    ENDIF.
*    IF lv_entity_type EQ  'QualChar'.                       "#EC NOTEXT
*      ls_changeset_request-entry_provider->read_entry_data( "#EC CI_FLDEXT_OK
*                                 IMPORTING es_data = ls_qualchar ).
*
*      APPEND ls_qualchar TO lt_qualchr.
*      copy_data_to_ref( EXPORTING is_data = ls_qualchar "#EC CI_FLDEXT_OK
*           CHANGING cr_data = lwa_changeset_response-entity_data ).
*      INSERT lwa_changeset_response INTO TABLE ct_changeset_response.
*
*    ENDIF.

    IF lv_entity_type EQ  'QualCharExt'.                    "#EC NOTEXT
      ls_changeset_request-entry_provider->read_entry_data( "#EC CI_FLDEXT_OK
                                 IMPORTING es_data = ls_qualcharext ).

      APPEND ls_qualcharext TO lt_qualchrext.
      copy_data_to_ref( EXPORTING is_data = ls_qualcharext "#EC CI_FLDEXT_OK
           CHANGING cr_data = lwa_changeset_response-entity_data ).
      INSERT lwa_changeset_response INTO TABLE ct_changeset_response.

    ENDIF.

    IF lv_entity_type EQ  'QualCharFetch_ext'.              "#EC NOTEXT

      ls_changeset_request-entry_provider->read_entry_data( "#EC CI_FLDEXT_OK
                                 IMPORTING es_data = ls_qualchrfetch ).


      MOVE-CORRESPONDING ls_qualchrfetch TO ls_qualcharext.
      APPEND ls_qualcharext TO lt_qualchrext.

      copy_data_to_ref( EXPORTING is_data = ls_qualchrfetch "#EC CI_FLDEXT_OK
           CHANGING cr_data = lwa_changeset_response-entity_data ).
      INSERT lwa_changeset_response INTO TABLE ct_changeset_response.

    ENDIF.

    IF lv_entity_type EQ  'QulaChrF4_ext'.                  "#EC NOTEXT
      ls_changeset_request-entry_provider->read_entry_data( "#EC CI_FLDEXT_OK
                                 IMPORTING es_data = ls_qualchrf4 ).

      APPEND ls_qualchrf4 TO lt_qualchrf4.
      copy_data_to_ref( EXPORTING is_data = ls_qualchrf4 "#EC CI_FLDEXT_OK
           CHANGING cr_data = lwa_changeset_response-entity_data ).
      INSERT lwa_changeset_response INTO TABLE ct_changeset_response.

    ENDIF.

    IF lv_entity_type EQ 'InspectionPoint'.                 "#EC NOTEXT
      ls_changeset_request-entry_provider->read_entry_data( IMPORTING es_data = ls_inspectionpointset ).
*          ASSIGN ls_inspectionpointset TO <ls_data>.
      copy_data_to_ref( EXPORTING is_data = ls_inspectionpointset
                   CHANGING cr_data = lwa_changeset_response-entity_data ).

      INSERT lwa_changeset_response INTO TABLE ct_changeset_response.
*        RETURN.
    ENDIF.

*
    IF lv_entity_type EQ 'Upload_ext'.                      "#EC NOTEXT

      ls_changeset_request-entry_provider->read_entry_data(
      IMPORTING es_data = ls_upload_ext ).

      APPEND ls_upload_ext TO lt_upload_ext.

      lv_upload = abap_true.
*          ASSIGN ls_inspectionpointset TO <ls_data>.
      copy_data_to_ref(
         EXPORTING is_data = ls_upload_ext
         CHANGING  cr_data = lwa_changeset_response-entity_data ).

      INSERT lwa_changeset_response INTO TABLE ct_changeset_response.

    ENDIF.

  ENDLOOP.

**--------------------------------------------------------------------*
**             Inspection Lot
**--------------------------------------------------------------------*
  IF lt_qualchrext IS NOT INITIAL.

    CALL METHOD me->insppoints_recording_update
      EXPORTING
        im_t_qulachrf4 = lt_qualchrf4
        im_t_qulachr   = lt_qualchrext
      IMPORTING
        ex_t_rettab    = lt_return
      CHANGING
        ch_s_insppoint = ls_inspectionpointset
        ch_s_qualchar  = ls_qualcharext.

*--------------------------------------------------------------------*
*       Inspection Points Image Upload
*--------------------------------------------------------------------*

    IF lv_upload IS NOT INITIAL.

      LOOP AT lt_upload_ext ASSIGNING FIELD-SYMBOL(<fs_upload_ext>).
        CALL METHOD me->uploading_imgext_insp_point
          EXPORTING
            im_s_imgupload = <fs_upload_ext>
            im_s_insppoint = ls_inspectionpointset
            im_s_quachar   = ls_qualcharext
          IMPORTING
            et_messages    = lt_messages.
      ENDLOOP.

*-- Preparing the messages in the container
      IF lt_messages IS NOT INITIAL.
        exception_messages( EXPORTING it_messages = lt_messages ).
      ENDIF.

    ENDIF.

  ELSE.
    CALL METHOD super->/iwbep/if_mgw_appl_srv_runtime~changeset_process
      EXPORTING
        it_changeset_request  = it_changeset_request
      CHANGING
        ct_changeset_response = ct_changeset_response.

  ENDIF.

**-----------------------------------------------------------------------------
**-----------------------------------------------------------------------------
**-- Calling of respective methods for data processing
**-----------------------------------------------------------------------------
**-----------------------------------------------------------------------------
*
*  IF lt_measurement IS NOT INITIAL.
*
*
*    READ TABLE lt_measurement INTO ls_measurement INDEX 1.
*    lv_aslvl = ls_measurement-aslvl.
*
*    SELECT SINGLE mdtyp
*            FROM /agri/glagha
*            INTO @ls_mdhdr-mdtyp
*            WHERE class = @ls_measurement-class
*              AND aslvl = @lv_aslvl.
*
*    IF ls_measurement-tplnr_fl IS NOT INITIAL.
*      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
*        EXPORTING
*          input      = ls_measurement-tplnr_fl
*        IMPORTING
*          output     = ls_measurement-tplnr_fl
*        EXCEPTIONS
*          not_found  = 1
*          not_active = 2
*          OTHERS     = 3.
*      IF sy-subrc <> 0.
** Implement suitable error handling here
*      ENDIF.
*    ENDIF.
*
*    SELECT SINGLE tplnr_fl, contr, datab, datbi         "#EC CI_NOORDER
*       FROM /agri/glflcma
*       INTO (@ls_mdhdr-tplnr_fl,@ls_mdhdr-contr,
*             @ls_mdhdr-datab,   @ls_mdhdr-datbi)
*       WHERE tplnr_fl =  @ls_measurement-tplnr_fl
*         AND cmnum    =  @ls_measurement-cmnum
*         AND datab    <= @ls_measurement-mdate
*         AND datbi    >= @ls_measurement-mdate
*         AND astat    =  @lc_astat "Active
*         AND loevm    <> @abap_true.
*
*    IF lv_aslvl = 'I'.
*      SELECT SINGLE mdtyp
*        FROM /agri/glagha
*        INTO ls_mdhdr-mdtyp
*        WHERE class = ls_measurement-class.
*      ls_mdhdr-equnr = ls_measurement-equnr.
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = ls_mdhdr-equnr
*        IMPORTING
*          output = ls_mdhdr-equnr.
*
*    ENDIF.
*
*    ls_mdhdr-aslvl    = ls_measurement-aslvl.
*    ls_mdhdr-tplnr_fl = ls_measurement-tplnr_fl.
*    ls_mdhdr-cmnum    = ls_measurement-cmnum.
*    ls_mdhdr-mpgrp    = ls_measurement-class.
*    TRANSLATE ls_measurement-muser TO UPPER CASE.
*    ls_mdhdr-muser    = ls_measurement-muser.
*    ls_mdhdr-mdate    = ls_measurement-mdate.
*    ls_mdhdr-mtime    = ls_measurement-mtime.
*    ls_mdhdr-updkz    = 'I'.
*    ls_mdhdr-mdocm    = 'New'.
*    ls_mdhdr-mandt    = sy-mandt.
*    ls_mdhdr-ivtyp    = ls_mdhdr-mdtyp.
*    ls_mddoc-mdocm    = ls_mdhdr-mdocm.
*    ls_mddoc-updkz    = ls_mdhdr-updkz.
*
*    ls_mdhdr-zzimei1    = ls_measurement-zzimei1.
*    ls_mdhdr-zzimei2    = ls_measurement-zzimei2.
*    ls_mdhdr-zbadge     = ls_measurement-zbadge.
*    MOVE-CORRESPONDING ls_mdhdr TO ls_mddoc-x-mdhdr.
*    APPEND ls_mdhdr TO lt_mdhdr.
*
*    LOOP AT lt_measurement INTO ls_measurement.
*
*      CALL FUNCTION '/AGRI/G_CONV_EXIT_ATINN_INPUT'
*        EXPORTING
*          i_input  = ls_measurement-atnam
*        IMPORTING
*          o_output = lv_atinn.
*
*      APPEND lv_atinn TO lt_atinn.
*    ENDLOOP.
*
*    LOOP AT lt_measurement INTO ls_measurement.
*      ls_mditm-atnam    = ls_measurement-atnam.
*      ls_mditm-atwrt    = ls_measurement-atwrt.
*      ls_mditm-cunit    = ls_measurement-cunit.
*      ls_mditm-updkz    = 'I'.
*
*      IF ls_mditm-atnam IS NOT INITIAL.
*        CALL FUNCTION '/AGRI/G_CONV_EXIT_ATINN_INPUT'
*          EXPORTING
*            i_input  = ls_mditm-atnam
*          IMPORTING
*            o_output = lv_atinn. "ls_mditm-atinn.
*      ENDIF.
*
*      IF lv_atinn IS INITIAL.
*        lv_atinn = ls_mditm-atinn.
*      ENDIF.
*
*      CALL METHOD /agri/cl_gattr_utils=>attributes_read
*        EXPORTING
*          i_agtyp                       = 'X90'
*          i_datuv                       = sy-datum
*          i_language                    = sy-langu
*          it_atinn                      = lt_atinn
*        IMPORTING
*          et_attr_header                = mt_athdr_tmp
*        EXCEPTIONS
*          invalid_parameter_combination = 1
*          OTHERS                        = 2.
*      IF sy-subrc = 0.
*        SORT mt_athdr_tmp BY atinn.
*      ENDIF.
*
*      CALL METHOD /agri/cl_gattr_utils=>attribute_value_check
*        EXPORTING
*          i_agtyp     = 'X90' "c_agtyp-measurements
*          i_atinn     = lv_atinn
*          i_atwrt     = ls_mditm-atwrt
**         i_sdpde     = gs_ootyp-sdpde
*        IMPORTING
*          et_messages = lt_messages[]
*        CHANGING
*          cs_attr_val = lwa_attribute
*          c_valid     = lv_x. "lv_valid.
*      IF lv_x = abap_true.
*        MOVE-CORRESPONDING lwa_attribute TO ls_mditm.
*        IF lwa_attribute-atflv IS NOT INITIAL.
**          CLEAR ls_mditm-atwrt.
*        ENDIF.
*      ELSE.
*
*        READ TABLE mt_athdr_tmp ASSIGNING FIELD-SYMBOL(<fs_athdr>)
*                                WITH KEY atinn = lv_atinn
*                                BINARY SEARCH.
*        IF sy-subrc = 0.
*          IF <fs_athdr>-atfor = 'DATE'.
*            lv_tmpdate = <fs_athdr>-atsch.
*            REPLACE 'AAAA' IN lv_tmpdate WITH ls_mditm-atwrt+6(4).
*            REPLACE 'MM'   IN lv_tmpdate WITH ls_mditm-atwrt+3(2).
*            REPLACE 'DD'   IN lv_tmpdate WITH ls_mditm-atwrt(2).
*            ls_mditm-atwrt = lv_tmpdate.
*          ELSEIF <fs_athdr>-atfor = 'NUM'.
*            REPLACE '.' IN ls_mditm-atwrt WITH ','.
*            CALL METHOD /agri/cl_gattr_utils=>attribute_value_check
*              EXPORTING
*                i_agtyp     = 'X90' "c_agtyp-measurements
*                i_atinn     = lv_atinn
*                i_atwrt     = ls_mditm-atwrt
**               i_sdpde     = gs_ootyp-sdpde
*              IMPORTING
*                et_messages = lt_messages[]
*              CHANGING
*                cs_attr_val = lwa_attribute
*                c_valid     = lv_x. "lv_valid.
*            IF lv_x EQ 'X'.
*              lv_tmpdate = abap_true.
*              MOVE-CORRESPONDING lwa_attribute TO ls_mditm.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*
*        IF lv_tmpdate IS INITIAL.
*          MOVE-CORRESPONDING lwa_attribute TO ls_mditm.
*        ENDIF.
**        CLEAR: ls_mditm-atwrt.
*      ENDIF.
*
*      MOVE-CORRESPONDING ls_mditm TO ls_mdatv.
*      ls_mdatv-mdocm = 'New'.                               "#EC NOTEXT
*      ls_mdatv-mandt = sy-mandt.
*      IF ls_mdatv-atinn IS INITIAL.
*        ls_mdatv-atinn = lv_atinn.
*      ENDIF.
*      APPEND ls_mdatv TO lt_mdatv.
*      APPEND ls_mdatv TO ls_mddoc-x-mdatv.
*      CLEAR: ls_measurement, ls_mditm, ls_mdatv,
*             lwa_attribute, lv_atinn, lv_tmpdate.
*      REFRESH: mt_athdr_tmp.
*    ENDLOOP.
*
*    APPEND ls_mddoc TO lt_mddoc.
*
*    SELECT objtyp, notyp
*      FROM /agri/tganto
*      INTO TABLE @DATA(lt_tgant)
*         WHERE objtyp = @lc_objtyp.
*    IF lt_tgant IS INITIAL.
*
*      ls_messages-msgty = 'E'.
**      ls_messages-msgv1 = 'Defects occured during confirmation'. "#EC NOTEXT
*      ls_messages-msgid = '/AGRI/GLOBAL'.
*      ls_messages-msgno = '009'.
*      APPEND ls_messages TO lt_messages.
*      exception_messages( EXPORTING it_messages = lt_messages ).
*      EXIT.
*    ENDIF.
*    CALL FUNCTION '/AGRI/GLMD_CREATE_MASS'
*      EXPORTING
*        it_mdhdr          = lt_mdhdr
*        it_mdatv          = lt_mdatv
*      IMPORTING
*        et_mddoc          = lt_mddoc
*        et_messages       = lt_messages
*      EXCEPTIONS
*        inconsistent_data = 1
*        OTHERS            = 2.
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*    ENDIF.
*
**  IF sy-subrc = 0.
*    lv_msgno = sy-msgno.
*    lref_msg_container  = mo_context->get_message_container( ).
*    lref_msg_container->add_message(
*    EXPORTING iv_msg_type               = sy-msgty
*              iv_msg_id                 = sy-msgid
*              iv_msg_number             = lv_msgno
*              iv_msg_v1                 = sy-msgv1
*              iv_msg_v2                 = sy-msgv2
*              iv_msg_v3                 = sy-msgv3
*              iv_msg_v4                 = sy-msgv4
*              iv_add_to_response_header = abap_true ).
*
*    IF ls_mdhdr-datab = '00000000' AND lv_aslvl NE 'T'
*                                   AND lv_aslvl NE 'I'.
*
*      ls_messages-msgid = '/AGRI/GLMD'.
*      ls_messages-msgno = '019'.
*      ls_messages-msgty = 'E'.
*
*      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
*        EXPORTING
*          input  = ls_measurement-tplnr_fl
*        IMPORTING
*          output = ls_measurement-tplnr_fl.
*
*      ls_messages-msgv1 = ls_mddoc-x-mdhdr-tplnr_fl.
*      ls_messages-msgv2 = ls_mddoc-x-mdhdr-cmnum.
*      ls_messages-msgv3 = ls_mddoc-x-mdhdr-mdate.
*      APPEND ls_messages TO lt_messages.
*
*    ENDIF.
*
**--- Adding Notes to MDoc
*    IF lt_mddoc IS NOT INITIAL.
*      DELETE lt_mddoc WHERE updkz NE 'X'.
*
*      LOOP AT lt_mddoc INTO ls_mddoc.
*
*        READ TABLE lt_measurement INTO ls_measurement INDEX 1.
*        lv_objkey = ls_mddoc-mdocm.
*        ls_line = ls_measurement-line.
*        APPEND ls_line TO lt_line.
*
*        notes_save(
*          EXPORTING
*            iv_objtyp = '/AGRI/GLMD'
*            iv_objkey = lv_objkey "Measurement doc. number
**               iv_subobj =
**               it_notes  =
*          IMPORTING
*            et_messages = lt_messages
*          CHANGING
*            ct_line   = lt_line
*        ).
*
*        CLEAR:lt_line,ls_line,lv_objkey.
*      ENDLOOP.
*    ENDIF.
*
*    exception_messages( EXPORTING it_messages = lt_messages ).
*

ENDMETHOD.


  METHOD cropseason_extse_get_entityset.
    " request_header call.
    CALL METHOD me->request_header.

    cropseason_ext_get(
        EXPORTING
          io_tech_request_context = io_tech_request_context
        IMPORTING
          et_entityset            = et_entityset ).

*** Getting Delta token
    CALL METHOD me->get_delta_token
      EXPORTING
        io_tech_request_context  = io_tech_request_context
        mr_service_document_name = mr_service_document_name
        mr_service_version       = mr_service_version
        it_entityset             = et_entityset
      IMPORTING
        es_response_context      = es_response_context.

  ENDMETHOD.


  METHOD cropseason_ext_get.

    "types.
    TYPES: BEGIN OF ltyp_flcma,
             tplnr_fl TYPE /agri/glflcma-tplnr_fl,
             contr    TYPE /agri/glflcma-contr,
             cmnum    TYPE /agri/glflcma-cmnum,
             season   TYPE /agri/glflcma-season,
             datab    TYPE /agri/glflcma-datab,
             datbi    TYPE /agri/glflcma-datbi,
             aarea    TYPE /agri/glflcma-aarea,
             msehi    TYPE /agri/glflcma-msehi,
             exhad    TYPE /agri/glflcma-exhad,
             eston    TYPE /agri/glflcma-eston,
             esuom    TYPE /agri/glflcma-esuom,
             ernam    TYPE /agri/glflcma-ernam,
             erdat    TYPE /agri/glflcma-erdat,
             erzet    TYPE /agri/glflcma-erzet,
             aenam    TYPE /agri/glflcma-aenam,
             aedat    TYPE /agri/glflcma-aedat,
             aezet    TYPE /agri/glflcma-aezet,
           END OF ltyp_flcma.

    TYPES: BEGIN OF ltyp_glfl,
             tplnr_fl TYPE /agri/glflot-tplnr_fl,
             pltxt    TYPE /agri/glflot-pltxt,
             strno    TYPE /agri/glflot-strno,
           END OF ltyp_glfl.

    TYPES : BEGIN OF ltyp_cmhdrt,
              cmnum TYPE /agri/glcmhdrt-cmnum,
              descr TYPE /agri/glcmhdrt-descr,
            END OF ltyp_cmhdrt.

    TYPES: BEGIN OF ls_charg_data,
             charg_in  TYPE /agri/glcharg_in,
             charg_inf TYPE /agri/glcharg_inf,
             ivdat     TYPE /agri/glivdat,
             menge     TYPE menge_d,
             meins     TYPE meins,
             exhad     TYPE /agri/glexhad,
             ageid     TYPE /agri/glageid,
           END OF ls_charg_data,
           lt_charg_tab TYPE TABLE OF ls_charg_data.

    "local declarations.
    DATA: lt_flcma       TYPE TABLE OF ltyp_flcma, "/agri/t_glflcma,
          lt_amhdr       TYPE TABLE OF /agri/glamhdr,
          ls_amhdr       TYPE /agri/glamhdr,
          lwa_charg_data TYPE ls_charg_data,
          ls_flcma       TYPE ltyp_flcma, "/agri/s_glflcma,
          lt_rtfla       TYPE TABLE OF /agri/glrtfla,
          lt_flot        TYPE TABLE OF ltyp_glfl, "/agri/t_glflot,
          ls_flot        TYPE ltyp_glfl, "/agri/s_glflot,
          lt_cmhdrt      TYPE TABLE OF ltyp_cmhdrt, "/agri/t_glcmhdrt,
          ls_cmhdrt      TYPE ltyp_cmhdrt, "/agri/s_glcmhdrt,
          ls_entityset   LIKE LINE OF et_entityset,
          lrt_pernr      TYPE RANGE OF persno,
          lrt_lifnr      TYPE RANGE OF lifnr,
          lr_pernr       LIKE LINE OF lrt_pernr,
          lv_persno      TYPE persno,
          lr_lifnr       LIKE LINE OF lrt_lifnr,
          lv_lifnr       TYPE lifnr,
          lt_rtusr       TYPE TABLE OF zabs_usrpernr.

    DATA : lt_filter            TYPE /iwbep/t_mgw_select_option,
           lo_filter            TYPE REF TO /iwbep/if_mgw_req_filter,
           lv_filter_str        TYPE string,
           lv_tabix             TYPE sy-tabix,
           ls_filter            TYPE /iwbep/s_mgw_select_option,
           lrt_date             TYPE RANGE OF datab,
           ls_date              LIKE LINE  OF lrt_date,
           lt_glcmhdr           TYPE TABLE OF /agri/glcmhdr,
           ls_glcmhdr           TYPE          /agri/glcmhdr,
           lrt_tplnr_fl         TYPE RANGE OF /agri/gltplnr_fl,
           ls_tplnr_fl          LIKE LINE OF lrt_tplnr_fl,
           lv_where             TYPE string,
           lv_category          TYPE /agri/glivcat,
           ls_messages          TYPE /agri/s_gprolog,
           lo_message_container TYPE REF TO /iwbep/if_message_container.

    lo_filter = io_tech_request_context->get_filter( ).
    lv_filter_str = lo_filter->get_filter_string( ).
    lt_filter = lo_filter->get_filter_select_options( ).

    lo_message_container = mo_context->get_message_container( ).

    LOOP AT lt_filter INTO ls_filter.

      CASE ls_filter-property.
        WHEN 'TPLNR_FL'.
          lo_filter->convert_select_option(
            EXPORTING
              is_select_option = ls_filter
            IMPORTING
              et_select_option = lrt_tplnr_fl ).
          READ TABLE lrt_tplnr_fl INTO ls_tplnr_fl INDEX 1.
        WHEN 'DATAB'.
          lo_filter->convert_select_option(
            EXPORTING
              is_select_option = ls_filter
            IMPORTING
              et_select_option = lrt_date ).
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
          READ TABLE lrt_date INTO ls_date INDEX 1.
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

    IF ls_date IS NOT INITIAL.
      CONCATENATE 'DATAB LE LS_DATE-LOW AND DATBI GE LS_DATE-LOW AND  LOEVM EQ SPACE'
             space INTO lv_where.

    ELSE.

      CONCATENATE 'LOEVM EQ SPACE'
             space INTO lv_where.
    ENDIF.

    IF ls_tplnr_fl IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
        EXPORTING
          input      = ls_tplnr_fl-low
        IMPORTING
          output     = ls_tplnr_fl-low
        EXCEPTIONS
          not_found  = 1
          not_active = 2
          OTHERS     = 3.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.


      CONCATENATE lv_where 'AND TPLNR_FL EQ LS_TPLNR_FL-LOW'
                  INTO lv_where SEPARATED BY space.
    ENDIF.

    "change -> Login User based validation to User+Employee based valiadtion
    IF lrt_pernr IS NOT INITIAL.
      SELECT *
      FROM zabs_usrpernr
      INTO TABLE lt_rtusr
      WHERE pernr IN lrt_pernr.
    ELSEIF lrt_lifnr IS NOT INITIAL.
      SELECT *
      FROM zabs_usrpernr
      INTO TABLE lt_rtusr
      WHERE lifnr IN lrt_lifnr.
    ENDIF.

    IF lt_rtusr IS NOT INITIAL.
      SELECT * FROM /agri/glrtfla
               INTO TABLE lt_rtfla
        FOR ALL ENTRIES IN lt_rtusr                "#EC CI_NO_TRANSFORM
        WHERE route EQ lt_rtusr-route.
    ENDIF.

    DATA : lv_cnval1   TYPE zabs_del_cnval,
           lv_prevdate TYPE p0001-begda,
           lv_days     TYPE t5a4a-dlydy.

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

    IF lt_rtfla IS NOT INITIAL.
      SELECT * FROM /agri/glflcma
              INTO CORRESPONDING FIELDS OF TABLE lt_flcma
              FOR ALL ENTRIES IN lt_rtfla          "#EC CI_NO_TRANSFORM
              WHERE tplnr_fl EQ lt_rtfla-tplnr_fl
                AND astat EQ 'A'
                AND datab LE sy-datum
                AND datbi GE lv_prevdate
                AND (lv_where).

    ENDIF.

    IF lt_flcma IS INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
        EXPORTING
          input  = ls_tplnr_fl-low
        IMPORTING
          output = ls_tplnr_fl-low.

      ls_messages-msgv1 = ls_tplnr_fl-low.
      lo_message_container->add_message(
      EXPORTING iv_msg_type               = 'E'
                iv_msg_id                 = '/AGRI/FMPR'
                iv_msg_number             = 162
                iv_msg_v1                 = ls_messages-msgv1
                iv_msg_v2                 = ls_messages-msgv2
                iv_msg_v3                 = ls_messages-msgv3
                iv_msg_v4                 = ls_messages-msgv4
                iv_add_to_response_header = abap_true ).
    ENDIF.

    IF lt_flcma IS NOT INITIAL.

      SELECT * FROM /agri/glamhdr             "#EC CI_ALL_FIELDS_NEEDED
        INTO TABLE lt_amhdr
         FOR ALL ENTRIES IN lt_flcma               "#EC CI_NO_TRANSFORM
       WHERE ivcat    EQ 'T'
         AND tplnr_fl EQ lt_flcma-tplnr_fl
         AND contr    EQ lt_flcma-contr
         AND acode    EQ 'D'.

      SELECT * FROM /agri/glcmhdr             "#EC CI_ALL_FIELDS_NEEDED
               INTO TABLE lt_glcmhdr
               FOR ALL ENTRIES IN lt_flcma         "#EC CI_NO_TRANSFORM
               WHERE cmnum EQ lt_flcma-cmnum.
      IF sy-subrc = 0.
        SORT lt_glcmhdr BY cmnum.
      ENDIF.

      SELECT tplnr_fl pltxt strno FROM  /agri/glflot
             INTO CORRESPONDING FIELDS OF TABLE lt_flot
             FOR ALL ENTRIES IN lt_flcma           "#EC CI_NO_TRANSFORM
             WHERE tplnr_fl EQ lt_flcma-tplnr_fl
              AND  loevm    = abap_true.
      IF sy-subrc = 0.
        SORT lt_flot BY tplnr_fl.
      ENDIF.

      SELECT cmnum descr FROM /agri/glcmhdrt
             INTO CORRESPONDING FIELDS OF TABLE lt_cmhdrt
             FOR ALL ENTRIES IN lt_flcma           "#EC CI_NO_TRANSFORM
             WHERE cmnum EQ lt_flcma-cmnum
               AND spras EQ sy-langu.
    ENDIF.

    LOOP AT lt_flcma INTO ls_flcma.
      READ TABLE lt_amhdr INTO ls_amhdr WITH KEY
                                tplnr_fl = ls_flcma-tplnr_fl
                                contr    = ls_flcma-contr.
      IF sy-subrc EQ 0.
        lv_tabix = sy-tabix.
      ELSE.
        MOVE-CORRESPONDING ls_flcma TO ls_entityset.

        READ TABLE lt_flot INTO ls_flot WITH KEY tplnr_fl = ls_flcma-tplnr_fl
                                        BINARY SEARCH."Displaying data in Tplnr_Fl
        IF sy-subrc EQ 0.
          ls_entityset-strno = ls_flot-strno.
        ENDIF.
        READ TABLE lt_glcmhdr INTO ls_glcmhdr WITH KEY cmnum = ls_flcma-cmnum
                                              BINARY SEARCH.
        IF sy-subrc = 0.
          ls_entityset-rmatnr = ls_glcmhdr-rmatnr.
        ENDIF.
        ls_entityset-tplnr_fl = ls_flcma-tplnr_fl.
        ls_entityset-contr =    ls_flcma-contr.
        ls_entityset-cmnum =    ls_flcma-cmnum.
        ls_entityset-season =   ls_flcma-season.
        ls_entityset-datab =    ls_flcma-datab.
        ls_entityset-datbi =    ls_flcma-datbi.
        ls_entityset-aarea =    ls_flcma-aarea.
        ls_entityset-msehi =    ls_flcma-msehi.
        ls_entityset-exhad =    ls_flcma-exhad.
        ls_entityset-eston =    ls_flcma-eston.
        ls_entityset-esuom =    ls_flcma-esuom.
        ls_entityset-ernam =    ls_flcma-ernam.
        ls_entityset-erdat =    ls_flcma-erdat.
        ls_entityset-erzet =    ls_flcma-erzet.
        ls_entityset-aenam =    ls_flcma-aenam.
        ls_entityset-aedat =    ls_flcma-aedat.
        ls_entityset-aezet =    ls_flcma-aezet.

        CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
          EXPORTING
            input  = ls_entityset-tplnr_fl
          IMPORTING
            output = ls_entityset-tplnr_fl.

        ls_entityset-pernr = lv_persno.
        ls_entityset-lifnr = lv_lifnr.

        APPEND ls_entityset TO et_entityset.
        CLEAR ls_entityset.
        CONTINUE.
      ENDIF.

      LOOP AT lt_amhdr INTO ls_amhdr FROM lv_tabix.
        IF sy-subrc EQ 0.

          IF ls_amhdr-acode EQ 'D'."c_action-remove.
            lwa_charg_data-ivdat = ls_amhdr-ivdat_ref.
            lwa_charg_data-menge = -1 * lwa_charg_data-menge.
            IF lwa_charg_data-ivdat LT sy-datum.
              lwa_charg_data-ageid = sy-datum - lwa_charg_data-ivdat.
            ENDIF.
          ENDIF.
          ls_entityset-charg_in  =    ls_amhdr-charg_in.


          MOVE-CORRESPONDING ls_flcma TO ls_entityset.

          READ TABLE lt_flot INTO ls_flot WITH KEY tplnr_fl = ls_flcma-tplnr_fl
                                          BINARY SEARCH."Displaying data in Tplnr_Fl
          IF sy-subrc EQ 0.
            ls_entityset-strno = ls_flot-strno.
          ENDIF.
          READ TABLE lt_glcmhdr INTO ls_glcmhdr WITH KEY cmnum = ls_flcma-cmnum
                                                BINARY SEARCH.
          IF sy-subrc = 0.
            ls_entityset-rmatnr = ls_glcmhdr-rmatnr.
          ENDIF.
          ls_entityset-tplnr_fl = ls_flcma-tplnr_fl.
          ls_entityset-contr =    ls_flcma-contr.
          ls_entityset-cmnum =    ls_flcma-cmnum.
          ls_entityset-season =   ls_flcma-season.
          ls_entityset-datab =    ls_flcma-datab.
          ls_entityset-datbi =    ls_flcma-datbi.
          ls_entityset-aarea =    ls_flcma-aarea.
          ls_entityset-msehi =    ls_flcma-msehi.
          ls_entityset-exhad =    ls_flcma-exhad.
          ls_entityset-eston =    ls_flcma-eston.
          ls_entityset-esuom =    ls_flcma-esuom.
          ls_entityset-ernam =    ls_flcma-ernam.
          ls_entityset-erdat =    ls_flcma-erdat.
          ls_entityset-erzet =    ls_flcma-erzet.
          ls_entityset-aenam =    ls_flcma-aenam.
          ls_entityset-aedat =    ls_flcma-aedat.
          ls_entityset-aezet =    ls_flcma-aezet.

          CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
            EXPORTING
              input  = ls_entityset-tplnr_fl
            IMPORTING
              output = ls_entityset-tplnr_fl.

          ls_entityset-pernr = lv_persno.
          ls_entityset-lifnr = lv_lifnr.

          APPEND ls_entityset TO et_entityset.
          CLEAR ls_entityset.
        ENDIF.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD delete_from_database.

    DATA : lr_expimp  TYPE REF TO cl_abap_expimp_db.
    CREATE OBJECT lr_expimp.

    TRY.
        CALL METHOD lr_expimp->delete
          EXPORTING
            tabname          = tabname "‘INDX’
            client           = client "‘800’
            area             = area "‘ZZ’
            id               = id "key
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


  METHOD employee_usersse_get_entityset.

    get_employee_users(
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


  METHOD exception_messages.

    DATA: lwa_bapi_messsages TYPE bapiret2,
          lo_msg_container   TYPE REF TO /iwbep/if_message_container,
          ls_message         TYPE /agri/s_gprolog,
          lv_msg_error       TYPE xfld,
          lv_msgno           TYPE symsgno.

****Authorizations - Activities
    CONSTANTS: BEGIN OF c_msg_type,
                 info    LIKE sy-msgty VALUE 'I',           "#EC NOTEXT
                 warning LIKE sy-msgty VALUE 'W',           "#EC NOTEXT
                 error   LIKE sy-msgty VALUE 'E',           "#EC NOTEXT
                 abend   LIKE sy-msgty VALUE 'A',           "#EC NOTEXT
                 success LIKE sy-msgty VALUE 'S',           "#EC NOTEXT
                 x       LIKE sy-msgty VALUE 'X',           "#EC NOTEXT
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
                   iv_msg_v1                 = 'Invalid user'"is_message-msgv1
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
      APPEND ls_entity_set TO et_entityset.
      CLEAR :ls_entity_set,
             ls_usr_emp.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_employee_users.

    DATA : lt_dd07       TYPE TABLE OF dd07v,
           ls_entity_set LIKE LINE OF et_entityset.

    SELECT *
      FROM zabs_usr_emp
      INTO TABLE @DATA(lt_usr_emp)
     WHERE bname EQ @sy-uname.
    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = 'ZABS_DOM_FPCNF'
        text           = 'X'
        langu          = sy-langu
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
      READ TABLE lt_dd07 INTO DATA(ls_dd07) WITH KEY domvalue_l = ls_entity_set-fpcnf.
      IF sy-subrc EQ 0.
        ls_entity_set-ctext = ls_dd07-ddtext.
      ENDIF.

      APPEND ls_entity_set TO et_entityset.
      CLEAR :ls_entity_set,
             ls_usr_emp.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_qualcharf4ext.

    "constants.
    CONSTANTS: lc_qkatart TYPE qkatart VALUE '1'.

    "types.
    TYPES : BEGIN OF ty_qpct,
              codegruppe TYPE qcodegrp,
              version    TYPE qversnr,
              kurztext   TYPE qtxt_code,
              code       TYPE qcode,
            END OF ty_qpct.

    "local declarations.
    DATA : lrt_mkmnr            TYPE RANGE OF qmerknr,
           lrs_mkmnr            LIKE LINE OF lrt_mkmnr,
           lt_qpct              TYPE TABLE OF ty_qpct,
           ls_qpct              TYPE ty_qpct,
           lrt_werks            TYPE RANGE OF werks_d,
           lrs_werks            LIKE LINE OF lrt_werks,
           lt_filter            TYPE /iwbep/t_mgw_select_option,
           ls_filter            TYPE /iwbep/s_mgw_select_option,
           ls_so                TYPE /iwbep/s_cod_select_option,
           ls_entityset         LIKE LINE OF et_entityset,
           lv_filter_str        TYPE string,
           lo_filter            TYPE REF TO /iwbep/if_mgw_req_filter,
           ls_converted_keys    LIKE LINE OF et_entityset,
           ls_enityset          LIKE LINE OF et_entityset,
           lrt_mstr_char        TYPE RANGE OF qcodegrp,
           lrt_version          TYPE RANGE OF qversnr,
           lo_message_container TYPE REF TO /iwbep/if_message_container.

    DATA: BEGIN OF lwa_codes,
            code     TYPE qpcd-code,
            kurztext TYPE qpct-kurztext,
            qtype    TYPE c LENGTH 1,
            qtext    TYPE c LENGTH 40,
            werks    TYPE c LENGTH 4,
            mkmnr    TYPE c LENGTH 8,
            vvalue   TYPE c LENGTH 40,
          END OF lwa_codes,
          lt_code  LIKE TABLE OF lwa_codes,
          lt_codes LIKE TABLE OF lwa_codes,
          ls_code  LIKE lwa_codes,
          BEGIN OF lwa_value,
            code     TYPE qpcd-code,
            kurztext TYPE qpct-kurztext,
          END OF lwa_value,
          lt_values  LIKE TABLE OF lwa_value,
          lt_fields  TYPE ddfields,
          lwa_field  TYPE dfies,
          lv_display.

    lo_filter     = io_tech_request_context->get_filter( ).
    lt_filter     = lo_filter->get_filter_select_options( ).
    lv_filter_str = lo_filter->get_filter_string( ).

    IF  lv_filter_str IS NOT INITIAL
    AND lt_filter[]   IS INITIAL.

      me->/iwbep/if_sb_dpc_comm_services~log_message(
      EXPORTING
        iv_msg_type   = 'E'
        iv_msg_id     = '/IWBEP/MC_SB_DPC_ADM'
        iv_msg_number = 000 ).
      " Raise Exception
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
        EXPORTING
          textid = /iwbep/cx_mgw_tech_exception=>internal_error.

    ENDIF.

    io_tech_request_context->get_converted_source_keys(
      IMPORTING
        es_key_values  = ls_converted_keys ).

    lo_message_container  = mo_context->get_message_container( ).

* Maps filter table lines to function module parameters
    LOOP AT lt_filter INTO ls_filter.

      CASE ls_filter-property.
        WHEN 'MSTR_CHAR'.
          lo_filter->convert_select_option(
            EXPORTING
              is_select_option = ls_filter
            IMPORTING
              et_select_option = lrt_mstr_char ).
        WHEN 'VERSION'.
          lo_filter->convert_select_option(
            EXPORTING
              is_select_option = ls_filter
            IMPORTING
              et_select_option = lrt_version ).
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

    "fetching the code group and version details.
    SELECT  codegruppe, version, kurztext, code
           FROM qpct                                    "#EC CI_GENBUFF
          INTO TABLE @lt_qpct
          WHERE katalogart = @lc_qkatart " Characteristic Attributes
            AND codegruppe IN @lrt_mstr_char
            AND version    IN @lrt_version
            AND inaktiv    = @space.

    LOOP AT lt_qpct INTO ls_qpct.
      ls_entityset-mstr_char = ls_qpct-codegruppe.
      ls_entityset-version   = ls_qpct-version.
      ls_entityset-kurztext  = ls_qpct-kurztext.
      ls_entityset-code      = ls_qpct-code.
      ls_entityset-sel_set1  = ls_qpct-codegruppe.
      APPEND ls_entityset TO et_entityset.
      CLEAR: ls_entityset,ls_qpct.
    ENDLOOP.

    SORT et_entityset BY code.

  ENDMETHOD.


  METHOD get_qualchar_fetch.

    "constants.
    CONSTANTS: lc_slwbez TYPE qslwbez        VALUE 'Z28',
               lc_03     TYPE qpart          VALUE '03',
               lc_autyp  TYPE /agri/gl_autyp VALUE 'TO',
               lc_astat  TYPE /agri/glastat  VALUE 'A'.

    "types.
    TYPES : BEGIN OF ty_qals,
              aufnr    TYPE aufnr,
              prueflos TYPE qplos,
            END OF ty_qals.

    "local declarations.
    DATA: lt_sampleres      TYPE TABLE OF bapi2045d3,
          lt_samplerest     TYPE TABLE OF bapi2045d3,
          ls_key_tab        TYPE /iwbep/s_mgw_name_value_pair,
          lt_qals           TYPE TABLE OF ty_qals,
          ls_qals           TYPE ty_qals,
          lt_req_temp       TYPE TABLE OF bapi2045d1,
          ls_req_temp       TYPE bapi2045d1,
          lt_req            TYPE TABLE OF bapi2045d1,
          ls_req            TYPE bapi2045d1,
          lt_aufnr          TYPE TABLE OF aufnr,
          ls_aufnr          TYPE aufnr,
          lrt_prueflost     TYPE RANGE OF qplos,
          lrt_prueflos      TYPE RANGE OF qplos,
          lrt_pernr         TYPE RANGE OF persno,
          lr_pernr          LIKE LINE OF lrt_pernr,
          lv_persno         TYPE persno,
          ls_prueflos       LIKE LINE OF lrt_prueflos,
          lrt_inspoper      TYPE RANGE OF qibpvornr,
          lrs_inspoper      LIKE LINE OF lrt_inspoper,
          lwa_prueflos      TYPE qplos,
          lt_prueflos       TYPE TABLE OF qplos,
          lt_oprn_temp      TYPE TABLE OF bapi2045l2,
          ls_oprn_temp      TYPE bapi2045l2,
          lt_oprn           TYPE TABLE OF bapi2045l2,
          ls_oprn           TYPE bapi2045l2,
          lt_filter         TYPE /iwbep/t_mgw_select_option,
          ls_filter         TYPE /iwbep/s_mgw_select_option,
          ls_so             TYPE /iwbep/s_cod_select_option,
          ls_entityset      LIKE LINE OF et_entityset,
          lv_filter_str     TYPE string,
          lt_rttrn          TYPE zcl_zabs_agri_mobile_e_mpc=>tt_worouteterrainextend,
          lo_filter         TYPE REF TO /iwbep/if_mgw_req_filter,
          ls_converted_keys LIKE LINE OF et_entityset,
          ls_enityset       LIKE LINE OF et_entityset,
          lt_constants      TYPE zabs_tty_vkey_const,
          lt_enstehdat      TYPE zabs_tty_vkey_const.

    DATA: lv_index,
          lv_subrc            TYPE sy-subrc,
          lwa_activities_fcat TYPE /agri/s_fmfpact_fcat,
          lv_vornr            TYPE vornr,
          lv_prueflos         TYPE qibplosnr,
          lv_i                TYPE i,
          lv_fieldname        TYPE fieldname,
          lv_tabix            TYPE sy-tabix,
          lv_var              TYPE char1,
          lv_urole            TYPE zabs_del_urole.

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

    LOOP AT lt_filter INTO ls_filter.
      CASE ls_filter-property.
        WHEN 'INSPLOT'.
          lo_filter->convert_select_option(
            EXPORTING
              is_select_option = ls_filter
            IMPORTING
              et_select_option = lrt_prueflos ).
        WHEN 'INSPOPER'.
          lo_filter->convert_select_option(
            EXPORTING
              is_select_option = ls_filter
            IMPORTING
              et_select_option = lrt_inspoper ).
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
        WHEN OTHERS.
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
    SELECT a~task, b~urole
      FROM zabst_task_app AS a
      INNER JOIN zabs_emp_role AS b
      ON a~urole = b~urole
      INTO TABLE @DATA(lt_taskmat)
      WHERE b~pernr IN @lrt_pernr
        AND a~stapp EQ @abap_true.

    IF sy-subrc NE 0.
*- If user role is not having any Task maintained - then dont show any task orders
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

      READ TABLE lt_taskmat ASSIGNING FIELD-SYMBOL(<ls_taskmat>) INDEX 1.
      IF <ls_taskmat> IS ASSIGNED.
        lv_urole = <ls_taskmat>-urole.
      ENDIF.
    ENDIF.

    DATA : lv_low_pastrterm    TYPE sy-datum,
           lv_high_pastrterm   TYPE sy-datum,
           lv_cnval1_pastrterm TYPE zabs_del_cnval,
           lr_pastrterm        TYPE RANGE OF qprstart.
    CALL METHOD zcl_abs_get_variants=>get_constant_single
      EXPORTING
        iv_mod    = 'C'
        iv_objid  = 'MOBL'
        iv_k1val  = 'INSPOP'
        iv_k2val  = 'LPSTRTRM'
        iv_k3val  = lv_urole
      IMPORTING
        ev_cnval1 = lv_cnval1_pastrterm.
    CONDENSE lv_cnval1_pastrterm.
    lv_low_pastrterm   = sy-datum - lv_cnval1_pastrterm.
    CALL METHOD zcl_abs_get_variants=>get_constant_single
      EXPORTING
        iv_mod    = 'C'
        iv_objid  = 'MOBL'
        iv_k1val  = 'INSPOP'
        iv_k2val  = 'HPSTRTRM'
        iv_k3val  = lv_urole
      IMPORTING
        ev_cnval1 = lv_cnval1_pastrterm.
    CONDENSE lv_cnval1_pastrterm.
    lv_high_pastrterm = sy-datum + lv_cnval1_pastrterm.
    lr_pastrterm = VALUE #( sign = 'I' option = 'BT'
                            ( low = lv_low_pastrterm
                              high = lv_high_pastrterm ) ).

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

    SELECT a~prueflos, a~werk, a~slwbez, b~aufnr, b~objnr
      FROM qals AS a
      INNER JOIN /agri/fmfphdr AS b
      ON a~aufnr = b~aufnr
      INTO TABLE @DATA(lt_qalst)
       FOR ALL ENTRIES IN @lt_rttrn
      WHERE b~autyp    = @lc_autyp
        AND b~tplnr_fl = @lt_rttrn-tplnr"lt_flcma-tplnr_fl
        AND b~matnr    IN @lr_matnr
        AND b~gstrp    IN @ltr_gstrp
        AND b~tecom    = @space
        AND a~prueflos IN @lrt_prueflos
        AND a~art      = @lc_03
        AND a~pastrterm IN @lr_pastrterm
        AND a~slwbez   = @lc_slwbez.

*--Get variant table data.
    CALL METHOD zcl_abs_get_variants=>get_constant_multiple
      EXPORTING
        iv_mod       = 'C'
        iv_objid     = 'MOBL'
        iv_k1val     = 'MOBL'
        iv_k2val     = 'INSPOP'
        iv_k3val     = 'Z28STEUS'
      IMPORTING
        et_constants = lt_constants.

    DATA lr_steus TYPE RANGE OF steus.lr_steus =
    VALUE #( FOR ls_constant IN lt_constants (
    sign   = 'I'
    option = 'EQ'
    low    = ls_constant-cnval1 ) ).
    DELETE lr_steus WHERE low IS INITIAL.
    SORT lr_steus BY low.
    DELETE ADJACENT DUPLICATES FROM lr_steus COMPARING low.

    "Fetching FMFP item details.
    IF lt_qalst IS NOT INITIAL.
      SELECT aufnr, vornr, steus
        FROM /agri/fmfpitm
        INTO TABLE @DATA(lt_fmfpitm)
         FOR ALL ENTRIES IN @lt_qalst
       WHERE aufnr EQ @lt_qalst-aufnr
         AND steus IN @lr_steus.
      SORT lt_fmfpitm BY aufnr.
    ENDIF.

    SORT lt_qalst BY prueflos.

    IF lrt_prueflos IS NOT INITIAL
      AND lrt_inspoper IS  NOT INITIAL.  "When Single Inspection lot
      LOOP AT lrt_prueflos INTO ls_prueflos .

        READ TABLE lt_qalst INTO DATA(ls_qalst)
                          WITH KEY prueflos = ls_prueflos-low
                          BINARY SEARCH.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        LOOP AT lt_fmfpitm INTO DATA(ls_fmfpitm)
          WHERE aufnr EQ ls_qalst-aufnr.
          CALL FUNCTION 'BAPI_INSPOPER_GETDETAIL'
            EXPORTING
              insplot                = ls_qalst-prueflos "ls_oprn-insplot
              inspoper               = ls_fmfpitm-vornr "ls_oprn-inspoper
              read_char_requirements = abap_true
              read_char_results      = abap_true
              read_sample_results    = abap_true
              read_single_results    = abap_true
              char_filter_no         = '1'
              char_filter_tcode      = 'QE11'
              max_insppoints         = 100
              insppoint_from         = 0
            TABLES
*             INSPPOINTS             =
              sample_results         = lt_samplerest
              char_requirements      = lt_req. "MSTR_CHAR  ,INSPCHAR

          APPEND LINES OF lt_samplerest TO lt_sampleres.

          APPEND LINES OF lt_req TO lt_req_temp.

          REFRESH: lt_req, lt_samplerest.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

    IF lt_req_temp IS INITIAL.  "When All Inpection lot

      LOOP AT lt_qalst ASSIGNING FIELD-SYMBOL(<fs_qalst>).

        READ TABLE lt_fmfpitm TRANSPORTING NO FIELDS
                              WITH KEY aufnr = <fs_qalst>-aufnr
                              BINARY SEARCH.
        IF sy-subrc EQ 0.
          lv_tabix = sy-tabix.
          LOOP AT lt_fmfpitm ASSIGNING FIELD-SYMBOL(<ls_itm>) FROM lv_tabix.
            IF <ls_itm>-aufnr <> <fs_qalst>-aufnr.
              EXIT.
            ENDIF.
            CALL FUNCTION 'BAPI_INSPOPER_GETDETAIL'
              EXPORTING
                insplot                = <fs_qalst>-prueflos "ls_oprn-insplot
                inspoper               = <ls_itm>-vornr "ls_oprn-inspoper
                read_char_requirements = abap_true
                read_char_results      = abap_true
                read_sample_results    = abap_true
                read_single_results    = abap_true
                char_filter_no         = '1'
                char_filter_tcode      = 'QE11'
                max_insppoints         = 100
                insppoint_from         = 0
              TABLES
                sample_results         = lt_samplerest
                char_requirements      = lt_req.

            APPEND LINES OF lt_samplerest TO lt_sampleres.

            IF lt_samplerest IS NOT INITIAL.
              LOOP AT lt_req INTO ls_req.
                MOVE-CORRESPONDING ls_req TO ls_req_temp.
                APPEND ls_req_temp TO lt_req_temp.
                CLEAR : ls_req_temp,ls_req.
              ENDLOOP.
            ENDIF.
            REFRESH lt_samplerest.

          ENDLOOP.
        ENDIF.

      ENDLOOP.
    ENDIF.

    SORT lt_sampleres BY insplot    ASCENDING
                         inspoper   ASCENDING
                         inspchar   ASCENDING
                         inspsample DESCENDING.

    DELETE ADJACENT DUPLICATES FROM lt_sampleres COMPARING insplot inspoper inspchar.
    SORT lt_sampleres BY insplot inspoper inspchar.

    DATA lr_mstrchar TYPE RANGE OF qmstr_char.lr_mstrchar =
    VALUE #( FOR ls_mstrchart IN lt_req_temp (
    sign   = 'I'
    option = 'EQ'
    low    = ls_mstrchart-mstr_char ) ).
    DELETE lr_mstrchar WHERE low IS INITIAL.

    SELECT *
      FROM zabs_qachar_grp
      INTO TABLE @DATA(lt_qachargrp)
      WHERE mstr_char IN @lr_mstrchar.
    SORT lt_qachargrp BY mstr_char.

    SORT lt_req_temp BY insplot inspoper inspchar.
    LOOP AT lt_req_temp INTO ls_req_temp.
      ls_entityset-inspchar  = ls_req_temp-inspchar.

      IF ls_req_temp-sel_set1 IS NOT INITIAL.
        SELECT SINGLE codegruppe
          FROM qpac
          WHERE auswahlmge = @ls_req_temp-sel_set1
            AND werks      = @ls_req_temp-psel_set1
            AND katalogart = @ls_req_temp-cat_type1
          INTO @ls_entityset-mstr_char.
        IF sy-subrc <> 0.
          ls_entityset-mstr_char = ls_req_temp-mstr_char.
        ENDIF.
      ELSE.
        ls_entityset-mstr_char = ls_req_temp-mstr_char.
      ENDIF.
      ls_entityset-insplot    = ls_req_temp-insplot.
      ls_entityset-inspoper   = ls_req_temp-inspoper.
      ls_entityset-char       = ls_req_temp-char_type.
      ls_entityset-version    = ls_req_temp-vmstr_char.
      ls_entityset-munit      = ls_req_temp-meas_unit.
      ls_entityset-insp_point = ls_req_temp-insp_point.
      ls_entityset-char_descr = ls_req_temp-char_descr.
      ls_entityset-formula    = ls_req_temp-formula.

      READ TABLE lt_sampleres ASSIGNING FIELD-SYMBOL(<fs_sampleres>)
                              WITH KEY insplot  = ls_req_temp-insplot
                                       inspoper = ls_req_temp-inspoper
                                       inspchar = ls_req_temp-inspchar
                                       BINARY SEARCH.
      IF sy-subrc = 0.
        IF <fs_sampleres>-mean_value IS NOT INITIAL.
          CONDENSE <fs_sampleres>-mean_value.
          ls_entityset-result = <fs_sampleres>-mean_value.
        ELSE.
          ls_entityset-result = <fs_sampleres>-code1.
        ENDIF.
        ls_entityset-inspsample = <fs_sampleres>-inspsample.
      ENDIF.

      READ TABLE lt_qachargrp ASSIGNING FIELD-SYMBOL(<fs_qachargrp>)
                                WITH KEY mstr_char = ls_req_temp-mstr_char
                                BINARY SEARCH.
      IF sy-subrc = 0.
        ls_entityset-inspgroup = <fs_qachargrp>-inspgroup.
        ls_entityset-inspgrpseq = <fs_qachargrp>-grp_seq.
      ENDIF.

      IF lv_prueflos IS INITIAL.
        lv_prueflos = ls_req_temp-insplot.
        lv_vornr    = ls_req_temp-inspchar.
      ELSE.
        IF lv_prueflos <> ls_req_temp-insplot
         AND lv_vornr <> ls_req_temp-inspoper.
          lv_prueflos = ls_req_temp-insplot.
          lv_vornr    = ls_req_temp-inspoper.
          CLEAR lv_i.
        ENDIF.
      ENDIF.

      lv_i = lv_i + 1.
      ls_entityset-inspmicseq = lv_i.

      CONDENSE ls_entityset-result.
      ls_entityset-pernr = lv_persno.

      APPEND ls_entityset TO et_entityset.
      CLEAR : ls_entityset,ls_req_temp.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_quality_operations_extend.

    TYPES: BEGIN OF ty_rout_final,
             aufpl TYPE co_aufpl,
             plnty TYPE plnty,
             steus TYPE steus,
             vornr TYPE vornr,
             aufnr TYPE aufnr,
           END OF ty_rout_final.

    TYPES: BEGIN OF ty_oper_final,
             aufnr      TYPE aufnr,
             slwbez     TYPE qslwbez,
             bapi2045l2 TYPE bapi2045l2,
             matnr      TYPE matnr,
           END OF ty_oper_final.

    TYPES: BEGIN OF lty_cvornr,
             cvornr TYPE zabs_cvornr,
           END OF lty_cvornr.

    CONSTANTS: lc_autyp TYPE /agri/gl_autyp VALUE 'TO',
               lc_qherk TYPE qherk          VALUE '03',
               lc_astat TYPE /agri/glastat  VALUE 'A'.

    DATA: lrt_prueflos      TYPE RANGE OF qplos,
          lrt_steus         TYPE RANGE OF steus,
          ls_prueflos       LIKE LINE OF lrt_prueflos,
          lt_cvornr         TYPE TABLE OF lty_cvornr,
          lwa_prueflos      TYPE qplos,
          lt_prueflos       TYPE TABLE OF qplos,
          lt_oprn           TYPE TABLE OF bapi2045l2,
          ls_oprn           TYPE bapi2045l2,
          lt_filter         TYPE /iwbep/t_mgw_select_option,
          lt_rttrn          TYPE zcl_zabs_agri_mobile_e_mpc=>tt_worouteterrainextend,
          ls_filter         TYPE /iwbep/s_mgw_select_option,
          ls_entityset      LIKE LINE OF et_entityset,
          lv_filter_str     TYPE string,
          lo_filter         TYPE REF TO /iwbep/if_mgw_req_filter,
          ls_converted_keys LIKE LINE OF et_entityset,
          ls_rout_final     TYPE ty_rout_final,
          lt_rout_final     TYPE TABLE OF ty_rout_final,
          ls_oper_final     TYPE ty_oper_final,
          lt_oper_final     TYPE TABLE OF ty_oper_final,
          lt_constants      TYPE zabs_tty_vkey_const,
          ls_constants      TYPE zabs_str_vkey_const,
          lt_steus          TYPE RANGE OF steus,
          lrt_pernr         TYPE RANGE OF persno,
          lrs_pernr         LIKE LINE OF lrt_pernr,
          lr_pernr          LIKE LINE OF lrt_pernr,
          lv_persno         TYPE persno,
          lv_tabix          TYPE sy-tabix,
          lv_enstehdat      TYPE sy-datum,
          lv_var            TYPE char1,
          lv_urole          TYPE zabs_del_urole.
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

    LOOP AT lt_filter INTO ls_filter.
      CASE ls_filter-property.
        WHEN 'INSPLOT'.
          lo_filter->convert_select_option(
            EXPORTING
              is_select_option = ls_filter
            IMPORTING
              et_select_option = lrt_prueflos ).

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

        WHEN OTHERS.
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

*--Get variant table data
    CALL METHOD zcl_abs_get_variants=>get_constant_multiple
      EXPORTING
        iv_mod       = 'C'
        iv_objid     = 'MOBL'
        iv_k1val     = 'INSPOP'
        iv_k2val     = 'STEUS'
      IMPORTING
        et_constants = lt_constants.

    DATA lr_steus TYPE RANGE OF steus.
    lr_steus =  VALUE #( FOR ls_constant IN lt_constants (
    sign   = 'I'
    option = 'EQ'
    low    = ls_constant-cnval1 ) ).
    DELETE lr_steus WHERE low IS INITIAL.
    SORT lr_steus BY low.
    DELETE ADJACENT DUPLICATES FROM lr_steus COMPARING low.

    "change -> Login User based validation to User+Employee based valiadtion
    SELECT a~task, a~vornr, b~urole
        FROM zabst_task_app AS a
        INNER JOIN zabs_emp_role AS b
        ON a~urole = b~urole
        INTO TABLE @DATA(lt_taskmat)
        WHERE b~pernr IN @lrt_pernr
          AND a~stapp EQ @abap_true.

    IF sy-subrc NE 0.
      RETURN.
    ELSE.
      DATA(lt_opertmp) = lt_taskmat.
      lt_opertmp = lt_taskmat.
      DELETE lt_opertmp WHERE vornr IS INITIAL.
      SORT lt_opertmp BY task.

      DATA lr_matnr TYPE RANGE OF matnr.lr_matnr =
      VALUE #( FOR ls_taskmat IN lt_taskmat (
      sign   = 'I'
      option = 'EQ'
      low    = ls_taskmat-task ) ).
      DELETE lr_matnr WHERE low IS INITIAL.
      SORT lr_matnr BY low.
      DELETE ADJACENT DUPLICATES FROM lr_matnr COMPARING low.

      READ TABLE lt_taskmat ASSIGNING FIELD-SYMBOL(<ls_taskmat>) INDEX 1.
      IF <ls_taskmat> IS ASSIGNED.
        lv_urole = <ls_taskmat>-urole.
      ENDIF.
    ENDIF.

    DATA : lv_low_pastrterm    TYPE sy-datum,
           lv_high_pastrterm   TYPE sy-datum,
           lv_cnval1_pastrterm TYPE zabs_del_cnval,
           lr_pastrterm        TYPE RANGE OF qprstart.
    CALL METHOD zcl_abs_get_variants=>get_constant_single
      EXPORTING
        iv_mod    = 'C'
        iv_objid  = 'MOBL'
        iv_k1val  = 'INSPOP'
        iv_k2val  = 'LPSTRTRM'
        iv_k3val  = lv_urole
      IMPORTING
        ev_cnval1 = lv_cnval1_pastrterm.
    CONDENSE lv_cnval1_pastrterm.
    lv_low_pastrterm   = sy-datum - lv_cnval1_pastrterm.
    CALL METHOD zcl_abs_get_variants=>get_constant_single
      EXPORTING
        iv_mod    = 'C'
        iv_objid  = 'MOBL'
        iv_k1val  = 'INSPOP'
        iv_k2val  = 'HPSTRTRM'
        iv_k3val  = lv_urole
      IMPORTING
        ev_cnval1 = lv_cnval1_pastrterm.
    CONDENSE lv_cnval1_pastrterm.
    lv_high_pastrterm = sy-datum + lv_cnval1_pastrterm.
    lr_pastrterm = VALUE #( sign = 'I' option = 'BT'
                            ( low = lv_low_pastrterm
                              high = lv_high_pastrterm ) ).
*Fetch PERNR/LIFNR based Terrains
    get_route_terrain_dtls(
        EXPORTING
          io_tech_request_context = io_tech_request_context
        IMPORTING
          et_rttrn                = lt_rttrn ).
    IF lt_rttrn[] IS INITIAL.
      RETURN.
    ENDIF.

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

    DATA : lr_slwbez TYPE RANGE OF qslwbez.
    lr_slwbez = VALUE #( sign = 'I' option = 'EQ' ( low = 'Z23' )
                                                  ( low = 'Z28' ) ).
    SELECT a~aufnr, a~auart, a~matnr, b~prueflos, b~slwbez, b~aufpl
      INTO TABLE @DATA(lt_fmhdr_qals)
      FROM /agri/fmfphdr AS a
     INNER JOIN qals     AS b
         ON b~aufnr = a~aufnr
       FOR ALL ENTRIES IN @lt_rttrn
     WHERE a~autyp     EQ @lc_autyp
       AND a~tplnr_fl  EQ @lt_rttrn-tplnr "@lt_flcma-tplnr_fl
       AND a~matnr     IN @lr_matnr
       AND a~gstrp     IN @ltr_gstrp
       AND a~tecom     EQ @space
       AND b~prueflos  IN @lrt_prueflos
       AND b~pastrterm IN @lr_pastrterm
       AND b~herkunft  EQ @lc_qherk
       AND b~slwbez    IN @lr_slwbez.

    IF lt_fmhdr_qals IS INITIAL.
      RETURN.
    ENDIF.

**--Get variant table data

    IF lt_fmhdr_qals IS NOT INITIAL.
      SELECT aufnr, steus, vornr, ltxa1
        FROM /agri/fmfpitm
        INTO TABLE @DATA(lt_fmfpitm)
         FOR ALL ENTRIES IN @lt_fmhdr_qals
       WHERE aufnr  =  @lt_fmhdr_qals-aufnr"lrt_aufnr
         AND steus IN @lr_steus.
      SORT lt_fmfpitm BY aufnr vornr.

      SELECT prueflos, zzcflag, zzvornr
        FROM zabs_mob_ilot
        INTO TABLE @DATA(lt_mobilot)
         FOR ALL ENTRIES IN @lt_fmhdr_qals
       WHERE prueflos = @lt_fmhdr_qals-prueflos."@lt_oper_final-bapi2045l2-insplot.
      IF sy-subrc = 0.
        SORT lt_mobilot BY prueflos zzvornr.
      ENDIF.

      SELECT insplot, inspoper, cflag
        FROM zabs_qchar_hdr
        INTO TABLE @DATA(lt_qchar_hdr)
         FOR ALL ENTRIES IN @lt_fmhdr_qals
       WHERE insplot EQ @lt_fmhdr_qals-prueflos
         AND cflag   EQ @abap_true.
      IF sy-subrc = 0.
        SORT lt_qchar_hdr BY insplot inspoper cflag.
      ENDIF.

    ENDIF. "lt_fmhdr_qals

    SORT: lt_fmhdr_qals BY aufnr,
          lt_fmfpitm    BY aufnr,
          lt_opertmp    BY task.

    LOOP AT lt_fmfpitm ASSIGNING FIELD-SYMBOL(<fs_fmfpitm>).

      READ TABLE lt_fmhdr_qals ASSIGNING FIELD-SYMBOL(<fs_fmhdr>)
                               WITH KEY aufnr = <fs_fmfpitm>-aufnr
                               BINARY SEARCH.
      IF sy-subrc = 0.

*----task based vornr
        READ TABLE lt_opertmp ASSIGNING FIELD-SYMBOL(<fs_opertmp>)
                              WITH KEY task  = <fs_fmhdr>-matnr
                              BINARY SEARCH.
        IF sy-subrc = 0.
          CHECK <fs_fmfpitm>-vornr = <fs_opertmp>-vornr.
        ENDIF.
*-----filter already confirmed lots and closed operations
        READ TABLE lt_mobilot ASSIGNING FIELD-SYMBOL(<fs_mobilot>)
                               WITH KEY prueflos = <fs_fmhdr>-prueflos
                                        zzvornr  = <fs_fmfpitm>-vornr "ls_oper_final-bapi2045l2-insplot
                               BINARY SEARCH.
        IF sy-subrc = 0.
          "skipping the ilot's with final confirmation.
          IF <fs_mobilot>-zzcflag EQ 'X'.
            CONTINUE.
          ENDIF.
          SPLIT <fs_mobilot>-zzvornr AT ';' INTO TABLE lt_cvornr.
          SORT lt_cvornr BY cvornr.
          READ TABLE lt_cvornr TRANSPORTING NO FIELDS
                               WITH KEY cvornr = <fs_fmfpitm>-vornr "ls_oper_final-bapi2045l2-inspoper
                               BINARY SEARCH.
          IF sy-subrc = 0.
            CONTINUE.
          ENDIF.
        ENDIF.

        READ TABLE lt_qchar_hdr INTO DATA(ls_qchar_hdr)
                               WITH KEY insplot = <fs_fmhdr>-prueflos
                                        inspoper = <fs_fmfpitm>-vornr
                                        cflag   = abap_true
                               BINARY SEARCH.
        IF sy-subrc EQ 0.
          CLEAR : ls_qchar_hdr.
          CONTINUE.
        ENDIF.

        ls_entityset-insplot    = <fs_fmhdr>-prueflos.
        ls_entityset-inspoper   = <fs_fmfpitm>-vornr.
        ls_entityset-txt_oper    = <fs_fmfpitm>-ltxa1.
        ls_entityset-steus      = <fs_fmfpitm>-steus."ls_rout_final-steus.
        ls_entityset-slwbez     = <fs_fmhdr>-slwbez."ls_oper_final-slwbez.
        ls_entityset-pernr      = lv_persno.

        APPEND ls_entityset TO et_entityset.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_qualtaskord.

    "types.
    TYPES : BEGIN OF ty_qals,
              aufnr    TYPE aufnr,
              prueflos TYPE qplos,
              aufpl    TYPE co_aufpl,
              plnnr    TYPE plnnr,
            END OF ty_qals.

    TYPES : BEGIN OF ty_fmfp,
              aufnr    TYPE aufnr,
              tplnr_fl TYPE /agri/gltplnr_fl,
              cmnum    TYPE /agri/glcmnum,
              matnr    TYPE /agri/glmatnr,
              gstrp    TYPE co_gstrp,
              objnr    TYPE j_objnr,
            END OF ty_fmfp.

    TYPES: BEGIN OF lty_cvornr,
             cvornr TYPE zabs_cvornr,
           END OF lty_cvornr.
    "constants.
    CONSTANTS: lc_autyp TYPE /agri/gl_autyp VALUE 'TO',
               lc_qherk TYPE qherk          VALUE '03',
               lc_cstat TYPE /agri/fmcstat  VALUE 'CNF',
               lc_astat TYPE /agri/glastat  VALUE 'A'.
    "local declarations.
    DATA: ls_key_tab        TYPE /iwbep/s_mgw_name_value_pair,
          lt_qals           TYPE TABLE OF ty_qals,
          lt_qalsdef        TYPE TABLE OF ty_qals,
          lt_oper           TYPE TABLE OF bapi2045l2,
          lt_cvornr         TYPE TABLE OF lty_cvornr,
          lt_constants      TYPE zabs_tty_vkey_const,
          ls_qals           TYPE ty_qals,
          lt_aufnr          TYPE TABLE OF aufnr,
          ls_mob_ilot       TYPE zabs_mob_ilot,
          ls_aufnr          TYPE aufnr,
          lrt_tplnr_fl      TYPE RANGE OF /agri/gltplnr_fl,
          lrs_tplnr_fl      LIKE LINE OF lrt_tplnr_fl,
          lt_rttrn          TYPE zcl_zabs_agri_mobile_e_mpc=>tt_worouteterrainextend,
          lrt_cmnum         TYPE RANGE OF /agri/glcmnum,
          ls_cmnum          LIKE LINE OF lrt_cmnum,
          lrt_pernr         TYPE RANGE OF persno,
          lrs_pernr         LIKE LINE OF lrt_pernr,
          lr_pernr          LIKE LINE OF lrt_pernr,
          lv_persno         TYPE persno,
          lrt_aufnr         TYPE RANGE OF /agri/fmfpnum, "/agri/fmaufnr,
          lt_filter         TYPE /iwbep/t_mgw_select_option,
          ls_filter         TYPE /iwbep/s_mgw_select_option,
          ls_so             TYPE /iwbep/s_cod_select_option,
          ls_entityset      LIKE LINE OF et_entityset,
          lv_filter_str     TYPE string,
          lv_error          TYPE flag,
          lo_filter         TYPE REF TO /iwbep/if_mgw_req_filter,
          ls_converted_keys LIKE LINE OF et_entityset,
          ls_enityset       LIKE LINE OF et_entityset,
          lv_var            TYPE char1,
          lv_urole          TYPE zabs_del_urole.


    DATA: lv_index,
          lv_subrc            TYPE sy-subrc,
          lwa_activities_fcat TYPE /agri/s_fmfpact_fcat,
          lv_prueflos         TYPE qplos,
          lv_fieldname        TYPE fieldname.

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
        WHEN 'AUFNR'.
          lo_filter->convert_select_option(
            EXPORTING
              is_select_option = ls_filter
            IMPORTING
              et_select_option = lrt_aufnr ).
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

        WHEN OTHERS.
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

    "change -> Login User based validation to User+Employee based valiadtion
    SELECT a~task, a~vornr, b~urole
      FROM zabst_task_app AS a
      INNER JOIN zabs_emp_role AS b
      ON a~urole = b~urole
      INTO TABLE @DATA(lt_taskmat)
      WHERE b~pernr IN @lrt_pernr
        AND a~stapp EQ @abap_true.

    IF sy-subrc NE 0.
*- If user role is not having any Task maintained - then dont show any task orders
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

      READ TABLE lt_taskmat ASSIGNING FIELD-SYMBOL(<ls_taskmat>) INDEX 1.
      IF <ls_taskmat> IS ASSIGNED.
        lv_urole = <ls_taskmat>-urole.
      ENDIF.
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
    SORT lt_rttrn BY tplnr.

*--Get variant table data
    CALL METHOD zcl_abs_get_variants=>get_constant_multiple
      EXPORTING
        iv_mod       = 'C'
        iv_objid     = 'MOBL'
        iv_k1val     = 'INSPOP'
        iv_k2val     = 'STEUS'
      IMPORTING
        et_constants = lt_constants.

    DATA lr_steus TYPE RANGE OF steus.lr_steus =
    VALUE #( FOR ls_constant IN lt_constants (
    sign   = 'I'
    option = 'EQ'
    low    = ls_constant-cnval1 ) ).
    DELETE lr_steus WHERE low IS INITIAL.
    SORT lr_steus BY low.
    DELETE ADJACENT DUPLICATES FROM lr_steus COMPARING low.

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

    SELECT a~aufnr,
           a~tplnr_fl,
           a~cmnum,
           a~matnr,
           a~gstrp,
           a~objnr,
           b~vornr,
           b~steus
      FROM /agri/fmfphdr AS a
      INNER JOIN /agri/fmfpitm AS b
         ON b~aufnr = a~aufnr
      INTO TABLE @DATA(lt_fmfpitm)
       FOR ALL ENTRIES IN @lt_rttrn
     WHERE a~autyp     EQ @lc_autyp
       AND a~tplnr_fl  EQ @lt_rttrn-tplnr
       AND a~matnr     IN @lr_matnr
       AND a~aufnr     IN @lrt_aufnr
       AND a~cmnum     IN @lrt_cmnum
       AND a~gstrp     IN @ltr_gstrp
       AND a~tecom     EQ @space
       AND b~steus     IN @lr_steus
       AND b~cstat    <> @lc_cstat.

    IF sy-subrc = 0.
      SORT lt_fmfpitm BY aufnr.
      SELECT *
        FROM makt
        INTO TABLE @DATA(lt_makt)
        WHERE matnr IN @lr_matnr"@lt_fmfp-matnr
          AND spras =  @sy-langu.
      SORT lt_makt BY matnr.
      DELETE ADJACENT DUPLICATES FROM lt_makt COMPARING matnr.
    ENDIF.

    IF lt_fmfpitm IS NOT INITIAL.

      DATA : lv_low_pastrterm    TYPE sy-datum,
             lv_high_pastrterm   TYPE sy-datum,
             lv_cnval1_pastrterm TYPE zabs_del_cnval,
             lr_pastrterm        TYPE RANGE OF qprstart.
      CALL METHOD zcl_abs_get_variants=>get_constant_single
        EXPORTING
          iv_mod    = 'C'
          iv_objid  = 'MOBL'
          iv_k1val  = 'INSPOP'
          iv_k2val  = 'LPSTRTRM'
          iv_k3val  = lv_urole
        IMPORTING
          ev_cnval1 = lv_cnval1_pastrterm.
      CONDENSE lv_cnval1_pastrterm.
      lv_low_pastrterm   = sy-datum - lv_cnval1_pastrterm.
      CALL METHOD zcl_abs_get_variants=>get_constant_single
        EXPORTING
          iv_mod    = 'C'
          iv_objid  = 'MOBL'
          iv_k1val  = 'INSPOP'
          iv_k2val  = 'HPSTRTRM'
          iv_k3val  = lv_urole
        IMPORTING
          ev_cnval1 = lv_cnval1_pastrterm.
      CONDENSE lv_cnval1_pastrterm.
      lv_high_pastrterm = sy-datum + lv_cnval1_pastrterm.
      lr_pastrterm = VALUE #( sign = 'I' option = 'BT'
                              ( low = lv_low_pastrterm
                               high = lv_high_pastrterm ) ).
      SELECT aufnr, prueflos, aufpl, plnnr
       FROM qals
       INTO TABLE @lt_qals
        FOR ALL ENTRIES IN @lt_fmfpitm"lt_fmfp      "#EC CI_NO_TRANSFORM
          WHERE aufnr     EQ @lt_fmfpitm-aufnr
            AND herkunft  EQ @lc_qherk
            AND pastrterm IN @lr_pastrterm. "lt_fmfp-aufnr.

      IF lt_qals IS NOT INITIAL.
        SELECT prueflos, zzcflag, zzvornr
          FROM zabs_mob_ilot
          INTO TABLE @DATA(lt_mob_ilot)
           FOR ALL ENTRIES IN @lt_qals
         WHERE prueflos = @lt_qals-prueflos.
        IF sy-subrc EQ 0.
          SORT lt_mob_ilot BY prueflos
                              zzcflag.
        ENDIF.

        SELECT insplot, cflag
          FROM zabs_qchar_hdr
          INTO TABLE @DATA(lt_qchar_hdr)
           FOR ALL ENTRIES IN @lt_qals
         WHERE insplot EQ @lt_qals-prueflos
           AND cflag   EQ @abap_true.
        IF sy-subrc = 0.
          SORT lt_qchar_hdr BY insplot cflag.
        ENDIF.

      ENDIF.
    ENDIF.

    DATA lv_tabix TYPE sy-tabix.
    LOOP AT lt_qals ASSIGNING FIELD-SYMBOL(<fs_qals>).

      READ TABLE lt_mob_ilot ASSIGNING FIELD-SYMBOL(<fs_mobilot>)
                                   WITH KEY prueflos = <fs_qals>-prueflos
                                   BINARY SEARCH.
      IF sy-subrc = 0.
        "skipping the ilot's with final confirmation.
        IF <fs_mobilot>-zzcflag EQ 'X'.
          CONTINUE.
        ENDIF.
        SPLIT <fs_mobilot>-zzvornr AT ';' INTO TABLE lt_cvornr.
        SORT lt_cvornr BY cvornr.
      ENDIF.

      READ TABLE lt_qchar_hdr INTO DATA(ls_qchar_hdr)
                                   WITH KEY insplot = <fs_qals>-prueflos
                                            cflag = abap_true
                                   BINARY SEARCH.
      IF sy-subrc EQ 0.
        CLEAR : ls_qchar_hdr.
        REFRESH lt_cvornr.
        CONTINUE.
      ENDIF.

      READ TABLE lt_fmfpitm  TRANSPORTING NO FIELDS
                             WITH KEY aufnr = <fs_qals>-aufnr
                             BINARY SEARCH.
      IF sy-subrc = 0.
        lv_tabix = sy-tabix.
        LOOP AT lt_fmfpitm ASSIGNING FIELD-SYMBOL(<fs_fmfpitm>) FROM lv_tabix.
          IF <fs_fmfpitm>-aufnr NE <fs_qals>-aufnr.
            EXIT.
          ENDIF.
          READ TABLE lt_cvornr TRANSPORTING NO FIELDS
                               WITH KEY cvornr = <fs_fmfpitm>-vornr"<fs_fmfp>-vornr
                               BINARY SEARCH.
          IF sy-subrc = 0.
            CONTINUE.
          ENDIF.
          APPEND <fs_qals> TO lt_qalsdef.
        ENDLOOP."lt_fmfpitm
      ENDIF.

      REFRESH lt_cvornr.
    ENDLOOP.

    LOOP AT lt_qalsdef INTO ls_qals.

      READ TABLE lt_fmfpitm INTO DATA(ls_fmfpitm)"ls_fmfp
        WITH KEY aufnr = ls_qals-aufnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        READ TABLE lt_rttrn INTO DATA(ls_rttrn)
                            WITH KEY tplnr = ls_fmfpitm-tplnr_fl
                            BINARY SEARCH.
        IF sy-subrc EQ 0.
          ls_entityset-tplnr_fl = ls_rttrn-tplnr_fl.
        ENDIF.
        ls_entityset-cmnum    = ls_fmfpitm-cmnum."ls_fmfp-cmnum.
        ls_entityset-matnr    = ls_fmfpitm-matnr."ls_fmfp-matnr.
        ls_entityset-gstrp    = ls_fmfpitm-gstrp+6(2) && '.' && ls_fmfpitm-gstrp+4(2) && '.' && ls_fmfpitm-gstrp(4).

      ENDIF.
      ls_entityset-insplot = ls_qals-prueflos.
      ls_entityset-aufnr   = ls_qals-aufnr.
      ls_entityset-plnnr   = ls_qals-plnnr.

      READ TABLE lt_makt ASSIGNING FIELD-SYMBOL(<ls_makt>)
                      WITH KEY matnr = ls_fmfpitm-matnr
                      BINARY SEARCH.
      IF sy-subrc = 0.
        ls_entityset-maktx = <ls_makt>-maktx.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = ls_entityset-aufnr
        IMPORTING
          output = ls_entityset-aufnr.

      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
        EXPORTING
          input  = ls_entityset-tplnr_fl
        IMPORTING
          output = ls_entityset-tplnr_fl.

      ls_entityset-pernr = lv_persno.
      APPEND ls_entityset TO et_entityset.
      CLEAR : ls_entityset,ls_aufnr.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_qual_char_v1.

    "types.
    TYPES : BEGIN OF ty_qals,
              aufnr    TYPE aufnr,
              prueflos TYPE qplos,
            END OF ty_qals.
    "constants.
    CONSTANTS: lc_autyp TYPE /agri/gl_autyp VALUE 'TO',
               lc_qherk TYPE qherk          VALUE '03',
               lc_qpart TYPE qpart          VALUE '03',
               lc_astat TYPE /agri/glastat  VALUE 'A'.
    "local declarations.
    DATA: ls_key_tab        TYPE /iwbep/s_mgw_name_value_pair,
          lt_qals           TYPE TABLE OF ty_qals,
          ls_qals           TYPE ty_qals,
          lt_req_temp       TYPE TABLE OF bapi2045d1,
          ls_req_temp       TYPE bapi2045d1,
          lt_req            TYPE TABLE OF bapi2045d1,
          ls_req            TYPE bapi2045d1,
          lt_aufnr          TYPE TABLE OF aufnr,
          ls_aufnr          TYPE aufnr,
          lrt_prueflos      TYPE RANGE OF qplos,
          ls_prueflos       LIKE LINE OF lrt_prueflos,
          lrt_inspoper      TYPE RANGE OF qibpvornr,
          lrs_inspoper      LIKE LINE OF lrt_inspoper,
          lrt_pernr         TYPE RANGE OF persno,
          lr_pernr          LIKE LINE OF lrt_pernr,
          lrt_matnr         TYPE RANGE OF matnr,
          lrs_matnr         LIKE LINE OF lrt_matnr,
          lrt_werks         TYPE RANGE OF werks_d,
          lrs_werks         LIKE LINE OF lrt_werks,
          lv_persno         TYPE persno,
          lwa_prueflos      TYPE qplos,
          lt_prueflos       TYPE TABLE OF qplos,
          lt_oprn_temp      TYPE TABLE OF bapi2045l2,
          ls_oprn_temp      TYPE bapi2045l2,
          lt_oprn           TYPE TABLE OF bapi2045l2,
          ls_oprn           TYPE bapi2045l2,
          lt_filter         TYPE /iwbep/t_mgw_select_option,
          ls_filter         TYPE /iwbep/s_mgw_select_option,
          ls_so             TYPE /iwbep/s_cod_select_option,
          ls_entityset      LIKE LINE OF et_entityset,
          lv_filter_str     TYPE string,
          lo_filter         TYPE REF TO /iwbep/if_mgw_req_filter,
          ls_converted_keys LIKE LINE OF et_entityset,
          ls_enityset       LIKE LINE OF et_entityset,
          lt_constants      TYPE zabs_tty_vkey_const,
          ls_constants      TYPE zabs_str_vkey_const,
          lt_steus          TYPE RANGE OF steus,
          lrt_plnnr         TYPE RANGE OF plnnr,
          lr_plnnr          LIKE LINE OF lrt_plnnr.

    DATA: lv_index,
          lv_subrc            TYPE sy-subrc,
          lwa_activities_fcat TYPE /agri/s_fmfpact_fcat,
          lv_vornr            TYPE vornr,
          lv_prueflos         TYPE qibplosnr,
          lv_i                TYPE i,
          lv_fieldname        TYPE fieldname,
          lv_tabix            TYPE sy-tabix,
          ls_qmkst            TYPE qmkst.

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

    LOOP AT lt_filter INTO ls_filter.
      CASE ls_filter-property.
        WHEN 'INSPLOT'.
          lo_filter->convert_select_option(
            EXPORTING
              is_select_option = ls_filter
            IMPORTING
              et_select_option = lrt_prueflos ).
        WHEN 'INSPOPER'.
          lo_filter->convert_select_option(
            EXPORTING
              is_select_option = ls_filter
            IMPORTING
              et_select_option = lrt_inspoper ).
        WHEN 'MATNR'.
          lo_filter->convert_select_option(
            EXPORTING
              is_select_option = ls_filter
            IMPORTING
              et_select_option = lrt_matnr ).
        WHEN 'WERKS'.
          lo_filter->convert_select_option(
            EXPORTING
              is_select_option = ls_filter
            IMPORTING
              et_select_option = lrt_werks ).
        WHEN 'PLNNR'.
          lo_filter->convert_select_option(
            EXPORTING
              is_select_option = ls_filter
            IMPORTING
              et_select_option = lrt_plnnr ).
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
        WHEN OTHERS.
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

    IF lrt_pernr IS NOT INITIAL.
      SELECT *
      FROM zabs_usrpernr
      INTO TABLE @DATA(lt_rtusr)
      WHERE pernr IN @lrt_pernr.
    ENDIF.

    IF lt_rtusr IS NOT INITIAL.
      SELECT *
      FROM /agri/glrtfla
      INTO TABLE @DATA(lt_rtfla)
      FOR ALL ENTRIES IN @lt_rtusr                 "#EC CI_NO_TRANSFORM
      WHERE route = @lt_rtusr-route.
      IF sy-subrc = 0.
        SORT lt_rtfla BY route.

        SORT lt_rtfla BY tplnr_fl.
        DELETE ADJACENT DUPLICATES FROM lt_rtfla COMPARING tplnr_fl.
        IF lt_rtfla[] IS NOT INITIAL.
          SELECT tplnr_fl, swerk
          FROM /agri/glflot
          INTO TABLE @DATA(lt_flot)
          FOR ALL ENTRIES IN @lt_rtfla
           WHERE tplnr_fl = @lt_rtfla-tplnr_fl
             AND loevm    = @space.
          SORT lt_flot BY swerk.
          DELETE ADJACENT DUPLICATES FROM lt_flot COMPARING swerk.
          IF sy-subrc = 0.
            DATA: lr_werks TYPE RANGE OF swerk.
            lr_werks = VALUE #( FOR ls_flot IN lt_flot (
                                sign   = 'I'
                                option = 'EQ'
                                low    = ls_flot-swerk ) ).

            DELETE lr_werks WHERE low IS INITIAL.
            SORT lr_werks BY low.
            DELETE ADJACENT DUPLICATES FROM lr_werks COMPARING low.
          ENDIF.
        ENDIF.

*--Get variant table data for STEUS - Control Key
        CALL METHOD zcl_abs_get_variants=>get_constant_multiple
          EXPORTING
            iv_mod       = 'C'
            iv_objid     = 'MOBL'
            iv_k1val     = 'INSPOP'
            iv_k2val     = 'STEUS'
          IMPORTING
            et_constants = lt_constants.

        LOOP AT lt_constants INTO DATA(lwa_constants).
          APPEND VALUE #( low = lwa_constants-cnval1 sign = 'I' option = 'EQ' )
          TO lt_steus.
        ENDLOOP.

      ENDIF.
    ENDIF.

    "change -> Login User based validation to User+Employee based valiadtion
    SELECT a~task, b~urole
      FROM zabst_task_app AS a
      INNER JOIN zabs_emp_role AS b
      ON a~urole = b~urole
      INTO TABLE @DATA(lt_taskmat)
      WHERE b~pernr IN @lrt_pernr
        AND a~task IN @lrt_matnr
        AND a~stapp EQ @abap_true.
    IF sy-subrc NE 0.
*- If user role is not having any Task maintained - then dont show any task orders
      RETURN.
    ENDIF.
    IF lt_taskmat IS NOT INITIAL.

      SELECT matnr, werks, plnty, plnnr, plnal
      FROM mapl
      INTO TABLE @DATA(lt_mapl)
      FOR ALL ENTRIES IN @lt_taskmat
      WHERE matnr = @lt_taskmat-task
        AND werks IN @lr_werks
        AND plnty = 'N'
        AND plnnr IN @lrt_plnnr"BOC - 25.06.2020
        AND loekz = @space.
      IF sy-subrc = 0.

        SELECT plnty,plnnr,plnal,zaehl,loekz,verwe,werks
        FROM plko
        INTO TABLE @DATA(lt_plko)                  "#EC CI_NO_TRANSFORM
        FOR ALL ENTRIES IN @lt_mapl
        WHERE plnty EQ @lt_mapl-plnty
          AND plnnr EQ @lt_mapl-plnnr
          AND plnal EQ @lt_mapl-plnal
          AND datuv LE @sy-datum
          AND loekz EQ @space
          AND verwe EQ '1' "IN @lt_verwe
          AND werks EQ @lt_mapl-werks
          AND statu EQ '4'. "IN @li_statu. "Released Status for General

        IF sy-subrc = 0.
          SELECT plnty, plnnr, plnkn, vornr, steus, werks
          FROM plpo
          INTO TABLE @DATA(lt_plpo)
          FOR ALL ENTRIES IN @lt_plko
          WHERE plnty = @lt_plko-plnty
            AND plnnr = @lt_plko-plnnr
            AND loekz = @space
            AND steus IN @lt_steus
            AND werks = @lt_plko-werks.
          IF sy-subrc = 0.

            SELECT plnty, plnnr, plnkn, merknr, verwmerkm,
                   kurztext, formel1, steuerkz, mkversion,
                   auswmenge1,auswmgwrk1
             FROM plmk
            INTO TABLE @DATA(lt_plmk)
            FOR ALL ENTRIES IN @lt_plpo
            WHERE plnty = @lt_plpo-plnty
              AND plnnr = @lt_plpo-plnnr
              AND plnkn = @lt_plpo-plnkn
              AND loekz = @space.

          ENDIF.

        ENDIF.

      ENDIF.
    ENDIF.

    SELECT *
      FROM zabs_qachar_grp
      INTO TABLE @DATA(lt_qachargrp)
      FOR ALL ENTRIES IN @lt_plmk
      WHERE mstr_char EQ @lt_plmk-verwmerkm.
    SORT lt_qachargrp BY mstr_char.

    SORT: lt_mapl BY plnty plnnr plnal,
          lt_plpo BY plnty plnnr plnkn, "zaehl,
          lt_plmk BY plnty plnnr plnkn merknr." verwmerkm.

    LOOP AT lt_plko INTO DATA(ls_plko).

      READ TABLE lt_mapl INTO DATA(ls_mapl)
            WITH KEY plnty = ls_plko-plnty
                     plnnr = ls_plko-plnnr
                     plnal = ls_plko-plnal BINARY SEARCH.
      IF sy-subrc = 0.
        ls_entityset-matnr = ls_mapl-matnr.
        ls_entityset-werks = ls_mapl-werks.
      ELSE.
        CONTINUE.
      ENDIF.

      LOOP AT lt_plpo INTO DATA(ls_plpo)
             WHERE plnty = ls_plko-plnty
               AND plnnr = ls_plko-plnnr
               AND werks = ls_plko-werks.

        ls_entityset-inspoper = ls_plpo-vornr.
        ls_entityset-pernr = lv_persno.

        CLEAR lv_tabix.
        READ TABLE lt_plmk TRANSPORTING NO FIELDS
                           WITH KEY plnty = ls_plpo-plnty
                                    plnnr = ls_plpo-plnnr
                                    plnkn = ls_plpo-plnkn
                                    BINARY SEARCH.
        IF sy-subrc EQ 0.
          lv_tabix = sy-tabix.
          LOOP AT lt_plmk INTO DATA(ls_plmk) FROM lv_tabix.
            IF ls_plmk-plnty <> ls_plpo-plnty
              OR ls_plmk-plnnr <> ls_plpo-plnnr
              OR ls_plmk-plnkn <> ls_plpo-plnkn.
              CLEAR lv_i.
              EXIT.
            ENDIF.
            lv_i = lv_i + 1.
            ls_entityset-inspmicseq = lv_i.
            ls_entityset-plnnr     = ls_plmk-plnnr.
            ls_entityset-inspchar  = ls_plmk-merknr.

            IF ls_plmk-auswmenge1 IS INITIAL.
              ls_entityset-mstr_char = ls_plmk-verwmerkm.
            ELSE.
              ls_entityset-mstr_char = ls_plmk-auswmenge1.
            ENDIF.
            ls_entityset-char_descr = ls_plmk-kurztext.
            MOVE-CORRESPONDING ls_plmk TO ls_qmkst.
            ls_qmkst-quantitat = ls_plmk-steuerkz+0(1).
            IF ls_qmkst-quantitat EQ 'X'.
              ls_entityset-char       = '01'."Quantitative
            ELSEIF ls_qmkst-quantitat EQ space.
              ls_entityset-char       = '02'. "Qualitative
            ENDIF.

            ls_entityset-formula = ls_plmk-formel1.
            ls_entityset-version = ls_plmk-mkversion.
            READ TABLE lt_qachargrp ASSIGNING FIELD-SYMBOL(<fs_qachargrp>)
                                    WITH KEY mstr_char = ls_plmk-verwmerkm
                                    BINARY SEARCH.
            IF sy-subrc = 0.
              ls_entityset-inspgroup  = <fs_qachargrp>-inspgroup.
              ls_entityset-inspgrpseq = <fs_qachargrp>-grp_seq.

              APPEND ls_entityset TO et_entityset.
              CLEAR : ls_qmkst.
            ENDIF.

          ENDLOOP.
        ENDIF.
      ENDLOOP.
      CLEAR : ls_entityset.
    ENDLOOP.

    SORT et_entityset BY inspgrpseq.

  ENDMETHOD.


  METHOD get_route_terrain_dtls.

    "local declarations.
    DATA:
      lt_rtfla             TYPE TABLE OF /agri/glrtfla,
      ls_rtfla             TYPE /agri/glrtfla,
      ls_entityset         LIKE LINE OF et_entityset,
      lt_strno             TYPE TABLE OF /agri/glstrno,
      ls_strno             TYPE /agri/glstrno,
      lrt_tplnr_fl         TYPE RANGE OF /agri/gltplnr_fl,
      lrs_tplnr_fl         LIKE LINE OF lrt_tplnr_fl,
      lrt_pernr            TYPE RANGE OF persno,
      lr_pernr             LIKE LINE OF lrt_pernr,
      lrt_lifnr            TYPE RANGE OF lifnr,
      lrt_cmnum            TYPE RANGE OF /agri/glcmnum,
      ls_cmnum             LIKE LINE OF lrt_cmnum,
      lr_lifnr             LIKE LINE OF lrt_lifnr,
      lt_rtusr             TYPE TABLE OF zabs_usrpernr,
      lo_message_container TYPE REF TO /iwbep/if_message_container,
      lv_tmstmp            TYPE ad_tstamp.

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
      ENDLOOP.
    ENDIF.

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

*---Fetching Route based Terrain
    IF lt_rtusr IS NOT INITIAL.
      SELECT * FROM /agri/glrtfla
            INTO TABLE @lt_rtfla
        FOR ALL ENTRIES IN @lt_rtusr               "#EC CI_NO_TRANSFORM
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
             strno, aedat, erdat
        FROM /agri/glflot
        INTO TABLE @DATA(lt_glflot)
        FOR ALL ENTRIES IN @lt_rtfla               "#EC CI_NO_TRANSFORM
        WHERE tplnr_fl   = @lt_rtfla-tplnr_fl
          AND kfrst      = @space
          AND loevm      = @space. "abap_false.
      IF sy-subrc = 0.
        SORT lt_glflot BY tplnr_fl.

        SELECT  tplnr_fl,                              "#EC CI_DYNWHERE
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
        FOR ALL ENTRIES IN @lt_glflot              "#EC CI_NO_TRANSFORM
             WHERE tplnr_fl = @lt_glflot-tplnr_fl
                AND cmnum IN @lrt_cmnum
                AND astat EQ @lc_astat
                AND datab LE @sy-datum
                AND datbi GE @lv_prevdate
                AND loevm  = @space.

        SELECT tplnr_fl, tplma, rbnr1
          FROM /agri/glflot
          INTO TABLE @DATA(lt_glflota)
           FOR ALL ENTRIES IN @lt_glflot
         WHERE tplnr_fl = @lt_glflot-tplma
           AND loevm    = @space."abap_true.
        IF sy-subrc = 0.
          SORT lt_glflota BY tplnr_fl.
        ENDIF.

      ENDIF. "lt_glflot
    ENDIF. "lt_rtfla

    LOOP AT lt_flcma ASSIGNING FIELD-SYMBOL(<fs_flcma>).

      READ TABLE lt_glflot INTO DATA(ls_glflot) WITH KEY
                                          tplnr_fl = <fs_flcma>-tplnr_fl
                                          BINARY SEARCH.
      IF sy-subrc = 0.
        ls_entityset-cmnum = <fs_flcma>-cmnum.
        ls_entityset-strno = ls_glflot-strno.
        ls_entityset-pltxt = ls_glflot-pltxt.

        CALL FUNCTION 'ADDR_CONVERT_DATE_TO_TIMESTAMP'
          EXPORTING
            iv_date      = ls_glflot-erdat
          IMPORTING
            ev_timestamp = lv_tmstmp.

        ls_entityset-erdat = lv_tmstmp.

        CALL FUNCTION 'ADDR_CONVERT_DATE_TO_TIMESTAMP'
          EXPORTING
            iv_date      = ls_glflot-aedat
          IMPORTING
            ev_timestamp = lv_tmstmp.

        ls_entityset-aedat = lv_tmstmp.

        READ TABLE lt_glflota INTO DATA(ls_glflota) WITH KEY
                                          tplnr_fl = ls_glflot-tplma
                                          BINARY SEARCH.
        IF sy-subrc = 0.
          ls_entityset-rbnr1 = ls_glflota-rbnr1.
        ENDIF.

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


  METHOD get_terrquaset.

    "Constants.
    CONSTANTS: lc_autyp TYPE /agri/gl_autyp VALUE 'TO',
               lc_cstat TYPE /agri/fmcstat  VALUE 'CNF',
               lc_qherk TYPE qherk          VALUE '03'.

    "Types.
    TYPES : BEGIN OF ty_qals,
              prueflos TYPE qplos,
              aufnr    TYPE aufnr,
            END OF ty_qals.

    TYPES : BEGIN OF ty_mob_ilot,
              prueflos TYPE qplos,
              cflag    TYPE zabs_cflag,
            END OF ty_mob_ilot.

    "Local Declarations.
    DATA: lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
          lt_filter     TYPE /iwbep/t_mgw_select_option,
          ls_filter     TYPE /iwbep/s_mgw_select_option,
          lv_filter_str TYPE string.

    DATA: lt_flot      TYPE TABLE OF /agri/glflot,
          lt_qals      TYPE TABLE OF ty_qals,
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
          lt_constants TYPE zabs_tty_vkey_const,
          lt_rttrn     TYPE zcl_zabs_agri_mobile_e_mpc=>tt_worouteterrainextend,
          lt_rttrn_tmp TYPE zcl_zabs_agri_mobile_e_mpc=>tt_worouteterrainextend,
          ls_rttrn     TYPE zcl_zabs_agri_mobile_e_mpc=>ts_worouteterrainextend,
          lt_entityset TYPE zcl_zabs_agri_mobile_e_mpc=>tt_terrainqua_ext,
          ls_entityset LIKE LINE OF et_entityset,
          lt_orderby   TYPE /iwbep/t_mgw_tech_order,
          ls_orderby   TYPE /iwbep/s_mgw_tech_order,
          lt_rtusr     TYPE TABLE OF zabs_usrpernr,
          lv_tmstmp    TYPE ad_tstamp,
          lv_enstehdat TYPE sy-datum,
          lv_index     TYPE sy-tabix,
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
    SELECT a~task, b~urole
       FROM zabst_task_app AS a
       INNER JOIN zabs_emp_role AS b
       ON a~urole = b~urole
       INTO TABLE @DATA(lt_taskmat)
       WHERE b~pernr IN @lrt_pernr
         AND a~stapp EQ @abap_true.

    IF sy-subrc NE 0.
*- If user role is not having any Task maintained - then dont show any task orders
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

*--Get variant table data
    CALL METHOD zcl_abs_get_variants=>get_constant_multiple
      EXPORTING
        iv_mod       = 'C'
        iv_objid     = 'MOBL'
        iv_k1val     = 'INSPOP'
        iv_k2val     = 'STEUS'
      IMPORTING
        et_constants = lt_constants.

    DATA lr_steus TYPE RANGE OF steus.
    lr_steus =  VALUE #( FOR ls_constant IN lt_constants (
    sign   = 'I'
    option = 'EQ'
    low    = ls_constant-cnval1 ) ).
    DELETE lr_steus WHERE low IS INITIAL.
    SORT lr_steus BY low.
    DELETE ADJACENT DUPLICATES FROM lr_steus COMPARING low.

    SELECT a~aufnr,
           a~tplnr_fl
      FROM /agri/fmfphdr AS a
     INNER JOIN /agri/fmfpitm AS b
        ON b~aufnr = a~aufnr
      INTO TABLE @DATA(lt_fphdr) "et_entityset
       FOR ALL ENTRIES IN @lt_rttrn"lt_flott
     WHERE a~autyp    = @lc_autyp
       AND a~tplnr_fl = @lt_rttrn-tplnr"lt_flott-tplnr_fl
       AND a~matnr   IN @lr_matnr
       AND a~gstrp   IN @ltr_gstrp "BOC - 03.07.2020
       AND a~tecom    = @space
       AND a~cstat   <> @lc_cstat "BOC - 22.06.2020.
       AND b~steus   IN @lr_steus.

    IF sy-subrc = 0.

      DATA : lv_low_pastrterm    TYPE sy-datum,
             lv_high_pastrterm   TYPE sy-datum,
             lv_cnval1_pastrterm TYPE zabs_del_cnval,
             lr_pastrterm        TYPE RANGE OF qprstart.
      CALL METHOD zcl_abs_get_variants=>get_constant_single
        EXPORTING
          iv_mod    = 'C'
          iv_objid  = 'MOBL'
          iv_k1val  = 'INSPOP'
          iv_k2val  = 'LPSTRTRM'
          iv_k3val  = lv_urole
        IMPORTING
          ev_cnval1 = lv_cnval1_pastrterm.
      CONDENSE lv_cnval1_pastrterm.
      lv_low_pastrterm   = sy-datum - lv_cnval1_pastrterm.
      CALL METHOD zcl_abs_get_variants=>get_constant_single
        EXPORTING
          iv_mod    = 'C'
          iv_objid  = 'MOBL'
          iv_k1val  = 'INSPOP'
          iv_k2val  = 'HPSTRTRM'
          iv_k3val  = lv_urole
        IMPORTING
          ev_cnval1 = lv_cnval1_pastrterm.
      CONDENSE lv_cnval1_pastrterm.
      lv_high_pastrterm = sy-datum + lv_cnval1_pastrterm.
      lr_pastrterm = VALUE #( sign = 'I' option = 'BT'
                              ( low = lv_low_pastrterm
                                high = lv_high_pastrterm ) ).

      DATA : lr_slwbez TYPE RANGE OF qslwbez.
      lr_slwbez = VALUE #( sign = 'I' option = 'EQ' ( low = 'Z23' )
                                                    ( low = 'Z28' ) ).
      SELECT prueflos, aufnr
        FROM qals
       INTO TABLE @lt_qals
        FOR ALL ENTRIES IN @lt_fphdr               "#EC CI_NO_TRANSFORM
        WHERE herkunft = @lc_qherk
          AND pastrterm IN @lr_pastrterm
          AND aufnr    = @lt_fphdr-aufnr
          AND slwbez   IN @lr_slwbez.

      IF lt_qals IS NOT INITIAL.

        SORT lt_qals BY aufnr.

        SELECT prueflos, zzcflag
          FROM zabs_mob_ilot
          INTO TABLE @DATA(lt_mob_ilot)
           FOR ALL ENTRIES IN @lt_qals
         WHERE prueflos = @lt_qals-prueflos.

        IF sy-subrc EQ 0.
          SORT lt_mob_ilot BY prueflos
                              zzcflag.
        ENDIF.

        SELECT insplot, cflag
          FROM zabs_qchar_hdr
          INTO TABLE @DATA(lt_qchar_hdr)
           FOR ALL ENTRIES IN @lt_qals
         WHERE insplot EQ @lt_qals-prueflos
           AND cflag   EQ @abap_true.
        IF sy-subrc = 0.
          SORT lt_qchar_hdr BY insplot cflag.
        ENDIF.

      ENDIF.

    ENDIF.

    SORT lt_rttrn BY tplnr_fl.
    SORT lt_fphdr BY tplnr_fl.
    DELETE ADJACENT DUPLICATES FROM lt_rttrn COMPARING tplnr_fl.

    SORT lt_rttrn BY tplnr_fl.

    LOOP AT lt_rttrn INTO ls_rttrn.
      READ TABLE lt_fphdr TRANSPORTING NO FIELDS
                          WITH KEY tplnr_fl = ls_rttrn-tplnr
                          BINARY SEARCH.
      IF sy-subrc = 0.
        lv_index = sy-tabix.
        LOOP AT lt_fphdr ASSIGNING FIELD-SYMBOL(<fs_fphdr>) FROM lv_index.
          IF <fs_fphdr>-tplnr_fl NE ls_rttrn-tplnr.
            EXIT.
          ENDIF.
          READ TABLE lt_qals ASSIGNING FIELD-SYMBOL(<fs_qals>)
                             WITH KEY aufnr = <fs_fphdr>-aufnr
                             BINARY SEARCH.
          IF sy-subrc = 0.

            READ TABLE lt_qchar_hdr INTO DATA(ls_qchar_hdr)
                               WITH KEY insplot = <fs_qals>-prueflos
                                        cflag   = abap_true
                               BINARY SEARCH.
            IF sy-subrc EQ 0.
              CLEAR : ls_qchar_hdr.
              CONTINUE.
            ENDIF.

            READ TABLE lt_mob_ilot TRANSPORTING NO FIELDS
                           WITH KEY prueflos = <fs_qals>-prueflos
                                    zzcflag  = abap_true
                           BINARY SEARCH.
            IF sy-subrc <> 0.
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
                  output         = ls_entityset-msehi
                EXCEPTIONS
                  unit_not_found = 1
                  OTHERS         = 2.
              IF sy-subrc <> 0.
*      Implement suitable error handling here
              ENDIF.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                EXPORTING
                  input  = ls_entityset-owner
                IMPORTING
                  output = ls_entityset-owner.

              ls_entityset-pernr = lv_persno.
              ls_entityset-lifnr = lv_lifnr.

              CLEAR: ls_entityset-aedat, ls_entityset-erdat.
              APPEND ls_entityset TO lt_entityset.
              CLEAR: ls_entityset.
            ENDIF.

          ENDIF.
        ENDLOOP.
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

  ENDMETHOD.


  METHOD insppoints_recording_update.

    DATA : lt_qchar_itm       TYPE TABLE OF zabs_qchar_itm,
           ls_qchar_hdr       TYPE zabs_qchar_hdr,
           ls_qchar_itm       TYPE zabs_qchar_itm,
           lt_messages        TYPE /agri/t_gprolog,
           ls_messages        TYPE /agri/s_gprolog,
           lref_msg_container TYPE REF TO /iwbep/if_message_container.

    lref_msg_container  = mo_context->get_message_container( ).

    LOOP AT im_t_qulachr INTO DATA(ls_qulachr).
      ls_qchar_hdr-zzchar = ls_qulachr-char.
      ls_qchar_hdr-ernam  = ls_qchar_hdr-mobusr = sy-uname.
      ls_qchar_hdr-erdat  = sy-datum.
      ls_qchar_hdr-erzet  = sy-uzeit.
      CLEAR: ls_qulachr-inspmicseq,ls_qulachr-inspgrpseq.
      MOVE-CORRESPONDING ls_qulachr TO ls_qchar_hdr.
      MOVE-CORRESPONDING ls_qulachr TO ls_qchar_itm.
      APPEND ls_qchar_itm TO lt_qchar_itm.
      CLEAR : ls_qchar_itm.
    ENDLOOP.

    DATA : lv_subrc TYPE sy-subrc.

    IF ls_qchar_hdr IS NOT INITIAL AND
       lt_qchar_itm IS NOT INITIAL.
      INSERT zabs_qchar_hdr FROM ls_qchar_hdr.
      COMMIT WORK.
      lv_subrc = sy-subrc.
      INSERT zabs_qchar_itm FROM TABLE lt_qchar_itm.
      COMMIT WORK.

      IF sy-subrc IS INITIAL AND lv_subrc IS INITIAL.
        ls_messages-msgty = zcl_abs_abap_maintain=>c_msgty_success. "'S''S'.
        ls_messages-msgv1 = TEXT-006.                       "#EC NOTEXT
        ls_messages-msgid = '00'.
        ls_messages-msgno = '208'.
        APPEND ls_messages TO lt_messages.
        exception_messages( EXPORTING it_messages = lt_messages ).
      ELSE.
        ls_messages-msgty = zcl_abs_abap_maintain=>c_msgty_error. "'E'.
        ls_messages-msgv1 = TEXT-007.                       "#EC NOTEXT
        ls_messages-msgid = '00'.
        ls_messages-msgno = '208'.
        APPEND ls_messages TO lt_messages.
        exception_messages( EXPORTING it_messages = lt_messages ).
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD qualcharextset_get_entityset.

*-- Variables
    DATA: lv_totrec     TYPE i,
          lv_entity_rec TYPE i,
          lv_srtfd      TYPE indx_srtfd.

**--Constants
    CONSTANTS lc_rule(7) TYPE c VALUE 'ZZQACHR'.

*-- Internal Table's
    DATA : lt_filter TYPE /iwbep/t_mgw_select_option.

*-- Get Filters defined
    lt_filter = io_tech_request_context->get_filter( )->get_filter_select_options( ).

    CONCATENATE lc_rule sy-uname INTO lv_srtfd.

    IF io_tech_request_context->get_skiptoken( ) IS INITIAL.
      CALL METHOD me->delete_from_database
        EXPORTING
          tabname          = 'INDX'
          client           = sy-mandt
          area             = 'ID'
          id               = lv_srtfd
          client_specified = abap_false.
    ELSE.
      IMPORT: lt_dummy TO et_entityset FROM DATABASE indx(id) ID lv_srtfd.
    ENDIF.

    IF et_entityset IS INITIAL. "sy-subrc <> 0.

      TRY.
          get_qual_char_v1(
          EXPORTING
            io_tech_request_context = io_tech_request_context
          IMPORTING
            et_entityset            = et_entityset ).
          ##NO_HANDLER
        CATCH /iwbep/cx_mgw_busi_exception .
          ##NO_HANDLER
        CATCH /iwbep/cx_mgw_tech_exception .
      ENDTRY.

      IF et_entityset IS NOT INITIAL.
        EXPORT lt_dummy FROM et_entityset TO DATABASE indx(id) ID lv_srtfd.
      ENDIF.

    ENDIF.

    DESCRIBE TABLE et_entityset LINES lv_entity_rec.

*** Inlinecount
    IF io_tech_request_context->has_inlinecount( ) = abap_true.
      DESCRIBE TABLE et_entityset LINES es_response_context-inlinecount.
    ELSE.
      CLEAR es_response_context-inlinecount.
    ENDIF.

    CALL METHOD me->get_skip_token
      EXPORTING
        io_tech_request_context = io_tech_request_context
      IMPORTING
        es_response_context     = es_response_context
        ev_totrecords           = lv_totrec
      CHANGING
        ct_entityset            = et_entityset.

    IF et_entityset IS INITIAL
      OR lv_entity_rec <= lv_totrec.

      CLEAR: es_response_context-skiptoken,
             es_response_context-inlinecount.

      CALL METHOD me->delete_from_database
        EXPORTING
          tabname          = 'INDX'
          client           = sy-mandt
          area             = 'ID'
          id               = lv_srtfd
          client_specified = abap_false.

    ENDIF.

  ENDMETHOD.


  METHOD qualcharfetch_ex_get_entityset.

*-- Variables
    DATA: lv_totrec     TYPE i,
          lv_entity_rec TYPE i,
          lv_srtfd      TYPE indx_srtfd.

**--Constants
    CONSTANTS lc_rule(7) TYPE c VALUE 'ZZQAFCH'.

*-- Internal Table's
    DATA : lt_filter TYPE /iwbep/t_mgw_select_option.

*-- Get Filters defined
    lt_filter = io_tech_request_context->get_filter( )->get_filter_select_options( ).

    CONCATENATE lc_rule sy-uname INTO lv_srtfd.

    IF io_tech_request_context->get_skiptoken( ) IS INITIAL.
      CALL METHOD me->delete_from_database
        EXPORTING
          tabname          = 'INDX'
          client           = sy-mandt
          area             = 'ID'
          id               = lv_srtfd
          client_specified = abap_false.
    ELSE.
      IMPORT: lt_dummy TO et_entityset FROM DATABASE indx(id) ID lv_srtfd.
    ENDIF.

    IF et_entityset IS INITIAL. "sy-subrc <> 0.
      TRY.
          get_qualchar_fetch(
          EXPORTING
            io_tech_request_context = io_tech_request_context
          IMPORTING
            et_entityset            = et_entityset ).
          ##NO_HANDLER
        CATCH /iwbep/cx_mgw_busi_exception .
          ##NO_HANDLER
        CATCH /iwbep/cx_mgw_tech_exception .
      ENDTRY.

      IF et_entityset IS NOT INITIAL.
        EXPORT lt_dummy FROM et_entityset TO DATABASE indx(id) ID lv_srtfd.
      ENDIF.

    ENDIF.

    DESCRIBE TABLE et_entityset LINES lv_entity_rec.

*** Inlinecount
    IF io_tech_request_context->has_inlinecount( ) = abap_true.
      DESCRIBE TABLE et_entityset LINES es_response_context-inlinecount.
    ELSE.
      CLEAR es_response_context-inlinecount.
    ENDIF.

    CALL METHOD me->get_skip_token
      EXPORTING
        io_tech_request_context = io_tech_request_context
      IMPORTING
        es_response_context     = es_response_context
        ev_totrecords           = lv_totrec
      CHANGING
        ct_entityset            = et_entityset.

    IF et_entityset IS INITIAL
      OR lv_entity_rec <= lv_totrec.

      CLEAR: es_response_context-skiptoken,
             es_response_context-inlinecount.

      CALL METHOD me->delete_from_database
        EXPORTING
          tabname          = 'INDX'
          client           = sy-mandt
          area             = 'ID'
          id               = lv_srtfd
          client_specified = abap_false.

    ENDIF.

  ENDMETHOD.


  METHOD qualoprextendset_get_entityset.

*-- Variables
    DATA: lv_totrec     TYPE i,
          lv_entity_rec TYPE i,
          lv_srtfd      TYPE indx_srtfd.

**--Constants
    CONSTANTS lc_rule(7) TYPE c VALUE 'ZZQAOPR'.

*-- Internal Table's
    DATA : lt_filter TYPE /iwbep/t_mgw_select_option.

*-- Get Filters defined
    lt_filter = io_tech_request_context->get_filter( )->get_filter_select_options( ).

    CONCATENATE lc_rule sy-uname INTO lv_srtfd.

    IF io_tech_request_context->get_skiptoken( ) IS INITIAL.
      CALL METHOD me->delete_from_database
        EXPORTING
          tabname          = 'INDX'
          client           = sy-mandt
          area             = 'ID'
          id               = lv_srtfd
          client_specified = abap_false.
    ELSE.
      IMPORT: lt_dummy TO et_entityset FROM DATABASE indx(id) ID lv_srtfd.
    ENDIF.

    IF et_entityset IS INITIAL. "sy-subrc <> 0.

      TRY.
          CALL METHOD me->get_quality_operations_extend
            EXPORTING
              io_tech_request_context = io_tech_request_context
            IMPORTING
              et_entityset            = et_entityset.
          ##NO_HANDLER
        CATCH /iwbep/cx_mgw_busi_exception .
          ##NO_HANDLER
        CATCH /iwbep/cx_mgw_tech_exception .
      ENDTRY.

      IF et_entityset IS NOT INITIAL.
        EXPORT lt_dummy FROM et_entityset TO DATABASE indx(id) ID lv_srtfd.
      ENDIF.

    ENDIF.

    DESCRIBE TABLE et_entityset LINES lv_entity_rec.

*** Inlinecount
    IF io_tech_request_context->has_inlinecount( ) = abap_true.
      DESCRIBE TABLE et_entityset LINES es_response_context-inlinecount.
    ELSE.
      CLEAR es_response_context-inlinecount.
    ENDIF.

    CALL METHOD me->get_skip_token
      EXPORTING
        io_tech_request_context = io_tech_request_context
      IMPORTING
        es_response_context     = es_response_context
        ev_totrecords           = lv_totrec
      CHANGING
        ct_entityset            = et_entityset.

    IF et_entityset IS INITIAL
      OR lv_entity_rec <= lv_totrec.

      CLEAR: es_response_context-skiptoken,
             es_response_context-inlinecount.

      CALL METHOD me->delete_from_database
        EXPORTING
          tabname          = 'INDX'
          client           = sy-mandt
          area             = 'ID'
          id               = lv_srtfd
          client_specified = abap_false.

    ENDIF.

  ENDMETHOD.


  METHOD qualtaskord_exts_get_entityset.

*-- Variables
    DATA: lv_totrec     TYPE i,
          lv_entity_rec TYPE i,
          lv_srtfd      TYPE indx_srtfd.

**--Constants
    CONSTANTS lc_rule(7) TYPE c VALUE 'ZZQATSK'.

*-- Internal Table's
    DATA : lt_filter TYPE /iwbep/t_mgw_select_option.

*-- Get Filters defined
    lt_filter = io_tech_request_context->get_filter( )->get_filter_select_options( ).

    CONCATENATE lc_rule sy-uname INTO lv_srtfd.

    IF io_tech_request_context->get_skiptoken( ) IS INITIAL.
      CALL METHOD me->delete_from_database
        EXPORTING
          tabname          = 'INDX'
          client           = sy-mandt
          area             = 'ID'
          id               = lv_srtfd
          client_specified = abap_false.
    ELSE.
      IMPORT: lt_dummy TO et_entityset FROM DATABASE indx(id) ID lv_srtfd.
    ENDIF.

    IF et_entityset IS INITIAL. "sy-subrc <> 0.

      TRY.
          get_qualtaskord(
       EXPORTING
         io_tech_request_context = io_tech_request_context
       IMPORTING
         et_entityset            = et_entityset ).
          ##NO_HANDLER
        CATCH /iwbep/cx_mgw_busi_exception .
          ##NO_HANDLER
        CATCH /iwbep/cx_mgw_tech_exception .
      ENDTRY.

      IF et_entityset IS NOT INITIAL.
        EXPORT lt_dummy FROM et_entityset TO DATABASE indx(id) ID lv_srtfd.
      ENDIF.

    ENDIF.

    DESCRIBE TABLE et_entityset LINES lv_entity_rec.

*** Inlinecount
    IF io_tech_request_context->has_inlinecount( ) = abap_true.
      DESCRIBE TABLE et_entityset LINES es_response_context-inlinecount.
    ELSE.
      CLEAR es_response_context-inlinecount.
    ENDIF.

    CALL METHOD me->get_skip_token
      EXPORTING
        io_tech_request_context = io_tech_request_context
      IMPORTING
        es_response_context     = es_response_context
        ev_totrecords           = lv_totrec
      CHANGING
        ct_entityset            = et_entityset.

    IF et_entityset IS INITIAL
      OR lv_entity_rec <= lv_totrec.

      CLEAR: es_response_context-skiptoken,
             es_response_context-inlinecount.

      CALL METHOD me->delete_from_database
        EXPORTING
          tabname          = 'INDX'
          client           = sy-mandt
          area             = 'ID'
          id               = lv_srtfd
          client_specified = abap_false.

    ENDIF.

  ENDMETHOD.


  METHOD qulachrf4_extset_get_entityset.

*-- Variables
    DATA: lv_totrec     TYPE i,
          lv_entity_rec TYPE i,
          lv_srtfd      TYPE indx_srtfd.

**--Constants
    CONSTANTS lc_rule(7) TYPE c VALUE 'ZZQACF4'.

*-- Internal Table's
    DATA : lt_filter TYPE /iwbep/t_mgw_select_option.

*-- Get Filters defined
    lt_filter = io_tech_request_context->get_filter( )->get_filter_select_options( ).

    CONCATENATE lc_rule sy-uname INTO lv_srtfd.

    IF io_tech_request_context->get_skiptoken( ) IS INITIAL.
      CALL METHOD me->delete_from_database
        EXPORTING
          tabname          = 'INDX'
          client           = sy-mandt
          area             = 'ID'
          id               = lv_srtfd
          client_specified = abap_false.
    ELSE.
      IMPORT: lt_dummy TO et_entityset FROM DATABASE indx(id) ID lv_srtfd.
    ENDIF.

    IF et_entityset IS INITIAL. "sy-subrc <> 0.

      TRY.
          get_qualcharf4ext(
          EXPORTING
            io_tech_request_context = io_tech_request_context
          IMPORTING
            et_entityset            = et_entityset ).
          ##NO_HANDLER
        CATCH /iwbep/cx_mgw_busi_exception .
          ##NO_HANDLER
        CATCH /iwbep/cx_mgw_tech_exception .
      ENDTRY.

      IF et_entityset IS NOT INITIAL.
        EXPORT lt_dummy FROM et_entityset TO DATABASE indx(id) ID lv_srtfd.
      ENDIF.

    ENDIF.

    DESCRIBE TABLE et_entityset LINES lv_entity_rec.

*** Inlinecount
    IF io_tech_request_context->has_inlinecount( ) = abap_true.
      DESCRIBE TABLE et_entityset LINES es_response_context-inlinecount.
    ELSE.
      CLEAR es_response_context-inlinecount.
    ENDIF.

    CALL METHOD me->get_skip_token
      EXPORTING
        io_tech_request_context = io_tech_request_context
      IMPORTING
        es_response_context     = es_response_context
        ev_totrecords           = lv_totrec
      CHANGING
        ct_entityset            = et_entityset.

    IF et_entityset IS INITIAL
      OR lv_entity_rec <= lv_totrec.

      CLEAR: es_response_context-skiptoken,
             es_response_context-inlinecount.

      CALL METHOD me->delete_from_database
        EXPORTING
          tabname          = 'INDX'
          client           = sy-mandt
          area             = 'ID'
          id               = lv_srtfd
          client_specified = abap_false.

    ENDIF.

  ENDMETHOD.


  METHOD request_header.

*-- Service Request/Response Headers
    DATA: lwa_header  TYPE ihttpnvp.

    lwa_header-name = 'Content-Encoding' ##NO_TEXT .
    lwa_header-value = 'gzip,deflate'.
    /iwbep/if_mgw_conv_srv_runtime~set_header( lwa_header ).

  ENDMETHOD.


  METHOD terrainqua_extse_get_entityset.

*-- Variables
    DATA: lv_totrec     TYPE i,
          lv_entity_rec TYPE i,
          lv_srtfd      TYPE indx_srtfd.

**--Constants
    CONSTANTS lc_rule(7) TYPE c VALUE 'ZZQATRR'.

*-- Internal Table's
    DATA : lt_filter TYPE /iwbep/t_mgw_select_option.

*-- Get Filters defined
    lt_filter = io_tech_request_context->get_filter( )->get_filter_select_options( ).

    CONCATENATE lc_rule sy-uname INTO lv_srtfd.

    IF io_tech_request_context->get_skiptoken( ) IS INITIAL.
      CALL METHOD me->delete_from_database
        EXPORTING
          tabname          = 'INDX'
          client           = sy-mandt
          area             = 'ID'
          id               = lv_srtfd
          client_specified = abap_false.
    ELSE.
      IMPORT: lt_dummy TO et_entityset FROM DATABASE indx(id) ID lv_srtfd.
    ENDIF.

    IF et_entityset IS INITIAL. "sy-subrc <> 0.

      TRY.
          get_terrquaset(
         EXPORTING
           io_tech_request_context = io_tech_request_context
         IMPORTING
           et_entityset            = et_entityset ).
          ##NO_HANDLER
        CATCH /iwbep/cx_mgw_busi_exception .
          ##NO_HANDLER
        CATCH /iwbep/cx_mgw_tech_exception .
      ENDTRY.

      IF et_entityset IS NOT INITIAL.
        EXPORT lt_dummy FROM et_entityset TO DATABASE indx(id) ID lv_srtfd.
      ENDIF.

    ENDIF.

    DESCRIBE TABLE et_entityset LINES lv_entity_rec.

*** Inlinecount
    IF io_tech_request_context->has_inlinecount( ) = abap_true.
      DESCRIBE TABLE et_entityset LINES es_response_context-inlinecount.
    ELSE.
      CLEAR es_response_context-inlinecount.
    ENDIF.

    CALL METHOD me->get_skip_token
      EXPORTING
        io_tech_request_context = io_tech_request_context
      IMPORTING
        es_response_context     = es_response_context
        ev_totrecords           = lv_totrec
      CHANGING
        ct_entityset            = et_entityset.

    IF et_entityset IS INITIAL
      OR lv_entity_rec <= lv_totrec.

      CLEAR: es_response_context-skiptoken,
             es_response_context-inlinecount.

      CALL METHOD me->delete_from_database
        EXPORTING
          tabname          = 'INDX'
          client           = sy-mandt
          area             = 'ID'
          id               = lv_srtfd
          client_specified = abap_false.

    ENDIF.

  ENDMETHOD.


  METHOD uploading_imgext_insp_point.

    DATA:
*-- Tables
      lt_objhead         TYPE TABLE OF soli,
      lt_binary_content  TYPE solix_tab,
      lt_soli_tab        TYPE soli_tab,
*-- Structures
      ls_obj_id          TYPE soodk,
      ls_obj_data        TYPE sood1,
      ls_trdetails       TYPE /agri/cl_mobile_mpc=>ts_upload,
      ls_parameter       TYPE /iwbep/s_mgw_name_value_pair,
      ls_object          TYPE borident,
      ls_note            TYPE borident,
      ls_folmem_k        TYPE sofmk,
      ls_fol_id          TYPE soodk,
      lwa_binary_content TYPE solix,
*-- Variables
      lv_xstring         TYPE xstring,
      lv_ep_note         TYPE borident-objkey,
      lv_insplot         TYPE zcl_zabs_mob_qins_mpc=>ts_qualcharext-insplot,
      lv_oper            TYPE zcl_zabs_mob_qins_mpc=>ts_qualcharext-inspoper,
      lv_userc1          TYPE qusrchar18,
      lv_userc2          TYPE qusrchar10,
      lv_usern           TYPE qusrnumc10,
      lv_usern2          TYPE qusrnumc10,
      lv_probenr         TYPE qprobenrpp.

*-- Declare FS for subsequent message assignment.
    APPEND INITIAL LINE TO et_messages ASSIGNING FIELD-SYMBOL(<fs_messages>).

    CALL FUNCTION 'SSFC_BASE64_DECODE'
      EXPORTING
        b64data                  = im_s_imgupload-value
      IMPORTING
        bindata                  = lv_xstring
      EXCEPTIONS
        ssf_krn_error            = 1
        ssf_krn_noop             = 2
        ssf_krn_nomemory         = 3
        ssf_krn_opinv            = 4
        ssf_krn_input_data_error = 5
        ssf_krn_invalid_par      = 6
        ssf_krn_invalid_parlen   = 7
        OTHERS                   = 8.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

*-- Xstring to binary
    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer     = lv_xstring
      TABLES
        binary_tab = lt_binary_content.


*-- Converting the Binary Content
    CALL FUNCTION 'SO_SOLIXTAB_TO_SOLITAB'
      EXPORTING
        ip_solixtab = lt_binary_content
      IMPORTING
        ep_solitab  = lt_soli_tab.

    CALL FUNCTION 'SO_CONVERT_CONTENTS_BIN'
      EXPORTING
        it_contents_bin = lt_soli_tab[]
      IMPORTING
        et_contents_bin = lt_soli_tab[].

    lv_insplot  = im_s_imgupload-prueflos.
    lv_oper     = im_s_quachar-inspoper.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_oper
      IMPORTING
        output = lv_oper.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_insplot
      IMPORTING
        output = lv_insplot.

    SELECT SINGLE a~plnkn
      FROM afvc AS a
      INNER JOIN qals AS b
      ON a~aufpl = b~aufpl
      INTO @DATA(lv_plnkn)
      WHERE b~prueflos = @lv_insplot
        AND a~vornr    = @lv_oper.

    lv_userc1 = im_s_insppoint-userc1.
    lv_userc2 = im_s_insppoint-userc2.
    TRANSLATE: lv_userc1 TO UPPER CASE,
               lv_userc2 TO UPPER CASE.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_userc1
      IMPORTING
        output = lv_userc1.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_userc2
      IMPORTING
        output = lv_userc2.

    lv_usern  = im_s_insppoint-usern1.
    lv_usern2 = im_s_insppoint-usern2.

    SELECT SINGLE probenr
      FROM qapp
      INTO @lv_probenr
      WHERE prueflos = @lv_insplot
        AND vorglfnr = @lv_plnkn
        AND userc1   = @lv_userc1
        AND userc2   = @lv_userc2
        AND usern1   = @lv_usern
        AND usern2   = @lv_usern2
        AND userd1   = @im_s_insppoint-userd1
        AND usert1   = @im_s_insppoint-usert1.


    ls_object-objkey  = lv_insplot && lv_oper && lv_probenr.  " Inspection Point - According to table SWOTLV: The Obj. BUS204503 must be composed of these 3 fields
    ls_object-objtype = 'BUS204503'.                          " Object Type - Inspection Point level

    CALL FUNCTION 'SO_FOLDER_ROOT_ID_GET'
      EXPORTING
        region    = 'B'
      IMPORTING
        folder_id = ls_fol_id
      EXCEPTIONS
        OTHERS    = 1.

    ls_obj_data-objsns = 'O'.
    ls_obj_data-objla  = sy-langu.
    ls_obj_data-objdes = im_s_imgupload-zfilename.

    ls_obj_data-file_ext = 'JPG'.
    ls_obj_data-objlen = lines( lt_soli_tab ) * 255.

*-- Getting the data and saving in the folder
    CALL FUNCTION 'SO_OBJECT_INSERT'
      EXPORTING
        folder_id             = ls_fol_id
        object_type           = 'BIN'   " 'EXT'
        object_hd_change      = ls_obj_data
      IMPORTING
        object_id             = ls_obj_id
      TABLES
        objhead               = lt_objhead
        objcont               = lt_soli_tab
      EXCEPTIONS
        active_user_not_exist = 35
        folder_not_exist      = 6
        object_type_not_exist = 17
        owner_not_exist       = 22
        parameter_error       = 23
        OTHERS                = 1000.

    IF sy-subrc = 0 AND ls_object-objkey IS NOT INITIAL.

*-- This is standard procedure. Confirmed it by looking at standard implementation of the same.
      ls_folmem_k-foltp = ls_fol_id-objtp.
      ls_folmem_k-folyr = ls_fol_id-objyr.
      ls_folmem_k-folno = ls_fol_id-objno.
      ls_folmem_k-doctp = ls_obj_id-objtp.
      ls_folmem_k-docyr = ls_obj_id-objyr.
      ls_folmem_k-docno = ls_obj_id-objno.

      lv_ep_note = ls_folmem_k.

      ls_note-objtype = 'MESSAGE'.
      ls_note-objkey = lv_ep_note.

      CALL FUNCTION 'BINARY_RELATION_CREATE_COMMIT'
        EXPORTING
          obj_rolea    = ls_object
          obj_roleb    = ls_note
          relationtype = 'ATTA'
        EXCEPTIONS
          OTHERS       = 1.

      IF sy-subrc = 0.
        <fs_messages>-msgid = 'ZABS_MSGCLS'.
        <fs_messages>-msgno = '000'.
        <fs_messages>-msgty = 'S'.
        <fs_messages>-msgv1 = TEXT-000.
      ELSE.
        <fs_messages>-msgid = 'ZABS_MSGCLS'.
        <fs_messages>-msgno = '000'.
        <fs_messages>-msgty = 'W'.
        <fs_messages>-msgv1 = TEXT-001.
      ENDIF.
    ELSE.
      <fs_messages>-msgid = 'ZABS_MSGCLS'.
      <fs_messages>-msgno = '000'.
      <fs_messages>-msgty = 'W'.
      <fs_messages>-msgv1 = TEXT-001.
    ENDIF.

  ENDMETHOD.


  METHOD worouteterrainex_get_entityset.

*-- Variables
    DATA: lv_totrec     TYPE i,
          lv_entity_rec TYPE i,
          lv_srtfd      TYPE indx_srtfd.

**--Constants
    CONSTANTS lc_rule(7) TYPE c VALUE 'ZZWOROT'.

*-- Internal Table's
    DATA : lt_filter TYPE /iwbep/t_mgw_select_option.

*-- Get Filters defined
    lt_filter = io_tech_request_context->get_filter( )->get_filter_select_options( ).

    CONCATENATE lc_rule sy-uname INTO lv_srtfd.

    IF io_tech_request_context->get_skiptoken( ) IS INITIAL.
      CALL METHOD me->delete_from_database
        EXPORTING
          tabname          = 'INDX'
          client           = sy-mandt
          area             = 'ID'
          id               = lv_srtfd
          client_specified = abap_false.
    ELSE.
      IMPORT: lt_dummy TO et_entityset FROM DATABASE indx(id) ID lv_srtfd.
    ENDIF.

    IF et_entityset IS INITIAL." sy-subrc <> 0.

      TRY.
          get_route_terrain_dtls(
      EXPORTING
        io_tech_request_context = io_tech_request_context
      IMPORTING
        et_entityset            = et_entityset ).
          ##NO_HANDLER
        CATCH /iwbep/cx_mgw_busi_exception .
          ##NO_HANDLER
        CATCH /iwbep/cx_mgw_tech_exception .
      ENDTRY.

      IF et_entityset IS NOT INITIAL.
        EXPORT lt_dummy FROM et_entityset TO DATABASE indx(id) ID lv_srtfd.
      ENDIF.

    ENDIF.

    DESCRIBE TABLE et_entityset LINES lv_entity_rec.

*** Inlinecount
    IF io_tech_request_context->has_inlinecount( ) = abap_true.
      DESCRIBE TABLE et_entityset LINES es_response_context-inlinecount.
    ELSE.
      CLEAR es_response_context-inlinecount.
    ENDIF.

    CALL METHOD me->get_skip_token
      EXPORTING
        io_tech_request_context = io_tech_request_context
      IMPORTING
        es_response_context     = es_response_context
        ev_totrecords           = lv_totrec
      CHANGING
        ct_entityset            = et_entityset.

    IF et_entityset IS INITIAL
      OR lv_entity_rec <= lv_totrec.

      CLEAR: es_response_context-skiptoken,
             es_response_context-inlinecount.

      CALL METHOD me->delete_from_database
        EXPORTING
          tabname          = 'INDX'
          client           = sy-mandt
          area             = 'ID'
          id               = lv_srtfd
          client_specified = abap_false.

    ENDIF.

  ENDMETHOD.


  METHOD woroute_extset_get_entityset.

    DATA:
      lt_defrtt    TYPE TABLE OF /agri/glrthdrt, "/agri/gldefrtt,
      ls_defrtt    TYPE /agri/glrthdrt, "/agri/gldefrtt,
      ls_entityset LIKE LINE OF et_entityset,
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
         FOR ALL ENTRIES IN @lt_rtusr              "#EC CI_NO_TRANSFORM
       WHERE route EQ @lt_rtusr-route
         AND spras EQ @sy-langu.
    ENDIF.


    LOOP AT lt_rtusr INTO ls_rtusr.
      MOVE-CORRESPONDING ls_rtusr TO ls_entityset.
      READ TABLE lt_defrtt INTO ls_defrtt WITH KEY route = ls_rtusr-route
                                                   spras = sy-langu.
      IF sy-subrc EQ 0.
        ls_entityset-descr = ls_defrtt-descr.
      ENDIF.
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

  ENDMETHOD.
ENDCLASS.
