class ZCL_ZABS_MOB_MCN_CNF_DPC_EXT definition
  public
  inheriting from ZCL_ZABS_MOB_MCN_CNF_DPC
  create public .

public section.

  types:
    BEGIN OF ty_accom_oper,
        mackey       TYPE zabs_del_mackey,
        posnr        TYPE /agri/glposnr,
        operation_no TYPE i,
      END OF ty_accom_oper .
  types:
    BEGIN OF ty_mackey,
        mackey TYPE zabs_del_mackey,
      END OF ty_mackey .
  types:
*--Types declaration
    BEGIN OF ty_tplnr_mat,
             tplnr_fl TYPE /agri/gltplnr_fl,
             contr    TYPE /agri/gcontr,
             matnr    TYPE /agri/gltmatnr,
             iwerk    TYPE iwerk,
           END OF ty_tplnr_mat .

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_BEGIN
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_END
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_PROCESS
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITYSET_DELTA
    redefinition .
protected section.

  methods MCNACCITEMSSET_GET_ENTITYSET
    redefinition .
  methods MCNACTVITYSET_GET_ENTITYSET
    redefinition .
  methods MCNCONFIRMATIONS_GET_ENTITYSET
    redefinition .
  methods MCNCONFIRMATIONS_UPDATE_ENTITY
    redefinition .
  methods MCNEMPLOYEESET_GET_ENTITYSET
    redefinition .
  methods MCNFLEETSET_GET_ENTITYSET
    redefinition .
  methods MCNGISTOKENSET_GET_ENTITYSET
    redefinition .
  methods MCNNPEMPLOYEESET_GET_ENTITYSET
    redefinition .
  methods MCNNPFLEETSET_GET_ENTITYSET
    redefinition .
  methods MCNTASKORDSET_GET_ENTITYSET
    redefinition .
  methods MCNUSERSET_GET_ENTITYSET
    redefinition .
  methods MCNCONFIRMATIONS_DELETE_ENTITY
    redefinition .
private section.

  methods GET_USER_ROLE
    exporting
      !ET_USER_ROLE type ZCL_ZABS_MOB_MCN_CNF_MPC=>TT_MCNUSER
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
  methods GET_EMPLOYEES
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_EMPLOYEE type ZCL_ZABS_MOB_MCN_CNF_MPC=>TT_MCNEMPLOYEE
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods GET_ACTIVITIES
    exporting
      !ET_ACTIVITIES type ZCL_ZABS_MOB_MCN_CNF_MPC=>TT_MCNACTVITY
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods GET_TASK_ORDS
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_TASK_ORD type ZCL_ZABS_MOB_MCN_CNF_MPC=>TT_MCNTASKORD
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods POPULATE_ACCOMPLISHMENT_DATA
    importing
      !IS_ACHDR type /AGRI/S_FMACHDR
      !IT_ACITM type /AGRI/T_FMACITM
    exporting
      !ES_ACDOC type /AGRI/S_FMACS_DOC .
  methods CREATE_ACCOMPLISHMENT
    importing
      !IS_FMAC_DOC type /AGRI/S_FMACS_DOC
    exporting
      !ES_FMAC_DOC type /AGRI/S_FMACS_DOC
      !ET_MESSAGES type /AGRI/T_GPROLOG .
  methods GET_ACCITEMS
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_ACCITEMS type ZCL_ZABS_MOB_MCN_CNF_MPC=>TT_MCNACCITEMS .
  methods GET_FLEET
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_FLEET type ZABS_TTY_MCN_FLEET .
  methods GET_DELTA_TOKEN
    importing
      !IV_ENTITY_SET_NAME type STRING
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
    exporting
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
    changing
      !CT_ENTITYSET type STANDARD TABLE .
  methods GET_EMPLOYEES_NP
    exporting
      !ET_EMPLOYEE_NP type ZCL_ZABS_MOB_MCN_CNF_MPC=>TT_MCNNPEMPLOYEE .
  methods GET_FLEET_NP
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_FLEET_NP type ZCL_ZABS_MOB_MCN_CNF_MPC=>TT_MCNNPFLEET .
ENDCLASS.



CLASS ZCL_ZABS_MOB_MCN_CNF_DPC_EXT IMPLEMENTATION.


METHOD /iwbep/if_mgw_appl_srv_runtime~changeset_begin.

  CLEAR: cv_defer_mode.
  cv_defer_mode = abap_true.

*-- Looping Entityset name in the Operation Table
  LOOP AT it_operation_info ASSIGNING FIELD-SYMBOL(<fs_operation_info>).
    IF NOT ( <fs_operation_info>-entity_set EQ 'McnConfirmationsSet' ).
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
    ls_acc_items           TYPE zcl_zabs_mob_mcn_cnf_mpc=>ts_mcnconfirmations,
    lt_acc_items           TYPE zcl_zabs_mob_mcn_cnf_mpc=>tt_mcnconfirmations,
    ls_achdr               TYPE /agri/s_fmachdr,
    lt_achdr               TYPE /agri/t_fmachdr,
    ls_acitm               TYPE /agri/s_fmacitm,
    lt_acitm               TYPE /agri/t_fmfmacitm,
    lt_acitm_tmp           TYPE /agri/t_fmfmacitm,
    lt_acitm_posnr         TYPE /agri/t_fmfmacitm,
    ls_fmac_doc            TYPE /agri/s_fmacs_doc,
    ls_message             TYPE /agri/s_gprolog,
    lt_messages            TYPE /agri/t_gprolog,
    lt_bapi_messages       TYPE bapiret2_tt,
    lv_operation_no        TYPE i,
    lwa_changeset_response TYPE /iwbep/if_mgw_appl_types=>ty_s_changeset_response,
    lt_accom               TYPE /agri/t_fmacom,
    ls_accom               TYPE /agri/s_fmacom,
    lt_acdoc               TYPE /agri/t_fmacs_doc,
    lt_accom_oper          TYPE STANDARD TABLE OF ty_accom_oper,
    ls_accom_oper          TYPE ty_accom_oper,
    lt_mackey              TYPE STANDARD TABLE OF ty_mackey,
    ls_mackey              TYPE ty_mackey,
    lv_accom               TYPE /agri/fmaccom,
    lv_create              TYPE xfeld,
    ls_mcn_np              TYPE zabs_mcn_np,
    lt_mcn_np              TYPE STANDARD TABLE OF zabs_mcn_np.

  LOOP AT it_changeset_request ASSIGNING FIELD-SYMBOL(<fs_changeset_request>).
    CLEAR lv_entity_type.
    lo_update_context ?= <fs_changeset_request>-request_context.
    lv_entity_type = lo_update_context->get_entity_type_name( ).

    IF lv_entity_type NE 'McnConfirmations'.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
        EXPORTING
          textid      = /iwbep/cx_mgw_tech_exception=>operation_not_supported
          operation   = 'UPDATE_ENTITY'
          entity_type = lv_entity_type.
    ENDIF.

    CASE lv_entity_type.

      WHEN 'McnConfirmations'.
        <fs_changeset_request>-entry_provider->read_entry_data( "#EC CI_FLDEXT_OK
                           IMPORTING es_data = ls_acc_items ).
        IF ls_acc_items-zznonpro IS INITIAL.
*-- Collect Accomplishment Keys
          CLEAR ls_mackey.
            ls_mackey-mackey = ls_acc_items-zzmackey.
            APPEND ls_mackey TO lt_mackey.

*-- Collect the Accomplishment headers
          CLEAR ls_achdr.
          MOVE-CORRESPONDING ls_acc_items TO ls_achdr.
          CLEAR ls_achdr-updkz.
          APPEND ls_achdr TO lt_achdr.

*-- Collect Accomplishment items
*        CLEAR ls_acitm.
*        SELECT accom
*         FROM /agri/fmacitm
*          INTO TABLE @DATA(lt_acitm)
*           WHERE accom    EQ @ls_acc_items-accom
*           AND strtdat  EQ @ls_acc_items-strtdat "'DEL'
*           AND strttim  EQ @ls_acc_items-strttim
*           AND zzmackey EQ @ls_acc_items-zzmackey.
*          IF sy-subrc eq 0.
*
*          ENDIF.

          MOVE-CORRESPONDING ls_acc_items TO ls_acitm.
          ls_acitm-zztplnr = ls_acc_items-tplnr.
          ls_acitm-zzcdate = sy-datum.
          ls_acitm-zzctime = sy-uzeit.
          ls_acitm-zzcrimei  = ls_acc_items-imei.
          ls_acitm-zzcrbadge = ls_acc_items-badge.
          ls_acitm-zzfposnr  = ls_acc_items-posnr.
          APPEND ls_acitm TO lt_acitm.

        ELSEIF ls_acc_items-zznonpro IS NOT INITIAL.
          MOVE-CORRESPONDING ls_acc_items TO ls_mcn_np.
          APPEND ls_mcn_np TO lt_mcn_np.
          CLEAR ls_mcn_np.
        ENDIF.

        lwa_changeset_response-operation_no = <fs_changeset_request>-operation_no.
        copy_data_to_ref( EXPORTING is_data = ls_acc_items
             CHANGING cr_data = lwa_changeset_response-entity_data ).
        INSERT lwa_changeset_response INTO TABLE ct_changeset_response.

    ENDCASE.
  ENDLOOP.

  IF lt_mackey IS NOT INITIAL.
    SORT lt_mackey.
    DELETE ADJACENT DUPLICATES FROM lt_mackey COMPARING ALL FIELDS.

*-- Fetch the Accomplishment Numbers for the keys
    SELECT accom, zzmackey
      FROM /agri/fmachdr
      INTO TABLE @DATA(lt_accom_key)
       FOR ALL ENTRIES IN @lt_mackey
     WHERE status   NE @zcl_abs_abap_maintain=>c_ac_status_deleted "'DEL'
       AND zzmackey EQ @lt_mackey-mackey.
    IF sy-subrc EQ 0.
      lt_accom = lt_accom_key.
      SORT lt_accom_key BY zzmackey.
    ENDIF.

    IF lt_accom IS NOT INITIAL.
*-- Fetch the Accomplishment data
      CALL FUNCTION '/AGRI/FMAC_VIEW'
        EXPORTING
          it_accom       = lt_accom
        IMPORTING
          et_acdoc       = lt_acdoc
        EXCEPTIONS
          no_data_exists = 1
          OTHERS         = 2.
    ENDIF.

*    LOOP AT it_changeset_request ASSIGNING <fs_changeset_request>.
*      CLEAR lv_entity_type.
*      lo_update_context ?= <fs_changeset_request>-request_context.
*      lv_entity_type = lo_update_context->get_entity_type_name( ).
*
*      CASE lv_entity_type.
*        WHEN 'McnConfirmations'.
*          <fs_changeset_request>-entry_provider->read_entry_data( "#EC CI_FLDEXT_OK
*                             IMPORTING es_data = ls_acc_items ).

**-- Populate the accomplishment number
*          READ TABLE lt_accom_key INTO DATA(ls_accom_key)
*                WITH KEY zzmackey = ls_acc_items-zzmackey
*              BINARY SEARCH.
*          IF sy-subrc EQ 0.
*            ls_acc_items-accom = ls_accom_key-accom.
*          ENDIF.

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
*          APPEND ls_acitm TO lt_acitm.

**-- Collect the items coming from front end
*          APPEND ls_acc_items TO lt_acc_items.

**-- Collect the operations
*          CLEAR ls_accom_oper.
*          ls_accom_oper-mackey = ls_acitm-zzmackey.
*          ls_accom_oper-posnr  = ls_acitm-posnr.
*          ls_accom_oper-operation_no = <fs_changeset_request>-operation_no.
*          APPEND ls_accom_oper TO lt_accom_oper.

*      ENDCASE.
*    ENDLOOP.

    SORT lt_achdr BY zzmackey.
    DELETE ADJACENT DUPLICATES FROM lt_achdr COMPARING zzmackey.

    SORT lt_acitm BY zzmackey posnr.

*    SORT lt_acc_items BY zzmackey posnr.
*    SORT lt_accom_oper BY mackey posnr.

*-- For each accomplishment header
    LOOP AT lt_achdr INTO ls_achdr.

      REFRESH lt_acitm_tmp.
      CLEAR lv_create.

      READ TABLE lt_accom_key INTO DATA(ls_accom_key)
            WITH KEY zzmackey = ls_achdr-zzmackey
          BINARY SEARCH.
      IF sy-subrc EQ 0.                  "If Accomplish already created

        READ TABLE lt_acdoc ASSIGNING FIELD-SYMBOL(<fs_acdoc>)
              WITH KEY accom = ls_accom_key-accom
            BINARY SEARCH.
        IF sy-subrc EQ 0.

          REFRESH lt_acitm_posnr.
          lt_acitm_posnr = <fs_acdoc>-x-acitm.
          SORT lt_acitm_posnr BY accom posnr DESCENDING.
          READ TABLE lt_acitm_posnr INTO DATA(ls_acitm_posnr) INDEX 1.
          IF sy-subrc EQ 0.
            DATA(lv_posnr) = ls_acitm_posnr-posnr.
          ENDIF.

          SORT lt_acitm_posnr BY zzmackey zzmcnim aufnr equnr
                                 idresource strtdat strttim.

          READ TABLE lt_acitm TRANSPORTING NO FIELDS
                WITH KEY zzmackey = ls_achdr-zzmackey
              BINARY SEARCH.
          IF sy-subrc EQ 0.
            DATA(lv_tabix) = sy-tabix.
            LOOP AT lt_acitm INTO ls_acitm FROM lv_tabix.
              IF ls_acitm-zzmackey NE ls_achdr-zzmackey.
                EXIT.
              ENDIF.

*-- Checking the Accomplishment already exist with the combination of
*-- Accomplishment number, start date, end date and Mackey
              READ TABLE lt_acitm_posnr TRANSPORTING NO FIELDS
                           WITH KEY zzmackey   = ls_acitm-zzmackey
                                    zzmcnim    = ls_acitm-zzmcnim
                                    aufnr      = ls_acitm-aufnr
                                    equnr      = ls_acitm-equnr
                                    idresource = ls_acitm-idresource
                                    strtdat    = ls_acitm-strtdat
                                    strttim    = ls_acitm-strttim
                            BINARY SEARCH.
              IF sy-subrc EQ 0.
                CONTINUE.
              ENDIF.

**              CASE ls_acitm-updkz.
**
**                WHEN zcl_abs_abap_maintain=>c_updkz_insert.
*
*              READ TABLE <fs_acdoc>-x-acitm TRANSPORTING NO FIELDS
*                                WITH KEY accom = ls_acitm-accom
*                                         posnr = ls_acitm-posnr.
*              IF sy-subrc NE 0.
**-- Add new item
**                IF ls_acitm-zzbill = 'YES'.
*                ls_acitm-status = zcl_abs_abap_maintain=>c_ac_status_created.
*                <fs_acdoc>-x-achdr-status = zcl_abs_abap_maintain=>c_ac_status_created.
**                ELSE.
**                  ls_acitm-status = zcl_abs_abap_maintain=>c_ac_status_confirm.
**                ENDIF.
*                ls_acitm-updkz = zcl_abs_abap_maintain=>c_updkz_insert.
*                APPEND ls_acitm TO <fs_acdoc>-x-acitm.
*
**              ENDCASE.
*
*                APPEND ls_acitm TO lt_acitm_tmp.
*              ENDIF.

              lv_posnr = lv_posnr + 1.
              ls_acitm-accom = <fs_acdoc>-accom.
              ls_acitm-posnr = lv_posnr.
              ls_acitm-status = zcl_abs_abap_maintain=>c_ac_status_created.
              <fs_acdoc>-x-achdr-status = zcl_abs_abap_maintain=>c_ac_status_created.
              ls_acitm-updkz = zcl_abs_abap_maintain=>c_updkz_insert.
              APPEND ls_acitm TO <fs_acdoc>-x-acitm.
              lv_create = abap_true.
            ENDLOOP.
          ENDIF.

          DATA(ls_acdoc) = <fs_acdoc>.
          CLEAR lv_posnr.
        ENDIF.

      ELSE.                                    "No accomplishment created

        ls_achdr-findat = ls_achdr-strtdat + 1.
        CLEAR lv_posnr.
        READ TABLE lt_acitm TRANSPORTING NO FIELDS
              WITH KEY zzmackey = ls_achdr-zzmackey
            BINARY SEARCH.
        IF sy-subrc EQ 0.
          lv_tabix = sy-tabix.
          LOOP AT lt_acitm INTO ls_acitm FROM lv_tabix.
            IF ls_acitm-zzmackey NE ls_achdr-zzmackey.
              EXIT.
            ENDIF.
            lv_posnr = lv_posnr + 1.
            ls_acitm-posnr = lv_posnr.
            APPEND ls_acitm TO lt_acitm_tmp.
          ENDLOOP.
        ENDIF.

        IF lt_acitm_tmp IS NOT INITIAL.
*-- Populate Accomplishment data
          CALL METHOD me->populate_accomplishment_data
            EXPORTING
              is_achdr = ls_achdr
              it_acitm = lt_acitm_tmp
            IMPORTING
              es_acdoc = ls_acdoc.
          lv_create = abap_true.
        ENDIF.
      ENDIF.

*      IF lv_create IS INITIAL.
***-- Give error message
*        CLEAR ls_message.
*        ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error. "'E'
*        ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
*        ls_message-msgno = '188'.
*        ls_message-msgv1 = ls_accom_key-accom.
*        add_messages_to_msg_container( is_message = ls_message ).
*        CONTINUE.
*      ENDIF.

      IF ls_acdoc IS NOT INITIAL AND
         lv_create EQ abap_true.
        REFRESH lt_messages.
        CLEAR ls_fmac_doc.

*-- Create/Update the Accomplishment
        CALL METHOD me->create_accomplishment
          EXPORTING
            is_fmac_doc = ls_acdoc
          IMPORTING
            es_fmac_doc = ls_fmac_doc
            et_messages = lt_messages.

**-- Populate Accomplishment Sheet number and Operation number
*        CLEAR lv_accom.
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*          EXPORTING
*            input  = ls_fmac_doc-accom
*          IMPORTING
*            output = lv_accom.

*        LOOP AT lt_acitm_tmp INTO ls_acitm.
*          CLEAR ls_accom_oper.
*          READ TABLE lt_accom_oper INTO ls_accom_oper
*                WITH KEY mackey = ls_acitm-zzmackey
*                         posnr  = ls_acitm-posnr
*              BINARY SEARCH.
*          IF sy-subrc EQ 0.
**-- Populate the operation number
*            lwa_changeset_response-operation_no = ls_accom_oper-operation_no.
*          ENDIF.
*
*          READ TABLE lt_acc_items ASSIGNING FIELD-SYMBOL(<fs_acc_items>)
*                WITH KEY zzmackey = ls_acitm-zzmackey
*                         posnr    = ls_acitm-posnr
*              BINARY SEARCH.
*          IF sy-subrc EQ 0.
*            <fs_acc_items>-accom = lv_accom.
*          ENDIF.
*
*          copy_data_to_ref( EXPORTING is_data = <fs_acc_items>
*               CHANGING cr_data = lwa_changeset_response-entity_data ).
*          INSERT lwa_changeset_response INTO TABLE ct_changeset_response.
*
*          CLEAR ls_acdoc.
*        ENDLOOP.

*-- Add the messages to containersa
        add_messages_to_msg_container( iv_exception = abap_true
                                       it_messages = lt_messages ).

      ENDIF.
      CLEAR ls_accom_key.
    ENDLOOP.
  ENDIF.

  "------------Start Wave 3
  IF lt_mcn_np[] IS NOT INITIAL.

    SELECT *
      FROM zabs_mcn_np
INTO TABLE @DATA(lt_zmcn_np)
   FOR ALL ENTRIES IN @lt_mcn_np
     WHERE zzmackey EQ @lt_mcn_np-zzmackey
       AND zznpitm  EQ @lt_mcn_np-zznpitm
       AND actcomp  EQ @space.
    IF sy-subrc = 0.
      SORT lt_zmcn_np BY zzmackey zznpitm.
    ENDIF.
    LOOP AT lt_mcn_np INTO DATA(ls_mnc_np).
      READ TABLE lt_zmcn_np INTO DATA(ls_zmnc_np)
                            WITH KEY zzmackey = ls_mnc_np-zzmackey
                                     zznpitm = ls_mnc_np-zznpitm
                          BINARY SEARCH.
      IF sy-subrc = 0.
        ls_mnc_np-ernam = ls_zmnc_np-ernam.
        ls_mnc_np-erdat = ls_zmnc_np-erdat.
        ls_mnc_np-erzet = ls_zmnc_np-erzet.
        ls_mnc_np-aenam = sy-uname.
        ls_mnc_np-aedat = sy-datum.
        ls_mnc_np-aezet = sy-uzeit.
*        MODIFY zabs_mcn_np FROM ls_mnc_np.
        UPDATE zabs_mcn_np FROM ls_mnc_np.
**        IF sy-subrc = 0.
**          CLEAR : ls_message.
**          ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_success.
**          ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
**          ls_message-msgno = '125'.
**          APPEND ls_message TO lt_messages.
**          CLEAR : ls_message.
**        ENDIF.
      ELSE.
        ls_mnc_np-ernam = sy-uname.
        ls_mnc_np-erdat = sy-datum.
        ls_mnc_np-erzet = sy-uzeit.
        INSERT zabs_mcn_np FROM ls_mnc_np.
**        IF sy-subrc = 0.
**          CLEAR : ls_message.
**          ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_success.
**          ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
**          ls_message-msgno = '125'.
**          APPEND ls_message TO lt_messages.
**          CLEAR : ls_message.
**        ENDIF.
      ENDIF.
      CLEAR: ls_mnc_np, ls_zmnc_np.
    ENDLOOP.
    REFRESH lt_mcn_np.

**    SORT lt_messages BY msgno.
**    DELETE ADJACENT DUPLICATES FROM lt_messages.
**    add_messages_to_msg_container( iv_exception = abap_true
**                                   it_messages = lt_messages ).

    ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_success.
    ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
    ls_message-msgno = '125'.
    add_messages_to_msg_container( is_message = ls_message ).

  ENDIF.
  "------------End Wave 3

ENDMETHOD.


METHOD /iwbep/if_mgw_appl_srv_runtime~get_entityset_delta.

*-- Local Declarations
  DATA: lv_entity_set_name TYPE string,
        lv_delta_token     TYPE string,
        lo_dp_facade       TYPE REF TO /iwbep/if_mgw_dp_facade,
        lt_mcnemployee     TYPE zcl_zabs_mob_mcn_cnf_mpc=>tt_mcnemployee,
        lt_mcnemployee_del TYPE zcl_zabs_mob_mcn_cnf_mpc=>tt_mcnemployee,
        lt_mcnuser         TYPE zcl_zabs_mob_mcn_cnf_mpc=>tt_mcnuser,
        lt_mcnuser_del     TYPE zcl_zabs_mob_mcn_cnf_mpc=>tt_mcnuser,
        lt_mcnfleet        TYPE zcl_zabs_mob_mcn_cnf_mpc=>tt_mcnfleet,
        lt_mcnfleet_del    TYPE zcl_zabs_mob_mcn_cnf_mpc=>tt_mcnfleet,
        lt_mcnactivity     TYPE zcl_zabs_mob_mcn_cnf_mpc=>tt_mcnactvity,
        lt_mcnactivity_del TYPE zcl_zabs_mob_mcn_cnf_mpc=>tt_mcnactvity,
        lt_mcngistoken     TYPE zcl_zabs_mob_mcn_cnf_mpc=>tt_mcngistoken,
        lt_mcngistoken_del TYPE zcl_zabs_mob_mcn_cnf_mpc=>tt_mcngistoken,
        lt_mcntaskord      TYPE zcl_zabs_mob_mcn_cnf_mpc=>tt_mcntaskord,
        lt_mcntaskord_del  TYPE zcl_zabs_mob_mcn_cnf_mpc=>tt_mcntaskord,
        lt_mcnaccitems     TYPE zcl_zabs_mob_mcn_cnf_mpc=>tt_mcnaccitems,
        lt_mcnaccitems_del TYPE zcl_zabs_mob_mcn_cnf_mpc=>tt_mcnaccitems.

*-- Getting the EntitySet name
  lv_entity_set_name = io_tech_request_context->get_entity_set_name( ).

  CASE lv_entity_set_name.

    WHEN 'McnEmployeeSet'.

*-- Fetch Employees
      CALL METHOD me->get_employees
        EXPORTING
          io_tech_request_context = io_tech_request_context
        IMPORTING
          et_employee             = lt_mcnemployee.

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
              it_entityset             = lt_mcnemployee
            IMPORTING
              et_deleted_entityset     = lt_mcnemployee_del
              et_entityset             = lt_mcnemployee
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
                         is_data = lt_mcnemployee_del
                        CHANGING
                         cr_data = er_deleted_entityset ).

*-- Export the changed entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_mcnemployee
                        CHANGING
                         cr_data = er_entityset ).

    WHEN 'McnUserSet'.

*-- Get login user role
      CALL METHOD me->get_user_role
        IMPORTING
          et_user_role = lt_mcnuser.

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
              it_entityset             = lt_mcnuser
            IMPORTING
              et_deleted_entityset     = lt_mcnuser_del
              et_entityset             = lt_mcnuser
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
                         is_data = lt_mcnuser_del
                        CHANGING
                         cr_data = er_deleted_entityset ).

*-- Export the changed entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_mcnuser
                        CHANGING
                         cr_data = er_entityset ).

    WHEN 'McnFleetSet'.

*-- Fetch Fleet
      CALL METHOD me->get_fleet
        EXPORTING
          io_tech_request_context = io_tech_request_context
        IMPORTING
          et_fleet                = lt_mcnfleet.

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
              it_entityset             = lt_mcnfleet
            IMPORTING
              et_deleted_entityset     = lt_mcnfleet_del
              et_entityset             = lt_mcnfleet
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
                         is_data = lt_mcnfleet_del
                        CHANGING
                         cr_data = er_deleted_entityset ).

*-- Export the changed entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_mcnfleet
                        CHANGING
                         cr_data = er_entityset ).

    WHEN 'McnActivitySet'.

*-- Fetch Activities
      CALL METHOD me->get_activities
        IMPORTING
          et_activities = lt_mcnactivity.

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
              it_entityset             = lt_mcnactivity
            IMPORTING
              et_deleted_entityset     = lt_mcnactivity_del
              et_entityset             = lt_mcnactivity
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
                         is_data = lt_mcnactivity_del
                        CHANGING
                         cr_data = er_deleted_entityset ).

*-- Export the changed entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_mcnactivity
                        CHANGING
                         cr_data = er_entityset ).

    WHEN 'McnGisTokenSet'.

*-- Fetch GIS Tokendata
      SELECT *
        FROM zabst_gistoken
        INTO TABLE @DATA(lt_gistoken)
       WHERE sys EQ @zcl_abs_abap_maintain=>c_gis_system. "'GIS'
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING lt_gistoken TO lt_mcngistoken.
      ENDIF.

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
              it_entityset             = lt_mcngistoken
            IMPORTING
              et_deleted_entityset     = lt_mcngistoken_del
              et_entityset             = lt_mcngistoken
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
                         is_data = lt_mcngistoken_del
                        CHANGING
                         cr_data = er_deleted_entityset ).

*-- Export the changed entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_mcngistoken
                        CHANGING
                         cr_data = er_entityset ).

    WHEN 'McnTaskOrdSet'.

*-- Prepare the task orders
      CALL METHOD me->get_task_ords
        EXPORTING
          io_tech_request_context = io_tech_request_context
        IMPORTING
          et_task_ord             = lt_mcntaskord.

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
              it_entityset             = lt_mcntaskord
            IMPORTING
              et_deleted_entityset     = lt_mcntaskord_del
              et_entityset             = lt_mcntaskord
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
                         is_data = lt_mcntaskord_del
                        CHANGING
                         cr_data = er_deleted_entityset ).

*-- Export the changed entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_mcntaskord
                        CHANGING
                         cr_data = er_entityset ).

    WHEN 'McnAccItemsSet'.

      CALL METHOD me->get_accitems
        EXPORTING
          io_tech_request_context = io_tech_request_context
        IMPORTING
          et_accitems             = lt_mcnaccitems.

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
              it_entityset             = lt_mcnaccitems
            IMPORTING
              et_deleted_entityset     = lt_mcnaccitems_del
              et_entityset             = lt_mcnaccitems
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
                         is_data = lt_mcnaccitems_del
                        CHANGING
                         cr_data = er_deleted_entityset ).

*-- Export the changed entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_mcnaccitems
                        CHANGING
                         cr_data = er_entityset ).

  ENDCASE.

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


METHOD create_accomplishment.

  DATA: ls_fmac_doc TYPE /agri/s_fmacs_doc,
        lt_messages TYPE /agri/t_gprolog.

*-- Create the new Accomplishment sheet
  CALL FUNCTION 'ZABS_FMAC_CREATE'
    EXPORTING
      is_fmacdoc  = is_fmac_doc
      iv_mcnf     = abap_true
    IMPORTING
      es_fmac_doc = es_fmac_doc
      et_messages = et_messages.

*  READ TABLE et_messages TRANSPORTING NO FIELDS
*        WITH KEY msgty = 'E'.
*  IF sy-subrc NE 0.
*    ls_fmac_doc = es_fmac_doc.
*    CLEAR es_fmac_doc.
*    CALL FUNCTION 'ZABS_FMAC_CONFIRMATION'
*      EXPORTING
*        is_fmacdoc  = ls_fmac_doc
*        iv_mcnf     = abap_true
*      IMPORTING
*        es_fmac_doc = es_fmac_doc
*        et_messages = lt_messages.
*
*    APPEND LINES OF lt_messages TO et_messages.
*  ENDIF.

ENDMETHOD.


METHOD get_accitems.

*--Local declarations
  DATA: lt_task_ord TYPE zcl_zabs_mob_mcn_cnf_mpc=>tt_mcntaskord,
        lv_days     TYPE t5a4a-dlydy,
        lv_date     TYPE sy-datum,
        lv_cnval1   TYPE zabs_del_cnval,
        ls_accitems TYPE zcl_zabs_mob_mcn_cnf_mpc=>ts_mcnaccitems,
        ls_message  TYPE /agri/s_gprolog.
*        lv_msgv1    LIKE sy-msgv1.


  CALL METHOD me->get_task_ords
    EXPORTING
      io_tech_request_context = io_tech_request_context
    IMPORTING
      et_task_ord             = lt_task_ord.

*  CALL METHOD me->get_task_ords
*    IMPORTING
*      et_task_ord = lt_task_ord.

  SORT lt_task_ord BY werks.
  DELETE ADJACENT DUPLICATES FROM lt_task_ord COMPARING werks.

*--Getting variant data
  CALL METHOD zcl_abs_get_variants=>get_constant_single
    EXPORTING
      iv_mod    = space
      iv_objid  = zcl_abs_abap_maintain=>c_objid_mch "'MCH'
      iv_k1val  = zcl_abs_abap_maintain=>c_key_mch_days "'DAYS'
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
      calc_date = lv_date.

  IF lt_task_ord IS NOT INITIAL.
*--Fetching accomplishment data
    SELECT a~accom, a~werks, a~datum, a~zzmackey, b~posnr
      FROM /agri/fmachdr AS a INNER JOIN /agri/fmacitm AS b
        ON b~accom EQ a~accom
      INTO TABLE @DATA(lt_ac_hdr_itm) "et_accitems
       FOR ALL ENTRIES IN @lt_task_ord
     WHERE a~actyp   EQ @zcl_abs_abap_maintain=>c_acctyp_tast "'TAST'
       AND a~werks   EQ @lt_task_ord-werks
       AND a~strtdat GE @lv_date
       AND a~strtdat LE @sy-datum
       AND a~status  NE @zcl_abs_abap_maintain=>c_ac_status_deleted. "'DEL'
    IF sy-subrc EQ 0.
      READ TABLE lt_task_ord INTO DATA(ls_task_ord) INDEX 1.
      IF sy-subrc EQ 0.
        LOOP AT lt_ac_hdr_itm INTO DATA(ls_ac_hdr_itm).

**--Document &1 is locked by User &2.
*          CLEAR lv_msgv1.
*          CALL FUNCTION 'ENQUEUE_/AGRI/EZ_FMAC'
*            EXPORTING
*              accom          = ls_ac_hdr_itm-accom
*            EXCEPTIONS
*              foreign_lock   = 1
*              system_failure = 2
*              OTHERS         = 3.
*          IF sy-subrc <> 0.
*            lv_msgv1 = sy-msgv1.
**            CLEAR ls_message.
*            ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error.
*            ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
*            ls_message-msgno = '185'.
*            ls_message-msgv1 = ls_ac_hdr_itm-accom.
*            ls_message-msgv2 = lv_msgv1.
*            add_messages_to_msg_container( is_message = ls_message ).
**          ELSE.
**            CALL FUNCTION 'DEQUEUE_/AGRI/EZ_FMAC'
**              EXPORTING
**                accom = ls_ac_hdr_itm-accom.
*          ENDIF.

          CLEAR ls_accitems.
          MOVE-CORRESPONDING ls_ac_hdr_itm TO ls_accitems.
          ls_accitems-fleet = ls_task_ord-fleet.
          ls_accitems-name1 = ls_task_ord-name1.
*          IF ls_message IS INITIAL.
          APPEND ls_accitems TO et_accitems.
*          ENDIF.
        ENDLOOP. "lt_ac_hdr_itm
      ENDIF. "lt_task_ord
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD get_activities.

*-- Local Declarations
  DATA: ls_message TYPE /agri/s_gprolog,
        lv_idact   TYPE /agri/fmidact,
        lv_cnval1  TYPE zabs_del_cnval.

  CALL METHOD zcl_abs_get_variants=>get_constant_single
    EXPORTING
      iv_mod    = space
      iv_objid  = zcl_abs_abap_maintain=>c_objid_mch "'MCH'
      iv_k1val  = zcl_abs_abap_maintain=>c_key_mch_act "'ACT'
    IMPORTING
      ev_cnval1 = lv_cnval1.  "'DESLCTO'

  lv_idact = lv_cnval1.

*-- Fetch the activities (productions and events) with descriptions
  SELECT idactv rstype
         bill actype zzactcg
    FROM /agri/fmacact
    INTO TABLE et_activities
   WHERE rstype  EQ zcl_abs_abap_maintain=>c_rstyp_equip "'B'
     AND ( zzactcg EQ zcl_abs_abap_maintain=>c_actcg_machinery "'M'
      OR zzactcg EQ zcl_abs_abap_maintain=>c_actcg_non_productive ). "'N'
  IF sy-subrc EQ 0.
    DELETE et_activities WHERE zzactcg IS INITIAL.
    DELETE et_activities WHERE idactve EQ lv_idact.
  ENDIF.

  IF et_activities IS INITIAL.
*-- Give error message if no activities found
    CLEAR ls_message.
    ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error.
    ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
    ls_message-msgno = '167'.
    add_messages_to_msg_container( is_message = ls_message ).
    RETURN.
  ENDIF.

*--Fetching Activity description
  SELECT idactv, description
    FROM /agri/fmacactt
    INTO TABLE @DATA(lt_fmactt)
     FOR ALL ENTRIES IN @et_activities
   WHERE idactv EQ @et_activities-idactve
     AND spras  EQ @sy-langu.
  IF sy-subrc EQ 0.
    SORT lt_fmactt BY idactv.
  ENDIF.

*--Processing activities data to fill description
  LOOP AT et_activities ASSIGNING FIELD-SYMBOL(<fs_activities>).
    READ TABLE lt_fmactt INTO DATA(ls_fmactt)
          WITH KEY idactv = <fs_activities>-idactve
        BINARY SEARCH.
    IF sy-subrc EQ 0.
      <fs_activities>-description = ls_fmactt-description.
    ENDIF.
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


METHOD get_employees.

**-- Local Declarations
*  DATA: lo_message_container TYPE REF TO /iwbep/if_message_container,
*        ls_message           TYPE /agri/s_gprolog.
*
*  CALL METHOD me->/iwbep/if_mgw_conv_srv_runtime~get_message_container
*    RECEIVING
*      ro_message_container = lo_message_container.

*-- Local Declarations
  DATA : lt_filter     TYPE /iwbep/t_mgw_select_option,
         ls_filter     TYPE /iwbep/s_mgw_select_option,
         ls_message    TYPE /agri/s_gprolog,
         lv_filter_str TYPE string,
         lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
         ltr_fleet     TYPE RANGE OF zabs_del_fleet,
         lsr_fleet     LIKE LINE OF ltr_fleet.

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
      WHEN 'FLEET'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = ltr_fleet ).

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

*  READ TABLE ltr_fleet INTO lsr_fleet INDEX 1.
*  IF sy-subrc = 0.
**--Fetching Resource data
*    SELECT SINGLE arbpl
*      FROM /agri/fmacres
*      INTO @DATA(lv_arbpl)
*     WHERE idresource EQ @lsr_fleet-low
*       AND rstype     EQ @zcl_abs_abap_maintain=>c_rstyp_equip. "'B'
*    IF sy-subrc NE 0.
*      CLEAR ls_message.
*      ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error. "'E'
*      ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class. "'ZABS_MSGCLS'
*      ls_message-msgno = '184'.
*      add_messages_to_msg_container( is_message = ls_message ).
*    ENDIF.
*  ENDIF.
*
*  IF lv_arbpl IS NOT INITIAL.
**-- Fetch the employees
*    SELECT idresource arbpl pernr
*      FROM /agri/fmacres
*      INTO TABLE et_employee
*     WHERE arbpl  EQ lv_arbpl
*       AND rstype EQ zcl_abs_abap_maintain=>c_rstyp_labour. "'A'
*    IF sy-subrc EQ 0.
*      LOOP AT et_employee ASSIGNING FIELD-SYMBOL(<fs_employee>).
*
**--Populating Fleet
*        READ TABLE ltr_fleet INTO lsr_fleet INDEX 1.
*        IF sy-subrc = 0.
*          <fs_employee>-fleet = lsr_fleet-low.
*        ENDIF.
*
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*          EXPORTING
*            input  = <fs_employee>-idresource
*          IMPORTING
*            output = <fs_employee>-idresource.
*      ENDLOOP.
*    ENDIF.
*  ENDIF. "lv_arbpl
*
*  IF et_employee IS INITIAL.
**-- Give error message if not mapping maintained
*    CLEAR ls_message.
*    ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error. "'E'
*    ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class. "'ZABS_MSGCLS'
*    ls_message-msgno = '162'.
*    add_messages_to_msg_container( is_message = ls_message ).
*  ENDIF.

  READ TABLE ltr_fleet INTO lsr_fleet INDEX 1.
  IF sy-subrc = 0.
*--Fetching Resource data
    SELECT arbpl
      FROM /agri/fmacres
      INTO TABLE @DATA(lt_arbpl)
     WHERE idresource EQ @lsr_fleet-low
       AND rstype     EQ @zcl_abs_abap_maintain=>c_rstyp_equip. "'B'
    IF sy-subrc NE 0.
      CLEAR ls_message.
      ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error. "'E'
      ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class. "'ZABS_MSGCLS'
      ls_message-msgno = '184'.
      add_messages_to_msg_container( is_message = ls_message ).
    ENDIF.
  ENDIF.

  IF lt_arbpl IS NOT INITIAL.
*-- Fetch the employees
    SELECT idresource arbpl pernr zabsausencia
      FROM /agri/fmacres
      INTO CORRESPONDING FIELDS OF TABLE et_employee
       FOR ALL ENTRIES IN lt_arbpl
     WHERE arbpl  EQ lt_arbpl-arbpl
       AND rstype EQ zcl_abs_abap_maintain=>c_rstyp_labour. "'A'
    IF sy-subrc EQ 0.
      LOOP AT et_employee ASSIGNING FIELD-SYMBOL(<fs_employee>).

*--Populating Fleet
        READ TABLE ltr_fleet INTO lsr_fleet INDEX 1.
        IF sy-subrc = 0.
          <fs_employee>-fleet = lsr_fleet-low.
        ENDIF.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = <fs_employee>-idresource
          IMPORTING
            output = <fs_employee>-idresource.
      ENDLOOP.
    ENDIF.
  ENDIF. "lt_arbpl

  IF et_employee IS INITIAL.
*-- Give error message if not mapping maintained
    CLEAR ls_message.
    ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error. "'E'
    ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class. "'ZABS_MSGCLS'
    ls_message-msgno = '162'.
    add_messages_to_msg_container( is_message = ls_message ).
  ENDIF.

ENDMETHOD.


  METHOD get_employees_np.

**----Local decleration
    DATA: lt_constants TYPE zabs_tty_vkey_const.

    REFRESH: lt_constants[].

    CALL METHOD zcl_abs_get_variants=>get_constant_multiple
      EXPORTING
        iv_mod       = space
        iv_objid     = zcl_abs_abap_maintain=>c_objid_mch
        iv_k1val     = zcl_abs_abap_maintain=>c_key_npro
        iv_k2val     = zcl_abs_abap_maintain=>c_key_emp_role
      IMPORTING
        et_constants = lt_constants.

    SELECT pernr urole
      FROM zabs_emp_role
INTO TABLE et_employee_np
   FOR ALL ENTRIES IN lt_constants
     WHERE urole EQ lt_constants-cnval1(2).
**     WHERE urole EQ zcl_abs_abap_maintain=>c_urole_01
**        OR urole EQ zcl_abs_abap_maintain=>c_urole_04.

  ENDMETHOD.


METHOD get_fleet.

*-- Local Declarations
  DATA : lt_filter     TYPE /iwbep/t_mgw_select_option,
         ls_filter     TYPE /iwbep/s_mgw_select_option,
         ls_message    TYPE /agri/s_gprolog,
         lv_filter_str TYPE string,
         lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
         ls_fleet      TYPE zabs_s_mcn_fleet,
         ltr_badge     TYPE RANGE OF zabs_del_badge,
         lsr_badge     LIKE LINE OF ltr_badge,
         ltr_fleet     TYPE RANGE OF zabs_del_fleet,
         lsr_fleet     LIKE LINE OF ltr_fleet.

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

      WHEN 'FLEET'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = ltr_fleet ).

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

  READ TABLE ltr_badge INTO lsr_badge INDEX 1.
  IF sy-subrc = 0.
    ls_fleet-badge = lsr_badge-low.
  ENDIF.

  READ TABLE ltr_fleet INTO lsr_fleet INDEX 1.
  IF sy-subrc = 0.

*--Fetching Resource data
    SELECT SINGLE idresource
      FROM /agri/fmacres
      INTO @DATA(lv_fleet)
     WHERE idresource EQ @lsr_fleet-low
       AND rstype     EQ @zcl_abs_abap_maintain=>c_rstyp_equip. "'B'
    IF sy-subrc NE 0.
      CLEAR : ls_message.
      ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error.
      ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
      ls_message-msgno = '166'.
      add_messages_to_msg_container( is_message = ls_message ).
      RETURN.
    ELSE.
      ls_fleet-fleet = lsr_fleet-low.
    ENDIF.

    "Begin of José Sequeira - 14.07.2021 12:42:01
    "Days to delete the GPX file on the users device...
    DATA: lv_gpx TYPE zabs_del_cnval.
    CALL METHOD zcl_abs_get_variants=>get_constant_single
      EXPORTING
        iv_mod    = space
        iv_objid  = 'MCH'"Machine confirmation...
        iv_k1val  = 'GPX'"GPX
      IMPORTING
        ev_cnval1 = lv_gpx.
    IF lv_gpx IS NOT INITIAL.
      ls_fleet-days = lv_gpx.
    ELSE.
      ls_fleet-days = 30."If no value is set, lets use 30 days...
    ENDIF.
    "End of José Sequeira - 14.07.2021 12:42:01

  ENDIF.

  APPEND ls_fleet TO et_fleet.
  CLEAR ls_fleet.

ENDMETHOD.


  METHOD get_fleet_np.

*-- Local Declarations
    DATA : lt_filter     TYPE /iwbep/t_mgw_select_option,
           ls_filter     TYPE /iwbep/s_mgw_select_option,
           ls_message    TYPE /agri/s_gprolog,
           lv_filter_str TYPE string,
           lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
           ltr_fleet     TYPE RANGE OF zabs_del_fleet,
           lsr_fleet     LIKE LINE OF ltr_fleet.

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

        WHEN 'FLEET'.
          lo_filter->convert_select_option(
            EXPORTING
              is_select_option = ls_filter
            IMPORTING
              et_select_option = ltr_fleet ).

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

    READ TABLE ltr_fleet INTO lsr_fleet INDEX 1.
    IF sy-subrc = 0.

*--Fetching Plant
**      SELECT fleet werks
**        FROM zabst_fleet
**      INTO TABLE et_fleet_np
**        WHERE fleet EQ lsr_fleet-low.

      SELECT a~fleet a~werks b~name1
        INTO TABLE et_fleet_np
        FROM ( zabst_fleet AS a INNER JOIN t001w AS b ON
               a~werks = b~werks )
        WHERE a~fleet EQ lsr_fleet-low.

      IF sy-subrc EQ 0.
        SORT et_fleet_np BY werks.
        DELETE ADJACENT DUPLICATES FROM et_fleet_np.
*--Fetching Fleets
**        SELECT fleet werks
**          FROM zabst_fleet
**          APPENDING TABLE et_fleet_np
**          FOR ALL ENTRIES IN et_fleet_np
**          WHERE werks EQ et_fleet_np-werks.

        SELECT a~fleet a~werks b~name1
          APPENDING TABLE et_fleet_np
          FROM ( zabst_fleet AS a INNER JOIN t001w AS b ON
                 a~werks = b~werks )
          FOR ALL ENTRIES IN et_fleet_np
          WHERE a~werks EQ et_fleet_np-werks.

      ELSEIF sy-subrc NE 0.
        CLEAR : ls_message.
        ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error.
        ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
        ls_message-msgno = '166'.
        add_messages_to_msg_container( is_message = ls_message ).
        RETURN.

      ENDIF.

    ENDIF.
    SORT et_fleet_np BY fleet werks.
    DELETE ADJACENT DUPLICATES FROM et_fleet_np.
    SORT et_fleet_np BY fleet ASCENDING.
  ENDMETHOD.


METHOD get_task_ords.

*--Local data declaration
  DATA:
*        lt_user_role TYPE zcl_zabs_mob_mcn_cnf_mpc=>tt_mcnuser,
    ls_user_role    TYPE zabs_s_mcn_fleet,
    lt_tplnr_mat    TYPE STANDARD TABLE OF ty_tplnr_mat,
    ls_tplnr_mat    TYPE ty_tplnr_mat,
    ls_task_ord     TYPE zcl_zabs_mob_mcn_cnf_mpc=>ts_mcntaskord,
    lv_atflv        TYPE p DECIMALS 3,
    lv_idact        TYPE /agri/fmidact,
    lv_cnval1       TYPE zabs_del_cnval,
    lt_filter       TYPE /iwbep/t_mgw_select_option,
    ls_filter       TYPE /iwbep/s_mgw_select_option,
    lv_filter_str   TYPE string,
    lo_filter       TYPE REF TO /iwbep/if_mgw_req_filter,
    ltr_fleet       TYPE RANGE OF zabs_del_fleet,
    lsr_fleet       LIKE LINE OF ltr_fleet,
    lv_low_gstrp    TYPE sy-datum,
    lv_high_gstrp   TYPE sy-datum,
    lv_cnval1_gstrp TYPE zabs_del_cnval,
    ltr_gstrp       TYPE RANGE OF co_gstrp,
    lv_cnval3       TYPE zabs_del_cnval,
    lv_prevdate     TYPE p0001-begda,
    lv_days         TYPE t5a4a-dlydy,
    lv_actype       TYPE /agri/fmactype,

    lr_lstar        TYPE RANGE OF lstar,
    lwa_lstar       LIKE LINE OF lr_lstar,
    ltr_arbpl       TYPE RANGE OF arbpl,
    lsr_arbpl       LIKE LINE OF ltr_arbpl.

  CONSTANTS: BEGIN OF c_operator_word,
               equalto(2) VALUE 'EQ',
             END OF c_operator_word.

*  CALL METHOD me->get_user_role
*    IMPORTING
*      et_user_role = lt_user_role.
*
*  READ TABLE lt_user_role INTO DATA(ls_user_role) INDEX 1.
*  IF sy-subrc EQ 0.

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
      WHEN 'FLEET'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = ltr_fleet ).

        READ TABLE ltr_fleet INTO lsr_fleet INDEX 1.
        IF sy-subrc = 0.
          ls_user_role-fleet = lsr_fleet-low.
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

**--Fetching Resource data
*  SELECT SINGLE idresource, arbpl, equnr
*    FROM /agri/fmacres
*    INTO @DATA(ls_afmacres)
*   WHERE idresource EQ @ls_user_role-fleet.

*--Fetching Fleet data
  SELECT *
    FROM zabst_fleet
    INTO TABLE @DATA(lt_fleet)
   WHERE fleet EQ @ls_user_role-fleet.
  IF sy-subrc EQ 0.

    DATA(lt_fleet_tmp) = lt_fleet.
    SORT lt_fleet_tmp BY werks tsk_grp.
    DELETE ADJACENT DUPLICATES FROM lt_fleet_tmp COMPARING werks tsk_grp.

*--Fetching task group data
    SELECT *
      FROM zabst_tsk_grp
      INTO TABLE @DATA(lt_tsk_grp)
       FOR ALL ENTRIES IN @lt_fleet_tmp
     WHERE werks   EQ @lt_fleet_tmp-werks
       AND tsk_grp EQ @lt_fleet_tmp-tsk_grp.
    IF sy-subrc EQ 0.
      SORT lt_tsk_grp BY werks matnr.
    ENDIF.
  ENDIF.
*ENDIF.

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

  IF lt_tsk_grp IS NOT INITIAL.
    DATA(lt_tsk_grp_tmp) = lt_tsk_grp.
    SORT lt_tsk_grp_tmp BY werks.
    DELETE ADJACENT DUPLICATES FROM lt_tsk_grp_tmp COMPARING werks.

*--Fetching terrain data
    SELECT tplnr_fl, pltxt, iwerk, strno
      FROM /agri/glflot
      INTO TABLE @DATA(lt_flot)
       FOR ALL ENTRIES IN @lt_tsk_grp_tmp
     WHERE tplvl EQ 2
       AND iwerk EQ @lt_tsk_grp_tmp-werks
       AND kfrst EQ @space
       AND loevm EQ @space.
    IF sy-subrc EQ 0.
      SORT lt_flot BY tplnr_fl.

*--Fetching Crop season data
      SELECT tplnr_fl, contr, iwerk
        FROM /agri/glflcma
        INTO TABLE @DATA(lt_flcma)
         FOR ALL ENTRIES IN @lt_flot
       WHERE tplnr_fl EQ @lt_flot-tplnr_fl
         AND datab    LE @sy-datum
         AND datbi    GE @lv_prevdate
         AND astat    EQ @zcl_abs_abap_maintain=>c_cs_active "'A'
         AND loevm    EQ @space.
      IF sy-subrc EQ 0.
        SORT lt_flcma BY iwerk.
      ENDIF.
    ENDIF.

    IF lt_flcma IS NOT INITIAL.

*--Fetching Terrain Attribute Values to get Crop Coefficient value
      SELECT mdocm, tplnr_fl,
             contr, mpgrp,
             mdate, mtime
        INTO TABLE @DATA(lt_glmdhdr)
        FROM /agri/glmdhdr
         FOR ALL ENTRIES IN @lt_flcma
       WHERE mdtyp    EQ @zcl_abs_abap_maintain=>c_mdtyp_zpta "'ZPTA'
         AND tplnr_fl EQ @lt_flcma-tplnr_fl
         AND contr    EQ @lt_flcma-contr
         AND mpgrp    EQ @zcl_abs_abap_maintain=>c_class_fazplantio "'FAZ-PLANTIO'
         AND kfrst    EQ @space.
      IF sy-subrc = 0.
        SORT lt_glmdhdr BY tplnr_fl contr
                           mdate DESCENDING
                           mtime DESCENDING.
        DELETE ADJACENT DUPLICATES FROM lt_glmdhdr COMPARING tplnr_fl contr mdate.

        SELECT a~mdocm, a~atzhl, a~atwrt,
               a~atflv, b~atinn, b~atnam
         FROM /agri/glmdatv AS a
         JOIN cabn          AS b
           ON b~atinn EQ a~atinn
         INTO TABLE @DATA(lt_glmdatv)
          FOR ALL ENTRIES IN @lt_glmdhdr
        WHERE a~mdocm EQ @lt_glmdhdr-mdocm
          AND b~atnam EQ @zcl_abs_abap_maintain=>c_charact_cit. "'CIT-ESPACAMENTO-RUA'
        IF sy-subrc = 0.
          SORT lt_glmdatv BY mdocm.
        ENDIF.

      ENDIF. "lt_glmdhdr
    ENDIF. "lt_flcma

    LOOP AT lt_tsk_grp INTO DATA(ls_tsk_grp).
      READ TABLE lt_flcma TRANSPORTING NO FIELDS
            WITH KEY iwerk = ls_tsk_grp-werks
          BINARY SEARCH.
      IF sy-subrc EQ 0.
        DATA(lv_tabix) = sy-tabix.
        LOOP AT lt_flcma INTO DATA(ls_flcma) FROM lv_tabix.
          IF ls_tsk_grp-werks NE ls_flcma-iwerk.
            EXIT.
          ENDIF.

          CLEAR ls_tplnr_mat.
          ls_tplnr_mat-tplnr_fl = ls_flcma-tplnr_fl.
          ls_tplnr_mat-contr    = ls_flcma-contr.
          ls_tplnr_mat-matnr    = ls_tsk_grp-matnr.
          ls_tplnr_mat-iwerk    = ls_flcma-iwerk.
          APPEND ls_tplnr_mat TO lt_tplnr_mat.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDIF.

  CALL METHOD zcl_abs_get_variants=>get_constant_single
    EXPORTING
      iv_mod    = space
      iv_objid  = zcl_abs_abap_maintain=>c_objid_mobile "'MOBC'
      iv_k1val  = zcl_abs_abap_maintain=>c_key_lgstrp   "'LGSTRP'
    IMPORTING
      ev_cnval1 = lv_cnval1_gstrp.
  CONDENSE lv_cnval1_gstrp.
  lv_low_gstrp   = sy-datum - lv_cnval1_gstrp.

  CALL METHOD zcl_abs_get_variants=>get_constant_single
    EXPORTING
      iv_mod    = space
      iv_objid  = zcl_abs_abap_maintain=>c_objid_mobile "'MOBC'
      iv_k1val  = zcl_abs_abap_maintain=>c_key_hgstrp   "'HGSTRP'
    IMPORTING
      ev_cnval1 = lv_cnval1_gstrp.
  CONDENSE lv_cnval1_gstrp.
  lv_high_gstrp  = sy-datum + lv_cnval1_gstrp.

  ltr_gstrp = VALUE #( sign = zcl_abs_abap_maintain=>c_rsign_include
                       option = zcl_abs_abap_maintain=>c_ropt_between " 'BT'
                      ( low  = lv_low_gstrp
                       high = lv_high_gstrp ) ).

  IF lt_tplnr_mat IS NOT INITIAL.
*--Fetching process order header data
    SELECT aufnr, tplnr_fl, contr, matnr, cstat,
           gamng, gwemg, comng, gmein, iwerk, erdat, erzet
      FROM /agri/fmfphdr
      INTO TABLE @DATA(lt_fphdr)
       FOR ALL ENTRIES IN @lt_tplnr_mat
     WHERE autyp    EQ @zcl_abs_abap_maintain=>c_autyp_tsk_ord "'TO'
       AND tplnr_fl EQ @lt_tplnr_mat-tplnr_fl
       AND contr    EQ @lt_tplnr_mat-contr
       AND matnr    EQ @lt_tplnr_mat-matnr
       AND gstrp    IN @ltr_gstrp
       AND iwerk    EQ @lt_tplnr_mat-iwerk
       AND tecom    EQ @space.
    IF sy-subrc EQ 0.
      DELETE lt_fphdr WHERE cstat EQ zcl_abs_abap_maintain=>c_ac_stat_confirmed. "'CNF'
      SORT lt_fphdr BY iwerk tplnr_fl matnr aufnr.

      DATA(lt_fphdr_tmp) = lt_fphdr.
      SORT lt_fphdr_tmp BY matnr.
      DELETE ADJACENT DUPLICATES FROM lt_fphdr_tmp COMPARING matnr.

*--Fetching material group data
      SELECT matnr, matkl
        FROM mara
        INTO TABLE @DATA(lt_mara)
         FOR ALL ENTRIES IN @lt_fphdr_tmp
       WHERE matnr EQ @lt_fphdr_tmp-matnr.
      IF sy-subrc EQ 0.
        SORT lt_mara BY matnr.

        DATA(lt_mara_tmp) = lt_mara.
        SORT lt_mara_tmp BY matkl.
        DELETE ADJACENT DUPLICATES FROM lt_mara_tmp COMPARING matkl.

*--Fetching material group description data
        SELECT matkl, wgbez
          FROM t023t
          INTO TABLE @DATA(lt_t023t)
           FOR ALL ENTRIES IN @lt_mara_tmp
         WHERE spras EQ @sy-langu
           AND matkl EQ @lt_mara_tmp-matkl.
        IF sy-subrc EQ 0.
          SORT lt_t023t BY matkl.
        ENDIF.
      ENDIF.

*--Fetching material
      SELECT matnr, maktx
        FROM makt
        INTO TABLE @DATA(lt_makt)
         FOR ALL ENTRIES IN @lt_fphdr_tmp
       WHERE matnr EQ @lt_fphdr_tmp-matnr
         AND spras EQ @sy-langu.
      IF sy-subrc EQ 0.
        SORT lt_makt BY matnr.
      ENDIF.

      lt_fphdr_tmp = lt_fphdr.
      SORT lt_fphdr_tmp BY iwerk.
      DELETE ADJACENT DUPLICATES FROM lt_fphdr_tmp COMPARING iwerk.

      SELECT werks, name1
        FROM t001w
        INTO TABLE @DATA(lt_t001w)
         FOR ALL ENTRIES IN @lt_fphdr_tmp
       WHERE werks EQ @lt_fphdr_tmp-iwerk.
      IF sy-subrc EQ 0.
        SORT lt_t001w BY werks.
      ENDIF.

      CALL METHOD zcl_abs_get_variants=>get_constant_single
        EXPORTING
          iv_mod    = space
          iv_objid  = zcl_abs_abap_maintain=>c_objid_mch "'MCH'
          iv_k1val  = zcl_abs_abap_maintain=>c_key_mch_act "'ACT'
        IMPORTING
          ev_cnval1 = lv_cnval1.  "'DESLCTO'

      lv_idact = lv_cnval1.

    ENDIF. "lt_fphdr
  ENDIF. "lt_tplnr_mat

*--Fetching Resource data
  SELECT idresource, arbpl, equnr
    FROM /agri/fmacres
    INTO TABLE @DATA(lt_afmacres)
*     FOR ALL ENTRIES IN @lt_crhd
   WHERE idresource EQ @ls_user_role-fleet
*     AND arbpl      EQ @lt_crhd-arbpl
     AND rstype     EQ @zcl_abs_abap_maintain=>c_rstyp_equip. "'B'
  IF sy-subrc = 0.
    SORT lt_afmacres BY arbpl.

    LOOP AT lt_afmacres INTO DATA(ls_afmacres).
      lsr_arbpl-sign = 'I'.
      lsr_arbpl-option = 'EQ'.
      lsr_arbpl-low = ls_afmacres-arbpl.
      APPEND lsr_arbpl TO ltr_arbpl.
    ENDLOOP.
  ENDIF.

*--Fetching Routing no. of Operations based on task order
  SELECT aufnr, aufpl
    FROM afko
    INTO TABLE @DATA(lt_aufpl)
     FOR ALL ENTRIES IN @lt_fphdr
   WHERE aufnr EQ @lt_fphdr-aufnr.
  IF sy-subrc = 0.
    SORT lt_aufpl BY aufnr.
  ENDIF.

*--Fetching Operations data for order
  IF lt_aufpl IS NOT INITIAL.
    SELECT aufpl, arbid,
           lar01, lar02,
           lar03, lar04,
           lar05, lar06
      INTO TABLE @DATA(lt_afvc)
      FROM afvc
       FOR ALL ENTRIES IN @lt_aufpl
     WHERE aufpl EQ @lt_aufpl-aufpl.                    "#EC CI_NOORDER
*       AND vgwts EQ @lv_vgwts.
    IF sy-subrc = 0.
      SORT lt_afvc BY aufpl arbid.
    ENDIF.
  ENDIF.

  IF lt_afvc IS NOT INITIAL AND ltr_arbpl IS NOT INITIAL.
*--Fetching standard value key based on work center
    SELECT objid, arbpl, vgwts
      FROM crhd
      INTO TABLE @DATA(lt_crhd)
      FOR ALL ENTRIES IN @lt_afvc
     WHERE objty EQ 'A'
       AND objid EQ @lt_afvc-arbid
       AND arbpl IN @ltr_arbpl.
    IF sy-subrc = 0.
      SORT lt_crhd BY objid.
*--Fetching standard value key data
      SELECT vgwts, par01,
             par02, par03,
             par04, par05, par06
        FROM tc21
        INTO TABLE @DATA(lt_tc21)
        FOR ALL ENTRIES IN @lt_crhd
       WHERE vgwts EQ @lt_crhd-vgwts.
      IF sy-subrc = 0.
        SORT lt_tc21 BY vgwts.
      ENDIF.

**--Fetching Resource data
*    SELECT idresource, arbpl, equnr
*      FROM /agri/fmacres
*      INTO TABLE @DATA(lt_afmacres)
*       FOR ALL ENTRIES IN @lt_crhd
*     WHERE idresource EQ @ls_user_role-fleet
*       AND arbpl      EQ @lt_crhd-arbpl
*       AND rstype     EQ @zcl_abs_abap_maintain=>c_rstyp_equip. "'B'
*    IF sy-subrc = 0.
*      SORT lt_afmacres BY arbpl.
*    ENDIF.
    ENDIF.
  ENDIF.

  IF lt_tc21 IS NOT INITIAL.
*--Fetching Accomploshment Resource data
    SELECT actyp, parid,
           rstyp, txtlg                       "#EC CI_FAE_LINES_ENSURED
      FROM /agri/tfmacrsc
      INTO TABLE @DATA(lt_fmacrsc)
       FOR ALL ENTRIES IN @lt_tc21
     WHERE ( parid EQ @lt_tc21-par01
        OR  parid EQ @lt_tc21-par02
        OR  parid EQ @lt_tc21-par03
        OR  parid EQ @lt_tc21-par04
        OR  parid EQ @lt_tc21-par05
        OR  parid EQ @lt_tc21-par06 )
       AND  rstyp EQ @zcl_abs_abap_maintain=>c_acc_rstyp_equip " "'EQ'
       AND  actyp EQ @zcl_abs_abap_maintain=>c_acctyp_tast. "'TAST'
  ENDIF.

**--Fetching Operations data for order
*  IF lt_aufpl IS NOT INITIAL.
*    SELECT aufpl, vgwts,
*           lar01, lar02,
*           lar03, lar04,
*           lar05, lar06
*      INTO TABLE @DATA(lt_afvc)
*      FROM afvc
*       FOR ALL ENTRIES IN @lt_aufpl
*     WHERE aufpl EQ @lt_aufpl-aufpl                     "#EC CI_NOORDER
*       AND vgwts EQ @lv_vgwts.
*    IF sy-subrc = 0.
*      SORT lt_afvc BY aufpl vgwts.
*    ENDIF.
*  ENDIF.

*--Fetching Activity data
  SELECT idactv, actype
    INTO TABLE @DATA(lt_activity)
    FROM /agri/fmacact
   WHERE rstype EQ @zcl_abs_abap_maintain=>c_rstyp_equip "'B'
     AND bill   EQ @zcl_abs_abap_maintain=>c_bill_yes. "'YES'
  IF sy-subrc = 0.
    SORT lt_activity BY actype.
  ENDIF.

*--Processing Task order data
  LOOP AT lt_fphdr INTO DATA(ls_fphdr).

    CLEAR : ls_task_ord,
            lv_atflv.

    ls_task_ord-aufnr = ls_fphdr-aufnr.
    ls_task_ord-werks = ls_fphdr-iwerk.

    READ TABLE lt_t001w INTO DATA(ls_t001w)
          WITH KEY werks = ls_fphdr-iwerk
        BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_task_ord-name1 = ls_t001w-name1.
    ENDIF.

    ls_task_ord-tplnr = ls_fphdr-tplnr_fl.
    ls_task_ord-contr = ls_fphdr-contr.

    READ TABLE lt_glmdhdr INTO DATA(ls_glmdhdr)
                     WITH KEY tplnr_fl = ls_fphdr-tplnr_fl
                              contr    = ls_fphdr-contr
                     BINARY SEARCH.
    IF sy-subrc = 0.

      READ TABLE lt_glmdatv INTO DATA(ls_glmdatv)
      WITH KEY mdocm = ls_glmdhdr-mdocm
               atnam = zcl_abs_abap_maintain=>c_charact_cit "'CIT-ESPACAMENTO-RUA'
      BINARY SEARCH.
      IF sy-subrc = 0.

*        IF ls_glmdatv-atwrt IS NOT INITIAL.
*          ls_task_ord-wdline = ls_glmdatv-atwrt.
*        ELSEIF ls_glmdatv-atflv IS NOT INITIAL.
*          lv_atflv = ls_glmdatv-atflv.
*          ls_task_ord-wdline =  lv_atflv.
*        ENDIF.

        lv_atflv = ls_glmdatv-atflv.
        ls_task_ord-wdline = lv_atflv.

      ENDIF. "lt_glmdatv
    ENDIF. "lt_glmdhdr

    READ TABLE lt_flot INTO DATA(ls_flot)
          WITH KEY tplnr_fl = ls_fphdr-tplnr_fl
        BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_task_ord-pltxt = ls_flot-pltxt.
    ENDIF.

    ls_task_ord-tmatnr = ls_fphdr-matnr.
    READ TABLE lt_makt INTO DATA(ls_makt)
          WITH KEY matnr = ls_fphdr-matnr
        BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_task_ord-maktx = ls_makt-maktx.
    ENDIF.

    READ TABLE lt_mara INTO DATA(ls_mara)
          WITH KEY matnr = ls_fphdr-matnr
        BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_task_ord-matkl = ls_mara-matkl.

      READ TABLE lt_t023t INTO DATA(ls_t023t)
            WITH KEY matkl = ls_mara-matkl
          BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_task_ord-wgbez = ls_t023t-wgbez.
      ENDIF.
    ENDIF.

    ls_task_ord-gamng = ls_fphdr-gamng.
    ls_task_ord-gwemg = ls_fphdr-gwemg.
    ls_task_ord-comng = ls_fphdr-comng.
    ls_task_ord-gmein = ls_fphdr-gmein.
    ls_task_ord-zzmeinh = ls_fphdr-gmein.

*      ls_task_ord-equnr = ls_afmacres-equnr.
*      ls_task_ord-arbpl = ls_afmacres-arbpl.

    CLEAR ls_tsk_grp.
    READ TABLE lt_tsk_grp INTO ls_tsk_grp
          WITH KEY werks = ls_fphdr-iwerk
                   matnr = ls_fphdr-matnr
        BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_task_ord-rec_speed = ls_tsk_grp-rec_speed.
      ls_task_ord-min_speed = ls_tsk_grp-min_speed.
      ls_task_ord-max_speed = ls_tsk_grp-max_speed.
    ENDIF.

    ls_task_ord-erdat = ls_fphdr-erdat.
    ls_task_ord-erzet = ls_fphdr-erzet.
    ls_task_ord-idactve = lv_idact.
    ls_task_ord-fleet = ls_user_role-fleet.

*--Getting Activity Id
    READ TABLE lt_aufpl INTO DATA(ls_aufpl)
    WITH KEY aufnr = ls_fphdr-aufnr
    BINARY SEARCH.
    IF sy-subrc = 0.

*      READ TABLE lt_afvc INTO DATA(ls_afvc)
*      WITH KEY aufpl = ls_aufpl-aufpl
**                 vgwts = lv_vgwts
*      BINARY SEARCH.
*      IF sy-subrc = 0.

      LOOP AT lt_afvc INTO DATA(ls_afvc) WHERE aufpl = ls_aufpl-aufpl.

        READ TABLE lt_crhd INTO DATA(ls_crhd)
        WITH KEY objid = ls_afvc-arbid
        BINARY SEARCH.
        IF sy-subrc NE 0.
          CONTINUE.
        ENDIF.
*          ls_task_ord-arbpl = ls_crhd-arbpl.

        CLEAR ls_afmacres.
        READ TABLE lt_afmacres INTO ls_afmacres
        WITH KEY arbpl = ls_crhd-arbpl
        BINARY SEARCH.
        IF sy-subrc = 0.
          ls_task_ord-equnr = ls_afmacres-equnr.
          ls_task_ord-arbpl = ls_afmacres-arbpl.
        ENDIF. "LT_AFMACRES

        READ TABLE lt_tc21 INTO DATA(ls_tc21)
        WITH KEY vgwts = ls_crhd-vgwts
        BINARY SEARCH.
        IF sy-subrc EQ 0.

          CLEAR lv_idact.
          LOOP AT lt_fmacrsc INTO DATA(ls_fmacrsc).

            IF ls_fmacrsc-parid = ls_tc21-par01
           AND ls_afvc-lar01 IS NOT INITIAL.
              lv_actype = ls_afvc-lar01.
              EXIT.
            ENDIF.
            IF ls_fmacrsc-parid = ls_tc21-par02
              AND ls_afvc-lar02 IS NOT INITIAL.
              lv_actype = ls_afvc-lar02.
              EXIT.
            ENDIF.
            IF ls_fmacrsc-parid = ls_tc21-par03
              AND ls_afvc-lar03 IS NOT INITIAL.
              lv_actype = ls_afvc-lar03.
              EXIT.
            ENDIF.
            IF ls_fmacrsc-parid = ls_tc21-par04
              AND ls_afvc-lar04 IS NOT INITIAL.
              lv_actype = ls_afvc-lar04.
              EXIT.
            ENDIF.
            IF ls_fmacrsc-parid = ls_tc21-par05
              AND ls_afvc-lar05 IS NOT INITIAL.
              lv_actype = ls_afvc-lar05.
              EXIT.
            ENDIF.
            IF ls_fmacrsc-parid = ls_tc21-par06
              AND ls_afvc-lar06 IS NOT INITIAL.
              lv_actype = ls_afvc-lar06.
              EXIT.
            ENDIF.
          ENDLOOP. "lt_fmacrsc

          READ TABLE lt_activity INTO DATA(ls_activity)
          WITH KEY actype = lv_actype.
          IF sy-subrc = 0.
            ls_task_ord-idactv = ls_activity-idactv.
          ENDIF.

        ENDIF. "LT_TC21
*        ENDIF. "LT_CRHD
*      ENDIF. "lt_afvc
        EXIT.
      ENDLOOP.
    ENDIF. "lt_aufpl

    APPEND ls_task_ord TO et_task_ord.

  ENDLOOP. "lt_fphdr

  SORT et_task_ord BY erdat erzet.

ENDMETHOD.


METHOD get_user_role.

*-- Local Declarations
  DATA: lo_message_container TYPE REF TO /iwbep/if_message_container,
        ls_message           TYPE /agri/s_gprolog,
        lv_cnval1            TYPE zabs_del_cnval,
        ls_user_role         TYPE zabs_s_mcn_usr_emp.

  CALL METHOD me->/iwbep/if_mgw_conv_srv_runtime~get_message_container
    RECEIVING
      ro_message_container = lo_message_container.

**-- Fetch user role
*  SELECT bname urole fleet
*    FROM zabs_usr_emp
*    INTO TABLE et_user_role
*   WHERE bname EQ sy-uname.

"-----------------------------------Wave 3 changes
  CALL METHOD zcl_abs_get_variants=>get_constant_single
    EXPORTING
      iv_mod    = space
      iv_objid  = zcl_abs_abap_maintain=>c_objid_mch
      iv_k1val  = zcl_abs_abap_maintain=>c_key_mch_gtol
    IMPORTING
      ev_cnval1 = lv_cnval1.

  CLEAR ls_user_role.
  ls_user_role-gtol = lv_cnval1.
  APPEND ls_user_role to et_user_role.
"-----------------------------------Wave 3 changes

*  IF sy-subrc NE 0.
**-- Give error message if not mapping maintained
*    CLEAR ls_message.
*    ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error. "'E'
*    ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class. "'ZABS_MSGCLS'
*    ls_message-msgno = '086'.
*    ls_message-msgv1 = sy-uname.
*    add_messages_to_msg_container( is_message = ls_message ).
*  ENDIF.

ENDMETHOD.


METHOD mcnaccitemsset_get_entityset.

  CALL METHOD me->get_accitems
    EXPORTING
      io_tech_request_context = io_tech_request_context
    IMPORTING
      et_accitems             = et_entityset.

*  CALL METHOD me->get_accitems
*    IMPORTING
*      et_accitems = et_entityset.

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


METHOD mcnactvityset_get_entityset.

*-- Fetch Activities
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


  method MCNCONFIRMATIONS_DELETE_ENTITY.
**TRY.
*CALL METHOD SUPER->MCNCONFIRMATIONS_DELETE_ENTITY
*  EXPORTING
*    IV_ENTITY_NAME          =
*    IV_ENTITY_SET_NAME      =
*    IV_SOURCE_NAME          =
*    IT_KEY_TAB              =
**    io_tech_request_context =
*    IT_NAVIGATION_PATH      =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
  endmethod.


METHOD mcnconfirmations_get_entityset.

  DATA: lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
        lt_filter     TYPE /iwbep/t_mgw_select_option,
        ls_filter     TYPE /iwbep/s_mgw_select_option,
        lv_filter_str TYPE string,
        ltr_equnr     TYPE RANGE OF /agri/fmaequnr,
        lsr_equnr     LIKE LINE OF ltr_equnr,
        lv_equnr      TYPE /agri/fmaequnr,
        ls_mcn_ac_cnf TYPE zabs_s_mcn_ac_cnf.

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

  LOOP AT lt_filter INTO ls_filter.
    CASE ls_filter-property.
      WHEN 'EQUNR'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = ltr_equnr ).

        READ TABLE ltr_equnr INTO lsr_equnr INDEX 1.
        IF sy-subrc = 0.
          lv_equnr = lsr_equnr-low.
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

*--Fetching Non-Prodective data
  SELECT SINGLE *
      FROM zabs_mcn_np
  INTO @DATA(ls_zmcn_np)
     WHERE equnr   EQ @lv_equnr
       AND actcomp EQ @space.
  IF sy-subrc = 0.
    MOVE-CORRESPONDING ls_zmcn_np TO ls_mcn_ac_cnf.
    ls_mcn_ac_cnf-findt_chk = abap_true.
    APPEND ls_mcn_ac_cnf TO et_entityset.
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


  METHOD mcnconfirmations_update_entity.

    DATA: lv_1 TYPE c.

    lv_1 = 'A' .
  ENDMETHOD.


METHOD mcnemployeeset_get_entityset.

*-- Fetch Employees
  CALL METHOD me->get_employees
    EXPORTING
      io_tech_request_context = io_tech_request_context
    IMPORTING
      et_employee             = et_entityset.

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


METHOD mcnfleetset_get_entityset.

  CALL METHOD me->get_fleet
    EXPORTING
      io_tech_request_context = io_tech_request_context
    IMPORTING
      et_fleet                = et_entityset.

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


METHOD mcngistokenset_get_entityset.

*-- Local Declarations
  DATA: ls_message TYPE /agri/s_gprolog.

*-- Fetch GIS Tokendata
  SELECT *
    FROM zabst_gistoken
    INTO TABLE @DATA(lt_gistoken)
   WHERE sys EQ @zcl_abs_abap_maintain=>c_gis_system. "'GIS'
  IF sy-subrc EQ 0.
    MOVE-CORRESPONDING lt_gistoken TO et_entityset.
  ENDIF.

  IF et_entityset IS INITIAL.
*-- Give error message
    CLEAR ls_message.
    ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error. "'E'
    ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
    ls_message-msgno = '030'.
    add_messages_to_msg_container( is_message = ls_message ).
  ENDIF.

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


  METHOD mcnnpemployeeset_get_entityset.

*-- Fetch Employees
    CALL METHOD me->get_employees_np
      IMPORTING
        et_employee_np          = et_entityset.

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


  METHOD mcnnpfleetset_get_entityset.

    CALL METHOD me->get_fleet_np
      EXPORTING
        io_tech_request_context = io_tech_request_context
      IMPORTING
        et_fleet_np             = et_entityset.

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


METHOD mcntaskordset_get_entityset.

*-- Prepare the task orders
  CALL METHOD me->get_task_ords
    EXPORTING
      io_tech_request_context = io_tech_request_context
    IMPORTING
      et_task_ord             = et_entityset.

*  CALL METHOD me->get_task_ords
*    IMPORTING
*      et_task_ord = et_entityset.

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


METHOD mcnuserset_get_entityset.

*-- Get login user role
  CALL METHOD me->get_user_role
    IMPORTING
      et_user_role = et_entityset.

*** inlinecount
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


METHOD populate_accomplishment_data.

*-- Local Declarations
  DATA:
*        lv_actyp TYPE /agri/fmactyp,
    lv_task  TYPE /agri/gltmatnr,
    lv_posnr TYPE /agri/glposnr.

  DATA(ls_achdr) = is_achdr.
  DATA(lt_acitm) = it_acitm.

**-- Read Accomplishment type and task
*  CALL METHOD zcl_abs_get_variants=>get_constant_single
*    EXPORTING
*      iv_objid  = zcl_abs_abap_maintain=>c_objid_mobile
*      iv_k1val  = zcl_abs_abap_maintain=>c_key_accomplish_type
*      iv_k2val  = ls_achdr-zzactcg
*    IMPORTING
*      ev_cnval1 = lv_actyp.

*-- Populate Accomplishment header data
  ls_achdr-accom  = TEXT-001.
  ls_achdr-actyp  = 'TAST'. "lv_actyp.
  ls_achdr-gjahr  = ls_achdr-strtdat(4).
*  IF ls_achdr-zzactcg = zcl_abs_abap_maintain=>c_actcg_prod.
*  ls_achdr-status = zcl_abs_abap_maintain=>c_ac_status_created.
*  ELSE.
*    ls_achdr-status = zcl_abs_abap_maintain=>c_ac_status_confirm.
*  ENDIF.
  ls_achdr-descr  = ls_achdr-zzmackey.
  ls_achdr-updkz  = zcl_abs_abap_maintain=>c_insert.

*  READ TABLE lt_acitm INTO DATA(ls_acitm) INDEX 1.
*  IF sy-subrc EQ 0.
*    ls_achdr-zztplnr  = ls_acitm-tplnr.
*  ENDIF.

*-- Fetch the company code for the plant
  SELECT SINGLE bukrs
    FROM t001k
    INTO ls_achdr-bukrs
   WHERE bwkey EQ ls_achdr-werks.

  LOOP AT lt_acitm ASSIGNING FIELD-SYMBOL(<fs_acitm>).
*-- Populate the Accomplishment items
    <fs_acitm>-accom = TEXT-001.
    <fs_acitm>-zztplnr = <fs_acitm>-tplnr.
    IF <fs_acitm>-zzbill = 'YES'.
      <fs_acitm>-status = zcl_abs_abap_maintain=>c_ac_status_created.
      ls_achdr-status = zcl_abs_abap_maintain=>c_ac_status_created.
    ELSE.
      <fs_acitm>-status = zcl_abs_abap_maintain=>c_ac_status_confirm.
    ENDIF.
    <fs_acitm>-updkz = zcl_abs_abap_maintain=>c_insert.
  ENDLOOP.

  IF ls_achdr-status IS INITIAL.
    ls_achdr-status = zcl_abs_abap_maintain=>c_ac_status_confirm.
  ENDIF.

  es_acdoc-accom = TEXT-001.
  es_acdoc-x-achdr = ls_achdr.
  es_acdoc-x-acitm = lt_acitm.

ENDMETHOD.
ENDCLASS.
