class ZCL_ZABS_MOB_IRRG_DPC_EXT definition
  public
  inheriting from ZCL_ZABS_MOB_IRRG_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_BEGIN
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_END
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_PROCESS
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITYSET_DELTA
    redefinition .
protected section.

  methods MIRRCONFIRMATION_GET_ENTITYSET
    redefinition .
  methods MIRRFORMSET_GET_ENTITYSET
    redefinition .
  methods MIRRINICONTRSET_GET_ENTITYSET
    redefinition .
  methods MIRRNUMDAYSSET_GET_ENTITYSET
    redefinition .
  methods MIRRPROJECTSET_GET_ENTITYSET
    redefinition .
  methods MIRRSHIFTF4SET_GET_ENTITYSET
    redefinition .
  methods MIRRSUPPLYMATSET_GET_ENTITYSET
    redefinition .
  methods MIRRUSERSET_GET_ENTITYSET
    redefinition .
private section.

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
  methods GET_DELTA_TOKEN
    importing
      !IV_ENTITY_SET_NAME type STRING
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
    exporting
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
    changing
      !CT_ENTITYSET type STANDARD TABLE .
  methods GET_FORMS
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
    exporting
      !ET_FORMS type ZABS_TTY_MIRR_PLANTS .
  methods GET_PROJECTS
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
    exporting
      !ET_PROJECTS type ZABS_TTY_MIRR_PROJECT .
  methods GET_SHIFTS
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
    exporting
      !ET_SHIFTS type ZABS_TTY_MIRR_SHIFTF4 .
  methods GET_INITIAL_COUNTER
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
    exporting
      !ET_ICONTR type ZABS_TTY_MIRR_ICONTR .
  methods GET_SUPPLY_MATERIALS
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
    exporting
      !ET_SUP_MAT type ZABS_TTY_MIRR_SUPPLY .
  methods GET_NUMDAYS
    exporting
      !ET_NUMDAYS type ZABS_TTY_MIRR_NUMDAYS .
  methods GET_USER
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_USER type ZABS_TTY_MIRR_USER .
ENDCLASS.



CLASS ZCL_ZABS_MOB_IRRG_DPC_EXT IMPLEMENTATION.


METHOD /iwbep/if_mgw_appl_srv_runtime~changeset_begin.

  CLEAR: cv_defer_mode.
  cv_defer_mode = abap_true.

*-- Looping Entityset name in the Operation Table
  LOOP AT it_operation_info ASSIGNING FIELD-SYMBOL(<fs_operation_info>).
    IF NOT <fs_operation_info>-entity_set  EQ 'MirrConfirmationSet'.
      cv_defer_mode = abap_false.
      EXIT.
    ENDIF.
  ENDLOOP.

ENDMETHOD.


METHOD /iwbep/if_mgw_appl_srv_runtime~changeset_end.

ENDMETHOD.


METHOD /iwbep/if_mgw_appl_srv_runtime~changeset_process.

*-- Local Declarations
  DATA: lo_update_context      TYPE REF TO /iwbep/if_mgw_req_entity_u,
        lv_entity_type         TYPE string,
        ls_mirrconf            TYPE zcl_zabs_mob_irrg_mpc=>ts_mirrconfirmation,
        ls_mirrconf_hdr        TYPE zabs_s_mirr_confhdr,
        ls_mirrconf_itm        TYPE zabs_s_mirr_confitm,
        ls_wocnf               TYPE zabs_str_ocnf,
        ls_wocon               TYPE zabs_str_ocon,
        lt_wocon               TYPE TABLE OF zabs_str_ocon,
        lt_mirrconf_hdr        TYPE TABLE OF zabs_s_mirr_confhdr,
        lt_mirrconf_itm        TYPE TABLE OF zabs_s_mirr_confitm,
        lt_messages            TYPE /agri/t_gprolog,
        lt_msg                 TYPE /agri/t_gprolog,
        lt_mirrconf            TYPE zcl_zabs_mob_irrg_mpc=>tt_mirrconfirmation,
        lwa_changeset_response TYPE /iwbep/if_mgw_appl_types=>ty_s_changeset_response.

  FIELD-SYMBOLS : <lfs_data> TYPE any.

  LOOP AT it_changeset_request ASSIGNING FIELD-SYMBOL(<fs_changeset_request>).
    CLEAR lv_entity_type.
    lo_update_context ?= <fs_changeset_request>-request_context.
    lv_entity_type = lo_update_context->get_entity_type_name( ).

    IF NOT lv_entity_type EQ 'MirrConfirmation'.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
        EXPORTING
          textid      = /iwbep/cx_mgw_tech_exception=>operation_not_supported
          operation   = 'UPDATE_ENTITY'
          entity_type = lv_entity_type.
    ENDIF.

*-- Populate the operation number
    lwa_changeset_response-operation_no = <fs_changeset_request>-operation_no.

    CASE lv_entity_type.
      WHEN 'MirrConfirmation'.
        <fs_changeset_request>-entry_provider->read_entry_data( IMPORTING es_data = ls_mirrconf ).
*--Collecting Confirmation Header
        MOVE-CORRESPONDING ls_mirrconf TO ls_mirrconf_hdr.
        MOVE-CORRESPONDING ls_mirrconf TO ls_mirrconf_itm.
        APPEND ls_mirrconf_hdr TO lt_mirrconf_hdr.
        IF ls_mirrconf_itm-esmng NE 0.
          APPEND ls_mirrconf_itm TO lt_mirrconf_itm.
        ENDIF.

        ASSIGN ls_mirrconf TO <lfs_data>.
        CLEAR: ls_mirrconf,
               ls_mirrconf_hdr,
               ls_mirrconf_itm.
    ENDCASE.

    copy_data_to_ref( EXPORTING is_data = <lfs_data>
               CHANGING cr_data = lwa_changeset_response-entity_data ).
    INSERT lwa_changeset_response INTO TABLE ct_changeset_response.

  ENDLOOP.

  IF lt_mirrconf_hdr IS NOT INITIAL.

    SORT lt_mirrconf_hdr BY confkey.
    DELETE ADJACENT DUPLICATES FROM lt_mirrconf_hdr COMPARING confkey.

    LOOP AT lt_mirrconf_hdr INTO ls_mirrconf_hdr.

      REFRESH lt_wocon.
      CLEAR ls_wocnf.

      MOVE-CORRESPONDING ls_mirrconf_hdr TO ls_wocnf.

      READ TABLE lt_mirrconf_itm TRANSPORTING NO FIELDS
                                 WITH KEY confkey = ls_mirrconf_hdr-confkey
                                 BINARY SEARCH.
      IF sy-subrc = 0.

        DATA(lv_tabix) = sy-tabix.
        LOOP AT lt_mirrconf_itm INTO ls_mirrconf_itm FROM lv_tabix.
          IF ls_mirrconf_itm-confkey <> ls_mirrconf_hdr-confkey.
            EXIT.
          ENDIF.

          MOVE-CORRESPONDING ls_mirrconf_itm TO ls_wocon.

          APPEND ls_wocon TO lt_wocon.
          CLEAR : ls_wocon,
                  ls_mirrconf_itm.

        ENDLOOP. "ls_mirrconf_itm

      ENDIF. "LT_MIRRCONF_ITM

*--Calling FM to Conform
      CALL FUNCTION 'ZABS_FM_WO_CONF'
        EXPORTING
          is_woconf   = ls_wocnf
          it_wocon    = lt_wocon
        IMPORTING
          et_messages = lt_messages.

      CLEAR : ls_mirrconf_hdr.

      IF lt_messages IS NOT INITIAL.
        APPEND LINES OF lt_messages TO lt_msg.
      ENDIF.

    ENDLOOP. "LT_MIRRCONF_HDR
  ENDIF. ""LT_MIRRCONF_HDR

*-- Add the messages to containersa
  add_messages_to_msg_container( iv_exception = abap_true
                                 it_messages = lt_msg ).

ENDMETHOD.


METHOD /iwbep/if_mgw_appl_srv_runtime~get_entityset_delta.

*-- Local Declarations
  DATA: lv_entity_set_name   TYPE string,
        lv_delta_token       TYPE string,
        lo_dp_facade         TYPE REF TO /iwbep/if_mgw_dp_facade,
        lt_mirrform          TYPE zcl_zabs_mob_irrg_mpc=>tt_mirrform,
        lt_mirrform_del      TYPE zcl_zabs_mob_irrg_mpc=>tt_mirrform,
        lt_mirrproject       TYPE zcl_zabs_mob_irrg_mpc=>tt_mirrproject,
        lt_mirrproject_del   TYPE zcl_zabs_mob_irrg_mpc=>tt_mirrproject,
        lt_mirrshiftf4       TYPE zcl_zabs_mob_irrg_mpc=>tt_mirrshiftf4,
        lt_mirrshiftf4_del   TYPE zcl_zabs_mob_irrg_mpc=>tt_mirrshiftf4,
        lt_mirrinicontr      TYPE zcl_zabs_mob_irrg_mpc=>tt_mirrinicontr,
        lt_mirrinicontr_del  TYPE zcl_zabs_mob_irrg_mpc=>tt_mirrinicontr,
        lt_mirrsupplymat     TYPE zcl_zabs_mob_irrg_mpc=>tt_mirrsupplymat,
        lt_mirrsupplymat_del TYPE zcl_zabs_mob_irrg_mpc=>tt_mirrsupplymat,
        lt_mirrnumdays       TYPE zcl_zabs_mob_irrg_mpc=>tt_mirrnumdays,
        lt_mirrnumdays_del   TYPE zcl_zabs_mob_irrg_mpc=>tt_mirrnumdays.
*        lt_mirruser          TYPE zcl_zabs_mob_irrg_mpc=>tt_mirruser,
*        lt_mirruser_del      TYPE zcl_zabs_mob_irrg_mpc=>tt_mirruser.

*-- Getting the EntitySet name
  lv_entity_set_name = io_tech_request_context->get_entity_set_name( ).

  CASE lv_entity_set_name.

    WHEN 'MirrFormSet'.

*      CALL METHOD me->get_forms
*        IMPORTING
*          et_forms = lt_mirrform.

      CALL METHOD me->get_forms
        EXPORTING
          io_tech_request_context = io_tech_request_context
        IMPORTING
          et_forms                = lt_mirrform.

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
              it_entityset             = lt_mirrform
            IMPORTING
              et_deleted_entityset     = lt_mirrform_del
              et_entityset             = lt_mirrform
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
                         is_data = lt_mirrform_del
                        CHANGING
                         cr_data = er_deleted_entityset ).

*-- Export the changed entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_mirrform
                        CHANGING
                         cr_data = er_entityset ).

    WHEN 'MirrProjectSet'.

*--Calling method to get Projects data
*      CALL METHOD me->get_projects
*        IMPORTING
*          et_projects = lt_mirrproject.

      CALL METHOD me->get_projects
        EXPORTING
          io_tech_request_context = io_tech_request_context
        IMPORTING
          et_projects             = lt_mirrproject.

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
              it_entityset             = lt_mirrproject
            IMPORTING
              et_deleted_entityset     = lt_mirrproject_del
              et_entityset             = lt_mirrproject
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
                         is_data = lt_mirrproject_del
                        CHANGING
                         cr_data = er_deleted_entityset ).

*-- Export the changed entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_mirrproject
                        CHANGING
                         cr_data = er_entityset ).

    WHEN 'MirrShiftF4Set'.

*--Calling method to get Shifts data
*      CALL METHOD me->get_shifts
*        IMPORTING
*          et_shifts = lt_mirrshiftf4.

      CALL METHOD me->get_shifts
        EXPORTING
          io_tech_request_context = io_tech_request_context
        IMPORTING
          et_shifts               = lt_mirrshiftf4.

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
              it_entityset             = lt_mirrshiftf4
            IMPORTING
              et_deleted_entityset     = lt_mirrshiftf4_del
              et_entityset             = lt_mirrshiftf4
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
                         is_data = lt_mirrshiftf4_del
                        CHANGING
                         cr_data = er_deleted_entityset ).

*-- Export the changed entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_mirrshiftf4
                        CHANGING
                         cr_data = er_entityset ).

    WHEN 'MirrIniContrSet'.

*--Calling method to get Initial Counter data
*      CALL METHOD me->get_initial_counter
*        IMPORTING
*          et_icontr = lt_mirrinicontr.

      CALL METHOD me->get_initial_counter
        EXPORTING
          io_tech_request_context = io_tech_request_context
        IMPORTING
          et_icontr               = lt_mirrinicontr.

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
              it_entityset             = lt_mirrinicontr
            IMPORTING
              et_deleted_entityset     = lt_mirrinicontr_del
              et_entityset             = lt_mirrinicontr
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
                         is_data = lt_mirrinicontr_del
                        CHANGING
                         cr_data = er_deleted_entityset ).

*-- Export the changed entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_mirrinicontr
                        CHANGING
                         cr_data = er_entityset ).

    WHEN 'MirrSupplyMatSet'.

*--Calling Supply materials data
*      CALL METHOD me->get_supply_materials
*        IMPORTING
*          et_sup_mat = lt_mirrsupplymat.

      CALL METHOD me->get_supply_materials
        EXPORTING
          io_tech_request_context = io_tech_request_context
        IMPORTING
          et_sup_mat              = lt_mirrsupplymat.

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
              it_entityset             = lt_mirrsupplymat
            IMPORTING
              et_deleted_entityset     = lt_mirrsupplymat_del
              et_entityset             = lt_mirrsupplymat
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
                         is_data = lt_mirrsupplymat_del
                        CHANGING
                         cr_data = er_deleted_entityset ).

*-- Export the changed entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_mirrsupplymat
                        CHANGING
                         cr_data = er_entityset ).

    WHEN 'MirrNumDaysSet'.

      CALL METHOD me->get_numdays
        IMPORTING
          et_numdays = lt_mirrnumdays.

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
              it_entityset             = lt_mirrnumdays
            IMPORTING
              et_deleted_entityset     = lt_mirrnumdays_del
              et_entityset             = lt_mirrnumdays
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
                         is_data = lt_mirrnumdays_del
                        CHANGING
                         cr_data = er_deleted_entityset ).

*-- Export the changed entity set
      copy_data_to_ref( EXPORTING
                         is_data = lt_mirrnumdays
                        CHANGING
                         cr_data = er_entityset ).

*    WHEN 'MirrUserSet'.
*
*      CALL METHOD me->get_user
*        EXPORTING
*          io_tech_request_context = io_tech_request_context
*        IMPORTING
*          et_user                 = lt_mirruser.
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
*              it_entityset             = lt_mirruser
*            IMPORTING
*              et_deleted_entityset     = lt_mirruser_del
*              et_entityset             = lt_mirruser
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
*                         is_data = lt_mirruser_del
*                        CHANGING
*                         cr_data = er_deleted_entityset ).
*
**-- Export the changed entity set
*      copy_data_to_ref( EXPORTING
*                         is_data = lt_mirruser
*                        CHANGING
*                         cr_data = er_entityset ).

  ENDCASE.

ENDMETHOD.


  METHOD add_messages_to_msg_container.

*-- Local Declarations
    DATA: ls_bapi_messages TYPE bapiret2,
          lo_msg_container TYPE REF TO /iwbep/if_message_container,
          le_msg_container TYPE REF TO /iwbep/if_message_container,
          ls_message       TYPE /agri/s_gprolog,
          lv_msg_error     TYPE xfld,
          lv_msgno         TYPE symsgno.

    CONSTANTS: c_true TYPE xfld VALUE 'X'.

*** Get Message container object
    lo_msg_container = me->mo_context->get_message_container( ).
    le_msg_container = me->mo_context->get_message_container( ).

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
          lv_msgno = ls_message-msgno.
          le_msg_container->add_message(
             EXPORTING iv_msg_type               = ls_message-msgty
                       iv_msg_id                 = ls_message-msgid
                       iv_msg_number             = lv_msgno
                       iv_msg_v1                 = ls_message-msgv1
                       iv_msg_v2                 = ls_message-msgv2
                       iv_msg_v3                 = ls_message-msgv3
                       iv_msg_v4                 = ls_message-msgv4
                       iv_entity_type            = iv_entity_name
                       iv_add_to_response_header = abap_true ).
        else.
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
       ENDIF.
      ENDLOOP.
    ENDIF.

    IF iv_exception EQ abap_true AND lv_msg_error IS NOT INITIAL.
*    " Raise Exception
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          textid            = /iwbep/cx_mgw_busi_exception=>business_error
          entity_type       = iv_entity_name
          message_container = le_msg_container.
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


METHOD get_forms.

*-- Local Declarations
  DATA: lo_message_container TYPE REF TO /iwbep/if_message_container,
        ls_message           TYPE /agri/s_gprolog,
        lo_filter            TYPE REF TO /iwbep/if_mgw_req_filter,
        lt_filter            TYPE /iwbep/t_mgw_select_option,
        ls_filter            TYPE /iwbep/s_mgw_select_option,
        lv_filter_str        TYPE string,
        ltr_badge            TYPE RANGE OF zabs_del_badge,
        lsr_badge            LIKE LINE OF ltr_badge.

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

  CALL METHOD me->/iwbep/if_mgw_conv_srv_runtime~get_message_container
    RECEIVING
      ro_message_container = lo_message_container.

  IF ltr_badge IS NOT INITIAL.
*-- Fetch the forms for the badge who logged in
    SELECT werks
      INTO TABLE @DATA(lt_werks)
      FROM zabs_irr_usr
     WHERE badge IN @ltr_badge.
    IF sy-subrc EQ 0.
*-- Fetch the Plants
      SELECT werks
             name1
        FROM t001w
        INTO TABLE et_forms
         FOR ALL ENTRIES IN lt_werks
       WHERE werks EQ lt_werks-werks.
      IF sy-subrc EQ 0.
        CLEAR lsr_badge.
        READ TABLE ltr_badge INTO lsr_badge INDEX 1.
        IF sy-subrc EQ 0.
          LOOP AT et_forms ASSIGNING FIELD-SYMBOL(<fs_forms>).
            <fs_forms>-badge = lsr_badge-low.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

**-- Fetch the Plants
*  SELECT werks
*         name1
*    FROM t001w
*    INTO TABLE et_forms.
*  IF sy-subrc NE 0.
**-- Give error message
*    CLEAR ls_message.
*    ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error. "'E'
*    ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
*    ls_message-msgno = '133'.
*    add_messages_to_msg_container( is_message = ls_message ).
*  ENDIF.

  IF et_forms IS INITIAL.
    CLEAR ls_message.
    ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error. "'E'
    ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
    ls_message-msgno = '133'.
    add_messages_to_msg_container( is_message = ls_message ).
  ENDIF.

ENDMETHOD.


METHOD get_initial_counter.

*--Local declarations
  DATA : lt_project           TYPE zabs_tty_mirr_project,
         lt_shift             TYPE zabs_tty_mirr_shiftf4,
         ls_icontr            LIKE LINE OF et_icontr,
         lv_pdate             TYPE datum,
         lv_bck_mnth          TYPE numc3,
         lv_cnval1            TYPE zabs_del_cnval,
         lo_message_container TYPE REF TO /iwbep/if_message_container,
         ls_message           TYPE /agri/s_gprolog,
         lv_lmnga             TYPE /agri/fmlmnga,
         lv_tqty              TYPE gamng,
         lv_vnsft             TYPE zabs_del_vnsft. "Vazao Nominal Shift

  CALL METHOD me->/iwbep/if_mgw_conv_srv_runtime~get_message_container
    RECEIVING
      ro_message_container = lo_message_container.

*--Get variant table data
  CALL METHOD zcl_abs_get_variants=>get_constant_single
    EXPORTING
      iv_mod    = space
      iv_objid  = zcl_abs_abap_maintain=>c_objid_irr_wb "'IRWO'
      iv_k1val  = zcl_abs_abap_maintain=>c_key_irr_mnth "'MNTH'
    IMPORTING
      ev_cnval1 = lv_cnval1. "'1'

  lv_bck_mnth = lv_cnval1.

  CALL FUNCTION 'CCM_GO_BACK_MONTHS'
    EXPORTING
      currdate   = sy-datum
      backmonths = lv_bck_mnth
    IMPORTING
      newdate    = lv_pdate.

*--Calling method to get projects data
*  CALL METHOD me->get_projects
*    IMPORTING
*      et_projects = lt_project.

  CALL METHOD me->get_projects
    EXPORTING
      io_tech_request_context = io_tech_request_context
    IMPORTING
      et_projects             = lt_project.

*--Calling method to get Shift data
*  CALL METHOD me->get_shifts
*    IMPORTING
*      et_shifts = lt_shift.

  CALL METHOD me->get_shifts
    EXPORTING
      io_tech_request_context = io_tech_request_context
    IMPORTING
      et_shifts               = lt_shift.

  SORT lt_shift BY vornr.
  DELETE ADJACENT DUPLICATES FROM lt_shift COMPARING vornr.

  SORT lt_project BY equnr.
  DELETE ADJACENT DUPLICATES FROM lt_project COMPARING equnr.

  IF lt_project IS NOT INITIAL.

*--Fetching Terrain details to get area(Shifts)
    SELECT *
      FROM /agri/fmirflo
      INTO TABLE @DATA(lt_fmirflo)
       FOR ALL ENTRIES IN @lt_project
     WHERE equnr EQ @lt_project-equnr.
    IF sy-subrc = 0.

      SORT lt_fmirflo BY equnr tplnr_fl.

      SELECT tplnr_fl, garea, msehi
        FROM /agri/glflot
        INTO TABLE @DATA(lt_glflot)
        FOR ALL ENTRIES IN @lt_fmirflo
        WHERE tplnr_fl EQ @lt_fmirflo-tplnr_fl.
      IF sy-subrc = 0.
        SORT lt_glflot BY tplnr_fl.
      ENDIF.

    ENDIF.

*-- Fetcing the Vazao Nominal Shift from Additional tab
    SELECT equnr, zzvns01, zzvns02, zzvns03,
           zzvns04, zzvns05, zzvns06
      FROM /agri/fmirhdr
      INTO TABLE @DATA(lt_irhdr)
      FOR ALL ENTRIES IN @lt_project
      WHERE equnr EQ @lt_project-equnr.
*    SELECT a~ordno, a~vornr,
*           a~cdate, a~fcontr_hm,
*           a~erzet, b~equnr
*      INTO TABLE @DATA(lt_ordcnf)
*      FROM zabst_ordcnf AS a
*     INNER JOIN zabst_ordhdr AS b
*        ON b~ordno EQ a~ordno
*       FOR ALL ENTRIES IN @lt_project
*     WHERE a~ordno EQ @lt_project-ordno
*       AND a~cdate BETWEEN @lv_pdate
*                       AND @sy-datum
*       AND a~loevm EQ @space.
*    IF sy-subrc = 0.
*      SORT lt_ordcnf BY equnr
**                        vornr
**                        ordno DESCENDING
**                        cdate DESCENDING
*                        fcontr_hm DESCENDING.
**                        erzet DESCENDING.
**      DELETE ADJACENT DUPLICATES FROM lt_ordcnf COMPARING ordno vornr.
*    ENDIF.

  ENDIF. "lt_project

*  SORT lt_project BY equnr.
*  DELETE ADJACENT DUPLICATES FROM lt_project COMPARING equnr.

  SORT lt_irhdr BY equnr.

*--Processing order header data
  LOOP AT lt_project INTO DATA(ls_project).

    CLEAR ls_icontr.
    ls_icontr-equnr = ls_project-equnr.
    ls_icontr-badge = ls_project-badge.

*-- Getting the Vazao Nominal Shift value
    READ TABLE lt_irhdr INTO DATA(sl_irhdr) WITH KEY
                                 equnr = ls_project-equnr
                                 BINARY SEARCH.

    LOOP AT lt_shift INTO DATA(ls_shift).

      ls_icontr-vornr = ls_shift-vornr.

      CLEAR: lv_lmnga,
             lv_tqty.
      READ TABLE lt_fmirflo TRANSPORTING NO FIELDS
                                 WITH KEY equnr = ls_project-equnr
                                 BINARY SEARCH.
      IF sy-subrc EQ 0.
        DATA(lv_tabix) = sy-tabix.
        LOOP AT lt_fmirflo INTO DATA(ls_fmirflo) FROM lv_tabix.
          IF ls_fmirflo-equnr <> ls_project-equnr.
            EXIT.
          ENDIF.

          READ TABLE lt_glflot INTO DATA(ls_glflot)
                                  WITH KEY tplnr_fl = ls_fmirflo-tplnr_fl
                                  BINARY SEARCH.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.

          ls_icontr-gmein = ls_glflot-msehi.
*-- Based on shifts
          CASE ls_shift-vornr.
            WHEN '0010'.
              IF ls_fmirflo-zzcshift1 IS NOT INITIAL.
                lv_lmnga = lv_lmnga + ls_glflot-garea * ( ls_fmirflo-zzcshift1 / 100 ).
                lv_tqty = lv_tqty + ls_glflot-garea.
              ENDIF.
                lv_vnsft = sl_irhdr-zzvns01.
            WHEN '0020'.
              IF ls_fmirflo-zzcshift2 IS NOT INITIAL.
                lv_lmnga = lv_lmnga + ls_glflot-garea * ( ls_fmirflo-zzcshift2 / 100 ).
                lv_tqty = lv_tqty + ls_glflot-garea.
              ENDIF.
              lv_vnsft = sl_irhdr-zzvns02.
            WHEN '0030'.
              IF ls_fmirflo-zzcshift3 IS NOT INITIAL.
                lv_lmnga = lv_lmnga + ls_glflot-garea * ( ls_fmirflo-zzcshift3 / 100 ).
                lv_tqty = lv_tqty + ls_glflot-garea.
              ENDIF.
              lv_vnsft = sl_irhdr-zzvns03.
            WHEN '0040'.
              IF ls_fmirflo-zzcshift4 IS NOT INITIAL.
                lv_lmnga = lv_lmnga + ls_glflot-garea * ( ls_fmirflo-zzcshift4 / 100 ).
                lv_tqty = lv_tqty + ls_glflot-garea.
              ENDIF.
              lv_vnsft = sl_irhdr-zzvns04.
            WHEN '0050'.
              IF ls_fmirflo-zzcshift5 IS NOT INITIAL.
                lv_lmnga = lv_lmnga + ls_glflot-garea * ( ls_fmirflo-zzcshift5 / 100 ).
                lv_tqty = lv_tqty + ls_glflot-garea.
              ENDIF.
              lv_vnsft = sl_irhdr-zzvns05.
            WHEN '0060'.
              IF ls_fmirflo-zzcshift6 IS NOT INITIAL.
                lv_lmnga = lv_lmnga + ls_glflot-garea * ( ls_fmirflo-zzcshift6 / 100 ).
                lv_tqty = lv_tqty + ls_glflot-garea.
              ENDIF.
              lv_vnsft = sl_irhdr-zzvns06.
          ENDCASE.

        ENDLOOP. "lt_fmirflo
      ENDIF. "lt_fmirflo

      ls_icontr-lmnga = lv_lmnga.
      ls_icontr-gamng = lv_tqty.
      ls_icontr-vnsft = lv_vnsft.

*      ls_icontr-fcontr_hm = 1.
*      READ TABLE lt_ordcnf INTO DATA(ls_ordcnf)
*                        WITH KEY equnr = ls_project-equnr
**                                 vornr = ls_shift-vornr
*                        BINARY SEARCH.
*      IF sy-subrc = 0.
*        ls_icontr-fcontr_hm = ls_ordcnf-fcontr_hm.
*      ENDIF.

      APPEND ls_icontr TO et_icontr.
    ENDLOOP. "lt_shift
  ENDLOOP. "lt_project

ENDMETHOD.


METHOD get_numdays.

  DATA : lv_cnval1  TYPE zabs_del_cnval,
         lv_cnval2  TYPE zabs_del_cnval,
         ls_numdays TYPE zabs_s_mirr_numdays.

*--Get variant table data
  CALL METHOD zcl_abs_get_variants=>get_constant_single
    EXPORTING
      iv_mod    = space
      iv_objid  = zcl_abs_abap_maintain=>c_objid_mobile "'MOBC'
      iv_k1val  = zcl_abs_abap_maintain=>c_key_irr_days "'DAYS'
    IMPORTING
      ev_cnval2 = lv_cnval2. "'35'

*-- Get Vazão nominal data
  CALL METHOD zcl_abs_get_variants=>get_constant_single
    EXPORTING
      iv_mod    = space
      iv_objid  = zcl_abs_abap_maintain=>c_objid_mobile "'MOBC'
      iv_k1val  = zcl_abs_abap_maintain=>c_key_irr_vnper "'VNPER'
    IMPORTING
      ev_cnval1 = lv_cnval1. "'10'

  ls_numdays-no_days = lv_cnval2.
  ls_numdays-vnomper = lv_cnval1.
  APPEND ls_numdays TO et_numdays.
  CLEAR ls_numdays.

ENDMETHOD.


METHOD get_projects.

*--Local declarations
  DATA : lv_prevdate          TYPE p0001-begda,
         lv_days              TYPE t5a4a-dlydy,
         lv_cnval             TYPE zabs_del_cnval,
         lv_pdate             TYPE datum,
         lv_bck_mnth          TYPE numc3,
         lv_cnval1            TYPE zabs_del_cnval,
         lv_pv_strdt          TYPE datum,
         lv_pv_enddt          TYPE datum,
         lv_cu_strdt          TYPE datum,
         lv_cu_enddt          TYPE datum,
         lv_tabix             TYPE sy-tabix,
         ls_projects          LIKE LINE OF et_projects,
         lt_forms             TYPE zabs_tty_mirr_plants,
         lo_message_container TYPE REF TO /iwbep/if_message_container,
         ls_message           TYPE /agri/s_gprolog.

  CALL METHOD me->/iwbep/if_mgw_conv_srv_runtime~get_message_container
    RECEIVING
      ro_message_container = lo_message_container.

*--Get variant table data
  CALL METHOD zcl_abs_get_variants=>get_constant_single
    EXPORTING
      iv_mod    = space
      iv_objid  = zcl_abs_abap_maintain=>c_objid_mobile "'MOBC'
      iv_k1val  = zcl_abs_abap_maintain=>c_key_irr_days "'DAYS'
      iv_k2val  = 'LDAY'
    IMPORTING
      ev_cnval2 = lv_cnval. "'2'

  lv_days = lv_cnval.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      date      = sy-datum
      days      = lv_days
      months    = 0
      signum    = '-'
      years     = 0
    IMPORTING
      calc_date = lv_prevdate.

  IF lv_prevdate IS NOT INITIAL.
    CALL FUNCTION 'GET_WEEK_INFO_BASED_ON_DATE'
      EXPORTING
        date   = lv_prevdate
      IMPORTING
        monday = lv_pv_strdt
        sunday = lv_pv_enddt.
  ENDIF.

  IF sy-datum IS NOT INITIAL.
    CALL FUNCTION 'GET_WEEK_INFO_BASED_ON_DATE'
      EXPORTING
        date   = sy-datum
      IMPORTING
        monday = lv_cu_strdt
        sunday = lv_cu_enddt.
  ENDIF.

*--Get variant table data
  CALL METHOD zcl_abs_get_variants=>get_constant_single
    EXPORTING
      iv_mod    = space
      iv_objid  = zcl_abs_abap_maintain=>c_objid_irr_wb "'IRWO'
      iv_k1val  = zcl_abs_abap_maintain=>c_key_irr_mnth "'MNTH'
    IMPORTING
      ev_cnval1 = lv_cnval1. "'1'

  lv_bck_mnth = lv_cnval1.

*--For Final Counter
  CALL FUNCTION 'CCM_GO_BACK_MONTHS'
    EXPORTING
      currdate   = sy-datum
      backmonths = lv_bck_mnth
    IMPORTING
      newdate    = lv_pdate.

*--Calling method to get Plants data
*  CALL METHOD me->get_forms
*    IMPORTING
*      et_forms = lt_forms.

  CALL METHOD me->get_forms
    EXPORTING
      io_tech_request_context = io_tech_request_context
    IMPORTING
      et_forms                = lt_forms.

  IF lt_forms IS NOT INITIAL.
*--Fetching Order Numbers
    SELECT a~ordno, a~werks,
           a~equnr, a~datab,
           a~datbi, b~descr, b~zzhydfac
      INTO TABLE @DATA(lt_ordhdr)
      FROM zabst_ordhdr AS a
     INNER JOIN /agri/fmirhdr AS b
        ON b~equnr EQ a~equnr
       FOR ALL ENTRIES IN @lt_forms
     WHERE a~werks EQ @lt_forms-werks
       AND a~datab GE @lv_pv_strdt
       AND a~datbi LE @lv_cu_enddt
       AND b~kfrst EQ @space.
    IF sy-subrc <> 0.
*-- Give error message if Projects are there
      CLEAR ls_message.
      ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error. "'E'
      ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
      ls_message-msgno = '134'.
      add_messages_to_msg_container( is_message = ls_message ).
    ENDIF.
  ENDIF. "lt_forms

  IF lt_ordhdr IS NOT INITIAL.
    SELECT a~ordno,
           a~fcontr_hm,
           b~equnr
      INTO TABLE @DATA(lt_ordcnf)
      FROM zabst_ordcnf AS a
     INNER JOIN zabst_ordhdr AS b
        ON b~ordno EQ a~ordno
       FOR ALL ENTRIES IN @lt_ordhdr
     WHERE a~ordno EQ @lt_ordhdr-ordno
       AND a~cdate BETWEEN @lv_pdate
                       AND @sy-datum
       AND a~loevm EQ @space.
    IF sy-subrc = 0.
      SORT lt_ordcnf BY equnr
                        fcontr_hm DESCENDING.
    ENDIF.
  ENDIF. "lt_ordhdr

*  SORT lt_ordhdr BY ordno.
*  DATA(lt_ordhdr_temp) = lt_ordhdr.
*  SORT lt_ordhdr_temp BY equnr.
*  DELETE ADJACENT DUPLICATES FROM lt_ordhdr_temp COMPARING equnr.

*  IF lt_ordhdr_temp IS NOT INITIAL.
**--Fetching Terrain Header data
*    SELECT a~equnr, a~tplnr_fl,
*           b~garea, b~msehi
*      INTO TABLE @DATA(lt_tplnr)
*      FROM /agri/fmirflo AS a
*     INNER JOIN /agri/glflot AS b
*        ON b~tplnr_fl EQ a~tplnr_fl
*       FOR ALL ENTRIES IN @lt_ordhdr_temp
*     WHERE a~equnr EQ @lt_ordhdr_temp-equnr.
*    IF sy-subrc = 0.
*      SORT lt_tplnr BY equnr tplnr_fl.
*    ENDIF.
*  ENDIF.

  READ TABLE lt_forms INTO DATA(ls_forms) INDEX 1.

  LOOP AT lt_ordhdr INTO DATA(lwa_ordhdr).

    ls_projects-equnr      = lwa_ordhdr-equnr.
    ls_projects-datab      = lwa_ordhdr-datab.
    ls_projects-datbi      = lwa_ordhdr-datbi.
    ls_projects-descr      = lwa_ordhdr-descr.
    ls_projects-werks      = lwa_ordhdr-werks.
    ls_projects-ordno      = lwa_ordhdr-ordno.
    ls_projects-zzhydfac   = lwa_ordhdr-zzhydfac.
    ls_projects-irrqty_uom = zcl_abs_abap_maintain=>c_uom. "'M3'
    ls_projects-badge      = ls_forms-badge.

    ls_projects-fcontr_hm = 1.
    READ TABLE lt_ordcnf INTO DATA(lwa_ordcnf)
                      WITH KEY equnr = lwa_ordhdr-equnr
*                                 vornr = ls_shift-vornr
                      BINARY SEARCH.
    IF sy-subrc = 0.
      ls_projects-fcontr_hm = lwa_ordcnf-fcontr_hm.
    ENDIF.

*    READ TABLE lt_tplnr TRANSPORTING NO FIELDS
*                              WITH KEY equnr = lwa_ordhdr-equnr
*                              BINARY SEARCH.
*    IF sy-subrc <> 0.
*      CONTINUE.
*    ENDIF.
*    lv_tabix = sy-tabix.
*    LOOP AT lt_tplnr INTO DATA(lwa_tplnr) FROM lv_tabix.
*      IF lwa_tplnr-equnr <> lwa_ordhdr-equnr.
*        EXIT.
*      ENDIF.
*      ls_projects-garea  = ls_projects-garea + lwa_tplnr-garea.
*      ls_projects-msehi  = lwa_tplnr-msehi.
*    ENDLOOP. "lt_tplnr

    APPEND ls_projects TO et_projects.
    CLEAR ls_projects.

  ENDLOOP. "lt_ordhdr

ENDMETHOD.


METHOD get_shifts.

*-- Local Declarations
  DATA : lt_forms             TYPE zabs_tty_mirr_plants,
         lrt_tmatnr           TYPE RANGE OF matnr,
         lrs_tmatnr           LIKE LINE  OF lrt_tmatnr,
         ls_shifts            LIKE LINE OF et_shifts,
         lv_tabix             TYPE sy-tabix,
         lo_message_container TYPE REF TO /iwbep/if_message_container,
         ls_message           TYPE /agri/s_gprolog.

  CALL METHOD me->/iwbep/if_mgw_conv_srv_runtime~get_message_container
    RECEIVING
      ro_message_container = lo_message_container.

*--Get Process/Task Materials from Variant table
  CALL METHOD zcl_abs_get_variants=>get_constant_multiple
    EXPORTING
      iv_mod       = space
      iv_objid     = zcl_abs_abap_maintain=>c_objid_irr_wb "'IRWO'
      iv_k1val     = zcl_abs_abap_maintain=>c_key_irr_po "'IRPO'
    IMPORTING
      et_constants = DATA(lt_constants).

  lrs_tmatnr-sign = zcl_abs_abap_maintain=>c_rsign_include. "'I'
  lrs_tmatnr-option = zcl_abs_abap_maintain=>c_ropt_equal. "'EQ'
  LOOP AT lt_constants INTO DATA(lwa_constants).
    lrs_tmatnr-low = lwa_constants-cnval2.
    APPEND lrs_tmatnr TO lrt_tmatnr.
  ENDLOOP.

*--Calling method to get Plants data
*  CALL METHOD me->get_forms
*    IMPORTING
*      et_forms = lt_forms.

  CALL METHOD me->get_forms
    EXPORTING
      io_tech_request_context = io_tech_request_context
    IMPORTING
      et_forms                = lt_forms.

  IF lt_forms IS NOT INITIAL.
*--Fetching Assignment of Task Lists to Materials data
    SELECT werks,
           plnty,
           plnnr,
           plnal
      FROM mapl
      INTO TABLE @DATA(lt_mapl)
       FOR ALL ENTRIES IN @lt_forms
     WHERE matnr IN @lrt_tmatnr
       AND werks EQ @lt_forms-werks
       AND loekz EQ @space.
    IF sy-subrc <> 0.
      CLEAR ls_message.
      ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error. "'E'
      ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
      ls_message-msgno = '158'.
      add_messages_to_msg_container( is_message = ls_message ).
    ENDIF.
  ENDIF.

  SORT lt_mapl BY plnty plnnr plnal.

  IF lt_mapl IS NOT INITIAL.
*--Fetching Operations data
    SELECT a~plnty, a~plnnr,
           a~plnal, b~vornr,
           b~ltxa1
      INTO TABLE @DATA(lt_plpo)
      FROM plas AS a
     INNER JOIN plpo AS b
        ON b~plnty = a~plnty
       AND b~plnnr = a~plnnr
       AND b~plnkn = a~plnkn
       AND b~zaehl = a~zaehl
       FOR ALL ENTRIES IN @lt_mapl
     WHERE a~plnty EQ @lt_mapl-plnty
       AND a~plnnr EQ @lt_mapl-plnnr
       AND a~plnal EQ @lt_mapl-plnal
       AND a~loekz EQ @space.
    IF sy-subrc = 0.
      SORT lt_plpo BY plnty plnnr plnal vornr.
    ENDIF.
  ENDIF. "lt_mapl

  READ TABLE lt_forms INTO DATA(ls_forms) INDEX 1.

  LOOP AT lt_mapl INTO DATA(ls_mapl).

    READ TABLE lt_plpo TRANSPORTING NO FIELDS
                       WITH KEY plnty = ls_mapl-plnty
                                plnnr = ls_mapl-plnnr
                                plnal = ls_mapl-plnal
                       BINARY SEARCH.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    lv_tabix = sy-tabix.
    LOOP AT lt_plpo INTO DATA(ls_plpo) FROM lv_tabix.

      IF ls_plpo-plnty <> ls_mapl-plnty
      OR ls_plpo-plnnr <> ls_mapl-plnnr
      OR ls_plpo-plnal <> ls_mapl-plnal.
        EXIT.
      ENDIF.

      ls_shifts-werks = ls_mapl-werks.
      ls_shifts-vornr = ls_plpo-vornr.
      ls_shifts-ltxa1 = ls_plpo-ltxa1.
      ls_shifts-badge = ls_forms-badge.
      APPEND ls_shifts TO et_shifts.
      CLEAR ls_shifts.

    ENDLOOP. "lt_plpo
  ENDLOOP. "lt_mapl

  SORT et_shifts BY werks vornr.
  DELETE ADJACENT DUPLICATES FROM et_shifts COMPARING werks vornr.

ENDMETHOD.


METHOD get_supply_materials.

*--Local declaration
  DATA : lt_project           TYPE zabs_tty_mirr_project,
         lt_shift             TYPE zabs_tty_mirr_shiftf4,
         ls_sup_mat           TYPE zabs_s_mirr_supply,
*         lrt_shift            TYPE RANGE OF vornr,
*         lrs_shift            LIKE LINE  OF lrt_shift,
         lv_tabix             TYPE sy-tabix,
         lo_message_container TYPE REF TO /iwbep/if_message_container,
         ls_message           TYPE /agri/s_gprolog.

  CALL METHOD me->/iwbep/if_mgw_conv_srv_runtime~get_message_container
    RECEIVING
      ro_message_container = lo_message_container.

*--Calling method to get Projects data
*  CALL METHOD me->get_projects
*    IMPORTING
*      et_projects = lt_project.

  CALL METHOD me->get_projects
    EXPORTING
      io_tech_request_context = io_tech_request_context
    IMPORTING
      et_projects             = lt_project.

*  lrs_shift-sign = zcl_abs_abap_maintain=>c_rsign_include. "'I'
*  lrs_shift-option = zcl_abs_abap_maintain=>c_ropt_equal. "'EQ'
*  LOOP AT lt_shift INTO DATA(ls_shift).
*    lrs_shift-low = ls_shift-vornr.
*    APPEND lrs_shift TO lrt_shift.
*    CLEAR lrs_shift.
*  ENDLOOP.

*  IF lt_project IS NOT INITIAL.
*
*    SELECT ordno, equnr
*      INTO TABLE @DATA(lt_ordhdr)
*      FROM zabst_ordhdr
*       FOR ALL ENTRIES IN @lt_project
*     WHERE equnr EQ @lt_project-equnr.
*    IF sy-subrc <> 0.
*      CLEAR ls_message.
*      ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error. "'E'
*      ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
*      ls_message-msgno = '160'.
*      add_messages_to_msg_container( is_message = ls_message ).
*    ENDIF.
*
*  ENDIF. "lt_project

  IF lt_project IS NOT INITIAL.

*--Fetching Order Items data
    SELECT ordno, wonum
      FROM zabst_orditm
      INTO TABLE @DATA(lt_orditm)
       FOR ALL ENTRIES IN @lt_project
     WHERE ordno EQ @lt_project-ordno.
    IF sy-subrc = 0.

      SORT lt_orditm BY ordno.
      DELETE ADJACENT DUPLICATES FROM lt_orditm COMPARING ordno.

*--Fetching components details to get materials
      SELECT wonum, vornr,
             contr, matnr,
             esmng, erfme
        INTO TABLE @DATA(lt_supply)
        FROM /agri/fmwocom
         FOR ALL ENTRIES IN @lt_orditm
       WHERE wonum EQ @lt_orditm-wonum.
      IF sy-subrc = 0.
        SORT lt_supply BY wonum.

        SELECT matnr, maktx
          FROM makt
          INTO TABLE @DATA(lt_makt)
           FOR ALL ENTRIES IN @lt_supply
         WHERE matnr EQ @lt_supply-matnr
           AND spras EQ @sy-langu.
        IF sy-subrc = 0.
          SORT lt_makt BY matnr.
        ENDIF.
      ENDIF. "LT_SUPPLY
    ENDIF. "LT_ORDITM

  ENDIF. "lt_ordhdr

  LOOP AT lt_project INTO DATA(ls_project).

    READ TABLE lt_orditm INTO DATA(ls_orditm)
    WITH KEY ordno = ls_project-ordno
    BINARY SEARCH.
    IF sy-subrc = 0.

      READ TABLE lt_supply TRANSPORTING NO FIELDS
                                WITH KEY wonum = ls_orditm-wonum
                                BINARY SEARCH.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      lv_tabix = sy-tabix.
      LOOP AT lt_supply INTO DATA(ls_supply) FROM lv_tabix.

        IF ls_supply-wonum <> ls_orditm-wonum.
          EXIT.
        ENDIF.

        READ TABLE lt_makt INTO DATA(ls_makt)
        WITH KEY matnr = ls_supply-matnr
        BINARY SEARCH.
        IF sy-subrc = 0.
          ls_sup_mat-maktx = ls_makt-maktx.
        ENDIF.

        ls_sup_mat-ordno = ls_project-ordno.
        ls_sup_mat-equnr = ls_project-equnr.
        ls_sup_mat-matnr = ls_supply-matnr.
        ls_sup_mat-esmng = 0.
        ls_sup_mat-vornr = ls_supply-vornr.
        ls_sup_mat-erfme = ls_supply-erfme.
        ls_sup_mat-badge = ls_project-badge.

        APPEND ls_sup_mat TO et_sup_mat.
        CLEAR  ls_sup_mat.

      ENDLOOP. "lt_supply
    ENDIF. "lt_orditm
  ENDLOOP. "lt_ordhdr

  SORT et_sup_mat BY equnr ordno vornr matnr.
  DELETE ADJACENT DUPLICATES FROM et_sup_mat COMPARING equnr ordno vornr matnr.

ENDMETHOD.


METHOD get_user.

*-- Local Declarations
  DATA : lt_filter     TYPE /iwbep/t_mgw_select_option,
         ls_filter     TYPE /iwbep/s_mgw_select_option,
         lv_filter_str TYPE string,
         lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
         ls_user       TYPE zabs_s_mirr_user,
         ltr_badge     TYPE RANGE OF zabs_del_badge,
         lsr_badge     LIKE LINE OF ltr_badge.

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
    ls_user-badge = lsr_badge-low.
  ENDIF.

   APPEND ls_user TO et_user.
   CLEAR ls_user.

ENDMETHOD.


METHOD mirrconfirmation_get_entityset.
ENDMETHOD.


METHOD mirrformset_get_entityset.

*-- Local Declarations
  DATA: lt_forms TYPE zabs_tty_mirr_plants.

*  CALL METHOD me->get_forms
*    IMPORTING
*      et_forms = et_entityset.

  CALL METHOD me->get_forms
    EXPORTING
      io_tech_request_context = io_tech_request_context
    IMPORTING
      et_forms                = et_entityset.

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


METHOD mirrinicontrset_get_entityset.

*--Calling method to get Initial Counter data
*  CALL METHOD me->get_initial_counter
*    IMPORTING
*      et_icontr = et_entityset.

  CALL METHOD me->get_initial_counter
    EXPORTING
      io_tech_request_context = io_tech_request_context
    IMPORTING
      et_icontr               = et_entityset.

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


METHOD mirrnumdaysset_get_entityset.

  CALL METHOD me->get_numdays
    IMPORTING
      et_numdays = et_entityset.

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


METHOD mirrprojectset_get_entityset.

*--Calling method to get Projects data
*  CALL METHOD me->get_projects
*    IMPORTING
*      et_projects = et_entityset.

  CALL METHOD me->get_projects
    EXPORTING
      io_tech_request_context = io_tech_request_context
    IMPORTING
      et_projects             = et_entityset.

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


METHOD mirrshiftf4set_get_entityset.

*--Calling method to get Shifts data
*  CALL METHOD me->get_shifts
*    IMPORTING
*      et_shifts = et_entityset.

  CALL METHOD me->get_shifts
    EXPORTING
      io_tech_request_context = io_tech_request_context
    IMPORTING
      et_shifts               = et_entityset.

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


METHOD mirrsupplymatset_get_entityset.

*--Calling Supply materials data
*  CALL METHOD me->get_supply_materials
*    IMPORTING
*      et_sup_mat = et_entityset.

  CALL METHOD me->get_supply_materials
    EXPORTING
      io_tech_request_context = io_tech_request_context
    IMPORTING
      et_sup_mat              = et_entityset.

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


METHOD mirruserset_get_entityset.

  CALL METHOD me->get_user
    EXPORTING
      io_tech_request_context = io_tech_request_context
    IMPORTING
      et_user                 = et_entityset.

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
