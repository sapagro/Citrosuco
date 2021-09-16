class ZCL_ZABS_MOB_ACM_TRANS_DPC_EXT definition
  public
  inheriting from ZCL_ZABS_MOB_ACM_TRANS_DPC
  create public .

public section.

  types:
    BEGIN OF ty_btch,
        in_licplate TYPE zabs_del_licplate,
        charg       TYPE charg_d,
      END OF ty_btch .
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

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~EXECUTE_ACTION
    redefinition .
protected section.

  methods TRANSBATCHUPDSET_CREATE_ENTITY
    redefinition .
  methods TRANSBATCHUPDSET_GET_ENTITYSET
    redefinition .
  methods TRANSHDRPLATESET_GET_ENTITYSET
    redefinition .
  methods TRANSPLATESET_GET_ENTITYSET
    redefinition .
  methods USERROLESET_GET_ENTITYSET
    redefinition .
private section.

  methods GET_USR_ROLE
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_USR_ROLE type ZABS_TTY_TRP_USR_ROLE
    raising
      /IWBEP/CX_MGW_BTC_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
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
  methods GET_TRANSPLATESET
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_TRANSPLATE type ZCL_ZABS_MOB_ACM_TRANS_MPC=>TT_TRANSPLATE
    raising
      /IWBEP/CX_MGW_BTC_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods FETCH_CARIMBO
    importing
      !IT_TPLNR type /AGRI/T_GLTPLNR
    changing
      !CT_CARIMBO type TT_CARIMBO .
  methods GET_TRANSHDR
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ET_TRANSHDR type ZCL_ZABS_MOB_ACM_TRANS_MPC=>TT_TRANSHDRPLATE .
ENDCLASS.



CLASS ZCL_ZABS_MOB_ACM_TRANS_DPC_EXT IMPLEMENTATION.


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
      WHEN 'ValidateCarimVarTrans'.

        IF it_parameter IS NOT INITIAL.
          READ TABLE it_parameter INTO DATA(ls_parameter)
                WITH KEY name = 'BCARIMBO'.
          IF sy-subrc EQ 0.
            lv_bcarimbo = ls_parameter-value.
          ENDIF.

          CLEAR ls_parameter.
          READ TABLE it_parameter INTO ls_parameter
                WITH KEY name = 'YMATNR'.
          IF sy-subrc EQ 0.
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
             AND loevm       EQ @space
             AND transbord   EQ @abap_true.
          IF sy-subrc EQ 0.
            LOOP AT lt_ilp_btch_exit TRANSPORTING NO FIELDS
                                    WHERE bcarimbo NE lv_bcarimbo.
              EXIT.
            ENDLOOP.
            IF sy-subrc EQ 0.
              ls_entity-msgty = 'W'. " 'E'.
              ls_entity-msg = TEXT-001.
              APPEND ls_entity TO lt_entity.
              CLEAR ls_entity.
            ENDIF.

            LOOP AT lt_ilp_btch_exit TRANSPORTING NO FIELDS
                                    WHERE ymatnr NE lv_ymatnr.
              EXIT.
            ENDLOOP.
            IF sy-subrc EQ 0.
              ls_entity-msgty = 'E'. " 'W'.
              ls_entity-msg = TEXT-002.
              APPEND ls_entity TO lt_entity.
              CLEAR ls_entity.
            ENDIF.
          ENDIF.

          IF lt_entity[] IS INITIAL.
            ls_entity-msgty = 'S'.
            APPEND ls_entity TO lt_entity.
            CLEAR ls_entity.
          ENDIF.

          DESCRIBE TABLE lt_entity LINES DATA(lv_count).
          IF lv_count EQ 2.
            DELETE lt_entity WHERE msgty EQ 'W'.
          ENDIF.

          "Call method copy_data_to_ref and export entity set data
          copy_data_to_ref( EXPORTING is_data = lt_entity
                  CHANGING cr_data = er_data ).

        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD add_messages_to_msg_container.
*-- LOCAL declarations
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


  METHOD get_transhdr.

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
    SELECT prnum, gjahr, werks, lic_plate, semireb1, semireb2
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
          ls_entityset-werks = ls_prhdr-werks.
          ls_entityset-trl_plate = ls_prhdr-semireb1.
          APPEND ls_entityset TO et_transhdr.
        ENDIF.

        IF ls_prhdr-semireb2 IS NOT INITIAL.
          CLEAR ls_entityset.
          ls_entityset-hdr_plate = ls_prhdr-lic_plate.
          ls_entityset-werks = ls_prhdr-werks.
          ls_entityset-trl_plate = ls_prhdr-semireb2.
          APPEND ls_entityset TO et_transhdr.
        ENDIF.
      ENDIF.
    ENDLOOP.

    SORT et_transhdr BY hdr_plate trl_plate.
    DELETE ADJACENT DUPLICATES FROM et_transhdr
                          COMPARING hdr_plate trl_plate.

  ENDMETHOD.


METHOD get_transplateset.

  "Local declarations
  DATA: ls_transplate  TYPE zcl_zabs_mob_acm_trans_mpc=>ts_transplate,
        ltr_farm       TYPE RANGE OF zabs_del_farm,
        lt_tplnr       TYPE /agri/t_gltplnr,
        lsr_farm       LIKE LINE OF ltr_farm,
        ls_tplnr       TYPE /agri/s_gltplnr,
        lt_carimbo     TYPE tt_carimbo,
        lv_carimbo     TYPE zabs_del_tpclht,
        lv_diff        TYPE xfeld,
        ls_message     TYPE /agri/s_gprolog,
        lv_int_tr_prov TYPE zabs_del_trprov.

  DATA(lo_filter)     = io_tech_request_context->get_filter( ).
  DATA(lt_filter)     = lo_filter->get_filter_select_options( ).
  DATA(lv_filter_str) = lo_filter->get_filter_string( ).

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
  LOOP AT lt_filter INTO DATA(ls_filter).
    CASE ls_filter-property.
      WHEN 'FARM'.
        READ TABLE ls_filter-select_options INTO DATA(ls_farm) INDEX 1.
        IF sy-subrc EQ 0.
          DATA(lv_farm) = ls_farm-low.
        ENDIF.

      WHEN 'INT_TR_PROV'.
        READ TABLE ls_filter-select_options INTO DATA(ls_int_tr_prov) INDEX 1.
        IF sy-subrc EQ 0.
          CLEAR lv_int_tr_prov.
          lv_int_tr_prov = ls_int_tr_prov-low.
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

  IF lv_farm IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
      EXPORTING
        input      = lv_farm
      IMPORTING
        output     = lv_farm
      EXCEPTIONS
        not_found  = 1
        not_active = 2
        OTHERS     = 3.
  ENDIF.

  IF lv_int_tr_prov IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_int_tr_prov
      IMPORTING
        output = lv_int_tr_prov.
  ENDIF.

  IF lv_farm IS NOT INITIAL.

    IF lv_int_tr_prov IS NOT INITIAL.
*   Fetch "Internal License Plate" from ZABST_TRAN_TYP
      SELECT farm, int_tr_prov, in_licplate, trans_bord
        FROM zabst_tran_typ
        INTO TABLE @DATA(lt_tran_typ)
       WHERE farm        EQ @lv_farm
         AND int_tr_prov EQ @lv_int_tr_prov
         AND trans_bord  EQ @abap_true.
      IF sy-subrc NE 0.
        EXIT.
      ENDIF.
    ENDIF.

    "Fetch the Plates from ZABS_ILP_BTCH for respective farm
    SELECT in_licplate, charg, farm, tplnr, werks, maktx, ymatnr
      FROM zabs_ilp_btch
      INTO TABLE @DATA(lt_plates)
   FOR ALL ENTRIES IN @lt_tran_typ
     WHERE in_licplate EQ @lt_tran_typ-in_licplate
       AND farm        EQ @lv_farm " @lsr_farm-low
       AND weighbridge EQ @space
       AND loevm       EQ @space
       AND transbord   EQ @abap_true.
    IF sy-subrc EQ 0.

      "Collecting all terrain data
      LOOP AT lt_plates INTO DATA(ls_plates).
        CLEAR ls_tplnr.
        ls_tplnr-tplnr_fl = ls_plates-tplnr.
        APPEND ls_tplnr TO lt_tplnr.
      ENDLOOP.

      DATA(lt_plates_tmp) = lt_plates.
      SORT lt_plates BY in_licplate charg tplnr.
      DELETE ADJACENT DUPLICATES FROM lt_plates COMPARING in_licplate.
      SORT lt_plates_tmp BY in_licplate.
    ENDIF.

    IF lt_tplnr IS NOT INITIAL.
      SORT lt_tplnr BY tplnr_fl.
      DELETE ADJACENT DUPLICATES FROM lt_tplnr COMPARING tplnr_fl.

      "Fetch Carimbo and description
      CALL METHOD me->fetch_carimbo
        EXPORTING
          it_tplnr   = lt_tplnr
        CHANGING
          ct_carimbo = lt_carimbo.
    ENDIF.

    "Filling entity set data
    LOOP AT lt_plates INTO ls_plates.
      ls_transplate-in_licplate = ls_plates-in_licplate.
*      ls_transplate-lifnr       = ls_plates-charg.
      ls_transplate-tplnr       = ls_plates-tplnr.
      ls_transplate-werks       = ls_plates-werks.
      ls_transplate-ymatnr      = ls_plates-ymatnr.
      ls_transplate-maktx       = ls_plates-maktx.

*      ls_transplate-name1       = ls_plates-name1.

      "Checking carimbo value is same or different.
      READ TABLE lt_plates_tmp TRANSPORTING NO FIELDS
                               WITH KEY in_licplate = ls_plates-in_licplate
                               BINARY SEARCH.
      IF sy-subrc EQ 0.
        DATA(lv_tabix) = sy-tabix.
        LOOP AT lt_plates_tmp INTO DATA(ls_plates_tmp) FROM lv_tabix.
          IF ls_plates_tmp-in_licplate NE ls_plates-in_licplate.
            EXIT.
          ENDIF.
          "Fetching the carimbo data
          READ TABLE lt_carimbo INTO DATA(ls_carimbo)
                WITH KEY tplnr = ls_plates_tmp-tplnr
              BINARY SEARCH.
          IF sy-subrc EQ 0.
            IF lv_carimbo IS INITIAL.
              lv_carimbo = ls_carimbo-carimbo.
            ELSE.
              IF lv_carimbo EQ ls_carimbo-carimbo.
                CONTINUE.
              ELSEIF lv_carimbo NE ls_carimbo-carimbo.
                lv_diff = abap_true.
                EXIT.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.

      IF lv_diff IS NOT INITIAL.
        ls_transplate-bcarimbo = '23'.
*        ls_transplate-btoclhtt = ls_carimbo-toclhtt.
      ELSE.
        ls_transplate-bcarimbo = ls_carimbo-carimbo.
        ls_transplate-btoclhtt = ls_carimbo-toclhtt.
      ENDIF.

      APPEND ls_transplate TO et_transplate.
      CLEAR: ls_transplate, lv_carimbo, lv_diff.
    ENDLOOP.

  ENDIF.

ENDMETHOD.


METHOD get_usr_role.

*-- Filters are BADGE and FARM
*-- Validate the FARM in ZABST_TRAN_TYP table

*-- Local Declarations
  DATA : lt_filter     TYPE /iwbep/t_mgw_select_option,
         ls_filter     TYPE /iwbep/s_mgw_select_option,
         lv_filter_str TYPE string,
         lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
         ls_message    TYPE /agri/s_gprolog,
         ltr_badge     TYPE RANGE OF zabs_del_badge,
         lsr_badge     LIKE LINE OF ltr_badge,
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
*        lo_filter->convert_select_option(
*          EXPORTING
*            is_select_option = ls_filter
*          IMPORTING
*            et_select_option = ltr_badge ).
        READ TABLE ls_filter-select_options INTO DATA(ls_badge) INDEX 1.
        IF sy-subrc EQ 0.
**          ls_usr_role-badge = ls_badge-low.
          ls_usr_role-lifnr = ls_badge-low.
        ENDIF.
      WHEN 'FARM'.
*        lo_filter->convert_select_option(
*          EXPORTING
*            is_select_option = ls_filter
*          IMPORTING
*            et_select_option = ltr_farm ).

        READ TABLE ls_filter-select_options INTO DATA(ls_farm) INDEX 1.
        IF sy-subrc EQ 0.
          ls_usr_role-farm = ls_farm-low.
        ENDIF.
*      WHEN 'INT_TR_PROV'.
*        READ TABLE ls_filter-select_options INTO DATA(ls_int_tr_prov) INDEX 1.
*        IF sy-subrc EQ 0.
*          ls_usr_role-lifnr = ls_int_tr_prov-low.
*        ENDIF.
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

  CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
    EXPORTING
      input      = ls_usr_role-farm " lsr_farm-low
    IMPORTING
      output     = ls_usr_role-farm
    EXCEPTIONS
      not_found  = 1
      not_active = 2
      OTHERS     = 3.
  IF sy-subrc NE 0.
    CLEAR : ls_message.
    ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error.
    ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
    ls_message-msgno = '165'.
    add_messages_to_msg_container( iv_exception = abap_true
                                   is_message = ls_message ).
    RETURN.
  ENDIF.

  IF ls_usr_role-farm IS NOT INITIAL.
    SELECT SINGLE farm
      FROM zabst_tran_typ
      INTO @DATA(ls_tran)
      WHERE farm EQ @ls_usr_role-farm.

    IF sy-subrc NE 0.
      CLEAR : ls_message.
      ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error.
      ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
      ls_message-msgno = '165'.
      add_messages_to_msg_container( is_message   = ls_message
                                     iv_exception = abap_true ).
      RETURN.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = ls_usr_role-lifnr
    IMPORTING
      output = ls_usr_role-lifnr.
  IF sy-subrc NE 0.
    CLEAR : ls_message.
    ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error.
    ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
    ls_message-msgno = '164'.
    add_messages_to_msg_container( iv_exception = abap_true
                                   is_message = ls_message ).
    RETURN.
  ENDIF.

  IF ls_usr_role-lifnr IS NOT INITIAL.
    SELECT SINGLE int_tr_prov
      FROM zabst_tran_typ
      INTO @DATA(ls_tr_prov)
      WHERE int_tr_prov EQ @ls_usr_role-lifnr
        AND trans_bord  EQ @abap_true.

    IF sy-subrc NE 0.
      CLEAR : ls_message.
      ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error.
      ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
      ls_message-msgno = '164'.
      add_messages_to_msg_container( is_message   = ls_message
                                     iv_exception = abap_true ).
      RETURN.
    ENDIF.
  ENDIF.

  CLEAR ls_usr_role.

ENDMETHOD.


METHOD transbatchupdset_create_entity.

  "Update the batches and update the records in ZABS_ILP_BTCH
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
        lt_bapi_messages   TYPE bapiret2_tt,

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
  FIELD-SYMBOLS : <fv_value> TYPE any.

  DATA: ls_entity LIKE er_entity.

  io_data_provider->read_entry_data( IMPORTING es_data = ls_entity ).

  er_entity = ls_entity.

  SELECT *
    FROM zabs_ilp_btch
    INTO TABLE lt_ilp_btch
  WHERE ( in_licplate EQ ls_entity-in_licplate
    AND weighbridge   EQ space
    AND loevm         EQ space
    AND transbord     EQ abap_true )
    OR ( in_licplate  EQ ls_entity-hdr_plate
     AND weighbridge  EQ abap_true
     AND loevm        EQ space
     AND transbord    EQ abap_true ).
  IF sy-subrc EQ 0.
    SORT lt_ilp_btch BY in_licplate charg.
  ENDIF.

  SELECT tpclht, toclhtt
    FROM yotpclht
    INTO TABLE @DATA(lt_yotpclht).
  IF sy-subrc = 0 .
    SORT lt_yotpclht BY tpclht.
  ENDIF.

  LOOP AT lt_ilp_btch INTO ls_ilp_btch
                     WHERE in_licplate = ls_entity-in_licplate.

*--Calling FM to get Characteristics
    CALL FUNCTION 'VBWS_MARA_CLASSIFICATION_GET'
      EXPORTING
        i_matnr              = ls_ilp_btch-tmatnr
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

    CONCATENATE ls_ilp_btch-tmatnr ls_ilp_btch-charg
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
        return             = lt_bapi_messages.

    IF ls_entity-hdr_plate IS NOT INITIAL.
      CLEAR ls_allocvalueschar.
      ls_allocvalueschar-charact    = 'RC_FR_PLACA_CM'.
      ls_allocvalueschar-value_char = ls_entity-hdr_plate.
      APPEND ls_allocvalueschar TO lt_allocvalueschar.
    ENDIF.

    IF ls_entity-trl_plate IS NOT INITIAL.
      CLEAR ls_allocvalueschar.
      ls_allocvalueschar-charact    = 'RC_FR_PLACA_CR'.
      ls_allocvalueschar-value_char = ls_entity-trl_plate.
      APPEND ls_allocvalueschar TO lt_allocvalueschar.
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
        return             = lt_bapi_messages.

    READ TABLE lt_bapi_messages INTO ls_messages WITH KEY type = zcl_abs_abap_maintain=>c_msgty_error. "'E'
    IF sy-subrc = 0.
      EXIT.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

      REFRESH lt_bapi_messages.

      CLEAR ls_messages.
      ls_messages-id = zcl_abs_abap_maintain=>c_custom_msg_class.
      ls_messages-number = '114'.
      ls_messages-type = zcl_abs_abap_maintain=>c_msgty_success. "'S'
      APPEND ls_messages TO lt_bapi_messages.

      ls_ilp_btch-aenam = sy-uname.
      ls_ilp_btch-aedat = sy-datum.
      ls_ilp_btch-aezet = sy-uzeit.
      ls_ilp_btch-loevm = abap_true.
      ls_ilp_btch-chimei  = ls_entity-imei.
      ls_ilp_btch-chbadge = ls_entity-badge.
      APPEND ls_ilp_btch TO lt_update.


      READ TABLE lt_ilp_btch TRANSPORTING NO FIELDS
            WITH KEY in_licplate = ls_entity-hdr_plate
                     charg       = ls_ilp_btch-charg
            BINARY SEARCH.
      IF sy-subrc NE 0.
        "Inserting when Transbord is x
        ls_insert-in_licplate = ls_entity-hdr_plate.
        ls_insert-trl_plate   = ls_entity-trl_plate.
        ls_insert-charg       = ls_ilp_btch-charg.
        ls_ilp_btch-farm      = ls_ilp_btch-farm.
        ls_insert-farm        = ls_ilp_btch-farm.
        ls_insert-bin         = ls_ilp_btch-bin.
        ls_insert-tmatnr      = ls_ilp_btch-tmatnr.
        ls_insert-carimbo     = ls_ilp_btch-carimbo.
        READ TABLE lt_yotpclht INTO DATA(ls_yotpclht)
                 WITH KEY tpclht = ls_ilp_btch-carimbo BINARY SEARCH.
        IF sy-subrc = 0.
          ls_insert-toclhtt    = ls_yotpclht-toclhtt.
        ENDIF.
        ls_insert-zzturma     = ls_ilp_btch-zzturma.
        ls_insert-tplnr       = ls_ilp_btch-tplnr.
        ls_insert-weighbridge = abap_true.
        ls_insert-ersda       = ls_ilp_btch-ersda.
        ls_insert-ertme       = ls_ilp_btch-ertme.
        ls_insert-baggp       = ls_ilp_btch-baggp.
        ls_insert-mackey      = ls_ilp_btch-mackey.
        ls_insert-ldcde       = ls_ilp_btch-ldcde.
        ls_insert-werks       = ls_ilp_btch-werks.
        ls_insert-ernam       = sy-uname.
        ls_insert-erdat       = sy-datum.
        ls_insert-erzet       = sy-uzeit.
        ls_insert-crimei      = ls_entity-imei.
        ls_insert-crbadge     = ls_entity-badge.
        ls_insert-bcarimbo    = ls_entity-bcarimbo.
        CLEAR ls_yotpclht.
        READ TABLE lt_yotpclht INTO ls_yotpclht
                WITH KEY tpclht = ls_entity-bcarimbo BINARY SEARCH.
        IF sy-subrc = 0.
          ls_insert-btoclhtt    = ls_yotpclht-toclhtt.
        ENDIF.
        ls_insert-ymatnr      = ls_ilp_btch-ymatnr.
        ls_insert-maktx       = ls_ilp_btch-maktx.
        ls_insert-transbord   = ls_ilp_btch-transbord.

        ls_ilp_btch-ibag_no   = ls_ilp_btch-ibag_no.
        ls_ilp_btch-fbag_no   = ls_ilp_btch-fbag_no.
        ls_ilp_btch-tot_qty   = ls_ilp_btch-tot_qty.
        ls_ilp_btch-maktx     = ls_ilp_btch-maktx.
        ls_ilp_btch-ymatnr    = ls_ilp_btch-ymatnr.
        ls_ilp_btch-toclhtt   = ls_ilp_btch-toclhtt.
        ls_ilp_btch-bcarimbo  = ls_entity-bcarimbo.
        ls_ilp_btch-btoclhtt  = ls_entity-btoclhtt.
        ls_ilp_btch-bdate     = ls_ilp_btch-bdate.
        ls_ilp_btch-btime     = ls_ilp_btch-btime.
        ls_ilp_btch-transbord = ls_ilp_btch-transbord.
        APPEND ls_insert TO lt_insert.
        CLEAR ls_insert.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CLEAR ls_yotpclht.
  IF ls_entity-bcarimbo = '23'.
    LOOP AT lt_ilp_btch INTO ls_ilp_btch
                       WHERE in_licplate = ls_entity-hdr_plate
                         AND bcarimbo NE ls_entity-bcarimbo.
      READ TABLE lt_yotpclht INTO ls_yotpclht
              WITH KEY tpclht = ls_entity-bcarimbo BINARY SEARCH.
      ls_ilp_btch-bcarimbo = ls_entity-bcarimbo.
      ls_ilp_btch-btoclhtt = ls_yotpclht-toclhtt. "lv_toclhtt.
      ls_ilp_btch-aenam = sy-uname.
      ls_ilp_btch-aedat = sy-datum.
      ls_ilp_btch-aezet = sy-uzeit.
      ls_ilp_btch-chimei  = ls_entity-imei.
      ls_ilp_btch-chbadge = ls_entity-badge.
      APPEND ls_ilp_btch TO lt_update.
      CLEAR: ls_ilp_btch, ls_yotpclht.
    ENDLOOP.
  ENDIF.

  IF lt_update IS NOT INITIAL.
    UPDATE zabs_ilp_btch FROM TABLE lt_update.
  ENDIF.

  IF lt_insert IS NOT INITIAL.
    INSERT zabs_ilp_btch FROM TABLE lt_insert.
  ENDIF.

  COMMIT WORK.

  "Add the messages to containersa
  add_messages_to_msg_container( iv_exception = abap_true
                             it_bapi_messages = lt_bapi_messages ).

ENDMETHOD.


METHOD transbatchupdset_get_entityset.
ENDMETHOD.


METHOD transhdrplateset_get_entityset.

*-- Plant is filter and copy same code from harvest app in BEXITHDRPLATESET_GET_ENTITYSET
  CALL METHOD me->get_transhdr
    EXPORTING
      io_tech_request_context = io_tech_request_context
    IMPORTING
      et_transhdr             = et_entityset.

ENDMETHOD.


METHOD transplateset_get_entityset.

*-- Filter is FARM and get the Plates from ZABS_ILP_BTCH
  CALL METHOD me->get_transplateset
    EXPORTING
      io_tech_request_context = io_tech_request_context
    IMPORTING
      et_transplate           = et_entityset.

ENDMETHOD.


METHOD userroleset_get_entityset.

  CALL METHOD me->get_usr_role
    EXPORTING
      io_tech_request_context = io_tech_request_context
    IMPORTING
      et_usr_role             = et_entityset.

ENDMETHOD.
ENDCLASS.
