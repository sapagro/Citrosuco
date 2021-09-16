class ZCL_ZABS_GIS_IRRG_DPC_EXT definition
  public
  inheriting from ZCL_ZABS_GIS_IRRG_DPC
  create public .

public section.
protected section.

  methods DEPTHINDSET_GET_ENTITYSET
    redefinition .
  methods IRRANALYTICSSET_GET_ENTITYSET
    redefinition .
  methods IRRFORMSET_GET_ENTITYSET
    redefinition .
  methods IRRGISTOKENSET_GET_ENTITYSET
    redefinition .
  methods IRRORDERSET_CREATE_ENTITY
    redefinition .
  methods IRRPROJECTSET_GET_ENTITYSET
    redefinition .
  methods IRRSHIFTF4SET_GET_ENTITYSET
    redefinition .
  methods IRRTERRAINSSET_GET_ENTITYSET
    redefinition .
  methods EQUIP_DETSET_GET_ENTITYSET
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
ENDCLASS.



CLASS ZCL_ZABS_GIS_IRRG_DPC_EXT IMPLEMENTATION.


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


  METHOD depthindset_get_entityset.

**local decleration
    DATA: lv_itm_tabname TYPE dd02l-tabname,
          ls_entityset   TYPE zabs_s_irr_depth_ind.

**Fetching Depth Indicators
    SELECT *
      FROM zabst_vtcitm INTO TABLE @DATA(lt_vtcitm)
     WHERE objid EQ @zcl_abs_abap_maintain=>c_objid_mobl
       AND k1val EQ @zcl_abs_abap_maintain=>c_key_girr
       AND k2val EQ @zcl_abs_abap_maintain=>c_key_dind.

    LOOP AT lt_vtcitm INTO DATA(ls_vtcitm).
      ls_entityset-depth_ind      = ls_vtcitm-k3val.
      ls_entityset-depth_ind_desc = ls_vtcitm-cnval1.
      APPEND ls_entityset TO et_entityset.
      CLEAR ls_vtcitm.
    ENDLOOP.

  ENDMETHOD.


  METHOD equip_detset_get_entityset.

    DATA: lrt_tplnrfl  TYPE RANGE OF /agri/gltplnr_fl,
          lrt_equnr    TYPE RANGE OF /agri/glequnr,
          ls_entityset LIKE LINE OF et_entityset.

    CONSTANTS:lc_irtyp  TYPE /agri/fmirtyp VALUE '001'.

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
        WHEN 'TPLNR_FL'.
          lo_filter->convert_select_option(
            EXPORTING
              is_select_option = ls_filter
            IMPORTING
              et_select_option = lrt_tplnrfl ).

        WHEN 'EQUNR'.
          lo_filter->convert_select_option(
            EXPORTING
              is_select_option = ls_filter
            IMPORTING
              et_select_option = lrt_equnr ).

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

    SELECT equnr,
           tplnr_fl
           FROM /agri/fmirflo
           INTO TABLE @DATA(lt_irflo)
           WHERE equnr IN @lrt_equnr
           AND tplnr_fl IN @lrt_tplnrfl.

    IF lt_irflo IS NOT INITIAL.
      SELECT equnr, irtyp, descr,
             zzhydfac, weekend, lamina, zzrefcty,
             zzvns01, zzvns02, zzvns03,
             zzvns04, zzvns05, zzvns06
             FROM /agri/fmirhdr
             INTO TABLE @DATA(lt_irhdr)
             FOR ALL ENTRIES IN @lt_irflo
             WHERE equnr EQ @lt_irflo-equnr
               AND irtyp EQ @lc_irtyp.
    ENDIF.

    SORT: lt_irflo BY equnr tplnr_fl,
          lt_irhdr BY equnr.

    LOOP AT lt_irflo INTO DATA(sl_irflo).
      ls_entityset-equnr    = sl_irflo-equnr.
      ls_entityset-tplnr_fl = sl_irflo-tplnr_fl.
      READ TABLE lt_irhdr INTO DATA(sl_irhdr) WITH KEY
                                              equnr = sl_irflo-equnr
                                              BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_entityset-irtyp    = sl_irhdr-irtyp.
        ls_entityset-descr    = sl_irhdr-descr.
        ls_entityset-zzhydfac = sl_irhdr-zzhydfac.
        ls_entityset-weekend  = sl_irhdr-weekend.
        ls_entityset-lamina   = sl_irhdr-lamina.
        ls_entityset-zzrefcty = sl_irhdr-zzrefcty.
        ls_entityset-zzvns01  = sl_irhdr-zzvns01.
        ls_entityset-zzvns02  = sl_irhdr-zzvns02.
        ls_entityset-zzvns03  = sl_irhdr-zzvns03.
        ls_entityset-zzvns04  = sl_irhdr-zzvns04.
        ls_entityset-zzvns05  = sl_irhdr-zzvns05.
        ls_entityset-zzvns06  = sl_irhdr-zzvns06.
      else.
        CONTINUE.
      ENDIF.
      APPEND ls_entityset TO et_entityset.
      CLEAR: sl_irflo, sl_irhdr, ls_entityset.
    ENDLOOP.

  ENDMETHOD.


METHOD irranalyticsset_get_entityset.

*-- Local Declarations
  DATA : lt_filter     TYPE /iwbep/t_mgw_select_option,
         ls_filter     TYPE /iwbep/s_mgw_select_option,
         lv_filter_str TYPE string,
         lv_tabix      TYPE sy-tabix,
         lv_atflv      TYPE p DECIMALS 3,
         lv_datum      TYPE datum,
         lv_hyd_bal    TYPE zabs_del_hydbal,
         lv_field_cap  TYPE zabs_del_fcap,
         lv_date_mod   TYPE char12,
         lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
         lrt_werks     TYPE RANGE OF werks_d,
         lrs_werks     LIKE LINE OF lrt_werks,
         lrt_date      TYPE RANGE OF datum,
         lrs_date      LIKE LINE OF lrt_date,
         lrt_vornr     TYPE RANGE OF vornr,
         lrs_vornr     LIKE LINE OF lrt_vornr,
         lrt_atyp      TYPE RANGE OF zabs_del_atyp,
         lrs_atyp      LIKE LINE OF lrt_atyp,
         lrt_depthind  TYPE RANGE OF zabs_del_dpt_ind,
         lrt_equnr     TYPE RANGE OF /agri/glequnr,
         lrs_depthind  LIKE LINE OF lrt_depthind,
         ls_message    TYPE /agri/s_gprolog,

         ls_entityset  LIKE LINE OF et_entityset.

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
            et_select_option = lrt_werks ).
      WHEN 'DATE'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_date ).

        READ TABLE lrt_date INTO lrs_date INDEX 1.
        IF sy-subrc = 0.
          lv_datum = lrs_date-low.
        ENDIF.

      WHEN 'VORNR'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_vornr ).
      WHEN 'ATYP'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_atyp ).
      WHEN 'DEPTH_IND'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_depthind ).

      WHEN 'EQUNR'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_equnr ).

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

  READ TABLE lrt_equnr INTO DATA(ls_eqnr) INDEX 1.
  IF ls_eqnr-low IS INITIAL.
*--Fetching Equipment header and Equipment master plants data
    SELECT a~equnr,
           b~werks
      FROM /agri/fmirhdr AS a
     INNER JOIN /agri/fmirwrk AS b
        ON a~equnr = b~equnr
      INTO TABLE @DATA(lt_equnr)
     WHERE a~irtyp EQ @zcl_abs_abap_maintain=>c_irrtyp "'001'
       AND a~kfrst EQ @space
       AND b~werks IN @lrt_werks.
    IF sy-subrc <> 0.
      RETURN.
*    LOOP AT lt_equnr INTO DATA(lwa_equnr).
**-- Give error message
*      CLEAR ls_message.
*      ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error. "'E'
*      ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
*      ls_message-msgno = '147'.
*      ls_message-msgv1 =  lwa_equnr-werks.
*      add_messages_to_msg_container( is_message = ls_message ).
*    ENDLOOP.
    ENDIF.

  ELSEIF ls_eqnr-low IS NOT INITIAL.
*--Fetching Equipment header and Equipment master plants data
    SELECT a~equnr
           b~werks
      FROM /agri/fmirhdr AS a
     INNER JOIN /agri/fmirwrk AS b
        ON a~equnr = b~equnr
      INTO TABLE lt_equnr
     WHERE a~equnr IN lrt_equnr
       AND a~irtyp EQ zcl_abs_abap_maintain=>c_irrtyp "'001'
       AND a~kfrst EQ space
       AND b~werks IN lrt_werks.

  ENDIF.

  SORT lt_equnr BY equnr.

**Fetching Depth Indicators
  SELECT SINGLE *
    FROM zabst_vtcitm INTO @DATA(ls_depth_ind)
   WHERE k3val IN @lrt_depthind.

*--Fetching Terrains for particular Project
  SELECT equnr,
         tplnr_fl,
         zzcshift1, zzcshift2, zzcshift3,
         zzcshift4, zzcshift5, zzcshift6
    FROM /agri/fmirflo
    INTO TABLE @DATA(lt_tplnr)
     FOR ALL ENTRIES IN @lt_equnr
   WHERE equnr EQ @lt_equnr-equnr.
  IF sy-subrc = 0.
    IF lrt_vornr IS NOT INITIAL.
      READ TABLE lrt_vornr INTO lrs_vornr INDEX 1.
      IF lrs_vornr-low EQ '0010'.
        DELETE lt_tplnr WHERE zzcshift1 EQ 0.
      ELSEIF lrs_vornr-low EQ '0020'.
        DELETE lt_tplnr WHERE zzcshift2 EQ 0.
      ELSEIF lrs_vornr-low EQ '0030'.
        DELETE lt_tplnr WHERE zzcshift3 EQ 0.
      ELSEIF lrs_vornr-low EQ '0040'.
        DELETE lt_tplnr WHERE zzcshift4 EQ 0.
      ELSEIF lrs_vornr-low EQ '0050'.
        DELETE lt_tplnr WHERE zzcshift5 EQ 0.
      ELSEIF lrs_vornr-low EQ '0060'.
        DELETE lt_tplnr WHERE zzcshift6 EQ 0.
      ENDIF.
    ELSE.
      DELETE lt_tplnr WHERE zzcshift1 EQ 0 AND
                            zzcshift2 EQ 0 AND
                            zzcshift3 EQ 0 AND
                            zzcshift4 EQ 0 AND
                            zzcshift5 EQ 0 AND
                            zzcshift6 EQ 0.
    ENDIF.
    SORT lt_tplnr BY equnr tplnr_fl.
  ENDIF. "lt_tplnr

  READ TABLE lrt_atyp INTO lrs_atyp INDEX 1.
  IF lrs_atyp-low EQ zcl_abs_abap_maintain=>c_irrflag_tensio "'T'
    AND lt_tplnr IS NOT INITIAL.

    SELECT a~tplnr_fl,
           b~equnr
      INTO TABLE @DATA(lt_equnr_3)
      FROM /agri/fmirflo AS a
     INNER JOIN /agri/fmirhdr AS b
        ON a~equnr = b~equnr
       FOR ALL ENTRIES IN @lt_tplnr
     WHERE a~tplnr_fl EQ @lt_tplnr-tplnr_fl
       AND b~irtyp    EQ @zcl_abs_abap_maintain=>c_irrtyp_003 "'003'
       AND b~kfrst    EQ @space.
    IF sy-subrc = 0.

      SORT lt_equnr_3 BY tplnr_fl.

*--Fetching Terrain Attribute Values to get TENSIOMETRIA value
      SELECT mdocm, tplnr_fl,
             equnr, mpgrp,
             mdate, mtime
        INTO TABLE @DATA(lt_glmdhdr)
        FROM /agri/glmdhdr
         FOR ALL ENTRIES IN @lt_equnr_3
*       WHERE equnr    EQ @lt_equnr_3-equnr
       WHERE tplnr_fl EQ @lt_equnr_3-tplnr_fl
         AND mpgrp    EQ @zcl_abs_abap_maintain=>c_class_tensio "'TENSIOMETRIA'
*         AND mdate    IN @lrt_date
         AND kfrst    EQ @space
         AND canceled EQ @space.
      IF sy-subrc = 0.

*        SORT lt_glmdhdr BY mdocm equnr mpgrp mdate
*                           mtime DESCENDING.
        SORT lt_glmdhdr BY tplnr_fl mdate DESCENDING.
        DELETE ADJACENT DUPLICATES FROM lt_glmdhdr
                         COMPARING tplnr_fl.

*--Fetching measurement attribute data
        SELECT a~mdocm, a~atwrt,
               a~atflv, b~atinn, b~atnam
          FROM /agri/glmdatv AS a
          JOIN cabn          AS b
            ON b~atinn = a~atinn
          INTO TABLE @DATA(lt_glmdatv)
           FOR ALL ENTRIES IN @lt_glmdhdr
         WHERE a~mdocm EQ @lt_glmdhdr-mdocm
           AND b~atnam EQ @ls_depth_ind-cnval2. " @zcl_abs_abap_maintain=>c_charact_tensio. "'TENSAO_SOLO'
        IF sy-subrc = 0.

          SORT lt_glmdatv BY mdocm atnam.

        ENDIF. "Lt_glmdatv
      ENDIF. "lt_glmdhdr
    ENDIF.  "lt_equnr_3

  ELSEIF lrs_atyp-low NE zcl_abs_abap_maintain=>c_irrflag_tensio. "'T'

*--Fetching Irrigation Order Number
    SELECT ordno,
           equnr
      FROM zabst_ordhdr
      INTO TABLE @DATA(lt_ordhdr)
       FOR ALL ENTRIES IN @lt_equnr
     WHERE equnr EQ @lt_equnr-equnr
       AND datab LE @lv_datum
       AND datbi GE @lv_datum.
    IF sy-subrc = 0.

      SORT lt_ordhdr BY equnr.

*CONCATENATE lv_datum+0(4) lv_datum+4(2) lv_datum+6(2)
*    INTO lv_date_mod
*    SEPARATED BY '.' .

*--Fetching Irrigation monitor data
      SELECT ordno,
             werks,
             equnr,
             irrdate,
             shift,
             hyd_bal,
             cfield_cap
        FROM zabst_irrmon
        INTO TABLE @DATA(lt_irrmon)
         FOR ALL ENTRIES IN @lt_ordhdr
       WHERE ordno   EQ @lt_ordhdr-ordno
         AND werks   IN @lrt_werks
         AND equnr   EQ @lt_ordhdr-equnr
*         AND irrdate EQ @lv_date_mod
         AND irrdate EQ @lv_datum
         AND shift   IN @lrt_vornr
         AND loevm   EQ @abap_false.

      IF sy-subrc = 0.
        SORT lt_irrmon BY ordno werks equnr irrdate shift.
      ENDIF. "lt_irrmon
    ENDIF. "lt_ordhdr
  ENDIF. "LRT_ATYP

*--Processing data
  LOOP AT lt_equnr INTO DATA(ls_equnr).

    CLEAR :ls_entityset,
           lv_hyd_bal,
           lv_field_cap.

    READ TABLE lt_tplnr TRANSPORTING NO FIELDS
                            WITH KEY equnr = ls_equnr-equnr
                            BINARY SEARCH.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    lv_tabix = sy-tabix.

*--Processing Order Header Data
    READ TABLE lt_ordhdr INTO DATA(ls_ordhdr)
    WITH KEY equnr = ls_equnr-equnr
    BINARY SEARCH.
    IF sy-subrc = 0.

*--Processing Custom table data
      READ TABLE lt_irrmon INTO DATA(ls_irrmon)
      WITH KEY ordno = ls_ordhdr-ordno
      BINARY SEARCH.
      IF sy-subrc = 0.
        CLEAR lrs_atyp.

        READ TABLE lrt_atyp INTO lrs_atyp INDEX 1.
        IF sy-subrc = 0
       AND lrs_atyp-low EQ zcl_abs_abap_maintain=>c_irrflag_hydric. "'H'
          lv_hyd_bal = ls_irrmon-hyd_bal.
        ELSEIF sy-subrc = 0
        AND lrs_atyp-low EQ zcl_abs_abap_maintain=>c_irrflag_field_cap. "'F'
          lv_field_cap = ls_irrmon-cfield_cap.
        ENDIF. "lrs_atyp_LOW

      ENDIF. "lt_irrmon
    ENDIF. "lt_ordhdr

    LOOP AT lt_tplnr INTO DATA(ls_tplnr) FROM lv_tabix.

      IF ls_tplnr-equnr <> ls_equnr-equnr.
        EXIT.
      ENDIF.

      ls_entityset-equnr     = ls_equnr-equnr.
      ls_entityset-tplnr_fl  = ls_tplnr-tplnr_fl.
      ls_entityset-hyd_bal   = lv_hyd_bal.
      ls_entityset-field_cap = lv_field_cap.

      READ TABLE lrt_atyp INTO lrs_atyp INDEX 1.
      IF sy-subrc EQ 0 AND lrs_atyp-low EQ zcl_abs_abap_maintain=>c_irrflag_tensio. "'T'

*--Building Tensiometria
        READ TABLE lt_equnr_3 TRANSPORTING NO FIELDS
                                      WITH KEY tplnr_fl = ls_tplnr-tplnr_fl
                                      BINARY SEARCH.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        lv_tabix = sy-tabix.
        LOOP AT lt_equnr_3 INTO DATA(ls_equnr_3) FROM lv_tabix.

          IF ls_tplnr-tplnr_fl <> ls_equnr_3-tplnr_fl.
            EXIT.
          ENDIF.

          READ TABLE lt_glmdhdr INTO DATA(ls_glmdhdr)
          WITH KEY tplnr_fl = ls_equnr_3-tplnr_fl
*                   equnr = ls_equnr_3-equnr
                   mpgrp = zcl_abs_abap_maintain=>c_class_tensio "'TENSIOMETRIA'
                   BINARY SEARCH.
          IF sy-subrc = 0.
            ls_entityset-mdocm = ls_glmdhdr-mdocm.
            READ TABLE lt_glmdatv INTO DATA(ls_glmdatv)
                    WITH KEY mdocm = ls_glmdhdr-mdocm
                             atnam = ls_depth_ind-cnval2 " zcl_abs_abap_maintain=>c_charact_tensio "'TENSAO_SOLO'
                             BINARY SEARCH.
            IF sy-subrc = 0.

              IF ls_glmdatv-atwrt IS NOT INITIAL.

                ls_entityset-tensio = ls_glmdatv-atwrt.
                EXIT.

              ELSEIF ls_glmdatv-atflv IS NOT INITIAL.

                lv_atflv = ls_glmdatv-atflv.
                ls_entityset-tensio =  lv_atflv.
                EXIT.
              ENDIF. "lS_glmdatv-atwrt

            ENDIF. "lt_glmdatv
          ENDIF. "lt_glmdhdr
          CLEAR: ls_glmdhdr, ls_glmdatv.
        ENDLOOP. "lt_equnr_3

      ENDIF. "LRT_ATYP

      APPEND ls_entityset TO et_entityset.
      CLEAR ls_entityset.

    ENDLOOP. "lt_tplnr

  ENDLOOP. "lt_equnr

ENDMETHOD.


METHOD irrformset_get_entityset.

*-- Local Declarations
  DATA: ls_message TYPE /agri/s_gprolog.

*-- Fetch the Plants
  SELECT werks,
         name1
    FROM t001w
    INTO TABLE @DATA(lt_t001w).
  IF sy-subrc EQ 0.
    MOVE-CORRESPONDING lt_t001w TO et_entityset.
  ENDIF.

  IF et_entityset IS INITIAL.
*-- Give error message if no turma maintained for the incharge
    CLEAR ls_message.
    ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error. "'E'
    ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
    ls_message-msgno = '133'.
*    ls_message-msgv1 =  ls_usr_emp-pernr.
    add_messages_to_msg_container( is_message = ls_message ).
  ENDIF.

ENDMETHOD.


METHOD irrgistokenset_get_entityset.

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

ENDMETHOD.


METHOD irrorderset_create_entity.

*-- Local Declarations
  DATA: ls_irrorder TYPE zcl_zabs_gis_irrg_mpc=>ts_irrorder,
        ls_messages TYPE /agri/s_gprolog,
        lt_messages TYPE /agri/t_gprolog.

  io_data_provider->read_entry_data( IMPORTING es_data = ls_irrorder ).

*--Validating Irrigation Order Number
  SELECT ordno
    FROM zabst_ordhdr
    INTO TABLE @DATA(lt_ordhdr)
   WHERE werks EQ @ls_irrorder-werks
     AND equnr EQ @ls_irrorder-equnr
     AND datab LE @ls_irrorder-date
     AND datbi GE @ls_irrorder-date.
  IF sy-subrc = 0.
    LOOP AT lt_ordhdr INTO DATA(ls_ordhdr).
*-- Give error message if Order Number already exists
      CLEAR ls_messages.
      ls_messages-msgty = zcl_abs_abap_maintain=>c_msgty_error. "'E'
      ls_messages-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
      ls_messages-msgno = '138'.
      ls_messages-msgv1 =  ls_ordhdr-ordno.
      ls_messages-msgv2 =  ls_irrorder-equnr.
      APPEND ls_messages TO lt_messages.
    ENDLOOP. "lt_ordhdr
  ENDIF.

  IF lt_messages IS INITIAL.
*-- Create Irrigation Order
    CALL FUNCTION 'ZABS_FM_WO_CREATION'
      EXPORTING
        i_farm      = ls_irrorder-werks
        i_equnr     = ls_irrorder-equnr
        i_date      = ls_irrorder-date
      IMPORTING
        et_messages = lt_messages
        ev_ordno    = ls_irrorder-ordno.

    er_entity = ls_irrorder.
  ENDIF.

*-- Add the messages to containersa
  add_messages_to_msg_container( iv_exception = abap_true
                                 it_messages = lt_messages ).

ENDMETHOD.


METHOD irrprojectset_get_entityset.

*-- Local Declarations
  DATA : lt_filter     TYPE /iwbep/t_mgw_select_option,
         ls_filter     TYPE /iwbep/s_mgw_select_option,
         lv_filter_str TYPE string,
         lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
         lrt_werks     TYPE RANGE OF werks_d,
         lrs_werks     LIKE LINE OF lrt_werks,
         ls_message    TYPE /agri/s_gprolog.

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
            et_select_option = lrt_werks ).
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

*-- Fetch the Projects under given plant
  SELECT a~equnr, a~descr, b~werks
    FROM /agri/fmirhdr AS a
   INNER JOIN /agri/fmirwrk AS b
      ON b~equnr EQ a~equnr
    INTO TABLE @DATA(lt_irhdr)
   WHERE a~irtyp EQ @zcl_abs_abap_maintain=>c_irrtyp "'001'
     AND a~kfrst EQ @space
     AND b~werks IN @lrt_werks.
  IF sy-subrc EQ 0.
    MOVE-CORRESPONDING lt_irhdr TO et_entityset.
  ENDIF.

  READ TABLE lrt_werks INTO lrs_werks INDEX 1.
  IF sy-subrc = 0.
    IF et_entityset IS INITIAL.
*-- Give error message
      CLEAR ls_message.
      ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error. "'E'
      ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
      ls_message-msgno = '134'.
      ls_message-msgv1 =  lrs_werks-low.
      add_messages_to_msg_container( is_message = ls_message ).
    ENDIF.
  ENDIF. "lrs_werks

ENDMETHOD.


METHOD irrshiftf4set_get_entityset.

*-- Local Declarations
  DATA : ls_entityset  LIKE LINE OF et_entityset,
         ls_werks      TYPE zabs_s_irr_plants,
         lt_filter     TYPE /iwbep/t_mgw_select_option,
         ls_filter     TYPE /iwbep/s_mgw_select_option,
         lv_filter_str TYPE string,
         lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
         lrt_werks     TYPE RANGE OF werks_d,
         lrs_werks     LIKE LINE OF lrt_werks,
         lrt_tmatnr    TYPE RANGE OF matnr,
         lrs_tmatnr    LIKE LINE  OF lrt_tmatnr,
         ls_message    TYPE /agri/s_gprolog.

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
            et_select_option = lrt_werks ).
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

*--Fetching Assignment of Task Lists to Materials data
  SELECT plnty,
         plnnr,
         plnal
    FROM mapl
    INTO TABLE @DATA(lt_mapl)
   WHERE matnr IN @lrt_tmatnr
     AND werks IN @lrt_werks
     AND loekz EQ @space.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  SORT lt_mapl BY plnty plnnr plnal.

*--Fetching Operations data
  SELECT b~vornr, b~werks, b~ltxa1
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
    SORT lt_plpo BY vornr.
  ENDIF.

  LOOP AT lt_plpo INTO DATA(ls_plpo).

    ls_entityset-vornr = ls_plpo-vornr.
    ls_entityset-werks = ls_plpo-werks.
    ls_entityset-ltxa1 = ls_plpo-ltxa1.
    APPEND ls_entityset TO et_entityset.
    CLEAR ls_entityset.

  ENDLOOP. "lt_plpo

ENDMETHOD.


METHOD irrterrainsset_get_entityset.

*-- Local Declarations
  DATA : lt_filter     TYPE /iwbep/t_mgw_select_option,
         ls_filter     TYPE /iwbep/s_mgw_select_option,
         lv_filter_str TYPE string,
         lo_filter     TYPE REF TO /iwbep/if_mgw_req_filter,
         lrt_equnr     TYPE RANGE OF /agri/glequnr,
         lrs_equnr     LIKE LINE OF lrt_equnr,
         ls_message    TYPE /agri/s_gprolog.

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
      WHEN 'EQUNR'.
        lo_filter->convert_select_option(
          EXPORTING
            is_select_option = ls_filter
          IMPORTING
            et_select_option = lrt_equnr ).
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

*-- Fetch the terrains under given project
  SELECT equnr, tplnr_fl,
         zzcshift1, zzcshift2, zzcshift3,
         zzcshift4, zzcshift5, zzcshift6
    FROM /agri/fmirflo
    INTO TABLE @DATA(lt_irflo)
   WHERE equnr IN @lrt_equnr.
  IF sy-subrc EQ 0.
    MOVE-CORRESPONDING lt_irflo TO et_entityset.
  ENDIF.

  READ TABLE lrt_equnr INTO lrs_equnr INDEX 1.
  IF sy-subrc = 0.
    IF et_entityset IS INITIAL.
*-- Give error message
      CLEAR ls_message.
      ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_error. "'E'
      ls_message-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
      ls_message-msgno = '135'.
      ls_message-msgv1 =  lrs_equnr-low.
      add_messages_to_msg_container( is_message = ls_message ).
    ENDIF.
  ENDIF. "lrt_equnr

ENDMETHOD.
ENDCLASS.
