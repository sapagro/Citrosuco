************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name       :  ZABS_REP_ACCOM_BGPROG_SUB                       *
* Tcode             :  ZABS_TRN_ACCOM                                  *
* Created By        :                                                  *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  09.03.2020                                      *
* TR                :  C4DK924087                                      *
* Version           :  001                                             *
* Description       :  Accomplishment Confirmation Back Ground Program *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*&---------------------------------------------------------------------*
*& Form SCREEN_MODIFY
*&---------------------------------------------------------------------*
*& SCREEN_MODIFY
*&---------------------------------------------------------------------*
FORM screen_modify.

*--Screen Validations
  LOOP AT SCREEN.
    IF p_pro IS NOT INITIAL.
      IF ( screen-group1 = 'K6'
     OR screen-group1 = 'K7'
     OR screen-group1 = 'K8'
     OR screen-group1 = 'K9'
     OR screen-group1 = 'K10'
     OR screen-group1 = 'K11').
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF p_rpro IS NOT INITIAL.

      IF p_acco IS NOT INITIAL.
        IF ( screen-group1 = 'K8'
        OR screen-group1 = 'K11').
          screen-active = '0'.
          MODIFY SCREEN.
        ENDIF.

      ELSEIF  p_cnfm IS NOT INITIAL.
        IF ( screen-group1 = 'K8'
         OR screen-group1 = 'K10').
          screen-active = '0'.
          MODIFY SCREEN.
        ENDIF.

      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SCREEN_VALIDATIONS
*&---------------------------------------------------------------------*
*& SCREEN_VALIDATIONS
*&---------------------------------------------------------------------*
FORM screen_validations.

  CHECK sy-ucomm = zcl_abs_abap_maintain=>c_ucomm_onli "'ONLI'
   OR sy-ucomm = space.

  IF p_rpro IS NOT INITIAL
  AND p_acco IS NOT INITIAL
   OR p_cnfm IS NOT INITIAL.

    IF s_mac IS INITIAL
     AND s_erdat IS INITIAL.
      MESSAGE TEXT-011 TYPE zcl_abs_abap_maintain=>c_msgty_error.  "'E'
    ENDIF.

  ELSEIF p_pro IS NOT INITIAL.
    IF s_erdat IS INITIAL.
      MESSAGE TEXT-012 TYPE zcl_abs_abap_maintain=>c_msgty_error.
    ENDIF.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form INITIALIZE_GLOBAL_DATA
*&---------------------------------------------------------------------*
*& INITIALIZE_GLOBAL_DATA
*&---------------------------------------------------------------------*
FORM initialize_global_data.
  REFRESH gt_str_accomlog.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form PROCESS_ACCOM_CREATE_CONFIRM
*&---------------------------------------------------------------------*
*& PROCESS_ACCOM_CREATE_CONFIRM
*&---------------------------------------------------------------------*
FORM process_accom_create_confirm.

  DATA: lt_mackey     TYPE tt_mackey,
        ls_mackey     TYPE ty_mackey,
        ls_mackey_tmp TYPE ty_mackey,
        lt_mackey_tmp TYPE tt_mackey,
        lt_acitm_crt  TYPE zabs_tty_bgp_accom,
        lt_acbgp_cnf  TYPE zabs_tty_ac_baggrp,
        lt_achdr_crt  TYPE zabs_tty_hdr_accom,
        lt_acdoc      TYPE /agri/t_fmacs_doc.

  PERFORM fetch_buffer_accom_create USING abap_false
                                 CHANGING lt_acitm_crt.

  PERFORM fetch_buffer_accom_confirm USING abap_false
                                  CHANGING lt_acbgp_cnf.

  LOOP AT lt_acitm_crt INTO DATA(ls_acitm_crt).
    ls_mackey-zzmackey = ls_acitm_crt-zzmackey.
    APPEND ls_mackey TO lt_mackey.
    CLEAR ls_mackey.
  ENDLOOP.

  LOOP AT lt_acbgp_cnf INTO DATA(ls_acbgp_cnf).
    ls_mackey_tmp-zzmackey = ls_acbgp_cnf-mackey.
    APPEND ls_mackey_tmp TO lt_mackey_tmp.
    CLEAR ls_mackey_tmp.
  ENDLOOP.

  APPEND LINES OF lt_mackey_tmp TO lt_mackey.
  REFRESH lt_mackey_tmp.

  SORT lt_mackey.
  DELETE ADJACENT DUPLICATES FROM lt_mackey COMPARING zzmackey.

  PERFORM fetch_accom_data USING lt_mackey
                        CHANGING lt_achdr_crt
                                 lt_acdoc.

*  IF sy-ucomm = zcl_abs_abap_maintain=>c_ucomm_onli "'ONLI'
*   OR sy-ucomm = space
*   AND lt_acitm_crt IS INITIAL
*    AND lt_acbgp_cnf IS INITIAL.
*    MESSAGE TEXT-003 TYPE zcl_abs_abap_maintain=>c_msgty_info.
*    LEAVE LIST-PROCESSING.
*  ENDIF.

  IF lt_acitm_crt IS INITIAL AND lt_acbgp_cnf IS INITIAL.
    MESSAGE TEXT-003 TYPE zcl_abs_abap_maintain=>c_msgty_info.
    LEAVE LIST-PROCESSING.
  ENDIF.

  PERFORM accom_create USING abap_false
                             lt_acitm_crt
                    CHANGING lt_achdr_crt
                             lt_acdoc.

  PERFORM accom_conf USING abap_false
                           lt_achdr_crt
                           lt_acbgp_cnf
                           lt_acdoc.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form REPROCESS_ACCOM_CREATE
*&---------------------------------------------------------------------*
*& REPROCESS_ACCOM_CREATE
*&---------------------------------------------------------------------*
FORM reprocess_accom_create.

  DATA: lt_mackey    TYPE tt_mackey,
        ls_mackey    TYPE ty_mackey,
        lt_acitm_crt TYPE zabs_tty_bgp_accom,
        lt_achdr_crt TYPE zabs_tty_hdr_accom,
        lt_acdoc     TYPE /agri/t_fmacs_doc.

  PERFORM fetch_buffer_accom_create USING abap_true
                                 CHANGING lt_acitm_crt.

  LOOP AT lt_acitm_crt INTO DATA(ls_acitm_crt).
    ls_mackey-zzmackey = ls_acitm_crt-zzmackey.
    APPEND ls_mackey TO lt_mackey.
    CLEAR ls_mackey.
  ENDLOOP.

  SORT lt_mackey.
  DELETE ADJACENT DUPLICATES FROM lt_mackey COMPARING zzmackey.

  PERFORM fetch_accom_data USING lt_mackey
                        CHANGING lt_achdr_crt
                                 lt_acdoc.

*  IF sy-ucomm = zcl_abs_abap_maintain=>c_ucomm_onli "'ONLI'
*   OR sy-ucomm = space
*    AND lt_acitm_crt IS INITIAL.
*    MESSAGE TEXT-003 TYPE zcl_abs_abap_maintain=>c_msgty_info.
*    LEAVE LIST-PROCESSING.
*  ENDIF.

  IF lt_acitm_crt IS INITIAL.
    MESSAGE TEXT-003 TYPE zcl_abs_abap_maintain=>c_msgty_info.
    LEAVE LIST-PROCESSING.
  ENDIF.

  PERFORM accom_create USING abap_true
                             lt_acitm_crt
                    CHANGING lt_achdr_crt
                             lt_acdoc.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form REPROCESS_ACCOM_CONFIRM
*&---------------------------------------------------------------------*
*& REPROCESS_ACCOM_CONFIRM
*&---------------------------------------------------------------------*
FORM reprocess_accom_confirm.

  DATA: lt_mackey    TYPE tt_mackey,
        ls_mackey    TYPE ty_mackey,
        lt_acbgp_cnf TYPE zabs_tty_ac_baggrp,
        lt_achdr_crt TYPE zabs_tty_hdr_accom,
        lt_acdoc     TYPE /agri/t_fmacs_doc.

  PERFORM fetch_buffer_accom_confirm USING abap_true
                                  CHANGING lt_acbgp_cnf.

  LOOP AT lt_acbgp_cnf INTO DATA(ls_acbgp_cnf).
    ls_mackey-zzmackey = ls_acbgp_cnf-mackey.
    APPEND ls_mackey TO lt_mackey.
    CLEAR ls_mackey.
  ENDLOOP.

  SORT lt_mackey.
  DELETE ADJACENT DUPLICATES FROM lt_mackey COMPARING zzmackey.

  PERFORM fetch_accom_data USING lt_mackey
                        CHANGING lt_achdr_crt
                                 lt_acdoc.

*  IF sy-ucomm = zcl_abs_abap_maintain=>c_ucomm_onli
*    OR sy-ucomm = space
*   AND lt_acbgp_cnf IS INITIAL.
*    MESSAGE TEXT-003 TYPE zcl_abs_abap_maintain=>c_msgty_info.
*    LEAVE LIST-PROCESSING.
*  ENDIF.

  IF lt_acbgp_cnf IS INITIAL.
    MESSAGE TEXT-003 TYPE zcl_abs_abap_maintain=>c_msgty_info.
    LEAVE LIST-PROCESSING.
  ENDIF.

  PERFORM accom_conf USING abap_true
                           lt_achdr_crt
                           lt_acbgp_cnf
                           lt_acdoc.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FETCH_BUFFER_ACCOM_CREATE
*&---------------------------------------------------------------------*
*& FETCH_BUFFER_ACCOM_CREATE
*&---------------------------------------------------------------------*
FORM fetch_buffer_accom_create USING pv_reprocess TYPE xfeld
                            CHANGING ct_acitm_crt TYPE zabs_tty_bgp_accom.

  IF pv_reprocess EQ abap_false.
*-- Fetching Item data
    SELECT *
      FROM zabs_bgp_accom
      INTO TABLE ct_acitm_crt
     WHERE status    EQ zcl_abs_abap_maintain=>c_status_create "'C'
*       AND duplicate EQ space
       AND erdat     IN s_erdat.
  ELSE.
*-- Fetching Item data
    SELECT *
      FROM zabs_bgp_accom
      INTO TABLE ct_acitm_crt
     WHERE zzmackey IN s_mac
*       AND posnr    IN s_pos
       AND status   EQ zcl_abs_abap_maintain=>c_status_error "'E'
       AND erdat    IN s_erdat.
  ENDIF.

  SORT ct_acitm_crt BY zzmackey zzkeyposnr.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FETCH_BUFFER_ACCOM_CONFIRM
*&---------------------------------------------------------------------*
*& FETCH_BUFFER_ACCOM_CONFIRM
*&---------------------------------------------------------------------*
FORM fetch_buffer_accom_confirm USING pv_reprocess TYPE xfeld
                             CHANGING ct_acbgp_cnf TYPE zabs_tty_ac_baggrp.

  IF pv_reprocess EQ abap_false.
*-- Fetching confirmations data
    SELECT *
      FROM zabs_ac_bggrp
      INTO TABLE ct_acbgp_cnf
     WHERE status EQ zcl_abs_abap_maintain=>c_status_create "'C'
       AND erdat IN s_erdat.
  ELSE.
*-- Fetching confirmations data
    SELECT *
      FROM zabs_ac_bggrp
      INTO TABLE ct_acbgp_cnf
     WHERE mackey IN s_mac
       AND baggp  IN s_bggrp
       AND status EQ zcl_abs_abap_maintain=>c_status_error "'E'
       AND erdat  IN s_erdat.
  ENDIF.

  SORT ct_acbgp_cnf BY mackey baggp.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FETCH_ACCOM_DATA
*&---------------------------------------------------------------------*
*& FETCH_ACCOM_DATA
*&---------------------------------------------------------------------*
FORM fetch_accom_data USING lt_mackey    TYPE tt_mackey
                   CHANGING ct_achdr_crt TYPE zabs_tty_hdr_accom
                            ct_acdoc     TYPE /agri/t_fmacs_doc.

  DATA: ls_accom TYPE /agri/s_fmacom,
        lt_accom TYPE /agri/t_fmacom.

*-- Fetching Header data
  SELECT *
    FROM zabs_hdr_accom
    INTO TABLE ct_achdr_crt
     FOR ALL ENTRIES IN lt_mackey
   WHERE zzmackey EQ lt_mackey-zzmackey.
  IF sy-subrc EQ 0.
    SORT ct_achdr_crt BY pernr zzmackey.
  ENDIF.

  DATA(lt_achdr_crt) = ct_achdr_crt.
  DELETE lt_achdr_crt WHERE accom IS INITIAL.

  LOOP AT lt_achdr_crt INTO DATA(ls_achdr_crt).
    CLEAR ls_accom.
    ls_accom-accom = ls_achdr_crt-accom.
    APPEND ls_accom TO lt_accom.
  ENDLOOP.

  IF lt_accom IS NOT INITIAL.
    CALL FUNCTION '/AGRI/FMAC_VIEW'
      EXPORTING
        it_accom       = lt_accom
      IMPORTING
        et_acdoc       = ct_acdoc
      EXCEPTIONS
        no_data_exists = 1
        OTHERS         = 2.
    IF sy-subrc EQ 0.
      SORT ct_acdoc BY accom.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form Accom_Create
*&---------------------------------------------------------------------*
*& Create Accomplishment
*&---------------------------------------------------------------------*
FORM accom_create USING pv_reprocess TYPE xfeld
                        pt_acitm_crt TYPE zabs_tty_bgp_accom
               CHANGING ct_achdr_crt TYPE zabs_tty_hdr_accom
                        ct_acdoc     TYPE /agri/t_fmacs_doc.

*-- Local Declarations
  DATA:
    ls_achdr             TYPE /agri/s_fmachdr,
    ls_acitm             TYPE /agri/s_fmacitm,
    ls_fmac_doc          TYPE /agri/s_fmacs_doc,
    lt_acitm             TYPE /agri/t_fmacitm,
    lt_messages          TYPE /agri/t_gprolog,
    ls_acdoc             TYPE /agri/s_fmacs_doc,
    lt_acdoc             TYPE /agri/t_fmacs_doc,
*    lt_duplicate_key     TYPE SORTED TABLE OF ty_duplicate_key
*                   WITH UNIQUE KEY zzmackey idresource zzhdate strttim,
*    ls_duplicate_key     TYPE ty_duplicate_key,
    lt_achdr_crt         TYPE zabs_tty_hdr_accom,
    lt_acitm_crt         TYPE zabs_tty_bgp_accom,
    ls_error_log_crt     TYPE zabs_bgp_accelog,
    lt_error_log_crt_del TYPE STANDARD TABLE OF zabs_bgp_accelog,
    lt_error_log_crt_ins TYPE STANDARD TABLE OF zabs_bgp_accelog,
    lv_msg               TYPE bapi_msg,
    ls_error_dis         TYPE zabs_str_accommlog.

  lt_acitm_crt = pt_acitm_crt.
  lt_acdoc = ct_acdoc.

  IF pv_reprocess = abap_false.
    DATA(lv_status) = zcl_abs_abap_maintain=>c_status_process. "'P'
  ELSEIF pv_reprocess = abap_true.
    lv_status = zcl_abs_abap_maintain=>c_status_reprocess. "'R'
    DATA(lv_rdate) = sy-datum.
  ENDIF.

  LOOP AT ct_achdr_crt ASSIGNING FIELD-SYMBOL(<fs_achdr_crt>).

    REFRESH: lt_acitm.
    CLEAR ls_achdr.

    READ TABLE lt_acdoc ASSIGNING FIELD-SYMBOL(<fs_acdoc>)
          WITH KEY accom = <fs_achdr_crt>-accom
        BINARY SEARCH.
    IF sy-subrc EQ 0.
      lt_acitm = <fs_acdoc>-x-acitm.
*      SORT lt_acitm BY zzmackey idresource zzhdate strttim.
      SORT lt_acitm BY posnr DESCENDING.

      READ TABLE lt_acitm INTO ls_acitm INDEX 1.
      IF sy-subrc EQ 0.
        DATA(lv_posnr) = ls_acitm-posnr.
      ENDIF.

      READ TABLE lt_acitm_crt TRANSPORTING NO FIELDS
            WITH KEY zzmackey = <fs_achdr_crt>-zzmackey
          BINARY SEARCH.
      IF sy-subrc EQ 0.
        DATA(lv_tabix) = sy-tabix.
        LOOP AT lt_acitm_crt ASSIGNING FIELD-SYMBOL(<fs_acitm_crt>)
                                                          FROM lv_tabix.
          IF <fs_achdr_crt>-zzmackey <> <fs_acitm_crt>-zzmackey.
            EXIT.
          ENDIF.

*          <fs_acitm_crt>-aenam   = sy-uname.
*          <fs_acitm_crt>-aedat   = sy-datum.
*          <fs_acitm_crt>-aezet   = sy-uzeit.

          <fs_acitm_crt>-status  = lv_status.
          <fs_acitm_crt>-rdate   = lv_rdate.

*          DATA(lv_duplicate) = abap_true.
*          READ TABLE lt_acitm TRANSPORTING NO FIELDS
*                WITH KEY zzmackey   = <fs_acitm_crt>-zzmackey
*                         idresource = <fs_acitm_crt>-idresource
*                         zzhdate    = <fs_acitm_crt>-zzhdate
*                         strttim    = <fs_acitm_crt>-strttim
*              BINARY SEARCH.
*          IF sy-subrc NE 0.
*            READ TABLE lt_duplicate_key TRANSPORTING NO FIELDS
*                  WITH KEY zzmackey  = <fs_acitm_crt>-zzmackey
*                          idresource = <fs_acitm_crt>-idresource
*                          zzhdate    = <fs_acitm_crt>-zzhdate
*                          strttim    = <fs_acitm_crt>-strttim
*                BINARY SEARCH.
*            IF sy-subrc NE 0.
*              CLEAR lv_duplicate.
*            ENDIF.
*          ENDIF.

*          IF lv_duplicate EQ abap_true.
*            <fs_acitm_crt>-duplicate = abap_true.
*            <fs_acitm_crt>-status = zcl_abs_abap_maintain=>c_status_create.
*          ELSE.

*            CLEAR ls_duplicate_key.
*            ls_duplicate_key-zzmackey   = <fs_acitm_crt>-zzmackey.
*            ls_duplicate_key-idresource = <fs_acitm_crt>-idresource.
*            ls_duplicate_key-zzhdate    = <fs_acitm_crt>-zzhdate.
*            ls_duplicate_key-strttim    = <fs_acitm_crt>-strttim.
*            INSERT ls_duplicate_key INTO TABLE lt_duplicate_key.

          CLEAR ls_acitm.
          MOVE-CORRESPONDING <fs_acitm_crt> TO ls_acitm.
          ls_acitm-accom   = <fs_acdoc>-accom.
          lv_posnr = lv_posnr + 1.
          ls_acitm-posnr = lv_posnr.
          ls_acitm-zztplnr = <fs_acitm_crt>-tplnr.
          ls_acitm-zzcdate = sy-datum.
          ls_acitm-zzctime = sy-uzeit.
          ls_acitm-zzcrimei  = <fs_acitm_crt>-imei.
          ls_acitm-zzcrbadge = <fs_acitm_crt>-badge.
          ls_acitm-updkz  = zcl_abs_abap_maintain=>c_updkz_insert. "'I'

*-- Add new item
          IF <fs_acdoc>-x-achdr-zzactcg = zcl_abs_abap_maintain=>c_actcg_prod.
            ls_acitm-status = zcl_abs_abap_maintain=>c_ac_status_created.
            <fs_acdoc>-x-achdr-status = zcl_abs_abap_maintain=>c_ac_status_created.
          ELSE.
            ls_acitm-status = zcl_abs_abap_maintain=>c_ac_status_confirm.
          ENDIF.
          APPEND ls_acitm TO <fs_acdoc>-x-acitm.

          DATA(lv_create) = abap_true.

*          ENDIF.
        ENDLOOP.
      ENDIF.

      IF lv_create = abap_true.
        ls_acdoc = <fs_acdoc>.
      ENDIF.

    ELSE.

      READ TABLE lt_acitm_crt INTO DATA(ls_acitm_crt)
            WITH KEY zzmackey = <fs_achdr_crt>-zzmackey
          BINARY SEARCH.
      IF sy-subrc EQ 0.
        lv_tabix = sy-tabix.

        CLEAR ls_achdr.
        MOVE-CORRESPONDING ls_acitm_crt TO ls_achdr.

        LOOP AT lt_acitm_crt ASSIGNING <fs_acitm_crt> FROM lv_tabix.
          IF <fs_achdr_crt>-zzmackey <> <fs_acitm_crt>-zzmackey.
            EXIT.
          ENDIF.

*          <fs_acitm_crt>-aenam   = sy-uname.
*          <fs_acitm_crt>-aedat   = sy-datum.
*          <fs_acitm_crt>-aezet   = sy-uzeit.

          <fs_acitm_crt>-status  = lv_status.
          <fs_acitm_crt>-rdate   = lv_rdate.

*          lv_duplicate = abap_true.
*          READ TABLE lt_duplicate_key TRANSPORTING NO FIELDS
*                WITH KEY zzmackey   = <fs_acitm_crt>-zzmackey
*                         idresource = <fs_acitm_crt>-idresource
*                         zzhdate    = <fs_acitm_crt>-zzhdate
*                         strttim    = <fs_acitm_crt>-strttim
*              BINARY SEARCH.
*          IF sy-subrc NE 0.
*            CLEAR lv_duplicate.
*          ENDIF.
*
*          IF lv_duplicate EQ abap_true.
*            <fs_acitm_crt>-duplicate = abap_true.
*            <fs_acitm_crt>-status = zcl_abs_abap_maintain=>c_status_create.
*          ELSE.

*            CLEAR ls_duplicate_key.
*            ls_duplicate_key-zzmackey   = <fs_acitm_crt>-zzmackey.
*            ls_duplicate_key-idresource = <fs_acitm_crt>-idresource.
*            ls_duplicate_key-zzhdate    = <fs_acitm_crt>-zzhdate.
*            ls_duplicate_key-strttim    = <fs_acitm_crt>-strttim.
*            INSERT ls_duplicate_key INTO TABLE lt_duplicate_key.

          CLEAR ls_acitm.
          MOVE-CORRESPONDING <fs_acitm_crt> TO ls_acitm.
          lv_posnr = lv_posnr + 1.
          ls_acitm-posnr = lv_posnr.
          ls_acitm-zztplnr = <fs_acitm_crt>-tplnr.
          ls_acitm-zzcdate = sy-datum.
          ls_acitm-zzctime = sy-uzeit.
          ls_acitm-zzcrimei  = <fs_acitm_crt>-imei.
          ls_acitm-zzcrbadge = <fs_acitm_crt>-badge.
          APPEND ls_acitm TO lt_acitm.
*          ENDIF.

        ENDLOOP.
      ENDIF.

      IF lt_acitm IS NOT INITIAL.
*-- Populate Accomplishment data
        CLEAR ls_acdoc.
        PERFORM populate_accom_data USING ls_achdr
                                          lt_acitm
                                 CHANGING ls_acdoc.
        lv_create = abap_true.
      ENDIF.
    ENDIF.

    IF lv_create IS NOT INITIAL.
      REFRESH lt_messages.
      CLEAR: ls_fmac_doc, lv_create.

*-- Create/Update the Accomplishment
      PERFORM create_accomplishment USING ls_acdoc
                                 CHANGING ls_fmac_doc
                                          lt_messages.

      READ TABLE lt_messages INTO DATA(ls_messages)
            WITH KEY msgty = zcl_abs_abap_maintain=>c_msgty_error. "'E'
      IF sy-subrc EQ 0.
        PERFORM message_build USING ls_messages
                           CHANGING lv_msg.

        READ TABLE lt_acitm_crt TRANSPORTING NO FIELDS
              WITH KEY zzmackey = <fs_achdr_crt>-zzmackey
            BINARY SEARCH.
        IF sy-subrc EQ 0.
          lv_tabix = sy-tabix.
          LOOP AT lt_acitm_crt ASSIGNING <fs_acitm_crt> FROM lv_tabix.
            IF <fs_achdr_crt>-zzmackey <> <fs_acitm_crt>-zzmackey.
              EXIT.
            ENDIF.

*            IF <fs_acitm_crt>-duplicate IS INITIAL.
            <fs_acitm_crt>-status = zcl_abs_abap_maintain=>c_status_error.  "'E'

            CLEAR ls_error_log_crt.
            ls_error_log_crt-zzmackey = <fs_acitm_crt>-zzmackey.
            ls_error_log_crt-zzkeyposnr = <fs_acitm_crt>-zzkeyposnr.
*            ls_error_log_crt-posnr = <fs_acitm_crt>-posnr.
            ls_error_log_crt-msgid = ls_messages-msgid.
            ls_error_log_crt-msgno = ls_messages-msgno.
            ls_error_log_crt-msgtyp = ls_messages-msgty.
            ls_error_log_crt-msgtxt = lv_msg.
            ls_error_log_crt-ernam = sy-uname.
            ls_error_log_crt-erdat = sy-datum.
            ls_error_log_crt-erzet = sy-uzeit.
            APPEND ls_error_log_crt TO lt_error_log_crt_ins.

            CLEAR ls_error_dis.
            ls_error_dis-zzmackey = <fs_acitm_crt>-zzmackey.
            ls_error_dis-zzkeyposnr = <fs_acitm_crt>-zzkeyposnr.
*            ls_error_dis-posnr = <fs_acitm_crt>-posnr.
            ls_error_dis-msgid = ls_messages-msgid.
            ls_error_dis-msgno = ls_messages-msgno.
            ls_error_dis-msgtyp = ls_messages-msgty.
            ls_error_dis-msgtxt = lv_msg.
            APPEND ls_error_dis TO gt_str_accomlog.

*            ENDIF.
          ENDLOOP.
        ENDIF.

      ELSE.

        IF <fs_achdr_crt>-accom IS INITIAL.
          <fs_achdr_crt>-accom = ls_fmac_doc-accom.
          APPEND <fs_achdr_crt> TO lt_achdr_crt.
        ELSE.
          DELETE ct_acdoc WHERE accom = ls_fmac_doc-accom.
        ENDIF.

        APPEND ls_fmac_doc TO ct_acdoc.

        CLEAR: lv_msg, ls_messages.
        READ TABLE lt_messages INTO ls_messages
           WITH KEY msgty = zcl_abs_abap_maintain=>c_msgty_success. "'S'
        IF sy-subrc EQ 0.
          PERFORM message_build USING ls_messages
                             CHANGING lv_msg.
        ENDIF.

        READ TABLE lt_acitm_crt TRANSPORTING NO FIELDS
              WITH KEY zzmackey = <fs_achdr_crt>-zzmackey
            BINARY SEARCH.
        IF sy-subrc EQ 0.
          lv_tabix = sy-tabix.
          LOOP AT lt_acitm_crt ASSIGNING <fs_acitm_crt> FROM lv_tabix.
            IF <fs_achdr_crt>-zzmackey <> <fs_acitm_crt>-zzmackey.
              EXIT.
            ENDIF.

            IF lv_status EQ zcl_abs_abap_maintain=>c_status_reprocess. "'R'
              CLEAR ls_error_log_crt.
              ls_error_log_crt-zzmackey = <fs_acitm_crt>-zzmackey.
*              ls_error_log_crt-posnr = <fs_acitm_crt>-posnr.
              ls_error_log_crt-zzkeyposnr = <fs_acitm_crt>-zzkeyposnr.
              APPEND ls_error_log_crt TO lt_error_log_crt_del.
            ENDIF.

            CLEAR ls_error_dis.
            ls_error_dis-zzmackey = <fs_acitm_crt>-zzmackey.
*            ls_error_dis-posnr = <fs_acitm_crt>-posnr.
            ls_error_dis-zzkeyposnr = <fs_acitm_crt>-zzkeyposnr.
            ls_error_dis-msgid = ls_messages-msgid.
            ls_error_dis-msgno = ls_messages-msgno.
            ls_error_dis-msgtyp = ls_messages-msgty.
            ls_error_dis-msgtxt = lv_msg.
            APPEND ls_error_dis TO gt_str_accomlog.

          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.

    REFRESH lt_acitm.
    CLEAR lv_posnr.

  ENDLOOP.

  SORT ct_acdoc BY accom.

  IF lt_achdr_crt IS NOT INITIAL.
    LOOP AT lt_achdr_crt ASSIGNING <fs_achdr_crt>.
      <fs_achdr_crt>-aenam = sy-uname.
      <fs_achdr_crt>-aedat = sy-datum.
      <fs_achdr_crt>-aezet = sy-uzeit.
    ENDLOOP.
    UPDATE zabs_hdr_accom FROM TABLE lt_achdr_crt.
  ENDIF.

  IF lt_acitm_crt IS NOT INITIAL.
    LOOP AT lt_acitm_crt ASSIGNING <fs_acitm_crt>.
      <fs_acitm_crt>-aenam = sy-uname.
      <fs_acitm_crt>-aedat = sy-datum.
      <fs_acitm_crt>-aezet = sy-uzeit.
    ENDLOOP.
    UPDATE zabs_bgp_accom FROM TABLE lt_acitm_crt.
  ENDIF.

  IF lt_error_log_crt_del IS NOT INITIAL.
    DELETE zabs_bgp_accelog FROM TABLE lt_error_log_crt_del.
  ENDIF.

  IF lt_error_log_crt_ins IS NOT INITIAL.
    MODIFY zabs_bgp_accelog FROM TABLE lt_error_log_crt_ins.
  ENDIF.

  COMMIT WORK AND WAIT.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form POPULATE_ACCOM_DATA
*&---------------------------------------------------------------------*
*& POPULATE_ACCOM_DATA
*&---------------------------------------------------------------------*
FORM populate_accom_data USING ps_achdr     TYPE /agri/s_fmachdr
                               pt_acitm_tmp TYPE /agri/t_fmfmacitm
                      CHANGING cs_acdoc     TYPE /agri/s_fmacs_doc.

*-- Local Declarations
  DATA: lv_actyp TYPE /agri/fmactyp.

  DATA(ls_achdr) = ps_achdr.
  DATA(lt_acitm) = pt_acitm_tmp.

*-- Read Accomplishment type and task
  CALL METHOD zcl_abs_get_variants=>get_constant_single
    EXPORTING
      iv_objid  = zcl_abs_abap_maintain=>c_objid_mobile
      iv_k1val  = zcl_abs_abap_maintain=>c_key_accomplish_type
      iv_k2val  = ls_achdr-zzactcg
    IMPORTING
      ev_cnval1 = lv_actyp.

*-- Populate Accomplishment header data
  ls_achdr-accom  = TEXT-007.
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
    <fs_acitm>-accom = TEXT-007.
    <fs_acitm>-zztplnr = <fs_acitm>-tplnr.
    IF ls_achdr-zzactcg = zcl_abs_abap_maintain=>c_actcg_prod.
      <fs_acitm>-status = zcl_abs_abap_maintain=>c_ac_status_created.
    ELSE.
      <fs_acitm>-status = zcl_abs_abap_maintain=>c_ac_status_confirm.
    ENDIF.
    <fs_acitm>-updkz = zcl_abs_abap_maintain=>c_insert.
  ENDLOOP.

  cs_acdoc-accom = TEXT-007.
  cs_acdoc-x-achdr = ls_achdr.
  cs_acdoc-x-acitm = lt_acitm.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CREATE_ACCOMPLISHMENT
*&---------------------------------------------------------------------*
*& CREATE_ACCOMPLISHMENT
*&---------------------------------------------------------------------*
FORM create_accomplishment USING ps_acdoc    TYPE /agri/s_fmacs_doc
                        CHANGING cs_fmac_doc TYPE /agri/s_fmacs_doc
                                 ct_messages TYPE /agri/t_gprolog.

*-- Create the new Accomplishment sheet
  CALL FUNCTION 'ZABS_FMAC_CREATE'
    EXPORTING
      is_fmacdoc  = ps_acdoc
    IMPORTING
      es_fmac_doc = cs_fmac_doc
      et_messages = ct_messages.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form Accom_Data
*&---------------------------------------------------------------------*
*& Accomplishment Create
*&---------------------------------------------------------------------*
FORM accom_conf USING pv_reprocess TYPE xfeld
                      pt_achdr_crt TYPE zabs_tty_hdr_accom
                      pt_ac_bggrp  TYPE zabs_tty_ac_baggrp
                      pt_acdoc     TYPE /agri/t_fmacs_doc.

*-- Local Declarations
  DATA:
    lt_messages          TYPE /agri/t_gprolog,
    lt_ac_bggrp          TYPE zabs_tty_ac_baggrp,
    ls_error_log_cnf     TYPE zabs_acbgrp_elog,
    lt_error_log_cnf_del TYPE STANDARD TABLE OF zabs_acbgrp_elog,
    lt_error_log_cnf_ins TYPE STANDARD TABLE OF zabs_acbgrp_elog,
    ls_error_dis         TYPE zabs_str_accommlog.

  IF pv_reprocess = abap_false.
    DATA(lv_status) = zcl_abs_abap_maintain=>c_status_process. "'P'
  ELSEIF pv_reprocess = abap_true.
    lv_status = zcl_abs_abap_maintain=>c_status_reprocess. "'R'
    DATA(lv_rdate) = sy-datum.
  ENDIF.

  LOOP AT pt_achdr_crt INTO DATA(ls_achdr_crt).

    READ TABLE pt_acdoc INTO DATA(ls_acdoc)
          WITH KEY accom = ls_achdr_crt-accom
        BINARY SEARCH.
    IF sy-subrc EQ 0.
      READ TABLE pt_ac_bggrp TRANSPORTING NO FIELDS
            WITH KEY mackey = ls_achdr_crt-zzmackey
          BINARY SEARCH.
      IF sy-subrc EQ 0.
        DATA(lv_tabix) = sy-tabix.
        LOOP AT pt_ac_bggrp INTO DATA(ls_ac_bggrp) FROM lv_tabix.
          IF ls_achdr_crt-zzmackey <> ls_ac_bggrp-mackey.
            EXIT.
          ENDIF.

*          CLEAR ls_ac_bggrp.
          ls_ac_bggrp-status  = lv_status.
          ls_ac_bggrp-rdate   = lv_rdate.

*          ls_ac_bggrp-aenam   = sy-uname.
*          ls_ac_bggrp-aedat   = sy-datum.
*          ls_ac_bggrp-aezet   = sy-uzeit.

          REFRESH lt_messages.
          CALL FUNCTION 'ZABS_FMAC_CONFIRMATION'
            EXPORTING
              iv_baggp    = ls_ac_bggrp-baggp
              is_fmacdoc  = ls_acdoc
            IMPORTING
              et_messages = lt_messages.

          READ TABLE lt_messages INTO DATA(ls_messages)
                WITH KEY msgty = zcl_abs_abap_maintain=>c_msgty_error. "'E'
          IF sy-subrc EQ 0.
            ls_ac_bggrp-status = zcl_abs_abap_maintain=>c_status_error. "'E'

            CLEAR ls_error_log_cnf.
            ls_error_log_cnf-mackey = ls_ac_bggrp-mackey.
            ls_error_log_cnf-baggp = ls_ac_bggrp-baggp.
            ls_error_log_cnf-msgid = ls_messages-msgid.
            ls_error_log_cnf-msgno = ls_messages-msgno.
            ls_error_log_cnf-msgtyp = ls_messages-msgty.
            PERFORM message_build USING ls_messages
                               CHANGING ls_error_log_cnf-msgtxt.
            ls_error_log_cnf-ernam = sy-uname.
            ls_error_log_cnf-erdat = sy-datum.
            ls_error_log_cnf-erzet = sy-uzeit.
            APPEND ls_error_log_cnf TO lt_error_log_cnf_ins.

            CLEAR ls_error_dis.
            ls_error_dis-zzmackey = ls_ac_bggrp-mackey.
            ls_error_dis-baggp = ls_ac_bggrp-baggp.
            ls_error_dis-msgid = ls_messages-msgid.
            ls_error_dis-msgno = ls_messages-msgno.
            ls_error_dis-msgtyp = ls_messages-msgty.
            ls_error_dis-msgtxt = ls_error_log_cnf-msgtxt.
            APPEND ls_error_dis TO gt_str_accomlog.

          ELSE.

            IF lv_status EQ zcl_abs_abap_maintain=>c_status_reprocess. "'R'
              CLEAR ls_error_log_cnf.
              ls_error_log_cnf-mackey = ls_ac_bggrp-mackey.
              ls_error_log_cnf-baggp  = ls_ac_bggrp-baggp.
              APPEND ls_error_log_cnf TO lt_error_log_cnf_del.
            ENDIF.

            READ TABLE lt_messages INTO ls_messages
                WITH KEY msgty = zcl_abs_abap_maintain=>c_msgty_success. "'S'
            IF sy-subrc EQ 0.
              CLEAR ls_error_dis.
              ls_error_dis-zzmackey = ls_ac_bggrp-mackey.
              ls_error_dis-baggp = ls_ac_bggrp-baggp.
              ls_error_dis-msgid = ls_messages-msgid.
              ls_error_dis-msgno = ls_messages-msgno.
              ls_error_dis-msgtyp = ls_messages-msgty.
              PERFORM message_build USING ls_messages
                             CHANGING ls_error_dis-msgtxt.
              APPEND ls_error_dis TO gt_str_accomlog.
            ENDIF.
          ENDIF.

          APPEND ls_ac_bggrp TO lt_ac_bggrp.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF lt_ac_bggrp IS NOT INITIAL.
    LOOP AT lt_ac_bggrp ASSIGNING FIELD-SYMBOL(<fs_ac_bggrp>).
      <fs_ac_bggrp>-aenam   = sy-uname.
      <fs_ac_bggrp>-aedat   = sy-datum.
      <fs_ac_bggrp>-aezet   = sy-uzeit.
    ENDLOOP.
    UPDATE zabs_ac_bggrp FROM TABLE lt_ac_bggrp.
  ENDIF.

  IF lt_error_log_cnf_del IS NOT INITIAL.
    DELETE zabs_acbgrp_elog FROM TABLE lt_error_log_cnf_del.
  ENDIF.

  IF lt_error_log_cnf_ins IS NOT INITIAL.
    MODIFY zabs_acbgrp_elog FROM TABLE lt_error_log_cnf_ins.
  ENDIF.

  COMMIT WORK AND WAIT.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form MESSAGE_BUILD
*&---------------------------------------------------------------------*
*& MESSAGE_BUILD
*&---------------------------------------------------------------------*
FORM message_build USING ps_messages TYPE /agri/s_gprolog
                CHANGING cv_msg TYPE bapi_msg.

  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
    EXPORTING
      msgid               = ps_messages-msgid
      msgnr               = ps_messages-msgno
      msgv1               = ps_messages-msgv1
      msgv2               = ps_messages-msgv1
      msgv3               = ps_messages-msgv1
      msgv4               = ps_messages-msgv1
    IMPORTING
      message_text_output = cv_msg.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_DATA
*&---------------------------------------------------------------------*
*& DISPLAY_DATA
*&---------------------------------------------------------------------*
FORM display_data.

  TYPE-POOLS: slis.

  DATA: lt_fcat   TYPE slis_t_fieldcat_alv,
        ls_layout TYPE slis_layout_alv.

  ls_layout-colwidth_optimize = abap_true.
  ls_layout-zebra             = abap_true.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-repid
      i_structure_name       = 'ZABS_STR_ACCOMMLOG'
    CHANGING
      ct_fieldcat            = lt_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  SORT gt_str_accomlog.
  DELETE ADJACENT DUPLICATES FROM gt_str_accomlog COMPARING ALL FIELDS.

  IF lt_fcat IS NOT INITIAL.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        is_layout   = ls_layout
        it_fieldcat = lt_fcat
      TABLES
        t_outtab    = gt_str_accomlog.
  ENDIF.

ENDFORM.
