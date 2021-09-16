FUNCTION zabs_fm_order_dbupdate.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IT_ORDHDR) TYPE  ZABS_TTY_ORDHDR
*"     REFERENCE(IT_ORDITM) TYPE  ZABS_TTY_ORDITM
*"     REFERENCE(IT_ORDCNF) TYPE  ZABS_TTY_ORDCNF
*"     REFERENCE(IT_ORDCON) TYPE  ZABS_TTY_ORDCON
*"     REFERENCE(IT_ORDGRP) TYPE  ZABS_TTY_ORDGRP
*"  EXPORTING
*"     REFERENCE(ET_MESSAGES) TYPE  ESP1_MESSAGE_TAB_TYPE
*"  CHANGING
*"     REFERENCE(EV_ORDNO) TYPE  /AGRI/FMFPNUM OPTIONAL
*"----------------------------------------------------------------------

*--Local declarations
  DATA: lwa_ordhdr_insert TYPE zabst_ordhdr,
        lwa_orditm_insert TYPE zabst_orditm,
        lwa_ordcnf_insert TYPE zabst_ordcnf,
        lwa_ordcon_insert TYPE zabst_ordcon,
        lwa_ordgrp_insert TYPE zabst_ordgrp,
        lwa_ordhdr_update TYPE zabst_ordhdr,
        lwa_orditm_update TYPE zabst_orditm,
        lwa_ordcnf_update TYPE zabst_ordcnf,
        lwa_ordcon_update TYPE zabst_ordcon,
        lwa_ordgrp_update TYPE zabst_ordgrp,
        lwa_var           TYPE ty_var,
        lwa_messages      TYPE esp1_message_wa_type,

        lv_tabix          TYPE sy-tabix,
        lv_flag           TYPE boolean,

        lt_ordhdr_insert  TYPE TABLE OF zabst_ordhdr,
        lt_orditm_insert  TYPE TABLE OF zabst_orditm,
        lt_ordcnf_insert  TYPE TABLE OF zabst_ordcnf,
        lt_ordcon_insert  TYPE TABLE OF zabst_ordcon,
        lt_ordgrp_insert  TYPE TABLE OF zabst_ordgrp,
        lt_ordhdr_update  TYPE TABLE OF zabst_ordhdr,
        lt_orditm_update  TYPE TABLE OF zabst_orditm,
        lt_ordcnf_update  TYPE TABLE OF zabst_ordcnf,
        lt_ordcon_update  TYPE TABLE OF zabst_ordcon,
        lt_ordgrp_update  TYPE TABLE OF zabst_ordgrp.

*--Fetching Order Confirmation data
  SELECT *
    FROM zabst_ordcnf
    INTO TABLE @DATA(lt_ordcnf_temp).
  IF sy-subrc = 0.
    SORT lt_ordcnf_temp BY ordno contr vornr.
  ENDIF.

*--Order Header table
  LOOP AT it_ordhdr INTO DATA(lwa_ordhdr).

    IF lwa_ordhdr-updkz = zcl_abs_abap_maintain=>c_updkz_insert. "'I'

      MOVE-CORRESPONDING lwa_ordhdr TO lwa_ordhdr_insert.
*--Perform Number Range
      PERFORM number_range CHANGING lwa_var.
      ev_ordno = lwa_var-next_no.
*      APPEND lwa_var TO ct_ordno.
      lwa_ordhdr_insert-ordno = lwa_var-next_no.
      lwa_messages-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
      lwa_messages-msgno = 123.
      lwa_messages-msgv1 = lwa_var-next_no.
      lwa_messages-msgty = zcl_abs_abap_maintain=>c_msgty_success.

      APPEND lwa_messages TO et_messages.
      CLEAR lwa_messages.

      lwa_ordhdr_insert-ernam = sy-uname.
      lwa_ordhdr_insert-erdat = sy-datum.
      lwa_ordhdr_insert-erzet = sy-uzeit.
      APPEND lwa_ordhdr_insert TO lt_ordhdr_insert.
      CLEAR lwa_ordhdr_insert.

    ELSEIF lwa_ordhdr-updkz = zcl_abs_abap_maintain=>c_updkz_update. "'U'

      MOVE-CORRESPONDING lwa_ordhdr TO lwa_ordhdr_update.
      lwa_ordhdr_update-aenam = sy-uname.
      lwa_ordhdr_update-aedat = sy-datum.
      lwa_ordhdr_update-aezet = sy-uzeit.
      APPEND lwa_ordhdr_update TO lt_ordhdr_update.

    ENDIF. "lwa_ordhdr-updkz

*--Order Items table
    READ TABLE it_orditm TRANSPORTING NO FIELDS
                         WITH KEY ordno = lwa_ordhdr-ordno
                         BINARY SEARCH.
    IF sy-subrc = 0.

      lv_tabix = sy-tabix.
      LOOP AT it_orditm INTO DATA(lwa_orditm) FROM lv_tabix.
        IF lwa_orditm-ordno <> lwa_ordhdr-ordno.
          EXIT.
        ENDIF.

        IF lwa_orditm-updkz = zcl_abs_abap_maintain=>c_updkz_insert. "'I'
          MOVE-CORRESPONDING lwa_orditm TO lwa_orditm_insert.
          lwa_orditm_insert-ordno = lwa_var-next_no.
          lwa_orditm_insert-ernam = sy-uname.
          lwa_orditm_insert-erdat = sy-datum.
          lwa_orditm_insert-erzet = sy-uzeit.
          APPEND lwa_orditm_insert TO lt_orditm_insert.
        ELSEIF lwa_orditm-updkz = zcl_abs_abap_maintain=>c_updkz_update. "'U'
          MOVE-CORRESPONDING lwa_orditm TO lwa_orditm_update.
          lwa_orditm_update-aenam = sy-uname.
          lwa_orditm_update-aedat = sy-datum.
          lwa_orditm_update-aezet = sy-uzeit.
          APPEND lwa_orditm_update TO lt_orditm_update.
        ENDIF.
      ENDLOOP. "lt_orditm
    ENDIF. "it_orditm

*--Order Confirmations table
    READ TABLE it_ordcnf TRANSPORTING NO FIELDS
                          WITH KEY ordno = lwa_orditm-ordno
                          BINARY SEARCH.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    lv_tabix = sy-tabix.
    LOOP AT it_ordcnf INTO DATA(lwa_ordcnf) FROM lv_tabix.
      IF lwa_ordcnf-ordno <> lwa_orditm-ordno.
        EXIT.
      ENDIF.

      IF lwa_ordcnf-updkz = zcl_abs_abap_maintain=>c_updkz_insert. "'I'
        MOVE-CORRESPONDING lwa_ordcnf TO lwa_ordcnf_insert.
        lwa_ordcnf_insert-ernam = sy-uname.
        lwa_ordcnf_insert-erdat = sy-datum.
        lwa_ordcnf_insert-erzet = sy-uzeit.
        APPEND lwa_ordcnf_insert TO lt_ordcnf_insert.

        READ TABLE lt_ordcnf_temp TRANSPORTING NO FIELDS
                                  WITH KEY ordno = lwa_ordcnf-ordno
                                           contr = lwa_ordcnf-contr
                                           vornr = lwa_ordcnf-vornr
                                  BINARY SEARCH.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
        lv_tabix = sy-tabix.
        LOOP AT lt_ordcnf_temp INTO DATA(lwa_ordcnf_temp) FROM lv_tabix.
          IF lwa_ordcnf_temp-ordno <> lwa_ordcnf-ordno.
            EXIT.
          ENDIF.
          IF sy-subrc = 0.
            lwa_messages-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
            lwa_messages-msgno = 126.
            lwa_messages-msgv1 = lwa_ordcnf_temp-ordno.
            lwa_messages-msgv2 = lwa_ordcnf_temp-vornr.
            lwa_messages-msgty = zcl_abs_abap_maintain=>c_msgty_info.
            APPEND lwa_messages TO et_messages.
            CLEAR lwa_messages.
          ENDIF.
          IF et_messages IS NOT INITIAL.
            CLEAR:lt_ordcnf_insert.
            lv_flag = abap_true.
            EXIT.
          ENDIF.
        ENDLOOP. "lwa_ordcnf_temp

      ELSEIF lwa_ordcnf-updkz = zcl_abs_abap_maintain=>c_updkz_update. "'U'
        MOVE-CORRESPONDING lwa_ordcnf TO lwa_ordcnf_update.
        lwa_ordcnf_update-aenam = sy-uname.
        lwa_ordcnf_update-aedat = sy-datum.
        lwa_ordcnf_update-aezet = sy-uzeit.
        APPEND lwa_ordcnf_update TO lt_ordcnf_update.

      ENDIF.
    ENDLOOP. "lt_ordcnf

    IF lv_flag = ''.

*--Order Consumptions table
      READ TABLE it_ordcon TRANSPORTING NO FIELDS
                            WITH KEY ordno = lwa_ordcnf-ordno
                            BINARY SEARCH.
      IF sy-subrc = 0.

        lv_tabix = sy-tabix.
        LOOP AT it_ordcon INTO DATA(lwa_ordcon) FROM lv_tabix.
          IF lwa_ordcon-ordno <> lwa_ordcnf-ordno.
            EXIT.
          ENDIF.

          IF lwa_ordcon-updkz = zcl_abs_abap_maintain=>c_updkz_insert. "'I'
            MOVE-CORRESPONDING lwa_ordcon TO lwa_ordcon_insert.
            lwa_ordcon_insert-ernam = sy-uname.
            lwa_ordcon_insert-erdat = sy-datum.
            lwa_ordcon_insert-erzet = sy-uzeit.
            APPEND lwa_ordcon_insert TO lt_ordcon_insert.
          ELSEIF lwa_ordcon-updkz = zcl_abs_abap_maintain=>c_updkz_update. "'U'
            MOVE-CORRESPONDING lwa_ordcon TO lwa_ordcon_update.
            lwa_ordcon_update-aenam = sy-uname.
            lwa_ordcon_update-aedat = sy-datum.
            lwa_ordcon_update-aezet = sy-uzeit.
            APPEND lwa_ordcon_update TO lt_ordcon_update.
          ENDIF.

          lwa_messages-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
          lwa_messages-msgno = 125.
          lwa_messages-msgv1 = lwa_var-next_no.
          lwa_messages-msgty = zcl_abs_abap_maintain=>c_msgty_success.
          APPEND lwa_messages TO et_messages.
          CLEAR lwa_messages.
        ENDLOOP. "lt_ordcon
      ENDIF. "LT_ORDCON

*--Order Groups table
      READ TABLE it_ordgrp TRANSPORTING NO FIELDS
                            WITH KEY ordno = lwa_ordhdr-ordno
                            BINARY SEARCH.
      IF sy-subrc = 0.

        lv_tabix = sy-tabix.
        LOOP AT it_ordgrp INTO DATA(lwa_ordgrp) FROM lv_tabix.
          IF lwa_ordgrp-ordno <> lwa_ordgrp-ordno.
            EXIT.
          ENDIF.
          IF lwa_ordgrp-updkz = zcl_abs_abap_maintain=>c_updkz_insert. "'I'
            MOVE-CORRESPONDING lwa_ordgrp TO lwa_ordgrp_insert.
            lwa_ordgrp_insert-ernam = sy-uname.
            lwa_ordgrp_insert-erdat = sy-datum.
            lwa_ordgrp_insert-erzet = sy-uzeit.
            APPEND lwa_ordgrp_insert TO lt_ordgrp_insert.
          ELSEIF lwa_ordgrp-updkz = zcl_abs_abap_maintain=>c_updkz_update. "'U'
            MOVE-CORRESPONDING lwa_ordgrp TO lwa_ordgrp_update.
            lwa_ordgrp_update-aenam = sy-uname.
            lwa_ordgrp_update-aedat = sy-datum.
            lwa_ordgrp_update-aezet = sy-uzeit.
            APPEND lwa_ordgrp_update TO lt_ordgrp_update.
          ENDIF.
        ENDLOOP. "lt_ordGRP
      ENDIF. "LT_ORDGRP

    ENDIF. "'flag"

  ENDLOOP. "lt_ordhdr

  IF lt_ordhdr_insert IS NOT INITIAL
    OR  lt_ordhdr_update IS NOT INITIAL.
*--Inserting and Updating Order Header table
    PERFORM db_update TABLES lt_ordhdr_insert
                             lt_ordhdr_update
                       USING c_tablename-c_ordhdr
                             et_messages.
  ENDIF.

  IF lt_orditm_insert IS NOT INITIAL
    OR lt_orditm_update IS NOT INITIAL.
*--Inserting and Updating Order Items table
    PERFORM db_update TABLES lt_orditm_insert
                             lt_orditm_update
                       USING c_tablename-c_orditm
                             et_messages.
  ENDIF.

  IF lt_ordcnf_insert IS NOT INITIAL
    OR lt_ordcnf_update IS NOT INITIAL.
*--Inserting and Updating Order Confirmations table
    PERFORM db_update TABLES lt_ordcnf_insert
                             lt_ordcnf_update
                       USING c_tablename-c_ordcnf
                             et_messages.
  ENDIF.

  IF lt_ordcon_insert IS NOT INITIAL
    OR lt_ordcon_update IS NOT INITIAL.
*--Inserting and Updating Order Consumptions table
    PERFORM db_update TABLES lt_ordcon_insert
                             lt_ordcon_update
                       USING c_tablename-c_ordcon
                             et_messages.
  ENDIF.

  IF lt_ordgrp_insert IS NOT INITIAL
   OR  lt_ordgrp_update IS NOT INITIAL.
*--Inserting and Updating Order Header table
    PERFORM db_update TABLES lt_ordgrp_insert
                             lt_ordgrp_update
                       USING c_tablename-c_ordgrp
                             et_messages.
  ENDIF.

ENDFUNCTION.
