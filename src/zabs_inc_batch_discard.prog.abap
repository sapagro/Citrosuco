************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Enhancement Name  :  ZABS_IENH_NURSERY_PROCESS                       *
* Form Name         :  FCODE_PROCESSING                                *
* Include Name      :  ZABS_INC_BATCH_DISCARD                          *
* Created By        :  Chandrakanth Karanam                            *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  06.24.2019                                      *
* TR                :  C4DK901784                                      *
* Version           :  001                                             *
* Description       :  Batch Discard functionality                     *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*--Internal Tables
DATA : lt_ocdoc    TYPE /agri/t_fmoc_doc,
       lt_rows     TYPE lvc_t_row,
       lt_return   TYPE TABLE OF bapiret2,
       lt_items_gr TYPE TABLE OF bapi2017_gm_item_create,
       lt_items_gi TYPE TABLE OF bapi2017_gm_item_create,
       lt_ocindx   TYPE STANDARD TABLE OF /agri/fmocindx,
       lt_ochdr    TYPE STANDARD TABLE OF /agri/fmochdr.

*--Workareas
DATA : lwa_ochdr        TYPE /agri/fmochdr,
       lwa_tcoko        TYPE tcoko,
       lwa_rows         TYPE lvc_s_row,
       lwa_return       TYPE bapiret2,
       lwa_batches_fcat TYPE /agri/s_fmfpbch_fcat,
       lwa_grheader_gr  TYPE bapi2017_gm_head_01,
       lwa_item         TYPE bapi2017_gm_item_create,
       lwa_mvt_code     TYPE bapi2017_gm_code,
       lwa_fpoc_doc     TYPE /agri/s_fmoc_doc,
       lwa_ocindx       TYPE /agri/s_fmocindx,
       lwa_marc         TYPE marc,
       lwa_grheader     TYPE bapi2017_gm_head_ret.

DATA : lv_errors,
       lv_subrc    TYPE sysubrc,
       lv_conf     TYPE xfeld,
       lv_cancel   TYPE itex132,
       lv_continue TYPE itex132,
       lv_titlebar TYPE itex132,
       lv_question TYPE itex132,
       lv_answer   TYPE c.

IF fcode EQ zcl_abs_abap_maintain=>c_fcode_btch_canc.
*--Cancel/Cancelar
  MESSAGE i090(zfmfp) INTO lv_cancel.
*--Cotinue/Continuar
  MESSAGE i091(zfmfp) INTO lv_continue.
*--Batch Cancellation/Estornar Lote
  MESSAGE i092(zfmfp) INTO lv_titlebar.
*--Do you want to continue to do batch cancellation?
*--Confirmar o estorno do lote?
  MESSAGE i093(zfmfp) INTO lv_question.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = lv_titlebar
      text_question         = lv_question
      text_button_1         = lv_continue
      text_button_2         = lv_cancel
      default_button        = '2'
      display_cancel_button = ''
    IMPORTING
      answer                = lv_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  IF lv_answer EQ '2'.
    RETURN.
  ENDIF.
ENDIF.

IF gs_variables-initiator IS INITIAL.
  gs_variables-initiator = c_log_initiator-check.
  PERFORM messages_initialize USING gs_variables-initiator
                                    c_log_subobject-check.
ENDIF.

*--Getting Selected Rows
CALL METHOD ref_grid_batches->get_selected_rows
  IMPORTING
    et_index_rows = lt_rows.

*--User should select atleast one batch
IF lt_rows IS INITIAL.
  MESSAGE i024(zabs_msgcls).
  RETURN.
ENDIF.

CLEAR lv_conf.

IF fcode EQ zcl_abs_abap_maintain=>c_fcode_btch_discard.
  LOOP AT lt_rows INTO lwa_rows.
    READ TABLE gt_batches_fcat INTO lwa_batches_fcat INDEX lwa_rows-index.
    IF sy-subrc = 0.

      IF lwa_batches_fcat-zzconf IS INITIAL.
        MESSAGE e139(zabs_msgcls)
           WITH lwa_batches_fcat-charg
           INTO sy-msgli.
        message_simple space.
        lv_errors = abap_true.
        CONTINUE.
      ENDIF.

      lv_conf = abap_true.

      IF lwa_batches_fcat-zzgwemg IS INITIAL.
        MESSAGE e028(zabs_msgcls)
           WITH lwa_batches_fcat-charg
           INTO sy-msgli.
        message_simple space.
        lv_errors = abap_true.
        CONTINUE.
      ENDIF.

      IF ( lwa_batches_fcat-gwemg - lwa_batches_fcat-zzdiscarded_qty )
                                           LT lwa_batches_fcat-zzgwemg.
        MESSAGE e029(zabs_msgcls)
           WITH lwa_batches_fcat-charg
           INTO sy-msgli.
        message_simple space.
        lv_errors = abap_true.
        CONTINUE.
      ENDIF.
    ENDIF.
  ENDLOOP.

ELSE.

  CLEAR lv_conf.
  LOOP AT lt_rows INTO lwa_rows.
    READ TABLE gt_batches_fcat INTO lwa_batches_fcat INDEX lwa_rows-index.
    IF sy-subrc = 0 AND lwa_batches_fcat-zzconf IS NOT INITIAL.
      lv_conf = abap_true.
    ENDIF.
  ENDLOOP.
ENDIF.

IF lv_errors IS INITIAL AND lv_conf IS NOT INITIAL.
*--Fetching Order Confirmation Data based on Order Number
  SELECT * FROM /agri/fmocindx
    INTO TABLE lt_ocindx
   WHERE aufnr = gs_fpdoc_infocus-aufnr.
  IF sy-subrc EQ 0.
    SELECT * FROM /agri/fmochdr
      INTO TABLE lt_ochdr
       FOR ALL ENTRIES IN lt_ocindx
     WHERE ocnum = lt_ocindx-ocnum.
    IF sy-subrc EQ 0.
      DELETE lt_ochdr WHERE ocnum_ref IS NOT INITIAL.
      SORT lt_ochdr BY charg_ref contr_ref.
    ENDIF.

    IF lt_ochdr[] IS NOT INITIAL.
      SELECT * FROM /agri/fmocopr
        INTO TABLE @DATA(lt_ocopr)
        FOR ALL ENTRIES IN @lt_ochdr
       WHERE ocnum EQ @lt_ochdr-ocnum.
      IF sy-subrc EQ 0.
        SORT lt_ocopr BY ocnum rueck rmzhl.

        SELECT * FROM /agri/fmoccom
          INTO TABLE @DATA(lt_occom)
          FOR ALL ENTRIES IN @lt_ocopr
         WHERE ocnum EQ @lt_ocopr-ocnum
           AND rueck EQ @lt_ocopr-rueck
           AND rmzhl EQ @lt_ocopr-rmzhl.
        IF sy-subrc EQ 0.
          SORT lt_occom BY ocnum rueck rmzhl contr.
        ENDIF.

        SELECT *
          FROM afwi
          INTO TABLE @DATA(lt_afwi)
          FOR ALL ENTRIES IN @lt_ocopr
         WHERE rueck EQ @lt_ocopr-rueck
           AND rmzhl EQ @lt_ocopr-rmzhl.
        IF sy-subrc EQ 0.
          SORT lt_afwi BY rueck rmzhl mblnr mjahr mblpo.

          SELECT * FROM matdoc
            INTO TABLE @DATA(lt_mseg)
            FOR ALL ENTRIES IN @lt_afwi
           WHERE mblnr EQ @lt_afwi-mblnr
             AND mjahr EQ @lt_afwi-mjahr
             AND zeile EQ @lt_afwi-mblpo.
          IF sy-subrc EQ 0.
            SORT lt_mseg BY mblnr mjahr zeile.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDIF.

IF lv_errors IS INITIAL.

  LOOP AT lt_rows INTO lwa_rows.

    READ TABLE gt_batches_fcat ASSIGNING FIELD-SYMBOL(<fs_batches_fcat>)
                               INDEX lwa_rows-index.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    IF <fs_batches_fcat>-zzconf IS NOT INITIAL.
      CLEAR lwa_grheader_gr.
      lwa_grheader_gr-pstng_date = sy-datum.

      CLEAR lwa_item.
      lwa_item-prod_date = sy-datum.
      lwa_item-material  = <fs_batches_fcat>-matnr.
      IF fcode EQ zcl_abs_abap_maintain=>c_fcode_btch_canc.
        lwa_item-move_type = zcl_abs_abap_maintain=>c_move_type_po_reversal."'102'.
        lwa_item-entry_qnt = <fs_batches_fcat>-gwemg.
      ELSE.
        lwa_item-move_type = zcl_abs_abap_maintain=>c_move_type_batch_disc."'Y91'.
        lwa_item-entry_qnt = <fs_batches_fcat>-zzgwemg.
      ENDIF.
      lwa_item-entry_uom = <fs_batches_fcat>-erfme.

      IF lwa_item-move_type NE zcl_abs_abap_maintain=>c_move_type_batch_disc."'Y91'.
        lwa_item-mvt_ind = zcl_abs_abap_maintain=>c_mvt_ind_prod_order. "'F'.
        lwa_item-orderid = <fs_batches_fcat>-aufnr.
        lwa_mvt_code-gm_code = zcl_abs_abap_maintain=>c_gm_code_consumption. "'02'.
      ELSE.
        IF <fs_batches_fcat>-matnr IS NOT INITIAL
        AND gs_fpdoc_infocus-x-fphdr-iwerk IS NOT INITIAL.
          CALL FUNCTION 'MARC_SINGLE_READ'
            EXPORTING
              matnr             = <fs_batches_fcat>-matnr
              werks             = gs_fpdoc_infocus-x-fphdr-iwerk
            IMPORTING
              wmarc             = lwa_marc
            EXCEPTIONS
              lock_on_marc      = 1
              lock_system_error = 2
              wrong_call        = 3
              not_found         = 4
              OTHERS            = 5.

          IF sy-subrc EQ 0.
            lwa_item-stge_loc   = lwa_marc-lgpro.
            lwa_item-profit_ctr = lwa_marc-prctr.
            lwa_item-gl_account = '6301020009'.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = lwa_item-gl_account
              IMPORTING
                output = lwa_item-gl_account.
          ENDIF.
        ENDIF.
        lwa_mvt_code-gm_code = '03'.
      ENDIF.

      lwa_item-plant     = gs_fpdoc_infocus-x-fphdr-iwerk.
      lwa_item-batch     = <fs_batches_fcat>-charg.
      APPEND lwa_item TO lt_items_gr.

      REFRESH lt_return.
      CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
        EXPORTING
          goodsmvt_header  = lwa_grheader_gr
          goodsmvt_code    = lwa_mvt_code
        IMPORTING
          goodsmvt_headret = lwa_grheader
        TABLES
          goodsmvt_item    = lt_items_gr
          return           = lt_return.

      gs_variables-item_infocus = <fs_batches_fcat>-contr.
      PERFORM messages_context_set USING gs_fpdoc_infocus-x-fphdr.
      CLEAR: gs_variables-item_infocus.

*--Collect Messages
      LOOP AT lt_return INTO lwa_return
                       WHERE type EQ c_msg_type-error
                          OR type EQ c_msg_type-abend.
        MESSAGE ID lwa_return-id TYPE c_msg_type-error
         NUMBER lwa_return-number
           WITH lwa_return-message_v1 lwa_return-message_v2
                lwa_return-message_v3 lwa_return-message_v4
           INTO sy-msgli.
        message_simple space.
        EXIT.
      ENDLOOP.
      IF sy-subrc EQ 0.
        lv_errors = abap_true.
        CONTINUE.
      ELSE.
*--Commit if no errors
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = c_true.
        MESSAGE s015(zabs_msgcls) WITH  <fs_batches_fcat>-charg
           INTO sy-msgli.
        message_simple space.

        CLEAR lwa_fpoc_doc.

        MOVE-CORRESPONDING gs_fpdoc_infocus-x-fphdr TO lwa_fpoc_doc-x-ochdr.
        CLEAR: lwa_fpoc_doc-x-ochdr-updkz,
               lwa_fpoc_doc-x-ochdr-matnr,
               lwa_fpoc_doc-x-ochdr-charg.

        lwa_fpoc_doc-x-ochdr-charg_ref = <fs_batches_fcat>-charg.
        lwa_fpoc_doc-x-ochdr-contr_ref = <fs_batches_fcat>-contr.
        READ TABLE lt_ochdr INTO lwa_ochdr
              WITH KEY charg_ref = <fs_batches_fcat>-charg
                       contr_ref = <fs_batches_fcat>-contr
            BINARY SEARCH.
        IF sy-subrc EQ 0.
          lwa_fpoc_doc-x-ochdr-ocnum_ref = lwa_ochdr-ocnum.
        ENDIF.
*        lwa_fpoc_doc-x-ochdr-menge = <fs_batches_fcat>-zzgwemg.
        lwa_fpoc_doc-x-ochdr-menge = lwa_item-entry_qnt.
        lwa_fpoc_doc-x-ochdr-meins = gs_fpdoc_infocus-x-fphdr-gmein.
        lwa_fpoc_doc-x-ochdr-matnr = gs_fpdoc_infocus-x-fphdr-matnr.
        lwa_fpoc_doc-x-ochdr-charg = <fs_batches_fcat>-charg.
        lwa_fpoc_doc-x-ochdr-updkz = c_updkz_new.

        CLEAR lwa_ocindx.
        lwa_ocindx-aufnr = gs_fpdoc_infocus-aufnr.
        lwa_ocindx-gwemg = lwa_fpoc_doc-x-ochdr-menge.
        lwa_ocindx-gmein = lwa_fpoc_doc-x-ochdr-meins.
        lwa_ocindx-zzmblnr = lwa_grheader-mat_doc.
        lwa_ocindx-zzmjahr = lwa_grheader-doc_year.
        lwa_ocindx-updkz = c_updkz_new.
        APPEND lwa_ocindx TO lwa_fpoc_doc-x-ocindx.
        APPEND lwa_fpoc_doc TO lt_ocdoc.

        IF fcode EQ zcl_abs_abap_maintain=>c_fcode_btch_canc.
          READ TABLE gs_fpdoc_infocus-x-fpbch ASSIGNING FIELD-SYMBOL(<fs_infocus_fpbch>)
          WITH KEY aufnr = <fs_batches_fcat>-aufnr
                   contr = <fs_batches_fcat>-contr
                   charg = <fs_batches_fcat>-charg.
          IF sy-subrc = 0.
            <fs_infocus_fpbch>-zzloevm = abap_true.
            <fs_infocus_fpbch>-updkz   = zcl_abs_abap_maintain=>c_updkz_update. "'U'
          ENDIF.
        ELSE.
          READ TABLE gs_fpdoc_infocus-x-fpbch ASSIGNING <fs_infocus_fpbch>
          WITH KEY aufnr = <fs_batches_fcat>-aufnr
                   contr = <fs_batches_fcat>-contr
                   charg = <fs_batches_fcat>-charg.
          IF sy-subrc = 0.
            <fs_infocus_fpbch>-zzdiscarded_qty = <fs_infocus_fpbch>-zzdiscarded_qty +
                                                 <fs_batches_fcat>-zzgwemg.
            <fs_infocus_fpbch>-updkz   = zcl_abs_abap_maintain=>c_updkz_update. "'U'
            CLEAR: <fs_batches_fcat>-zzgwemg, <fs_infocus_fpbch>-zzgwemg.
          ENDIF.
        ENDIF.
      ENDIF.
      IF <fs_batches_fcat>-charg IS ASSIGNED
      AND <fs_batches_fcat>-charg IS NOT INITIAL
      AND fcode EQ zcl_abs_abap_maintain=>c_fcode_btch_canc.
        READ TABLE lt_ochdr INTO DATA(ls_ochdr)
          WITH KEY charg_ref = <fs_batches_fcat>-charg BINARY SEARCH.
        IF sy-subrc EQ 0.
          READ TABLE lt_occom INTO DATA(ls_occom)
            WITH KEY ocnum = ls_ochdr-ocnum BINARY SEARCH.
          IF sy-subrc EQ 0.
            READ TABLE lt_afwi INTO DATA(ls_awfi)
              WITH KEY rueck = ls_occom-rueck
                       rmzhl = ls_occom-rmzhl BINARY SEARCH.
            IF sy-subrc EQ 0.
              READ TABLE lt_mseg TRANSPORTING NO FIELDS
                WITH KEY mblnr = ls_awfi-mblnr
                         mjahr = ls_awfi-mjahr BINARY SEARCH.
              IF sy-subrc EQ 0.
                PERFORM co_constants_read CHANGING lwa_tcoko.
                LOOP AT lt_mseg INTO DATA(ls_mseg) FROM sy-tabix.
                  IF ls_mseg-mblnr <> ls_awfi-mblnr
                  OR ls_mseg-mjahr <> ls_awfi-mjahr.
                    EXIT.
                  ENDIF.
                  INSERT INITIAL LINE INTO TABLE lt_items_gi
                    ASSIGNING FIELD-SYMBOL(<ls_item_gi>).
                  IF sy-subrc EQ 0.
                    <ls_item_gi>-material      = ls_mseg-matnr.
                    <ls_item_gi>-plant         = ls_mseg-werks.
                    <ls_item_gi>-stge_loc      = ls_mseg-lgort.
                    <ls_item_gi>-batch         = ls_mseg-charg.
                    <ls_item_gi>-move_type     = lwa_tcoko-wa_bwarts.
                    <ls_item_gi>-entry_qnt     = ls_mseg-menge.
                    <ls_item_gi>-entry_uom     = ls_mseg-meins.
                    <ls_item_gi>-orderid       = ls_mseg-aufnr.
                    <ls_item_gi>-prod_date     = sy-datum. "ls_mseg-budat.
                  ENDIF.
                ENDLOOP.
                REFRESH lt_return.
                lwa_mvt_code = '06'.
                lwa_grheader_gr-pstng_date = sy-datum. "ls_mseg-budat.
                PERFORM goods_movement_create TABLES lt_items_gi
                                                     lt_return
                                               USING abap_true
                                                     lwa_mvt_code
                                                     lwa_grheader_gr
                                            CHANGING lv_subrc.
                IF lv_subrc EQ 0.
                  LOOP AT lt_items_gi ASSIGNING <ls_item_gi>.
                    MESSAGE s015(zabs_msgcls) WITH <ls_item_gi>-batch
                       INTO sy-msgli.
                    message_simple space.
                  ENDLOOP.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      READ TABLE gs_fpdoc_infocus-x-fpbch ASSIGNING <fs_infocus_fpbch>
      WITH KEY aufnr = <fs_batches_fcat>-aufnr
               contr = <fs_batches_fcat>-contr
               charg = <fs_batches_fcat>-charg.
      IF sy-subrc = 0.
        <fs_infocus_fpbch>-zzloevm = abap_true.
        <fs_infocus_fpbch>-updkz   = zcl_abs_abap_maintain=>c_updkz_update. "'U'
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF lt_ocdoc IS NOT INITIAL.
*--Perform Update Confirmations
    PERFORM update_confirmations TABLES lt_ocdoc
                                  USING c_true.
  ENDIF.

  IF lv_errors IS INITIAL.
    PERFORM document_infocus_save USING abap_true
                               CHANGING gs_fpdoc_infocus
                                        lv_subrc.
  ENDIF.

ENDIF.

PERFORM messages_display USING gs_variables-initiator.
