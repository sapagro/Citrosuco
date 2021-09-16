************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Enhancement Name  :  ZABS_IENH_NURSERY_PROCESS                       *
* Form Name         :  COMPONENTS_DATA_DISPLAY                         *
* Include Name      :  ZABS_INC_BATCH_DISPLAY                          *
* Created By        :  Chandrakanth Karanam                            *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  06.26.2019                                      *
* TR                :  C4DK901784                                      *
* Version           :  001                                             *
* Description       :  Split the Consumption Quantity as batch wise    *
*                      for the batch enabled consumption material      *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*--Types declaration
TYPES: BEGIN OF ty_mchb,
         matnr TYPE matnr,
         charg TYPE charg_d,
         clabs TYPE labst,
         laeda TYPE dats,
         grcvr TYPE atwrt,
         grcgo TYPE atwrt,
         werks TYPE werks_d,
         lgort TYPE lgort_d,
       END OF ty_mchb.

TYPES: BEGIN OF ty_char_value,
         atwrt      TYPE atwrt,
         codegruppe TYPE qcodegrp,
         code       TYPE qcode,
       END OF ty_char_value.

TYPES: BEGIN OF ty_value,
         codegruppe TYPE qpk1ac-codegruppe,
         fillr(1)   TYPE c,
         code       TYPE qpk1ac-code,
       END OF ty_value.

*--Internal Tables
DATA : lt_mchb       TYPE TABLE OF ty_mchb,
       lt_char_batch TYPE STANDARD TABLE OF clbatch,
       lt_fpcom      TYPE /agri/t_fmfpcom,
       lt_char_value TYPE STANDARD TABLE OF ty_char_value.

*--Workarea declarations
DATA : ls_fpcom_in     TYPE /agri/s_fmfpcom,
       ls_task_itm_mod TYPE lvc_s_modi,
       ls_mchb         TYPE ty_mchb,
       ls_fpcom        TYPE /agri/s_fmfpcom,
       ls_fpcnf_fcat   TYPE /agri/s_fmfp_cnf_fcat,
       ls_char_batch   TYPE clbatch,
       ls_char_value   TYPE ty_char_value,
       ls_value        TYPE ty_value.

*--Field-Symbols
FIELD-SYMBOLS : <fs_mchb> TYPE ty_mchb.

*--Local Variables
DATA : lv_date         TYPE string,
       lv_tabix        TYPE sy-tabix,
       lv_contr        TYPE /agri/fmccontr,
       lv_lmnga        TYPE /agri/fmlmnga,
       lv_grid_refresh.

DATA: lv_confirm_qty_x    TYPE /agri/fmlmnga,
      lwa_fpcom_x         TYPE /agri/s_fmfpcom,
      lwa_fpcom_fcat_x    TYPE /agri/s_fmfpcom_fcat,
      lwa_fpdoc_infocus_x TYPE /agri/s_fmfp_doc,
      lwa_makt_x          TYPE makt,
      lwa_edit_x          TYPE lvc_s_styl.

CALL FUNCTION 'ZABS_FM_GET_GRID_REFRESH'
  IMPORTING
    ev_grid_refresh = lv_grid_refresh.

CALL FUNCTION 'ZABS_FM_SET_GRID_REFRESH'
  EXPORTING
    iv_grid_refresh = ''.

IF fcode EQ zcl_abs_abap_maintain=>c_ucomm_batch_conf
OR fcode EQ zcl_abs_abap_maintain=>c_batch_details
OR gs_variables-cust_scr = c_screen-batches.
  LOOP AT gt_fpcnf_fcat INTO ls_fpcnf_fcat.
    LOOP AT gs_fpdoc_infocus-x-fpcom INTO ls_fpcom
                    WHERE aufnr EQ ls_fpcnf_fcat-aufnr
                      AND posnr EQ ls_fpcnf_fcat-posnr
                      AND flgch EQ zcl_abs_abap_maintain=>c_costing_rel_full "'X'
                      AND zztask IS INITIAL.
      CLEAR ls_mchb.
      ls_mchb-matnr = ls_fpcom-matnr.
      ls_mchb-werks = ls_fpcom-werks.
      ls_mchb-lgort = ls_fpcom-lgort.
      COLLECT ls_mchb INTO lt_mchb.
    ENDLOOP.
  ENDLOOP.

  IF lt_mchb[] IS NOT INITIAL.
*--Fetching Batch Stock data
    SELECT matnr charg
           clabs laeda
      FROM mchb
      INTO TABLE lt_mchb
      FOR ALL ENTRIES IN lt_mchb
     WHERE matnr EQ lt_mchb-matnr
       AND werks EQ lt_mchb-werks
       AND lgort EQ lt_mchb-lgort.

    IF sy-subrc EQ 0.
      SORT lt_mchb BY matnr.
    ENDIF.
  ENDIF.

*--Getting Characteristic Value based on Batch and Material
  LOOP AT lt_mchb ASSIGNING <fs_mchb>.
    REFRESH lt_char_batch.
    CALL FUNCTION 'VB_BATCH_GET_DETAIL'
      EXPORTING
        matnr              = <fs_mchb>-matnr
        charg              = <fs_mchb>-charg
        get_classification = zcl_abs_abap_maintain=>c_costing_rel_full  "'X'
      TABLES
        char_of_batch      = lt_char_batch.

    CLEAR ls_char_batch.
    READ TABLE lt_char_batch INTO ls_char_batch
    WITH KEY atnam = zcl_abs_abap_maintain=>c_charact_psdat. "'ZLOBM_HSDAT'
    IF sy-subrc = 0.
*--calling FM to convert date format to SAP date format
      CALL FUNCTION 'CONVERSION_EXIT_SDATE_INPUT'
        EXPORTING
          input  = ls_char_batch-atwtb
        IMPORTING
          output = <fs_mchb>-laeda.
    ENDIF.

    CLEAR ls_char_batch.
    READ TABLE lt_char_batch INTO ls_char_batch
    WITH KEY atnam = 'PPMPV_0005'.
    IF sy-subrc = 0.
      <fs_mchb>-grcvr = ls_char_batch-atwtb.
      CLEAR ls_value.
      ls_value = ls_char_batch-atwtb.

      CLEAR ls_char_value.
      ls_char_value-atwrt = ls_char_batch-atwtb.
      ls_char_value-codegruppe = ls_value-codegruppe.
      ls_char_value-code = ls_value-code.
      APPEND ls_char_value TO lt_char_value.
    ENDIF.

    CLEAR ls_char_batch.
    READ TABLE lt_char_batch INTO ls_char_batch
    WITH KEY atnam = 'PPMPV_0002'.
    IF sy-subrc = 0.
      <fs_mchb>-grcgo = ls_char_batch-atwtb.
      CLEAR ls_value.
      ls_value = ls_char_batch-atwtb.

      CLEAR ls_char_value.
      ls_char_value-atwrt = ls_char_batch-atwtb.
      ls_char_value-codegruppe = ls_value-codegruppe.
      ls_char_value-code = ls_value-code.
      APPEND ls_char_value TO lt_char_value.
    ENDIF.
  ENDLOOP.

  SORT lt_char_value BY atwrt.
  DELETE ADJACENT DUPLICATES FROM lt_char_value COMPARING atwrt.

  IF lt_char_value IS NOT INITIAL.
    SELECT katalogart, codegruppe, code, kurztext
      FROM qpct
      INTO TABLE @DATA(lt_qpct)
       FOR ALL ENTRIES IN @lt_char_value
     WHERE codegruppe EQ @lt_char_value-codegruppe
       AND code       EQ @lt_char_value-code
       AND sprache    EQ @sy-langu.
    IF sy-subrc EQ 0.
      SORT lt_qpct BY codegruppe code.
    ENDIF.
  ENDIF.

  SORT lt_mchb BY matnr laeda.

  REFRESH lt_fpcom.
  LOOP AT gs_fpdoc_infocus-x-fpcom INTO ls_fpcom
                                   WHERE zztask IS INITIAL.
    READ TABLE <gs_fpdoc_infocus>-x-fpcom INTO ls_fpcom_in
      WITH KEY aufnr = ls_fpcom-aufnr
               posnr = ls_fpcom-posnr
               matnr = ls_fpcom-matnr.
    IF sy-subrc EQ 0.
      APPEND ls_fpcom_in TO lt_fpcom.
    ENDIF.
  ENDLOOP.

  CLEAR <gs_fpdoc_infocus>-x-fpcom.
  LOOP AT gt_fpcnf_fcat INTO ls_fpcnf_fcat.
    CLEAR lv_contr.

*--Splitting the Consumption Quantity as batch wise
    LOOP AT lt_fpcom INTO ls_fpcom
                     WHERE aufnr EQ ls_fpcnf_fcat-aufnr
                       AND posnr EQ ls_fpcnf_fcat-posnr.
      IF ls_fpcom-flgch IS INITIAL.
        lv_contr = lv_contr + 1.
        ls_fpcom-contr = lv_contr.
        APPEND ls_fpcom TO <gs_fpdoc_infocus>-x-fpcom.
      ELSE.
        lv_lmnga = ls_fpcnf_fcat-lmnga.

        READ TABLE lt_mchb ASSIGNING <fs_mchb>
        WITH KEY matnr = ls_fpcom-matnr BINARY SEARCH .
        IF sy-subrc = 0.
          lv_tabix = sy-tabix.
          LOOP AT lt_mchb ASSIGNING <fs_mchb> FROM lv_tabix.
            IF <fs_mchb>-matnr <> ls_fpcom-matnr.
              EXIT.
            ENDIF.
            ls_fpcom-charg = <fs_mchb>-charg.

            CLEAR ls_char_value.
            READ TABLE lt_char_value INTO ls_char_value
                  WITH KEY atwrt = <fs_mchb>-grcvr.
            IF sy-subrc EQ 0.
              READ TABLE lt_qpct INTO DATA(ls_qpct)
                    WITH KEY codegruppe = ls_char_value-codegruppe
                             code       = ls_char_value-code
                  BINARY SEARCH.
              IF sy-subrc EQ 0.
                ls_fpcom-zzgrcvr = ls_qpct-kurztext.
              ENDIF.
            ENDIF.

            CLEAR ls_char_value.
            READ TABLE lt_char_value INTO ls_char_value
                  WITH KEY atwrt = <fs_mchb>-grcgo.
            IF sy-subrc EQ 0.
              CLEAR ls_qpct.
              READ TABLE lt_qpct INTO ls_qpct
                    WITH KEY codegruppe = ls_char_value-codegruppe
                             code       = ls_char_value-code
                  BINARY SEARCH.
              IF sy-subrc EQ 0.
                ls_fpcom-zzgrcgo = ls_qpct-kurztext.
              ENDIF.
            ENDIF.

            IF lv_lmnga > <fs_mchb>-clabs.
              ls_fpcom-lmnga = <fs_mchb>-clabs.
              lv_lmnga = lv_lmnga - <fs_mchb>-clabs.
            ELSE.
              ls_fpcom-lmnga = lv_lmnga.
              lv_lmnga = 0.
            ENDIF.
            lv_contr = lv_contr + 1.
            ls_fpcom-contr = lv_contr.
            ls_fpcom-zzclabs = <fs_mchb>-clabs.
            APPEND ls_fpcom TO <gs_fpdoc_infocus>-x-fpcom.
          ENDLOOP.
          IF sy-subrc NE 0.
            lv_contr = lv_contr + 1.
            ls_fpcom-contr = lv_contr.
            APPEND ls_fpcom TO <gs_fpdoc_infocus>-x-fpcom.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  DELETE <gs_fpdoc_infocus>-x-fpcom WHERE zzclabs IS INITIAL.

*  IF <gs_fpdoc_infocus> IS ASSIGNED.
*    DATA(lv_components) = lines( gt_components ).
*    DATA(lv_fpcom) = lines( <gs_fpdoc_infocus>-x-fpcom ).
*    DATA(lt_components) = gt_components[].
*    DELETE lt_components WHERE updkz NE 'I'.
*    IF lv_components NE lv_fpcom.
*      LOOP AT lt_components INTO DATA(ls_component).
*        CLEAR ls_fpcom.
*        MOVE-CORRESPONDING ls_component TO ls_fpcom.
*        lv_contr = lv_contr + 1.
*        ls_fpcom-contr = lv_contr.
*        APPEND ls_fpcom TO <gs_fpdoc_infocus>-x-fpcom.
*      ENDLOOP.
*    ENDIF.
*  ENDIF.

  CHECK gs_variables-refresh_components_grid IS NOT INITIAL
     OR ref_grid_components IS INITIAL.

  IF lv_grid_refresh EQ abap_false.
    REFRESH: gt_components.
    IF <gs_fpdoc_infocus> IS ASSIGNED.
      LOOP AT <gs_fpdoc_infocus>-x-fpcom ASSIGNING FIELD-SYMBOL(<lwa_fpcom_x>)
                                       WHERE aufnr EQ /agri/s_fmfp_cnf-aufnr
                                         AND posnr EQ /agri/s_fmfp_cnf-posnr.
        MOVE-CORRESPONDING <lwa_fpcom_x> TO lwa_fpcom_fcat_x.
        key_text_get 'MAKT' 'MATNR' lwa_fpcom_fcat_x-matnr
                 lwa_makt_x lwa_fpcom_fcat_x-maktx.
        PERFORM components_styles_prepare CHANGING lwa_fpcom_fcat_x.
        CLEAR lwa_fpcom_fcat_x-lmnga.
        READ TABLE lwa_fpcom_fcat_x-styles ASSIGNING FIELD-SYMBOL(<lwa_style>)
          WITH KEY fieldname = 'BWART'.
        IF sy-subrc EQ 0.
          DELETE lwa_fpcom_fcat_x-styles INDEX sy-tabix.
        ENDIF.
        APPEND lwa_fpcom_fcat_x TO gt_components.
      ENDLOOP.
    ENDIF.
  ELSE.
    IF gt_batches_mod_rows_x[] IS NOT INITIAL
    AND <gs_fpdoc_infocus> IS ASSIGNED.
      LOOP AT gt_batches_mod_rows_x INTO DATA(lwa_mod_row_x).
        READ TABLE gt_components INTO DATA(lwa_component)
          INDEX lwa_mod_row_x-row_id.
        IF sy-subrc EQ 0.
          READ TABLE <gs_fpdoc_infocus>-x-fpcom ASSIGNING <lwa_fpcom_x>
            WITH KEY aufnr = lwa_component-aufnr
                     posnr = lwa_component-posnr
                     contr = lwa_component-contr
                     vornr = lwa_component-vornr
                     matnr = lwa_component-matnr.
          IF sy-subrc EQ 0.
            MOVE-CORRESPONDING lwa_component TO <lwa_fpcom_x>.
          ENDIF.
        ENDIF.
      ENDLOOP.

      REFRESH gt_batches_mod_rows_x.
    ENDIF.
  ENDIF.
ELSEIF fcode EQ 'TORD'
    OR ( lv_grid_refresh EQ abap_true AND
         gs_variables-cust_scr = c_screen-items ).

  CHECK gs_variables-refresh_components_grid IS NOT INITIAL
     OR ref_grid_components IS INITIAL.

  IF <gs_fpdoc_infocus>-x-fpcom[] IS NOT INITIAL.
    SELECT matnr, bwkey, bwtar, lbkum,
           bklas, mtorg, mtuse
      FROM mbew
      INTO TABLE @DATA(lt_mbew_x)
      FOR ALL ENTRIES IN @<gs_fpdoc_infocus>-x-fpcom
     WHERE matnr = @<gs_fpdoc_infocus>-x-fpcom-matnr
       AND bwkey = @<gs_fpdoc_infocus>-x-fpcom-werks.
    SORT lt_mbew_x BY matnr bwkey.
  ENDIF.

  REFRESH: gt_components.
  LOOP AT <gs_fpdoc_infocus>-x-fpcom ASSIGNING <lwa_fpcom_x>
                                   WHERE aufnr EQ /agri/s_fmfp_cnf-aufnr
                                     AND posnr EQ /agri/s_fmfp_cnf-posnr.
    MOVE-CORRESPONDING <lwa_fpcom_x> TO lwa_fpcom_fcat_x.
    key_text_get 'MAKT' 'MATNR' lwa_fpcom_fcat_x-matnr
             lwa_makt_x lwa_fpcom_fcat_x-maktx.
    READ TABLE lt_mbew_x INTO DATA(lwa_mbew_x)
      WITH KEY matnr = <lwa_fpcom_x>-matnr
               bwkey = <lwa_fpcom_x>-werks BINARY SEARCH.
    IF sy-subrc EQ 0.
      lwa_fpcom_fcat_x-zzclabs = lwa_mbew_x-lbkum.
      <lwa_fpcom_x>-zzclabs = lwa_mbew_x-lbkum.
    ENDIF.
    PERFORM components_styles_prepare CHANGING lwa_fpcom_fcat_x.
    APPEND lwa_fpcom_fcat_x TO gt_components.
  ENDLOOP.
ENDIF.
