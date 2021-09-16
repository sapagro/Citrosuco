************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Enhancement Name  :  ZABS_IENH_NURSERY_PROCESS                       *
* Form Name         :  post_batch_confirmations                        *
* Include Name      :  ZABS_INC_BATCH_UPDATE                           *
* Created By        :  Chandrakanth Karanam                            *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  06.18.2019                                      *
* TR                :  C4DK901784                                      *
* Version           :  001                                             *
* Description       :  Fill the batch characteristic (Date) when       *
*                      GRN created                                     *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*
*--Types
  TYPES: lty_goodsmovements TYPE TABLE OF bapi2017_gm_item_show,
         lty_failedmoves    TYPE TABLE OF bapi_coru_fgm,
         lty_pp_conf_detail TYPE bapi_pp_confirm.

*--Local Variables
  DATA : lv_objectkey TYPE bapi1003_key-object_long,
         lv_classnum  TYPE bapi1003_key-classnum,
         lv_float     TYPE cawn-atflv,
         lv_atwrt     TYPE cawn-atwrt.

*--Internal Tables
  DATA : lt_allocvaluescurr  TYPE tt_bapi1003_alloc_values_curr,
         lt_allocvaluesnum   TYPE tt_bapi1003_alloc_values_num,
         lt_allocvalueschar  TYPE tt_bapi1003_alloc_values_char,
         lt_return_x         TYPE bapiret2_tab,
         lt_char             TYPE STANDARD TABLE OF api_char,
         lt_goodsmovem_bapi  TYPE lty_goodsmovements,
         lt_mat_doc          TYPE lty_goodsmovements,
         lt_failedmoves_bapi TYPE lty_failedmoves.

*--Workarea declaration
  DATA : ls_allocvaluesnum  TYPE bapi1003_alloc_values_num,
         ls_allocvalueschar TYPE bapi1003_alloc_values_char,
         ls_allocvaluescurr TYPE bapi1003_alloc_values_curr,
         ls_fmfpcnf         TYPE /agri/s_fmfp_cnf,
         ls_msgs            TYPE bapiret1,
         ls_pp_conf_detail  TYPE lty_pp_conf_detail.

*-- Update the confirmation flag
  READ TABLE lwa_fpdoc_infocus-x-fpbch ASSIGNING FIELD-SYMBOL(<fs_fpbch>)
        WITH KEY charg = gs_variables-batch_infocus.
  IF sy-subrc EQ 0.
    <fs_fpbch>-zzconf = abap_true.
  ENDIF.

  READ TABLE lt_itemsgm TRANSPORTING NO FIELDS
        WITH KEY material = gs_fpdoc_infocus-x-fphdr-matnr.
  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  CALL METHOD ref_grid_batches->get_selected_rows
    IMPORTING
      et_row_no = DATA(lt_rows).

  DATA(ls_row) = lt_rows[ 1 ].
  DATA(ls_fpbch) = gs_fpdoc_infocus-x-fpbch[ ls_row-row_id ].

  IF ls_fpbch IS NOT INITIAL.
    SELECT *
      FROM zabst_btchchr
      INTO TABLE @DATA(lt_btchchr)
      WHERE aufnr = @ls_fpbch-aufnr
      AND   contr = @ls_fpbch-contr
      AND   batch = @ls_fpbch-charg.
    IF sy-subrc = 0.
      SORT lt_btchchr BY atinn.
    ENDIF.
  ENDIF.

*--Calling FM to get Characteristics
  CALL FUNCTION 'VBWS_MARA_CLASSIFICATION_GET'
    EXPORTING
      i_matnr              = gs_fpdoc_infocus-x-fphdr-matnr
    IMPORTING
      e_class              = lv_classnum
    TABLES
      e_cl_char            = lt_char
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

  IF lt_char IS NOT INITIAL.
    SELECT atnam,
           atfor
      FROM cabn
      INTO TABLE @DATA(lt_cabn)
      FOR ALL ENTRIES IN @lt_char
      WHERE atinn = @lt_char-atinn.
    IF sy-subrc = 0.
      SORT lt_cabn BY atnam.
    ENDIF.
  ENDIF.


  CONCATENATE gs_fpdoc_infocus-x-fphdr-matnr gs_variables-batch_infocus
  INTO lv_objectkey RESPECTING BLANKS.

*--Filling Characteristic values based on data type
  LOOP AT lt_char INTO DATA(ls_char).

    READ TABLE lt_btchchr INTO DATA(ls_btchchr)
      WITH KEY atinn = ls_char-atinn
      BINARY SEARCH.
    IF sy-subrc NE 0.
      IF ls_char-atnam = zcl_abs_abap_maintain=>c_charact_psdat.
        CLEAR: ls_btchchr, ls_fmfpcnf.
        READ TABLE lt_fmfpcnf INTO ls_fmfpcnf
          WITH KEY grcre = abap_true.
        IF sy-subrc EQ 0 AND ls_fmfpcnf-budat IS NOT INITIAL.
          ls_btchchr-atnam = zcl_abs_abap_maintain=>c_charact_psdat.
          lv_atwrt = ls_fmfpcnf-budat.
        ENDIF.
      ELSE.
        CONTINUE.
      ENDIF.
    ENDIF.

    READ TABLE lt_cabn INTO DATA(ls_cabn)
      WITH KEY atnam = ls_btchchr-atnam
      BINARY SEARCH.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    CASE ls_cabn-atfor.
      WHEN zcl_abs_abap_maintain=>c_char_datatyp_char. "'CHAR'
        CLEAR ls_allocvalueschar.
        ls_allocvalueschar-charact    = ls_btchchr-atnam.
        ls_allocvalueschar-value_char = ls_btchchr-value.
        APPEND ls_allocvalueschar TO lt_allocvalueschar.
      WHEN zcl_abs_abap_maintain=>c_char_datatyp_curr. "'CURR'
        CLEAR ls_allocvaluescurr.
        ls_allocvaluescurr-charact    = ls_btchchr-atnam.
        ls_allocvaluescurr-value_from = ls_btchchr-value.
        APPEND ls_allocvaluescurr TO lt_allocvaluescurr.
      WHEN zcl_abs_abap_maintain=>c_char_datatyp_date. "'DATE'
*--Calling FM to convert date to float format
        CLEAR ls_allocvaluesnum.
        IF lv_atwrt IS INITIAL.
          CONCATENATE ls_btchchr-value+6(4)
                      ls_btchchr-value+3(2)
                      ls_btchchr-value(2)
                      INTO lv_atwrt.
        ENDIF.
        CALL FUNCTION 'CTCV_CONVERT_DATE_TO_FLOAT'
          EXPORTING
            date  = lv_atwrt
          IMPORTING
            float = lv_float.
        ls_allocvaluesnum-value_from = lv_float.
        ls_allocvaluesnum-charact    = ls_btchchr-atnam.
        APPEND ls_allocvaluesnum TO lt_allocvaluesnum.
        CLEAR lv_atwrt.
      WHEN OTHERS.
        CLEAR ls_allocvaluesnum.
        ls_allocvaluesnum-charact    = ls_btchchr-atnam.
        ls_allocvaluesnum-value_from = ls_btchchr-value.
        APPEND ls_allocvaluesnum TO lt_allocvaluesnum.
    ENDCASE.

  ENDLOOP.

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
      return             = lt_return_x.

  LOOP AT lt_return_x INTO DATA(lwa_return_x)
                   WHERE type EQ c_msg_type-error.
    MESSAGE ID lwa_return_x-id
       TYPE lwa_return_x-type
     NUMBER lwa_return_x-number
       WITH lwa_return_x-message_v1 lwa_return_x-message_v2
            lwa_return_x-message_v3 lwa_return_x-message_v4
       INTO sy-msgli.
    message_simple space.
  ENDLOOP.
