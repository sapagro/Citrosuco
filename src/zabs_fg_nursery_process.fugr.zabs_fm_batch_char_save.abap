FUNCTION zabs_fm_batch_char_save.
*"----------------------------------------------------------------------
*"*"Interface local:
*"----------------------------------------------------------------------
************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Enhancement Name  :  ZABS_IENH_NURSERY_PROCESS                       *
* FM Name           :  zabs_fm_batch_char_save                         *
* Created By        :  Jetendra Mantena                                *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  06.18.2019                                      *
* TR                :  C4DK901784                                      *
* Version           :  001                                             *
* Description       :  Batch Characteristics Update and Insert.        *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*--Local data declaration
  DATA: lt_batch_char_update TYPE tty_batch_char,
        lt_batch_char_insert TYPE tty_batch_char,
        lt_allocvaluescurr   TYPE tt_bapi1003_alloc_values_curr,
        lt_allocvaluesnum    TYPE tt_bapi1003_alloc_values_num,
        lt_allocvalueschar   TYPE tt_bapi1003_alloc_values_char,
        ls_allocvaluesnum    TYPE bapi1003_alloc_values_num,
        ls_allocvalueschar   TYPE bapi1003_alloc_values_char,
        ls_allocvaluescurr   TYPE bapi1003_alloc_values_curr,
        lt_return_x          TYPE bapiret2_tab.

*--Local Variables
  DATA : lv_objectkey TYPE bapi1003_key-object_long,
         lv_classnum  TYPE bapi1003_key-classnum,
         lv_float     TYPE cawn-atflv,
         lv_atwrt     TYPE cawn-atwrt,
         lv_matnr     TYPE matnr.

  FIELD-SYMBOLS: <lv_matnr> TYPE matnr.

****Message Types
  CONSTANTS : BEGIN OF c_msg_type,
                info    LIKE sy-msgty VALUE 'I',
                warning LIKE sy-msgty VALUE 'W',
                error   LIKE sy-msgty VALUE 'E',
                abend   LIKE sy-msgty VALUE 'A',
                success LIKE sy-msgty VALUE 'S',
                x       LIKE sy-msgty VALUE 'X',
              END   OF c_msg_type.

*--Insert Or Update custom table only if user enter/change
*  values for batch charachteristics
  CHECK gs_variables-data_change IS NOT INITIAL.

*--Get batch characteristics data from custom table
*  to check if data exists
  SELECT * FROM zabst_btchchr
    INTO TABLE @DATA(lt_btchchr)
    FOR ALL ENTRIES IN @gt_batch_char
   WHERE aufnr = @gt_batch_char-aufnr
     AND contr = @gt_batch_char-contr
     AND batch = @gt_batch_char-batch
     AND class = @gt_batch_char-class
     AND atinn = @gt_batch_char-atinn.
  IF sy-subrc = 0.
    SORT lt_btchchr BY aufnr contr batch class atinn.
  ENDIF.

  REFRESH: lt_batch_char_update, lt_batch_char_insert.
*--Processing data to update/insert data in custom database table.
  LOOP AT gt_batch_char ASSIGNING FIELD-SYMBOL(<ls_batch_char>).
    READ TABLE lt_btchchr INTO DATA(ls_btchchr)
    WITH KEY aufnr = <ls_batch_char>-aufnr
             contr = <ls_batch_char>-contr
             batch = <ls_batch_char>-batch
             class = <ls_batch_char>-class
             atinn = <ls_batch_char>-atinn
             BINARY SEARCH.
    IF sy-subrc = 0.
      IF <ls_batch_char>-value NE ls_btchchr-value.
        <ls_batch_char>-aenam = sy-uname.
        <ls_batch_char>-aedat = sy-datum.
        <ls_batch_char>-aezet = sy-uzeit.
        APPEND <ls_batch_char> TO lt_batch_char_update.
      ENDIF.
    ELSE.
      <ls_batch_char>-ernam = sy-uname.
      <ls_batch_char>-erdat = sy-datum.
      <ls_batch_char>-erzet = sy-uzeit.
      APPEND <ls_batch_char> TO lt_batch_char_insert.
    ENDIF.
  ENDLOOP.

  IF lt_batch_char_update IS NOT INITIAL.
    UPDATE zabst_btchchr FROM TABLE lt_batch_char_update.
  ENDIF.

  IF lt_batch_char_insert IS NOT INITIAL.
    INSERT zabst_btchchr FROM TABLE lt_batch_char_insert.
  ENDIF.

  IF gt_char[] IS NOT INITIAL.
    SELECT atinn, adzhl, atnam,
           atidn, atfor, auswahlmge
      FROM cabn
      INTO TABLE @DATA(lt_cabn)
      FOR ALL ENTRIES IN @gt_char
     WHERE atinn = @gt_char-atinn.

    IF sy-subrc EQ 0.
      SORT lt_cabn BY atnam.
    ENDIF.

    DATA(lt_batch_char) = gt_batch_char.
    SORT lt_batch_char BY atnam.
    LOOP AT gt_char INTO DATA(ls_char).
      READ TABLE lt_cabn INTO DATA(ls_cabn)
        WITH KEY atnam = ls_char-atnam BINARY SEARCH.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      READ TABLE lt_batch_char INTO DATA(ls_batch_char)
        WITH KEY atnam = ls_char-atnam BINARY SEARCH.
      IF sy-subrc EQ 0.
        IF ls_batch_char-value IS INITIAL.
          CONTINUE.
        ENDIF.
      ELSE.
        CONTINUE.
      ENDIF.

      CASE ls_cabn-atfor.
        WHEN zcl_abs_abap_maintain=>c_char_datatyp_char. "'CHAR'
          CLEAR ls_allocvalueschar.
          ls_allocvalueschar-charact    = ls_batch_char-atnam.
          ls_allocvalueschar-value_char = ls_batch_char-value.
          APPEND ls_allocvalueschar TO lt_allocvalueschar.
        WHEN zcl_abs_abap_maintain=>c_char_datatyp_curr. "'CURR'
          CLEAR ls_allocvaluescurr.
          ls_allocvaluescurr-charact    = ls_batch_char-atnam.
          ls_allocvaluescurr-value_from = ls_batch_char-value.
          APPEND ls_allocvaluescurr TO lt_allocvaluescurr.
        WHEN zcl_abs_abap_maintain=>c_char_datatyp_date. "'DATE'
*--Calling FM to convert date to float format
          CLEAR ls_allocvaluesnum.
          IF lv_atwrt IS INITIAL.
            CONCATENATE ls_batch_char-value+6(4)
                        ls_batch_char-value+3(2)
                        ls_batch_char-value(2)
                        INTO lv_atwrt.
          ENDIF.
          CALL FUNCTION 'CTCV_CONVERT_DATE_TO_FLOAT'
            EXPORTING
              date  = lv_atwrt
            IMPORTING
              float = lv_float.
          ls_allocvaluesnum-value_from = lv_float.
          ls_allocvaluesnum-charact    = ls_batch_char-atnam.
          APPEND ls_allocvaluesnum TO lt_allocvaluesnum.
          CLEAR lv_atwrt.
        WHEN OTHERS.
          CLEAR ls_allocvaluesnum.
          ls_allocvaluesnum-charact    = ls_batch_char-atnam.
          ls_allocvaluesnum-value_from = ls_batch_char-value.
          APPEND ls_allocvaluesnum TO lt_allocvaluesnum.
      ENDCASE.
    ENDLOOP.
  ENDIF.

  ASSIGN ('(/AGRI/SAPLFMNSM)GS_FPDOC_INFOCUS-X-FPHDR-MATNR') TO <lv_matnr>.
  IF sy-subrc EQ 0.
    READ TABLE gt_batch_char INTO ls_batch_char INDEX 1.
    IF sy-subrc EQ 0
    AND <lv_matnr> IS NOT INITIAL
    AND ls_batch_char-batch IS NOT INITIAL.
      CONCATENATE <lv_matnr> ls_batch_char-batch INTO lv_objectkey RESPECTING BLANKS.

      CALL FUNCTION 'BAPI_OBJCL_CHANGE'
        EXPORTING
          objecttable        = 'MCH1'
          classnum           = gv_classnum
          classtype          = '023'
          objectkey_long     = lv_objectkey
        TABLES
          allocvaluesnumnew  = lt_allocvaluesnum
          allocvaluescharnew = lt_allocvalueschar
          allocvaluescurrnew = lt_allocvaluescurr
          return             = lt_return_x.

      READ TABLE lt_return_x INTO DATA(lwa_return_x)
        WITH KEY type = c_msg_type-error.
      IF sy-subrc NE 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

        READ TABLE lt_return_x TRANSPORTING NO FIELDS
          WITH KEY type = c_msg_type-success.
        IF sy-subrc EQ 0.
*-- Características do Lote Salvas
          MESSAGE i025(zabs_msgcls).
        ELSE.
*-- Erro ao Salvar as Características do Lote!
          MESSAGE i327(zfmfp).
        ENDIF.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        IF lwa_return_x-id IS NOT INITIAL
        AND lwa_return_x-type IS NOT INITIAL
        AND lwa_return_x-number IS NOT INITIAL.
          MESSAGE ID lwa_return_x-id TYPE lwa_return_x-type NUMBER lwa_return_x-number
             WITH lwa_return_x-message_v1 lwa_return_x-message_v2
                  lwa_return_x-message_v3 lwa_return_x-message_v4.
        ELSE.
*-- Erro ao Salvar as Características do Lote!
          MESSAGE i327(zfmfp).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  CLEAR gs_variables-data_change.

ENDFUNCTION.
