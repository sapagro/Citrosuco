FUNCTION zabs_fm_change_format.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_ATFLV) TYPE  ATFLV
*"  EXPORTING
*"     REFERENCE(EV_FORMATTED) TYPE  CHAR10
*"----------------------------------------------------------------------
  DATA: lv_old_char10 TYPE char10,
        lv_new_char10 TYPE char10,
        lv_nodec(20)  TYPE n,
        lv_menge      TYPE menge_d.

  lv_menge = iv_atflv.
  WRITE lv_menge TO lv_nodec DECIMALS 0.
  SHIFT lv_nodec LEFT DELETING LEADING '0'.
  CONDENSE lv_nodec.
  IF strlen( lv_nodec ) LE 10.
    lv_new_char10 = lv_nodec.
    ev_formatted = lv_new_char10.
  ENDIF.

*  DATA: lv_old_char10 TYPE char10,
*        lv_new_char10 TYPE char10,
*        lv_i          TYPE i,
*        lv_offset     TYPE i.
*
*  IF iv_atflv IS NOT INITIAL.
*    lv_i = iv_atflv.
*    lv_old_char10 = lv_i.
*
*    CALL FUNCTION 'FKK_AMOUNT_CHECK_AND_CONVERT'
*      EXPORTING
*        i_amount             = lv_old_char10
*        i_waers              = 'BRL'
*      IMPORTING
*        e_amount_c           = lv_old_char10
*      EXCEPTIONS
*        invalid_input_format = 1
*        OTHERS               = 2.
*
*    IF sy-subrc EQ 0.
*      CONDENSE lv_old_char10.
*      IF lv_old_char10 CA ','.
*        DATA(lv_len) = strlen( lv_old_char10 ).
*        DO lv_len TIMES.
*          DATA(lv_index) = sy-index - 1.
*          IF lv_old_char10+lv_index(1) CO '1234567890.'.
*            CONCATENATE lv_new_char10 lv_old_char10+lv_index(1) INTO lv_new_char10.
*          ELSEIF lv_old_char10+lv_index(1) EQ ','.
*            EXIT.
*          ENDIF.
*        ENDDO.
*      ELSEIF lv_old_char10 CO '1234567890.'.
*        lv_new_char10 = lv_old_char10.
*      ENDIF.
*
*      ev_formatted = lv_new_char10.
*    ENDIF.
*  ENDIF.

ENDFUNCTION.
