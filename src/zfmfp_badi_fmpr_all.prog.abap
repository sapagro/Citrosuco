*&---------------------------------------------------------------------*
*& Subroutinenpool ZFMFP_BADI_FMPR_ALL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
PROGRAM zfmfp_badi_fmpr_all.

*&---------------------------------------------------------------------*
*& Form INSERT_ALLOCVALUESNUMNEW
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> WA_CABN_ATFOR
*&      <-- T_ALLOCVALUESNUMNEW
*&      <-- IF
*&      <-- FOUND
*&---------------------------------------------------------------------*
FORM insert_allocvaluesnumnew USING v_atnam TYPE atnam
                                    v_atfor TYPE atfor
  CHANGING t_num  TYPE tt_bapi1003_alloc_values_num
           t_char TYPE tt_bapi1003_alloc_values_char
           t_curr TYPE tt_bapi1003_alloc_values_curr.

  DATA: lwa_num  LIKE LINE OF t_num,
        lwa_char LIKE LINE OF t_char,
        lwa_curr LIKE LINE OF t_curr,
        lv_float TYPE cawn-atflv,
        lv_atwrt TYPE cawn-atwrt.

  CASE v_atfor.
    WHEN 'CHAR'.
      lwa_char-charact = v_atnam.
      lwa_char-value_char = '2'.
      APPEND lwa_char TO t_char.
    WHEN 'CURR'.
      lwa_curr-charact = v_atnam.
      lwa_curr-value_from = 2.
      APPEND lwa_curr TO t_curr.
    WHEN 'DATE'.
*--Calling FM to convert date to float format
      IF lv_atwrt IS INITIAL.
        CONCATENATE sy-datum(4)
                    sy-datum+4(2)
                    sy-datum+6(2)
                    INTO lv_atwrt.
      ENDIF.
      CALL FUNCTION 'CTCV_CONVERT_DATE_TO_FLOAT'
        EXPORTING
          date  = lv_atwrt
        IMPORTING
          float = lv_float.
      lwa_num-value_from = lv_float.
      lwa_num-charact    = v_atnam.
      APPEND lwa_num TO t_num.
    WHEN OTHERS.
      lwa_num-charact = v_atnam.
      lwa_num-value_from = '2'.
      APPEND lwa_num TO t_num.
  ENDCASE.

ENDFORM.
