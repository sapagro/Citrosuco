FUNCTION zabs_fm_ord_save.
*"----------------------------------------------------------------------
*"*"Update Function Module:
*"
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_ACCOM) TYPE  /AGRI/FMACCOM
*"----------------------------------------------------------------------

*-- Local Declarations
  DATA: ls_acm_ord TYPE zabs_acm_ord,
        lt_acm_ord TYPE STANDARD TABLE OF zabs_acm_ord.

*-- Field Symbols
  FIELD-SYMBOLS: <fs_t_details> TYPE STANDARD TABLE,
                 <fs_s_details> TYPE any.

  ASSIGN ('(SAPLZFG_ACM)GT_DETAILS') TO <fs_t_details>.
  IF sy-subrc EQ 0 AND <fs_t_details> IS ASSIGNED.
    LOOP AT <fs_t_details> ASSIGNING <fs_s_details>.
      CLEAR ls_acm_ord.
      MOVE-CORRESPONDING <fs_s_details> TO ls_acm_ord.
      ls_acm_ord-accom = iv_accom.
      APPEND ls_acm_ord TO lt_acm_ord.
    ENDLOOP.
  ENDIF.

*-- Insert the orders for accomplishment
  IF lt_acm_ord IS NOT INITIAL.
    INSERT zabs_acm_ord FROM TABLE lt_acm_ord.
  ENDIF.

ENDFUNCTION.
