FUNCTION zabs_fm_ord_get.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_ACCOM) TYPE  /AGRI/FMACCOM
*"  TABLES
*"      T_ACM_ORD STRUCTURE  ZABS_ACM_ORD
*"----------------------------------------------------------------------

*-- Fetch the task orders for the accomplishment sheet
  SELECT *
    FROM zabs_acm_ord
    INTO TABLE t_acm_ord
   WHERE accom EQ iv_accom.

ENDFUNCTION.
