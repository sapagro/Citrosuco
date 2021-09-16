FUNCTION zfmpl_production_order_create.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IT_PLHDR) TYPE  ZT_FMPLHDR
*"     REFERENCE(IT_PLITM) TYPE  ZT_FMPLITM
*"  EXPORTING
*"     REFERENCE(ET_FMFPITM) TYPE  /AGRI/T_FMFPITM
*"----------------------------------------------------------------------
  PERFORM production_orders_create USING it_plhdr
                                         it_plitm
                                   CHANGING et_fmfpitm.
ENDFUNCTION.
