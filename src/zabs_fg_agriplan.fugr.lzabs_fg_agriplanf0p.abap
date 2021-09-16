*&---------------------------------------------------------------------*
*& Form UPDATE_POSITION_ITMS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM update_position_itms .

  FIELD-SYMBOLS: <lwa_acitm> TYPE zsc_fmacitm.

  LOOP AT gs_acdoc_infocus-x-acitm ASSIGNING <lwa_acitm>.
    CLEAR : <lwa_acitm>-mandt.
    <lwa_acitm>-acnum = gs_acdoc_infocus-x-achdr-acnum.
    <lwa_acitm>-updkz = c_updkz_new.
    <lwa_acitm>-acpos = ( sy-tabix * 10 ).
  ENDLOOP.

ENDFORM.
