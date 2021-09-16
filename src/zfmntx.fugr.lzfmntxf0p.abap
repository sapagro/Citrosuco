*----------------------------------------------------------------------*
***INCLUDE LZFMNTXF0P.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form PLANT_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> I_WERKS
*&---------------------------------------------------------------------*
FORM plant_check USING    lv_werks TYPE werks_d
                 CHANGING lv_subrc TYPE int4.

  DATA: lv_plant TYPE werks_d.

  SELECT SINGLE bwkey FROM t001k
                      INTO (lv_plant)
                 WHERE bukrs = cgl_const-bukrs
                  AND  bwkey = lv_werks.
  MOVE sy-subrc TO lv_subrc.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form PRNUM_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> I_PRNUM
*&      <-- LV_SUBRC
*&---------------------------------------------------------------------*
FORM prnum_check  USING    lv_prnum TYPE /agri/fmprnum
                  CHANGING lv_subrc TYPE int4.
  DATA: lv_prnum_local TYPE /agri/fmprnum.

  SELECT SINGLE prnum FROM /agri/fmprhdr
                      INTO (lv_prnum_local)
                WHERE prnum = lv_prnum
                   AND gjahr = sy-datum+0(4).
           MOVE sy-subrc TO lv_subrc.

ENDFORM.
