*----------------------------------------------------------------------*
***INCLUDE /AGRI/LGLFLSF0E.
*----------------------------------------------------------------------*
*{   INSERT         &$&$&$&$                                          1
*&---------------------------------------------------------------------*
*&      Form  ENVIRONMENT_INITIALIZE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM environment_initialize .

**  REFRESH: gt_srch_sel[], gt_search_hdr[].
**
**  CLEAR:  gt_srch_sel, gt_search_hdr.
**
**  CLEAR: so_strno, so_pltxt, so_fltyp, so_tplvl, so_pspnr,
**         so_tplkz, so_tplma, so_owrol, so_owner, so_ownsh,
**         so_garea, so_eqfnr, so_grup1, so_grup2, so_grup3,
**         so_stort, so_swerk, so_bukrs, so_iwerk, so_ernam,
**         so_erdat, so_aenam, so_aedat, p_mxhit.
**
**  REFRESH: so_strno[], so_pltxt[], so_fltyp[], so_tplvl[], so_pspnr[],
**           so_tplkz[], so_tplma[], so_owrol[], so_owner[], so_ownsh[],
**           so_garea[], so_eqfnr[], so_grup1[], so_grup2[], so_grup3[],
**           so_stort[], so_swerk[], so_bukrs[], so_iwerk[], so_ernam[],
**           so_erdat[], so_aenam[], so_aedat[].

ENDFORM.
*}   INSERT
