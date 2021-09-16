*&---------------------------------------------------------------------*
*&  Include           /AGRI/LGLFLSSEL
*&---------------------------------------------------------------------*


**INCLUDE /agri/gstatus_selections.
**
**SELECTION-SCREEN BEGIN OF SCREEN 0010.
**
**SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-001.
**SELECT-OPTIONS: so_strno   FOR /agri/glflot-strno,
**                so_pltxt   FOR /agri/glflot-pltxt,
**                so_fltyp   FOR /agri/glflot-fltyp,
**                so_tplvl   FOR /agri/glflot-tplvl,
**                so_pspnr   FOR /agri/glflot-pspnr,
**                so_tplkz   FOR /agri/glflot-tplkz,
**                so_tplma   FOR /agri/glflot-tplma,
**                so_owrol   FOR /agri/glflot-owrol,
**                so_owner   FOR /agri/glflot-owner, "VALUE-REQUEST,
**                so_ownsh   FOR /agri/glflot-ownshp,
**                so_garea   FOR /agri/glflot-garea,
**                so_eqfnr   FOR /agri/glflot-eqfnr,
**                so_grup1   FOR /agri/glflot-grup1,
**                so_grup2   FOR /agri/glflot-grup2,
**                so_grup3   FOR /agri/glflot-grup3,
**                so_stort   FOR /agri/glflot-stort,
**                so_swerk   FOR iloa-swerk,
**                so_bukrs   FOR /agri/glflot-bukrs,
**                so_iwerk   FOR /agri/glflot-iwerk.
**SELECTION-SCREEN END OF BLOCK b0.
**
**SELECTION-SCREEN BEGIN OF  BLOCK  b1  WITH FRAME TITLE TEXT-002.
**SELECT-OPTIONS: so_ernam   FOR /agri/glflot-ernam,
**                so_erdat   FOR /agri/glflot-erdat,
**                so_aenam   FOR /agri/glflot-aenam,
**                so_aedat   FOR /agri/glflot-aedat.
**SELECTION-SCREEN END OF BLOCK b1.
**
****** Max Hits in Selection
**PARAMETERS: p_mxhit TYPE /agri/gmxhit MEMORY ID /agri/gmxhit.
**
**SELECTION-SCREEN END OF SCREEN 0010.
