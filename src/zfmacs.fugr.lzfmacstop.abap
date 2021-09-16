FUNCTION-POOL ZFMACS.                  "MESSAGE-ID ..

* INCLUDE /AGRI/LGLFLSD...                   " Local class definition

INCLUDE /agri/gstatus_selections.
INCLUDE /agri/global_constants.
INCLUDE /agri/gprolog_macros.

TABLES: zfmachdr,
        zfmaitm ,
        sscrfields.

DATA: gs_subobj   TYPE /agri/tgdspbas,
      gt_srch_sel TYPE TABLE OF /agri/s_gsrchp_sel.

DATA : ls_srchp_sel   TYPE /agri/s_gsrchp_sel.
