FUNCTION-POOL zabs_fg_acm.                  "MESSAGE-ID ..

* INCLUDE LZABS_FG_ACMD...                   " Local class definition

TYPES: BEGIN OF ty_tplnr_sh,
         zztplnr TYPE zabs_del_tplnr,
         pltxt   TYPE /agri/glpltxt,
       END OF ty_tplnr_sh.

TYPES: BEGIN OF ty_details.
    INCLUDE STRUCTURE /agri/s_fmoc_details.
TYPES: tplnr_fl TYPE /agri/gltplnr_fl,
       prnum    TYPE /agri/fmprnum,
       END OF ty_details.
