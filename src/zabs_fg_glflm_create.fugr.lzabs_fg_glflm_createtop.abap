FUNCTION-POOL zabs_fg_glflm_create.         "MESSAGE-ID ..

*INCLUDE /agri/lglflmtop.                   " Global Data
INCLUDE zabs_inc_lglflmtop.
INCLUDE /agri/lglflmcls.                   " Local class def & imp
INCLUDE /agri/lglflmuxx.                   " Function Modules

*******************************************************************
*   User-defined Include-files (if necessary).                    *
*******************************************************************
* INCLUDE /AGRI/LGLFLMF...                   " Subroutines
* INCLUDE /AGRI/LGLFLMO...                   " PBO-Modules
* INCLUDE /AGRI/LGLFLMI...                   " PAI-Modules
* INCLUDE /AGRI/LGLFLME...                   " Events
* INCLUDE /AGRI/LGLFLMP...                   " Local class implement.

INCLUDE /agri/lglflmevt.

INCLUDE /agri/lglflmo01.

INCLUDE /agri/lglflmf0t.

INCLUDE /agri/lglflmf0s.

INCLUDE /agri/lglflmf0c.

INCLUDE /agri/lglflmi01.

INCLUDE /agri/lglflmf0e.

INCLUDE /agri/lglflmf0f.

INCLUDE /agri/lglflmf0w.

INCLUDE /agri/lglflmf0d.

INCLUDE /agri/lglflmf0i.

INCLUDE /agri/lglflmf0a.

INCLUDE /agri/lglflmf0u.

INCLUDE /agri/lglflmf0h.

INCLUDE /agri/lglflmf0g.

INCLUDE /agri/lglflmf0o.

INCLUDE /agri/lglflmf0m.

INCLUDE /agri/lglflmf0n.

INCLUDE /agri/lglflmf0p.

INCLUDE /agri/lglflmf0r.

INCLUDE /agri/lglflmf0b.

INCLUDE /agri/lglflmf0l.

DATA: rb_std TYPE abap_bool,
      rb_cst TYPE abap_bool,
      rb_faz TYPE abap_bool VALUE abap_true,
      rb_tal TYPE abap_bool.
