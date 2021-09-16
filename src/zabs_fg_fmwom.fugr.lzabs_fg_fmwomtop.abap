FUNCTION-POOL zabs_fg_fmwom.                "MESSAGE-ID ..

* INCLUDE LZABS_FG_FMWOMD...                 " Local class definition

*--Constants declaration
CONSTANTS: BEGIN OF c_tablename,
             c_ordhdr TYPE dd02d-tabname VALUE 'ZABST_ORDHDR',
             c_orditm TYPE dd02d-tabname VALUE 'ZABST_ORDITM',
             c_ordcnf TYPE dd02d-tabname VALUE 'ZABST_ORDCNF',
             c_ordcon TYPE dd02d-tabname VALUE 'ZABST_ORDCON',
             c_ordgrp TYPE dd02d-tabname VALUE 'ZABST_ORDGRP',
           END OF c_tablename.

*--Types declaration
TYPES : BEGIN OF ty_var,
          next_no TYPE /agri/fmfpnum,
          contr   TYPE nrnr,
*         ctext   TYPE  zabs_del_cnval,
        END OF ty_var.

TYPES: BEGIN OF ty_vornr,
         aufnr TYPE aufnr,
         vornr TYPE vornr,
         posnr TYPE co_posnr,
       END OF ty_vornr.

DATA: ltr_aufnr  TYPE RANGE OF aufnr,
      ltr_pmatnr TYPE RANGE OF matnr,
      ltr_tmatnr TYPE RANGE OF matnr,
      lr_aufnr   LIKE LINE  OF ltr_aufnr,
      lr_pmatnr  LIKE LINE  OF ltr_pmatnr,
      lr_tmatnr  LIKE LINE  OF ltr_tmatnr.

*TYPES : tty_aufnr  LIKE TABLE OF ltr_aufnr,
*        tty_pmatnr LIKE TABLE OF ltr_pmatnr,
*        tty_tmatnr LIKE TABLE OF ltr_tmatnr.


INCLUDE /agri/gprolog_macros.
INCLUDE /agri/glprolog_macros.

*--Types declaration
TYPES : BEGIN OF ty_shift,
          werks TYPE werks_d,
          equnr TYPE /agri/glequnr,
          odate TYPE datum,
          vornr TYPE vornr,
          ltxa1 TYPE ltxa1,
        END OF ty_shift,
        tty_shift TYPE TABLE OF ty_shift,

        BEGIN OF ty_shift_f4,
          vornr TYPE vornr,
          ltxa1 TYPE ltxa1,
        END OF ty_shift_f4,
        tty_shift_f4 TYPE TABLE OF ty_shift_f4,

        BEGIN OF ty_opr,
          wonum TYPE /agri/fmwonum,
          vornr TYPE vornr,
          ltxa1 TYPE ltxa1,
        END OF ty_opr,
        tty_opr TYPE TABLE OF ty_opr,

        BEGIN OF ty_ordhdr,
          ordno TYPE /agri/fmfpnum,
          werks TYPE werks_d,
          equnr TYPE /agri/glequnr,
          odate TYPE datum,
          wonum TYPE /agri/fmwonum,
          erzet TYPE erzet,
        END OF ty_ordhdr,
        tty_ordhdr TYPE TABLE OF ty_ordhdr,

        BEGIN OF ty_woitm,
          wonum    TYPE	/agri/fmwonum,
          aufnr	   TYPE /agri/fmaufnr_to,
          tplnr_fl TYPE /agri/gltplnr_fl,
        END OF ty_woitm,
        tty_woitm TYPE TABLE OF ty_woitm,

        BEGIN OF ty_fmfphdr,
          aufnr	   TYPE /agri/fmfpnum,
          tplnr_fl TYPE	/agri/gltplnr_fl,
          gamng	   TYPE gamng,
          gmein    TYPE	co_gmein,
          tecom    TYPE /agri/fmtecom,
        END OF ty_fmfphdr,
        tty_fmfphdr TYPE TABLE OF ty_fmfphdr,
        tty_fmirflo TYPE TABLE OF /agri/fmirflo,

        BEGIN OF ty_post,
          wonum   TYPE /agri/fmwonum,
          fmfpcnf TYPE /agri/t_fmfp_cnf,
          fmfpcom TYPE /agri/t_fmfpcom,
        END OF ty_post,
        tty_post TYPE TABLE OF ty_post,

        BEGIN OF ty_comsplit,
          wonum TYPE /agri/fmwonum,
          gamng	TYPE gamng,
          lmnga TYPE /agri/fmlmnga,
        END OF ty_comsplit,
        tty_comsplit TYPE TABLE OF ty_comsplit,

        BEGIN OF ty_task,
          wonum TYPE /agri/fmwonum,
          aufnr	TYPE /agri/fmfpnum,
          gamng	TYPE gamng,
          lmnga TYPE /agri/fmlmnga,
          shift TYPE /agri/fmprcov,
        END OF ty_task,
        tty_task TYPE TABLE OF ty_task,

        BEGIN OF ty_mod,
          row TYPE i,
        END OF ty_mod,

        BEGIN OF ty_update,
          ordcnf TYPE zabs_tty_ordcnf,
          ordcon TYPE zabs_tty_ordcon,
          ordgrp TYPE zabs_tty_ordgrp,
        END OF ty_update,

        BEGIN OF ty_order,
          ordhdr TYPE zabs_tty_ordhdr,
          orditm TYPE zabs_tty_orditm,
        END OF ty_order,

        BEGIN OF ty_ordcnf,
          ordno     TYPE /agri/fmfpnum,
          contr     TYPE /irm/gcontr,
          fcontr_hm TYPE zabs_del_fcontr,
        END OF ty_ordcnf,
        tty_ordcnf TYPE TABLE OF ty_ordcnf.

*CLASS:lcl_event_handler DEFINITION DEFERRED.

*--Constant declarations
CONSTANTS: c_wo_cnf_0100_cc(30) TYPE c VALUE 'WO_CNF_0100_CC',
           c_wo_con_0100_cc(30) TYPE c VALUE 'WO_CON_0100_CC',
           c_shift1             TYPE zabs_del_shift VALUE '0010',
           c_shift2             TYPE zabs_del_shift VALUE '0020',
           c_shift3             TYPE zabs_del_shift VALUE '0030',
           c_shift4             TYPE zabs_del_shift VALUE '0040',
           c_shift5             TYPE zabs_del_shift VALUE '0050',
           c_shift6             TYPE zabs_del_shift VALUE '0060',
           c_true(1)            TYPE c VALUE 'X'.

*--Table type declarations
TYPES: tty_wo_cnf TYPE TABLE OF zabs_str_ocnf,
       tty_wo_con TYPE TABLE OF zabs_str_ocon.

*--Variables declaration
DATA : BEGIN OF gs_variables,
         mode,
         manual_changes,
         changes,
       END OF gs_variables,
*--Global table declarations
       gt_wo_cnf    TYPE tty_wo_cnf,
       gt_wo_con    TYPE tty_wo_con,
       gt_shift     TYPE tty_shift,
       gt_supply    TYPE tty_wo_con,
       gt_post      TYPE tty_post,
       gt_woitm     TYPE tty_woitm,
       gt_fmfphdr   TYPE tty_fmfphdr,
       gt_fmirflo   TYPE tty_fmirflo,
       gt_mod       TYPE lvc_t_modi,
       gt_comsplit  TYPE tty_comsplit,
       gt_task      TYPE tty_task,
       gt_ordgrp    TYPE zabs_tty_ordgrp,
       gt_ordcnf    TYPE tty_ordcnf,
       gt_wo_dbcnf  TYPE TABLE OF zabst_ordcnf,
       gt_messages  TYPE  /agri/t_gprolog,
       gt_wocon_imp TYPE zabs_tty_wo_con,
       gwa_wo_cnf   TYPE zabs_str_ocnf,
       gwa_order    TYPE ty_order.
