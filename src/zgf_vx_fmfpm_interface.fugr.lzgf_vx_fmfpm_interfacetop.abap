FUNCTION-POOL zgf_vx_fmfpm_interface.         "MESSAGE-ID ..

* INCLUDE LZGF_VX_PROCESS_ORDERD...          " Local class definition

*-- Types :
TYPES : BEGIN OF gty_cpros,
          cmnum TYPE /agri/glcmnum,
          varia	TYPE /agri/glvaria,
          cpros	TYPE /agri/glcpros,
        END OF gty_cpros,

        BEGIN OF gty_fmfphdr,
          aufnr	TYPE /agri/fmfpnum,
          cmnum	TYPE /agri/glcmnum,
          varia	TYPE /agri/glvaria,
          cpros	TYPE /agri/glcpros,
        END OF gty_fmfphdr,

        BEGIN OF gty_marc,
          matnr TYPE matnr,
          werks TYPE werks_d,
          lgpro TYPE lgpro,
        END OF gty_marc,

        BEGIN OF gty_fmfphdr_task,
          aufnr TYPE /agri/fmfpnum,
          tecom TYPE /agri/fmtecom,
        END OF gty_fmfphdr_task,

        BEGIN OF gty_fpitm,
          cstat TYPE /agri/fmcstat,
          gamng TYPE gamng,
        END OF gty_fpitm,

        BEGIN OF gty_batch_tot,
          matnr TYPE matnr,
          erfmg TYPE erfmg,
        END OF gty_batch_tot.

*-- Constants
CONSTANTS: BEGIN OF gc_constants,
             msgty_error       TYPE msgty          VALUE 'E',
             msgty_success     TYPE msgty          VALUE 'S',
             msgty_warn        TYPE msgty          VALUE 'W',
             msgid             TYPE msgid          VALUE 'ZVX_MSG_CLS',
             aplobj            TYPE balobj_d       VALUE 'ZVX_SLC',
             subobj            TYPE balsubobj      VALUE 'ZVX_PRC_ORDER',
             insert            TYPE updkz          VALUE 'I',
             delete            TYPE updkz          VALUE 'D',
             update            TYPE updkz          VALUE 'U',
             action_add        TYPE /agri/gaction  VALUE 'A',
             autyp             TYPE /agri/gl_autyp VALUE 'AO',
             autyp_task_ord    TYPE /agri/gl_autyp VALUE 'TO',
             operation_0010    TYPE vornr          VALUE '0010',
             version_0001      TYPE verid          VALUE '0001',
             flag              TYPE c              VALUE 'X',
             positionno        TYPE positionno     VALUE '0010',
             mat_type_dien     TYPE mtart          VALUE 'DIEN',
             item_cat_l        TYPE postp          VALUE 'L',
             item_cat_n        TYPE postp          VALUE 'N',
             meinh             TYPE vorme          VALUE 'HAR',
             status_cnf        TYPE /agri/fmcstat  VALUE 'CNF',
             status_pcnf       TYPE /agri/fmcstat  VALUE 'PCNF',
             item_no_0001      TYPE rspos          VALUE '0001',
             text_t            TYPE postp          VALUE 'T',
             comma             TYPE char1          VALUE ',',
             dbt_cr_ind_h      TYPE shkzg          VALUE 'H',
             inv_mgmt_me       TYPE kappl          VALUE 'ME',
             schprc_bch_me0001 TYPE kalsma_ch      VALUE 'ME0001',
           END OF gc_constants.

*-- Global variables
##NEEDED
DATA : gv_tplnr   TYPE /agri/gltplnr_fl,
       gv_fm_call TYPE char20 VALUE 'Y_OFARM_TASK_ORDER',
       gv_yaufnr  TYPE /agri/glyaufnr.

*-- Global Workareas
##NEEDED
DATA : gs_fmfphdr_task TYPE gty_fmfphdr_task,
       gs_fpitm        TYPE gty_fpitm.

*-- Global Internal Table
##NEEDED
DATA : gt_cskey        TYPE /agri/t_glcs_key,
       gt_cpros        TYPE TABLE OF gty_cpros,
       gt_fmfphdr      TYPE TABLE OF gty_fmfphdr,
       gt_marc         TYPE TABLE OF gty_marc,
       gt_bdbatch      TYPE TABLE OF bdbatch,
       gt_batch_tot    TYPE TABLE OF gty_batch_tot,
       gt_fpcomponents TYPE /agri/t_fmfpcom.
