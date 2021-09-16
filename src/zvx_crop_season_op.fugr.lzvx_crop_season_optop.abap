FUNCTION-POOL zvx_crop_season_op.           "MESSAGE-ID ..

* INCLUDE LZVX_CROP_SEASON_OPD...            " Local class definition

*-- Types
TYPES : BEGIN OF gty_glflcma,
          tplnr_fl TYPE /agri/gltplnr_fl,
          contr    TYPE /agri/gcontr,
          season   TYPE /agri/gl_season,
          astat    TYPE /agri/glastat,
          yaufnr   TYPE /agri/glyaufnr,
        END OF gty_glflcma,
        tt_glflcma TYPE STANDARD TABLE OF gty_glflcma,

        BEGIN OF gty_glcmhdr,
          cmnum  TYPE /agri/glcmnum,
          exyld  TYPE /agri/glexyld,
          ymatnr TYPE /agri/glymatnr,
        END OF gty_glcmhdr,

        BEGIN OF gty_cpros,
          cmnum TYPE /agri/glcmnum,
          varia	TYPE /agri/glvaria,
          cpros	TYPE /agri/glcpros,
        END OF gty_cpros,

        BEGIN OF gty_fmfphdr,
          aufnr	TYPE /agri/fmfpnum,
          cmnum	TYPE /agri/glcmnum,
          varia	TYPE /agri/glvaria,
          cpros	TYPE /agri/glcpros,
        END OF gty_fmfphdr.

*-- Constants
CONSTANTS: BEGIN OF gc_constants,
             msgid         TYPE msgid           VALUE 'ZVX_MSG_CLS',
             subobj        TYPE balsubobj       VALUE 'ZVX_CS_PO',
             aplobj        TYPE balobj_d        VALUE 'ZVX_SLC',
             valid_to      TYPE sy-datum        VALUE '99991231',
             version       TYPE bosdvers        VALUE '001',
             aufart        TYPE co_aufart_pp    VALUE 'ZY00',
             saf(3)        TYPE c               VALUE 'SAF',
             autyp         TYPE /agri/gl_autyp  VALUE 'AO',
             uom_kg(2)     TYPE c               VALUE 'KG',
             msgty_error   TYPE msgty           VALUE 'E',
             msgty_success TYPE msgty           VALUE 'S',
             msgty_warn    TYPE msgty           VALUE 'W',
             insert        TYPE updkz           VALUE 'I',
             delete        TYPE updkz           VALUE 'D',
             update        TYPE updkz           VALUE 'U',
             flag(1)       TYPE c               VALUE 'X',
             astat_a       TYPE /agri/glastat   VALUE 'A', "active
             astat_i       TYPE /agri/glastat   VALUE 'I', "inactive
             astat_closed  TYPE /agri/glastat   VALUE 'C',
             slash(1)      TYPE c               VALUE '/',
             iphen(1)      TYPE c               VALUE '-',
             teco_call     TYPE char20          VALUE 'Z_TECO_CALL',
           END OF gc_constants.

*-- Global variables
##NEEDED
DATA: gv_tplnr        TYPE /agri/gltplnr_fl,
      gv_iwerk        TYPE iwerk,
      gv_bwtar        TYPE bwtar_d,
      gv_datab        TYPE /agri/gldatab,
      gv_fm_call      TYPE char25 VALUE 'Y_OFARM_CROP_SEASON_OP',
      gv_teco_call    TYPE char20,
      gv_po_error     TYPE char1,
      gv_poflag       TYPE xfeld.

*-- Global internal tables
##NEEDED
DATA: gt_cskey     TYPE TABLE OF gty_glflcma,
      gt_cskey_upd TYPE TABLE OF gty_glflcma,
      gt_cpros     TYPE TABLE OF gty_cpros,
      gt_fmfphdr   TYPE TABLE OF gty_fmfphdr,
      gt_cs_key    TYPE TABLE of gty_glflcma.

*-- Global workareas
##NEEDED
DATA: gs_glcmhdr TYPE gty_glcmhdr.
