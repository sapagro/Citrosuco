FUNCTION-POOL zgf_vx_glcm_interface.        "MESSAGE-ID ..

* INCLUDE LZGF_VX_GLCM_INTERFACED...         " Local class definition

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
        END OF gty_glcmhdr.

*-- Constants
CONSTANTS: BEGIN OF gc_constants,
             msgid          TYPE msgid           VALUE 'ZVX_MSG_CLS',
             subobj         TYPE balsubobj       VALUE 'ZVX_CRP_CRT',
             aplobj         TYPE balobj_d        VALUE 'ZVX_SLC',
             valid_to       TYPE sy-datum        VALUE '99991231',
             version        TYPE bosdvers        VALUE '001',
             aufart         TYPE co_aufart_pp    VALUE 'ZY00',
             saf(3)         TYPE c               VALUE 'SAF',
             uom_har        TYPE /agri/glmsehi   VALUE 'HAR',
             routclsd       TYPE seocmpname      VALUE '001',
             routclsd_r     TYPE seocmpname      VALUE '005',
             autyp          TYPE /agri/gl_autyp  VALUE 'AO',
             uom_kg(2)      TYPE c               VALUE 'KG',
             no_descr_na(2) TYPE c               VALUE 'NA',
             msgty_error    TYPE msgty           VALUE 'E',
             msgty_success  TYPE msgty           VALUE 'S',
             msgty_warn     TYPE msgty           VALUE 'W',
             insert         TYPE updkz           VALUE 'I',
             delete         TYPE updkz           VALUE 'D',
             update         TYPE updkz           VALUE 'U',
             flag(1)        TYPE c               VALUE 'X',
             astat_a        TYPE /agri/glastat   VALUE 'A', "active
             astat_i        TYPE /agri/glastat   VALUE 'I', "inactive
             astat_closed   TYPE /agri/glastat   VALUE 'C',
             slash(1)       TYPE c               VALUE '/',
             iphen(1)       TYPE c               VALUE '-',
             ab_terr_area_1 TYPE /agri/glaarea   VALUE '1',
             work_center    TYPE cr_objty        VALUE 'A',
           END OF gc_constants.

*-- Global data
##NEEDED
DATA: gv_tplnr   TYPE /agri/gltplnr_fl,
      gv_iwerk   TYPE iwerk,
      gv_bwtar   TYPE bwtar_d,
      gv_fm_call TYPE char20 VALUE 'Y_OFARM_LAV_LOGICA',
      gv_datab   TYPE /agri/gldatab.

*-- Global internal tables
##NEEDED
DATA: gt_cskey     TYPE TABLE OF gty_glflcma,
      gt_cskey_upd TYPE TABLE OF gty_glflcma.

*-- Global workareas
##NEEDED
DATA: gs_glcmhdr TYPE gty_glcmhdr.
