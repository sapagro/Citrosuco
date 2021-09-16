FUNCTION-POOL zabs_fg_attr_group_fps.       "MESSAGE-ID ..

* INCLUDE LZABS_FG_ATTR_GROUP_FPSD...        " Local class definition

*&---------------------------------------------------------------------*
*&    TYPES
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_fmfphdr,
         aufnr    TYPE /agri/fmfpnum,
         auart    TYPE aufart,
         autyp    TYPE /agri/gl_autyp,
         tplnr_fl TYPE /agri/gltplnr_fl,
         contr    TYPE /agri/gcontr,
         tplma    TYPE /agri/gltplma,
         cmnum    TYPE /agri/glcmnum,
         varia    TYPE /agri/glvaria,
         cpros    TYPE /agri/glcpros,
         iwerk    TYPE iwerk,
         tecom    TYPE /agri/fmtecom,
       END OF ty_fmfphdr,

       ty_fmfphdr_tab TYPE TABLE OF ty_fmfphdr,

       BEGIN OF ty_cpros,
         cmnum TYPE /agri/glcmnum,
         varia TYPE /agri/glvaria,
         cpros TYPE /agri/glcpros,
         matnr TYPE matnr,
         midur TYPE /agri/glmidur,
         miunt TYPE /agri/gltunit,
         descr TYPE /agri/gdescr_40,
       END OF ty_cpros,

       ty_cpros_tab TYPE TABLE OF ty_cpros.

*&---------------------------------------------------------------------*
*&    CONSTANTS
*&---------------------------------------------------------------------*
****Ownership
CONSTANTS : BEGIN OF c_ownership,
              own         TYPE /agri/glownshp VALUE 'OW',
              third_party TYPE /agri/glownshp VALUE 'TP',
            END OF c_ownership,

            BEGIN OF c_crop_season,
              formacao   TYPE /agri/glvaria VALUE 'FORMAÇÃO',
              manutencao TYPE /agri/glvaria VALUE 'MANUT&COLHEITA',
            END OF c_crop_season,

            BEGIN OF c_crop_process,
              formacao    TYPE /agri/glcpros VALUE 'FORMAÇÃO',
              manutencao  TYPE /agri/glcpros VALUE 'MANUTEÇÃO',
              colheita    TYPE /agri/glcpros VALUE 'COLHEITA',
              implantacao TYPE /agri/glcpros VALUE 'IMPLANT',
              close_all   TYPE /agri/glcpros VALUE abap_true,
            END OF c_crop_process.

****Update Indicators
CONSTANTS: c_updkz_new(1)     TYPE c VALUE 'I',
           c_updkz_update(1)  TYPE c VALUE 'U',
           c_updkz_delete(1)  TYPE c VALUE 'D',
           c_updkz_old(1)     TYPE c VALUE ' ',
****Rel 60E SP6
           c_updkz_newrow     TYPE c VALUE 'N',
****
           c_updkz_propose(1) TYPE c VALUE 'P'. "ESP5 11129 - Generic Fiori app changes,
