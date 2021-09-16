FUNCTION-POOL zabs_fg_attr_group_fp.        "MESSAGE-ID ..

* INCLUDE LZABS_FG_ATTR_GROUP_FPD...         " Local class definition

*&---------------------------------------------------------------------*
*&    TYPES
*&---------------------------------------------------------------------*
TYPES: BEGIN OF gy_fmfphdr,
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
       END OF gy_fmfphdr,

       gy_fmfphdr_tab TYPE STANDARD TABLE OF gy_fmfphdr.

*&---------------------------------------------------------------------*
*&    CONSTANTS
*&---------------------------------------------------------------------*
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
