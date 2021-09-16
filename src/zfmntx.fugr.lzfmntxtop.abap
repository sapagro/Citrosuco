FUNCTION-POOL zfmntx.                       "MESSAGE-ID ..

* INCLUDE LZFMNTXD...                        " Local class definition

INCLUDE:
 /agri/abgl_constants,
          /agri/global_macros,
          /agri/global_constants,
          /agri/gprolog_macros,
          /agri/global_brf_macros.

CONSTANTS: BEGIN OF cgl_const,
             bukrs  TYPE bukrs         VALUE 'FAI',
             fldty  TYPE /agri/glcmtyp VALUE 'CITR',
             cmnum  TYPE /agri/glcmnum VALUE 'LARANJA',
             gtart  TYPE /agri/glgtart VALUE 'OWPR',
             status TYPE /agri/gleloek VALUE 'S',
           END OF cgl_const.
