FUNCTION-POOL zabs_fg_cs_mass_all.          "MESSAGE-ID ..

* INCLUDE LZABS_FG_CS_MASS_ALLD...           " Local class definition

TABLES: /agri/s_glflcma, /agri/s_glcsscrfields, /agri/glseason.

DATA: fcode   TYPE sy-ucomm,
      ok_code TYPE sy-ucomm.

DATA: BEGIN OF gs_variables,
       year  TYPE gjahr,
       date  TYPE sydatum,
      END OF gs_variables.
