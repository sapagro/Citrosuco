FUNCTION-POOL zabs_fg_glmdm.                "MESSAGE-ID ..

INCLUDE /agri/global_constants.
INCLUDE /agri/lglmducon.
INCLUDE /agri/gprolog_macros.
INCLUDE /agri/global_brf_macros.

*--BAdI's
DATA: ref_badi_glmd_all    TYPE REF TO /agri/badi_glmd_all.
DATA: gs_mddoc_infocus TYPE /agri/s_glmd_doc.

**** MACRO ADMIN FILLL
DEFINE admin_data_fill.
  IF &1-updkz EQ c_updkz_new.
    &1-ernam = sy-uname.
    &1-erdat = sy-datum.
    &1-erzet = sy-uzeit.
  ELSEIF &1-updkz EQ c_updkz_update OR
         &1-updkz EQ c_updkz_delete.
    &1-aenam = sy-uname.
    &1-aedat = sy-datum.
    &1-aezet = sy-uzeit.
  ENDIF.
END-OF-DEFINITION.
