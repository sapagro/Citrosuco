FUNCTION-POOL ZFMPLU.                  "MESSAGE-ID ..

INCLUDE /agri/global_constants.
INCLUDE zlfmplucon.
INCLUDE /agri/abgl_constants.
INCLUDE /agri/gprolog_macros.

**DATA: gv_objnr_tmp TYPE /agri/s_glflot-objnr,
**      gs_itob      TYPE itob.

****ESP6 Task #29928 - Partner Engine Changes
DATA: BEGIN OF gs_variables,
        gpar_version,
      END OF gs_variables.
****

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

DEFINE bapi_msg_to_log_wa.
  CLEAR &2.
  &2-msgid = &1-id.
  &2-msgno = &1-number.
  &2-msgty = &1-type.
  &2-msgv1 = &1-message_v1.
  &2-msgv2 = &1-message_v2.
  &2-msgv3 = &1-message_v3.
  &2-msgv4 = &1-message_v4.
  &2-aplobj = &1-log_no.
END-OF-DEFINITION.
