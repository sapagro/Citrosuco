*----------------------------------------------------------------------*
***INCLUDE /AGRI/LFMACMNF0A .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM authority_check  USING lwa_achdr TYPE zsc_fmachdr
                            lv_activity
                            lv_message_type
                   CHANGING lv_subrc.

ENDFORM.                    " AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  ADMIN_DATA_MAINTAIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM admin_data_maintain .

  DATA: lv_subobj       LIKE dd03p-fieldname,
        ls_screenfields TYPE /agri/gadminscrfields.

****Header Admin Data
  MOVE-CORRESPONDING gs_acdoc_infocus-x-achdr TO ls_screenfields.
  CALL FUNCTION '/AGRI/GADMIN_SUBSCREEN_IMPORT'
    EXPORTING
      i_objtyp              = c_object-bor
      i_objkey              = gs_acdoc_infocus-x-achdr-acnum
      i_subobj              = lv_subobj
      is_scrfields          = ls_screenfields
      i_suppress_activities = c_true
      i_suppress_notes      = c_false
    CHANGING
      c_program             = gs_variables-admin_program
      c_subscreen           = gs_variables-subscr_admin.

ENDFORM.                    " ADMIN_DATA_MAINTAIN
