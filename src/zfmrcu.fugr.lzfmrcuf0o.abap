*----------------------------------------------------------------------*
***INCLUDE /AGRI/LGLFLUF0O .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  OBJECT_NUMBER_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM object_number_get USING  lv_object  lv_objid
                    CHANGING  lv_objnr.

  DATA: ls_ionr LIKE ionr,
        lv_objnr_new LIKE onr00-objnr.

  SELECT SINGLE obart FROM /agri/tglbo00
           INTO ls_ionr-obart
          WHERE object EQ lv_object.

*  ls_ionr-obart = lv_obart.
  ls_ionr-objid = lv_objid.

  lv_objnr_new = ls_ionr.

  CALL FUNCTION '/AGRI/G_STATUS_SWITCH_OBJNR'
    EXPORTING
      i_objnr_new       = lv_objnr_new
      i_objnr_old       = lv_objnr
    EXCEPTIONS
      objnr_not_changed = 1
      OTHERS            = 2.
  IF sy-subrc <> 0.
  ELSE.
    lv_objnr = lv_objnr_new.
  ENDIF.

ENDFORM.                    " OBJECT_NUMBER_GET
