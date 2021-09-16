*----------------------------------------------------------------------*
***INCLUDE /AGRI/LGLFLUF0N .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  NOTES_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM notes_save USING  lv_objtyp
                       lv_objkey
                       lv_updkz
                       lv_update_task.

  DATA: lv_subobject TYPE /agri/gsubobject.

****Agreement (Header)
  IF lv_updkz EQ c_updkz_delete.
    CALL FUNCTION '/AGRI/G_NOTES_DELETE'
      EXPORTING
        i_objtyp      = lv_objtyp
        i_objkey      = lv_objkey
        i_subobj      = space
*       I_ONLY_BUFFER = ' '
*       I_COMMIT      = ' '
        i_update_task = lv_update_task.
  ELSE.
    CALL FUNCTION '/AGRI/G_NOTES_SAVE'
      EXPORTING
        i_objtyp      = lv_objtyp
        i_objkey      = lv_objkey
        i_subobj      = space
*       I_COMMIT      = ' '
        i_update_task = lv_update_task.
  ENDIF.

ENDFORM.                    " NOTES_SAVE
