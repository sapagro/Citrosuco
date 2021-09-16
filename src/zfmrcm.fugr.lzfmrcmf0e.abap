*----------------------------------------------------------------------*
***INCLUDE /AGRI/LFMRCMNF0E .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  EXIT_PROCESSING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM exit_processing .
  DATA: lv_answer.

  fcode = ok_code.
  CLEAR ok_code.
  CASE sy-dynnr.
    WHEN c_screen-main_screen.

****Check whether there are any changes
      PERFORM changes_confirm CHANGING lv_answer.

      IF lv_answer NE 'A'.
        IF gs_variables-document_mode NE c_mode_display.
          PERFORM document_infocus_unlock
            USING gs_rckey.
        ENDIF.
      ELSE.
        IF ok_code EQ c_fcode-save.
          IF fcode EQ c_fcode-exit.
            gs_variables-exit_after_save = c_true.
          ELSE.
            gs_variables-exit_after_save = 'C'.
          ENDIF.
          ok_code = c_fcode-back.
        ENDIF.
      ENDIF.

      CHECK lv_answer NE 'A'.

      IF fcode EQ c_fcode-exit.
        SET SCREEN 0.
        LEAVE SCREEN.
      ELSE.
        PERFORM document_data_initialize USING c_true.
        SET SCREEN 100.
        LEAVE SCREEN.
      ENDIF.
      EXIT.
    WHEN c_screen-create_mass_dialog.
      EXIT.
    WHEN c_screen-create_dialog.
      gs_variables-cancelled = c_true.
    WHEN c_screen-popup_version_modify.
      CLEAR: zsc_fmrcvrs.
      CALL FUNCTION 'DEQUEUE_ALL'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN c_screen-popup_recipe_create.
      CLEAR: zsc_fmrchdr.
      CALL FUNCTION 'DEQUEUE_ALL'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN c_screen-popup_version_create.
      CLEAR: zsc_fmrcvrs.
      CALL FUNCTION 'DEQUEUE_ALL'.
      SET SCREEN 0.
      LEAVE SCREEN.
  ENDCASE.

  SET SCREEN 0.
  LEAVE SCREEN.

ENDFORM.                    " EXIT_PROCESSING
