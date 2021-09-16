*----------------------------------------------------------------------*
***INCLUDE LZABS_FG_CS_MASS_ALLO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_SET OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_set OUTPUT.
  PERFORM status_set.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form STATUS_SET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM status_set.
  SET PF-STATUS 'S100'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Module CONTROLS_DISPLAY OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE controls_display OUTPUT.
  PERFORM controls_display.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form CONTROLS_DISPLAY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM controls_display.

  TYPE-POOLS : vrm.
  DATA: ld_field   TYPE vrm_id,
        lt_listbox TYPE vrm_values,
        lv_date    TYPE sydatum,
        lv_list_year(4),
        wa_listbox LIKE LINE OF lt_listbox.

  IF /agri/s_glflcma-season IS NOT INITIAL.

    SELECT SINGLE *
      FROM /agri/glseason
      INTO /agri/glseason
     WHERE season EQ /agri/s_glflcma-season.
    IF sy-subrc NE 0.
      MESSAGE e046(zabs_msgcls).
    ENDIF.

    lv_date           = gs_variables-date.
    lv_date+4(4)      = /agri/glseason-sperd.
    IF lv_date LE gs_variables-date.
      gs_variables-year = gs_variables-year + 1.
    ENDIF.
    DATA(lv_year) = gs_variables-year - 1.

    DO 10 TIMES.
      lv_list_year = lv_year.
      wa_listbox-key = lv_list_year.
      APPEND wa_listbox TO lt_listbox.
*--BOC-T_T.KONNO-05.28.21
      lv_year = lv_year + 1.
*--EOC-T_T.KONNO-05.28.21
    ENDDO.
    CLEAR lv_year.

    ld_field = '/AGRI/S_GLCSSCRFIELDS-GYEAR'.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = ld_field
        values = lt_listbox.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  EXIT_PROCESSING  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_processing INPUT.
  PERFORM exit_processing.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form EXIT_PROCESSING
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM exit_processing.
  SET SCREEN 0.
  LEAVE SCREEN.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  FCODE_PROCESSING  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE fcode_processing INPUT.
  PERFORM fcode_processing.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form FCODE_PROCESSING
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM fcode_processing.

  DATA : lv_subroutine(40) TYPE c VALUE 'FCODE_'.

  fcode = ok_code.
  CLEAR ok_code.
  CONCATENATE lv_subroutine fcode INTO lv_subroutine.
  PERFORM (lv_subroutine) IN PROGRAM (sy-repid)
                          IF FOUND.

ENDFORM.            "FCODE_PROCESSING

*&---------------------------------------------------------------------*
*& Form FCODE_CANC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM fcode_cont.

  LEAVE TO SCREEN 0.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FCODE_CANC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM fcode_canc.

  CLEAR: /agri/s_glflcma, /agri/s_glcsscrfields.
  LEAVE TO SCREEN 0.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  SEASON_VALIDATION  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE season_validation INPUT.
  PERFORM season_validation.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form SEASON_VALIDATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM season_validation.
  IF ok_code = 'CANC'.
   RETURN.
  ENDIF.
  IF /agri/s_glflcma-season IS INITIAL.
    MESSAGE e046(zabs_msgcls).
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  YEAR_VALIDATION  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE year_validation INPUT.
  PERFORM year_validation.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form YEAR_VALIDATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM year_validation.
  IF ok_code = 'CANC'.
   RETURN.
  ENDIF.
  IF /agri/s_glcsscrfields-gyear IS INITIAL AND
     /agri/s_glflcma-season IS NOT INITIAL.
     /agri/s_glcsscrfields-gyear = gs_variables-year.
  ENDIF.
ENDFORM.
