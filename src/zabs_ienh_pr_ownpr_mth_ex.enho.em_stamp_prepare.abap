METHOD stamp_prepare .

*  DATA: lv_tplnr_external TYPE /agri/gltplnr_fl,
*        lv_week           TYPE  scal-week.
*
*  CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
*    EXPORTING
*      input  = i_tplnr_fl
*    IMPORTING
*      output = lv_tplnr_external.
*
*  c_imovs = lv_tplnr_external+6(6).
*  c_talhao = lv_tplnr_external+0(5).
*
*  CALL FUNCTION 'DATE_GET_WEEK'
*    EXPORTING
*      date         = i_date
*    IMPORTING
*      week         = lv_week
*    EXCEPTIONS
*      date_invalid = 1
*      OTHERS       = 2.
*
*  IF sy-subrc EQ 0.
*    MOVE lv_week to c_weeks.
*  ENDIF.

ENDMETHOD.
