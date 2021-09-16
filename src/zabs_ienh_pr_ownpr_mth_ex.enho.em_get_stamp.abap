METHOD get_stamp .

*  DATA: ls_stam TYPE yocotaseg.
*
*  IF sy-uname EQ 'T_T.KONNO'.
*    BREAK-POINT.
*  ENDIF.
*
*  SELECT SINGLE * FROM yocotaseg INTO ls_stam
*    WHERE centro = i_werks
*      AND imov = i_imovs
*      AND talhao  = i_talhao
*      AND variedade = i_varie
*      AND semana = i_weeks.
*
*  IF sy-subrc EQ 0.
*    c_stamp = ls_stam-carimbo.
*  ENDIF.

ENDMETHOD.
