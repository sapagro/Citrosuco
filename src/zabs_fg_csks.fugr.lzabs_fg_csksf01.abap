*----------------------------------------------------------------------*
***INCLUDE LZABS_FG_CSKSF01.
*----------------------------------------------------------------------*

FORM fetch_description.

  IF zabs_csks-kostl IS NOT INITIAL.
    SELECT t~ltext FROM cskt AS t
      INNER JOIN csks AS c
      ON t~kostl = c~kostl UP TO 1 ROWS
      INTO zabs_csks-ltext
     WHERE t~spras = 'P'
       AND c~kostl = zabs_csks-kostl.
    ENDSELECT.
  ENDIF.

ENDFORM.
