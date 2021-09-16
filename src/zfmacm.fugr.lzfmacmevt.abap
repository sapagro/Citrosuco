*&---------------------------------------------------------------------*
*& Include          LZFMACMEVT
*&---------------------------------------------------------------------*


AT SELECTION-SCREEN OUTPUT.
  SET TITLEBAR 'T010'.

  IF p_ajahr IS INITIAL.
    p_ajahr = sy-datum(4).
  ENDIF.
*  PERFORM dropdown_tables_display.
  PERFORM screen_modify.

AT SELECTION-SCREEN.
  IF sscrfields-ucomm EQ 'CRET'.
    PERFORM mandatory_field_check.
    PERFORM croparea_create.
  ENDIF.

AT SELECTION-SCREEN ON p_datbi.
  PERFORM high_date_check.


  DEFINE inputparameter_refresh_all.
    CLEAR:
    p_acdes,
    p_actyp,
    p_ajahr,
    p_datab,
    p_datbi,
    so_werks[].
  END-OF-DEFINITION.

  DEFINE converse_terrains.

    CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
      EXPORTING
        input            = &1
     IMPORTING
       output            = &2
     EXCEPTIONS
       not_found        = 1
       not_active       = 2
       OTHERS           = 3.

  END-OF-DEFINITION.
