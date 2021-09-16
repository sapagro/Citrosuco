*&---------------------------------------------------------------------*
*& Include          LZFMRCMEVT
*&---------------------------------------------------------------------*


AT SELECTION-SCREEN OUTPUT.
  SET TITLEBAR 'T010'.

**  IF p_ajahr IS INITIAL.
**    p_ajahr = sy-datum(4).
**  ENDIF.
***  PERFORM dropdown_tables_display.
**  PERFORM screen_modify.
**
**AT SELECTION-SCREEN.
**  IF sscrfields-ucomm EQ 'CRET'.
**    PERFORM mandatory_field_check.
**    PERFORM croparea_create.
**  ENDIF.
**
**AT SELECTION-SCREEN ON p_datbi.
**  PERFORM high_date_check.
**
**
**  DEFINE inputparameter_refresh_all.
**    CLEAR:
**    p_acdes,
**    p_RCTYP,
**    p_ajahr,
**    p_datab,
**    p_datbi,
**    p_werks.
**  END-OF-DEFINITION.
**
**  DEFINE converse_terrains.
**
**    CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
**      EXPORTING
**        input            = &1
**     IMPORTING
**       OUTPUT            = &2
**     EXCEPTIONS
**       NOT_FOUND        = 1
**       NOT_ACTIVE       = 2
**       OTHERS           = 3.
**
**    END-OF-DEFINITION.
