FUNCTION zfmpl_planning_r.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(ACNUM) TYPE  ZFMACNUM
*"     VALUE(EXTWG) TYPE  EXTWG
*"     VALUE(MATKL) TYPE  MATKL
*"     VALUE(PERIODO) TYPE  SPMON
*"  EXPORTING
*"     VALUE(ENTITY) TYPE  ZFMPLMENS
*"     VALUE(RETURN) TYPE  BAPIRET2
*"----------------------------------------------------------------------

  DATA: dummy.

  SELECT SINGLE *
    FROM zfmplmens
    INTO @entity
   WHERE acnum   EQ @acnum
     AND extwg   EQ @extwg
     AND matkl   EQ @matkl
     AND periodo EQ @periodo.

  IF sy-subrc NE 0.
*...Área de cultivo &1 inexistente.
    MESSAGE e001(zfmpl) INTO dummy WITH acnum.
    return-type    = 'E'.
    return-id      = 'ZFMPL'.
    return-number  = 001.
    return-message = sy-msgli.
  ENDIF.

ENDFUNCTION.
