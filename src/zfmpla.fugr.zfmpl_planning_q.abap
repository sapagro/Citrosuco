FUNCTION zfmpl_planning_q.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(ACNUM) TYPE  ZFMACNUM OPTIONAL
*"     VALUE(TOP) TYPE  BAPIMAXROW OPTIONAL
*"     VALUE(MATKL) TYPE  MATKL OPTIONAL
*"     VALUE(EXTWG) TYPE  EXTWG OPTIONAL
*"     VALUE(PERIODO) TYPE  SPMON OPTIONAL
*"  EXPORTING
*"     VALUE(ENTITY) TYPE  ZT_FMPLMENS
*"     VALUE(RETURN) TYPE  BAPIRET2
*"----------------------------------------------------------------------

  DATA: r_acnum   TYPE RANGE OF zfmacnum,
        r_matkl   TYPE RANGE OF matkl,
        r_extwg   TYPE RANGE OF extwg,
        r_periodo TYPE RANGE OF spmon,
        dummy.

  IF acnum IS NOT INITIAL.
    INSERT INITIAL LINE INTO TABLE r_acnum
      ASSIGNING FIELD-SYMBOL(<lwa_acnum>).
    IF sy-subrc EQ 0.
      <lwa_acnum> = 'IEQ'.
      <lwa_acnum>-low = acnum.
    ENDIF.
  ENDIF.

  IF matkl IS SUPPLIED.
    INSERT INITIAL LINE INTO TABLE r_matkl
      ASSIGNING FIELD-SYMBOL(<lwa_matkl>).
    IF sy-subrc EQ 0.
      <lwa_matkl> = 'IEQ'.
      <lwa_matkl>-low = matkl.
    ENDIF.
  ENDIF.

  IF extwg IS SUPPLIED.
    INSERT INITIAL LINE INTO TABLE r_extwg
      ASSIGNING FIELD-SYMBOL(<lwa_extwg>).
    IF sy-subrc EQ 0.
      <lwa_extwg> = 'IEQ'.
      <lwa_extwg>-low = extwg.
    ENDIF.
  ENDIF.

  IF periodo IS SUPPLIED.
    INSERT INITIAL LINE INTO TABLE r_periodo
      ASSIGNING FIELD-SYMBOL(<lwa_periodo>).
    IF sy-subrc EQ 0.
      <lwa_periodo> = 'IEQ'.
      <lwa_periodo>-low = periodo.
    ENDIF.
  ENDIF.

  SELECT * UP TO @top ROWS
    FROM zfmplmens
    INTO TABLE @entity
   WHERE acnum   IN @r_acnum[]
     AND extwg   IN @r_extwg[]
     AND matkl   IN @r_matkl[]
     AND periodo IN @r_periodo[]
    ORDER BY acnum, extwg, matkl, periodo.

  IF sy-subrc NE 0.
*...Área de cultivo &1 inexistente.
    MESSAGE e001(zfmpl) INTO dummy WITH acnum.
    return-type    = 'E'.
    return-id      = 'ZFMPL'.
    return-number  = 001.
    return-message = sy-msgli.
  ENDIF.

ENDFUNCTION.
