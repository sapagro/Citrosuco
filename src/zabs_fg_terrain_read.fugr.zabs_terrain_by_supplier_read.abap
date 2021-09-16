FUNCTION zabs_terrain_by_supplier_read.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IV_LIFNR) TYPE  LIFNR
*"     VALUE(IV_FUNCAO_PARCEIRO) TYPE  PARVW OPTIONAL
*"  EXPORTING
*"     VALUE(ET_TERRAIN) TYPE  ZABS_TTY_GLFLOT
*"  EXCEPTIONS
*"      SUPPLIER_NOT_FOUND
*"----------------------------------------------------------------------

  DATA: lwa_lfa1    TYPE lfa1,
        lr_partner  TYPE RANGE OF /agri/glptrno,
        lwa_partner LIKE LINE OF lr_partner,
        lv_parvw    TYPE /agri/glflptr-parvw,
        lv_lifnr_fo TYPE lifnr.

  IF iv_funcao_parceiro IS SUPPLIED.
    lv_parvw = iv_funcao_parceiro.
  ELSE.
    lv_parvw = 'FO'.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_PARVW_INPUT'
    EXPORTING
      input  = lv_parvw
    IMPORTING
      output = lv_parvw.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = iv_lifnr
    IMPORTING
      output = lv_lifnr_fo.

  CALL FUNCTION 'READ_LFA1'
    EXPORTING
      xlifnr         = lv_lifnr_fo
    IMPORTING
      xlfa1          = lwa_lfa1
    EXCEPTIONS
      key_incomplete = 1
      not_authorized = 2
      not_found      = 3
      OTHERS         = 4.

  IF sy-subrc <> 0.
*-- Not found
    RAISE supplier_not_found.
  ELSE.
    SELECT ptrno AS low
      FROM /agri/glflptr
      INTO CORRESPONDING FIELDS OF TABLE @lr_partner
     WHERE parvw = @lv_parvw
       AND lifnr = @lv_lifnr_fo.

    IF sy-subrc EQ 0.
      lwa_partner = 'IEQ'.
      MODIFY lr_partner FROM lwa_partner TRANSPORTING sign option
        WHERE low IS NOT INITIAL.
    ENDIF.

    IF lr_partner[] IS NOT INITIAL.
      SELECT *
        FROM /agri/glflot
        INTO TABLE @DATA(lt_terrain)
       WHERE ptrno IN @lr_partner[].
    ENDIF.
  ENDIF.

  LOOP AT lt_terrain ASSIGNING FIELD-SYMBOL(<lwa_terrain>).
    IF <lwa_terrain>-tplnr_fl IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
        EXPORTING
          input  = <lwa_terrain>-tplnr_fl
        IMPORTING
          output = <lwa_terrain>-tplnr_fl.
    ENDIF.

    IF <lwa_terrain>-tplma IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
        EXPORTING
          input  = <lwa_terrain>-tplma
        IMPORTING
          output = <lwa_terrain>-tplma.
    ENDIF.
  ENDLOOP.

  et_terrain[] = lt_terrain[].

ENDFUNCTION.
