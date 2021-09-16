FUNCTION zabs_terrain_by_property_read.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IV_CODIGO_IMOVEL) TYPE  /AGRI/GLTPLNR_FL
*"  EXPORTING
*"     VALUE(ET_TERRAIN) TYPE  ZABS_TTY_GLFLOT
*"  EXCEPTIONS
*"      PROPERTY_IS_NOT_FARM
*"----------------------------------------------------------------------

  CONSTANTS: BEGIN OF lc_nivel_terreno,
               fazenda TYPE /agri/gltplvl VALUE 1,
               talhao  TYPE /agri/gltplvl VALUE 2,
             END OF lc_nivel_terreno.

  DATA: lv_codigo_imovel TYPE /agri/gltplnr_fl.

  lv_codigo_imovel = iv_codigo_imovel.

  CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
    EXPORTING
      input      = lv_codigo_imovel
    IMPORTING
      output     = lv_codigo_imovel
    EXCEPTIONS
      not_found  = 1
      not_active = 2
      OTHERS     = 3.

  SELECT SINGLE tplnr_fl, tplma
    FROM /agri/glflot
    INTO @DATA(lwa_farm)
   WHERE tplnr_fl = @lv_codigo_imovel
     AND tplvl    = @lc_nivel_terreno-fazenda.

  IF sy-subrc NE 0.
    RAISE property_is_not_farm.
  ELSE.
    SELECT *
      FROM /agri/glflot
      INTO TABLE @DATA(lt_terrain)
     WHERE tplma = @lv_codigo_imovel.
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
