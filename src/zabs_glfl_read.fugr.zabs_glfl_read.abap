FUNCTION zabs_glfl_read.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IV_TERRAIN) TYPE  /AGRI/GLTPLNR_FL
*"  EXPORTING
*"     VALUE(ET_TERRAIN) TYPE  /AGRI/T_GLFL_DOC
*"     VALUE(ES_FLHDR) TYPE  /AGRI/S_GLFLOT
*"     VALUE(ES_ADRC) TYPE  /AGRI/S_GLADRC
*"     VALUE(ET_IFLOTX) TYPE  /AGRI/T_GLIFLOTX
*"     VALUE(ET_IHPA) TYPE  /AGRI/T_GLIHPA
*"     VALUE(ET_FLPPL) TYPE  /AGRI/T_GLFLPPL
*"     VALUE(ET_FLATG) TYPE  /AGRI/T_GLFLATG
*"     VALUE(ET_FLATV) TYPE  /AGRI/T_GLFLATV
*"     VALUE(ET_FLCMA) TYPE  /AGRI/T_GLFLCMA
*"     VALUE(ET_FLOWN) TYPE  /AGRI/T_GLFLOWN
*"     VALUE(ET_FLOS) TYPE  /AGRI/T_GLFLOS
*"----------------------------------------------------------------------

  DATA: lt_tplnr   TYPE /agri/t_gltplnr,
        lt_fl_doc  TYPE /agri/t_glfl_doc,
        lv_terrain TYPE /agri/gltplnr_fl.

  lv_terrain = iv_terrain.

  CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
    EXPORTING
      input      = lv_terrain
    IMPORTING
      output     = lv_terrain
    EXCEPTIONS
      not_found  = 1
      not_active = 2
      OTHERS     = 3.

  APPEND lv_terrain TO lt_tplnr.

  CALL FUNCTION '/AGRI/GLFL_VIEW'
    EXPORTING
      it_tplnr       = lt_tplnr
    IMPORTING
      et_fldoc       = lt_fl_doc
    EXCEPTIONS
      no_data_exists = 1
      OTHERS         = 2.

  IF sy-subrc EQ 0.
    et_terrain[] = lt_fl_doc[].
    READ TABLE lt_fl_doc INTO DATA(lwa_terrain) INDEX 1.
    IF sy-subrc EQ 0.
      es_flhdr    = lwa_terrain-x-flhdr.
      es_adrc     = lwa_terrain-x-adrc.
      et_iflotx[] = lwa_terrain-x-iflotx[].
      et_ihpa[]   = lwa_terrain-x-ihpa[].
      et_flppl[]  = lwa_terrain-x-flppl[].
      et_flatg[]  = lwa_terrain-x-flatg[].
      et_flatv[]  = lwa_terrain-x-flatv[].
      et_flcma[]  = lwa_terrain-x-flcma[].
      et_flown[]  = lwa_terrain-x-flown[].
      et_flos[]   = lwa_terrain-x-flos[].
    ENDIF.
  ENDIF.

ENDFUNCTION.
