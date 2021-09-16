FUNCTION zabs_fm_shp_idresource.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR
*"     VALUE(CALLCONTROL) TYPE  DDSHF4CTRL
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ly_out,
           idresource  TYPE /agri/fmidrsc,
           description TYPE /agri/fmdescr,
           lifnr       TYPE lifnr,
           pernr       TYPE co_pernr,
         END OF ly_out.

  DATA: lt_fmacres TYPE STANDARD TABLE OF /agri/fmacres INITIAL SIZE 0,
        lt_out     TYPE STANDARD TABLE OF ly_out INITIAL SIZE 0,
        lwa_out    LIKE LINE OF lt_out.

  IF callcontrol-step <> 'SELECT'
  AND callcontrol-step <> 'DISP'.
    EXIT.
  ENDIF.

  IF callcontrol-step = 'SELECT'.
    SELECT *
      FROM /agri/fmacres
      INTO TABLE lt_fmacres.

    LOOP AT lt_fmacres INTO DATA(lwa_fmacres).
      lwa_out-idresource  = lwa_fmacres-idresource.
      lwa_out-description = lwa_fmacres-description.
      lwa_out-lifnr       = lwa_fmacres-lifnr.
      lwa_out-pernr       = lwa_fmacres-pernr.
      APPEND lwa_out TO lt_out.
      record_tab-string = lwa_out.
      SHIFT record_tab-string RIGHT DELETING TRAILING space.
      APPEND record_tab.
    ENDLOOP.

*    callcontrol-step = 'DISP'.
  ELSEIF callcontrol-step EQ 'DISP'.
    SELECT *
      FROM /agri/fmacres
      INTO TABLE lt_fmacres.

    LOOP AT lt_fmacres INTO lwa_fmacres.
      lwa_out-idresource  = lwa_fmacres-idresource.
      lwa_out-description = lwa_fmacres-description.
      lwa_out-lifnr       = lwa_fmacres-lifnr.
      lwa_out-pernr       = lwa_fmacres-pernr.
      APPEND lwa_out TO lt_out.
      record_tab-string = lwa_out.
      SHIFT record_tab-string RIGHT DELETING TRAILING space.
      APPEND record_tab.
    ENDLOOP.

    CALL FUNCTION 'F4UT_RESULTS_MAP'
      TABLES
        shlp_tab          = shlp_tab
        record_tab        = record_tab
        source_tab        = lt_out
      CHANGING
        shlp              = shlp
        callcontrol       = callcontrol
      EXCEPTIONS
        illegal_structure = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  ENDIF.

ENDFUNCTION.
