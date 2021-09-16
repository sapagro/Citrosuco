FUNCTION zabs_fm_shp_exit_fleet.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     REFERENCE(SHLP) TYPE  SHLP_DESCR
*"     REFERENCE(CALLCONTROL) TYPE  DDSHF4CTRL
*"----------------------------------------------------------------------

  IF callcontrol-step EQ 'DISP'.

    SELECT fleet
      FROM zabst_fleet
      INTO TABLE @DATA(lt_fleet).
    IF sy-subrc EQ 0.
      SORT lt_fleet BY fleet.
      DELETE ADJACENT DUPLICATES FROM lt_fleet COMPARING fleet.
    ENDIF.

*--Filling record_tab from internal table
    REFRESH record_tab[].
    LOOP AT lt_fleet INTO DATA(lwa_fleet).
      record_tab-string+3(10)  = lwa_fleet-fleet.
      APPEND record_tab.
    ENDLOOP.

  ENDIF.

ENDFUNCTION.
