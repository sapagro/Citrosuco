FUNCTION zabs_fm_shp_exit_attr_grp.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     REFERENCE(SHLP) TYPE  SHLP_DESCR
*"     REFERENCE(CALLCONTROL) TYPE  DDSHF4CTRL
*"----------------------------------------------------------------------
************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* FM Name           :  ZABS_FM_SHP_EXIT_ATTR                           *
* Created By        :  Jetendra Mantena                                *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  09.09.2019                                      *
* TR                :  C4DK901784                                      *
* Version           :  001                                             *
* Description       :  Search help exit for Measurement doc and terrain*
*                      attribute group                                 *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*--Local variable declaration
  DATA : lv_agcat     TYPE /agri/glagcat,

*--Internal table declaration
         lt_attr_grp  TYPE TABLE OF ty_attr_grp,

*--Workarea declarations
         lwa_attr_grp TYPE ty_attr_grp,
         lwa_rec_tab  LIKE seahlpres.

  IF callcontrol-step EQ 'DISP'.

*--Fetching search help interface data
    READ TABLE shlp-interface
     INTO DATA(lwa_shlp-interface)
         INDEX 1.

    IF lwa_shlp-interface-valfield = zcl_abs_abap_maintain=>c_valfield_mdclass. "'MDCLASS'
      lv_agcat = zcl_abs_abap_maintain=>c_agcat_mp. "'MP'
    ELSEIF lwa_shlp-interface-valfield = zcl_abs_abap_maintain=>c_valfield_trclass. "'TRCLASS'
      lv_agcat = zcl_abs_abap_maintain=>c_agcat_tr. "'TR'
    ENDIF.

*--Fetching Attribute Group Data based on attr group category
    SELECT class
    FROM /agri/glagha
    INTO TABLE @DATA(lt_glagha)
    WHERE agcat = @lv_agcat.

*--Fetching class header data  based on class number
    IF lt_glagha IS NOT INITIAL.
      SELECT clint,class
      FROM klah
      INTO TABLE @DATA(lt_klah)
      FOR ALL ENTRIES IN @lt_glagha
      WHERE class = @lt_glagha-class.
    ENDIF.

*--Fetching attribute description
    IF lt_klah IS NOT INITIAL.
      SELECT clint,kschl
      FROM swor
      INTO TABLE @DATA(lt_swor)
      FOR ALL ENTRIES IN @lt_klah
      WHERE clint = @lt_klah-clint
      AND   spras = @sy-langu.
    ENDIF.

*--Processing data to fill internal table with attribute group and description
    LOOP AT lt_glagha INTO DATA(lwa_glagha).
      lwa_attr_grp-class = lwa_glagha-class.
      READ TABLE lt_klah INTO DATA(lwa_klah)
                         WITH KEY class = lwa_glagha-class.
      IF sy-subrc = 0.
        READ TABLE lt_swor INTO DATA(lwa_swor)
                           WITH KEY clint = lwa_klah-clint.
        IF sy-subrc = 0.

          lwa_attr_grp-kschl = lwa_swor-kschl.
        ENDIF.
      ENDIF.
      APPEND lwa_attr_grp TO lt_attr_grp.
      CLEAR lwa_attr_grp.
    ENDLOOP.

*--Filling record_tab from internal table
    LOOP AT lt_attr_grp INTO lwa_attr_grp.
      record_tab-string+0(18) = lwa_attr_grp-class.
      record_tab-string+19(30) = lwa_attr_grp-kschl.
      APPEND record_tab.
    ENDLOOP.

  ENDIF.

ENDFUNCTION.
