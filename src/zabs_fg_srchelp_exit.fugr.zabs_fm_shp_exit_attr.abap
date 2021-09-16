FUNCTION zabs_fm_shp_exit_attr.
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
*                      attribute                                       *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*--Workarea declarations
  DATA : lwa_attr    TYPE ty_attr,
         lwa_rec_tab LIKE seahlpres,

*--Local variable declarations
         lv_agcat    TYPE /agri/glagcat,

*--Internal table declarations
         lt_attr     TYPE TABLE OF ty_attr.

  IF callcontrol-step EQ 'DISP'.

*--Fetching search help interface data
    READ TABLE shlp-interface
    INTO DATA(lwa_shlp-interface)
    INDEX 1.

    IF lwa_shlp-interface-valfield = zcl_abs_abap_maintain=>c_valfield_mdatnam. "'MDATNAM'
      lv_agcat = zcl_abs_abap_maintain=>c_agcat_mp. "'MP'
    ELSEIF lwa_shlp-interface-valfield = zcl_abs_abap_maintain=>c_valfield_tratnam. "'TRATNAM'
      lv_agcat = zcl_abs_abap_maintain=>c_agcat_tr. "'TR'
    ENDIF.

*--Fetching Attribute Group Data based on attr group category
    SELECT class
    FROM /agri/glagha
    INTO TABLE @DATA(lt_glagha)
    WHERE agcat = @lv_agcat.
    IF sy-subrc <> 0.
      RETURN.
    ELSE.
      SORT lt_glagha BY class.
    ENDIF.

*--Fetching class header data  based on class number
    SELECT clint,
           class
    FROM klah
    INTO TABLE @DATA(lt_klah)
    FOR ALL ENTRIES IN @lt_glagha
    WHERE class = @lt_glagha-class.
    IF sy-subrc <> 0.
      RETURN.
    ELSE.
      SORT lt_klah BY class.
    ENDIF.

*--Fetching Characteristics data
    SELECT clint,
           imerk
    FROM ksml
    INTO TABLE @DATA(lt_ksml)
    FOR ALL ENTRIES IN @lt_klah
    WHERE clint = @lt_klah-clint.
    IF sy-subrc <> 0.
      RETURN.
    ELSE.
      SORT lt_ksml BY clint imerk.
      DELETE ADJACENT DUPLICATES FROM lt_ksml COMPARING clint imerk.
    ENDIF.

*--Fetching attribute and attribute description data
    SELECT a~atinn,
           a~atnam,
           b~atbez
    INTO TABLE @DATA(lt_cabn)
    FROM cabn AS a
    INNER JOIN cabnt AS b
    ON a~atinn = b~atinn
    FOR ALL ENTRIES IN @lt_ksml
    WHERE a~atinn = @lt_ksml-imerk.
    IF sy-subrc = 0.
      SORT lt_cabn BY atinn.
    ENDIF.

*--Processing data to fill internal table with attribute and attribute description
    LOOP AT lt_cabn INTO DATA(lwa_cabn).
      lwa_attr-atnam = lwa_cabn-atnam.
      lwa_attr-atbez = lwa_cabn-atbez.
      APPEND lwa_attr TO lt_attr.
      CLEAR lwa_attr.
    ENDLOOP.

*--Filling record_tab from internal table
    LOOP AT lt_attr INTO lwa_attr.
      record_tab-string+0(30)  = lwa_attr-atnam.
      record_tab-string+31(30) = lwa_attr-atbez.
      APPEND record_tab.
    ENDLOOP.

  ENDIF.

ENDFUNCTION.
