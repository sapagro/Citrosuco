FUNCTION zabs_fm_shp_exit_cs_afn.
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
* Description       :  Search help exit for Fieldnames                 *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*--Object declaration
  DATA: lref_descr   TYPE REF TO cl_abap_structdescr,

*--Workarea declarations
        lwa_glcsview TYPE zabs_str_glflcmaca,
        lwa_comp     TYPE abap_compdescr,
        lwa_cs_afn   TYPE /plmb/s_fieldname,

*--Internal table declaration
        lt_cs_afn    TYPE /plmb/t_fieldname.


  IF callcontrol-step EQ 'DISP'.

*--Retrieving field names dynamically into internal table
    lref_descr ?= cl_abap_structdescr=>describe_by_data( lwa_glcsview ).

    LOOP AT lref_descr->components INTO lwa_comp.
      lwa_cs_afn-fieldname = lwa_comp-name.
      APPEND lwa_cs_afn TO lt_cs_afn.
    ENDLOOP.

    IF lt_cs_afn IS INITIAL.
      RETURN.
    ENDIF.

*--Fetching table fields data based on field name
    SELECT fieldname,
           rollname
      FROM dd03l
    INTO TABLE @DATA(lt_dd03l)
    FOR ALL ENTRIES IN @lt_cs_afn
    WHERE fieldname = @lt_cs_afn-fieldname
      AND tabname   = @zcl_abs_abap_maintain=>c_tbl_fields_tabname.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

*--Fetching Data element texts data to get field description
    SELECT rollname,
           scrtext_l
      FROM dd04t
    INTO TABLE @DATA(lt_dd04t)
    FOR ALL ENTRIES IN @lt_dd03l
    WHERE rollname   = @lt_dd03l-rollname
      AND ddlanguage = @sy-langu.
    IF  sy-subrc = 0.

*--Processing data to fill record_tab
      LOOP AT lt_cs_afn INTO lwa_cs_afn.
        READ TABLE lt_dd03l INTO DATA(lwa_dd03l)
                            WITH KEY fieldname = lwa_cs_afn-fieldname.
        IF sy-subrc = 0.
          record_tab-string+0(30) = lwa_cs_afn-fieldname.
          READ TABLE lt_dd04t INTO DATA(lwa_dd04t)
                              WITH KEY rollname = lwa_dd03l-rollname.
          IF sy-subrc = 0.
            record_tab-string+31(40) = lwa_dd04t-scrtext_l.
          ENDIF.
        ENDIF.
        APPEND record_tab.
      ENDLOOP.
    ENDIF.

  ENDIF.

ENDFUNCTION.
