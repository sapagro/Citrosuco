FUNCTION zabs_fm_cs_add_fields_upd.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IS_MDHDR) TYPE  /AGRI/S_GLMDHDR
*"     REFERENCE(IT_MDITM) TYPE  /AGRI/T_GLMDITM
*"     REFERENCE(IT_MDATV) TYPE  /AGRI/T_GLMDATV
*"----------------------------------------------------------------------
************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* FM Name           :  ZABS_FM_CS_ADD_FIELDS_UPD                       *
* Created By        :  Jetendra Mantena                                *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  09.18.2019                                      *
* TR                :  C4DK901784                                      *
* Version           :  001                                             *
* Description       :  The attribute values from measurement document  *
*                      workbench gets updated in the crop season       *
*                      based on custom table.                          *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*--Internal table declarations
  DATA: lt_tplnr      TYPE /agri/t_gltplnr,
        lt_fldoc      TYPE /agri/t_glfl_doc,
        lt_clint      TYPE /agri/t_gclint,
        lt_atnam      TYPE /agri/t_gatnam,
        lt_mdclass    TYPE /agri/t_glatgrp,
        lt_messages   TYPE /agri/t_gprolog,
        lt_cskey      TYPE /agri/t_glcs_key,
        lt_csdoc      TYPE /agri/t_glcs_doc,
        lt_csdoc_temp TYPE /agri/t_glcs_doc,
        lt_klah       TYPE tt_klah,
        lt_cs_afn     TYPE /plmb/t_fieldname,

*--Workarea declarations
        lwa_atnam     TYPE /agri/s_gatnam,
        lwa_clint     TYPE /agri/s_gclint,
        lwa_attr_val  TYPE auspdata,
        lwa_flatv     TYPE /agri/s_glflatv,
        lwa_glflcmaca TYPE zabs_str_glflcmaca,
        lwa_cs_afn    TYPE /plmb/s_fieldname,
        lwa_comp      TYPE abap_compdescr,

*--Local Reference declarations
        lref_descr    TYPE REF TO cl_abap_structdescr,
        lref_date     TYPE REF TO data.

  FIELD-SYMBOLS: <fs_csdoc> TYPE /agri/s_glcs_doc,
                 <fs_date>  TYPE any,
                 <fs_value> TYPE any.

*--Get Fieldnames from additional fileds structure of crop season
  lref_descr ?= cl_abap_structdescr=>describe_by_data( lwa_glflcmaca ).
  LOOP AT lref_descr->components INTO lwa_comp.
    lwa_cs_afn-fieldname = lwa_comp-name.
    APPEND lwa_cs_afn TO lt_cs_afn.
  ENDLOOP.

*--Fetch MD attribute, attribute group and filedname from custom table
  SELECT *
    FROM zabst_md_atr_map
    INTO TABLE @DATA(lt_md_attr_map)
    FOR ALL ENTRIES IN @lt_cs_afn
    WHERE fieldname = @lt_cs_afn-fieldname
      AND mdclass   = @is_mdhdr-mpgrp.
  IF sy-subrc <> 0.
    RETURN.
  ELSE.
    SORT lt_md_attr_map BY mdclass mdatnam.
  ENDIF.

*--Fetch class header data
  SELECT *
    FROM klah
    INTO TABLE lt_klah
    WHERE class = is_mdhdr-mpgrp.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

*--Calling meathod to get attribute header data
  CALL METHOD /agri/cl_gattr_utils=>attribute_groups_attr_read
    EXPORTING
      it_klah  = lt_klah
      i_agtyp  = zcl_abs_abap_maintain=>c_agtyp_measurement_doc
    IMPORTING
      et_athdr = DATA(lt_athdr).
  IF lt_athdr IS NOT INITIAL.
    SORT lt_athdr BY atnam.
  ENDIF.

  SELECT tplnr_fl
         contr
    FROM /agri/glflcma
    INTO TABLE lt_cskey
    WHERE tplnr_fl EQ is_mdhdr-tplnr_fl
*      AND cmnum    EQ is_mdhdr-cmnum
      AND class    EQ '1'
      AND astat    EQ 'A'
      AND loevm    EQ space.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  CALL FUNCTION '/AGRI/GLCS_VIEW'
    EXPORTING
*     I_MODE         = 'V'
      it_cskey       = lt_cskey
    IMPORTING
      et_csdoc       = lt_csdoc_temp
    EXCEPTIONS
      no_data_exists = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  SORT lt_csdoc_temp BY tplnr_fl contr.

  LOOP AT lt_csdoc_temp INTO DATA(lwa_csdoc_temp).

    IF lwa_csdoc_temp-x-cshdr-datab LE is_mdhdr-mdate AND
       lwa_csdoc_temp-x-cshdr-datbi GE is_mdhdr-mdate AND
       lwa_csdoc_temp-x-cshdr-mcycl EQ abap_true.

      DATA(lv_contr) = lwa_csdoc_temp-contr.

      UNASSIGN <fs_csdoc>.
      ASSIGN lwa_csdoc_temp TO <fs_csdoc>.
      IF <fs_csdoc> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      LOOP AT lt_md_attr_map INTO DATA(lwa_md_attr_map).
        READ TABLE lt_athdr INTO DATA(ls_athdr)
        WITH KEY atnam = lwa_md_attr_map-mdatnam
        BINARY SEARCH.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        READ TABLE it_mdatv INTO DATA(lwa_mdatv)
        WITH KEY atinn = ls_athdr-atinn.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
*--Convert float to char value.
        IF lwa_mdatv-atwrt IS INITIAL
       AND lwa_mdatv-atflv IS NOT INITIAL."--Added on 08/10/2020 T_C.KARANAM
          CREATE DATA lref_date TYPE p LENGTH ls_athdr-anzst DECIMALS ls_athdr-anzdz.
          ASSIGN lref_date->* TO <fs_date>.
          <fs_date> = lwa_mdatv-atflv.
          lwa_mdatv-atwrt = <fs_date>.
          CONDENSE lwa_mdatv-atwrt NO-GAPS.
        ENDIF.

        ASSIGN COMPONENT lwa_md_attr_map-fieldname OF STRUCTURE <fs_csdoc>-x-cshdr TO <fs_value>.
        IF <fs_value> IS ASSIGNED.
          <fs_value> = lwa_mdatv-atwrt.
          <fs_csdoc>-x-cshdr-updkz = 'U'.
          <fs_csdoc>-updkz = abap_true.
        ENDIF.
      ENDLOOP.
      APPEND <fs_csdoc> TO lt_csdoc.

    ENDIF.

    READ TABLE lt_csdoc_temp INTO DATA(lwa_csdoc)
      WITH KEY x-cshdr-contr_ref = lv_contr.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    UNASSIGN <fs_csdoc>.
    ASSIGN lwa_csdoc TO <fs_csdoc>.

    lv_contr = lwa_csdoc-x-cshdr-contr.
    CLEAR lwa_md_attr_map.
    LOOP AT lt_md_attr_map INTO lwa_md_attr_map.
      READ TABLE lt_athdr INTO ls_athdr
      WITH KEY atnam = lwa_md_attr_map-mdatnam
      BINARY SEARCH.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      READ TABLE it_mdatv INTO lwa_mdatv
      WITH KEY atinn = ls_athdr-atinn.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
*--Convert float to char value.
      IF lwa_mdatv-atwrt IS INITIAL
     AND lwa_mdatv-atflv IS NOT INITIAL."--Added on 08/10/2020 T_C.KARANAM
        CREATE DATA lref_date TYPE p LENGTH ls_athdr-anzst DECIMALS ls_athdr-anzdz.
        ASSIGN lref_date->* TO <fs_date>.
        <fs_date> = lwa_mdatv-atflv.
        lwa_mdatv-atwrt = <fs_date>.
        CONDENSE lwa_mdatv-atwrt NO-GAPS.
      ENDIF.

      ASSIGN COMPONENT lwa_md_attr_map-fieldname OF STRUCTURE <fs_csdoc>-x-cshdr TO <fs_value>.
      IF <fs_value> IS ASSIGNED.
        <fs_value> = lwa_mdatv-atwrt.
        <fs_csdoc>-x-cshdr-updkz = 'U'.
        <fs_csdoc>-updkz = abap_true.
      ENDIF.
    ENDLOOP.

    APPEND <fs_csdoc> TO lt_csdoc.

  ENDLOOP.

  IF lt_csdoc IS NOT INITIAL.

    CALL FUNCTION '/AGRI/GLCS_SAVE'
      EXPORTING
        i_set_update_task  = abap_true
        i_commit_work      = ' '
      CHANGING
        ct_csdoc           = lt_csdoc
        ct_messages        = lt_messages
      EXCEPTIONS
        no_change          = 1
        error_while_saving = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDIF.

ENDFUNCTION.
