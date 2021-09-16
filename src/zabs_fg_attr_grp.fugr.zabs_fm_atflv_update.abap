*FUNCTION zabs_fm_atflv_update.
**"----------------------------------------------------------------------
**"*"Local Interface:
**"  IMPORTING
**"     REFERENCE(IS_MDHDR) TYPE  /AGRI/S_GLMDHDR
**"     REFERENCE(IT_MDITM) TYPE  /AGRI/T_GLMDITM
**"     REFERENCE(IT_MDATV) TYPE  /AGRI/T_GLMDATV
**"----------------------------------------------------------------------
*************************************************************************
**  Confidential property of Citrosuco                                  *
**  All Rights Reserved                                                 *
*************************************************************************
** FM Name           :  ZABS_FM_SHP_EXIT_ATTR                           *
** Created By        :  Jetendra Mantena                                *
** Requested by      :  Mario Alfredo                                   *
** Created on        :  09.11.2019                                      *
** TR                :  C4DK901784                                      *
** Version           :  001                                             *
** Description       :  The attribute values from measurement document  *
**                      workbench gets updated in the terrain workbench *
**                      based on custom table.                          *
**----------------------------------------------------------------------*
**  Modification Log:                                                   *
**----------------------------------------------------------------------*
** MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
**                                                                      *
**&--------------------------------------------------------------------&*
*
**--Internal table declarations
*  DATA: lt_tplnr     TYPE /agri/t_gltplnr,
*        lt_fldoc     TYPE /agri/t_glfl_doc,
*        lt_clint     TYPE /agri/t_gclint,
*        lt_atnam     TYPE /agri/t_gatnam,
*        lt_trclass   TYPE /agri/t_glatgrp,
*        lt_messages  TYPE /agri/t_gprolog,
*
**--Workarea declarations
*        lwa_atnam    TYPE /agri/s_gatnam,
*        lwa_clint    TYPE /agri/s_gclint,
*        lwa_attr_val TYPE auspdata,
*        lwa_flatv    TYPE /agri/s_glflatv.
*
**--Fetching custom table data
*  SELECT *
*    FROM zabst_md_atr_map
*    INTO TABLE @DATA(lt_md_atr_map)
*    WHERE mdclass = @is_mdhdr-mpgrp.
*  IF sy-subrc <> 0 OR lt_md_atr_map IS INITIAL.
*    RETURN.
*  ELSE.
*    SORT lt_md_atr_map.
*  ENDIF.
*
**--Processing above collected data to fill the internal table
*  REFRESH lt_atnam.
*  LOOP AT lt_md_atr_map INTO DATA(lwa_md_atr_map).
*    APPEND lwa_md_atr_map-mdatnam TO lt_atnam.
*    APPEND lwa_md_atr_map-tratnam TO lt_atnam.
*    COLLECT lwa_md_atr_map-trclass INTO lt_trclass.
*  ENDLOOP.
*  DELETE ADJACENT DUPLICATES FROM lt_atnam COMPARING atnam.
*
**--Fetching characteristic data
*  SELECT atinn,
*         atnam
*    FROM cabn
*    INTO TABLE @DATA(lt_cabn)
*    FOR ALL ENTRIES IN @lt_atnam
*    WHERE atnam EQ @lt_atnam-atnam.
*  IF sy-subrc <> 0.
*    RETURN.
*  ELSE.
*    SORT lt_cabn BY atnam.
*  ENDIF.
*
**--Getting terrain view
*  APPEND is_mdhdr-tplnr_fl TO lt_tplnr.
*  CALL FUNCTION '/AGRI/GLFL_VIEW'
*    EXPORTING
*      it_tplnr       = lt_tplnr
*    IMPORTING
*      et_fldoc       = lt_fldoc
*    EXCEPTIONS
*      no_data_exists = 1
*      OTHERS         = 2.
*  IF sy-subrc <> 0.
** Implement suitable error handling here
*  ENDIF.
*
*  READ TABLE lt_fldoc ASSIGNING FIELD-SYMBOL(<fs_fldoc>) INDEX 1.
*  IF sy-subrc <> 0.
*    RETURN.
*  ENDIF.
*
*  LOOP AT lt_trclass INTO DATA(ls_trclass).
*    READ TABLE <fs_fldoc>-x-flatg INTO DATA(lwa_flatg)
*    WITH KEY class = ls_trclass.
*    IF sy-subrc = 0.
*      lwa_clint-clint = lwa_flatg-clint.
*      APPEND lwa_clint TO lt_clint.
*    ENDIF.
*  ENDLOOP.
*
*  CHECK lt_clint IS NOT INITIAL.
*
**--Fetching class header and characteristic data
*  SELECT a~clint,
*         a~class,
*         b~imerk
*    FROM klah AS a
*    JOIN ksml AS b
*    ON   b~clint = a~clint
*    INTO TABLE @DATA(lt_klah_ksml)
*    FOR ALL ENTRIES IN @lt_clint
*    WHERE a~clint EQ @lt_clint-clint.
*  IF sy-subrc <> 0.
*    RETURN.
*  ELSE.
*    SORT lt_klah_ksml BY imerk.
*  ENDIF.
*
**--Processing data to update field values in crop season
*  LOOP AT lt_md_atr_map INTO lwa_md_atr_map.
*
*    READ TABLE lt_cabn INTO DATA(lwa_cabn)
*    WITH KEY atnam = lwa_md_atr_map-mdatnam
*    BINARY SEARCH.
*    IF sy-subrc <> 0.
*      CONTINUE.
*    ENDIF.
*
*    READ TABLE it_mdatv INTO DATA(lwa_mdatv)
*    WITH  KEY atinn = lwa_cabn-atinn.
*    IF sy-subrc <> 0.
*      CONTINUE.
*    ELSE.
*      CLEAR lwa_attr_val.
*      MOVE-CORRESPONDING lwa_mdatv TO lwa_attr_val.
*    ENDIF.
*
*    CLEAR lwa_cabn.
*    READ TABLE lt_cabn INTO lwa_cabn
*    WITH KEY atnam = lwa_md_atr_map-tratnam
*    BINARY SEARCH.
*    IF sy-subrc <> 0.
*      CONTINUE.
*    ENDIF.
*
*    READ TABLE lt_klah_ksml INTO DATA(lwa_klah_ksml)
*    WITH  KEY imerk = lwa_cabn-atinn
*    BINARY SEARCH.
*    IF sy-subrc <> 0.
*      CONTINUE.
*    ENDIF.
*
*    READ TABLE <fs_fldoc>-x-flatv ASSIGNING FIELD-SYMBOL(<fs_flatv>)
*    WITH KEY clint = lwa_klah_ksml-clint
*             atinn = lwa_klah_ksml-imerk.
*    IF sy-subrc = 0.
*      <fs_flatv>-updkz = zcl_abs_abap_maintain=>c_updkz_update. "'U'.
*      MOVE-CORRESPONDING lwa_attr_val TO <fs_flatv>.
*    ELSE.
*      lwa_flatv-updkz = zcl_abs_abap_maintain=>c_updkz_insert. "'I'
*      lwa_flatv-tplnr_fl = <fs_fldoc>-tplnr_fl.
*      lwa_flatv-class    = lwa_klah_ksml-class.
*      lwa_flatv-clint    = lwa_klah_ksml-clint.
*      lwa_flatv-atinn    = lwa_klah_ksml-imerk.
*      MOVE-CORRESPONDING lwa_attr_val TO lwa_flatv.
*      APPEND lwa_flatv TO <fs_fldoc>-x-flatv.
*    ENDIF.
*    DATA(lv_update) = abap_true.
*  ENDLOOP.
*
*  IF lv_update IS NOT INITIAL.
*    CALL FUNCTION '/AGRI/GLFL_SAVE_SINGLE'
*      EXPORTING
*        i_set_update_task  = abap_true
*        i_commit_work      = space
*      CHANGING
*        cs_fldoc           = <fs_fldoc>
*        ct_messages        = lt_messages
*      EXCEPTIONS
*        no_change          = 1
*        error_while_saving = 2
*        OTHERS             = 3.
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*    ENDIF.
*  ENDIF.
*
*ENDFUNCTION.
