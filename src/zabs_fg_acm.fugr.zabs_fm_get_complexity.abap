FUNCTION zabs_fm_get_complexity.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IT_TPLNR) TYPE  /AGRI/T_GLTPLNR
*"  EXPORTING
*"     REFERENCE(ET_COMPLEX) TYPE  ZABS_T_TERRAIN_COMPLEX
*"----------------------------------------------------------------------

*-- Local Declarations
  DATA: ls_athdr   TYPE /agri/s_gathdr,
        lv_atwrt   TYPE atwrt,
        ls_complex TYPE zabs_s_terrain_complex,
        lv_mdtyp   TYPE /agri/glmdtyp,
        lv_atgrp   TYPE /agri/glmpgrp,
        lv_atnam   TYPE atnam.

*-- Fetch the measurement document type, attribute group, attribute name
*-- to get the terrain complexity
  CALL METHOD zcl_abs_get_variants=>get_constant_single
    EXPORTING
      iv_objid  = zcl_abs_abap_maintain=>c_objid_accomplish "'ACCM'
      iv_k1val  = zcl_abs_abap_maintain=>c_key_complex "'COMP'
    IMPORTING
      ev_cnval1 = lv_mdtyp
      ev_cnval2 = lv_atgrp
      ev_cnval3 = lv_atnam.

*-- Fetch the Internal Characteristic value
  SELECT SINGLE atinn
    FROM cabn
    INTO @DATA(lv_atinn)
   WHERE atnam EQ @lv_atnam. "'FAZ_COMPLEXIDADE'.
  IF sy-subrc EQ 0.
*-- Fetch the attribute header details
    SELECT SINGLE *
      FROM /agri/gatha AS a INNER JOIN cabn AS b
        ON b~atinn EQ a~atinn
      INTO CORRESPONDING FIELDS OF ls_athdr
     WHERE a~atinn EQ lv_atinn.
    IF sy-subrc EQ 0.
*-- Fetch the measurement documents with complexity attribute values
      SELECT a~mdocm, a~tplnr_fl, a~mdate, a~mtime, b~atzhl, b~atflv
        INTO TABLE @DATA(lt_mdatv)
        FROM /agri/glmdhdr AS a INNER JOIN /agri/glmdatv AS b
          ON b~mdocm    EQ a~mdocm
         FOR ALL ENTRIES IN @it_tplnr
       WHERE a~mdtyp    EQ @lv_mdtyp "'ZTYP'
         AND a~aslvl    EQ @zcl_abs_abap_maintain=>c_msrmnt_level_terrain
         AND a~tplnr_fl EQ @it_tplnr-tplnr_fl
         AND a~mpgrp    EQ @lv_atgrp "'FAZ_COMPLEXIDADE'
         and a~kfrst    EQ @space
         AND b~atinn    EQ @lv_atinn.
      IF sy-subrc EQ 0.
        SORT lt_mdatv BY tplnr_fl mdate DESCENDING mtime DESCENDING.
        LOOP AT it_tplnr ASSIGNING FIELD-SYMBOL(<fs_tplnr>).
          CLEAR ls_complex.
          ls_complex-tplnr = <fs_tplnr>-tplnr_fl.

          READ TABLE lt_mdatv INTO DATA(ls_mdatv)
                WITH KEY tplnr_fl = ls_complex-tplnr
              BINARY SEARCH.
          IF sy-subrc EQ 0.
            CLEAR lv_atwrt.
            CALL METHOD /agri/cl_gattr_utils=>attr_value_for_display_prepare
              EXPORTING
                i_agtyp  = 'X90'
                is_athdr = ls_athdr
              CHANGING
                c_atwrt  = lv_atwrt
                c_atflv  = ls_mdatv-atflv.

*-- Populate the latest complex attribute value
            ls_complex-complex = lv_atwrt.
            APPEND ls_complex TO et_complex.
          ENDIF.
        ENDLOOP.
        SORT et_complex BY tplnr.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFUNCTION.
