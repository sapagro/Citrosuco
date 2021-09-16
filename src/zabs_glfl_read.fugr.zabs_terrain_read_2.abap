FUNCTION zabs_terrain_read_2.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_RANGE) TYPE  YS4_RANGE_STRNO_TAB
*"  EXPORTING
*"     VALUE(ET_DOC_MEDICAO) TYPE  ZABS_TTY_DOC_MEDICAO
*"----------------------------------------------------------------------

  DATA: lt_tplnr           TYPE /agri/t_gltplnr,
        lt_fl_doc          TYPE /agri/t_glfl_doc,
        lt_cskey           TYPE /agri/t_glcs_key,
        lt_amhdr           TYPE /agri/t_glamhdr,
        lt_ativos          TYPE /agri/t_glamhdr,
        lt_cshdr           TYPE STANDARD TABLE OF /agri/glflcma,
        lt_additional_data TYPE TABLE OF /agri/s_abgl_user_scrfields,
        lt_asset_summary   TYPE /agri/t_glam_summary_fcat,
        lwa_summary_list   TYPE /agri/s_glam_summary_fcat,
        lwa_table_field    TYPE tabfield,
        lr_atinn           TYPE RANGE OF atinn,
        lr_mdtyp           TYPE RANGE OF /agri/glmdtyp,
        lr_mpgrp           TYPE RANGE OF /agri/glmpgrp,
        lv_user_structure  TYPE ddobjname VALUE '/AGRI/S_GLFLOTCA',
        lv_terrain         TYPE /agri/gltplnr_fl,
        lv_fieldname       TYPE fnam_____4,
        lv_cultura         TYPE /agri/glcmnum VALUE 'CITROS',
        lv_tabix           TYPE sytabix,
        lv_msgid           TYPE symsgid,
        lv_msgty           TYPE symsgty,
        lv_msgno           TYPE symsgno,
        lv_msgv1           TYPE symsgv,
        lv_msgv2           TYPE symsgv,
        lv_msgv3           TYPE symsgv,
        lv_msgv4           TYPE symsgv,
        lv_int_value(128)  TYPE c.

*--Additional data
  FIELD-SYMBOLS: <lt_additional_data> TYPE table,
                 <lv_field_value>     TYPE any.

  CONSTANTS: BEGIN OF c_action,
               add    TYPE /agri/glivact VALUE 'A',
               remove TYPE /agri/glivact VALUE 'D',
               reset  TYPE /agri/glivact VALUE 'R',
             END OF c_action.

*  lv_terrain = iv_terrain.

*  CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
*    EXPORTING
*      input      = lv_terrain
*    IMPORTING
*      output     = lv_terrain
*    EXCEPTIONS
*      not_found  = 1
*      not_active = 2
*      OTHERS     = 3.
*
*  APPEND lv_terrain TO lt_tplnr.
*
*  CALL FUNCTION '/AGRI/GLFL_VIEW'
*    EXPORTING
*      it_tplnr       = lt_tplnr
*    IMPORTING
*      et_fldoc       = lt_fl_doc
*    EXCEPTIONS
*      no_data_exists = 1
*      OTHERS         = 2.

  IF i_range IS NOT INITIAL.

    SELECT atinn, atnam FROM cabn
      INTO TABLE @DATA(lt_cabn)
      WHERE atnam IN ('FAZ_VAR_MP_TECNICA', 'FAZ_FIM_MP_PLANTIO',
                       'CIT-TP-CONTAGEM','INV-QTDADE-ARV').
    IF sy-subrc = 0.
      lr_atinn = VALUE #( FOR ls_cabn IN lt_cabn ( sign = 'I'
                                                   option = 'EQ'
                                                   low = ls_cabn-atinn ) ).
    ENDIF.

*    et_terrain[] = lt_fl_doc[].
*    READ TABLE lt_fl_doc INTO DATA(lwa_terrain) INDEX 1.

      DO 4 TIMES.
        DATA(lv_index) = sy-index.
        IF lv_index LE 2.
*-- Tipo de documento de medição
          INSERT INITIAL LINE INTO TABLE lr_mdtyp
            ASSIGNING FIELD-SYMBOL(<lwa_mdtyp>).
          IF sy-subrc EQ 0.
            <lwa_mdtyp> = 'IEQ'.
            CASE lv_index.
              WHEN 1.
                <lwa_mdtyp>-low = 'ZARV'.
              WHEN 2.
                <lwa_mdtyp>-low = 'ZPTA'.
            ENDCASE.
          ENDIF.
        ENDIF.
*-- Grupo de medições
        INSERT INITIAL LINE INTO TABLE lr_mpgrp
          ASSIGNING FIELD-SYMBOL(<lwa_mpgrp>).
        IF sy-subrc EQ 0.
          <lwa_mpgrp> = 'IEQ'.
          CASE lv_index.
            WHEN 1.
              <lwa_mpgrp>-low = 'TEST_CS_UPDATE'.
            WHEN 2.
              <lwa_mpgrp>-low = 'FAZ-PLANTIO'.
            WHEN 3.
              <lwa_mpgrp>-low = 'CIT-INV-PLANTAS'.
            WHEN 4.
              <lwa_mpgrp>-low = 'FAZ-INV-PLANTAS'.
          ENDCASE.
        ENDIF.

      ENDDO.

      SELECT m~mdocm, m~mdtyp, m~aslvl, m~tplnr_fl,
             m~contr, m~cmnum, m~equnr, m~mpgrp,
             m~datab, m~datbi, m~mdate, g~strno,
             a~atinn, a~atzhl, a~atwrt, a~atflv,
             c~adzhl, t~spras, t~atwtb, t~lkenz
        FROM /agri/glmdhdr AS m
        INNER JOIN /agri/glmdatv AS a
          ON m~mdocm EQ a~mdocm
        INNER JOIN /agri/glflot AS g
          ON  g~tplnr_fl EQ m~tplnr_fl
        LEFT OUTER JOIN cawn AS c
          ON  c~atinn = a~atinn
          AND c~atwrt = a~atwrt
        LEFT OUTER JOIN cawnt AS t
          ON  c~atinn = t~atinn
          AND c~atzhl = t~atzhl
          AND c~adzhl = t~adzhl
        INTO TABLE @DATA(lt_glmdhdr)
       WHERE m~mdtyp    IN @lr_mdtyp[]
*         AND m~tplnr_fl EQ @lv_terrain
         AND g~strno    IN @i_range
         AND m~mpgrp    IN @lr_mpgrp[]
         AND a~atinn    IN @lr_atinn[].
*         AND t~spras  EQ @sy-langu

      SORT lt_glmdhdr BY mdtyp atinn mdate DESCENDING.
*      DELETE ADJACENT DUPLICATES FROM lt_glmdhdr COMPARING mdtyp atinn.
      DELETE lt_glmdhdr WHERE lkenz EQ abap_true.
      SORT lt_glmdhdr BY atinn.

      LOOP AT lt_glmdhdr INTO DATA(lwa_glmdhdr).
        INSERT INITIAL LINE INTO TABLE et_doc_medicao
          ASSIGNING FIELD-SYMBOL(<lwa_doc_medicao>).
        IF sy-subrc EQ 0.
          MOVE-CORRESPONDING lwa_glmdhdr TO <lwa_doc_medicao>.
        ENDIF.
      ENDLOOP.

  ENDIF.

ENDFUNCTION.
