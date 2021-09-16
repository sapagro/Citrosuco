FUNCTION zabs_fm_shp_insumo.
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
           matnr TYPE zfmrcmatnr,
           maktx TYPE maktx,
         END OF ly_out.

  DATA: lt_out   TYPE STANDARD TABLE OF ly_out INITIAL SIZE 0,
        lt_lista TYPE STANDARD TABLE OF zfmrclst INITIAL SIZE 0,
        lwa_out  LIKE LINE OF lt_out.
*        lv_rcnum TYPE zfmrcnum VALUE '1200000034'.

  FIELD-SYMBOLS: <lt_rclst> TYPE zt_fmrclst,
                 <lv_rcnum> TYPE zfmrcnum.

  IF callcontrol-step <> 'SELECT'
  AND callcontrol-step <> 'DISP'.
    EXIT.
  ENDIF.

  ASSIGN: ('(SAPLZFMRCM)GS_RCDOC_INFOCUS-RCNUM') TO <lv_rcnum>,
          ('(SAPLZFMRCM)GS_RCDOC_INFOCUS-X-RCLST[]') TO <lt_rclst>.

  IF <lt_rclst> IS ASSIGNED.
    lt_lista[] = CORRESPONDING #( <lt_rclst>[] ).
  ENDIF.

  IF <lv_rcnum> IS ASSIGNED.
    IF callcontrol-step = 'SELECT'.
      SELECT *
        FROM zfmrclst
        APPENDING TABLE @lt_lista
       WHERE rcnum = @<lv_rcnum>.

      IF sy-subrc EQ 0.
        SORT lt_lista BY rcnum matnr_ins.
        DELETE ADJACENT DUPLICATES FROM lt_lista COMPARING rcnum matnr_ins.

        SELECT matnr, spras, maktx
          FROM makt
          INTO TABLE @DATA(lt_makt)
          FOR ALL ENTRIES IN @lt_lista
         WHERE matnr = @lt_lista-matnr_ins
           AND spras = @sy-langu.

        SORT lt_makt BY matnr.
      ENDIF.

      LOOP AT lt_lista INTO DATA(lwa_lista).
        lwa_out-matnr  = lwa_lista-matnr_ins.

        READ TABLE lt_makt INTO DATA(lwa_makt)
          WITH KEY matnr = lwa_lista-matnr_ins BINARY SEARCH.
        IF sy-subrc EQ 0.
          lwa_out-maktx  = lwa_makt-maktx.
        ENDIF.

        APPEND lwa_out TO lt_out.
        record_tab-string = lwa_out.
        SHIFT record_tab-string RIGHT DELETING TRAILING space.
        APPEND record_tab.
      ENDLOOP.
    ELSEIF callcontrol-step EQ 'DISP'.
      SELECT *
        FROM zfmrclst
        APPENDING TABLE @lt_lista
       WHERE rcnum = @<lv_rcnum>.

      IF sy-subrc EQ 0.
        SORT lt_lista BY rcnum matnr_ins.
        DELETE ADJACENT DUPLICATES FROM lt_lista COMPARING rcnum matnr_ins.

        SELECT matnr, spras, maktx
          FROM makt
          INTO TABLE @lt_makt
          FOR ALL ENTRIES IN @lt_lista
         WHERE matnr = @lt_lista-matnr_ins
           AND spras = @sy-langu.

        SORT lt_makt BY matnr.
      ENDIF.

      LOOP AT lt_lista INTO lwa_lista.
        lwa_out-matnr  = lwa_lista-matnr_ins.

        READ TABLE lt_makt INTO lwa_makt
          WITH KEY matnr = lwa_lista-matnr_ins BINARY SEARCH.
        IF sy-subrc EQ 0.
          lwa_out-maktx  = lwa_makt-maktx.
        ENDIF.

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
    ENDIF.
  ENDIF.

ENDFUNCTION.
