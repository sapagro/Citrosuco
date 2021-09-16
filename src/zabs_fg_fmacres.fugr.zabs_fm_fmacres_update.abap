FUNCTION zabs_fm_fmacres_update.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IT_FMACRES_INSERT) TYPE  /AGRI/T_FMFMACRES OPTIONAL
*"     VALUE(IT_FMACRES_UPDATE) TYPE  /AGRI/T_FMFMACRES OPTIONAL
*"     VALUE(IT_FMACRES_DELETE) TYPE  /AGRI/T_FMFMACRES OPTIONAL
*"  EXPORTING
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_TAB
*"  EXCEPTIONS
*"      NO_ENTRIES
*"----------------------------------------------------------------------

  DATA: lt_fmacres_db TYPE STANDARD TABLE OF /agri/fmacres INITIAL SIZE 0,
        lt_fmacres    TYPE STANDARD TABLE OF /agri/fmacres INITIAL SIZE 0,
        lt_insert     TYPE STANDARD TABLE OF /agri/fmacres INITIAL SIZE 0,
        lt_update     TYPE STANDARD TABLE OF /agri/fmacres INITIAL SIZE 0,
        lt_delete     TYPE STANDARD TABLE OF /agri/fmacres INITIAL SIZE 0,
        lv_fieldname  TYPE fieldname,
        dummy.

  APPEND LINES OF it_fmacres_insert TO lt_fmacres.
  APPEND LINES OF it_fmacres_update TO lt_fmacres.
  APPEND LINES OF it_fmacres_delete TO lt_fmacres.

  IF lt_fmacres[] IS NOT INITIAL.
    SELECT *
      FROM /agri/fmacres
      INTO TABLE @lt_fmacres_db
      FOR ALL ENTRIES IN @lt_fmacres
     WHERE idresource EQ @lt_fmacres-idresource
       AND arbpl      EQ @lt_fmacres-arbpl.

    SORT lt_fmacres_db BY idresource arbpl.
  ELSE.
*-- Nenhum parâmetro informado.
    INSERT INITIAL LINE INTO TABLE et_return
      ASSIGNING FIELD-SYMBOL(<ls_return>).
    IF sy-subrc EQ 0.
      <ls_return>-type = 'E'.
      <ls_return>-id = 'ZFMFP'.
      <ls_return>-number = 283.
      MESSAGE e283(zfmfp) INTO dummy.
      MESSAGE e283(zfmfp) INTO sy-msgli.
      <ls_return>-message = sy-msgli.
    ENDIF.
    RAISE no_entries.
  ENDIF.

  IF it_fmacres_insert[] IS SUPPLIED.
    LOOP AT it_fmacres_insert INTO DATA(ls_fmacres).
      DATA(lv_row) = sy-tabix.
      IF ls_fmacres-idresource IS INITIAL
      OR ls_fmacres-arbpl IS INITIAL
      OR ls_fmacres-description IS INITIAL
      OR ls_fmacres-rstype IS INITIAL
      OR ls_fmacres-equnr IS INITIAL.
        IF ls_fmacres-idresource IS INITIAL.
          lv_fieldname = 'IDRESOURCE'.
        ELSEIF ls_fmacres-arbpl IS INITIAL.
          lv_fieldname = 'ARBPL'.
        ELSEIF ls_fmacres-description IS INITIAL.
          lv_fieldname = 'DESCRIPTION'.
        ELSEIF ls_fmacres-rstype IS INITIAL.
          lv_fieldname = 'RSTYPE'.
        ELSEIF ls_fmacres-equnr IS INITIAL.
          lv_fieldname = 'EQUNR'.
        ENDIF.
*-- Inserção Linha &1: verificar campo obrigatório &2.
        INSERT INITIAL LINE INTO TABLE et_return
          ASSIGNING <ls_return>.
        IF sy-subrc EQ 0.
          <ls_return>-type = 'E'.
          <ls_return>-id = 'ZFMFP'.
          <ls_return>-number = 286.
          <ls_return>-message_v1 = lv_row.
          <ls_return>-message_v2 = lv_fieldname.
          MESSAGE e286(zfmfp) WITH lv_row INTO dummy.
          MESSAGE e286(zfmfp) INTO sy-msgli.
          <ls_return>-message = sy-msgli.
        ENDIF.
      ELSE.
*-- Check same key
        READ TABLE lt_fmacres_db INTO DATA(ls_fmacres_db)
          WITH KEY idresource = ls_fmacres-idresource
                   arbpl      = ls_fmacres-arbpl BINARY SEARCH.
        IF sy-subrc EQ 0.
*-- Inserção Linha &1: já existe entrada de mesma chave.
          INSERT INITIAL LINE INTO TABLE et_return
            ASSIGNING <ls_return>.
          IF sy-subrc EQ 0.
            <ls_return>-type = 'E'.
            <ls_return>-id = 'ZFMFP'.
            <ls_return>-number = 281.
            <ls_return>-message_v1 = lv_row.
            MESSAGE e281(zfmfp) WITH lv_row INTO dummy.
            MESSAGE e281(zfmfp) INTO sy-msgli.
            <ls_return>-message = sy-msgli.
          ENDIF.
        ELSE.
          APPEND ls_fmacres TO lt_insert.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF NOT lt_insert[] IS INITIAL.
      INSERT /agri/fmacres FROM TABLE lt_insert.
      IF sy-dbcnt IS NOT INITIAL.
*-- &1 linhas foram inseridas com sucesso.
        INSERT INITIAL LINE INTO TABLE et_return
          ASSIGNING <ls_return>.
        IF sy-subrc EQ 0.
          <ls_return>-type = 'S'.
          <ls_return>-id = 'ZFMFP'.
          <ls_return>-number = 287.
          <ls_return>-message_v1 = sy-dbcnt.
          MESSAGE e287(zfmfp) WITH sy-dbcnt INTO dummy.
          MESSAGE e287(zfmfp) WITH sy-dbcnt INTO sy-msgli.
          <ls_return>-message = sy-msgli.
        ENDIF.
      ENDIF.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

  IF it_fmacres_update[] IS SUPPLIED.
    LOOP AT it_fmacres_update INTO ls_fmacres.
      lv_row = sy-tabix.
*-- Check same key
      READ TABLE lt_fmacres_db INTO ls_fmacres_db
        WITH KEY idresource = ls_fmacres-idresource
                 arbpl      = ls_fmacres-arbpl BINARY SEARCH.
      IF sy-subrc NE 0.
*-- Atualização Linha &1: entrada inexistente.
        INSERT INITIAL LINE INTO TABLE et_return
          ASSIGNING <ls_return>.
        IF sy-subrc EQ 0.
          <ls_return>-type = 'E'.
          <ls_return>-id = 'ZFMFP'.
          <ls_return>-number = 282.
          <ls_return>-message_v1 = lv_row.
          MESSAGE e282(zfmfp) WITH lv_row INTO dummy.
          MESSAGE e282(zfmfp) WITH lv_row INTO sy-msgli.
          <ls_return>-message = sy-msgli.
        ENDIF.
      ELSE.
        APPEND ls_fmacres TO lt_update.
      ENDIF.
    ENDLOOP.

    IF NOT lt_update[] IS INITIAL.
      UPDATE /agri/fmacres FROM TABLE lt_update.
      IF sy-dbcnt IS NOT INITIAL.
*-- &1 linhas foram modificadas com sucesso.
        INSERT INITIAL LINE INTO TABLE et_return
          ASSIGNING <ls_return>.
        IF sy-subrc EQ 0.
          <ls_return>-type = 'S'.
          <ls_return>-id = 'ZFMFP'.
          <ls_return>-number = 288.
          <ls_return>-message_v1 = sy-dbcnt.
          MESSAGE e288(zfmfp) WITH sy-dbcnt INTO dummy.
          MESSAGE e288(zfmfp) WITH sy-dbcnt INTO sy-msgli.
          <ls_return>-message = sy-msgli.
        ENDIF.
      ENDIF.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

  IF it_fmacres_delete[] IS SUPPLIED.
    LOOP AT it_fmacres_delete INTO ls_fmacres.
      lv_row = sy-tabix.
      IF ls_fmacres-idresource IS INITIAL.
*-- Eliminação Linha &1: campo IDRESOURCE deve ser informado.
        INSERT INITIAL LINE INTO TABLE et_return
          ASSIGNING <ls_return>.
        IF sy-subrc EQ 0.
          <ls_return>-type = 'E'.
          <ls_return>-id = 'ZFMFP'.
          <ls_return>-number = 285.
          <ls_return>-message_v1 = lv_row.
          MESSAGE e285(zfmfp) WITH lv_row INTO dummy.
          MESSAGE e285(zfmfp) WITH lv_row INTO sy-msgli.
          <ls_return>-message = sy-msgli.
        ENDIF.
      ELSE.
*-- Check if entry exists
        READ TABLE lt_fmacres_db INTO ls_fmacres_db
          WITH KEY idresource = ls_fmacres-idresource BINARY SEARCH.
        IF sy-subrc NE 0.
*-- Eliminação Linha &1: entrada inexistente.
          INSERT INITIAL LINE INTO TABLE et_return
            ASSIGNING <ls_return>.
          IF sy-subrc EQ 0.
            <ls_return>-type = 'E'.
            <ls_return>-id = 'ZFMFP'.
            <ls_return>-number = 284.
            <ls_return>-message_v1 = lv_row.
            MESSAGE e284(zfmfp) WITH lv_row INTO dummy.
            MESSAGE e284(zfmfp) WITH lv_row INTO sy-msgli.
            <ls_return>-message = sy-msgli.
          ENDIF.
        ELSE.
          APPEND ls_fmacres TO lt_delete.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF NOT lt_delete[] IS INITIAL.
      SELECT *
        FROM /agri/fmacres
        INTO TABLE @lt_delete
        FOR ALL ENTRIES IN @lt_delete
       WHERE idresource = @lt_delete-idresource.
      IF sy-subrc EQ 0.
        DELETE /agri/fmacres FROM TABLE lt_delete.
        IF sy-dbcnt IS NOT INITIAL.
*-- &1 linhas foram eliminadas com sucesso.
          INSERT INITIAL LINE INTO TABLE et_return
            ASSIGNING <ls_return>.
          IF sy-subrc EQ 0.
            <ls_return>-type = 'S'.
            <ls_return>-id = 'ZFMFP'.
            <ls_return>-number = 289.
            <ls_return>-message_v1 = sy-dbcnt.
            MESSAGE e289(zfmfp) WITH sy-dbcnt INTO dummy.
            MESSAGE e289(zfmfp) WITH sy-dbcnt INTO sy-msgli.
            <ls_return>-message = sy-msgli.
          ENDIF.
        ENDIF.
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFUNCTION.
