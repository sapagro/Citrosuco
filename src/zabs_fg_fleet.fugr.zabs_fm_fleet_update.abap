FUNCTION zabs_fm_fleet_update.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IT_FLEET_INSERT) TYPE  ZABS_TTY_FLEET OPTIONAL
*"     VALUE(IT_FLEET_DELETE) TYPE  ZABS_TTY_FLEET OPTIONAL
*"  EXPORTING
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_TAB
*"  EXCEPTIONS
*"      NO_ENTRIES
*"----------------------------------------------------------------------

  DATA: lt_fleet_db  TYPE STANDARD TABLE OF zabst_fleet INITIAL SIZE 0,
        lt_fleet     TYPE STANDARD TABLE OF zabst_fleet INITIAL SIZE 0,
        lt_insert    TYPE STANDARD TABLE OF zabst_fleet INITIAL SIZE 0,
        lt_update    TYPE STANDARD TABLE OF zabst_fleet INITIAL SIZE 0,
        lt_delete    TYPE STANDARD TABLE OF zabst_fleet INITIAL SIZE 0,
        lv_fieldname TYPE fieldname,
        dummy.

  APPEND LINES OF it_fleet_insert TO lt_fleet.
  APPEND LINES OF it_fleet_delete TO lt_fleet.

  IF lt_fleet[] IS NOT INITIAL.
    SELECT *
      FROM zabst_fleet
      INTO TABLE @lt_fleet_db
      FOR ALL ENTRIES IN @lt_fleet
     WHERE fleet   EQ @lt_fleet-fleet
       AND werks   EQ @lt_fleet-werks
       AND tsk_grp EQ @lt_fleet-tsk_grp.

    SORT lt_fleet_db BY fleet werks tsk_grp.
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

  IF it_fleet_insert[] IS SUPPLIED.
    LOOP AT it_fleet_insert INTO DATA(ls_fleet).
      DATA(lv_row) = sy-tabix.
      IF ls_fleet-fleet IS INITIAL
      OR ls_fleet-werks IS INITIAL
      OR ls_fleet-tsk_grp IS INITIAL.
        IF ls_fleet-fleet IS INITIAL.
          lv_fieldname = 'FLEET'.
        ELSEIF ls_fleet-werks IS INITIAL.
          lv_fieldname = 'WERKS'.
        ELSEIF ls_fleet-tsk_grp IS INITIAL.
          lv_fieldname = 'TSK_GRP'.
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
        READ TABLE lt_fleet_db INTO DATA(ls_fleet_db)
          WITH KEY fleet   = ls_fleet-fleet
                   werks   = ls_fleet-werks
                   tsk_grp = ls_fleet-tsk_grp BINARY SEARCH.
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
            MESSAGE e281(zfmfp) WITH lv_row INTO sy-msgli.
            <ls_return>-message = sy-msgli.
          ENDIF.
        ELSE.
          APPEND ls_fleet TO lt_insert.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF NOT lt_insert[] IS INITIAL.
      INSERT zabst_fleet FROM TABLE lt_insert.
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

  IF it_fleet_delete[] IS SUPPLIED.
    LOOP AT it_fleet_delete INTO ls_fleet.
      lv_row = sy-tabix.
*-- Check same key
      READ TABLE lt_fleet_db INTO ls_fleet_db
        WITH KEY fleet   = ls_fleet-fleet
                 werks   = ls_fleet-werks
                 tsk_grp = ls_fleet-tsk_grp BINARY SEARCH.
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
        APPEND ls_fleet TO lt_delete.
      ENDIF.
    ENDLOOP.

    IF NOT lt_delete[] IS INITIAL.
      DELETE zabst_fleet FROM TABLE lt_delete.
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

ENDFUNCTION.
