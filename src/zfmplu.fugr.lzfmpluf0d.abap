*----------------------------------------------------------------------*
***INCLUDE /AGRI/LGLFLUF0D .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  DB_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM db_update  TABLES  lt_insert
                        lt_update
                        lt_delete
               USING    lv_tablename.

  IF NOT lt_delete[] IS INITIAL.
    DELETE (lv_tablename) FROM TABLE lt_delete. "#EC CI_DYNTAB
  ENDIF.

****Insert
  IF NOT lt_insert[] IS INITIAL.
    INSERT (lv_tablename) FROM TABLE lt_insert. "#EC CI_DYNTAB
  ENDIF.

****Update
  IF NOT lt_update[] IS INITIAL.
    UPDATE (lv_tablename) FROM TABLE lt_update. "#EC CI_DYNTAB
  ENDIF.

ENDFORM.                    " DB_UPDATE
