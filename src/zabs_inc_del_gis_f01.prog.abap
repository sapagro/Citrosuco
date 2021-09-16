*&---------------------------------------------------------------------*
*& Include ZABS_INC_DEL_GIS_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form CHANGE_SCREEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM change_screen .

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SELECT_GUID
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM select_guid .

  SELECT *
    FROM zabs_map_guid
    INTO TABLE @gt_guid
   WHERE data LT @p_date.
  IF sy-subrc NE 0.
*-- Não existem registros para a data informada!
    MESSAGE i422(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DELETE_GUID
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM delete_guid .

  IF gt_guid[] IS NOT INITIAL.
    DELETE zabs_map_guid FROM TABLE gt_guid.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
*-- &1 registros eliminados com sucesso!
      MESSAGE i423(zfmfp) WITH sy-dbcnt.
      LEAVE LIST-PROCESSING.
    ELSE.
      ROLLBACK WORK.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

ENDFORM.
