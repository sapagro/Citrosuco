*----------------------------------------------------------------------*
***INCLUDE ZABS_REP_STORDENS_F01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form SELECTION_VALIDATIONS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM selection_validations .

ENDFORM.

*&---------------------------------------------------------------------*
*& Form REFRESH_GLOBAL_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM refresh_global_data .

  REFRESH: gt_orcamento,
           gt_message,
           gt_fieldcat,
           gt_outtab.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form BUILD_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM build_data .

  REFRESH gt_orcamento_db.

*--Preparing Budget's Data
  SELECT *
    FROM zabs_orcamento
    INTO TABLE @gt_orcamento_db
   WHERE acnum IN @s_acnum[].

  IF gt_orcamento_db[] IS INITIAL.
*--Não existem registros para os parâmetros informados!
    MESSAGE i209(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form VALIDATE_PARAMETERS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM validate_parameters .

  DATA: ls_variant TYPE disvariant.

  IF s_acnum[] IS INITIAL.
*--O parâmetro 'Área de Cultivo' é de preenchimento obrigatório!
    MESSAGE i270(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form COPY_BUDGET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM copy_budget_data .

*-- Elimina backup antigo dos dados de Orçamento para Área de Cultivo
  SELECT *
    FROM zabs_corcamento
    INTO TABLE @DATA(lt_corcamento_db)
   WHERE acnum IN @s_acnum[].
  IF sy-subrc EQ 0.
    DELETE zabs_corcamento FROM TABLE lt_corcamento_db.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

*-- Efetiva backup com dados atuais de Orçamento para Área de Cultivo
  IF gt_orcamento_db[] IS NOT INITIAL.
    MODIFY zabs_corcamento FROM TABLE gt_orcamento_db[].
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

*--Backup dos dados de Orçamento realizado com sucesso!
  MESSAGE i314(zfmfp).

ENDFORM.
