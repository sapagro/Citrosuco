FUNCTION zabs_fm_check_formula.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_ATNAM) TYPE  ATNAM
*"     REFERENCE(IV_FORMULA) TYPE  ZABS_DEL_FORMULA
*"     REFERENCE(IT_ATTRIBUTES) TYPE  ZABS_TTY_ATTR_FORML
*"  EXPORTING
*"     REFERENCE(EV_SUBRC) TYPE  SY-SUBRC
*"     REFERENCE(EV_MESSAGE) TYPE  CHAR80
*"----------------------------------------------------------------------
************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Enhancement Name  :  ZABS_IENH_NURSERY_PROCESS                       *
* FM Name           :  ZABS_FM_CHECK_FORMULA                           *
* Created By        :  Jetendra Mantena                                *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  07.04.2019                                      *
* TR                :  C4DK901784                                      *
* Version           :  001                                             *
* Description       :  Validating Formula Field                        *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

  gt_attributes = it_attributes.
  SORT gt_attributes BY ident.

  CALL FUNCTION 'CHECK_FORMULA'
    EXPORTING
      formula           = iv_formula
      program           = sy-repid
      routine           = 'GET_CHECK_VALUES'
    IMPORTING
      message           = ev_message
    EXCEPTIONS
      error_in_formula  = 1
      missing_parameter = 2
      OTHERS            = 3.

  ev_subrc = sy-subrc.

  IF gs_variables-subrc IS NOT INITIAL
    AND ev_subrc IS NOT INITIAL.
    CLEAR ev_message.
    CONCATENATE gs_variables-ident TEXT-001 iv_atnam INTO ev_message SEPARATED BY space.
  ELSEIF ev_subrc IS NOT INITIAL.
    CONCATENATE TEXT-002 iv_atnam INTO ev_message SEPARATED BY space.
  ENDIF.

  REFRESH gt_attributes.

ENDFUNCTION.
