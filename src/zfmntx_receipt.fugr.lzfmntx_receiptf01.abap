*----------------------------------------------------------------------*
***INCLUDE LZFMNTX_RECEIPTF01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form DOCUMENT_INFOCUS_UNLOCK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> I_TICKET
*&      --> I_EXERCICIO
*&      --> IF
*&      --> FOUND
*&---------------------------------------------------------------------*
FORM document_infocus_lock USING lv_prnum TYPE /agri/fmprnum
                                 lv_gjahr TYPE gjahr
                        CHANGING lv_subrc TYPE sy-subrc.

  DATA: lv_msgv1_new TYPE sy-msgv1,
        lv_msgli_new TYPE sy-msgli.

  CALL FUNCTION 'ENQUEUE_/AGRI/EZ_FMPR'
    EXPORTING
      mode_/agri/fmprhdr = 'E'
      mandt              = sy-mandt
      prnum              = lv_prnum
      gjahr              = lv_gjahr
    EXCEPTIONS
      foreign_lock       = 1
      system_failure     = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
    lv_subrc = sy-subrc.
    lv_msgv1_new = sy-msgv1.
    MESSAGE e017(/agri/fmpr) WITH lv_prnum lv_msgv1_new INTO lv_msgli_new.
    sy-msgli = lv_msgli_new.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_INFOCUS_UNLOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM document_infocus_unlock  USING lv_prnum TYPE /agri/fmprnum
                                    lv_gjahr TYPE gjahr.

  CALL FUNCTION 'DEQUEUE_/AGRI/EZ_FMPR'
    EXPORTING
      mode_/agri/fmprhdr = 'E'
      mandt              = sy-mandt
      prnum              = lv_prnum
      gjahr              = lv_gjahr.

ENDFORM.                    " DOCUMENT_INFOCUS_UNLOCK
