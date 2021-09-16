*----------------------------------------------------------------------*
***INCLUDE LZABS_FG_ACMF01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form DOCUMENT_INFOCUS_LOCK
*&---------------------------------------------------------------------*
*& DOCUMENT_INFOCUS_LOCK
*&---------------------------------------------------------------------*
FORM document_infocus_lock  USING    pv_accom TYPE /agri/fmaccom
                            CHANGING cv_subrc
                                     cs_messages TYPE /agri/s_gprolog.

  DATA: lv_msgv1 LIKE sy-msgv1,
        lv_msgli TYPE sy-msgli.

  CHECK NOT pv_accom IS INITIAL.

  CALL FUNCTION 'ENQUEUE_/AGRI/EZ_FMAC'
    EXPORTING
      accom          = pv_accom
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
    cv_subrc = sy-subrc.
    lv_msgv1 = sy-msgv1.
****Document &1 is locked by User &2.
*    SET CURSOR FIELD 'P_ACCOM'.
*    MESSAGE i185(zabs_msgcls) WITH lv_accom lv_msgv1 INTO lv_msgli.
*    sy-msgli = lv_msgli.
*    message_simple space.
    cs_messages-msgty = zcl_abs_abap_maintain=>c_msgty_error.
    cs_messages-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
    cs_messages-msgno = '185'.
    cs_messages-msgv1 = pv_accom.
    cs_messages-msgv2 = lv_msgv1.

  ENDIF.

ENDFORM.                    " DOCUMENT_INFOCUS_LOCK
