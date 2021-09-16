*&---------------------------------------------------------------------*
*& Include          ZLFMRCMMCR
*&---------------------------------------------------------------------*
DEFINE material_covert.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input              = &1
   IMPORTING
     output              = &2
   EXCEPTIONS
     length_error       = 1
     OTHERS             = 2.
  IF sy-subrc <> 0
   AND NOT sy-msgid IS INITIAL
   AND NOT sy-msgty IS INITIAL
   AND NOT sy-msgno IS INITIAL.
     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
   ENDIF.
END-OF-DEFINITION.

DEFINE date_formt_ddmmyyyy.

  CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
    EXPORTING
      input         = &1
   IMPORTING
     output         = &2.

END-OF-DEFINITION.
