FUNCTION zfmfp_inspoper_recordresults.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(INSPLOT) LIKE  BAPI2045L2-INSPLOT
*"     VALUE(INSPOPER) LIKE  BAPI2045L2-INSPOPER
*"     VALUE(INSPPOINTDATA) LIKE  BAPI2045L4 STRUCTURE  BAPI2045L4
*"       OPTIONAL
*"     VALUE(HANDHELD_APPLICATION) LIKE
*"        BAPI2045L5-HANDHELD_APPLICATION OPTIONAL
*"  EXPORTING
*"     VALUE(RETURN) LIKE  BAPIRET2 STRUCTURE  BAPIRET2
*"  TABLES
*"      CHAR_RESULTS STRUCTURE  BAPI2045D2 OPTIONAL
*"      SAMPLE_RESULTS STRUCTURE  BAPI2045D3 OPTIONAL
*"      SINGLE_RESULTS STRUCTURE  BAPI2045D4 OPTIONAL
*"      RETURNTABLE STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------

  CALL FUNCTION 'BAPI_INSPOPER_RECORDRESULTS'
    EXPORTING
      insplot              = insplot
      inspoper             = inspoper
      insppointdata        = insppointdata
      handheld_application = handheld_application
    IMPORTING
      return               = return
    TABLES
      char_results         = char_results
      sample_results       = sample_results
      single_results       = single_results
      returntable          = returntable.

  READ TABLE returntable TRANSPORTING NO FIELDS WITH KEY type = 'E'.
  IF sy-subrc NE 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.
  ENDIF.

ENDFUNCTION.
