FUNCTION zabs_fm_status_reversal.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(LT_OCNUM) TYPE  /AGRI/T_FMOCNUM
*"  EXPORTING
*"     REFERENCE(ET_MESSAGES) TYPE  /AGRI/T_GPROLOG
*"----------------------------------------------------------------------

*-- Local declarations
  DATA :lt_fmoc_doc  TYPE /agri/t_fmoc_doc,
        lv_subrc     TYPE sy-subrc,
        lv_initiator TYPE /agri/gdescr,
        lt_messages  TYPE /agri/t_gprolog,
        ls_messages  TYPE /agri/s_gprolog.

  CONSTANTS:c_true(1) TYPE c VALUE 'X'.


  CALL FUNCTION '/AGRI/FMOC_VIEW'
    EXPORTING
      it_ocnum       = lt_ocnum
    IMPORTING
      et_ocdoc       = lt_fmoc_doc
    EXCEPTIONS
      no_data_exists = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  PERFORM messages_initialize USING 'SAVE'
                                    'SAVE'.

  LOOP AT lt_fmoc_doc INTO DATA(lwa_fmoc_doc).
*--Reverse Confirmations
    PERFORM reverse_confirmations USING c_true
                                CHANGING lwa_fmoc_doc
                                         lv_subrc.
  ENDLOOP.

  PERFORM messages_get  USING  lv_initiator
                        CHANGING et_messages.

ENDFUNCTION.
