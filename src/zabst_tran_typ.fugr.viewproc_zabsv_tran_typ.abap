*---------------------------------------------------------------------*
*    program for:   VIEWPROC_ZABSV_TRAN_TYP
*   generation date: 09.11.2020 at 05:15:33
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION VIEWPROC_ZABSV_TRAN_TYP       .
*----------------------------------------------------------------------*
* Initialization: set field-symbols etc.                               *
*----------------------------------------------------------------------*
   IF LAST_VIEW_INFO NE VIEW_NAME.
ASSIGN ZABSV_TRAN_TYP TO <TABLE1>.
ASSIGN *ZABSV_TRAN_TYP TO <INITIAL>.
ASSIGN STATUS_ZABSV_TRAN_TYP TO <STATUS>.
     PERFORM INITIALISIEREN.
   ENDIF.
   PERFORM JUSTIFY_ACTION_MODE.
   MOVE: VIEW_ACTION TO MAINT_MODE,
         CORR_NUMBER TO CORR_NBR.

*----------------------------------------------------------------------*
* Get data from database                                               *
*----------------------------------------------------------------------*
  IF FCODE EQ READ OR FCODE EQ READ_AND_EDIT.
    PERFORM PREPARE_READ_REQUEST.
    IF X_HEADER-FRM_RP_GET NE SPACE.
            PERFORM (X_HEADER-FRM_RP_GET) IN PROGRAM.
    ELSE.
PERFORM GET_DATA_ZABSV_TRAN_TYP.
    ENDIF.
    IF FCODE EQ READ_AND_EDIT. FCODE = EDIT. ENDIF.
  ENDIF.

  CASE FCODE.
    WHEN  EDIT.                          " Edit read data
      PERFORM CALL_DYNPRO.
      PERFORM CHECK_UPD.
*....................................................................*

    WHEN SAVE.                           " Write data into database
      PERFORM PREPARE_SAVING.
      IF <STATUS>-UPD_FLAG NE SPACE.
        IF X_HEADER-FRM_RP_UPD NE SPACE.
          PERFORM (X_HEADER-FRM_RP_UPD) IN PROGRAM.
        ELSE.
          IF SY-SUBRC EQ 0.
PERFORM DB_UPD_ZABSV_TRAN_TYP.
          ENDIF.
        ENDIF.
        PERFORM AFTER_SAVING.
      ENDIF.
*....................................................................*

    WHEN RESET_LIST.     " Refresh all marked entries of EXTRACT from db
      PERFORM RESET_ENTRIES USING LIST_BILD.
*....................................................................*

    WHEN RESET_ENTRY.               " Refresh single entry from database
      PERFORM RESET_ENTRIES USING DETAIL_BILD.
*.......................................................................
  ENDCASE.
MOVE STATUS_ZABSV_TRAN_TYP-UPD_FLAG TO UPDATE_REQUIRED.
ENDFUNCTION.
