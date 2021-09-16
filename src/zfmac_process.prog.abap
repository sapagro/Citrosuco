REPORT  zfmac_process.

INCLUDE: /agri/global_constants.

TABLES: zfmachdr, zfmaitm.

DATA: gt_achdr   TYPE zt_fmachdr,
      lwa_achdr  TYPE zsc_fmachdr,
      gt_acitm   TYPE zt_fmacitm,
      lwa_acitm  TYPE zsc_fmacitm.


CONSTANTS: c_program_ac_process TYPE sy-repid VALUE 'ZFMAC_PROCESS'.

PARAMETERS: p_excall TYPE c NO-DISPLAY.

START-OF-SELECTION.

GET zfmachdr.
  MOVE-CORRESPONDING zfmachdr TO lwa_achdr.
  APPEND lwa_achdr TO gt_achdr.

GET zfmaitm.
  MOVE-CORRESPONDING zfmaitm TO lwa_acitm.
  APPEND lwa_acitm TO gt_acitm.

END-OF-SELECTION.

  IF p_excall IS INITIAL.
    IF gt_achdr IS NOT INITIAL.
      PERFORM data_display.
    ELSE.
      MESSAGE s751(/agri/global).
    ENDIF.
  ELSE.
    CALL FUNCTION 'ZFMAC_SEARCH_RESULTS_GET'
      CHANGING
        ct_achdr = gt_achdr
        ct_acitm = gt_acitm.

    LEAVE PROGRAM.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  DATA_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM data_display .
  DATA: lt_fieldcat TYPE slis_t_fieldcat_alv,
        lt_sort     TYPE slis_t_sortinfo_alv,
        lwa_sort    TYPE slis_sortinfo_alv,
        ls_layout   TYPE slis_layout_alv.

  ls_layout-colwidth_optimize = c_true.

  lwa_sort-fieldname = 'ACNUM'.
  lwa_sort-up        = c_true.
  APPEND lwa_sort TO lt_sort.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name   = c_program_ac_process
*     I_INTERNAL_TABNAME           =
      i_structure_name = 'ZSC_FMACHDR'
*     I_CLIENT_NEVER_DISPLAY       = 'X'
*     I_INCLNAME       =
*     I_BYPASSING_BUFFER           =
*     I_BUFFER_ACTIVE  =
    CHANGING
      ct_fieldcat      = lt_fieldcat
   EXCEPTIONS
     INCONSISTENT_INTERFACE       = 1
     PROGRAM_ERROR    = 2
     OTHERS           = 3
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK  = ' '
*     I_BYPASSING_BUFFER = ' '
*     I_BUFFER_ACTIVE    = ' '
      i_callback_program = sy-cprog
*     I_CALLBACK_PF_STATUS_SET          = ' '
*     I_CALLBACK_USER_COMMAND           = ' '
*     I_CALLBACK_TOP_OF_PAGE            = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME   =
*     I_BACKGROUND_ID    = ' '
*     I_GRID_TITLE       =
*     I_GRID_SETTINGS    =
      is_layout          = ls_layout
      it_fieldcat        = lt_fieldcat
*     IT_EXCLUDING       =
*     IT_SPECIAL_GROUPS  =
      it_sort            = lt_sort
*     IT_FILTER          =
*     IS_SEL_HIDE        =
      i_default          = c_true
      i_save             = c_true
*     IS_VARIANT         =
*     IT_EVENTS          =
*     IT_EVENT_EXIT      =
*     IS_PRINT           =
*     IS_REPREP_ID       =
*     I_SCREEN_START_COLUMN             = 0
*     I_SCREEN_START_LINE               = 0
*     I_SCREEN_END_COLUMN               = 0
*     I_SCREEN_END_LINE  = 0
*     I_HTML_HEIGHT_TOP  = 0
*     I_HTML_HEIGHT_END  = 0
*     IT_ALV_GRAPHICS    =
*     IT_HYPERLINK       =
*     IT_ADD_FIELDCAT    =
*     IT_EXCEPT_QINFO    =
*     IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*     E_EXIT_CAUSED_BY_CALLER           =
*     ES_EXIT_CAUSED_BY_USER            =
    TABLES
      t_outtab           = gt_achdr
 EXCEPTIONS
     PROGRAM_ERROR      = 1
     OTHERS             = 2.

ENDFORM.                    " DATA_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  AUTHORITY_AT_MD_TYPE_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM authority_at_md_type_check .

  DATA: lv_selections_filtered,
        lv_subrc               TYPE sy-subrc,
        ls_message             TYPE /agri/s_gprolog,
        lwa_mdhdr              TYPE /agri/s_glmdhdr,
        lwa_mdhdr2             TYPE /agri/s_glmdhdr.


ENDFORM.                    " AUTHORITY_AT_MD_TYPE_CHECK
