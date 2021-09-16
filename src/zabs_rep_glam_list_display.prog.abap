*&---------------------------------------------------------------------*
*& Report  /AGRI/GLAM_LIST_DISPLAY
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  /agri/glam_list_display.

TABLES: /agri/glflot, /agri/glamhdr.

INCLUDE: /agri/abgl_constants,
         /agri/global_constants,
         /agri/gprolog_macros.

DATA: BEGIN OF gs_variables,
        program        TYPE sy-repid,
        subscr_details TYPE sy-dynnr,
      END OF gs_variables.
*--------------Constant Declarations-------------------------------
CONSTANTS: c_program   TYPE sy-repid VALUE '/AGRI/GLAM_LIST_DISPLAY'.

*--------------Global Declarations---------------------------------
DATA: gt_amhdr  TYPE /agri/t_glamhdr,
      gwa_amhdr TYPE /agri/s_glamhdr,
      ok_code   TYPE sy-ucomm,
      fcode     TYPE sy-ucomm.

*DATA : gt_selection TYPE /agri/t_bsel_values.
*
*
*PARAMETERS: p_jbcnt TYPE tbtcjob-jobcount NO-DISPLAY,
*            p_jobnr TYPE guid_16 NO-DISPLAY.

INITIALIZATION.

*  AUTHORITY-CHECK OBJECT 'V_AB_CNFTX'
*    ID '/AGRI/TCOD' FIELD '/AGRI/GLAM05'
*    ID 'ACTVT'  FIELD '03'.
*  IF sy-subrc <> 0.
*    MESSAGE ID '/AGRI/GLCOM' TYPE 'E' NUMBER '023' INTO sy-msgli.
*    message_simple space.
*  ENDIF.

START-OF-SELECTION.

***S4H Get Data Search profile
*  PERFORM get_search_fields.

GET /agri/glamhdr.

  MOVE-CORRESPONDING /agri/glamhdr TO gwa_amhdr.
  APPEND gwa_amhdr TO gt_amhdr.

END-OF-SELECTION.

  SORT gt_amhdr BY ivref charg_in ivact acode menge meins
                   autyp ivtyp ivcat tplnr_fl contr erdat.
  DELETE ADJACENT DUPLICATES FROM gt_amhdr
    COMPARING ivref charg_in ivact acode menge meins
              autyp ivtyp ivcat tplnr_fl contr. "erdat.

  SORT gt_amhdr BY ivtyp ivcat tplnr_fl ivdat charg_in usnam cputm ivact menge.
  DELETE ADJACENT DUPLICATES FROM gt_amhdr
    COMPARING ivtyp ivcat tplnr_fl ivdat charg_in usnam cputm ivact menge.

*-- Data de Fechamento
  IF p_ivdat IS NOT INITIAL.
    DELETE gt_amhdr WHERE ivdat GT p_ivdat.
  ENDIF.

  IF gt_amhdr[] IS NOT INITIAL.
*---Function Module That Gives Header Data In Subscreen.
    CALL FUNCTION '/AGRI/GLAM_LIST_DISPLAY'
      EXPORTING
        i_from_list        = c_true
        i_program          = sy-repid
        it_amhdr           = gt_amhdr
      CHANGING
        c_program          = gs_variables-program
        c_subscreen        = gs_variables-subscr_details
      EXCEPTIONS
        no_data_to_process = 1
        OTHERS             = 2.

    IF sy-subrc EQ 0.
*---Displaying Inventory List------------------------------------------*
      CALL SCREEN 0100.
    ELSE.
*-- Não existem ativos a serem exibidos!
      MESSAGE i330(zfmfp).
    ENDIF.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Module  STATUS_SET  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_set OUTPUT.

  PERFORM status_set.

ENDMODULE.                 " STATUS_SET  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  STATUS_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM status_set.

  SET PF-STATUS 'S100'.

ENDFORM.                    " STATUS_SET
*&---------------------------------------------------------------------*
*&      Module  TITLE_SET  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE title_set OUTPUT.

  PERFORM title_set.

ENDMODULE.                 " TITLE_SET  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  TITLE_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM title_set.

  SET TITLEBAR 'T001'.

ENDFORM.                    " TITLE_SET            " STATUS_0110  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  FCODE_PROCESSING  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE fcode_processing INPUT.

  PERFORM fcode_processing.

ENDMODULE.                 " FCODE_PROCESSING  INPUT
*&---------------------------------------------------------------------*
*&      Form  FCODE_PROCESSING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fcode_processing.

  DATA : lv_subroutine(40) TYPE c VALUE 'FCODE_'.

  fcode = ok_code.
  CLEAR ok_code.

  CONCATENATE lv_subroutine fcode INTO lv_subroutine.
  PERFORM (lv_subroutine) IN PROGRAM (c_program)
                          IF FOUND.

ENDFORM.                    " FCODE_PROCESSING

*&---------------------------------------------------------------------*
*&      Form  fcode_back
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_back.

  SET SCREEN 0.

ENDFORM.                    "fcode_back
*&---------------------------------------------------------------------*
*&      Form  fcode_canc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_canc.

  LEAVE PROGRAM.

ENDFORM.                    "fcode_canc
*&---------------------------------------------------------------------*
*&      Module  EXIT_PROCESSING  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_processing INPUT.

  PERFORM exit_processing.

ENDMODULE.                 " EXIT_PROCESSING  INPUT
*&---------------------------------------------------------------------*
*&      Form  EXIT_PROCESSING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exit_processing.

  SET SCREEN 0.

ENDFORM.                    " EXIT_PROCESSING
**&---------------------------------------------------------------------*
**&      Form  GET_SEARCH_FIELDS
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*FORM get_search_fields .
*  DATA: ls_gsp_environment  TYPE /agri/s_gsrchp_environment.
****
**  CREATE OBJECT ref_gsp_callback.
****
*  ls_gsp_environment-objtp = '/AGRI/GLCS'.
**  ls_gsp_environment-appln = '0'.
*  ls_gsp_environment-repname = '/AGRI/GLAM_LIST_DISPLAY'.
*  ls_gsp_environment-jobnr = p_jobnr.
*  ls_gsp_environment-jbcnt = p_jbcnt.
*  ls_gsp_environment-enable_bg_process = abap_true.
*  ls_gsp_environment-version = '002'.
*
*  CALL FUNCTION '/AGRI/GLCS_DB_SEARCH'
*    EXPORTING
*      is_srch_prfl_envr = ls_gsp_environment
**     IREF_CALLBACK     =
*    IMPORTING
**     ET_HEADER         =
*      et_amhdr          = gt_amhdr[]
**     E_SKIP_SELECT     =
*    CHANGING
*      ct_selection      = gt_selection
*    EXCEPTIONS
*      no_data_selected  = 1
*      OTHERS            = 2.
*
*  IF sy-subrc <> 0.
** Implement suitable error handling here
*  ENDIF.
*
*ENDFORM.
