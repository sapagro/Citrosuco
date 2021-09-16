*&---------------------------------------------------------------------*
*& Include          LZFG_ACMF0N
*&---------------------------------------------------------------------*
FORM notes_refresh USING lv_objkey.

  CALL FUNCTION '/AGRI/G_NOTES_REFRESH'
    EXPORTING
      i_objtyp = c_object-bor
      i_objkey = lv_objkey.
*     I_SUBOBJ = ' '

ENDFORM.
FORM notes_maintain .

  DATA: lv_call_from_popup,
        lv_descr TYPE /agri/gdescr,
        lv_subobj LIKE dd03p-fieldname,
        lv_mode LIKE t180-trtyp.


  DATA: lv_accom TYPE /agri/fmaccom.

  lv_accom = gs_acdoc_infocus-x-achdr-accom.
  lv_mode = gs_variables-document_mode.
  lv_descr = text-001.

  CALL FUNCTION '/AGRI/G_NOTES_SUBSCREEN_IMPORT'
    EXPORTING
      i_mode            = lv_mode
      i_objtyp          = c_object-bor
      i_descr           = lv_descr
      i_objkey          = lv_accom
      i_subobj          = lv_subobj
      i_call_from_popup = lv_call_from_popup
    CHANGING
      c_program         = gs_variables-program
      c_subscreen       = gs_variables-subs_items
      c_change_flag     = gs_variables-document_changed.
ENDFORM.                    " NOTES_MAINTAIN
*&---------------------------------------------------------------------*
*&      Form  NOTES_TITLE_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM notes_title_prepare .
  DATA: lv_fieldname(5),
        ls_tabstrip_fcode TYPE /agri/s_gtabstrip,
        lv_gdescr TYPE /agri/gdescr.

  FIELD-SYMBOLS: <lv_notes_text> TYPE char060.

  READ TABLE gt_tabstrip_fcodes INTO ls_tabstrip_fcode
                  WITH KEY local_fcode = c_fcode-tab_notes.
  CHECK sy-subrc EQ 0.
  lv_fieldname = ls_tabstrip_fcode-tabname+27.
  ASSIGN COMPONENT lv_fieldname OF STRUCTURE /agri/s_gtabstrip_captions
                       TO <lv_notes_text>.
  CHECK <lv_notes_text> IS ASSIGNED.
****E SP2
  IF gs_variables-notes_title IS INITIAL.
    gs_variables-notes_title = <lv_notes_text>.
  ENDIF.
  <lv_notes_text> = gs_variables-notes_title.
***
*  CHECK <lv_notes_text> IS ASSIGNED.
  lv_gdescr = <lv_notes_text>.
  notes_title_prepare c_object-bor
         gs_acdoc_infocus-x-achdr-accom space
         lv_gdescr.
ENDFORM.                    " NOTES_TITLE_PREPARE
