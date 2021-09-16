*----------------------------------------------------------------------*
***INCLUDE /AGRI/LFMRCMNF0N .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  NOTES_REFRESH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM notes_refresh USING lv_objkey.

  CALL FUNCTION '/AGRI/G_NOTES_REFRESH'
    EXPORTING
      i_objtyp = c_object-bor
      i_objkey = lv_objkey
*     I_SUBOBJ = ' '
    .
ENDFORM.                    " NOTES_REFRESH
*&---------------------------------------------------------------------*
*&      Form  NOTES_MAINTAIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM notes_maintain .

  DATA: lv_call_from_popup,
        lv_descr           TYPE /agri/gdescr,
        lv_subobj          LIKE dd03p-fieldname,
        lv_mode            LIKE t180-trtyp.

  DATA: lv_rcnum TYPE zfmrcnum.

  lv_rcnum = gs_rcdoc_infocus-x-rchdr-rcnum.
  lv_mode = gs_variables-document_mode.
  lv_descr = TEXT-001.

  CALL FUNCTION '/AGRI/G_NOTES_SUBSCREEN_IMPORT'
    EXPORTING
      i_mode            = lv_mode
      i_objtyp          = c_object-bor
      i_descr           = lv_descr
      i_objkey          = lv_rcnum
      i_subobj          = lv_subobj
      i_call_from_popup = lv_call_from_popup
    CHANGING
      c_program         = gs_variables-program
      c_subscreen       = gs_variables-subscr_details
      c_change_flag     = gs_variables-data_changed.

ENDFORM.                    " NOTES_MAINTAIN
*&---------------------------------------------------------------------*
*&      Form  NOTES_TITLE_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM notes_title_prepare .

  DATA: lv_fieldname(5),
        ls_tabstrip_fcode TYPE /agri/s_gtabstrip,
        lv_gdescr         TYPE /agri/gdescr.

  FIELD-SYMBOLS: <lv_notes_text> TYPE char060.

  READ TABLE gt_tabstrip_fcodes INTO ls_tabstrip_fcode
                  WITH KEY local_fcode = c_fcode-tab_notes.
  CHECK sy-subrc EQ 0.
  lv_fieldname = ls_tabstrip_fcode-tabname+27.
  ASSIGN COMPONENT lv_fieldname OF STRUCTURE /agri/s_gtabstrip_captions
                       TO <lv_notes_text>.
  IF gs_variables-notes_title IS INITIAL.
    gs_variables-notes_title = <lv_notes_text>.
  ENDIF.
  <lv_notes_text> = gs_variables-notes_title.
  CHECK <lv_notes_text> IS ASSIGNED.
  lv_gdescr = <lv_notes_text>.
  notes_title_prepare c_object-bor
         gs_rcdoc_infocus-x-rchdr-rcnum space
         lv_gdescr.

ENDFORM.                    " NOTES_TITLE_PREPARE
*&---------------------------------------------------------------------*
*& Form NEW_POSITION_SET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_DOSE_LAYOUT_POSNR
*&---------------------------------------------------------------------*
FORM new_position_set USING VALUE(table_name)
                      CHANGING lv_acpos TYPE posnr.

  FIELD-SYMBOLS: <lt_rcdoc_infocus> TYPE ANY TABLE,
                 <lwa_rcdoc>        TYPE any,
                 <matnr_ins>        TYPE any,
                 <posnr>            TYPE any.

  DATA: lt_rcdoc_dinamyc TYPE string.
  DATA: lv_post TYPE int4.

  CASE table_name.
    WHEN 'RCVRS'.
*...Vistex-11.01.2019/Begin
*      SORT: gs_rcdoc_infocus-x-rcvrs ASCENDING BY posnr.
      SORT gs_rcdoc_infocus-x-rcvrs BY verid.
*...Vistex-11.01.2019/End
    WHEN 'RCLST'.
      SORT: gs_rcdoc_infocus-x-rclst ASCENDING BY posnr.
  ENDCASE.
*...Vistex-11.01.2019/Begin
*  CONCATENATE 'gs_rcdoc_infocus-x-' table_name INTO lt_rcdoc_dinamyc.
  CONCATENATE 'GS_RCDOC_INFOCUS-X-' table_name INTO lt_rcdoc_dinamyc.
*...Vistex-11.01.2019/End
  ASSIGN (lt_rcdoc_dinamyc) TO <lt_rcdoc_infocus>.

  LOOP AT <lt_rcdoc_infocus> ASSIGNING <lwa_rcdoc>.
    ASSIGN COMPONENT 'POSNR' OF STRUCTURE <lwa_rcdoc> TO <posnr>.
    ASSIGN COMPONENT 'MATNR_INS' OF STRUCTURE <lwa_rcdoc> TO <matnr_ins>.
    IF <matnr_ins> IS ASSIGNED
      OR <posnr> IS ASSIGNED.
      lv_post = <posnr>.
    ENDIF.
  ENDLOOP.

  lv_acpos =  ( ( lv_post / 10 ) + 1 ).
  lv_acpos =  ( lv_acpos * 10 ).

ENDFORM.
