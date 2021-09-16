FUNCTION ZFMAC_CREATE_DIALOG.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_ASLVL) TYPE  /AGRI/GLASLVL
*"     REFERENCE(I_POPUP) TYPE  XFLD DEFAULT 'X'
*"     REFERENCE(IT_TPLNR) TYPE  /AGRI/T_GLTPLNR OPTIONAL
*"     REFERENCE(IT_EQUNR) TYPE  /AGRI/T_FMEQUNR OPTIONAL
*"     REFERENCE(IT_CSKEY) TYPE  /AGRI/T_GLCS_KEY OPTIONAL
*"  EXPORTING
*"     REFERENCE(ET_acdoc) TYPE  /AGRI/T_FMAC_DOC
*"  EXCEPTIONS
*"      INCONSISTENT_DATA
*"      NO_AUTHORIZATION
*"      NO_DATA_EXISTS
*"      CANCELLED
*"--------------------------------------------------------------------

**  DATA: lv_subrc  TYPE sy-subrc,
**        lwa_mdsnp TYPE /agri/s_FMACsnp,
**        lv_lines  TYPE i.
**
**  PERFORM transaction_init USING c_mode_create.
**  PERFORM document_data_initialize USING c_true.
**
**  IF i_aslvl IS INITIAL.
**    RAISE inconsistent_data.
**  ENDIF.
**
**  gs_variables-document_mode = c_mode_create.
**  gs_variables-characteristics_display = i_aslvl.
**  CLEAR: /agri/1899SC_FMACHDR, /agri/s_glflcma,
**         gs_variables-external_dialog, gs_variables-mass_action,
**         gs_variables-cancelled.
**
**  /agri/1899SC_FMACHDR-aslvl = gs_variables-measurement_level = i_aslvl.
**
**  REFRESH: gt_mdsnp.
**  PERFORM get_selection_data TABLES it_tplnr
**                                    it_equnr
**                                    it_cskey
**                              USING gs_variables-measurement_level
**                           CHANGING lv_subrc.
**  IF gt_mdsnp IS INITIAL.
**    RAISE no_data_exists.
**  ENDIF.
**
**  IF i_popup IS NOT INITIAL.
**
**    DESCRIBE TABLE gt_mdsnp LINES lv_lines.
**    IF lv_lines EQ 1.
**
**      READ TABLE gt_mdsnp INTO lwa_mdsnp INDEX 1.
**      MOVE-CORRESPONDING lwa_mdsnp TO gs_acdoc_infocus-x-achdr.
**      IF i_aslvl EQ c_measurement_level-crop_seasons OR
**         i_aslvl EQ c_measurement_level-harvest.
**        SELECT SINGLE * FROM /agri/glflcma "#EC CI_ALL_FIELDS_NEEDED
**          INTO CORRESPONDING FIELDS OF /agri/s_glflcma
**         WHERE tplnr_fl EQ gs_acdoc_infocus-x-achdr-tplnr_fl
**           AND contr    EQ gs_acdoc_infocus-x-achdr-contr.
**      ENDIF.
**      gs_acdoc_infocus-acnum =
**      gs_acdoc_infocus-x-achdr-acnum = text-046.
**
**      gs_variables-refresh_items_grid = c_true.
**      gs_variables-external_dialog = c_true.
**      CALL SCREEN 0102 STARTING AT 15 1.
**      IF gs_variables-cancelled IS NOT INITIAL.
**        RAISE cancelled.
**      ENDIF.
**
**    ELSE.
**
**      gs_variables-mass_action = c_true.
**      gs_variables-refresh_output_table = c_true.
**      CALL SCREEN 0101 STARTING AT 15 1 ENDING AT 180 30.
**      IF gs_variables-cancelled IS NOT INITIAL.
**        RAISE cancelled.
**      ENDIF.
**
**    ENDIF.
**
**  ELSE.
**
**    gs_variables-refresh_output_table = c_true.
**    gs_variables-mass_action = c_true.
**    CALL SCREEN 0101.
**
**  ENDIF.
**
**  et_acdoc = gt_acdoc.

ENDFUNCTION.
