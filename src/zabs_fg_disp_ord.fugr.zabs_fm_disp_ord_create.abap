FUNCTION zabs_fm_disp_ord_create.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IT_SELECTED_ROWS) TYPE  LVC_T_ROW
*"     REFERENCE(I_DISPLAY) TYPE  XFELD
*"  EXCEPTIONS
*"      NO_DATA_FOUND
*"----------------------------------------------------------------------
************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* FM Name           :  ZABS_FM_GET_GRID_REFRESH                        *
* Created By        :  Jetendra Mantena                                *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  06.18.2019                                      *
* TR                :  C4DK901784                                      *
* Version           :  001                                             *
* Description       :  Creating Multiple dispatch orders for single    *
*                      crop seasons                                    *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

  FIELD-SYMBOLS <fs_t_csdoc> TYPE /agri/t_glcs_doc.

  IF it_selected_rows IS INITIAL.
    MESSAGE e003(zabs_msgcls) DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_info. "Select atleast one row
    RETURN.
  ENDIF.

  PERFORM initialize_global_data.

  CLEAR gs_variables.
  READ TABLE it_selected_rows INTO DATA(ls_selected_rows) INDEX 2.
  IF sy-subrc EQ 0.
    MESSAGE e004(zabs_msgcls) DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_info.
  ENDIF.
  CLEAR ls_selected_rows.
  READ TABLE it_selected_rows INTO ls_selected_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e006(zabs_msgcls) DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_info.
  ENDIF.

  gs_variables-display_mode = i_display.

  ASSIGN ('(/AGRI/SAPLGLCSM)GT_CSDOC_INFOCUS[]') TO <fs_t_csdoc>.
  gt_csdoc_infocus = <fs_t_csdoc>.

  READ TABLE gt_csdoc_infocus INTO gs_csdoc_infocus INDEX ls_selected_rows-index.

  SELECT SINGLE b~fldty
    FROM /agri/glflcma AS a
    JOIN /agri/glcmhdr AS b
      ON a~cmnum = b~cmnum
    INTO @DATA(ls_fldty)
    WHERE a~tplnr_fl = @gs_csdoc_infocus-tplnr_fl
    AND   a~contr    = @gs_csdoc_infocus-contr.
  IF sy-subrc = 0.
    IF ls_fldty NE 'NURD'.
      MESSAGE e006(zabs_msgcls) DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_info.
    ENDIF.
  ELSE.
    MESSAGE e010(zabs_msgcls) DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_info.
  ENDIF.

  SELECT SINGLE pltxt
    INTO gs_variables-nustn_txt
    FROM /agri/glflot
    WHERE tplnr_fl = gs_csdoc_infocus-tplnr_fl.
  IF sy-subrc NE 0.
    CLEAR gs_variables-nustn_txt.
  ENDIF.

  PERFORM display_dispatch_orders CHANGING gt_disp_ord_fcat.

  CALL SCREEN 100 STARTING AT 18 1 ENDING AT 120 16.

ENDFUNCTION.
