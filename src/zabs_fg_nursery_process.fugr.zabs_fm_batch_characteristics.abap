FUNCTION zabs_fm_batch_characteristics.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IS_FPDOC_INFOCUS) TYPE  /AGRI/S_FMFP_DOC
*"     REFERENCE(IV_OVERVIEW_MODE)
*"     REFERENCE(REF_GRID_BATCHES) TYPE REF TO  /AGRI/CL_GUI_ALV_GRID
*"     REFERENCE(IT_FMFPCNF) TYPE  /AGRI/T_FMFP_CNF
*"  CHANGING
*"     REFERENCE(CV_DATA_CHANGE)
*"----------------------------------------------------------------------
************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Enhancement Name  :  ZABS_IENH_NURSERY_PROCESS                       *
* FM Name           :  ZABS_FM_BATCH_CHARACTERISTICS                   *
* Created By        :  Jetendra Mantena                                *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  06.18.2019                                      *
* TR                :  C4DK901784                                      *
* Version           :  001                                             *
* Description       :  Get Batch Characteristics based on sleceted     *
*                      batch in nursery process workbench, When user   *
*                      clicks on characteristics button                *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*--Local data declaration
  DATA : lt_char            TYPE STANDARD TABLE OF api_char,
         ls_batch_char      TYPE zabst_btchchr,
         ls_fmfpcnf         TYPE /agri/s_fmfp_cnf,
         lv_class           TYPE klah-class.

  REFRESH: gt_batch_char, gt_char.
  CLEAR: gv_classnum.

*--Get selected row of batches
  CALL METHOD ref_grid_batches->get_selected_rows
    IMPORTING
      et_row_no = DATA(lt_rows).

*--User can select only single batch at a time
  DESCRIBE TABLE lt_rows LINES DATA(lv_lines).
  IF lv_lines GT 1.
    MESSAGE i026(zabs_msgcls).
    RETURN.
  ELSEIF lv_lines EQ 0.
    MESSAGE i024(zabs_msgcls).
    RETURN.
  ENDIF.

  DATA(ls_row) = lt_rows[ 1 ].
*--Get batch data based on selected row
*  DATA(ls_fpbch) = is_fpdoc_infocus-x-fpbch[ ls_row-row_id ].
  FIELD-SYMBOLS: <lt_batches_fcat> TYPE /agri/t_fmfpbch_fcat.
  ASSIGN ('(/AGRI/SAPLFMNSM)GT_BATCHES_FCAT[]') TO <lt_batches_fcat>.
  IF sy-subrc EQ 0.
    DATA(ls_batch_fcat) = <lt_batches_fcat>[ ls_row-row_id ].
    READ TABLE is_fpdoc_infocus-x-fpbch INTO DATA(ls_fpbch)
      WITH KEY matnr = ls_batch_fcat-matnr
               charg = ls_batch_fcat-charg.
  ENDIF.

*--Fetch batch characteristics data from
*  custom table to display when process order
*  is in display mode.
  SELECT *
    FROM zabst_btchchr
    INTO TABLE @DATA(lt_clschar)
    WHERE aufnr EQ @ls_fpbch-aufnr
      AND contr EQ @ls_fpbch-contr
      AND batch EQ @ls_fpbch-charg.
  IF sy-subrc EQ 0.
    SORT lt_clschar BY atinn.
    READ TABLE lt_clschar INTO DATA(ls_clschar) INDEX 1.
    IF sy-subrc = 0.
      rmclf-class = ls_clschar-class.
      rmclf-kltxt = ls_clschar-kltxt.
    ENDIF.
    IF iv_overview_mode EQ zcl_abs_abap_maintain=>c_mode_display.
      gt_batch_char = lt_clschar.
      gs_variables-overview_mode = abap_true.
    ELSE.
      CLEAR gs_variables-overview_mode.
    ENDIF.
  ELSE.
    IF iv_overview_mode EQ zcl_abs_abap_maintain=>c_mode_display.
      MESSAGE i023(zabs_msgcls) WITH ls_fpbch-charg.
      RETURN.
    ENDIF.
  ENDIF.

  IF iv_overview_mode NE zcl_abs_abap_maintain=>c_mode_display.
*--Get batch characteristics based on process material
    CALL FUNCTION 'VBWS_MARA_CLASSIFICATION_GET'
      EXPORTING
        i_matnr              = is_fpdoc_infocus-x-fphdr-matnr
      IMPORTING
        e_class              = lv_class
      TABLES
        e_cl_char            = lt_char
      EXCEPTIONS
        classtype_not_found  = 1
        classtype_not_active = 2
        class_not_found      = 3
        no_allocations       = 4
        characters_not_found = 5
        OTHERS               = 6.

    IF sy-subrc NE 0.
      MESSAGE i027(zabs_msgcls).
      RETURN.
    ELSE.
      gt_char[] = lt_char[].
      gv_classnum = lv_class.
    ENDIF.

    IF lt_clschar[] IS NOT INITIAL.
*--Process batch characteristics to build and display characteristics
      LOOP AT lt_char INTO DATA(ls_char_temp).
        CLEAR ls_clschar.
        READ TABLE lt_clschar INTO ls_clschar
          WITH KEY atinn = ls_char_temp-atinn.
        IF sy-subrc = 0.
          APPEND ls_clschar TO gt_batch_char.
        ELSE.
          ls_clschar-aufnr = ls_fpbch-aufnr.
          ls_clschar-contr = ls_fpbch-contr.
          ls_clschar-batch = ls_fpbch-charg.
          ls_clschar-class = rmclf-class.
          ls_clschar-kltxt = rmclf-kltxt.
          ls_clschar-atinn = ls_char_temp-atinn.
          ls_clschar-atnam = ls_char_temp-atnam.
          ls_clschar-atbez = ls_char_temp-atbez.
          APPEND ls_clschar TO gt_batch_char.
        ENDIF.
      ENDLOOP.
    ELSE.
      SELECT SINGLE clint
       FROM klah
       INTO @DATA(lv_clint)
       WHERE class = @lv_class.
      IF sy-subrc = 0.
        rmclf-class = lv_class.
        SELECT SINGLE kschl
          FROM swor
          INTO rmclf-kltxt
          WHERE clint = lv_clint
            AND spras = sy-langu.
        IF sy-subrc <> 0.
          CLEAR rmclf-kltxt.
        ENDIF.
      ENDIF.

      ls_batch_char-aufnr = ls_fpbch-aufnr.
      ls_batch_char-contr = ls_fpbch-contr.
      ls_batch_char-batch = ls_fpbch-charg.
      ls_batch_char-class = rmclf-class.
      ls_batch_char-kltxt = rmclf-kltxt.
      LOOP AT lt_char INTO DATA(ls_char).
        ls_batch_char-atinn = ls_char-atinn.
        ls_batch_char-atnam = ls_char-atnam.
        ls_batch_char-atbez = ls_char-atbez.
        APPEND ls_batch_char TO gt_batch_char.
      ENDLOOP.
    ENDIF.
  ENDIF.

  DELETE gt_batch_char WHERE atnam = zcl_abs_abap_maintain=>c_charact_psdat.

*--Fetch code texts
  PERFORM fetch_code_texts.

*--Call Screen to display batch characteristics data
  CALL SCREEN 100 STARTING AT 18 1 ENDING AT 120 30.
  IF gs_variables-data_change IS NOT INITIAL.
    cv_data_change = abap_true.
  ENDIF.

ENDFUNCTION.
