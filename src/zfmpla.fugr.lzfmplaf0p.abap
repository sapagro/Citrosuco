*&---------------------------------------------------------------------*
*& Form data_tables_prepare
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> I_MODE
*&      <-- ET_FLDOC
*&---------------------------------------------------------------------*
FORM data_tables_prepare USING    lv_mode
                         CHANGING et_pldoc TYPE zt_fmpl_doc.

  DATA: lwa_plhdr    LIKE LINE OF gt_plhdr,
        lwa_plitm    LIKE LINE OF gt_plitm,
        lwa_fmpl_doc LIKE LINE OF et_pldoc.

  CHECK gt_plhdr[] IS NOT INITIAL.

  LOOP AT gt_plhdr INTO lwa_plhdr.
    CLEAR lwa_fmpl_doc.

    lwa_fmpl_doc-x-plhdr = lwa_plhdr.

    LOOP AT gt_plitm INTO lwa_plitm
                    WHERE plnum = lwa_plhdr-plnum.
      APPEND lwa_plitm TO lwa_fmpl_doc-x-plitm[].
    ENDLOOP.

    APPEND lwa_fmpl_doc TO et_pldoc.

  ENDLOOP.

ENDFORM.                    " DATA_TABLES_PREPARE
*&---------------------------------------------------------------------*
*& Form PRODUCTION_ORDER_CREATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM production_orders_create USING lt_plhdr TYPE zt_fmplhdr
                                    lt_plitm TYPE zt_fmplitm
                              CHANGING lt_fmfpitm TYPE /agri/t_fmfpitm.
  DATA: lv_set_infocus,
        lv_answer,
        lv_subrc         TYPE sy-subrc,
        lt_fmpmdoc       TYPE /agri/t_fmfp_doc,
        ls_fpdoc_infocus TYPE /agri/s_fmfp_doc,
        lwa_fpitm        TYPE /agri/s_fmfpitm,
        lwa_selected_row TYPE lvc_s_row,
        lwa_fpitm_fcat   TYPE /agri/s_fmfpitm_fcat,
        lwa_row          TYPE lvc_s_row,
        lt_fmaufnr       TYPE /agri/t_fmaufnr,
        lwa_fmaufnr      TYPE /agri/s_fmaufnr,
        lwa_plitm        TYPE zsc_fmplitm,
        lv_count         TYPE int4,
        lt_rows          TYPE lvc_t_row.

  FIELD-SYMBOLS: <lwa_fmfpi> TYPE /agri/s_fmfpitm.


  DESCRIBE TABLE lt_plitm LINES lv_count.
  IF lv_count GT 1.
    SORT lt_plitm BY aufnr ASCENDING .
    DELETE ADJACENT DUPLICATES FROM lt_plitm COMPARING aufnr.
  ENDIF.

  LOOP AT lt_plitm INTO lwa_plitm.
    lwa_fmaufnr-aufnr = lwa_plitm-aufnr.
    APPEND lwa_fmaufnr TO lt_fmaufnr.
  ENDLOOP.

  CALL FUNCTION '/AGRI/FMFP_VIEW'
    EXPORTING
      it_aufnr       = lt_fmaufnr
    IMPORTING
      et_fpdoc       = lt_fmpmdoc
    EXCEPTIONS
      no_data_exists = 1
      OTHERS         = 2.


  LOOP AT lt_fmpmdoc INTO ls_fpdoc_infocus.

    LOOP AT ls_fpdoc_infocus-x-fpitm ASSIGNING <lwa_fmfpi>
                                     WHERE aufnr_to IS INITIAL.
      MOVE c_true TO <lwa_fmfpi>-confm.
    ENDLOOP.
    PERFORM create_task_orders IN PROGRAM ('/AGRI/SAPLFMFPM')
                               CHANGING ls_fpdoc_infocus
                                        lv_set_infocus.
  ENDLOOP.

  CALL FUNCTION '/AGRI/FMFP_READ'
    EXPORTING
      it_aufnr       = lt_fmaufnr
    IMPORTING
      et_fmfpitm     = lt_fmfpitm
    EXCEPTIONS
      no_data_exists = 1
      OTHERS         = 2.

ENDFORM.                    "Production_orders_create
