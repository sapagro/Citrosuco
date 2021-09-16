*----------------------------------------------------------------------*
***INCLUDE LZVX_CROP_SEASON_OPF02.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                    SOFTWARE FACTORY <Vistex>                         *
*----------------------------------------------------------------------*
* Requirement ID    : Gap ID 03 + GAP ID 04                            *
* Created By        : Adonis Rossato                                   *
* Creation Date     : 15.04.2021                                       *
* Descriptión       : Logical Crop Creation and Process orders         *
*----------------------------------------------------------------------*
*                    LOG OF MODIFICATIONS                              *
*----------------------------------------------------------------------*
* Changed by        :                                                  *
* Changed Date      :                                                  *
* Transport         :                                                  *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form move_indata
*&---------------------------------------------------------------------*
FORM move_indata USING ps_indata    TYPE zvxs_crop_input
              CHANGING cs_po_indata TYPE zvxs_prc_order.

  cs_po_indata-ziwerk    = gv_iwerk.
  cs_po_indata-zstrno    = gv_tplnr.
  cs_po_indata-zcmnum    = ps_indata-cmnum.
  cs_po_indata-zdatab    = ps_indata-datab.
  cs_po_indata-zvaria    = ps_indata-varia.
  cs_po_indata-zindfield = ps_indata-indfield.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form get_crop_season_key
*&---------------------------------------------------------------------*
FORM get_crop_season_key USING ps_indata   TYPE zvxs_prc_order
                      CHANGING ct_messages TYPE zvxtt_message_gprolog.

*-- Local variables
  DATA: lv_msgv1 TYPE msgv1.

*-- Validate Crop & variant with in validate date or not and get counter
  SELECT tplnr_fl contr season astat yaufnr
    FROM /agri/glflcma
    INTO TABLE gt_cs_key
   WHERE tplnr_fl EQ gv_tplnr
     AND cmnum    EQ ps_indata-zcmnum
     AND varia    EQ ps_indata-zvaria
     AND ( datab  LE ps_indata-zdatab
     AND   datbi  GE ps_indata-zdatab )
     AND loevm    EQ space.
  IF sy-subrc NE 0.
*-- Build messages
    lv_msgv1 = ps_indata-zcmnum.
    PERFORM build_msgs USING gc_constants-msgty_error '034'
                             lv_msgv1 space
                    CHANGING ct_messages[].
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form create_process_order
*&---------------------------------------------------------------------*
FORM create_process_order  USING ps_indata   TYPE zvxs_prc_order
                        CHANGING ct_messages TYPE zvxtt_message_gprolog
                                 ct_outdata  TYPE zvxtt_prc_order_out.

*-- Local Internal tables:
  DATA: lt_messages TYPE /agri/t_gprolog,
        lt_fpdoc    TYPE /agri/t_fmfp_doc,
        lt_aufnr    TYPE /agri/t_fmaufnr,
        lt_cskey    TYPE /agri/t_glcs_key,

*-- Local workareas:
        ls_mesg     TYPE zvxs_message_gprolog,
        ls_outdata  TYPE zvxs_prc_order_out,
        ls_aufnr    TYPE /agri/s_fmaufnr,
        ls_cskey    TYPE /agri/s_glcs_key,

*-- Local Variables:
        lv_gstrp    TYPE co_gstrp,
        lv_msgv1    TYPE msgv1,
        lv_msgv2    TYPE msgv2.

*-- Check if any existing processing order exist for Input data
  PERFORM check_order USING ps_indata.

  READ TABLE gt_cs_key INTO DATA(ls_cskey1) INDEX 1.
  ls_cskey-tplnr_fl = ls_cskey1-tplnr_fl.
  ls_cskey-contr = ls_cskey1-contr.
  APPEND ls_cskey TO lt_cskey.

*-- Create process orders for all crop process
  LOOP AT gt_cpros INTO DATA(ls_cpros).
    CLEAR : lv_msgv1, lv_msgv2.

*-- Check if any active process order exist for that cpros, variant
    READ TABLE gt_fmfphdr INTO DATA(gs_fmfphdr)
         WITH KEY cmnum = ls_cpros-cmnum
                  varia = ls_cpros-varia
                  cpros = ls_cpros-cpros.
    IF sy-subrc EQ 0.

*-- Append to exporting table
      ls_outdata-cpros = gs_fmfphdr-cpros.
      ls_outdata-aufnr = gs_fmfphdr-aufnr.
      APPEND ls_outdata TO ct_outdata.

*-- Build messages
      lv_msgv1 = gs_fmfphdr-aufnr.
      lv_msgv2 = gs_fmfphdr-cpros.
      PERFORM build_msgs USING gc_constants-msgty_error '043'
                               lv_msgv1 lv_msgv2
                      CHANGING ct_messages[].

    ELSE.

*-- If not active process order found, then create process order
      lv_gstrp = ps_indata-zdatab.
      CALL FUNCTION '/AGRI/FMFP_ORDER_CREATE'
        EXPORTING
          i_save_messages       = abap_true
          i_commit_work         = abap_true
          i_cpros               = ls_cpros-cpros
          i_gstrp               = lv_gstrp
          it_cskey              = lt_cskey[]
        IMPORTING
          et_fpdoc              = lt_fpdoc[]
          et_messages           = lt_messages[]
        EXCEPTIONS
          inconsistent_data     = 1
          no_valid_crop_seasons = 2
          OTHERS                = 3.
      IF sy-subrc EQ 0.
*-- Build messages
        LOOP AT lt_fpdoc INTO DATA(ls_fpdoc).
          lv_msgv1         = ls_fpdoc-aufnr.
          ls_outdata-cpros = ls_cpros-cpros.
          ls_outdata-aufnr = ls_fpdoc-aufnr.
          APPEND ls_outdata TO ct_outdata.
*-- Collect the process orders created.
          ls_aufnr = ls_fpdoc-aufnr.
          APPEND ls_aufnr TO lt_aufnr.
          PERFORM build_msgs USING gc_constants-msgty_success '038'
                                   lv_msgv1 space
                          CHANGING ct_messages[].
          CLEAR : lv_msgv1, ls_outdata.
        ENDLOOP.
      ENDIF.

      IF lt_fpdoc IS INITIAL.
*-- If one of the process order creation fails we need to stop
*   creation of the others process orders..
        READ TABLE lt_messages TRANSPORTING NO FIELDS
        WITH KEY   msgty = gc_constants-msgty_error.
        IF sy-subrc EQ 0.
          PERFORM build_msgs USING gc_constants-msgty_error '163'
                                   space space
                          CHANGING ct_messages[].
          EXIT.
        ENDIF.

        LOOP AT lt_messages INTO DATA(ls_message).
          CLEAR : ls_mesg.
          ls_mesg = CORRESPONDING #( ls_message ).
          APPEND ls_mesg TO ct_messages.
        ENDLOOP.
      ENDIF.
      CLEAR : lt_fpdoc[], lt_messages[].

    ENDIF.
  ENDLOOP.

*-- If any errors, TECO already created process orders
  READ TABLE ct_messages TRANSPORTING NO FIELDS
  WITH KEY   msgty = gc_constants-msgty_error.
  IF sy-subrc EQ 0.
    gv_po_error = abap_true.
    IF NOT lt_aufnr[] IS INITIAL.
*-- Export the Memory variable to BADI : WORKORDER_UPDATE
      gv_teco_call = gc_constants-teco_call.
      EXPORT gv_teco_call FROM gv_teco_call TO MEMORY ID 'Z_PO_TECO_CHECK'.

      PERFORM teco_proc_order USING lt_aufnr
                           CHANGING ct_messages.
      CLEAR gv_teco_call.
    ENDIF.

  ELSE.
    DESCRIBE TABLE gt_cpros LINES DATA(lv_process).
    DESCRIBE TABLE lt_aufnr LINES DATA(lv_aufnr).
    IF lv_process EQ lv_aufnr.
      gv_poflag = gc_constants-flag.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form teco_proc_order
*&---------------------------------------------------------------------*
FORM teco_proc_order USING pt_aufnr    TYPE /agri/t_fmaufnr
                  CHANGING ct_messages TYPE zvxtt_message_gprolog.

*-- Local internal tables
  DATA: lt_messages TYPE /agri/t_gprolog,
        lt_fpdoc    TYPE /agri/t_fmfp_doc,

*-- Local work areas
        ls_mesg     TYPE zvxs_message_gprolog,

*-- Local varaibles
        lv_msgv1    TYPE msgv1.

*-- Order Mass Techincal Complete
  CALL FUNCTION '/AGRI/FMFP_ORDER_MASS_TECO'
    EXPORTING
      it_aufnr          = pt_aufnr[]
    IMPORTING
      et_fpdoc          = lt_fpdoc[]
      et_messages       = lt_messages[]
    EXCEPTIONS
      inconsistent_data = 1
      OTHERS            = 2.
  IF sy-subrc EQ 0.
*-- Build messages
    LOOP AT lt_fpdoc INTO DATA(ls_fpdoc).
      lv_msgv1         = ls_fpdoc-aufnr.
      PERFORM build_msgs USING gc_constants-msgty_success '044'
                               lv_msgv1 space
                      CHANGING ct_messages[].
      CLEAR lv_msgv1.
    ENDLOOP.
  ENDIF.

  LOOP AT lt_messages INTO DATA(ls_message) WHERE msgty NE 'S'.
    CLEAR : ls_mesg.
    ls_mesg = CORRESPONDING #( ls_message ).
    APPEND ls_mesg TO ct_messages.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form check_order
*&---------------------------------------------------------------------*
FORM check_order USING ps_indata TYPE zvxs_prc_order.

  IF gt_cpros IS NOT INITIAL.
*-- Fetch all acvtive process orders to check before creation
    SELECT aufnr cmnum varia cpros                 "#EC CI_NO_TRANSFORM
      FROM /agri/fmfphdr
      INTO TABLE gt_fmfphdr
       FOR ALL ENTRIES IN gt_cpros
     WHERE autyp    EQ gc_constants-autyp     "Document category - AO
       AND tplnr_fl EQ gv_tplnr
       AND cmnum    EQ ps_indata-zcmnum
       AND varia    EQ ps_indata-zvaria
       AND cpros    EQ gt_cpros-cpros
       AND ( datab  LE ps_indata-zdatab
       AND   datbi  GE ps_indata-zdatab )
       AND iwerk    EQ ps_indata-ziwerk
       AND tecom    EQ space.

    IF sy-subrc EQ 0.
      SORT gt_fmfphdr BY cmnum varia cpros.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form inactivate_process_order
*&---------------------------------------------------------------------*
FORM inactivate_process_order USING ps_indata   TYPE zvxs_prc_order
                           CHANGING ct_messages TYPE zvxtt_message_gprolog
                                    ct_outdata  TYPE zvxtt_prc_order_out.

*-- Local Internal tables:
  DATA : lt_aufnr    TYPE /agri/t_fmaufnr,
         lt_messages TYPE /agri/t_gprolog,
         lt_fpdoc    TYPE /agri/t_fmfp_doc.

**-- Local workareas:
  DATA : ls_mesg    TYPE zvxs_message_gprolog,
         ls_outdata TYPE zvxs_prc_order_out.

*-- Local Variables:
  DATA : lv_msgv1 TYPE msgv1.

*-- When indicator field is 'D', we need to fetch all active
*-- Process orders based on RFC input to do Techo
  SELECT aufnr
    FROM /agri/fmfphdr
    INTO CORRESPONDING FIELDS OF TABLE lt_aufnr
   WHERE autyp    EQ gc_constants-autyp     "Document category - AO
     AND tplnr_fl EQ gv_tplnr
     AND cmnum    EQ ps_indata-zcmnum
     AND varia    EQ ps_indata-zvaria
     AND ( datab  LE ps_indata-zdatab
     AND   datbi  GE ps_indata-zdatab )
     AND iwerk    EQ ps_indata-ziwerk
     AND tecom    EQ space.

  IF lt_aufnr IS INITIAL.
*-- Build messages
    lv_msgv1 = ps_indata-zcmnum.
    PERFORM build_msgs USING gc_constants-msgty_success '162'
                             lv_msgv1 space
                    CHANGING ct_messages.

  ELSE.

*-- Order Mass Techincal Complete
    CALL FUNCTION '/AGRI/FMFP_ORDER_MASS_TECO'
      EXPORTING
        it_aufnr          = lt_aufnr[]
      IMPORTING
        et_fpdoc          = lt_fpdoc[]
        et_messages       = lt_messages[]
      EXCEPTIONS
        inconsistent_data = 1
        OTHERS            = 2.
    IF sy-subrc EQ 0.
*-- Build messages
      LOOP AT lt_fpdoc INTO DATA(ls_fpdoc).
        lv_msgv1         = ls_fpdoc-aufnr.
        ls_outdata-aufnr = ls_fpdoc-aufnr.
        APPEND ls_outdata TO ct_outdata.
        PERFORM build_msgs USING gc_constants-msgty_success '044'
                                 lv_msgv1 space
                        CHANGING ct_messages[].
        CLEAR : lv_msgv1, ls_outdata, ls_fpdoc.
      ENDLOOP.
    ENDIF.

    LOOP AT lt_messages INTO DATA(ls_message) WHERE msgty NE 'S'.
      CLEAR : ls_mesg.
      ls_mesg = CORRESPONDING #( ls_message ).
      APPEND ls_mesg TO ct_messages.
    ENDLOOP.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form upd_poflag_in_season
*&---------------------------------------------------------------------*
FORM upd_poflag_in_season USING ps_indata   TYPE zvxs_crop_input
                       CHANGING ct_messages TYPE zvxtt_message_gprolog.

*-- Local internal tables
  DATA: lt_flcma        TYPE /agri/t_glflcma,
        lt_csprs        TYPE /agri/t_glcsprs,
        lt_csprso       TYPE /agri/t_glcsprso,
        lt_csprst       TYPE /agri/t_glcsprst,
        lt_csprsor      TYPE /agri/t_glcsprsor,
        lt_csdfl        TYPE /agri/t_glcsdfl,
        lt_messages     TYPE /agri/t_gprolog,
        lt_flcma_change TYPE /agri/t_glflcma,
        lt_glcs_change  TYPE /agri/t_glcs_doc,
        lt_cskey        TYPE /agri/t_glcs_key,

*-- Local work area
        ls_mesg         TYPE zvxs_message_gprolog,
        ls_glcs_change  TYPE /agri/s_glcs_doc,
        ls_cskey        TYPE /agri/s_glcs_key,

*-- Local Variables
        lv_msgv1        TYPE msgv1,
        lv_msgv2        TYPE msgv2.

  READ TABLE gt_cs_key INTO DATA(ls_cskey1) INDEX 1.
  IF sy-subrc EQ 0.
    ls_cskey-tplnr_fl = ls_cskey1-tplnr_fl.
    ls_cskey-contr    = ls_cskey1-contr.
    APPEND ls_cskey TO lt_cskey.
  ENDIF.

*-- Get Crop Season data
  CALL FUNCTION '/AGRI/GLCS_READ'
    EXPORTING
      it_cskey       = lt_cskey
    IMPORTING
      et_flcma       = lt_flcma
      et_csprs       = lt_csprs
      et_csprso      = lt_csprso
      et_csprst      = lt_csprst
      et_csprsor     = lt_csprsor
      et_csdfl       = lt_csdfl
    EXCEPTIONS
      no_data_exists = 1
      OTHERS         = 2.
  IF sy-subrc NE 0.
*-- Build messages
    lv_msgv1      = ps_indata-tplnr.
    lv_msgv2      = ls_cskey-contr.
    PERFORM build_msgs USING gc_constants-msgty_error '057'
                             lv_msgv1 lv_msgv2
                    CHANGING ct_messages[].
  ENDIF.

  SORT lt_flcma BY tplnr_fl contr.
  READ TABLE lt_flcma INTO DATA(ls_flcma)
  WITH KEY   tplnr_fl = ls_cskey-tplnr_fl
             contr    = ls_cskey-contr
  BINARY SEARCH.
  IF sy-subrc EQ 0.
    ls_flcma-updkz     = gc_constants-update.
    ls_flcma-zzpoflag  = gv_poflag.
    CLEAR gv_poflag.

    ls_glcs_change-tplnr_fl  = ls_cskey-tplnr_fl.
    ls_glcs_change-contr     = ls_cskey-contr.
    ls_glcs_change-updkz     = gc_constants-update.
    ls_glcs_change-x-cshdr   = ls_flcma.
    ls_glcs_change-x-csprs   = lt_csprs.
    ls_glcs_change-x-csprso  = lt_csprso.
    ls_glcs_change-x-csprst  = lt_csprst.
    ls_glcs_change-x-csprsor = lt_csprsor.
    ls_glcs_change-x-csdfl   = lt_csdfl.

    APPEND ls_flcma TO lt_flcma_change.
    APPEND ls_glcs_change TO lt_glcs_change.

*-- Update the crop season
    CALL FUNCTION '/AGRI/GLCS_CHANGE'
      EXPORTING
        it_flcma                = lt_flcma_change[]
      IMPORTING
        et_messages             = lt_messages[]
      CHANGING
        ct_csdoc                = lt_glcs_change[]
      EXCEPTIONS
        no_documents_to_process = 1
        change_failed           = 2
        crop_locked             = 3
        OTHERS                  = 4.
    IF sy-subrc NE 0.
      LOOP AT lt_messages INTO DATA(ls_message).
        CLEAR : ls_mesg.
        ls_mesg = CORRESPONDING #( ls_message ).
        APPEND ls_mesg TO ct_messages.
      ENDLOOP.
    ELSE.
*-- Build messages
      lv_msgv1 = ps_indata-tplnr.
      lv_msgv2 = ls_cskey-contr.
      PERFORM build_msgs USING gc_constants-msgty_success '061'
                               lv_msgv1 lv_msgv2
                      CHANGING ct_messages[].
    ENDIF.
  ENDIF.

ENDFORM.
