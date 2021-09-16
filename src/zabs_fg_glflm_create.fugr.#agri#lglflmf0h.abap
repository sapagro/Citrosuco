*----------------------------------------------------------------------*
***INCLUDE /AGRI/LGLFLMF0H .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  HEADER_DATA_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM header_data_check .

  DATA: lv_subrc TYPE sy-subrc,
        lv_tplnr TYPE /agri/gltplnr_fl,
        lv_msgv1 TYPE sy-msgv1.

*  STATICS: lwa_iflos_min TYPE ilox_iflos_min.

  IF /agri/s_glflot-strno IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
      EXPORTING
        input      = /agri/s_glflot-strno
*     IMPORTING
*       OUTPUT     =
      EXCEPTIONS
        not_found  = 1
        not_active = 2
        OTHERS     = 3.
    IF sy-subrc NE 1.
      MESSAGE e005(/agri/glfl) WITH /agri/s_glflot-strno
                               INTO sy-msgli.
      message_simple space.
      gs_variables-errors = c_true.
      EXIT.
    ENDIF.

*
*    IF gs_variables-labeling_active IS INITIAL.
*
*      SELECT SINGLE tplnr
*               FROM iflot
*               INTO lv_tplnr
*              WHERE tplnr = /agri/s_glflot-tplnr_fl.
*      IF sy-subrc = 0.
*        MESSAGE e005(/agri/glfl) WITH /agri/s_glflot-tplnr_fl
*                                 INTO sy-msgli.
*        message_simple space.
*        gs_variables-errors = c_true.
*        EXIT.
*      ENDIF.
*
*    ELSE.
*
*      IF lwa_iflos_min-strno NE /agri/s_glflot-tplnr_fl.
*
*        PERFORM label_infocus_unlock USING lwa_iflos_min-strno.
*
*        CLEAR lwa_iflos_min.
*        lwa_iflos_min-alkey = gs_variables-alkey.
*        lwa_iflos_min-strno = /agri/s_glflot-tplnr_fl.
*        lwa_iflos_min-tplkz = /agri/s_glflot-tplkz.
*        lwa_iflos_min-prkey = c_true.
*
**--alternate labeling
*        CALL FUNCTION 'ILOX_IFLOS_EXISTENCE_CHECK'
*          EXPORTING
*            i_iflos_min                   = lwa_iflos_min
*            i_flg_check_tplkz             = space
**           I_FLG_MSG                     = 'X'
**     IMPORTING
**           E_FLG_OK                      =
*          EXCEPTIONS
*            error_in_structured_key       = 1
*            creation_in_same_session      = 2
*            creation_by_other_user        = 3
*            new_strno_in_historic_version = 4
*            new_strno_in_same_alkey       = 5
*            new_strno_in_other_alkey      = 6
*            alkey_does_not_exist          = 7
*            enqueue_failure               = 8
*            new_strno_exists              = 9
*            OTHERS                        = 10.
*        IF sy-subrc <> 0.
*          CLEAR lwa_iflos_min.
*          MESSAGE ID sy-msgid TYPE c_msg_type-error NUMBER sy-msgno
*             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
*             INTO sy-msgli.
*          message_simple space.
*        ENDIF.
*
*      ENDIF.
*
*  ENDIF.

  ENDIF.

  CHECK ok_code NE c_fcode-enter.

  IF /agri/s_glflot-strno IS INITIAL.
    MESSAGE e001(/agri/glfl) INTO sy-msgli.
    message_simple space.
    gs_variables-errors = c_true.
    EXIT.
  ENDIF.

  PERFORM get_funloc_level USING /agri/s_glflot-strno
                                 /agri/s_glflscrfields-stufm
                        CHANGING /agri/s_glflot-tplvl
                                 lv_subrc.
  IF lv_subrc NE 0.
    EXIT.
  ENDIF.

  PERFORM get_superior_funloc USING /agri/s_glflot-strno
                                    /agri/s_glflscrfields-stufm
                           CHANGING /agri/s_glflot-tplma
                                    lv_subrc.
  IF lv_subrc NE 0.
    EXIT.
  ENDIF.

  PERFORM check_editmask USING "lwa_iflos_min-alkey
                               /agri/s_glflot-tplkz
                      CHANGING /agri/s_glflot-strno
                               lv_subrc.
  IF lv_subrc NE 0.
    EXIT.
  ENDIF.

ENDFORM.                    " HEADER_DATA_CHECK
*&---------------------------------------------------------------------*
*&      Form  HEADER_DATA_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM header_data_display .

  CLEAR: /agri/s_glflot, */agri/s_glflot, /agri/s_glflscrfields.
  MOVE-CORRESPONDING gs_fldoc_infocus-x-flhdr TO /agri/s_glflot.
  MOVE-CORRESPONDING gs_fldoc_infocus-x-flhdr TO */agri/s_glflot.

ENDFORM.                    " HEADER_DATA_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  HEADER_DATA_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM header_data_update .

  DATA: ls_flhdr TYPE /agri/s_glflot.

  CHECK gs_variables-document_mode NE c_mode_display
    AND gs_variables-document_mode NE space.

  MOVE-CORRESPONDING gs_fldoc_infocus-x-flhdr TO ls_flhdr.

  IF ls_flhdr NE /agri/s_glflot.

    MOVE-CORRESPONDING /agri/s_glflot TO gs_fldoc_infocus-x-flhdr.
    IF gs_fldoc_infocus-x-flhdr-updkz NE c_updkz_new.
      gs_fldoc_infocus-x-flhdr-updkz = c_updkz_update.
    ENDIF.
    gs_variables-data_changed = c_true.
    gs_fldoc_infocus-tplnr_fl = gs_fldoc_infocus-x-flhdr-tplnr_fl.

  ENDIF.

ENDFORM.                    " HEADER_DATA_UPDATE
*&---------------------------------------------------------------------*
*&      Form  HEADER_MASS_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM header_mass_process  TABLES lt_terrains TYPE /agri/t_glflot_wl
                          USING  lv_mode.

  DATA: lt_tglfllvl TYPE STANDARD TABLE OF /agri/tglfllvl,
        lt_glflot   TYPE STANDARD TABLE OF /agri/glflot.

  CHECK lt_terrains[] IS NOT INITIAL.

  SELECT tplkz tplvl hdrstr FROM /agri/tglfllvl
    INTO CORRESPONDING FIELDS OF TABLE lt_tglfllvl
     FOR ALL ENTRIES IN lt_terrains
   WHERE tplkz EQ lt_terrains-tplkz
     AND tplvl EQ lt_terrains-tplvl.

  SELECT DISTINCT tplkz tplvl FROM /agri/glflot
    INTO CORRESPONDING FIELDS OF TABLE lt_glflot
     FOR ALL ENTRIES IN lt_terrains
   WHERE tplnr_fl EQ lt_terrains-tplnr_fl.

*  PERFORM structure_fields_get TABLES lt_additional_fields
*                                USING lv_user_structure.
ENDFORM.                    " HEADER_MASS_PROCESS

*&---------------------------------------------------------------------*
*&      Form  header_partner_update
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM header_partner_update.
****Release 60E - Address Match

  DATA: lwa_flptr  TYPE /agri/s_glihpa,
        lwa_xflptr TYPE /agri/s_glihpa.

  DATA: lwa_vtcom LIKE vtcom.

****Release 60E - Material and Partner determination changes
  DATA: lv_tabix        TYPE i,
        lv_subrc        LIKE sy-subrc,
        lv_nrart        TYPE nrart,
        lv_partner_chng,
        lv_partner_upd,
        lv_material_det.

  FIELD-SYMBOLS: <lwa_flptr> TYPE /agri/s_glihpa.

  RANGES: lr_ktonr FOR kna1-kunnr.

****Release E Access Framework
*  DATA: lt_qualifier TYPE /irm/t_gafqlfvl,
*        lt_error_fields TYPE /irm/t_gaffields,
*        lt_rtmessage TYPE /irm/t_gafrtmsg,
*        lt_setvalue TYPE /irm/t_gafrtsvl,
*        lt_setstatus TYPE /irm/t_gafrtsst,
*        lt_unsetstatus TYPE /irm/t_gafrtsst,
*        lt_unsetvalue TYPE /irm/t_gafrtsvl,
*        lwa_qualifier TYPE /irm/s_gafqlfvl,
*        lwa_critm TYPE /irm/s_gcritm,
*        lwa_cried TYPE /irm/s_gcried,
*        lv_posnr TYPE /irm/gcritem.
*
*  FIELD-SYMBOLS: <lwa_critm> TYPE /irm/s_gcritm,
*                 <lwa_cried> TYPE /irm/s_gcried.
*****

****60E SP1 - Partner maintenance issues
*  LOOP AT gt_xflptr_chng INTO lwa_flptr WHERE updkz IS NOT INITIAL.
*
*    READ TABLE gs_fldoc_infocus-x-ihpa
*    ASSIGNING <lwa_flptr>
*    WITH KEY posnr = lwa_flptr-posnr
*             parvw = lwa_flptr-parvw
*    BINARY SEARCH.
*
*    IF sy-subrc EQ 0
*   AND <lwa_flptr> NE lwa_flptr.
*
*      <lwa_flptr> = lwa_flptr.
*      lv_partner_chng = lv_partner_upd = c_true.
*
*    ELSEIF sy-subrc EQ 8.
*
*      APPEND lwa_flptr TO gs_fldoc_infocus-x-ihpa.
*      lv_partner_chng = lv_partner_upd = c_true.
*
*    ELSEIF sy-subrc EQ 4.
*
*      INSERT lwa_flptr INTO gs_fldoc_infocus-x-ihpa INDEX sy-tabix.
*      lv_partner_chng = lv_partner_upd = c_true.
*
*    ENDIF.
*
*    READ TABLE gt_ptr_prior_mat
*    TRANSPORTING NO FIELDS
*    WITH KEY parvw = lwa_flptr-parvw.
*
*    IF sy-subrc EQ 0
*   AND lv_partner_chng EQ c_true.
*
*      lv_material_det = c_true.
*    ENDIF.
*
*    UNASSIGN <lwa_flptr>.
*    CLEAR lv_partner_chng.
*  ENDLOOP.
****

****Release 60E Issue while deleting new entry
*  LOOP AT gs_fldoc_infocus-x-ihpa INTO lwa_flptr
*   WHERE updkz EQ c_updkz_new.
*
*    lv_tabix = sy-tabix.
*
*    READ TABLE gt_xflptr_chng
*    TRANSPORTING NO FIELDS
*    WITH KEY posnr = lwa_flptr-posnr
*             parvw = lwa_flptr-parvw
*             BINARY SEARCH.
*
*    IF sy-subrc NE 0.
*      DELETE gs_fldoc_infocus-x-ihpa INDEX lv_tabix.
*    ENDIF.
*  ENDLOOP.
****
****Release 60E_SP1 Partner Address Match
  CHECK gs_variables-partners_changed EQ c_true.

  LOOP AT gt_yflptr_chng INTO lwa_flptr WHERE updkz EQ c_updkz_delete.

*    READ TABLE gs_fldoc_infocus-y-crptr
*    ASSIGNING <lwa_flptr>
*    WITH KEY posnr = lwa_flptr-posnr
*             parvw = lwa_flptr-parvw
*    BINARY SEARCH.

*    IF sy-subrc EQ 0.
*      <lwa_flptr> = lwa_flptr.
    lv_partner_upd = c_true.
    READ TABLE gs_fldoc_infocus-x-ihpa
    INTO lwa_xflptr
    WITH KEY posnr = lwa_flptr-posnr
             parvw = lwa_flptr-parvw
    BINARY SEARCH.

    IF sy-subrc EQ 0.
      DELETE gs_fldoc_infocus-x-ihpa INDEX sy-tabix.

      IF lwa_xflptr-updkz NE c_updkz_new.

        READ TABLE gs_fldoc_infocus-y-ihpa
        ASSIGNING <lwa_flptr>
        WITH KEY posnr = lwa_flptr-posnr
                 parvw = lwa_flptr-parvw
        BINARY SEARCH.

        IF sy-subrc EQ 0.
          <lwa_flptr> = lwa_flptr.
****60E SP1 - Partner maintenance issues
*        ELSEIF sy-subrc EQ 8.
*          APPEND lwa_flptr TO gs_fldoc_infocus-y-crptr.
*        ELSEIF sy-subrc EQ 4.
*          INSERT lwa_flptr INTO gs_fldoc_infocus-y-crptr INDEX sy-tabix.
****
        ENDIF.
      ENDIF.

*      READ TABLE gt_ptr_prior_mat
*      TRANSPORTING NO FIELDS
*      WITH KEY parvw = lwa_flptr-parvw.
*
*      IF sy-subrc EQ 0.
*        lv_material_det = c_true.
*      ENDIF.
    ENDIF.
  ENDLOOP.

****60E SP1 - Partner maintenance issues
  LOOP AT gt_xflptr_chng INTO lwa_flptr WHERE updkz IS NOT INITIAL.

    READ TABLE gs_fldoc_infocus-x-ihpa
    ASSIGNING <lwa_flptr>
    WITH KEY posnr = lwa_flptr-posnr
             parvw = lwa_flptr-parvw
    BINARY SEARCH.

    IF sy-subrc EQ 0
   AND <lwa_flptr> NE lwa_flptr.
      <lwa_flptr> = lwa_flptr.
      lv_partner_chng = lv_partner_upd = c_true.
    ELSEIF sy-subrc EQ 8.
      APPEND lwa_flptr TO gs_fldoc_infocus-x-ihpa.
      lv_partner_chng = lv_partner_upd = c_true.
    ELSEIF sy-subrc EQ 4.
      INSERT lwa_flptr INTO gs_fldoc_infocus-x-ihpa INDEX sy-tabix.
      lv_partner_chng = lv_partner_upd = c_true.
    ENDIF.

*    READ TABLE gt_ptr_prior_mat
*    TRANSPORTING NO FIELDS
*    WITH KEY parvw = lwa_flptr-parvw.
*
*    IF sy-subrc EQ 0
*   AND lv_partner_chng EQ c_true.
*      lv_material_det = c_true.
*    ENDIF.

    UNASSIGN <lwa_flptr>.
    CLEAR lv_partner_chng.
  ENDLOOP.
****

****60E SP1- Issue while deleting new entry
****ESP5 Bug #28822 ( QA - 8654 ) - Partner is not getting deleted from items details in validation
*  IF NOT gt_xflptr_chng[] IS INITIAL.
  IF NOT gt_xflptr_chng[] IS INITIAL
    AND ok_code EQ 'PSLO'.
****
    LOOP AT gs_fldoc_infocus-x-ihpa INTO lwa_flptr.

      IF lwa_flptr-posnr NE c_posnr.
        EXIT.
      ENDIF.
      CHECK lwa_flptr-updkz EQ c_updkz_new.

      lv_tabix = sy-tabix.
      READ TABLE gt_xflptr_chng
      TRANSPORTING NO FIELDS
      WITH KEY posnr = lwa_flptr-posnr
               parvw = lwa_flptr-parvw
               BINARY SEARCH.
      IF sy-subrc NE 0.
        READ TABLE gt_yflptr_chng
        TRANSPORTING NO FIELDS
        WITH KEY posnr = lwa_flptr-posnr
                 parvw = lwa_flptr-parvw
                 BINARY SEARCH.
        IF sy-subrc NE 0.
          DELETE gs_fldoc_infocus-x-ihpa INDEX lv_tabix.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
****

*  IF lv_material_det EQ c_true.
*    gs_variables-redetermine_items = c_true.
*  ENDIF.

****Release 60E Restriction

*    LOOP AT gt_xflptr_chng INTO lwa_flptr WHERE posnr NE c_posnr.
*      lv_posnr = lwa_flptr-posnr.
*      EXIT.
*    ENDLOOP.

*  IF NOT gs_fldoc_infocus-x-flhdr-crtyp IS INITIAL
*  AND lv_partner_upd EQ c_true.
*    lwa_qualifier-qualf = c_qualifier-claim_type.
*    lwa_qualifier-value = gs_fldoc_infocus-x-flhdr-crtyp.
*    APPEND lwa_qualifier TO lt_qualifier.
*
**      READ TABLE gs_fldoc_infocus-x-critm
**      ASSIGNING <lwa_critm>
**      WITH KEY posnr = lv_posnr.
**      IF <lwa_critm> IS ASSIGNED.
**        lwa_critm = <lwa_critm>.
**      ENDIF.
**
**      READ TABLE gs_fldoc_infocus-x-cried
**      ASSIGNING <lwa_cried>
**      WITH KEY posnr = lv_posnr.
**      IF <lwa_cried> IS ASSIGNED.
**        lwa_cried = <lwa_cried>.
**      ENDIF.
*
*    PERFORM restrictions_evaluate USING lt_qualifier
*                                        gs_fldoc_infocus-x-flhdr
*                                        lwa_critm
*                                        lwa_cried
*                                        gt_xflptr_chng
*                                        'P'
*                               CHANGING lt_error_fields
*                                        lt_rtmessage
*                                        lt_setvalue
*                                        lt_setstatus
*                                        lt_unsetvalue
*                                        lt_unsetstatus.
*
*    PERFORM restriction_action_set USING lt_rtmessage
*                                         lt_setvalue
*                                         lt_setstatus
*                                         lt_unsetvalue
*                                         lt_unsetstatus
*                                CHANGING gs_fldoc_infocus-x-flhdr
*                                         lwa_critm
*                                         lwa_cried
*                                         gt_xflptr_chng
*                                         lv_subrc.
*
*  ENDIF.

  REFRESH:  gt_xflptr_chng,
            gt_yflptr_chng.
****
  READ TABLE gs_fldoc_infocus-x-ihpa INTO lwa_flptr
                                      WITH KEY parvw = 'RG'.
  IF sy-subrc EQ 0.
****Release 60D_SP2 - ECIP# 2576 - Partner not present
****in Sales are being accepted
*    IF gs_fldoc_infocus-x-flhdr-kunrg NE lwa_flptr-kunnr.
*      MOVE-CORRESPONDING gs_fldoc_infocus-x-flhdr TO lwa_vtcom.
*      lwa_vtcom-aland = gs_fldoc_infocus-x-flhdr-landtx.
*      lwa_vtcom-noablad = c_true.
*      lwa_vtcom-kunnr = lwa_flptr-kunnr.
*
*      IF NOT lwa_vtcom-kunnr IS INITIAL.
*****Release 60E-SO
**        CALL FUNCTION 'VIEW_KURGV'
**          EXPORTING
**            comwa      = lwa_vtcom
**          IMPORTING
**            rgwa       = gwa_kurgv
**          EXCEPTIONS
**            no_kna1    = 1
**            no_knkk    = 2
**            no_knvv    = 3
**            no_tvta    = 4
**            no_tpakd   = 5
**            no_address = 6
**            OTHERS     = 7.
*        CALL FUNCTION '/AGRI/GL_VIEW_KURGV'
*          EXPORTING
*            is_comwa   = lwa_vtcom
*          IMPORTING
*            es_rgwa    = gwa_kurgv
*          EXCEPTIONS ##FM_SUBRC_OK
*            no_kna1    = 1
*            no_knkk    = 2
*            no_knvv    = 3
*            no_tvta    = 4
*            no_tpakd   = 5
*            no_address = 6
*            OTHERS     = 7.
*****
*        IF NOT sy-msgid IS INITIAL
*       AND NOT sy-msgty IS INITIAL
*       AND NOT sy-msgno IS INITIAL
*       AND NOT sy-subrc IS INITIAL.
*          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
*          message_simple space.
*****Stop Processing during Function Module
*          IF sy-msgty EQ c_msg_type-error.
*            EXIT.
*          ENDIF.
*        ENDIF.
*
**        gs_fldoc_infocus-x-flhdr-kunrg = lwa_flptr-kunnr.
*      ENDIF.
*    ENDIF.
  ENDIF.

****Release 60E_SP1 Partner Address Match
  gs_variables-data_changed = c_true.
  CLEAR gs_variables-partners_changed.
****
ENDFORM.                    " header_partner_update
