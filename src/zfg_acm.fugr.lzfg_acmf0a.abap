*&---------------------------------------------------------------------*
*& Include          LZFG_ACMF0A
*&---------------------------------------------------------------------*
FORM authority_check USING lwa_achdr TYPE /agri/s_fmachdr
                           lv_activity
                           lv_display_message
                  CHANGING lv_subrc.

*--Call crop master type authority check.

  PERFORM badi_authority_check USING lwa_achdr
                                     lv_activity
                            CHANGING lv_subrc.

ENDFORM.                    " AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  ACCOM_DATA_DISPLAY
*&---------------------------------------------------------------------*
FORM accom_data_display .
  DATA: lwa_header TYPE /agri/s_fmachdr,
        lv_subrc   TYPE sy-subrc.

  gs_variables-refresh_worklist = c_true.

  SORT gt_search_header BY accom.
  DELETE ADJACENT DUPLICATES FROM gt_search_header COMPARING accom.

  DESCRIBE TABLE gt_search_header LINES gs_variables-wl_srch_count.

  IF gs_variables-wl_srch_count EQ 1 AND gs_acdoc_infocus IS INITIAL.
    READ TABLE gt_search_header INTO lwa_header INDEX 1.
    PERFORM document_infocus_set USING lwa_header-accom
                              CHANGING lv_subrc.
  ENDIF.
ENDFORM.                    " ACCOM_DATA_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  ACCOM_CREATE
*&---------------------------------------------------------------------*
FORM accom_create.
  PERFORM document_data_initialize USING c_true.
  gs_variables-document_mode = c_mode_create.

  CLEAR: gs_tfmactyp, /agri/s_fmachdr.

  CALL SELECTION-SCREEN 010 STARTING AT 5 5.
*  CALL SCREEN 201 STARTING AT 5 5.
  PERFORM fcode_processing.

  CHECK /agri/s_fmachdr-actyp IS NOT INITIAL.

  PERFORM document_infocus_prepare.
ENDFORM.                    " ACCOM_CREATE
*&---------------------------------------------------------------------*
*&      Form  ACCOM_TYPE_CONTROL_READ
*&---------------------------------------------------------------------*
FORM accom_type_control_read
                  USING lv_actyp TYPE /agri/fmactyp.

  DATA: lv_txtgr   TYPE txtgr,
        ls_fmactyp TYPE /agri/tfmactyp.
  CHECK gs_variables-actyp_in_focus NE lv_actyp.

  SELECT SINGLE *
    FROM /agri/tfmactyp
     INTO gs_tfmactyp
    WHERE actyp = lv_actyp.

  IF sy-subrc NE 0.
    MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                            NUMBER '032' WITH lv_actyp
                            INTO sy-msgli.
    message_simple space.
    gs_variables-errors = c_true.
    EXIT.
  ENDIF.
*****Additional Data
  REFRESH gt_additional_data.
  PERFORM acco_additional_fields_prepare.
  gs_variables-user_structure = gs_tfmactyp-hdrstr.

  CLEAR: gs_tabstrip_captions.
  PERFORM tabstrip_build.

  gs_variables-actyp_in_focus =  lv_actyp.

ENDFORM.                    " ACCOM_TYPE_CONTROL_READ
*&---------------------------------------------------------------------*
*&      Form  AUFNR_INFOCUS_READ
*&---------------------------------------------------------------------*
FORM aufnr_infocus_read USING    lt_aufnr  TYPE /agri/t_fmaufnr
                        CHANGING lt_fpdoc  TYPE /agri/t_fmfp_doc.
  CALL FUNCTION '/AGRI/FMFP_VIEW'
    EXPORTING
      it_aufnr       = lt_aufnr
    IMPORTING
      et_fpdoc       = lt_fpdoc
    EXCEPTIONS ##FM_SUBRC_OK
      no_data_exists = 1
      OTHERS         = 2.

ENDFORM.                    " AUFNR_INFOCUS_READ
*&---------------------------------------------------------------------*
*&      Form  ADMIN_DATA_MAINTAIN
*&---------------------------------------------------------------------*
FORM admin_data_maintain .
  DATA: lv_subobj       LIKE dd03p-fieldname,
        ls_screenfields TYPE /agri/gadminscrfields.

****Header Admin Data
  MOVE-CORRESPONDING gs_acdoc_infocus-x-achdr TO ls_screenfields.
  CALL FUNCTION '/AGRI/GADMIN_SUBSCREEN_IMPORT'
    EXPORTING
      i_objtyp              = c_object-bor
      i_objkey              = gs_variables-accom_in_focus
      i_subobj              = lv_subobj
      is_scrfields          = ls_screenfields
      i_suppress_activities = c_true
      i_suppress_notes      = c_false
    CHANGING
****Release 60D_SP1 - SCI and PQ Issues
      c_program             = gs_variables-admin_program
      c_subscreen           = gs_variables-subscr_admin.
****
ENDFORM.                    " ADMIN_DATA_MAINTAIN
*&---------------------------------------------------------------------*
*&      Form  AC_DESC_PREPARE
*&---------------------------------------------------------------------*
FORM ac_desc_prepare.

  DATA: lt_acdesc         TYPE /agri/t_fmachdrt,
        lwa_acdesc        TYPE /agri/s_fmachdrt,
        lwa_acdesc_layout TYPE /agri/s_fmachdrt_layout_fcat.

  CLEAR: gt_ac_desc_layout.

  lt_acdesc = gs_acdoc_infocus-x-acdes.

  LOOP AT lt_acdesc INTO lwa_acdesc.
    MOVE-CORRESPONDING lwa_acdesc TO lwa_acdesc_layout.
    PERFORM desc_styles_prepare CHANGING lwa_acdesc_layout.
    APPEND lwa_acdesc_layout TO gt_ac_desc_layout.
  ENDLOOP.

ENDFORM.                    " AC_DESC_PREPARE
*&---------------------------------------------------------------------*
*&      Form  additional_data_fields_get
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LV_USER_STRUCTURE  text
*----------------------------------------------------------------------*
FORM additional_data_fields_get  USING lv_user_structure.

  DATA: ls_additional_data TYPE /agri/s_abgl_user_scrfields.
  DATA: lt_dd03p TYPE TABLE OF dd03p,
        ls_dd03p TYPE dd03p.

  REFRESH gt_additional_data.

  CHECK NOT lv_user_structure IS INITIAL.

  CALL FUNCTION 'DDIF_TABL_GET'
    EXPORTING
      name          = lv_user_structure
*     STATE         = 'A'
      langu         = sy-langu
    TABLES
      dd03p_tab     = lt_dd03p
    EXCEPTIONS ##FM_SUBRC_OK
      illegal_input = 1
      OTHERS        = 2.

  LOOP AT lt_dd03p INTO ls_dd03p.
    CLEAR ls_additional_data.
    ls_additional_data-fieldname = ls_dd03p-fieldname.
    IF NOT ls_dd03p-scrtext_m IS INITIAL.
      ls_additional_data-fieldtxt = ls_dd03p-scrtext_m.
    ELSEIF NOT ls_dd03p-scrtext_l IS INITIAL.
      ls_additional_data-fieldtxt = ls_dd03p-scrtext_l.
    ELSEIF NOT ls_dd03p-scrtext_s IS INITIAL.
      ls_additional_data-fieldtxt = ls_dd03p-scrtext_s.
    ELSEIF NOT ls_dd03p-reptext IS INITIAL.
      ls_additional_data-fieldtxt = ls_dd03p-reptext.
    ELSE.
      ls_additional_data-fieldtxt = ls_dd03p-fieldname.
    ENDIF.
    ls_additional_data-fieldtyp = ls_dd03p-inttype.
    ls_additional_data-outputlen = ls_dd03p-outputlen.
    ls_additional_data-convexit = ls_dd03p-convexit.
***Single Field
    ls_additional_data-tabname = lv_user_structure.
    APPEND ls_additional_data TO gt_additional_data.
  ENDLOOP.

  DELETE gt_additional_data WHERE fieldname EQ '.INCLUDE'
                               OR fieldname EQ '.APPEND'
                               OR fieldname CS 'DUMMY'
                               OR fieldname CS '.INCLU'.


ENDFORM.                    " ADDITIONAL_DATA_FIELDS_GET
*&---------------------------------------------------------------------*
*&      Form  crop_additional_fields_prepare
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM acco_additional_fields_prepare .

  DATA: lv_user_structure  TYPE ddobjname VALUE '/AGRI/S_FMACHDRCA',
        lt_customer_fields TYPE TABLE OF /agri/s_abgl_user_scrfields,
        lt_cstru_fields    TYPE TABLE OF /agri/s_abgl_user_scrfields,
        ls_cstru_field     TYPE /agri/s_abgl_user_scrfields,
        lt_additional_data TYPE TABLE OF /agri/s_abgl_user_scrfields.

  CHECK gs_tfmactyp-hdrstr IS NOT INITIAL.

  PERFORM additional_data_fields_get USING lv_user_structure.
  lt_customer_fields = gt_additional_data.

  PERFORM additional_data_fields_get USING gs_tfmactyp-hdrstr.
  lt_cstru_fields = gt_additional_data.
  REFRESH gt_additional_data.

  LOOP AT lt_cstru_fields INTO ls_cstru_field.
    READ TABLE lt_customer_fields
                WITH KEY fieldname = ls_cstru_field-fieldname
                  TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0.
      APPEND ls_cstru_field TO gt_additional_data.
    ENDIF.
  ENDLOOP.

  IF gt_additional_data IS INITIAL.
    CLEAR gs_tfmactyp-hdrstr.
  ENDIF.

ENDFORM.                    " ACCOM_ADDITIONAL_FIELDS_PREPARE
*&---------------------------------------------------------------------*
*&      Form  AUFNR_DATA_CHECK
*&---------------------------------------------------------------------*
FORM aufnr_data_check.

  DATA: lt_wo       TYPE /agri/t_fmac_wo,
        lt_fp       TYPE /agri/t_fmac_fp,
        lt_cmprs    TYPE /agri/t_fmac_cmprs,
        lt_fptmp    TYPE /agri/t_fmac_fp,
        lt_fpitm    TYPE /agri/t_fmac_fpitm,
        lwa_fp      TYPE /agri/s_fmac_fp,
        lwa_aufnr   LIKE LINE OF gr_aufnr[],
        lt_terrain  TYPE STANDARD TABLE OF /agri/glflcma,
        lwa_terrain TYPE /agri/glflcma,
        lt_order    TYPE STANDARD TABLE OF /agri/fmfphdr,
        lwa_order   TYPE /agri/fmfphdr.

  RANGES: lr_aufnrtmp   FOR aufk-aufnr.

  FIELD-SYMBOLS: <lwa_wo>    TYPE /agri/s_fmac_wo,
                 <lwa_fpitm> TYPE /agri/s_fmac_fpitm,
                 <lwa_cmprs> TYPE /agri/s_fmac_cmprs.

* get the terrain & counter details.
*
*  SELECT tplnr_fl contr FROM /agri/glflcma INTO CORRESPONDING FIELDS OF TABLE lt_terrain
*                                                        WHERE tplnr_fl IN so_tplnr
*                                                        AND   datab <= p_strdat
*                                                        AND   datbi >= p_findat
*                                                        AND   astat = 'A'
*                                                        AND   loevm = space.
*if sy-subrc = 0.
*SELECT aufnr auart tplnr_fl contr matnr FROM /agri/fmfphdr INTO CORRESPONDING FIELDS OF TABLE lt_order
*                                                          FOR ALL ENTRIES IN lt_terrain
*                                                            WHERE aufnr in so_aufnr
*                                                             and  tplnr_fl = lt_terrain-tplnr_fl
*                                                              AND contr    = lt_terrain-contr
*                                                              AND matnr   = p_mat.
*
*endif.
* get the Order details based on terrain & counter & material.


      IF gr_aufnrtmp[] IS NOT INITIAL.
        lr_aufnrtmp[] = gr_aufnrtmp[].
      ELSE.
        lr_aufnrtmp[] = gr_aufnr[].
*    REFRESH gr_aufnr.
      ENDIF.

      IF lr_aufnrtmp[] IS NOT INITIAL.

        SELECT aufnr tplnr_fl cmnum varia cpros matnr
          FROM /agri/fmfphdr
          INTO TABLE lt_fp
          WHERE aufnr IN lr_aufnrtmp[].

        IF lt_fp IS NOT INITIAL.
          LOOP AT lr_aufnrtmp[] INTO lwa_aufnr.
            READ TABLE lt_fp INTO lwa_fp
                          WITH KEY lwa_aufnr-low.
            IF sy-subrc NE 0.
              SET CURSOR FIELD 'SO_AUFNR'.
              MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-warning
                                   NUMBER '036'
                                   WITH   lwa_aufnr-low
                                   INTO   sy-msgli.
              IF so_aufnr IS NOT INITIAL.
                CLEAR so_aufnr.
              ENDIF.
              gs_variables-errors = c_true.
              message_simple space.
            ENDIF.
          ENDLOOP.
        ELSE.
          SET CURSOR FIELD 'SO_AUFNR'.
          MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error
                                    NUMBER '034'
                                    INTO   sy-msgli.
          IF so_aufnr IS NOT INITIAL.
            CLEAR so_aufnr.
          ENDIF.
          gs_variables-errors = c_true.
          message_simple space.
        ENDIF.

        CHECK lt_fp IS NOT INITIAL.

        SELECT aufnr posnr aufnr_to
          FROM /agri/fmfpitm
          INTO TABLE lt_fpitm
          WHERE aufnr_to IN lr_aufnrtmp[].

        SORT lt_fpitm BY aufnr.
        IF lt_fpitm IS NOT INITIAL.

          SELECT aufnr tplnr_fl cmnum varia cpros matnr
             FROM /agri/fmfphdr
             INTO TABLE lt_fptmp
              FOR ALL ENTRIES IN lt_fpitm          "#EC CI_NO_TRANSFORM
             WHERE aufnr EQ lt_fpitm-aufnr.

          IF lt_fptmp IS NOT INITIAL.

            SELECT cmnum varia cpros matnr prprs
              FROM /agri/glcmprs
              INTO TABLE lt_cmprs
              FOR ALL ENTRIES IN lt_fptmp          "#EC CI_NO_TRANSFORM
              WHERE cmnum EQ lt_fptmp-cmnum
                AND varia EQ lt_fptmp-varia
                AND cpros EQ lt_fptmp-cpros
                AND matnr EQ lt_fptmp-matnr.

            SORT lt_fptmp BY cmnum varia cpros.

            IF lt_cmprs IS NOT INITIAL.
              LOOP AT lt_cmprs ASSIGNING <lwa_cmprs>.
                IF  <lwa_cmprs>-prprs EQ c_true.
                  READ TABLE lt_fptmp INTO lwa_fp
                              WITH KEY cmnum = <lwa_cmprs>-cmnum
                                       varia = <lwa_cmprs>-varia
                                       cpros = <lwa_cmprs>-cpros
                                       BINARY SEARCH.
                  MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-warning
                                          NUMBER '035'
                                          WITH   lwa_fp-aufnr
                                          INTO   sy-msgli.
                  IF so_aufnr IS NOT INITIAL.
                    CLEAR so_aufnr.
                  ENDIF.
                  gs_variables-errors = c_true.
                  message_simple space.
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDIF.
        IF lt_fp IS NOT INITIAL.

          SELECT t1~wonum t1~wotyp t1~cmnum t1~matnr t1~stort
                 t2~aufnr t2~tplnr_fl t2~contr
          INTO TABLE lt_wo
          FROM /agri/fmwochdr AS t1
          INNER JOIN /agri/fmwoitm AS t2
          ON t1~wonum = t2~wonum
          FOR ALL ENTRIES IN lt_fp
          WHERE  t2~aufnr = lt_fp-aufnr.

          IF sy-subrc EQ 0.
            LOOP AT lt_wo ASSIGNING <lwa_wo>.
              MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-warning
                                        NUMBER '033'
                                        WITH   <lwa_wo>-aufnr
                                        INTO   sy-msgli.
              IF so_aufnr IS NOT INITIAL.
                CLEAR so_aufnr.
              ENDIF.
              gs_variables-errors = c_true.
              message_simple space.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.

ENDFORM.                    " AUFNR_DATA_CHECK
*&---------------------------------------------------------------------*
*&      Form  ACCOM_TSKORD_SAVE
*&---------------------------------------------------------------------*
FORM accom_tskord_save.

  DATA: lt_fmactsk  TYPE TABLE OF /agri/fmactsk,
        lwa_fmactsk TYPE /agri/fmactsk,
        lwa_details TYPE ty_details,
        lt_aufk     TYPE TABLE OF aufk,
        lwa_aufk    TYPE aufk.

  FIELD-SYMBOLS: <lwa_aufnr>   LIKE LINE OF gr_aufnr,
                 <lwa_details> TYPE ty_details.

  CHECK gs_tfmactyp-acapp EQ c_accom_appli-aufnr.

  SELECT SINGLE * FROM /agri/fmactsk "#EC CI_ALL_FIELDS_NEEDED        "#EC CI_NOORDER
    INTO lwa_fmactsk
    WHERE accom = gs_acdoc_infocus-x-achdr-accom.

    CHECK sy-subrc NE 0.

    CLEAR lwa_fmactsk.

    MOVE-CORRESPONDING gs_acdoc_infocus-x-achdr TO lwa_fmactsk.

    IF gt_details[] IS NOT INITIAL.
      LOOP AT gt_details INTO lwa_details.
        MOVE lwa_details-aufnr TO lwa_fmactsk-aufnr.
        APPEND lwa_fmactsk TO lt_fmactsk.
      ENDLOOP.
    ELSE.
      IF gr_aufnr[] IS NOT INITIAL.

        SELECT *
        INTO TABLE lt_aufk
        FROM aufk
        WHERE aufnr IN gr_aufnr[].

          IF lt_aufk IS NOT INITIAL.
            LOOP AT lt_aufk INTO lwa_aufk.
              MOVE lwa_aufk-aufnr TO lwa_fmactsk-aufnr.
              APPEND lwa_fmactsk TO lt_fmactsk.
            ENDLOOP.
          ENDIF.
        ELSEIF so_aufnr[] IS NOT INITIAL.

          SELECT *
          INTO TABLE lt_aufk
          FROM aufk
          WHERE aufnr IN so_aufnr[].

            IF lt_aufk IS NOT INITIAL.
              LOOP AT lt_aufk INTO lwa_aufk.
                MOVE lwa_aufk-aufnr TO lwa_fmactsk-aufnr.
                APPEND lwa_fmactsk TO lt_fmactsk.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDIF.

        SORT lt_fmactsk BY aufnr.
        DELETE ADJACENT DUPLICATES FROM lt_fmactsk COMPARING accom aufnr bukrs werks.

        IF lt_fmactsk IS NOT INITIAL.
          INSERT /agri/fmactsk FROM TABLE lt_fmactsk.
        ENDIF.


ENDFORM.                    " ACCOM_TSKORD_SAVE
*&---------------------------------------------------------------------*
*&      Form  ASSIGNMENTS_GRID_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM ASSIGNMENTS_GRID_UPDATE .
*  data: lwa_cshdr      type /agri/s_glflcma,
*        lwa_flcma      type /agri/s_glflcma,
*        lwa_flot       type /agri/glflot,
*        lwa_flhdr      type /agri/s_glflot,
*        lwa_flcma_old  type /agri/s_glflcma,
**        lwa_flcma_fcat TYPE /agri/s_glflcma_fcat,
*        lwa_mod_cells type lvc_s_modi,
*        lwa_mod_row type lvc_s_modi,
*        lv_modified,
*        lv_fieldname type string,
*        lv_fieldname2 type string,
*        lv_count(10) type n,
*        lv_subrc type sy-subrc,
*        lv_valid.
*
*  field-symbols: <lv_cpros> type any,
*                 <lv_value> type any,
*                 <lv_target> type any,
*                 <lwa_csprs> type /agri/s_glcsprs,
*                 <lwa_flcma> type /agri/s_glflcma,
*                 <lwa_flcma_fcat> type any,
*                 <lwa_csdoc_infocus> type /agri/s_glcs_doc.
*
*  check gs_variables-overview_mode ne c_mode_display.
*
*  fcode = ok_code.
*  if fcode is initial.
*    fcode = sy-ucomm.
*  endif.
*  clear: gs_variables-errors.
*  if gs_variables-initiator is initial.
*    gs_variables-initiator = c_log_initiator-check.
*    perform messages_initialize using gs_variables-initiator
*                                      c_log_subobject-check.
*  endif.
*
*  if fcode eq c_fcode-asgn_delete.
*    perform fcode_asgn_delete.
*    clear ok_code.
*  endif.
*
*  if fcode eq c_fcode-asgn_undo_del.
*    perform fcode_asgn_undo_del.
*    clear ok_code.
*  endif.
*
*  lv_modified = ref_assignments_grid->data_modified_check( ).
*  if lv_modified eq c_true or
*     gs_variables-manual_changes eq c_true.
*    call method ref_assignments_grid->check_changed_data
*      importing
*        e_valid = lv_valid.
*    if lv_valid is initial.
*      clear ok_code.
*      exit.
*    else.
*      gs_variables-refresh_assignments = c_true.
*    endif.
*  endif.
*
*  loop at gt_assgn_mod_rows into lwa_mod_row.
*
*    clear: lwa_cshdr.
*    unassign: <lwa_flcma_fcat>, <lwa_csdoc_infocus>.
*    read table <gt_flcma_fcat> assigning <lwa_flcma_fcat>
*                             index lwa_mod_row-row_id.
*    check <lwa_flcma_fcat> is assigned and
*          <lwa_flcma_fcat> is not initial.
*    move-corresponding <lwa_flcma_fcat> to lwa_cshdr.
*
*    if lwa_cshdr-gcontr is not initial.
*      read table gt_csdoc_infocus assigning <lwa_csdoc_infocus>
*                       with key contr    = lwa_cshdr-gcontr.
*    else.
*      read table gt_csdoc_infocus assigning <lwa_csdoc_infocus>
*                       with key tplnr_fl = lwa_cshdr-tplnr_fl
*                                contr    = lwa_cshdr-contr.
*    endif.
*    check <lwa_csdoc_infocus> is assigned.
*
*    if lwa_cshdr-tplnr_fl ne <lwa_csdoc_infocus>-x-cshdr-tplnr_fl.
*      perform assignment_single_lock using lwa_cshdr-tplnr_fl
*                                  changing lv_subrc.
*      if lv_subrc is not initial.
*        lwa_cshdr-tplnr_fl = <lwa_csdoc_infocus>-x-cshdr-tplnr_fl.
*      endif.
*    endif.
*
*    perform assignment_update using lwa_cshdr
*                           changing <lwa_csdoc_infocus>.
*
*    loop at gt_assgn_mod_cells into lwa_mod_cells
*                              where row_id eq lwa_mod_row-row_id.
*      check lwa_mod_cells-fieldname cs 'STRDT' or
*            lwa_mod_cells-fieldname cs 'ENDDT'.
*      split lwa_mod_cells-fieldname at '_' into lv_fieldname lv_count.
*      check lv_count is not initial.
*      concatenate 'CPROS_' lv_count into lv_fieldname2.
*      assign component lv_fieldname2 of structure <lwa_flcma_fcat>
*                                              to <lv_cpros>.
*      check sy-subrc eq 0.
*      read table <lwa_csdoc_infocus>-x-csprs assigning <lwa_csprs>
*                                             with key cpros = <lv_cpros>.
*      if sy-subrc eq 0.
*        assign component lv_fieldname of structure <lwa_csprs>
*                                                to <lv_target>.
*        assign component lwa_mod_cells-fieldname
*                      of structure <lwa_flcma_fcat> to <lv_value>.
*        if <lv_target> ne <lv_value>.
*          <lv_target> = <lv_value>.
*          gs_variables-data_changed = c_true.
*          if <lwa_csprs>-updkz ne c_updkz_new.
*            <lwa_csprs>-updkz = c_updkz_update.
*          endif.
*        endif.
*      endif.
*    endloop.
*
*    delete gt_assgn_mod_rows where row_id eq lwa_mod_row-row_id.
*
*  endloop.
*
*  if gs_variables-errors eq c_true or
*     lv_subrc is not initial.
*    clear ok_code.
*  endif.
*
*  perform messages_display using gs_variables-initiator.

*ENDFORM.                    " ASSIGNMENTS_GRID_UPDATE
*&---------------------------------------------------------------------*
*&      Form  ACTIVITY_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LWA_ITEM  text
*----------------------------------------------------------------------*
FORM activity_check  USING lv_rstype

                  CHANGING lwa_item     TYPE /agri/s_fmacitm_layout
                           lt_activity  TYPE /agri/t_fmac_src_act
                           lt_fmacrsc   TYPE /agri/t_fmacrsc.

  DATA: lwa_activity    TYPE /agri/s_fmac_src_act,

        ls_items_layout TYPE /agri/s_fmacitm_layout,
        lt_return       TYPE TABLE OF ddshretval,
        lv_retfield     TYPE dfies-fieldname,
        lv_dynprofield  TYPE help_info-dynprofld,
        lv_aufpl        TYPE co_aufpl,
        lt_afvc         TYPE /agri/t_fmac_src_ord,
        ls_afvc         TYPE /agri/s_fmac_src_ord,
        lr_lstar        TYPE RANGE OF lstar,
        lwa_lstar       LIKE LINE OF lr_lstar,
        lv_crhd         TYPE crhd-vgwts,
        lt_vgwts        TYPE /agri/t_vgwts,
        ls_vgwts        TYPE /agri/s_vgwts,
        lwa_vgwts       TYPE /agri/s_vgwts,
        lt_parameters   TYPE /agri/t_parameters_tc21,
        ls_parameters   TYPE /agri/s_parameters_tc21,
        lwa_parameters  TYPE /agri/s_parameters_tc21,
        lt_tc20         TYPE /agri/t_parameters_tc20,
        lwa_tc20        TYPE tc20,
        lwa_fmachdr     TYPE /agri/s_fmachdr,
*        lt_fmacrsc       TYPE /agri/t_fmacrsc,
        lwa_fmacrsc     TYPE /agri/s_fmacrsc.

  FIELD-SYMBOLS <lwa_afvc> TYPE /agri/s_fmac_src_ord.


  SELECT SINGLE aufpl
   FROM afko
   INTO lv_aufpl
   WHERE aufnr = lwa_item-aufnr.

    SELECT SINGLE vgwts
      FROM crhd
      INTO lv_crhd
      WHERE arbpl = lwa_item-arbpl.                     "#EC CI_NOORDER

      SELECT SINGLE vgwts lar01 lar02 lar03 lar04 lar05 lar06
        INTO ls_afvc
        FROM afvc
        WHERE aufpl = lv_aufpl                          "#EC CI_NOORDER
        AND vgwts = lv_crhd.

****  14/09/2016
        SELECT SINGLE arbpl vgwts
        FROM crhd
        INTO ls_vgwts
        WHERE arbpl = lwa_item-arbpl.                   "#EC CI_NOORDER

          SELECT SINGLE vgwts par01 par02 par03 par04 par05 par06
          INTO ls_parameters
          FROM tc21
          WHERE vgwts = ls_vgwts-vgwts.                 "#EC CI_NOORDER

            SELECT parid
            INTO CORRESPONDING FIELDS OF TABLE lt_tc20
            FROM tc20
            WHERE parid = ls_parameters-par01
              OR  parid = ls_parameters-par02
              OR  parid = ls_parameters-par03
              OR  parid = ls_parameters-par04
              OR  parid = ls_parameters-par05
              OR  parid = ls_parameters-par06.

*              READ TABLE gt_search_header INTO lwa_fmachdr WITH KEY accom = gs_acdoc_infocus-x-achdr-accom."lwa_item_layout-accom.

              IF lt_tc20 IS NOT INITIAL.
                IF lv_rstype EQ c_rstype-labor.
                  SELECT actyp parid rstyp txtlg "#EC CI_FAE_LINES_ENSURED
                  FROM /agri/tfmacrsc
                  INTO TABLE lt_fmacrsc
                  FOR ALL ENTRIES IN lt_tc20
                  WHERE parid = lt_tc20-parid
                  AND rstyp = c_accom_id-employee
                  AND actyp = gs_acdoc_infocus-x-achdr-actyp.
*                  AND actyp = lwa_fmachdr-actyp.

                  ELSE.

                    SELECT actyp parid rstyp txtlg "#EC CI_FAE_LINES_ENSURED
                    FROM /agri/tfmacrsc
                    INTO TABLE lt_fmacrsc
                    FOR ALL ENTRIES IN lt_tc20
                    WHERE parid = lt_tc20-parid
                    AND rstyp = c_accom_id-equipment
                    AND actyp = gs_acdoc_infocus-x-achdr-actyp.
*                    AND actyp = lwa_fmachdr-actyp.

                    ENDIF.
                  ENDIF.


                  IF ls_afvc-vgwts EQ ls_parameters-vgwts.

                    LOOP AT lt_fmacrsc INTO lwa_fmacrsc.

                      IF lwa_fmacrsc-parid = ls_parameters-par01
                        AND ls_afvc-lar01 IS NOT INITIAL.
                        lwa_lstar-sign = c_sign-include.
                        lwa_lstar-option = c_operator_word-equalto.
                        lwa_lstar-low = ls_afvc-lar01.
                        APPEND lwa_lstar TO lr_lstar.
                      ENDIF.
                      IF lwa_fmacrsc-parid = ls_parameters-par02
                        AND ls_afvc-lar02 IS NOT INITIAL.
                        lwa_lstar-sign = c_sign-include.
                        lwa_lstar-option = c_operator_word-equalto.
                        lwa_lstar-low = ls_afvc-lar02.
                        APPEND lwa_lstar TO lr_lstar.
                      ENDIF.
                      IF lwa_fmacrsc-parid = ls_parameters-par03
                        AND ls_afvc-lar03 IS NOT INITIAL.
                        lwa_lstar-sign = c_sign-include.
                        lwa_lstar-option = c_operator_word-equalto.
                        lwa_lstar-low = ls_afvc-lar03.
                        APPEND lwa_lstar TO lr_lstar.
                      ENDIF.
                      IF lwa_fmacrsc-parid = ls_parameters-par04
                        AND ls_afvc-lar04 IS NOT INITIAL.
                        lwa_lstar-sign = c_sign-include.
                        lwa_lstar-option = c_operator_word-equalto.
                        lwa_lstar-low = ls_afvc-lar04.
                        APPEND lwa_lstar TO lr_lstar.
                      ENDIF.
                      IF lwa_fmacrsc-parid = ls_parameters-par05
                        AND ls_afvc-lar05 IS NOT INITIAL.
                        lwa_lstar-sign = c_sign-include.
                        lwa_lstar-option = c_operator_word-equalto.
                        lwa_lstar-low = ls_afvc-lar05.
                        APPEND lwa_lstar TO lr_lstar.
                      ENDIF.
                      IF lwa_fmacrsc-parid = ls_parameters-par06
                        AND ls_afvc-lar06 IS NOT INITIAL.
                        lwa_lstar-sign = c_sign-include.
                        lwa_lstar-option = c_operator_word-equalto.
                        lwa_lstar-low = ls_afvc-lar06.
                        APPEND lwa_lstar TO lr_lstar.
                      ENDIF.

                    ENDLOOP.

                  ENDIF.
*
*****
                  CLEAR: gr_lstar[], gr_lstar.
                  IF lr_lstar[] IS NOT INITIAL.
                    gr_lstar[] = lr_lstar[].
                    SELECT t1~idactv t1~rstype t1~bill t1~actype t2~description "#EC CI_BUFFJOIN
                      INTO TABLE lt_activity
                      FROM /agri/fmacact AS t1 INNER JOIN /agri/fmacactt AS t2
                      ON t1~idactv = t2~idactv
                      WHERE t1~rstype = lv_rstype
                      AND t2~spras = sy-langu
                      AND t1~actype IN lr_lstar
                      OR  t1~bill = c_no.
                    ELSE.
                      SELECT t1~idactv t1~rstype t1~bill t1~actype t2~description
                        INTO TABLE lt_activity
                        FROM /agri/fmacact AS t1 INNER JOIN /agri/fmacactt AS t2 "#EC CI_BUFFJOIN
                        ON t1~idactv = t2~idactv
                        WHERE t1~rstype = lv_rstype
                        AND t2~spras = sy-langu
                        AND  t1~bill = c_no.
                      ENDIF.


ENDFORM.                    " ACTIVITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  ACTIVITY_MEIN_CHANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LWA_ACTIVITY  text
*      <--P_LWA_ITEM  text
*----------------------------------------------------------------------*
FORM activity_mein_change  USING     lt_fmacrsc   TYPE /agri/t_fmacrsc
                           CHANGING lwa_item TYPE /agri/s_fmacitm_layout
                                     lwa_activity TYPE /agri/s_fmac_src_act.

  DATA: lv_aufpl      TYPE afko-aufpl,
        ls_afvc       TYPE /agri/s_fmac_src_ord,
        lv_auszt      TYPE auszt,
        ls_parameters TYPE /agri/s_parameters_tc21,
        ls_vgwts      TYPE /agri/s_vgwts,
        ls_lstar      LIKE LINE OF gr_lstar,
        lwa_fmacrsc   TYPE /agri/s_fmacrsc.

  CASE lwa_activity-bill.
    WHEN c_no.

      SELECT SINGLE aufpl
      FROM afko
      INTO lv_aufpl
      WHERE aufnr = lwa_item-aufnr.

        SELECT SINGLE af~vgwts af~lar01 af~lar02 af~lar03 af~lar04 af~lar05 af~lar06
        INTO CORRESPONDING FIELDS OF ls_afvc
        FROM afvc AS af INNER JOIN crhd AS cr
        ON af~arbid = cr~objid
        WHERE af~aufpl = lv_aufpl                       "#EC CI_NOORDER
        AND cr~arbpl = lwa_item-arbpl.

          SELECT SINGLE arbpl vgwts
          FROM crhd
          INTO ls_vgwts
          WHERE arbpl = lwa_item-arbpl.                 "#EC CI_NOORDER

            SELECT SINGLE vgwts par01 par02 par03 par04 par05 par06
            INTO ls_parameters
            FROM tc21
            WHERE vgwts = ls_vgwts-vgwts.               "#EC CI_NOORDER

              IF lt_fmacrsc[] IS NOT INITIAL.

                LOOP AT lt_fmacrsc INTO lwa_fmacrsc.

                  IF ls_parameters-par01 = lwa_fmacrsc-parid.
                    lwa_activity-actype = ls_afvc-lar01.
                    EXIT.                               "#EC CI_NOORDER
                  ELSEIF ls_parameters-par02 = lwa_fmacrsc-parid.
                    lwa_activity-actype = ls_afvc-lar02.
                    EXIT.                               "#EC CI_NOORDER
                  ELSEIF ls_parameters-par03 = lwa_fmacrsc-parid.
                    lwa_activity-actype = ls_afvc-lar03.
                    EXIT.                               "#EC CI_NOORDER
                  ELSEIF ls_parameters-par04 = lwa_fmacrsc-parid.
                    lwa_activity-actype = ls_afvc-lar04.
                    EXIT.                               "#EC CI_NOORDER
                  ELSEIF ls_parameters-par05 = lwa_fmacrsc-parid.
                    lwa_activity-actype = ls_afvc-lar05.
                    EXIT.                               "#EC CI_NOORDER
                  ELSEIF ls_parameters-par06 = lwa_fmacrsc-parid.
                    lwa_activity-actype = ls_afvc-lar06.
                    EXIT.                               "#EC CI_NOORDER
                  ENDIF.

                ENDLOOP.

              ELSE.

                IF ls_afvc-lar01 IS NOT INITIAL.
                  lwa_activity-actype = ls_afvc-lar01.
                ELSEIF ls_afvc-lar02 IS NOT INITIAL.
                  lwa_activity-actype = ls_afvc-lar02.
                ELSEIF ls_afvc-lar03 IS NOT INITIAL.
                  lwa_activity-actype = ls_afvc-lar03.
                ELSEIF ls_afvc-lar04 IS NOT INITIAL.
                  lwa_activity-actype = ls_afvc-lar04.
                ELSEIF ls_afvc-lar05 IS NOT INITIAL.
                  lwa_activity-actype = ls_afvc-lar05.
                ELSEIF ls_afvc-lar06 IS NOT INITIAL.
                  lwa_activity-actype = ls_afvc-lar06.
                ENDIF.
              ENDIF.

              SELECT SINGLE leinh FROM csla INTO lwa_item-qmein "#EC CI_GENBUFF
                WHERE lstar = lwa_activity-actype       "#EC CI_NOORDER
                  AND datbi >= gs_acdoc_infocus-x-achdr-strtdat
                  AND datab <= gs_acdoc_infocus-x-achdr-findat.

              WHEN OTHERS.
                SELECT SINGLE leinh FROM csla INTO lwa_item-qmein "#EC CI_GENBUFF
                  WHERE lstar = lwa_activity-actype     "#EC CI_NOORDER
                  AND datbi >= gs_acdoc_infocus-x-achdr-strtdat
                  AND datab <= gs_acdoc_infocus-x-achdr-findat.
              ENDCASE.
              CHECK lwa_item-qmein IS NOT INITIAL
                  AND lwa_item-meins IS NOT INITIAL
                  AND lwa_item-duran IS NOT INITIAL.

              IF lwa_item-qmein EQ 'S'
                 OR lwa_item-qmein EQ 'MIN'
                 OR lwa_item-qmein EQ 'H'.

                PERFORM time_convert USING    lwa_item-qmein
                                              lwa_item-meins
                                              lwa_item-duran
                                     CHANGING lv_auszt.
                IF lv_auszt IS NOT INITIAL.
                  lwa_item-menge = lv_auszt.
                ENDIF.
              ELSE.
                IF lwa_item-menge IS NOT INITIAL.
                  CLEAR lwa_item-menge.
                ENDIF.
              ENDIF.


ENDFORM.                    " ACTIVITY_MEIN_CHANGE
*&---------------------------------------------------------------------*
*&      Form  AGREEMENTS_PO_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ACITM[]  text
*      <--P_LT_PURCHS  text
*----------------------------------------------------------------------*
FORM agreements_po_confirm.

  DATA: lt_toagroup    TYPE /agri/t_fmacoll,
        lv_commit_work,
        lv_subrc       TYPE sy-subrc.

  PERFORM document_agreement_get CHANGING lt_toagroup[].
*---
  CHECK lt_toagroup IS NOT INITIAL.
  MOVE c_true TO lv_commit_work.
  PERFORM good_receipt_create USING    lv_commit_work
                              CHANGING lv_subrc
                                       lt_toagroup[].
  IF lv_subrc NE 0.
    EXIT.
  ELSE.
    CLEAR: lv_commit_work, lv_subrc.
    PERFORM good_receipt_create USING
             lv_commit_work
    CHANGING lv_subrc
      lt_toagroup[].
  ENDIF.

ENDFORM.                    "agreements_po_confirm
*&---------------------------------------------------------------------*
*&      Form  AGREEMENT_NUMBER_GET
*&---------------------------------------------------------------------*
*       text
*-
*---------------------------------------------------------------------*
*      -->P_LT_AGREEM  text
*      <--P_LT_AGRNUM  text
*----------------------------------------------------------------------*
FORM agreement_number_get  USING  lt_agreem   TYPE /agri/t_fmacagr
                           CHANGING lt_agrnum TYPE /agri/t_fmknuma_ag
                                    lv_subrc  TYPE int4.
  IF lt_agreem IS NOT INITIAL.
    SELECT * FROM /agri/fmaghdr AS ag INNER JOIN /agri/fmagtm AS tm "#EC CI_FAE_LINES_ENSURED
    ON ag~knuma_ag EQ tm~knuma_ag
        INTO CORRESPONDING FIELDS OF TABLE lt_agrnum
         FOR ALL ENTRIES IN lt_agreem
        WHERE ag~bukrs EQ /agri/s_fmachdr-bukrs
       AND  ag~owner EQ lt_agreem-parnr
*     AND  werks EQ /agri/s_fmachdr-werks
       AND  tm~matnr EQ lt_agreem-matnr
       AND  ag~deleted NE c_true
        AND ( ag~datab GE /agri/s_fmachdr-strtdat OR
             ag~datab LE /agri/s_fmachdr-findat )
        AND ( ag~datbi GE /agri/s_fmachdr-strtdat OR
             ag~datbi LE /agri/s_fmachdr-findat ).
    ENDIF.
    IF lt_agrnum IS INITIAL.
      MOVE 4 TO lv_subrc.
      MESSAGE ID '/AGRI/FMAC' TYPE c_msg_type-error NUMBER '069' INTO sy-msgli.
      message_simple space.
      EXIT.
    ELSE.
      MOVE 0 TO lv_subrc.
    ENDIF.

ENDFORM.                    "agreement_number_get
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_ACCOUNT_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_PURCH_ORDER  text
*----------------------------------------------------------------------*
FORM assign_account_get  USING lt_purch_order TYPE /agri/t_fmagxpo
                         CHANGING lt_ekkn TYPE ty_ekkn.

  IF lt_purch_order IS NOT INITIAL.
    SELECT * FROM ekkn AS e INNER JOIN        "#EC CI_FAE_LINES_ENSURED
              ekpo AS k ON e~ebeln EQ k~ebeln ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
                       AND e~ebelp EQ k~ebelp
                 INTO CORRESPONDING FIELDS OF TABLE lt_ekkn
      FOR ALL ENTRIES IN lt_purch_order
                WHERE e~ebeln EQ lt_purch_order-ebeln
        AND k~loekz NE 'L'.
    ENDIF.

ENDFORM.                    "assign_account_get
