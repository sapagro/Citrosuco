*----------------------------------------------------------------------*
***INCLUDE /AGRI/LGLFLMF0F .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FCODE_PROCESSING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_processing .

  DATA: lv_routine(30) TYPE c VALUE 'FCODE_'.

  IF ok_code IS NOT INITIAL.

    fcode = ok_code.
    CLEAR ok_code.
    IF fcode(2) EQ 'T\'.
      PERFORM fcode_processing_tabstrip.
    ELSE.
      CONCATENATE lv_routine fcode INTO lv_routine.
      PERFORM (lv_routine) IN PROGRAM (c_program-funloc)
                           IF FOUND.
    ENDIF.

  ENDIF.

ENDFORM.                    " FCODE_PROCESSING
*&---------------------------------------------------------------------*
*&      Form  FCODE_PROCESSING_TABSTRIP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_processing_tabstrip .

  DATA: ls_tabstrip TYPE /agri/s_gtabstrip.

****Check Rules
  READ TABLE gt_tabstrip_fcodes INTO ls_tabstrip WITH KEY ts_fcode = fcode.
  CASE ls_tabstrip-local_fcode.
    WHEN c_fcode-tab_texts.
      CALL FUNCTION '/AGRI/G_WORD_PROCESSING_INIT'.
    WHEN OTHERS.
  ENDCASE.

  ts_items-activetab = fcode.

ENDFORM.                    " FCODE_PROCESSING_TABSTRIP
*&---------------------------------------------------------------------*
*&      Form  fcode_max
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_/agri/max .
****Maximize Worklist(Full Screen)
  DATA : lv_extension TYPE i VALUE 2000.

  CHECK gs_variables-worklist_is_visible EQ c_true.

  CALL METHOD ref_worklist_container->set_extension
    EXPORTING
      extension  = lv_extension
    EXCEPTIONS
      cntl_error = 1
      OTHERS     = 2.
ENDFORM.                    "fcode_max
*&---------------------------------------------------------------------*
*&      Form  fcode_min
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_/agri/min .
  DATA : lv_extension TYPE i VALUE 300.
  CHECK gs_variables-worklist_is_visible EQ c_true.
  CALL METHOD ref_worklist_container->set_extension
    EXPORTING
      extension  = lv_extension
    EXCEPTIONS
      cntl_error = 1
      OTHERS     = 2.
ENDFORM.                    "fcode_min
*&---------------------------------------------------------------------*
*&      Form  fcode_dhn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_/agri/dhn .
****Display/Hide Navigator
  IF gs_variables-worklist_is_visible EQ c_true.
    CLEAR gs_variables-worklist_is_visible.
  ELSE.
    gs_variables-worklist_is_visible = c_true.
  ENDIF.

  CALL METHOD ref_worklist_container->set_visible
    EXPORTING
      visible           = gs_variables-worklist_is_visible
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    "fcode_dhn
*&---------------------------------------------------------------------*
*&      Form  fcode_dich
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_dich.

  DATA: lv_answer.

  IF gs_variables-overview_mode = c_mode_change.

    PERFORM changes_confirm CHANGING lv_answer.
    IF lv_answer NE 'A'.

      gs_variables-overview_mode = c_mode_display.

      IF gs_variables-document_mode NE c_mode_display.
        PERFORM document_infocus_unlock
                                  USING gs_fldoc_infocus-tplnr_fl
                                        gs_fldoc_infocus-x-flhdr-strno.
        gs_variables-document_mode = c_mode_display.
        PERFORM document_infocus_set USING gs_fldoc_infocus-tplnr_fl.
      ENDIF.

    ELSE.
      IF ok_code EQ c_fcode-save.
        CLEAR: lv_answer, ok_code.
        PERFORM document_infocus_save USING c_false.
        PERFORM document_infocus_unlock USING gs_fldoc_infocus-tplnr_fl
                                              gs_fldoc_infocus-x-flhdr-strno.

        gs_variables-overview_mode = c_mode_display.
        gs_variables-document_mode = c_mode_display.
        PERFORM document_infocus_set USING
                        gs_fldoc_infocus-x-flhdr-tplnr_fl.
      ELSE.
        EXIT.
      ENDIF.
    ENDIF.

  ELSE.
    gs_variables-overview_mode = c_mode_change.
    PERFORM document_infocus_set USING gs_fldoc_infocus-tplnr_fl.
  ENDIF.

ENDFORM.                    "fcode_dich
*&---------------------------------------------------------------------*
*&      Form  fcode_crea
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_crea.

  PERFORM functional_location_create.

ENDFORM.                    "fcode_crea

**&---------------------------------------------------------------------*
**&      Form  fcode_crea
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*FORM fcode_crea.
*
*  PERFORM functional_location_create.
*
*ENDFORM.                    "fcode_crea

*&--------

*&---------------------------------------------------------------------*
*&      Form  functional_location_create
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM functional_location_create.

  DATA: lv_subrc       TYPE sy-subrc,
        lv_not_allowed,
        ls_tabstrip    TYPE /agri/s_gtabstrip.

  PERFORM document_data_initialize USING c_true.
  gs_variables-document_mode = c_mode_create.

  CLEAR: /agri/s_glflot.
  CALL SCREEN 201 STARTING AT 5 5.

  CHECK /agri/s_glflot-tplkz IS NOT INITIAL.

  MOVE-CORRESPONDING /agri/s_glflot TO gs_fldoc_infocus-x-flhdr.
*  MOVE-CORRESPONDING /agri/s_glflot TO gs_fldoc_infocus-x-iflot.

  gs_fldoc_infocus-x-flhdr-updkz = c_updkz_new.
*  gs_fldoc_infocus-x-iloa-updkz  = c_updkz_new.
*  gs_fldoc_infocus-x-iflot-updkz = c_updkz_new.

  PERFORM document_infocus_prepare.

  CHECK gs_fldoc_infocus IS NOT INITIAL.

  PERFORM authority_check USING gs_fldoc_infocus-x-flhdr
                                c_authorization_activity-release
                                c_msg_type-info
                       CHANGING lv_subrc.
  IF lv_subrc NE 0.
    gs_fldoc_infocus-x-flhdr-kfrst = 'A'.
  ENDIF.

  PERFORM release_status_allowed_check
                                 USING gs_fldoc_infocus-x-flhdr
                                       gs_fldoc_infocus-x-flhdr-kfrst
                              CHANGING lv_not_allowed.
  IF lv_not_allowed IS NOT INITIAL.
    CLEAR: gs_fldoc_infocus, gs_variables-document_mode.
  ENDIF.

****Set active tab
  LOOP AT gt_tabstrip_fcodes INTO ls_tabstrip WHERE invisible IS INITIAL.
    EXIT.
  ENDLOOP.
  ts_items-activetab = ls_tabstrip-ts_fcode.

ENDFORM.                    "functional_location_create

*&---------------------------------------------------------------------*
*&      Form  fcode_cont
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_cont.

  CASE sy-dynnr.
    WHEN c_screen-new_label.
      PERFORM update_terrain_label.
    WHEN OTHERS.
  ENDCASE.
  SET SCREEN 0.
  LEAVE SCREEN.

ENDFORM.                    "fcode_cont
*&---------------------------------------------------------------------*
*&      Form  FCODE_EXCLUDES_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_excludes_prepare CHANGING lt_fcode_excludes TYPE ui_functions.

  CASE sy-dynnr.

    WHEN c_screen-overview.
      IF gs_variables-overview_mode EQ c_mode_display
      OR NOT gs_fldoc_infocus IS INITIAL.
        APPEND c_fcode-create TO lt_fcode_excludes.
      ENDIF.

      IF gs_fldoc_infocus IS INITIAL.
        APPEND c_fcode-back            TO lt_fcode_excludes.
        APPEND c_fcode-cancel          TO lt_fcode_excludes.
        APPEND c_fcode-save            TO lt_fcode_excludes.
        APPEND c_fcode-copy            TO lt_fcode_excludes.
        APPEND c_fcode-delete          TO lt_fcode_excludes.
        APPEND c_fcode-undo_delete     TO lt_fcode_excludes.
        APPEND c_fcode-superior_funloc TO lt_fcode_excludes.
        APPEND c_fcode-change_docs     TO lt_fcode_excludes.
        APPEND c_fcode-where_used_list TO lt_fcode_excludes.
        APPEND c_fcode-class_assign    TO lt_fcode_excludes.
        APPEND c_fcode-measurement_doc TO lt_fcode_excludes.
        APPEND c_fcode-measure_doc_display TO lt_fcode_excludes.
        APPEND c_fcode-hierarchy_display TO lt_fcode_excludes.
        APPEND c_fcode-alternative_label TO lt_fcode_excludes.
      ELSE.
        IF gs_fldoc_infocus-x-flhdr-loevm IS INITIAL.
          APPEND c_fcode-undo_delete TO lt_fcode_excludes.
        ELSE.
          APPEND c_fcode-delete TO lt_fcode_excludes.
        ENDIF.
      ENDIF.

      IF gs_variables-document_mode EQ c_mode_display.
        APPEND c_fcode-save TO lt_fcode_excludes.
        APPEND c_fcode-delete TO lt_fcode_excludes.
        APPEND c_fcode-copy TO lt_fcode_excludes.
        APPEND c_fcode-measurement_doc TO lt_fcode_excludes.
      ENDIF.

      IF gs_variables-document_mode EQ c_mode_create.
        APPEND c_fcode-delete TO lt_fcode_excludes.
        APPEND c_fcode-copy TO lt_fcode_excludes.
        APPEND c_fcode-where_used_list TO lt_fcode_excludes.
        APPEND c_fcode-class_assign TO lt_fcode_excludes.
        APPEND c_fcode-measurement_doc TO lt_fcode_excludes.
        APPEND c_fcode-measure_doc_display TO lt_fcode_excludes.
        APPEND c_fcode-hierarchy_display TO lt_fcode_excludes.
        APPEND c_fcode-alternative_label TO lt_fcode_excludes.
      ENDIF.

      APPEND c_fcode-copy            TO lt_fcode_excludes.

      APPEND c_fcode-user_function1 TO lt_fcode_excludes.
      APPEND c_fcode-user_function2 TO lt_fcode_excludes.
      APPEND c_fcode-user_function3 TO lt_fcode_excludes.
      APPEND c_fcode-user_function4 TO lt_fcode_excludes.
      APPEND c_fcode-user_function5 TO lt_fcode_excludes.

      IF gs_variables-external EQ c_true.
        APPEND c_fcode-display_change_toggle TO lt_fcode_excludes.
      ENDIF.

*      IF gs_variables-labeling_active IS INITIAL.
*        APPEND c_fcode-alternative_label TO lt_fcode_excludes.
*      ENDIF.

    WHEN c_screen-gis_map.
      APPEND c_fcode-enter TO lt_fcode_excludes.
  ENDCASE.

ENDFORM.                    " FCODE_EXCLUDES_PREPARE
*&---------------------------------------------------------------------*
*&      Form  FILL_EDIT_MASK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_edit_mask .

*  STATICS: lv_tplkz TYPE tplkz,
*           ls_glflscrfields TYPE /agri/s_glflscrfields.

  DATA: ls_tgl370s TYPE /agri/tgl370s.

  PERFORM get_edit_mask USING /agri/s_glflot-tplkz
                     CHANGING ls_tgl370s.

*  IF lv_tplkz NE /agri/s_glflot-tplkz.
*
*    lv_tplkz = /agri/s_glflot-tplkz.
*    CLEAR: ls_glflscrfields.
*
*    CALL FUNCTION 'FUNC_LOCATION_GET_EDITMASK'
*      EXPORTING
*        structure_indicator     = lv_tplkz
**       LANGUAGE                = SY-LANGU
*      IMPORTING
*        editmask                = ls_glflscrfields-editm
*        hierarchy_levels        = ls_glflscrfields-stufm
*        description             = ls_glflscrfields-tplxt
*      EXCEPTIONS
*        str_indicator_not_found = 1
*        OTHERS                  = 2.
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*    ENDIF.
*
*  ENDIF.

  /agri/s_glflscrfields-editm = ls_tgl370s-editm.
  /agri/s_glflscrfields-stufm = ls_tgl370s-stufm.
*  /agri/s_glflscrfields-tplxt = ls_tgl370s-tplxt.
  keytext_get_simple  '/AGRI/TGL370S' 'TPLKZ' ls_tgl370s-tplkz
  /agri/s_glflscrfields-tplxt.

ENDFORM.                    " FILL_EDIT_MASK
*&---------------------------------------------------------------------*
*&      Form  FUNLOC_DATA_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM funloc_data_display .

*  CLEAR: /agri/s_gliflot, /agri/s_gliloa.
*  MOVE-CORRESPONDING gs_fldoc_infocus-x-iflot TO /agri/s_gliflot.
*  MOVE-CORRESPONDING gs_fldoc_infocus-x-iloa TO /agri/s_gliloa.

ENDFORM.                    " FUNLOC_DATA_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  FUNLOC_DATA_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM funloc_data_update .

  DATA: "ls_iflot TYPE /agri/s_gliflot,
*        ls_iloa  TYPE /agri/s_gliloa,
    ls_flhdr TYPE /agri/s_glflot,
    ls_flot  TYPE /agri/glflot.

  CHECK gs_variables-document_mode NE c_mode_display
    AND gs_variables-document_mode NE space.

  MOVE-CORRESPONDING gs_fldoc_infocus-x-flhdr TO ls_flhdr.
*  MOVE-CORRESPONDING gs_fldoc_infocus-x-iflot TO ls_iflot.
*  MOVE-CORRESPONDING gs_fldoc_infocus-x-iloa TO ls_iloa.

*  MOVE-CORRESPONDING /agri/s_glflot TO /agri/s_gliflot.
*  MOVE-CORRESPONDING /agri/s_glflot TO /agri/s_gliloa.
*  /agri/s_gliloa-owner = ls_iloa-owner.
*  /agri/s_gliloa-stort = ls_iloa-stort.
*  /agri/s_gliloa-adrnr = ls_iloa-adrnr.
*  /agri/s_gliflot-erdat = ls_iflot-erdat.
*  /agri/s_gliflot-ernam = ls_iflot-ernam.
*  /agri/s_gliflot-aedat = ls_iflot-aedat.
*  /agri/s_gliflot-aenam = ls_iflot-aenam.

*  IF ls_iflot NE /agri/s_gliflot.
*
*    MOVE-CORRESPONDING /agri/s_gliflot TO gs_fldoc_infocus-x-iflot.
*    IF gs_fldoc_infocus-x-iflot-updkz NE c_updkz_new.
*      gs_fldoc_infocus-x-iflot-updkz = c_updkz_update.
*    ENDIF.
*    gs_variables-data_changed = c_true.
*
*  ENDIF.
*
*  IF ls_iloa NE /agri/s_gliloa.
*
*    MOVE-CORRESPONDING /agri/s_gliloa TO gs_fldoc_infocus-x-iloa.
*    IF gs_fldoc_infocus-x-iloa-updkz NE c_updkz_new.
*      gs_fldoc_infocus-x-iloa-updkz = c_updkz_update.
*    ENDIF.
*    gs_variables-data_changed = c_true.
*
*  ENDIF.

  IF ls_flhdr NE /agri/s_glflot.

    MOVE-CORRESPONDING /agri/s_glflot TO gs_fldoc_infocus-x-flhdr.
    IF gs_fldoc_infocus-x-flhdr-updkz NE c_updkz_new.
      gs_fldoc_infocus-x-flhdr-updkz = c_updkz_update.
    ENDIF.
    gs_variables-data_changed = c_true.

  ENDIF.


ENDFORM.                    " FUNLOC_DATA_UPDATE

*&---------------------------------------------------------------------*
*&      Form  field_value_conversions
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LV_INT_EXT text
*----------------------------------------------------------------------*
FORM field_value_conversions  USING lv_int_ext.

  DATA: lv_fieldname      TYPE fnam_____4,
        lv_tabix          TYPE sytabix,
        lv_msgid          TYPE symsgid,
        lv_msgty          TYPE symsgty,
        lv_msgno          TYPE symsgno,
        lv_msgv1          TYPE symsgv,
        lv_msgv2          TYPE symsgv,
        lv_msgv3          TYPE symsgv,
        lv_msgv4          TYPE symsgv,
        lv_int_value(128) TYPE c.

***lv_int_ext 1 = int-ext 2 = ext-int
  DATA: ls_table_field LIKE tabfield.

  FIELD-SYMBOLS: <lv_field_value>      TYPE any,
*                 <lv_value> TYPE any,
                 <lwa_additional_data> TYPE /agri/s_abgl_user_scrfields.

  LOOP AT gt_additional_data ASSIGNING <lwa_additional_data>.

    ASSIGN COMPONENT <lwa_additional_data>-fieldname
       OF STRUCTURE gs_fldoc_infocus-x-flhdr TO <lv_field_value>.
    IF sy-subrc EQ 0.
      ls_table_field-fieldname = <lwa_additional_data>-fieldname.
***Single Field
      ls_table_field-tabname = <lwa_additional_data>-tabname.
      IF lv_int_ext EQ '2'.
        CHECK gs_variables-refresh_additional_data_grid IS INITIAL.
        CALL FUNCTION 'RS_CONV_EX_2_IN'
          EXPORTING
            input_external               = <lwa_additional_data>-fieldval
            table_field                  = ls_table_field
*           CURRENCY                     =
          IMPORTING
            output_internal              = <lv_field_value>
          EXCEPTIONS ##FM_SUBRC_OK
            input_not_numerical          = 1
            too_many_decimals            = 2
            more_than_one_sign           = 3
            ill_thousand_separator_dist  = 4
            too_many_digits              = 5
            sign_for_unsigned            = 6
            too_large                    = 7
            too_small                    = 8
            invalid_date_format          = 9
            invalid_date                 = 10
            invalid_time_format          = 11
            invalid_time                 = 12
            invalid_hex_digit            = 13
            unexpected_error             = 14
            invalid_fieldname            = 15
            field_and_descr_incompatible = 16
            input_too_long               = 17
            no_decimals                  = 18
            invalid_float                = 19
            conversion_exit_error        = 20
            OTHERS                       = 21.

        lv_fieldname = ls_table_field-fieldname.
        CALL FUNCTION 'DDUT_INPUT_CHECK'
          EXPORTING
            tabname       = ls_table_field-tabname
            fieldname     = lv_fieldname
            value         = <lv_field_value>
          IMPORTING
            msgid         = lv_msgid
            msgty         = lv_msgty
            msgno         = lv_msgno
            msgv1         = lv_msgv1
            msgv2         = lv_msgv2
            msgv3         = lv_msgv3
            msgv4         = lv_msgv4
          EXCEPTIONS ##FM_SUBRC_OK
            no_ddic_field = 1
            illegal_move  = 2
            OTHERS        = 3.
        IF NOT lv_msgid IS INITIAL.
          MESSAGE ID lv_msgid TYPE lv_msgty NUMBER lv_msgno
          WITH lv_msgv1 lv_msgv2 lv_msgv3 lv_msgv4 INTO sy-msgli.
          message_simple space.
***CI-IP #3313
**          IF sy-subrc NE 0 AND lv_msgty EQ c_msg_type-error.
          IF sy-subrc NE 0 OR lv_msgty EQ c_msg_type-error.
            gs_variables-errors = c_true.
          ENDIF.
        ENDIF.

*        <lwa_additional_data>-fieldval = <lv_field_value>.

      ELSEIF lv_int_ext EQ '1'.
        IF <lwa_additional_data>-fieldtyp EQ 'D'.
          IF <lv_field_value> EQ '00000000'.
            CLEAR <lwa_additional_data>-fieldval.
          ELSE.
            CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
              EXPORTING
                date_internal            = <lv_field_value>
              IMPORTING
                date_external            = <lwa_additional_data>-fieldval
              EXCEPTIONS
                date_internal_is_invalid = 1
                OTHERS                   = 2.
            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
              message_simple space.
            ENDIF.
          ENDIF.
        ELSE.
          lv_int_value = <lv_field_value>.
****DS_CONV fm delimts data which has lenght more than 45
          IF <lwa_additional_data>-outputlen LT 46.
            CALL FUNCTION 'RS_DS_CONV_IN_2_EX'
              EXPORTING
                input       = lv_int_value
*               DESCR       =
                table_field = ls_table_field
              IMPORTING
                output      = <lwa_additional_data>-fieldval.
          ELSE.
            CALL FUNCTION '/AGRI/G_CONVRT_FIELD_TO_EXTERN'
              EXPORTING
                i_conve = <lwa_additional_data>-convexit
                i_feld  = lv_int_value
              IMPORTING
                e_feld  = <lwa_additional_data>-fieldval.
****
          ENDIF.
        ENDIF.
      ENDIF.
      keytext_get_simple  ls_table_field-tabname
                         <lwa_additional_data>-fieldname
                         <lv_field_value>
                         <lwa_additional_data>-fielddscr.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " FIELD_VALUE_CONVERSIONS
*&---------------------------------------------------------------------*
*&      Form  FUNLOC_CATEGORY_CONTROL_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM funloc_category_control_read  USING   lv_fltyp TYPE fltyp.

*  STATICS: lv_fltyp_old TYPE fltyp.
*
*  CHECK lv_fltyp NE lv_fltyp_old.
*
*  CLEAR: lv_fltyp_old.
*  SELECT SINGLE * FROM /agri/tglfltyp
*           INTO gs_tglfltyp
*          WHERE fltyp = lv_fltyp.
*  IF sy-subrc NE 0.
*    gs_variables-errors = c_true.
*    MESSAGE e006(/agri/glfl) WITH lv_fltyp INTO sy-msgli.
*    message_simple space.
*    EXIT.
*  ENDIF.
*  lv_fltyp_old = lv_fltyp.
*
*  SELECT SINGLE * FROM t370f
*           INTO gs_t370f
*          WHERE fltyp = lv_fltyp.
*
*****Additional Data
*  REFRESH gt_additional_data.
*  PERFORM additional_fields_prepare.
*  gs_variables-user_structure = gs_tglfltyp-hdrstr.
*
*  CLEAR: gs_tabstrip_captions.
*  PERFORM tabstrip_build.

ENDFORM.                    " FUNLOC_CATEGORY_CONTROL_READ
*&---------------------------------------------------------------------*
*&      Form  funloc_control_read
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM funloc_control_read USING lv_tplkz TYPE tplkz
                               lv_tplvl TYPE /agri/gltplvl.

  STATICS: lv_tplkz_old TYPE tplkz,
           lv_tplvl_old TYPE /agri/gltplvl.

  CHECK: lv_tplkz NE lv_tplkz_old OR
         lv_tplvl NE lv_tplvl_old.

  SELECT SINGLE * FROM /agri/tgl370s
           INTO gs_t370s
          WHERE tplkz EQ lv_tplkz.
  IF sy-subrc NE 0.
    gs_variables-errors = c_true.
    MESSAGE e029(/agri/glfl) WITH lv_tplkz INTO sy-msgli.
    message_simple space.
    EXIT.
  ENDIF.
  lv_tplkz = gs_t370s-tplkz.
*
*  SELECT SINGLE * FROM t370s
*           INTO gs_t370s
*          WHERE tplkz EQ lv_tplkz.
*  IF sy-subrc NE 0.
*     gs_variables-errors = c_true.
*    MESSAGE e029(/agri/glfl) WITH lv_tplkz INTO sy-msgli.
*    message_simple space.
*    EXIT.
*  ENDIF.

  SELECT SINGLE * FROM /agri/tglfllvl
           INTO gs_tglfllvl
          WHERE tplkz EQ lv_tplkz
            AND tplvl EQ lv_tplvl.
  IF sy-subrc NE 0.
    gs_variables-errors = c_true.
    MESSAGE e030(/agri/glfl) WITH lv_tplkz lv_tplvl INTO sy-msgli.
    message_simple space.
    EXIT.
  ENDIF.

*  SELECT SINGLE * FROM t370f
*           INTO gs_t370f
*          WHERE fltyp EQ gs_tglfllvl-fltyp.
*  IF sy-subrc NE 0.
*    gs_variables-errors = c_true.
*    MESSAGE e006(/agri/glfl) WITH gs_tglfllvl-fltyp INTO sy-msgli.
*    message_simple space.
*    EXIT.
*  ENDIF.
  lv_tplkz_old = lv_tplkz.
  lv_tplvl_old = lv_tplvl.

****Additional Data
  REFRESH gt_additional_data.
  PERFORM additional_fields_prepare.
  gs_variables-user_structure = gs_tglfllvl-hdrstr.

  CLEAR: gs_tabstrip_captions.
  PERFORM tabstrip_build.

ENDFORM.                    " FUNLOC_CONTROL_READ
*&---------------------------------------------------------------------*
*&      Form  FIELD_CATALOG_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM field_catalog_prepare  USING    lv_structure
                            CHANGING lt_fcat TYPE lvc_t_fcat.

  DATA: lv_tabix  TYPE sy-tabix,
        lwa_fcat  TYPE lvc_s_fcat,
        lv_colpos TYPE lvc_colpos.

  FIELD-SYMBOLS: <lwa_fcat> TYPE lvc_s_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
*     I_BUFFER_ACTIVE        =
      i_structure_name       = lv_structure
*     I_CLIENT_NEVER_DISPLAY = c_true
*     I_BYPASSING_BUFFER     =
*     I_INTERNAL_TABNAME     =
    CHANGING
      ct_fieldcat            = lt_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  CASE lv_structure.
****WorkList Header
    WHEN c_structure_name-worklist_header.
      READ TABLE lt_fcat ASSIGNING <lwa_fcat> WITH KEY fieldname = 'TPLNR_FL'.
      IF <lwa_fcat> IS ASSIGNED.
        <lwa_fcat>-hotspot = c_true.
      ENDIF.

    WHEN c_structure_name-attributes.
      LOOP AT lt_fcat ASSIGNING <lwa_fcat>.
        CASE <lwa_fcat>-fieldname.
          WHEN 'ATWRT'.
            <lwa_fcat>-edit = c_true.
            <lwa_fcat>-f4availabl = c_true.

          WHEN 'CLASS' OR 'ATNAM' OR    "OR 'ATGRP' "#EC WHEN_DOUBLE_OK
               'ATBEZ' OR 'ATWTB'.

          WHEN OTHERS.
            <lwa_fcat>-tech = c_true.
        ENDCASE.
        CLEAR <lwa_fcat>-col_pos.
      ENDLOOP.

      lv_colpos = lv_colpos + 1.
      READ TABLE lt_fcat ASSIGNING <lwa_fcat>
                              WITH KEY fieldname = 'CLASS'.
      IF sy-subrc = 0.
        <lwa_fcat>-col_pos = lv_colpos.
        lv_colpos = lv_colpos + 1.
      ENDIF.

      lv_colpos = lv_colpos + 1.
      READ TABLE lt_fcat ASSIGNING <lwa_fcat>
                              WITH KEY fieldname = 'ATNAM'.
      IF sy-subrc = 0.
        <lwa_fcat>-col_pos = lv_colpos.
        lv_colpos = lv_colpos + 1.
      ENDIF.
      lv_colpos = lv_colpos + 1.
      READ TABLE lt_fcat ASSIGNING <lwa_fcat>
                              WITH KEY fieldname = 'ATBEZ'.
      IF sy-subrc = 0.
        <lwa_fcat>-col_pos = lv_colpos.
        lv_colpos = lv_colpos + 1.
      ENDIF.
      lv_colpos = lv_colpos + 1.
      READ TABLE lt_fcat ASSIGNING <lwa_fcat>
                              WITH KEY fieldname = 'ATWRT'.
      IF sy-subrc = 0.
        <lwa_fcat>-col_pos = lv_colpos.
        lv_colpos = lv_colpos + 1.
      ENDIF.
      lv_colpos = lv_colpos + 1.
      READ TABLE lt_fcat ASSIGNING <lwa_fcat>
                              WITH KEY fieldname = 'ATWTB'.
      IF sy-subrc = 0.
        <lwa_fcat>-col_pos = lv_colpos.
        lv_colpos = lv_colpos + 1.
      ENDIF.

    WHEN c_structure_name-class_assignment.
      LOOP AT lt_fcat ASSIGNING <lwa_fcat>.
        CASE <lwa_fcat>-fieldname.
          WHEN 'CLASS'.
            <lwa_fcat>-edit = c_true.
          WHEN 'KLBEZ'.

          WHEN OTHERS.
            <lwa_fcat>-tech = c_true.
        ENDCASE.
      ENDLOOP.

    WHEN c_structure_name-assignments.
      LOOP AT lt_fcat ASSIGNING <lwa_fcat>.
        CASE <lwa_fcat>-fieldname.
          WHEN 'CMNUM' OR 'DATAB' OR 'DATBI' OR
               'PROID' OR 'PAREA' OR 'AAREA' OR
               'MPGRP' OR 'AUFNR'.
            <lwa_fcat>-edit = c_true.
          WHEN OTHERS.
            <lwa_fcat>-tech = c_true.
        ENDCASE.
      ENDLOOP.

    WHEN c_structure_name-owners.
      LOOP AT lt_fcat ASSIGNING <lwa_fcat>.
        CASE <lwa_fcat>-fieldname.
          WHEN 'OWROL'.
            <lwa_fcat>-col_pos = 1.
            <lwa_fcat>-f4availabl = c_true.
          WHEN 'OWNER'.
            <lwa_fcat>-col_pos = 2.
            <lwa_fcat>-edit = c_true.
            <lwa_fcat>-f4availabl = c_true.
          WHEN 'OWNPC'.
            <lwa_fcat>-col_pos = 3.
            <lwa_fcat>-edit = c_true.
          WHEN OTHERS.
            <lwa_fcat>-tech = c_true.
        ENDCASE.
      ENDLOOP.

    WHEN c_structure_name-plants.
      LOOP AT lt_fcat ASSIGNING <lwa_fcat>.
        CASE <lwa_fcat>-fieldname.
          WHEN 'TPLNR_FL' OR 'UPDKZ'.
            <lwa_fcat>-tech = c_true.
          WHEN 'NAME1'.
          WHEN 'DFULT'.
            <lwa_fcat>-checkbox = c_true.
            <lwa_fcat>-edit = c_true.
          WHEN 'PWERK' OR 'DSTNC' OR 'DSUNT'
            OR 'ROUTE'.
            <lwa_fcat>-edit = c_true.
          WHEN OTHERS.
        ENDCASE.
      ENDLOOP.

    WHEN c_structure_name-fl_multi_lang_desc.
      LOOP AT lt_fcat ASSIGNING <lwa_fcat>.
        CASE <lwa_fcat>-fieldname.
          WHEN 'SPRAS'.
            <lwa_fcat>-outputlen = 8.
            <lwa_fcat>-edit = c_true.
          WHEN 'PLTXT'.
            <lwa_fcat>-edit = c_true.
          WHEN OTHERS.
            <lwa_fcat>-tech = c_true.
        ENDCASE.
      ENDLOOP.
    WHEN c_structure_name-terrain_labels.
      LOOP AT lt_fcat ASSIGNING <lwa_fcat>.
        CASE <lwa_fcat>-fieldname.
          WHEN 'TPLNR_FL' OR 'VERSN' OR
               'UPDKZ'.
            <lwa_fcat>-tech = c_true.
          WHEN 'STRNO'.
            <lwa_fcat>-col_pos = 1.
          WHEN 'TPLKZ'.
            <lwa_fcat>-col_pos = 2.
          WHEN 'ACTVS'.
            <lwa_fcat>-col_pos = 3.
            <lwa_fcat>-checkbox = 'X'.
          WHEN 'ERNAM'.
            <lwa_fcat>-col_pos = 4.
          WHEN 'ERDAT'.
            <lwa_fcat>-col_pos = 5.
        ENDCASE.
      ENDLOOP.
    WHEN c_structure_name-hierarchy_display.
      LOOP AT lt_fcat ASSIGNING <lwa_fcat>.
        CASE <lwa_fcat>-fieldname.
          WHEN 'TPLNR_FL'.
            <lwa_fcat>-key = c_true.
          WHEN 'TPLMA'.
            <lwa_fcat>-tech = c_true.
          WHEN OTHERS.
        ENDCASE.
      ENDLOOP.
    WHEN c_structure_name-extdfl_fcat.
      LOOP AT lt_fcat ASSIGNING <lwa_fcat>
                      WHERE fieldname = 'TPLMA'
                         OR fieldname = 'TPLNR_FL'
                         OR fieldname = 'TPLNR_TXT'
                         OR fieldname = 'TPLMA_TXT'
                         OR fieldname = 'LEVEL'
                         OR fieldname = 'HASHIER'
                         OR fieldname = 'ROWCOLOR'
                         OR fieldname = 'STYLES'.
        <lwa_fcat>-no_out = c_true.
      ENDLOOP.
    WHEN OTHERS.
      LOOP AT lt_fcat ASSIGNING <lwa_fcat>.
        CASE <lwa_fcat>-fieldname.
          WHEN 'FIELDNAME'.
            <lwa_fcat>-no_out = c_true.
          WHEN 'FIELDVAL'.
            <lwa_fcat>-edit = c_true.
            <lwa_fcat>-f4availabl = c_true.
            <lwa_fcat>-outputlen = 10.
****
            <lwa_fcat>-lowercase = c_true.
          WHEN 'FIELDDSCR'.
            <lwa_fcat>-outputlen = 40.
          WHEN 'FIELDTXT'.
            <lwa_fcat>-outputlen = 15.

          WHEN 'FLDTY' OR 'VTEXT' OR 'NAME1'
            OR 'BUTXT' OR 'DESCR' OR 'CMNUM'
            OR 'ROWCOLOR' OR 'FIELDTYP' OR 'OUTPUTLEN' OR 'CONVEXIT' OR 'TABNAME'.
            <lwa_fcat>-no_out = <lwa_fcat>-tech = c_true.
        ENDCASE.
      ENDLOOP.
  ENDCASE.

ENDFORM.                    " FIELD_CATALOG_PREPARE
*&---------------------------------------------------------------------*
*&      Form  fcode_klaa
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_klaa.
  CALL SCREEN c_screen-class_assignment
                          STARTING AT 30 2 ENDING AT 85 15.
ENDFORM.                    "fcode_klaa
*&---------------------------------------------------------------------*
*&      Form  fcode_klai
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_klai.

  APPEND INITIAL LINE TO gt_flatg_fcat.
  CALL METHOD ref_classes_grid->refresh_table_display
    EXCEPTIONS
      finished = 1
      OTHERS   = 2.
  IF sy-subrc <> 0.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CLEAR gs_variables-refresh_class_grid.

ENDFORM.                    "fcode_klai
*&---------------------------------------------------------------------*
*&      Form  FCODE_CLASS_DELETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_class_delete .

  DATA: lt_rows   TYPE lvc_t_row,
        lwa_row   TYPE lvc_s_row,
        lv_answer TYPE c,
        lwa_flatg LIKE LINE OF gt_flatg_fcat.

  FIELD-SYMBOLS: <lwa_class_modi> TYPE lvc_s_modi,
                 <lwa_flatg>      TYPE /agri/s_glflatg,
                 <lwa_flatv>      TYPE /agri/s_glflatv.

  CALL METHOD ref_classes_grid->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.

  CHECK lt_rows IS NOT INITIAL.

  popup_to_confirm TEXT-007 TEXT-033 c_true lv_answer.

  IF lv_answer EQ '1'  .

    gs_variables-refresh_class_grid = c_true.
    gs_variables-refresh_attributes = c_true.
    SORT lt_rows BY index DESCENDING.

    LOOP AT lt_rows INTO lwa_row.

      LOOP AT gt_class_mod_rows ASSIGNING <lwa_class_modi>
                                WHERE row_id GE lwa_row-index.
        IF <lwa_class_modi>-row_id NE lwa_row-index.
          <lwa_class_modi>-row_id = <lwa_class_modi>-row_id - 1.
        ELSE.
          DELETE gt_class_mod_rows
                             WHERE row_id EQ <lwa_class_modi>-row_id.
        ENDIF.
      ENDLOOP.

      READ TABLE gt_flatg_fcat INTO lwa_flatg INDEX lwa_row-index.
      READ TABLE gs_fldoc_infocus-y-flatg ASSIGNING <lwa_flatg>
                             WITH KEY clint = lwa_flatg-clint.
      IF sy-subrc EQ 0.
        <lwa_flatg>-updkz = c_updkz_delete.
        LOOP AT gs_fldoc_infocus-y-flatv ASSIGNING <lwa_flatv>
                                             WHERE clint = lwa_flatg-clint.
          <lwa_flatv>-updkz = c_updkz_delete.
        ENDLOOP.
      ENDIF.

      DELETE gs_fldoc_infocus-x-flatv WHERE clint = lwa_flatg-clint.
      DELETE gt_athdr WHERE clint = lwa_flatg-clint.
      DELETE gs_fldoc_infocus-x-flatg INDEX lwa_row-index.
      DELETE gt_flatg_fcat INDEX lwa_row-index.

      UNASSIGN: <lwa_flatg>.

    ENDLOOP.
  ENDIF.

ENDFORM.                    " FCODE_CLASS_DELETE

*&---------------------------------------------------------------------*
*&      Form  fcode_srch
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_srch.

  DATA: lt_search_hdr_more LIKE gt_search_header,
        lv_continue        TYPE c VALUE c_true.

  PERFORM document_infocus_clear CHANGING lv_continue.
  CHECK lv_continue EQ c_true.

*  CALL SELECTION-SCREEN 10.

  PERFORM worklist_search CHANGING lt_search_hdr_more.

ENDFORM.                    "FCODE_SRCH
*&---------------------------------------------------------------------*
*&      Form  fcode_srch_more
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_srch_more.

  DATA: lt_search_hdr_more LIKE gt_search_header,
        lv_continue        TYPE c VALUE c_true.

  PERFORM document_infocus_clear CHANGING lv_continue.
  CHECK lv_continue EQ c_true.

  lt_search_hdr_more = gt_search_header.
  PERFORM worklist_search CHANGING lt_search_hdr_more.

ENDFORM.                    "fcode_SRCH_MORE

*&---------------------------------------------------------------------*
*&      Form  fcode_tmap
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_tmap.

  IF gs_tglfllvl-wpurl IS INITIAL.
    MESSAGE i046(/agri/glfl) INTO sy-msgli.
    message_simple space.
    EXIT.
  ENDIF.

  IF ref_container_map IS NOT INITIAL.
    CALL METHOD ref_container_map->free
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    FREE: ref_container_map.
  ENDIF.

  IF ref_html_viewer_map IS NOT INITIAL.
    CALL METHOD ref_html_viewer_map->free
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    FREE: ref_html_viewer_map.
  ENDIF.

  CALL SCREEN c_screen-gis_map STARTING AT  10  5
                                 ENDING AT 150 30.
ENDFORM.                    "fcode_tmap

*&---------------------------------------------------------------------*
*&      Form  fl_data_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fl_data_display .
  DATA: lwa_header TYPE /agri/s_glflot,
        lv_subrc   TYPE sy-subrc.

  gs_variables-worklist_refresh = c_true.

  SORT gt_search_header BY tplnr_fl.
  SORT gt_search_text BY tplnr_fl spras.
  DELETE ADJACENT DUPLICATES FROM gt_search_header COMPARING tplnr_fl.

  LOOP AT gt_search_header ASSIGNING FIELD-SYMBOL(<lwa_search_header>).
    READ TABLE gt_search_text INTO DATA(lwa_search_text)
                              WITH KEY tplnr_fl = <lwa_search_header>-tplnr_fl
                                       spras    = sy-langu
                              BINARY SEARCH.
    IF sy-subrc EQ 0.
      <lwa_search_header>-pltxt = lwa_search_text-pltxt.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE gt_search_header LINES gs_variables-wl_srch_count.

  IF gs_variables-wl_srch_count EQ 1 AND gs_fldoc_infocus IS INITIAL.
    READ TABLE gt_search_header INTO lwa_header INDEX 1.
    PERFORM document_infocus_set USING lwa_header-tplnr_fl.
  ENDIF.

ENDFORM.                    " CM_DATA_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  fcode_hids
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_hids.

  DATA : lv_no_data_exists.

*  PERFORM display_fl_hierarchy USING /agri/s_glflot-tplnr_fl.
  PERFORM display_fl_hierarchy CHANGING lv_no_data_exists.

  IF lv_no_data_exists EQ c_true.
    MESSAGE ID '/AGRI/GLFL' TYPE c_msg_type-success NUMBER '057'
    WITH gs_fldoc_infocus-tplnr_fl INTO sy-msgli.
    message_simple space.
    EXIT.
  ENDIF.

  gs_variables-refresh_extdfl_tree = c_true.

  CALL SCREEN c_screen-reference_summary.

ENDFORM.                    "fcode_hids
*&---------------------------------------------------------------------*
*&      Form  fcode_back
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_back.

  DATA:  lv_answer.
*         lv_subrc TYPE sy-subrc,
*         lv_dummy.
*
*****Check whether there are any changes
*  PERFORM changes_confirm CHANGING lv_answer.
*
*  IF lv_answer EQ 'A'.
*    IF ok_code EQ c_fcode-save.
*      CLEAR ok_code.
*      PERFORM document_infocus_save USING space.
*    ELSE.
*      EXIT.
*    ENDIF.
*  ENDIF.
*  PERFORM document_infocus_unlock USING gs_fldoc_infocus-tplnr_fl.
*  PERFORM document_data_initialize USING c_true.
*  gs_variables-document_mode = c_mode_display.

  PERFORM document_infocus_clear USING lv_answer.

ENDFORM.                    "fcode_back
*&---------------------------------------------------------------------*
*&      Form  FL_DESC_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fl_desc_prepare .

  DATA: lwa_fldesc        TYPE /agri/s_gliflotx,
        lwa_fldesc_layout TYPE /agri/s_gliflotx_fcat.

  CLEAR: gt_fl_desc_layout.

  LOOP AT gs_fldoc_infocus-x-iflotx INTO lwa_fldesc.
    MOVE-CORRESPONDING lwa_fldesc TO lwa_fldesc_layout.
    PERFORM desc_styles_prepare CHANGING lwa_fldesc_layout.
    APPEND lwa_fldesc_layout TO gt_fl_desc_layout.
  ENDLOOP.

ENDFORM.                    " FL_DESC_PREPARE
*&---------------------------------------------------------------------*
*&      Form  fcode_desc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_desc.
  CALL SCREEN c_screen-multi_lang_desc
                           STARTING AT 30 2 ENDING AT 85 15.
ENDFORM.                    "fcode_desc
*&---------------------------------------------------------------------*
*&      Form  fcode_desc_delete
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_desc_delete.

  DATA: lt_rows         TYPE lvc_t_row,
        lwa_row         TYPE lvc_s_row,
        lv_answer       TYPE c,
        lwa_desc_layout LIKE LINE OF gt_fl_desc_layout.

  FIELD-SYMBOLS: <lwa_desc_modi> TYPE lvc_s_modi,
                 <lwa_desc>      TYPE /agri/s_gliflotx.

  CALL METHOD ref_multi_lang_desc_grid->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.

  CHECK lt_rows IS NOT INITIAL.

  popup_to_confirm TEXT-007 TEXT-032 c_true lv_answer.

  IF lv_answer EQ '1'  .

    gs_variables-refresh_desc_grid = c_true.
    SORT lt_rows BY index DESCENDING.

    LOOP AT lt_rows INTO lwa_row.

      LOOP AT gt_desc_mod_rows ASSIGNING <lwa_desc_modi> WHERE row_id GE lwa_row-index.
        IF <lwa_desc_modi>-row_id NE lwa_row-index.
          <lwa_desc_modi>-row_id = <lwa_desc_modi>-row_id - 1.
        ELSE.
          DELETE gt_desc_mod_rows WHERE row_id EQ <lwa_desc_modi>-row_id.
        ENDIF.
      ENDLOOP.

      READ TABLE gt_fl_desc_layout INTO lwa_desc_layout INDEX lwa_row-index.
      READ TABLE gs_fldoc_infocus-y-iflotx ASSIGNING <lwa_desc>
                             WITH KEY spras = lwa_desc_layout-spras.

      IF sy-subrc EQ 0.
        <lwa_desc>-updkz = c_updkz_delete.
      ENDIF.

      DELETE gs_fldoc_infocus-x-iflotx INDEX lwa_row-index.
      DELETE gt_fl_desc_layout INDEX lwa_row-index.

      IF lwa_desc_layout-spras = sy-langu.
        READ TABLE gs_fldoc_infocus-x-iflotx ASSIGNING <lwa_desc>
                                             WITH KEY spras = sy-langu.
        IF sy-subrc = 0.
          gs_fldoc_infocus-x-flhdr-pltxt = <lwa_desc>-pltxt.
        ELSE.
          CLEAR gs_fldoc_infocus-x-flhdr-pltxt.
        ENDIF.
        IF gs_fldoc_infocus-x-flhdr-updkz NE c_updkz_new.
          gs_fldoc_infocus-x-flhdr-updkz = c_updkz_update.
        ENDIF.
      ENDIF.

      UNASSIGN: <lwa_desc>.
    ENDLOOP.
  ENDIF.

ENDFORM.                    "fcode_desc_delete
*&---------------------------------------------------------------------*
*&      Form  fcode_dins
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_dins.

  APPEND INITIAL LINE TO gt_fl_desc_layout.
  CALL METHOD ref_multi_lang_desc_grid->refresh_table_display
    EXCEPTIONS
      finished = 1
      OTHERS   = 2.
  IF sy-subrc <> 0.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CLEAR gs_variables-refresh_desc_grid.

ENDFORM.                    "fcode_dins
*&---------------------------------------------------------------------*
*&      Form  fcode_save
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_save.
  PERFORM document_infocus_save USING c_true.
ENDFORM.                    "fcode_save
*&---------------------------------------------------------------------*
*&      Form  fcode_wlhc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_wlhc.

****Hotspot click - Worklist
  DATA: lv_answer,
        lv_subrc         TYPE sy-subrc,
        lv_dummy,
        lwa_selected_doc LIKE LINE OF gt_selected_docs.

  READ TABLE gt_selected_docs INTO lwa_selected_doc INDEX 1.
  CHECK sy-subrc EQ 0.

****Confirm Changes made to the document
  PERFORM changes_confirm CHANGING lv_answer.

  IF lv_answer EQ 'A'.

    IF ok_code EQ c_fcode-save.
****Save - Yes
      CLEAR ok_code.
****Save the infocus document
      PERFORM document_infocus_save USING space.

    ELSE.
****Save - Cancel
****Changes were not saved
      MESSAGE ID '/AGRI/GLFL' TYPE c_msg_type-success NUMBER '008'
      INTO sy-msgli.
      message_simple space.
      PERFORM messages_display USING gs_variables-initiator.
      EXIT.
    ENDIF.
  ENDIF.

****Dequeue the document infocus
  PERFORM document_infocus_unlock USING gs_fldoc_infocus-tplnr_fl
                                        gs_fldoc_infocus-x-flhdr-strno.

  PERFORM document_infocus_set USING lwa_selected_doc-tplnr_fl.

ENDFORM.                    "fcode_wlhc
*&---------------------------------------------------------------------*
*&      Form  fcode_medo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_medo.

  DATA: lt_tplnr  TYPE /agri/t_gltplnr,
        lwa_tplnr TYPE /agri/s_gltplnr.

  lwa_tplnr-tplnr_fl = gs_fldoc_infocus-x-flhdr-tplnr_fl.
  APPEND lwa_tplnr TO lt_tplnr.

  CALL FUNCTION '/AGRI/GLMD_CREATE_DIALOG'
    EXPORTING
      i_aslvl           = 'T'
*     I_POPUP           = 'X'
      it_tplnr          = lt_tplnr
*     IT_EQUNR          =
*     IT_CSKEY          =
*   IMPORTING
*     ET_MDDOC          =
    EXCEPTIONS
      inconsistent_data = 1
      no_authorization  = 2
      no_data_exists    = 3
      cancelled         = 4
      OTHERS            = 5.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.                    "fcode_medo

*&---------------------------------------------------------------------*
*&      Form  fcode_mdsp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_mdsp.
****Display Measurement Documents
  RANGES: lr_tplnr FOR /agri/s_glflot-tplnr_fl.

  CALL SELECTION-SCREEN 0099 STARTING AT 5 5 ENDING AT 120 12.

  lr_tplnr-sign   = 'I'.
  lr_tplnr-option = 'EQ'.
  lr_tplnr-low    = gs_fldoc_infocus-tplnr_fl.
  COLLECT lr_tplnr.

  SUBMIT /agri/glmd_list_display
    WITH so_strno IN lr_tplnr
     AND RETURN.

*  SUBMIT /agri/glmd_measurement_doc      "#EC CI_SUBMIT
*    WITH p_mpgrp  EQ p_mpgrp
*    WITH so_tplnr IN lr_tplnr
*    WITH so_mdate IN so_mdate AND RETURN.

ENDFORM.                    "fcode_mdsp

*&---------------------------------------------------------------------*
*&      Form  fcode_adcr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_adcr.
*  PERFORM address_create.
ENDFORM.                    "fcode_adcr
*&---------------------------------------------------------------------*
*&      Form  fcode_ipar_iobj
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_ipar_iobj.

  PERFORM partners_fcode_execute USING fcode.

ENDFORM.                    "fcode_ipar_iobj
*&---------------------------------------------------------------------*
*&      Form  fcode_ipar_delp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_ipar_delp.

  PERFORM partners_fcode_execute USING fcode.

ENDFORM.                    "fcode_ipar_iobj
*&---------------------------------------------------------------------*
*&      Form  FUNLOC_DATA_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM funloc_data_prepare CHANGING lwa_flhdr TYPE /agri/s_glflot.
*                                  lwa_iflot TYPE /agri/s_gliflot
*                                  lwa_ifloa TYPE /agri/s_gliloa.

*  DATA: lwa_iflo TYPE iflo.
  DATA: lwa_flhdr_st TYPE /agri/s_glflot.

  IF lwa_flhdr-tplnr_fl IS NOT INITIAL.
    SELECT SINGLE * FROM /agri/glflot "#EC CI_ALL_FIELDS_NEEDED
      INTO lwa_flhdr_st
     WHERE tplnr_fl EQ lwa_flhdr-tplma.
  ENDIF.
*  lwa_iflot-mlang = sy-langu.
*  CALL FUNCTION 'FUNC_LOCATION_READ'
*    EXPORTING
**     AUTH_CHECK      = ' '
**     AUTH_TCODE      = 'IL03'
**     BUFFER_BYPASS   = ' '
**     DYFIELD         = ' '
**     NO_OTHER_LANGUAGE = ' '
**     SPRAS           = SY-LANGU
*      tplnr           = lwa_flhdr-tplma
*    IMPORTING
*      iflo_wa         = lwa_flhdr_st
**     PLTXT           =
*    EXCEPTIONS
*      iflot_not_found = 1
*      iloa_not_found  = 2
*      no_authority    = 3
*      OTHERS          = 4.
*  IF sy-subrc <> 0.
*    EXIT.
** Implement suitable error handling here
*  ENDIF.

*--move header data
  lwa_flhdr-bukrs = lwa_flhdr_st-bukrs.
  lwa_flhdr-iwerk = lwa_flhdr_st-iwerk.

*  lwa_iflot-datab = lwa_iflo-datab.
*  lwa_flhdr-begru = lwa_flhdr_st-begru.

*  lwa_ifloa-bukrs = lwa_iflo-bukrs.
  lwa_flhdr-swerk = lwa_flhdr_st-swerk.
  lwa_flhdr-kokrs = lwa_flhdr_st-kokrs.
  lwa_flhdr-gsber = lwa_flhdr_st-gsber.

  IF lwa_flhdr-swerk IS NOT INITIAL.

    PERFORM check_plant_all CHANGING lwa_flhdr.
*                                     lwa_iflot
*                                     lwa_ifloa.

  ENDIF.

ENDFORM.                    " FUNLOC_DATA_PREPARE
*&---------------------------------------------------------------------*
*&      Form  fcode_cmph
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_cmph.
  CLEAR gs_variables-header_display.
ENDFORM.                    "fcode_cmph

*&---------------------------------------------------------------------*
*&      Form  fcode_exph
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_exph.
  gs_variables-header_display = c_true.
ENDFORM.                    "fcode_exph
*&---------------------------------------------------------------------*
*&      Form  fcode_dele
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_dele.

  DATA: lv_subrc     TYPE sy-subrc,
        lv_answer,
        lv_text(100),
        lwa_message  TYPE /agri/s_gprolog,
        lt_messages  TYPE /agri/t_gprolog.

  gs_variables-initiator = c_log_initiator-change.
  PERFORM messages_initialize USING gs_variables-initiator
                                    c_log_subobject-change
                                    gs_fldoc_infocus-x-flhdr.

  lv_text = TEXT-014.
  REPLACE '&1' WITH gs_fldoc_infocus-x-flhdr-strno INTO lv_text.
  popup_to_confirm TEXT-007 lv_text c_true lv_answer.

  CHECK lv_answer EQ '1'.

*--Call Authority Check
  PERFORM authority_check USING gs_fldoc_infocus-x-flhdr
                                c_authorization_activity-delete
                                c_msg_type-info
                       CHANGING lv_subrc.
  IF lv_subrc NE 0.
    PERFORM messages_display USING gs_variables-initiator.
    EXIT.
  ENDIF.

  IF gs_fldoc_infocus-x-flhdr-loevm IS INITIAL.
    gs_fldoc_infocus-x-flhdr-loevm = c_true.
    IF gs_fldoc_infocus-x-flhdr-updkz NE c_updkz_new.
      gs_fldoc_infocus-x-flhdr-updkz = c_updkz_update.
    ENDIF.
  ENDIF.

  PERFORM document_infocus_save USING c_true.
*  CALL FUNCTION '/AGRI/GLFL_STATUS_CHANGE'
*    EXPORTING
*      i_tplnr            = gs_fldoc_infocus-x-flhdr-tplnr_fl
**     I_SET_UPDATE_TASK  = c_true
**     I_COMMIT_WORK      = c_true
*      i_activity         = c_activity-delete
*    CHANGING
*      ct_messages        = lt_messages
*    EXCEPTIONS
*      no_data_exists     = 1
*      no_change          = 2
*      error_while_saving = 3
*      OTHERS             = 4.
*  IF sy-subrc <> 0.
*
*    LOOP AT lt_messages INTO lwa_message.
*      MESSAGE ID lwa_message-msgid TYPE lwa_message-msgty
*      NUMBER lwa_message-msgno WITH lwa_message-msgv1
*      lwa_message-msgv2 lwa_message-msgv3 lwa_message-msgv4
*      INTO sy-msgli.
*      message_simple space.
*    ENDLOOP.
*
*  ELSE.
*    MESSAGE ID '/AGRI/GLFL' TYPE c_msg_type-success NUMBER '007'
*                        WITH gs_fldoc_infocus-x-flhdr-tplnr_fl INTO
*                        sy-msgli.
*    message_simple space.
*  ENDIF.
*
*  PERFORM messages_display USING gs_variables-initiator.
*
*  CLEAR gs_variables-data_changed.
*  PERFORM document_infocus_set USING gs_fldoc_infocus-x-flhdr-tplnr_fl.

ENDFORM.                    "fcode_dele
*&---------------------------------------------------------------------*
*&      Form  fcode_cdoc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_cdoc.

  DATA: lv_objectid  TYPE cdhdr-objectid,
        lv_subrc     TYPE sy-subrc,
        lt_tplnr_fl  TYPE /agri/t_gltplnr,
        lwa_tplnr_fl LIKE LINE OF lt_tplnr_fl.

  DATA: lt_objid  TYPE /agri/t_gcdobjid,
        lwa_objid TYPE /agri/s_gcdobjid.

  CHECK: NOT gs_fldoc_infocus-tplnr_fl IS INITIAL.

  lwa_tplnr_fl-tplnr_fl = gs_fldoc_infocus-tplnr_fl.
*  APPEND lwa_tplnr_fl TO lt_tplnr_fl.

*  CALL FUNCTION '/AGRI/GLFL_CHANGE_DOCS_DISPLAY'
*    EXPORTING
*      it_tplnr_fl        = lt_tplnr_fl
**     IT_CDHDR           =
**     I_CLASS            = c_class
**     I_PROGRAM          =
**     IT_RULES           =
**     I_NPRIC            = gs_fldoc_infocus-x-
*    EXCEPTIONS
*      no_documents_found = 1
*      no_object_selected = 2
*      OTHERS             = 3.
*  IF sy-subrc <> 0.
** Implement suitable error handling here
*  ENDIF.
  APPEND lwa_tplnr_fl TO lt_objid.
  CALL FUNCTION '/AGRI/GCD_DISPLAY'
    EXPORTING
      i_object           = c_object-change_documents
*     I_TITLE            =
      i_html_view        = c_true
*     T_CDHDR            =
      t_objid            = lt_objid
*     IREF_GCD_PROCESS   =
    EXCEPTIONS
      no_documents_found = 1
      no_object_selected = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    "fcode_cdoc
*&---------------------------------------------------------------------*
*&      Form  fcode_undl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_undl.


  DATA: lv_subrc     TYPE sy-subrc,
        lv_answer,
        lv_text(100),
        lwa_message  TYPE /agri/s_gprolog,
        lt_messages  TYPE /agri/t_gprolog.

  gs_variables-initiator = c_log_initiator-change.
  PERFORM messages_initialize USING gs_variables-initiator
                                    c_log_subobject-change
                                    gs_fldoc_infocus-x-flhdr.

  lv_text = TEXT-015.
  REPLACE '&1' WITH gs_fldoc_infocus-x-flhdr-strno INTO lv_text.
  popup_to_confirm TEXT-007 lv_text c_true lv_answer.

  CHECK lv_answer EQ '1'.

*--Call Authority Check
  PERFORM authority_check USING gs_fldoc_infocus-x-flhdr
                                c_authorization_activity-change
                                c_msg_type-info
                       CHANGING lv_subrc.
  IF lv_subrc NE 0.
    PERFORM messages_display USING gs_variables-initiator.
    EXIT.
  ENDIF.

  IF gs_fldoc_infocus-x-flhdr-loevm IS NOT INITIAL.
    CLEAR gs_fldoc_infocus-x-flhdr-loevm.
    gs_fldoc_infocus-x-flhdr-updkz = c_updkz_update.
  ENDIF.

  PERFORM document_infocus_save USING c_true.

*  DATA: lv_subrc     TYPE sy-subrc,
*        lv_answer,
*        lv_text(100),
*        lwa_message  TYPE /agri/s_gprolog,
*        lt_messages  TYPE /agri/t_gprolog.
*
*  gs_variables-initiator = c_log_initiator-change.
*  PERFORM messages_initialize USING gs_variables-initiator
*                                    c_log_subobject-change
*                                    gs_fldoc_infocus-x-flhdr.
*
*  lv_text = TEXT-015.
*  REPLACE '&1' WITH gs_fldoc_infocus-x-flhdr-strno INTO lv_text.
*  popup_to_confirm TEXT-007 lv_text c_true lv_answer.
*
*  CHECK lv_answer EQ '1'.
*
*  CALL FUNCTION '/AGRI/GLFL_STATUS_CHANGE'
*    EXPORTING
*      i_tplnr            = gs_fldoc_infocus-x-flhdr-tplnr_fl
**     I_SET_UPDATE_TASK  = c_true
**     I_COMMIT_WORK      = c_true
*      i_activity         = c_activity-undo_delete
*    CHANGING
*      ct_messages        = lt_messages
*    EXCEPTIONS
*      no_data_exists     = 1
*      no_change          = 2
*      error_while_saving = 3
*      OTHERS             = 4.
*  IF sy-subrc <> 0.
*
*    LOOP AT lt_messages INTO lwa_message.
*      MESSAGE ID lwa_message-msgid TYPE lwa_message-msgty
*      NUMBER lwa_message-msgno WITH lwa_message-msgv1
*      lwa_message-msgv2 lwa_message-msgv3 lwa_message-msgv4
*      INTO sy-msgli.
*      message_simple space.
*    ENDLOOP.
*
*  ELSE.
*    MESSAGE ID '/AGRI/GLFL' TYPE c_msg_type-success NUMBER '007'
*                        WITH gs_fldoc_infocus-x-flhdr-tplnr_fl INTO
*                        sy-msgli.
*    message_simple space.
*  ENDIF.
*
*  PERFORM messages_display USING gs_variables-initiator.
*
*  CLEAR gs_variables-data_changed.
*  PERFORM document_infocus_set USING gs_fldoc_infocus-x-flhdr-tplnr_fl.

ENDFORM.                    "fcode_undl
*&---------------------------------------------------------------------*
*&      Form  fcode_pred
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_pred.
  CALL FUNCTION '/AGRI/GLFL_HIERARCHY_GET'
    EXPORTING
      i_tplnr         = gs_fldoc_infocus-x-flhdr-tplnr_fl
    IMPORTING
      et_flhier_list  = gt_flhier_list
    EXCEPTIONS
      invalid_data    = 1
      incomplete_data = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
  CHECK gt_flhier_list IS NOT INITIAL.
  gs_variables-refresh_hierarchy_grid = c_true.
  CALL SCREEN 0206 STARTING AT 4 5 ENDING AT 86 20.
  REFRESH gt_flhier_list.
ENDFORM.                    "fcode_pred
*&---------------------------------------------------------------------*
*&      Form  fcode_oins
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_oins.

  DATA: lwa_flown TYPE /agri/s_glflown_fcat.

  lwa_flown-owrol = gs_fldoc_infocus-x-flhdr-owrol.
  APPEND lwa_flown TO gt_owners.
  CALL METHOD ref_owners_grid->refresh_table_display
*    EXPORTING
*      is_stable      =
*      i_soft_refresh =
    EXCEPTIONS
      finished = 1
      OTHERS   = 2.
  IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  CLEAR gs_variables-refresh_owners_grid.

ENDFORM.                    "fcode_oins
*&---------------------------------------------------------------------*
*&      Form  owner_data_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM owner_data_check
                  USING lwa_flown TYPE /agri/s_glflown
                        lv_subrc.

  DATA: lv_cnt   TYPE i,
        lv_error.

  CLEAR lv_subrc.
  IF lwa_flown-owner IS INITIAL.
    lv_error = c_true.
    lv_subrc = 4.
    MESSAGE ID '/AGRI/GLFL' TYPE 'E' NUMBER '016' INTO sy-msgli.
    message_simple space.
    EXIT.
  ENDIF.

  LOOP AT gs_fldoc_infocus-x-flown TRANSPORTING NO FIELDS
                            WHERE owner EQ lwa_flown-owner.
    lv_cnt = lv_cnt + 1.
    IF lv_cnt GT 1.
      lv_error = c_true.
      MESSAGE ID '/AGRI/GLCOM' TYPE 'E' NUMBER '005' INTO sy-msgli.
      message_simple space.
    ENDIF.
  ENDLOOP.

  IF lv_error IS NOT INITIAL.
    lv_subrc = 4.
    EXIT.
  ENDIF.

  PERFORM owner_values_check USING space
                                   lwa_flown-owner
                                   lwa_flown-owrol.
  IF gs_variables-errors IS NOT INITIAL.
    lv_subrc = 4.
    EXIT.
  ENDIF.

  IF lwa_flown-ownpc IS INITIAL.
    lv_error = c_true.
    MESSAGE ID '/AGRI/GLFL' TYPE 'E' NUMBER '020' INTO sy-msgli.
    message_simple space.
  ELSEIF lwa_flown-ownpc GT 100.
    lv_error = c_true.
    MESSAGE ID '/AGRI/GLFL' TYPE 'E' NUMBER '021' INTO sy-msgli.
    message_simple space.
  ENDIF.

  IF lv_error IS NOT INITIAL.
    lv_subrc = 4.
  ENDIF.

ENDFORM.                    " owner_data_check
*&---------------------------------------------------------------------*
*&      Form  FCODE_OWNER_DELETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_owner_delete .

  DATA: lt_rows          TYPE lvc_t_row,
        lwa_row          TYPE lvc_s_row,
        lv_answer        TYPE c,
        lwa_owner_layout LIKE LINE OF gt_owners.

  FIELD-SYMBOLS: <lwa_owner_modi> TYPE lvc_s_modi,
                 <lwa_owner>      TYPE /agri/s_glflown.

  popup_to_confirm TEXT-007 TEXT-034 c_true lv_answer.

  IF lv_answer EQ '1'  .

    CALL METHOD ref_owners_grid->get_selected_rows
      IMPORTING
        et_index_rows = lt_rows.

    CHECK lt_rows IS NOT INITIAL.
    gs_variables-refresh_owners_grid = c_true.
    SORT lt_rows BY index DESCENDING.

    LOOP AT lt_rows INTO lwa_row.

      LOOP AT gt_owner_mod_rows ASSIGNING <lwa_owner_modi>
                                WHERE row_id GE lwa_row-index.
        IF sy-subrc EQ 0.
          IF <lwa_owner_modi>-row_id NE lwa_row-index.
            <lwa_owner_modi>-row_id = <lwa_owner_modi>-row_id - 1.
          ELSE.
            DELETE gt_owner_mod_rows WHERE row_id EQ <lwa_owner_modi>-row_id.
          ENDIF.
        ENDIF.
      ENDLOOP.

      READ TABLE gt_owners INTO lwa_owner_layout INDEX lwa_row-index.
      READ TABLE gs_fldoc_infocus-y-flown ASSIGNING <lwa_owner>
                             INDEX lwa_row-index.

      IF sy-subrc EQ 0.
        <lwa_owner>-updkz = c_updkz_delete.
      ENDIF.

      DELETE gs_fldoc_infocus-x-flown INDEX lwa_row-index.
      DELETE gt_owners INDEX lwa_row-index.

      UNASSIGN: <lwa_owner>.
    ENDLOOP.
  ENDIF.

  READ TABLE gs_fldoc_infocus-x-flown TRANSPORTING NO FIELDS
                                      WITH KEY owner = gs_fldoc_infocus-x-flhdr-owner.
  IF sy-subrc NE 0.
    CLEAR: gs_fldoc_infocus-x-flhdr-owner,
           gs_variables-owners_assigned.
    MESSAGE ID '/AGRI/GLFL' TYPE 'W' NUMBER '019' INTO sy-msgli.
    message_simple space.
  ENDIF.

ENDFORM.                    " FCODE_OWNER_DELETE

*&---------------------------------------------------------------------*
*&      Form  fcode_chsf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_chsf.
*****Change Superior Functional Location

  CALL SCREEN c_screen-change_superior_fl STARTING AT 17 05
                  ENDING AT 90 08.

ENDFORM.                    "fcode_chsf
*&---------------------------------------------------------------------*
*&      Form  FUNLOC_DATA_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM funloc_data_check .

  IF gs_fldoc_infocus-x-flhdr-swerk NE /agri/s_glflot-swerk AND
     /agri/s_glflot-swerk IS NOT INITIAL.
    PERFORM check_plant_all CHANGING /agri/s_glflot.
*                                     /agri/s_gliflot
*                                     /agri/s_gliloa.
  ENDIF.

  IF /agri/s_glflot-garea IS INITIAL.
    MESSAGE ID '/AGRI/GLFL' TYPE 'E' NUMBER '026' INTO sy-msgli.
    message_simple space.
    gs_variables-errors = c_true.
  ENDIF.

  IF /agri/s_glflot-msehi IS INITIAL.
    MESSAGE ID '/AGRI/GLFL' TYPE 'E' NUMBER '027' INTO sy-msgli.
    message_simple space.
    gs_variables-errors = c_true.
  ENDIF.

ENDFORM.                    " FUNLOC_DATA_CHECK
*&---------------------------------------------------------------------*
*&      Form  FUNLOC_INFOCUS_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM funloc_infocus_save CHANGING lv_subrc.

  DATA: lt_messages  TYPE /agri/t_gprolog,
        ls_message   TYPE /agri/s_gprolog,
        lv_stop_save.

  PERFORM terrain_data_check USING c_true
                          CHANGING lv_subrc.
  IF lv_subrc NE 0.
    EXIT.
  ENDIF.
  PERFORM field_value_conversions USING '2'.
  CHECK gs_variables-errors IS INITIAL.

*--call badi Document check
  PERFORM badi_document_check CHANGING  lv_stop_save.
  IF lv_stop_save IS NOT INITIAL.
    EXIT.
  ENDIF.

  PERFORM address_number_get.

*  PERFORM status_outcome_set USING gs_fldoc_infocus-x-flhdr-objnr.

  CALL FUNCTION '/AGRI/GLFL_SAVE_SINGLE'
    EXPORTING
*     I_SET_UPDATE_TASK  = 'X'
*     i_commit_work      = 'X'
      iref_text          = ref_text
    CHANGING
      cs_fldoc           = gs_fldoc_infocus
      ct_messages        = lt_messages
    EXCEPTIONS
      no_change          = 1
      error_while_saving = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  lv_subrc  = sy-subrc.
  LOOP AT lt_messages INTO ls_message.
    MESSAGE ID ls_message-msgid TYPE ls_message-msgty
       NUMBER ls_message-msgno WITH ls_message-msgv1
       ls_message-msgv2 ls_message-msgv3 ls_message-msgv4
                  INTO sy-msgli.
    message_simple space.
  ENDLOOP.
*
*  IF lv_subrc EQ 0 OR
*     lv_subrc EQ 1.
*    PERFORM assignments_infocus_save CHANGING lv_subrc.
*  ENDIF.
*
*  IF lv_subrc EQ 0.
*    COMMIT WORK.
*  ELSE.
*    ROLLBACK WORK.
*  ENDIF.

  IF lv_subrc NE 0.
    IF lv_subrc EQ 1.
      MESSAGE ID '/AGRI/GLOBAL' TYPE c_msg_type-success NUMBER '322'
                                                        INTO sy-msgli.
      message_simple space.
    ELSE.
      MESSAGE ID '/AGRI/GLFL' TYPE c_msg_type-error NUMBER '009'
                                                        INTO sy-msgli.
      message_simple space.
    ENDIF.
  ELSE.
    MESSAGE ID '/AGRI/GLFL' TYPE c_msg_type-success NUMBER '007'
                            WITH gs_fldoc_infocus-x-flhdr-tplnr_fl INTO
                            sy-msgli.
    message_simple space.
  ENDIF.

ENDFORM.                    " FUNLOC_INFOCUS_SAVE
*&---------------------------------------------------------------------*
*&      Form  fcode_lbfo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_lbfo.

  gs_variables-refresh_label_grid = c_true.
  CALL SCREEN c_screen-terrain_labels STARTING AT 0 5 ENDING AT 95 20.

*  DATA: lv_changed,
*        lv_changes_allowed,
*        lwa_iflos_new      TYPE ilox_iflos,
*        lt_iflos           TYPE ilox_t_iflos,
*        lwa_tplnr_ilox     TYPE ilox_tplnr,
*        lt_tplnr_ilox      TYPE ilox_t_tplnr.
*
*  IF gs_variables-document_mode NE c_mode_display.
*    lv_changes_allowed = c_true.
*  ENDIF.
*
*  CALL FUNCTION 'ILOY_FUNC_LOCATION_SHOW_HIST'
*    EXPORTING
*      i_tplnr       = gs_fldoc_infocus-x-flhdr-tplnr_fl
*      i_flg_changes = lv_changes_allowed
*    IMPORTING
*      e_flg_changes = lv_changed
*    EXCEPTIONS
*      not_found     = 1
*      OTHERS        = 2.
*  IF sy-subrc <> 0.
** Implement suitable error handling here
*  ENDIF.
*
*  CHECK lv_changed IS NOT INITIAL.
*
*  lwa_tplnr_ilox-tplnr = gs_fldoc_infocus-x-flhdr-tplnr_fl.
*  APPEND lwa_tplnr_ilox TO lt_tplnr_ilox.
*
*  CALL FUNCTION 'ILOX_IFLOS_READ_BY_TPLNR'
*    EXPORTING
*      it_tplnr  = lt_tplnr_ilox
*    IMPORTING
*      et_iflos  = lt_iflos
*    EXCEPTIONS
*      not_found = 1
*      OTHERS    = 2.
*  CHECK sy-subrc EQ 0.
*  LOOP AT lt_iflos INTO lwa_iflos_new
*                  WHERE prkey = c_true
*                    AND actvs = c_true.
*    EXIT.
*  ENDLOOP.
*
*  CHECK lwa_iflos_new-strno NE gs_fldoc_infocus-x-flhdr-strno.
*
*  gs_variables-data_changed = c_true.
*  gs_fldoc_infocus-x-flhdr-strno = lwa_iflos_new-strno.
*  gs_fldoc_infocus-x-flhdr-tplkz = lwa_iflos_new-tplkz.
*  IF gs_fldoc_infocus-x-flhdr-updkz NE c_updkz_new.
*    gs_fldoc_infocus-x-flhdr-updkz = c_updkz_update.
*  ENDIF.

ENDFORM.                    "fcode_lbfo
*&---------------------------------------------------------------------*
*&      Form  FL_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fl_authority_check  USING    lwa_flhdr TYPE /agri/s_glflot
                                  lv_activity TYPE activ_auth
                         CHANGING lv_subrc TYPE sy-subrc.

  IF lv_activity EQ c_authorization_activity-release.
    CHECK lwa_flhdr-kfrst EQ space OR lwa_flhdr-kfrst EQ 'R'.
  ENDIF.

  AUTHORITY-CHECK OBJECT 'V_AB_FLTYP'
           ID 'TPLKZ' FIELD lwa_flhdr-tplkz
           ID 'TPLVL' FIELD lwa_flhdr-tplvl
           ID 'ACTVT' FIELD lv_activity.
  lv_subrc = sy-subrc.

ENDFORM.                    " FL_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  FCODE_PLANT_DELETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_plant_delete .

  DATA: lt_rows    TYPE lvc_t_row,
        lwa_row    TYPE lvc_s_row,
        lv_answer  TYPE c,
        lwa_plants LIKE LINE OF gt_plants.

  FIELD-SYMBOLS: <lwa_plant_modi> TYPE lvc_s_modi,
                 <lwa_plant>      TYPE /agri/s_glflppl.

  CALL METHOD ref_plants_grid->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.

  CHECK lt_rows IS NOT INITIAL.

  popup_to_confirm TEXT-007 TEXT-031 c_true lv_answer.

  IF lv_answer EQ '1'  .

    gs_variables-refresh_plants_grid = c_true.
    SORT lt_rows BY index DESCENDING.

    LOOP AT lt_rows INTO lwa_row.

      LOOP AT gt_plant_mod_rows ASSIGNING <lwa_plant_modi> WHERE row_id GE lwa_row-index.
        IF <lwa_plant_modi>-row_id NE lwa_row-index.
          <lwa_plant_modi>-row_id = <lwa_plant_modi>-row_id - 1.
        ELSE.
          DELETE gt_plant_mod_rows WHERE row_id EQ <lwa_plant_modi>-row_id.
        ENDIF.
      ENDLOOP.

      READ TABLE gt_plants INTO lwa_plants INDEX lwa_row-index.
      DELETE gs_fldoc_infocus-x-flppl INDEX lwa_row-index.
      READ TABLE gs_fldoc_infocus-y-flppl ASSIGNING <lwa_plant>
                                          WITH KEY tplnr_fl = lwa_plants-tplnr_fl
                                                   pwerk = lwa_plants-pwerk
                                                   route = lwa_plants-route
                                        BINARY SEARCH.
      IF sy-subrc EQ 0.
        <lwa_plant>-updkz = c_updkz_delete.
      ENDIF.

      DELETE gt_plants INDEX lwa_row-index.
      UNASSIGN: <lwa_plant>.

    ENDLOOP.
  ENDIF.

ENDFORM.                    " FCODE_PLANT_DELETE
*&---------------------------------------------------------------------*
*&      Form  fcode_ppli
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_ppli.
  APPEND INITIAL LINE TO gt_plants.
  CALL METHOD ref_plants_grid->refresh_table_display
    EXCEPTIONS
      finished = 1
      OTHERS   = 2.
  IF sy-subrc <> 0.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CLEAR gs_variables-refresh_plants_grid.
ENDFORM.                    "fcode_ppli
*&---------------------------------------------------------------------*
*&      Form  fcode_mass_hdr_chng
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fcode_mass_hdr_chng.

  PERFORM mass_processing_initialize USING c_fcode-mass_header_change.

ENDFORM.                    "fcode_mass_hdr_chng
*&---------------------------------------------------------------------*
*& Form FL_LABELS_PREPARE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fl_labels_prepare .

  DATA: lwa_fl_label         TYPE /agri/s_glflos,
        lwa_fl_labels_layout TYPE /agri/s_glflos_fcat.

  CLEAR: gt_fl_labels_layout.

  LOOP AT gs_fldoc_infocus-x-flos INTO lwa_fl_label.
    MOVE-CORRESPONDING lwa_fl_label TO lwa_fl_labels_layout.
    IF lwa_fl_labels_layout-strno EQ gs_fldoc_infocus-x-flhdr-strno.
      lwa_fl_labels_layout-actvs = c_true.
    ENDIF.
    PERFORM label_styles_prepare CHANGING lwa_fl_labels_layout.
    APPEND lwa_fl_labels_layout TO gt_fl_labels_layout.
    CLEAR lwa_fl_labels_layout.
  ENDLOOP.

ENDFORM.
FORM fcode_lact.

  DATA: ls_selected_row TYPE lvc_s_row,
        ls_labels_fcat  TYPE /agri/s_glflos_fcat.

  CHECK gt_selected_rows IS NOT INITIAL.
  READ TABLE gt_selected_rows INTO ls_selected_row INDEX 1.
  READ TABLE gt_fl_labels_layout INTO ls_labels_fcat INDEX ls_selected_row-index.

  IF ls_labels_fcat-strno NE gs_fldoc_infocus-x-flhdr-strno AND
     ls_labels_fcat IS NOT INITIAL.
    gs_variables-data_changed = c_true.
    gs_fldoc_infocus-x-flhdr-strno = ls_labels_fcat-strno.
    gs_fldoc_infocus-x-flhdr-tplkz = ls_labels_fcat-tplkz.
    IF gs_fldoc_infocus-x-flhdr-updkz NE c_updkz_new.
      gs_fldoc_infocus-x-flhdr-updkz = c_updkz_update.
    ENDIF.
  ENDIF.

  LEAVE TO SCREEN 0.

ENDFORM.
FORM fcode_lcre.

  MOVE-CORRESPONDING /agri/s_glflot TO */agri/s_glflot.
  gs_variables-new_label = c_true.
  CALL SCREEN 207 STARTING AT 5 5.
  LEAVE TO SCREEN 0.

ENDFORM.
