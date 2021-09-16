*----------------------------------------------------------------------*
***INCLUDE /AGRI/LGLFLMF0P .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  PARTNERS_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM partners_init .

*  DATA: lv_aktyp TYPE akttyp.
*
*  IF gs_variables-document_mode = c_mode_display OR
*     gs_variables-document_mode IS INITIAL.
*    lv_aktyp = c_mode_display.
*  ELSE.
*    lv_aktyp = c_mode_change.
*  ENDIF.
*
*  CALL FUNCTION 'PM_PARTNER_INIT'
*    EXPORTING
*      aktyp = lv_aktyp
*      objnr = gs_fldoc_infocus-x-flhdr-objnr
*      obtyp = 'IFL'
*      pargr = gs_fldoc_infocus-x-flhdr-pargr
**     PARVW_AP             = 'AP'
**     PARVW_INT            = 'ZM'
**     PARVW_KUND           = 'AG'
**     PARVW_LIEF           = 'LF'
**     PARVW_HER            = 'HR'
**     PARVW_VERA           = 'ZM'
**     PARVW_AUTO           = 'ZM'
**     REFRESH              = 'X'
**     VKORG = ' '
**     VTWEG = ' '
**     SPART = ' '
**     EQUNR = ' '
**     TPLNR = ' '
**     READ_ALL             = 'X'
**     SERVICE_FLAG         = ' '
**     TEXT  = ' '
**     NO_SORT              =
**     NO_TABIX_DET         =
**     NO_TEXT_DET          =
**     IP_OBJECT_ARCH       = ' '
**   IMPORTING
**     OFFSET               =
*    .

ENDFORM.                    " PARTNERS_INIT
*&---------------------------------------------------------------------*
*&      Form  PARATNERS_DATA_IMPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*FORM paratners_data_import .
*
*  DATA: lv_partners_changed.
*
*  FIELD-SYMBOLS: <lwa_ihpa> TYPE ihpavb.
*
*  CHECK gs_variables-document_mode NE c_mode_display.
*
*  CALL FUNCTION 'PM_PARTNER_GET_CHANGE_FLAG'
*    EXPORTING
*      id_objnr      = gs_fldoc_infocus-x-flhdr-objnr
**     ID_NO_ADDRESS =
*    IMPORTING
*      flg_change    = lv_partners_changed.
*  IF lv_partners_changed IS NOT INITIAL.
*
*    CALL FUNCTION 'PM_PARTNER_GET'
*      EXPORTING
*        objnr                 = gs_fldoc_infocus-x-flhdr-objnr
**     I_FROM_DB             =
**   IMPORTING
**     DIADR_WA              =
**     ET_IHPA_TIME          =
*      TABLES
*        ihpa_tab              = gs_fldoc_infocus-x-ihpa
**     IHPA_TAB_BEFORE       =
*              .
*    gs_variables-data_changed = c_true.
*
*    LOOP AT gs_fldoc_infocus-y-ihpa ASSIGNING <lwa_ihpa>.
*      READ TABLE gs_fldoc_infocus-x-ihpa TRANSPORTING NO FIELDS
*            WITH KEY objnr = gs_fldoc_infocus-x-flhdr-objnr
*                     parvw = <lwa_ihpa>-parvw
*                     parnr = <lwa_ihpa>-parnr.
*      IF sy-subrc NE 0.
*        <lwa_ihpa>-updkz = c_updkz_delete.
*      ENDIF.
*    ENDLOOP.
*
*  ENDIF.
*
*ENDFORM.                    " PARATNERS_DATA_IMPORT
*&---------------------------------------------------------------------*
*&      Form  PARTNERS_FCODE_EXECUTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM partners_fcode_execute USING lv_ucomm TYPE sy-ucomm.

*  DATA: lv_fcode TYPE fcode.
*
*  lv_fcode = lv_ucomm.
*
*  CALL FUNCTION 'PM_PARTNER_FCODE'
*    EXPORTING
*      fcode = lv_fcode.

ENDFORM.                    " PARTNERS_FCODE_EXECUTE

*&---------------------------------------------------------------------*
*&      Form  PORTAL_FLHDR_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM portal_flhdr_update  CHANGING ls_flhdr TYPE /agri/s_glflot
                                   lv_subrc TYPE sy-subrc.

  messages_init.
  messages_collect_all.
  gs_variables-initiator = c_log_initiator-change.
  messages_initiator_set gs_variables-initiator
  c_object-log c_log_subobject-change.

  CLEAR gs_fldoc_infocus-x-flhdr-fltyp.

  IF lv_subrc EQ 0.
    gs_fldoc_infocus-x-flhdr = ls_flhdr.
  ELSE.
    gs_variables-header_display = c_true.
  ENDIF.

  messages_init.
  CLEAR gs_variables-initiator.

ENDFORM.                    " PORTAL_FLHDR_UPDATE
*&---------------------------------------------------------------------*
*&      Form  PLANTS_DATA_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM plants_data_prepare .

  DATA: lv_lines       TYPE i,
        lwa_flppl      TYPE /agri/s_glflppl,
        lwa_plant_fcat TYPE /agri/s_glflppl_fcat.

  CHECK ref_plants_grid IS INITIAL OR
        gs_variables-refresh_plants_grid IS NOT INITIAL.

  CLEAR: gt_plants.

  LOOP AT gs_fldoc_infocus-x-flppl INTO lwa_flppl.

    CLEAR lwa_plant_fcat.
    MOVE-CORRESPONDING lwa_flppl TO lwa_plant_fcat.
    PERFORM plant_details_get CHANGING lwa_plant_fcat.
    keytext_get_simple '/AGRI/GLRTHDR' 'ROUTE' lwa_plant_fcat-route
                       lwa_plant_fcat-descr.
    PERFORM plant_styles_prepare CHANGING lwa_plant_fcat.
    APPEND lwa_plant_fcat TO gt_plants.

  ENDLOOP.

  IF gs_variables-document_mode NE c_mode_display.
    DESCRIBE TABLE gt_plants LINES lv_lines.
    IF lv_lines GE 10.
      lv_lines = lv_lines MOD 10.
      lv_lines = 10 - lv_lines.
    ELSE.
      lv_lines = 10 - lv_lines.
    ENDIF.
    DO lv_lines TIMES.
      APPEND INITIAL LINE TO gt_plants.
    ENDDO.
  ENDIF.

ENDFORM.                    " PLANTS_DATA_PREPARE
*&---------------------------------------------------------------------*
*&      Form  PLANTS_GRID_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM plants_grid_update .

  DATA: lv_modified,
        lv_valid,
        lv_subrc    TYPE sy-subrc,
        lwa_flppl   TYPE /agri/s_glflppl,
        lwa_plants  TYPE /agri/s_glflppl_fcat,
        lwa_mod_row TYPE lvc_s_modi,
        lv_tabix    TYPE sy-tabix.

  FIELD-SYMBOLS: <lwa_flppl> TYPE /agri/s_glflppl.
*                 <lwa_plants> TYPE /agri/s_glflppl_fcat.

  CHECK gs_variables-document_mode NE c_mode_display.

  IF ok_code EQ c_fcode-plant_delete.
    PERFORM fcode_plant_delete.
    CLEAR ok_code.
  ENDIF.

  lv_modified = ref_plants_grid->data_modified_check( ).
  IF lv_modified EQ c_true OR
     gs_variables-manual_changes IS NOT INITIAL.
    CALL METHOD ref_plants_grid->check_changed_data
      IMPORTING
        e_valid = lv_valid.
    IF lv_valid IS INITIAL.
      CLEAR ok_code.
      EXIT.
    ELSE.
      gs_variables-refresh_plants_grid = c_true.
    ENDIF.
  ENDIF.

  LOOP AT gt_plant_mod_rows INTO lwa_mod_row.

    lv_tabix = sy-tabix.
    READ TABLE gt_plants INTO lwa_plants
                                  INDEX lwa_mod_row-row_id.
    CHECK lwa_plants IS NOT INITIAL.

    MOVE-CORRESPONDING lwa_plants TO lwa_flppl.
    PERFORM plant_data_check USING lwa_flppl
                                   lv_subrc.
    IF lv_subrc NE 0.
      gs_variables-errors = c_true.
      CONTINUE.
    ENDIF.

    READ TABLE gs_fldoc_infocus-x-flppl ASSIGNING <lwa_flppl>
                              WITH KEY pwerk = lwa_plants-pwerk
                                       route = lwa_plants-route
                                       tplnr_fl = lwa_plants-tplnr_fl.
    IF sy-subrc EQ 0.
      IF lwa_flppl NE <lwa_flppl>.
        MOVE lwa_flppl TO <lwa_flppl>.
        IF <lwa_flppl>-updkz NE c_updkz_new.
          gs_variables-data_changed = c_true.
          <lwa_flppl>-updkz = c_updkz_update.
        ENDIF.
      ENDIF.
    ELSE.
      gs_variables-refresh_plants_grid = c_true.
      gs_variables-data_changed = c_true.
      lwa_plants-tplnr_fl = gs_fldoc_infocus-tplnr_fl.
      lwa_plants-updkz = c_updkz_new.
      MOVE-CORRESPONDING lwa_plants TO lwa_flppl.
      APPEND lwa_flppl TO gs_fldoc_infocus-x-flppl.
    ENDIF.

    DELETE gt_plant_mod_rows INDEX lv_tabix.

  ENDLOOP.

  IF lv_subrc IS INITIAL.
    CLEAR: gs_variables-manual_changes.
  ELSE.
    CLEAR: ok_code.
  ENDIF.

ENDFORM.                    " PLANTS_GRID_UPDATE
*&---------------------------------------------------------------------*
*&      Form  PLANT_DATA_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM plant_data_check  USING lwa_flppl TYPE /agri/s_glflppl
                             lv_subrc.

  DATA: lv_cnt   TYPE i,
        lv_pwerk TYPE /agri/glpwerk,
        lv_route TYPE /agri/gl_route.

  IF lwa_flppl-pwerk IS INITIAL.
    lv_subrc = 4.
    MESSAGE ID '/AGRI/GLFL' TYPE 'E' NUMBER '020' INTO sy-msgli.
    message_simple space.
    EXIT.
  ENDIF.

  IF lwa_flppl-route IS INITIAL.
    lv_subrc = 4.
    MESSAGE ID '/AGRI/GLFL' TYPE 'E' NUMBER '020' INTO sy-msgli.
    message_simple space.
    EXIT.
  ENDIF.

  LOOP AT gt_plants TRANSPORTING NO FIELDS
                             WHERE pwerk EQ lwa_flppl-pwerk
                               AND route EQ lwa_flppl-route.
    lv_cnt = lv_cnt + 1.
    IF lv_cnt GT 1.
      lv_subrc = 4.
      MESSAGE ID '/AGRI/GLCOM' TYPE 'E' NUMBER '005' INTO sy-msgli.
      message_simple space.
      EXIT.
    ENDIF.
  ENDLOOP.

  CLEAR lv_cnt.
  LOOP AT gt_plants TRANSPORTING NO FIELDS
                             WHERE dfult IS NOT INITIAL.
    lv_cnt = lv_cnt + 1.
    IF lv_cnt GT 1.
      lv_subrc = 4.
      MESSAGE ID '/AGRI/GLFL' TYPE 'E' NUMBER '040' INTO sy-msgli.
      message_simple space.
      EXIT.
    ENDIF.
  ENDLOOP.

  SELECT SINGLE pwerk
           FROM /agri/glpphdr  "#EC CI_GENBUFF
           INTO lv_pwerk
          WHERE pwerk EQ lwa_flppl-pwerk.
  IF sy-subrc NE 0.
    lv_subrc = 4.
    MESSAGE ID '/AGRI/GLFL' TYPE 'E' NUMBER '039' INTO sy-msgli.
    message_simple space.
    EXIT.
  ENDIF.

  SELECT SINGLE route
           FROM /agri/glrthdr
           INTO lv_route
          WHERE route EQ lwa_flppl-route.
  IF sy-subrc NE 0.
    lv_subrc = 4.
    MESSAGE ID '/AGRI/GLFL' TYPE 'E' NUMBER '045'
       WITH lwa_flppl-route INTO sy-msgli.
    message_simple space.
    EXIT.
  ENDIF.

  IF lwa_flppl-dstnc IS NOT INITIAL AND
     lwa_flppl-dsunt IS INITIAL.
    lv_subrc = 4.
    MESSAGE ID '/AGRI/GLFL' TYPE 'E' NUMBER '042'
       WITH lwa_flppl-pwerk
       INTO sy-msgli.
    message_simple space.
    EXIT.
  ENDIF.

  IF lwa_flppl-dstnc IS INITIAL AND
     lwa_flppl-dsunt IS NOT INITIAL.
    lv_subrc = 4.
    MESSAGE ID '/AGRI/GLFL' TYPE 'E' NUMBER '043'
       WITH lwa_flppl-pwerk
       INTO sy-msgli.
    message_simple space.
    EXIT.
  ENDIF.

  PERFORM check_dimension USING lwa_flppl-dsunt
                                c_dimensions-length
                                c_true
                       CHANGING lv_subrc.
  IF lv_subrc NE 0.
    EXIT.
  ENDIF.

ENDFORM.                    " PLANT_DATA_CHECK
*&---------------------------------------------------------------------*
*&      Form  PLANT_STYLES_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM plant_styles_prepare  CHANGING lwa_plant
                              TYPE /agri/s_glflppl_fcat.

  DATA: lwa_style TYPE lvc_s_styl.

  REFRESH: lwa_plant-styles.
  IF lwa_plant-pwerk IS NOT INITIAL.
    lwa_style-fieldname = 'PWERK'.
    lwa_style-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT lwa_style INTO TABLE lwa_plant-styles.
  ENDIF.
  IF lwa_plant-route IS NOT INITIAL.
    lwa_style-fieldname = 'ROUTE'.
    lwa_style-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT lwa_style INTO TABLE lwa_plant-styles.
  ENDIF.

ENDFORM.                    " PLANT_STYLES_PREPARE
*&---------------------------------------------------------------------*
*&      Form  PLANT_DETAILS_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LWA_PLANT_FCAT  text
*----------------------------------------------------------------------*
FORM plant_details_get CHANGING
                      lwa_plant TYPE /agri/s_glflppl_fcat.

  DATA: lwa_t001w TYPE t001w.

  CHECK lwa_plant-pwerk IS NOT INITIAL.

  CALL FUNCTION '/AGRI/G_GET_PLANT_DETAILS'
    EXPORTING
      i_werks         = lwa_plant-pwerk
*     I_GET_ADDRESS   = '  '
    IMPORTING
*     E_NAME          =
*     E_ADDR1         =
*     E_ADDR2         =
*     E_ADDR3         =
*     E_ADDR4         =
      es_t001w        = lwa_t001w
    EXCEPTIONS
      not_found       = 1
      parameter_error = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    EXIT.
* Implement suitable error handling here
  ENDIF.

  MOVE-CORRESPONDING lwa_t001w TO lwa_plant.

ENDFORM.                    " PLANT_DETAILS_GET

*&---------------------------------------------------------------------*
*& Form PARTNERS_SUBSCREEN_SET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM partners_subscreen_set .

  gt_xflptr_chng = gs_fldoc_infocus-x-ihpa.
  gt_yflptr_chng = gs_fldoc_infocus-y-ihpa.

  CALL FUNCTION '/AGRI/G_PAR_SUBSCREEN_IMPORT'
    EXPORTING
      i_objtyp      = c_object-bor
      i_pargr_h     = gs_fldoc_infocus-x-flhdr-pargr
*     I_PARGR_I     =
*     I_PRIMARY_PARTNER_ROLE =
*     i_vkorg       =
*     i_vtweg       =
*     i_spart       =
*     I_EKORG       =
*     I_BSTYP       =
      i_mode        = gs_variables-document_mode
      i_keyfield    = 'PTRNO'
*     I_KEYFIELD1   =
      i_keyvalue    = gs_fldoc_infocus-x-flhdr-ptrno
*     I_KEYVALUE1   =
****ESP6 Task #30408 - Partner Engine Version Control Changes
      i_objtp       = /agri/if_global_constants=>mc_switch_object_type-terrain
****
    TABLES
      t_xpartners   = gs_fldoc_infocus-x-ihpa
      t_ypartners   = gs_fldoc_infocus-y-ihpa
    CHANGING
      c_program     = gs_variables-program
      c_subscreen   = gs_variables-subs_details
      c_change_flag = gs_variables-partners_changed.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PARTNERS_MAINTAIN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_FLDOC_INFOCUS_X_IHPA
*&      --> GS_FLDOC_INFOCUS_Y_IHPA
*&      --> LWA_IHPA_PARVW
*&      --> LV_KTONR
*&      --> LV_KTONR_OLD
*&      --> LWA_IHPA_ADRNR
*&      --> LWA_IHPA_ADRDA
*&      --> C_TRUE
*&      <-- LV_SUBRC
*&---------------------------------------------------------------------*
FORM partners_maintain TABLES lt_xflptr
                              lt_yflptr
                        USING lv_parvw
                              lv_ktonr_new
                              lv_ktonr
                              lv_adrnr
                              lv_adrda
                              lv_collect_messages
                     CHANGING lv_subrc.

  DATA: lv_tabname    TYPE tabname,
        lv_tplnr      TYPE /agri/gltplnr_fl,
        ls_address    TYPE addr1_data,
        lt_flptr_tmp  TYPE /agri/t_glihpa,
        lt_yflptr_tmp TYPE /agri/t_glihpa.

  CLEAR lv_subrc.

  IF gs_variables-document_mode EQ c_mode_create AND
                   gs_fldoc_infocus-x-flhdr-tplnr_fl IS INITIAL.
    lv_tplnr = 'NEW'.
  ELSE.
    lv_tplnr = gs_fldoc_infocus-x-flhdr-tplnr_fl.
  ENDIF.

  lt_flptr_tmp = lt_xflptr[].
  lt_flptr_tmp = lt_yflptr[].

  IF lv_adrda EQ 'E'.
***Task#32588  S/4 1709 -  ATC Checks
*    SELECT SINGLE * FROM adrc INTO CORRESPONDING FIELDS OF ls_address
    lv_tabname = 'ADRC'.
    SELECT SINGLE * FROM (lv_tabname) INTO CORRESPONDING FIELDS OF ls_address
***
      WHERE addrnumber = lv_adrnr.
  ENDIF.

  CALL FUNCTION '/AGRI/G_PARTNER_MAINTAIN'
    EXPORTING
      i_objtyp                    = c_object-bor
*     I_PRIMARY_PARTNER_ROLE      =
      i_pargr_h                   = gs_fldoc_infocus-x-flhdr-pargr
*     I_PARGR_I                   =
      i_keyfield                  = 'TPLNR_FL'
*     I_KEYFIELD1                 =
      i_keyvalue                  = lv_tplnr
*     I_KEYVALUE1                 =
      i_parvw                     = lv_parvw
      i_ktonr                     = lv_ktonr_new
      i_ktonr_old                 = lv_ktonr
*     i_vkorg                     = lv_vkorg
*     i_vtweg                     = lv_vtweg
*     i_spart                     = lv_spart
*     I_EKORG                     =
*     I_BSTYP                     =
*     I_OPERATION                 =
*     I_ORIGIN                    =
      i_adrnr                     = lv_adrnr
      i_adrda                     = lv_adrda
      is_address                  = ls_address
****ESP6 Task #29928 - Partner Engine Changes
      i_objtp                     = /agri/if_global_constants=>mc_switch_object_type-terrain
****
    TABLES
      t_xpartners                 = lt_xflptr[]
      t_ypartners                 = lt_yflptr[]
    CHANGING
      c_change_flag               = gs_variables-data_changed
    EXCEPTIONS
      partner_function_is_empty   = 1
      partner_number_is_empty     = 2
      partner_function_is_invalid = 3
      inconsistent_data           = 4
      partner_change_not_allowed  = 5
      partner_delete_not_allowed  = 6
      OTHERS                      = 7.
  IF sy-subrc NE 0
  AND NOT sy-msgid IS INITIAL
  AND NOT sy-msgty IS INITIAL.
    lv_subrc = sy-subrc.
    IF lv_collect_messages EQ c_true.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
      message_simple space.
    ENDIF.
  ENDIF.

ENDFORM.
