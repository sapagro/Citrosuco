*----------------------------------------------------------------------*
***INCLUDE LZVX_CROP_SEASON_OPF01.
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
*& Form global_data_initialize
*&---------------------------------------------------------------------*
FORM global_data_initialize.

*-- Clear Global data
  CLEAR: gv_tplnr, gv_iwerk, gv_bwtar, gv_datab, gt_cskey,
         gt_cskey_upd, gt_cpros, gt_fmfphdr, gt_cs_key,
         gv_poflag, gv_po_error.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form validate_input_data
*&---------------------------------------------------------------------*
FORM validate_input_data CHANGING cs_indata   TYPE zvxs_crop_input
                                  ct_messages TYPE zvxtt_message_gprolog.

*-- Check input if any mandatory fields are missing
  IF cs_indata-tplnr      IS INITIAL OR
     cs_indata-cmnum      IS INITIAL OR
     cs_indata-varia      IS INITIAL OR
     cs_indata-season     IS INITIAL OR
     cs_indata-gyear      IS INITIAL OR
     cs_indata-aarea      IS INITIAL OR
     cs_indata-datab      IS INITIAL OR
     cs_indata-z_pro_proj IS INITIAL OR
     cs_indata-indfield   IS INITIAL.

*-- Build messages
    PERFORM build_msgs USING gc_constants-msgty_error '001'
                             space space
                    CHANGING ct_messages[].
  ENDIF.

  CHECK ct_messages[] IS INITIAL.

*-- If crop_ex is other than X or Space then throw error
  IF ( cs_indata-crop_ex  NE gc_constants-flag ) AND
     ( cs_indata-crop_ex  NE space ).

*-- Build messages
    PERFORM build_msgs USING gc_constants-msgty_error '068'
                             space space
                    CHANGING ct_messages[].

  ENDIF.

  CHECK ct_messages[] IS INITIAL.

*-- If crop exchange is 'X' exchange crop field is mandatory
  IF ( cs_indata-indfield EQ gc_constants-insert OR
       cs_indata-indfield EQ gc_constants-update ) AND
     ( cs_indata-crop_ex  EQ gc_constants-flag   ).
    IF cs_indata-crop_o  IS INITIAL.

*-- Build messages
      PERFORM build_msgs USING gc_constants-msgty_error '001'
                               space space
                      CHANGING ct_messages[].
    ENDIF.
  ENDIF.

  CHECK ct_messages[] IS INITIAL.

*-- Validate Indicator Field
  IF cs_indata-indfield NE gc_constants-insert AND
     cs_indata-indfield NE gc_constants-delete AND
     cs_indata-indfield NE gc_constants-update.

*-- Build messages
    PERFORM build_msgs USING gc_constants-msgty_error '042'
                             space space
                    CHANGING ct_messages[].
  ENDIF.

  CHECK ct_messages[] IS INITIAL.

*-- Validate Terrain
  PERFORM validate_terrain USING cs_indata
                        CHANGING ct_messages[].

  CHECK ct_messages[] IS INITIAL.

*-- Convert crop date to internal format and validate
  PERFORM convert_date CHANGING cs_indata-datab
                                ct_messages[].

  CHECK ct_messages[] IS INITIAL.

*-- Validate Season, Area and year fields
  PERFORM validate_numc USING cs_indata
                     CHANGING ct_messages[].

  CHECK ct_messages[] IS INITIAL.

*-- Validate Crop
  PERFORM validate_crop CHANGING cs_indata
                                 ct_messages[].

  CHECK ct_messages[] IS INITIAL.

*-- Validate exchange crop and Original yard order
  IF ( cs_indata-indfield EQ gc_constants-insert OR
       cs_indata-indfield EQ gc_constants-update ) AND
     ( cs_indata-crop_ex EQ gc_constants-flag ).

    PERFORM validate_crop_exchange USING cs_indata
                                CHANGING ct_messages[].

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form validate_terrain
*&---------------------------------------------------------------------*
FORM validate_terrain USING ps_indata   TYPE zvxs_crop_input
                   CHANGING ct_messages TYPE zvxtt_message_gprolog.

*-- Local variables
  DATA: lv_msgv1 TYPE msgv1.

*-- Conversion terrain to SAP internal format.
  CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
    EXPORTING
      input      = ps_indata-tplnr
    IMPORTING
      output     = gv_tplnr
    EXCEPTIONS
      not_found  = 1
      not_active = 2
      OTHERS     = 3.
  IF sy-subrc NE 0.
*-- Build messages
    lv_msgv1 = ps_indata-tplnr.
    PERFORM build_msgs USING gc_constants-msgty_error '015'
                             lv_msgv1 space
                    CHANGING ct_messages[].
  ENDIF.

*-- Validate terrain & Get Plant for given terrain
  IF gv_tplnr IS NOT INITIAL.
    SELECT SINGLE iwerk
      FROM /agri/glflot
      INTO gv_iwerk
     WHERE tplnr_fl EQ gv_tplnr
       AND loevm    EQ space.
    IF sy-subrc NE 0.
*-- Build messages
      lv_msgv1 = ps_indata-tplnr.
      PERFORM build_msgs USING gc_constants-msgty_error '015'
                               lv_msgv1 space
                      CHANGING ct_messages[].
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form convert_date
*&---------------------------------------------------------------------*
FORM convert_date CHANGING cv_date     TYPE char10
                           ct_messages TYPE zvxtt_message_gprolog.

*-- Local variables
  DATA: lv_date  TYPE datum,
        lv_msgv1 TYPE msgv1.

*-- Convert date to SAP internal format
  CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
    EXPORTING
      date_external            = cv_date
    IMPORTING
      date_internal            = lv_date
    EXCEPTIONS
      date_external_is_invalid = 1
      OTHERS                   = 2.
  IF sy-subrc NE 0.
*-- Build messages
    lv_msgv1 = cv_date.
    PERFORM build_msgs USING gc_constants-msgty_error '013'
                             lv_msgv1 space
                    CHANGING ct_messages[].
  ELSE.
    cv_date  = lv_date.
    gv_datab = lv_date.
    CONDENSE cv_date.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form validate_crop
*&---------------------------------------------------------------------*
FORM validate_crop CHANGING cs_indata   TYPE zvxs_crop_input
                            ct_messages TYPE zvxtt_message_gprolog.

*-- Local Variables
  DATA: lv_msgv1 TYPE msgv1,
        lv_msgv2 TYPE msgv2.

*-- Validate Crop data whether crop is active or not
*   and get Yard material
  SELECT SINGLE cmnum exyld ymatnr
    FROM /agri/glcmhdr
    INTO gs_glcmhdr
   WHERE cmnum EQ cs_indata-cmnum
     AND loevm EQ space.
  IF sy-subrc NE 0.
*-- Build messages
    lv_msgv1 = cs_indata-cmnum.
    PERFORM build_msgs USING gc_constants-msgty_error '017'
                             lv_msgv1 space
                    CHANGING ct_messages[].
  ENDIF.

  CHECK ct_messages[] IS INITIAL.

*-- If no yard material is maintained in crop master then throw error
  IF gs_glcmhdr-ymatnr IS INITIAL.
*-- Build messages
    lv_msgv1 = cs_indata-cmnum.
    PERFORM build_msgs USING gc_constants-msgty_error '063'
                             lv_msgv1 space
                    CHANGING ct_messages[].

  ENDIF.

  CHECK ct_messages[] IS INITIAL.

*-- Fetch all CPROS assignt to crop and variant.
  ##WARN_OK
  SELECT cmnum varia cpros                         "#EC CI_NO_TRANSFORM
    FROM /agri/glcmprs
    INTO TABLE gt_cpros
   WHERE cmnum EQ cs_indata-cmnum
     AND varia EQ cs_indata-varia
   ORDER BY cmnum varia cpros.
  IF sy-subrc NE 0.
*-- Build messages
    lv_msgv1 = cs_indata-cmnum.
    lv_msgv2 = cs_indata-varia.
    PERFORM build_msgs USING gc_constants-msgty_error '040'
                             lv_msgv1 lv_msgv2
                    CHANGING ct_messages[].
  ENDIF.

  CHECK ct_messages[] IS INITIAL.

*-- Validate season
  SELECT SINGLE season
    FROM /agri/glseason
    INTO @DATA(lv_season)         ##NEEDED
   WHERE season EQ @cs_indata-season.
  IF sy-subrc NE 0.
*-- Build messages
    lv_msgv1 = cs_indata-season.
    PERFORM build_msgs USING gc_constants-msgty_error '064'
                             lv_msgv1 space
                    CHANGING ct_messages[].
  ENDIF.

  CHECK ct_messages[] IS INITIAL.

*-- Validate Crop & variant with in validate date or not and get counter
  SELECT tplnr_fl contr season astat yaufnr
    FROM /agri/glflcma
    INTO TABLE gt_cskey_upd
   WHERE tplnr_fl EQ gv_tplnr
     AND cmnum    EQ cs_indata-cmnum
     AND varia    EQ cs_indata-varia
     AND season   EQ cs_indata-season
     AND ( datab  LE cs_indata-datab
     AND   datbi  GE cs_indata-datab )
     AND iwerk    EQ gv_iwerk
     AND loevm    EQ space.

  CASE cs_indata-indfield.

    WHEN gc_constants-delete.
      IF gt_cskey_upd IS INITIAL.
*-- Build messages
        lv_msgv1 = cs_indata-cmnum.
        PERFORM build_msgs USING gc_constants-msgty_error '034'
                                 lv_msgv1 space
                        CHANGING ct_messages[].
      ELSE.
        READ TABLE gt_cskey_upd INTO DATA(ls_cskey) INDEX 1.
        IF ( sy-subrc EQ 0 ) AND
           ( ls_cskey-astat NE gc_constants-astat_a ).
*-- Build messages
          PERFORM build_msgs USING gc_constants-msgty_error '067'
                                   space space
                          CHANGING ct_messages[].
        ENDIF.
      ENDIF.

    WHEN gc_constants-update.

      IF cs_indata-crop_ex EQ space.
*-- Normal crop season update
        IF gt_cskey_upd IS INITIAL.
*-- Build messages
          lv_msgv1 = cs_indata-cmnum.
          PERFORM build_msgs USING gc_constants-msgty_error '034'
                                   lv_msgv1 space
                          CHANGING ct_messages[].
        ELSE.
          READ TABLE gt_cskey_upd INTO ls_cskey INDEX 1.
          IF ( sy-subrc EQ 0 ) AND
             ( ls_cskey-astat NE gc_constants-astat_a ).
*-- Build messages
            PERFORM build_msgs USING gc_constants-msgty_error '067'
                                     space space
                            CHANGING ct_messages[].
          ENDIF.
        ENDIF.
      ELSEIF cs_indata-crop_ex EQ gc_constants-flag.
*-- Crop season creation scenario
        READ TABLE gt_cskey_upd INTO ls_cskey INDEX 1.
        IF ( sy-subrc EQ 0 ) AND
           ( ls_cskey-astat NE gc_constants-astat_a ).
*-- Build messages
          PERFORM build_msgs USING gc_constants-msgty_error '105'
                                   space space
                          CHANGING ct_messages[].
        ENDIF.
      ENDIF.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form validate_numc
*&---------------------------------------------------------------------*
FORM validate_numc USING ps_indata   TYPE zvxs_crop_input
                CHANGING ct_messages TYPE zvxtt_message_gprolog.

*-- Local Variables
  DATA: lv_msgv1  TYPE msgv1,
        lv_gyear  TYPE char10,
        lv_season TYPE char10.

*-- Validate SEASON
  lv_season = ps_indata-season.
  CALL FUNCTION 'CATS_NUMERIC_INPUT_CHECK'
    EXPORTING
      input      = lv_season
    IMPORTING
      output     = lv_season
    EXCEPTIONS
      no_numeric = 1
      OTHERS     = 2.
  IF sy-subrc NE 0.
*-- Build messages
    lv_msgv1 = ps_indata-season.
    PERFORM build_msgs USING gc_constants-msgty_error '049'
                             lv_msgv1 space
                    CHANGING ct_messages[].
  ENDIF.


*-- Validate GYEAR
  lv_gyear = ps_indata-gyear.
  CALL FUNCTION 'CATS_NUMERIC_INPUT_CHECK'
    EXPORTING
      input      = lv_gyear
    IMPORTING
      output     = lv_gyear
    EXCEPTIONS
      no_numeric = 1
      OTHERS     = 2.
  IF sy-subrc NE 0.
*-- Build messages
    lv_msgv1 = ps_indata-gyear.
    PERFORM build_msgs USING gc_constants-msgty_error '050'
                             lv_msgv1 space
                    CHANGING ct_messages[].
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form validate_crop_exchange
*&---------------------------------------------------------------------*
FORM validate_crop_exchange USING ps_indata   TYPE zvxs_crop_input
                         CHANGING ct_messages TYPE zvxtt_message_gprolog.

*-- Local Variables
  DATA: lv_msgv1 TYPE msgv1.

*-- Validate Crop data whether Exchange crop is active or not
  SELECT SINGLE cmnum
    FROM /agri/glcmhdr
    INTO @DATA(lv_cmnum)   ##NEEDED
   WHERE cmnum EQ @ps_indata-crop_o
     AND loevm EQ @space.
  IF sy-subrc NE 0.
*-- Build messages
    lv_msgv1 = ps_indata-crop_o.
    PERFORM build_msgs USING gc_constants-msgty_error '053'
                             lv_msgv1 space
                    CHANGING ct_messages[].
  ENDIF.

*-- Validate original yard order
*-- Get the Yard order for the original Crop CROP_O
  SELECT tplnr_fl contr season astat yaufnr
    FROM /agri/glflcma
    INTO TABLE gt_cskey
   WHERE tplnr_fl EQ gv_tplnr
     AND cmnum    EQ ps_indata-crop_o
     AND season   EQ ps_indata-season
     AND ( datab  LE ps_indata-datab
     AND   datbi  GE ps_indata-datab )
     AND iwerk    EQ gv_iwerk
     AND loevm    EQ space.
  IF gt_cskey IS INITIAL.
*-- Build messages
    lv_msgv1 = ps_indata-crop_o.
    PERFORM build_msgs USING gc_constants-msgty_error '160'
                             lv_msgv1 space
                    CHANGING ct_messages[].
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form create_crop_season
*&---------------------------------------------------------------------*
FORM create_crop_season USING ps_indata    TYPE zvxs_crop_input
                     CHANGING ct_messages  TYPE zvxtt_message_gprolog
                              ct_csdoc_crt TYPE /agri/t_glcs_doc.

*-- Local internal tables
  DATA: lt_flcma    TYPE /agri/t_glflcma,
        lt_messages TYPE /agri/t_gprolog,

*-- Local work area
        ls_flcma    TYPE /agri/s_glflcma,
        ls_mesg     TYPE zvxs_message_gprolog,

*-- Local Variables
        lv_msgv1    TYPE msgv1,
        lv_msgv2    TYPE msgv2.

  ls_flcma-tplnr_fl  = gv_tplnr.
  ls_flcma-cmnum     = ps_indata-cmnum.
  ls_flcma-varia     = ps_indata-varia.
  ls_flcma-season    = ps_indata-season.
  ls_flcma-datab     = ps_indata-datab.
  ls_flcma-aarea     = ps_indata-aarea.
  ls_flcma-ymatnr    = gs_glcmhdr-ymatnr.
  ls_flcma-zzpro_yld = ps_indata-z_pro_proj.
  ls_flcma-updkz     = gc_constants-insert.
  APPEND ls_flcma TO lt_flcma.

*-- Create crop season with RFC input data
  ##FM_SUBRC_OK
  CALL FUNCTION '/AGRI/GLCS_CREATE'
    EXPORTING
      it_flcma                = lt_flcma[]
    IMPORTING
      et_csdoc                = ct_csdoc_crt[]
      et_messages             = lt_messages[]
    EXCEPTIONS
      no_documents_to_process = 1
      no_authorization        = 2
      creation_failed         = 3
      OTHERS                  = 4.

  IF ct_csdoc_crt IS NOT INITIAL.
*-- Build messages
    READ TABLE ct_csdoc_crt INTO DATA(ls_csdoc_crt) INDEX 1.
    lv_msgv1 = ps_indata-tplnr.
    lv_msgv2 = ls_csdoc_crt-contr.
    PERFORM build_msgs USING gc_constants-msgty_success '052'
                             lv_msgv1 lv_msgv2
                    CHANGING ct_messages[].
  ELSE.
    LOOP AT lt_messages INTO DATA(ls_message).
      CLEAR : ls_mesg.
      ls_mesg = CORRESPONDING #( ls_message ).
      APPEND ls_mesg TO ct_messages.
    ENDLOOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form create_yard_order
*&---------------------------------------------------------------------*
FORM create_yard_order  USING ps_indata     TYPE zvxs_crop_input
                              pt_csdoc_crt  TYPE /agri/t_glcs_doc
                     CHANGING ct_messages   TYPE zvxtt_message_gprolog
                              cv_yard_order TYPE aufnr
                              cs_outdata    TYPE zvxs_crop_output.

*-- Local work area
  ##NEEDED
  DATA: ls_order_comm   TYPE /agri/s_glpocomm,
        ls_mesg         TYPE zvxs_message_gprolog,
        ls_messtab      TYPE bdcmsgcoll,

*-- Local internal tables
        lt_messtab      TYPE tab_bdcmsgcoll,

*-- Local Variables
        lv_msgv1        TYPE msgv1,
        lv_order_number TYPE aufnr.

  READ TABLE pt_csdoc_crt INTO DATA(ls_csdoc_crt) INDEX 1.

*-- Prepare Goods recipient
  PERFORM good_recipient USING ps_indata
                      CHANGING gv_bwtar.

*-- Export the Memory variable(Valuation type) to BADI: WORKORDER_UPDATE
  EXPORT gv_bwtar FROM gv_bwtar TO MEMORY ID 'GV_BWTAR'.

  ##ENH_OK
  MOVE-CORRESPONDING ls_csdoc_crt-x-cshdr TO ls_order_comm.
  ls_order_comm-iwerk = gv_iwerk.
  ls_order_comm-auart = gc_constants-aufart.

*-- Material Conversion
  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input        = gs_glcmhdr-ymatnr
    IMPORTING
      output       = ls_order_comm-matnr
    EXCEPTIONS
      length_error = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
*-- Build messages
    lv_msgv1      = gs_glcmhdr-ymatnr.
    PERFORM build_msgs USING gc_constants-msgty_error '048'
                             lv_msgv1 space
                    CHANGING ct_messages[].
  ENDIF.
  ls_order_comm-gstrp = ls_csdoc_crt-x-cshdr-datab.
  ls_order_comm-gltrp = ls_csdoc_crt-x-cshdr-datbi.
  ls_order_comm-gamng = ps_indata-aarea * ps_indata-z_pro_proj.
  ls_order_comm-gmein = gc_constants-uom_kg.

*-- Create Yard Order
  CALL METHOD /agri/cl_glco_process=>order_create
    EXPORTING
      i_commit_work = space
      is_order_comm = ls_order_comm
    IMPORTING
      e_aufnr       = lv_order_number
      et_messages   = lt_messtab.

  IF lv_order_number IS INITIAL.

    LOOP AT lt_messtab INTO ls_messtab.
      CLEAR : ls_mesg.
      ls_mesg-msgty   = ls_messtab-msgtyp.
      ls_mesg-msgno   = ls_messtab-msgnr.
      ls_mesg-msgid   = ls_messtab-msgid.
      ls_mesg-msgv1   = ls_messtab-msgv1.
      ls_mesg-msgv2   = ls_messtab-msgv2.
      ls_mesg-msgv3   = ls_messtab-msgv3.
      ls_mesg-msgv4   = ls_messtab-msgv4.
      APPEND ls_mesg TO ct_messages.
    ENDLOOP.
  ELSE.

*-- Commit work
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

*-- Build messages
    DATA(lv_success_flg) = abap_true.
    lv_msgv1         = lv_order_number.
    cv_yard_order    = lv_order_number.
    cs_outdata-aufnr = lv_order_number.
    cs_outdata-bwtar = gv_bwtar.
    PERFORM build_msgs USING gc_constants-msgty_success '056'
                             lv_msgv1 space
                    CHANGING ct_messages[].
  ENDIF.

  IF lv_success_flg EQ abap_true.
*-- Update work center in operations of yard order
    CALL FUNCTION 'ZVX_WORK_CENTER_UPD'
      EXPORTING
        iv_aufnr = lv_order_number
        iv_varia = ps_indata-varia.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form good_recipient
*&---------------------------------------------------------------------*
FORM good_recipient USING ps_indata TYPE zvxs_crop_input
                 CHANGING pv_bwtar  TYPE bwtar_d.

  DATA : lv_string TYPE string,
         lv_num(2) TYPE c.

*-- Composition for the component  should look like SAF20/21-1
  lv_num = ps_indata-season+2(2).
  lv_num = lv_num + 1.

  CONCATENATE gc_constants-saf
              ps_indata-season+2(2)
              gc_constants-slash
              lv_num
              gc_constants-iphen
              ps_indata-varia+0(1) INTO lv_string.
  CONDENSE lv_string.
  pv_bwtar = lv_string.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form yard_order_update
*&---------------------------------------------------------------------*
FORM yard_order_update USING ps_indata     TYPE zvxs_crop_input
                             pt_csdoc_crt  TYPE /agri/t_glcs_doc
                             pv_yard_order TYPE aufnr
                    CHANGING ct_messages   TYPE zvxtt_message_gprolog.

*-- Local internal tables
  DATA: lt_cskey        TYPE /agri/t_glcs_key,
        lt_flcma        TYPE /agri/t_glflcma,
        lt_csprs        TYPE /agri/t_glcsprs,
        lt_csprso       TYPE /agri/t_glcsprso,
        lt_csprst       TYPE /agri/t_glcsprst,
        lt_csprsor      TYPE /agri/t_glcsprsor,
        lt_csdfl        TYPE /agri/t_glcsdfl,
        lt_messages     TYPE /agri/t_gprolog,
        lt_glcs_change  TYPE /agri/t_glcs_doc,
        lt_flcma_change TYPE /agri/t_glflcma,

*-- Local work area
        ls_cskey        TYPE /agri/s_glcs_key,
        ls_flcma        TYPE /agri/s_glflcma,
        ls_mesg         TYPE zvxs_message_gprolog,
        ls_glcs_change  TYPE /agri/s_glcs_doc,

*-- Local Variables
        lv_msgv1        TYPE msgv1,
        lv_msgv2        TYPE msgv2.

  READ TABLE pt_csdoc_crt INTO DATA(ls_csdoc_crt) INDEX 1.
  ls_cskey-tplnr_fl = ls_csdoc_crt-tplnr_fl.
  ls_cskey-contr    = ls_csdoc_crt-contr.
  APPEND ls_cskey TO lt_cskey.

*-- Read the crop season
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
  IF sy-subrc <> 0.
*-- Build messages
    lv_msgv1      = ps_indata-tplnr.
    lv_msgv2      = ls_csdoc_crt-contr.
    PERFORM build_msgs USING gc_constants-msgty_error '057'
                             lv_msgv1 lv_msgv2
                    CHANGING ct_messages[].
  ENDIF.

  SORT lt_flcma BY tplnr_fl contr.
  READ TABLE lt_flcma INTO ls_flcma
  WITH KEY   tplnr_fl = ls_csdoc_crt-tplnr_fl
             contr    = ls_csdoc_crt-contr         ##WARN_OK
  BINARY SEARCH.
  IF sy-subrc EQ 0.
    ls_flcma-yaufnr = pv_yard_order.
    ls_flcma-aufnr  = pv_yard_order.
*-- If the absolute area in RFC input is greater than the gross area,
* update the area with the gross area only.
    IF ps_indata-aarea GT ls_flcma-garea.
    ELSE.
      ls_flcma-aarea  = ps_indata-aarea.
    ENDIF.
    ls_flcma-updkz  = gc_constants-update.

    IF ps_indata-crop_ex = gc_constants-flag.
      ls_flcma-zzcrop_ex = gc_constants-flag.
      READ TABLE gt_cskey INTO DATA(ls_cskey1) INDEX 1.
      IF sy-subrc EQ 0.
        ls_flcma-zzaufnr_o = ls_cskey1-yaufnr.
      ENDIF.
    ENDIF.

    ls_glcs_change-tplnr_fl  = ls_csdoc_crt-tplnr_fl.
    ls_glcs_change-contr     = ls_csdoc_crt-contr.
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
    IF sy-subrc <> 0.
      LOOP AT lt_messages INTO DATA(ls_message).
        CLEAR : ls_mesg.
        ls_mesg = CORRESPONDING #( ls_message ).
        APPEND ls_mesg TO ct_messages.
      ENDLOOP.
    ELSE.
*-- Build messages
      lv_msgv1      = pv_yard_order.
      PERFORM build_msgs USING gc_constants-msgty_success '058'
                               lv_msgv1 space
                      CHANGING ct_messages[].

      IF ps_indata-crop_ex = gc_constants-flag.
*-- Build messages
        lv_msgv1      = ls_cskey1-yaufnr.
        PERFORM build_msgs USING gc_constants-msgty_success '059'
                                 lv_msgv1 space
                        CHANGING ct_messages[].
      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form change_crop_season
*&---------------------------------------------------------------------*
FORM change_crop_season USING ps_indata   TYPE zvxs_crop_input
                     CHANGING ct_messages TYPE zvxtt_message_gprolog.

*-- Local internal tables
  DATA: lt_cskey        TYPE /agri/t_glcs_key,
        lt_flcma        TYPE /agri/t_glflcma,
        lt_csprs        TYPE /agri/t_glcsprs,
        lt_csprso       TYPE /agri/t_glcsprso,
        lt_csprst       TYPE /agri/t_glcsprst,
        lt_csprsor      TYPE /agri/t_glcsprsor,
        lt_csdfl        TYPE /agri/t_glcsdfl,
        lt_messages     TYPE /agri/t_gprolog,
        lt_flcma_change TYPE /agri/t_glflcma,
        lt_glcs_change  TYPE /agri/t_glcs_doc,

*-- Local work area
        ls_cskey        TYPE /agri/s_glcs_key,
        ls_mesg         TYPE zvxs_message_gprolog,
        ls_glcs_change  TYPE /agri/s_glcs_doc,
        ls_crop_output  TYPE zvxs_crop_output,

*-- Local Variables
        lv_msgv1        TYPE msgv1,
        lv_msgv2        TYPE msgv2,
        lv_yard_order   TYPE aufnr.

  READ TABLE gt_cskey_upd INTO DATA(ls_cskey1) INDEX 1.
  IF sy-subrc EQ 0.
    ls_cskey-tplnr_fl = ls_cskey1-tplnr_fl.
    ls_cskey-contr    = ls_cskey1-contr.
    APPEND ls_cskey TO lt_cskey.

*-- Revoke yard order if we are change closed crop season to active crop season
    IF ls_cskey1-astat EQ gc_constants-astat_closed.
      CALL METHOD /agri/cl_glco_process=>order_revoke_teco
        EXPORTING
          i_aufnr = ls_cskey1-yaufnr
        IMPORTING
          e_subrc = DATA(lv_subrc).
      IF lv_subrc EQ 0.
*-- Build messages
        lv_msgv1 = ls_cskey1-yaufnr.
        PERFORM build_msgs USING gc_constants-msgty_success '065'
                                 lv_msgv1 space
                        CHANGING ct_messages[].
      ELSE.
*-- Build messages
        lv_msgv1 = ls_cskey1-yaufnr.
        PERFORM build_msgs USING gc_constants-msgty_error '066'
                                 lv_msgv1 space
                        CHANGING ct_messages[].
      ENDIF.
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
    WITH KEY   tplnr_fl = ls_cskey1-tplnr_fl
               contr    = ls_cskey1-contr
    BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_flcma-updkz     = gc_constants-update.
      ls_flcma-exhad     = ps_indata-datab.
      ls_flcma-aarea     = ps_indata-aarea.
      ls_flcma-cmnum     = ps_indata-cmnum.
      ls_flcma-zzpro_yld = ps_indata-z_pro_proj.

*-- Change the crop season state to active..
      IF ls_flcma-astat EQ gc_constants-astat_closed.
        ls_flcma-astat = gc_constants-astat_a.
      ENDIF.

      ls_glcs_change-tplnr_fl  = ls_cskey1-tplnr_fl.
      ls_glcs_change-contr     = ls_cskey1-contr.
      ls_glcs_change-updkz     = gc_constants-update.
      ls_glcs_change-x-cshdr   = ls_flcma.
      ls_glcs_change-x-csprs   = lt_csprs.
      ls_glcs_change-x-csprso  = lt_csprso.
      ls_glcs_change-x-csprst  = lt_csprst.
      ls_glcs_change-x-csprsor = lt_csprsor.
      ls_glcs_change-x-csdfl   = lt_csdfl.

      APPEND ls_flcma TO lt_flcma_change.
      APPEND ls_glcs_change TO lt_glcs_change.
      IF ls_cskey1-yaufnr IS INITIAL.
*-- If Crop season creation fails create yard order.
        PERFORM create_yard_order USING ps_indata
                                        lt_glcs_change
                               CHANGING ct_messages[]
                                        lv_yard_order
                                        ls_crop_output.

        ls_flcma-yaufnr = lv_yard_order.
      ENDIF.

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
        lv_msgv2 = ls_cskey1-contr.
        PERFORM build_msgs USING gc_constants-msgty_success '061'
                                 lv_msgv1 lv_msgv2
                        CHANGING ct_messages[].
      ENDIF.
    ENDIF.
  ELSE.

*-- Build error message as crop season is in inactive or closed state
*   So no operation has to be performed
    lv_msgv1 = ps_indata-tplnr.
    lv_msgv2 = ps_indata-varia.
    PERFORM build_msgs USING gc_constants-msgty_error '105'
                             lv_msgv1 lv_msgv2
                    CHANGING ct_messages[].
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form delete_crop_season
*&---------------------------------------------------------------------*
FORM close_crop_season USING pt_cskey    TYPE tt_glflcma
                             ps_indata   TYPE zvxs_crop_input
                    CHANGING ct_messages TYPE zvxtt_message_gprolog.

*-- Local internal tables
  DATA: lt_cskey        TYPE /agri/t_glcs_key,
        lt_flcma        TYPE /agri/t_glflcma,
        lt_csprs        TYPE /agri/t_glcsprs,
        lt_csprso       TYPE /agri/t_glcsprso,
        lt_csprst       TYPE /agri/t_glcsprst,
        lt_csprsor      TYPE /agri/t_glcsprsor,
        lt_csdfl        TYPE /agri/t_glcsdfl,
        lt_messages     TYPE /agri/t_gprolog,
        lt_glcs_change  TYPE /agri/t_glcs_doc,
        lt_flcma_change TYPE /agri/t_glflcma,

*-- Local work area
        ls_cskey        TYPE /agri/s_glcs_key,
        ls_flcma        TYPE /agri/s_glflcma,
        ls_mesg         TYPE zvxs_message_gprolog,
        ls_glcs_change  TYPE /agri/s_glcs_doc,

*-- Local Variables
        lv_msgv1        TYPE msgv1,
        lv_msgv2        TYPE msgv2.

  READ TABLE pt_cskey INTO DATA(ls_cskey1) INDEX 1.
  IF sy-subrc EQ 0.

*-- First close the existing yard orders for this crop season
    PERFORM teco_yard_order USING ls_cskey1
                         CHANGING ct_messages[].

    ls_cskey-tplnr_fl = ls_cskey1-tplnr_fl.
    ls_cskey-contr    = ls_cskey1-contr.
    APPEND ls_cskey TO lt_cskey.

*-- Read the crop season
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
    READ TABLE lt_flcma INTO ls_flcma
    WITH KEY   tplnr_fl = ls_cskey-tplnr_fl
               contr    = ls_cskey-contr
    BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_flcma-astat           = gc_constants-astat_closed.
      ls_flcma-updkz           = gc_constants-update.
      IF gv_po_error EQ abap_true.
        ls_flcma-loevm = abap_true.
      ENDIF.
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

*-- Delete the crop season
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
      IF sy-subrc <> 0.
        LOOP AT lt_messages INTO DATA(ls_message).
          CLEAR : ls_mesg.
          ls_mesg = CORRESPONDING #( ls_message ).
          APPEND ls_mesg TO ct_messages.
        ENDLOOP.
      ELSE.
*-- Build messages
        lv_msgv1      = ps_indata-tplnr.
        lv_msgv2      = ls_cskey-contr.
        PERFORM build_msgs USING gc_constants-msgty_success '060'
                                 lv_msgv1 lv_msgv2
                        CHANGING ct_messages[].
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form close_yard_order
*&---------------------------------------------------------------------*
FORM teco_yard_order USING ps_cskey1   TYPE gty_glflcma
                  CHANGING ct_messages TYPE zvxtt_message_gprolog.

*-- Local internal tables
  DATA: lt_order         TYPE TABLE OF bapi_order_key,
        lt_detail_return TYPE TABLE OF bapi_order_return,

*-- Local work area
        ls_mesg          TYPE zvxs_message_gprolog,
        ls_order         TYPE bapi_order_key,
        ls_detail_return TYPE bapi_order_return.

  ls_order-order_number = ps_cskey1-yaufnr.
  APPEND ls_order TO lt_order.

  CALL FUNCTION 'BAPI_PRODORD_COMPLETE_TECH'
    TABLES
      orders        = lt_order[]
      detail_return = lt_detail_return[].

  LOOP AT lt_detail_return INTO ls_detail_return.
    CLEAR : ls_mesg.
    ls_mesg-msgty   = ls_detail_return-type.
    ls_mesg-msgno   = ls_detail_return-number.
    ls_mesg-msgid   = ls_detail_return-id.
    ls_mesg-message = ls_detail_return-message.
    ls_mesg-msgv1   = ls_detail_return-message_v1.
    ls_mesg-msgv2   = ls_detail_return-message_v2.
    ls_mesg-msgv3   = ls_detail_return-message_v3.
    ls_mesg-msgv4   = ls_detail_return-message_v4.
    APPEND ls_mesg TO ct_messages.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form build_msgs
*&---------------------------------------------------------------------*
FORM build_msgs USING pv_msgty    TYPE msgty
                      pv_msgno    TYPE msgno
                      pv_msgv1    TYPE msgv1
                      pv_msgv2    TYPE msgv2
             CHANGING ct_messages TYPE zvxtt_message_gprolog.

*-- Local work areas
  DATA: ls_messages TYPE zvxs_message_gprolog.

  ls_messages-msgid = gc_constants-msgid.
  ls_messages-msgty = pv_msgty.
  ls_messages-msgno = pv_msgno.
  ls_messages-msgv1 = pv_msgv1.
  ls_messages-msgv2 = pv_msgv2.
  APPEND ls_messages TO ct_messages.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form save_log
*&---------------------------------------------------------------------*
FORM save_log USING pt_messages TYPE zvxtt_message_gprolog.

*-- Local internal tables
  DATA : lt_message_log TYPE /agri/t_gprolog,

*-- Local work areas
         ls_message_log TYPE /agri/s_gprolog.

*-- Maintain message log for internal tracking purpose
  IF pt_messages IS NOT INITIAL.

    LOOP AT pt_messages ASSIGNING FIELD-SYMBOL(<fs_message>).
      ls_message_log = CORRESPONDING #( <fs_message> ).
      APPEND ls_message_log TO lt_message_log.
      CLEAR : ls_message_log.
      MESSAGE ID <fs_message>-msgid  TYPE <fs_message>-msgty
                                   NUMBER <fs_message>-msgno
                                     WITH <fs_message>-msgv1
                                          <fs_message>-msgv2
                                          <fs_message>-msgv3
                                          <fs_message>-msgv4
                                     INTO <fs_message>-message.
    ENDLOOP.

    CALL FUNCTION '/AGRI/G_PROLOG_MESSAGES_SAVE'
      EXPORTING
        i_aplobj   = gc_constants-aplobj
        i_subobj   = gc_constants-subobj
      TABLES
        t_messages = lt_message_log[].

  ENDIF.

ENDFORM.
