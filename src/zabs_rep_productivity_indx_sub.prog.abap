************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name    : ZABS_REP_PRODUCTIVITY_INDX_SUB                      *
* Tcode          : ZABS_RBREAK                                         *
* Created By     : Jetendra Mantena                                    *
* Requested by   : Mario Alfredo                                       *
* Created on     : 10.14.2019                                          *
* TR             : C4DK901784                                          *
* Version        : 001                                                 *
* Description    : To specify the requirement for the creation of      *
*                  “Report for Comparison of Productivity Index vs     *
*                  Reasons for Loss/Stops (Breaks)”                    *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*&---------------------------------------------------------------------*
*& Form REFRESH_GLOBAL_DATA
*&---------------------------------------------------------------------*
*& Global data refresh
*&---------------------------------------------------------------------*
FORM initialize_global_data.

  REFRESH gt_prod_indx.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_PRODUCTIVITY_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM get_productivity_data.

*-- Local declarations
  DATA: lwa_idactvl   TYPE ty_idactvl,
        lwa_prod_indx TYPE zabs_str_prod_indx,
        lwa_matnr     TYPE /isdfps/matnr,
        lt_matnr      TYPE /isdfps/matnr_tab,
        lt_idactvl    TYPE tty_idactvl,
        lt_plant_wc   TYPE STANDARD TABLE OF ty_plant_wc,
        lwa_plant_wc  TYPE ty_plant_wc,
        lt_tplnr      TYPE STANDARD TABLE OF ty_tplnr,
        lwa_tplnr     TYPE ty_tplnr,
        lt_breaks     TYPE STANDARD TABLE OF ty_breaks,
        lwa_breaks    TYPE ty_breaks.

*-- Fetch the active Crop Seasons in the given plant
  SELECT tplnr_fl,
         contr,
         cmnum,
         varia,
         season,
         datab,
         datbi,
         iwerk
    FROM /agri/glflcma
    INTO TABLE @DATA(lt_glflcma)
    WHERE tplnr_fl IN @so_tplnr
      AND ( datab  GE @so_date-low
      AND   datbi  LE @so_date-high )
*--date logic need to be changed.
      AND iwerk    EQ @p_werks
      AND astat    EQ @zcl_abs_abap_maintain=>c_cs_active
      AND loevm    EQ @space.
  IF sy-subrc NE 0.
    MESSAGE TEXT-006 TYPE zcl_abs_abap_maintain=>c_msgty_info.
    LEAVE LIST-PROCESSING.
  ENDIF.

  SORT lt_glflcma BY tplnr_fl contr.

*-- Fetch the task orders which are created for the above crop seasons
  SELECT aufnr,
         tplnr_fl,
         contr
    INTO TABLE @DATA(lt_fmfphdr)
    FROM /agri/fmfphdr
    FOR ALL ENTRIES IN @lt_glflcma
    WHERE tplnr_fl  EQ @lt_glflcma-tplnr_fl
      AND contr     EQ @lt_glflcma-contr
      AND tecom     EQ @space.
  IF sy-subrc NE 0.
    MESSAGE TEXT-007 TYPE zcl_abs_abap_maintain=>c_msgty_info.
    LEAVE LIST-PROCESSING.
  ENDIF.

  SORT lt_fmfphdr BY aufnr tplnr_fl contr.

*-- Fetch the Accomplisment sheets with above orders which are confirmed
*-- for non-billable
  SELECT a~accom, a~werks, b~posnr, b~tplnr, b~aufnr,
         b~tmatnr, b~arbpl, b~strtdat, b~strttim,
*         b~findat, b~fintim, b~duran, b~idactvl, b~zzactrn,
         b~findat, b~fintim, b~duran, b~idactvl, b~idactve,
         b~zzactrn, b~zzacdtr, b~zzbill, b~zzacppg, b~zzstdur
    FROM /agri/fmachdr AS a
    JOIN /agri/fmacitm AS b
    ON b~accom EQ a~accom
    INTO TABLE @DATA(lt_fmacitm)
    FOR ALL ENTRIES IN @lt_fmfphdr
   WHERE a~accom  IN @so_accom
     AND b~tplnr  EQ @lt_fmfphdr-tplnr_fl
     AND b~aufnr  EQ @lt_fmfphdr-aufnr
     AND b~tmatnr IN @so_matnr
     AND b~arbpl  IN @so_arbpl
     AND b~status EQ @zcl_abs_abap_maintain=>c_ac_stat_confirmed.
  IF sy-subrc EQ 0.
*    DELETE lt_fmacitm
*                  WHERE zzbill NE zcl_abs_abap_maintain=>c_non_billable.
    DELETE lt_fmacitm WHERE idactvl IS NOT INITIAL
                        AND zzbill NE 'NO'.
    DATA(lt_equip) = lt_fmacitm[].
    DELETE lt_equip WHERE idactve IS INITIAL.
    IF NOT lt_equip[] IS INITIAL.
      SELECT f~idactv, f~rstype, f~bill, f~actype,
             f~zzactcg, t~description
        FROM /agri/fmacact AS f
        INNER JOIN /agri/fmacactt AS t
        ON f~idactv = t~idactv
        INTO TABLE @DATA(lt_ativ)
        FOR ALL ENTRIES IN @lt_equip
       WHERE f~idactv = @lt_equip-idactve
         AND t~spras  = @sy-langu.

      IF lt_ativ[] IS NOT INITIAL.
        SORT lt_ativ BY idactv.
        LOOP AT lt_equip INTO DATA(ls_equip).
          READ TABLE lt_ativ INTO DATA(ls_ativ)
            WITH KEY idactv = ls_equip-idactve BINARY SEARCH.
          IF sy-subrc EQ 0
          AND ls_ativ-bill NE 'NO'.
            DELETE lt_fmacitm WHERE accom = ls_equip-accom
                                AND posnr = ls_equip-posnr.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.

  IF lt_fmacitm IS INITIAL.
    MESSAGE TEXT-008 TYPE zcl_abs_abap_maintain=>c_msgty_info.
    LEAVE LIST-PROCESSING.
  ENDIF.

  LOOP AT lt_fmacitm ASSIGNING FIELD-SYMBOL(<lwa_fmacitm>).

*-- Collect Activities
    CLEAR lwa_idactvl.
    lwa_idactvl-idactvl = <lwa_fmacitm>-idactvl.
    COLLECT lwa_idactvl INTO lt_idactvl.

*-- Collect Tasks
    CLEAR lwa_matnr.
    lwa_matnr-matnr = <lwa_fmacitm>-tmatnr.
    COLLECT lwa_matnr INTO lt_matnr.

*-- Collect Plants with Work Centers
    CLEAR lwa_plant_wc.
    lwa_plant_wc-werks = <lwa_fmacitm>-werks.
    lwa_plant_wc-arbpl = <lwa_fmacitm>-arbpl.
    COLLECT lwa_plant_wc INTO lt_plant_wc.

*-- Collect Terrains
    CLEAR lwa_tplnr.
    lwa_tplnr-tplnr = <lwa_fmacitm>-tplnr.
    COLLECT lwa_tplnr INTO lt_tplnr.

*-- Collect Breaks
    CLEAR lwa_breaks.
    lwa_breaks-acppg  = <lwa_fmacitm>-zzacppg.
    lwa_breaks-idactv = <lwa_fmacitm>-idactvl.
    lwa_breaks-period = <lwa_fmacitm>-strtdat(6).
    COLLECT lwa_breaks INTO lt_breaks.
  ENDLOOP.

  IF lt_idactvl IS NOT INITIAL.
*-- Fetch the activity type description
    SELECT *
      FROM /agri/fmacactt
      INTO TABLE @DATA(lt_fmacactt)
      FOR ALL ENTRIES IN @lt_idactvl
      WHERE idactv EQ @lt_idactvl-idactvl
        AND spras  EQ @sy-langu.
    IF sy-subrc = 0.
      SORT lt_fmacactt BY idactv.
    ENDIF.
  ENDIF.

  IF lt_matnr IS NOT INITIAL.
*-- Fetch the task description
    SELECT matnr,
           maktx
      FROM makt
      INTO TABLE @DATA(lt_makt)
      FOR ALL ENTRIES IN @lt_matnr
      WHERE matnr = @lt_matnr-matnr
        AND spras = @sy-langu.
    IF sy-subrc = 0.
      SORT lt_makt BY matnr.
    ENDIF.
  ENDIF.

  IF lt_tplnr IS NOT INITIAL.
*-- Fetch the terrain description
    SELECT tplnr_fl pltxt
      FROM /agri/glflotx
      INTO TABLE lt_tplnr
       FOR ALL ENTRIES IN lt_tplnr
     WHERE tplnr_fl EQ lt_tplnr-tplnr
       AND spras    EQ sy-langu.
  ENDIF.

  IF lt_plant_wc IS NOT INITIAL.
*--Fetching Work Center Header data to get description
    SELECT objid,
           arbpl,
           werks
      FROM crhd
      INTO TABLE @DATA(lt_crhd)
      FOR ALL ENTRIES IN @lt_plant_wc
      WHERE arbpl EQ @lt_plant_wc-arbpl
        AND werks EQ @lt_plant_wc-werks
        AND lvorm EQ @space.
    IF sy-subrc EQ 0.
      SORT lt_crhd BY werks arbpl.

*--Fetching Work Center Description
      SELECT objid,
             ktext
        FROM crtx
        INTO TABLE @DATA(lt_crtx)
        FOR ALL ENTRIES IN @lt_crhd
        WHERE spras EQ @sy-langu
          AND objid EQ @lt_crhd-objid.
      IF sy-subrc EQ 0.
        SORT lt_crtx BY objid.
      ENDIF.
    ENDIF.
  ENDIF.

  IF lt_breaks IS NOT INITIAL.
*-- Fetch the break description and break percentage
    SELECT acppg idactv period acdes acprt
      FROM zfmacsched_break
      INTO TABLE lt_breaks
       FOR ALL ENTRIES IN lt_breaks
     WHERE acppg  EQ lt_breaks-acppg
       AND idactv EQ lt_breaks-idactv
       AND period EQ lt_breaks-period.
    IF sy-subrc EQ 0.
      SORT lt_breaks BY acppg idactv period.
    ENDIF.
  ENDIF.

*-- Prepare the shifts and breaks for non-billable orders
  LOOP AT lt_fmacitm ASSIGNING <lwa_fmacitm>.
    CLEAR lwa_prod_indx.
    lwa_prod_indx-accom = <lwa_fmacitm>-accom.
    lwa_prod_indx-tplnr = <lwa_fmacitm>-tplnr.

    CLEAR lwa_tplnr.
    READ TABLE lt_tplnr INTO lwa_tplnr
      WITH KEY tplnr = lwa_prod_indx-tplnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      lwa_prod_indx-pltxt = lwa_tplnr-pltxt.
    ENDIF.

    lwa_prod_indx-aufnr    = <lwa_fmacitm>-aufnr.
    lwa_prod_indx-tmatnr   = <lwa_fmacitm>-tmatnr.

    READ TABLE lt_makt INTO DATA(lwa_makt)
      WITH KEY matnr = lwa_prod_indx-tmatnr BINARY SEARCH.
    IF sy-subrc = 0.
      lwa_prod_indx-maktx = lwa_makt-maktx.
    ENDIF.

    lwa_prod_indx-arbpl = <lwa_fmacitm>-arbpl.

    READ TABLE lt_crhd INTO DATA(lwa_crhdr)
      WITH KEY werks = <lwa_fmacitm>-werks
               arbpl = lwa_prod_indx-arbpl BINARY SEARCH.
    IF sy-subrc EQ 0.
      READ TABLE lt_crtx INTO DATA(lwa_crtx)
        WITH KEY objid = lwa_crhdr-objid BINARY SEARCH.
      IF sy-subrc EQ 0.
        lwa_prod_indx-ktext = lwa_crtx-ktext.
      ENDIF.
    ENDIF.

    lwa_prod_indx-idactvl  = <lwa_fmacitm>-idactvl.

    READ TABLE lt_fmacactt INTO DATA(lwa_fmacactt)
      WITH KEY idactv = lwa_prod_indx-idactvl BINARY SEARCH.
    IF sy-subrc = 0.
      lwa_prod_indx-actdesc = lwa_fmacactt-description.
    ENDIF.

    lwa_prod_indx-idactve  = <lwa_fmacitm>-idactve.

    READ TABLE lt_ativ INTO ls_ativ
      WITH KEY idactv = lwa_prod_indx-idactve BINARY SEARCH.
    IF sy-subrc = 0.
      lwa_prod_indx-actdesc_eq = ls_ativ-description.
    ENDIF.

    lwa_prod_indx-strtdat  = <lwa_fmacitm>-strtdat.
    lwa_prod_indx-strttim  = <lwa_fmacitm>-strttim.
    lwa_prod_indx-findat   = <lwa_fmacitm>-findat.
    lwa_prod_indx-fintim   = <lwa_fmacitm>-fintim.
    lwa_prod_indx-duran    = <lwa_fmacitm>-duran.
    lwa_prod_indx-actrn    = <lwa_fmacitm>-zzactrn.
    lwa_prod_indx-acdtr    = <lwa_fmacitm>-zzacdtr.
    lwa_prod_indx-actut    = <lwa_fmacitm>-zzstdur.

    IF lwa_prod_indx-actut NE 0.
      lwa_prod_indx-cacprt = lwa_prod_indx-duran /
                             lwa_prod_indx-actut * 100.
    ENDIF.

    lwa_prod_indx-acppg = <lwa_fmacitm>-zzacppg.
    CLEAR lwa_breaks.
    READ TABLE lt_breaks INTO lwa_breaks
      WITH KEY acppg  = lwa_prod_indx-acppg
               idactv = lwa_prod_indx-idactvl
               period = lwa_prod_indx-strtdat(6) BINARY SEARCH.

    IF sy-subrc EQ 0.
      lwa_prod_indx-acdes = lwa_breaks-acdes.
      lwa_prod_indx-acprt = lwa_breaks-acprt.

      IF lwa_breaks-acprt NE 0.
        lwa_prod_indx-varprt =
          ( lwa_prod_indx-cacprt - lwa_prod_indx-acprt ) /
           lwa_prod_indx-acprt * 100.
      ENDIF.
    ENDIF.

    APPEND lwa_prod_indx TO gt_prod_indx.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Module STATUS_SET OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_set OUTPUT.
  PERFORM status_set.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form STATUS_SET
*&---------------------------------------------------------------------*
*& STATUS_SET
*&---------------------------------------------------------------------*
FORM status_set.
  SET PF-STATUS 'S100'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Module TITLE_SET OUTPUT
*&---------------------------------------------------------------------*
*& TITLE_SET
*&---------------------------------------------------------------------*
MODULE title_set OUTPUT.
  PERFORM title_set.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form TITLE_SET
*&---------------------------------------------------------------------*
*& TITLE_SET
*&---------------------------------------------------------------------*
FORM title_set.
  SET TITLEBAR 'T100'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_PRODUCTIVITY_DATA
*&---------------------------------------------------------------------*
*& Display productivity data
*&---------------------------------------------------------------------*
FORM display_productivity_data.
  CALL SCREEN 100.
ENDFORM.

*&---------------------------------------------------------------------*
*& Module CONTROLS_DISPLAY OUTPUT
*&---------------------------------------------------------------------*
*& CONTROLS_DISPLAY
*&---------------------------------------------------------------------*
MODULE controls_display OUTPUT.
  PERFORM controls_display.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form CONTROLS_DISPLAY
*&---------------------------------------------------------------------*
*& CONTROLS_DISPLAY
*&---------------------------------------------------------------------*
FORM controls_display.

*-- Local Declarations
  DATA: lt_fcat   TYPE lvc_t_fcat,
        ls_layout TYPE lvc_s_layo.

  IF cl_gui_alv_grid=>offline( ) IS INITIAL.

*--Create Object For Custom Container
    CREATE OBJECT gobj_cont
      EXPORTING
        container_name = 'ZPROD_INDX_100CC'.

*--Create Object for ALV Grid
    CREATE OBJECT gobj_grid
      EXPORTING
        i_parent = gobj_cont.

  ELSE.

    CREATE OBJECT gobj_grid
      EXPORTING
        i_parent = gobj_cont.

  ENDIF.

*--Field Catalog
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZABS_STR_PROD_INDX'
    CHANGING
      ct_fieldcat            = lt_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*--Set ALV attributes FOR LAYOUT
  ls_layout-cwidth_opt = abap_true.
  ls_layout-zebra      = abap_true.

*--Displaying ALV Data
  IF gobj_grid IS NOT INITIAL.
    CALL METHOD gobj_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = ls_layout
      CHANGING
        it_outtab                     = gt_prod_indx
        it_fieldcatalog               = lt_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*& USER_COMMAND_0100
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  IF sy-ucomm = 'BACK'.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*& Form SELECTION_VALIDATIONS
*&---------------------------------------------------------------------*
*& SELECTION_VALIDATIONS
*&---------------------------------------------------------------------*
FORM selection_validations.

*-- Local declarations
  DATA: lv_iwerk TYPE iwerk.

*--Plant Validation
  IF p_werks IS INITIAL.
    MESSAGE TEXT-002 TYPE zcl_abs_abap_maintain=>c_msgty_error.
  ELSE.
    SELECT SINGLE werks
             FROM t001w
             INTO lv_iwerk
            WHERE werks EQ p_werks.
    IF sy-subrc NE 0.
      MESSAGE TEXT-003 TYPE zcl_abs_abap_maintain=>c_msgty_error.
    ENDIF.
  ENDIF.

*--Date lower limit initial check
  IF so_date-low IS INITIAL.
    MESSAGE TEXT-004 TYPE zcl_abs_abap_maintain=>c_msgty_error.
  ELSEIF so_date-high IS INITIAL.
    MESSAGE TEXT-005 TYPE zcl_abs_abap_maintain=>c_msgty_error.
  ENDIF.

ENDFORM.
