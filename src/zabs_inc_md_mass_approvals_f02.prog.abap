************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name    : ZABS_INC_MD_MASS_APPROVALS_F01                      *
* Tcode          : ZABS_MDMA                                           *
* Created By     : Chandrakanth Karanam                                *
* Requested by   : Rapheal                                             *
* Created on     : 11.06.2019                                          *
* TR             : C4DK903886                                          *
* Version        : 001                                                 *
* Description    : Provide the list of measurement documents for mass  *
*                  approvals based the user who executes this program  *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*&---------------------------------------------------------------------*
*& Form INITIALIZE_GLOBAL_DATA
*&---------------------------------------------------------------------*
*& INITIALIZE_GLOBAL_DATA
*&---------------------------------------------------------------------*
FORM initialize_global_data.

  REFRESH : gt_fcat,
            gt_mdhdr,
            gt_gsfstp,
            gt_gsfocm,
            gt_tj30.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_MEASUREMENT_DATA
*&---------------------------------------------------------------------*
*& GET_MEASUREMENT_DATA
*&---------------------------------------------------------------------*
FORM get_measurement_data.

  TYPES: BEGIN OF ly_approvals,
           imovel  TYPE zabs_del_imovel,
           talhao  TYPE zabs_del_talhao,
           florada TYPE char1,
         END OF ly_approvals.

*-- Local Declarations
  DATA: lt_constants TYPE zabs_tty_vkey_const,
        ltr_estat    TYPE RANGE OF j_estat,
        lt_dyn_table TYPE REF TO data,
        lt_copy      TYPE REF TO data,
        lt_aux       TYPE REF TO data,
        lt_cabn      TYPE tty_cabn,
        lt_fname     TYPE STANDARD TABLE OF ty_fname,
*--Workarea declaration
        lsr_estat    LIKE LINE OF ltr_estat,
        ls_approvals TYPE ly_approvals,
        ls_atgdoc    TYPE /agri/s_glatgdoc,
*--Local variable declaration
        lv_tabix     TYPE sy-tabix,
        lv_packed    TYPE p DECIMALS 4,
        lv_where     TYPE string,
        lv_mdtyp     TYPE /agri/gobjtyp_bor VALUE '/AGRI/GLMD',
        lv_reason    TYPE atinn.

  CONSTANTS: BEGIN OF lc_sort,
               imovel  TYPE fieldname VALUE 'IMOVEL',
               talhao  TYPE fieldname VALUE 'TALHAO',
               florada TYPE fieldname VALUE 'FLORADA',
               docto   TYPE fieldname VALUE 'MDOCM',
               motivo  TYPE fieldname VALUE 'MOT_EST',
               status  TYPE fieldname VALUE 'KFRST',
             END OF lc_sort.

*-- Fetch the status profile maintained in the variant table for the
*-- measurement document type and measurement group
  CALL METHOD zcl_abs_get_variants=>get_constant_multiple
    EXPORTING
      iv_objid     = zcl_abs_abap_maintain=>c_objid_md_status_profile
      iv_k1val     = zcl_abs_abap_maintain=>c_key_measurement_type
      iv_k2val     = p_mdtyp
    IMPORTING
      et_constants = lt_constants.

  REFRESH gr_stsma.
  SORT lt_constants BY cnval1.
  READ TABLE lt_constants TRANSPORTING NO FIELDS
    WITH KEY cnval1 = p_mpgrp.
  IF sy-subrc EQ 0.
    LOOP AT lt_constants INTO DATA(ls_constants) FROM sy-tabix.
      IF ls_constants-cnval1 NE p_mpgrp.
        EXIT.
      ENDIF.
      INSERT INITIAL LINE INTO TABLE gr_stsma
        ASSIGNING FIELD-SYMBOL(<ls_stsma>).
      IF sy-subrc EQ 0.
        <ls_stsma> = 'IEQ'.
        <ls_stsma>-low = ls_constants-cnval2.
      ENDIF.
    ENDLOOP.
  ENDIF.

*-- Give error message if status profile not maintainted
  IF gr_stsma[] IS INITIAL.
    MESSAGE TEXT-004 TYPE zcl_abs_abap_maintain=>c_msgty_error "'E'
    DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_info. "'I'
    LEAVE LIST-PROCESSING.
  ENDIF.

*-- Fetch the Status Flow Steps
  SELECT stsma stsfl flstp deflt sstat
    FROM /agri/tgsfstp
    INTO TABLE gt_gsfstp
   WHERE stsma IN gr_stsma[].
  IF sy-subrc = 0.
    SORT gt_gsfstp BY stsma sstat.
  ENDIF.

*-- Fetch the Status Flow Outcomes
  SELECT *
    FROM /agri/tgsfocm
    INTO TABLE gt_gsfocm
   WHERE stsma IN gr_stsma[].
  IF sy-subrc = 0.
    SORT gt_gsfocm BY stsma stsfl flstp soutc.
  ENDIF.

*-- Fetch the user status for the status profile
  SELECT *
    FROM tj30
    INTO TABLE @DATA(lt_tj30_tmp)
   WHERE stsma IN @gr_stsma[].
  IF sy-subrc EQ 0.
    gt_tj30 = lt_tj30_tmp.
    SORT gt_tj30 BY estat stsma.
    REFRESH lt_tj30_tmp.
  ENDIF.

  LOOP AT gt_gsfstp ASSIGNING FIELD-SYMBOL(<lfs_gsfstp>). " WHERE deflt IS INITIAL.
    READ TABLE gt_gsfocm INTO DATA(ls_gsfocm)
          WITH KEY stsfl = <lfs_gsfstp>-stsfl
                   flstp = <lfs_gsfstp>-flstp
          BINARY SEARCH.
    IF sy-subrc EQ 0 AND ls_gsfocm-uentr EQ abap_true.
      READ TABLE gt_tj30 INTO DATA(ls_tj30)
            WITH KEY estat = <lfs_gsfstp>-sstat
            BINARY SEARCH.
      IF sy-subrc EQ 0.
        APPEND ls_tj30 TO lt_tj30_tmp.
      ENDIF.
    ENDIF.
  ENDLOOP.

*--Authority Check
  LOOP AT lt_tj30_tmp INTO ls_tj30.
    IF ls_tj30-bersl IS NOT INITIAL.
      AUTHORITY-CHECK OBJECT 'B_USERSTAT'
       ID 'STSMA' FIELD gv_stsma
       ID 'OBTYP' FIELD 'XMD'
       ID 'BERSL' FIELD ls_tj30-bersl
       ID 'ACTVT' FIELD '01'.
      IF sy-subrc EQ 0.
        CLEAR lsr_estat.
        lsr_estat-sign   = zcl_abs_abap_maintain=>c_rsign_include. "'I'
        lsr_estat-option = zcl_abs_abap_maintain=>c_ropt_equal. "'EQ'
        lsr_estat-low    = ls_tj30-estat. "lv_estat.
        APPEND lsr_estat TO ltr_estat.
      ENDIF.
    ELSE.
      AUTHORITY-CHECK OBJECT 'B_USERSTAT'
       ID 'STSMA' FIELD gv_stsma
       ID 'OBTYP' FIELD 'XMD'
       ID 'BERSL' DUMMY
       ID 'ACTVT' FIELD '01'.
      IF sy-subrc EQ 0.
        CLEAR lsr_estat.
        lsr_estat-sign   = zcl_abs_abap_maintain=>c_rsign_include. "'I'
        lsr_estat-option = zcl_abs_abap_maintain=>c_ropt_equal. "'EQ'
        lsr_estat-low    = ls_tj30-estat. "lv_estat.
        APPEND lsr_estat TO ltr_estat.
      ENDIF.
    ENDIF.
  ENDLOOP.

*---Fetching Farms
  SELECT tplnr_fl
    FROM /agri/glflot
    INTO TABLE @DATA(lt_glflot1)
   WHERE tplvl  EQ @zcl_abs_abap_maintain=>c_tplvl_farm "'1'
     AND iwerk  EQ @p_werks
     AND kfrst  EQ @space
     AND loevm  EQ @space.
  IF sy-subrc NE 0.
    MESSAGE i030(zabs_msgcls).
    LEAVE LIST-PROCESSING.
  ELSE.
    SORT lt_glflot1 BY tplnr_fl.
  ENDIF.

*--Fetching Terrains from Farms
  SELECT tplnr_fl, tplma
    FROM /agri/glflot
    INTO TABLE @DATA(lt_glflot2)
     FOR ALL ENTRIES IN @lt_glflot1
   WHERE tplma EQ @lt_glflot1-tplnr_fl
     AND kfrst EQ @space
     AND loevm EQ @space.
  IF sy-subrc NE 0.
    MESSAGE i105(zabs_msgcls).
    LEAVE LIST-PROCESSING.
  ELSE.
    SORT lt_glflot2 BY tplnr_fl.
  ENDIF.

*--Fetching Measurement document data based on terrains
  SELECT mdocm mdtyp tplnr_fl
         tplnr_fl AS tplnr_out
         contr mpgrp kfrst stsma
         objnr ustat
    FROM /agri/glmdhdr
    INTO TABLE gt_mdhdr
     FOR ALL ENTRIES IN lt_glflot2
   WHERE mdocm    IN so_mdocm
     AND mdtyp    EQ p_mdtyp
     AND tplnr_fl EQ lt_glflot2-tplnr_fl
     AND mpgrp    EQ p_mpgrp.

  IF sy-subrc EQ 0.
    SELECT stsma, stsfl
      FROM /agri/tgsf
      INTO TABLE @DATA(lt_stsfl)
      FOR ALL ENTRIES IN @gt_mdhdr
     WHERE stsma = @gt_mdhdr-stsma
       AND objtp = @lv_mdtyp.

    IF sy-subrc EQ 0.
      SELECT * FROM /agri/tgsft
        INTO TABLE @DATA(lt_stsflt)
        FOR ALL ENTRIES IN @lt_stsfl
       WHERE stsma EQ @lt_stsfl-stsma
         AND stsfl EQ @lt_stsfl-stsfl
         AND spras EQ @sy-langu.

      IF sy-subrc EQ 0.
        SELECT *
          FROM tj20t
          INTO TABLE @DATA(lt_tj20t)
          FOR ALL ENTRIES IN @lt_stsfl
         WHERE stsma EQ @lt_stsfl-stsma
           AND spras EQ @sy-langu.
      ENDIF.

      SORT: lt_stsfl  BY stsma,
            lt_stsflt BY stsma,
            lt_tj20t  BY stsma.
    ENDIF.

    LOOP AT gt_mdhdr ASSIGNING FIELD-SYMBOL(<lwa_mdhdr>).
      IF <lwa_mdhdr>-tplnr_fl IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ABSFL_OUTPUT'
          EXPORTING
            input  = <lwa_mdhdr>-tplnr_fl
          IMPORTING
            output = <lwa_mdhdr>-tplnr_out.

        <lwa_mdhdr>-imovel = <lwa_mdhdr>-tplnr_out(6).
        <lwa_mdhdr>-imovel_std = <lwa_mdhdr>-tplnr_out(6).
        <lwa_mdhdr>-talhao = <lwa_mdhdr>-tplnr_out+7(5).

        CALL FUNCTION 'CONVERSION_EXIT_ABSFL_INPUT'
          EXPORTING
            input      = <lwa_mdhdr>-imovel_std
          IMPORTING
            output     = <lwa_mdhdr>-imovel_std
          EXCEPTIONS
            not_found  = 1
            not_active = 2
            OTHERS     = 3.

        IF sy-subrc NE 0.
          CLEAR <lwa_mdhdr>-imovel_std.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF so_imov[] IS NOT INITIAL.
      DELETE gt_mdhdr WHERE NOT imovel_std IN so_imov[].
    ENDIF.

    IF p_region IS INITIAL.
      SELECT tplnr_fl, pltxt, ptrno
        FROM /agri/glflot
        INTO TABLE @DATA(lt_glflot)
        FOR ALL ENTRIES IN @gt_mdhdr
       WHERE tplnr_fl EQ @gt_mdhdr-imovel_std.
    ELSE.
      SELECT tplnr_fl, pltxt, ptrno
        FROM /agri/glflot
        INTO TABLE @lt_glflot
        FOR ALL ENTRIES IN @gt_mdhdr
       WHERE tplnr_fl EQ @gt_mdhdr-imovel_std
         AND rbnr1    EQ @p_region.
    ENDIF.

    IF sy-subrc EQ 0.
      SORT lt_glflot BY tplnr_fl.

      SELECT * FROM /agri/glflptr
        INTO TABLE @DATA(lt_ihpa)
        FOR ALL ENTRIES IN @lt_glflot
       WHERE ptrno = @lt_glflot-ptrno.

      IF sy-subrc EQ 0.
        SORT lt_ihpa BY ptrno parvw.

        SELECT lifnr, name1
          FROM lfa1
          INTO TABLE @DATA(lt_lfa1)
          FOR ALL ENTRIES IN @lt_ihpa
         WHERE lifnr = @lt_ihpa-lifnr.

        SORT lt_lfa1 BY lifnr.
      ENDIF.
    ENDIF.

    SELECT ebeln, bukrs, bstyp, bsart,
           loekz, aedat, telf1, unsez
      FROM ekko
      INTO TABLE @DATA(lt_ekko)
      FOR ALL ENTRIES IN @gt_mdhdr
     WHERE bukrs = @c_proposal-bukrs
       AND bstyp = @c_proposal-bstyp
       AND bsart = @c_proposal-bsart
       AND telf1 = @gt_mdhdr-imovel.

    SORT lt_ekko BY telf1 ASCENDING
                    aedat DESCENDING.

    SELECT mdocm, atinn, atzhl, atwrt, atflv
      FROM /agri/glmdatv
      INTO TABLE @DATA(lt_mdatv)
       FOR ALL ENTRIES IN @gt_mdhdr
     WHERE mdocm EQ @gt_mdhdr-mdocm.
    IF sy-subrc EQ 0.
      SORT lt_mdatv BY mdocm atinn.
    ENDIF.

    DATA(lt_crop_seasons) = gt_mdhdr.
    SORT lt_crop_seasons BY tplnr_fl contr.
    DELETE ADJACENT DUPLICATES FROM lt_crop_seasons COMPARING tplnr_fl contr.

*--Fetching Crop Seasons data
    SELECT tplnr_fl, contr, datab, datbi, ymatnr, zzfazplantio
      FROM /agri/glflcma
      INTO TABLE @DATA(lt_glflcma)
       FOR ALL ENTRIES IN @lt_crop_seasons
     WHERE tplnr_fl  EQ @lt_crop_seasons-tplnr_fl
       AND contr     EQ @lt_crop_seasons-contr
       AND astat     EQ @zcl_abs_abap_maintain=>c_cs_active. "'A'
    IF sy-subrc NE 0.
      MESSAGE i030(zabs_msgcls).
      LEAVE LIST-PROCESSING.
    ELSE.
      SORT lt_glflcma BY tplnr_fl contr.
      DATA(lt_glflcma_temp) = lt_glflcma.
      DELETE ADJACENT DUPLICATES FROM lt_glflcma_temp COMPARING ymatnr.

      SELECT matnr, maktx
        FROM makt
        INTO TABLE @DATA(lt_makt)
        FOR ALL ENTRIES IN @lt_glflcma_temp
        WHERE matnr EQ @lt_glflcma_temp-ymatnr
          AND spras EQ @sy-langu.
      IF sy-subrc EQ 0.
        SORT lt_makt BY matnr.
      ENDIF.
    ENDIF.
  ENDIF.

*--Fetching Class Header Data
  SELECT clint, class, klart
    FROM klah
    INTO @DATA(ls_klah)
   WHERE class EQ @p_mpgrp.
  ENDSELECT.

  CALL FUNCTION '/AGRI/GLAG_VIEW_SINGLE'
    EXPORTING
      i_clint                = ls_klah-clint
      i_atgrp_type           = ls_klah-klart
      i_no_attributes_addata = abap_true
    IMPORTING
      es_atgdoc              = ls_atgdoc.

*--Fetching Attribute names for measurement documents
  IF ls_atgdoc-x-ksml IS NOT INITIAL.
    SELECT a~atinn a~atnam b~atbez
      FROM cabn AS a
     INNER JOIN cabnt AS b
        ON b~atinn = a~atinn
      INTO TABLE lt_cabn
       FOR ALL ENTRIES IN ls_atgdoc-x-ksml
     WHERE a~atinn = ls_atgdoc-x-ksml-imerk
       AND b~spras = sy-langu.
    IF sy-subrc = 0.
      SORT lt_cabn BY atinn atnam.

      READ TABLE lt_cabn INTO DATA(ls_cabn)
        WITH KEY atnam = 'CIT-JUS-EST'.
      IF sy-subrc = 0.
*--Fetching characteristic values to get description
        SELECT a~atinn,a~atwrt,
               b~atwtb
          INTO TABLE @DATA(lt_descr)
          FROM cawn AS a
         RIGHT JOIN cawnt AS b
            ON b~atinn EQ a~atinn
           AND b~atzhl EQ a~atzhl
         WHERE a~atinn EQ @ls_cabn-atinn
           AND b~spras EQ @sy-langu.
        IF sy-subrc = 0.
          SORT lt_descr BY atinn ASCENDING
                           atwrt ASCENDING.
        ENDIF.
      ENDIF.

      READ TABLE lt_cabn INTO ls_cabn
        WITH KEY atnam = 'CIT-MOT-EST'.
      IF sy-subrc = 0.
*--Fetching characteristic values to get description
        SELECT a~atinn,a~atwrt,
               b~atwtb
          APPENDING TABLE @lt_descr
          FROM cawn AS a
         RIGHT JOIN cawnt AS b
            ON b~atinn EQ a~atinn
           AND b~atzhl EQ a~atzhl
         WHERE a~atinn EQ @ls_cabn-atinn
           AND b~spras EQ @sy-langu.
        IF sy-subrc = 0.
          SORT lt_descr BY atinn ASCENDING
                           atwrt ASCENDING.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  PERFORM prepare_catalog.

  DATA(lv_parceiro_fo) = 'FO'.
  CALL FUNCTION 'CONVERSION_EXIT_PARVW_INPUT'
    EXPORTING
      input  = lv_parceiro_fo
    IMPORTING
      output = lv_parceiro_fo.

  LOOP AT gt_mdhdr INTO DATA(ls_mdhdr).

    UNASSIGN <lfs_gsfstp>.
    READ TABLE gt_gsfstp ASSIGNING <lfs_gsfstp>
      WITH KEY stsma = ls_mdhdr-stsma
               sstat = ls_mdhdr-ustat BINARY SEARCH.

    IF sy-subrc <> 0.
      CONTINUE.
    ELSE.
      INSERT INITIAL LINE INTO TABLE gt_output
        ASSIGNING FIELD-SYMBOL(<gfs_dyn_smdm>).
    ENDIF.

    READ TABLE lt_stsflt INTO DATA(ls_stsflt)
      WITH KEY stsma = ls_mdhdr-stsma BINARY SEARCH.
    IF sy-subrc EQ 0.
      ASSIGN COMPONENT 'STSFLDESC' OF STRUCTURE <gfs_dyn_smdm>
        TO FIELD-SYMBOL(<fs_value>).
      IF sy-subrc EQ 0.
        <fs_value> = ls_stsflt-descr.
      ENDIF.
    ENDIF.

    READ TABLE lt_tj20t INTO DATA(ls_tj20t)
      WITH KEY stsma = ls_mdhdr-stsma BINARY SEARCH.
    IF sy-subrc EQ 0.
      ASSIGN COMPONENT 'STSMADESC' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
      IF sy-subrc EQ 0.
        <fs_value> = ls_tj20t-txt.
      ENDIF.
    ENDIF.

    ASSIGN COMPONENT 'PPNUM' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      READ TABLE lt_ekko INTO DATA(ls_ekko)
        WITH KEY telf1 = ls_mdhdr-imovel BINARY SEARCH.
      IF sy-subrc NE 0.
        CLEAR ls_ekko.
      ENDIF.
      <fs_value> = ls_ekko-unsez.
    ENDIF.

    ASSIGN COMPONENT 'IMOVEL' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      <fs_value> = ls_mdhdr-imovel.
    ENDIF.

    ASSIGN COMPONENT 'KFRST' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      <fs_value> = ls_mdhdr-kfrst.
    ENDIF.

    ASSIGN COMPONENT 'IMODESC' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      READ TABLE lt_glflot INTO DATA(ls_glflot)
        WITH KEY tplnr_fl = ls_mdhdr-imovel_std BINARY SEARCH.
      IF sy-subrc NE 0.
        CLEAR ls_glflot.
      ENDIF.
      <fs_value> = ls_glflot-pltxt.
    ENDIF.

    ASSIGN COMPONENT 'LIFNR' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      DATA(lv_partner_found) = abap_false.
      READ TABLE lt_glflot INTO ls_glflot
        WITH KEY tplnr_fl = ls_mdhdr-imovel_std BINARY SEARCH.
      IF sy-subrc EQ 0.
        READ TABLE lt_ihpa INTO DATA(ls_ihpa)
          WITH KEY ptrno = ls_glflot-ptrno
                   parvw = lv_parceiro_fo BINARY SEARCH.
        IF sy-subrc EQ 0.
          lv_partner_found = abap_true.
        ENDIF.
      ENDIF.
      IF lv_partner_found = abap_true.
        <fs_value> = ls_ihpa-lifnr.
      ENDIF.
    ENDIF.

    IF lv_partner_found = abap_true.
      ASSIGN COMPONENT 'LIFNRDESC' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
      IF sy-subrc EQ 0.
        READ TABLE lt_lfa1 INTO DATA(ls_lfa1)
          WITH KEY lifnr = ls_ihpa-lifnr BINARY SEARCH.
        IF sy-subrc NE 0.
          CLEAR ls_lfa1.
        ENDIF.
        <fs_value> = ls_lfa1-name1.
      ENDIF.
    ENDIF.

    ASSIGN COMPONENT 'TALHAO' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      <fs_value> = ls_mdhdr-talhao.
    ENDIF.

    ASSIGN COMPONENT 'MDOCM' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      <fs_value> = ls_mdhdr-mdocm.
    ENDIF.

    UNASSIGN <fs_value>.
    ASSIGN COMPONENT 'MDTYP' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      <fs_value> = ls_mdhdr-mdtyp.
    ENDIF.

    UNASSIGN <fs_value>.
    ASSIGN COMPONENT 'MPGRP' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      <fs_value> = ls_mdhdr-mpgrp.
    ENDIF.

    UNASSIGN <fs_value>.
    ASSIGN COMPONENT 'STSFL' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      <fs_value> = <lfs_gsfstp>-stsfl.
    ENDIF.

    UNASSIGN <fs_value>.
    ASSIGN COMPONENT 'STSMA' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      <fs_value> = <lfs_gsfstp>-stsma.
    ENDIF.

    UNASSIGN <fs_value>.
    ASSIGN COMPONENT 'FLSTP' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      <fs_value> = <lfs_gsfstp>-flstp.
    ENDIF.

    UNASSIGN <fs_value>.
    ASSIGN COMPONENT 'TPLNR' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      <fs_value> = ls_mdhdr-tplnr_fl.
    ENDIF.

    READ TABLE lt_glflcma INTO DATA(ls_glflcma)
      WITH KEY tplnr_fl = ls_mdhdr-tplnr_fl
               contr    = ls_mdhdr-contr BINARY SEARCH.
    IF sy-subrc EQ 0.
      UNASSIGN <fs_value>.
      ASSIGN COMPONENT 'YMATNR' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
      IF <fs_value> IS ASSIGNED.
        <fs_value> = ls_glflcma-ymatnr.
      ENDIF.

      READ TABLE lt_makt INTO DATA(ls_makt)
        WITH KEY matnr = ls_glflcma-ymatnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        UNASSIGN <fs_value>.
        ASSIGN COMPONENT 'MAKTX' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
        IF <fs_value> IS ASSIGNED.
          <fs_value> = ls_makt-maktx.
        ENDIF.
      ENDIF.

      IF ls_glflcma-zzfazplantio IS NOT INITIAL.
        UNASSIGN <fs_value>.
        ASSIGN COMPONENT 'CSYEAR' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
        IF <fs_value> IS ASSIGNED.
          IF ls_glflcma-datbi GE sy-datum.
            ls_glflcma-datbi = sy-datum.
          ENDIF.
          <fs_value> = ( ls_glflcma-datbi - ls_glflcma-zzfazplantio ) / 365.
        ENDIF.
      ENDIF.
    ENDIF.

    READ TABLE lt_mdatv TRANSPORTING NO FIELDS
      WITH KEY mdocm = ls_mdhdr-mdocm BINARY SEARCH.

    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    lv_tabix = sy-tabix.

    LOOP AT lt_mdatv INTO DATA(ls_mdatv) FROM lv_tabix.
      IF ls_mdatv-mdocm <> ls_mdhdr-mdocm.
        EXIT.
      ENDIF.

      READ TABLE lt_fname INTO DATA(ls_fname)
        WITH KEY atinn = ls_mdatv-atinn.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      ASSIGN COMPONENT ls_fname-fieldname OF STRUCTURE <gfs_dyn_smdm>
       TO <gfs_value>.
      IF <gfs_value> IS ASSIGNED.
        IF ls_mdatv-atwrt IS INITIAL.
          lv_packed = ls_mdatv-atflv.
          ls_mdatv-atwrt = lv_packed.
          CONDENSE ls_mdatv-atwrt NO-GAPS.
        ENDIF.
        <gfs_value> = ls_mdatv-atwrt.

        IF ls_fname-fieldname EQ 'MOT_EST'.
          READ TABLE lt_descr INTO DATA(ls_descr)
            WITH KEY atinn = ls_mdatv-atinn
                     atwrt = ls_mdatv-atwrt BINARY SEARCH.
          IF sy-subrc EQ 0.
            ASSIGN COMPONENT 'MOT_ESTDESC' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
            IF sy-subrc EQ 0.
              <fs_value> = ls_descr-atwtb.
            ENDIF.
          ENDIF.
        ELSEIF ls_fname-fieldname EQ 'JUS_EST'.
          READ TABLE lt_descr INTO ls_descr
            WITH KEY atinn = ls_mdatv-atinn
                     atwrt = ls_mdatv-atwrt BINARY SEARCH.
          IF sy-subrc EQ 0.
            ASSIGN COMPONENT 'JUS_ESTDESC' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
            IF sy-subrc EQ 0.
              <fs_value> = ls_descr-atwtb.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDLOOP.
  ENDLOOP.

  gt_aux[] = gt_copy[] = gt_output[].

  SORT gt_output BY imovel  ASCENDING
                    talhao  ASCENDING
                    florada ASCENDING
                    mdocm   ASCENDING.

  SORT gt_copy BY imovel  ASCENDING
                  talhao  ASCENDING
                  florada ASCENDING
                  mdocm   DESCENDING.

  DELETE gt_aux WHERE kfrst <> abap_false.

  SORT gt_aux BY imovel  ASCENDING
                 talhao  ASCENDING
                 florada ASCENDING
                 mdocm   DESCENDING.

  CLEAR ls_approvals.
  LOOP AT gt_output ASSIGNING <gfs_dyn_smdm>.
    lv_tabix = sy-tabix.

    ASSIGN COMPONENT:
      'IMOVEL'  OF STRUCTURE <gfs_dyn_smdm> TO FIELD-SYMBOL(<fs_imovel>),
      'TALHAO'  OF STRUCTURE <gfs_dyn_smdm> TO FIELD-SYMBOL(<fs_talhao>),
      'FLORADA' OF STRUCTURE <gfs_dyn_smdm> TO FIELD-SYMBOL(<fs_florada>),
      'MOT_EST' OF STRUCTURE <gfs_dyn_smdm> TO FIELD-SYMBOL(<fs_motivo>),
      'MDOCM'   OF STRUCTURE <gfs_dyn_smdm> TO FIELD-SYMBOL(<fs_mdocm>),
      'KFRST'   OF STRUCTURE <gfs_dyn_smdm> TO FIELD-SYMBOL(<fs_kfrst>).

    IF  <fs_imovel>  IS ASSIGNED
    AND <fs_talhao>  IS ASSIGNED
    AND <fs_florada> IS ASSIGNED
    AND <fs_motivo>  IS ASSIGNED
    AND <fs_mdocm>   IS ASSIGNED
    AND <fs_kfrst>   IS ASSIGNED.
      IF <fs_motivo> EQ '1'
      OR <fs_motivo> EQ '2'.
*-- Checks whether it is the first document
        IF ls_approvals-imovel  NE <fs_imovel>
        OR ls_approvals-talhao  NE <fs_talhao>
        OR ls_approvals-florada NE <fs_florada>.
          ASSIGN COMPONENT 'INICIAL' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
          IF sy-subrc EQ 0.
            <fs_value> = abap_true.
          ENDIF.
        ENDIF.
      ENDIF.

*-- Checks whether it is the lat document
      READ TABLE gt_output ASSIGNING FIELD-SYMBOL(<gfs_scopy>)
        WITH KEY (lc_sort-imovel)  = <fs_imovel>
                 (lc_sort-talhao)  = <fs_talhao>
                 (lc_sort-florada) = <fs_florada> BINARY SEARCH.
      IF sy-subrc EQ 0.
        ASSIGN COMPONENT 'MDOCM' OF STRUCTURE <gfs_scopy> TO FIELD-SYMBOL(<fs_last_mdocm>).
        IF sy-subrc EQ 0.
          IF <fs_mdocm> EQ <fs_last_mdocm>.
            ASSIGN COMPONENT 'ATUAL' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
            IF sy-subrc EQ 0.
              <fs_value> = abap_true.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

*-- Released
      IF <fs_kfrst> EQ abap_false.
        READ TABLE gt_aux ASSIGNING FIELD-SYMBOL(<gfs_saux>)
          WITH KEY (lc_sort-imovel)  = <fs_imovel>
                   (lc_sort-talhao)  = <fs_talhao>
                   (lc_sort-florada) = <fs_florada> BINARY SEARCH.
        IF sy-subrc EQ 0.
          ASSIGN COMPONENT 'MDOCM' OF STRUCTURE <gfs_saux> TO FIELD-SYMBOL(<fs_last_released>).
          IF sy-subrc EQ 0.
            IF <fs_mdocm> EQ <fs_last_released>.
              ASSIGN COMPONENT 'ANTERIOR' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
              IF sy-subrc EQ 0.
                <fs_value> = abap_true.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    ASSIGN COMPONENT 'EST_COLHEITA' OF STRUCTURE <gfs_dyn_smdm> TO FIELD-SYMBOL(<fs_from>).
    IF sy-subrc EQ 0.
      ASSIGN COMPONENT 'EST_INICIAL' OF STRUCTURE <gfs_dyn_smdm> TO FIELD-SYMBOL(<fs_to>).
      IF sy-subrc EQ 0.
        <fs_to> = <fs_from>.
      ENDIF.
      ASSIGN COMPONENT 'EST_ATUAL' OF STRUCTURE <gfs_dyn_smdm> TO <fs_to>.
      IF sy-subrc EQ 0.
        <fs_to> = <fs_from>.
      ENDIF.
      ASSIGN COMPONENT 'EST_ANTERIOR' OF STRUCTURE <gfs_dyn_smdm> TO <fs_to>.
      IF sy-subrc EQ 0.
        <fs_to> = <fs_from>.
      ENDIF.
    ENDIF.

    IF p_year IS NOT INITIAL.
      ASSIGN COMPONENT 'MDM001' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
      IF sy-subrc EQ 0.
        IF <fs_value>(4) NE p_year.
          DELETE gt_output INDEX lv_tabix.
        ENDIF.
      ENDIF.
    ENDIF.

    ls_approvals-imovel  = <fs_imovel>.
    ls_approvals-talhao  = <fs_talhao>.
    ls_approvals-florada = <fs_florada>.
  ENDLOOP.

  IF gt_output IS INITIAL.
    MESSAGE i030(zabs_msgcls).
    LEAVE LIST-PROCESSING.
  ELSE.
    LOOP AT gt_output ASSIGNING <gfs_dyn_smdm>.
      ASSIGN COMPONENT 'INICIAL' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
      IF sy-subrc EQ 0.
        ASSIGN COMPONENT 'EST_INICIAL' OF STRUCTURE <gfs_dyn_smdm> TO FIELD-SYMBOL(<fs_inicial>).
        IF sy-subrc EQ 0.
          IF <fs_value> NE abap_true.
            CLEAR <fs_inicial>.
          ENDIF.
        ENDIF.
      ENDIF.

      ASSIGN COMPONENT 'ATUAL' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
      IF sy-subrc EQ 0.
        ASSIGN COMPONENT 'EST_ATUAL' OF STRUCTURE <gfs_dyn_smdm> TO FIELD-SYMBOL(<fs_atual>).
        IF sy-subrc EQ 0.
          IF <fs_value> NE abap_true.
            CLEAR <fs_atual>.
          ENDIF.
        ENDIF.
      ENDIF.

      ASSIGN COMPONENT 'ANTERIOR' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
      IF sy-subrc EQ 0.
        ASSIGN COMPONENT 'EST_ANTERIOR' OF STRUCTURE <gfs_dyn_smdm> TO FIELD-SYMBOL(<fs_anterior>).
        IF sy-subrc EQ 0.
          IF <fs_value> NE abap_true.
            CLEAR <fs_anterior>.
          ENDIF.
        ENDIF.
      ENDIF.

*-- Variação Anterior = [(Est.Anterior - Est.Inicial) / Est.Inicial ] * 100
      ASSIGN COMPONENT 'VAR_ANTERIOR' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
      IF sy-subrc EQ 0.
        IF <fs_anterior> IS ASSIGNED
        AND <fs_inicial> IS ASSIGNED
        AND <fs_inicial> IS NOT INITIAL.
          <fs_value> = ( ( <fs_anterior> - <fs_inicial> ) / <fs_inicial> ) * 100.
        ENDIF.
      ENDIF.

*-- Variação Atual = [(Est.Atual - Est.Inicial) / Est.Inicial ] * 100
      ASSIGN COMPONENT 'VAR_ATUAL' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
      IF sy-subrc EQ 0.
        IF <fs_atual> IS ASSIGNED
        AND <fs_inicial> IS ASSIGNED
        AND <fs_inicial> IS NOT INITIAL.
          <fs_value> = ( ( <fs_atual> - <fs_inicial> ) / <fs_inicial> ) * 100.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DYNAMIC_FCAT_PREPARE
*&---------------------------------------------------------------------*
*& DYNAMIC_FCAT_PREPARE
*&---------------------------------------------------------------------*
FORM dynamic_fcat_prepare  USING    pt_ksml  TYPE /agri/t_gksml
                                    pt_cabn  TYPE tty_cabn
                        CHANGING    pt_fname TYPE tty_fname.

*--Local variable declaration
  DATA : lv_col_pos  TYPE i,
         lv_contr(3) TYPE c,

*--Workarea declaration
         ls_fname    TYPE ty_fname,
         ls_fcat     TYPE lvc_s_fcat.

*--Field catalog prepare
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = zcl_abs_abap_maintain=>c_str_md_mass_approvals "'ZABS_STR_MD_MASS_APPROVALS'
    CHANGING
      ct_fieldcat            = gt_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*-- BOC-T_M.ALFREDO
*  LOOP AT gt_fcat INTO ls_fcat WHERE fieldname = 'STSFL'
*                                  OR fieldname = 'STSMA'
*                                  OR fieldname = 'FLSTP'.
*    IF sy-subrc = 0.
*      ls_fcat-no_out = abap_true.
*      MODIFY gt_fcat FROM ls_fcat TRANSPORTING no_out.
*    ENDIF.
*    CLEAR : ls_fcat.
*  ENDLOOP.
  LOOP AT gt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
    IF <ls_fcat>-fieldname = 'STSFL'
    OR <ls_fcat>-fieldname = 'STSMA'
    OR <ls_fcat>-fieldname = 'FLSTP'
    OR <ls_fcat>-fieldname = 'CSYEAR'
    OR <ls_fcat>-fieldname = 'TALHAO'
    OR <ls_fcat>-fieldname = 'INICIAL'
    OR <ls_fcat>-fieldname = 'ATUAL'
    OR <ls_fcat>-fieldname = 'ANTERIOR'.
      <ls_fcat>-no_out = abap_true.
    ENDIF.

    IF <ls_fcat>-fieldname = 'EST_INICIAL'
    OR <ls_fcat>-fieldname = 'EST_ATUAL'
    OR <ls_fcat>-fieldname = 'EST_ANTERIOR'.
      <ls_fcat>-key = abap_true.
      <ls_fcat>-do_sum = abap_true.
    ENDIF.
  ENDLOOP.
*-- EOC-T_M.ALFREDO

*-- BOC-T_M.ALFREDO
*  READ TABLE pt_cabn INTO DATA(ls_cabn)
*                      WITH KEY atnam = zcl_abs_abap_maintain=>c_charact_contamotivo
*                 BINARY SEARCH. "Reason
*  IF sy-subrc <> 0.
*    READ TABLE gt_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>) WITH KEY fieldname = 'RSDESC'.
*    IF sy-subrc = 0.
*      <fs_fcat>-no_out = abap_true.
**      MODIFY gt_fcat FROM ls_fcat TRANSPORTING no_out.
**          CLEAR : ls_fcat.
*    ENDIF.
*  ENDIF.
*
*  CLEAR ls_cabn.
*  READ TABLE pt_cabn INTO ls_cabn
*                 WITH KEY atnam = zcl_abs_abap_maintain=>c_charact_atividade
*            BINARY SEARCH. "Inventory Activity
*  IF sy-subrc <> 0.
*    UNASSIGN <fs_fcat>.
*    READ TABLE gt_fcat ASSIGNING <fs_fcat> WITH KEY fieldname = 'ACTDESC'.
*    IF sy-subrc = 0.
*      <fs_fcat>-no_out = abap_true.
**      MODIFY gt_fcat FROM ls_fcat TRANSPORTING no_out INDEX lv_tabix.
**      CLEAR : ls_fcat.
*    ENDIF.
*  ENDIF.
*-- EOC-T_M.ALFREDO

  LOOP AT pt_ksml INTO DATA(ls_ksml).

    DESCRIBE TABLE gt_fcat LINES lv_col_pos.

*--Fill Internal Table for Linking Attributes and Dynamic Columns
*-- BOC-T_M.ALFREDO
*    CLEAR ls_cabn.
*    READ TABLE pt_cabn INTO ls_cabn
*      WITH KEY atinn = ls_ksml-imerk BINARY SEARCH.
    READ TABLE pt_cabn INTO DATA(ls_cabn)
      WITH KEY atinn = ls_ksml-imerk BINARY SEARCH.
*-- EOC-T_M.ALFREDO

    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

*-- BOC-T_M.ALFREDO
**--Append Dynamic Fieldnames to Internal Table
*      ADD 1 TO lv_contr.
*      UNPACK lv_contr TO lv_contr.
*
**--Dynamic Field Names
*      CONCATENATE 'MDM' lv_contr INTO ls_fcat-fieldname.
    IF ls_ksml-atnam EQ 'CIT-MOT-EST'.
      ls_fcat-fieldname = 'MOT_EST'.
    ELSEIF ls_ksml-atnam EQ 'CIT-JUS-EST'.
      ls_fcat-fieldname = 'JUS_EST'.
    ELSEIF ls_ksml-atnam EQ 'CIT-ESTIMATIVA'.
      ls_fcat-fieldname = 'EST_COLHEITA'.
    ELSEIF ls_ksml-atnam EQ 'CIT-SAFRA'.
      ls_fcat-fieldname = 'SAFRA'.
    ELSEIF ls_ksml-atnam EQ 'CIT-FLORADA'.
      ls_fcat-fieldname = 'FLORADA'.
    ELSE.
*--Append Dynamic Fieldnames to Internal Table
      ADD 1 TO lv_contr.
      UNPACK lv_contr TO lv_contr.
*--Dynamic Field Names
      CONCATENATE 'MDM' lv_contr INTO ls_fcat-fieldname.
    ENDIF.
*-- EOC-T_M.ALFREDO

*--Append Dynamic Columns to Field Catalog
    ls_fname-fieldname = ls_fcat-fieldname.
    ls_fname-atinn     = ls_cabn-atinn.
    ls_fname-atnam     = ls_cabn-atnam.
    ls_fcat-col_pos    = lv_col_pos + 1.
    ls_fcat-intlen     = '30'.
    ls_fcat-scrtext_s  = ls_cabn-atbez.
    ls_fcat-scrtext_m  = ls_cabn-atbez.
    ls_fcat-scrtext_l  = ls_cabn-atbez.
    ls_fcat-reptext    = ls_cabn-atbez.
    ls_fcat-tooltip    = ls_cabn-atbez.
    ls_fcat-coltext    = ls_cabn-atbez.
    APPEND ls_fname TO pt_fname.
    APPEND ls_fcat TO gt_fcat.
    CLEAR ls_fcat.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_MEASUREMENT_DATA
*&---------------------------------------------------------------------*
*& DISPLAY_MEASUREMENT_DATA
*&---------------------------------------------------------------------*
FORM display_measurement_data.
  CALL SCREEN 100.
ENDFORM.

*&---------------------------------------------------------------------*
*& Module STATUS_SET OUTPUT
*&---------------------------------------------------------------------*
*& STATUS_SET
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
FORM title_set .
  SET TITLEBAR 'T100'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Module CONTROLS_DISPLAY OUTPUT
*&---------------------------------------------------------------------*
*& Display Controls
*&---------------------------------------------------------------------*
MODULE controls_display OUTPUT.
  PERFORM controls_display.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form CONTROLS_DISPLAY
*&---------------------------------------------------------------------*
*& Display Controls
*&---------------------------------------------------------------------*
FORM controls_display.

*--Workarea declaration
*-- BOC-T_M.ALFREDO
*  DATA : ls_layout TYPE lvc_s_layo.
  DATA: ls_layout  TYPE lvc_s_layo,
        ls_variant TYPE disvariant.
*-- EOC-T_M.ALFREDO

  IF gobj_alv IS NOT INITIAL.
    CALL METHOD gobj_alv->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    RETURN.
  ENDIF.

*--CREATE OBJECT for custom container
  CREATE OBJECT gobj_cont
    EXPORTING
      container_name = 'C_MDMS_APR_0100_CC'.

*--Create Object for ALV Grid
  CREATE OBJECT gobj_alv
    EXPORTING
      i_parent = gobj_cont.

*--Set ALV attributes for layout
  ls_layout-cwidth_opt = abap_true.
  ls_layout-zebra      = abap_true.
  ls_layout-sel_mode   = zcl_abs_abap_maintain=>c_layout_sel_mode.  "'A'

*-- BOC-T_M.ALFREDO
  ls_variant-report = sy-repid.
  ls_variant-handle = 'GRID'.

*-- Fornecedor/Imóvel/Material
  DO 3 TIMES.
    DATA(lv_index) = sy-index.
    INSERT INITIAL LINE INTO TABLE gt_sort
      ASSIGNING FIELD-SYMBOL(<ls_sort>).
    IF sy-subrc EQ 0.
      <ls_sort>-spos   = lv_index.
      <ls_sort>-up     = abap_true. "Ascending
      IF lv_index EQ 1.
        <ls_sort>-subtot = abap_true. "Subtotal at field
      ENDIF.
*      <ls_sort>-group  = '*'.
      CASE lv_index.
        WHEN 1.
          DATA(lv_fieldname) = 'LIFNRDESC'.
        WHEN 2.
          lv_fieldname = 'IMODESC'.
        WHEN 3.
          lv_fieldname = 'MAKTX'.
      ENDCASE.
      <ls_sort>-fieldname = lv_fieldname.
    ENDIF.
  ENDDO.

  SORT gt_output BY lifnrdesc imodesc maktx.
*-- EOC-T_M.ALFREDO

*--Displaying ALV Data
  IF gobj_alv IS NOT INITIAL.
    CALL METHOD gobj_alv->set_table_for_first_display
      EXPORTING
*-- BOC-T_M.ALFREDO
        is_variant                    = ls_variant
        i_save                        = 'A'
        i_default                     = abap_true
*-- EOC-T_M.ALFREDO
        is_layout                     = ls_layout
      CHANGING
        it_outtab                     = gt_output
        it_fieldcatalog               = gt_fcat
        it_sort                       = gt_sort
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
      MESSAGE i035(zabs_msgcls).
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Module  FCODE_PROCESSING  INPUT
*&---------------------------------------------------------------------*
*& FCODE_PROCESSING
*----------------------------------------------------------------------*
MODULE fcode_processing INPUT.
  PERFORM fcode_processing.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form FCODE_BACK
*&---------------------------------------------------------------------*
*& For Back Button
*&---------------------------------------------------------------------*
FORM fcode_back.
  SET SCREEN 0.
  LEAVE SCREEN.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form FCODE_CANC
*&---------------------------------------------------------------------*
*& For Cancel Button
*&---------------------------------------------------------------------*
FORM fcode_canc.
  SET SCREEN 0.
  LEAVE SCREEN.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form FCODE_EXIT
*&---------------------------------------------------------------------*
*& For Exit Button
*&---------------------------------------------------------------------*
FORM fcode_exit.
  SET SCREEN 0.
  LEAVE SCREEN.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form FCODE_EXIT
*&---------------------------------------------------------------------*
*& For Approvals
*&---------------------------------------------------------------------*
FORM fcode_approve.

  DATA: lv_soutc TYPE /agri/gsfsoc.

  lv_soutc = 1.
  PERFORM approve_reject USING lv_soutc.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FCODE_EXIT
*&---------------------------------------------------------------------*
*& For Reject
*&---------------------------------------------------------------------*
FORM fcode_reject.

  DATA: lv_soutc TYPE /agri/gsfsoc.

  lv_soutc = 2.
  PERFORM approve_reject USING lv_soutc.

ENDFORM.

**&---------------------------------------------------------------------*
**& Form F4_FOR_REGION
**&---------------------------------------------------------------------*
**& F4_FOR_REGION
**&---------------------------------------------------------------------*
*FORM f4_for_region.
*
**--Local declarations
*  DATA: lt_values TYPE TABLE OF cawn,
*        lv_value  TYPE atwrt.
*
*  CALL FUNCTION '/AGRI/G_CHARACTERISTIC_F4'
*    EXPORTING
*      i_atnam            = zcl_abs_abap_maintain=>c_charact_faz_regiao
*      i_show_description = abap_true
*    IMPORTING
*      e_value            = lv_value
*    TABLES
*      t_values           = lt_values
*    EXCEPTIONS
*      charact_not_found  = 1
*      no_values_found    = 2
*      OTHERS             = 3.
*  IF sy-subrc <> 0.
*    MESSAGE TEXT-006 TYPE zcl_abs_abap_maintain=>c_msgty_error.
*  ENDIF.
*
*  p_region = lv_value.
*
*ENDFORM.

*&---------------------------------------------------------------------*
*& Form MESSAGES_DISPLAY
*&---------------------------------------------------------------------*
*& MESSAGES_DISPLAY
*&---------------------------------------------------------------------*
FORM messages_display  USING lv_initiator TYPE /irm/gdescr.

  DATA: ls_variant TYPE disvariant.
  ls_variant-report = 'ZABS_INC_MD_MASS_APPROVALS'.
  ls_variant-handle = lv_initiator.

  messages_display lv_initiator c_true space space ls_variant.
  messages_init.
  CLEAR gs_variables-initiator.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form APPROVE_REJECT
*&---------------------------------------------------------------------*
*& APPROVE_REJECT
*&---------------------------------------------------------------------*
FORM approve_reject USING pv_soutc TYPE /agri/gsfsoc.

*--Internal table declaration
  DATA : lt_index_rows TYPE TABLE OF lvc_s_row,
         lt_msgtab     TYPE esp1_message_tab_type,
         lt_message    TYPE TABLE OF /agri/s_gprolog,
         lt_index      TYPE TABLE OF hcp_s_index,
         lt_mdcom      TYPE TABLE OF ty_msg_out,

*--Workarea declaration
         ls_message    TYPE /agri/s_gprolog,
         ls_rows       TYPE lvc_s_row,
         ls_msgtab     TYPE esp1_message_wa_type,
         ls_index      TYPE hcp_s_index,
         ls_outcome    TYPE /agri/s_gacoutcome,
         ls_mdcom      TYPE ty_msg_out,

*--Variable declaration
         lv_mdocm      TYPE /agri/glmdocm.

*--Field-symbol declaration
  FIELD-SYMBOLS :   <fs_value> TYPE any.

  IF sy-uname EQ 'T_M.ALFREDO'.
    BREAK-POINT."gv_stsma
  ENDIF.

  CALL METHOD gobj_alv->get_selected_rows
    IMPORTING
      et_index_rows = lt_index_rows
      et_row_no     = DATA(lt_row_no).

  gs_variables-initiator = c_log_initiator-save.
  PERFORM messages_initialize USING gs_variables-initiator
                                    c_log_subobject-save.

  IF lt_index_rows IS INITIAL.
    MESSAGE  TEXT-005 TYPE zcl_abs_abap_maintain=>c_msgty_info. "'I'
  ENDIF.

  LOOP AT lt_index_rows INTO ls_rows.

    READ TABLE <gfs_dyn_tmdm> ASSIGNING FIELD-SYMBOL(<fs_mdm>)
     INDEX ls_rows-index.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    READ TABLE gt_mdhdr INTO DATA(ls_mdhdr) INDEX ls_rows-index.

    IF ls_mdhdr-stsma IS NOT INITIAL AND ls_mdhdr-ustat IS NOT INITIAL.

      READ TABLE gt_gsfstp ASSIGNING FIELD-SYMBOL(<lfs_gsfstp>)
        WITH KEY stsma = ls_mdhdr-stsma
                 sstat = ls_mdhdr-ustat BINARY SEARCH.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      READ TABLE gt_gsfocm ASSIGNING FIELD-SYMBOL(<lfs_gsfocm>)
        WITH KEY stsma = <lfs_gsfstp>-stsma
                 stsfl = <lfs_gsfstp>-stsfl
                 flstp = <lfs_gsfstp>-flstp
                 soutc = pv_soutc BINARY SEARCH.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      IF pv_soutc = '2'.
        READ TABLE gt_tj30 INTO DATA(ls_tj30)
          WITH KEY stsma = <lfs_gsfstp>-stsma
                   estat = <lfs_gsfstp>-sstat BINARY SEARCH.
        IF sy-subrc = 0.
          IF ls_tj30-inist = abap_true.
            ls_msgtab-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
            ls_msgtab-msgno = '108'.
            ls_msgtab-msgty = zcl_abs_abap_maintain=>c_msgty_info. "'I'
            ls_msgtab-msgv1 = ls_mdhdr-mdocm.
            APPEND ls_msgtab TO lt_msgtab.
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.

      CLEAR : ls_outcome.
      REFRESH : lt_message[].
      ls_outcome-stsfl  = <lfs_gsfocm>-stsfl.
*-- BOC-T_M.ALFREDO
*      ls_outcome-stsma  = gv_stsma. " gw_gsfstp-stsma
      ls_outcome-stsma  = ls_mdhdr-stsma.
*-- EOC-T_M.ALFREDO
      ls_outcome-flstp  = <lfs_gsfstp>-flstp.
      ls_outcome-soutc  = pv_soutc.

      PERFORM messages_context_set USING ls_mdhdr-mdocm ."lv_posnr.

*--Seting User Status Externally for Measurement Document
      CALL FUNCTION '/AGRI/GLMD_USER_STATUS_SET'
        EXPORTING
          i_mdocm                        = ls_mdhdr-mdocm
          i_set_inactive                 = ''
          is_act_outcome                 = ls_outcome
        IMPORTING
          et_messages                    = lt_message
        EXCEPTIONS
          measurement_document_not_found = 1
          measurement_document_locked    = 2
          object_errors                  = 3
          errors_in_save                 = 4
          OTHERS                         = 5.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      LOOP AT lt_message INTO ls_message.
        ls_message-context-context-tabname = zcl_abs_abap_maintain=>c_str_glmd_context. "'ZABS_STR_GLMD_CONTEXT'
        messages_context_data_set ls_message-context-document
                                  space
                                  space
                                  ls_message-context-context-tabname
                                  ls_message-context-context-value.
        MESSAGE ID ls_message-msgid TYPE ls_message-msgty
           NUMBER ls_message-msgno WITH ls_message-msgv1
           ls_message-msgv2 ls_message-msgv3 ls_message-msgv4
                      INTO sy-msgli.
        message_simple space.

        IF ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_success. "'S'
          ls_mdcom-mdocm = ls_mdhdr-mdocm.
          COLLECT ls_mdcom INTO lt_mdcom.
        ENDIF.

        CLEAR : ls_msgtab, ls_message.
      ENDLOOP.

    ENDIF.
    CLEAR : ls_rows.
  ENDLOOP.

  LOOP AT <gfs_dyn_tmdm> ASSIGNING <fs_mdm>.
    DATA(lv_tabix) = sy-tabix.
    ASSIGN COMPONENT 'MDOCM' OF STRUCTURE <fs_mdm> TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      lv_mdocm = <fs_value>.
      READ TABLE lt_mdcom INTO ls_mdcom WITH KEY mdocm = lv_mdocm.
      IF sy-subrc = 0.
        DELETE <gfs_dyn_tmdm> INDEX lv_tabix.
      ENDIF.
    ENDIF.
  ENDLOOP.

  PERFORM messages_display USING gs_variables-initiator.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SELECTION_VALIDATIONS
*&---------------------------------------------------------------------*
*& SELECTION_VALIDATIONS
*&---------------------------------------------------------------------*
FORM selection_validations.

  DATA: lv_atinn TYPE atinn.

*--Validating Plant
  IF p_werks IS NOT INITIAL.
    SELECT SINGLE iwerk
      FROM t001w
      INTO @DATA(lv_plant)
     WHERE werks = @p_werks.
    IF sy-subrc <> 0.
      MESSAGE TEXT-002 TYPE zcl_abs_abap_maintain=>c_msgty_error. "'E'
    ENDIF.
  ENDIF.

**--Validating Region
*  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
*    EXPORTING
*      input  = zcl_abs_abap_maintain=>c_charact_faz_regiao
*    IMPORTING
*      output = lv_atinn.
*
*  SELECT SINGLE atinn
*    FROM cawn
*    INTO @DATA(lv_atinn_n)
*   WHERE atinn EQ @lv_atinn
*     AND atwrt EQ @p_region.
*  IF sy-subrc NE 0.
*    MESSAGE TEXT-003 TYPE zcl_abs_abap_maintain=>c_msgty_error. "'E'
*  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FCODE_PROCESSING
*&---------------------------------------------------------------------*
*& FCODE_PROCESSING
*&---------------------------------------------------------------------*
FORM fcode_processing.

  DATA : lv_subroutine(40) TYPE c VALUE 'FCODE_'.

  fcode = ok_code.
  CLEAR: ok_code.
  CONCATENATE lv_subroutine fcode INTO lv_subroutine.
  PERFORM (lv_subroutine) IN PROGRAM (sy-repid)
                          IF FOUND.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form MESSAGES_INITIALIZE
*&---------------------------------------------------------------------*
*& MESSAGES_INITIALIZE
*&---------------------------------------------------------------------*
FORM messages_initialize  USING lv_initiator TYPE /irm/gdescr
                                lv_subobject TYPE balsubobj.

  messages_init.
  messages_collect_all.
  messages_initiator_set lv_initiator c_object-log lv_subobject.

  CREATE OBJECT ref_log_handler.

  message_log_event_handler_set ref_log_handler
                                on_log_display_profile.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form  MESSAGES_CONTEXT_SET
*&---------------------------------------------------------------------*
*& MESSAGES_CONTEXT_SET
*----------------------------------------------------------------------*
FORM messages_context_set USING lv_mdocm TYPE /agri/glmdocm.

  DATA: lwa_context TYPE  zabs_str_glmd_context.

  IF lv_mdocm IS NOT INITIAL.
    lwa_context-mdocm = lv_mdocm.
    messages_context_data_set_new lv_mdocm
                                  space space
                                  zcl_abs_abap_maintain=>c_str_glmd_context "'ZABS_STR_GLMD_CONTEXT'
                                  lwa_context.
  ENDIF.

ENDFORM.                    " MESSAGES_CONTEXT_SET

*&---------------------------------------------------------------------*
*& Form PREPARE_CATALOG
*&---------------------------------------------------------------------*
FORM prepare_catalog.

  DATA: lt_fieldcat_slis TYPE slis_t_fieldcat_alv.

  REFRESH: gt_fcat.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZABS_STR_MD_MASS_APPROVALS'
    CHANGING
      ct_fieldcat            = lt_fieldcat_slis
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc EQ 0.
    CALL FUNCTION 'LVC_TRANSFER_FROM_SLIS'
      EXPORTING
        it_fieldcat_alv = lt_fieldcat_slis
      IMPORTING
        et_fieldcat_lvc = gt_fcat[]
      TABLES
        it_data         = gt_output[]
      EXCEPTIONS
        it_data_missing = 1
        OTHERS          = 2.
  ENDIF.

  LOOP AT gt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
    IF <ls_fcat>-fieldname = 'STSFL'
    OR <ls_fcat>-fieldname = 'STSMA'
    OR <ls_fcat>-fieldname = 'FLSTP'
    OR <ls_fcat>-fieldname = 'CSYEAR'
    OR <ls_fcat>-fieldname = 'TALHAO'
    OR <ls_fcat>-fieldname = 'INICIAL'
    OR <ls_fcat>-fieldname = 'ATUAL'
    OR <ls_fcat>-fieldname = 'ANTERIOR'.
      <ls_fcat>-no_out = abap_true.
    ENDIF.

    IF <ls_fcat>-fieldname = 'EST_INICIAL'
    OR <ls_fcat>-fieldname = 'EST_ATUAL'
    OR <ls_fcat>-fieldname = 'EST_ANTERIOR'
    OR <ls_fcat>-fieldname = 'VAR_ATUAL'.
      <ls_fcat>-do_sum = abap_true.
    ENDIF.
  ENDLOOP.

ENDFORM.
