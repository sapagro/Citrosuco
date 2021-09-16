************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name    : ZABS_INC_MD_MASS_APPROVALS_F01                      *
* Tcode          : ZABS_MDMA                                           *
* Created By     : Chandrakanth Karanam                                *
* Requested by   : Raphael                                             *
* Created on     : 11.06.2019                                          *
* TR             : C4DK903886                                          *
* Version        : 001                                                 *
* Description    : Provide \the list of measurement documents for mass  *
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

  REFRESH : gt_fcat, gt_mdhdr, gt_gsfstp,
            gt_gsfocm, gt_tj30, gt_sort,
            gr_stsma, gt_output, gt_copy,
            gt_aux.

* ------ Free all the ALV references ------
  IF gobj_alv IS BOUND.
    gobj_alv->free( ).
    IF gobj_cont IS BOUND.
      gobj_cont->free( ).
      FREE gobj_cont.
    ENDIF.
    FREE gobj_alv.
  ENDIF.

  CLEAR: gv_stsma, gobj_alv, gobj_cont, fcode, ok_code,
         gv_toolbar.

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
  DATA: lt_constants  TYPE zabs_tty_vkey_const,
        ltr_estat     TYPE RANGE OF j_estat,
        lt_dyn_table  TYPE REF TO data,
        lt_copy       TYPE REF TO data,
        lt_aux        TYPE REF TO data,
        lt_cabn       TYPE tty_cabn,
        lt_fname      TYPE STANDARD TABLE OF ty_fname,

*--Workarea declaration
        lsr_estat     LIKE LINE OF ltr_estat,
        ls_approvals  TYPE ly_approvals,
        ls_atgdoc     TYPE /agri/s_glatgdoc,
        ls_docto      LIKE LINE OF gt_doctos,

*--Local variable declaration
        lv_tabix      TYPE sy-tabix,
        lv_imovel     TYPE zabs_del_imovel,
        lv_ini_im_tot TYPE zabs_del_estini,
        lv_atu_im_tot TYPE zabs_del_estini,
        lv_ant_im_tot TYPE zabs_del_estini,
        lv_safra      TYPE ihrez,
        lv_packed     TYPE p DECIMALS 4,
        lv_where      TYPE string,
        lv_mdtyp      TYPE /agri/gobjtyp_bor VALUE '/AGRI/GLMD',
*-- BOC-T_T.KONNO-04.19.21
        lv_reason     TYPE atinn,
        lv_matnr      TYPE matnr.
*-- EOC-T_T.KONNO-04.19.21

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

*  INSERT INITIAL LINE INTO TABLE gr_stsma
*  ASSIGNING <ls_stsma>.
*    IF sy-subrc EQ 0.
*      <ls_stsma> = 'INE'.
*      <ls_stsma>-low = 'ZSTF_EST'.
*    ENDIF.

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

  LOOP AT gt_gsfstp ASSIGNING FIELD-SYMBOL(<lfs_gsfstp>).
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
     AND loevm  EQ @space
     AND beber1 IN @s_region[].
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
     AND mpgrp    EQ p_mpgrp
     AND canceled EQ abap_false.

  IF sy-subrc EQ 0.
    DATA(lv_langu) = sy-langu.
    SELECT j~stsma, j~estat, t~txt30
      FROM tj30 AS j
      INNER JOIN tj30t AS t
      ON j~stsma = t~stsma
      AND j~estat = t~estat
      INTO TABLE @DATA(lt_tj30)
      FOR ALL ENTRIES IN @gt_mdhdr
     WHERE j~stsma = @gt_mdhdr-stsma
       AND j~estat = @gt_mdhdr-ustat
       AND t~spras = @lv_langu.

    SORT lt_tj30 BY stsma estat.

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

    SELECT tplnr_fl, pltxt, ptrno, stort1
      FROM /agri/glflot
      INTO TABLE @DATA(lt_glflot)
      FOR ALL ENTRIES IN @gt_mdhdr
     WHERE tplnr_fl EQ @gt_mdhdr-imovel_std.

    IF p_compra IS NOT INITIAL.
      DELETE lt_glflot WHERE stort1 NE p_compra.
    ENDIF.

    IF lt_glflot[] IS NOT INITIAL.
      SORT lt_glflot BY tplnr_fl.

      SELECT stand, ktext
        FROM t499s
        INTO TABLE @DATA(lt_t499s)
        FOR ALL ENTRIES IN @lt_glflot
       WHERE werks = @p_werks
         AND stand = @lt_glflot-stort1.

      SORT lt_t499s BY stand.

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

    lv_safra = p_year.
    SELECT ebeln, bukrs, bstyp, bsart,
           loekz, aedat, telf1, unsez
      FROM ekko
      INTO TABLE @DATA(lt_ekko)
      FOR ALL ENTRIES IN @gt_mdhdr
     WHERE bukrs = @c_proposal-bukrs
       AND bstyp = @c_proposal-bstyp
       AND bsart = @c_proposal-bsart
       AND ihrez = @lv_safra
       AND telf1 = @gt_mdhdr-imovel.

*-- BOC-T_T.KONNO-04.19.21
    IF sy-subrc EQ 0.
      SELECT ebeln, ebelp, matnr
        FROM ekpo
        INTO TABLE @DATA(lt_ekpo)
        FOR ALL ENTRIES IN @lt_ekko
       WHERE ebeln = @lt_ekko-ebeln.

      SORT lt_ekpo BY ebeln matnr.
    ENDIF.
*-- EOC-T_T.KONNO-04.19.21

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

*--Building dynamic field catalog
  PERFORM dynamic_fcat_prepare USING ls_atgdoc-x-ksml
                                     lt_cabn
                            CHANGING lt_fname.

*--Calling method to generate the dynamic internal table
  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog           = gt_fcat
    IMPORTING
      ep_table                  = lt_dyn_table
    EXCEPTIONS
      generate_subpool_dir_full = 1
      OTHERS                    = 2.

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

*--Calling method to generate the dynamic internal table
  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog           = gt_fcat
    IMPORTING
      ep_table                  = lt_copy
    EXCEPTIONS
      generate_subpool_dir_full = 1
      OTHERS                    = 2.

  IF sy-subrc <> 0.
    RETURN.
  ELSE.
    ASSIGN lt_copy->* TO <gfs_tcopy>.
    CREATE DATA lt_copy LIKE LINE OF <gfs_tcopy>.
    ASSIGN lt_copy->* TO <gfs_scopy>.
  ENDIF.

*--Calling method to generate the dynamic internal table
  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog           = gt_fcat
    IMPORTING
      ep_table                  = lt_aux
    EXCEPTIONS
      generate_subpool_dir_full = 1
      OTHERS                    = 2.

  IF sy-subrc <> 0.
    RETURN.
  ELSE.
    ASSIGN lt_aux->* TO <gfs_taux>.
    CREATE DATA lt_aux LIKE LINE OF <gfs_taux>.
    ASSIGN lt_aux->* TO <gfs_saux>.
  ENDIF.

*--Assign Object to Dynamic table
  ASSIGN lt_dyn_table->* TO <gfs_dyn_tmdm>.
  CREATE DATA lt_dyn_table LIKE LINE OF <gfs_dyn_tmdm>.
  ASSIGN lt_dyn_table->* TO <gfs_dyn_smdm>.

  DATA(lv_parceiro_fo) = 'FO'.
  CALL FUNCTION 'CONVERSION_EXIT_PARVW_INPUT'
    EXPORTING
      input  = lv_parceiro_fo
    IMPORTING
      output = lv_parceiro_fo.

*  IF rb_confr EQ abap_true
*  AND rb_n_all EQ abap_true.
  IF rb_confr EQ abap_true
  AND ( rb_e002 EQ abap_true OR
        rb_e003 EQ abap_true ).

    DELETE gt_mdhdr WHERE ustat EQ c_status-approved  "'0005'
                       OR ustat EQ c_status-rejected. "'0006'

    DELETE gt_mdhdr WHERE stsma = 'ZSTF_EST'.

    DATA(lt_mdhdr) = gt_mdhdr[].
*    DELETE lt_mdhdr WHERE ustat EQ c_status-approved  "'0005'
*                       OR ustat EQ c_status-rejected. "'0006'

    IF rb_e002 EQ abap_true.
      "DELETE lt_mdhdr WHERE ustat EQ 'E0002'.
      DELETE lt_mdhdr WHERE ustat NE 'E0003'.
      DELETE gt_mdhdr WHERE ustat NE 'E0003'.
    ELSEIF rb_e003 EQ abap_true.
      "DELETE lt_mdhdr WHERE ustat EQ 'E0003'.
      DELETE lt_mdhdr WHERE ustat NE 'E0002'.
      DELETE gt_mdhdr WHERE ustat NE 'E0002'.
    ENDIF.

    SORT lt_mdhdr BY imovel.

    LOOP AT gt_mdhdr INTO DATA(ls_mdhdr).
      READ TABLE lt_mdhdr TRANSPORTING NO FIELDS
        WITH KEY imovel = ls_mdhdr-imovel BINARY SEARCH.
      IF sy-subrc NE 0.
        DELETE gt_mdhdr WHERE imovel = ls_mdhdr-imovel.
      ENDIF.
    ENDLOOP.

    IF gt_mdhdr[] IS INITIAL.
*--Não existem dados para os parâmetros informados
      "MESSAGE i061(zfmfp).
      MESSAGE i032(zfmfp) WITH TEXT-061.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

  LOOP AT gt_mdhdr ASSIGNING FIELD-SYMBOL(<fs_mdhdr>).
    ASSIGN COMPONENT 'IMODESC' OF STRUCTURE <gfs_dyn_smdm>
      TO FIELD-SYMBOL(<fs_value>).
    IF <fs_value> IS ASSIGNED.
      READ TABLE lt_glflot INTO DATA(ls_glflot)
        WITH KEY tplnr_fl = <fs_mdhdr>-imovel_std BINARY SEARCH.
      IF sy-subrc NE 0.
        CLEAR ls_glflot.
        CONTINUE.
      ENDIF.
      <fs_value> = ls_glflot-pltxt.
    ENDIF.

    READ TABLE gt_gsfstp INTO DATA(ls_gsfstp)
      WITH KEY stsma = <fs_mdhdr>-stsma
               sstat = <fs_mdhdr>-ustat BINARY SEARCH.
    IF sy-subrc <> 0.
      CLEAR ls_gsfstp.
    ENDIF.

    READ TABLE lt_stsflt INTO DATA(ls_stsflt)
      WITH KEY stsma = <fs_mdhdr>-stsma BINARY SEARCH.
    IF sy-subrc EQ 0.
      ASSIGN COMPONENT 'STSFLDESC' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
      IF sy-subrc EQ 0.
        <fs_value> = ls_stsflt-descr.
      ENDIF.
    ENDIF.

    READ TABLE lt_tj20t INTO DATA(ls_tj20t)
      WITH KEY stsma = <fs_mdhdr>-stsma BINARY SEARCH.
    IF sy-subrc EQ 0.
      ASSIGN COMPONENT 'STSMADESC' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
      IF sy-subrc EQ 0.
        <fs_value> = ls_tj20t-txt.
      ENDIF.
    ENDIF.

    READ TABLE lt_tj30 INTO DATA(ls_tj30x)
      WITH KEY stsma = <fs_mdhdr>-stsma
               estat = <fs_mdhdr>-ustat BINARY SEARCH.
    IF sy-subrc EQ 0.
      ASSIGN COMPONENT 'ETAPA_ATUAL' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
      IF sy-subrc EQ 0.
        <fs_value> = ls_tj30x-estat.
      ENDIF.

      ASSIGN COMPONENT 'ETAPA_DESC' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
      IF sy-subrc EQ 0.
        <fs_value> = ls_tj30x-txt30.
      ENDIF.
    ENDIF.

*-- BOC-T_T.KONNO-04.19.21
*    ASSIGN COMPONENT 'PPNUM' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
*    IF <fs_value> IS ASSIGNED.
*      READ TABLE lt_ekko INTO DATA(ls_ekko)
*        WITH KEY telf1 = <fs_mdhdr>-imovel BINARY SEARCH.
*      IF sy-subrc NE 0.
*        CLEAR ls_ekko.
*      ENDIF.
*      <fs_value> = ls_ekko-unsez.
*    ENDIF.
*-- EOC-T_T.KONNO-04.19.21

    ASSIGN COMPONENT 'IMOVEL' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      <fs_value> = <fs_mdhdr>-imovel.
      CLEAR ls_docto.
      ls_docto-imovel = <fs_mdhdr>-imovel.
      ls_docto-ustat = <fs_mdhdr>-ustat.
      COLLECT ls_docto INTO gt_doctos.
    ENDIF.

    ASSIGN COMPONENT 'KFRST' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      <fs_value> = <fs_mdhdr>-kfrst.
    ENDIF.

    CLEAR ls_glflot.
    ASSIGN COMPONENT 'COMPRADOR_IMOVEL' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      READ TABLE lt_glflot INTO ls_glflot
        WITH KEY tplnr_fl = <fs_mdhdr>-imovel_std BINARY SEARCH.
      IF sy-subrc NE 0.
        CLEAR ls_glflot.
      ENDIF.
      <fs_value> = ls_glflot-stort1.
    ENDIF.

    ASSIGN COMPONENT 'COMPRADOR_DESC' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      READ TABLE lt_t499s INTO DATA(ls_t499s)
        WITH KEY stand = ls_glflot-stort1 BINARY SEARCH.
      IF sy-subrc NE 0.
        CLEAR ls_t499s.
      ENDIF.
      <fs_value> = ls_t499s-ktext.
    ENDIF.

    ASSIGN COMPONENT 'LIFNR' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      DATA(lv_partner_found) = abap_false.
      READ TABLE lt_glflot INTO ls_glflot
        WITH KEY tplnr_fl = <fs_mdhdr>-imovel_std BINARY SEARCH.
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
        <fs_mdhdr>-lifnr = ls_ihpa-lifnr.
        IF s_lifnr[] IS NOT INITIAL.
          IF ls_ihpa-lifnr NOT IN s_lifnr[].
            CONTINUE.
          ENDIF.
        ENDIF.
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
      <fs_value> = <fs_mdhdr>-talhao.
    ENDIF.

    ASSIGN COMPONENT 'MDOCM' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      <fs_value> = <fs_mdhdr>-mdocm.
    ENDIF.

    UNASSIGN <fs_value>.
    ASSIGN COMPONENT 'MDTYP' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      <fs_value> = <fs_mdhdr>-mdtyp.
    ENDIF.

    UNASSIGN <fs_value>.
    ASSIGN COMPONENT 'MPGRP' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      <fs_value> = <fs_mdhdr>-mpgrp.
    ENDIF.

    UNASSIGN <fs_value>.
    ASSIGN COMPONENT 'STSFL' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      <fs_value> = ls_gsfstp-stsfl.
    ENDIF.

    UNASSIGN <fs_value>.
    ASSIGN COMPONENT 'STSMA' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      <fs_value> = ls_gsfstp-stsma.
    ENDIF.

    UNASSIGN <fs_value>.
    ASSIGN COMPONENT 'FLSTP' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      <fs_value> = ls_gsfstp-flstp.
    ENDIF.

    UNASSIGN <fs_value>.
    ASSIGN COMPONENT 'TPLNR' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      <fs_value> = <fs_mdhdr>-tplnr_fl.
    ENDIF.

    CLEAR lv_matnr.
    READ TABLE lt_glflcma INTO DATA(ls_glflcma)
      WITH KEY tplnr_fl = <fs_mdhdr>-tplnr_fl
               contr    = <fs_mdhdr>-contr BINARY SEARCH.
    IF sy-subrc EQ 0.
      UNASSIGN <fs_value>.
      ASSIGN COMPONENT 'YMATNR' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
      IF <fs_value> IS ASSIGNED.
        lv_matnr = <fs_value> = ls_glflcma-ymatnr.
        <fs_mdhdr>-ymatnr = ls_glflcma-ymatnr.
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

*-- BOC-T_T.KONNO-04.19.21
      ASSIGN COMPONENT 'PPNUM' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
      IF <fs_value> IS ASSIGNED.
        READ TABLE lt_ekko INTO DATA(ls_ekko)
          WITH KEY telf1 = <fs_mdhdr>-imovel BINARY SEARCH.
        IF sy-subrc NE 0.
          CLEAR ls_ekko.
        ENDIF.
        <fs_value> = ls_ekko-unsez.
        IF ls_ekko IS NOT INITIAL
        AND lv_matnr IS NOT INITIAL.
          READ TABLE lt_ekpo INTO DATA(ls_ekpo)
            WITH KEY ebeln = ls_ekko-ebeln
                     matnr = lv_matnr BINARY SEARCH.
          IF sy-subrc NE 0.
            CLEAR <fs_value>.
          ENDIF.
        ENDIF.
      ENDIF.
*-- EOC-T_T.KONNO-04.19.21

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
      WITH KEY mdocm = <fs_mdhdr>-mdocm BINARY SEARCH.

    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    lv_tabix = sy-tabix.

    DATA(lv_save) = abap_true.
    LOOP AT lt_mdatv INTO DATA(ls_mdatv) FROM lv_tabix.
      IF ls_mdatv-mdocm <> <fs_mdhdr>-mdocm.
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

        <fs_mdhdr>-safra = <gfs_value>.

*-- BOC-T_T.KONNO-04.19.21
*        IF ls_fname-fieldname EQ 'MOT_EST'.
        IF ls_fname-fieldname EQ 'SAFRA'.
          IF <gfs_value> NE p_year.
            lv_save = abap_false.
          ENDIF.
        ELSEIF ls_fname-fieldname EQ 'MOT_EST'.
*-- EOC-T_T.KONNO-04.19.21
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
*-- BOC-T_T.KONNO-04.19.21
    IF lv_save EQ abap_true.
      APPEND <gfs_dyn_smdm> TO <gfs_dyn_tmdm>.
    ENDIF.
*-- EOC-T_T.KONNO-04.19.21
    CLEAR <gfs_dyn_smdm>.
  ENDLOOP.

  DELETE gt_mdhdr WHERE safra NE p_year.
  IF gt_mdhdr[] IS INITIAL.
*--Não existem dados para os parâmetros informados
    MESSAGE i032(zfmfp) WITH TEXT-061.
*    MESSAGE i061(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

  SORT gt_doctos BY imovel ustat.

  <gfs_taux>[] = <gfs_tcopy>[] = <gfs_dyn_tmdm>[].

  SORT <gfs_dyn_tmdm> BY (c_sort-imovel)  ASCENDING
                         (c_sort-talhao)  ASCENDING
                         (c_sort-florada) ASCENDING
                         (c_sort-docto)   ASCENDING.

  SORT <gfs_tcopy> BY (c_sort-imovel)  ASCENDING
                      (c_sort-talhao)  ASCENDING
                      (c_sort-florada) ASCENDING
                      (c_sort-docto)   DESCENDING.

  lv_where = '''KFRST'' <> abap_false'.
  DELETE <gfs_taux>  WHERE (lv_where).

  SORT <gfs_taux> BY (c_sort-imovel)  ASCENDING
                     (c_sort-talhao)  ASCENDING
                     (c_sort-florada) ASCENDING
                     (c_sort-docto)   DESCENDING.

  CLEAR ls_approvals.
  LOOP AT <gfs_dyn_tmdm> ASSIGNING <gfs_dyn_smdm>.
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

*-- Checks whether it is the last document
      DATA(lv_lastdoc_found) = abap_false.
      READ TABLE <gfs_tcopy> ASSIGNING <gfs_scopy>
        WITH KEY (c_sort-imovel)  = <fs_imovel>
                 (c_sort-talhao)  = <fs_talhao>
                 (c_sort-florada) = <fs_florada> BINARY SEARCH.
      IF sy-subrc EQ 0.
        DATA(lv_index) = sy-tabix.
        ASSIGN COMPONENT c_sort-status_sf OF STRUCTURE <gfs_scopy> TO FIELD-SYMBOL(<fs_ustat>).
        IF sy-subrc EQ 0.
          IF <fs_ustat> EQ c_status-rejected.               "'E0006'
            lv_lastdoc_found = abap_false.
            LOOP AT <gfs_tcopy> ASSIGNING <gfs_scopy> FROM lv_index.
              ASSIGN COMPONENT:
                c_sort-imovel    OF STRUCTURE <gfs_scopy> TO FIELD-SYMBOL(<fs_imovel_x>),
                c_sort-talhao    OF STRUCTURE <gfs_scopy> TO FIELD-SYMBOL(<fs_talhao_x>),
                c_sort-florada   OF STRUCTURE <gfs_scopy> TO FIELD-SYMBOL(<fs_florada_x>),
                c_sort-status_sf OF STRUCTURE <gfs_scopy> TO FIELD-SYMBOL(<fs_ustat_x>).
              IF <fs_imovel_x>   IS ASSIGNED AND <fs_imovel_x>  EQ <fs_imovel>
              AND <fs_talhao_x>  IS ASSIGNED AND <fs_talhao_x>  EQ <fs_talhao>
              AND <fs_florada_x> IS ASSIGNED AND <fs_florada_x> EQ <fs_florada>.
                IF <fs_ustat_x>  IS ASSIGNED AND <fs_ustat_x>   NE c_status-rejected. "'E0006'
                  lv_lastdoc_found = abap_true.
                  EXIT.
                ENDIF.
              ELSE.
                EXIT.
              ENDIF.
            ENDLOOP.
          ELSE.
            lv_lastdoc_found = abap_true.
          ENDIF.
          IF lv_lastdoc_found = abap_true.
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
        ENDIF.
      ENDIF.

*-- Released
      IF <fs_kfrst> EQ abap_false.
        READ TABLE <gfs_taux> ASSIGNING <gfs_saux>
          WITH KEY (c_sort-imovel)  = <fs_imovel>
                   (c_sort-talhao)  = <fs_talhao>
                   (c_sort-florada) = <fs_florada> BINARY SEARCH.
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

    ASSIGN COMPONENT 'ETAPA_ATUAL' OF STRUCTURE <gfs_dyn_smdm> TO <fs_ustat>.
    IF sy-subrc EQ 0
    AND <fs_ustat> NE c_status-rejected.                    "'E0006'
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
    ENDIF.

    IF p_year IS NOT INITIAL.
      ASSIGN COMPONENT 'SAFRA' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
      IF sy-subrc EQ 0.
        IF <fs_value>(4) NE p_year.
          DELETE <gfs_dyn_tmdm> INDEX lv_tabix.
        ENDIF.
      ENDIF.
    ENDIF.

    IF <fs_imovel> IS ASSIGNED
    AND <fs_talhao> IS ASSIGNED
    AND <fs_florada> IS ASSIGNED.
      ls_approvals-imovel  = <fs_imovel>.
      ls_approvals-talhao  = <fs_talhao>.
      ls_approvals-florada = <fs_florada>.
    ENDIF.
  ENDLOOP.

  IF <gfs_dyn_tmdm> IS INITIAL.
    MESSAGE i030(zabs_msgcls).
    LEAVE LIST-PROCESSING.
  ELSE.
    LOOP AT <gfs_dyn_tmdm> ASSIGNING <gfs_dyn_smdm>.
      DATA(lv_tabix_x) = sy-tabix.
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

      IF rb_confr EQ abap_true.
        ASSIGN COMPONENT 'PPNUM' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
        IF sy-subrc EQ 0.
          IF <fs_value> IS INITIAL.
            DELETE <gfs_dyn_tmdm> INDEX lv_tabix_x.
          ENDIF.
        ENDIF.
      ELSEIF rb_detfr EQ abap_true.
        IF p_disp EQ abap_false.
          ASSIGN COMPONENT 'PPNUM' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
          IF sy-subrc EQ 0.
            IF <fs_value> IS INITIAL.
              DELETE <gfs_dyn_tmdm> INDEX lv_tabix_x.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

*-- Consolidado
  IF rb_confr EQ abap_true.
    SORT <gfs_dyn_tmdm> BY (c_sort-forn_desc) ASCENDING
                           (c_sort-imovel)    ASCENDING
                           (c_sort-material)  ASCENDING.
*-- Detalhado
  ELSEIF rb_detfr EQ abap_true.
    SORT <gfs_dyn_tmdm> BY (c_sort-fornecedor) ASCENDING
                           (c_sort-imovel)     ASCENDING
                           (c_sort-material)   ASCENDING.
  ENDIF.

  <gfs_tcopy>[] = <gfs_dyn_tmdm>[].

  REFRESH: <gfs_taux>.
  CLEAR lv_tabix.
  DATA(lv_lines) = lines( <gfs_dyn_tmdm> ).
  LOOP AT <gfs_dyn_tmdm> ASSIGNING <gfs_dyn_smdm>.
    lv_tabix = sy-tabix.
    ASSIGN COMPONENT: 'LIFNR'        OF STRUCTURE <gfs_dyn_smdm> TO FIELD-SYMBOL(<fs_from1>),
                      'IMOVEL'       OF STRUCTURE <gfs_dyn_smdm> TO FIELD-SYMBOL(<fs_from2>),
                      'YMATNR'       OF STRUCTURE <gfs_dyn_smdm> TO FIELD-SYMBOL(<fs_from3>),
                      'EST_INICIAL'  OF STRUCTURE <gfs_dyn_smdm> TO FIELD-SYMBOL(<fs_from4>),
                      'EST_ATUAL'    OF STRUCTURE <gfs_dyn_smdm> TO FIELD-SYMBOL(<fs_from5>),
                      'EST_ANTERIOR' OF STRUCTURE <gfs_dyn_smdm> TO FIELD-SYMBOL(<fs_from6>),
                      'PPNUM'        OF STRUCTURE <gfs_dyn_smdm> TO FIELD-SYMBOL(<fs_from7>).
    IF <fs_from1> IS NOT ASSIGNED
    OR <fs_from2> IS NOT ASSIGNED
    OR <fs_from3> IS NOT ASSIGNED
    OR <fs_from4> IS NOT ASSIGNED
    OR <fs_from5> IS NOT ASSIGNED
    OR <fs_from6> IS NOT ASSIGNED
    OR <fs_from7> IS NOT ASSIGNED.
      EXIT.
    ELSE.
      DATA(lv_ok) = abap_false.

      IF rb_confr EQ abap_true.
        IF  lv_imovel IS NOT INITIAL
        AND lv_imovel NE <fs_from2>.
          INSERT INITIAL LINE INTO TABLE <gfs_taux> ASSIGNING FIELD-SYMBOL(<fs_total>).
          IF sy-subrc EQ 0.
            ASSIGN COMPONENT 'LINE_COLOR' OF STRUCTURE <fs_total> TO <fs_to>.
            IF sy-subrc EQ 0.
              <fs_to> = 'C310'.
            ENDIF.

            ASSIGN COMPONENT 'TOTAL' OF STRUCTURE <fs_total> TO <fs_to>.
            IF sy-subrc EQ 0.
              <fs_to> = abap_true.
            ENDIF.

            ASSIGN COMPONENT 'IMOVEL' OF STRUCTURE <fs_total> TO <fs_to>.
            IF sy-subrc EQ 0.
              <fs_to> = lv_imovel.
            ENDIF.

            ASSIGN COMPONENT 'IMODESC' OF STRUCTURE <fs_total> TO <fs_to>.
            IF sy-subrc EQ 0.
              ASSIGN COMPONENT 'IMODESC' OF STRUCTURE <gfs_saux> TO <fs_from>.
              IF sy-subrc EQ 0.
                <fs_to> = <fs_from>.
              ENDIF.
            ENDIF.

            ASSIGN COMPONENT: 'EST_INICIAL'  OF STRUCTURE <fs_total> TO FIELD-SYMBOL(<fs_to4>),
                              'EST_ATUAL'    OF STRUCTURE <fs_total> TO FIELD-SYMBOL(<fs_to5>),
                              'EST_ANTERIOR' OF STRUCTURE <fs_total> TO FIELD-SYMBOL(<fs_to6>).
            IF <fs_to4> IS ASSIGNED
            AND <fs_to5> IS ASSIGNED
            AND <fs_to6> IS ASSIGNED.
              <fs_to4> = lv_ini_im_tot.
              <fs_to5> = lv_atu_im_tot.
              <fs_to6> = lv_ant_im_tot.
            ENDIF.
          ENDIF.

          lv_ini_im_tot = <fs_from4>.
          lv_atu_im_tot = <fs_from5>.
          lv_ant_im_tot = <fs_from6>.
        ELSE.
          lv_ini_im_tot = lv_ini_im_tot + <fs_from4>.
          lv_atu_im_tot = lv_atu_im_tot + <fs_from5>.
          lv_ant_im_tot = lv_ant_im_tot + <fs_from6>.
        ENDIF.
      ENDIF.

      READ TABLE <gfs_taux> ASSIGNING <gfs_saux>
        WITH KEY (c_sort-fornecedor) = <fs_from1>
                 (c_sort-imovel)     = <fs_from2>
                 (c_sort-material)   = <fs_from3> BINARY SEARCH.
      IF sy-subrc NE 0.
        INSERT INITIAL LINE INTO TABLE <gfs_taux> ASSIGNING <gfs_saux>.
        IF sy-subrc EQ 0.
          lv_ok = abap_true.

          "<gfs_saux> = CORRESPONDING #( <gfs_dyn_smdm> ).

          ASSIGN COMPONENT 'LIFNR' OF STRUCTURE <gfs_saux> TO <fs_to>.
          IF sy-subrc EQ 0.
            <fs_to> = <fs_from1>.
          ENDIF.

          ASSIGN COMPONENT 'IMOVEL' OF STRUCTURE <gfs_saux> TO <fs_to>.
          IF sy-subrc EQ 0.
            <fs_to> = <fs_from2>.
          ENDIF.

          ASSIGN COMPONENT 'YMATNR' OF STRUCTURE <gfs_saux> TO <fs_to>.
          IF sy-subrc EQ 0.
            <fs_to> = <fs_from3>.
          ENDIF.

          ASSIGN COMPONENT 'LIFNRDESC' OF STRUCTURE <gfs_dyn_smdm> TO <fs_from>.
          IF sy-subrc EQ 0.
            ASSIGN COMPONENT 'LIFNRDESC' OF STRUCTURE <gfs_saux> TO <fs_to>.
            IF sy-subrc EQ 0.
              <fs_to> = <fs_from>.
            ENDIF.
          ENDIF.

          ASSIGN COMPONENT 'IMODESC' OF STRUCTURE <gfs_dyn_smdm> TO <fs_from>.
          IF sy-subrc EQ 0.
            ASSIGN COMPONENT 'IMODESC' OF STRUCTURE <gfs_saux> TO <fs_to>.
            IF sy-subrc EQ 0.
              <fs_to> = <fs_from>.
            ENDIF.
          ENDIF.

          ASSIGN COMPONENT 'MAKTX' OF STRUCTURE <gfs_dyn_smdm> TO <fs_from>.
          IF sy-subrc EQ 0.
            ASSIGN COMPONENT 'MAKTX' OF STRUCTURE <gfs_saux> TO <fs_to>.
            IF sy-subrc EQ 0.
              <fs_to> = <fs_from>.
            ENDIF.
          ENDIF.

          ASSIGN COMPONENT 'PPNUM' OF STRUCTURE <gfs_dyn_smdm> TO <fs_from>.
          IF sy-subrc EQ 0.
            ASSIGN COMPONENT 'PPNUM' OF STRUCTURE <gfs_saux> TO <fs_to>.
            IF sy-subrc EQ 0.
              <fs_to> = <fs_from>.
            ENDIF.
          ENDIF.

          ASSIGN COMPONENT 'COMPRADOR_IMOVEL' OF STRUCTURE <gfs_dyn_smdm> TO <fs_from>.
          IF sy-subrc EQ 0.
            ASSIGN COMPONENT 'COMPRADOR_IMOVEL' OF STRUCTURE <gfs_saux> TO <fs_to>.
            IF sy-subrc EQ 0.
              <fs_to> = <fs_from>.
            ENDIF.
          ENDIF.

          ASSIGN COMPONENT 'COMPRADOR_DESC' OF STRUCTURE <gfs_dyn_smdm> TO <fs_from>.
          IF sy-subrc EQ 0.
            ASSIGN COMPONENT 'COMPRADOR_DESC' OF STRUCTURE <gfs_saux> TO <fs_to>.
            IF sy-subrc EQ 0.
              <fs_to> = <fs_from>.
            ENDIF.
          ENDIF.
        ENDIF.

      ELSE.
        lv_ok = abap_true.
      ENDIF.
      IF lv_ok = abap_true.
        ASSIGN COMPONENT: 'EST_INICIAL'  OF STRUCTURE <gfs_saux> TO <fs_to4>,
                          'EST_ATUAL'    OF STRUCTURE <gfs_saux> TO <fs_to5>,
                          'EST_ANTERIOR' OF STRUCTURE <gfs_saux> TO <fs_to6>.
        IF <fs_to4> IS ASSIGNED
        AND <fs_to5> IS ASSIGNED
        AND <fs_to6> IS ASSIGNED.
          <fs_to4> = <fs_to4> + <fs_from4>.
          <fs_to5> = <fs_to5> + <fs_from5>.
          <fs_to6> = <fs_to6> + <fs_from6>.
        ENDIF.
      ENDIF.

      lv_imovel = <fs_from2>.
    ENDIF.
  ENDLOOP.

  IF  rb_confr EQ abap_true
  AND lv_tabix EQ lv_lines
  AND lv_tabix IS NOT INITIAL
  AND <gfs_saux> IS ASSIGNED.
    INSERT INITIAL LINE INTO TABLE <gfs_taux> ASSIGNING <fs_total>.
    IF sy-subrc EQ 0.
      ASSIGN COMPONENT 'LINE_COLOR' OF STRUCTURE <fs_total> TO <fs_to>.
      IF sy-subrc EQ 0.
        <fs_to> = 'C310'.
      ENDIF.

      ASSIGN COMPONENT 'TOTAL' OF STRUCTURE <fs_total> TO <fs_to>.
      IF sy-subrc EQ 0.
        <fs_to> = abap_true.
      ENDIF.

      ASSIGN COMPONENT 'IMOVEL' OF STRUCTURE <fs_total> TO <fs_to>.
      IF sy-subrc EQ 0.
        <fs_to> = lv_imovel.
      ENDIF.

      ASSIGN COMPONENT 'IMODESC' OF STRUCTURE <fs_total> TO <fs_to>.
      IF sy-subrc EQ 0.
        ASSIGN COMPONENT 'IMODESC' OF STRUCTURE <gfs_saux> TO <fs_from>.
        IF sy-subrc EQ 0.
          <fs_to> = <fs_from>.
        ENDIF.
      ENDIF.

      ASSIGN COMPONENT: 'EST_INICIAL'  OF STRUCTURE <fs_total> TO <fs_to4>,
                        'EST_ATUAL'    OF STRUCTURE <fs_total> TO <fs_to5>,
                        'EST_ANTERIOR' OF STRUCTURE <fs_total> TO <fs_to6>.
      IF <fs_to4> IS ASSIGNED
      AND <fs_to5> IS ASSIGNED
      AND <fs_to6> IS ASSIGNED.
        <fs_to4> = lv_ini_im_tot.
        <fs_to5> = lv_atu_im_tot.
        <fs_to6> = lv_ant_im_tot.
      ENDIF.
    ENDIF.

    LOOP AT <gfs_taux> ASSIGNING <gfs_saux>.
      ASSIGN COMPONENT: 'EST_ANTERIOR' OF STRUCTURE <gfs_saux> TO <fs_anterior>,
                        'EST_ATUAL'    OF STRUCTURE <gfs_saux> TO <fs_atual>,
                        'EST_INICIAL'  OF STRUCTURE <gfs_saux> TO <fs_inicial>.

      IF <fs_anterior> IS NOT ASSIGNED
      OR <fs_atual>    IS NOT ASSIGNED
      OR <fs_inicial>  IS NOT ASSIGNED.
        EXIT.
      ENDIF.

*-- Variação Anterior % = [(Est.Anterior - Est.Inicial) / Est.Inicial ] * 100
      ASSIGN COMPONENT 'VAR_PERC_ANTERIOR' OF STRUCTURE <gfs_saux> TO <fs_value>.
      IF sy-subrc EQ 0.
        IF <fs_inicial> IS NOT INITIAL.
          <fs_value> = ( ( <fs_anterior> - <fs_inicial> ) / <fs_inicial> ) * 100.
        ENDIF.
      ENDIF.

*-- Variação Anterior Qtd. = Est.Anterior - Est.Inicial
      ASSIGN COMPONENT 'VAR_QTD_ANTERIOR' OF STRUCTURE <gfs_saux> TO <fs_value>.
      IF sy-subrc EQ 0.
        IF <fs_inicial> IS NOT INITIAL.
          <fs_value> = <fs_anterior> - <fs_inicial>.
        ENDIF.
      ENDIF.

**-- Variação Atual % = [(Est.Atual - Est.Inicial) / Est.Inicial ] * 100
*      ASSIGN COMPONENT 'VAR_PERC_ATUAL' OF STRUCTURE <gfs_saux> TO <fs_value>.
*      IF sy-subrc EQ 0.
*        IF <fs_inicial> IS NOT INITIAL.
*          <fs_value> = ( ( <fs_atual> - <fs_inicial> ) / <fs_inicial> ) * 100.
*        ENDIF.
*      ENDIF.
*-- Variação Atual % = [(Est.Atual - Est.Anterior) / Est.Anterior ] * 100
      ASSIGN COMPONENT 'VAR_PERC_ATUAL' OF STRUCTURE <gfs_saux> TO <fs_value>.
      IF sy-subrc EQ 0.
        IF <fs_anterior> IS NOT INITIAL.
          <fs_value> = ( ( <fs_atual> - <fs_anterior> ) / <fs_anterior> ) * 100.
        ENDIF.
      ENDIF.

**-- Variação Atual Qtd. = Est.Atual - Est.Inicial
*      ASSIGN COMPONENT 'VAR_QTD_ATUAL' OF STRUCTURE <gfs_saux> TO <fs_value>.
*      IF sy-subrc EQ 0.
*        IF <fs_inicial> IS NOT INITIAL.
*          <fs_value> = <fs_atual> - <fs_inicial>.
*        ENDIF.
*      ENDIF.
*-- Variação Atual Qtd. = Est.Atual - Est.Anterior
      ASSIGN COMPONENT 'VAR_QTD_ATUAL' OF STRUCTURE <gfs_saux> TO <fs_value>.
      IF sy-subrc EQ 0.
        <fs_value> = <fs_atual> - <fs_anterior>.
      ENDIF.
    ENDLOOP.

*    SORT <gfs_taux> BY (c_sort-fornecedor) ASCENDING
*                       (c_sort-imovel)     ASCENDING
*                       (c_sort-material)   ASCENDING.

    <gfs_dyn_tmdm>[] = <gfs_taux>[].
  ENDIF.

*-- Detalhado: <gfs_tcopy>[]
*-- Consolidado: <gfs_dyn_tmdm>[]
  IF  p_alcada EQ abap_true.
    LOOP AT gt_doctos ASSIGNING FIELD-SYMBOL(<fs_docto>).
      READ TABLE <gfs_dyn_tmdm> ASSIGNING <gfs_dyn_smdm>
        WITH KEY ('IMOVEL') = <fs_docto>-imovel
                 ('TOTAL')  = abap_true.
      IF sy-subrc EQ 0.
        ASSIGN COMPONENT 'EST_INICIAL' OF STRUCTURE <gfs_dyn_smdm>
          TO FIELD-SYMBOL(<fs_est_ini>).
        IF sy-subrc EQ 0.
          <fs_docto>-est_inicial = <fs_est_ini>.
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
*     i_structure_name       = zcl_abs_abap_maintain=>c_str_md_mass_approvals "'ZABS_STR_MD_MASS_APPROVALS'
      i_structure_name       = 'ZABS_STR_MDM_MASS_APPROVALS'
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

  LOOP AT gt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
    IF <ls_fcat>-fieldname = 'STSFL'
    OR <ls_fcat>-fieldname = 'STSMA'
    OR <ls_fcat>-fieldname = 'FLSTP'
    OR <ls_fcat>-fieldname = 'CSYEAR'
    OR <ls_fcat>-fieldname = 'TALHAO'
    OR <ls_fcat>-fieldname = 'INICIAL'
    OR <ls_fcat>-fieldname = 'ATUAL'
    OR <ls_fcat>-fieldname = 'ANTERIOR'
    OR <ls_fcat>-fieldname = 'LINE_COLOR'
    OR <ls_fcat>-fieldname = 'TOTAL'.
      <ls_fcat>-no_out = abap_true.
    ENDIF.

*    IF rb_confx EQ abap_true.
*      IF <ls_fcat>-fieldname = 'EST_INICIAL'
*      OR <ls_fcat>-fieldname = 'EST_ATUAL'
*      OR <ls_fcat>-fieldname = 'EST_ANTERIOR'.
*        <ls_fcat>-do_sum = abap_true.
*      ENDIF.
*    ENDIF.
  ENDLOOP.

  LOOP AT pt_ksml INTO DATA(ls_ksml).

    DESCRIBE TABLE gt_fcat LINES lv_col_pos.

*--Fill Internal Table for Linking Attributes and Dynamic Columns
    READ TABLE pt_cabn INTO DATA(ls_cabn)
      WITH KEY atinn = ls_ksml-imerk BINARY SEARCH.

    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

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

  IF gv_toolbar EQ abap_false.
    SET PF-STATUS 'S200'.
  ELSE.
    SET PF-STATUS 'S100'.
  ENDIF.

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
  DATA: lt_toolbar_exc TYPE ui_functions,
        ls_layout      TYPE lvc_s_layo,
        ls_variant     TYPE disvariant.

  CALL FUNCTION 'STATUS_BUFFER_REFRESH'.

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
*--Field that identify color line in internal table
  ls_layout-info_fname = 'LINE_COLOR'.

  IF p_layout EQ abap_true.
    ls_variant-report = sy-repid.
    ls_variant-handle = 'GRID'.
  ENDIF.

*-- Fornecedor/Imóvel/Material
*  IF rb_confx EQ abap_true.
*    DO 3 TIMES.
*      DATA(lv_index) = sy-index.
*      INSERT INITIAL LINE INTO TABLE gt_sort
*        ASSIGNING FIELD-SYMBOL(<ls_sort>).
*      IF sy-subrc EQ 0.
*        <ls_sort>-spos   = lv_index.
*        <ls_sort>-up     = abap_true.
*        <ls_sort>-subtot = abap_true.
*        CASE lv_index.
*          WHEN 1.
*            DATA(lv_fieldname) = 'LIFNRDESC'.
*          WHEN 2.
*            lv_fieldname = 'IMODESC'.
*          WHEN 3.
*            lv_fieldname = 'MAKTX'.
*        ENDCASE.
*        <ls_sort>-fieldname = lv_fieldname.
*      ENDIF.
*    ENDDO.
*  ENDIF.

*--Displaying ALV Data
  IF gobj_alv IS NOT INITIAL
  AND <gfs_dyn_tmdm> IS ASSIGNED.
    IF rb_detfr EQ abap_true.
      PERFORM toolbar_excludes_prepare TABLES lt_toolbar_exc.
    ENDIF.

    CALL METHOD gobj_alv->set_table_for_first_display
      EXPORTING
        is_variant                    = ls_variant
        i_save                        = 'A'
        i_default                     = abap_true
        is_layout                     = ls_layout
        it_toolbar_excluding          = lt_toolbar_exc
      CHANGING
        it_outtab                     = <gfs_dyn_tmdm>
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

* ------ Free all the ALV references ------
  IF gobj_alv IS BOUND.
    gobj_alv->free( ).
    IF gobj_cont IS BOUND.
      gobj_cont->free( ).
      FREE gobj_cont.
    ENDIF.
    FREE gobj_alv.
  ENDIF.

  CLEAR: gobj_alv, gobj_cont.

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

  SORT gt_mdhdr BY lifnr imovel ymatnr.

  LOOP AT lt_index_rows INTO ls_rows.
    READ TABLE <gfs_dyn_tmdm> ASSIGNING FIELD-SYMBOL(<fs_mdm>)
     INDEX ls_rows-index.
    IF sy-subrc <> 0.
      CONTINUE.
    ELSE.
      ASSIGN COMPONENT c_sort-total OF STRUCTURE <fs_mdm>
        TO FIELD-SYMBOL(<fs_total>).
      IF sy-subrc EQ 0.
        IF <fs_total> EQ abap_true.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDIF.

    ASSIGN COMPONENT:
      c_sort-fornecedor OF STRUCTURE <fs_mdm> TO FIELD-SYMBOL(<fs_fornecedor1>),
      c_sort-imovel     OF STRUCTURE <fs_mdm> TO FIELD-SYMBOL(<fs_imovel1>),
      c_sort-material   OF STRUCTURE <fs_mdm> TO FIELD-SYMBOL(<fs_material1>).

    IF <fs_fornecedor1> IS NOT ASSIGNED
    OR <fs_imovel1> IS NOT ASSIGNED
    OR <fs_material1> IS NOT ASSIGNED.
      EXIT.
    ELSE.
      READ TABLE gt_mdhdr INTO DATA(ls_mdhdr)
        WITH KEY lifnr  = <fs_fornecedor1>
                 imovel = <fs_imovel1>
                 ymatnr = <fs_material1> BINARY SEARCH.

      WHILE sy-subrc EQ 0.
        DATA(lv_tabix) = sy-tabix + 1.
        IF ls_mdhdr-stsma IS NOT INITIAL
        AND ls_mdhdr-ustat IS NOT INITIAL
        AND ls_mdhdr-kfrst EQ 'A'.
          READ TABLE gt_gsfstp ASSIGNING FIELD-SYMBOL(<lfs_gsfstp>)
            WITH KEY stsma = ls_mdhdr-stsma
                     sstat = ls_mdhdr-ustat BINARY SEARCH.
          IF sy-subrc EQ 0.
            READ TABLE gt_gsfocm ASSIGNING FIELD-SYMBOL(<lfs_gsfocm>)
             WITH KEY stsma = <lfs_gsfstp>-stsma
                      stsfl = <lfs_gsfstp>-stsfl
                      flstp = <lfs_gsfstp>-flstp
                      soutc = pv_soutc BINARY SEARCH.
            IF sy-subrc EQ 0.
              CLEAR : ls_outcome.
              REFRESH : lt_message[].
              ls_outcome-stsfl  = <lfs_gsfstp>-stsfl.
*-- BOC-T_M.ALFREDO
*      ls_outcome-stsma  = gv_stsma. " gw_gsfstp-stsma
              ls_outcome-stsma  = ls_mdhdr-stsma.
*-- EOC-T_M.ALFREDO
              ls_outcome-flstp  = <lfs_gsfstp>-flstp.
              ls_outcome-soutc  = pv_soutc.

              PERFORM messages_context_set USING ls_mdhdr-mdocm."lv_posnr.

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

              LOOP AT lt_message INTO ls_message.
                ls_message-context-context-tabname = zcl_abs_abap_maintain=>c_str_glmd_context. "'ZABS_STR_GLMD_CONTEXT'
                ls_message-context-document = ls_mdhdr-mdocm.
                ls_message-context-context-value = ls_mdhdr-mdocm.
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
          ENDIF.
        ENDIF.
        READ TABLE gt_mdhdr INTO ls_mdhdr INDEX lv_tabix
          COMPARING lifnr imovel ymatnr.
      ENDWHILE.
    ENDIF.
    CLEAR : ls_rows.
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

  IF rb_detfr EQ abap_true.
    p_layout = abap_true.
    CLEAR p_alcada.
  ELSEIF rb_confr EQ abap_true.
    p_layout = abap_false.
  ENDIF.

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
*& Form CREATE_BUTTON
*&---------------------------------------------------------------------*
FORM create_button.

  DATA: ls_button TYPE smp_dyntxt.

*  CLEAR gv_pressed.
  ls_button-text      = 'Aplicar Alçadas'.
  ls_button-icon_id   = icon_set_state.
  ls_button-icon_text = 'Aplicar Alçadas de Aprovação'.
  ls_button-quickinfo = 'Aplicar Alçadas de Aprovação'.
  sscrfields-functxt_01 = ls_button.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form UPDATE_SCREEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM update_screen.

  LOOP AT SCREEN.
    IF screen-group1 = 'T4'.
      screen-invisible = 1.
      screen-input     = 0.
      screen-active    = 0.
      MODIFY SCREEN.
    ENDIF.

    IF rb_detfr EQ abap_true.
      IF screen-group1 = 'T3'
      OR screen-group1 = 'T5'.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
        MODIFY SCREEN.
      ELSEIF screen-group1 = 'T6'.
        screen-invisible = 0.
        screen-input     = 1.
        screen-active    = 1.
        MODIFY SCREEN.
      ENDIF.
    ELSEIF rb_confr EQ abap_true.
      IF screen-group1 = 'T3'
      OR screen-group1 = 'T5'.
        screen-invisible = 0.
        screen-input     = 1.
        screen-active    = 1.
        MODIFY SCREEN.
      ELSEIF screen-group1 = 'T6'.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form VALIDATE_PARAMETERS
*&---------------------------------------------------------------------*
FORM validate_parameters.

  IF p_mdtyp IS INITIAL.
*-- Informar Tipo de documento de medição
    MESSAGE i063(zfmfp).
    LEAVE LIST-PROCESSING.
  ELSE.
    SELECT SINGLE mdtyp
      INTO @DATA(lv_mdtyp)
      FROM /agri/tglmdtyp
     WHERE mdtyp EQ @p_mdtyp.

    IF sy-subrc NE 0.
*-- Documento de medição &1 inexistente
      MESSAGE i066(zfmfp) WITH p_mdtyp.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

  IF p_mpgrp IS INITIAL.
*-- Informar Grupo de medições
    MESSAGE i064(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF p_werks IS INITIAL.
*-- Informar Centro
    MESSAGE i065(zfmfp).
    LEAVE LIST-PROCESSING.
  ELSE.
    SELECT SINGLE iwerk
      FROM t001w
      INTO @DATA(lv_plant)
     WHERE werks = @p_werks.

    IF sy-subrc NE 0.
*-- Centro &1 inexistente
      MESSAGE i068(zfmfp) WITH p_werks.
      LEAVE LIST-PROCESSING.
    ELSE.
      IF p_compra IS NOT INITIAL.
        SELECT SINGLE werks, stand
          FROM t499s
          INTO @DATA(ls_499s)
         WHERE werks EQ @p_werks
           AND stand EQ @p_compra.

        IF sy-subrc NE 0.
*-- Comprador &1 inexistente no Centro &2.
          MESSAGE i069(zfmfp) WITH p_compra p_werks.
          LEAVE LIST-PROCESSING.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form UPDATE_STATUS_FLOW
*&---------------------------------------------------------------------*
FORM update_status_flow .

  DATA: lt_mdocm    TYPE /agri/t_glmd_doc,
        ls_mdocm    LIKE LINE OF lt_mdocm,
        lt_bapiret2 TYPE STANDARD TABLE OF bapiret2 INITIAL SIZE 0,
        lt_messages TYPE /agri/t_gprolog,
        ls_message  LIKE LINE OF lt_messages,
        lt_vtx_msg  TYPE zabs_tty_messages,
        ls_vtx_msg  LIKE LINE OF lt_vtx_msg,
        lt_mdhdr    TYPE /agri/t_glmdhdr,
        lt_mditm    TYPE /agri/t_glmditm,
        lt_mdatv    TYPE /agri/t_glmdatv,
        lv_var_cx   TYPE zabs_del_cx_ate,
        lv_subrc    TYPE sysubrc.

****Update Indicators
  CONSTANTS: c_updkz_new(1)     TYPE c VALUE 'I',
             c_updkz_update(1)  TYPE c VALUE 'U',
             c_updkz_delete(1)  TYPE c VALUE 'D',
             c_updkz_old(1)     TYPE c VALUE ' ',
             c_updkz_newrow     TYPE c VALUE 'N',
             c_updkz_propose(1) TYPE c VALUE 'P'.

****Message Types
  CONSTANTS : BEGIN OF c_msg_type,
                info    LIKE sy-msgty VALUE 'I',
                warning LIKE sy-msgty VALUE 'W',
                error   LIKE sy-msgty VALUE 'E',
                abend   LIKE sy-msgty VALUE 'A',
                success LIKE sy-msgty VALUE 'S',
                x       LIKE sy-msgty VALUE 'X',
              END   OF c_msg_type.

****Object
  CONSTANTS: BEGIN OF c_object,
               bor              TYPE /agri/tgabo-object VALUE '/AGRI/GLMD',
               log              TYPE balobj-object      VALUE '/AGRI/GLMD',
               change_documents TYPE cdobjectcl         VALUE '/AGRI/GLMD',
               esh_object       TYPE /agri/geshobjtp    VALUE '/AGRI/GLMD',
               text_object      TYPE thead-tdobject     VALUE '/AGRI/GLMD',
             END OF c_object.

  FIELD-SYMBOLS: <fs_mddoc_infocus> TYPE /agri/s_glmd_doc.

  DEFINE d_add_message.
    ls_vtx_msg-msgid = sy-msgid.
    ls_vtx_msg-msgno = sy-msgno.
    ls_vtx_msg-msgty = sy-msgty.
    ls_vtx_msg-msgv1 = sy-msgv1.
    ls_vtx_msg-msgv2 = sy-msgv2.
    ls_vtx_msg-msgv3 = sy-msgv3.
    ls_vtx_msg-msgv4 = sy-msgv4.
    ls_vtx_msg-msgli = sy-msgli.
    APPEND ls_vtx_msg TO lt_vtx_msg.
  END-OF-DEFINITION.

*-- Tab.Auxiliar: <gfs_taux>[]
*-- Detalhado: <gfs_tcopy>[]
*-- Consolidado: <gfs_dyn_tmdm>[]
  SELECT *
    FROM zabst_sf_estimat
    INTO TABLE @DATA(lt_ranges)
   ORDER BY prioridade.

  <gfs_taux>[] = <gfs_dyn_tmdm>[].

  SORT <gfs_taux> BY (c_sort-fornecedor) ASCENDING
                     (c_sort-imovel)     ASCENDING
                     (c_sort-material)   ASCENDING.

  SORT <gfs_tcopy> BY (c_sort-fornecedor) ASCENDING
                      (c_sort-imovel)     ASCENDING
                      (c_sort-material)   ASCENDING.

  IF gt_mdhdr[] IS NOT INITIAL.
    SORT gt_mdhdr BY mdocm.
    DATA(lv_executed) = abap_false.

    DATA(lt_status_ini) = gt_doctos[].
    DATA(lt_status_dif) = gt_doctos[].
    DELETE lt_status_ini WHERE ustat NE 'E0002'.
*-- Desconsidera documentos inconsistentes (sem Status Flow)
    DELETE lt_status_dif WHERE ustat EQ 'E0002'
                            OR ustat IS INITIAL.
    DELETE gt_doctos WHERE ustat IS INITIAL.

    LOOP AT <gfs_taux> ASSIGNING <gfs_saux>.
      ASSIGN COMPONENT:
        c_sort-total    OF STRUCTURE <gfs_saux> TO FIELD-SYMBOL(<fs_total_tot>),
        c_sort-imovel   OF STRUCTURE <gfs_saux> TO FIELD-SYMBOL(<fs_imovel_tot>),
        c_sort-var_perc OF STRUCTURE <gfs_saux> TO FIELD-SYMBOL(<fs_var_perc_tot>),
        c_sort-var_qtd  OF STRUCTURE <gfs_saux> TO FIELD-SYMBOL(<fs_var_qtd_tot>).

      IF <fs_total_tot> IS ASSIGNED
      AND <fs_imovel_tot> IS ASSIGNED
      AND <fs_var_perc_tot> IS ASSIGNED
      AND <fs_var_qtd_tot> IS ASSIGNED.
        IF <fs_total_tot> EQ abap_false.
          CONTINUE.
        ELSE.
          DATA(lv_excecao) = abap_false.
*-- Todos os Documentos de Medição do Imóvel possuem status inicial
          READ TABLE lt_status_ini INTO DATA(ls_status_ini)
            WITH KEY imovel = <fs_imovel_tot> BINARY SEARCH.
          IF sy-subrc EQ 0.
            READ TABLE lt_status_dif INTO DATA(ls_status_dif)
            WITH KEY imovel = <fs_imovel_tot> BINARY SEARCH.
            IF sy-subrc NE 0.
              lv_excecao = abap_true.
            ENDIF.
          ENDIF.

          LOOP AT <gfs_tcopy> ASSIGNING <gfs_scopy>.
            ASSIGN COMPONENT:
              c_sort-docto      OF STRUCTURE <gfs_scopy> TO FIELD-SYMBOL(<fs_documento>),
              c_sort-fornecedor OF STRUCTURE <gfs_scopy> TO FIELD-SYMBOL(<fs_fornecedor>),
              c_sort-imovel     OF STRUCTURE <gfs_scopy> TO FIELD-SYMBOL(<fs_imovel>),
              c_sort-material   OF STRUCTURE <gfs_scopy> TO FIELD-SYMBOL(<fs_material>),
              c_sort-status_sf  OF STRUCTURE <gfs_scopy> TO FIELD-SYMBOL(<fs_status_sf>).

            IF <fs_documento> IS NOT ASSIGNED
            OR <fs_fornecedor> IS NOT ASSIGNED
            OR <fs_imovel> IS NOT ASSIGNED
            OR <fs_material> IS NOT ASSIGNED
            OR <fs_status_sf> IS NOT ASSIGNED.
              EXIT.
            ELSE.
              IF <fs_status_sf> NE 'E0002'
              AND <fs_status_sf> IS NOT INITIAL.
                CONTINUE.
              ENDIF.
              IF <fs_imovel_tot> EQ <fs_imovel>.
                READ TABLE gt_mdhdr INTO DATA(ls_mdhdr)
                  WITH KEY mdocm = <fs_documento> BINARY SEARCH.

                IF sy-subrc EQ 0.
                  IF ls_mdhdr-kfrst EQ 'A'.
                    lv_executed = abap_true.
                    IF <fs_var_qtd_tot> LT 0.
                      <fs_var_qtd_tot> = <fs_var_qtd_tot> * -1.
                    ENDIF.
                    IF <fs_var_perc_tot> LT 0.
                      <fs_var_perc_tot> = <fs_var_perc_tot> * -1.
                    ENDIF.
*-verificar status do documento
*-tratar msg de retorno
*-1: docto. bloqueado.
*-2: sucesso
*-3: outros erros
                    IF lv_excecao EQ abap_false.
                      lv_var_cx = <fs_var_qtd_tot>.
                    ELSEIF lv_excecao EQ abap_true.
                      READ TABLE gt_doctos INTO DATA(lwa_docto)
                        WITH KEY imovel = <fs_imovel> BINARY SEARCH.
                      IF sy-subrc EQ 0
                      AND lwa_docto-est_inicial IS NOT INITIAL.
                        lv_var_cx = lwa_docto-est_inicial.
                      ENDIF.
                    ENDIF.

                    LOOP AT lt_ranges INTO DATA(lwa_range).
                      IF ( ( lv_var_cx BETWEEN lwa_range-var_cx_de
                                         AND lwa_range-var_cx_ate )
                       AND ( <fs_var_perc_tot> BETWEEN lwa_range-var_perc_de
                                                  AND lwa_range-varia_perc_ate )
                       AND ( lv_excecao EQ abap_false ) ) OR
                         ( ( lv_var_cx BETWEEN lwa_range-var_cx_de
                                         AND lwa_range-var_cx_ate )
                       AND ( lv_excecao EQ abap_true ) ).
*                        IF lwa_range-stsfl NE ls_mdhdr-stsma.
*-- Delete old status object.
                        IF ls_mdhdr-stsma IS NOT INITIAL
                        AND ls_mdhdr-objnr IS NOT INITIAL.
*                        OR ls_mdhdr-stsma IS INITIAL.
                          PERFORM document_infocus_read IN PROGRAM /agri/saplglmdm
                                                             USING ls_mdhdr-mdocm IF FOUND.

                          CLEAR lv_subrc.
                          PERFORM document_infocus_lock USING ls_mdhdr-mdocm
                                                     CHANGING lv_subrc.
                          IF lv_subrc NE 0.
*-- Document &1 is locked by User &2.
                            MESSAGE i005(/agri/glmd) WITH ls_mdhdr-mdocm sy-msgv1 INTO sy-msgli.
                            sy-msgid = '/AGRI/GLMD'.
                            sy-msgno = '005'.
                            sy-msgv2 = sy-msgv1.
                            sy-msgv1 = ls_mdhdr-mdocm.
                            d_add_message.
                          ELSE.
                            ASSIGN ('(/AGRI/SAPLGLMDM)GS_MDDOC_INFOCUS') TO <fs_mddoc_infocus>.
                            IF sy-subrc EQ 0.
*-- Delete Status Object
                              CALL FUNCTION 'STATUS_OBJECT_DELETE'
                                EXPORTING
                                  objnr  = <fs_mddoc_infocus>-x-mdhdr-objnr
                                EXCEPTIONS
                                  OTHERS = 0.

*-- Delete status object and associated statuses
                              CALL FUNCTION '/AGRI/G_USER_STATUS_UPDATE_DB'
                                EXPORTING
                                  i_objnr = <fs_mddoc_infocus>-x-mdhdr-objnr
                                  i_updkz = c_updkz_delete.

                              <fs_mddoc_infocus>-x-mdhdr-muser = sy-uname.
                              <fs_mddoc_infocus>-x-mdhdr-mdate = sy-datum.
                              <fs_mddoc_infocus>-x-mdhdr-mtime = sy-uzeit.
                              <fs_mddoc_infocus>-x-mdhdr-updkz = c_updkz_update.
                              <fs_mddoc_infocus>-updkz = c_updkz_update.

                              CLEAR: <fs_mddoc_infocus>-x-mdhdr-stsma,
                                     <fs_mddoc_infocus>-x-mdhdr-objnr.

*                                CALL FUNCTION '/AGRI/G_STATUS_INIT'.

*-- Determine new status object type depending on
*-- the variation between estimated and realized value.
                              <fs_mddoc_infocus>-x-mdhdr-stsma = lwa_range-stsfl.

*-- Create new status object with new object number and with new user
*-- status scheme.
                              IF <fs_mddoc_infocus>-x-mdhdr-stsma IS NOT INITIAL.
                                PERFORM status_object_create IN PROGRAM /agri/saplglmdm
                                  USING <fs_mddoc_infocus>-x-mdhdr-stsma
                                  CHANGING <fs_mddoc_infocus>-x-mdhdr-objnr IF FOUND.

                                PERFORM status_update IN PROGRAM /agri/saplglmdm IF FOUND.

                                PERFORM object_number_get IN PROGRAM /agri/saplglmdu
                                  USING c_object-bor <fs_mddoc_infocus>-x-mdhdr-mdocm
                                  CHANGING <fs_mddoc_infocus>-x-mdhdr-objnr IF FOUND.
*                              ELSE.
*                                ASSIGN ('(/AGRI/SAPLGLMDM)/AGRI/S_GLMDHDR') TO FIELD-SYMBOL(<ls_glmdhdr>).
*                                IF sy-subrc EQ 0.
*                                  <fs_mddoc_infocus>-x-mdhdr-kfrst = space.
*                                  CLEAR <fs_mddoc_infocus>-x-mdhdr-ustat.
*                                  MOVE-CORRESPONDING <fs_mddoc_infocus>-x-mdhdr TO <ls_glmdhdr>.
*                                ENDIF.
                              ENDIF.
                              REFRESH: lt_mdocm, lt_messages.
                              PERFORM zabs_mddoc_infocus_save IN PROGRAM /agri/saplglmdm
                                                                CHANGING lt_messages
                                                                         lv_subrc
                                                                         lt_mdocm IF FOUND.

                              LOOP AT lt_messages INTO ls_message.
                                MESSAGE ID ls_message-msgid TYPE ls_message-msgty
                                   NUMBER ls_message-msgno WITH ls_message-msgv1
                                   ls_message-msgv2 ls_message-msgv3 ls_message-msgv4
                                              INTO sy-msgli.
                                d_add_message.
                              ENDLOOP.

                              IF lv_subrc NE 0.
                                IF lv_subrc EQ 1.
                                  MESSAGE ID '/AGRI/GLOBAL' TYPE c_msg_type-success NUMBER '322'
                                                                                    INTO sy-msgli.
                                  sy-msgid = '/AGRI/GLOBAL'.
                                  sy-msgno = '322'.
                                  sy-msgty = c_msg_type-success.
                                  d_add_message.
                                ELSE.
                                  MESSAGE ID '/AGRI/GLMD' TYPE c_msg_type-error NUMBER '011'
                                                          WITH <fs_mddoc_infocus>-x-mdhdr-mdocm
                                                          INTO sy-msgli.
                                  sy-msgid = '/AGRI/GLMD'.
                                  sy-msgno = '011'.
                                  sy-msgty = c_msg_type-error.
                                  sy-msgv1 = <fs_mddoc_infocus>-x-mdhdr-mdocm.
                                  d_add_message.
                                ENDIF.
                              ELSE.
                                LOOP AT lt_mdocm INTO DATA(lwa_mddoc_infocus).
                                  MESSAGE ID '/AGRI/GLMD' TYPE c_msg_type-success NUMBER '006'
                                     WITH <fs_mddoc_infocus>-x-mdhdr-mdocm INTO sy-msgli.
                                  sy-msgid = '/AGRI/GLMD'.
                                  sy-msgno = '006'.
                                  sy-msgty = c_msg_type-success.
                                  sy-msgv1 = <fs_mddoc_infocus>-x-mdhdr-mdocm.
                                  d_add_message.
                                ENDLOOP.
                              ENDIF.
                            ENDIF.
                          ENDIF.
                          PERFORM document_infocus_unlock USING ls_mdhdr-mdocm.
                        ENDIF.
                        EXIT.
*                        ENDIF.
                      ENDIF.
                    ENDLOOP.
                  ENDIF.
                ENDIF.
              ELSE.
                CONTINUE.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF lv_executed = abap_false.
    IF gt_mdhdr[] IS NOT INITIAL.
*--Nenhum status modificado!
      MESSAGE i074(zfmfp).
      LEAVE LIST-PROCESSING.
    ELSE.
*--Não existem dados para os parâmetros informados
*      MESSAGE i061(zfmfp).
      MESSAGE i032(zfmfp) WITH TEXT-061.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ELSE.
    LOOP AT lt_vtx_msg INTO ls_vtx_msg.
      INSERT INITIAL LINE INTO TABLE lt_bapiret2
        ASSIGNING FIELD-SYMBOL(<ls_bapiret2>).
      IF sy-subrc EQ 0.
        <ls_bapiret2>-type       = ls_vtx_msg-msgty.
        <ls_bapiret2>-id         = ls_vtx_msg-msgid.
        <ls_bapiret2>-number     = ls_vtx_msg-msgno.
        <ls_bapiret2>-message    = ls_vtx_msg-msgli.
        <ls_bapiret2>-message_v1 = ls_vtx_msg-msgv1.
        <ls_bapiret2>-message_v2 = ls_vtx_msg-msgv2.
        <ls_bapiret2>-message_v3 = ls_vtx_msg-msgv3.
        <ls_bapiret2>-message_v4 = ls_vtx_msg-msgv4.
      ENDIF.
    ENDLOOP.

    IF lt_bapiret2[] IS NOT INITIAL.
      CALL FUNCTION 'C14ALD_BAPIRET2_SHOW'
        TABLES
          i_bapiret2_tab = lt_bapiret2[].
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_INFOCUS_LOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM document_infocus_lock  USING lv_mdocm TYPE /agri/glmdocm
                         CHANGING lv_subrc.

  DATA: lv_msgv1 LIKE sy-msgv1,
        lv_msgli TYPE sy-msgli.

  CHECK NOT lv_mdocm IS INITIAL.

  CALL FUNCTION 'ENQUEUE_/AGRI/EZ_GLMD'
    EXPORTING
      mdocm          = lv_mdocm
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
    lv_subrc = sy-subrc.
    lv_msgv1 = sy-msgv1.
****Document &1 is locked by User &2.
    MESSAGE i005(/agri/glmd) WITH lv_mdocm lv_msgv1 INTO lv_msgli.
    sy-msgli = lv_msgli.
    message_simple space.
  ENDIF.

ENDFORM.                    " DOCUMENT_INFOCUS_LOCK

*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_INFOCUS_UNLOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM document_infocus_unlock USING lv_mdocm.

  CHECK lv_mdocm IS NOT INITIAL.

  CALL FUNCTION 'DEQUEUE_/AGRI/EZ_GLMD'
    EXPORTING
      mdocm = lv_mdocm.

ENDFORM.                    " DOCUMENT_INFOCUS_UNLOCK

*&---------------------------------------------------------------------*
*& Form AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM authority_check.

  AUTHORITY-CHECK OBJECT 'ZABS_GSFM'
   ID 'ACTVT' FIELD c_authorization_activity-change.

*  IF sy-subrc <> 0.
**-- Sem autorização para aplicar Alçadas de Aprovação
*    MESSAGE ID 'ZFMFP' TYPE c_msg_type-info NUMBER '070' INTO sy-msgli.
*    LEAVE LIST-PROCESSING.
*  ENDIF.
  IF sy-subrc NE 0.
    gv_toolbar = abap_false.
  ELSE.
    gv_toolbar = abap_true.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  TOOLBAR_EXCLUDES_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM toolbar_excludes_prepare TABLES lt_toolbar_exc TYPE ui_functions.

  APPEND 'APPROVE' TO lt_toolbar_exc.
  APPEND 'REJECT'  TO lt_toolbar_exc.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_REGION
*&---------------------------------------------------------------------*
FORM get_region.

  SELECT rbnr
    INTO TABLE @DATA(lt_region)
    FROM t352b.

  IF sy-subrc EQ 0.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        pvalkey          = ' '
        retfield         = 'RBNR'
        dynpprog         = sy-repid
        dynpnr           = sy-dynnr
        dynprofield      = 'P_REGION'
        callback_program = sy-repid
        value_org        = 'S'
      TABLES
        value_tab        = lt_region
      EXCEPTIONS
        parameter_error  = 1
        no_values_found  = 2
        OTHERS           = 3.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SET_DEFAULT_VALUES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_default_values .

  CONSTANTS: BEGIN OF c_tipo_safra,
               contabil TYPE zfmtpsafra VALUE 'C',
               tecnica  TYPE zfmtpsafra VALUE 'T',
             END OF c_tipo_safra.

  IF p_year IS INITIAL.
    SELECT *
      FROM zfmfpsafras
      INTO TABLE @DATA(lt_safras)
     WHERE tipo_safra = @c_tipo_safra-tecnica.

    IF sy-subrc EQ 0.
      LOOP AT lt_safras INTO DATA(ls_safra).
        IF sy-datum BETWEEN ls_safra-inicio_safra AND ls_safra-fim_safra.
          p_year = ls_safra-ano_safra.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.
