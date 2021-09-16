************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name    : ZABS_INC_APROVA_DOC_MED_F01                         *
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

  REFRESH: gt_fcat, gt_mdhdr, gt_gsfstp,
           gt_gsfocm, gt_tj30.

  CLEAR: gs_variables, gv_stsma. "fcode, ok_code.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_MEASUREMENT_DATA
*&---------------------------------------------------------------------*
*& GET_MEASUREMENT_DATA
*&---------------------------------------------------------------------*
FORM get_measurement_data.

*-- Local Declarations
  DATA: lt_constants TYPE zabs_tty_vkey_const,
        ltr_estat    TYPE RANGE OF j_estat,
        lt_dyn_table TYPE REF TO data,
        lt_cabn      TYPE tty_cabn_new, "tty_cabn,
        lt_fname     TYPE STANDARD TABLE OF ty_fname,

*--Workarea declaration
        lsr_estat    LIKE LINE OF ltr_estat,
        ls_atgdoc    TYPE /agri/s_glatgdoc,

*--Local variable declaration
        lv_tabix     TYPE sy-tabix,
        lv_packed    TYPE p DECIMALS 4,
        lv_data_char TYPE atwrt,
        lv_i         TYPE i,
        lv_reason    TYPE atinn.

*-- Fetch the status profile maintained in the variant table for the
*-- measurement document type and measurement group
  CALL METHOD zcl_abs_get_variants=>get_constant_multiple
    EXPORTING
      iv_objid     = zcl_abs_abap_maintain=>c_objid_md_status_profile
      iv_k1val     = zcl_abs_abap_maintain=>c_key_measurement_type
      iv_k2val     = p_mdtyp
    IMPORTING
      et_constants = lt_constants.

  READ TABLE lt_constants INTO DATA(ls_constants)
        WITH KEY cnval1 = p_mpgrp.
  IF sy-subrc EQ 0.
    gv_stsma = ls_constants-cnval2.
  ENDIF.

*-- Give error message if status profile not maintainted
  IF gv_stsma IS INITIAL.
    MESSAGE TEXT-004 TYPE zcl_abs_abap_maintain=>c_msgty_error "'E'
    DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_info. "'I'
    LEAVE LIST-PROCESSING.
  ENDIF.

*-- Fetch the Status Flow Steps
  SELECT stsma stsfl flstp deflt sstat
    FROM /agri/tgsfstp
    INTO TABLE gt_gsfstp
   WHERE stsma EQ gv_stsma.
  IF sy-subrc = 0.
    SORT gt_gsfstp BY stsma sstat.
  ENDIF.

  IF p_etapa IS NOT INITIAL.
    DELETE gt_gsfstp WHERE flstp NE p_etapa.
  ENDIF.

*-- Fetch the Status Flow Outcomes
  SELECT *
    FROM /agri/tgsfocm
    INTO TABLE gt_gsfocm
   WHERE stsma EQ gv_stsma.
  IF sy-subrc = 0.
    SORT gt_gsfocm BY stsma stsfl flstp soutc.
  ENDIF.

**-- Fetch the user status for the status profile
*  SELECT *
*    FROM tj30
*    INTO TABLE @DATA(lt_tj30_tmp)
*   WHERE stsma EQ @gv_stsma.
*  IF sy-subrc EQ 0.
*    gt_tj30 = lt_tj30_tmp.
*    SORT gt_tj30 BY estat stsma.
*    REFRESH lt_tj30_tmp.
*  ENDIF.
*
*  LOOP AT gt_gsfstp ASSIGNING FIELD-SYMBOL(<lfs_gsfstp>).
*    READ TABLE gt_gsfocm INTO DATA(ls_gsfocm)
*          WITH KEY stsfl = <lfs_gsfstp>-stsfl
*                   flstp = <lfs_gsfstp>-flstp
*          BINARY SEARCH.
*    IF sy-subrc EQ 0 AND ls_gsfocm-uentr EQ abap_true.
*      READ TABLE gt_tj30 INTO DATA(ls_tj30)
*            WITH KEY estat = <lfs_gsfstp>-sstat
*            BINARY SEARCH.
*      IF sy-subrc EQ 0.
*        APPEND ls_tj30 TO lt_tj30_tmp.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
*
**--Authority Check
*  LOOP AT lt_tj30_tmp INTO ls_tj30.
*    IF ls_tj30-bersl IS NOT INITIAL.
*      AUTHORITY-CHECK OBJECT 'B_USERSTAT'
*       ID 'STSMA' FIELD gv_stsma
*       ID 'OBTYP' FIELD 'XMD'
*       ID 'BERSL' FIELD ls_tj30-bersl
*       ID 'ACTVT' FIELD '01'.
*      IF sy-subrc EQ 0.
*        CLEAR lsr_estat.
*        lsr_estat-sign   = zcl_abs_abap_maintain=>c_rsign_include. "'I'
*        lsr_estat-option = zcl_abs_abap_maintain=>c_ropt_equal. "'EQ'
*        lsr_estat-low    = ls_tj30-estat. "lv_estat.
*        APPEND lsr_estat TO ltr_estat.
*      ENDIF.
*    ELSE.
*      AUTHORITY-CHECK OBJECT 'B_USERSTAT'
*       ID 'STSMA' FIELD gv_stsma
*       ID 'OBTYP' FIELD 'XMD'
*       ID 'BERSL' DUMMY
*       ID 'ACTVT' FIELD '01'.
*      IF sy-subrc EQ 0.
*        CLEAR lsr_estat.
*        lsr_estat-sign   = zcl_abs_abap_maintain=>c_rsign_include. "'I'
*        lsr_estat-option = zcl_abs_abap_maintain=>c_ropt_equal. "'EQ'
*        lsr_estat-low    = ls_tj30-estat. "lv_estat.
*        APPEND lsr_estat TO ltr_estat.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.


  SELECT *
    FROM /agri/tgsfrcp
    INTO TABLE gt_tgsfrcp
     FOR ALL ENTRIES IN gt_gsfstp
   WHERE stsfl = gt_gsfstp-stsfl AND
         flstp = gt_gsfstp-flstp.




  IF p_werks IS NOT INITIAL.
*---Fetching Farms
    SELECT tplnr_fl
      FROM /agri/glflot
      INTO TABLE @DATA(lt_glflot1)
     WHERE tplvl  EQ @zcl_abs_abap_maintain=>c_tplvl_farm "'1'
       AND iwerk  EQ @p_werks
       AND kfrst  EQ @space
       AND ownshp EQ @zcl_abs_abap_maintain=>c_ownership_own "'OW'
       AND loevm  EQ @space.
  ELSE.
*---Fetching Farms
    SELECT tplnr_fl
      FROM /agri/glflot
      INTO TABLE @lt_glflot1
     WHERE tplvl  EQ @zcl_abs_abap_maintain=>c_tplvl_farm "'1'
       AND kfrst  EQ @space
       AND ownshp EQ @zcl_abs_abap_maintain=>c_ownership_own "'OW'
       AND loevm  EQ @space.
  ENDIF.

  IF lt_glflot1[] IS INITIAL.
    MESSAGE i030(zabs_msgcls).
    LEAVE LIST-PROCESSING.
  ELSE.
    SORT lt_glflot1 BY tplnr_fl.
  ENDIF.

  IF p_region IS NOT INITIAL.
*--Fetching Terrain Attribute Values to get Region
    SELECT a~tplnr_fl, a~atwrt, b~atinn
      FROM /agri/glflatv AS a
      JOIN cabn          AS b
        ON b~atinn = a~atinn
      INTO TABLE @DATA(lt_glflatv)
       FOR ALL ENTRIES IN @lt_glflot1
     WHERE a~tplnr_fl  EQ @lt_glflot1-tplnr_fl
       AND a~class     EQ @zcl_abs_abap_maintain=>c_attrgrp_citimovel "'CIT-IMOVEL'
       AND a~atwrt     EQ @p_region
       AND b~atnam     EQ @zcl_abs_abap_maintain=>c_charact_faz_regiao. "'FAZ_REGIAO'

    IF sy-subrc NE 0.
      MESSAGE i030(zabs_msgcls).
      LEAVE LIST-PROCESSING.
    ELSE.
      SORT lt_glflatv BY tplnr_fl atinn.

      LOOP AT lt_glflot1 INTO DATA(ls_glflot1).
        lv_tabix = sy-tabix.
        READ TABLE lt_glflatv TRANSPORTING NO FIELDS
          WITH KEY tplnr_fl = ls_glflot1-tplnr_fl BINARY SEARCH.
        IF sy-subrc NE 0.
          DELETE lt_glflot1 INDEX lv_tabix.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

  IF lt_glflot1[] IS INITIAL.
    MESSAGE i030(zabs_msgcls).
    LEAVE LIST-PROCESSING.
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
    MESSAGE i030(zabs_msgcls).
    LEAVE LIST-PROCESSING.
  ELSE.
    SORT lt_glflot2 BY tplnr_fl.
  ENDIF.

*--Fetching Measurement document data based on terrains
  SELECT mdocm mdtyp tplnr_fl contr mpgrp stsma objnr ustat
    FROM /agri/glmdhdr
    INTO TABLE gt_mdhdr
     FOR ALL ENTRIES IN lt_glflot2
   WHERE mdocm    IN so_mdocm
     AND mdtyp    EQ p_mdtyp
     AND tplnr_fl EQ lt_glflot2-tplnr_fl
     AND mpgrp    EQ p_mpgrp
     AND ustat    IN ltr_estat.
*
*  IF sy-subrc EQ 0.
*    SELECT mdocm, atinn, atzhl, atwrt, atflv
*      FROM /agri/glmdatv
*      INTO TABLE @DATA(lt_mdatv)
*       FOR ALL ENTRIES IN @gt_mdhdr
*     WHERE mdocm EQ @gt_mdhdr-mdocm.
*
*    IF sy-subrc EQ 0.
*      SORT lt_mdatv BY mdocm atinn.
*    ENDIF.
*
*    DATA(lt_crop_seasons) = gt_mdhdr.
*    SORT lt_crop_seasons BY tplnr_fl contr.
*    DELETE ADJACENT DUPLICATES FROM lt_crop_seasons COMPARING tplnr_fl contr.
*
*    IF p_mpgrp NE 'FAZ-COMPLEXIDADE'.
**--Fetching Crop Seasons data
*      SELECT *
*        FROM /agri/glflcma
*        INTO TABLE @DATA(lt_glflcma)
*        FOR ALL ENTRIES IN @lt_crop_seasons
*       WHERE tplnr_fl EQ @lt_crop_seasons-tplnr_fl
*         AND contr    EQ @lt_crop_seasons-contr.
*
*      IF sy-subrc NE 0.
*        MESSAGE i030(zabs_msgcls).
*        LEAVE LIST-PROCESSING.
*      ELSE.
*        SORT lt_glflcma BY tplnr_fl contr.
*        DATA(lt_glflcma_temp) = lt_glflcma.
*        DELETE ADJACENT DUPLICATES FROM lt_glflcma_temp COMPARING ymatnr.
*
*        SELECT matnr, maktx
*          FROM makt
*          INTO TABLE @DATA(lt_makt)
*          FOR ALL ENTRIES IN @lt_glflcma_temp
*          WHERE matnr EQ @lt_glflcma_temp-ymatnr
*            AND spras EQ @sy-langu.
*        IF sy-subrc EQ 0.
*          SORT lt_makt BY matnr.
*        ENDIF.
*      ENDIF.
*
*      SELECT *
*        FROM /agri/glflcma
*        INTO TABLE @DATA(lt_previsto)
*        FOR ALL ENTRIES IN @lt_crop_seasons
*       WHERE tplnr_fl   EQ @lt_crop_seasons-tplnr_fl
*         AND zzprevisto EQ @abap_true.
*
*      SORT lt_previsto BY tplnr_fl contr.
*  ENDIF.
*ENDIF.

**--Fetching Class Header Data
*  SELECT clint, class, klart
*    FROM klah
*    INTO @DATA(ls_klah)
*   WHERE class EQ @p_mpgrp.
*  ENDSELECT.
*
*  CALL FUNCTION '/AGRI/GLAG_VIEW_SINGLE'
*    EXPORTING
*      i_clint                = ls_klah-clint
*      i_atgrp_type           = ls_klah-klart
*      i_no_attributes_addata = abap_true
*    IMPORTING
*      es_atgdoc              = ls_atgdoc.
*
**--Fetching Attribute names for measurement documents
*  IF ls_atgdoc-x-ksml IS NOT INITIAL.
*    SELECT a~atinn, a~adzhl, a~atnam, a~atidn,
*           a~atfor, a~anzst, a~anzdz, a~atvor,
*           a~atsch, b~atbez
*      FROM cabn AS a
*      INNER JOIN cabnt AS b
*      ON b~atinn = a~atinn
*      INTO TABLE @lt_cabn
*      FOR ALL ENTRIES IN @ls_atgdoc-x-ksml
*     WHERE a~atinn = @ls_atgdoc-x-ksml-imerk
*       AND b~spras = @sy-langu.
*    IF sy-subrc = 0.
*      SORT lt_cabn BY atinn atnam.
*
*      READ TABLE lt_cabn INTO DATA(ls_cabn)
*        WITH KEY atnam = zcl_abs_abap_maintain=>c_charact_contamotivo.  "'CIT-CONTA-MOTIVO'
*      IF sy-subrc = 0.
**--Fetching Characteristic values to get Reason description
*        SELECT a~atinn,a~atwrt,
*               b~atwtb
*          INTO TABLE @DATA(lt_attdesc)
*          FROM cawn  AS a
*         RIGHT JOIN cawnt AS b
*            ON b~atinn EQ a~atinn
*           AND b~atzhl EQ a~atzhl
*         WHERE a~atinn EQ @ls_cabn-atinn
*           AND b~spras EQ @sy-langu.
*        IF sy-subrc = 0.
*          SORT lt_attdesc BY atinn atwrt.
*        ENDIF.
*      ENDIF.
*
*      CLEAR ls_cabn.
*      READ TABLE lt_cabn INTO ls_cabn
*        WITH KEY atnam = zcl_abs_abap_maintain=>c_charact_atividade. "Inventory Activity
*      IF sy-subrc = 0.
*        SELECT a~ivact,b~descr
*          INTO TABLE @DATA(lt_actdesc)
*          FROM /agri/tglamact AS a
*         INNER JOIN /agri/tglamactt AS b
*            ON a~ivact = b~ivact
*         WHERE spras EQ @sy-langu.
*        IF sy-subrc = 0.
*          SORT lt_actdesc BY ivact.
*        ENDIF.
*      ENDIF.
*
*    ENDIF.
*  ENDIF.

**--Building dynamic field catalog
*PERFORM dynamic_fcat_prepare USING ls_atgdoc-x-ksml
*                                   lt_cabn
*                          CHANGING lt_fname.
*
**--Calling method to generate the dynamic internal table
*CALL METHOD cl_alv_table_create=>create_dynamic_table
*  EXPORTING
*    it_fieldcatalog           = gt_fcat
*  IMPORTING
*    ep_table                  = lt_dyn_table
*  EXCEPTIONS
*    generate_subpool_dir_full = 1
*    OTHERS                    = 2.
*IF sy-subrc <> 0.
*  RETURN.
*ENDIF.
*
**--assiagn Object to Dynamic table
*ASSIGN lt_dyn_table->* TO <gfs_dyn_tmdm>.
*CREATE DATA lt_dyn_table LIKE LINE OF <gfs_dyn_tmdm>.
*ASSIGN lt_dyn_table->* TO <gfs_dyn_smdm>.

  LOOP AT gt_mdhdr INTO DATA(ls_mdhdr).

    " UNASSIGN <lfs_gsfstp>.
    READ TABLE gt_gsfstp TRANSPORTING NO FIELDS WITH KEY stsma = ls_mdhdr-stsma
                                                         sstat = ls_mdhdr-ustat
                                                BINARY SEARCH.
    IF sy-subrc <> 0.

      CONTINUE.
    ENDIF.

    gv_send_email = abap_true.
    EXIT.
*    ASSIGN COMPONENT 'MDOCM' OF STRUCTURE <gfs_dyn_smdm> TO FIELD-SYMBOL(<fs_value>).
*    IF <fs_value> IS ASSIGNED.
*      <fs_value> = ls_mdhdr-mdocm.
*    ENDIF.
*
*    UNASSIGN <fs_value>.
*    ASSIGN COMPONENT 'MDTYP' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
*    IF <fs_value> IS ASSIGNED.
*      <fs_value> = ls_mdhdr-mdtyp.
*    ENDIF.
*
*    UNASSIGN <fs_value>.
*    ASSIGN COMPONENT 'MPGRP' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
*    IF <fs_value> IS ASSIGNED.
*      <fs_value> = ls_mdhdr-mpgrp.
*    ENDIF.
*
*    UNASSIGN <fs_value>.
*    ASSIGN COMPONENT 'STSFL' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
*    IF <fs_value> IS ASSIGNED.
*      <fs_value> = <lfs_gsfstp>-stsfl.
*    ENDIF.
*
*    UNASSIGN <fs_value>.
*    ASSIGN COMPONENT 'STSMA' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
*    IF <fs_value> IS ASSIGNED.
*      <fs_value> = <lfs_gsfstp>-stsma.
*    ENDIF.
*
*    UNASSIGN <fs_value>.
*    ASSIGN COMPONENT 'FLSTP' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
*    IF <fs_value> IS ASSIGNED.
*      <fs_value> = <lfs_gsfstp>-flstp.
*    ENDIF.
*
*    UNASSIGN <fs_value>.
*    ASSIGN COMPONENT 'TPLNR' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
*    IF <fs_value> IS ASSIGNED.
*      <fs_value> = ls_mdhdr-tplnr_fl.
*    ENDIF.
*
*    READ TABLE lt_glflcma INTO DATA(ls_glflcma)
*      WITH KEY tplnr_fl = ls_mdhdr-tplnr_fl
*               contr    = ls_mdhdr-contr BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      UNASSIGN <fs_value>.
*      ASSIGN COMPONENT 'YMATNR' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
*      IF <fs_value> IS ASSIGNED.
*        <fs_value> = ls_glflcma-ymatnr.
*      ENDIF.
*
*      READ TABLE lt_makt INTO DATA(ls_makt)
*            WITH KEY matnr = ls_glflcma-ymatnr
*          BINARY SEARCH.
*      IF sy-subrc EQ 0.
*        UNASSIGN <fs_value>.
*        ASSIGN COMPONENT 'MAKTX' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
*        IF <fs_value> IS ASSIGNED.
*          <fs_value> = ls_makt-maktx.
*        ENDIF.
*      ENDIF.
*
*      IF ls_glflcma-zzfazplantio IS NOT INITIAL.
*        UNASSIGN <fs_value>.
*        ASSIGN COMPONENT 'CSYEAR' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
*        IF <fs_value> IS ASSIGNED.
*          IF ls_glflcma-datbi GE sy-datum.
*            ls_glflcma-datbi = sy-datum.
*          ENDIF.
*          <fs_value> = ( ls_glflcma-datbi - ls_glflcma-zzfazplantio ) / 365.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
*    READ TABLE lt_previsto INTO DATA(ls_previsto)
*      WITH KEY tplnr_fl = ls_mdhdr-tplnr_fl
*               contr    = ls_mdhdr-contr BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      UNASSIGN <fs_value>.
*      ASSIGN COMPONENT 'ZZFAZVARTECNIA' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
*      IF <fs_value> IS ASSIGNED.
*        <fs_value> = ls_previsto-zzfazvartecnia.
*      ENDIF.
*
*      ASSIGN COMPONENT 'ZZPORTA_ENXERTO' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
*      IF <fs_value> IS ASSIGNED.
*        <fs_value> = ls_previsto-zzporta_enxerto.
*      ENDIF.
*
*      ASSIGN COMPONENT 'ZZESP_RUA' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
*      IF <fs_value> IS ASSIGNED.
*        <fs_value> = ls_previsto-zzesp_rua.
*      ENDIF.
*
*      ASSIGN COMPONENT 'ZZESP_PES' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
*      IF <fs_value> IS ASSIGNED.
*        <fs_value> = ls_previsto-zzesp_pes.
*      ENDIF.
*
*      ASSIGN COMPONENT 'ZZQTD_PLANTAS' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
*      IF <fs_value> IS ASSIGNED.
*        <fs_value> = ls_previsto-zzqtd_plantas.
*      ENDIF.
*
*      ASSIGN COMPONENT 'ZZPREV_PLANTIO' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
*      IF <fs_value> IS ASSIGNED.
*        <fs_value> = ls_previsto-zzprev_plantio.
*      ENDIF.
*
*      ASSIGN COMPONENT 'ZZPREV_ERRAD' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
*      IF <fs_value> IS ASSIGNED.
*        <fs_value> = ls_previsto-zzprev_errad.
*      ENDIF.
*
*      ASSIGN COMPONENT 'ZZAREA_TALHAO' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
*      IF <fs_value> IS ASSIGNED.
*        <fs_value> = ls_previsto-zzarea_talhao.
*      ENDIF.
*    ENDIF.
*
*    READ TABLE lt_mdatv TRANSPORTING NO FIELDS
*      WITH KEY mdocm = ls_mdhdr-mdocm BINARY SEARCH.
*    IF sy-subrc <> 0.
*      CONTINUE.
*    ENDIF.
*    lv_tabix = sy-tabix.
*    LOOP AT lt_mdatv INTO DATA(ls_mdatv) FROM lv_tabix.
*      IF ls_mdatv-mdocm <> ls_mdhdr-mdocm.
*        EXIT.
*      ENDIF.
*
*      READ TABLE lt_fname INTO DATA(ls_fname)
*               WITH KEY atinn = ls_mdatv-atinn.
*      IF sy-subrc <> 0.
*        CONTINUE.
*      ENDIF.
*
*      ASSIGN COMPONENT ls_fname-fieldname OF STRUCTURE <gfs_dyn_smdm>
*       TO <gfs_value>.
*      IF <gfs_value> IS ASSIGNED.
*        IF ls_mdatv-atwrt IS INITIAL.
*          CLEAR lv_data_char.
*          CALL FUNCTION 'CEVA_CONVERT_FLOAT_TO_CHAR'
*            EXPORTING
*              float_imp  = ls_mdatv-atflv
*              format_imp = lv_i
*            IMPORTING
*              char_exp   = lv_data_char.
*
**          lv_packed = ls_mdatv-atflv.
**          ls_mdatv-atwrt = lv_packed.
*          ls_mdatv-atwrt = lv_data_char.
*          CONDENSE ls_mdatv-atwrt NO-GAPS.
*        ENDIF.
*        <gfs_value> = ls_mdatv-atwrt.
*
*        IF ls_fname-atnam = zcl_abs_abap_maintain=>c_charact_contamotivo.  "Reason
*          READ TABLE lt_attdesc INTO DATA(ls_attdesc)
*                                 WITH KEY atinn = ls_fname-atinn
*                                          atwrt = ls_mdatv-atwrt
*                            BINARY SEARCH.
*          IF sy-subrc = 0.
*            UNASSIGN <fs_value>.
*            ASSIGN COMPONENT 'RSDESC' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
*            IF <fs_value> IS ASSIGNED.
*              <fs_value> = ls_attdesc-atwtb.
*            ENDIF.
*          ENDIF.
*
*        ELSEIF ls_fname-atnam = zcl_abs_abap_maintain=>c_charact_atividade. "Inventory Activity
*          READ TABLE lt_actdesc INTO DATA(ls_actdesc)
*                                 WITH KEY ivact = ls_mdatv-atwrt
*                            BINARY SEARCH.
*          IF sy-subrc = 0.
*            UNASSIGN <fs_value>.
*            ASSIGN COMPONENT 'ACTDESC' OF STRUCTURE <gfs_dyn_smdm> TO <fs_value>.
*            IF <fs_value> IS ASSIGNED.
*              <fs_value> = ls_actdesc-descr.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*
*    ENDLOOP.
*
*    APPEND <gfs_dyn_smdm> TO <gfs_dyn_tmdm>.
  ENDLOOP.

ENDFORM.

**&---------------------------------------------------------------------*
**& Form DYNAMIC_FCAT_PREPARE
**&---------------------------------------------------------------------*
**& DYNAMIC_FCAT_PREPARE
**&---------------------------------------------------------------------*
*FORM dynamic_fcat_prepare  USING    pt_ksml  TYPE /agri/t_gksml
*                                    pt_cabn  TYPE tty_cabn_new
*                        CHANGING    pt_fname TYPE tty_fname.
*
**--Local variable declaration
*  DATA : ls_domain_attr TYPE dd01v,
*         lv_col_pos     TYPE i,
*         lv_contr(3)    TYPE c,
*
**--Workarea declaration
*         ls_fname       TYPE ty_fname,
*         ls_fcat        TYPE lvc_s_fcat.
*
**--Field catalog prepare
*  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
*    EXPORTING
*      i_structure_name       = 'ZABS_STR_MD_APROVA_DOC'
**      "zcl_abs_abap_maintain=>c_str_md_mass_approvals
*    CHANGING
*      ct_fieldcat            = gt_fcat
*    EXCEPTIONS
*      inconsistent_interface = 1
*      program_error          = 2
*      OTHERS                 = 3.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
*  LOOP AT gt_fcat INTO ls_fcat WHERE fieldname = 'STSFL'
*                                  OR fieldname = 'STSMA'
*                                  OR fieldname = 'FLSTP'.
*    IF sy-subrc = 0.
*      ls_fcat-no_out = abap_true.
*      MODIFY gt_fcat FROM ls_fcat TRANSPORTING no_out.
*    ENDIF.
*    CLEAR : ls_fcat.
*  ENDLOOP.
*
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
*
*  LOOP AT pt_ksml INTO DATA(ls_ksml).
*
*    DESCRIBE TABLE gt_fcat LINES lv_col_pos.
*
**--Fill Internal Table for Linking Attributes and Dynamic Columns
*    CLEAR ls_cabn.
*    READ TABLE pt_cabn INTO ls_cabn
*      WITH KEY atinn = ls_ksml-imerk BINARY SEARCH.
*    IF sy-subrc <> 0.
*      CONTINUE.
*    ENDIF.
*
**--Append Dynamic Fieldnames to Internal Table
*    ADD 1 TO lv_contr.
*    UNPACK lv_contr
*    TO lv_contr.
*
*    CASE ls_cabn-atnam.
*      WHEN 'CIT-ESPACAMENTO-PES'.
*        ls_fcat-domname = 'ZABS_DOM_MENG30'.
*      WHEN 'CIT-ESPACAMENTO-RUA'.
*        ls_fcat-domname = 'ZABS_DOM_MENG30'.
*      WHEN 'CIT-PORTA-ENXERTO'.
*        ls_fcat-domname = 'ZABS_DOM_PORTA_ENXERTO'.
*      WHEN 'FAZ_FIM_MP_PLANTIO'.
*        ls_fcat-domname = 'ZABS_DOM_PREV_PLANTIO'.
*      WHEN 'FAZ_QUANTIDADE_PLANTAS'.
*        ls_fcat-domname = 'ZABS_DOM_MENG30_0'.
*      WHEN 'FAZ_VAR_MP_TECNICA'.
*        ls_fcat-domname = 'CHAR30'.
*      WHEN 'FAZ_PREV_ERRADICACAO'.
*        ls_fcat-domname = 'ZABS_DOM_PREV_ERRAD'.
*      WHEN 'FAZ_AREA_TALHAO'.
*        ls_fcat-domname = 'ZABS_DOM_MENG30'.
*    ENDCASE.
*
*    IF ls_fcat-domname = 'ZABS_DOM_MENG30'
*    OR ls_fcat-domname = 'ZABS_DOM_PORTA_ENXERTO'
*    OR ls_fcat-domname = 'ZABS_DOM_PREV_PLANTIO'
*    OR ls_fcat-domname = 'ZABS_DOM_MENG30_0'
*    OR ls_fcat-domname = 'CHAR30'
*    OR ls_fcat-domname = 'ZABS_DOM_PREV_ERRAD'.
*      READ TABLE gt_fcat INTO DATA(ls_fcat_ref)
*        WITH KEY domname = ls_fcat-domname.
*      IF sy-subrc EQ 0.
*        MOVE-CORRESPONDING ls_fcat_ref TO ls_fcat.
*        CLEAR: ls_fcat-ref_table, ls_fcat-ref_field.
*      ENDIF.
*    ELSE.
*      IF ls_fcat-datatype IS INITIAL
*      AND ls_fcat-outputlen IS INITIAL.
*        CLEAR ls_domain_attr.
*        CALL FUNCTION 'G_DOMAIN_READ'
*          EXPORTING
*            domain      = ls_fcat-domname
*            langu       = sy-langu
*          IMPORTING
*            domain_attr = ls_domain_attr
*          EXCEPTIONS
*            not_found   = 1
*            OTHERS      = 2.
*        IF sy-subrc EQ 0.
*          ls_fcat-datatype  = ls_domain_attr-datatype.
*          ls_fcat-decimals  = ls_domain_attr-decimals.
*          ls_fcat-outputlen = ls_domain_attr-outputlen.
*          ls_fcat-intlen    = ls_domain_attr-leng.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
**--Dynamic Field Names
*    CONCATENATE 'MDM' lv_contr
*    INTO ls_fcat-fieldname.
**--Append Dynamic Columns to Field Catalog
*    ls_fname-fieldname = ls_fcat-fieldname.
*    ls_fname-atinn     = ls_cabn-atinn.
*    ls_fname-atnam     = ls_cabn-atnam.
**-- Field Catalog
*    IF ls_fcat-intlen IS INITIAL.
*      ls_fcat-intlen = '30'.
*    ENDIF.
*    ls_fcat-col_pos    = lv_col_pos + 1.
*    ls_fcat-scrtext_s  = ls_cabn-atbez.
*    ls_fcat-scrtext_m  = ls_cabn-atbez.
*    ls_fcat-scrtext_l  = ls_cabn-atbez.
*    ls_fcat-reptext    = ls_cabn-atbez.
*    ls_fcat-tooltip    = ls_cabn-atbez.
*    ls_fcat-coltext    = ls_cabn-atbez.
*
*    APPEND ls_fname TO pt_fname.
*    APPEND ls_fcat TO gt_fcat.
*    CLEAR ls_fcat.
*  ENDLOOP.
*
*ENDFORM.

**&---------------------------------------------------------------------*
*& Form SEND_EMAIL
*&---------------------------------------------------------------------*
*& SEND_EMAIL
*&---------------------------------------------------------------------*
FORM send_email .

* *&Get the Email id and User id Whom you want to Send  ******
  DATA:lt_receivers       TYPE STANDARD TABLE OF  somlreci1,
       ls_lt_receivers    LIKE LINE OF lt_receivers,
       lt_packing_list    TYPE STANDARD TABLE OF  sopcklsti1,
       gd_doc_data        TYPE sodocchgi1,
       ls_lt_packing_list LIKE LINE OF  lt_packing_list,
       psubject(90)       TYPE c,
       lt_message         TYPE STANDARD TABLE OF solisti1,
       ls_message         LIKE LINE OF lt_message,
       ls_address         LIKE  bapiaddr3,
       lv_bname           LIKE  bapibname-bapibname,
       lt_bapiret2        TYPE STANDARD TABLE OF bapiret2 INITIAL SIZE 0,
       c1(99)             TYPE c,
       lt_servlist        TYPE TABLE OF icm_sinfo,
       c2(15)             TYPE c,
       num_lines          TYPE i.


  IF gv_send_email IS INITIAL.
    MESSAGE i030(zabs_msgcls).
    LEAVE LIST-PROCESSING.
    EXIT.
  ENDIF.

  LOOP AT gt_tgsfrcp ASSIGNING FIELD-SYMBOL(<fs_tgsfrcp>).


    lv_bname = <fs_tgsfrcp>-procr.

    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = lv_bname
      IMPORTING
        address  = ls_address
      TABLES
        return   = lt_bapiret2.

    " Assign Approver's Email id and User id  -------------&
    FREE ls_lt_receivers.
    ls_lt_receivers-receiver   = ls_address-e_mail. "&---- Assign Email id
    ls_lt_receivers-rec_type   = 'U'.                    "&---- Send to External Email id
    ls_lt_receivers-com_type   = 'INT'.
    ls_lt_receivers-notif_del  = 'X'.
    ls_lt_receivers-notif_ndel = 'X'.
    APPEND ls_lt_receivers TO lt_receivers .
    FREE ls_lt_receivers.
    ls_lt_receivers-receiver   = lv_bname.  "&----- Assign SAP User Id
    ls_lt_receivers-rec_type   = 'B'.                    "&-- Send to SAP Inbox
    ls_lt_receivers-com_type   = 'INT'.
    ls_lt_receivers-notif_del  = 'X'.
    ls_lt_receivers-notif_ndel = 'X'.
    APPEND ls_lt_receivers TO lt_receivers .

  ENDLOOP.

  SORT lt_receivers.
  DELETE ADJACENT DUPLICATES FROM lt_receivers COMPARING ALL FIELDS.

  IF lt_receivers[] IS NOT INITIAL.
*&---------------------------------------------------------------------
* Add thetext to mail text table
*&----------------------------------------------------------------------
*&-- Subject of the mail -------------&*
    psubject = 'Existem documentos pendentes de aprovação'.
    "&--  Body  of the mail ----------------&*
    CLEAR ls_message.
    ls_message-line = 'Sr(a),'.
    APPEND ls_message TO lt_message.
*** insert Blank Line *********************************************
    CLEAR ls_message.
    ls_message-line = '                               '.
    APPEND ls_message TO lt_message.
******* Assign your Text  below *************************************
    CLEAR ls_message.
    ls_message-line = 'Existem documentos de medição pendentes para aprovação do Plano Diretor.'.
    APPEND ls_message TO lt_message.
*** insert Blank Line{} *********************************************
    CLEAR ls_message.
    ls_message-line = '                                        '.
    APPEND ls_message TO lt_message.

*** insert Blank Line{} *********************************************
    CLEAR ls_message.
    ls_message-line = '                                        '.
    APPEND ls_message TO lt_message.

    CALL FUNCTION 'ICM_GET_INFO'
      TABLES
        servlist = lt_servlist.
    IF sy-subrc = 0.
      READ TABLE lt_servlist INTO DATA(ls_serv_list) INDEX 1.
      CLEAR ls_message.
      ls_message-line = '<a href="http://' && ls_serv_list-hostname && ':' && ls_serv_list-service &&
            '/sap/bc/gui/sap/its/webgui?~transaction=*zabs_aprova_doc P_MDTYP=' && p_mdtyp && ';P_MPGRP=' && p_mpgrp && ';P_ETAPA=' && p_etapa
            && '">Clique aqui para Revisar e Aprovar os Documentos</a>'..
      APPEND ls_message TO lt_message.
    ENDIF.

*** insert Blank Line{} *********************************************
    CLEAR ls_message.
    ls_message-line = '                                        '.
    APPEND ls_message TO lt_message.


*** insert Blank Line{} *********************************************
    CLEAR ls_message.
    ls_message-line = '                                        '.
    APPEND ls_message TO lt_message.

*** insert Blank Line{} *********************************************
    CLEAR ls_message.
    ls_message-line = 'Atenciosamente,'.
    APPEND ls_message TO lt_message.

*** insert Blank Line{} *********************************************
    CLEAR ls_message.
    ls_message-line = '                                        '.
    APPEND ls_message TO lt_message.

*** insert Blank Line{} *********************************************
    CLEAR ls_message.
    ls_message-line = 'Equipe de Plantio.'.
    APPEND ls_message TO lt_message.

**********& Send EMAIL MESSAGE  &*********************************
    gd_doc_data-doc_size = 1.
*Populate the subject/generic message attributes
    gd_doc_data-obj_langu = sy-langu.
    gd_doc_data-obj_name = 'SAPRPT'.
    gd_doc_data-obj_descr = psubject.
    gd_doc_data-sensitivty = 'F'.
*Describe the body of the message
    CLEAR ls_lt_packing_list.
    REFRESH lt_packing_list.
    ls_lt_packing_list-transf_bin = space.
    ls_lt_packing_list-head_start = 1.
    ls_lt_packing_list-head_num = 0.
    ls_lt_packing_list-body_start = 1.
    DESCRIBE TABLE lt_message LINES ls_lt_packing_list-body_num.
    ls_lt_packing_list-doc_type = 'RAW'.
    APPEND ls_lt_packing_list TO lt_packing_list.
*&------ Call the Function Module to send the message to External and SAP Inbox
    CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
      EXPORTING
        document_data              = gd_doc_data
        put_in_outbox              = 'X'
        commit_work                = 'X'
      TABLES
        packing_list               = lt_packing_list
        contents_txt               = lt_message
        receivers                  = lt_receivers
      EXCEPTIONS
        too_many_receivers         = 1
        document_not_sent          = 2
        document_type_not_exist    = 3
        operation_no_authorization = 4
        parameter_error            = 5
        x_error                    = 6
        enqueue_error              = 7
        OTHERS                     = 8.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      MESSAGE i000(zabs_msgcls)
        WITH 'E-mails enviados para os aprovadores dos documentos'
                                         'pendentes de aprovação.'.
    ENDIF.
  ENDIF. "&---- END of Check the Sender Email id or SAP User id is got or not.

ENDFORM.

**&---------------------------------------------------------------------*
**& Module STATUS_SET OUTPUT
**&---------------------------------------------------------------------*
**& STATUS_SET
**&---------------------------------------------------------------------*
*MODULE status_set OUTPUT.
*  PERFORM status_set.
*ENDMODULE.
*
**&---------------------------------------------------------------------*
**& Form STATUS_SET
**&---------------------------------------------------------------------*
**& STATUS_SET
**&---------------------------------------------------------------------*
*FORM status_set.
*  SET PF-STATUS 'S100'.
*ENDFORM.
*
**&---------------------------------------------------------------------*
**& Module TITLE_SET OUTPUT
**&---------------------------------------------------------------------*
**& TITLE_SET
**&---------------------------------------------------------------------*
*MODULE title_set OUTPUT.
*  PERFORM title_set.
*ENDMODULE.
*
**&---------------------------------------------------------------------*
**& Form TITLE_SET
**&---------------------------------------------------------------------*
**& TITLE_SET
**&---------------------------------------------------------------------*
*FORM title_set .
*  SET TITLEBAR 'T100'.
*ENDFORM.
*
**&---------------------------------------------------------------------*
**& Module CONTROLS_DISPLAY OUTPUT
**&---------------------------------------------------------------------*
**& Display Controls
**&---------------------------------------------------------------------*
*MODULE controls_display OUTPUT.
*  PERFORM controls_display.
*ENDMODULE.
*
**&---------------------------------------------------------------------*
**& Form CONTROLS_DISPLAY
**&---------------------------------------------------------------------*
**& Display Controls
**&---------------------------------------------------------------------*
*FORM controls_display.
*
**--Local table declaration
*  DATA: lt_fcat    TYPE lvc_t_fcat,
**--Workarea declaration
*        ls_variant TYPE disvariant,
*        ls_stable  TYPE lvc_s_stbl,
*        ls_layout  TYPE lvc_s_layo,
**--Variables
*        lv_valid   TYPE char01,
*        lv_input   TYPE i.
*
**--Set ALV attributes for layout
*  ls_layout-cwidth_opt = abap_true.
*  ls_layout-zebra      = abap_true.
*  ls_layout-sel_mode   = zcl_abs_abap_maintain=>c_layout_sel_mode.  "'A'
*  ls_layout-smalltitle = abap_true.
*
*  IF gobj_alv IS NOT BOUND.
**--Create Object for custom container
*    CREATE OBJECT gobj_cont
*      EXPORTING
*        container_name = 'C_MDMS_APR_0100_CC'.
*
**--Create Object for ALV Grid
*    CREATE OBJECT gobj_alv
*      EXPORTING
*        i_parent = gobj_cont.
*  ENDIF.
*
**--Displaying ALV Data
*  IF gobj_alv IS NOT INITIAL.
*    IF <gfs_dyn_tmdm> IS ASSIGNED.
*      ls_variant-report = sy-repid.
*      CALL METHOD gobj_alv->set_table_for_first_display
*        EXPORTING
*          is_variant                    = ls_variant
*          i_save                        = 'A'
*          is_layout                     = ls_layout
*        CHANGING
*          it_outtab                     = <gfs_dyn_tmdm>
*          it_fieldcatalog               = gt_fcat
*        EXCEPTIONS
*          invalid_parameter_combination = 1
*          program_error                 = 2
*          too_many_lines                = 3
*          OTHERS                        = 4.
*      IF sy-subrc <> 0.
*        MESSAGE i035(zabs_msgcls).
*        LEAVE LIST-PROCESSING.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
*ENDFORM.
*
**&---------------------------------------------------------------------*
**& Module  FCODE_PROCESSING  INPUT
**&---------------------------------------------------------------------*
**& FCODE_PROCESSING
**----------------------------------------------------------------------*
*MODULE fcode_processing INPUT.
*  PERFORM fcode_processing.
*ENDMODULE.
*
**&---------------------------------------------------------------------*
**& Form FCODE_BACK
**&---------------------------------------------------------------------*
**& For Back Button
**&---------------------------------------------------------------------*
*FORM fcode_back.
*  SET SCREEN 0.
*  LEAVE SCREEN.
*ENDFORM.
*
**&---------------------------------------------------------------------*
**& Form FCODE_CANC
**&---------------------------------------------------------------------*
**& For Cancel Button
**&---------------------------------------------------------------------*
*FORM fcode_canc.
*  SET SCREEN 0.
*  LEAVE SCREEN.
*ENDFORM.
*
**&---------------------------------------------------------------------*
**& Form FCODE_EXIT
**&---------------------------------------------------------------------*
**& For Exit Button
**&---------------------------------------------------------------------*
*FORM fcode_exit.
*  SET SCREEN 0.
*  LEAVE SCREEN.
*ENDFORM.
*
**&---------------------------------------------------------------------*
**& Form FCODE_EXIT
**&---------------------------------------------------------------------*
**& For Approvals
**&---------------------------------------------------------------------*
*FORM fcode_approve.
*
*  DATA: lv_soutc TYPE /agri/gsfsoc.
*
*  lv_soutc = 1.
*  PERFORM approve_reject USING lv_soutc.
*
*ENDFORM.
*
**&---------------------------------------------------------------------*
**& Form FCODE_REJECT
**&---------------------------------------------------------------------*
**& For Reject
**&---------------------------------------------------------------------*
*FORM fcode_reject.
*
*  DATA: lv_soutc TYPE /agri/gsfsoc.
*
*  lv_soutc = 2.
*  PERFORM approve_reject USING lv_soutc.
*
*ENDFORM.
*
**&---------------------------------------------------------------------*
**& Form FCODE_REFRESH
**&---------------------------------------------------------------------*
**& For Refresh
**&---------------------------------------------------------------------*
*FORM fcode_refresh.
*
**-- Initializing Global Data
*  PERFORM initialize_global_data.
*
**-- Fetch the mesaurement data
*  PERFORM get_measurement_data.
*
*ENDFORM.
*
**&---------------------------------------------------------------------*
**& Form MESSAGES_DISPLAY
**&---------------------------------------------------------------------*
**& MESSAGES_DISPLAY
**&---------------------------------------------------------------------*
*FORM messages_display  USING lv_initiator TYPE /irm/gdescr.
*
*  DATA: ls_variant TYPE disvariant.
*  ls_variant-report = 'ZABS_INC_MD_MASS_APPROVALS'.
*  ls_variant-handle = lv_initiator.
*
*  messages_display lv_initiator c_true space space ls_variant.
*  messages_init.
*  CLEAR gs_variables-initiator.
*
*ENDFORM.
*
**&---------------------------------------------------------------------*
**& Form APPROVE_REJECT
**&---------------------------------------------------------------------*
**& APPROVE_REJECT
**&---------------------------------------------------------------------*
*FORM approve_reject USING pv_soutc TYPE /agri/gsfsoc.
*
**--Internal table declaration
*  DATA : lt_index_rows TYPE TABLE OF lvc_s_row,
*         lt_msgtab     TYPE esp1_message_tab_type,
*         lt_message    TYPE TABLE OF /agri/s_gprolog,
*         lt_index      TYPE TABLE OF hcp_s_index,
*         lt_mdcom      TYPE TABLE OF ty_msg_out,
*
**--Workarea declaration
*         ls_message    TYPE /agri/s_gprolog,
*         ls_rows       TYPE lvc_s_row,
*         ls_msgtab     TYPE esp1_message_wa_type,
*         ls_index      TYPE hcp_s_index,
*         ls_outcome    TYPE /agri/s_gacoutcome,
*         ls_mdcom      TYPE ty_msg_out,
*
**--Variable declaration
*         lv_mdocm      TYPE /agri/glmdocm.
*
**--Field-symbol declaration
*  FIELD-SYMBOLS :   <fs_value> TYPE any.
*
*  CALL METHOD gobj_alv->get_selected_rows
*    IMPORTING
*      et_index_rows = lt_index_rows
*      et_row_no     = DATA(lt_row_no).
*
*  gs_variables-initiator = c_log_initiator-save.
*  PERFORM messages_initialize USING gs_variables-initiator
*                                    c_log_subobject-save.
*
*  IF lt_index_rows IS INITIAL.
*    MESSAGE  TEXT-005 TYPE zcl_abs_abap_maintain=>c_msgty_info. "'I'
*  ENDIF.
*
*  LOOP AT lt_index_rows INTO ls_rows.
*
*    READ TABLE <gfs_dyn_tmdm> ASSIGNING FIELD-SYMBOL(<fs_mdm>)
*     INDEX ls_rows-index.
*    IF sy-subrc <> 0.
*      CONTINUE.
*    ENDIF.
*
*    READ TABLE gt_mdhdr INTO DATA(ls_mdhdr) INDEX ls_rows-index.
*
*    IF ls_mdhdr-stsma IS NOT INITIAL AND ls_mdhdr-ustat IS NOT INITIAL.
*
*      READ TABLE gt_gsfstp ASSIGNING FIELD-SYMBOL(<lfs_gsfstp>) WITH KEY stsma = ls_mdhdr-stsma
*                                                                         sstat = ls_mdhdr-ustat
*                                                                BINARY SEARCH.
*      IF sy-subrc <> 0.
*        CONTINUE.
*      ENDIF.
*
*      READ TABLE gt_gsfocm ASSIGNING FIELD-SYMBOL(<lfs_gsfocm>) WITH KEY stsma = <lfs_gsfstp>-stsma
*                                                   stsfl = <lfs_gsfstp>-stsfl
*                                                   flstp = <lfs_gsfstp>-flstp
*                                                   soutc = pv_soutc
*                                                   BINARY SEARCH.
*      IF sy-subrc <> 0.
*        CONTINUE.
*      ENDIF.
*
*      IF pv_soutc = '2'.
*        READ TABLE gt_tj30 INTO DATA(ls_tj30) WITH KEY stsma = <lfs_gsfstp>-stsma
*                                                       estat = <lfs_gsfstp>-sstat
*                                                       BINARY SEARCH.
*        IF sy-subrc = 0.
*          IF ls_tj30-inist = abap_true.
*            ls_msgtab-msgid = zcl_abs_abap_maintain=>c_custom_msg_class.
*            ls_msgtab-msgno = '108'.
*            ls_msgtab-msgty = zcl_abs_abap_maintain=>c_msgty_info. "'I'
*            ls_msgtab-msgv1 = ls_mdhdr-mdocm.
*            APPEND ls_msgtab TO lt_msgtab.
*            CONTINUE.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*
*      CLEAR : ls_outcome.
*      REFRESH : lt_message[].
*      ls_outcome-stsfl  = <lfs_gsfocm>-stsfl.
*      ls_outcome-stsma  = gv_stsma. " gw_gsfstp-stsma
*      ls_outcome-flstp  = <lfs_gsfstp>-flstp.
*      ls_outcome-soutc  = pv_soutc.
*
*      PERFORM messages_context_set USING ls_mdhdr-mdocm ."lv_posnr.
*
**--Seting User Status Externally for Measurement Document
*      CALL FUNCTION '/AGRI/GLMD_USER_STATUS_SET'
*        EXPORTING
*          i_mdocm                        = ls_mdhdr-mdocm
*          i_set_inactive                 = ''
*          is_act_outcome                 = ls_outcome
*        IMPORTING
*          et_messages                    = lt_message
*        EXCEPTIONS
*          measurement_document_not_found = 1
*          measurement_document_locked    = 2
*          object_errors                  = 3
*          errors_in_save                 = 4
*          OTHERS                         = 5.
*      IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      ENDIF.
*
*      LOOP AT lt_message INTO ls_message.
*        ls_message-context-context-tabname = zcl_abs_abap_maintain=>c_str_glmd_context. "'ZABS_STR_GLMD_CONTEXT'
*        messages_context_data_set ls_message-context-document
*                                  space
*                                  space
*                                  ls_message-context-context-tabname
*                                  ls_message-context-context-value.
*        MESSAGE ID ls_message-msgid TYPE ls_message-msgty
*           NUMBER ls_message-msgno WITH ls_message-msgv1
*           ls_message-msgv2 ls_message-msgv3 ls_message-msgv4
*                      INTO sy-msgli.
*        message_simple space.
*
*        IF ls_message-msgty = zcl_abs_abap_maintain=>c_msgty_success. "'S'
*          ls_mdcom-mdocm = ls_mdhdr-mdocm.
*          COLLECT ls_mdcom INTO lt_mdcom.
*        ENDIF.
*
*        CLEAR : ls_msgtab, ls_message.
*      ENDLOOP.
*
*    ENDIF.
*    CLEAR : ls_rows.
*  ENDLOOP.
*
*  LOOP AT <gfs_dyn_tmdm> ASSIGNING <fs_mdm>.
*    DATA(lv_tabix) = sy-tabix.
*    ASSIGN COMPONENT 'MDOCM' OF STRUCTURE <fs_mdm> TO <fs_value>.
*    IF <fs_value> IS ASSIGNED.
*      lv_mdocm = <fs_value>.
*      READ TABLE lt_mdcom INTO ls_mdcom WITH KEY mdocm = lv_mdocm.
*      IF sy-subrc = 0.
*        DELETE <gfs_dyn_tmdm> INDEX lv_tabix.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
*
*  PERFORM messages_display USING gs_variables-initiator.
*
*ENDFORM.

*&---------------------------------------------------------------------*
*& Form SELECTION_VALIDATIONS
*&---------------------------------------------------------------------*
*& SELECTION_VALIDATIONS
*&---------------------------------------------------------------------*
FORM selection_validations.

  DATA : lv_atinn TYPE atinn.

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

*--Validating Region
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = zcl_abs_abap_maintain=>c_charact_faz_regiao
    IMPORTING
      output = lv_atinn.

  IF p_region IS NOT INITIAL
  AND lv_atinn IS NOT INITIAL.
    SELECT SINGLE atinn
      FROM cawn
      INTO @DATA(lv_atinn_n)
     WHERE atinn EQ @lv_atinn
       AND atwrt EQ @p_region.
    IF sy-subrc NE 0.
      MESSAGE TEXT-003 TYPE zcl_abs_abap_maintain=>c_msgty_error. "'E'
    ENDIF.
  ENDIF.

ENDFORM.

**&---------------------------------------------------------------------*
**& Form FCODE_PROCESSING
**&---------------------------------------------------------------------*
**& FCODE_PROCESSING
**&---------------------------------------------------------------------*
*FORM fcode_processing.
*
*  DATA : lv_subroutine(40) TYPE c VALUE 'FCODE_'.
*
*  fcode = ok_code.
*  CLEAR: ok_code.
*  CONCATENATE lv_subroutine fcode INTO lv_subroutine.
*  PERFORM (lv_subroutine) IN PROGRAM (sy-repid)
*                          IF FOUND.
*
*ENDFORM.
*
**&---------------------------------------------------------------------*
**& Form MESSAGES_INITIALIZE
**&---------------------------------------------------------------------*
**& MESSAGES_INITIALIZE
**&---------------------------------------------------------------------*
*FORM messages_initialize  USING lv_initiator TYPE /irm/gdescr
*                                lv_subobject TYPE balsubobj.
*
*  messages_init.
*  messages_collect_all.
*  messages_initiator_set lv_initiator c_object-log lv_subobject.
*
*  CREATE OBJECT ref_log_handler.
*
*  message_log_event_handler_set ref_log_handler
*                                on_log_display_profile.
*
*ENDFORM.
*
**&---------------------------------------------------------------------*
**& Form  MESSAGES_CONTEXT_SET
**&---------------------------------------------------------------------*
**& MESSAGES_CONTEXT_SET
**----------------------------------------------------------------------*
*FORM messages_context_set USING lv_mdocm TYPE /agri/glmdocm.
*
*  DATA: lwa_context TYPE  zabs_str_glmd_context.
*
*  IF lv_mdocm IS NOT INITIAL.
*    lwa_context-mdocm = lv_mdocm.
*    messages_context_data_set_new lv_mdocm
*                                  space space
*                                  zcl_abs_abap_maintain=>c_str_glmd_context "'ZABS_STR_GLMD_CONTEXT'
*                                  lwa_context.
*  ENDIF.
*
*ENDFORM.                    " MESSAGES_CONTEXT_SET

*&---------------------------------------------------------------------*
*& Form F4_FOR_REGION
*&---------------------------------------------------------------------*
*& F4_FOR_REGION
*&---------------------------------------------------------------------*
FORM f4_for_region.

*--Local declarations
  DATA: lt_values TYPE TABLE OF cawn,
        lv_value  TYPE atwrt.

  CALL FUNCTION '/AGRI/G_CHARACTERISTIC_F4'
    EXPORTING
      i_atnam            = zcl_abs_abap_maintain=>c_charact_faz_regiao
      i_show_description = abap_true
    IMPORTING
      e_value            = lv_value
    TABLES
      t_values           = lt_values
    EXCEPTIONS
      charact_not_found  = 1
      no_values_found    = 2
      OTHERS             = 3.
*  IF sy-subrc <> 0.
*    MESSAGE TEXT-006 TYPE zcl_abs_abap_maintain=>c_msgty_error.
*  ENDIF.

  p_region = lv_value.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F4_FOR_ETAPA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f4_for_etapa .

  TYPES: BEGIN OF tty_etapa_f4,
           flstp TYPE /agri/gsfflstp,
         END OF tty_etapa_f4.

  DATA: lt_etapa_f4 TYPE TABLE OF tty_etapa_f4,
        lv_numc2(2) TYPE n,
        lt_return   TYPE STANDARD TABLE OF ddshretval.

  DO 6 TIMES.
    DATA(lv_index) = sy-index.
    INSERT INITIAL LINE INTO TABLE lt_etapa_f4
      ASSIGNING FIELD-SYMBOL(<ls_etapa_f4>).
    IF sy-subrc EQ 0.
      lv_numc2 = lv_index.
      <ls_etapa_f4>-flstp = lv_numc2.
    ENDIF.
  ENDDO.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'FLSTP'
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      value_org       = 'S'
    TABLES
      value_tab       = lt_etapa_f4
      return_tab      = lt_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc = 0.
    READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
    IF sy-subrc = 0.
      p_etapa = ls_return-fieldval.
    ENDIF.
  ENDIF.

ENDFORM.
