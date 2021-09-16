************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name       :  ZABS_REP_TERRAIN_DETAILS                        *
* Tcode             :  ZABS_TRN_TERRAIN_DTL                            *
* Created By        :  Chandrakanth Karanam                            *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  11.06.2019                                      *
* TR                :  C4DK903782                                      *
* Version           :  002                                             *
* Description       :  Terrain Details data preparation and display    *
*                      data                                            *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*&---------------------------------------------------------------------*
*& Form BUILD_TERRAIN_DATA
*&---------------------------------------------------------------------*
*& BUILD_TERRAIN_DATA
*&---------------------------------------------------------------------*
FORM build_terrain_data CHANGING lv_subrc TYPE sysubrc.

*--Workarea declaration
  DATA: lwa_terrain_dtls TYPE zabs_str_terrain_dtls,
        lwa_farm_dtls    TYPE ty_farm_dtls,
        lwa_farm_subtot  TYPE zabs_str_farm_subtot,
*--Internal table declaration
        lt_farm_dtls     TYPE TABLE OF ty_farm_dtls,
*--Local variable declaration
        lv_month         TYPE numc2,
        lv_year          TYPE numc4,
        lv_quan          TYPE p DECIMALS 3,
        lv_farm          TYPE zabs_del_farm,
        lv_value         TYPE zabs_del_plant,
        lv_reason        TYPE atinn.

  RANGES: rt_glmdatv FOR /agri/glmdatv-mdocm.

*--Fetching Terrain Header data to get Farm details
  SELECT tplnr_fl, pltxt, strno
    FROM /agri/glflot
    INTO TABLE @DATA(lt_glflot1)
   WHERE tplvl  EQ @zcl_abs_abap_maintain=>c_tplvl_farm "'1'
     AND iwerk  IN @s_werks " Wave 3 change " EQ @p_werks
     AND kfrst  EQ @space
     AND ownshp EQ @zcl_abs_abap_maintain=>c_ownership_own "'OW'
     AND loevm  EQ @space.
  IF sy-subrc NE 0.
    lv_subrc = 4.
    MESSAGE i030(zabs_msgcls).
    LEAVE LIST-PROCESSING.
  ELSE.
    SORT lt_glflot1 BY tplnr_fl.
  ENDIF.

*--Fetching Terrain Attribute Values to get Region
  SELECT a~tplnr_fl, a~atwrt, b~atinn
    FROM /agri/glflatv AS a
    JOIN cabn          AS b
      ON b~atinn = a~atinn
    INTO TABLE @DATA(lt_glflatv)
     FOR ALL ENTRIES IN @lt_glflot1
   WHERE a~tplnr_fl  EQ @lt_glflot1-tplnr_fl
     AND a~class     EQ @zcl_abs_abap_maintain=>c_attrgrp_citimovel "'CIT-IMOVEL'
     AND a~atwrt     IN @s_region " EQ @p_region
     AND b~atnam     EQ @zcl_abs_abap_maintain=>c_charact_faz_regiao. "'FAZ_REGIAO'
  IF sy-subrc EQ 0.
    SORT lt_glflatv BY tplnr_fl.
  ELSE.
    lv_subrc = 4.
    MESSAGE i104(zabs_msgcls).
    LEAVE LIST-PROCESSING.
  ENDIF.

  DATA(lt_glflatv_temp) = lt_glflatv.
  SORT lt_glflatv_temp BY atwrt atinn.
  DELETE ADJACENT DUPLICATES FROM lt_glflatv_temp COMPARING atwrt
                                                            atinn.

*--Fetching Characteristic values to get Region description
  SELECT a~atinn,a~atwrt,
         b~atwtb
    INTO TABLE @DATA(lt_cawn)
    FROM cawn AS a
    JOIN cawnt AS b
      ON b~atinn = a~atinn
     AND b~atzhl = a~atzhl
     FOR ALL ENTRIES IN @lt_glflatv_temp
   WHERE a~atinn EQ @lt_glflatv_temp-atinn
     AND a~atwrt EQ @lt_glflatv_temp-atwrt
     AND b~spras EQ @sy-langu.
  IF sy-subrc = 0.
    SORT lt_cawn BY atwrt.
  ENDIF.

*--Fetching Terrain Header data for 2nd level terrain
  SELECT tplnr_fl,pltxt,tplma,strno
    FROM /agri/glflot
    INTO TABLE @DATA(lt_glflot2)
     FOR ALL ENTRIES IN @lt_glflatv
   WHERE tplnr_fl IN @s_tplnr
     AND tplma    EQ @lt_glflatv-tplnr_fl
     AND kfrst    EQ @space
     AND loevm    EQ @space.
  IF sy-subrc NE 0.
    lv_subrc = 4.
    MESSAGE i105(zabs_msgcls).
    LEAVE LIST-PROCESSING.
  ELSE.
    SORT lt_glflot2 BY tplnr_fl.
  ENDIF.

  lv_month = p_per(2).
  lv_year  = p_per+3.

*--Callinf FM to Determine start and end date of a month
  CALL FUNCTION 'OIL_MONTH_GET_FIRST_LAST'
    EXPORTING
      i_month     = lv_month
      i_year      = lv_year
    IMPORTING
      e_first_day = gv_datab
      e_last_day  = gv_datbi
    EXCEPTIONS
      wrong_date  = 1
      OTHERS      = 2.

*--Fetching Crop Seasons data
  SELECT tplnr_fl,contr
    FROM /agri/glflcma
    INTO TABLE @DATA(lt_glflcma)
     FOR ALL ENTRIES IN @lt_glflot2
   WHERE tplnr_fl  EQ @lt_glflot2-tplnr_fl
     AND ( ( datab GE @gv_datab
     AND datab     LE @gv_datbi )
      OR ( datbi   GE @gv_datab
     AND datab     LE @gv_datbi ) )
     AND astat     EQ @zcl_abs_abap_maintain=>c_cs_active. "'A'
  IF sy-subrc NE 0.
    lv_subrc = 4.
    MESSAGE i030(zabs_msgcls).
    LEAVE LIST-PROCESSING.
  ELSE.
    SORT lt_glflcma BY tplnr_fl contr.
  ENDIF.

*--Fetching Measurement Document Header data
  SELECT mdocm,tplnr_fl,contr
    FROM /agri/glmdhdr
    INTO TABLE @DATA(lt_glmdhdr)
     FOR ALL ENTRIES IN @lt_glflcma
   WHERE mdtyp    EQ @zcl_abs_abap_maintain=>c_mdtyp_zarv "'ZARV'
     AND tplnr_fl EQ @lt_glflcma-tplnr_fl
     AND contr    EQ @lt_glflcma-contr
     AND mdate    GE @gv_datab
     AND mdate    LE @gv_datbi
     AND kfrst    EQ @space.
  IF sy-subrc NE 0.
    lv_subrc = 4.
    MESSAGE i106(zabs_msgcls).
    LEAVE LIST-PROCESSING.
  ELSE.
    SORT lt_glmdhdr BY mdocm.
  ENDIF.

*--Fetching Measurement Document Attribute Values data
  SELECT a~mdocm,a~atinn,
         a~atwrt,a~atflv,
         b~atnam
    FROM /agri/glmdatv AS a
    JOIN cabn AS b
      ON b~atinn = a~atinn
    INTO TABLE @DATA(lt_glmdatv)
     FOR ALL ENTRIES IN @lt_glmdhdr
   WHERE a~mdocm   EQ @lt_glmdhdr-mdocm
     AND ( b~atnam EQ @zcl_abs_abap_maintain=>c_charact_atividade    "'INV-ATIVIDADE'
      OR b~atnam   EQ @zcl_abs_abap_maintain=>c_charact_contamotivo  "'CIT-CONTA-MOTIVO'
      OR b~atnam   EQ @zcl_abs_abap_maintain=>c_charact_qtdadearv ). "'INV-QTDADE-ARV'
  IF sy-subrc NE 0.
    lv_subrc = 4.
    MESSAGE i107(zabs_msgcls).
    LEAVE LIST-PROCESSING.
  ELSE.
    SORT lt_glmdatv BY mdocm atinn.

    rt_glmdatv-sign   = zcl_abs_abap_maintain=>c_rsign_exclude. "'E'
    rt_glmdatv-option = zcl_abs_abap_maintain=>c_ropt_equal.    "'EQ'

    IF rb_lep IS NOT INITIAL.
      LOOP AT lt_glmdatv INTO DATA(lwa_glmdatv).
        IF ( lwa_glmdatv-atnam EQ zcl_abs_abap_maintain=>c_charact_atividade "'INV-ATIVIDADE'
         AND ( lwa_glmdatv-atwrt EQ zcl_abs_abap_maintain=>c_charval_103       "'103'
          OR   lwa_glmdatv-atwrt EQ zcl_abs_abap_maintain=>c_charval_104 ) ).  "'104'
          rt_glmdatv-low    = lwa_glmdatv-mdocm.
          APPEND rt_glmdatv.
        ENDIF.
      ENDLOOP.
    ELSE.
      LOOP AT lt_glmdatv INTO lwa_glmdatv.
        IF ( lwa_glmdatv-atnam EQ zcl_abs_abap_maintain=>c_charact_atividade "'INV-ATIVIDADE'
         AND ( lwa_glmdatv-atwrt EQ zcl_abs_abap_maintain=>c_charval_101       "'101'
          OR   lwa_glmdatv-atwrt EQ zcl_abs_abap_maintain=>c_charval_102 ) ).  "'102'
          rt_glmdatv-low = lwa_glmdatv-mdocm.
          APPEND rt_glmdatv.
        ENDIF.
      ENDLOOP.
    ENDIF. "rb_lep

    IF rt_glmdatv IS NOT INITIAL.
*--Deleting records based on radio button selected
      DELETE lt_glmdatv WHERE mdocm IN rt_glmdatv.
    ENDIF.

    DATA(lt_glmdatv_temp) = lt_glmdatv.
    SORT lt_glmdatv_temp BY atwrt atinn.
    DELETE ADJACENT DUPLICATES FROM lt_glmdatv_temp COMPARING atwrt
                                                              atinn.

    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
      EXPORTING
        input  = zcl_abs_abap_maintain=>c_charact_contamotivo  "'CIT-CONTA-MOTIVO'
      IMPORTING
        output = lv_reason.

*--Fetching Characteristic values to get Reason description
    SELECT a~atinn,a~atwrt,
           b~atwtb
      INTO TABLE @DATA(lt_rsdesc)
      FROM cawn  AS a
     RIGHT JOIN cawnt AS b
        ON b~atinn = a~atinn
       AND b~atzhl = a~atzhl
       FOR ALL ENTRIES IN @lt_glmdatv_temp
     WHERE a~atinn EQ @lv_reason
       AND a~atwrt EQ @lt_glmdatv_temp-atwrt
       AND b~spras EQ @sy-langu.
    IF sy-subrc = 0.
      SORT lt_rsdesc BY atwrt.
    ENDIF.
  ENDIF.

*--Processing the Terrain data
  LOOP AT lt_glmdhdr ASSIGNING FIELD-SYMBOL(<lfs_glmdhdr>).
    CLEAR : lv_quan.

    TRY.
        DATA(lwa_invact) = lt_glmdatv[ mdocm = <lfs_glmdhdr>-mdocm
                                       atnam = zcl_abs_abap_maintain=>c_charact_atividade ]. "'INV-ATIVIDADE'
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
    IF lwa_invact IS INITIAL.
      CONTINUE.
    ENDIF.

    TRY.
        lwa_farm_dtls-reason = lt_glmdatv[ mdocm = lwa_invact-mdocm
                                           atnam = zcl_abs_abap_maintain=>c_charact_contamotivo ]-atwrt. "'CIT-CONTA-MOTIVO'
        lwa_farm_subtot-reason = lt_glmdatv[ mdocm = lwa_invact-mdocm
                                             atnam = zcl_abs_abap_maintain=>c_charact_contamotivo ]-atwrt. "'CIT-CONTA-MOTIVO'
        lwa_farm_subtot-rsdesc = lt_rsdesc[ atwrt = lwa_farm_subtot-reason ]-atwtb. "'CIT-CONTA-MOTIVO'
        lwa_farm_dtls-rsdesc = lt_rsdesc[ atwrt = lwa_farm_subtot-reason ]-atwtb. "'CIT-CONTA-MOTIVO'
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    TRY.
        lv_quan = lt_glmdatv[ mdocm = lwa_invact-mdocm
                              atnam = zcl_abs_abap_maintain=>c_charact_qtdadearv ]-atflv. "'INV-QTDADE-ARV'
        lwa_farm_dtls-qtd_plants = lv_quan.
        lwa_farm_subtot-qtd_plants = lv_quan.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    READ TABLE lt_glflot2 ASSIGNING FIELD-SYMBOL(<lfs_glflot2>)
    WITH KEY tplnr_fl = <lfs_glmdhdr>-tplnr_fl
    BINARY SEARCH.
    IF sy-subrc = 0.
      lwa_farm_dtls-farm  = <lfs_glflot2>-tplma.
      READ TABLE lt_glflot1 INTO DATA(ls_glflot1)
            WITH KEY tplnr_fl = <lfs_glflot2>-tplma
          BINARY SEARCH.
      IF sy-subrc EQ 0.
        lwa_farm_dtls-pltxt = ls_glflot1-pltxt.
      ENDIF.

      READ TABLE lt_glflatv INTO DATA(lwa_glflatv)
      WITH KEY tplnr_fl = <lfs_glflot2>-tplma
      BINARY SEARCH.
      IF sy-subrc = 0.
        lwa_farm_dtls-region = lwa_glflatv-atwrt.
        CONDENSE  lwa_farm_dtls-region.
        IF lwa_farm_dtls-region IS NOT INITIAL.
          COLLECT lwa_farm_dtls INTO lt_farm_dtls.
          MOVE-CORRESPONDING lwa_farm_dtls TO lwa_farm_subtot.

          READ TABLE lt_cawn INTO DATA(ls_cawn)
          WITH KEY atwrt = lwa_farm_dtls-region BINARY SEARCH. " Wave 3
          lwa_farm_subtot-reg_txt = ls_cawn-atwtb.

          COLLECT lwa_farm_subtot INTO gt_farm_subtot.
          TRY.
              gv_regtxt = lt_cawn[ atwrt = lwa_farm_dtls-region ]-atwtb.
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.

*--Filling text based on radio button selected
          IF rb_lep IS NOT INITIAL.
            gv_rbtxt = TEXT-007.
          ELSEIF rb_lpp IS NOT INITIAL.
            gv_rbtxt = TEXT-008.
          ENDIF.
          CLEAR: lwa_farm_dtls,lwa_farm_subtot,lwa_invact.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  DELETE lt_farm_dtls   WHERE reason IS INITIAL.
  DELETE gt_farm_subtot WHERE reason IS INITIAL.

  SORT lt_farm_dtls BY region farm.

*--Processing farm data to get subtotal value
  LOOP AT lt_farm_dtls INTO lwa_farm_dtls.
    IF lv_farm <> lwa_farm_dtls-farm.
      lwa_terrain_dtls-pltxt = lwa_farm_dtls-pltxt.
    ELSE.
      lwa_terrain_dtls-pltxt = space.
    ENDIF.

    MOVE-CORRESPONDING lwa_farm_dtls TO lwa_terrain_dtls.
    APPEND lwa_terrain_dtls TO gt_terrain_dtls.
    lv_value = lv_value + lwa_farm_dtls-qtd_plants.
    AT END OF farm.
      CLEAR: lwa_terrain_dtls-reason,
             lwa_terrain_dtls-rsdesc.
      lwa_terrain_dtls-sub_tot = TEXT-009.  "Subtotal
      lwa_terrain_dtls-qtd_plants  = lv_value.
      lwa_terrain_dtls-pltxt = space.
      APPEND lwa_terrain_dtls TO gt_terrain_dtls.
      CLEAR: lv_value.
    ENDAT.
    lv_farm = lwa_farm_dtls-farm.
    CLEAR: lwa_terrain_dtls,lwa_farm_dtls.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form INITIALIZE_GLOBAL_DATA
*&---------------------------------------------------------------------*
*& INITIALIZE_GLOBAL_DATA
*&---------------------------------------------------------------------*
FORM initialize_global_data.

  REFRESH: gt_terrain_dtls, gt_farm_subtot, gt_att_content.
  CLEAR: gv_subrc.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form TERRAINS_DATA_SFP
*&---------------------------------------------------------------------*
*& TERRAINS_DATA_SFP
*&---------------------------------------------------------------------*
FORM terrains_data_sfp.

*--Local variable declaration
  DATA : lv_fm_name       TYPE rs38l_fnam,
         lv_datab         TYPE char10,
         lv_datbi         TYPE char10,
*--Workarea declaration
         lwa_terrain_dtls TYPE zabs_str_terrain_dtls,
         lfp_docparams    TYPE sfpdocparams,
         lfp_outputparams TYPE sfpoutputparams,
         lfp_formoutput   TYPE fpformoutput.

*--To get FM name to be generated by Adobeform
  CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
    EXPORTING
      i_name     = zcl_abs_abap_maintain=>c_frmname_trndtls "'ZABS_ADBFRM_TERRAIN'.
    IMPORTING
      e_funcname = lv_fm_name.

  lfp_outputparams-nodialog = abap_true.
  lfp_outputparams-getpdf   = abap_true.
  lfp_outputparams-preview  = space.

  CALL FUNCTION 'FP_JOB_OPEN'
    CHANGING
      ie_outputparams = lfp_outputparams
    EXCEPTIONS
      cancel          = 1
      usage_error     = 2
      system_error    = 3
      internal_error  = 4
      OTHERS          = 5.
*-- BOC-T_T.KONNO
*  IF sy-subrc <> 0.
*    CASE sy-subrc.
*      WHEN OTHERS.
*    ENDCASE.                           " CASE sy-subrc
*  ENDIF.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
*-- EOC-T_T.KONNO

  gv_langu = sy-langu.
  WRITE gv_datab USING EDIT MASK '__/__/____' TO lv_datab.
  WRITE gv_datbi USING EDIT MASK '__/__/____' TO lv_datbi.

*--Calling generated Function Module
  CALL FUNCTION lv_fm_name "'/1BCDWB/SM00000149'
    EXPORTING
      /1bcdwb/docparams  = lfp_docparams
      gt_terrain_dtls    = gt_terrain_dtls
      gt_farm_subtot     = gt_farm_subtot
      gv_langu           = gv_langu
      gv_datab           = lv_datab
      gv_datbi           = lv_datbi
      gv_rbtxt           = gv_rbtxt
      gv_regtxt          = gv_regtxt
    IMPORTING
      /1bcdwb/formoutput = lfp_formoutput
    EXCEPTIONS
      usage_error        = 1
      system_error       = 2
      internal_error     = 3
      OTHERS             = 4.
*-- BOC-T_T.KONNO
*  IF sy-subrc <> 0.
*    CASE sy-subrc.
*      WHEN OTHERS.
*    ENDCASE.
*  ENDIF.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
*-- EOC-T_T.KONNO

  CALL FUNCTION 'FP_JOB_CLOSE'
    EXCEPTIONS
      usage_error    = 1
      system_error   = 2
      internal_error = 3
      OTHERS         = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*--Converting PDF to Binary format
  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer     = lfp_formoutput-pdf
    TABLES
      binary_tab = gt_att_content_hex.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DOWNLOAD_PDF
*&---------------------------------------------------------------------*
*& DOWNLOAD_PDF
*&---------------------------------------------------------------------*
FORM download_pdf.

*--Local variable declaration
  DATA: lv_file_path TYPE string.

  CONCATENATE p_path '/' p_per+3(4) p_per(2) '_' s_region-low '.PDF'
    INTO lv_file_path. " due to wave 3 changes added

  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      filename                = lv_file_path
      filetype                = zcl_abs_abap_maintain=>c_filetyp_binary "'BIN'
    CHANGING
      data_tab                = gt_att_content_hex
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      not_supported_by_gui    = 22
      error_no_gui            = 23
      OTHERS                  = 24.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F4_FOR_REGION
*&---------------------------------------------------------------------*
*& F4_FOR_REGION
*&---------------------------------------------------------------------*
FORM f4_for_region.

  DATA: lt_values TYPE TABLE OF cawn.

  CALL FUNCTION '/AGRI/G_CHARACTERISTIC_F4'
    EXPORTING
      i_atnam            = zcl_abs_abap_maintain=>c_charact_faz_regiao "'FAZ_REGIAO'
      i_show_description = abap_true
    IMPORTING
      e_value            = s_region " p_region
    TABLES
      t_values           = lt_values
    EXCEPTIONS
      charact_not_found  = 1
      no_values_found    = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FILE_PATH
*&---------------------------------------------------------------------*
*& FILE_PATH
*&---------------------------------------------------------------------*
FORM file_path.

  CALL METHOD cl_gui_frontend_services=>directory_browse
    CHANGING
      selected_folder      = p_path
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SELECTION_VALIDATIONS
*&---------------------------------------------------------------------*
*& SELECTION_VALIDATIONS
*&---------------------------------------------------------------------*
FORM selection_validations.

  DATA: lv_length TYPE char7.

*--Validating Plant
  IF s_werks[] IS NOT INITIAL. " due to Wave 3 changes added
    SELECT iwerk
      FROM t001w
      INTO TABLE @DATA(lt_plant)
     WHERE werks IN @s_werks. " due to Wave 3 changes added
    IF sy-subrc <> 0.
      MESSAGE TEXT-010 TYPE zcl_abs_abap_maintain=>c_msgty_error. "'E'
    ENDIF.
  ENDIF.

*--Validating Period
  IF p_per IS NOT INITIAL.
    lv_length = strlen( p_per ).
    CONDENSE lv_length.
    IF lv_length LT 7
    OR p_per+2(1) NE '/'.
      MESSAGE TEXT-012 TYPE zcl_abs_abap_maintain=>c_msgty_error. "'E'
    ENDIF.
    IF p_per(2) GT 12.
      MESSAGE TEXT-013 TYPE zcl_abs_abap_maintain=>c_msgty_error. "'E'
    ENDIF.
  ENDIF.

*--Fetching Terrain Header data to get Farm details
  SELECT tplnr_fl
    FROM /agri/glflot
    INTO TABLE @DATA(lt_glflot1)
   WHERE tplvl    EQ @zcl_abs_abap_maintain=>c_tplvl_farm "'1'
     AND iwerk    IN @s_werks " Wave 3 Changes EQ @p_werks
     AND kfrst    EQ @space
     AND ownshp   EQ @zcl_abs_abap_maintain=>c_ownership_own "'OW'
     AND loevm    EQ @space.
  IF sy-subrc EQ 0.
    SORT lt_glflot1 BY tplnr_fl.
*--Fetching Terrain Attribute Values to get Region
    SELECT a~tplnr_fl, a~atwrt,b~atinn
      FROM /agri/glflatv AS a
      JOIN cabn          AS b
        ON b~atinn = a~atinn
      INTO TABLE @DATA(lt_glflatv)
       FOR ALL ENTRIES IN @lt_glflot1
     WHERE a~tplnr_fl  EQ @lt_glflot1-tplnr_fl
       AND a~class     EQ @zcl_abs_abap_maintain=>c_attrgrp_citimovel "'CIT-IMOVEL'
       AND a~atwrt     IN @s_region " Wave 3 changes EQ @p_region
       AND b~atnam     EQ @zcl_abs_abap_maintain=>c_charact_faz_regiao. "'FAZ_REGIAO'
    IF sy-subrc NE 0.
      MESSAGE TEXT-011 TYPE zcl_abs_abap_maintain=>c_msgty_error. "'E'
    ENDIF.
  ENDIF.

ENDFORM.

**&---------------------------------------------------------------------*
**& Form MAIL_ATTACHMENT
**&---------------------------------------------------------------------*
**& MAIL_ATTACHMENT
**&---------------------------------------------------------------------*
*FORM mail_attachment.
*
*  CLASS cl_bcs DEFINITION LOAD.
*
**--Internal table declaration
*  DATA : lt_message_body   TYPE bcsy_text VALUE IS INITIAL,
*
**--Local object declarations
*         lo_send_request   TYPE REF TO cl_bcs VALUE IS INITIAL,
*         lo_document       TYPE REF TO cl_document_bcs VALUE IS INITIAL,
*         lx_document_bcs   TYPE REF TO cx_document_bcs VALUE IS INITIAL,
*         lo_sender         TYPE REF TO if_sender_bcs VALUE IS INITIAL,
*         lo_recipient      TYPE REF TO if_recipient_bcs VALUE IS INITIAL,
*
**--Local variable declaration
*         lv_sent_to_all(1) TYPE c VALUE IS INITIAL.
*
*  lo_send_request = cl_bcs=>create_persistent( ).
*
**--Message body and subject
*  APPEND TEXT-001 TO lt_message_body.
*  APPEND ' ' TO lt_message_body.
*  APPEND TEXT-002 TO lt_message_body.
*  APPEND ' ' TO lt_message_body.
*  APPEND TEXT-003 TO lt_message_body.
*
*  lo_document = cl_document_bcs=>create_document(
*  i_type = TEXT-004
*  i_text = lt_message_body
*  i_subject = TEXT-005 ).
*
*  TRY.
*      lo_document->add_attachment(
*      EXPORTING
*      i_attachment_type = TEXT-006
*      i_attachment_subject = TEXT-005
** I_ATTACHMENT_SIZE =
** I_ATTACHMENT_LANGUAGE = SPACE
** I_ATT_CONTENT_TEXT =
** I_ATTACHMENT_HEADER =
*      i_att_content_hex = gt_att_content_hex ).
*    CATCH cx_document_bcs INTO lx_document_bcs.
*  ENDTRY.
*
**--Add attachment
**--Pass the document to send request
*  lo_send_request->set_document( lo_document ).
*
**--Create Sender
*  lo_sender = cl_sapuser_bcs=>create( sy-uname ).
*
**--Set sender
*  lo_send_request->set_sender(
*  EXPORTING
*  i_sender = lo_sender ).
*
**--Create recipient
*  lo_recipient = cl_cam_address_bcs=>create_internet_address( s_mail-low ).
*
**--Set recipient
*  lo_send_request->add_recipient(
*  EXPORTING
*  i_recipient = lo_recipient
*  i_express = abap_true ).
*
**--Send email
*  lo_send_request->send(
*  EXPORTING
*  i_with_error_screen = abap_true
*  RECEIVING
*  result = lv_sent_to_all ).
*  COMMIT WORK.
*  MESSAGE i073(zabs_msgcls).
*
*ENDFORM.

*&---------------------------------------------------------------------*
*& Form TERRAINS_DATA_SFP_REG
*&---------------------------------------------------------------------*
*& TERRAINS_DATA_SFP_REG
*&---------------------------------------------------------------------*
FORM terrains_data_sfp_reg .

*--Local variable declaration
  DATA: lv_fm_name         TYPE rs38l_fnam,
        lv_datab           TYPE char10,
        lv_datbi           TYPE char10,
*--Workarea declaration
        lwa_terrain_dtls   TYPE zabs_str_terrain_dtls,
        lfp_docparams      TYPE sfpdocparams,
        lfp_outputparams   TYPE sfpoutputparams,
        lfp_formoutput     TYPE fpformoutput,
*-- Local internal table
        lt_terrain_dtls    TYPE tty_terrain_dtls,
        lt_farm_subtot     TYPE zabs_tty_farm_subtot,
        lv_regtxt          TYPE atwtb,
        lt_att_content_hex TYPE solix_tab,
        lt_formoutput      TYPE tfpcontent,
        lv_merged_document TYPE xstring,
        lv_rc              TYPE i,
        lo_pdf_merger      TYPE REF TO cl_rspo_pdf_merge.

*--To get FM name to be generated by Adobeform
  CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
    EXPORTING
      i_name     = zcl_abs_abap_maintain=>c_frmname_trndtls "'ZABS_ADBFRM_TERRAIN'.
    IMPORTING
      e_funcname = lv_fm_name.

  lfp_outputparams-nodialog = abap_true.
  lfp_outputparams-getpdf   = abap_true.
  lfp_outputparams-preview  = space.

  CALL FUNCTION 'FP_JOB_OPEN'
    CHANGING
      ie_outputparams = lfp_outputparams
    EXCEPTIONS
      cancel          = 1
      usage_error     = 2
      system_error    = 3
      internal_error  = 4
      OTHERS          = 5.
*-- BOC-T_T.KONNO
*  IF sy-subrc <> 0.
*    CASE sy-subrc.
*      WHEN OTHERS.
*    ENDCASE.
*  ENDIF.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
*-- EOC-T_T.KONNO

  gv_langu = sy-langu.
  WRITE gv_datab USING EDIT MASK '__/__/____' TO lv_datab.
  WRITE gv_datbi USING EDIT MASK '__/__/____' TO lv_datbi.

  LOOP AT gt_farm_subtot INTO DATA(ls_farm_subtot).
    LOOP AT gt_terrain_dtls INTO DATA(ls_terrain_dtls)
            WHERE region = ls_farm_subtot-region.

      APPEND ls_terrain_dtls TO lt_terrain_dtls.

      AT END OF region.
        APPEND ls_farm_subtot TO lt_farm_subtot.
        lv_regtxt = ls_farm_subtot-reg_txt.

*--Calling generated Function Module
        CALL FUNCTION lv_fm_name "'/1BCDWB/SM00000149'
          EXPORTING
            /1bcdwb/docparams  = lfp_docparams
            gt_terrain_dtls    = lt_terrain_dtls
            gt_farm_subtot     = lt_farm_subtot
            gv_langu           = gv_langu
            gv_datab           = lv_datab
            gv_datbi           = lv_datbi
            gv_rbtxt           = gv_rbtxt
            gv_regtxt          = lv_regtxt
          IMPORTING
            /1bcdwb/formoutput = lfp_formoutput
          EXCEPTIONS
            usage_error        = 1
            system_error       = 2
            internal_error     = 3
            OTHERS             = 4.
*-- BOC-T_T.KONNO
*        IF sy-subrc <> 0.
*          CASE sy-subrc.
*            WHEN OTHERS.
*          ENDCASE.
*        ENDIF.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
*-- EOC-T_T.KONNO

        APPEND lfp_formoutput-pdf TO lt_formoutput.

        REFRESH: lt_terrain_dtls,
                 lt_farm_subtot,
                 lt_att_content_hex.
        CLEAR: lv_regtxt, lfp_formoutput.

      ENDAT.

    ENDLOOP.
  ENDLOOP.

  CALL FUNCTION 'FP_JOB_CLOSE'
    EXCEPTIONS
      usage_error    = 1
      system_error   = 2
      internal_error = 3
      OTHERS         = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*&&-- Merging different PDF files into one
  CREATE OBJECT lo_pdf_merger.

* Add documents to attribute table of PDF merger
  LOOP AT lt_formoutput INTO DATA(lwa_form).
    lo_pdf_merger->add_document( lwa_form ).
  ENDLOOP.

* Call kernel method to do the merge of the specified files.
  lo_pdf_merger->merge_documents( IMPORTING merged_document = lv_merged_document ).
*                                                         rc = lv_rc ).​

  lfp_formoutput-pdf = lv_merged_document.

*--Converting PDF to Binary format
  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer     = lfp_formoutput-pdf
    TABLES
      binary_tab = gt_att_content_hex.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form PARAMETERS_VALIDATIONS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM parameters_validations CHANGING lv_subrc TYPE sysubrc.

  DATA: lv_length TYPE char7.

*-- BOC-T_T.KONNO
  IF p_print EQ abap_true
  AND p_path IS INITIAL.
    lv_subrc = 4.
*-- Informar o diretório para geração do arquivo PDF!
    MESSAGE i317(zfmfp).
    LEAVE LIST-PROCESSING.
  ENDIF.
*-- EOC-T_T.KONNO

*--Validating Plant
  IF s_werks[] IS NOT INITIAL. " due to Wave 3 changes added
    SELECT iwerk
      FROM t001w
      INTO TABLE @DATA(lt_plant)
     WHERE werks IN @s_werks. " due to Wave 3 changes added
    IF sy-subrc <> 0.
      lv_subrc = 4.
*-- Informar Centro válido!
      MESSAGE i318(zfmfp).
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

*--Validating Period
  IF p_per IS NOT INITIAL.
    lv_length = strlen( p_per ).
    CONDENSE lv_length.
    IF lv_length LT 7
    OR p_per+2(1) NE '/'.
      lv_subrc = 4.
*-- O Período deve ser informado no formato MM/AAAA!
      MESSAGE i319(zfmfp).
      LEAVE LIST-PROCESSING.
    ENDIF.
    IF p_per(2) GT 12.
      lv_subrc = 4.
*-- Informe um Período válido!
      MESSAGE i320(zfmfp).
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

*--Fetching Terrain Header data to get Farm details
  SELECT tplnr_fl
    FROM /agri/glflot
    INTO TABLE @DATA(lt_glflot1)
   WHERE tplvl    EQ @zcl_abs_abap_maintain=>c_tplvl_farm "'1'
     AND iwerk    IN @s_werks " Wave 3 Changes EQ @p_werks
     AND kfrst    EQ @space
     AND ownshp   EQ @zcl_abs_abap_maintain=>c_ownership_own "'OW'
     AND loevm    EQ @space.
  IF sy-subrc EQ 0.
    SORT lt_glflot1 BY tplnr_fl.
*--Fetching Terrain Attribute Values to get Region
    SELECT a~tplnr_fl, a~atwrt,b~atinn
      FROM /agri/glflatv AS a
      JOIN cabn          AS b
        ON b~atinn = a~atinn
      INTO TABLE @DATA(lt_glflatv)
       FOR ALL ENTRIES IN @lt_glflot1
     WHERE a~tplnr_fl  EQ @lt_glflot1-tplnr_fl
       AND a~class     EQ @zcl_abs_abap_maintain=>c_attrgrp_citimovel "'CIT-IMOVEL'
       AND a~atwrt     IN @s_region " Wave 3 changes EQ @p_region
       AND b~atnam     EQ @zcl_abs_abap_maintain=>c_charact_faz_regiao. "'FAZ_REGIAO'
    IF sy-subrc NE 0.
      lv_subrc = 4.
*-- Informe uma Região válida!
      MESSAGE i321(zfmfp).
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CREATE_PDF
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_pdf .

*--Local variable declaration
  DATA: lv_file_path TYPE string.

  REFRESH: gt_att_content.

  CONCATENATE p_path '/' p_per+3(4) p_per(2) '_' s_region-low '.PDF'
    INTO lv_file_path. " due to wave 3 changes added

*  CALL METHOD cl_gui_frontend_services=>gui_download
*    EXPORTING
*      filename                = lv_file_path
*      filetype                = zcl_abs_abap_maintain=>c_filetyp_binary "'BIN'
*    CHANGING
*      data_tab                = gt_att_content_hex
*    EXCEPTIONS
*      file_write_error        = 1
*      no_batch                = 2
*      gui_refuse_filetransfer = 3
*      invalid_type            = 4
*      no_authority            = 5
*      unknown_error           = 6
*      header_not_allowed      = 7
*      separator_not_allowed   = 8
*      filesize_not_allowed    = 9
*      header_too_long         = 10
*      dp_error_create         = 11
*      dp_error_send           = 12
*      dp_error_write          = 13
*      unknown_dp_error        = 14
*      access_denied           = 15
*      dp_out_of_memory        = 16
*      disk_full               = 17
*      dp_timeout              = 18
*      file_not_found          = 19
*      dataprovider_exception  = 20
*      control_flush_error     = 21
*      not_supported_by_gui    = 22
*      error_no_gui            = 23
*      OTHERS                  = 24.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.

ENDFORM.
