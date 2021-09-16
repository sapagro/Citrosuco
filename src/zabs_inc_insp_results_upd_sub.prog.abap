**&---------------------------------------------------------------------*
**& Include          ZABS_REP_INSPREC_UPDATE_SUB
**&---------------------------------------------------------------------*
*
**&---------------------------------------------------------------------*
**& Form INSPREC_UPDATE
**&---------------------------------------------------------------------*
**& INSPREC_UPDATE
**&---------------------------------------------------------------------*
FORM insprec_update.

  DATA: lt_qchar_hdr TYPE zabs_tty_qchar_hdr,
        lt_qapp      TYPE qapptab,
        ls_qchar_hdr TYPE zabs_qchar_hdr.

  REFRESH gt_qchar_hdr.

  lt_qchar_hdr = p_qchdr.

  IF lt_qchar_hdr IS INITIAL.
    SELECT *
      FROM zabs_qchar_hdr
      INTO TABLE lt_qchar_hdr
     WHERE insplot    IN s_inslot
       AND inspoper   IN s_insopr
       AND dttimstamp IN s_dtstmp
       AND inspdate   IN s_insdt
       AND recflag    EQ space
       AND ernam      IN s_musr
       AND erdat      IN s_erdat
       AND erzet      IN s_erzet.

    IF sy-subrc <> 0.
      MESSAGE TEXT-004 TYPE zcl_abs_abap_maintain=>c_msgty_info.
      LEAVE LIST-PROCESSING.
    ELSE.
      gt_qchar_hdr[] = lt_qchar_hdr[].
    ENDIF.

    DELETE lt_qchar_hdr WHERE batch EQ 'B'.
    IF lt_qchar_hdr IS INITIAL.
      MESSAGE TEXT-007 TYPE zcl_abs_abap_maintain=>c_msgty_info
                       DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_error.
      LEAVE LIST-PROCESSING.
    ENDIF.

    SORT lt_qchar_hdr BY cflag ASCENDING.

    IF p_par IS NOT INITIAL.
      ls_qchar_hdr-batch = 'B'.
      ls_qchar_hdr-aenam = sy-uname.
      ls_qchar_hdr-aedat = sy-datum.
      ls_qchar_hdr-aezet = sy-uzeit.
      MODIFY lt_qchar_hdr FROM ls_qchar_hdr TRANSPORTING batch aenam aedat aezet
      WHERE insplot NE space.
      MODIFY zabs_qchar_hdr FROM TABLE lt_qchar_hdr.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

  IF lt_qchar_hdr IS NOT INITIAL
  AND p_par IS INITIAL.
    SORT lt_qchar_hdr BY insplot inspoper dttimstamp.
    SELECT *
      FROM zabs_qchar_itm
      FOR ALL ENTRIES IN @lt_qchar_hdr
     WHERE insplot    EQ @lt_qchar_hdr-insplot
       AND inspoper   EQ @lt_qchar_hdr-inspoper
       AND dttimstamp EQ @lt_qchar_hdr-dttimstamp
      INTO TABLE @DATA(lt_qchar_itm).
    IF sy-subrc = 0.
      SORT lt_qchar_itm BY insplot inspoper dttimstamp.
    ENDIF.

*--Fetching Error Log data to check if the record is in the table with error
    SELECT *
      FROM zabs_qchar_erlog
      INTO TABLE @DATA(lt_qchar_errorlog)
       FOR ALL ENTRIES IN @lt_qchar_hdr
     WHERE insplot    EQ @lt_qchar_hdr-insplot
       AND inspoper   EQ @lt_qchar_hdr-inspoper
       AND dttimstamp EQ @lt_qchar_hdr-dttimstamp
       AND ernam      IN @s_musr
       AND erdat      IN @s_erdat
       AND erzet      IN @s_erzet.
    IF sy-subrc = 0.
      SORT lt_qchar_errorlog BY insplot inspoper dttimstamp.
    ENDIF.
  ENDIF.

  SORT lt_qchar_hdr BY insplot inspoper dttimstamp.

  IF lt_qchar_hdr[] IS NOT INITIAL
  AND p_par IS NOT INITIAL.
    SELECT *
      FROM qapp
      INTO TABLE @lt_qapp
      FOR ALL ENTRIES IN @lt_qchar_hdr
     WHERE prueflos EQ @lt_qchar_hdr-insplot.
  ENDIF.

  IF p_par IS NOT INITIAL.
    PERFORM execute_parallel_processing USING lt_qchar_hdr
                                              lt_qapp.
    MESSAGE TEXT-006 TYPE zcl_abs_abap_maintain=>c_msgty_success.
  ELSE.
    PERFORM result_recording USING lt_qchar_hdr
                                   lt_qchar_itm
                                   lt_qchar_errorlog.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form INSPECTION_LOT_FLAG_CHECK
*&---------------------------------------------------------------------*
*& INSPECTION_LOT_FLAG_CHECK
*&---------------------------------------------------------------------*
FORM inspection_lot_flag_check USING    pv_insplot  TYPE qals-prueflos
                                        pv_inspoper TYPE qapo-vornr
                               CHANGING ps_messages TYPE /agri/s_gprolog.

  TYPES: BEGIN OF lty_cvornr,
           cvornr TYPE zabs_cvornr,
         END OF lty_cvornr.

  DATA: lt_cvornr   TYPE TABLE OF lty_cvornr,
        lwa_mob_lot TYPE zabs_mob_ilot.

  CHECK pv_insplot IS NOT INITIAL.

  SELECT SINGLE *
    FROM zabs_mob_ilot
    INTO @lwa_mob_lot
   WHERE prueflos = @pv_insplot.
  IF sy-subrc = 0.
    IF lwa_mob_lot-zzvornr IS NOT INITIAL.
      SPLIT lwa_mob_lot-zzvornr AT ';' INTO TABLE lt_cvornr.
      SORT lt_cvornr BY cvornr.
      READ TABLE lt_cvornr TRANSPORTING NO FIELDS
        WITH KEY cvornr = pv_inspoper BINARY SEARCH.
      IF sy-subrc = 0.
        ps_messages-msgty = 'E'.
        ps_messages-msgv1 = TEXT-005.                       "#EC NOTEXT
        ps_messages-msgv2 = TEXT-008.                       "#EC NOTEXT
        ps_messages-msgid = '00'.
        ps_messages-msgno = '208'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form INSPPOINTS_DATA_MAP
*&---------------------------------------------------------------------*
*& INSPPOINTS_DATA_MAP
*&---------------------------------------------------------------------*
FORM insppoints_data_map  USING    pv_insplot     TYPE qals-prueflos
                                   pv_inspoper    TYPE qapo-vornr
                                   pv_pernr       TYPE p_pernr
                                   pv_inspdate    TYPE datum
                                   pv_insptime    TYPE uzeit
                                   pv_slwbez      TYPE qslwbez
                                   pv_tplnrfl     TYPE /agri/gltplnr_fl
                                   pv_strno       TYPE /agri/glstrno
                                   pv_rbnr1       TYPE zabs_del_setor
                                   pv_isample_z28 TYPE qibpppktnr
                                   lt_insppoints  TYPE /agri/t_fmbapi2045l4
                          CHANGING ps_inspoint    TYPE bapi2045l4.

  DATA: lv_fterra TYPE c LENGTH 6,
        lv_lterra TYPE string,
        lv_length TYPE i.

  IF strlen( pv_strno ) <= 6.
    lv_lterra = pv_strno(6).
  ELSE.
    lv_length = strlen( pv_strno ) - 6.
    lv_lterra = pv_strno+lv_length(6).
  ENDIF.

  REPLACE ALL OCCURRENCES OF REGEX '[.,;:-]' IN lv_lterra WITH space.

  lv_fterra = pv_strno.

  CASE pv_slwbez.
    WHEN 'Z23'.
      WAIT UP TO 2 SECONDS.
      ps_inspoint-userc1 = pv_pernr.
      ps_inspoint-userc2 = lv_fterra.
      ps_inspoint-usern1 = lv_lterra.
      ps_inspoint-usern2 = pv_rbnr1.
      ps_inspoint-userd1 = pv_inspdate.
      ps_inspoint-usert1 = pv_insptime.
    WHEN 'Z24'.

    WHEN 'Z25'.

    WHEN 'Z26' OR 'Z27'.

    WHEN 'Z28'.

*-- BOC - 25/11/2020 By Umakanth
*      SELECT SINGLE a~plnkn
*        FROM afvc AS a
*        INNER JOIN qals AS b
*        ON a~aufpl = b~aufpl
*        INTO @DATA(lv_plnkn)
*       WHERE b~prueflos = @pv_insplot
*         AND a~vornr    = @pv_inspoper.
*
*      SELECT SINGLE *
*        FROM qapp
*        INTO @DATA(ls_qapp)
*       WHERE prueflos = @pv_insplot
*         AND vorglfnr = @lv_plnkn.
*
*      ps_inspoint = CORRESPONDING #( ls_qapp MAPPING  insplot   = prueflos
*                                                      insppoint = probenr ).
*      ps_inspoint-insplot   = pv_insplot.
*      ps_inspoint-inspoper  = pv_inspoper.
*      ps_inspoint-insppoint = ls_qapp-probenr.
*      ps_inspoint-usern2    = pv_rbnr1.
*      ps_inspoint-userd1    = pv_inspdate.
*      ps_inspoint-usert1    = pv_insptime.


      READ TABLE lt_insppoints INTO DATA(ls_insppoint) INDEX 1.
      IF sy-subrc = 0.
        ps_inspoint-userc1 = ls_insppoint-userc1.
        ps_inspoint-userc2 = ls_insppoint-userc2.
        ps_inspoint-usern1 = ls_insppoint-usern1.
      ENDIF.
      ps_inspoint-usern2 = pv_rbnr1.
      ps_inspoint-userd1 = pv_inspdate.
      ps_inspoint-usert1 = pv_insptime.
*-- EOC - 25/11/2020 By Umakanth
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form INSPECTION_LOT_FLAG_UPDATE
*&---------------------------------------------------------------------*
*& INSPECTION_LOT_FLAG_UPDATE
*&---------------------------------------------------------------------*
FORM inspection_lot_flag_update USING    ps_qchar_hdr TYPE zabs_qchar_hdr
                                         pv_insplot   TYPE qals-prueflos
                                         pv_inspoper  TYPE qapo-vornr
                                CHANGING ps_messages  TYPE /agri/s_gprolog.

  TYPES: BEGIN OF lty_cvornr,
           cvornr TYPE zabs_cvornr,
         END OF lty_cvornr.

  DATA: lt_cvornr    TYPE TABLE OF lty_cvornr,
        lt_oper_list TYPE TABLE OF bapi2045l2,
        lt_constants TYPE zabs_tty_vkey_const,
        lv_tabix     TYPE sytabix,
        lwa_mob_lot  TYPE zabs_mob_ilot.

  CHECK pv_insplot IS NOT INITIAL.

  DATA(lv_prueflos) = pv_insplot.

  SELECT SINGLE prueflos, aufnr
    FROM qals
    INTO @DATA(ls_qals)
    WHERE prueflos = @lv_prueflos.
  IF sy-subrc = 0.
    SELECT SINGLE aufnr, aufpl
     INTO @DATA(ls_afko)
     FROM afko
     WHERE aufnr = @ls_qals-aufnr.
  ENDIF.

*--Get variant table data
  CALL METHOD zcl_abs_get_variants=>get_constant_multiple
    EXPORTING
      iv_mod       = 'C'
      iv_objid     = 'MOBL'
      iv_k1val     = 'INSPOP'
      iv_k2val     = 'STEUS'
    IMPORTING
      et_constants = lt_constants.

  DATA lr_steus TYPE RANGE OF steus.
  lr_steus = VALUE #( FOR ls_steust IN lt_constants (
                      sign   = 'I'
                      option = 'EQ'
                      low    = ls_steust-cnval1 ) ).
  DELETE lr_steus WHERE low IS INITIAL.
  SORT lr_steus BY low.
  DELETE ADJACENT DUPLICATES FROM lr_steus COMPARING low.

  SELECT aufpl, plnty, steus, vornr
    INTO TABLE @DATA(lt_afvc)
    FROM afvc
   WHERE aufpl = @ls_afko-aufpl
     AND steus IN @lr_steus.

*--Fetching operations for inspection lot.
  CALL FUNCTION 'BAPI_INSPLOT_GETOPERATIONS'
    EXPORTING
      number        = pv_insplot
    TABLES
      inspoper_list = lt_oper_list.

  SELECT SINGLE * FROM zabs_mob_ilot
    INTO lwa_mob_lot
   WHERE prueflos EQ pv_insplot.
  IF sy-subrc = 0.
*-- JOBREGON: Start of modification
    IF lwa_mob_lot-zzvornr IS NOT INITIAL.
      SPLIT lwa_mob_lot-zzvornr AT ';' INTO TABLE lt_cvornr.
      SORT lt_cvornr BY cvornr.
      READ TABLE lt_cvornr TRANSPORTING NO FIELDS
                           WITH KEY cvornr = pv_inspoper
                           BINARY SEARCH.
      IF sy-subrc <>  0.
        CONCATENATE lwa_mob_lot-zzvornr ';' pv_inspoper INTO lwa_mob_lot-zzvornr.
      ENDIF.
    ELSE.
      lwa_mob_lot-zzvornr = pv_inspoper.
    ENDIF.

    REFRESH lt_cvornr.
    SPLIT lwa_mob_lot-zzvornr AT ';' INTO TABLE lt_cvornr.
    LOOP AT lt_afvc ASSIGNING FIELD-SYMBOL(<fs_afvc>).
      lv_tabix = sy-tabix.
      READ TABLE lt_cvornr TRANSPORTING NO FIELDS
                           WITH KEY cvornr = <fs_afvc>-vornr.
      IF sy-subrc = 0.
        IF lv_tabix = lines( lt_afvc ).
          lwa_mob_lot-zzcflag = abap_true.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ENDLOOP.
*-- JOBREGON: End of modification

    lwa_mob_lot-prueflos = pv_insplot.
*    lwa_mob_lot-zzcflag  = 'X'.
    lwa_mob_lot-aenam    = sy-uname.
    lwa_mob_lot-aedat    = sy-datum.
    lwa_mob_lot-aezet    = sy-uzeit.

    UPDATE zabs_mob_ilot FROM lwa_mob_lot.
  ELSE.
    lwa_mob_lot-zzvornr  = pv_inspoper.

*-- JOBREGON: Start of modification
    REFRESH lt_cvornr.
    APPEND pv_inspoper TO lt_cvornr.
    LOOP AT lt_oper_list ASSIGNING FIELD-SYMBOL(<fs_oper_listt>).
      lv_tabix = sy-tabix.
      READ TABLE lt_cvornr TRANSPORTING NO FIELDS
                           WITH KEY cvornr = <fs_oper_listt>-inspoper.
      IF sy-subrc = 0.
        IF lv_tabix = lines( lt_oper_list ).
          lwa_mob_lot-zzcflag = abap_true.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ENDLOOP.
*-- JOBREGON: End of modification

    lwa_mob_lot-prueflos = pv_insplot.
*    lwa_mob_lot-zzcflag  = 'X'.
    lwa_mob_lot-ernam    = sy-uname.
    lwa_mob_lot-erdat    = sy-datum.
    lwa_mob_lot-erzet    = sy-uzeit.
    INSERT zabs_mob_ilot FROM lwa_mob_lot.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form INITIALIZE_GLOBAL_DATA
*&---------------------------------------------------------------------*
*& INITIALIZE_GLOBAL_DATA
*&---------------------------------------------------------------------*
FORM initialize_global_data.
  REFRESH : gt_qchar_elog, gt_str_qcharmlog.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_DATA
*&---------------------------------------------------------------------*
*& DISPLAY_DATA
*&---------------------------------------------------------------------*
FORM display_data.

  TYPE-POOLS: slis.

  DATA: lt_fcat   TYPE slis_t_fieldcat_alv,
        ls_fcat   TYPE slis_fieldcat_alv,
        ls_layout TYPE slis_layout_alv.

  ls_layout-colwidth_optimize = abap_true.
  ls_layout-zebra             = abap_true.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-repid
*     i_internal_tabname     = 'TABI'
      i_structure_name       = 'ZABS_STR_QCHARMLOG'
*     i_inclname             = sy-repid
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

  IF lt_fcat IS NOT INITIAL.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        is_layout   = ls_layout
        it_fieldcat = lt_fcat
      TABLES
        t_outtab    = gt_str_qcharmlog.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form RESULT_RECORDING
*&---------------------------------------------------------------------*
*& RESULT_RECORDING
*&---------------------------------------------------------------------*
FORM result_recording USING pt_qchar_hdr      TYPE zabs_tty_qchar_hdr
                            pt_qchar_itm      TYPE tty_qchar_itm
                            pt_qchar_errorlog TYPE tty_qchar_elog.

  DATA:
*-- Tables
    lt_return        TYPE TABLE OF bapiret2,
    lt_char_result   TYPE TABLE OF bapi2045d2,
    lt_sample_result TYPE TABLE OF bapi2045d3,
    lt_charres       TYPE TABLE OF bapi2045d2,
    lt_singres       TYPE TABLE OF bapi2045d4,
    lt_sampres       TYPE TABLE OF bapi2045d3,
    lt_char_req_tmp  TYPE TABLE OF bapi2045d1,
    lt_insppoints    TYPE TABLE OF bapi2045l4,
    lt_mobqapp_rec   TYPE TABLE OF zabs_mobqapp_rec,
    lt_messages      TYPE /agri/t_gprolog,
    lt_qchar_elog    TYPE TABLE OF zabs_qchar_erlog,
    lt_str_qcharmlog TYPE TABLE OF zabs_str_qcharmlog,
*-- Structures
    ls_qchar_elog    TYPE zabs_qchar_erlog,
    ls_str_qcharmlog TYPE zabs_str_qcharmlog,
    ls_return        TYPE bapiret2,
    ls_char_result   TYPE bapi2045d2,
    ls_sample_result TYPE bapi2045d3,
    ls_inspoint      TYPE bapi2045l4,
    ls_messages      TYPE /agri/s_gprolog,
*-- Variables
    lv_insplot       TYPE qals-prueflos,
    lv_inspoper      TYPE qapo-vornr,
    lv_inspchar      TYPE qamv-merknr,
    lv_isample_z28   TYPE qibpppktnr,
    lv_slwbez        TYPE qslwbez,
    lv_tplnrfl       TYPE /agri/gltplnr_fl,
    lv_strno         TYPE /agri/glstrno,
    lv_rbnr1         TYPE zabs_del_setor,
    lv_insppoint     TYPE char1,
    ex_t_rettab      TYPE bapiret2_t,
    ch_s_insppoint   TYPE bapi2045l4,
    lv_pernr         TYPE p_pernr,
    lv_inspdate      TYPE datum,
    lv_insptime      TYPE uzeit.

  IF pt_qchar_hdr IS NOT INITIAL.
    SELECT *
      FROM zabs_qchar_hdr
      INTO TABLE @DATA(lt_qchar_hdr)
      FOR ALL ENTRIES IN @pt_qchar_hdr
     WHERE insplot  EQ @pt_qchar_hdr-insplot
       AND inspoper EQ @pt_qchar_hdr-inspoper
       AND cflag    EQ @abap_true.

    SORT lt_qchar_hdr BY insplot inspoper.
  ENDIF.

  DATA(lt_qapp_submit) = p_qapp[].

  LOOP AT pt_qchar_hdr ASSIGNING FIELD-SYMBOL(<lfs_qchar_hdr>).
    DATA(lv_tabix_hdr) = sy-tabix.

    "READ TABLE lt_qapp_submit INTO DATA(ls_qapp_submit) INDEX lv_tabix_hdr.
    READ TABLE lt_qapp_submit INTO DATA(ls_qapp_submit)
      WITH KEY prueflos = <lfs_qchar_hdr>-insplot.
    IF sy-subrc NE 0.
      CLEAR ls_qapp_submit.
    ELSE.
      DELETE lt_qapp_submit INDEX lv_tabix_hdr.
    ENDIF.

    <lfs_qchar_hdr>-recflag = abap_true.
    <lfs_qchar_hdr>-batch   = space.
    <lfs_qchar_hdr>-aenam   = sy-uname.
    <lfs_qchar_hdr>-aedat   = sy-datum.
    <lfs_qchar_hdr>-aezet   = sy-uzeit.

*---Record is already processed and sitting in Error Log.
    READ TABLE pt_qchar_errorlog INTO DATA(ls_qchar_errorlog)
      WITH KEY insplot    = <lfs_qchar_hdr>-insplot
               inspoper   = <lfs_qchar_hdr>-inspoper
               dttimstamp = <lfs_qchar_hdr>-dttimstamp BINARY SEARCH.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.

    lv_insplot     = <lfs_qchar_hdr>-insplot.
    lv_inspoper    = <lfs_qchar_hdr>-inspoper.
    lv_slwbez      = <lfs_qchar_hdr>-slwbez.
    lv_tplnrfl     = <lfs_qchar_hdr>-tplnr_fl.
    lv_strno       = <lfs_qchar_hdr>-strno.
    lv_rbnr1       = <lfs_qchar_hdr>-rbnr1.
    lv_insppoint   = <lfs_qchar_hdr>-insp_point.
    lv_isample_z28 = <lfs_qchar_hdr>-inspsample.

    ls_qchar_elog-insplot    = <lfs_qchar_hdr>-insplot.
    ls_qchar_elog-inspoper   = <lfs_qchar_hdr>-inspoper.
    ls_qchar_elog-dttimstamp = <lfs_qchar_hdr>-dttimstamp.
    ls_qchar_elog-pernr      = <lfs_qchar_hdr>-pernr.

    ls_str_qcharmlog-insplot    = <lfs_qchar_hdr>-insplot.
    ls_str_qcharmlog-inspoper   = <lfs_qchar_hdr>-inspoper.
    ls_str_qcharmlog-dttimstamp = <lfs_qchar_hdr>-dttimstamp.
    ls_str_qcharmlog-pernr      = <lfs_qchar_hdr>-pernr.

    IF <lfs_qchar_hdr>-cflag EQ space.
      READ TABLE lt_qchar_hdr INTO DATA(ls_qchar_hdr_c)
        WITH KEY insplot  = <lfs_qchar_hdr>-insplot
                 inspoper = <lfs_qchar_hdr>-inspoper BINARY SEARCH.
      IF sy-subrc EQ 0.
        IF ls_qchar_hdr_c-dttimstamp LE <lfs_qchar_hdr>-dttimstamp.
          PERFORM check_inspection_lot_flag USING lv_insplot
                                                  lv_inspoper
                                         CHANGING ls_messages.
        ENDIF.
      ENDIF.
    ELSE.
      PERFORM check_inspection_lot_flag USING lv_insplot
                                              lv_inspoper
                                     CHANGING ls_messages.
    ENDIF.

    IF ls_messages IS NOT INITIAL.
      APPEND ls_messages TO lt_messages.

      CONCATENATE ls_messages-msgv1 ls_messages-msgv2
                  ls_messages-msgv3 ls_messages-msgv4
        INTO ls_qchar_elog-msgtxt SEPARATED BY space.

      ls_qchar_elog-msgid     = ls_messages-msgid.
      ls_qchar_elog-msgno     = ls_messages-msgno.
      ls_qchar_elog-msgtyp    = ls_messages-msgty.
      ls_str_qcharmlog-msgid  = ls_messages-msgid.
      ls_str_qcharmlog-msgno  = ls_messages-msgno.
      ls_str_qcharmlog-msgtyp = ls_messages-msgty.
      ls_str_qcharmlog-msgtxt = ls_qchar_elog-msgtxt.

      IF ls_qchar_elog-msgtyp EQ zcl_abs_abap_maintain=>c_msgty_error. "'E'
        APPEND ls_qchar_elog TO gt_qchar_elog.
      ENDIF.

      APPEND ls_str_qcharmlog TO gt_str_qcharmlog.
      CLEAR : ls_qchar_elog-msgid, ls_qchar_elog-msgno, ls_qchar_elog-msgtyp,
              ls_qchar_elog-msgtxt,ls_str_qcharmlog-msgid, ls_str_qcharmlog-msgno,
              ls_str_qcharmlog-msgtyp, ls_str_qcharmlog-msgtxt, ls_messages.
      CONTINUE.
    ENDIF.

    CALL FUNCTION 'BAPI_INSPOPER_GETDETAIL'
      EXPORTING
        insplot                = lv_insplot
        inspoper               = lv_inspoper
        read_insppoints        = abap_true
        read_char_requirements = abap_true
        read_char_results      = abap_true
        read_sample_results    = abap_true
        read_single_results    = abap_true
        max_insppoints         = 1000
      IMPORTING
        return                 = ls_return
      TABLES
        insppoints             = lt_insppoints
        char_requirements      = lt_char_req_tmp
        sample_results         = lt_sampres
        char_results           = lt_charres
        single_results         = lt_singres.

    SORT lt_char_req_tmp BY insplot inspoper inspchar.

    IF ls_return-id IS NOT INITIAL
    AND ls_return-type IS NOT INITIAL.
      ls_str_qcharmlog-msgid  = ls_return-id.
      ls_str_qcharmlog-msgno  = ls_return-number.
      ls_str_qcharmlog-msgtyp = ls_return-type.
      ls_str_qcharmlog-msgtxt = ls_return-message.
      APPEND ls_str_qcharmlog TO gt_str_qcharmlog.
      CLEAR : ls_str_qcharmlog-msgid,
              ls_str_qcharmlog-msgno,
              ls_str_qcharmlog-msgtyp,
              ls_str_qcharmlog-msgtxt.
    ENDIF.

    IF ls_return-type = zcl_abs_abap_maintain=>c_msgty_error.
      ls_qchar_elog-msgid  = ls_return-id.
      ls_qchar_elog-msgno  = ls_return-number.
      ls_qchar_elog-msgtyp = ls_return-type.
      ls_qchar_elog-msgtxt = ls_return-message.

      APPEND ls_qchar_elog TO gt_qchar_elog.

      CLEAR : ls_qchar_elog-msgid,
              ls_qchar_elog-msgno,
              ls_qchar_elog-msgtyp,
              ls_qchar_elog-msgtxt.

      CONTINUE.
    ENDIF.

*    SORT lt_insppoints BY insplot inspoper insppoint DESCENDING.



    READ TABLE pt_qchar_itm TRANSPORTING NO FIELDS
      WITH KEY insplot    = <lfs_qchar_hdr>-insplot
               inspoper   = <lfs_qchar_hdr>-inspoper
               dttimstamp = <lfs_qchar_hdr>-dttimstamp BINARY SEARCH.
    IF sy-subrc EQ 0.
      DATA(lv_tabix) = sy-tabix.
      LOOP AT pt_qchar_itm INTO DATA(ls_qchar_itm) FROM lv_tabix.
        IF ls_qchar_itm-dttimstamp NE <lfs_qchar_hdr>-dttimstamp.
          EXIT.
        ENDIF.

        ls_char_result-insplot      = ls_qchar_itm-insplot.
        ls_char_result-inspoper     = ls_qchar_itm-inspoper.
        ls_char_result-inspchar     = ls_qchar_itm-inspchar.
        lv_inspchar                 = ls_qchar_itm-inspchar.
        ls_sample_result-insplot    = lv_insplot.
        ls_sample_result-inspoper   = lv_inspoper.

        DATA(lv_inspsample) = ls_qapp_submit-probenr.

        ls_sample_result-insplot     = lv_insplot.
        ls_sample_result-inspoper    = lv_inspoper.
        ls_sample_result-closed      = abap_true.
        ls_sample_result-evaluated   = abap_true.
        ls_sample_result-evaluation  = 'A'.

        READ TABLE lt_char_req_tmp ASSIGNING FIELD-SYMBOL(<fs_char_req_tmp>)
          WITH KEY insplot  = lv_insplot
                   inspoper = lv_inspoper
                   inspchar = ls_qchar_itm-inspchar BINARY SEARCH.

        IF <fs_char_req_tmp> IS ASSIGNED.
          ls_sample_result-inspchar = <fs_char_req_tmp>-inspchar.
          IF <fs_char_req_tmp>-char_type = '01'.
            ls_sample_result-mean_value = ls_qchar_itm-value.
          ELSEIF <fs_char_req_tmp>-char_type = '02'.
            ls_sample_result-code1     = ls_qchar_itm-value.
            ls_sample_result-code_grp1 = ls_qchar_itm-mstr_char.
          ENDIF.
        ENDIF.

        MOVE-CORRESPONDING ls_sample_result TO ls_char_result.
        ls_char_result-inspector   = <lfs_qchar_hdr>-pernr.
        ls_char_result-start_date  = <lfs_qchar_hdr>-inspdate.
        ls_char_result-start_time  = <lfs_qchar_hdr>-insptime.
        ls_char_result-end_date    = <lfs_qchar_hdr>-inspdate.
        ls_char_result-end_time    = <lfs_qchar_hdr>-insptime.
        ls_char_result-closed      = abap_true.
        ls_char_result-evaluated   = abap_true.
        ls_char_result-evaluation  = 'A'.

        IF <lfs_qchar_hdr>-slwbez EQ 'Z28'.
          ls_sample_result-inspsample = lv_inspsample.
          APPEND: ls_sample_result TO lt_sample_result.
        ELSE.
          IF <fs_char_req_tmp> IS ASSIGNED.
            IF <fs_char_req_tmp>-sample_res = abap_true.
              ls_sample_result-inspsample = lv_inspsample.
              CLEAR ls_char_result-evaluation.
              APPEND: ls_sample_result TO lt_sample_result.
            ENDIF.
          ENDIF.
        ENDIF.

        APPEND: ls_char_result   TO lt_char_result.

        CLEAR: ls_sample_result-mean_value, ls_sample_result-code1,
               ls_sample_result-code_grp1, ls_sample_result-inspchar,
               ls_char_result.

      ENDLOOP. "lt_qchar_itm
*--- Append Formula Fields which are not saved in ZABS_QCHAR_ITM table for Z28 Insp Type
      IF <lfs_qchar_hdr>-slwbez = 'Z28'.
        LOOP AT lt_char_req_tmp INTO DATA(ls_char_req) WHERE formula IS NOT INITIAL.
          ls_char_result-insplot      = lv_insplot."ls_char_req-insplot.
          ls_char_result-inspoper     = lv_inspoper."ls_char_req-inspoper.
          ls_char_result-inspchar     = ls_char_req-inspchar.
          ls_sample_result-inspchar   = ls_char_req-inspchar.
          ls_sample_result-insplot    = lv_insplot.
          ls_sample_result-inspoper   = lv_inspoper.

          DATA(lv_inspsample1) = ls_qapp_submit-probenr.

*        ls_sample_result-insplot     = lv_insplot.
*        ls_sample_result-inspoper    = lv_inspoper.
          ls_sample_result-closed      = abap_true.
          ls_sample_result-evaluated   = abap_true.
          ls_sample_result-evaluation  = 'A'.

          MOVE-CORRESPONDING ls_sample_result TO ls_char_result.
          ls_char_result-inspector   = <lfs_qchar_hdr>-pernr.
          ls_char_result-start_date  = <lfs_qchar_hdr>-inspdate.
          ls_char_result-start_time  = <lfs_qchar_hdr>-insptime.
          ls_char_result-end_date    = <lfs_qchar_hdr>-inspdate.
          ls_char_result-end_time    = <lfs_qchar_hdr>-insptime.
          ls_char_result-closed      = abap_true.
          ls_char_result-evaluated   = abap_true.
          ls_char_result-evaluation  = 'A'.
          ls_sample_result-inspsample = lv_inspsample1.

          APPEND: ls_sample_result TO lt_sample_result.
          APPEND: ls_char_result   TO lt_char_result.
          CLEAR:  ls_sample_result, ls_char_result.
        ENDLOOP.
      ENDIF.
    ENDIF. "lt_qchar_itm


    ls_inspoint-insplot   = lv_insplot.
    ls_inspoint-inspoper  = lv_inspoper.
    ls_inspoint-insppoint = lv_inspsample.

    CLEAR : lv_pernr,
            lv_inspdate,
            lv_insptime,
            ls_inspoint-insp_date,
            ls_inspoint-insp_time,
            ls_inspoint-inspector.

    lv_pernr     = <lfs_qchar_hdr>-pernr.
    lv_inspdate  = <lfs_qchar_hdr>-inspdate.
    lv_insptime  = <lfs_qchar_hdr>-insptime.
    ls_inspoint-insp_date = <lfs_qchar_hdr>-inspdate.
    ls_inspoint-insp_time = <lfs_qchar_hdr>-insptime.

    PERFORM insppoints_data_map USING lv_insplot
                                      lv_inspoper
                                      lv_pernr
                                      lv_inspdate
                                      lv_insptime
                                      lv_slwbez
                                      lv_tplnrfl
                                      lv_strno
                                      lv_rbnr1
                                      lv_isample_z28
                                      lt_insppoints
                             CHANGING ls_inspoint.

    ch_s_insppoint = CORRESPONDING #( ls_inspoint  ).

    DO 20 TIMES.
      REFRESH : ex_t_rettab.
      sy-uname = ls_inspoint-inspector = <lfs_qchar_hdr>-mobusr.

      CALL FUNCTION 'BAPI_INSPOPER_RECORDRESULTS' ""IN UPDATE TASK
        EXPORTING
          insplot        = lv_insplot
          inspoper       = lv_inspoper
          insppointdata  = ls_inspoint
        IMPORTING
          return         = ls_return
        TABLES
          char_results   = lt_char_result
          sample_results = lt_sample_result
          returntable    = ex_t_rettab.

*-- Lote de controle &1 operação &2 atualmente bloq.por outro usuário
      READ TABLE ex_t_rettab TRANSPORTING NO FIELDS
                             WITH KEY type   = 'E'
                                      id     = 'QT'
                                      number = '400'.
      IF sy-subrc <> 0.
        EXIT.
      ELSE.
        WAIT UP TO 1 SECONDS.
      ENDIF.
    ENDDO.

    READ TABLE ex_t_rettab TRANSPORTING NO FIELDS
                           WITH KEY type = 'E'.

    IF sy-subrc <> 0.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait   = abap_true
        IMPORTING
          return = ls_return.

      ls_str_qcharmlog-msgid  = '00'.
      ls_str_qcharmlog-msgno  = '208'.
      ls_str_qcharmlog-msgtyp = 'S'.
      ls_str_qcharmlog-msgtxt = TEXT-002.
      APPEND ls_str_qcharmlog TO gt_str_qcharmlog.

      CLEAR : ls_str_qcharmlog-msgid,
              ls_str_qcharmlog-msgno,
              ls_str_qcharmlog-msgtyp,
              ls_str_qcharmlog-msgtxt.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      LOOP AT ex_t_rettab ASSIGNING FIELD-SYMBOL(<fs_rettab>).
        ls_qchar_elog-msgid  = <fs_rettab>-id.
        ls_qchar_elog-msgno  = <fs_rettab>-number.
        ls_qchar_elog-msgtyp = <fs_rettab>-type.
        ls_qchar_elog-msgtxt = <fs_rettab>-message.

        IF ls_qchar_elog-msgtyp = zcl_abs_abap_maintain=>c_msgty_error. "'E'
          APPEND ls_qchar_elog TO gt_qchar_elog.
        ENDIF.

        ls_str_qcharmlog-msgid  = <fs_rettab>-id.
        ls_str_qcharmlog-msgno  = <fs_rettab>-number.
        ls_str_qcharmlog-msgtyp = <fs_rettab>-type.
        ls_str_qcharmlog-msgtxt = <fs_rettab>-message.
        APPEND ls_qchar_elog    TO gt_qchar_elog.
        APPEND ls_str_qcharmlog TO gt_str_qcharmlog.
        CLEAR : ls_str_qcharmlog-msgid, ls_str_qcharmlog-msgno,
                ls_str_qcharmlog-msgtyp, ls_str_qcharmlog-msgtxt, ls_messages.
      ENDLOOP.
    ENDIF.

    IF ex_t_rettab IS INITIAL.
*-- Custom Table Update Logic
      IF <lfs_qchar_hdr>-cflag = abap_true.
        PERFORM inspection_lot_flag_update USING <lfs_qchar_hdr>
                                                 lv_insplot
                                                 lv_inspoper
                                        CHANGING ls_messages.

        IF ls_messages IS NOT INITIAL.
          CONCATENATE ls_messages-msgv1 ls_messages-msgv2
                      ls_messages-msgv3 ls_messages-msgv4
            INTO ls_str_qcharmlog-msgtxt SEPARATED BY space.

          ls_str_qcharmlog-msgid  = ls_messages-msgid.
          ls_str_qcharmlog-msgno  = ls_messages-msgno.
          ls_str_qcharmlog-msgtyp = ls_messages-msgty.
          APPEND ls_str_qcharmlog TO gt_str_qcharmlog.

          CLEAR: ls_str_qcharmlog-msgid,
                 ls_str_qcharmlog-msgno,
                 ls_str_qcharmlog-msgtyp,
                 ls_str_qcharmlog-msgtxt,
                 ls_messages.
        ENDIF.
      ENDIF.

      CLEAR: ls_return.
      COMMIT WORK AND WAIT.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ch_s_insppoint-userc1
        IMPORTING
          output = ch_s_insppoint-userc1.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ch_s_insppoint-userc2
        IMPORTING
          output = ch_s_insppoint-userc2.

      IF ls_return IS INITIAL.
        WAIT UP TO 1 SECONDS.

        SELECT SINGLE prueflos, vorglfnr, probenr, ppsortkey
          FROM qapp
          INTO @DATA(ls_inspqapp)
         WHERE prueflos = @lv_insplot
           AND userc1   = @ch_s_insppoint-userc1
           AND userc2   = @ch_s_insppoint-userc2
           AND usern1   = @ch_s_insppoint-usern1
           AND usern2   = @ch_s_insppoint-usern2
           AND userd1   = @ch_s_insppoint-userd1
           AND usert1   = @ch_s_insppoint-usert1.

        APPEND INITIAL LINE TO lt_mobqapp_rec ASSIGNING FIELD-SYMBOL(<fs_mobqapp_rec>).
        <fs_mobqapp_rec>-prueflos  = ls_inspqapp-prueflos.
        <fs_mobqapp_rec>-vorglfnr  = ls_inspqapp-vorglfnr.
        <fs_mobqapp_rec>-probenr   = ls_inspqapp-probenr.
        <fs_mobqapp_rec>-ppsortkey = ls_inspqapp-ppsortkey.
        <fs_mobqapp_rec>-zzimei1   = <lfs_qchar_hdr>-zzimei1.
        <fs_mobqapp_rec>-zbadge    = <lfs_qchar_hdr>-zbadge.
        <fs_mobqapp_rec>-ernam     = sy-uname.
        <fs_mobqapp_rec>-erdat     = sy-datum.
        <fs_mobqapp_rec>-erzet     = sy-uzeit.

        MODIFY zabs_mobqapp_rec FROM <fs_mobqapp_rec>.
        IF sy-subrc = 0.
          COMMIT WORK AND WAIT.
        ENDIF.

        APPEND ls_return TO ex_t_rettab.
      ENDIF. "ls_return
    ENDIF.

    CLEAR : lv_insplot, lv_inspoper, ls_inspoint, ls_return.
    REFRESH : lt_char_result, lt_sample_result.
  ENDLOOP. "lt_qchar_hdr

  IF pt_qchar_hdr IS NOT INITIAL.
    DATA(lt_qchar_hdr_aux) = pt_qchar_hdr[].
    SELECT prueflos, vorglfnr, probenr, userd1,
           usert1, entstehdat, entstehzt
      FROM qapp
      INTO TABLE @DATA(lt_qapp_aux)
      FOR ALL ENTRIES IN @lt_qchar_hdr_aux
     WHERE prueflos   = @lt_qchar_hdr_aux-insplot
       AND usert1     = @lt_qchar_hdr_aux-insptime
       AND userd1 = @lt_qchar_hdr_aux-inspdate.
    "AND entstehdat = @lt_qchar_hdr_aux-inspdate.
    "SORT lt_qapp_aux BY prueflos usert1 entstehdat.
    SORT lt_qapp_aux BY prueflos userd1 usert1.
    LOOP AT lt_qchar_hdr_aux ASSIGNING FIELD-SYMBOL(<ls_hdr_aux>).
      READ TABLE lt_qapp_aux INTO DATA(ls_qapp_aux)
        WITH KEY prueflos   = <ls_hdr_aux>-insplot
                 userd1 = <ls_hdr_aux>-inspdate
                 usert1     = <ls_hdr_aux>-insptime BINARY SEARCH.
      "entstehdat = <ls_hdr_aux>-inspdate BINARY SEARCH.
      IF sy-subrc NE 0.
        CLEAR: <ls_hdr_aux>-recflag, <ls_hdr_aux>-batch.
        ls_qchar_elog-insplot    = <ls_hdr_aux>-insplot.
        ls_qchar_elog-inspoper   = <ls_hdr_aux>-inspoper.
        ls_qchar_elog-dttimstamp = <ls_hdr_aux>-dttimstamp.
        ls_qchar_elog-pernr      = <ls_hdr_aux>-pernr.
        ls_qchar_elog-msgid     = 'ZFMFP'.
        ls_qchar_elog-msgno     = '316'.
        ls_qchar_elog-msgtyp    = 'E'.
*-- Erro ao processar Lote &1 Operação &2.
        MESSAGE e316(zfmfp) INTO ls_qchar_elog-msgtxt
          WITH ls_qchar_elog-insplot ls_qchar_elog-inspoper.
        ls_qchar_elog-ernam = sy-uname.
        ls_qchar_elog-erdat = sy-datum.
        ls_qchar_elog-erzet = sy-uzeit.
        APPEND ls_qchar_elog TO lt_qchar_elog.
      ENDIF.
    ENDLOOP.

    IF lt_qchar_elog[] IS NOT INITIAL.
      APPEND LINES OF lt_qchar_elog TO pt_qchar_errorlog.
      APPEND LINES OF lt_qchar_elog TO gt_qchar_elog.
      lt_str_qcharmlog = CORRESPONDING #( lt_qchar_elog ).
      APPEND LINES OF lt_str_qcharmlog TO gt_str_qcharmlog.
    ENDIF.

    MODIFY zabs_qchar_hdr FROM TABLE lt_qchar_hdr_aux.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

  LOOP AT gt_qchar_elog ASSIGNING FIELD-SYMBOL(<lfs_qchar_elog>).
    <lfs_qchar_elog>-ernam = sy-uname.
    <lfs_qchar_elog>-erdat = sy-datum.
    <lfs_qchar_elog>-erzet = sy-uzeit.
  ENDLOOP.

  IF gt_qchar_elog IS NOT INITIAL.
    MODIFY zabs_qchar_erlog FROM TABLE gt_qchar_elog.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      form  sub_job_status_check
*&---------------------------------------------------------------------*
FORM sub_job_status_check USING pv_maxjobs TYPE /irm/g_maxsn.

*-- Local data
  DATA : lv_aborted  TYPE tbtco-status VALUE 'A',
         lv_finished TYPE tbtco-status VALUE 'F',
         lv_unknown  TYPE tbtco-status VALUE 'X',
         lv_schedule TYPE tbtco-status VALUE 'P'.

*-- Field Symbols
  FIELD-SYMBOLS : <ls_joblog> TYPE ty_joblog.

  DO.
    IF gv_active_sessions LT pv_maxjobs.
      EXIT.
    ENDIF.

    LOOP AT gt_joblog ASSIGNING <ls_joblog>.
      IF <ls_joblog>-status NE lv_finished
      AND <ls_joblog>-status NE lv_aborted.

        CALL FUNCTION 'BP_JOB_STATUS_GET'
          EXPORTING
            jobcount                   = <ls_joblog>-jnumb
            jobname                    = <ls_joblog>-jname
          IMPORTING
            status                     = <ls_joblog>-status
          EXCEPTIONS
            job_doesnt_exist           = 1
            unknown_error              = 2
            parent_child_inconsistency = 3
            OTHERS                     = 4.

        IF <ls_joblog>-status EQ lv_aborted
        OR <ls_joblog>-status EQ lv_finished.
          SUBTRACT 1 FROM gv_active_sessions.
        ELSEIF <ls_joblog>-status EQ lv_schedule.
* If it is a scheduled Job Release it
          CALL FUNCTION 'BP_JOB_RELEASE'
            EXPORTING
              jobname                     = <ls_joblog>-jname
              jobcount                    = <ls_joblog>-jnumb
            EXCEPTIONS
              missing_jobname             = 1
              missing_jobcount            = 2
              missing_start_date          = 3
              status_not_scheduled        = 4
              cant_enq_job                = 5
              cant_start_job_immediately  = 6
              no_privilege_to_release_job = 7
              cant_release_job            = 8
              job_not_exist               = 9
              job_have_no_steps           = 10
              error_job_modify            = 11
              OTHERS                      = 12.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDLOOP.

    IF gv_active_sessions GE pv_maxjobs.
      WAIT UP TO 10 SECONDS.
    ENDIF.
  ENDDO.

ENDFORM.                    " SUB_JOB_STATUS_CHECK

*&---------------------------------------------------------------------*
*& Form SCREEN_MODIFY
*&---------------------------------------------------------------------*
*& SCREEN_MODIFY
*&---------------------------------------------------------------------*
FORM screen_modify.

*--Screen Validations
  LOOP AT SCREEN.
    IF p_par IS INITIAL.
      IF ( screen-group1 = 'K1'
      OR screen-group1 = 'K2'
      OR screen-group1 = 'K3'
      OR screen-group1 = 'K4'
      OR screen-group1 = 'K5' ).
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form EXECUTE_PARALLEL_PROCESSING
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_QCHAR_HDR
*&---------------------------------------------------------------------*
FORM execute_parallel_processing USING pt_qchar_hdr TYPE zabs_tty_qchar_hdr
                                       pt_qapp      TYPE qapptab.

*--Data declaration
  DATA : lt_qchdr          TYPE STANDARD TABLE OF zabs_qchar_hdr,
         lt_qchar_hdr      TYPE STANDARD TABLE OF zabs_qchar_hdr,
         lt_qchdr_submit   TYPE STANDARD TABLE OF zabs_qchar_hdr,
         lt_qchar_errorlog TYPE STANDARD TABLE OF zabs_qchar_erlog,
         lt_final          LIKE lt_qchdr,
         lt_qapp_submit    TYPE qapptab,
         ls_joblog         TYPE ty_joblog,
         lv_job_name       TYPE btcjob,
         lv_job_number     TYPE btcjobcnt,
         lv_index          TYPE sy-index,
         lv_maxsn          TYPE /irm/s_g_parallel_pro_ctrl-maxsn,
         lv_psize          TYPE /irm/s_g_parallel_pro_ctrl-psize,
         lv_char_index     TYPE char20.

  lt_qchdr[] = pt_qchar_hdr[].

  DATA(lt_qapp) = pt_qapp[].
  SORT lt_qapp BY prueflos ASCENDING
                  probenr  DESCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_qapp COMPARING prueflos.

  lv_maxsn = p_maxsn.
  lv_psize = p_psize.
  lt_final[] = lt_qchdr[].
  DELETE lt_final WHERE cflag EQ abap_false.
  DELETE lt_qchdr WHERE cflag EQ abap_true.

*-- Schedule the jobs based on Participants data
  DO.
    lv_index = sy-index.

    CLEAR: lv_job_name,
           lv_char_index,
           lv_job_number.

    DATA(lv_final) = abap_false.
    IF lt_qchdr[] IS INITIAL
    AND lt_final[] IS NOT INITIAL.
      lv_final = abap_true.
      lv_psize = lines( lt_final ).
      lt_qchdr[] = lt_final[].
      REFRESH lt_final.
    ENDIF.

    REFRESH: lt_qchdr_submit, lt_qapp_submit.
    LOOP AT lt_qchdr INTO DATA(ls_qchdr) FROM 1 TO lv_psize.
      READ TABLE lt_qapp ASSIGNING FIELD-SYMBOL(<ls_qapp>)
        WITH KEY prueflos = ls_qchdr-insplot.
      IF sy-subrc EQ 0.
        ADD 1 TO <ls_qapp>-probenr.
        APPEND <ls_qapp> TO lt_qapp_submit.
      ENDIF.
      APPEND ls_qchdr TO lt_qchdr_submit.
      CLEAR ls_qchdr.
    ENDLOOP."lt_qchdr

*-- Prepare Job name
    lv_job_name   = p_jobpfx.
    lv_char_index = lv_index.

    CONDENSE lv_char_index NO-GAPS.

    CONCATENATE p_rdate p_time '_' lv_char_index
           INTO lv_char_index.

    SHIFT lv_char_index LEFT DELETING LEADING: '0',
                                               space.

    CONCATENATE lv_job_name '-' lv_char_index INTO lv_job_name.
    CONDENSE lv_job_name NO-GAPS.

*-- Getting the job number
    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = lv_job_name
      IMPORTING
        jobcount         = lv_job_number
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.
    IF sy-subrc EQ 0.
*-- Ceate Post Settlement Adjustment Calculation Run
      SUBMIT zabs_rep_insp_results_update
             WITH p_qchdr EQ lt_qchdr_submit
             WITH p_qapp  EQ lt_qapp_submit
             WITH p_par   EQ space
             WITH p_final EQ lv_final
             VIA JOB lv_job_name
             NUMBER lv_job_number
             AND RETURN.
    ENDIF. " Job Open subrc check

*-- Closing the job
    DO.
      CALL FUNCTION 'JOB_CLOSE'
        EXPORTING
          jobcount             = lv_job_number
          jobname              = lv_job_name
          strtimmed            = abap_true
        EXCEPTIONS
          cant_start_immediate = 1
          invalid_startdate    = 2
          jobname_missing      = 3
          job_close_failed     = 4
          job_nosteps          = 5
          job_notex            = 6
          lock_failed          = 7
          invalid_target       = 8
          OTHERS               = 9.

      IF sy-subrc <> 0.
        WAIT UP TO 2 SECONDS.
      ELSE.
        ls_joblog-jname = lv_job_name.
        ls_joblog-jnumb = lv_job_number.
        ls_joblog-index = lv_index.
        APPEND ls_joblog TO gt_joblog.

        ADD 1 TO gv_active_sessions.
        EXIT.
      ENDIF.
    ENDDO."Job Close

*-- Delete the processed records
    DELETE lt_qchdr FROM 1 TO lv_psize.

    IF lt_qchdr[] IS INITIAL.
      lv_maxsn = 1.
    ENDIF.

*-- Exit if all the participant data is scheduled in job
    IF lt_qchdr[] IS INITIAL
    AND lt_final[] IS INITIAL.
      EXIT.
    ENDIF.

*-- Check Max Jobs
    IF gv_active_sessions GE lv_maxsn.
*-- Number of active parallel background work-processes should not be
*-- greater than max number specified
      PERFORM sub_job_status_check USING lv_maxsn.
    ENDIF."gv_active_sessions
  ENDDO."lt_qchdr

  CLEAR lv_maxsn.
  PERFORM check_job_processing USING lv_maxsn.

  IF gt_qchar_hdr[] IS NOT INITIAL.
    SELECT prueflos, vorglfnr, probenr, userd1,
           usert1, entstehdat, entstehzt
      FROM qapp
      INTO TABLE @DATA(lt_qapp_aux)
      FOR ALL ENTRIES IN @gt_qchar_hdr
     WHERE prueflos = @gt_qchar_hdr-insplot
       AND userd1   = @gt_qchar_hdr-inspdate
       AND usert1   = @gt_qchar_hdr-insptime.
    SORT lt_qapp_aux BY prueflos userd1 usert1.
    LOOP AT gt_qchar_hdr ASSIGNING FIELD-SYMBOL(<ls_hdr_aux>).
      READ TABLE lt_qapp_aux INTO DATA(ls_qapp_aux)
        WITH KEY prueflos = <ls_hdr_aux>-insplot
                 userd1   = <ls_hdr_aux>-inspdate
                 usert1   = <ls_hdr_aux>-insptime BINARY SEARCH.
      IF sy-subrc NE 0.
        APPEND <ls_hdr_aux> TO lt_qchar_hdr.
      ENDIF.
    ENDLOOP.
    SORT lt_qchar_hdr BY insplot inspoper dttimstamp.
    IF lt_qchar_hdr[] IS NOT INITIAL.
      SELECT *
        FROM zabs_qchar_itm
        FOR ALL ENTRIES IN @lt_qchar_hdr
       WHERE insplot    EQ @lt_qchar_hdr-insplot
         AND inspoper   EQ @lt_qchar_hdr-inspoper
         AND dttimstamp EQ @lt_qchar_hdr-dttimstamp
        INTO TABLE @DATA(lt_qchar_itm).
      IF sy-subrc = 0.
        SORT lt_qchar_itm BY insplot inspoper dttimstamp.
      ENDIF.

      PERFORM result_recording USING lt_qchar_hdr
                                     lt_qchar_itm
                                     lt_qchar_errorlog.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_INSPECTION_LOT_FLAG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_INSPLOT
*&      --> LV_INSPOPER
*&      <-- LS_MESSAGES
*&---------------------------------------------------------------------*
FORM check_inspection_lot_flag USING    pv_insplot  TYPE qals-prueflos
                                        pv_inspoper TYPE qapo-vornr
                               CHANGING ps_messages TYPE /agri/s_gprolog.

  TYPES: BEGIN OF lty_cvornr,
           cvornr TYPE zabs_cvornr,
         END OF lty_cvornr.

  DATA: lt_cvornr   TYPE TABLE OF lty_cvornr,
        lwa_mob_lot TYPE zabs_mob_ilot.

  IF pv_insplot IS NOT INITIAL.
    SELECT SINGLE *
      FROM zabs_mob_ilot
      INTO @lwa_mob_lot
     WHERE prueflos = @pv_insplot.
    IF sy-subrc = 0.
      IF lwa_mob_lot-zzvornr IS NOT INITIAL.
        SPLIT lwa_mob_lot-zzvornr AT ';' INTO TABLE lt_cvornr.
        SORT lt_cvornr BY cvornr.
        READ TABLE lt_cvornr TRANSPORTING NO FIELDS
          WITH KEY cvornr = pv_inspoper BINARY SEARCH.
        IF sy-subrc = 0.
*-- Lote &1 Operação &2 definido como "Avaliação Final".
*-- Lot &1 Operation &2 is defined as "Final Evaluation".
          ps_messages-msgty = 'E'.
          ps_messages-msgv1 = pv_insplot.
          ps_messages-msgv1 = pv_inspoper.
          ps_messages-msgid = 'ZFMFP'.
          ps_messages-msgno = '294'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_JOB_PROCESSING
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_MAXSN
*&---------------------------------------------------------------------*
FORM check_job_processing USING pv_maxjobs TYPE /irm/g_maxsn.

*-- Local data
  DATA : lv_aborted  TYPE tbtco-status VALUE 'A',
         lv_finished TYPE tbtco-status VALUE 'F',
         lv_unknown  TYPE tbtco-status VALUE 'X',
         lv_schedule TYPE tbtco-status VALUE 'P'.

*-- Field Symbols
  FIELD-SYMBOLS : <ls_joblog> TYPE ty_joblog.

  DO.
    IF gv_active_sessions EQ pv_maxjobs.
      EXIT.
    ENDIF.

    LOOP AT gt_joblog ASSIGNING <ls_joblog>.
      IF <ls_joblog>-status NE lv_finished
      AND <ls_joblog>-status NE lv_aborted.

        CALL FUNCTION 'BP_JOB_STATUS_GET'
          EXPORTING
            jobcount                   = <ls_joblog>-jnumb
            jobname                    = <ls_joblog>-jname
          IMPORTING
            status                     = <ls_joblog>-status
          EXCEPTIONS
            job_doesnt_exist           = 1
            unknown_error              = 2
            parent_child_inconsistency = 3
            OTHERS                     = 4.

        IF <ls_joblog>-status EQ lv_aborted
        OR <ls_joblog>-status EQ lv_finished.
          SUBTRACT 1 FROM gv_active_sessions.
        ELSEIF <ls_joblog>-status EQ lv_schedule.
* If it is a scheduled Job Release it
          CALL FUNCTION 'BP_JOB_RELEASE'
            EXPORTING
              jobname                     = <ls_joblog>-jname
              jobcount                    = <ls_joblog>-jnumb
            EXCEPTIONS
              missing_jobname             = 1
              missing_jobcount            = 2
              missing_start_date          = 3
              status_not_scheduled        = 4
              cant_enq_job                = 5
              cant_start_job_immediately  = 6
              no_privilege_to_release_job = 7
              cant_release_job            = 8
              job_not_exist               = 9
              job_have_no_steps           = 10
              error_job_modify            = 11
              OTHERS                      = 12.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDLOOP.

    IF gv_active_sessions GE pv_maxjobs.
      WAIT UP TO 10 SECONDS.
    ENDIF.
  ENDDO.

ENDFORM.
