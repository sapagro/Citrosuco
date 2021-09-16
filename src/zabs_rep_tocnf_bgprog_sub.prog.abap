************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name       :  ZABS_REP_TOCNF_BGPROG_SUB                       *
* Tcode             :  ZABS_TRN_TOMON                                  *
* Created By        :                                                  *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  08.13.2020                                      *
* TR                :  ZABS_TRN_BGP_TOCNF                              *
* Version           :  001                                             *
* Description       :  Task Order Confirmation Back Ground Program     *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

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
*& Form INITIALIZE_GLOBAL_DATA
*&---------------------------------------------------------------------*
*& INITIALIZE_GLOBAL_DATA
*&---------------------------------------------------------------------*
FORM initialize_global_data.
  REFRESH : gt_tocnf_elog, gt_str_tocnfmlog.
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
      i_structure_name       = zcl_abs_abap_maintain=>c_tocnf_mlog_fcat "'ZABS_STR_TOCNF_MLOG'
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
        t_outtab    = gt_str_tocnfmlog.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form TASKORDER_CONF
*&---------------------------------------------------------------------*
*& TASKORDER_CONF
*&---------------------------------------------------------------------*
FORM taskorder_conf.

*--Local declarations
  DATA: lt_tocnf_hdr TYPE zabs_tty_tocnf_hdr,
        ls_tocnf_hdr TYPE zabs_tocnf_hdr.

  lt_tocnf_hdr = p_tohdr.

  IF lt_tocnf_hdr IS INITIAL.
    SELECT *
      FROM zabs_tocnf_hdr
      INTO TABLE lt_tocnf_hdr
     WHERE aufnr      IN s_aufnr
       AND vornr      IN s_vornr
       AND dttimstamp IN s_dtstmp
       AND recflag    EQ space
*       and batch       EQ space
       AND ernam      IN s_musr
       AND erdat      IN s_erdat
       AND erzet      IN s_erzet.
    IF sy-subrc <> 0.
      MESSAGE TEXT-003 TYPE zcl_abs_abap_maintain=>c_msgty_info.
      LEAVE LIST-PROCESSING.
    ENDIF.

    DELETE lt_tocnf_hdr WHERE batch EQ zcl_abs_abap_maintain=>c_job_batch. "'B'
    IF lt_tocnf_hdr IS INITIAL.
      MESSAGE TEXT-004 TYPE zcl_abs_abap_maintain=>c_msgty_info
                       DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_error.
      LEAVE LIST-PROCESSING.
    ENDIF.

    IF p_par IS NOT INITIAL.
      ls_tocnf_hdr-batch = zcl_abs_abap_maintain=>c_job_batch. "'B'
      ls_tocnf_hdr-aenam = sy-uname.
      ls_tocnf_hdr-aedat = sy-datum.
      ls_tocnf_hdr-aezet = sy-uzeit.
      MODIFY lt_tocnf_hdr FROM ls_tocnf_hdr TRANSPORTING batch aenam aedat aezet
      WHERE aufnr NE space.
      MODIFY zabs_tocnf_hdr FROM TABLE lt_tocnf_hdr.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

  IF lt_tocnf_hdr IS NOT INITIAL
  AND p_par IS INITIAL.
    SORT lt_tocnf_hdr BY aufnr vornr dttimstamp.
    SELECT *
      FROM zabs_tocnf_itm
       FOR ALL ENTRIES IN @lt_tocnf_hdr
     WHERE aufnr      EQ @lt_tocnf_hdr-aufnr
       AND vornr      EQ @lt_tocnf_hdr-vornr
       AND dttimstamp EQ @lt_tocnf_hdr-dttimstamp
      INTO TABLE @DATA(lt_tocnf_itm).
    IF sy-subrc = 0.
      SORT lt_tocnf_itm BY aufnr vornr dttimstamp.
    ENDIF.

*--Fetching ErrprLog data to check if the record is in the table with error
    SELECT *
      FROM zabs_tocnf_elog
      INTO TABLE @DATA(lt_tocnf_errorlog)
       FOR ALL ENTRIES IN @lt_tocnf_hdr
     WHERE aufnr      EQ @lt_tocnf_hdr-aufnr
       AND vornr      EQ @lt_tocnf_hdr-vornr
       AND dttimstamp EQ @lt_tocnf_hdr-dttimstamp
       AND ernam      IN @s_musr
       AND erdat      IN @s_erdat
       AND erzet      IN @s_erzet.
    IF sy-subrc = 0.
      SORT lt_tocnf_errorlog BY aufnr vornr dttimstamp.
    ENDIF.
  ENDIF.

  IF p_par IS NOT INITIAL.
    PERFORM parallel_processing USING lt_tocnf_hdr.
    MESSAGE TEXT-005 TYPE zcl_abs_abap_maintain=>c_msgty_success.
  ELSE.
    PERFORM taskorder_confirm CHANGING lt_tocnf_hdr
                                       lt_tocnf_itm
                                       lt_tocnf_errorlog.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form PARALLEL_PROCESSING
*&---------------------------------------------------------------------*
*& PARALLEL_PROCESSING
*&---------------------------------------------------------------------*
FORM parallel_processing  USING pt_tocnf_hdr TYPE zabs_tty_tocnf_hdr.

*--Data declaration
  DATA : lt_tohdr        TYPE TABLE OF zabs_tocnf_hdr,
         lt_tohdr_submit TYPE TABLE OF zabs_tocnf_hdr,

         ls_joblog       TYPE ty_joblog,

         lv_job_name     TYPE btcjob,
         lv_job_number   TYPE btcjobcnt,
         lv_index        TYPE sy-index,
         lv_char_index   TYPE char20.

  lt_tohdr[] = pt_tocnf_hdr[].

*-- Schedule the jobs based on Participants data
  DO.

    CLEAR: lv_job_name,
           lv_char_index,
           lv_job_number.

    REFRESH lt_tohdr_submit.
*-- Collect the Participant to be used while creating Calculation run
    LOOP AT lt_tohdr INTO DATA(ls_tohdr) FROM 1 TO p_psize.
      APPEND ls_tohdr TO lt_tohdr_submit.
      CLEAR ls_tohdr.
    ENDLOOP."lt_qchdr

*-- Prepare Job name
    lv_job_name   = p_jobpfx.
    lv_index      = sy-index.
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
      SUBMIT zabs_rep_tocnf_bgprog
             WITH p_tohdr EQ lt_tohdr_submit
             WITH p_par   EQ space
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
*         targetgroup          = ls_parallel_pro_ctrl-srgrp
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
    DELETE lt_tohdr FROM 1 TO p_psize.

*-- Exit if all the participant data is scheduled in job
    IF lt_tohdr IS INITIAL.
      EXIT.
    ENDIF."lt_partid

    IF gv_active_sessions GE p_maxsn.

*-- Number of active parallel background work-processes should not be
*-- greater than max number specified
      PERFORM sub_job_status_check USING p_maxsn.   " Max Jobs

    ENDIF."gv_active_sessions

  ENDDO."lt_qchdr

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SUB_JOB_STATUS_CHECK
*&---------------------------------------------------------------------*
*& SUB_JOB_STATUS_CHECK
*&---------------------------------------------------------------------*
FORM sub_job_status_check  USING
               pv_maxjobs TYPE /irm/s_g_parallel_pro_ctrl-maxsn. "i.

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
        IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        IF <ls_joblog>-status EQ lv_aborted OR
           <ls_joblog>-status EQ lv_finished.

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

*&---------------------------------------------------------------------*
*& Form RESULT_RECORDING
*&---------------------------------------------------------------------*
*& RESULT_RECORDING
*&---------------------------------------------------------------------*
FORM taskorder_confirm CHANGING ct_tocnf_hdr      TYPE zabs_tty_tocnf_hdr
                                ct_tocnf_itm      TYPE tty_tocnf_itm
                                ct_tocnf_errorlog TYPE tty_tocnf_elog.

  TYPES: BEGIN OF lty_strno,
           strno TYPE /agri/glstrno,
         END OF lty_strno.

  CONSTANTS: lc_memreason(8) TYPE c     VALUE 'ZZREASON',
             lc_timecons     TYPE p     DECIMALS 3 VALUE '9',
             lc_postp        TYPE postp VALUE 'L'.

*-- Data Declarations
  DATA: lt_strno         TYPE TABLE OF lty_strno,
        lv_timecalc      TYPE syuzeit,
        lt_fmfp_doc      TYPE /agri/t_fmfp_doc,
        ls_fmfpcnf       TYPE /agri/s_fmfp_cnf,
        lt_fmfpcnf       TYPE /agri/t_fmfp_cnf,
        lt_fpcom         TYPE /agri/t_fmfpcom,
*        lt_woactivities    TYPE zcl_zabs_agri_mobile_e_mpc=>tt_woactivities_ext,
        ls_woactivities  TYPE zcl_zabs_agri_mobile_e_mpc=>ts_woactivities_ext,
        lv_tabix         TYPE sy-tabix,
        lv_acqtb         TYPE zfmacqtb,
        lv_period        TYPE zfmacperiod,
        lt_messages      TYPE /agri/t_gprolog,
        lt_tocnf_elog    TYPE TABLE OF zabs_tocnf_elog,
        ls_tocnf_elog    TYPE zabs_tocnf_elog,
        ls_str_tocnfmlog TYPE zabs_str_tocnf_mlog,
        ls_fpcom         TYPE /agri/s_fmfpcom,
        ls_messages      TYPE /agri/s_gprolog,
        lv_msgno         TYPE symsgno,
        lv_subrc         TYPE sy-subrc,
        lv_uzeit         TYPE sy-uzeit,
        lv_aufnr         TYPE aufnr,
        lv_commit_true   TYPE xfeld,
        lv_msehi         TYPE msehi,
        lv_exhour        TYPE p DECIMALS 3,
        lv_exhourt       TYPE p DECIMALS 3,
        lv_reason        TYPE flag,
        lv_times         TYPE c LENGTH 13,
        lwa_rcr01        TYPE rcr01,
        lv_arbpl         TYPE arbpl,
        lv_arbid         TYPE rcrco_text-arbid,
        lwa_text         TYPE rcrco_text,
        lv_index,
        lv_days          TYPE t5a4a-dlydy,
        lv_ldate         TYPE sy-datum,
        lv_hdate         TYPE sy-datum,
        lv_fieldname     TYPE fieldname,
        lv_ism01         TYPE ru_ismng.

*-- FIELD-SYMBOLS.
  FIELD-SYMBOLS: <ls_fpcom> TYPE /agri/s_fmfpcom,
                 <ls_log>   TYPE /agri/s_gprolog,
                 <lv_text>  TYPE any.

  DATA : lobj_db TYPE REF TO zcl_zabs_agri_mobile_e_dpc_ext.

  CREATE OBJECT lobj_db.

  IF ct_tocnf_hdr IS NOT INITIAL.
*--Login User based validation to User+Employee based valiadtion
    SELECT *
      FROM zabs_emp_role
      INTO TABLE @DATA(lt_usremp)
       FOR ALL ENTRIES IN @ct_tocnf_hdr
     WHERE pernr EQ @ct_tocnf_hdr-pernr.

*-- For selecting RSPOS - this will be useful as key for the REASON functionality.
    SELECT *
      FROM /agri/fmfpcom
      INTO TABLE @DATA(lt_fpcomd)
      FOR ALL ENTRIES IN @ct_tocnf_hdr
      WHERE aufnr = @ct_tocnf_hdr-aufnr.
  ENDIF.

  IF sy-subrc = 0.
    DATA lr_fpcnf TYPE RANGE OF zabs_del_fpcnf.lr_fpcnf =
    VALUE #( FOR ls_usrempt IN lt_usremp (
    sign   = 'I'
    option = 'EQ'
    low    = ls_usrempt-fpcnf ) ).
    DELETE lr_fpcnf WHERE low IS INITIAL.
    SORT lr_fpcnf BY low.
    DELETE ADJACENT DUPLICATES FROM lr_fpcnf COMPARING low.
  ENDIF.

  LOOP AT ct_tocnf_hdr ASSIGNING FIELD-SYMBOL(<lfs_tocnf_hdr>).

    <lfs_tocnf_hdr>-recflag = abap_true.
    <lfs_tocnf_hdr>-batch   = space.
    <lfs_tocnf_hdr>-aenam   = sy-uname.
    <lfs_tocnf_hdr>-aedat   = sy-datum.
    <lfs_tocnf_hdr>-aezet   = sy-uzeit.

    ls_tocnf_elog-aufnr      = <lfs_tocnf_hdr>-aufnr.
    ls_tocnf_elog-vornr      = <lfs_tocnf_hdr>-vornr.
    ls_tocnf_elog-dttimstamp = <lfs_tocnf_hdr>-dttimstamp.
    ls_tocnf_elog-pernr      = <lfs_tocnf_hdr>-pernr.

    ls_str_tocnfmlog-aufnr      = <lfs_tocnf_hdr>-aufnr.
    ls_str_tocnfmlog-vornr      = <lfs_tocnf_hdr>-vornr.
    ls_str_tocnfmlog-dttimstamp = <lfs_tocnf_hdr>-dttimstamp.
    ls_str_tocnfmlog-pernr      = <lfs_tocnf_hdr>-pernr.

*---Record is already processed and sitting in Error Log.
    READ TABLE ct_tocnf_errorlog INTO DATA(ls_tocnf_errorlog)
    WITH KEY aufnr      = <lfs_tocnf_hdr>-aufnr
             vornr      = <lfs_tocnf_hdr>-vornr
             dttimstamp = <lfs_tocnf_hdr>-dttimstamp
             BINARY SEARCH.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <lfs_tocnf_hdr>-aufnr
      IMPORTING
        output = <lfs_tocnf_hdr>-aufnr.

    lv_aufnr = <lfs_tocnf_hdr>-aufnr.

*--Unit conversion
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        input          = <lfs_tocnf_hdr>-meinh
        language       = sy-langu
      IMPORTING
        output         = <lfs_tocnf_hdr>-meinh
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.

    IF sy-subrc = 0.
      <lfs_tocnf_hdr>-arbpl_ext = <lfs_tocnf_hdr>-arbpl.
      <lfs_tocnf_hdr>-gicre     = abap_true.
      <lfs_tocnf_hdr>-lmnga     = <lfs_tocnf_hdr>-gwemg.

      IF <lfs_tocnf_hdr>-budat IS NOT INITIAL.
        <lfs_tocnf_hdr>-budat = <lfs_tocnf_hdr>-isdd.
      ENDIF.

*      <lfs_tocnf_hdr>-zznrtank  = iv_tank_calc.

    ENDIF.
    IF <lfs_tocnf_hdr>-anzms IS INITIAL.
      CLEAR: <lfs_tocnf_hdr>-ism01.
    ENDIF.

    IF <lfs_tocnf_hdr>-anzms IS NOT INITIAL.
      CALL FUNCTION '/AGRI/G_CR_WRKSTN_READ_ROUTING'
        EXPORTING
          i_arbpl          = <lfs_tocnf_hdr>-arbpl "lv_arbpl
          i_vgwkz          = abap_true
          i_werks          = <lfs_tocnf_hdr>-werks ""ls_fmfpcnf-iwerk
        IMPORTING
          e_works          = lwa_rcr01
        EXCEPTIONS
          not_found        = 1
          type_not_allowed = 2
          wrong_parameters = 3
          OTHERS           = 4.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

      IF lwa_rcr01 IS NOT INITIAL.
        <lfs_tocnf_hdr>-arbid = lwa_rcr01-arbid.
        lv_arbid              = lwa_rcr01-arbid.

        CALL FUNCTION '/AGRI/G_CR_WC_READ_ACTIVTEXT'
          EXPORTING
            i_arbid   = lv_arbid
            i_date    = sy-datum
          IMPORTING
            e_text    = lwa_text
          EXCEPTIONS
            not_found = 1
            OTHERS    = 2.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.

        lv_timecalc  = <lfs_tocnf_hdr>-iedz - <lfs_tocnf_hdr>-isdz.

        lv_msehi = 'H'.
        CALL FUNCTION 'START_TIME_DETERMINE'
          EXPORTING
            duration                   = 1
            unit                       = lv_msehi
          IMPORTING
            start_time                 = lv_uzeit
          CHANGING
            end_date                   = <lfs_tocnf_hdr>-iedd "<fs_fmfpcnf>-iedd
            end_time                   = lv_timecalc
          EXCEPTIONS
            factory_calendar_not_found = 1
            date_out_of_calendar_range = 2
            date_not_valid             = 3
            unit_conversion_error      = 4
            si_unit_missing            = 5
            parameters_not_valid       = 6.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.

        lv_timecalc = lv_uzeit.
        lv_times    = lv_timecalc.
        lv_times    = lv_times(2) && ':' && lv_times+2(2).

        cl_hrpadjp_time_utility=>hhmm_to_hour(
        EXPORTING
          iv_hhmm   = lv_times
         IMPORTING
          ev_hour   = lv_exhour ).

        lv_timecalc = <lfs_tocnf_hdr>-iedz - <lfs_tocnf_hdr>-isdz.
        lv_times    = lv_timecalc.
        lv_times    = lv_times(2) && ':' && lv_times+2(2).

        cl_hrpadjp_time_utility=>hhmm_to_hour(
        EXPORTING
          iv_hhmm  = lv_times
         IMPORTING
          ev_hour  = lv_exhourt ).

        IF lv_exhourt >= lc_timecons.
          <lfs_tocnf_hdr>-ism01 = lv_exhour.
        ENDIF.

        CLEAR lv_ism01.
        lv_ism01 = <lfs_tocnf_hdr>-ism01.
        DO 6 TIMES.
          lv_index = sy-index.
          CONCATENATE 'TEXT' lv_index INTO lv_fieldname.
          ASSIGN COMPONENT lv_fieldname OF STRUCTURE lwa_text
                                        TO <lv_text>.
          CHECK <lv_text> IS ASSIGNED.
          IF <lv_text> IS INITIAL.
            EXIT.
          ENDIF.

*          ls_woactivities-text1 = <lv_text>.

          "-----------------------------------
          CASE lv_index.
            WHEN 1.
              <lfs_tocnf_hdr>-ism01 = lv_ism01 * <lfs_tocnf_hdr>-anzms.
              IF NOT <lfs_tocnf_hdr>-ism01 IS INITIAL ."<fs_fmfpcnf>-ism01 IS INITIAL.
                <lfs_tocnf_hdr>-leinh1 = 'H'.
              ENDIF.

            WHEN 2.
              <lfs_tocnf_hdr>-ism02 = lv_ism01 * <lfs_tocnf_hdr>-anzms.
              IF NOT  <lfs_tocnf_hdr>-ism02 IS INITIAL ."<fs_fmfpcnf>-ism02 IS INITIAL.
                <lfs_tocnf_hdr>-leinh2 = 'H'.
              ENDIF.

            WHEN 3.

              <lfs_tocnf_hdr>-ism03 = lv_ism01 * <lfs_tocnf_hdr>-anzms.
              IF NOT <lfs_tocnf_hdr>-ism03 IS INITIAL ."<fs_fmfpcnf>-ism03 IS INITIAL.
                <lfs_tocnf_hdr>-leinh3 = 'H'.
              ENDIF.

            WHEN 4.

              <lfs_tocnf_hdr>-ism04 = lv_ism01 * <lfs_tocnf_hdr>-anzms.
              IF NOT <lfs_tocnf_hdr>-ism04 IS INITIAL ."<fs_fmfpcnf>-ism04 IS INITIAL.
                <lfs_tocnf_hdr>-leinh4 = 'H'.
              ENDIF.

            WHEN 5.

              <lfs_tocnf_hdr>-ism05 = lv_ism01 * <lfs_tocnf_hdr>-anzms.
              IF NOT <lfs_tocnf_hdr>-ism05 IS INITIAL ."<fs_fmfpcnf>-ism05 IS INITIAL.
                <lfs_tocnf_hdr>-leinh5 = 'H'.
              ENDIF.
            WHEN 6.

              <lfs_tocnf_hdr>-ism06 = lv_ism01 * <lfs_tocnf_hdr>-anzms.
              IF NOT <lfs_tocnf_hdr>-ism06 IS INITIAL ."<fs_fmfpcnf>-ism06 IS INITIAL.
                <lfs_tocnf_hdr>-leinh6 = 'H'.
              ENDIF.

          ENDCASE. "lv_index
        ENDDO.
      ENDIF. "lv_index.
    ENDIF. "<lfs_tocnf_hdr>-anzms

*--Prallel Cursor for Components tabel
    READ TABLE ct_tocnf_itm TRANSPORTING NO FIELDS
              WITH KEY aufnr      = <lfs_tocnf_hdr>-aufnr
                       vornr      = <lfs_tocnf_hdr>-vornr
                       dttimstamp = <lfs_tocnf_hdr>-dttimstamp
               BINARY SEARCH.
    IF sy-subrc EQ 0.
      lv_tabix = sy-tabix.
      LOOP AT ct_tocnf_itm ASSIGNING FIELD-SYMBOL(<lfs_tocnf_itm>) FROM lv_tabix.
        IF <lfs_tocnf_itm>-dttimstamp NE <lfs_tocnf_hdr>-dttimstamp.
          EXIT.
        ENDIF.

        APPEND INITIAL LINE TO lt_fpcom[] ASSIGNING FIELD-SYMBOL(<lfs_fpcom>).
        MOVE-CORRESPONDING <lfs_tocnf_itm> TO <lfs_fpcom>.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <lfs_fpcom>-aufnr
          IMPORTING
            output = <lfs_fpcom>-aufnr.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <lfs_fpcom>-matnr
          IMPORTING
            output = <lfs_fpcom>-matnr.

        <lfs_fpcom>-zzreason = <lfs_tocnf_itm>-reason.
        <lfs_fpcom>-lmnga    = <lfs_fpcom>-erfmg.
        <lfs_fpcom>-vornr    = <lfs_tocnf_hdr>-vornr.

        IF <ls_fpcom>-zzreason IS NOT INITIAL
          AND lv_reason IS INITIAL.
          lv_reason = abap_true.
        ENDIF.

        IF <ls_fpcom>-rspos = 0.
          READ TABLE lt_fpcomd ASSIGNING FIELD-SYMBOL(<ls_fpcomd>)
                                WITH KEY aufnr = <ls_fpcom>-aufnr
                                         posnr = <ls_fpcom>-posnr
                                         contr = <ls_fpcom>-contr.
          IF sy-subrc = 0.
            <ls_fpcom>-rspos = <ls_fpcomd>-rspos.
            <ls_fpcom>-lgort = <ls_fpcomd>-lgort.
          ENDIF.
        ENDIF.
        UNASSIGN <ls_fpcomd>.

        IF lv_reason IS NOT INITIAL.

          IF lobj_db IS BOUND.
            CALL METHOD lobj_db->delete_from_database
              EXPORTING
                tabname          = 'INDX'
                client           = sy-mandt
                area             = 'ID'
                id               = lc_memreason
                client_specified = abap_false.

            EXPORT lt_fpcom FROM ct_tocnf_itm TO DATABASE indx(id) ID lc_memreason.
          ENDIF.
        ENDIF.

        IF  'S' NOT IN lr_fpcnf
          AND 'B' NOT IN lr_fpcnf.
          REFRESH ct_tocnf_itm.
        ENDIF.


*        sy-uname = <lfs_tocnf_hdr>-mobusr.

      ENDLOOP. "ct_tocnf_itm
    ENDIF. "ct_tocnf_itm

  ENDLOOP. "ct_tocnf_hdr

  IF ct_tocnf_hdr IS NOT INITIAL.
*    REFRESH lt_fmfp_doc.
    DO 5 TIMES.
      IF lv_commit_true IS INITIAL.
        CALL FUNCTION '/AGRI/FMFP_ORDER_CONFIRM'
          EXPORTING
            i_commit_work     = abap_true
            it_fmfpcnf        = ct_tocnf_hdr
            it_fmfpcom        = lt_fpcom
          IMPORTING
            et_messages       = lt_messages
            et_fpdoc          = lt_fmfp_doc
          EXCEPTIONS
            inconsistent_data = 1
            OTHERS            = 2.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ELSE.
          WAIT UP TO 1 SECONDS.
          lv_commit_true = 'X'.   "c_true.
        ENDIF.
        WAIT UP TO 1 SECONDS.
      ENDIF. "lv_commit_true
    ENDDO.
  ENDIF. "ct_tocnf_hdr

  LOOP AT lt_messages INTO ls_messages..

    CONCATENATE ls_messages-msgv1
                ls_messages-msgv2
                ls_messages-msgv3
                ls_messages-msgv4
              INTO ls_tocnf_elog-msgtxt
              SEPARATED BY space.

    ls_str_tocnfmlog-msgid  = ls_messages-msgid.
    ls_str_tocnfmlog-msgno  = ls_messages-msgno.
    ls_str_tocnfmlog-msgtyp = ls_messages-msgty.
    ls_str_tocnfmlog-msgtxt = ls_tocnf_elog-msgtxt.
    APPEND ls_str_tocnfmlog TO gt_str_tocnfmlog.
    CLEAR : ls_str_tocnfmlog-msgid, ls_str_tocnfmlog-msgno,
            ls_str_tocnfmlog-msgtyp, ls_str_tocnfmlog-msgtxt, ls_messages.

    IF ls_messages-msgty = 'E'.
*    OR ls_messages-msgty = 'W'.
      CONCATENATE ls_messages-msgv1
                  ls_messages-msgv2
                  ls_messages-msgv3
                  ls_messages-msgv4
                INTO ls_tocnf_elog-msgtxt
                SEPARATED BY space.
      ls_tocnf_elog-msgid  = ls_messages-msgid.
      ls_tocnf_elog-msgno  = ls_messages-msgno.
      ls_tocnf_elog-msgtyp = ls_messages-msgty.
      ls_tocnf_elog-ernam  = sy-uname.
      ls_tocnf_elog-erdat  = sy-datum.
      ls_tocnf_elog-erzet  = sy-uzeit.
      APPEND ls_tocnf_elog TO gt_tocnf_elog.
      CLEAR : ls_tocnf_elog-msgid, ls_tocnf_elog-msgno, ls_tocnf_elog-msgtyp,
              ls_tocnf_elog-msgtxt,ls_messages.
    ENDIF.
  ENDLOOP. "lt_messages

  IF ct_tocnf_hdr IS NOT INITIAL.
    MODIFY zabs_tocnf_hdr FROM TABLE ct_tocnf_hdr.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

  IF gt_tocnf_elog IS NOT INITIAL.
    MODIFY zabs_tocnf_elog FROM TABLE gt_tocnf_elog.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

ENDFORM.
