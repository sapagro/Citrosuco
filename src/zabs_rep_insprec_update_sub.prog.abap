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

  DATA:
*-- Tables
*    lt_return        TYPE TABLE OF bapiret2,
*    lt_char_result   TYPE TABLE OF bapi2045d2,
*    lt_sample_result TYPE TABLE OF bapi2045d3,
*    lt_charres       TYPE TABLE OF bapi2045d2,
*    lt_singres       TYPE TABLE OF bapi2045d4,
*    lt_sampres       TYPE TABLE OF bapi2045d3,
*    lt_char_req_tmp  TYPE TABLE OF bapi2045d1,
*    lt_insppoints    TYPE TABLE OF bapi2045l4,
*    lt_mobqapp_rec   TYPE TABLE OF zabs_mobqapp_rec,
*    lt_messages      TYPE /agri/t_gprolog,
*    lt_qchar_elog    TYPE TABLE OF zabs_qchar_erlog,

    lt_qchar_hdr TYPE zabs_tty_qchar_hdr,
    ls_qchar_hdr TYPE zabs_qchar_hdr.

**-- Structures
*    ls_qchar_elog    TYPE zabs_qchar_erlog,
*    ls_str_qcharmlog TYPE zabs_str_qcharmlog,
*    ls_return        TYPE bapiret2,
*    ls_char_result   TYPE bapi2045d2,
*    ls_sample_result TYPE bapi2045d3,
*    ls_inspoint      TYPE bapi2045l4,
*    ls_messages      TYPE /agri/s_gprolog,
**-- Variables
*    lv_insplot       TYPE qals-prueflos,
*    lv_inspoper      TYPE qapo-vornr,
*    lv_inspchar      TYPE qamv-merknr,
**    lv_inspsample    TYPE qibpppktnr,
*    lv_isample_z28   TYPE qibpppktnr,
*    lv_slwbez        TYPE qslwbez,
*    lv_tplnrfl       TYPE /agri/gltplnr_fl,
*    lv_strno         TYPE /agri/glstrno,
*    lv_rbnr1         TYPE zabs_del_setor,
*    lv_insppoint     TYPE char1,
**    lv_msgtxt        TYPE BAPI_MSG,
*    ex_t_rettab      TYPE bapiret2_t,
*    ch_s_insppoint   TYPE bapi2045l4,
*
*    lv_pernr         TYPE p_pernr,
*    lv_inspdate      TYPE datum,
*    lv_insptime      TYPE uzeit.
*
**  lv_insplot   = ch_s_qualchar-insplot.
**  lv_inspoper  = ch_s_qualchar-inspoper.
**  lv_inspchar  = ch_s_qualchar-inspchar.
***---BOC "28/07/2020
**  lv_slwbez    = ch_s_qualchar-slwbez.
**  lv_tplnrfl   = ch_s_qualchar-tplnr_fl.
**  lv_strno     = ch_s_qualchar-strno.
**  lv_rbnr1     = ch_s_qualchar-rbnr1.
**  lv_insppoint = ch_s_qualchar-insp_point.
***---EOC "28/07/2020

  lt_qchar_hdr = p_qchdr.

  IF lt_qchar_hdr IS INITIAL.
    SELECT *
      FROM zabs_qchar_hdr
      INTO TABLE lt_qchar_hdr
     WHERE insplot    IN s_inslot
       AND inspoper   IN s_insopr
       AND dttimstamp IN s_dtstmp
       AND inspdate   IN s_insdt
       AND cflag      EQ p_fcflg"--On 27.10.2020 T_C.KARANAM
       AND recflag    EQ space
*       and batch       EQ space
       AND ernam      IN s_musr
       AND erdat      IN s_erdat
       AND erzet      IN s_erzet.
    IF sy-subrc <> 0.
      MESSAGE TEXT-004 TYPE zcl_abs_abap_maintain=>c_msgty_info.
      LEAVE LIST-PROCESSING.
    ENDIF.

    DELETE lt_qchar_hdr WHERE batch EQ 'B'.
    IF lt_qchar_hdr IS INITIAL.
      MESSAGE TEXT-007 TYPE zcl_abs_abap_maintain=>c_msgty_info
                       DISPLAY LIKE zcl_abs_abap_maintain=>c_msgty_error.
      LEAVE LIST-PROCESSING.
    ENDIF.

    IF p_par IS NOT INITIAL.
*      LOOP AT lt_qchar_hdr INTO ls_qchar_hdr.
      ls_qchar_hdr-batch = 'B'.
      ls_qchar_hdr-aenam = sy-uname.
      ls_qchar_hdr-aedat = sy-datum.
      ls_qchar_hdr-aezet = sy-uzeit.
      MODIFY lt_qchar_hdr FROM ls_qchar_hdr TRANSPORTING batch aenam aedat aezet
      WHERE insplot NE space.
      MODIFY zabs_qchar_hdr FROM TABLE lt_qchar_hdr.
      COMMIT WORK AND WAIT.
*      ENDLOOP.
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

*--Fetching ErrprLog data to check if the record is in the table with error
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
  SORT lt_qchar_hdr BY insplot inspoper dttimstamp."--On 23.10.2020 t_c.karanam
  IF p_par IS NOT INITIAL.
    PERFORM parallel_processing USING lt_qchar_hdr.
    MESSAGE TEXT-006 TYPE zcl_abs_abap_maintain=>c_msgty_success.
  ELSE.
    PERFORM result_recording USING lt_qchar_hdr
                                   lt_qchar_itm
                                   lt_qchar_errorlog.
  ENDIF.

*  LOOP AT lt_qchar_hdr INTO DATA(ls_qchar_hdr).
*
**---Record is already processed and sitting in Error Log.
*    READ TABLE lt_qchar_errorlog INTO DATA(ls_qchar_errorlog)
*    WITH KEY insplot    = ls_qchar_hdr-insplot
*             inspoper   = ls_qchar_hdr-inspoper
*             dttimstamp = ls_qchar_hdr-dttimstamp
*               BINARY SEARCH.
*    IF sy-subrc = 0.
*      CONTINUE.
*    ENDIF.
*
*    lv_insplot     = ls_qchar_hdr-insplot.
*    lv_inspoper    = ls_qchar_hdr-inspoper.
**    lv_inspchar   = ls_qchar_hdr-inspchar.
*    lv_slwbez      = ls_qchar_hdr-slwbez.
*    lv_tplnrfl     = ls_qchar_hdr-tplnr_fl.
*    lv_strno       = ls_qchar_hdr-strno.
*    lv_rbnr1       = ls_qchar_hdr-rbnr1.
*    lv_insppoint   = ls_qchar_hdr-insp_point.
*    lv_isample_z28 = ls_qchar_hdr-inspsample.
*
*    ls_qchar_elog-insplot    = ls_qchar_hdr-insplot.
*    ls_qchar_elog-inspoper   = ls_qchar_hdr-inspoper.
*    ls_qchar_elog-dttimstamp = ls_qchar_hdr-dttimstamp.
*
*    ls_str_qcharmlog-insplot    = ls_qchar_hdr-insplot.
*    ls_str_qcharmlog-inspoper   = ls_qchar_hdr-inspoper.
*    ls_str_qcharmlog-dttimstamp = ls_qchar_hdr-dttimstamp.
*
*    PERFORM inspection_lot_flag_check USING lv_insplot
*                                            lv_inspoper
*                                   CHANGING ls_messages.
*
*    IF ls_messages IS NOT INITIAL.
*      APPEND ls_messages TO lt_messages.
*
*      CONCATENATE ls_messages-msgv1
*                  ls_messages-msgv2
*                  ls_messages-msgv3
*                  ls_messages-msgv4
*                  INTO ls_qchar_elog-msgtxt
*                  SEPARATED BY space.
*      ls_qchar_elog-msgid = ls_messages-msgid.
*      ls_qchar_elog-msgno = ls_messages-msgno.
*      ls_qchar_elog-msgtyp = ls_messages-msgty.
*      ls_str_qcharmlog-msgid  = ls_messages-msgid.
*      ls_str_qcharmlog-msgno  = ls_messages-msgno.
*      ls_str_qcharmlog-msgtyp = ls_messages-msgty.
*      ls_str_qcharmlog-msgtxt = ls_qchar_elog-msgtxt.
*      APPEND ls_qchar_elog    TO gt_qchar_elog.
*      APPEND ls_str_qcharmlog TO gt_str_qcharmlog.
*      CLEAR : ls_qchar_elog-msgid, ls_qchar_elog-msgno, ls_qchar_elog-msgtyp,
*              ls_qchar_elog-msgtxt,ls_str_qcharmlog-msgid, ls_str_qcharmlog-msgno,
*              ls_str_qcharmlog-msgtyp, ls_str_qcharmlog-msgtxt, ls_messages.
*      CONTINUE.
*
*    ENDIF.
*
**    REFRESH : lt_insppoints,
**              lt_char_req_tmp,
**              lt_sampres,
**              lt_charres,
**              lt_charres.
*    CALL FUNCTION 'BAPI_INSPOPER_GETDETAIL'
*      EXPORTING
*        insplot                = lv_insplot
*        inspoper               = lv_inspoper
*        read_insppoints        = abap_true
*        read_char_requirements = abap_true
*        read_char_results      = abap_true
*        read_sample_results    = abap_true
*        read_single_results    = abap_true
*      IMPORTING
*        return                 = ls_return
*      TABLES
*        insppoints             = lt_insppoints
*        char_requirements      = lt_char_req_tmp
*        sample_results         = lt_sampres
*        char_results           = lt_charres
*        single_results         = lt_singres.
*
*    SORT lt_char_req_tmp BY insplot inspoper inspchar.
*
*    IF ls_return-id IS NOT INITIAL
*    AND ls_return-type IS NOT INITIAL.
*      ls_str_qcharmlog-msgid  = ls_return-id.
*      ls_str_qcharmlog-msgno  = ls_return-number.
*      ls_str_qcharmlog-msgtyp = ls_return-type.
*      ls_str_qcharmlog-msgtxt = ls_return-message.
*      APPEND ls_str_qcharmlog TO gt_str_qcharmlog.
*      CLEAR : ls_str_qcharmlog-msgid,
*              ls_str_qcharmlog-msgno,
*              ls_str_qcharmlog-msgtyp,
*              ls_str_qcharmlog-msgtxt.
*    ENDIF.
*
*    IF ls_return-type = zcl_abs_abap_maintain=>c_msgty_error. "'E'
*      ls_qchar_elog-msgid  = ls_return-id.
*      ls_qchar_elog-msgno  = ls_return-number.
*      ls_qchar_elog-msgtyp = ls_return-type.
*      ls_qchar_elog-msgtxt = ls_return-message.
*      APPEND ls_qchar_elog TO gt_qchar_elog.
*      CLEAR : ls_qchar_elog-msgid,
*              ls_qchar_elog-msgno,
*              ls_qchar_elog-msgtyp,
*              ls_qchar_elog-msgtxt.
*      CONTINUE.
*    ENDIF.
*
*    SORT lt_insppoints BY insplot inspoper insppoint DESCENDING.
*
*    READ TABLE lt_qchar_itm TRANSPORTING NO FIELDS
*              WITH KEY insplot    = ls_qchar_hdr-insplot
*                       inspoper   = ls_qchar_hdr-inspoper
*                       dttimstamp = ls_qchar_hdr-dttimstamp
*               BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      DATA(lv_tabix) = sy-tabix.
*      LOOP AT lt_qchar_itm INTO DATA(ls_qchar_itm) FROM lv_tabix.
*        IF ls_qchar_itm-insplot    NE ls_qchar_hdr-insplot
*        OR ls_qchar_itm-inspoper   NE ls_qchar_hdr-inspoper
*        OR ls_qchar_itm-dttimstamp NE ls_qchar_hdr-dttimstamp.
*          EXIT.
*        ENDIF.
*
*        ls_char_result-insplot      = ls_qchar_itm-insplot.
*        ls_char_result-inspoper     = ls_qchar_itm-inspoper.
*        ls_char_result-inspchar     = ls_qchar_itm-inspchar.
*        lv_inspchar                 = ls_qchar_itm-inspchar.
*
*        ls_sample_result-insplot    = lv_insplot.
*        ls_sample_result-inspoper   = lv_inspoper.
*
**--------Boc - 28/07/2020
**        CLEAR lv_inspsample.
*        SELECT MAX( probenr )
*          FROM qapp
*          INTO @DATA(lv_inspsample)
*          WHERE prueflos = @lv_insplot.
*
**      lv_inspsample = lv_inspsample + 1.
*
***-- For Z28 - Sample wi/Insp Point will always be in the Lot and we do update for the sample
***-- for Z23 - We may have inp sample /insp point and use it to increae sample number or iterate from 0.
***        SORT lt_insppoints BY insplot inspoper insppoint DESCENDING.
**        CLEAR lv_inspsample.
**        READ TABLE lt_insppoints INTO DATA(ls_insppoints) INDEX 1.
**        IF sy-subrc = 0.
**          lv_inspsample = ls_insppoints-insppoint.
**        ENDIF.
*
*        IF ls_qchar_hdr-slwbez EQ 'Z23'."and lv_inspsample is INITIAL.
*          lv_inspsample = lv_inspsample + 1.
*        ENDIF.
*
*        ls_sample_result-insplot    = lv_insplot.
*        ls_sample_result-inspoper   = lv_inspoper.
*        ls_sample_result-closed      = abap_true.
*        ls_sample_result-evaluated   = abap_true.
*        ls_sample_result-evaluation  = 'A'.
**    READ TABLE lt_char_req_tmp ASSIGNING FIELD-SYMBOL(<fs_char_req_tmp>)
**                               WITH KEY mstr_char = <fs_qualchrf4>-mstr_char.
*        READ TABLE lt_char_req_tmp ASSIGNING FIELD-SYMBOL(<fs_char_req_tmp>)
*                                   WITH KEY insplot  =  lv_insplot
*                                            inspoper = lv_inspoper
*                                            inspchar = ls_qchar_itm-inspchar
*                                            BINARY SEARCH.
*
**    IF sy-subrc EQ 0.
*        IF <fs_char_req_tmp> IS ASSIGNED.
*
*          ls_sample_result-inspchar = <fs_char_req_tmp>-inspchar.
*
*          IF <fs_char_req_tmp>-char_type = '01'.
*
**        ls_sample_result-mean_value = <fs_qualchrf4>-code.
*            ls_sample_result-mean_value = ls_qchar_itm-value.
*
*          ELSEIF <fs_char_req_tmp>-char_type = '02'.
*
**        ls_sample_result-code1     = <fs_qualchrf4>-code.
*            ls_sample_result-code1     = ls_qchar_itm-value.
**        ls_sample_result-code_grp1 = <fs_qualchrf4>-mstr_char.
*            ls_sample_result-code_grp1 = ls_qchar_itm-mstr_char.
*
*          ENDIF.
*
*        ENDIF.
*
*        MOVE-CORRESPONDING ls_sample_result TO ls_char_result.
*
*        ls_char_result-inspector   = ls_qchar_hdr-pernr.
*        ls_char_result-start_date  = ls_qchar_hdr-inspdate.
*        ls_char_result-start_time  = ls_qchar_hdr-insptime.
*        ls_char_result-end_date    = ls_qchar_hdr-inspdate.
*        ls_char_result-end_time    = ls_qchar_hdr-insptime.
*
*        IF <fs_char_req_tmp> IS ASSIGNED.
*          IF <fs_char_req_tmp>-sample_res = abap_true.
*            ls_sample_result-inspsample = lv_inspsample.
*            CLEAR ls_char_result-evaluation.
*            APPEND: ls_sample_result TO lt_sample_result.
*          ENDIF.
*        ENDIF.
*
*        APPEND: ls_char_result   TO lt_char_result.
*
*        CLEAR: ls_sample_result-mean_value, ls_sample_result-code1,
*               ls_sample_result-code_grp1, ls_sample_result-inspchar,
*               ls_char_result.
*
*      ENDLOOP. "lt_qchar_itm
*    ENDIF. "lt_qchar_itm
*
*    ls_inspoint-insplot       = lv_insplot.
*    ls_inspoint-inspoper      = lv_inspoper.
**    ls_qchar_hdr-insp_point   = abap_true.
*
*    ls_inspoint-insppoint = lv_inspsample.
*
**    ls_inspoint-userc1        = ch_s_insppoint-userc1.
**    ls_inspoint-userc2        = ch_s_insppoint-userc2.
**    ls_inspoint-usern1        = ch_s_insppoint-usern1.
*
*    CLEAR : lv_pernr,
*            lv_inspdate,
*            lv_insptime,
*            ls_inspoint-insp_date,
*            ls_inspoint-insp_time,
*            ls_inspoint-inspector.
**  READ TABLE lt_qualchar INTO ls_qualchar INDEX 1.
**  IF sy-subrc EQ 0.
*    lv_pernr     = ls_qchar_hdr-pernr.
*    lv_inspdate  = ls_qchar_hdr-inspdate.
*    lv_insptime  = ls_qchar_hdr-insptime.
*    ls_inspoint-insp_date = ls_qchar_hdr-inspdate.
*    ls_inspoint-insp_time = ls_qchar_hdr-insptime.
*
*    PERFORM insppoints_data_map USING lv_insplot
*                                      lv_inspoper
*                                      lv_pernr
*                                      lv_inspdate
*                                      lv_insptime
*                                      lv_slwbez
*                                      lv_tplnrfl
*                                      lv_strno
*                                      lv_rbnr1
*                                      lv_isample_z28
**                                      lv_insppoint
*                             CHANGING ls_inspoint.
*
*    ch_s_insppoint = CORRESPONDING #( ls_inspoint  ).
*
*    DO 10 TIMES.
*
*      REFRESH : ex_t_rettab.
*
*      sy-uname = ls_inspoint-inspector = ls_qchar_hdr-mobusr.
*
*      CALL FUNCTION 'BAPI_INSPOPER_RECORDRESULTS'
*        EXPORTING
*          insplot        = lv_insplot
*          inspoper       = lv_inspoper
*          insppointdata  = ls_inspoint
*        IMPORTING
*          return         = ls_return
*        TABLES
*          char_results   = lt_char_result
*          sample_results = lt_sample_result
*          returntable    = ex_t_rettab.
*      READ TABLE ex_t_rettab TRANSPORTING NO FIELDS
*                             WITH KEY type   = 'E'
*                                      id     = 'QT'
*                                      number = '400'.
*      IF sy-subrc <> 0.
*        EXIT.
*      ELSE.
*        WAIT UP TO 1 SECONDS.
*      ENDIF.
*    ENDDO.
*
*    READ TABLE ex_t_rettab TRANSPORTING NO FIELDS
*                           WITH KEY type = 'E'.
*
*    IF sy-subrc <> 0.
*
*      ls_str_qcharmlog-msgid  = '00'.
*      ls_str_qcharmlog-msgno  = '208'.
*      ls_str_qcharmlog-msgtyp = 'S'.
*      ls_str_qcharmlog-msgtxt = TEXT-002.
*      APPEND ls_str_qcharmlog TO gt_str_qcharmlog.
*      CLEAR : ls_str_qcharmlog-msgid,
*              ls_str_qcharmlog-msgno,
*              ls_str_qcharmlog-msgtyp,
*              ls_str_qcharmlog-msgtxt.
*
*      IF ls_qchar_hdr IS NOT INITIAL.
*        ls_qchar_hdr-recflag = abap_true.
*        ls_qchar_hdr-aenam = sy-uname.
*        ls_qchar_hdr-aedat = sy-datum.
*        ls_qchar_hdr-aezet = sy-uzeit.
*        MODIFY zabs_qchar_hdr FROM ls_qchar_hdr.
*        IF sy-subrc = 0.
*          COMMIT WORK AND WAIT.
*        ENDIF.
*      ENDIF.
*
*    ELSE.
*
*      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*
*      LOOP AT ex_t_rettab ASSIGNING FIELD-SYMBOL(<fs_rettab>).
*        IF <fs_rettab>-type = 'E'.
*          ls_messages-msgty = 'W'.
*        ELSE.
*          ls_messages-msgty = <fs_rettab>-type.
*        ENDIF.
*
*        ls_qchar_elog-msgid = <fs_rettab>-id.
*        ls_qchar_elog-msgno = <fs_rettab>-number.
*        ls_qchar_elog-msgtyp = <fs_rettab>-type.
*        ls_qchar_elog-msgtxt = <fs_rettab>-message.
*        ls_str_qcharmlog-msgid  = <fs_rettab>-id.
*        ls_str_qcharmlog-msgno  = <fs_rettab>-number.
*        ls_str_qcharmlog-msgtyp = <fs_rettab>-type.
*        ls_str_qcharmlog-msgtxt = <fs_rettab>-message.
*        APPEND ls_qchar_elog    TO gt_qchar_elog.
*        APPEND ls_str_qcharmlog TO gt_str_qcharmlog.
*        CLEAR : ls_qchar_elog-msgid, ls_qchar_elog-msgno, ls_qchar_elog-msgtyp,
*                ls_qchar_elog-msgtxt,ls_str_qcharmlog-msgid, ls_str_qcharmlog-msgno,
*                ls_str_qcharmlog-msgtyp, ls_str_qcharmlog-msgtxt, ls_messages.
*
*      ENDLOOP.
*
*    ENDIF.
*
*    IF ex_t_rettab IS INITIAL.
*
**-- Custom Table Update Logic
*      IF ls_qchar_hdr-cflag = abap_true.
*
*        PERFORM inspection_lot_flag_update USING lv_insplot
*                                                 lv_inspoper
*                                        CHANGING ls_messages.
*
*        IF ls_messages IS NOT INITIAL.
*          CONCATENATE ls_messages-msgv1
*                ls_messages-msgv2
*                ls_messages-msgv3
*                ls_messages-msgv4
*                INTO ls_str_qcharmlog-msgtxt
*                SEPARATED BY space.
*          ls_str_qcharmlog-msgid = ls_messages-msgid.
*          ls_str_qcharmlog-msgno = ls_messages-msgno.
*          ls_str_qcharmlog-msgtyp = ls_messages-msgty.
*          APPEND ls_str_qcharmlog TO gt_str_qcharmlog.
*          CLEAR : ls_str_qcharmlog-msgid,
*                  ls_str_qcharmlog-msgno,
*                  ls_str_qcharmlog-msgtyp,
*                  ls_str_qcharmlog-msgtxt,
*                  ls_messages.
*        ENDIF.
*
*      ENDIF.
*
*      CLEAR: ls_return.
*
*      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*        EXPORTING
*          wait   = abap_true
*        IMPORTING
*          return = ls_return.
*
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = ch_s_insppoint-userc1
*        IMPORTING
*          output = ch_s_insppoint-userc1.
*
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = ch_s_insppoint-userc2
*        IMPORTING
*          output = ch_s_insppoint-userc2.
*
*      IF ls_return IS INITIAL.
*
*        WAIT UP TO 1 SECONDS.
*
*        SELECT SINGLE prueflos, vorglfnr, probenr, ppsortkey
*        FROM qapp
*        INTO @DATA(ls_inspqapp)
*        WHERE prueflos = @lv_insplot
*          AND userc1 = @ch_s_insppoint-userc1
*          AND userc2 = @ch_s_insppoint-userc2
*          AND usern1 = @ch_s_insppoint-usern1
*          AND usern2 = @ch_s_insppoint-usern2
*          AND userd1 = @ch_s_insppoint-userd1
*          AND usert1 = @ch_s_insppoint-usert1.
*
*        APPEND INITIAL LINE TO lt_mobqapp_rec ASSIGNING FIELD-SYMBOL(<fs_mobqapp_rec>).
*        <fs_mobqapp_rec>-prueflos  = ls_inspqapp-prueflos.
*        <fs_mobqapp_rec>-vorglfnr  = ls_inspqapp-vorglfnr.
*        <fs_mobqapp_rec>-probenr   = ls_inspqapp-probenr.
*        <fs_mobqapp_rec>-ppsortkey = ls_inspqapp-ppsortkey.
*        <fs_mobqapp_rec>-zzimei1   = ls_qchar_hdr-zzimei1.
*        <fs_mobqapp_rec>-zbadge    = ls_qchar_hdr-zbadge.
*        <fs_mobqapp_rec>-ernam     = sy-uname.
*        <fs_mobqapp_rec>-erdat     = sy-datum.
*        <fs_mobqapp_rec>-erzet     = sy-uzeit.
*
*        MODIFY zabs_mobqapp_rec FROM <fs_mobqapp_rec>.
*        IF sy-subrc = 0.
*          COMMIT WORK AND WAIT.
*        ENDIF.
*
*        APPEND ls_return TO ex_t_rettab.
**        ls_messages-msgty = ls_return-type.
*        ls_messages-msgid = ls_return-id.
*        ls_messages-msgno = ls_return-number.
*        ls_messages-msgv1 = ls_return-message_v1.
*        ls_messages-msgv2 = ls_return-message_v2.
*        ls_messages-msgv3 = ls_return-message_v3.
*        ls_messages-msgv4 = ls_return-message_v4.
*        IF ls_messages IS NOT INITIAL.
*          APPEND ls_messages TO lt_messages.
*        ENDIF.

*        ls_str_qcharmlog-msgid  = ls_return-id.
*        ls_str_qcharmlog-msgno  = ls_return-number.
*        ls_str_qcharmlog-msgtyp = ls_return-type.
*        ls_str_qcharmlog-msgtxt = ls_return-message.
*        APPEND ls_str_qcharmlog TO gt_str_qcharmlog.
*        CLEAR : ls_str_qcharmlog-msgid,
*                ls_str_qcharmlog-msgno,
*                ls_str_qcharmlog-msgtyp,
*                ls_str_qcharmlog-msgtxt.
*      ENDIF. "ls_return
*
*    ENDIF.
*    CLEAR : lv_insplot, lv_inspoper, ls_inspoint, ls_return.
*    REFRESH : lt_char_result, lt_sample_result.
*  ENDLOOP. "lt_qchar_hdr
*
*  IF gt_qchar_elog IS NOT INITIAL.
*    MODIFY zabs_qchar_erlog FROM TABLE gt_qchar_elog.
*    IF sy-subrc = 0.
*      COMMIT WORK AND WAIT.
*    ENDIF.
*  ENDIF.
*
ENDFORM.

*&---------------------------------------------------------------------*
*& Form INSPECTION_LOT_FLAG_CHECK
*&---------------------------------------------------------------------*
*& INSPECTION_LOT_FLAG_CHECK
*&---------------------------------------------------------------------*
FORM inspection_lot_flag_check  USING    pv_insplot  TYPE qals-prueflos
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
                           WITH KEY cvornr = pv_inspoper
                           BINARY SEARCH.
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
FORM insppoints_data_map  USING    pv_insplot   TYPE qals-prueflos
                                   pv_inspoper  TYPE qapo-vornr
                                   pv_pernr     TYPE p_pernr
                                   pv_inspdate  TYPE datum
                                   pv_insptime  TYPE uzeit
                                   pv_slwbez    TYPE qslwbez
                                   pv_tplnrfl   TYPE /agri/gltplnr_fl
                                   pv_strno     TYPE /agri/glstrno
                                   pv_rbnr1     TYPE zabs_del_setor
*                                   pv_insppoint TYPE char1
                                   pv_isample_z28 TYPE qibpppktnr
                          CHANGING ps_inspoint  TYPE bapi2045l4.

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

*  CASE ls_qals-slwbez.
  CASE pv_slwbez.
    WHEN 'Z23'.
      WAIT UP TO 2 SECONDS.
*      ch_s_insppoint-userc1 = sy-uname.
      ps_inspoint-userc1 = pv_pernr.
      ps_inspoint-userc2 = lv_fterra.
      ps_inspoint-usern1 = lv_lterra.
      ps_inspoint-usern2 = pv_rbnr1.
*      ch_s_insppoint-usern2 = ls_glflota-rbnr1.
*      ch_s_insppoint-userd1 = sy-datum.
*      ch_s_insppoint-usert1 = sy-uzeit.
      ps_inspoint-userd1 = pv_inspdate.
      ps_inspoint-usert1 = pv_insptime.
    WHEN 'Z24'.
*      ch_s_insppoint-userc1 = lv_fterra.
*      ch_s_insppoint-userc2 = ls_glflot-beber1.
*      ch_s_insppoint-usern1 = lv_week.
*      ch_s_insppoint-usern2 = lv_lterra.
*      ch_s_insppoint-userd1 = sy-datum.
    WHEN 'Z25'.

*      ch_s_insppoint-userc1  = lv_fterra.
*      ch_s_insppoint-userc2 = lv_lterra.
*      ch_s_insppoint-usern1 = im_v_rua.
*      ch_s_insppoint-usern2 = im_v_arvore.
*      ch_s_insppoint-userd1 = sy-datum.

    WHEN 'Z26' OR 'Z27'.

    WHEN 'Z28'.

      SELECT SINGLE a~plnkn
       FROM afvc AS a
       INNER JOIN qals AS b
       ON a~aufpl = b~aufpl
       INTO @DATA(lv_plnkn)
       WHERE b~prueflos = @pv_insplot
       AND a~vornr      = @pv_inspoper.

      SELECT SINGLE *
        FROM qapp
        INTO @DATA(ls_qapp)
        WHERE prueflos = @pv_insplot
          AND vorglfnr = @lv_plnkn.
      ps_inspoint = CORRESPONDING #( ls_qapp MAPPING  insplot   = prueflos
                                                       insppoint = probenr ).

*      ch_s_insppoint = CORRESPONDING #( ls_qapp MAPPING  insplot   = prueflos
*                                                         insppoint = probenr ).


      ps_inspoint-insplot   = pv_insplot.
      ps_inspoint-inspoper  = pv_inspoper.
*      ps_inspoint-insppoint = pv_isample_z28.
*      ps_inspoint-insppoint = pv_insppoint.
      ps_inspoint-insppoint = ls_qapp-probenr.
*      ch_s_insppoint-insppoint = ls_qapp-probenr.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form INSPECTION_LOT_FLAG_UPDATE
*&---------------------------------------------------------------------*
*& INSPECTION_LOT_FLAG_UPDATE
*&---------------------------------------------------------------------*
FORM inspection_lot_flag_update  USING    pv_insplot  TYPE qals-prueflos
                                          pv_inspoper TYPE qapo-vornr
                                 CHANGING ps_messages TYPE /agri/s_gprolog.

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

  DATA lr_steus TYPE RANGE OF steus.lr_steus =
  VALUE #( FOR ls_steust IN lt_constants (
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
*      lwa_mob_lot-zzcflag = 'X'.     " JOBREGON: Commented
    lwa_mob_lot-aenam   = sy-uname.
    lwa_mob_lot-aedat   = sy-datum.
    lwa_mob_lot-aezet   = sy-uzeit.

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
*      lwa_mob_lot-zzcflag = 'X'.
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
FORM result_recording  USING    pt_qchar_hdr      TYPE zabs_tty_qchar_hdr
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
*    lt_qchar_hdr     TYPE zabs_tty_qchar_hdr,
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
*    lv_value         TYPE char1,
    lv_inspdate      TYPE datum,
    lv_insptime      TYPE uzeit.

**--Begin of 23.10.2020
  IF pt_qchar_hdr IS NOT INITIAL.
    SELECT *
      FROM zabs_qchar_hdr
      INTO TABLE @DATA(lt_qchar_hdr)
      FOR ALL ENTRIES IN @pt_qchar_hdr
     WHERE insplot EQ @pt_qchar_hdr-insplot
       AND inspoper EQ @pt_qchar_hdr-inspoper
       AND cflag EQ @abap_true.
    SORT lt_qchar_hdr BY insplot inspoper.
  ENDIF.
**--End of 23.10.2020
  LOOP AT pt_qchar_hdr ASSIGNING FIELD-SYMBOL(<lfs_qchar_hdr>).

    <lfs_qchar_hdr>-recflag = abap_true.
    <lfs_qchar_hdr>-batch   = space.
    <lfs_qchar_hdr>-aenam   = sy-uname.
    <lfs_qchar_hdr>-aedat   = sy-datum.
    <lfs_qchar_hdr>-aezet   = sy-uzeit.

*---Record is already processed and sitting in Error Log.
    READ TABLE pt_qchar_errorlog INTO DATA(ls_qchar_errorlog)
    WITH KEY insplot    = <lfs_qchar_hdr>-insplot
             inspoper   = <lfs_qchar_hdr>-inspoper
             dttimstamp = <lfs_qchar_hdr>-dttimstamp
             BINARY SEARCH.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.

    lv_insplot     = <lfs_qchar_hdr>-insplot.
    lv_inspoper    = <lfs_qchar_hdr>-inspoper.
*    lv_inspchar   = <lfs_qchar_hdr>-inspchar.
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
**--Begin of 23.10.2020
    IF <lfs_qchar_hdr>-cflag EQ space.
      READ TABLE lt_qchar_hdr INTO DATA(ls_qchar_hdr_c)
      WITH KEY insplot  = <lfs_qchar_hdr>-insplot
               inspoper = <lfs_qchar_hdr>-inspoper BINARY SEARCH.
      IF sy-subrc EQ 0.
        IF ls_qchar_hdr_c-dttimstamp LE <lfs_qchar_hdr>-dttimstamp.
*          IF sy-subrc NE 0.
**--End of 23.10.2020
          PERFORM inspection_lot_flag_check USING lv_insplot
                                                  lv_inspoper
                                         CHANGING ls_messages.
**--Begin of 23.10.2020
        ENDIF.
      ENDIF.
    ELSE.
      PERFORM inspection_lot_flag_check USING lv_insplot
                                               lv_inspoper
                                      CHANGING ls_messages.
    ENDIF.

**--End of 23.10.2020
    IF ls_messages IS NOT INITIAL.
      APPEND ls_messages TO lt_messages.

      CONCATENATE ls_messages-msgv1
                  ls_messages-msgv2
                  ls_messages-msgv3
                  ls_messages-msgv4
                  INTO ls_qchar_elog-msgtxt
                  SEPARATED BY space.
      ls_qchar_elog-msgid = ls_messages-msgid.
      ls_qchar_elog-msgno = ls_messages-msgno.
      ls_qchar_elog-msgtyp = ls_messages-msgty.
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

*    REFRESH : lt_insppoints,
*              lt_char_req_tmp,
*              lt_sampres,
*              lt_charres,
*              lt_charres.
    CALL FUNCTION 'BAPI_INSPOPER_GETDETAIL'
      EXPORTING
        insplot                = lv_insplot
        inspoper               = lv_inspoper
        read_insppoints        = abap_true
        read_char_requirements = abap_true
        read_char_results      = abap_true
        read_sample_results    = abap_true
        read_single_results    = abap_true
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

    IF ls_return-type = zcl_abs_abap_maintain=>c_msgty_error. "'E'
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

    SORT lt_insppoints BY insplot inspoper insppoint DESCENDING.

*    CLEAR lv_value.
    READ TABLE pt_qchar_itm TRANSPORTING NO FIELDS
              WITH KEY insplot    = <lfs_qchar_hdr>-insplot
                       inspoper   = <lfs_qchar_hdr>-inspoper
                       dttimstamp = <lfs_qchar_hdr>-dttimstamp
               BINARY SEARCH.
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

*--------Boc - 28/07/2020
*        CLEAR lv_inspsample.
        SELECT MAX( probenr )
          FROM qapp
          INTO @DATA(lv_inspsample)
          WHERE prueflos = @lv_insplot.

*      lv_inspsample = lv_inspsample + 1.

**-- For Z28 - Sample wi/Insp Point will always be in the Lot and we do update for the sample
**-- for Z23 - We may have inp sample /insp point and use it to increae sample number or iterate from 0.
**        SORT lt_insppoints BY insplot inspoper insppoint DESCENDING.
*        CLEAR lv_inspsample.
*        READ TABLE lt_insppoints INTO DATA(ls_insppoints) INDEX 1.
*        IF sy-subrc = 0.
*          lv_inspsample = ls_insppoints-insppoint.
*        ENDIF.

        IF <lfs_qchar_hdr>-slwbez EQ 'Z23'."and lv_inspsample is INITIAL.
          lv_inspsample = lv_inspsample + 1.
        ENDIF.

        ls_sample_result-insplot    = lv_insplot.
        ls_sample_result-inspoper   = lv_inspoper.
        ls_sample_result-closed      = abap_true.
        ls_sample_result-evaluated   = abap_true.
        ls_sample_result-evaluation  = 'A'.
*    READ TABLE lt_char_req_tmp ASSIGNING FIELD-SYMBOL(<fs_char_req_tmp>)
*                               WITH KEY mstr_char = <fs_qualchrf4>-mstr_char.
        READ TABLE lt_char_req_tmp ASSIGNING FIELD-SYMBOL(<fs_char_req_tmp>)
                                   WITH KEY insplot  =  lv_insplot
                                            inspoper = lv_inspoper
                                            inspchar = ls_qchar_itm-inspchar
                                            BINARY SEARCH.

*    IF sy-subrc EQ 0.
        IF <fs_char_req_tmp> IS ASSIGNED.

          ls_sample_result-inspchar = <fs_char_req_tmp>-inspchar.

          IF <fs_char_req_tmp>-char_type = '01'.

*            IF ls_qchar_itm-value CO '-0123456789. '.
*              ls_sample_result-mean_value = ls_qchar_itm-value.
*            ELSE.
*              lv_value = abap_true.
*              ls_str_qcharmlog-msgid  = '00'.
*              ls_str_qcharmlog-msgno  = '208'.
*              ls_str_qcharmlog-msgtyp = zcl_abs_abap_maintain=>c_msgty_error.  "'E'
*              ls_str_qcharmlog-msgtxt = TEXT-009.
*
*              ls_qchar_elog-msgid  = '00'.
*              ls_qchar_elog-msgno  = '208'.
*              ls_qchar_elog-msgtyp = zcl_abs_abap_maintain=>c_msgty_error.  "'E'
*              ls_qchar_elog-msgtxt = TEXT-009.
*
*              APPEND : ls_str_qcharmlog TO gt_str_qcharmlog,
*                       ls_qchar_elog    TO gt_qchar_elog.
*              CLEAR : ls_str_qcharmlog-msgid, ls_str_qcharmlog-msgno, ls_str_qcharmlog-msgtyp,
*                      ls_str_qcharmlog-msgtxt, ls_qchar_elog-msgid, ls_qchar_elog-msgno,
*                      ls_qchar_elog-msgtyp, ls_qchar_elog-msgtxt.
*              EXIT.
*            ENDIF.

*            ls_sample_result-mean_value = <fs_qualchrf4>-code.
            ls_sample_result-mean_value = ls_qchar_itm-value.

          ELSEIF <fs_char_req_tmp>-char_type = '02'.

*        ls_sample_result-code1     = <fs_qualchrf4>-code.
            ls_sample_result-code1     = ls_qchar_itm-value.
*        ls_sample_result-code_grp1 = <fs_qualchrf4>-mstr_char.
            ls_sample_result-code_grp1 = ls_qchar_itm-mstr_char.

          ENDIF.

        ENDIF.

        MOVE-CORRESPONDING ls_sample_result TO ls_char_result.

        ls_char_result-inspector   = <lfs_qchar_hdr>-pernr.
        ls_char_result-start_date  = <lfs_qchar_hdr>-inspdate.
        ls_char_result-start_time  = <lfs_qchar_hdr>-insptime.
        ls_char_result-end_date    = <lfs_qchar_hdr>-inspdate.
        ls_char_result-end_time    = <lfs_qchar_hdr>-insptime.

        IF <fs_char_req_tmp> IS ASSIGNED.
          IF <fs_char_req_tmp>-sample_res = abap_true.
            ls_sample_result-inspsample = lv_inspsample.
            CLEAR ls_char_result-evaluation.
            APPEND: ls_sample_result TO lt_sample_result.
          ENDIF.
        ENDIF.

        APPEND: ls_char_result   TO lt_char_result.

        CLEAR: ls_sample_result-mean_value, ls_sample_result-code1,
               ls_sample_result-code_grp1, ls_sample_result-inspchar,
               ls_char_result.

      ENDLOOP. "lt_qchar_itm
    ENDIF. "lt_qchar_itm
*
*    IF lv_value = abap_true.
*      CONTINUE.
*    ENDIF.

    ls_inspoint-insplot       = lv_insplot.
    ls_inspoint-inspoper      = lv_inspoper.
*    <lfs_qchar_hdr>-insp_point   = abap_true.

    ls_inspoint-insppoint = lv_inspsample.

*    ls_inspoint-userc1        = ch_s_insppoint-userc1.
*    ls_inspoint-userc2        = ch_s_insppoint-userc2.
*    ls_inspoint-usern1        = ch_s_insppoint-usern1.

    CLEAR : lv_pernr,
            lv_inspdate,
            lv_insptime,
            ls_inspoint-insp_date,
            ls_inspoint-insp_time,
            ls_inspoint-inspector.
*  READ TABLE lt_qualchar INTO ls_qualchar INDEX 1.
*  IF sy-subrc EQ 0.
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
*                                      lv_insppoint
                             CHANGING ls_inspoint.

    ch_s_insppoint = CORRESPONDING #( ls_inspoint  ).

**    SET UPDATE TASK LOCAL.

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

*      IF <lfs_qchar_hdr> IS NOT INITIAL.
**        <lfs_qchar_hdr>-recflag = abap_true.
*        <lfs_qchar_hdr>-aenam   = sy-uname.
*        ls_qchar_hdr-aedat   = sy-datum.
*        ls_qchar_hdr-aezet   = sy-uzeit.
*        MODIFY zabs_qchar_hdr FROM ls_qchar_hdr.
*        IF sy-subrc = 0.
*          COMMIT WORK AND WAIT.
*        ENDIF.
*      ENDIF.

    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      LOOP AT ex_t_rettab ASSIGNING FIELD-SYMBOL(<fs_rettab>).
*        IF <fs_rettab>-type = 'E'.
*          ls_messages-msgty = 'W'.
*        ELSE.
*          ls_messages-msgty = <fs_rettab>-type.
*        ENDIF.

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
*         ls_qchar_elog-msgid, ls_qchar_elog-msgno, ls_qchar_elog-msgtyp,
*                ls_qchar_elog-msgtxt,

      ENDLOOP.

    ENDIF.

*    LOOP AT lt_qchar_hdr ASSIGNING FIELD-SYMBOL(<lfs_qchar_hdr>) FROM lv-tabix.
*      <lfs_qchar_hdr>-recflag = abap_true.
*    ENDLOOP.

    IF ex_t_rettab IS INITIAL.

*-- Custom Table Update Logic
      IF <lfs_qchar_hdr>-cflag = abap_true.

        PERFORM inspection_lot_flag_update USING lv_insplot
                                                 lv_inspoper
                                        CHANGING ls_messages.

        IF ls_messages IS NOT INITIAL.
*          APPEND ls_messages TO lt_messages.
          CONCATENATE ls_messages-msgv1
                ls_messages-msgv2
                ls_messages-msgv3
                ls_messages-msgv4
                INTO ls_str_qcharmlog-msgtxt
                SEPARATED BY space.
          ls_str_qcharmlog-msgid = ls_messages-msgid.
          ls_str_qcharmlog-msgno = ls_messages-msgno.
          ls_str_qcharmlog-msgtyp = ls_messages-msgty.
          APPEND ls_str_qcharmlog TO gt_str_qcharmlog.
          CLEAR : ls_str_qcharmlog-msgid,
                  ls_str_qcharmlog-msgno,
                  ls_str_qcharmlog-msgtyp,
                  ls_str_qcharmlog-msgtxt,
                  ls_messages.
        ENDIF.

      ENDIF.

      CLEAR: ls_return.
      COMMIT WORK AND WAIT.
**      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
**        EXPORTING
**          wait   = abap_true
**        IMPORTING
**          return = ls_return.

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
          AND userc1 = @ch_s_insppoint-userc1
          AND userc2 = @ch_s_insppoint-userc2
          AND usern1 = @ch_s_insppoint-usern1
          AND usern2 = @ch_s_insppoint-usern2
          AND userd1 = @ch_s_insppoint-userd1
          AND usert1 = @ch_s_insppoint-usert1.

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
    MODIFY zabs_qchar_hdr FROM TABLE pt_qchar_hdr.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

  LOOP AT gt_qchar_elog ASSIGNING FIELD-SYMBOL(<lfs_qchar_elog>).
    <lfs_qchar_elog>-ernam   = sy-uname.
    <lfs_qchar_elog>-erdat   = sy-datum.
    <lfs_qchar_elog>-erzet   = sy-uzeit.
  ENDLOOP.

  IF gt_qchar_elog IS NOT INITIAL.
    MODIFY zabs_qchar_erlog FROM TABLE gt_qchar_elog.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& PARALLEL_PROCESSING
*&---------------------------------------------------------------------*
*& PARALLEL_PROCESSING
*&---------------------------------------------------------------------*
FORM parallel_processing  USING pt_qchar_hdr TYPE zabs_tty_qchar_hdr.

*--Data declaration
  DATA : lt_qchdr        TYPE TABLE OF zabs_qchar_hdr,
         lt_qchdr_submit TYPE TABLE OF zabs_qchar_hdr,

         ls_joblog       TYPE ty_joblog,

         lv_job_name     TYPE btcjob,
         lv_job_number   TYPE btcjobcnt,
         lv_index        TYPE sy-index,
         lv_char_index   TYPE char20.

  lt_qchdr[] = pt_qchar_hdr[].

*-- Schedule the jobs based on Participants data
  DO.

    CLEAR: lv_job_name,
           lv_char_index,
           lv_job_number.

    REFRESH lt_qchdr_submit.
*-- Collect the Participant to be used while creating Calculation run
    LOOP AT lt_qchdr INTO DATA(ls_qchdr) FROM 1 TO p_psize.
      APPEND ls_qchdr TO lt_qchdr_submit.
      CLEAR ls_qchdr.
    ENDLOOP."lt_qchdr

*-- Prepare Job name
    lv_job_name   = p_jobpfx.
    lv_index      = sy-index.
    lv_char_index = lv_index.

    CONDENSE lv_char_index NO-GAPS.

*    CONCATENATE p_rdate p_time lv_counter
*               INTO lv_char_index SEPARATED BY '_'.

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
      SUBMIT zabs_rep_insprec_update
             WITH p_qchdr EQ lt_qchdr_submit
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
    DELETE lt_qchdr FROM 1 TO p_psize.

*-- Exit if all the participant data is scheduled in job
    IF lt_qchdr IS INITIAL.
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
*&      form  sub_job_status_check
*&---------------------------------------------------------------------*
FORM sub_job_status_check USING
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
