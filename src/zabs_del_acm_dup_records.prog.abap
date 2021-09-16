*&---------------------------------------------------------------------*
*& Report ZABS_CORRECTION_PRGM
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabs_del_acm_dup_records.

*-- Types Declaration
TYPES: BEGIN OF ty_final,
         accom  TYPE /agri/fmaccom,
         posnr  TYPE /agri/glposnr,
         msgty  TYPE zabs_del_msgtyp,
         msgtxt TYPE zabs_del_msgtext,
       END OF ty_final.

*-- Data Declarations
DATA:gv_accom     TYPE /agri/fmaccom,
     gv_date      TYPE /agri/fmastrtdat,
     gt_final_msg TYPE TABLE OF ty_final.

*-- Selection Screen
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: s_accom FOR gv_accom,
                s_stdat FOR gv_date.
SELECTION-SCREEN:END OF BLOCK b1.

AT SELECTION-SCREEN.

  IF s_accom-low IS NOT INITIAL.
    SELECT SINGLE accom
             INTO @DATA(lv_accom)
             FROM /agri/fmachdr
            WHERE accom IN @s_accom.
    IF sy-subrc <> 0.
      MESSAGE TEXT-004 TYPE zcl_abs_abap_maintain=>c_msgty_error.
    ENDIF.
  ENDIF.

  IF s_stdat-low IS NOT INITIAL.
    SELECT SINGLE strtdat
             INTO @DATA(lv_stdat)
             FROM /agri/fmacitm
            WHERE strtdat IN @s_stdat.
    IF sy-subrc <> 0.
      MESSAGE TEXT-005 TYPE zcl_abs_abap_maintain=>c_msgty_error.
    ENDIF.
  ENDIF.

*-- Start of Selection event
START-OF-SELECTION.

*-- message in selection screen
  IF s_accom IS INITIAL
    AND s_stdat IS INITIAL.
    MESSAGE TEXT-006 TYPE zcl_abs_abap_maintain=>c_msgty_info."'I'
    LEAVE LIST-PROCESSING.
  ENDIF.

*-- Fetching duplicate Accomplishment items
  PERFORM get_accom_data.

END-OF-SELECTION.
*-- Display data
  IF gt_final_msg IS NOT INITIAL.
    PERFORM display_data.
  ENDIF.

*&---------------------------------------------------------------------*
*& Form GET_ACCOM_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_accom_data .

*-- Local Declarations
  DATA: lt_accom     TYPE /agri/t_fmacom,
        gt_acdoc     TYPE /agri/t_fmacs_doc,
        lt_acdoc_tmp TYPE /agri/t_fmacs_doc,
        lt_acitm_new TYPE /agri/t_fmfmacitm,
        lt_acitm_dup TYPE /agri/t_fmfmacitm,
        lt_acitm_ctd TYPE /agri/t_fmfmacitm,
        lt_acitm_cnf TYPE /agri/t_fmfmacitm,
        lt_acitm     TYPE /agri/t_fmfmacitm,
        lt_messages  TYPE /agri/t_gprolog,
        lt_ocnum_new TYPE /agri/t_fmocnum,
        ls_ocnum_new TYPE /agri/s_fmocnum,
        lv_count1    TYPE i,
        lv_count2    TYPE i,
        ls_final     TYPE ty_final,
        ref_text     TYPE REF TO /agri/cl_gtext_process.

*-- Fetching Accomplishment sheets matched with
*  selection parameters of type TAS2 and TAS3
  SELECT  accom
    FROM /agri/fmachdr
    INTO TABLE   lt_accom
   WHERE accom   IN s_accom
     AND actyp   IN (zcl_abs_abap_maintain=>c_acctyp_tas2, "TAS2
                     zcl_abs_abap_maintain=>c_acctyp_tas3) "TAS3
     AND strtdat IN s_stdat.
  IF sy-subrc EQ 0.
*-- Fetching DOC structure
    CALL FUNCTION '/AGRI/FMAC_VIEW'
      EXPORTING
        it_accom       = lt_accom
      IMPORTING
        et_acdoc       = gt_acdoc
      EXCEPTIONS
        no_data_exists = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    IF gt_acdoc[] IS NOT INITIAL.
      lt_acdoc_tmp[] = gt_acdoc[].

      LOOP AT  lt_acdoc_tmp ASSIGNING FIELD-SYMBOL(<ls_acdoc_tmp>).
*-- Fetching item data
        lt_acitm_new[] = <ls_acdoc_tmp>-x-acitm[].
*-- Fetching ZZHDATE
        LOOP AT lt_acitm_new ASSIGNING FIELD-SYMBOL(<ls_acitm_new>) WHERE zzhdate IS INITIAL.
          <ls_acitm_new>-zzhdate = <ls_acitm_new>-strtdat.
        ENDLOOP.
        CLEAR lv_count1.
        DESCRIBE TABLE lt_acitm_new LINES lv_count1. "collecting count of item table

        SORT lt_acitm_new BY accom idresource zzhdate strttim posnr.
        DATA(lt_acitm_tmp) = lt_acitm_new[].
*-- Deleting the duplicate items in tmp table
        DELETE ADJACENT DUPLICATES FROM lt_acitm_tmp COMPARING accom idresource zzhdate strttim.
        CLEAR lv_count2.
        DESCRIBE TABLE lt_acitm_tmp LINES lv_count2. "collecting count after deleting duplicates

        IF lv_count1 EQ lv_count2. " No Duplicates
          DELETE gt_acdoc WHERE accom = <ls_acdoc_tmp>-accom.
        ELSE.
          SORT lt_acitm_new BY accom posnr.
*-- Fetching duplicat items into internal table
          LOOP AT lt_acitm_tmp INTO DATA(ls_acitm_tmp).
            READ TABLE lt_acitm_new INTO DATA(ls_acitm_new)
                                    WITH KEY accom = ls_acitm_tmp-accom
                                             posnr = ls_acitm_tmp-posnr
                                    BINARY SEARCH.
            IF sy-subrc EQ 0.
              DELETE lt_acitm_new WHERE accom = ls_acitm_tmp-accom
                                    AND posnr = ls_acitm_tmp-posnr.
            ENDIF.
          ENDLOOP.
          APPEND LINES OF lt_acitm_new TO lt_acitm_dup.
        ENDIF.
      ENDLOOP.

      IF gt_acdoc IS NOT INITIAL
       AND lt_acitm_dup IS NOT INITIAL.
        SORT lt_acitm_dup BY accom.

        LOOP AT gt_acdoc ASSIGNING FIELD-SYMBOL(<ls_acdoc>).

          READ TABLE lt_acitm_dup TRANSPORTING NO FIELDS
                                  WITH KEY accom = <ls_acdoc>-accom
                                  BINARY SEARCH.
          IF sy-subrc EQ 0.
            DATA(lv_tabix) = sy-tabix.
*-- Fetching CTD and CNF data
            LOOP AT lt_acitm_dup INTO DATA(ls_acitm_dup) FROM lv_tabix.

              IF ls_acitm_dup-accom <> <ls_acdoc>-accom.
                EXIT.
              ENDIF.

              IF <ls_acdoc>-x-achdr-actyp = zcl_abs_abap_maintain=>c_acctyp_tas2
               AND ls_acitm_dup-status = zcl_abs_abap_maintain=>c_ac_status_confirm.
                APPEND ls_acitm_dup TO lt_acitm_cnf.
              ELSE.
                APPEND ls_acitm_dup TO lt_acitm_ctd.
              ENDIF.

            ENDLOOP.
          ENDIF.

        ENDLOOP.

        IF lt_acitm_cnf IS NOT INITIAL.
*-- Fetching confirmation groups
          SELECT ocnum, rueck,
                 rmzhl, aufnr,
                 rueck_ref, rmzhl_ref
            FROM /agri/fmocopr
            INTO TABLE @DATA(lt_ocnum)
            FOR ALL ENTRIES IN @lt_acitm_cnf
            WHERE aufnr EQ @lt_acitm_cnf-aufnr
              AND rueck EQ @lt_acitm_cnf-rueck.
          IF sy-subrc EQ 0.
            SORT lt_ocnum BY rueck rmzhl aufnr.
            DATA(lt_ocnum1) = lt_ocnum.
            SORT lt_ocnum1 BY aufnr rueck_ref rmzhl_ref.
*-- Fetching reversed items into CTD and remaining in CNF table
            LOOP AT lt_acitm_cnf INTO DATA(ls_acitm_cnf).
              READ TABLE lt_ocnum1 INTO DATA(ls_ocnum1)
                                  WITH KEY rueck_ref = ls_acitm_cnf-rueck
                                           rmzhl_ref = ls_acitm_cnf-rmzhl
                                           BINARY SEARCH.
              IF sy-subrc EQ 0.
                APPEND ls_acitm_cnf TO lt_acitm_ctd.
                CLEAR ls_acitm_cnf.
              ELSE.
                READ TABLE lt_ocnum INTO DATA(ls_ocnum)
                                WITH KEY rueck = ls_acitm_cnf-rueck
                                         rmzhl = ls_acitm_cnf-rmzhl
                                         BINARY SEARCH.
                IF sy-subrc EQ 0.
                  ls_ocnum_new-ocnum = ls_ocnum-ocnum.
                  APPEND ls_ocnum_new TO lt_ocnum_new.
                  CLEAR ls_ocnum_new.

*-- calling Function module for Reversal
                  CALL FUNCTION 'ZABS_FM_STATUS_REVERSAL'
                    EXPORTING
                      lt_ocnum    = lt_ocnum_new
                    IMPORTING
                      et_messages = lt_messages.
*-- Fetching messages into final display
                  READ TABLE lt_messages INTO DATA(ls_messages) INDEX 1.
                  IF ls_messages-msgty = zcl_abs_abap_maintain=>c_msgty_success."'S'
                    APPEND ls_acitm_cnf TO lt_acitm_ctd.
                  ELSE.
                    ls_final-accom = ls_acitm_cnf-accom.
                    ls_final-posnr = ls_acitm_cnf-posnr.
                    ls_final-msgty = zcl_abs_abap_maintain=>c_icon_error."'@0A@'
                    MESSAGE ID ls_messages-msgid TYPE ls_messages-msgty
                                               NUMBER ls_messages-msgno
                                                 WITH ls_messages-msgv1 ls_messages-msgv2
                                                 INTO ls_final-msgtxt.
                    APPEND ls_final TO gt_final_msg.
                    CLEAR ls_final.
                  ENDIF.
                  CLEAR ls_messages.
                ENDIF.
                CLEAR ls_ocnum.
              ENDIF.
              CLEAR: ls_ocnum1,ls_messages.
              REFRESH: lt_messages, lt_ocnum_new.
            ENDLOOP.
          ENDIF.

        ENDIF."Confirmed

*-- Saving the item data
        IF lt_acitm_ctd IS NOT INITIAL.
          SORT lt_acitm_ctd BY accom posnr.
          LOOP AT gt_acdoc ASSIGNING <ls_acdoc>.

            LOOP AT <ls_acdoc>-x-acitm ASSIGNING FIELD-SYMBOL(<ls_xacitm>).

              READ TABLE lt_acitm_ctd INTO DATA(ls_acitm_ctd)
                                      WITH KEY accom = <ls_xacitm>-accom
                                               posnr = <ls_xacitm>-posnr
                                               BINARY SEARCH.
              IF sy-subrc EQ 0.
                <ls_xacitm>-updkz = zcl_abs_abap_maintain=>c_updkz_delete. "'D'
              ENDIF.

            ENDLOOP.
            CLEAR ls_acitm_ctd.

            LOOP AT <ls_acdoc>-y-acitm ASSIGNING FIELD-SYMBOL(<ls_yacitm>).
              READ TABLE lt_acitm_ctd INTO     ls_acitm_ctd
                                      WITH KEY accom = <ls_yacitm>-accom
                                               posnr = <ls_yacitm>-posnr
                                               BINARY SEARCH.
              IF sy-subrc EQ 0.
                <ls_yacitm>-updkz = zcl_abs_abap_maintain=>c_updkz_delete. "D
              ENDIF.
            ENDLOOP.

            <ls_acdoc>-updkz = zcl_abs_abap_maintain=>c_updkz_update."U

          ENDLOOP.

          CALL FUNCTION '/AGRI/FMAC_SAVE'
            EXPORTING
              iref_text   = ref_text
            CHANGING
              ct_acdoc    = gt_acdoc
              ct_messages = lt_messages
            EXCEPTIONS
              no_change   = 1
              OTHERS      = 2.
          LOOP AT lt_acitm_ctd INTO ls_acitm_ctd.
            READ TABLE lt_messages INTO ls_messages
                                  WITH KEY msgty = zcl_abs_abap_maintain=>c_msgty_error.
            IF sy-subrc EQ 0.
              ls_final-accom = ls_acitm_ctd-accom.
              ls_final-posnr = ls_acitm_ctd-posnr.
              ls_final-msgty = zcl_abs_abap_maintain=>c_icon_error.
              MESSAGE ID ls_messages-msgid TYPE ls_messages-msgty
                                         NUMBER ls_messages-msgno
                                           WITH ls_messages-msgv1 ls_messages-msgv2
                                           INTO ls_final-msgtxt.
              APPEND ls_final TO gt_final_msg.
              CLEAR ls_final.

            ELSE.
              ls_final-accom = ls_acitm_ctd-accom.
              ls_final-msgty = zcl_abs_abap_maintain=>c_icon_success.
              ls_final-msgtxt = TEXT-002.
              APPEND ls_final TO gt_final_msg.
              SORT gt_final_msg BY accom.
              DELETE ADJACENT DUPLICATES FROM gt_final_msg COMPARING accom.
              CLEAR ls_final.
            ENDIF.
          ENDLOOP.

        ENDIF.

      ELSE.
        MESSAGE TEXT-003 TYPE zcl_abs_abap_maintain=>c_msgty_info."I
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE TEXT-007 TYPE zcl_abs_abap_maintain=>c_msgty_error.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_data .

  DATA: alv_table TYPE REF TO   cl_salv_table,
        lv_msg    TYPE REF TO cx_salv_msg.

  TRY.
      CALL METHOD cl_salv_table=>factory(
        IMPORTING
          r_salv_table = alv_table
        CHANGING
          t_table      = gt_final_msg ).
    CATCH cx_salv_msg INTO lv_msg.
  ENDTRY.

  alv_table->display( ).

ENDFORM.
