class ZCL_ABS_BADI_FMOC_ALL definition
  public
  final
  create public .

public section.

  interfaces /AGRI/IF_EX_BADI_FMOC_ALL .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ABS_BADI_FMOC_ALL IMPLEMENTATION.


METHOD /agri/if_ex_badi_fmoc_all~after_save.

  TYPES: BEGIN OF lty_aufnr,
           aufnr  TYPE aufnr,
           ocnum  TYPE /agri/fmocnum,
           contr  TYPE /agri/fmccontr,
           vornr  TYPE vornr,
           matnr  TYPE matnr,
           nrtank TYPE zabs_e_nrtank,
           reason	TYPE zabs_e_reason,
         END OF lty_aufnr.

  DATA: lt_aufnr     TYPE TABLE OF lty_aufnr,
        lt_taskcnrec TYPE TABLE OF zabs_taskcn_rec,
        lt_fpcom     TYPE /agri/t_fmfpcom,
        ls_taskcnrec TYPE zabs_taskcn_rec,
        ls_fmoccom   TYPE /agri/s_fmoccom,
*--BOC-T_T.KONNO-09.08.21
*        lv_matnr     TYPE c LENGTH 12,
        lv_matnr     TYPE c LENGTH 18,
*--EOC-T_T.KONNO-09.08.21
        lt_occom     TYPE /agri/t_fmoccom.

  FIELD-SYMBOLS: <fs_aufnr> TYPE lty_aufnr.

  CONSTANTS: lc_memreason(8) TYPE c VALUE 'ZZREASON'.

  lt_occom = it_fmoccom.

  IMPORT: lt_fpcom TO lt_fpcom FROM DATABASE indx(id) ID lc_memreason.

  DELETE FROM DATABASE: indx(id) ID lc_memreason.

  SORT lt_occom BY ocnum contr matnr.

  DATA(lt_fpcom_aux) = lt_fpcom[].
  LOOP AT lt_fpcom_aux ASSIGNING FIELD-SYMBOL(<ls_fpcom_aux>).
    <ls_fpcom_aux>-matnr = |{ <ls_fpcom_aux>-matnr ALPHA = OUT }|.
  ENDLOOP.
  SORT lt_fpcom_aux BY matnr.

  LOOP AT it_fmocopr ASSIGNING FIELD-SYMBOL(<fs_fmocopr>).
    READ TABLE lt_occom TRANSPORTING NO FIELDS
      WITH KEY ocnum = <fs_fmocopr>-ocnum BINARY SEARCH.
    IF sy-subrc = 0.
      LOOP AT lt_occom ASSIGNING FIELD-SYMBOL(<fs_data>).
        IF <fs_data>-ocnum <> <fs_fmocopr>-ocnum.
          EXIT.
        ENDIF.

        IF <fs_fmocopr>-zznrtank IS NOT INITIAL.
          APPEND INITIAL LINE TO lt_aufnr ASSIGNING <fs_aufnr>.
          IF sy-subrc EQ 0.
            <fs_aufnr>-aufnr  = <fs_fmocopr>-aufnr.
            <fs_aufnr>-matnr  = <fs_data>-matnr.
            <fs_aufnr>-vornr  = <fs_fmocopr>-vornr.
            <fs_aufnr>-ocnum  = <fs_fmocopr>-ocnum.
*--BOC-T_T.KONNO-09.08.21
*          <fs_aufnr>-contr  = <fs_data>-contr.
            DATA(lv_matnr_aux) = |{ <fs_data>-matnr ALPHA = OUT }|.
            READ TABLE lt_fpcom_aux INTO DATA(ls_fpcom_aux)
              WITH KEY matnr = lv_matnr_aux BINARY SEARCH.
            IF sy-subrc EQ 0.
              <fs_aufnr>-contr = ls_fpcom_aux-contr.
            ENDIF.
*--EOC-T_T.KONNO-09.08.21
            <fs_aufnr>-nrtank = <fs_fmocopr>-zznrtank.
          ENDIF.
        ENDIF.
      ENDLOOP.
      LOOP AT lt_fpcom ASSIGNING FIELD-SYMBOL(<fs_datat>).
*--BOC-T_T.KONNO-09.08.21
*        CHECK <fs_datat>-zzreason IS NOT INITIAL.
*        READ TABLE lt_occom TRANSPORTING NO FIELDS
*          WITH KEY matnr = <fs_datat>-aufnr
*                   rspos = <fs_datat>-rspos.
*        IF sy-subrc EQ 0.
*          APPEND INITIAL LINE TO lt_aufnr ASSIGNING <fs_aufnr>.
*          IF sy-subrc EQ 0.
*            <fs_aufnr>-aufnr  = <fs_fmocopr>-aufnr.
*            <fs_aufnr>-matnr  = <fs_datat>-matnr.
*            <fs_aufnr>-vornr  = <fs_fmocopr>-vornr.
*            <fs_aufnr>-ocnum  = <fs_fmocopr>-ocnum.
*            <fs_aufnr>-contr  = <fs_datat>-contr.
*            <fs_aufnr>-nrtank = <fs_fmocopr>-zznrtank.
*            <fs_aufnr>-reason = <fs_datat>-zzreason.
*          ENDIF.
*        ENDIF.
        IF <fs_datat>-zzreason IS NOT INITIAL.
          APPEND INITIAL LINE TO lt_aufnr ASSIGNING <fs_aufnr>.
          IF sy-subrc EQ 0.
            <fs_aufnr>-aufnr  = <fs_fmocopr>-aufnr.
            <fs_aufnr>-matnr  = <fs_datat>-matnr.
            <fs_aufnr>-vornr  = <fs_fmocopr>-vornr.
            <fs_aufnr>-ocnum  = <fs_fmocopr>-ocnum.
            <fs_aufnr>-contr  = <fs_datat>-contr.
            <fs_aufnr>-nrtank = <fs_fmocopr>-zznrtank.
            <fs_aufnr>-reason = <fs_datat>-zzreason.
          ENDIF.
        ENDIF.
*--EOC-T_T.KONNO-09.08.21
      ENDLOOP.
    ELSE.
      IF <fs_fmocopr>-zznrtank IS NOT INITIAL
        AND lt_fpcom IS INITIAL.
        APPEND INITIAL LINE TO lt_aufnr ASSIGNING <fs_aufnr>.
        <fs_aufnr>-aufnr  = <fs_fmocopr>-aufnr.
        <fs_aufnr>-vornr  = <fs_fmocopr>-vornr.
        <fs_aufnr>-ocnum  = <fs_fmocopr>-ocnum.
        <fs_aufnr>-nrtank = <fs_fmocopr>-zznrtank.
        <fs_aufnr>-reason = <fs_fmocopr>-zzreason.
      ENDIF.

      LOOP AT lt_fpcom ASSIGNING FIELD-SYMBOL(<ls_datat>).
*--BOC-T_T.KONNO-09.08.21
*        CHECK <ls_datat>-zzreason IS NOT INITIAL.
*        READ TABLE lt_occom TRANSPORTING NO FIELDS
*                            WITH KEY matnr = <ls_datat>-aufnr
*                                     rspos = <ls_datat>-rspos.
*        CHECK sy-subrc <> 0.
*        APPEND INITIAL LINE TO lt_aufnr ASSIGNING <fs_aufnr>.
*        <fs_aufnr>-aufnr  = <fs_fmocopr>-aufnr.
*        <fs_aufnr>-matnr  = <ls_datat>-matnr.
*        <fs_aufnr>-vornr  = <fs_fmocopr>-vornr.
*        <fs_aufnr>-ocnum  = <fs_fmocopr>-ocnum.
*        <fs_aufnr>-contr  = <ls_datat>-contr.
*        <fs_aufnr>-nrtank = <fs_fmocopr>-zznrtank.
*        <fs_aufnr>-reason = <ls_datat>-zzreason.
        IF <ls_datat>-zzreason IS NOT INITIAL.
          APPEND INITIAL LINE TO lt_aufnr ASSIGNING <fs_aufnr>.
          IF sy-subrc EQ 0.
            <fs_aufnr>-aufnr  = <fs_fmocopr>-aufnr.
            <fs_aufnr>-matnr  = <ls_datat>-matnr.
            <fs_aufnr>-vornr  = <fs_fmocopr>-vornr.
            <fs_aufnr>-ocnum  = <fs_fmocopr>-ocnum.
            <fs_aufnr>-contr  = <ls_datat>-contr.
            <fs_aufnr>-nrtank = <fs_fmocopr>-zznrtank.
            <fs_aufnr>-reason = <ls_datat>-zzreason.
          ENDIF.
        ENDIF.
*--EOC-T_T.KONNO-09.08.21
      ENDLOOP.
    ENDIF.
    CLEAR ls_fmoccom.
  ENDLOOP.

  CHECK lt_aufnr IS NOT INITIAL.

  SORT lt_aufnr BY aufnr vornr ocnum.
  DELETE lt_aufnr WHERE nrtank IS INITIAL
                    AND reason IS INITIAL.

  lt_occom = it_fmoccom.

  SORT lt_occom BY ocnum matnr.

  LOOP AT lt_aufnr ASSIGNING <fs_aufnr>.
    ls_taskcnrec-nrtank = <fs_aufnr>-nrtank.
    ls_taskcnrec-aufnr  = <fs_aufnr>-aufnr.
    ls_taskcnrec-ocnum  = <fs_aufnr>-ocnum.
    ls_taskcnrec-vornr  = <fs_aufnr>-vornr.
    ls_taskcnrec-ernam  = sy-uname.
    ls_taskcnrec-erdat  = sy-datum.
    ls_taskcnrec-erzet  = sy-uzeit.

    IF <fs_aufnr>-reason IS INITIAL.
      READ TABLE lt_occom TRANSPORTING NO FIELDS
                          WITH KEY ocnum = <fs_aufnr>-ocnum
                          BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT lt_occom ASSIGNING FIELD-SYMBOL(<fs_occom>) FROM sy-tabix.
          IF <fs_occom>-ocnum <> <fs_aufnr>-ocnum.
            EXIT.
          ENDIF.
          CHECK ls_taskcnrec-nrtank IS NOT INITIAL.
*--BOC-T_T.KONNO-09.08.21
*          READ TABLE lt_fpcom ASSIGNING FIELD-SYMBOL(<ls_fpcom>)
*                              WITH KEY aufnr = ls_taskcnrec-aufnr
*                                       vornr = ls_taskcnrec-vornr
*                                       matnr = |{ <fs_occom>-matnr ALPHA = OUT }|. "<fs_occom>-matnr.
*
*          IF sy-subrc = 0.
*            ls_taskcnrec-contr  = <ls_fpcom>-contr.
*          ELSE.
*            ls_taskcnrec-contr  = <fs_occom>-contr.
*          ENDIF.
*          IF ls_taskcnrec-contr IS INITIAL.
*            ls_taskcnrec-contr = <fs_aufnr>-contr.
*          ENDIF.
          READ TABLE lt_fpcom_aux ASSIGNING FIELD-SYMBOL(<ls_fpcom>)
            WITH KEY aufnr = ls_taskcnrec-aufnr
                     vornr = ls_taskcnrec-vornr
                     matnr = |{ <fs_occom>-matnr ALPHA = OUT }|.
          IF sy-subrc EQ 0.
            ls_taskcnrec-contr = <ls_fpcom>-contr.
          ENDIF.
*--EOC-T_T.KONNO-09.08.21

          ls_taskcnrec-matnr  = <fs_occom>-matnr.
*         ls_taskcnrec-reason = <fs_occom>-zzreason.
          APPEND ls_taskcnrec TO lt_taskcnrec.
        ENDLOOP.
      ELSE.
        APPEND ls_taskcnrec TO lt_taskcnrec.
      ENDIF.
      CLEAR ls_taskcnrec.
    ELSE.

*--BOC-T_T.KONNO-09.08.21
*      READ TABLE lt_fpcom ASSIGNING FIELD-SYMBOL(<ls_fpcon>)
*                              WITH KEY aufnr = ls_taskcnrec-aufnr
*                                       vornr = ls_taskcnrec-vornr
*                                       matnr = |{ <fs_aufnr>-matnr ALPHA = OUT }|.
*      IF sy-subrc = 0.
*        ls_taskcnrec-contr  = <ls_fpcon>-contr.
*      ELSE.
*        ls_taskcnrec-contr  =  <fs_aufnr>-contr.
*      ENDIF.
      READ TABLE lt_fpcom_aux ASSIGNING FIELD-SYMBOL(<ls_fpcon>)
        WITH KEY aufnr = ls_taskcnrec-aufnr
                 vornr = ls_taskcnrec-vornr
                 matnr = |{ <fs_aufnr>-matnr ALPHA = OUT }| BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_taskcnrec-contr  = <ls_fpcon>-contr.
      ENDIF.
*--EOC-T_T.KONNO-09.08.21

      lv_matnr = |{ <fs_aufnr>-matnr ALPHA = IN }|.
      ls_taskcnrec-matnr  = lv_matnr.
      ls_taskcnrec-reason = <fs_aufnr>-reason.
      APPEND ls_taskcnrec TO lt_taskcnrec.
      CLEAR ls_taskcnrec.
    ENDIF.
  ENDLOOP.

  IF lt_taskcnrec IS NOT INITIAL.
    MODIFY zabs_taskcn_rec FROM TABLE lt_taskcnrec.
  ENDIF.

ENDMETHOD.
ENDCLASS.
