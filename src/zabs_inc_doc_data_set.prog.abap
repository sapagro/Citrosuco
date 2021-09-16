************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Enhancement Name  :  ZABS_IENH_NURSERY_PROCESS                       *
* Form Name         :  document_data_set                               *
* Include Name      :  ZABS_INC_DOC_DATA_SET                           *
* Created By        :  Chandrakanth Karanam                            *
* Requested by      :  Mario Alfredo                                   *
* Created on        :  06.18.2019                                      *
* TR                :  C4DK901784                                      *
* Version           :  001                                             *
* Description       :  Populate the item quantity in process order     *
*----------------------------------------------------------------------*
*  Modification Log:                                                   *
*----------------------------------------------------------------------*
* MOD#|Date(MM.DD.YYYY)|Changed By| TR#       |Description             *
*                                                                      *
*&--------------------------------------------------------------------&*

*{   REPLACE        S4HK902502                                        1
*\  DATA: ls_marc  TYPE marc,
*** S4/HANA:
  DATA: ls_marc      TYPE v_marc_md,
***
*}   REPLACE
        ls_fphdr     TYPE /agri/s_fmfphdr,
        ls_rcr01     TYPE rcr01,
        ls_fmfpcnf   TYPE /agri/s_fmfp_cnf,
        lt_fphdr_new TYPE /agri/t_fmfphdr,
        lt_fpcom     TYPE /agri/t_fmfpcom.     "LOC
  FIELD-SYMBOLS: <ls_fpitm> TYPE /agri/s_fmfpitm,
                 <ls_fpcom> TYPE /agri/s_fmfpcom,
                 <ls_fpbch> TYPE /agri/s_fmfpbch.

  IF lwa_fpdoc_infocus-x-fphdr-ordlv EQ c_order_level-process_task AND
     lwa_fpdoc_infocus-x-fpitm IS NOT INITIAL.
    SELECT * FROM /agri/fmfphdr               "#EC CI_ALL_FIELDS_NEEDED
             INTO CORRESPONDING FIELDS OF TABLE lt_fphdr_new
              FOR ALL ENTRIES IN lwa_fpdoc_infocus-x-fpitm
            WHERE aufnr EQ lwa_fpdoc_infocus-x-fpitm-aufnr_to.
    IF sy-subrc EQ 0.
      SORT lt_fphdr_new BY aufnr.
    ENDIF.
  ENDIF.

  REFRESH: gt_fmfpcnf.
  LOOP AT lwa_fpdoc_infocus-x-fpitm ASSIGNING <ls_fpitm>.

*    IF lwa_fpdoc_infocus-x-fphdr-ordlv EQ c_order_level-process.  "LOC

    <ls_fpitm>-actdt = sy-datum.
    CLEAR: ls_fmfpcnf.
    MOVE-CORRESPONDING <ls_fpitm> TO ls_fmfpcnf.
    ls_fmfpcnf-budat = sy-datum.
    ls_fmfpcnf-arbpl_ext = ls_fmfpcnf-arbpl.
    ls_fmfpcnf-gicre = c_true.
    IF <ls_fpitm>-gamng GT <ls_fpitm>-gwemg.
      ls_fmfpcnf-lmnga = <ls_fpitm>-gamng - <ls_fpitm>-gwemg.
    ENDIF.

    PERFORM propose_item_data USING    c_true
                              CHANGING ls_fmfpcnf.

    PERFORM item_activities_prepare
                              USING lwa_fpdoc_infocus-x-fphdr-iwerk
                                    ls_fmfpcnf-arbpl
                           CHANGING ls_rcr01.
    ls_fmfpcnf-arbid = ls_rcr01-arbid.
    APPEND ls_fmfpcnf TO gt_fmfpcnf.
*    ELSEIF lwa_fpdoc_infocus-x-fphdr-ordlv EQ c_order_level-process_task. "LOC
    IF <ls_fpitm>-schdt IS NOT INITIAL.
      <ls_fpitm>-schdl = c_true.
    ENDIF.
    IF <ls_fpitm>-aufnr_to IS NOT INITIAL.
      CLEAR: ls_fphdr.
      READ TABLE lt_fphdr_new INTO ls_fphdr   "#EC CI_ALL_FIELDS_NEEDED
                          WITH KEY aufnr = <ls_fpitm>-aufnr_to
                        BINARY SEARCH.
      IF sy-subrc EQ 0.
        <ls_fpitm>-cstat = ls_fphdr-cstat.    "#EC CI_ALL_FIELDS_NEEDED
        <ls_fpitm>-gwemg = ls_fphdr-gwemg.    "#EC CI_ALL_FIELDS_NEEDED
        <ls_fpitm>-verid = ls_fphdr-verid.    "#EC CI_ALL_FIELDS_NEEDED
        <ls_fpitm>-equnr = ls_fphdr-equnr.    "#EC CI_ALL_FIELDS_NEEDED
      ENDIF.
    ELSE.
      <ls_fpitm>-tomng = <ls_fpitm>-gamng.
    ENDIF.

*    ENDIF.                                                                 "LOC

    LOOP AT lwa_fpdoc_infocus-x-fpcom ASSIGNING <ls_fpcom>
                                          WHERE aufnr EQ <ls_fpitm>-aufnr
                                            AND posnr EQ <ls_fpitm>-posnr.
*--SOC
*      IF lwa_fpdoc_infocus-x-fphdr-ordlv EQ c_order_level-process_task.
      IF <ls_fpcom>-zztask EQ abap_true.
*--EOC
        <ls_fpitm>-matnr = <ls_fpcom>-matnr.
      ELSEIF <ls_fpcom>-matnr IS NOT INITIAL AND
             <ls_fpcom>-xwaok IS NOT INITIAL.
        <ls_fpcom>-lmnga = ls_fmfpcnf-lmnga * <ls_fpcom>-esmng.
        <ls_fpcom>-vornr = <ls_fpitm>-vornr.
        CLEAR ls_marc.
*{   REPLACE        S4HK902502                                        2
*\        SELECT SINGLE * FROM marc INTO ls_marc
*** S4/HANA:
        SELECT SINGLE * FROM v_marc_md INTO ls_marc "#EC CI_ALL_FIELDS_NEEDED  "#EC CI_SEL_NESTED
***
*}   REPLACE
                       WHERE matnr EQ <ls_fpcom>-matnr
                         AND werks EQ <ls_fpcom>-werks.
*        CALL FUNCTION 'MARC_SINGLE_READ'
*          EXPORTING
*            matnr             = <ls_fpcom>-matnr
*            werks             = <ls_fpcom>-werks
*          IMPORTING
*            wmarc             = ls_marc
*          EXCEPTIONS
*            lock_on_marc      = 1
*            lock_system_error = 2
*            wrong_call        = 3
*            not_found         = 4
*            OTHERS            = 5.
*        IF sy-subrc <> 0.
** Implement suitable error handling here
*        ENDIF.
        IF ls_marc-xchar EQ c_true.           "#EC CI_ALL_FIELDS_NEEDED
          <ls_fpcom>-flgch = c_true.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDLOOP.

  LOOP AT lwa_fpdoc_infocus-x-fpbch ASSIGNING <ls_fpbch>
    WHERE gwemg IS INITIAL.
    <ls_fpbch>-lmnga = <ls_fpbch>-erfmg.
*    <ls_fpbch>-budat = sy-datum.
    IF <ls_fpbch>-zzbudat IS NOT INITIAL.
      <ls_fpbch>-budat = <ls_fpbch>-zzbudat.
    ELSE.
      <ls_fpbch>-budat = sy-datum.
    ENDIF.
  ENDLOOP.

  RETURN.
