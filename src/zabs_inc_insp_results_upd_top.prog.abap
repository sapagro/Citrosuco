*&---------------------------------------------------------------------*
*& Include          ZABS_REP_INSPREC_UPDATE_TOP
*&---------------------------------------------------------------------*

TABLES : qals, qapo, zabs_qchar_hdr.

TYPES : tty_qchar_itm  TYPE STANDARD TABLE OF zabs_qchar_itm,
        tty_qchar_elog TYPE STANDARD TABLE OF zabs_qchar_erlog,
        BEGIN OF ty_joblog,
          jname  TYPE tbtcjob-jobname,       " Job Name
          jnumb  TYPE tbtcjob-jobcount,      " Job Number
          status TYPE tbtco-status,          " Job Status
          index  TYPE indx_srtfd,
        END OF ty_joblog,
        tt_joblog TYPE STANDARD TABLE OF ty_joblog.

*--Global table declarations
DATA : gt_qchar_elog      TYPE TABLE OF zabs_qchar_erlog,
       gt_qchar_hdr       TYPE STANDARD TABLE OF zabs_qchar_hdr INITIAL SIZE 0,
       gt_str_qcharmlog   TYPE TABLE OF zabs_str_qcharmlog,
       gt_joblog          TYPE tt_joblog,
       gv_active_sessions TYPE i.

*&--------------------------------------------------------------------*
*&    SELECTION-SCREEN
*&--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: s_inslot FOR qals-prueflos,
                s_insopr FOR qapo-vornr,
                s_dtstmp FOR zabs_qchar_hdr-dttimstamp,
                s_insdt  FOR zabs_qchar_hdr-inspdate,
                s_musr   FOR zabs_qchar_hdr-ernam,
                s_erdat  FOR zabs_qchar_hdr-erdat,
                s_erzet  FOR zabs_qchar_hdr-erzet.
PARAMETERS : p_qchdr TYPE zabs_tty_qchar_hdr NO-DISPLAY,
             p_qapp  TYPE qapptab NO-DISPLAY,
             p_par   TYPE c AS CHECKBOX USER-COMMAND xy,
             p_final TYPE abap_bool NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK bpp WITH FRAME TITLE TEXT-003.
PARAMETERS: p_psize  TYPE /irm/s_g_parallel_pro_ctrl-psize
                          DEFAULT '500' MODIF ID k1,
            p_maxsn  TYPE /irm/s_g_parallel_pro_ctrl-maxsn
                          DEFAULT '5' MODIF ID k2,
            p_jobpfx TYPE char14
                          DEFAULT 'ZABS_INSRECUPD' MODIF ID k3,
            p_rdate  TYPE sy-datum DEFAULT sy-datum MODIF ID k4,
            p_time   TYPE sy-uzeit DEFAULT sy-uzeit MODIF ID k5.
SELECTION-SCREEN END OF BLOCK bpp.

AT SELECTION-SCREEN OUTPUT.
*--Screen validation
  PERFORM screen_modify.
