************************************************************************
*  Confidential property of Citrosuco                                  *
*  All Rights Reserved                                                 *
************************************************************************
* Report Name       :  ZABS_REP_TOCNF_BGPROG_TOP                       *
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

TABLES : /agri/fmfpitm, zabs_tocnf_hdr.

TYPES : tty_tocnf_itm  TYPE STANDARD TABLE OF zabs_tocnf_itm,
        tty_tocnf_elog TYPE STANDARD TABLE OF zabs_tocnf_elog,
        BEGIN OF ty_joblog,
          jname  TYPE tbtcjob-jobname,       " Job Name
          jnumb  TYPE tbtcjob-jobcount,      " Job Number
          status TYPE tbtco-status,          " Job Status
          index  TYPE indx_srtfd,
        END OF ty_joblog,
        tt_joblog TYPE STANDARD TABLE OF ty_joblog.

*--Global table declarations
DATA : gt_tocnf_elog      TYPE TABLE OF zabs_tocnf_elog,
       gt_str_tocnfmlog   TYPE TABLE OF zabs_str_tocnf_mlog,
       gt_joblog          TYPE tt_joblog,
       gv_active_sessions TYPE i.

*&--------------------------------------------------------------------*
*&    SELECTION-SCREEN
*&--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: s_aufnr  FOR /agri/fmfpitm-aufnr,
                s_vornr  FOR /agri/fmfpitm-vornr,
                s_dtstmp FOR zabs_tocnf_hdr-dttimstamp,
*                s_insdt  FOR zabs_TOCNF_hdr-inspdate,
                s_musr   FOR zabs_tocnf_hdr-ernam,
                s_erdat  FOR zabs_tocnf_hdr-erdat,
                s_erzet  FOR zabs_tocnf_hdr-erzet.
PARAMETERS : p_tohdr TYPE zabs_tty_tocnf_hdr NO-DISPLAY,
             p_par   TYPE c AS CHECKBOX USER-COMMAND xy.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK bpp WITH FRAME TITLE TEXT-002.
PARAMETERS: p_psize  TYPE /irm/s_g_parallel_pro_ctrl-psize
                          DEFAULT '500' MODIF ID k1,
            p_maxsn  TYPE /irm/s_g_parallel_pro_ctrl-maxsn
                          DEFAULT '5' MODIF ID k2,
*            p_mwait  TYPE /irm/s_g_parallel_pro_ctrl-mwait
*                          DEFAULT '25' MODIF ID m2,
*            p_jobpfx TYPE /irm/s_g_parallel_pro_ctrl-jobpf
*                          DEFAULT sy-repid MODIF ID k3,
            p_jobpfx TYPE char14
                          DEFAULT 'ZABS_TOCNF' MODIF ID k3,
            p_rdate  TYPE sy-datum DEFAULT sy-datum MODIF ID k4,
            p_time   TYPE sy-uzeit DEFAULT sy-uzeit MODIF ID k5.
SELECTION-SCREEN END OF BLOCK bpp.

AT SELECTION-SCREEN OUTPUT.
*--Screen validation
  PERFORM screen_modify.
