FUNCTION zfmrc_update.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_SET_UPDATE_TASK) TYPE  RV56A-SELKZ
*"  TABLES
*"      T_XRCHDR TYPE  ZT_FMRCHDR
*"      T_YRCHDR TYPE  ZT_FMRCHDR
*"      T_XRCLST TYPE  ZT_FMRCLST
*"      T_YRCLST TYPE  ZT_FMRCLST
*"      T_XRCBOM TYPE  ZT_FMRCBOM
*"      T_YRCBOM TYPE  ZT_FMRCBOM
*"      T_XRCVRS TYPE  ZT_FMRCVRS
*"      T_YRCVRS TYPE  ZT_FMRCVRS
*"----------------------------------------------------------------------

  DATA:
    lt_ins_rchdr TYPE zt_fmrchdr,
    lt_upd_rchdr TYPE zt_fmrchdr,
    lt_del_rchdr TYPE zt_fmrchdr,

    lt_ins_rclst TYPE zt_fmrclst,
    lt_upd_rclst TYPE zt_fmrclst,
    lt_del_rclst TYPE zt_fmrclst,

    lt_ins_rcbom TYPE zt_fmrcbom,
    lt_upd_rcbom TYPE zt_fmrcbom,
    lt_del_rcbom TYPE zt_fmrcbom,

    lt_ins_rcvrs TYPE zt_fmrcvrs,
    lt_upd_rcvrs TYPE zt_fmrcvrs,
    lt_del_rcvrs TYPE zt_fmrcvrs.


  PERFORM update_entries_collect TABLES t_xrchdr
                                        t_yrchdr
                                        lt_ins_rchdr
                                        lt_upd_rchdr
                                        lt_del_rchdr.

  PERFORM update_entries_collect TABLES t_xrclst
                                        t_yrclst
                                        lt_ins_rclst
                                        lt_upd_rclst
                                        lt_del_rclst.

  PERFORM update_entries_collect TABLES t_xrcbom
                                        t_yrcbom
                                        lt_ins_rcbom
                                        lt_upd_rcbom
                                        lt_del_rcbom.

  PERFORM update_entries_collect TABLES t_xrcvrs
                                        t_yrcvrs
                                        lt_ins_rcvrs
                                        lt_upd_rcvrs
                                        lt_del_rcvrs.
  IF i_set_update_task NE space.
    CALL FUNCTION 'ZFMRC_UPDATE_DB' IN UPDATE TASK
      TABLES
        t_ins_rchdr = lt_ins_rchdr
        t_upd_rchdr = lt_upd_rchdr
        t_del_rchdr = lt_del_rchdr
        t_ins_rclst = lt_ins_rclst
        t_upd_rclst = lt_upd_rclst
        t_del_rclst = lt_del_rclst
        t_ins_rcbom = lt_ins_rcbom
        t_upd_rcbom = lt_upd_rcbom
        t_del_rcbom = lt_del_rcbom
        t_ins_rcvrs = lt_ins_rcvrs
        t_upd_rcvrs = lt_upd_rcvrs
        t_del_rcvrs = lt_del_rcvrs.
  ELSE.

    CALL FUNCTION 'ZFMRC_UPDATE_DB'
      TABLES
        t_ins_rchdr = lt_ins_rchdr
        t_upd_rchdr = lt_upd_rchdr
        t_del_rchdr = lt_del_rchdr
        t_ins_rclst = lt_ins_rclst
        t_upd_rclst = lt_upd_rclst
        t_del_rclst = lt_del_rclst
        t_ins_rcbom = lt_ins_rcbom
        t_upd_rcbom = lt_upd_rcbom
        t_del_rcbom = lt_del_rcbom
        t_ins_rcvrs = lt_ins_rcvrs
        t_upd_rcvrs = lt_upd_rcvrs
        t_del_rcvrs = lt_del_rcvrs.
  ENDIF.

ENDFUNCTION.
