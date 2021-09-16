FUNCTION zfmac_update.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_SET_UPDATE_TASK) TYPE  RV56A-SELKZ
*"  TABLES
*"      T_XACHDR TYPE  ZT_FMACHDR
*"      T_YACHDR TYPE  ZT_FMACHDR
*"      T_XACITM TYPE  ZT_FMACITM
*"      T_YACITM TYPE  ZT_FMACITM
*"      T_XACVLC TYPE  ZT_FMACVLCL
*"      T_YACVLC TYPE  ZT_FMACVLCL
*"----------------------------------------------------------------------

  DATA:
    lt_ins_achdr TYPE zt_fmachdr,
    lt_upd_achdr TYPE zt_fmachdr,
    lt_del_achdr TYPE zt_fmachdr,

    lt_ins_acitm TYPE zt_fmacitm,
    lt_upd_acitm TYPE zt_fmacitm,
    lt_del_acitm TYPE zt_fmacitm,

    lt_ins_acvlc TYPE zt_fmacvlcl,
    lt_upd_acvlc TYPE zt_fmacvlcl,
    lt_del_acvlc TYPE zt_fmacvlcl.


  PERFORM update_entries_collect TABLES t_xachdr
                                        t_yachdr
                                        lt_ins_achdr
                                        lt_upd_achdr
                                        lt_del_achdr.

  PERFORM update_entries_collect TABLES t_xacitm
                                        t_yacitm
                                        lt_ins_acitm
                                        lt_upd_acitm
                                        lt_del_acitm.

  PERFORM update_entries_collect TABLES t_xacvlc
                                        t_yacvlc
                                        lt_ins_acvlc
                                        lt_upd_acvlc
                                        lt_del_acvlc.

  IF i_set_update_task NE space.
    CALL FUNCTION 'ZFMAC_UPDATE_DB' IN UPDATE TASK
      TABLES
        t_ins_achdr = lt_ins_achdr
        t_upd_achdr = lt_upd_achdr
        t_del_achdr = lt_del_achdr
        t_ins_acitm = lt_ins_acitm
        t_upd_acitm = lt_upd_acitm
        t_del_acitm = lt_del_acitm
        t_ins_acvlc = lt_ins_acvlc
        t_upd_acvlc = lt_upd_acvlc
        t_del_acvlc = lt_del_acvlc.
  ELSE.
    CALL FUNCTION 'ZFMAC_UPDATE_DB'
      TABLES
        t_ins_achdr = lt_ins_achdr
        t_upd_achdr = lt_upd_achdr
        t_del_achdr = lt_del_achdr
        t_ins_acitm = lt_ins_acitm
        t_upd_acitm = lt_upd_acitm
        t_del_acitm = lt_del_acitm
        t_ins_acvlc = lt_ins_acvlc
        t_upd_acvlc = lt_upd_acvlc
        t_del_acvlc = lt_del_acvlc.
  ENDIF.

ENDFUNCTION.
