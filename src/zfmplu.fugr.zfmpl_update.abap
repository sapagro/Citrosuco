FUNCTION ZFMPL_UPDATE.
*"----------------------------------------------------------------------
*"*"Interfple local:
*"  IMPORTING
*"     REFERENCE(I_SET_UPDATE_TASK) TYPE  RV56A-SELKZ
*"  TABLES
*"      T_XPLHDR TYPE  ZT_FMPLHDR
*"      T_YPLHDR TYPE  ZT_FMPLHDR
*"      T_XPLITM TYPE  ZT_FMPLITM
*"      T_YPLITM TYPE  ZT_FMPLITM
*"----------------------------------------------------------------------

  DATA:
    lt_ins_plhdr   TYPE zt_fmplhdr,
    lt_upd_plhdr   TYPE zt_fmplhdr,
    lt_del_plhdr   TYPE zt_fmplhdr,

    lt_ins_plitm   TYPE zt_fmplitm,
    lt_upd_plitm   TYPE zt_fmplitm,
    lt_del_plitm   TYPE zt_fmplitm.


  PERFORM update_entries_collect TABLES t_xplhdr
                                        t_yplhdr
                                        lt_ins_plhdr
                                        lt_upd_plhdr
                                        lt_del_plhdr.

  PERFORM update_entries_collect TABLES t_xplitm
                                        t_yplitm
                                        lt_ins_plitm
                                        lt_upd_plitm
                                        lt_del_plitm.
  IF i_set_update_task NE space.
    CALL FUNCTION 'ZFMPL_UPDATE_DB' IN UPDATE TASK
      TABLES
        t_ins_plhdr   = lt_ins_plhdr
        t_upd_plhdr   = lt_upd_plhdr
        t_del_plhdr   = lt_del_plhdr
        t_ins_plitm   = lt_ins_plitm
        t_upd_plitm   = lt_upd_plitm
        t_del_plitm   = lt_del_plitm.
  ELSE.

    CALL FUNCTION 'ZFMPL_UPDATE_DB'
      TABLES
        t_ins_plhdr   = lt_ins_plhdr
        t_upd_plhdr   = lt_upd_plhdr
        t_del_plhdr   = lt_del_plhdr
        t_ins_plitm   = lt_ins_plitm
        t_upd_plitm   = lt_upd_plitm
        t_del_plitm   = lt_del_plitm.

  ENDIF.

ENDFUNCTION.
