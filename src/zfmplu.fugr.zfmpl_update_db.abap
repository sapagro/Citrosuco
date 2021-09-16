FUNCTION ZFMPL_UPDATE_DB.
*"----------------------------------------------------------------------
*"*"Módulo função atualização:
*"
*"*"Interface local:
*"  TABLES
*"      T_INS_PLHDR TYPE  ZT_FMPLHDR
*"      T_UPD_PLHDR TYPE  ZT_FMPLHDR
*"      T_DEL_PLHDR TYPE  ZT_FMPLHDR
*"      T_INS_PLITM TYPE  ZT_FMPLITM
*"      T_UPD_PLITM TYPE  ZT_FMPLITM
*"      T_DEL_PLITM TYPE  ZT_FMPLITM
*"----------------------------------------------------------------------

  PERFORM db_update TABLES t_ins_plhdr
                           t_upd_plhdr
                           t_del_plhdr
                     USING c_tablename-plhdr.

  PERFORM db_update TABLES t_ins_plitm
                           t_upd_plitm
                           t_del_plitm
                     USING c_tablename-plitm.

ENDFUNCTION.
