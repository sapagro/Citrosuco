FUNCTION ZFMAC_UPDATE_DB.
*"----------------------------------------------------------------------
*"*"Módulo função atualização:
*"
*"*"Interface local:
*"  TABLES
*"      T_INS_ACHDR TYPE  ZT_FMACHDR
*"      T_UPD_ACHDR TYPE  ZT_FMACHDR
*"      T_DEL_ACHDR TYPE  ZT_FMACHDR
*"      T_INS_ACITM TYPE  ZT_FMACITM
*"      T_UPD_ACITM TYPE  ZT_FMACITM
*"      T_DEL_ACITM TYPE  ZT_FMACITM
*"      T_INS_ACVLC TYPE  ZT_FMACVLCL
*"      T_UPD_ACVLC TYPE  ZT_FMACVLCL
*"      T_DEL_ACVLC TYPE  ZT_FMACVLCL
*"----------------------------------------------------------------------

  PERFORM db_update TABLES t_ins_achdr
                           t_upd_achdr
                           t_del_achdr
                     USING c_tablename-achdr.

  PERFORM db_update TABLES t_ins_acitm
                           t_upd_acitm
                           t_del_acitm
                     USING c_tablename-acitm.

  PERFORM db_update TABLES t_ins_acvlc
                           t_upd_acvlc
                           t_del_acvlc
                     USING c_tablename-acvlc.

ENDFUNCTION.
