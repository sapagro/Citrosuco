FUNCTION zfmrc_update_db.
*"----------------------------------------------------------------------
*"*"Módulo função atualização:
*"
*"*"Interface local:
*"  TABLES
*"      T_INS_RCHDR TYPE  ZT_FMRCHDR
*"      T_UPD_RCHDR TYPE  ZT_FMRCHDR
*"      T_DEL_RCHDR TYPE  ZT_FMRCHDR
*"      T_INS_RCLST TYPE  ZT_FMRCLST
*"      T_UPD_RCLST TYPE  ZT_FMRCLST
*"      T_DEL_RCLST TYPE  ZT_FMRCLST
*"      T_INS_RCBOM TYPE  ZT_FMRCBOM
*"      T_UPD_RCBOM TYPE  ZT_FMRCBOM
*"      T_DEL_RCBOM TYPE  ZT_FMRCBOM
*"      T_INS_RCVRS TYPE  ZT_FMRCVRS
*"      T_UPD_RCVRS TYPE  ZT_FMRCVRS
*"      T_DEL_RCVRS TYPE  ZT_FMRCVRS
*"----------------------------------------------------------------------

  PERFORM db_update TABLES t_ins_rchdr
                           t_upd_rchdr
                           t_del_rchdr
                     USING c_tablename-rchdr.

  PERFORM db_update TABLES t_ins_rclst
                           t_upd_rclst
                           t_del_rclst
                     USING c_tablename-rclst.

  PERFORM db_update TABLES t_ins_rcbom
                           t_upd_rcbom
                           t_del_rcbom
                     USING c_tablename-rcbom.

  PERFORM db_update TABLES t_ins_rcvrs
                           t_upd_rcvrs
                           t_del_rcvrs
                     USING c_tablename-rcvrs.

ENDFUNCTION.
