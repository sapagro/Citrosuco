*&---------------------------------------------------------------------*
*& Include          LZFG_ACMSEL
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF SCREEN 0010.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-006.

*select-options: so_tplnr for /agri/s_glflot-strno.
PARAMETERS:
  p_actyp  TYPE /agri/s_fmachdr-actyp AS LISTBOX VISIBLE LENGTH 30 USER-COMMAND entr,
  p_accom  TYPE /agri/s_fmachdr-accom MODIF ID acc,
  p_bukrs  TYPE /agri/s_fmachdr-bukrs MODIF ID cpy,
  p_werks  TYPE /agri/s_fmachdr-werks,
  p_strdat TYPE /agri/s_fmachdr-strtdat,
  p_findat TYPE /agri/s_fmachdr-findat,
  p_wonum  TYPE /agri/s_fmachdr-wonum MODIF ID won,
  p_mat    TYPE /agri/s_fmfphdr-matnr MODIF ID auf.
SELECT-OPTIONS:
                so_tplnr FOR /agri/s_fmfphdr-tplnr_fl MODIF ID auf,
                so_aufnr  FOR aufk-aufnr NO INTERVALS MODIF ID auf.
PARAMETERS:
                p_descr   TYPE /agri/s_fmachdr-descr,
                p_turma   TYPE /agri/s_fmachdr-zzturma MATCHCODE OBJECT zabs_sh_turma.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN END OF SCREEN 0010.

INITIALIZATION.

AT SELECTION-SCREEN.

  ok_code = sy-ucomm.
  PERFORM desc_data_fill_1.
  PERFORM header_data_check_1.

AT SELECTION-SCREEN ON EXIT-COMMAND.

  ok_code = sy-ucomm.

AT SELECTION-SCREEN OUTPUT.

  SET TITLEBAR 'T01'.

  PERFORM header_data_update_1.

  PERFORM screen_modify.

  IF gs_tfmactyp-acapp EQ c_accom_appli-aufnr.
    CLEAR: p_accom, p_wonum.
  ELSEIF gs_tfmactyp-acapp EQ c_accom_appli-wonum.
    CLEAR: so_aufnr, so_aufnr[],p_accom.
  ELSEIF gs_tfmactyp-acapp EQ c_accom_appli-prnum.
    CLEAR: so_aufnr, so_aufnr[], p_wonum.
  ENDIF.
