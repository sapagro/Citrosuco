*&---------------------------------------------------------------------*
*& Include ZABS_GEN_MACROS
*&---------------------------------------------------------------------*

*-- Global Constants
CONSTANTS:
*-- Conditions
  c_true  TYPE flag VALUE abap_true,
  c_false TYPE flag VALUE space,
*-- Message Types
  c_err   TYPE symsgty VALUE 'E',
  c_war   TYPE symsgty VALUE 'W',
  c_suc   TYPE symsgty VALUE 'S',
  c_inf   TYPE symsgty VALUE 'I',
*-- Traffic Lights
  c_redl  TYPE pmlights VALUE '@0A@',
  c_yell  TYPE pmlights VALUE '@09@',
  c_grel  TYPE pmlights VALUE '@08@',
  c_gryl  TYPE pmlights VALUE '@EB@'.

*-- Types
TYPES: BEGIN OF lty_altlog.
         INCLUDE TYPE bal_s_msg.
         TYPES: msgv_alt1 TYPE char30,
         msgv_alt2 TYPE char30,
         msgv_alt3 TYPE char30,
         msgv_alt4 TYPE char30,
       END OF lty_altlog.
*-- Global Data
DATA:
  lt_msg     TYPE bal_t_msg,
  lwa_msg    TYPE bal_s_msg,
  lwa_altmsg TYPE lty_altlog.

DEFINE log_altcreate.

  TRY.
  CREATE DATA &1 TYPE STANDARD TABLE OF lty_altlog.
  CATCH cx_root.
  ENDTRY.

END-OF-DEFINITION.

DEFINE log_altadd.

  TRY.
    MOVE-CORRESPONDING lwa_msg TO lwa_altmsg.
    lwa_altmsg-msgv_alt1 = &2.
    lwa_altmsg-msgv_alt2 = &3.
    lwa_altmsg-msgv_alt3 = &4.
    lwa_altmsg-msgv_alt4 = &5.
    APPEND lwa_altmsg TO &1.
    CATCH cx_root.
  ENDTRY.

END-OF-DEFINITION.

DEFINE log_add.

  TRY.
    lwa_msg-msgid = &1.
    lwa_msg-msgty = &2.
    lwa_msg-msgno = &3.
    lwa_msg-msgv1 = &4.
    lwa_msg-msgv2 = &5.
    lwa_msg-msgv3 = &6.
    lwa_msg-msgv4 = &7.
    lwa_msg-probclass = &8.

    CONDENSE: lwa_msg-msgv1,
    lwa_msg-msgv2,
    lwa_msg-msgv3,
    lwa_msg-msgv4.
*  -- Agregar mensaje al log
    zabs_cl_logup_utilities=>add_msg(
    EXPORTING im_wa_msg = lwa_msg
    im_v_log_handle = gv_log_handle ).
   CATCH cx_root.
 ENDTRY.

END-OF-DEFINITION.
***********************************************************************************
*log_add gc_messid c_err '023'
*<ls_header>-id_hechos <ls_header>-line space space space.
*log_altadd gt_msg <ls_header>-num_identificacion <ls_header>-id_hechos space space.

*I_V_PFILE  TYPE STRING OPTIONAL
*I_V_ETYPE  TYPE CHAR1  DEFAULT 'R'
*I_V_SRC  TYPE CHAR1 OPTIONAL
*I_V_SRCD  TYPE CHAR200 OPTIONAL

*DATA:
*lv_extnumber TYPE balnrext.
*
**-- Se asignan valores informativos.
*gv_pfile = i_v_pfile.
*gv_etype = i_v_etype.
*gv_src = i_v_src.
*
**-- Crear log
*zcl_log_utilities=>create_log(
*EXPORTING im_v_extnumber = lv_extnumber
*im_v_object = gc_object
*im_v_subobject = gc_subobject
*IMPORTING ex_v_log_handle = gv_log_handle ).
*
** Añadimos detalles al log
*zcl_log_utilities=>add_initial_msg( EXPORTING im_v_path = gv_pfile
*im_v_tipo = gv_etype
*im_v_log_handle = gv_log_handle
*im_v_msj_opc_1 = i_v_srcd ).

**-- Muestra el log solo si es una ejecución local.
*APPEND gv_log_handle TO lti_handle.
*IF gv_src = 'L'.
*zcl_log_utilities=>display_log(
*EXPORTING im_ti_log_handle = lti_handle ).
*ENDIF.
*
**-- Guarda log para visualizar por la SLG1
*IF gv_etype = gc_mr.
*zcl_log_utilities=>save_log(
*EXPORTING im_ti_log_handle = lti_handle ).
*ENDIF.
