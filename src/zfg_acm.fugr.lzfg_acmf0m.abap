*&---------------------------------------------------------------------*
*& Include          LZFG_ACMF0M
*&---------------------------------------------------------------------*
FORM messages_initialize USING lv_initiator TYPE /agri/gdescr
                               lv_subobject TYPE balsubobj
                               ls_achdr TYPE /agri/s_fmachdr.

  DATA: ls_context TYPE /agri/s_fmac_context.

  messages_init.
  messages_collect_all.
  messages_initiator_set lv_initiator c_object-log lv_subobject.

****Set the context for Process Log
  IF NOT ls_achdr IS INITIAL.
    MOVE-CORRESPONDING ls_achdr TO ls_context.
    messages_context_data_set ls_achdr-accom space space
                              '/AGRI/S_FMAC_CONTEXT' ls_context.
  ENDIF.

ENDFORM.                    " MESSAGES_INITIALIZE
*&---------------------------------------------------------------------*
*&      Form  MESSAGES_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_VARIABLES_INITIATOR  text
*----------------------------------------------------------------------*
FORM messages_display  USING  lv_initiator TYPE /agri/gdescr.

  DATA: ls_variant TYPE disvariant.
  ls_variant-report = c_program.
  ls_variant-handle = lv_initiator.

  messages_display lv_initiator c_true space space ls_variant.
  messages_init.
  CLEAR gs_variables-initiator.

ENDFORM.                    " MESSAGES_DISPLAY

FORM messages_get USING lv_initiator TYPE /agri/gdescr
               CHANGING ct_messages  TYPE /agri/t_gprolog.

  messages_get lv_initiator ct_messages.
  messages_init.
  CLEAR gs_variables-initiator.

ENDFORM.
