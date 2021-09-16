FUNCTION /agri/glfl_user_status_set.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_TPLNR) TYPE  /AGRI/GLTPLNR_FL
*"     VALUE(I_USER_STATUS) TYPE  /AGRI/GUSTAT OPTIONAL
*"     VALUE(I_SET_INACTIVE) TYPE  SY-DATAR OPTIONAL
*"     VALUE(I_SAVE) TYPE  SY-DATAR DEFAULT 'X'
*"     VALUE(I_SET_UPDATE_TASK) TYPE  SY-DATAR DEFAULT 'X'
*"     VALUE(I_COMMIT_WORK) TYPE  SY-DATAR DEFAULT 'X'
*"     VALUE(IS_ACT_OUTCOME) TYPE  /AGRI/S_GACOUTCOME OPTIONAL
*"  EXPORTING
*"     REFERENCE(ET_MESSAGES) TYPE  /AGRI/T_GPROLOG
*"  EXCEPTIONS
*"      TERRAIN_NOT_FOUND
*"      TERRAIN_LOCKED
*"      OBJECT_ERRORS
*"      ERRORS_IN_SAVE
*"----------------------------------------------------------------------
  DATA:  lv_subrc         TYPE sysubrc,
         lv_errors_in_save,
         ls_message       TYPE /agri/s_gprolog,
         ls_fldoc_infocus TYPE /agri/s_glfl_doc,
         lt_fldoc_infocus TYPE /agri/t_glfl_doc,
         lt_tplnr         TYPE /agri/t_gltplnr,
         lt_messages      TYPE /agri/t_gprolog.

  DATA lv_objky  TYPE na_objkey.
  DATA: lv_kfrst TYPE kfrst.

  IF i_tplnr IS INITIAL.
    RAISE terrain_not_found.
  ENDIF.

  PERFORM document_data_initialize USING c_true.

  APPEND i_tplnr TO lt_tplnr.
  CALL FUNCTION '/AGRI/GLFL_VIEW'
    EXPORTING
      it_tplnr       = lt_tplnr
    IMPORTING
      et_fldoc       = lt_fldoc_infocus
    EXCEPTIONS
      no_data_exists = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
    RAISE terrain_not_found.
  ELSE.
    READ TABLE lt_fldoc_infocus INTO ls_fldoc_infocus INDEX 1.
  ENDIF.

  gs_fldoc_infocus = ls_fldoc_infocus.

  CHECK ls_fldoc_infocus-x-flhdr-objnr NE space.
  CHECK ls_fldoc_infocus-x-flhdr-ustat NE i_user_status.
****Start messages
  messages_init.
  messages_collect_all.
  messages_capture_rule_set space.
  gs_variables-initiator = c_log_initiator-change.
  messages_initiator_set gs_variables-initiator
  c_object-log c_log_subobject-change.

  PERFORM document_infocus_lock USING ls_fldoc_infocus-tplnr_fl
                                      ls_fldoc_infocus-x-flhdr-strno
                                      c_msg_type-error
                               CHANGING lv_subrc.

  IF lv_subrc NE 0.
    messages_get gs_variables-initiator et_messages[].
    CLEAR gs_variables-initiator.
    RAISE terrain_locked.
  ENDIF.
  gs_variables-document_mode = c_mode_change.

  lv_objky = ls_fldoc_infocus-x-flhdr-tplnr_fl.
  CALL FUNCTION '/AGRI/G_RV_MESSAGES_READ'
    EXPORTING
      i_msg_kappl    = 'WS'
      i_msg_objky    = lv_objky
*     I_MSG_OBJKY_TO = ' '
    .

  PERFORM user_status_update USING ls_fldoc_infocus-x-flhdr-tplnr_fl
                                     i_user_status
                                     i_set_inactive
                                     is_act_outcome
                                     c_true
                            CHANGING ls_fldoc_infocus-x-flhdr
                                     lv_subrc.

  IF lv_subrc NE 0.
    messages_get gs_variables-initiator et_messages[].
    CLEAR gs_variables-initiator.
    messages_init.
    RAISE object_errors.
  ENDIF.

  PERFORM release_status_determine USING ls_fldoc_infocus-x-flhdr-ustat
                                   CHANGING lv_kfrst.

  IF NOT lv_kfrst IS INITIAL.
    IF lv_kfrst EQ 'R'.
      lv_kfrst = space.
*      ls_FLdoc_infocus-x-FLhdr-status = space.
    ENDIF.
    IF ls_fldoc_infocus-x-flhdr-kfrst NE lv_kfrst.
      ls_fldoc_infocus-x-flhdr-kfrst = lv_kfrst.
      IF ls_fldoc_infocus-x-flhdr-updkz NE c_updkz_new.
        ls_fldoc_infocus-x-flhdr-updkz = c_updkz_update.
      ENDIF.
    ENDIF.
  ENDIF.

  IF i_save EQ c_true.
    REFRESH lt_fldoc_infocus.
    APPEND ls_fldoc_infocus TO lt_fldoc_infocus.

    CALL FUNCTION '/AGRI/GLFL_SAVE_SINGLE'       ##COMPATIBLE
*     EXPORTING
*       I_SET_UPDATE_TASK        = 'X'
*       I_COMMIT_WORK            = 'X'
*       IREF_TEXT                =
      CHANGING
        cs_fldoc                 = lt_fldoc_infocus     "#EC COMPATIBLE
        ct_messages              = lt_messages
     EXCEPTIONS
       no_change                = 1
       error_while_saving       = 2
       OTHERS                   = 3
              .

    lv_subrc = sy-subrc.
    LOOP AT lt_messages INTO ls_message.
      MESSAGE ID ls_message-msgid TYPE ls_message-msgty
         NUMBER ls_message-msgno WITH ls_message-msgv1
         ls_message-msgv2 ls_message-msgv3 ls_message-msgv4
                    INTO sy-msgli.
      message_simple space.
    ENDLOOP.

    LOOP AT lt_messages INTO ls_message.
      MESSAGE ID ls_message-msgid TYPE ls_message-msgty
         NUMBER ls_message-msgno WITH ls_message-msgv1
         ls_message-msgv2 ls_message-msgv3 ls_message-msgv4
                    INTO sy-msgli.
      message_simple space.
    ENDLOOP.

    IF lv_subrc NE 0.
      messages_get gs_variables-initiator et_messages[].
      CLEAR gs_variables-initiator.
      messages_init.
      RAISE errors_in_save.
    ENDIF.
  ENDIF.

  messages_get gs_variables-initiator et_messages[].
  CLEAR gs_variables-initiator.
  messages_init.


ENDFUNCTION.   "#EC CI_VALPAR
