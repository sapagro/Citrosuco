METHOD zabs_message_add .

  DATA: lwa_messages TYPE /agri/s_gprolog,
        lv_probclass TYPE /agri/s_gprolog-probclass.

****Added on 06/29/2005
  IF NOT ms_environment-plcrule IS INITIAL.
    IF ms_environment-plcrule EQ mc_capture_none.
      EXIT.
    ELSEIF ms_environment-plcrule EQ mc_capture_errors_n_warnings.
      CHECK i_msgty CA 'EW'.
    ELSEIF ms_environment-plcrule EQ mc_capture_errors.
      CHECK i_msgty EQ 'E'.
    ENDIF.
  ENDIF.

****First of all check whether to collect or display messages
  IF ms_environment-collect_all EQ space.
    IF i_msgty EQ 'E'.
      lv_probclass = '2'.
      IF ms_environment-collect_errors EQ space.
*        MESSAGE ID i_msgid TYPE i_msgty
*        NUMBER i_msgno WITH i_msgv1 i_msgv2 i_msgv3 i_msgv4.
*        RAISING message_to_be_displayed.
      ENDIF.
    ELSEIF i_msgty EQ 'W'.
      lv_probclass = '3'.
      IF ms_environment-collect_warnings EQ space.
*        MESSAGE ID i_msgid TYPE i_msgty
*        NUMBER i_msgno WITH i_msgv1 i_msgv2 i_msgv3 i_msgv4.
*        RAISING message_to_be_displayed.
      ENDIF.
    ELSEIF i_msgty EQ 'I' OR i_msgty EQ 'S'.
      lv_probclass = '4'.
      IF ms_environment-collect_info EQ space.
*        MESSAGE ID i_msgid TYPE i_msgty
*        NUMBER i_msgno WITH i_msgv1 i_msgv2 i_msgv3 i_msgv4.
*        RAISING message_to_be_displayed.
      ENDIF.
    ELSE.
*      lv_probclass = '1'.  "A or X ??
*      MESSAGE ID i_msgid TYPE i_msgty
*      NUMBER i_msgno WITH i_msgv1 i_msgv2 i_msgv3 i_msgv4.
*      RAISING message_to_be_displayed.
    ENDIF.
  ENDIF.

  lwa_messages-level = i_level.
  lwa_messages-msgid = i_msgid.
  lwa_messages-msgty = i_msgty.
  lwa_messages-msgno = i_msgno.
  lwa_messages-msgv1 = i_msgv1.
  lwa_messages-msgv2 = i_msgv2.
  lwa_messages-msgv3 = i_msgv3.
  lwa_messages-msgv4 = i_msgv4.
  lwa_messages-probclass = lv_probclass.

****Context data
  IF i_default_context_data EQ 'X'.
    lwa_messages-context = ms_context_data.
  ELSE.
    lwa_messages-context = is_context_data.
  ENDIF.

****Initiator
  lwa_messages-initiator = mv_initiator.
  lwa_messages-aplobj = mv_aplobj.
  lwa_messages-subobj = mv_subobj.
  APPEND lwa_messages TO mt_messages.

  CALL FUNCTION 'ZFMFP_MEMORY_MESSAGES'
    EXPORTING
      it_messages = mt_messages[].

ENDMETHOD.
