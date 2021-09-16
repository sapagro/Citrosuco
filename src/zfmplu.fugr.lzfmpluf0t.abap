*----------------------------------------------------------------------*
***INCLUDE /AGRI/LGLFLUF0T .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  TABLES_COMPLETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tables_complete USING lref_text TYPE REF TO /agri/cl_gtext_process
                  CHANGING lwa_pldoc TYPE zsc_fmpl_doc
                           lv_failed.

  CASE lwa_pldoc-x-plhdr-updkz.
    WHEN c_updkz_new.

      PERFORM pl_number_generate USING lwa_pldoc-x-plhdr-pltyp
                              CHANGING lwa_pldoc-plnum
                                       lv_failed.
      IF lv_failed EQ c_true.
        EXIT.
      ENDIF.

      PERFORM pl_label_replace USING lwa_pldoc
                                     lref_text.

      admin_data_fill lwa_pldoc-x-plhdr.
    WHEN OTHERS.
      IF lwa_pldoc-x-plhdr-updkz IS INITIAL AND NOT
         lwa_pldoc-x IS INITIAL.
        lwa_pldoc-x-plhdr-updkz = c_updkz_update.
      ENDIF.
      admin_data_fill lwa_pldoc-x-plhdr.
  ENDCASE.

ENDFORM.                    " TABLES_COMPLETE
*&---------------------------------------------------------------------*
*&      Form  TEXT_OBJECT_VALUE_SWITCH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM text_object_value_switch  USING lv_tplnr
                            CHANGING lref_text TYPE REF TO /agri/cl_gtext_process.

  DATA: lwa_tdobtxt TYPE /agri/s_gtdobtxt.

**** ESP6 Task #30035 - Global Text Engine Integration
*  DATA: lv_object     TYPE tdobname,
  DATA: lv_object     TYPE /agri/gtxobjval,
*        lv_object_new TYPE tdobname,
        lv_object_new TYPE /agri/gtxobjval,
*        lv_object_tmp  TYPE tdobname.
        lv_object_tmp TYPE /agri/gtxobjval.
****

  IF NOT lref_text IS INITIAL.
    LOOP AT lref_text->mt_tdobtxt INTO lwa_tdobtxt.

      lv_object     = lwa_tdobtxt-objval.
      lv_object_tmp = lwa_tdobtxt-objval.

      SHIFT lv_object_tmp LEFT BY 3 PLACES.

      CONCATENATE lv_tplnr lv_object_tmp INTO lv_object_new.
      CONDENSE lv_object_new.

      CALL METHOD lref_text->text_objval_switch
        EXPORTING
          i_object             = lwa_tdobtxt-tdobject
          i_objval_old         = lv_object
          i_objval_new         = lv_object_new
        EXCEPTIONS
          object_key_not_found = 1
          OTHERS               = 2.

    ENDLOOP.
  ENDIF.

ENDFORM.                    " TEXT_OBJECT_VALUE_SWITCH
