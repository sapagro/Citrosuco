FUNCTION y_ofarm_lav_logica.
*"----------------------------------------------------------------------
*"*"Interface local:
*"----------------------------------------------------------------------
*----------------------------------------------------------------------*
*                    SOFTWARE FACTORY <Vistex>                         *
*----------------------------------------------------------------------*
* Requirement ID    : Gap ID 03                                        *
* Program           : LOGICAL_CROP_CREATION                            *
* Created By        : Adonis Rossato                                   *
* Creation Date     : 15.04.2021                                       *
* Descriptión       : Logical Crop Creation                            *
*----------------------------------------------------------------------*
*                    LOG OF MODIFICATIONS                              *
*----------------------------------------------------------------------*
* Changed by        :                                                  *
* Changed Date      :                                                  *
* Transport         :                                                  *
*----------------------------------------------------------------------*

*-- Export the Memory variable to BADI : WORKORDER_UPDATE
  EXPORT gv_fm_call TO MEMORY ID 'Z_PROG_CHECK'.

*-- Clear global data
  PERFORM global_data_intialize.

*-- Validations to input data
  PERFORM validate_input_data CHANGING is_indata
                                       et_messages[].

*-- Save messages in log for internal purpose
  PERFORM save_log USING et_messages[].

  CHECK et_messages[] IS INITIAL.

  CASE is_indata-indfield.
    WHEN gc_constants-insert.

*-- Crop season creation
      CALL FUNCTION 'ZVX_CREATE_CROP_SEASON'
        EXPORTING
          is_indata   = is_indata
        IMPORTING
          et_messages = et_messages[]
          es_outdata  = es_outdata.

    WHEN gc_constants-update.

*-- Crop season changes
      CALL FUNCTION 'ZVX_CHANGE_CROP_SEASON'
        EXPORTING
          is_indata   = is_indata
        IMPORTING
          et_messages = et_messages[]
          es_outdata  = es_outdata.

    WHEN gc_constants-delete.

*-- Crop season deletion
      CALL FUNCTION 'ZVX_DELETE_CROP_SEASON'
        EXPORTING
          is_indata   = is_indata
        IMPORTING
          et_messages = et_messages[].

  ENDCASE.

*-- Save messages in log for internal purpose
  PERFORM save_log USING et_messages[].

ENDFUNCTION.
