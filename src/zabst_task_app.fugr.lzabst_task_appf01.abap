*----------------------------------------------------------------------*
***INCLUDE LZABST_TASK_APPF01.
*----------------------------------------------------------------------*
FORM before_save.

  DATA: lt_task_app TYPE TABLE OF zabst_task_app.

  FIELD-SYMBOLS : <fs_field> TYPE any.

  LOOP AT total.

    IF <action> EQ 'N' OR <action> EQ 'U'.

      APPEND <vim_total_struc> TO lt_task_app.

    ENDIF.

  ENDLOOP.



  IF lt_task_app[] IS NOT INITIAL.

* Perform validation

    LOOP AT lt_task_app ASSIGNING FIELD-SYMBOL(<fs_task_app>).

      IF <fs_task_app>-ctapp IS INITIAL
        AND <fs_task_app>-stapp IS INITIAL.

        MESSAGE TEXT-002 TYPE 'S' DISPLAY LIKE 'E'.

        vim_abort_saving = abap_true.

        sy-subrc = 4.

        EXIT.

      ENDIF.

    ENDLOOP.

  ENDIF.

ENDFORM.
