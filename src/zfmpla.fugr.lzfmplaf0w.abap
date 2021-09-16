*&---------------------------------------------------------------------*
*& Include          LZFMACAF0W
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form WORK_DAYS_GET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> I_DATAB
*&      --> I_DATBI
*&---------------------------------------------------------------------*
FORM work_days_get  USING    lv_datab TYPE datum
                             lv_datbi TYPE datum
                    CHANGING lt_work_days TYPE wgrc_fabkl_tty.

  CALL FUNCTION 'DAY_ATTRIBUTES_GET'
    EXPORTING
      factory_calendar           = 'BR'
      holiday_calendar           = 'BR'
      date_from                  = lv_datab
      date_to                    = lv_datbi
      language                   = sy-langu
*     NON_ISO                    = ' '
*    IMPORTING
*     year_of_valid_from         =
*     year_of_valid_to           =
*     RETURNCODE                 =
    TABLES
      day_attributes             = lt_work_days
    EXCEPTIONS
      factory_calendar_not_found = 1
      holiday_calendar_not_found = 2
      date_has_invalid_format    = 3
      date_inconsistency         = 4
      OTHERS                     = 5.

ENDFORM.
