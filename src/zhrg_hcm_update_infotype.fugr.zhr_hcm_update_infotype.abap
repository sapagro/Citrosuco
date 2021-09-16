FUNCTION zhr_hcm_update_infotype.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IS_P2001) TYPE  P2001 OPTIONAL
*"     VALUE(IS_P9900) TYPE  P9900 OPTIONAL
*"  EXPORTING
*"     VALUE(EX_RETURN) LIKE  BAPIRETURN1 STRUCTURE  BAPIRETURN1
*"     VALUE(EX_KEY) LIKE  BAPIPAKEY STRUCTURE  BAPIPAKEY
*"----------------------------------------------------------------------


  IF is_p2001 IS NOT INITIAL.

    CALL FUNCTION 'HR_EMPLOYEE_ENQUEUE'
      EXPORTING
        number = is_p2001-pernr.

    CALL FUNCTION 'HR_INFOTYPE_OPERATION'
      EXPORTING
        infty         = '2001'
        number        = is_p2001-pernr
        validityend   = is_p2001-endda
        validitybegin = is_p2001-begda
        record        = is_p2001
        operation     = 'INS'
      IMPORTING
        return        = ex_return
        key           = ex_key.

    CALL FUNCTION 'HR_EMPLOYEE_DEQUEUE'
      EXPORTING
        number = is_p2001-pernr.

  ENDIF.

  IF is_p9900 IS NOT INITIAL.

    CALL FUNCTION 'HR_EMPLOYEE_ENQUEUE'
      EXPORTING
        number = is_p9900-pernr.

    CALL FUNCTION 'HR_INFOTYPE_OPERATION'
      EXPORTING
        infty         = '9900'
        number        = is_p9900-pernr
        validityend   = is_p9900-endda
        validitybegin = is_p9900-begda
        record        = is_p9900
        operation     = 'INS'
      IMPORTING
        return        = ex_return
        key           = ex_key.

    CALL FUNCTION 'HR_EMPLOYEE_DEQUEUE'
      EXPORTING
        number = is_p9900-pernr.

  ENDIF.

ENDFUNCTION.
