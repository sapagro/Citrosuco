FUNCTION ZFMPL_READ.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IT_PLNUM) TYPE  ZT_FM_PLNUM
*"  EXPORTING
*"     REFERENCE(ET_PLHDR) TYPE  ZT_FMPLHDR
*"     REFERENCE(ET_PLITM) TYPE  ZT_FMPLITM
*"  EXCEPTIONS
*"      NO_DATA_EXISTS
*"----------------------------------------------------------------------

  REFRESH: et_plhdr[],
           et_plitm[].

  IF it_plnum[] IS INITIAL.
    RAISE no_data_exists.
  ENDIF.
  SELECT * FROM zfmplhdr
           INTO CORRESPONDING FIELDS OF TABLE et_plhdr
            FOR ALL ENTRIES IN it_plnum
          WHERE plnum EQ it_plnum-plnum.
  IF sy-subrc NE 0.
    RAISE no_data_exists.
  ENDIF.
  IF it_plnum[] IS NOT INITIAL.
    SELECT * FROM zfmplitm
             INTO CORRESPONDING FIELDS OF TABLE et_plitm
              FOR ALL ENTRIES IN it_plnum
            WHERE plnum = it_plnum-plnum.
    IF sy-subrc NE 0.
      RAISE no_data_exists.
    ENDIF.
  ENDIF.

  SORT: et_plitm[]  BY plnum,
        et_plhdr[]  BY plnum.

ENDFUNCTION.
