FUNCTION zfmac_read .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IT_ACNUM) TYPE  ZT_FM_ACNUM OPTIONAL
*"  EXPORTING
*"     REFERENCE(ET_ACHDR) TYPE  ZT_FMACHDR
*"     REFERENCE(ET_ACITM) TYPE  ZT_FMACITM
*"     REFERENCE(ET_ACVLC) TYPE  ZT_FMACVLCL
*"  EXCEPTIONS
*"      NO_DATA_EXISTS
*"----------------------------------------------------------------------
  REFRESH: et_achdr[],
           et_acitm[],
           et_acvlc[].

  IF it_acnum[] IS SUPPLIED.
    SELECT * FROM zfmachdr
      INTO TABLE @et_achdr
      FOR ALL ENTRIES IN @it_acnum
     WHERE acnum EQ @it_acnum-acnum.
  ELSE.
    SELECT * FROM zfmachdr
      INTO TABLE @et_achdr.
  ENDIF.

  IF et_achdr[] IS NOT INITIAL.
    SELECT * FROM zfmacvlcl
      INTO TABLE @et_acvlc
      FOR ALL ENTRIES IN @et_achdr
     WHERE acnum = @et_achdr-acnum.
  ENDIF.

  IF et_achdr[] IS NOT INITIAL.
    SELECT * FROM zfmaitm
      INTO TABLE @et_acitm
      FOR ALL ENTRIES IN @et_achdr
     WHERE acnum = @et_achdr-acnum.
  ENDIF.

  IF et_achdr[] IS INITIAL.
    RAISE no_data_exists.
  ELSE.
    SORT: et_acitm[] BY acnum,
          et_achdr[] BY acnum,
          et_acvlc[] BY acnum.
  ENDIF.

ENDFUNCTION.
