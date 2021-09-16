FUNCTION zfmrc_read .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IT_RCNUM) TYPE  ZT_FMRCNUM OPTIONAL
*"  EXPORTING
*"     REFERENCE(ET_RCHDR) TYPE  ZT_FMRCHDR
*"     REFERENCE(ET_RCLST) TYPE  ZT_FMRCLST
*"     REFERENCE(ET_RCBOM) TYPE  ZT_FMRCBOM
*"     REFERENCE(ET_RCVRS) TYPE  ZT_FMRCVRS
*"  EXCEPTIONS
*"      NO_DATA_EXISTS
*"----------------------------------------------------------------------
  REFRESH: et_rchdr[],
           et_rclst[],
           et_rcbom[].

  IF it_rcnum[] IS SUPPLIED.
    SELECT * FROM zfmrchdr
      INTO TABLE @et_rchdr
      FOR ALL ENTRIES IN @it_rcnum
     WHERE rcnum EQ @it_rcnum-rcnum.
  ENDIF.

  IF et_rchdr[] IS NOT INITIAL.
    SELECT * FROM zfmrclst
      INTO TABLE @et_rclst
      FOR ALL ENTRIES IN @et_rchdr
     WHERE rcnum = @et_rchdr-rcnum
      AND stlal = @et_rchdr-stlal.
  ENDIF.

  IF et_rchdr[] IS NOT INITIAL.
    SELECT * FROM zfmrcbom
      INTO TABLE @et_rcbom
      FOR ALL ENTRIES IN @et_rchdr
     WHERE rcnum = @et_rchdr-rcnum.
  ENDIF.

   IF et_rchdr[] IS NOT INITIAL.
    SELECT * FROM zfmrcvrs
      INTO TABLE @et_rcvrs
      FOR ALL ENTRIES IN @et_rchdr
     WHERE rcnum = @et_rchdr-rcnum.
  ENDIF.


  IF et_rchdr[] IS INITIAL.
    RAISE no_data_exists.
  ELSE.
    SORT: et_rclst[] BY rcnum,
          et_rchdr[] BY rcnum,
          et_rcbom[] BY rcnum,
          et_rcvrs[] BY rcnum.
  ENDIF.

ENDFUNCTION.
