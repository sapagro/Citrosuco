interface ZIF_ZFMPL_PLANNING_Q
  public .


  types:
    MANDT type C length 000003 .
  types:
    ZFMACNUM type C length 000010 .
  types:
    ZFMAPLMATNR type C length 000040 .
  types:
    SPMON type N length 000006 .
  types:
    MATKL type C length 000009 .
  types:
    ZFMPLAPR type P length 8  decimals 000002 .
  types:
    MEINS type C length 000003 .
  types:
    BOOLEAN type C length 000001 .
  types:
    ZFMPLCHAR type C length 000020 .
  types:
    ZFMPLPER type C length 000010 .
  types:
    begin of ZFMPLMENS,
      MANDT type MANDT,
      ACNUM type ZFMACNUM,
      MATNR type ZFMAPLMATNR,
      PERIODO type SPMON,
      MATKL type MATKL,
      QTPLM type ZFMPLAPR,
      UNPLM type MEINS,
      QTD_PASSADAS type ZFMPLAPR,
      DATA_INI type DATS,
      DATA_FIM type DATS,
      INVALIDO type BOOLEAN,
      PERIODO1 type SPMON,
      PERIODO2 type SPMON,
      PERIODO3 type SPMON,
      PERIODO4 type SPMON,
      PERIODO5 type SPMON,
      CARACTERISTICA type ZFMPLCHAR,
      VALOR1 type ZFMPLAPR,
      VALOR2 type ZFMPLAPR,
      VALOR3 type ZFMPLAPR,
      VALOR4 type ZFMPLAPR,
      VALOR5 type ZFMPLAPR,
      PERIODO1_TXT type ZFMPLPER,
      PERIODO2_TXT type ZFMPLPER,
      PERIODO3_TXT type ZFMPLPER,
      PERIODO4_TXT type ZFMPLPER,
      PERIODO5_TXT type ZFMPLPER,
    end of ZFMPLMENS .
  types:
    ZT_FMPLMENS                    type standard table of ZFMPLMENS                      with non-unique default key .
  types:
    BAPI_MTYPE type C length 000001 .
  types:
    SYMSGID type C length 000020 .
  types:
    SYMSGNO type N length 000003 .
  types:
    BAPI_MSG type C length 000220 .
  types:
    BALOGNR type C length 000020 .
  types:
    BALMNR type N length 000006 .
  types:
    SYMSGV type C length 000050 .
  types:
    BAPI_PARAM type C length 000032 .
  types:
    BAPI_FLD type C length 000030 .
  types:
    BAPILOGSYS type C length 000010 .
  types:
    begin of BAPIRET2,
      TYPE type BAPI_MTYPE,
      ID type SYMSGID,
      NUMBER type SYMSGNO,
      MESSAGE type BAPI_MSG,
      LOG_NO type BALOGNR,
      LOG_MSG_NO type BALMNR,
      MESSAGE_V1 type SYMSGV,
      MESSAGE_V2 type SYMSGV,
      MESSAGE_V3 type SYMSGV,
      MESSAGE_V4 type SYMSGV,
      PARAMETER type BAPI_PARAM,
      ROW type INT4,
      FIELD type BAPI_FLD,
      SYSTEM type BAPILOGSYS,
    end of BAPIRET2 .
  types BAPIMAXROW type INT4 .
endinterface.
