class ZCL_ZABS_GLTR_GIS_EXT_MPC definition
  public
  inheriting from /IWBEP/CL_MGW_PUSH_ABS_MODEL
  create public .

public section.

  types:
    begin of TS_CREATETASKORDERFI,
        AUFNR type C length 12,
        MATNR type C length 18,
    end of TS_CREATETASKORDERFI .
  types:
   begin of ts_text_element,
      artifact_name  type c length 40,       " technical name
      artifact_type  type c length 4,
      parent_artifact_name type c length 40, " technical name
      parent_artifact_type type c length 4,
      text_symbol    type textpoolky,
   end of ts_text_element .
  types:
         tt_text_elements type standard table of ts_text_element with key text_symbol .
  types:
    begin of TS_STATUSFI,
        DATAB type TIMESTAMPL,
        TPLNR_FL type C length 40,
    end of TS_STATUSFI .
  types:
    begin of TS_GISTASKORDFI,
        TPLNR_FL type C length 40,
    end of TS_GISTASKORDFI .
  types:
    begin of TS_GETTASKFI,
        VARIA type C length 18,
        TPLNRFLL type C length 40,
        TPLNRFLH type C length 40,
        DATAB type TIMESTAMPL,
        CPROS type C length 10,
        CMNUM type C length 10,
        AUFNR type C length 12,
    end of TS_GETTASKFI .
  types:
    begin of TS_GETVARIANTANDCPROSFI,
        TPLNR_FLL type C length 40,
        TPLNR_FLH type C length 40,
        DATAB type TIMESTAMP,
        CMNUM type C length 10,
    end of TS_GETVARIANTANDCPROSFI .
  types:
    begin of TS_WORKORDERFI,
        TPLNR_FL2 type C length 40,
        TPLNR_FL3 type C length 40,
        VARIA type C length 18,
        TPLNR_FL1 type C length 40,
        DATAB type TIMESTAMPL,
        CPROS type C length 10,
        IWERK type C length 4,
        MATNR type C length 18,
        CMNUM type C length 10,
        TPLNR_FL15 type C length 40,
        TPLNR_FL14 type C length 40,
        TPLNR_FL13 type C length 40,
        TPLNR_FL12 type C length 40,
        TPLNR_FL11 type C length 40,
        TPLNR_FL10 type C length 40,
        TPLNR_FL9 type C length 40,
        TPLNR_FL8 type C length 40,
        TPLNR_FL7 type C length 40,
        TPLNR_FL6 type C length 40,
        TPLNR_FL5 type C length 40,
        TPLNR_FL4 type C length 40,
    end of TS_WORKORDERFI .
  types:
    begin of TS_GETCPROSFI,
        VARIA type C length 18,
        TPLNR_FLL type C length 40,
        TPLNR_FLH type C length 40,
        DATAB type TIMESTAMPL,
        CMNUM type C length 10,
    end of TS_GETCPROSFI .
  types:
    begin of TS_WOCONFIRMFI,
        TPLNR_FL type C length 40,
    end of TS_WOCONFIRMFI .
  types:
    begin of TS_WODETAILSFI,
        MATNR type C length 18,
        TPLNR_FL type C length 40,
        WONUM type C length 10,
    end of TS_WODETAILSFI .
  types:
    begin of TS_TASKCHECKFI,
        IWERK type C length 4,
        MATNR type C length 18,
    end of TS_TASKCHECKFI .
  types:
     TS_MEASUREMENTATTRIBUTES type ZABS_STR_GLMDATV_ODS .
  types:
TT_MEASUREMENTATTRIBUTES type standard table of TS_MEASUREMENTATTRIBUTES .
  types:
     TS_MEASUREMENTDOCUMENTHEADER type ZABS_STR_GLMDHDR_ODS .
  types:
TT_MEASUREMENTDOCUMENTHEADER type standard table of TS_MEASUREMENTDOCUMENTHEADER .
  types:
     TS_MEASUREMENTGROUP type /AGRI/GLAGHA .
  types:
TT_MEASUREMENTGROUP type standard table of TS_MEASUREMENTGROUP .
  types:
  begin of TS_MEASUREMENTTYPES,
     MDTYP type C length 4,
     NUMKI type C length 2,
     ASLVL type C length 1,
     DESCR type C length 30,
     DDTEXT type C length 60,
  end of TS_MEASUREMENTTYPES .
  types:
TT_MEASUREMENTTYPES type standard table of TS_MEASUREMENTTYPES .
  types:
  begin of TS_CATANALYTICS,
     WOGJAHRCONF type C length 4,
     WOCONF type C length 10,
     WO type C length 10,
     WERKS_DESC type C length 30,
     WERKS type C length 4,
     TPLNR type C length 40,
     TOTAL type P length 7 decimals 3,
     TMAT_DESC type C length 40,
     TASK_MATERIAL type C length 40,
     START_TIME type T,
     START_DATE type TIMESTAMPL,
     POS type C length 6,
     PLTXT type C length 40,
     MSEHI type C length 3,
     MJAHR_261 type C length 4,
     MBLNR_261 type C length 10,
     MATNR type C length 40,
     MAKTX type C length 40,
     LGORT type C length 4,
     LGOBE type C length 16,
     GJAHR type C length 4,
     FINISH_TIME type T,
     FINISH_DATE type TIMESTAMPL,
     ENTRY_QNT type P length 7 decimals 3,
     CMNUM type C length 10,
     CHARG type C length 10,
     BUKRS type C length 4,
     BUDAT type TIMESTAMPL,
     BLDAT type TIMESTAMPL,
     AUFNR type C length 12,
  end of TS_CATANALYTICS .
  types:
TT_CATANALYTICS type standard table of TS_CATANALYTICS .
  types:
  begin of TS_CROPSEASON,
     VARIA type C length 18,
     VALIDON type TIMESTAMPL,
     TPLNR_FL type C length 40,
     SEASON type C length 10,
     POSNR type C length 4,
     PAREA type P length 8 decimals 2,
     MSEHI type C length 3,
     MATNR type C length 40,
     MAKTX type C length 40,
     IWERK type C length 4,
     GYEAR type C length 4,
     GAREA type P length 8 decimals 2,
     EXHAD type TIMESTAMPL,
     ESUOM type C length 3,
     ESTON type P length 8 decimals 3,
     DESCR type C length 40,
     DATBI type TIMESTAMPL,
     DATAB type TIMESTAMPL,
     CPROS type C length 10,
     CONTR type C length 6,
     CMNUM type C length 10,
     CLYLD type P length 8 decimals 3,
     CLUOM type C length 3,
     AUFNR_TO type C length 12,
     AUFNR type C length 12,
     AAREA type P length 8 decimals 2,
  end of TS_CROPSEASON .
  types:
TT_CROPSEASON type standard table of TS_CROPSEASON .
  types:
  begin of TS_CSMDMITEM,
     ANZST type /IWBEP/SB_ODATA_TY_INT2,
     ATFOR type C length 4,
     ATSON type C length 1,
     MDOCM type C length 20,
     MDITM type C length 6,
     ERROR type C length 1,
     DELETED type C length 1,
     DATUV type TIMESTAMPL,
     CUNIT type C length 6,
     ATZIS type C length 3,
     ATWTB type C length 50,
     ATWRT type C length 30,
     ATVGLART type C length 1,
     ATTLV type F,
     ATTLB type F,
     ATSRT type C length 4,
     ATPRZ type C length 1,
     ATNAM type C length 30,
     ATINC type F,
     ATIMB type C length 30,
     ASLVL type C length 1,
     MPGRP type C length 18,
     MDTYP type C length 4,
     ATFLV type F,
     ATFLB type F,
     ATEXT type C length 40,
     ATCOD type C length 1,
     ATBEZ type C length 30,
     ATAWE type C length 3,
     UPDKZ type C length 1,
     ATAW1 type C length 3,
     ATINN type C length 30,
     ATAUT type C length 1,
  end of TS_CSMDMITEM .
  types:
TT_CSMDMITEM type standard table of TS_CSMDMITEM .
  types:
  begin of TS_CSMDMMASSDATA,
     MDTYP type C length 4,
     MPGRP type C length 18,
     MDOCM type C length 20,
     TPLNR_FL type C length 40,
     TPLNR_FLL type C length 40,
     TPLNR_FLH type C length 40,
     PLTXT type C length 40,
     CONTR type C length 6,
     CMNUM type C length 10,
     DESCR type C length 40,
     MUSER type C length 12,
     EQUNR type C length 18,
     ASLVL type C length 1,
     DATAB type TIMESTAMPL,
     DATBI type TIMESTAMPL,
     FLDNAM type C length 30,
     ATNAM type C length 30,
     CUNIT type C length 6,
     UPDKZ type C length 1,
  end of TS_CSMDMMASSDATA .
  types:
TT_CSMDMMASSDATA type standard table of TS_CSMDMMASSDATA .
  types:
  begin of TS_FIELDCATALOG,
     DECMLFIELD type C length 30,
     DECIMALS_O type C length 6,
     ROUNDFIELD type C length 30,
     TXT_FIELD type C length 30,
     REF_TABLE type C length 30,
     REF_FIELD type C length 30,
     PARAMETER9 type I,
     PARAMETER8 type I,
     PARAMETER7 type I,
     PARAMETER6 type I,
     PARAMETER5 type I,
     PARAMETER4 type C length 30,
     PARAMETER3 type C length 30,
     PARAMETER2 type C length 30,
     PARAMETER1 type C length 30,
     PARAMETER0 type C length 30,
     DECFLOAT_STYLE type C length 2,
     DRDN_ALIAS type C length 1,
     NO_INIT_CH type C length 1,
     COL_OPT type C length 1,
     H_FTYPE type C length 3,
     NO_MERGING type C length 1,
     DRDN_FIELD type C length 30,
     DRDN_HNDL type I,
     STYLE4 type X length 4,
     STYLE3 type X length 4,
     STYLE2 type X length 4,
     MARK type C length 1,
     GET_STYLE type C length 1,
     INDX_DECML type I,
     INDX_ROUND type I,
     INDX_IFIEL type I,
     INDX_QFIEL type I,
     INDX_CFIEL type I,
     INDX_FIELD type I,
     MAC type C length 1,
     DRAGDROPID type I,
     DD_ROLL type C length 30,
     H_SELECT type C length 1,
     H_COL_KEY type C length 12,
     HIER_CPOS type /IWBEP/SB_ODATA_TY_INT2,
     TECH_COMP type C length 1,
     TECH_FORM type I,
     TECH_COL type I,
     EDIT type C length 1,
     TIPDDICTXT type C length 1,
     SELDDICTXT type C length 1,
     COLDDICTXT type C length 1,
     SCRTEXT_S type C length 10,
     SCRTEXT_M type C length 20,
     SCRTEXT_L type C length 40,
     COLTEXT type C length 40,
     DECIMALS type C length 6,
     DD_OUTLEN type C length 6,
     DO_SUM type C length 1,
     FIX_COLUMN type C length 1,
     EMPHASIZE type C length 4,
     EDIT_MASK type C length 60,
     NO_CONVEXT type C length 1,
     NO_ZERO type C length 1,
     NO_SIGN type C length 1,
     LZERO type C length 1,
     JUST type C length 1,
     CHECKBOX type C length 1,
     SYMBOL type C length 1,
     ICON type C length 1,
     KEY_SEL type C length 1,
     KEY type C length 1,
     EXPONENT type C length 3,
     ROUND type I,
     IFIELDNAME type C length 30,
     QFIELDNAME type C length 30,
     QUANTITY type C length 3,
     CFIELDNAME type C length 30,
     CURRENCY type C length 5,
     TABNAME type C length 30,
     FIELDNAME type C length 30,
     COL_POS type I,
     ROW_POS type I,
     STRUCTURE type C length 30,
     BOR type C length 10,
     STYLE type X length 4,
     HREF_HNDL type I,
     WEB_FIELD type C length 30,
     VALEXI type C length 1,
     CHECKTABLE type C length 30,
     AUTO_VALUE type C length 1,
     F4AVAILABL type C length 1,
     COL_ID type I,
     DFIELDNAME type C length 30,
     HOTSPOT type C length 1,
     SP_GROUP type C length 4,
     DOMNAME type C length 30,
     REPREP type C length 1,
     HIER_LEVEL type I,
     REPTEXT type C length 55,
     LOWERCASE type C length 1,
     INTLEN type C length 6,
     INTTYPE type C length 1,
     DATATYPE type C length 10,
     ROLLNAME type C length 30,
     TOOLTIP type C length 40,
     SELTEXT type C length 40,
     CONVEXIT type C length 5,
     OUTPUTLEN type C length 6,
     TECH type C length 1,
     NO_OUT type C length 1,
     NO_SUM type C length 1,
  end of TS_FIELDCATALOG .
  types:
TT_FIELDCATALOG type standard table of TS_FIELDCATALOG .
  types:
  begin of TS_MDMCRF4,
     ASLVL type C length 1,
     MDTYP type C length 4,
     MPGRP type C length 18,
  end of TS_MDMCRF4 .
  types:
TT_MDMCRF4 type standard table of TS_MDMCRF4 .
  types:
  begin of TS_POCONFIRMATIONS,
     WONUM type C length 10,
     VORNR type C length 4,
     UPDKZ type C length 1,
     RUECK type C length 10,
     RMZHL type C length 8,
     OCNUM_REF type C length 10,
     OCNUM type C length 10,
     MJAHR_GI type C length 4,
     MJAHR type C length 4,
     MENGE type P length 7 decimals 3,
     MEINS type C length 3,
     MBLNR_GI type C length 10,
     MBLNR type C length 10,
     MATNR type C length 40,
     MAKTX type C length 40,
     LTXA1 type C length 40,
     LGORT type C length 4,
     ERZET type T,
     ERNAM type C length 12,
     ERDAT type TIMESTAMPL,
     CONTR_REF type C length 6,
     CMNUM type C length 10,
     CHARG_REF type C length 10,
     CHARG type C length 10,
     CGLST_TXT type C length 60,
     CGLST type C length 1,
     BWART_GI type C length 3,
     BWART type C length 3,
     BUDAT_MKPF type TIMESTAMPL,
     AUFNR type C length 12,
     AEZET type T,
     AENAM type C length 12,
     AEDAT type TIMESTAMPL,
  end of TS_POCONFIRMATIONS .
  types:
TT_POCONFIRMATIONS type standard table of TS_POCONFIRMATIONS .
  types:
  begin of TS_PODETAILS,
     GMEIN type C length 3,
     GSTRI type TIMESTAMPL,
     GSTRP type TIMESTAMPL,
     GSTRS type TIMESTAMPL,
     GSUZI type T,
     GSUZP type T,
     GSUZS type T,
     GWEMG type P length 7 decimals 3,
     MAKTX type C length 40,
     MANDT type C length 3,
     MATNR type C length 40,
     MIMEZ type P length 7 decimals 3,
     OBJNR type C length 22,
     REDKZ type C length 1,
     REDKZ_TXT type C length 40,
     SICHZ type C length 3,
     STTXT type C length 40,
     TERHW type C length 1,
     TERHW_TXT type C length 60,
     TERKZ type C length 1,
     TERKZ_TXT type C length 30,
     VORGZ type C length 3,
     WERKS type C length 4,
     AEDAT type TIMESTAMPL,
     AENAM type C length 12,
     AEZET type T,
     APRIO type C length 1,
     APROZ type P length 3 decimals 2,
     ASTTX type C length 40,
     AUART type C length 4,
     AUFNR type C length 12,
     BEZEI type C length 20,
     ERDAT type TIMESTAMPL,
     ERNAM type C length 12,
     ERZET type T,
     FHORI type C length 3,
     FREIZ type C length 3,
     FTRMI type TIMESTAMPL,
     FTRMS type TIMESTAMPL,
     GAMNG type P length 7 decimals 3,
     GASMG type P length 7 decimals 3,
     GLTRI type TIMESTAMPL,
     GLTRP type TIMESTAMPL,
     GLTRS type TIMESTAMPL,
     GLUZP type T,
     GLUZS type T,
  end of TS_PODETAILS .
  types:
TT_PODETAILS type standard table of TS_PODETAILS .
  types:
  begin of TS_POHEADER,
     GWEMG type P length 7 decimals 3,
     IWERK type C length 4,
     MADUR type P length 5 decimals 3,
     MAKTX type C length 40,
     MATNR type C length 40,
     MAUNT type C length 3,
     MIDUR type P length 5 decimals 3,
     MIUNT type C length 3,
     OBJNR type C length 22,
     OBTXT type C length 40,
     ORDLV type C length 1,
     OSTAT type C length 1,
     PERNR type C length 8,
     POSNR_V type C length 4,
     PRODNET type C length 1,
     PROID type C length 24,
     RSNUM type C length 10,
     SAPPL type C length 2,
     SCHTA type C length 1,
     SICON type C length 8,
     STTXT type C length 40,
     TECOM type C length 1,
     TPLMA type C length 40,
     TPLNR_FL type C length 40,
     UPDKZ type C length 1,
     VARIA type C length 18,
     VERID type C length 4,
     ZUKRI type C length 40,
     AEDAT type TIMESTAMPL,
     AENAM type C length 12,
     AEZET type T,
     ASTTX type C length 40,
     AUART type C length 4,
     AUFNR type C length 12,
     AUFNR_V type C length 12,
     AUFPL type C length 10,
     AUTYP type C length 2,
     BEZEI type C length 20,
     CHARG type C length 10,
     CLASS type C length 1,
     CMNUM type C length 10,
     COMNG type P length 7 decimals 3,
     CONTR type C length 6,
     CPROS type C length 10,
     CSTAT type C length 4,
     DATAB type TIMESTAMPL,
     DATBI type TIMESTAMPL,
     EQUNR type C length 18,
     ERDAT type TIMESTAMPL,
     ERNAM type C length 12,
     ERZET type T,
     GAMNG type P length 7 decimals 3,
     GLTRP type TIMESTAMPL,
     GMEIN type C length 3,
     GRCRE type C length 1,
     GSTRP type TIMESTAMPL,
  end of TS_POHEADER .
  types:
TT_POHEADER type standard table of TS_POHEADER .
  types:
  begin of TS_PROCESSORDER,
     VERID type C length 4,
     VARIA type C length 18,
     UNPLN type C length 1,
     TPLNR_FL type C length 30,
     TPLMA type C length 40,
     TECOM type C length 1,
     SCHTA type C length 1,
     PRPRS type C length 1,
     PROID type C length 24,
     POSNR type C length 4,
     OSTAT type C length 1,
     ORDLV type C length 1,
     OBJNR type C length 22,
     MATNR type C length 40,
     MAKTX type C length 40,
     IWERK type C length 4,
     GWEMG type P length 7 decimals 3,
     GSTRP type TIMESTAMPL,
     GMEIN type C length 3,
     GLTRP type TIMESTAMPL,
     GAMNG type P length 7 decimals 3,
     ERZET type T,
     ERNAM type C length 12,
     ERDAT type TIMESTAMPL,
     DATBI type TIMESTAMPL,
     DATAB type TIMESTAMPL,
     CSTAT type C length 4,
     CPROS type C length 10,
     CONTR type C length 6,
     COMNG type P length 7 decimals 3,
     CMNUM type C length 10,
     AUTYP type C length 2,
     AUFNR_V type C length 12,
     AUFNR_TO type C length 12,
     AUFNR type C length 12,
     AUART type C length 4,
     AEZET type T,
     AENAM type C length 12,
     AEDAT type TIMESTAMPL,
  end of TS_PROCESSORDER .
  types:
TT_PROCESSORDER type standard table of TS_PROCESSORDER .
  types:
  begin of TS_TASKANALYTICS,
     GSTRP type TIMESTAMPL,
     GWEMG type P length 7 decimals 3,
     IWERK type C length 4,
     MADUR type P length 5 decimals 3,
     MATNR type C length 40,
     MAUNT type C length 3,
     MIDUR type P length 5 decimals 3,
     MIUNT type C length 3,
     OBJNR type C length 22,
     OBTXT type C length 40,
     ORDLV type C length 1,
     OSTAT type C length 1,
     PERNR type C length 8,
     POSNR_V type C length 4,
     PRODNET type C length 1,
     PROID type C length 24,
     RSNUM type C length 10,
     SAPPL type C length 2,
     SCHTA type C length 1,
     TECOM type C length 1,
     TPLMA type C length 40,
     TPLNR_FL type C length 40,
     VARIA type C length 18,
     VERID type C length 4,
     ZUKRI type C length 40,
     MAKTX type C length 40,
     AEDAT type TIMESTAMPL,
     AENAM type C length 12,
     AEZET type T,
     AUART type C length 4,
     AUFNR type C length 12,
     AUFNR_V type C length 12,
     AUFPL type C length 10,
     AUTYP type C length 2,
     CHARG type C length 10,
     CLASS type C length 1,
     CMNUM type C length 10,
     COMNG type P length 7 decimals 3,
     CONTR type C length 6,
     CPROS type C length 10,
     CSTAT type C length 4,
     DATAB type TIMESTAMPL,
     DATBI type TIMESTAMPL,
     EQUNR type C length 18,
     ERDAT type TIMESTAMPL,
     ERNAM type C length 12,
     ERZET type T,
     GAMNG type P length 7 decimals 3,
     GANGCD type C length 10,
     GLTRP type TIMESTAMPL,
     GMEIN type C length 3,
     GRCRE type C length 1,
  end of TS_TASKANALYTICS .
  types:
TT_TASKANALYTICS type standard table of TS_TASKANALYTICS .
  types:
  begin of TS_TASKORDER,
     IWERK type C length 4,
     VORNR type C length 4,
     VERID type C length 4,
     VENDOR type C length 10,
     VARIA type C length 18,
     UPDKZ type C length 1,
     UNPLN type C length 1,
     UMREN type F,
     TPLNR_FL type C length 30,
     TOMNG type P length 7 decimals 3,
     STEUS type C length 4,
     SCHDT type TIMESTAMPL,
     SCHDL type C length 1,
     SAPPL type C length 2,
     RUECK type C length 10,
     POSNR type C length 4,
     MEINH type C length 3,
     MATNR type C length 40,
     LTXA1 type C length 40,
     GWEMG type P length 7 decimals 3,
     GRCRE type C length 1,
     GAMNG type P length 7 decimals 3,
     EQUNR type C length 18,
     DATAB type TIMESTAMPL,
     CSTAT type C length 4,
     CPROS type C length 10,
     CONTR type C length 6,
     CONFM type C length 1,
     CMNUM type C length 10,
     BYPRD type C length 1,
     AUFNR_TO type C length 12,
     AUFNR type C length 12,
     ARBPL type C length 8,
     ACTION type C length 1,
     ACTDT type TIMESTAMPL,
  end of TS_TASKORDER .
  types:
TT_TASKORDER type standard table of TS_TASKORDER .
  types:
  begin of TS_TERRAIN,
     BUKRS type C length 4,
     IWERK type C length 4,
     TPLNR_FL type C length 40,
     PLTXT type C length 40,
     FLTYP type C length 1,
     TPLMA type C length 40,
     GAREA type P length 8 decimals 2,
     MSEHI type C length 3,
     OWNER type C length 10,
     OWNSHP type C length 2,
     OWNSHPDESC type C length 60,
     OWNNAME type C length 40,
     SWERK type C length 4,
  end of TS_TERRAIN .
  types:
TT_TERRAIN type standard table of TS_TERRAIN .
  types:
  begin of TS_TERRAINHDR,
     TPLNR_FL type C length 40,
     TPLMA type C length 40,
     BUKRS type C length 4,
     IWERK type C length 4,
     PLTXT type C length 40,
     FLTYP type C length 1,
     GAREA type P length 8 decimals 2,
     MSEHI type C length 3,
     OWNER type C length 10,
     OWNSHP type C length 2,
     OWNSHPDESC type C length 60,
     OWNNAME type C length 40,
     SWERK type C length 4,
  end of TS_TERRAINHDR .
  types:
TT_TERRAINHDR type standard table of TS_TERRAINHDR .
  types:
     TS_WOACTIVITIES type /AGRI/S_FMWOACT_FCAT .
  types:
TT_WOACTIVITIES type standard table of TS_WOACTIVITIES .
  types:
  begin of TS_WOCOMPONENTS,
     WONUM type C length 10,
     VORNR type C length 4,
     CONTR type C length 6,
     MATNR type C length 40,
     ESMNG type P length 7 decimals 3,
     ERFME type C length 3,
     BWART type C length 3,
     WERKS type C length 4,
     LGORT type C length 4,
     CHARG type C length 10,
     LMNGA type P length 7 decimals 3,
     UPDKZ type C length 1,
     MAKTX type C length 40,
     FLGCH type C length 1,
     LTXA1 type C length 40,
  end of TS_WOCOMPONENTS .
  types:
TT_WOCOMPONENTS type standard table of TS_WOCOMPONENTS .
  types:
  begin of TS_WOCONFIRMATION,
     WOTYP type C length 4,
     WONUM type C length 10,
     VERID type C length 4,
     UPDKZ type C length 1,
     TPLNR_FL type C length 40,
     TPLML type C length 40,
     TOMNG type P length 7 decimals 3,
     STRNO type C length 999,
     STORT type C length 10,
     POSNR type C length 4,
     OBJNR type C length 22,
     MSGLI type C length 200,
     MATNR type C length 40,
     MAKTX type C length 40,
     LMNGA type P length 7 decimals 3,
     LIFNR type C length 10,
     KUNNR type C length 10,
     KFRST type C length 1,
     IWERK type C length 4,
     GWEMG type P length 7 decimals 3,
     GMEIN type C length 3,
     GAMNG type P length 7 decimals 3,
     FCNFM_EXT type C length 1,
     FCNFM type C length 1,
     ERROR type C length 1,
     ERNAM type C length 12,
     ERDAT type TIMESTAMPL,
     EQUNR type C length 18,
     DATBI type TIMESTAMPL,
     DATAB type TIMESTAMPL,
     CSTAT type C length 4,
     CONTR type C length 6,
     CONFM type C length 1,
     CMNUM type C length 10,
     AUFNR_TO type C length 12,
     AUFNR type C length 12,
     AENAM type C length 12,
     AEDAT type TIMESTAMPL,
  end of TS_WOCONFIRMATION .
  types:
TT_WOCONFIRMATION type standard table of TS_WOCONFIRMATION .
  types:
  begin of TS_WOOPERATIONS,
     WONUM type C length 10,
     VORNR type C length 4,
     UPDKZ type C length 1,
     UMREN type P length 3 decimals 0,
     TPLNR_FL type C length 40,
     TEXT6 type C length 20,
     TEXT5 type C length 20,
     TEXT4 type C length 20,
     TEXT3 type C length 20,
     TEXT2 type C length 20,
     TEXT1 type C length 20,
     PERNR type C length 8,
     MEINH type C length 3,
     LTXA1 type C length 40,
     LMNGA type P length 7 decimals 3,
     LEINH6 type C length 3,
     LEINH5 type C length 3,
     LEINH4 type C length 3,
     LEINH3 type C length 3,
     LEINH2 type C length 3,
     LEINH1 type C length 3,
     ISM06 type P length 7 decimals 3,
     ISM05 type P length 7 decimals 3,
     ISM04 type P length 7 decimals 3,
     ISM03 type P length 7 decimals 3,
     ISM02 type P length 7 decimals 3,
     ISM01 type P length 7 decimals 3,
     ISDZ type T,
     ISDD type TIMESTAMPL,
     IEDZ type T,
     IEDD type TIMESTAMPL,
     GICRE type C length 1,
     CONFM type C length 1,
     BUDAT type TIMESTAMPL,
     ARBPL_EXT type C length 8,
     ARBPL type C length 8,
     ANZMS type P length 3 decimals 2,
     ABLAD type C length 25,
  end of TS_WOOPERATIONS .
  types:
TT_WOOPERATIONS type standard table of TS_WOOPERATIONS .
  types:
  begin of TS_WORKORDER,
     AEDAT type TIMESTAMPL,
     AENAM type C length 12,
     AUFNR type C length 12,
     CMNUM type C length 10,
     DATAB type TIMESTAMPL,
     DATBI type TIMESTAMPL,
     EQUNR type C length 18,
     ERDAT type TIMESTAMPL,
     ERNAM type C length 12,
     IWERK type C length 4,
     KFRST type C length 1,
     KUNNR type C length 10,
     LIFNR type C length 10,
     MATNR type C length 40,
     OBJNR type C length 22,
     STORT type C length 10,
     TPLML type C length 40,
     TPLNR_FL type C length 40,
     VERID type C length 4,
     WONUM type C length 10,
     WOTYP type C length 4,
  end of TS_WORKORDER .
  types:
TT_WORKORDER type standard table of TS_WORKORDER .
  types:
  begin of TS_WORKORDERCREATE,
     WOTYP type C length 4,
     WONUM type C length 10,
     VERID type C length 4,
     VARIA type C length 18,
     TPLNR_FL type C length 40,
     TPLML type C length 40,
     STORT type C length 10,
     OBJNR type C length 22,
     MATNR type C length 40,
     LIFNR type C length 10,
     KUNNR type C length 10,
     KFRST type C length 1,
     IWERK type C length 4,
     GSTRP type TIMESTAMPL,
     GLTRP type TIMESTAMPL,
     ERNAM type C length 12,
     ERDAT type TIMESTAMPL,
     EQUNR type C length 18,
     DATBI type TIMESTAMPL,
     DATAB type TIMESTAMPL,
     CPROS type C length 10,
     CMNUM type C length 10,
     AUFNR type C length 12,
     AENAM type C length 12,
     AEDAT type TIMESTAMPL,
  end of TS_WORKORDERCREATE .
  types:
TT_WORKORDERCREATE type standard table of TS_WORKORDERCREATE .
  types:
  begin of TS_CHEMICAL_DETAILS,
     ERFME type C length 3,
     LMNGA type P length 7 decimals 3,
     MAKTX type C length 40,
     MATNR type C length 40,
     TPLNR_FL type C length 40,
     AUFNR type C length 12,
  end of TS_CHEMICAL_DETAILS .
  types:
TT_CHEMICAL_DETAILS type standard table of TS_CHEMICAL_DETAILS .
  types:
  begin of TS_F4CROPPROCESS,
     TPLNR_FL type /AGRI/GLTPLNR_FL,
     CMNUM type /AGRI/GLCMNUM,
     VARIA type /AGRI/GLVARIA,
     CPROS type C length 10,
     PPROS type C length 10,
     DESCR type C length 40,
  end of TS_F4CROPPROCESS .
  types:
TT_F4CROPPROCESS type standard table of TS_F4CROPPROCESS .
  types:
     TS_F4CROPVARIANTS type /AGRI/GLCMVAR .
  types:
TT_F4CROPVARIANTS type standard table of TS_F4CROPVARIANTS .
  types:
  begin of TS_F4WORKORDERTYPES,
     SPRAS type C length 2,
     WOTYP type C length 4,
     DESCR type C length 40,
  end of TS_F4WORKORDERTYPES .
  types:
TT_F4WORKORDERTYPES type standard table of TS_F4WORKORDERTYPES .
  types:
     TS_CATMASTERDATA type ZABS_STR_CAT_MASTER_DATA .
  types:
TT_CATMASTERDATA type standard table of TS_CATMASTERDATA .
  types:
     TS_TASKORDERANALYTICS type ZABS_STR_TASK_ORDERS .
  types:
TT_TASKORDERANALYTICS type standard table of TS_TASKORDERANALYTICS .
  types:
     TS_CATMASTER1 type ZABS_STR_CAT_ORDERS_DATA .
  types:
TT_CATMASTER1 type standard table of TS_CATMASTER1 .

  constants GC_WORKORDERCREATE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'WorkOrderCreate' ##NO_TEXT.
  constants GC_WORKORDER type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'WorkOrder' ##NO_TEXT.
  constants GC_WOOPERATIONS type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'WoOperations' ##NO_TEXT.
  constants GC_WOCONFIRMATION type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'WoConfirmation' ##NO_TEXT.
  constants GC_WOCOMPONENTS type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'WoComponents' ##NO_TEXT.
  constants GC_WOACTIVITIES type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'WoActivities' ##NO_TEXT.
  constants GC_TERRAINHDR type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'TerrainHdr' ##NO_TEXT.
  constants GC_TERRAIN type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'Terrain' ##NO_TEXT.
  constants GC_TASKORDERANALYTICS type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'TaskOrderAnalytics' ##NO_TEXT.
  constants GC_TASKORDER type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'TaskOrder' ##NO_TEXT.
  constants GC_TASKANALYTICS type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'TaskAnalytics' ##NO_TEXT.
  constants GC_PROCESSORDER type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'ProcessOrder' ##NO_TEXT.
  constants GC_POHEADER type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PoHeader' ##NO_TEXT.
  constants GC_PODETAILS type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PoDetails' ##NO_TEXT.
  constants GC_POCONFIRMATIONS type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'PoConfirmations' ##NO_TEXT.
  constants GC_MEASUREMENTTYPES type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'MeasurementTypes' ##NO_TEXT.
  constants GC_MEASUREMENTGROUP type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'MeasurementGroup' ##NO_TEXT.
  constants GC_MEASUREMENTDOCUMENTHEADER type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'MeasurementDocumentHeader' ##NO_TEXT.
  constants GC_MEASUREMENTATTRIBUTES type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'MeasurementAttributes' ##NO_TEXT.
  constants GC_MDMCRF4 type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'MDMCrF4' ##NO_TEXT.
  constants GC_FIELDCATALOG type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'FieldCatalog' ##NO_TEXT.
  constants GC_F4WORKORDERTYPES type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'F4WorkOrderTypes' ##NO_TEXT.
  constants GC_F4CROPVARIANTS type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'F4CropVariants' ##NO_TEXT.
  constants GC_F4CROPPROCESS type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'F4CropProcess' ##NO_TEXT.
  constants GC_CSMDMMASSDATA type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'CsMDMMassData' ##NO_TEXT.
  constants GC_CSMDMITEM type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'CsMDMItem' ##NO_TEXT.
  constants GC_CROPSEASON type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'CropSeason' ##NO_TEXT.
  constants GC_CHEMICAL_DETAILS type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'chemical_Details' ##NO_TEXT.
  constants GC_CATMASTERDATA type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'CATMasterData' ##NO_TEXT.
  constants GC_CATMASTER1 type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'CatMaster1' ##NO_TEXT.
  constants GC_CATANALYTICS type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'CatAnalytics' ##NO_TEXT.

  methods GET_EXTENDED_MODEL
  final
    exporting
      !EV_EXTENDED_SERVICE type /IWBEP/MED_GRP_TECHNICAL_NAME
      !EV_EXT_SERVICE_VERSION type /IWBEP/MED_GRP_VERSION
      !EV_EXTENDED_MODEL type /IWBEP/MED_MDL_TECHNICAL_NAME
      !EV_EXT_MODEL_VERSION type /IWBEP/MED_MDL_VERSION
    raising
      /IWBEP/CX_MGW_MED_EXCEPTION .
  methods LOAD_TEXT_ELEMENTS
  final
    returning
      value(RT_TEXT_ELEMENTS) type TT_TEXT_ELEMENTS
    raising
      /IWBEP/CX_MGW_MED_EXCEPTION .

  methods DEFINE
    redefinition .
  methods GET_LAST_MODIFIED
    redefinition .
protected section.
private section.

  constants GC_INCL_NAME type STRING value 'ZCL_ZABS_GLTR_GIS_EXT_MPC=====CP' ##NO_TEXT.

  methods CREATE_NEW_ARTIFACTS
    raising
      /IWBEP/CX_MGW_MED_EXCEPTION .
ENDCLASS.



CLASS ZCL_ZABS_GLTR_GIS_EXT_MPC IMPLEMENTATION.


  method CREATE_NEW_ARTIFACTS.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS          &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL   &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                    &*
*&                                                                     &*
*&---------------------------------------------------------------------*


DATA:
  lo_entity_type    TYPE REF TO /iwbep/if_mgw_odata_entity_typ,                      "#EC NEEDED
  lo_complex_type   TYPE REF TO /iwbep/if_mgw_odata_cmplx_type,                      "#EC NEEDED
  lo_property       TYPE REF TO /iwbep/if_mgw_odata_property,                        "#EC NEEDED
  lo_association    TYPE REF TO /iwbep/if_mgw_odata_assoc,                           "#EC NEEDED
  lo_assoc_set      TYPE REF TO /iwbep/if_mgw_odata_assoc_set,                       "#EC NEEDED
  lo_ref_constraint TYPE REF TO /iwbep/if_mgw_odata_ref_constr,                      "#EC NEEDED
  lo_nav_property   TYPE REF TO /iwbep/if_mgw_odata_nav_prop,                        "#EC NEEDED
  lo_action         TYPE REF TO /iwbep/if_mgw_odata_action,                          "#EC NEEDED
  lo_parameter      TYPE REF TO /iwbep/if_mgw_odata_property,                        "#EC NEEDED
  lo_entity_set     TYPE REF TO /iwbep/if_mgw_odata_entity_set.                      "#EC NEEDED


***********************************************************************************************************************************
*   ENTITY - MeasurementAttributes
***********************************************************************************************************************************
lo_entity_type = model->create_entity_type( iv_entity_type_name = 'MeasurementAttributes' iv_def_entity_set = abap_false ). "#EC NOTEXT

***********************************************************************************************************************************
*Properties
***********************************************************************************************************************************

lo_property = lo_entity_type->create_property( iv_property_name = 'Mdocm' iv_abap_fieldname = 'MDOCM' ). "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 20 ).
lo_property->set_conversion_exit( 'ALPHA' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_true ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Atinn' iv_abap_fieldname = 'ATINN' ). "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 30 ).
lo_property->set_conversion_exit( 'ATINN' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_true ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Atzhl' iv_abap_fieldname = 'ATZHL' ). "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 4 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_true ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Atwrt' iv_abap_fieldname = 'ATWRT' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 70 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Atflv' iv_abap_fieldname = 'ATFLV' ). "#EC NOTEXT
lo_property->set_type_edm_double( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Atawe' iv_abap_fieldname = 'ATAWE' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 3 ).
lo_property->set_conversion_exit( 'CUNIT' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Atflb' iv_abap_fieldname = 'ATFLB' ). "#EC NOTEXT
lo_property->set_type_edm_double( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Ataw1' iv_abap_fieldname = 'ATAW1' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 3 ).
lo_property->set_conversion_exit( 'CUNIT' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Atcod' iv_abap_fieldname = 'ATCOD' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 1 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Attlv' iv_abap_fieldname = 'ATTLV' ). "#EC NOTEXT
lo_property->set_type_edm_double( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Attlb' iv_abap_fieldname = 'ATTLB' ). "#EC NOTEXT
lo_property->set_type_edm_double( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Atprz' iv_abap_fieldname = 'ATPRZ' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 1 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Atinc' iv_abap_fieldname = 'ATINC' ). "#EC NOTEXT
lo_property->set_type_edm_double( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Ataut' iv_abap_fieldname = 'ATAUT' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 1 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Datuv' iv_abap_fieldname = 'DATUV' ). "#EC NOTEXT
lo_property->set_type_edm_datetime( ).
lo_property->set_precison( iv_precision = 7 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_true ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Atimb' iv_abap_fieldname = 'ATIMB' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 30 ).
lo_property->set_conversion_exit( 'ATINN' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Atzis' iv_abap_fieldname = 'ATZIS' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 3 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Atsrt' iv_abap_fieldname = 'ATSRT' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 4 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Atvglart' iv_abap_fieldname = 'ATVGLART' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 1 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Error' iv_abap_fieldname = 'ERROR' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 1 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Ernam' iv_abap_fieldname = 'ERNAM' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 12 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Erdat' iv_abap_fieldname = 'ERDAT' ). "#EC NOTEXT
lo_property->set_type_edm_datetime( ).
lo_property->set_precison( iv_precision = 7 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_true ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Erzet' iv_abap_fieldname = 'ERZET' ). "#EC NOTEXT
lo_property->set_type_edm_time( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Aenam' iv_abap_fieldname = 'AENAM' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 12 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Aedat' iv_abap_fieldname = 'AEDAT' ). "#EC NOTEXT
lo_property->set_type_edm_datetime( ).
lo_property->set_precison( iv_precision = 7 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_true ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Aezet' iv_abap_fieldname = 'AEZET' ). "#EC NOTEXT
lo_property->set_type_edm_time( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Atbez' iv_abap_fieldname = 'ATBEZ' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 30 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Atwtb' iv_abap_fieldname = 'ATWTB' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 70 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Updkz' iv_abap_fieldname = 'UPDKZ' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '998' iv_text_element_container = gc_incl_name ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 1 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Atgrp' iv_abap_fieldname = 'ATGRP' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 40 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Atkla' iv_abap_fieldname = 'ATKLA' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 10 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Atnam' iv_abap_fieldname = 'ATNAM' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 30 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Atvtx' iv_abap_fieldname = 'ATVTX' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 4 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Atson' iv_abap_fieldname = 'ATSON' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 1 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Atsch' iv_abap_fieldname = 'ATSCH' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '999' iv_text_element_container = gc_incl_name ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 70 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Atfor' iv_abap_fieldname = 'ATFOR' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 4 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Atinp' iv_abap_fieldname = 'ATINP' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '001' iv_text_element_container = gc_incl_name ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 1 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Attab' iv_abap_fieldname = 'ATTAB' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 30 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Atfel' iv_abap_fieldname = 'ATFEL' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 30 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Attei' iv_abap_fieldname = 'ATTEI' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 30 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Atprt' iv_abap_fieldname = 'ATPRT' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 30 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Atprr' iv_abap_fieldname = 'ATPRR' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 40 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Atprf' iv_abap_fieldname = 'ATPRF' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 30 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Atwrd' iv_abap_fieldname = 'ATWRD' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 1 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Atfod' iv_abap_fieldname = 'ATFOD' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 1 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Ludat' iv_abap_fieldname = 'LUDAT' ). "#EC NOTEXT
lo_property->set_type_edm_datetime( ).
lo_property->set_precison( iv_precision = 7 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_true ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'AtwtbOld' iv_abap_fieldname = 'ATWTB_OLD' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 70 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'AtwrtOld' iv_abap_fieldname = 'ATWRT_OLD' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 70 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'DatuvOld' iv_abap_fieldname = 'DATUV_OLD' ). "#EC NOTEXT
lo_property->set_type_edm_datetime( ).
lo_property->set_precison( iv_precision = 7 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_true ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Mdtyp' iv_abap_fieldname = 'MDTYP' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 4 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_true ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Aslvl' iv_abap_fieldname = 'ASLVL' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 1 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Mpgrp' iv_abap_fieldname = 'MPGRP' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 18 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Data' iv_abap_fieldname = 'DATA' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '002' iv_text_element_container = gc_incl_name ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 1 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).

lo_entity_type->bind_structure( iv_structure_name   = 'ZABS_STR_GLMDATV_ODS'
                                iv_bind_conversions = 'X' ). "#EC NOTEXT


lo_entity_type = model->create_entity_type( iv_entity_type_name = 'MeasurementDocumentHeader' iv_def_entity_set = abap_false ). "#EC NOTEXT

***********************************************************************************************************************************
*Properties
***********************************************************************************************************************************

lo_property = lo_entity_type->create_property( iv_property_name = 'Mdocm' iv_abap_fieldname = 'MDOCM' ). "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 20 ).
lo_property->set_conversion_exit( 'ALPHA' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_true ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Mdtyp' iv_abap_fieldname = 'MDTYP' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 4 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_true ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Aslvl' iv_abap_fieldname = 'ASLVL' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 1 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'TplnrFl' iv_abap_fieldname = 'TPLNR_FL' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 40 ).
lo_property->set_conversion_exit( 'ABSFL' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Contr' iv_abap_fieldname = 'CONTR' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 6 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Cmnum' iv_abap_fieldname = 'CMNUM' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 10 ).
lo_property->set_conversion_exit( 'ALPHA' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Equnr' iv_abap_fieldname = 'EQUNR' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 18 ).
lo_property->set_conversion_exit( 'ALPHA' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Mpgrp' iv_abap_fieldname = 'MPGRP' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 18 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Datab' iv_abap_fieldname = 'DATAB' ). "#EC NOTEXT
lo_property->set_type_edm_datetime( ).
lo_property->set_precison( iv_precision = 7 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_true ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Datbi' iv_abap_fieldname = 'DATBI' ). "#EC NOTEXT
lo_property->set_type_edm_datetime( ).
lo_property->set_precison( iv_precision = 7 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_true ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Muser' iv_abap_fieldname = 'MUSER' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 12 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Mdate' iv_abap_fieldname = 'MDATE' ). "#EC NOTEXT
lo_property->set_type_edm_datetime( ).
lo_property->set_precison( iv_precision = 7 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_true ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Mtime' iv_abap_fieldname = 'MTIME' ). "#EC NOTEXT
lo_property->set_type_edm_time( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Txtgr' iv_abap_fieldname = 'TXTGR' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 2 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Ivtyp' iv_abap_fieldname = 'IVTYP' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 4 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Kfrst' iv_abap_fieldname = 'KFRST' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 1 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Stsma' iv_abap_fieldname = 'STSMA' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 8 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Objnr' iv_abap_fieldname = 'OBJNR' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 22 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Ustat' iv_abap_fieldname = 'USTAT' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 5 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Ernam' iv_abap_fieldname = 'ERNAM' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 12 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Erdat' iv_abap_fieldname = 'ERDAT' ). "#EC NOTEXT
lo_property->set_type_edm_datetime( ).
lo_property->set_precison( iv_precision = 7 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_true ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Erzet' iv_abap_fieldname = 'ERZET' ). "#EC NOTEXT
lo_property->set_type_edm_time( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Aenam' iv_abap_fieldname = 'AENAM' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 12 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Aedat' iv_abap_fieldname = 'AEDAT' ). "#EC NOTEXT
lo_property->set_type_edm_datetime( ).
lo_property->set_precison( iv_precision = 7 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_true ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Aezet' iv_abap_fieldname = 'AEZET' ). "#EC NOTEXT
lo_property->set_type_edm_time( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Canceled' iv_abap_fieldname = 'CANCELED' ). "#EC NOTEXT
lo_property->set_type_edm_boolean( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Updkz' iv_abap_fieldname = 'UPDKZ' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '003' iv_text_element_container = gc_incl_name ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 1 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Descr' iv_abap_fieldname = 'DESCR' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 30 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'AslvlTxt' iv_abap_fieldname = 'ASLVL_TXT' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 60 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).

lo_entity_type->bind_structure( iv_structure_name   = 'ZABS_STR_GLMDHDR_ODS'
                                iv_bind_conversions = 'X' ). "#EC NOTEXT


lo_entity_type = model->create_entity_type( iv_entity_type_name = 'MeasurementGroup' iv_def_entity_set = abap_false ). "#EC NOTEXT

***********************************************************************************************************************************
*Properties
***********************************************************************************************************************************

lo_property = lo_entity_type->create_property( iv_property_name = 'Mpgrp' iv_abap_fieldname = 'CLASS' ). "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 18 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_true ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Klart' iv_abap_fieldname = 'KLART' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 3 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Agcat' iv_abap_fieldname = 'AGCAT' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 2 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Aslvl' iv_abap_fieldname = 'ASLVL' ). "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 1 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_true ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Datuv' iv_abap_fieldname = 'DATUV' ). "#EC NOTEXT
lo_property->set_type_edm_datetime( ).
lo_property->set_precison( iv_precision = 7 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_true ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Mdtyp' iv_abap_fieldname = 'MDTYP' ). "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 4 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_true ).

lo_entity_type->bind_structure( iv_structure_name   = '/AGRI/GLAGHA'
                                iv_bind_conversions = 'X' ). "#EC NOTEXT


lo_entity_type = model->create_entity_type( iv_entity_type_name = 'MeasurementTypes' iv_def_entity_set = abap_false ). "#EC NOTEXT

***********************************************************************************************************************************
*Properties
***********************************************************************************************************************************

lo_property = lo_entity_type->create_property( iv_property_name = 'Mdtyp' iv_abap_fieldname = 'MDTYP' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '004' iv_text_element_container = gc_incl_name ). "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 4 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_true ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Numki' iv_abap_fieldname = 'NUMKI' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '005' iv_text_element_container = gc_incl_name ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 2 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Aslvl' iv_abap_fieldname = 'ASLVL' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '006' iv_text_element_container = gc_incl_name ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 1 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_true ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Descr' iv_abap_fieldname = 'DESCR' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '007' iv_text_element_container = gc_incl_name ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 30 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'AslvlTxt' iv_abap_fieldname = 'DDTEXT' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '008' iv_text_element_container = gc_incl_name ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 60 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).

lo_entity_type->bind_structure( iv_structure_name  = 'ZCL_ZABS_GLTR_GIS_EXT_MPC=>TS_MEASUREMENTTYPES' ). "#EC NOTEXT


lo_entity_type = model->create_entity_type( iv_entity_type_name = 'F4CropProcess' iv_def_entity_set = abap_false ). "#EC NOTEXT

***********************************************************************************************************************************
*Properties
***********************************************************************************************************************************

lo_property = lo_entity_type->create_property( iv_property_name = 'TplnrFl' iv_abap_fieldname = 'TPLNR_FL' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '661' iv_text_element_container = gc_incl_name ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 40 ).
lo_property->set_conversion_exit( 'ABSFL' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Cmnum' iv_abap_fieldname = 'CMNUM' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '662' iv_text_element_container = gc_incl_name ). "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 10 ).
lo_property->set_conversion_exit( 'ALPHA' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Varia' iv_abap_fieldname = 'VARIA' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '663' iv_text_element_container = gc_incl_name ). "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 18 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Cpros' iv_abap_fieldname = 'CPROS' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '673' iv_text_element_container = gc_incl_name ). "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 10 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Ppros' iv_abap_fieldname = 'PPROS' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '674' iv_text_element_container = gc_incl_name ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 10 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Descr' iv_abap_fieldname = 'DESCR' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '675' iv_text_element_container = gc_incl_name ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 40 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).

lo_entity_type->bind_structure( iv_structure_name  = 'ZCL_ZABS_GLTR_GIS_EXT_MPC=>TS_F4CROPPROCESS' ). "#EC NOTEXT


lo_entity_type = model->create_entity_type( iv_entity_type_name = 'F4CropVariants' iv_def_entity_set = abap_false ). "#EC NOTEXT

***********************************************************************************************************************************
*Properties
***********************************************************************************************************************************

lo_property = lo_entity_type->create_property( iv_property_name = 'Cmnum' iv_abap_fieldname = 'CMNUM' ). "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 10 ).
lo_property->set_conversion_exit( 'ALPHA' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Varia' iv_abap_fieldname = 'VARIA' ). "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 18 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Descr' iv_abap_fieldname = 'DESCR' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 40 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Class' iv_abap_fieldname = 'CLASS' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 1 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Pctyp' iv_abap_fieldname = 'PCTYP' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 10 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Mcycl' iv_abap_fieldname = 'MCYCL' ). "#EC NOTEXT
lo_property->set_type_edm_boolean( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).

lo_entity_type->bind_structure( iv_structure_name   = '/AGRI/GLCMVAR'
                                iv_bind_conversions = 'X' ). "#EC NOTEXT


lo_entity_type = model->create_entity_type( iv_entity_type_name = 'F4WorkOrderTypes' iv_def_entity_set = abap_false ). "#EC NOTEXT

***********************************************************************************************************************************
*Properties
***********************************************************************************************************************************

lo_property = lo_entity_type->create_property( iv_property_name = 'Spras' iv_abap_fieldname = 'SPRAS' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '676' iv_text_element_container = gc_incl_name ). "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 2 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_true ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Wotyp' iv_abap_fieldname = 'WOTYP' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '677' iv_text_element_container = gc_incl_name ). "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 4 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_true ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Descr' iv_abap_fieldname = 'DESCR' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '678' iv_text_element_container = gc_incl_name ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 40 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).

lo_entity_type->bind_structure( iv_structure_name  = 'ZCL_ZABS_GLTR_GIS_EXT_MPC=>TS_F4WORKORDERTYPES' ). "#EC NOTEXT


lo_entity_type = model->create_entity_type( iv_entity_type_name = 'CATMasterData' iv_def_entity_set = abap_false ). "#EC NOTEXT

***********************************************************************************************************************************
*Properties
***********************************************************************************************************************************

lo_property = lo_entity_type->create_property( iv_property_name = 'Matnr' iv_abap_fieldname = 'MATNR' ). "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 40 ).
lo_property->set_conversion_exit( 'MATN1' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_true ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Stort' iv_abap_fieldname = 'STORT' ). "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 10 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_true ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Cattyp' iv_abap_fieldname = 'CATTYP' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 4 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Manufacturer' iv_abap_fieldname = 'MANUFACTURER' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 10 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Url' iv_abap_fieldname = 'URL' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 255 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'EpaReg' iv_abap_fieldname = 'EPA_REG' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 20 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Vendor' iv_abap_fieldname = 'VENDOR' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 10 ).
lo_property->set_conversion_exit( 'ALPHA' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'NonLiq' iv_abap_fieldname = 'NON_LIQ' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 1 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'RestUsePest' iv_abap_fieldname = 'REST_USE_PEST' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 1 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'OralNot' iv_abap_fieldname = 'ORAL_NOT' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 1 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'OrganoAux' iv_abap_fieldname = 'ORGANO_AUX' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 1 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'ToxicityClass' iv_abap_fieldname = 'TOXICITY_CLASS' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 3 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'PackDesc' iv_abap_fieldname = 'PACK_DESC' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 20 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'PackUnit' iv_abap_fieldname = 'PACK_UNIT' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 3 ).
lo_property->set_conversion_exit( 'CUNIT' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Rei' iv_abap_fieldname = 'REI' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 6 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Cauom' iv_abap_fieldname = 'CAUOM' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 3 ).
lo_property->set_conversion_exit( 'CUNIT' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Pesttype' iv_abap_fieldname = 'PESTTYPE' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 3 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'MatnrRef' iv_abap_fieldname = 'MATNR_REF' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 50 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Txtgr' iv_abap_fieldname = 'TXTGR' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 2 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Objnr' iv_abap_fieldname = 'OBJNR' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 22 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Status' iv_abap_fieldname = 'STATUS' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 1 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Ernam' iv_abap_fieldname = 'ERNAM' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 12 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Erdat' iv_abap_fieldname = 'ERDAT' ). "#EC NOTEXT
lo_property->set_type_edm_datetime( ).
lo_property->set_precison( iv_precision = 7 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_true ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Erzet' iv_abap_fieldname = 'ERZET' ). "#EC NOTEXT
lo_property->set_type_edm_time( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Aenam' iv_abap_fieldname = 'AENAM' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 12 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Aedat' iv_abap_fieldname = 'AEDAT' ). "#EC NOTEXT
lo_property->set_type_edm_datetime( ).
lo_property->set_precison( iv_precision = 7 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_true ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Aezet' iv_abap_fieldname = 'AEZET' ). "#EC NOTEXT
lo_property->set_type_edm_time( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Maktx' iv_abap_fieldname = 'MAKTX' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '679' iv_text_element_container = gc_incl_name ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 40 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Descr' iv_abap_fieldname = 'DESCR' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '680' iv_text_element_container = gc_incl_name ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 40 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).

lo_entity_type->bind_structure( iv_structure_name   = 'ZABS_STR_CAT_MASTER_DATA'
                                iv_bind_conversions = 'X' ). "#EC NOTEXT


lo_entity_type = model->create_entity_type( iv_entity_type_name = 'TaskOrderAnalytics' iv_def_entity_set = abap_false ). "#EC NOTEXT

***********************************************************************************************************************************
*Properties
***********************************************************************************************************************************

lo_property = lo_entity_type->create_property( iv_property_name = 'Aufnr' iv_abap_fieldname = 'AUFNR' ). "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 12 ).
lo_property->set_conversion_exit( 'ALPHA' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Auart' iv_abap_fieldname = 'AUART' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 4 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Autyp' iv_abap_fieldname = 'AUTYP' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 2 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'TplnrFl' iv_abap_fieldname = 'TPLNR_FL' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 40 ).
lo_property->set_conversion_exit( 'ABSFL' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Contr' iv_abap_fieldname = 'CONTR' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 6 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Tplma' iv_abap_fieldname = 'TPLMA' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 40 ).
lo_property->set_conversion_exit( 'ABSFL' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Cmnum' iv_abap_fieldname = 'CMNUM' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 10 ).
lo_property->set_conversion_exit( 'ALPHA' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Varia' iv_abap_fieldname = 'VARIA' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 18 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Cpros' iv_abap_fieldname = 'CPROS' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 10 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Class' iv_abap_fieldname = 'CLASS' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 1 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Sappl' iv_abap_fieldname = 'SAPPL' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 2 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Ostat' iv_abap_fieldname = 'OSTAT' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 1 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Datab' iv_abap_fieldname = 'DATAB' ). "#EC NOTEXT
lo_property->set_type_edm_datetime( ).
lo_property->set_precison( iv_precision = 7 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_true ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Datbi' iv_abap_fieldname = 'DATBI' ). "#EC NOTEXT
lo_property->set_type_edm_datetime( ).
lo_property->set_precison( iv_precision = 7 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_true ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Zukri' iv_abap_fieldname = 'ZUKRI' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 40 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Matnr' iv_abap_fieldname = 'MATNR' ). "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 40 ).
lo_property->set_conversion_exit( 'MATN1' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Cstat' iv_abap_fieldname = 'CSTAT' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 4 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Gamng' iv_abap_fieldname = 'GAMNG' ). "#EC NOTEXT
lo_property->set_type_edm_decimal( ).
lo_property->set_precison( iv_precision = 3 ).
lo_property->set_maxlength( iv_max_length = 13 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Gwemg' iv_abap_fieldname = 'GWEMG' ). "#EC NOTEXT
lo_property->set_type_edm_decimal( ).
lo_property->set_precison( iv_precision = 3 ).
lo_property->set_maxlength( iv_max_length = 13 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Comng' iv_abap_fieldname = 'COMNG' ). "#EC NOTEXT
lo_property->set_type_edm_decimal( ).
lo_property->set_precison( iv_precision = 3 ).
lo_property->set_maxlength( iv_max_length = 13 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Gmein' iv_abap_fieldname = 'GMEIN' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 3 ).
lo_property->set_conversion_exit( 'CUNIT' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Gstrp' iv_abap_fieldname = 'GSTRP' ). "#EC NOTEXT
lo_property->set_type_edm_datetime( ).
lo_property->set_precison( iv_precision = 7 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_true ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Gltrp' iv_abap_fieldname = 'GLTRP' ). "#EC NOTEXT
lo_property->set_type_edm_datetime( ).
lo_property->set_precison( iv_precision = 7 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_true ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Objnr' iv_abap_fieldname = 'OBJNR' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 22 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Verid' iv_abap_fieldname = 'VERID' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 4 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Charg' iv_abap_fieldname = 'CHARG' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 10 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Pernr' iv_abap_fieldname = 'PERNR' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 8 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Obtxt' iv_abap_fieldname = 'OBTXT' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 40 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Proid' iv_abap_fieldname = 'PROID' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 24 ).
lo_property->set_conversion_exit( 'ABPSP' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Iwerk' iv_abap_fieldname = 'IWERK' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 4 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Midur' iv_abap_fieldname = 'MIDUR' ). "#EC NOTEXT
lo_property->set_type_edm_decimal( ).
lo_property->set_precison( iv_precision = 3 ).
lo_property->set_maxlength( iv_max_length = 9 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Miunt' iv_abap_fieldname = 'MIUNT' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 3 ).
lo_property->set_conversion_exit( 'CUNIT' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Madur' iv_abap_fieldname = 'MADUR' ). "#EC NOTEXT
lo_property->set_type_edm_decimal( ).
lo_property->set_precison( iv_precision = 3 ).
lo_property->set_maxlength( iv_max_length = 9 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Maunt' iv_abap_fieldname = 'MAUNT' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 3 ).
lo_property->set_conversion_exit( 'CUNIT' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Ordlv' iv_abap_fieldname = 'ORDLV' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 1 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Grcre' iv_abap_fieldname = 'GRCRE' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 1 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Schta' iv_abap_fieldname = 'SCHTA' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 1 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'AufnrV' iv_abap_fieldname = 'AUFNR_V' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 12 ).
lo_property->set_conversion_exit( 'ALPHA' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'PosnrV' iv_abap_fieldname = 'POSNR_V' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 4 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Aufpl' iv_abap_fieldname = 'AUFPL' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 10 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Rsnum' iv_abap_fieldname = 'RSNUM' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 10 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Equnr' iv_abap_fieldname = 'EQUNR' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 18 ).
lo_property->set_conversion_exit( 'ALPHA' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Prodnet' iv_abap_fieldname = 'PRODNET' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 1 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Gangcd' iv_abap_fieldname = 'GANGCD' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 10 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Dummy' iv_abap_fieldname = 'DUMMY' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '681' iv_text_element_container = gc_incl_name ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 1 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Tecom' iv_abap_fieldname = 'TECOM' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 1 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Ernam' iv_abap_fieldname = 'ERNAM' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 12 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Erdat' iv_abap_fieldname = 'ERDAT' ). "#EC NOTEXT
lo_property->set_type_edm_datetime( ).
lo_property->set_precison( iv_precision = 7 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_true ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Erzet' iv_abap_fieldname = 'ERZET' ). "#EC NOTEXT
lo_property->set_type_edm_time( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Aenam' iv_abap_fieldname = 'AENAM' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 12 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Aedat' iv_abap_fieldname = 'AEDAT' ). "#EC NOTEXT
lo_property->set_type_edm_datetime( ).
lo_property->set_precison( iv_precision = 7 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_true ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Aezet' iv_abap_fieldname = 'AEZET' ). "#EC NOTEXT
lo_property->set_type_edm_time( ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Updkz' iv_abap_fieldname = 'UPDKZ' ). "#EC NOTEXT
lo_property->set_label_from_text_element( iv_text_element_symbol = '682' iv_text_element_container = gc_incl_name ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 1 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Sicon' iv_abap_fieldname = 'SICON' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 8 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Actdt' iv_abap_fieldname = 'ACTDT' ). "#EC NOTEXT
lo_property->set_type_edm_datetime( ).
lo_property->set_precison( iv_precision = 7 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_true ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Schdt' iv_abap_fieldname = 'SCHDT' ). "#EC NOTEXT
lo_property->set_type_edm_datetime( ).
lo_property->set_precison( iv_precision = 7 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_true ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Unpln' iv_abap_fieldname = 'UNPLN' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 1 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Maktx' iv_abap_fieldname = 'MAKTX' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 40 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Schdl' iv_abap_fieldname = 'SCHDL' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 1 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Sttxt' iv_abap_fieldname = 'STTXT' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 40 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Cstxt' iv_abap_fieldname = 'CSTXT' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 60 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).

lo_entity_type->bind_structure( iv_structure_name   = 'ZABS_STR_TASK_ORDERS'
                                iv_bind_conversions = 'X' ). "#EC NOTEXT


lo_entity_type = model->create_entity_type( iv_entity_type_name = 'CatMaster1' iv_def_entity_set = abap_false ). "#EC NOTEXT

***********************************************************************************************************************************
*Properties
***********************************************************************************************************************************

lo_property = lo_entity_type->create_property( iv_property_name = 'Aufnr' iv_abap_fieldname = 'AUFNR' ). "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 12 ).
lo_property->set_conversion_exit( 'ALPHA' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Auart' iv_abap_fieldname = 'AUART' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 4 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Autyp' iv_abap_fieldname = 'AUTYP' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 2 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'TplnrFl' iv_abap_fieldname = 'TPLNR_FL' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 40 ).
lo_property->set_conversion_exit( 'ABSFL' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Contr' iv_abap_fieldname = 'CONTR' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 6 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Cmnum' iv_abap_fieldname = 'CMNUM' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 10 ).
lo_property->set_conversion_exit( 'ALPHA' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Werks' iv_abap_fieldname = 'WERKS' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 4 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Datum' iv_abap_fieldname = 'DATUM' ). "#EC NOTEXT
lo_property->set_type_edm_datetime( ).
lo_property->set_precison( iv_precision = 7 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_true ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Gmein' iv_abap_fieldname = 'GMEIN' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 3 ).
lo_property->set_conversion_exit( 'CUNIT' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Lmnga' iv_abap_fieldname = 'LMNGA' ). "#EC NOTEXT
lo_property->set_type_edm_decimal( ).
lo_property->set_precison( iv_precision = 3 ).
lo_property->set_maxlength( iv_max_length = 13 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Matnr' iv_abap_fieldname = 'MATNR' ). "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 40 ).
lo_property->set_conversion_exit( 'MATN1' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Maktx' iv_abap_fieldname = 'MAKTX' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 40 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Tmatnr' iv_abap_fieldname = 'TMATNR' ). "#EC NOTEXT
lo_property->set_is_key( ).
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 40 ).
lo_property->set_conversion_exit( 'MATN1' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Matxt' iv_abap_fieldname = 'MATXT' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 40 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Meins' iv_abap_fieldname = 'MEINS' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 3 ).
lo_property->set_conversion_exit( 'CUNIT' ). "#EC NOTEXT
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Menge' iv_abap_fieldname = 'MENGE' ). "#EC NOTEXT
lo_property->set_type_edm_decimal( ).
lo_property->set_precison( iv_precision = 3 ).
lo_property->set_maxlength( iv_max_length = 13 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Posnr' iv_abap_fieldname = 'POSNR' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 4 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Sappl' iv_abap_fieldname = 'SAPPL' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 2 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).
lo_property = lo_entity_type->create_property( iv_property_name = 'Sttxt' iv_abap_fieldname = 'STTXT' ). "#EC NOTEXT
lo_property->set_type_edm_string( ).
lo_property->set_maxlength( iv_max_length = 40 ).
lo_property->set_creatable( abap_false ).
lo_property->set_updatable( abap_false ).
lo_property->set_sortable( abap_false ).
lo_property->set_nullable( abap_false ).
lo_property->set_filterable( abap_false ).

lo_entity_type->bind_structure( iv_structure_name   = 'ZABS_STR_CAT_ORDERS_DATA'
                                iv_bind_conversions = 'X' ). "#EC NOTEXT


***********************************************************************************************************************************
*   ENTITY SETS
***********************************************************************************************************************************
lo_entity_type = model->get_entity_type( iv_entity_name = 'MeasurementAttributes' ). "#EC NOTEXT
lo_entity_set = lo_entity_type->create_entity_set( 'MeasurementAttributesSet' ). "#EC NOTEXT

lo_entity_set->set_creatable( abap_false ).
lo_entity_set->set_updatable( abap_false ).
lo_entity_set->set_deletable( abap_false ).

lo_entity_set->set_pageable( abap_false ).
lo_entity_set->set_addressable( abap_true ).
lo_entity_set->set_has_ftxt_search( abap_false ).
lo_entity_set->set_subscribable( abap_false ).
lo_entity_set->set_filter_required( abap_false ).
lo_entity_type = model->get_entity_type( iv_entity_name = 'MeasurementDocumentHeader' ). "#EC NOTEXT
lo_entity_set = lo_entity_type->create_entity_set( 'MeasurementDocumentHeaderSet' ). "#EC NOTEXT

lo_entity_set->set_creatable( abap_false ).
lo_entity_set->set_updatable( abap_false ).
lo_entity_set->set_deletable( abap_false ).

lo_entity_set->set_pageable( abap_false ).
lo_entity_set->set_addressable( abap_true ).
lo_entity_set->set_has_ftxt_search( abap_false ).
lo_entity_set->set_subscribable( abap_false ).
lo_entity_set->set_filter_required( abap_false ).
lo_entity_type = model->get_entity_type( iv_entity_name = 'MeasurementGroup' ). "#EC NOTEXT
lo_entity_set = lo_entity_type->create_entity_set( 'MeasurementGroupSet' ). "#EC NOTEXT

lo_entity_set->set_creatable( abap_false ).
lo_entity_set->set_updatable( abap_false ).
lo_entity_set->set_deletable( abap_false ).

lo_entity_set->set_pageable( abap_false ).
lo_entity_set->set_addressable( abap_true ).
lo_entity_set->set_has_ftxt_search( abap_false ).
lo_entity_set->set_subscribable( abap_false ).
lo_entity_set->set_filter_required( abap_false ).
lo_entity_type = model->get_entity_type( iv_entity_name = 'MeasurementTypes' ). "#EC NOTEXT
lo_entity_set = lo_entity_type->create_entity_set( 'MeasurementTypesSet' ). "#EC NOTEXT

lo_entity_set->set_creatable( abap_false ).
lo_entity_set->set_updatable( abap_false ).
lo_entity_set->set_deletable( abap_false ).

lo_entity_set->set_pageable( abap_false ).
lo_entity_set->set_addressable( abap_false ).
lo_entity_set->set_has_ftxt_search( abap_false ).
lo_entity_set->set_subscribable( abap_false ).
lo_entity_set->set_filter_required( abap_false ).
lo_entity_type = model->get_entity_type( iv_entity_name = 'F4CropProcess' ). "#EC NOTEXT
lo_entity_set = lo_entity_type->create_entity_set( 'F4CropProcessSet' ). "#EC NOTEXT

lo_entity_set->set_creatable( abap_false ).
lo_entity_set->set_updatable( abap_false ).
lo_entity_set->set_deletable( abap_false ).

lo_entity_set->set_pageable( abap_false ).
lo_entity_set->set_addressable( abap_false ).
lo_entity_set->set_has_ftxt_search( abap_false ).
lo_entity_set->set_subscribable( abap_false ).
lo_entity_set->set_filter_required( abap_false ).
lo_entity_type = model->get_entity_type( iv_entity_name = 'F4CropVariants' ). "#EC NOTEXT
lo_entity_set = lo_entity_type->create_entity_set( 'F4CropVariantsSet' ). "#EC NOTEXT

lo_entity_set->set_creatable( abap_false ).
lo_entity_set->set_updatable( abap_false ).
lo_entity_set->set_deletable( abap_false ).

lo_entity_set->set_pageable( abap_false ).
lo_entity_set->set_addressable( abap_true ).
lo_entity_set->set_has_ftxt_search( abap_false ).
lo_entity_set->set_subscribable( abap_false ).
lo_entity_set->set_filter_required( abap_false ).
lo_entity_type = model->get_entity_type( iv_entity_name = 'F4WorkOrderTypes' ). "#EC NOTEXT
lo_entity_set = lo_entity_type->create_entity_set( 'F4WorkOrderTypesSet' ). "#EC NOTEXT

lo_entity_set->set_creatable( abap_false ).
lo_entity_set->set_updatable( abap_false ).
lo_entity_set->set_deletable( abap_false ).

lo_entity_set->set_pageable( abap_false ).
lo_entity_set->set_addressable( abap_false ).
lo_entity_set->set_has_ftxt_search( abap_false ).
lo_entity_set->set_subscribable( abap_false ).
lo_entity_set->set_filter_required( abap_false ).
lo_entity_type = model->get_entity_type( iv_entity_name = 'CATMasterData' ). "#EC NOTEXT
lo_entity_set = lo_entity_type->create_entity_set( 'CATMasterDataSet' ). "#EC NOTEXT

lo_entity_set->set_creatable( abap_false ).
lo_entity_set->set_updatable( abap_false ).
lo_entity_set->set_deletable( abap_false ).

lo_entity_set->set_pageable( abap_false ).
lo_entity_set->set_addressable( abap_true ).
lo_entity_set->set_has_ftxt_search( abap_false ).
lo_entity_set->set_subscribable( abap_false ).
lo_entity_set->set_filter_required( abap_false ).
lo_entity_type = model->get_entity_type( iv_entity_name = 'TaskOrderAnalytics' ). "#EC NOTEXT
lo_entity_set = lo_entity_type->create_entity_set( 'TaskOrderAnalyticsSet' ). "#EC NOTEXT

lo_entity_set->set_creatable( abap_false ).
lo_entity_set->set_updatable( abap_false ).
lo_entity_set->set_deletable( abap_false ).

lo_entity_set->set_pageable( abap_false ).
lo_entity_set->set_addressable( abap_true ).
lo_entity_set->set_has_ftxt_search( abap_false ).
lo_entity_set->set_subscribable( abap_false ).
lo_entity_set->set_filter_required( abap_false ).
lo_entity_type = model->get_entity_type( iv_entity_name = 'CatMaster1' ). "#EC NOTEXT
lo_entity_set = lo_entity_type->create_entity_set( 'CatMaster1Set' ). "#EC NOTEXT

lo_entity_set->set_creatable( abap_false ).
lo_entity_set->set_updatable( abap_false ).
lo_entity_set->set_deletable( abap_false ).

lo_entity_set->set_pageable( abap_false ).
lo_entity_set->set_addressable( abap_true ).
lo_entity_set->set_has_ftxt_search( abap_false ).
lo_entity_set->set_subscribable( abap_false ).
lo_entity_set->set_filter_required( abap_false ).


***********************************************************************************************************************************
*   new_associations
***********************************************************************************************************************************

 lo_association = model->create_association(
                            iv_association_name = 'MeasurementsToAttributes' "#EC NOTEXT
                            iv_left_type        = 'MeasurementDocumentHeader' "#EC NOTEXT
                            iv_right_type       = 'MeasurementAttributes' "#EC NOTEXT
                            iv_right_card       = 'N' "#EC NOTEXT
                            iv_left_card        = '1' ). "#EC NOTEXT
* Referential constraint for association - MeasurementsToAttributes
lo_ref_constraint = lo_association->create_ref_constraint( ).
lo_ref_constraint->add_property( iv_principal_property = 'Mdocm'   iv_dependent_property = 'Mdocm' )."#EC NOTEXT
* Association Sets for association - MeasurementsToAttributes
lo_assoc_set = lo_association->create_assoc_set( iv_assoc_set_name = 'MeasurementsToAttributesSet' ). "#EC NOTEXT


* Navigation Properties for entity - MeasurementAttributes
lo_entity_type = model->get_entity_type( iv_entity_name = 'MeasurementAttributes' ). "#EC NOTEXT
lo_nav_property = lo_entity_type->create_navigation_property( iv_property_name  = 'MeasurementDocumentHeader' "#EC NOTEXT
                                                          iv_association_name = 'MeasurementsToAttributes' ). "#EC NOTEXT
* Navigation Properties for entity - MeasurementDocumentHeader
lo_entity_type = model->get_entity_type( iv_entity_name = 'MeasurementDocumentHeader' ). "#EC NOTEXT
lo_nav_property = lo_entity_type->create_navigation_property( iv_property_name  = 'MeasurementAttributesSet' "#EC NOTEXT
                                                          iv_association_name = 'MeasurementsToAttributes' ). "#EC NOTEXT
  endmethod.


  method DEFINE.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS          &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL   &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                    &*
*&                                                                     &*
*&---------------------------------------------------------------------*


data:
  lo_entity_type    type ref to /iwbep/if_mgw_odata_entity_typ, "#EC NEEDED
  lo_complex_type   type ref to /iwbep/if_mgw_odata_cmplx_type, "#EC NEEDED
  lo_property       type ref to /iwbep/if_mgw_odata_property, "#EC NEEDED
  lo_association    type ref to /iwbep/if_mgw_odata_assoc,  "#EC NEEDED
  lo_assoc_set      type ref to /iwbep/if_mgw_odata_assoc_set, "#EC NEEDED
  lo_ref_constraint type ref to /iwbep/if_mgw_odata_ref_constr, "#EC NEEDED
  lo_nav_property   type ref to /iwbep/if_mgw_odata_nav_prop, "#EC NEEDED
  lo_action         type ref to /iwbep/if_mgw_odata_action, "#EC NEEDED
  lo_parameter      type ref to /iwbep/if_mgw_odata_property, "#EC NEEDED
  lo_entity_set     type ref to /iwbep/if_mgw_odata_entity_set, "#EC NEEDED
  lo_complex_prop   type ref to /iwbep/if_mgw_odata_cmplx_prop. "#EC NEEDED

* Extend the model
model->extend_model( iv_model_name = '/AGRI/GLTR_GIS_MDL' iv_model_version = '0001' ). "#EC NOTEXT

model->set_schema_namespace( '/AGRI/GLTR_GIS_SRV' ).
* New artifacts have been created in the service builder after the redefinition of service
create_new_artifacts( ).
  endmethod.


  method GET_EXTENDED_MODEL.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS          &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL   &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                    &*
*&                                                                     &*
*&---------------------------------------------------------------------*



ev_extended_service  = '/AGRI/GLTR_GIS_SRV'.                "#EC NOTEXT
ev_ext_service_version = '0001'.               "#EC NOTEXT
ev_extended_model    = '/AGRI/GLTR_GIS_MDL'.                    "#EC NOTEXT
ev_ext_model_version = '0001'.                   "#EC NOTEXT
  endmethod.


  method GET_LAST_MODIFIED.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS          &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL   &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                    &*
*&                                                                     &*
*&---------------------------------------------------------------------*


  constants: lc_gen_date_time type timestamp value '20200518063024'. "#EC NOTEXT
rv_last_modified = super->get_last_modified( ).
IF rv_last_modified LT lc_gen_date_time.
  rv_last_modified = lc_gen_date_time.
ENDIF.
  endmethod.


  method LOAD_TEXT_ELEMENTS.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS          &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL   &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                    &*
*&                                                                     &*
*&---------------------------------------------------------------------*


data:
  lo_entity_type    type ref to /iwbep/if_mgw_odata_entity_typ,           "#EC NEEDED
  lo_complex_type   type ref to /iwbep/if_mgw_odata_cmplx_type,           "#EC NEEDED
  lo_property       type ref to /iwbep/if_mgw_odata_property,             "#EC NEEDED
  lo_association    type ref to /iwbep/if_mgw_odata_assoc,                "#EC NEEDED
  lo_assoc_set      type ref to /iwbep/if_mgw_odata_assoc_set,            "#EC NEEDED
  lo_ref_constraint type ref to /iwbep/if_mgw_odata_ref_constr,           "#EC NEEDED
  lo_nav_property   type ref to /iwbep/if_mgw_odata_nav_prop,             "#EC NEEDED
  lo_action         type ref to /iwbep/if_mgw_odata_action,               "#EC NEEDED
  lo_parameter      type ref to /iwbep/if_mgw_odata_property,             "#EC NEEDED
  lo_entity_set     type ref to /iwbep/if_mgw_odata_entity_set.           "#EC NEEDED


DATA:
     ls_text_element TYPE ts_text_element.                   "#EC NEEDED
clear ls_text_element.


clear ls_text_element.
ls_text_element-artifact_name          = 'Updkz'.      "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                       "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'MEASUREMENTATTRIBUTES'.     "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'ETYP'.                             "#EC NOTEXT
ls_text_element-text_symbol            = '998'.         "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.
clear ls_text_element.
ls_text_element-artifact_name          = 'Atsch'.      "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                       "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'MEASUREMENTATTRIBUTES'.     "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'ETYP'.                             "#EC NOTEXT
ls_text_element-text_symbol            = '999'.         "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.
clear ls_text_element.
ls_text_element-artifact_name          = 'Atinp'.      "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                       "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'MEASUREMENTATTRIBUTES'.     "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'ETYP'.                             "#EC NOTEXT
ls_text_element-text_symbol            = '001'.         "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.
clear ls_text_element.
ls_text_element-artifact_name          = 'Data'.      "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                       "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'MEASUREMENTATTRIBUTES'.     "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'ETYP'.                             "#EC NOTEXT
ls_text_element-text_symbol            = '002'.         "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.
clear ls_text_element.
ls_text_element-artifact_name          = 'Updkz'.      "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                       "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'MEASUREMENTDOCUMENTHEADER'.     "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'ETYP'.                             "#EC NOTEXT
ls_text_element-text_symbol            = '003'.         "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.
clear ls_text_element.
ls_text_element-artifact_name          = 'Mdtyp'.      "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                       "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'MEASUREMENTTYPES'.     "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'ETYP'.                             "#EC NOTEXT
ls_text_element-text_symbol            = '004'.         "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.
clear ls_text_element.
ls_text_element-artifact_name          = 'Numki'.      "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                       "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'MEASUREMENTTYPES'.     "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'ETYP'.                             "#EC NOTEXT
ls_text_element-text_symbol            = '005'.         "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.
clear ls_text_element.
ls_text_element-artifact_name          = 'Aslvl'.      "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                       "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'MEASUREMENTTYPES'.     "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'ETYP'.                             "#EC NOTEXT
ls_text_element-text_symbol            = '006'.         "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.
clear ls_text_element.
ls_text_element-artifact_name          = 'Descr'.      "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                       "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'MEASUREMENTTYPES'.     "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'ETYP'.                             "#EC NOTEXT
ls_text_element-text_symbol            = '007'.         "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.
clear ls_text_element.
ls_text_element-artifact_name          = 'AslvlTxt'.      "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                       "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'MEASUREMENTTYPES'.     "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'ETYP'.                             "#EC NOTEXT
ls_text_element-text_symbol            = '008'.         "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.
clear ls_text_element.
ls_text_element-artifact_name          = 'TplnrFl'.      "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                       "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'F4CROPPROCESS'.     "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'ETYP'.                             "#EC NOTEXT
ls_text_element-text_symbol            = '661'.         "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.
clear ls_text_element.
ls_text_element-artifact_name          = 'Cmnum'.      "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                       "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'F4CROPPROCESS'.     "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'ETYP'.                             "#EC NOTEXT
ls_text_element-text_symbol            = '662'.         "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.
clear ls_text_element.
ls_text_element-artifact_name          = 'Varia'.      "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                       "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'F4CROPPROCESS'.     "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'ETYP'.                             "#EC NOTEXT
ls_text_element-text_symbol            = '663'.         "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.
clear ls_text_element.
ls_text_element-artifact_name          = 'Cpros'.      "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                       "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'F4CROPPROCESS'.     "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'ETYP'.                             "#EC NOTEXT
ls_text_element-text_symbol            = '673'.         "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.
clear ls_text_element.
ls_text_element-artifact_name          = 'Ppros'.      "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                       "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'F4CROPPROCESS'.     "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'ETYP'.                             "#EC NOTEXT
ls_text_element-text_symbol            = '674'.         "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.
clear ls_text_element.
ls_text_element-artifact_name          = 'Descr'.      "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                       "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'F4CROPPROCESS'.     "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'ETYP'.                             "#EC NOTEXT
ls_text_element-text_symbol            = '675'.         "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.
clear ls_text_element.
ls_text_element-artifact_name          = 'Spras'.      "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                       "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'F4WORKORDERTYPES'.     "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'ETYP'.                             "#EC NOTEXT
ls_text_element-text_symbol            = '676'.         "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.
clear ls_text_element.
ls_text_element-artifact_name          = 'Wotyp'.      "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                       "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'F4WORKORDERTYPES'.     "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'ETYP'.                             "#EC NOTEXT
ls_text_element-text_symbol            = '677'.         "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.
clear ls_text_element.
ls_text_element-artifact_name          = 'Descr'.      "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                       "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'F4WORKORDERTYPES'.     "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'ETYP'.                             "#EC NOTEXT
ls_text_element-text_symbol            = '678'.         "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.
clear ls_text_element.
ls_text_element-artifact_name          = 'Maktx'.      "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                       "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'CATMASTERDATA'.     "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'ETYP'.                             "#EC NOTEXT
ls_text_element-text_symbol            = '679'.         "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.
clear ls_text_element.
ls_text_element-artifact_name          = 'Descr'.      "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                       "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'CATMASTERDATA'.     "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'ETYP'.                             "#EC NOTEXT
ls_text_element-text_symbol            = '680'.         "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.
clear ls_text_element.
ls_text_element-artifact_name          = 'Dummy'.      "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                       "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'TASKORDERANALYTICS'.     "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'ETYP'.                             "#EC NOTEXT
ls_text_element-text_symbol            = '681'.         "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.
clear ls_text_element.
ls_text_element-artifact_name          = 'Updkz'.      "#EC NOTEXT
ls_text_element-artifact_type          = 'PROP'.                       "#EC NOTEXT
ls_text_element-parent_artifact_name   = 'TASKORDERANALYTICS'.     "#EC NOTEXT
ls_text_element-parent_artifact_type   = 'ETYP'.                             "#EC NOTEXT
ls_text_element-text_symbol            = '682'.         "#EC NOTEXT
APPEND ls_text_element TO rt_text_elements.
  endmethod.
ENDCLASS.
