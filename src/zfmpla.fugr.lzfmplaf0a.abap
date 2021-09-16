*&---------------------------------------------------------------------*
*& Include          LZFMACAF0A
*&---------------------------------------------------------------------*


FORM attribute_get CHANGING lt_fmacitm TYPE zt_fmacitm .

  DATA: lt_fmattr      TYPE TABLE OF zsc_fmattr,
        lwa_fmattr     TYPE zsc_fmattr,
        lt_glmdatv     TYPE TABLE OF /agri/glmdatv,
        lt_fmcufld     TYPE ztt_fmcufld,
        lt_fmcugrp     TYPE ztt_fmcugrp,
        lt_fmacitm_tem TYPE zt_fmacitm.

  FIELD-SYMBOLS: <lwa_fmcugrp> TYPE  zsc_fmcugrp,
                 <lwa_fmcufld> TYPE  zsc_fmcufld,
                 <lwa_fmacitm> TYPE  zsc_fmacitm.

  DATA: r_mdtyp  TYPE RANGE OF /agri/glmdhdr-mdtyp.
  DATA: r_aslvl  TYPE RANGE OF /agri/glmdhdr-aslvl.
  DATA: r_mpgrp  TYPE RANGE OF /agri/glmdhdr-mpgrp.
  DATA: r_atinn  TYPE RANGE OF /agri/glmdatv-atinn.
  DATA: lsr_mdtyp LIKE LINE OF r_mdtyp.
  DATA: lsr_aslvl LIKE LINE OF r_aslvl.
  DATA: lsr_mpgrp LIKE LINE OF r_mpgrp.
  DATA: lsr_atinn LIKE LINE OF r_atinn.


*---- Master Data
  PERFORM master_data_attribute_get CHANGING lt_fmcufld
                                             lt_fmcugrp.
  LOOP AT lt_fmcugrp ASSIGNING <lwa_fmcugrp>.
    lsr_mdtyp-sign   =  'I'.
    lsr_mdtyp-option =  'EQ'.
    lsr_mdtyp-low    =  <lwa_fmcugrp>-mdtyp.
    APPEND lsr_mdtyp TO r_mdtyp.

    lsr_aslvl-sign   =  'I'.
    lsr_aslvl-option =  'EQ'.
    lsr_aslvl-low    =  <lwa_fmcugrp>-aslvl.
    APPEND lsr_aslvl TO r_aslvl.

    lsr_mpgrp-sign   =  'I'.
    lsr_mpgrp-option =  'EQ'.
    lsr_mpgrp-low    =  <lwa_fmcugrp>-class.
    APPEND lsr_mpgrp TO r_mpgrp.

    LOOP AT  lt_fmcufld ASSIGNING <lwa_fmcufld>
                        WHERE class = <lwa_fmcugrp>-class.
      lsr_atinn-sign   =  'I'.
      lsr_atinn-option =  'EQ'.
      lsr_atinn-low    =  <lwa_fmcufld>-atinn.
      APPEND lsr_atinn TO r_atinn.

    ENDLOOP.
  ENDLOOP.

  IF lt_fmacitm IS NOT INITIAL.
    SELECT * FROM /agri/glmdhdr AS r
                  INNER JOIN /agri/glmdatv AS v
                  ON r~mdocm = v~mdocm
                     INTO CORRESPONDING FIELDS OF TABLE lt_fmattr
                    WHERE r~mdtyp IN r_mdtyp
                AND r~aslvl IN r_aslvl
             AND r~mpgrp  IN r_mpgrp
           AND v~atinn IN r_atinn.
  ENDIF.

  LOOP AT lt_fmacitm ASSIGNING <lwa_fmacitm>.
    LOOP AT lt_fmattr INTO lwa_fmattr WHERE
                           tplnr_fl = <lwa_fmacitm>-tplnr_fl.
        IF lwa_fmattr-atinn = '0000000324'.
          MOVE lwa_fmattr-atflv TO <lwa_fmacitm>-adqpl.
          ELSEIF lwa_fmattr-atinn = '0000000325'.
          MOVE lwa_fmattr-atflv TO <lwa_fmacitm>-advlc.
        ENDIF.
    ENDLOOP.
  ENDLOOP.

ENDFORM.
