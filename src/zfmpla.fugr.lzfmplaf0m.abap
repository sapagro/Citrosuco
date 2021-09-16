*&---------------------------------------------------------------------*
*& Include          LZFMACAF0M
*&---------------------------------------------------------------------*

FORM master_data_attribute_get CHANGING lt_fmcufld TYPE ztt_fmcufld
                                        lt_fmcugrp TYPE ztt_fmcugrp.

  SELECT * FROM ztg_fmcufld
           INTO TABLE lt_fmcufld.

  SELECT * FROM ztg_fmcugrp
           INTO TABLE lt_fmcugrp.

ENDFORM.
