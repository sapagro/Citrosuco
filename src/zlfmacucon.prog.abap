*&---------------------------------------------------------------------*
*&  Include           /AGRI/LGLFLUCON
*&---------------------------------------------------------------------*

CONSTANTS: BEGIN OF c_tablename,
            achdr  TYPE dd02d-tabname VALUE 'ZFMACHDR',
            acitm  TYPE dd02d-tabname VALUE 'ZFMAITM',
            acvlc  TYPE dd02d-tabname VALUE 'ZFMACVLCL',
           END OF c_tablename.

*--Object
CONSTANTS: BEGIN OF c_object,
             func_loc         TYPE tabelle            VALUE 'ZFMACHDR',
             bor              LIKE /agri/tgabo-object VALUE 'ZFMAC',
             log              LIKE balobj-object      VALUE 'ZFMAC',
             change_documents TYPE cdobjectcl         VALUE 'ZFMAC',
             esh_object       TYPE /agri/geshobjtp    VALUE 'ZFMAC',
           END OF c_object.

****Log Sub-object
CONSTANTS : BEGIN OF c_log_subobject,
              create TYPE balsubobj VALUE 'CREATE',
              change TYPE balsubobj VALUE 'CHANGE',
              post   TYPE balsubobj VALUE 'RELEASE',
              save   TYPE balsubobj VALUE 'SAVE',
              check  TYPE balsubobj VALUE 'CHECK',
              upload TYPE balsubobj VALUE 'UPLOAD',
              read   TYPE balsubobj VALUE 'READ',
            END OF c_log_subobject.

CONSTANTS: BEGIN OF c_number_range,
            ac TYPE inri-object VALUE 'ZRNADC',
           END OF c_number_range.
