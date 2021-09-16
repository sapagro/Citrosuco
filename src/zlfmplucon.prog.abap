*&---------------------------------------------------------------------*
*& Include          ZLFMPLUCON
*&---------------------------------------------------------------------*

CONSTANTS: BEGIN OF c_tablename,
            plhdr  TYPE dd02d-tabname VALUE 'ZFMPLHDR',
            plitm  TYPE dd02d-tabname VALUE 'ZFMPLITM',
           END OF c_tablename.

*--Object
CONSTANTS: BEGIN OF c_object,
             func_loc         TYPE tabelle            VALUE 'ZFMPLHDR',
             bor              LIKE /agri/tgabo-object VALUE 'ZFMPLC',
             log              LIKE balobj-object      VALUE 'ZFMPLC',
             change_documents TYPE cdobjectcl         VALUE 'ZFMPLC',
             esh_object       TYPE /agri/geshobjtp    VALUE 'ZFMPLC',
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
            pl TYPE inri-object VALUE 'ZRNAPL',
           END OF c_number_range.
