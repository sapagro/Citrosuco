*&---------------------------------------------------------------------*
*&  Include           /AGRI/LGLFLUCON
*&---------------------------------------------------------------------*

CONSTANTS: BEGIN OF c_tablename,
             rchdr TYPE dd02d-tabname VALUE 'ZFMRCHDR',
             rclst TYPE dd02d-tabname VALUE 'ZFMRCLST',
             rcbom TYPE dd02d-tabname VALUE 'ZFMRCBOM',
             rcvrs TYPE dd02d-tabname VALUE 'ZFMRCVRS',
           END OF c_tablename.

*--Object
CONSTANTS: BEGIN OF c_object,
             func_loc         TYPE tabelle            VALUE 'ZFMRCHDR',
             bor              LIKE /agri/tgabo-object VALUE 'ZFMRC',
             log              LIKE balobj-object      VALUE 'ZFMRC',
             change_documents TYPE cdobjectcl         VALUE 'ZFMRC',
             esh_object       TYPE /agri/geshobjtp    VALUE 'ZFMRC',
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
             rc TYPE inri-object VALUE 'ZRNARC',
           END OF c_number_range.
