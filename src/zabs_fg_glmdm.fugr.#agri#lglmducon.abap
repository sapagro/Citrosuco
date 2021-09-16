*&---------------------------------------------------------------------*
*&  Include           /AGRI/LGLMDUCON
*&---------------------------------------------------------------------*

CONSTANTS: BEGIN OF c_tablename,
            mdhdr  TYPE dd02d-tabname VALUE '/AGRI/GLMDHDR',
            mditm  TYPE dd02d-tabname VALUE '/AGRI/GLMDITM',
            mdatv  TYPE dd02d-tabname VALUE '/AGRI/GLMDATV',
           END OF c_tablename.

CONSTANTS: BEGIN OF c_number_range,
             md   LIKE inri-object VALUE '/AGRI/GLMD',
           END OF c_number_range.

*--Object
CONSTANTS: BEGIN OF c_object,
             bor              LIKE /agri/tgabo-object VALUE '/AGRI/GLMD',
             log              LIKE balobj-object      VALUE '/AGRI/GLMD',
             change_documents TYPE cdobjectcl         VALUE '/AGRI/GLMD',
             esh_object       TYPE /agri/geshobjtp    VALUE '/AGRI/GLMD',
             text_object      TYPE thead-tdobject     VALUE '/AGRI/GLMD',
           END OF c_object.
*{   INSERT         C4DK903770                                        1
CONSTANTS: BEGIN OF c_brf_object,
            glmd TYPE /agri/gbrfobjtp VALUE '/AGRI/GLMD',
           END OF c_brf_object.
*}   INSERT
