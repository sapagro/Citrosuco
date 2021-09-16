FUNCTION-POOL zfmfp_task_order_create.      "MESSAGE-ID ..

* INCLUDE LZFMFP_TASK_ORDER_CREATED...       " Local class definition

DATA: gt_fmfphdr          TYPE STANDARD TABLE OF /agri/fmfphdr INITIAL SIZE 0,
      gt_fpdoc            TYPE /agri/t_fmfp_doc,
      gv_process_material TYPE matnr,
      gv_subrc            TYPE sysubrc.

****Log Initiator
CONSTANTS: BEGIN OF c_log_initiator,
             create  TYPE /agri/gdescr VALUE 'CREATE',
             change  TYPE /agri/gdescr VALUE 'CHANGE',
             display TYPE /agri/gdescr VALUE 'DISPLAY',
             save    TYPE /agri/gdescr VALUE 'SAVE',
             post    TYPE /agri/gdescr VALUE 'POST',
             check   TYPE /agri/gdescr VALUE 'CHECK',
             read    TYPE /agri/gdescr VALUE 'READ',
             upload  TYPE /agri/gdescr VALUE 'UPLOAD',
             copy    TYPE /agri/gdescr VALUE 'COPY',
             mass    TYPE /agri/gdescr VALUE 'MASS',
           END OF c_log_initiator.

CONSTANTS : BEGIN OF c_log_subobject,
              create TYPE balsubobj VALUE 'CREATE',
              change TYPE balsubobj VALUE 'CHANGE',
              post   TYPE balsubobj VALUE 'RELEASE',
              save   TYPE balsubobj VALUE 'SAVE',
              check  TYPE balsubobj VALUE 'CHECK',
              upload TYPE balsubobj VALUE 'UPLOAD',
              read   TYPE balsubobj VALUE 'READ',
              mass   TYPE balsubobj VALUE 'MASS',
            END OF c_log_subobject.
