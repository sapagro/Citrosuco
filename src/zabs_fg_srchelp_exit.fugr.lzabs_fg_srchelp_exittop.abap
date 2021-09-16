FUNCTION-POOL zabs_fg_srchelp_exit.         "MESSAGE-ID ..

* INCLUDE LZABS_FG_SRCHELP_EXITD...          " Local class definition

TYPES : BEGIN OF ty_attr_grp,
          class TYPE /agri/gatgrp,
          kschl TYPE klschl,
        END OF ty_attr_grp,

        BEGIN OF ty_attr,
          atnam TYPE atnam,
          atbez TYPE atbez,
        END OF ty_attr.
