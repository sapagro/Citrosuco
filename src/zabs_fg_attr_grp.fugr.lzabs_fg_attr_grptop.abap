FUNCTION-POOL zabs_fg_attr_grp.             "MESSAGE-ID ..

* INCLUDE LZABS_FG_ATTR_GRPD...              " Local class definition

DATA:
  gt_attributes TYPE zabs_tty_attr_forml.

DATA  : BEGIN OF gs_variables,
          subrc TYPE subrc,
          atnam TYPE /agri/gatnam,
          ident TYPE zabs_del_ident,
        END OF gs_variables.
