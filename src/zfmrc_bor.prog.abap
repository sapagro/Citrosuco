*****           Implementation of object type ZFMAC                *****
INCLUDE <OBJECT>.
BEGIN_DATA OBJECT. " Do not change.. DATA is generated
* only private members may be inserted into structure private
DATA:
" begin of private,
"   to declare private attributes remove comments and
"   insert private attributes here ...
" end of private,
  BEGIN OF KEY,
      RECEITA LIKE ZFMRCHDR-RCNUM,
      PLANT LIKE ZFMRCHDR-WERKS,
      TAREFA LIKE ZFMRCHDR-MATNR,
      TIPODERECEITA LIKE ZFMRCHDR-RCTYP,
  END OF KEY.
END_DATA OBJECT. " Do not change.. DATA is generated
