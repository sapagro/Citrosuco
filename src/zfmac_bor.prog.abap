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
      CROPAREA LIKE ZFMACHDR-ACNUM,
  END OF KEY.
END_DATA OBJECT. " Do not change.. DATA is generated
