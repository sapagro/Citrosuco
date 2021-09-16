*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 26.12.2019 at 00:47:10
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZABS_TAB_CBCH...................................*
DATA:  BEGIN OF STATUS_ZABS_TAB_CBCH                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZABS_TAB_CBCH                 .
CONTROLS: TCTRL_ZABS_TAB_CBCH
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZABS_TAB_CBCH                 .
TABLES: ZABS_TAB_CBCH                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
