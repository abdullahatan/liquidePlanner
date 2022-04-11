*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: YFIN_LP_V01.....................................*
TABLES: YFIN_LP_V01, *YFIN_LP_V01. "view work areas
CONTROLS: TCTRL_YFIN_LP_V01
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_YFIN_LP_V01. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_YFIN_LP_V01.
* Table for entries selected to show on screen
DATA: BEGIN OF YFIN_LP_V01_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE YFIN_LP_V01.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF YFIN_LP_V01_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF YFIN_LP_V01_TOTAL OCCURS 0010.
INCLUDE STRUCTURE YFIN_LP_V01.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF YFIN_LP_V01_TOTAL.

*...processing: YFIN_LP_V02.....................................*
TABLES: YFIN_LP_V02, *YFIN_LP_V02. "view work areas
CONTROLS: TCTRL_YFIN_LP_V02
TYPE TABLEVIEW USING SCREEN '0002'.
DATA: BEGIN OF STATUS_YFIN_LP_V02. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_YFIN_LP_V02.
* Table for entries selected to show on screen
DATA: BEGIN OF YFIN_LP_V02_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE YFIN_LP_V02.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF YFIN_LP_V02_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF YFIN_LP_V02_TOTAL OCCURS 0010.
INCLUDE STRUCTURE YFIN_LP_V02.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF YFIN_LP_V02_TOTAL.

*.........table declarations:.................................*
TABLES: YFIN_LP_T01                    .
TABLES: YFIN_LP_T02                    .
