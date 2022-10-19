     1 IDENTIFICATION DIVISION.
     2 PROGRAM-ID.                 SUNDATE.
     3 ENVIRONMENT DIVISION.
     4 CONFIGURATION SECTION.
     5 DATA DIVISION.
     6 FILE SECTION.
     7 WORKING-STORAGE SECTION.
     8 01 DIFFTME-CTR PIC 9(09) COMP-3 VALUE 0.
     9 01 ELRATEX-CTR PIC 9(09) COMP-3 VALUE 0.
    10 01 ELRAMTX-CTR PIC 9(09) COMP-3 VALUE 0.
    11 01 ELRTRMX-CTR PIC 9(09) COMP-3 VALUE 0.
    12 01 ELUPRMX-CTR PIC 9(09) COMP-3 VALUE 0.
    13 01 ELDATCX-CTR PIC 9(09) COMP-3 VALUE 0.
    14 01 ALLCALL-CTR PIC 9(09) COMP-3 VALUE 0.
    15 01 SUNBTIME.
    16    05  SUNBHH   PIC 9(02).
    17    05  SUNBMN   PIC 9(02).
    18    05  SUNBSE   PIC 9(02).
    19    05  SUNBHS   PIC 9(02).
    20 01 SUNETIME.
    21    05  SUNEHH   PIC 9(02).
    22    05  SUNEMN   PIC 9(02).
    23    05  SUNESE   PIC 9(02).
    24    05  SUNEHS   PIC 9(02).
    25 01 SUNRTN  PIC X(08).
    26     01  WS-NCL-OB-DATES-TBL OCCURS 3.
    27         16  WS-NCL-OB-DATES    PIC 9(08).
    28         16  WS-NCL-OB-DATES-R  REDEFINES WS-NCL-OB-DATES.
    29             20  WS-OB-CCYY      PIC 9(04).
    30             20  WS-OB-CCYR  REDEFINES  WS-OB-CCYY.
    31                 24  WS-OB-CC    PIC 9(02).
    32                 24  WS-OB-YR    PIC 9(02).
    33             20  WS-OB-MO        PIC 9(02).
    34             20  WS-OB-DA        PIC 9(02).
SUNPSD*****COPY ELCDATE.
    36******************************************************************06/11/98
    37*                                                                *ELCDATE
    38*                                                                *ELCDATE
    39*                            ELCDATE.                            *   LV023
    40*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE               CL*16
    41*                            VMOD=2.003                           ELCDATE
    42*                                                                *ELCDATE
    43*                                                                *ELCDATE
    44*   DESCRIPTION:  DATA PASSED TO DATE CONVERSION ROUTINE.        *ELCDATE
    45*                 LENGTH = 200                                   *   CL*19
    46******************************************************************ELCDATE
    47                                                                  ELCDATE
    48 01  DATE-CONVERSION-DATA.                                        ELCDATE
    49     12  DC-COMM-LENGTH                PIC S9(4) COMP VALUE +200.    CL*19
    50     12  DC-OPTION-CODE                PIC X.                        CL*15
    51         88  BIN-TO-GREG                VALUE ' '.                ELCDATE
    52         88  ELAPSED-BETWEEN-BIN        VALUE '1'.                ELCDATE
    53         88  EDIT-GREG-TO-BIN           VALUE '2'.                ELCDATE
    54         88  YMD-GREG-TO-BIN            VALUE '3'.                ELCDATE
    55         88  MDY-GREG-TO-BIN            VALUE '4'.                ELCDATE
    56         88  JULIAN-TO-BIN              VALUE '5'.                ELCDATE
    57         88  BIN-PLUS-ELAPSED           VALUE '6'.                ELCDATE
    58         88  FIND-CENTURY               VALUE '7'.                   CL*22
    59         88  ELAPSED-BETWEEN-BIN-3      VALUE '8'.                ELCDATE
    60         88  EDIT-GREG-TO-BIN-3         VALUE '9'.                ELCDATE
    61         88  YMD-GREG-TO-BIN-3          VALUE 'A'.                ELCDATE
    62         88  MDY-GREG-TO-BIN-3          VALUE 'B'.                ELCDATE
    63         88  JULIAN-TO-BIN-3            VALUE 'C'.                ELCDATE
    64         88  BIN-PLUS-ELAPSED-3         VALUE 'D'.                ELCDATE
    65         88  JULIAN-EXPANDED-TO-BIN     VALUE 'E'.                   CL*14
    66         88  JULIAN-EXPANDED-TO-BIN-3   VALUE 'F'.                   CL*14
    67         88  BIN-TO-JULIAN-EXPANDED     VALUE 'G'.                   CL*14
    68         88  JULIAN-EXPANDED            VALUE 'E', 'F', 'G'.         CL*14
    69         88  CHECK-LEAP-YEAR            VALUE 'H'.                   CL*14
    70         88  BIN-3-TO-GREG              VALUE 'I'.                   CL*14
    71         88  CYMD-GREG-TO-BIN-3         VALUE 'J'.                   CL*14
    72         88  MDCY-GREG-TO-BIN-3         VALUE 'K'.                   CL*14
    73         88  CYMD-GREG-TO-BIN           VALUE 'L'.                   CL*14
    74         88  MDCY-GREG-TO-BIN           VALUE 'M'.                   CL*14
    75         88  MDY-GREG-TO-JULIAN         VALUE 'N'.                   CL*14
    76         88  MDCY-GREG-TO-JULIAN        VALUE 'O'.                   CL*14
    77         88  YMD-GREG-TO-JULIAN         VALUE 'P'.                   CL*14
    78         88  CYMD-GREG-TO-JULIAN        VALUE 'Q'.                   CL*14
    79         88  THREE-CHARACTER-BIN                                  ELCDATE
    80                  VALUES  '8' '9' 'A' 'B' 'C' 'D' 'I' 'J' 'K'.       CL*14
    81         88  GREGORIAN-TO-BIN                                        CL**2
    82                  VALUES '2' '3' '4' '9' 'A' 'B' 'J' 'K' 'L' 'M'.    CL*14
    83         88  BIN-TO-GREGORIAN                                        CL**2
    84                  VALUES ' ' '1' 'I' '8' 'G'.                        CL*14
    85         88  JULIAN-TO-BINARY                                        CL**2
    86                  VALUES '5' 'C' 'E' 'F'.                            CL*14
    87     12  DC-ERROR-CODE                 PIC X.                        CL*15
    88         88  NO-CONVERSION-ERROR        VALUE ' '.                   CL**3
    89         88  DATE-CONVERSION-ERROR                                ELCDATE
    90                  VALUES '1' '2' '3' '4' '5' '9' 'A' 'B' 'C'.        CL*14
    91         88  DATE-IS-ZERO               VALUE '1'.                ELCDATE
    92         88  DATE-IS-NON-NUMERIC        VALUE '2'.                ELCDATE
    93         88  DATE-IS-INVALID            VALUE '3'.                ELCDATE
    94         88  DATE1-GREATER-DATE2        VALUE '4'.                ELCDATE
    95         88  ELAPSED-PLUS-NEGATIVE      VALUE '5'.                ELCDATE
    96         88  DATE-INVALID-OPTION        VALUE '9'.                ELCDATE
    97         88  INVALID-CENTURY            VALUE 'A'.                ELCDATE
    98         88  ONLY-CENTURY               VALUE 'B'.                ELCDATE
    99         88  ONLY-LEAP-YEAR             VALUE 'C'.                ELCDATE
   100         88  VALID-CENTURY-LEAP-YEAR    VALUE 'B', 'C'.              CL**3
   101     12  DC-END-OF-MONTH               PIC X.                        CL*15
   102         88  CALCULATE-END-OF-MONTH     VALUE '1'.                ELCDATE
   103     12  DC-CENTURY-ADJUSTMENT         PIC X   VALUE SPACES.         CL*15
   104         88  USE-NORMAL-PROCESS         VALUE ' '.                ELCDATE
   105         88  ADJUST-DOWN-100-YRS        VALUE '1'.                ELCDATE
   106         88  ADJUST-UP-100-YRS          VALUE '2'.                ELCDATE
   107     12  FILLER                        PIC X.                        CL*15
   108     12  DC-CONVERSION-DATES.                                        CL*15
   109         16  DC-BIN-DATE-1             PIC XX.                       CL*15
   110         16  DC-BIN-DATE-2             PIC XX.                       CL*15
   111         16  DC-GREG-DATE-1-EDIT       PIC X(08).                    CL*15
   112         16  DC-GREG-DATE-1-EDIT-R REDEFINES                         CL*15
   113                       DC-GREG-DATE-1-EDIT.                       ELCDATE
   114             20  DC-EDIT1-MONTH        PIC 99.                       CL*15
   115             20  SLASH1-1              PIC X.                        CL*15
   116             20  DC-EDIT1-DAY          PIC 99.                       CL*15
   117             20  SLASH1-2              PIC X.                        CL*15
   118             20  DC-EDIT1-YEAR         PIC 99.                       CL*15
   119         16  DC-GREG-DATE-2-EDIT       PIC X(08).                    CL*15
   120         16  DC-GREG-DATE-2-EDIT-R REDEFINES                         CL*15
   121                     DC-GREG-DATE-2-EDIT.                         ELCDATE
   122             20  DC-EDIT2-MONTH        PIC 99.                       CL*15
   123             20  SLASH2-1              PIC X.                        CL*15
   124             20  DC-EDIT2-DAY          PIC 99.                       CL*15
   125             20  SLASH2-2              PIC X.                        CL*15
   126             20  DC-EDIT2-YEAR         PIC 99.                       CL*15
   127         16  DC-GREG-DATE-1-YMD        PIC 9(06).                    CL*15
   128         16  DC-GREG-DATE-1-YMD-R  REDEFINES                         CL*15
   129                     DC-GREG-DATE-1-YMD.                          ELCDATE
   130             20  DC-YMD-YEAR           PIC 99.                       CL*15
   131             20  DC-YMD-MONTH          PIC 99.                       CL*15
   132             20  DC-YMD-DAY            PIC 99.                       CL*15
   133         16  DC-GREG-DATE-1-MDY        PIC 9(06).                    CL*15
   134         16  DC-GREG-DATE-1-MDY-R REDEFINES                          CL*15
   135                      DC-GREG-DATE-1-MDY.                         ELCDATE
   136             20  DC-MDY-MONTH          PIC 99.                       CL*15
   137             20  DC-MDY-DAY            PIC 99.                       CL*15
   138             20  DC-MDY-YEAR           PIC 99.                       CL*15
   139         16  DC-GREG-DATE-1-ALPHA.                                   CL*15
   140             20  DC-ALPHA-MONTH        PIC X(10).                    CL*15
   141             20  DC-ALPHA-DAY          PIC 99.                       CL*15
   142             20  FILLER                PIC XX.                       CL*15
   143             20  DC-ALPHA-CENTURY.                                   CL*15
   144                 24 DC-ALPHA-CEN-N     PIC 99.                       CL*15
   145             20  DC-ALPHA-YEAR         PIC 99.                       CL*15
   146         16  DC-ELAPSED-MONTHS         PIC S9(4)     COMP.           CL*15
   147         16  DC-ODD-DAYS-OVER          PIC S9(4)     COMP.           CL*15
   148         16  DC-ELAPSED-DAYS           PIC S9(4)     COMP.           CL*15
   149         16  DC-JULIAN-DATE            PIC 9(05).                    CL*15
   150         16  DC-JULIAN-YYDDD REDEFINES DC-JULIAN-DATE                CL*20
   151                                       PIC 9(05).                    CL*20
   152         16  DC-JULIAN-DT REDEFINES DC-JULIAN-DATE.                  CL*15
   153             20  DC-JULIAN-YEAR        PIC 99.                       CL*15
   154             20  DC-JULIAN-DAYS        PIC 999.                      CL*15
   155         16  DC-DAYS-IN-MONTH          PIC S9(3)       COMP-3.       CL*15
   156         16  DC-DAY-OF-WEEK            PIC S9  VALUE ZERO COMP-3.    CL*15
   157         16  DC-DAY-OF-WEEK2           PIC S9  VALUE ZERO COMP-3.    CL*15
   158     12  DATE-CONVERSION-VARIBLES.                                   CL*15
   159         16  HOLD-CENTURY-1            PIC 9(11) VALUE 0.            CL*15
   160         16  HOLD-CENTURY-1-SPLIT REDEFINES HOLD-CENTURY-1.          CL*15
   161             20  FILLER                PIC 9(3).                     CL*15
   162             20  HOLD-CEN-1-CCYY.                                    CL*15
   163                 24  HOLD-CEN-1-CC     PIC 99.                       CL*17
   164                 24  HOLD-CEN-1-YY     PIC 99.                       CL*17
   165             20  HOLD-CEN-1-MO         PIC 99.                       CL*17
   166             20  HOLD-CEN-1-DA         PIC 99.                       CL*17
   167         16  HOLD-CENTURY-1-R   REDEFINES HOLD-CENTURY-1.            CL*15
   168             20  HOLD-CEN-1-R-MO       PIC 99.                       CL*17
   169             20  HOLD-CEN-1-R-DA       PIC 99.                       CL*17
   170             20  HOLD-CEN-1-R-CCYY.                                  CL*15
   171                 24  HOLD-CEN-1-R-CC   PIC 99.                       CL*17
   172                 24  HOLD-CEN-1-R-YY   PIC 99.                       CL*17
   173             20  FILLER                PIC 9(3).                     CL*15
   174         16  HOLD-CENTURY-1-X.                                       CL*15
   175             20  FILLER                PIC X(3)  VALUE SPACES.       CL*15
   176             20  HOLD-CEN-1-X-CCYY.                                  CL*15
   177                 24  HOLD-CEN-1-X-CC   PIC XX VALUE SPACES.          CL*17
   178                 24  HOLD-CEN-1-X-YY   PIC XX VALUE SPACES.          CL*17
   179             20  HOLD-CEN-1-X-MO       PIC XX VALUE SPACES.          CL*17
   180             20  HOLD-CEN-1-X-DA       PIC XX VALUE SPACES.          CL*17
   181         16  HOLD-CENTURY-1-R-X REDEFINES HOLD-CENTURY-1-X.          CL*15
   182             20  HOLD-CEN-1-R-X-MO     PIC XX.                       CL*17
   183             20  HOLD-CEN-1-R-X-DA     PIC XX.                       CL*17
   184             20  HOLD-CEN-1-R-X-CCYY.                                CL*15
   185                 24  HOLD-CEN-1-R-X-CC PIC XX.                       CL*17
   186                 24  HOLD-CEN-1-R-X-YY PIC XX.                       CL*17
   187             20  FILLER                PIC XXX.                      CL*23
   188         16  DC-BIN-DATE-EXPAND-1      PIC XXX.                      CL*15
   189         16  DC-BIN-DATE-EXPAND-2      PIC XXX.                      CL*15
   190         16  DC-JULIAN-DATE-1          PIC 9(07).                    CL*15
   191         16  DC-JULIAN-DATE-1-R REDEFINES DC-JULIAN-DATE-1.          CL*15
   192             20  DC-JULIAN-1-CCYY.                                   CL*15
   193                 24  DC-JULIAN-1-CC    PIC 99.                       CL*15
   194                 24  DC-JULIAN-1-YR    PIC 99.                       CL*15
   195             20  DC-JULIAN-DA-1        PIC 999.                      CL*15
   196         16  DC-JULIAN-DATE-2          PIC 9(07).                    CL*15
   197         16  DC-JULIAN-DATE-2-R REDEFINES DC-JULIAN-DATE-2.          CL*15
   198             20  DC-JULIAN-2-CCYY.                                   CL*15
   199                 24  DC-JULIAN-2-CC    PIC 99.                       CL*15
   200                 24  DC-JULIAN-2-YR    PIC 99.                       CL*15
   201             20  DC-JULIAN-DA-2        PIC 999.                      CL*15
   202         16  DC-GREG-DATE-A-EDIT.                                    CL*15
   203             20  DC-EDITA-MONTH        PIC 99.                       CL*15
   204             20  SLASHA-1              PIC X VALUE '/'.              CL*15
   205             20  DC-EDITA-DAY          PIC 99.                       CL*15
   206             20  SLASHA-2              PIC X VALUE '/'.              CL*15
   207             20  DC-EDITA-CCYY.                                      CL*15
   208                 24  DC-EDITA-CENT     PIC 99.                       CL*15
   209                 24  DC-EDITA-YEAR     PIC 99.                       CL*15
   210         16  DC-GREG-DATE-B-EDIT.                                    CL*15
   211             20  DC-EDITB-MONTH        PIC 99.                       CL*15
   212             20  SLASHB-1              PIC X VALUE '/'.              CL*15
   213             20  DC-EDITB-DAY          PIC 99.                       CL*15
   214             20  SLASHB-2              PIC X VALUE '/'.              CL*15
   215             20  DC-EDITB-CCYY.                                      CL*15
   216                 24  DC-EDITB-CENT     PIC 99.                       CL*15
   217                 24  DC-EDITB-YEAR     PIC 99.                       CL*15
   218         16  DC-GREG-DATE-CYMD         PIC 9(08).                    CL*15
   219         16  DC-GREG-DATE-CYMD-R REDEFINES                           CL*15
   220                              DC-GREG-DATE-CYMD.                  ELCDATE
   221             20  DC-CYMD-CEN           PIC 99.                       CL*15
   222             20  DC-CYMD-YEAR          PIC 99.                       CL*15
   223             20  DC-CYMD-MONTH         PIC 99.                       CL*15
   224             20  DC-CYMD-DAY           PIC 99.                       CL*15
   225         16  DC-GREG-DATE-MDCY         PIC 9(08).                    CL*15
   226         16  DC-GREG-DATE-MDCY-R REDEFINES                           CL*15
   227                              DC-GREG-DATE-MDCY.                  ELCDATE
   228             20  DC-MDCY-MONTH         PIC 99.                       CL*15
   229             20  DC-MDCY-DAY           PIC 99.                       CL*15
   230             20  DC-MDCY-CEN           PIC 99.                       CL*15
   231             20  DC-MDCY-YEAR          PIC 99.                       CL*15
   232    12  DC-FORCE-EL310-DATE-SW         PIC X    VALUE SPACE.      ELCDATE
   233        88  DC-FORCE-EL310-DATE                 VALUE 'Y'.        ELCDATE
   234    12  DC-EL310-DATE                  PIC X(21).                    CL*26
   235    12  FILLER                         PIC X(28).                    CL*26
   236 PROCEDURE DIVISION.
   237
   238     DISPLAY 'SUNDATE 2 MILLION DATE CALL TEST'.
   239     PERFORM 1000-DT THRU 1000-EXIT 2000000 TIMES.
   240     PERFORM 1999-EOJ.
   241
   242 1000-DT.
   243     MOVE 20020825               TO WS-NCL-OB-DATES (2).
   244     MOVE +0                     TO DC-ELAPSED-DAYS.
   245     MOVE -2                     TO DC-ELAPSED-MONTHS.
   246     MOVE '6'                    TO DC-OPTION-CODE.
   247     PERFORM 0400-DATE-CONVERSION-ROUTINE THRU 0409-EXIT.
   248 1000-EXIT.
   249     EXIT.
   250
   251 1999-EOJ.
   252     DISPLAY "SUNPSD ELRATEX-CTR="   ELRATEX-CTR.
   253     DISPLAY "SUNPSD ELRAMTX-CTR="   ELRAMTX-CTR.
   254     DISPLAY "SUNPSD ELRTRMX-CTR="   ELRTRMX-CTR.
   255     DISPLAY "SUNPSD ELUPRMX-CTR="   ELUPRMX-CTR.
   256     DISPLAY "SUNPSD ELDATCX-CTR="   ELDATCX-CTR.
   257     DISPLAY "SUNPSD DIFFTME-CTR="   DIFFTME-CTR.
   258     DISPLAY "SUNPSD --------------------------".
   259     DISPLAY "SUNPSD ALLCALL-CTR="   ALLCALL-CTR.
   260     GOBACK.
   261 0400-DATE-CONVERSION-ROUTINE.
   262
SUNPSD*****COPY SUNDCS.
   264****************************************************************  04/15/98
   265*                                                              *  ELCDCS
   266*                                                              *  ELCDCS
   267*                            ELCDCS.                           *     LV003
   268*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE               CL**2
   269*                            VMOD=2.002                        *     CL**3
   270*                                                              *  ELCDCS
   271*           THIS SECTION CALLS THE DATE CONVERSION SUBROUTINE. *  ELCDCS
   272****************************************************************. ELCDCS
   273                                                                  ELCDCS
   274 8510-DATE-CONVERSION.                                            ELCDCS
   275  ACCEPT SUNBTIME FROM TIME.
   276     ADD 1 TO ELDATCX-CTR ALLCALL-CTR.
   277     CALL 'SUNATCX' USING DATE-CONVERSION-DATA.                   ELCDCS
   278  ACCEPT SUNETIME FROM TIME.
   279  IF SUNETIME NOT EQUAL SUNBTIME
   280  ADD 1 TO DIFFTME-CTR.
   281**DISPLAY "SUNPSD -> SUNATCX " SUNBTIME " " SUNETIME.
   282                                                                  ELCDCS
   283 8590-EXIT.                                                       ELCDCS
   284     EXIT.                                                        ELCDCS
   285                                                                  ELCDCS
   286
   287 0409-EXIT.
   288     EXIT.
