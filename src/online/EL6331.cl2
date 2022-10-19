00001  IDENTIFICATION DIVISION.                                         02/27/96
00002                                                                   EL6331
00003  PROGRAM-ID.                 EL6331.                                 LV008
00004 *              PROGRAM CONVERTED BY                                  CL**7
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**7
00006 *              CONVERSION DATE 02/12/96 09:49:43.                    CL**7
00007 *                            VMOD=2.008                              CL**8
00008 *                                                                 EL6331
00008 *                                                                 EL6331
00009 *AUTHOR.        LOGIC,INC.                                           CL**7
00010 *               DALLAS, TEXAS.                                       CL**7
00011                                                                   EL6331
00012 *DATE-COMPILED.                                                      CL**7
00013                                                                   EL6331
00014 *SECURITY.   *****************************************************   CL**7
00015 *            *                                                   *   CL**7
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**7
00017 *            *                                                   *   CL**7
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**7
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**7
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**7
00021 *            *                                                   *   CL**7
00022 *            *****************************************************   CL**7
00023 *                                                                 EL6331
00024 *REMARKS.                                                            CL**4
00025 *        TRANSACTION - EXB8 - COMPENSATION PAYMENTS/ADJUSTMENTS.     CL**4
00023 *                                                                 EL6331
101101******************************************************************
101101*                   C H A N G E   L O G
101101*
101101* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101101*-----------------------------------------------------------------
101101*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101101* EFFECTIVE    NUMBER
101101*-----------------------------------------------------------------
101101* 101101    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
101101*                              ADJUSTED REDEFINES EL633BI FILLER
010803* 010803                   PEMA  ADD 1825013200 FOR DCC
042303* 042303                   PEMA ADD PROCESSING FOR DUE PREM ADJS
022504* 022504                   PEMA ADD GL CODE FOR DCC
093004* 093004                   PEMA ADD NEW GL CODE FOR DCC
120204* 120204                   PEMA NOTICED LOW VALS IN BATCH REPORTS
060205* 060205                   PEMA ADD ERCOMP TYPE TO ERPYAJ
061605* 061605    2005051300001  PEMA ADD GL NUM EDIT FOR DCC
122105* 122105    2005033100001  PEMA ADD GL NUM EDIT FOR CSI
032806* 032806                   PEMA ADD MORE GL NUMBERS FOR CSI
071806* 071806    2006012600002  PEMA CHANGE DCC GL NUMBERS
080206* 080206    2006012600002  PEMA ADD GL NUMBER FOR CID & DCC
092506* 092506                   PEMA ADD NEW GL CODE FOR DCC
031909* 031909    2009030300001  AJRA ADD NEW GL CODE FOR CID
040109* 040109    2008050500001  AJRA ADD GL NUMBERS FOR CCC
031710* 031710  CR2009100700001  PEMA ADD RUNNING NET TOTAL
120711* 120711  CR2011120100004  PEMA ADD GL NUMBER FOR CCC
031912* 031912  CR2011120900003  AJRA AHL COMPANY CODE
100713* 100713  CR2013100700001  AJRA ADD 1825091000 FOR CID
081414* 081414    2014012300001  PEMA  ADD PROCESSING FOR CARRIER 7 DCC
110315* 110315  CR2015101400001  PEMA ADD NEW DCC G/L #'S FOR ACH
111715* 111715  IR2015111600001  PEMA  CHG G/L ACCTNO ON CID ACH
021716* 021716  CR2016021000003  PEMA  ADD NEW G/L FOR MACHENS ACCTS
111016* 111016  CR2016110900002  TANA  REPLACE GL NUMBERS FOR CCC
060817* 060817  CR2017060700005  PEMA  ADD NEW G/L FOR VPP
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
101101******************************************************************
00026                                                                   EL6331
00027  ENVIRONMENT DIVISION.                                            EL6331
00028  DATA DIVISION.                                                   EL6331
00029  EJECT                                                            EL6331
00030  WORKING-STORAGE SECTION.                                         EL6331
00031  77  FILLER  PIC X(32)  VALUE '********************************'. EL6331
00032  77  FILLER  PIC X(32)  VALUE '*    EL6331 WORKING STORAGE    *'. EL6331
00033  77  FILLER  PIC X(32)  VALUE '************ V/M 2.008 *********'.    CL**8
CIDMOD
CIDMOD 77  HLD-XX  PIC X(32)  VALUE '********************************'.
CIDMOD 77  FILLER  PIC X(32)  VALUE '/// SAVE COMPENSATION MASTER ///'.
CIDMOD 77  SV-COMP PIC X(400) VALUE SPACE.                                   000
060205 77  C1      PIC S999 COMP-3 VALUE +0.
CIDMOD*77  FILLER  PIC X(32)  VALUE '/////// SAVE COFA MASTER ///////'.
CIDMOD*77  SV-COFA PIC X(42)  VALUE SPACE.                                   000
CIDMOD*77  K-SPACE PIC X(11)  VALUE SPACES.                                  000
00034                                                                   EL6331
00035                              COPY ELCSCTM.                           CL**5
00036                              COPY ELCSCRTY.                          CL**5
00037                                                                   EL6331
CIDMOD                                                                       000
CIDMOD*01  FORCE-DUMP-X            PIC X(1)        VALUE SPACE.              000
CIDMOD*01  FORCE-DUMP  REDEFINES FORCE-DUMP-X                                000
CIDMOD*                            PIC S9.                                   000
00038     EJECT                                                         EL6331
00039                                                                   EL6331
00040  01  STANDARD-AREAS.                                              EL6331
00041      12  GETMAIN-SPACE       PIC  X          VALUE SPACE.         EL6331
00042      12  MAP-NAME            PIC  X(8)       VALUE 'EL633B '.        CL**2
00043      12  MAPSET-NAME         PIC  X(8)       VALUE 'EL6331S'.     EL6331
00044      12  SCREEN-NUMBER       PIC  X(4)       VALUE '633B'.        EL6331
00045      12  TRANS-ID            PIC  X(4)       VALUE 'EXB8'.        EL6331
00046      12  THIS-PGM            PIC  X(8)       VALUE 'EL6331'.      EL6331
00047      12  PGM-NAME            PIC  X(8).                           EL6331
00048      12  TIME-IN             PIC S9(7).                           EL6331
00049      12  TIME-OUT-R  REDEFINES  TIME-IN.                          EL6331
00050          16  FILLER          PIC  X.                              EL6331
00051          16  TIME-OUT        PIC  9(2)V9(2).                      EL6331
00052          16  FILLER          PIC  X(2).                           EL6331
00053      12  XCTL-005            PIC  X(8)       VALUE 'EL005'.       EL6331
00054      12  XCTL-010            PIC  X(8)       VALUE 'EL010'.       EL6331
00055      12  XCTL-626            PIC  X(8)       VALUE 'EL626'.       EL6331
00056      12  XCTL-652            PIC  X(8)       VALUE 'EL652'.       EL6331
00057      12  LINK-001            PIC  X(8)       VALUE 'EL001'.       EL6331
00058      12  LINK-004            PIC  X(8)       VALUE 'EL004'.       EL6331
00059      12  LINK-CLDATCV        PIC  X(8)       VALUE 'ELDATCV'.     EL6331
00060      12  PYAJ-FILE-ID        PIC  X(8)       VALUE 'ERPYAJ'.      EL6331
00061      12  COMP-FILE-ID        PIC  X(8)       VALUE 'ERCOMP'.      EL6331
00062      12  WS-CURRENT-DT       PIC  X(8)       VALUE SPACES.        EL6331
00063      12  WS-CURRENT-BIN-DT   PIC  X(2)       VALUE SPACES.        EL6331
00064      12  WORK-SEQ-NO         PIC S9(9)                  COMP-3.   EL6331
031710     12  TOTAL-AMOUNT        PIC S9(9)V99      VALUE ZEROS.       01960007
00065      12  CHECK-REC-TYPE      PIC  X          VALUE SPACE.         EL6331
CIDMOD         88  VALID-REC-TYPE                  VALUE  'R' 'D' 'C'        000
CIDMOD                                                    'S' 'T' 'U'        000
CIDMOD                                                    'X' 'Y' 'Z'        000
CIDMOD                                                    'F'.               000
CIDMOD     12  CHECK-CANC-TYPE         PIC X       VALUE SPACE.              000
CIDMOD         88  VALID-CANC-TYPE                 VALUE 'N' 'Y'.            000
00070      12  DEEDIT-FIELD            PIC X(11).                          CL**2
CIDMOD     12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD PIC S9(9)V99.          000
CIDMOD     12  WS-EDITED-AMTS OCCURS 15 TIMES                                000
00072                                 INDEXED BY WS-INDX.                  CL**2
00073          16  WS-EDITED-AMT       PIC S9(9)V99.                       CL**2
CIDMOD*    12  COFA-FILE-ID            PIC  X(8)   VALUE 'COFAXXX'.          000
CIDMOD*01  FILLER                      PIC  X(15)                            000
CIDMOD*                                VALUE '* ACCT BREAK  *'.              000
CIDMOD*01  WS-ACCT-BREAK.                                                    000
CIDMOD*    05  WS-ACCT-1               PIC  X.                               000
CIDMOD*    05  FILLER                  PIC  X(5).                            000
CIDMOD                                                                       000

060205 01  WS-ERCOMP-TYPE-TYPE.
060205     12  WS-ERCOMP-TYPE OCCURS 15   PIC X.

CIDMOD******************************************************************000
CIDMOD*                      TABLE OF STATE NAMES                           *000
CIDMOD******************************************************************000
CIDMOD                                                                       000
CIDMOD 01  CHECK-STATE-CODE            PIC  XX     VALUE SPACE.              000
CIDMOD     88  VALID-STATE-CODE        VALUE 'AK' 'AL' 'AR' 'AZ' 'CA'   000
CIDMOD                                       'CD' 'CO' 'CT' 'DC' 'DE'   000
CIDMOD                                       'FL' 'GA' 'GM' 'HI' 'IA'   000
CIDMOD                                       'ID' 'IL' 'IN' 'KS' 'KY'   000
CIDMOD                                       'LA' 'MA' 'MD' 'ME' 'MI'   000
CIDMOD                                       'MN' 'MO' 'MS' 'MT' 'MX'   000
CIDMOD                                       'NC' 'ND' 'NE' 'NH' 'NJ'   000
CIDMOD                                       'NM' 'NV' 'NY' 'OF' 'OH'   000
CIDMOD                                       'OK' 'OR' 'PA' 'PI' 'PR'   000
CIDMOD                                       'RI' 'SC' 'SD' 'TN' 'TX'   000
CIDMOD                                       'UT' 'VA' 'VI' 'VT' 'WA'   000
CIDMOD                                       'WI' 'WV' 'WY'.                 000
CIDMOD
CIDMOD 01  CHECK-GL-ACCT         PIC X(10)  VALUE SPACE.
CIDMOD     88  VALID-GL-ACCOUNT  VALUE '1108121010'
111715                                 '1108124700'
CIDMOD                                 '1108125100'
CIDMOD                                 '1721211400'
CIDMOD                                 '1825011200'
CIDMOD                                 '1825011300'
100713                                 '1825091000'
CIDMOD                                 '1825099050'
031909                                 '8505700033'
CIDMOD                                 '8506400030'
CIDMOD                                 '8507200020' 
080206                                 '8507200010'
021716                                 '2725010160'.

071806     88  VALID-DCC-GL-ACCOUNT  VALUE '2725040300'
071806                                     '2725040320'
071806                                     '2725040330'
071806                                     '2725040310'
071806                                     '8506400030'
080206                                     '8507200010'
092506                                     '1108121250'.

111016*    88  VALID-CSI-GL-ACCOUNT  VALUE '2725040100'
111016*                                    '2725040110'
111016*                                    '2725040120'
111016*                                    '2725040130'
111016*                                    '2725020400'.
111016     88  VALID-CSI-GL-ACCOUNT  VALUE '1108121010'
111016                                     '1825013200'
111016                                     '1825013300'
111016                                     '1825013400'.

040109
040109     88  VALID-CCC-GL-ACCOUNT  VALUE '1825013100'
040109                                     '1825013200'
040109                                     '1825013300'
040109                                     '1825013400'
040109                                     '8506400030'
040109                                     '8507200010'
040109                                     '8507200020'
120711                                     '1108121010'.

           88  VALID-VPP-GL-ACCOUNT  VALUE '2725040510'
                                           '2725040520'
060817                                     '7206100400'
060817                                     '7206104100'.

062121     88  VALID-FNL-GL-ACCOUNT  VALUE '1108121010'
062121                                     '1721211400'
062121                                     '1825011100'
062121                                     '1825011200'
062121                                     '1825011300'
062121                                     '1825099050'
062121                                     '2718000110'
062121                                     '2718000120'
062121                                     '8506400030'
062121                                     '8507200010'.

00074                                                                   EL6331
00075  01  ACCESS-KEYS.                                                 EL6331
00076      12  ERPYAJ-KEY.                                              EL6331
00077          16  PYAJ-COMP-CD        PIC  X      VALUE SPACE.         EL6331
00078          16  PYAJ-CARRIER        PIC  X      VALUE SPACES.        EL6331
00079          16  PYAJ-GROUPING       PIC  X(6)   VALUE SPACES.        EL6331
00080          16  PYAJ-FIN-RESP       PIC  X(10)  VALUE SPACES.        EL6331
00081          16  PYAJ-ACCOUNT        PIC  X(10)  VALUE SPACES.        EL6331
00082          16  PYAJ-FILE-SEQ-NO    PIC S9(8)   VALUE +0   COMP.     EL6331
00083          16  PYAJ-RECORD-TYPE    PIC  X      VALUE SPACES.        EL6331
00084                                                                      CL**2
00085      12  ERPYAJ-RECORD-LENGTH    PIC S9(4)   VALUE +200 COMP.        CL**2
00086      12  ERPYAJ-JOURNAL-LENGTH   PIC S9(4)   VALUE +223 COMP.        CL**2
00087                                                                      CL**2
00088      12  ERCOMP-KEY.                                              EL6331
00089          16  COMP-COMP-CD        PIC  X      VALUE SPACE.         EL6331
00090          16  COMP-CARRIER        PIC  X      VALUE SPACES.        EL6331
00091          16  COMP-GROUPING       PIC  X(6)   VALUE SPACES.        EL6331
00092          16  COMP-FIN-RESP       PIC  X(10)  VALUE SPACES.        EL6331
00093          16  COMP-ACCOUNT        PIC  X(10)  VALUE SPACES.        EL6331
00094          16  COMP-RECORD-TYPE    PIC  X      VALUE SPACES.        EL6331
CIDMOD*    12  COFA-KEY-X.                                                   000
CIDMOD*        16  COFA-COMPANY-X      PIC  X(4)   VALUE SPACES.             000
CIDMOD*        16  COFA-ACCOUNT.                                             000
CIDMOD*            20  COFA-FILLER     PIC  X(11)  VALUE SPACES.             000
CIDMOD*            20  COFA-MSA-ACCT   PIC  X(07)  VALUE SPACES.             000
CIDMOD*                                                                      000
00095  EJECT                                                            EL6331
00096  01  ERROR-NUMBERS.                                               EL6331
00097      12  ER-0000             PIC  X(4)       VALUE '0000'.        EL6331
00098      12  ER-0008             PIC  X(4)       VALUE '0008'.        EL6331
00099      12  ER-0029             PIC  X(4)       VALUE '0029'.        EL6331
00100      12  ER-0070             PIC  X(4)       VALUE '0070'.        EL6331
00101      12  ER-0194             PIC  X(4)       VALUE '0194'.        EL6331
00102      12  ER-0195             PIC  X(4)       VALUE '0195'.        EL6331
00103      12  ER-0197             PIC  X(4)       VALUE '0197'.        EL6331
00104      12  ER-2230             PIC  X(4)       VALUE '2230'.        EL6331
00105      12  ER-2232             PIC  X(4)       VALUE '2232'.        EL6331
00106      12  ER-2233             PIC  X(4)       VALUE '2233'.        EL6331
00107      12  ER-2234             PIC  X(4)       VALUE '2234'.        EL6331
00108      12  ER-2235             PIC  X(4)       VALUE '2235'.        EL6331
00109      12  ER-2236             PIC  X(4)       VALUE '2236'.        EL6331
00110      12  ER-2245             PIC  X(4)       VALUE '2245'.        EL6331
00111      12  ER-2562             PIC  X(4)       VALUE '2562'.        EL6331
00112      12  ER-2587             PIC  X(4)       VALUE '2587'.        EL6331
00113      12  ER-2588             PIC  X(4)       VALUE '2588'.        EL6331
00114      12  ER-2595             PIC  X(4)       VALUE '2595'.        EL6331
00115      12  ER-2596             PIC  X(4)       VALUE '2596'.        EL6331
00116      12  ER-2763             PIC  X(4)       VALUE '2763'.           CL**6
CIDMOD     12  ER-2956             PIC  X(4)       VALUE '2956'.             000
CIDMOD     12  ER-2957             PIC  X(4)       VALUE '2957'.             000
CIDMOD     12  ER-2958             PIC  X(4)       VALUE '2958'.             000
CIDMOD     12  ER-2959             PIC  X(4)       VALUE '2959'.             000
CIDMOD     12  ER-2960             PIC  X(4)       VALUE '2960'.             000
CIDMOD     12  ER-2961             PIC  X(4)       VALUE '2961'.             000
CIDMOD     12  ER-9030             PIC  X(4)       VALUE '9030'.             000
CIDMOD                                                                       000
00117  EJECT                                                            EL6331
00118                                      COPY ELCDATE.                   CL**5
00119  EJECT                                                            EL6331
00120                                      COPY ELCLOGOF.                  CL**5
00121  EJECT                                                            EL6331
00122                                      COPY ELCATTR.                   CL**5
00123  EJECT                                                            EL6331
00124                                      COPY ELCEMIB.                   CL**5
00125  EJECT                                                            EL6331
00126                                      COPY ELCINTF.                   CL**5
00127      12  FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.                 EL6331
00128          16  PI-PYAJ-FILE-SW         PIC  X.                      EL6331
00129              88  END-OF-ACCT                 VALUE 'A'.           EL6331
00130              88  END-OF-FILE                 VALUE 'X'.           EL6331
00131              88  NO-RECORDS                  VALUE 'Y'.           EL6331
00132              88  NOT-OPEN                    VALUE 'Z'.           EL6331
00133          16  PI-PREV-FUNCTION        PIC  X.                      EL6331
00134          16  PI-SAV-FUNCTION         PIC  X.                      EL6331
00135          16  PI-SEQ-NOS.                                          EL6331
00136              20  FILLER  OCCURS 13 TIMES                          EL6331
00137                              INDEXED BY NDX.                      EL6331
00138                  24  PI-REC-TYPE     PIC  X.                      EL6331
00139                  24  PI-FILE-SEQ-NO  PIC S9(8).                   EL6331
00140          16  PI-SAV-ENDING-PYAJ-KEY.                              EL6331
00141              20  PI-SAV-COMP-CD      PIC  X.                      EL6331
00142              20  PI-SAV-CARRIER      PIC  X.                      EL6331
00143              20  PI-SAV-GROUPING     PIC  X(3).                   EL6331
00144              20  PI-SAV-FIN-RESP     PIC  X(6).                   EL6331
00145              20  PI-SAV-ACCOUNT      PIC  X(6).                   EL6331
00146              20  PI-SAV-FILE-SEQ-NO  PIC S9(8)          COMP.     EL6331
00147              20  PI-SAV-RECORD-TYPE  PIC  X.                      EL6331
00148          16  FILLER                  PIC  X(498).                    CL**7
00149  EJECT                                                            EL6331
00150                              COPY ELCJPFX.                           CL**5
00151                              PIC  X(223).                            CL**2
00152  EJECT                                                            EL6331
00153                              COPY ELCAID.                            CL**5
00154                                                                   EL6331
00155  01  FILLER    REDEFINES DFHAID.                                  EL6331
00156      12  FILLER              PIC  X(8).                           EL6331
00157      12  PF-VALUES           PIC  X          OCCURS 2 TIMES.      EL6331
00158  EJECT                                                            EL6331
00159                              COPY EL6331S.                           CL**5
00160                                                                   EL6331
00161  01  MAP-EL633B   REDEFINES  EL633BI.                                CL**2
101101     12  FILLER                  PIC  X(44).                      EL6331
060205*    12  DATA-AREA       OCCURS  6 TIMES                             CL**2
060205     12  DATA-AREA       OCCURS 15 TIMES
00164                              INDEXED BY INDX.                     EL6331
00165          16  CARR-LEN            PIC S9(4)              COMP.     EL6331
00166          16  CARR-ATTRB          PIC  X.                          EL6331
00167          16  CARRIER             PIC  X.                          EL6331
00168          16  GRP-LEN             PIC S9(4)              COMP.     EL6331
00169          16  GRP-ATTRB           PIC  X.                          EL6331
00170          16  GROUPING            PIC  X(6).                       EL6331
00171          16  FIN-LEN             PIC S9(4)              COMP.     EL6331
00172          16  FIN-ATTRB           PIC  X.                          EL6331
00173          16  FIN-RESP            PIC  X(10).                      EL6331
00174          16  ACCT-LEN            PIC S9(4)              COMP.     EL6331
00175          16  ACCT-ATTRB          PIC  X.                          EL6331
00176          16  ACCT                PIC  X(10).                      EL6331
00177          16  RTYPE-LEN           PIC S9(4)              COMP.     EL6331
00178          16  RTYPE-ATTRB         PIC  X.                          EL6331
00179          16  RTYPE               PIC  X.                          EL6331
00180          16  AMT-LEN             PIC S9(4)              COMP.     EL6331
00181          16  AMT-ATTRB           PIC  X.                          EL6331
00182          16  AMT                 PIC S9(9)V99.                       CL**8
00183          16  AMTO  REDEFINES                                      EL6331
00184              AMT                 PIC Z(7).9(2)-.                     CL**2
CIDMOD         16  GL-ACCT-LEN         PIC S9(4)              COMP.          000
CIDMOD         16  GL-ACCT-ATTRB       PIC  X.                               000
CIDMOD         16  GL-ACCT             PIC  X(10).                           000
CIDMOD         16  WSL-COMM  REDEFINES GL-ACCT.                              000
CIDMOD             20  WSL-COMM-DTE.
CIDMOD                 24  WSL-MO      PIC  X(2).
CIDMOD                 24  WSL-DA      PIC  X(2).
CIDMOD                 24  WSL-YR      PIC  X(2).
CIDMOD             20  FILLER          PIC  X(4).
CIDMOD         16  GL-STATE-LEN        PIC S9(4)              COMP.          000
CIDMOD         16  GL-STATE-ATTRB      PIC  X.                               000
CIDMOD         16  GL-STATE            PIC  X(02).                           000
CIDMOD         16  GL-CANC-LEN         PIC S9(4)              COMP.          000
CIDMOD         16  GL-CANC-ATTRB       PIC  X.                               000
CIDMOD         16  GL-CANC             PIC  X(01).                           000
CIDMOD         16  GL-COMM-LEN         PIC S9(4)              COMP.          000
CIDMOD         16  GL-COMM-ATTRB       PIC  X.                               000
CIDMOD         16  GL-COMM             PIC  X(10).                           000
CIDMOD         16  FILLER  REDEFINES GL-COMM.                                000
CIDMOD             20  GL-CHECK-NO     PIC  9(06).                           000
CIDMOD             20  FILLER          PIC  X(04).
031710     12  MSG1-LEN                PIC S9(4)  COMP.
031710     12  MSG1-ATTRB              PIC X.
031710     12  MSG1                    PIC X(79).
031710     12  MSG2-LEN                PIC S9(4)  COMP.
031710     12  MSG2-ATTRB              PIC X.
031710     12  MSG2                    PIC X(79).
031710     12  FILLER                  PIC X(5).
031710     12  GAMT-LEN                PIC S9(4)  COMP.
031710     12  GAMT-ATTRB              PIC X.
031710     12  GROSS-AMT               PIC S9(9)V99.
031710     12  GROSS-AMTO REDEFINES GROSS-AMT
031710                                 PIC Z(7).9(2)-.
CIDMOD                                                                       000
CIDMOD/                                                                 EL6331
00196  LINKAGE SECTION.                                                 EL6331
00197  01  DFHCOMMAREA             PIC  X(1024).                        EL6331
00198  EJECT                                                            EL6331
00204                              COPY ERCPYAJ.                           CL**5
00205  EJECT                                                            EL6331
00206                              COPY ERCCOMP.                           CL**5
00207  EJECT                                                            EL6331
CIDMOD*                                                                      000
CIDMOD*                            COPY AIRL0009.                            000
CIDMOD/                                                                      000
00208  PROCEDURE DIVISION.                                              EL6331
00209                                                                   EL6331
00210      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL6331
00211      MOVE 2                      TO  EMI-NUMBER-OF-LINES.         EL6331
00212                                                                   EL6331
00213      IF EIBCALEN = ZERO                                           EL6331
00214          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL6331
00215                                                                   EL6331
00216      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL6331
00217      MOVE '5'                    TO  DC-OPTION-CODE.              EL6331
00218                                                                   EL6331
00219      PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.                  EL6331
00220                                                                   EL6331
00221      MOVE DC-BIN-DATE-1          TO  WS-CURRENT-BIN-DT.           EL6331
00222      MOVE DC-GREG-DATE-1-EDIT    TO  WS-CURRENT-DT.               EL6331
00223                                                                   EL6331
00224      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL6331
00225          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL6331
00226              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6    EL6331
00227              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5    EL6331
00228              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4    EL6331
00229              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3    EL6331
00230              MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2    EL6331
00231              MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1    EL6331
00232              MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM  EL6331
00233              MOVE THIS-PGM              TO  PI-CALLING-PROGRAM    EL6331
00234          ELSE                                                     EL6331
00235              MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM    EL6331
00236              MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM  EL6331
00237              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1    EL6331
00238              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2    EL6331
00239              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3    EL6331
00240              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4    EL6331
00241              MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5    EL6331
00242              MOVE SPACES                TO  PI-SAVED-PROGRAM-6.   EL6331
00243                                                                   EL6331
00244      MOVE LOW-VALUES             TO  EL633BI.                        CL**2
00245                                                                   EL6331
00246      COMPUTE WORK-SEQ-NO  =  EIBTIME  *  10.                      EL6331
00247                                                                   EL6331
00248      IF EIBTRNID NOT = TRANS-ID                                   EL6331
00249          MOVE SPACE              TO  PI-PYAJ-FILE-SW              EL6331
00250          GO TO 8100-SEND-INITIAL-MAP.                             EL6331
00251                                                                   EL6331
00252      EXEC CICS HANDLE CONDITION                                   EL6331
00253          PGMIDERR  (9600-PGMID-ERROR)                             EL6331
00254          ERROR     (9990-ABEND)                                   EL6331
00255          END-EXEC.                                                EL6331
00256                                                                   EL6331
00257      IF EIBAID = DFHCLEAR                                         EL6331
00258          GO TO 9400-CLEAR.                                        EL6331
00259  EJECT                                                            EL6331
00260  0200-RECEIVE.                                                    EL6331
00261      IF EIBAID = DFHPA1              OR  DFHPA2  OR  DFHPA3       EL6331
00262          MOVE ER-0008            TO  EMI-ERROR                    EL6331
00263          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL6331
00264          MOVE -1                 TO  PFENTERL                     EL6331
00265          GO TO 8200-SEND-DATAONLY.                                EL6331
00266                                                                   EL6331
00267      EXEC CICS RECEIVE                                            EL6331
00268          MAP     (MAP-NAME)                                       EL6331
00269          MAPSET  (MAPSET-NAME)                                    EL6331
00270          INTO    (EL633BI)                                           CL**2
00271          END-EXEC.                                                EL6331
00272                                                                   EL6331
00273      IF PFENTERL = ZERO                                           EL6331
00274          GO TO 0300-CHECK-PFKEYS.                                 EL6331
00275                                                                   EL6331
00276      IF (PFENTERI  IS NUMERIC)                                    EL6331
00277        AND (PFENTERI  IS GREATER THAN  ZERO                       EL6331
00278        AND  IS LESS THAN  25)                                     EL6331
00279          MOVE PF-VALUES (PFENTERI)  TO  EIBAID                    EL6331
00280      ELSE                                                         EL6331
00281          MOVE ER-0029               TO  EMI-ERROR                 EL6331
00282          GO TO 0320-INPUT-ERROR.                                  EL6331
00283                                                                   EL6331
00284  0300-CHECK-PFKEYS.                                               EL6331
00285      IF EIBAID = DFHPF23                                          EL6331
00286          GO TO 8810-PF23.                                         EL6331
00287                                                                   EL6331
00288      IF EIBAID = DFHPF24                                          EL6331
00289          GO TO 9200-RETURN-MAIN-MENU.                             EL6331
00290                                                                   EL6331
00291      IF EIBAID = DFHPF12                                          EL6331
00292          GO TO 9500-PF12.                                         EL6331
00293                                                                   EL6331
00294      IF EIBAID = DFHENTER                                         EL6331
00295          GO TO 1000-EDIT-DATA.                                    EL6331
CIDMOD                                                                  EL6331
CIDMOD     IF EIBAID = DFHPF1                                           EL6331
CIDMOD        GO TO 1000-EDIT-DATA.
00296                                                                   EL6331
00297  0320-INPUT-ERROR.                                                EL6331
00298      MOVE ER-0029                TO  EMI-ERROR.                   EL6331
00299                                                                   EL6331
00300      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL6331
00301                                                                   EL6331
00302      MOVE AL-UNBON               TO  PFENTERA.                    EL6331
00303                                                                   EL6331
00304      IF PFENTERL = ZERO                                           EL6331
00305          MOVE -1                 TO  PFENTERL                     EL6331
00306      ELSE                                                         EL6331
00307          MOVE -1                 TO  PFENTERL.                    EL6331
00308                                                                   EL6331
00309      GO TO 8200-SEND-DATAONLY.                                    EL6331
00310  EJECT                                                            EL6331
00311  1000-EDIT-DATA.                                                  EL6331
00312      IF NOT MODIFY-CAP                                            EL6331
00313          MOVE 'UPDATE'       TO SM-READ                           EL6331
00314          PERFORM 9995-SECURITY-VIOLATION                          EL6331
00315          MOVE ER-0070        TO EMI-ERROR                         EL6331
00316          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6331
00317          GO TO 8100-SEND-INITIAL-MAP.                             EL6331
00318                                                                   EL6331
00319      MOVE PI-COMPANY-CD          TO  PI-SAV-COMP-CD               EL6331
00320                                      COMP-COMP-CD.                EL6331
00321                                                                   EL6331
00322      SET INDX                    TO  1.                           EL6331
060205     MOVE +1                     TO C1
031710     MOVE ZEROS                  TO TOTAL-AMOUNT
00323                                                                   EL6331
           .
00324  1010-EDIT-LOOP.                                                  EL6331
00325      IF CARR-LEN (INDX) = ZEROS                                   EL6331
00326        AND GRP-LEN (INDX) = ZEROS                                 EL6331
00327        AND FIN-LEN (INDX) = ZEROS                                 EL6331
00328        AND ACCT-LEN (INDX) = ZEROS                                EL6331
CIDMOD       AND GL-ACCT-LEN (INDX) = ZEROS                                  000
CIDMOD       AND GL-STATE-LEN (INDX) = ZEROS                                 000
CIDMOD       AND GL-CANC-LEN (INDX) = ZEROS                                  000
CIDMOD       AND GL-COMM-LEN (INDX) = ZEROS                                  000
00330        AND RTYPE-LEN (INDX) = ZEROS                               EL6331
00331        AND AMT-LEN (INDX) = ZEROS                                 EL6331
00332          GO TO 1040-INCREMENT-INDX.                               EL6331
00333                                                                   EL6331
00334      IF CARR-LEN (INDX) NOT = ZEROS                               EL6331
00335          MOVE AL-UANON           TO  CARR-ATTRB (INDX)            EL6331
00336          MOVE CARRIER (INDX)     TO  COMP-CARRIER                 EL6331
00337                                      PI-SAV-CARRIER               EL6331
00338          IF CARRIER (INDX) NOT = ZEROS                            EL6331
00339            AND (PI-ZERO-CARRIER  OR  PI-ZERO-CAR-GROUP)           EL6331
00340              MOVE ER-2587        TO  EMI-ERROR                    EL6331
00341              MOVE -1             TO  CARR-LEN (INDX)              EL6331
00342              MOVE AL-UABON       TO  CARR-ATTRB (INDX)            EL6331
00343              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL6331
00344          ELSE                                                     EL6331
00345              NEXT SENTENCE                                        EL6331
00346      ELSE                                                         EL6331
00347          MOVE ER-0194            TO  EMI-ERROR                    EL6331
00348          MOVE -1                 TO  CARR-LEN (INDX)              EL6331
00349          MOVE AL-UABON           TO  CARR-ATTRB (INDX)            EL6331
00350          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.              EL6331
00351                                                                   EL6331
00352      IF GRP-LEN (INDX) NOT = ZEROS                                EL6331
00353          MOVE AL-UANON           TO  GRP-ATTRB (INDX)             EL6331
00354          MOVE GROUPING (INDX)    TO  COMP-GROUPING                EL6331
00355                                      PI-SAV-GROUPING              EL6331
00356          IF GROUPING (INDX) NOT = ZEROS                           EL6331
00357            AND (PI-ZERO-GROUPING  OR  PI-ZERO-CAR-GROUP)          EL6331
00358              MOVE ER-2588        TO  EMI-ERROR                    EL6331
00359              MOVE -1             TO  GRP-LEN (INDX)               EL6331
00360              MOVE AL-UABON       TO  GRP-ATTRB (INDX)             EL6331
00361              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL6331
00362          ELSE                                                     EL6331
00363              NEXT SENTENCE                                        EL6331
00364      ELSE                                                         EL6331
00365          MOVE ER-0195            TO  EMI-ERROR                    EL6331
00366          MOVE -1                 TO  GRP-LEN (INDX)               EL6331
00367          MOVE AL-UABON           TO  GRP-ATTRB (INDX)             EL6331
00368          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.              EL6331
00369                                                                   EL6331
00370      IF FIN-LEN (INDX) NOT = ZEROS                                EL6331
00371          MOVE AL-UANON           TO  FIN-ATTRB (INDX)             EL6331
00372          MOVE FIN-RESP (INDX)    TO  COMP-FIN-RESP                EL6331
00373                                      PI-SAV-FIN-RESP              EL6331
00374                                      PI-CR-FIN-RESP               EL6331
00375      ELSE                                                         EL6331
00376          MOVE ER-2562            TO  EMI-ERROR                    EL6331
00377          MOVE -1                 TO  FIN-LEN (INDX)               EL6331
00378          MOVE AL-UABON           TO  FIN-ATTRB (INDX)             EL6331
00379          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.              EL6331
00380                                                                   EL6331
00381      IF ACCT-LEN (INDX) NOT = ZEROS                               EL6331
00382          MOVE AL-UANON           TO  ACCT-ATTRB (INDX)            EL6331
00383          MOVE ACCT (INDX)        TO  COMP-ACCOUNT                 EL6331
00384                                      PI-SAV-ACCOUNT               EL6331
00385      ELSE                                                         EL6331
00386          MOVE LOW-VALUES         TO  COMP-ACCOUNT                 EL6331
00387                                      PI-SAV-ACCOUNT.              EL6331
00388                                                                   EL6331
00389      EXEC CICS HANDLE CONDITION                                   EL6331
00390          NOTFND   (1020-NO-COMP-MSTR)                             EL6331
00391          NOTOPEN  (7100-COMP-FILE-NOTOPEN)                        EL6331
00392          END-EXEC.                                                EL6331
00393                                                                   EL6331
00394      MOVE SPACES                 TO  COMP-RECORD-TYPE.            EL6331
00395                                                                   EL6331
00396      EXEC CICS READ                                               EL6331
00397          DATASET  (COMP-FILE-ID)                                  EL6331
00398          SET      (ADDRESS OF COMPENSATION-MASTER)                   CL**7
00399          RIDFLD   (ERCOMP-KEY)                                    EL6331
00400          GTEQ                                                     EL6331
00401          END-EXEC.                                                EL6331
00402                                                                   EL6331
CIDMOD     MOVE COMPENSATION-MASTER    TO SV-COMP.                           000
00403      IF PI-COMPANY-CD = CO-COMPANY-CD                             EL6331
00404        AND COMP-CARRIER = CO-CARRIER                              EL6331
00405        AND COMP-GROUPING = CO-GROUPING                            EL6331
00406        AND COMP-FIN-RESP = CO-RESP-NO                             EL6331
00407        AND COMP-ACCOUNT = CO-ACCOUNT                              EL6331
00408          GO TO 1025-CHECK-STATUS.                                    CL**6
00409                                                                   EL6331
00410  1020-NO-COMP-MSTR.                                               EL6331
00411      MOVE ER-2230                TO  EMI-ERROR.                   EL6331
00412      MOVE -1                     TO  CARR-LEN (INDX)              EL6331
00413      MOVE AL-UABON               TO  CARR-ATTRB (INDX)            EL6331
00414                                      GRP-ATTRB (INDX)             EL6331
00415                                      FIN-ATTRB (INDX)             EL6331
00416                                      ACCT-ATTRB (INDX)            EL6331
00417                                                                   EL6331
00418      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL6331
00419                                                                      CL**6
00420      GO TO 1030-CONTINUE-EDIT.                                       CL**6
00421                                                                      CL**6
00422  1025-CHECK-STATUS.                                                  CL**6

060205     MOVE CO-TYPE                TO WS-ERCOMP-TYPE (C1)

00423      IF PI-COMPANY-ID = 'NCL'                                        CL**6
00424          IF CO-GA-INACTIVE                                           CL**6
00425              MOVE ER-2763            TO  EMI-ERROR                   CL**6
00426              MOVE -1                 TO  CARR-LEN   (INDX)           CL**6
00427              MOVE AL-UABON           TO  CARR-ATTRB (INDX)           CL**6
00428                                          GRP-ATTRB  (INDX)           CL**6
00429                                          FIN-ATTRB  (INDX)           CL**6
00430                                          ACCT-ATTRB (INDX)           CL**6
00431              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.             CL**6
00432                                                                   EL6331
00433  1030-CONTINUE-EDIT.                                              EL6331
CIDMOD     IF GL-ACCT-LEN (INDX) NOT = ZEROS                                 000
CIDMOD         MOVE AL-UANON           TO  GL-ACCT-ATTRB (INDX)              000
00436          IF PI-COMPANY-ID NOT = 'WSL'                             EL6331
00437              NEXT SENTENCE                                        EL6331
00438          ELSE                                                     EL6331
00439              MOVE WSL-COMM-DTE (INDX)  TO  DC-GREG-DATE-1-MDY     EL6331
00440              MOVE '4'                  TO  DC-OPTION-CODE         EL6331
00441              PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT           EL6331
00442              IF NO-CONVERSION-ERROR                               EL6331
00443                  NEXT SENTENCE                                    EL6331
00444              ELSE                                                 EL6331
00445                  MOVE ER-2595    TO  EMI-ERROR                    EL6331
CIDMOD                 MOVE -1         TO  GL-ACCT-LEN (INDX)                000
CIDMOD                 MOVE AL-UABON   TO  GL-ACCT-ATTRB (INDX)              000
00448                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT       EL6331
00449      ELSE                                                         EL6331
00450          IF PI-COMPANY-ID = 'WSL'                                 EL6331
00451              MOVE ER-2596        TO  EMI-ERROR                    EL6331
CIDMOD             MOVE -1             TO  GL-ACCT-LEN (INDX)                000
CIDMOD             MOVE AL-UABON       TO  GL-ACCT-ATTRB (INDX)              000
00454              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL6331
00455                                                                   EL6331
00456      IF RTYPE-LEN (INDX) NOT = ZEROS                              EL6331
00457          MOVE RTYPE (INDX)       TO  CHECK-REC-TYPE               EL6331
021716         IF (PI-COMPANY-ID = 'DCC' or 'VPP')
042303            AND (RTYPE (INDX) = 'P')
042303            SET VALID-REC-TYPE   TO TRUE
042303         END-IF
00458          IF NOT VALID-REC-TYPE
00459              MOVE -1             TO  RTYPE-LEN (INDX)             EL6331
00460              MOVE ER-2234        TO  EMI-ERROR                    EL6331
00461              MOVE AL-UABON       TO  RTYPE-ATTRB (INDX)           EL6331
00462              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL6331
00463          ELSE                                                     EL6331
00464              MOVE AL-UANON       TO  RTYPE-ATTRB (INDX)           EL6331
00465      ELSE                                                         EL6331
00466          MOVE -1                 TO  RTYPE-LEN (INDX)             EL6331
00467          MOVE ER-2235            TO  EMI-ERROR                    EL6331
00468          MOVE AL-UABON           TO  RTYPE-ATTRB (INDX)           EL6331
00469          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.              EL6331
00470                                                                   EL6331
00471      IF AMT-LEN (INDX) NOT = ZEROS                                EL6331
00472          MOVE AL-UNNON           TO  AMT-ATTRB (INDX)             EL6331
00473          EXEC CICS BIF DEEDIT
00474               FIELD (AMT (INDX))
00475               LENGTH (11)
00476          END-EXEC
00477          IF AMT (INDX) = ZEROS                                         000
00478              MOVE ER-2245        TO  EMI-ERROR                    EL6331
00479              MOVE -1             TO  AMT-LEN(INDX)                EL6331
00480              MOVE AL-UNBON       TO  AMT-ATTRB (INDX)             EL6331
00481              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL6331
00482          ELSE                                                     EL6331
00483              SET WS-INDX          TO INDX                            CL**2
00484              MOVE AMT(INDX)      TO WS-EDITED-AMT (WS-INDX)            000
031710             IF RTYPE (INDX) = 'R' OR 'D' OR 'S' OR 'Z'
031710                ADD WS-EDITED-AMT (WS-INDX)
031710                                 TO TOTAL-AMOUNT
031710             ELSE
031710                SUBTRACT WS-EDITED-AMT (WS-INDX)
031710                                 FROM TOTAL-AMOUNT
031710             END-IF
CIDMOD         END-IF                                                        000
00485      ELSE                                                         EL6331
00486          MOVE -1                 TO  AMT-LEN (INDX)               EL6331
00487          MOVE ER-2236            TO  EMI-ERROR                    EL6331
00488          MOVE AL-UNBON           TO  AMT-ATTRB (INDX)             EL6331
00489          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.              EL6331
00490                                                                   EL6331
021716     IF (PI-COMPANY-ID = 'DCC' or 'VPP')
042303        AND (RTYPE (INDX) = 'P')
042303        CONTINUE
042303     ELSE
CIDMOD        IF GL-ACCT-LEN (INDX) = ZEROES
CIDMOD           MOVE -1               TO GL-ACCT-LEN (INDX)
CIDMOD           MOVE ER-2956          TO EMI-ERROR
CIDMOD           MOVE AL-UABON         TO GL-ACCT-ATTRB (INDX)
CIDMOD           PERFORM 9900-ERROR-FORMAT
CIDMOD                                 THRU 9900-EXIT
CIDMOD           GO TO 1040-CONTINUE-EDIT
042303        END-IF
042303     END-IF

120711     IF GL-ACCT-LEN (INDX) NOT = ZEROS
120711        MOVE GL-ACCT (INDX)      TO CHECK-GL-ACCT
120711     ELSE
120711        MOVE SPACES              TO CHECK-GL-ACCT
120711     END-IF
120711
120711     EVALUATE TRUE
021716        WHEN (PI-COMPANY-ID = 'DCC')
110315           AND (CO-CARRIER = '1' OR '2' or '9')
120711           AND (VALID-DCC-GL-ACCOUNT)
120711           GO TO 1040-CONTINUE-EDIT
021716        WHEN (PI-COMPANY-ID = 'DCC')
120711           AND (CO-CARRIER = '3' OR '4')
120711           AND VALID-CSI-GL-ACCOUNT
120711           GO TO 1040-CONTINUE-EDIT
021716        WHEN (PI-COMPANY-ID = 'DCC')
081414           AND (CO-CARRIER = '5' OR '6' or '7')
120711           AND (VALID-CCC-GL-ACCOUNT)
120711           GO TO 1040-CONTINUE-EDIT
062121        WHEN (PI-COMPANY-ID = 'CID' OR 'AHL')
120711           AND (VALID-GL-ACCOUNT)
120711           GO TO 1040-CONTINUE-EDIT
031912        WHEN (PI-COMPANY-ID = 'VPP')
120711           AND (VALID-VPP-GL-ACCOUNT)
120711           GO TO 1040-CONTINUE-EDIT
062121        WHEN (PI-COMPANY-ID = 'FNL')
062121           AND (VALID-FNL-GL-ACCOUNT)
062121           GO TO 1040-CONTINUE-EDIT
120711     END-EVALUATE

CIDMOD*    EXEC CICS HANDLE CONDITION                                        000
CIDMOD*         NOTFND (1040-NO-COFA-MSTR)                                   000
CIDMOD*         NOTOPEN (7100-COFA-FILE-NOTOPEN)                             000
CIDMOD*    END-EXEC.                                                         000
CIDMOD*                                                                      000
CIDMOD*    EXEC CICS READ                                                    000
CIDMOD*         DATASET (COFA-FILE-ID)                                       000
CIDMOD*         SET     (ADDRESS OF CHART-OF-ACCOUNTS)                       000
CIDMOD*         RIDFLD  (COFA-KEY-X)                                         000
CIDMOD*    END-EXEC.                                                         000
CIDMOD*                                                                      000
CIDMOD*    MOVE CHART-OF-ACCOUNTS      TO SV-COFA.                           000
CIDMOD*    GO TO 1040-CONTINUE-EDIT.                                         000

           .
CIDMOD 1040-NO-COFA-MSTR.                                                    000
CIDMOD                                                                       000
CIDMOD     MOVE ER-2960                TO EMI-ERROR.                         000
CIDMOD     MOVE -1                     TO GL-ACCT-LEN (INDX).                000
CIDMOD     MOVE AL-UABON               TO GL-ACCT-ATTRB (INDX).              000
CIDMOD     PERFORM 9900-ERROR-FORMAT THRU                                    000
CIDMOD             9900-EXIT.                                                000
CIDMOD                                                                       000
CIDMOD 1040-CONTINUE-EDIT.                                                   000
CIDMOD                                                                       000
CIDMOD*    MOVE MSA-ACCT (INDX)        TO WS-ACCT-BREAK.                     000
CIDMOD     IF GL-STATE-LEN (INDX) NOT EQUAL ZEROS                            000
CIDMOD         MOVE GL-STATE (INDX)    TO CHECK-STATE-CODE                   000
CIDMOD         IF NOT VALID-STATE-CODE                                       000
CIDMOD             MOVE -1             TO GL-STATE-LEN (INDX)                000
CIDMOD             MOVE ER-2957        TO EMI-ERROR                          000
CIDMOD             MOVE AL-UABON       TO GL-STATE-ATTRB (INDX)              000
CIDMOD             PERFORM 9900-ERROR-FORMAT THRU                            000
CIDMOD                     9900-EXIT                                         000
CIDMOD         ELSE                                                          000
CIDMOD             MOVE AL-UANON       TO GL-STATE-ATTRB (INDX)              000
CIDMOD         END-IF                                                        000
CIDMOD     END-IF.                                                           000
CIDMOD                                                                       000
CIDMOD 1040-CSO-SKIP.                                                        000
CIDMOD                                                                       000
CIDMOD     IF GL-CANC (INDX) = LOW-VALUES
CIDMOD         MOVE SPACES             TO GL-CANC (INDX)
CIDMOD     END-IF

CIDMOD     IF GL-CANC-LEN (INDX) NOT EQUAL ZEROS                             000
CIDMOD         MOVE GL-CANC (INDX)     TO CHECK-CANC-TYPE                    000
CIDMOD         IF NOT VALID-CANC-TYPE                                        000
CIDMOD             MOVE -1             TO GL-CANC-LEN (INDX)                 000
CIDMOD             MOVE ER-2958        TO EMI-ERROR                          000
CIDMOD             MOVE AL-UABON       TO GL-CANC-ATTRB (INDX)               000
CIDMOD             PERFORM 9900-ERROR-FORMAT THRU                            000
CIDMOD                     9900-EXIT                                         000
CIDMOD         ELSE                                                          000
CIDMOD             MOVE AL-UANON       TO GL-CANC-ATTRB (INDX)               000
CIDMOD         END-IF                                                        000
CIDMOD     END-IF.                                                           000
CIDMOD                                                                       000
CIDMOD     IF (CARRIER (INDX) EQUAL '6' AND                                  000
CIDMOD         GL-ACCT (INDX) EQUAL '1082202')                               000
CIDMOD         CONTINUE                                                      000
CIDMOD     ELSE                                                              000
CIDMOD         GO TO 1040-INCREMENT-INDX                                     000
CIDMOD     END-IF.                                                           000
CIDMOD                                                                       000
CIDMOD     IF GL-CHECK-NO (INDX) NOT NUMERIC                                 000
CIDMOD         MOVE -1                 TO GL-COMM-LEN (INDX)                 000
CIDMOD         MOVE ER-2961            TO EMI-ERROR                          000
CIDMOD         MOVE AL-UABON           TO GL-COMM-ATTRB (INDX)               000
CIDMOD         PERFORM 9900-ERROR-FORMAT THRU                                000
CIDMOD                 9900-EXIT                                             000
CIDMOD     END-IF.                                                           000
CIDMOD                                                                       000
00491  1040-INCREMENT-INDX.                                             EL6331
00492      SET INDX  UP  BY  1.                                         EL6331

060205     ADD +1                      TO C1

00493                                                                   EL6331
CIDMOD     IF INDX  IS NOT GREATER THAN  +15                               CL**2
00495          GO TO 1010-EDIT-LOOP.                                    EL6331
00496                                                                   EL6331
00497      IF EMI-ERROR = ZEROS                                         EL6331
00498          GO TO 2000-UPDATE-THE-FILE                               EL6331
00499      ELSE                                                         EL6331
00500          GO TO 8200-SEND-DATAONLY.                                EL6331
00501  EJECT                                                            EL6331
00502  2000-UPDATE-THE-FILE.                                            EL6331
CIDMOD
CIDMOD     IF EIBAID = DFHPF1                                           EL6331
CIDMOD        CONTINUE                                                  EL6331
CIDMOD     ELSE                                                         EL6331
CIDMOD        MOVE 'PRESS PF1 TO UPDATE FILE' TO EMI-MESSAGE-AREA (1)   EL6331
CIDMOD        MOVE SPACES TO EMI-MESSAGE-AREA (2)                       EL6331
CIDMOD        MOVE -1 TO  CAR1L                                         EL6331
031710        MOVE TOTAL-AMOUNT        TO GROSS-AMTO
CIDMOD        GO TO 8200-SEND-DATAONLY.
CIDMOD
00503      SET INDX                    TO  1.                           EL6331
060205     MOVE +1                     TO C1
00504                                                                   EL6331
           .
00505  2100-UPDATE-LOOP.                                                EL6331
00506      IF INDX  IS GREATER THAN  +15                                   CL**2
00507          GO TO 2200-UPDATE-COMPLETE.                              EL6331
00508                                                                   EL6331
00509      IF CARR-LEN (INDX) = ZEROS                                   EL6331
00510          SET INDX  UP  BY  1                                      EL6331
060205         ADD +1        TO C1
00511          GO TO 2100-UPDATE-LOOP.                                  EL6331
00512                                                                   EL6331
00513      EXEC CICS GETMAIN                                            EL6331
00514          SET      (ADDRESS OF PENDING-PAY-ADJ)                       CL**7
00515          LENGTH   (ERPYAJ-RECORD-LENGTH)                             CL**2
00516          INITIMG  (GETMAIN-SPACE)                                 EL6331
00517          END-EXEC.                                                EL6331
00518                                                                   EL6331
00519      MOVE 'PY'                   TO  PY-RECORD-ID.                EL6331
00520      MOVE PI-COMPANY-CD          TO  PY-COMPANY-CD.               EL6331
00521      MOVE CARRIER     (INDX)     TO  PY-CARRIER.                     CL**2
00522      MOVE GROUPING    (INDX)     TO  PY-GROUPING.                    CL**2
00523      MOVE FIN-RESP    (INDX)     TO  PY-FIN-RESP.                    CL**2
00524      MOVE ACCT        (INDX)     TO  PY-ACCOUNT.                     CL**2
00525      MOVE RTYPE       (INDX)     TO  PY-RECORD-TYPE.                 CL**2
00526                                                                      CL**2
00527      MOVE WORK-SEQ-NO            TO  PY-FILE-SEQ-NO.              EL6331
00528                                                                   EL6331
00529      ADD +1                      TO  WORK-SEQ-NO.                 EL6331
00530                                                                   EL6331
120204     IF GL-ACCT-LEN (INDX) NOT = ZEROS
120204        MOVE GL-ACCT (INDX)      TO  PY-GL-ACCOUNT
120204     END-IF

120204     IF GL-STATE-LEN (INDX) NOT = ZEROS
120204        MOVE GL-STATE (INDX)     TO  PY-GL-STATE
120204     END-IF
120204
120204     IF GL-CANC-LEN (INDX) NOT = ZEROS 
120204        MOVE GL-CANC (INDX)      TO  PY-GL-CANC-SW
120204     END-IF
120204
120204     IF GL-COMM-LEN (INDX) NOT = ZEROS
120204        MOVE GL-COMM (INDX)      TO  PY-GL-COMMENT
120204     END-IF

060205     MOVE WS-ERCOMP-TYPE (C1)    TO  PY-ERCOMP-TYPE
00533      SET WS-INDX                 TO  INDX.                           CL**2
00534      MOVE WS-EDITED-AMT(WS-INDX) TO  PY-ENTRY-AMT.                   CL**2
00535      MOVE PI-PROCESSOR-ID        TO  PY-LAST-MAINT-BY.            EL6331
00536      MOVE EIBTIME                TO  PY-LAST-MAINT-HHMMSS.        EL6331
00537      MOVE WS-CURRENT-BIN-DT      TO  PY-LAST-MAINT-DT             EL6331
00538                                      PY-INPUT-DT.                 EL6331
00539      MOVE ZEROS                  TO  PY-CHECK-QUE-CONTROL         EL6331
00540                                      PY-CHECK-QUE-SEQUENCE.       EL6331
00541      MOVE LOW-VALUES             TO  PY-CREDIT-ACCEPT-DT          EL6331
00542                                      PY-BILLED-DATE               EL6331
00543                                      PY-AR-DATE                      CL**2
00544                                      PY-REPORTED-DT               EL6331
00545                                      PY-CHECK-WRITTEN-DT.         EL6331
00546      MOVE PI-CR-MONTH-END-DT     TO  PY-CREDIT-SELECT-DT.         EL6331
00547      MOVE 'A'                    TO  JP-RECORD-TYPE.                 CL**2
00548      MOVE PENDING-PAY-ADJ        TO  JP-RECORD-AREA.                 CL**2
00549                                                                   EL6331
00550      EXEC CICS WRITE                                              EL6331
00551          DATASET  (PYAJ-FILE-ID)                                  EL6331
00552          FROM     (PENDING-PAY-ADJ)                               EL6331
00553          RIDFLD   (PY-CONTROL-PRIMARY)                            EL6331
00554          END-EXEC.                                                EL6331
00555                                                                   EL6331
00556      PERFORM 8400-LOG-JOURNAL-RECORD.                                CL**2
00557                                                                   EL6331
00558      MOVE LOW-VALUES             TO  DATA-AREA (INDX).            EL6331
00559                                                                   EL6331
00560      GO TO 2100-UPDATE-LOOP.                                      EL6331
00561                                                                   EL6331
00562  2200-UPDATE-COMPLETE.                                            EL6331
00563      MOVE LOW-VALUES             TO  EL633BI.                        CL**2
00564      MOVE ER-0000                TO  EMI-ERROR.                   EL6331
00565      MOVE -1                     TO  CAR1L.                       EL6331
00566                                                                   EL6331
00567      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT                   EL6331
00568                                                                   EL6331
00569      GO TO 8100-SEND-INITIAL-MAP.                                 EL6331
00570  EJECT                                                            EL6331
00571  7000-PYAJ-FILE-NOTOPEN.                                          EL6331
00572      MOVE -1                     TO  PFENTERL.                    EL6331
00573      MOVE ER-2232                TO  EMI-ERROR.                   EL6331
00574                                                                   EL6331
00575      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL6331
00576                                                                   EL6331
00577      GO TO 8200-SEND-DATAONLY.                                    EL6331
00578                                                                   EL6331
00579  7100-COMP-FILE-NOTOPEN.                                          EL6331
00580      MOVE -1                     TO  PFENTERL.                    EL6331
00581      MOVE ER-2233                TO  EMI-ERROR.                   EL6331
00582                                                                   EL6331
00583      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL6331
00584                                                                   EL6331
00585      GO TO 8200-SEND-DATAONLY.                                    EL6331
CIDMOD/                                                                 EL6331
CIDMOD*                                                                      000
CIDMOD*7100-COFA-FILE-NOTOPEN.                                               000
CIDMOD*    MOVE -1                     TO PFENTERL.                          000
CIDMOD*    MOVE ER-2959                TO EMI-ERROR.                         000
CIDMOD*    PERFORM 9900-ERROR-FORMAT THRU                                    000
CIDMOD*            9900-EXIT.                                                000
CIDMOD*    GO TO 8200-SEND-DATAONLY.                                         000
CIDMOD*                                                                      000
00587  8100-SEND-INITIAL-MAP.                                           EL6331
00588      MOVE WS-CURRENT-DT          TO  DATEO.                       EL6331
00589      MOVE EIBTIME                TO  TIME-IN.                     EL6331
00590      MOVE TIME-OUT               TO  TIMEO.                       EL6331
101101     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO  USERIDO.
00591      MOVE -1                     TO  CAR1L.                       EL6331
00592      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.                    EL6331
00593      MOVE EMI-MESSAGE-AREA (2)   TO  ERRMSG2O.                    EL6331
00594                                                                   EL6331
00595      EXEC CICS SEND                                               EL6331
00596          MAP     (MAP-NAME)                                       EL6331
00597          MAPSET  (MAPSET-NAME)                                    EL6331
00598          FROM    (EL633BO)                                           CL**2
00599          ERASE                                                    EL6331
00600          CURSOR                                                   EL6331
00601          END-EXEC.                                                EL6331
00602                                                                   EL6331
00603      GO TO 9100-RETURN-TRAN.                                      EL6331
00604  EJECT                                                            EL6331
00605  8200-SEND-DATAONLY.                                              EL6331
00606      MOVE WS-CURRENT-DT          TO  DATEO.                       EL6331
00607      MOVE EIBTIME                TO  TIME-IN.                     EL6331
00608      MOVE TIME-OUT               TO  TIMEO.                       EL6331
101101     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO  USERIDO.
00609      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.                    EL6331
00610      MOVE EMI-MESSAGE-AREA (2)   TO  ERRMSG2O.                    EL6331
00611                                                                   EL6331
00612      EXEC CICS SEND                                               EL6331
00613          MAP     (MAP-NAME)                                       EL6331
00614          MAPSET  (MAPSET-NAME)                                    EL6331
00615          FROM    (EL633BO)                                           CL**2
00616          DATAONLY                                                 EL6331
00617          CURSOR                                                   EL6331
00618          END-EXEC.                                                EL6331
00619                                                                   EL6331
00620      GO TO 9100-RETURN-TRAN.                                      EL6331
00621                                                                   EL6331
00622  8300-SEND-TEXT.                                                  EL6331
00623      EXEC CICS SEND TEXT                                          EL6331
00624          FROM    (LOGOFF-TEXT)                                    EL6331
00625          LENGTH  (LOGOFF-LENGTH)                                  EL6331
00626          ERASE                                                    EL6331
00627          FREEKB                                                   EL6331
00628          END-EXEC.                                                EL6331
00629                                                                   EL6331
00630      EXEC CICS RETURN                                             EL6331
00631          END-EXEC.                                                EL6331
00632  EJECT                                                            EL6331
00633  8400-LOG-JOURNAL-RECORD.                                            CL**2
00634 *    MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.                  EL6331
00635 *    MOVE THIS-PGM               TO  JP-PROGRAM-ID.               EL6331
00636 *    MOVE PYAJ-FILE-ID           TO  JP-FILE-ID.                  EL6331
00637 *    MOVE ZERO                   TO  JP-GENERIC-KEY-LENGTH.       EL6331
00638                                                                   EL6331
00639 *    EXEC CICS JOURNAL                                            EL6331
00640 *        JFILEID  (PI-JOURNAL-FILE-ID)                            EL6331
00641 *        JTYPEID  ('EL')                                          EL6331
00642 *        FROM     (JOURNAL-RECORD)                                EL6331
00643 *        LENGTH   (223)                                              CL**2
00644 *        END-EXEC.                                                EL6331
00645                                                                   EL6331
00646  8500-DATE-CONVERT.                                               EL6331
00647      EXEC CICS LINK                                               EL6331
00648          PROGRAM   (LINK-CLDATCV)                                 EL6331
00649          COMMAREA  (DATE-CONVERSION-DATA)                         EL6331
00650          LENGTH    (DC-COMM-LENGTH)                               EL6331
00651          END-EXEC.                                                EL6331
00652                                                                   EL6331
00653  8500-EXIT.                                                       EL6331
00654      EXIT.                                                        EL6331
00655                                                                   EL6331
00656  8600-DEEDIT.                                                        CL**2
00657      EXEC CICS BIF DEEDIT                                            CL**2
00658          FIELD   (DEEDIT-FIELD)                                      CL**2
00659          LENGTH  (11)                                                CL**2
00660          END-EXEC.                                                   CL**2
00661                                                                   EL6331
00662  8600-EXIT.                                                          CL**2
00663      EXIT.                                                           CL**2
00664  EJECT                                                            EL6331
00665  8800-UNAUTHORIZED-ACCESS.                                        EL6331
00666      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.                  EL6331
00667                                                                   EL6331
00668      GO TO 8300-SEND-TEXT.                                        EL6331
00669                                                                   EL6331
00670  8810-PF23.                                                       EL6331
00671      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               EL6331
00672      MOVE XCTL-005               TO  PGM-NAME.                    EL6331
00673                                                                   EL6331
00674      GO TO 9300-XCTL.                                             EL6331
00675                                                                   EL6331
00676  9000-RETURN-CICS.                                                EL6331
00677      EXEC CICS RETURN                                             EL6331
00678          END-EXEC.                                                EL6331
00679                                                                   EL6331
00680  9100-RETURN-TRAN.                                                EL6331
00681      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            EL6331
00682      MOVE SCREEN-NUMBER          TO  PI-CURRENT-SCREEN-NO.        EL6331
00683                                                                   EL6331
00684      EXEC CICS RETURN                                             EL6331
00685          TRANSID   (TRANS-ID)                                     EL6331
00686          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL6331
00687          LENGTH    (PI-COMM-LENGTH)                               EL6331
00688          END-EXEC.                                                EL6331
00689                                                                   EL6331
00690  9200-RETURN-MAIN-MENU.                                           EL6331
00691      MOVE XCTL-626               TO  PGM-NAME.                    EL6331
00692                                                                   EL6331
00693      GO TO 9300-XCTL.                                             EL6331
00694                                                                   EL6331
00695  9300-XCTL.                                                       EL6331
00696      EXEC CICS XCTL                                               EL6331
00697          PROGRAM   (PGM-NAME)                                     EL6331
00698          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL6331
00699          LENGTH    (PI-COMM-LENGTH)                               EL6331
00700          END-EXEC.                                                EL6331
00701                                                                   EL6331
00702  9400-CLEAR.                                                      EL6331
00703      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.                    EL6331
00704                                                                   EL6331
00705      GO TO 9300-XCTL.                                             EL6331
00706                                                                   EL6331
00707  9500-PF12.                                                       EL6331
00708      MOVE XCTL-010               TO  PGM-NAME.                    EL6331
00709                                                                   EL6331
00710      GO TO 9300-XCTL.                                             EL6331
00711                                                                   EL6331
00712  9600-PGMID-ERROR.                                                EL6331
00713      EXEC CICS HANDLE CONDITION                                   EL6331
00714          PGMIDERR  (8300-SEND-TEXT)                               EL6331
00715          END-EXEC.                                                EL6331
00716                                                                   EL6331
00717      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.          EL6331
00718      MOVE ' '                    TO  PI-ENTRY-CD-1.               EL6331
00719      MOVE XCTL-005               TO  PGM-NAME.                    EL6331
00720      MOVE PGM-NAME               TO  LOGOFF-PGM.                  EL6331
00721      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 EL6331
00722                                                                   EL6331
00723      GO TO 9300-XCTL.                                             EL6331
00724                                                                   EL6331
00725  9900-ERROR-FORMAT.                                               EL6331
00726      IF NOT EMI-ERRORS-COMPLETE                                   EL6331
00727          MOVE LINK-001           TO  PGM-NAME                     EL6331
00728          EXEC CICS LINK                                           EL6331
00729              PROGRAM   (PGM-NAME)                                 EL6331
00730              COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)            EL6331
00731              LENGTH    (EMI-COMM-LENGTH)                          EL6331
00732              END-EXEC.                                            EL6331
00733                                                                   EL6331
00734  9900-EXIT.                                                       EL6331
00735      EXIT.                                                        EL6331
00736                                                                   EL6331
00737  9990-ABEND.                                                      EL6331
00738      MOVE LINK-004               TO  PGM-NAME.                    EL6331
00739      MOVE DFHEIBLK               TO  EMI-LINE1.                   EL6331
00740                                                                   EL6331
00741      EXEC CICS LINK                                               EL6331
00742          PROGRAM   (PGM-NAME)                                     EL6331
00743          COMMAREA  (EMI-LINE1)                                    EL6331
00744          LENGTH    (72)                                           EL6331
00745          END-EXEC.                                                EL6331
00746                                                                   EL6331
00747      MOVE -1                     TO  PFENTERL.                    EL6331
00748                                                                   EL6331
00749      GO TO 8200-SEND-DATAONLY.                                    EL6331
00750                                                                   EL6331
00751      GOBACK.                                                      EL6331
00752                                                                   EL6331
00753  9995-SECURITY-VIOLATION.                                         EL6331
00754                              COPY ELCSCTP.                        EL6331
00755                                                                   EL6331
00756  9995-EXIT.                                                       EL6331
00757      EXIT.                     

