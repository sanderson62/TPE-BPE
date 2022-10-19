00001  IDENTIFICATION DIVISION.                                         12/22/97
00002                                                                   EL103
00003  PROGRAM-ID.                 EL103 .                                 LV021
00004 *              PROGRAM CONVERTED BY                                  CL*17
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*17
00006 *              CONVERSION DATE 11/28/95 11:14:54.                    CL*17
00007 *                            VMOD=2.021                              CL*21
00008 *                                                                 EL103
00008 *                                                                 EL103
00009 *AUTHOR.        LOGIC,INC.                                           CL*17
00010 *               DALLAS, TEXAS.                                       CL*17
00011                                                                   EL103
00012 *DATE-COMPILED.                                                      CL*17
00013                                                                   EL103
00014 *SECURITY.   *****************************************************   CL*17
00015 *            *                                                   *   CL*17
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*17
00017 *            *                                                   *   CL*17
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*17
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*17
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*17
00021 *            *                                                   *   CL*17
00022 *            *****************************************************   CL*17
00023                                                                   EL103
00024 *                                                                    CL**7
00025 *REMARKS.                                                            CL**4
00026 *        TRANSACTION - EX09 - USER RECORD MAINTENANCE                CL**4
031808******************************************************************
031808*                   C H A N G E   L O G
031808*
031808* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
031808*-----------------------------------------------------------------
031808*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
031808* EFFECTIVE    NUMBER
031808*-----------------------------------------------------------------
031808* 031808    2006032200004  AJRA  ADD APPROVAL LEVEL 4
011812* 011812    2011022800001  AJRA  ADD CSR IND
091813* 091813    2013082900001  AJRA  ADD APPROVAL LEVEL 5
031808******************************************************************
00027                                                                   EL103
00028  ENVIRONMENT DIVISION.                                            EL103
00029  DATA DIVISION.                                                   EL103
00030      EJECT                                                        EL103
00031  WORKING-STORAGE SECTION.                                         EL103
00032  77  FILLER  PIC  X(32) VALUE '********************************'. EL103
00033  77  FILLER  PIC  X(32) VALUE '*    EL103 WORKING STORAGE     *'. EL103
00034  77  FILLER  PIC  X(32) VALUE '********* VMOD=2.021 ***********'.    CL*21
00035                                                                      CL**6
00036  01  WS-MAP-AREAS.                                                   CL**6
00037      12  WS-QCDAYS                  PIC X(35)                        CL**6
00038          VALUE 'DIFFERENCE IN QUOTED/CALC.   DAYS :'.                CL**6
00039      12  WS-AH                      PIC X(03)                        CL**6
00040          VALUE 'A/H'.                                                CL**6
00041      12  WS-LIFE                    PIC X(04)                        CL**6
00042          VALUE 'LIFE'.                                               CL**6
00043      12  WS-MAXDAYS                 PIC X(35)                        CL**6
00044          VALUE 'MAXIMUM REGULAR PAYMENT      DAYS :'.                CL**6
00045      12  WS-AUTO-PAY                PIC X(35)                        CL**6
00046          VALUE 'MAXIMUM AUTO. PAYMENT TERM   MTHS :'.                CL**6
00047      12  WS-AMOUNT                  PIC X(05)                        CL**6
00048          VALUE 'AMT :'.                                              CL**6
00049      12  WS-EXPENSE                 PIC X(35)                        CL*15
00050          VALUE 'MAXIMUM EXPENSE PAYMENT            '.                CL*15
00051                                                                      CL**7
00052  01  WS-SAVE-PROCESSOR-RCDS.                                         CL**7
00053      12  SAVE-PROCESSOR-ZERO        PIC X(750).                      CL*11
00054      12  SAVE-PROCESSOR-ONE         PIC X(750).                      CL*11
00055                                                                      CL*20
00056  01  WS-PASSWORD-NUMERIC            PIC X.                           CL*20
00057  01  WS-PASSWORD-SIXDIGIT           PIC X.                           CL*20
00058                                                                      CL*20
00059  01  WS-SAVE-PASSWORD.                                               CL*20
00060      12  WS-PASSWORD-X1             PIC X.                           CL*20
00061      12  WS-PASSWORD-X2             PIC X.                           CL*20
00062      12  WS-PASSWORD-X3             PIC X.                           CL*20
00063      12  WS-PASSWORD-X4             PIC X.                           CL*20
00064      12  WS-PASSWORD-X5             PIC X.                           CL*20
00065      12  WS-PASSWORD-X6             PIC X.                           CL*20
00066      12  WS-PASSWORD-X7             PIC X.                           CL*20
00067      12  WS-PASSWORD-X8             PIC X.                           CL*20
00068      12  WS-PASSWORD-X9             PIC X.                           CL*20
00069      12  WS-PASSWORD-X10            PIC X.                           CL*20
00070      12  WS-PASSWORD-X11            PIC X.                           CL*20
00071                                                                      CL*20
00072  01  WS-SAVE-PASSWORD-NUMERIC REDEFINES  WS-SAVE-PASSWORD.           CL*20
00073      12  WS-PASSWORD-N1             PIC 9.                           CL*20
00074      12  WS-PASSWORD-N2             PIC 9.                           CL*20
00075      12  WS-PASSWORD-N3             PIC 9.                           CL*20
00076      12  WS-PASSWORD-N4             PIC 9.                           CL*20
00077      12  WS-PASSWORD-N5             PIC 9.                           CL*20
00078      12  WS-PASSWORD-N6             PIC 9.                           CL*20
00079      12  WS-PASSWORD-N7             PIC 9.                           CL*20
00080      12  WS-PASSWORD-N8             PIC 9.                           CL*20
00081      12  WS-PASSWORD-N9             PIC 9.                           CL*20
00082      12  WS-PASSWORD-N10            PIC 9.                           CL*20
00083      12  WS-PASSWORD-N11            PIC 9.                           CL*20
00084                                                                   EL103
00085  01  ERROR-MESSAGES.                                              EL103
00086      12  ER-0000             PIC  X(4)       VALUE '0000'.        EL103
00087      12  ER-0004             PIC  X(4)       VALUE '0004'.        EL103
00088      12  ER-0007             PIC  X(4)       VALUE '0007'.        EL103
00089      12  ER-0019             PIC  X(4)       VALUE '0019'.        EL103
00090      12  ER-0023             PIC  X(4)       VALUE '0023'.        EL103
00091      12  ER-0029             PIC  X(4)       VALUE '0029'.        EL103
00092      12  ER-0042             PIC  X(4)       VALUE '0042'.        EL103
00093      12  ER-0050             PIC  X(4)       VALUE '0050'.        EL103
00094      12  ER-0063             PIC  X(4)       VALUE '0063'.        EL103
00095      12  ER-0068             PIC  X(4)       VALUE '0068'.        EL103
00096      12  ER-0070             PIC  X(4)       VALUE '0070'.        EL103
00097      12  ER-0073             PIC  X(4)       VALUE '0073'.        EL103
00098      12  ER-0074             PIC  X(4)       VALUE '0074'.        EL103
00099      12  ER-0075             PIC  X(4)       VALUE '0075'.        EL103
00100      12  ER-0077             PIC  X(4)       VALUE '0077'.        EL103
00101      12  ER-0078             PIC  X(4)       VALUE '0078'.        EL103
00102      12  ER-0079             PIC  X(4)       VALUE '0079'.        EL103
00103      12  ER-0080             PIC  X(4)       VALUE '0080'.        EL103
00104      12  ER-0081             PIC  X(4)       VALUE '0081'.        EL103
00105      12  ER-0082             PIC  X(4)       VALUE '0082'.        EL103
00106      12  ER-0083             PIC  X(4)       VALUE '0083'.        EL103
00107      12  ER-0084             PIC  X(4)       VALUE '0084'.        EL103
00108      12  ER-0085             PIC  X(4)       VALUE '0085'.        EL103
00109      12  ER-0086             PIC  X(4)       VALUE '0086'.        EL103
00110      12  ER-0130             PIC  X(4)       VALUE '0130'.           CL**7
00111      12  ER-0252             PIC  X(4)       VALUE '0252'.        EL103
00112      12  ER-0425             PIC  X(4)       VALUE '0425'.        EL103
00113      12  ER-0614             PIC  X(4)       VALUE '0614'.        EL103
00114      12  ER-1889             PIC  X(4)       VALUE '1889'.           CL*12
00115      12  ER-2376             PIC  X(4)       VALUE '2376'.        EL103
00116      12  ER-2548             PIC  X(4)       VALUE '2548'.           CL*20
00117      12  ER-2549             PIC  X(4)       VALUE '2549'.           CL*20
00118      12  ER-7008             PIC  X(4)       VALUE '7008'.        EL103
00119      12  ER-7084             PIC  X(4)       VALUE '7084'.        EL103
00120      12  ER-7085             PIC  X(4)       VALUE '7085'.        EL103
00121      12  ER-7087             PIC  X(4)       VALUE '7087'.        EL103
00122      12  ER-7088             PIC  X(4)       VALUE '7088'.        EL103
00123      12  ER-7089             PIC  X(4)       VALUE '7089'.           CL*10
00124      12  ER-8026             PIC  X(4)       VALUE '8026'.           CL*20
00125      12  ER-9014             PIC  X(4)       VALUE '9014'.           CL**7
00126      12  ER-9946             PIC  X(4)       VALUE '9946'.           CL*16
00127                                                                   EL103
00128  01  WS-DATE-AREA.                                                EL103
00129      12  SAVE-DATE           PIC  X(8)       VALUE SPACES.        EL103
00130      12  SAVE-BIN-DATE       PIC  XX         VALUE SPACES.        EL103
00131                                                                      CL**6
00132      EJECT                                                        EL103
00133  01  STANDARD-AREAS.                                              EL103
00134      12  MAP-NAME            PIC  X(8)       VALUE 'EL103A'.      EL103
00135      12  MAPSET-NAME         PIC  X(8)       VALUE 'EL103S'.      EL103
00136      12  TRANS-ID            PIC  X(4)       VALUE 'EX09'.        EL103
00137      12  THIS-PGM            PIC  X(8)       VALUE 'EL103'.       EL103
00138      12  PGM-NAME            PIC  X(8).                           EL103
00139      12  TIME-IN             PIC S9(7).                           EL103
00140      12  TIME-OUT-R  REDEFINES  TIME-IN.                          EL103
00141          16  FILLER          PIC  X.                              EL103
00142          16  TIME-OUT        PIC  99V99.                          EL103
00143          16  FILLER          PIC  XX.                             EL103
00144      12  XCTL-005            PIC  X(8)       VALUE 'EL005'.       EL103
00145      12  XCTL-010            PIC  X(8)       VALUE 'EL010'.       EL103
00146      12  XCTL-126            PIC  X(8)       VALUE 'EL126'.       EL103
00147      12  XCTL-1031           PIC  X(8)       VALUE 'EL1031'.         CL**6
00148      12  XCTL-EM626          PIC  X(8)       VALUE 'EM626'.          CL**7
00149      12  XCTL-626            PIC  X(8)       VALUE 'EL626'.          CL**6
00150      12  XCTL-800            PIC  X(8)       VALUE 'GL800'.       EL103
00151      12  LINK-001            PIC  X(8)       VALUE 'EL001'.       EL103
00152      12  LINK-004            PIC  X(8)       VALUE 'EL004'.       EL103
00153      12  LINK-ELDATCV        PIC  X(8)       VALUE 'ELDATCV'.     EL103
00154      12  FILE-ID             PIC  X(8)       VALUE 'ELCNTL'.      EL103
00155      12  MORTGAGE-SCRTY      PIC  X(12)      VALUE 'ACCESS CODE:'.   CL**7
00156      12  CREDIT-SCRTY        PIC  X(12)      VALUE '    ACCOUNT:'.   CL**7
00157                                                                   EL103
00158  01  MISC-WORK-AREAS.                                             EL103
00159      12  SYS                 PIC S9(4)      VALUE +0    COMP.        CL**6
00160      12  MAXSYS              PIC S9(4)      VALUE +4    COMP.        CL**6
00161      12  SLOT                PIC S9(4)      VALUE +0    COMP.        CL**6
00162      12  MAXSLOT             PIC S9(4)      VALUE +44   COMP.        CL**6
00163      12  READ-PREV-SW        PIC X           VALUE SPACE.            CL*12
00164          88  FIRST-READ-PREV                 VALUE '1'.              CL**7
00165          88  SECOND-READ-PREV                VALUE '2'.              CL**7
00166      12  GETMAIN-SPACE       PIC  X          VALUE SPACE.         EL103
00167      12  TERMINAL-TEST-AREA.                                      EL103
00168          16  TTA-1           PIC  X.                              EL103
00169          16  TTA-2           PIC  X.                              EL103
00170          16  TTA-3           PIC  X.                              EL103
00171          16  TTA-4           PIC  X.                              EL103
00172                                                                      CL**6
00173  01  SYSTEM-DESCRIPTIONS.                                            CL**6
00174      12  CREDIT      PIC X(26) VALUE 'CREDIT APPLICATIONS       '.   CL**6
00175      12  CLAIMS      PIC X(26) VALUE 'CLAIMS APPLICATIONS       '.   CL**6
00176      12  CREDIT-CARD PIC X(26) VALUE 'CREDIT CARD APPLICATIONS  '.   CL**6
00177      12  ACCT-RECV   PIC X(26) VALUE 'ACCT RECV APPLICATIONS    '.   CL**6
00178      12  MORTGAGE    PIC X(26) VALUE 'CONVENIENCE APPLICATIONS  '.   CL*15
00179      12  GNRL-LDGR   PIC X(26) VALUE 'GENERAL LDGR APPLICATIONS '.   CL**6
00180                                                                      CL**6
00181  01  DESCRIPTION-TABLE  REDEFINES  SYSTEM-DESCRIPTIONS.              CL**6
00182      12  DESCRIPT  OCCURS  6 TIMES  PIC X(26).                       CL**6
00183                                                                      CL**6
00184      EJECT                                                        EL103
00185                                                                   EL103
00186  01  ACCESS-KEYS.                                                 EL103
00187      12  ELCNTL-KEY.                                              EL103
00188          16  CK-COMP-ID      PIC  X(3).                           EL103
00189          16  CK-REC-TYPE     PIC  X          VALUE '2'.           EL103
00190          16  CK-USER-CD      PIC  X(4)       VALUE SPACES.        EL103
00191          16  CK-CARRIER  REDEFINES                                EL103
00192              CK-USER-CD.                                          EL103
00193              20  FILLER      PIC  X(3).                           EL103
00194              20  CK-CARR     PIC  X.                              EL103
00195          16  CK-SEQ          PIC S9(4)       VALUE +0      COMP.  EL103
00196      EJECT                                                        EL103
00197      COPY ELCDATE.                                                   CL*12
00198      EJECT                                                        EL103
00199      COPY ELCLOGOF.                                                  CL*12
00200      EJECT                                                        EL103
00201      COPY ELCATTR.                                                   CL*12
00202      EJECT                                                        EL103
00203      COPY ELCEMIB.                                                   CL*12
00204      EJECT                                                        EL103
00205      COPY ELCINTF.                                                   CL*12
00206      12  FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.                 EL103
00207          16  PI-PREV-USER    PIC  X(4).                           EL103
00208          16  PI-READ-SW      PIC S9.                                 CL*12
00209              88  READ-PROCESSOR-ZERO   VALUE +0.                     CL**7
00210              88  READ-PROCESSOR-ONE    VALUE +1.                     CL**7
00211          16  PI-APPLICATION  PIC S9.                                 CL*12
00212              88  PI-VALID-APP          VALUES ARE +1 +2 +3 +4.       CL**6
00213              88  CREDIT-APP            VALUES ARE +1.                CL**7
00214              88  CLAIMS-APP            VALUES ARE +2.                CL**7
00215              88  CREDIT-CARD-APP       VALUES ARE +3.                CL**7
00216              88  ACCT-RECV-APP         VALUES ARE +4.                CL**7
00217              88  MORTGAGE-APP          VALUES ARE +1.                CL**7
00218          16  FILLER          PIC  X(634).                            CL*18
00219      EJECT                                                        EL103
00220      COPY ELCJPFX.                                                   CL*12
00221      PIC  X(750).                                                    CL*12
00222      EJECT                                                        EL103
00223      COPY ELCAID.                                                    CL*12
00224                                                                   EL103
00225  01  FILLER  REDEFINES  DFHAID.                                   EL103
00226      12  FILLER              PIC  X(8).                           EL103
00227      12  PF-VALUES           PIC  X          OCCURS 2.            EL103
00228                                                                      CL**6
00229      EJECT                                                        EL103
00230      COPY EL103S.                                                    CL*12
00231                                                                      CL**6
00232  01  FILLER  REDEFINES  EL103AI.                                  EL103
011812     12  FILLER              PIC  X(291).                            CL*20
00234      12  APP-INFO     OCCURS 44 TIMES.                               CL**6
00235          16  APP-LENGTH      PIC S9(4)   COMP.                       CL**6
00236          16  APP-ATTRB       PIC  X.                                 CL**6
00237          16  APP             PIC  XX.                                CL**6
00238                                                                      CL**6
00239      EJECT                                                        EL103
00240  LINKAGE SECTION.                                                 EL103
00241  01  DFHCOMMAREA             PIC  X(1024).                        EL103
00242                                                                   EL103
00243 *01 PARMLIST .                                                       CL*17
00244 *    12  FILLER              PIC S9(8)   COMP.                       CL*17
00245 *    12  ELCNTL-POINTER      PIC S9(8)   COMP.                       CL*17
00246      EJECT                                                        EL103
00247      COPY ELCCNTL.                                                   CL*12
00248      EJECT                                                        EL103
00249  PROCEDURE DIVISION.                                              EL103
00250                                                                   EL103
00251      MOVE EIBDATE                TO DC-JULIAN-YYDDD.                 CL**6
00252      MOVE '5'                    TO DC-OPTION-CODE.                  CL**6
00253                                                                   EL103
00254      PERFORM 9700-LINK-DATE-CONVERT  THRU  9799-EXIT.             EL103
00255                                                                   EL103
00256      MOVE DC-GREG-DATE-1-EDIT    TO SAVE-DATE.                       CL**6
00257      MOVE DC-BIN-DATE-1          TO SAVE-BIN-DATE.                   CL**6
00258      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.         CL**6
00259                                                                      CL*17
00260      MOVE LOW-VALUES             TO EL103AO.                         CL*17
00261                                                                   EL103
00262 ***************************************************************   EL103
00263 *       IF ATTEMPTING TO EXECUTE PROGRAM WITHOUT SIGNING ON   *   EL103
00264 *       (COMM LENGTH EQUAL ZERO), SEND ERROR MESSAGE.         *      CL**6
00265 ***************************************************************   EL103
00266                                                                   EL103
00267      IF EIBCALEN  EQUAL 0                                            CL**6
00268          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL103
00269                                                                   EL103
00270      IF PI-CALLING-PROGRAM  NOT EQUAL THIS-PGM                       CL**6
00271          IF PI-RETURN-TO-PROGRAM  NOT EQUAL THIS-PGM                 CL**6
00272              MOVE PI-SAVED-PROGRAM-5    TO PI-SAVED-PROGRAM-6        CL**6
00273              MOVE PI-SAVED-PROGRAM-4    TO PI-SAVED-PROGRAM-5        CL**6
00274              MOVE PI-SAVED-PROGRAM-3    TO PI-SAVED-PROGRAM-4        CL**6
00275              MOVE PI-SAVED-PROGRAM-2    TO PI-SAVED-PROGRAM-3        CL**6
00276              MOVE PI-SAVED-PROGRAM-1    TO PI-SAVED-PROGRAM-2        CL**6
00277              MOVE PI-RETURN-TO-PROGRAM  TO PI-SAVED-PROGRAM-1        CL**6
00278              MOVE PI-CALLING-PROGRAM    TO PI-RETURN-TO-PROGRAM      CL**6
00279              MOVE THIS-PGM              TO PI-CALLING-PROGRAM        CL**6
00280              MOVE SPACES                TO PI-PREV-USER              CL**7
00281              MOVE ZEROS                 TO PI-APPLICATION            CL**6
00282          ELSE                                                     EL103
00283              MOVE PI-RETURN-TO-PROGRAM  TO PI-CALLING-PROGRAM        CL**6
00284              MOVE PI-SAVED-PROGRAM-1    TO PI-RETURN-TO-PROGRAM      CL**6
00285              MOVE PI-SAVED-PROGRAM-2    TO PI-SAVED-PROGRAM-1        CL**6
00286              MOVE PI-SAVED-PROGRAM-3    TO PI-SAVED-PROGRAM-2        CL**6
00287              MOVE PI-SAVED-PROGRAM-4    TO PI-SAVED-PROGRAM-3        CL**6
00288              MOVE PI-SAVED-PROGRAM-5    TO PI-SAVED-PROGRAM-4        CL**6
00289              MOVE PI-SAVED-PROGRAM-6    TO PI-SAVED-PROGRAM-5        CL**6
00290              MOVE SPACES                TO PI-SAVED-PROGRAM-6.       CL**6
00291                                                                   EL103
00292      EXEC CICS  HANDLE CONDITION                                  EL103
00293          DUPREC    (8840-DUPREC)                                  EL103
00294          NOTOPEN   (8870-NOTOPEN)                                 EL103
00295          NOTFND    (8880-NOT-FOUND)                               EL103
00296          PGMIDERR  (9600-PGMID-ERROR)                             EL103
00297          ERROR     (9999-ABEND)                                   EL103
00298      END-EXEC.                                                       CL*12
00299                                                                   EL103
00300      IF EIBTRNID  NOT EQUAL TRANS-ID                                 CL**6
00301          IF PI-PREV-USER NOT EQUAL SPACES                            CL**7
00302              MOVE PI-PREV-USER   TO USERCDI                          CL**7
00303              MOVE +4             TO USERCDL                          CL**6
00304              GO TO 1000-SHOW-USER.                                   CL**7
00305                                                                      CL**7
00306      IF EIBTRNID  NOT EQUAL TRANS-ID                                 CL**7
00307          IF CREDIT-SESSION                                           CL**7
00308              MOVE ZERO           TO PI-READ-SW                       CL**7
00309              MOVE 1              TO PI-APPLICATION                   CL*12
00310              MOVE DESCRIPT(1)    TO DESCRO                           CL**7
00311              GO TO 8100-SEND-INITIAL-MAP                             CL**7
00312          ELSE                                                        CL**6
00313          IF CLAIM-SESSION                                            CL**7
00314              MOVE ZERO           TO PI-READ-SW                       CL**7
00315              MOVE 2              TO PI-APPLICATION                   CL*12
00316              MOVE DESCRIPT(2)    TO DESCRO                           CL**7
00317              MOVE AL-UANON       TO AUTAMTA                          CL**7
00318                                     QCDAYSA                          CL**7
00319                                     QCAMTA                           CL**7
00320                                     MAXDAYA                          CL**7
00321                                     MXAHAMTA                         CL**7
00322                                     MXLFAMTA                         CL**7
00323                                     AUTMONA                          CL**7
00324                                     MAXEXPA                          CL*15
00325              MOVE AL-SANON       TO XAMT1A                           CL**7
00326                                     XAMT2A                           CL**7
00327                                     XUTMONA                          CL**7
00328                                     XCDAYSA                          CL**7
00329                                     XAHA                             CL**7
00330                                     XLIFEA                           CL**7
00331                                     XAXDAYA                          CL**7
00332                                     XEXP1A                           CL*15
00333                                     EXPAMTA                          CL*15
00334              MOVE WS-QCDAYS      TO XCDAYSI                          CL**7
00335              MOVE WS-AH          TO XAHI                             CL**7
00336              MOVE WS-LIFE        TO XLIFEI                           CL**7
00337              MOVE WS-MAXDAYS     TO XAXDAYI                          CL**7
00338              MOVE WS-AUTO-PAY    TO XUTMONI                          CL**7
00339              MOVE WS-AMOUNT      TO XAMT1I                           CL**7
00340                                     XAMT2I                           CL**7
00341                                     EXPAMTI                          CL*15
00342              MOVE WS-EXPENSE     TO XEXP1I                           CL*15
00343              GO TO 8100-SEND-INITIAL-MAP                             CL**7
00344          ELSE                                                        CL**7
00345          IF MORTGAGE-SESSION                                         CL**7
00346              MOVE 1              TO PI-READ-SW                       CL*12
00347                                     PI-APPLICATION                   CL*12
00348              MOVE DESCRIPT(5)    TO DESCRO                           CL**7
00349              GO TO 8100-SEND-INITIAL-MAP.                            CL**6
00350                                                                      CL**6
00351      IF EIBAID  EQUAL DFHCLEAR                                       CL**6
00352          GO TO 9400-CLEAR.                                        EL103
00353                                                                      CL**6
00354      EJECT                                                        EL103
00355  0200-RECEIVE.                                                    EL103
00356                                                                   EL103
00357      MOVE LOW-VALUES             TO EL103AI.                         CL**6
00358                                                                      CL**6
00359      IF EIBAID  EQUAL DFHPA1 OR DFHPA2 OR DFHPA3                     CL**6
00360          MOVE ER-7008            TO EMI-ERROR                        CL**6
00361          PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT               EL103
00362          MOVE -1                 TO MAINTL                           CL**6
00363          GO TO 8200-SEND-DATAONLY.                                EL103
00364                                                                   EL103
00365      EXEC CICS  RECEIVE                                           EL103
00366          MAP     (MAP-NAME)                                       EL103
00367          MAPSET  (MAPSET-NAME)                                    EL103
00368          INTO    (EL103AI)                                        EL103
00369      END-EXEC.                                                       CL*12
00370                                                                   EL103
00371      IF ENTERPFL  EQUAL ZERO                                         CL**6
00372          GO TO 0300-CHECK-PFKEYS.                                 EL103
00373                                                                   EL103
00374      IF EIBAID  NOT EQUAL DFHENTER                                   CL**6
00375          MOVE ER-0004            TO EMI-ERROR                        CL**6
00376          GO TO 0310-INPUT-ERROR.                                  EL103
00377                                                                   EL103
00378      IF (ENTERPFI NUMERIC) AND (ENTERPFI GREATER 0 AND LESS 25)      CL**7
00379          MOVE PF-VALUES (ENTERPFI)  TO EIBAID                        CL**6
00380      ELSE                                                         EL103
00381          MOVE ER-0029               TO EMI-ERROR                     CL**6
00382          GO TO 0310-INPUT-ERROR.                                  EL103
00383                                                                      CL**6
00384      EJECT                                                        EL103
00385  0300-CHECK-PFKEYS.                                               EL103
00386      IF EIBAID  EQUAL DFHPF23                                        CL**6
00387          GO TO 8820-PF23.                                         EL103
00388                                                                   EL103
00389      IF EIBAID  EQUAL DFHPF24                                        CL**6
00390          GO TO 9200-RETURN-MAIN-MENU.                             EL103
00391                                                                   EL103
00392      IF PI-USER-ALMIGHTY-YES                                      EL103
00393          NEXT SENTENCE                                               CL**7
00394      ELSE                                                            CL**7
00395      IF PI-PROCESSOR-ID EQUAL USERCDI                                CL**7
00396          NEXT SENTENCE                                            EL103
00397      ELSE                                                         EL103
00398          MOVE ER-0007            TO EMI-ERROR                        CL**6
00399          PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT               EL103
00400          MOVE LOW-VALUES         TO EL103AO                          CL**6
00401          GO TO 8100-SEND-INITIAL-MAP.                             EL103
00402                                                                   EL103
00403      IF EIBAID  EQUAL DFHPF12                                        CL**6
00404          GO TO 9500-PF12.                                         EL103
00405                                                                   EL103
00406      IF MAINTL GREATER ZERO AND                                      CL*19
00407         MAINTI NOT EQUAL SPACE AND                                   CL**6
00408         EIBAID NOT EQUAL DFHENTER                                    CL**6
00409          MOVE ER-0050            TO EMI-ERROR                        CL**6
00410          GO TO 0310-INPUT-ERROR.                                  EL103
00411                                                                   EL103
00412      IF EIBAID  EQUAL DFHPF1                                         CL**6
00413          GO TO 5000-FIND-NEXT-USER.                               EL103
00414                                                                   EL103
00415      IF EIBAID  EQUAL DFHPF2                                         CL**6
00416          GO TO 5100-FIND-PREV-USER.                                  CL**6
00417                                                                   EL103
00418      IF EIBAID  EQUAL DFHPF3                                         CL**6
00419          IF PI-PREV-USER NOT EQUAL SPACES                            CL**7
00420              MOVE 1              TO PI-APPLICATION                   CL*12
00421              MOVE ZERO           TO PI-READ-SW                       CL**7
00422              GO TO 1000-SHOW-USER                                    CL**7
00423          ELSE                                                        CL**7
00424              MOVE ER-0019        TO EMI-ERROR                        CL**7
00425              PERFORM 9000-ERROR-FORMAT THRU 9099-EXIT                CL**7
00426              MOVE -1             TO USERCDL                          CL**7
00427              MOVE AL-UABON       TO USERCDA                          CL**7
00428              GO TO 8200-SEND-DATAONLY.                               CL**7
00429                                                                   EL103
00430      IF EIBAID  EQUAL DFHPF4                                         CL**6
00431          IF PI-PREV-USER NOT EQUAL SPACES                            CL**7
00432              MOVE 2              TO PI-APPLICATION                   CL*12
00433              MOVE ZERO           TO PI-READ-SW                       CL**7
00434              GO TO 1000-SHOW-USER                                    CL**7
00435          ELSE                                                        CL**7
00436              MOVE ER-0019        TO EMI-ERROR                        CL**7
00437              PERFORM 9000-ERROR-FORMAT THRU 9099-EXIT                CL**7
00438              MOVE -1             TO USERCDL                          CL**7
00439              MOVE AL-UABON       TO USERCDA                          CL**7
00440              GO TO 8200-SEND-DATAONLY.                               CL**7
00441                                                                      CL**6
00442      IF EIBAID  EQUAL DFHPF5                                         CL**6
00443          IF PI-PREV-USER NOT EQUAL SPACES                            CL**7
00444              MOVE 3              TO PI-APPLICATION                   CL*12
00445              MOVE ZERO           TO PI-READ-SW                       CL**7
00446              GO TO 1000-SHOW-USER                                    CL**7
00447          ELSE                                                        CL**7
00448              MOVE ER-0019        TO EMI-ERROR                        CL**7
00449              PERFORM 9000-ERROR-FORMAT THRU 9099-EXIT                CL**7
00450              MOVE -1             TO USERCDL                          CL**7
00451              MOVE AL-UABON       TO USERCDA                          CL**7
00452              GO TO 8200-SEND-DATAONLY.                               CL**7
00453                                                                      CL**6
00454      IF EIBAID  EQUAL DFHPF6                                         CL**6
00455          IF PI-PREV-USER NOT EQUAL SPACES                            CL**7
00456              MOVE 4              TO PI-APPLICATION                   CL*12
00457              MOVE ZERO           TO PI-READ-SW                       CL**7
00458              GO TO 1000-SHOW-USER                                    CL**7
00459          ELSE                                                        CL**7
00460              MOVE ER-0019        TO EMI-ERROR                        CL**7
00461              PERFORM 9000-ERROR-FORMAT THRU 9099-EXIT                CL**7
00462              MOVE -1             TO USERCDL                          CL**7
00463              MOVE AL-UABON       TO USERCDA                          CL**7
00464              GO TO 8200-SEND-DATAONLY.                               CL**7
00465                                                                      CL**6
00466      IF EIBAID  EQUAL DFHPF7                                         CL**6
00467          IF PI-PREV-USER NOT EQUAL SPACES                            CL**7
00468              MOVE 1              TO PI-APPLICATION                   CL*12
00469                                     PI-READ-SW                       CL*12
00470              GO TO 1000-SHOW-USER                                    CL**7
00471          ELSE                                                        CL**7
00472              MOVE ER-0019        TO EMI-ERROR                        CL**7
00473              PERFORM 9000-ERROR-FORMAT THRU 9099-EXIT                CL**7
00474              MOVE -1             TO USERCDL                          CL**7
00475              MOVE AL-UABON       TO USERCDA                          CL**7
00476              GO TO 8200-SEND-DATAONLY.                               CL**7
00477                                                                      CL**7
00478      IF EIBAID  EQUAL DFHPF8                                         CL**7
00479          IF PI-PREV-USER NOT EQUAL SPACES                            CL**7
00480              MOVE XCTL-1031      TO PGM-NAME                         CL**7
00481              GO TO 9300-XCTL                                         CL**7
00482          ELSE                                                        CL**7
00483              MOVE ER-0019        TO EMI-ERROR                        CL**7
00484              PERFORM 9000-ERROR-FORMAT THRU 9099-EXIT                CL**7
00485              MOVE -1             TO USERCDL                          CL**7
00486              MOVE AL-UABON       TO USERCDA                          CL**7
00487              GO TO 8200-SEND-DATAONLY.                               CL**7
00488                                                                      CL**6
00489      IF EIBAID  EQUAL DFHENTER                                       CL**6
00490          GO TO 0320-EDIT-DATA.                                       CL**6
00491                                                                      CL**6
00492      MOVE ER-0029                TO EMI-ERROR.                       CL**6
00493                                                                      CL**6
00494  0310-INPUT-ERROR.                                                   CL**6
00495                                                                      CL**6
00496      PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT.                     CL**6
00497                                                                      CL**6
00498      MOVE AL-UNBON               TO ENTERPFA.                        CL**6
00499                                                                      CL**6
00500      IF ENTERPFL EQUAL ZERO                                          CL**6
00501          MOVE -1                 TO MAINTL                           CL**6
00502      ELSE                                                            CL**6
00503          MOVE -1                 TO ENTERPFL.                        CL**6
00504                                                                      CL**6
00505      GO TO 8200-SEND-DATAONLY.                                       CL**6
00506                                                                      CL**6
00507      EJECT                                                           CL**6
00508  0320-EDIT-DATA.                                                     CL**6
00509                                                                      CL**6
00510      IF MAINTI  EQUAL 'S'                                            CL**6
00511          GO TO 1000-SHOW-USER.                                       CL**6
00512                                                                      CL**6
00513      IF MAINTI  EQUAL 'C'                                            CL**6
00514          GO TO 2000-CHANGE-USER.                                  EL103
00515                                                                   EL103
00516      IF PI-USER-ALMIGHTY-YES AND                                     CL**7
00517         MAINTI  EQUAL 'A'                                            CL**7
00518          GO TO 3000-ADD-USER.                                     EL103
00519                                                                   EL103
00520      IF PI-USER-ALMIGHTY-YES AND                                     CL**7
00521         MAINTI  EQUAL 'D'                                            CL**7
00522          GO TO 4000-DELETE-USER.                                  EL103
00523                                                                   EL103
00524      MOVE ER-0023                TO EMI-ERROR.                       CL**6
00525                                                                   EL103
00526      PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT.                  EL103
00527                                                                   EL103
00528      MOVE -1                     TO MAINTL.                          CL**6
00529      MOVE AL-UABON               TO MAINTA.                          CL**6
00530                                                                   EL103
00531      GO TO 8200-SEND-DATAONLY.                                    EL103
00532                                                                      CL**6
00533      EJECT                                                        EL103
00534  1000-SHOW-USER.                                                  EL103
00535                                                                      CL**6
00536      IF USERCDL  EQUAL ZERO                                          CL**6
00537          MOVE ER-0019            TO EMI-ERROR                        CL**6
00538          PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT               EL103
00539          MOVE -1                 TO USERCDL                          CL**6
00540          MOVE AL-UABON           TO USERCDA                          CL**6
00541          GO TO 8200-SEND-DATAONLY.                                EL103
00542                                                                   EL103
00543      MOVE PI-COMPANY-ID          TO CK-COMP-ID.                      CL**6
00544      MOVE USERCDI                TO CK-USER-CD.                      CL**6
00545      MOVE '2'                    TO CK-REC-TYPE.                     CL**6
00546                                                                   EL103
00547      PERFORM 7500-READ-CONTROL-FILE THRU 7500-EXIT.                  CL**6
00548                                                                      CL**4
00549      IF MORTGAGE-APP                                                 CL**7
00550          MOVE CONTROL-FILE       TO SAVE-PROCESSOR-ZERO              CL**7
00551          MOVE PI-READ-SW         TO CK-SEQ                           CL**7
00552          PERFORM 7500-READ-CONTROL-FILE THRU 7500-EXIT.              CL**7
00553                                                                      CL**7
00554      IF PI-USER-ALMIGHTY-YES                                         CL**4
00555             GO TO 7000-BUILD-OUTPUT-MAP.                             CL**4
00556                                                                   EL103
00557      IF CREDIT-APP                                                   CL**7
00558         IF CF-ADMINISTRATION-CONTROLS(1) EQUAL 'YN' OR 'YY'          CL**6
00559             NEXT SENTENCE                                         EL103
00560         ELSE                                                      EL103
00561             MOVE ER-0070                TO EMI-ERROR                 CL**7
00562             PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT               CL**7
00563             MOVE -1                     TO MAINTL                    CL**7
00564             MOVE AL-UABON               TO MAINTA                    CL**7
00565             GO TO 8200-SEND-DATAONLY.                                CL**7
00566                                                                   EL103
00567      IF CLAIMS-APP                                                   CL**7
00568         IF CF-ADMINISTRATION-CONTROLS(2) EQUAL 'YN' OR 'YY'          CL**6
00569             NEXT SENTENCE                                         EL103
00570         ELSE                                                      EL103
00571             MOVE ER-0070                TO EMI-ERROR                 CL**7
00572             PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT               CL**7
00573             MOVE -1                     TO MAINTL                    CL**7
00574             MOVE AL-UABON               TO MAINTA                    CL**7
00575             GO TO 8200-SEND-DATAONLY.                                CL**7
00576                                                                   EL103
00577      IF CREDIT-CARD-APP                                              CL**7
00578         IF CF-ADMINISTRATION-CONTROLS(3) EQUAL 'YN' OR 'YY'          CL**7
00579             NEXT SENTENCE                                            CL**7
00580         ELSE                                                         CL**7
00581             MOVE ER-0070                TO EMI-ERROR                 CL**7
00582             PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT               CL**7
00583             MOVE -1                     TO MAINTL                    CL**7
00584             MOVE AL-UABON               TO MAINTA                    CL**7
00585             GO TO 8200-SEND-DATAONLY.                                CL**7
00586                                                                      CL**7
00587      IF ACCT-RECV-APP                                                CL**7
00588         IF CF-ADMINISTRATION-CONTROLS(4) EQUAL 'YN' OR 'YY'          CL**7
00589             NEXT SENTENCE                                            CL**7
00590         ELSE                                                         CL**7
00591             MOVE ER-0070                TO EMI-ERROR                 CL**7
00592             PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT               CL**7
00593             MOVE -1                     TO MAINTL                    CL**7
00594             MOVE AL-UABON               TO MAINTA                    CL**7
00595             GO TO 8200-SEND-DATAONLY.                                CL**7
00596                                                                      CL**7
00597      IF MORTGAGE-APP                                                 CL**7
00598         IF CF-ADMINISTRATION-CONTROLS(1) EQUAL 'YN' OR 'YY'          CL**7
00599             NEXT SENTENCE                                            CL**7
00600         ELSE                                                         CL**7
00601             MOVE ER-0070                TO EMI-ERROR                 CL**7
00602             PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT               CL**7
00603             MOVE -1                     TO MAINTL                    CL**7
00604             MOVE AL-UABON               TO MAINTA                    CL**7
00605             GO TO 8200-SEND-DATAONLY.                                CL**7
00606                                                                      CL**6
00607      GO TO 7000-BUILD-OUTPUT-MAP.                                 EL103
00608                                                                      CL**6
00609      EJECT                                                        EL103
00610  2000-CHANGE-USER.                                                EL103
00611                                                                      CL**6
00612      IF USERCDL EQUAL ZERO                                           CL**6
00613          MOVE ER-0019            TO EMI-ERROR                        CL**6
00614          PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT               EL103
00615          MOVE -1                 TO USERCDL                          CL**6
00616          MOVE AL-UABON           TO USERCDA                          CL**6
00617          GO TO 8200-SEND-DATAONLY.                                EL103
00618                                                                   EL103
00619      IF USERCDI  NOT EQUAL PI-PREV-USER                              CL**6
00620          MOVE ER-0074            TO EMI-ERROR                        CL**6
00621          PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT               EL103
00622          MOVE -1                 TO USERCDL                          CL**6
00623          MOVE AL-UABON           TO USERCDA                          CL**6
00624          GO TO 8200-SEND-DATAONLY.                                EL103
00625                                                                   EL103
00626      PERFORM 6000-EDIT-INPUT-DATA  THRU  6999-EXIT.               EL103
00627                                                                   EL103
00628      IF NOT EMI-NO-ERRORS                                         EL103
00629          GO TO 8200-SEND-DATAONLY.                                EL103
00630                                                                   EL103
00631      MOVE PI-COMPANY-ID          TO CK-COMP-ID.                      CL**6
00632      MOVE USERCDI                TO CK-USER-CD.                      CL**6
00633      MOVE '2'                    TO CK-REC-TYPE.                     CL**6
00634                                                                   EL103
00635      PERFORM 7600-READ-CONTROL-FILE-UPDATE THRU 7600-EXIT.           CL**6
00636                                                                   EL103
00637      IF CF-LAST-MAINT-BY     NOT EQUAL PI-UPDATE-BY OR               CL**6
00638         CF-LAST-MAINT-HHMMSS NOT EQUAL PI-UPDATE-HHMMSS              CL**6
00639          EXEC CICS  UNLOCK                                        EL103
00640              DATASET  ('ELCNTL')                                  EL103
00641              END-EXEC                                             EL103
00642          MOVE ER-0068            TO EMI-ERROR                        CL**6
00643          PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT               EL103
00644          GO TO 1000-SHOW-USER.                                    EL103
00645                                                                   EL103
00646      MOVE 'B'                    TO JP-RECORD-TYPE.                  CL**6
00647      MOVE CONTROL-FILE           TO JP-RECORD-AREA.                  CL**6
00648                                                                   EL103
00649      PERFORM 8400-LOG-JOURNAL-RECORD  THRU  8499-EXIT.            EL103
00650                                                                   EL103
00651      IF USRPWDL GREATER ZERO                                         CL*19
00652          MOVE USRPWDI            TO CF-PROCESSOR-PASSWORD.           CL**7
00653                                                                      CL*19
00654      IF PI-USER-ALMIGHTY-YES                                         CL**7
00655          NEXT SENTENCE                                               CL**7
00656      ELSE                                                            CL**7
00657          GO TO 2200-JOURNAL-RECORDS.                                 CL**7
00658                                                                      CL*12
00659      IF NAMEL GREATER ZERO                                           CL*19
00660          MOVE NAMEI              TO CF-PROCESSOR-NAME.               CL**6
00661                                                                   EL103
00662      IF TITLEL GREATER ZERO                                          CL*19
00663          MOVE TITLEI             TO CF-PROCESSOR-TITLE.              CL**6
00664                                                                      CL**7
00665      IF USRPRNTL GREATER ZERO                                        CL*19
00666          MOVE USRPRNTI           TO CF-PROCESSOR-PRINTER.            CL**7
00667                                                                   EL103
00668      IF CURRTRML GREATER ZERO                                        CL*19
00669          MOVE CURRTRMI           TO CF-CURRENT-TERM-ON.              CL**6
00670                                                                      CL*12
00671      IF APPLVL GREATER ZERO                                          CL*19
00672          MOVE APPLVI             TO CF-APPROVAL-LEVEL.               CL*12
011812
011812     IF CSRL GREATER ZERO
011812         MOVE CSRI               TO CF-CSR-IND
011812     END-IF.
00673                                                                   EL103
00674      IF CREDITL GREATER ZERO                                         CL*19
00675          MOVE CREDITI            TO CF-PROC-SYS-ACCESS-CREDIT.       CL**6
00676                                                                   EL103
00677      IF CLAIMSL GREATER ZERO                                         CL*19
00678          MOVE CLAIMSI            TO CF-PROC-SYS-ACCESS-CLAIMS.       CL**6
00679                                                                      CL**6
00680      IF LIFEL GREATER ZERO                                           CL*19
00681          MOVE LIFEI              TO CF-PROC-SYS-ACCESS-LIFE.         CL**6
00682                                                                      CL**6
00683      IF LOGONL GREATER ZERO                                          CL*19
00684          MOVE LOGONI             TO CF-MESSAGE-AT-LOGON-CAP.         CL**6
00685                                                                      CL**6
00686      IF USRALML GREATER ZERO                                         CL*19
00687          MOVE USRALMI            TO CF-PROCESSOR-USER-ALMIGHTY.      CL**6
00688                                                                      CL*16
00689      IF LANGTYPL GREATER ZERO                                        CL*19
00690          MOVE LANGTYPI           TO CF-LANGUAGE-TYPE.                CL*16
00691                                                                      CL**6
00692      IF NOT CLAIMS-APP                                               CL**7
00693          GO TO 2050-SKIP-CLAIMS-UPDATE.                              CL**7
00694                                                                      CL**6
00695      IF QCDAYSL GREATER ZERO                                         CL*19
00696          MOVE QCDAYSI            TO CF-PROC-CALC-DAYS-TOL.           CL**6
00697                                                                      CL**6
00698      IF QCAMTL GREATER ZERO                                          CL*19
00699          MOVE QCAMTI             TO CF-PROC-CALC-AMT-TOL.            CL**6
00700                                                                      CL**6
00701      IF MAXDAYL GREATER ZERO                                         CL*19
00702          MOVE MAXDAYI            TO CF-PROC-MAX-REG-DAYS.            CL**6
00703                                                                      CL**6
00704      IF MXAHAMTL GREATER ZERO                                        CL*19
00705          MOVE MXAHAMTI           TO CF-PROC-MAX-REG-PMT.             CL**6
00706                                                                      CL**6
00707      IF MXLFAMTL GREATER ZERO                                        CL*19
00708          MOVE MXLFAMTI           TO CF-PROC-MAX-LF-PMT.              CL**6
00709                                                                      CL**6
00710      IF AUTMONL GREATER ZERO                                         CL*19
00711          MOVE AUTMONI            TO CF-PROC-MAX-AUTO-MOS.            CL**6
00712                                                                      CL**6
00713      IF AUTAMTL GREATER ZERO                                         CL*19
00714          MOVE AUTAMTI            TO CF-PROC-MAX-AUTO-PMT.            CL**6
00715                                                                      CL**6
00716      IF MAXEXPL GREATER ZERO                                         CL*19
00717          MOVE MAXEXPI            TO CF-PROC-MAX-EXP-PMT.             CL*15
00718                                                                      CL*15
00719  2050-SKIP-CLAIMS-UPDATE.                                            CL**7
00720                                                                      CL**7
00721      IF READ-PROCESSOR-ONE                                           CL**7
00722          MOVE PI-PROCESSOR-ID    TO CF-LAST-MAINT-BY                 CL**7
00723          MOVE EIBTIME            TO CF-LAST-MAINT-HHMMSS             CL**7
00724          MOVE SAVE-BIN-DATE      TO CF-LAST-MAINT-DT                 CL**7
00725          MOVE 'C'                TO JP-RECORD-TYPE                   CL**7
00726          MOVE CONTROL-FILE       TO JP-RECORD-AREA                   CL**7
00727          PERFORM 7800-REWRITE-CONTROL-FILE THRU 7800-EXIT            CL**7
00728          PERFORM 8400-LOG-JOURNAL-RECORD  THRU  8499-EXIT            CL**7
00729          MOVE 1                  TO CK-SEQ                           CL*12
00730          PERFORM 7600-READ-CONTROL-FILE-UPDATE THRU 7600-EXIT        CL**7
00731          MOVE 'B'                TO JP-RECORD-TYPE                   CL**7
00732          MOVE CONTROL-FILE       TO JP-RECORD-AREA                   CL**7
00733          PERFORM 8400-LOG-JOURNAL-RECORD  THRU  8499-EXIT.           CL**7
00734                                                                      CL**7
00735      MOVE PI-APPLICATION         TO SYS.                             CL**7
00736                                                                      CL**7
00737      IF SLCTSYSL GREATER ZERO                                        CL*19
00738          MOVE SLCTSYSI           TO                                  CL**7
00739                                  CF-ADMINISTRATION-CONTROLS(SYS).    CL**7
00740                                                                      CL**7
00741      IF FORCEL GREATER ZERO                                          CL*19
00742          MOVE FORCEI             TO CF-APPLICATION-FORCE(SYS).       CL**7
00743                                                                      CL**7
00744      MOVE 1                      TO SLOT.                            CL*12
00745                                                                      CL**7
00746  2100-APPLICATION-LOOP.                                              CL**7
00747                                                                      CL**7
00748      IF APP-LENGTH(SLOT) GREATER ZERO                                CL*19
00749          MOVE APP(SLOT)          TO CF-APP-SWITCHES(SYS SLOT).       CL**7
00750                                                                      CL**7
00751      IF SLOT LESS THAN MAXSLOT                                       CL**7
00752          ADD 1                   TO SLOT                             CL*12
00753          GO TO 2100-APPLICATION-LOOP.                                CL**7
00754                                                                      CL**7
00755      IF ACCTL GREATER ZERO                                           CL*19
00756          MOVE ACCTI              TO CF-PROCESSOR-ACCOUNT.            CL**7
00757                                                                      CL**7
00758      IF CARRL GREATER ZERO                                           CL*19
00759          MOVE CARRI              TO CF-PROCESSOR-CARRIER.            CL**7
00760                                                                      CL**7
00761  2200-JOURNAL-RECORDS.                                               CL**6
00762                                                                      CL**6
00763      MOVE PI-PROCESSOR-ID        TO CF-LAST-MAINT-BY.                CL**6
00764      MOVE EIBTIME                TO CF-LAST-MAINT-HHMMSS.            CL**6
00765      MOVE SAVE-BIN-DATE          TO CF-LAST-MAINT-DT.                CL**7
00766      MOVE 'C'                    TO JP-RECORD-TYPE.                  CL**6
00767      MOVE CONTROL-FILE           TO JP-RECORD-AREA.                  CL**6
00768                                                                   EL103
00769      PERFORM 7800-REWRITE-CONTROL-FILE THRU 7800-EXIT                CL**7
00770                                                                   EL103
00771      PERFORM 8400-LOG-JOURNAL-RECORD  THRU  8499-EXIT.            EL103
00772                                                                   EL103
00773      IF PI-USER-ALMIGHTY-YES                                         CL*10
00774          MOVE ER-0000            TO EMI-ERROR                        CL*10
00775      ELSE                                                            CL*10
00776          MOVE ER-7089            TO EMI-ERROR.                       CL*10
00777                                                                   EL103
00778      PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT.                  EL103
00779                                                                   EL103
00780      MOVE SPACES                 TO PI-PREV-USER.                    CL**6
00781      MOVE ZERO                   TO CK-SEQ.                          CL**7
00782      GO TO 1000-SHOW-USER.                                           CL**7
00783                                                                      CL**6
00784      EJECT                                                        EL103
00785  3000-ADD-USER.                                                   EL103
00786                                                                      CL**6
00787      IF USERCDL  EQUAL ZERO                                          CL**6
00788          MOVE ER-0019            TO EMI-ERROR                        CL**6
00789          PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT               EL103
00790          MOVE -1                 TO USERCDL                          CL**6
00791          MOVE AL-UABON           TO USERCDA                          CL**6
00792          GO TO 8200-SEND-DATAONLY.                                EL103
00793                                                                   EL103
00794      IF USERCDI  EQUAL SPACES OR LOW-VALUES                          CL**6
00795          MOVE ER-0019            TO EMI-ERROR                        CL**6
00796          PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT               EL103
00797          MOVE -1                 TO USERCDL                          CL**6
00798          MOVE AL-UABON           TO USERCDA                          CL**6
00799          GO TO 8200-SEND-DATAONLY.                                EL103
00800                                                                   EL103
00801 ***  IF USERCDI  EQUAL 'LGXX'                                        CL*21
00802 ***      MOVE ER-0425            TO EMI-ERROR                        CL*21
00803 ***      PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT                  CL*21
00804 ***      MOVE -1                 TO USERCDL                          CL*21
00805 ***      MOVE AL-UABON           TO USERCDA                          CL*21
00806 ***      GO TO 8200-SEND-DATAONLY.                                   CL*21
00807                                                                   EL103
00808      IF USRPWDL  EQUAL ZERO                                          CL**6
00809          MOVE ER-0079            TO EMI-ERROR                        CL**6
00810          PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT               EL103
00811          MOVE -1                 TO USRPWDL                          CL**6
00812          MOVE AL-UADON           TO USRPWDA                          CL*20
00813      ELSE                                                            CL**6
00814          MOVE AL-UADON           TO USRPWDA.                         CL*20
00815                                                                      CL**6
00816      IF CURRTRML  EQUAL ZERO OR                                      CL**6
00817         CURRTRMI  EQUAL SPACES                                       CL**6
00818          MOVE AL-UANOF           TO CURRTRMA                         CL**6
00819      ELSE                                                            CL**6
00820          MOVE ER-0080            TO EMI-ERROR                        CL**6
00821          PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT                  CL**6
00822          MOVE -1                 TO CURRTRML                         CL**6
00823          MOVE AL-UABON           TO CURRTRMA.                        CL**6
00824                                                                   EL103
00825      PERFORM 6000-EDIT-INPUT-DATA  THRU  6999-EXIT.               EL103
00826                                                                   EL103
00827      IF NOT EMI-NO-ERRORS                                         EL103
00828          GO TO 8200-SEND-DATAONLY.                                EL103
00829                                                                   EL103
00830      EXEC CICS  GETMAIN                                           EL103
00831          SET      (ADDRESS OF CONTROL-FILE)                          CL*17
00832          LENGTH   (750)                                              CL*11
00833          INITIMG  (GETMAIN-SPACE)                                 EL103
00834      END-EXEC.                                                       CL*12
00835                                                                   EL103
00836      MOVE 'CF'                   TO CF-RECORD-ID.                    CL**6
00837      MOVE PI-COMPANY-ID          TO CF-COMPANY-ID.                   CL**6
00838      MOVE '2'                    TO CF-RECORD-TYPE.                  CL**6
00839      MOVE USERCDI                TO CF-PROCESSOR                     CL**7
00840                                     CK-USER-CD.                      CL**7
00841      MOVE +0                     TO CF-SEQUENCE-NO.                  CL**6
00842      MOVE SAVE-BIN-DATE          TO CF-LAST-MAINT-DT.                CL**7
00843      MOVE PI-PROCESSOR-ID        TO CF-LAST-MAINT-BY.                CL**6
00844      MOVE EIBTIME                TO CF-LAST-MAINT-HHMMSS.            CL**6
00845      MOVE +0                     TO CF-PROC-CALC-AMT-TOL             CL**6
00846                                     CF-PROC-MAX-REG-PMT              CL**6
00847                                     CF-PROC-MAX-LF-PMT               CL**6
00848                                     CF-PROC-MAX-REG-DAYS             CL**6
00849                                     CF-PROC-MAX-AUTO-PMT             CL**6
00850                                     CF-PROC-MAX-AUTO-MOS             CL**6
00851                                     CF-PROC-CALC-DAYS-TOL            CL*19
00852                                     CF-PROC-MAX-EXP-PMT.             CL*19
00853                                                                   EL103
00854  3300-INITIALIZE-RECORD.                                             CL**6
00855                                                                   EL103
00856      MOVE 1                      TO SYS                              CL*12
00857                                     SLOT.                            CL**6
00858                                                                   EL103
00859  3310-SYSTEM-INITIALIZATION.                                         CL**6
00860                                                                   EL103
00861      MOVE 'NN'                   TO                                  CL**6
00862                                  CF-ADMINISTRATION-CONTROLS(SYS).    CL**6
00863                                                                      CL*19
00864      MOVE 'N'                TO  CF-APPLICATION-FORCE (SYS)          CL*19
00865                                  CF-PROC-SYS-ACCESS-CLAIMS           CL*19
00866                                  CF-PROC-SYS-ACCESS-CREDIT           CL*19
00867                                  CF-MESSAGE-AT-LOGON-CAP             CL*19
00868                                  CF-PROCESSOR-USER-ALMIGHTY          CL*19
00869                                  CF-LANGUAGE-TYPE                    CL*19
00870                                  CF-PROC-SYS-ACCESS-LIFE.            CL*19
00871                                                                   EL103
00872  3320-APP-INITIALIZATION.                                            CL**6
00873                                                                   EL103
00874      MOVE 'NN'                   TO CF-APP-SWITCHES(SYS SLOT).       CL**6
00875                                                                      CL**6
00876      IF SLOT LESS THAN MAXSLOT                                       CL**6
00877          ADD 1                   TO SLOT                             CL*12
00878          GO TO 3320-APP-INITIALIZATION.                              CL**6
00879                                                                      CL**6
00880      IF SYS LESS THAN MAXSYS                                         CL**6
00881          ADD 1                   TO SYS                              CL*12
00882          MOVE 1                  TO SLOT                             CL*12
00883          GO TO 3310-SYSTEM-INITIALIZATION.                           CL**6
00884                                                                      CL**6
00885      MOVE CONTROL-FILE           TO SAVE-PROCESSOR-ONE.              CL**7
00886      MOVE PI-APPLICATION         TO SYS.                             CL**6
00887                                                                      CL**6
00888      IF NAMEL GREATER ZERO                                           CL*19
00889          MOVE NAMEI              TO CF-PROCESSOR-NAME.               CL**7
00890                                                                      CL**6
00891      IF TITLEL GREATER ZERO                                          CL*19
00892          MOVE TITLEI             TO CF-PROCESSOR-TITLE.              CL**7
00893                                                                      CL**6
00894      IF USRPWDL GREATER ZERO                                         CL*19
00895          MOVE USRPWDI            TO CF-PROCESSOR-PASSWORD.           CL**7
00896                                                                      CL**6
00897      IF USRPRNTL GREATER ZERO                                        CL*19
00898          MOVE USRPRNTI           TO CF-PROCESSOR-PRINTER.            CL**7
00899                                                                      CL**6
00900      IF CURRTRML GREATER ZERO                                        CL*19
00901          MOVE CURRTRMI           TO CF-CURRENT-TERM-ON.              CL**7
00902                                                                      CL**6
00903      IF CREDITL GREATER ZERO                                         CL*19
00904          MOVE CREDITI            TO CF-PROC-SYS-ACCESS-CREDIT.       CL**7
00905                                                                      CL*16
00906      IF LANGTYPL GREATER ZERO                                        CL*19
00907          MOVE LANGTYPI           TO CF-LANGUAGE-TYPE.                CL*16
00908                                                                      CL**6
00909      IF CLAIMSL GREATER ZERO                                         CL*19
00910          MOVE CLAIMSI            TO CF-PROC-SYS-ACCESS-CLAIMS.       CL**7
00911                                                                      CL**7
00912      IF LIFEL GREATER ZERO                                           CL*19
00913          MOVE LIFEI              TO CF-PROC-SYS-ACCESS-LIFE.         CL**7
00914                                                                      CL**7
00915      IF LOGONL GREATER ZERO                                          CL*19
00916          MOVE LOGONI             TO CF-MESSAGE-AT-LOGON-CAP.         CL**7
00917                                                                      CL**7
00918      IF USRALML GREATER ZERO                                         CL*19
00919          MOVE USRALMI            TO CF-PROCESSOR-USER-ALMIGHTY.      CL**7
00920                                                                      CL**7
00921      IF APPLVL GREATER ZERO                                          CL*19
00922          MOVE APPLVI             TO CF-APPROVAL-LEVEL.               CL*12
011812
011812     IF CSRL GREATER ZERO
011812         MOVE CSRI               TO CF-CSR-IND
011812     END-IF.
00923                                                                      CL*12
00924 **************************************************************       CL**7
00925                                                                      CL**6
00926      IF SYS NOT EQUAL 2                                              CL*12
00927          GO TO 3350-DONT-BUILD-CLAIMS.                               CL**7
00928                                                                      CL**7
00929 *************   RELEVANT TO CLAIMS SYSTEM ONLY   *************       CL**7
00930                                                                      CL**6
00931      IF QCDAYSL GREATER ZERO                                         CL*19
00932          MOVE QCDAYSI            TO CF-PROC-CALC-DAYS-TOL.           CL**6
00933                                                                      CL**6
00934      IF QCAMTL GREATER ZERO                                          CL*19
00935          MOVE QCAMTI             TO CF-PROC-CALC-AMT-TOL.            CL**6
00936                                                                      CL**6
00937      IF MAXDAYL GREATER ZERO                                         CL*19
00938          MOVE MAXDAYI            TO CF-PROC-MAX-REG-DAYS.            CL**6
00939                                                                      CL**6
00940      IF MXAHAMTL GREATER ZERO                                        CL*19
00941          MOVE MXAHAMTI           TO CF-PROC-MAX-REG-PMT.             CL**6
00942                                                                      CL**6
00943      IF MXLFAMTL GREATER ZERO                                        CL*19
00944          MOVE MXLFAMTI           TO CF-PROC-MAX-LF-PMT.              CL**6
00945                                                                      CL**6
00946      IF AUTMONL GREATER ZERO                                         CL*19
00947          MOVE AUTMONI            TO CF-PROC-MAX-AUTO-MOS.            CL**6
00948                                                                      CL**6
00949      IF AUTAMTL GREATER ZERO                                         CL*19
00950          MOVE AUTAMTI            TO CF-PROC-MAX-AUTO-PMT.            CL**6
00951                                                                      CL*15
00952      IF MAXEXPL GREATER ZERO                                         CL*19
00953          MOVE MAXEXPI            TO CF-PROC-MAX-EXP-PMT.             CL*15
00954                                                                      CL**6
00955  3350-DONT-BUILD-CLAIMS.                                             CL**7
00956                                                                      CL**7
00957      IF READ-PROCESSOR-ONE                                           CL**7
00958          MOVE 'A'                TO JP-RECORD-TYPE                   CL**7
00959          MOVE CONTROL-FILE       TO JP-RECORD-AREA                   CL**7
00960                                     SAVE-PROCESSOR-ZERO              CL**7
00961          PERFORM 7700-WRITE-CONTROL-FILE THRU 7700-EXIT              CL**7
00962          PERFORM 8400-LOG-JOURNAL-RECORD  THRU  8499-EXIT            CL**7
00963          MOVE SAVE-PROCESSOR-ONE TO CONTROL-FILE                     CL**7
00964          MOVE PI-READ-SW         TO CF-SEQUENCE-NO.                  CL**7
00965                                                                      CL**7
00966      IF SLCTSYSL GREATER ZERO                                        CL*19
00967          MOVE SLCTSYSI           TO                                  CL**7
00968                                  CF-ADMINISTRATION-CONTROLS(SYS).    CL**7
00969                                                                      CL**7
00970      IF FORCEL GREATER ZERO                                          CL*19
00971          MOVE FORCEI             TO CF-APPLICATION-FORCE(SYS).       CL**7
00972                                                                      CL**7
00973      MOVE 1                      TO SLOT.                            CL*12
00974                                                                      CL**7
00975  3400-APPLICATION-LOOP.                                              CL**7
00976                                                                      CL**7
00977      IF APP-LENGTH(SLOT) GREATER ZERO                                CL*19
00978          MOVE APP(SLOT)          TO CF-APP-SWITCHES(SYS SLOT).       CL**7
00979                                                                      CL**7
00980      IF SLOT LESS THAN MAXSLOT                                       CL**7
00981          ADD 1                   TO SLOT                             CL*12
00982          GO TO 3400-APPLICATION-LOOP.                                CL**7
00983                                                                      CL**7
00984      IF CARRL GREATER ZERO                                           CL*19
00985          MOVE CARRI              TO CF-PROCESSOR-CARRIER.            CL**7
00986                                                                      CL**7
00987      IF ACCTL GREATER ZERO                                           CL*19
00988          MOVE ACCTI              TO CF-PROCESSOR-ACCOUNT.            CL**7
00989                                                                      CL**7
00990  3500-JOURNAL-RECORDS.                                               CL**6
00991                                                                      CL**6
00992      MOVE 'A'                    TO JP-RECORD-TYPE.                  CL**6
00993      MOVE CONTROL-FILE           TO JP-RECORD-AREA.                  CL**6
00994                                                                   EL103
00995      PERFORM 7700-WRITE-CONTROL-FILE THRU 7700-EXIT.                 CL**7
00996                                                                   EL103
00997      PERFORM 8400-LOG-JOURNAL-RECORD  THRU  8499-EXIT.            EL103
00998                                                                   EL103
00999      IF READ-PROCESSOR-ZERO                                          CL**7
01000          MOVE 'A'                TO JP-RECORD-TYPE                   CL**7
01001          MOVE CONTROL-FILE       TO SAVE-PROCESSOR-ZERO              CL**7
01002          MOVE SAVE-PROCESSOR-ONE TO CONTROL-FILE                     CL**7
01003          MOVE 1                  TO CF-SEQUENCE-NO                   CL*12
01004          MOVE CONTROL-FILE       TO JP-RECORD-AREA                   CL**7
01005          PERFORM 7700-WRITE-CONTROL-FILE THRU 7700-EXIT              CL**7
01006          PERFORM 8400-LOG-JOURNAL-RECORD  THRU  8499-EXIT.           CL**7
01007                                                                      CL**7
01008  3600-ADD-REMINDERS.                                                 CL**6
01009                                                                      CL**6
01010      MOVE 'R'                    TO CF-RECORD-TYPE.                  CL**6
01011      MOVE +0                     TO CF-SEQUENCE-NO.                  CL**6
01012      MOVE SPACES                 TO CF-RECORD-BODY.                  CL**6
01013      MOVE LOW-VALUES             TO CF-START-REMIND-DT (1)           CL**6
01014                                     CF-START-REMIND-DT (2)           CL**7
01015                                     CF-START-REMIND-DT (3)           CL**7
01016                                     CF-START-REMIND-DT (4)           CL**7
01017                                     CF-START-REMIND-DT (5)           CL**7
01018                                     CF-START-REMIND-DT (6)           CL**7
01019                                     CF-START-REMIND-DT (7)           CL**7
01020                                     CF-START-REMIND-DT (8)           CL**7
01021                                     CF-END-REMIND-DT (1)             CL**7
01022                                     CF-END-REMIND-DT (2)             CL**7
01023                                     CF-END-REMIND-DT (3)             CL**7
01024                                     CF-END-REMIND-DT (4)             CL**7
01025                                     CF-END-REMIND-DT (5)             CL**7
01026                                     CF-END-REMIND-DT (6)             CL**7
01027                                     CF-END-REMIND-DT (7)             CL**7
01028                                     CF-END-REMIND-DT (8).            CL**7
01029                                                                   EL103
01030      MOVE 'A'                    TO JP-RECORD-TYPE.                  CL**6
01031      MOVE CONTROL-FILE           TO JP-RECORD-AREA.                  CL**6
01032                                                                   EL103
01033      PERFORM 7700-WRITE-CONTROL-FILE THRU 7700-EXIT.                 CL**7
01034                                                                   EL103
01035      PERFORM 8400-LOG-JOURNAL-RECORD  THRU  8499-EXIT.            EL103
01036                                                                   EL103
01037      MOVE ER-0000                TO EMI-ERROR.                       CL**6
01038      PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT.                  EL103
01039                                                                   EL103
01040      GO TO 1000-SHOW-USER.                                           CL**7
01041                                                                      CL**6
01042      EJECT                                                        EL103
01043  4000-DELETE-USER.                                                EL103
01044                                                                   EL103
01045      EXEC CICS HANDLE CONDITION                                      CL**9
01046          NOTFND     (4030-END-OF-DELETE)                             CL*12
01047      END-EXEC.                                                       CL*12
01048                                                                      CL**9
01049      IF USERCDL  EQUAL ZERO                                          CL**6
01050          MOVE ER-0019            TO EMI-ERROR                        CL**6
01051          PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT               EL103
01052          MOVE -1                 TO USERCDL                          CL**6
01053          MOVE AL-UABON           TO USERCDA                          CL**6
01054          GO TO 8200-SEND-DATAONLY.                                EL103
01055                                                                   EL103
01056      IF USERCDI  NOT EQUAL PI-PREV-USER                              CL**6
01057          MOVE ER-0074            TO EMI-ERROR                        CL**6
01058          PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT               EL103
01059          MOVE -1                 TO USERCDL                          CL**6
01060          MOVE AL-UABON           TO USERCDA                          CL**6
01061          GO TO 8200-SEND-DATAONLY.                                EL103
01062                                                                   EL103
01063      IF USERCDI  EQUAL PI-PROCESSOR-ID                               CL**6
01064          MOVE ER-0083            TO EMI-ERROR                        CL**6
01065          PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT                  CL**6
01066          MOVE -1                 TO MAINTL                           CL**6
01067          GO TO 8200-SEND-DATAONLY.                                   CL**6
01068                                                                   EL103
01069      MOVE PI-COMPANY-ID          TO CK-COMP-ID.                      CL**6
01070      MOVE USERCDI                TO CK-USER-CD.                      CL**6
01071      MOVE '2'                    TO CK-REC-TYPE.                     CL**6
01072                                                                   EL103
01073      PERFORM 7600-READ-CONTROL-FILE-UPDATE THRU 7600-EXIT.           CL**6
01074                                                                   EL103
01075      IF CF-LAST-MAINT-BY      NOT EQUAL PI-UPDATE-BY OR              CL**6
01076         CF-LAST-MAINT-HHMMSS  NOT EQUAL PI-UPDATE-HHMMSS             CL**6
01077          MOVE ER-0068            TO EMI-ERROR                        CL**6
01078          PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT               EL103
01079          GO TO 4050-DELETE-UNLOCK.                                EL103
01080                                                                   EL103
01081      IF CF-CURRENT-TERM-ON  NOT EQUAL SPACES                         CL**6
01082          MOVE ER-0084            TO EMI-ERROR                        CL**6
01083          PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT               EL103
01084          GO TO 4050-DELETE-UNLOCK.                                EL103
01085                                                                   EL103
01086      MOVE 'D'                    TO JP-RECORD-TYPE.                  CL**6
01087      MOVE CONTROL-FILE           TO JP-RECORD-AREA.                  CL**6
01088                                                                   EL103
01089      PERFORM 7900-DELETE-CONTROL-FILE THRU 7900-EXIT.                CL**7
01090                                                                      CL**7
01091      PERFORM 8400-LOG-JOURNAL-RECORD  THRU  8499-EXIT.               CL**7
01092                                                                      CL**7
01093      MOVE 1                      TO CK-SEQ.                          CL*12
01094      PERFORM 7600-READ-CONTROL-FILE-UPDATE THRU 7600-EXIT.           CL**7
01095      MOVE CONTROL-FILE           TO JP-RECORD-AREA.                  CL**7
01096                                                                      CL**7
01097      PERFORM 7900-DELETE-CONTROL-FILE THRU 7900-EXIT.                CL**7
01098                                                                   EL103
01099      PERFORM 8400-LOG-JOURNAL-RECORD  THRU  8499-EXIT.            EL103
01100                                                                   EL103
01101  4020-DELETE-REMINDERS.                                           EL103
01102                                                                      CL**7
01103      MOVE 'R'                    TO CK-REC-TYPE.                     CL**6
01104      MOVE ZERO                   TO CK-SEQ.                          CL**7
01105                                                                   EL103
01106      PERFORM 7600-READ-CONTROL-FILE-UPDATE THRU 7600-EXIT.           CL**6
01107                                                                   EL103
01108      MOVE 'D'                    TO JP-RECORD-TYPE.                  CL**6
01109      MOVE CONTROL-FILE           TO JP-RECORD-AREA.                  CL**6
01110                                                                   EL103
01111      PERFORM 7900-DELETE-CONTROL-FILE THRU 7900-EXIT.                CL**7
01112                                                                   EL103
01113      PERFORM 8400-LOG-JOURNAL-RECORD  THRU  8499-EXIT.               CL**7
01114                                                                   EL103
01115  4030-END-OF-DELETE.                                                 CL**9
01116                                                                      CL**9
01117      MOVE ER-9014                TO EMI-ERROR.                       CL**7
01118      PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT.                     CL**7
01119                                                                      CL**7
01120      MOVE LOW-VALUES             TO EL103AO.                         CL**6
01121      MOVE -1                     TO MAINTL.                          CL**6
01122      MOVE AL-UANOF               TO MAINTA.                          CL**7
01123      MOVE SPACES                 TO MAINTI                           CL**7
01124                                     PI-PREV-USER.                    CL**7
01125      MOVE CK-USER-CD             TO USERCDO.                         CL**6
01126      MOVE AL-UANON               TO USERCDA.                         CL**6
01127                                                                   EL103
01128      GO TO 8100-SEND-INITIAL-MAP.                                 EL103
01129                                                                   EL103
01130  4050-DELETE-UNLOCK.                                              EL103
01131      EXEC CICS  UNLOCK                                            EL103
01132          DATASET  ('ELCNTL')                                      EL103
01133      END-EXEC                                                        CL*12
01134                                                                   EL103
01135      GO TO 1000-SHOW-USER.                                        EL103
01136                                                                      CL**6
01137      EJECT                                                        EL103
01138  5000-FIND-NEXT-USER.                                             EL103
01139                                                                   EL103
01140      MOVE PI-COMPANY-ID          TO CK-COMP-ID.                      CL**6
01141      MOVE 2                      TO CK-SEQ.                          CL*12
01142                                                                   EL103
01143      IF USERCDL  EQUAL ZERO                                          CL**6
01144          MOVE PI-PREV-USER       TO CK-USER-CD                       CL**7
01145      ELSE                                                            CL**6
01146          MOVE USERCDI            TO CK-USER-CD.                      CL**7
01147                                                                      CL**6
01148      MOVE '2'                    TO CK-REC-TYPE.                     CL**6
01149                                                                   EL103
01150      EXEC CICS  HANDLE CONDITION                                  EL103
01151          NOTFND  (5010-USER-NOT-FOUND)                               CL**7
01152      END-EXEC.                                                       CL*12
01153                                                                   EL103
01154      PERFORM 7510-READ-CONTROL-FILE-GTEQ THRU 7510-EXIT.             CL**7
01155                                                                   EL103
01156      IF CF-COMPANY-ID   NOT EQUAL PI-COMPANY-ID OR                   CL**6
01157         CF-RECORD-TYPE  NOT EQUAL '2'                                CL**6
01158         GO TO 5010-USER-NOT-FOUND.                                   CL**7
01159                                                                   EL103
01160      IF READ-PROCESSOR-ONE                                           CL**7
01161          MOVE CONTROL-FILE       TO SAVE-PROCESSOR-ZERO              CL**7
01162          MOVE CF-CONTROL-PRIMARY TO ELCNTL-KEY                       CL**7
01163          ADD +1                  TO CK-SEQ                           CL**7
01164          PERFORM 7500-READ-CONTROL-FILE THRU 7500-EXIT.              CL**7
01165                                                                      CL**7
01166      IF PI-PREV-USER EQUAL SPACES                                    CL**7
01167          MOVE ER-0085            TO EMI-ERROR                        CL**6
01168          PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT.              EL103
01169                                                                   EL103
01170      GO TO 7000-BUILD-OUTPUT-MAP.                                 EL103
01171                                                                      CL**6
01172  5010-USER-NOT-FOUND.                                                CL**7
01173                                                                      CL**7
01174      GO TO 8860-ENDFILE.                                             CL**7
01175                                                                      CL**7
01176      EJECT                                                           CL**6
01177  5100-FIND-PREV-USER.                                                CL**6
01178                                                                      CL**6
01179      MOVE '1'                    TO READ-PREV-SW.                    CL**7
01180      MOVE PI-COMPANY-ID          TO CK-COMP-ID.                      CL**6
01181                                                                      CL**6
01182      IF USERCDL  EQUAL ZERO                                          CL**6
01183          MOVE PI-PREV-USER       TO CK-USER-CD                       CL**7
01184      ELSE                                                            CL**6
01185          MOVE USERCDI            TO CK-USER-CD.                      CL**6
01186                                                                      CL**6
01187      MOVE ZERO                   TO CK-SEQ.                          CL**6
01188      MOVE '2'                    TO CK-REC-TYPE.                     CL**6
01189                                                                      CL**6
01190      EXEC CICS  HANDLE CONDITION                                     CL**6
01191          NOTFND  (8860-ENDFILE)                                      CL**6
01192      END-EXEC.                                                       CL*12
01193                                                                      CL**6
01194      EXEC CICS STARTBR                                               CL**6
01195          DATASET  ('ELCNTL')                                         CL**6
01196          RIDFLD   (ELCNTL-KEY)                                       CL**6
01197          GTEQ                                                        CL**6
01198      END-EXEC.                                                       CL*12
01199                                                                      CL**6
01200  5100-READPREV-LOOP.                                                 CL**6
01201                                                                      CL**6
01202      EXEC CICS  READPREV                                             CL**6
01203          DATASET  ('ELCNTL')                                         CL**6
01204          SET      (ADDRESS OF CONTROL-FILE)                          CL*17
01205          RIDFLD   (ELCNTL-KEY)                                       CL**6
01206      END-EXEC.                                                       CL*12
01207                                                                      CL**6
01208      IF CF-COMPANY-ID   NOT EQUAL PI-COMPANY-ID OR                   CL**6
01209         CF-RECORD-TYPE  NOT EQUAL '2'                                CL**6
01210         PERFORM 5100-END-BROWSE                                      CL**6
01211         GO TO 8860-ENDFILE.                                          CL**6
01212                                                                      CL**6
01213      IF FIRST-READ-PREV                                              CL**6
01214          MOVE '2'                TO READ-PREV-SW                     CL**7
01215          GO TO 5100-READPREV-LOOP.                                   CL**6
01216                                                                      CL**7
01217      IF SECOND-READ-PREV                                             CL**7
01218          MOVE '3'                TO READ-PREV-SW                     CL**7
01219          IF READ-PROCESSOR-ONE                                       CL**7
01220              MOVE CONTROL-FILE   TO SAVE-PROCESSOR-ONE               CL**7
01221              GO TO 5100-READPREV-LOOP                                CL**7
01222          ELSE                                                        CL**7
01223              GO TO 5100-READPREV-LOOP.                               CL**7
01224                                                                      CL**6
01225  5100-END-BROWSE.                                                    CL**6
01226                                                                      CL**6
01227      EXEC CICS ENDBR                                                 CL**6
01228          DATASET ('ELCNTL')                                          CL**6
01229      END-EXEC.                                                       CL**6
01230                                                                      CL**6
01231  5100-EXIT-ROUTINE.                                                  CL**6
01232                                                                      CL**6
01233      PERFORM 7500-READ-CONTROL-FILE THRU 7500-EXIT.                  CL**6
01234                                                                      CL**7
01235      IF READ-PROCESSOR-ONE                                           CL**7
01236          MOVE CONTROL-FILE       TO SAVE-PROCESSOR-ZERO              CL**7
01237          MOVE SAVE-PROCESSOR-ONE TO CONTROL-FILE.                    CL**7
01238                                                                      CL**6
01239      GO TO 7000-BUILD-OUTPUT-MAP.                                    CL**6
01240                                                                      CL**6
01241      EJECT                                                        EL103
01242  6000-EDIT-INPUT-DATA.                                            EL103
01243                                                                      CL**6
01244      IF CARRL  EQUAL ZERO                                            CL**6
01245          GO TO 6200-EDIT-ACCT                                     EL103
01246      ELSE                                                         EL103
01247          IF CARRI  EQUAL SPACES                                      CL**6
01248              MOVE SPACES         TO ACCTI                            CL**6
01249              MOVE AL-UANON       TO CARRA                            CL**6
01250                                     ACCTA                            CL**6
01251              GO TO 6300-CONT.                                     EL103
01252                                                                   EL103
01253      MOVE PI-COMPANY-ID          TO CK-COMP-ID.                      CL*12
01254      MOVE SPACES                 TO CK-CARRIER.                      CL*12
01255      MOVE CARRI                  TO CK-CARR.                         CL*12
01256      MOVE '6'                    TO CK-REC-TYPE.                     CL*12
01257                                                                   EL103
01258      EXEC CICS  HANDLE CONDITION                                  EL103
01259          NOTFND  (6100-CARRIER-MISSING)                           EL103
01260      END-EXEC.                                                       CL*12
01261                                                                   EL103
01262      PERFORM 7500-READ-CONTROL-FILE THRU 7500-EXIT.                  CL**6
01263                                                                   EL103
01264      MOVE AL-UANON               TO CARRA.                           CL**6
01265                                                                   EL103
01266      GO TO 6200-EDIT-ACCT.                                        EL103
01267                                                                   EL103
01268  6100-CARRIER-MISSING.                                            EL103
01269                                                                   EL103
01270      MOVE -1                     TO CARRL.                           CL*12
01271      MOVE AL-UABON               TO CARRA.                           CL*12
01272      MOVE ER-0252                TO EMI-ERROR.                       CL*12
01273      PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT.                  EL103
01274                                                                   EL103
01275  6200-EDIT-ACCT.                                                  EL103
01276                                                                      CL**6
01277      IF NOT MORTGAGE-SESSION                                         CL**7
01278          IF ACCTL GREATER ZERO                                       CL**7
01279              IF CARRL GREATER ZERO                                   CL**7
01280                  MOVE AL-UANON       TO ACCTA                        CL**7
01281              ELSE                                                 EL103
01282                  IF ST-ACCNT-CNTL OR ACCNT-CNTL                      CL**7
01283                      NEXT SENTENCE                                   CL**7
01284                  ELSE                                                CL**7
01285                      MOVE ER-2376    TO EMI-ERROR                    CL**7
01286                      MOVE -1         TO CARRL                        CL**7
01287                      MOVE AL-UABON   TO CARRA                        CL**7
01288                      PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT.     CL*12
01289 *    ELSE                                                            CL*12
01290 *        MOVE AL-UANON               TO ACCTA.                       CL*12
01291                                                                   EL103
01292  6300-CONT.                                                       EL103
01293                                                                      CL**6
01294      IF CURRTRML GREATER ZERO                                        CL*19
01295          MOVE CURRTRMI           TO TERMINAL-TEST-AREA               CL**6
01296          IF TERMINAL-TEST-AREA  EQUAL SPACES                         CL**6
01297              MOVE AL-UANON       TO CURRTRMA                         CL**6
01298          ELSE                                                     EL103
01299              IF ' '  EQUAL TTA-1 OR TTA-2 OR                         CL**6
01300                            TTA-3 OR TTA-4                            CL*12
01301                  MOVE -1         TO CURRTRML                         CL**6
01302                  MOVE AL-UABON   TO CURRTRMA                         CL**6
01303                  MOVE ER-0063    TO EMI-ERROR                        CL**6
01304                  PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT       EL103
01305              ELSE                                                 EL103
01306                  MOVE AL-UANON   TO CURRTRMA.                        CL**6
01307                                                                   EL103
01308      IF CREDITL GREATER ZERO                                         CL*19
01309          IF CREDITI  NOT EQUAL 'Y' AND 'N'                           CL**6
01310              MOVE -1             TO CREDITL                          CL**6
01311              MOVE AL-UABON       TO CREDITA                          CL**6
01312              MOVE ER-0075        TO EMI-ERROR                        CL**6
01313              PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT           EL103
01314          ELSE                                                     EL103
01315              MOVE AL-UANON       TO CREDITA.                         CL**6
01316                                                                   EL103
01317      IF CLAIMSL GREATER ZERO                                         CL*19
01318          IF CLAIMSI  NOT EQUAL 'Y' AND 'N'                           CL**6
01319              MOVE -1             TO CLAIMSL                          CL**6
01320              MOVE AL-UABON       TO CLAIMSA                          CL**6
01321              MOVE ER-0075        TO EMI-ERROR                        CL**6
01322              PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT           EL103
01323          ELSE                                                     EL103
01324              MOVE AL-UANON       TO CLAIMSA.                         CL**6
01325                                                                   EL103
01326      IF LIFEL GREATER ZERO                                           CL*19
01327          IF LIFEI  NOT EQUAL 'Y' AND 'N'                             CL**6
01328              MOVE -1             TO LIFEL                            CL**6
01329              MOVE AL-UABON       TO LIFEA                            CL**6
01330              MOVE ER-0075        TO EMI-ERROR                        CL**6
01331              PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT           EL103
01332          ELSE                                                     EL103
01333              MOVE AL-UANON       TO LIFEA.                           CL**6
01334                                                                   EL103
01335      IF LOGONL GREATER ZERO                                          CL*19
01336          IF LOGONI  NOT EQUAL 'Y' AND 'N'                            CL**6
01337              MOVE -1             TO LOGONL                           CL**6
01338              MOVE AL-UABON       TO LOGONA                           CL**6
01339              MOVE ER-0075        TO EMI-ERROR                        CL**6
01340              PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT           EL103
01341          ELSE                                                     EL103
01342              MOVE AL-UANON       TO LOGONA.                          CL**6
01343                                                                   EL103
01344                                                                      CL*20
01345      IF USRPWDL  GREATER ZERO                                        CL*20
01346          MOVE SPACES TO  WS-SAVE-PASSWORD                            CL*20
01347          MOVE 'N'    TO  WS-PASSWORD-NUMERIC                         CL*20
01348          MOVE 'N'    TO  WS-PASSWORD-SIXDIGIT                        CL*20
01349          MOVE USRPWDI  TO  WS-SAVE-PASSWORD                          CL*20
01350          IF (WS-PASSWORD-X1 > SPACE) AND                             CL*20
01351             (WS-PASSWORD-X2 > SPACE) AND                             CL*20
01352             (WS-PASSWORD-X3 > SPACE) AND                             CL*20
01353             (WS-PASSWORD-X4 > SPACE) AND                             CL*20
01354             (WS-PASSWORD-X5 > SPACE) AND                             CL*20
01355             (WS-PASSWORD-X6 > SPACE)                                 CL*20
01356             MOVE 'Y' TO  WS-PASSWORD-SIXDIGIT                        CL*20
01357          END-IF                                                      CL*20
01358          IF (WS-PASSWORD-N1  NUMERIC) OR                             CL*20
01359             (WS-PASSWORD-N2  NUMERIC) OR                             CL*20
01360             (WS-PASSWORD-N3  NUMERIC) OR                             CL*20
01361             (WS-PASSWORD-N4  NUMERIC) OR                             CL*20
01362             (WS-PASSWORD-N5  NUMERIC) OR                             CL*20
01363             (WS-PASSWORD-N6  NUMERIC) OR                             CL*20
01364             (WS-PASSWORD-N7  NUMERIC) OR                             CL*20
01365             (WS-PASSWORD-N8  NUMERIC) OR                             CL*20
01366             (WS-PASSWORD-N9  NUMERIC) OR                             CL*20
01367             (WS-PASSWORD-N10 NUMERIC) OR                             CL*20
01368             (WS-PASSWORD-N11 NUMERIC)                                CL*20
01369             MOVE 'Y' TO  WS-PASSWORD-NUMERIC                         CL*20
01370          END-IF                                                      CL*20
01371          IF WS-PASSWORD-NUMERIC = 'N'  OR                            CL*20
01372             WS-PASSWORD-SIXDIGIT = 'N'                               CL*20
01373              MOVE -1             TO USRPWDL                          CL*20
01374              MOVE AL-UADON       TO USRPWDA                          CL*20
01375              MOVE ER-2549        TO EMI-ERROR                        CL*20
01376              PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT              CL*20
01377          ELSE                                                        CL*20
01378              MOVE AL-UADON       TO USRPWDA                          CL*20
01379          END-IF                                                      CL*20
01380          IF NEWPWDI NOT EQUAL  WS-SAVE-PASSWORD                      CL*20
01381             MOVE SPACES         TO NEWPWDO                           CL*20
01382             MOVE -1             TO NEWPWDL                           CL*20
01383             MOVE AL-UADON       TO NEWPWDA                           CL*20
01384             MOVE ER-8026        TO EMI-ERROR                         CL*20
01385             PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT               CL*20
01386          ELSE                                                        CL*20
01387              MOVE AL-UADON       TO NEWPWDA                          CL*20
01388          END-IF                                                      CL*20
01389      ELSE                                                            CL*20
01390         IF NEWPWDL  GREATER ZERO                                     CL*20
01391            MOVE SPACES         TO NEWPWDO                            CL*20
01392            MOVE -1             TO NEWPWDL                            CL*20
01393            MOVE AL-UADON       TO NEWPWDA                            CL*20
01394            MOVE ER-2548        TO EMI-ERROR                          CL*20
01395            PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT.               CL*20
01396                                                                      CL*20
01397                                                                      CL*20
01398      IF LANGTYPL GREATER ZERO                                        CL*19
01399          IF LANGTYPI NOT EQUAL 'F' AND 'E' AND ' '                   CL*16
01400              MOVE -1             TO LANGTYPL                         CL*16
01401              MOVE AL-UABON       TO LANGTYPA                         CL*16
01402              MOVE ER-9946        TO EMI-ERROR                        CL*16
01403              PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT              CL*16
01404          ELSE                                                        CL*16
01405              MOVE AL-UANON       TO LANGTYPA.                        CL*16
01406                                                                      CL*16
01407      IF USRALML GREATER ZERO                                         CL*19
01408          IF USRALMI  NOT EQUAL 'Y' AND 'N'                           CL**6
01409              MOVE ER-0075        TO EMI-ERROR                        CL**6
01410              PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT           EL103
01411              MOVE -1             TO USRALML                          CL**6
01412              MOVE AL-UABON       TO USRALMA                          CL**6
01413          ELSE                                                        CL**6
01414              MOVE AL-UANON       TO USRALMA.                         CL**6
01415                                                                   EL103
01416      IF APPLVL GREATER ZERO                                          CL*12
01417          IF APPLVI NOT = '1' AND '2' AND '3' AND ' '                 CL*12
091813               AND '4' AND '5'
01418              MOVE ER-1889        TO EMI-ERROR                        CL*12
01419              PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT              CL*12
01420              MOVE -1             TO APPLVL                           CL*12
01421              MOVE AL-UABON       TO APPLVA                           CL*12
01422          ELSE                                                        CL*12
01423              MOVE AL-UANON       TO APPLVA.                          CL*12
01424                                                                      CL*12
01425      IF SLCTSYSI GREATER ZERO                                        CL*19
01426          IF SLCTSYSI  EQUAL 'YY' OR 'YN' OR 'NN'                     CL**6
01427              MOVE AL-UANON       TO SLCTSYSA                         CL**6
01428          ELSE                                                     EL103
01429              MOVE ER-7087        TO EMI-ERROR                        CL**6
01430              PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT              CL**6
01431              MOVE -1             TO SLCTSYSL                         CL**6
01432              MOVE AL-UABON       TO SLCTSYSA.                        CL**6
01433                                                                   EL103
01434      IF FORCEL GREATER ZERO                                          CL*19
01435          IF FORCEI    NOT EQUAL 'Y' AND 'N'                          CL**6
01436              MOVE ER-0075        TO EMI-ERROR                        CL**6
01437              PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT           EL103
01438              MOVE -1             TO FORCEL                           CL**6
01439              MOVE AL-UABON       TO FORCEA                           CL**6
01440          ELSE                                                        CL**6
01441              MOVE AL-UANON       TO FORCEA.                          CL**6
01442                                                                   EL103
01443      MOVE PI-APPLICATION         TO SYS.                             CL**6
01444      MOVE 1                      TO SLOT.                            CL*12
01445                                                                   EL103
01446  6400-APPLICATION-LOOP.                                              CL**6
01447                                                                   EL103
01448      IF APP-LENGTH (SLOT)  NOT EQUAL ZERO                            CL**6
01449          IF APP (SLOT)  EQUAL 'YY' OR 'YN' OR 'NN'                   CL**6
01450              MOVE AL-UANON       TO APP-ATTRB (SLOT)                 CL**6
01451          ELSE                                                     EL103
01452              MOVE ER-7084        TO EMI-ERROR                        CL**6
01453              PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT           EL103
01454              MOVE -1             TO APP-LENGTH (SLOT)                CL**6
01455              MOVE AL-UABON       TO APP-ATTRB (SLOT).                CL**6
01456                                                                   EL103
01457      IF SLOT LESS THAN MAXSLOT                                       CL**6
01458          ADD 1                   TO SLOT                             CL*12
01459          GO TO 6400-APPLICATION-LOOP.                                CL**6
01460                                                                   EL103
01461 **********************************************                       CL**6
01462 *  CHECK TO SEE IF ADDITIONAL CLAIMS FIELDS  *                       CL**6
01463 *  SHOULD BE EDITED.                         *                       CL**7
01464 **********************************************                       CL**6
01465                                                                   EL103
01466      IF NOT CLAIMS-APP                                               CL**7
01467          GO TO 6999-EXIT.                                            CL**6
01468                                                                   EL103
01469      IF QCDAYSL GREATER ZERO AND                                     CL*19
01470         QCDAYSI  EQUAL SPACES                                        CL**6
01471          MOVE 0                  TO  QCDAYSI                      EL103
01472          MOVE AL-UNNON           TO  QCDAYSA                      EL103
01473      ELSE                                                         EL103
01474          IF QCDAYSL GREATER ZERO                                     CL*19
01475              IF QCDAYSI NOT NUMERIC                               EL103
01476                  MOVE -1         TO  QCDAYSL                      EL103
01477                  MOVE AL-UNBON   TO  QCDAYSA                      EL103
01478                  MOVE ER-0082    TO EMI-ERROR                        CL**6
01479                  PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT       EL103
01480              ELSE                                                 EL103
01481                  MOVE AL-UNNON   TO  QCDAYSA.                     EL103
01482                                                                   EL103
01483      IF QCAMTL GREATER ZERO                                          CL*19
01484          EXEC CICS  BIF DEEDIT                                    EL103
01485              FIELD   (QCAMTI)                                     EL103
01486              LENGTH  (7)                                          EL103
01487          END-EXEC                                                    CL*12
01488          IF QCAMTI LESS ZERO                                      EL103
01489              MOVE -1             TO  QCAMTL                       EL103
01490              MOVE AL-UNBON       TO  QCAMTA                       EL103
01491              MOVE ER-0078        TO EMI-ERROR                        CL**6
01492              PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT           EL103
01493          ELSE                                                     EL103
01494              MOVE AL-UNNON       TO  QCAMTA.                      EL103
01495                                                                   EL103
01496      IF MAXDAYL  GREATER ZERO AND                                    CL*19
01497         MAXDAYI  EQUAL SPACES                                        CL**6
01498          MOVE 0                  TO MAXDAYI                          CL**6
01499          MOVE AL-UNNON           TO MAXDAYA                          CL**6
01500      ELSE                                                         EL103
01501          IF MAXDAYL GREATER ZERO                                     CL*19
01502              IF MAXDAYI NOT NUMERIC                               EL103
01503                  MOVE -1         TO MAXDAYL                          CL**6
01504                  MOVE AL-UNBON   TO MAXDAYA                          CL**6
01505                  MOVE ER-0082    TO EMI-ERROR                        CL**6
01506                  PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT       EL103
01507              ELSE                                                 EL103
01508                  MOVE AL-UNNON   TO MAXDAYA.                         CL**6
01509                                                                   EL103
01510      IF MXAHAMTL GREATER ZERO                                        CL*19
01511          EXEC CICS  BIF DEEDIT                                    EL103
01512              FIELD   (MXAHAMTI)                                   EL103
01513              LENGTH  (11)                                         EL103
01514          END-EXEC                                                    CL*12
01515          IF MXAHAMTI LESS ZERO                                    EL103
01516              MOVE -1             TO MXAHAMTL                         CL**6
01517              MOVE AL-UNBON       TO MXAHAMTA                         CL**6
01518              MOVE ER-0078        TO EMI-ERROR                        CL**6
01519              PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT           EL103
01520          ELSE                                                     EL103
01521              MOVE AL-UNNON       TO MXAHAMTA.                        CL**6
01522                                                                   EL103
01523      IF MXLFAMTL GREATER ZERO                                        CL*19
01524          EXEC CICS  BIF DEEDIT                                    EL103
01525              FIELD   (MXLFAMTI)                                   EL103
01526              LENGTH  (11)                                         EL103
01527          END-EXEC                                                    CL*12
01528          IF MXLFAMTI LESS ZERO                                    EL103
01529              MOVE -1             TO MXLFAMTL                         CL**6
01530              MOVE AL-UNBON       TO MXLFAMTA                         CL**6
01531              MOVE ER-0078        TO EMI-ERROR                        CL**6
01532              PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT           EL103
01533          ELSE                                                     EL103
01534              MOVE AL-UNNON       TO MXLFAMTA.                        CL**6
01535                                                                   EL103
01536      IF AUTMONL  GREATER ZERO AND                                    CL*19
01537         AUTMONI  EQUAL SPACES                                        CL**6
01538          MOVE 0                  TO  AUTMONI                      EL103
01539          MOVE AL-UNNON           TO  AUTMONA                      EL103
01540      ELSE                                                         EL103
01541          IF AUTMONL GREATER ZERO                                     CL*19
01542              IF AUTMONI  NOT NUMERIC                              EL103
01543                  MOVE -1         TO  AUTMONL                      EL103
01544                  MOVE AL-UNBON   TO  AUTMONA                      EL103
01545                  MOVE ER-0077    TO EMI-ERROR                        CL**6
01546                  PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT       EL103
01547              ELSE                                                 EL103
01548                  MOVE AL-UNNON   TO  AUTMONA.                     EL103
01549                                                                   EL103
01550      IF AUTAMTL GREATER ZERO                                         CL*19
01551          EXEC CICS  BIF DEEDIT                                    EL103
01552              FIELD   (AUTAMTI)                                    EL103
01553              LENGTH  (11)                                         EL103
01554          END-EXEC                                                    CL*12
01555          IF AUTAMTI  LESS  ZERO                                   EL103
01556              MOVE -1             TO  AUTAMTL                      EL103
01557              MOVE AL-UNBON       TO  AUTAMTA                      EL103
01558              MOVE ER-0078        TO EMI-ERROR                        CL**6
01559              PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT           EL103
01560          ELSE                                                     EL103
01561              MOVE AL-UNNON       TO  AUTAMTA.                     EL103
01562                                                                   EL103
01563      IF MAXEXPL GREATER ZERO                                         CL*19
01564          EXEC CICS BIF DEEDIT                                        CL*15
01565              FIELD   (MAXEXPI)                                       CL*15
01566              LENGTH  (11)                                            CL*15
01567          END-EXEC                                                    CL*15
01568          IF MAXEXPI LESS ZERO                                        CL*15
01569              MOVE -1             TO MAXEXPL                          CL*15
01570              MOVE AL-UNBON       TO MAXEXPA                          CL*15
01571              MOVE ER-0078        TO EMI-ERROR                        CL*15
01572              PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT              CL*15
01573          ELSE                                                        CL*15
01574              MOVE AL-UNNON       TO MAXEXPA.                         CL*15
01575                                                                      CL*15
01576      IF EMI-NO-ERRORS                                             EL103
01577          GO TO 6999-EXIT.                                         EL103
01578                                                                   EL103
01579      IF QCAMTL GREATER ZERO                                          CL*19
01580          MOVE QCAMTI             TO QCAMTO.                          CL**6
01581                                                                   EL103
01582      IF MXAHAMTL GREATER ZERO                                        CL*19
01583          MOVE MXAHAMTI           TO MXAHAMTO.                        CL**6
01584                                                                   EL103
01585      IF MXLFAMTL GREATER ZERO                                        CL*19
01586          MOVE MXLFAMTI           TO MXLFAMTO.                        CL**6
01587                                                                   EL103
01588      IF AUTAMTL GREATER ZERO                                         CL*19
01589          MOVE AUTAMTI            TO AUTAMTO.                         CL**6
01590                                                                      CL*15
01591      IF MAXEXPL GREATER ZERO                                         CL*19
01592          MOVE MAXEXPI            TO MAXEXPO.                         CL*15
01593                                                                   EL103
01594  6999-EXIT.                                                       EL103
01595      EXIT.                                                        EL103
01596                                                                      CL**6
01597      EJECT                                                        EL103
01598  7000-BUILD-OUTPUT-MAP.                                           EL103
01599                                                                      CL**6
01600      MOVE LOW-VALUES             TO EL103AO.                         CL**6
01601      MOVE -1                     TO MAINTL.                          CL**6
01602      MOVE AL-UANOF               TO MAINTA.                          CL**6
01603      MOVE SPACES                 TO MAINTI.                          CL**7
01604                                                                      CL**6
01605      MOVE PI-APPLICATION         TO SYS.                             CL**7
01606                                                                      CL**7
01607      IF READ-PROCESSOR-ONE                                           CL**7
01608          COMPUTE SYS EQUAL SYS + +4                                  CL**7
01609          MOVE DESCRIPT(SYS)      TO DESCRO                           CL**7
01610          COMPUTE SYS EQUAL SYS - +4                                  CL**7
01611      ELSE                                                            CL**7
01612          MOVE DESCRIPT(SYS)      TO DESCRO.                          CL**7
01613                                                                      CL**7
01614      MOVE CF-ADMINISTRATION-CONTROLS(SYS)                            CL**7
01615                                  TO SLCTSYSO.                        CL**7
01616      MOVE AL-UANON               TO SLCTSYSA.                        CL**7
01617                                                                      CL**7
01618      MOVE CF-APPLICATION-FORCE(SYS)                                  CL**7
01619                                  TO FORCEO.                          CL**7
01620      MOVE AL-UANON               TO FORCEA.                          CL**7
01621                                                                      CL**7
01622      MOVE 1                      TO SLOT.                            CL*12
01623                                                                      CL**7
01624  7100-APPLICATION-LOOP.                                              CL**7
01625                                                                      CL**7
01626      MOVE CF-APP-SWITCHES(SYS SLOT)                                  CL**7
01627                                  TO APP (SLOT).                      CL**7
01628      MOVE AL-UANON               TO APP-ATTRB (SLOT).                CL**7
01629                                                                      CL**7
01630      IF SLOT   LESS   MAXSLOT                                        CL**7
01631          ADD 1                   TO SLOT                             CL*12
01632          GO TO 7100-APPLICATION-LOOP.                                CL**7
01633                                                                   EL103
01634      IF NO-CARRIER-SECURITY                                       EL103
01635          MOVE SPACES                TO CARRO                         CL**7
01636          MOVE AL-UANOF              TO CARRA                         CL**7
01637      ELSE                                                         EL103
01638          MOVE CF-PROCESSOR-CARRIER  TO CARRO                         CL**6
01639          MOVE AL-UANON              TO CARRA.                        CL**6
01640                                                                   EL103
01641      IF NO-ACCOUNT-SECURITY                                       EL103
01642          MOVE AL-UANOF              TO ACCTA                         CL**7
01643      ELSE                                                         EL103
01644          MOVE CF-PROCESSOR-ACCOUNT  TO  ACCTO                     EL103
01645          MOVE AL-UANON              TO  ACCTA.                    EL103
01646                                                                      CL**7
01647      IF READ-PROCESSOR-ONE                                           CL**7
01648          MOVE SAVE-PROCESSOR-ZERO                                    CL**7
01649                                  TO CONTROL-FILE.                    CL**7
01650                                                                      CL**7
01651      MOVE CF-PROCESSOR           TO USERCDO                          CL**7
01652                                     PI-PREV-USER.                    CL**7
01653      MOVE AL-UANON               TO USERCDA.                         CL**7
01654                                                                   EL103
01655      MOVE CF-PROCESSOR-NAME      TO NAMEO.                           CL**6
01656      MOVE AL-UANON               TO NAMEA.                           CL**6
01657                                                                      CL**6
01658      MOVE CF-PROCESSOR-TITLE     TO TITLEO.                          CL**6
01659      MOVE AL-UANON               TO TITLEA.                          CL**6
01660                                                                      CL**6
01661      MOVE CF-APPROVAL-LEVEL      TO APPLVO.                          CL*12
01662      MOVE AL-UANON               TO APPLVA.                          CL*12
011812
011812     MOVE CF-CSR-IND             TO CSRO.
011812     MOVE AL-UANON               TO CSRA.
01663                                                                      CL*12
01664      MOVE CF-LAST-MAINT-BY       TO LSTUSRO                          CL**6
01665                                     PI-UPDATE-BY.                    CL**6
01666      MOVE CF-LAST-MAINT-HHMMSS   TO TIME-IN                          CL**6
01667                                     PI-UPDATE-HHMMSS.                CL**6
01668      MOVE TIME-OUT               TO LSTTIMEO.                        CL**6
01669      MOVE ' '                    TO DC-OPTION-CODE.                  CL**6
01670      MOVE CF-LAST-MAINT-DT       TO DC-BIN-DATE-1.                   CL**6
01671      MOVE LINK-ELDATCV           TO PGM-NAME.                        CL**6
01672                                                                   EL103
01673      EXEC CICS  LINK                                              EL103
01674          PROGRAM   (PGM-NAME)                                     EL103
01675          COMMAREA  (DATE-CONVERSION-DATA)                         EL103
01676          LENGTH    (DC-COMM-LENGTH)                               EL103
01677      END-EXEC.                                                       CL*12
01678                                                                   EL103
01679      IF DATE-CONVERSION-ERROR                                     EL103
01680          MOVE ZEROS              TO LSTDTEO                          CL**7
01681      ELSE                                                         EL103
01682          MOVE DC-GREG-DATE-1-EDIT  TO LSTDTEO.                       CL**6
01683                                                                   EL103
01684      MOVE CF-PROCESSOR-PRINTER   TO USRPRNTO.                        CL**7
01685                                                                   EL103
01686      IF PI-COMPANY-ID EQUAL 'SLI' AND                                CL*17
01687        (PI-PROCESSOR-ID NOT EQUAL USERCDI)                           CL*17
01688          MOVE SPACES             TO USRPWDO                          CL*17
01689          MOVE SPACES             TO NEWPWDO                          CL*20
01690      ELSE                                                            CL**6
01691          IF PI-USER-ALMIGHTY-YES  OR                                 CL*17
01692             PI-PROCESSOR-ID EQUAL CF-PROCESSOR                       CL*17
01693              MOVE AL-UADOF       TO USRPWDA                          CL*20
01694          ELSE                                                        CL*17
01695              MOVE AL-UADOF       TO USRPWDA.                         CL*20
01696                                                                      CL**6
01697      MOVE CF-CURRENT-TERM-ON     TO CURRTRMO.                        CL**6
01698      MOVE AL-UANON               TO CURRTRMA.                        CL*12
01699                                                                      CL**6
01700      MOVE CF-PROC-SYS-ACCESS-CREDIT                                  CL**6
01701                                  TO CREDITO.                         CL**6
01702                                                                      CL**6
01703      MOVE CF-PROC-SYS-ACCESS-CLAIMS                                  CL**6
01704                                  TO CLAIMSO.                         CL**6
01705                                                                      CL*16
01706      MOVE CF-LANGUAGE-TYPE                                           CL*16
01707                                  TO LANGTYPO.                        CL*16
01708                                                                      CL**6
01709      MOVE CF-PROC-SYS-ACCESS-LIFE                                    CL**6
01710                                  TO LIFEO.                           CL**6
01711                                                                      CL**6
01712      MOVE CF-MESSAGE-AT-LOGON-CAP                                    CL**6
01713                                  TO LOGONO.                          CL**6
01714                                                                      CL**6
01715      MOVE CF-PROCESSOR-USER-ALMIGHTY                                 CL**6
01716                                  TO USRALMO.                         CL**6
01717                                                                      CL**6
01718  7200-SPECIAL-CLAIMS-FIELDS.                                         CL**6
01719                                                                      CL**6
01720      IF SYS EQUAL 2                                                  CL*12
01721          MOVE CF-PROC-CALC-DAYS-TOL                                  CL**6
01722                                  TO QCDAYSO                          CL**6
01723          MOVE AL-UANON           TO QCDAYSA                          CL**6
01724          MOVE CF-PROC-CALC-AMT-TOL                                   CL**6
01725                                  TO QCAMTO                           CL**6
01726          MOVE AL-UANON           TO QCAMTA                           CL**6
01727          MOVE CF-PROC-MAX-REG-DAYS                                   CL**6
01728                                  TO MAXDAYO                          CL**6
01729          MOVE AL-UANON           TO MAXDAYA                          CL**6
01730          MOVE CF-PROC-MAX-REG-PMT                                    CL**6
01731                                  TO MXAHAMTO                         CL**6
01732          MOVE AL-UANON           TO MXAHAMTA                         CL**6
01733          MOVE CF-PROC-MAX-LF-PMT TO MXLFAMTO                         CL**6
01734          MOVE AL-UANON           TO MXLFAMTA                         CL**6
01735          MOVE CF-PROC-MAX-AUTO-MOS                                   CL**6
01736                                  TO AUTMONO                          CL**6
01737          MOVE AL-UANON           TO AUTMONA                          CL**6
01738          MOVE CF-PROC-MAX-AUTO-PMT                                   CL**6
01739                                  TO AUTAMTO                          CL**6
01740          MOVE AL-UANON           TO AUTAMTA                          CL**6
01741          IF CF-PROC-MAX-EXP-PMT NOT NUMERIC                          CL*19
01742              MOVE 0              TO MAXEXPO                          CL*19
01743           ELSE                                                       CL*19
01744              MOVE CF-PROC-MAX-EXP-PMT                                CL*19
01745                                  TO MAXEXPO                          CL*15
01746          END-IF                                                      CL*19
01747          MOVE AL-UANON           TO MAXEXPA                          CL*15
01748          MOVE WS-QCDAYS          TO XCDAYSI                          CL**6
01749          MOVE WS-AH              TO XAHI                             CL**6
01750          MOVE WS-LIFE            TO XLIFEI                           CL**6
01751          MOVE WS-MAXDAYS         TO XAXDAYI                          CL**6
01752          MOVE WS-AUTO-PAY        TO XUTMONI                          CL**6
01753          MOVE WS-AMOUNT          TO XAMT1I                           CL**6
01754                                     XAMT2I                           CL**6
01755                                     EXPAMTI                          CL*15
01756          MOVE WS-EXPENSE         TO XEXP1I                           CL*15
01757          MOVE AL-SANON           TO XAMT1A                           CL**6
01758                                     XAMT2A                           CL**6
01759                                     XUTMONA                          CL**6
01760                                     XCDAYSA                          CL**6
01761                                     XAHA                             CL**6
01762                                     XLIFEA                           CL**6
01763                                     XAXDAYA                          CL**6
01764                                     XEXP1A                           CL*15
01765                                     EXPAMTA                          CL*15
01766      ELSE                                                         EL103
01767          MOVE AL-SADOF           TO XCDAYSA                          CL**6
01768                                     QCDAYSA                          CL**6
01769                                     XAHA                             CL**6
01770                                     QCAMTA                           CL**6
01771                                     XLIFEA                           CL**6
01772                                     XAXDAYA                          CL**6
01773                                     MAXDAYA                          CL**6
01774                                     XAMT1A                           CL**6
01775                                     XAMT2A                           CL**6
01776                                     MXAHAMTA                         CL**6
01777                                     MXLFAMTA                         CL**6
01778                                     XUTMONA                          CL**6
01779                                     AUTMONA                          CL**6
01780                                     AUTAMTA                          CL*15
01781                                     XEXP1A                           CL*15
01782                                     EXPAMTA.                         CL*15
01783                                                                   EL103
01784      MOVE AL-SANON               TO PFCRDTA                          CL**6
01785                                     PFCARDA                          CL**6
01786                                     PFCLMSA                          CL**6
01787                                     PFMORTA                          CL**7
01788                                     PFRECVA.                         CL**6
01789                                                                   EL103
01790      IF CREDIT-APP                                                   CL**7
01791          IF READ-PROCESSOR-ZERO                                      CL**7
01792              MOVE AL-SABON       TO PFCRDTA.                         CL**7
01793                                                                   EL103
01794      IF CLAIMS-APP                                                   CL**7
01795          IF READ-PROCESSOR-ZERO                                      CL**7
01796              MOVE AL-SABON       TO PFCLMSA.                         CL**7
01797                                                                      CL**7
01798      IF CREDIT-CARD-APP                                              CL**7
01799          IF READ-PROCESSOR-ZERO                                      CL**7
01800              MOVE AL-SABON       TO PFCARDA.                         CL**7
01801                                                                      CL**7
01802      IF ACCT-RECV-APP                                                CL**7
01803          IF READ-PROCESSOR-ZERO                                      CL**7
01804              MOVE AL-SABON       TO PFRECVA.                         CL**7
01805                                                                      CL**7
01806      IF MORTGAGE-APP                                                 CL**7
01807          IF READ-PROCESSOR-ONE                                       CL**7
01808              MOVE AL-SABON       TO PFMORTA.                         CL**7
01809                                                                   EL103
01810      MOVE AL-UNNOF               TO ENTERPFA.                        CL**6
01811                                                                   EL103
01812      GO TO 8100-SEND-INITIAL-MAP.                                    CL*13
01813                                                                   EL103
01814      EJECT                                                           CL**6
01815  7500-READ-CONTROL-FILE.                                             CL**6
01816                                                                   EL103
01817      EXEC CICS  READ                                                 CL**6
01818          DATASET  ('ELCNTL')                                         CL**6
01819          SET      (ADDRESS OF CONTROL-FILE)                          CL*17
01820          RIDFLD   (ELCNTL-KEY)                                       CL**6
01821      END-EXEC.                                                       CL*12
01822                                                                      CL**6
01823  7500-EXIT.                                                          CL**6
01824      EXIT.                                                           CL**6
01825                                                                      CL**6
01826  7510-READ-CONTROL-FILE-GTEQ.                                        CL**7
01827                                                                      CL**7
01828      EXEC CICS  READ                                                 CL**7
01829          DATASET  ('ELCNTL')                                         CL**7
01830          SET      (ADDRESS OF CONTROL-FILE)                          CL*17
01831          RIDFLD   (ELCNTL-KEY)                                       CL**7
01832          GTEQ                                                        CL**7
01833      END-EXEC.                                                       CL*12
01834                                                                      CL**7
01835  7510-EXIT.                                                          CL**7
01836      EXIT.                                                           CL**7
01837                                                                      CL**7
01838  7600-READ-CONTROL-FILE-UPDATE.                                      CL**6
01839                                                                      CL**6
01840      EXEC CICS  READ                                                 CL**6
01841          UPDATE                                                      CL**6
01842          DATASET  ('ELCNTL')                                         CL**6
01843          SET      (ADDRESS OF CONTROL-FILE)                          CL*17
01844          RIDFLD   (ELCNTL-KEY)                                       CL**6
01845      END-EXEC.                                                       CL*12
01846                                                                      CL**6
01847  7600-EXIT.                                                          CL**6
01848      EXIT.                                                           CL**6
01849                                                                      CL**6
01850  7700-WRITE-CONTROL-FILE.                                            CL**7
01851                                                                      CL**7
01852      EXEC CICS  WRITE                                                CL**7
01853          FROM     (CONTROL-FILE)                                     CL**7
01854          DATASET  ('ELCNTL')                                         CL**7
01855          RIDFLD   (CF-CONTROL-PRIMARY)                               CL**7
01856      END-EXEC.                                                       CL*12
01857                                                                      CL**7
01858  7700-EXIT.                                                          CL**7
01859      EXIT.                                                           CL**7
01860                                                                      CL**7
01861  7800-REWRITE-CONTROL-FILE.                                          CL**7
01862                                                                      CL**7
01863      EXEC CICS  REWRITE                                              CL**7
01864          DATASET  ('ELCNTL')                                         CL**7
01865          FROM     (CONTROL-FILE)                                     CL**7
01866      END-EXEC.                                                       CL*12
01867                                                                      CL**7
01868  7800-EXIT.                                                          CL**7
01869      EXIT.                                                           CL**7
01870                                                                      CL**7
01871  7900-DELETE-CONTROL-FILE.                                           CL**7
01872                                                                      CL**7
01873      EXEC CICS  DELETE                                               CL**7
01874          DATASET  ('ELCNTL')                                         CL**7
01875      END-EXEC.                                                       CL*12
01876                                                                      CL**7
01877  7900-EXIT.                                                          CL**7
01878      EXIT.                                                           CL**7
01879                                                                      CL**7
01880      EJECT                                                        EL103
01881  8100-SEND-INITIAL-MAP.                                           EL103
01882                                                                      CL**6
01883      IF ST-ACCNT-CNTL                                             EL103
01884        OR  ACCNT-CNTL                                             EL103
01885          MOVE AL-SADOF           TO CARRA                            CL**6
01886                                     CARRDESA.                        CL**6
01887                                                                   EL103
01888      IF PI-VALID-APP                                                 CL**6
01889          GO TO 8110-APP-ASSIGNED.                                    CL**6
01890                                                                      CL**6
01891      IF CREDIT-SESSION                                               CL**6
01892          MOVE 1                  TO PI-APPLICATION.                  CL*12
01893                                                                      CL**6
01894      IF MORTGAGE-SESSION                                             CL**7
01895          MOVE 1                  TO PI-APPLICATION.                  CL*12
01896                                                                      CL**7
01897      IF CLAIM-SESSION                                                CL**6
01898          MOVE 2                  TO PI-APPLICATION.                  CL*12
01899                                                                      CL**6
01900  8110-APP-ASSIGNED.                                                  CL**6
01901                                                                      CL**6
01902      IF READ-PROCESSOR-ZERO                                          CL**7
01903          MOVE CREDIT-SCRTY       TO SCRTYO                           CL**7
01904      ELSE                                                            CL**7
01905          MOVE MORTGAGE-SCRTY     TO SCRTYO.                          CL**7
01906                                                                      CL**7
01907      MOVE SAVE-DATE              TO RUNDTEO.                         CL**6
01908      MOVE EIBTIME                TO TIME-IN.                         CL**6
01909      MOVE TIME-OUT               TO RUNTIMEO.                        CL**6
01910      MOVE PI-COMPANY-ID          TO COMPO.                           CL**7
01911      MOVE -1                     TO MAINTL.                          CL**6
01912      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.                         CL**6
01913                                                                   EL103
CIDMOD     IF PI-COMPANY-ID = 'CID'                                        CL*20
CIDMOD         IF PI-USER-ALMIGHTY-YES
CIDMOD             MOVE AL-UANON       TO USRPWDA                          CL*20
CIDMOD                                    NEWPWDA                          CL*20
CIDMOD         ELSE
CIDMOD             MOVE AL-UADON       TO USRPWDA                          CL*20
CIDMOD                                    NEWPWDA                          CL*20
CIDMOD         END-IF                                                      CL*20
CIDMOD     END-IF                                                          CL*20
01914
01914      EXEC CICS  SEND                                              EL103
01915          MAP     (MAP-NAME)                                       EL103
01916          MAPSET  (MAPSET-NAME)                                    EL103
01917          FROM    (EL103AO)                                        EL103
01918          ERASE                                                    EL103
01919          CURSOR                                                   EL103
01920      END-EXEC.                                                       CL*12
01921                                                                   EL103
01922      GO TO 9100-RETURN-TRAN.                                      EL103
01923                                                                   EL103
01924  8200-SEND-DATAONLY.                                              EL103
01925                                                                      CL**6
01926      IF ST-ACCNT-CNTL                                             EL103
01927        OR  ACCNT-CNTL                                             EL103
01928          MOVE AL-SADOF           TO CARRA                            CL**6
01929                                     CARRDESA.                        CL**6
01930                                                                   EL103
01931      IF READ-PROCESSOR-ZERO                                          CL**7
01932          MOVE CREDIT-SCRTY       TO SCRTYO                           CL**7
01933      ELSE                                                            CL**7
01934          MOVE MORTGAGE-SCRTY     TO SCRTYO.                          CL**7
01935                                                                      CL**7
01936      MOVE SAVE-DATE              TO RUNDTEO.                         CL**6
01937      MOVE EIBTIME                TO TIME-IN.                         CL**6
01938      MOVE TIME-OUT               TO RUNTIMEO.                        CL**6
01939      MOVE PI-COMPANY-ID          TO COMPO.                           CL**7
01940      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.                         CL*12
01941                                                                   EL103
CIDMOD     IF PI-COMPANY-ID = 'CID'                                        CL*20
CIDMOD         IF PI-USER-ALMIGHTY-YES
CIDMOD             MOVE AL-UANON       TO USRPWDA                          CL*20
CIDMOD                                    NEWPWDA                          CL*20
CIDMOD         ELSE                                                        CL*20
CIDMOD             MOVE AL-UADON       TO USRPWDA                          CL*20
CIDMOD                                    NEWPWDA                          CL*20
CIDMOD         END-IF                                                      CL*20
CIDMOD     END-IF                                                          CL*20
CIDMOD
01942      EXEC CICS  SEND                                              EL103
01943          MAP     (MAP-NAME)                                       EL103
01944          MAPSET  (MAPSET-NAME)                                    EL103
01945          FROM    (EL103AO)                                        EL103
01946          DATAONLY                                                 EL103
01947          CURSOR                                                   EL103
01948      END-EXEC.                                                       CL*12
01949                                                                   EL103
01950      GO TO 9100-RETURN-TRAN.                                      EL103
01951                                                                   EL103
01952  8300-SEND-TEXT.                                                  EL103
01953      EXEC CICS  SEND TEXT                                         EL103
01954          FROM    (LOGOFF-TEXT)                                    EL103
01955          LENGTH  (LOGOFF-LENGTH)                                  EL103
01956          ERASE                                                    EL103
01957          FREEKB                                                   EL103
01958      END-EXEC.                                                       CL*12
01959                                                                   EL103
01960      EXEC CICS  RETURN                                            EL103
01961      END-EXEC.                                                       CL*12
01962                                                                   EL103
01963  8400-LOG-JOURNAL-RECORD.                                         EL103
01964 *    IF PI-JOURNAL-FILE-ID  EQUAL ZERO                               CL*15
01965 *        GO TO 8499-EXIT.                                            CL*15
01966 *                                                                    CL*15
01967 *    MOVE PI-PROCESSOR-ID        TO JP-USER-ID.                      CL*15
01968 *    MOVE FILE-ID                TO JP-FILE-ID.                      CL*15
01969 *    MOVE THIS-PGM               TO JP-PROGRAM-ID.                   CL*15
01970 *                                                                    CL*15
01971 *    EXEC CICS  JOURNAL                                              CL*15
01972 *        JFILEID  (PI-JOURNAL-FILE-ID)                               CL*15
01973 *        JTYPEID  ('EL')                                             CL*15
01974 *        FROM     (JOURNAL-RECORD)                                   CL*15
01975 *        LENGTH   (773)                                              CL*15
01976 *    END-EXEC.                                                       CL*15
01977 *                                                                    CL*15
01978  8499-EXIT.                                                       EL103
01979      EXIT.                                                        EL103
01980                                                                   EL103
01981  8800-UNAUTHORIZED-ACCESS.                                        EL103
01982      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                      CL**6
01983                                                                   EL103
01984      GO TO 8300-SEND-TEXT.                                        EL103
01985                                                                   EL103
01986  8820-PF23.                                                       EL103
01987      MOVE EIBAID                 TO PI-ENTRY-CD-1.                   CL**6
01988      MOVE XCTL-005               TO PGM-NAME.                        CL**6
01989                                                                   EL103
01990      GO TO 9300-XCTL.                                             EL103
01991                                                                   EL103
01992  8840-DUPREC.                                                     EL103
01993      MOVE ER-0081                TO EMI-ERROR.                       CL**6
01994                                                                   EL103
01995      PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT.                  EL103
01996                                                                   EL103
01997      MOVE -1                     TO USERCDL.                         CL**6
01998      MOVE AL-UABON               TO USERCDA.                         CL**6
01999                                                                   EL103
02000      IF QCAMTL GREATER ZERO                                          CL*19
02001          MOVE QCAMTI             TO QCAMTO.                          CL**6
02002                                                                   EL103
02003      IF MXAHAMTL GREATER ZERO                                        CL*19
02004          MOVE MXAHAMTI           TO MXAHAMTO.                        CL**6
02005                                                                   EL103
02006      IF MXLFAMTL GREATER ZERO                                        CL*19
02007          MOVE MXLFAMTI           TO MXLFAMTO.                        CL**6
02008                                                                   EL103
02009      IF AUTAMTL GREATER ZERO                                         CL*19
02010          MOVE AUTAMTI            TO AUTAMTO.                         CL**6
02011                                                                      CL*15
02012      IF MAXEXPL GREATER ZERO                                         CL*19
02013          MOVE MAXEXPI            TO MAXEXPO.                         CL*15
02014                                                                   EL103
02015      GO TO 8200-SEND-DATAONLY.                                    EL103
02016                                                                   EL103
02017  8860-ENDFILE.                                                    EL103
02018                                                                   EL103
02019      MOVE ER-0130                TO EMI-ERROR.                       CL**7
02020      PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT.                  EL103
02021      MOVE -1                     TO MAINTL.                          CL**6
02022      GO TO 8200-SEND-DATAONLY.                                       CL**7
02023                                                                   EL103
02024  8870-NOTOPEN.                                                    EL103
02025                                                                   EL103
02026      MOVE ER-0042                TO EMI-ERROR.                       CL**6
02027      PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT.                  EL103
02028      MOVE -1                     TO MAINTL.                          CL**6
02029      GO TO 8200-SEND-DATAONLY.                                    EL103
02030                                                                   EL103
02031  8880-NOT-FOUND.                                                  EL103
02032                                                                   EL103
02033      MOVE ER-0073                TO EMI-ERROR.                       CL**6
02034      PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT.                  EL103
02035      MOVE -1                     TO USERCDL.                         CL**6
02036      MOVE AL-UABON               TO USERCDA.                         CL**6
02037      GO TO 8200-SEND-DATAONLY.                                    EL103
02038                                                                   EL103
02039  9000-RETURN-CICS.                                                EL103
02040      EXEC CICS  RETURN                                            EL103
02041      END-EXEC.                                                       CL**4
02042                                                                   EL103
02043  9100-RETURN-TRAN.                                                EL103
02044                                                                      CL**6
02045      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.                CL**6
02046      MOVE '103A'                 TO PI-CURRENT-SCREEN-NO.            CL**6
02047                                                                   EL103
02048      EXEC CICS  RETURN                                            EL103
02049          TRANSID   (TRANS-ID)                                     EL103
02050          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL103
02051          LENGTH    (PI-COMM-LENGTH)                               EL103
02052      END-EXEC.                                                       CL*12
02053                                                                   EL103
02054  9200-RETURN-MAIN-MENU.                                           EL103
02055                                                                      CL**3
02056      IF PI-SESSION-IN-PROGRESS  EQUAL '1'                            CL**6
02057          MOVE XCTL-126           TO PGM-NAME.                        CL**6
02058                                                                      CL**3
02059      IF PI-SESSION-IN-PROGRESS  EQUAL '2'                            CL**6
02060          MOVE XCTL-626           TO PGM-NAME.                        CL**6
02061                                                                   EL103
02062      IF PI-SESSION-IN-PROGRESS  EQUAL '4'                            CL**6
02063          MOVE XCTL-EM626         TO PGM-NAME.                        CL**7
02064                                                                   EL103
02065      IF PI-SESSION-IN-PROGRESS  EQUAL '5'                            CL**6
02066          MOVE XCTL-800           TO PGM-NAME.                        CL**6
02067                                                                   EL103
02068  9300-XCTL.                                                       EL103
02069      EXEC CICS  XCTL                                              EL103
02070          PROGRAM   (PGM-NAME)                                     EL103
02071          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL103
02072          LENGTH    (PI-COMM-LENGTH)                               EL103
02073      END-EXEC.                                                       CL*12
02074                                                                   EL103
02075  9400-CLEAR.                                                      EL103
02076      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.                        CL**6
02077                                                                   EL103
02078      GO TO 9300-XCTL.                                             EL103
02079                                                                   EL103
02080  9500-PF12.                                                       EL103
02081      MOVE XCTL-010               TO PGM-NAME.                        CL**6
02082                                                                   EL103
02083      GO TO 9300-XCTL.                                             EL103
02084                                                                   EL103
02085  9600-PGMID-ERROR.                                                EL103
02086      EXEC CICS  HANDLE CONDITION                                  EL103
02087          PGMIDERR  (8300-SEND-TEXT)                               EL103
02088      END-EXEC.                                                       CL*12
02089                                                                   EL103
02090      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.              CL**6
02091      MOVE ' '                    TO PI-ENTRY-CD-1.                   CL**6
02092      MOVE XCTL-005               TO PGM-NAME.                        CL**6
02093      MOVE PGM-NAME               TO LOGOFF-PGM.                      CL**6
02094      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                     CL**6
02095                                                                   EL103
02096      GO TO 9300-XCTL.                                             EL103
02097                                                                   EL103
02098  9700-LINK-DATE-CONVERT.                                          EL103
02099      EXEC CICS  LINK                                              EL103
02100          PROGRAM   ('ELDATCV')                                    EL103
02101          COMMAREA  (DATE-CONVERSION-DATA)                         EL103
02102          LENGTH    (DC-COMM-LENGTH)                               EL103
02103      END-EXEC.                                                       CL*12
02104                                                                   EL103
02105  9799-EXIT.                                                       EL103
02106      EXIT.                                                        EL103
02107                                                                   EL103
02108  9000-ERROR-FORMAT.                                               EL103
02109      IF NOT  EMI-ERRORS-COMPLETE                                  EL103
02110          MOVE LINK-001           TO PGM-NAME                         CL**6
02111          EXEC CICS  LINK                                          EL103
02112              PROGRAM   (PGM-NAME)                                 EL103
02113              COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)            EL103
02114              LENGTH    (EMI-COMM-LENGTH)                          EL103
02115          END-EXEC.                                                   CL*12
02116                                                                   EL103
02117  9099-EXIT.                                                       EL103
02118      EXIT.                                                        EL103
02119                                                                   EL103
02120  9999-ABEND.                                                      EL103
02121                                                                      CL**7
02122      MOVE LINK-004               TO PGM-NAME.                        CL**6
02123      MOVE DFHEIBLK               TO EMI-LINE1.                       CL*12
02124                                                                   EL103
02125      EXEC CICS  LINK                                              EL103
02126          PROGRAM   (PGM-NAME)                                     EL103
02127          COMMAREA  (EMI-LINE1)                                    EL103
02128          LENGTH    (72)                                           EL103
02129      END-EXEC.                                                       CL*12
02130                                                                   EL103
02131      GO TO 8200-SEND-DATAONLY.                                    EL103
02132                                                                   EL103
02133      GOBACK.                                                      EL103
