00001  IDENTIFICATION DIVISION.                                         06/26/96
00002                                                                   EL680
00003  PROGRAM-ID.                 EL680 .                                 LV015
00004 *              PROGRAM CONVERTED BY                                  CL*14
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*14
00006 *              CONVERSION DATE 04/20/94 15:21:06.                    CL*14
00007 *                            VMOD 2.015                              CL*15
00008 *                                                                 EL680
00009 *AUTHOR.    LOGIC, INC.                                              CL*14
00010 *           DALLAS, TEXAS.                                           CL*14
00011                                                                   EL680
00012 *DATE-COMPILED.                                                      CL*14
00013                                                                   EL680
00014 *SECURITY.   *****************************************************   CL*14
00015 *            *                                                   *   CL*14
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*14
00017 *            *                                                   *   CL*14
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*14
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*14
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*14
00021 *            *                                                   *   CL*14
00022 *            *****************************************************   CL*14
00023                                                                   EL680
00024 *REMARKS.    TRANSACTION - EXF6 - PAYMENT CALCULATIONS.              CL*12
030612******************************************************************
030612*                   C H A N G E   L O G
030612*
030612* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
030612*-----------------------------------------------------------------
030612*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
030612* EFFECTIVE    NUMBER
030612*-----------------------------------------------------------------
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
030612******************************************************************
00025                                                                   EL680
00026      EJECT                                                        EL680
00027  ENVIRONMENT DIVISION.                                            EL680
00028                                                                   EL680
00029  DATA DIVISION.                                                   EL680
00030                                                                   EL680
00031  WORKING-STORAGE SECTION.                                         EL680
00032  77  FILLER  PIC X(32)  VALUE '********************************'. EL680
00033  77  FILLER  PIC X(32)  VALUE '*   EL680  WORKING STORAGE     *'. EL680
00034  77  FILLER  PIC X(32)  VALUE '************VMOD=2.015 *********'.    CL*15
00035                                                                   EL680
00036  01  WS-DATE-AREA.                                                EL680
00037      05  SAVE-DATE           PIC X(8)    VALUE SPACES.               CL*11
00038      05  SAVE-BIN-DATE       PIC XX      VALUE SPACES.               CL*11
00039                                                                   EL680
00040  01  ACCESS-KEYS.                                                 EL680
00041      12  ELCNTL-KEY.                                              EL680
00042          16  CNTL-COMP-ID    PIC XXX.                                CL**4
00043          16  CNTL-REC-TYPE   PIC X.                               EL680
00044          16  CNTL-ACCESS     PIC X(4).                            EL680
00045          16  CNTL-SEQ-NO     PIC S9(4)    COMP.                   EL680
00046                                                                   EL680
00047  01  SPC-FLD.                                                     EL680
00048      03  SPC-FLD-9    PIC 99.                                     EL680
00049                                                                   EL680
00050 *********************  INPUT AREAS *******************************EL680
00051  01  FILLER.                                                      EL680
00052      05  WS-NAME                 PIC X(20)           VALUE SPACES.EL680
00053      05  WS-FREQ                 PIC XX              VALUE SPACES.   CL*11
00054      05  WS-BASIS-X              PIC XX              VALUE SPACES.EL680
00055      05  WS-NPBEN                PIC X               VALUE SPACES.EL680
00056      05  WS-BALYN                PIC X               VALUE SPACES.EL680
00057      05  WS-OBYN                 PIC X               VALUE SPACES.   CL**4
00058      05  WS-ADDCHG               PIC X               VALUE SPACES.   CL**6
00059          88 NO-CHG                                   VALUE ' '.      CL**6
00060          88 CHG-LF                                   VALUE '1'.      CL**6
00061          88 CHG-AH                                   VALUE '2'.      CL**6
00062          88 CHG-LF-AH                                VALUE '3'.      CL**6
00063                                                                   EL680
00064  01  FILLER      COMP-3.                                          EL680
00065      05  WS-AMOUNT-REQUESTED     PIC S9(9)V99        VALUE +0.    EL680
00066      05  WS-APR                  PIC S9(6)V9(8)      VALUE +0.    EL680
00067      05  WS-NUMBER-OF-PMTS       PIC S999            VALUE +0.       CL**4
00068      05  WS-ADDTL-DAYS           PIC S999            VALUE +0.       CL**4
00069      05  WS-TRUNC                PIC S999            VALUE +0.       CL**4
00070      05  WS-EXTRA                PIC S9              VALUE +0.    EL680
00071      05  WS-LIFE-RATE            PIC S9(6)V9(8)      VALUE +0.       CL*11
00072      05  WS-LIFE-DEVIATION       PIC S9(6)V9(8)      VALUE +0.    EL680
00073      05  WS-AH-RATE              PIC S9(6)V9(8)      VALUE +0.    EL680
00074      05  WS-BL-RATE              PIC S9(6)V9(8)      VALUE +0.       CL**4
00075      05  WS-AH-DEVIATION         PIC S9(6)V9(8)      VALUE +0.    EL680
00076      05  WS-PAYMENTS-COVERED     PIC S999            VALUE +0.       CL**4
00077      05  WS-BALLOON-AMOUNT       PIC S9(6)V99        VALUE +0.    EL680
00078 *    05  WS-REGULAR-PAYMENT      PIC S9(6)V99        VALUE +0.       CL**5
00079                                                                   EL680
00080      EJECT                                                        EL680
00081                                                                   EL680
00082  01  FILLER         COMP-3.                                          CL**4
00083      05  ANGLM                   PIC S9(7)V9(11) VALUE ZERO.      EL680
00084      05  ANGLN                   PIC S9(7)V9(11) VALUE ZERO.      EL680
00085                                                                   EL680
00086      05  AHN                     PIC S999        VALUE ZERO.         CL**4
00087      05  ATF                     PIC S9(7)V9(11) VALUE ZERO.      EL680
00088      05  EMII                    PIC S999        VALUE ZERO.         CL**4
00089      05  OTF                     PIC S9(7)V9(11) VALUE ZERO.      EL680
00090      05  PPY                     PIC S999        VALUE ZERO.         CL**4
00091                                                                   EL680
00092      05  CA                      PIC S9(7)V9(11) VALUE ZERO.      EL680
00093      05  I                       PIC S9(7)V9(11) VALUE ZERO.      EL680
00094      05  K1                      PIC S9(7)V9(11) VALUE ZERO.      EL680
00095      05  L                       PIC S9(7)V9(11) VALUE ZERO.      EL680
00096      05  L1                      PIC S9(7)V9(11) VALUE ZERO.      EL680
00097      05  M                       PIC S9(7)V9(11) VALUE ZERO.      EL680
00098      05  N                       PIC S9(5)       VALUE ZERO.      EL680
00099      05  N1                      PIC S9(7)V9(11) VALUE ZERO.      EL680
00100      05  RA                      PIC S9(7)V9(11) VALUE ZERO.      EL680
00101      05  TA                      PIC S9(7)V9(11) VALUE ZERO.      EL680
00102      05  V                       PIC S9(7)V9(11) VALUE ZERO.      EL680
00103      05  VA                      PIC S9(7)V9(11) VALUE ZERO.      EL680
00104      05  VU                      PIC S9(7)V9(11) VALUE ZERO.      EL680
00105      05  VU1                     PIC S9(7)V9(11) VALUE ZERO.      EL680
00106                                                                   EL680
00107      05  NALF                    PIC S9(7)V9(11) VALUE ZERO.      EL680
00108      05  NPLF                    PIC S9(7)V9(11) VALUE ZERO.      EL680
00109      05  PADJ                    PIC S9(7)V9(11) VALUE ZERO.      EL680
00110                                                                   EL680
00111      05  TOT-PMTS                PIC S9(7)V9(11) VALUE ZERO.      EL680
00112      05  ATOT-PMTS               PIC S9(7)V9(11) VALUE ZERO.      EL680
00113      05  ADL-DAYS                PIC S999        VALUE ZERO.         CL**4
00114      05  ADL-ADJ                 PIC S9(7)V9(11) VALUE ZERO.      EL680
00115      05  COV-TERM                PIC S999        VALUE ZERO.         CL**4
00116                                                                   EL680
00117      05  LIFE-RATE               PIC S9(7)V9(11) VALUE ZERO.      EL680
00118      05  LIFEDEV                 PIC S9(7)V9(11) VALUE ZERO.      EL680
00119      05  LR                      PIC S9(7)V9(11) VALUE ZERO.      EL680
00120      05  LIFE-PREM               PIC S9(7)V9(11) VALUE ZERO.      EL680
00121                                                                   EL680
00122      05  AH-RATE                 PIC S9(7)V9(11) VALUE ZERO.      EL680
00123      05  AHDEV                   PIC S9(7)V9(11) VALUE ZERO.      EL680
00124      05  AH                      PIC S9(7)V9(11) VALUE ZERO.      EL680
00125      05  AH-PREM                 PIC S9(7)V9(11) VALUE ZERO.      EL680
00126                                                                      CL**4
00127      05  BL-RATE                 PIC S9(7)V9(11) VALUE ZERO.         CL**4
00128      05  BL                      PIC S9(7)V9(11) VALUE ZERO.         CL**4
00129                                                                      CL**4
00130 ************ OUTPUT AREAS ******                                  EL680
00131      05  L-PREMIUM               PIC S9(7)V99    VALUE ZERO.      EL680
00132      05  A-PREMIUM               PIC S9(7)V99    VALUE ZERO.      EL680
00133      05  PAYMENT                 PIC S9(7)V99    VALUE ZERO.      EL680
00134      05  TOT-FIN                 PIC S9(7)V99    VALUE ZERO.      EL680
00135      05  TOT-PAYMT               PIC S9(7)V99    VALUE ZERO.      EL680
00136      05  TOT-INTRS               PIC S9(7)V99    VALUE ZERO.      EL680
00137                                                                   EL680
00138  01  FILLER         COMP-3.                                       EL680
00139      05  V-NTH                   PIC S9(7)V9(11) VALUE ZERO.      EL680
00140      05  B                       PIC S9(7)V9(11) VALUE ZERO.      EL680
00141      05  ANGLN-LESS-1            PIC S9(7)V9(11) VALUE ZERO.      EL680
00142      05  NM1                     PIC S9(7)V9(11) VALUE ZERO.      EL680
00143      05  AEQ                     PIC S9(7)V9(11) VALUE ZERO.      EL680
00144      05  MPT                     PIC S9(7)V9(11) VALUE ZERO.      EL680
00145      05  MPB                     PIC S9(7)V9(11) VALUE ZERO.      EL680
00146      05  MP                      PIC S9(7)V9(11) VALUE ZERO.      EL680
00147      05  IA                      PIC S9(7)V9(11) VALUE ZERO.      EL680
00148                                                                   EL680
00149  01  FILLER.                                                      EL680
00150      05  XREPT                       PIC S999    COMP.               CL*11
00151      05  WS-ERROR-COUNT              PIC S999        VALUE ZERO.     CL*11
00152      05  WS-MESSAGE                  PIC X(79).                      CL*11
00153      05  WS-BASIS                    PIC S99         VALUE ZERO.     CL*11
00154      05  WS-TIME-WORK                PIC S9(7)    VALUE ZERO.        CL*11
00155      05  WS-TIME                     REDEFINES                       CL*11
00156          WS-TIME-WORK                PIC S999V9(4).                  CL*11
00157                                                                   EL680
00158      EJECT                                                        EL680
00159  01  ERROR-MESSAGES.                                              EL680
00160      12  ER-0004                 PIC X(4)  VALUE '0004'.          EL680
00161      12  ER-0008                 PIC X(4)  VALUE '0008'.          EL680
00162      12  ER-0029                 PIC X(4)  VALUE '0029'.          EL680
00163      12  ER-0042                 PIC X(4)  VALUE '0042'.          EL680
00164      12  ER-0190                 PIC X(4)  VALUE '0190'.          EL680
00165      12  ER-0412                 PIC X(4)  VALUE '0412'.          EL680
00166      12  ER-0413                 PIC X(4)  VALUE '0413'.          EL680
00167      12  ER-2361                 PIC X(4)  VALUE '2361'.          EL680
00168      12  ER-2363                 PIC X(4)  VALUE '2363'.          EL680
00169      12  ER-2364                 PIC X(4)  VALUE '2364'.          EL680
00170      12  ER-2365                 PIC X(4)  VALUE '2365'.          EL680
00171      12  ER-2366                 PIC X(4)  VALUE '2366'.          EL680
00172      12  ER-2367                 PIC X(4)  VALUE '2367'.          EL680
00173      12  ER-2920                 PIC X(4)  VALUE '2920'.          EL680
00174      12  ER-2921                 PIC X(4)  VALUE '2921'.          EL680
00175      12  ER-2922                 PIC X(4)  VALUE '2922'.          EL680
00176      12  ER-2923                 PIC X(4)  VALUE '2923'.          EL680
00177      12  ER-2924                 PIC X(4)  VALUE '2924'.          EL680
00178      12  ER-2925                 PIC X(4)  VALUE '2925'.          EL680
00179      12  ER-2926                 PIC X(4)  VALUE '2926'.          EL680
00180      12  ER-2927                 PIC X(4)  VALUE '2927'.          EL680
00181      12  ER-4004                 PIC X(4)  VALUE '4004'.             CL**6
00182      12  ER-4007                 PIC X(4)  VALUE '4007'.             CL*13
00183      EJECT                                                        EL680
00184  01  STANDARD-AREAS.                                              EL680
00185      12  MAPSET-NAME         PIC X(8)    VALUE 'EL680S'.          EL680
00186      12  MAP-NAME            PIC X(8)    VALUE 'EL680A'.          EL680
00187      12  SCREEN-NUMBER       PIC X(4)    VALUE '680A'.            EL680
00188      12  TRANS-ID            PIC X(4)    VALUE 'EXF6'.            EL680
00189      12  START-TRANS-ID      PIC X(4)    VALUE 'EXF7'.            EL680
00190      12  THIS-PGM            PIC X(8)    VALUE 'EL680'.           EL680
00191      12  PGM-NAME            PIC X(8).                            EL680
00192      12  XCTL-005            PIC X(8)    VALUE 'EL005'.           EL680
00193      12  XCTL-010            PIC X(8)    VALUE 'EL010'.           EL680
00194      12  XCTL-626            PIC X(8)    VALUE 'EL626'.           EL680
00195      12  LINK-001            PIC X(8)    VALUE 'EL001'.           EL680
00196      12  LINK-004            PIC X(8)    VALUE 'EL004'.           EL680
00197                                                                   EL680
00198      EJECT                                                        EL680
00199                              COPY EL680S.                            CL*13
00200      EJECT                                                        EL680
00201                              COPY ELCLOGOF.                          CL*13
00202      EJECT                                                        EL680
00203                              COPY ELCDATE.                           CL*13
00204      EJECT                                                        EL680
00205                              COPY ELCATTR.                           CL*13
00206      EJECT                                                        EL680
00207                              COPY ELCAID.                            CL*13
00208  01  FILLER    REDEFINES DFHAID.                                  EL680
00209      12  FILLER              PIC X(8).                            EL680
00210      12  PF-VALUES           PIC X       OCCURS 24 TIMES.         EL680
00211      EJECT                                                        EL680
00212                              COPY ELCEMIB.                           CL*13
00213      EJECT                                                        EL680
00214                              COPY ELCINTF.                           CL*13
00215      EJECT                                                        EL680
00216  LINKAGE SECTION.                                                 EL680
00217  01  DFHCOMMAREA         PIC X(1024).                                CL**4
00218 *01 PARMLIST   COMP.                                                 CL*14
00219 *    02  FILLER          PIC S9(8).                                  CL*14
00220 *    02  ELCNTL-POINTER  PIC S9(8).                                  CL*14
00221                                                                      CL**4
00222                               COPY ELCCNTL.                          CL*13
00223      EJECT                                                        EL680
00224  PROCEDURE DIVISION.                                              EL680
00225      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL680
00226      MOVE '5'                   TO DC-OPTION-CODE.                EL680
00227      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL680
00228      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL680
00229      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                EL680
00230                                                                   EL680
00231      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.                 EL680
00232      MOVE 2           TO EMI-NUMBER-OF-LINES.                        CL**4
00233                                                                   EL680
00234      IF EIBCALEN = 0                                              EL680
00235          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL680
00236                                                                   EL680
00237      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL680
00238          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL680
00239              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL680
00240              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL680
00241              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL680
00242              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL680
00243              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL680
00244              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL680
00245              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL680
00246              MOVE THIS-PGM             TO PI-CALLING-PROGRAM.        CL**6
00247                                                                   EL680
00248      IF EIBTRNID NOT = TRANS-ID                                   EL680
00249          GO TO 8100-SEND-INITIAL-MAP.                             EL680
00250                                                                   EL680
00251      IF EIBAID = DFHCLEAR                                         EL680
00252          GO TO 9400-CLEAR.                                        EL680
00253                                                                   EL680
00254      EXEC CICS HANDLE CONDITION                                   EL680
00255          MAPFAIL (8100-SEND-INITIAL-MAP)                          EL680
00256      END-EXEC.                                                    EL680
00257                                                                   EL680
00258  0100-RECEIVE-MAP.                                                EL680
00259      EXEC CICS RECEIVE                                            EL680
00260          MAP    (MAP-NAME)                                        EL680
00261          MAPSET (MAPSET-NAME)                                     EL680
00262          INTO   (EL680AI)                                         EL680
00263      END-EXEC.                                                    EL680
00264                                                                   EL680
00265      EJECT                                                        EL680
00266  0200-RECEIVE.                                                    EL680
00267      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL680
00268          MOVE ER-0008  TO EMI-ERROR                               EL680
00269          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL680
00270          MOVE -1    TO ANAMEL                                     EL680
00271          GO TO 8200-SEND-DATAONLY.                                EL680
00272                                                                   EL680
00273      IF PFENTERL = 0                                              EL680
00274          GO TO 0300-CHECK-PFKEYS.                                 EL680
00275      IF EIBAID NOT = DFHENTER                                     EL680
00276          MOVE ER-0004  TO EMI-ERROR                               EL680
00277          GO TO 0320-INPUT-ERROR.                                  EL680
00278                                                                   EL680
00279      IF (PFENTERI NUMERIC) AND (PFENTERI GREATER 0 AND LESS 25)      CL**4
00280          MOVE PF-VALUES (PFENTERI) TO EIBAID                      EL680
00281      ELSE                                                         EL680
00282          MOVE ER-0029  TO EMI-ERROR                               EL680
00283          GO TO 0320-INPUT-ERROR.                                  EL680
00284                                                                   EL680
00285  0300-CHECK-PFKEYS.                                               EL680
00286      IF EIBAID = DFHPF1                                           EL680
00287          GO TO 1000-EDIT-INPUT-DATA.                              EL680
00288                                                                   EL680
00289      IF EIBAID = DFHPF23                                          EL680
00290          GO TO 8810-PF23.                                         EL680
00291                                                                   EL680
00292      IF EIBAID = DFHPF24                                          EL680
00293          GO TO 9200-RETURN-MAIN-MENU.                             EL680
00294                                                                   EL680
00295      IF EIBAID = DFHPF12                                          EL680
00296          GO TO 9500-PF12.                                         EL680
00297                                                                   EL680
00298      IF EIBAID = DFHENTER                                         EL680
00299          GO TO 1000-EDIT-INPUT-DATA.                              EL680
00300                                                                   EL680
00301      MOVE ER-0029 TO EMI-ERROR.                                   EL680
00302  0320-INPUT-ERROR.                                                EL680
00303      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL680
00304      MOVE AL-UNBON               TO PFENTERA.                     EL680
00305      IF PFENTERL = 0                                              EL680
00306          MOVE -1                 TO ANAMEL                        EL680
00307      ELSE                                                         EL680
00308          MOVE -1                 TO PFENTERL.                     EL680
00309                                                                      CL**4
00310      GO TO 8200-SEND-DATAONLY.                                    EL680
00311                                                                   EL680
00312      EJECT                                                        EL680
00313  1000-EDIT-INPUT-DATA.                                            EL680
00314 ******EDIT AMOUNT REQUESTED                                          CL*11
00315                                                                   EL680
00316      EXEC CICS BIF DEEDIT                                         EL680
00317          FIELD  (AAMOUNTI)                                        EL680
00318          LENGTH (12)                                              EL680
00319      END-EXEC.                                                    EL680
00320                                                                   EL680
00321      IF (AAMOUNTI NUMERIC)  AND                                   EL680
00322         (AAMOUNTI NOT = ZERO)                                     EL680
00323          MOVE AAMOUNTI       TO  WS-AMOUNT-REQUESTED              EL680
00324                                  AAMOUNTO                         EL680
00325          MOVE AL-UNNON       TO  AAMOUNTA                         EL680
00326        ELSE                                                       EL680
00327          MOVE -1             TO  AAMOUNTL                         EL680
00328          MOVE AL-UNBON       TO  AAMOUNTA                         EL680
00329          MOVE ER-2361        TO  EMI-ERROR                        EL680
00330          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL680
00331          ADD +1  TO  WS-ERROR-COUNT.                              EL680
00332                                                                   EL680
00333 ***********INTEREST RATE (APR)                                    EL680
00334      EXEC CICS BIF DEEDIT                                         EL680
00335          FIELD  (AINTRATI)                                        EL680
00336          LENGTH (8)                                               EL680
00337      END-EXEC.                                                    EL680
00338                                                                   EL680
00339      IF (AINTRATI NUMERIC)  AND                                   EL680
00340         (AINTRATI NOT = ZERO)                                     EL680
00341          MOVE AINTRATI       TO  WS-APR                           EL680
00342                                  AINTRATO                         EL680
00343          MOVE AL-UNNON       TO  AINTRATA                         EL680
00344        ELSE                                                       EL680
00345          MOVE -1             TO  AINTRATL                         EL680
00346          MOVE AL-UNBON       TO  AINTRATA                         EL680
00347          MOVE ER-2363        TO  EMI-ERROR                        EL680
00348          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL680
00349          ADD +1  TO  WS-ERROR-COUNT.                              EL680
00350                                                                   EL680
00351 ***********PAYMENT FREQUENCY                                         CL*11
00352      IF AFREQI = 'MO' OR 'SM' OR 'BW' OR 'WK' OR                     CL*11
00353                  'SA' OR 'AN' OR '13'                                CL*11
00354          MOVE AL-UANON       TO  AFREQA                              CL*11
00355          MOVE AFREQI         TO  WS-FREQ                             CL*11
00356        ELSE                                                          CL*11
00357          MOVE -1             TO  AFREQL                              CL*11
00358          MOVE AL-UABON       TO  AFREQA                              CL*11
00359          MOVE ER-2920        TO  EMI-ERROR                           CL*11
00360          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*11
00361          ADD +1  TO  WS-ERROR-COUNT.                                 CL*11
00362                                                                      CL*11
00363 ***********NUMBER OF PAYMENTS                                        CL*11
00364      EXEC CICS BIF DEEDIT                                         EL680
00365          FIELD  (ANOPMTSI)                                           CL*11
00366          LENGTH (3)                                               EL680
00367      END-EXEC.                                                    EL680
00368                                                                   EL680
00369      IF (ANOPMTSI NUMERIC) AND                                       CL*11
00370         (ANOPMTSI NOT = ZERO)                                        CL*11
00371          MOVE ANOPMTSI       TO  WS-NUMBER-OF-PMTS                   CL*11
00372                                  ANOPMTSO                            CL*11
00373          MOVE AL-UNNON       TO  ANOPMTSA                            CL*11
00374        ELSE                                                       EL680
00375          MOVE -1             TO  ANOPMTSL                            CL*11
00376          MOVE AL-UNBON       TO  ANOPMTSA                            CL*11
00377          MOVE ER-2364        TO  EMI-ERROR                        EL680
00378          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL680
00379          ADD +1  TO  WS-ERROR-COUNT.                              EL680
00380                                                                   EL680
00381 ***********ADDITIONAL DAYS TO FIRST PAYMENT                       EL680
00382      IF AADDAYSL GREATER THAN ZERO                                   CL*11
00383          EXEC CICS BIF DEEDIT                                     EL680
00384              FIELD  (AADDAYSI)                                    EL680
00385              LENGTH (3)                                           EL680
00386          END-EXEC                                                 EL680
00387          IF AADDAYSI NUMERIC                                      EL680
00388              MOVE AADDAYSI       TO  WS-ADDTL-DAYS                EL680
00389                                      AADDAYSO                     EL680
00390              MOVE AL-UNNON       TO  AADDAYSA                     EL680
00391            ELSE                                                   EL680
00392              MOVE -1             TO  AADDAYSL                     EL680
00393              MOVE AL-UNBON       TO  AADDAYSA                     EL680
00394              MOVE ER-2921        TO  EMI-ERROR                    EL680
00395              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL680
00396              ADD +1  TO  WS-ERROR-COUNT.                          EL680
00397                                                                   EL680
00398 ***********ADDITIONAL DAYS CHARGING METHOD                           CL**6
00399 ********** SPACE=NO CHG, 1=LF, 2=AH, 3=LF AND AH                     CL**6
00400      IF ADDCHGL  GREATER THAN ZERO                                   CL*11
00401      IF ADDCHGI = '1' OR '2' OR '3' OR ' '                           CL**6
00402          MOVE AL-UANON       TO  ADDCHGA                             CL**6
00403          MOVE ADDCHGI        TO  WS-ADDCHG                           CL**6
00404        ELSE                                                          CL**6
00405          MOVE -1             TO  ADDCHGL                             CL**6
00406          MOVE AL-UABON       TO  ADDCHGA                             CL**6
00407          MOVE ER-4004        TO  EMI-ERROR                           CL**6
00408          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**6
00409          ADD +1  TO  WS-ERROR-COUNT.                                 CL**6
00410                                                                      CL**6
00411 ***********BASIS                                                  EL680
00412      IF ABASISI = 'AF' OR 'TP' OR 'NP'                               CL**6
00413          MOVE AL-UANON       TO  ABASISA                          EL680
00414          MOVE ABASISI        TO  WS-BASIS-X                       EL680
00415        ELSE                                                       EL680
00416          MOVE -1             TO  ABASISL                          EL680
00417          MOVE AL-UABON       TO  ABASISA                          EL680
00418          MOVE ER-2367        TO  EMI-ERROR                        EL680
00419          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL680
00420          ADD +1  TO  WS-ERROR-COUNT.                              EL680
00421                                                                   EL680
00422 **********USE O/B NET ACTUARIAL RATE                                 CL**5
00423      IF OBYORNL GREATER ZERO                                         CL**5
00424      IF OBYORNI = 'Y' OR 'N'                                         CL**5
00425          MOVE AL-UANON       TO  OBYORNA                             CL**5
00426          MOVE OBYORNI        TO  WS-OBYN                             CL**5
00427        ELSE                                                          CL**5
00428          MOVE -1             TO  OBYORNL                             CL**5
00429          MOVE AL-UABON       TO  OBYORNA                             CL**5
00430          MOVE ER-2922        TO  EMI-ERROR                           CL**5
00431          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**5
00432          ADD +1  TO  WS-ERROR-COUNT.                                 CL**5
00433                                                                      CL**5
00434 ***********NP BENEFIT INCLUDES PREMIUM (Y/N)                      EL680
00435      IF WS-BASIS-X = 'NP'                                            CL**4
00436          IF ANPREMI = 'Y' OR 'N'                                  EL680
00437              MOVE AL-UANON       TO  ANPREMA                      EL680
00438              MOVE ANPREMI        TO  WS-NPBEN                     EL680
00439            ELSE                                                   EL680
00440              MOVE -1             TO  ANPREML                      EL680
00441              MOVE AL-UABON       TO  ANPREMA                      EL680
00442              MOVE ER-2922        TO  EMI-ERROR                    EL680
00443              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL680
00444              ADD +1  TO  WS-ERROR-COUNT.                          EL680
00445                                                                   EL680
00446 ***********TRUNCATED LIFE NO. OF PAYMENTS COVERED                 EL680
00447      IF ATRUNCL GREATER THAN ZERO                                    CL*11
00448          EXEC CICS BIF DEEDIT                                     EL680
00449              FIELD  (ATRUNCI)                                     EL680
00450              LENGTH (3)                                           EL680
00451          END-EXEC                                                 EL680
00452          IF ATRUNCI IS NUMERIC                                    EL680
00453              MOVE ATRUNCI        TO  WS-TRUNC                     EL680
00454                                      ATRUNCO                      EL680
00455              MOVE AL-UNNON       TO  ATRUNCA                      EL680
00456            IF WS-TRUNC GREATER WS-NUMBER-OF-PMTS                     CL*13
00457              MOVE -1             TO  ATRUNCL                         CL*13
00458              MOVE AL-UNBON       TO  ATRUNCA                         CL*13
00459              MOVE ER-4007        TO  EMI-ERROR                       CL*13
00460              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*13
00461              ADD +1  TO  WS-ERROR-COUNT                              CL*13
00462            ELSE                                                      CL*13
00463              NEXT SENTENCE                                           CL*13
00464            ELSE                                                   EL680
00465              MOVE -1             TO  ATRUNCL                      EL680
00466              MOVE AL-UNBON       TO  ATRUNCA                      EL680
00467              MOVE ER-2923        TO  EMI-ERROR                    EL680
00468              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL680
00469              ADD +1  TO  WS-ERROR-COUNT.                          EL680
00470                                                                   EL680
00471 ***********EXTRA INTEREST PERIODS                                 EL680
00472      IF AEXTRAL GREATER THAN ZERO                                    CL*11
00473          EXEC CICS BIF DEEDIT                                     EL680
00474              FIELD  (AEXTRAI)                                     EL680
00475              LENGTH (1)                                           EL680
00476          END-EXEC                                                 EL680
00477          IF AEXTRAI NUMERIC                                       EL680
00478              MOVE AEXTRAI        TO  WS-EXTRA                     EL680
00479                                      AEXTRAO                      EL680
00480              MOVE AL-UNNON       TO  AEXTRAA                      EL680
00481            ELSE                                                   EL680
00482              MOVE -1             TO  AEXTRAL                      EL680
00483              MOVE AL-UNBON       TO  AEXTRAA                      EL680
00484              MOVE ER-2924        TO  EMI-ERROR                    EL680
00485              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL680
00486              ADD +1  TO  WS-ERROR-COUNT.                          EL680
00487                                                                   EL680
00488 ***********LIFE RATE                                              EL680
00489      IF ALRATEL GREATER THAN ZERO                                    CL*11
00490          EXEC CICS BIF DEEDIT                                     EL680
00491              FIELD  (ALRATEI)                                     EL680
00492              LENGTH (8)                                           EL680
00493          END-EXEC                                                 EL680
00494          IF ALRATEI NUMERIC                                       EL680
00495              MOVE ALRATEI        TO  WS-LIFE-RATE                    CL*11
00496                                      ALRATEO                      EL680
00497              MOVE AL-UNNON       TO  ALRATEA                      EL680
00498            ELSE                                                   EL680
00499              MOVE -1             TO  ALRATEL                      EL680
00500              MOVE AL-UNBON       TO  ALRATEA                      EL680
00501              MOVE ER-2365        TO  EMI-ERROR                    EL680
00502              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL680
00503              ADD +1  TO  WS-ERROR-COUNT.                          EL680
00504                                                                   EL680
00505 ***********LIFE DEVIATION                                         EL680
00506      IF ALFDEVL GREATER THAN ZERO                                    CL*11
00507          EXEC CICS BIF DEEDIT                                     EL680
00508              FIELD  (ALFDEVI)                                     EL680
00509              LENGTH (7)                                           EL680
00510          END-EXEC                                                 EL680
00511          IF ALFDEVI NUMERIC                                       EL680
00512              MOVE ALFDEVI        TO  WS-LIFE-DEVIATION            EL680
00513                                      ALFDEVO                      EL680
00514              MOVE AL-UNNON       TO  ALFDEVA                      EL680
00515            ELSE                                                   EL680
00516              MOVE -1             TO  ALFDEVL                      EL680
00517              MOVE AL-UNBON       TO  ALFDEVA                      EL680
00518              MOVE ER-2925        TO  EMI-ERROR                    EL680
00519              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL680
00520              ADD +1  TO  WS-ERROR-COUNT                              CL**6
00521      ELSE                                                            CL**6
00522         MOVE 100 TO WS-LIFE-DEVIATION                             EL680
00523                     ALFDEVO.                                      EL680
00524                                                                   EL680
00525 ***********DISABILITY RATE                                        EL680
00526      IF ADRATEL GREATER THAN ZERO                                    CL*11
00527          EXEC CICS BIF DEEDIT                                     EL680
00528              FIELD  (ADRATEI)                                     EL680
00529              LENGTH (8)                                           EL680
00530          END-EXEC                                                 EL680
00531          IF ADRATEI NUMERIC                                       EL680
00532              MOVE ADRATEI        TO  WS-AH-RATE                   EL680
00533                                      ADRATEO                      EL680
00534              MOVE AL-UNNON       TO  ADRATEA                      EL680
00535            ELSE                                                   EL680
00536              MOVE -1             TO  ADRATEL                      EL680
00537              MOVE AL-UNBON       TO  ADRATEA                      EL680
00538              MOVE ER-2366        TO  EMI-ERROR                    EL680
00539              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL680
00540              ADD +1  TO  WS-ERROR-COUNT.                          EL680
00541                                                                   EL680
00542 ***********AH DEVIATION                                           EL680
00543      IF AAHDEVL GREATER THAN ZERO                                    CL*11
00544          EXEC CICS BIF DEEDIT                                     EL680
00545              FIELD  (AAHDEVI)                                     EL680
00546              LENGTH (7)                                           EL680
00547          END-EXEC                                                 EL680
00548          IF AAHDEVI NUMERIC                                       EL680
00549              MOVE AAHDEVI        TO  WS-AH-DEVIATION              EL680
00550                                      AAHDEVO                      EL680
00551              MOVE AL-UNNON       TO  AAHDEVA                      EL680
00552            ELSE                                                   EL680
00553              MOVE -1             TO  AAHDEVL                      EL680
00554              MOVE AL-UNBON       TO  AAHDEVA                      EL680
00555              MOVE ER-2926        TO  EMI-ERROR                    EL680
00556              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL680
00557              ADD +1  TO  WS-ERROR-COUNT                              CL**6
00558      ELSE                                                            CL**6
00559         MOVE 100 TO WS-AH-DEVIATION                               EL680
00560                     AAHDEVO.                                      EL680
00561                                                                   EL680
00562 ********** NO. PAYMENTS COVERED IF NOT SAME AS LIFE                  CL*11
00563      IF ANOTLFL GREATER THAN ZERO                                    CL*11
00564          EXEC CICS BIF DEEDIT                                        CL*11
00565              FIELD  (ANOTLFI)                                        CL*11
00566              LENGTH (3)                                              CL*11
00567          END-EXEC                                                    CL*11
00568          IF ANOTLFI NUMERIC                                          CL*11
00569              MOVE ANOTLFI        TO  WS-PAYMENTS-COVERED             CL*11
00570                                      ANOTLFO                         CL*11
00571              MOVE AL-UNNON       TO  ANOTLFA                         CL*11
00572            ELSE                                                      CL*11
00573              MOVE -1             TO  ANOTLFL                         CL*11
00574              MOVE AL-UNBON       TO  ANOTLFA                         CL*11
00575              MOVE ER-2927        TO  EMI-ERROR                       CL*11
00576              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*11
00577              ADD +1  TO  WS-ERROR-COUNT.                             CL*11
00578                                                                      CL*11
00579 ********** BALLOON AMOUNT                                         EL680
00580      IF BALAMTL GREATER THAN ZERO                                    CL*11
00581          EXEC CICS BIF DEEDIT                                     EL680
00582              FIELD  (BALAMTI)                                     EL680
00583              LENGTH (12)                                          EL680
00584          END-EXEC                                                 EL680
00585          IF BALAMTI NUMERIC                                       EL680
00586              MOVE BALAMTI        TO  WS-BALLOON-AMOUNT            EL680
00587                                      BALAMTO                      EL680
00588              MOVE AL-UNNON       TO  BALAMTA                      EL680
00589            ELSE                                                   EL680
00590              MOVE -1             TO  BALAMTL                      EL680
00591              MOVE AL-UNBON       TO  BALAMTA                      EL680
00592              MOVE ER-2927        TO  EMI-ERROR                    EL680
00593              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL680
00594              ADD +1  TO  WS-ERROR-COUNT.                          EL680
00595                                                                   EL680
00596 ***********BALLOON RATE                                              CL**4
00597      IF BLRATEL GREATER THAN ZERO                                    CL*11
00598          EXEC CICS BIF DEEDIT                                        CL**4
00599              FIELD  (BLRATEI)                                        CL**4
00600              LENGTH (8)                                              CL**4
00601          END-EXEC                                                    CL**4
00602          IF BLRATEI NUMERIC                                          CL**4
00603              MOVE BLRATEI        TO  WS-BL-RATE                      CL**4
00604                                      BLRATEO                         CL**4
00605              MOVE AL-UNNON       TO  BLRATEA                         CL**4
00606            ELSE                                                      CL**4
00607              MOVE -1             TO  BLRATEL                         CL**4
00608              MOVE AL-UNBON       TO  BLRATEA                         CL**4
00609              MOVE ER-2366        TO  EMI-ERROR                       CL**4
00610              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**4
00611              ADD +1  TO  WS-ERROR-COUNT.                             CL**4
00612                                                                      CL**4
00613 ********** BALLOON PAYMENT COINCIDE W/LAST PAYMENT (Y/N)          EL680
00614      IF BALYNL GREATER THAN ZERO                                     CL*11
00615          IF BALYNI = 'Y' OR 'N'                                   EL680
00616              MOVE BALYNI         TO  WS-BALYN                     EL680
00617              MOVE AL-UANON       TO  BALYNA                       EL680
00618            ELSE                                                   EL680
00619              MOVE -1             TO  BALYNL                       EL680
00620              MOVE AL-UABON       TO  BALYNA                       EL680
00621              MOVE ER-2927        TO  EMI-ERROR                    EL680
00622              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL680
00623              ADD +1  TO  WS-ERROR-COUNT.                          EL680
00624                                                                   EL680
00625 ********** REGULAR PAYMENT AMOUNT                                 EL680
00626 *    IF BALPAYL GREATER THAN ZERO                                    CL*11
00627 *        EXEC CICS BIF DEEDIT                                        CL**5
00628 *            FIELD  (BALPAYI)                                        CL**5
00629 *            LENGTH (12)                                             CL**5
00630 *        END-EXEC                                                    CL**5
00631 *        IF BALPAYI NUMERIC  AND                                     CL**5
00632 *           BALPAYI NOT = ZERO                                       CL**5
00633 *            MOVE BALPAYI        TO  WS-REGULAR-PAYMENT              CL**5
00634 *                                    BALPAYO                         CL**5
00635 *            MOVE AL-UNNON       TO  BALPAYA                         CL**5
00636 *          ELSE                                                      CL**5
00637 *            MOVE -1             TO  BALPAYL                         CL**5
00638 *            MOVE AL-UNBON       TO  BALPAYA                         CL**5
00639 *            MOVE ER-2927        TO  EMI-ERROR                       CL**5
00640 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**5
00641 *            ADD +1  TO  WS-ERROR-COUNT.                             CL**5
00642                                                                      CL**4
00643      IF WS-ERROR-COUNT GREATER THAN ZERO                             CL*11
00644          GO TO 8200-SEND-DATAONLY.                                EL680
00645     EJECT                                                         EL680
00646                                                                   EL680
00647  4000-PERFORM-CALCULATIONS.                                       EL680
00648      IF WS-FREQ = 'MO'                                               CL*11
00649           MOVE 12 TO PPY.                                            CL*11
00650      IF WS-FREQ = '13'                                               CL*11
00651           MOVE 13 TO PPY.                                            CL*11
00652      IF WS-FREQ = 'SM'                                               CL*11
00653           MOVE 24 TO PPY.                                            CL*11
00654      IF WS-FREQ = 'BW'                                               CL*11
00655           MOVE 26 TO PPY.                                            CL*11
00656      IF WS-FREQ = 'WK'                                               CL*11
00657           MOVE 52 TO PPY.                                            CL*11
00658      IF WS-FREQ = 'SA'                                               CL*11
00659           MOVE 2 TO PPY.                                             CL*11
00660      IF WS-FREQ = 'AN'                                               CL*11
00661           MOVE 1 TO PPY.                                             CL*11
00662                                                                   EL680
00663      IF WS-BASIS-X = 'AF'                                         EL680
00664           MOVE 1 TO WS-BASIS.                                     EL680
00665      IF WS-BASIS-X = 'TP'                                         EL680
00666           MOVE 2 TO WS-BASIS.                                     EL680
00667      IF WS-BASIS-X = 'NP'                                         EL680
00668           MOVE 3 TO WS-BASIS.                                     EL680
00669                                                                      CL*11
00670      MOVE WS-AMOUNT-REQUESTED TO OTF.                                CL*11
00671                                                                   EL680
00672      COMPUTE I = WS-APR / (100 * PPY).                            EL680
00673                                                                   EL680
00674      MOVE WS-ADDTL-DAYS TO ADL-DAYS.                              EL680
00675                                                                   EL680
00676      COMPUTE ADL-ADJ = 1 + I * ADL-DAYS / (365 / PPY).            EL680
00677                                                                   EL680
00678      COMPUTE ATF = OTF * ADL-ADJ.                                 EL680
00679                                                                   EL680
00680      MOVE WS-TRUNC TO COV-TERM.                                   EL680
00681                                                                   EL680
00682      MOVE WS-PAYMENTS-COVERED TO AHN.                             EL680
00683                                                                   EL680
00684      MOVE WS-NUMBER-OF-PMTS TO N.                                    CL*11
00685                                                                      CL*11
00686      IF AHN LESS 1                                                EL680
00687          MOVE COV-TERM TO AHN.                                    EL680
00688                                                                   EL680
00689      IF AHN LESS 1                                                EL680
00690          MOVE N TO AHN.                                           EL680
00691                                                                   EL680
00692      MOVE WS-EXTRA TO EMII.                                       EL680
00693                                                                   EL680
00694      EJECT                                                        EL680
00695                                                                   EL680
00696  4000-CONTINUE-CALCS-1.                                           EL680
00697 ********* M USED FOR TRUNCATED NET PAY                               CL*14
00698      IF PI-COMPANY-ID EQUAL 'CDC' OR 'GTL' OR 'MON' OR 'CSO'         CL*12
00699                          OR 'LGX'                                    CL*12
00700         IF WS-TRUNC NOT EQUAL +0                                     CL**7
00701            MOVE WS-TRUNC         TO M.                               CL**7
00702                                                                      CL**7
00703      MOVE WS-LIFE-RATE TO LIFE-RATE.                                 CL*11
00704                                                                   EL680
00705      COMPUTE LR = (LIFE-RATE / 100) * 12 / PPY.                   EL680
00706                                                                   EL680
00707      COMPUTE LIFEDEV = WS-LIFE-DEVIATION / 100.                   EL680
00708                                                                   EL680
00709      COMPUTE LR = LR * LIFEDEV.                                   EL680
00710                                                                   EL680
00711      MOVE WS-AH-RATE TO AH-RATE.                                  EL680
00712                                                                   EL680
00713      COMPUTE AH = AH-RATE / 100.                                  EL680
00714                                                                   EL680
00715      COMPUTE AHDEV = WS-AH-DEVIATION / 100.                       EL680
00716                                                                   EL680
00717      COMPUTE AH = AH * AHDEV.                                     EL680
00718                                                                      CL**4
00719 ****** BALLOON RATE                                                  CL**4
00720      MOVE WS-BL-RATE TO BL-RATE.                                     CL**4
00721                                                                      CL**4
00722      COMPUTE BL = BL-RATE / 100.                                     CL**4
00723 *******************                                                  CL**4
00724                                                                   EL680
00725 *****  N1 USED FOR O/B                                               CL*14
00726      IF COV-TERM = ZERO                                              CL**2
00727         COMPUTE N1 = N * 12 / PPY                                    CL**2
00728        ELSE                                                          CL**2
00729         COMPUTE N1 = COV-TERM * 12 / PPY.                            CL**2
00730                                                                   EL680
00731      COMPUTE K1 = 12 / PPY.                                       EL680
00732                                                                      CL*14
00733      IF PPY = 1 OR 2                                                 CL*14
00734          MOVE 1     TO K1.                                           CL*14
00735                                                                   EL680
00736      COMPUTE PADJ = (N1 + K1) / (N1 + 1).                         EL680
00737                                                                   EL680
00738      IF WS-OBYN = 'Y'                                                CL**4
030612        IF PI-COMPANY-ID EQUAL 'CSO' OR 'CID' OR 'LGX' OR 'AHL'
062121              or 'FNL'
00740         COMPUTE L = LR * PADJ * M / 12 * (N1 + 1) / 20 * 12 / N1     CL*12
00741         ELSE                                                         CL*12
00742         COMPUTE L = LR * PADJ * N / 12 * (N1 + 1) / 20 * 12 / N1  EL680
00743      ELSE                                                            CL**7
00744         IF PI-COMPANY-ID EQUAL 'CDC' OR 'GTL' OR 'MON' OR 'CSO'      CL*12
062121                         OR 'CID' OR 'AHL' OR 'FNL'                  CL*12
00745                          OR 'LGX'                                    CL*12
00746            COMPUTE L = LR * PADJ * M / 12                            CL**7
00747         ELSE                                                         CL**7
00748 *          COMPUTE L = LR * PADJ * N / 12.                           CL*14
00749            COMPUTE L = LR * PADJ * AHN / 12.                         CL*14
00750                                                                   EL680
00751      IF WS-BALLOON-AMOUNT NOT = ZERO                              EL680
00752          GO TO 7000-BALLOON-CALC.                                 EL680
00753                                                                   EL680
00754      COMPUTE V = 1 / (1 + I).                                     EL680
00755                                                                   EL680
00756      COMPUTE VU = 1 - V ** N.                                     EL680
00757                                                                   EL680
00758      COMPUTE ANGLN = VU / I.                                      EL680
00759                                                                   EL680
00760      IF PI-COMPANY-ID EQUAL 'CDC' OR 'GTL' OR 'MON' OR 'CSO'         CL*12
062121                         OR 'CID' OR 'AHL' or 'FNL'
00761                          OR 'LGX'                                    CL*12
00762         COMPUTE TOT-PMTS = I / VU * M                                CL**7
00763      ELSE                                                            CL**7
00764         COMPUTE TOT-PMTS = I / VU * N.                               CL**7
00765                                                                   EL680
00766      COMPUTE ATOT-PMTS = I / VU * AHN.                            EL680
00767                                                                   EL680
00768      EJECT                                                        EL680
00769                                                                   EL680
00770  4000-CONTINUE-CALCS-2.                                           EL680
00771      GO TO 4000-COMM, 4000-COMM, 4000-NP                             CL**4
00772          DEPENDING ON WS-BASIS.                                   EL680
00773                                                                   EL680
00774  4000-NP.                                                         EL680
00775      MOVE COV-TERM TO M.                                          EL680
00776      IF M = 0                                                     EL680
00777         MOVE N TO M.                                              EL680
00778                                                                   EL680
00779      COMPUTE VU1 = 1 - V ** (N - M).                              EL680
00780                                                                   EL680
00781      COMPUTE ANGLM = VU1 / I.                                     EL680
00782                                                                   EL680
00783      COMPUTE VA = (M - ANGLN + ANGLM) * 2 * N                     EL680
00784                    / (I * M * (2 * N - M + 1) * ANGLN).           EL680
00785                                                                      CL**7
00786      IF PI-COMPANY-ID EQUAL 'CDC' OR 'GTL' OR 'MON' OR 'CSO'         CL*12
062121                         OR 'CID' OR 'AHL' OR 'FNL'
00787                          OR 'LGX'                                    CL*12
00788         COMPUTE VA = VA * ((1 + N) / (1 + M)).                       CL**7
00789                                                                   EL680
00790      COMPUTE RA = 1 - (N - M) * (N - M + 1) / (N * (N + 1)).      EL680
00791                                                                   EL680
00792      COMPUTE TA = VA * RA.                                        EL680
00793                                                                   EL680
00794      COMPUTE NPLF = TA * L * (1 + EMII * I).                      EL680
00795                                                                   EL680
00796      IF WS-NPBEN = 'Y'                                            EL680
00797         COMPUTE NPLF = NPLF / (1 - NPLF).                         EL680
00798                                                                   EL680
00799  4000-COMM.                                                       EL680
00800      MOVE ZERO TO LIFE-PREM AH-PREM.                              EL680
00801                                                                   EL680
00802      PERFORM 4000-GET-PREMIUM THRU 4000-EXIT.                        CL**4
00803      GO TO 6000-DISPLAY-RESULTS.                                  EL680
00804                                                                   EL680
00805  4000-GET-PREMIUM.                                                EL680
00806      PERFORM 5000-COMPUTE-PREMIUMS THRU 5000-EXIT                    CL**4
00807          VARYING XREPT FROM +1 BY +1 UNTIL XREPT = +15.           EL680
00808  4000-EXIT.                                                          CL**4
00809      EXIT.                                                        EL680
00810                                                                   EL680
00811     EJECT                                                         EL680
00812                                                                   EL680
00813  5000-COMPUTE-PREMIUMS.                                           EL680
00814      GO TO 5000-AF, 5000-TP, 5000-NP                                 CL**4
00815          DEPENDING ON WS-BASIS.                                   EL680
00816                                                                   EL680
00817  5000-AF.                                                         EL680
00818      COMPUTE LIFE-PREM = L / (1 - L) * (1 + AH-PREM).             EL680
00819      GO TO 5000-GET-AH-PREM.                                      EL680
00820                                                                   EL680
00821  5000-TP.                                                         EL680
00822      COMPUTE LIFE-PREM = TOT-PMTS * L / (1 - TOT-PMTS * L)        EL680
00823                              * (1 + AH-PREM).                        CL*11
00824      GO TO 5000-GET-AH-PREM.                                      EL680
00825                                                                   EL680
00826  5000-NP.                                                         EL680
00827      COMPUTE LIFE-PREM = NPLF * (1 + AH-PREM).                    EL680
00828      GO TO 5000-GET-AH-PREM.                                      EL680
00829                                                                   EL680
00830  5000-GET-AH-PREM.                                                EL680
00831      COMPUTE AH-PREM = ATOT-PMTS * AH / (1 - ATOT-PMTS * AH)      EL680
00832                              * (1 + LIFE-PREM).                   EL680
00833                                                                   EL680
00834      IF AH = 0 OR                                                    CL**4
00835         LR = 0                                                       CL**4
00836           GO TO 4000-EXIT.                                           CL**4
00837                                                                   EL680
00838  5000-EXIT.                                                          CL**4
00839      EXIT.                                                        EL680
00840                                                                   EL680
00841    EJECT                                                          EL680
00842                                                                   EL680
00843  6000-DISPLAY-RESULTS.                                            EL680
00844      IF CHG-LF OR CHG-LF-AH                                          CL**6
00845          COMPUTE L-PREMIUM = LIFE-PREM * ATF                         CL**6
00846        ELSE                                                          CL**6
00847          COMPUTE L-PREMIUM = LIFE-PREM * OTF.                        CL**6
00848                                                                   EL680
00849      COMPUTE L-PREMIUM = (L-PREMIUM * 100 + .502) / 100.          EL680
00850                                                                   EL680
00851      IF CHG-AH OR CHG-LF-AH                                          CL**6
00852          COMPUTE A-PREMIUM = AH-PREM * ATF                           CL**6
00853        ELSE                                                          CL**6
00854          COMPUTE A-PREMIUM = AH-PREM * OTF.                          CL**6
00855                                                                   EL680
00856      COMPUTE A-PREMIUM = (A-PREMIUM * 100 + .502) / 100.          EL680
00857                                                                   EL680
00858      COMPUTE PAYMENT = I / VU * (ATF + L-PREMIUM + A-PREMIUM).    EL680
00859                                                                   EL680
00860      COMPUTE PAYMENT = (PAYMENT * 100 + .002) / 100.              EL680
00861                                                                   EL680
00862      COMPUTE TOT-FIN = OTF + L-PREMIUM + A-PREMIUM.               EL680
00863                                                                   EL680
00864      COMPUTE TOT-PAYMT = N * PAYMENT.                             EL680
00865                                                                   EL680
00866      COMPUTE TOT-INTRS = TOT-PAYMT - TOT-FIN.                     EL680
00867                                                                   EL680
00868  6500-COMMON-DISPLAY.                                             EL680
00869      MOVE TOT-FIN             TO BAMOUNTO.                        EL680
00870                                                                   EL680
00871      MOVE L-PREMIUM           TO BLAMTO.                          EL680
00872      MOVE A-PREMIUM           TO BDAMTO.                          EL680
00873      MOVE PAYMENT             TO BMOPMTO.                         EL680
00874      MOVE TOT-PAYMT           TO BTOTPMTO.                        EL680
00875      MOVE TOT-INTRS           TO BTOTINTO.                        EL680
00876      MOVE B                   TO BALPMTO.                         EL680
00877                                                                   EL680
00878      MOVE -1                  TO ANAMEL.                          EL680
00879      GO TO 8200-SEND-DATAONLY.                                    EL680
00880                                                                   EL680
00881      EJECT                                                        EL680
00882  7000-BALLOON-CALC.                                               EL680
00883      MOVE WS-BALLOON-AMOUNT   TO B.                               EL680
00884 *    MOVE WS-REGULAR-PAYMENT  TO MP.                                 CL**5
00885                                                                   EL680
00886      COMPUTE V = 1 / (1 + I).                                     EL680
00887                                                                   EL680
00888      COMPUTE V-NTH = V ** N.                                      EL680
00889                                                                   EL680
00890      COMPUTE ANGLN = (1 - V-NTH) / I.                             EL680
00891                                                                   EL680
00892      COMPUTE ANGLN-LESS-1 = (1 - V-NTH / V) / I.                  EL680
00893                                                                   EL680
00894      COMPUTE NM1 = N - 1.                                         EL680
00895                                                                   EL680
00896      COMPUTE AEQ = N / (N + 1).                                   EL680
00897                                                                   EL680
00898      IF WS-BALYN = 'Y'                                            EL680
00899          MOVE N TO NM1                                            EL680
00900          MOVE ANGLN TO ANGLN-LESS-1                               EL680
00901          MOVE 1 TO AEQ.                                           EL680
00902                                                                   EL680
00903      IF WS-BASIS = 2                                              EL680
00904          GO TO 7500-TP-BALLOON.                                   EL680
00905                                                                   EL680
00906 *    IF MP NOT = ZERO                                                CL**5
00907 *        GO TO 7200-REGULAR-PAYMENT.                                 CL**5
00908                                                                   EL680
00909      COMPUTE MPT = (N + 1) / 2 * (ATF - B * V-NTH) + L * (1 + EMIIEL680
00910                   * I) * B * ANGLN.                               EL680
00911                                                                   EL680
00912      COMPUTE MPB = (N + 1) / 2 * (ANGLN-LESS-1 - NM1 * AH) - L    EL680
00913                    * (1 + EMII * I) * ((NM1 - ANGLN-LESS-1) / I). EL680
00914                                                                   EL680
00915      DIVIDE MPT BY MPB GIVING MP.                                 EL680
00916                                                                   EL680
00917      GO TO 8000-DISPLAY-BALLOON-RESULTS.                          EL680
00918                                                                   EL680
00919  7200-REGULAR-PAYMENT.                                            EL680
00920      COMPUTE MPB = (N + 1) / 2 * (ANGLN-LESS-1 - NM1 * AH) - L    EL680
00921                     * (1 + EMII * I) * (NM1 - ANGLN-LESS-1) / I.  EL680
00922                                                                   EL680
00923      COMPUTE B = (MPB * MP - (N + 1) / 2 * ATF) / (L * (1 + EMII  EL680
00924                    * I) * ANGLN - (N + 1) / 2 * V-NTH).           EL680
00925                                                                   EL680
00926      GO TO 8000-DISPLAY-BALLOON-RESULTS.                          EL680
00927                                                                   EL680
00928      EJECT                                                        EL680
00929  7500-TP-BALLOON.                                                 EL680
00930 *    IF MP NOT = ZERO                                                CL**5
00931 *        GO TO 7600-REGULAR-PAYMENT.                                 CL**5
00932                                                                   EL680
00933      COMPUTE MPT = (ATF - B * V-NTH) + L * B * 2 * N / (N + 1).   EL680
00934                                                                   EL680
00935      COMPUTE MPB = (ANGLN-LESS-1 - NM1 * AH) - L * NM1 * AEQ.     EL680
00936                                                                   EL680
00937      COMPUTE MP = MPT / MPB.                                      EL680
00938                                                                   EL680
00939      GO TO 7700-SKIP.                                             EL680
00940                                                                   EL680
00941  7600-REGULAR-PAYMENT.                                            EL680
00942      COMPUTE MPB = (ANGLN-LESS-1 - NM1 * AH) - L * NM1 * AEQ.     EL680
00943                                                                   EL680
00944      COMPUTE B = (MPB * MP - ATF) / (L * 2 * N / (N + 1) - V-NTH).EL680
00945                                                                   EL680
00946  7700-SKIP.                                                       EL680
00947      COMPUTE IA = MP * NM1 + B.                                   EL680
00948                                                                   EL680
00949      IF WS-BALYN = 'Y'                                            EL680
00950          COMPUTE VA = 1 + B / IA * (N - 1) / (N + 1)              EL680
00951       ELSE                                                        EL680
00952          COMPUTE VA = N * (IA + B) / (IA * (N + 1)).              EL680
00953                                                                   EL680
00954  8000-DISPLAY-BALLOON-RESULTS.                                    EL680
00955      COMPUTE A-PREMIUM = NM1 * MP * AH.                           EL680
00956                                                                   EL680
00957      IF WS-BASIS = 2                                              EL680
00958         COMPUTE L-PREMIUM = L * IA * VA                           EL680
00959       ELSE                                                        EL680
00960         COMPUTE L-PREMIUM = MP * ANGLN-LESS-1 + B * V-NTH - ATF - EL680
00961                 A-PREMIUM.                                        EL680
00962                                                                      CL**4
00963 ** IF BALLOON RATE KEYED, ADD PREMIUM TO LIFE PREMIUM                CL**4
00964      IF BL GREATER ZERO                                              CL**4
00965          COMPUTE L-PREMIUM = L-PREMIUM + (B * BL).                   CL**4
00966                                                                   EL680
00967      COMPUTE L-PREMIUM = (L-PREMIUM * 100 + .502) / 100.          EL680
00968                                                                   EL680
00969      COMPUTE A-PREMIUM = (A-PREMIUM * 100 + .502) / 100.          EL680
00970                                                                   EL680
00971      COMPUTE TOT-FIN = OTF + L-PREMIUM + A-PREMIUM.               EL680
00972                                                                   EL680
00973      COMPUTE MP = (MP * 100 + .002) / 100.                        EL680
00974      COMPUTE TOT-PAYMT = MP * NM1 + B.                            EL680
00975      MOVE MP TO PAYMENT.                                          EL680
00976                                                                   EL680
00977      COMPUTE TOT-INTRS = TOT-PAYMT - TOT-FIN.                     EL680
00978                                                                   EL680
00979      GO TO 6500-COMMON-DISPLAY.                                   EL680
00980                                                                   EL680
00981      EJECT                                                        EL680
00982  8100-SEND-INITIAL-MAP SECTION.                                   EL680
00983      MOVE LOW-VALUES             TO  EL680AI.                     EL680
00984                                                                   EL680
00985      MOVE SAVE-DATE              TO  ADATEO.                      EL680
00986      MOVE EIBTIME                TO  WS-TIME-WORK.                EL680
00987      MOVE WS-TIME                TO  ATIMEO.                      EL680
00988                                                                   EL680
00989      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     EL680
00990                                                                   EL680
00991      EXEC CICS SEND                                               EL680
00992          FROM   (EL680AI)                                         EL680
00993          MAPSET (MAPSET-NAME)                                     EL680
00994          MAP    (MAP-NAME)                                        EL680
00995          ERASE                                                    EL680
00996      END-EXEC.                                                    EL680
00997                                                                   EL680
00998      GO TO 9100-RETURN-TRAN.                                      EL680
00999                                                                   EL680
01000  8100-EXIT.                                                       EL680
01001       EXIT.                                                       EL680
01002                                                                   EL680
01003  8200-SEND-DATAONLY SECTION.                                      EL680
01004      MOVE SAVE-DATE              TO  ADATEO.                      EL680
01005      MOVE EIBTIME                TO  WS-TIME-WORK.                EL680
01006      MOVE WS-TIME                TO  ATIMEO.                      EL680
01007                                                                   EL680
01008      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     EL680
01009                                                                   EL680
01010      IF EMI-ERROR = 0                                             EL680
01011         IF EIBAID = DFHPF1                                        EL680
01012            MOVE SPACE            TO EIBAID                           CL**4
01013            PERFORM 9000-START-PRINTER THRU 9000-EXIT                 CL**4
01014            MOVE SPACES           TO SPC-FLD                          CL**4
01015            MOVE SPC-FLD          TO PFENTERI                         CL**4
01016            MOVE 0                TO PFENTERL                         CL**4
01017            MOVE AL-UNNOF         TO PFENTERA.                        CL**4
01018                                                                   EL680
01019      EXEC CICS SEND DATAONLY                                      EL680
01020          FROM   (EL680AI)                                         EL680
01021          MAPSET (MAPSET-NAME)                                     EL680
01022          MAP    (MAP-NAME)                                        EL680
01023          CURSOR                                                   EL680
01024      END-EXEC.                                                    EL680
01025                                                                   EL680
01026      GO TO 9100-RETURN-TRAN.                                      EL680
01027                                                                   EL680
01028  8200-EXIT.                                                       EL680
01029      EXIT.                                                        EL680
01030                                                                   EL680
01031      EJECT                                                        EL680
01032  8300-SEND-TEXT SECTION.                                          EL680
01033      EXEC CICS SEND TEXT                                          EL680
01034          FROM   (LOGOFF-TEXT)                                     EL680
01035          LENGTH (LOGOFF-LENGTH)                                   EL680
01036          ERASE  FREEKB                                            EL680
01037      END-EXEC.                                                    EL680
01038                                                                   EL680
01039      EXEC CICS RETURN                                             EL680
01040      END-EXEC.                                                    EL680
01041                                                                   EL680
01042  8300-EXIT.                                                       EL680
01043      EXIT.                                                        EL680
01044                                                                   EL680
01045  8400-NOT-FOUND.                                                  EL680
01046      MOVE ER-0190                TO EMI-ERROR.                    EL680
01047      PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.                   EL680
01048      GO TO 8200-SEND-DATAONLY.                                    EL680
01049                                                                   EL680
01050  8820-TERMID-ERROR.                                               EL680
01051      MOVE ER-0412                TO EMI-ERROR.                    EL680
01052      GO TO 8999-OPEN-ERROR.                                       EL680
01053                                                                   EL680
01054  8830-TRANS-ERROR.                                                EL680
01055      MOVE ER-0413                TO EMI-ERROR.                    EL680
01056      GO TO 8999-OPEN-ERROR.                                       EL680
01057                                                                   EL680
01058  8840-CNTL-NOT-OPEN.                                              EL680
01059      MOVE ER-0042 TO EMI-ERROR.                                   EL680
01060      GO TO 8999-OPEN-ERROR.                                       EL680
01061                                                                   EL680
01062  8800-UNAUTHORIZED-ACCESS.                                        EL680
01063      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL680
01064      GO TO 8300-SEND-TEXT.                                        EL680
01065                                                                   EL680
01066  8810-PF23.                                                       EL680
01067      MOVE EIBAID                 TO PI-ENTRY-CD-1.                EL680
01068      MOVE XCTL-005               TO PGM-NAME.                     EL680
01069      GO TO 9300-XCTL.                                             EL680
01070                                                                   EL680
01071  8999-OPEN-ERROR.                                                 EL680
01072      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL680
01073      GO TO 8200-SEND-DATAONLY.                                    EL680
01074                                                                   EL680
01075      EJECT                                                        EL680
01076  9000-START-PRINTER.                                              EL680
01077      MOVE PI-COMPANY-ID TO CNTL-COMP-ID.                          EL680
01078                                                                   EL680
01079      EXEC CICS HANDLE CONDITION                                   EL680
01080           NOTOPEN     (8840-CNTL-NOT-OPEN)                        EL680
01081           NOTFND      (8400-NOT-FOUND)                            EL680
01082           TERMIDERR   (8820-TERMID-ERROR)                         EL680
01083           TRANSIDERR  (8830-TRANS-ERROR)                          EL680
01084      END-EXEC.                                                    EL680
01085                                                                   EL680
01086      MOVE '1'        TO CNTL-REC-TYPE.                               CL**4
01087      MOVE SPACES     TO CNTL-ACCESS.                                 CL**4
01088      MOVE +0         TO CNTL-SEQ-NO.                                 CL**4
01089                                                                   EL680
01090      EXEC CICS READ                                               EL680
01091          DATASET ('ELCNTL')                                       EL680
01092          SET     (ADDRESS OF CONTROL-FILE)                           CL*14
01093          RIDFLD  (ELCNTL-KEY)                                     EL680
01094      END-EXEC.                                                    EL680
01095                                                                   EL680
062121     IF PI-COMPANY-ID = 'DMD' OR 'CID' OR 'AHL' OR 'FNL'
01097 *        MOVE EIBTRMID       TO CF-FORMS-PRINTER-ID                  CL*15
01098          MOVE EL680AI        TO PI-PROGRAM-WORK-AREA                 CL*15
01099          EXEC CICS START                                             CL*15
01100               INTERVAL  (0)                                          CL*15
01101               TRANSID   (START-TRANS-ID)                             CL*15
01102               FROM      (PROGRAM-INTERFACE-BLOCK)                    CL*15
01103               LENGTH    (PI-COMM-LENGTH)                             CL*15
01104 *             TERMID (CF-FORMS-PRINTER-ID)                           CL*15
01105               END-EXEC                                               CL*15
01106      ELSE                                                            CL*15
01107          EXEC CICS START                                             CL*15
01108              TRANSID(START-TRANS-ID)                                 CL*15
01109              TERMID (CF-FORMS-PRINTER-ID)                            CL*15
01110              FROM   (EL680AI)                                        CL*15
01111              LENGTH (426)                                            CL*15
01112          END-EXEC.                                                   CL*15
01113                                                                   EL680
01114  9000-EXIT.                                                          CL**4
01115      EXIT.                                                        EL680
01116                                                                   EL680
01117      EJECT                                                        EL680
01118  9100-RETURN-TRAN.                                                EL680
01119      MOVE    EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.          EL680
01120      MOVE SCREEN-NUMBER             TO PI-CURRENT-SCREEN-NO.         CL**4
01121      EXEC CICS RETURN                                             EL680
01122          TRANSID    (TRANS-ID)                                    EL680
01123          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL680
01124          LENGTH     (PI-COMM-LENGTH)                              EL680
01125      END-EXEC.                                                       CL**4
01126                                                                   EL680
01127      GOBACK.                                                      EL680
01128                                                                   EL680
01129  9200-RETURN-MAIN-MENU.                                           EL680
01130      MOVE XCTL-626               TO PGM-NAME.                     EL680
01131      GO TO 9300-XCTL.                                             EL680
01132                                                                   EL680
01133  9300-XCTL.                                                       EL680
01134      EXEC CICS XCTL                                               EL680
01135          PROGRAM    (PGM-NAME)                                    EL680
01136          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL680
01137          LENGTH     (PI-COMM-LENGTH)                              EL680
01138      END-EXEC.                                                    EL680
01139                                                                   EL680
01140  9400-CLEAR.                                                      EL680
01141      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.                     EL680
01142      GO TO 9300-XCTL.                                             EL680
01143                                                                   EL680
01144  9500-PF12.                                                       EL680
01145      MOVE XCTL-010               TO PGM-NAME.                     EL680
01146      GO TO 9300-XCTL.                                             EL680
01147                                                                   EL680
01148  9600-PGMID-ERROR.                                                EL680
01149      EXEC CICS HANDLE CONDITION                                   EL680
01150          PGMIDERR    (8300-SEND-TEXT)                             EL680
01151      END-EXEC.                                                    EL680
01152                                                                   EL680
01153      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.           EL680
01154      MOVE ' '                    TO PI-ENTRY-CD-1.                EL680
01155      MOVE XCTL-005               TO PGM-NAME.                     EL680
01156      MOVE PGM-NAME               TO LOGOFF-PGM.                   EL680
01157      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  EL680
01158      GO TO 9300-XCTL.                                             EL680
01159                                                                   EL680
01160  9700-LINK-DATE-CONVERT.                                          EL680
01161      EXEC CICS LINK                                               EL680
01162          PROGRAM    ('ELDATCV')                                   EL680
01163          COMMAREA   (DATE-CONVERSION-DATA)                        EL680
01164          LENGTH     (DC-COMM-LENGTH)                              EL680
01165      END-EXEC.                                                    EL680
01166                                                                   EL680
01167  9700-EXIT.                                                       EL680
01168      EXIT.                                                        EL680
01169                                                                   EL680
01170  9900-ERROR-FORMAT.                                               EL680
01171      IF NOT EMI-ERRORS-COMPLETE                                   EL680
01172          MOVE LINK-001           TO PGM-NAME                      EL680
01173          EXEC CICS LINK                                           EL680
01174              PROGRAM    (PGM-NAME)                                EL680
01175              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           EL680
01176              LENGTH     (EMI-COMM-LENGTH)                         EL680
01177          END-EXEC.                                                EL680
01178                                                                   EL680
01179  9900-EXIT.                                                       EL680
01180      EXIT.                                                        EL680
01181                                                                   EL680
01182  9990-ABEND.                                                      EL680
01183      MOVE LINK-004               TO PGM-NAME.                     EL680
01184      MOVE DFHEIBLK               TO EMI-LINE1.                       CL**4
01185                                                                      CL**4
01186      EXEC CICS LINK                                               EL680
01187          PROGRAM   (PGM-NAME)                                     EL680
01188          COMMAREA  (EMI-LINE1)                                    EL680
01189          LENGTH    (72)                                           EL680
01190      END-EXEC.                                                    EL680
01191                                                                   EL680
01192      GO TO 8200-SEND-DATAONLY.                                    EL680
01193                                                                   EL680
