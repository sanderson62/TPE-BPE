00001  IDENTIFICATION DIVISION.                                         07/02/96
00002                                                                   EL657
00003  PROGRAM-ID.                 EL657 .                                 LV010
00004 *              PROGRAM CONVERTED BY                                  CL**9
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**9
00006 *              CONVERSION DATE 02/14/96 12:01:06.                    CL**9
00007 *                            VMOD=2.010.                             CL*10
00008 *                                                                 EL657
00008 *                                                                 EL657
00009 *AUTHOR.     LOGIC,INC.                                              CL**9
00010 *            DALLAS, TEXAS.                                          CL**9
00011                                                                   EL657
00012 *DATE-COMPILED.                                                      CL**9
00013 *SECURITY.   *****************************************************   CL**9
00014 *            *                                                   *   CL**9
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**9
00016 *            *                                                   *   CL**9
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**9
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**9
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**9
00020 *            *                                                   *   CL**9
00021 *            *****************************************************   CL**9
00022                                                                   EL657
00023 *REMARKS.    TRANSACTION - EXH1 - REINSURANCE TABLES TEST CASE.      CL**8
040208******************************************************************
040208*                   C H A N G E   L O G
040208*
040208* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
040208*-----------------------------------------------------------------
040208*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
040208* EFFECTIVE    NUMBER
040208*-----------------------------------------------------------------
040208* 040208  IR2008032700001  PEMA  FIX LFPRM,AHPRM,LFBEN,AHBEN FLDS
040208******************************************************************
00024                                                                   EL657
00025  ENVIRONMENT DIVISION.                                            EL657
00026      EJECT                                                        EL657
00027  DATA DIVISION.                                                   EL657
00028  WORKING-STORAGE SECTION.                                         EL657
00029  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.         CL**9
00030  77  FILLER  PIC X(32)  VALUE '********************************'. EL657
00031  77  FILLER  PIC X(32)  VALUE '*    EL657 WORKING STORAGE     *'. EL657
00032  77  FILLER   PIC X(32) VALUE '******** VMOD=2.010 ************'.    CL*10
00033                                                                   EL657
00034  77  INCR1                           PIC S999    VALUE +1  COMP-3.   CL**4
00035  77  INCR2                           PIC S999    VALUE +1  COMP-3.   CL**4
00036                                                                      CL**4
00037  01  WS-WORK-AREA.                                                   CL**4
00038      05  WS-LF-PREMIUM-AMT           PIC S9(7)V99 VALUE ZEROS.
00039      05  WS-LF-BENEFIT-AMT           PIC S9(9)V99 VALUE ZEROS.
00040                                                                   EL657
00041  01  WS-DATE-AREA.                                                EL657
00042      05  SAVE-DATE                   PIC X(8)    VALUE SPACES.    EL657
00043      05  SAVE-BIN-DATE               PIC XX      VALUE SPACES.       CL**4
00044                                                                   EL657
00045                                      COPY ELCREIN.                   CL**8
00046                                                                   EL657
00047      EJECT                                                        EL657
00048  01  STANDARD-AREAS.                                              EL657
00049      12  MAP-NAME                    PIC X(8)    VALUE 'EL657A'.  EL657
00050      12  MAPSET-NAME                 PIC X(8)    VALUE 'EL657S'.  EL657
00051      12  TRANS-ID                    PIC X(4)    VALUE 'EXH1'.    EL657
00052      12  PGM-NAME                    PIC X(8)    VALUE SPACES.    EL657
00053      12  THIS-PGM                    PIC X(8)    VALUE 'EL657'.   EL657
00054      12  LINK-ELREIN                 PIC X(8)    VALUE 'ELREIN'.  EL657
00055      12  CERT-FILE-ID                PIC X(8)    VALUE 'ELCERT'.  EL657
00056      12  CNTL-FILE-ID                PIC X(8)    VALUE 'ELCNTL'.  EL657
00057      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'. EL657
00058      12  LINK-001                    PIC X(8)    VALUE 'EL001'.   EL657
00059      12  LINK-004                    PIC X(8)    VALUE 'EL004'.   EL657
00060      12  LINK-005                    PIC X(8)    VALUE 'EL005'.      CL**2
00061      12  LINK-010                    PIC X(8)    VALUE 'EL010'.      CL**2
00062      12  LINK-126                    PIC X(8)    VALUE 'EL126'.      CL**2
00063      12  TIME-IN                     PIC S9(7).                   EL657
00064      12  TIME-OUT-R   REDEFINES TIME-IN.                          EL657
00065          16  FILLER                  PIC X.                       EL657
00066          16  TIME-OUT                PIC 99V99.                   EL657
00067          16  FILLER                  PIC XX.                         CL**4
00068      12  REIN-LENGTH                 PIC S9(4)   VALUE +4000 COMP.EL657
00069      12  ITEM-VALUE                  PIC S9(4)   VALUE +1    COMP.EL657
00070      12  QID.                                                     EL657
00071          16  QID-TERM                PIC X(4).                    EL657
00072          16  FILLER                  PIC X(4)    VALUE '657A'.    EL657
00073                                                                   EL657
00074  01  BLD-LINE.                                                    EL657
00075      12  FILLER                      PIC X       VALUE SPACE.     EL657
00076      12  BL-CEDED                    PIC XXX.                     EL657
00077      12  FILLER                      PIC X       VALUE SPACE.     EL657
00078      12  BL-CEDED-SUB                PIC XXX.                     EL657
00079      12  FILLER                      PIC X(6)    VALUE SPACE.     EL657
00080      12  BL-LF-PREMIUM               PIC ZZZZ,ZZZ.ZZ.                CL**6
00081      12  FILLER                      PIC XXX     VALUE SPACE.     EL657
00082      12  BL-LF-BENEFIT               PIC ZZZ,ZZZ,ZZZ.ZZ.             CL**6
00083      12  FILLER                      PIC X(7)    VALUE SPACE.     EL657
00084      12  BL-AH-PREMIUM               PIC ZZZZ,ZZZ.ZZ.                CL**6
00085      12  FILLER                      PIC XXX     VALUE SPACE.     EL657
00086      12  BL-AH-BENEFIT               PIC ZZZZ,ZZZ.ZZ.                CL**6
00087      12  FILLER                      PIC X(5).                    EL657
00088                                                                   EL657
00089  01  SAVE-REIN-WORK.                                              EL657
00090      12  REIN-SUB-1                  PIC S9(4)   VALUE +0   COMP. EL657
00091      12  REIN-SUB-2                  PIC S9(4)   VALUE +0   COMP. EL657
00092      12  REIN-MAX                    PIC S9(4)   VALUE +30  COMP. EL657
00093      12  REIN-LOW-TIME               PIC 9(6)    VALUE  999999.   EL657
00094      12  REIN-LOW-SUB                PIC S9(4)   VALUE +1   COMP. EL657
00095                                                                   EL657
00096      EJECT                                                        EL657
00097  01  ERROR-MESSAGES.                                              EL657
00098      12  ER-0008                     PIC X(4)    VALUE '0008'.       CL**2
00099      12  ER-0029                     PIC X(4)    VALUE '0029'.    EL657
00100      12  ER-0130                     PIC X(4)    VALUE '0130'.    EL657
00101      12  ER-0131                     PIC X(4)    VALUE '0131'.    EL657
00102      12  ER-0195                     PIC X(4)    VALUE '0195'.    EL657
00103      12  ER-0196                     PIC X(4)    VALUE '0196'.    EL657
00104      12  ER-0197                     PIC X(4)    VALUE '0197'.    EL657
00105      12  ER-0203                     PIC X(4)    VALUE '0203'.    EL657
00106      12  ER-0244                     PIC X(4)    VALUE '0244'.    EL657
00107      12  ER-2203                     PIC X(4)    VALUE '2203'.    EL657
00108      12  ER-2205                     PIC X(4)    VALUE '2205'.    EL657
00109      12  ER-2223                     PIC X(4)    VALUE '2223'.    EL657
00110      12  ER-2301                     PIC X(4)    VALUE '2301'.    EL657
00111      12  ER-2341                     PIC X(4)    VALUE '2341'.    EL657
00112      12  ER-2425                     PIC X(4)    VALUE '2425'.    EL657
00113      12  ER-2429                     PIC X(4)    VALUE '2429'.    EL657
00114      12  ER-2473                     PIC X(4)    VALUE '2473'.    EL657
00115      12  ER-2474                     PIC X(4)    VALUE '2474'.    EL657
00116      12  ER-2482                     PIC X(4)    VALUE '2482'.    EL657
00117      12  ER-2484                     PIC X(4)    VALUE '2484'.    EL657
00118      12  ER-2485                     PIC X(4)    VALUE '2485'.    EL657
00119      12  ER-2489                     PIC X(4)    VALUE '2489'.    EL657
00120      12  ER-2504                     PIC X(4)    VALUE '2504'.    EL657
00121      12  ER-2505                     PIC X(4)    VALUE '2505'.    EL657
00122      12  ER-2615                     PIC X(4)    VALUE '2615'.    EL657
00123                                                                   EL657
00124      EJECT                                                        EL657
00125  01  MISC-WORK-AREAS.                                             EL657
00126      12  WS-CONTROL-PRIMARY.                                      EL657
00127        16  WS-COMPANY-CD             PIC X.                       EL657
00128        16  WS-CARRIER                PIC X.                       EL657
00129        16  WS-GROUPING               PIC X(6).                    EL657
00130        16  WS-STATE                  PIC XX.                      EL657
00131        16  WS-ACCOUNT                PIC X(10).                   EL657
00132        16  WS-CERT-EFF-DT            PIC XX.                      EL657
00133        16  WS-CERT-NO                PIC X(11).                   EL657
00134                                                                      CL**5
00135      12  WS-COMPANY-CODE             PIC S9(4)   VALUE +0   COMP. EL657
00136      12  WS-COMPANY-CODE-R      REDEFINES WS-COMPANY-CODE.        EL657
00137          16  FILLER                  PIC X.                       EL657
00138          16  WS-COMP-CD              PIC X.                       EL657
00139                                                                      CL**5
00140      12  TEXT-AREA                   PIC X(66).                   EL657
00141      12  TEXT-LENGTH                 PIC S9(4)   VALUE +66  COMP. EL657
00142                                                                      CL**5
00143  01  TEXT-MESSAGES.                                               EL657
00144      12  TRAN-COMPLETE-MSG.                                       EL657
00145          16  FILLER                  PIC X(45)                    EL657
00146            VALUE '     CLEAR ENTERED - SESSION ENDED'.            EL657
00147      EJECT                                                        EL657
00148                                      COPY ELCDATE.                   CL**8
00149      EJECT                                                        EL657
00150                                      COPY ELCATTR.                   CL**8
00151      EJECT                                                        EL657
00152                                      COPY ELCEMIB.                   CL**8
00153      EJECT                                                        EL657
00154                                      COPY ELCAID.                    CL**8
00155                                                                      CL**2
00156  01  FILLER REDEFINES DFHAID.                                        CL**5
00157      05  FILLER                      PIC X(8).                       CL**2
00158      05  PF-VALUES                   PIC X  OCCURS 24.               CL**5
00159                                                                      CL**2
00160      EJECT                                                        EL657
00161                                      COPY ELCLOGOF.                  CL**8
00162      EJECT                                                        EL657
00163                                      COPY EL657S.                    CL**8
00164      EJECT                                                        EL657
00165                                      COPY ELCINTF.                   CL**8
00166      12  PI-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.             EL657
00167          16  FILLER                  PIC X(4).                    EL657
00168          16  PI-TBCOD                PIC XXX.                     EL657
00169          16  FILLER                  PIC XXX.                     EL657
00170          16  PI-SAVE-INCR1           PIC S999    COMP-3.             CL**4
00171          16  PI-FIRST-SW             PIC X.                       EL657
00172          16  FILLER                  PIC X(97).                   EL657
00173          16  PI-SAVE-SW              PIC X.                       EL657
00174          16  PI-SAVE-TBCOD           PIC XXX.                     EL657
00175          16  PI-SAVE-EFFDT           PIC X(6).                    EL657
00176          16  PI-SAVE-AGE             PIC 99.                      EL657
00177          16  PI-SAVE-TERM            PIC 999.                     EL657
00178          16  PI-SAVE-LFTYP           PIC XX.                      EL657
00179          16  PI-SAVE-LFPRM           PIC 9(7)V99.                 EL657
00180          16  PI-SAVE-LFBEN           PIC 9(9)V99.                 EL657
00181          16  PI-SAVE-AHTYP           PIC XX.                      EL657
00182          16  PI-SAVE-AHPRM           PIC 9(7)V99.                 EL657
00183          16  PI-SAVE-AHBEN           PIC 9(7)V99.                 EL657
00184          16  PI-SAVE-IGCOD           PIC X.                       EL657
00185          16  PI-SAVE-CARR            PIC X.                       EL657
00186          16  PI-SAVE-GRP             PIC X(6).                    EL657
00187          16  PI-SAVE-STATE           PIC XX.                      EL657
00188          16  PI-SAVE-ACCT            PIC X(10).                   EL657
00189          16  PI-SAVE-CERT            PIC X(11).                   EL657
00190          16  PI-SAVE-CEFDT           PIC X(6).                    EL657
00191          16  FILLER                  PIC X(436).                     CL**9
00192                                                                   EL657
00193  LINKAGE SECTION.                                                 EL657
00194  01  DFHCOMMAREA                     PIC X(1024).                 EL657
00195 *01 PARMLIST    COMP.                                                CL**9
00196 *    02  FILLER                      PIC S9(8).                      CL**9
00197 *    02  ELCERT-POINTER              PIC S9(8).                      CL**9
00198                                                                      CL**5
00199                                      COPY ELCCERT.                   CL**8
00200      EJECT                                                        EL657
00201  PROCEDURE DIVISION.                                              EL657
00202                                                                   EL657
00203      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL657
00204      MOVE '5'                    TO  DC-OPTION-CODE.              EL657
00205      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL657
00206      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL657
00207      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL657
00208                                                                   EL657
00209      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL657
00210                                                                   EL657
00211      MOVE EIBTRMID               TO  QID-TERM.                    EL657
00212                                                                   EL657
00213      IF EIBCALEN = 0                                              EL657
00214          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL657
00215                                                                      CL**4
00216      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL657
00217          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL657
00218              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6     EL657
00219              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5     EL657
00220              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4     EL657
00221              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3     EL657
00222              MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2     EL657
00223              MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1     EL657
00224              MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM   EL657
00225              MOVE THIS-PGM             TO  PI-CALLING-PROGRAM.    EL657
00226                                                                   EL657
00227      IF EIBTRNID  = TRANS-ID                                      EL657
00228         GO TO SAME-TRAN.                                          EL657
00229                                                                   EL657
00230      MOVE LOW-VALUES             TO  EL657AI.                     EL657
00231                                                                   EL657
00232      IF PI-TBCOD NOT = PI-SAVE-TBCOD                              EL657
00233          MOVE PI-TBCOD           TO  TBCODO                       EL657
00234          MOVE AL-UANON           TO  TBCODA                       EL657
00235          GO TO 8100-SEND-INITIAL-MAP.                             EL657
00236                                                                   EL657
00237      IF PI-SAVE-SW = 'X'                                          EL657
00238          MOVE PI-SAVE-TBCOD      TO  TBCODO                       EL657
00239          MOVE PI-SAVE-EFFDT      TO  EFFDTO                       EL657
00240          MOVE PI-SAVE-AGE        TO  ISSAGEO                      EL657
00241          MOVE PI-SAVE-TERM       TO  ORIGTRMO                     EL657
00242          MOVE PI-SAVE-LFTYP      TO  LFTYPO                       EL657
00243          MOVE PI-SAVE-LFPRM      TO  LFPRMO                       EL657
00244          MOVE PI-SAVE-LFBEN      TO  LFBENO                       EL657
00245          MOVE PI-SAVE-AHTYP      TO  AHTYPO                       EL657
00246          MOVE PI-SAVE-AHPRM      TO  AHPRMO                       EL657
00247          MOVE PI-SAVE-AHBEN      TO  AHBENO                       EL657
00248          MOVE PI-SAVE-IGCOD      TO  IGCODEO                      EL657
00249          MOVE PI-SAVE-CARR       TO  CARRO                        EL657
00250          MOVE PI-SAVE-GRP        TO  GROUPO                       EL657
00251          MOVE PI-SAVE-STATE      TO  STATEO                       EL657
00252          MOVE PI-SAVE-ACCT       TO  ACCTO                        EL657
00253          MOVE PI-SAVE-CERT       TO  CERTO                        EL657
00254          MOVE PI-SAVE-CEFDT      TO  CEFDTO                       EL657
00255          MOVE AL-UNNON           TO  ISSAGEA ORIGTRMA             EL657
00256                                      LFPRMA  AHPRMA   EFFDTA      EL657
00257                                      LFBENA  AHBENA               EL657
00258          MOVE AL-UANON           TO  TBCODA  LFTYPA               EL657
00259                                      AHTYPA  IGCODEA.             EL657
00260                                                                   EL657
00261      GO TO 8100-SEND-INITIAL-MAP.                                 EL657
00262                                                                   EL657
00263  SAME-TRAN.                                                       EL657
00264      IF EIBAID = DFHCLEAR                                         EL657
00265          GO TO 9400-CLEAR.                                        EL657
00266                                                                   EL657
00267      EXEC CICS HANDLE CONDITION                                   EL657
00268          MAPFAIL (8100-SEND-INITIAL-MAP)                          EL657
00269      END-EXEC.                                                    EL657
00270                                                                   EL657
00271      EXEC CICS RECEIVE                                            EL657
00272          MAP    (MAP-NAME)                                        EL657
00273          MAPSET (MAPSET-NAME)                                     EL657
00274          INTO   (EL657AI)                                         EL657
00275      END-EXEC.                                                    EL657
00276                                                                   EL657
00277      EJECT                                                        EL657
00278                                                                      CL**2
00279      IF PFKEYL = 0                                                   CL**2
00280          GO TO 0200-CHECK-PFKEYS.                                    CL**2
00281                                                                      CL**2
00282      IF EIBAID NOT = DFHENTER                                        CL**2
00283          MOVE ER-0008            TO  EMI-ERROR                       CL**2
00284          MOVE -1                 TO  PFKEYL                          CL**2
00285          GO TO 0320-INPUT-ERROR.                                     CL**2
00286                                                                      CL**2
00287      IF PFKEYI GREATER 0 AND LESS 25                                 CL**2
00288              MOVE PF-VALUES (PFKEYI)    TO  EIBAID                   CL**2
00289      ELSE                                                            CL**2
00290          MOVE ER-0029                    TO  EMI-ERROR               CL**2
00291          GO TO 0320-INPUT-ERROR.                                     CL**2
00292                                                                      CL**2
00293  0200-CHECK-PFKEYS.                                                  CL**2
00294      IF EIBAID = DFHPF12                                             CL**2
00295          MOVE LINK-010           TO  PGM-NAME                        CL**2
00296          GO TO 9300-XCTL.                                            CL**2
00297                                                                      CL**2
00298      IF EIBAID = DFHPF23                                             CL**2
00299          GO TO 9000-RETURN-CICS.                                     CL**2
00300                                                                      CL**2
00301      IF EIBAID = DFHPF24                                             CL**2
00302          MOVE LINK-126           TO  PGM-NAME                        CL**2
00303          GO TO 9300-XCTL.                                            CL**2
00304                                                                   EL657
00305      IF (EIBAID = DFHPF1 OR DFHPF2)  AND                          EL657
00306         (PI-FIRST-SW NOT = 'X')                                   EL657
00307          GO TO 0320-INPUT-ERROR                                   EL657
00308        ELSE                                                       EL657
00309          PERFORM 7200-RECOVER-TEMP-STORAGE THRU 7200-EXIT.        EL657
00310                                                                   EL657
00311      IF EIBAID = DFHPF1                                           EL657
00312          GO TO 6000-PAGE-FORWARD.                                 EL657
00313      IF EIBAID = DFHPF2                                           EL657
00314          GO TO 7000-PAGE-BACKWARD.                                EL657
00315                                                                   EL657
00316      IF EIBAID = DFHENTER                                         EL657
00317          GO TO 1000-EDIT-INPUT.                                   EL657
00318                                                                   EL657
00319  0320-INPUT-ERROR.                                                EL657
00320      MOVE ER-0029                TO  EMI-ERROR.                   EL657
00321      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL657
00322      MOVE -1                     TO  TBCODL.                      EL657
00323      GO TO 8200-SEND-DATAONLY.                                    EL657
00324      EJECT                                                        EL657
00325                                                                   EL657
00326  1000-EDIT-INPUT.                                                 EL657
00327      IF CARRL NOT = ZEROS                                         EL657
00328          GO TO 2000-EDIT-CERT.                                    EL657
00329                                                                   EL657
00330 *******************************TABLE CODE                         EL657
00331      IF TBCODL NOT = ZEROS                                        EL657
00332          MOVE TBCODI             TO  CP-TBCOD PI-SAVE-TBCOD       EL657
00333                                               PI-TBCOD            EL657
00334          MOVE AL-UANON           TO  TBCODA                       EL657
00335      ELSE                                                         EL657
00336          MOVE ER-2341            TO  EMI-ERROR                    EL657
00337          MOVE -1                 TO  TBCODL                       EL657
00338          MOVE AL-UABON           TO  TBCODA                       EL657
00339          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL657
00340                                                                   EL657
00341 *****************************EFFECTIVE DATE                       EL657
00342      IF EFFDTL NOT = ZEROS                                        EL657
00343          IF EFFDTI NUMERIC                                        EL657
00344              MOVE EFFDTI         TO  DC-GREG-DATE-1-MDY           EL657
00345                                      PI-SAVE-EFFDT                EL657
00346              MOVE 4              TO  DC-OPTION-CODE               EL657
00347              PERFORM 8500-DATE-CONVERT THRU 8500-EXIT             EL657
00348              IF NO-CONVERSION-ERROR                               EL657
00349                  MOVE DC-BIN-DATE-1  TO  CP-EFF-DT                EL657
00350                  MOVE AL-UNNON   TO  EFFDTA                       EL657
00351              ELSE                                                 EL657
00352                  MOVE ER-2474    TO  EMI-ERROR                    EL657
00353                  MOVE -1         TO  EFFDTL                       EL657
00354                  MOVE AL-UNBON   TO  EFFDTA                       EL657
00355                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL657
00356          ELSE                                                     EL657
00357              MOVE ER-2482        TO  EMI-ERROR                    EL657
00358              MOVE -1             TO  EFFDTL                       EL657
00359              MOVE AL-UNBON       TO  EFFDTA                       EL657
00360              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL657
00361      ELSE                                                         EL657
00362          MOVE ER-2473            TO  EMI-ERROR                    EL657
00363          MOVE -1                 TO  EFFDTL                       EL657
00364          MOVE AL-UNBON           TO  EFFDTA                       EL657
00365          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL657
00366                                                                   EL657
00367 *****************************ISSUE AGE                            EL657
00368      IF ISSAGEL NOT = ZEROS                                       EL657
00369          IF ISSAGEI NUMERIC                                       EL657
00370              MOVE ISSAGEI        TO  CP-ISSUE-AGE PI-SAVE-AGE     EL657
00371                                      ISSAGEO                      EL657
00372              MOVE AL-UNNON       TO  ISSAGEA                      EL657
00373          ELSE                                                     EL657
00374              MOVE ER-2505        TO  EMI-ERROR                    EL657
00375              MOVE -1             TO  ISSAGEL                      EL657
00376              MOVE AL-UNBON       TO  ISSAGEA                      EL657
00377              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL657
00378        ELSE                                                       EL657
00379           MOVE ZEROS             TO  CP-ISSUE-AGE PI-SAVE-AGE.    EL657
00380                                                                   EL657
00381 *******************************ORIGINAL TERM                      EL657
00382      EXEC CICS BIF DEEDIT                                            CL**5
00383          FIELD  (ORIGTRMI)                                           CL**5
00384          LENGTH (3)                                                  CL**5
00385      END-EXEC.                                                       CL**5
00386                                                                   EL657
00387      IF ORIGTRML NOT = ZEROS                                      EL657
00388          IF ORIGTRMI NUMERIC                                      EL657
00389              MOVE ORIGTRMI       TO  CP-ORIGINAL-TERM             EL657
00390                                      PI-SAVE-TERM                 EL657
00391                                      ORIGTRMO                     EL657
00392              MOVE AL-UANON       TO  ORIGTRMA                     EL657
00393          ELSE                                                     EL657
00394              MOVE ER-2504        TO  EMI-ERROR                    EL657
00395              MOVE -1             TO  ORIGTRML                     EL657
00396              MOVE AL-UABON       TO  ORIGTRMA                     EL657
00397              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL657
00398       ELSE                                                        EL657
00399           MOVE ZEROS             TO  CP-ORIGINAL-TERM             EL657
00400                                      ORIGTRMO                     EL657
00401                                      PI-SAVE-TERM.                EL657
00402                                                                   EL657
00403 *******************************LF TYPE                            EL657
00404      IF LFTYPL NOT = ZEROS                                        EL657
00405          MOVE LFTYPI             TO  CP-LF-TYPE  PI-SAVE-LFTYP    EL657
00406          MOVE AL-UANON           TO  LFTYPA                       EL657
00407       ELSE                                                        EL657
00408         MOVE ZEROS               TO  CP-LF-TYPE PI-SAVE-LFTYP.    EL657
00409                                                                   EL657
00410 ******************************LF PREMIUM                          EL657
00411      EXEC CICS BIF DEEDIT                                            CL**5
00412          FIELD  (LFPRMI)                                             CL**5
00413          LENGTH (11)                                                 CL**5
00414      END-EXEC.                                                       CL**5
00415                                                                   EL657
00416        IF LFPRML NOT = ZEROS                                      EL657
00417          IF LFPRMI NUMERIC                                        EL657
00418              MOVE LFPRMI         TO  CP-LF-PREM  PI-SAVE-LFPRM    EL657
040208             MOVE PI-SAVE-LFPRM  TO  LFPRMO
00420              MOVE AL-UNNON       TO  LFPRMA                       EL657
00421          ELSE                                                     EL657
00422              MOVE ER-2484        TO  EMI-ERROR                    EL657
00423              MOVE -1             TO  LFPRML                       EL657
00424              MOVE AL-UNBON       TO  LFPRMA                       EL657
00425              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL657
00426        ELSE                                                       EL657
00427           MOVE ZEROS             TO  CP-LF-PREM PI-SAVE-LFPRM     EL657
00428                                      LFPRMO.                      EL657
00429                                                                   EL657
00430 ******************************LF BENEFIT                          EL657
00431      EXEC CICS BIF DEEDIT                                            CL**5
00432          FIELD  (LFBENI)                                             CL**5
00433          LENGTH (14)                                                 CL**5
00434      END-EXEC.                                                       CL**5
00435                                                                   EL657
00436      IF LFBENL NOT = ZEROS                                           CL**5
00437          IF LFBENI NUMERIC                                        EL657
00438              MOVE LFBENI         TO  CP-LF-BEN PI-SAVE-LFBEN      EL657
040208             MOVE PI-SAVE-LFBEN  TO  LFBENO
00440              MOVE AL-UNNON       TO  LFBENA                       EL657
00441          ELSE                                                     EL657
00442              MOVE ER-2485        TO  EMI-ERROR                    EL657
00443              MOVE -1             TO  LFBENL                       EL657
00444              MOVE AL-UNBON       TO  LFBENA                       EL657
00445              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL657
00446        ELSE                                                       EL657
00447           MOVE ZEROS             TO  CP-LF-BEN PI-SAVE-LFBEN      EL657
00448                                      LFBENO.                      EL657
00449                                                                   EL657
00450 *******************************AH TYPE                            EL657
00451      IF AHTYPL NOT = ZEROS                                        EL657
00452          MOVE AHTYPI             TO  CP-AH-TYPE  PI-SAVE-AHTYP    EL657
00453          MOVE AL-UANON           TO  AHTYPA                       EL657
00454       ELSE                                                        EL657
00455        MOVE ZERO                 TO  CP-AH-TYPE PI-SAVE-AHTYP.    EL657
00456                                                                   EL657
00457 ******************************AH PREMIUM                          EL657
00458      EXEC CICS BIF DEEDIT                                            CL**5
00459          FIELD  (AHPRMI)                                             CL**5
00460          LENGTH (11)                                                 CL**5
00461      END-EXEC.                                                       CL**5
00462                                                                   EL657
00463      IF AHPRML NOT = ZEROS                                           CL**5
00464          IF AHPRMI NUMERIC                                        EL657
00465              MOVE AHPRMI         TO  CP-AH-PREM  PI-SAVE-AHPRM    EL657
040208             MOVE PI-SAVE-AHPRM  TO  AHPRMO
00467              MOVE AL-UNNON       TO  AHPRMA                       EL657
00468          ELSE                                                     EL657
00469              MOVE ER-2484        TO  EMI-ERROR                    EL657
00470              MOVE -1             TO  AHPRML                       EL657
00471              MOVE AL-UNBON       TO  AHPRMA                       EL657
00472              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL657
00473        ELSE                                                       EL657
00474            MOVE ZEROS            TO  CP-AH-PREM PI-SAVE-AHPRM     EL657
00475                                      AHPRMO.                      EL657
00476                                                                   EL657
00477 ******************************AH BENEFIT                          EL657
00478      EXEC CICS BIF DEEDIT                                            CL**5
00479          FIELD  (AHBENI)                                             CL**5
00480          LENGTH (11)                                                 CL**5
00481      END-EXEC.                                                       CL**6
00482                                                                   EL657
00483      IF AHBENL NOT = ZEROS                                           CL**5
00484          IF AHBENI NUMERIC                                        EL657
00485              MOVE AHBENI         TO  CP-AH-BEN  PI-SAVE-AHBEN     EL657
040208             MOVE PI-SAVE-AHBEN  TO  AHBENO
00487              MOVE AL-UNNON       TO  AHBENA                       EL657
00488          ELSE                                                     EL657
00489              MOVE ER-2485        TO  EMI-ERROR                    EL657
00490              MOVE -1             TO  AHBENL                       EL657
00491              MOVE AL-UNBON       TO  AHBENA                       EL657
00492              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL657
00493        ELSE                                                       EL657
00494            MOVE ZEROS            TO  CP-AH-BEN PI-SAVE-AHBEN      EL657
00495                                      AHBENO.                      EL657
00496                                                                   EL657
00497 *******************************I/G                                EL657
00498      IF IGCODEI = 'I' OR 'G'                                         CL**5
00499          MOVE IGCODEI            TO  CP-IG-CODE  PI-SAVE-IGCOD    EL657
00500          MOVE AL-UANON           TO  IGCODEA                      EL657
00501      ELSE                                                         EL657
00502          MOVE SPACE              TO  CP-IG-CODE  PI-SAVE-IGCOD    EL657
00503                                      IGCODEO                      EL657
00504          MOVE AL-UANON           TO  IGCODEA.                     EL657
00505                                                                   EL657
00506      GO TO 3000-PERFORM-CALC-ROUTINE.                             EL657
00507                                                                   EL657
00508  2000-EDIT-CERT.                                                  EL657
00509 *******************************CARRIER                            EL657
00510      IF CARRI NOT = SPACES                                        EL657
00511          MOVE CARRI              TO  WS-CARRIER PI-SAVE-CARR      EL657
00512          MOVE AL-UANON           TO  CARRA                        EL657
00513      ELSE                                                         EL657
00514          MOVE ZEROS              TO  CARRL                        EL657
00515          MOVE SPACES             TO  GROUPI STATEI CERTI          EL657
00516                                      ACCTI CEFDTI                 EL657
00517          GO TO 1000-EDIT-INPUT.                                   EL657
00518                                                                   EL657
00519 *******************************GROUPING                           EL657
00520      IF GROUPI NOT = SPACES                                       EL657
00521          MOVE GROUPI             TO  WS-GROUPING  PI-SAVE-GRP     EL657
00522          MOVE AL-UANON           TO  GROUPA                       EL657
00523      ELSE                                                         EL657
00524          MOVE ER-0195            TO  EMI-ERROR                    EL657
00525          MOVE -1                 TO  GROUPL                       EL657
00526          MOVE AL-UABON           TO  GROUPA                       EL657
00527          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL657
00528                                                                   EL657
00529 *******************************STATE                              EL657
00530      IF STATEI NOT = SPACES                                       EL657
00531          MOVE STATEI             TO  WS-STATE  PI-SAVE-STATE      EL657
00532          MOVE AL-UANON           TO  STATEA                       EL657
00533      ELSE                                                         EL657
00534          MOVE ER-0196            TO  EMI-ERROR                    EL657
00535          MOVE -1                 TO  STATEL                       EL657
00536          MOVE AL-UABON           TO  STATEA                       EL657
00537          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL657
00538                                                                   EL657
00539 *******************************ACCOUNT                            EL657
00540      IF ACCTI NOT = SPACES                                        EL657
00541          MOVE ACCTI              TO  WS-ACCOUNT PI-SAVE-ACCT      EL657
00542          MOVE AL-UANON           TO  ACCTA                        EL657
00543      ELSE                                                         EL657
00544          MOVE ER-0197            TO  EMI-ERROR                    EL657
00545          MOVE -1                 TO  ACCTL                        EL657
00546          MOVE AL-UABON           TO  ACCTA                        EL657
00547          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL657
00548                                                                   EL657
00549 *******************************CERT                               EL657
00550      IF CERTI NOT = SPACES                                        EL657
00551          MOVE CERTI              TO  WS-CERT-NO  PI-SAVE-CERT     EL657
00552          MOVE AL-UANON           TO  CERTA                        EL657
00553      ELSE                                                         EL657
00554          MOVE ER-0203            TO  EMI-ERROR                    EL657
00555          MOVE -1                 TO  CERTL                        EL657
00556          MOVE AL-UABON           TO  CERTA                        EL657
00557          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL657
00558                                                                   EL657
00559 *****************************EFFECTIVE DATE                       EL657
00560      IF CEFDTL NOT = ZEROS                                        EL657
00561          IF CEFDTI NUMERIC                                        EL657
00562              MOVE CEFDTI         TO  DC-GREG-DATE-1-MDY           EL657
00563                                      PI-SAVE-CEFDT                EL657
00564              MOVE 4              TO  DC-OPTION-CODE               EL657
00565              PERFORM 8500-DATE-CONVERT THRU 8500-EXIT             EL657
00566              IF NO-CONVERSION-ERROR                               EL657
00567                  MOVE DC-BIN-DATE-1  TO  WS-CERT-EFF-DT           EL657
00568                  MOVE AL-UNNON   TO  CEFDTA                       EL657
00569              ELSE                                                 EL657
00570                  MOVE ER-2474    TO  EMI-ERROR                    EL657
00571                  MOVE -1         TO  CEFDTL                       EL657
00572                  MOVE AL-UNBON   TO  CEFDTA                       EL657
00573                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL657
00574          ELSE                                                     EL657
00575              MOVE ER-2482        TO  EMI-ERROR                    EL657
00576              MOVE -1             TO  CEFDTL                       EL657
00577              MOVE AL-UNBON       TO  CEFDTA                       EL657
00578              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL657
00579      ELSE                                                         EL657
00580          MOVE ER-2473            TO  EMI-ERROR                    EL657
00581          MOVE -1                 TO  CEFDTL                       EL657
00582          MOVE AL-UNBON           TO  CEFDTA                       EL657
00583          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL657
00584                                                                   EL657
00585      MOVE PI-COMPANY-CD          TO  WS-COMPANY-CD.               EL657
00586                                                                   EL657
00587      EXEC CICS HANDLE CONDITION                                   EL657
00588          NOTFND (2500-CERT-NOT-FOUND)                             EL657
00589      END-EXEC.                                                    EL657
00590                                                                   EL657
00591      EXEC CICS READ                                               EL657
00592           SET      (ADDRESS OF CERTIFICATE-MASTER)                   CL**9
00593           DATASET  ('ELCERT')                                     EL657
00594           RIDFLD   (WS-CONTROL-PRIMARY)                           EL657
00595      END-EXEC.                                                    EL657
00596                                                                   EL657
00597      IF TBCODL = ZERO  OR                                         EL657
00598         TBCODI = SPACES                                           EL657
00599         MOVE CM-REIN-TABLE       TO  CP-TBCOD PI-SAVE-TBCOD       EL657
00600                                      PI-TBCOD TBCODO              EL657
00601        ELSE                                                       EL657
00602          MOVE TBCODI             TO  CP-TBCOD PI-SAVE-TBCOD       EL657
00603                                      PI-TBCOD                     EL657
00604          MOVE AL-UANON           TO  TBCODA.                      EL657
00605                                                                   EL657
00606      MOVE CM-CERT-EFF-DT         TO  CP-EFF-DT.                   EL657
00607      MOVE CP-EFF-DT              TO  DC-BIN-DATE-1.               EL657
00608      MOVE SPACES                 TO  DC-OPTION-CODE.              EL657
00609      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL657
00610      MOVE DC-GREG-DATE-1-MDY     TO  EFFDTO PI-SAVE-CEFDT         EL657
00611                                      CEFDTO PI-SAVE-EFFDT.        EL657
00612                                                                   EL657
00613      MOVE CM-INSURED-ISSUE-AGE   TO  CP-ISSUE-AGE ISSAGEO         EL657
00614                                      PI-SAVE-AGE.                 EL657
00615      IF CM-LF-BENEFIT-CD NOT = ZEROS  AND  SPACES                    CL**8
00616          MOVE CM-LF-ORIG-TERM    TO  CP-ORIGINAL-TERM ORIGTRMO       CL**8
00617                                      PI-SAVE-TERM                    CL**8
00618      ELSE                                                            CL**8
00619          MOVE CM-AH-ORIG-TERM    TO  CP-ORIGINAL-TERM ORIGTRMO       CL**8
00620                                      PI-SAVE-TERM.                EL657
00621      MOVE CM-LF-BENEFIT-CD       TO  CP-LF-TYPE    LFTYPO         EL657
00622                                      PI-SAVE-LFTYP.               EL657
00623      MOVE CM-AH-BENEFIT-CD       TO  CP-AH-TYPE    AHTYPO         EL657
00624                                      PI-SAVE-AHTYP.               EL657
00625                                                                      CL**4
00626      ADD CM-LF-ALT-PREMIUM-AMT                                       CL**4
00627          CM-LF-PREMIUM-AMT       GIVING WS-LF-PREMIUM-AMT.           CL**4
00628                                                                      CL**4
00629      MOVE WS-LF-PREMIUM-AMT      TO  CP-LF-PREM    LFPRMO            CL**4
00630                                      PI-SAVE-LFPRM.               EL657
00631      MOVE CM-LF-ALT-PREMIUM-AMT  TO  CP-LFPRM-ALT.                   CL**3
00632                                                                      CL**3
00633      MOVE CM-AH-PREMIUM-AMT      TO  CP-AH-PREM    AHPRMO         EL657
00634                                      PI-SAVE-AHPRM.               EL657
00635      ADD CM-LF-ALT-BENEFIT-AMT                                       CL**4
00636          CM-LF-BENEFIT-AMT       GIVING WS-LF-BENEFIT-AMT.           CL**4
00637                                                                      CL**4
00638      MOVE WS-LF-BENEFIT-AMT      TO  CP-LF-BEN     LFBENO            CL**4
00639                                      PI-SAVE-LFBEN.               EL657
00640      MOVE CM-LF-ALT-BENEFIT-AMT  TO  CP-LFAMT-ALT.                   CL**3
00641      MOVE CM-AH-BENEFIT-AMT      TO  CP-AH-BEN     AHBENO         EL657
00642                                      PI-SAVE-AHBEN.               EL657
00643      MOVE CM-IND-GRP-TYPE        TO  CP-IG-CODE    IGCODEO        EL657
00644                                      PI-SAVE-IGCOD.               EL657
00645                                                                   EL657
00646      MOVE AL-UNNON               TO  ISSAGEA  ORIGTRMA  LFPRMA    EL657
00647                                      AHPRMA   EFFDTA    LFBENA    EL657
00648                                      AHBENA.                      EL657
00649      MOVE AL-UANON               TO  TBCODA   LFTYPA    AHTYPA    EL657
00650                                      IGCODEA.                     EL657
00651                                                                      CL**5
00652      GO TO 3000-PERFORM-CALC-ROUTINE.                             EL657
00653                                                                   EL657
00654  2500-CERT-NOT-FOUND.                                             EL657
00655      MOVE ER-0244                TO  EMI-ERROR                    EL657
00656      MOVE -1                     TO  CARRL.                       EL657
00657      MOVE AL-UABON               TO  CARRA.                       EL657
00658      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL657
00659      GO TO 8200-SEND-DATAONLY.                                    EL657
00660                                                                   EL657
00661  3000-PERFORM-CALC-ROUTINE.                                       EL657
00662      IF EMI-ERROR NOT = ZEROS                                     EL657
00663          GO TO 8200-SEND-DATAONLY.                                EL657
00664                                                                   EL657
00665      MOVE 'X'                    TO  PI-SAVE-SW PI-FIRST-SW.      EL657
00666      MOVE PI-COMPANY-CD          TO  CP-COMPANY-CD.               EL657
00667      MOVE PI-COMPANY-ID          TO  CP-COMPANY-ID.               EL657
00668      MOVE ZERO                   TO  CP-RETURN-CODE.              EL657
00669      MOVE LINK-ELREIN            TO  PGM-NAME.                    EL657
00670                                                                   EL657
00671      PERFORM 9700-LINK THRU 9700-EXIT.                            EL657
00672      PERFORM 7100-CREATE-TEMP-STORAGE THRU 7100-EXIT.             EL657
00673                                                                   EL657
00674  4000-DISPLAY-RESULTS.                                            EL657
00675      IF CP-RETURN-CODE NOT = 0                                    EL657
00676          GO TO 4000-ERROR.                                        EL657
00677                                                                   EL657
00678      MOVE 1                      TO  INCR1 PI-SAVE-INCR1.         EL657
00679                                                                   EL657
00680      PERFORM 5000-BLD-LINE THRU 5000-XIT.                         EL657
00681                                                                   EL657
00682      IF CARRL NOT = ZEROS                                         EL657
00683          MOVE -1                 TO  CARRL                        EL657
00684      ELSE                                                         EL657
00685          MOVE -1                 TO  TBCODL.                      EL657
00686                                                                   EL657
00687      GO TO 8200-SEND-DATAONLY.                                    EL657
00688                                                                   EL657
00689  4000-ERROR.                                                      EL657
00690      IF CP-RETURN-CODE = 1                                        EL657
00691          MOVE ER-2425            TO  EMI-ERROR                    EL657
00692          MOVE -1                 TO  LFTYPL                       EL657
00693          MOVE AL-UABON           TO  LFTYPA                       EL657
00694          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL657
00695      IF CP-RETURN-CODE = 2                                        EL657
00696          MOVE ER-2429            TO  EMI-ERROR                    EL657
00697          MOVE -1                 TO  AHTYPL                       EL657
00698          MOVE AL-UABON           TO  AHTYPA                       EL657
00699          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL657
00700      IF CP-RETURN-CODE = 3                                        EL657
00701          MOVE ER-2615            TO  EMI-ERROR                    EL657
00702          MOVE -1                 TO  TBCODL                       EL657
00703          MOVE AL-UABON           TO  TBCODA                       EL657
00704          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL657
00705                                                                   EL657
00706      GO TO 8200-SEND-DATAONLY.                                    EL657
00707                                                                   EL657
00708  5000-BLD-LINE.                                                   EL657
00709      MOVE 1                      TO  INCR2.                       EL657
00710      MOVE SPACES       TO  REPLINEO (1) REPLINEO (2) REPLINEO (3) EL657
00711                            REPLINEO (4) REPLINEO (5) REPLINEO (6) EL657
00712                            REPLINEO (7) REPLINEO (8) REPLINEO (9) EL657
00713                            REPLINEO (10).                         EL657
00714                                                                   EL657
00715  5000-BLD-LINE-LOOP.                                              EL657
00716      IF REIN-COMP (INCR1) = SPACES                                EL657
00717          GO TO 5000-XIT.                                             CL**5
00718                                                                      CL**5
00719      MOVE SPACES                     TO  BLD-LINE.                   CL*10
00720      MOVE REIN-CO-PRIME (INCR1)      TO  BL-CEDED.                EL657
00721      MOVE REIN-CO-SUB   (INCR1)      TO  BL-CEDED-SUB.            EL657
00722      MOVE REIN-LFPRM    (INCR1)      TO  BL-LF-PREMIUM.              CL**5
00723      MOVE REIN-AHPRM    (INCR1)      TO  BL-AH-PREMIUM.              CL**5
00724      MOVE REIN-LFAMT    (INCR1)      TO  BL-LF-BENEFIT.              CL**5
00725      MOVE REIN-AHAMT    (INCR1)      TO  BL-AH-BENEFIT.              CL**5
00726                                                                   EL657
00727      MOVE BLD-LINE               TO  REPLINEO (INCR2).            EL657
00728      ADD  1                      TO  INCR1 INCR2  PI-SAVE-INCR1.  EL657
00729      IF INCR2 = 11                                                EL657
00730           GO TO 5000-XIT.                                         EL657
00731                                                                   EL657
00732      GO TO 5000-BLD-LINE-LOOP.                                    EL657
00733                                                                   EL657
00734  5000-XIT.                                                        EL657
00735      EXIT.                                                        EL657
00736                                                                   EL657
00737  6000-PAGE-FORWARD.                                               EL657
00738      MOVE PI-SAVE-INCR1          TO  INCR1.                       EL657
00739      IF REIN-COMP (INCR1) = SPACES                                EL657
00740          MOVE -1                 TO  TBCODL                       EL657
00741          MOVE ER-0130            TO  EMI-ERROR                    EL657
00742          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL657
00743          GO TO 8200-SEND-DATAONLY.                                EL657
00744                                                                   EL657
00745 *    MOVE 11                     TO  INCR1 PI-SAVE-INCR1.            CL**5
00746      PERFORM 5000-BLD-LINE THRU 5000-XIT.                         EL657
00747      MOVE -1                     TO  TBCODL.                      EL657
00748      GO TO 8200-SEND-DATAONLY.                                    EL657
00749                                                                   EL657
00750  7000-PAGE-BACKWARD.                                              EL657
00751      MOVE PI-SAVE-INCR1          TO  INCR1.                       EL657
00752      IF INCR1 LESS 12                                             EL657
00753          MOVE -1                 TO  TBCODL                       EL657
00754          MOVE ER-0131            TO  EMI-ERROR                    EL657
00755          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL657
00756          GO TO 8200-SEND-DATAONLY.                                EL657
00757                                                                   EL657
00758      MOVE 1                      TO  INCR1 PI-SAVE-INCR1.         EL657
00759      PERFORM 5000-BLD-LINE THRU 5000-XIT.                         EL657
00760      MOVE -1                     TO  TBCODL.                      EL657
00761      GO TO 8200-SEND-DATAONLY.                                    EL657
00762                                                                   EL657
00763    EJECT                                                          EL657
00764  7100-CREATE-TEMP-STORAGE.                                        EL657
00765      PERFORM 7300-DELETE-TEMP-STORAGE THRU 7300-EXIT.             EL657
00766      EXEC CICS WRITEQ TS                                          EL657
00767          QUEUE   (QID)                                            EL657
00768          FROM    (REIN-HOLD-AREAS)                                EL657
00769          LENGTH  (REIN-LENGTH)                                    EL657
00770          ITEM    (ITEM-VALUE)                                     EL657
00771      END-EXEC.                                                    EL657
00772                                                                   EL657
00773  7100-EXIT.                                                       EL657
00774       EXIT.                                                       EL657
00775                                                                   EL657
00776  7200-RECOVER-TEMP-STORAGE.                                       EL657
00777      EXEC CICS HANDLE CONDITION                                   EL657
00778          QIDERR  (7200-EXIT)                                      EL657
00779      END-EXEC.                                                    EL657
00780                                                                   EL657
00781      EXEC CICS READQ TS                                           EL657
00782          QUEUE    (QID)                                           EL657
00783          INTO     (REIN-HOLD-AREAS)                               EL657
00784          LENGTH   (REIN-LENGTH)                                   EL657
00785          ITEM     (ITEM-VALUE)                                    EL657
00786      END-EXEC.                                                    EL657
00787                                                                   EL657
00788  7200-EXIT.                                                       EL657
00789       EXIT.                                                       EL657
00790                                                                   EL657
00791  7300-DELETE-TEMP-STORAGE.                                        EL657
00792      EXEC CICS HANDLE CONDITION                                   EL657
00793          QIDERR  (7300-EXIT)                                      EL657
00794      END-EXEC.                                                    EL657
00795                                                                   EL657
00796      EXEC CICS DELETEQ TS                                         EL657
00797          QUEUE  (QID)                                             EL657
00798      END-EXEC.                                                    EL657
00799                                                                   EL657
00800  7300-EXIT.                                                       EL657
00801       EXIT.                                                       EL657
00802                                                                   EL657
00803  8100-SEND-INITIAL-MAP.                                           EL657
00804      MOVE SAVE-DATE              TO  DATEO.                       EL657
00805      MOVE EIBTIME                TO  TIME-IN.                     EL657
00806      MOVE TIME-OUT               TO  TIMEO.                       EL657
00807      MOVE -1                     TO  TBCODL.                      EL657
00808      MOVE EMI-MESSAGE-AREA (1)   TO  MSGO.                        EL657
00809                                                                   EL657
00810      MOVE PI-LIFE-OVERRIDE-L2    TO  LFHDG1O                      EL657
00811                                      LFHDG2O                      EL657
00812                                      LFHDG3O                      EL657
00813                                      LFHDG4O                      EL657
00814                                      LFHDG5O.                     EL657
00815      MOVE PI-AH-OVERRIDE-L2      TO  AHHDG1O                      EL657
00816                                      AHHDG2O                      EL657
00817                                      AHHDG3O                      EL657
00818                                      AHHDG4O                      EL657
00819                                      AHHDG5O.                     EL657
00820      MOVE AL-SABON               TO  LFHDG1A  LFHDG2A  LFHDG3A    EL657
00821                                      LFHDG4A  LFHDG5A             EL657
00822                                      AHHDG1A  AHHDG2A  AHHDG3A    EL657
00823                                      AHHDG4A  AHHDG5A.            EL657
00824                                                                   EL657
00825      EXEC CICS SEND                                               EL657
00826          MAP    (MAP-NAME)                                        EL657
00827          MAPSET (MAPSET-NAME)                                     EL657
00828          FROM   (EL657AI)                                         EL657
00829          ERASE                                                    EL657
00830          CURSOR                                                   EL657
00831      END-EXEC.                                                    EL657
00832                                                                   EL657
00833      GO TO 9100-RETURN-TRAN.                                      EL657
00834                                                                   EL657
00835  8200-SEND-DATAONLY.                                              EL657
00836      MOVE SAVE-DATE              TO  DATEO.                       EL657
00837      MOVE EIBTIME                TO  TIME-IN.                     EL657
00838      MOVE TIME-OUT               TO  TIMEO.                       EL657
00839      MOVE EMI-MESSAGE-AREA (1)   TO  MSGO.                        EL657
00840                                                                   EL657
00841      MOVE PI-LIFE-OVERRIDE-L2    TO  LFHDG1O                      EL657
00842                                      LFHDG2O                      EL657
00843                                      LFHDG3O                      EL657
00844                                      LFHDG4O                      EL657
00845                                      LFHDG5O.                     EL657
00846      MOVE PI-AH-OVERRIDE-L2      TO  AHHDG1O                      EL657
00847                                      AHHDG2O                      EL657
00848                                      AHHDG3O                      EL657
00849                                      AHHDG4O                      EL657
00850                                      AHHDG5O.                     EL657
00851      MOVE AL-SABON               TO  LFHDG1A  LFHDG2A  LFHDG3A    EL657
00852                                      LFHDG4A  LFHDG5A             EL657
00853                                      AHHDG1A  AHHDG2A  AHHDG3A    EL657
00854                                      AHHDG4A  AHHDG5A.            EL657
00855                                                                   EL657
00856      EXEC CICS SEND                                               EL657
00857          MAP    (MAP-NAME)                                        EL657
00858          MAPSET (MAPSET-NAME)                                     EL657
00859          FROM   (EL657AI)                                         EL657
00860          DATAONLY                                                 EL657
00861          CURSOR                                                   EL657
00862      END-EXEC.                                                    EL657
00863                                                                   EL657
00864      GO TO 9100-RETURN-TRAN.                                      EL657
00865                                                                   EL657
00866      EJECT                                                        EL657
00867  8300-SEND-TEXT SECTION.                                          EL657
00868      EXEC CICS SEND TEXT                                          EL657
00869          FROM   (LOGOFF-TEXT)                                     EL657
00870          LENGTH (LOGOFF-LENGTH)                                   EL657
00871          ERASE                                                    EL657
00872          FREEKB                                                   EL657
00873      END-EXEC.                                                    EL657
00874                                                                   EL657
00875      EXEC CICS RETURN                                             EL657
00876      END-EXEC.                                                    EL657
00877                                                                   EL657
00878  8300-EXIT.                                                       EL657
00879      EXIT.                                                        EL657
00880                                                                   EL657
00881      EJECT                                                        EL657
00882  8500-DATE-CONVERT.                                               EL657
00883      MOVE LINK-ELDATCV           TO  PGM-NAME.                    EL657
00884      EXEC CICS LINK                                               EL657
00885          PROGRAM    (PGM-NAME)                                    EL657
00886          COMMAREA   (DATE-CONVERSION-DATA)                        EL657
00887          LENGTH     (DC-COMM-LENGTH)                              EL657
00888      END-EXEC.                                                    EL657
00889                                                                   EL657
00890  8500-EXIT.                                                       EL657
00891      EXIT.                                                        EL657
00892                                                                   EL657
00893  8800-UNAUTHORIZED-ACCESS.                                        EL657
00894      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.                  EL657
00895      GO TO 8300-SEND-TEXT.                                        EL657
00896                                                                   EL657
00897  8950-CLEAR-RETURN.                                               EL657
00898      MOVE TRAN-COMPLETE-MSG      TO  TEXT-AREA.                   EL657
00899                                                                   EL657
00900  8990-SEND-TEXT.                                                  EL657
00901      EXEC CICS SEND TEXT                                          EL657
00902          FROM    (TEXT-AREA)                                      EL657
00903          LENGTH  (TEXT-LENGTH)                                    EL657
00904          ERASE                                                    EL657
00905          FREEKB                                                   EL657
00906      END-EXEC.                                                    EL657
00907       EJECT                                                       EL657
00908                                                                   EL657
00909  9000-RETURN-CICS.                                                EL657
00910      MOVE LINK-005               TO  PGM-NAME.                       CL**2
00911      MOVE EIBAID                 TO  PI-ENTRY-CD-1.                  CL**2
00912      GO TO 9300-XCTL.                                                CL**2
00913                                                                   EL657
00914  9100-RETURN-TRAN.                                                EL657
00915      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.               CL**2
00916      MOVE '657A'                 TO  PI-CURRENT-SCREEN-NO.           CL**2
00917                                                                      CL**2
00918      EXEC CICS RETURN                                             EL657
00919          TRANSID   (TRANS-ID)                                     EL657
00920          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL657
00921          LENGTH    (PI-COMM-LENGTH)                               EL657
00922      END-EXEC.                                                    EL657
00923                                                                   EL657
00924      GOBACK.                                                         CL**2
00925                                                                   EL657
00926  9300-XCTL.                                                       EL657
00927      EXEC CICS XCTL                                               EL657
00928          PROGRAM    (PGM-NAME)                                    EL657
00929          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL657
00930          LENGTH     (PI-COMM-LENGTH)                              EL657
00931      END-EXEC.                                                    EL657
00932                                                                   EL657
00933  9400-CLEAR.                                                      EL657
00934      MOVE SPACE                  TO  PI-FIRST-SW.                 EL657
00935      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.                    EL657
00936      GO TO 9300-XCTL.                                             EL657
00937                                                                   EL657
00938  9700-LINK.                                                       EL657
00939      EXEC CICS LINK                                               EL657
00940          PROGRAM   (PGM-NAME)                                     EL657
00941          COMMAREA  (REINSURANCE-PASS-AREA)                        EL657
00942          LENGTH    (CP-COMM-LENGTH)                               EL657
00943      END-EXEC.                                                    EL657
00944                                                                   EL657
00945  9700-EXIT.                                                       EL657
00946       EXIT.                                                       EL657
00947       EJECT                                                       EL657
00948                                                                   EL657
00949  9900-ERROR-FORMAT.                                               EL657
00950      IF NOT EMI-ERRORS-COMPLETE                                   EL657
00951          MOVE LINK-001           TO  PGM-NAME                     EL657
00952          EXEC CICS LINK                                           EL657
00953              PROGRAM   (PGM-NAME)                                 EL657
00954              COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)            EL657
00955              LENGTH    (EMI-COMM-LENGTH)                          EL657
00956           END-EXEC.                                               EL657
00957                                                                   EL657
00958  9900-EXIT.                                                       EL657
00959      EXIT.                                                        EL657
00960                                                                   EL657
00961  9990-ABEND.                                                      EL657
00962      MOVE LINK-004               TO  PGM-NAME.                    EL657
00963      MOVE DFHEIBLK               TO  EMI-LINE1.                   EL657
00964      EXEC CICS LINK                                               EL657
00965          PROGRAM   (PGM-NAME)                                     EL657
00966          COMMAREA  (EMI-LINE1)                                    EL657
00967          LENGTH    (72)                                           EL657
00968      END-EXEC.                                                    EL657
00969                                                                   EL657
00970      GO TO 8200-SEND-DATAONLY.                                    EL657
