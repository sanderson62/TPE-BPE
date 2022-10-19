00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL1275.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 11/21/95 10:11:33.
00007 *                            VMOD=2.032.
00008 *
00008 *
00009 *AUTHOR.    LOGIC, INC.
00010 *           DALLAS, TEXAS.
00011
00024 *REMARKS.  TRANSACTION - EXX5
00025
00026 *        CERT COVERAGES DISPLAY PROGRAM.
00027
00028 *    SCREENS     - EL127E - CERTIFICATE PROFILE
00029
00030 *    ENTERED BY  - EL1273 - CERTIFICATE UPDATE
00031 *                - EL1274 - CERTIFICATE PROFILE
00032
00033 *    EXIT TO     - CALLING PROGRAM
00034 *                - EL132  - CLAIM LOOK-UP
00035
00036 *    INPUT FILE  - ELCERT - CERTIFICATE INFORCE FILE
00037 *                  ELCNTL - CONTROL FILE
00038
00039 *    NARRATIVE   - PROGRAM USES THE KEY TO THE CERTIFICATE MASTER
00040 *                  PASSED IN THE COMMAREA TO DISPLAY THE
00041 *                  COVERAGES ASSOCIATED WITH A CERTIFICATE AND
00042 *                  RETURNS WITH THE TRANSACTION OF THE CALLING
00043 *                  PROGRAM.
101201******************************************************************
101201*                   C H A N G E   L O G
101201*
101201* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101201*-----------------------------------------------------------------
101201*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101201* EFFECTIVE    NUMBER
101201*-----------------------------------------------------------------
101201* 101201    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
121802* 121802    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYP I
122002* 122002    2002111500006  PEMA  ADD MONTHLY PROCESSING          
101509* 101509    2008100900003  AJRA  CALL NEW CERT NOTE SCREEN
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
010816* 010816  IR2015092900001  PEMA  USE CLP STATE WHERE NEEDED
092118* 092118  CR2018073000001  PEMA  Add lf & ah refund method
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
080322* 080322  CR2021100800003  TANA  Add B and H claim types
101201******************************************************************

00044
00045      EJECT
00046  ENVIRONMENT DIVISION.
00047
00048  DATA DIVISION.
00049
00050  WORKING-STORAGE SECTION.
00051
00052  77  FILLER  PIC X(32)  VALUE '********************************'.
00053  77  FILLER  PIC X(32)  VALUE '*   EL1275 WORKING STORAGE     *'.
00054  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.032 *********'.
       77  P1                          PIC S999 COMP-3 VALUE +0.
080322 77  WS-ATT-AGE                  PIC S9(3)V99    COMP-3 VALUE +0.
080322 77  WS-EDIT-AGE                 PIC S999       COMP-3 VALUE ZERO.

00055
00056  01  WS-DATE-AREA.
00057      05  SAVE-DATE                   PIC X(8)     VALUE SPACES.
00058      05  SAVE-BIN-DATE               PIC X(2)     VALUE SPACES.
00059      05  WS-VALUATION-DT             PIC X(2)    VALUE LOW-VALUES.

092118 01  ws-elcrtt-sw                pic x  value spaces.
092118     88  good-hit-on-trlr          value 'Y'.
092118     88  crtt-not-found            value 'N'.
092118     88  crtt-not-read             value ' '.

       01  WS-RESPONSE                 PIC S9(8)       COMP.
           88  RESP-NORMAL                 VALUE +00.
           88  RESP-NOTFND                 VALUE +13.
           88  RESP-DUPKEY                 VALUE +15.
           88  RESP-NOTOPEN                VALUE +19.
           88  RESP-ENDFILE                VALUE +20.

00061  01  FILLER                          COMP-3.
00062      05  WS-ERROR-COUNT              PIC S9(3)    VALUE ZERO.
00063      05  WS-TIME-WORK                PIC S9(7)    VALUE ZERO.
00064      05  WS-TIME                     REDEFINES
00065          WS-TIME-WORK                PIC S9(3)V9(4).
00066      05  WS-NOT-FOUND                PIC S9       VALUE ZERO.
00067          88  BENEFIT-FOUND                        VALUE +1.
00068      05  ONE-MON-EARNED              PIC S9(7)V99 VALUE +0.
00069      05  WS-CALC-REFUND              PIC S9(7)V99 VALUE +0.
00070      05  WS-TERM                     PIC S9(4)    VALUE +0.
00071      05  WS-REMAINING-AMT            PIC S9(9)V99 VALUE +0.
           05  WS-PDEF-RECORD-SW       PIC X           VALUE ' '.
               88  PDEF-FOUND                          VALUE 'Y'.
00072

       01  ERPDEF-KEY-SAVE             PIC X(18).
       01  ERPDEF-KEY.
           12  ERPDEF-COMPANY-CD       PIC X.
           12  ERPDEF-STATE            PIC XX.
           12  ERPDEF-PROD-CD          PIC XXX.
           12  F                       PIC X(7).
           12  ERPDEF-BEN-TYPE         PIC X.
           12  ERPDEF-BEN-CODE         PIC XX.
           12  ERPDEF-EXP-DT           PIC XX.

00073  01  FILLER.
           05  WS-BROWSE-STARTED-SW    PIC  X(01)      VALUE SPACE.
               88  BROWSE-STARTED                      VALUE 'Y'.
           05  WS-ACCT-RECORD-SW       PIC  X(01)      VALUE SPACE.
               88  ACCT-FOUND                          VALUE 'Y'.
00074      05  WS-CALC-CD                  PIC X.
00075
00076      05  WS-BENEFIT-TYPE             PIC X        VALUE SPACE.
00077      05  WS-BENEFIT-NO               PIC XX       VALUE ZERO.
00078      05  WS-BENEFIT-DESCRIP          PIC X(10)    VALUE SPACES.
00079      05  WS-COMMENT                  PIC X(10)    VALUE SPACES.
00080      05  WS-KIND                     PIC X(3)     VALUE SPACES.
00081      05  WS-LF-COVERAGE-TYPE         PIC X        VALUE SPACE.
00082      05  WS-BENEFIT-CODE             PIC X(02)    VALUE SPACES.
00083      05  WS-EARNINGS-CALC            PIC X(01)    VALUE SPACE.
00084      05  WS-OVRD-EARNINGS-CALC       PIC X(01)    VALUE SPACE.
00085      05  WS-STATE-ABBREV             PIC X(02)    VALUE SPACES.
00086      05  WS-CLAIM-SW                 PIC X        VALUE SPACE.
00087          88  CHECK-AH-CLAIM          VALUE 'A'.
00088          88  CHECK-LF-CLAIM          VALUE 'L'.
00089
00090      05  WS-BROWSE-SW                PIC X        VALUE SPACE.
00091      05  WS-FREE-LOOK                PIC X        VALUE SPACE.
00092
00093      05  WS-MAPSET-NAME              PIC X(8)     VALUE 'EL1275S'.
00094      05  WS-MAP-NAME                 PIC X(8)     VALUE 'EL127E'.
00095      05  WS-MAP-NUMBER               PIC X(4)     VALUE '127E'.
00096
00097      05  WS-PROGRAM-ID               PIC X(8)     VALUE 'EL1275'.
00098
00099      05  QID.
00100          10  QID-TERM                PIC X(04)    VALUE SPACES.
00101          10  FILLER                  PIC X(04)    VALUE '127E'.
00102
00103      05  WS-TRANS-ID                 PIC X(4)     VALUE 'EXX5'.
00104      05  EL001                       PIC X(8)     VALUE 'EL001'.
00105      05  EL004                       PIC X(8)     VALUE 'EL004'.
00106      05  EL005                       PIC X(8)     VALUE 'EL005'.
00107      05  EL010                       PIC X(8)     VALUE 'EL010'.
00108      05  ELDATCV                     PIC X(8)     VALUE 'ELDATCV'.
00109      05  ELRTRM                      PIC X(8)     VALUE 'ELRTRM'.
00110      05  ELRAMT                      PIC X(8)     VALUE 'ELRAMT'.
00111      05  ELRFND                      PIC X(8)     VALUE 'ELRFND'.
00112
00113      05  WS-CONTROL-FILE-DSID        PIC X(8)     VALUE 'ELCNTL'.
00114      05  WS-CERTIFICATE-MASTER-DSID  PIC X(8)     VALUE 'ELCERT'.
00115      05  WS-CLAIM-MASTER-DSID        PIC X(8)     VALUE 'ELMSTR5'.
00116
00117      05  DEEDIT-FIELD                PIC X(15).
00118      05  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD   PIC S9(15).
00119
00120      05  WS-ACCOUNT.
00121          10  FILLER                  PIC X(4).
00122          10  WS-ACCT                 PIC X(6).
00123
00124      05  WS-INDEX                    PIC S9(4)    VALUE ZERO
00125                                      COMP
00126                                      SYNC.
00127
00128      EJECT
00129      05  ER-0008                     PIC X(4)    VALUE '0008'.
00130      05  ER-0029                     PIC X(4)    VALUE '0029'.
00131      05  ER-0142                     PIC X(4)    VALUE '0142'.
00132      05  ER-7048                     PIC X(4)    VALUE '7048'.
00133      05  ER-7237                     PIC X(4)    VALUE '7237'.
00134
00135      05  WS-CONTROL-FILE-KEY.
00136          10  WS-CFK-COMPANY-ID       PIC X(3)     VALUE SPACES.
00137          10  WS-CFK-RECORD-TYPE      PIC X        VALUE ZERO.
00138 *          88  STATE-MASTER                       VALUE '3'.
00139 *          88  LF-BENEFIT-MASTER                  VALUE '4'.
00140 *          88  AH-BENEFIT-MASTER                  VALUE '5'.
00141 *          88  CARRIER-MASTER                     VALUE '6'.
00142          10  WS-CFK-ACCESS.
00143              15  WS-CFK-STATE        PIC XX       VALUE SPACES.
00144              15  WS-CFK-BENEFIT-NO                VALUE SPACES.
00145                  20  FILLER          PIC X.
00146                  20  WS-CFK-CARRIER  PIC X.
00147          10  WS-CFK-SEQUENCE-NO      PIC S9(4)    VALUE ZERO COMP.
00148
00149      05  WS-CERTIFICATE-KEY.
00150          10  WS-CK-COMPANY-CD        PIC X.
00151          10  WS-CK-CARRIER           PIC X.
00152          10  WS-CK-GROUPING          PIC X(6).
00153          10  WS-CK-STATE             PIC XX.
00154          10  WS-CK-ACCOUNT           PIC X(10).
00155          10  WS-CK-CERT-EFF-DT       PIC XX.
00156          10  WS-CK-CERT-NO.
00157              15  WS-CK-CERT-PRIME    PIC X(10).
00158              15  WS-CK-CERT-SFX      PIC X.
00159
00160      05  WS-CLAIM-KEY.
00161          10  WS-CL-COMPANY-CD        PIC X.
00162          10  WS-CL-CERT-NO.
00163              15  WS-CL-CERT-PRIME    PIC X(10).
00164              15  WS-CL-CERT-SFX      PIC X.

092118     05  WS-ELCRTT-KEY.
092118         10  WS-CS-COMPANY-CD        PIC X.
092118         10  WS-CS-CARRIER           PIC X.
092118         10  WS-CS-GROUPING          PIC X(6).
092118         10  WS-CS-STATE             PIC XX.
092118         10  WS-CS-ACCOUNT           PIC X(10).
092118         10  WS-CS-CERT-EFF-DT       PIC XX.
092118         10  WS-CS-CERT-NO.
092118             15  WS-CS-CERT-PRIME    PIC X(10).
092118             15  WS-CS-CERT-SFX      PIC X.
092118         10  WS-CS-REC-TYPE          PIC X.

           05  WS-ACCT-KEY.
               10  WS-AK-COMPANY-CD        PIC X.
               10  WS-AK-CARRIER           PIC X.
               10  WS-AK-GROUPING          PIC X(6).
               10  WS-AK-STATE             PIC XX.
               10  WS-AK-ACCOUNT           PIC X(10).
               10  WS-AK-ACCT-EXP-DT       PIC XX.

          
                                       COPY ERCACCT.
092118                                 COPY ELCCRTT.

00167                                      COPY ELCINTF.
00168      12  FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.
00169          16  FILLER              PIC X(314).
00170          16  PI-1ST-TIME-SW      PIC X.
00171          16  PI-BENEFIT-IND      PIC X.
00172          16  PI-PREV-BENEFIT     PIC X.
00173          16  PI-PEND-SW          PIC X.
00174          16  PI-CLAIM-SW         PIC X.
00175          16  FILLER              PIC X(321).
00176
00178                                      COPY ELCDATE.
00180                                      COPY EL1275S.
00182                                      COPY ELCCALC.
00185                                      COPY ELCEMIB.
00188                                      COPY ELCLOGOF.
00191                                      COPY ELCATTR.
00194                                      COPY ELCAID.
00195
00196  01  FILLER REDEFINES DFHAID.
00197      05  FILLER                      PIC X(8).
00198      05  PF-VALUES                   PIC X
00199          OCCURS 24 TIMES.
00200
00201  LINKAGE SECTION.
00202
00203  01  DFHCOMMAREA                     PIC X(1024).
00204
00205 *01 PARMLIST   COMP SYNC.
00206 *    05  FILLER                      PIC S9(8).
00207 *    05  ELCERT-POINTER              PIC S9(8).
00208 *    05  ELCNTL-POINTER              PIC S9(8).
00209 *    05  ELMSTR-POINTER              PIC S9(8).
00210
00211      EJECT
00212                                      COPY ELCCERT.
00213      EJECT
00214                                      COPY ELCCNTL.
00215      EJECT
00216                                      COPY ELCMSTR.

                                       COPY ERCPDEF.
00217
00218      EJECT
00219  PROCEDURE DIVISION.
00220
00221      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00222      MOVE '5'                    TO  DC-OPTION-CODE.
00223      PERFORM 8500-DATE-CONVERSION.
00224      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00225      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00226
00227 *    NOTE *******************************************************
00228 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL *
00229 *         *  FROM ANOTHER MODULE.                               *
00230 *         *******************************************************.
00231
00232      IF EIBCALEN NOT GREATER THAN ZERO
00233          MOVE UNACCESS-MSG       TO  LOGOFF-MSG
00234          GO TO 8300-SEND-TEXT.
00235
00236      EXEC CICS HANDLE CONDITION
00237          ERROR  (9990-ERROR)
00238          QIDERR (0001-FIRST-TIME)
00239      END-EXEC.
00240
00241      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00242      MOVE EIBTRMID               TO  QID-TERM.
00243
00244      EXEC CICS READQ TS
00245          QUEUE    (QID)
00246          INTO     (PROGRAM-INTERFACE-BLOCK)
00247          LENGTH   (PI-COMM-LENGTH)
00248      END-EXEC.
00249
00250      EXEC CICS DELETEQ TS
00251          QUEUE    (QID)
00252      END-EXEC.
00253
00254  0001-FIRST-TIME.
00255
00256      IF PI-1ST-TIME-SW = '1'
00257          MOVE '2'                TO  PI-1ST-TIME-SW
00258          GO TO 0100-RECEIVE.
00259
00260      IF PI-1ST-TIME-SW = '2'
00261          GO TO 0100-RECEIVE.
00262
00263      MOVE '1'                    TO  PI-1ST-TIME-SW.
00264      GO TO 1000-DISPLAY-CERTIFICATE.
00265
00266      EJECT
00267  0100-RECEIVE.
00268      IF EIBAID = DFHCLEAR
00269          GO TO 9400-CLEAR.
00270
00271      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00272          MOVE ER-0008            TO  EMI-ERROR
00273          PERFORM 9900-ERROR-FORMAT
00274          MOVE -1                 TO  EPFKEYL
00275          GO TO 8200-SEND-DATAONLY.
00276
00277      EXEC CICS RECEIVE
00278          MAPSET (WS-MAPSET-NAME)
00279          MAP    (WS-MAP-NAME)
00280          INTO   (EL127EI)
00281      END-EXEC.
00282
00283      IF EPFKEYL = 0
00284          GO TO 0200-CHECK-PFKEYS.
00285
00286      IF EIBAID NOT = DFHENTER
00287          MOVE ER-0008            TO  EMI-ERROR
00288          MOVE -1                 TO  EPFKEYL
00289          GO TO 0300-INPUT-ERROR.
00290
00291      IF (EPFKEYI IS NUMERIC) AND
00292          (EPFKEYI IS GREATER THAN 0 AND LESS THAN 25)
00293              MOVE PF-VALUES (EPFKEYI)    TO  EIBAID
00294      ELSE
00295          MOVE ER-0029                    TO  EMI-ERROR
00296          GO TO 0300-INPUT-ERROR.
00297
00298  0200-CHECK-PFKEYS.
00299
00300      IF EIBAID = DFHPF3
00301         MOVE 'EL1274'            TO  WS-PROGRAM-ID
00302         GO TO 9300-XCTL.
00303
00304      IF EIBAID = DFHPF4
00305          MOVE '1'                TO  PI-1ST-TIME-SW
00306          MOVE 'EL1273'           TO  WS-PROGRAM-ID
00307          GO TO 9300-XCTL.
00308
00309      IF EIBAID = DFHPF5
00310          IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')
00311              MOVE ER-0029        TO  EMI-ERROR
00312              MOVE -1             TO  EPFKEYL
00313              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00314              GO TO 8200-SEND-DATAONLY
00315          ELSE
00316              IF PI-COMPANY-ID = 'DMD'
00317                  MOVE 'EL401DMD' TO  WS-PROGRAM-ID
00318                ELSE
101509*00319                  MOVE 'EL1276'   TO  WS-PROGRAM-ID
101509                 MOVE 'EL1279'   TO  WS-PROGRAM-ID
00320              END-IF
00321              GO TO 9300-XCTL.
00322
00323      IF PI-MAIL-YES
00324          IF EIBAID = DFHPF6
00325              IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')
00326                  MOVE ER-0029    TO  EMI-ERROR
00327                  MOVE -1         TO  EPFKEYL
00328                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00329                  GO TO 8200-SEND-DATAONLY
00330              ELSE
00331                  MOVE 'EL1277'   TO  WS-PROGRAM-ID
00332                  GO TO 9300-XCTL.
00333
00334      IF EIBAID IS EQUAL TO DFHPF7
00335          IF PI-CLAIM-SW IS EQUAL TO 'Y'
00336                     AND
00337             CREDIT-SESSION
00338              MOVE ' '                    TO  PI-1ST-TIME-SW
00339              EXEC CICS WRITEQ TS
00340                  QUEUE    (QID)
00341                  FROM     (PROGRAM-INTERFACE-BLOCK)
00342                  LENGTH   (PI-COMM-LENGTH)
00343              END-EXEC
00344              MOVE WS-PROGRAM-ID          TO  PI-CALLING-PROGRAM
00345              MOVE 'EL132   '             TO  WS-PROGRAM-ID
00346              GO TO 9300-XCTL
00347          ELSE
00348              MOVE ER-0029                TO  EMI-ERROR
00349              MOVE -1                     TO  EPFKEYL
00350              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00351              GO TO 8200-SEND-DATAONLY.
00352
00353      IF EIBAID = DFHPF12
00354          MOVE 'EL010   '         TO  WS-PROGRAM-ID
00355          GO TO 9300-XCTL.
00356
00357      IF EIBAID = DFHPF23
00358          GO TO 9000-RETURN-CICS.
00359
00360      IF EIBAID = DFHPF24
00361          MOVE 'EL126   '         TO  WS-PROGRAM-ID
00362          GO TO 9300-XCTL.
00363
00364      IF EIBAID = DFHPF1 OR DFHPF2
00365          GO TO 1000-DISPLAY-CERTIFICATE.
00366
00367      IF EIBAID = DFHENTER
00368          GO TO 0400-EDIT.
00369
00370      MOVE ER-0029                TO  EMI-ERROR.
00371
00372  0300-INPUT-ERROR.
00373
00374      PERFORM 9900-ERROR-FORMAT.
00375      MOVE AL-UNBON               TO  EPFKEYA.
00376      MOVE -1                     TO  EPFKEYL.
00377      GO TO 8200-SEND-DATAONLY.
00378
00379  0400-EDIT.
00380      IF EUEDTL IS GREATER THAN 0
00381          MOVE EUEDTI                 TO  DEEDIT-FIELD
00382          PERFORM 7500-DEEDIT THRU 7500-EXIT
00383          IF DEEDIT-FIELD-V0 IS GREATER THAN 0
00384              MOVE DEEDIT-FIELD-V0    TO  DC-GREG-DATE-1-MDY
00385              MOVE '4'                TO  DC-OPTION-CODE
00386              PERFORM 8500-DATE-CONVERSION
00387              IF NO-CONVERSION-ERROR
00388                  MOVE DC-BIN-DATE-1  TO  WS-VALUATION-DT
00389                  GO TO 1000-DISPLAY-CERTIFICATE.
00390
00391      MOVE PI-CR-MONTH-END-DT         TO  WS-VALUATION-DT.
00392      EJECT
00393  1000-DISPLAY-CERTIFICATE.
00394
00395 ******************************************************************
00396 *                                                                *
00397 *               READ THE REQUESTED CERT RECORD                   *
00398 *                                                                *
00399 ******************************************************************
00400
00401      MOVE PI-COMPANY-CD          TO  WS-CK-COMPANY-CD.
00402      MOVE PI-CARRIER             TO  WS-CK-CARRIER.
00403      MOVE PI-GROUPING            TO  WS-CK-GROUPING.
00404      MOVE PI-STATE               TO  WS-CK-STATE.
00405      MOVE PI-ACCOUNT             TO  WS-CK-ACCOUNT.
00406      MOVE PI-CERT-NO             TO  WS-CK-CERT-NO.
00407      MOVE PI-CERT-EFF-DT         TO  WS-CK-CERT-EFF-DT.
00408
00409      EXEC CICS HANDLE CONDITION
00410          NOTFND (8880-NOT-FOUND)
00411      END-EXEC.
00412
00413      EXEC CICS READ
00414          DATASET (WS-CERTIFICATE-MASTER-DSID)
00415          RIDFLD  (WS-CERTIFICATE-KEY)
00416          SET     (ADDRESS OF CERTIFICATE-MASTER)
00417      END-EXEC.
00418
00419      EJECT
00420  2000-BUILD-OUTPUT-MAP.
00421
00422 ******************************************************************
00423 *                                                                *
00424 *              BUILD THE OUTPUT SCREEN TO BE DISPLAYED           *
00425 *                                                                *
00426 ******************************************************************
00427
00428      MOVE LOW-VALUES             TO  EL127EO.
00429      MOVE CM-CERT-PRIME          TO  ECERTNOO.
00430      MOVE CM-CERT-SFX            TO  ECRTSFXO.
00431      MOVE CM-ACCOUNT             TO  EACCTNOO
00432                                      WS-ACCOUNT.
00433      MOVE CM-STATE               TO  ESTATEO.
00434      MOVE CM-CARRIER             TO  ECARIERO.
00435      MOVE CM-GROUPING            TO  EGROUPO.
00436
00437      IF CM-MEMB-STATE   = CM-STATE OR
00438         CM-MEMB-ACCOUNT = WS-ACCT
00439          MOVE SPACES             TO  EMEMNOO
00440      ELSE
00441          MOVE CM-MEMBER-NO       TO  EMEMNOO.
00442
00443      IF CM-CERT-EFF-DT NOT = LOW-VALUES
00444          MOVE SPACES             TO  DC-OPTION-CODE
00445          MOVE CM-CERT-EFF-DT     TO  DC-BIN-DATE-1
00446          PERFORM 8500-DATE-CONVERSION
00447          MOVE DC-GREG-DATE-1-EDIT TO EEFFDTO.
00448
00449      MOVE CM-REIN-TABLE          TO  EREITBLO.
00450      MOVE CM-SPECIAL-REIN-CODE   TO  ESPECO.
00451
00452      IF PI-MAIL-YES
00453          MOVE AL-SANON           TO  EPFKEY6A
00454      ELSE
00455          MOVE AL-SADOF           TO  EPFKEY6A.
00456
00457      EJECT
00458 ******************************************************************
00459 *                                                                *
00460 *              PI-BENEFIT-INDICATOR VALUES:                      *
00461 *                   1 = LIFE COVERAGE ONLY                       *
00462 *                   2 = A&H  COVERAGE ONLY                       *
00463 *                   3 = TWO  COVERAGES PRESENT                   *
00464 *                                                                *
00465 ******************************************************************
00466      IF CM-LF-BENEFIT-CD NOT = '00'  AND
00467         CM-AH-BENEFIT-CD NOT = '00'
00468              MOVE '3'            TO  PI-BENEFIT-IND
00469          ELSE
00470              IF CM-LF-BENEFIT-CD NOT = '00'
00471                  MOVE '1'        TO  PI-BENEFIT-IND
00472              ELSE
00473                  IF CM-AH-BENEFIT-CD NOT = '00'
00474                      MOVE '2'    TO  PI-BENEFIT-IND
00475                  ELSE
00476                      MOVE '0'    TO  PI-BENEFIT-IND.
00477
00478      IF PI-BENEFIT-IND = '3' AND
00479          PI-1ST-TIME-SW = '1'
00480              GO TO 2100-LF-BENEFIT.
00481
00482      IF EIBAID = DFHPF1
00483          IF PI-BENEFIT-IND = '3'
00484              IF PI-PREV-BENEFIT = '1'
00485                  GO TO 2200-AH-BENEFIT.
00486
00487      IF EIBAID = DFHPF1
00488          IF PI-BENEFIT-IND = '3'
00489              IF PI-PREV-BENEFIT = '2'
00490                  MOVE ER-7237        TO  EMI-ERROR
00491                  MOVE -1             TO  EPFKEYL
00492                  PERFORM 9900-ERROR-FORMAT
00493                  GO TO 8200-SEND-DATAONLY.
00494
00495      IF EIBAID = DFHPF2
00496          IF PI-BENEFIT-IND = '3'
00497              IF PI-PREV-BENEFIT = '2'
00498                  GO TO 2100-LF-BENEFIT.
00499
00500      IF EIBAID = DFHPF2
00501          IF PI-BENEFIT-IND = '3'
00502              IF PI-PREV-BENEFIT = '1'
00503                  MOVE ER-7237        TO  EMI-ERROR
00504                  MOVE -1             TO  EPFKEYL
00505                  PERFORM 9900-ERROR-FORMAT
00506                  GO TO 8200-SEND-DATAONLY.
00507
00508      IF PI-BENEFIT-IND = '1'
00509          MOVE ER-7048                TO  EMI-ERROR
00510          PERFORM 9900-ERROR-FORMAT
00511          GO TO 2100-LF-BENEFIT.
00512
00513      IF PI-BENEFIT-IND = '2'
00514          MOVE ER-7048                TO  EMI-ERROR
00515          PERFORM 9900-ERROR-FORMAT
00516          GO TO 2200-AH-BENEFIT.
00517
00518      IF EIBAID = DFHENTER
00519          IF PI-PREV-BENEFIT IS EQUAL TO '1'
00520              GO TO 2100-LF-BENEFIT
00521          ELSE
00522              GO TO 2200-AH-BENEFIT.
00523
00524      GO TO 8200-SEND-DATAONLY.
00525
00526      EJECT
00527  2100-LF-BENEFIT.
00528      MOVE '1'                    TO  PI-PREV-BENEFIT.
00529
00530      MOVE '4'                    TO  WS-CFK-RECORD-TYPE.
00531      MOVE CM-LF-BENEFIT-CD       TO  WS-BENEFIT-NO.
00532      MOVE +0                     TO  WS-CFK-SEQUENCE-NO.
00533      PERFORM 3000-LOCATE-BENEFIT.
00534
00535      IF WS-CALC-CD IS EQUAL TO 'Z'
00536          MOVE AL-UNNOF           TO  ETERMA.
00537
00538      MOVE '2'                    TO CP-PROCESS-TYPE.
00539      MOVE WS-LF-COVERAGE-TYPE    TO  CP-BENEFIT-TYPE.
00540      MOVE WS-EARNINGS-CALC       TO  CP-EARNING-METHOD
00541                                      CP-RATING-METHOD.
00542      MOVE WS-CALC-CD             TO  CP-SPECIAL-CALC-CD.
00543      MOVE CM-CERT-EFF-DT         TO  CP-CERT-EFF-DT.
00544      MOVE CM-LOAN-1ST-PMT-DT     TO  CP-FIRST-PAY-DATE.
00545      MOVE SAVE-BIN-DATE          TO  CP-VALUATION-DT.
00546      IF CM-LF-ORIG-TERM IS EQUAL TO 0
00547          MOVE 1                  TO  CP-ORIGINAL-TERM
00548      ELSE
00549          MOVE CM-LF-ORIG-TERM    TO  CP-ORIGINAL-TERM.
00550      MOVE CM-LOAN-TERM           TO  CP-LOAN-TERM.
00551      MOVE PI-REM-TRM-CALC-OPTION TO  CP-REM-TRM-CALC-OPTION.
00552      MOVE '4'                    TO  CP-REM-TERM-METHOD.
00553      MOVE PI-COMPANY-ID          TO  CP-COMPANY-ID.
00554      MOVE PI-COMPANY-CD          TO  CP-COMPANY-CD.
00555
00556 *** READ STATE MASTER RECORD FOR FREE LOOK PERIOD ***
00557      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
00558      MOVE '3'                    TO  WS-CFK-RECORD-TYPE.
00559      MOVE SPACES                 TO  WS-CFK-ACCESS.
00560      MOVE CM-STATE               TO  WS-CFK-STATE.
00561      MOVE +0                     TO  WS-CFK-SEQUENCE-NO.
00562      MOVE 'Y'                    TO  WS-FREE-LOOK.
00563
00564      PERFORM 3500-READ-CNTL THRU 3500-EXIT.
00565
00566      MOVE CF-ST-FREE-LOOK-PERIOD TO CP-FREE-LOOK.
00567
00568      PERFORM 9800-LINK-REM-TERM.
00569
00570      MOVE PI-LIFE-OVERRIDE-L12   TO  EKINDO.
00571      MOVE CM-LF-BENEFIT-CD       TO  ECODEO.
00572      MOVE WS-BENEFIT-DESCRIP     TO  EDESCO.
00573      MOVE CM-LF-ORIG-TERM        TO  ETERMO.
00574      MOVE CP-REMAINING-TERM-3    TO  ERTERMO.
00575      IF CP-REMAINING-TERM-3 GREATER THAN CM-LF-ORIG-TERM
00576         MOVE CM-LF-ORIG-TERM     TO  ERTERMO.
00577      MOVE CM-LF-TERM-IN-DAYS     TO  ETRMDAYO.
00578      MOVE WS-COMMENT             TO  ECOMENTO.
00579      MOVE CM-LF-CRITICAL-PERIOD  TO  ECRITPDO.
00580      MOVE CM-LF-PREMIUM-AMT      TO  EPREMO.
           IF CP-EARNING-METHOD = 'B'
00581         MOVE CM-LF-ALT-PREMIUM-AMT  TO  EALTPRMO
           END-IF
           IF CM-LF-CLP NOT NUMERIC
              MOVE ZEROS               TO CM-LF-CLP
           END-IF

           MOVE CM-CLP-STATE           TO  ECLPSTO
           MOVE CM-LF-CLP              TO  ECLPO
      *    MOVE CM-ADDL-CLP            TO  EACLPO


00582      MOVE CM-LF-NSP-PREMIUM-AMT  TO  EREINSPO.
00583      MOVE CM-LF-BENEFIT-AMT      TO  EBENEO.
           IF CP-EARNING-METHOD = 'B'
00584         MOVE CM-LF-ALT-BENEFIT-AMT  TO  EALTBENO
           END-IF
00585      MOVE CM-LF-ITD-CANCEL-AMT   TO  EITDREFO.
00586      MOVE CM-LF-EXIT-BATCH       TO  EEXBTCHO.
00587      MOVE CM-LF-ITD-DEATH-AMT    TO  EITDPMTO.
00588      IF CM-LF-PREMIUM-RATE NUMERIC
00589          MOVE CM-LF-PREMIUM-RATE     TO  EPRRTO
00590      ELSE
00591          MOVE ZEROS                  TO  EPRRTO.
00592      IF CM-LF-ALT-PREMIUM-RATE NUMERIC
00593          MOVE CM-LF-ALT-PREMIUM-RATE TO  EALPRRTO
00594      ELSE
00595          MOVE ZEROS                  TO  EALPRRTO.
00596      MOVE CM-LF-DEV-CODE         TO  EDVCDO.
00597      MOVE CM-LIFE-COMM-PCT       TO  EACCPCTO.
00598      MOVE CM-REIN-TABLE          TO  EREITBLO.
00599      MOVE SPACES                 TO  EPDTHRUO.
00600
00601      MOVE SPACES                 TO  ECLSTATO.
00602      IF CM-CLAIM-ATTACHED-COUNT GREATER THAN +0
00603          MOVE '*'                TO  EASRISKO
00604          MOVE 'L'                TO WS-CLAIM-SW
00605          PERFORM 7600-CHECK-FOR-CLAIM
00606      ELSE
00607          MOVE ' '                TO  PI-CLAIM-SW
00608                                      EASRISKO.
00609
00610      IF CM-LF-LOAN-EXPIRE-DT IS NOT = LOW-VALUES AND SPACES
00611          MOVE SPACES                 TO  DC-OPTION-CODE
00612          MOVE CM-LF-LOAN-EXPIRE-DT   TO  DC-BIN-DATE-1
00613          PERFORM 8500-DATE-CONVERSION
00614          MOVE DC-GREG-DATE-1-EDIT    TO  EEXPDTO.
00615
00616 ******************************************************************
00617 *               DISPLAY ENTRY STATUS CODES                       *
00618 ******************************************************************
00619
00620      MOVE SPACES                 TO EENTSTO.
00621
00622      IF CM-ENTRY-STATUS = '1'
00623          MOVE 'NORM'             TO  EENTSTO.
00624      IF CM-ENTRY-STATUS = '2'
00625          MOVE 'PEND'             TO  EENTSTO
00626          MOVE 'P'                TO  PI-PEND-SW.
00627      IF CM-ENTRY-STATUS = '4'
00628          MOVE 'CONV'             TO  EENTSTO.
00629      IF CM-ENTRY-STATUS = '5'
00630          MOVE 'REIS'             TO  EENTSTO.
122002     IF CM-ENTRY-STATUS = 'M'
122002         MOVE 'MONTHLY'          TO  EENTSTO.
00631      IF CM-ENTRY-STATUS = '9'
00632          MOVE 'REIN'             TO  EENTSTO.
00633
00634      IF CM-ENTRY-STATUS = 'D'
00635          MOVE 'DECL'             TO  EENTSTO.
00636
00637      IF CM-ENTRY-STATUS = 'V'
00638          MOVE 'VOID'             TO  EENTSTO.
00639
00640      IF CM-ENTRY-STATUS = 'U'
00641          MOVE 'UNDERWRT'         TO  EENTSTO.
00642
00643 ******************************************************************
00644 *                                                                *
00645 *               DISPLAY CURRENT STATUS CODES                     *
00646 *                                                                *
00647 ******************************************************************
00648
00649      MOVE SPACES                 TO ECURSTO.
00650
00651      IF CM-LF-CURRENT-STATUS = '1' OR '4'
00652          IF CP-REMAINING-TERM-3 = ZEROS
00653              MOVE 'EXPIRED'      TO  ECURSTO
00654          ELSE
00655              MOVE 'ACTIVE'       TO  ECURSTO.
00656
00657      IF CM-LF-CURRENT-STATUS = '2'
00658          MOVE 'PEND'             TO  ECURSTO
00659          MOVE 'P'                TO  PI-PEND-SW.
00660
00661      IF CM-LF-CURRENT-STATUS = '3'
00662          MOVE 'RESTORE'          TO  ECURSTO.
00663
00664      IF CM-LF-CURRENT-STATUS = '5'
00665          MOVE 'REISSUE'          TO  ECURSTO.
122002     IF CM-LF-CURRENT-STATUS = 'M'
122002         MOVE 'MONTHLY'          TO  ECURSTO.
00666
00667      IF CM-LF-CURRENT-STATUS = '6'
100518         MOVE 'LMP BEN'          TO  ECURSTO.
00669
00670      IF CM-LF-CURRENT-STATUS = '7'
00671          MOVE 'DEATH'            TO  ECURSTO.
00672
00673      IF CM-LF-CURRENT-STATUS = '8'
00674          MOVE 'CANCEL'           TO  ECURSTO.
00675
00676      IF CM-LF-CURRENT-STATUS = '9'
00677          MOVE 'RE-ONLY'          TO  ECURSTO.
00678
00679      IF CM-LF-CURRENT-STATUS = 'D'
00680          MOVE 'DECLINE'          TO  ECURSTO.
00681
00682      IF CM-LF-CURRENT-STATUS = 'V'
00683          MOVE 'VOID'             TO  ECURSTO.
00684
00685 ******************************************************************
00686 *                                                                *
00687 *            CALCULATE REMAINING BENEFIT AMOUNT                  *
00688 *                                                                *
00689 ******************************************************************
00690
00691      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
00692      MOVE '3'                    TO  WS-CFK-RECORD-TYPE.
00693      MOVE SPACES                 TO  WS-CFK-ACCESS.
00694      MOVE CM-STATE               TO  WS-CFK-STATE.
00695      MOVE +0                     TO  WS-CFK-SEQUENCE-NO.
00696
00697      PERFORM 3500-READ-CNTL THRU 3500-EXIT.
00698
00699      MOVE CF-STATE-ABBREVIATION  TO  WS-STATE-ABBREV
00700                                      CP-STATE-STD-ABBRV.
00701
00702      MOVE CF-ST-FREE-LOOK-PERIOD TO  CP-FREE-LOOK.
00703
00704      MOVE CM-LF-BENEFIT-AMT      TO  CP-ORIGINAL-BENEFIT
00705                                      CP-RATING-BENEFIT-AMT.
00706      MOVE CM-LF-PREMIUM-AMT      TO  CP-ORIGINAL-PREMIUM.
00707      MOVE CM-LF-ALT-BENEFIT-AMT  TO  CP-ALTERNATE-BENEFIT.
00708      MOVE CM-LF-ALT-PREMIUM-AMT  TO  CP-ALTERNATE-PREMIUM.
00709      MOVE CM-LOAN-APR            TO  CP-LOAN-APR.
00710      MOVE CM-PAY-FREQUENCY       TO  CP-PAY-FREQUENCY.
00711      MOVE CM-LOAN-TERM           TO  CP-LOAN-TERM.
00712
CIDMOD     MOVE CM-RATE-CLASS          TO  CP-CLASS-CODE
00713      MOVE CP-REMAINING-TERM-3    TO  CP-REMAINING-TERM.
00714
00715      IF CM-LF-CURRENT-STATUS = '6' OR '7' OR '8'
00716          MOVE ZEROS                  TO  EREMBENO
00717      ELSE
              PERFORM 2500-GET-ERACCT  THRU 2500-EXIT
              MOVE ZEROS               TO CP-R-MAX-TOT-BEN
010816        IF (PI-COMPANY-ID = 'DCC' OR 'CAP')
                 AND (ACCT-FOUND)
                 AND (AM-DCC-PRODUCT-CODE <> SPACES)
                 MOVE ' '             TO WS-PDEF-RECORD-SW
                 PERFORM 2600-GET-MAX-BENEFIT
                                       THRU 2600-EXIT
080322           PERFORM 9800-LINK-REM-TERM

                 IF PDEF-FOUND
080322              IF CM-INSURED-JOINT-AGE LESS CM-INSURED-ISSUE-AGE
080322                  MOVE CM-INSURED-JOINT-AGE TO WS-EDIT-AGE
080322              ELSE
080322                  MOVE CM-INSURED-ISSUE-AGE TO WS-EDIT-AGE
080322              END-IF
080322              COMPUTE WS-ATT-AGE = WS-EDIT-AGE
080322                 + ((CM-LF-ORIG-TERM - CP-REMAINING-TERM-3) / 12)
                    display ' att age ' ws-att-age
                    PERFORM VARYING P1 FROM +1 BY +1 UNTIL
080322                 (P1 > +11)
                       OR ((PD-PROD-CODE (P1) = 'L' or 'O')
080322                   AND (PD-MAX-ATT-AGE (P1) >= WS-ATT-AGE))
                    END-PERFORM
080322              IF P1 < +12
                       MOVE PD-MAX-AMT (P1) TO CP-R-MAX-TOT-BEN
                       display ' max 1 ' cp-r-max-tot-ben
                    END-IF
                 END-IF
              END-IF
00718         PERFORM 9700-LINK-REM-AMT
              display ' max , rem ' cp-r-max-tot-ben ' '
                  cp-remaining-amt
00719         MOVE CP-REMAINING-AMT       TO  EREMBENO
           END-IF
00720
00721      IF PI-LIFE-OVERRIDE-L1 IS EQUAL TO 'P' OR
00722         WS-LF-COVERAGE-TYPE IS EQUAL TO 'P'
00723          COMPUTE WS-REMAINING-AMT = CM-LF-BENEFIT-AMT -
00724                                     CM-LF-ITD-DEATH-AMT
00725          MOVE WS-REMAINING-AMT       TO  EREMBENO.
00726
00727 ******************************************************************
00728 *                                                                *
00729 *            CALCULATE UNEARNED PREMIUM AMOUNT                   *
00730 *                      ONE MONTH EARNED PREMIUM AMOUNT           *
00731 *                                                                *
00732 ******************************************************************
00733
00734      MOVE '2'                    TO  CP-PROCESS-TYPE.
00735      MOVE CM-STATE               TO  CP-STATE.
00736      MOVE CM-RATE-CLASS          TO  CP-CLASS-CODE.
00737      MOVE CM-LF-DEV-CODE         TO  CP-DEVIATION-CODE.
00738      MOVE CM-INSURED-ISSUE-AGE   TO  CP-ISSUE-AGE.
00739
00740      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
00741      MOVE '1'                    TO  WS-CFK-RECORD-TYPE.
00742      MOVE SPACES                 TO  WS-CFK-ACCESS.
00743      MOVE +0                     TO  WS-CFK-SEQUENCE-NO.
00744      PERFORM 3500-READ-CNTL THRU 3500-EXIT.
00745      MOVE CF-LIFE-OVERRIDE-L1    TO  CP-LIFE-OVERRIDE-CODE.
00746      MOVE CF-CR-R78-METHOD       TO  CP-R78-OPTION.
00747      MOVE CF-CR-REM-TERM-CALC    TO  CP-REM-TERM-METHOD.
00748      MOVE PI-REM-TRM-CALC-OPTION TO  CP-REM-TRM-CALC-OPTION.
00749
00750      IF CP-STATE-STD-ABBRV = 'OR'
00751          COMPUTE CP-RATING-BENEFIT-AMT = CM-LF-BENEFIT-AMT +
00752                                          CM-LF-ALT-BENEFIT-AMT.
00753
00754      MOVE WS-BENEFIT-CODE        TO  CP-BENEFIT-CD.
00755      IF WS-CALC-CD IS EQUAL TO 'T'
00756          MOVE WS-OVRD-EARNINGS-CALC  TO  CP-EARNING-METHOD.
00757
00758      IF WS-CALC-CD IS EQUAL TO 'D'
00759          MOVE CM-LF-TERM-IN-DAYS          TO  CP-TERM-OR-EXT-DAYS
00760      ELSE
00761          IF CM-PMT-EXTENSION-DAYS IS NUMERIC
00762              MOVE CM-PMT-EXTENSION-DAYS   TO  CP-TERM-OR-EXT-DAYS
00763          ELSE
00764              MOVE ZEROS                   TO  CP-TERM-OR-EXT-DAYS.
00765
00766      MOVE CM-LOAN-1ST-PMT-DT     TO  CP-FIRST-PAY-DATE.
00767
00768      IF WS-VALUATION-DT IS EQUAL TO LOW-VALUES
00769          MOVE PI-CR-MONTH-END-DT TO  CP-VALUATION-DT
00770                                      WS-VALUATION-DT
00771      ELSE
00772          MOVE WS-VALUATION-DT    TO  CP-VALUATION-DT.
00773
00774      PERFORM 9800-LINK-REM-TERM.
00775      MOVE CP-REMAINING-TERM-1    TO  CP-REMAINING-TERM.
00776      PERFORM 9700-LINK-REM-AMT.
00777      MOVE CP-REMAINING-AMT       TO  CP-REMAINING-BENEFIT.
00778
00779      MOVE CP-REMAINING-TERM-1    TO  CP-REMAINING-TERM.
00780      PERFORM 9850-LINK-REFUND-RTN THRU 9850-EXIT.
00781      MOVE CP-CALC-REFUND         TO  EUEPREMO
00782                                      WS-CALC-REFUND.
00783      ADD +1                      TO  CP-REMAINING-TERM.
00784      IF CP-REMAINING-TERM IS EQUAL TO CM-LF-ORIG-TERM
00785          COMPUTE ONE-MON-EARNED = CM-LF-PREMIUM-AMT -
00786                                                 CP-CALC-REFUND
00787      ELSE
00788          PERFORM 9850-LINK-REFUND-RTN THRU 9850-EXIT
00789          COMPUTE ONE-MON-EARNED = WS-CALC-REFUND -
00790                                                 CP-CALC-REFUND.
00791
00792      MOVE ONE-MON-EARNED         TO  EOMEARNO.
00793
00794      IF WS-VALUATION-DT IS NOT = LOW-VALUES AND SPACES
00795          MOVE SPACES                 TO  DC-OPTION-CODE
00796          MOVE WS-VALUATION-DT        TO  DC-BIN-DATE-1
00797          PERFORM 8500-DATE-CONVERSION
00798          MOVE AL-UANON               TO  EUEDTA
00799          MOVE DC-GREG-DATE-1-EDIT    TO  EUEDTO.
00800
00801 ******************************************************************
00802 *                                                                *
00803 *                     DISPLAY CANCEL DATE                        *
00804 *                             CANCEL EXIT DATE                   *
00805 *                             CANCEL EXIT STATUS                 *
00806 *                                                                *
00807 ******************************************************************
00808
00809      MOVE SPACES                       TO  EEXITDTO.
00810
00811      IF CM-LF-CANCEL-EXIT-DT NOT = LOW-VALUES AND SPACES
00812          MOVE SPACES                   TO  DC-OPTION-CODE
00813          MOVE CM-LF-CANCEL-EXIT-DT     TO  DC-BIN-DATE-1
00814          PERFORM 8500-DATE-CONVERSION
00815          IF NOT DATE-CONVERSION-ERROR
00816              MOVE DC-GREG-DATE-1-EDIT  TO  EEXITDTO
00817          ELSE
00818              MOVE SPACES               TO  EEXITDTO.
00819
00820      MOVE SPACES                       TO  ECANDTO.
00821
00822      IF CM-LF-CANCEL-DT IS NOT = LOW-VALUES AND SPACES
00823          MOVE SPACES                   TO  DC-OPTION-CODE
00824          MOVE CM-LF-CANCEL-DT          TO  DC-BIN-DATE-1
00825          PERFORM 8500-DATE-CONVERSION
00826          IF NOT DATE-CONVERSION-ERROR
00827              MOVE DC-GREG-DATE-1-EDIT  TO  ECANDTO
00828          ELSE
00829              MOVE SPACES               TO  ECANDTO.
00830
00831      IF CM-LF-STATUS-AT-CANCEL = ' '
00832          MOVE SPACES             TO  EEXITSTO.
00833      IF CM-LF-STATUS-AT-CANCEL EQUAL '1' OR '2' OR '4' OR '9'
00834          MOVE 'ACTIVE'           TO  EEXITSTO.
00835      IF CM-LF-STATUS-AT-CANCEL = '6'
00836          MOVE 'PREV SETTLE'      TO  EEXITSTO.
00837      IF CM-LF-STATUS-AT-CANCEL = '7'
00838          MOVE 'PREV DTH'         TO  EEXITSTO.
00839      IF CM-LF-STATUS-AT-CANCEL = '8'
00840          MOVE 'PREV CANCEL'      TO  EEXITSTO.
00841
00842 ******************************************************************
00843 *                                                                *
00844 *                     DISPLAY CLAIM EXIT DATE                    *
00845 *                             CLAIM EXIT STATUS                  *
00846 *                                                                *
00847 ******************************************************************
00848
00849      MOVE SPACES                             TO  EEXDATEO.
00850
00851      IF CM-LF-DEATH-EXIT-DT IS NOT = LOW-VALUES AND
00852          (CM-LF-CURRENT-STATUS = '7' OR '8')
00853              MOVE SPACES                     TO  DC-OPTION-CODE
00854              MOVE CM-LF-DEATH-EXIT-DT        TO  DC-BIN-DATE-1
00855              PERFORM 8500-DATE-CONVERSION
00856              IF NOT DATE-CONVERSION-ERROR
00857                  MOVE DC-GREG-DATE-1-EDIT    TO  EEXDATEO
00858              ELSE
00859                  MOVE SPACES                 TO  EEXDATEO.
00860
00861      MOVE SPACES                 TO  EEXSTATO.
00862
00863      IF CM-LF-STATUS-AT-DEATH = ' '
00864          MOVE SPACES             TO  EEXSTATO.
00865      IF CM-LF-STATUS-AT-DEATH = '1' OR '2' OR '4' OR '9'
00866          MOVE 'ACTIVE'           TO  EEXSTATO.
00867      IF CM-LF-STATUS-AT-DEATH = '6'
00868          MOVE 'PREV SETTLE'      TO  EEXSTATO.
00869      IF CM-LF-STATUS-AT-DEATH = '7'
00870          MOVE 'PREV DTH'         TO  EEXSTATO.
00871      IF CM-LF-STATUS-AT-DEATH = '8'
00872          MOVE 'PREV CAN'         TO  EEXSTATO.

092118     move cm-control-primary     to ws-elcrtt-key
092118     move 'C'                    to ws-cs-rec-type
092118     exec cics read
092118        dataset   ('ELCRTT')
092118        ridfld    (ws-elcrtt-key)
092118        into      (certificate-trailers)
092118        resp      (ws-response)
092118     end-exec
092118     if resp-normal
092118        set good-hit-on-trlr to true
092118         evaluate true
092118            when cs-lf-refund-method = '1'
092118               move 'R78'        to refmetho
092118            when cs-lf-refund-method = '2'
092118               move 'PRO'        to refmetho
092118            when cs-lf-refund-method = '3'
092118               move 'CALIF'      to refmetho
092118            when cs-lf-refund-method = '4'
092118               move 'IRR/FARM'   to refmetho
092118            when cs-lf-refund-method = '5'
092118               move 'NET'        to refmetho
092118            when cs-lf-refund-method = '6'
092118               move 'ANTIC'      to refmetho
092118            when cs-lf-refund-method = '8'
092118               move 'MEAN'       to refmetho
092118            when cs-lf-refund-method = '9'
092118               move 'SUMDI'      to refmetho
092118            when cs-lf-refund-method = 'G'
092118               move 'GAPNR'      to refmetho
092118            when cs-lf-refund-method = 'S'
092118               move 'GAPA'       to refmetho
092118            when cs-lf-refund-method = 'D'
092118               move 'DDF'        to refmetho
092118            when cs-lf-refund-method = 'I'
092118               move 'DDFIU'      to refmetho
092118            when cs-lf-refund-method = 'R'
092118               move 'REPO'       to refmetho
092118            when other
092118               move cs-lf-refund-method
092118                                 to refmetho
092118         end-evaluate
092118     else
092118        set crtt-not-found to true
092118     end-if

00873
00874 ******************************************************************
00875 *                                                                *
00876 *                   DISPLAY EXTENSION DAYS                       *
00877 *                                                                *
00878 ******************************************************************
00879
00880      IF CM-PMT-EXTENSION-DAYS NUMERIC
00881          MOVE CM-PMT-EXTENSION-DAYS  TO  EEXTDAYO
00882      ELSE
00883          MOVE ZEROS                  TO  EEXTDAYO.
00884
00885 ******************************************************************
00886 *                                                                *
00887 *                   DISPLAY CEDED BENEFIT                        *
00888 *                                                                *
00889 ******************************************************************
00890
121802*    IF PI-COMPANY-ID EQUAL 'CVL' OR 'LGX'
121802*        IF CM-LF-CEDED-BENEFIT NUMERIC
121802*            MOVE CM-LF-CEDED-BENEFIT
121802*                                TO  ECEDBENO
121802*        ELSE
121802*            MOVE ZEROS          TO  ECEDBENO
121802*    ELSE
00898          MOVE AL-SADOF           TO  ECEDBENA, ECEDHDA.
00899
00900      GO TO 8100-SEND-INITIAL-MAP.
00901      EJECT
00902  2200-AH-BENEFIT.
00903
00904      MOVE '2'                    TO  PI-PREV-BENEFIT.
00905
00906      MOVE '5'                    TO  WS-CFK-RECORD-TYPE.
00907      MOVE CM-AH-BENEFIT-CD       TO  WS-BENEFIT-NO.
00908      MOVE +0                     TO  WS-CFK-SEQUENCE-NO.
00909      PERFORM 3000-LOCATE-BENEFIT.
00910
00911      IF WS-CALC-CD IS EQUAL TO 'Z'
00912          MOVE AL-UNNOF           TO  ETERMA.
00913
00914      MOVE '2'                    TO  CP-PROCESS-TYPE.
00915      MOVE 'A'                    TO  CP-BENEFIT-TYPE.
00916      MOVE WS-EARNINGS-CALC       TO  CP-EARNING-METHOD
00917                                      CP-RATING-METHOD.
00918      MOVE WS-CALC-CD             TO  CP-SPECIAL-CALC-CD.
00919      MOVE CM-CERT-EFF-DT         TO  CP-CERT-EFF-DT.
00920      MOVE CM-LOAN-1ST-PMT-DT     TO  CP-FIRST-PAY-DATE.
00921      MOVE SAVE-BIN-DATE          TO  CP-VALUATION-DT.
00922      IF CM-AH-ORIG-TERM IS EQUAL TO 0
00923          MOVE 1                  TO  CP-ORIGINAL-TERM
00924      ELSE
00925          MOVE CM-AH-ORIG-TERM    TO  CP-ORIGINAL-TERM.
00926      MOVE PI-REM-TRM-CALC-OPTION TO  CP-REM-TRM-CALC-OPTION.
00927      MOVE '4'                    TO  CP-REM-TERM-METHOD.
00928      MOVE PI-COMPANY-ID          TO  CP-COMPANY-ID.
00929      MOVE PI-COMPANY-CD          TO  CP-COMPANY-CD.
00930
00931 *** READ STATE MASTER RECORD FOR FREE LOOK PERIOD ***
00932      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
00933      MOVE '3'                    TO  WS-CFK-RECORD-TYPE.
00934      MOVE SPACES                 TO  WS-CFK-ACCESS.
00935      MOVE CM-STATE               TO  WS-CFK-STATE.
00936      MOVE +0                     TO  WS-CFK-SEQUENCE-NO.
00937      MOVE 'Y'                    TO  WS-FREE-LOOK.
00938
00939      PERFORM 3500-READ-CNTL THRU 3500-EXIT.
00940
00941      MOVE CF-ST-FREE-LOOK-PERIOD TO CP-FREE-LOOK.
00942
00943      PERFORM 9800-LINK-REM-TERM.
00944
00945      MOVE PI-AH-OVERRIDE-L12     TO  EKINDO.
00946      MOVE CM-AH-BENEFIT-CD       TO  ECODEO.
00947      MOVE WS-BENEFIT-DESCRIP     TO  EDESCO.
00948      MOVE CM-AH-ORIG-TERM        TO  ETERMO.
00949      MOVE CP-REMAINING-TERM-1    TO  ERTERMO.
00950      MOVE WS-COMMENT             TO  ECOMENTO.
00951      MOVE CM-AH-CRITICAL-PERIOD  TO  ECRITPDO.
00952      MOVE CM-AH-PREMIUM-AMT      TO  EPREMO.
00953      MOVE CM-AH-NSP-PREMIUM-AMT  TO  EREINSPO.
           IF CM-AH-CLP NOT NUMERIC
              MOVE ZEROS               TO CM-AH-CLP
           END-IF
           IF CM-ADDL-CLP NOT NUMERIC
              MOVE ZEROS               TO CM-ADDL-CLP
           END-IF
           MOVE CM-CLP-STATE           TO  ECLPSTO
           MOVE CM-AH-CLP              TO  ECLPO
           MOVE CM-ADDL-CLP            TO  EACLPO
00954      MOVE CM-AH-BENEFIT-AMT      TO  EBENEO.
00955      MOVE CM-AH-ITD-CANCEL-AMT   TO  EITDREFO.
00956      MOVE CM-AH-EXIT-BATCH       TO  EEXBTCHO.
00957      MOVE CM-AH-ITD-AH-PMT       TO  EITDPMTO.
00958      IF CM-AH-PREMIUM-RATE NUMERIC
00959          MOVE CM-AH-PREMIUM-RATE TO  EPRRTO
00960      ELSE
00961          MOVE ZEROS              TO  EPRRTO.
00962      MOVE CM-AH-DEV-CODE         TO  EDVCDO.
00963      MOVE CM-AH-COMM-PCT         TO  EACCPCTO.
00964
00965      MOVE SPACES                 TO  ECLSTATO.
00966      IF CM-CLAIM-ATTACHED-COUNT GREATER THAN +0
00967          MOVE '*'                TO  EASRISKO
00968          MOVE 'A'                TO  WS-CLAIM-SW
00969          PERFORM 7600-CHECK-FOR-CLAIM
00970      ELSE
00971          MOVE ' '                TO  PI-CLAIM-SW
00972                                      EASRISKO.
00973
00974      IF CM-AH-LOAN-EXPIRE-DT IS NOT = LOW-VALUES AND SPACES
00975          MOVE SPACES                 TO  DC-OPTION-CODE
00976          MOVE CM-AH-LOAN-EXPIRE-DT   TO  DC-BIN-DATE-1
00977          PERFORM 8500-DATE-CONVERSION
00978          MOVE DC-GREG-DATE-1-EDIT    TO  EEXPDTO.
00979
00980      IF CM-AH-PAID-THRU-DT IS NOT = LOW-VALUES AND SPACES
00981         IF NOT PI-USES-PAID-TO
00982            MOVE ' '                    TO  DC-OPTION-CODE
00983            MOVE CM-AH-PAID-THRU-DT     TO  DC-BIN-DATE-1
00984            PERFORM 8500-DATE-CONVERSION
00985            MOVE DC-GREG-DATE-1-EDIT    TO  EPDTHRUO
00986         ELSE
00987            MOVE '6'                    TO  DC-OPTION-CODE
00988            MOVE CM-AH-PAID-THRU-DT     TO  DC-BIN-DATE-1
00989            MOVE +1                     TO  DC-ELAPSED-DAYS
00990            MOVE +0                     TO  DC-ELAPSED-MONTHS
00991            PERFORM 8500-DATE-CONVERSION
00992            MOVE DC-GREG-DATE-1-EDIT    TO  EPDTHRUO.
00993
00994 ******************************************************************
00995 *                                                                *
00996 *               DISPLAY ENTRY STATUS CODES                       *
00997 *                                                                *
00998 ******************************************************************
00999
01000      MOVE SPACES                 TO  EENTSTO.
01001
01002      IF CM-ENTRY-STATUS = '1'
01003          MOVE 'NORM'             TO  EENTSTO.
01004      IF CM-ENTRY-STATUS = '2'
01005          MOVE 'PEND'             TO  EENTSTO
01006          MOVE 'P'                TO  PI-PEND-SW.
01007      IF CM-ENTRY-STATUS = '4'
01008          MOVE 'CONV'             TO  EENTSTO.
01009      IF CM-ENTRY-STATUS = '5'
01010          MOVE 'REIS'             TO  EENTSTO.
122002     IF CM-ENTRY-STATUS = 'M'
122002         MOVE 'MONTHLY'          TO  EENTSTO.
01011      IF CM-ENTRY-STATUS = '9'
01012          MOVE 'REIN'             TO  EENTSTO.
01013
01014      IF CM-ENTRY-STATUS = 'D'
01015          MOVE 'DECL'             TO  EENTSTO.
01016
01017      IF CM-ENTRY-STATUS = 'V'
01018          MOVE 'VOID'             TO  EENTSTO.
01019
01020      IF CM-ENTRY-STATUS = 'U'
01021          MOVE 'UNDERWRT'         TO  EENTSTO.
01022
01023 ******************************************************************
01024 *                                                                *
01025 *               DISPLAY CURRENT STATUS CODES                     *
01026 *                                                                *
01027 ******************************************************************
01028
01029      MOVE SPACES                 TO  ECURSTO.
01030
01031      IF CM-AH-CURRENT-STATUS = '1' OR '4'
01032          IF CP-REMAINING-TERM-3 = ZEROS
01033              MOVE 'EXPIRED'      TO  ECURSTO
01034          ELSE
01035              MOVE 'ACTIVE'       TO  ECURSTO.
01036
01037      IF CM-AH-CURRENT-STATUS = '2'
01038          MOVE 'PEND'             TO  ECURSTO
01039          MOVE 'P'                TO  PI-PEND-SW.
01040
01041      IF CM-AH-CURRENT-STATUS = '3'
01042          MOVE 'RESTORE'          TO  ECURSTO.
01043
01044      IF CM-AH-CURRENT-STATUS = '5'
01045          MOVE 'REISSUE'          TO  ECURSTO.
122002     IF CM-AH-CURRENT-STATUS = 'M'
122002         MOVE 'MONTHLY'          TO  ECURSTO.
01046
01047      IF CM-AH-CURRENT-STATUS = '6'
01048          MOVE 'LMP DIS'          TO  ECURSTO.
01049
01050      IF CM-AH-CURRENT-STATUS = '7'
01051          MOVE 'DEATH'            TO  ECURSTO.
01052
01053      IF CM-AH-CURRENT-STATUS = '8'
01054          MOVE 'CANCEL'           TO  ECURSTO.
01055
01056      IF CM-AH-CURRENT-STATUS = '9'
01057          MOVE 'RE-ONLY'          TO  ECURSTO.
01058
01059      IF CM-AH-CURRENT-STATUS = 'D'
01060          MOVE 'DECLINE'          TO  ECURSTO.
01061
01062      IF CM-AH-CURRENT-STATUS = 'V'
01063          MOVE 'VOID'             TO  ECURSTO.
01064
01065 ******************************************************************
01066 *                                                                *
01067 *            CALCULATE REMAINING BENEFIT AMOUNT                  *
01068 *                                                                *
01069 ******************************************************************
01070
01071      IF CM-AH-CURRENT-STATUS = '6' OR '7' OR '8'
01072          MOVE ZEROS              TO  EREMBENO
01073      ELSE
              PERFORM 2500-GET-ERACCT  THRU 2500-EXIT
              MOVE ZEROS               TO CP-R-MAX-MON-BEN
010816        IF (PI-COMPANY-ID = 'DCC' OR 'CAP')
                 AND (ACCT-FOUND)
                 AND (AM-DCC-PRODUCT-CODE = 'DDF')
                 MOVE ' '             TO WS-PDEF-RECORD-SW
                 PERFORM 2600-GET-MAX-BENEFIT
                                       THRU 2600-EXIT
                 IF PDEF-FOUND
080322              IF CM-INSURED-JOINT-AGE LESS CM-INSURED-ISSUE-AGE
080322                  MOVE CM-INSURED-JOINT-AGE TO WS-EDIT-AGE
080322              ELSE
080322                  MOVE CM-INSURED-ISSUE-AGE TO WS-EDIT-AGE
080322              END-IF
080322              COMPUTE WS-ATT-AGE = WS-EDIT-AGE
080322                 + ((CM-LF-ORIG-TERM - CP-REMAINING-TERM-3) / 12)

                    PERFORM VARYING P1 FROM +1 BY +1 UNTIL
080322                 (P1 > +11)
                       OR (PD-PROD-CODE (P1) = 'A'
080322                   AND PD-MAX-ATT-AGE (P1) >= WS-ATT-AGE )
                    END-PERFORM
080322              IF P1 < +12
                       MOVE PD-MAX-AMT (P1) TO CP-R-MAX-MON-BEN
                    END-IF
                 END-IF
              END-IF
              IF CP-R-MAX-MON-BEN = ZEROS
                 MULTIPLY CM-AH-BENEFIT-AMT BY CP-REMAINING-TERM-3
01075                                     GIVING EREMBENO
              ELSE
                 MULTIPLY CP-R-MAX-MON-BEN BY CP-REMAINING-TERM-3
01075                                     GIVING EREMBENO
              END-IF
           END-IF
01076
01077 ******************************************************************
01078 *                                                                *
01079 *            CALCULATE UNEARNED PREMIUM AMOUNT                   *
01080 *                      ONE MONTH EARNED PREMIUM AMOUNT           *
01081 *                                                                *
01082 ******************************************************************
01083
01084      MOVE '2'                    TO  CP-PROCESS-TYPE.
01085      MOVE CM-STATE               TO  CP-STATE.
01086      MOVE CM-RATE-CLASS          TO  CP-CLASS-CODE.
01087      MOVE CM-AH-DEV-CODE         TO  CP-DEVIATION-CODE.
01088      MOVE CM-INSURED-ISSUE-AGE   TO  CP-ISSUE-AGE.
01089      MOVE CM-AH-BENEFIT-AMT      TO  CP-ORIGINAL-BENEFIT
01090                                      CP-RATING-BENEFIT-AMT.
01091      MOVE CM-AH-PREMIUM-AMT      TO  CP-ORIGINAL-PREMIUM.
01092      MOVE CM-LOAN-APR            TO  CP-LOAN-APR.
01093      MOVE CM-LOAN-TERM           TO  CP-LOAN-TERM.
01094
01095      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
01096      MOVE '1'                    TO  WS-CFK-RECORD-TYPE.
01097      MOVE SPACES                 TO  WS-CFK-ACCESS.
01098      MOVE +0                     TO  WS-CFK-SEQUENCE-NO.
01099      PERFORM 3500-READ-CNTL THRU 3500-EXIT.
01100      MOVE CF-AH-OVERRIDE-L1      TO  CP-AH-OVERRIDE-CODE.
01101      MOVE CF-CR-R78-METHOD       TO  CP-R78-OPTION.
01102      MOVE PI-REM-TRM-CALC-OPTION TO  CP-REM-TRM-CALC-OPTION.
01103      MOVE CF-CR-REM-TERM-CALC    TO  CP-REM-TERM-METHOD.
01104
01105      MOVE '3'                    TO  WS-CFK-RECORD-TYPE.
01106      MOVE PI-AH-OVERRIDE-L1      TO  WS-BENEFIT-TYPE.
01107      MOVE CM-AH-BENEFIT-CD       TO  WS-BENEFIT-NO.
01108      MOVE SPACES                 TO  WS-CFK-ACCESS.
01109      MOVE CM-STATE               TO  WS-CFK-STATE.
01110      MOVE +0                     TO  WS-CFK-SEQUENCE-NO.
01111      PERFORM 3500-READ-CNTL THRU 3500-EXIT.
01112      MOVE CF-STATE-ABBREVIATION  TO  CP-STATE-STD-ABBRV.
01113      MOVE CF-ST-FREE-LOOK-PERIOD TO  CP-FREE-LOOK.
01114
01115      IF CP-STATE-STD-ABBRV = 'OR'
01116          COMPUTE CP-RATING-BENEFIT-AMT = CM-AH-BENEFIT-AMT *
01117                                          CM-AH-ORIG-TERM.
01118
01119 *    IF CF-TRUNCATED-LIFE (WS-INDEX)
01120 *        MOVE CM-PAY-FREQUENCY   TO  CP-PAY-FREQUENCY
01121 *        MOVE CF-CO-OVRD-EARNINGS-CALC (WS-INDEX) TO
01122 *                                           CP-EARNING-METHOD.
01123
01124      MOVE CM-LOAN-1ST-PMT-DT     TO  CP-FIRST-PAY-DATE.
01125
01126      IF WS-VALUATION-DT IS EQUAL TO LOW-VALUES
01127          MOVE PI-CR-MONTH-END-DT TO  CP-VALUATION-DT
01128                                      WS-VALUATION-DT
01129      ELSE
01130          MOVE WS-VALUATION-DT    TO  CP-VALUATION-DT.
01131
01132      PERFORM 9800-LINK-REM-TERM.
01133      MOVE CP-REMAINING-TERM-1    TO  CP-REMAINING-TERM.
01134      PERFORM 9850-LINK-REFUND-RTN THRU 9850-EXIT.
01135      MOVE CP-CALC-REFUND         TO  EUEPREMO
01136                                      WS-CALC-REFUND.
01137      ADD +1                      TO  CP-REMAINING-TERM.
01138      IF CP-REMAINING-TERM IS EQUAL TO CM-AH-ORIG-TERM
01139          COMPUTE ONE-MON-EARNED = CM-AH-PREMIUM-AMT -
01140                                                 CP-CALC-REFUND
01141      ELSE
01142          PERFORM 9850-LINK-REFUND-RTN THRU 9850-EXIT
01143          COMPUTE ONE-MON-EARNED = WS-CALC-REFUND -
01144                                                 CP-CALC-REFUND.
01145
01146      MOVE ONE-MON-EARNED         TO  EOMEARNO.
01147
01148      IF WS-VALUATION-DT IS NOT = LOW-VALUES AND SPACES
01149          MOVE SPACES                 TO  DC-OPTION-CODE
01150          MOVE WS-VALUATION-DT        TO  DC-BIN-DATE-1
01151          PERFORM 8500-DATE-CONVERSION
01152          MOVE AL-UANON               TO  EUEDTA
01153          MOVE DC-GREG-DATE-1-EDIT    TO  EUEDTO.
01154
01155 ******************************************************************
01156 *                                                                *
01157 *                     DISPLAY CANCEL DATE                        *
01158 *                             CANCEL EXIT DATE                   *
01159 *                             CANCEL EXIT STATUS                 *
01160 *                                                                *
01161 ******************************************************************
01162
01163      MOVE SPACES                       TO  EEXITDTO.
01164
01165      IF CM-AH-CANCEL-EXIT-DT IS NOT = LOW-VALUES AND SPACES
01166          MOVE SPACES                   TO  DC-OPTION-CODE
01167          MOVE CM-AH-CANCEL-EXIT-DT     TO  DC-BIN-DATE-1
01168          PERFORM 8500-DATE-CONVERSION
01169          IF NOT DATE-CONVERSION-ERROR
01170              MOVE DC-GREG-DATE-1-EDIT  TO  EEXITDTO
01171          ELSE
01172              MOVE SPACES               TO  EEXITDTO.
01173
01174      MOVE SPACES                       TO  ECANDTO.
01175
01176      IF CM-AH-CANCEL-DT IS NOT = LOW-VALUES AND SPACES
01177          MOVE SPACES                   TO  DC-OPTION-CODE
01178          MOVE CM-AH-CANCEL-DT          TO  DC-BIN-DATE-1
01179          PERFORM 8500-DATE-CONVERSION
01180          IF NOT DATE-CONVERSION-ERROR
01181              MOVE DC-GREG-DATE-1-EDIT  TO  ECANDTO
01182          ELSE
01183              MOVE SPACES               TO  ECANDTO.
01184
01185      MOVE SPACES                       TO  EEXITSTO.
01186
01187      IF CM-AH-STATUS-AT-CANCEL = ' '
01188          MOVE SPACES             TO  EEXITSTO.
01189      IF CM-AH-STATUS-AT-CANCEL EQUAL '1' OR '2' OR '4' OR '9'
01190          MOVE 'ACTIVE'           TO  EEXITSTO.
01191      IF CM-AH-STATUS-AT-CANCEL = '6'
01192          MOVE 'PREV SETTLE'      TO  EEXITSTO.
01193      IF CM-AH-STATUS-AT-CANCEL = '7'
01194          MOVE 'PREV DTH'         TO  EEXITSTO.
01195      IF CM-AH-STATUS-AT-CANCEL = '8'
01196          MOVE 'PREV CANCEL'      TO  EEXITSTO.
01197
01198 ******************************************************************
01199 *                                                                *
01200 *                 DISPLAY CLAIM SETTLEMENT EXIT DATE             *
01201 *                         CLAIM SETTLEMENT STATUS                *
01202 *                                                                *
01203 ******************************************************************
01204
01205      MOVE SPACES                            TO  EEXDATEO.
01206
01207      IF CM-AH-SETTLEMENT-EXIT-DT NOT = LOW-VALUES AND SPACE
01208         MOVE SPACES                    TO  DC-OPTION-CODE
01209         MOVE CM-AH-SETTLEMENT-EXIT-DT  TO  DC-BIN-DATE-1
01210         PERFORM 8500-DATE-CONVERSION
01211         IF NOT DATE-CONVERSION-ERROR
01212            MOVE DC-GREG-DATE-1-EDIT    TO  EEXDATEO
01213         ELSE
01214            MOVE SPACES                 TO  EEXDATEO.
01215
01216      MOVE SPACES                 TO  EEXSTATO.
01217
01218      IF CM-AH-STATUS-AT-SETTLEMENT = ' '
01219          MOVE SPACES             TO  EEXSTATO.
01220      IF CM-AH-STATUS-AT-SETTLEMENT EQUAL '1' OR '2' OR '4' OR '9'
01221          MOVE 'ACTIVE'           TO  EEXSTATO.
01222      IF CM-AH-STATUS-AT-SETTLEMENT = '6'
01223          MOVE 'PREV SETTLE'      TO  EEXSTATO.
01224      IF CM-AH-STATUS-AT-SETTLEMENT = '7'
01225          MOVE 'PREV DTH'         TO  EEXSTATO.
01226      IF CM-AH-STATUS-AT-SETTLEMENT = '8'
01227          MOVE 'PREV CAN'         TO  EEXSTATO.

092118     if crtt-not-read
092118        move cm-control-primary  to ws-elcrtt-key
092118        move 'C'                 to ws-cs-rec-type
092118        exec cics read
092118           dataset   ('ELCRTT')
092118           ridfld    (ws-elcrtt-key)
092118           into      (certificate-trailers)
092118           resp      (ws-response)
092118        end-exec
092118        if resp-normal
092118           set good-hit-on-trlr  to true
092118        else
092118           set crtt-not-found    to true
092118        end-if
092118     end-if
092118     if good-hit-on-trlr
092118        evaluate true
092118           when cs-ah-refund-method = '1'
092118              move 'R78'         to refmetho
092118           when cs-ah-refund-method = '2'
092118              move 'PRO'         to refmetho
092118           when cs-ah-refund-method = '3'
092118              move 'CALIF'       to refmetho
092118           when cs-ah-refund-method = '4'
092118              move 'FARM'        to refmetho
092118           when cs-ah-refund-method = '5'
092118              move 'NET'         to refmetho
092118           when cs-ah-refund-method = '6'
092118              move 'ANTIC'       to refmetho
092118           when cs-ah-refund-method = '8'
092118              move 'MEAN'        to refmetho
092118           when cs-ah-refund-method = '9'
092118              move 'SUMDI'       to refmetho
092118           when cs-ah-refund-method = 'G'
092118              move 'GAPNR'       to refmetho
092118           when cs-ah-refund-method = 'S'
092118              move 'GAPA'        to refmetho
092118           when cs-ah-refund-method = 'D'
092118              move 'DDF'         to refmetho
092118           when cs-ah-refund-method = 'I'
092118              move 'DDFIU'       to refmetho
092118           when cs-ah-refund-method = 'R'
092118              move 'REPO'        to refmetho
092118           when other
092118              move cs-ah-refund-method
092118                                 to refmetho
092118        end-evaluate
092118     end-if
01228
01229 ******************************************************************
01230 *                                                                *
01231 *                   DISPLAY EXTENSION DAYS                       *
01232 *                                                                *
01233 ******************************************************************
01234
01235      IF CM-PMT-EXTENSION-DAYS NUMERIC
01236          MOVE CM-PMT-EXTENSION-DAYS  TO  EEXTDAYO
01237      ELSE
01238          MOVE ZEROS                  TO  EEXTDAYO.
01239
01240 ******************************************************************
01241 *                                                                *
01242 *                   DISPLAY CEDED BENEFIT                        *
01243 *                                                                *
01244 ******************************************************************
01245
121802*    IF PI-COMPANY-ID EQUAL 'CVL' OR 'LGX'
121802*        IF CM-AH-CEDED-BENEFIT NUMERIC
121802*            MOVE CM-AH-CEDED-BENEFIT
121802*                                TO  ECEDBENO
121802*             ELSE
121802*            MOVE ZEROS          TO  ECEDBENO
121802*    ELSE
01253          MOVE AL-SADOF           TO  ECEDBENA, ECEDHDA.
01254
01255      GO TO 8100-SEND-INITIAL-MAP.
01256
01257      EJECT

       2500-GET-ERACCT.

           MOVE CM-CONTROL-PRIMARY (1:22)
                                       TO WS-ACCT-KEY

           MOVE ' '                    TO WS-ACCT-RECORD-SW
     
           EXEC CICS STARTBR
               DATASET  ('ERACCT')
               RIDFLD   (WS-ACCT-KEY)
               GTEQ
               RESP     (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              GO TO 2500-EXIT
           END-IF

           .     
       2500-READ-ERACCT-NEXT.
     
           EXEC CICS READNEXT
               DATASET  ('ERACCT')
               INTO     (ACCOUNT-MASTER)
               RIDFLD   (WS-ACCT-KEY)
               RESP     (WS-RESPONSE)
           END-EXEC
     
           IF NOT RESP-NORMAL
              GO TO 2500-EXIT
           END-IF     

           IF AM-CONTROL-PRIMARY (1:20) = CM-CONTROL-PRIMARY (1:20)
              IF (CM-CERT-EFF-DT < AM-EXPIRATION-DT)
                 AND (CM-CERT-EFF-DT >= AM-EFFECTIVE-DT)
                 SET ACCT-FOUND        TO TRUE
              ELSE
                 GO TO 2500-READ-ERACCT-NEXT
           END-IF

           .
       2500-EXIT.
           EXIT.


       2600-GET-MAX-BENEFIT.

           MOVE ' '                    TO WS-PDEF-RECORD-SW

           MOVE PI-COMPANY-CD          TO ERPDEF-KEY
           MOVE CM-STATE               TO ERPDEF-STATE
010816     if cm-clp-state not = cm-state and spaces and zeros
010816        move cm-clp-state        to erpdef-state
010816     end-if
           MOVE AM-DCC-PRODUCT-CODE    TO ERPDEF-PROD-CD
           MOVE 'A'                    TO ERPDEF-BEN-TYPE
           MOVE CM-AH-BENEFIT-CD       TO ERPDEF-BEN-CODE
           MOVE CM-CERT-EFF-DT         TO ERPDEF-EXP-DT
           MOVE ERPDEF-KEY             TO ERPDEF-KEY-SAVE

           EXEC CICS STARTBR
               DATASET  ('ERPDEF')
               RIDFLD   (ERPDEF-KEY)
               GTEQ
               RESP     (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              GO TO 2600-EXIT
           END-IF

           .
       2600-READNEXT.

           EXEC CICS READNEXT
              DATASET  ('ERPDEF')
              SET      (ADDRESS OF PRODUCT-MASTER)
              RIDFLD   (ERPDEF-KEY)
              RESP     (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              GO TO 2600-ENDBR
           END-IF

           IF (ERPDEF-KEY-SAVE (1:16) = PD-CONTROL-PRIMARY (1:16))
              IF (CM-CERT-EFF-DT < PD-PROD-EXP-DT)
                 MOVE 'Y'              TO WS-PDEF-RECORD-SW
              ELSE
                 GO TO 2600-READNEXT
              END-IF
           ELSE
              GO TO 2600-ENDBR
           END-IF

           .
       2600-ENDBR.

           EXEC CICS ENDBR
              DATASET  ('ERPDEF')
           END-EXEC

           .
       2600-EXIT.
           EXIT.

01258  3000-LOCATE-BENEFIT         SECTION.
01259
01260      EXEC CICS HANDLE CONDITION
01261              NOTFND  (3000-EXIT)
01262      END-EXEC.
01263
01264      MOVE SPACES                 TO  WS-KIND
01265                                      WS-CFK-ACCESS.
01266      MOVE ZERO                   TO  WS-NOT-FOUND.
01267      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
01268      MOVE WS-BENEFIT-NO          TO  WS-CFK-BENEFIT-NO.
01269
01270      EXEC CICS READ
01271          DATASET  (WS-CONTROL-FILE-DSID)
01272          RIDFLD   (WS-CONTROL-FILE-KEY)
01273          SET      (ADDRESS OF CONTROL-FILE)
01274          GTEQ
01275      END-EXEC.
01276
01277      IF WS-CFK-COMPANY-ID NOT = CF-COMPANY-ID OR
01278          WS-CFK-RECORD-TYPE NOT = CF-RECORD-TYPE
01279              GO TO 3000-EXIT.
01280
01281      MOVE +1                     TO  WS-INDEX.
01282
01283  3000-LOOKUP-BENEFIT.
01284
01285      IF WS-BENEFIT-NO = CF-BENEFIT-CODE (WS-INDEX)
01286          MOVE CF-BENEFIT-ALPHA (WS-INDEX)   TO  WS-KIND
01287          MOVE CF-SPECIAL-CALC-CD (WS-INDEX) TO  WS-CALC-CD
01288          MOVE CF-BENEFIT-DESCRIP (WS-INDEX) TO  WS-BENEFIT-DESCRIP
01289          MOVE CF-BENEFIT-COMMENT (WS-INDEX) TO  WS-COMMENT
01290          MOVE CF-BENEFIT-CODE    (WS-INDEX) TO  WS-BENEFIT-CODE
01291          MOVE CF-LF-COVERAGE-TYPE (WS-INDEX)         TO
01292                                              WS-LF-COVERAGE-TYPE
01293          MOVE CF-CO-EARNINGS-CALC (WS-INDEX)         TO
01294                                              WS-EARNINGS-CALC
01295          MOVE CF-CO-OVRD-EARNINGS-CALC (WS-INDEX)    TO
01296                                              WS-OVRD-EARNINGS-CALC
01297          MOVE +1                            TO  WS-NOT-FOUND
01298          GO TO 3000-EXIT.
01299
01300      IF CF-BENEFIT-CODE (WS-INDEX) NOT LESS CF-HI-BEN-IN-REC
01301          GO TO 3000-EXIT.
01302
01303      IF WS-INDEX LESS THAN +8
01304          ADD +1                  TO  WS-INDEX
01305          GO TO 3000-LOOKUP-BENEFIT.
01306
01307  3000-EXIT.
01308      EXIT.
01309
01310  3500-READ-CNTL                  SECTION.
01311
01312      EXEC CICS HANDLE CONDITION
01313              NOTFND  (3500-NOTFND)
01314      END-EXEC.
01315
01316      EXEC CICS READ
01317          DATASET  (WS-CONTROL-FILE-DSID)
01318          RIDFLD   (WS-CONTROL-FILE-KEY)
01319          SET      (ADDRESS OF CONTROL-FILE)
01320      END-EXEC.
01321
01322      IF WS-FREE-LOOK = 'Y'
01323          MOVE SPACE              TO WS-FREE-LOOK
01324          GO TO 3500-EXIT.
01325
01326      IF WS-CFK-RECORD-TYPE IS EQUAL TO '3'
01327          NEXT SENTENCE
01328      ELSE
01329          GO TO 3500-EXIT.
01330
01331      MOVE +1                     TO  WS-INDEX.
01332
01333  3500-FIND-ST-CALC-METHOD.
01334
01335      IF WS-BENEFIT-TYPE = CF-ST-BENEFIT-KIND (WS-INDEX)  AND
01336         WS-BENEFIT-NO = CF-ST-BENEFIT-CD (WS-INDEX)
01337          MOVE CF-ST-REM-TERM-CALC (WS-INDEX)    TO
01338                                      CP-REM-TERM-METHOD
01339          GO TO 3500-EXIT.
01340
01341      IF WS-INDEX LESS +50
01342          ADD +1                  TO  WS-INDEX
01343          GO TO 3500-FIND-ST-CALC-METHOD.
01344
01345  3500-NOTFND.
01346
01347      IF WS-FREE-LOOK = 'Y'
01348          MOVE SPACE              TO WS-FREE-LOOK
01349          MOVE ZERO               TO CF-ST-FREE-LOOK-PERIOD.
01350
01351  3500-EXIT.
01352      EXIT.
01353
01354      EJECT
01355  7500-DEEDIT.
01356      EXEC CICS BIF
01357           DEEDIT
01358           FIELD  (DEEDIT-FIELD)
01359           LENGTH (15)
01360      END-EXEC.
01361
01362  7500-EXIT.
01363      EXIT.
01364
01365  7600-CHECK-FOR-CLAIM            SECTION.
01366
01367      MOVE WS-CK-COMPANY-CD  TO  WS-CL-COMPANY-CD.
01368      MOVE WS-CK-CERT-NO     TO  WS-CL-CERT-NO.
01369
01370      EXEC CICS HANDLE CONDITION
01371          NOTFND (7600-CONTINUE)
01372          ENDFILE (7600-CONTINUE)
01373      END-EXEC.
01374
01375      EXEC CICS IGNORE CONDITION
01376          DUPKEY
01377      END-EXEC.
01378
01379      EXEC CICS STARTBR
01380          DATASET   (WS-CLAIM-MASTER-DSID)
01381          RIDFLD    (WS-CLAIM-KEY)
01382      END-EXEC.
01383
01384      MOVE 'Y'                    TO  WS-BROWSE-SW.
01385
01386  7600-NEXT-CLAIM.
01387
01388      EXEC CICS READNEXT
01389          DATASET   (WS-CLAIM-MASTER-DSID)
01390          RIDFLD    (WS-CLAIM-KEY)
01391          SET       (ADDRESS OF CLAIM-MASTER)
01392      END-EXEC.
01393
01394      IF WS-CK-COMPANY-CD NOT = WS-CL-COMPANY-CD  OR
01395         WS-CK-CERT-NO    NOT = WS-CL-CERT-NO
01396            GO TO 7600-CONTINUE.
01397
01398      IF CM-CARRIER     NOT = CL-CARRIER       OR
01399         CM-GROUPING    NOT = CL-CERT-GROUPING OR
01400         CM-STATE       NOT = CL-CERT-STATE    OR
01401         CM-ACCOUNT     NOT = CL-CERT-ACCOUNT  OR
01402         CM-CERT-EFF-DT NOT = CL-CERT-EFF-DT
01403            GO TO 7600-NEXT-CLAIM.
01404
01405      MOVE 'Y'                    TO  PI-CLAIM-SW.
01406
01407      IF CHECK-AH-CLAIM
121802        IF CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G'
052614         OR 'F'
080322         OR 'B' OR 'H'
01409            IF CLAIM-IS-OPEN
01410               MOVE 'OPEN'   TO ECLSTATO
01411            ELSE
01412               MOVE 'CLOSED' TO ECLSTATO
121802           END-IF
01413         ELSE
01414            GO TO 7600-NEXT-CLAIM
121802        END-IF
121802     END-IF
01415
01416      IF CHECK-LF-CLAIM
100518         IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
01418              IF CLAIM-IS-OPEN
01419                  MOVE 'OPEN'   TO ECLSTATO
01420              ELSE
01421                  MOVE 'CLOSED' TO ECLSTATO
01422          ELSE
01423              GO TO 7600-NEXT-CLAIM.
01424
01425  7600-CONTINUE.
01426
01427      IF WS-BROWSE-SW IS EQUAL TO 'Y'
01428          EXEC CICS ENDBR
01429              DATASET   (WS-CLAIM-MASTER-DSID)
01430          END-EXEC
01431      ELSE
01432          MOVE 'NO CLAIM'         TO  ECLSTATO.
01433
01434  7600-EXIT.
01435      EXIT.
01436
01437      EJECT
01438  8100-SEND-INITIAL-MAP           SECTION.
01439
01440      MOVE SAVE-DATE              TO  EDATEO.
01441      MOVE EIBTIME                TO  WS-TIME-WORK.
01442      MOVE WS-TIME                TO  ETIMEO.
101201     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101201     MOVE PI-PROCESSOR-ID        TO  USERIDO.
01443      MOVE EMI-MESSAGE-AREA (1)   TO  EERMSG1O.
01444      MOVE PI-MEMBER-CAPTION      TO  EMEMCAPO.
01445      MOVE -1                     TO  EPFKEYL.
01446
01447      IF PI-USES-PAID-TO
01448         MOVE 'PAID  TO  :'  TO EPTHRHDO.
01449
01450      IF CREDIT-SESSION
01451             AND
01452         PI-CLAIM-SW IS EQUAL TO 'Y'
01453          MOVE AL-SANON           TO  EPFKEY7A
01454      ELSE
01455          MOVE AL-SADOF           TO  EPFKEY7A.
01456
01457      EXEC CICS SEND
01458          FROM   (EL127EI)
01459          MAPSET (WS-MAPSET-NAME)
01460          MAP    (WS-MAP-NAME)
01461          CURSOR
01462          ERASE
01463      END-EXEC.
01464
01465      GO TO 9100-RETURN-TRAN.
01466
01467  8100-EXIT.
01468      EXIT.
01469
01470      EJECT
01471  8200-SEND-DATAONLY SECTION.
01472
01473      IF PI-1ST-TIME-SW = '1'
01474          GO TO 8100-SEND-INITIAL-MAP.
01475
01476      MOVE SAVE-DATE              TO  EDATEO.
01477      MOVE EIBTIME                TO  WS-TIME-WORK.
01478      MOVE WS-TIME                TO  ETIMEO.
101201     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101201     MOVE PI-PROCESSOR-ID        TO  USERIDO.
01479      MOVE EMI-MESSAGE-AREA (1)   TO  EERMSG1O.
01480      MOVE PI-MEMBER-CAPTION      TO  EMEMCAPO.
01481      MOVE -1                     TO  EPFKEYL.
01482
01483      IF PI-USES-PAID-TO
01484         MOVE 'PAID  TO  :'  TO EPTHRHDO.
01485
01486      IF CREDIT-SESSION
01487             AND
01488         PI-CLAIM-SW IS EQUAL TO 'Y'
01489          MOVE AL-SANON           TO  EPFKEY7A
01490      ELSE
01491          MOVE AL-SADOF           TO  EPFKEY7A.
01492
01493      EXEC CICS SEND DATAONLY
01494          FROM   (EL127EI)
01495          MAPSET (WS-MAPSET-NAME)
01496          MAP    (WS-MAP-NAME)
01497          CURSOR
01498      END-EXEC.
01499
01500      GO TO 9100-RETURN-TRAN.
01501
01502  8200-EXIT.
01503      EXIT.
01504
01505      EJECT
01506  8300-SEND-TEXT SECTION.
01507
01508      EXEC CICS SEND TEXT
01509          FROM   (LOGOFF-TEXT)
01510          LENGTH (LOGOFF-LENGTH)
01511          ERASE
01512          FREEKB
01513      END-EXEC.
01514
01515      EXEC CICS RETURN
01516      END-EXEC.
01517
01518  8300-EXIT.
01519      EXIT.
01520
01521      EJECT
01522  8500-DATE-CONVERSION SECTION.
01523
01524      EXEC CICS LINK
01525          PROGRAM  (ELDATCV)
01526          COMMAREA (DATE-CONVERSION-DATA)
01527          LENGTH   (DC-COMM-LENGTH)
01528      END-EXEC.
01529
01530  8500-EXIT.
01531      EXIT.
01532
01533  8880-NOT-FOUND SECTION.
01534
01535      MOVE -1                     TO  EPFKEYL.
01536      MOVE ER-0142                TO  EMI-ERROR.
01537      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01538      GO TO 8100-SEND-INITIAL-MAP.
01539
01540  8880-EXIT.
01541      EXIT.
01542
01543  9000-RETURN-CICS SECTION.
01544
01545      MOVE EL005                  TO  WS-PROGRAM-ID.
01546      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
01547      PERFORM 9300-XCTL.
01548
01549  9000-EXIT.
01550      EXIT.
01551
01552  9100-RETURN-TRAN SECTION.
01553
01554      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
01555      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.
01556
01557      EXEC CICS RETURN
01558          COMMAREA (PROGRAM-INTERFACE-BLOCK)
01559          LENGTH   (PI-COMM-LENGTH)
01560          TRANSID  (WS-TRANS-ID)
01561      END-EXEC.
01562
01563  9100-EXIT.
01564      EXIT.
01565
01566  9300-XCTL SECTION.
01567
01568      MOVE DFHENTER               TO  EIBAID.
01569
01570      EXEC CICS XCTL
01571          PROGRAM  (WS-PROGRAM-ID)
01572          COMMAREA (PROGRAM-INTERFACE-BLOCK)
01573          LENGTH   (PI-COMM-LENGTH)
01574      END-EXEC.
01575
01576  9300-EXIT.
01577      EXIT.
01578
01579      EJECT
01580  9400-CLEAR SECTION.
01581
01582      MOVE ' '                    TO PI-1ST-TIME-SW.
01583      MOVE PI-RETURN-TO-PROGRAM   TO  WS-PROGRAM-ID.
01584      GO TO 9300-XCTL.
01585
01586  9400-EXIT.
01587      EXIT.
01588
01589  9600-PGMIDERR SECTION.
01590
01591      EXEC CICS HANDLE CONDITION
01592          PGMIDERR (8300-SEND-TEXT)
01593      END-EXEC.
01594
01595      MOVE WS-PROGRAM-ID          TO  PI-CALLING-PROGRAM.
01596
01597      MOVE EL005                  TO  WS-PROGRAM-ID
01598                                      LOGOFF-PGM.
01599      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
01600      MOVE SPACES                 TO  PI-ENTRY-CD-1.
01601      GO TO 9300-XCTL.
01602
01603  9600-EXIT.
01604      EXIT.
01605
01606  9700-LINK-REM-AMT               SECTION.
01607
01608      EXEC CICS LINK
01609          PROGRAM   (ELRAMT)
01610          COMMAREA  (CALCULATION-PASS-AREA)
01611          LENGTH    (CP-COMM-LENGTH)
01612      END-EXEC.
01613
01614  9700-EXIT.
01615      EXIT.
01616
01617  9800-LINK-REM-TERM              SECTION.
01618
01619      EXEC CICS LINK
01620          PROGRAM   (ELRTRM)
01621          COMMAREA  (CALCULATION-PASS-AREA)
01622          LENGTH    (CP-COMM-LENGTH)
01623      END-EXEC.
01624
01625  9800-EXIT.
01626      EXIT.
01627
01628  9850-LINK-REFUND-RTN            SECTION.
01629
01630      EXEC CICS LINK
01631          PROGRAM   (ELRFND)
01632          COMMAREA  (CALCULATION-PASS-AREA)
01633          LENGTH    (CP-COMM-LENGTH)
01634      END-EXEC.
01635
01636      IF CP-RETURN-CODE NOT EQUAL ZEROS
01637          MOVE ZEROS TO CP-CALC-REFUND.
01638
01639  9850-EXIT.
01640      EXIT.
01641
01642
01643      EJECT
01644  9900-ERROR-FORMAT SECTION.
01645
01646      ADD +1                      TO  WS-ERROR-COUNT.
01647
01648      IF EMI-ERRORS-COMPLETE
01649          MOVE ZERO               TO  EMI-ERROR
01650          GO TO 9900-EXIT.
01651
01652      EXEC CICS LINK
01653          PROGRAM  (EL001)
01654          COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)
01655          LENGTH   (EMI-COMM-LENGTH)
01656      END-EXEC.
01657
01658      MOVE ZERO                   TO  EMI-ERROR.
01659
01660  9900-EXIT.
01661      EXIT.
01662
01663  9990-ERROR SECTION.
01664
01665      MOVE DFHEIBLK               TO  EMI-LINE1.
01666
01667      EXEC CICS LINK
01668          PROGRAM  (EL004)
01669          COMMAREA (EMI-LINE1)
01670          LENGTH   (72)
01671      END-EXEC.
01672
01673      GO TO 8100-SEND-INITIAL-MAP.
01674
01675  9990-EXIT.
01676      EXIT.
01677
01678  9999-LAST-PARAGRAPH SECTION.
01679
01680      GOBACK.

