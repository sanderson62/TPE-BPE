00001  ID DIVISION.                                                     00010000
00002                                                                   00020000
00003  PROGRAM-ID.                 EL1276.                              00030000
00004 *              PROGRAM CONVERTED BY                               00040000
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   00050000
00006 *              CONVERSION DATE 02/13/96 09:59:22.                 00060000
00007 *                            VMOD=2.004.                          00070000
00008 *                                                                 00080000
00008 *                                                                 00090000
00009 *AUTHOR.     LOGIC,INC.                                           00100000
00010 *            DALLAS, TEXAS.                                       00110000
00011                                                                   00120000
00012 *DATE-COMPILED.                                                   00130000
00013                                                                   00140000
00014 *SECURITY.   *****************************************************00150000
00015 *            *                                                   *00160000
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *00170000
00017 *            *                                                   *00180000
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *00190000
00019 *                                                                *00200000
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *00210000
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *00220000
00022 *            *                                                   *00230000
00023 *            *****************************************************00240000
00024 *                                                                 00250000
00025 *REMARKS.    TRANSACTION - EXX6 - CERTIFICATE AND BILLING NOTES.  00260000
00026 *                                                                 00270000
101201******************************************************************00280000
101201*                   C H A N G E   L O G                           00290000
101201*                                                                 00300000
101201* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.                00310000
101201*-----------------------------------------------------------------00320000
101201*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE            00330000
101201* EFFECTIVE    NUMBER                                             00340000
101201*-----------------------------------------------------------------00350000
101201* 101201    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID) 00360000
101201*                              ADJUSTED REDEFINES EL127FO FILLER  00370000
092309* 092309    2008100900003  AJRA  CERT NOTES MOVED TO NEW SCREEN
110811* 110811  CR2011110800002  PEMA CORRECT SECURITY ISSUE
041320* 041320  CR2020030500002  PEMA  Distinguish between iss and canc notes.
101201******************************************************************00380000
                                                                        00390000
00027  ENVIRONMENT DIVISION.                                            00400000
00028                                                                   00410000
00029      EJECT                                                        00420000
00030  DATA DIVISION.                                                   00430000
00031  WORKING-STORAGE SECTION.                                         00440000
00032  77  FILLER  PIC X(32)  VALUE '********************************'. 00450000
00033  77  FILLER  PIC X(32)  VALUE '*    EL1276 WORKING STORAGE    *'. 00460000
00034  77  FILLER  PIC X(32)  VALUE '************ V/M 2.004 *********'. 00470000
00035                                                                   00480000
00036      COPY ELCSCTM.                                                00490000
00037                                                                   00500000
00038      COPY ELCSCRTY.                                               00510000
00039                                                                   00520000
00040  01  WS-DATE-AREA.                                                00530000
00041      05  SAVE-DATE                   PIC X(8)    VALUE SPACES.    00540000
00042      05  SAVE-BIN-DATE               PIC XX      VALUE SPACES.    00550000
00043                                                                   00560000
00044  01  STANDARD-AREAS.                                              00570000
00045      12  SC-ITEM                     PIC S9(4)   VALUE +0001 COMP.00580000
00046      12  GETMAIN-SPACE               PIC X       VALUE SPACE.     00590000
00047      12  MAP-NAME                    PIC X(8)    VALUE 'EL127F'.  00600000
00048      12  MAPSET-NAME                 PIC X(8)    VALUE 'EL1276S'. 00610000
00049      12  SCREEN-NUMBER               PIC X(4)    VALUE '127F'.    00620000
00050      12  TRANS-ID                    PIC X(4)    VALUE 'EXX6'.    00630000
00051      12  THIS-PGM                    PIC X(8)    VALUE 'EL1276'.  00640000
00052      12  PGM-NAME                    PIC X(8).                    00650000
00053      12  TIME-IN                     PIC S9(7).                   00660000
00054      12  TIME-OUT-R  REDEFINES TIME-IN.                           00670000
00055          16  FILLER                  PIC X.                       00680000
00056          16  TIME-OUT                PIC 99V99.                   00690000
00057          16  FILLER                  PIC XX.                      00700000
00058      12  XCTL-005                    PIC X(8)    VALUE 'EL005'.   00710000
00059      12  XCTL-010                    PIC X(8)    VALUE 'EL010'.   00720000
00060      12  XCTL-626                    PIC X(8)    VALUE 'EL626'.   00730000
092309     12  PGM-1279                    PIC X(8)    VALUE 'EL1279'.
00061      12  LINK-001                    PIC X(8)    VALUE 'EL001'.   00740000
00062      12  LINK-004                    PIC X(8)    VALUE 'EL004'.   00750000
00063      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'. 00760000
00064      12  ERNOTE-ID                   PIC X(8)    VALUE 'ERNOTE'.  00770000
00065      12  ELCERT-ID                   PIC X(8)    VALUE 'ELCERT'.  00780000
00066      12  WS-RECORD-LENGTHS   COMP.                                00790000
00067          16  WS-ERNOTE-RECORD-LENGTH  PIC S9(4)  VALUE +825.      00800000
00068          16  WS-ELCERT-RECORD-LENGTH  PIC S9(4)  VALUE +450.      00810000
00069          16  WS-JOURNAL-RECORD-LENGTH PIC S9(4)  VALUE ZEROS.     00820000
00070                                                                   00830000
00071      12  DEEDIT-FIELD                PIC X(15).                   00840000
00072      12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD   PIC S9(15).     00850000
00073                                                                   00860000
00074      12  RETURN-FROM                 PIC X(8).                    00870000
00075      12  QID.                                                     00880000
00076          16  QID-TERM                PIC X(4).                    00890000
00077          16  FILLER                  PIC X(4)    VALUE '127F'.    00900000
00078      12  WS-SUB1                     PIC  S99  COMP.              00910000
00079      12  WS-NOTE-LINE-SW             PIC  X      VALUE SPACE.     00920000
00080          88  NOTE-LINES-PRESENT                  VALUE 'Y'.       00930000
00081      12  WS-RECORD-FOUND-SW          PIC  X      VALUE SPACE.     00940000
00082          88  RECORD-FOUND                        VALUE 'Y'.       00950000
00083          88  RECORD-NOT-FOUND                    VALUE 'N'.       00960000
00084      12  WS-DUPREC-SW                PIC  X      VALUE SPACE.     00970000
00085          88  DUPLICATE-RECORD-FOUND              VALUE 'Y'.       00980000
00086                                                                   00990000
00087      EJECT                                                        01000000
00088      12  ERROR-MESSAGES.                                          01010000
00089          16  ER-0000                 PIC  X(4)   VALUE '0000'.    01020000
00090          16  ER-0004                 PIC  X(4)   VALUE '0004'.    01030000
00091          16  ER-0008                 PIC  X(4)   VALUE '0008'.    01040000
00092          16  ER-0023                 PIC  X(4)   VALUE '0023'.    01050000
00093          16  ER-0029                 PIC  X(4)   VALUE '0029'.    01060000
00094          16  ER-0070                 PIC  X(4)   VALUE '0070'.    01070000
00095          16  ER-0142                 PIC  X(4)   VALUE '0142'.    01080000
00096          16  ER-2520                 PIC  X(4)   VALUE '2520'.    01090000
00097          16  ER-2521                 PIC  X(4)   VALUE '2521'.    01100000
00098          16  ER-2522                 PIC  X(4)   VALUE '2522'.    01110000
00099          16  ER-2523                 PIC  X(4)   VALUE '2523'.    01120000
00100          16  ER-2524                 PIC  X(4)   VALUE '2524'.    01130000
00101          16  ER-2525                 PIC  X(4)   VALUE '2525'.    01140000
00102          16  ER-2528                 PIC  X(4)   VALUE '2528'.    01150000
00103          16  ER-2706                 PIC  X(4)   VALUE '2706'.    01160000
00104                                                                   01170000
00105      EJECT                                                        01180000
00106                                                                   01190000
00107  01  WS-CM-CONTROL-PRIMARY.                                       01200000
00108      05  WS-CM-COMPANY-CD            PIC  X.                      01210000
00109      05  WS-CM-CARRIER               PIC  X.                      01220000
00110      05  WS-CM-GROUPING              PIC  X(6).                   01230000
00111      05  WS-CM-STATE                 PIC  XX.                     01240000
00112      05  WS-CM-ACCOUNT               PIC  X(10).                  01250000
00113      05  WS-CM-CERT-EFF-DT           PIC  XX.                     01260000
00114      05  WS-CM-CERT-NO.                                           01270000
00115          10  WS-CM-CERT-PRIME        PIC  X(10).                  01280000
00116          10  WS-CM-CERT-SFX          PIC  X.                      01290000
00117                                                                   01300000
041320 01  WS-CN-CONTROL-PRIMARY.                                       01200000
041320     05  WS-CN-COMPANY-CD            PIC  X.                      01210000
041320     05  WS-CN-CARRIER               PIC  X.                      01220000
041320     05  WS-CN-GROUPING              PIC  X(6).                   01230000
041320     05  WS-CN-STATE                 PIC  XX.                     01240000
041320     05  WS-CN-ACCOUNT               PIC  X(10).                  01250000
041320     05  WS-CN-CERT-EFF-DT           PIC  XX.                     01260000
041320     05  WS-CN-CERT-NO.                                           01270000
041320         10  WS-CN-CERT-PRIME        PIC  X(10).                  01280000
041320         10  WS-CN-CERT-SFX          PIC  X.
041320     05  WS-CN-RECORD-TYPE           PIC  X.

00118      COPY ELCDATE.                                                01310000
00119                                                                   01320000
00120      EJECT                                                        01330000
00121      COPY ELCLOGOF.                                               01340000
00122                                                                   01350000
00123      EJECT                                                        01360000
00124      COPY ELCATTR.                                                01370000
00125                                                                   01380000
00126      EJECT                                                        01390000
00127      COPY ELCEMIB.                                                01400000
00128                                                                   01410000
00129      EJECT                                                        01420000
00130      COPY ELCINTF.                                                01430000
00161      12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.                
092309         16  FILLER              PIC X(318).                      
041320         16  PI-TOTAL-LINES      PIC S9(3).
041320         16  PI-CURRENT-LINE     PIC S9(3)   COMP-3.
041320         16  PI-TEMP-STOR-ITEMS  PIC S9(4)   COMP.
041320         16  PI-UPDATE-SW        PIC X.
041320             88  PI-CHANGES-MADE             VALUE '1'.
041320         16  PI-PF5-PRESSED      PIC X.
041320             88 PF5-PRESSED                  VALUE 'Y'.
041320         16  PI-PF6-PRESSED      PIC X.
041320             88 PF6-PRESSED                  VALUE 'Y'.
092309         16  PI-CURR-NOTE-REC-TYPE PIC X.
092309             88 PI-CERT-NOTE                 VALUE '1'.
092309             88 PI-CLAIM-NOTE                VALUE '2'.
092309         16  PI-BILLING-NOTES-EXIST PIC X.
092309         16  PI-CERT-NOTES-EXIST    PIC X.
092309         16  PI-CLAIM-NOTES-EXIST   PIC X.
041320         16  filler                 pic x.
041320         16  pi-iss-can-trans    pic x.
041320             88  pi-from-issue     value '1'.
041320             88  pi-from-cancel    value '2'.
041320         16  pi-active-notes        pic x.
041320             88  pi-issue-notes       value '1'.
041320             88  pi-cancel-notes      value '2'.

00131                                                                   01440000
00134      COPY ELCJPFX.                                                01470000
00135                                      PIC X(523).                  01480000
00136      EJECT                                                        01490000
00137                                                                   01500000
00138      COPY ELCAID.                                                 01510000
00139  01  FILLER    REDEFINES DFHAID.                                  01520000
00140      12  FILLER                      PIC X(8).                    01530000
00141      12  PF-VALUES                   PIC X       OCCURS 24 TIMES. 01540000
00142                                                                   01550000
00143      EJECT                                                        01560000
00144      COPY EL1276S.                                                01570000
00145                                                                   01580000
00146  01  FILLER REDEFINES EL127FO.                                    01590000
041320     05  FILLER                      PIC X(183).                  01600000
00148      05  DNOTE-LINES   OCCURS 10 TIMES.                           01610000
00149          10  DNOTE-LINE-LENGTH       PIC S9(4) COMP.              01620000
00150          10  DNOTE-LINE-ATTRB        PIC X.                       01630000
00151          10  DNOTE-LINE              PIC X(77).                   01640000
041320     05  FILLER                      PIC X(104).                  01650000
00153                                                                   01660000
00154      EJECT                                                        01670000
00155                                                                   01680000
00156  LINKAGE SECTION.                                                 01690000
00157  01  DFHCOMMAREA                     PIC X(1024).                 01700000
00158                                                                   01710000
00159      EJECT                                                        01720000
00160                                                                   01730000
00161      COPY ERCNOTE.
00162                                                                   01750000
00163      EJECT                                                        01760000
00164                                                                   01770000
00165      COPY ELCCERT.                                                01780000
00166                                                                   01790000
00167      EJECT                                                        01800000
00168                                                                   01810000
00169  PROCEDURE DIVISION.                                              01820000
00170                                                                   01830000
00171      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             01840000
00172      MOVE '5'                    TO  DC-OPTION-CODE.              01850000
00173      PERFORM 9700-DATE-LINK.                                      01860000
00174      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   01870000
00175      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               01880000
00176                                                                   01890000
00177      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     01900000
00178      MOVE 1                      TO  EMI-NUMBER-OF-LINES.         01910000
00179      MOVE EIBTRMID               TO  QID-TERM.                    01920000
00180                                                                   01930000
00181      IF EIBCALEN = 0                                              01940000
00182          GO TO 8800-UNAUTHORIZED-ACCESS.                          01950000
00183                                                                   01960000
092309     IF PI-CALLING-PROGRAM = PGM-1279
041320         move pi-iss-can-trans   to pi-active-notes
092309         MOVE THIS-PGM           TO PI-CALLING-PROGRAM
092309     END-IF.
092309     
00184      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         01970000
00185          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   01980000
00186              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      01990000
00187              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      02000000
00188              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      02010000
00189              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      02020000
00190              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      02030000
00191              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      02040000
00192              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    02050000
00193              MOVE THIS-PGM TO PI-CALLING-PROGRAM                  02060000
00194          ELSE                                                     02070000
00195              MOVE PI-CALLING-PROGRAM   TO RETURN-FROM             02080000
00196              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      02090000
00197              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    02100000
00198              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      02110000
00199              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      02120000
00200              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      02130000
00201              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      02140000
00202              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      02150000
00203              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     02160000
00204                                                                   02170000
092309     EXEC CICS HANDLE AID                                            CL**4
092309          CLEAR(9400-CLEAR)                                          CL**4
092309     END-EXEC.                                                       CL**4
092309*     IF EIBAID = DFHCLEAR                                         02180000
092309*         GO TO 9400-CLEAR.                                        02190000
00207                                                                   02200000
00208      IF PI-PROCESSOR-ID = 'LGXX'                                  02210000
00209          NEXT SENTENCE                                            02220000
00210      ELSE                                                         02230000
00211          EXEC CICS READQ TS                                       02240000
00212              QUEUE   (PI-SECURITY-TEMP-STORE-ID)                  02250000
00213              INTO    (SECURITY-CONTROL)                           02260000
00214              LENGTH  (SC-COMM-LENGTH)                             02270000
00215              ITEM    (SC-ITEM)                                    02280000
00216          END-EXEC                                                 02290000
00217          MOVE SC-CREDIT-DISPLAY (32)   TO  PI-DISPLAY-CAP         02300000
00218          MOVE SC-CREDIT-UPDATE  (32)   TO  PI-MODIFY-CAP          02310000
00219          IF NOT DISPLAY-CAP                                       02320000
00220              MOVE 'READ'               TO  SM-READ                02330000
00221              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT       02340000
00222              MOVE ER-0070              TO  EMI-ERROR              02350000
00223              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             02360000
00224              GO TO 8100-SEND-INITIAL-MAP.                         02370000
00225                                                                   02380000
00226      IF  EIBTRNID NOT = TRANS-ID                                  02390000
00227          MOVE LOW-VALUES         TO  EL127FI                      02400000
00228          PERFORM 7000-FORMAT-SCREEN                               02410000
00229          GO TO 8100-SEND-INITIAL-MAP.                             02420000
00237                                                                   02500000
00238      EXEC CICS    HANDLE    CONDITION                             02510000
00239           PGMIDERR          (9600-PGMID-ERROR)                    02520000
00240           ERROR             (9990-ABEND)                          02530000
00241           END-EXEC.                                               02540000
00242                                                                   02550000
00243      EJECT                                                        02560000
00244                                                                   02570000
00245  0200-RECEIVE.                                                    02580000
00246      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       02590000
00247          MOVE ER-0008            TO  EMI-ERROR                    02600000
00248          PERFORM 9900-ERROR-FORMAT                                02610000
00249          MOVE -1                 TO  FMAINTL                      02620000
00250          GO TO 8200-SEND-DATAONLY.                                02630000
00251                                                                   02640000
00252      EXEC CICS RECEIVE                                            02650000
00253          MAP      (MAP-NAME)                                      02660000
00254          MAPSET   (MAPSET-NAME)                                   02670000
00255          INTO     (EL127FI)                                       02680000
00256      END-EXEC.                                                    02690000
00257                                                                   02700000
00258      IF FPFENTRL = 0                                              02710000
00259          GO TO 0300-CHECK-PFKEYS.                                 02720000
00260                                                                   02730000
00261      IF EIBAID NOT = DFHENTER                                     02740000
00262          MOVE ER-0004            TO  EMI-ERROR                    02750000
00263          GO TO 0320-INPUT-ERROR.                                  02760000
00264                                                                   02770000
00265      IF  FPFENTRI NUMERIC                                         02780000
00266          IF  FPFENTRI = '12' OR '23' OR '24'                      02790000
00267              MOVE PF-VALUES (FPFENTRI)   TO  EIBAID               02800000
00268          ELSE                                                     02810000
00269              MOVE ER-0029                TO  EMI-ERROR            02820000
00270              GO TO 0320-INPUT-ERROR.                              02830000
00271                                                                   02840000
00272  0300-CHECK-PFKEYS.                                               02850000
00273      IF EIBAID = DFHPF23                                          02860000
00274          GO TO 8810-PF23.                                         02870000
00275                                                                   02880000
00276      IF EIBAID = DFHPF24                                          02890000
00277          GO TO 9200-RETURN-MAIN-MENU.                             02900000
00278                                                                   02910000
00279      IF EIBAID = DFHPF12                                          02920000
00280          GO TO 9500-PF12.                                         02930000
00281                                                                   02940000
092309     IF EIBAID = DFHPF5
092309         MOVE PGM-1279         TO PGM-NAME
092309         SET PI-CERT-NOTE     TO TRUE
092309         GO TO 9300-XCTL
092309     END-IF.
092309     
092309     IF EIBAID = DFHPF6
092309         MOVE PGM-1279         TO PGM-NAME
092309         SET PI-CLAIM-NOTE     TO TRUE
092309         GO TO 9300-XCTL
092309     END-IF.

041320     if eibaid = dfhpf7
041320        if pi-cancel-notes
041320           set pi-issue-notes to true
041320        else
041320           set pi-cancel-notes to true
041320        end-if
041320        perform 7000-format-screen
041320        go to 8100-send-initial-map
041320     end-if

00282      IF EIBAID = DFHENTER                                         02950000
00283          GO TO 400-EDIT-INPUT-DATA.                               02960000
00284                                                                   02970000
00285      MOVE ER-0029                TO  EMI-ERROR.                   02980000
00286                                                                   02990000
00287  0320-INPUT-ERROR.                                                03000000
00288      PERFORM 9900-ERROR-FORMAT.                                   03010000
00289      MOVE AL-UNBON               TO  FPFENTRA.                    03020000
00290                                                                   03030000
00291      IF FPFENTRL = 0                                              03040000
00292          MOVE -1                 TO  FMAINTL                      03050000
00293      ELSE                                                         03060000
00294          MOVE -1                 TO  FPFENTRL.                    03070000
00295                                                                   03080000
00296      GO TO 8200-SEND-DATAONLY.                                    03090000
00297                                                                   03100000
00298      EJECT                                                        03110000
00299                                                                   03120000
00300  400-EDIT-INPUT-DATA.                                             03130000
00301      IF FMAINTI = 'A' OR 'C' OR 'D'                               03140000
00302          NEXT SENTENCE                                            03150000
00303      ELSE                                                         03160000
00304          MOVE ER-0023            TO  EMI-ERROR                    03170000
00305          MOVE -1                 TO  FMAINTL                      03180000
00306          MOVE AL-UABON           TO  FMAINTA                      03190000
00307          PERFORM 9900-ERROR-FORMAT                                03200000
00308          GO TO 8200-SEND-DATAONLY.                                03210000

110811     IF NOT MODIFY-CAP
110811        MOVE 'UPDATE'            TO SM-READ
110811        PERFORM 9995-SECURITY-VIOLATION
110811                                 THRU 9995-EXIT
110811        MOVE ER-0070             TO EMI-ERROR
110811        PERFORM 9900-ERROR-FORMAT
110811                                 THRU 9900-EXIT
110811        GO TO 8100-SEND-INITIAL-MAP
110811     END-IF

00310      IF FMAINTI = 'D'                                             03230000
00311          GO TO 410-CHECK-ERRORS.                                  03240000
00312                                                                   03250000
00313      IF FBSTARTL GREATER THAN ZERO                                03260000
00314         IF FBSTARTI NOT NUMERIC                                   03270000
00315             MOVE ER-2520         TO  EMI-ERROR                    03280000
00316             MOVE AL-UNBON        TO  FBSTARTA                     03290000
00317             MOVE -1              TO  FBSTARTL                     03300000
00318             PERFORM 9900-ERROR-FORMAT                             03310000
00319         ELSE                                                      03320000
00320             IF FBSTARTI LESS '01' OR GREATER '10'                 03330000
00321                 MOVE ER-2520     TO  EMI-ERROR                    03340000
00322                 MOVE -1          TO  FBSTARTL                     03350000
00323                 PERFORM 9900-ERROR-FORMAT.                        03360000
00324                                                                   03370000
00325      IF FBENDL GREATER THAN ZERO                                  03380000
00326         IF FBENDI NOT NUMERIC                                     03390000
00327             MOVE ER-2521         TO  EMI-ERROR                    03400000
00328             MOVE AL-UNBON        TO  FBENDA                       03410000
00329             MOVE -1              TO  FBENDL                       03420000
00330             PERFORM 9900-ERROR-FORMAT                             03430000
00331         ELSE                                                      03440000
00332             IF FBENDI LESS '01' OR GREATER '10'                   03450000
00333                 MOVE ER-2521     TO  EMI-ERROR                    03460000
00334                 MOVE -1          TO  FBENDL                       03470000
00335                 PERFORM 9900-ERROR-FORMAT.                        03480000
00336                                                                   03490000
00337      IF FBENDL GREATER THAN ZEROS                                 03500000
00338         IF FBSTARTL = ZEROS                                       03510000
00339             MOVE ER-2524         TO  EMI-ERROR                    03520000
00340             MOVE -1              TO  FBSTARTL                     03530000
00341             PERFORM 9900-ERROR-FORMAT                             03540000
00342             GO TO 410-CHECK-ERRORS.                               03550000
00343                                                                   03560000
00344      IF FBSTARTL = ZERO OR                                        03570000
00345         FBENDL   = ZERO                                           03580000
00346            GO TO 410-CHECK-ERRORS.                                03590000
00347                                                                   03600000
00348      IF FBSTARTI GREATER THAN FBENDI                              03610000
00349          MOVE ER-2522            TO  EMI-ERROR                    03620000
00350          MOVE -1                 TO  FBSTARTL                     03630000
00351          PERFORM 9900-ERROR-FORMAT.                               03640000
00352                                                                   03650000
00353  410-CHECK-ERRORS.                                                03660000
00354      IF EMI-ERROR = ZEROS                                         03670000
00355         NEXT SENTENCE                                             03680000
00356      ELSE                                                         03690000
00357          GO TO 8200-SEND-DATAONLY.                                03700000
00358                                                                   03710000
00359      IF FMAINTI = 'A'                                             03720000
00360          GO TO 1000-ADD-RECORD.                                   03730000
00361                                                                   03740000
00362      IF FMAINTI = 'C'                                             03750000
00363          GO TO 2000-CHANGE-RECORD.                                03760000
00364                                                                   03770000
00365      GO TO 3000-DELETE-RECORD.                                    03780000
00366                                                                   03790000
00367      EJECT                                                        03800000
00368                                                                   03810000
00369  1000-ADD-RECORD       SECTION.                                   03820000
00370                                                                   03830000
00371      EXEC CICS GETMAIN                                            03840000
00372          SET      (ADDRESS OF CERTIFICATE-NOTE)                   03850000
00373          LENGTH   (825)                                           03860000
00374          INITIMG  (GETMAIN-SPACE)                                 03870000
00375      END-EXEC.                                                    03880000
00376                                                                   03890000
00377      MOVE  SPACES                TO  CERTIFICATE-NOTE.            03900000
00378                                                                   03910000
00379      MOVE  'CN'                  TO  CN-RECORD-ID.                03920000
00380                                                                   03930000
041320     MOVE  PI-CARRIER            TO  WS-CN-CARRIER
041320     MOVE  PI-GROUPING           TO  WS-CN-GROUPING
041320     MOVE  PI-STATE              TO  WS-CN-STATE
041320     MOVE  PI-ACCOUNT            TO  WS-CN-ACCOUNT
041320     MOVE  PI-CERT-PRIME         TO  WS-CN-CERT-PRIME
041320     MOVE  PI-CERT-SFX           TO  WS-CN-CERT-SFX
041320     MOVE  PI-CERT-EFF-DT        TO  WS-CN-CERT-EFF-DT
041320     MOVE  PI-COMPANY-CD         TO  WS-CN-COMPANY-CD
041320     IF PI-CANCEL-NOTES
041320        MOVE '2'                 TO  WS-CN-RECORD-TYPE
041320     ELSE
041320        MOVE '1'                 TO  WS-CN-RECORD-TYPE
041320     END-IF
041320
041320     MOVE  WS-CN-CONTROL-PRIMARY TO  CN-CONTROL-PRIMARY
041320                                     ws-cm-control-primary
00391                                                                   04040000
00392      IF FBSTARTL GREATER ZERO                                     04050000
00393          MOVE FBSTARTI           TO  CN-BILLING-START-LINE-NO     04060000
00394      ELSE                                                         04070000
00395          MOVE ZEROS              TO  CN-BILLING-START-LINE-NO.    04080000
00396                                                                   04090000
00397      IF FBENDL GREATER ZERO                                       04100000
00398          MOVE FBENDI             TO  CN-BILLING-END-LINE-NO       04110000
00399      ELSE                                                         04120000
00400          MOVE ZEROS              TO  CN-BILLING-END-LINE-NO.      04130000
00401                                                                   04140000
00402      IF FBENDL = ZEROS                                            04150000
00403          IF FBSTARTL GREATER ZERO                                 04160000
00404             MOVE FBSTARTI        TO  CN-BILLING-END-LINE-NO.      04170000
00405                                                                   04180000
00406      MOVE +0                     TO  WS-SUB1.                     04190000
00407                                                                   04200000
00408  1100-BUILD-NOTE-LINES.                                           04210000
00409      ADD +1                      TO  WS-SUB1.                     04220000
00410                                                                   04230000
00411      IF WS-SUB1 GREATER +10                                       04240000
00412         GO TO 1700-WRITE-RECORD.                                  04250000
00413                                                                   04260000
00414      IF DNOTE-LINE-LENGTH (WS-SUB1) GREATER ZERO                  04270000
00415          MOVE DNOTE-LINE   (WS-SUB1) TO  CN-LINE (WS-SUB1)        04280000
00416          MOVE 'Y'                    TO  WS-NOTE-LINE-SW          04290000
00417        ELSE                                                       04300000
00418          MOVE SPACES                 TO  CN-LINE (WS-SUB1).       04310000
00419                                                                   04320000
00420      GO TO 1100-BUILD-NOTE-LINES.                                 04330000
00421                                                                   04340000
00422  1700-WRITE-RECORD.                                               04350000
00423      IF NOTE-LINES-PRESENT                                        04360000
00424          NEXT SENTENCE                                            04370000
00425      ELSE                                                         04380000
00426          MOVE ER-2523            TO  EMI-ERROR                    04390000
00427          MOVE -1                 TO  FMAINTL                      04400000
00428          PERFORM 9900-ERROR-FORMAT                                04410000
00429          GO TO 8200-SEND-DATAONLY.                                04420000
00430                                                                   04430000
00431      MOVE PI-PROCESSOR-ID        TO  CN-LAST-MAINT-USER.          04440000
00432      MOVE EIBTIME                TO  CN-LAST-MAINT-HHMMSS.        04450000
00433      MOVE SAVE-BIN-DATE          TO  CN-LAST-MAINT-DT.            04460000
00434                                                                   04470000
00435      MOVE 'A'                    TO  JP-RECORD-TYPE.              04480000
00436      MOVE CERTIFICATE-NOTE       TO  JP-RECORD-AREA.              04490000
00437                                                                   04500000
00438      PERFORM 6400-WRITE-NOTE-RECORD.                              04510000
00439                                                                   04520000
00440      IF DUPLICATE-RECORD-FOUND                                    04530000
00441          GO TO 1800-DUPLICATE-RECORD.                             04540000
00442                                                                   04550000
00443      COMPUTE WS-JOURNAL-RECORD-LENGTH =                           04560000
00444              WS-ERNOTE-RECORD-LENGTH  +  23.                      04570000
00445                                                                   04580000
00446      PERFORM 8400-LOG-JOURNAL-RECORD.                             04590000
00447      MOVE ER-0000                TO  EMI-ERROR.                   04600000
00448      PERFORM 9900-ERROR-FORMAT.                                   04610000
00449      PERFORM 7000-FORMAT-SCREEN.                                  04620000
00450      PERFORM 4000-CERTIFICATE-UPDATE.                             04630000
00451      GO TO 8100-SEND-INITIAL-MAP.                                 04640000
00452                                                                   04650000
00453  1800-DUPLICATE-RECORD.                                           04660000
00454      MOVE ER-2525                TO  EMI-ERROR.                   04670000
00455      PERFORM 9900-ERROR-FORMAT.                                   04680000
00456      PERFORM 7000-FORMAT-SCREEN.                                  04690000
00457      GO TO 8100-SEND-INITIAL-MAP.                                 04700000
00458                                                                   04710000
00459  1900-EXIT.                                                       04720000
00460      EXIT.                                                        04730000
00461                                                                   04740000
00462      EJECT                                                        04750000
00463                                                                   04760000
00464  2000-CHANGE-RECORD   SECTION.                                    04770000
00465                                                                   04780000
041320     MOVE  PI-CARRIER            TO  WS-CN-CARRIER
041320     MOVE  PI-GROUPING           TO  WS-CN-GROUPING
041320     MOVE  PI-STATE              TO  WS-CN-STATE
041320     MOVE  PI-ACCOUNT            TO  WS-CN-ACCOUNT
041320     MOVE  PI-CERT-PRIME         TO  WS-CN-CERT-PRIME
041320     MOVE  PI-CERT-SFX           TO  WS-CN-CERT-SFX
041320     MOVE  PI-CERT-EFF-DT        TO  WS-CN-CERT-EFF-DT
041320     MOVE  PI-COMPANY-CD         TO  WS-CN-COMPANY-CD
041320     IF PI-CANCEL-NOTEs
041320        MOVE '2'                 TO  WS-CN-RECORD-TYPE
041320     ELSE
041320        MOVE '1'                 TO  WS-CN-RECORD-TYPE
041320     END-IF
041320
041320     MOVE WS-CN-CONTROL-PRIMARY  TO WS-CM-CONTROL-PRIMARY
00475      PERFORM 6300-READ-NOTE-FILE-UPDT
00476                                                                   04890000
00477      IF RECORD-NOT-FOUND                                          04900000
00478          GO TO 1000-ADD-RECORD.                                   04910000
00479                                                                   04920000
00480      MOVE 'B'                    TO  JP-RECORD-TYPE.              04930000
00481      MOVE CERTIFICATE-NOTE       TO  JP-RECORD-AREA.              04940000
00482                                                                   04950000
00483      COMPUTE WS-JOURNAL-RECORD-LENGTH =                           04960000
00484              WS-ERNOTE-RECORD-LENGTH  +  23.                      04970000
00485                                                                   04980000
00486      PERFORM 8400-LOG-JOURNAL-RECORD.                             04990000
00487                                                                   05000000
00488      IF FBSTARTL GREATER ZERO                                     05010000
00489          MOVE FBSTARTI           TO  CN-BILLING-START-LINE-NO     05020000
00490      ELSE                                                         05030000
00491          MOVE ZEROS              TO  CN-BILLING-START-LINE-NO.    05040000
00492                                                                   05050000
00493      IF FBENDL GREATER ZERO                                       05060000
00494          MOVE FBENDI             TO  CN-BILLING-END-LINE-NO       05070000
00495      ELSE                                                         05080000
00496          MOVE ZEROS              TO  CN-BILLING-END-LINE-NO.      05090000
00497                                                                   05100000
00498      MOVE +0                     TO  WS-SUB1.                     05110000
00499                                                                   05120000
00500  2100-BUILD-NOTE-LINES.                                           05130000
00501      ADD +1                      TO  WS-SUB1.                     05140000
00502                                                                   05150000
00503      IF WS-SUB1 GREATER 10                                        05160000
00504         GO TO 2700-REWRITE-RECORD.                                05170000
00505                                                                   05180000
00506      IF DNOTE-LINE-LENGTH (WS-SUB1) GREATER ZERO                  05190000
00507          MOVE DNOTE-LINE   (WS-SUB1) TO  CN-LINE (WS-SUB1)        05200000
00508          MOVE 'Y'                    TO  WS-NOTE-LINE-SW          05210000
00509      ELSE                                                         05220000
00510          MOVE SPACES                 TO  CN-LINE (WS-SUB1).       05230000
00511                                                                   05240000
00512      GO TO 2100-BUILD-NOTE-LINES.                                 05250000
00513                                                                   05260000
00514  2700-REWRITE-RECORD.                                             05270000
00515      IF NOTE-LINES-PRESENT                                        05280000
00516          NEXT SENTENCE                                            05290000
00517      ELSE                                                         05300000
00518          MOVE ER-2523            TO  EMI-ERROR                    05310000
00519          MOVE -1                 TO  FMAINTL                      05320000
00520          PERFORM 9900-ERROR-FORMAT                                05330000
00521          GO TO 8200-SEND-DATAONLY.                                05340000
00522                                                                   05350000
00523      MOVE PI-PROCESSOR-ID        TO  CN-LAST-MAINT-USER.          05360000
00524      MOVE EIBTIME                TO  CN-LAST-MAINT-HHMMSS.        05370000
00525      MOVE SAVE-BIN-DATE          TO  CN-LAST-MAINT-DT.            05380000
00526                                                                   05390000
00527      MOVE 'C'                    TO  JP-RECORD-TYPE.              05400000
00528      MOVE CERTIFICATE-NOTE       TO  JP-RECORD-AREA.              05410000
00529                                                                   05420000
00530      PERFORM 6500-REWRITE-NOTE-RECORD.                            05430000
00531                                                                   05440000
00532      COMPUTE WS-JOURNAL-RECORD-LENGTH =                           05450000
00533              WS-ERNOTE-RECORD-LENGTH  +  23.                      05460000
00534                                                                   05470000
00535      PERFORM 8400-LOG-JOURNAL-RECORD.                             05480000
00536                                                                   05490000
00537      MOVE ER-0000                TO  EMI-ERROR.                   05500000
00538      PERFORM 9900-ERROR-FORMAT.                                   05510000
00539      PERFORM 7000-FORMAT-SCREEN.                                  05520000
00540      PERFORM 4000-CERTIFICATE-UPDATE.                             05530000
00541      GO TO 8100-SEND-INITIAL-MAP.                                 05540000
00542                                                                   05550000
00543  2900-EXIT.                                                       05560000
00544      EXIT.                                                        05570000
00545                                                                   05580000
00546      EJECT                                                        05590000
00547                                                                   05600000
00548  3000-DELETE-RECORD    SECTION.                                   05610000
00549                                                                   05620000
00550      MOVE  PI-CARRIER            TO  WS-CN-CARRIER.               05630000
00551      MOVE  PI-GROUPING           TO  WS-CN-GROUPING.              05640000
00552      MOVE  PI-STATE              TO  WS-CN-STATE.                 05650000
00553      MOVE  PI-ACCOUNT            TO  WS-CN-ACCOUNT.               05660000
00554      MOVE  PI-CERT-PRIME         TO  WS-CN-CERT-PRIME.            05670000
00555      MOVE  PI-CERT-SFX           TO  WS-CN-CERT-SFX.              05680000
00556      MOVE  PI-CERT-EFF-DT        TO  WS-CN-CERT-EFF-DT.           05690000
00557      MOVE  PI-COMPANY-CD         TO  WS-CN-COMPANY-CD.            05700000
           IF PI-CANCEL-NOTEs
              MOVE '2'                 TO  WS-CN-RECORD-TYPE
           ELSE
              MOVE '1'                 TO  WS-CN-RECORD-TYPE
           END-IF
           MOVE WS-CN-CONTROL-PRIMARY  TO WS-CM-CONTROL-PRIMARY
00559      PERFORM 6300-READ-NOTE-FILE-UPDT.                            05720000
00560                                                                   05730000
00561      IF RECORD-NOT-FOUND                                          05740000
00562         GO TO 3600-RECORD-PREVS-DELETED.                          05750000
00563                                                                   05760000
00564      MOVE 'D'                    TO  JP-RECORD-TYPE.              05770000
00565      MOVE CERTIFICATE-NOTE       TO  JP-RECORD-AREA.              05780000
00566                                                                   05790000
00567      PERFORM 6600-DELETE-NOTE-RECORD.                             05800000
00568                                                                   05810000
00569      COMPUTE WS-JOURNAL-RECORD-LENGTH =                           05820000
00570              WS-ERNOTE-RECORD-LENGTH  +  23.                      05830000
00571                                                                   05840000
00572      PERFORM 8400-LOG-JOURNAL-RECORD.                             05850000
00573      PERFORM 4000-CERTIFICATE-UPDATE.                             05860000
00574                                                                   05870000
00575  3500-RECORD-DELETED.                                             05880000
00576      MOVE SPACE                  TO  FMAINTO.                     05890000
00577      MOVE ER-0000                TO  EMI-ERROR.                   05900000
00578      MOVE -1                     TO  FMAINTL.                     05910000
00579      PERFORM 9900-ERROR-FORMAT.                                   05920000
00580      GO TO 8200-SEND-DATAONLY.                                    05930000
00581                                                                   05940000
00582  3600-RECORD-PREVS-DELETED.                                       05950000
00583      MOVE ER-2528                TO  EMI-ERROR.                   05960000
00584      MOVE -1                     TO  FMAINTL.                     05970000
00585      PERFORM 9900-ERROR-FORMAT.                                   05980000
00586      GO TO 8200-SEND-DATAONLY.                                    05990000
00587                                                                   06000000
00588  3900-EXIT.                                                       06010000
00589      EXIT.                                                        06020000
00590                                                                   06030000
00591      EJECT                                                        06040000
00592                                                                   06050000
00593  4000-CERTIFICATE-UPDATE         SECTION.                         06060000
00594                                                                   06070000
00595      EXEC CICS HANDLE CONDITION                                   06080000
00596          NOTFND   (4400-NOT-FOUND)                                06090000
00597      END-EXEC.                                                    06100000
00598                                                                   06110000
00599      EXEC CICS READ                                               06120000
00600          EQUAL                                                    06130000
00601          DATASET   (ELCERT-ID)                                    06140000
00602          SET       (ADDRESS OF CERTIFICATE-MASTER)                06150000
00603          RIDFLD    (WS-CM-CONTROL-PRIMARY)                        06160000
00604          UPDATE                                                   06170000
00605      END-EXEC.                                                    06180000
00606                                                                   06190000
00607      IF FMAINTI = 'D'                                             06200000
092309       IF CM-NOTE-SW = '2'
092309         MOVE SPACE              TO  CM-NOTE-SW
092309       ELSE
092309         IF CM-NOTE-SW = '3'
092309            MOVE '1'             TO  CM-NOTE-SW                   06210000
092309         ELSE 
092309           IF CM-NOTE-SW = '6'
092309              MOVE '4'           TO  CM-NOTE-SW
092309           ELSE
092309             IF CM-NOTE-SW = '7'
092309                MOVE '5'         TO  CM-NOTE-SW
092309             END-IF
092309           END-IF
092309         END-IF
092309       END-IF
092309       MOVE 'N'                  TO  PI-BILLING-NOTES-EXIST            
092309       GO TO 4200-REWRITE-CERT-MASTER
092309     END-IF.                          
092309
092309     IF CM-NOTE-SW = ' ' 
092309         MOVE '2'                TO  CM-NOTE-SW
092309     ELSE
092309       IF CM-NOTE-SW = '1'
092309          MOVE '3'               TO  CM-NOTE-SW
092309       ELSE
092309         IF CM-NOTE-SW = '4'
092309            MOVE '6'             TO  CM-NOTE-SW
092309         ELSE
092309           IF CM-NOTE-SW = '5'
092309              MOVE '7'           TO  CM-NOTE-SW
092309           END-IF
092309         END-IF
092309       END-IF
092309     END-IF.
092309     MOVE 'Y'                    TO  PI-BILLING-NOTES-EXIST
092309     GO TO 4200-REWRITE-CERT-MASTER.
00610                                                                   06230000
00611      MOVE 'B'                    TO  JP-RECORD-TYPE.              06240000
00612      MOVE CERTIFICATE-MASTER     TO  JP-RECORD-AREA.              06250000
00613                                                                   06260000
00614      COMPUTE WS-JOURNAL-RECORD-LENGTH =                           06270000
00615              WS-ELCERT-RECORD-LENGTH  +  23.                      06280000
00616                                                                   06290000
00617      PERFORM 8400-LOG-JOURNAL-RECORD.                             06300000
00618                                                                   06310000
092309*00619      IF CN-BILLING-START-LINE-NO = ZERO                           06320000
092309*00620          MOVE '1'                TO  CM-NOTE-SW                   06330000
092309*00621          GO TO 4200-REWRITE-CERT-MASTER.                          06340000
092309*00622                                                                   06350000
092309*00623      MOVE +0                     TO  WS-SUB1.                     06360000
092309*00624      MOVE +2                     TO  CM-NOTE-SW.                  06370000
092309*00625                                                                   06380000
092309*00626  4100-CHECK-NOTE-LINE.                                            06390000
092309*00627      ADD +1  TO  WS-SUB1.                                         06400000
092309*00628                                                                   06410000
092309*00629      IF WS-SUB1 GREATER +10                                       06420000
092309*00630          GO TO 4200-REWRITE-CERT-MASTER.                          06430000
092309*00631                                                                   06440000
092309*00632      IF WS-SUB1 LESS    CN-BILLING-START-LINE-NO  OR              06450000
092309*00633                 GREATER CN-BILLING-END-LINE-NO                    06460000
092309*00634          NEXT SENTENCE                                            06470000
092309*00635      ELSE                                                         06480000
092309*00636          GO TO 4100-CHECK-NOTE-LINE.                              06490000
092309*00637                                                                   06500000
092309*00638      IF CN-LINE (WS-SUB1) GREATER THAN SPACES                     06510000
092309*00639          MOVE  3                 TO  CM-NOTE-SW                   06520000
092309*00640          GO TO 4200-REWRITE-CERT-MASTER                           06530000
092309*00641      ELSE                                                         06540000
092309*00642          GO TO 4100-CHECK-NOTE-LINE.                              06550000
00643                                                                   06560000
00644  4200-REWRITE-CERT-MASTER.                                        06570000
00645      MOVE 'C'                    TO  JP-RECORD-TYPE.              06580000
00646      MOVE CERTIFICATE-MASTER     TO  JP-RECORD-AREA.              06590000
00647                                                                   06600000
00648      EXEC CICS REWRITE                                            06610000
00649          FROM      (CERTIFICATE-MASTER)                           06620000
00650          DATASET   (ELCERT-ID)                                    06630000
00651      END-EXEC.                                                    06640000
00652                                                                   06650000
00653      COMPUTE WS-JOURNAL-RECORD-LENGTH =                           06660000
00654              WS-ELCERT-RECORD-LENGTH  +  23.                      06670000
00655                                                                   06680000
00656      PERFORM 8400-LOG-JOURNAL-RECORD.                             06690000
00657                                                                   06700000
00658      GO TO 4500-EXIT.                                             06710000
00659                                                                   06720000
00660  4400-NOT-FOUND.                                                  06730000
00661      MOVE -1                     TO  FMAINTL.                     06740000
00662      MOVE ER-0142                TO  EMI-ERROR.                   06750000
00663      PERFORM 9900-ERROR-FORMAT.                                   06760000
00664      GO TO 8200-SEND-DATAONLY.                                    06770000
00665                                                                   06780000
00666  4500-EXIT.                                                       06790000
00667      EXIT.                                                        06800000
00668                                                                   06810000
00669      EJECT                                                        06820000
00670                                                                   06830000
00671  6200-READ-NOTE-FILE             SECTION.                         06840000
00672                                                                   06850000
00673      EXEC CICS HANDLE CONDITION                                   06860000
00674          NOTFND   (6250-NOT-FOUND)                                06870000
00675      END-EXEC.                                                    06880000
00676                                                                   06890000
00677      EXEC CICS READ                                               06900000
00678          EQUAL                                                    06910000
00679          DATASET   (ERNOTE-ID)                                    06920000
00680          SET       (ADDRESS OF CERTIFICATE-NOTE)                  06930000
041320         RIDFLD    (WS-CN-CONTROL-PRIMARY)                        06940000
00682      END-EXEC.                                                    06950000
00683                                                                   06960000
00684      GO TO 6290-EXIT.                                             06970000
00685                                                                   06980000
00686  6250-NOT-FOUND.                                                  06990000
00687      MOVE 'N'                    TO  WS-RECORD-FOUND-SW.          07000000
00688                                                                   07010000
00689  6290-EXIT.                                                       07020000
00690       EXIT.                                                       07030000
00691                                                                   07040000
00692      EJECT                                                        07050000
00693                                                                   07060000
00694  6300-READ-NOTE-FILE-UPDT        SECTION.                         07070000
00695                                                                   07080000
00696      EXEC CICS HANDLE CONDITION                                   07090000
00697          NOTFND   (6350-NOT-FOUND)                                07100000
00698      END-EXEC.                                                    07110000
00699                                                                   07120000
00700      EXEC CICS READ                                               07130000
00701          EQUAL                                                    07140000
00702          DATASET   (ERNOTE-ID)                                    07150000
00703          SET       (ADDRESS OF CERTIFICATE-NOTE)                  07160000
041320         RIDFLD    (WS-CN-CONTROL-PRIMARY)                        07170000
00705          UPDATE                                                   07180000
00706      END-EXEC.                                                    07190000
00707                                                                   07200000
00708         GO TO 6390-EXIT.                                          07210000
00709                                                                   07220000
00710  6350-NOT-FOUND.                                                  07230000
00711      MOVE 'N'                    TO  WS-RECORD-FOUND-SW.          07240000
00712                                                                   07250000
00713  6390-EXIT.                                                       07260000
00714       EXIT.                                                       07270000
00715                                                                   07280000
00716       EJECT                                                       07290000
00717                                                                   07300000
00718  6400-WRITE-NOTE-RECORD          SECTION.                         07310000
00719                                                                   07320000
00720      EXEC CICS HANDLE CONDITION                                   07330000
00721          DUPREC   (6450-DUPLICATE-RECORD)                         07340000
00722      END-EXEC.                                                    07350000
00723                                                                   07360000
00724      EXEC CICS WRITE                                              07370000
00725          FROM      (CERTIFICATE-NOTE)                             07380000
00726          DATASET   (ERNOTE-ID)                                    07390000
041320         RIDFLD    (CN-CONTROL-PRIMARY)
00728      END-EXEC.                                                    07410000
00729                                                                   07420000
00730         GO TO 6490-EXIT.                                          07430000
00731                                                                   07440000
00732  6450-DUPLICATE-RECORD.                                           07450000
00733      MOVE 'Y'                    TO  WS-DUPREC-SW.                07460000
00734                                                                   07470000
00735  6490-EXIT.                                                       07480000
00736      EXIT.                                                        07490000
00737                                                                   07500000
00738      EJECT                                                        07510000
00739                                                                   07520000
00740  6500-REWRITE-NOTE-RECORD        SECTION.                         07530000
00741                                                                   07540000
00742      EXEC CICS REWRITE                                            07550000
00743          FROM      (CERTIFICATE-NOTE)                             07560000
00744          DATASET   (ERNOTE-ID)                                    07570000
00745      END-EXEC.                                                    07580000
00746                                                                   07590000
00747                                                                   07600000
00748  6590-EXIT.                                                       07610000
00749      EXIT.                                                        07620000
00750                                                                   07630000
00751      EJECT                                                        07640000
00752                                                                   07650000
00753  6600-DELETE-NOTE-RECORD         SECTION.                         07660000
00754                                                                   07670000
00755      EXEC CICS HANDLE CONDITION                                   07680000
00756          NOTFND (6650-RECORD-PREVS-DELETED)                       07690000
00757      END-EXEC.                                                    07700000
00758                                                                   07710000
00759      EXEC CICS DELETE                                             07720000
00760          DATASET   (ERNOTE-ID)                                    07730000
00761      END-EXEC.                                                    07740000
00762                                                                   07750000
00763      GO TO 6690-EXIT.                                             07760000
00764                                                                   07770000
00765  6650-RECORD-PREVS-DELETED.                                       07780000
00766      MOVE 'N'                    TO  WS-RECORD-FOUND-SW.          07790000
00767                                                                   07800000
00768  6690-EXIT.                                                       07810000
00769      EXIT.                                                        07820000
00770                                                                   07830000
00771      EJECT                                                        07840000
00772                                                                   07850000
00773  7000-FORMAT-SCREEN              SECTION.                         07860000
00774                                                                   07870000
00775      MOVE LOW-VALUES             TO  EL127FI.                     07880000
00776                                                                   07890000
00777      MOVE  PI-CARRIER            TO  FCARRIRO.                    07900000
00778      MOVE  PI-GROUPING           TO  FGROUPO.                     07910000
00779      MOVE  PI-STATE              TO  FSTO                         07920000
00780      MOVE  PI-ACCOUNT            TO  FACOUNTO.                    07930000
00781      MOVE  PI-CERT-PRIME         TO  FCERTO.                      07940000
00782      MOVE  PI-CERT-SFX           TO  FCRTSFXO.                    07950000
00783      MOVE  ' '                   TO  DC-OPTION-CODE.              07960000
00784      MOVE  PI-CERT-EFF-DT        TO  DC-BIN-DATE-1.               07970000
00785      PERFORM 9700-DATE-LINK.                                      07980000
00786      MOVE DC-GREG-DATE-1-EDIT    TO  FEFFDTO.                     07990000
092309     IF PI-CERT-NOTES-EXIST EQUAL 'Y'
092309         MOVE 'YES'              TO  FCERTYNO
092309     ELSE
092309         MOVE 'NO '              TO  FCERTYNO
092309     END-IF.
092309     IF PI-CLAIM-NOTES-EXIST EQUAL 'Y'
092309         MOVE 'YES'              TO  FCLMYNO
092309     ELSE
092309         MOVE 'NO '              TO  FCLMYNO
092309     END-IF.

041320     if pi-cancel-notes
041320        move '*****  C A N C E L  *****'
041320                                 TO FHEADO
041320        move 'PF7=ISSUE NOTES'   TO FPF7HO
041320        move '2'                 to ws-cn-record-type
041320     ELSE
041320        move '*****   I S S U E   *****'
041320                                 TO FHEADO
041320        MOVE 'PF7=CANCEL NOTES'  TO FPF7HO
041320        move '1'                 to ws-cn-record-type
041320     END-IF

041320     MOVE  PI-CARRIER            TO  WS-CN-CARRIER
041320     MOVE  PI-GROUPING           TO  WS-CN-GROUPING
041320     MOVE  PI-STATE              TO  WS-CN-STATE
041320     MOVE  PI-ACCOUNT            TO  WS-CN-ACCOUNT
041320     MOVE  PI-CERT-PRIME         TO  WS-CN-CERT-PRIME
041320     MOVE  PI-CERT-SFX           TO  WS-CN-CERT-SFX
041320     MOVE  PI-CERT-EFF-DT        TO  WS-CN-CERT-EFF-DT
041320     MOVE  PI-COMPANY-CD         TO  WS-CN-COMPANY-CD

00797      PERFORM 6200-READ-NOTE-FILE.                                 08100000
00798                                                                   08110000
00799      IF RECORD-NOT-FOUND                                          08120000
00800         MOVE 'A'                 TO  FMAINTI                      08130000
00801         MOVE AL-UANON            TO  FMAINTA                      08140000
00802         GO TO 7900-EXIT.                                          08150000
00803                                                                   08160000
00804      MOVE 'C'                    TO  FMAINTO.                     08170000
00805      MOVE AL-UANON               TO  FMAINTA.                     08180000
00806                                                                   08190000
00807      IF CN-LAST-MAINT-DT = SPACES                                 08200000
00808          MOVE EIBTIME            TO  CN-LAST-MAINT-HHMMSS         08210000
00809          MOVE SAVE-BIN-DATE      TO  CN-LAST-MAINT-DT.            08220000
00810                                                                   08230000
00811      IF CN-LAST-MAINT-DT NOT = SPACES AND LOW-VALUES              08240000
00812          MOVE  ' '               TO  DC-OPTION-CODE               08250000
00813          MOVE  CN-LAST-MAINT-DT  TO  DC-BIN-DATE-1                08260000
00814          PERFORM 9700-DATE-LINK                                   08270000
00815          MOVE DC-GREG-DATE-1-EDIT TO  MAINTONO.                   08280000
00816                                                                   08290000
00817      IF CN-LAST-MAINT-HHMMSS NUMERIC                              08300000
00818          MOVE CN-LAST-MAINT-HHMMSS TO TIME-IN                     08310000
00819          MOVE TIME-OUT             TO MAINTATO                    08320000
00820        ELSE                                                       08330000
00821          MOVE ZEROS                TO TIME-IN                     08340000
00822          MOVE TIME-OUT             TO MAINTATO.                   08350000
00823                                                                   08360000
00824      MOVE CN-LAST-MAINT-USER       TO MAINTBYO.                   08370000
00825                                                                   08380000
00826      IF CN-BILLING-START-LINE-NO GREATER THAN ZEROS               08390000
00827          MOVE CN-BILLING-START-LINE-NO   TO  FBSTARTO             08400000
00828          MOVE AL-UNNON                   TO  FBSTARTA.            08410000
00829                                                                   08420000
00830      IF CN-BILLING-END-LINE-NO GREATER THAN ZEROS                 08430000
00831          MOVE CN-BILLING-END-LINE-NO     TO  FBENDO               08440000
00832          MOVE AL-UNNON                   TO  FBENDA.              08450000
00833                                                                   08460000
00834      MOVE +0                     TO  WS-SUB1.                     08470000
00835                                                                   08480000
00836  7100-BUILD-NOTE-LINES.                                           08490000
00837      ADD +1 TO  WS-SUB1.                                          08500000
00838                                                                   08510000
00839      IF WS-SUB1 GREATER THAN +10                                  08520000
00840         GO TO 7900-EXIT.                                          08530000
00841                                                                   08540000
00842      MOVE  CN-LINE (WS-SUB1)     TO  DNOTE-LINE       (WS-SUB1).  08550000
00843      MOVE  AL-UANON              TO  DNOTE-LINE-ATTRB (WS-SUB1).  08560000
00844                                                                   08570000
00845      GO TO 7100-BUILD-NOTE-LINES.                                 08580000
00846                                                                   08590000
00847  7900-EXIT.                                                       08600000
00848      EXIT.                                                        08610000
00849                                                                   08620000
00850      EJECT                                                        08630000
00851                                                                   08640000
00852  8100-SEND-INITIAL-MAP SECTION.                                   08650000
00853                                                                   08660000
00854      MOVE SAVE-DATE              TO  FDATEO.                      08670000
00855      MOVE EIBTIME                TO  TIME-IN.                     08680000
00856      MOVE TIME-OUT               TO  FTIMEO.                      08690000
101201     MOVE PI-COMPANY-ID          TO  CMPNYIDO.                    08700000
101201     MOVE PI-PROCESSOR-ID        TO  USERIDO.                     08710000
00857      MOVE -1                     TO  FMAINTL.                     08720000
00858      MOVE EMI-MESSAGE-AREA (1)   TO  FERRMSGO.                    08730000
00859      EXEC CICS SEND                                               08740000
00860          MAP      (MAP-NAME)                                      08750000
00861          MAPSET   (MAPSET-NAME)                                   08760000
00862          FROM     (EL127FO)                                       08770000
00863          ERASE                                                    08780000
00864          CURSOR                                                   08790000
00865      END-EXEC.                                                    08800000
00866                                                                   08810000
00867      GO TO 9100-RETURN-TRAN.                                      08820000
00868                                                                   08830000
00869  8200-SEND-DATAONLY     SECTION.                                  08840000
00870                                                                   08850000
00871      MOVE SAVE-DATE              TO  FDATEO.                      08860000
00872      MOVE EIBTIME                TO  TIME-IN.                     08870000
00873      MOVE TIME-OUT               TO  FTIMEO.                      08880000
101201     MOVE PI-COMPANY-ID          TO  CMPNYIDO.                    08881001
101201     MOVE PI-PROCESSOR-ID        TO  USERIDO.                     08882001
00874      MOVE EMI-MESSAGE-AREA (1)   TO  FERRMSGO.                    08890000
00875      EXEC CICS SEND                                               08900000
00876          MAP      (MAP-NAME)                                      08910000
00877          MAPSET   (MAPSET-NAME)                                   08920000
00878          FROM     (EL127FO)                                       08930000
00879          DATAONLY                                                 08940000
00880          ERASEAUP                                                 08950000
00881          CURSOR                                                   08960000
00882      END-EXEC.                                                    08970000
00883                                                                   08980000
00884      GO TO 9100-RETURN-TRAN.                                      08990000
00885                                                                   09000000
00886      EJECT                                                        09010000
00887                                                                   09020000
00888  8300-SEND-TEXT         SECTION.                                  09030000
00889      EXEC CICS SEND TEXT                                          09040000
00890          FROM     (LOGOFF-TEXT)                                   09050000
00891          LENGTH   (LOGOFF-LENGTH)                                 09060000
00892          ERASE                                                    09070000
00893          FREEKB                                                   09080000
00894      END-EXEC.                                                    09090000
00895                                                                   09100000
00896      EXEC CICS RETURN                                             09110000
00897      END-EXEC.                                                    09120000
00898                                                                   09130000
00899      EJECT                                                        09140000
00900                                                                   09150000
00901  8400-LOG-JOURNAL-RECORD         SECTION.                         09160000
00902                                                                   09170000
00903 *    MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.                  09180000
00904 *    MOVE ERNOTE-ID              TO  JP-FILE-ID.                  09190000
00905 *    MOVE THIS-PGM               TO  JP-PROGRAM-ID.               09200000
00906 *    EXEC CICS JOURNAL                                            09210000
00907 *        JFILEID     (PI-JOURNAL-FILE-ID)                         09220000
00908 *        JTYPEID     ('ER')                                       09230000
00909 *        FROM        (JOURNAL-RECORD)                             09240000
00910 *        LENGTH      (WS-JOURNAL-RECORD-LENGTH)                   09250000
00911 *    END-EXEC.                                                    09260000
00912                                                                   09270000
00913  8400-EXIT.                                                       09280000
00914      EXIT.                                                        09290000
00915                                                                   09300000
00916  8600-DEEDIT           SECTION.                                   09310000
00917      EXEC CICS BIF DEEDIT                                         09320000
00918           FIELD   (DEEDIT-FIELD)                                  09330000
00919           LENGTH  (15)                                            09340000
00920      END-EXEC.                                                    09350000
00921                                                                   09360000
00922  8800-UNAUTHORIZED-ACCESS        SECTION.                         09370000
00923      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.                  09380000
00924      GO TO 8300-SEND-TEXT.                                        09390000
00925                                                                   09400000
00926  8810-PF23              SECTION.                                  09410000
00927      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               09420000
00928      MOVE XCTL-005               TO  PGM-NAME.                    09430000
00929      GO TO 9300-XCTL.                                             09440000
00930                                                                   09450000
00931  9100-RETURN-TRAN       SECTION.                                  09460000
00932      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            09470000
00933      MOVE SCREEN-NUMBER          TO  PI-CURRENT-SCREEN-NO.        09480000
00934      EXEC CICS RETURN                                             09490000
00935          TRANSID    (TRANS-ID)                                    09500000
00936          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     09510000
00937          LENGTH     (PI-COMM-LENGTH)                              09520000
00938      END-EXEC.                                                    09530000
00939                                                                   09540000
00940  9200-RETURN-MAIN-MENU SECTION.                                   09550000
00941      MOVE XCTL-626               TO  PGM-NAME.                    09560000
00942      GO TO 9300-XCTL.                                             09570000
00943                                                                   09580000
00944  9300-XCTL             SECTION.                                   09590000
00945      EXEC CICS XCTL                                               09600000
00946          PROGRAM    (PGM-NAME)                                    09610000
00947          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     09620000
00948          LENGTH     (PI-COMM-LENGTH)                              09630000
00949      END-EXEC.                                                    09640000
00950                                                                   09650000
00951  9400-CLEAR            SECTION.                                   09660000
00952      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.                    09670000
00953      GO TO 9300-XCTL.                                             09680000
00954                                                                   09690000
00955  9500-PF12             SECTION.                                   09700000
00956      MOVE XCTL-010               TO  PGM-NAME.                    09710000
00957      GO TO 9300-XCTL.                                             09720000
00958                                                                   09730000
00959  9600-PGMID-ERROR      SECTION.                                   09740000
00960      EXEC CICS HANDLE CONDITION                                   09750000
00961          PGMIDERR    (8300-SEND-TEXT)                             09760000
00962      END-EXEC.                                                    09770000
00963                                                                   09780000
00964      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.          09790000
00965      MOVE ' '                    TO  PI-ENTRY-CD-1.               09800000
00966      MOVE XCTL-005               TO  PGM-NAME.                    09810000
00967      MOVE PGM-NAME               TO  LOGOFF-PGM.                  09820000
00968      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 09830000
00969      GO TO 9300-XCTL.                                             09840000
00970                                                                   09850000
00971  9700-DATE-LINK         SECTION.                                  09860000
00972      MOVE LINK-ELDATCV           TO  PGM-NAME                     09870000
00973      EXEC CICS LINK                                               09880000
00974          PROGRAM    (PGM-NAME)                                    09890000
00975          COMMAREA   (DATE-CONVERSION-DATA)                        09900000
00976          LENGTH     (DC-COMM-LENGTH)                              09910000
00977      END-EXEC.                                                    09920000
00978                                                                   09930000
00979  9900-ERROR-FORMAT       SECTION.                                 09940000
00980      IF NOT EMI-ERRORS-COMPLETE                                   09950000
00981          MOVE LINK-001           TO  PGM-NAME                     09960000
00982          EXEC CICS LINK                                           09970000
00983              PROGRAM    (PGM-NAME)                                09980000
00984              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           09990000
00985              LENGTH     (EMI-COMM-LENGTH)                         10000000
00986          END-EXEC.                                                10010000
00987                                                                   10020000
00988  9900-EXIT.                                                       10030000
00989      EXIT.                                                        10040000
00990                                                                   10050000
00991  9990-ABEND             SECTION.                                  10060000
00992      MOVE LINK-004               TO  PGM-NAME.                    10070000
00993      MOVE DFHEIBLK               TO  EMI-LINE1                    10080000
00994                                                                   10090000
00995      EXEC CICS LINK                                               10100000
00996          PROGRAM   (PGM-NAME)                                     10110000
00997          COMMAREA  (EMI-LINE1)                                    10120000
00998          LENGTH    (72)                                           10130000
00999      END-EXEC.                                                    10140000
01000                                                                   10150000
01001      MOVE -1                     TO  FMAINTL.                     10160000
01002      GO TO 8200-SEND-DATAONLY.                                    10170000
01003                                                                   10180000
01004  9995-SECURITY-VIOLATION.                                         10190000
01005      COPY ELCSCTP.                                                10200000
01006                                                                   10210000
01007  9995-EXIT.                                                       10220000
01008      EXIT.                                                        10230000
01009                                                                   10240000
