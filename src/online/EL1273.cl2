00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL1273.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 12/07/94 15:31:19.
00007 *                            VMOD=2.041.
00008 *
00008 *
00009 *AUTHOR.    LOGIC, INC.
00010 *           DALLAS, TEXAS.
00011
00012 *DATE-COMPILED.
00013
00014 *SECURITY.   *****************************************************
00015 *            *                                                   *
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00017 *            *                                                   *
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00021 *            *                                                   *
00022 *            *****************************************************
00023
00024 *REMARKS.
00025 *          TRANSACTION - EXX3
00026
00027 *        CERT UPDATE PROGRAM.
00028
00029 *    SCREENS     - EL127C - CERTIFICATE DISPLAY
00030
00031 *    ENTERED BY  - EL1272 - CERTIFICATE LOOKUP
00032 *                  EL150  - STATUS DISPLAY
00033 *                  EL677  - CHECK MAINTENANCE
00034
00035 *    EXIT TO     - CALLING PROGRAM
00036
00037 *    INPUT FILE  - ELCERT - CERTIFICATE INFORCE FILE
00038 *                  ELCNTL - CONTROL FILE
00039
00040 *    OUTPUT FILE - ELCERT - CERTIFICATE INFORCE FILE
00041 *                  ERCRTC - PENDING MAINT TO CERT FILE
00042
00043 *    COMMAREA    - PASSED.  IF A CERTIFICATE IS SELECTED, THE
00044 *                  CONTROL OF THAT CERTIFICATE IS PLACED IN THE
00045 *                  APPROPRIATE FIELDS OF THE COMMAREA FOR
00046 *                  REFERENCE BY SUCCESSIVE PROGRAMS.  THE PROGRAM
00047 *                  WORK AREA OF THE COMMAREA IS USED TO PASS THE
00048 *                  RECORD KEY INFORMATION NEEDED BY EL1272 TO
00049 *                  LOCATE THE CERTIFICATE.
00050
00051
00052 *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL1272.
00053 *                  USE THE KEY TO THE CERTIFICATE MASTER PASSED
00054 *                  IN THE COMMAREA TO DISPLAY THE CERTIFICATE AND
00055 *                  RETURN WITH THE TRANSACTION OF THE CALLING
00056 *                  PROGRAM.
101201******************************************************************
101201*                   C H A N G E   L O G
101201*
101201* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101201*-----------------------------------------------------------------
101201*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101201* EFFECTIVE    NUMBER
101201*-----------------------------------------------------------------
101201* 101201    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING  
101005* 101005    2005080800005  PEMA  OPEN MON BEN FOR MNTHLY DCC
102706* 102706  CR2006052600003  PEMA  ADD SIG SW PROCESSING FOR CID
040909* 040909    2009031600001  AJRA  ADD VIN NUMBER
101509* 101509    2008100900003  AJRA  CALL NEW CERT NOTE SCREEN
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
022210* 022210  CR2009090400001  PEMA  ADD VALIDATION FOR NAME FIELDS
121410* 121410  CR2010100600001  AJRA  ADD CLAIM REFUND FLAG 
010412* 010412  CR2011022800001  AJRA  GO TO REV AND CORR ON CLM RES/REF
062712* 062712  CR2011022800001  AJRA  REDEFINE ORIG DATA
090612* 090612  IR2012090600001  AJRA  NAPERSOFT
092412* 092412  IR2012091100004  AJRA  CHECK ADDRESS BEFORE LETTER WRITER
121312* 121312  CR2012101700002  AJRA  RESC/REF ARE YOU SURE
121712* 121712  CR2012101700002  AJRA  ADD DEFAULT AGE FLAG
122712* 122712  CR2012101700002  AJRA  PASS FLAG FOR ZERO COMM PCT TO CANCEL
011413* 011413  IR2012122700003  AJRA  ADD CRTO ISSUE/CANCEL INDICATOR
101513* 101513  CR2013090300001  AJRA  NAPERSOFT PHASE 2
120313* 120313  CR2013090300001  AJRA  NAPERSOFT PHASE 2
071015* 071015  CR2014011600001  PEMA  ADD ALL RPT CODES TO SCREEN
031416* 031416  CR2016030800002  PEMA  ALLOW USER TO REMOVE VIN
100917* 100917  CR2017092000002  PEMA  Pass vin to elcanc
062017* 062017  CR2015091000001  PEMA  ADD PROCESSING FOR TN REF INTEREST
020218* 020218  CR2017062000002  PEMA  AUDIT NB FOR PREV CLAIMS
052918* 052918  CR2018040600002  PEMA  ADD REFUND DIRECT INDICATOR
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
101918* 101918  CR2018050200001  PEMA  Add check for cancel rec
101201******************************************************************
00057
00058      EJECT
00059  ENVIRONMENT DIVISION.
00060
00061  DATA DIVISION.
00062
00063  WORKING-STORAGE SECTION.
00064  77  FILLER  PIC X(32)  VALUE '********************************'.
00065  77  FILLER  PIC X(32)  VALUE '*   EL1273 WORKING STORAGE     *'.
00066  77  FILLER  PIC X(32)  VALUE '************VMOD=2.041 ********'.
       77  WS-DENIAL-TYPE       PIC X  VALUE SPACES.
       77  WS-DONE-SW           PIC X  VALUE SPACES.
           88  I-AM-DONE          VALUE 'Y'.
       77  W-ARCH-NUMBER               PIC S9(08)      COMP value +0.
010412 77  WS-ERMAIL-SW                PIC X  VALUE ' '.
010412     88  ERMAIL-FOUND                 VALUE 'Y'.

       77  WS-RESPONSE         PIC S9(8)   COMP.                    
           88  RESP-NORMAL              VALUE +00.
           88  RESP-ERROR               VALUE +01.
           88  RESP-NOTFND              VALUE +13.
           88  RESP-DUPREC              VALUE +14.
           88  RESP-DUPKEY              VALUE +15.
00067
       01  P pointer.
       01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
       01  KIXHOST             pic x(9) value Z"HOSTNAME".
       01  var-ptr pointer.
       01  env-var-len                 pic 9(4)  binary.
       01  rc                          pic 9(9)  binary.

       01  WS-KIXHOST                  PIC X(10).
       01  WS-KIXSYS.
           05  WS-KIX-FIL1             PIC X(10).
           05  WS-KIX-APPS             PIC X(10).
           05  WS-KIX-ENV              PIC X(10).
           05  WS-KIX-MYENV            PIC X(10).
           05  WS-KIX-SYS              PIC X(10).

00068                                      COPY ELCSCTM.
00069
00070                                      COPY ELCSCRTY.
00071
00072  01  WS-DATE-AREA.
00073      05  SAVE-DATE                   PIC X(8)     VALUE SPACES.
00074      05  SAVE-BIN-DATE               PIC X(2)     VALUE SPACES.

       01  ELCNTL-KEY.
           05  CNTL-COMP-ID        PIC X(3)  VALUE SPACES.
           05  CNTL-REC-TYPE       PIC X     VALUE SPACES.
           05  CNTL-ACCESS.
               10  CNTL-STATE      PIC XX    VALUE SPACES.
               10  FILLER          PIC X     VALUE SPACES.
               10  CNTL-CARRIER    PIC X     VALUE SPACES.
           05  CNTL-SEQ            PIC S9(4) VALUE +0 COMP.


00075

       01  CANCEL-GEN-PASS-AREA.
           05  CG-OPTION-CODE          PIC X.
               88  CG-VALID-OPTION       VALUE '1' '2' '3' '4'.
               88  CG-FLAT-CANCEL        VALUE '1'.
               88  CG-CANCEL             VALUE '2'.
               88  CG-CANCEL-REISSUE     VALUE '3'.
               88  CG-FLAT-CANCEL-SAME-BATCH VALUE '4'.
           05  CG-ERROR-CODE           PIC 99.
               88  CG-SUCCESS            VALUE 00.
               88  CG-DATE-ERROR         VALUE 01.
               88  CG-CERT-NOT-FOUND     VALUE 02.
               88  CG-AMOUNT-ERROR       VALUE 04.
               88  CG-OPTION-ERROR       VALUE 05.
               88  CG-PREV-CAN           VALUE 06.
               88  CG-INVALID-DATA       VALUE 07.
               88  CG-NO-ACCT-MSTR       VALUE 08.
               88  CG-SFX-A-EXIST        VALUE 09.
               88  CG-MISC-ERROR         VALUE 99.
           05  CG-COMPANY-ID           PIC XXX.
           05  CG-PROC-ID              PIC XXXX.
           05  CG-CURRENT-DT           PIC XX.
           05  CG-MONTH-END-DT         PIC XX.
           05  CG-CERT-KEY.
               10  CG-CERT-COMPANY-CD  PIC X.
               10  CG-CERT-CARRIER     PIC X.
               10  CG-CERT-GROUP       PIC X(6).
               10  CG-CERT-STATE       PIC XX.
               10  CG-CERT-ACCOUNT     PIC X(10).
               10  CG-CERT-EFF-DT      PIC XX.
               10  CG-CERT-CERT-NO     PIC X(11).
           05  CG-LF-CAN-DATA.
               10  CG-LF-CAN-DT        PIC XX.
               10  CG-LF-CAN-AMT       PIC S9(7)V99 COMP-3.
           05  CG-AH-CAN-DATA.
               10  CG-AH-CAN-DT        PIC XX.
               10  CG-AH-CAN-AMT       PIC S9(7)V99 COMP-3.
           05  CG-CERT-PROFILE-DATA.
               10  CG-INS-LNAME        PIC X(15).
               10  CG-INS-FNAME        PIC X(10).
               10  CG-INS-MID-INIT     PIC X.
               10  CG-INS-AGE          PIC 99.
               10  CG-JNT-LNAME        PIC X(15).
               10  CG-JNT-FNAME        PIC X(10).
               10  CG-JNT-MID-INIT     PIC X.
               10  CG-JNT-AGE          PIC 99.
           05  CG-LF-ISS-DATA.
               10  CG-LF-BENCD         PIC XX.
               10  CG-LF-PREM-AMT      PIC S9(7)V99 COMP-3.
072312         10  CG-LF-ALT-PREM-AMT  PIC S9(7)V99 COMP-3.
           05  CG-AH-ISS-DATA.
               10  CG-AH-BENCD         PIC XX.
               10  CG-AH-PREM-AMT      PIC S9(7)V99 COMP-3.
           05  CG-BATCH-NO             PIC X(6).
010412     05  CG-BATCH-SEQ-NO         PIC 9(4)  COMP.
072312     05  CG-DCC-REASON-CD        PIC X.
122712     05  CG-COMM-PCT-ZERO        PIC X.
100917     05  cg-vin                  pic x(17).
101918     05  cg-from-where           pic x(6).
101918     05  FILLER                  PIC X(281).

00076  01  FILLER                          COMP-3.
00077      05  WS-LF-ALT                   PIC S9(9)V99.
00078      05  WS-ERROR-COUNT              PIC S9(3)    VALUE ZERO.
00079      05  TIME-IN                     PIC S9(7)    VALUE ZERO.
00080      05  TIME-OUT                    REDEFINES
00081          TIME-IN                     PIC S9(3)V9(4).
00082
00083      05  WS-ELAPSED-MONTHS           PIC S9(3)    VALUE ZERO.
00084
00085      05  WS-UPDATE-SW                PIC S9       VALUE ZERO.
00086          88  NO-UPDATES-MADE                      VALUE ZERO.
00087      05  WS-ST-REC-NOT-FOUND         PIC S9       VALUE ZERO.
040909     05  WS-CERT-TRL-REC-NOT-FOUND   PIC S9       VALUE ZERO.
00088      05  WS-NOT-FOUND                PIC S9       VALUE ZERO.
00089          88  BENEFIT-FOUND                        VALUE +1.
00090      05  WS-READNEXT-SW              PIC S9       VALUE ZERO.
00091
00092      05  WS-COMPLETED-SUCCESSFUL     PIC S9       VALUE ZERO.
00093        88  TRANSACTION-SUCCESSFUL                 VALUE +1.
00094
010412 01  WS-WORK.
010412     05  GETMAIN-SPACE               PIC X VALUE SPACES.
010412     05  ERPNDB-LENGTH               PIC 9(04)  VALUE 585 COMP.
010412     05  ELCRTO-LENGTH               PIC 9(04)  VALUE 524 COMP.
010412     05  WS-WRK-SFX-COMP             PIC S9(4) COMP.
010412     05  WS-WRK-SFX-R REDEFINES WS-WRK-SFX-COMP.
010412         10  FILLER                  PIC X.
010412         10  WS-WRK-SFX              PIC X.
010412
00095  01  FILLER   COMP SYNC.
00096      05  W-ONE                       PIC S9(4)    VALUE +1.
00097      05  SC-ITEM                     PIC S9(4)    VALUE +0001.
00098  01  WS-SAVE-CERT-CHANGE-REC         PIC X(300).

                                       COPY ERCACCT.
       01  ERACCT-KEY.                                              
           05  ERACCT-COMPANY-CD       PIC  X.                   
           05  ERACCT-CARRIER          PIC  X.                   
           05  ERACCT-GROUPING         PIC  X(6).                
           05  ERACCT-STATE            PIC  XX.                  
           05  ERACCT-ACCOUNT          PIC  X(10).               
           05  ERACCT-EXP-DT.
               10  ERACCT-DT           PIC  XX.
               10  ERACCT-FILL         PIC  X(4).

       01  ELMSTR5-KEY.
           05  MSTR5-COMP-CD           PIC X.
           05  MSTR5-CERT-NO.
               20  MSTR5-CERT-NO-PRIME PIC X(10).
               20  MSTR5-CERT-NO-SUFX  PIC X.

010412 01  ERPNDB-KEY.
010412     05  PNDB-COMPANY-CD     PIC X.
010412     05  PNDB-BATCH-NO       PIC X(6).
010412     05  PNDB-SEQ-NO         PIC 9(4) BINARY.
010412     05  PNDB-CHG-SEQ-NO     PIC 9(4) BINARY.
010412
010412 01  ELCRTO-KEY.                                    
010412     05  ELCRTO-COMPANY-CD       PIC X.             
010412     05  ELCRTO-CARRIER          PIC X.             
010412     05  ELCRTO-GROUPING         PIC X(6).          
010412     05  ELCRTO-STATE            PIC XX.            
010412     05  ELCRTO-ACCOUNT          PIC X(10).         
010412     05  ELCRTO-CERT-EFF-DT      PIC XX.            
010412     05  ELCRTO-CERT-NO.                            
010412         10  ELCRTO-CERT-PRIME   PIC X(10).         
010412         10  ELCRTO-CERT-SFX     PIC X.             
010412     05  ELCRTO-RECORD-TYPE      PIC X.             
010412     05  ELCRTO-SEQ-NO           PIC 9(4)  BINARY.  
010412
00100  01  FILLER.
00101      05  W-QID.
00102          10  W-QID-TERM              PIC X(04) VALUE SPACES.
00103          10  FILLER                  PIC X(04) VALUE '127C'.
00104      05  WS-CLBEN.
00105          10  WS-CLBEN2                       PIC ZZZ,ZZZ,ZZ9.99.
00106          10  WS-CLBEN0 REDEFINES WS-CLBEN2   PIC ZZ,ZZZ,ZZZ,ZZ9.
00107
00108      05  WS-CALC-CD                  PIC X.
00109
00110      05  WS-JOINT-INDICATOR          PIC X.
00111          88  WS-JOINT-COVERAGE       VALUE 'J'.
00112
00113      05  WS-SAVE-CC-KEY              PIC X(37)    VALUE SPACES.
00114
00115      05  WS-BENEFIT-NO               PIC XX       VALUE ZERO.
00116      05  WS-BENEFIT-DESCRIP          PIC X(10)    VALUE SPACES.
00117      05  WS-KIND                     PIC X(3)     VALUE SPACES.
00118      05  WS-SPACES                   PIC X        VALUE SPACES.
00119
00120      05  DEEDIT-FIELD                PIC X(15).
00121      05  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD  PIC S9(15).
00122
00123      05  WS-VAL-TABLE.
00124          10  WS-VAL-CD               PIC X.
00125          10  WS-VAL-TERM             PIC X.
00126          10  WS-VAL-BENE             PIC X.
00127
00128      05  WS-MAPSET-NAME              PIC X(8)     VALUE 'EL1273S'.
00129      05  WS-MAP-NAME                 PIC X(8)     VALUE 'EL127C'.
00130      05  WS-MAP-NUMBER               PIC X(4)     VALUE '127C'.
00131
00132      05  THIS-PGM                    PIC X(8)     VALUE 'EL1273'.
00133
00134      05  WS-TRANS-ID                 PIC X(4)     VALUE 'EXX3'.
00135      05  EL001                       PIC X(8)     VALUE 'EL001'.
00136      05  EL004                       PIC X(8)     VALUE 'EL004'.
00137      05  EL005                       PIC X(8)     VALUE 'EL005'.
00138      05  EL010                       PIC X(8)     VALUE 'EL010'.
00139      05  ELDATCV                     PIC X(8)     VALUE 'ELDATCV'.
00140      05  ELRTRM                      PIC X(8)     VALUE 'ELRTRM'.
00141      05  ELRAMT                      PIC X(8)     VALUE 'ELRAMT'.
00142
00143      05  WS-CONTROL-FILE-DSID        PIC X(8)     VALUE 'ELCNTL'.
00144      05  WS-CERTIFICATE-MASTER-DSID  PIC X(8)     VALUE 'ELCERT'.
00145      05  WS-CERT-MAINT-FILE-DSID     PIC X(8)     VALUE 'ERCRTC'.
040909     05  WS-CERT-TRAILERS-DSID       PIC X(8)     VALUE 'ELCRTT'.
010412
010412     05  WS-01-CANCEL-ERR        PIC X(25)
010412         VALUE 'CANCEL DATE ERROR'.
010412     05  WS-02-CANCEL-ERR        PIC X(25)
010412         VALUE 'CERT NOT FOUND'.
010412     05  WS-04-CANCEL-ERR        PIC X(25)
010412         VALUE 'CANCEL AMOUNT ERROR'.
010412     05  WS-05-CANCEL-ERR        PIC X(25)
010412         VALUE 'CANCEL OPTION ERROR'.
010412     05  WS-06-CANCEL-ERR        PIC X(25)
010412         VALUE 'PREVIOUSLY CANCELLED'.
010412     05  WS-07-CANCEL-ERR        PIC X(25)
010412         VALUE 'INVALID DATA'.
010412     05  WS-08-CANCEL-ERR        PIC X(25)
010412         VALUE 'NO ACCOUNT MASTER'.
010412     05  WS-09-CANCEL-ERR        PIC X(25)
010412         VALUE 'SUFFIX ALREADY EXISTS'.
010412     05  WS-99-CANCEL-ERR        PIC X(25)
010412         VALUE 'MISC CANCEL ERROR'.
00146
00147      05  WS-INDEX                    PIC S9(4)    VALUE ZERO
00148                                      COMP
00149                                      SYNC.
00150
00151      05  WS-CURRENT-DATE             PIC XX      VALUE LOW-VALUES.
00152      05  WS-JOURNAL-TYPE-ID          PIC XX      VALUE 'EL'.
00153
00154      05  WS-CAGEI                    PIC S99        VALUE ZERO.
00155      05  WS-CJAGEI                   PIC S99        VALUE ZERO.
00156      05  WS-APR                      PIC S999V9(4)  VALUE +0.
00157      05  WS-CLM-DEDUCT               PIC S9(5)V99   VALUE +0.
00158      05  WS-CAN-DEDUCT               PIC S9(5)V99   VALUE +0.
00159      05  WS-LOAN-BAL                 PIC S9(7)V99   VALUE +0.
00160      05  WS-LF-ORIG-TERM             PIC S9(3)      VALUE +0.
00161      05  WS-AH-ORIG-TERM             PIC S9(3)      VALUE +0.
00162      05  WS-LIVES                    PIC S9(7)      VALUE +0.
00163      05  WS-BILLED                   PIC S9(7)      VALUE +0.
00164      05  WS-LF-PREM                  PIC S9(7)V99   VALUE +0.
00165      05  WS-LF-BENE                  PIC S9(9)V99   VALUE +0.
00166      05  WS-AH-PREM                  PIC S9(7)V99   VALUE +0.
00167      05  WS-AH-BENE                  PIC S9(7)V99   VALUE +0.
00168      05  WS-SSNO                     PIC X(11).
00169      05  WS-SS-NO REDEFINES WS-SSNO  PIC 999B99B9999.
00170      05  WS-ACCOUNT.
00171          10  FILLER                  PIC X(4).
00172          10  WS-ACCT                 PIC X(6).
00173
00174      05  WS-MEMBER-NO            PIC X(12).
00175      05  FILLER  REDEFINES  WS-MEMBER-NO.
00176          10  WS-MEMBER-NO-1-8    PIC 9(8).
00177          10  FILLER              PIC X(4).
00178
00179      05  WS-I-MICRO-NO           PIC S9(9)        COMP-3.
00180
00181      05  WS-STATUS-DESC.
00182          10  FILLER              PIC X(7).
00183          10  WS-UW-STATUS        PIC X(5).
00184
00185      EJECT
00186      05  ERROR-MESSAGES.
00187          10  ER-0008                 PIC X(4)    VALUE '0008'.
00188          10  ER-0029                 PIC X(4)    VALUE '0029'.
00189          10  ER-0033                 PIC X(4)    VALUE '0033'.
00190          10  ER-0070                 PIC X(4)    VALUE '0070'.
010412         10  ER-0132                 PIC X(4)    VALUE '0132'.
00191          10  ER-0142                 PIC X(4)    VALUE '0142'.
00192          10  ER-0151                 PIC X(4)    VALUE '0151'.
               10  ER-0236                 PIC X(4)    VALUE '0236'.
00193          10  ER-0588                 PIC X(4)    VALUE '0588'.
00194          10  ER-0692                 PIC X(4)    VALUE '0692'.
092412         10  ER-1609                 PIC X(4)    VALUE '1609'.
00195          10  ER-2152                 PIC X(4)    VALUE '2152'.
00196          10  ER-2223                 PIC X(4)    VALUE '2223'.
00197          10  ER-2351                 PIC X(4)    VALUE '2351'.
00198          10  ER-2352                 PIC X(4)    VALUE '2352'.
00199          10  ER-2354                 PIC X(4)    VALUE '2354'.
00200          10  ER-2547                 PIC X(4)    VALUE '2547'.
00201          10  ER-2848                 PIC X(4)    VALUE '2848'.
092412         10  ER-3000                 PIC X(4)    VALUE '3000'.
121410         10  ER-3036                 PIC X(4)    VALUE '3036'.
040909         10  ER-3825                 PIC X(4)    VALUE '3825'.
040909         10  ER-3826                 PIC X(4)    VALUE '3826'.
040909         10  ER-3827                 PIC X(4)    VALUE '3827'.
010412         10  ER-3830                 PIC X(4)    VALUE '3830'.
010412         10  ER-3831                 PIC X(4)    VALUE '3831'.
092412         10  ER-3834                 PIC X(4)    VALUE '3834'.
092412         10  ER-3835                 PIC X(4)    VALUE '3835'.
121312         10  ER-3838                 PIC X(4)    VALUE '3838'.
120313         10  ER-7049                 PIC X(4)    VALUE '7049'.
00202          10  ER-7233                 PIC X(4)    VALUE '7233'.
00203          10  ER-7234                 PIC X(4)    VALUE '7234'.
00204          10  ER-7242                 PIC X(4)    VALUE '7242'.
00205          10  ER-7248                 PIC X(4)    VALUE '7248'.
00206
00207      05  WS-CONTROL-FILE-KEY.
00208          10  WS-CFK-COMPANY-ID       PIC X(3)     VALUE SPACES.
00209          10  WS-CFK-RECORD-TYPE      PIC X        VALUE ZERO.
00210 *          88  STATE-MASTER                       VALUE '3'.
00211 *          88  LF-BENEFIT-MASTER                  VALUE '4'.
00212 *          88  AH-BENEFIT-MASTER                  VALUE '5'.
00213 *          88  CARRIER-MASTER                     VALUE '6'.
00214          10  WS-CFK-ACCESS.
00215              15  WS-CFK-STATE        PIC XX       VALUE SPACES.
00216              15  WS-CFK-BENEFIT-NO                VALUE SPACES.
00217                  20  FILLER          PIC X.
00218                  20  WS-CFK-CARRIER  PIC X.
00219          10  WS-CFK-SEQUENCE-NO      PIC S9(4)    VALUE ZERO COMP.
00220
052918     05  ws-eracnt-key.
052918         10  ws-nt-company-cd    pic x.
052918         10  ws-nt-carrier       pic x.
052918         10  ws-nt-grouping      pic x(6).
052918         10  ws-nt-state         pic xx.
052918         10  ws-nt-account       pic x(10).
052918         10  ws-nt-rec-type      pic x.    *> 2
052918         10  ws-nt-seq-no        pic s9(4) comp. *> 3

00221      05  WS-CERTIFICATE-KEY.
00222          10  WS-CK-COMPANY-CD        PIC X.
00223          10  WS-CK-CARRIER           PIC X.
00224          10  WS-CK-GROUPING          PIC X(6).
00225          10  WS-CK-STATE             PIC XX.
00226          10  WS-CK-ACCOUNT           PIC X(10).
00227          10  WS-CK-CERT-EFF-DT       PIC XX.
00228          10  WS-CK-CERT-NO.
00229              15  WS-CK-CERT-PRIME    PIC X(10).
00230              15  WS-CK-CERT-SFX      PIC X.

00221      05  WS-ERMAIL-KEY               PIC X(33).

040909     05  WS-ELCRTT-KEY.
040909         10  WS-ELCRTT-PRIMARY       PIC X(33).
040909         10  WS-ELCRTT-REC-TYPE      PIC X(1).
040909 01  WS-NUM-TABLE                PIC X(26)  VALUE
040909     '12345678012345070923456789'.
040909 01  FILLER REDEFINES WS-NUM-TABLE.
040909     05  WS-NUM OCCURS 26        PIC 9.
040909 01  V1                          PIC S999 COMP-3.
040909 01  V2                          PIC S999 COMP-3.
040909 01  V3                          PIC S999 COMP-3.
040909 01  WS-WORK-VIN                 PIC X(17)  VALUE SPACES.
040909 01  FILLER REDEFINES WS-WORK-VIN.
040909     05  WS-WORK-VIN-N OCCURS 17        PIC 9.
040909 01  WS-VIN-TOTAL                PIC S9(9)  VALUE +0.
040909 01  WS-VIN-FINAL                PIC S9(7)  VALUE +0.
040909 01  WS-VIN-REMAINDER            PIC S999   VALUE +0.
040909 01  WS-HEX-WORK.
040909     05  FILLER                  PIC X  VALUE LOW-VALUES.
040909     05  WS-HEX-BYTE             PIC X.
040909 01  WS-CHARCD REDEFINES WS-HEX-WORK PIC S9(4)  COMP.
040909 01  WS-CHARCD-A                 PIC S9(4)   COMP VALUE +65.
040909
010412 01  WS-PASS-631.
010412     12  WS-PASS-WORK-AREA         PIC X(384).
010412     12  WS-PASS-PROGRAM-WORK-AREA PIC X(640).
010412     12  FILLER REDEFINES WS-PASS-PROGRAM-WORK-AREA.
010412         COPY ELC631PI.
010412         16  FILLER                PIC X(94).
010412
010412
00233                                      COPY ELCINTF.
00234      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.
00235          16  FILLER                  PIC X(7).
00236          16  PI-TO-EL677-KEY.
00237              20  PI-CHEK-COMP-CD     PIC X.
00238              20  PI-CHEK-CARRIER     PIC X.
00239              20  PI-CHEK-GROUPING    PIC X(6).
00240              20  PI-CHEK-STATE       PIC XX.
00241              20  PI-CHEK-ACCOUNT     PIC X(10).
00242              20  PI-CHEK-EFF-DT      PIC XX.
00243              20  PI-CHEK-CERT-NO     PIC X(10).
00244              20  PI-CHEK-SFX         PIC X.
00245              20  PI-CHEK-SEQUENCE    PIC S9(4)    COMP.
00246          16  FILLER                  PIC X(183).
00247          16  PI-PFKEY                PIC XXX.
00248              88  PI-TO-EL1273-FROM-EL677        VALUE 'PF3'.
00249              88  PI-TO-EL677-FROM-EL1273        VALUE 'PF8'.
00250          16  FILLER                  PIC X(86).
00251          16  PI-1ST-TIME-SW          PIC X.
00252          16  FILLER                  PIC XX.
00253          16  PI-PEND-SW              PIC X.
010412         16  PI-CANCEL-TYPE          PIC X.
010412         16  PI-RES-REF-CLM-TYPE     PIC X.
121312         16  PI-PF11-OK              PIC X.
121312         16  PI-PF14-OK              PIC X.
121312         16  FILLER                  PIC X(318).
00255

052918                                 copy ERCACNT.
00257                                      COPY ELCDATE.
00259                                      COPY EL1273S.

00261                                      COPY ELCJPFX.
00262                                      PIC X(450).
00263
00265                                      COPY ELCCALC.
00268                                      COPY ELCEMIB.
00271                                      COPY ELCLOGOF.
00274                                      COPY ELCATTR.
00277                                      COPY ELCAID.
00278
00279  01  FILLER REDEFINES DFHAID.
00280      05  FILLER                      PIC X(8).
00281      05  PF-VALUES                   PIC X
00282          OCCURS 24 TIMES.
00283
00284  LINKAGE SECTION.
00285
00286  01  DFHCOMMAREA                     PIC X(1024).
00287
       01  var  pic x(30).
00289                                      COPY ELCCERT.
                                           COPY ERCMAIL.
                                           COPY ELCMSTR.
040909                                     COPY ELCCRTT.
010412                                     COPY ELCCRTO.
010412                                     COPY ERCPNDB.                                           
00290
00291      EJECT
00292                                      COPY ELCCNTL.
00293
00294      EJECT
00295                                      COPY ERCCRTC.
00296
00297      EJECT
00298  PROCEDURE DIVISION.
00299
00300      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00301      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00302      MOVE '5'                    TO DC-OPTION-CODE.
00303      PERFORM 8500-DATE-CONVERSION.
00304      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00305      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00306
00307
00308 *    NOTE *******************************************************
00309 *         *                                                     *
00310 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL *
00311 *         *  FROM ANOTHER MODULE.                               *
00312 *         *                                                     *
00313 *         *******************************************************.
00314
00315      IF EIBCALEN NOT > ZERO
00316          MOVE UNACCESS-MSG       TO  LOGOFF-MSG
00317          GO TO 8300-SEND-TEXT.
00318

           display ' entering el1273 '
           set P to address of KIXSYS
           CALL "getenv" using by value P returning var-ptr
           if var-ptr = null then
              display ' kixsys not set '
           else
              set address of var to var-ptr
              move 0 to env-var-len
              inspect var tallying env-var-len
                for characters before X'00' 
      *       DISPLAY '  KIXSYS = ' var (1:env-var-len)
              unstring var (1:env-var-len) delimited by '/'
                 into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
                    WS-KIX-SYS
              end-unstring
      *       DISPLAY ' WS KIX SYS ' WS-KIXSYS
      *       DISPLAY ' WS KIX MYENV ' WS-KIX-MYENV
           end-if

           set P to address of KIXHOST
           CALL "getenv" using by value P returning var-ptr
           if var-ptr = null then
              display ' kixhost not set '
           else
              set address of var to var-ptr
              move 0 to env-var-len
              inspect var tallying env-var-len
                for characters before X'00' 
      *       DISPLAY '  KIXHOST = ' var (1:env-var-len)
              MOVE var(1:env-var-len)  to ws-kixhost
      *       unstring var (1:env-var-len) delimited by '/'
      *          into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
      *             WS-KIX-SYS
      *       end-unstring
              DISPLAY ' WS KIX HOST ' WS-KIXhost
      *       DISPLAY ' WS KIX MYENV ' WS-KIX-MYENV
           end-if




00319      EXEC CICS HANDLE CONDITION
00320          DUPKEY (2100-WRITE-DUPKEY)
00321          ERROR  (9990-ERROR)
00322          QIDERR (0100-DISPLAY-CERTIFICATE)
00323      END-EXEC.
00324
00325      MOVE EIBTRMID               TO  W-QID-TERM.
00326
00327      IF PI-PROCESSOR-ID = 'LGXX'
00328          NEXT SENTENCE
00329      ELSE
00330          EXEC CICS READQ TS
00331              QUEUE   (PI-SECURITY-TEMP-STORE-ID)
00332              INTO    (SECURITY-CONTROL)
00333              LENGTH  (SC-COMM-LENGTH)
00334              ITEM    (SC-ITEM)
00335          END-EXEC
00336          MOVE SC-CREDIT-DISPLAY (33) TO  PI-DISPLAY-CAP
00337          MOVE SC-CREDIT-UPDATE  (33) TO  PI-MODIFY-CAP.
00338
CIDMOD*    IF PI-CALLING-PROGRAM = 'EL689' OR 'EL690'
CIDMOD     IF PI-CALLING-PROGRAM = 'EL689' OR 'EL690' OR 'EL6314'
010412        OR (PI-CALLING-PROGRAM EQUAL 'EL6311' AND
010412            PI-RETURN-TO-PROGRAM EQUAL 'EL1273' AND
010412            EIBAID NOT EQUAL '7')
00340          PERFORM 3100-RECOVER-PI-TS THRU 3100-EXIT
00341          GO TO 0100-DISPLAY-CERTIFICATE.
00342
010412     IF PI-CALLING-PROGRAM EQUAL 'EL6311' AND
010412        PI-RETURN-TO-PROGRAM EQUAL 'EL1273' AND
010412        EIBAID EQUAL '7'
010412           MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6
010412           MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5
010412           MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4
010412           MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3
010412           MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2
010412           MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1
010412           MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM
010412           MOVE THIS-PGM             TO  PI-CALLING-PROGRAM
010412           GO TO 0100-DISPLAY-CERTIFICATE
010412     END-IF.
010412          
00343      IF PI-CALLING-PROGRAM = THIS-PGM
00344          IF PI-1ST-TIME-SW = '1'
00345              MOVE ' '            TO  PI-1ST-TIME-SW
00346              GO TO 0100-DISPLAY-CERTIFICATE.
00347
00348      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00349          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00350              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6
00351              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5
00352              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4
00353              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3
00354              MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2
00355              MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1
00356              MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM
00357              MOVE THIS-PGM             TO  PI-CALLING-PROGRAM
00358              PERFORM 3100-DELETE-PI-TS THRU 3100-EXIT
00359            ELSE
00360              MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM
00361              MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM
00362              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1
00363              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2
00364              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3
00365              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4
00366              MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5
00367              MOVE SPACES               TO  PI-SAVED-PROGRAM-6
00368        ELSE
00369          GO TO 1000-EDIT-MAP.
00370
00371      EJECT
00372  0100-DISPLAY-CERTIFICATE.
00373      IF NOT DISPLAY-CAP
00374          MOVE 'READ'             TO  SM-READ
00375          PERFORM 9995-SECURITY-VIOLATION
00376          MOVE ER-0070            TO  EMI-ERROR
00377          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00378          GO TO 8100-SEND-INITIAL-MAP.
00379
00380      MOVE LOW-VALUES             TO  EL127CO.
00381
00382      MOVE PI-COMPANY-CD          TO  WS-CK-COMPANY-CD.
00383      MOVE PI-CARRIER             TO  WS-CK-CARRIER.
00384      MOVE PI-GROUPING            TO  WS-CK-GROUPING.
00385      MOVE PI-STATE               TO  WS-CK-STATE.
00386      MOVE PI-ACCOUNT             TO  WS-CK-ACCOUNT.
00387      MOVE PI-CERT-NO             TO  WS-CK-CERT-NO.
00388      MOVE PI-CERT-EFF-DT         TO  WS-CK-CERT-EFF-DT.
00389
00390      EXEC CICS HANDLE CONDITION
00391          NOTFND (8880-NOT-FOUND)
00392      END-EXEC.
00393
00394      EXEC CICS READ
00395          DATASET (WS-CERTIFICATE-MASTER-DSID)
00396          RIDFLD  (WS-CERTIFICATE-KEY)
00397          SET     (ADDRESS OF CERTIFICATE-MASTER)
00398      END-EXEC.
00399
00400      IF CM-CLAIM-ATTACHED-COUNT > ZERO
00401         MOVE '*'                TO CASRISKO
              MOVE SPACES             TO WS-DENIAL-TYPE
010412                                   PI-RES-REF-CLM-TYPE              
              PERFORM 2200-GET-CLAIM   THRU 2200-EXIT
              IF (WS-DENIAL-TYPE NOT = SPACES AND LOW-VALUES)
                 AND (CLAIM-IS-CLOSED)
                 EVALUATE WS-DENIAL-TYPE
                    WHEN '1'
                       MOVE 'DENIAL STATUS - DENIAL     '
                                       TO DENSTATO
                    WHEN '2'
                       MOVE 'DENIAL STATUS - RESCISSION '
                                       TO DENSTATO
010412                 MOVE WS-DENIAL-TYPE TO PI-RES-REF-CLM-TYPE
                    WHEN '3'
                       MOVE 'DENIAL STATUS - REFORMATION'
                                       TO DENSTATO
010412                 MOVE WS-DENIAL-TYPE TO PI-RES-REF-CLM-TYPE
                    WHEN '4'
                       MOVE 'DENIAL STAT - END NOT RECVD'
                                       TO DENSTATO
010412                 MOVE WS-DENIAL-TYPE TO PI-RES-REF-CLM-TYPE
                    WHEN OTHER
                       MOVE SPACES     TO DENSTATO
                 END-EVALUATE
                 MOVE AL-SANOF         TO DENSTATA
              END-IF
00402      ELSE
00403          MOVE SPACES             TO  CASRISKO
           END-IF
00404
00405      MOVE CM-CERT-PRIME          TO  CCERTNOO.
00406      MOVE CM-CERT-SFX            TO  CCRTSFXO.
00407      MOVE CM-ACCOUNT             TO  CACCTNOO
00408                                      WS-ACCOUNT.
00409      MOVE CM-STATE               TO  CSTATEO.
00410      MOVE CM-CARRIER             TO  CCARIERO.
00411      MOVE CM-GROUPING            TO  CGROUPO.
00412
00413      IF CM-CERT-EFF-DT  NOT = LOW-VALUES
00414          MOVE SPACES             TO  DC-OPTION-CODE
00415          MOVE CM-CERT-EFF-DT     TO  DC-BIN-DATE-1
00416          PERFORM 8500-DATE-CONVERSION
00417          MOVE DC-GREG-DATE-1-EDIT TO CEFDATEO.
00418
00419      IF CM-MEMB-STATE   NOT = CM-STATE  OR
00420         CM-MEMB-ACCOUNT NOT = WS-ACCT
00421          MOVE AL-SANON           TO  CMEMCAPA
00422          MOVE AL-SANOF           TO  CMEMNOA
00423          MOVE CM-MEMBER-NO       TO  CMEMNOO.
00424
00425      MOVE CM-INSURED-LAST-NAME   TO  CLNAMEO.
00426      MOVE CM-INSURED-FIRST-NAME  TO  CFNAMEO.
00427      MOVE CM-INSURED-INITIAL2    TO  CINITO.
00428      MOVE CM-INSURED-ISSUE-AGE   TO  CAGEO.
00429      MOVE CM-INSURED-JOINT-AGE   TO  CJAGEO.
00430      MOVE CM-BENEFICIARY         TO  CBNAMEO.
00431      MOVE CM-JT-LAST-NAME        TO  CJLNAMEO.
00432      MOVE CM-JT-FIRST-NAME       TO  CJFNAMEO.
00433      MOVE CM-JT-INITIAL          TO  CJINITO.

121410     MOVE AL-SANOF               TO CCLMFLGA
121410     MOVE AL-SADOF               TO CCLMFLHA.
121410     PERFORM 8800-READ-CERT-TRAILER THRU 8800-EXIT.
040909     IF WS-CERT-TRL-REC-NOT-FOUND = ZERO
               if cs-agent-edit-status = 'U' or 'N' or 'R'
                  move cs-agent-name   to agtnameo
               else
                  if (cs-agent-lname not = spaces)
                     or (cs-agent-fname not = spaces)
                     string
                        cs-agent-fname ' '
                        cs-agent-mi    ' '
                        cs-agent-lname ' '
                        delimited by '  ' into agtnameo
                     end-string
                  end-if
               end-if
               move cs-license-no      to agtlicno
               move cs-npn-number      to agtnpnno
020218         move cs-claim-verification-status
020218                                 to cclmhso




121410         IF PI-COMPANY-ID = 'DCC' OR 'VPP' or 'CID'
040909             MOVE CS-VIN-NUMBER TO CVINO
040909         END-IF
121410         IF CS-REFUND-CLAIM-FLAG > SPACES
121410             MOVE CS-REFUND-CLAIM-FLAG TO CCLMFLGO
121410             MOVE AL-UANOF       TO CCLMFLGA
121410             MOVE AL-SANOF       TO CCLMFLHA
121410         END-IF
121712         IF CS-INS-AGE-DEFAULT-FLAG = 'Y'
121712             MOVE '*'            TO CAGEDEFO
121712         ELSE
121712             MOVE ' '            TO CAGEDEFO
121712         END-IF
121712         IF CS-JNT-AGE-DEFAULT-FLAG = 'Y'
121712             MOVE '*'            TO CJAGEDFO
121712         ELSE
121712             MOVE ' '            TO CJAGEDFO
121712         END-IF
040909     END-IF.

102706     IF CM-USER-FIELD = 'Y'
102706        MOVE 'YES'               TO CUSERCDO
102706     ELSE
102706        MOVE SPACES              TO CUSERCDO
102706     END-IF

102706*    MOVE CM-USER-FIELD          TO  CUSERCDO.
00435
00436      IF CM-SSN-STATE   NOT = CM-STATE  OR
00437         CM-SSN-ACCOUNT NOT = WS-ACCT
00438          MOVE CM-SOC-SEC-NO      TO  CSSNO
00439      ELSE
00440          MOVE SPACES             TO  CSSNO.
00441
00442      MOVE CM-LOAN-NUMBER         TO  LOANNOO.
00443
00444      IF PI-COMPANY-ID = 'HER'
00445        IF CM-LOAN-NUMBER = SPACES  OR  ZEROS
00446            MOVE CM-USER-RESERVED TO  LOANNOO.
00447
00448      MOVE CM-LOAN-BALANCE        TO  LOANBALO.
00449      MOVE CM-LOAN-OFFICER        TO  LNOFCO.
00450      MOVE CM-POLICY-FORM-NO      TO  CFORMNOO.
00451
00452      IF (CM-LIVES NUMERIC)
              AND (CM-LIVES > ZEROS)
              PERFORM 2300-READ-ERACCT  THRU 2300-EXIT
              IF (RESP-NORMAL)
                 AND (AM-DCC-PRODUCT-CODE = 'DDF')
                 CONTINUE
              ELSE
00453            MOVE CM-LIVES         TO  CLIVESO
              END-IF
00454      ELSE
00455          MOVE ZERO               TO  CLIVESO
           END-IF
00456
071015*    IF CM-BILLED NUMERIC
071015*        MOVE CM-BILLED          TO  CBILLEDO
071015*    ELSE
071015*        MOVE ZERO               TO  CBILLEDO.
00461
00462      IF PI-COMPANY-ID  EQUAL  'HER'
00463          IF CM-STATE  NOT EQUAL  CM-MEMB-STATE
00464              MOVE CM-MEMBER-NO   TO  WS-MEMBER-NO
00465              IF WS-MEMBER-NO-1-8  IS NUMERIC
00466                  IF WS-MEMBER-NO-1-8  >        ZEROS
00467                      MOVE WS-MEMBER-NO-1-8
00468                                  TO  CISSMICO.
00469
011410*    IF CM-ISS-MICROFILM-NO  IS NUMERIC
011410*        IF CM-ISS-MICROFILM-NO > ZEROS
011410*            MOVE CM-ISS-MICROFILM-NO
011410*                                TO  CISSMICO.
00474
00475 *    IF PI-COMPANY-ID  EQUAL  'HER'
00476 *        MOVE CM-USER-RESERVED   TO  WS-MEMBER-NO
00477 *            IF WS-MEMBER-NO-1-8  IS NUMERIC
00478 *                IF WS-MEMBER-NO-1-8  >        ZEROS
00479 *                    MOVE WS-MEMBER-NO-1-8
00480 *                                TO  CCANMICO.
00481
062017     IF CM-INT-ON-REFS NUMERIC
062017        IF CM-INT-ON-REFS NOT = ZEROS
062017           MOVE CM-INT-ON-REFS TO CNHINTO
072308        END-IF
072308     END-IF
00486
00487      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00488      MOVE '5'                    TO  DC-OPTION-CODE.
00489      PERFORM 8500-DATE-CONVERSION.
00490      MOVE DC-BIN-DATE-1          TO  DC-BIN-DATE-2
00491                                      WS-CURRENT-DATE.
00492      MOVE CM-CERT-EFF-DT         TO  DC-BIN-DATE-1.
00493      MOVE '1'                    TO  DC-OPTION-CODE.
00494      PERFORM 8500-DATE-CONVERSION.
00495
00496      MOVE DC-ELAPSED-MONTHS      TO  WS-ELAPSED-MONTHS.

071015     move ws-certificate-key     to eracct-key
071015     move low-values             to eracct-fill
071015
071015     exec cics read
071015          dataset   ('ERACCT')
071015          into      (account-master)
071015          ridfld    (eracct-key)
071015          gteq
071015          resp      (ws-response)
071015     end-exec
071015     if (resp-normal)
071015        and (ws-certificate-key (1:20) =
071015                         am-control-primary (1:20))
071015        move am-report-code-1    to rptcd1o
071015        move am-report-code-2    to rptcd2o
071015        move am-report-code-3    to rptcd3o
071015     end-if

052918     move am-control-primary (1:20)
052918                                 to ws-eracnt-key
052918     move '2'                    to ws-nt-rec-type
052918     move +3                     to ws-nt-seq-no
052918
052918     exec cics read
052918          dataset   ('ERACNT')
052918          into      (note-file)
052918          ridfld    (ws-eracnt-key)
052918          resp      (ws-response)
052918     end-exec
052918     if (resp-normal)
052918        and (nt-account-special = 'Y')
052918        move 'YES'               to cspcinso
052918     else
052918        move 'NO'                to cspcinso
052918     end-if

00498      IF CM-LF-BENEFIT-CD  = '00'
00499          GO TO 0200-AH-BENEFIT.
00500
00501      EJECT
00502
00503      IF CM-LF-ALT-BENEFIT-AMT IS NOT NUMERIC
00504          MOVE +0                 TO  CM-LF-ALT-BENEFIT-AMT.
00505
00506      IF CM-LF-ALT-PREMIUM-AMT IS NOT NUMERIC
00507          MOVE +0                 TO  CM-LF-ALT-PREMIUM-AMT.
00508
00509      MOVE '4'                    TO  WS-CFK-RECORD-TYPE.
00510      MOVE CM-LF-BENEFIT-CD       TO  WS-BENEFIT-NO
00511                                      CLCDO.
00512      MOVE PI-LIFE-OVERRIDE-L2    TO  CLKINDO.
00513
00514      PERFORM 8700-LOCATE-BENEFIT THRU 8700-EXIT.
00515
00516      IF CF-SUMMARY-PROCESSING (WS-INDEX)
00517          MOVE AL-UNNOF           TO CLTERMA.
00518
00519      MOVE WS-BENEFIT-DESCRIP     TO  CLDESCO.
00520      MOVE CF-LF-COVERAGE-TYPE (WS-INDEX) TO CP-BENEFIT-TYPE.
00521      MOVE CF-CO-EARNINGS-CALC (WS-INDEX) TO CP-EARNING-METHOD.
00522      MOVE CF-SPECIAL-CALC-CD (WS-INDEX) TO CP-SPECIAL-CALC-CD.
00523
00524      MOVE CM-CERT-EFF-DT         TO  CP-CERT-EFF-DT.
00525      MOVE CM-LOAN-1ST-PMT-DT     TO  CP-FIRST-PAY-DATE.
00526      MOVE WS-CURRENT-DATE        TO  CP-VALUATION-DT.
00527
00528      MOVE WS-KIND                TO  CLEDESCO.
00529
           IF CP-EARNING-METHOD = 'B'
              COMPUTE WS-LF-ALT = CM-LF-BENEFIT-AMT
00531                        + CM-LF-ALT-BENEFIT-AMT
           ELSE
              MOVE CM-LF-BENEFIT-AMT TO WS-LF-ALT
           END-IF
00532
00533      MOVE WS-LF-ALT              TO  CLBENO.
00534
00535      MOVE CM-LF-ORIG-TERM        TO  CLTERMO
00536                                      CP-ORIGINAL-TERM.
00537      MOVE PI-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.
00538      MOVE '4'                    TO  CP-REM-TERM-METHOD.
00539      MOVE PI-COMPANY-ID          TO  CP-COMPANY-ID.
00540
00541 *** READ STATE MASTER RECORD FOR FREE LOOK PERIOD ***
00542
00543      PERFORM 8400-READ-STATE-CNTL THRU 8400-EXIT.
00544
00545      IF WS-ST-REC-NOT-FOUND = ZERO
00546         MOVE CF-ST-FREE-LOOK-PERIOD  TO CP-FREE-LOOK
00547      ELSE
00548         MOVE ER-2848             TO EMI-ERROR
00549         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00550         GO TO 8100-SEND-INITIAL-MAP.
00551
00552      PERFORM 9800-LINK-REM-TERM.
00553      MOVE CP-REMAINING-TERM-3    TO  CLREMO.
00554
00555      IF CP-REMAINING-TERM-3 > CM-LF-ORIG-TERM
00556         MOVE CM-LF-ORIG-TERM     TO  CLREMO.
00557
100518     IF CM-LF-CURRENT-STATUS = '8' OR '6'
00559         IF CM-LF-CANCEL-DT NOT = LOW-VALUES
00560             MOVE CM-LF-CANCEL-DT TO DC-BIN-DATE-1
00561             MOVE SPACES          TO DC-OPTION-CODE
00562             PERFORM 8500-DATE-CONVERSION
00563             IF NOT DATE-CONVERSION-ERROR
00564                 MOVE DC-GREG-DATE-1-EDIT TO CLCANCLO.
00565
00566      IF CM-LF-CURRENT-STATUS = '7'
00567         IF CM-LF-DEATH-DT NOT = LOW-VALUES
00568             MOVE CM-LF-DEATH-DT TO DC-BIN-DATE-1
00569             MOVE SPACES          TO DC-OPTION-CODE
00570             PERFORM 8500-DATE-CONVERSION
00571             IF NOT DATE-CONVERSION-ERROR
00572                 MOVE DC-GREG-DATE-1-EDIT TO CLCANCLO.
00573
00574      IF CM-LF-CURRENT-STATUS = '1' OR '4'
00575         IF CP-REMAINING-TERM-3 = ZEROS
00576            MOVE 'EXPIRED'        TO WS-STATUS-DESC
00577            MOVE AL-SABOF         TO CLSTATA
00578         ELSE
00579            MOVE 'ACTIVE'         TO WS-STATUS-DESC.
00580
00581      IF CM-LF-CURRENT-STATUS = '2'
00582         MOVE 'PEND  '            TO WS-STATUS-DESC
00583         MOVE 'P'                 TO PI-PEND-SW.
00584
00585      IF CM-LF-CURRENT-STATUS = '3'
00586         MOVE 'RESTORE'           TO WS-STATUS-DESC.
00587
00588      IF CM-LF-CURRENT-STATUS = '5'
00589         MOVE 'REISSUE'           TO WS-STATUS-DESC.
00590
122002     IF CM-LF-CURRENT-STATUS = 'M'
111005        IF CP-REMAINING-TERM-3 = ZEROS
111005           MOVE 'EXPIRED'        TO WS-STATUS-DESC
111005           MOVE AL-SABOF         TO CLSTATA
111005        ELSE
122002           MOVE 'MONTHLY'        TO WS-STATUS-DESC
111005        END-IF
111005     END-IF
122002
00591      IF CM-LF-CURRENT-STATUS = '6'
100518        MOVE 'LMP BEN'           TO WS-STATUS-DESC.
00593
00594      IF CM-LF-CURRENT-STATUS = '7'
00595         MOVE 'DEATH  '           TO WS-STATUS-DESC.
00596
00597      IF CM-LF-CURRENT-STATUS = '8'
00598         MOVE 'CANCEL '           TO WS-STATUS-DESC.
00599
00600      IF CM-LF-CURRENT-STATUS = '9'
00601         MOVE 'RE-ONLY'           TO WS-STATUS-DESC.
00602
00603      IF CM-LF-CURRENT-STATUS = 'D'
00604         MOVE 'DECLINE'           TO WS-STATUS-DESC.
00605
00606      IF CM-LF-CURRENT-STATUS = 'V'
00607         MOVE 'VOID'              TO WS-STATUS-DESC.
00608
00609      IF CM-POLICY-UNDERWRITTEN  OR
00610         CM-ENTRY-STATUS = 'U'
00611          MOVE ' - UW'            TO WS-UW-STATUS
00612      ELSE
00613          MOVE SPACES             TO WS-UW-STATUS.
00614
00615      MOVE WS-STATUS-DESC         TO CLSTATO.
00616
           IF CP-EARNING-METHOD = 'B'
00617         COMPUTE WS-LF-ALT = CM-LF-PREMIUM-AMT
00618                        + CM-LF-ALT-PREMIUM-AMT
           ELSE
              MOVE CM-LF-PREMIUM-AMT TO WS-LF-ALT
           END-IF
00619
00620      MOVE WS-LF-ALT              TO CLPREMO.
00621
00622  0200-AH-BENEFIT.
00623      IF CM-AH-BENEFIT-CD = '00'
00624          GO TO 0300-CONTINUE.
00625
00626      MOVE '5'                    TO  WS-CFK-RECORD-TYPE.
00627      MOVE CM-AH-BENEFIT-CD       TO  WS-BENEFIT-NO
00628                                      CACDO.
00629      MOVE PI-AH-OVERRIDE-L2      TO  CAKINDO.
00630
00631      PERFORM 8700-LOCATE-BENEFIT THRU 8700-EXIT.
00632
00633      IF  CF-SUMMARY-PROCESSING   (WS-INDEX)
00634          MOVE AL-UNNOF           TO CATERMA.
00635
00636      MOVE 'A'                    TO CP-BENEFIT-TYPE.
00637      MOVE CF-CO-EARNINGS-CALC (WS-INDEX) TO CP-EARNING-METHOD.
00638      MOVE CF-SPECIAL-CALC-CD (WS-INDEX) TO CP-SPECIAL-CALC-CD.
00639      MOVE WS-BENEFIT-DESCRIP     TO  CADESCO.
00640      MOVE WS-KIND                TO  CAEDESCO.
00641      MOVE CM-CERT-EFF-DT         TO  CP-CERT-EFF-DT.
00642      MOVE CM-LOAN-1ST-PMT-DT     TO  CP-FIRST-PAY-DATE.
00643      MOVE WS-CURRENT-DATE        TO  CP-VALUATION-DT.
00644      MOVE CM-AH-ORIG-TERM        TO  CATERMO
00645                                      CP-ORIGINAL-TERM.
00646      MOVE PI-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.
00647      MOVE '4'                    TO  CP-REM-TERM-METHOD.
00648      MOVE PI-COMPANY-ID          TO  CP-COMPANY-ID.
00649
00650 *** READ STATE MASTER RECORD FOR FREE LOOK PERIOD ***
00651
00652      PERFORM 8400-READ-STATE-CNTL THRU 8400-EXIT.
00653
00654      IF WS-ST-REC-NOT-FOUND = ZERO
00655         MOVE CF-ST-FREE-LOOK-PERIOD  TO CP-FREE-LOOK
00656      ELSE
00657         MOVE ER-2848             TO EMI-ERROR
00658         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00659         GO TO 8100-SEND-INITIAL-MAP.
00660
00661      PERFORM 9800-LINK-REM-TERM.
00662      MOVE CP-REMAINING-TERM-3    TO  CAREMO.
00663
00664      MOVE CM-AH-BENEFIT-AMT      TO  CABENO.
00665
00666      IF CM-AH-CURRENT-STATUS = '8'
00667         IF CM-AH-CANCEL-DT  NOT = LOW-VALUES
00668             MOVE CM-AH-CANCEL-DT TO DC-BIN-DATE-1
00669             MOVE SPACES          TO DC-OPTION-CODE
00670             PERFORM 8500-DATE-CONVERSION
00671             IF NOT DATE-CONVERSION-ERROR
00672                 MOVE DC-GREG-DATE-1-EDIT TO CACANCLO.
00673
00674      IF CM-AH-CURRENT-STATUS = '6' OR '7'
00675         IF CM-AH-SETTLEMENT-DT  NOT = LOW-VALUES
00676             MOVE CM-AH-SETTLEMENT-DT TO DC-BIN-DATE-1
00677             MOVE SPACES          TO DC-OPTION-CODE
00678             PERFORM 8500-DATE-CONVERSION
00679             IF NOT DATE-CONVERSION-ERROR
00680                 MOVE DC-GREG-DATE-1-EDIT TO CACANCLO.
00681
00682      IF CM-AH-CURRENT-STATUS = '1' OR '4'
00683         IF CP-REMAINING-TERM-3 = ZEROS
00684            MOVE 'EXPIRED'        TO WS-STATUS-DESC
00685            MOVE AL-SABOF         TO CASTATA
00686         ELSE
00687            MOVE 'ACTIVE'         TO WS-STATUS-DESC.
00688
00689      IF CM-AH-CURRENT-STATUS = '2'
00690         MOVE 'PEND  '            TO WS-STATUS-DESC
00691         MOVE 'P'                 TO PI-PEND-SW.
00692
00693      IF CM-AH-CURRENT-STATUS = '3'
00694         MOVE 'RESTORE'           TO WS-STATUS-DESC.
00695
00696      IF CM-AH-CURRENT-STATUS = '5'
00697         MOVE 'REISSUE'           TO WS-STATUS-DESC.
00698
122002     IF CM-AH-CURRENT-STATUS = 'M'
111005        IF CP-REMAINING-TERM-3 = ZEROS
111005           MOVE 'EXPIRED'        TO WS-STATUS-DESC
111005           MOVE AL-SABOF         TO CASTATA
111005        ELSE
122002           MOVE 'MONTHLY'        TO WS-STATUS-DESC
111005        END-IF
111005     END-IF
122002
00699      IF CM-AH-CURRENT-STATUS = '6'
00700         MOVE 'LMP DIS'           TO WS-STATUS-DESC.
00701
00702      IF CM-AH-CURRENT-STATUS = '7'
00703         MOVE 'DEATH  '           TO WS-STATUS-DESC.
00704
00705      IF CM-AH-CURRENT-STATUS = '8'
00706         MOVE 'CANCEL '           TO WS-STATUS-DESC.
00707
00708      IF CM-AH-CURRENT-STATUS = '9'
00709         MOVE 'RE-ONLY'           TO WS-STATUS-DESC.
00710
00711      IF CM-AH-CURRENT-STATUS = 'D'
00712         MOVE 'DECLINE'           TO WS-STATUS-DESC.
00713
00714      IF CM-AH-CURRENT-STATUS = 'V'
00715         MOVE 'VOID'              TO WS-STATUS-DESC.
00716
00717      IF CM-POLICY-UNDERWRITTEN  OR
00718         CM-ENTRY-STATUS = 'U'
00719          MOVE ' - UW'            TO WS-UW-STATUS
00720      ELSE
00721          MOVE SPACES             TO WS-UW-STATUS.
00722
00723      MOVE WS-STATUS-DESC         TO CASTATO.
00724
00725      MOVE CM-AH-PREMIUM-AMT      TO CAPREMO.
00726
00727  0300-CONTINUE.
00728      IF CM-NOTE-SW EQUAL ' '
00729         MOVE 'NO '               TO  CNOTESO
00730      ELSE
00731         MOVE 'YES'               TO  CNOTESO.
00732
00733      MOVE CM-INSURED-SEX         TO  CSEXO.
00734      MOVE CM-LOAN-APR            TO  CAPRO.
00735      MOVE CM-IND-GRP-TYPE        TO  CINDGRPO.
00736      MOVE CM-PREMIUM-TYPE        TO  CPREMTPO.
00737
00738      IF CM-SING-PRM
00739         MOVE 'SP'                TO CPTDESCO
00740         ELSE
00741         IF CM-O-B-COVERAGE
00742            MOVE 'OB'             TO CPTDESCO
00743            ELSE
00744            IF CM-OPEN-END
00745               MOVE 'OE'          TO CPTDESCO.
00746
00747 *    IF CM-CLAIM-DEDUCT-WITHHELD NOT NUMERIC
00748 *        MOVE ZEROS              TO CCLMDEDO
00749 *    ELSE
00750 *        MOVE CM-CLAIM-DEDUCT-WITHHELD
00751 *                                TO CCLMDEDO.
00752
00753 *    IF CM-CANCEL-DEDUCT-WITHHELD NOT NUMERIC
00754 *        MOVE ZEROS              TO CCANDEDO
00755 *    ELSE
00756 *        MOVE CM-CANCEL-DEDUCT-WITHHELD
00757 *                                TO CCANDEDO.
00758
071015*    IF PI-COMPANY-ID = 'DMD'
071015*       MOVE ZEROS               TO CCLMDEDO
071015*                                   CCANDEDO.
00762
00763      MOVE CM-CSR-CODE            TO  CCSRCDO.
00764
00765      IF CERT-ADDED-ONLINE
00766         MOVE ' PENDING ISSUE ONLINE'     TO CNOTE2O.
00767      IF CERT-PEND-ISSUE-ERROR
00768         MOVE ' PENDING ISSUE IN ERROR'   TO CNOTE2O.
00769      IF CERT-PURGED-OFFLINE
00770         MOVE ' PURGED FROM OFFLINE   '   TO CNOTE2O.
00771      IF CERT-PEND-ISSUE-RETURNED
00772         MOVE ' CERTIFICATE RETURNED  '   TO CNOTE2O.
00773      IF CERT-CANCELLED-ONLINE
00774         MOVE ' PENDING CANCEL ONLINE'    TO CNOTE3O.
00775      IF CERT-PEND-CANCEL-ERROR
00776         MOVE ' PENDING CANCEL IN ERROR'  TO CNOTE3O.
00777      IF CERT-PEND-CANCEL-VOID
00778         MOVE ' PENDING VOID OF CANCEL '  TO CNOTE3O.
00779      IF CERT-PEND-CAN-VOID-ERROR
00780         MOVE ' PENDING VOID IN ERROR  '  TO CNOTE3O.
00781      IF CERT-PEND-CANCEL-RETURNED
00782         MOVE ' CANCEL RETURNED        '  TO CNOTE3O.
00783
00784      MOVE AL-UANOF               TO  CMEMNOA   CLNAMEA   CINITA
00785                                      CFNAMEA   CSEXA     CSSNA.
00786      MOVE AL-UNNOF               TO  CAGEA     CAPRA     CJAGEA
071015*                                    CCLMDEDA  CCANDEDA.
00788
00789      IF PI-MAIL-YES
00790          MOVE AL-SANON               TO  PF6KEYA
00791      ELSE
00792          MOVE AL-SADOF               TO  PF6KEYA.
00793
00794      IF PI-COMPANY-ID = 'DMD'
00795        IF PI-RETURN-TO-PROGRAM = 'EL150' OR 'EL131'
00796          MOVE 'PF6=CERT CHANGES'     TO  PF6KEYO
00797          MOVE AL-SANON               TO  PF6KEYA.
00798
00799      IF CERT-WAS-CREATED-FOR-CLAIM
00800          MOVE 'CERTIFICATE WAS CREATED FOR A CLAIM'
00801                                  TO CNOTE1O
00802      ELSE
00803          MOVE AL-SADOF           TO CNOTE1A.
00804
00805      IF CERT-WAS-CREATED-FOR-CLAIM
00806          MOVE AL-UANON           TO  CLCDA      CACDA
00807                                      CLTERMA    CATERMA
00808                                      CLPREMA    CAPREMA
00809                                      CLBENA     CABENA
00810          GO TO 8100-SEND-INITIAL-MAP.
00811

101005        IF (PI-COMPANY-ID = 'DCC' OR 'VPP')
101005           AND (CM-ENTRY-STATUS = '5')
101005           MOVE AL-UANON         TO CABENA
101005                                    CACDA
101005                                    CATERMA
101005           GO TO 8100-SEND-INITIAL-MAP
101005        END-IF

00812      IF NOT CM-O-B-COVERAGE AND CM-OPEN-END
00813          GO TO 8100-SEND-INITIAL-MAP.
00814
00815      IF (CM-LF-CURRENT-STATUS = '5' OR
00816          CM-AH-CURRENT-STATUS = '5')
00817                    AND
00818          CM-ENTRY-STATUS = '5'
00819             MOVE AL-UANON        TO  CLCDA      CACDA
00820                                      CLTERMA    CATERMA
00821                                      CLPREMA    CAPREMA
00822                                      CLBENA     CABENA.
00823
00824      GO TO 8100-SEND-INITIAL-MAP.
00825
00826      EJECT
00827  1000-EDIT-MAP.
00828      MOVE LOW-VALUES             TO EL127CI.
00829
00830      IF EIBAID = DFHCLEAR
00831          GO TO 9400-CLEAR.
00832
00833      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00834          MOVE ER-0008            TO EMI-ERROR
00835          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00836          GO TO 8200-SEND-DATAONLY.
00837
00838      EXEC CICS RECEIVE
00839          MAPSET (WS-MAPSET-NAME)
00840          MAP    (WS-MAP-NAME)
00841          INTO   (EL127CI)
00842      END-EXEC.
00843
00844      IF CEMSG2L = 0
00845          GO TO 1100-CHECK-PFKEYS.
00846
00847      IF EIBAID NOT = DFHENTER
00848          MOVE ER-0008            TO EMI-ERROR
00849          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00850          MOVE -1                 TO CEMSG2L
00851          GO TO 8200-SEND-DATAONLY.
00852
00853      IF (CEMSG2I NUMERIC) AND (CEMSG2I > 0 AND < 25)
00854          MOVE PF-VALUES (CEMSG2I)    TO  EIBAID
00855      ELSE
00856          MOVE ER-0029                    TO  EMI-ERROR
00857          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00858          MOVE -1                 TO CEMSG2L
00859          GO TO 8200-SEND-DATAONLY.
00860
00861  1100-CHECK-PFKEYS.
00862
00863      IF EIBAID = DFHPF23
00864          GO TO 9000-RETURN-CICS.
00865
00866      IF EIBAID = DFHPF24
00867          MOVE 'EL126 '           TO  THIS-PGM
00868          GO TO 9300-XCTL.
00869
121312     IF EIBAID NOT EQUAL DFHPF11
121312         MOVE 'N' TO PI-PF11-OK
121312     END-IF
121312     IF EIBAID NOT EQUAL DFHPF14
121312         MOVE 'N' TO PI-PF14-OK
121312     END-IF
121312
092412     IF EIBAID = DFHPF1
092412         MOVE PI-COMPANY-CD      TO  WS-CK-COMPANY-CD
092412         MOVE PI-CARRIER         TO  WS-CK-CARRIER
092412         MOVE PI-GROUPING        TO  WS-CK-GROUPING
092412         MOVE PI-STATE           TO  WS-CK-STATE
092412         MOVE PI-ACCOUNT         TO  WS-CK-ACCOUNT
092412         MOVE PI-CERT-NO         TO  WS-CK-CERT-NO
092412         MOVE PI-CERT-EFF-DT     TO  WS-CK-CERT-EFF-DT
092412
092412         MOVE WS-CERTIFICATE-KEY     TO ERACCT-KEY
092412         MOVE LOW-VALUES             TO ERACCT-FILL
092412
092412         EXEC CICS READ
092412              DATASET   ('ERACCT')
092412              INTO      (ACCOUNT-MASTER)
092412              RIDFLD    (ERACCT-KEY)
092412              GTEQ
092412              RESP      (WS-RESPONSE)
092412         END-EXEC
092412        IF (RESP-NORMAL)
092412           AND (WS-CERTIFICATE-KEY (1:20) =
092412              AM-CONTROL-PRIMARY (1:20))
092412           IF AM-NAME NOT GREATER THAN SPACES OR
092412               AM-ADDRS NOT GREATER THAN SPACES  OR
092412               AM-ADDR-CITY NOT GREATER THAN SPACES OR
092412               AM-ADDR-STATE NOT GREATER THAN SPACES OR
092412               AM-ZIP NOT GREATER THAN SPACES
092412               MOVE ER-3834             TO EMI-ERROR
092412               PERFORM 9900-ERROR-FORMAT
092412                                 THRU 9900-EXIT
092412               GO TO 8200-SEND-DATAONLY
092412           END-IF
092412        ELSE
092412           MOVE ER-1609             TO EMI-ERROR
092412           PERFORM 9900-ERROR-FORMAT
092412                                THRU 9900-EXIT
092412           GO TO 8200-SEND-DATAONLY
092412        END-IF
092412
092412        MOVE WS-CERTIFICATE-KEY    TO WS-ERMAIL-KEY
092412
092412        EXEC CICS READ
092412           DATASET   ('ERMAIL')
092412           SET       (ADDRESS OF MAILING-DATA)
092412           RIDFLD    (WS-ERMAIL-KEY)
092412           RESP      (WS-RESPONSE)
092412        END-EXEC
092412
092412        IF RESP-NORMAL
092412           IF (MA-ADDRESS-LINE-1 NOT GREATER THAN SPACES AND
092412               MA-ADDRESS-LINE-2 NOT GREATER THAN SPACES)  OR
092412               MA-CITY NOT GREATER THAN SPACES OR
092412               MA-ADDR-STATE NOT GREATER THAN SPACES OR
092412               MA-ZIP-CODE NOT GREATER THAN SPACES
092412               MOVE ER-3835             TO EMI-ERROR
092412               PERFORM 9900-ERROR-FORMAT
092412                                 THRU 9900-EXIT
092412               GO TO 8200-SEND-DATAONLY
092412           END-IF
092412        ELSE
092412           MOVE ER-3000             TO EMI-ERROR
092412           PERFORM 9900-ERROR-FORMAT
092412                                THRU 9900-EXIT
092412           GO TO 8200-SEND-DATAONLY
092412        END-IF
092412
092412     END-IF.
092412
00870      IF EIBAID = DFHPF1
00871          IF PI-COMPANY-ID = 'AIG' OR 'AUK'
00872              MOVE ER-0029        TO  EMI-ERROR
00873              MOVE -1             TO  CEMSG2L
00874              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00875              GO TO 8200-SEND-DATAONLY
00876          ELSE
00877              MOVE 'PF1'          TO  PI-PFKEY
00878              PERFORM 3000-WRITE-PI-TS THRU 3000-EXIT
00879              MOVE LOW-VALUES     TO  PI-PROGRAM-WORK-AREA
00880              MOVE 'EL689'        TO  THIS-PGM
00881              GO TO 9300-XCTL.
00882
00883      IF EIBAID = DFHPF2
00884          IF PI-COMPANY-ID = 'AIG' OR 'AUK'
00885              MOVE ER-0029        TO  EMI-ERROR
00886              MOVE -1             TO  CEMSG2L
00887              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00888              GO TO 8200-SEND-DATAONLY
00889          ELSE
00890              MOVE 'PF2'          TO  PI-PFKEY
00891              PERFORM 3000-WRITE-PI-TS THRU 3000-EXIT
00892              MOVE LOW-VALUES     TO  PI-PROGRAM-WORK-AREA
00893              MOVE 'EL690'        TO  THIS-PGM
00894              GO TO 9300-XCTL.
00895
00896      IF EIBAID = DFHPF3
00897          MOVE 'EL1274'           TO  THIS-PGM
00898          GO TO 9300-XCTL.
00899
00900      IF EIBAID = DFHPF4
00901          MOVE ' '                TO  PI-1ST-TIME-SW
00902          MOVE 'EL1275'           TO  THIS-PGM
00903          GO TO 9300-XCTL.
00904
00905      IF EIBAID = DFHPF5
00906          IF PI-COMPANY-ID = 'AIG' OR 'AUK'
00907              MOVE ER-0029        TO  EMI-ERROR
00908              MOVE -1             TO  CEMSG2L
00909              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00910              GO TO 8200-SEND-DATAONLY
00911          ELSE
00912          IF PI-COMPANY-ID = 'DMD'
00913              MOVE 'EL401DMD'     TO  THIS-PGM
00914              GO TO 9300-XCTL
00915           ELSE
101509*00916              MOVE 'EL1276'       TO  THIS-PGM
101509             MOVE 'EL1279'       TO  THIS-PGM
00917              GO TO 9300-XCTL.
00918
00919      IF PI-COMPANY-ID NOT = 'DMD'
00920      IF PI-MAIL-YES
00921          IF EIBAID = DFHPF6
00922              IF PI-COMPANY-ID = 'AIG' OR 'AUK'
00923                  MOVE ER-0029    TO  EMI-ERROR
00924                  MOVE -1         TO  CEMSG2L
00925                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00926                  GO TO 8200-SEND-DATAONLY
00927              ELSE
00928                  MOVE 'EL1277'   TO  THIS-PGM
00929                  GO TO 9300-XCTL.
00930
00931      IF PI-COMPANY-ID = 'DMD'
00932         IF EIBAID = DFHPF6
00933            IF PI-RETURN-TO-PROGRAM = 'EL150' OR 'EL131'
00934                MOVE 'EL400DMD'   TO  THIS-PGM
00935                GO TO 9300-XCTL.
00936
00937      IF EIBAID = DFHPF7
00938          IF PI-COMPANY-ID = 'AIG' OR 'AUK'
00939              MOVE ER-0029        TO  EMI-ERROR
00940              MOVE -1             TO  CEMSG2L
00941              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00942              GO TO 8200-SEND-DATAONLY
00943          ELSE
00944              MOVE 'EL1278'       TO  THIS-PGM
00945              GO TO 9300-XCTL.
00946
00947      IF EIBAID = DFHPF8
00948          IF PI-COMPANY-ID = 'AIG' OR 'AUK'
00949              MOVE ER-0029                TO  EMI-ERROR
00950              MOVE -1                     TO  CEMSG2L
00951              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00952              GO TO 8200-SEND-DATAONLY
00953          ELSE
00954              IF NOT PI-TO-EL1273-FROM-EL677
00955                  MOVE PI-COMPANY-CD      TO  PI-CHEK-COMP-CD
00956                  MOVE PI-CARRIER         TO  PI-CHEK-CARRIER
00957                  MOVE PI-GROUPING        TO  PI-CHEK-GROUPING
00958                  MOVE PI-STATE           TO  PI-CHEK-STATE
00959                  MOVE PI-ACCOUNT         TO  PI-CHEK-ACCOUNT
00960                  MOVE PI-CERT-EFF-DT     TO  PI-CHEK-EFF-DT
00961                  MOVE PI-CERT-PRIME      TO  PI-CHEK-CERT-NO
00962                  MOVE PI-CERT-SFX        TO  PI-CHEK-SFX
00963                  MOVE +1                 TO  PI-CHEK-SEQUENCE
00964                  MOVE 'PF8'              TO  PI-PFKEY
00965                  MOVE 'EL677'            TO  THIS-PGM
00966                  GO TO 9300-XCTL
00967              ELSE
00968                  IF PI-RETURN-TO-PROGRAM = 'EL677'
00969                      GO TO 9400-CLEAR.
00970
010412*    IF EIBAID = DFHPF9
010412*       MOVE 'EL1280'            TO  THIS-PGM
010412*       GO TO 9300-XCTL
010412*    END-IF

           IF EIBAID = DFHPF15
072312        IF NOT PI-PROCESSOR-IS-CSR
072312           MOVE ER-0070     TO  EMI-ERROR
072312           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
072312            GO TO 8200-SEND-DATAONLY
072312        END-IF
120313        IF PI-PEND-SW = 'P'
120313           MOVE ER-7049     TO  EMI-ERROR
120313           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
120313            GO TO 8200-SEND-DATAONLY
120313        END-IF
              MOVE 'PF15'              TO PI-PFKEY
              PERFORM 3000-WRITE-PI-TS THRU 3000-EXIT
              MOVE '1'                 TO PI-PROGRAM-WORK-AREA
120313        MOVE 'EL6314'            TO THIS-PGM
              GO TO 9300-XCTL
           END-IF

PEMTST     IF EIBAID = DFHPF10
PEMTST        MOVE 'PF10'              TO PI-PFKEY
PEMTST        PERFORM 3000-WRITE-PI-TS THRU 3000-EXIT
PEMTST        MOVE '1'                 TO PI-PROGRAM-WORK-AREA
PEMTST        MOVE PI-CARRIER          TO PI-CR-CARRIER
PEMTST        MOVE PI-GROUPING         TO PI-CR-GROUPING
PEMTST        MOVE PI-STATE            TO PI-CR-STATE
PEMTST        MOVE PI-ACCOUNT          TO PI-CR-ACCOUNT
PEMTST        MOVE 'EL650 '            TO THIS-PGM
PEMTST        GO TO 9300-XCTL
PEMTST     END-IF

121312     IF EIBAID = DFHPF11
121312        IF PI-PF11-OK NOT = 'Y'
121312           MOVE 'Y'              TO PI-PF11-OK
121312           MOVE ER-3838          TO  EMI-ERROR
121312           MOVE -1               TO  CEMSG2L
121312           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
121312           GO TO 8200-SEND-DATAONLY
121312        END-IF
121312     END-IF
121312     IF EIBAID = DFHPF14
121312        IF PI-PF14-OK NOT = 'Y'
121312           MOVE 'Y'              TO PI-PF14-OK
121312           MOVE ER-3838          TO  EMI-ERROR
121312           MOVE -1               TO  CEMSG2L
121312           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
121312           GO TO 8200-SEND-DATAONLY
121312        END-IF
121312     END-IF
090612     IF EIBAID = DFHPF14 OR DFHPF11
072312        IF NOT PI-PROCESSOR-IS-CSR
072312           MOVE ER-0070     TO  EMI-ERROR
072312           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
072312            GO TO 8200-SEND-DATAONLY
072312        END-IF
010412        IF EIBAID = DFHPF11
010412            MOVE '1'        TO PI-CANCEL-TYPE
010412        ELSE
010412            MOVE '3'        TO PI-CANCEL-TYPE
010412        END-IF
010412        PERFORM 4000-CLAIM-RESC-REFORM THRU 4000-EXIT
010412
010412        PERFORM 3000-WRITE-PI-TS THRU 3000-EXIT
010412        MOVE PROGRAM-INTERFACE-BLOCK TO WS-PASS-631
010412        MOVE LOW-VALUES     TO WS-PASS-PROGRAM-WORK-AREA
010412        MOVE PI-COMPANY-CD  TO PI-PB-COMPANY-CD
010412        MOVE CG-BATCH-NO    TO PI-PB-ENTRY-BATCH
010412        IF PI-CANCEL-TYPE EQUAL '1'
010412           MOVE 1           TO PI-PB-BATCH-SEQ-NO
010412        ELSE
010412           MOVE 2           TO PI-PB-BATCH-SEQ-NO
010412        END-IF
010412        MOVE 'Y'            TO PI-ALL-ISSUES-SW
010412                               PI-ALL-CANCELS-SW
010412                               PI-CSR-SESSION-SW
010412        MOVE 'N'            TO PI-ISSUES-IN-ERROR-SW
010412                               PI-CANCELS-IN-ERROR-SW
010412                               PI-ONLY-BATCH-HEADERS-SW
010412                               PI-ALL-OUT-OF-BAL-SW
010412                               PI-HOLD-REC-SW
010412                               PI-CHANGE-REC-SW
010412                               PI-CHK-REQ-REC-SW
010412                               PI-ISSUE-WARNING-SW
010412                               PI-CANCEL-WARNING-SW
010412        MOVE '1'            TO PI-BROWSE-TYPE
010412        MOVE DFHENTER       TO  EIBAID
010412
010412        EXEC CICS XCTL
010412            PROGRAM    ('EL6311')
010412            COMMAREA   (WS-PASS-631)
010412            LENGTH     (1300)
010412        END-EXEC
           END-IF

CIDMOD     .
00971  1110-CHECK-PF12.
00972      IF EIBAID = DFHPF12
00973         MOVE 'EL010 '            TO  THIS-PGM
00974         GO TO 9300-XCTL.
00975
00976      IF NOT MODIFY-CAP
00977          MOVE 'UPDATE'           TO  SM-READ
00978          PERFORM 9995-SECURITY-VIOLATION
00979          MOVE ER-0070            TO  EMI-ERROR
00980          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00981          GO TO 8100-SEND-INITIAL-MAP.
00982
00983      IF EIBAID NOT = DFHENTER
00984          MOVE ER-0008            TO EMI-ERROR
00985          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00986          GO TO 8200-SEND-DATAONLY.
00987
00988      IF PI-COMPANY-ID = 'CRI'
00989          IF CLAIM-SESSION
00990              IF PI-CERT-SFX = 'X'
00991                  NEXT SENTENCE
00992              ELSE
00993                  MOVE ER-0692    TO  EMI-ERROR
00994                  MOVE -1         TO  CEMSG2L
00995                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00996                  GO TO 8200-SEND-DATAONLY.
00997
071015*    IF PI-COMPANY-ID NOT = 'DMD'
071015*        MOVE ZEROS             TO CBILLEDL.
01000
01001      IF CMEMNOL  > ZERO OR
01002         CLNAMEL  > ZERO OR
01003         CFNAMEL  > ZERO OR
01004         CLIVESL  > ZERO OR
071015*       CBILLEDL > ZERO OR
01006         CINITL   > ZERO
01007          MOVE +1                 TO  WS-UPDATE-SW.
01008
           IF CLNAMEL > ZERO
              IF CLNAMEI = SPACES
                 MOVE AL-UNBON         TO CLNAMEA
                 MOVE -1               TO CLNAMEL
                 MOVE ER-0236          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF

01009      IF CAGEL > ZERO
01010          IF CAGEI NUMERIC
01011              MOVE AL-UNNON       TO  CAGEA
01012              MOVE +1             TO  WS-UPDATE-SW
01013              MOVE CAGEI          TO  WS-CAGEI
01014          ELSE
01015              MOVE AL-UNBON       TO  CAGEA
01016              MOVE -1             TO  CAGEL
01017              MOVE ER-2352        TO  EMI-ERROR
01018              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01019
01020      IF CSEXL > ZERO
01021          IF CSEXI = 'M' OR 'F'
01022              MOVE AL-UANON       TO  CSEXA
01023              MOVE +1             TO  WS-UPDATE-SW
01024          ELSE
01025              MOVE AL-UABON       TO  CSEXA
01026              MOVE -1             TO  CSEXL
01027              MOVE ER-2351        TO  EMI-ERROR
01028              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01029
01030      IF CSSNL    > ZERO OR
01031         CJLNAMEL > ZERO OR
01032         CJFNAMEL > ZERO OR
01033         CJINITL  > ZERO
01034          MOVE +1                 TO  WS-UPDATE-SW.
01035
01036      IF CJAGEL > ZERO
01037          IF CJAGEI NUMERIC
01038              MOVE AL-UNNON       TO  CJAGEA
01039              MOVE +1             TO  WS-UPDATE-SW
01040              MOVE CJAGEI         TO  WS-CJAGEI
01041          ELSE
01042              MOVE AL-UNBON       TO  CJAGEA
01043              MOVE -1             TO  CJAGEL
01044              MOVE ER-2352        TO  EMI-ERROR
01045              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01046
01047      IF CBNAMEL > ZERO OR
01048         LOANNOL > ZERO
01049          MOVE +1                 TO  WS-UPDATE-SW.
01050
040909     IF CVINL > 0
031416      if cvini = spaces
031416        move +1 to ws-update-sw
031416      else
031416        IF (PI-COMPANY-ID = 'VPP' OR 'CID')
031416                 OR
031416          ((PI-COMPANY-ID = 'DCC')
031416          AND (PI-CARRIER = '2' OR '4' OR '6'))
040909            MOVE CVINI             TO WS-WORK-VIN
040909            MOVE +8                TO V2
040909            MOVE ZEROS             TO WS-VIN-TOTAL
040909            PERFORM VARYING V1 FROM +1 BY +1 UNTIL
040909                (V1 > 17)
040909                IF WS-WORK-VIN (V1:1) = ' ' OR 'O' OR 'I' OR 'Q'
040909                   MOVE ER-3825     TO EMI-ERROR
040909                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
040909                ELSE
040909                   IF V1 = 8
040909                      MOVE 10       TO V2
040909                   END-IF
040909                   IF V1 = 9
040909                      MOVE 0        TO V2
040909                   END-IF
040909                   IF V1 = 10
040909                      MOVE 9        TO V2
040909                   END-IF
040909                   IF WS-WORK-VIN (V1:1) NUMERIC
040909                      COMPUTE WS-VIN-TOTAL = WS-VIN-TOTAL
040909                       + (WS-WORK-VIN-N (V1) * V2)
040909                   ELSE
040909                      MOVE WS-WORK-VIN (V1:1) TO WS-HEX-BYTE
040909                      COMPUTE V3 = (WS-CHARCD - WS-CHARCD-A) + 1
040909                      COMPUTE WS-VIN-TOTAL = WS-VIN-TOTAL
040909                          + (WS-NUM (V3) * V2)
040909                   END-IF
040909                   COMPUTE V2 = V2 - 1
040909                END-IF
040909            END-PERFORM
040909            MOVE ZEROS TO WS-VIN-REMAINDER
040909            DIVIDE WS-VIN-TOTAL BY 11 GIVING WS-VIN-FINAL
040909                REMAINDER WS-VIN-REMAINDER
040909            IF ((WS-WORK-VIN (9:1) = 'X')
040909                AND (WS-VIN-REMAINDER = 10))
040909                           OR
040909                (WS-WORK-VIN-N (9) = WS-VIN-REMAINDER)
040909                MOVE +1             TO WS-UPDATE-SW
040909            ELSE
031416             IF WS-WORK-VIN (1:1) > 0 AND < 6
031416                MOVE ER-3826     TO emi-ERROR
031416                move -1          to cvinl
031416                PERFORM 9900-ERROR-FORMAT
031416                                 THRU 9900-EXIT
031416             ELSE
031416                MOVE ER-3827     TO emi-ERROR
031416                move -1          to cvinl
031416                PERFORM 9900-ERROR-FORMAT
031416                                 THRU 9900-EXIT
031416             END-IF
031416            end-if
031416        end-if
031416      end-if
031416     end-if

01051      IF LOANBALL > ZERO
01052          EXEC CICS BIF
01053              DEEDIT
01054              FIELD  (LOANBALI)
01055              LENGTH (11)
01056          END-EXEC
01057          IF LOANBALI NOT > 999999999
01058              MOVE LOANBALI           TO  WS-LOAN-BAL
01059              MOVE +1                 TO  WS-UPDATE-SW
01060          ELSE
01061              MOVE -1                 TO  LOANBALL
01062              MOVE AL-UNBON           TO  LOANBALA
01063              MOVE ER-7233            TO  EMI-ERROR
01064              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01065
01066      IF LNOFCL > ZERO
01067          MOVE +1                 TO  WS-UPDATE-SW.
01068
01069      IF CAPRL > ZERO
01070          EXEC CICS BIF
01071              DEEDIT
CIDMOD             FIELD  (CAPRI)
CIDMOD             LENGTH (8)
01074          END-EXEC
01072 **           FIELD  (LOANBALI)
01073 **           LENGTH (11)
01075          IF CAPRI NOT > 9999999
01076              MOVE CAPRI              TO  WS-APR
01077              MOVE AL-UNNON           TO  CAPRA
01078              MOVE +1                 TO  WS-UPDATE-SW
01079            ELSE
01080              MOVE AL-UNBON           TO  CAPRA
01081              MOVE -1                 TO  CAPRL
01082              MOVE ER-2354            TO  EMI-ERROR
01083              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01084
01085      IF CFORMNOL > ZERO  OR
01086         CUSERCDL > ZERO  OR
01087         CINDGRPL > ZERO
01088          MOVE +1                 TO  WS-UPDATE-SW.
01089
01090      IF CINDGRPL > ZERO
01091          IF CINDGRPI = 'I' OR 'G'
01092              MOVE AL-UANON       TO  CINDGRPA
01093              MOVE +1             TO  WS-UPDATE-SW
01094          ELSE
01095              MOVE AL-UABON       TO  CINDGRPA
01096              MOVE -1             TO  CINDGRPL
01097              MOVE ER-2152        TO  EMI-ERROR
01098              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01099
01100      IF CPREMTPL > ZERO
01101          IF CPREMTPI = '1' OR '2' OR '3'
01102              MOVE +1             TO  WS-UPDATE-SW
01103          ELSE
01104              MOVE -1             TO  CPREMTPL
01105              MOVE AL-UABON       TO  CPREMTPA
01106              MOVE ER-7234        TO  EMI-ERROR
01107              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01108
01109 *    IF CCLMDEDL > ZERO
01110 *        EXEC CICS BIF
01111 *            DEEDIT
01112 *            FIELD  (CCLMDEDI)
01113 *            LENGTH (8)
01114 *        END-EXEC
01115 *        IF CCLMDEDI NOT > 9999999
01116 *            MOVE CCLMDEDI           TO  WS-CLM-DEDUCT
01117 *            MOVE AL-UNNON           TO  CCLMDEDA
01118 *            MOVE +1                 TO  WS-UPDATE-SW
01119 *          ELSE
01120 *            MOVE AL-UNBON           TO  CCLMDEDA
01121 *            MOVE -1                 TO  CCLMDEDL
01122 *            MOVE ER-7242            TO  EMI-ERROR
01123 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01124 *
01125 *    IF CCANDEDL > ZERO
01126 *        EXEC CICS BIF
01127 *            DEEDIT
01128 *            FIELD  (CCANDEDI)
01129 *            LENGTH (8)
01130 *        END-EXEC
01131 *        IF CCANDEDL NOT > 9999999
01132 *            MOVE CCANDEDI           TO  WS-CAN-DEDUCT
01133 *            MOVE AL-UNNON           TO  CCANDEDA
01134 *            MOVE +1                 TO  WS-UPDATE-SW
01135 *          ELSE
01136 *            MOVE AL-UNBON           TO  CCANDEDA
01137 *            MOVE -1                 TO  CCANDEDL
01138 *            MOVE ER-7242            TO  EMI-ERROR
01139 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01140
01141      IF CLIVESL > ZERO
01142          IF CLIVESI NUMERIC
01143              MOVE CLIVESI                TO  DEEDIT-FIELD
01144              PERFORM 8600-DEEDIT
01145              IF DEEDIT-FIELD-V0 NOT > 9999999
01146                  MOVE AL-UNNON           TO  CLIVESA
01147                  MOVE +1                 TO  WS-UPDATE-SW
01148                  MOVE DEEDIT-FIELD-V0    TO  WS-LIVES
01149                ELSE
01150                  MOVE AL-UNBON           TO  CLIVESA
01151                  MOVE -1                 TO  CLIVESL
01152                  MOVE ER-2223            TO  EMI-ERROR
01153                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01154          ELSE
01155              MOVE AL-UNBON       TO  CLIVESA
01156              MOVE -1             TO  CLIVESL
01157              MOVE ER-2547        TO  EMI-ERROR
01158              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01159
071015*    IF PI-COMPANY-ID NOT = 'DMD'
071015*        MOVE ZEROS             TO CBILLEDL.
01162
071015*    IF CBILLEDL > ZERO
071015*        IF CBILLEDI NUMERIC
071015*            MOVE CBILLEDI               TO  DEEDIT-FIELD
071015*            PERFORM 8600-DEEDIT
071015*            IF DEEDIT-FIELD-V0 NOT > 9999999
071015*                MOVE AL-UNNON           TO  CBILLEDA
071015*                MOVE +1                 TO  WS-UPDATE-SW
071015*                MOVE DEEDIT-FIELD-V0    TO  WS-BILLED
071015*              ELSE
071015*                MOVE AL-UNBON           TO  CBILLEDA
071015*                MOVE -1                 TO  CBILLEDL
071015*                MOVE ER-2223            TO  EMI-ERROR
071015*                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
071015*        ELSE
071015*            MOVE AL-UNBON       TO  CBILLEDA
071015*            MOVE -1             TO  CBILLEDL
071015*            MOVE ER-2547        TO  EMI-ERROR
071015*            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
121410
121410     IF CCLMFLGL > ZERO
121410        IF CCLMFLGI = SPACES OR '1' OR '2' OR '3'                  
121410           MOVE +1               TO  WS-UPDATE-SW
121410        ELSE                                                    
121410           MOVE -1               TO CCLMFLGL                       
121410           MOVE AL-UABON         TO CCLMFLGA                       
121410           MOVE ER-3036          TO EMI-ERROR                     
121410           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
121410        END-IF
121410     END-IF.
01181
01182      MOVE 'NNN'                  TO  WS-VAL-TABLE.
01183
01184      IF CLCDL > ZERO
01185          MOVE '4'                TO  WS-CFK-RECORD-TYPE
01186          MOVE CLCDI              TO  WS-BENEFIT-NO
01187          PERFORM  8700-LOCATE-BENEFIT  THRU  8700-EXIT
01188          IF BENEFIT-FOUND
01189              MOVE AL-UNNON       TO  CLCDA
01190              MOVE +1             TO  WS-UPDATE-SW
01191              MOVE 'Y'            TO  WS-VAL-CD
01192          ELSE
01193              MOVE AL-UNBON       TO  CLCDA
01194              MOVE -1             TO  CLCDL
01195              MOVE ER-0151        TO  EMI-ERROR
01196              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01197              GO TO 1000-TEST-ERROR.
01198
01199      IF CLTERML > ZERO
01200          MOVE CLTERMI            TO  DEEDIT-FIELD
01201          PERFORM 8600-DEEDIT
01202          IF DEEDIT-FIELD-V0 > ZEROS
01203              MOVE AL-UNNON       TO  CLTERMA
01204              MOVE +1             TO  WS-UPDATE-SW
01205              MOVE DEEDIT-FIELD-V0
01206                                  TO  WS-LF-ORIG-TERM
01207              MOVE 'Y'            TO  WS-VAL-TERM
01208          ELSE
01209              MOVE AL-UNBON       TO  CLTERMA
01210              MOVE -1             TO  CLTERML
01211              MOVE ER-2223        TO  EMI-ERROR
01212              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01213              GO TO 1000-TEST-ERROR.
01214
01215      IF CLPREML > ZERO
01216          EXEC CICS BIF
01217              DEEDIT
01218              FIELD  (CLPREMI)
01219              LENGTH (11)
01220          END-EXEC
01221          MOVE +1                 TO  WS-UPDATE-SW
01222          MOVE CLPREMI            TO  WS-LF-PREM.
01223
01224      IF CLBENL > ZERO
01225          EXEC CICS BIF
01226              DEEDIT
01227              FIELD  (CLBENI)
01228              LENGTH (14)
01229          END-EXEC
01230          IF CLBENI > ZEROS
01231              MOVE +1             TO  WS-UPDATE-SW
01232              MOVE 'Y'            TO  WS-VAL-BENE
01233              MOVE CLBENI         TO  WS-LF-BENE
01234          ELSE
01235              MOVE AL-UNBON       TO  CLBENA
01236              MOVE -1             TO  CLBENL
01237              MOVE ER-2223        TO  EMI-ERROR
01238              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01239              GO TO 1000-TEST-ERROR.
01240
01241       IF WS-VAL-TABLE = 'NNN' OR 'YYY'
01242           NEXT SENTENCE
01243       ELSE
01244           MOVE AL-UNBON          TO  CLCDA
01245           MOVE -1                TO  CLCDL
01246           MOVE ER-7248           TO  EMI-ERROR
01247           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01248           GO TO 1000-TEST-ERROR.
01249
01250      MOVE 'NNN'                  TO  WS-VAL-TABLE.
01251
01252      IF CACDL > ZERO
01253          MOVE '5'                TO  WS-CFK-RECORD-TYPE
01254          MOVE CACDI              TO  WS-BENEFIT-NO
01255          PERFORM  8700-LOCATE-BENEFIT  THRU  8700-EXIT
01256          IF BENEFIT-FOUND
01257              MOVE AL-UNNON       TO  CACDA
01258              MOVE +1             TO  WS-UPDATE-SW
01259              MOVE 'Y'            TO  WS-VAL-CD
01260          ELSE
01261              MOVE AL-UNBON       TO  CACDA
01262              MOVE -1             TO  CACDL
01263              MOVE ER-0151        TO  EMI-ERROR
01264              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01265              GO TO 1000-TEST-ERROR.
01266
01267      IF CATERML > ZERO
01268          MOVE CATERMI            TO  DEEDIT-FIELD
01269          PERFORM 8600-DEEDIT
01270          IF DEEDIT-FIELD-V0 > ZEROS
01271              MOVE AL-UNNON       TO  CATERMA
01272              MOVE +1             TO  WS-UPDATE-SW
01273              MOVE DEEDIT-FIELD-V0
01274                                  TO  WS-AH-ORIG-TERM
01275              MOVE 'Y'            TO  WS-VAL-TERM
01276            ELSE
01277              MOVE AL-UNBON       TO  CATERMA
01278              MOVE -1             TO  CATERML
01279              MOVE ER-2223        TO  EMI-ERROR
01280              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01281              GO TO 1000-TEST-ERROR.
01282
01283      IF CAPREML > ZERO
01284          EXEC CICS BIF
01285              DEEDIT
01286              FIELD  (CAPREMI)
01287              LENGTH (11)
01288          END-EXEC
01289          MOVE +1                 TO  WS-UPDATE-SW
01290          MOVE CAPREMI            TO  WS-AH-PREM.
01291
01292      IF CABENL > ZERO
01293          EXEC CICS BIF
01294              DEEDIT
01295              FIELD  (CABENI)
01296              LENGTH (14)
01297          END-EXEC
01298          IF CABENI > ZEROS
01299              MOVE +1             TO  WS-UPDATE-SW
01300              MOVE 'Y'            TO  WS-VAL-BENE
01301              MOVE CABENI         TO  WS-AH-BENE
01302          ELSE
01303              MOVE AL-UNBON       TO  CATERMA
01304              MOVE -1             TO  CATERML
01305              MOVE ER-2223        TO  EMI-ERROR
01306              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01307              GO TO 1000-TEST-ERROR.
01308
01309       IF WS-VAL-TABLE = 'NNN' OR 'YYY'
01310           NEXT SENTENCE
01311       ELSE
01312           MOVE AL-UNBON          TO  CACDA
01313           MOVE -1                TO  CACDL
01314           MOVE ER-7248           TO  EMI-ERROR
01315           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01316
01317      MOVE 'NNN'                  TO  WS-VAL-TABLE.
01318
01319  1000-TEST-ERROR.
01320
01321      IF EMI-NO-ERRORS
01322          NEXT SENTENCE
01323      ELSE
01324          GO TO 8200-SEND-DATAONLY.
01325
01326      IF NO-UPDATES-MADE
01327          GO TO 0100-DISPLAY-CERTIFICATE.
01328
01329      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
01330          GO TO 1000-CONTINUE-EDITS.
01331
01332 ******************************************************************
01333 *       READ CONTROL FILE TO ACQUIRE CURRENT MONTH END DATE      *
01334 ******************************************************************
01335
01336      MOVE LOW-VALUES             TO  WS-CONTROL-FILE-KEY.
01337      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
01338      MOVE '1'                    TO  WS-CFK-RECORD-TYPE.
01339      MOVE SPACES                 TO  WS-CFK-ACCESS.
01340      MOVE +0                     TO  WS-CFK-SEQUENCE-NO.
01341
01342      EXEC CICS READ
01343          DATASET  (WS-CONTROL-FILE-DSID)
01344          RIDFLD   (WS-CONTROL-FILE-KEY)
01345          SET      (ADDRESS OF CONTROL-FILE)
01346      END-EXEC.
01347
01348 ******************************************************************
01349 *  ACQUIRE STORAGE FOR PENDING CERTIFICATE MAINTENANCE FILE AND  *
01350 *  INITIALIZE NUMERIC FIELDS                                     *
01351 ******************************************************************
01352
01353      EXEC CICS GETMAIN
01354          SET     (ADDRESS OF PENDING-MAINT-TO-CERT-FILE)
01355          LENGTH  (300)
01356          INITIMG (WS-SPACES)
01357      END-EXEC.
01358
01359      MOVE ZEROS                  TO  CC-FILE-SEQ-NO
01360                                      CC-LAST-MAINT-HHMMSS
01361                                      CC-AGE
01362                                      CC-INSURED-JOINT-AGE
01363                                      CC-PAY-FREQUENCY
01364                                      CC-LOAN-APR
01365                                      CC-LIVES
01366                                      CC-BILLED
01367                                      CC-LF-ORIG-TERM
01368                                      CC-AH-ORIG-TERM
01369                                      CC-LOAN-BALANCE
01370                                      CC-CLAIM-DEDUCT-WITHHELD
01371                                      CC-CANCEL-DEDUCT-WITHHELD.
01372
01373      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
01374      MOVE '5'                    TO  DC-OPTION-CODE.
01375      PERFORM 8500-DATE-CONVERSION.
01376      MOVE DC-BIN-DATE-1          TO  CC-LAST-MAINT-DT.
01377
01378  1000-CONTINUE-EDITS.
01379
01380      MOVE PI-COMPANY-CD          TO  WS-CK-COMPANY-CD.
01381      MOVE PI-CARRIER             TO  WS-CK-CARRIER.
01382      MOVE PI-GROUPING            TO  WS-CK-GROUPING.
01383      MOVE PI-STATE               TO  WS-CK-STATE.
01384      MOVE PI-ACCOUNT             TO  WS-CK-ACCOUNT.
01385      MOVE PI-CERT-NO             TO  WS-CK-CERT-NO.
01386      MOVE PI-CERT-EFF-DT         TO  WS-CK-CERT-EFF-DT.
01387
01388      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
01389          NEXT SENTENCE
01390      ELSE
01391          MOVE WS-CERTIFICATE-KEY TO  CC-CONTROL-PRIMARY
01392                                      WS-SAVE-CC-KEY.
01393
01394      EXEC CICS READ UPDATE
01395          DATASET (WS-CERTIFICATE-MASTER-DSID)
01396          RIDFLD  (WS-CERTIFICATE-KEY)
01397          SET     (ADDRESS OF CERTIFICATE-MASTER)
01398      END-EXEC.
01399
01400      IF CERT-ADDED-BATCH
01401          NEXT SENTENCE
01402      ELSE
01403          MOVE ER-0588            TO EMI-ERROR
01404          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01405          GO TO 8200-SEND-DATAONLY.
01406
01407      IF CMEMNOL > ZERO
01408          MOVE CMEMNOI            TO  CM-MEMBER-NO.
01409
           IF CLNAMEL > ZERO
              IF (CLNAMEI NOT = SPACES)
                 AND (CLNAMEI (1:1) = SPACES)
                 PERFORM UNTIL CLNAMEI (1:1) NOT = SPACES
                    MOVE CLNAMEI (2:14)   TO CLNAMEI
                 END-PERFORM
              END-IF
           END-IF

01410      IF CLNAMEL > ZERO
01411          MOVE CLNAMEI            TO  CM-INSURED-LAST-NAME.
01412
           IF CFNAMEL > ZERO
              IF (CFNAMEI NOT = SPACES)
                 AND (CFNAMEI (1:1) = SPACES)
                 PERFORM UNTIL CFNAMEI (1:1) NOT = SPACES
                    MOVE CFNAMEI (2:9)    TO CFNAMEI
                 END-PERFORM
              END-IF
           END-IF

01413      IF CFNAMEL > ZERO
01414          MOVE CFNAMEI            TO  CM-INSURED-FIRST-NAME
01415                                      CM-INSURED-INITIAL1.
01416
01417      IF CINITL > ZERO
01418          MOVE CINITI             TO  CM-INSURED-INITIAL2.
01419
01420      IF CAGEL > ZERO
01421          MOVE WS-CAGEI           TO  CM-INSURED-ISSUE-AGE.
01422
01423      IF CSEXL > ZERO
01424          MOVE CSEXI              TO  CM-INSURED-SEX.
01425
01426      IF CSSNL > ZERO
01427          MOVE CSSNI              TO  CM-SOC-SEC-NO.
01428
           IF CJLNAMEL > ZERO
              IF (CJLNAMEI NOT = SPACES)
                 AND (CJLNAMEI (1:1) = SPACES)
                 PERFORM UNTIL CJLNAMEI (1:1) NOT = SPACES
                    MOVE CJLNAMEI (2:14)  TO CJLNAMEI
                 END-PERFORM
              END-IF
           END-IF

01429      IF CJLNAMEL > ZERO
01430          MOVE CJLNAMEI           TO  CM-JT-LAST-NAME.
01431
01432      IF CJLNAMEL > ZERO
01433         IF PI-COMPANY-ID EQUAL 'LBL'
01434             MOVE '4'            TO  WS-CFK-RECORD-TYPE
01435             MOVE CM-LF-BENEFIT-CD TO  WS-BENEFIT-NO
01436             PERFORM 8700-LOCATE-BENEFIT THRU 8700-EXIT
01437             IF WS-JOINT-COVERAGE
01438                 MOVE CJLNAMEI    TO  CM-MEMBER-NO
01439                                      CMEMNOO.
01440
           IF CJFNAMEL > ZERO
              IF (CJFNAMEI NOT = SPACES)
                 AND (CJFNAMEI (1:1) = SPACES)
                 PERFORM UNTIL CJFNAMEI (1:1) NOT = SPACES
                    MOVE CJFNAMEI (2:9)   TO CJFNAMEI
                 END-PERFORM
              END-IF
           END-IF

01441      IF CJFNAMEL > ZERO
01442          MOVE CJFNAMEI           TO  CM-JT-FIRST-NAME.
01443
01444      IF CJINITL > ZERO
01445          MOVE CJINITI            TO  CM-JT-INITIAL.
01446
01447      IF CJAGEL > ZERO
01448          MOVE WS-CJAGEI          TO  CM-INSURED-JOINT-AGE.
01449
01450      IF CBNAMEL > ZERO
01451          MOVE CBNAMEI            TO  CM-BENEFICIARY.
01452
01453      IF LOANNOL > ZERO
01454          MOVE LOANNOI            TO  CM-LOAN-NUMBER.
01455
01456      IF LOANBALL > ZERO
01457          MOVE WS-LOAN-BAL        TO  CM-LOAN-BALANCE
01458                                      LOANBALO.
01459
01460      IF LNOFCL > ZERO
01461          MOVE LNOFCI             TO  CM-LOAN-OFFICER.
01462
01463      IF CAPRL > ZERO
01464          MOVE WS-APR             TO  CM-LOAN-APR
01465                                      CAPRO.
01466
01467      IF CFORMNOL > ZERO
01468          MOVE CFORMNOI           TO  CM-POLICY-FORM-NO.
01469
01470      IF CUSERCDL > ZERO
01471          MOVE CUSERCDI           TO  CM-USER-FIELD.
01472
01473      IF CINDGRPL > ZERO
01474          MOVE CINDGRPI           TO  CM-IND-GRP-TYPE.
01475
01476      IF CPREMTPL > ZERO
01477          MOVE CPREMTPI           TO  CM-PREMIUM-TYPE.
01478
01479 *    IF CCLMDEDL > ZERO
01480 *        MOVE WS-CLM-DEDUCT      TO  CM-CLAIM-DEDUCT-WITHHELD
01481 *                                    CCLMDEDO.
01482 *
01483 *    IF CCANDEDL > ZERO
01484 *        MOVE WS-CAN-DEDUCT      TO  CM-CANCEL-DEDUCT-WITHHELD
01485 *                                    CCANDEDO.
01486
01487      IF CLIVESL > ZERO
01488         MOVE WS-LIVES            TO  CM-LIVES
01489                                      CLIVESO.
01490
071015*    IF PI-COMPANY-ID NOT = 'DMD'
071015*        MOVE ZEROS              TO CBILLEDL.
071015*
071015*    IF CBILLEDL > ZERO
071015*       MOVE WS-BILLED           TO  CM-BILLED
071015*                                    CBILLEDO.
01497
01498      IF CLCDL > ZERO
01499         MOVE CLCDI               TO  CM-LF-BENEFIT-CD.
01500
01501      IF CLTERML > ZERO
01502         MOVE WS-LF-ORIG-TERM     TO  CM-LF-ORIG-TERM
01503                                      CLTERMO
01504                                      DC-ELAPSED-MONTHS
01505         MOVE CM-CERT-EFF-DT      TO  DC-BIN-DATE-1
01506         MOVE '6'                 TO  DC-OPTION-CODE
01507         PERFORM 8500-DATE-CONVERSION
01508         IF NO-CONVERSION-ERROR
01509             MOVE DC-BIN-DATE-2   TO  CM-LF-LOAN-EXPIRE-DT.
01510
01511      IF CLPREML > ZERO
01512         MOVE WS-LF-PREM          TO  CM-LF-PREMIUM-AMT.
01513
01514      IF CLBENL > ZERO
01515         MOVE WS-LF-BENE          TO  CM-LF-BENEFIT-AMT.
01516
01517      IF CACDL > ZERO
01518         MOVE CACDI               TO  CM-AH-BENEFIT-CD.
01519
01520      IF CATERML > ZERO
01521         MOVE WS-AH-ORIG-TERM     TO  CM-AH-ORIG-TERM
01522                                      CATERMO
01523                                      DC-ELAPSED-MONTHS
01524         MOVE CM-CERT-EFF-DT      TO  DC-BIN-DATE-1
01525         MOVE '6'                 TO  DC-OPTION-CODE
01526         PERFORM 8500-DATE-CONVERSION
01527         IF NO-CONVERSION-ERROR
01528             MOVE DC-BIN-DATE-2   TO  CM-AH-LOAN-EXPIRE-DT.
01529
01530      IF CAPREML > ZERO
01531         MOVE WS-AH-PREM          TO  CM-AH-PREMIUM-AMT.
01532
01533      IF CABENL > ZERO
01534         MOVE WS-AH-BENE          TO  CM-AH-BENEFIT-AMT.
01535
01536      IF CM-CLAIM-ATTACHED-COUNT < +1
01537          MOVE ZERO               TO  CM-CLAIM-ATTACHED-COUNT
01538          MOVE SPACES             TO  CM-CLAIM-INTERFACE-SW.
01539
01540      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
01541          GO TO 1050-CONTINUE-EDITS.
01542
01543      MOVE 'CC'                   TO  CC-RECORD-ID.
01544      MOVE CM-INSURED-LAST-NAME   TO  CC-INSURED-LAST-NAME.
01545      MOVE CM-INSURED-FIRST-NAME  TO  CC-INSURED-FIRST-NAME.
01546      MOVE CM-INSURED-INITIAL2    TO  CC-INSURED-INITIAL2.
01547      MOVE CM-INSURED-ISSUE-AGE   TO  CC-AGE.
01548      MOVE CM-INSURED-JOINT-AGE   TO  CC-INSURED-JOINT-AGE.
01549      MOVE LOW-VALUES             TO  CC-BIRTHDAY
01550                                      CC-CREDIT-ACCEPT-DT
01551      MOVE CM-INSURED-SEX         TO  CC-INSURED-SEX.
01552      MOVE CM-LOAN-APR            TO  CC-LOAN-APR.
01553      MOVE CM-JT-LAST-NAME        TO  CC-JT-LAST-NAME.
01554      MOVE CM-JT-FIRST-NAME       TO  CC-JT-FIRST-NAME.
01555      MOVE CM-JT-INITIAL          TO  CC-JT-INITIAL.
01556      MOVE CM-BENEFICIARY         TO  CC-BENEFICIARY.
01557      MOVE CM-LOAN-NUMBER         TO  CC-LOAN-NUMBER.
01558      MOVE CM-LOAN-BALANCE        TO  CC-LOAN-BALANCE.
01559      MOVE CM-LOAN-OFFICER        TO  CC-LOAN-OFFICER.
01560      MOVE CM-PAY-FREQUENCY       TO  CC-PAY-FREQUENCY.
01561      MOVE CM-POLICY-FORM-NO      TO  CC-POLICY-FORM-NO.
01562      MOVE CM-PREMIUM-TYPE        TO  CC-PREMIUM-TYPE.
01563
01564 *    IF CM-CLAIM-DEDUCT-WITHHELD NUMERIC
01565 *        MOVE CM-CLAIM-DEDUCT-WITHHELD
01566 *                                TO  CC-CLAIM-DEDUCT-WITHHELD.
01567 *    IF CM-CANCEL-DEDUCT-WITHHELD NUMERIC
01568 *        MOVE CM-CANCEL-DEDUCT-WITHHELD
01569 *                                TO  CC-CANCEL-DEDUCT-WITHHELD.
01570
01571      MOVE CM-IND-GRP-TYPE        TO  CC-IND-GRP-TYPE.
01572      MOVE CM-USER-FIELD          TO  CC-USER-FIELD.
01573      MOVE CM-SOC-SEC-NO          TO  CC-SOC-SEC-NO.
01574      MOVE CM-MEMBER-NO           TO  CC-MEMBER-NO.
01575      MOVE CM-LF-ORIG-TERM        TO  CC-LF-ORIG-TERM.
01576      MOVE CM-AH-ORIG-TERM        TO  CC-AH-ORIG-TERM.
01577      MOVE CM-LIVES               TO  CC-LIVES
01578
01579      IF CM-BILLED NUMERIC
01580          MOVE CM-BILLED          TO  CC-BILLED
01581        ELSE
01582          MOVE ZEROS              TO  CC-BILLED.
01583
01584      MOVE CM-LF-BENEFIT-CD       TO  CC-LF-BENEFIT-CD.
01585      MOVE CM-LF-PREMIUM-AMT      TO  CC-LF-PREMIUM-AMT.
01586      MOVE CM-LF-BENEFIT-AMT      TO  CC-LF-BENEFIT-AMT.
01587      MOVE CM-LF-LOAN-EXPIRE-DT   TO  CC-LF-EXPIRY-DT.
01588      MOVE CM-AH-BENEFIT-CD       TO  CC-AH-BENEFIT-CD.
01589      MOVE CM-AH-PREMIUM-AMT      TO  CC-AH-PREMIUM-AMT.
01590      MOVE CM-AH-BENEFIT-AMT      TO  CC-AH-BENEFIT-AMT.
01591      MOVE CM-AH-LOAN-EXPIRE-DT   TO  CC-AH-EXPIRY-DT.
01592
01593      MOVE CF-CURRENT-MONTH-END   TO  CC-CREDIT-SELECT-DT.
01594      MOVE EIBTIME                TO  CC-LAST-MAINT-HHMMSS.
01595      MOVE PI-PROCESSOR-ID        TO  CC-LAST-MAINT-BY.
01596
01597  1050-CONTINUE-EDITS.

           IF PI-COMPANY-ID NOT = 'DCC' and 'VPP'
              IF CBNAMEL > 0
                 MOVE WS-CERTIFICATE-KEY
                                       TO WS-ERMAIL-KEY
                 EXEC CICS READ
                    UPDATE
                    DATASET   ('ERMAIL')
                    RIDFLD    (WS-ERMAIL-KEY)
                    SET       (ADDRESS OF MAILING-DATA)
                    RESP      (WS-RESPONSE)
                 END-EXEC
                 IF RESP-NORMAL
                    IF CBNAMEI NOT = MA-CRED-BENE-NAME
                       MOVE CBNAMEI    TO MA-CRED-BENE-NAME
                       EXEC CICS REWRITE
                          DATASET      ('ERMAIL')
                          FROM         (MAILING-DATA)
                          RESP         (WS-RESPONSE)
                       END-EXEC
                    END-IF
                 END-IF
              END-IF
           END-IF

01599      EXEC CICS REWRITE
01600          DATASET (WS-CERTIFICATE-MASTER-DSID)
01601          FROM    (CERTIFICATE-MASTER)
01602      END-EXEC
01603
040909
040909     IF CVINL > 0
040909        IF ((PI-COMPANY-ID = 'DCC') AND
040909           (PI-CARRIER = '2' OR '4' OR '6'))
101615                       or
101615            (pi-company-id = 'CID' OR 'VPP')
040909            MOVE WS-CERTIFICATE-KEY TO WS-ELCRTT-PRIMARY
040909            MOVE 'C'                TO WS-ELCRTT-REC-TYPE
040909
040909            EXEC CICS READ
040909                UPDATE
040909                DATASET  (WS-CERT-TRAILERS-DSID)
040909                RIDFLD   (WS-ELCRTT-KEY)
040909                SET      (ADDRESS OF CERTIFICATE-TRAILERS)
040909                RESP     (WS-RESPONSE)
040909            END-EXEC
040909            IF RESP-NORMAL
040909               IF CVINI NOT EQUAL CS-VIN-NUMBER
040909                   MOVE CVINI TO CS-VIN-NUMBER
040909                   EXEC CICS REWRITE
040909                      DATASET  (WS-CERT-TRAILERS-DSID)
040909                      FROM     (CERTIFICATE-TRAILERS)
040909                      RESP     (WS-RESPONSE)
040909                   END-EXEC
040909               END-IF
040909            ELSE
040909               IF RESP-NOTFND
040909                  MOVE SPACES       TO CERTIFICATE-TRAILERS
040909                  MOVE 'CS'         TO CS-RECORD-ID
040909                  MOVE WS-CERTIFICATE-KEY TO WS-ELCRTT-PRIMARY
040909                  MOVE 'C'          TO WS-ELCRTT-REC-TYPE
040909                  MOVE WS-ELCRTT-KEY TO CS-CONTROL-PRIMARY
040909                  MOVE CVINI        TO CS-VIN-NUMBER
040909                  EXEC CICS WRITE
040909                     DATASET  (WS-CERT-TRAILERS-DSID)
040909                     RIDFLD   (WS-ELCRTT-KEY)
040909                     FROM     (CERTIFICATE-TRAILERS)
040909                     RESP     (WS-RESPONSE)
040909                  END-EXEC
040909               END-IF
040909            END-IF
040909        END-IF
040909     END-IF.
040909
121712     IF CCLMFLGL > ZERO OR 
121712       CAGEL > ZERO OR 
121712       CJAGEL > ZERO
121410         MOVE WS-CERTIFICATE-KEY TO WS-ELCRTT-PRIMARY
121410         MOVE 'C'                TO WS-ELCRTT-REC-TYPE
121410
121410         EXEC CICS READ
121410             UPDATE
121410             DATASET  (WS-CERT-TRAILERS-DSID)
121410             RIDFLD   (WS-ELCRTT-KEY)
121410             SET      (ADDRESS OF CERTIFICATE-TRAILERS)
121410             RESP     (WS-RESPONSE)
121410         END-EXEC
121410         IF RESP-NORMAL
121410            IF CCLMFLGI NOT EQUAL CS-REFUND-CLAIM-FLAG
121410                MOVE CCLMFLGI TO CS-REFUND-CLAIM-FLAG
121712            END-IF
121712            IF CAGEI > ZERO
121712                MOVE 'N' TO CS-INS-AGE-DEFAULT-FLAG
121712            END-IF
121712            IF CJAGEI > ZERO
121712                MOVE 'N' TO CS-JNT-AGE-DEFAULT-FLAG
121712            END-IF
121712            EXEC CICS REWRITE
121410                   DATASET  (WS-CERT-TRAILERS-DSID)
121410                   FROM     (CERTIFICATE-TRAILERS)
121410                   RESP     (WS-RESPONSE)
121712            END-EXEC
121410        END-IF
121410     END-IF.
121410
01604      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
01605          MOVE +1                 TO  WS-COMPLETED-SUCCESSFUL
01606          GO TO 0100-DISPLAY-CERTIFICATE.
01607
01608      EXEC CICS HANDLE CONDITION
01609          DUPREC (2000-DUPREC)
01610      END-EXEC.
01611
01612      MOVE PENDING-MAINT-TO-CERT-FILE TO WS-SAVE-CERT-CHANGE-REC.
01613
01614      IF CERT-WAS-CREATED-FOR-CLAIM
01615          NEXT SENTENCE
01616      ELSE
01617          EXEC CICS WRITE
01618              DATASET (WS-CERT-MAINT-FILE-DSID)
01619              RIDFLD  (CC-CONTROL-PRIMARY)
01620              FROM    (PENDING-MAINT-TO-CERT-FILE)
01621          END-EXEC.
01622
01623      MOVE +1                     TO  WS-COMPLETED-SUCCESSFUL.
01624
01625      GO TO 0100-DISPLAY-CERTIFICATE.
01626
01627  2000-DUPREC.
01628      MOVE WS-SAVE-CC-KEY         TO  CC-CONTROL-PRIMARY.
01629
01630      EXEC CICS READ UPDATE
01631          DATASET (WS-CERT-MAINT-FILE-DSID)
01632          RIDFLD  (CC-CONTROL-PRIMARY)
01633          INTO    (PENDING-MAINT-TO-CERT-FILE)
01634      END-EXEC.
01635
01636      MOVE WS-SAVE-CERT-CHANGE-REC TO PENDING-MAINT-TO-CERT-FILE.
01637
01638      EXEC CICS REWRITE
01639          DATASET (WS-CERT-MAINT-FILE-DSID)
01640          FROM    (PENDING-MAINT-TO-CERT-FILE)
01641          END-EXEC.
01642      MOVE +1                     TO  WS-COMPLETED-SUCCESSFUL.
01643
01644      GO TO 0100-DISPLAY-CERTIFICATE.
01645
01646  2100-WRITE-DUPKEY.
01647      MOVE +1                     TO  WS-COMPLETED-SUCCESSFUL.
01648
01649      GO TO 0100-DISPLAY-CERTIFICATE.

       2200-GET-CLAIM.

           MOVE SPACES             TO WS-DONE-SW
           MOVE PI-COMPANY-CD      TO MSTR5-COMP-CD
           MOVE CM-CERT-NO         TO MSTR5-CERT-NO
           EXEC CICS STARTBR
05731         DATASET    ('ELMSTR5')
05732         RIDFLD     (ELMSTR5-KEY)
              RESP       (WS-RESPONSE)
05733      END-EXEC
           IF RESP-NORMAL
              PERFORM UNTIL I-AM-DONE
                 EXEC CICS READNEXT
05742               DATASET   ('ELMSTR5')
05743               SET       (ADDRESS OF CLAIM-MASTER)
05744               RIDFLD    (ELMSTR5-KEY)
                    RESP      (WS-RESPONSE)
05745            END-EXEC
                 IF (RESP-NORMAL)
                    OR (RESP-DUPKEY)
                    OR (RESP-DUPREC)
                    IF (PI-COMPANY-CD NOT = MSTR5-COMP-CD)
                       OR (CL-CERT-NO NOT = MSTR5-CERT-NO)
                       SET I-AM-DONE      TO TRUE
                    END-IF
                    IF (CL-COMPANY-CD = CM-COMPANY-CD)
                       AND (CL-CERT-CARRIER = CM-CARRIER)
                       AND (CL-CERT-GROUPING = CM-GROUPING)
                       AND (CL-CERT-STATE = CM-STATE)
                       AND (CL-CERT-ACCOUNT = CM-ACCOUNT)
                       AND (CL-CERT-EFF-DT = CM-CERT-EFF-DT)
                       AND (CL-CERT-NO = CM-CERT-NO)
                       MOVE CL-DENIAL-TYPE TO WS-DENIAL-TYPE
                       SET I-AM-DONE TO TRUE
                    END-IF
                 ELSE
                    SET I-AM-DONE TO TRUE
                 END-IF
              END-PERFORM
              EXEC CICS ENDBR
05731            DATASET    ('ELMSTR5')
                 RESP       (WS-RESPONSE)
05733         END-EXEC
           END-IF

           .
       2200-EXIT.
           EXIT.

       2300-READ-ERACCT.
           MOVE CM-CONTROL-PRIMARY     TO ERACCT-KEY
           MOVE LOW-VALUES             TO ERACCT-FILL

           EXEC CICS READ
                DATASET   ('ERACCT')
                INTO      (ACCOUNT-MASTER)
                RIDFLD    (ERACCT-KEY)
                GTEQ
                RESP      (WS-RESPONSE)
           END-EXEC

           .
       2300-EXIT.
           EXIT.
01652  3000-WRITE-PI-TS.
01653
01654      EXEC CICS WRITEQ TS
01655          QUEUE  (W-QID)
01656          FROM   (PROGRAM-INTERFACE-BLOCK)
01657          LENGTH (PI-COMM-LENGTH)
01658          ITEM   (W-ONE)
01659      END-EXEC.
01660
01661  3000-EXIT.
01662      EXIT.
01663      EJECT
01664  3100-RECOVER-PI-TS.
01665
01666      EXEC CICS HANDLE CONDITION
01667          QIDERR  (3100-QID-ERROR)
01668      END-EXEC.
01669
01670      EXEC CICS READQ TS
01671          QUEUE  (W-QID)
01672          LENGTH (PI-COMM-LENGTH)
01673          INTO   (PROGRAM-INTERFACE-BLOCK)
01674          ITEM   (W-ONE)
01675      END-EXEC.
01676
01677  3100-DELETE-PI-TS.
01678
01679      EXEC CICS DELETEQ TS
01680          QUEUE  (W-QID)
01681      END-EXEC.
01682
01683      GO TO 3100-EXIT.
01684
01685  3100-QID-ERROR.
01686
01687      MOVE ER-0033                TO  EMI-ERROR.
01688      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01689
01690  3100-EXIT.
01691      EXIT.
01692      EJECT
010412 4000-CLAIM-RESC-REFORM.

062712     PERFORM 4400-ADD-ORIG-REC THRU 4400-EXIT
100917     if WS-CERT-TRL-REC-NOT-FOUND = ZERO
100917        move cs-vin-number       to cg-vin
100917     end-if

           MOVE PI-CANCEL-TYPE         TO CG-OPTION-CODE
           MOVE PI-COMPANY-ID          TO CG-COMPANY-ID
           MOVE PI-PROCESSOR-ID        TO CG-PROC-ID
           MOVE PI-CR-MONTH-END-DT     TO CG-MONTH-END-DT
           MOVE PI-COMPANY-CD          TO CG-CERT-COMPANY-CD
           MOVE PI-CARRIER             TO CG-CERT-CARRIER
           MOVE PI-GROUPING            TO CG-CERT-GROUP
           MOVE PI-STATE               TO CG-CERT-STATE
           MOVE PI-ACCOUNT             TO CG-CERT-ACCOUNT
           MOVE PI-CERT-EFF-DT         TO CG-CERT-EFF-DT
           MOVE PI-CERT-NO             TO CG-CERT-CERT-NO
           MOVE SAVE-BIN-DATE          TO CG-CURRENT-DT
                                       
           MOVE PI-COMPANY-CD          TO  WS-CK-COMPANY-CD
           MOVE PI-CARRIER             TO  WS-CK-CARRIER
           MOVE PI-GROUPING            TO  WS-CK-GROUPING
           MOVE PI-STATE               TO  WS-CK-STATE
           MOVE PI-ACCOUNT             TO  WS-CK-ACCOUNT
           MOVE PI-CERT-NO             TO  WS-CK-CERT-NO
           MOVE PI-CERT-EFF-DT         TO  WS-CK-CERT-EFF-DT

           MOVE SPACES                 TO CG-BATCH-NO
                                          CG-LF-BENCD
                                          CG-AH-BENCD
           MOVE ZEROS                  TO CG-LF-CAN-AMT
                                          CG-AH-CAN-AMT
           MOVE LOW-VALUES             TO CG-LF-CAN-DT
                                          CG-AH-CAN-DT

062712    IF CM-LF-BENEFIT-CD NOT = '00' AND '  ' AND 'DD'
062712       AND (CM-LF-CANCEL-DT = LOW-VALUES OR SPACES)
062712        COMPUTE CG-LF-CAN-AMT = CM-LF-PREMIUM-AMT +
062712            CM-LF-ALT-PREMIUM-AMT
062712        MOVE CM-CERT-EFF-DT TO CG-LF-CAN-DT
062712    END-IF
062712    IF CM-AH-BENEFIT-CD NOT = '00' AND '  '
062712       AND (CM-AH-CANCEL-DT = LOW-VALUES OR SPACES)
062712         MOVE CM-AH-PREMIUM-AMT TO CG-AH-CAN-AMT
062712         MOVE CM-CERT-EFF-DT TO CG-AH-CAN-DT
062712    END-IF

010412     IF PI-CANCEL-TYPE = '3'
010412         IF CM-LF-BENEFIT-CD NOT = '00' AND '  ' AND 'DD'
010412           AND (CM-LF-CANCEL-DT = LOW-VALUES OR SPACES)
010412             MOVE CM-LF-BENEFIT-CD   TO CG-LF-BENCD
010412             MOVE CM-LF-PREMIUM-AMT  TO CG-LF-PREM-AMT
072312             MOVE CM-LF-ALT-PREMIUM-AMT TO CG-LF-ALT-PREM-AMT
010412         END-IF
010412         IF CM-AH-BENEFIT-CD NOT = '00' AND '  ' 
010412           AND (CM-AH-CANCEL-DT = LOW-VALUES OR SPACES)
010412             MOVE CM-AH-BENEFIT-CD   TO CG-AH-BENCD
010412             MOVE CM-AH-PREMIUM-AMT  TO CG-AH-PREM-AMT
010412         END-IF
010412         MOVE CM-INSURED-LAST-NAME TO CG-INS-LNAME
010412         MOVE CM-INSURED-FIRST-NAME TO CG-INS-FNAME
010412         MOVE CM-INSURED-INITIAL2 TO CG-INS-MID-INIT
010412         MOVE CM-INSURED-ISSUE-AGE TO CG-INS-AGE
010412         MOVE CM-JT-LAST-NAME    TO CG-JNT-LNAME
010412         MOVE CM-JT-FIRST-NAME   TO CG-JNT-FNAME
010412         MOVE CM-JT-INITIAL      TO CG-JNT-MID-INIT
010412         MOVE CM-INSURED-JOINT-AGE TO CG-JNT-AGE
101513         IF ((CM-LIFE-COMM-PCT = ZERO AND
101513            (CM-LF-BENEFIT-CD NOT = '00' AND SPACES)) OR
101513            (CM-LF-BENEFIT-CD = '00' OR SPACES))  
101513                    AND
101513            ((CM-AH-COMM-PCT = ZERO AND
101513            (CM-AH-BENEFIT-CD NOT = '00' AND SPACES)) OR
101513            (CM-AH-BENEFIT-CD = '00' OR SPACES))
122712              MOVE 'Y'           TO CG-COMM-PCT-ZERO
122712         END-IF
010412     END-IF           
010412
101918     move 'EL1273'               to cg-from-where
           EXEC CICS LINK
               PROGRAM  ('ELCANC')
               COMMAREA (CANCEL-GEN-PASS-AREA)
           END-EXEC
           IF CG-SUCCESS
              CONTINUE
           ELSE
010412        MOVE SPACES              TO EMI-MESSAGE-AREA (1)           
              move '3'                 to emi-switch1
              MOVE CG-ERROR-CODE       TO EMI-TEXT-VARIABLE (1)
010412        EVALUATE TRUE
010412           WHEN CG-DATE-ERROR
010412             MOVE WS-01-CANCEL-ERR TO EMI-ERROR-TEXT (1) (12:25)
010412           WHEN CG-CERT-NOT-FOUND
010412             MOVE WS-02-CANCEL-ERR TO EMI-ERROR-TEXT (1) (12:25)
010412           WHEN CG-AMOUNT-ERROR
010412             MOVE WS-04-CANCEL-ERR TO EMI-ERROR-TEXT (1) (12:25)
010412           WHEN CG-OPTION-ERROR
010412             MOVE WS-05-CANCEL-ERR TO EMI-ERROR-TEXT (1) (12:25)
010412           WHEN CG-PREV-CAN
010412             MOVE WS-06-CANCEL-ERR TO EMI-ERROR-TEXT (1) (12:25)
010412           WHEN CG-INVALID-DATA
010412             MOVE WS-07-CANCEL-ERR TO EMI-ERROR-TEXT (1) (12:25)
010412           WHEN CG-NO-ACCT-MSTR
010412             MOVE WS-08-CANCEL-ERR TO EMI-ERROR-TEXT (1) (12:25)
010412           WHEN CG-SFX-A-EXIST
010412             MOVE WS-09-CANCEL-ERR TO EMI-ERROR-TEXT (1) (12:25)
010412           WHEN CG-MISC-ERROR
010412             MOVE WS-99-CANCEL-ERR TO EMI-ERROR-TEXT (1) (12:25)
010412           WHEN OTHER
010412             move ' error - elcanc - return '
                                       to emi-error-text (1) (12:25)
010412        END-EVALUATE
              go to 8200-send-dataonly
           end-if


           .
       4000-exit.
           exit.
010412
010412 4400-ADD-ORIG-REC.
010412
062712     MOVE PI-COMPANY-CD          TO  WS-CK-COMPANY-CD.
062712     MOVE PI-CARRIER             TO  WS-CK-CARRIER.
062712     MOVE PI-GROUPING            TO  WS-CK-GROUPING.
062712     MOVE PI-STATE               TO  WS-CK-STATE.
062712     MOVE PI-ACCOUNT             TO  WS-CK-ACCOUNT.
062712     MOVE PI-CERT-NO             TO  WS-CK-CERT-NO.
062712     MOVE PI-CERT-EFF-DT         TO  WS-CK-CERT-EFF-DT.
062712
062712     EXEC CICS READ
062712         DATASET (WS-CERTIFICATE-MASTER-DSID)
062712         RIDFLD  (WS-CERTIFICATE-KEY)
062712         SET     (ADDRESS OF CERTIFICATE-MASTER)
062712         RESP    (WS-RESPONSE)
062712     END-EXEC
062712     IF RESP-NORMAL
062712         CONTINUE
062712     ELSE
062712        MOVE ER-0142                TO EMI-ERROR
062712        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
062712        GO TO 8100-SEND-INITIAL-MAP
062712     END-IF
062712
062712     IF CERT-ADDED-BATCH AND CERT-AS-LOADED
062712*         OR EIBAID = DFHPF15
062712          CONTINUE
062712     ELSE
062712          MOVE ER-0588   TO  EMI-ERROR
062712          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
062712          GO TO 8200-SEND-DATAONLY
062712     END-IF
062712
062712******************************************************************
062712*            A D D   O R I G   C E R T   I N F O                 *
062712******************************************************************
062712
062712     display ' made it to add orig cert ' ws-certificate-key
062712
062712     MOVE WS-CERTIFICATE-KEY     TO WS-ERMAIL-KEY
062712     MOVE ' '                    TO WS-ERMAIL-SW
062712
062712     EXEC CICS READ
062712        DATASET   ('ERMAIL')
062712        SET       (ADDRESS OF MAILING-DATA)
062712        RIDFLD    (WS-ERMAIL-KEY)
062712        RESP      (WS-RESPONSE)
062712     END-EXEC
062712
062712     IF RESP-NORMAL
062712        SET ERMAIL-FOUND TO TRUE
062712     END-IF
121712
121712     PERFORM 8800-READ-CERT-TRAILER THRU 8800-EXIT
062712
062712     MOVE WS-CERTIFICATE-KEY  TO ELCRTO-KEY (1:33)
062712     MOVE 'I'                 TO ELCRTO-RECORD-TYPE
062712     MOVE +0                  TO ELCRTO-SEQ-NO
062712
062712     EXEC CICS READ
062712        DATASET   ('ELCRTO')
062712        SET       (ADDRESS OF ORIGINAL-CERTIFICATE)
062712        RIDFLD    (ELCRTO-KEY)
062712        GTEQ
062712        RESP      (WS-RESPONSE)
062712     END-EXEC
062712     
062712     display ' just read gteq ' ws-response
062712     IF RESP-NORMAL
062712        AND (OC-CONTROL-PRIMARY (1:33) =
062712                 WS-CERTIFICATE-KEY)
062712        AND (OC-RECORD-TYPE = 'I')
062712        display ' resp norm, key =, type i '
062712        IF (OC-ENDORSEMENT-PROCESSED-DT = LOW-VALUES)
062712           EXEC CICS READ
062712              DATASET   ('ELCRTO')
062712              SET       (ADDRESS OF ORIGINAL-CERTIFICATE)
062712              RIDFLD    (OC-CONTROL-PRIMARY)
062712              UPDATE
062712              RESP      (WS-RESPONSE)
062712           END-EXEC
062712           DISPLAY ' JUST DID READ UPD ' WS-RESPONSE
062712           MOVE CM-INSURED-LAST-NAME   TO OC-INS-LAST-NAME   
062712           MOVE CM-INSURED-FIRST-NAME  TO OC-INS-FIRST-NAME  
062712           MOVE CM-INSURED-INITIAL2    TO OC-INS-MIDDLE-INIT 
062712           MOVE CM-INSURED-ISSUE-AGE   TO OC-INS-AGE         
062712           MOVE CM-JT-LAST-NAME        TO OC-JNT-LAST-NAME   
062712           MOVE CM-JT-FIRST-NAME       TO OC-JNT-FIRST-NAME  
062712           MOVE CM-JT-INITIAL          TO OC-JNT-MIDDLE-INIT 
062712           MOVE CM-INSURED-JOINT-AGE   TO OC-JNT-AGE         
072312           IF CM-LF-BENEFIT-CD NOT = '00' AND '  ' AND 'DD'
072312            AND (CM-LF-CANCEL-DT = LOW-VALUES OR SPACES)
072312              MOVE CM-LF-BENEFIT-CD    TO OC-LF-BENCD        
072312              MOVE CM-LF-ORIG-TERM     TO OC-LF-TERM         
072312              MOVE CM-LF-BENEFIT-AMT   TO OC-LF-BEN-AMT      
072312              MOVE CM-LF-PREMIUM-AMT   TO OC-LF-PRM-AMT      
072312              MOVE CM-LF-ALT-BENEFIT-AMT TO OC-LF-ALT-BEN-AMT  
072312              MOVE CM-LF-ALT-PREMIUM-AMT TO OC-LF-ALT-PRM-AMT  
072312              MOVE CM-LF-LOAN-EXPIRE-DT TO OC-LF-EXP-DT       
072312              MOVE CM-LIFE-COMM-PCT    TO OC-LF-COMM-PCT     
072312              COMPUTE OC-LF-CANCEL-AMT = CM-LF-PREMIUM-AMT +
072312                    CM-LF-ALT-PREMIUM-AMT
072312              MOVE CM-CERT-EFF-DT      TO OC-LF-CANCEL-DT
072312           ELSE
072312              MOVE SPACES              TO OC-LF-BENCD        
072312              MOVE ZEROS               TO OC-LF-TERM         
072312                                          OC-LF-BEN-AMT      
072312                                          OC-LF-PRM-AMT      
072312                                          OC-LF-ALT-BEN-AMT  
072312                                          OC-LF-ALT-PRM-AMT  
072312                                          OC-LF-COMM-PCT     
072312                                          OC-LF-CANCEL-AMT
072312              MOVE LOW-VALUES          TO OC-LF-EXP-DT       
072312              MOVE CM-LF-CANCEL-DT     TO OC-LF-CANCEL-DT
072312           END-IF
072312           MOVE CM-LF-ITD-CANCEL-AMT   TO OC-LF-ITD-CANCEL-AMT   
072312           IF CM-AH-BENEFIT-CD NOT = '00' AND '  '
072312            AND (CM-AH-CANCEL-DT = LOW-VALUES OR SPACES)
072312              MOVE CM-AH-BENEFIT-CD    TO OC-AH-BENCD        
072312              MOVE CM-AH-ORIG-TERM     TO OC-AH-TERM         
072312              MOVE CM-AH-BENEFIT-AMT   TO OC-AH-BEN-AMT      
072312              MOVE CM-AH-PREMIUM-AMT   TO OC-AH-PRM-AMT      
072312              MOVE CM-AH-LOAN-EXPIRE-DT TO OC-AH-EXP-DT       
072312              MOVE CM-AH-COMM-PCT      TO OC-AH-COMM-PCT     
072312              MOVE CM-AH-CRITICAL-PERIOD TO OC-AH-CP           
072312              MOVE CM-AH-PREMIUM-AMT   TO OC-AH-CANCEL-AMT
072312              MOVE CM-CERT-EFF-DT      TO OC-AH-CANCEL-DT
072312           ELSE
072312              MOVE SPACES              TO OC-AH-BENCD        
072312              MOVE ZEROS               TO OC-AH-TERM         
072312                                          OC-AH-BEN-AMT      
072312                                          OC-AH-PRM-AMT
072312                                          OC-AH-COMM-PCT     
072312                                          OC-AH-CANCEL-AMT
072312                                          OC-AH-CP
072312              MOVE LOW-VALUES          TO OC-AH-EXP-DT       
072312              MOVE CM-AH-CANCEL-DT     TO OC-AH-CANCEL-DT
072312           END-IF
072312           MOVE CM-AH-ITD-CANCEL-AMT   TO OC-AH-ITD-CANCEL-AMT
062712           MOVE CM-LOAN-1ST-PMT-DT     TO OC-1ST-PMT-DT
011413           MOVE 'Y'                    TO OC-ISSUE-TRAN-IND
011413           MOVE 'Y'                    TO OC-CANCEL-TRAN-IND
062712           MOVE PI-PROCESSOR-ID        TO OC-LAST-MAINT-BY
062712           MOVE EIBTIME                TO OC-LAST-MAINT-HHMMSS
062712           MOVE EIBDATE                TO DC-JULIAN-YYDDD
062712           MOVE '5'                    TO DC-OPTION-CODE
062712           PERFORM 8500-DATE-CONVERSION
062712           MOVE DC-BIN-DATE-1          TO OC-LAST-MAINT-DT
062712           IF ERMAIL-FOUND
062712               MOVE MA-CRED-BENE-NAME
062712                           TO OC-CRED-BENE-NAME
062712           END-IF
121712           IF WS-CERT-TRL-REC-NOT-FOUND = ZERO
121712               MOVE CS-INS-AGE-DEFAULT-FLAG TO 
121712                                  OC-INS-AGE-DEFAULT-FLAG
121712               MOVE CS-JNT-AGE-DEFAULT-FLAG TO 
121712                                  OC-JNT-AGE-DEFAULT-FLAG
121712           END-IF
062712           EXEC CICS REWRITE
062712              DATASET   ('ELCRTO')
062712              FROM      (ORIGINAL-CERTIFICATE)
062712              RESP      (WS-RESPONSE)
062712           END-EXEC
062712           display ' just rewrote ' ws-response
062712           IF NOT RESP-NORMAL
062712              MOVE ER-3830          TO EMI-ERROR
062712              PERFORM 9900-ERROR-FORMAT
062712                                 THRU 9900-EXIT
062712              GO TO 8200-SEND-DATAONLY
062712           END-IF
062712           
062712           GO TO 4400-EXIT
062712
062712        ELSE
062712           SUBTRACT +1 FROM OC-KEY-SEQ-NO
062712        END-IF
062712     ELSE
062712        MOVE SPACES              TO ORIGINAL-CERTIFICATE
062712        MOVE 'OC'                TO OC-RECORD-ID
062712        MOVE WS-CERTIFICATE-KEY  TO OC-CONTROL-PRIMARY (1:33)
062712        MOVE 'I'                 TO OC-RECORD-TYPE
062712        MOVE +4096               TO OC-KEY-SEQ-NO
062712     END-IF
062712
062712     MOVE CM-INSURED-LAST-NAME   TO OC-INS-LAST-NAME   
062712     MOVE CM-INSURED-FIRST-NAME  TO OC-INS-FIRST-NAME  
062712     MOVE CM-INSURED-INITIAL2    TO OC-INS-MIDDLE-INIT 
062712     MOVE CM-INSURED-ISSUE-AGE   TO OC-INS-AGE         
062712     MOVE CM-JT-LAST-NAME        TO OC-JNT-LAST-NAME   
062712     MOVE CM-JT-FIRST-NAME       TO OC-JNT-FIRST-NAME  
062712     MOVE CM-JT-INITIAL          TO OC-JNT-MIDDLE-INIT 
062712     MOVE CM-INSURED-JOINT-AGE   TO OC-JNT-AGE         
072312     IF CM-LF-BENEFIT-CD NOT = '00' AND '  ' AND 'DD'
072312       AND (CM-LF-CANCEL-DT = LOW-VALUES OR SPACES)
072312        MOVE CM-LF-BENEFIT-CD    TO OC-LF-BENCD        
072312        MOVE CM-LF-ORIG-TERM     TO OC-LF-TERM         
072312        MOVE CM-LF-BENEFIT-AMT   TO OC-LF-BEN-AMT      
072312        MOVE CM-LF-PREMIUM-AMT   TO OC-LF-PRM-AMT      
072312        MOVE CM-LF-ALT-BENEFIT-AMT TO OC-LF-ALT-BEN-AMT  
072312        MOVE CM-LF-ALT-PREMIUM-AMT TO OC-LF-ALT-PRM-AMT  
072312        MOVE CM-LF-LOAN-EXPIRE-DT TO OC-LF-EXP-DT       
072312        MOVE CM-LIFE-COMM-PCT    TO OC-LF-COMM-PCT     
062712        COMPUTE OC-LF-CANCEL-AMT = CM-LF-PREMIUM-AMT +
062712              CM-LF-ALT-PREMIUM-AMT
062712        MOVE CM-CERT-EFF-DT      TO OC-LF-CANCEL-DT
072312     ELSE
072312        MOVE SPACES              TO OC-LF-BENCD        
072312        MOVE ZEROS               TO OC-LF-TERM         
072312                                    OC-LF-BEN-AMT      
072312                                    OC-LF-PRM-AMT      
072312                                    OC-LF-ALT-BEN-AMT  
072312                                    OC-LF-ALT-PRM-AMT  
072312                                    OC-LF-COMM-PCT     
072312                                    OC-LF-CANCEL-AMT
072312        MOVE LOW-VALUES          TO OC-LF-EXP-DT       
072312        MOVE CM-LF-CANCEL-DT     TO OC-LF-CANCEL-DT
062712     END-IF
062712     MOVE CM-LF-ITD-CANCEL-AMT   TO OC-LF-ITD-CANCEL-AMT   
072312     IF CM-AH-BENEFIT-CD NOT = '00' AND '  '
072312      AND (CM-AH-CANCEL-DT = LOW-VALUES OR SPACES)
062712        MOVE CM-AH-BENEFIT-CD    TO OC-AH-BENCD        
062712        MOVE CM-AH-ORIG-TERM     TO OC-AH-TERM         
062712        MOVE CM-AH-BENEFIT-AMT   TO OC-AH-BEN-AMT      
062712        MOVE CM-AH-PREMIUM-AMT   TO OC-AH-PRM-AMT      
062712        MOVE CM-AH-LOAN-EXPIRE-DT TO OC-AH-EXP-DT       
062712        MOVE CM-AH-COMM-PCT      TO OC-AH-COMM-PCT     
062712        MOVE CM-AH-CRITICAL-PERIOD TO OC-AH-CP           
072312        MOVE CM-AH-PREMIUM-AMT   TO OC-AH-CANCEL-AMT
072312        MOVE CM-CERT-EFF-DT      TO OC-AH-CANCEL-DT
072312     ELSE
072312        MOVE SPACES              TO OC-AH-BENCD        
072312        MOVE ZEROS               TO OC-AH-TERM         
072312                                    OC-AH-BEN-AMT      
072312                                    OC-AH-PRM-AMT
072312                                    OC-AH-COMM-PCT     
072312                                    OC-AH-CANCEL-AMT
072312                                    OC-AH-CP
072312        MOVE LOW-VALUES          TO OC-AH-EXP-DT       
072312        MOVE CM-AH-CANCEL-DT     TO OC-AH-CANCEL-DT
072312     END-IF
072312     MOVE CM-AH-ITD-CANCEL-AMT   TO OC-AH-ITD-CANCEL-AMT
062712     MOVE CM-LOAN-1ST-PMT-DT     TO OC-1ST-PMT-DT
011413     MOVE 'Y'                    TO OC-ISSUE-TRAN-IND
011413     MOVE 'Y'                    TO OC-CANCEL-TRAN-IND
062712     MOVE PI-PROCESSOR-ID        TO OC-LAST-MAINT-BY
062712     MOVE EIBTIME                TO OC-LAST-MAINT-HHMMSS
062712     MOVE EIBDATE                TO DC-JULIAN-YYDDD
062712     MOVE '5'                    TO DC-OPTION-CODE
062712     PERFORM 8500-DATE-CONVERSION
062712     MOVE DC-BIN-DATE-1          TO OC-LAST-MAINT-DT
062712     IF ERMAIL-FOUND
062712         MOVE MA-CRED-BENE-NAME
062712                     TO OC-CRED-BENE-NAME
062712     END-IF
121712     IF WS-CERT-TRL-REC-NOT-FOUND = ZERO
121712         MOVE CS-INS-AGE-DEFAULT-FLAG TO 
121712                            OC-INS-AGE-DEFAULT-FLAG
121712         MOVE CS-JNT-AGE-DEFAULT-FLAG TO 
121712                            OC-JNT-AGE-DEFAULT-FLAG
121712     END-IF
062712     MOVE LOW-VALUES       TO OC-ENDORSEMENT-PROCESSED-DT

           Display 'about to do 4400-write ' original-certificate
           
062712     .
062712 4400-WRITE-ELCRTO.
062712
062712     EXEC CICS WRITE
062712        DATASET   ('ELCRTO')
062712        FROM      (ORIGINAL-CERTIFICATE)
062712        RIDFLD    (OC-CONTROL-PRIMARY)
062712        RESP      (WS-RESPONSE)
062712     END-EXEC
062712
062712    IF RESP-DUPKEY OR RESP-DUPREC
062712        SUBTRACT +1    FROM OC-KEY-SEQ-NO
062712        GO TO 4400-WRITE-ELCRTO
062712    ELSE
062712        IF NOT RESP-NORMAL
062712           MOVE ER-3830          TO EMI-ERROR
062712           PERFORM 9900-ERROR-FORMAT
062712                                 THRU 9900-EXIT
062712           GO TO 8200-SEND-DATAONLY
062712        END-IF
062712    END-IF
062712
062712     .
062712 4400-EXIT.
062712    EXIT.
062712
062712
062712

01693  8100-SEND-INITIAL-MAP.

01694      MOVE SAVE-DATE              TO CDATEO
01695      MOVE EIBTIME                TO TIME-IN
01696      MOVE TIME-OUT               TO CTIMEO
101201     MOVE PI-COMPANY-ID          TO CMPNYIDO
101201     MOVE PI-PROCESSOR-ID        TO USERIDO
           move function upper-case(ws-kix-myenv)
                                       to  sysenvo
01697
01698      IF EMI-ERROR  NOT = ZERO
01699          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01700      ELSE
01701          IF TRANSACTION-SUCCESSFUL
01702          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01703
01704      MOVE EMI-MESSAGE-AREA (1)    TO  CEMSG1O.
01705      MOVE PI-MEMBER-CAPTION       TO  CMEMCAPO.
01706
01707      IF PI-COMPANY-ID = 'DMD'
01708          MOVE AL-SANOF          TO CLNAMEA
01710                                    CFNAMEA
01711                                    CINITA
01712                                    CAGEA
01713                                    CSEXA
01714                                    CSSNA
01715                                    CJLNAMEA
01716                                    CJFNAMEA
01717                                    CJINITA
01718                                    CJAGEA
01719                                    CBNAMEA
01720                                    LOANNOA
01721                                    LOANBALA
01722                                    LNOFCA
01723                                    CAPRA
01724 *                                  CFORMNOA
01725                                    CUSERCDA
01726                                    CINDGRPA
01727                                    CPREMTPA
01728                                    CLIVESA
071015*                                  CCLMDEDA
071015*                                  CCANDEDA
071015*                                  CBILLEDA
01732                                    CMEMNOA
01733          MOVE -1                TO CEMSG2L
01734      ELSE
01735          MOVE -1                TO CMEMNOL
           END-IF

01737      EXEC CICS SEND
01738          FROM   (EL127CI)
01739          MAPSET (WS-MAPSET-NAME)
01740          MAP    (WS-MAP-NAME)
01741          CURSOR
01742          ERASE
01743      END-EXEC.
01744
01745      GO TO 9100-RETURN-TRAN.
01746
01747  8100-EXIT.
01748      EXIT.
01749
01750      EJECT
01751  8200-SEND-DATAONLY.
01752      MOVE SAVE-DATE              TO  CDATEO.
01753      MOVE EIBTIME                TO  TIME-IN.
01754      MOVE TIME-OUT               TO  CTIMEO.
           move function upper-case(ws-kix-myenv)
                                       to  sysenvo
101201     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101201     MOVE PI-PROCESSOR-ID        TO  USERIDO.
01755
01756      IF EMI-ERROR  NOT = ZERO
01757          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01758
01759      MOVE EMI-MESSAGE-AREA (1)   TO  CEMSG1O.
01760      MOVE PI-MEMBER-CAPTION      TO  CMEMCAPO.
01761
01762      IF PI-COMPANY-ID = 'DMD'
01763          MOVE AL-SANOF          TO CLNAMEA
01765                                    CFNAMEA
01766                                    CINITA
01767                                    CAGEA
01768                                    CSEXA
01769                                    CSSNA
01770                                    CJLNAMEA
01771                                    CJFNAMEA
01772                                    CJINITA
01773                                    CJAGEA
01774                                    CBNAMEA
01775                                    LOANNOA
01776                                    LOANBALA
01777                                    LNOFCA
01778                                    CAPRA
01779 *                                  CFORMNOA
01780                                    CUSERCDA
01781                                    CINDGRPA
01782                                    CPREMTPA
01783                                    CLIVESA
071015*                                  CCLMDEDA
071015*                                  CCANDEDA
071015*                                  CBILLEDA
01787                                    CMEMNOA
01788          MOVE -1                TO CEMSG2L.
01789
01790      EXEC CICS SEND DATAONLY
01791          FROM   (EL127CI)
01792          MAPSET (WS-MAPSET-NAME)
01793          MAP    (WS-MAP-NAME)
01794          CURSOR
01795      END-EXEC.
01796
01797      GO TO 9100-RETURN-TRAN.
01798
01799  8200-EXIT.
01800      EXIT.
01801
01802      EJECT
01803  8300-SEND-TEXT.
01804      EXEC CICS SEND TEXT
01805          FROM   (LOGOFF-TEXT)
01806          LENGTH (LOGOFF-LENGTH)
01807          ERASE
01808          FREEKB
01809      END-EXEC.
01810
01811      EXEC CICS RETURN
01812      END-EXEC.
01813
01814  8300-EXIT.
01815      EXIT.
01816      EJECT
01817
01818  8400-READ-STATE-CNTL.
01819
01820      MOVE ZERO                   TO  WS-ST-REC-NOT-FOUND.
01821      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
01822      MOVE '3'                    TO  WS-CFK-RECORD-TYPE.
01823      MOVE PI-STATE               TO  WS-CFK-ACCESS.
01824      MOVE +0                     TO  WS-CFK-SEQUENCE-NO.
01825
01826      EXEC CICS HANDLE CONDITION
01827          NOTFND (8410-STATE-REC-NOTFND)
01828      END-EXEC.
01829
01830      EXEC CICS READ
01831          DATASET  (WS-CONTROL-FILE-DSID)
01832          RIDFLD   (WS-CONTROL-FILE-KEY)
01833          SET      (ADDRESS OF CONTROL-FILE)
01834      END-EXEC.
01835
01836      GO TO 8400-EXIT.
01837
01838  8410-STATE-REC-NOTFND.
01839      MOVE +1                     TO WS-ST-REC-NOT-FOUND.
01840
01841  8400-EXIT.
01842      EXIT.
01843      EJECT
01844
01845  8500-DATE-CONVERSION.
01846      EXEC CICS LINK
01847          PROGRAM  (ELDATCV)
01848          COMMAREA (DATE-CONVERSION-DATA)
01849          LENGTH   (DC-COMM-LENGTH)
01850      END-EXEC.
01851
01852  8500-EXIT.
01853      EXIT.
01854
01855  8600-DEEDIT.
01856      EXEC CICS BIF
01857           DEEDIT
01858           FIELD  (DEEDIT-FIELD)
01859           LENGTH (15)
01860      END-EXEC.
01861
01862  8600-EXIT.
01863      EXIT.
01864
01865  8700-LOCATE-BENEFIT.
01866      EXEC CICS HANDLE CONDITION
01867          NOTFND (8700-EXIT)
01868      END-EXEC.
01869
01870      MOVE SPACES                 TO  WS-KIND.
01871      MOVE ZERO                   TO  WS-NOT-FOUND.
01872
01873      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
01874      MOVE SPACES                 TO  WS-CFK-ACCESS.
01875      MOVE WS-BENEFIT-NO          TO  WS-CFK-BENEFIT-NO.
01876
01877      EXEC CICS READ
01878          DATASET (WS-CONTROL-FILE-DSID)
01879          RIDFLD  (WS-CONTROL-FILE-KEY)
01880          SET     (ADDRESS OF CONTROL-FILE)
01881          GTEQ
01882      END-EXEC.
01883
01884      IF WS-CFK-COMPANY-ID  NOT = CF-COMPANY-ID  OR
01885         WS-CFK-RECORD-TYPE NOT = CF-RECORD-TYPE
01886           GO TO 8700-EXIT.
01887
01888      MOVE +1                     TO  WS-INDEX.
01889
01890  8700-LOOKUP-BENEFIT.
01891      IF WS-BENEFIT-NO = CF-BENEFIT-CODE (WS-INDEX)
01892          MOVE CF-BENEFIT-ALPHA (WS-INDEX)   TO WS-KIND
01893          MOVE CF-SPECIAL-CALC-CD (WS-INDEX) TO WS-CALC-CD
01894          MOVE CF-BENEFIT-DESCRIP (WS-INDEX) TO WS-BENEFIT-DESCRIP
01895          MOVE CF-JOINT-INDICATOR (WS-INDEX) TO WS-JOINT-INDICATOR
01896          MOVE +1                            TO WS-NOT-FOUND
01897          GO TO 8700-EXIT.
01898
01899      IF CF-BENEFIT-CODE (WS-INDEX) NOT < CF-HI-BEN-IN-REC
01900          GO TO 8700-EXIT.
01901
01902      IF WS-INDEX < +8
01903          ADD +1  TO  WS-INDEX
01904          GO TO 8700-LOOKUP-BENEFIT.
01905
01906  8700-EXIT.
01907      EXIT.
040909
040909 8800-READ-CERT-TRAILER.
040909
040909     MOVE ZERO                   TO  WS-CERT-TRL-REC-NOT-FOUND.
040909     MOVE WS-CERTIFICATE-KEY     TO  WS-ELCRTT-PRIMARY.
040909     MOVE 'C'                    TO  WS-ELCRTT-REC-TYPE.
040909
040909     EXEC CICS HANDLE CONDITION
040909         NOTFND (8800-CERT-TRL-REC-NOTFND)
040909     END-EXEC.
040909
040909     EXEC CICS READ
040909         DATASET  (WS-CERT-TRAILERS-DSID)
040909         RIDFLD   (WS-ELCRTT-KEY)
040909         SET      (ADDRESS OF CERTIFICATE-TRAILERS)
040909     END-EXEC.
040909
040909     GO TO 8800-EXIT.
040909
040909 8800-CERT-TRL-REC-NOTFND.
040909     MOVE +1                     TO WS-CERT-TRL-REC-NOT-FOUND.
040909
040909 8800-EXIT.
040909     EXIT.
040909     EJECT
01908
01909  8880-NOT-FOUND.
01910      MOVE ER-0142                TO EMI-ERROR.
01911      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01912      GO TO 8100-SEND-INITIAL-MAP.
01913
01914  8880-EXIT.
01915      EXIT.
01916
01917  9000-RETURN-CICS.
01918      MOVE EL005                  TO  THIS-PGM.
01919      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
01920      GO TO 9300-XCTL.
01921
01922  9000-EXIT.
01923      EXIT.
01924
01925  9100-RETURN-TRAN.
01926      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
01927      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.
01928
01929      EXEC CICS RETURN
01930          COMMAREA (PROGRAM-INTERFACE-BLOCK)
01931          LENGTH   (PI-COMM-LENGTH)
01932          TRANSID  (WS-TRANS-ID)
01933      END-EXEC.
01934
01935  9100-EXIT.
01936      EXIT.
01937
01938  9300-XCTL.
01939      MOVE DFHENTER               TO  EIBAID.
01940
01941      EXEC CICS XCTL
01942          PROGRAM  (THIS-PGM)
01943          COMMAREA (PROGRAM-INTERFACE-BLOCK)
01944          LENGTH   (PI-COMM-LENGTH)
01945      END-EXEC.
01946
01947  9300-EXIT.
01948      EXIT.
01949
01950      EJECT
01951  9400-CLEAR.
01952      MOVE PI-RETURN-TO-PROGRAM   TO  THIS-PGM.
01953      GO TO 9300-XCTL.
01954
01955      EJECT
01956  9900-ERROR-FORMAT.
01957      ADD +1                      TO  WS-ERROR-COUNT.
01958
01959      IF EMI-ERRORS-COMPLETE
01960          MOVE ZERO               TO  EMI-ERROR
01961          GO TO 9900-EXIT.
01962
01963      EXEC CICS LINK
01964          PROGRAM  (EL001)
01965          COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)
01966          LENGTH   (EMI-COMM-LENGTH)
01967      END-EXEC.
01968
01969      MOVE ZERO                   TO  EMI-ERROR.
01970
01971  9900-EXIT.
01972      EXIT.
01973
01974      EJECT
01975
01976  9800-LINK-REM-TERM.
01977      EXEC CICS LINK
01978          PROGRAM  (ELRTRM)
01979          COMMAREA (CALCULATION-PASS-AREA)
01980          LENGTH   (CP-COMM-LENGTH)
01981      END-EXEC.
01982
01983  9800-EXIT.
01984      EXIT.
01985
01986  9990-ERROR.
01987      MOVE DFHEIBLK               TO  EMI-LINE1.
01988
01989      EXEC CICS LINK
01990          PROGRAM  (EL004)
01991          COMMAREA (EMI-LINE1)
01992          LENGTH   (72)
01993      END-EXEC.
01994
01995      IF CLAIM-SESSION
01996          GO TO 8100-SEND-INITIAL-MAP
01997      ELSE
01998          GO TO 8200-SEND-DATAONLY.
01999
02000  9990-EXIT.
02001      EXIT.
02002
02003  9995-SECURITY-VIOLATION.
02004                              COPY ELCSCTP.

