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
092412* 092412  IR2012091100004  AJRA  CHECK ADDRESS BEFORE LETTER WRITE
121312* 121312  CR2012101700002  AJRA  RESC/REF ARE YOU SURE
121712* 121712  CR2012101700002  AJRA  ADD DEFAULT AGE FLAG
122712* 122712  CR2012101700002  AJRA  PASS FLAG FOR ZERO COMM PCT TO CA
011413* 011413  IR2012122700003  AJRA  ADD CRTO ISSUE/CANCEL INDICATOR
101513* 101513  CR2013090300001  AJRA  NAPERSOFT PHASE 2
120313* 120313  CR2013090300001  AJRA  NAPERSOFT PHASE 2
071015* 071015  CR2014011600001  PEMA  ADD ALL RPT CODES TO SCREEN
031416* 031416  CR2016030800002  PEMA  ALLOW USER TO REMOVE VIN
100917* 100917  CR2017092000002  PEMA  Pass vin to elcanc
062017* 062017  CR2015091000001  PEMA  ADD PROCESSING FOR TN REF INTERES
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
       01  DFH-START PIC X(04).
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
00068 *                                    COPY ELCSCTM.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCSCTM                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY MESSAGE AREA     *
00007 *                                                                *
00008 ******************************************************************
00009  01  SECURITY-MESSAGE.
00010      12  FILLER                          PIC X(30)
00011             VALUE '** LOGIC SECURITY VIOLATION -'.
00012      12  SM-READ                         PIC X(6).
00013      12  FILLER                          PIC X(5)
00014             VALUE ' PGM='.
00015      12  SM-PGM                          PIC X(6).
00016      12  FILLER                          PIC X(5)
00017             VALUE ' OPR='.
00018      12  SM-PROCESSOR-ID                 PIC X(4).
00019      12  FILLER                          PIC X(6)
00020             VALUE ' TERM='.
00021      12  SM-TERMID                       PIC X(4).
00022      12  FILLER                          PIC XX   VALUE SPACE.
00023      12  SM-JUL-DATE                     PIC 9(5).
00024      12  FILLER                          PIC X    VALUE SPACE.
00025      12  SM-TIME                         PIC 99.99.
00026
00069
00070 *                                    COPY ELCSCRTY.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCSCRTY                            *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY DATA AREA        *
00007 *        AREA ACQUIRED BY SIGN ON PROGRAM EL125 AND ADDRESS      *
00008 *        SAVED IN PI-SECURITY-ADDRESS.                           *
00009 *                                                                *
00010 ******************************************************************
00011  01  SECURITY-CONTROL.
00012      12  SC-COMM-LENGTH               PIC S9(4) VALUE +144 COMP.
00013      12  FILLER                       PIC XX    VALUE 'SC'.
00014      12  SC-CREDIT-CODES.
00015          16  SC-CREDIT-AUTHORIZATION OCCURS 40 TIMES.
00016              20  SC-CREDIT-DISPLAY    PIC X.
00017              20  SC-CREDIT-UPDATE     PIC X.
00018      12  SC-CLAIMS-CODES.
00019          16  SC-CLAIMS-AUTHORIZATION OCCURS 30 TIMES.
00020              20  SC-CLAIMS-DISPLAY    PIC X.
00021              20  SC-CLAIMS-UPDATE     PIC X.
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
      *                                COPY ERCACCT.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCACCT                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.031                          *
00006 *                                                                *
00007 *   CREDIT SYSTEM ACCOUNT MASTER FILE                            *
00008 *                                                                *
00009 *   THIS COPYBOOK IS USED FOR BOTH THE ONLINE AND BATCH          *
00010 *   VSAM ACCOUNT MASTER FILES.                                   *
00011 *                                                                *
00012 *   FILE DESCRIPTION = ACCOUNT OR PRODUCER FILES                 *
00013 *                                                                *
00014 *   FILE TYPE = VSAM,KSDS                                        *
00015 *   RECORD SIZE = 2000  RECFORM = FIX                            *
00016 *                                                                *
00017 *   BASE CLUSTER NAME = ERACCT                    RKP=2,LEN=26   *
00018 *       ALTERNATE PATH1 = ERACCT2 (ALT GROUPING) RKP=28,LEN=26   *
00019 *                                                                *
00020 *   LOG = NO                                                     *
00021 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00022 *                                                                *
00023 *                                                                *
00024 ******************************************************************
102004*                   C H A N G E   L O G
102004*
102004* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
102004*-----------------------------------------------------------------
102004*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
102004* EFFECTIVE    NUMBER
102004*-----------------------------------------------------------------
102004* 102004    2003031400002  PEMA  ADD NEW STATUS CODE
092705* 092705    2005050300006  PEMA  ADD SPP LEASES
022808* 022808    2007083100002  PEMA  ADD FREEZE STATUS
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
030211* 030211  CR2010012100001  PEMA  ADD EMAILS FROM RDS
031811* 031811  CR2011012700001  PEMA  ADD ACCT STATUS S - SUSPENDED
101711* 101711  CR2011092000001  PEMA  ADD UNEARNED FACTOR STATE FOR DCC
021916* 021916  CR2014010900001  TANA  ADD NEW STATUS CODE VALUES
102004******************************************************************
00025
00026  01  ACCOUNT-MASTER.
00027      12  AM-RECORD-ID                      PIC XX.
00028          88  VALID-AM-ID                      VALUE 'AM'.
00029
00030      12  AM-CONTROL-PRIMARY.
00031          16  AM-COMPANY-CD                 PIC X.
00032          16  AM-MSTR-CNTRL.
00033              20  AM-CONTROL-A.
00034                  24  AM-CARRIER            PIC X.
00035                  24  AM-GROUPING.
00036                      28 AM-GROUPING-PREFIX PIC XXX.
00037                      28 AM-GROUPING-PRIME  PIC XXX.
00038                  24  AM-STATE              PIC XX.
00039                  24  AM-ACCOUNT.
00040                      28  AM-ACCOUNT-PREFIX PIC X(4).
00041                      28  AM-ACCOUNT-PRIME  PIC X(6).
00042              20  AM-CNTRL-1   REDEFINES   AM-CONTROL-A
00043                                            PIC X(19).
00044              20  AM-CNTRL-B.
00045                  24  AM-EXPIRATION-DT      PIC XX.
00046                  24  FILLER                PIC X(4).
00047              20  AM-CNTRL-2 REDEFINES AM-CNTRL-B.
00048                  24  AM-EXPIRE-DT          PIC 9(11)  COMP-3.
00049
00050      12  AM-CONTROL-BY-VAR-GRP.
00051          16  AM-COMPANY-CD-A1              PIC X.
00052          16  AM-VG-CARRIER                 PIC X.
00053          16  AM-VG-GROUPING                PIC X(6).
00054          16  AM-VG-STATE                   PIC XX.
00055          16  AM-VG-ACCOUNT                 PIC X(10).
00056          16  AM-VG-DATE.
00057              20  AM-VG-EXPIRATION-DT       PIC XX.
00058              20  FILLER                    PIC X(4).
00059          16  AM-VG-EXP-DATE REDEFINES AM-VG-DATE
00060                                            PIC 9(11)      COMP-3.
030211     12  FILLER REDEFINES AM-CONTROL-BY-VAR-GRP.
030211         16  FILLER                        PIC X(10).
030211         16  AM-VG-KEY3.
030211             20  AM-VG3-ACCOUNT            PIC X(10).
030211             20  AM-VG3-EXP-DT             PIC XX.
030211         16  FILLER                        PIC X(4).
00061      12  AM-MAINT-INFORMATION.
00062          16  AM-LAST-MAINT-DT              PIC XX.
00063          16  AM-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
00064          16  AM-LAST-MAINT-USER            PIC X(4).
00065          16  FILLER                        PIC XX.
00066
00067      12  AM-EFFECTIVE-DT                   PIC XX.
00068      12  AM-EFFECT-DT                      PIC 9(11)      COMP-3.
00069
00070      12  AM-PREV-DATES  COMP-3.
00071          16  AM-PREV-EXP-DT                PIC 9(11).
00072          16  AM-PREV-EFF-DT                PIC 9(11).
00073
00074      12  AM-REPORT-CODE-1                  PIC X(10).
00075      12  AM-REPORT-CODE-2                  PIC X(10).
00076
00077      12  AM-CITY-CODE                      PIC X(4).
00078      12  AM-COUNTY-PARISH                  PIC X(6).
00079
00080      12  AM-NAME                           PIC X(30).
00081      12  AM-PERSON                         PIC X(30).
00082      12  AM-ADDRS                          PIC X(30).
00083      12  AM-CITY.
               16  AM-ADDR-CITY                  PIC X(28).
               16  AM-ADDR-STATE                 PIC XX.
00084      12  AM-ZIP.
00085          16  AM-ZIP-PRIME.
00086              20  AM-ZIP-PRI-1ST            PIC X.
00087                  88  AM-CANADIAN-POST-CODE    VALUE 'A' THRU 'Z'.
00088              20  FILLER                    PIC X(4).
00089          16  AM-ZIP-PLUS4                  PIC X(4).
00090      12  AM-CANADIAN-POSTAL-CODE  REDEFINES  AM-ZIP.
00091          16  AM-CAN-POSTAL-1               PIC XXX.
00092          16  AM-CAN-POSTAL-2               PIC XXX.
00093          16  FILLER                        PIC XXX.
00094      12  AM-TEL-NO.
00095          16  AM-AREA-CODE                  PIC 999.
00096          16  AM-TEL-PRE                    PIC 999.
00097          16  AM-TEL-NBR                    PIC 9(4).
00098      12  AM-TEL-LOC                        PIC X.
00099          88  AM-TEL-AT-HOME                   VALUE 'H'.
00100          88  AM-TEL-AT-BUSINESS               VALUE 'B'.
00101
00102      12  AM-COMM-STRUCTURE.
00103          16  AM-DEFN-1.
00104              20  AM-AGT-COMMS       OCCURS 10 TIMES.
00105                  24  AM-AGT.
00106                      28  AM-AGT-PREFIX     PIC X(4).
00107                      28  AM-AGT-PRIME      PIC X(6).
00108                  24  AM-COM-TYP            PIC X.
00109                  24  AM-L-COM              PIC SV9(5)     COMP-3.
00110                  24  AM-J-COM              PIC SV9(5)     COMP-3.
00111                  24  AM-A-COM              PIC SV9(5)     COMP-3.
00112                  24  AM-RECALC-LV-INDIC    PIC X.
00113                  24  AM-RETRO-LV-INDIC     PIC X.
00114                  24  AM-GL-CODES           PIC X.
00115                  24  AM-COMM-CHARGEBACK    PIC 9(02).
00116                  24  FILLER                PIC X(01).
00117          16  AM-DEFN-2   REDEFINES   AM-DEFN-1.
00118              20  AM-COM-TBLS        OCCURS 10 TIMES.
00119                  24  FILLER                PIC X(11).
00120                  24  AM-L-COMA             PIC XXX.
00121                  24  AM-J-COMA             PIC XXX.
00122                  24  AM-A-COMA             PIC XXX.
00123                  24  FILLER                PIC X(6).
00124
00125      12  AM-COMM-CHANGE-STATUS             PIC X.
00126          88  AM-COMMISSIONS-CHANGED           VALUE '*'.
00127
00128      12  AM-CSR-CODE                       PIC X(4).
00129
00130      12  AM-BILLING-STATUS                 PIC X.
00131          88  AM-ACCOUNT-BILLED                VALUE 'B'.
00132          88  AM-ACCOUNT-NOT-BILLED            VALUE ' '.
00133      12  AM-AUTO-REFUND-SW                 PIC X.
00134          88  AUTO-REFUNDS-USED                VALUE 'Y'.
00135          88  AUTO-REFUNDS-NOT-USED            VALUE 'N' ' '.
00136      12  AM-GPCD                           PIC 99.
00137      12  AM-IG                             PIC X.
00138          88  AM-HAS-INDIVIDUAL                VALUE '1'.
00139          88  AM-HAS-GROUP                     VALUE '2'.
00140      12  AM-STATUS                         PIC X.
00141          88  AM-ACCOUNT-ACTIVE                VALUE '0'.
00142          88  AM-ACCOUNT-INACTIVE              VALUE '1'.
00143          88  AM-ACCOUNT-TRANSFERRED           VALUE '2'.
102004         88  AM-ACCOUNT-CANCELLED             VALUE '3'.
022808         88  AM-ACCOUNT-FROZEN                VALUE '4'.
031811         88  AM-ACCOUNT-SUSPENDED             VALUE '5'.
021916         88  AM-ACCOUNT-DROPPED               VALUE '6'.
021916         88  AM-ACCOUNT-LAPSED                VALUE '7'.
021916         88  AM-ACCOUNT-RUN-OFF               VALUE '8'.
021916         88  AM-ACCOUNT-PENDING               VALUE '9'.
00144      12  AM-REMIT-TO                       PIC 99.
00145      12  AM-ID-NO                          PIC X(11).
00146
00147      12  AM-CAL-TABLE                      PIC XX.
00148      12  AM-LF-DEVIATION                   PIC XXX.
00149      12  AM-AH-DEVIATION                   PIC XXX.
00150      12  AM-LF-DEVIATION-PCT               PIC S9V9(6)    COMP-3.
00151      12  AM-AH-DEVIATION-PCT               PIC S9V9(6)    COMP-3.
00152      12  AM-LF-OB-RATE                     PIC S99V9(5)   COMP-3.
00153      12  AM-AH-OB-RATE                     PIC S99V9(5)   COMP-3.
00154      12  AM-LF-OB-RATE-JNT                 PIC S99V9(5)   COMP-3.
00155      12  AM-AH-OB-RATE-JNT                 PIC S99V9(5)   COMP-3.
00156
00157      12  AM-USER-FIELDS.
00158          16  AM-FLD-1                      PIC XX.
00159          16  AM-FLD-2                      PIC XX.
00160          16  AM-FLD-3                      PIC XX.
00161          16  AM-FLD-4                      PIC XX.
00162          16  AM-FLD-5                      PIC XX.
00163
00164      12  AM-1ST-PROD-DATE.
00165          16  AM-1ST-PROD-YR                PIC XX.
00166          16  AM-1ST-PROD-MO                PIC XX.
00167          16  AM-1ST-PROD-DA                PIC XX.
00168      12  AM-ANNIVERSARY-DATE               PIC 9(11)  COMP-3.
00169      12  AM-CERTS-PURGED-DATE.
00170          16  AM-PUR-YR                     PIC XX.
00171          16  AM-PUR-MO                     PIC XX.
00172          16  AM-PUR-DA                     PIC XX.
00173      12  AM-HI-CERT-DATE                   PIC 9(11)  COMP-3.
00174      12  AM-LO-CERT-DATE                   PIC 9(11)  COMP-3.
00175      12  AM-ENTRY-DATE                     PIC 9(11)  COMP-3.
00176      12  AM-INACTIVE-DATE.
00177          16  AM-INA-MO                     PIC 99.
00178          16  AM-INA-DA                     PIC 99.
00179          16  AM-INA-YR                     PIC 99.
00180      12  AM-AR-HI-CERT-DATE                PIC XX.
00181
00182      12  AM-LF-PSI-FACTOR                  PIC S9V9(6)    COMP-3.
00183      12  AM-AH-PSI-FACTOR                  PIC S9V9(6)    COMP-3.
00184
00185      12  AM-OB-PAYMENT-MODE                PIC X.
00186          88  AM-OB-PAID-MONTHLY               VALUE 'M' ' '.
00187          88  AM-OB-PAID-QUARTERLY             VALUE 'Q'.
00188          88  AM-OB-PAID-SEMI-ANNUALLY         VALUE 'S'.
00189          88  AM-OB-PAID-ANNUALLY              VALUE 'A'.
00190
00191      12  AM-AH-ONLY-INDICATOR              PIC X.
00192          88  AM-AH-ONLY-ALLOWED               VALUE 'Y' ' '.
00193          88  AM-NO-AH-ONLY                    VALUE 'N'.
00194
00195      12  AM-EDIT-LOAN-OFC                  PIC X(01).
00196
00197      12  AM-OVER-SHORT.
00198          16 AM-OVR-SHT-AMT                 PIC S999V99    COMP-3.
00199          16 AM-OVR-SHT-PCT                 PIC S9V9(4)    COMP-3.
00200
011410     12  AM-DCC-PRODUCT-CODE               PIC XXX.
041910     12  AM-DCC-CLP-STATE                  PIC XX.
00202
00203      12  AM-RECALC-COMM                    PIC X.
00204      12  AM-RECALC-REIN                    PIC X.
00205
00206      12  AM-REI-TABLE                      PIC XXX.
00207      12  AM-REI-ET-LF                      PIC X.
00208      12  AM-REI-ET-AH                      PIC X.
00209      12  AM-REI-PE-LF                      PIC X.
00210      12  AM-REI-PE-AH                      PIC X.
00211      12  AM-REI-PRT-ST                     PIC X.
00212      12  AM-REI-FEE-LF                     PIC S9V9999    COMP-3.
00213      12  AM-REI-FEE-AH                     PIC S9V9999    COMP-3.
00214      12  AM-REI-LF-TAX                     PIC S9V9999    COMP-3.
00215      12  AM-REI-GROUP-A                    PIC X(6).
00216      12  AM-REI-MORT                       PIC X(4).
00217      12  AM-REI-PRT-OW                     PIC X.
00218      12  AM-REI-PR-PCT                     PIC S9V9999    COMP-3.
00219      12  AM-REI-78-PCT                     PIC S9V9999    COMP-3.
00220      12  AM-REI-AH-TAX                     PIC S9V9999    COMP-3.
00221      12  AM-REI-GROUP-B                    PIC X(6).
00222
00223      12  AM-TRUST-TYPE                     PIC X(2).
00224
00225      12  AM-EMPLOYER-STMT-USED             PIC X.
00226      12  AM-GROUPED-CHECKS-Y-N             PIC X.
00227
00228      12  AM-STD-AH-TYPE                    PIC XX.
00229      12  AM-EARN-METHODS.
00230          16  AM-EARN-METHOD-R              PIC X.
00231              88 AM-REF-RL-R78                 VALUE 'R'.
00232              88 AM-REF-RL-PR                  VALUE 'P'.
00233              88 AM-REF-RL-MEAN                VALUE 'M'.
00234              88 AM-REF-RL-ANTICIPATION        VALUE 'A'.
00235          16  AM-EARN-METHOD-L              PIC X.
00236              88 AM-REF-LL-R78                 VALUE 'R'.
00237              88 AM-REF-LL-PR                  VALUE 'P'.
00238              88 AM-REF-LL-MEAN                VALUE 'M'.
00239              88 AM-REF-LL-ANTICIPATION        VALUE 'A'.
00240          16  AM-EARN-METHOD-A              PIC X.
00241              88 AM-REF-AH-R78                 VALUE 'R'.
00242              88 AM-REF-AH-PR                  VALUE 'P'.
00243              88 AM-REF-AH-MEAN                VALUE 'M'.
00244              88 AM-REF-AH-ANTICIPATION        VALUE 'A'.
00245              88 AM-REF-AH-CALIF-SPEC          VALUE 'C'.
00246              88 AM-REF-AH-NET                 VALUE 'N'.
00247
00248      12  AM-TOL-PREM                       PIC S999V99    COMP-3.
00249      12  AM-TOL-REF                        PIC S999V99    COMP-3.
00250      12  AM-TOL-CLM                        PIC S999V99    COMP-3.
00251
00252      12  AM-RET-Y-N                        PIC X.
00253      12  AM-RET-P-E                        PIC X.
00254      12  AM-LF-RET                         PIC S9V9999    COMP-3.
00255      12  AM-AH-RET                         PIC S9V9999    COMP-3.
00256      12  AM-RET-GRP                        PIC X(6).
00257      12  AM-RETRO-POOL  REDEFINES  AM-RET-GRP.
00258          16  AM-POOL-PRIME                 PIC XXX.
00259          16  AM-POOL-SUB                   PIC XXX.
00260      12  AM-RETRO-EARNINGS.
00261          16  AM-RET-EARN-R                 PIC X.
00262          16  AM-RET-EARN-L                 PIC X.
00263          16  AM-RET-EARN-A                 PIC X.
00264      12  AM-RET-ST-TAX-USE                 PIC X.
00265          88  CHARGE-ST-TAXES-ON-RETRO         VALUE 'Y' 'E' 'P'.
00266          88  TAXES-NOT-IN-RETRO               VALUE 'N' ' '.
00267      12  AM-RETRO-BEG-EARNINGS.
00268          16  AM-RET-BEG-EARN-R             PIC X.
00269          16  AM-RET-BEG-EARN-L             PIC X.
00270          16  AM-RET-BEG-EARN-A             PIC X.
00271      12  AM-RET-MIN-LOSS-L                 PIC SV999      COMP-3.
00272      12  AM-RET-MIN-LOSS-A                 PIC SV999      COMP-3.
00273
00274      12  AM-USER-SELECT-OPTIONS.
00275          16  AM-USER-SELECT-1              PIC X(10).
00276          16  AM-USER-SELECT-2              PIC X(10).
00277          16  AM-USER-SELECT-3              PIC X(10).
00278          16  AM-USER-SELECT-4              PIC X(10).
00279          16  AM-USER-SELECT-5              PIC X(10).
00280
00281      12  AM-LF-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3.
00282
00283      12  AM-AH-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3.
00284
00285      12  AM-RPT045A-SWITCH                 PIC X.
00286          88  RPT045A-OFF                   VALUE 'N'.
00287
00288      12  AM-INSURANCE-LIMITS.
00289          16  AM-MAX-MON-BEN                PIC S9(7)      COMP-3.
00290          16  AM-MAX-TOT-BEN                PIC S9(7)      COMP-3.
00291
00292      12  AM-PROFILE-CHANGE-SWITCH          PIC X.
00293          88  AM-PROFILE-DATA-CHANGED          VALUE '*'.
00294
00295      12  AM-DISMBR-COVERAGE-SW             PIC X.
00296          88  AM-DISMBR-COVERAGE               VALUE 'Y'.
00297          88  AM-NO-DISMBR-COVERAGE            VALUE 'N'.
00298
00299      12  AM-CANCEL-FEE                     PIC S9(3)V9(2) COMP-3.
00300
00301      12  AM-TOL-REF-PCT                    PIC S9V9(4)    COMP-3.
090803     12  AM-CLP-TOL-PCT                    PIC S9V9(4)    COMP-3.
092705     12  AM-SPP-LEASE-COMM                 PIC S9(5)V99   COMP-3.
           12  AM-DCC-MAX-MARKETING-FEE          PIC S9(5)      COMP-3.
           12  AM-DCC-UEF-STATE                  PIC XX.
           12  FILLER                            PIC XXX.
120406     12  AM-REPORT-CODE-3                  PIC X(10).
090803*    12  FILLER                            PIC X(22).
00303
00304      12  AM-RESERVE-DATE.
00305          16  AM-TARGET-LOSS-RATIO          PIC S9V9(4) COMP-3.
00306          16  AM-LIFE-IBNR-PCT              PIC S9V9(4) COMP-3.
00307          16  AM-CRDT-MODIFICATION-PCT      PIC S9V9(4) COMP-3.
00308
00309      12  AM-3RD-PARTY-NOTIF-LEVEL          PIC 99.
00310      12  AM-NOTIFICATION-TYPES.
00311          16  AM-NOTIF-OF-LETTERS           PIC X.
00312          16  AM-NOTIF-OF-PAYMENTS          PIC X.
00313          16  AM-NOTIF-OF-REPORTS           PIC X.
00314          16  AM-NOTIF-OF-STATUS            PIC X.
00315
00316      12  AM-BENEFIT-TABLE-USAGE            PIC X.
00317          88  AM-BENEFIT-TABLE-USED            VALUE 'Y'.
00318          88  AM-USE-DEVIATIONS-ONLY           VALUE 'D'.
00319          88  AM-EDIT-BENEFITS-ONLY            VALUE 'E'.
00320          88  AM-EDITS-NOT-USED                VALUE ' '  'N'.
00321
00322      12  AM-BENEFIT-CONTROLS.
00323          16  AM-ALLOWABLE-BENEFITS  OCCURS  20  TIMES.
00324              20  AM-BENEFIT-CODE           PIC XX.
00325              20  AM-BENEFIT-TYPE           PIC X.
00326              20  AM-BENEFIT-REVISION       PIC XXX.
00327              20  AM-BENEFIT-REM-TERM       PIC X.
00328              20  AM-BENEFIT-RETRO-Y-N      PIC X.
00329              20  FILLER                    PIC XX.
00330          16  FILLER                        PIC X(80).
00331
00332      12  AM-TRANSFER-DATA.
00333          16  AM-TRANSFERRED-FROM.
00334              20  AM-TRNFROM-CARRIER        PIC X.
00335              20  AM-TRNFROM-GROUPING.
00336                  24  AM-TRNFROM-GRP-PREFIX PIC XXX.
00337                  24  AM-TRNFROM-GRP-PRIME  PIC XXX.
00338              20  AM-TRNFROM-STATE          PIC XX.
00339              20  AM-TRNFROM-ACCOUNT.
00340                  24  AM-TRNFROM-ACCT-PREFIX PIC X(4).
00341                  24  AM-TRNFROM-ACCT-PRIME PIC X(6).
00342              20  AM-TRNFROM-DTE            PIC XX.
00343          16  AM-TRANSFERRED-TO.
00344              20  AM-TRNTO-CARRIER          PIC X.
00345              20  AM-TRNTO-GROUPING.
00346                  24  AM-TRNTO-GRP-PREFIX   PIC XXX.
00347                  24  AM-TRNTO-GRP-PRIME    PIC XXX.
00348              20  AM-TRNTO-STATE            PIC XX.
00349              20  AM-TRNTO-ACCOUNT.
00350                  24  AM-TRNTO-ACCT-PREFIX  PIC X(4).
00351                  24  AM-TRNTO-ACCT-PRIME   PIC X(6).
00352              20  AM-TRNTO-DTE              PIC XX.
00353          16  FILLER                        PIC X(10).
00354
00355      12  AM-SAVED-REMIT-TO                 PIC 99.
00356
00357      12  AM-COMM-STRUCTURE-SAVED.
00358          16  AM-DEFN-1-SAVED.
00359              20  AM-AGT-COMMS-SAVED    OCCURS 10 TIMES.
00360                  24  AM-AGT-SV             PIC X(10).
00361                  24  AM-COM-TYP-SV         PIC X.
00362                  24  AM-L-COM-SV           PIC SV9(5)     COMP-3.
00363                  24  AM-J-COM-SV           PIC SV9(5)     COMP-3.
00364                  24  AM-A-COM-SV           PIC SV9(5)     COMP-3.
00365                  24  AM-RECALC-LV-INDIC-SV PIC X.
00366                  24  FILLER                PIC X.
00367                  24  AM-GL-CODES-SV        PIC X.
00368                  24  AM-COM-CHARGEBACK-SV  PIC 99.
00369                  24  FILLER                PIC X.
00370          16  AM-DEFN-2-SAVED   REDEFINES   AM-DEFN-1-SAVED.
00371              20  AM-COM-TBLS-SAVED    OCCURS 10 TIMES.
00372                  24  FILLER                PIC X(11).
00373                  24  AM-L-COMA-SV          PIC XXX.
00374                  24  AM-J-COMA-SV          PIC XXX.
00375                  24  AM-A-COMA-SV          PIC XXX.
00376                  24  FILLER                PIC X(6).
00377
00378      12  AM-FLC-NET-PREMIUM-ALLOWANCE.
00379          16 AM-ACCOUNT-ALLOWANCE OCCURS  5 TIMES.
00380             20  AM-ALLOW-BEGIN-RANGE       PIC S9(5)      COMP-3.
00381             20  AM-ALLOW-END-RANGE         PIC S9(5)      COMP-3.
00382             20  AM-ALLOWANCE-AMT           PIC S9(5)V99   COMP-3.
00383
122806     12  AM-ORIG-DEALER-NO                 PIC X(10).
122806     12  FILLER                            PIC X(120).
00385
00386      12  AM-ACCOUNT-EXECUTIVE-DATA.
00387          16  AM-CONTROL-NAME               PIC X(30).
00388          16  AM-EXECUTIVE-ONE.
00389              20  AM-EXEC1-NAME             PIC X(15).
00390              20  AM-EXEC1-DIS-PERCENT      PIC S9(01)V9(04)
00391                                                           COMP-3.
00392              20  AM-EXEC1-LIFE-PERCENT     PIC S9(01)V9(04)
00393                                                           COMP-3.
00394          16  AM-EXECUTIVE-TWO.
00395              20  AM-EXEC2-NAME             PIC X(15).
00396              20  AM-EXEC2-DIS-PERCENT      PIC S9(01)V9(04)
00397                                                           COMP-3.
00398              20  AM-EXEC2-LIFE-PERCENT     PIC S9(01)V9(04)
00399                                                           COMP-3.
00400
00401      12  AM-RETRO-ADDITIONAL-DATA.
00402          16  AM-RETRO-QUALIFY-LIMIT        PIC S9(7)      COMP-3.
00403          16  AM-RETRO-PREM-P-E             PIC X.
00404          16  AM-RETRO-CLMS-P-I             PIC X.
00405          16  AM-RETRO-RET-BRACKET-LF.
00406              20  AM-RETRO-RET-METHOD-LF    PIC X.
00407                  88  AM-RETRO-USE-PCT-LF      VALUE 'P' ' '.
00408                  88  AM-RETRO-USE-SCALE-LF    VALUE 'S'.
00409              20  AM-RETRO-RET-BASIS-LF     PIC X.
00410                  88  AM-RETRO-EARN-BASIS-LF   VALUE 'E' ' '.
00411                  88  AM-RETRO-PAID-BASIS-LF   VALUE 'P'.
00412              20  AM-RETRO-BRACKETS-LF  OCCURS  3 TIMES.
00413                  24  AM-RETRO-RET-PCT-LF   PIC S9V9999    COMP-3.
00414                  24  AM-RETRO-RET-THRU-LF  PIC S9(7)      COMP-3.
00415          16  AM-RETRO-RET-BRACKET-AH.
00416              20  AM-RETRO-RET-METHOD-AH    PIC X.
00417                  88  AM-RETRO-USE-PCT-AH      VALUE 'P' ' '.
00418                  88  AM-RETRO-USE-SCALE-AH    VALUE 'S'.
00419                  88  AM-RETRO-USE-LIFE-METHOD VALUE 'L'.
00420              20  AM-RETRO-RET-BASIS-AH     PIC X.
00421                  88  AM-RETRO-EARN-BASIS-AH   VALUE 'E' ' '.
00422                  88  AM-RETRO-PAID-BASIS-AH   VALUE 'P'.
00423              20  AM-RETRO-BRACKETS-AH  OCCURS  3 TIMES.
00424                  24  AM-RETRO-RET-PCT-AH   PIC S9V9999    COMP-3.
00425                  24  AM-RETRO-RET-THRU-AH  PIC S9(7)      COMP-3.
00426
00427      12  AM-COMMENTS.
00428          16  AM-COMMENT-LINE           PIC X(50)   OCCURS 5 TIMES.
00429
00430      12  AM-CLIENT-OVERLAY-FLI   REDEFINES   AM-COMMENTS.
00431          16  AM-FLI-RETRO-SHARE-CODE       PIC X.
00432          16  AM-FLI-BILLING-CODE           PIC X.
00433          16  AM-FLI-ALT-STATE-CODE         PIC XX.
00434          16  AM-FLI-UNITED-IDENT           PIC X.
00435          16  AM-FLI-INTEREST-LOST-DATA.
00436              20  AM-FLI-BANK-NO            PIC X(5).
00437              20  AM-FLI-BANK-BALANCE       PIC S9(9)V99   COMP-3.
00438              20  AM-FLI-BANK-1ST-6-PREM    PIC S9(9)V99   COMP-3.
00439              20  AM-FLI-BANK-CAP-AMT       PIC S9(9)V99   COMP-3.
00440          16  AM-FLI-ALT-AGENT-CODES   OCCURS 10 TIMES.
00441              20  AM-FLI-AGT                PIC X(9).
00442              20  AM-FLI-AGT-COMM-ACC       PIC X.
00443              20  AM-FLI-AGT-SHARE-PCT      PIC S9V99      COMP-3.
00444          16  FILLER                        PIC X(102).
00445
00446      12  AM-CLIENT-OVERLAY-DMD   REDEFINES   AM-COMMENTS.
00447          16  AM-ALLOWABLE-DMD-BENEFITS  OCCURS 30 TIMES.
00448              20  AM-BENEFIT-DMD-CODE         PIC XX.
00449              20  AM-BENEFIT-DMD-TYPE         PIC X.
00450              20  AM-BENEFIT-DMD-REVISION     PIC XXX.
00451              20  AM-BENEFIT-DMD-REM-TERM     PIC X.
00452              20  AM-BENEFIT-DMD-RETRO-Y-N    PIC X.
00453          16  FILLER                          PIC X(10).
00454 ******************************************************************
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
010412*        COPY ELC631PI.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELC631PI                            *
00004 *                            VMOD=2.012                          *
00005 *                                                                *
00006 *    THIS IS THE PI-PROGRAM-WORK-AREA THAT IS USED FOR THE       *
00007 *    REVIEW AND CORRRECTION SUB-SYSTEM.  ANY CHANGES WILL        *
00008 *    WILL EFFECT THE PROGRAMS OF THAT SUB-SYSTEM.                *
00009 *                                                                *
00010 *    IF THE LENGTH OF THIS PI-AREA CHANGES THE LENGTH MUST       *
00011 *    BE CHANGED FOR THE COMM-AREA WHEN PASSING THIS PI-AREA      *
00012 *    BETWEEN PROGRAMS.                                           *
00013 *                                                                *
00014 *    THE FOLLOWING PROGRAMS USE THIS COPYBOOK:                   *
00015 *                                                                *
00016 *               EL631 - EL6311 - EL6312 - EL6313                 *
00017 *                                                                *
00018 ******************************************************************
021414*                   C H A N G E   L O G
021414*
021414* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
021414*-----------------------------------------------------------------
021414*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
021414* EFFECTIVE    NUMBER
021414*-----------------------------------------------------------------
021414* 021414    2003053000001  PEMA  changes for auto chk request
021414******************************************************************
00019
00020          16  PI-631-DATA.
00021              20  PI-ERPNDB-KEY.
00022                  24  PI-PB-COMPANY-CD     PIC X.
00023                  24  PI-PB-ENTRY-BATCH    PIC X(6).
00024                  24  PI-PB-BATCH-SEQ-NO   PIC S9(4) COMP.
00025                  24  PI-PB-BATCH-CHG-SEQ-NO PIC S9(4) COMP.
00026
00027              20  PI-ERPNDB-ALT-KEY.
00028                  24  PI-PB-COMPANY-CD-A1  PIC X.
00029                  24  PI-PB-CARRIER        PIC X.
00030                  24  PI-PB-GROUPING       PIC X(6).
00031                  24  PI-PB-STATE          PIC XX.
00032                  24  PI-PB-ACCOUNT        PIC X(10).
00033                  24  PI-PB-CERT-EFF-DT    PIC XX.
00034                  24  PI-PB-CERT-NO.
00035                      28  PI-PB-CERT-PRIME PIC X(10).
00036                      28  PI-PB-CERT-SFX   PIC X.
00037                  24  PI-PB-ALT-CHG-SEQ-NO PIC S9(4) COMP.
00038                  24  PI-PB-RECORD-TYPE    PIC X.
00039
00040              20  PI-ERPNDB-CSR-KEY.
00041                  24  PI-PB-CSR-COMPANY-CD-A2  PIC X.
00042                  24  PI-PB-CSR-ID             PIC X(4).
00043                  24  PI-PB-CSR-ENTRY-BATCH    PIC X(6).
00044                  24  PI-PB-CSR-BTCH-SEQ-NO    PIC S9(4) COMP.
00045                  24  PI-PB-CSR-BTCH-CHG-SEQ-NO PIC S9(4) COMP.
00046
00047              20  PI-BROWSE-TYPE               PIC X.
00048                  88  PI-FILE-BROWSE             VALUE ' '.
00049                  88  PI-PRIMARY-BROWSE          VALUE '1'.
00050                  88  PI-ALTERNATE-BROWSE        VALUE '2'.
00051                  88  PI-PRIMARY-WITH-SELECT     VALUE '3'.
00052                  88  PI-CSR-BROWSE              VALUE '4'.
00053
00054              20  PI-MAINT-FUNCTION            PIC X.
00055                  88  PI-ADD-FUNCTION            VALUE 'A'.
00056                  88  PI-BROWSE-FUNCTION         VALUE 'B'.
00057                  88  PI-CHANGE-FUNCTION         VALUE 'C'.
00058                  88  PI-DELETE-FUNCTION         VALUE 'D'.
00059                  88  PI-SHOW-FUNCTION           VALUE 'S'.
00060                  88  PI-PF5-FUNCTION            VALUE '5'.
00061                  88  PI-PF6-FUNCTION            VALUE '6'.
00062
00063              20  PI-FILE-SWITCHES.
00064                  24  PI-ALL-ISSUES-SW         PIC X.
00065                      88  ALL-ISSUES             VALUE 'Y'.
00066                  24  PI-ALL-CANCELS-SW        PIC X.
00067                      88  ALL-CANCELS            VALUE 'Y'.
00068                  24  PI-ISSUES-IN-ERROR-SW    PIC X.
00069                      88  ISSUES-IN-ERROR        VALUE 'Y'.
00070                  24  PI-CANCELS-IN-ERROR-SW   PIC X.
00071                      88  CANCEL-IN-ERROR        VALUE 'Y'.
00072                  24  PI-ONLY-BATCH-HEADERS-SW PIC X.
00073                      88  ONLY-BATCH-HEADERS     VALUE 'Y'.
00074                  24  PI-ALL-OUT-OF-BAL-SW     PIC X.
00075                      88  ALL-OUT-OF-BAL         VALUE 'Y'.
00076                  24  PI-HOLD-REC-SW           PIC X.
00077                      88  DISPLAY-HOLD-RECORDS   VALUE 'Y'.
00078                  24  PI-CHANGE-REC-SW         PIC X.
00079                      88  DISPLAY-CHANGE-RECORDS VALUE 'Y'.
00080                  24  PI-CHK-REQ-REC-SW        PIC X.
00081                      88  DISPLAY-CHK-REQ-RECORDS VALUE 'Y'.
00082                  24  PI-ISSUE-WARNING-SW      PIC X.
00083                      88  ISSUE-WITH-WARNING     VALUE 'Y'.
00084                  24  PI-CANCEL-WARNING-SW     PIC X.
00085                      88  CANCEL-WITH-WARNING    VALUE 'Y'.
00086              20  PI-DISPLAY-SCREEN-SW         PIC X.
00087                      88  PI-DISPLAY-SCREEN      VALUE 'Y'.
00088              20  PI-ORIGINAL-BATCH-SW         PIC X.
00089                      88  PI-DISPLAY-ORIGINAL-BATCH VALUE 'Y'.
00090
00091              20  PI-MAP-NAME                  PIC X(8).
00092
00093              20  PI-CURSOR                    PIC S9(4) COMP.
00094
00095              20  PI-PREV-ALT-KEY              PIC X(36).
00096              20  PI-PREV-CSR-KEY              PIC X(15).
00097              20  PI-PREV-KEY.
00098                  24  PI-PREV-COMPANY-CD       PIC X.
00099                  24  PI-PREV-BATCH            PIC X(6).
00100                  24  PI-PREV-SEQ-NO           PIC S9(4) COMP.
00101                  24  PI-PREV-CHG-SEQ-NO       PIC S9(4) COMP.
00102              20  PI-PREV-CONTROL-PRIMARY      PIC X(11).
00103              20  PI-BROWSE-SW                 PIC X.
00104                  88  PI-GOOD-BROWSE             VALUE 'Y'.
00105                  88  PI-NO-PB-RECS-FOUND        VALUE '9'.
00106              20  PI-SV-CARRIER                PIC X.
00107              20  PI-SV-GROUPING               PIC X(6).
00108              20  PI-SV-STATE                  PIC XX.
00109              20  PI-EDIT-SW                   PIC X.
00110              20  PI-DISPLAY-SW                PIC XX.
00111                  88 PI-DISPLAY-LIFE        VALUE 'LF'.
00112                  88 PI-DISPLAY-AH          VALUE 'AH'.
00113              20  PI-CRITERIA-DATA             PIC X(350).
00114              20  PI-BMODE                     PIC X.
00115              20  PI-BPMTAMT                   PIC S9(7)V99 COMP-3.
00116              20  PI-BPMTS                     PIC S999     COMP-3.
00117              20  PI-BTYPE                     PIC XXX OCCURS 2.
00118              20  PI-HIGH-SEQ-NO               PIC S9(4) COMP.
                   20  PI-CSR-SESSION-SW            PIC X.
                       88  CSR-EDIT-SESSION           VALUE 'Y'.
                   20  PI-ERRORS-SW                 PIC X.
062712                 88  FATAL-ERRORS               VALUE 'X'.
062712*                88  FATAL-OR-UNFORCED          VALUE 'X'.
021414             20  pi-unforced-sw               pic x.
021414                 88  unforced-errors            value 'X'.
021414             20  FILLER                       PIC X(4).
00120
010412         16  FILLER                PIC X(94).
010412
010412
00233 *                                    COPY ELCINTF.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCINTF.                            *
00004 *                            VMOD=2.017                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON DATA AREA                 *
00007 *                                                                *
00008 *       LENGTH = 1024                                            *
00009 *                                                                *
00010 ******************************************************************
011812*                   C H A N G E   L O G
011812*
011812* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
011812*-----------------------------------------------------------------
011812*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
011812* EFFECTIVE    NUMBER
011812*-----------------------------------------------------------------
011812* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
011812******************************************************************
00011  01  PROGRAM-INTERFACE-BLOCK.
00012      12  PI-COMM-LENGTH                PIC S9(4) COMP VALUE +1024.
00013      12  PI-CALLING-PROGRAM              PIC X(8).
00014      12  PI-SAVED-PROGRAM-1              PIC X(8).
00015      12  PI-SAVED-PROGRAM-2              PIC X(8).
00016      12  PI-SAVED-PROGRAM-3              PIC X(8).
00017      12  PI-SAVED-PROGRAM-4              PIC X(8).
00018      12  PI-SAVED-PROGRAM-5              PIC X(8).
00019      12  PI-SAVED-PROGRAM-6              PIC X(8).
00020      12  PI-RETURN-TO-PROGRAM            PIC X(8).
00021      12  PI-COMPANY-ID                   PIC XXX.
00022      12  PI-COMPANY-CD                   PIC X.
00023
00024      12  PI-COMPANY-PASSWORD             PIC X(8).
00025
00026      12  PI-JOURNAL-FILE-ID              PIC S9(4) COMP.
00027
00028      12  PI-CONTROL-IN-PROGRESS.
00029          16  PI-CARRIER                  PIC X.
00030          16  PI-GROUPING                 PIC X(6).
00031          16  PI-STATE                    PIC XX.
00032          16  PI-ACCOUNT                  PIC X(10).
00033          16  PI-PRODUCER REDEFINES PI-ACCOUNT
00034                                          PIC X(10).
00035          16  PI-CLAIM-CERT-GRP.
00036              20  PI-CLAIM-NO             PIC X(7).
00037              20  PI-CERT-NO.
00038                  25  PI-CERT-PRIME       PIC X(10).
00039                  25  PI-CERT-SFX         PIC X.
00040              20  PI-CERT-EFF-DT          PIC XX.
00041          16  PI-PLAN-DATA REDEFINES PI-CLAIM-CERT-GRP.
00042              20  PI-PLAN-CODE            PIC X(2).
00043              20  PI-REVISION-NUMBER      PIC X(3).
00044              20  PI-PLAN-EFF-DT          PIC X(2).
00045              20  PI-PLAN-EXP-DT          PIC X(2).
00046              20  FILLER                  PIC X(11).
00047          16  PI-OE-REFERENCE-1 REDEFINES PI-CLAIM-CERT-GRP.
00048              20  PI-OE-REFERENCE-1.
00049                  25  PI-OE-REF-1-PRIME   PIC X(18).
00050                  25  PI-OE-REF-1-SUFF    PIC XX.
00051
00052      12  PI-SESSION-IN-PROGRESS          PIC X.
00053          88  CLAIM-SESSION                   VALUE '1'.
00054          88  CREDIT-SESSION                  VALUE '2'.
00055          88  WARRANTY-SESSION                VALUE '3'.
00056          88  MORTGAGE-SESSION                VALUE '4'.
00057          88  GENERAL-LEDGER-SESSION          VALUE '5'.
00058
00059
00060 *THE FOLLOWING TWO FIELDS ARE USED ONLY WITH MULTI COMPANY CLIENTS
00061
00062      12  PI-ORIGINAL-COMPANY-ID          PIC X(3).
00063      12  PI-ORIGINAL-COMPANY-CD          PIC X.
00064
00065      12  PI-CREDIT-USER                  PIC X.
00066          88  PI-NOT-CREDIT-USER              VALUE 'N'.
00067          88  PI-HAS-CLAS-IC-CREDIT           VALUE 'Y'.
00068
00069      12  PI-CLAIM-USER                   PIC X.
00070          88  PI-NOT-CLAIM-USER               VALUE 'N'.
00071          88  PI-HAS-CLAS-IC-CLAIM            VALUE 'Y'.
00072
00073      12  PI-PROCESSOR-SYS-ACCESS         PIC X.
00074          88  PI-ACCESS-TO-BOTH-SYSTEMS       VALUE ' '.
00075          88  PI-ACCESS-TO-ALL-SYSTEMS        VALUE ' '.
00076          88  PI-ACCESS-TO-CLAIM-ONLY         VALUE '1'.
00077          88  PI-ACCESS-TO-CREDIT-ONLY        VALUE '2'.
00078          88  PI-ACCESS-TO-MORTGAGE-ONLY      VALUE '3'.
00079
00080      12  PI-PROCESSOR-ID                 PIC X(4).
00081
00082      12  PI-PROCESSOR-PASSWORD           PIC X(11).
00083
00084      12  PI-MEMBER-CAPTION               PIC X(10).
00085
00086      12  PI-PROCESSOR-USER-ALMIGHTY      PIC X.
00087          88  PI-USER-ALMIGHTY-YES            VALUE 'Y'.
00088
00089      12  PI-LIFE-OVERRIDE-L1             PIC X.
00090      12  PI-LIFE-OVERRIDE-L2             PIC XX.
00091      12  PI-LIFE-OVERRIDE-L6             PIC X(6).
00092      12  PI-LIFE-OVERRIDE-L12            PIC X(12).
00093
00094      12  PI-AH-OVERRIDE-L1               PIC X.
00095      12  PI-AH-OVERRIDE-L2               PIC XX.
00096      12  PI-AH-OVERRIDE-L6               PIC X(6).
00097      12  PI-AH-OVERRIDE-L12              PIC X(12).
00098
00099      12  PI-NEW-SYSTEM                   PIC X(2).
00100
00101      12  PI-PRIMARY-CERT-NO              PIC X(11).
00102      12  PI-CLAIM-PAID-THRU-TO           PIC X(01).
00103          88  PI-USES-PAID-TO                 VALUE '1'.
00104      12  PI-CRDTCRD-SYSTEM.
00105          16  PI-CRDTCRD-USER             PIC X.
00106              88  PI-NOT-CRDTCRD-USER         VALUE 'N'.
00107              88  PI-HAS-CLAS-IC-CRDTCRD      VALUE 'Y'.
00108          16  PI-CC-MONTH-END-DT          PIC XX.
00109      12  PI-PROCESSOR-PRINTER            PIC X(4).
00110
00111      12  PI-OE-REFERENCE-2.
00112          16  PI-OE-REF-2-PRIME           PIC X(10).
00113          16  PI-OE-REF-2-SUFF            PIC X.
00114
00115      12  PI-REM-TRM-CALC-OPTION          PIC X.
00116
00117      12  PI-LANGUAGE-TYPE                PIC X.
00118              88  PI-LANGUAGE-IS-ENG          VALUE 'E'.
00119              88  PI-LANGUAGE-IS-FR           VALUE 'F'.
00120              88  PI-LANGUAGE-IS-SPAN         VALUE 'S'.
00121
00122      12  PI-POLICY-LINKAGE-IND           PIC X.
00123          88  PI-USE-POLICY-LINKAGE           VALUE 'Y'.
00124          88  PI-POLICY-LINKAGE-NOT-USED      VALUE 'N'
00125                                                    LOW-VALUES.
00126
00127      12  PI-ALT-DMD-PRT-ID               PIC X(4).
00128      12  PI-CLAIM-PW-SESSION             PIC X(1).
00129          88  PI-CLAIM-CREDIT                 VALUE '1'.
00130          88  PI-CLAIM-CONVEN                 VALUE '2'.
011812
011812     12  PI-PROCESSOR-CSR-IND            PIC X.
011812         88  PI-PROCESSOR-IS-CSR             VALUE 'Y' 'S'.
011812         88  PI-PROCESSOR-IS-CSR-SUPER       VALUE 'S'.
011812
011812     12  FILLER                          PIC X(3).
00132
00133      12  PI-SYSTEM-LEVEL                 PIC X(145).
00134
00135      12  PI-CLAIMS-CREDIT-LEVEL          REDEFINES
00136          PI-SYSTEM-LEVEL.
00137
00138          16  PI-ENTRY-CODES.
00139              20  PI-ENTRY-CD-1           PIC X.
00140              20  PI-ENTRY-CD-2           PIC X.
00141
00142          16  PI-RETURN-CODES.
00143              20  PI-RETURN-CD-1          PIC X.
00144              20  PI-RETURN-CD-2          PIC X.
00145
00146          16  PI-UPDATE-STATUS-SAVE.
00147              20  PI-UPDATE-BY            PIC X(4).
00148              20  PI-UPDATE-HHMMSS        PIC S9(7)     COMP-3.
00149
00150          16  PI-LOWER-CASE-LETTERS       PIC X.
00151              88  LOWER-CASE-LETTERS-USED     VALUE 'Y'.
00152
00153 *        16  PI-CLAIM-ACCESS-CONTROL     PIC X.
00154 *            88  CLAIM-NO-UNIQUE             VALUE '1'.
00155 *            88  CARRIER-CLM-CNTL            VALUE '2'.
00156
00157          16  PI-CERT-ACCESS-CONTROL      PIC X.
00158              88  ST-ACCNT-CNTL               VALUE ' '.
00159              88  CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
00160              88  CARR-ST-ACCNT-CNTL          VALUE '2'.
00161              88  ACCNT-CNTL                  VALUE '3'.
00162              88  CARR-ACCNT-CNTL             VALUE '4'.
00163
00164          16  PI-PROCESSOR-CAP-LIST.
00165              20  PI-SYSTEM-CONTROLS.
00166                 24 PI-SYSTEM-DISPLAY     PIC X.
00167                  88  SYSTEM-DISPLAY-CAP      VALUE 'Y'.
00168                 24 PI-SYSTEM-MODIFY      PIC X.
00169                  88  SYSTEM-MODIFY-CAP       VALUE 'Y'.
00170              20  FILLER                  PIC XX.
00171              20  PI-DISPLAY-CAP          PIC X.
00172                  88  DISPLAY-CAP             VALUE 'Y'.
00173              20  PI-MODIFY-CAP           PIC X.
00174                  88  MODIFY-CAP              VALUE 'Y'.
00175              20  PI-MSG-AT-LOGON-CAP     PIC X.
00176                  88  MSG-AT-LOGON-CAP        VALUE 'Y'.
00177              20  PI-FORCE-CAP            PIC X.
00178                  88  FORCE-CAP               VALUE 'Y'.
00179
00180          16  PI-PROGRAM-CONTROLS.
00181              20  PI-PGM-PRINT-OPT        PIC X.
00182              20  PI-PGM-FORMAT-OPT       PIC X.
00183              20  PI-PGM-PROCESS-OPT      PIC X.
00184              20  PI-PGM-TOTALS-OPT       PIC X.
00185
00186          16  PI-HELP-INTERFACE.
00187              20  PI-LAST-ERROR-NO        PIC X(4).
00188              20  PI-CURRENT-SCREEN-NO    PIC X(4).
00189
00190          16  PI-CARRIER-CONTROL-LEVEL    PIC X.
00191              88  CONTROL-IS-ACTUAL-CARRIER   VALUE SPACE.
00192
00193          16  PI-CR-CONTROL-IN-PROGRESS.
00194              20  PI-CR-CARRIER           PIC X.
00195              20  PI-CR-GROUPING          PIC X(6).
00196              20  PI-CR-STATE             PIC XX.
00197              20  PI-CR-ACCOUNT           PIC X(10).
00198              20  PI-CR-FIN-RESP          PIC X(10).
00199              20  PI-CR-TYPE              PIC X.
00200
00201          16  PI-CR-BATCH-NUMBER          PIC X(6).
00202
00203          16  PI-CR-MONTH-END-DT          PIC XX.
00204
00205          16  PI-CAR-GROUP-ACCESS-CNTL    PIC X.
00206              88  PI-USE-ACTUAL-CARRIER       VALUE ' '.
00207              88  PI-ZERO-CARRIER             VALUE '1'.
00208              88  PI-ZERO-GROUPING            VALUE '2'.
00209              88  PI-ZERO-CAR-GROUP           VALUE '3'.
00210
00211          16  PI-CARRIER-SECURITY         PIC X.
00212              88  PI-NO-CARRIER-SECURITY      VALUE ' '.
00213
00214          16  PI-ACCOUNT-SECURITY         PIC X(10).
00215              88  PI-NO-ACCOUNT-SECURITY      VALUE SPACES.
00216              88  PI-NO-PRODUCER-SECURITY     VALUE SPACES.
00217
00218          16  PI-CODE-SECURITY REDEFINES PI-ACCOUNT-SECURITY.
00219              20  PI-ACCESS-CODE          OCCURS 10 TIMES
00220                                          INDEXED BY PI-ACCESS-NDX
00221                                          PIC X.
00222
00223          16  PI-GA-BILLING-CONTROL       PIC X.
00224              88  PI-GA-BILLING               VALUE '1'.
00225
00226          16  PI-MAIL-PROCESSING          PIC X.
00227              88  PI-MAIL-YES                 VALUE 'Y'.
00228
00229          16  PI-SECURITY-TEMP-STORE-ID   PIC X(8).
00230
00231          16  PI-AR-SYSTEM.
00232              20  PI-AR-PROCESSING-CNTL   PIC X.
00233                  88  PI-AR-PROCESSING        VALUE 'Y'.
00234              20  PI-AR-SUMMARY-CODE      PIC X(6).
00235              20  PI-AR-MONTH-END-DT      PIC XX.
00236
00237          16  PI-MP-SYSTEM.
00238              20  PI-MORTGAGE-USER            PIC X.
00239                  88  PI-NOT-MORTGAGE-USER            VALUE 'N'.
00240                  88  PI-HAS-CLAS-IC-MORTGAGE         VALUE 'Y'.
00241              20  PI-MORTGAGE-ACCESS-CONTROL  PIC X.
00242                  88  PI-MP-ST-PROD-CNTL              VALUE ' '.
00243                  88  PI-MP-CARR-GRP-ST-PROD-CNTL     VALUE '1'.
00244                  88  PI-MP-CARR-ST-PROD-CNTL         VALUE '2'.
00245                  88  PI-MP-PROD-CNTL                 VALUE '3'.
00246                  88  PI-MP-CARR-PROD-CNTL            VALUE '4'.
00247              20  PI-MP-MONTH-END-DT          PIC XX.
00248              20  PI-MP-REFERENCE-NO.
00249                  24  PI-MP-REFERENCE-PRIME   PIC X(18).
00250                  24  PI-MP-REFERENCE-SFX     PIC XX.
00251
00252          16  PI-LABEL-CONTROL            PIC X(01).
00253              88  PI-CREATE-LABELS                    VALUE 'Y'.
00254              88  PI-BYPASS-LABELS                    VALUE 'N'.
00255
00256          16  PI-BILL-GROUPING-CODE       PIC X(01).
00257              88  PI-CO-HAS-BILL-GROUPING             VALUE 'Y'.
00258
00259          16  PI-RATE-DEV-AUTHORIZATION   PIC X(01).
00260              88  PI-RATE-DEV-AUTHORIZED              VALUE 'Y'.
00261              88  PI-RATE-DEV-NOT-AUTHORIZED          VALUE 'N'.
00262
00263          16  FILLER                      PIC X(14).
00264
00265      12  PI-PROGRAM-WORK-AREA            PIC X(640).
00266 ******************************************************************
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
052918*                                copy ERCACNT.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCACNT.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = NOTE FILE FOR RECORDING OF ACCOUNT NOTES  *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 120   RECFORM = FIXED                          *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ERACNT             RKP=2,LEN=23          *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *                                                                *
00016 *   LOG = YES                                                    *
00017 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00018 ******************************************************************
110706*                   C H A N G E   L O G
110706*
110706* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
110706*-----------------------------------------------------------------
110706*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
110706* EFFECTIVE    NUMBER
110706*-----------------------------------------------------------------
110706* 110706  CR2006071700004  PEMA  ADD BRANCH LOCATIONS
110706*           AND SHIPPING ADDRESS TO ACCOUNT NOTES FILE
110706******************************************************************
00019  01  NOTE-FILE.
00020      12  NT-FILE-ID                  PIC XX.
00021          88  VALID-NOTE-ID              VALUE 'NT'.
00022
00023      12  NT-CONTROL-PRIMARY.
00024          16  NT-COMPANY-CD           PIC X.
00027          16  NT-ACCT-NOTE-KEY.
00028              18  NT-CARRIER              PIC X.
00029              18  NT-GROUPING             PIC X(06).
00030              18  NT-STATE                PIC XX.
00031              18  NT-ACCOUNT              PIC X(10).
00025          16  NT-RECORD-TYPE          PIC X.
00026               88  ACCT-NOTE          VALUE '1'.
110706              88  ACCT-BRANCH-LOC    VALUE '2'.
110706              88  ACCT-SHIPPING-ADDR VALUE '3'.
00032          16  NT-LINE-SEQUENCE        PIC S9(4)     COMP.
00033
00034      12  NT-LAST-MAINT-DT            PIC XX.
00035      12  NT-LAST-MAINT-BY            PIC X(4).
00036      12  NT-LAST-MAINT-HHMMSS        PIC S9(7) COMP-3.
00037
110706*  ALL NOTE LINES ARE RECORD TYPE '1' WITH ALMOST UNLIMITED
110706*     SEQUENCE NUMBERS
110706     12  NT-NOTE-INFORMATION.
110706         16  NT-NOTE-LINE            PIC X(60).
00040          16  FILLER                  PIC X(25).
110706*  BOTH BRANCH LOCATION LINES ARE RECORD TYPE '2' SEQ 1 AND 2
110706     12  NT-LOCATION-INFORMATION REDEFINES
110706                         NT-NOTE-INFORMATION.
110706         16  NT-BRANCH-LOC-LINE      PIC X(60).
110706         16  FILLER                  PIC X(25).
052918* Account special indicator is record type '2', sequence 3
052918     12  filler REDEFINES NT-NOTE-INFORMATION.
052918         16  nt-account-special      PIC X.
052918         16  FILLER                  PIC X(84).
110706*  ALL SHIPPING ADDRESS LINES ARE RECORD TYPE '3'AND
      *     SEQUENCE NUMBER 1 IS NAME LINE 1
      *     SEQUENCE NUMBER 2 IS NAME LINE 2
      *     SEQUENCE NUMBER 3 IS ADDR LINE 1
      *     SEQUENCE NUMBER 4 IS ADDR LINE 2
      *     SEQUENCE NUMBER 5 IS ADDR LINE 3
      *     SEQUENCE NUMBER 6 IS CITY, ST AND ZIP
110706     12  NT-SHIPPING-INFORMATION REDEFINES
110706                         NT-NOTE-INFORMATION.
               16  NT-SHIPPING-LINE        PIC X(60).
110706         16  NT-SHIP-STATE           PIC XX.
110706         16  NT-SHIP-ZIP             PIC X(10).
110706         16  FILLER                  PIC X(13).
00041 *****************************************************************
00257 *                                    COPY ELCDATE.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCDATE.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003
00006 *                                                                *
00007 *                                                                *
00008 *   DESCRIPTION:  DATA PASSED TO DATE CONVERSION ROUTINE.        *
00009 *                 LENGTH = 200                                   *
00010 ******************************************************************
00011
00012  01  DATE-CONVERSION-DATA.
00013      12  DC-COMM-LENGTH                PIC S9(4) COMP VALUE +200.
00014      12  DC-OPTION-CODE                PIC X.
00015          88  BIN-TO-GREG                VALUE ' '.
00016          88  ELAPSED-BETWEEN-BIN        VALUE '1'.
00017          88  EDIT-GREG-TO-BIN           VALUE '2'.
00018          88  YMD-GREG-TO-BIN            VALUE '3'.
00019          88  MDY-GREG-TO-BIN            VALUE '4'.
00020          88  JULIAN-TO-BIN              VALUE '5'.
00021          88  BIN-PLUS-ELAPSED           VALUE '6'.
00022          88  FIND-CENTURY               VALUE '7'.
00023          88  ELAPSED-BETWEEN-BIN-3      VALUE '8'.
00024          88  EDIT-GREG-TO-BIN-3         VALUE '9'.
00025          88  YMD-GREG-TO-BIN-3          VALUE 'A'.
00026          88  MDY-GREG-TO-BIN-3          VALUE 'B'.
00027          88  JULIAN-TO-BIN-3            VALUE 'C'.
00028          88  BIN-PLUS-ELAPSED-3         VALUE 'D'.
00029          88  JULIAN-EXPANDED-TO-BIN     VALUE 'E'.
00030          88  JULIAN-EXPANDED-TO-BIN-3   VALUE 'F'.
00031          88  BIN-TO-JULIAN-EXPANDED     VALUE 'G'.
00032          88  JULIAN-EXPANDED            VALUE 'E', 'F', 'G'.
00033          88  CHECK-LEAP-YEAR            VALUE 'H'.
00034          88  BIN-3-TO-GREG              VALUE 'I'.
00035          88  CYMD-GREG-TO-BIN-3         VALUE 'J'.
00036          88  MDCY-GREG-TO-BIN-3         VALUE 'K'.
00037          88  CYMD-GREG-TO-BIN           VALUE 'L'.
00038          88  MDCY-GREG-TO-BIN           VALUE 'M'.
00039          88  MDY-GREG-TO-JULIAN         VALUE 'N'.
00040          88  MDCY-GREG-TO-JULIAN        VALUE 'O'.
00041          88  YMD-GREG-TO-JULIAN         VALUE 'P'.
00042          88  CYMD-GREG-TO-JULIAN        VALUE 'Q'.
00043          88  THREE-CHARACTER-BIN
00044                   VALUES  '8' '9' 'A' 'B' 'C' 'D' 'I' 'J' 'K'.
00045          88  GREGORIAN-TO-BIN
00046                   VALUES '2' '3' '4' '9' 'A' 'B' 'J' 'K' 'L' 'M'.
00047          88  BIN-TO-GREGORIAN
00048                   VALUES ' ' '1' 'I' '8' 'G'.
00049          88  JULIAN-TO-BINARY
00050                   VALUES '5' 'C' 'E' 'F'.
00051      12  DC-ERROR-CODE                 PIC X.
00052          88  NO-CONVERSION-ERROR        VALUE ' '.
00053          88  DATE-CONVERSION-ERROR
00054                   VALUES '1' '2' '3' '4' '5' '9' 'A' 'B' 'C'.
00055          88  DATE-IS-ZERO               VALUE '1'.
00056          88  DATE-IS-NON-NUMERIC        VALUE '2'.
00057          88  DATE-IS-INVALID            VALUE '3'.
00058          88  DATE1-GREATER-DATE2        VALUE '4'.
00059          88  ELAPSED-PLUS-NEGATIVE      VALUE '5'.
00060          88  DATE-INVALID-OPTION        VALUE '9'.
00061          88  INVALID-CENTURY            VALUE 'A'.
00062          88  ONLY-CENTURY               VALUE 'B'.
00063          88  ONLY-LEAP-YEAR             VALUE 'C'.
00064          88  VALID-CENTURY-LEAP-YEAR    VALUE 'B', 'C'.
00065      12  DC-END-OF-MONTH               PIC X.
00066          88  CALCULATE-END-OF-MONTH     VALUE '1'.
00067      12  DC-CENTURY-ADJUSTMENT         PIC X   VALUE SPACES.
00068          88  USE-NORMAL-PROCESS         VALUE ' '.
00069          88  ADJUST-DOWN-100-YRS        VALUE '1'.
00070          88  ADJUST-UP-100-YRS          VALUE '2'.
00071      12  FILLER                        PIC X.
00072      12  DC-CONVERSION-DATES.
00073          16  DC-BIN-DATE-1             PIC XX.
00074          16  DC-BIN-DATE-2             PIC XX.
00075          16  DC-GREG-DATE-1-EDIT       PIC X(08).
00076          16  DC-GREG-DATE-1-EDIT-R REDEFINES
00077                        DC-GREG-DATE-1-EDIT.
00078              20  DC-EDIT1-MONTH        PIC 99.
00079              20  SLASH1-1              PIC X.
00080              20  DC-EDIT1-DAY          PIC 99.
00081              20  SLASH1-2              PIC X.
00082              20  DC-EDIT1-YEAR         PIC 99.
00083          16  DC-GREG-DATE-2-EDIT       PIC X(08).
00084          16  DC-GREG-DATE-2-EDIT-R REDEFINES
00085                      DC-GREG-DATE-2-EDIT.
00086              20  DC-EDIT2-MONTH        PIC 99.
00087              20  SLASH2-1              PIC X.
00088              20  DC-EDIT2-DAY          PIC 99.
00089              20  SLASH2-2              PIC X.
00090              20  DC-EDIT2-YEAR         PIC 99.
00091          16  DC-GREG-DATE-1-YMD        PIC 9(06).
00092          16  DC-GREG-DATE-1-YMD-R  REDEFINES
00093                      DC-GREG-DATE-1-YMD.
00094              20  DC-YMD-YEAR           PIC 99.
00095              20  DC-YMD-MONTH          PIC 99.
00096              20  DC-YMD-DAY            PIC 99.
00097          16  DC-GREG-DATE-1-MDY        PIC 9(06).
00098          16  DC-GREG-DATE-1-MDY-R REDEFINES
00099                       DC-GREG-DATE-1-MDY.
00100              20  DC-MDY-MONTH          PIC 99.
00101              20  DC-MDY-DAY            PIC 99.
00102              20  DC-MDY-YEAR           PIC 99.
00103          16  DC-GREG-DATE-1-ALPHA.
00104              20  DC-ALPHA-MONTH        PIC X(10).
00105              20  DC-ALPHA-DAY          PIC 99.
00106              20  FILLER                PIC XX.
00107              20  DC-ALPHA-CENTURY.
00108                  24 DC-ALPHA-CEN-N     PIC 99.
00109              20  DC-ALPHA-YEAR         PIC 99.
00110          16  DC-ELAPSED-MONTHS         PIC S9(4)     COMP.
00111          16  DC-ODD-DAYS-OVER          PIC S9(4)     COMP.
00112          16  DC-ELAPSED-DAYS           PIC S9(4)     COMP.
00113          16  DC-JULIAN-DATE            PIC 9(05).
00114          16  DC-JULIAN-YYDDD REDEFINES DC-JULIAN-DATE
00115                                        PIC 9(05).
00116          16  DC-JULIAN-DT REDEFINES DC-JULIAN-DATE.
00117              20  DC-JULIAN-YEAR        PIC 99.
00118              20  DC-JULIAN-DAYS        PIC 999.
00119          16  DC-DAYS-IN-MONTH          PIC S9(3)       COMP-3.
00120          16  DC-DAY-OF-WEEK            PIC S9  VALUE ZERO COMP-3.
00121          16  DC-DAY-OF-WEEK2           PIC S9  VALUE ZERO COMP-3.
00122      12  DATE-CONVERSION-VARIBLES.
00123          16  HOLD-CENTURY-1            PIC 9(11) VALUE 0.
00124          16  HOLD-CENTURY-1-SPLIT REDEFINES HOLD-CENTURY-1.
00125              20  FILLER                PIC 9(3).
00126              20  HOLD-CEN-1-CCYY.
00127                  24  HOLD-CEN-1-CC     PIC 99.
00128                  24  HOLD-CEN-1-YY     PIC 99.
00129              20  HOLD-CEN-1-MO         PIC 99.
00130              20  HOLD-CEN-1-DA         PIC 99.
00131          16  HOLD-CENTURY-1-R   REDEFINES HOLD-CENTURY-1.
00132              20  HOLD-CEN-1-R-MO       PIC 99.
00133              20  HOLD-CEN-1-R-DA       PIC 99.
00134              20  HOLD-CEN-1-R-CCYY.
00135                  24  HOLD-CEN-1-R-CC   PIC 99.
00136                  24  HOLD-CEN-1-R-YY   PIC 99.
00137              20  FILLER                PIC 9(3).
00138          16  HOLD-CENTURY-1-X.
00139              20  FILLER                PIC X(3)  VALUE SPACES.
00140              20  HOLD-CEN-1-X-CCYY.
00141                  24  HOLD-CEN-1-X-CC   PIC XX VALUE SPACES.
00142                  24  HOLD-CEN-1-X-YY   PIC XX VALUE SPACES.
00143              20  HOLD-CEN-1-X-MO       PIC XX VALUE SPACES.
00144              20  HOLD-CEN-1-X-DA       PIC XX VALUE SPACES.
00145          16  HOLD-CENTURY-1-R-X REDEFINES HOLD-CENTURY-1-X.
00146              20  HOLD-CEN-1-R-X-MO     PIC XX.
00147              20  HOLD-CEN-1-R-X-DA     PIC XX.
00148              20  HOLD-CEN-1-R-X-CCYY.
00149                  24  HOLD-CEN-1-R-X-CC PIC XX.
00150                  24  HOLD-CEN-1-R-X-YY PIC XX.
00151              20  FILLER                PIC XXX.
00152          16  DC-BIN-DATE-EXPAND-1      PIC XXX.
00153          16  DC-BIN-DATE-EXPAND-2      PIC XXX.
00154          16  DC-JULIAN-DATE-1          PIC 9(07).
00155          16  DC-JULIAN-DATE-1-R REDEFINES DC-JULIAN-DATE-1.
00156              20  DC-JULIAN-1-CCYY.
00157                  24  DC-JULIAN-1-CC    PIC 99.
00158                  24  DC-JULIAN-1-YR    PIC 99.
00159              20  DC-JULIAN-DA-1        PIC 999.
00160          16  DC-JULIAN-DATE-2          PIC 9(07).
00161          16  DC-JULIAN-DATE-2-R REDEFINES DC-JULIAN-DATE-2.
00162              20  DC-JULIAN-2-CCYY.
00163                  24  DC-JULIAN-2-CC    PIC 99.
00164                  24  DC-JULIAN-2-YR    PIC 99.
00165              20  DC-JULIAN-DA-2        PIC 999.
00166          16  DC-GREG-DATE-A-EDIT.
00167              20  DC-EDITA-MONTH        PIC 99.
00168              20  SLASHA-1              PIC X VALUE '/'.
00169              20  DC-EDITA-DAY          PIC 99.
00170              20  SLASHA-2              PIC X VALUE '/'.
00171              20  DC-EDITA-CCYY.
00172                  24  DC-EDITA-CENT     PIC 99.
00173                  24  DC-EDITA-YEAR     PIC 99.
00174          16  DC-GREG-DATE-B-EDIT.
00175              20  DC-EDITB-MONTH        PIC 99.
00176              20  SLASHB-1              PIC X VALUE '/'.
00177              20  DC-EDITB-DAY          PIC 99.
00178              20  SLASHB-2              PIC X VALUE '/'.
00179              20  DC-EDITB-CCYY.
00180                  24  DC-EDITB-CENT     PIC 99.
00181                  24  DC-EDITB-YEAR     PIC 99.
00182          16  DC-GREG-DATE-CYMD         PIC 9(08).
00183          16  DC-GREG-DATE-CYMD-R REDEFINES
00184                               DC-GREG-DATE-CYMD.
00185              20  DC-CYMD-CEN           PIC 99.
00186              20  DC-CYMD-YEAR          PIC 99.
00187              20  DC-CYMD-MONTH         PIC 99.
00188              20  DC-CYMD-DAY           PIC 99.
00189          16  DC-GREG-DATE-MDCY         PIC 9(08).
00190          16  DC-GREG-DATE-MDCY-R REDEFINES
00191                               DC-GREG-DATE-MDCY.
00192              20  DC-MDCY-MONTH         PIC 99.
00193              20  DC-MDCY-DAY           PIC 99.
00194              20  DC-MDCY-CEN           PIC 99.
00195              20  DC-MDCY-YEAR          PIC 99.
CIDMOD    12  DC-FORCE-EL310-DATE-SW         PIC X    VALUE SPACE.
CIDMOD        88  DC-FORCE-EL310-DATE                 VALUE 'Y'.
CIDMOD    12  DC-EL310-DATE                  PIC X(21).
CIDMOD    12  FILLER                         PIC X(28).
00259 *                                    COPY EL1273S.
       01  EL127CI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  CDATEL PIC S9(0004) COMP.
           05  CDATEF PIC  X(0001).
           05  FILLER REDEFINES CDATEF.
               10  CDATEA PIC  X(0001).
           05  CDATEI PIC  X(0008).
      *    -------------------------------
           05  CTIMEL PIC S9(0004) COMP.
           05  CTIMEF PIC  X(0001).
           05  FILLER REDEFINES CTIMEF.
               10  CTIMEA PIC  X(0001).
           05  CTIMEI PIC  X(0005).
      *    -------------------------------
           05  SYSENVL PIC S9(0004) COMP.
           05  SYSENVF PIC  X(0001).
           05  FILLER REDEFINES SYSENVF.
               10  SYSENVA PIC  X(0001).
           05  SYSENVI PIC  X(0006).
      *    -------------------------------
           05  CMPNYIDL PIC S9(0004) COMP.
           05  CMPNYIDF PIC  X(0001).
           05  FILLER REDEFINES CMPNYIDF.
               10  CMPNYIDA PIC  X(0001).
           05  CMPNYIDI PIC  X(0003).
      *    -------------------------------
           05  USERIDL PIC S9(0004) COMP.
           05  USERIDF PIC  X(0001).
           05  FILLER REDEFINES USERIDF.
               10  USERIDA PIC  X(0001).
           05  USERIDI PIC  X(0004).
      *    -------------------------------
           05  CMEMCAPL PIC S9(0004) COMP.
           05  CMEMCAPF PIC  X(0001).
           05  FILLER REDEFINES CMEMCAPF.
               10  CMEMCAPA PIC  X(0001).
           05  CMEMCAPI PIC  X(0010).
      *    -------------------------------
           05  CASRISKL PIC S9(0004) COMP.
           05  CASRISKF PIC  X(0001).
           05  FILLER REDEFINES CASRISKF.
               10  CASRISKA PIC  X(0001).
           05  CASRISKI PIC  X(0001).
      *    -------------------------------
           05  CCERTNOL PIC S9(0004) COMP.
           05  CCERTNOF PIC  X(0001).
           05  FILLER REDEFINES CCERTNOF.
               10  CCERTNOA PIC  X(0001).
           05  CCERTNOI PIC  X(0010).
      *    -------------------------------
           05  CCRTSFXL PIC S9(0004) COMP.
           05  CCRTSFXF PIC  X(0001).
           05  FILLER REDEFINES CCRTSFXF.
               10  CCRTSFXA PIC  X(0001).
           05  CCRTSFXI PIC  X(0001).
      *    -------------------------------
           05  CACCTNOL PIC S9(0004) COMP.
           05  CACCTNOF PIC  X(0001).
           05  FILLER REDEFINES CACCTNOF.
               10  CACCTNOA PIC  X(0001).
           05  CACCTNOI PIC  X(0010).
      *    -------------------------------
           05  CSTATEL PIC S9(0004) COMP.
           05  CSTATEF PIC  X(0001).
           05  FILLER REDEFINES CSTATEF.
               10  CSTATEA PIC  X(0001).
           05  CSTATEI PIC  X(0002).
      *    -------------------------------
           05  CCARIERL PIC S9(0004) COMP.
           05  CCARIERF PIC  X(0001).
           05  FILLER REDEFINES CCARIERF.
               10  CCARIERA PIC  X(0001).
           05  CCARIERI PIC  X(0001).
      *    -------------------------------
           05  CGROUPL PIC S9(0004) COMP.
           05  CGROUPF PIC  X(0001).
           05  FILLER REDEFINES CGROUPF.
               10  CGROUPA PIC  X(0001).
           05  CGROUPI PIC  X(0006).
      *    -------------------------------
           05  CEFDATEL PIC S9(0004) COMP.
           05  CEFDATEF PIC  X(0001).
           05  FILLER REDEFINES CEFDATEF.
               10  CEFDATEA PIC  X(0001).
           05  CEFDATEI PIC  X(0008).
      *    -------------------------------
           05  CMEMNOL PIC S9(0004) COMP.
           05  CMEMNOF PIC  X(0001).
           05  FILLER REDEFINES CMEMNOF.
               10  CMEMNOA PIC  X(0001).
           05  CMEMNOI PIC  X(0012).
      *    -------------------------------
           05  AGTNAMEL PIC S9(0004) COMP.
           05  AGTNAMEF PIC  X(0001).
           05  FILLER REDEFINES AGTNAMEF.
               10  AGTNAMEA PIC  X(0001).
           05  AGTNAMEI PIC  X(0034).
      *    -------------------------------
           05  AGTLICNL PIC S9(0004) COMP.
           05  AGTLICNF PIC  X(0001).
           05  FILLER REDEFINES AGTLICNF.
               10  AGTLICNA PIC  X(0001).
           05  AGTLICNI PIC  X(0015).
      *    -------------------------------
           05  AGTNPNNL PIC S9(0004) COMP.
           05  AGTNPNNF PIC  X(0001).
           05  FILLER REDEFINES AGTNPNNF.
               10  AGTNPNNA PIC  X(0001).
           05  AGTNPNNI PIC  X(0010).
      *    -------------------------------
           05  CLNAMEL PIC S9(0004) COMP.
           05  CLNAMEF PIC  X(0001).
           05  FILLER REDEFINES CLNAMEF.
               10  CLNAMEA PIC  X(0001).
           05  CLNAMEI PIC  X(0015).
      *    -------------------------------
           05  CFNAMEL PIC S9(0004) COMP.
           05  CFNAMEF PIC  X(0001).
           05  FILLER REDEFINES CFNAMEF.
               10  CFNAMEA PIC  X(0001).
           05  CFNAMEI PIC  X(0010).
      *    -------------------------------
           05  CINITL PIC S9(0004) COMP.
           05  CINITF PIC  X(0001).
           05  FILLER REDEFINES CINITF.
               10  CINITA PIC  X(0001).
           05  CINITI PIC  X(0001).
      *    -------------------------------
           05  CAGEL PIC S9(0004) COMP.
           05  CAGEF PIC  X(0001).
           05  FILLER REDEFINES CAGEF.
               10  CAGEA PIC  X(0001).
           05  CAGEI PIC  X(0002).
      *    -------------------------------
           05  CAGEDEFL PIC S9(0004) COMP.
           05  CAGEDEFF PIC  X(0001).
           05  FILLER REDEFINES CAGEDEFF.
               10  CAGEDEFA PIC  X(0001).
           05  CAGEDEFI PIC  X(0001).
      *    -------------------------------
           05  CSEXL PIC S9(0004) COMP.
           05  CSEXF PIC  X(0001).
           05  FILLER REDEFINES CSEXF.
               10  CSEXA PIC  X(0001).
           05  CSEXI PIC  X(0001).
      *    -------------------------------
           05  CSSNL PIC S9(0004) COMP.
           05  CSSNF PIC  X(0001).
           05  FILLER REDEFINES CSSNF.
               10  CSSNA PIC  X(0001).
           05  CSSNI PIC  X(0012).
      *    -------------------------------
           05  CJLNAMEL PIC S9(0004) COMP.
           05  CJLNAMEF PIC  X(0001).
           05  FILLER REDEFINES CJLNAMEF.
               10  CJLNAMEA PIC  X(0001).
           05  CJLNAMEI PIC  X(0015).
      *    -------------------------------
           05  CJFNAMEL PIC S9(0004) COMP.
           05  CJFNAMEF PIC  X(0001).
           05  FILLER REDEFINES CJFNAMEF.
               10  CJFNAMEA PIC  X(0001).
           05  CJFNAMEI PIC  X(0010).
      *    -------------------------------
           05  CJINITL PIC S9(0004) COMP.
           05  CJINITF PIC  X(0001).
           05  FILLER REDEFINES CJINITF.
               10  CJINITA PIC  X(0001).
           05  CJINITI PIC  X(0001).
      *    -------------------------------
           05  CJAGEL PIC S9(0004) COMP.
           05  CJAGEF PIC  X(0001).
           05  FILLER REDEFINES CJAGEF.
               10  CJAGEA PIC  X(0001).
           05  CJAGEI PIC  X(0002).
      *    -------------------------------
           05  CJAGEDFL PIC S9(0004) COMP.
           05  CJAGEDFF PIC  X(0001).
           05  FILLER REDEFINES CJAGEDFF.
               10  CJAGEDFA PIC  X(0001).
           05  CJAGEDFI PIC  X(0001).
      *    -------------------------------
           05  CBNAMEL PIC S9(0004) COMP.
           05  CBNAMEF PIC  X(0001).
           05  FILLER REDEFINES CBNAMEF.
               10  CBNAMEA PIC  X(0001).
           05  CBNAMEI PIC  X(0025).
      *    -------------------------------
           05  CCLMHSL PIC S9(0004) COMP.
           05  CCLMHSF PIC  X(0001).
           05  FILLER REDEFINES CCLMHSF.
               10  CCLMHSA PIC  X(0001).
           05  CCLMHSI PIC  X(0002).
      *    -------------------------------
           05  CVINL PIC S9(0004) COMP.
           05  CVINF PIC  X(0001).
           05  FILLER REDEFINES CVINF.
               10  CVINA PIC  X(0001).
           05  CVINI PIC  X(0017).
      *    -------------------------------
           05  CSPCINSL PIC S9(0004) COMP.
           05  CSPCINSF PIC  X(0001).
           05  FILLER REDEFINES CSPCINSF.
               10  CSPCINSA PIC  X(0001).
           05  CSPCINSI PIC  X(0003).
      *    -------------------------------
           05  LOANNOL PIC S9(0004) COMP.
           05  LOANNOF PIC  X(0001).
           05  FILLER REDEFINES LOANNOF.
               10  LOANNOA PIC  X(0001).
           05  LOANNOI PIC  X(0008).
      *    -------------------------------
           05  LOANBALL PIC S9(0004) COMP.
           05  LOANBALF PIC  X(0001).
           05  FILLER REDEFINES LOANBALF.
               10  LOANBALA PIC  X(0001).
           05  LOANBALI PIC  9(09)V99.
      *    -------------------------------
           05  LNOFCL PIC S9(0004) COMP.
           05  LNOFCF PIC  X(0001).
           05  FILLER REDEFINES LNOFCF.
               10  LNOFCA PIC  X(0001).
           05  LNOFCI PIC  X(0005).
      *    -------------------------------
           05  CAPRL PIC S9(0004) COMP.
           05  CAPRF PIC  X(0001).
           05  FILLER REDEFINES CAPRF.
               10  CAPRA PIC  X(0001).
           05  CAPRI PIC  9(4)V9(4).
      *    -------------------------------
           05  CFORMNOL PIC S9(0004) COMP.
           05  CFORMNOF PIC  X(0001).
           05  FILLER REDEFINES CFORMNOF.
               10  CFORMNOA PIC  X(0001).
           05  CFORMNOI PIC  X(0012).
      *    -------------------------------
           05  CUSERCDL PIC S9(0004) COMP.
           05  CUSERCDF PIC  X(0001).
           05  FILLER REDEFINES CUSERCDF.
               10  CUSERCDA PIC  X(0001).
           05  CUSERCDI PIC  X(0003).
      *    -------------------------------
           05  CINDGRPL PIC S9(0004) COMP.
           05  CINDGRPF PIC  X(0001).
           05  FILLER REDEFINES CINDGRPF.
               10  CINDGRPA PIC  X(0001).
           05  CINDGRPI PIC  X(0001).
      *    -------------------------------
           05  CPREMTPL PIC S9(0004) COMP.
           05  CPREMTPF PIC  X(0001).
           05  FILLER REDEFINES CPREMTPF.
               10  CPREMTPA PIC  X(0001).
           05  CPREMTPI PIC  X(0001).
      *    -------------------------------
           05  CPTDESCL PIC S9(0004) COMP.
           05  CPTDESCF PIC  X(0001).
           05  FILLER REDEFINES CPTDESCF.
               10  CPTDESCA PIC  X(0001).
           05  CPTDESCI PIC  X(0002).
      *    -------------------------------
           05  CLIVESL PIC S9(0004) COMP.
           05  CLIVESF PIC  X(0001).
           05  FILLER REDEFINES CLIVESF.
               10  CLIVESA PIC  X(0001).
           05  CLIVESI PIC  9(7).
      *    -------------------------------
           05  CISSMICL PIC S9(0004) COMP.
           05  CISSMICF PIC  X(0001).
           05  FILLER REDEFINES CISSMICF.
               10  CISSMICA PIC  X(0001).
           05  CISSMICI PIC  9(9).
      *    -------------------------------
           05  CNHINTL PIC S9(0004) COMP.
           05  CNHINTF PIC  X(0001).
           05  FILLER REDEFINES CNHINTF.
               10  CNHINTA PIC  X(0001).
           05  CNHINTI PIC  9(7)V99.
      *    -------------------------------
           05  CNOTESL PIC S9(0004) COMP.
           05  CNOTESF PIC  X(0001).
           05  FILLER REDEFINES CNOTESF.
               10  CNOTESA PIC  X(0001).
           05  CNOTESI PIC  X(0003).
      *    -------------------------------
           05  RPTCD1L PIC S9(0004) COMP.
           05  RPTCD1F PIC  X(0001).
           05  FILLER REDEFINES RPTCD1F.
               10  RPTCD1A PIC  X(0001).
           05  RPTCD1I PIC  X(0010).
      *    -------------------------------
           05  RPTCD2L PIC S9(0004) COMP.
           05  RPTCD2F PIC  X(0001).
           05  FILLER REDEFINES RPTCD2F.
               10  RPTCD2A PIC  X(0001).
           05  RPTCD2I PIC  X(0010).
      *    -------------------------------
           05  CCSRCDL PIC S9(0004) COMP.
           05  CCSRCDF PIC  X(0001).
           05  FILLER REDEFINES CCSRCDF.
               10  CCSRCDA PIC  X(0001).
           05  CCSRCDI PIC  X(0003).
      *    -------------------------------
           05  RPTCD3L PIC S9(0004) COMP.
           05  RPTCD3F PIC  X(0001).
           05  FILLER REDEFINES RPTCD3F.
               10  RPTCD3A PIC  X(0001).
           05  RPTCD3I PIC  X(0010).
      *    -------------------------------
           05  CCLMFLHL PIC S9(0004) COMP.
           05  CCLMFLHF PIC  X(0001).
           05  FILLER REDEFINES CCLMFLHF.
               10  CCLMFLHA PIC  X(0001).
           05  CCLMFLHI PIC  X(0013).
      *    -------------------------------
           05  CCLMFLGL PIC S9(0004) COMP.
           05  CCLMFLGF PIC  X(0001).
           05  FILLER REDEFINES CCLMFLGF.
               10  CCLMFLGA PIC  X(0001).
           05  CCLMFLGI PIC  X(0001).
      *    -------------------------------
           05  DENSTATL PIC S9(0004) COMP.
           05  DENSTATF PIC  X(0001).
           05  FILLER REDEFINES DENSTATF.
               10  DENSTATA PIC  X(0001).
           05  DENSTATI PIC  X(0027).
      *    -------------------------------
           05  CLKINDL PIC S9(0004) COMP.
           05  CLKINDF PIC  X(0001).
           05  FILLER REDEFINES CLKINDF.
               10  CLKINDA PIC  X(0001).
           05  CLKINDI PIC  X(0002).
      *    -------------------------------
           05  CLCDL PIC S9(0004) COMP.
           05  CLCDF PIC  X(0001).
           05  FILLER REDEFINES CLCDF.
               10  CLCDA PIC  X(0001).
           05  CLCDI PIC  X(0002).
      *    -------------------------------
           05  CLEDESCL PIC S9(0004) COMP.
           05  CLEDESCF PIC  X(0001).
           05  FILLER REDEFINES CLEDESCF.
               10  CLEDESCA PIC  X(0001).
           05  CLEDESCI PIC  X(0003).
      *    -------------------------------
           05  CLDESCL PIC S9(0004) COMP.
           05  CLDESCF PIC  X(0001).
           05  FILLER REDEFINES CLDESCF.
               10  CLDESCA PIC  X(0001).
           05  CLDESCI PIC  X(0010).
      *    -------------------------------
           05  CLTERML PIC S9(0004) COMP.
           05  CLTERMF PIC  X(0001).
           05  FILLER REDEFINES CLTERMF.
               10  CLTERMA PIC  X(0001).
           05  CLTERMI PIC  9(3).
      *    -------------------------------
           05  CLREML PIC S9(0004) COMP.
           05  CLREMF PIC  X(0001).
           05  FILLER REDEFINES CLREMF.
               10  CLREMA PIC  X(0001).
           05  CLREMI PIC  X(0003).
      *    -------------------------------
           05  CLPREML PIC S9(0004) COMP.
           05  CLPREMF PIC  X(0001).
           05  FILLER REDEFINES CLPREMF.
               10  CLPREMA PIC  X(0001).
           05  CLPREMI PIC  9(09)V99.
      *    -------------------------------
           05  CLBENL PIC S9(0004) COMP.
           05  CLBENF PIC  X(0001).
           05  FILLER REDEFINES CLBENF.
               10  CLBENA PIC  X(0001).
           05  CLBENI PIC  9(12)V99.
      *    -------------------------------
           05  CLCANCLL PIC S9(0004) COMP.
           05  CLCANCLF PIC  X(0001).
           05  FILLER REDEFINES CLCANCLF.
               10  CLCANCLA PIC  X(0001).
           05  CLCANCLI PIC  X(0008).
      *    -------------------------------
           05  CLSTATL PIC S9(0004) COMP.
           05  CLSTATF PIC  X(0001).
           05  FILLER REDEFINES CLSTATF.
               10  CLSTATA PIC  X(0001).
           05  CLSTATI PIC  X(0012).
      *    -------------------------------
           05  CAKINDL PIC S9(0004) COMP.
           05  CAKINDF PIC  X(0001).
           05  FILLER REDEFINES CAKINDF.
               10  CAKINDA PIC  X(0001).
           05  CAKINDI PIC  X(0002).
      *    -------------------------------
           05  CACDL PIC S9(0004) COMP.
           05  CACDF PIC  X(0001).
           05  FILLER REDEFINES CACDF.
               10  CACDA PIC  X(0001).
           05  CACDI PIC  X(0002).
      *    -------------------------------
           05  CAEDESCL PIC S9(0004) COMP.
           05  CAEDESCF PIC  X(0001).
           05  FILLER REDEFINES CAEDESCF.
               10  CAEDESCA PIC  X(0001).
           05  CAEDESCI PIC  X(0003).
      *    -------------------------------
           05  CADESCL PIC S9(0004) COMP.
           05  CADESCF PIC  X(0001).
           05  FILLER REDEFINES CADESCF.
               10  CADESCA PIC  X(0001).
           05  CADESCI PIC  X(0010).
      *    -------------------------------
           05  CATERML PIC S9(0004) COMP.
           05  CATERMF PIC  X(0001).
           05  FILLER REDEFINES CATERMF.
               10  CATERMA PIC  X(0001).
           05  CATERMI PIC  9(3).
      *    -------------------------------
           05  CAREML PIC S9(0004) COMP.
           05  CAREMF PIC  X(0001).
           05  FILLER REDEFINES CAREMF.
               10  CAREMA PIC  X(0001).
           05  CAREMI PIC  X(0003).
      *    -------------------------------
           05  CAPREML PIC S9(0004) COMP.
           05  CAPREMF PIC  X(0001).
           05  FILLER REDEFINES CAPREMF.
               10  CAPREMA PIC  X(0001).
           05  CAPREMI PIC  9(09)V99.
      *    -------------------------------
           05  CABENL PIC S9(0004) COMP.
           05  CABENF PIC  X(0001).
           05  FILLER REDEFINES CABENF.
               10  CABENA PIC  X(0001).
           05  CABENI PIC  9(12)V99.
      *    -------------------------------
           05  CACANCLL PIC S9(0004) COMP.
           05  CACANCLF PIC  X(0001).
           05  FILLER REDEFINES CACANCLF.
               10  CACANCLA PIC  X(0001).
           05  CACANCLI PIC  X(0008).
      *    -------------------------------
           05  CASTATL PIC S9(0004) COMP.
           05  CASTATF PIC  X(0001).
           05  FILLER REDEFINES CASTATF.
               10  CASTATA PIC  X(0001).
           05  CASTATI PIC  X(0012).
      *    -------------------------------
           05  CNOTE1L PIC S9(0004) COMP.
           05  CNOTE1F PIC  X(0001).
           05  FILLER REDEFINES CNOTE1F.
               10  CNOTE1A PIC  X(0001).
           05  CNOTE1I PIC  X(0079).
      *    -------------------------------
           05  CNOTE2L PIC S9(0004) COMP.
           05  CNOTE2F PIC  X(0001).
           05  FILLER REDEFINES CNOTE2F.
               10  CNOTE2A PIC  X(0001).
           05  CNOTE2I PIC  X(0039).
      *    -------------------------------
           05  CNOTE3L PIC S9(0004) COMP.
           05  CNOTE3F PIC  X(0001).
           05  FILLER REDEFINES CNOTE3F.
               10  CNOTE3A PIC  X(0001).
           05  CNOTE3I PIC  X(0038).
      *    -------------------------------
           05  CEMSG1L PIC S9(0004) COMP.
           05  CEMSG1F PIC  X(0001).
           05  FILLER REDEFINES CEMSG1F.
               10  CEMSG1A PIC  X(0001).
           05  CEMSG1I PIC  X(0079).
      *    -------------------------------
           05  CEMSG2L PIC S9(0004) COMP.
           05  CEMSG2F PIC  X(0001).
           05  FILLER REDEFINES CEMSG2F.
               10  CEMSG2A PIC  X(0001).
           05  CEMSG2I PIC  99.
      *    -------------------------------
           05  PF3KEYL PIC S9(0004) COMP.
           05  PF3KEYF PIC  X(0001).
           05  FILLER REDEFINES PF3KEYF.
               10  PF3KEYA PIC  X(0001).
           05  PF3KEYI PIC  X(0009).
      *    -------------------------------
           05  PF5KEYL PIC S9(0004) COMP.
           05  PF5KEYF PIC  X(0001).
           05  FILLER REDEFINES PF5KEYF.
               10  PF5KEYA PIC  X(0001).
           05  PF5KEYI PIC  X(0007).
      *    -------------------------------
           05  PF7KEYL PIC S9(0004) COMP.
           05  PF7KEYF PIC  X(0001).
           05  FILLER REDEFINES PF7KEYF.
               10  PF7KEYA PIC  X(0001).
           05  PF7KEYI PIC  X(0025).
      *    -------------------------------
           05  PF4KEYL PIC S9(0004) COMP.
           05  PF4KEYF PIC  X(0001).
           05  FILLER REDEFINES PF4KEYF.
               10  PF4KEYA PIC  X(0001).
           05  PF4KEYI PIC  X(0007).
      *    -------------------------------
           05  PF6KEYL PIC S9(0004) COMP.
           05  PF6KEYF PIC  X(0001).
           05  FILLER REDEFINES PF6KEYF.
               10  PF6KEYA PIC  X(0001).
           05  PF6KEYI PIC  X(0006).
      *    -------------------------------
           05  PF8KEYL PIC S9(0004) COMP.
           05  PF8KEYF PIC  X(0001).
           05  FILLER REDEFINES PF8KEYF.
               10  PF8KEYA PIC  X(0001).
           05  PF8KEYI PIC  X(0025).
       01  EL127CO REDEFINES EL127CI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SYSENVO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMPNYIDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USERIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMEMCAPO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CASRISKO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCERTNOO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCRTSFXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CACCTNOO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSTATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCARIERO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CGROUPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CEFDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMEMNOO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGTNAMEO PIC  X(0034).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGTLICNO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGTNPNNO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLNAMEO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CFNAMEO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CINITO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAGEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAGEDEFO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSEXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSSNO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CJLNAMEO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CJFNAMEO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CJINITO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CJAGEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CJAGEDFO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CBNAMEO PIC  X(0025).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCLMHSO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CVINO PIC  X(0017).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSPCINSO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LOANNOO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LOANBALO PIC  ZZZZ,ZZZ.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LNOFCO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAPRO PIC  ZZ9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CFORMNOO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CUSERCDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CINDGRPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPREMTPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPTDESCO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLIVESO PIC  ZZZZZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CISSMICO PIC  999999999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CNHINTO PIC  ZZZZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CNOTESO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RPTCD1O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RPTCD2O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCSRCDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RPTCD3O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCLMFLHO PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCLMFLGO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DENSTATO PIC  X(0027).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLKINDO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLCDO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLEDESCO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLDESCO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLTERMO PIC  ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLREMO PIC  ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLPREMO PIC  ZZZ,ZZZ.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLBENO PIC  ZZZ,ZZZ,ZZZ.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLCANCLO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLSTATO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAKINDO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CACDO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAEDESCO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CADESCO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CATERMO PIC  ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAREMO PIC  ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAPREMO PIC  ZZZ,ZZZ.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CABENO PIC  ZZZ,ZZZ,ZZZ.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CACANCLO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CASTATO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CNOTE1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CNOTE2O PIC  X(0039).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CNOTE3O PIC  X(0038).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CEMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CEMSG2O PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PF3KEYO PIC  X(0009).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PF5KEYO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PF7KEYO PIC  X(0025).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PF4KEYO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PF6KEYO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PF8KEYO PIC  X(0025).
      *    -------------------------------
00261 *                                    COPY ELCJPFX.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCJPFX.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *    USER DATA FOR SYSTEM JOURNAL RECORDS  JOURNAL I.D. = "EL"   *
00008 *                                                                *
00009 *     ALL RECORDS ARE JOURNALED FOR ERROR RECOVERY               *
00010 *     FILES JOURNALED FOR AUDIT TRAIL (BEFORE CHANGE) ARE -      *
00011 *        ELCNTL - CONTROL FILE                                   *
00012 *        ELMSTR - CLAIM MASTERS                                  *
00013 *        ELTRLR - ACTIVITY TRAILERS                              *
00014 *        ELCHKQ - CHECK QUE                                      *
00015 ******************************************************************
00016  01  JOURNAL-RECORD.
           12  jp-date                     pic s9(5) comp-3.
           12  jp-time                     pic s9(7) comp-3.
00017      12  JP-USER-ID                  PIC X(4).
00018      12  JP-FILE-ID                  PIC X(8).
00019      12  JP-PROGRAM-ID               PIC X(8).
00020      12  JP-RECORD-TYPE              PIC X.
00021          88 JP-ADD              VALUE 'A'.
00022          88 JP-BEFORE-CHANGE    VALUE 'B'.
00023          88 JP-AFTER-CHANGE     VALUE 'C'.
00024          88 JP-DELETE           VALUE 'D'.
00025          88 JP-GENERIC-DELETE   VALUE 'G'.
00026          88 JP-KEY-CHG-DELETE   VALUE 'K'.
00027          88 JP-KEY-CHG-ADD      VALUE 'N'.
00028      12  JP-GENERIC-KEY-LENGTH       PIC S9(4)   COMP.
00029      12  JP-RECORD-AREA
00030
00031
00262                                      PIC X(450).
00263
00265 *                                    COPY ELCCALC.
00001 ******************************************************************
00002 *                                                                *
00003 *                           ELCCALC.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.025                          *
00006 *                                                                *
00007 *   DESCRIPTION:  DATA TO BE PASSED TO REMAINING TERM ROUTINE    *
00008 *                 REMAINING AMOUNT ROUTINE, LOSS RESERVE ROUTINE *
00009 *                 REFUND CALCULATIONS ROUTINE, EARNINGS CALCU -  *
00010 *                 LATIONS ROUTINE, AND THE RATING ROUTINE.       *
00011 *                                                                *
00012 *  PASSED TO ELRTRM                                              *
00013 *  -----------------                                             *
00014 *  METHOD CODE (I.E. FULL MONTH, HALF ADJ, ETC)                  *
00015 *  ORIGINAL TERM                                                 *
00016 *  BEGINNING DATE                                                *
00017 *  ENDING DATE                                                   *
00018 *  COMPANY I.D.                                                  *
00019 *  ACCOUNT MASTER USER FIELD                                     *
00020 *  PROCESS SWITCH (CANCEL, CLAIM)                                *
00021 *  FREE LOOK DAYS                                                *
00022 *                                                                *
00023 *  RETURNED FROM ELRTRM                                          *
00024 *  ---------------------                                         *
00025 *  REMAINING TERM 1 - USED FOR EARNINGS                          *
00026 *  REMAINING TERM 2 - USED FOR BENEFIT CALCULATIONS              *
00027 *  REMAINING TERM 3 - USED FOR CLAIM BENEFITS                    *
00028 *  ODD DAYS - REMAINING DAYS PAST FULL MONTHS                    *
00029 *----------------------------------------------------------------*
00030 *  PASSED TO ELRAMT                                              *
00031 *  ----------------                                              *
00032 *  REMAINING TERM 1 OR 2 OR 3 (FROM ELRTRM)                      *
00033 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00034 *  ORIGINAL AMOUNT                                               *
00035 *  ALTERNATE BENEFIT (BALLON)                                    *
00036 *  A.P.R. - NET PAY ONLY                                         *
00037 *  METHOD
00038 *  PAYMENT FREQUENCY - FOR FARM PLAN                             *
00039 *  COMPANY I.D.                                                  *
00040 *  BENEFIT TYPE                                                  *
00041 *                                                                *
00042 *  RETURNED FROM ELRAMT                                          *
00043 *  --------------------                                          *
00044 *  REMAINING AMOUNT 1 - CURRENT                                  *
00045 *  REMAINING AMOUNT 2 - PREVIOUS MONTH                           *
00046 *  REMAINING AMOUNT FACTOR
00047 *----------------------------------------------------------------*
00048 *  PASSED TO ELRESV                                              *
00049 *  -----------------                                             *
00050 *  CERTIFICATE EFFECTIVE DATE                                    *
00051 *  VALUATION DATE                                                *
00052 *  PAID THRU DATE                                                *
00053 *  BENEFIT                                                       *
00054 *  INCURRED DATE                                                 *
00055 *  REPORTED DATE                                                 *
00056 *  ISSUE AGE                                                     *
00057 *  TERM                                                          *
00058 *  CDT PERCENT                                                   *
00059 *  CDT METHOD (I.E. INTERPOLATED, AVERAGE, ETC)                  *
00060 * *CLAIM TYPE (LIFE, A/H)                                        *
00061 * *REMAINING BENEFIT (FROM ELRAMT)                               *
00062 * *ONLY FIELDS REQUIRED FOR LIFE CLAIMS                          *
00063 *                                                                *
00064 *  RETURNED FROM ELRESV                                          *
00065 *  --------------------                                          *
00066 *  CDT TABLE USED                                                *
00067 *  CDT FACTOR USED                                               *
00068 *  PAY TO CURRENT RESERVE                                        *
00069 *  I.B.N.R. - A/H ONLY                                           *
00070 *  FUTURE (ACCRUED) AH ONLY                                      *
00071 *----------------------------------------------------------------*
00072 *  PASSED TO ELRATE                                              *
00073 *  ----------------                                              *
00074 *  CERT ISSUE DATE                                               *
00075 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00076 *  TERM OR EXT DAYS  (DAY TERM FOR SP CALC = 'D', ELSE EXT DAYS) *
00077 *  CAPPED TERM   (ONLY FOR TRUNCATED LIFE)                       *
00078 *  STATE CODE (CLIENT DEFINED)                                   *
00079 *  STATE CODE (STANDARD P.O. ABBRV)                              *
00080 *  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
00081 *  DEVIATION CODE                                                *
00082 *  ISSUE AGE                                                     *
00083 *  ORIGINAL BENEFIT AMOUNT                                       *
00084 *  RATING BENEFIT AMT (TOTAL BENEFIT AMT FOR BALLOONS)           *
00085 *  PROCESS TYPE (ISSUE OR CANCEL)                                *
00086 *  BENEFIT KIND (LIFE OR A/H)                                    *
00087 *  A.P.R.                                                        *
00088 *  METHOD
00089 *  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
00090 *  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
00091 *  COMPANY I.D. (3 CHARACTER)                                    *
00092 *  BENEFIT CODE                                                  *
00093 *  BENEFIT OVERRIDE CODE                                         *
00094 *  MAXIMUM MONTHLY BENEFIT (FROM ACCT MASTER - CSL ONLY)         *
00095 *  MAXIMUM TOTAL BENEFIT (FROM ACCT MASTER - CSL ONLY)           *
00096 *  JOINT INDICATOR (CSL ONLY)                                    *
00097 *  FIRST PAYMENT DATE (CSL ONLY)                                 *
00098 *  PERIODIC PAYMENT AMOUNT (IN CP-REMAINING-TERM - CSL ONLY)     *
00099 *                                                                *
00100 *  RETURNED FROM ELRATE                                          *
00101 *  --------------------                                          *
00102 *  CALCULATED PREMIUM                                            *
00103 *  PREMIUM RATE                                                  *
00104 *  MORTALITY CODE                                                *
00105 *  MAX ATTAINED AGE                                              *
00106 *  MAX AGE                                                       *
00107 *  MAX TERM                                                      *
00108 *  MAX MONTHLY BENEFIT                                           *
00109 *  MAX TOTAL BENIFIT                                             *
00110 *  COMPOSITE RATE (OPEN-END ONLY)                                *
00111 *----------------------------------------------------------------*
00112 *  PASSED TO ELRFND                                              *
00113 *  ----------------                                              *
00114 *  CERT ISSUE DATE                                               *
00115 *  REFUND DATE                                                   *
00116 *  RULE OF 78 OPTION (FROM CONTROL RECORD)                       *
00117 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00118 *  TERM OR EXT DAYS  (DAY TERM FOR SP CALC = 'D', ELSE EXT DAYS) *
00119 *  REMAINING TERM (REMAINING TERM 1 FROM ELTERM)                 *
00120 *  STATE CODE (CLIENT DEFINED)                                   *
00121 *  STATE CODE (STANDARD P.O. ABBRV)                              *
00122 *  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
00123 *  DEVIATION CODE                                                *
00124 *  ISSUE AGE                                                     *
00125 *  ORIGINAL BENEFIT AMOUNT                                       *
00126 *  RATING BENEFIT AMT (TOTAL BENEFIT AMT FOR BALLOONS)           *
00127 *  PROCESS TYPE (CANCEL)                                         *
00128 *  BENEFIT KIND (LIFE OR A/H)                                    *
00129 *  A.P.R.                                                        *
00130 *  EARNING METHOD - (CODE FROM BENEFIT, STATE OR ACCOUNT RECORD) *
00131 *  RATING METHOD -  (CODE FROM BENEFIT)                          *
00132 *  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
00133 *  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
00134 *  COMPANY I.D. (3 CHARACTER)                                    *
00135 *  BENEFIT CODE                                                  *
00136 *  BENEFIT OVERRIDE CODE                                         *
00137 *                                                                *
00138 *  RETURNED FROM ELRFND                                          *
00139 *  --------------------                                          *
00140 *  CALCULATED REFUND                                             *
00141 *----------------------------------------------------------------*
00142 *  PASSED TO ELEARN                                              *
00143 *  ----------------                                              *
00144 *  CERT ISSUE DATE                                               *
00145 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00146 *  REMAINING TERM (REMAINING TERM 1 FROM ELTERM)                 *
00147 *  RULE OF 78 OPTION (FROM CONTROL RECORD)                       *
00148 *  STATE CODE (CLIENT DEFINED)                                   *
00149 *  STATE CODE (STANDARD P.O. ABBRV)                              *
00150 *  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
00151 *  DEVIATION CODE                                                *
00152 *  ISSUE AGE                                                     *
00153 *  ORIGINAL BENEFIT AMOUNT                                       *
00154 *  BENEFIT KIND (LIFE OR A/H)                                    *
00155 *  A.P.R.                                                        *
00156 *  METHOD - (EARNING CODE FROM BENEFIT RECORD)                   *
00157 *  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
00158 *  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
00159 *  COMPANY I.D. (3 CHARACTER)                                    *
00160 *  BENEFIT CODE                                                  *
00161 *  BENEFIT OVERRIDE CODE                                         *
00162 *                                                                *
00163 *  RETURNED FROM ELEARN                                          *
00164 *  --------------------                                          *
00165 *  INDICATED  EARNINGS                                           *
00166 *----------------------------------------------------------------*
00167 *                 LENGTH = 450                                   *
00168 *                                                                *
00169 ******************************************************************
010303******************************************************************
010303*                   C H A N G E   L O G
010303*
010303* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
010303*-----------------------------------------------------------------
010303*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
010303* EFFECTIVE    NUMBER
010303*-----------------------------------------------------------------
010303* 010303    2001061800003  PEMA  ADD DCC/MONTHLY PROCESSING
033104* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
101807* 101807    2007100100007  PEMA  EXPAND CLM RESERVE FIELDS
010410* 010410    2008021200005  PEMA  ADD FIELDS FOR MN NET PAY BALLOON
010410* 010410    2009050700003  PEMA  ADD FIELDS FOR SPP-DD
041310* 041310  CR2008021200005  PEMA  ADD CODE FOR MN LEVEL
041710* 041710    2007111300001  AJRA  ADD CLAIM CALC SW FOR SC NP+6
101110* 101110  CR2010012700001  PEMA ADD DDF REFUND/UEP PROCESSING
071211* 071211  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
040615* 040615  CR2013072200002  PEMA  ADD EXTRA PERIODS
010716* 010716    2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
010303******************************************************************
00170
00171  01  CALCULATION-PASS-AREA.
00172      12  CP-COMM-LENGTH            PIC S9(4)         VALUE +450
00173                                      COMP.
00174
00175      12  CP-RETURN-CODE            PIC X             VALUE ZERO.
00176        88  NO-CP-ERROR                             VALUE ZERO.
00177        88  CP-ERROR-OCCURED VALUE '1' '2' '3' '4' '5' '6' '7' '8'
00178                                   '9' 'A' 'B' 'C' 'D' 'E' 'H'.
00179        88  CP-ERROR-IN-AMOUNTS                     VALUE '1'.
00180        88  CP-ERROR-IN-DATES                       VALUE '2'.
00181        88  CP-ERROR-IN-OPTIONS                     VALUE '3'.
00182        88  CP-ERROR-IN-TERMS                       VALUE '4'.
00183        88  CP-ERROR-IN-FREQUENCY                   VALUE '5'.
00184        88  CP-ERROR-RATE-NOT-FOUND                 VALUE '6'.
00185        88  CP-ERROR-RATE-IS-ZERO                   VALUE '7'.
00186        88  CP-ERROR-AMT-OUTSIDE-LIMIT              VALUE '8'.
00187        88  CP-ERROR-TERM-OUTSIDE-LIMIT             VALUE '9'.
00188        88  CP-ERROR-AGE-OUTSIDE-LIMIT              VALUE 'A'.
00189        88  CP-ERROR-ATT-OUTSIDE-LIMIT              VALUE 'B'.
00190        88  CP-ERROR-TOT-OUTSIDE-LIMIT              VALUE 'C'.
00191        88  CP-ERROR-RATE-FILE-NOTOPEN              VALUE 'D'.
00192        88  CP-ERROR-ISSUE-AGE-ZERO                 VALUE 'E'.
00193        88  CP-ERROR-NO-LIMITS-CRI                  VALUE 'F'.
00194        88  CP-ERROR-DIV-BY-ZERO                    VALUE 'G'.
00195        88  CP-ERROR-LOAN-TERM                      VALUE 'H'.
00196
00197      12  CP-RETURN-CODE-2          PIC X             VALUE ZERO.
00198        88  NO-CP-ERROR-2                           VALUE ZERO.
00199 ***********************  INPUT AREAS ****************************
00200
00201      12  CP-CALCULATION-AREA.
00202          16  CP-ACCOUNT-NUMBER     PIC X(10)       VALUE SPACES.
00203          16  CP-CERT-EFF-DT        PIC XX.
00204          16  CP-VALUATION-DT       PIC XX.
00205          16  CP-PAID-THRU-DT       PIC XX.
00206          16  CP-BENEFIT-TYPE       PIC X.
00207            88  CP-AH                               VALUE 'A' 'D'
00208                                                    'I' 'U'.
00209            88  CP-REDUCING-LIFE                    VALUE 'R'.
00210            88  CP-LEVEL-LIFE                       VALUE 'L' 'P'.
00211          16  CP-INCURRED-DT        PIC XX.
00212          16  CP-REPORTED-DT        PIC XX.
00213          16  CP-ACCT-FLD-5         PIC XX            VALUE SPACE.
00214          16  CP-COMPANY-ID         PIC XXX           VALUE SPACE.
00215          16  CP-ISSUE-AGE          PIC S9(3)         VALUE ZERO
00216                                      COMP-3.
00217          16  CP-CDT-PERCENT        PIC S9(3)V99      VALUE ZERO
00218                                      COMP-3.
00219          16  CP-CDT-METHOD         PIC X.
00220            88  CP-CDT-ROUND-NEAR                   VALUE '1'.
00221            88  CP-CDT-ROUND-HIGH                   VALUE '2'.
00222            88  CP-CDT-INTERPOLATED                 VALUE '3'.
00223          16  CP-CLAIM-TYPE         PIC X.
00224            88  CP-AH-CLAIM                         VALUE 'A'.
00225            88  CP-LIFE-CLAIM                       VALUE 'L'.
00226          16  CP-ORIGINAL-TERM      PIC S9(3)         VALUE ZERO
00227                                      COMP-3.
00228          16  CP-ORIGINAL-BENEFIT   PIC S9(9)V99      VALUE ZERO
00229                                      COMP-3.
00230          16  CP-ORIGINAL-PREMIUM   PIC S9(7)V99      VALUE ZERO
00231                                      COMP-3.
00232          16  CP-REMAINING-TERM     PIC S9(3)V99      VALUE ZERO
00233                                      COMP-3.
00234          16  CP-REMAINING-BENEFIT  PIC S9(9)V99      VALUE ZERO
00235                                      COMP-3.
00236          16  CP-LOAN-APR           PIC S9(3)V9(4)    VALUE ZERO
00237                                      COMP-3.
00238          16  CP-PAY-FREQUENCY      PIC S9(3)         VALUE ZERO
00239                                      COMP-3.
00240          16  CP-REM-TERM-METHOD    PIC X.
00241            88  CP-EARN-AFTER-15TH                  VALUE '1'.
00242            88  CP-EARN-ON-HALF-MONTH               VALUE '2'.
00243            88  CP-EARN-ON-1ST-DAY                  VALUE '3'.
00244            88  CP-EARN-ON-FULL-MONTH               VALUE '4'.
00245            88  CP-EARN-WITH-NO-DAYS                VALUE '5'.
00246            88  CP-EARN-AFTER-14TH                  VALUE '6'.
00247            88  CP-EARN-AFTER-16TH                  VALUE '7'.
00248          16  CP-EARNING-METHOD     PIC X.
00249            88  CP-EARN-BY-R78                      VALUE '1' 'R'.
00250            88  CP-EARN-BY-PRORATA                  VALUE '2' 'P'.
00251            88  CP-EARN-AS-CALIF                    VALUE '3' 'C'.
00252            88  CP-EARN-AS-TEXAS                    VALUE '4' 'T'.
00253            88  CP-EARN-AS-FARM-PLAN                VALUE '4' 'T'.
00254            88  CP-EARN-AS-NET-PAY                  VALUE '5' 'N'.
00255            88  CP-EARN-ANTICIPATION                VALUE '6' 'A'.
00256            88  CP-EARN-AS-MEAN                     VALUE '8' 'M'.
00257            88  CP-EARN-AS-SUM-OF-DIGITS            VALUE '9'.
00258            88  CP-EARN-AS-REG-BALLOON              VALUE 'B'.
033104           88  CP-GAP-NON-REFUNDABLE               VALUE 'G'.
033104           88  CP-GAP-ACTUARIAL                    VALUE 'S'.
092310           88  CP-DCC-SPP-DDF                      VALUE 'D' 'I'.
                 88  CP-DCC-SPP-DDF-IU                   VALUE 'I'.
00259          16  CP-PROCESS-TYPE       PIC X.
00260            88  CP-CLAIM                            VALUE '1'.
00261            88  CP-CANCEL                           VALUE '2'.
00262            88  CP-ISSUE                            VALUE '3'.
00263          16  CP-SPECIAL-CALC-CD    PIC X.
00264            88  CP-OUTSTANDING-BAL              VALUE 'O'.
00265            88  CP-1-MTH-INTEREST               VALUE ' '.
00266            88  CP-0-MTH-INTEREST               VALUE 'A'.
00267            88  CP-OB-OFFLINE-RESERVED          VALUE 'B'.
00268            88  CP-CRITICAL-PERIOD              VALUE 'C'.
00269            88  CP-TERM-IS-DAYS                 VALUE 'D'.
00270            88  CP-USE-PREM-AS-ENTERED          VALUE 'E'.
00271            88  CP-FARM-PLAN                    VALUE 'F'.
00272            88  CP-RATE-AS-STANDARD             VALUE 'G'.
00273            88  CP-2-MTH-INTEREST               VALUE 'I'.
00274            88  CP-3-MTH-INTEREST               VALUE 'J'.
00275            88  CP-4-MTH-INTEREST               VALUE 'K'.
00276            88  CP-BALLOON-LAST-PMT             VALUE 'L'.
00277            88  CP-MORTGAGE-REC                 VALUE 'M'.
00278            88  CP-OUTSTANDING-BALANCE          VALUE 'O'.
00279            88  CP-NET-PAY-PRUDENTIAL           VALUE 'P'.
00280            88  CP-NET-PAY-SIMPLE               VALUE 'S'.
00281            88  CP-TRUNCATED-LIFE               VALUE 'T' 'U' 'V'
00282                                                      'W' 'X'.
00283            88  CP-TRUNCATE-0-MTH               VALUE 'T'.
00284            88  CP-TRUNCATE-1-MTH               VALUE 'U'.
00285            88  CP-TRUNCATE-2-MTH               VALUE 'V'.
00286            88  CP-TRUNCATE-3-MTH               VALUE 'W'.
00287            88  CP-TRUNCATE-4-MTH               VALUE 'X'.
00288            88  CP-SUMMARY-REC                  VALUE 'Z'.
00289            88  CP-PROPERTY-BENEFIT             VALUE '2'.
00290            88  CP-UNEMPLOYMENT-BENEFIT         VALUE '3'.
00291            88  CP-AD-D-BENEFIT                 VALUE '4'.
00292            88  CP-CSL-METH-1                   VALUE '5'.
00293            88  CP-CSL-METH-2                   VALUE '6'.
00294            88  CP-CSL-METH-3                   VALUE '7'.
00295            88  CP-CSL-METH-4                   VALUE '8'.
00296
00297          16  CP-LOAN-TERM          PIC S9(3)       VALUE ZERO
00298                                      COMP-3.
00299          16  CP-CLASS-CODE         PIC XX          VALUE ZERO.
00300          16  CP-DEVIATION-CODE     PIC XXX         VALUE ZERO.
00301          16  CP-STATE              PIC XX          VALUE SPACE.
00302          16  CP-STATE-STD-ABBRV    PIC XX          VALUE SPACE.
00303          16  CP-BENEFIT-CD         PIC XX          VALUE ZERO.
00304            88  CP-CSL-VALID-NP-BENEFIT-CD VALUES '12' '13'
00305                '34' '35' '36' '37' '44' '45' '46' '47' '72' '73'.
00306          16  CP-R78-OPTION         PIC X.
00307            88  CP-TERM-TIMES-TERM-PLUS-1           VALUE ' '.
00308            88  CP-TERM-TIMES-TERM                  VALUE '1'.
00309
00310          16  CP-COMPANY-CD         PIC X             VALUE SPACE.
00311          16  CP-IBNR-RESERVE-SW    PIC X.
00312          16  CP-CLAIM-STATUS       PIC X.
00313          16  CP-RATE-FILE          PIC X.
00314          16  CP-TERM-OR-EXT-DAYS   PIC S9(05)        VALUE ZERO
00315                                      COMP-3.
00316
00317          16  CP-LIFE-OVERRIDE-CODE PIC X.
00318          16  CP-AH-OVERRIDE-CODE   PIC X.
00319
00320          16  CP-RATE-DEV-PCT       PIC S9V9(6)       VALUE ZERO
00321                                      COMP-3.
               16  CP-CLP-RATE-UP        REDEFINES CP-RATE-DEV-PCT
                                         PIC S9(5)V99 COMP-3.
00322          16  CP-CRITICAL-MONTHS    PIC S9(3)         VALUE ZERO
00323                                      COMP-3.
00324          16  CP-ALTERNATE-BENEFIT  PIC S9(9)V99      VALUE ZERO
00325                                      COMP-3.
00326          16  CP-ALTERNATE-PREMIUM  PIC S9(7)V99      VALUE ZERO
00327                                      COMP-3.
               16  CP-DDF-CSO-ADMIN-FEE REDEFINES CP-ALTERNATE-PREMIUM
                                        PIC S9(7)V99 COMP-3.
00328
00329          16  CP-PAID-FROM-DATE     PIC X(02).
00330          16  CP-CLAIM-CALC-METHOD  PIC X(01).
00331          16  CP-EXT-DAYS-CALC      PIC X.
00332            88  CP-EXT-NO-CHG                   VALUE ' '.
00333            88  CP-EXT-CHG-LF                   VALUE '1'.
00334            88  CP-EXT-CHG-AH                   VALUE '2'.
00335            88  CP-EXT-CHG-LF-AH                VALUE '3'.
00336          16  CP-DOMICILE-STATE     PIC XX.
00337          16  CP-CARRIER            PIC X.
00338          16  CP-REIN-FLAG          PIC X.
00339          16  CP-REM-TRM-CALC-OPTION PIC X.
00340            88  VALID-REM-TRM-CALC-OPTION    VALUE '1'
00341                       '2' '3' '4' '5'.
00342            88  CP-CALC-OPTION-DEFAULT       VALUE '4'.
00343            88  CP-CONSIDER-EXTENSION        VALUE '3' '4' '5'.
00344            88  CP-30-DAY-MONTH              VALUE '1' '3' '5'.
00345            88  CP-NO-EXT-30-DAY-MONTH       VALUE '1'.
00346            88  CP-NO-EXT-ACTUAL-DAYS        VALUE '2'.
00347            88  CP-EXT-30-DAY-MONTH          VALUE '3'.
00348            88  CP-EXT-ACTUAL-DAYS           VALUE '4'.
                 88  CP-USE-EXP-AND-1ST-PMT       VALUE '5'.
00349          16  CP-SIG-SWITCH         PIC X.
00350          16  CP-RATING-METHOD      PIC X.
00351            88  CP-RATE-AS-R78                      VALUE '1' 'R'.
00352            88  CP-RATE-AS-PRORATA                  VALUE '2' 'P'.
00353            88  CP-RATE-AS-CALIF                    VALUE '3' 'C'.
00354            88  CP-RATE-AS-TEXAS                    VALUE '4' 'T'.
00355            88  CP-RATE-AS-FARM-PLAN                VALUE '4' 'T'.
00356            88  CP-RATE-AS-NET-PAY                  VALUE '5' 'N'.
00357            88  CP-RATE-AS-ANTICIPATION             VALUE '6' 'A'.
00358            88  CP-RATE-AS-MEAN                     VALUE '8' 'M'.
00359            88  CP-RATE-AS-REG-BALLOON              VALUE 'B'.
00360          16  CP-SALES-TAX          PIC S9V9999     VALUE  ZEROS
00361                                      COMP-3.
090803         16  CP-BEN-CATEGORY       PIC X.
011904         16  CP-DCC-LF-RATE        PIC S99V9(5) COMP-3 VALUE +0.
               16  CP-DCC-ACT-COMM REDEFINES CP-DCC-LF-RATE
                                         PIC S99V9(5) COMP-3.
011904         16  CP-DCC-AH-RATE        PIC S99V9(5) COMP-3 VALUE +0.
               16  CP-DCC-PMF-COMM REDEFINES CP-DCC-AH-RATE
                                         PIC S99V9(5) COMP-3.
080305         16  CP-DAYS-TO-1ST-PMT    PIC S999     COMP-3 VALUE +0.
               16  CP-AH-BALLOON-SW      PIC X  VALUE ' '.
041310         16  CP-EXPIRE-DT          PIC XX.
041710         16  CP-LF-CLAIM-CALC-SW   PIC X  VALUE ' '.
               16  CP-DDF-HI-FACT        PIC S9V999   COMP-3 VALUE +0.
               16  CP-DDF-LO-FACT        PIC S9V999   COMP-3 VALUE +0.
               16  CP-DDF-CLP            PIC S9(5)V99 COMP-3 VALUE +0.
               16  CP-DDF-SPEC-CALC      PIC X.
                   88  CP-CALC-GROSS-FEE        VALUE 'G'.
                   88  CP-CALC-CLP              VALUE 'C'.
               16  CP-IU-RATE-UP         PIC S9(5)V99   COMP-3 VALUE +0.
               16  CP-CANCEL-REASON      PIC X.
               16  CP-DDF-ADMIN-FEES     PIC S9(5)V99 COMP-3 VALUE +0.
               16  CP-PMT-MODE           PIC X.
               16  CP-NO-OF-PMTS         PIC S999 COMP-3 VALUE +0.
071211         16  CP-1ST-YR-ALLOW       PIC S999V99 COMP-3 VALUE +0.
               16  CP-DDF-COMM-AND-MFEE  PIC S9(5)V99 COMP-3 VALUE +0.
               16  CP-DDF-YR1AF          PIC S9(5)V99 COMP-3 VALUE +0.
071211         16  FILLER                PIC X.
00363
00364 ***************    OUTPUT FROM ELRESV   ************************
00365
00366          16  CP-CDT-TABLE          PIC 9             VALUE ZERO.
00367
00368          16  CP-CDT-FACTOR         PIC S9(5)V9(6)    VALUE ZERO
00369                                      COMP-3.
101807         16  CP-PTC-RESERVE        PIC S9(7)V99   VALUE ZERO
101807                                     COMP-3.
101807         16  CP-IBNR-RESERVE       PIC S9(7)V99   VALUE ZERO
101807                                     COMP-3.
101807         16  CP-FUTURE-RESERVE     PIC S9(7)V99   VALUE ZERO
101807                                     COMP-3.
101807         16  FILLER                PIC X(09).
00377 ***************    OUTPUT FROM ELRTRM   *************************
00378
00379          16  CP-REMAINING-TERM-1   PIC S9(4)V9    VALUE ZERO
00380                                      COMP-3.
00381          16  CP-REMAINING-TERM-2   PIC S9(4)V9    VALUE ZERO
00382                                      COMP-3.
00383          16  CP-REMAINING-TERM-3   PIC S9(4)V9    VALUE ZERO
00384                                      COMP-3.
00385          16  CP-ODD-DAYS           PIC S9(3)      VALUE ZERO
00386                                      COMP-3.
00387          16  FILLER                PIC X(12).
00388
00389 ***************    OUTPUT FROM ELRAMT   *************************
00390
00391          16  CP-REMAINING-AMT      PIC S9(9)V99   VALUE ZERO
00392                                      COMP-3.
00393          16  CP-REMAINING-AMT-PRV  PIC S9(9)V99   VALUE ZERO
00394                                      COMP-3.
00395          16  FILLER                PIC X(12).
00396
00397 ***************    OUTPUT FROM ELRATE   *************************
00398
00399          16  CP-CALC-PREMIUM       PIC S9(7)V99   VALUE ZERO
00400                                      COMP-3.
00401          16  CP-PREMIUM-RATE       PIC S9(2)V9(5) VALUE ZERO
00402                                      COMP-3.
00403          16  CP-MORTALITY-CODE     PIC X(4).
00404          16  CP-RATE-EDIT-FLAG     PIC X.
00405              88  CP-RATES-NEED-APR                  VALUE '1'.
00406          16  CP-COMPOSITE-RATE     PIC S99V999    VALUE ZERO
00407                                      COMP-3.
010716         16  CP-CANCEL-FEE         PIC S9(3)V99 VALUE +0 COMP-3.
032905         16  CP-LF-PREM            PIC S9(7)V99 VALUE +0 COMP-3.
               16  CP-LF-BALLOON-PREM REDEFINES CP-LF-PREM
                                         PIC S9(7)V99 COMP-3.
00409          16  FILLER                PIC X(07).
00410
00411 ***************    OUTPUT FROM ELRFND   *************************
00412
00413          16  CP-CALC-REFUND        PIC S9(7)V99   VALUE ZERO
00414                                      COMP-3.
00415          16  CP-REFUND-TYPE-USED   PIC X.
00416            88  CP-R-AS-R78                         VALUE '1'.
00417            88  CP-R-AS-PRORATA                     VALUE '2'.
00418            88  CP-R-AS-CALIF                       VALUE '3'.
00419            88  CP-R-AS-TEXAS                       VALUE '4'.
00420            88  CP-R-AS-FARM-PLAN                   VALUE '4'.
00421            88  CP-R-AS-NET-PAY                     VALUE '5'.
00422            88  CP-R-AS-ANTICIPATION                VALUE '6'.
00423            88  CP-R-AS-MEAN                        VALUE '8'.
00424            88  CP-R-AS-SUM-OF-DIGITS               VALUE '9'.
033104           88  CP-R-AS-GAP-NON-REFUND              VALUE 'G'.
033104           88  CP-R-AS-GAP-ACTUARIAL               VALUE 'S'.
092310           88  CP-R-AS-SPP-DDF                     VALUE 'D'.
092310           88  CP-R-AS-SPP-DDF-IU                  VALUE 'I'.
                 88  CP-R-AS-REPOSSESSION                VALUE 'R'.
00425          16  FILLER                PIC X(12).
00426
00427 ***************    OUTPUT FROM ELEARN   *************************
00428
00429          16  CP-R78-U-PRM          PIC S9(7)V99   VALUE ZERO
00430                                      COMP-3.
00431          16  CP-R78-U-PRM-ALT      PIC S9(7)V99   VALUE ZERO
00432                                      COMP-3.
00433          16  CP-PRORATA-U-PRM      PIC S9(7)V99   VALUE ZERO
00434                                      COMP-3.
00435          16  CP-PRORATA-U-PRM-ALT  PIC S9(7)V99   VALUE ZERO
00436                                      COMP-3.
00437          16  CP-STATE-U-PRM        PIC S9(7)V99   VALUE ZERO
00438                                      COMP-3.
00439          16  CP-DOMICILE-U-PRM     PIC S9(7)V99   VALUE ZERO
00440                                      COMP-3.
00441          16  CP-EARNING-TYPE-USED  PIC X.
00442            88  CP-E-AS-SPECIAL                     VALUE 'S'.
00443            88  CP-E-AS-R78                         VALUE '1'.
00444            88  CP-E-AS-PRORATA                     VALUE '2'.
00445            88  CP-E-AS-TEXAS                       VALUE '4'.
00446            88  CP-E-AS-FARM-PLAN                   VALUE '4'.
00447            88  CP-E-AS-NET-PAY                     VALUE '5'.
00448            88  CP-E-AS-ANTICIPATION                VALUE '6'.
00449            88  CP-E-AS-MEAN                        VALUE '8'.
00450            88  CP-E-AS-SUM-OF-DIGITS               VALUE '9'.
00451          16  FILLER                PIC X(12).
00452
00453 ***************    OUTPUT FROM ELPMNT   *************************
00454
00455          16  CP-ACTUAL-DAYS        PIC S9(05)     VALUE ZERO
00456                                      COMP-3.
00457          16  CP-CLAIM-PAYMENT      PIC S9(7)V99   VALUE ZERO
00458                                      COMP-3.
00459          16  FILLER                PIC X(12).
00460
00461 ***************   MISC WORK AREAS    *****************************
00462          16  CP-TOTAL-PAID         PIC S9(7)V99   VALUE ZERO
00463                                      COMP-3.
00464          16  CP-R-MAX-ATT-AGE      PIC S9(3)      VALUE ZERO
00465                                      COMP-3.
00466          16  CP-R-MAX-AGE          PIC S9(3)      VALUE ZERO
00467                                      COMP-3.
00468          16  CP-R-MAX-TERM         PIC S9(5)      VALUE ZERO
00469                                      COMP-3.
00470          16  CP-R-MAX-TOT-BEN      PIC S9(7)V99   VALUE ZERO
00471                                      COMP-3.
00472          16  CP-R-MAX-MON-BEN      PIC S9(7)V99   VALUE ZERO
00473                                      COMP-3.
00474          16  CP-IO-FUNCTION        PIC X          VALUE SPACE.
00475              88  OPEN-RATE-FILE                   VALUE 'O'.
00476              88  CLOSE-RATE-FILE                  VALUE 'C'.
00477              88  IO-ERROR                         VALUE 'E'.
00478
00479          16  CP-FIRST-PAY-DATE     PIC XX.
00480
00481          16  CP-JOINT-INDICATOR    PIC X.
00482
00483          16  CP-RESERVE-REMAINING-TERM
00484                                    PIC S9(4)V9    VALUE ZERO
00485                                      COMP-3.
00486
00487          16  CP-INSURED-BIRTH-DT   PIC XX.
00488
00489          16  CP-INCURRED-AGE       PIC S9(3)      VALUE ZERO
00490                                      COMP-3.
00491
00492          16  CP-MONTHLY-PAYMENT    PIC S9(5)V99   VALUE ZERO
00493                                      COMP-3.
00494
00495          16  CP-RATING-BENEFIT-AMT PIC S9(9)V99   VALUE ZERO
00496                                      COMP-3.
00497
00498          16  CP-ODD-DAYS-TO-PMT    PIC S9(3)      VALUE ZERO
00499                                      COMP-3.
00500
00501          16  CP-MNTHS-TO-FIRST-PMT PIC S9(3)      VALUE ZERO
00502                                      COMP-3.
00503
00504          16  CP-REMAMT-FACTOR      PIC S9(4)V9(9) VALUE ZEROS
00505                                      COMP-3.
00506
00507          16  CP-FREE-LOOK          PIC S9(3)      VALUE ZERO
00508                                      COMP-3.
00509
00510          16  CP-ROA-REFUND         PIC X          VALUE 'N'.
00511              88  CP-ROA-PREM-AT-REFUND            VALUE 'Y'.
00512
010303         16  CP-NET-BENEFIT-AMT    PIC S9(9)V99   VALUE ZERO
010303                                     COMP-3.
041710         16  CP-SCNP-6MO-AMT       PIC S9(9)V99   VALUE ZERO
041710                                     COMP-3.
               16  CP-MONTH              PIC S999     COMP-3 VALUE +0.
040615         16  cp-extra-periods      pic 9 value zeros.
070115         16  cp-net-only-state     pic x value spaces.
041710         16  FILLER                PIC X(13).
00514 ******************************************************************
00268 *                                    COPY ELCEMIB.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCEMIB.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.005                          *
00006 *                                                                *
00007 *    STANDARD CLAS-IC ERROR MESSAGE COMMUNICATIONS AREA          *
00008 *                                                                *
00009 ******************************************************************
00010  01  ERROR-MESSAGE-INTERFACE-BLOCK.
00011      12  EMI-COMM-LENGTH         PIC S9(4)    VALUE +400 COMP.
00012      12  EMI-NUMBER-OF-LINES     PIC 9        VALUE 1.
00013      12  EMI-ERROR               PIC 9(4)     VALUE ZEROS.
00014      12  EMI-SUB                 PIC 99       VALUE 1 COMP-3.
00015      12  EMI-NOTE-CTR            PIC 999      VALUE 0 COMP-3.
00016      12  EMI-WARNING-CTR         PIC 999      VALUE 0 COMP-3.
00017      12  EMI-FORCABLE-CTR        PIC 999      VALUE 0 COMP-3.
00018      12  EMI-FATAL-CTR           PIC 999      VALUE 0 COMP-3.
00019      12  EMI-SWITCH1             PIC X        VALUE '1'.
00020          88  EMI-NO-ERRORS                    VALUE '1'.
00021          88  EMI-ERRORS-NOT-COMPLETE          VALUE '2'.
00022          88  EMI-ERRORS-COMPLETE              VALUE '3'.
00023      12  EMI-SWITCH2             PIC X        VALUE '1'.
00024          88  EMI-FORMAT-CODES-ONLY            VALUE '2'.
00025      12  EMI-SWITCH-AREA-1       PIC X        VALUE '1'.
00026          88  EMI-AREA1-EMPTY                  VALUE '1'.
00027          88  EMI-AREA1-FULL                   VALUE '2'.
00028      12  EMI-SWITCH-AREA-2       PIC X        VALUE '1'.
00029          88  EMI-AREA2-EMPTY                  VALUE '1'.
00030          88  EMI-AREA2-FULL                   VALUE '2'.
00031      12  EMI-ACTION-SWITCH       PIC X        VALUE ' '.
00032          88  EMI-PROCESS-ALL-ERRORS           VALUE ' '.
00033          88  EMI-BYPASS-NOTES                 VALUE 'N'.
00034          88  EMI-BYPASS-WARNINGS              VALUE 'W'.
00035          88  EMI-BYPASS-FORCABLES             VALUE 'F'.
00036          88  EMI-BYPASS-FATALS                VALUE 'X'.
00037      12  EMI-ERROR-LINES.
00038          16  EMI-LINE1           PIC X(72)   VALUE SPACES.
00039          16  EMI-LINE2           PIC X(72)   VALUE SPACES.
00040          16  EMI-LINE3           PIC X(72)   VALUE SPACES.
00041          16  EMI-CODE-LINE REDEFINES EMI-LINE3.
00042              20  EMI-ERR-CODES OCCURS 10 TIMES.
00043                  24  EMI-ERR-NUM         PIC X(4).
00044                  24  EMI-FILLER          PIC X.
00045                  24  EMI-SEV             PIC X.
00046                  24  FILLER              PIC X.
00047              20  FILLER                  PIC X(02).
00048      12  EMI-ERR-LINES REDEFINES EMI-ERROR-LINES.
00049          16  EMI-MESSAGE-AREA OCCURS 3 TIMES INDEXED BY EMI-INDX.
00050              20  EMI-ERROR-NUMBER    PIC X(4).
00051              20  EMI-FILL            PIC X.
00052              20  EMI-SEVERITY        PIC X.
00053              20  FILLER              PIC X.
00054              20  EMI-ERROR-TEXT.
00055                  24  EMI-TEXT-VARIABLE   PIC X(10).
00056                  24  FILLER          PIC X(55).
00057      12  EMI-SEVERITY-SAVE           PIC X.
00058          88  EMI-NOTE                    VALUE 'N'.
00059          88  EMI-WARNING                 VALUE 'W'.
00060          88  EMI-FORCABLE                VALUE 'F'.
00061          88  EMI-FATAL                   VALUE 'X'.
00062      12  EMI-MESSAGE-FLAG            PIC X.
00063          88  EMI-MESSAGE-FORMATTED       VALUE 'Y'.
00064          88  EMI-NO-MESSAGE-FORMATTED    VALUE 'N'.
00065      12  EMI-ROLL-SWITCH             PIC X       VALUE SPACES.
00066      12  EMI-LANGUAGE-IND            PIC X       VALUE SPACES.
00067          88  EMI-LANGUAGE-IS-FR                  VALUE 'F'.
00068          88  EMI-LANGUAGE-IS-ENG                 VALUE 'E'.
00069          88  EMI-LANGUAGE-IS-SPAN                VALUE 'S'.
           12  emi-claim-no                pic x(7).
           12  emi-claim-type              pic x(6).
00070      12  FILLER                      PIC X(124)  VALUE SPACES.
00071      12  EMI-DATE-FIELD              PIC X(06)   VALUE SPACES.
00072      12  EMI-CLIENT-ID               PIC X(3)    VALUE SPACES.
00073      12  EMI-LIFE-OVERRIDE-L6        PIC X(6).
00074      12  EMI-AH-OVERRIDE-L6          PIC X(6).
00271 *                                    COPY ELCLOGOF.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCLOGOF.                           *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *             STANDARD CLAS-IC LOGOFF TEXT AREA                  *
00007 *                                                                *
00008 ******************************************************************
00009  01  CLASIC-LOGOFF.
00010      12  LOGOFF-LENGTH       PIC S9(4)   VALUE +185   COMP.
00011      12  LOGOFF-TEXT.
00012          16  FILLER          PIC X(5)    VALUE SPACES.
00013          16  LOGOFF-MSG.
00014              20  LOGOFF-PGM  PIC X(8)    VALUE SPACES.
00015              20  FILLER      PIC X       VALUE SPACES.
00016              20  LOGOFF-FILL PIC X(66)   VALUE SPACES.
00017          16  FILLER          PIC X(80)
00018            VALUE '* YOU ARE NOW LOGGED OFF'.
00019          16  FILLER          PIC X(7)    VALUE '* LOGIC'.
00020          16  FILLER          PIC X       VALUE QUOTE.
00021          16  LOGOFF-SYS-MSG  PIC X(17)
00022            VALUE 'S CLAS-IC SYSTEM '.
00023      12  TEXT-MESSAGES.
00024          16  UNACCESS-MSG    PIC X(29)
00025              VALUE  'UNAUTHORIZED ACCESS ATTEMPTED'.
00026          16  PGMIDERR-MSG    PIC X(17)
00027              VALUE 'PROGRAM NOT FOUND'.
00274 *                                    COPY ELCATTR.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCATTR.                            *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *             LIST OF STANDARD ATTRIBUTE VALUES                  *
00007 *                                                                *
00008 *   THE DATA NAMES IN THIS COPY BOOK WERE ASSIGNED AS FOLLOWS:   *
00009 *                                                                *
00010 *                   POS 1   P=PROTECTED                          *
00011 *                           U=UNPROTECTED                        *
00012 *                           S=ASKIP                              *
00013 *                   POS 2   A=ALPHA/NUMERIC                      *
00014 *                           N=NUMERIC                            *
00015 *                   POS 3   N=NORMAL                             *
00016 *                           B=BRIGHT                             *
00017 *                           D=DARK                               *
00018 *                   POS 4-5 ON=MODIFIED DATA TAG ON              *
00019 *                           OF=MODIFIED DATA TAG OFF             *
00020 *                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCATTR                          *
00021 ******************************************************************
00022  01  ATTRIBUTE-LIST.
00023      12  AL-PABOF            PIC X       VALUE 'Y'.
00024      12  AL-PABON            PIC X       VALUE 'Z'.
00025      12  AL-PADOF            PIC X       VALUE '%'.
00026      12  AL-PADON            PIC X       VALUE '_'.
00027      12  AL-PANOF            PIC X       VALUE '-'.
00028      12  AL-PANON            PIC X       VALUE '/'.
00029      12  AL-SABOF            PIC X       VALUE '8'.
00030      12  AL-SABON            PIC X       VALUE '9'.
00031      12  AL-SADOF            PIC X       VALUE '@'.
00032      12  AL-SADON            PIC X       VALUE QUOTE.
00033      12  AL-SANOF            PIC X       VALUE '0'.
00034      12  AL-SANON            PIC X       VALUE '1'.
00035      12  AL-UABOF            PIC X       VALUE 'H'.
00036      12  AL-UABON            PIC X       VALUE 'I'.
00037      12  AL-UADOF            PIC X       VALUE '<'.
00038      12  AL-UADON            PIC X       VALUE '('.
00039      12  AL-UANOF            PIC X       VALUE ' '.
00040      12  AL-UANON            PIC X       VALUE 'A'.
00041      12  AL-UNBOF            PIC X       VALUE 'Q'.
00042      12  AL-UNBON            PIC X       VALUE 'R'.
00043      12  AL-UNDOF            PIC X       VALUE '*'.
00044      12  AL-UNDON            PIC X       VALUE ')'.
00045      12  AL-UNNOF            PIC X       VALUE '&'.
00046      12  AL-UNNON            PIC X       VALUE 'J'.
00277 *                                    COPY ELCAID.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCAID.                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   DESCRIPTION:  ATTENTION IDENTIFER CHARACTERS.                *
CIDMOD*                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCAID                           *
051007*  051007  2007041300002 Change PF22 from x'D5' to x'5B'
00007 ******************************************************************
00008
00009  01  DFHAID.
00010    02  DFHNULL   PIC  X  VALUE  ' '.
00011    02  DFHENTER  PIC  X  VALUE  QUOTE.
00012    02  DFHCLEAR  PIC  X  VALUE  '_'.
00013    02  DFHPEN    PIC  X  VALUE  '='.
00014    02  DFHOPID   PIC  X  VALUE  'W'.
00015    02  DFHPA1    PIC  X  VALUE  '%'.
00016    02  DFHPA2    PIC  X  VALUE  '>'.
00017    02  DFHPA3    PIC  X  VALUE  ','.
00018    02  DFHPF1    PIC  X  VALUE  '1'.
00019    02  DFHPF2    PIC  X  VALUE  '2'.
00020    02  DFHPF3    PIC  X  VALUE  '3'.
00021    02  DFHPF4    PIC  X  VALUE  '4'.
00022    02  DFHPF5    PIC  X  VALUE  '5'.
00023    02  DFHPF6    PIC  X  VALUE  '6'.
00024    02  DFHPF7    PIC  X  VALUE  '7'.
00025    02  DFHPF8    PIC  X  VALUE  '8'.
00026    02  DFHPF9    PIC  X  VALUE  '9'.
00027    02  DFHPF10   PIC  X  VALUE  ':'.
00028    02  DFHPF11   PIC  X  VALUE  '#'.
00029    02  DFHPF12   PIC  X  VALUE  '@'.
00030    02  DFHPF13   PIC  X  VALUE  'A'.
00031    02  DFHPF14   PIC  X  VALUE  'B'.
00032    02  DFHPF15   PIC  X  VALUE  'C'.
00033    02  DFHPF16   PIC  X  VALUE  'D'.
00034    02  DFHPF17   PIC  X  VALUE  'E'.
00035    02  DFHPF18   PIC  X  VALUE  'F'.
00036    02  DFHPF19   PIC  X  VALUE  'G'.
00037    02  DFHPF20   PIC  X  VALUE  'H'.
00038    02  DFHPF21   PIC  X  VALUE  'I'.
051007*00039    02  DFHPF22   PIC  X  VALUE  '�'.
051007   02  DFHPF22   PIC  X  VALUE  '['.
00040    02  DFHPF23   PIC  X  VALUE  '.'.
00041    02  DFHPF24   PIC  X  VALUE  '<'.
00042    02  DFHMSRE   PIC  X  VALUE  'X'.
00043    02  DFHSTRF   PIC  X  VALUE  'h'.
00044    02  DFHTRIG   PIC  X  VALUE  '"'.
00278
00279  01  FILLER REDEFINES DFHAID.
00280      05  FILLER                      PIC X(8).
00281      05  PF-VALUES                   PIC X
00282          OCCURS 24 TIMES.
00283
      ****************************************************************
      *                                                               
      * Copyright (c) 2007-2013 Dell Inc.                             
      * All rights reserved.                                          
      *                                                               
      ****************************************************************
       01  DFHEIV.                                                    
         02  DFHEIV0               PIC X(35).                         
         02  DFHEIV1               PIC X(08).                         
         02  DFHEIV2               PIC X(08).                         
         02  DFHEIV3               PIC X(08).                         
         02  DFHEIV4               PIC X(06).                         
         02  DFHEIV5               PIC X(04).                         
         02  DFHEIV6               PIC X(04).                         
         02  DFHEIV7               PIC X(02).                         
         02  DFHEIV8               PIC X(02).                         
         02  DFHEIV9               PIC X(01).                         
         02  DFHEIV10              PIC S9(7) COMP-3.                  
         02  DFHEIV11              PIC S9(4) COMP SYNC.               
         02  DFHEIV12              PIC S9(4) COMP SYNC.               
         02  DFHEIV13              PIC S9(4) COMP SYNC.               
         02  DFHEIV14              PIC S9(4) COMP SYNC.               
         02  DFHEIV15              PIC S9(4) COMP SYNC.               
         02  DFHEIV16              PIC S9(9) COMP SYNC.               
         02  DFHEIV17              PIC X(04).                         
         02  DFHEIV18              PIC X(04).                         
         02  DFHEIV19              PIC X(04).                         
         02  DFHEIV20              USAGE IS POINTER.                  
         02  DFHEIV21              USAGE IS POINTER.                  
         02  DFHEIV22              USAGE IS POINTER.                  
         02  DFHEIV23              USAGE IS POINTER.                  
         02  DFHEIV24              USAGE IS POINTER.                  
         02  DFHEIV25              PIC S9(9) COMP SYNC.               
         02  DFHEIV26              PIC S9(9) COMP SYNC.               
         02  DFHEIV27              PIC S9(9) COMP SYNC.               
         02  DFHEIV28              PIC S9(9) COMP SYNC.               
         02  DFHEIV29              PIC S9(9) COMP SYNC.               
         02  DFHEIV30              PIC S9(9) COMP SYNC.               
         02  DFHEIV31              PIC S9(9) COMP SYNC.               
         02  DFHEIV32              PIC S9(4) COMP SYNC.               
         02  DFHEIV33              PIC S9(4) COMP SYNC.               
         02  DFHEIV34              PIC S9(4) COMP SYNC.               
         02  DFHEIV35              PIC S9(4) COMP SYNC.               
         02  DFHEIV97              PIC S9(7) COMP-3 VALUE ZERO.       
         02  DFHEIV98              PIC S9(4) COMP SYNC VALUE ZERO.    
         02  FILLER                PIC X(02).                         
         02  DFHEIV99              PIC X(08) VALUE SPACE.             
         02  DFHEIVL0              PIC X(48) VALUE SPACE.             
         02  DFHEIVL1              PIC X(48) VALUE SPACE.             
         02  DFHEIVL2              PIC X(48) VALUE SPACE.             
         02  DFHEIVL3              PIC X(48) VALUE SPACE.             
         02  DFHEIVL4              PIC X(255) VALUE SPACE.            
         02  DFHEIVL5              PIC X(255) VALUE SPACE.            
       LINKAGE  SECTION.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2007-2013 Dell Inc.                             *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       01  dfheiblk.
           02  eibtime          pic s9(7) comp-3.
           02  eibdate          pic s9(7) comp-3.
           02  eibtrnid         pic x(4).
           02  eibtaskn         pic s9(7) comp-3.
           02  eibtrmid         pic x(4).
           02  dfheigdi         pic s9(4) comp.
           02  eibcposn         pic s9(4) comp.
           02  eibcalen         pic s9(4) comp.
           02  eibaid           pic x(1).
           02  eibfiller1       pic x(1).
           02  eibfn            pic x(2).
           02  eibfiller2       pic x(2).
           02  eibrcode         pic x(6).
           02  eibfiller3       pic x(2).
           02  eibds            pic x(8).
           02  eibreqid         pic x(8).
           02  eibrsrce         pic x(8).
           02  eibsync          pic x(1).
           02  eibfree          pic x(1).
           02  eibrecv          pic x(1).
           02  eibsend          pic x(1).
           02  eibatt           pic x(1).
           02  eibeoc           pic x(1).
           02  eibfmh           pic x(1).
           02  eibcompl         pic x(1).
           02  eibsig           pic x(1).
           02  eibconf          pic x(1).
           02  eiberr           pic x(1).
           02  eibrldbk         pic x(1).
           02  eiberrcd         pic x(4).
           02  eibsynrb         pic x(1).
           02  eibnodat         pic x(1).
           02  eibfiller5       pic x(2).
           02  eibresp          pic s9(8) comp.
           02  eibresp2         pic s9(8) comp.
           02  dfheigdj         pic s9(4) comp.
           02  dfheigdk         pic s9(4) comp.
00285
00286  01  DFHCOMMAREA                     PIC X(1024).
00287
       01  var  pic x(30).
00289 *                                    COPY ELCCERT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCCERT.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.013                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CERTIFICATE MASTER                        *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 450  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCERT                         RKP=2,LEN=33   *
00013 *       ALTERNATE PATH1 = ELCERT2 (BY NAME)       RKP=35,LEN=18  *
00014 *       ALTERNATE PATH2 = ELCERT3 (BY SOC SEC NO) RKP=53,LEN=12  *
00015 *       ALTERNATE PATH3 = ELCERT5 (BY CERT NO.)   RKP=65,LEN=12  *
00016 *       ALTERNATE PATH4 = ELCERT6 (BY MEMBER NO.) RKP=77,LEN=13  *
00017 *                                                                *
00018 *   LOG = YES                                                    *
00019 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
040504* 040504  CR2003080800002  PEMA  ADD DEALER INCENTIVE PROCESSING
061405* 061405  CR2005060300001  PEMA  ADD CLP STATE PROCESS FOR DCC
110105* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
072308* 072308  CR2007110500003  PEMA  ADD NH REFUND INTEREST PROCESSING
102109* 102109  CR2008100900003  AJRA  ADD CLAIM CERT NOTE IND
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
032612* 032612  CR2011110200001  PEMA  AHL CHANGES
090314* 090314  CR2014081300001  PEMA  LOAD CERTS INVOLVED IN THAO
010716* 010716  CR2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
062017* 062017  CR2015091000001  PEMA RENAME INTEREST FIELD
122002******************************************************************
00021
00022  01  CERTIFICATE-MASTER.
00023      12  CM-RECORD-ID                      PIC XX.
00024          88  VALID-CM-ID                      VALUE 'CM'.
00025
00026      12  CM-CONTROL-PRIMARY.
00027          16  CM-COMPANY-CD                 PIC X.
00028          16  CM-CARRIER                    PIC X.
00029          16  CM-GROUPING.
00030              20  CM-GROUPING-PREFIX        PIC X(3).
00031              20  CM-GROUPING-PRIME         PIC X(3).
00032          16  CM-STATE                      PIC XX.
00033          16  CM-ACCOUNT.
00034              20  CM-ACCOUNT-PREFIX         PIC X(4).
00035              20  CM-ACCOUNT-PRIME          PIC X(6).
00036          16  CM-CERT-EFF-DT                PIC XX.
00037          16  CM-CERT-NO.
00038              20  CM-CERT-PRIME             PIC X(10).
00039              20  CM-CERT-SFX               PIC X.
00040
00041      12  CM-CONTROL-BY-NAME.
00042          16  CM-COMPANY-CD-A1              PIC X.
00043          16  CM-INSURED-LAST-NAME          PIC X(15).
00044          16  CM-INSURED-INITIALS.
00045              20  CM-INSURED-INITIAL1       PIC X.
00046              20  CM-INSURED-INITIAL2       PIC X.
00047
00048      12  CM-CONTROL-BY-SSN.
00049          16  CM-COMPANY-CD-A2              PIC X.
00050          16  CM-SOC-SEC-NO.
00051              20  CM-SSN-STATE              PIC XX.
00052              20  CM-SSN-ACCOUNT            PIC X(6).
00053              20  CM-SSN-LN3.
00054                  25  CM-INSURED-INITIALS-A2.
00055                      30 CM-INSURED-INITIAL1-A2   PIC X.
00056                      30 CM-INSURED-INITIAL2-A2   PIC X.
00057                  25 CM-PART-LAST-NAME-A2         PIC X.
00058
00059      12  CM-CONTROL-BY-CERT-NO.
00060          16  CM-COMPANY-CD-A4              PIC X.
00061          16  CM-CERT-NO-A4                 PIC X(11).
00062
00063      12  CM-CONTROL-BY-MEMB.
00064          16  CM-COMPANY-CD-A5              PIC X.
00065          16  CM-MEMBER-NO.
00066              20  CM-MEMB-STATE             PIC XX.
00067              20  CM-MEMB-ACCOUNT           PIC X(6).
00068              20  CM-MEMB-LN4.
00069                  25  CM-INSURED-INITIALS-A5.
00070                      30 CM-INSURED-INITIAL1-A5   PIC X.
00071                      30 CM-INSURED-INITIAL2-A5   PIC X.
00072                  25 CM-PART-LAST-NAME-A5         PIC XX.
00073
00074      12  CM-INSURED-PROFILE-DATA.
00075          16  CM-INSURED-FIRST-NAME.
00076              20  CM-INSURED-1ST-INIT       PIC X.
00077              20  FILLER                    PIC X(9).
00078          16  CM-INSURED-ISSUE-AGE          PIC 99.
00079          16  CM-INSURED-SEX                PIC X.
00080              88  CM-SEX-MALE                  VALUE 'M'.
00081              88  CM-SEX-FEMAL                 VALUE 'F'.
00082          16  CM-INSURED-JOINT-AGE          PIC 99.
00083          16  CM-JOINT-INSURED-NAME.
00084              20  CM-JT-LAST-NAME           PIC X(15).
00085              20  CM-JT-FIRST-NAME.
00086                  24  CM-JT-1ST-INIT        PIC X.
00087                  24  FILLER                PIC X(9).
00088              20  CM-JT-INITIAL             PIC X.
00089
00090      12  CM-LIFE-DATA.
00091          16  CM-LF-BENEFIT-CD              PIC XX.
00092          16  CM-LF-ORIG-TERM               PIC S999      COMP-3.
00093          16  CM-LF-CRITICAL-PERIOD         PIC S999      COMP-3.
00094          16  CM-LF-TERM-IN-DAYS            PIC S9(5)     COMP-3.
00095          16  CM-LF-DEV-CODE                PIC XXX.
00096          16  CM-LF-DEV-PCT                 PIC S9V9(6)   COMP-3.
00097          16  CM-LF-BENEFIT-AMT             PIC S9(9)V99  COMP-3.
00098          16  CM-LF-PREMIUM-AMT             PIC S9(7)V99  COMP-3.
00099          16  CM-LF-ALT-BENEFIT-AMT         PIC S9(9)V99  COMP-3.
00100          16  CM-LF-ALT-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00101          16  CM-LF-NSP-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00102          16  CM-LF-REMAINING-AMT           PIC S9(9)V99  COMP-3.
00103          16  CM-LF-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
00104          16  CM-LF-ITD-DEATH-AMT           PIC S9(9)V99  COMP-3.
00105          16  CM-LF-PREMIUM-RATE            PIC S99V9(5)  COMP-3.
00106          16  CM-LF-POLICY-FEE              PIC S9(3)V99  COMP-3.
00107          16  CM-LF-ALT-PREMIUM-RATE        PIC S99V9(5)  COMP-3.
090314         16  cm-temp-epiq                  pic xx.
090314             88  EPIQ-CLASS                  value 'EQ'.
090314*        16  FILLER                        PIC XX.
00109
00110      12  CM-AH-DATA.
00111          16  CM-AH-BENEFIT-CD              PIC XX.
00112          16  CM-AH-ORIG-TERM               PIC S999      COMP-3.
00113          16  CM-AH-CRITICAL-PERIOD         PIC S999      COMP-3.
00114          16  CM-AH-DEV-CODE                PIC XXX.
00115          16  CM-AH-DEV-PCT                 PIC S9V9(6)   COMP-3.
00116          16  CM-AH-BENEFIT-AMT             PIC S9(7)V99  COMP-3.
00117          16  CM-AH-PREMIUM-AMT             PIC S9(7)V99  COMP-3.
00118          16  CM-AH-NSP-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00119          16  CM-AH-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
00120          16  CM-AH-ITD-LUMP-PMT            PIC S9(7)V99  COMP-3.
00121          16  CM-AH-ITD-AH-PMT              PIC S9(9)V99  COMP-3.
00122          16  CM-AH-PAID-THRU-DT            PIC XX.
00123              88  NO-AH-CLAIMS-PAID            VALUE LOW-VALUE.
00124          16  CM-AH-PREMIUM-RATE            PIC S99V9(5)  COMP-3.
010716         16  CM-CANCEL-FEE                 PIC S9(3)V99  COMP-3.
00126          16  CM-AH-CEDED-BENEFIT           PIC S9(7)V99  COMP-3.
00127          16  FILLER                        PIC X.
00128
00129      12  CM-LOAN-INFORMATION.
00130          16  CM-LIVES                      PIC S9(7)     COMP-3.
011410         16  CM-DDF-IU-RATE-UP REDEFINES CM-LIVES
011410                                           PIC S9(5)V99  COMP-3.
00131          16  CM-BILLED                     PIC S9(7)     COMP-3.
00132          16  CM-LOAN-APR                   PIC S999V9(4) COMP-3.
00133          16  CM-PAY-FREQUENCY              PIC S99.
00134          16  CM-LOAN-TERM                  PIC S999      COMP-3.
00135          16  CM-RATE-CLASS                 PIC XX.
00136          16  CM-BENEFICIARY                PIC X(25).
00137          16  CM-POLICY-FORM-NO             PIC X(12).
00138          16  CM-PMT-EXTENSION-DAYS         PIC S999      COMP-3.
00139          16  CM-LAST-ADD-ON-DT             PIC XX.
00140          16  CM-DEDUCTIBLE-AMOUNTS.
00141              20  CM-CLAIM-DEDUCT-WITHHELD  PIC S9(5)V99  COMP-3.
00142              20  CM-CANCEL-DEDUCT-WITHHELD PIC S9(5)V99  COMP-3.
00143          16  CM-RESIDENT-RATE REDEFINES CM-DEDUCTIBLE-AMOUNTS.
00144              20  CM-RESIDENT-STATE         PIC XX.
00145              20  CM-RATE-CODE              PIC X(4).
00146              20  FILLER                    PIC XX.
110105         16  FILLER REDEFINES CM-DEDUCTIBLE-AMOUNTS.
110105             20  CM-LOAN-OFFICER           PIC X(5).
110105             20  FILLER                    PIC XXX.
00147          16  CM-CSR-CODE                   PIC XXX.
00148          16  CM-UNDERWRITING-CODE          PIC X.
00149              88  CM-POLICY-UNDERWRITTEN       VALUE 'Y'.
081606         16  CM-POST-CARD-IND              PIC X.
062017         16  CM-REF-INTERFACE-SW           PIC X.
00151          16  CM-PREMIUM-TYPE               PIC X.
00152              88  CM-SING-PRM                  VALUE '1'.
00153              88  CM-O-B-COVERAGE              VALUE '2'.
00154              88  CM-OPEN-END                  VALUE '3'.
00155          16  CM-IND-GRP-TYPE               PIC X.
00156              88  CM-INDIVIDUAL                VALUE 'I'.
00157              88  CM-GROUP                     VALUE 'G'.
00158          16  CM-SKIP-CODE                  PIC X.
00159              88  NO-MONTHS-SKIPPED            VALUE SPACE.
00160              88  SKIP-JULY                    VALUE '1'.
00161              88  SKIP-AUGUST                  VALUE '2'.
00162              88  SKIP-SEPTEMBER               VALUE '3'.
00163              88  SKIP-JULY-AUG                VALUE '4'.
00164              88  SKIP-AUG-SEPT                VALUE '5'.
00165              88  SKIP-JULY-AUG-SEPT           VALUE '6'.
00166              88  SKIP-JUNE-JULY-AUG           VALUE '7'.
00167              88  SKIP-JUNE                    VALUE '8'.
00168              88  SKIP-JUNE-JULY               VALUE '9'.
00169              88  SKIP-AUG-SEPT-OCT            VALUE 'A'.
00170              88  SKIP-BI-WEEKLY-3RD-PMT       VALUE 'X'.
00171          16  CM-PAYMENT-MODE               PIC X.
00172              88  PAY-MONTHLY                  VALUE SPACE.
00173              88  PAY-WEEKLY                   VALUE '1'.
00174              88  PAY-SEMI-MONTHLY             VALUE '2'.
00175              88  PAY-BI-WEEKLY                VALUE '3'.
00176              88  PAY-SEMI-ANUALLY             VALUE '4'.
00177          16  CM-LOAN-NUMBER                PIC X(8).
00178          16  CM-LOAN-BALANCE               PIC S9(7)V99  COMP-3.
110105         16  CM-OLD-LOF                    PIC XXX.
00179 *        16  CM-LOAN-OFFICER               PIC XXX.
00180          16  CM-REIN-TABLE                 PIC XXX.
00181          16  CM-SPECIAL-REIN-CODE          PIC X.
00182          16  CM-LF-LOAN-EXPIRE-DT          PIC XX.
00183          16  CM-AH-LOAN-EXPIRE-DT          PIC XX.
00184          16  CM-LOAN-1ST-PMT-DT            PIC XX.
00185
00186      12  CM-STATUS-DATA.
00187          16  CM-ENTRY-STATUS               PIC X.
00188          16  CM-ENTRY-DT                   PIC XX.
00189
00190          16  CM-LF-STATUS-AT-CANCEL        PIC X.
00191          16  CM-LF-CANCEL-DT               PIC XX.
00192          16  CM-LF-CANCEL-EXIT-DT          PIC XX.
00193
00194          16  CM-LF-STATUS-AT-DEATH         PIC X.
00195          16  CM-LF-DEATH-DT                PIC XX.
00196          16  CM-LF-DEATH-EXIT-DT           PIC XX.
00197
00198          16  CM-LF-CURRENT-STATUS          PIC X.
00199              88  CM-LF-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
00200                                                 'M' '4' '5' '9'.
00201              88  CM-LF-NORMAL-ENTRY           VALUE '1'.
00202              88  CM-LF-POLICY-PENDING         VALUE '2'.
00203              88  CM-LF-POLICY-IS-RESTORE      VALUE '3'.
00204              88  CM-LF-CONVERSION-ENTRY       VALUE '4'.
00205              88  CM-LF-POLICY-IS-REISSUE      VALUE '5'.
                   88  CM-LF-POLICY-IS-CASH         VALUE 'C'.
122002             88  CM-LF-POLICY-IS-MONTHLY      VALUE 'M'.
00206              88  CM-LF-LUMP-SUM-DISAB         VALUE '6'.
00207              88  CM-LF-DEATH-CLAIM-APPLIED    VALUE '7'.
00208              88  CM-LF-CANCEL-APPLIED         VALUE '8'.
00209              88  CM-LF-IS-REIN-ONLY           VALUE '9'.
00210              88  CM-LF-DECLINED               VALUE 'D'.
00211              88  CM-LF-VOIDED                 VALUE 'V'.
00212
00213          16  CM-AH-STATUS-AT-CANCEL        PIC X.
00214          16  CM-AH-CANCEL-DT               PIC XX.
00215          16  CM-AH-CANCEL-EXIT-DT          PIC XX.
00216
00217          16  CM-AH-STATUS-AT-SETTLEMENT    PIC X.
00218          16  CM-AH-SETTLEMENT-DT           PIC XX.
00219          16  CM-AH-SETTLEMENT-EXIT-DT      PIC XX.
00220
00221          16  CM-AH-CURRENT-STATUS          PIC X.
00222              88  CM-AH-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
00223                                                 'M' '4' '5' '9'.
00224              88  CM-AH-NORMAL-ENTRY           VALUE '1'.
00225              88  CM-AH-POLICY-PENDING         VALUE '2'.
00226              88  CM-AH-POLICY-IS-RESTORE      VALUE '3'.
00227              88  CM-AH-CONVERSION-ENTRY       VALUE '4'.
00228              88  CM-AH-POLICY-IS-REISSUE      VALUE '5'.
                   88  CM-AH-POLICY-IS-CASH         VALUE 'C'.
122002             88  CM-AH-POLICY-IS-MONTHLY      VALUE 'M'.
00229              88  CM-AH-LUMP-SUM-DISAB         VALUE '6'.
00230              88  CM-AH-DEATH-CLAIM-APPLIED    VALUE '7'.
00231              88  CM-AH-CANCEL-APPLIED         VALUE '8'.
00232              88  CM-AH-IS-REIN-ONLY           VALUE '9'.
00233              88  CM-AH-DECLINED               VALUE 'D'.
00234              88  CM-AH-VOIDED                 VALUE 'V'.
00235
00236          16  CM-CLAIM-INTERFACE-SW         PIC X.
00237              88  NO-CLAIM-ATTACHED            VALUE SPACE.
00238              88  CERT-AND-CLAIM-ONLINE        VALUE '1'.
00239              88  CERT-WAS-CREATED-FOR-CLAIM   VALUE '2'.
00240          16  CM-CLAIM-ATTACHED-COUNT       PIC S9(4)     COMP.
00241
00242          16  CM-ENTRY-BATCH                PIC X(6).
00243          16  CM-LF-EXIT-BATCH              PIC X(6).
00244          16  CM-AH-EXIT-BATCH              PIC X(6).
00245          16  CM-LAST-MONTH-END             PIC XX.
00246
00247      12  CM-NOTE-SW                        PIC X.
00248          88  CERT-NOTES-ARE-NOT-PRESENT       VALUE ' '.
00249          88  CERT-NOTES-PRESENT               VALUE '1'.
00250          88  BILLING-NOTES-PRESENT            VALUE '2'.
00251          88  CERT-BILLING-NOTES-PRESENT       VALUE '3'.
102109         88  CLAIM-NOTES-PRESENT              VALUE '4'.
102109         88  CLAIM-CERT-NOTES-PRESENT         VALUE '5'.
102109         88  CLAIM-BILLING-NOTES-PRESENT      VALUE '6'.
102109         88  CLAIM-CERT-BILL-NOTES-PRESENT    VALUE '7'.
00252      12  CM-COMP-EXCP-SW                   PIC X.
00253          88  COMPENSATION-SAME-AS-ACCT        VALUE ' '.
00254          88  THIS-CERT-HAS-ERCOMM-ENTRY       VALUE '1'.
00255      12  CM-INSURED-ADDRESS-SW             PIC X.
00256          88  INSURED-ADDR-NOT-PRESENT         VALUE ' '.
00257          88  INSURED-ADDR-PRESENT             VALUE '1'.
00258
011410*    12  CM-LF-CEDED-BENEFIT               PIC S9(7)V99   COMP-3.
011410     12  CM-LF-CLP                         PIC S9(5)V99   COMP-3.
011410     12  FILLER                            PIC X.
00260
011410*    12  CM-ISS-MICROFILM-NO               PIC S9(9)      COMP-3.
011410     12  CM-AH-CLP                         PIC S9(5)V99   COMP-3.
011410     12  FILLER                            PIC X.
072308*    12  CM-CAN-MICROFILM-NO               PIC S9(9)      COMP-3.
062017     12  CM-INT-ON-REFS                    PIC S9(7)V99   COMP-3.
00263
00264      12  CM-CREDIT-INTERFACE-SW-1          PIC X.
00265          88  CERT-ADDED-BATCH                 VALUE ' '.
00266          88  CERT-ADDED-ONLINE                VALUE '1'.
00267          88  CERT-PEND-ISSUE-ERROR            VALUE '2'.
00268          88  CERT-PURGED-OFFLINE              VALUE '3'.
00269          88  CERT-PEND-ISSUE-RETURNED         VALUE '4'.
00270      12  CM-CREDIT-INTERFACE-SW-2          PIC X.
00271          88  CERT-AS-LOADED                   VALUE ' '.
00272          88  CERT-CANCELLED-ONLINE            VALUE '1'.
00273          88  CERT-CLAIM-ONLINE                VALUE '2'.
00274          88  CERT-CLAIM-CANCEL-ONLINE         VALUE '3'.
00275          88  CERT-PEND-CANCEL-ERROR           VALUE '4'.
00276          88  CERT-PEND-CANCEL-VOID            VALUE '5'.
00277          88  CERT-PEND-CAN-VOID-ERROR         VALUE '6'.
00278          88  CERT-PEND-CANCEL-RETURNED        VALUE '7'.
00279
00280      12  CM-ACCOUNT-COMM-PCTS.
00281          16  CM-LIFE-COMM-PCT              PIC SV9(5)    COMP-3.
00282          16  CM-AH-COMM-PCT                PIC SV9(5)    COMP-3.
00283
00284      12  CM-USER-FIELD                     PIC X.
040504     12  CM-ADDL-CLP                       PIC S9(5)V99  COMP-3.
061405     12  CM-CLP-STATE                      PIC XX.
032612     12  CM-LF-CLASS-CD REDEFINES CM-CLP-STATE PIC XX.
061405     12  CM-USER-RESERVED                  PIC XXX.
032612     12  FILLER REDEFINES CM-USER-RESERVED.
032612         16  CM-AH-CLASS-CD                PIC XX.
032612         16  F                             PIC X.
00286 ******************************************************************
      *                                    COPY ERCMAIL.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCMAIL                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = MAILING DATA CAPTURE RECORDS              *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
CIDMOD*   RECORD SIZE = 374   RECFORM = FIX                            *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ERMAIL                 RKP=2,LEN=33      *
00013 *   ALTERNATE PATH    = NOT USED                                 *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
080406******************************************************************
080406*                   C H A N G E   L O G
080406*
080406* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
080406*-----------------------------------------------------------------
080406*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
080406* EFFECTIVE    NUMBER
080406*-----------------------------------------------------------------
080406* 080406    2006051800002  PEMA  ADD POST CARD MAIL INFO
090408* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
111108* 111108                   PEMA  ADD CRED BENE ADDR2
00017 ******************************************************************
00018
00019  01  MAILING-DATA.
00020      12  MA-RECORD-ID                      PIC XX.
00021          88  VALID-MA-ID                       VALUE 'MA'.
00022
00023      12  MA-CONTROL-PRIMARY.
00024          16  MA-COMPANY-CD                 PIC X.
00025          16  MA-CARRIER                    PIC X.
00026          16  MA-GROUPING.
00027              20  MA-GROUPING-PREFIX        PIC XXX.
00028              20  MA-GROUPING-PRIME         PIC XXX.
00029          16  MA-STATE                      PIC XX.
00030          16  MA-ACCOUNT.
00031              20  MA-ACCOUNT-PREFIX         PIC X(4).
00032              20  MA-ACCOUNT-PRIME          PIC X(6).
00033          16  MA-CERT-EFF-DT                PIC XX.
00034          16  MA-CERT-NO.
00035              20  MA-CERT-PRIME             PIC X(10).
00036              20  MA-CERT-SFX               PIC X.
00037
00038      12  FILLER                            PIC XX.
00039
00040      12  MA-ACCESS-CONTROL.
00041          16  MA-SOURCE-SYSTEM              PIC XX.
00042              88  MA-FROM-CREDIT                VALUE 'CR'.
00043              88  MA-FROM-VSI                   VALUE 'VS'.
00044              88  MA-FROM-WARRANTY              VALUE 'WA'.
00045              88  MA-FROM-OTHER                 VALUE 'OT'.
00046          16  MA-RECORD-ADD-DT              PIC XX.
00047          16  MA-RECORD-ADDED-BY            PIC XXXX.
00048          16  MA-LAST-MAINT-DT              PIC XX.
00049          16  MA-LAST-MAINT-BY              PIC XXXX.
00050          16  MA-LAST-MAINT-HHMMSS          PIC S9(6)       COMP-3.
00051
00052      12  MA-PROFILE-INFO.
00053          16  MA-QUALIFY-CODE-1             PIC XX.
00054          16  MA-QUALIFY-CODE-2             PIC XX.
00055          16  MA-QUALIFY-CODE-3             PIC XX.
00056          16  MA-QUALIFY-CODE-4             PIC XX.
00057          16  MA-QUALIFY-CODE-5             PIC XX.
00058
00059          16  MA-INSURED-LAST-NAME          PIC X(15).
00060          16  MA-INSURED-FIRST-NAME         PIC X(10).
00061          16  MA-INSURED-MIDDLE-INIT        PIC X.
00062          16  MA-INSURED-ISSUE-AGE          PIC 99.
00063          16  MA-INSURED-BIRTH-DT           PIC XX.
00064          16  MA-INSURED-SEX                PIC X.
00065              88  MA-SEX-MALE                   VALUE 'M'.
00066              88  MA-SEX-FEMALE                 VALUE 'F'.
00067          16  MA-INSURED-SOC-SEC-NO         PIC X(11).
00068
080406         16  MA-ADDRESS-CORRECTED          PIC X.
081108         16  MA-JOINT-BIRTH-DT             PIC XX.
00069 *        16  FILLER                        PIC X(12).
00070
00071          16  MA-ADDRESS-LINE-1             PIC X(30).
00072          16  MA-ADDRESS-LINE-2             PIC X(30).
00073          16  MA-CITY-STATE.
                   20  MA-CITY                   PIC X(28).
                   20  MA-ADDR-STATE             PIC XX.
00074          16  MA-ZIP.
00075              20  MA-ZIP-CODE.
00076                  24  MA-ZIP-CODE-1ST       PIC X(1).
00077                      88  MA-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00078                  24  FILLER                PIC X(4).
00079              20  MA-ZIP-PLUS4              PIC X(4).
00080          16  MA-CANADIAN-POSTAL-CODE REDEFINES MA-ZIP.
00081              20  MA-CAN-POSTAL-CODE-1      PIC X(3).
00082              20  MA-CAN-POSTAL-CODE-2      PIC X(3).
00083              20  FILLER                    PIC X(3).
00084
00085          16  MA-PHONE-NO                   PIC 9(11)       COMP-3.
00086
               16  FILLER                        PIC XXX.
00087 *        16  FILLER                        PIC X(10).
00088
           12  MA-CRED-BENE-INFO.
CIDMOD         16  MA-CRED-BENE-NAME                 PIC X(25).
CIDMOD         16  MA-CRED-BENE-ADDR                 PIC X(30).
               16  MA-CRED-BENE-ADDR2                PIC X(30).
CIDMOD         16  MA-CRED-BENE-CTYST.
                   20  MA-CRED-BENE-CITY             PIC X(28).
                   20  MA-CRED-BENE-STATE            PIC XX.
CIDMOD         16  MA-CRED-BENE-ZIP.
CIDMOD             20  MA-CB-ZIP-CODE.
CIDMOD                 24  MA-CB-ZIP-CODE-1ST        PIC X(1).
CIDMOD                     88  MA-CB-CANADIAN-POST-CODE
                                                 VALUE 'A' THRU 'Z'.
CIDMOD                 24  FILLER                    PIC X(4).
CIDMOD             20  MA-CB-ZIP-PLUS4               PIC X(4).
CIDMOD         16  MA-CB-CANADIAN-POSTAL-CODE
                                  REDEFINES MA-CRED-BENE-ZIP.
CIDMOD             20  MA-CB-CAN-POSTAL-CODE-1       PIC X(3).
CIDMOD             20  MA-CB-CAN-POSTAL-CODE-2       PIC X(3).
CIDMOD             20  FILLER                        PIC X(3).
080406     12  MA-POST-CARD-MAIL-DATA.
080406         16  MA-MAIL-DATA OCCURS 7.
080406             20  MA-MAIL-TYPE              PIC X.
080406                 88  MA-12MO-MAILING           VALUE '1'.
080406                 88  MA-EXP-MAILING            VALUE '2'.
080406             20  MA-MAIL-STATUS            PIC X.
080406                 88  MA-MAIL-ST-MAILED         VALUE '1'.
080406                 88  MA-MAIL-ST-RETURNED       VALUE '2'.
080406                 88  MA-MAIL-ST-NOT-MAILED     VALUE '3'.
080406             20  MA-MAIL-DATE              PIC XX.
080406     12  FILLER                            PIC XX.
           12  FILLER                            PIC XX.
080406*    12  FILLER                            PIC X(30).
00090 ******************************************************************
      *                                    COPY ELCMSTR.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCMSTR.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.012                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CLAIM MASTER FILE                         *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 350  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELMSTR                         RKP=2,LEN=20   *
00013 *       ALTERNATE PATH1 = ELMSTR2 (BY NAME)       RKP=22,LEN=29  *
00014 *       ALTERNATE PATH2 = ELMSTR3 (BY SOC SEC NO) RKP=51,LEN=12  *
00015 *       ALTERNATE PATH3 = ELMSTR5 (BY CERT NO)    RKP=63,LEN=12  *
00016 *       ALTERNATE PATH4 = ELMSTR6 (BY CREDIT CARD NO)            *
00017 *                                                 RKP=75,LEN=21  *
00018 *                                                                *
00019 *   **** NOTE ****                                               *
00020 *             ANY CHANGES TO THIS COPYBOOK MUST ALSO BE          *
00021 *             IMPLEMENTED IN COPYBOOK ELCRETR (RETRIEVE MASTER)  *
00022 *                                                                *
00023 *   LOG = YES                                                    *
00024 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
120503******************************************************************
120503*                   C H A N G E   L O G
120503*
120503* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
120503*-----------------------------------------------------------------
120503*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
120503* EFFECTIVE    NUMBER
120503*-----------------------------------------------------------------
120503* 120503    2003080800002  SMVA  INITIAL SECURE PAY CHANGES
080307* 080307    2007032100001  PEMA  ADD TOTAL INTEREST PAID FIELD
031213* 031213    2012113000002  PEMA  ADD ACCIDENT INDICATOR
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
081817* 081817    2016100700001  TANA  ADD NBR OF EXTENSIONS
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
00025 ******************************************************************
00026  01  CLAIM-MASTER.
00027      12  CL-RECORD-ID                PIC XX.
00028          88  VALID-CL-ID         VALUE 'CL'.
00029
00030      12  CL-CONTROL-PRIMARY.
00031          16  CL-COMPANY-CD           PIC X.
00032          16  CL-CARRIER              PIC X.
00033          16  CL-CLAIM-NO             PIC X(7).
00034          16  CL-CERT-NO.
00035              20  CL-CERT-PRIME       PIC X(10).
00036              20  CL-CERT-SFX         PIC X.
00037
00038      12  CL-CONTROL-BY-NAME.
00039          16  CL-COMPANY-CD-A1        PIC X.
00040          16  CL-INSURED-LAST-NAME    PIC X(15).
00041          16  CL-INSURED-NAME.
00042              20  CL-INSURED-1ST-NAME PIC X(12).
00043              20  CL-INSURED-MID-INIT PIC X.
00044
00045      12  CL-CONTROL-BY-SSN.
00046          16  CL-COMPANY-CD-A2        PIC X.
00047          16  CL-SOC-SEC-NO.
00048              20  CL-SSN-STATE        PIC XX.
00049              20  CL-SSN-ACCOUNT      PIC X(6).
00050              20  CL-SSN-LN3          PIC X(3).
00051
00052      12  CL-CONTROL-BY-CERT-NO.
00053          16  CL-COMPANY-CD-A4        PIC X.
00054          16  CL-CERT-NO-A4.
00055              20  CL-CERT-A4-PRIME    PIC X(10).
00056              20  CL-CERT-A4-SFX      PIC X.
00057
00058      12  CL-CONTROL-BY-CCN.
00059          16  CL-COMPANY-CD-A5        PIC X.
00060          16  CL-CCN-A5.
00061              20  CL-CCN.
00062                  24  CL-CCN-PREFIX-A5 PIC X(4).
00063                  24  CL-CCN-PRIME-A5 PIC X(12).
00064              20  CL-CCN-FILLER-A5    PIC X(4).
00065
00066      12  CL-INSURED-PROFILE-DATA.
00067          16  CL-INSURED-BIRTH-DT     PIC XX.
00068          16  CL-INSURED-SEX-CD       PIC X.
00069              88  INSURED-IS-MALE        VALUE 'M'.
00070              88  INSURED-IS-FEMALE      VALUE 'F'.
00071              88  INSURED-SEX-UNKNOWN    VALUE ' '.
00072          16  CL-INSURED-OCC-CD       PIC X(6).
00073          16  FILLER                  PIC X(5).
00074
00075      12  CL-PROCESSING-INFO.
00076          16  CL-PROCESSOR-ID         PIC X(4).
00077          16  CL-CLAIM-STATUS         PIC X.
00078              88  CLAIM-IS-OPEN          VALUE 'O'.
00079              88  CLAIM-IS-CLOSED        VALUE 'C'.
00080          16  CL-CLAIM-TYPE           PIC X.
00081 *            88  AH-CLAIM               VALUE 'A'.
00082 *            88  LIFE-CLAIM             VALUE 'L'.
00083 *            88  PROPERTY-CLAIM         VALUE 'P'.
00084 *            88  IUI-CLAIM              VALUE 'I'.
120503*            88  GAP-CLAIM              VALUE 'G'.
052614*            88  FAMILY-LEAVE-CLAIM     VALUE 'F'.
100518*            88  OTHER-CLAIM            VALUE 'O'.
00085          16  CL-CLAIM-PREM-TYPE      PIC X.
00086              88  SINGLE-PREMIUM         VALUE '1'.
00087              88  O-B-COVERAGE           VALUE '2'.
00088              88  OPEN-END-COVERAGE      VALUE '3'.
00089          16  CL-INCURRED-DT          PIC XX.
00090          16  CL-REPORTED-DT          PIC XX.
00091          16  CL-FILE-ESTABLISH-DT    PIC XX.
00092          16  CL-EST-END-OF-DISAB-DT  PIC XX.
00093          16  CL-LAST-PMT-DT          PIC XX.
00094          16  CL-LAST-PMT-AMT         PIC S9(7)V99  COMP-3.
00095          16  CL-PAID-THRU-DT         PIC XX.
00096          16  CL-TOTAL-PAID-AMT       PIC S9(7)V99  COMP-3.
00097          16  CL-NO-OF-PMTS-MADE      PIC S9(3)     COMP-3.
00098          16  CL-NO-OF-DAYS-PAID      PIC S9(4)     COMP.
00099          16  CL-PMT-CALC-METHOD      PIC X.
00100              88  CL-360-DAY-YR          VALUE '1'.
00101              88  CL-365-DAY-YR          VALUE '2'.
00102              88  CL-FULL-MONTHS         VALUE '3'.
00103          16  CL-CAUSE-CD             PIC X(6).
00104
00105          16  CL-PRIME-CERT-NO.
00106              20  CL-PRIME-CERT-PRIME PIC X(10).
00107              20  CL-PRIME-CERT-SFX   PIC X.
00108
00109          16  CL-SYSTEM-IDENTIFIER    PIC XX.
00110              88  CL-CREDIT-CLAIM        VALUE 'CR'.
00111              88  CL-CONVENIENCE-CLAIM   VALUE 'CV'.
00112
00113          16  CL-MICROFILM-NO         PIC X(10).
051414         16  FILLER REDEFINES CL-MICROFILM-NO.
051414             20  CL-BENEFIT-PERIOD   PIC 99.
051414             20  FILLER              PIC X(8).
00114          16  CL-PROG-FORM-TYPE       PIC X.
00115          16  CL-LAST-ADD-ON-DT       PIC XX.
00116
00117          16  CL-LAST-REOPEN-DT       PIC XX.
00118          16  CL-LAST-CLOSE-DT        PIC XX.
00119          16  CL-LAST-CLOSE-REASON    PIC X(01).
00120              88  FINAL-PAID             VALUE '1'.
00121              88  CLAIM-DENIED           VALUE '2'.
00122              88  AUTO-CLOSE             VALUE '3'.
00123              88  MANUAL-CLOSE           VALUE '4'.
00124              88  BENEFITS-CHANGED       VALUE 'C'.
00125              88  SETUP-ERRORS           VALUE 'E'.
00126          16  CL-ASSOC-CERT-SEQU      PIC S99.
00127          16  CL-ASSOC-CERT-TOTAL     PIC S99.
00128          16  CL-CLAIM-PAYMENT-STATUS PIC 9.
00129              88  PAYMENT-IN-PREP        VALUE 1 THRU 9.
080307         16  CL-TOTAL-INT-PAID       PIC S9(5)V99 COMP-3.
080307         16  FILLER                  PIC X.
00131
00132      12  CL-CERTIFICATE-DATA.
00133          16  CL-CERT-ORIGIN          PIC X.
00134              88  CERT-WAS-ONLINE        VALUE '1'.
00135              88  CERT-WAS-CREATED       VALUE '2'.
00136              88  COVERAGE-WAS-ADDED     VALUE '3'.
00137          16  CL-CERT-KEY-DATA.
00138              20  CL-CERT-CARRIER     PIC X.
00139              20  CL-CERT-GROUPING    PIC X(6).
00140              20  CL-CERT-STATE       PIC XX.
00141              20  CL-CERT-ACCOUNT.
00142                  24  CL-CERT-ACCOUNT-PREFIX PIC X(4).
00143                  24  CL-CERT-ACCOUNT-PRIME  PIC X(6).
00144              20  CL-CERT-EFF-DT      PIC XX.
00145
00146      12  CL-STATUS-CONTROLS.
00147          16  CL-PRIORITY-CD          PIC X.
00148              88  CONFIDENTIAL-DATA      VALUE '8'.
00149              88  HIGHEST-PRIORITY       VALUE '9'.
00150          16  CL-SUPV-ATTN-CD         PIC X.
00151              88  SUPV-NOT-REQUIRED      VALUE ' ' 'N'.
00152              88  SUPV-IS-REQUIRED       VALUE 'Y'.
00153          16  CL-PURGED-DT            PIC XX.
00154          16  CL-RESTORED-DT          PIC XX.
00155          16  CL-NEXT-AUTO-PAY-DT     PIC XX.
00156          16  CL-NEXT-RESEND-DT       PIC XX.
00157          16  CL-NEXT-FOLLOWUP-DT     PIC XX.
031213         16  CL-CRITICAL-PERIOD      PIC 99.
031213*        16  FILLER                  PIC XX.
00159          16  CL-LAST-MAINT-DT        PIC XX.
00160          16  CL-LAST-MAINT-USER      PIC X(4).
00161          16  CL-LAST-MAINT-HHMMSS    PIC S9(6)     COMP-3.
00162          16  CL-LAST-MAINT-TYPE      PIC X.
00163              88  CLAIM-SET-UP           VALUE ' '.
00164              88  PAYMENT-MADE           VALUE '1'.
00165              88  LETTER-SENT            VALUE '2'.
00166              88  MASTER-WAS-ALTERED     VALUE '3'.
00167              88  MASTER-WAS-RESTORED    VALUE '4'.
00168              88  INCURRED-DATE-CHANGED  VALUE '5'.
00169              88  FILE-CONVERTED         VALUE '6'.
00170              88  CHANGE-OF-BENEFITS     VALUE 'C'.
00171              88  ERROR-CORRECTION       VALUE 'E'.
00172          16  CL-RELATED-CLAIM-NO     PIC X(7).
00173          16  CL-HISTORY-ARCHIVE-DT   PIC XX.
00174          16  CL-BENEFICIARY          PIC X(10).
00175          16  CL-FILE-ESTABLISHED-BY  PIC X(4).
120808         16  CL-DENIAL-TYPE          PIC X.
                   88  CL-TYPE-DENIAL          VALUE '1'.
                   88  CL-TYPE-RESCISSION      VALUE '2'.
                   88  CL-TYPE-REFORMATION     VALUE '3'.
                   88  CL-TYPE-REF-TO-RES      VALUE '4'.
                   88  CL-TYPE-RECONSIDERED    VALUE '5'.
081817         16  CL-NO-OF-EXTENSIONS     PIC 99.
081817         16  filler                  pic x(3).
      *        16  CL-CRIT-PER-RECURRENT   PIC X.
      *        16  CL-CRIT-PER-RTW-MOS     PIC 99.
      *        16  CL-RTW-DT               PIC XX.
00177
00178      12  CL-TRAILER-CONTROLS.
00179          16  CL-TRAILER-SEQ-CNT      PIC S9(4)     COMP.
00180              88  CL-1ST-TRL-AVAIL       VALUE +4095.
00181              88  CL-LAST-TRL-AVAIL      VALUE +100.
00182              88  CL-RESV-EXP-HIST-TRLR  VALUE +0.
00183          16  CL-LAST-INC-DT-CHANGE   PIC S9(4)     COMP.
00184          16  FILLER                  PIC XX.
00185          16  CL-AUTO-PAY-SEQ         PIC S9(4)     COMP.
00186          16  CL-ADDRESS-TRAILER-CNT.
00187              20  CL-INSURED-ADDR-CNT  PIC S9(1).
00188                  88  NO-INSURED-AVAILABLE    VALUE ZERO.
00189              20  CL-ACCOUNT-ADDR-CNT  PIC S9(1).
00190                  88  ACCOUNT-IS-ONLINE       VALUE ZERO.
00191              20  CL-BENIF-ADDR-CNT    PIC S9(1).
00192                  88  BENEFICIARY-IS-ONLINE   VALUE ZERO.
00193              20  CL-EMPLOYER-ADDR-CNT PIC S9(1).
00194                  88  NO-EMPLOY-AVAILABLE     VALUE ZERO.
00195              20  CL-DOCTOR-ADDR-CNT   PIC S9(1).
00196                  88  NO-DOCTOR-AVAILABLE     VALUE ZERO.
00197              20  CL-OTHER-1-ADDR-CNT  PIC S9(1).
00198                  88  NO-OTHER-1-ADDRESSES    VALUE ZERO.
00199              20  CL-OTHER-2-ADDR-CNT  PIC S9(1).
00200                  88  NO-OTHER-2-ADDRESSES    VALUE ZERO.
00201
00202      12  CL-CV-REFERENCE-NO.
00203          16  CL-CV-REFNO-PRIME       PIC X(18).
00204          16  CL-CV-REFNO-SFX         PIC XX.
00205
00206      12  CL-FILE-LOCATION            PIC X(4).
00207
00208      12  CL-PROCESS-ERRORS.
00209          16  CL-FATAL-ERROR-CNT      PIC S9(4)     COMP.
00210              88  NO-FATAL-ERRORS        VALUE ZERO.
00211          16  CL-FORCEABLE-ERROR-CNT  PIC S9(4)     COMP.
00212              88  NO-FORCABLE-ERRORS     VALUE ZERO.
00213
00214      12  CL-PRODUCT-CD               PIC X.
00215
00216      12  CL-CURRENT-KEY-DATA.
00217          16  CL-CURRENT-CARRIER      PIC X.
00218          16  CL-CURRENT-GROUPING     PIC X(6).
00219          16  CL-CURRENT-STATE        PIC XX.
00220          16  CL-CURRENT-ACCOUNT      PIC X(10).
00221
00222      12  CL-ASSOCIATES               PIC X.
00223          88  CL-ASSOC-NO-INTERFACE      VALUE 'A'.
00224          88  CL-ASSOC-INTERFACE         VALUE 'I'.
00225          88  CL-NON-ASSOC-NO-INTERFACE  VALUE 'N'.
00226          88  CL-NON-ASSOC-INTERFACE     VALUE 'M'.
00227
00228      12  CL-ACTIVITY-CODE            PIC 99.
00229      12  CL-ACTIVITY-MAINT-DT        PIC XX.
00230      12  CL-ACTIVITY-MAINT-TYPE      PIC X(4).
00231
00232      12  CL-LAPSE-REPORT-CODE        PIC 9.
00233      12  CL-LAG-REPORT-CODE          PIC 9.
00234      12  CL-LOAN-TYPE                PIC XX.
00235      12  CL-LEGAL-STATE              PIC XX.
00236
CIDMOD     12  CL-YESNOSW                  PIC X.
031213     12  CL-ACCIDENT-CLAIM-SW        PIC X.
031213         88  CL-ACCIDENT-NOT-SET           VALUE ' '.
031213         88  CL-CLAIM-DUE-TO-ACCIDENT      VALUE 'Y'.
031213         88  CL-CLAIM-NOT-DUE-TO-ACCIDENT  VALUE 'N'.
051414     12  cl-insured-type             pic x.
051414         88  cl-claim-on-primary         value 'P'.
051414         88  cl-claim-on-co-borrower     value 'C'.
031213     12  cl-benefit-expiration-dt    PIC XX.
040909*                                    COPY ELCCRTT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCCRTT.                            *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CERTIFICATE TRAILERS                      *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 552  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCRTT                         RKP=2,LEN=34   *
00017 *                                                                *
00018 *   LOG = YES                                                    *
00019 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
111204******************************************************************
111204*                   C H A N G E   L O G
111204*
111204* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
111204*-----------------------------------------------------------------
111204*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
111204* EFFECTIVE    NUMBER
111204*-----------------------------------------------------------------
111204* 111204                   PEMA  NEW FILE TO SPLIT BANK COMM
040109* 040109  2009031600001    AJRA  ADD NEW TRAILER TYPE AND REDEFINE
012010* 012010  2009061500002    AJRA  ADD FLAG FOR REFUND WITH OPEN CLA
121712* 121712  CR2012101700002  AJRA  ADD DEFAULT AGE FLAG
061013* 061013  CR2012113000002  PEMA  SPP CLAIM RELATED CHANGES
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052614* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
022715* 022715  CR2015010800003  PEMA  AGENT SIGNATURE
020816* 020816  CR2015082500001  PEMA  ADD NEW VPP COMPANY
012918* 012918  CR2017062000002  PEMA  AUDIT NB FOR PREV CLAIMS
091318* 091318  CR2018073000001  PEMA  ADD Refund methods
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
111204******************************************************************
00021
00022  01  CERTIFICATE-TRAILERS.
00023      12  CS-RECORD-ID                      PIC XX.
00024          88  VALID-CS-ID                      VALUE 'CS'.
00025
00026      12  CS-CONTROL-PRIMARY.
00027          16  CS-COMPANY-CD                 PIC X.
00028          16  CS-CARRIER                    PIC X.
00029          16  CS-GROUPING                   PIC X(6).
00032          16  CS-STATE                      PIC XX.
00033          16  CS-ACCOUNT                    PIC X(10).
00036          16  CS-CERT-EFF-DT                PIC XX.
00037          16  CS-CERT-NO.
00038              20  CS-CERT-PRIME             PIC X(10).
00039              20  CS-CERT-SFX               PIC X.
               16  CS-TRAILER-TYPE               PIC X.
                   88  COMM-TRLR           VALUE 'A'.
061013             88  CLAIM-HISTORY-TRLR  VALUE 'B'.
040109             88  CERT-DATA-TRLR      VALUE 'C'.
00040
040109     12  CS-DATA-AREA                      PIC X(516).
040109
040109     12  CS-BANK-COMMISSIONS REDEFINES CS-DATA-AREA.
040109         16  CS-BANK-COMMISSION-AREA.
040109             20  CS-BANK-COMMS       OCCURS 10.
040109                 24  CS-AGT                PIC X(10).
040109                 24  CS-COM-TYP            PIC X.
040109                 24  CS-SPP-FEES           PIC S9(5)V99   COMP-3.
040109                 24  CS-RECALC-LV-INDIC    PIC X.
040109                 24  FILLER                PIC X(10).
040109         16  FILLER                        PIC X(256).
040109
061013     12  CS-CLAIM-HISTORY-TRAILER REDEFINES CS-DATA-AREA.
061013****  TO CALC NO OF BENEFITS PAID = (CS-DAYS-PAID / 30)
               16  CS-MB-CLAIM-DATA OCCURS 24.
                   20  CS-CLAIM-NO               PIC X(7).
                   20  CS-CLAIM-TYPE             PIC X.
                       88  CS-AH-CLM               VALUE 'A'.
                       88  CS-IU-CLM               VALUE 'I'.
                       88  CS-GP-CLM               VALUE 'G'.
                       88  CS-LF-CLM               VALUE 'L'.
                       88  CS-PR-CLM               VALUE 'P'.
052614                 88  CS-FL-CLM               VALUE 'F'.
100518                 88  CS-OT-CLM               VALUE 'O'.
                   20  CS-INSURED-TYPE           PIC X.
                       88  CS-PRIM-INSURED          VALUE 'P'.
                       88  CS-CO-BORROWER           VALUE 'C'.
                   20  CS-BENEFIT-PERIOD         PIC 99.
                   20  CS-DAYS-PAID              PIC S9(5) COMP-3.
                   20  CS-TOTAL-PAID             PIC S9(7)V99 COMP-3.
                   20  CS-REMAINING-BENS         PIC S999 COMP-3.
               16  FILLER                        PIC X(12).
040109     12  CS-CERT-DATA REDEFINES CS-DATA-AREA.
040109         16  CS-VIN-NUMBER                 PIC X(17).
012010         16  CS-REFUND-CLAIM-FLAG          PIC X(01).
121712         16  CS-INS-AGE-DEFAULT-FLAG       PIC X(01).
121712         16  CS-JNT-AGE-DEFAULT-FLAG       PIC X(01).
022715         16  cs-agent-name.
022715             20  cs-agent-fname            pic x(20).
022715             20  cs-agent-mi               pic x.
022715             20  cs-agent-lname            pic x(25).
022715         16  cs-license-no                 pic x(15).
022715         16  cs-npn-number                 pic x(10).
022715         16  cs-agent-edit-status          pic x.
022715             88  cs-ae-refer-to-manager      value 'M'.
022715             88  cs-ae-cover-sheet           value 'C'.
022715             88  cs-ae-sig-form              value 'S'.
022715             88  cs-ae-verified              value 'V'.
022715             88  cs-unidentified-signature   value 'U'.
022715             88  cs-cert-returned            value 'R'.
022715             88  cs-accept-no-commission     value 'N'.
020816         16  cs-year                       pic 9999.
020816         16  cs-make                       pic x(20).
020816         16  cs-model                      pic x(20).
020816         16  cs-future                     pic x(20).
020816         16  cs-vehicle-odometer           pic s9(7) comp-3.
012918         16  cs-claim-verification-status  pic x.
012918             88  cs-clm-ver-eligible         value 'A'.
012918             88  cs-clm-ver-partial-elig     value 'B'.
012918             88  cs-clm-ver-not-eligible     value 'C'.
012918             88  cs-clm-ver-not-elig-opn-clm value 'D'.
012918             88  cs-clm-ver-not-part-elig-rw value 'E'.
012918             88  cs-clm-ver-ND-CERT          value 'F'.
012918             88  cs-clm-ver-spec-other       value 'G'.
012918             88  cs-clam-ver-pratial-corrected
012918                                             value 'H'.
012918             88  cs-clm-ver-no-matches       value 'I'.
012918             88  cs-clm-ver-not-elig-corrected
012918                                             value 'J'.
012918             88  cs-clm-ver-needs-review     value 'R'.
012918             88  cs-clm-ver-sent-to-claims   value 'W'.
091318         16  CS-LF-REFUND-METHOD           PIC X.
091318         16  CS-AH-REFUND-METHOD           PIC X.
020816         16  FILLER                        PIC X(353). *> was 420
121712*        16  FILLER                        PIC X(496).
010412*                                    COPY ELCCRTO.
      ******************************************************************
      *                                                                *
      *                            ELCCRTO.                            *
      *                                                                *
      *   FILE DESCRIPTION = ORIGINAL CERTIFICATE INFORMATION          *
      *                                                                *
      *   FILE TYPE = VSAM,KSDS                                        *
      *   RECORD SIZE = 524  RECFORM = FIXED                           *
      *                                                                *
      *   BASE CLUSTER = ELCRTO                         RKP=2,LEN=36   *
      *                                                                *
      *   LOG = YES                                                    *
      *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
061011* 061011  2011022800001    PEMA  NEW FILE TO SAVE ORIG CERT INFO
062712* 062712  2011022800001    AJRA  REDEFINE ORIG DATA
071712* 071712  CR2011022800001  AJRA  NAPERSOFT CANCELS
121812* 121812  CR2012101700002  AJRA  ADD DEFAULT AGE FLAG
011413* 011413  IR2012122700003  AJRA  ADD CRTO ISSUE/CANCEL INDICATOR
      ******************************************************************
       01  ORIGINAL-CERTIFICATE.
           12  OC-RECORD-ID                      PIC XX.
               88  VALID-OC-ID                      VALUE 'OC'.
           12  OC-CONTROL-PRIMARY.
               16  OC-COMPANY-CD                 PIC X.
               16  OC-CARRIER                    PIC X.
               16  OC-GROUPING                   PIC X(6).
               16  OC-STATE                      PIC XX.
               16  OC-ACCOUNT                    PIC X(10).
               16  OC-CERT-EFF-DT                PIC XX.
               16  OC-CERT-NO.
                   20  OC-CERT-PRIME             PIC X(10).
                   20  OC-CERT-SFX               PIC X.
               16  OC-RECORD-TYPE                PIC X.
               16  OC-KEY-SEQ-NO                 PIC 9(4) BINARY.
           12  OC-LAST-MAINT-DT                  PIC XX.
           12  OC-LAST-MAINT-BY                  PIC X(4).
           12  OC-LAST-MAINT-HHMMSS              PIC S9(6)   COMP-3.
           12  OC-ENDORSEMENT-PROCESSED-DT       PIC XX.
           12  FILLER                            PIC X(49).
062712     12  OC-ORIG-REC.
062712         16  OC-INS-LAST-NAME              PIC X(15).
062712         16  OC-INS-FIRST-NAME             PIC X(10).
062712         16  OC-INS-MIDDLE-INIT            PIC X.
062712         16  OC-INS-AGE                    PIC S999     COMP-3.
062712         16  OC-JNT-LAST-NAME              PIC X(15).
062712         16  OC-JNT-FIRST-NAME             PIC X(10).
062712         16  OC-JNT-MIDDLE-INIT            PIC X.
062712         16  OC-JNT-AGE                    PIC S999     COMP-3.
062712         16  OC-LF-BENCD                   PIC XX.
062712         16  OC-LF-TERM                    PIC S999      COMP-3.
062712         16  OC-LF-BEN-AMT                 PIC S9(9)V99  COMP-3.
062712         16  OC-LF-PRM-AMT                 PIC S9(7)V99  COMP-3.
062712         16  OC-LF-ALT-BEN-AMT             PIC S9(9)V99  COMP-3.
062712         16  OC-LF-ALT-PRM-AMT             PIC S9(7)V99  COMP-3.
062712         16  OC-LF-EXP-DT                  PIC XX.
062712         16  OC-LF-COMM-PCT                PIC SV9(5)    COMP-3.
062712         16  OC-LF-CANCEL-DT               PIC XX.
062712         16  OC-LF-CANCEL-AMT              PIC S9(7)V99  COMP-3.
071712         16  OC-LF-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
062712         16  OC-AH-BENCD                   PIC XX.
062712         16  OC-AH-TERM                    PIC S999      COMP-3.
062712         16  OC-AH-BEN-AMT                 PIC S9(9)V99  COMP-3.
062712         16  OC-AH-PRM-AMT                 PIC S9(7)V99  COMP-3.
062712         16  OC-AH-EXP-DT                  PIC XX.
062712         16  OC-AH-COMM-PCT                PIC SV9(5)    COMP-3.
062712         16  OC-AH-CP                      PIC 99.
062712         16  OC-AH-CANCEL-DT               PIC XX.
062712         16  OC-AH-CANCEL-AMT              PIC S9(7)V99  COMP-3.
071712         16  OC-AH-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
062712         16  OC-CRED-BENE-NAME             PIC X(25).
062712         16  OC-1ST-PMT-DT                 PIC XX.
121812         16  OC-INS-AGE-DEFAULT-FLAG       PIC X.
121812         16  OC-JNT-AGE-DEFAULT-FLAG       PIC X.
011413         16  OC-ISSUE-TRAN-IND             PIC X.
011413         16  OC-CANCEL-TRAN-IND            PIC X.
011413         16  FILLER                        PIC X(211).
062712
062712     12  FILLER                            PIC X(50).
      ******************************************************************
010412*                                    COPY ERCPNDB.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCPNDB.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.025                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = PENDING NEW BUSINESS (ISSUES AND CANCELS) *
00008 *                                                                *
00009 ******************************************************************
00010 *   NOTE: IF THIS FORMAT IS CHANGED, THE SORT RECORD IN THE      *
00011 *         EL861 A/R SYSTEM MAY NEED TO BE CHANGED.               *
00012 ******************************************************************
00013 *                                                                *
00014 *                                                                *
00015 *   FILE TYPE = VSAM,KSDS                                        *
00016 *   RECORD SIZE = 585  RECFORM = FIXED                           *
00017 *                                                                *
00018 *   BASE CLUSTER = ERPNDB                         RKP=2,LEN=11   *
00019 *       ALTERNATE PATH1 = ERPNDB2  (BY CAR GRP STATE ACCOUNT     *
00020 *                                  EFF-DT CERT CHG-SEQ REC-TYPE) *
00021 *                                                 RKP=13,LEN=36  *
00022 *       ALTERNATE PATH2 = ERPNDB3  (BY CO, ORIGINAL BATCH, SEQ.  *
00023 *                                      AND CHG-SEQ.)             *
00024 *                                                RKP=49,LEN=11   *
00025 *       ALTERNATE PATH3 = ERPNDB4  (BY CO, CSR-ID, BATCH, SEQ.   *
00026 *                                      AND CHG-SEQ.)             *
00027 *                                                RKP=60,LEN=15   *
00028 *                                                                *
00029 *   LOG = NO                                                     *
00030 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00031 ******************************************************************
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
011904* 011904                   PEMA  ADD TOTAL FEE PROCESSING
040504* 040504    2003080800002  PEMA  ADD DEALER INCENTIVE PROCESSING
020305* 020305    2005020000000  PEMA  ADD CLP STATE TO PNDB RECORD
110105* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
032306* 032306                   PEMA  ADD BOW LOAN NUMBER
081606* 081606    2006080800002  PEMA  ADD VIN, ISSUES ONLY
073107* 073107  CR2006051600002  PEMA  ADD OVERCHARGE PROCESSING
072308* 072308  CR2007110500003  PEMA  ADD NH REFUND INTEREST PROCESSING
090408* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
032109* 032109    2009021700002  PEMA  ADD CRED BENE TO PNDB REC
072209* 072209  CR2008101500003  AJRA  ADD BORROWER FIRST NAME TO ENDORS
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
071211* 071211  CR2010012700001  PEMA  ADD SPP DEALER DIRECT
073114* 073114  CR2014012300001  PEMA  ADD CU CARRIER 7 PROCESSING
010716* 010716    2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
010517* 010517  CR2016021600005  PEMA ADD NEW FORCE CODE FOR AGG
062017* 062017  CR2015091000001  PEMA RENAME INTEREST FIELD
122002******************************************************************
00032
00033  01  PENDING-BUSINESS.
00034      12  PB-RECORD-ID                     PIC XX.
00035          88  VALID-PB-ID                        VALUE 'PB'.
00036
00037      12  PB-CONTROL-PRIMARY.
00038          16  PB-COMPANY-CD                PIC X.
00039          16  PB-ENTRY-BATCH               PIC X(6).
00040          16  PB-BATCH-SEQ-NO              PIC S9(4)     COMP.
00041          16  PB-BATCH-CHG-SEQ-NO          PIC S9(4)     COMP.
00042
00043      12  PB-CONTROL-BY-ACCOUNT.
00044          16  PB-COMPANY-CD-A1             PIC X.
00045          16  PB-CARRIER                   PIC X.
00046          16  PB-GROUPING.
00047              20  PB-GROUPING-PREFIX       PIC XXX.
00048              20  PB-GROUPING-PRIME        PIC XXX.
00049          16  PB-STATE                     PIC XX.
00050          16  PB-ACCOUNT.
00051              20  PB-ACCOUNT-PREFIX        PIC X(4).
00052              20  PB-ACCOUNT-PRIME         PIC X(6).
00053          16  PB-CERT-EFF-DT               PIC XX.
00054          16  PB-CERT-NO.
00055              20  PB-CERT-PRIME            PIC X(10).
00056              20  PB-CERT-SFX              PIC X.
00057          16  PB-ALT-CHG-SEQ-NO            PIC S9(4)     COMP.
00058
00059          16  PB-RECORD-TYPE               PIC X.
00060              88  PB-MAILING-DATA                VALUE '0'.
00061              88  PB-ISSUE                       VALUE '1'.
00062              88  PB-CANCELLATION                VALUE '2'.
00063              88  PB-BATCH-TRAILER               VALUE '9'.
00064
00065      12  PB-CONTROL-BY-ORIG-BATCH.
00066          16  PB-ORIGINAL-COMPANY-CD       PIC X.
00067          16  PB-ORIGINAL-ENTRY-BATCH      PIC X(6).
00068          16  PB-ORIGINAL-SEQ-NO           PIC S9(4)     COMP.
00069          16  PB-ORIGINAL-CHG-SEQ-NO       PIC S9(4)     COMP.
00070
00071      12  PB-CONTROL-BY-CSR.
00072          16  PB-CSR-COMPANY-CD            PIC X.
00073          16  PB-CSR-ID                    PIC X(4).
00074          16  PB-CSR-ENTRY-BATCH           PIC X(6).
00075          16  PB-CSR-BATCH-SEQ-NO          PIC S9(4)     COMP.
00076          16  PB-CSR-BATCH-CHG-SEQ-NO      PIC S9(4)     COMP.
00077 ******************************************************************
00078 *    MAILING DATA IS PROCESSED IN ONLY PRGS. EL512 & EL513       *
00079 ******************************************************************
00080
00081      12  PB-LAST-MAINT-DT                 PIC XX.
00082      12  PB-LAST-MAINT-BY                 PIC X(4).
00083      12  PB-LAST-MAINT-HHMMSS             PIC S9(6)     COMP-3.
00084
00085      12  PB-RECORD-BODY                   PIC X(375).
00086
00087      12  PB-ISSUE-RECORD   REDEFINES PB-RECORD-BODY.
00088          16  PB-CERT-ORIGIN               PIC X.
00089              88  CLASIC-CREATED-CERT         VALUE '1'.
00090          16  PB-I-NAME.
00091              20  PB-I-INSURED-LAST-NAME   PIC X(15).
00092              20  PB-I-INSURED-FIRST-NAME.
00093                  24  PB-I-INSURED-1ST-INIT PIC X.
00094                  24  FILLER                PIC X(9).
00095              20  PB-I-INSURED-MIDDLE-INIT PIC X.
00096          16  PB-I-AGE                     PIC S99   COMP-3.
00097          16  PB-I-JOINT-AGE               PIC S99   COMP-3.
00098          16  PB-I-BIRTHDAY                PIC XX.
00099          16  PB-I-INSURED-SEX             PIC X.
00100              88  PB-SEX-MALE     VALUE 'M'.
00101              88  PB-SEX-FEMALE   VALUE 'F'.
00102
00103          16  PB-I-LF-TERM                 PIC S999   COMP-3.
00104          16  PB-I-AH-TERM                 PIC S999   COMP-3.
00105          16  PB-I-LOAN-TERM               PIC S999   COMP-3.
00106          16  PB-I-PAY-FREQUENCY           PIC S99    COMP-3.
00107          16  PB-I-SKIP-CODE               PIC X.
00108              88  PB-NO-MONTHS-SKIPPED      VALUE ' ' '0'.
00109              88  PB-SKIP-JULY              VALUE '1'.
00110              88  PB-SKIP-AUGUST            VALUE '2'.
00111              88  PB-SKIP-SEPTEMBER         VALUE '3'.
00112              88  PB-SKIP-JULY-AUG          VALUE '4'.
00113              88  PB-SKIP-AUG-SEPT          VALUE '5'.
00114              88  PB-SKIP-JULY-AUG-SEPT     VALUE '6'.
00115              88  PB-SKIP-JUNE-JULY-AUG     VALUE '7'.
00116              88  PB-SKIP-JUNE              VALUE '8'.
00117              88  PB-SKIP-JUNE-JULY         VALUE '9'.
00118              88  PB-SKIP-AUG-SEPT-OCT      VALUE 'A'.
00119              88  PB-SKIP-BI-WKLY-3RD-PMT   VALUE 'X'.
00120          16  PB-I-TERM-TYPE               PIC X.
00121              88  PB-PAID-MONTHLY           VALUE ' ' 'M'.
00122              88  PB-PAID-WEEKLY            VALUE 'W'.
00123              88  PB-PAID-SEMI-MONTHLY      VALUE 'S'.
00124              88  PB-PAID-BI-WEEKLY         VALUE 'B'.
00125              88  PB-PAID-13-YEARLY         VALUE 'T'.
00126          16  PB-I-NO-OF-PAYMENTS          PIC S999   COMP-3.
00127          16  PB-I-POLICY-FORM-NO          PIC X(12).
00128          16  PB-I-DATA-ENTRY-SW           PIC X.
00129              88  PB-EFF-DT-PROCESSING      VALUE '1' ' '.
00130              88  PB-EXT-DAYS-PROCESSING    VALUE '2'.
00131              88  PB-EXPIRE-DT-PROCESSING   VALUE '3'.
00132              88  PB-1ST-PMT-DT-PROCESSING  VALUE '4'.
00133          16  PB-I-PAYMENT-AMOUNT          PIC S9(7)V99  COMP-3.
073107         16  PB-I-DCC-OVER-CHG-AMT        PIC S9(5)V99  COMP-3.
011410*        16  PB-I-MICROFILM-NO            PIC S9(9)      COMP-3.
011410         16  PB-I-AH-CLP                  PIC S9(5)V99 COMP-3.
011410         16  FILLER                       PIC X.
00136
00137          16  PB-I-LIFE-BENEFIT-CD         PIC XX.
00138              88  PB-VALID-LIFE               VALUE '01' THRU '89'.
00139              88  PB-INVALID-LIFE             VALUE '  ' '00'
00140                                                    '90' THRU '99'.
00141          16  PB-I-LF-BENEFIT-CD   REDEFINES PB-I-LIFE-BENEFIT-CD
00142                                           PIC XX.
00143          16  PB-I-LF-BENEFIT-AMT          PIC S9(9)V99   COMP-3.
100703         16  PB-I-AMOUNT-FINANCED REDEFINES
100703                  PB-I-LF-BENEFIT-AMT     PIC S9(9)V99   COMP-3.
00144          16  PB-I-LF-ALT-BENEFIT-AMT      PIC S9(9)V99   COMP-3.
100703         16  PB-I-UNPAID-CASH-PRICE REDEFINES
100703                  PB-I-LF-ALT-BENEFIT-AMT PIC S9(9)V99   COMP-3.
00145          16  PB-I-LF-PREMIUM-AMT          PIC S9(7)V99   COMP-3.
00146          16  PB-I-LF-ALT-PREMIUM-AMT      PIC S9(7)V99   COMP-3.
100703         16  PB-I-CLP-AMOUNT REDEFINES
100703                  PB-I-LF-ALT-PREMIUM-AMT PIC S9(7)V99   COMP-3.
00147          16  PB-I-LF-CALC-FLAG            PIC X.
00148              88 PB-COMP-LF-PREM               VALUE '?'.
00149          16  PB-I-LF-PREM-CALC            PIC S9(7)V99   COMP-3.
00150          16  PB-I-LF-ALT-PREM-CALC        PIC S9(7)V99   COMP-3.
00151          16  PB-I-LF-RATE                 PIC S99V9(5)   COMP-3.
00152          16  PB-I-LF-ALT-RATE             PIC S99V9(5)   COMP-3.
00153          16  PB-I-LF-POLICY-FEE           PIC S9(3)V99   COMP-3.
00154          16  PB-I-LF-REI-RATE             PIC S99V9(5)   COMP-3.
00155          16  PB-I-LF-ALT-REI-RATE         PIC S99V9(5)   COMP-3.
00156          16  PB-I-LF-ABBR                 PIC XXX.
00157          16  PB-I-LF-INPUT-CD             PIC XX.
00158
00159          16  PB-I-AH-BENEFIT-CD           PIC XX.
00160              88  PB-VALID-AH                 VALUE '01' THRU '89'.
00161              88  PB-INVALID-AH               VALUE '  ' '00'
00162                                                    '90' THRU '99'.
00163          16  PB-I-AH-BENEFIT-AMT          PIC S9(7)V99   COMP-3.
00164          16  PB-I-AH-PREMIUM-AMT          PIC S9(7)V99   COMP-3.
00165          16  PB-I-AH-CALC-FLAG            PIC X.
00166              88 PB-COMP-AH-PREM                  VALUE '?'.
00167          16  PB-I-AH-PREM-CALC            PIC S9(7)V99   COMP-3.
00168          16  PB-I-AH-RATE                 PIC S99V9(5)   COMP-3.
010716         16  PB-I-CANCEL-FEE              PIC S9(3)V99   COMP-3.
00170          16  PB-I-AH-REI-RATE             PIC S99V9(5)   COMP-3.
00171          16  PB-I-AH-RATE-TRM             PIC S999       COMP-3.
00172          16  PB-I-AH-ABBR                 PIC XXX.
00173          16  PB-I-AH-INPUT-CD             PIC XXX.
00174
00175          16  PB-I-SPECIAL-REIN-CODE       PIC X.
00176          16  PB-I-REIN-TABLE              PIC XXX.
00177          16  PB-I-BUSINESS-TYPE           PIC 99.
00178          16  PB-I-INDV-GRP-CD             PIC X.
00179          16  PB-I-MORT-CODE.
00180              20  PB-I-TABLE               PIC X.
00181              20  PB-I-INTEREST            PIC XX.
00182              20  PB-I-MORT-TYP            PIC X.
00183          16  PB-I-LF-CRIT-PER             PIC S9(3)      COMP-3.
00184          16  PB-I-AH-CRIT-PER             PIC S9(3)      COMP-3.
011410         16  PB-I-LF-CLP                  PIC S9(5)V99   COMP-3.
00186          16  PB-I-INDV-GRP-OVRD           PIC X.
00187          16  PB-I-RATE-CLASS-OVRD         PIC XX.
00188          16  PB-I-SIG-SW                  PIC X.
00189              88  PB-POLICY-SIGNED             VALUE 'Y'.
00190          16  PB-I-RATE-CLASS              PIC XX.
00191          16  PB-I-RATE-DEVIATION-LF       PIC XXX.
00192          16  PB-I-RATE-DEVIATION-AH       PIC XXX.
00193          16  PB-I-RATE-DEV-PCT-LF         PIC S9V9(6)    COMP-3.
00194          16  PB-I-RATE-DEV-PCT-AH         PIC S9V9(6)    COMP-3.
00195          16  PB-I-LIFE-COMMISSION         PIC SV9(5)     COMP-3.
00196          16  PB-I-JOINT-COMMISSION        PIC SV9(5)     COMP-3.
00197          16  PB-I-AH-COMMISSION           PIC SV9(5)     COMP-3.
00198          16  PB-I-BENEFIT-TYPE            PIC XXX.
00199          16  PB-I-OB-FLAG                 PIC X.
00200              88  PB-I-OB                      VALUE 'B'.
00201              88  PB-I-SUMMARY                 VALUE 'Z'.
00202          16  PB-I-ENTRY-STATUS            PIC X.
00203              88  PB-I-POLICY-IS-ACTIVE        VALUE '1' '3' '4'
122002                                              'M' '5' '9' '2'.
00205              88  PB-I-NORMAL-ENTRY            VALUE '1'.
00206              88  PB-I-POLICY-PENDING          VALUE '2'.
00207              88  PB-I-CONVERSION-ENTRY        VALUE '4'.
00208              88  PB-I-POLICY-IS-REISSUE       VALUE '5'.
                   88  PB-I-POLICY-IS-CASH          VALUE 'C'.
122002             88  PB-I-POLICY-IS-MONTHLY       VALUE 'M'.
00209              88  PB-I-REIN-ONLY               VALUE '9'.
00210              88  PB-I-POLICY-IS-DECLINED      VALUE 'D'.
00211              88  PB-I-POLICY-IS-VOIDED        VALUE 'V'.
00212              88  PB-I-PREM-ACCTNG-ONLY        VALUE 'P'.
00213              88  PB-I-UNDERWRITE-POLICY       VALUE 'U'.
00214          16  PB-I-INT-CODE                PIC X.
00215              88  PB-ADD-ON-INTEREST           VALUE 'A'.
00216              88  PB-SIMPLE-INTEREST           VALUE 'S'.
00217          16  PB-I-LOAN-APR                PIC 9(3)V9(4)   COMP-3.
00218          16  PB-I-SOC-SEC-NO              PIC X(11).
00219          16  PB-I-MEMBER-NO               PIC X(12).
00220          16  PB-I-CURR-SEQ                PIC S9(4)       COMP.
110105*        16  PB-I-LOAN-OFFICER            PIC XXX.
110105         16  PB-I-OLD-LOF                 PIC XXX.
00222          16  PB-I-LF-EXPIRE-DT            PIC XX.
00223          16  PB-I-AH-EXPIRE-DT            PIC XX.
00224          16  PB-I-EXTENTION-DAYS          PIC S999        COMP-3.
00225          16  PB-I-TERM-IN-DAYS            PIC S9(5)       COMP-3.
00226          16  PB-I-LIFE-INDICATOR          PIC X.
00227              88  PB-I-JOINT-COVERAGE         VALUE 'J'.
00228          16  PB-I-LIVES                   PIC S9(7)       COMP-3.
071211         16  PB-I-DDF-IU-RATE-UP REDEFINES PB-I-LIVES
071211                                          PIC S9(5)V99    COMP-3.
00229          16  PB-I-MAIL-ADDRS-SW           PIC X.
00230              88 PB-I-MAIL-ADDRS-NOT-PRESENT  VALUE ' '.
00231              88 PB-I-MAIL-ADDRS-PRESENT      VALUE '1'.
00232          16  PB-I-1ST-PMT-DT              PIC XX.
00233          16  PB-I-JOINT-INSURED.
00234              20 PB-I-JOINT-LAST-NAME      PIC X(15).
00235              20 PB-I-JOINT-FIRST-NAME.
00236                 24  PB-I-JOINT-FIRST-INIT PIC X.
00237                 24  FILLER                PIC X(9).
00238              20 PB-I-JOINT-MIDDLE-INIT    PIC X.
100703*        16  PB-I-BENEFICIARY-NAME        PIC X(25).
100703         16  PB-I-BENEFICIARY-NAME.
100703             20  PB-I-BANK-NUMBER         PIC X(10).
100703             20  FILLER                   PIC X(15).
00240          16  PB-I-LAST-ADD-ON-DT          PIC XX.
011904         16  PB-I-REFERENCE               PIC X(12).
011904         16  FILLER REDEFINES PB-I-REFERENCE.
011904             20  PB-I-TOT-FEES            PIC S9(7)V99 COMP-3.
011904             20  PB-I-TOT-FEES-CALC       PIC S9(7)V99 COMP-3.
020305             20  PB-I-CLP-STATE           PIC XX.
00242          16  PB-I-UNDERWRITING-STATUS     PIC X.
00243              88  PB-I-POLICY-ACCEPTED         VALUE 'A' 'N'.
00244              88  PB-I-POLICY-DECLINED         VALUE 'D'.
00245              88  PB-I-NEEDS-UNDERWRITING      VALUE 'U'.
00246          16  PB-I-STATE-TAX               PIC S9(7)V99 COMP-3.
00247          16  PB-I-MUNI-TAX                PIC S9(7)V99 COMP-3.
00248          16  PB-I-RESIDENT-STATE          PIC XX.
00249          16  PB-I-RATE-CODE               PIC X(4).
00250          16  PB-I-NUM-BILLED              PIC S9(7)    COMP-3.
PEMMOD         16  PB-I-LF-PREM-TAX             PIC S9V9(4)  COMP-3.
PEMMOD         16  PB-I-AH-PREM-TAX             PIC S9V9(4)  COMP-3.
100703         16  PB-I-BANK-FEE                PIC S999V99  COMP-3.
100703         16  PB-I-BANK-NOCHRGB            PIC 99.
040504         16  PB-I-ADDL-CLP                PIC S9(5)V99 COMP-3.
081108         16  PB-I-JOINT-BIRTHDAY          PIC XX.
00252
00253      12  PB-CANCEL-RECORD   REDEFINES PB-RECORD-BODY.
00254          16  PB-C-LF-CANCEL-VOID-SW       PIC X.
00255              88  PB-C-LF-CANCEL-VOIDED        VALUE '1'.
00256          16  PB-C-CANCEL-ORIGIN           PIC X.
00257              88  PB-C-CLAIM-CREATED-CANCEL   VALUE '1'.
00258          16  PB-C-LF-CANCEL-DT            PIC XX.
00259          16  PB-C-LF-CANCEL-AMT           PIC S9(7)V99    COMP-3.
00260          16  PB-C-LF-CALC-REQ             PIC X.
00261              88 PB-COMP-LF-CANCEL            VALUE '?'.
00262          16  PB-C-LF-REF-CALC             PIC S9(7)V99    COMP-3.
00263          16  PB-C-LF-REM-TERM             PIC S9(3)       COMP-3.
00264          16  PB-C-AH-CANCEL-VOID-SW       PIC X.
00265              88  PB-C-AH-CANCEL-VOIDED        VALUE '1'.
00266          16  PB-C-AH-CANCEL-DT            PIC XX.
00267          16  PB-C-AH-CANCEL-AMT           PIC S9(7)V99    COMP-3.
00268          16  PB-C-AH-CALC-REQ             PIC X.
00269              88 PB-COMP-AH-CANCEL            VALUE '?'.
00270          16  PB-C-AH-REF-CALC             PIC S9(7)V99    COMP-3.
00271          16  PB-C-AH-REM-TERM             PIC S9(3)       COMP-3.
00272          16  PB-C-LAST-NAME               PIC X(15).
00273          16  PB-C-REFUND-SW               PIC X.
00274              88  PB-C-REFUND-CREATED          VALUE 'Y'.
00275              88  PB-C-REFUND-REQUESTED        VALUE 'R'.
00276          16  PB-C-LIVES                   PIC S9(3)       COMP-3.
00277          16  PB-C-PAYEE-CODE              PIC X(6).
00278          16  PB-C-LF-REFUND-OVERRIDE      PIC X.
00279          16  PB-C-AH-REFUND-OVERRIDE      PIC X.
00280          16  PB-C-LF-COMM-CHARGEBACK      PIC X.
00281          16  PB-C-AH-COMM-CHARGEBACK      PIC X.
00282          16  PB-C-REFERENCE               PIC X(12).
PEMMOD         16  PB-C-LF-PREM-TAX             PIC S9V9(4)  COMP-3.
PEMMOD         16  PB-C-AH-PREM-TAX             PIC S9V9(4)  COMP-3.
081606         16  PB-C-POST-CARD-IND           PIC X.
081606         16  PB-C-CANCEL-REASON           PIC X.
072308         16  PB-C-REF-INTERFACE-SW        PIC X.
071211         16  PB-C-LF-RFND-CLP             PIC S9(5)V99 COMP-3.
071211         16  PB-C-AH-RFND-CLP             PIC S9(5)V99 COMP-3.
00283          16  FILLER                       PIC X(01).
PEMMOD*        16  FILLER                       PIC X(18).
00284          16  PB-C-POLICY-FORM-NO          PIC X(12).
072308*        16  PB-C-MICROFILM-NO            PIC S9(9)      COMP-3.
062017         16  PB-C-INT-ON-REFS             PIC S9(7)V99   COMP-3.
00286          16  PB-CANCELED-CERT-DATA.
00287              20  PB-CI-INSURED-NAME.
00288                  24  PB-CI-LAST-NAME      PIC X(15).
00289                  24  PB-CI-INITIALS       PIC XX.
00290              20  PB-CI-INSURED-AGE        PIC S99         COMP-3.
00291              20  PB-CI-INSURED-SEX        PIC X.
00292              20  PB-CI-LF-TERM            PIC S999        COMP-3.
00293              20  PB-CI-LF-BENEFIT-CD      PIC XX.
00294              20  PB-CI-LF-BENEFIT-AMT     PIC S9(9)V99    COMP-3.
00295              20  PB-CI-LF-ALT-BENEFIT-AMT PIC S9(9)V99    COMP-3.
00296              20  PB-CI-LF-PREMIUM-AMT     PIC S9(7)V99    COMP-3.
00297              20  PB-CI-LF-ALT-PREMIUM-AMT PIC S9(7)V99    COMP-3.
00298              20  PB-CI-AH-TERM            PIC S999        COMP-3.
00299              20  PB-CI-AH-BENEFIT-CD      PIC XX.
00300              20  PB-CI-AH-BENEFIT-AMT     PIC S9(7)V99    COMP-3.
00301              20  PB-CI-AH-PREMIUM-AMT     PIC S9(7)V99    COMP-3.
00302              20  PB-CI-RATE-CLASS         PIC XX.
00303              20  PB-CI-RATE-DEV-LF        PIC XXX.
00304              20  PB-CI-RATE-DEV-AH        PIC XXX.
00305              20  PB-CI-RATE-DEV-PCT-LF    PIC S9V9(6)     COMP-3.
00306              20  PB-CI-RATE-DEV-PCT-AH    PIC S9V9(6)     COMP-3.
00307              20  PB-CI-LIFE-COMMISSION    PIC SV9(5)      COMP-3.
00308              20  PB-CI-AH-COMMISSION      PIC SV9(5)      COMP-3.
00309              20  PB-CI-LF-ABBR            PIC X(3).
00310              20  PB-CI-AH-ABBR            PIC X(3).
00311              20  PB-CI-OB-FLAG            PIC X.
00312                  88  PB-CI-OB                VALUE 'B'.
00313              20  PB-CI-LF-POLICY-STATUS   PIC X.
00314                  88  PB-CI-LF-POLICY-IS-ACTIVE       VALUE '1' '3'
122002                                           'M' '4' '5' '9' '2'.
00316                  88  PB-CI-LF-NORMAL-ENTRY           VALUE '1'.
00317                  88  PB-CI-LF-POLICY-PENDING         VALUE '2'.
00318                  88  PB-CI-LF-POLICY-IS-RESTORE      VALUE '3'.
00319                  88  PB-CI-LF-CONVERSION-ENTRY       VALUE '4'.
00320                  88  PB-CI-LF-POLICY-IS-REISSUE      VALUE '5'.
                       88  PB-CI-LF-POLICY-IS-CASH         VALUE 'C'.
122002                 88  PB-CI-LF-POLICY-IS-MONTHLY      VALUE 'M'.
00321                  88  PB-CI-LF-LUMP-SUM-DISAB         VALUE '6'.
00322                  88  PB-CI-LF-DEATH-CLAIM-APPLIED    VALUE '7'.
00323                  88  PB-CI-LF-CANCEL-APPLIED         VALUE '8'.
00324                  88  PB-CI-LF-REIN-ONLY              VALUE '9'.
00325                  88  PB-CI-LF-POLICY-IS-DECLINED     VALUE 'D'.
00326                  88  PB-CI-LF-POLICY-IS-VOID         VALUE 'V'.
00327              20  PB-CI-AH-POLICY-STATUS   PIC X.
00328                  88  PB-CI-AH-POLICY-IS-ACTIVE       VALUE '1' '3'
122002                                           'M' '4' '5' '9' '2'.
00330                  88  PB-CI-AH-NORMAL-ENTRY           VALUE '1'.
00331                  88  PB-CI-AH-POLICY-PENDING         VALUE '2'.
00332                  88  PB-CI-AH-POLICY-IS-RESTORE      VALUE '3'.
00333                  88  PB-CI-AH-CONVERSION-ENTRY       VALUE '4'.
00334                  88  PB-CI-AH-POLICY-IS-REISSUE      VALUE '5'.
                       88  PB-CI-AH-POLICY-IS-CASH         VALUE 'C'.
122002                 88  PB-CI-AH-POLICY-IS-MONTHLY      VALUE 'M'.
00335                  88  PB-CI-AH-LUMP-SUM-DISAB         VALUE '6'.
00336                  88  PB-CI-AH-DEATH-CLAIM-APPLIED    VALUE '7'.
00337                  88  PB-CI-AH-CANCEL-APPLIED         VALUE '8'.
00338                  88  PB-CI-AH-REIN-ONLY              VALUE '9'.
00339                  88  PB-CI-AH-POLICY-IS-DECLINED     VALUE 'D'.
00340                  88  PB-CI-AH-POLICY-IS-VOID         VALUE 'V'.
00341              20  PB-CI-PAY-FREQUENCY      PIC 99.
00342              20  PB-CI-LOAN-APR           PIC 9(3)V9(4)   COMP-3.
00343              20  PB-CI-SOC-SEC-NO         PIC X(11).
00344              20  PB-CI-MEMBER-NO          PIC X(12).
00345              20  PB-CI-INT-CODE           PIC X.
00346                  88  PB-CI-ADD-ON                  VALUE 'A'.
00347                  88  PB-CI-SIMPLE                  VALUE 'S'.
00348              20  PB-CI-LOAN-TERM          PIC S999        COMP-3.
00349              20  PB-CI-LOAN-1ST-PMT-DT    PIC X(2).
00350              20  PB-CI-COMP-EXCP-SW       PIC X.
00351                  88  PB-CI-NO-COMP-EXCP            VALUE ' '.
00352                  88  PB-CI-CERT-HAS-ERCOMM-ENTRY   VALUE '1'.
00353              20  PB-CI-ENTRY-STATUS       PIC X.
00354              20  PB-CI-CURR-SEQ           PIC S9(4)       COMP.
00355              20  PB-CI-AH-PAID-THRU-DT    PIC XX.
00356              20  PB-CI-AH-SETTLEMENT-DT   PIC XX.
00357              20  PB-CI-DEATH-DT           PIC XX.
00358              20  PB-CI-LF-PRIOR-CANCEL-DT PIC XX.
00359              20  PB-CI-AH-PRIOR-CANCEL-DT PIC XX.
00360              20  PB-CI-AH-CANCEL-AMT      PIC S9(7)V99    COMP-3.
00361              20  PB-CI-LF-CANCEL-AMT      PIC S9(7)V99    COMP-3.
00362              20  PB-CI-CREDIT-INTERFACE-SW-1 PIC X.
00363              20  PB-CI-CREDIT-INTERFACE-SW-2 PIC X.
00364              20  PB-CI-ENTRY-DT              PIC XX.
00365              20  PB-CI-ENTRY-BATCH           PIC X(6).
00366              20  PB-CI-LF-EXPIRE-DT          PIC XX.
00367              20  PB-CI-AH-EXPIRE-DT          PIC XX.
00368              20  PB-CI-EXTENTION-DAYS        PIC S999     COMP-3.
00369              20  PB-CI-TERM-IN-DAYS          PIC S9(5)    COMP-3.
110105             20  PB-CI-OLD-LOF               PIC XXX.
110105*            20  PB-CI-LOAN-OFFICER          PIC XXX.
00371              20  PB-CI-LIVES                 PIC S9(3)    COMP-3.
00372              20  PB-CI-LF-CRIT-PER           PIC S9(3)    COMP-3.
00373              20  PB-CI-AH-CRIT-PER           PIC S9(3)    COMP-3.
00374              20  PB-CI-INDV-GRP-CD           PIC X.
100703             20  PB-CI-BENEFICIARY-NAME.
100703                 24  PB-CI-BANK-NUMBER       PIC X(10).
100703                 24  FILLER                  PIC X(15).
00376              20  PB-CI-NOTE-SW               PIC X.
00377              20  PB-CI-CANCEL-FEE            PIC S9(3)V99 COMP-3.
00378              20  PB-CI-MUNI-TAX              PIC S9(7)V99 COMP-3.
00379              20  PB-CI-STATE-TAX             PIC S9(7)V99 COMP-3.
040504             20  PB-CI-ADDL-CLP              PIC S9(5)V99 COMP-3.
110105             20  PB-CI-LOAN-OFFICER          PIC X(5).
032306             20  PB-CI-BOW-LOAN-NUMBER       PIC X(14).
072209             20  PB-CI-FIRST-NAME            PIC X(10).
071211             20  PB-CI-DDF-IU-RATE-UP        PIC S9(5)V99 COMP-3.
00380
072209         16  FILLER                       PIC X(13).
072209*032306  16  FILLER                       PIC X(27).
040504*        16  FILLER                       PIC X(46).
00382
00383      12  PB-MAIL-RECORD    REDEFINES PB-RECORD-BODY.
00384          16  FILLER                       PIC X(10).
00385          16  PB-M-INSURED-LAST-NAME       PIC X(15).
00386          16  PB-M-INSURED-FIRST-NAME      PIC X(10).
00387          16  PB-M-INSURED-MID-INIT        PIC X.
00388          16  PB-M-INSURED-AGE             PIC 99.
00389          16  PB-M-INSURED-BIRTHDAY        PIC XX.
00390          16  PB-M-INSURED-SEX             PIC X.
00391          16  PB-M-INSURED-SOC-SEC-NO      PIC X(11).
00392          16  PB-M-INSURED-ADDRESS-1       PIC X(30).
00393          16  PB-M-INSURED-ADDRESS-2       PIC X(30).
00394          16  PB-M-INSURED-CITY-STATE.
051810             20  PB-M-INSURED-CITY        PIC X(28).
051810             20  PB-M-INSURED-STATE       PIC XX.
00395          16  PB-M-INSURED-ZIP-CODE.
00396              20  PB-M-INSURED-ZIP-PRIME.
00397                  24  PB-M-INSURED-ZIP-1   PIC X.
00398                      88  PB-M-CANADIAN-POST-CODE
00399                                              VALUE 'A' THRU 'Z'.
00400                  24  FILLER               PIC X(4).
00401              20  PB-M-INSURED-ZIP-PLUS4   PIC X(4).
00402          16  PB-M-INSURED-CANADIAN-ZIP  REDEFINES
00403                                         PB-M-INSURED-ZIP-CODE.
00404              20  PM-M-INS-CAN-POST1       PIC XXX.
00405              20  PM-M-INS-CAN-POST2       PIC XXX.
00406              20  FILLER                   PIC XXX.
00407          16  PB-M-INSURED-PHONE-NO        PIC 9(10).
081108         16  PB-M-JOINT-BIRTHDAY          PIC XX.
               16  PB-M-CRED-BENE-NAME          PIC X(30).
               16  PB-M-CRED-BENE-ADDR1         PIC X(30).
               16  PB-M-CRED-BENE-ADDR2         PIC X(30).
               16  PB-M-CRED-BENE-CITYST.
                   20  PB-M-CRED-BENE-CITY      PIC X(28).
                   20  PB-M-CRED-BENE-STATE     PIC XX.
081108         16  FILLER                       PIC X(92).
00409
00410      12  PB-BATCH-RECORD   REDEFINES PB-RECORD-BODY.
00411          16  FILLER                       PIC X(10).
00412          16  PB-B-LF-ISS-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00413          16  PB-B-LF-ISS-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00414          16  PB-B-LF-ISS-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00415          16  PB-B-LF-CAN-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00416          16  PB-B-LF-CAN-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00417          16  PB-B-LF-CAN-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00418          16  PB-B-AH-ISS-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00419          16  PB-B-AH-ISS-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00420          16  PB-B-AH-ISS-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00421          16  PB-B-AH-CAN-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00422          16  PB-B-AH-CAN-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00423          16  PB-B-AH-CAN-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00424          16  PB-B-ISSUE-CNT-REMITTED      PIC S9(5)    COMP-3.
00425          16  PB-B-ISSUE-CNT-ENTERED       PIC S9(5)    COMP-3.
00426          16  PB-B-CANCEL-CNT-REMITTED     PIC S9(5)    COMP-3.
00427          16  PB-B-CANCEL-CNT-ENTERED      PIC S9(5)    COMP-3.
00428          16  PB-B-HIGHEST-SEQ-NO          PIC S9(4)    COMP.
00429          16  PB-ACCOUNT-NAME              PIC X(30).
00430          16  PB-PREM-REF-RPT-FLAG         PIC X.
00431          16  PB-REFERENCE                 PIC X(12).
00432          16  PB-B-RECEIVED-DT             PIC XX.
00433          16  FILLER                       PIC X(234).
00434
00435      12  PB-RECORD-STATUS.
00436          16  PB-CREDIT-SELECT-DT          PIC XX.
00437          16  PB-CREDIT-ACCEPT-DT          PIC XX.
00438          16  PB-BILLED-DT                 PIC XX.
00439          16  PB-BILLING-STATUS            PIC X.
00440              88  PB-ENTRY-REVERSED            VALUE 'R'.
00441              88  PB-EL860-INTERNAL-ERROR      VALUE 'E'.
00442              88  PB-EL860-INTERNAL-PROCESS    VALUE 'P'.
00443          16  PB-RECORD-BILL               PIC X.
00444              88  PB-RECORD-ON-HOLD            VALUE 'H'.
00445              88  PB-RECORD-RETURNED           VALUE 'R'.
00446              88  PB-RECORD-ENDORSED           VALUE 'E'.
00447              88  PB-OVERRIDE-LIFE             VALUE 'L'.
00448              88  PB-OVERRIDE-AH               VALUE 'A'.
00449              88  PB-OVERRIDE-BOTH             VALUE 'B'.
00450          16  PB-BATCH-ENTRY               PIC X.
00451              88  PB-POLICY-IS-DECLINED        VALUE 'D'.
00452              88  PB-REIN-ONLY-CERT            VALUE 'R'.
00453              88  PB-REISSUED-CERT             VALUE 'E'.
                   88  PB-CASH-CERT                 VALUE 'C'.
122002             88  PB-MONTHLY-CERT              VALUE 'M'.
00454              88  PB-PREM-ACCTNG-ONLY          VALUE 'P'.
00455              88  PB-NEEDS-UNDERWRITING        VALUE 'U'.
00456              88  PB-POLICY-IS-VOIDED          VALUE 'V'.
00457          16  PB-FORCE-CODE                PIC X.
00458              88  PB-FORCE-OFF                 VALUE ' ' '0'.
00459              88  PB-ISSUE-FORCE               VALUE 'A' 'O'.
00460              88  PB-CANCEL-FORCE              VALUE '8'.
00461              88  PB-ALL-ISSUE-FORCED          VALUE 'A' 'O'.
00462              88  PB-ALL-CANCEL-FORCED         VALUE '8'.
00463              88  PB-ALL-CANCEL-FORCED-NO-FEE  VALUE '9'.
00464              88  PB-CANCEL-DATE-FORCED        VALUE 'D'.
00465              88  PB-CANCEL-DATE-FORCED-NO-FEE VALUE 'E'.
00466              88  PB-ISSUE-DATE-FORCED         VALUE 'D'.
010517             88  PB-EXCEEDED-LIMIT-FORCED     VALUE 'L'.
073107             88  PB-OVERCHARGE-FORCE          VALUE 'O'.
00467          16  PB-FATAL-FLAG                PIC X.
00468              88  PB-FATAL-ERRORS              VALUE 'X'.
00469          16  PB-FORCE-ER-CD               PIC X.
00470              88  PB-FORCE-ERRORS              VALUE 'F'.
00471              88  PB-UNFORCED-ERRORS           VALUE 'X'.
00472          16  PB-WARN-ER-CD                PIC X.
00473              88  PB-WARNING-ERRORS            VALUE 'W'.
00474          16  FILLER                       PIC X.
00475          16  PB-OUT-BAL-CD                PIC X.
00476              88  PB-OUT-OF-BAL                VALUE 'O'.
00477          16  PB-LIFE-OVERRIDE-L1          PIC X.
00478          16  PB-AH-OVERRIDE-L1            PIC X.
00479          16  PB-INPUT-DT                  PIC XX.
00480          16  PB-INPUT-BY                  PIC X(4).
00481          16  PB-CHG-COUNT                 PIC 9(3)        COMP-3.
00482          16  PB-CALC-TOLERANCE            PIC 9(3)V99     COMP-3.
00483          16  PB-TOLERANCE-REJECT-SW       PIC X.
00484          16  PB-LF-EARNING-METHOD         PIC X.
00485          16  PB-AH-EARNING-METHOD         PIC X.
00486          16  PB-LF-TERM-CALC-METHOD       PIC X.
00487          16  PB-AH-TERM-CALC-METHOD       PIC X.
00488          16  PB-REIN-CD                   PIC XXX.
00489          16  PB-LF-REFUND-TYPE            PIC X.
00490          16  PB-AH-REFUND-TYPE            PIC X.
00491          16  PB-ACCT-EFF-DT               PIC XX.
00492          16  PB-ACCT-EXP-DT               PIC XX.
00493          16  PB-COMPANY-ID                PIC X(3).
00494          16  PB-LF-BILLED-AMTS            PIC S9(7)V99  COMP-3.
00495          16  PB-AH-BILLED-AMTS            PIC S9(7)V99  COMP-3.
00496          16  PB-SV-CARRIER                PIC X.
00497          16  PB-SV-GROUPING               PIC X(6).
00498          16  PB-SV-STATE                  PIC XX.
00499          16  PB-CONFIRMATION-REPT-DT      PIC XX.
00500          16  PB-GA-BILLING-INFO.
00501              20  PB-GA-BILL-DT OCCURS 5 TIMES
00502                                           PIC XX.
00503          16  PB-SV-REMIT-TO  REDEFINES
00504              PB-GA-BILLING-INFO           PIC X(10).
00505          16  PB-NO-OF-ERRORS              PIC S9(3) COMP-3.
110105         16  PB-I-LOAN-OFFICER            PIC X(5).
081606         16  PB-I-VIN                     PIC X(17).
00506
110105         16  FILLER                       PIC X(04).
110105         16  IMNET-BYPASS-SW              PIC X.
00508
00509 ******************************************************************
00510 *                COMMON EDIT ERRORS                              *
00511 ******************************************************************
00512
00513      12  PB-COMMON-ERRORS.
00514          16  PB-COMMON-ERROR    OCCURS 10 TIMES
00515                                            PIC S9(4)     COMP.
00516
00517 ******************************************************************
00290
00291      EJECT
00292 *                                    COPY ELCCNTL.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCCNTL.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.059                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = SYSTEM CONTROL FILE                       *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 750  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCNTL                        RKP=2,LEN=10    *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
082503*                   C H A N G E   L O G
082503*
082503* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
082503*-----------------------------------------------------------------
082503*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
082503* EFFECTIVE    NUMBER
082503*-----------------------------------------------------------------
082503* 082503                   PEMA  ADD BENEFIT GROUP
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
033104* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
092705* 092705    2005050300006  PEMA  ADD SPP LEASES
031808* 031808    2006032200004  AJRA  ADD APPROVAL LEVEL 4
071508* 071508  CR2007110500003  PEMA  ADD NH INTEREST REFUND PROCESSING
091808* 091808    2008022800002  AJRA  ADD CHECK NUMBER TO STATE CNTL FO
011410* 011410    2009061500002  AJRA  ADD REFUND IND FOR AH AND DEATH C
061511* 061511    2011042000002  AJRA  ADD IND TO VERIFY 2ND BENEFICIARY
011812* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
012913* 012913    2012092400007  AJRA  ADD CAUSAL STATE IND
032813* 032813    2011013100001  AJRA  ADD CLAIM REAUDIT FIELDS
091813* 091813    2013082900001  AJRA  ADD APPROVAL LEVEL 5
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
102717* 102717  CR2017062000003  PEMA  COMM CAP CHANGES
082503******************************************************************
00018 *
00019  01  CONTROL-FILE.
00020      12  CF-RECORD-ID                       PIC XX.
00021          88  VALID-CF-ID                        VALUE 'CF'.
00022
00023      12  CF-CONTROL-PRIMARY.
00024          16  CF-COMPANY-ID                  PIC XXX.
00025          16  CF-RECORD-TYPE                 PIC X.
00026              88  CF-COMPANY-MASTER              VALUE '1'.
00027              88  CF-PROCESSOR-MASTER            VALUE '2'.
00028              88  CF-STATE-MASTER                VALUE '3'.
00029              88  CF-LF-BENEFIT-MASTER           VALUE '4'.
00030              88  CF-AH-BENEFIT-MASTER           VALUE '5'.
00031              88  CF-CARRIER-MASTER              VALUE '6'.
00032              88  CF-MORTALITY-MASTER            VALUE '7'.
00033              88  CF-BUSINESS-TYPE-MASTER        VALUE '8'.
00034              88  CF-TERMINAL-MASTER             VALUE '9'.
00035              88  CF-AH-EDIT-MASTER              VALUE 'A'.
00036              88  CF-CREDIBILITY-FACTOR-MASTER   VALUE 'B'.
00037              88  CF-CUSTOM-REPORT-MASTER        VALUE 'C'.
00038              88  CF-MORTGAGE-HT-WT-CHART        VALUE 'H'.
00039              88  CF-LIFE-EDIT-MASTER            VALUE 'L'.
00040              88  CF-MORTGAGE-PLAN-MASTER        VALUE 'M'.
00041              88  CF-MORTGAGE-COMPANY-MASTER     VALUE 'N'.
00042              88  CF-REMINDERS-MASTER            VALUE 'R'.
00043              88  CF-AUTO-ACTIVITY-MASTER        VALUE 'T'.
00044          16  CF-ACCESS-CD-GENL              PIC X(4).
00045          16  CF-ACCESS-OF-PROCESSOR  REDEFINES CF-ACCESS-CD-GENL.
00046              20  CF-PROCESSOR               PIC X(4).
00047          16  CF-ACCESS-OF-STATE  REDEFINES  CF-ACCESS-CD-GENL.
00048              20  CF-STATE-CODE              PIC XX.
00049              20  FILLER                     PIC XX.
00050          16  CF-ACCESS-OF-BENEFIT  REDEFINES  CF-ACCESS-CD-GENL.
00051              20  FILLER                     PIC XX.
00052              20  CF-HI-BEN-IN-REC           PIC XX.
00053          16  CF-ACCESS-OF-CARRIER  REDEFINES  CF-ACCESS-CD-GENL.
00054              20  FILLER                     PIC XXX.
00055              20  CF-CARRIER-CNTL            PIC X.
00056          16  CF-ACCESS-OF-BUS-TYPE REDEFINES  CF-ACCESS-CD-GENL.
00057              20  FILLER                     PIC XX.
00058              20  CF-HI-TYPE-IN-REC          PIC 99.
00059          16  CF-ACCESS-OF-CRDB-TBL REDEFINES  CF-ACCESS-CD-GENL.
00060              20  CF-CRDB-TABLE-INDICATOR    PIC X.
00061                  88  CF-CRDB-NAIC-TABLE         VALUE '9'.
00062              20  CF-CRDB-BENEFIT-TYPE       PIC X.
00063              20  CF-CRDB-WAITING-PERIOD     PIC XX.
00064          16  CF-ACCESS-OF-CUST-RPT REDEFINES  CF-ACCESS-CD-GENL.
00065              20  FILLER                     PIC X.
00066              20  CF-CUSTOM-REPORT-NO        PIC 999.
00067          16  CF-ACCESS-OF-PLAN   REDEFINES  CF-ACCESS-CD-GENL.
00068              20  FILLER                     PIC XX.
00069              20  CF-MORTGAGE-PLAN           PIC XX.
00070          16  CF-SEQUENCE-NO                 PIC S9(4)   COMP.
00071
00072      12  CF-LAST-MAINT-DT                   PIC XX.
00073      12  CF-LAST-MAINT-BY                   PIC X(4).
00074      12  CF-LAST-MAINT-HHMMSS               PIC S9(6)   COMP-3.
00075
00076      12  CF-RECORD-BODY                     PIC X(728).
00077
00078
00079 ****************************************************************
00080 *             COMPANY MASTER RECORD                            *
00081 ****************************************************************
00082
00083      12  CF-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00084          16  CF-COMPANY-ADDRESS.
00085              20  CF-CL-MAIL-TO-NAME         PIC X(30).
00086              20  CF-CL-IN-CARE-OF           PIC X(30).
00087              20  CF-CL-ADDR-LINE-1          PIC X(30).
00088              20  CF-CL-ADDR-LINE-2          PIC X(30).
00089              20  CF-CL-CITY-STATE           PIC X(30).
00090              20  CF-CL-ZIP-CODE-NUM         PIC 9(9)    COMP-3.
00091              20  CF-CL-PHONE-NO             PIC 9(11)   COMP-3.
00092          16  CF-COMPANY-CD                  PIC X.
00093          16  CF-COMPANY-PASSWORD            PIC X(8).
00094          16  CF-SECURITY-OPTION             PIC X.
00095              88  ALL-SECURITY                   VALUE '1'.
00096              88  COMPANY-VERIFY                 VALUE '2'.
00097              88  PROCESSOR-VERIFY               VALUE '3'.
00098              88  NO-SECURITY                    VALUE '4'.
00099              88  ALL-BUT-TERM                   VALUE '5'.
00100          16  CF-CARRIER-CONTROL-LEVEL       PIC X.
00101              88  USE-ACTUAL-CARRIER             VALUE SPACE.
00102          16  CF-LGX-INTERFACE-CNTL          PIC X.
00103              88  LGX-TIME-SHR-COMPANY           VALUE '1'.
00104          16  CF-INFORCE-LOCATION            PIC X.
00105              88  CERTS-ARE-ONLINE               VALUE '1'.
00106              88  CERTS-ARE-OFFLINE              VALUE '2'.
00107              88  NO-CERTS-AVAILABLE             VALUE '3'.
00108          16  CF-LOWER-CASE-LETTERS          PIC X.
00109          16  CF-CERT-ACCESS-CONTROL         PIC X.
00110              88  CF-ST-ACCNT-CNTL               VALUE ' '.
00111              88  CF-CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
00112              88  CF-CARR-ST-ACCNT-CNTL          VALUE '2'.
00113              88  CF-ACCNT-CNTL                  VALUE '3'.
00114              88  CF-CARR-ACCNT-CNTL             VALUE '4'.
00115
00116          16  CF-FORMS-PRINTER-ID            PIC X(4).
00117          16  CF-CHECK-PRINTER-ID            PIC X(4).
00118
00119          16  CF-LGX-CREDIT-USER             PIC X.
00120              88  CO-IS-NOT-USER                 VALUE 'N'.
00121              88  CO-HAS-CLAS-IC-CREDIT          VALUE 'Y'.
00122
00123          16 CF-CREDIT-CALC-CODES.
00124              20  CF-CR-REM-TERM-CALC PIC X.
00125                88  CR-EARN-AFTER-15TH           VALUE '1'.
00126                88  CR-EARN-ON-HALF-MO           VALUE '2'.
00127                88  CR-EARN-ON-1ST-DAY           VALUE '3'.
00128                88  CR-EARN-ON-FULL-MO           VALUE '4'.
00129                88  CR-EARN-WITH-NO-DAYS         VALUE '5'.
00130                88  CR-EARN-AFTER-14TH           VALUE '6'.
00131                88  CR-EARN-AFTER-16TH           VALUE '7'.
00132              20  CF-CR-R78-METHOD           PIC X.
00133                88  USE-TERM-PLUS-ONE            VALUE SPACE.
00134                88  DONT-USE-PLUS-ONE            VALUE '1'.
00135
00136          16  CF-CLAIM-CONTROL-COUNTS.
00137              20  CF-CO-CLAIM-COUNTER        PIC S9(8)   COMP.
00138                  88  CO-CLM-COUNT-RESET         VALUE +99999.
00139
00140              20  CF-CO-ARCHIVE-COUNTER      PIC S9(8)   COMP.
00141                  88  CO-ARCHIVE-COUNT-RESET     VALUE +999999.
00142
00143              20  CF-CO-CHECK-COUNTER        PIC S9(8)   COMP.
00144                  88  CO-CHECK-COUNT-RESET       VALUE +9999999.
00145
00146              20  CF-CO-CHECK-QUE-COUNTER    PIC S9(8)   COMP.
00147                  88  CO-QUE-COUNT-RESET         VALUE +9999999.
00148
00149          16  CF-CURRENT-MONTH-END           PIC XX.
00150
00151          16  CF-CO-CALC-QUOTE-TOLERANCE.
00152              20  CF-CO-TOL-CLAIM            PIC S999V99   COMP-3.
00153              20  CF-CO-TOL-PREM             PIC S999V99   COMP-3.
00154              20  CF-CO-TOL-REFUND           PIC S999V99   COMP-3.
00155              20  CF-CO-CLAIM-REJECT-SW      PIC X.
00156                  88 CO-WARN-IF-CLAIM-OUT        VALUE SPACE.
00157                  88 CO-FORCE-IF-CLAIM-OUT       VALUE '1'.
00158              20  CF-CO-PREM-REJECT-SW       PIC X.
00159                  88 CO-WARN-IF-PREM-OUT         VALUE SPACE.
00160                  88 CO-FORCE-IF-PREM-OUT        VALUE '1'.
00161              20  CF-CO-REF-REJECT-SW        PIC X.
00162                  88 CO-WARN-IF-REF-OUT          VALUE SPACE.
00163                  88 CO-FORCE-IF-REF-OUT         VALUE '1'.
00164
00165          16  CF-CO-REPORTING-DT             PIC XX.
00166          16  CF-CO-REPORTING-MONTH-DT       PIC XX.
00167          16  CF-CO-REPORTING-MONTH-END-SW   PIC X.
00168            88  CF-CO-NOT-MONTH-END              VALUE SPACES.
00169            88  CF-CO-MONTH-END                  VALUE '1'.
00170
00171          16  CF-LGX-CLAIM-USER              PIC X.
00172              88  CO-IS-NOT-CLAIM-USER           VALUE 'N'.
00173              88  CO-HAS-CLAS-IC-CLAIM           VALUE 'Y'.
00174
00175          16  CF-CREDIT-EDIT-CONTROLS.
00176              20  CF-MIN-PREMIUM             PIC S999V99   COMP-3.
00177              20  CF-MIN-AGE                 PIC 99.
00178              20  CF-DEFAULT-AGE             PIC 99.
00179              20  CF-MIN-TERM                PIC S999      COMP-3.
00180              20  CF-MAX-TERM                PIC S999      COMP-3.
00181              20  CF-DEFAULT-SEX             PIC X.
00182              20  CF-JOINT-AGE-INPUT         PIC X.
00183                  88 CF-JOINT-AGE-IS-INPUT       VALUE '1'.
00184              20  CF-BIRTH-DATE-INPUT        PIC X.
00185                  88 CF-BIRTH-DATE-IS-INPUT      VALUE '1'.
00186              20  CF-CAR-GROUP-ACCESS-CNTL   PIC X.
00187                  88  CF-USE-ACTUAL-CARRIER      VALUE ' '.
00188                  88  CF-ZERO-CARRIER            VALUE '1'.
00189                  88  CF-ZERO-GROUPING           VALUE '2'.
00190                  88  CF-ZERO-CAR-GROUP          VALUE '3'.
00191              20  CF-EDIT-SW                 PIC X.
00192                  88  CF-START-EDIT-TONIGHT      VALUE '1'.
00193              20  CF-EDIT-RESTART-BATCH      PIC X(6).
00194              20  CF-CR-PR-METHOD            PIC X.
00195                88  USE-NORMAL-PR-METHOD         VALUE SPACE.
00196                88  ADJUST-ORIG-TERM-BY-5        VALUE '1'.
00197              20  FILLER                     PIC X.
00198
00199          16  CF-CREDIT-MISC-CONTROLS.
00200              20  CF-REIN-TABLE-SW           PIC X.
00201                  88 REIN-TABLES-ARE-USED        VALUE '1'.
00202              20  CF-COMP-TABLE-SW           PIC X.
00203                  88 COMP-TABLES-ARE-USED        VALUE '1'.
00204              20  CF-EXPERIENCE-RETENTION-AGE
00205                                             PIC S9        COMP-3.
00206              20  CF-CONVERSION-DT           PIC XX.
00207              20  CF-COMP-WRITE-OFF-AMT      PIC S999V99   COMP-3.
00208              20  CF-RUN-FREQUENCY-SW        PIC X.
00209                  88 CO-IS-PROCESSED-MONTHLY     VALUE SPACE.
00210                  88 CO-IS-PROCESSED-ON-QTR      VALUE '1'.
00211
00212              20  CF-CR-CHECK-NO-CONTROL.
00213                  24  CF-CR-CHECK-NO-METHOD    PIC X.
00214                      88  CR-CHECK-NO-MANUAL       VALUE '1'.
00215                      88  CR-CHECK-NO-AUTO-SEQ     VALUE '2'.
00216                      88  CR-CHECK-NO-AT-PRINT     VALUE '4'.
00217                  24  CF-CR-CHECK-COUNTER      PIC S9(8)   COMP.
00218                      88  CR-CHECK-CNT-RESET-VALUE VALUE +999999.
00219
00220                  24  CF-CR-CHECK-COUNT       REDEFINES
00221                      CF-CR-CHECK-COUNTER      PIC X(4).
00222
00223                  24  CF-CR-CHECK-QUE-COUNTER  PIC S9(8)  COMP.
00224                      88  CR-QUE-COUNT-RESET      VALUE +9999999.
00225
00226                  24  CF-CR-CHECK-QUE-COUNT   REDEFINES
00227                      CF-CR-CHECK-QUE-COUNTER  PIC X(4).
00228                  24  CF-MAIL-PROCESSING       PIC X.
00229                      88  MAIL-PROCESSING          VALUE 'Y'.
00230
00231          16  CF-MISC-SYSTEM-CONTROL.
00232              20  CF-SYSTEM-C                 PIC X.
00233                  88  CONFIRMATION-SYS-USED       VALUE '1'.
00234              20  CF-SYSTEM-D                 PIC X.
00235                  88  DAILY-BILL-SYS-USED         VALUE '1'.
00236              20  CF-SOC-SEC-NO-SW            PIC X.
00237                  88  SOC-SEC-NO-USED             VALUE '1'.
00238              20  CF-MEMBER-NO-SW             PIC X.
00239                  88  MEMBER-NO-USED              VALUE '1'.
00240              20  CF-TAX-ID-NUMBER            PIC X(11).
00241              20  CF-JOURNAL-FILE-ID          PIC S9(4) COMP.
00242              20  CF-PAYMENT-APPROVAL-SW      PIC X.
00243                  88  CF-PMT-APPROVAL-USED        VALUE 'Y' 'G'.
00244                  88  CF-NO-APPROVAL              VALUE ' ' 'N'.
00245                  88  CF-ALL-APPROVED             VALUE 'Y'.
00246                  88  CF-GRADUATED-APPROVAL       VALUE 'G'.
00247              20  CF-SYSTEM-E                 PIC X.
00248                  88  CF-AR-SYSTEM-USED           VALUE 'Y'.
00249
00250          16  CF-LGX-LIFE-USER               PIC X.
00251              88  CO-IS-NOT-LIFE-USER            VALUE 'N'.
00252              88  CO-HAS-CLAS-IC-LIFE            VALUE 'Y'.
00253
00254          16  CF-CR-MONTH-END-DT             PIC XX.
00255
00256          16  CF-FILE-MAINT-DATES.
00257              20  CF-LAST-BATCH-NO           PIC S9(8)   COMP.
00258                  88  CF-LAST-BATCH-RESET        VALUE +999999.
00259              20  CF-LAST-BATCH       REDEFINES
00260                  CF-LAST-BATCH-NO               PIC X(4).
00261              20  CF-RATES-FILE-MAINT-DT         PIC XX.
00262              20  CF-RATES-FILE-CREATE-DT        PIC XX.
00263              20  CF-COMMISSION-TAB-MAINT-DT     PIC XX.
00264              20  CF-COMMISSION-TAB-CREATE-DT    PIC XX.
00265              20  CF-ACCOUNT-MSTR-MAINT-DT       PIC XX.
00266              20  CF-ACCOUNT-MSTR-CREATE-DT      PIC XX.
00267              20  CF-REINSURANCE-TAB-MAINT-DT    PIC XX.
00268              20  CF-REINSURANCE-TAB-CREATE-DT   PIC XX.
00269              20  CF-COMPENSATION-MSTR-MAINT-DT  PIC XX.
00270              20  CF-COMPENSATION-MSTR-CREATE-DT PIC XX.
00271
00272          16  CF-NEXT-COMPANY-ID             PIC XXX.
00273          16  FILLER                         PIC X.
00274
00275          16  CF-ALT-MORT-CODE               PIC X(4).
00276          16  CF-MEMBER-CAPTION              PIC X(10).
00277
00278          16  CF-LIFE-ACCESS-CONTROL         PIC X.
00279              88  CF-LIFE-ST-ACCNT-CNTL          VALUE ' '.
00280              88  CF-LIFE-CARR-GRP-ST-ACCNT-CNTL VALUE '1'.
00281              88  CF-LIFE-CARR-ST-ACCNT-CNTL     VALUE '2'.
00282              88  CF-LIFE-ACCNT-CNTL             VALUE '3'.
00283              88  CF-LIFE-CARR-ACCNT-CNTL        VALUE '4'.
00284
00285          16  CF-STARTING-ARCH-NO            PIC S9(8) COMP.
00286
00287          16  CF-LIFE-OVERRIDE-L1            PIC X.
00288          16  CF-LIFE-OVERRIDE-L2            PIC XX.
00289          16  CF-LIFE-OVERRIDE-L6            PIC X(6).
00290          16  CF-LIFE-OVERRIDE-L12           PIC X(12).
00291
00292          16  CF-AH-OVERRIDE-L1              PIC X.
00293          16  CF-AH-OVERRIDE-L2              PIC XX.
00294          16  CF-AH-OVERRIDE-L6              PIC X(6).
00295          16  CF-AH-OVERRIDE-L12             PIC X(12).
00296
00297          16  CF-REPORT-CD1-CAPTION          PIC X(10).
00298          16  CF-REPORT-CD2-CAPTION          PIC X(10).
00299
00300          16  CF-CLAIM-CUTOFF-DATE           PIC XX.
00301          16  CF-AR-LAST-EL860-DT            PIC XX.
00302          16  CF-MP-MONTH-END-DT             PIC XX.
00303
00304          16  CF-MAX-NUM-PMTS-CHECK          PIC 99.
00305          16  CF-CLAIM-PAID-THRU-TO          PIC X.
00306              88  CF-CLAIM-PAID-TO               VALUE '1'.
00307
00308          16  CF-AR-MONTH-END-DT             PIC XX.
00309
00310          16  CF-CRDTCRD-USER                PIC X.
00311              88  CO-IS-NOT-CRDTCRD-USER         VALUE 'N'.
00312              88  CO-HAS-CLAS-IC-CRDTCRD         VALUE 'Y'.
00313
00314          16  CF-CC-MONTH-END-DT             PIC XX.
00315
00316          16  CF-PRINT-ADDRESS-LABELS        PIC X.
00317
00318          16  CF-MORTALITY-AGE-CALC-METHOD   PIC X.
00319              88  CF-USE-TABLE-ASSIGNED-METHOD   VALUE '1' ' '.
00320              88  CF-USE-ALL-AGE-LAST            VALUE '2'.
00321              88  CF-USE-ALL-AGE-NEAR            VALUE '3'.
00322          16  CF-CO-TOL-PREM-PCT             PIC S9V9(4)   COMP-3.
00323          16  CF-CO-TOL-REFUND-PCT           PIC S9V9(4)   COMP-3.
00324          16  CF-CO-TOL-CAP                  PIC S9(3)V99  COMP-3.
00325          16  CF-CO-RESERVE-OPTION-SWITCH    PIC  X.
00326              88  OPTIONAL-RESERVE-METHOD-AUTH    VALUE 'Y'.
00327              88  OPTIONAL-RESERVE-MTHD-NOT-AUTH  VALUE ' ' 'N'.
00328          16  CF-CO-IBNR-LAG-MONTHS          PIC S9(3)     COMP-3.
00329          16  CF-CO-CIDA-TABLE-DISCOUNT-PCT  PIC S9V9(4)   COMP-3.
00330          16  CF-CO-CRDB-TABLE-SELECTION     PIC  X.
00331              88  NIAC-CREDIBILITY-TABLE          VALUE '9'.
00332          16  CF-CO-ST-CALL-RPT-CNTL         PIC  X.
00333
00334          16  CF-CL-ZIP-CODE.
00335              20  CF-CL-ZIP-PRIME.
00336                  24  CF-CL-ZIP-1ST          PIC X.
00337                      88  CF-CL-CAN-POST-CODE  VALUE 'A' THRU 'Z'.
00338                  24  FILLER                 PIC X(4).
00339              20  CF-CL-ZIP-PLUS4            PIC X(4).
00340          16  CF-CL-CANADIAN-POSTAL-CODE REDEFINES CF-CL-ZIP-CODE.
00341              20  CF-CL-CAN-POSTAL-1         PIC XXX.
00342              20  CF-CL-CAN-POSTAL-2         PIC XXX.
00343              20  FILLER                     PIC XXX.
00344
00345          16  CF-CO-CALCULATION-INTEREST     PIC S9V9(4)  COMP-3.
00346          16  CF-CO-IBNR-AH-FACTOR           PIC S9V9(4)  COMP-3.
00347          16  CF-CO-IBNR-LIFE-FACTOR         PIC S9V9(4)  COMP-3.
00348          16  CF-CO-OPTION-START-DATE        PIC XX.
00349          16  CF-REM-TRM-CALC-OPTION         PIC X.
00350            88  CF-VALID-REM-TRM-OPTION          VALUE '1' '2'
00351                                                       '3' '4'.
00352            88  CF-CONSIDER-EXTENSION            VALUE '3' '4'.
00353            88  CF-30-DAY-MONTH                  VALUE '1' '3'.
00354            88  CF-NO-EXT-30-DAY-MONTH           VALUE '1'.
00355            88  CF-NO-EXT-ACTUAL-DAYS            VALUE '2'.
00356            88  CF-EXT-30-DAY-MONTH              VALUE '3'.
00357            88  CF-EXT-ACTUAL-DAYS               VALUE '4'.
00358
00359          16  CF-DEFAULT-APR                 PIC S999V9(4) COMP-3.
00360
00361          16  CF-PAYMENT-APPROVAL-LEVELS.
00362              20  CF-LIFE-PAY-APP-LEVEL-1    PIC S9(7)   COMP-3.
00363              20  CF-LIFE-PAY-APP-LEVEL-2    PIC S9(7)   COMP-3.
00364              20  CF-LIFE-PAY-APP-LEVEL-3    PIC S9(7)   COMP-3.
00365              20  CF-AH-PAY-APP-LEVEL-1      PIC S9(7)   COMP-3.
00366              20  CF-AH-PAY-APP-LEVEL-2      PIC S9(7)   COMP-3.
00367              20  CF-AH-PAY-APP-LEVEL-3      PIC S9(7)   COMP-3.
00368
00369          16  CF-END-USER-REPORTING-USER     PIC X.
00370              88  CO-NO-END-USER-REPORTING       VALUE 'N'.
00371              88  CO-USES-END-USER-REPORTING     VALUE 'Y'.
00372
00373          16  CF-CLAIMS-CHECK-RECON-USER     PIC X.
00374              88  CO-NO-USE-CLAIMS-RECON         VALUE 'N'.
00375              88  CO-USES-CLAIMS-RECON           VALUE 'Y'.
00376
00377          16  CF-CLAIMS-LAST-PROCESS-DT      PIC XX.
00378
071508         16  CF-CREDIT-REF-SSN-CNT          PIC S9(5)  COMP-3.
00379          16  FILLER                         PIC X.
00380
00381          16  CF-CREDIT-ARCHIVE-CNTL.
00382              20  CF-CREDIT-LAST-ARCH-NUM    PIC S9(9)  COMP-3.
00383              20  CF-CREDIT-START-ARCH-NUM   PIC S9(9)  COMP-3.
00384              20  CF-CREDIT-ARCH-PURGE-YR    PIC S9     COMP-3.
00385
00386          16  CF-CR-PRINT-ADDRESS-LABELS     PIC X.
00387
00388          16  CF-CLAIMS-AUDIT-CHANGES        PIC X.
00389              88  CO-NO-USE-AUDIT-CHANGES        VALUE 'N'.
00390              88  CO-USES-AUDIT-CHANGES          VALUE 'Y'.
00391
00392          16  CF-CLAIMS-CREDIT-CARD-INDEX    PIC X.
00393              88  CO-NO-USE-CREDIT-CARD-INDEX    VALUE 'N'.
00394              88  CO-USES-CREDIT-CARD-INDEX      VALUE 'Y'.
00395
00396          16  CF-CLAIMS-LETTER-MAINT-DAYS    PIC 99.
00397
00398          16  CF-CO-ACH-ID-CODE              PIC  X.
00399              88  CF-CO-ACH-ICD-IRS-EIN          VALUE '1'.
00400              88  CF-CO-ACH-ICD-DUNS             VALUE '2'.
00401              88  CF-CO-ACH-ICD-USER-NO          VALUE '3'.
00402          16  CF-CO-ACH-CLAIM-SEND-NAME      PIC X(23).
00403          16  CF-CO-ACH-CLAIM-BK-NO          PIC X(09).
00404          16  CF-CO-ACH-ADMIN-SEND-NAME      PIC X(23).
00405          16  CF-CO-ACH-ADMIN-NO             PIC X(09).
00406          16  CF-CO-ACH-RECV-NAME            PIC X(23).
00407          16  CF-CO-ACH-RECV-NO              PIC X(08).
00408          16  CF-CO-ACH-ORIGINATOR-NO        PIC X(08).
00409          16  CF-CO-ACH-COMPANY-ID           PIC X(09).
00410          16  CF-CO-ACH-TRACE-NO             PIC 9(07) COMP.
00411                  88  CO-ACH-TRACE-NO-RESET      VALUE 9999999.
00412          16  CF-CO-ACH-TRACE-SPACE REDEFINES
00413                  CF-CO-ACH-TRACE-NO         PIC X(4).
00414
00415          16  CF-CO-OVER-SHORT.
00416              20 CF-CO-OVR-SHT-AMT           PIC S999V99   COMP-3.
00417              20 CF-CO-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
00418
031808*         16  FILLER                         PIC X(102).
031808         16  CF-PAYMENT-APPROVAL-LEVELS-2.
031808             20  CF-LIFE-PAY-APP-LEVEL-4    PIC S9(7)   COMP-3.
031808             20  CF-AH-PAY-APP-LEVEL-4      PIC S9(7)   COMP-3.
031808
031808         16  CF-AH-APPROVAL-DAYS.
031808             20  CF-AH-APP-DAY-LEVEL-1     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-2     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-3     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-4     PIC S9(5)   COMP-3.
032813
032813         16  CF-CO-REAUDIT-INTERVAL        PIC S9(5)   COMP-3.
031808
091813         16  CF-APPROV-LEV-5.
091813             20  CF-LIFE-PAY-APP-LEVEL-5    PIC S9(7)   COMP-3.
091813             20  CF-AH-PAY-APP-LEVEL-5      PIC S9(7)   COMP-3.
091813             20  CF-AH-APP-DAY-LEVEL-5      PIC S9(5)   COMP-3.
091813
091813         16  FILLER                         PIC X(68).
00421 ****************************************************************
00422 *             PROCESSOR/USER RECORD                            *
00423 ****************************************************************
00424
00425      12  CF-PROCESSOR-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00426          16  CF-PROCESSOR-NAME              PIC X(30).
00427          16  CF-PROCESSOR-PASSWORD          PIC X(11).
00428          16  CF-PROCESSOR-TITLE             PIC X(26).
00429          16  CF-MESSAGE-AT-LOGON-CAP        PIC X.
00430                  88  MESSAGE-YES                VALUE 'Y'.
00431                  88  MESSAGE-NO                 VALUE ' ' 'N'.
00432
00433 *****************************************************
00434 ****  OCCURRENCE (1) CREDIT APPLICATIONS         ****
00435 ****  OCCURRENCE (2) CLAIMS APPLICATIONS         ****
00436 ****  OCCURRENCE (3) CREDIT CARD APPLICATIONS    ****
00437 ****  OCCURRENCE (4) ACCT RECV APPLICATIONS      ****
00438 *****************************************************
00439
00440          16  CF-SYSTEM-SECURITY  OCCURS  4 TIMES.
00441              20  CF-ADMINISTRATION-CONTROLS PIC XX.
00442              20  CF-APPLICATION-FORCE       PIC X.
00443              20  CF-INDIVIDUAL-APP.
00444                  24  CF-APP-SWITCHES  OCCURS  44 TIMES.
00445                      28  CF-BROWSE-APP      PIC X.
00446                      28  CF-UPDATE-APP      PIC X.
00447
00448          16  CF-CURRENT-TERM-ON             PIC X(4).
00449          16  CF-PROCESSOR-LIMITS-CLAIMS.
00450              20  CF-PROC-CALC-AMT-TOL       PIC S999V99   COMP-3.
00451              20  CF-PROC-MAX-REG-PMT        PIC S9(7)V99  COMP-3.
00452              20  CF-PROC-MAX-REG-DAYS       PIC S999      COMP-3.
00453              20  CF-PROC-MAX-AUTO-PMT       PIC S9(7)V99  COMP-3.
00454              20  CF-PROC-MAX-AUTO-MOS       PIC S999      COMP-3.
00455              20  CF-PROC-CALC-DAYS-TOL      PIC S999      COMP-3.
00456              20  CF-PROC-MAX-LF-PMT         PIC S9(7)V99  COMP-3.
00457          16  CF-PROCESSOR-CARRIER           PIC X.
00458              88  NO-CARRIER-SECURITY            VALUE ' '.
00459          16  CF-PROCESSOR-ACCOUNT           PIC X(10).
00460              88  NO-ACCOUNT-SECURITY            VALUE SPACES.
00461          16  CF-PROCESSOR-LIFE-ACCESS       PIC X.
00462              88  PROCESSOR-HAS-LIFE-ACCESS      VALUE 'Y'.
00463          16  CF-PROCESSOR-USER-ALMIGHTY     PIC X.
00464              88  PROCESSOR-USER-IS-ALMIGHTY     VALUE 'Y'.
00465
00466          16  CF-PROC-SYS-ACCESS-SW.
00467              20  CF-PROC-CREDIT-CLAIMS-SW.
00468                  24  CF-PROC-SYS-ACCESS-CREDIT  PIC X.
00469                      88  ACCESS-TO-CREDIT           VALUE 'Y'.
00470                  24  CF-PROC-SYS-ACCESS-CLAIMS  PIC X.
00471                      88  ACCESS-TO-CLAIMS           VALUE 'Y'.
00472              20  CF-PROC-CREDIT-CLAIMS   REDEFINES
00473                  CF-PROC-CREDIT-CLAIMS-SW       PIC XX.
00474                  88  ACCESS-TO-CLAIM-CREDIT         VALUE 'YY'.
00475              20  CF-PROC-LIFE-GNRLDGR-SW.
00476                  24  CF-PROC-SYS-ACCESS-LIFE    PIC X.
00477                      88  ACCESS-TO-LIFE             VALUE 'Y'.
00478                  24  CF-PROC-SYS-ACCESS-GNRLDGR PIC X.
00479                      88  ACCESS-TO-GNRLDGR          VALUE 'Y'.
00480              20  CF-PROC-LIFE-GNRLDGR    REDEFINES
00481                  CF-PROC-LIFE-GNRLDGR-SW        PIC XX.
00482                  88  ACCESS-TO-LIFE-GNRLDGR         VALUE 'YY'.
00483          16  CF-PROC-SYS-ACCESS-ALL      REDEFINES
00484              CF-PROC-SYS-ACCESS-SW              PIC X(4).
00485              88  ACCESS-TO-ALL-SYSTEMS              VALUE 'YYYY'.
00486          16  CF-PROCESSOR-PRINTER               PIC X(4).
00487
00488          16  CF-APPROVAL-LEVEL                  PIC X.
00489              88  APPROVAL-LEVEL-1                   VALUE '1'.
00490              88  APPROVAL-LEVEL-2                   VALUE '2'.
00491              88  APPROVAL-LEVEL-3                   VALUE '3'.
031808             88  APPROVAL-LEVEL-4                   VALUE '4'.
091813             88  APPROVAL-LEVEL-5                   VALUE '5'.
00492
00493          16  CF-PROC-MAX-EXP-PMT            PIC S9(7)V99  COMP-3.
00494
00495          16  CF-LANGUAGE-TYPE                   PIC X.
00496              88  CF-LANG-IS-ENG                     VALUE 'E'.
00497              88  CF-LANG-IS-FR                      VALUE 'F'.
011812
011812         16  CF-CSR-IND                         PIC X.
011812         16  FILLER                             PIC X(239).
00499
00500 ****************************************************************
00501 *             PROCESSOR/REMINDERS RECORD                       *
00502 ****************************************************************
00503
00504      12  CF-PROCESSOR-REMINDER-REC  REDEFINES  CF-RECORD-BODY.
00505          16  CF-PROCESSOR-REMINDERS  OCCURS 8 TIMES.
00506              20  CF-START-REMIND-DT         PIC XX.
00507              20  CF-END-REMIND-DT           PIC XX.
00508              20  CF-REMINDER-TEXT           PIC X(50).
00509          16  FILLER                         PIC X(296).
00510
00511
00512 ****************************************************************
00513 *             STATE MASTER RECORD                              *
00514 ****************************************************************
00515
00516      12  CF-STATE-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00517          16  CF-STATE-ABBREVIATION          PIC XX.
00518          16  CF-STATE-NAME                  PIC X(25).
00519          16  CF-ST-CALC-INTEREST            PIC S9V9(4)   COMP-3.
00520          16  CF-ST-CALC-QUOTE-TOLERANCE.
00521              20  CF-ST-TOL-CLAIM            PIC S999V99   COMP-3.
00522              20  CF-ST-TOL-PREM             PIC S999V99   COMP-3.
00523              20  CF-ST-TOL-REFUND           PIC S999V99   COMP-3.
00524              20  CF-ST-CLAIM-REJECT-SW      PIC X.
00525                  88 ST-WARN-IF-CLAIM-OUT        VALUE SPACE.
00526                  88 ST-FORCE-IF-CLAIM-OUT       VALUE '1'.
00527              20  CF-ST-PREM-REJECT-SW       PIC X.
00528                  88 ST-WARN-IF-PREM-OUT         VALUE SPACE.
00529                  88 ST-FORCE-IF-PREM-OUT        VALUE '1'.
00530              20  CF-ST-REF-REJECT-SW        PIC X.
00531                  88 ST-WARN-IF-REF-OUT          VALUE SPACE.
00532                  88 ST-FORCE-IF-REF-OUT         VALUE '1'.
00533          16  CF-ST-LF-EXP-PCT               PIC S999V9(4) COMP-3.
00534          16  CF-ST-AH-EXP-PCT               PIC S999V9(4) COMP-3.
00535          16  CF-ST-REFUND-RULES.
00536              20  CF-ST-REFUND-MIN           PIC S999V99    COMP-3.
00537              20  CF-ST-REFUND-DAYS-FIRST    PIC 99.
00538              20  CF-ST-REFUND-DAYS-SUBSEQ   PIC 99.
00539          16  CF-ST-FST-PMT-EXTENSION.
00540              20  CF-ST-FST-PMT-DAYS-MAX     PIC 999.
00541              20  CF-ST-FST-PMT-DAYS-CHG     PIC X.
00542                  88  CF-ST-EXT-NO-CHG           VALUE ' '.
00543                  88  CF-ST-EXT-CHG-LF           VALUE '1'.
00544                  88  CF-ST-EXT-CHG-AH           VALUE '2'.
00545                  88  CF-ST-EXT-CHG-LF-AH        VALUE '3'.
00546          16  CF-ST-STATE-CALL.
00547              20  CF-ST-CALL-UNEARNED        PIC X.
00548              20  CF-ST-CALL-RPT-CNTL        PIC X.
00549              20  CF-ST-CALL-RATE-DEV        PIC XXX.
00550          16  CF-REPLACEMENT-LAW-SW          PIC X.
00551              88  CF-REPLACEMENT-LAW-APPLIES     VALUE 'Y'.
00552              88  CF-REPL-LAW-NOT-APPLICABLE     VALUE 'N'.
00553          16  CF-REPLACEMENT-LETTER          PIC X(4).
00554          16  CF-ST-TOL-PREM-PCT             PIC S9V9999 COMP-3.
00555          16  CF-ST-TOL-REF-PCT              PIC S9V9999 COMP-3.
00556          16  CF-ST-TARGET-LOSS-RATIO        PIC S9V9(4) COMP-3.
00557          16  CF-ST-SPLIT-PAYMENT            PIC X.
00558          16  FILLER                         PIC X.
00559          16  CF-STATE-BENEFIT-CNTL  OCCURS 50 TIMES.
00560              20  CF-ST-BENEFIT-CD           PIC XX.
00561              20  CF-ST-BENEFIT-KIND         PIC X.
00562                  88  CF-ST-LIFE-KIND            VALUE 'L'.
00563                  88  CF-ST-AH-KIND              VALUE 'A'.
00564              20  CF-ST-REM-TERM-CALC        PIC X.
00565                  88  ST-REM-TERM-NOT-USED       VALUE SPACE.
00566                  88  ST-EARN-AFTER-15TH         VALUE '1'.
00567                  88  ST-EARN-ON-HALF-MO         VALUE '2'.
00568                  88  ST-EARN-ON-1ST-DAY         VALUE '3'.
00569                  88  ST-EARN-ON-FULL-MO         VALUE '4'.
00570                  88  ST-EARN-WITH-NO-DAYS       VALUE '5'.
00571                  88  ST-EARN-AFTER-14TH         VALUE '6'.
00572                  88  ST-EARN-AFTER-16TH         VALUE '7'.
00573
00574              20  CF-ST-REFUND-CALC          PIC X.
00575                  88  ST-REFUND-NOT-USED         VALUE SPACE.
00576                  88  ST-REFD-BY-R78             VALUE '1'.
00577                  88  ST-REFD-BY-PRO-RATA        VALUE '2'.
00578                  88  ST-REFD-AS-CALIF           VALUE '3'.
00579                  88  ST-REFD-AS-TEXAS           VALUE '4'.
00580                  88  ST-REFD-IS-NET-PAY         VALUE '5'.
00581                  88  ST-REFD-ANTICIPATION       VALUE '6'.
00582                  88  ST-REFD-UTAH               VALUE '7'.
00583                  88  ST-REFD-SUM-OF-DIGITS      VALUE '9'.
00584                  88  ST-REFD-REG-BALLOON        VALUE 'B'.
033104                 88  ST-REFD-GAP-NON-REFUND     VALUE 'G'.
00585
00586              20  CF-ST-EARNING-CALC         PIC X.
00587                  88  ST-EARNING-NOT-USED        VALUE SPACE.
00588                  88  ST-EARN-BY-R78             VALUE '1'.
00589                  88  ST-EARN-BY-PRO-RATA        VALUE '2'.
00590                  88  ST-EARN-AS-CALIF           VALUE '3'.
00591                  88  ST-EARN-AS-TEXAS           VALUE '4'.
00592                  88  ST-EARN-IS-NET-PAY         VALUE '5'.
00593                  88  ST-EARN-ANTICIPATION       VALUE '6'.
00594                  88  ST-EARN-MEAN               VALUE '8'.
00595                  88  ST-EARN-REG-BALLOON        VALUE 'B'.
00596
00597              20  CF-ST-OVRD-EARNINGS-CALC   PIC X.
00598                  88  ST-OVERRIDE-NOT-USED       VALUE SPACE.
00599                  88  ST-OVRD-BY-R78             VALUE '1'.
00600                  88  ST-OVRD-BY-PRO-RATA        VALUE '2'.
00601                  88  ST-OVRD-AS-CALIF           VALUE '3'.
00602                  88  ST-OVRD-AS-TEXAS           VALUE '4'.
00603                  88  ST-OVRD-IS-NET-PAY         VALUE '5'.
00604                  88  ST-OVRD-ANTICIPATION       VALUE '6'.
00605                  88  ST-OVRD-MEAN               VALUE '8'.
00606                  88  ST-OVRD-REG-BALLOON        VALUE 'B'.
                   20  cf-st-extra-periods        pic 9.
00607 *            20  FILLER                     PIC X.
00608
00609          16  CF-ST-COMMISSION-CAPS.
00610              20  CF-ST-COMM-CAP-SL          PIC S9V9(4) COMP-3.
00611              20  CF-ST-COMM-CAP-JL          PIC S9V9(4) COMP-3.
00612              20  CF-ST-COMM-CAP-SA          PIC S9V9(4) COMP-3.
00613              20  CF-ST-COMM-CAP-JA          PIC S9V9(4) COMP-3.
00614          16  CF-COMM-CAP-LIMIT-TO           PIC X.
00615                  88  ST-LIMIT-TO-ACCOUNT        VALUE 'A'.
102717                 88  ST-LIMIT-TO-GA             VALUE 'G'.
102717                 88  ST-LIMIT-TO-BOTH           VALUE 'B'.
00616
00617          16  CF-ST-RES-TAX-PCT              PIC S9V9(4) COMP-3.
00618
00619          16  CF-ST-STATUTORY-INTEREST.
00620              20  CF-ST-STAT-DATE-FROM       PIC X.
00621                  88  ST-STAT-FROM-INCURRED      VALUE 'I'.
00622                  88  ST-STAT-FROM-REPORTED      VALUE 'R'.
00623              20  CF-ST-NO-DAYS-ELAPSED      PIC 99.
00624              20  CF-ST-STAT-INTEREST        PIC S9V9(4) COMP-3.
00625              20  CF-ST-STAT-INTEREST-1      PIC S9V9(4) COMP-3.
00626              20  CF-ST-STAT-INTEREST-2      PIC S9V9(4) COMP-3.
00627              20  CF-ST-STAT-INTEREST-3      PIC S9V9(4) COMP-3.
00628
00629          16  CF-ST-OVER-SHORT.
00630              20 CF-ST-OVR-SHT-AMT           PIC S999V99 COMP-3.
00631              20 CF-ST-OVR-SHT-PCT           PIC S9V9(4) COMP-3.
00632
00633          16  CF-ST-FREE-LOOK-PERIOD         PIC S9(3)   COMP-3.
00634
CIDMOD         16  CF-ST-RT-CALC                  PIC X.
CIDMOD
PEMMOD         16  CF-ST-LF-PREM-TAX              PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-AH-PREM-TAX-I            PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-AH-PREM-TAX-G            PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-RF-LR-CALC               PIC X.
PEMMOD         16  CF-ST-RF-LL-CALC               PIC X.
PEMMOD         16  CF-ST-RF-LN-CALC               PIC X.
PEMMOD         16  CF-ST-RF-AH-CALC               PIC X.
PEMMOD         16  CF-ST-RF-CP-CALC               PIC X.
PEMMOD*        16  FILLER                         PIC X(206).
091808*CIDMOD         16  FILLER                         PIC X(192).
091808         16  CF-ST-CHECK-COUNTER            PIC S9(8)   COMP.
091808             88  CF-ST-CHECK-CNT-RESET      VALUE +9999999.
011410         16  CF-ST-REF-AH-DEATH-IND         PIC X.
061511         16  CF-ST-VFY-2ND-BENE             PIC X.
012913         16  CF-ST-CAUSAL-STATE             PIC X.
022415         16  CF-ST-EXTRA-INTEREST-PERIODS   PIC 9.
022415         16  CF-ST-EXTRA-PAYMENTS           PIC 9.
040915         16  CF-ST-AGENT-SIG-EDIT           PIC X.
040915             88  CF-ST-EDIT-FOR-SIG           VALUE 'Y'.
070115         16  CF-ST-NET-ONLY-STATE           PIC X.
070115             88  CF-ST-IS-NET-ONLY            VALUE 'Y'.
102717         16  cf-commission-cap-required     pic x.
102717         16  CF-ST-GA-COMMISSION-CAPS.
102717             20  CF-ST-GA-COMM-CAP-SL       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-JL       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-SA       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-JA       PIC S9V9(4) COMP-3.
102717         16  CF-ST-TOT-COMMISSION-CAPS.
102717             20  CF-ST-TOT-COMM-CAP-SL      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-JL      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-SA      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-JA      PIC S9V9(4) COMP-3.
102717         16  FILLER                         PIC X(156).
00636
00637 ****************************************************************
00638 *             BENEFIT MASTER RECORD                            *
00639 ****************************************************************
00640
00641      12  CF-BENEFIT-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00642          16  CF-BENEFIT-CONTROLS  OCCURS 8 TIMES.
00643              20  CF-BENEFIT-CODE            PIC XX.
00644              20  CF-BENEFIT-NUMERIC  REDEFINES
00645                  CF-BENEFIT-CODE            PIC XX.
00646              20  CF-BENEFIT-ALPHA           PIC XXX.
00647              20  CF-BENEFIT-DESCRIP         PIC X(10).
00648              20  CF-BENEFIT-COMMENT         PIC X(10).
00649
00650              20  CF-LF-COVERAGE-TYPE        PIC X.
00651                  88  CF-REDUCING                VALUE 'R'.
00652                  88  CF-LEVEL                   VALUE 'L' 'P'.
00653
00654              20  CF-SPECIAL-CALC-CD         PIC X.
00655                  88  CF-ALTERNATE-NET-PAY       VALUE 'A'.
00656                  88  CF-NP-0-MO-INT             VALUE 'A'.
00657                  88  CF-OB-OFFLINE-RESERVED     VALUE 'B'.
00658                  88  CF-CRITICAL-PERIOD         VALUE 'C'.
00659                  88  CF-TERM-IN-DAYS            VALUE 'D'.
00660                  88  CF-USE-PREMIUM-AS-ENTERED  VALUE 'E'.
00661                  88  CF-FARM-PLAN               VALUE 'F'.
00662                  88  CF-RATE-AS-STANDARD        VALUE 'G'.
00663                  88  CF-2-MTH-INTEREST          VALUE 'I'.
00664                  88  CF-3-MTH-INTEREST          VALUE 'J'.
00665                  88  CF-4-MTH-INTEREST          VALUE 'K'.
00666                  88  CF-BALLOON-LAST-PMT        VALUE 'L'.
00667                  88  CF-MORTGAGE-PROCESSING     VALUE 'M'.
00668                  88  CF-PRUDENTIAL              VALUE 'P'.
00669                  88  CF-OUTSTANDING-BAL         VALUE 'O'.
00670                  88  CF-TRUNCATED-LIFE          VALUE 'T'.
00671                  88  CF-TRUNCATED-LIFE-ONE      VALUE 'U'.
00672                  88  CF-TRUNCATED-LIFE-TWO      VALUE 'V'.
00673                  88  CF-NET-PAY-SIMPLE          VALUE 'S'.
00674                  88  CF-SUMMARY-PROCESSING      VALUE 'Z'.
00675
00676              20  CF-JOINT-INDICATOR         PIC X.
00677                  88  CF-JOINT-COVERAGE          VALUE 'J'.
00678
082603*            20  FILLER                     PIC X(12).
                   20  cf-maximum-benefits        pic s999 comp-3.
                   20  FILLER                     PIC X(09).
082503             20  CF-BENEFIT-CATEGORY        PIC X.
00680              20  CF-LOAN-TYPE               PIC X(8).
00681
00682              20  CF-CO-REM-TERM-CALC        PIC X.
00683                  88  CO-EARN-AFTER-15TH         VALUE '1'.
00684                  88  CO-EARN-ON-HALF-MO         VALUE '2'.
00685                  88  CO-EARN-ON-1ST-DAY         VALUE '3'.
00686                  88  CO-EARN-ON-FULL-MO         VALUE '4'.
00687                  88  CO-EARN-WITH-NO-DAYS       VALUE '5'.
00688
00689              20  CF-CO-EARNINGS-CALC        PIC X.
00690                  88  CO-EARN-BY-R78             VALUE '1'.
00691                  88  CO-EARN-BY-PRO-RATA        VALUE '2'.
00692                  88  CO-EARN-AS-CALIF           VALUE '3'.
00693                  88  CO-EARN-AS-TEXAS           VALUE '4'.
00694                  88  CO-EARN-IS-NET-PAY         VALUE '5'.
00695                  88  CO-EARN-ANTICIPATION       VALUE '6'.
00696                  88  CO-EARN-AS-MEAN            VALUE '8'.
00697                  88  CO-EARN-AS-REG-BALLOON     VALUE 'B'.
00698
00699              20  CF-CO-REFUND-CALC          PIC X.
00700                  88  CO-REFUND-NOT-USED         VALUE SPACE.
00701                  88  CO-REFD-BY-R78             VALUE '1'.
00702                  88  CO-REFD-BY-PRO-RATA        VALUE '2'.
00703                  88  CO-REFD-AS-CALIF           VALUE '3'.
00704                  88  CO-REFD-AS-TEXAS           VALUE '4'.
00705                  88  CO-REFD-IS-NET-PAY         VALUE '5'.
00706                  88  CO-REFD-ANTICIPATION       VALUE '6'.
00707                  88  CO-REFD-MEAN               VALUE '8'.
00708                  88  CO-REFD-SUM-OF-DIGITS      VALUE '9'.
00709                  88  CO-REFD-AS-REG-BALLOON     VALUE 'B'.
033104                 88  CO-REFD-GAP-NON-REFUND     VALUE 'G'.
00710
00711              20  CF-CO-OVRD-EARNINGS-CALC   PIC X.
00712                  88  CO-OVERRIDE-NOT-USED       VALUE SPACE.
00713                  88  CO-OVRD-BY-R78             VALUE '1'.
00714                  88  CO-OVRD-BY-PRO-RATA        VALUE '2'.
00715                  88  CO-OVRD-AS-CALIF           VALUE '3'.
00716                  88  CO-OVRD-AS-TEXAS           VALUE '4'.
00717                  88  CO-OVRD-IS-NET-PAY         VALUE '5'.
00718                  88  CO-OVRD-ANTICIPATION       VALUE '6'.
00719                  88  CO-OVRD-MEAN               VALUE '8'.
00720                  88  CO-OVRD-AS-REG-BALLOON     VALUE 'B'.
00721
00722              20  CF-CO-BEN-I-G-CD           PIC X.
00723                  88  CO-BEN-I-G-NOT-USED        VALUE SPACE.
00724                  88  CO-BEN-I-G-IS-INDV         VALUE 'I'.
00725                  88  CO-BEN-I-G-IS-GRP          VALUE 'G'.
00726
00727          16  FILLER                         PIC X(304).
00728
00729
00730 ****************************************************************
00731 *             CARRIER MASTER RECORD                            *
00732 ****************************************************************
00733
00734      12  CF-CARRIER-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00735          16  CF-ADDRESS-DATA.
00736              20  CF-MAIL-TO-NAME            PIC X(30).
00737              20  CF-IN-CARE-OF              PIC X(30).
00738              20  CF-ADDRESS-LINE-1          PIC X(30).
00739              20  CF-ADDRESS-LINE-2          PIC X(30).
00740              20  CF-CITY-STATE              PIC X(30).
00741              20  CF-ZIP-CODE-NUM            PIC 9(9)      COMP-3.
00742              20  CF-PHONE-NO                PIC 9(11)     COMP-3.
00743
00744          16  CF-CLAIM-NO-CONTROL.
00745              20  CF-CLAIM-NO-METHOD         PIC X.
00746                  88  CLAIM-NO-MANUAL            VALUE '1'.
00747                  88  CLAIM-NO-Y-M-SEQ           VALUE '2'.
00748                  88  CLAIM-NO-SEQ               VALUE '3'.
00749                  88  CLAIM-NO-ALPHA-SEQ         VALUE '5'.
00750              20  CF-CLAIM-COUNTER           PIC S9(8)   COMP.
00751                  88  CLAIM-CNT-RESET-IF-SEQ     VALUE +9999999.
00752                  88  CLAIM-CNT-RESET-IF-YRMO    VALUE +99999.
00753                  88  CLAIM-CNT-RESET-IF-YRALPHA VALUE +9999.
00754
00755          16  CF-CHECK-NO-CONTROL.
00756              20  CF-CHECK-NO-METHOD         PIC X.
00757                  88  CHECK-NO-MANUAL            VALUE '1'.
00758                  88  CHECK-NO-AUTO-SEQ          VALUE '2'.
00759                  88  CHECK-NO-CARR-SEQ          VALUE '3'.
00760                  88  CHECK-NO-AT-PRINT          VALUE '4'.
00761              20  CF-CHECK-COUNTER           PIC S9(8)   COMP.
00762                  88  CHECK-CNT-RESET-VALUE      VALUE +999999.
00763
00764          16  CF-DOMICILE-STATE              PIC XX.
00765
00766          16  CF-EXPENSE-CONTROLS.
00767              20  CF-EXPENSE-METHOD          PIC X.
00768                  88  EXPENSE-CALC-MANUAL        VALUE '1'.
00769                  88  DOLLARS-PER-PMT            VALUE '2'.
00770                  88  PERCENT-OF-PAYMENT         VALUE '3'.
00771                  88  DOLLARS-PER-MONTH          VALUE '4'.
00772              20  CF-EXPENSE-PERCENT         PIC S999V99   COMP-3.
00773              20  CF-EXPENSE-DOLLAR          PIC S999V99   COMP-3.
00774
00775          16  CF-CORRESPONDENCE-CONTROL.
00776              20  CF-LETTER-RESEND-OPT       PIC X.
00777                  88  LETTERS-NOT-ARCHIVED       VALUE SPACE.
00778                  88  LETTERS-ARE-ARCHIVED       VALUE '1'.
00779              20  FILLER                     PIC X(4).
00780
00781          16  CF-RESERVE-CONTROLS.
00782              20  CF-MANUAL-SW               PIC X.
00783                  88  CF-MANUAL-RESERVES-USED    VALUE '1'.
00784              20  CF-FUTURE-SW               PIC X.
00785                  88  CF-FUTURE-RESERVES-USED    VALUE '1'.
00786              20  CF-PTC-SW                  PIC X.
00787                  88  CF-PAY-TO-CURRENT-USED     VALUE '1'.
00788              20  CF-IBNR-SW                 PIC X.
00789                  88  CF-IBNR-RESERVES-USED      VALUE '1'.
00790              20  CF-PTC-LF-SW               PIC X.
00791                  88  CF-LF-PTC-USED             VALUE '1'.
00792              20  CF-CDT-ACCESS-METHOD       PIC X.
00793                  88  CF-CDT-ROUND-NEAR          VALUE '1'.
00794                  88  CF-CDT-ROUND-HIGH          VALUE '2'.
00795                  88  CF-CDT-INTERPOLATED        VALUE '3'.
00796              20  CF-PERCENT-OF-CDT          PIC S999V99   COMP-3.
00797
00798          16  CF-CLAIM-CALC-METHOD           PIC X.
00799              88  360-PLUS-MONTHS                VALUE '1'.
00800              88  365-PLUS-MONTHS                VALUE '2'.
00801              88  FULL-MONTHS-ACTUAL-DAY         VALUE '3'.
00802              88  360-DAILY                      VALUE '4'.
00803              88  365-DAILY                      VALUE '5'.
00804
00805          16  CF-LAST-ALPHA-CHARACTER        PIC X.
00806          16  FILLER                         PIC X(11).
00807
00808          16  CF-LIMIT-AMOUNTS.
00809              20  CF-CALC-AMT-TOL            PIC S999V99   COMP-3.
00810              20  CF-MAX-REG-PMT             PIC S9(7)V99  COMP-3.
00811              20  CF-MAX-REG-DAYS            PIC S999      COMP-3.
00812              20  CF-MAX-AUTO-PMT            PIC S9(7)V99  COMP-3.
00813              20  CF-MAX-AUTO-MOS            PIC S999      COMP-3.
00814              20  CF-CALC-DAYS-TOL           PIC S999      COMP-3.
00815              20  CF-CR-TOL-PREM             PIC S999V99   COMP-3.
00816              20  CF-CR-TOL-REFUND           PIC S999V99   COMP-3.
00817              20  CF-CR-TOL-PREM-PCT         PIC S9V9(4)   COMP-3.
00818              20  CF-CR-TOL-REFUND-PCT       PIC S9V9(4)   COMP-3.
00819
00820          16  CF-DAYS-BEFORE-CLOSED          PIC S999      COMP-3.
00821          16  CF-MONTHS-BEFORE-PURGED        PIC S999      COMP-3.
00822          16  CF-IBNR-PERCENT                PIC S9V9(4)   COMP-3.
00823
00824          16  CF-ZIP-CODE.
00825              20  CF-ZIP-PRIME.
00826                  24  CF-ZIP-1ST             PIC X.
00827                      88  CF-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00828                  24  FILLER                 PIC X(4).
00829              20  CF-ZIP-PLUS4               PIC X(4).
00830          16  CF-CANADIAN-POSTAL-CODE REDEFINES CF-ZIP-CODE.
00831              20  CF-CAN-POSTAL-1            PIC XXX.
00832              20  CF-CAN-POSTAL-2            PIC XXX.
00833              20  FILLER                     PIC XXX.
00834
00835          16  CF-IBNR-UEPRM-PERCENT          PIC S9V9(4) COMP-3.
00836          16  CF-IBNR-R78-PERCENT            PIC S9V9(4) COMP-3.
00837          16  CF-IBNR-PRO-PERCENT            PIC S9V9(4) COMP-3.
00838
00839          16  CF-RATING-SWITCH               PIC X.
00840              88  CF-PERFORM-RATING              VALUE ' ' 'Y'.
00841              88  CF-NO-RATING                   VALUE 'N'.
00842
00843          16  CF-BUILD-RETRIEVE-AFTER-MONTHS PIC 99.
00844
00845          16  CF-CARRIER-OVER-SHORT.
00846              20 CF-CR-OVR-SHT-AMT           PIC S999V99   COMP-3.
00847              20 CF-CR-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
00848
100703         16  CF-CARRIER-CLP-TOL-PCT         PIC S9V9(4)   COMP-3.
100703         16  CF-SECPAY-SWITCH               PIC X.
100703             88  CF-SECURE-PAY-CARRIER          VALUE 'Y'.
100703             88  CF-NO-SECURE-PAY               VALUE ' ' 'N'.
092705         16  CF-CARRIER-LEASE-COMM          PIC S9(5)V99  COMP-3.
032813         16  CF-CARRIER-NEXT-AUDIT-CHK-NO   PIC S9(8)     COMP.
032813         16  FILLER                         PIC X(444).
100703*        16  FILLER                         PIC X(452).
00850
00851
00852 ****************************************************************
00853 *             MORTALITY MASTER RECORD                          *
00854 ****************************************************************
00855
00856      12  CF-MORTALITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
00857          16  CF-MORT-TABLE-LINE OCCURS  9  TIMES
00858                                 INDEXED BY CF-MORT-NDX.
00859              20  CF-MORT-TABLE              PIC X(5).
00860              20  CF-MORT-TABLE-TYPE         PIC X.
00861                  88  CF-MORT-JOINT              VALUE 'J'.
00862                  88  CF-MORT-SINGLE             VALUE 'S'.
00863                  88  CF-MORT-COMBINED           VALUE 'C'.
00864                  88  CF-MORT-TYPE-VALID-C       VALUE 'J' 'S'.
00865                  88  CF-MORT-TYPE-VALID-M       VALUE 'J' 'S' 'C'.
00866              20  CF-MORT-INTEREST           PIC SV9(4)  COMP-3.
00867              20  CF-MORT-AGE-METHOD         PIC XX.
00868                  88  CF-AGE-LAST                VALUE 'AL'.
00869                  88  CF-AGE-NEAR                VALUE 'AN'.
00870              20  CF-MORT-RESERVE-ADJUSTMENT PIC S9V9(4) COMP-3.
00871              20  CF-MORT-ADJUSTMENT-DIRECTION
00872                                             PIC X.
00873                  88  CF-MINUS                   VALUE '-'.
00874                  88  CF-PLUS                    VALUE '+'.
00875              20  CF-MORT-JOINT-FACTOR       PIC S9V9(4) COMP-3.
00876              20  CF-MORT-JOINT-CODE         PIC X.
00877                  88  CF-VALID-JOINT-CODE        VALUE 'A' 'V'.
00878              20  CF-MORT-PC-Q               PIC X.
00879                  88  CF-VALID-PC-Q              VALUE 'Y' 'N' ' '.
00880              20  CF-MORT-TABLE-CODE         PIC X(4).
00881              20  CF-MORT-COMMENTS           PIC X(15).
00882              20  FILLER                     PIC X(14).
00883
00884          16  FILLER                         PIC X(251).
00885
00886
00887 ****************************************************************
00888 *             BUSSINESS TYPE MASTER RECORD                     *
00889 ****************************************************************
00890
00891      12  CF-BUSINESS-TYPE-MASTER-REC REDEFINES  CF-RECORD-BODY.
00892 * FIRST ENTRY IS TYPE 01.. LAST IS TYPE 20
00893 * RECORD 02 IS TYPES 21-40..RECORD 03 IS 41-60..04 IS 61-80
00894 * AND RECORD 05 IS TYPES 81-99
00895          16  CF-TYPE-DESCRIPTIONS   OCCURS  20  TIMES.
00896              20  CF-BUSINESS-TITLE          PIC  X(19).
00897              20  CF-BUS-MOD-ST-TRGT-LOSS-RATIO
00898                                             PIC S9V9(4) COMP-3.
00899              20  CF-BUS-EXCL-ST-CALL        PIC  X.
00900              20  FILLER                     PIC  X.
00901          16  FILLER                         PIC  X(248).
00902
00903
00904 ****************************************************************
00905 *             TERMINAL MASTER RECORD                           *
00906 ****************************************************************
00907
00908      12  CF-TERMINAL-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00909
00910          16  CF-COMPANY-TERMINALS.
00911              20  CF-TERMINAL-ID  OCCURS 120 TIMES
00912                                   PIC X(4).
00913          16  FILLER               PIC X(248).
00914
00915
00916 ****************************************************************
00917 *             LIFE EDIT MASTER RECORD                          *
00918 ****************************************************************
00919
00920      12  CF-LIFE-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
00921          16  CF-LIFE-EDIT-ENTRIES   OCCURS 120  TIMES.
00922              20  CF-LIFE-CODE-IN            PIC XX.
00923              20  CF-LIFE-CODE-OUT           PIC XX.
00924          16  FILLER                         PIC X(248).
00925
00926
00927 ****************************************************************
00928 *             AH EDIT MASTER RECORD                            *
00929 ****************************************************************
00930
00931      12  CF-AH-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
00932          16  CF-AH-EDIT-ENTRIES   OCCURS  96  TIMES.
00933              20  CF-AH-CODE-IN              PIC XXX.
00934              20  CF-AH-CODE-OUT             PIC XX.
00935          16  FILLER                         PIC X(248).
00936
00937
00938 ****************************************************************
00939 *             CREDIBILITY TABLES                               *
00940 ****************************************************************
00941
00942      12  CF-CREDIBILITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
00943          16  CF-CRDB-ENTRY   OCCURS 36 TIMES
00944                              INDEXED BY CF-CRDB-NDX.
00945              20  CF-CRDB-FROM               PIC S9(7)   COMP-3.
00946              20  CF-CRDB-TO                 PIC S9(7)   COMP-3.
00947              20  CF-CREDIBILITY-FACTOR      PIC S9V9(4) COMP-3.
00948          16  FILLER                         PIC  X(332).
00949
00950
00951 ****************************************************************
00952 *             REPORT CUSTOMIZATION RECORD                      *
00953 ****************************************************************
00954
00955      12  CF-CUSTOM-REPORT-REC  REDEFINES  CF-RECORD-BODY.
00956          16  CF-ACCOUNT-MASTER-STATUS       PIC X.
00957              88  CF-ACTIVE-ACCOUNTS             VALUE 'A'.
00958              88  CF-INACTIVE-ACCOUNTS           VALUE 'I'.
121307             88  CF-CANCELLED-ACCOUNTS          VALUE 'C'.
00959 **** NOTE: INACTIVE WILL INCLUDE ACCOUNT MASTER CODED WITH ****
00960 ****       A T-TRANSFER.                                   ****
00961              88  CF-ALL-ACCOUNTS                VALUE 'B'.
00962
00963          16  FILLER                         PIC XX.
00964
00965          16  CF-CARRIER-CNTL-OPT.
00966              20  CF-CARRIER-OPT-SEQ         PIC 9.
00967                  88  CF-CARRIER-OPT-USED        VALUE 1 THRU 6.
00968                  88  CF-CARRIER-OPT-NOT-USED    VALUE 0.
00969              20  CF-CARRIER-SELECT OCCURS 3 TIMES
00970                                             PIC X.
00971          16  CF-GROUP-CNTL-OPT.
00972              20  CF-GROUP-OPT-SEQ           PIC 9.
00973                  88  CF-GROUP-OPT-USED          VALUE 1 THRU 6.
00974                  88  CF-GROUP-OPT-NOT-USED      VALUE 0.
00975              20  CF-GROUP-SELECT OCCURS 3 TIMES
00976                                             PIC X(6).
00977          16  CF-STATE-CNTL-OPT.
00978              20  CF-STATE-OPT-SEQ           PIC 9.
00979                  88  CF-STATE-OPT-USED          VALUE 1 THRU 6.
00980                  88  CF-STATE-OPT-NOT-USED      VALUE 0.
00981              20  CF-STATE-SELECT OCCURS 3 TIMES
00982                                             PIC XX.
00983          16  CF-ACCOUNT-CNTL-OPT.
00984              20  CF-ACCOUNT-OPT-SEQ         PIC 9.
00985                  88  CF-ACCOUNT-OPT-USED        VALUE 1 THRU 6.
00986                  88  CF-ACCOUNT-OPT-NOT-USED    VALUE 0.
00987              20  CF-ACCOUNT-SELECT OCCURS 3 TIMES
00988                                             PIC X(10).
00989          16  CF-BUS-TYP-CNTL-OPT.
00990              20  CF-BUS-TYP-OPT-SEQ         PIC 9.
00991                  88  CF-BUS-TYP-OPT-USED        VALUE 1 THRU 6.
00992                  88  CF-BUS-TYP-OPT-NOT-USED    VALUE 0.
00993              20  CF-BUS-TYP-SELECT OCCURS 3 TIMES
00994                                             PIC XX.
00995          16  CF-LF-TYP-CNTL-OPT.
00996              20  CF-LF-TYP-OPT-SEQ          PIC 9.
00997                  88  CF-LF-TYP-OPT-USED         VALUE 1 THRU 6.
00998                  88  CF-LF-TYP-OPT-NOT-USED     VALUE 0.
00999              20  CF-BUS-LF-SELECT OCCURS 3 TIMES
01000                                             PIC XX.
01001          16  CF-AH-TYP-CNTL-OPT.
01002              20  CF-AH-TYP-OPT-SEQ          PIC 9.
01003                  88  CF-AH-TYP-OPT-USED         VALUE 1 THRU 6.
01004                  88  CF-AH-TYP-OPT-NOT-USED     VALUE 0.
01005              20  CF-BUS-AH-SELECT OCCURS 3 TIMES
01006                                             PIC XX.
01007          16  CF-REPTCD1-CNTL-OPT.
01008              20  CF-REPTCD1-OPT-SEQ         PIC 9.
01009                  88  CF-REPTCD1-OPT-USED        VALUE 1 THRU 6.
01010                  88  CF-REPTCD1-OPT-NOT-USED    VALUE 0.
01011              20  CF-REPTCD1-SELECT OCCURS 3 TIMES
01012                                             PIC X(10).
01013          16  CF-REPTCD2-CNTL-OPT.
01014              20  CF-REPTCD2-OPT-SEQ         PIC 9.
01015                  88  CF-REPTCD2-OPT-USED        VALUE 1 THRU 6.
01016                  88  CF-REPTCD2-OPT-NOT-USED    VALUE 0.
01017              20  CF-REPTCD2-SELECT OCCURS 3 TIMES
01018                                             PIC X(10).
01019          16  CF-USER1-CNTL-OPT.
01020              20  CF-USER1-OPT-SEQ           PIC 9.
01021                  88  CF-USER1-OPT-USED          VALUE 1 THRU 6.
01022                  88  CF-USER1-OPT-NOT-USED      VALUE 0.
01023              20  CF-USER1-SELECT OCCURS 3 TIMES
01024                                             PIC X(10).
01025          16  CF-USER2-CNTL-OPT.
01026              20  CF-USER2-OPT-SEQ           PIC 9.
01027                  88  CF-USER2-OPT-USED          VALUE 1 THRU 6.
01028                  88  CF-USER2-OPT-NOT-USED      VALUE 0.
01029              20  CF-USER2-SELECT OCCURS 3 TIMES
01030                                             PIC X(10).
01031          16  CF-USER3-CNTL-OPT.
01032              20  CF-USER3-OPT-SEQ           PIC 9.
01033                  88  CF-USER3-OPT-USED          VALUE 1 THRU 6.
01034                  88  CF-USER3-OPT-NOT-USED      VALUE 0.
01035              20  CF-USER3-SELECT OCCURS 3 TIMES
01036                                             PIC X(10).
01037          16  CF-USER4-CNTL-OPT.
01038              20  CF-USER4-OPT-SEQ           PIC 9.
01039                  88  CF-USER4-OPT-USED          VALUE 1 THRU 6.
01040                  88  CF-USER4-OPT-NOT-USED      VALUE 0.
01041              20  CF-USER4-SELECT OCCURS 3 TIMES
01042                                             PIC X(10).
01043          16  CF-USER5-CNTL-OPT.
01044              20  CF-USER5-OPT-SEQ           PIC 9.
01045                  88  CF-USER5-OPT-USED          VALUE 1 THRU 6.
01046                  88  CF-USER5-OPT-NOT-USED      VALUE 0.
01047              20  CF-USER5-SELECT OCCURS 3 TIMES
01048                                             PIC X(10).
01049          16  CF-REINS-CNTL-OPT.
01050              20  CF-REINS-OPT-SEQ           PIC 9.
01051                  88  CF-REINS-OPT-USED          VALUE 1 THRU 6.
01052                  88  CF-REINS-OPT-NOT-USED      VALUE 0.
01053              20  CF-REINS-SELECT OCCURS 3 TIMES.
01054                  24  CF-REINS-PRIME         PIC XXX.
01055                  24  CF-REINS-SUB           PIC XXX.
01056
01057          16  CF-AGENT-CNTL-OPT.
01058              20  CF-AGENT-OPT-SEQ           PIC 9.
01059                  88  CF-AGENT-OPT-USED          VALUE 1 THRU 6.
01060                  88  CF-AGENT-OPT-NOT-USED      VALUE 0.
01061              20  CF-AGENT-SELECT OCCURS 3 TIMES
01062                                             PIC X(10).
01063
01064          16  FILLER                         PIC X(43).
01065
01066          16  CF-LOSS-RATIO-SELECT.
01067              20  CF-SEL-LO-LOSS-RATIO       PIC S999V99  COMP-3.
01068              20  CF-SEL-HI-LOSS-RATIO       PIC S999V99  COMP-3.
01069          16  CF-ENTRY-DATE-SELECT.
01070              20  CF-SEL-LO-ENTRY-DATE       PIC XX.
01071              20  CF-SEL-HI-ENTRY-DATE       PIC XX.
01072          16  CF-EFFECTIVE-DATE-SELECT.
01073              20  CF-SEL-LO-EFFECTIVE-DATE   PIC XX.
01074              20  CF-SEL-HI-EFFECTIVE-DATE   PIC XX.
01075
01076          16  CF-EXCEPTION-LIST-IND          PIC X.
01077              88  CF-EXCEPTION-LIST-REQUESTED VALUE 'Y'.
01078
01079          16  FILLER                         PIC X(318).
01080
01081 ****************************************************************
01082 *                  EXCEPTION REPORTING RECORD                  *
01083 ****************************************************************
01084
01085      12  CF-EXCEPTION-REPORT-REC REDEFINES   CF-RECORD-BODY.
01086          16  CF-ACCOUNTS-LT-ONE-YEAR        PIC X.
01087              88  CF-EXCEPTION-ACCTS-WITHIN-ONE  VALUE 'Y'.
01088
01089          16  CF-COMBINED-LIFE-AH-OPT.
01090              20  CF-ISS-COUNT-DIFF          PIC S9(05)     COMP-3.
01091              20  CF-SINGLE-MO-PREM-PCT      PIC S9(02).
01092              20  CF-EARN-PREM-DECR-PCT      PIC S9(02).
01093              20  CF-CANCELLATION-RATIO      PIC S9(02).
01094
01095          16  CF-LIFE-OPT.
01096              20  CF-LF-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
01097              20  CF-LF-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
01098              20  CF-LF-PERIOD-PROFIT        PIC S9(03)     COMP-3.
01099              20  CF-LF-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
01100              20  CF-LF-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
01101              20  CF-LF-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
01102              20  CF-LF-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
01103              20  CF-LF-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
01104              20  CF-LF-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
01105              20  CF-LF-AVG-AGE-MAX          PIC S9(02).
01106
01107          16  CF-AH-OPT.
01108              20  CF-AH-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
01109              20  CF-AH-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
01110              20  CF-AH-PERIOD-PROFIT        PIC S9(03)     COMP-3.
01111              20  CF-AH-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
01112              20  CF-AH-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
01113              20  CF-AH-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
01114              20  CF-AH-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
01115              20  CF-AH-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
01116              20  CF-AH-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
01117              20  CF-AH-AVG-AGE-MAX          PIC S9(02).
01118
01119          16  CF-ACCT-ZERO-MONTH-PRODUCTION PIC X.
01120              88  CF-ACCT-CURRENT-MONTH-ACT      VALUE 'A'.
01121              88  CF-ACCT-WITH-NO-PRODUCTION     VALUE 'B'.
01122              88  CF-ACCT-WITH-ISSUE-ACTIVITY    VALUE 'C'.
01123
01124          16  CF-RETENTION-LIMIT             PIC S9(7)      COMP-3.
01125
01126          16  FILLER                         PIC X(673).
01127
01128
01129 ****************************************************************
01130 *             MORTGAGE SYSTEM PLAN RECORD                      *
01131 ****************************************************************
01132
01133      12  CF-MORTGAGE-PLAN-MASTER  REDEFINES  CF-RECORD-BODY.
01134          16  CF-PLAN-TYPE                   PIC X.
01135              88  CF-LIFE-MORT-PLAN             VALUE 'L'.
01136              88  CF-DISAB-MORT-PLAN            VALUE 'D'.
01137              88  CF-AD-D-MORT-PLAN             VALUE 'A'.
01138          16  CF-PLAN-ABBREV                 PIC XXX.
01139          16  CF-PLAN-DESCRIPT               PIC X(10).
01140          16  CF-PLAN-NOTES                  PIC X(20).
01141          16  CF-PLAN-ESTABLISH-DATE         PIC XX.
01142          16  CF-PLAN-UNDERWRITING.
01143              20  CF-PLAN-TERM-DATA.
01144                  24  CF-MINIMUM-TERM        PIC S999      COMP-3.
01145                  24  CF-MAXIMUM-TERM        PIC S999      COMP-3.
01146              20  CF-PLAN-AGE-DATA.
01147                  24  CF-MINIMUM-AGE         PIC S999      COMP-3.
01148                  24  CF-MAXIMUM-AGE         PIC S999      COMP-3.
01149                  24  CF-MAXIMUM-ATTAIN-AGE  PIC S999      COMP-3.
01150              20  CF-PLAN-BENEFIT-DATA.
01151                  24  CF-MINIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
01152                  24  CF-MAXIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
01153                  24  CF-MAXIMUM-MONTHLY-BENEFIT
01154                                             PIC S9(7)V99  COMP-3.
01155          16  CF-PLAN-POLICY-FORMS.
01156              20  CF-POLICY-FORM             PIC X(12).
01157              20  CF-MASTER-APPLICATION      PIC X(12).
01158              20  CF-MASTER-POLICY           PIC X(12).
01159          16  CF-PLAN-RATING.
01160              20  CF-RATE-CODE               PIC X(5).
01161              20  CF-SEX-RATING              PIC X.
01162                  88  CF-PLAN-NOT-SEX-RATED     VALUE '1'.
01163                  88  CF-PLAN-SEX-RATED         VALUE '2'.
01164              20  CF-SUB-STD-PCT             PIC S9V9999   COMP-3.
01165              20  CF-SUB-STD-TYPE            PIC X.
01166                  88  CF-PCT-OF-PREM            VALUE '1'.
01167                  88  CF-PCT-OF-BENE            VALUE '2'.
01168          16  CF-PLAN-PREM-TOLERANCES.
01169              20  CF-PREM-TOLERANCE          PIC S999      COMP-3.
01170              20  CF-PREM-TOLERANCE-PCT      PIC SV999     COMP-3.
01171          16  CF-PLAN-PYMT-TOLERANCES.
01172              20  CF-PYMT-TOLERANCE          PIC S999      COMP-3.
01173              20  CF-PYMT-TOLERANCE-PCT      PIC SV999     COMP-3.
01174          16  CF-PLAN-MISC-DATA.
01175              20  FILLER                     PIC X.
01176              20  CF-FREE-EXAM-DAYS          PIC S999      COMP-3.
01177              20  CF-RETRO-RETENTION         PIC S9V9999   COMP-3.
01178          16  CF-MORT-PLAN-USE-CTR           PIC S999      COMP-3.
01179          16  CF-PLAN-IND-GRP                PIC X.
01180              88  CF-MORT-INDIV-PLAN            VALUE 'I'
01181                                                      '1'.
01182              88  CF-MORT-GROUP-PLAN            VALUE 'G'
01183                                                      '2'.
01184          16  CF-MIB-SEARCH-SW               PIC X.
01185              88  CF-MIB-SEARCH-ALL             VALUE '1'.
01186              88  CF-MIB-SEARCH-NONE            VALUE '2'.
01187              88  CF-MIB-SEARCH-EXCEEDED        VALUE '3'.
01188              88  CF-MIB-SEARCH-VALID      VALUES ARE '1' '2' '3'.
01189          16  CF-ALPHA-SEARCH-SW             PIC X.
01190              88  CF-MIB-ALPHA-ALL              VALUE '1'.
01191              88  CF-MIB-ALPHA-NONE             VALUE '2'.
01192              88  CF-MIB-APLHA-EXCEEDED         VALUE '3'.
01193              88  CF-CLIENT-ALPHA-ALL           VALUE 'A'.
01194              88  CF-CLIENT-ALPHA-NONE          VALUE 'B'.
01195              88  CF-CLIENT-APLHA-EXCEEDED      VALUE 'C'.
01196              88  CF-BOTH-ALPHA-ALL             VALUE 'X'.
01197              88  CF-BOTH-ALPHA-NONE            VALUE 'Y'.
01198              88  CF-BOTH-APLHA-EXCEEDED        VALUE 'Z'.
01199              88  CF-ALPHA-SEARCH-VALID    VALUES ARE '1' '2' '3'
01200                                                      'A' 'B' 'C'
01201                                                      'X' 'Y' 'Z'.
01202          16  CF-EFF-DT-RULE-SW              PIC X.
01203              88  CF-EFF-DT-ENTER               VALUE 'E'.
01204              88  CF-EFF-DT-MONTH               VALUE 'M'.
01205              88  CF-EFF-DT-QTR                 VALUE 'Q'.
01206              88  CF-EFF-DT-SEMI                VALUE 'S'.
01207              88  CF-EFF-DT-ANN                 VALUE 'A'.
01208          16  FILLER                         PIC X(4).
01209          16  CF-HEALTH-QUESTIONS            PIC X.
01210              88  CF-VALID-QUESTIONS-CNT VALUES ARE '0' THRU '9'.
01211          16  CF-GRACE-PERIOD                PIC S999      COMP-3.
01212          16  CF-NUMBER-LAPSE-NOTICES        PIC S999      COMP-3.
01213          16  CF-PLAN-SNGL-JNT               PIC X.
01214              88  CF-COMBINED-PLAN              VALUE 'C'.
01215              88  CF-JNT-PLAN                   VALUE 'J'.
01216              88  CF-SNGL-PLAN                  VALUE 'S'.
01217          16  CF-DAYS-TO-1ST-NOTICE          PIC  99.
01218          16  CF-DAYS-TO-2ND-NOTICE          PIC  99.
01219          16  CF-DAYS-TO-3RD-NOTICE          PIC  99.
01220          16  CF-DAYS-TO-4TH-NOTICE          PIC  99.
01221          16  CF-RERATE-CNTL                 PIC  X.
01222              88  CF-RERATE-WITH-ISSUE-AGE       VALUE '1'.
01223              88  CF-RERATE-WITH-CURRENT-AGE     VALUE '2'.
01224              88  CF-DO-NOT-RERATE               VALUE '3' ' '.
01225              88  CF-AUTO-RECALC                 VALUE '4'.
01226          16  CF-BENEFIT-TYPE                PIC  X.
01227              88  CF-BENEFIT-IS-LEVEL            VALUE '1'.
01228              88  CF-BENEFIT-REDUCES             VALUE '2'.
01229          16  CF-POLICY-FEE                  PIC S999V99
01230                                                     COMP-3.
01231          16  CF-1ST-NOTICE-FORM             PIC  X(04).
01232          16  CF-2ND-NOTICE-FORM             PIC  X(04).
01233          16  CF-3RD-NOTICE-FORM             PIC  X(04).
01234          16  CF-4TH-NOTICE-FORM             PIC  X(04).
01235          16  FILLER                         PIC  X(32).
01236          16  CF-TERMINATION-FORM            PIC  X(04).
01237          16  FILLER                         PIC  X(08).
01238          16  CF-CLAIM-CAP                   PIC S9(7)V99
01239                                                        COMP-3.
01240          16  CF-REOCCURRING-DISABILITY-PRD  PIC S999   COMP-3.
01241          16  CF-ISSUE-LETTER                PIC  X(4).
01242          16  CF-YEARS-TO-NEXT-RERATE        PIC  99.
01243          16  CF-DEPENDENT-COVERAGE          PIC  X.
01244              88  CF-YES-DEP-COV                 VALUE 'Y'.
01245              88  CF-NO-DEP-COV             VALUES ARE 'N' ' '.
01246          16  CF-MP-REFUND-CALC              PIC X.
01247              88  CF-MP-REFUND-NOT-USED          VALUE SPACE.
01248              88  CF-MP-REFD-BY-R78              VALUE '1'.
01249              88  CF-MP-REFD-BY-PRO-RATA         VALUE '2'.
01250              88  CF-MP-REFD-AS-CALIF            VALUE '3'.
01251              88  CF-MP-REFD-AS-TEXAS            VALUE '4'.
01252              88  CF-MP-REFD-IS-NET-PAY          VALUE '5'.
01253              88  CF-MP-REFD-ANTICIPATION        VALUE '6'.
01254              88  CF-MP-REFD-MEAN                VALUE '8'.
01255          16  CF-ALT-RATE-CODE               PIC  X(5).
01256
01257
01258          16  FILLER                         PIC X(498).
01259 ****************************************************************
01260 *             MORTGAGE COMPANY MASTER RECORD                   *
01261 ****************************************************************
01262
01263      12  CF-MORTG-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
01264          16  CF-MORTG-ALT-MORT-CODE         PIC X(4).
01265          16  CF-MORTG-ACCESS-CONTROL        PIC X.
01266              88  CF-MORT-ST-PROD-CNTL                VALUE ' '.
01267              88  CF-MORT-CARR-GRP-ST-PROD-CNTL       VALUE '1'.
01268              88  CF-MORT-CARR-ST-PROD-CNTL           VALUE '2'.
01269              88  CF-MORT-PROD-CNTL                   VALUE '3'.
01270              88  CF-MORT-CARR-PROD-CNTL              VALUE '4'.
01271
01272          16  CF-MORTG-CONVERSION-DATE       PIC XX.
01273          16  CF-MORTG-RATE-FILE-MAINT-DATE  PIC XX.
01274          16  CF-MORTG-RATE-FILE-CREAT-DATE  PIC XX.
01275          16  CF-MORTG-PROD-FILE-MAINT-DATE  PIC XX.
01276          16  CF-MORTG-PROD-FILE-CREAT-DATE  PIC XX.
01277
01278          16  CF-MP-POLICY-LINKAGE-IND       PIC X(1).
01279              88  CF-MP-PLCY-LINKAGE-USED     VALUE 'Y'.
01280          16  CF-MP-RECON-USE-IND            PIC X(1).
01281              88  CF-MP-USE-RECON             VALUE 'Y'.
01282          16  CF-MORTG-CHECK-NO-COUNTER      PIC 9(6).
01283              88  CF-MP-CHECK-CNT-RESET-VALUE VALUE 999999.
01284          16  CF-MP-REPORT-LANGUAGE-IND      PIC X(1).
01285              88  CF-MP-LANGUAGE-IS-ENG       VALUE 'E'.
01286              88  CF-MP-LANGUAGE-IS-FR        VALUE 'F'.
01287          16  FILLER                         PIC X(1).
01288          16  CF-MORTG-CHECK-QUEUE-COUNTER   PIC 9(6).
01289              88  CF-MP-CHKQ-CNT-RESET-VALUE  VALUE 999999.
01290          16  CF-MORTG-MIB-VERSION           PIC X.
01291              88  CF-MORTG-MIB-BATCH         VALUE '1'.
01292              88  CF-MORTG-MIB-ONLINE        VALUE '2'.
01293              88  CF-MORTG-MIB-BOTH          VALUE '3'.
01294          16  CF-MORTG-ALT-MIB-SEARCH-CNTL.
01295              20  CF-MORTG-MIB-LNAME-SEARCH  PIC X.
01296                  88  CF-MIB-LAST-NAME-SEARCH     VALUE 'Y'.
01297              20  CF-MORTG-MIB-FNAME-SEARCH  PIC X.
01298                  88  CF-MIB-FIRST-NAME-SEARCH    VALUE 'Y'.
01299              20  CF-MORTG-MIB-MNAME-SEARCH  PIC X.
01300                  88  CF-MIB-MIDDLE-NAME-SEARCH   VALUE 'Y'.
01301              20  CF-MORTG-MIB-BDATE-SEARCH  PIC X.
01302                  88  CF-MIB-BIRTH-DATE-SEARCH    VALUE 'Y'.
01303              20  CF-MORTG-MIB-BSTATE-SEARCH PIC X.
01304                  88  CF-MIB-BIRTH-STATE-SEARCH   VALUE 'Y'.
01305              20  CF-MORTG-MIB-RSTATE-SEARCH PIC X.
01306                  88  CF-MIB-RESIDNT-STATE-SEARCH VALUE 'Y'.
01307          16  CF-MORTG-MIB-COMPANY-SYMBOL    PIC XXX.
01308          16  FILLER                         PIC X(7).
01309          16  CF-MORTG-DESTINATION-SYMBOL.
01310              20  CF-MORTG-MIB-COMM          PIC X(5).
01311              20  CF-MORTG-MIB-TERM          PIC X(5).
01312          16  CF-ASSIGN-POLICY-NO-SW         PIC X(01).
01313              88  CF-ASSIGN-POLICY-NO             VALUE 'Y'.
01314          16  FILLER                         PIC X(03).
01315          16  CF-MP-CHECK-NO-CONTROL.
01316              20  CF-MP-CHECK-NO-METHOD      PIC X(01).
01317                  88  CF-MP-CHECK-NO-MANUAL     VALUE '1'.
01318                  88  CF-MP-CHECK-NO-AUTO-SEQ   VALUE '2'
01319                                                 ' ' LOW-VALUES.
01320                  88  CF-MP-CHECK-NO-PRE-PRINTED
01321                                                VALUE '3'.
01322          16  CF-MORTG-LOAN-SHIFT-IND        PIC X(01).
01323          16  CF-MORTG-SOLICITATION-NUM      PIC S9(17) COMP-3.
01324          16  CF-MORTG-ALT-ALPHA-SEARCH-CNTL.
01325              20  CF-MORTG-ALP-LNAME-SEARCH  PIC X.
01326                  88  CF-ALPHA-LAST-NAME-SEARCH      VALUE 'Y'.
01327              20  CF-MORTG-ALP-FNAME-SEARCH  PIC X.
01328                  88  CF-ALPHA-FIRST-NAME-SEARCH     VALUE 'Y'.
01329              20  CF-MORTG-ALP-MNAME-SEARCH  PIC X.
01330                  88  CF-ALPHA-MIDDLE-NAME-SEARCH    VALUE 'Y'.
01331              20  CF-MORTG-ALP-BDATE-SEARCH  PIC X.
01332                  88  CF-ALPHA-BIRTH-DATE-SEARCH     VALUE 'Y'.
01333              20  CF-MORTG-ALP-BSTATE-SEARCH PIC X.
01334                  88  CF-ALPHA-BIRTH-STATE-SEARCH    VALUE 'Y'.
01335              20  CF-MORTG-ALP-RSTATE-SEARCH PIC X.
01336                  88  CF-ALPHA-RESIDNT-STATE-SEARCH  VALUE 'Y'.
01337          16  CF-MORTG-BILLING-AREA.
01338              20  CF-MORTG-BILL-CYCLE   OCCURS  5  TIMES
01339                                             PIC X.
01340          16  CF-MORTG-MONTH-END-DT          PIC XX.
01341          16  CF-MORTG-CURRENT-ARCH-NUM      PIC S9(8)  COMP.
01342          16  CF-MORTG-START-ARCH-NUM        PIC S9(8)  COMP.
01343          16  CF-MORTG-MIB-DEST-SW           PIC X.
01344              88 CF-MORTG-MIB-COMM-DEST              VALUE '1'.
01345              88 CF-MORTG-MIB-TERM-DEST              VALUE '2'.
01346          16  FILLER                         PIC X.
01347          16  CF-MORTG-LABEL-CONTROL         PIC X.
01348              88 CF-MORTG-CREATE-LABELS              VALUE 'Y'.
01349              88 CF-MORTG-BYPASS-LABELS              VALUE 'N'.
01350          16  CF-ACH-ORIGINATING-DFI-ID      PIC X(8).
01351          16  FILLER                         PIC X(8).
01352          16  CF-ACH-SENDING-DFI-NAME        PIC X(23).
01353          16  CF-ACH-RECVING-DFI-ROUTING-NO  PIC X(8).
01354          16  CF-ACH-RECVING-DFI-NAME        PIC X(23).
01355          16  CF-ACH-COMPANY-ID.
01356              20  CF-ACH-ID-CODE-DESIGNATOR  PIC X.
01357                  88  CF-ACH-ICD-IRS-EIN             VALUE '1'.
01358                  88  CF-ACH-ICD-DUNS                VALUE '3'.
01359                  88  CF-ACH-ICD-USER-ASSIGNED-NO    VALUE '9'.
01360              20  CF-ACH-COMPANY-ID-NO       PIC X(9).
01361          16  CF-MORTG-BILL-GROUPING-CODE    PIC X.
01362              88  CF-MORTG-CO-HAS-GROUPING           VALUE 'Y'.
01363          16  CF-RATE-DEV-AUTHORIZATION      PIC X.
01364              88  CF-RATE-DEV-AUTHORIZED             VALUE 'Y'.
01365              88  CF-RATE-DEV-NOT-AUTHORIZED         VALUE 'N'.
01366          16  CF-ACH-SENDING-DFI-ROUTING-NO  PIC X(9).
01367          16  CF-CBA-FILE-CREATE-NUM         PIC 9(4).
01368          16  FILLER                         PIC X(536).
01369
01370 ****************************************************************
01371 *             MORTGAGE HEIGHT - WEIGHT CHARTS                  *
01372 ****************************************************************
01373
01374      12  CF-FEMALE-HT-WT-REC  REDEFINES CF-RECORD-BODY.
01375          16  CF-FEMALE-HT-WT-INFO OCCURS 30 TIMES.
01376              20  CF-FEMALE-HEIGHT.
01377                  24  CF-FEMALE-FT           PIC 99.
01378                  24  CF-FEMALE-IN           PIC 99.
01379              20  CF-FEMALE-MIN-WT           PIC 999.
01380              20  CF-FEMALE-MAX-WT           PIC 999.
01381          16  FILLER                         PIC X(428).
01382
01383      12  CF-MALE-HT-WT-REC    REDEFINES CF-RECORD-BODY.
01384          16  CF-MALE-HT-WT-INFO   OCCURS 30 TIMES.
01385              20  CF-MALE-HEIGHT.
01386                  24  CF-MALE-FT             PIC 99.
01387                  24  CF-MALE-IN             PIC 99.
01388              20  CF-MALE-MIN-WT             PIC 999.
01389              20  CF-MALE-MAX-WT             PIC 999.
01390          16  FILLER                         PIC X(428).
01391 ******************************************************************
01392 *             AUTOMATIC ACTIVITY RECORD                          *
01393 ******************************************************************
01394      12  CF-AUTO-ACTIVITY-REC REDEFINES CF-RECORD-BODY.
01395          16  CF-SYSTEM-DEFINED-ACTIVITY OCCURS 09 TIMES.
01396              20  CF-SYS-ACTIVE-SW           PIC X(01).
01397              20  CF-SYS-LETTER-ID           PIC X(04).
01398              20  CF-SYS-RESEND-DAYS         PIC 9(03).
01399              20  CF-SYS-FOLLOW-UP-DAYS      PIC 9(03).
01400              20  CF-SYS-RESET-SW            PIC X(01).
01401              20  CF-SYS-REPORT-DAYS         PIC 9(03).
01402              20  CF-SYS-EACH-DAY-AFTER-SW   PIC X(01).
01403
01404          16  FILLER                         PIC X(50).
01405
01406          16  CF-USER-DEFINED-ACTIVITY  OCCURS 08 TIMES.
01407              20  CF-USER-ACTIVE-SW          PIC X(01).
01408              20  CF-USER-LETTER-ID          PIC X(04).
01409              20  CF-USER-RESEND-DAYS        PIC 9(03).
01410              20  CF-USER-FOLLOW-UP-DAYS     PIC 9(03).
01411              20  CF-USER-RESET-SW           PIC X(01).
01412              20  CF-USER-REPORT-DAYS        PIC 9(03).
01413              20  CF-USER-EACH-DAY-AFTER-SW  PIC X(01).
01414              20  CF-USER-ACTIVITY-DESC      PIC X(20).
01415
01416          16  FILLER                         PIC X(246).
00293
00294      EJECT
00295 *                                    COPY ERCCRTC.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCRTC                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.005                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = PENDING CHANGES TO CERTIFICATE RECORD     *
00008 *                      (MAINTENANCE ONLY - SEE ERCPNDB FOR NEW   *
00009 *                       ISSUES AND CANCELS)                      *
00010 *   FILE TYPE = VSAM,KSDS                                        *
00011 *   RECORD SIZE = 300  RECFORM = FIXED                           *
00012 *                                                                *
00013 *   BASE CLUSTER = ERCRTC                         RKP=2,LEN=37   *
00014 *       ALTERNATE PATHS = NONE                                   *
00015 *                                                                *
00016 *   LOG = YES                                                    *
00017 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00018 ******************************************************************
110105*                   C H A N G E   L O G
110105*
110105* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
110105*-----------------------------------------------------------------
110105*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
110105* EFFECTIVE    NUMBER
110105*-----------------------------------------------------------------
110105* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
110105******************************************************************
00019  01  PENDING-MAINT-TO-CERT-FILE.
00020      12  CC-RECORD-ID                     PIC XX.
00021          88  VALID-CC-ID                        VALUE 'CC'.
00022
00023      12  CC-CONTROL-PRIMARY.
00024          16  CC-COMPANY-CD                PIC X.
00025          16  CC-CARRIER                   PIC X.
00026          16  CC-GROUPING                  PIC X(6).
00027          16  CC-STATE                     PIC XX.
00028          16  CC-ACCOUNT                   PIC X(10).
00029          16  CC-CERT-EFF-DT               PIC XX.
00030          16  CC-CERT-NO.
00031              20  CC-CERT-PRIME            PIC X(10).
00032              20  CC-CERT-SFX              PIC X.
00033
00034          16  CC-FILE-SEQ-NO               PIC S9(8)     COMP.
00035
00036      12  CC-LAST-MAINT-DT                 PIC XX.
00037      12  CC-LAST-MAINT-BY                 PIC X(4).
00038      12  CC-LAST-MAINT-HHMMSS             PIC S9(6)     COMP-3.
00039
00040      12  CC-RECORD-BODY                   PIC X(191).
00041
00042      12  CC-CHANGES-RECORD   REDEFINES CC-RECORD-BODY.
00043          16  FILLER                       PIC X(10).
00044          16  CC-NAME.
00045              20  CC-INSURED-LAST-NAME     PIC X(15).
00046              20  CC-INSURED-FIRST-NAME.
00047                  24  CC-INSURED-1ST-INIT  PIC X.
00048                  24  FILLER               PIC X(9).
00049              20  CC-INSURED-INITIAL2      PIC XX.
00050          16  CC-AGE                       PIC S99.
00051          16  CC-INSURED-JOINT-AGE         PIC S99.
00052          16  CC-BIRTHDAY                  PIC XX.
00053          16  CC-INSURED-SEX               PIC X.
00054              88  CC-SEX-MALE                 VALUE 'M'.
00055              88  CC-SEX-FEMALE               VALUE 'F'.
00056          16  CC-JOINT-INSURED-NAME.
00057              20  CC-JT-LAST-NAME          PIC X(15).
00058              20  CC-JT-FIRST-NAME.
00059                  24  CC-JT-1ST-INIT       PIC X.
00060                  24  FILLER               PIC X(9).
00061              20  CC-JT-INITIAL            PIC X.
00062          16  CC-LOAN-APR                  PIC 9(3)V9(4).
00063          16  CC-PAY-FREQUENCY             PIC S99.
00064          16  CC-BENEFICIARY               PIC X(25).
00065          16  CC-POLICY-FORM-NO            PIC X(12).
00066          16  CC-PREMIUM-TYPE              PIC X.
00067          16  CC-IND-GRP-TYPE              PIC X.
00068          16  CC-LOAN-NUMBER               PIC X(8).
00069          16  CC-LOAN-BALANCE              PIC S9(7)V99  COMP-3.
110105         16  CC-OLD-LOF                   PIC XXX.
110105*        16  CC-LOAN-OFFICER              PIC XXX.
00071          16  CC-USER-FIELD                PIC X.
00072          16  CC-SOC-SEC-NO                PIC X(11).
00073          16  CC-MEMBER-NO                 PIC X(12).
00074          16  CC-LF-ORIG-TERM              PIC S9(3).
00075          16  CC-AH-ORIG-TERM              PIC S9(3).
00076          16  FILLER                       PIC X(3).
00077          16  CC-CLAIM-DEDUCT-WITHHELD     PIC S9(5)V99  COMP-3.
00078          16  CC-CANCEL-DEDUCT-WITHHELD    PIC S9(5)V99  COMP-3.
00079          16  CC-LIVES                     PIC S9(7)     COMP-3.
00080          16  CC-BILLED                    PIC S9(7)     COMP-3.
110105         16  CC-LOAN-OFFICER              PIC X(5).
110105         16  FILLER                       PIC XXX.
00082      12  CC-RECORD-STATUS.
00083          16  CC-CREDIT-SELECT-DT          PIC XX.
00084          16  CC-CREDIT-ACCEPT-DT          PIC XX.
00085      12  CC-ADDITIONAL-LF-DATA.
00086          16  CC-LF-BENEFIT-CD             PIC XX.
00087          16  CC-LF-PREMIUM-AMT            PIC S9(7)V99  COMP-3.
00088          16  CC-LF-BENEFIT-AMT            PIC S9(9)V99  COMP-3.
00089          16  CC-LF-EXPIRY-DT              PIC XX.
00090      12  CC-INPUT-DT                      PIC XX.
00091      12  CC-ERROR-FLAGS.
00092          16  CC-STANDARD-ERRORS.
00093              20  FILLER                   PIC X    OCCURS 10.
00094          16  CC-TRANSACTION-ERRORS.
00095              20  FILLER                   PIC X    OCCURS 10.
00096      12  CC-ADDITIONAL-AH-DATA.
00097          16  CC-AH-BENEFIT-CD             PIC XX.
00098          16  CC-AH-PREMIUM-AMT            PIC S9(7)V99  COMP-3.
00099          16  CC-AH-BENEFIT-AMT            PIC S9(7)V99  COMP-3.
00100          16  CC-AH-EXPIRY-DT              PIC XX.
00101
00102      12  FILLER                           PIC X(05).
00103 ******************************************************************
00296
00297      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA VAR
                                CERTIFICATE-MASTER MAILING-DATA
                                CLAIM-MASTER CERTIFICATE-TRAILERS
                                ORIGINAL-CERTIFICATE PENDING-BUSINESS
                                CONTROL-FILE
                                PENDING-MAINT-TO-CERT-FILE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL1273' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
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
00319      
      * EXEC CICS HANDLE CONDITION
00320 *        DUPKEY (2100-WRITE-DUPKEY)
00321 *        ERROR  (9990-ERROR)
00322 *        QIDERR (0100-DISPLAY-CERTIFICATE)
00323 *    END-EXEC.
      *    MOVE '"$$.N                 ! " #00006727' TO DFHEIV0
           MOVE X'2224242E4E20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303036373237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00324
00325      MOVE EIBTRMID               TO  W-QID-TERM.
00326
00327      IF PI-PROCESSOR-ID = 'LGXX'
00328          NEXT SENTENCE
00329      ELSE
00330          
      * EXEC CICS READQ TS
00331 *            QUEUE   (PI-SECURITY-TEMP-STORE-ID)
00332 *            INTO    (SECURITY-CONTROL)
00333 *            LENGTH  (SC-COMM-LENGTH)
00334 *            ITEM    (SC-ITEM)
00335 *        END-EXEC
      *    MOVE '*$II   L              ''   #00006738' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
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
00390      
      * EXEC CICS HANDLE CONDITION
00391 *        NOTFND (8880-NOT-FOUND)
00392 *    END-EXEC.
      *    MOVE '"$I                   ! # #00006816' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303036383136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00393
00394      
      * EXEC CICS READ
00395 *        DATASET (WS-CERTIFICATE-MASTER-DSID)
00396 *        RIDFLD  (WS-CERTIFICATE-KEY)
00397 *        SET     (ADDRESS OF CERTIFICATE-MASTER)
00398 *    END-EXEC.
      *    MOVE '&"S        E          (   #00006820' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CERTIFICATE-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CERTIFICATE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
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
071015     
      * exec cics read
071015*         dataset   ('ERACCT')
071015*         into      (account-master)
071015*         ridfld    (eracct-key)
071015*         gteq
071015*         resp      (ws-response)
071015*    end-exec
           MOVE LENGTH OF
            account-master
             TO DFHEIV11
           MOVE 'ERACCT' TO DFHEIV1
      *    MOVE '&"IL       G          (  N#00007007' TO DFHEIV0
           MOVE X'2622494C2020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303037303037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 account-master, 
                 DFHEIV11, 
                 eracct-key, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
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
052918     
      * exec cics read
052918*         dataset   ('ERACNT')
052918*         into      (note-file)
052918*         ridfld    (ws-eracnt-key)
052918*         resp      (ws-response)
052918*    end-exec
           MOVE LENGTH OF
            note-file
             TO DFHEIV11
           MOVE 'ERACNT' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00007026' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303037303236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 note-file, 
                 DFHEIV11, 
                 ws-eracnt-key, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
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
00838      
      * EXEC CICS RECEIVE
00839 *        MAPSET (WS-MAPSET-NAME)
00840 *        MAP    (WS-MAP-NAME)
00841 *        INTO   (EL127CI)
00842 *    END-EXEC.
           MOVE LENGTH OF
            EL127CI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00007411' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL127CI, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
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
092412         
      * EXEC CICS READ
092412*             DATASET   ('ERACCT')
092412*             INTO      (ACCOUNT-MASTER)
092412*             RIDFLD    (ERACCT-KEY)
092412*             GTEQ
092412*             RESP      (WS-RESPONSE)
092412*        END-EXEC
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV11
           MOVE 'ERACCT' TO DFHEIV1
      *    MOVE '&"IL       G          (  N#00007462' TO DFHEIV0
           MOVE X'2622494C2020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303037343632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACCOUNT-MASTER, 
                 DFHEIV11, 
                 ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
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
092412        
      * EXEC CICS READ
092412*          DATASET   ('ERMAIL')
092412*          SET       (ADDRESS OF MAILING-DATA)
092412*          RIDFLD    (WS-ERMAIL-KEY)
092412*          RESP      (WS-RESPONSE)
092412*       END-EXEC
           MOVE 'ERMAIL' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00007491' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303037343931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ERMAIL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF MAILING-DATA TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
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
010412        
      * EXEC CICS XCTL
010412*           PROGRAM    ('EL6311')
010412*           COMMAREA   (WS-PASS-631)
010412*           LENGTH     (1300)
010412*       END-EXEC
           MOVE 'EL6311' TO DFHEIV1
           MOVE 1300
             TO DFHEIV11
      *    MOVE '.$C                   %   #00007708' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303037373038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-PASS-631, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
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
01052          
      * EXEC CICS BIF
01053 *            DEEDIT
01054 *            FIELD  (LOANBALI)
01055 *            LENGTH (11)
01056 *        END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00007867' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LOANBALI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
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
01070          
      * EXEC CICS BIF
01071 *            DEEDIT
CIDMOD*            FIELD  (CAPRI)
CIDMOD*            LENGTH (8)
01074 *        END-EXEC
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00007885' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CAPRI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
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
01216          
      * EXEC CICS BIF
01217 *            DEEDIT
01218 *            FIELD  (CLPREMI)
01219 *            LENGTH (11)
01220 *        END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00008044' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303038303434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLPREMI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01221          MOVE +1                 TO  WS-UPDATE-SW
01222          MOVE CLPREMI            TO  WS-LF-PREM.
01223
01224      IF CLBENL > ZERO
01225          
      * EXEC CICS BIF
01226 *            DEEDIT
01227 *            FIELD  (CLBENI)
01228 *            LENGTH (14)
01229 *        END-EXEC
           MOVE 14
             TO DFHEIV11
      *    MOVE '@"L                   #   #00008053' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303038303533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLBENI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
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
01284          
      * EXEC CICS BIF
01285 *            DEEDIT
01286 *            FIELD  (CAPREMI)
01287 *            LENGTH (11)
01288 *        END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00008112' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303038313132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CAPREMI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01289          MOVE +1                 TO  WS-UPDATE-SW
01290          MOVE CAPREMI            TO  WS-AH-PREM.
01291
01292      IF CABENL > ZERO
01293          
      * EXEC CICS BIF
01294 *            DEEDIT
01295 *            FIELD  (CABENI)
01296 *            LENGTH (14)
01297 *        END-EXEC
           MOVE 14
             TO DFHEIV11
      *    MOVE '@"L                   #   #00008121' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303038313231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CABENI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
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
01342      
      * EXEC CICS READ
01343 *        DATASET  (WS-CONTROL-FILE-DSID)
01344 *        RIDFLD   (WS-CONTROL-FILE-KEY)
01345 *        SET      (ADDRESS OF CONTROL-FILE)
01346 *    END-EXEC.
      *    MOVE '&"S        E          (   #00008170' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038313730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01347
01348 ******************************************************************
01349 *  ACQUIRE STORAGE FOR PENDING CERTIFICATE MAINTENANCE FILE AND  *
01350 *  INITIALIZE NUMERIC FIELDS                                     *
01351 ******************************************************************
01352
01353      
      * EXEC CICS GETMAIN
01354 *        SET     (ADDRESS OF PENDING-MAINT-TO-CERT-FILE)
01355 *        LENGTH  (300)
01356 *        INITIMG (WS-SPACES)
01357 *    END-EXEC.
           MOVE 300
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00008181' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303038313831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 WS-SPACES
           SET ADDRESS OF PENDING-MAINT-TO-CERT-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
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
01394      
      * EXEC CICS READ UPDATE
01395 *        DATASET (WS-CERTIFICATE-MASTER-DSID)
01396 *        RIDFLD  (WS-CERTIFICATE-KEY)
01397 *        SET     (ADDRESS OF CERTIFICATE-MASTER)
01398 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00008222' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038323232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CERTIFICATE-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CERTIFICATE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
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
                 
      * EXEC CICS READ
      *             UPDATE
      *             DATASET   ('ERMAIL')
      *             RIDFLD    (WS-ERMAIL-KEY)
      *             SET       (ADDRESS OF MAILING-DATA)
      *             RESP      (WS-RESPONSE)
      *          END-EXEC
           MOVE 'ERMAIL' TO DFHEIV1
      *    MOVE '&"S        EU         (  N#00008462' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303038343632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ERMAIL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF MAILING-DATA TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
                 IF RESP-NORMAL
                    IF CBNAMEI NOT = MA-CRED-BENE-NAME
                       MOVE CBNAMEI    TO MA-CRED-BENE-NAME
                       
      * EXEC CICS REWRITE
      *                   DATASET      ('ERMAIL')
      *                   FROM         (MAILING-DATA)
      *                   RESP         (WS-RESPONSE)
      *                END-EXEC
           MOVE LENGTH OF
            MAILING-DATA
             TO DFHEIV11
           MOVE 'ERMAIL' TO DFHEIV1
      *    MOVE '&& L                  %  N#00008472' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'204E233030303038343732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 MAILING-DATA, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
                    END-IF
                 END-IF
              END-IF
           END-IF
01599      
      * EXEC CICS REWRITE
01600 *        DATASET (WS-CERTIFICATE-MASTER-DSID)
01601 *        FROM    (CERTIFICATE-MASTER)
01602 *    END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00008481' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303038343831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CERTIFICATE-MASTER-DSID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
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
040909            
      * EXEC CICS READ
040909*               UPDATE
040909*               DATASET  (WS-CERT-TRAILERS-DSID)
040909*               RIDFLD   (WS-ELCRTT-KEY)
040909*               SET      (ADDRESS OF CERTIFICATE-TRAILERS)
040909*               RESP     (WS-RESPONSE)
040909*           END-EXEC
      *    MOVE '&"S        EU         (  N#00008495' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303038343935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CERT-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ELCRTT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-TRAILERS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
040909            IF RESP-NORMAL
040909               IF CVINI NOT EQUAL CS-VIN-NUMBER
040909                   MOVE CVINI TO CS-VIN-NUMBER
040909                   
      * EXEC CICS REWRITE
040909*                     DATASET  (WS-CERT-TRAILERS-DSID)
040909*                     FROM     (CERTIFICATE-TRAILERS)
040909*                     RESP     (WS-RESPONSE)
040909*                  END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-TRAILERS
             TO DFHEIV11
      *    MOVE '&& L                  %  N#00008505' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'204E233030303038353035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CERT-TRAILERS-DSID, 
                 CERTIFICATE-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
040909               END-IF
040909            ELSE
040909               IF RESP-NOTFND
040909                  MOVE SPACES       TO CERTIFICATE-TRAILERS
040909                  MOVE 'CS'         TO CS-RECORD-ID
040909                  MOVE WS-CERTIFICATE-KEY TO WS-ELCRTT-PRIMARY
040909                  MOVE 'C'          TO WS-ELCRTT-REC-TYPE
040909                  MOVE WS-ELCRTT-KEY TO CS-CONTROL-PRIMARY
040909                  MOVE CVINI        TO CS-VIN-NUMBER
040909                  
      * EXEC CICS WRITE
040909*                    DATASET  (WS-CERT-TRAILERS-DSID)
040909*                    RIDFLD   (WS-ELCRTT-KEY)
040909*                    FROM     (CERTIFICATE-TRAILERS)
040909*                    RESP     (WS-RESPONSE)
040909*                 END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-TRAILERS
             TO DFHEIV11
      *    MOVE '&$ L                  ''  N#00008519' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303038353139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CERT-TRAILERS-DSID, 
                 CERTIFICATE-TRAILERS, 
                 DFHEIV11, 
                 WS-ELCRTT-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
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
121410         
      * EXEC CICS READ
121410*            UPDATE
121410*            DATASET  (WS-CERT-TRAILERS-DSID)
121410*            RIDFLD   (WS-ELCRTT-KEY)
121410*            SET      (ADDRESS OF CERTIFICATE-TRAILERS)
121410*            RESP     (WS-RESPONSE)
121410*        END-EXEC
      *    MOVE '&"S        EU         (  N#00008536' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303038353336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CERT-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ELCRTT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-TRAILERS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
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
121712            
      * EXEC CICS REWRITE
121410*                  DATASET  (WS-CERT-TRAILERS-DSID)
121410*                  FROM     (CERTIFICATE-TRAILERS)
121410*                  RESP     (WS-RESPONSE)
121712*           END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-TRAILERS
             TO DFHEIV11
      *    MOVE '&& L                  %  N#00008553' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'204E233030303038353533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CERT-TRAILERS-DSID, 
                 CERTIFICATE-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
121410        END-IF
121410     END-IF.
121410
01604      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
01605          MOVE +1                 TO  WS-COMPLETED-SUCCESSFUL
01606          GO TO 0100-DISPLAY-CERTIFICATE.
01607
01608      
      * EXEC CICS HANDLE CONDITION
01609 *        DUPREC (2000-DUPREC)
01610 *    END-EXEC.
      *    MOVE '"$%                   ! $ #00008565' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303038353635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01611
01612      MOVE PENDING-MAINT-TO-CERT-FILE TO WS-SAVE-CERT-CHANGE-REC.
01613
01614      IF CERT-WAS-CREATED-FOR-CLAIM
01615          NEXT SENTENCE
01616      ELSE
01617          
      * EXEC CICS WRITE
01618 *            DATASET (WS-CERT-MAINT-FILE-DSID)
01619 *            RIDFLD  (CC-CONTROL-PRIMARY)
01620 *            FROM    (PENDING-MAINT-TO-CERT-FILE)
01621 *        END-EXEC.
           MOVE LENGTH OF
            PENDING-MAINT-TO-CERT-FILE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00008574' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CERT-MAINT-FILE-DSID, 
                 PENDING-MAINT-TO-CERT-FILE, 
                 DFHEIV11, 
                 CC-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01622
01623      MOVE +1                     TO  WS-COMPLETED-SUCCESSFUL.
01624
01625      GO TO 0100-DISPLAY-CERTIFICATE.
01626
01627  2000-DUPREC.
01628      MOVE WS-SAVE-CC-KEY         TO  CC-CONTROL-PRIMARY.
01629
01630      
      * EXEC CICS READ UPDATE
01631 *        DATASET (WS-CERT-MAINT-FILE-DSID)
01632 *        RIDFLD  (CC-CONTROL-PRIMARY)
01633 *        INTO    (PENDING-MAINT-TO-CERT-FILE)
01634 *    END-EXEC.
           MOVE LENGTH OF
            PENDING-MAINT-TO-CERT-FILE
             TO DFHEIV11
      *    MOVE '&"IL       EU         (   #00008587' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CERT-MAINT-FILE-DSID, 
                 PENDING-MAINT-TO-CERT-FILE, 
                 DFHEIV11, 
                 CC-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01635
01636      MOVE WS-SAVE-CERT-CHANGE-REC TO PENDING-MAINT-TO-CERT-FILE.
01637
01638      
      * EXEC CICS REWRITE
01639 *        DATASET (WS-CERT-MAINT-FILE-DSID)
01640 *        FROM    (PENDING-MAINT-TO-CERT-FILE)
01641 *        END-EXEC.
           MOVE LENGTH OF
            PENDING-MAINT-TO-CERT-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00008595' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CERT-MAINT-FILE-DSID, 
                 PENDING-MAINT-TO-CERT-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
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
           
      * EXEC CICS STARTBR
05731 *       DATASET    ('ELMSTR5')
05732 *       RIDFLD     (ELMSTR5-KEY)
      *       RESP       (WS-RESPONSE)
05733 *    END-EXEC
           MOVE 'ELMSTR5' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00008611' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303038363131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ELMSTR5-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF RESP-NORMAL
              PERFORM UNTIL I-AM-DONE
                 
      * EXEC CICS READNEXT
05742 *             DATASET   ('ELMSTR5')
05743 *             SET       (ADDRESS OF CLAIM-MASTER)
05744 *             RIDFLD    (ELMSTR5-KEY)
      *             RESP      (WS-RESPONSE)
05745 *          END-EXEC
           MOVE 'ELMSTR5' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )  N#00008618' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303038363138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELMSTR5-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
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
              
      * EXEC CICS ENDBR
05731 *          DATASET    ('ELMSTR5')
      *          RESP       (WS-RESPONSE)
05733 *       END-EXEC
           MOVE 'ELMSTR5' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $  N#00008645' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'204E233030303038363435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           END-IF
           .
       2200-EXIT.
           EXIT.
       2300-READ-ERACCT.
           MOVE CM-CONTROL-PRIMARY     TO ERACCT-KEY
           MOVE LOW-VALUES             TO ERACCT-FILL
           
      * EXEC CICS READ
      *         DATASET   ('ERACCT')
      *         INTO      (ACCOUNT-MASTER)
      *         RIDFLD    (ERACCT-KEY)
      *         GTEQ
      *         RESP      (WS-RESPONSE)
      *    END-EXEC
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV11
           MOVE 'ERACCT' TO DFHEIV1
      *    MOVE '&"IL       G          (  N#00008656' TO DFHEIV0
           MOVE X'2622494C2020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303038363536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACCOUNT-MASTER, 
                 DFHEIV11, 
                 ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       2300-EXIT.
           EXIT.
01652  3000-WRITE-PI-TS.
01653
01654      
      * EXEC CICS WRITEQ TS
01655 *        QUEUE  (W-QID)
01656 *        FROM   (PROGRAM-INTERFACE-BLOCK)
01657 *        LENGTH (PI-COMM-LENGTH)
01658 *        ITEM   (W-ONE)
01659 *    END-EXEC.
      *    MOVE '*" I   L              ''   #00008668' TO DFHEIV0
           MOVE X'2A2220492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038363638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 W-ONE, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01660
01661  3000-EXIT.
01662      EXIT.
01663      EJECT
01664  3100-RECOVER-PI-TS.
01665
01666      
      * EXEC CICS HANDLE CONDITION
01667 *        QIDERR  (3100-QID-ERROR)
01668 *    END-EXEC.
      *    MOVE '"$N                   ! % #00008680' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303038363830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01669
01670      
      * EXEC CICS READQ TS
01671 *        QUEUE  (W-QID)
01672 *        LENGTH (PI-COMM-LENGTH)
01673 *        INTO   (PROGRAM-INTERFACE-BLOCK)
01674 *        ITEM   (W-ONE)
01675 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00008684' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038363834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 W-ONE, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01676
01677  3100-DELETE-PI-TS.
01678
01679      
      * EXEC CICS DELETEQ TS
01680 *        QUEUE  (W-QID)
01681 *    END-EXEC.
      *    MOVE '*&                    #   #00008693' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303038363933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
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
           
      * EXEC CICS LINK
      *        PROGRAM  ('ELCANC')
      *        COMMAREA (CANCEL-GEN-PASS-AREA)
      *    END-EXEC
           MOVE LENGTH OF
            CANCEL-GEN-PASS-AREA
             TO DFHEIV11
           MOVE 'ELCANC' TO DFHEIV1
      *    MOVE '."C                   (   #00008781' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038373831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CANCEL-GEN-PASS-AREA, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
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
062712     
      * EXEC CICS READ
062712*        DATASET (WS-CERTIFICATE-MASTER-DSID)
062712*        RIDFLD  (WS-CERTIFICATE-KEY)
062712*        SET     (ADDRESS OF CERTIFICATE-MASTER)
062712*        RESP    (WS-RESPONSE)
062712*    END-EXEC
      *    MOVE '&"S        E          (  N#00008830' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303038383330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CERTIFICATE-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CERTIFICATE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
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
062712     
      * EXEC CICS READ
062712*       DATASET   ('ERMAIL')
062712*       SET       (ADDRESS OF MAILING-DATA)
062712*       RIDFLD    (WS-ERMAIL-KEY)
062712*       RESP      (WS-RESPONSE)
062712*    END-EXEC
           MOVE 'ERMAIL' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00008862' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303038383632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ERMAIL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF MAILING-DATA TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
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
062712     
      * EXEC CICS READ
062712*       DATASET   ('ELCRTO')
062712*       SET       (ADDRESS OF ORIGINAL-CERTIFICATE)
062712*       RIDFLD    (ELCRTO-KEY)
062712*       GTEQ
062712*       RESP      (WS-RESPONSE)
062712*    END-EXEC
           MOVE 'ELCRTO' TO DFHEIV1
      *    MOVE '&"S        G          (  N#00008879' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303038383739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCRTO-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ORIGINAL-CERTIFICATE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
062712
062712     display ' just read gteq ' ws-response
062712     IF RESP-NORMAL
062712        AND (OC-CONTROL-PRIMARY (1:33) =
062712                 WS-CERTIFICATE-KEY)
062712        AND (OC-RECORD-TYPE = 'I')
062712        display ' resp norm, key =, type i '
062712        IF (OC-ENDORSEMENT-PROCESSED-DT = LOW-VALUES)
062712           
      * EXEC CICS READ
062712*             DATASET   ('ELCRTO')
062712*             SET       (ADDRESS OF ORIGINAL-CERTIFICATE)
062712*             RIDFLD    (OC-CONTROL-PRIMARY)
062712*             UPDATE
062712*             RESP      (WS-RESPONSE)
062712*          END-EXEC
           MOVE 'ELCRTO' TO DFHEIV1
      *    MOVE '&"S        EU         (  N#00008894' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303038383934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 OC-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ORIGINAL-CERTIFICATE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
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
062712           
      * EXEC CICS REWRITE
062712*             DATASET   ('ELCRTO')
062712*             FROM      (ORIGINAL-CERTIFICATE)
062712*             RESP      (WS-RESPONSE)
062712*          END-EXEC
           MOVE LENGTH OF
            ORIGINAL-CERTIFICATE
             TO DFHEIV11
           MOVE 'ELCRTO' TO DFHEIV1
      *    MOVE '&& L                  %  N#00008978' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'204E233030303038393738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ORIGINAL-CERTIFICATE, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
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
062712     
      * EXEC CICS WRITE
062712*       DATASET   ('ELCRTO')
062712*       FROM      (ORIGINAL-CERTIFICATE)
062712*       RIDFLD    (OC-CONTROL-PRIMARY)
062712*       RESP      (WS-RESPONSE)
062712*    END-EXEC
           MOVE LENGTH OF
            ORIGINAL-CERTIFICATE
             TO DFHEIV11
           MOVE 'ELCRTO' TO DFHEIV1
      *    MOVE '&$ L                  ''  N#00009085' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303039303835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ORIGINAL-CERTIFICATE, 
                 DFHEIV11, 
                 OC-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
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
01737      
      * EXEC CICS SEND
01738 *        FROM   (EL127CI)
01739 *        MAPSET (WS-MAPSET-NAME)
01740 *        MAP    (WS-MAP-NAME)
01741 *        CURSOR
01742 *        ERASE
01743 *    END-EXEC.
           MOVE LENGTH OF
            EL127CI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00009157' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303039313537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL127CI, 
                 DFHEIV12, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
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
01790      
      * EXEC CICS SEND DATAONLY
01791 *        FROM   (EL127CI)
01792 *        MAPSET (WS-MAPSET-NAME)
01793 *        MAP    (WS-MAP-NAME)
01794 *        CURSOR
01795 *    END-EXEC.
           MOVE LENGTH OF
            EL127CI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00009213' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303039323133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL127CI, 
                 DFHEIV12, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01796
01797      GO TO 9100-RETURN-TRAN.
01798
01799  8200-EXIT.
01800      EXIT.
01801
01802      EJECT
01803  8300-SEND-TEXT.
01804      
      * EXEC CICS SEND TEXT
01805 *        FROM   (LOGOFF-TEXT)
01806 *        LENGTH (LOGOFF-LENGTH)
01807 *        ERASE
01808 *        FREEKB
01809 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00009227' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303039323237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LOGOFF-TEXT, 
                 LOGOFF-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01810
01811      
      * EXEC CICS RETURN
01812 *    END-EXEC.
      *    MOVE '.(                    ''   #00009234' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303039323334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
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
01826      
      * EXEC CICS HANDLE CONDITION
01827 *        NOTFND (8410-STATE-REC-NOTFND)
01828 *    END-EXEC.
      *    MOVE '"$I                   ! & #00009249' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303039323439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01829
01830      
      * EXEC CICS READ
01831 *        DATASET  (WS-CONTROL-FILE-DSID)
01832 *        RIDFLD   (WS-CONTROL-FILE-KEY)
01833 *        SET      (ADDRESS OF CONTROL-FILE)
01834 *    END-EXEC.
      *    MOVE '&"S        E          (   #00009253' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039323533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
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
01846      
      * EXEC CICS LINK
01847 *        PROGRAM  (ELDATCV)
01848 *        COMMAREA (DATE-CONVERSION-DATA)
01849 *        LENGTH   (DC-COMM-LENGTH)
01850 *    END-EXEC.
      *    MOVE '."C                   (   #00009269' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039323639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELDATCV, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01851
01852  8500-EXIT.
01853      EXIT.
01854
01855  8600-DEEDIT.
01856      
      * EXEC CICS BIF
01857 *         DEEDIT
01858 *         FIELD  (DEEDIT-FIELD)
01859 *         LENGTH (15)
01860 *    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00009279' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303039323739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01861
01862  8600-EXIT.
01863      EXIT.
01864
01865  8700-LOCATE-BENEFIT.
01866      
      * EXEC CICS HANDLE CONDITION
01867 *        NOTFND (8700-EXIT)
01868 *    END-EXEC.
      *    MOVE '"$I                   ! '' #00009289' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303039323839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01869
01870      MOVE SPACES                 TO  WS-KIND.
01871      MOVE ZERO                   TO  WS-NOT-FOUND.
01872
01873      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
01874      MOVE SPACES                 TO  WS-CFK-ACCESS.
01875      MOVE WS-BENEFIT-NO          TO  WS-CFK-BENEFIT-NO.
01876
01877      
      * EXEC CICS READ
01878 *        DATASET (WS-CONTROL-FILE-DSID)
01879 *        RIDFLD  (WS-CONTROL-FILE-KEY)
01880 *        SET     (ADDRESS OF CONTROL-FILE)
01881 *        GTEQ
01882 *    END-EXEC.
      *    MOVE '&"S        G          (   #00009300' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039333030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
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
040909     
      * EXEC CICS HANDLE CONDITION
040909*        NOTFND (8800-CERT-TRL-REC-NOTFND)
040909*    END-EXEC.
      *    MOVE '"$I                   ! ( #00009338' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303039333338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
040909
040909     
      * EXEC CICS READ
040909*        DATASET  (WS-CERT-TRAILERS-DSID)
040909*        RIDFLD   (WS-ELCRTT-KEY)
040909*        SET      (ADDRESS OF CERTIFICATE-TRAILERS)
040909*    END-EXEC.
      *    MOVE '&"S        E          (   #00009342' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039333432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CERT-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ELCRTT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
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
01929      
      * EXEC CICS RETURN
01930 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
01931 *        LENGTH   (PI-COMM-LENGTH)
01932 *        TRANSID  (WS-TRANS-ID)
01933 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00009377' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303039333737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01934
01935  9100-EXIT.
01936      EXIT.
01937
01938  9300-XCTL.
01939      MOVE DFHENTER               TO  EIBAID.
01940
01941      
      * EXEC CICS XCTL
01942 *        PROGRAM  (THIS-PGM)
01943 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
01944 *        LENGTH   (PI-COMM-LENGTH)
01945 *    END-EXEC.
      *    MOVE '.$C                   %   #00009389' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303039333839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 THIS-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
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
01963      
      * EXEC CICS LINK
01964 *        PROGRAM  (EL001)
01965 *        COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)
01966 *        LENGTH   (EMI-COMM-LENGTH)
01967 *    END-EXEC.
      *    MOVE '."C                   (   #00009411' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039343131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EL001, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01968
01969      MOVE ZERO                   TO  EMI-ERROR.
01970
01971  9900-EXIT.
01972      EXIT.
01973
01974      EJECT
01975
01976  9800-LINK-REM-TERM.
01977      
      * EXEC CICS LINK
01978 *        PROGRAM  (ELRTRM)
01979 *        COMMAREA (CALCULATION-PASS-AREA)
01980 *        LENGTH   (CP-COMM-LENGTH)
01981 *    END-EXEC.
      *    MOVE '."C                   (   #00009425' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039343235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELRTRM, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01982
01983  9800-EXIT.
01984      EXIT.
01985
01986  9990-ERROR.
01987      MOVE DFHEIBLK               TO  EMI-LINE1.
01988
01989      
      * EXEC CICS LINK
01990 *        PROGRAM  (EL004)
01991 *        COMMAREA (EMI-LINE1)
01992 *        LENGTH   (72)
01993 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00009437' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039343337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EL004, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
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
02004 *                            COPY ELCSCTP.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCSCTP                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   DESCRIPTION = C.I.C.S. COMMON SECURITY-MESSAGE LINK          *
00007 ******************************************************************
00008
00008
00009      MOVE EIBDATE          TO SM-JUL-DATE.
00010      MOVE EIBTRMID         TO SM-TERMID.
00011      MOVE THIS-PGM         TO SM-PGM.
00012      MOVE EIBTIME          TO TIME-IN.
00013      MOVE TIME-OUT         TO SM-TIME.
00014      MOVE PI-PROCESSOR-ID  TO SM-PROCESSOR-ID.
00015
00016      
      * EXEC CICS LINK
00017 *         PROGRAM  ('EL003')
00018 *         COMMAREA (SECURITY-MESSAGE)
00019 *         LENGTH   (80)
00020 *    END-EXEC.
           MOVE 'EL003' TO DFHEIV1
           MOVE 80
             TO DFHEIV11
      *    MOVE '."C                   (   #00009469' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039343639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 SECURITY-MESSAGE, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00021
00022 ******************************************************************
00023

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1273' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 2100-WRITE-DUPKEY,
                     9990-ERROR,
                     0100-DISPLAY-CERTIFICATE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 8880-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 2000-DUPREC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 3100-QID-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 8410-STATE-REC-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 8700-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 8800-CERT-TRL-REC-NOTFND
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1273' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
