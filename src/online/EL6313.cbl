00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL6313.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 12/07/94 16:01:04.
00007 *                            VMOD=2.017.
00008 *
00009 *AUTHOR.     LOGIC,INC.
00010 *            DALLAS, TEXAS.
00011
00012 *DATE-COMPILED.
00013 *SECURITY.   *****************************************************
00014 *            *                                                   *
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00016 *            *                                                   *
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00020 *            *                                                   *
00021 *            *****************************************************
00022
00023 *REMARKS. TRANSACTION - EXB3 - NEW BUSINESS REVIEW AND CORRECTIONS
00024 *                              SECOND SCREEN.
00025
013107******************************************************************
013107*                   C H A N G E   L O G
013107*
013107* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
013107*-----------------------------------------------------------------
013107*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
013107* EFFECTIVE    NUMBER
013107*-----------------------------------------------------------------
013107* 013107  CR2007010300001  PEMA  ADD PERFORM STATEMENT
090408* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
072312* 072312  CR2011022800001  AJRA  NAPERSOFT MISC
121712* 121712  CR2012101700002  AJRA  ADD DEFAULT AGE FLAG
011413* 011413  IR2012122700003  AJRA  ADD CRTO ISSUE/CANCEL INDICATOR
100213* 100213  CR2013090300001  AJRA  NAPERSOFT PHASE 2
121713* 121713  CR2013090300001  AJRA  NO CHANGES TO PRIMARY NAME
100217* 100217  CR2016091600001  PEMA  ADD EDIT FOR ZIP CODE
041320* 041320  CR2020040200001  PEMA  PENDING BUSINESS JOURNALING
013107******************************************************************
00026  ENVIRONMENT DIVISION.
00027
00028      EJECT
00029  DATA DIVISION.
00030  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00031  77  FILLER  PIC X(32)  VALUE '********************************'.
00032  77  FILLER  PIC X(32)  VALUE '*    EL6313 WORKING STORAGE    *'.
00033  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.017 *********'.
121712 77  WS-CERT-TRL-REC-NOT-FOUND   PIC S9       VALUE +0.
121712     88  CERT-TRL-REC-NOT-FOUND     VALUE +1.
100217 77  a1                          pic s999 comp-3 value +0.
00034
00035 *    COPY ELCSCTM.
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
00036 *    COPY ELCSCRTY.
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
00037
100217 01  P pointer.
100217 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
100217 01  var-ptr pointer.
100217 01  env-var-len                 pic 9(4)  binary.
100217 01  rc                          pic 9(9)  binary.
100217
100217 01  WS-KIXSYS.
100217     05  WS-KIX-FIL1             PIC X(10).
100217     05  WS-KIX-APPS             PIC X(10).
100217     05  WS-KIX-ENV              PIC X(10).
100217     05  WS-KIX-MYENV            PIC X(10).
100217     05  WS-KIX-SYS              PIC X(10).
00038  01  STANDARD-AREAS.
00039      12  GETMAIN-SPACE       PIC X       VALUE SPACE.
00040      12  EL631F              PIC X(8)    VALUE 'EL631F'.
00041      12  MAPSET-EL6313S      PIC X(8)    VALUE 'EL6313S'.
00042      12  SCREEN-NUMBER       PIC X(6)    VALUE 'EL631F'.
00043      12  TRANS-EXB3          PIC X(4)    VALUE 'EXB3'.
00044      12  THIS-PGM            PIC X(8)    VALUE 'EL6313'.
00045      12  PGM-NAME            PIC X(8).
00046      12  TIME-IN             PIC S9(7).
00047      12  TIME-OUT-R  REDEFINES TIME-IN.
00048          16  FILLER          PIC X.
00049          16  TIME-OUT        PIC 99V99.
00050          16  FILLER          PIC XX.
00051      12  LINK-EL001          PIC X(8)    VALUE 'EL001'.
00052      12  LINK-EL004          PIC X(8)    VALUE 'EL004'.
00053      12  LINK-EL050        PIC X(8)      VALUE 'EL050'.
00054      12  XCTL-EL005          PIC X(8)    VALUE 'EL005'.
00055      12  XCTL-EL010          PIC X(8)    VALUE 'EL010'.
00056      12  XCTL-EL626          PIC X(8)    VALUE 'EL626'.
00057      12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.
00058      12  FILE-ID-ERPNDB      PIC X(8)    VALUE 'ERPNDB'.
00059      12  FILE-ID-ERPNDM      PIC X(8)    VALUE 'ERPNDM'.
00060      12  FILE-ID-ELCERT      PIC X(8)    VALUE 'ELCERT'.
00061      12  FILE-ID-ELCNTL      PIC X(8)    VALUE 'ELCNTL'.
121712     12  CRTT-ID             PIC X(8)    VALUE 'ELCRTT'.
00062      12  WS-CURRENT-DT       PIC X(8)    VALUE SPACES.
00063      12  WS-CURRENT-BIN-DT   PIC XX      VALUE SPACES.
00064      12  WS-TERM-IN-DAYS-SW  PIC X.
00065          88  WS-TERM-IN-DAYS-FOUND       VALUE 'Y'.
00066      12  WS-ADD-ADDRESS-SW   PIC X       VALUE 'N'.
00067          88  WS-ADDRESS-ADDED            VALUE 'Y'.
           12  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL              VALUE +00.
               88  RESP-ERROR               VALUE +01.
               88  RESP-NOTFND              VALUE +13.
               88  RESP-NOTOPEN             VALUE +19.
               88  RESP-ENDFILE             VALUE +20.
00068
00069      EJECT
00070
00071  01  ERROR-MESSAGES.
00072      12  ER-0004                 PIC X(4)  VALUE '0004'.
00073      12  ER-0008                 PIC X(4)  VALUE '0008'.
00074      12  ER-0029                 PIC X(4)  VALUE '0029'.
00075      12  ER-0042                 PIC X(4)  VALUE '0042'.
00076      12  ER-0070                 PIC X(4)  VALUE '0070'.
           12  ER-2209                 PIC X(4)  VALUE '2209'.
00077      12  ER-2223                 PIC X(4)  VALUE '2223'.
           12  ER-2228                 PIC X(4)  VALUE '2228'.
00078      12  ER-2239                 PIC X(4)  VALUE '2239'.
00079      12  ER-2600                 PIC X(4)  VALUE '2600'.
00080      12  ER-2625                 PIC X(4)  VALUE '2625'.
00081      12  ER-2695                 PIC X(4)  VALUE '2695'.
00082      12  ER-2725                 PIC X(4)  VALUE '2725'.
100213     12  ER-3269                 PIC X(4)  VALUE '3269'.
00083      12  ER-7822                 PIC X(4)  VALUE '7822'.
00084      12  ER-8204                 PIC X(4)  VALUE '8204'.
00085
00086      EJECT
00087
00088  01  ACCESS-KEYS.
00089      12  ERPNDB-KEY.
00090          16  ERPNDB-COMP-CD          PIC X     VALUE SPACE.
00091          16  ERPNDB-ENTRY-BATCH      PIC X(6)  VALUE SPACES.
00092          16  ERPNDB-BATCH-SEQ        PIC S9(4) VALUE +1 COMP.
00093          16  ERPNDB-BATCH-CHG-SEQ    PIC S9(4) VALUE +0 COMP.
00094
00095      12  ERPNDB-RECORD-LENGTH        PIC S9(4) COMP VALUE +585.
00096
00097      12  ERPNDM-KEY.
00098          16  ERPNDM-COMP-CD          PIC X     VALUE SPACE.
00099          16  ERPNDM-ENTRY-BATCH      PIC X(6)  VALUE SPACES.
00100          16  ERPNDM-BATCH-SEQ        PIC S9(4) VALUE +1 COMP.
00101          16  ERPNDM-BATCH-CHG-SEQ    PIC S9(4) VALUE +0 COMP.
00102
CIDMOD*    12  ERPNDM-RECORD-LENGTH        PIC S9(4) COMP VALUE +250.
CIDMOD     12  ERPNDM-RECORD-LENGTH        PIC S9(4) COMP VALUE +374.
00104
           12  ELCRTO-KEY.
               16  ELCRTO-COMPANY-CD       PIC X.
               16  ELCRTO-CARRIER          PIC X.
               16  ELCRTO-GROUPING         PIC X(6).
               16  ELCRTO-STATE            PIC XX.
               16  ELCRTO-ACCOUNT          PIC X(10).
               16  ELCRTO-CERT-EFF-DT      PIC XX.
               16  ELCRTO-CERT-NO.
                   20  ELCRTO-CERT-PRIME   PIC X(10).
                   20  ELCRTO-CERT-SFX     PIC X.
               16  ELCRTO-RECORD-TYPE      PIC X.
               16  ELCRTO-SEQ-NO           PIC 9(4)  BINARY.
00105      12  ELCERT-KEY.
00106          16  ELCERT-COMPANY-CD       PIC X     VALUE SPACE.
00107          16  ELCERT-CARRIER          PIC X     VALUE SPACE.
00108          16  ELCERT-GROUPING         PIC X(6)  VALUE SPACE.
00109          16  ELCERT-STATE            PIC XX    VALUE SPACE.
00110          16  ELCERT-ACCOUNT          PIC X(10) VALUE SPACE.
00111          16  ELCERT-CERT-EFF-DT      PIC XX    VALUE SPACE.
00112          16  ELCERT-CERT-NO.
00113              20  ELCERT-CERT-PRIME   PIC X(10) VALUE SPACE.
00114              20  ELCERT-CERT-SFX     PIC X     VALUE SPACE.
00115
00116      12  ELCERT-RECORD-LENGTH        PIC S9(4) COMP VALUE +450.
00117
121712
121712     12  ELCRTT-KEY.
121712         16  ELCRTT-PRIMARY          PIC X(33).
121712         16  ELCRTT-REC-TYPE         PIC X(1).
121712     12  ELCRTT-RECORD-LENGTH        PIC S9(4) COMP VALUE +552.
121712     12  ELCRTT-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +575.
041320     12  ERPNDB-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +585.
041320     12  WS-JOURNAL-RECORD-LENGTH    PIC S9(4) COMP VALUE +0000.
121712
           12  ELCNTL-KEY.
               16  ELCNTL-COMPANY-ID   PIC X(3)  VALUE SPACES.
               16  ELCNTL-REC-TYPE     PIC X     VALUE SPACES.
               16  ELCNTL-ACCESS.
                   20  FILLER          PIC XX.
                   20  FILLER          PIC XX.
               16  ELCNTL-SEQ          PIC S9(4) VALUE +0 COMP.
00125  01  WORK-AREA.
00126      12  DEEDIT-FIELD            PIC X(15).
00127      12  FILLER REDEFINES DEEDIT-FIELD.
00128          16  FILLER              PIC X(4).
00129          16  DEEDIT-FIELD-X11    PIC X(11).
00130      12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD PIC S9(15).
CIDMOD     12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD PIC S9(13)V99.
CIDMOD     12  DEEDIT-FIELD-V3 REDEFINES DEEDIT-FIELD PIC S9(12)V9(3).
CIDMOD     12  DEEDIT-FIELD-V4 REDEFINES DEEDIT-FIELD PIC S9(11)V9(4).
CIDMOD     12  DEEDIT-FIELD-V5 REDEFINES DEEDIT-FIELD PIC S9(10)V9(5).
00131      12  FILLER REDEFINES DEEDIT-FIELD.
00132          16  FILLER              PIC X(8).
00133          16  DEEDIT-FIELD-RATE   PIC S99V9(5).
00134      12  FILLER REDEFINES DEEDIT-FIELD.
00135          16  FILLER              PIC X(7).
00136          16  WS-XRATE-OUT        PIC X(8).
00137      12  FILLER REDEFINES DEEDIT-FIELD.
00138          16  FILLER              PIC X(7).
00139          16  WS-RATE-OUT         PIC ZZ.99999.
00140
00141      12  WS-EDIT-CODE                PIC X(4)  VALUE SPACES.
           12  WS-CONVERTED-BIRTH      PIC XX    VALUE LOW-VALUES.
           12  CENTURY-ADJ             PIC S9(08) VALUE +38400 COMP.
           12  WS-WORK-BIN-RED         PIC S9(08) VALUE +0 COMP.
           12  FILLER REDEFINES WS-WORK-BIN-RED.
               16  FILLER              PIC XX.
               16  WS-WORK-BIN-DT      PIC XX.
00143      12  WS-SAVE-INPUT-FIELDS.
00144
00145          16  WS-JNTAGE               PIC S99     VALUE +0  COMP-3.
00146          16  WS-PHONE                PIC S9(12)  VALUE +0  COMP-3.
00147
00148      12  WS-TIME                     PIC 9(6)     VALUE 0.
00149      12  WS-HR-MINS-SECS REDEFINES WS-TIME.
00150          16  WS-HR-MINS              PIC 99V99.
00151          16  FILLER                  PIC XX.
00152
00153      12  WS-EXP-DT-EDIT              PIC 99B99B99.
00154
00155      12  WS-ZIP-CODE.
00156          16  WS-ZIP-1            PIC X.
00157              88  WS-CANADIAN-ZIP    VALUE 'A' THRU 'Z'.
00158          16  WS-ZIP-2-3          PIC XX.
00159          16  WS-ZIP-4            PIC X.
00160          16  WS-ZIP-5            PIC X.
00161          16  WS-ZIP-6            PIC X.
00162          16  FILLER              PIC X(4).
00163      12  WS-ZIP-AM-1  REDEFINES  WS-ZIP-CODE.
00164          16  WS-ZIP-AM-1-CODE    PIC X(5).
00165          16  WS-ZIP-AM-1-PLUS4   PIC X(4).
00166          16  FILLER              PIC X.
00167      12  WS-ZIP-AM-2  REDEFINES  WS-ZIP-CODE.
00168          16  WS-ZIP-AM-2-CODE    PIC X(5).
00169          16  WS-ZIP-AM-2-DASH    PIC X.
00170          16  WS-ZIP-AM-2-PLUS4   PIC X(4).
00171      12  WS-ZIP-CAN-1  REDEFINES  WS-ZIP-CODE.
00172          16  WS-ZIP-CAN-1-POST1  PIC XXX.
00173          16  WS-ZIP-CAN-1-POST2  PIC XXX.
00174          16  FILLER              PIC X(4).
00175      12  WS-ZIP-CAN-2  REDEFINES  WS-ZIP-CODE.
00176          16  WS-ZIP-CAN-2-POST1  PIC XXX.
00177          16  FILLER              PIC X.
00178          16  WS-ZIP-CAN-2-POST2  PIC XXX.
00179          16  FILLER              PIC XXX.
00180
00181      12  WS-LFRTHDG.
00182          16  WS-LFRT-OVERRIDE-L2     PIC XX       VALUE 'LF'.
00183          16  FILLER                  PIC X(10)    VALUE
00184              '-RATE DEV.'.
00185
00186      12  WS-AHRTHDG.
00187          16  WS-AHRT-OVERRIDE-L2     PIC XX       VALUE 'AH'.
00188          16  FILLER                  PIC X(10)    VALUE
00189              '-RATE DEV.'.
00190
00191      12  WS-LFDVHDG.
00192          16  WS-LFDV-OVERRIDE-L2     PIC XX       VALUE 'LF'.
00193          16  FILLER                  PIC X(12)    VALUE
00194              '-RATE DEV. %'.
00195
00196      12  WS-AHDVHDG.
00197          16  WS-AHDV-OVERRIDE-L2     PIC XX       VALUE 'AH'.
00198          16  FILLER                  PIC X(12)    VALUE
00199              '-RATE DEV. %'.
00200
00201      12  WS-LFCMHDG.
00202          16  WS-LFCM-OVERRIDE-L2     PIC XX       VALUE 'LF'.
00203          16  FILLER                  PIC X(6)     VALUE
00204              '-COMM.'.
00205
00206      12  WS-AHCMHDG.
00207          16  WS-AHCM-OVERRIDE-L2     PIC XX       VALUE 'AH'.
00208          16  FILLER                  PIC X(12)    VALUE
00209              '-COMM.'.
00210
00211      12  WS-SAVE-COMMON-ERRORS       PIC X(20)   VALUE SPACES.
00212
00213      12  WS-ERRORS-PRESENT-SW        PIC X        VALUE 'N'.
00214          88 WS-ERRORS-PRESENT                     VALUE 'Y'.
00215          88 WS-ERRORS-NOT-PRESENT                 VALUE 'N'.
00216
00217
00218      12  WS-EDIT-PASS-LENGTH         PIC S9(4) VALUE +1024 COMP.
00219      12  WS-SUB                      PIC S9(4) COMP VALUE +0000.
00220      12  WS-SUB4                     PIC S9(4) COMP VALUE +0000.
00221      12  WS-ERR-SW                   PIC X.
00222          88  STD-ERRORS                        VALUE 'S'.
00223          88  TRN-ERRORS                        VALUE 'T'.
00224
00225      12  WS-SW-1                     PIC X     VALUE SPACE.
00226      12  WS-SW-2                     PIC X     VALUE SPACE.
00227
00228      12  WS-DLO-RESIDENT-STATE.
00229          16  DRES-STATE             PIC XX.
00230          16  DRES-RETURN-CODE       PIC XX.
00231      12  DRES-COMM-LENGTH           PIC S9(4) COMP VALUE +4.
00232
00233      12  WS-PFRATE                  PIC 9(4)V9(4).
121712
121712     12  WS-AGE-FIELDS.
121712         16  WS-INS-AGE-SET             PIC X VALUE 'N'.
121712         16  WS-JNT-AGE-SET             PIC X VALUE 'N'.
121712         16  WS-INS-AGE-DEFAULTED       PIC X VALUE 'N'.
121712         16  WS-JNT-AGE-DEFAULTED       PIC X VALUE 'N'.
00234
00235      EJECT
      *                                COPY ELCCRTO.
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
00236 *    COPY ELCDATE.
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
00237      EJECT
00238 *    COPY ELCLOGOF.
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
00239      EJECT
00240 *    COPY ELCATTR.
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
00241      EJECT
00242 *    COPY ELCEMIB.
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
00243      EJECT
00244 *    COPY ELCINTF.
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
00245      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.
00246 *    COPY ELC631PI.
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
00247             20 FILLER                 PIC X(94).
00248
00249      EJECT
00250
00251 ******************************************************************
00252 *         P A S S   A R E A   F O R   E D I T                    *
00253 ******************************************************************
00254
00255  01  PASSED-RECORD.
00256      12  RECORD-SAVE                  PIC X(585).
00257 *    COPY ELC50W1.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                             ELC50W1                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.004                          *
00006 *                                                                *
00007 *       THIS COPYBOOK IS USED BY EL050, EL051, AND EL517.        *
00008 *                                                                *
00009 *                                                                *
00010 ******************************************************************
00011
00012      12  WK-WORK-AREA.
00013          16  WK-CNTL-RECORD-FOUND-SW  PIC X   VALUE ' '.
00014          16  WK-LAST-CARRIER          PIC X   VALUE ' '.
00015          16  WK-CR-REM-TERM-CALC      PIC X   VALUE ' '.
00016          16  WK-CR-R78-METHOD         PIC X   VALUE ' '.
00017          16  WK-CO-MAX-CAP          PIC S9(3)V99  COMP-3 VALUE +0.
00018          16  WK-CO-TOL-CLAIM        PIC S9(3)V99  COMP-3 VALUE +0.
00019          16  WK-CO-TOL-PREM         PIC S9(3)V99  COMP-3 VALUE +0.
00020          16  WK-CO-TOL-REFUND       PIC S9(3)V99  COMP-3 VALUE +0.
00021          16  WK-CO-TOL-PREM-PCT     PIC S9V9(4)   COMP-3 VALUE +0.
00022          16  WK-CO-TOL-REFUND-PCT   PIC S9V9(4)   COMP-3 VALUE +0.
00023          16  WK-CO-PREM-REJECT-SW     PIC X   VALUE ' '.
00024          16  WK-CO-REF-REJECT-SW      PIC X   VALUE ' '.
00025          16  WK-BIRTH-DATE-INPUT      PIC X   VALUE ' '.
00026          16  WK-JOINT-AGE-INPUT       PIC X   VALUE ' '.
00027          16  WK-CURRENT-MONTH-END     PIC XX     VALUE LOW-VALUE.
00028          16  WK-CREDIT-EDIT-CONTROLS.
00029              20  WK-MIN-PREMIUM       PIC S9(3)V99  COMP-3.
00030              20  WK-MIN-AGE           PIC 99  VALUE 00.
00031              20  WK-DEFAULT-AGE       PIC 99  VALUE 00.
00032              20  WK-MIN-TERM          PIC S9(3) COMP-3 VALUE +0.
00033              20  WK-MAX-TERM          PIC S9(3) COMP-3 VALUE +0.
00034              20  WK-DEFAULT-SEX       PIC X  VALUE ' '.
00035          16  WK-CANCEL-EXIT-DT        PIC XX VALUE LOW-VALUE.
00036          16  WK-SAVE-REIN-DATA.
00037              20  WK-REIN-TABLE        PIC X(3) VALUE SPACES.
00038              20  WK-REIN-ST-AH        PIC X(2) VALUE SPACES.
00039              20  WK-REIN-ST-LF        PIC X(2) VALUE SPACES.
00040          16  WK-ENTRY-SW              PIC X    VALUE ' '.
00041              88  ENTRY-FROM-EL6311       VALUE '6'.
00042          16  WK-REM-TRM-CALC-OPTION   PIC X    VALUE ' '.
00043          16  WK-DEFAULT-APR           PIC S9(3)V9(4) COMP-3.
00044      12  WK-RECORD-ADDRESSES.
00045          16  WK-ACCT-ADDR             PIC S9(8) COMP VALUE ZEROS.
00046          16  WK-LIFE-EDIT-ADDR        PIC S9(8) COMP VALUE ZEROS.
00047          16  WK-AH-EDIT-ADDR          PIC S9(8) COMP VALUE ZEROS.
00048          16  WK-LIFE-BEN-ADDR         PIC S9(8) COMP VALUE ZEROS.
00049          16  WK-AH-BEN-ADDR           PIC S9(8) COMP VALUE ZEROS.
00050          16  WK-STATE-ADDR            PIC S9(8) COMP VALUE ZEROS.
00051          16  WK-PLAN-ADDR             PIC S9(8) COMP VALUE ZEROS.
00052          16  WK-FORM-ADDR             PIC S9(8) COMP VALUE ZEROS.
00053      12  WK-OVER-SHORT-AREA.
00054          16  WK-REFUND-OVS-AMT        PIC S999V99 COMP-3 VALUE +0.
00055          16  WK-REFUND-OVS-PCT        PIC S9V9(4) COMP-3 VALUE +0.
               16  WK-CSR-SESSION-SW        PIC X  VALUE ' '.
                   88  WK-CSR-EDIT-SESSION       VALUE 'Y'.
00056          16  FILLER                   PIC X(5) VALUE SPACES.
00258      EJECT
00259 *    COPY ELCEDITC.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCEDITC                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.012                          *
00006 *                                                                *
00007 *       THIS COPYBOOK IS USED BY EL6311, EL6312,                 *
00008 *                     EL6313, EL050 AND EL517.                   *
00009 *                                                                *
00010 ******************************************************************
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
122002******************************************************************
00011
00012      12  EDIT-CRITERIA-DATA.
00013          16  EC-CO-MONTH-END-DT       PIC XX.
00014          16  EC-CO-TOL-PREM           PIC S9(3)V99  COMP-3.
00015          16  EC-CO-TOL-PREM-PCT       PIC S9V9(4)   COMP-3.
00016          16  EC-CO-TOL-REFUND         PIC S9(3)V99  COMP-3.
00017          16  EC-CO-TOL-REFUND-PCT     PIC S9V9(4)   COMP-3.
00018          16  EC-CO-MIN-AGE            PIC S9(3)     COMP-3.
00019          16  EC-CO-MIN-PREMIUM        PIC S9(5)V99  COMP-3.
00020          16  EC-CO-MIN-TERM           PIC S9(3)     COMP-3.
00021          16  EC-CO-MAX-TERM           PIC S9(3)     COMP-3.
00022          16  EC-CO-REM-TERM-CALC      PIC X.
00023                88  EC-CO-EARN-AFTER-15TH           VALUE '1'.
00024                88  EC-CO-EARN-ON-HALF-MO           VALUE '2'.
00025                88  EC-CO-EARN-ON-1ST-DAY           VALUE '3'.
00026                88  EC-CO-EARN-ON-FULL-MO           VALUE '4'.
00027
00028          16  EC-BR-LF-EARNINGS-CALC    PIC X.
00029                88  EC-BR-LF-EARNING-NOT-USED        VALUE SPACE.
00030                88  EC-BR-LF-EARN-BY-R78             VALUE '1'.
00031                88  EC-BR-LF-EARN-BY-PRO-RATA        VALUE '2'.
00032                88  EC-BR-LF-EARN-AS-TEXAS           VALUE '4'.
00033                88  EC-BR-LF-EARN-AS-FARM-PLAN       VALUE '4'.
00034                88  EC-BR-LF-EARN-IS-NET-PAY         VALUE '5'.
00035                88  EC-BR-LF-EARN-ANTICIPATION       VALUE '6'.
00036                88  EC-BR-LF-EARN-MEAN               VALUE '8'.
00037                88  EC-BR-LF-EARN-SUM-OF-DIGIT       VALUE '9'.
00038                88  EC-BR-LF-EARN-REG-BALLOON        VALUE 'B'.
00039
00040          16  EC-BR-AH-EARNINGS-CALC    PIC X.
00041                88  EC-BR-AH-EARNING-NOT-USED        VALUE SPACE.
00042                88  EC-BR-AH-EARN-BY-R78             VALUE '1'.
00043                88  EC-BR-AH-EARN-BY-PRO-RATA        VALUE '2'.
00044                88  EC-BR-AH-EARN-AS-CALIF           VALUE '3'.
00045                88  EC-BR-AH-EARN-IS-NET-PAY         VALUE '5'.
00046                88  EC-BR-AH-EARN-ANTICIPATION       VALUE '6'.
00047                88  EC-BR-AH-EARN-MEAN               VALUE '8'.
00048                88  EC-BR-AH-EARN-REG-BALLOON        VALUE 'B'.
00049
00050          16  EC-BR-AH-SPECIAL-CALC-CD  PIC X.
00051                  88  EC-CF-AH-OUTSTANDING-BAL         VALUE 'O'.
00052                  88  EC-CF-AH-CRITICAL-PERIOD         VALUE 'C'.
00053
00054          16  EC-BR-LF-SPECIAL-CALC-CD  PIC X.
00055                  88  EC-CF-LF-OUTSTANDING-BAL         VALUE 'O'.
00056                  88  EC-CF-LF-TRUNCATED-LIFE-0        VALUE 'T'.
00057                  88  EC-CF-LF-TRUNCATED-LIFE-1        VALUE 'U'.
00058                  88  EC-CF-LF-TRUNCATED-LIFE-2        VALUE 'V'.
00059                  88  EC-CF-LF-ALTERNATE-NET-PAY       VALUE 'A'.
00060                  88  EC-CF-LF-NET-PAY-SIMPLE          VALUE 'S'.
00061                  88  EC-CF-LF-CRITICAL-PERIOD         VALUE 'C'.
00062                  88  EC-CF-LF-TERM-IN-DAYS            VALUE 'D'.
00063
00064          16  EC-BR-LF-COVERAGE-TYPE    PIC X.
00065                88  EC-BR-REDUCING                   VALUE 'R'.
00066                88  EC-BR-LEVEL                      VALUE 'L' 'P'.
00067
00068          16  EC-BR-AH-COVERAGE-TYPE    PIC X(3).
00069          16  EC-BR-LF-DESC             PIC X(10).
00070          16  EC-BR-AH-DESC             PIC X(10).
00071          16  EC-BR-LF-COMMENT          PIC X(10).
00072          16  EC-BR-AH-COMMENT          PIC X(10).
00073
00074          16  EC-BR-LF-REM-TERM-CALC    PIC X.
00075                88  EC-BR-LF-EARN-AFTER-15TH         VALUE '1'.
00076                88  EC-BR-LF-EARN-ON-HALF-MO         VALUE '2'.
00077                88  EC-BR-LF-EARN-ON-1ST-DAY         VALUE '3'.
00078                88  EC-BR-LF-EARN-ON-FULL-MO         VALUE '4'.
00079
00080          16  EC-BR-AH-REM-TERM-CALC    PIC X.
00081                88  EC-BR-AH-EARN-AFTER-15TH         VALUE '1'.
00082                88  EC-BR-AH-EARN-ON-HALF-MO         VALUE '2'.
00083                88  EC-BR-AH-EARN-ON-1ST-DAY         VALUE '3'.
00084                88  EC-BR-AH-EARN-ON-FULL-MO         VALUE '4'.
00085
00086          16  EC-BR-LF-REFUND-CALC      PIC X.
00087                  88  EC-BR-LF-REFD-BY-R78           VALUE '1'.
00088                  88  EC-BR-LF-REFD-BY-PRO-RATA      VALUE '2'.
00089                  88  EC-BR-LF-REFD-AS-TEXAS         VALUE '4'.
00090                  88  EC-BR-LF-REFD-IS-NET-PAY       VALUE '5'.
00091                  88  EC-BR-LF-REFD-ANTICIPATION     VALUE '6'.
00092                  88  EC-BR-LF-REFD-IS-MEAN          VALUE '8'.
00093                  88  EC-BR-LF-REFD-IS-SUM-OF-DIGIT  VALUE '9'.
00094
00095          16  EC-BR-AH-REFUND-CALC      PIC X.
00096                  88  EC-BR-AH-REFD-BY-R78           VALUE '1'.
00097                  88  EC-BR-AH-REFD-BY-PRO-RATA      VALUE '2'.
00098                  88  EC-BR-AH-REFD-AS-CALIF         VALUE '3'.
00099                  88  EC-BR-AH-REFD-AS-TEXAS         VALUE '4'.
00100                  88  EC-BR-AH-REFD-IS-NET-PAY       VALUE '5'.
00101                  88  EC-BR-AH-REFD-ANTICIPATION     VALUE '6'.
00102                  88  EC-BR-AH-REFD-IS-MEAN          VALUE '8'.
00103                  88  EC-BR-AH-REFD-SUM-OF-DIGIT     VALUE '9'.
00104
00105          16  EC-BR-LF-BEN-I-G-CD       PIC X.
00106                  88  EC-BR-LF-I-G-NOT-USED          VALUE ' '.
00107                  88  EC-BR-LF-I-G-IS-INDV           VALUE 'I'.
00108                  88  EC-BR-LF-I-G-IS-GRP            VALUE 'G'.
00109
00110          16  EC-BR-AH-BEN-I-G-CD       PIC X.
00111                  88  EC-BR-AH-I-G-NOT-USED          VALUE ' '.
00112                  88  EC-BR-AH-I-G-IS-INDV           VALUE 'I'.
00113                  88  EC-BR-AH-I-G-IS-GRP            VALUE 'G'.
00114
00115          16  EC-ST-TOL-PREM           PIC S9(3)V99  COMP-3.
00116          16  EC-ST-TOL-REFUND         PIC S9(3)V99  COMP-3.
00117          16  EC-ST-TOL-PREM-PCT       PIC S9V9(4)   COMP-3.
00118          16  EC-ST-TOL-REFUND-PCT     PIC S9V9(4)   COMP-3.
00119
00120          16  EC-ST-FST-PMT-DAYS-MAX   PIC S9(3)     COMP-3.
00121          16  EC-ST-FST-PMT-DAYS-CHG   PIC X.
00122                  88  EC-ST-EXT-NO-CHG               VALUE ' '.
00123                  88  EC-ST-EXT-CHG-LF               VALUE '1'.
00124                  88  EC-ST-EXT-CHG-AH               VALUE '2'.
00125                  88  EC-ST-EXT-CHG-LF-AH            VALUE '3'.
00126
00127          16  EC-ST-LF-REM-TERM-CALC    PIC X.
00128                88  EC-ST-LF-EARN-AFTER-15TH         VALUE '1'.
00129                88  EC-ST-LF-EARN-ON-HALF-MO         VALUE '2'.
00130                88  EC-ST-LF-EARN-ON-1ST-DAY         VALUE '3'.
00131                88  EC-ST-LF-EARN-ON-FULL-MO         VALUE '4'.
00132                88  EC-ST-LF-EARN-WITH-NO-DAYS       VALUE '5'.
00133
00134          16  EC-ST-AH-REM-TERM-CALC    PIC X.
00135                88  EC-ST-AH-EARN-AFTER-15TH         VALUE '1'.
00136                88  EC-ST-AH-EARN-ON-HALF-MO         VALUE '2'.
00137                88  EC-ST-AH-EARN-ON-1ST-DAY         VALUE '3'.
00138                88  EC-ST-AH-EARN-ON-FULL-MO         VALUE '4'.
00139                88  EC-ST-AH-EARN-WITH-NO-DAYS       VALUE '5'.
00140
00141          16  EC-ST-LF-REFUND-CALC      PIC X.
00142                  88  EC-ST-LF-REFD-BY-R78           VALUE '1'.
00143                  88  EC-ST-LF-REFD-BY-PRO-RATA      VALUE '2'.
00144                  88  EC-ST-LF-REFD-AS-CALIF         VALUE '3'.
00145                  88  EC-ST-LF-REFD-AS-TEXAS         VALUE '4'.
00146                  88  EC-ST-LF-REFD-IS-NET-PAY       VALUE '5'.
00147                  88  EC-ST-LF-REFD-ANTICIPATION     VALUE '6'.
00148                  88  EC-ST-LF-REFD-IS-MEAN          VALUE '8'.
00149                  88  EC-ST-LF-REFD-IS-SUM-OF-DIGIT  VALUE '9'.
00150
00151          16  EC-ST-AH-REFUND-CALC      PIC X.
00152                  88  EC-ST-AH-REFD-BY-R78           VALUE '1'.
00153                  88  EC-ST-AH-REFD-BY-PRO-RATA      VALUE '2'.
00154                  88  EC-ST-AH-REFD-AS-CALIF         VALUE '3'.
00155                  88  EC-ST-AH-REFD-AS-TEXAS         VALUE '4'.
00156                  88  EC-ST-AH-REFD-IS-NET-PAY       VALUE '5'.
00157                  88  EC-ST-AH-REFD-ANTICIPATION     VALUE '6'.
00158                  88  EC-ST-AH-REFD-IS-MEAN          VALUE '8'.
00159                  88  EC-ST-AH-REFD-SUM-OF-DIGIT     VALUE '9'.
00160
00161          16  EC-AM-LF-TOL-PREM         PIC S9(3)V99  COMP-3.
00162          16  EC-AM-AH-TOL-PREM         PIC S9(3)V99  COMP-3.
00163          16  EC-AM-LF-TOL-REFUND       PIC S9(3)V99  COMP-3.
00164          16  EC-AM-AH-TOL-REFUND       PIC S9(3)V99  COMP-3.
00165          16  EC-AM-EXPIRATION-DT       PIC XX.
00166          16  EC-AM-EFFECTIVE-DT        PIC XX.
00167          16  EC-AM-CLASS-CD            PIC XX.
00168          16  EC-AM-LF-DEVIATION        PIC XXX.
00169          16  EC-AM-LF-DEV-PERCENT      PIC S9V9(6)  COMP-3.
00170          16  EC-AM-AH-DEVIATION        PIC XXX.
00171          16  EC-AM-AH-DEV-PERCENT      PIC S9V9(6)  COMP-3.
00172          16  EC-AM-PHONE-NO            PIC X(10).
00173
00174          16  EC-AM-LF-REFUND-CALC      PIC X.
00175                  88  EC-AM-LF-REFD-BY-R78           VALUE '1'.
00176                  88  EC-AM-LF-REFD-BY-PRO-RATA      VALUE '2'.
00177                  88  EC-AM-LF-REFD-AS-TEXAS         VALUE '4'.
00178                  88  EC-AM-LF-REFD-IS-NET-PAY       VALUE '5'.
00179                  88  EC-AM-LF-REFD-ANTICIPATION     VALUE '6'.
00180                  88  EC-AM-LF-REFD-IS-MEAN          VALUE '8'.
00181                  88  EC-AM-LF-REFD-IS-SUM-OF-DIGIT  VALUE '9'.
00182
00183          16  EC-AM-AH-REFUND-CALC      PIC X.
00184                  88  EC-AM-AH-REFD-BY-R78           VALUE '1'.
00185                  88  EC-AM-AH-REFD-BY-PRO-RATA      VALUE '2'.
00186                  88  EC-AM-AH-REFD-AS-CALIF         VALUE '3'.
00187                  88  EC-AM-AH-REFD-IS-NET           VALUE '5'.
00188                  88  EC-AM-AH-REFD-ANTICIPATION     VALUE '6'.
00189                  88  EC-AM-AH-REFD-IS-MEAN          VALUE '8'.
00190                  88  EC-AM-AH-REFD-SUM-OF-DIGIT     VALUE '9'.
00191
00192          16  EC-AM-BEN-I-G-CD          PIC X.
00193                  88  WK-AM-I-G-NOT-USED             VALUE ' '.
00194                  88  EC-AM-I-G-IS-INDV              VALUE 'I'.
00195                  88  EC-AM-I-G-IS-GRP               VALUE 'G'.
00196
00197          16  EC-AM-LF-MAX-ATT-AGE      PIC S9(3)       COMP-3.
00198          16  EC-AM-LF-MAX-AGE          PIC S9(3)       COMP-3.
00199          16  EC-AM-LF-MAX-TERM         PIC S9(3)       COMP-3.
00200          16  EC-AM-LF-MAX-TOT-BEN      PIC S9(7)V99    COMP-3.
00201          16  EC-AM-AH-MAX-ATT-AGE      PIC S9(3)       COMP-3.
00202          16  EC-AM-AH-MAX-AGE          PIC S9(3)       COMP-3.
00203          16  EC-AM-AH-MAX-TERM         PIC S9(3)       COMP-3.
00204          16  EC-AM-AH-MAX-MON-BEN      PIC S9(7)V99    COMP-3.
00205          16  EC-AM-AH-MAX-TOT-BEN      PIC S9(7)V99    COMP-3.
00206
00207          16  EC-RT-LF-MAX-ATT-AGE      PIC S9(3)       COMP-3.
00208          16  EC-RT-LF-MAX-AGE          PIC S9(3)       COMP-3.
00209          16  EC-RT-LF-MAX-TERM         PIC S9(3)       COMP-3.
00210          16  EC-RT-LF-MAX-TOT-BEN      PIC S9(7)V99    COMP-3.
00211          16  EC-RT-AH-MAX-ATT-AGE      PIC S9(3)       COMP-3.
00212          16  EC-RT-AH-MAX-AGE          PIC S9(3)       COMP-3.
00213          16  EC-RT-AH-MAX-TERM         PIC S9(3)       COMP-3.
00214          16  EC-RT-AH-MAX-MON-BEN      PIC S9(7)V99    COMP-3.
00215          16  EC-RT-AH-MAX-TOT-BEN      PIC S9(7)V99    COMP-3.
00216          16  EC-RT-AH-RATE             PIC S9(3)V9(5)  COMP-3.
00217          16  EC-RT-LF-RATE             PIC S9(3)V9(5)  COMP-3.
00218          16  EC-RT-LF-NSP-ST           PIC XX.
00219          16  EC-RT-AH-NSP-ST           PIC XX.
00220          16  EC-RT-LF-NSP-RATE         PIC S9(3)V9(5)  COMP-3.
00221          16  EC-RT-AH-NSP-RATE         PIC S9(3)V9(5)  COMP-3.
00222
00223          16  EC-CM-LF-PRIOR-STATUS     PIC X.
00224              88  EC-CM-LF-POLICY-IS-ACTIVE     VALUE '1' '3' '4'
00225                                              'M' '5' '9' '2'.
00226              88  EC-CM-LF-NORMAL-ENTRY         VALUE '1'.
00227              88  EC-CM-LF-POLICY-PENDING       VALUE '2'.
00228              88  EC-CM-LF-POLICY-IS-RESTORE    VALUE '3'.
00229              88  EC-CM-LF-CONVERSION-ENTRY     VALUE '4'.
00230              88  EC-CM-LF-POLICY-IS-REISSUE    VALUE '5'.
122002             88  EC-CM-LF-POLICY-IS-MONTHLY    VALUE 'M'.
00231              88  EC-CM-LF-LUMP-SUM-DISAB       VALUE '6'.
00232              88  EC-CM-LF-DEATH-CLAIM-APPLIED  VALUE '7'.
00233              88  EC-CM-LF-CANCEL-APPLIED       VALUE '8'.
00234              88  EC-CM-LF-IS-REIN-ONLY         VALUE '9'.
00235
00236          16  EC-CM-AH-PRIOR-STATUS     PIC X.
00237              88  EC-CM-AH-POLICY-IS-ACTIVE     VALUE '1' '3' '4'
00238                                              'M' '5' '9' '2'.
00239              88  EC-CM-AH-NORMAL-ENTRY         VALUE '1'.
00240              88  EC-CM-AH-POLICY-PENDING       VALUE '2'.
00241              88  EC-CM-AH-POLICY-IS-RESTORE    VALUE '3'.
00242              88  EC-CM-AH-CONVERSION-ENTRY     VALUE '4'.
00243              88  EC-CM-AH-POLICY-IS-REISSUE    VALUE '5'.
122002             88  EC-CM-AH-POLICY-IS-MONTHLY    VALUE 'M'.
00244              88  EC-CM-AH-LUMP-SUM-DISAB       VALUE '6'.
00245              88  EC-CM-AH-DEATH-CLAIM-APPLIED  VALUE '7'.
00246              88  EC-CM-AH-CANCEL-APPLIED       VALUE '8'.
00247              88  EC-CM-AH-IS-REIN-ONLY         VALUE '9'.
00248
00249          16  EC-CM-LF-PRIOR-REFUND     PIC S9(7)V99     COMP-3.
00250          16  EC-CM-AH-PRIOR-REFUND     PIC S9(7)V99     COMP-3.
00251          16  EC-CM-LF-CANCEL-DT        PIC XX.
00252          16  EC-CM-AH-CANCEL-DT        PIC XX.
00253          16  EC-CM-DEATH-DT            PIC XX.
00254
00255 *DMD CUSTOM CODING FIELDS BELOW
00256 *POPULATED BY CF-RATING-SWITCH AT EL050 TIME
00257          16  EC-BR-DMD-RATING-SW       PIC X.
00258                  88  EC-DMD-AH-RATING            VALUE 'Y'.
00259                  88  EC-DMD-LF-RATING            VALUE 'Y'.
00260                  88  NO-DMD-RATING               VALUE 'N'.
00261
00262          16  EC-NEW-CERT               PIC X(11).
00263          16  EC-NEW-STATE              PIC XX.
00264          16  FILLER                    PIC X.
00265 *FIELDS FOR THE REFUND OVER AND SHORT TOLERANCES.
00266          16  OVER-SHORT-EDIT-FIELDS.
00267              20  EC-CO-OVR-SHT-AMT     PIC S999V99   COMP-3.
00268              20  EC-CO-OVR-SHT-PCT     PIC S9V9(4)   COMP-3.
00269              20  EC-ST-OVR-SHT-AMT     PIC S999V99   COMP-3.
00270              20  EC-ST-OVR-SHT-PCT     PIC S9V9(4)   COMP-3.
00271              20  EC-AM-TOL-REF-PCT     PIC S9V9(4)   COMP-3.
00272              20  EC-AM-LF-OVR-SHT-AMT  PIC S999V99   COMP-3.
00273              20  EC-AM-LF-OVR-SHT-PCT  PIC S9V9(4)   COMP-3.
00274              20  EC-AM-AH-OVR-SHT-AMT  PIC S999V99   COMP-3.
00275              20  EC-AM-AH-OVR-SHT-PCT  PIC S9V9(4)   COMP-3.
00276          16  FILLER                    PIC X(71).
00260
00261      EJECT
041320*                                COPY ELCJPFX.
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
041320                                 PIC X(825).
00262 *    COPY ELCAID.
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
00263  01  FILLER    REDEFINES DFHAID.
00264      12  FILLER              PIC X(8).
00265      12  PF-VALUES           PIC X       OCCURS 2.
00266
00267      EJECT
00268 *    COPY EL6313S.
       01  EL631FI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  DATEL PIC S9(0004) COMP.
           05  DATEF PIC  X(0001).
           05  FILLER REDEFINES DATEF.
               10  DATEA PIC  X(0001).
           05  DATEI PIC  X(0008).
      *    -------------------------------
           05  TIMEL PIC S9(0004) COMP.
           05  TIMEF PIC  X(0001).
           05  FILLER REDEFINES TIMEF.
               10  TIMEA PIC  X(0001).
           05  TIMEI PIC  X(0005).
      *    -------------------------------
           05  HEADL PIC S9(0004) COMP.
           05  HEADF PIC  X(0001).
           05  FILLER REDEFINES HEADF.
               10  HEADA PIC  X(0001).
           05  HEADI PIC  X(0038).
      *    -------------------------------
           05  COMPIDL PIC S9(0004) COMP.
           05  COMPIDF PIC  X(0001).
           05  FILLER REDEFINES COMPIDF.
               10  COMPIDA PIC  X(0001).
           05  COMPIDI PIC  X(0003).
      *    -------------------------------
           05  SYSENVL PIC S9(0004) COMP.
           05  SYSENVF PIC  X(0001).
           05  FILLER REDEFINES SYSENVF.
               10  SYSENVA PIC  X(0001).
           05  SYSENVI PIC  X(0006).
      *    -------------------------------
           05  PROCIDL PIC S9(0004) COMP.
           05  PROCIDF PIC  X(0001).
           05  FILLER REDEFINES PROCIDF.
               10  PROCIDA PIC  X(0001).
           05  PROCIDI PIC  X(0004).
      *    -------------------------------
           05  MAINTL PIC S9(0004) COMP.
           05  MAINTF PIC  X(0001).
           05  FILLER REDEFINES MAINTF.
               10  MAINTA PIC  X(0001).
           05  MAINTI PIC  X(0001).
      *    -------------------------------
           05  ENTERBYL PIC S9(0004) COMP.
           05  ENTERBYF PIC  X(0001).
           05  FILLER REDEFINES ENTERBYF.
               10  ENTERBYA PIC  X(0001).
           05  ENTERBYI PIC  X(0004).
      *    -------------------------------
           05  MAINTBYL PIC S9(0004) COMP.
           05  MAINTBYF PIC  X(0001).
           05  FILLER REDEFINES MAINTBYF.
               10  MAINTBYA PIC  X(0001).
           05  MAINTBYI PIC  X(0004).
      *    -------------------------------
           05  ENTERDTL PIC S9(0004) COMP.
           05  ENTERDTF PIC  X(0001).
           05  FILLER REDEFINES ENTERDTF.
               10  ENTERDTA PIC  X(0001).
           05  ENTERDTI PIC  X(0014).
      *    -------------------------------
           05  MAINTDTL PIC S9(0004) COMP.
           05  MAINTDTF PIC  X(0001).
           05  FILLER REDEFINES MAINTDTF.
               10  MAINTDTA PIC  X(0001).
           05  MAINTDTI PIC  X(0008).
      *    -------------------------------
           05  MAINTATL PIC S9(0004) COMP.
           05  MAINTATF PIC  X(0001).
           05  FILLER REDEFINES MAINTATF.
               10  MAINTATA PIC  X(0001).
           05  MAINTATI PIC  X(0005).
      *    -------------------------------
           05  BATCHL PIC S9(0004) COMP.
           05  BATCHF PIC  X(0001).
           05  FILLER REDEFINES BATCHF.
               10  BATCHA PIC  X(0001).
           05  BATCHI PIC  X(0006).
      *    -------------------------------
           05  SEQL PIC S9(0004) COMP.
           05  SEQF PIC  X(0001).
           05  FILLER REDEFINES SEQF.
               10  SEQA PIC  X(0001).
           05  SEQI PIC  X(0004).
      *    -------------------------------
           05  MOENDDTL PIC S9(0004) COMP.
           05  MOENDDTF PIC  X(0001).
           05  FILLER REDEFINES MOENDDTF.
               10  MOENDDTA PIC  X(0001).
           05  MOENDDTI PIC  X(0008).
      *    -------------------------------
           05  CARRL PIC S9(0004) COMP.
           05  CARRF PIC  X(0001).
           05  FILLER REDEFINES CARRF.
               10  CARRA PIC  X(0001).
           05  CARRI PIC  X(0001).
      *    -------------------------------
           05  GROUPL PIC S9(0004) COMP.
           05  GROUPF PIC  X(0001).
           05  FILLER REDEFINES GROUPF.
               10  GROUPA PIC  X(0001).
           05  GROUPI PIC  X(0006).
      *    -------------------------------
           05  STATEL PIC S9(0004) COMP.
           05  STATEF PIC  X(0001).
           05  FILLER REDEFINES STATEF.
               10  STATEA PIC  X(0001).
           05  STATEI PIC  X(0002).
      *    -------------------------------
           05  ACCTL PIC S9(0004) COMP.
           05  ACCTF PIC  X(0001).
           05  FILLER REDEFINES ACCTF.
               10  ACCTA PIC  X(0001).
           05  ACCTI PIC  X(0010).
      *    -------------------------------
           05  CERTL PIC S9(0004) COMP.
           05  CERTF PIC  X(0001).
           05  FILLER REDEFINES CERTF.
               10  CERTA PIC  X(0001).
           05  CERTI PIC  X(0010).
      *    -------------------------------
           05  SUFIXL PIC S9(0004) COMP.
           05  SUFIXF PIC  X(0001).
           05  FILLER REDEFINES SUFIXF.
               10  SUFIXA PIC  X(0001).
           05  SUFIXI PIC  X(0001).
      *    -------------------------------
           05  EFFDTL PIC S9(0004) COMP.
           05  EFFDTF PIC  X(0001).
           05  FILLER REDEFINES EFFDTF.
               10  EFFDTA PIC  X(0001).
           05  EFFDTI PIC  X(0008).
      *    -------------------------------
           05  LASTNML PIC S9(0004) COMP.
           05  LASTNMF PIC  X(0001).
           05  FILLER REDEFINES LASTNMF.
               10  LASTNMA PIC  X(0001).
           05  LASTNMI PIC  X(0015).
      *    -------------------------------
           05  FIRSTNML PIC S9(0004) COMP.
           05  FIRSTNMF PIC  X(0001).
           05  FILLER REDEFINES FIRSTNMF.
               10  FIRSTNMA PIC  X(0001).
           05  FIRSTNMI PIC  X(0010).
      *    -------------------------------
           05  INITL PIC S9(0004) COMP.
           05  INITF PIC  X(0001).
           05  FILLER REDEFINES INITF.
               10  INITA PIC  X(0001).
           05  INITI PIC  X(0001).
      *    -------------------------------
           05  SOCSECL PIC S9(0004) COMP.
           05  SOCSECF PIC  X(0001).
           05  FILLER REDEFINES SOCSECF.
               10  SOCSECA PIC  X(0001).
           05  SOCSECI PIC  X(0011).
      *    -------------------------------
           05  CBNAMEL PIC S9(0004) COMP.
           05  CBNAMEF PIC  X(0001).
           05  FILLER REDEFINES CBNAMEF.
               10  CBNAMEA PIC  X(0001).
           05  CBNAMEI PIC  X(0025).
      *    -------------------------------
           05  CBADDRL PIC S9(0004) COMP.
           05  CBADDRF PIC  X(0001).
           05  FILLER REDEFINES CBADDRF.
               10  CBADDRA PIC  X(0001).
           05  CBADDRI PIC  X(0030).
      *    -------------------------------
           05  CBCITYL PIC S9(0004) COMP.
           05  CBCITYF PIC  X(0001).
           05  FILLER REDEFINES CBCITYF.
               10  CBCITYA PIC  X(0001).
           05  CBCITYI PIC  X(0028).
      *    -------------------------------
           05  CBSTATEL PIC S9(0004) COMP.
           05  CBSTATEF PIC  X(0001).
           05  FILLER REDEFINES CBSTATEF.
               10  CBSTATEA PIC  X(0001).
           05  CBSTATEI PIC  X(0002).
      *    -------------------------------
           05  CBZIPL PIC S9(0004) COMP.
           05  CBZIPF PIC  X(0001).
           05  FILLER REDEFINES CBZIPF.
               10  CBZIPA PIC  X(0001).
           05  CBZIPI PIC  X(0010).
      *    -------------------------------
           05  ADDRS1L PIC S9(0004) COMP.
           05  ADDRS1F PIC  X(0001).
           05  FILLER REDEFINES ADDRS1F.
               10  ADDRS1A PIC  X(0001).
           05  ADDRS1I PIC  X(0030).
      *    -------------------------------
           05  ADDRS2L PIC S9(0004) COMP.
           05  ADDRS2F PIC  X(0001).
           05  FILLER REDEFINES ADDRS2F.
               10  ADDRS2A PIC  X(0001).
           05  ADDRS2I PIC  X(0029).
      *    -------------------------------
           05  ICITYL PIC S9(0004) COMP.
           05  ICITYF PIC  X(0001).
           05  FILLER REDEFINES ICITYF.
               10  ICITYA PIC  X(0001).
           05  ICITYI PIC  X(0028).
      *    -------------------------------
           05  ISTATEL PIC S9(0004) COMP.
           05  ISTATEF PIC  X(0001).
           05  FILLER REDEFINES ISTATEF.
               10  ISTATEA PIC  X(0001).
           05  ISTATEI PIC  X(0002).
      *    -------------------------------
           05  ZIPCDEL PIC S9(0004) COMP.
           05  ZIPCDEF PIC  X(0001).
           05  FILLER REDEFINES ZIPCDEF.
               10  ZIPCDEA PIC  X(0001).
           05  ZIPCDEI PIC  X(0010).
      *    -------------------------------
           05  PHONEL PIC S9(0004) COMP.
           05  PHONEF PIC  X(0001).
           05  FILLER REDEFINES PHONEF.
               10  PHONEA PIC  X(0001).
           05  PHONEI PIC  X(0012).
      *    -------------------------------
           05  JAGEDEFL PIC S9(0004) COMP.
           05  JAGEDEFF PIC  X(0001).
           05  FILLER REDEFINES JAGEDEFF.
               10  JAGEDEFA PIC  X(0001).
           05  JAGEDEFI PIC  X(0004).
      *    -------------------------------
           05  JNT1STL PIC S9(0004) COMP.
           05  JNT1STF PIC  X(0001).
           05  FILLER REDEFINES JNT1STF.
               10  JNT1STA PIC  X(0001).
           05  JNT1STI PIC  X(0010).
      *    -------------------------------
           05  JNTINITL PIC S9(0004) COMP.
           05  JNTINITF PIC  X(0001).
           05  FILLER REDEFINES JNTINITF.
               10  JNTINITA PIC  X(0001).
           05  JNTINITI PIC  X(0001).
      *    -------------------------------
           05  JNTLSTL PIC S9(0004) COMP.
           05  JNTLSTF PIC  X(0001).
           05  FILLER REDEFINES JNTLSTF.
               10  JNTLSTA PIC  X(0001).
           05  JNTLSTI PIC  X(0015).
      *    -------------------------------
           05  JNTAGEL PIC S9(0004) COMP.
           05  JNTAGEF PIC  X(0001).
           05  FILLER REDEFINES JNTAGEF.
               10  JNTAGEA PIC  X(0001).
           05  JNTAGEI PIC  9(2).
      *    -------------------------------
           05  BENFARYL PIC S9(0004) COMP.
           05  BENFARYF PIC  X(0001).
           05  FILLER REDEFINES BENFARYF.
               10  BENFARYA PIC  X(0001).
           05  BENFARYI PIC  X(0025).
      *    -------------------------------
           05  LIT2L PIC S9(0004) COMP.
           05  LIT2F PIC  X(0001).
           05  FILLER REDEFINES LIT2F.
               10  LIT2A PIC  X(0001).
           05  LIT2I PIC  X(0038).
      *    -------------------------------
           05  POLFRML PIC S9(0004) COMP.
           05  POLFRMF PIC  X(0001).
           05  FILLER REDEFINES POLFRMF.
               10  POLFRMA PIC  X(0001).
           05  POLFRMI PIC  X(0012).
      *    -------------------------------
           05  JNTDOBL PIC S9(0004) COMP.
           05  JNTDOBF PIC  X(0001).
           05  FILLER REDEFINES JNTDOBF.
               10  JNTDOBA PIC  X(0001).
           05  JNTDOBI PIC  X(0008).
      *    -------------------------------
           05  MUNITXL PIC S9(0004) COMP.
           05  MUNITXF PIC  X(0001).
           05  FILLER REDEFINES MUNITXF.
               10  MUNITXA PIC  X(0001).
           05  MUNITXI PIC  X(0012).
      *    -------------------------------
           05  RATEL PIC S9(0004) COMP.
           05  RATEF PIC  X(0001).
           05  FILLER REDEFINES RATEF.
               10  RATEA PIC  X(0001).
           05  RATEI PIC  999V99999.
      *    -------------------------------
           05  PFRATEL PIC S9(0004) COMP.
           05  PFRATEF PIC  X(0001).
           05  FILLER REDEFINES PFRATEF.
               10  PFRATEA PIC  X(0001).
           05  PFRATEI PIC  9999V9999.
      *    -------------------------------
           05  RTCODEL PIC S9(0004) COMP.
           05  RTCODEF PIC  X(0001).
           05  FILLER REDEFINES RTCODEF.
               10  RTCODEA PIC  X(0001).
           05  RTCODEI PIC  X(0004).
      *    -------------------------------
           05  RESSTL PIC S9(0004) COMP.
           05  RESSTF PIC  X(0001).
           05  FILLER REDEFINES RESSTF.
               10  RESSTA PIC  X(0001).
           05  RESSTI PIC  X(0002).
      *    -------------------------------
           05  LFRTHDGL PIC S9(0004) COMP.
           05  LFRTHDGF PIC  X(0001).
           05  FILLER REDEFINES LFRTHDGF.
               10  LFRTHDGA PIC  X(0001).
           05  LFRTHDGI PIC  X(0012).
      *    -------------------------------
           05  LFDVHDGL PIC S9(0004) COMP.
           05  LFDVHDGF PIC  X(0001).
           05  FILLER REDEFINES LFDVHDGF.
               10  LFDVHDGA PIC  X(0001).
           05  LFDVHDGI PIC  X(0014).
      *    -------------------------------
           05  AHRTHDGL PIC S9(0004) COMP.
           05  AHRTHDGF PIC  X(0001).
           05  FILLER REDEFINES AHRTHDGF.
               10  AHRTHDGA PIC  X(0001).
           05  AHRTHDGI PIC  X(0012).
      *    -------------------------------
           05  AHDVHDGL PIC S9(0004) COMP.
           05  AHDVHDGF PIC  X(0001).
           05  FILLER REDEFINES AHDVHDGF.
               10  AHDVHDGA PIC  X(0001).
           05  AHDVHDGI PIC  X(0014).
      *    -------------------------------
           05  REINTBLL PIC S9(0004) COMP.
           05  REINTBLF PIC  X(0001).
           05  FILLER REDEFINES REINTBLF.
               10  REINTBLA PIC  X(0001).
           05  REINTBLI PIC  X(0003).
      *    -------------------------------
           05  LFRTDEVL PIC S9(0004) COMP.
           05  LFRTDEVF PIC  X(0001).
           05  FILLER REDEFINES LFRTDEVF.
               10  LFRTDEVA PIC  X(0001).
           05  LFRTDEVI PIC  X(0003).
      *    -------------------------------
           05  LFRTPRCL PIC S9(0004) COMP.
           05  LFRTPRCF PIC  X(0001).
           05  FILLER REDEFINES LFRTPRCF.
               10  LFRTPRCA PIC  X(0001).
           05  LFRTPRCI PIC  99V999999.
      *    -------------------------------
           05  AHRTDEVL PIC S9(0004) COMP.
           05  AHRTDEVF PIC  X(0001).
           05  FILLER REDEFINES AHRTDEVF.
               10  AHRTDEVA PIC  X(0001).
           05  AHRTDEVI PIC  X(0003).
      *    -------------------------------
           05  AHRTPRCL PIC S9(0004) COMP.
           05  AHRTPRCF PIC  X(0001).
           05  FILLER REDEFINES AHRTPRCF.
               10  AHRTPRCA PIC  X(0001).
           05  AHRTPRCI PIC  99V999999.
      *    -------------------------------
           05  LFCMHDGL PIC S9(0004) COMP.
           05  LFCMHDGF PIC  X(0001).
           05  FILLER REDEFINES LFCMHDGF.
               10  LFCMHDGA PIC  X(0001).
           05  LFCMHDGI PIC  X(0008).
      *    -------------------------------
           05  AHCMHDGL PIC S9(0004) COMP.
           05  AHCMHDGF PIC  X(0001).
           05  FILLER REDEFINES AHCMHDGF.
               10  AHCMHDGA PIC  X(0001).
           05  AHCMHDGI PIC  X(0008).
      *    -------------------------------
           05  LFCOMMSL PIC S9(0004) COMP.
           05  LFCOMMSF PIC  X(0001).
           05  FILLER REDEFINES LFCOMMSF.
               10  LFCOMMSA PIC  X(0001).
           05  LFCOMMSI PIC  9V99999.
      *    -------------------------------
           05  AHCOMMSL PIC S9(0004) COMP.
           05  AHCOMMSF PIC  X(0001).
           05  FILLER REDEFINES AHCOMMSF.
               10  AHCOMMSA PIC  X(0001).
           05  AHCOMMSI PIC  9V99999.
      *    -------------------------------
           05  AEFFDTL PIC S9(0004) COMP.
           05  AEFFDTF PIC  X(0001).
           05  FILLER REDEFINES AEFFDTF.
               10  AEFFDTA PIC  X(0001).
           05  AEFFDTI PIC  X(0008).
      *    -------------------------------
           05  AEXPDTL PIC S9(0004) COMP.
           05  AEXPDTF PIC  X(0001).
           05  FILLER REDEFINES AEXPDTF.
               10  AEXPDTA PIC  X(0001).
           05  AEXPDTI PIC  X(0008).
      *    -------------------------------
           05  ERMSG1L PIC S9(0004) COMP.
           05  ERMSG1F PIC  X(0001).
           05  FILLER REDEFINES ERMSG1F.
               10  ERMSG1A PIC  X(0001).
           05  ERMSG1I PIC  X(0078).
      *    -------------------------------
           05  ERMSG2L PIC S9(0004) COMP.
           05  ERMSG2F PIC  X(0001).
           05  FILLER REDEFINES ERMSG2F.
               10  ERMSG2A PIC  X(0001).
           05  ERMSG2I PIC  X(0078).
      *    -------------------------------
           05  FPFENTRL PIC S9(0004) COMP.
           05  FPFENTRF PIC  X(0001).
           05  FILLER REDEFINES FPFENTRF.
               10  FPFENTRA PIC  X(0001).
           05  FPFENTRI PIC  9(2).
       01  EL631FO REDEFINES EL631FI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HEADO PIC  X(0038).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMPIDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SYSENVO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PROCIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ENTERBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ENTERDTO PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTATO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BATCHO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SEQO PIC  9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MOENDDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GROUPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SUFIXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LASTNMO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FIRSTNMO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INITO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SOCSECO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CBNAMEO PIC  X(0025).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CBADDRO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CBCITYO PIC  X(0028).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CBSTATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CBZIPO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADDRS1O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADDRS2O PIC  X(0029).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ICITYO PIC  X(0028).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ISTATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ZIPCDEO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PHONEO PIC  999B999B9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JAGEDEFO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNT1STO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTINITO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTLSTO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTAGEO PIC  9(2).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENFARYO PIC  X(0025).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LIT2O PIC  X(0038).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  POLFRMO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTDOBO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MUNITXO PIC  Z,ZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RATEO PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFRATEO PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RTCODEO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RESSTO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFRTHDGO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFDVHDGO PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHRTHDGO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHDVHDGO PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REINTBLO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFRTDEVO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFRTPRCO PIC  Z.999999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHRTDEVO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHRTPRCO PIC  Z.999999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFCMHDGO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHCMHDGO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFCOMMSO PIC  .99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHCOMMSO PIC  .99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEFFDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEXPDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERMSG1O PIC  X(0078).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERMSG2O PIC  X(0078).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FPFENTRO PIC  X(0002).
      *    -------------------------------
00269      EJECT
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
00271  01  DFHCOMMAREA             PIC X(1300).
00272
00273      EJECT
00274 *        COPY ERCPNDB.
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
012220* 012220  CR2018092700002  TANA ADD LETTER REQUIRED FIELD
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
012220         16  PB-I-LETTER-REQD             PIC X.
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
00275      EJECT
00276 *        COPY ERCPNDM.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCPNDM                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = PENDING MAILING DATA                      *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
CIDMOD*   RECORD SIZE = 374   RECFORM = FIX                            *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ERPNDM                 RKP=2,LEN=11      *
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
071108* 071108  CR2008040800002  PEMA  ADD CRED BENE INFORMATION
090408* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
100217* 100217  CR2016091600001  PEMA  ADD EDIT FOR ZIP CODE
00017 ******************************************************************
00018
00019  01  PENDING-MAILING-DATA.
00020      12  PM-RECORD-ID                      PIC XX.
00021          88  VALID-MA-ID                       VALUE 'PM'.
00022
00023      12  PM-CONTROL-PRIMARY.
00024          16  PM-COMPANY-CD                 PIC X.
00025          16  PM-ENTRY-BATCH                PIC X(6).
00026          16  PM-BATCH-SEQ-NO               PIC S9(4)     COMP.
00027          16  PM-BATCH-CHG-SEQ-NO           PIC S9(4)     COMP.
00028
00029      12  FILLER                            PIC X(14).
00030
00031      12  PM-ACCESS-CONTROL.
00032          16  PM-SOURCE-SYSTEM              PIC XX.
00033              88  PM-FROM-CREDIT                VALUE 'CR'.
00034              88  PM-FROM-VSI                   VALUE 'VS'.
00035              88  PM-FROM-WARRANTY              VALUE 'WA'.
00036              88  PM-FROM-OTHER                 VALUE 'OT'.
00037          16  PM-RECORD-ADD-DT              PIC XX.
00038          16  PM-RECORD-ADDED-BY            PIC XXXX.
00039          16  PM-LAST-MAINT-DT              PIC XX.
00040          16  PM-LAST-MAINT-BY              PIC XXXX.
00041          16  PM-LAST-MAINT-HHMMSS          PIC S9(6)       COMP-3.
00042
00043      12  PM-PROFILE-INFO.
00044          16  PM-QUALIFY-CODE-1             PIC XX.
00045          16  PM-QUALIFY-CODE-2             PIC XX.
00046          16  PM-QUALIFY-CODE-3             PIC XX.
00047          16  PM-QUALIFY-CODE-4             PIC XX.
00048          16  PM-QUALIFY-CODE-5             PIC XX.
00049
00050          16  PM-INSURED-LAST-NAME          PIC X(15).
00051          16  PM-INSURED-FIRST-NAME         PIC X(10).
00052          16  PM-INSURED-MIDDLE-INIT        PIC X.
00053          16  PM-INSURED-ISSUE-AGE          PIC 99.
00054          16  PM-INSURED-BIRTH-DT           PIC XX.
00055          16  PM-INSURED-SEX                PIC X.
00056              88  PM-SEX-MALE                   VALUE 'M'.
00057              88  PM-SEX-FEMALE                 VALUE 'F'.
00058          16  PM-INSURED-SOC-SEC-NO         PIC X(11).
00059
080406         16  PM-ADDRESS-CORRECTED          PIC X.
081108         16  PM-JOINT-BIRTH-DT             PIC XX.
00060 *        16  FILLER                        PIC X(12).
00061
00062          16  PM-ADDRESS-LINE-1             PIC X(30).
00063          16  PM-ADDRESS-LINE-2             PIC X(30).
00064          16  PM-CITY-STATE.
                   20  PM-CITY                   PIC X(28).
                   20  PM-STATE                  PIC XX.
00065          16  PM-ZIP.
00066              20  PM-ZIP-CODE.
00067                  24  PM-ZIP-1              PIC X.
00068                      88  PM-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00069                  24  FILLER                PIC X(4).
00070              20  PM-ZIP-PLUS4              PIC X(4).
00071          16  PM-CANADIAN-ZIP  REDEFINES  PM-ZIP.
00072              20  PM-CAN-POST1              PIC XXX.
00073              20  PM-CAN-POST2              PIC XXX.
00074              20  FILLER                    PIC XXX.
00075
00076          16  PM-PHONE-NO                   PIC 9(11)       COMP-3.
100217         16  pm-city-st-zip-verified       pic x.
100217         16  FILLER                        PIC XX.
00079
           12  PM-CRED-BENE-INFO.
CIDMOD         16  PM-CRED-BENE-NAME             PIC X(25).
CIDMOD         16  PM-CRED-BENE-ADDR             PIC X(30).
071108         16  PM-CRED-BENE-ADDR2            PIC X(30).
CIDMOD         16  PM-CRED-BENE-CTYST.
                   20  PM-CRED-BENE-CITY         PIC X(28).
                   20  PM-CRED-BENE-STATE        PIC XX.
CIDMOD         16  PM-CRED-BENE-ZIP.
CIDMOD             20  PM-CB-ZIP-CODE.
CIDMOD                 24  PM-CB-ZIP-1           PIC X.
CIDMOD                     88  PM-CB-CANADIAN-POST-CODE
                                        VALUE 'A' THRU 'Z'.
CIDMOD                 24  FILLER                PIC X(4).
CIDMOD             20  PM-CB-ZIP-PLUS4           PIC X(4).
CIDMOD         16  PM-CB-CANADIAN-ZIP  REDEFINES  PM-CRED-BENE-ZIP.
CIDMOD             20  PM-CB-CAN-POST1           PIC XXX.
CIDMOD             20  PM-CB-CAN-POST2           PIC XXX.
CIDMOD             20  FILLER                    PIC XXX.
080406     12  PM-POST-CARD-MAIL-DATA.
080406         16  PM-MAIL-DATA OCCURS 7.
080406             20  PM-MAIL-TYPE              PIC X.
080406                 88  PM-12MO-MAILING           VALUE '1'.
080406                 88  PM-EXP-MAILING            VALUE '2'.
080406             20  PM-MAIL-STATUS            PIC X.
080406                 88  PM-MAIL-ST-MAILED         VALUE '1'.
080406                 88  PM-MAIL-ST-RETURNED       VALUE '2'.
080406                 88  PM-MAIL-ST-NOT-MAILED     VALUE '3'.
080406             20  PM-MAIL-DATE              PIC XX.
080406     12  FILLER                            PIC XX.
           12  FILLER                            PIC X(12).
080406*    12  FILLER                            PIC X(30).
00075
00081 ******************************************************************
00277      EJECT
00278 *        COPY ELCCERT.
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
00279 *        COPY ELCCNTL.
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
121712
121712*        COPY ELCCRTT.
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
100217 01  var  pic x(30).
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA PENDING-BUSINESS
                                PENDING-MAILING-DATA
                                CERTIFICATE-MASTER CONTROL-FILE
                                CERTIFICATE-TRAILERS VAR.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL6313' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00282
00283      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
00284      MOVE PI-LIFE-OVERRIDE-L6    TO EMI-LIFE-OVERRIDE-L6.
00285      MOVE PI-AH-OVERRIDE-L6      TO EMI-AH-OVERRIDE-L6.
00286
00287      MOVE +2                     TO EMI-NUMBER-OF-LINES.
00288      MOVE 2                      TO EMI-SWITCH2.
100217     set P to address of KIXSYS
100217     CALL "getenv" using by value P returning var-ptr
100217     if var-ptr = null then
100217        display ' kixsys not set '
100217     else
100217        set address of var to var-ptr
100217        move 0 to env-var-len
100217        inspect var tallying env-var-len
100217          for characters before X'00'
100217        unstring var (1:env-var-len) delimited by '/'
100217           into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
100217              WS-KIX-SYS
100217        end-unstring
100217     end-if
100217     perform varying a1 from +1 by +1 until a1 > +10
100217        if ws-kix-myenv (a1:1) = low-values or high-values
100217           display ' found low or hi val '
100217           move spaces to ws-kix-myenv (a1:1)
100217        end-if
100217     end-perform
00289
00290      IF EIBCALEN = 0
00291          GO TO 8800-UNAUTHORIZED-ACCESS.
00292
00293      IF PI-LIFE-OVERRIDE-L2 GREATER SPACES
00294          MOVE PI-LIFE-OVERRIDE-L2 TO WS-LFRT-OVERRIDE-L2
00295          MOVE PI-LIFE-OVERRIDE-L2 TO WS-LFCM-OVERRIDE-L2
00296          MOVE PI-LIFE-OVERRIDE-L2 TO WS-LFDV-OVERRIDE-L2.
00297
00298      IF PI-AH-OVERRIDE-L2 GREATER SPACES
00299          MOVE PI-AH-OVERRIDE-L2  TO WS-AHRT-OVERRIDE-L2
00300          MOVE PI-AH-OVERRIDE-L2  TO WS-AHCM-OVERRIDE-L2
00301          MOVE PI-AH-OVERRIDE-L2  TO WS-AHDV-OVERRIDE-L2.
00302
00303      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00304      MOVE '5'                    TO DC-OPTION-CODE.
00305      PERFORM 9700-DATE-LINK.
00306      MOVE DC-BIN-DATE-1          TO WS-CURRENT-BIN-DT.
00307      MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DT.
00308
00309      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00310          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00311              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00312              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00313              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00314              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00315              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00316              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00317              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00318              MOVE THIS-PGM             TO PI-CALLING-PROGRAM
00319          ELSE
00320              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00321              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00322              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00323              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00324              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00325              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00326              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00327              MOVE SPACES               TO PI-SAVED-PROGRAM-6.
00328
00329      MOVE PI-PREV-CONTROL-PRIMARY TO ERPNDB-KEY.
00330
00331      IF EIBTRNID NOT = TRANS-EXB3
00332          MOVE LOW-VALUES          TO EL631FI
00333          GO TO 7000-DISPLAY-ISSUES.
00334
00335      
      * EXEC CICS HANDLE CONDITION
00336 *        PGMIDERR  (9600-PGMID-ERROR)
00337 *        ERROR     (9990-ABEND)
00338 *    END-EXEC.
      *    MOVE '"$L.                  ! " #00005217' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303035323137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00339
00340      IF EIBAID = DFHCLEAR
00341          GO TO 9400-CLEAR.
00342
00343      EJECT
00344  0200-RECEIVE.
00345      IF EIBAID = DFHPA1 OR
00346                  DFHPA2 OR
00347                  DFHPA3
00348          MOVE ER-0008            TO EMI-ERROR
00349          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00350          MOVE -1                 TO FPFENTRL
00351          GO TO 8200-SEND-DATAONLY.
00352
00353      
      * EXEC CICS RECEIVE
00354 *        MAP      (PI-MAP-NAME)
00355 *        MAPSET   (MAPSET-EL6313S)
00356 *        INTO     (EL631FI)
00357 *    END-EXEC.
           MOVE LENGTH OF
            EL631FI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00005235' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 EL631FI, 
                 DFHEIV11, 
                 MAPSET-EL6313S, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00358
00359      INSPECT EL631FI CONVERTING '_' TO ' '.
00360
00361      IF PI-MAP-NAME = EL631F
00362          IF FPFENTRL GREATER ZERO
00363              IF EIBAID NOT = DFHENTER
00364                  MOVE ER-0004    TO EMI-ERROR
00365                  MOVE AL-UNBOF   TO FPFENTRA
00366                  MOVE -1         TO FPFENTRL
00367                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00368                  GO TO 8200-SEND-DATAONLY
00369              ELSE
00370                  IF FPFENTRI NUMERIC  AND
00371                    (FPFENTRI GREATER 0 AND LESS 25)
00372                      MOVE PF-VALUES (FPFENTRI) TO EIBAID
00373                  ELSE
00374                      MOVE ER-0029  TO EMI-ERROR
00375                      MOVE AL-UNBOF TO FPFENTRA
00376                      MOVE -1       TO FPFENTRL
00377                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00378                      GO TO 8200-SEND-DATAONLY.
00379
00380      EJECT
00381  0300-CHECK-PFKEYS.
00382      IF EIBAID = DFHENTER
00383          GO TO 1000-EDIT-MAPF.
00384
00385      IF EIBAID = DFHPF12
00386          GO TO 9500-PF12.
00387
00388      IF EIBAID = DFHPF23
00389          GO TO 8810-PF23.
00390
00391      IF EIBAID = DFHPF24
00392          GO TO 9200-RETURN-MAIN-MENU.
00393
00394      MOVE ER-0029 TO EMI-ERROR.
00395      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00396
00397      MOVE -1                     TO FPFENTRL.
00398      GO TO 8200-SEND-DATAONLY.
00399
00400      EJECT
00401
00402  1000-EDIT-MAPF.
00403
00404      IF PI-COMPANY-ID NOT = 'DMD'
00405         MOVE ZEROS             TO RATEL
00406                                   PFRATEL
00407                                   RTCODEL
00408                                   RESSTL.
00409
00410      IF LASTNML  GREATER +0 OR
00411         FIRSTNML GREATER +0 OR
00412         INITL    GREATER +0 OR
00413         SOCSECL  GREATER +0 OR
CIDMOD        CBNAMEL        > +0 OR
CIDMOD        CBADDRL        > +0 OR
CIDMOD        CBCITYL        > +0 OR
CIDMOD        CBSTATEL       > +0 OR
CIDMOD        CBZIPL         > +0 OR
00414         ADDRS1L  GREATER +0 OR
00415         ADDRS2L  GREATER +0 OR
00416         ICITYL   GREATER +0 OR
00416         ISTATEL  GREATER +0 OR
00417         ZIPCDEL  GREATER +0 OR
00418         PHONEL   GREATER +0 OR
00419         JNT1STL  GREATER +0 OR
00420         JNTINITL GREATER +0 OR
00421         JNTLSTL  GREATER +0 OR
00422         JNTAGEL  GREATER +0 OR
00423         BENFARYL GREATER +0 OR
00424         POLFRML  GREATER +0 OR
00425         PFRATEL  GREATER +0 OR
00426         RTCODEL  GREATER +0 OR
00427         RESSTL   GREATER +0 OR
              JNTDOBL        > +0
00428            NEXT SENTENCE
00429         ELSE
00430            GO TO 7000-DISPLAY-ISSUES.
00431
00432      IF NOT MODIFY-CAP
00433          MOVE 'UPDATE'       TO SM-READ
00434          PERFORM 9995-SECURITY-VIOLATION
00435          MOVE ER-0070        TO EMI-ERROR
00436          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00437          GO TO 8100-SEND-INITIAL-MAP-CONT.
00438
00439      IF JNT1STL  GREATER +0
00440         MOVE AL-UANON          TO JNT1STA.
00441
00442      IF JNTINITL GREATER +0
00443         MOVE AL-UANON          TO JNTINITA.
00444
00445      IF JNTLSTL  GREATER +0
00446         MOVE AL-UANON          TO JNTLSTA.
00447
00448      IF JNTAGEL  GREATER +0
00449         IF JNTAGEI = '  '
00450            MOVE AL-UANON          TO JNTAGEA
00451            MOVE +0             TO WS-JNTAGE
00452         ELSE
00453            IF JNTAGEI  NUMERIC
00454               MOVE JNTAGEI        TO WS-JNTAGE
00455               MOVE AL-UNNON       TO JNTAGEA
00456            ELSE
00457               MOVE -1             TO JNTAGEL
00458               MOVE ER-2223        TO EMI-ERROR
00459               MOVE AL-UNBON       TO JNTAGEA
00460               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
100213
100213     IF JNTLSTL NOT = ZERO  OR
100213        JNT1STL NOT = ZERO  OR
100213        JNTINITL NOT = ZERO
100213         IF JNTAGEL = ZEROS
100213             MOVE -1                 TO JNTAGEL
100213             MOVE ER-3269            TO EMI-ERROR
100213             MOVE AL-UNBON           TO JNTAGEA
100213             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
100213         END-IF
100213     END-IF.
00461
           IF JNTDOBL > +0
              IF JNTDOBI NOT = ZEROS AND SPACES
                 MOVE JNTDOBI          TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT
                 MOVE DEEDIT-FIELD-V0  TO DC-GREG-DATE-1-MDY
                 MOVE AL-UNNON         TO JNTDOBA
                 MOVE '4'              TO DC-OPTION-CODE
                 PERFORM 9700-DATE-LINK
                 IF DATE-CONVERSION-ERROR
                    MOVE -1            TO JNTDOBL
                    MOVE ER-2228       TO EMI-ERROR
                    MOVE AL-UNBON      TO JNTDOBA
                    PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 END-IF
                 IF (NO-CONVERSION-ERROR)
                    AND (DC-BIN-DATE-1 > WS-CURRENT-BIN-DT)
                    MOVE DC-BIN-DATE-1 TO WS-WORK-BIN-DT
                    SUBTRACT CENTURY-ADJ FROM WS-WORK-BIN-RED
                    MOVE WS-WORK-BIN-DT
                                       TO DC-BIN-DATE-1
                                          WS-CONVERTED-BIRTH
                    MOVE AL-UANON      TO JNTDOBA
                 ELSE
                    MOVE DC-BIN-DATE-1 TO WS-CONVERTED-BIRTH
                    MOVE AL-UANON      TO JNTDOBA
                 END-IF
              ELSE
                 MOVE LOW-VALUES       TO WS-CONVERTED-BIRTH
              END-IF
           END-IF
00462      IF BENFARYL GREATER +0
00463          MOVE AL-UANON           TO BENFARYA.
00464
00465      IF POLFRML GREATER +0
00466          MOVE AL-UANON           TO POLFRMA.
00467      IF RATEL   GREATER +0
00468          MOVE AL-SANON           TO RATEA.
00469
00470      IF PFRATEL GREATER +0
00471          
      * EXEC CICS BIF DEEDIT
00472 *            FIELD   (PFRATEI)
00473 *            LENGTH  (8)
00474 *        END-EXEC
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005402' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PFRATEI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00475          IF PFRATEI  NUMERIC
00476             MOVE PFRATEI        TO WS-PFRATE
00477             MOVE AL-UNNON       TO PFRATEA
00478           ELSE
00479             MOVE -1             TO PFRATEL
00480             MOVE ER-7822        TO EMI-ERROR
00481             MOVE AL-UNBON       TO PFRATEA
00482             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00483
00484      IF RTCODEL GREATER +0
00485          MOVE AL-UANON           TO RTCODEA.
00486
00487      IF RESSTL  GREATER +0
00488        IF RESSTI NOT = SPACES
00489          MOVE RESSTI                 TO DRES-STATE
00490          
      * EXEC CICS LINK
00491 *            PROGRAM    ('DLO022')
00492 *            COMMAREA   (WS-DLO-RESIDENT-STATE)
00493 *            LENGTH     (DRES-COMM-LENGTH)
00494 *        END-EXEC
           MOVE 'DLO022' TO DFHEIV1
      *    MOVE '."C                   (   #00005421' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-DLO-RESIDENT-STATE, 
                 DRES-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00495          IF DRES-RETURN-CODE = 'OK'
00496              MOVE AL-UANON           TO RESSTA
00497            ELSE
00498              MOVE -1                 TO RESSTL
00499              MOVE AL-UABON           TO RESSTA
00500              MOVE ER-8204            TO EMI-ERROR
00501              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00502
CIDMOD     IF  CBNAMEL > +0
CIDMOD         MOVE AL-UANON           TO CBNAMEA
CIDMOD     END-IF
CIDMOD
CIDMOD     IF  CBADDRL > +0
CIDMOD         MOVE AL-UANON           TO CBADDRA
CIDMOD     END-IF
CIDMOD
CIDMOD     IF  CBCITYL > +0
CIDMOD         MOVE AL-UANON           TO CBCITYA
CIDMOD     END-IF
CIDMOD
      *    IF CBSTATEL > +0
      *       MOVE SPACES              TO ELCNTL-KEY
      *       MOVE PI-COMPANY-ID       TO ELCNTL-COMPANY-ID
      *       MOVE '3'                 TO ELCNTL-REC-TYPE
      *       MOVE CBSTATEI            TO ELCNTL-ACCESS
      *       MOVE +0                  TO ELCNTL-SEQ
      *       EXEC CICS READ
      *          DATASET   (FILE-ID-ELCNTL)
      *          SET       (ADDRESS OF CONTROL-FILE)
      *          RIDFLD    (ELCNTL-KEY)
      *          RESP      (WS-RESPONSE)
      *       END-EXEC
      *       IF RESP-NORMAL
      *          MOVE AL-UANON         TO CBSTATEA
      *       ELSE
      *          MOVE ER-2209          TO EMI-ERROR
      *          MOVE -1               TO CBSTATEL
      *          MOVE AL-UABON         TO CBSTATEA
      *          PERFORM 9900-ERROR-FORMAT
      *                                THRU 9900-EXIT
      *       END-IF
      *    END-IF
CIDMOD     IF  CBZIPL   > +0
CIDMOD         MOVE AL-UANON           TO CBZIPA
CIDMOD     END-IF
CIDMOD
00503      IF  ADDRS1L GREATER +0
00504          MOVE AL-UANON           TO ADDRS1A.
00505
00506      IF  ADDRS2L GREATER +0
00507          MOVE AL-UANON           TO ADDRS2A.
00508
00509      IF  ICITYL  GREATER +0
00510          MOVE AL-UANON           TO ICITYA.
      *    IF ISTATEL > +0
      *       MOVE SPACES              TO ELCNTL-KEY
      *       MOVE PI-COMPANY-ID       TO ELCNTL-COMPANY-ID
      *       MOVE '3'                 TO ELCNTL-REC-TYPE
      *       MOVE ISTATEI             TO ELCNTL-ACCESS
      *       MOVE +0                  TO ELCNTL-SEQ
      *       EXEC CICS READ
      *          DATASET   (FILE-ID-ELCNTL)
      *          SET       (ADDRESS OF CONTROL-FILE)
      *          RIDFLD    (ELCNTL-KEY)
      *          RESP      (WS-RESPONSE)
      *       END-EXEC
      *       IF RESP-NORMAL
      *          MOVE AL-UANON         TO ISTATEA
      *       ELSE
      *          MOVE ER-2209          TO EMI-ERROR
      *          MOVE -1               TO ISTATEL
      *          MOVE AL-UABON         TO ISTATEA
      *          PERFORM 9900-ERROR-FORMAT
      *                                THRU 9900-EXIT
      *       END-IF
      *    END-IF
00512      IF  ZIPCDEL GREATER +0
00513          MOVE AL-UANON           TO ZIPCDEA.
00514
           IF JNTDOBL > +0
              MOVE AL-UANON            TO JNTDOBA
           END-IF
00515      IF  PHONEL  GREATER +0
00516          MOVE PHONEI             TO DEEDIT-FIELD
00517          PERFORM 8600-DEEDIT
00518          MOVE DEEDIT-FIELD-V0 TO WS-PHONE
00519          MOVE AL-UANON       TO PHONEA.
00520
00521      IF  EMI-ERROR = ZEROS
00522          GO TO 1300-CHANGE-ISSUE-ROUTINE
00523      ELSE
00524          GO TO 8200-SEND-DATAONLY.
00525
00526      EJECT
00527
00528  1300-CHANGE-ISSUE-ROUTINE.
00529      MOVE 'C'                    TO PI-MAINT-FUNCTION.
00530
00531      IF  PI-MAIL-YES
00532          NEXT SENTENCE
00533       ELSE
00534          GO TO 1325-UPDATE-ISSUE-REC.
00535
00536      IF LASTNML  GREATER +0 OR
00537         FIRSTNML GREATER +0 OR
00538         INITL    GREATER +0 OR
00539         SOCSECL  GREATER +0 OR
CIDMOD        CBNAMEL        > +0 OR
CIDMOD        CBADDRL        > +0 OR
CIDMOD        CBCITYL        > +0 OR
              CBSTATEL       > +0 OR
CIDMOD        CBZIPL         > +0 OR
00540         ADDRS1L  GREATER +0 OR
00541         ADDRS2L  GREATER +0 OR
00542         ICITYL   GREATER +0 OR
00542         ISTATEL  GREATER +0 OR
00543         ZIPCDEL  GREATER +0 OR
00544         PHONEL   GREATER +0 OR
00545         JNT1STL  GREATER +0 OR
00546         JNTINITL GREATER +0 OR
00547         JNTLSTL  GREATER +0 OR
00548         BENFARYL GREATER +0 OR
00549         POLFRML  GREATER +0 OR
00550         PFRATEL  GREATER +0 OR
00551         RTCODEL  GREATER +0 OR
00552         RESSTL   GREATER +0 OR
              JNTDOBL > +0
00553            NEXT SENTENCE
00554         ELSE
00555            GO TO 1325-UPDATE-ISSUE-REC.
00556
00557      
      * EXEC CICS HANDLE CONDITION
00558 *         NOTFND    (1400-ADD-MAIL-RECORD)
00559 *    END-EXEC.
      *    MOVE '"$I                   ! # #00005557' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303035353537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00560
00744      
      * EXEC CICS GETMAIN
00745 *         SET       (ADDRESS OF PENDING-MAILING-DATA)
00746 *         LENGTH    (ERPNDM-RECORD-LENGTH)
00747 *         INITIMG   (GETMAIN-SPACE)
00748 *    END-EXEC.
      *    MOVE ',"IL                  $   #00005561' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERPNDM-RECORD-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF PENDING-MAILING-DATA TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00749
00561      
      * EXEC CICS READ
00562 *         DATASET   (FILE-ID-ERPNDM)
00563 *         RIDFLD    (ERPNDB-KEY)
00564 *         INTO      (PENDING-MAILING-DATA)
00565 *         UPDATE
00566 *    END-EXEC.
           MOVE LENGTH OF
            PENDING-MAILING-DATA
             TO DFHEIV11
      *    MOVE '&"IL       EU         (   #00005567' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDM, 
                 PENDING-MAILING-DATA, 
                 DFHEIV11, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00567
00568      MOVE PI-PROCESSOR-ID        TO PM-LAST-MAINT-BY.
00569      MOVE EIBTIME                TO PM-LAST-MAINT-HHMMSS.
00570      MOVE WS-CURRENT-BIN-DT      TO PM-LAST-MAINT-DT.
00571
00572      IF LASTNML  GREATER ZEROS
00573         MOVE LASTNMI             TO PM-INSURED-LAST-NAME.
00574
00575      IF FIRSTNML GREATER ZEROS
00576         MOVE FIRSTNMI            TO PM-INSURED-FIRST-NAME.
00577
00578      IF INITL    GREATER ZEROS
00579         MOVE INITI               TO PM-INSURED-MIDDLE-INIT.
00580
00581      IF SOCSECL  GREATER ZEROS
00582         MOVE SOCSECI             TO PM-INSURED-SOC-SEC-NO.
00583
CIDMOD     IF CBNAMEL > ZEROS
CIDMOD        MOVE CBNAMEI             TO  PM-CRED-BENE-NAME
CIDMOD     END-IF
CIDMOD
CIDMOD     IF CBADDRL > ZEROS
CIDMOD        MOVE CBADDRI             TO  PM-CRED-BENE-ADDR
CIDMOD     END-IF
CIDMOD
CIDMOD     IF CBCITYL > ZEROS
CIDMOD        MOVE CBCITYI             TO  PM-CRED-BENE-CITY
CIDMOD     END-IF
CIDMOD     IF CBSTATEL > ZEROS
CIDMOD        MOVE CBSTATEI            TO  PM-CRED-BENE-STATE
CIDMOD     END-IF
CIDMOD     IF CBZIPL   > ZEROS
CIDMOD        MOVE CBZIPI              TO  WS-ZIP-CODE
CIDMOD        IF WS-CANADIAN-ZIP
CIDMOD           IF WS-ZIP-4 = SPACE  OR  '-'
CIDMOD              MOVE WS-ZIP-CAN-2-POST1   TO PM-CB-CAN-POST1
CIDMOD              MOVE WS-ZIP-CAN-2-POST2   TO PM-CB-CAN-POST2
CIDMOD           ELSE
CIDMOD              MOVE WS-ZIP-CAN-1-POST1   TO PM-CB-CAN-POST1
CIDMOD              MOVE WS-ZIP-CAN-1-POST2   TO PM-CB-CAN-POST2
CIDMOD           END-IF
CIDMOD        ELSE
CIDMOD           IF WS-ZIP-6 = SPACE  OR  '-'
CIDMOD              MOVE WS-ZIP-AM-2-CODE     TO PM-CB-ZIP-CODE
CIDMOD              MOVE WS-ZIP-AM-2-PLUS4    TO PM-CB-ZIP-PLUS4
CIDMOD           ELSE
CIDMOD              MOVE WS-ZIP-AM-1-CODE     TO PM-CB-ZIP-CODE
CIDMOD              MOVE WS-ZIP-AM-1-PLUS4    TO PM-CB-ZIP-PLUS4
CIDMOD           END-IF
CIDMOD        END-IF
CIDMOD     END-IF
CIDMOD
00584      IF ADDRS1L GREATER ZEROS
00585         MOVE ADDRS1I             TO  PM-ADDRESS-LINE-1.
00586
00587      IF ADDRS2L GREATER ZEROS
00588         MOVE ADDRS2I             TO  PM-ADDRESS-LINE-2.
00589
100217     if (icityl > zeros)
100217        or (istatel > zeros)
100217        or (zipcdel > zeros)
100217        move spaces              to pm-city-st-zip-verified
100217     end-if
00590      IF ICITYL GREATER ZEROS
00591         MOVE ICITYI              TO  PM-CITY.
00590      IF ISTATEL GREATER ZEROS
00591         MOVE ISTATEI             TO  PM-STATE.
           IF JNTDOBL > +0
              MOVE WS-CONVERTED-BIRTH  TO PM-JOINT-BIRTH-DT
           END-IF
00593      IF ZIPCDEL GREATER ZEROS
00594          MOVE ZIPCDEI            TO  WS-ZIP-CODE
00595      ELSE
00596          GO TO 1310-CONTINUE.
00597
00598      IF WS-CANADIAN-ZIP
00599          IF WS-ZIP-4 = SPACE  OR  '-'
00600              MOVE WS-ZIP-CAN-2-POST1   TO PM-CAN-POST1
00601              MOVE WS-ZIP-CAN-2-POST2   TO PM-CAN-POST2
00602          ELSE
00603              MOVE WS-ZIP-CAN-1-POST1   TO PM-CAN-POST1
00604              MOVE WS-ZIP-CAN-1-POST2   TO PM-CAN-POST2
00605      ELSE
00606          IF WS-ZIP-6 = SPACE  OR  '-'
00607              MOVE WS-ZIP-AM-2-CODE     TO PM-ZIP-CODE
00608              MOVE WS-ZIP-AM-2-PLUS4    TO PM-ZIP-PLUS4
00609          ELSE
00610              MOVE WS-ZIP-AM-1-CODE     TO PM-ZIP-CODE
00611              MOVE WS-ZIP-AM-1-PLUS4    TO PM-ZIP-PLUS4.
00612
00613  1310-CONTINUE.
00614
00615      IF PHONEL  GREATER ZEROS
00616         MOVE WS-PHONE            TO  PM-PHONE-NO.
00617
00618      
      * EXEC CICS REWRITE
00619 *         DATASET   (FILE-ID-ERPNDM)
00620 *         FROM      (PENDING-MAILING-DATA)
00621 *    END-EXEC.
           MOVE LENGTH OF
            PENDING-MAILING-DATA
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005668' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDM, 
                 PENDING-MAILING-DATA, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00807      MOVE 'Y'                    TO WS-ADD-ADDRESS-SW.
00622
00623  1325-UPDATE-ISSUE-REC.
00624
00625      IF JNT1STL  GREATER +0 OR
00626         JNTINITL GREATER +0 OR
00627         JNTLSTL  GREATER +0 OR
00628         JNTAGEL  GREATER +0 OR
00629         BENFARYL GREATER +0 OR
00630         POLFRML  GREATER +0 OR
00631         PFRATEL  GREATER +0 OR
00632         RTCODEL  GREATER +0 OR
00633         RESSTL   GREATER +0 OR
00634         LASTNML  GREATER +0 OR
00635         FIRSTNML GREATER +0 OR
00636         INITL    GREATER +0 OR
00637         SOCSECL  GREATER +0 OR
              JNTDOBL        > +0 OR
00638         WS-ADDRESS-ADDED
00639            NEXT SENTENCE
00640         ELSE
00641            GO TO 7000-DISPLAY-ISSUES.
00642
00643      
      * EXEC CICS HANDLE CONDITION
00644 *         NOTFND    (1385-REC-NOT-FOUND)
00645 *    END-EXEC.
      *    MOVE '"$I                   ! $ #00005695' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303035363935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00646
00647      
      * EXEC CICS READ
00648 *         DATASET   (FILE-ID-ERPNDB)
00649 *         RIDFLD    (ERPNDB-KEY)
00650 *         SET       (ADDRESS OF PENDING-BUSINESS)
00651 *         UPDATE
00652 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00005699' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDB, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00653
041320     MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH
041320     MOVE FILE-ID-ERPNDB         TO JP-FILE-ID
041320     MOVE 'B'                    TO JP-RECORD-TYPE
041320     MOVE PENDING-BUSINESS       TO JP-RECORD-AREA
00655      MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY.
00656      MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.
00657      MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT.
00658
00659      IF PI-MAIL-YES
00660         MOVE '1'                 TO PB-I-MAIL-ADDRS-SW.
00661
00662      IF LASTNML  GREATER ZEROS
00663         MOVE LASTNMI             TO PB-I-INSURED-LAST-NAME.
00664
00665      IF FIRSTNML GREATER ZEROS
00666         MOVE FIRSTNMI            TO PB-I-INSURED-FIRST-NAME.
00667
00668      IF INITL    GREATER ZEROS
00669         MOVE INITI               TO PB-I-INSURED-MIDDLE-INIT.
00670
00671      IF SOCSECL  GREATER ZEROS
00672         MOVE SOCSECI             TO PB-I-SOC-SEC-NO.
00673
00674      IF JNT1STL  GREATER ZEROS
00675         MOVE JNT1STI             TO PB-I-JOINT-FIRST-NAME.
00676
00677      IF JNTINITL GREATER ZEROS
00678         MOVE JNTINITI            TO PB-I-JOINT-MIDDLE-INIT.
00679
00680      IF JNTLSTL  GREATER ZEROS
00681         MOVE JNTLSTI             TO PB-I-JOINT-LAST-NAME.
00682
00683      IF JNTAGEL  GREATER ZEROS
00684         MOVE WS-JNTAGE           TO PB-I-JOINT-AGE.
           IF JNTDOBL > +0
              MOVE WS-CONVERTED-BIRTH  TO PB-I-JOINT-BIRTHDAY
           END-IF
121712
121712     MOVE 'N'                    TO WS-INS-AGE-SET
121712                                    WS-JNT-AGE-SET.
121712     IF JNTAGEL  GREATER THAN ZEROS
121712        MOVE 'Y'                TO WS-JNT-AGE-SET
121712        IF WS-JNTAGE = ZERO AND
121712          PB-I-JOINT-INSURED > SPACES AND
121712          (PB-I-JOINT-BIRTHDAY = LOW-VALUES OR SPACES)
121712           MOVE 'Y'             TO WS-JNT-AGE-DEFAULTED
121712           MOVE 'AGE*'          TO JAGEDEFO
121712        ELSE
121712           MOVE 'N'             TO WS-JNT-AGE-DEFAULTED
121712           MOVE 'AGE '          TO JAGEDEFO
121712        END-IF
121712        PERFORM 1386-UPDATE-AGE-FLAGS THRU 1386-EXIT
121712     END-IF.
00686      IF BENFARYL GREATER ZEROS
00687         MOVE BENFARYI            TO PB-I-BENEFICIARY-NAME.
00688
00689      IF POLFRML GREATER +0
00690         MOVE POLFRMI             TO PB-I-POLICY-FORM-NO.
00691
00692      IF RTCODEL GREATER +0
00693         MOVE RTCODEI             TO PB-I-RATE-CODE.
00694
00695      IF PFRATEL GREATER +0
00696          IF PB-VALID-LIFE
00697             MOVE WS-PFRATE      TO PB-I-AH-RATE
00698           ELSE
00699             MOVE WS-PFRATE      TO PB-I-LF-RATE.
00700
00701      IF RESSTL  GREATER +0
00702         MOVE RESSTI              TO PB-I-RESIDENT-STATE.
00703
00704      MOVE PB-COMMON-ERRORS       TO WS-SAVE-COMMON-ERRORS.
00705
00706      PERFORM 9800-LINK-PENDING-EDIT THRU 9800-EXIT.
           PERFORM 7300-FORMAT-ERRORS  THRU 7399-EXIT
041320     PERFORM 8400-LOG-JOURNAL-RECORD  *> Before Issue Image
041320     MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH
041320     MOVE FILE-ID-ERPNDB         TO JP-FILE-ID
041320     MOVE 'C'                    TO JP-RECORD-TYPE
041320     MOVE PENDING-BUSINESS       TO JP-RECORD-AREA
00708      MOVE PB-SV-CARRIER          TO PI-SV-CARRIER.
00709      MOVE PB-SV-GROUPING         TO PI-SV-GROUPING.
00710      MOVE PB-SV-STATE            TO PI-SV-STATE.
00711
00712      
      * EXEC CICS REWRITE
00713 *         DATASET   (FILE-ID-ERPNDB)
00714 *         FROM      (PENDING-BUSINESS)
00715 *    END-EXEC.
           MOVE LENGTH OF
            PENDING-BUSINESS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005790' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDB, 
                 PENDING-BUSINESS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
041320     PERFORM 8400-LOG-JOURNAL-RECORD  *> After Issue Image
072312******************************************************************
072312*       U P D A T E   T H E   O R I G   C E R T   I N F O        *
072312******************************************************************
072312
072312     display ' made it to update orig cert '
121712
121712     PERFORM 1495-READ-CERT-TRAILER THRU 1495-EXIT
072312
072312     MOVE PB-CONTROL-BY-ACCOUNT (1:33)
072312                                 TO ELCRTO-KEY
072312     MOVE 'I'                    TO ELCRTO-RECORD-TYPE
072312     MOVE +0                     TO ELCRTO-SEQ-NO
072312
072312     
      * EXEC CICS READ
072312*       DATASET   ('ELCRTO')
072312*       INTO      (ORIGINAL-CERTIFICATE)
072312*       RIDFLD    (ELCRTO-KEY)
072312*       GTEQ
072312*       RESP      (WS-RESPONSE)
072312*    END-EXEC
           MOVE LENGTH OF
            ORIGINAL-CERTIFICATE
             TO DFHEIV11
           MOVE 'ELCRTO' TO DFHEIV1
      *    MOVE '&"IL       G          (  N#00005808' TO DFHEIV0
           MOVE X'2622494C2020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303035383038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ORIGINAL-CERTIFICATE, 
                 DFHEIV11, 
                 ELCRTO-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
072312
072312     display ' just read gteq ' ws-response
072312     IF RESP-NORMAL
072312        AND (OC-CONTROL-PRIMARY (1:33) =
072312                 PB-CONTROL-BY-ACCOUNT (1:33))
072312        AND (OC-RECORD-TYPE = 'I')
072312        display ' resp norm, key =, type i '
072312        IF (OC-ENDORSEMENT-PROCESSED-DT = LOW-VALUES)
072312           display ' end proc blank '
072312           display ' csr edit session ' PI-CSR-SESSION-SW
072312           IF NOT CSR-EDIT-SESSION
072312              
      * EXEC CICS READ
072312*                DATASET   ('ELCRTO')
072312*                INTO      (ORIGINAL-CERTIFICATE)
072312*                RIDFLD    (OC-CONTROL-PRIMARY)
072312*                UPDATE
072312*                RESP      (WS-RESPONSE)
072312*             END-EXEC
           MOVE LENGTH OF
            ORIGINAL-CERTIFICATE
             TO DFHEIV11
           MOVE 'ELCRTO' TO DFHEIV1
      *    MOVE '&"IL       EU         (  N#00005826' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303035383236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ORIGINAL-CERTIFICATE, 
                 DFHEIV11, 
                 OC-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
072312              DISPLAY ' JUST DID READ UPD ' WS-RESPONSE
072312              IF RESP-NORMAL
072312                 display ' good read upd '
072312                 MOVE PB-I-INSURED-LAST-NAME TO
072312                         OC-INS-LAST-NAME
072312                 MOVE PB-I-INSURED-FIRST-NAME TO
072312                         OC-INS-FIRST-NAME
072312                 MOVE PB-I-INSURED-MIDDLE-INIT TO
072312                         OC-INS-MIDDLE-INIT
072312                 MOVE PB-I-AGE         TO OC-INS-AGE
072312                 MOVE PB-I-JOINT-LAST-NAME   TO
072312                         OC-JNT-LAST-NAME
072312                 MOVE PB-I-JOINT-FIRST-NAME  TO
072312                         OC-JNT-FIRST-NAME
072312                 MOVE PB-I-JOINT-MIDDLE-INIT TO
072312                         OC-JNT-MIDDLE-INIT
072312                 MOVE PB-I-JOINT-AGE   TO OC-JNT-AGE
072312                 MOVE PB-I-LF-BENEFIT-CD TO OC-LF-BENCD
072312                 MOVE PB-I-LF-TERM     TO OC-LF-TERM
072312                 MOVE PB-I-LF-BENEFIT-AMT TO OC-LF-BEN-AMT
072312                 MOVE PB-I-LF-PREMIUM-AMT TO OC-LF-PRM-AMT
072312                 MOVE PB-I-LF-ALT-BENEFIT-AMT TO
072312                         OC-LF-ALT-BEN-AMT
072312                 MOVE PB-I-LF-ALT-PREMIUM-AMT TO
072312                         OC-LF-ALT-PRM-AMT
072312                 MOVE PB-I-LF-EXPIRE-DT TO OC-LF-EXP-DT
072312                 MOVE PB-I-LIFE-COMMISSION TO OC-LF-COMM-PCT
072312                 MOVE LOW-VALUES       TO OC-LF-CANCEL-DT
072312                 MOVE +0               TO OC-LF-CANCEL-AMT
072312                                          OC-LF-ITD-CANCEL-AMT
072312                 MOVE PB-I-AH-BENEFIT-CD TO OC-AH-BENCD
072312                 MOVE PB-I-AH-TERM     TO OC-AH-TERM
072312                 MOVE PB-I-AH-BENEFIT-AMT TO OC-AH-BEN-AMT
072312                 MOVE PB-I-AH-PREMIUM-AMT TO OC-AH-PRM-AMT
072312                 MOVE PB-I-AH-EXPIRE-DT TO OC-AH-EXP-DT
072312                 MOVE PB-I-AH-COMMISSION TO OC-AH-COMM-PCT
072312                 MOVE PB-I-AH-CRIT-PER TO OC-AH-CP
072312                 MOVE LOW-VALUES       TO OC-AH-CANCEL-DT
072312                 MOVE +0               TO OC-AH-CANCEL-AMT
072312                                          OC-AH-ITD-CANCEL-AMT
072312                 MOVE PB-I-1ST-PMT-DT  TO OC-1ST-PMT-DT
011413                 MOVE 'Y'              TO OC-ISSUE-TRAN-IND
072312
072312                 MOVE PI-PROCESSOR-ID  TO OC-LAST-MAINT-BY
072312                 MOVE EIBTIME          TO OC-LAST-MAINT-HHMMSS
072312                 MOVE WS-CURRENT-BIN-DT TO OC-LAST-MAINT-DT
                       IF WS-ADDRESS-ADDED
072312                    MOVE PM-CRED-BENE-NAME
072312                                 TO OC-CRED-BENE-NAME
                       END-IF
121712                 IF NOT CERT-TRL-REC-NOT-FOUND
121712                    MOVE CS-INS-AGE-DEFAULT-FLAG TO
121712                                  OC-INS-AGE-DEFAULT-FLAG
121712                    MOVE CS-JNT-AGE-DEFAULT-FLAG TO
121712                                  OC-JNT-AGE-DEFAULT-FLAG
121712                 END-IF
072312                 
      * EXEC CICS REWRITE
072312*                   DATASET   ('ELCRTO')
072312*                   FROM      (ORIGINAL-CERTIFICATE)
072312*                   RESP      (WS-RESPONSE)
072312*                END-EXEC
           MOVE LENGTH OF
            ORIGINAL-CERTIFICATE
             TO DFHEIV11
           MOVE 'ELCRTO' TO DFHEIV1
      *    MOVE '&& L                  %  N#00005889' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'204E233030303035383839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ORIGINAL-CERTIFICATE, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
072312                 display ' just rewrote ' ws-response
072312              END-IF
072312           END-IF
072312        ELSE
072312           SUBTRACT +1 FROM OC-KEY-SEQ-NO
072312           MOVE 'OC'             TO OC-RECORD-ID
072312           MOVE PI-PROCESSOR-ID  TO OC-LAST-MAINT-BY
072312           MOVE EIBTIME          TO OC-LAST-MAINT-HHMMSS
072312           MOVE WS-CURRENT-BIN-DT TO OC-LAST-MAINT-DT
072312           MOVE PB-I-INSURED-LAST-NAME TO OC-INS-LAST-NAME
072312           MOVE PB-I-INSURED-FIRST-NAME TO OC-INS-FIRST-NAME
072312           MOVE PB-I-INSURED-MIDDLE-INIT TO OC-INS-MIDDLE-INIT
072312           MOVE PB-I-AGE         TO OC-INS-AGE
072312           MOVE PB-I-JOINT-LAST-NAME TO OC-JNT-LAST-NAME
072312           MOVE PB-I-JOINT-FIRST-NAME TO OC-JNT-FIRST-NAME
072312           MOVE PB-I-JOINT-MIDDLE-INIT TO OC-JNT-MIDDLE-INIT
072312           MOVE PB-I-JOINT-AGE   TO OC-JNT-AGE
072312           MOVE PB-I-LF-BENEFIT-CD TO OC-LF-BENCD
072312           MOVE PB-I-LF-TERM     TO OC-LF-TERM
072312           MOVE PB-I-LF-BENEFIT-AMT TO OC-LF-BEN-AMT
072312           MOVE PB-I-LF-PREMIUM-AMT TO OC-LF-PRM-AMT
072312           MOVE PB-I-LF-ALT-BENEFIT-AMT TO OC-LF-ALT-BEN-AMT
072312           MOVE PB-I-LF-ALT-PREMIUM-AMT TO OC-LF-ALT-PRM-AMT
072312           MOVE PB-I-LF-EXPIRE-DT TO OC-LF-EXP-DT
072312           MOVE PB-I-LIFE-COMMISSION TO OC-LF-COMM-PCT
072312           MOVE LOW-VALUES       TO OC-LF-CANCEL-DT
072312           MOVE +0               TO OC-LF-CANCEL-AMT
072312                                    OC-LF-ITD-CANCEL-AMT
072312           MOVE PB-I-AH-BENEFIT-CD TO OC-AH-BENCD
072312           MOVE PB-I-AH-TERM     TO OC-AH-TERM
072312           MOVE PB-I-AH-BENEFIT-AMT TO OC-AH-BEN-AMT
072312           MOVE PB-I-AH-PREMIUM-AMT TO OC-AH-PRM-AMT
072312           MOVE PB-I-AH-EXPIRE-DT TO OC-AH-EXP-DT
072312           MOVE PB-I-AH-COMMISSION TO OC-AH-COMM-PCT
072312           MOVE PB-I-AH-CRIT-PER TO OC-AH-CP
072312           MOVE LOW-VALUES       TO OC-AH-CANCEL-DT
072312           MOVE +0               TO OC-AH-CANCEL-AMT
072312                                    OC-AH-ITD-CANCEL-AMT
072312           MOVE PB-I-1ST-PMT-DT  TO OC-1ST-PMT-DT
011413           MOVE 'Y'              TO OC-ISSUE-TRAN-IND
011413           MOVE 'N'              TO OC-CANCEL-TRAN-IND
072312           MOVE PM-CRED-BENE-NAME
072312                                 TO OC-CRED-BENE-NAME
121712           IF NOT CERT-TRL-REC-NOT-FOUND
121712              MOVE CS-INS-AGE-DEFAULT-FLAG TO
121712                            OC-INS-AGE-DEFAULT-FLAG
121712              MOVE CS-JNT-AGE-DEFAULT-FLAG TO
121712                            OC-JNT-AGE-DEFAULT-FLAG
121712           END-IF
072312           MOVE LOW-VALUES       TO OC-ENDORSEMENT-PROCESSED-DT
072312           
      * EXEC CICS WRITE
072312*             DATASET   ('ELCRTO')
072312*             FROM      (ORIGINAL-CERTIFICATE)
072312*             RIDFLD    (OC-CONTROL-PRIMARY)
072312*             RESP      (WS-RESPONSE)
072312*          END-EXEC
           MOVE LENGTH OF
            ORIGINAL-CERTIFICATE
             TO DFHEIV11
           MOVE 'ELCRTO' TO DFHEIV1
      *    MOVE '&$ L                  ''  N#00005944' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303035393434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ORIGINAL-CERTIFICATE, 
                 DFHEIV11, 
                 OC-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
072312        END-IF
072312     ELSE
072312        MOVE SPACES              TO ORIGINAL-CERTIFICATE
072312        MOVE 'OC'                TO OC-RECORD-ID
072312        MOVE PB-CONTROL-BY-ACCOUNT (1:33)
072312                                 TO OC-CONTROL-PRIMARY (1:33)
072312        MOVE 'I'                 TO OC-RECORD-TYPE
072312        MOVE +4096               TO OC-KEY-SEQ-NO
072312        MOVE PI-PROCESSOR-ID     TO OC-LAST-MAINT-BY
072312        MOVE EIBTIME             TO OC-LAST-MAINT-HHMMSS
072312        MOVE WS-CURRENT-BIN-DT   TO OC-LAST-MAINT-DT
072312        MOVE PB-I-INSURED-LAST-NAME TO OC-INS-LAST-NAME
072312        MOVE PB-I-INSURED-FIRST-NAME TO OC-INS-FIRST-NAME
072312        MOVE PB-I-INSURED-MIDDLE-INIT TO OC-INS-MIDDLE-INIT
072312        MOVE PB-I-AGE            TO OC-INS-AGE
072312        MOVE PB-I-JOINT-LAST-NAME TO OC-JNT-LAST-NAME
072312        MOVE PB-I-JOINT-FIRST-NAME TO OC-JNT-FIRST-NAME
072312        MOVE PB-I-JOINT-MIDDLE-INIT TO OC-JNT-MIDDLE-INIT
072312        MOVE PB-I-JOINT-AGE      TO OC-JNT-AGE
072312        MOVE PB-I-LF-BENEFIT-CD  TO OC-LF-BENCD
072312        MOVE PB-I-LF-TERM        TO OC-LF-TERM
072312        MOVE PB-I-LF-BENEFIT-AMT TO OC-LF-BEN-AMT
072312        MOVE PB-I-LF-PREMIUM-AMT TO OC-LF-PRM-AMT
072312        MOVE PB-I-LF-ALT-BENEFIT-AMT TO OC-LF-ALT-BEN-AMT
072312        MOVE PB-I-LF-ALT-PREMIUM-AMT TO OC-LF-ALT-PRM-AMT
072312        MOVE PB-I-LF-EXPIRE-DT   TO OC-LF-EXP-DT
072312        MOVE PB-I-LIFE-COMMISSION TO OC-LF-COMM-PCT
072312        MOVE LOW-VALUES          TO OC-LF-CANCEL-DT
072312        MOVE +0                  TO OC-LF-CANCEL-AMT
072312                                    OC-LF-ITD-CANCEL-AMT
072312        MOVE PB-I-AH-BENEFIT-CD  TO OC-AH-BENCD
072312        MOVE PB-I-AH-TERM        TO OC-AH-TERM
072312        MOVE PB-I-AH-BENEFIT-AMT TO OC-AH-BEN-AMT
072312        MOVE PB-I-AH-PREMIUM-AMT TO OC-AH-PRM-AMT
072312        MOVE PB-I-AH-EXPIRE-DT   TO OC-AH-EXP-DT
072312        MOVE PB-I-AH-COMMISSION  TO OC-AH-COMM-PCT
072312        MOVE PB-I-AH-CRIT-PER    TO OC-AH-CP
072312        MOVE LOW-VALUES          TO OC-AH-CANCEL-DT
072312        MOVE +0                  TO OC-AH-CANCEL-AMT
072312                                    OC-AH-ITD-CANCEL-AMT
072312        MOVE PB-I-1ST-PMT-DT     TO OC-1ST-PMT-DT
011413        MOVE 'Y'                 TO OC-ISSUE-TRAN-IND
011413        MOVE 'N'                 TO OC-CANCEL-TRAN-IND
072312        MOVE PM-CRED-BENE-NAME
072312                                 TO OC-CRED-BENE-NAME
121712        IF NOT CERT-TRL-REC-NOT-FOUND
121712           MOVE CS-INS-AGE-DEFAULT-FLAG TO
121712                         OC-INS-AGE-DEFAULT-FLAG
121712           MOVE CS-JNT-AGE-DEFAULT-FLAG TO
121712                         OC-JNT-AGE-DEFAULT-FLAG
121712        END-IF
072312        MOVE LOW-VALUES          TO OC-ENDORSEMENT-PROCESSED-DT
072312
072312        
      * EXEC CICS WRITE
072312*          DATASET   ('ELCRTO')
072312*          FROM      (ORIGINAL-CERTIFICATE)
072312*          RIDFLD    (OC-CONTROL-PRIMARY)
072312*          RESP      (WS-RESPONSE)
072312*       END-EXEC
           MOVE LENGTH OF
            ORIGINAL-CERTIFICATE
             TO DFHEIV11
           MOVE 'ELCRTO' TO DFHEIV1
      *    MOVE '&$ L                  ''  N#00006003' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303036303033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ORIGINAL-CERTIFICATE, 
                 DFHEIV11, 
                 OC-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
072312     END-IF
072312
00717      GO TO 7000-DISPLAY-ISSUES.
00718
00719  1385-REC-NOT-FOUND.
00720      MOVE -1                     TO MAINTL.
00721      MOVE ER-2239                TO EMI-ERROR.
00722      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00723      GO TO 8200-SEND-DATAONLY.
00724
121712 1386-UPDATE-AGE-FLAGS.
121712
121712     
      * EXEC CICS GETMAIN
121712*        SET     (ADDRESS OF CERTIFICATE-TRAILERS)
121712*        LENGTH  (ELCRTT-RECORD-LENGTH)
121712*        INITIMG (GETMAIN-SPACE)
121712*    END-EXEC
      *    MOVE ',"IL                  $   #00006021' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ELCRTT-RECORD-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF CERTIFICATE-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
121712
121712     
      * EXEC CICS HANDLE CONDITION
121712*        NOTFND   (1386-NOTFND)
121712*    END-EXEC.
      *    MOVE '"$I                   ! % #00006027' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303036303237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
121712
121712     MOVE PB-CONTROL-BY-ACCOUNT (1:33) TO ELCRTT-PRIMARY
121712     MOVE 'C'                TO ELCRTT-REC-TYPE
121712
121712     
      * EXEC CICS READ
121712*        UPDATE
121712*        DATASET  (CRTT-ID)
121712*        RIDFLD   (ELCRTT-KEY)
121712*        INTO     (CERTIFICATE-TRAILERS)
121712*    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-TRAILERS
             TO DFHEIV11
      *    MOVE '&"IL       EU         (   #00006034' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CRTT-ID, 
                 CERTIFICATE-TRAILERS, 
                 DFHEIV11, 
                 ELCRTT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
121712
121712     IF WS-INS-AGE-SET = 'Y'
121712        MOVE WS-INS-AGE-DEFAULTED TO CS-INS-AGE-DEFAULT-FLAG
121712     END-IF
121712     IF WS-JNT-AGE-SET = 'Y'
121712        MOVE WS-JNT-AGE-DEFAULTED TO CS-JNT-AGE-DEFAULT-FLAG
121712     END-IF
121712
121712     
      * EXEC CICS REWRITE
121712*       DATASET  (CRTT-ID)
121712*       FROM     (CERTIFICATE-TRAILERS)
121712*    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-TRAILERS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00006048' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CRTT-ID, 
                 CERTIFICATE-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
121712
121712     GO TO 1386-EXIT.
121712
121712 1386-NOTFND.
121712
121712     MOVE SPACES       TO CERTIFICATE-TRAILERS.
121712     MOVE 'CS'         TO CS-RECORD-ID.
121712     MOVE PB-CONTROL-BY-ACCOUNT (1:33) TO ELCRTT-PRIMARY.
121712     MOVE 'C'          TO ELCRTT-REC-TYPE.
121712     MOVE ELCRTT-KEY TO CS-CONTROL-PRIMARY.
121712     MOVE WS-INS-AGE-DEFAULTED TO CS-INS-AGE-DEFAULT-FLAG.
121712     MOVE WS-JNT-AGE-DEFAULTED TO CS-JNT-AGE-DEFAULT-FLAG.
121712     
      * EXEC CICS WRITE
121712*       DATASET  (CRTT-ID)
121712*       RIDFLD   (ELCRTT-KEY)
121712*       FROM     (CERTIFICATE-TRAILERS)
121712*    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-TRAILERS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00006064' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CRTT-ID, 
                 CERTIFICATE-TRAILERS, 
                 DFHEIV11, 
                 ELCRTT-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
121712
121712 1386-EXIT.
121712     EXIT.
121712
00725  1400-ADD-MAIL-RECORD.
00726
00727      IF  (ADDRS1L NOT GREATER ZEROS)
00728              AND
00729          (ADDRS2L NOT GREATER ZEROS)
00730              AND
00731          (ICITYL NOT GREATER ZEROS)
CIDMOD             AND
00731          (ISTATEL NOT GREATER ZEROS)
CIDMOD             AND
CIDMOD         (CBNAMEL NOT > +0)
CIDMOD             AND
CIDMOD         (CBADDRL NOT > +0)
CIDMOD             AND
CIDMOD         (CBCITYL  NOT > +0)
CIDMOD             AND
CIDMOD         (CBSTATEL NOT > +0)
CIDMOD             AND
CIDMOD         (CBZIPL   NOT > +0)
                   AND
               (JNTDOBL NOT > +0)
00732          GO TO 1325-UPDATE-ISSUE-REC.
00733
00734      
      * EXEC CICS HANDLE CONDITION
00735 *         NOTFND    (1485-REC-NOT-FOUND)
00736 *    END-EXEC.
      *    MOVE '"$I                   ! & #00006096' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303036303936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00737
00738      
      * EXEC CICS READ
00739 *         DATASET   (FILE-ID-ERPNDB)
00740 *         RIDFLD    (ERPNDB-KEY)
00741 *         SET       (ADDRESS OF PENDING-BUSINESS)
00742 *    END-EXEC.
      *    MOVE '&"S        E          (   #00006100' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036313030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDB, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00743
00744      
      * EXEC CICS GETMAIN
00745 *         SET       (ADDRESS OF PENDING-MAILING-DATA)
00746 *         LENGTH    (ERPNDM-RECORD-LENGTH)
00747 *         INITIMG   (GETMAIN-SPACE)
00748 *    END-EXEC.
      *    MOVE ',"IL                  $   #00006106' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036313036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERPNDM-RECORD-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF PENDING-MAILING-DATA TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00749
00750      MOVE 'PM'                       TO PM-RECORD-ID.
00751      MOVE 'ER'                       TO PM-SOURCE-SYSTEM.
00752
00753      MOVE PI-PROCESSOR-ID            TO PM-LAST-MAINT-BY
00754                                         PM-RECORD-ADDED-BY.
00755
00756      MOVE WS-CURRENT-BIN-DT          TO PM-LAST-MAINT-DT.
00757      MOVE EIBTIME                    TO PM-LAST-MAINT-HHMMSS.
00758
00759      MOVE ERPNDB-KEY                 TO PM-CONTROL-PRIMARY
00760      MOVE PB-I-INSURED-LAST-NAME     TO PM-INSURED-LAST-NAME.
00761      MOVE PB-I-INSURED-FIRST-NAME    TO PM-INSURED-FIRST-NAME.
00762      MOVE PB-I-INSURED-MIDDLE-INIT   TO PM-INSURED-MIDDLE-INIT.
00763      MOVE PB-I-AGE                   TO PM-INSURED-ISSUE-AGE.
00764      MOVE PB-I-BIRTHDAY              TO PM-INSURED-BIRTH-DT
           MOVE PB-I-JOINT-BIRTHDAY        TO PM-JOINT-BIRTH-DT
00765
CIDMOD     IF CBNAMEL > ZEROS
CIDMOD        MOVE CBNAMEI             TO  PM-CRED-BENE-NAME
CIDMOD     END-IF
CIDMOD
CIDMOD     IF CBADDRL > ZEROS
CIDMOD        MOVE CBADDRI             TO  PM-CRED-BENE-ADDR
CIDMOD     END-IF
CIDMOD
CIDMOD     IF CBCITYL > ZEROS
CIDMOD        MOVE CBCITYI             TO  PM-CRED-BENE-CITY
CIDMOD     END-IF
CIDMOD     IF CBSTATEL > ZEROS
CIDMOD        MOVE CBSTATEI            TO  PM-CRED-BENE-STATE
CIDMOD     END-IF
CIDMOD     IF CBZIPL   > ZEROS
CIDMOD        MOVE CBZIPI              TO  WS-ZIP-CODE
CIDMOD        IF WS-CANADIAN-ZIP
CIDMOD           IF WS-ZIP-4 = SPACE  OR  '-'
CIDMOD              MOVE WS-ZIP-CAN-2-POST1   TO PM-CB-CAN-POST1
CIDMOD              MOVE WS-ZIP-CAN-2-POST2   TO PM-CB-CAN-POST2
CIDMOD           ELSE
CIDMOD              MOVE WS-ZIP-CAN-1-POST1   TO PM-CB-CAN-POST1
CIDMOD              MOVE WS-ZIP-CAN-1-POST2   TO PM-CB-CAN-POST2
CIDMOD           END-IF
CIDMOD        ELSE
CIDMOD           IF WS-ZIP-6 = SPACE  OR  '-'
CIDMOD              MOVE WS-ZIP-AM-2-CODE     TO PM-CB-ZIP-CODE
CIDMOD              MOVE WS-ZIP-AM-2-PLUS4    TO PM-CB-ZIP-PLUS4
CIDMOD           ELSE
CIDMOD              MOVE WS-ZIP-AM-1-CODE     TO PM-CB-ZIP-CODE
CIDMOD              MOVE WS-ZIP-AM-1-PLUS4    TO PM-CB-ZIP-PLUS4
CIDMOD           END-IF
CIDMOD        END-IF
CIDMOD     END-IF
CIDMOD
00766      IF ADDRS1L GREATER ZEROS
00767         MOVE ADDRS1I             TO  PM-ADDRESS-LINE-1.
00768
00769      IF ADDRS2L GREATER ZEROS
00770         MOVE ADDRS2I             TO  PM-ADDRESS-LINE-2.
00771
00772      IF ICITYL  GREATER ZEROS
00773         MOVE ICITYI              TO  PM-CITY.
00772      IF ISTATEL  GREATER ZEROS
00773         MOVE ISTATEI             TO  PM-STATE.
           IF JNTDOBL > +0
              MOVE WS-CONVERTED-BIRTH  TO PM-JOINT-BIRTH-DT
           END-IF
00775      IF ZIPCDEL GREATER ZEROS
00776          MOVE ZIPCDEI            TO  WS-ZIP-CODE
00777      ELSE
00778          GO TO 1410-CONTINUE.
00779
00780      IF WS-CANADIAN-ZIP
00781          IF WS-ZIP-4 = SPACE  OR  '-'
00782              MOVE WS-ZIP-CAN-2-POST1   TO PM-CAN-POST1
00783              MOVE WS-ZIP-CAN-2-POST2   TO PM-CAN-POST2
00784          ELSE
00785              MOVE WS-ZIP-CAN-1-POST1   TO PM-CAN-POST1
00786              MOVE WS-ZIP-CAN-1-POST2   TO PM-CAN-POST2
00787      ELSE
00788          IF WS-ZIP-6 = SPACE  OR  '-'
00789              MOVE WS-ZIP-AM-2-CODE     TO PM-ZIP-CODE
00790              MOVE WS-ZIP-AM-2-PLUS4    TO PM-ZIP-PLUS4
00791          ELSE
00792              MOVE WS-ZIP-AM-1-CODE     TO PM-ZIP-CODE
00793              MOVE WS-ZIP-AM-1-PLUS4    TO PM-ZIP-PLUS4.
00794
00795  1410-CONTINUE.
00796
00797      IF PHONEL  GREATER ZEROS
00798         MOVE WS-PHONE            TO  PM-PHONE-NO.
00799
00800
00801      
      * EXEC CICS WRITE
00802 *         DATASET   (FILE-ID-ERPNDM)
00803 *         FROM      (PENDING-MAILING-DATA)
00804 *         RIDFLD    (PM-CONTROL-PRIMARY)
00805 *    END-EXEC.
           MOVE LENGTH OF
            PENDING-MAILING-DATA
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00006203' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDM, 
                 PENDING-MAILING-DATA, 
                 DFHEIV11, 
                 PM-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00806
00807      MOVE 'Y'                    TO WS-ADD-ADDRESS-SW.
00808
00809      GO TO 1325-UPDATE-ISSUE-REC.
00810
00811  1485-REC-NOT-FOUND.
00812      MOVE -1                     TO MAINTL.
00813      MOVE ER-2239                TO EMI-ERROR.
00814      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00815      GO TO 8200-SEND-DATAONLY.
00816
121712
121712 1495-READ-CERT-TRAILER.
121712
121712     
      * EXEC CICS GETMAIN
121712*        SET     (ADDRESS OF CERTIFICATE-TRAILERS)
121712*        LENGTH  (ELCRTT-RECORD-LENGTH)
121712*        INITIMG (GETMAIN-SPACE)
121712*    END-EXEC
      *    MOVE ',"IL                  $   #00006222' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ELCRTT-RECORD-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF CERTIFICATE-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
121712
121712     MOVE +0                     TO  WS-CERT-TRL-REC-NOT-FOUND.
121712     MOVE PB-CONTROL-BY-ACCOUNT (1:33) TO  ELCRTT-PRIMARY.
121712     MOVE 'C'                    TO  ELCRTT-REC-TYPE.
121712
121712     
      * EXEC CICS HANDLE CONDITION
121712*        NOTFND (1495-CERT-TRL-REC-NOTFND)
121712*    END-EXEC.
      *    MOVE '"$I                   ! '' #00006232' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303036323332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
121712
121712     
      * EXEC CICS READ
121712*        DATASET  (CRTT-ID)
121712*        RIDFLD   (ELCRTT-KEY)
121712*        INTO     (CERTIFICATE-TRAILERS)
121712*    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-TRAILERS
             TO DFHEIV11
      *    MOVE '&"IL       E          (   #00006236' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CRTT-ID, 
                 CERTIFICATE-TRAILERS, 
                 DFHEIV11, 
                 ELCRTT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
121712
121712     GO TO 1495-EXIT.
121712
121712 1495-CERT-TRL-REC-NOTFND.
121712     MOVE +1                     TO WS-CERT-TRL-REC-NOT-FOUND.
121712
121712 1495-EXIT.
121712     EXIT.
121712
00817  EJECT
00818  7000-DISPLAY-ISSUES.
00819      MOVE EL631F                 TO PI-MAP-NAME.
00820      MOVE LOW-VALUES             TO EL631FI.
121713*     MOVE -1                     TO LASTNML.
121713     MOVE -1                     TO CBNAMEL.
00822
00823      
      * EXEC CICS HANDLE CONDITION
00824 *         NOTFND (7090-REC-NOT-FOUND)
00825 *    END-EXEC.
      *    MOVE '"$I                   ! ( #00006257' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303036323537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00826
00827      
      * EXEC CICS READ
00828 *         DATASET  (FILE-ID-ERPNDB)
00829 *         RIDFLD   (ERPNDB-KEY)
00830 *         SET      (ADDRESS OF PENDING-BUSINESS)
00831 *    END-EXEC.
      *    MOVE '&"S        E          (   #00006261' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDB, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00832
00833      MOVE PB-CERT-PRIME              TO CERTO.
00834      MOVE PB-CERT-SFX                TO SUFIXO.
00835      MOVE PB-CERT-EFF-DT             TO DC-BIN-DATE-1.
00836      MOVE SPACE                      TO DC-OPTION-CODE.
00837      PERFORM 9700-DATE-LINK.
00838
00839      MOVE DC-GREG-DATE-1-EDIT        TO EFFDTO.
00840
00841      MOVE PB-LAST-MAINT-BY           TO MAINTBYO.
00842      MOVE PB-LAST-MAINT-HHMMSS       TO WS-TIME.
00843      MOVE WS-HR-MINS                 TO MAINTATO.
00844      MOVE PB-INPUT-DT                TO DC-BIN-DATE-1.
00845      MOVE SPACE                      TO DC-OPTION-CODE.
00846      PERFORM 9700-DATE-LINK.
00847
00848      MOVE DC-GREG-DATE-1-EDIT        TO ENTERDTO.
00849      MOVE PB-INPUT-BY                TO ENTERBYO.
00850
00851      MOVE PB-BATCH-SEQ-NO            TO SEQO.
00852
00853      IF PB-CREDIT-ACCEPT-DT NOT = LOW-VALUES
00854         MOVE PB-CREDIT-ACCEPT-DT     TO DC-BIN-DATE-1
00855         MOVE SPACE                   TO DC-OPTION-CODE
00856         PERFORM 9700-DATE-LINK
00857         MOVE DC-GREG-DATE-1-EDIT     TO MAINTDTO
00858         MOVE 'INACTIVE'              TO MOENDDTO
00859      ELSE
00860         MOVE PB-LAST-MAINT-DT        TO DC-BIN-DATE-1
00861         MOVE SPACE                   TO DC-OPTION-CODE
00862         PERFORM 9700-DATE-LINK
00863         MOVE DC-GREG-DATE-1-EDIT     TO MAINTDTO
00864
00865      MOVE PB-CREDIT-SELECT-DT        TO DC-BIN-DATE-1.
00866      MOVE SPACE                      TO DC-OPTION-CODE.
00867      PERFORM 9700-DATE-LINK.
00868      MOVE DC-GREG-DATE-1-EDIT        TO MOENDDTO.
00869
00870      MOVE PB-I-INSURED-LAST-NAME     TO LASTNMO.
00871      MOVE PB-I-INSURED-FIRST-NAME    TO FIRSTNMO.
00872      MOVE PB-I-INSURED-MIDDLE-INIT   TO INITO.
00873      MOVE PB-I-SOC-SEC-NO            TO SOCSECO.
00874
00875      IF  PB-I-JOINT-AGE GREATER ZEROS
00876          MOVE PB-I-JOINT-AGE         TO JNTAGEO.
121712
121712     PERFORM 1495-READ-CERT-TRAILER THRU 1495-EXIT
121712     IF NOT CERT-TRL-REC-NOT-FOUND
121712         IF CS-JNT-AGE-DEFAULT-FLAG = 'Y'
121712             MOVE 'AGE*'             TO JAGEDEFO
121712         ELSE
121712             MOVE 'AGE '             TO JAGEDEFO
121712         END-IF
121712     ELSE
121712         MOVE 'AGE '             TO JAGEDEFO
121712     END-IF
           IF PB-I-JOINT-BIRTHDAY NOT = SPACES AND LOW-VALUES
              MOVE PB-I-JOINT-BIRTHDAY TO DC-BIN-DATE-1
              MOVE SPACES              TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK
              MOVE DC-GREG-DATE-1-EDIT     TO JNTDOBO
           END-IF
00878      IF  PB-I-JOINT-FIRST-NAME  GREATER SPACES
00879          MOVE PB-I-JOINT-FIRST-NAME  TO JNT1STO.
00880
00881      IF  PB-I-JOINT-MIDDLE-INIT GREATER SPACES
00882          MOVE PB-I-JOINT-MIDDLE-INIT TO JNTINITO.
00883
00884      IF  PB-I-JOINT-LAST-NAME   GREATER SPACES
00885          MOVE PB-I-JOINT-LAST-NAME   TO JNTLSTO.
00886
00887      IF  PB-I-BENEFICIARY-NAME  GREATER SPACES
00888          MOVE PB-I-BENEFICIARY-NAME  TO BENFARYO.
00889
00890      IF PB-I-POLICY-FORM-NO GREATER SPACES
00891          MOVE PB-I-POLICY-FORM-NO    TO POLFRMO.
00892
00893 *    IF PB-I-STATE-TAX NUMERIC
00894 *        MOVE PB-I-STATE-TAX         TO STTAXO.
00895
00896 *    IF PB-I-MUNI-TAX NUMERIC
00897 *        MOVE PB-I-MUNI-TAX          TO MUNITXO.
00898
00899      IF PB-I-RESIDENT-STATE GREATER SPACES
00900          MOVE PB-I-RESIDENT-STATE    TO RESSTO.
00901
00902      IF PB-I-RATE-CODE GREATER SPACES
00903          MOVE PB-I-RATE-CODE         TO RTCODEO.
00904
00905      IF PB-VALID-LIFE
00906          MOVE PB-I-LF-RATE           TO WS-RATE-OUT
00907          MOVE WS-XRATE-OUT           TO RATEO
00908        ELSE
00909          MOVE PB-I-AH-RATE           TO WS-RATE-OUT
00910          MOVE WS-XRATE-OUT           TO RATEO.
00911
00912      IF PB-VALID-LIFE
00913          MOVE PB-I-AH-RATE           TO WS-RATE-OUT
00914          MOVE WS-XRATE-OUT           TO PFRATEO
00915        ELSE
00916          MOVE PB-I-LF-RATE           TO WS-RATE-OUT
00917          MOVE WS-XRATE-OUT           TO PFRATEO.
00918
00919      IF PB-I-REIN-TABLE GREATER SPACES
00920         MOVE PB-I-REIN-TABLE         TO REINTBLO.
00921
00922      IF PB-I-RATE-DEVIATION-LF GREATER THAN SPACES
00923         MOVE PB-I-RATE-DEVIATION-LF  TO LFRTDEVO.
00924
00925      IF PB-I-RATE-DEV-PCT-LF GREATER ZEROS
00926         MOVE PB-I-RATE-DEV-PCT-LF    TO LFRTPRCO.
00927
00928      IF PB-I-RATE-DEVIATION-AH GREATER THAN SPACES
00929         MOVE PB-I-RATE-DEVIATION-AH  TO AHRTDEVO.
00930
00931      IF PB-I-RATE-DEV-PCT-AH GREATER ZEROS
00932         MOVE PB-I-RATE-DEV-PCT-AH    TO AHRTPRCO.
00933
00934      IF PB-I-LIFE-COMMISSION GREATER ZEROS
00935         MOVE PB-I-LIFE-COMMISSION    TO LFCOMMSO.
00936
00937      IF PB-I-AH-COMMISSION GREATER ZEROS
00938         MOVE PB-I-AH-COMMISSION      TO AHCOMMSO.
00939
00940      IF PB-ACCT-EFF-DT NOT = LOW-VALUES
00941         MOVE PB-ACCT-EFF-DT          TO DC-BIN-DATE-1
00942         MOVE SPACE                   TO DC-OPTION-CODE
00943         PERFORM 9700-DATE-LINK
00944         IF NO-CONVERSION-ERROR
00945            MOVE DC-GREG-DATE-1-EDIT  TO AEFFDTO.
00946
00947      IF PB-ACCT-EXP-DT = HIGH-VALUES
00948         MOVE 999999                  TO WS-EXP-DT-EDIT
00949         INSPECT WS-EXP-DT-EDIT CONVERTING SPACES TO '/'
00950         MOVE WS-EXP-DT-EDIT          TO AEXPDTO
00951      ELSE
00952         IF PB-ACCT-EXP-DT NOT = LOW-VALUES
00953            MOVE PB-ACCT-EXP-DT          TO DC-BIN-DATE-1
00954            MOVE SPACE                   TO DC-OPTION-CODE
00955            PERFORM 9700-DATE-LINK
00956            IF NO-CONVERSION-ERROR
00957               MOVE DC-GREG-DATE-1-EDIT  TO AEXPDTO.
00958
00959      IF PI-MAIL-YES
00960         NEXT SENTENCE
00961      ELSE
00962         MOVE AL-SANOF                TO ADDRS1A
00963                                         ADDRS2A
00964                                         ICITYA
                                              ISTATEA
00965                                         ZIPCDEA
00966                                         PHONEA
CIDMOD                                        CBNAMEA
CIDMOD                                        CBADDRA
CIDMOD                                        CBCITYA
                                              CBSTATEA
CIDMOD                                        CBZIPA
00967         GO TO 8100-SEND-INITIAL-MAP.
00968
00969      
      * EXEC CICS HANDLE CONDITION
00970 *         NOTFND (8100-SEND-INITIAL-MAP)
00971 *    END-EXEC.
      *    MOVE '"$I                   ! ) #00006425' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303036343235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00972
00973      
      * EXEC CICS READ
00974 *         DATASET  (FILE-ID-ERPNDM)
00975 *         RIDFLD   (ERPNDB-KEY)
00976 *         SET      (ADDRESS OF PENDING-MAILING-DATA)
00977 *    END-EXEC.
      *    MOVE '&"S        E          (   #00006429' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDM, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-MAILING-DATA TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00978
CIDMOD     IF PM-CRED-BENE-NAME > SPACES
CIDMOD        MOVE PM-CRED-BENE-NAME       TO CBNAMEO
CIDMOD     END-IF
CIDMOD
CIDMOD     IF PM-CRED-BENE-ADDR > SPACES
CIDMOD        MOVE PM-CRED-BENE-ADDR       TO CBADDRO
CIDMOD     END-IF
CIDMOD
CIDMOD     IF PM-CRED-BENE-CITY > SPACES
CIDMOD        MOVE PM-CRED-BENE-CITY       TO CBCITYO
CIDMOD     END-IF
CIDMOD     IF PM-CRED-BENE-STATE > SPACES
CIDMOD        MOVE PM-CRED-BENE-STATE      TO CBSTATEO
CIDMOD     END-IF
CIDMOD     IF PM-CRED-BENE-ZIP > SPACES
CIDMOD        MOVE SPACES              TO WS-ZIP-CODE
CIDMOD        IF PM-CB-CANADIAN-POST-CODE
CIDMOD           MOVE PM-CB-CAN-POST1  TO WS-ZIP-CAN-2-POST1
CIDMOD           MOVE PM-CB-CAN-POST2  TO WS-ZIP-CAN-2-POST2
CIDMOD           MOVE WS-ZIP-CODE      TO CBZIPO
CIDMOD        ELSE
CIDMOD           MOVE PM-CB-ZIP-CODE   TO WS-ZIP-AM-2-CODE
CIDMOD           MOVE WS-ZIP-CODE      TO CBZIPO
CIDMOD           IF PM-CB-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
CIDMOD              MOVE '-'           TO WS-ZIP-AM-2-DASH
CIDMOD              MOVE PM-CB-ZIP-PLUS4
CIDMOD                                 TO WS-ZIP-AM-2-PLUS4
CIDMOD              MOVE WS-ZIP-CODE   TO CBZIPO
CIDMOD           END-IF
CIDMOD        END-IF
CIDMOD     END-IF
01001
00979      IF PM-ADDRESS-LINE-1 GREATER SPACES
00980         MOVE PM-ADDRESS-LINE-1       TO ADDRS1O.
00981
00982      IF PM-ADDRESS-LINE-2 GREATER SPACES
00983         MOVE PM-ADDRESS-LINE-2       TO ADDRS2O.
00984
00985      IF PM-CITY           GREATER SPACES
00986         MOVE PM-CITY                 TO ICITYO.
00985      IF PM-STATE          GREATER SPACES
00986         MOVE PM-STATE                TO ISTATEO.
           IF PM-JOINT-BIRTH-DT NOT = LOW-VALUES
              MOVE PM-JOINT-BIRTH-DT   TO DC-BIN-DATE-1
              MOVE SPACE               TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT
                                       TO JNTDOBO
              END-IF
           END-IF
00988      IF PM-ZIP            GREATER SPACES
00989          MOVE SPACES               TO WS-ZIP-CODE
00990          IF PM-CANADIAN-POST-CODE
00991              MOVE PM-CAN-POST1     TO WS-ZIP-CAN-2-POST1
00992              MOVE PM-CAN-POST2     TO WS-ZIP-CAN-2-POST2
00993              MOVE WS-ZIP-CODE      TO ZIPCDEO
00994          ELSE
00995              MOVE PM-ZIP-CODE      TO WS-ZIP-AM-2-CODE
00996              MOVE WS-ZIP-CODE      TO ZIPCDEO
00997              IF PM-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
00998                  MOVE '-'          TO WS-ZIP-AM-2-DASH
00999                  MOVE PM-ZIP-PLUS4 TO WS-ZIP-AM-2-PLUS4
01000                  MOVE WS-ZIP-CODE  TO ZIPCDEO.
01001
01002      IF PM-PHONE-NO NUMERIC
01003         IF PM-PHONE-NO GREATER ZEROS
01004            MOVE PM-PHONE-NO          TO PHONEO
01005            INSPECT PHONEO CONVERTING ' ' TO '-'.
01006
01007      GO TO 8100-SEND-INITIAL-MAP.
01008
01009  7090-REC-NOT-FOUND.
01010      MOVE -1                     TO MAINTL.
01011      MOVE ER-2239                TO EMI-ERROR.
01012      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01013      GO TO 8100-SEND-INITIAL-MAP-CONT.
01014
01015      EJECT
01016
01017      EJECT
01018  7300-FORMAT-ERRORS.
01019
01020      IF PB-COMMON-ERRORS = LOW-VALUES
01021         IF PB-FATAL-ERRORS
01022            MOVE ER-2695          TO EMI-ERROR
01023            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01024            GO TO 7399-EXIT.
01025
01026      MOVE +0                     TO WS-SUB4.
01027
01028  7310-ERROR-LOOP.
01029
01030      ADD +1                      TO WS-SUB4.
01031
01032      IF WS-SUB4 GREATER THAN PB-NO-OF-ERRORS
01033         GO TO 7350-SET-ERROR-FLAGS.
01034
01035      MOVE PB-COMMON-ERROR (WS-SUB4) TO EMI-ERROR.
01036      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01037
01038      GO TO 7310-ERROR-LOOP.
01039
01040  7350-SET-ERROR-FLAGS.
01041
01042      IF EMI-FATAL-CTR NOT = ZEROS
01043         MOVE 'Y'                 TO WS-ERRORS-PRESENT-SW
01044         MOVE 'X'                 TO PB-FATAL-FLAG
01045         GO TO 7355-SET-ERROR-FLAGS.
01046
01047      IF EMI-FORCABLE-CTR NOT = ZEROS
01048         MOVE 'Y'                 TO WS-ERRORS-PRESENT-SW
01049         IF PB-ISSUE
01050            IF PB-ISSUE-FORCE
01051               MOVE 'F'           TO PB-FORCE-ER-CD
01052               MOVE ER-2600       TO EMI-ERROR
01053               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01054              ELSE
01055               MOVE 'X'           TO PB-FORCE-ER-CD
01056           ELSE
01057            IF PB-CANCEL-FORCE
01058               MOVE 'F'           TO PB-FORCE-ER-CD
01059               MOVE ER-2600       TO EMI-ERROR
01060               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01061              ELSE
01062               MOVE 'X'           TO PB-FORCE-ER-CD.
01063
01064  7355-SET-ERROR-FLAGS.
01065
01066      IF EMI-WARNING-CTR NOT = ZEROS
01067         MOVE 'Y'                 TO WS-ERRORS-PRESENT-SW
01068         MOVE 'W'                 TO PB-WARN-ER-CD.
01069
01070      IF PB-UNFORCED-ERRORS OR
01071         PB-FATAL-ERRORS    OR
01072         PB-RECORD-ON-HOLD  OR
01073         PB-RECORD-RETURNED OR
01074         PB-CANCELLATION
01075           NEXT SENTENCE
01076         ELSE
01077           GO TO 7399-EXIT.
01078
01079      
      * EXEC CICS  HANDLE CONDITION
01080 *           NOTFND   (7399-EXIT)
01081 *    END-EXEC.
      *    MOVE '"$I                   ! * #00006577' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303036353737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01082
01083      MOVE PB-CONTROL-BY-ACCOUNT  TO ELCERT-KEY.
01084      MOVE PB-SV-CARRIER          TO ELCERT-CARRIER.
01085      MOVE PB-SV-GROUPING         TO ELCERT-GROUPING.
01086      MOVE PB-SV-STATE            TO ELCERT-STATE.
01087
01088      
      * EXEC CICS READ
01089 *        SET     (ADDRESS OF CERTIFICATE-MASTER)
01090 *        DATASET (FILE-ID-ELCERT)
01091 *        RIDFLD  (ELCERT-KEY)
01092 *        UPDATE
01093 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00006586' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ELCERT, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01094
01095      MOVE CM-CREDIT-INTERFACE-SW-1         TO WS-SW-1
01096      MOVE CM-CREDIT-INTERFACE-SW-2         TO WS-SW-2
01097
01098      IF (CERT-ADDED-BATCH OR CERT-PURGED-OFFLINE) AND PB-ISSUE
01099         GO TO 7360-REWRITE-CERT-MASTER.
01100
01101      IF PB-ISSUE
01102         IF PB-RECORD-RETURNED
01103             MOVE '4'  TO CM-CREDIT-INTERFACE-SW-1
01104             GO TO 7360-REWRITE-CERT-MASTER
01105         ELSE
01106             MOVE '2' TO CM-CREDIT-INTERFACE-SW-1
01107             GO TO 7360-REWRITE-CERT-MASTER.
01108
01109      IF PB-RECORD-RETURNED
01110          MOVE '7'      TO CM-CREDIT-INTERFACE-SW-2
01111          GO TO 7360-REWRITE-CERT-MASTER.
01112
01113      IF PB-C-LF-CANCEL-VOIDED
01114         IF (PB-UNFORCED-ERRORS OR
01115             PB-FATAL-ERRORS    OR
01116             PB-RECORD-ON-HOLD)
01117             MOVE '6'             TO CM-CREDIT-INTERFACE-SW-2
01118             GO TO 7360-REWRITE-CERT-MASTER
01119         ELSE
01120             MOVE '5'             TO CM-CREDIT-INTERFACE-SW-2
01121             GO TO 7360-REWRITE-CERT-MASTER.
01122
01123      IF (PB-UNFORCED-ERRORS OR PB-FATAL-ERRORS
01124                             OR PB-RECORD-ON-HOLD)
01125         MOVE '4'                 TO CM-CREDIT-INTERFACE-SW-2
01126      ELSE
01127         MOVE '1'                 TO CM-CREDIT-INTERFACE-SW-2.
01128
01129  7360-REWRITE-CERT-MASTER.
01130      IF WS-SW-1 = CM-CREDIT-INTERFACE-SW-1 AND
01131         WS-SW-2 = CM-CREDIT-INTERFACE-SW-2
01132         
      * EXEC CICS UNLOCK
01133 *            DATASET    (FILE-ID-ELCERT)
01134 *       END-EXEC
      *    MOVE '&*                    #   #00006630' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ELCERT, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01135         GO TO 7399-EXIT.
01136
01137      
      * EXEC CICS REWRITE
01138 *         DATASET    (FILE-ID-ELCERT)
01139 *         FROM       (CERTIFICATE-MASTER)
01140 *    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00006635' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ELCERT, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01141
01142  7399-EXIT.
01143       EXIT.
01144
01145      EJECT
01146  8100-SEND-INITIAL-MAP.
01147      PERFORM 7300-FORMAT-ERRORS  THRU 7399-EXIT.
01148
01149  8100-SEND-INITIAL-MAP-CONT.
01150      MOVE EIBTIME                TO TIME-IN.
01151
01152      MOVE WS-LFRTHDG             TO LFRTHDGO.
01153      MOVE WS-AHRTHDG             TO AHRTHDGO.
01154      MOVE WS-LFDVHDG             TO LFDVHDGO.
01155      MOVE WS-AHDVHDG             TO AHDVHDGO.
01156      MOVE WS-LFCMHDG             TO LFCMHDGO.
01157      MOVE WS-AHCMHDG             TO AHCMHDGO.
01158      MOVE WS-CURRENT-DT          TO DATEO.
01159      MOVE TIME-OUT               TO TIMEO.
01160      MOVE EMI-MESSAGE-AREA (1)   TO ERMSG1O.
01161      MOVE EMI-MESSAGE-AREA (2)   TO ERMSG2O.
01162      MOVE PB-ENTRY-BATCH         TO BATCHO.
01163      MOVE PB-CARRIER             TO CARRO.
01164      MOVE PB-GROUPING            TO GROUPO.
01165      MOVE PB-ACCOUNT             TO ACCTO.
01166      MOVE PB-STATE               TO STATEO.
01167
           IF CSR-EDIT-SESSION
              MOVE '- CUSTOMER SERVICE REVIEW/CORRECTION -'
                                       TO HEADO
           END-IF
100217     move pi-company-id        to compido
100217     move function upper-case(ws-kix-myenv)
100217                               to sysenvo
100217     move pi-processor-id      to procido
01168 *    IF PI-COMPANY-ID = 'DMD'
01169 *       MOVE AL-SANOF          TO LIT1A LIT2A STTAXA MUNITXA
01170 *       MOVE AL-SANON          TO RATEA
01171 *       MOVE AL-UANOF          TO PFRATEA RTCODEA RESSTA.
121713
121713***DO NOT ALLOW PRIMARY NAME TO CHANGE ON THIS SCREEN
121713     MOVE AL-SANOF             TO LASTNMA FIRSTNMA INITA.
01172
01173      
      * EXEC CICS SEND
01174 *        MAP      (PI-MAP-NAME)
01175 *        MAPSET   (MAPSET-EL6313S)
01176 *        FROM     (EL631FI)
01177 *        ERASE
01178 *        CURSOR
01179 *    END-EXEC.
           MOVE LENGTH OF
            EL631FI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00006682' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 EL631FI, 
                 DFHEIV12, 
                 MAPSET-EL6313S, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01180
01181      GO TO 9100-RETURN-TRAN.
01182
01183      EJECT
01184
01185  8200-SEND-DATAONLY.
01186      MOVE WS-CURRENT-DT          TO DATEO.
01187      MOVE EIBTIME                TO TIME-IN.
01188      MOVE TIME-OUT               TO TIMEO.
01189      MOVE EMI-MESSAGE-AREA (1)   TO ERMSG1O.
01190      MOVE EMI-MESSAGE-AREA (2)   TO ERMSG2O.
01191
01192 *    IF PI-COMPANY-ID = 'DMD'
01193 *       MOVE AL-SANOF          TO LIT1A LIT2A STTAXA MUNITXA
01194 *                                 RATEA
01195 *       MOVE AL-UANOF          TO PFRATEA RTCODEA RESSTA.
121713
121713***DO NOT ALLOW PRIMARY NAME TO CHANGE ON THIS SCREEN
121713     MOVE AL-SANOF             TO LASTNMA FIRSTNMA INITA.
01196
01197      
      * EXEC CICS SEND
01198 *         MAP      (PI-MAP-NAME)
01199 *         MAPSET   (MAPSET-EL6313S)
01200 *         FROM     (EL631FI)
01201 *         DATAONLY
01202 *         CURSOR
01203 *    END-EXEC.
           MOVE LENGTH OF
            EL631FI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00006709' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 EL631FI, 
                 DFHEIV12, 
                 MAPSET-EL6313S, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01204
01205      GO TO 9100-RETURN-TRAN.
01206
01207      EJECT
01208
01209  8300-SEND-TEXT.
01210      
      * EXEC CICS SEND TEXT
01211 *        FROM     (LOGOFF-TEXT)
01212 *        LENGTH   (LOGOFF-LENGTH)
01213 *        ERASE
01214 *        FREEKB
01215 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00006722' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373232' TO DFHEIV0(25:11)
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
           
01216
01217      
      * EXEC CICS RETURN
01218 *    END-EXEC
      *    MOVE '.(                    ''   #00006729' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
043120 8400-LOG-JOURNAL-RECORD.
043120
043120     display ' made it to 8400- '
043120
043120     if (pi-journal-file-id > 0)
043120        and (jp-file-id = file-id-erpndb)
043120        move eibdate             to jp-date
043120        move eibtime             to jp-time
043120*       move FILE-ID-ERPNDB      TO JP-FILE-ID
043120        MOVE PI-PROCESSOR-ID     TO JP-USER-ID
043120        MOVE 03                  TO PI-JOURNAL-FILE-ID
043120        if csr-edit-session
043120           move 'EL6311C'        to jp-program-id
043120        else
043120           MOVE THIS-PGM         TO JP-PROGRAM-ID
043120        end-if
043120
043120**      length is 585 plus 30 extra for jrnl stuff
043120**      system already accounts for the 34
043120
043120        
      * EXEC CICS JOURNAL
043120*          JFILEID   (PI-JOURNAL-FILE-ID)
043120*          JTYPEID   ('EL')
043120*          FROM      (JOURNAL-RECORD)
043120*          LENGTH    (615)
043120*          resp      (ws-response)
043120*       END-EXEC
           MOVE 'EL' TO DFHEIV7
           MOVE 615
             TO DFHEIV11
      *    MOVE '4"LF                  (  N#00006752' TO DFHEIV0
           MOVE X'34224C462020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303036373532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-JOURNAL-FILE-ID, 
                 DFHEIV7, 
                 JOURNAL-RECORD, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
043120
043120        if resp-normal
043120           continue
043120        else
043120           display ' error-el6313-journal ' ws-response
043120        end-if
043120     end-if
043120     .
043120 8400-exit.
043120     exit.
01219  8600-DEEDIT.
01220      
      * EXEC CICS BIF DEEDIT
01221 *        FIELD   (DEEDIT-FIELD)
01222 *        LENGTH  (15)
01223 *    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00006770' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01224
01225  8600-EXIT.
01226      EXIT.
01227
01228  8800-UNAUTHORIZED-ACCESS.
01229      MOVE UNACCESS-MSG           TO LOGOFF-MSG.
01230      GO TO 8300-SEND-TEXT.
01231
01232  8810-PF23.
01233      MOVE EIBAID                 TO PI-ENTRY-CD-1.
01234      MOVE XCTL-EL005             TO PGM-NAME.
01235      GO TO 9300-XCTL.
01236
01237  9100-RETURN-TRAN.
01238      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
01239      MOVE SCREEN-NUMBER          TO PI-CURRENT-SCREEN-NO.
01240      
      * EXEC CICS RETURN
01241 *        TRANSID    (TRANS-EXB3)
01242 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
01243 *        LENGTH     (1536)
01244 *    END-EXEC.
           MOVE 1536
             TO DFHEIV11
      *    MOVE '.(CT                  ''   #00006790' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-EXB3, 
                 PROGRAM-INTERFACE-BLOCK, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01245
01246  9200-RETURN-MAIN-MENU.
01247      MOVE XCTL-EL626             TO PGM-NAME.
01248      GO TO 9300-XCTL.
01249
01250  9300-XCTL.
01251      
      * EXEC CICS XCTL
01252 *        PROGRAM    (PGM-NAME)
01253 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
01254 *        LENGTH     (1536)
01255 *    END-EXEC.
           MOVE 1536
             TO DFHEIV11
      *    MOVE '.$C                   %   #00006801' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01256
01257  9400-CLEAR.
01258      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME
01259      GO TO 9300-XCTL.
01260
01261  9500-PF12.
01262      MOVE XCTL-EL010             TO PGM-NAME.
01263      GO TO 9300-XCTL.
01264
01265  9600-PGMID-ERROR.
01266      
      * EXEC CICS HANDLE CONDITION
01267 *        PGMIDERR    (8300-SEND-TEXT)
01268 *    END-EXEC.
      *    MOVE '"$L                   ! + #00006816' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303036383136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01269
01270      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
01271      MOVE ' '                    TO PI-ENTRY-CD-1.
01272      MOVE XCTL-EL005             TO PGM-NAME.
01273      MOVE PGM-NAME               TO LOGOFF-PGM.
01274      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
01275      GO TO 9300-XCTL.
01276
01277  9700-DATE-LINK.
01278      
      * EXEC CICS LINK
01279 *        PROGRAM  (LINK-ELDATCV)
01280 *        COMMAREA (DATE-CONVERSION-DATA)
01281 *        LENGTH   (DC-COMM-LENGTH)
01282 *    END-EXEC.
      *    MOVE '."C                   (   #00006828' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LINK-ELDATCV, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01283
01284  9700-EXIT.
01285      EXIT.
01286
01287  9800-LINK-PENDING-EDIT.
01288      MOVE PI-LIFE-OVERRIDE-L1    TO PB-LIFE-OVERRIDE-L1.
01289      MOVE PI-AH-OVERRIDE-L1      TO PB-AH-OVERRIDE-L1.
01290
01291      MOVE LINK-EL050             TO PGM-NAME.
01292      MOVE PENDING-BUSINESS       TO PASSED-RECORD.
01293      MOVE ZEROS                  TO WK-CO-MAX-CAP
01294                                     WK-CO-TOL-CLAIM
01295                                     WK-CO-TOL-PREM
01296                                     WK-CO-TOL-REFUND
01297                                     WK-CO-TOL-PREM-PCT
01298                                     WK-CO-TOL-REFUND-PCT
01299                                     WK-REFUND-OVS-AMT
01300                                     WK-REFUND-OVS-PCT
01301                                     WK-ACCT-ADDR
01302                                     WK-LIFE-EDIT-ADDR
01303                                     WK-AH-EDIT-ADDR
01304                                     WK-LIFE-BEN-ADDR
01305                                     WK-AH-BEN-ADDR
01306                                     WK-STATE-ADDR
01307                                     WK-PLAN-ADDR
01308                                     WK-FORM-ADDR.
01309
01310      MOVE '6'                    TO WK-ENTRY-SW.
01311
01312      MOVE SPACES                 TO EDIT-CRITERIA-DATA.
01313
01314      MOVE LOW-VALUES             TO EC-CO-MONTH-END-DT
01315                                     EC-AM-EXPIRATION-DT
01316                                     EC-AM-EFFECTIVE-DT
01317                                     EC-CM-LF-CANCEL-DT
01318                                     EC-CM-AH-CANCEL-DT
01319                                     EC-CM-DEATH-DT.
01320
01321      MOVE ZEROS                  TO EC-CO-TOL-PREM
01322                                     EC-CO-TOL-REFUND
01323                                     EC-CO-MIN-AGE
01324                                     EC-CO-MIN-PREMIUM
01325                                     EC-CO-MIN-TERM
01326                                     EC-CO-MAX-TERM
01327                                     EC-ST-TOL-PREM
01328                                     EC-ST-TOL-REFUND
01329                                     EC-AM-LF-TOL-PREM
01330                                     EC-AM-AH-TOL-PREM
01331                                     EC-AM-LF-MAX-ATT-AGE
01332                                     EC-AM-LF-MAX-AGE
01333                                     EC-AM-LF-MAX-TERM
01334                                     EC-AM-LF-MAX-TOT-BEN
01335                                     EC-AM-AH-MAX-ATT-AGE
01336                                     EC-AM-AH-MAX-AGE
01337                                     EC-AM-AH-MAX-TERM
01338                                     EC-AM-AH-MAX-TOT-BEN
01339                                     EC-AM-AH-MAX-MON-BEN
01340                                     EC-RT-LF-MAX-ATT-AGE
01341                                     EC-RT-LF-MAX-AGE
01342                                     EC-RT-LF-MAX-TERM
01343                                     EC-RT-LF-MAX-TOT-BEN
01344                                     EC-RT-AH-MAX-ATT-AGE
01345                                     EC-RT-AH-MAX-AGE
01346                                     EC-RT-AH-MAX-TERM
01347                                     EC-RT-AH-MAX-TOT-BEN
01348                                     EC-RT-AH-MAX-MON-BEN
01349                                     EC-RT-AH-RATE
01350                                     EC-RT-LF-RATE
01351                                     EC-RT-LF-NSP-RATE
01352                                     EC-RT-AH-NSP-RATE
01353                                     EC-CM-LF-PRIOR-REFUND
01354                                     EC-CM-AH-PRIOR-REFUND.
01355
01356      
      * EXEC CICS LINK
01357 *         PROGRAM   (PGM-NAME)
01358 *         COMMAREA  (PASSED-RECORD)
01359 *         LENGTH    (WS-EDIT-PASS-LENGTH)
01360 *     END-EXEC.
      *    MOVE '."C                   (   #00006906' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PASSED-RECORD, 
                 WS-EDIT-PASS-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01361
01362      MOVE '1'                    TO PI-EDIT-SW.
01363      MOVE EDIT-CRITERIA-DATA     TO PI-CRITERIA-DATA.
01364
01365      MOVE PASSED-RECORD          TO PENDING-BUSINESS.
01366
01367      IF VALID-PB-ID
01368         GO TO 9800-EXIT.
01369
01370      MOVE PENDING-BUSINESS       TO EMI-LINE1.
01371
01372      MOVE -1                     TO MAINTL.
01373
01374      GO TO 8200-SEND-DATAONLY.
01375
01376  9800-EXIT.
01377       EXIT.
01378
01379  9900-ERROR-FORMAT.
01380      MOVE 2                      TO EMI-NUMBER-OF-LINES
01381      MOVE PI-COMPANY-ID          TO EMI-CLIENT-ID.
01382
01383      IF NOT EMI-ERRORS-COMPLETE
01384          MOVE LINK-EL001         TO PGM-NAME
01385          
      * EXEC CICS LINK
01386 *            PROGRAM    (PGM-NAME)
01387 *            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
01388 *            LENGTH     (EMI-COMM-LENGTH)
01389 *        END-EXEC.
      *    MOVE '."C                   (   #00006935' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01390
01391  9900-EXIT.
01392      EXIT.
01393
01394  9990-ABEND.
01395      MOVE LINK-EL004             TO PGM-NAME.
01396      MOVE DFHEIBLK               TO EMI-LINE1
01397      
      * EXEC CICS LINK
01398 *        PROGRAM   (PGM-NAME)
01399 *        COMMAREA  (EMI-LINE1)
01400 *        LENGTH    (72)
01401 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00006947' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01402
01403       MOVE -1                    TO FPFENTRL
01404
01405      GO TO 8200-SEND-DATAONLY.
01406
01407  9995-SECURITY-VIOLATION.
01408 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00006975' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393735' TO DFHEIV0(25:11)
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
01409

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6313' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9600-PGMID-ERROR,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 1400-ADD-MAIL-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 1385-REC-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 1386-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 1485-REC-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 1495-CERT-TRL-REC-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 7090-REC-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 8100-SEND-INITIAL-MAP
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 7399-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6313' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
