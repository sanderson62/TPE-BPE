      $SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL142 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 06/29/95 10:53:47.
00007 *                            VMOD=2.033
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
00025 *        THIS PROGRAM PROVIDES THE FUNCTIONS TO BROWSE THRU AND
00026 *    PERFORM MAINTENANCE ON ACTIVITY TRAILERS.  THE TRAILERS ARE
00027 *    CREATED BY OTHER VARIOUS FUNCTIONS.
00028
00029 *    TRANS ID = EX25
00030
00031 *    SCREENS     - EL142A - BROWSE QUALIFICATION
00032 *                  EL142B - PAYMENTS
00033 *                  EL142C - AUTOMATIC PAYMENTS
00034 *                  EL142D - LETTERS
00035 *                  EL142E - GENERAL INFORMATION
00036 *                  EL142F - REMINDERS
00037 *                  EL142G - DENIAL RECORD
00038 *                  EL142H - RESERVES & EXPENSES
00039 *                  EL142I - INCURED CHANGE INFORMATION
00040 *                  EL142J - FORM INFORMATION
00041
00042 *    ENTERED BY  - EL131  - CLAIM MAINTENANCE
00043 *                  EL150  - STATUS
00044 *                  EL1622 - CLAIM AUDIT
00045
00046 *    EXIT TO     - CALLING PROGRAM
00047
00048 *    INPUT FILES - ELTRLR - ACTIVITY TRAILERS
00049 *                  ELMSTR - CLAIM MASTER
00050
00051 *    OUTPUT FILES - ELTRLR - ACTIVITY TRAILERS
00052 *                   ELMSTR - CLAIM MASTER
00053
00054 *    COMMAREA    - PASSED.  WHEN CALLED BY EL150, A 4 BYTE
00055 *                  CHARACTER, TRAILER SEQUENCE NUMBER, IS PASSED
00056 *                  IN THE FIRST 4 BYTES OF THE PROGRAM-WORK-AREA
00057 *                  OF THE INTERFACE BLOCK.
00058
00059 *    NARRATIVE   - ON FIRST ENTRY, THE BROWSE QUALIFICATION SCREEN
00060 *                  IS SENT.  IF THE ENTRY CAME FROM EL150, THE
00061 *                  PASSED SEQUENCE NUMBER IS PLACED IN THE "START-
00062 *                  ING SEQUENCE NUMBER FIELD" SO THAT THE OPERATOR
00063 *                  HAS THE OPTION OF STARTING AT THE SAME POINT AS
00064 *                  THE STATUS DISPLAY.
00065
00066 *                  VIA THE QUALIFICATION SCREEN THE OPERATOR
00067 *                  INDICATES WHAT TYPE OF TRAILERS ARE TO BE
00068 *                  VIEWED, THE EARLIEST ACTIVITY AND A STARTING
00069 *                  SEQUENCE NUMBER.  THE DATE IS OPTIONAL, SPACE
00070 *                  IMPLIES ALL DATES.  IF THE SEQUENCE NUMBER IS
00071 *                  NOT GIVEN, ZERO IS ASSUMED.  THE TRAILER FILE
00072 *                  IS ALWAYS READ FORWARD, SO THAT ACTIVITY WILL
00073 *                  BE SHOWN AS MOST RECENT FIRST.  THE QUALIFICA-
00074 *                  TIONS ARE SAVED IN THE PROGRAM WORK AREA OF THE
00075 *                  INTERFACE BLOCK AND THE FIRST QUALIFYING
00076 *                  TRAILER IS READ.
00077
00078 *                  THE DISPLAY AND MAINTENANCE TYPE DEPENDS ON THE
00079 *                  TYPE OF TRAILER RECORD READ.  SCREENS EL142B
00080 *                  THRU EL142H ARE USED DEPENDING ON THE RECORD
00081 *                  TYPE.
062602******************************************************************
062602*                   C H A N G E   L O G
062602*
062602* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
062602*-----------------------------------------------------------------
062602*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
062602* EFFECTIVE    NUMBER
062602*-----------------------------------------------------------------
062602* 062602    2002030700006  PEMA  Add note type of 'S'
062602*                                  (special review)
022106* 022106    2004040700004  PEMA  ADD LIFE CLAIM INTEREST CALC
052506* 052506    2006030600001  AJRA  ADD PROOF DT TO PAYMENT & DENIAL
082807* 082807    2007032100001  PEMA  ADD INT RATE TO PMT TRLR
101807* 101807    2007100100007  PEMA  INCREASE CLM RESERVE FIELDS
031808* 031808    2006032200004  AJRA  ADD APPROVED BY TO PAYMENT
070909* 070909    2009060400001  AJRA  ADD AUTO PAY END LETTER
033010* 033010  CR2008100900001  PEMA  ADD DENIAL CODE EDITS
042110* 042110  CR2008100900001  PEMA  ADD DELETE PROCESSING FOR MAPG
050110* 050110    2009122800001  AJRA  ADD NAPERSOFT
100610* 100610    2009122800001  AJRA  LOCK RECV DT W/ RESEND PRINTED DT
102510* 102510    2009122800001  AJRA  UPDATE ELNAPS WHEN RESEND DT CHGD
102610* 102510    2009122800001  AJRA  ADD STOP DATE TO LETTER TRAILER
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
020413* 020413    2012071700001  AJRA  PRINT SURVEY AND PRINT CLM FORM I
041613* 041613    2013040400004  AJRA  ADD ENC CODE FOR SPECIAL HANDLING
091813* 091813    2013082900001  AJRA  ADD APPROVAL LEVEL 5
102413* 102413    2013100800001  AJRA  ADD SPEC RELEASE, FIX ENX ENC COD
111113* 111113    2013110600002  AJRA  CHG LEVEL 5 RESTRICTIONS TO LEVEL
021114* 021114    2014012100002  AJRA  ADD CHECK CASHED DATE
081214* 081214  IR2014081100001  PEMA  CORRECT INT CHK CASHED DT PROCESS
091714* 091714  IR2014090800003  PEMA  ADD "manual" table to query
111714* 111714  CR2014073000001  PEMA  DRAFTS TO CHECKS
051215* 051215  IR2015051100002  PEMA  Correct bogus cashed date
060315* 060315  IR2015060200004  PEMA  Correct cut-off date
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
013017* 013017  CR2017022000001  PEMA  DRAFTS TO CHECKS - DCC
120718* 120718  CR2018120400002  PEMA  REMOVE USER HARDCODING
040819* 040819  IR2019030400004  TANA  PROTECT HOLD UNTIL DATE FIELD
043019* 043019  IR2019042300001  PEMA  DISALLOW PROOF DT > REC DT OR < I
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
011822* 011822  CR2019012500003  PEMA  Convert to SQLSERVER 2016
062602******************************************************************
00082
00083      EJECT
00084  ENVIRONMENT DIVISION.
00085
00086  DATA DIVISION.
00087
00088  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00089  77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.
       77  ws-sql-code                 pic s9(7) value zeros.
       77  ws-dis-sql-code             pic -9999999 value zeros.
00090  01  LCP-CURRENT-DATE-68.
00091      05  LCP-MONTH                 PIC X(2).
00092      05  FILLER                    PIC X VALUE '/'.
00093      05  LCP-DAY1                  PIC X(2).
00094      05  FILLER                    PIC X VALUE '/'.
00095      05  LCP-YEAR                  PIC X(2).
00096  01  LCP-CICS-DATE                 PIC 9(15).
00097
00098  77  FILLER  PIC X(32)  VALUE '********************************'.
00099  77  FILLER  PIC X(32)  VALUE '*   EL142  WORKING STORAGE     *'.
00100  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.033 *********'.
021114
021114 EXEC SQL
021114    INCLUDE SQLDA
021114 END-EXEC
021114
021114 EXEC SQL
021114    INCLUDE SQLCA
021114 END-EXEC
021114
021114 EXEC SQL
021114    BEGIN DECLARE SECTION
021114 END-EXEC
021114
021114 01  SQLCMD                      PIC X(1024).
021114 01  SVR                         PIC X(32).
021114 01  USR                         PIC X(32).
021114 01  PASS                        PIC X(32).
021114 01  USR-PASS                    PIC X(64).
021114
111714 01  ws-sql-manual-data.
111714     05  ws-check-number-man     pic x(7).
111714     05  ws-check-company-man    pic xxx.
111714
021114 01  WS-SQL-DATA.
111714     05  ws-draft-or-check       pic x.
021114     05  WS-CHECK-TYPE           PIC X.
111714     05  WS-CHECK-NUMBER         PIC X(10).
111714     05  ws-claim-number         pic x(7).
021114     05  WS-CHECK-AMOUNT         PIC X(10).
111714     05  WS-CHECK-COMPANY        PIC X(5).
111714     05  WS-CHECK-CASHED-DT      PIC X(20).
021114
       01  ws-ach-data.
           05  ws-carrier              pic x.
           05  ws-state                pic xx.
           05  ws-account-no           pic x(10).
           05  ws-cert-no              pic x(11).
           05  ws-claim-no             pic x(7).
           05  ws-check-no             pic x(10).
           05  ws-cashed-date          pic x(10).
021114 EXEC SQL
021114    END DECLARE SECTION
021114 END-EXEC
021114
00101
00102 *                                    COPY ELCSCTM.
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
00103
00104 *                                    COPY ELCSCRTY.
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
00105
00106 *                                    COPY ELCDCTB.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCDCTB.                            *
00004 *                            VMOD=2.002                          *
00005 *                                                                *
00006 *   DESCRIPTION = DISTRIBUTION CONTROL TABLE MAINTENANCE PROGRAM *
00007 *       COMMUNICATIONS AREA                                      *
00008 *                                                                *
00009 ******************************************************************
00010  01  DCT-COMMUNICATION-AREA.
00011      12  DCT-BILLING-BANK-ID      PIC  X(05).
00012      12  DCT-LOGIC-BENEFICIARY-ID PIC  X(10).
00013      12  DCT-CREDIT-CARD-NUMBER   PIC  X(16).
00014      12  DCT-PRODUCT-CODE         PIC  X(02).
00015      12  DCT-COLUMN-ID-REQUESTED  PIC  X(02).
00016      12  DCT-RETURN-CODE          PIC  X(02).
00017      12  DCT-MAIL-CODE            PIC  X(05).
00018      12  DCT-DISTRIBUTION-CODE    PIC  X(04).
00019      12  DCT-MSA-ACCT-NO          PIC  X(07).
00107
00108 *                                    COPY ELCNWA.
00001 *****************************************************************
00002 *                                                               *
00002 *                                                               *
00003 *                            ELCNWA.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                         *
00006 *                                                               *
00007 *            M O V E   N A M E   W O R K   A R E A.             *
00008 *                                                               *
00009 *****************************************************************.
00010
00011  01  WS-NAME-WORK-AREA.
00012      05  WS-INSURED-LAST-NAME        PIC X(15).
00013      05  WS-INSURED-1ST-NAME         PIC X(12).
00014      05  WS-INSURED-MID-INIT         PIC X.
00015
00016      05  WS-NAME-WORK.
00017          10  WS-NW                   PIC X
00018              OCCURS 30 TIMES INDEXED BY NWA-INDEX.
00019
00020      05  WS-NAME-WORK2.
00021          10  WS-NW2                  PIC X
00022              OCCURS 20 TIMES INDEXED BY NWA-INDEX2 NWA-INDEX3
00023                                         NWA-INDEX0.
00024
00025      05  WS-NAME-SW                  PIC S9          VALUE ZERO
00026                                      COMP-3.
00027
00109
020810 01  WS-ELDENY-KEY.
020810     05  ELDENY-COMPANY-CD   PIC X.
020810     05  ELDENY-DENIAL-CODE  PIC X(4).
020810     05  FILLER              PIC X(10).
020810 01  WS-RESPONSE             PIC S9(8)   COMP.
020810     88  RESP-NORMAL              VALUE +00.
020810     88  RESP-ERROR               VALUE +01.
020810     88  RESP-NOTFND              VALUE +13.
020810     88  RESP-NOTOPEN             VALUE +19.
020810     88  RESP-ENDFILE             VALUE +20.
00110  01  FILLER                          COMP-3.
00111      05  WS-RECORD-COUNT             PIC S9(5)       VALUE ZERO.
00112      05  WS-RECORD-REMAINDER         PIC S9(5)       VALUE ZERO.
00113      05  WS-RECORD-DIV               PIC S9(5)       VALUE ZERO.
00114      05  WS-READNEXT-SW              PIC S9          VALUE ZERO.
00115      05  WS-NOT-FOUND                PIC S9          VALUE ZERO.
00116      05  WS-ERROR-COUNT              PIC S9(3)       VALUE ZERO.
00117      05  WS-UPDATE-SW                PIC S9          VALUE ZERO.
00118      05  WS-COMPLETED-SUCCESSFUL     PIC S9          VALUE ZERO.
00119        88  TRANSACTION-SUCCESSFUL                    VALUE +1.
00120
00121      05  TIME-IN                     PIC S9(7)       VALUE ZERO.
00122      05  TIME-OUT                    REDEFINES
00123          TIME-IN                     PIC S9(3)V9(4).
00124
00125      05  WS-HPCTCDTI                 PIC S9(3)V99 VALUE ZERO.
101807     05  WS-HMANAMTI                 PIC S9(7)V99 VALUE ZERO.
00127      05  WS-HEXPAMTI                 PIC S9(5)V99 VALUE ZERO.
00128
00129  01  FILLER                          COMP  SYNC.
00130      05  SC-ITEM                     PIC S9(4)       VALUE +0001.
00131      05  WS-INDEX                    PIC S9(4)       VALUE ZERO.
00132
00133      05  WS-ACTIVITY-TRAILERS-LENGTH PIC S9(4)       VALUE +200.
00134      05  WS-CHECK-QUEUE-LENGTH       PIC S9(4)       VALUE +100.
00135      05  WS-DEEDIT-LENGTH            PIC S9(4)       VALUE +8.
00136      05  WS-HPCTCDT-LENGTH           PIC S9(4)       VALUE +7.
101807     05  WS-HMANAMT-LENGTH           PIC S9(4)       VALUE +12.
00138      05  WS-HEXPAMT-LENGTH           PIC S9(4)       VALUE +7.
00139      05  WS-CHECK-QUE                PIC S9(8)       VALUE ZERO.
00140      05  WS-CHECK-QUE-SEQ            PIC S9(4)       VALUE ZERO.
00141      05  WS-DMO-LENGTH               PIC S9(4)       VALUE +108.
00142      05  WS-DCT-LENGTH               PIC S9(4)       VALUE +53.
00143
00144  01  FILLER.
00145      05  WS-CL-CERT-KEY-DATA.
00146          10  WS-CL-CERT-CARRIER  PIC X.
00147          10  WS-CL-CERT-GROUPING PIC X(6).
00148          10  WS-CL-CERT-STATE    PIC XX.
00149          10  WS-CL-CERT-ACCOUNT  PIC X(10).
00150          10  WS-CL-CERT-EFF-DT   PIC XX.
00151      05  WS-CL-CERT-NO           PIC X(11).
00152      05  WS-CL-BENEFICIARY       PIC X(10).
00153      05  WS-CL-CCN               PIC X(16).
00154      05  WS-CL-CLAIM-NO          PIC X(7).
00155      05  WS-CL-CLAIM-TYPE        PIC X.
00156      05  WS-CL-INSURED-LAST-NAME PIC X(15).
00157      05  WS-CL-INSURED-NAME.
00158          10  WS-CL-INSURED-1ST-NAME PIC X(12).
00159          10  WS-CL-INSURED-MID-INIT PIC X.
00160      05  W-NAME-LAST             PIC  X(15).
00161      05  W-NAME-FIRST            PIC  X(15).
00162      05  W-NAME-MIDDLE.
00163          10  FILLER              PIC  X.
00164          10  W-NAME-MIDDLE-2     PIC  X.
00165          10  FILLER              PIC  X(13).
00166      05  WS-CL-NO-OF-PMTS-MADE   PIC S9(3) COMP-3.
00167
00168      05  NOTE-KEY.
00169          10  NOTE-COMP-CD            PIC X.
00170          10  NOTE-CERT-KEY.
00171              15  NOTE-CARRIER        PIC X.
00172              15  NOTE-GROUP          PIC X(6).
00173              15  NOTE-STATE          PIC XX.
00174              15  NOTE-ACCOUNT        PIC X(10).
00175              15  NOTE-DATE           PIC XX.
00176              15  NOTE-CERT-NO        PIC X(11).
00177
00178      05  WS-ACTIVITY-TRAILERS-KEY.
00179          10  WS-ATK-COMPANY-CODE     PIC X.
00180          10  WS-ATK-CARRIER          PIC X.
00181          10  WS-ATK-CLAIM-NO         PIC X(7).
00182          10  WS-ATK-CERT-NO.
00183              15  WS-ATK-CERT-NO-PRIME  PIC X(10).
00184              15  WS-ATK-CERT-NO-SUFX   PIC X.
00185          10  WS-ATK-SEQUENCE-NO      PIC S9(4) COMP.
00186
00187      05  WS-CLAIM-KEY.
00188          10  WS-CK-COMPANY-CODE     PIC X.
00189          10  WS-CK-CARRIER          PIC X.
00190          10  WS-CK-CLAIM-NO         PIC X(7).
00191          10  WS-CK-CERT-NO.
00192              15  WS-CK-CERT-NO-PRIME   PIC X(10).
00193              15  WS-CK-CERT-NO-SUFX    PIC X.
00194
00195      05  WS-ACCOUNT-MASTER-KEY.
00196          10  WS-AM-COMPANY-CD        PIC X.
00197          10  WS-AM-CARRIER           PIC X.
00198          10  WS-AM-GROUPING          PIC X(6).
00199          10  WS-AM-STATE             PIC XX.
00200          10  WS-AM-ACCOUNT           PIC X(10).
00201          10  WS-AM-EXPIRATION-DT     PIC XX.
00202          10  FILLER                  PIC X(4).
00203
00204      05  WS-PRODUCER-MASTER-KEY.
00205          10  WS-PD-COMPANY-CD        PIC X.
00206          10  WS-PD-CARRIER           PIC X.
00207          10  WS-PD-GROUPING          PIC X(6).
00208          10  WS-PD-STATE             PIC XX.
00209          10  WS-PD-PRODUCER          PIC X(10).
00210          10  WS-PD-EXPIRATION-DT     PIC XX.
00211
00212      05  WS-LETTER-ARCHIVE-KEY.
00213          10  WS-LA-COMPANY-CD        PIC X.
00214          10  WS-LA-ARCHIVE-NO        PIC S9(8)       COMP.
00215          10  WS-LA-RECORD-TYPE       PIC X.
00216 *          88  HEADER-DATA                           VALUE '1'.
00217 *          88  ADDRESS-DATA                          VALUE '2'.
00218 *          88  TEXT-DATA                             VALUE '3'.
00219          10  WS-LA-LINE-SEQ-NO       PIC S9(4)       COMP.
102510
102510     05  WS-NAPERSOFT-KEY.
102510         10  WS-NA-COMPANY-CD        PIC X.
102510         10  WS-NA-CARRIER           PIC X.
102510         10  WS-NA-CLAIM-NO          PIC X(7).
102510         10  WS-NA-CERT-NO           PIC X(11).
102510         10  WS-NA-ARCHIVE-NO        PIC 9(8).
041613
041613     05  WS-ELENCC-KEY.
041613         10  WS-ELENCC-COMPANY-CD    PIC X.
041613         10  WS-ELENCC-REC-TYPE      PIC X.
041613         10  WS-ELENCC-ENC-CODE      PIC X(5).
041613         10  F                       PIC X(09).
00220
00221      05  WS-LETTER-ARCHIVE-ALT-KEY.
00222          10  WS-LA-ALT-COMPANY-CD    PIC X.
00223          10  WS-LA-ALT-RECORD-TYPE   PIC X.
00224          10  WS-LA-ALT-ARCHIVE-NO    PIC S9(8)       COMP.
00225          10  WS-LA-ATL-LINE-SEQ-NO   PIC S9(4)       COMP.
00226
00227      05  WS-CHECK-QUEUE-KEY.
00228          10  WS-CQ-COMPANY-CD        PIC X.
00229          10  WS-CQ-CONTROL-NUMBER    PIC S9(9)       COMP.
00230          10  WS-CQ-SEQUENCE-NUMBER   PIC S9(4)       COMP.
00231
00232      05  WS-CONTROL-FILE-KEY.
00233          16  CNTL-CO             PIC X(3).
00234          16  CNTL-RECORD-TYPE    PIC X       VALUE '1'.
00235          16  CNTL-GENL           PIC X(4)    VALUE SPACES.
00236          16  CNTL-SEQ            PIC S9(4)   VALUE +0    COMP.
00237
00238      05  WS-MAPSET-NAME              PIC X(8)      VALUE 'EL142S'.
00239
00240      05  EL142A                      PIC X(8) VALUE 'EL142A'.
00241      05  EL142B                      PIC X(8) VALUE 'EL142B'.
00242      05  EL142B2                     PIC X(8) VALUE 'EL142B2'.
00243      05  EL142C                      PIC X(8) VALUE 'EL142C'.
00244      05  EL142D                      PIC X(8) VALUE 'EL142D'.
00245      05  EL142D2                     PIC X(8) VALUE 'EL142D2'.
00246      05  EL142E                      PIC X(8) VALUE 'EL142E'.
00247      05  EL142F                      PIC X(8) VALUE 'EL142F'.
00248      05  EL142G                      PIC X(8) VALUE 'EL142G'.
00249      05  EL142H                      PIC X(8) VALUE 'EL142H'.
00250      05  EL142I                      PIC X(8) VALUE 'EL142I'.
00251      05  EL142J                      PIC X(8) VALUE 'EL142J'.
00252
00253      05  THIS-PGM                    PIC X(8)      VALUE 'EL142'.
00254      05  XCTL-PGM                    PIC X(8).
00255
00256      05  PGM-EL1501                  PIC X(8)      VALUE 'EL1501'.
00257
00258      05  EL001                       PIC X(8)      VALUE 'EL001'.
00259      05  EL004                       PIC X(8)      VALUE 'EL004'.
00260      05  EL005                       PIC X(8)      VALUE 'EL005'.
00261      05  EL010                       PIC X(8)      VALUE 'EL010'.
00262      05  EL126                       PIC X(8)      VALUE 'EL126'.
00263      05  ELDATCV                     PIC X(8) VALUE 'ELDATCV'.
00264
00265      05  WS-CLAIM-MASTER-DSID        PIC X(8) VALUE 'ELMSTR'.
00266      05  WS-ACCOUNT-MASTER-DSID      PIC X(8) VALUE 'ERACCT'.
00267      05  WS-PRODUCER-MASTER-DSID     PIC X(8) VALUE 'MPPROD'.
00268      05  WS-ACTIVITY-TRAILERS-DSID   PIC X(8) VALUE 'ELTRLR'.
00269      05  WS-LETTER-ARCHIVE-DSID      PIC X(8) VALUE 'ELARCH'.
00270      05  WS-ELARCT-FILE-ID           PIC X(8) VALUE 'ELARCT'.
00271      05  WS-LETTER-ARCHIVE-DSID2     PIC X(8) VALUE 'ELARCH2'.
00272      05  WS-CHECK-QUEUE-DSID         PIC X(8) VALUE 'ELCHKQ'.
00273      05  WS-CONTROL-FILE-DSID        PIC X(8) VALUE 'ELCNTL'.
00274      05  WS-NOTE-FILE-DSID           PIC X(8) VALUE 'ERNOTE'.
102510     05  WS-NAPERSOFT-DSID           PIC X(8) VALUE 'ELNAPS'.
041613     05  WS-ELENCC-FILE-DSID         PIC X(8) VALUE 'ELENCC'.
00275
00276      05  WS-TRANS-ID                 PIC X(4)        VALUE 'EX25'.
00277
00278      05  WS-CLAIM-TYPE               PIC X           VALUE SPACES.
00279
00280      05  WS-PI-EL142-PRIORITY        PIC X           VALUE SPACES.
00281
00282      05  WS-TEXT-MESSAGE-LENGTH      PIC S9(4)       VALUE +70
00283                                      COMP SYNC.
00284
00285      05  WS-TEXT-MESSAGE             PIC X(70)       VALUE SPACES.
00286
00287      05  WS-DEEDIT-FIELD             PIC X(8)        VALUE ZERO.
00288
00289      05  WS-DEEDIT-FIELD-V0          REDEFINES
00290          WS-DEEDIT-FIELD             PIC S9(8).
00291
00292      05  WS-RESEND-DATE              PIC XX VALUE LOW-VALUES.
00293      05  WS-SEND-ON-DATE             PIC XX VALUE LOW-VALUES.
00294      05  WS-FOLLOW-UP-DATE           PIC XX VALUE LOW-VALUES.
00295      05  WS-RECEIVED-DATE            PIC XX VALUE LOW-VALUES.
00296      05  WS-RECEIVED-PHY-DATE        PIC XX VALUE LOW-VALUES.
00297      05  WS-RECEIVED-EMP-DATE        PIC XX VALUE LOW-VALUES.
00298      05  WS-START-DATE               PIC XX VALUE LOW-VALUES.
00299      05  WS-END-DATE                 PIC XX VALUE LOW-VALUES.
00300      05  WS-CURRENT-DATE             PIC XX VALUE LOW-VALUES.
00301      05  SAVE-DATE-CCYYMMDD.
00302          10  SAVE-DATE-CC            PIC XX VALUE SPACES.
00303          10  SAVE-DATE-YMD.
00304              15  SAVE-DATE-YY        PIC XX VALUE SPACES.
00305              15  FILLER              PIC X(4) VALUE SPACES.
00306      05  WS-DATE-SENT                PIC XX VALUE LOW-VALUES.
00307      05  WS-IN-PRINT-DATE            PIC XX VALUE LOW-VALUES.
00308      05  WS-REPRINTED-DATE           PIC XX VALUE LOW-VALUES.
00309      05  WS-ARCHIVE-NUMBER           PIC 9(8)  VALUE ZEROS.
050110     05  WS-RESEND-FORM-NUMBER       PIC X(4) VALUE LOW-VALUES.
102610     05  WS-STOP-LETTER-DATE         PIC XX VALUE LOW-VALUES.
041613     05  WS-ENCLOSURE-CODE           PIC X(3) VALUE SPACES.
041613     05  WS-TEMP-ENCCODE             PIC X(3) VALUE SPACES.
021114     05  WS-CHECK-AMT-TMP            PIC Z(7).99.
021114     05  WS-CHECK-AMT-TMPX REDEFINES
021114         WS-CHECK-AMT-TMP            PIC X(10).
00310
00311
00312      05  WS-CRSEL                    PIC XX VALUE LOW-VALUES.
00313      05  WS-VOIDSD                   PIC XX VALUE LOW-VALUES.
00314      05  WS-FORM-NUMBER              PIC X(4)    VALUE SPACES.
00315      05  WS-TEMP-DT                  PIC 99B99B99.
00316
00317      05  SLASH                       PIC X           VALUE '/'.
00318
00319      05  WS-SPACES                   PIC X           VALUE SPACES.
00320      05  WS-ZIP.
00321          10  WS-ZIP-CODE         PIC X(5).
00322          10  WS-DASH             PIC X     VALUE '-'.
00323          10  WS-ZIP-PLUS4        PIC X(4).
00324      05  WS-CANADIAN-POSTAL-CODES REDEFINES WS-ZIP.
00325          10  WS-CAN-POSTAL-CD-1  PIC X(3).
00326          10  WS-DASH-CAN         PIC X.
00327          10  WS-CAN-POSTAL-CD-2  PIC X(3).
00328          10  WS-CAN-FILLER       PIC X(3).
00329      05  WS-WORK-PHONE           PIC X(10)  VALUE ZEROS.
00330      05  WS-NUMERIC-PHONE REDEFINES WS-WORK-PHONE
00331                                  PIC 9(10).
00332
052506     05  WS-PRF-DT               PIC X(2)    VALUE LOW-VALUES.
00333      EJECT
00334      05  ER-ZERO                 PIC 9(4)        VALUE 0000.
00335      05  ER-0000                 PIC 9(4)        VALUE 0000.
00336      05  ER-0004                 PIC 9(4)        VALUE 0004.
00337      05  ER-0006                 PIC 9(4)        VALUE 0006.
00338      05  ER-0008                 PIC 9(4)        VALUE 0008.
00339      05  ER-0021                 PIC 9(4)        VALUE 0021.
00340      05  ER-0023                 PIC 9(4)        VALUE 0023.
00341      05  ER-0029                 PIC 9(4)        VALUE 0029.
00342      05  ER-0068                 PIC 9(4)        VALUE 0068.
00343      05  ER-0070                 PIC 9(4)        VALUE 0070.
00344      05  ER-0105                 PIC 9(4)        VALUE 0105.
00345      05  ER-0106                 PIC 9(4)        VALUE 0106.
00346      05  ER-0107                 PIC 9(4)        VALUE 0107.
00347      05  ER-0109                 PIC 9(4)        VALUE 0109.
00348      05  ER-0111                 PIC 9(4)        VALUE 0111.
00349      05  ER-0175                 PIC 9(4)        VALUE 0175.
00350      05  ER-0198                 PIC 9(4)        VALUE 0198.
00351      05  ER-0285                 PIC 9(4)        VALUE 0285.
00352      05  ER-0286                 PIC 9(4)        VALUE 0286.
00353      05  ER-0287                 PIC 9(4)        VALUE 0287.
00354      05  ER-0288                 PIC 9(4)        VALUE 0288.
00355      05  ER-0289                 PIC 9(4)        VALUE 0289.
00356      05  ER-0290                 PIC 9(4)        VALUE 0290.
00357      05  ER-0291                 PIC 9(4)        VALUE 0291.
00358      05  ER-0292                 PIC 9(4)        VALUE 0292.
00359      05  ER-0293                 PIC 9(4)        VALUE 0293.
00360      05  ER-0294                 PIC 9(4)        VALUE 0294.
00361      05  ER-0295                 PIC 9(4)        VALUE 0295.
00362      05  ER-0296                 PIC 9(4)        VALUE 0296.
00363      05  ER-0297                 PIC 9(4)        VALUE 0297.
00364      05  ER-0298                 PIC 9(4)        VALUE 0298.
00365      05  ER-0299                 PIC 9(4)        VALUE 0299.
00366      05  ER-0300                 PIC 9(4)        VALUE 0300.
00367      05  ER-0303                 PIC 9(4)        VALUE 0303.
042110     05  ER-0310                 PIC 9(4)        VALUE 0310.
00368      05  ER-0324                 PIC 9(4)        VALUE 0324.
00369      05  ER-0325                 PIC 9(4)        VALUE 0325.
00370      05  ER-0327                 PIC 9(4)        VALUE 0327.
00371      05  ER-0328                 PIC 9(4)        VALUE 0328.
00372      05  ER-0329                 PIC 9(4)        VALUE 0329.
00373      05  ER-0341                 PIC 9(4)        VALUE 0341.
00374      05  ER-0342                 PIC 9(4)        VALUE 0342.
00375      05  ER-0344                 PIC 9(4)        VALUE 0344.
00376      05  ER-0384                 PIC 9(4)        VALUE 0384.
00377      05  ER-0388                 PIC 9(4)        VALUE 0388.
00378      05  ER-0532                 PIC 9(4)        VALUE 0532.
00379      05  ER-0538                 PIC 9(4)        VALUE 0538.
00380      05  ER-0550                 PIC 9(4)        VALUE 0550.
00381      05  ER-0551                 PIC 9(4)        VALUE 0551.
00382      05  ER-0564                 PIC 9(4)        VALUE 0564.
00383      05  ER-0570                 PIC 9(4)        VALUE 0570.
00384      05  ER-0571                 PIC 9(4)        VALUE 0571.
00385      05  ER-0574                 PIC 9(4)        VALUE 0574.
00386      05  ER-0575                 PIC 9(4)        VALUE 0575.
00387      05  ER-0576                 PIC 9(4)        VALUE 0576.
00388      05  ER-0577                 PIC 9(4)        VALUE 0577.
00389      05  ER-0578                 PIC 9(4)        VALUE 0578.
00390      05  ER-0579                 PIC 9(4)        VALUE 0579.
00391      05  ER-0580                 PIC 9(4)        VALUE 0580.
00392      05  ER-0581                 PIC 9(4)        VALUE 0581.
00393      05  ER-0641                 PIC 9(4)        VALUE 0641.
00394      05  ER-0642                 PIC 9(4)        VALUE 0642.
00395      05  ER-0643                 PIC 9(4)        VALUE 0643.
042110     05  ER-0755                 PIC 9(4)        VALUE 0755.
00396      05  ER-0830                 PIC 9(4)        VALUE 0830.
00397      05  ER-0849                 PIC 9(4)        VALUE 0849.
052506     05  ER-0873                 PIC 9(4)        VALUE 0873.
020810     05  ER-0884                 PIC 9(4)        VALUE 0884.
102610     05  ER-0897                 PIC 9(4)        VALUE 0897.
00398      05  ER-0919                 PIC 9(4)        VALUE 0919.
00399      05  ER-0921                 PIC 9(4)        VALUE 0921.
00400      05  ER-0937                 PIC 9(4)        VALUE 0937.
00401      05  ER-0946                 PIC 9(4)        VALUE 0946.
00402      05  ER-0947                 PIC 9(4)        VALUE 0947.
00403      05  ER-0948                 PIC 9(4)        VALUE 0948.
00404      05  ER-0949                 PIC 9(4)        VALUE 0949.
00405      05  ER-0950                 PIC 9(4)        VALUE 0950.
00406      05  ER-0951                 PIC 9(4)        VALUE 0951.
00407      05  ER-0954                 PIC 9(4)        VALUE 0954.
00408      05  ER-0969                 PIC 9(4)        VALUE 0969.
00409      05  ER-0974                 PIC 9(4)        VALUE 0974.
00410      05  ER-0975                 PIC 9(4)        VALUE 0975.
041613     05  ER-1560                 PIC 9(4)        VALUE 1560.
           05  ER-1561                 PIC 9(4)        VALUE 1561.
020413     05  ER-1566                 PIC 9(4)        VALUE 1566.
020413     05  ER-1567                 PIC 9(4)        VALUE 1567.
041613     05  ER-1568                 PIC 9(4)        VALUE 1568.
102413     05  ER-1569                 PIC 9(4)        VALUE 1569.
00411      05  ER-2466                 PIC 9(4)        VALUE 2466.
00412      05  ER-8003                 PIC 9(4)        VALUE 8003.
00413      05  ER-8004                 PIC 9(4)        VALUE 8004.
00414      05  ER-8051                 PIC 9(4)        VALUE 8051.
00415      05  ER-8052                 PIC 9(4)        VALUE 8052.
00416      05  ER-8053                 PIC 9(4)        VALUE 8053.
00417      05  ER-8054                 PIC 9(4)        VALUE 8054.
00418      05  ER-8055                 PIC 9(4)        VALUE 8055.
00419      05  ER-8056                 PIC 9(4)        VALUE 8056.
00420      05  ER-8057                 PIC 9(4)        VALUE 8057.
00421      05  ER-8058                 PIC 9(4)        VALUE 8058.
00422      05  ER-8059                 PIC 9(4)        VALUE 8059.
00423      05  ER-8060                 PIC 9(4)        VALUE 8060.
00424      05  ER-8061                 PIC 9(4)        VALUE 8061.
00425      05  ER-8062                 PIC 9(4)        VALUE 8062.
00426      05  ER-8063                 PIC 9(4)        VALUE 8063.
00427      05  ER-8064                 PIC 9(4)        VALUE 8064.
00428      05  ER-8065                 PIC 9(4)        VALUE 8065.
00429      05  ER-8066                 PIC 9(4)        VALUE 8066.
00430      05  ER-8152                 PIC 9(4)        VALUE 8152.
00431      05  ER-8153                 PIC 9(4)        VALUE 8153.
00432      05  ER-8154                 PIC 9(4)        VALUE 8154.
00433      05  ER-8155                 PIC 9(4)        VALUE 8155.
00434      05  ER-9616                 PIC 9(4)        VALUE 9616.
00435
00436  01  HAN-PAYMENT-NOTE-DATA.
00437      12  WS-HAN-PAYMENT-NOTE.
00438          16  WS-HAN-PMT-CODE     PIC X.
00439          16  WS-HAN-PMT-TEXT     PIC X(59).
00440
00441  01  HAN-LETTER-REASON-DATA.
00442      12  WS-REASON-TEXT.
00443          16  WS-RE-NDX           PIC 99.
00444          16  FILLER              PIC X(68).
00445
00446      12  HAN-REASON-TABLE.
00447          16  FILLER              PIC X(50) VALUE
00448            'ADDITIONAL INFO REQUESTED FROM PHYSICIAN          '.
00449          16  FILLER              PIC X(50) VALUE
00450            'CHECKING PRE-EXISTING CONDITION                   '.
00451          16  FILLER              PIC X(50) VALUE
00452            'ADDITIONAL INFO RECEIVED / CLAIM REOPENED         '.
00453          16  FILLER              PIC X(50) VALUE
00454            'LETTER TO INSURED                                 '.
00455          16  FILLER              PIC X(50) VALUE
00456            'LETTER TO CREDITOR                                '.
00457          16  FILLER              PIC X(50) VALUE
00458            'LETTER TO EMPLOYER                                '.
00459          16  FILLER              PIC X(50) VALUE
00460            'LETTER TO INSURED / 2ND REQUEST                   '.
00461          16  FILLER              PIC X(50) VALUE
00462            'LETTER TO CREDITOR / 2ND REQUEST                  '.
00463          16  FILLER              PIC X(50) VALUE
00464            'LETTER TO EMPLOYER / 2ND REQUEST                  '.
00465          16  FILLER              PIC X(50) VALUE
00466            'AWAITING INITIAL CLAIM FORM                       '.
00467          16  FILLER              PIC X(50) VALUE
00468            'AWAITING SUPPLEMENTAL INFORMATION                 '.
00469          16  FILLER              PIC X(50) VALUE
00470            'DENIED / PRE-EXISTING CONDITION                   '.
00471          16  FILLER              PIC X(50) VALUE
00472            'DENIED / WAITING PERIOD NOT MET                   '.
00473          16  FILLER              PIC X(50) VALUE
00474            'DENIED / NORMAL PREGNANCY                         '.
00475          16  FILLER              PIC X(50) VALUE
00476            'DENIED / ACT OF WAR                               '.
00477          16  FILLER              PIC X(50) VALUE
00478            'DENIED / NOT TOTALLY DISABLED                     '.
00479          16  FILLER              PIC X(50) VALUE
00480            'DENIED / NOT UNDER CARE & TREATMENT OF PHYSICIAN  '.
00481          16  FILLER              PIC X(50) VALUE
00482            'DENIED / NO COVERAGE INFORCE                      '.
00483          16  FILLER              PIC X(50) VALUE
00484            'DENIED / DISABLED ON DATE OF LOAN                 '.
00485          16  FILLER              PIC X(50) VALUE
00486            'DENIED / OVER MAXIMUM AGE                         '.
00487          16  FILLER              PIC X(50) VALUE
00488            'CLOSED / CLAIM INFO NOT PROVIDED                  '.
00489          16  FILLER              PIC X(50) VALUE
00490            'PHYSICIAN INFORMATION INCOMPLETE                  '.
00491          16  FILLER              PIC X(50) VALUE
00492            'ACKNOWLEDGEMENT LETTER TO INSURED                 '.
00493
00494      12  HAN-LETTER-REASON-TABLE  REDEFINES  HAN-REASON-TABLE.
00495          16  HAN-TABLE-ENTRIES  OCCURS  23  TIMES.
00496              20  HAN-REASON-TEXT PIC X(50).
00497
00498      EJECT
00499 *                                COPY ELCINTF.
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
00500
00501      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.
00502          16  PI-TRAILER-NUMBER       PIC 9(4) COMP.
00503          16  PI-EL142-PRIORITY       PIC X.
00504          16  FILLER                  PIC X.
00505
00506          16  PI-MAP-NAME             PIC X(8).
00507
00508          16  FILLER                  REDEFINES
00509              PI-MAP-NAME.
00510              20  FILLER              PIC XX.
00511              20  PI-MAP-NUMBER       PIC X(6).
00512
00513          16  PI-QUALIFICATION-SWITCHES    COMP-3.
00514              20  PI-REMINDERS-SW     PIC S9.
00515              20  PI-LETTERS-SW       PIC S9.
00516              20  PI-PAYMENTS-SW      PIC S9.
00517              20  PI-AUTO-PAY-SW      PIC S9.
00518              20  PI-NOTES-SW         PIC S9.
00519              20  PI-RES-EXP-SW       PIC S9.
00520              20  PI-DENIALS-SW       PIC S9.
00521              20  PI-INCURRED-DATE-SW PIC S9.
00522              20  PI-FORMS-SW         PIC S9.
00523
00524          16  PI-AFTER-DATE           PIC XX.
00525          16  PI-AFTER-DATE-2         PIC XX.
00526          16  PI-AFTER-DATE-3         PIC XX.
00527          16  PI-HOLD-UNTIL-DATE      PIC XX.
00528
00529          16  PI-ACTIVITY-TRAILERS-KEY.
00530              20  PI-ATK-COMPANY-CODE PIC X.
00531              20  PI-ATK-CARRIER      PIC X.
00532              20  PI-ATK-CLAIM-NO     PIC X(7).
00533              20  PI-ATK-CERT-NO.
00534                  24  PI-ATK-CERT-NO-PRIME  PIC X(10).
00535                  24  PI-ATK-CERT-NO-SUFX   PIC X.
00536              20  PI-ATK-SEQUENCE-NO  PIC S9(4) COMP.
00537
00538          16  PI-PREV-ACTIVITY-TRAILERS-KEY.
00539              20  PI-PREV-ATK-COMPANY-CODE PIC X.
00540              20  PI-PREV-ATK-CARRIER      PIC X.
00541              20  PI-PREV-ATK-CLAIM-NO     PIC X(7).
00542              20  PI-PREV-ATK-CERT-NO.
00543                  24  PI-PREV-ATK-CERT-NO-PRIME PIC X(10).
00544                  24  PI-PREV-ATK-CERT-NO-SUFX  PIC X.
00545              20  PI-PREV-ATK-SEQUENCE-NO  PIC S9(4) COMP.
00546
00547          16  PI-SAVE-KEY.
00548              20  PI-SAVE-ATK-COMPANY-CODE PIC X.
00549              20  PI-SAVE-ATK-CARRIER      PIC X.
00550              20  PI-SAVE-ATK-CLAIM-NO     PIC X(7).
00551              20  PI-SAVE-ATK-CERT-NO.
00552                  24  PI-SAVE-ATK-CERT-NO-PRIME  PIC X(10).
00553                  24  PI-SAVE-ATK-CERT-NO-PRIME  PIC X.
00554              20  PI-SAVE-ATK-SEQUENCE-NO        PIC S9(4) COMP.
00555
00556          16  PI-PREV-AID             PIC X.
00557
00558          16  PI-RECORD-COUNT         PIC S9  COMP-3.
00559          16  PI-END-OF-FILE          PIC S9  COMP-3.
042110         16  PI-DENIAL-REASON-CODE   PIC X(4).
042110         16  PI-MAPG-DELETE-CNT      PIC 9.
00560          16  FILLER                  PIC X.
00561
00562          16  PI-FIRST-TIME-SW        PIC X.
00563              88  FIRST-TIME                       VALUE 'Y'.
00564
00565          16  PI-SAVE-LAST-MAINT-DT   PIC XX.
00566          16  PI-SAVE-LAST-UPD-BY     PIC X(4).
020413         16  PI-APPROVAL-LEVEL       PIC X.
041613         16  PI-ENC-CODE             PIC X(3).
041613         16  PI-CREATED-IN-NAPERSOFT PIC X.
043019         16  pi-den-recorded-dt      pic xx.
043019         16  pi-incurred-dt          pic xx.
00567
043019         16  FILLER                  PIC X(520).
00569
00570      EJECT
00571 *                                    COPY EL142S.
       01  EL142BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  BDATEL PIC S9(0004) COMP.
           05  BDATEF PIC  X(0001).
           05  FILLER REDEFINES BDATEF.
               10  BDATEA PIC  X(0001).
           05  BDATEI PIC  X(0008).
      *    -------------------------------
           05  BTIMEL PIC S9(0004) COMP.
           05  BTIMEF PIC  X(0001).
           05  FILLER REDEFINES BTIMEF.
               10  BTIMEA PIC  X(0001).
           05  BTIMEI PIC  X(0005).
      *    -------------------------------
           05  BTYPEL PIC S9(0004) COMP.
           05  BTYPEF PIC  X(0001).
           05  FILLER REDEFINES BTYPEF.
               10  BTYPEA PIC  X(0001).
           05  BTYPEI PIC  X(0010).
      *    -------------------------------
           05  BRECDTEL PIC S9(0004) COMP.
           05  BRECDTEF PIC  X(0001).
           05  FILLER REDEFINES BRECDTEF.
               10  BRECDTEA PIC  X(0001).
           05  BRECDTEI PIC  X(0008).
      *    -------------------------------
           05  BBYL PIC S9(0004) COMP.
           05  BBYF PIC  X(0001).
           05  FILLER REDEFINES BBYF.
               10  BBYA PIC  X(0001).
           05  BBYI PIC  X(0004).
      *    -------------------------------
           05  BTLRTYPL PIC S9(0004) COMP.
           05  BTLRTYPF PIC  X(0001).
           05  FILLER REDEFINES BTLRTYPF.
               10  BTLRTYPA PIC  X(0001).
           05  BTLRTYPI PIC  X(0001).
      *    -------------------------------
           05  BSEQL PIC S9(0004) COMP.
           05  BSEQF PIC  X(0001).
           05  FILLER REDEFINES BSEQF.
               10  BSEQA PIC  X(0001).
           05  BSEQI PIC  X(0004).
      *    -------------------------------
           05  BMANTBYL PIC S9(0004) COMP.
           05  BMANTBYF PIC  X(0001).
           05  FILLER REDEFINES BMANTBYF.
               10  BMANTBYA PIC  X(0001).
           05  BMANTBYI PIC  X(0004).
      *    -------------------------------
           05  BMANTONL PIC S9(0004) COMP.
           05  BMANTONF PIC  X(0001).
           05  FILLER REDEFINES BMANTONF.
               10  BMANTONA PIC  X(0001).
           05  BMANTONI PIC  X(0008).
      *    -------------------------------
           05  BMANTATL PIC S9(0004) COMP.
           05  BMANTATF PIC  X(0001).
           05  FILLER REDEFINES BMANTATF.
               10  BMANTATA PIC  X(0001).
           05  BMANTATI PIC  X(0005).
      *    -------------------------------
           05  BMAINTL PIC S9(0004) COMP.
           05  BMAINTF PIC  X(0001).
           05  FILLER REDEFINES BMAINTF.
               10  BMAINTA PIC  X(0001).
           05  BMAINTI PIC  X(0001).
      *    -------------------------------
           05  BEOBYNL PIC S9(0004) COMP.
           05  BEOBYNF PIC  X(0001).
           05  FILLER REDEFINES BEOBYNF.
               10  BEOBYNA PIC  X(0001).
           05  BEOBYNI PIC  X(0001).
      *    -------------------------------
           05  BCLMYNL PIC S9(0004) COMP.
           05  BCLMYNF PIC  X(0001).
           05  FILLER REDEFINES BCLMYNF.
               10  BCLMYNA PIC  X(0001).
           05  BCLMYNI PIC  X(0001).
      *    -------------------------------
           05  BSRVYNL PIC S9(0004) COMP.
           05  BSRVYNF PIC  X(0001).
           05  FILLER REDEFINES BSRVYNF.
               10  BSRVYNA PIC  X(0001).
           05  BSRVYNI PIC  X(0001).
      *    -------------------------------
           05  BACHPMTL PIC S9(0004) COMP.
           05  BACHPMTF PIC  X(0001).
           05  FILLER REDEFINES BACHPMTF.
               10  BACHPMTA PIC  X(0001).
           05  BACHPMTI PIC  X(0003).
      *    -------------------------------
           05  BPRFDTL PIC S9(0004) COMP.
           05  BPRFDTF PIC  X(0001).
           05  FILLER REDEFINES BPRFDTF.
               10  BPRFDTA PIC  X(0001).
           05  BPRFDTI PIC  X(0008).
      *    -------------------------------
           05  BSPRELL PIC S9(0004) COMP.
           05  BSPRELF PIC  X(0001).
           05  FILLER REDEFINES BSPRELF.
               10  BSPRELA PIC  X(0001).
           05  BSPRELI PIC  X(0001).
      *    -------------------------------
           05  BCKNOL PIC S9(0004) COMP.
           05  BCKNOF PIC  X(0001).
           05  FILLER REDEFINES BCKNOF.
               10  BCKNOA PIC  X(0001).
           05  BCKNOI PIC  X(0007).
      *    -------------------------------
           05  BPAYEEL PIC S9(0004) COMP.
           05  BPAYEEF PIC  X(0001).
           05  FILLER REDEFINES BPAYEEF.
               10  BPAYEEA PIC  X(0001).
           05  BPAYEEI PIC  X(0011).
      *    -------------------------------
           05  BDTWRITL PIC S9(0004) COMP.
           05  BDTWRITF PIC  X(0001).
           05  FILLER REDEFINES BDTWRITF.
               10  BDTWRITA PIC  X(0001).
           05  BDTWRITI PIC  X(0008).
      *    -------------------------------
           05  BPNAMEL PIC S9(0004) COMP.
           05  BPNAMEF PIC  X(0001).
           05  FILLER REDEFINES BPNAMEF.
               10  BPNAMEA PIC  X(0001).
           05  BPNAMEI PIC  X(0030).
      *    -------------------------------
           05  BWRITBYL PIC S9(0004) COMP.
           05  BWRITBYF PIC  X(0001).
           05  FILLER REDEFINES BWRITBYF.
               10  BWRITBYA PIC  X(0001).
           05  BWRITBYI PIC  X(0004).
      *    -------------------------------
           05  BAPPVBYL PIC S9(0004) COMP.
           05  BAPPVBYF PIC  X(0001).
           05  FILLER REDEFINES BAPPVBYF.
               10  BAPPVBYA PIC  X(0001).
           05  BAPPVBYI PIC  X(0004).
      *    -------------------------------
           05  BAMTL PIC S9(0004) COMP.
           05  BAMTF PIC  X(0001).
           05  FILLER REDEFINES BAMTF.
               10  BAMTA PIC  X(0001).
           05  BAMTI PIC  X(0013).
      *    -------------------------------
           05  BRESERVL PIC S9(0004) COMP.
           05  BRESERVF PIC  X(0001).
           05  FILLER REDEFINES BRESERVF.
               10  BRESERVA PIC  X(0001).
           05  BRESERVI PIC  X(0010).
      *    -------------------------------
           05  BTHRUHDL PIC S9(0004) COMP.
           05  BTHRUHDF PIC  X(0001).
           05  FILLER REDEFINES BTHRUHDF.
               10  BTHRUHDA PIC  X(0001).
           05  BTHRUHDI PIC  X(0014).
      *    -------------------------------
           05  BPDTHRUL PIC S9(0004) COMP.
           05  BPDTHRUF PIC  X(0001).
           05  FILLER REDEFINES BPDTHRUF.
               10  BPDTHRUA PIC  X(0001).
           05  BPDTHRUI PIC  X(0008).
      *    -------------------------------
           05  BEXPL PIC S9(0004) COMP.
           05  BEXPF PIC  X(0001).
           05  FILLER REDEFINES BEXPF.
               10  BEXPA PIC  X(0001).
           05  BEXPI PIC  X(0010).
      *    -------------------------------
           05  BDAYSPDL PIC S9(0004) COMP.
           05  BDAYSPDF PIC  X(0001).
           05  FILLER REDEFINES BDAYSPDF.
               10  BDAYSPDA PIC  X(0001).
           05  BDAYSPDI PIC  X(0007).
      *    -------------------------------
           05  BRATHDL PIC S9(0004) COMP.
           05  BRATHDF PIC  X(0001).
           05  FILLER REDEFINES BRATHDF.
               10  BRATHDA PIC  X(0001).
           05  BRATHDI PIC  X(0014).
      *    -------------------------------
           05  BDAYRATL PIC S9(0004) COMP.
           05  BDAYRATF PIC  X(0001).
           05  FILLER REDEFINES BDAYRATF.
               10  BDAYRATA PIC  X(0001).
           05  BDAYRATI PIC  X(0010).
      *    -------------------------------
           05  BPAYTYPL PIC S9(0004) COMP.
           05  BPAYTYPF PIC  X(0001).
           05  FILLER REDEFINES BPAYTYPF.
               10  BPAYTYPA PIC  X(0001).
           05  BPAYTYPI PIC  X(0022).
      *    -------------------------------
           05  BCRSELL PIC S9(0004) COMP.
           05  BCRSELF PIC  X(0001).
           05  FILLER REDEFINES BCRSELF.
               10  BCRSELA PIC  X(0001).
           05  BCRSELI PIC  X(0008).
      *    -------------------------------
           05  BFORCEDL PIC S9(0004) COMP.
           05  BFORCEDF PIC  X(0001).
           05  FILLER REDEFINES BFORCEDF.
               10  BFORCEDA PIC  X(0001).
           05  BFORCEDI PIC  X(0001).
      *    -------------------------------
           05  BVOIDSDL PIC S9(0004) COMP.
           05  BVOIDSDF PIC  X(0001).
           05  FILLER REDEFINES BVOIDSDF.
               10  BVOIDSDA PIC  X(0001).
           05  BVOIDSDI PIC  X(0008).
      *    -------------------------------
           05  BVOIDDTL PIC S9(0004) COMP.
           05  BVOIDDTF PIC  X(0001).
           05  FILLER REDEFINES BVOIDDTF.
               10  BVOIDDTA PIC  X(0001).
           05  BVOIDDTI PIC  X(0008).
      *    -------------------------------
           05  BCKQUEL PIC S9(0004) COMP.
           05  BCKQUEF PIC  X(0001).
           05  FILLER REDEFINES BCKQUEF.
               10  BCKQUEA PIC  X(0001).
           05  BCKQUEI PIC  S9(8).
      *    -------------------------------
           05  BCKSEQL PIC S9(0004) COMP.
           05  BCKSEQF PIC  X(0001).
           05  FILLER REDEFINES BCKSEQF.
               10  BCKSEQA PIC  X(0001).
           05  BCKSEQI PIC  S9(4).
      *    -------------------------------
           05  BNOTE1L PIC S9(0004) COMP.
           05  BNOTE1F PIC  X(0001).
           05  FILLER REDEFINES BNOTE1F.
               10  BNOTE1A PIC  X(0001).
           05  BNOTE1I PIC  X(0060).
      *    -------------------------------
           05  BNOTE2L PIC S9(0004) COMP.
           05  BNOTE2F PIC  X(0001).
           05  FILLER REDEFINES BNOTE2F.
               10  BNOTE2A PIC  X(0001).
           05  BNOTE2I PIC  X(0060).
      *    -------------------------------
           05  BORIGINL PIC S9(0004) COMP.
           05  BORIGINF PIC  X(0001).
           05  FILLER REDEFINES BORIGINF.
               10  BORIGINA PIC  X(0001).
           05  BORIGINI PIC  X(0010).
      *    -------------------------------
           05  BPMTORGL PIC S9(0004) COMP.
           05  BPMTORGF PIC  X(0001).
           05  FILLER REDEFINES BPMTORGF.
               10  BPMTORGA PIC  X(0001).
           05  BPMTORGI PIC  X(0001).
      *    -------------------------------
           05  BCASHEDL PIC S9(0004) COMP.
           05  BCASHEDF PIC  X(0001).
           05  FILLER REDEFINES BCASHEDF.
               10  BCASHEDA PIC  X(0001).
           05  BCASHEDI PIC  X(0008).
      *    -------------------------------
           05  BEXPHDGL PIC S9(0004) COMP.
           05  BEXPHDGF PIC  X(0001).
           05  FILLER REDEFINES BEXPHDGF.
               10  BEXPHDGA PIC  X(0001).
           05  BEXPHDGI PIC  X(0014).
      *    -------------------------------
           05  BEXPTYPL PIC S9(0004) COMP.
           05  BEXPTYPF PIC  X(0001).
           05  FILLER REDEFINES BEXPTYPF.
               10  BEXPTYPA PIC  X(0001).
           05  BEXPTYPI PIC  X(0001).
      *    -------------------------------
           05  BCRACPL PIC S9(0004) COMP.
           05  BCRACPF PIC  X(0001).
           05  FILLER REDEFINES BCRACPF.
               10  BCRACPA PIC  X(0001).
           05  BCRACPI PIC  X(0008).
      *    -------------------------------
           05  BHOLDATL PIC S9(0004) COMP.
           05  BHOLDATF PIC  X(0001).
           05  FILLER REDEFINES BHOLDATF.
               10  BHOLDATA PIC  X(0001).
           05  BHOLDATI PIC  X(0008).
      *    -------------------------------
           05  BVOIDACL PIC S9(0004) COMP.
           05  BVOIDACF PIC  X(0001).
           05  FILLER REDEFINES BVOIDACF.
               10  BVOIDACA PIC  X(0001).
           05  BVOIDACI PIC  X(0008).
      *    -------------------------------
           05  BEMSG1L PIC S9(0004) COMP.
           05  BEMSG1F PIC  X(0001).
           05  FILLER REDEFINES BEMSG1F.
               10  BEMSG1A PIC  X(0001).
           05  BEMSG1I PIC  X(0079).
      *    -------------------------------
           05  BPFKL PIC S9(0004) COMP.
           05  BPFKF PIC  X(0001).
           05  FILLER REDEFINES BPFKF.
               10  BPFKA PIC  X(0001).
           05  BPFKI PIC  9(2).
       01  EL142BO REDEFINES EL142BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPEO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRECDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTLRTYPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEQO PIC  9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BMANTBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BMANTONO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BMANTATO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BMAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BEOBYNO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCLMYNO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSRVYNO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACHPMTO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPRFDTO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSPRELO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKNOO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPAYEEO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDTWRITO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPNAMEO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BWRITBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAPPVBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAMTO PIC  Z,ZZZ,ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRESERVO PIC  ZZ,ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTHRUHDO PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPDTHRUO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BEXPO PIC  ZZ,ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDAYSPDO PIC  ZZ,ZZ9-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRATHDO PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDAYRATO PIC  ZZ9.99999-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPAYTYPO PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCRSELO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BFORCEDO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BVOIDSDO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BVOIDDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKQUEO PIC  Z(7)9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKSEQO PIC  Z(3)9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNOTE1O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNOTE2O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BORIGINO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPMTORGO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCASHEDO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BEXPHDGO PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BEXPTYPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCRACPO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BHOLDATO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BVOIDACO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BEMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPFKO PIC  99.
      *    -------------------------------
       01  EL142HI REDEFINES EL142BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  HDATEL PIC S9(0004) COMP.
           05  HDATEF PIC  X(0001).
           05  FILLER REDEFINES HDATEF.
               10  HDATEA PIC  X(0001).
           05  HDATEI PIC  X(0008).
      *    -------------------------------
           05  HTIMEL PIC S9(0004) COMP.
           05  HTIMEF PIC  X(0001).
           05  FILLER REDEFINES HTIMEF.
               10  HTIMEA PIC  X(0001).
           05  HTIMEI PIC  X(0005).
      *    -------------------------------
           05  HTYPEL PIC S9(0004) COMP.
           05  HTYPEF PIC  X(0001).
           05  FILLER REDEFINES HTYPEF.
               10  HTYPEA PIC  X(0001).
           05  HTYPEI PIC  X(0010).
      *    -------------------------------
           05  HRECDTEL PIC S9(0004) COMP.
           05  HRECDTEF PIC  X(0001).
           05  FILLER REDEFINES HRECDTEF.
               10  HRECDTEA PIC  X(0001).
           05  HRECDTEI PIC  X(0008).
      *    -------------------------------
           05  HBYL PIC S9(0004) COMP.
           05  HBYF PIC  X(0001).
           05  FILLER REDEFINES HBYF.
               10  HBYA PIC  X(0001).
           05  HBYI PIC  X(0004).
      *    -------------------------------
           05  HTLRTYPL PIC S9(0004) COMP.
           05  HTLRTYPF PIC  X(0001).
           05  FILLER REDEFINES HTLRTYPF.
               10  HTLRTYPA PIC  X(0001).
           05  HTLRTYPI PIC  X(0001).
      *    -------------------------------
           05  HSEQL PIC S9(0004) COMP.
           05  HSEQF PIC  X(0001).
           05  FILLER REDEFINES HSEQF.
               10  HSEQA PIC  X(0001).
           05  HSEQI PIC  X(0004).
      *    -------------------------------
           05  HMANTBYL PIC S9(0004) COMP.
           05  HMANTBYF PIC  X(0001).
           05  FILLER REDEFINES HMANTBYF.
               10  HMANTBYA PIC  X(0001).
           05  HMANTBYI PIC  X(0004).
      *    -------------------------------
           05  HMANTONL PIC S9(0004) COMP.
           05  HMANTONF PIC  X(0001).
           05  FILLER REDEFINES HMANTONF.
               10  HMANTONA PIC  X(0001).
           05  HMANTONI PIC  X(0008).
      *    -------------------------------
           05  HMANTATL PIC S9(0004) COMP.
           05  HMANTATF PIC  X(0001).
           05  FILLER REDEFINES HMANTATF.
               10  HMANTATA PIC  X(0001).
           05  HMANTATI PIC  X(0005).
      *    -------------------------------
           05  HMAINTL PIC S9(0004) COMP.
           05  HMAINTF PIC  X(0001).
           05  FILLER REDEFINES HMAINTF.
               10  HMAINTA PIC  X(0001).
           05  HMAINTI PIC  X(0001).
      *    -------------------------------
           05  HRESMANL PIC S9(0004) COMP.
           05  HRESMANF PIC  X(0001).
           05  FILLER REDEFINES HRESMANF.
               10  HRESMANA PIC  X(0001).
           05  HRESMANI PIC  X(0001).
      *    -------------------------------
           05  HMANAMTL PIC S9(0004) COMP.
           05  HMANAMTF PIC  X(0001).
           05  FILLER REDEFINES HMANAMTF.
               10  HMANAMTA PIC  X(0001).
           05  HMANAMTI PIC  S9(10)V99.
      *    -------------------------------
           05  HITDCL PIC S9(0004) COMP.
           05  HITDCF PIC  X(0001).
           05  FILLER REDEFINES HITDCF.
               10  HITDCA PIC  X(0001).
           05  HITDCI PIC  X(0010).
      *    -------------------------------
           05  HRESFUTL PIC S9(0004) COMP.
           05  HRESFUTF PIC  X(0001).
           05  FILLER REDEFINES HRESFUTF.
               10  HRESFUTA PIC  X(0001).
           05  HRESFUTI PIC  X(0001).
      *    -------------------------------
           05  HFUTAMTL PIC S9(0004) COMP.
           05  HFUTAMTF PIC  X(0001).
           05  FILLER REDEFINES HFUTAMTF.
               10  HFUTAMTA PIC  X(0001).
           05  HFUTAMTI PIC  X(0012).
      *    -------------------------------
           05  HITDNCL PIC S9(0004) COMP.
           05  HITDNCF PIC  X(0001).
           05  FILLER REDEFINES HITDNCF.
               10  HITDNCA PIC  X(0001).
           05  HITDNCI PIC  X(0010).
      *    -------------------------------
           05  HRESIBNL PIC S9(0004) COMP.
           05  HRESIBNF PIC  X(0001).
           05  FILLER REDEFINES HRESIBNF.
               10  HRESIBNA PIC  X(0001).
           05  HRESIBNI PIC  X(0001).
      *    -------------------------------
           05  HIBNAMTL PIC S9(0004) COMP.
           05  HIBNAMTF PIC  X(0001).
           05  FILLER REDEFINES HIBNAMTF.
               10  HIBNAMTA PIC  X(0001).
           05  HIBNAMTI PIC  X(0012).
      *    -------------------------------
           05  HLSTCLOL PIC S9(0004) COMP.
           05  HLSTCLOF PIC  X(0001).
           05  FILLER REDEFINES HLSTCLOF.
               10  HLSTCLOA PIC  X(0001).
           05  HLSTCLOI PIC  X(0008).
      *    -------------------------------
           05  HRESLFPL PIC S9(0004) COMP.
           05  HRESLFPF PIC  X(0001).
           05  FILLER REDEFINES HRESLFPF.
               10  HRESLFPA PIC  X(0001).
           05  HRESLFPI PIC  X(0001).
      *    -------------------------------
           05  HPTCAMTL PIC S9(0004) COMP.
           05  HPTCAMTF PIC  X(0001).
           05  FILLER REDEFINES HPTCAMTF.
               10  HPTCAMTA PIC  X(0001).
           05  HPTCAMTI PIC  X(0012).
      *    -------------------------------
           05  HLSTOPEL PIC S9(0004) COMP.
           05  HLSTOPEF PIC  X(0001).
           05  FILLER REDEFINES HLSTOPEF.
               10  HLSTOPEA PIC  X(0001).
           05  HLSTOPEI PIC  X(0008).
      *    -------------------------------
           05  HRESAHPL PIC S9(0004) COMP.
           05  HRESAHPF PIC  X(0001).
           05  FILLER REDEFINES HRESAHPF.
               10  HRESAHPA PIC  X(0001).
           05  HRESAHPI PIC  X(0001).
      *    -------------------------------
           05  HCDTAML PIC S9(0004) COMP.
           05  HCDTAMF PIC  X(0001).
           05  FILLER REDEFINES HCDTAMF.
               10  HCDTAMA PIC  X(0001).
           05  HCDTAMI PIC  X(0001).
      *    -------------------------------
           05  HPCTCDTL PIC S9(0004) COMP.
           05  HPCTCDTF PIC  X(0001).
           05  FILLER REDEFINES HPCTCDTF.
               10  HPCTCDTA PIC  X(0001).
           05  HPCTCDTI PIC  S9(5)V99.
      *    -------------------------------
           05  HEXPL PIC S9(0004) COMP.
           05  HEXPF PIC  X(0001).
           05  FILLER REDEFINES HEXPF.
               10  HEXPA PIC  X(0001).
           05  HEXPI PIC  X(0001).
      *    -------------------------------
           05  HEXPAMTL PIC S9(0004) COMP.
           05  HEXPAMTF PIC  X(0001).
           05  FILLER REDEFINES HEXPAMTF.
               10  HEXPAMTA PIC  X(0001).
           05  HEXPAMTI PIC  S9(5)V99.
      *    -------------------------------
           05  HOCDT01L PIC S9(0004) COMP.
           05  HOCDT01F PIC  X(0001).
           05  FILLER REDEFINES HOCDT01F.
               10  HOCDT01A PIC  X(0001).
           05  HOCDT01I PIC  X(0008).
      *    -------------------------------
           05  HOC01L PIC S9(0004) COMP.
           05  HOC01F PIC  X(0001).
           05  FILLER REDEFINES HOC01F.
               10  HOC01A PIC  X(0001).
           05  HOC01I PIC  X(0001).
      *    -------------------------------
           05  HOCAS01L PIC S9(0004) COMP.
           05  HOCAS01F PIC  X(0001).
           05  FILLER REDEFINES HOCAS01F.
               10  HOCAS01A PIC  X(0001).
           05  HOCAS01I PIC  X(0005).
      *    -------------------------------
           05  HOCDT02L PIC S9(0004) COMP.
           05  HOCDT02F PIC  X(0001).
           05  FILLER REDEFINES HOCDT02F.
               10  HOCDT02A PIC  X(0001).
           05  HOCDT02I PIC  X(0008).
      *    -------------------------------
           05  HOC02L PIC S9(0004) COMP.
           05  HOC02F PIC  X(0001).
           05  FILLER REDEFINES HOC02F.
               10  HOC02A PIC  X(0001).
           05  HOC02I PIC  X(0001).
      *    -------------------------------
           05  HOCAS02L PIC S9(0004) COMP.
           05  HOCAS02F PIC  X(0001).
           05  FILLER REDEFINES HOCAS02F.
               10  HOCAS02A PIC  X(0001).
           05  HOCAS02I PIC  X(0005).
      *    -------------------------------
           05  HOCDT03L PIC S9(0004) COMP.
           05  HOCDT03F PIC  X(0001).
           05  FILLER REDEFINES HOCDT03F.
               10  HOCDT03A PIC  X(0001).
           05  HOCDT03I PIC  X(0008).
      *    -------------------------------
           05  HCO03L PIC S9(0004) COMP.
           05  HCO03F PIC  X(0001).
           05  FILLER REDEFINES HCO03F.
               10  HCO03A PIC  X(0001).
           05  HCO03I PIC  X(0001).
      *    -------------------------------
           05  HCAS03L PIC S9(0004) COMP.
           05  HCAS03F PIC  X(0001).
           05  FILLER REDEFINES HCAS03F.
               10  HCAS03A PIC  X(0001).
           05  HCAS03I PIC  X(0005).
      *    -------------------------------
           05  HOCDT04L PIC S9(0004) COMP.
           05  HOCDT04F PIC  X(0001).
           05  FILLER REDEFINES HOCDT04F.
               10  HOCDT04A PIC  X(0001).
           05  HOCDT04I PIC  X(0008).
      *    -------------------------------
           05  HCO04L PIC S9(0004) COMP.
           05  HCO04F PIC  X(0001).
           05  FILLER REDEFINES HCO04F.
               10  HCO04A PIC  X(0001).
           05  HCO04I PIC  X(0001).
      *    -------------------------------
           05  HCAS04L PIC S9(0004) COMP.
           05  HCAS04F PIC  X(0001).
           05  FILLER REDEFINES HCAS04F.
               10  HCAS04A PIC  X(0001).
           05  HCAS04I PIC  X(0005).
      *    -------------------------------
           05  HCDT05L PIC S9(0004) COMP.
           05  HCDT05F PIC  X(0001).
           05  FILLER REDEFINES HCDT05F.
               10  HCDT05A PIC  X(0001).
           05  HCDT05I PIC  X(0008).
      *    -------------------------------
           05  HCO05L PIC S9(0004) COMP.
           05  HCO05F PIC  X(0001).
           05  FILLER REDEFINES HCO05F.
               10  HCO05A PIC  X(0001).
           05  HCO05I PIC  X(0001).
      *    -------------------------------
           05  HCAS05L PIC S9(0004) COMP.
           05  HCAS05F PIC  X(0001).
           05  FILLER REDEFINES HCAS05F.
               10  HCAS05A PIC  X(0001).
           05  HCAS05I PIC  X(0005).
      *    -------------------------------
           05  HCDT06L PIC S9(0004) COMP.
           05  HCDT06F PIC  X(0001).
           05  FILLER REDEFINES HCDT06F.
               10  HCDT06A PIC  X(0001).
           05  HCDT06I PIC  X(0008).
      *    -------------------------------
           05  HCO06L PIC S9(0004) COMP.
           05  HCO06F PIC  X(0001).
           05  FILLER REDEFINES HCO06F.
               10  HCO06A PIC  X(0001).
           05  HCO06I PIC  X(0001).
      *    -------------------------------
           05  HCAS06L PIC S9(0004) COMP.
           05  HCAS06F PIC  X(0001).
           05  FILLER REDEFINES HCAS06F.
               10  HCAS06A PIC  X(0001).
           05  HCAS06I PIC  X(0005).
      *    -------------------------------
           05  HCDT07L PIC S9(0004) COMP.
           05  HCDT07F PIC  X(0001).
           05  FILLER REDEFINES HCDT07F.
               10  HCDT07A PIC  X(0001).
           05  HCDT07I PIC  X(0008).
      *    -------------------------------
           05  HCO07L PIC S9(0004) COMP.
           05  HCO07F PIC  X(0001).
           05  FILLER REDEFINES HCO07F.
               10  HCO07A PIC  X(0001).
           05  HCO07I PIC  X(0001).
      *    -------------------------------
           05  HCAS07L PIC S9(0004) COMP.
           05  HCAS07F PIC  X(0001).
           05  FILLER REDEFINES HCAS07F.
               10  HCAS07A PIC  X(0001).
           05  HCAS07I PIC  X(0005).
      *    -------------------------------
           05  HCDT08L PIC S9(0004) COMP.
           05  HCDT08F PIC  X(0001).
           05  FILLER REDEFINES HCDT08F.
               10  HCDT08A PIC  X(0001).
           05  HCDT08I PIC  X(0008).
      *    -------------------------------
           05  HCO08L PIC S9(0004) COMP.
           05  HCO08F PIC  X(0001).
           05  FILLER REDEFINES HCO08F.
               10  HCO08A PIC  X(0001).
           05  HCO08I PIC  X(0001).
      *    -------------------------------
           05  HCAS08L PIC S9(0004) COMP.
           05  HCAS08F PIC  X(0001).
           05  FILLER REDEFINES HCAS08F.
               10  HCAS08A PIC  X(0001).
           05  HCAS08I PIC  X(0005).
      *    -------------------------------
           05  HCDT09L PIC S9(0004) COMP.
           05  HCDT09F PIC  X(0001).
           05  FILLER REDEFINES HCDT09F.
               10  HCDT09A PIC  X(0001).
           05  HCDT09I PIC  X(0008).
      *    -------------------------------
           05  HCO09L PIC S9(0004) COMP.
           05  HCO09F PIC  X(0001).
           05  FILLER REDEFINES HCO09F.
               10  HCO09A PIC  X(0001).
           05  HCO09I PIC  X(0001).
      *    -------------------------------
           05  HCAS09L PIC S9(0004) COMP.
           05  HCAS09F PIC  X(0001).
           05  FILLER REDEFINES HCAS09F.
               10  HCAS09A PIC  X(0001).
           05  HCAS09I PIC  X(0005).
      *    -------------------------------
           05  HCDT10L PIC S9(0004) COMP.
           05  HCDT10F PIC  X(0001).
           05  FILLER REDEFINES HCDT10F.
               10  HCDT10A PIC  X(0001).
           05  HCDT10I PIC  X(0008).
      *    -------------------------------
           05  HCO10L PIC S9(0004) COMP.
           05  HCO10F PIC  X(0001).
           05  FILLER REDEFINES HCO10F.
               10  HCO10A PIC  X(0001).
           05  HCO10I PIC  X(0001).
      *    -------------------------------
           05  HCAS10L PIC S9(0004) COMP.
           05  HCAS10F PIC  X(0001).
           05  FILLER REDEFINES HCAS10F.
               10  HCAS10A PIC  X(0001).
           05  HCAS10I PIC  X(0005).
      *    -------------------------------
           05  HEMSG1L PIC S9(0004) COMP.
           05  HEMSG1F PIC  X(0001).
           05  FILLER REDEFINES HEMSG1F.
               10  HEMSG1A PIC  X(0001).
           05  HEMSG1I PIC  X(0079).
      *    -------------------------------
           05  HPFKL PIC S9(0004) COMP.
           05  HPFKF PIC  X(0001).
           05  FILLER REDEFINES HPFKF.
               10  HPFKA PIC  X(0001).
           05  HPFKI PIC  9(2).
       01  EL142HO REDEFINES EL142BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HTYPEO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HRECDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HTLRTYPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HSEQO PIC  9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HMANTBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HMANTONO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HMANTATO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HMAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HRESMANO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HMANAMTO PIC  ZZZZ,ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HITDCO PIC  ZZ,ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HRESFUTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HFUTAMTO PIC  ZZZZ,ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HITDNCO PIC  ZZ,ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HRESIBNO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HIBNAMTO PIC  ZZZZ,ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HLSTCLOO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HRESLFPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HPTCAMTO PIC  ZZZZ,ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HLSTOPEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HRESAHPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HCDTAMO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HPCTCDTO PIC  ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HEXPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HEXPAMTO PIC  ZZZ.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HOCDT01O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HOC01O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HOCAS01O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HOCDT02O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HOC02O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HOCAS02O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HOCDT03O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HCO03O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HCAS03O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HOCDT04O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HCO04O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HCAS04O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HCDT05O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HCO05O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HCAS05O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HCDT06O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HCO06O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HCAS06O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HCDT07O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HCO07O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HCAS07O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HCDT08O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HCO08O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HCAS08O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HCDT09O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HCO09O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HCAS09O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HCDT10O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HCO10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HCAS10O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HEMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HPFKO PIC  99.
      *    -------------------------------
       01  EL142DI REDEFINES EL142BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  DDATEL PIC S9(0004) COMP.
           05  DDATEF PIC  X(0001).
           05  FILLER REDEFINES DDATEF.
               10  DDATEA PIC  X(0001).
           05  DDATEI PIC  X(0008).
      *    -------------------------------
           05  DTIMEL PIC S9(0004) COMP.
           05  DTIMEF PIC  X(0001).
           05  FILLER REDEFINES DTIMEF.
               10  DTIMEA PIC  X(0001).
           05  DTIMEI PIC  X(0005).
      *    -------------------------------
           05  DTYPEL PIC S9(0004) COMP.
           05  DTYPEF PIC  X(0001).
           05  FILLER REDEFINES DTYPEF.
               10  DTYPEA PIC  X(0001).
           05  DTYPEI PIC  X(0010).
      *    -------------------------------
           05  DRECDTEL PIC S9(0004) COMP.
           05  DRECDTEF PIC  X(0001).
           05  FILLER REDEFINES DRECDTEF.
               10  DRECDTEA PIC  X(0001).
           05  DRECDTEI PIC  X(0008).
      *    -------------------------------
           05  DBYL PIC S9(0004) COMP.
           05  DBYF PIC  X(0001).
           05  FILLER REDEFINES DBYF.
               10  DBYA PIC  X(0001).
           05  DBYI PIC  X(0004).
      *    -------------------------------
           05  DTLRTYPL PIC S9(0004) COMP.
           05  DTLRTYPF PIC  X(0001).
           05  FILLER REDEFINES DTLRTYPF.
               10  DTLRTYPA PIC  X(0001).
           05  DTLRTYPI PIC  X(0001).
      *    -------------------------------
           05  DSEQL PIC S9(0004) COMP.
           05  DSEQF PIC  X(0001).
           05  FILLER REDEFINES DSEQF.
               10  DSEQA PIC  X(0001).
           05  DSEQI PIC  X(0004).
      *    -------------------------------
           05  DMANTBYL PIC S9(0004) COMP.
           05  DMANTBYF PIC  X(0001).
           05  FILLER REDEFINES DMANTBYF.
               10  DMANTBYA PIC  X(0001).
           05  DMANTBYI PIC  X(0004).
      *    -------------------------------
           05  DMANTONL PIC S9(0004) COMP.
           05  DMANTONF PIC  X(0001).
           05  FILLER REDEFINES DMANTONF.
               10  DMANTONA PIC  X(0001).
           05  DMANTONI PIC  X(0008).
      *    -------------------------------
           05  DMANTATL PIC S9(0004) COMP.
           05  DMANTATF PIC  X(0001).
           05  FILLER REDEFINES DMANTATF.
               10  DMANTATA PIC  X(0001).
           05  DMANTATI PIC  X(0005).
      *    -------------------------------
           05  DMAINTL PIC S9(0004) COMP.
           05  DMAINTF PIC  X(0001).
           05  FILLER REDEFINES DMAINTF.
               10  DMAINTA PIC  X(0001).
           05  DMAINTI PIC  X(0001).
      *    -------------------------------
           05  DENCCODL PIC S9(0004) COMP.
           05  DENCCODF PIC  X(0001).
           05  FILLER REDEFINES DENCCODF.
               10  DENCCODA PIC  X(0001).
           05  DENCCODI PIC  X(0003).
      *    -------------------------------
           05  DFORMNOL PIC S9(0004) COMP.
           05  DFORMNOF PIC  X(0001).
           05  FILLER REDEFINES DFORMNOF.
               10  DFORMNOA PIC  X(0001).
           05  DFORMNOI PIC  X(0004).
      *    -------------------------------
           05  DPURGHDL PIC S9(0004) COMP.
           05  DPURGHDF PIC  X(0001).
           05  FILLER REDEFINES DPURGHDF.
               10  DPURGHDA PIC  X(0001).
           05  DPURGHDI PIC  X(0017).
      *    -------------------------------
           05  DPURGDTL PIC S9(0004) COMP.
           05  DPURGDTF PIC  X(0001).
           05  FILLER REDEFINES DPURGDTF.
               10  DPURGDTA PIC  X(0001).
           05  DPURGDTI PIC  X(0008).
      *    -------------------------------
           05  DARCHNOL PIC S9(0004) COMP.
           05  DARCHNOF PIC  X(0001).
           05  FILLER REDEFINES DARCHNOF.
               10  DARCHNOA PIC  X(0001).
           05  DARCHNOI PIC  X(0008).
      *    -------------------------------
           05  DBENLETL PIC S9(0004) COMP.
           05  DBENLETF PIC  X(0001).
           05  FILLER REDEFINES DBENLETF.
               10  DBENLETA PIC  X(0001).
           05  DBENLETI PIC  X(0040).
      *    -------------------------------
           05  DDTSENTL PIC S9(0004) COMP.
           05  DDTSENTF PIC  X(0001).
           05  FILLER REDEFINES DDTSENTF.
               10  DDTSENTA PIC  X(0001).
           05  DDTSENTI PIC  X(0008).
      *    -------------------------------
           05  DINPRNTL PIC S9(0004) COMP.
           05  DINPRNTF PIC  X(0001).
           05  FILLER REDEFINES DINPRNTF.
               10  DINPRNTA PIC  X(0001).
           05  DINPRNTI PIC  X(0008).
      *    -------------------------------
           05  DRESENDL PIC S9(0004) COMP.
           05  DRESENDF PIC  X(0001).
           05  FILLER REDEFINES DRESENDF.
               10  DRESENDA PIC  X(0001).
           05  DRESENDI PIC  X(0008).
      *    -------------------------------
           05  DREPRNTL PIC S9(0004) COMP.
           05  DREPRNTF PIC  X(0001).
           05  FILLER REDEFINES DREPRNTF.
               10  DREPRNTA PIC  X(0001).
           05  DREPRNTI PIC  X(0008).
      *    -------------------------------
           05  DREPLYL PIC S9(0004) COMP.
           05  DREPLYF PIC  X(0001).
           05  FILLER REDEFINES DREPLYF.
               10  DREPLYA PIC  X(0001).
           05  DREPLYI PIC  X(0008).
      *    -------------------------------
           05  DRESFRML PIC S9(0004) COMP.
           05  DRESFRMF PIC  X(0001).
           05  FILLER REDEFINES DRESFRMF.
               10  DRESFRMA PIC  X(0001).
           05  DRESFRMI PIC  X(0004).
      *    -------------------------------
           05  DRECEVEL PIC S9(0004) COMP.
           05  DRECEVEF PIC  X(0001).
           05  FILLER REDEFINES DRECEVEF.
               10  DRECEVEA PIC  X(0001).
           05  DRECEVEI PIC  X(0008).
      *    -------------------------------
           05  DAUTOCLL PIC S9(0004) COMP.
           05  DAUTOCLF PIC  X(0001).
           05  FILLER REDEFINES DAUTOCLF.
               10  DAUTOCLA PIC  X(0001).
           05  DAUTOCLI PIC  X(0001).
      *    -------------------------------
           05  DWRITENL PIC S9(0004) COMP.
           05  DWRITENF PIC  X(0001).
           05  FILLER REDEFINES DWRITENF.
               10  DWRITENA PIC  X(0001).
           05  DWRITENI PIC  X(0001).
      *    -------------------------------
           05  DSTOPLTL PIC S9(0004) COMP.
           05  DSTOPLTF PIC  X(0001).
           05  FILLER REDEFINES DSTOPLTF.
               10  DSTOPLTA PIC  X(0001).
           05  DSTOPLTI PIC  X(0008).
      *    -------------------------------
           05  DREASONL PIC S9(0004) COMP.
           05  DREASONF PIC  X(0001).
           05  FILLER REDEFINES DREASONF.
               10  DREASONA PIC  X(0001).
           05  DREASONI PIC  X(0070).
      *    -------------------------------
           05  DMAILTOL PIC S9(0004) COMP.
           05  DMAILTOF PIC  X(0001).
           05  FILLER REDEFINES DMAILTOF.
               10  DMAILTOA PIC  X(0001).
           05  DMAILTOI PIC  X(0030).
      *    -------------------------------
           05  DADDR1L PIC S9(0004) COMP.
           05  DADDR1F PIC  X(0001).
           05  FILLER REDEFINES DADDR1F.
               10  DADDR1A PIC  X(0001).
           05  DADDR1I PIC  X(0030).
      *    -------------------------------
           05  DADDR2L PIC S9(0004) COMP.
           05  DADDR2F PIC  X(0001).
           05  FILLER REDEFINES DADDR2F.
               10  DADDR2A PIC  X(0001).
           05  DADDR2I PIC  X(0030).
      *    -------------------------------
           05  DCITYSTL PIC S9(0004) COMP.
           05  DCITYSTF PIC  X(0001).
           05  FILLER REDEFINES DCITYSTF.
               10  DCITYSTA PIC  X(0001).
           05  DCITYSTI PIC  X(0030).
      *    -------------------------------
           05  DZIPL PIC S9(0004) COMP.
           05  DZIPF PIC  X(0001).
           05  FILLER REDEFINES DZIPF.
               10  DZIPA PIC  X(0001).
           05  DZIPI PIC  X(0010).
      *    -------------------------------
           05  DPHONEL PIC S9(0004) COMP.
           05  DPHONEF PIC  X(0001).
           05  FILLER REDEFINES DPHONEF.
               10  DPHONEA PIC  X(0001).
           05  DPHONEI PIC  X(0012).
      *    -------------------------------
           05  DEMSG1L PIC S9(0004) COMP.
           05  DEMSG1F PIC  X(0001).
           05  FILLER REDEFINES DEMSG1F.
               10  DEMSG1A PIC  X(0001).
           05  DEMSG1I PIC  X(0079).
      *    -------------------------------
           05  DPFKL PIC S9(0004) COMP.
           05  DPFKF PIC  X(0001).
           05  FILLER REDEFINES DPFKF.
               10  DPFKA PIC  X(0001).
           05  DPFKI PIC  9(2).
       01  EL142DO REDEFINES EL142BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DTYPEO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DRECDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DTLRTYPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DSEQO PIC  9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DMANTBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DMANTONO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DMANTATO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DMAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DENCCODO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DFORMNOO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DPURGHDO PIC  X(0017).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DPURGDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DARCHNOO PIC  Z(7)9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DBENLETO PIC  X(0040).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DDTSENTO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DINPRNTO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DRESENDO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DREPRNTO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DREPLYO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DRESFRMO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DRECEVEO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DAUTOCLO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DWRITENO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DSTOPLTO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DREASONO PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DMAILTOO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DADDR1O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DADDR2O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DCITYSTO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DZIPO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DPHONEO PIC  999B999B9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DEMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DPFKO PIC  99.
      *    -------------------------------
       01  EL142JI REDEFINES EL142BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  JDATEL PIC S9(0004) COMP.
           05  JDATEF PIC  X(0001).
           05  FILLER REDEFINES JDATEF.
               10  JDATEA PIC  X(0001).
           05  JDATEI PIC  X(0008).
      *    -------------------------------
           05  JTIMEL PIC S9(0004) COMP.
           05  JTIMEF PIC  X(0001).
           05  FILLER REDEFINES JTIMEF.
               10  JTIMEA PIC  X(0001).
           05  JTIMEI PIC  X(0005).
      *    -------------------------------
           05  JTYPEL PIC S9(0004) COMP.
           05  JTYPEF PIC  X(0001).
           05  FILLER REDEFINES JTYPEF.
               10  JTYPEA PIC  X(0001).
           05  JTYPEI PIC  X(0010).
      *    -------------------------------
           05  JRECDTEL PIC S9(0004) COMP.
           05  JRECDTEF PIC  X(0001).
           05  FILLER REDEFINES JRECDTEF.
               10  JRECDTEA PIC  X(0001).
           05  JRECDTEI PIC  X(0008).
      *    -------------------------------
           05  JBYL PIC S9(0004) COMP.
           05  JBYF PIC  X(0001).
           05  FILLER REDEFINES JBYF.
               10  JBYA PIC  X(0001).
           05  JBYI PIC  X(0004).
      *    -------------------------------
           05  JTLRTYPL PIC S9(0004) COMP.
           05  JTLRTYPF PIC  X(0001).
           05  FILLER REDEFINES JTLRTYPF.
               10  JTLRTYPA PIC  X(0001).
           05  JTLRTYPI PIC  X(0001).
      *    -------------------------------
           05  JSEQL PIC S9(0004) COMP.
           05  JSEQF PIC  X(0001).
           05  FILLER REDEFINES JSEQF.
               10  JSEQA PIC  X(0001).
           05  JSEQI PIC  X(0004).
      *    -------------------------------
           05  JMANTBYL PIC S9(0004) COMP.
           05  JMANTBYF PIC  X(0001).
           05  FILLER REDEFINES JMANTBYF.
               10  JMANTBYA PIC  X(0001).
           05  JMANTBYI PIC  X(0004).
      *    -------------------------------
           05  JMANTONL PIC S9(0004) COMP.
           05  JMANTONF PIC  X(0001).
           05  FILLER REDEFINES JMANTONF.
               10  JMANTONA PIC  X(0001).
           05  JMANTONI PIC  X(0008).
      *    -------------------------------
           05  JMANTATL PIC S9(0004) COMP.
           05  JMANTATF PIC  X(0001).
           05  FILLER REDEFINES JMANTATF.
               10  JMANTATA PIC  X(0001).
           05  JMANTATI PIC  X(0005).
      *    -------------------------------
           05  JMAINTL PIC S9(0004) COMP.
           05  JMAINTF PIC  X(0001).
           05  FILLER REDEFINES JMAINTF.
               10  JMAINTA PIC  X(0001).
           05  JMAINTI PIC  X(0001).
      *    -------------------------------
           05  JDTSENTL PIC S9(0004) COMP.
           05  JDTSENTF PIC  X(0001).
           05  FILLER REDEFINES JDTSENTF.
               10  JDTSENTA PIC  X(0001).
           05  JDTSENTI PIC  X(0008).
      *    -------------------------------
           05  JRESENDL PIC S9(0004) COMP.
           05  JRESENDF PIC  X(0001).
           05  FILLER REDEFINES JRESENDF.
               10  JRESENDA PIC  X(0001).
           05  JRESENDI PIC  X(0008).
      *    -------------------------------
           05  JSI1L PIC S9(0004) COMP.
           05  JSI1F PIC  X(0001).
           05  FILLER REDEFINES JSI1F.
               10  JSI1A PIC  X(0001).
           05  JSI1I PIC  X(0028).
      *    -------------------------------
           05  JREPLYL PIC S9(0004) COMP.
           05  JREPLYF PIC  X(0001).
           05  FILLER REDEFINES JREPLYF.
               10  JREPLYA PIC  X(0001).
           05  JREPLYI PIC  X(0008).
      *    -------------------------------
           05  JSI2L PIC S9(0004) COMP.
           05  JSI2F PIC  X(0001).
           05  FILLER REDEFINES JSI2F.
               10  JSI2A PIC  X(0001).
           05  JSI2I PIC  X(0028).
      *    -------------------------------
           05  JRECEVEL PIC S9(0004) COMP.
           05  JRECEVEF PIC  X(0001).
           05  FILLER REDEFINES JRECEVEF.
               10  JRECEVEA PIC  X(0001).
           05  JRECEVEI PIC  X(0008).
      *    -------------------------------
           05  JSI3L PIC S9(0004) COMP.
           05  JSI3F PIC  X(0001).
           05  FILLER REDEFINES JSI3F.
               10  JSI3A PIC  X(0001).
           05  JSI3I PIC  X(0028).
      *    -------------------------------
           05  JPHYRECL PIC S9(0004) COMP.
           05  JPHYRECF PIC  X(0001).
           05  FILLER REDEFINES JPHYRECF.
               10  JPHYRECA PIC  X(0001).
           05  JPHYRECI PIC  X(0008).
      *    -------------------------------
           05  JEMPRECL PIC S9(0004) COMP.
           05  JEMPRECF PIC  X(0001).
           05  FILLER REDEFINES JEMPRECF.
               10  JEMPRECA PIC  X(0001).
           05  JEMPRECI PIC  X(0008).
      *    -------------------------------
           05  JREMDTL PIC S9(0004) COMP.
           05  JREMDTF PIC  X(0001).
           05  FILLER REDEFINES JREMDTF.
               10  JREMDTA PIC  X(0001).
           05  JREMDTI PIC  X(0008).
      *    -------------------------------
           05  JFORML PIC S9(0004) COMP.
           05  JFORMF PIC  X(0001).
           05  FILLER REDEFINES JFORMF.
               10  JFORMA PIC  X(0001).
           05  JFORMI PIC  X(0008).
      *    -------------------------------
           05  JCARR1L PIC S9(0004) COMP.
           05  JCARR1F PIC  X(0001).
           05  FILLER REDEFINES JCARR1F.
               10  JCARR1A PIC  X(0001).
           05  JCARR1I PIC  X(0001).
      *    -------------------------------
           05  JCLAIM1L PIC S9(0004) COMP.
           05  JCLAIM1F PIC  X(0001).
           05  FILLER REDEFINES JCLAIM1F.
               10  JCLAIM1A PIC  X(0001).
           05  JCLAIM1I PIC  X(0007).
      *    -------------------------------
           05  JCERT1L PIC S9(0004) COMP.
           05  JCERT1F PIC  X(0001).
           05  FILLER REDEFINES JCERT1F.
               10  JCERT1A PIC  X(0001).
           05  JCERT1I PIC  X(0011).
      *    -------------------------------
           05  JCARR2L PIC S9(0004) COMP.
           05  JCARR2F PIC  X(0001).
           05  FILLER REDEFINES JCARR2F.
               10  JCARR2A PIC  X(0001).
           05  JCARR2I PIC  X(0001).
      *    -------------------------------
           05  JCLAIM2L PIC S9(0004) COMP.
           05  JCLAIM2F PIC  X(0001).
           05  FILLER REDEFINES JCLAIM2F.
               10  JCLAIM2A PIC  X(0001).
           05  JCLAIM2I PIC  X(0007).
      *    -------------------------------
           05  JCERT2L PIC S9(0004) COMP.
           05  JCERT2F PIC  X(0001).
           05  FILLER REDEFINES JCERT2F.
               10  JCERT2A PIC  X(0001).
           05  JCERT2I PIC  X(0011).
      *    -------------------------------
           05  JMAILTOL PIC S9(0004) COMP.
           05  JMAILTOF PIC  X(0001).
           05  FILLER REDEFINES JMAILTOF.
               10  JMAILTOA PIC  X(0001).
           05  JMAILTOI PIC  X(0030).
      *    -------------------------------
           05  JADDR1L PIC S9(0004) COMP.
           05  JADDR1F PIC  X(0001).
           05  FILLER REDEFINES JADDR1F.
               10  JADDR1A PIC  X(0001).
           05  JADDR1I PIC  X(0030).
      *    -------------------------------
           05  JADDR2L PIC S9(0004) COMP.
           05  JADDR2F PIC  X(0001).
           05  FILLER REDEFINES JADDR2F.
               10  JADDR2A PIC  X(0001).
           05  JADDR2I PIC  X(0030).
      *    -------------------------------
           05  JCITYSTL PIC S9(0004) COMP.
           05  JCITYSTF PIC  X(0001).
           05  FILLER REDEFINES JCITYSTF.
               10  JCITYSTA PIC  X(0001).
           05  JCITYSTI PIC  X(0030).
      *    -------------------------------
           05  JZIPL PIC S9(0004) COMP.
           05  JZIPF PIC  X(0001).
           05  FILLER REDEFINES JZIPF.
               10  JZIPA PIC  X(0001).
           05  JZIPI PIC  X(0010).
      *    -------------------------------
           05  JPHONEL PIC S9(0004) COMP.
           05  JPHONEF PIC  X(0001).
           05  FILLER REDEFINES JPHONEF.
               10  JPHONEA PIC  X(0001).
           05  JPHONEI PIC  X(0012).
      *    -------------------------------
           05  JEMSG1L PIC S9(0004) COMP.
           05  JEMSG1F PIC  X(0001).
           05  FILLER REDEFINES JEMSG1F.
               10  JEMSG1A PIC  X(0001).
           05  JEMSG1I PIC  X(0079).
      *    -------------------------------
           05  JPFKL PIC S9(0004) COMP.
           05  JPFKF PIC  X(0001).
           05  FILLER REDEFINES JPFKF.
               10  JPFKA PIC  X(0001).
           05  JPFKI PIC  9(2).
       01  EL142JO REDEFINES EL142BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JTYPEO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JRECDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JTLRTYPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JSEQO PIC  9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JMANTBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JMANTONO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JMANTATO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JMAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JDTSENTO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JRESENDO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JSI1O PIC  X(0028).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JREPLYO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JSI2O PIC  X(0028).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JRECEVEO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JSI3O PIC  X(0028).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JPHYRECO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JEMPRECO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JREMDTO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JFORMO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JCARR1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JCLAIM1O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JCERT1O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JCARR2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JCLAIM2O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JCERT2O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JMAILTOO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JADDR1O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JADDR2O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JCITYSTO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JZIPO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JPHONEO PIC  999B999B9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JEMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JPFKO PIC  99.
      *    -------------------------------
       01  EL142AI REDEFINES EL142BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  ADATEL PIC S9(0004) COMP.
           05  ADATEF PIC  X(0001).
           05  FILLER REDEFINES ADATEF.
               10  ADATEA PIC  X(0001).
           05  ADATEI PIC  X(0008).
      *    -------------------------------
           05  ATIMEL PIC S9(0004) COMP.
           05  ATIMEF PIC  X(0001).
           05  FILLER REDEFINES ATIMEF.
               10  ATIMEA PIC  X(0001).
           05  ATIMEI PIC  X(0005).
      *    -------------------------------
           05  ARECDTEL PIC S9(0004) COMP.
           05  ARECDTEF PIC  X(0001).
           05  FILLER REDEFINES ARECDTEF.
               10  ARECDTEA PIC  X(0001).
           05  ARECDTEI PIC  X(0008).
      *    -------------------------------
           05  AREMINDL PIC S9(0004) COMP.
           05  AREMINDF PIC  X(0001).
           05  FILLER REDEFINES AREMINDF.
               10  AREMINDA PIC  X(0001).
           05  AREMINDI PIC  X(0001).
      *    -------------------------------
           05  ALETTERL PIC S9(0004) COMP.
           05  ALETTERF PIC  X(0001).
           05  FILLER REDEFINES ALETTERF.
               10  ALETTERA PIC  X(0001).
           05  ALETTERI PIC  X(0001).
      *    -------------------------------
           05  APAYMNTL PIC S9(0004) COMP.
           05  APAYMNTF PIC  X(0001).
           05  FILLER REDEFINES APAYMNTF.
               10  APAYMNTA PIC  X(0001).
           05  APAYMNTI PIC  X(0001).
      *    -------------------------------
           05  AAUTOPAL PIC S9(0004) COMP.
           05  AAUTOPAF PIC  X(0001).
           05  FILLER REDEFINES AAUTOPAF.
               10  AAUTOPAA PIC  X(0001).
           05  AAUTOPAI PIC  X(0001).
      *    -------------------------------
           05  ANOTESL PIC S9(0004) COMP.
           05  ANOTESF PIC  X(0001).
           05  FILLER REDEFINES ANOTESF.
               10  ANOTESA PIC  X(0001).
           05  ANOTESI PIC  X(0001).
      *    -------------------------------
           05  ARESEXPL PIC S9(0004) COMP.
           05  ARESEXPF PIC  X(0001).
           05  FILLER REDEFINES ARESEXPF.
               10  ARESEXPA PIC  X(0001).
           05  ARESEXPI PIC  X(0001).
      *    -------------------------------
           05  ADENIALL PIC S9(0004) COMP.
           05  ADENIALF PIC  X(0001).
           05  FILLER REDEFINES ADENIALF.
               10  ADENIALA PIC  X(0001).
           05  ADENIALI PIC  X(0001).
      *    -------------------------------
           05  AIDCL PIC S9(0004) COMP.
           05  AIDCF PIC  X(0001).
           05  FILLER REDEFINES AIDCF.
               10  AIDCA PIC  X(0001).
           05  AIDCI PIC  X(0001).
      *    -------------------------------
           05  AFORMSL PIC S9(0004) COMP.
           05  AFORMSF PIC  X(0001).
           05  FILLER REDEFINES AFORMSF.
               10  AFORMSA PIC  X(0001).
           05  AFORMSI PIC  X(0001).
      *    -------------------------------
           05  ASEQL PIC S9(0004) COMP.
           05  ASEQF PIC  X(0001).
           05  FILLER REDEFINES ASEQF.
               10  ASEQA PIC  X(0001).
           05  ASEQI PIC  X(0004).
      *    -------------------------------
           05  AEMSG1L PIC S9(0004) COMP.
           05  AEMSG1F PIC  X(0001).
           05  FILLER REDEFINES AEMSG1F.
               10  AEMSG1A PIC  X(0001).
           05  AEMSG1I PIC  X(0079).
      *    -------------------------------
           05  AEMSG2L PIC S9(0004) COMP.
           05  AEMSG2F PIC  X(0001).
           05  FILLER REDEFINES AEMSG2F.
               10  AEMSG2A PIC  X(0001).
           05  AEMSG2I PIC  X(0079).
      *    -------------------------------
           05  AEMSG3L PIC S9(0004) COMP.
           05  AEMSG3F PIC  X(0001).
           05  FILLER REDEFINES AEMSG3F.
               10  AEMSG3A PIC  X(0001).
           05  AEMSG3I PIC  X(0079).
       01  EL142AO REDEFINES EL142BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARECDTEO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AREMINDO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALETTERO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAYMNTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAUTOPAO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANOTESO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARESEXPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADENIALO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIDCO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFORMSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASEQO PIC  9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEMSG2O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEMSG3O PIC  X(0079).
      *    -------------------------------
       01  EL142B2I REDEFINES EL142BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  KDATEL PIC S9(0004) COMP.
           05  KDATEF PIC  X(0001).
           05  FILLER REDEFINES KDATEF.
               10  KDATEA PIC  X(0001).
           05  KDATEI PIC  X(0008).
      *    -------------------------------
           05  KTIMEL PIC S9(0004) COMP.
           05  KTIMEF PIC  X(0001).
           05  FILLER REDEFINES KTIMEF.
               10  KTIMEA PIC  X(0001).
           05  KTIMEI PIC  X(0005).
      *    -------------------------------
           05  KMAINTL PIC S9(0004) COMP.
           05  KMAINTF PIC  X(0001).
           05  FILLER REDEFINES KMAINTF.
               10  KMAINTA PIC  X(0001).
           05  KMAINTI PIC  X(0001).
      *    -------------------------------
           05  KCONTRLL PIC S9(0004) COMP.
           05  KCONTRLF PIC  X(0001).
           05  FILLER REDEFINES KCONTRLF.
               10  KCONTRLA PIC  X(0001).
           05  KCONTRLI PIC  9(8).
      *    -------------------------------
           05  KSEQL PIC S9(0004) COMP.
           05  KSEQF PIC  X(0001).
           05  FILLER REDEFINES KSEQF.
               10  KSEQA PIC  X(0001).
           05  KSEQI PIC  9(4).
      *    -------------------------------
           05  KTYPEL PIC S9(0004) COMP.
           05  KTYPEF PIC  X(0001).
           05  FILLER REDEFINES KTYPEF.
               10  KTYPEA PIC  X(0001).
           05  KTYPEI PIC  X(0001).
      *    -------------------------------
           05  KTSEQL PIC S9(0004) COMP.
           05  KTSEQF PIC  X(0001).
           05  FILLER REDEFINES KTSEQF.
               10  KTSEQA PIC  X(0001).
           05  KTSEQI PIC  9(4).
      *    -------------------------------
           05  KCARRL PIC S9(0004) COMP.
           05  KCARRF PIC  X(0001).
           05  FILLER REDEFINES KCARRF.
               10  KCARRA PIC  X(0001).
           05  KCARRI PIC  X(0001).
      *    -------------------------------
           05  KCTYPEL PIC S9(0004) COMP.
           05  KCTYPEF PIC  X(0001).
           05  FILLER REDEFINES KCTYPEF.
               10  KCTYPEA PIC  X(0001).
           05  KCTYPEI PIC  X(0001).
      *    -------------------------------
           05  KCLMNOL PIC S9(0004) COMP.
           05  KCLMNOF PIC  X(0001).
           05  FILLER REDEFINES KCLMNOF.
               10  KCLMNOA PIC  X(0001).
           05  KCLMNOI PIC  X(0007).
      *    -------------------------------
           05  KCOVERL PIC S9(0004) COMP.
           05  KCOVERF PIC  X(0001).
           05  FILLER REDEFINES KCOVERF.
               10  KCOVERA PIC  X(0001).
           05  KCOVERI PIC  X(0002).
      *    -------------------------------
           05  KCERTNOL PIC S9(0004) COMP.
           05  KCERTNOF PIC  X(0001).
           05  FILLER REDEFINES KCERTNOF.
               10  KCERTNOA PIC  X(0001).
           05  KCERTNOI PIC  X(0011).
      *    -------------------------------
           05  KCKNOL PIC S9(0004) COMP.
           05  KCKNOF PIC  X(0001).
           05  FILLER REDEFINES KCKNOF.
               10  KCKNOA PIC  X(0001).
           05  KCKNOI PIC  X(0007).
      *    -------------------------------
           05  KCKNO2L PIC S9(0004) COMP.
           05  KCKNO2F PIC  X(0001).
           05  FILLER REDEFINES KCKNO2F.
               10  KCKNO2A PIC  X(0001).
           05  KCKNO2I PIC  X(0007).
      *    -------------------------------
           05  KPAYTYPL PIC S9(0004) COMP.
           05  KPAYTYPF PIC  X(0001).
           05  FILLER REDEFINES KPAYTYPF.
               10  KPAYTYPA PIC  X(0001).
           05  KPAYTYPI PIC  X(0017).
      *    -------------------------------
           05  KCKAMTL PIC S9(0004) COMP.
           05  KCKAMTF PIC  X(0001).
           05  FILLER REDEFINES KCKAMTF.
               10  KCKAMTA PIC  X(0001).
           05  KCKAMTI PIC  S9(11)V9(2).
      *    -------------------------------
           05  KBYL PIC S9(0004) COMP.
           05  KBYF PIC  X(0001).
           05  FILLER REDEFINES KBYF.
               10  KBYA PIC  X(0001).
           05  KBYI PIC  X(0004).
      *    -------------------------------
           05  KCKDATEL PIC S9(0004) COMP.
           05  KCKDATEF PIC  X(0001).
           05  FILLER REDEFINES KCKDATEF.
               10  KCKDATEA PIC  X(0001).
           05  KCKDATEI PIC  X(0008).
      *    -------------------------------
           05  KVOIDL PIC S9(0004) COMP.
           05  KVOIDF PIC  X(0001).
           05  FILLER REDEFINES KVOIDF.
               10  KVOIDA PIC  X(0001).
           05  KVOIDI PIC  X(0003).
      *    -------------------------------
           05  KTIMPRTL PIC S9(0004) COMP.
           05  KTIMPRTF PIC  X(0001).
           05  FILLER REDEFINES KTIMPRTF.
               10  KTIMPRTA PIC  X(0001).
           05  KTIMPRTI PIC  9(4).
      *    -------------------------------
           05  KPRENOL PIC S9(0004) COMP.
           05  KPRENOF PIC  X(0001).
           05  FILLER REDEFINES KPRENOF.
               10  KPRENOA PIC  X(0001).
           05  KPRENOI PIC  X(0003).
      *    -------------------------------
           05  KEMSG1L PIC S9(0004) COMP.
           05  KEMSG1F PIC  X(0001).
           05  FILLER REDEFINES KEMSG1F.
               10  KEMSG1A PIC  X(0001).
           05  KEMSG1I PIC  X(0079).
      *    -------------------------------
           05  KEMSG2L PIC S9(0004) COMP.
           05  KEMSG2F PIC  X(0001).
           05  FILLER REDEFINES KEMSG2F.
               10  KEMSG2A PIC  X(0001).
           05  KEMSG2I PIC  X(0079).
      *    -------------------------------
           05  KEMSG3L PIC S9(0004) COMP.
           05  KEMSG3F PIC  X(0001).
           05  FILLER REDEFINES KEMSG3F.
               10  KEMSG3A PIC  X(0001).
           05  KEMSG3I PIC  X(0079).
      *    -------------------------------
           05  KPFKL PIC S9(0004) COMP.
           05  KPFKF PIC  X(0001).
           05  FILLER REDEFINES KPFKF.
               10  KPFKA PIC  X(0001).
           05  KPFKI PIC  9(2).
       01  EL142B2O REDEFINES EL142BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KMAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KCONTRLO PIC  ZZZZZZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KSEQO PIC  ZZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KTYPEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KTSEQO PIC  ZZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KCARRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KCTYPEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KCLMNOO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KCOVERO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KCERTNOO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KCKNOO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KCKNO2O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KPAYTYPO PIC  X(0017).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KCKAMTO PIC  Z,ZZZ,ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KCKDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KVOIDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KTIMPRTO PIC  ZZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KPRENOO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KEMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KEMSG2O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KEMSG3O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KPFKO PIC  99.
      *    -------------------------------
       01  EL142CI REDEFINES EL142BI.
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
           05  CTYPEL PIC S9(0004) COMP.
           05  CTYPEF PIC  X(0001).
           05  FILLER REDEFINES CTYPEF.
               10  CTYPEA PIC  X(0001).
           05  CTYPEI PIC  X(0010).
      *    -------------------------------
           05  CRECDTEL PIC S9(0004) COMP.
           05  CRECDTEF PIC  X(0001).
           05  FILLER REDEFINES CRECDTEF.
               10  CRECDTEA PIC  X(0001).
           05  CRECDTEI PIC  X(0008).
      *    -------------------------------
           05  CBYL PIC S9(0004) COMP.
           05  CBYF PIC  X(0001).
           05  FILLER REDEFINES CBYF.
               10  CBYA PIC  X(0001).
           05  CBYI PIC  X(0004).
      *    -------------------------------
           05  CTLRTYPL PIC S9(0004) COMP.
           05  CTLRTYPF PIC  X(0001).
           05  FILLER REDEFINES CTLRTYPF.
               10  CTLRTYPA PIC  X(0001).
           05  CTLRTYPI PIC  X(0001).
      *    -------------------------------
           05  CSEQL PIC S9(0004) COMP.
           05  CSEQF PIC  X(0001).
           05  FILLER REDEFINES CSEQF.
               10  CSEQA PIC  X(0001).
           05  CSEQI PIC  X(0004).
      *    -------------------------------
           05  CMANTBYL PIC S9(0004) COMP.
           05  CMANTBYF PIC  X(0001).
           05  FILLER REDEFINES CMANTBYF.
               10  CMANTBYA PIC  X(0001).
           05  CMANTBYI PIC  X(0004).
      *    -------------------------------
           05  CMANTONL PIC S9(0004) COMP.
           05  CMANTONF PIC  X(0001).
           05  FILLER REDEFINES CMANTONF.
               10  CMANTONA PIC  X(0001).
           05  CMANTONI PIC  X(0008).
      *    -------------------------------
           05  CMANTATL PIC S9(0004) COMP.
           05  CMANTATF PIC  X(0001).
           05  FILLER REDEFINES CMANTATF.
               10  CMANTATA PIC  X(0001).
           05  CMANTATI PIC  X(0005).
      *    -------------------------------
           05  CMAINTL PIC S9(0004) COMP.
           05  CMAINTF PIC  X(0001).
           05  FILLER REDEFINES CMAINTF.
               10  CMAINTA PIC  X(0001).
           05  CMAINTI PIC  X(0001).
      *    -------------------------------
           05  CEFFDTEL PIC S9(0004) COMP.
           05  CEFFDTEF PIC  X(0001).
           05  FILLER REDEFINES CEFFDTEF.
               10  CEFFDTEA PIC  X(0001).
           05  CEFFDTEI PIC  X(0008).
      *    -------------------------------
           05  CREPDTEL PIC S9(0004) COMP.
           05  CREPDTEF PIC  X(0001).
           05  FILLER REDEFINES CREPDTEF.
               10  CREPDTEA PIC  X(0001).
           05  CREPDTEI PIC  X(0008).
      *    -------------------------------
           05  C1STPAL PIC S9(0004) COMP.
           05  C1STPAF PIC  X(0001).
           05  FILLER REDEFINES C1STPAF.
               10  C1STPAA PIC  X(0001).
           05  C1STPAI PIC  X(0013).
      *    -------------------------------
           05  CREGPAL PIC S9(0004) COMP.
           05  CREGPAF PIC  X(0001).
           05  FILLER REDEFINES CREGPAF.
               10  CREGPAA PIC  X(0001).
           05  CREGPAI PIC  X(0013).
      *    -------------------------------
           05  C1STPSL PIC S9(0004) COMP.
           05  C1STPSF PIC  X(0001).
           05  FILLER REDEFINES C1STPSF.
               10  C1STPSA PIC  X(0001).
           05  C1STPSI PIC  X(0008).
      *    -------------------------------
           05  CLSTPSL PIC S9(0004) COMP.
           05  CLSTPSF PIC  X(0001).
           05  FILLER REDEFINES CLSTPSF.
               10  CLSTPSA PIC  X(0001).
           05  CLSTPSI PIC  X(0008).
      *    -------------------------------
           05  CDIFPL PIC S9(0004) COMP.
           05  CDIFPF PIC  X(0001).
           05  FILLER REDEFINES CDIFPF.
               10  CDIFPA PIC  X(0001).
           05  CDIFPI PIC  X(0006).
      *    -------------------------------
           05  CMBPAYL PIC S9(0004) COMP.
           05  CMBPAYF PIC  X(0001).
           05  FILLER REDEFINES CMBPAYF.
               10  CMBPAYA PIC  X(0001).
           05  CMBPAYI PIC  X(0006).
      *    -------------------------------
           05  CLSTPATL PIC S9(0004) COMP.
           05  CLSTPATF PIC  X(0001).
           05  FILLER REDEFINES CLSTPATF.
               10  CLSTPATA PIC  X(0001).
           05  CLSTPATI PIC  X(0007).
      *    -------------------------------
           05  CPAYEEL PIC S9(0004) COMP.
           05  CPAYEEF PIC  X(0001).
           05  FILLER REDEFINES CPAYEEF.
               10  CPAYEEA PIC  X(0001).
           05  CPAYEEI PIC  X(0011).
      *    -------------------------------
           05  CENDLETL PIC S9(0004) COMP.
           05  CENDLETF PIC  X(0001).
           05  FILLER REDEFINES CENDLETF.
               10  CENDLETA PIC  X(0001).
           05  CENDLETI PIC  X(0004).
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
           05  CEMSG2I PIC  X(0079).
      *    -------------------------------
           05  CPFKL PIC S9(0004) COMP.
           05  CPFKF PIC  X(0001).
           05  FILLER REDEFINES CPFKF.
               10  CPFKA PIC  X(0001).
           05  CPFKI PIC  9(2).
       01  EL142CO REDEFINES EL142BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CTYPEO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRECDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CTLRTYPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSEQO PIC  9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMANTBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMANTONO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMANTATO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CEFFDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CREPDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  C1STPAO PIC  Z,ZZZ,ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CREGPAO PIC  Z,ZZZ,ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  C1STPSO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLSTPSO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CDIFPO PIC  Z,ZZ9-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMBPAYO PIC  Z,ZZ9-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLSTPATO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPAYEEO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CENDLETO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CEMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CEMSG2O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPFKO PIC  99.
      *    -------------------------------
       01  EL142D2I REDEFINES EL142BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  LDATEL PIC S9(0004) COMP.
           05  LDATEF PIC  X(0001).
           05  FILLER REDEFINES LDATEF.
               10  LDATEA PIC  X(0001).
           05  LDATEI PIC  X(0008).
      *    -------------------------------
           05  LTIMEL PIC S9(0004) COMP.
           05  LTIMEF PIC  X(0001).
           05  FILLER REDEFINES LTIMEF.
               10  LTIMEA PIC  X(0001).
           05  LTIMEI PIC  X(0005).
      *    -------------------------------
           05  LARCHNOL PIC S9(0004) COMP.
           05  LARCHNOF PIC  X(0001).
           05  FILLER REDEFINES LARCHNOF.
               10  LARCHNOA PIC  X(0001).
           05  LARCHNOI PIC  X(0008).
      *    -------------------------------
           05  LNXTDUEL PIC S9(0004) COMP.
           05  LNXTDUEF PIC  X(0001).
           05  FILLER REDEFINES LNXTDUEF.
               10  LNXTDUEA PIC  X(0001).
           05  LNXTDUEI PIC  X(0008).
      *    -------------------------------
           05  LCARRL PIC S9(0004) COMP.
           05  LCARRF PIC  X(0001).
           05  FILLER REDEFINES LCARRF.
               10  LCARRA PIC  X(0001).
           05  LCARRI PIC  X(0001).
      *    -------------------------------
           05  LAUTOPYL PIC S9(0004) COMP.
           05  LAUTOPYF PIC  X(0001).
           05  FILLER REDEFINES LAUTOPYF.
               10  LAUTOPYA PIC  X(0001).
           05  LAUTOPYI PIC  X(0008).
      *    -------------------------------
           05  LCLMNOL PIC S9(0004) COMP.
           05  LCLMNOF PIC  X(0001).
           05  FILLER REDEFINES LCLMNOF.
               10  LCLMNOA PIC  X(0001).
           05  LCLMNOI PIC  X(0007).
      *    -------------------------------
           05  LCREATNL PIC S9(0004) COMP.
           05  LCREATNF PIC  X(0001).
           05  FILLER REDEFINES LCREATNF.
               10  LCREATNA PIC  X(0001).
           05  LCREATNI PIC  X(0001).
      *    -------------------------------
           05  LCRTNOL PIC S9(0004) COMP.
           05  LCRTNOF PIC  X(0001).
           05  FILLER REDEFINES LCRTNOF.
               10  LCRTNOA PIC  X(0001).
           05  LCRTNOI PIC  X(0011).
      *    -------------------------------
           05  LORIGARL PIC S9(0004) COMP.
           05  LORIGARF PIC  X(0001).
           05  FILLER REDEFINES LORIGARF.
               10  LORIGARA PIC  X(0001).
           05  LORIGARI PIC  X(0008).
      *    -------------------------------
           05  LLETRIDL PIC S9(0004) COMP.
           05  LLETRIDF PIC  X(0001).
           05  FILLER REDEFINES LLETRIDF.
               10  LLETRIDA PIC  X(0001).
           05  LLETRIDI PIC  X(0004).
      *    -------------------------------
           05  LPROMPTL PIC S9(0004) COMP.
           05  LPROMPTF PIC  X(0001).
           05  FILLER REDEFINES LPROMPTF.
               10  LPROMPTA PIC  X(0001).
           05  LPROMPTI PIC  X(0001).
      *    -------------------------------
           05  LPROCL PIC S9(0004) COMP.
           05  LPROCF PIC  X(0001).
           05  FILLER REDEFINES LPROCF.
               10  LPROCA PIC  X(0001).
           05  LPROCI PIC  X(0004).
      *    -------------------------------
           05  LORIGENL PIC S9(0004) COMP.
           05  LORIGENF PIC  X(0001).
           05  FILLER REDEFINES LORIGENF.
               10  LORIGENA PIC  X(0001).
           05  LORIGENI PIC  X(0001).
      *    -------------------------------
           05  LCREDTEL PIC S9(0004) COMP.
           05  LCREDTEF PIC  X(0001).
           05  FILLER REDEFINES LCREDTEF.
               10  LCREDTEA PIC  X(0001).
           05  LCREDTEI PIC  X(0008).
      *    -------------------------------
           05  LNOCPYSL PIC S9(0004) COMP.
           05  LNOCPYSF PIC  X(0001).
           05  FILLER REDEFINES LNOCPYSF.
               10  LNOCPYSA PIC  X(0001).
           05  LNOCPYSI PIC  X(0003).
      *    -------------------------------
           05  LINPRNTL PIC S9(0004) COMP.
           05  LINPRNTF PIC  X(0001).
           05  FILLER REDEFINES LINPRNTF.
               10  LINPRNTA PIC  X(0001).
           05  LINPRNTI PIC  X(0008).
      *    -------------------------------
           05  LENCCODL PIC S9(0004) COMP.
           05  LENCCODF PIC  X(0001).
           05  FILLER REDEFINES LENCCODF.
               10  LENCCODA PIC  X(0001).
           05  LENCCODI PIC  X(0003).
      *    -------------------------------
           05  LFUPDTEL PIC S9(0004) COMP.
           05  LFUPDTEF PIC  X(0001).
           05  FILLER REDEFINES LFUPDTEF.
               10  LFUPDTEA PIC  X(0001).
           05  LFUPDTEI PIC  X(0008).
      *    -------------------------------
           05  LRESDTEL PIC S9(0004) COMP.
           05  LRESDTEF PIC  X(0001).
           05  FILLER REDEFINES LRESDTEF.
               10  LRESDTEA PIC  X(0001).
           05  LRESDTEI PIC  X(0008).
      *    -------------------------------
           05  LRSLTIDL PIC S9(0004) COMP.
           05  LRSLTIDF PIC  X(0001).
           05  FILLER REDEFINES LRSLTIDF.
               10  LRSLTIDA PIC  X(0001).
           05  LRSLTIDI PIC  X(0004).
      *    -------------------------------
           05  LREPRNTL PIC S9(0004) COMP.
           05  LREPRNTF PIC  X(0001).
           05  FILLER REDEFINES LREPRNTF.
               10  LREPRNTA PIC  X(0001).
           05  LREPRNTI PIC  X(0008).
      *    -------------------------------
           05  LADDTYPL PIC S9(0004) COMP.
           05  LADDTYPF PIC  X(0001).
           05  FILLER REDEFINES LADDTYPF.
               10  LADDTYPA PIC  X(0001).
           05  LADDTYPI PIC  X(0001).
      *    -------------------------------
           05  LCORSEQL PIC S9(0004) COMP.
           05  LCORSEQF PIC  X(0001).
           05  FILLER REDEFINES LCORSEQF.
               10  LCORSEQA PIC  X(0001).
           05  LCORSEQI PIC  X(0006).
      *    -------------------------------
           05  L1STPRTL PIC S9(0004) COMP.
           05  L1STPRTF PIC  X(0001).
           05  FILLER REDEFINES L1STPRTF.
               10  L1STPRTA PIC  X(0001).
           05  L1STPRTI PIC  X(0008).
      *    -------------------------------
           05  LEMSG1L PIC S9(0004) COMP.
           05  LEMSG1F PIC  X(0001).
           05  FILLER REDEFINES LEMSG1F.
               10  LEMSG1A PIC  X(0001).
           05  LEMSG1I PIC  X(0079).
      *    -------------------------------
           05  LEMSG2L PIC S9(0004) COMP.
           05  LEMSG2F PIC  X(0001).
           05  FILLER REDEFINES LEMSG2F.
               10  LEMSG2A PIC  X(0001).
           05  LEMSG2I PIC  X(0079).
      *    -------------------------------
           05  LEMSG3L PIC S9(0004) COMP.
           05  LEMSG3F PIC  X(0001).
           05  FILLER REDEFINES LEMSG3F.
               10  LEMSG3A PIC  X(0001).
           05  LEMSG3I PIC  X(0079).
      *    -------------------------------
           05  LPFKL PIC S9(0004) COMP.
           05  LPFKF PIC  X(0001).
           05  FILLER REDEFINES LPFKF.
               10  LPFKA PIC  X(0001).
           05  LPFKI PIC  9(2).
       01  EL142D2O REDEFINES EL142BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LARCHNOO PIC  99999999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LNXTDUEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCARRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LAUTOPYO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCLMNOO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCREATNO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCRTNOO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LORIGARO PIC  99999999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LLETRIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LPROMPTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LPROCO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LORIGENO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCREDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LNOCPYSO PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINPRNTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LENCCODO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFUPDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LRESDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LRSLTIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LREPRNTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LADDTYPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCORSEQO PIC  999999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L1STPRTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LEMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LEMSG2O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LEMSG3O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LPFKO PIC  99.
      *    -------------------------------
       01  EL142EI REDEFINES EL142BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  EDATEL PIC S9(0004) COMP.
           05  EDATEF PIC  X(0001).
           05  FILLER REDEFINES EDATEF.
               10  EDATEA PIC  X(0001).
           05  EDATEI PIC  X(0008).
      *    -------------------------------
           05  ETIMEL PIC S9(0004) COMP.
           05  ETIMEF PIC  X(0001).
           05  FILLER REDEFINES ETIMEF.
               10  ETIMEA PIC  X(0001).
           05  ETIMEI PIC  X(0005).
      *    -------------------------------
           05  ETYPEL PIC S9(0004) COMP.
           05  ETYPEF PIC  X(0001).
           05  FILLER REDEFINES ETYPEF.
               10  ETYPEA PIC  X(0001).
           05  ETYPEI PIC  X(0010).
      *    -------------------------------
           05  ERECDTEL PIC S9(0004) COMP.
           05  ERECDTEF PIC  X(0001).
           05  FILLER REDEFINES ERECDTEF.
               10  ERECDTEA PIC  X(0001).
           05  ERECDTEI PIC  X(0008).
      *    -------------------------------
           05  EBYL PIC S9(0004) COMP.
           05  EBYF PIC  X(0001).
           05  FILLER REDEFINES EBYF.
               10  EBYA PIC  X(0001).
           05  EBYI PIC  X(0004).
      *    -------------------------------
           05  ETLRTYPL PIC S9(0004) COMP.
           05  ETLRTYPF PIC  X(0001).
           05  FILLER REDEFINES ETLRTYPF.
               10  ETLRTYPA PIC  X(0001).
           05  ETLRTYPI PIC  X(0001).
      *    -------------------------------
           05  ESEQL PIC S9(0004) COMP.
           05  ESEQF PIC  X(0001).
           05  FILLER REDEFINES ESEQF.
               10  ESEQA PIC  X(0001).
           05  ESEQI PIC  X(0004).
      *    -------------------------------
           05  EMANTBYL PIC S9(0004) COMP.
           05  EMANTBYF PIC  X(0001).
           05  FILLER REDEFINES EMANTBYF.
               10  EMANTBYA PIC  X(0001).
           05  EMANTBYI PIC  X(0004).
      *    -------------------------------
           05  EMANTONL PIC S9(0004) COMP.
           05  EMANTONF PIC  X(0001).
           05  FILLER REDEFINES EMANTONF.
               10  EMANTONA PIC  X(0001).
           05  EMANTONI PIC  X(0008).
      *    -------------------------------
           05  EMANTATL PIC S9(0004) COMP.
           05  EMANTATF PIC  X(0001).
           05  FILLER REDEFINES EMANTATF.
               10  EMANTATA PIC  X(0001).
           05  EMANTATI PIC  X(0005).
      *    -------------------------------
           05  EMAINTL PIC S9(0004) COMP.
           05  EMAINTF PIC  X(0001).
           05  FILLER REDEFINES EMAINTF.
               10  EMAINTA PIC  X(0001).
           05  EMAINTI PIC  X(0001).
      *    -------------------------------
           05  ETYPENL PIC S9(0004) COMP.
           05  ETYPENF PIC  X(0001).
           05  FILLER REDEFINES ETYPENF.
               10  ETYPENA PIC  X(0001).
           05  ETYPENI PIC  X(0001).
      *    -------------------------------
           05  ECALLTL PIC S9(0004) COMP.
           05  ECALLTF PIC  X(0001).
           05  FILLER REDEFINES ECALLTF.
               10  ECALLTA PIC  X(0001).
           05  ECALLTI PIC  X(0010).
      *    -------------------------------
           05  ECALLL PIC S9(0004) COMP.
           05  ECALLF PIC  X(0001).
           05  FILLER REDEFINES ECALLF.
               10  ECALLA PIC  X(0001).
           05  ECALLI PIC  X(0001).
      *    -------------------------------
           05  ELINE1L PIC S9(0004) COMP.
           05  ELINE1F PIC  X(0001).
           05  FILLER REDEFINES ELINE1F.
               10  ELINE1A PIC  X(0001).
           05  ELINE1I PIC  X(0070).
      *    -------------------------------
           05  ELINE2L PIC S9(0004) COMP.
           05  ELINE2F PIC  X(0001).
           05  FILLER REDEFINES ELINE2F.
               10  ELINE2A PIC  X(0001).
           05  ELINE2I PIC  X(0070).
      *    -------------------------------
           05  EEMSG1L PIC S9(0004) COMP.
           05  EEMSG1F PIC  X(0001).
           05  FILLER REDEFINES EEMSG1F.
               10  EEMSG1A PIC  X(0001).
           05  EEMSG1I PIC  X(0079).
      *    -------------------------------
           05  EEMSG2L PIC S9(0004) COMP.
           05  EEMSG2F PIC  X(0001).
           05  FILLER REDEFINES EEMSG2F.
               10  EEMSG2A PIC  X(0001).
           05  EEMSG2I PIC  X(0079).
      *    -------------------------------
           05  EPFKL PIC S9(0004) COMP.
           05  EPFKF PIC  X(0001).
           05  FILLER REDEFINES EPFKF.
               10  EPFKA PIC  X(0001).
           05  EPFKI PIC  9(2).
       01  EL142EO REDEFINES EL142BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ETIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ETYPEO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERECDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ETLRTYPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ESEQO PIC  9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EMANTBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EMANTONO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EMANTATO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EMAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ETYPENO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ECALLTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ECALLO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ELINE1O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ELINE2O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EEMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EEMSG2O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EPFKO PIC  99.
      *    -------------------------------
       01  EL142FI REDEFINES EL142BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FDATEL PIC S9(0004) COMP.
           05  FDATEF PIC  X(0001).
           05  FILLER REDEFINES FDATEF.
               10  FDATEA PIC  X(0001).
           05  FDATEI PIC  X(0008).
      *    -------------------------------
           05  FTIMEL PIC S9(0004) COMP.
           05  FTIMEF PIC  X(0001).
           05  FILLER REDEFINES FTIMEF.
               10  FTIMEA PIC  X(0001).
           05  FTIMEI PIC  X(0005).
      *    -------------------------------
           05  FTYPEL PIC S9(0004) COMP.
           05  FTYPEF PIC  X(0001).
           05  FILLER REDEFINES FTYPEF.
               10  FTYPEA PIC  X(0001).
           05  FTYPEI PIC  X(0010).
      *    -------------------------------
           05  FRECDTEL PIC S9(0004) COMP.
           05  FRECDTEF PIC  X(0001).
           05  FILLER REDEFINES FRECDTEF.
               10  FRECDTEA PIC  X(0001).
           05  FRECDTEI PIC  X(0008).
      *    -------------------------------
           05  FBYL PIC S9(0004) COMP.
           05  FBYF PIC  X(0001).
           05  FILLER REDEFINES FBYF.
               10  FBYA PIC  X(0001).
           05  FBYI PIC  X(0004).
      *    -------------------------------
           05  FTLRTYPL PIC S9(0004) COMP.
           05  FTLRTYPF PIC  X(0001).
           05  FILLER REDEFINES FTLRTYPF.
               10  FTLRTYPA PIC  X(0001).
           05  FTLRTYPI PIC  X(0001).
      *    -------------------------------
           05  FSEQL PIC S9(0004) COMP.
           05  FSEQF PIC  X(0001).
           05  FILLER REDEFINES FSEQF.
               10  FSEQA PIC  X(0001).
           05  FSEQI PIC  X(0004).
      *    -------------------------------
           05  FMANTBYL PIC S9(0004) COMP.
           05  FMANTBYF PIC  X(0001).
           05  FILLER REDEFINES FMANTBYF.
               10  FMANTBYA PIC  X(0001).
           05  FMANTBYI PIC  X(0004).
      *    -------------------------------
           05  FMANTONL PIC S9(0004) COMP.
           05  FMANTONF PIC  X(0001).
           05  FILLER REDEFINES FMANTONF.
               10  FMANTONA PIC  X(0001).
           05  FMANTONI PIC  X(0008).
      *    -------------------------------
           05  FMANTATL PIC S9(0004) COMP.
           05  FMANTATF PIC  X(0001).
           05  FILLER REDEFINES FMANTATF.
               10  FMANTATA PIC  X(0001).
           05  FMANTATI PIC  X(0005).
      *    -------------------------------
           05  FMAINTL PIC S9(0004) COMP.
           05  FMAINTF PIC  X(0001).
           05  FILLER REDEFINES FMAINTF.
               10  FMAINTA PIC  X(0001).
           05  FMAINTI PIC  X(0001).
      *    -------------------------------
           05  FSNOTIFL PIC S9(0004) COMP.
           05  FSNOTIFF PIC  X(0001).
           05  FILLER REDEFINES FSNOTIFF.
               10  FSNOTIFA PIC  X(0001).
           05  FSNOTIFI PIC  X(0008).
      *    -------------------------------
           05  FENOTIFL PIC S9(0004) COMP.
           05  FENOTIFF PIC  X(0001).
           05  FILLER REDEFINES FENOTIFF.
               10  FENOTIFA PIC  X(0001).
           05  FENOTIFI PIC  X(0008).
      *    -------------------------------
           05  FLINE1L PIC S9(0004) COMP.
           05  FLINE1F PIC  X(0001).
           05  FILLER REDEFINES FLINE1F.
               10  FLINE1A PIC  X(0001).
           05  FLINE1I PIC  X(0070).
      *    -------------------------------
           05  FLINE2L PIC S9(0004) COMP.
           05  FLINE2F PIC  X(0001).
           05  FILLER REDEFINES FLINE2F.
               10  FLINE2A PIC  X(0001).
           05  FLINE2I PIC  X(0070).
      *    -------------------------------
           05  FEMSG1L PIC S9(0004) COMP.
           05  FEMSG1F PIC  X(0001).
           05  FILLER REDEFINES FEMSG1F.
               10  FEMSG1A PIC  X(0001).
           05  FEMSG1I PIC  X(0079).
      *    -------------------------------
           05  FEMSG2L PIC S9(0004) COMP.
           05  FEMSG2F PIC  X(0001).
           05  FILLER REDEFINES FEMSG2F.
               10  FEMSG2A PIC  X(0001).
           05  FEMSG2I PIC  X(0079).
      *    -------------------------------
           05  FPFKL PIC S9(0004) COMP.
           05  FPFKF PIC  X(0001).
           05  FILLER REDEFINES FPFKF.
               10  FPFKA PIC  X(0001).
           05  FPFKI PIC  9(2).
       01  EL142FO REDEFINES EL142BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FTYPEO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FRECDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FTLRTYPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FSEQO PIC  9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FMANTBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FMANTONO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FMANTATO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FMAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FSNOTIFO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FENOTIFO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FLINE1O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FLINE2O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FEMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FEMSG2O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FPFKO PIC  99.
      *    -------------------------------
       01  EL142GI REDEFINES EL142BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  GDATEL PIC S9(0004) COMP.
           05  GDATEF PIC  X(0001).
           05  FILLER REDEFINES GDATEF.
               10  GDATEA PIC  X(0001).
           05  GDATEI PIC  X(0008).
      *    -------------------------------
           05  GTIMEL PIC S9(0004) COMP.
           05  GTIMEF PIC  X(0001).
           05  FILLER REDEFINES GTIMEF.
               10  GTIMEA PIC  X(0001).
           05  GTIMEI PIC  X(0005).
      *    -------------------------------
           05  GTYPEL PIC S9(0004) COMP.
           05  GTYPEF PIC  X(0001).
           05  FILLER REDEFINES GTYPEF.
               10  GTYPEA PIC  X(0001).
           05  GTYPEI PIC  X(0010).
      *    -------------------------------
           05  GRECDTEL PIC S9(0004) COMP.
           05  GRECDTEF PIC  X(0001).
           05  FILLER REDEFINES GRECDTEF.
               10  GRECDTEA PIC  X(0001).
           05  GRECDTEI PIC  X(0008).
      *    -------------------------------
           05  GBYL PIC S9(0004) COMP.
           05  GBYF PIC  X(0001).
           05  FILLER REDEFINES GBYF.
               10  GBYA PIC  X(0001).
           05  GBYI PIC  X(0004).
      *    -------------------------------
           05  GTLRTYPL PIC S9(0004) COMP.
           05  GTLRTYPF PIC  X(0001).
           05  FILLER REDEFINES GTLRTYPF.
               10  GTLRTYPA PIC  X(0001).
           05  GTLRTYPI PIC  X(0001).
      *    -------------------------------
           05  GSEQL PIC S9(0004) COMP.
           05  GSEQF PIC  X(0001).
           05  FILLER REDEFINES GSEQF.
               10  GSEQA PIC  X(0001).
           05  GSEQI PIC  X(0004).
      *    -------------------------------
           05  GMANTBYL PIC S9(0004) COMP.
           05  GMANTBYF PIC  X(0001).
           05  FILLER REDEFINES GMANTBYF.
               10  GMANTBYA PIC  X(0001).
           05  GMANTBYI PIC  X(0004).
      *    -------------------------------
           05  GMANTONL PIC S9(0004) COMP.
           05  GMANTONF PIC  X(0001).
           05  FILLER REDEFINES GMANTONF.
               10  GMANTONA PIC  X(0001).
           05  GMANTONI PIC  X(0008).
      *    -------------------------------
           05  GMANTATL PIC S9(0004) COMP.
           05  GMANTATF PIC  X(0001).
           05  FILLER REDEFINES GMANTATF.
               10  GMANTATA PIC  X(0001).
           05  GMANTATI PIC  X(0005).
      *    -------------------------------
           05  GMAINTL PIC S9(0004) COMP.
           05  GMAINTF PIC  X(0001).
           05  FILLER REDEFINES GMAINTF.
               10  GMAINTA PIC  X(0001).
           05  GMAINTI PIC  X(0001).
      *    -------------------------------
           05  GLINE1L PIC S9(0004) COMP.
           05  GLINE1F PIC  X(0001).
           05  FILLER REDEFINES GLINE1F.
               10  GLINE1A PIC  X(0001).
           05  GLINE1I PIC  X(0070).
      *    -------------------------------
           05  GLINE2L PIC S9(0004) COMP.
           05  GLINE2F PIC  X(0001).
           05  FILLER REDEFINES GLINE2F.
               10  GLINE2A PIC  X(0001).
           05  GLINE2I PIC  X(0070).
      *    -------------------------------
           05  GRECONSL PIC S9(0004) COMP.
           05  GRECONSF PIC  X(0001).
           05  FILLER REDEFINES GRECONSF.
               10  GRECONSA PIC  X(0001).
           05  GRECONSI PIC  X(0008).
      *    -------------------------------
           05  GRSNCDL PIC S9(0004) COMP.
           05  GRSNCDF PIC  X(0001).
           05  FILLER REDEFINES GRSNCDF.
               10  GRSNCDA PIC  X(0001).
           05  GRSNCDI PIC  X(0004).
      *    -------------------------------
           05  GPRFDTL PIC S9(0004) COMP.
           05  GPRFDTF PIC  X(0001).
           05  FILLER REDEFINES GPRFDTF.
               10  GPRFDTA PIC  X(0001).
           05  GPRFDTI PIC  X(0008).
      *    -------------------------------
           05  GEMSG1L PIC S9(0004) COMP.
           05  GEMSG1F PIC  X(0001).
           05  FILLER REDEFINES GEMSG1F.
               10  GEMSG1A PIC  X(0001).
           05  GEMSG1I PIC  X(0079).
      *    -------------------------------
           05  GEMSG2L PIC S9(0004) COMP.
           05  GEMSG2F PIC  X(0001).
           05  FILLER REDEFINES GEMSG2F.
               10  GEMSG2A PIC  X(0001).
           05  GEMSG2I PIC  X(0079).
      *    -------------------------------
           05  GPFKL PIC S9(0004) COMP.
           05  GPFKF PIC  X(0001).
           05  FILLER REDEFINES GPFKF.
               10  GPFKA PIC  X(0001).
           05  GPFKI PIC  9(2).
       01  EL142GO REDEFINES EL142BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GTYPEO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRECDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GTLRTYPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GSEQO PIC  9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GMANTBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GMANTONO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GMANTATO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GMAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GLINE1O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GLINE2O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRECONSO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRSNCDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GPRFDTO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GEMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GEMSG2O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GPFKO PIC  99.
      *    -------------------------------
       01  EL142II REDEFINES EL142BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  IDATEL PIC S9(0004) COMP.
           05  IDATEF PIC  X(0001).
           05  FILLER REDEFINES IDATEF.
               10  IDATEA PIC  X(0001).
           05  IDATEI PIC  X(0008).
      *    -------------------------------
           05  ITIMEL PIC S9(0004) COMP.
           05  ITIMEF PIC  X(0001).
           05  FILLER REDEFINES ITIMEF.
               10  ITIMEA PIC  X(0001).
           05  ITIMEI PIC  X(0005).
      *    -------------------------------
           05  ITYPEL PIC S9(0004) COMP.
           05  ITYPEF PIC  X(0001).
           05  FILLER REDEFINES ITYPEF.
               10  ITYPEA PIC  X(0001).
           05  ITYPEI PIC  X(0010).
      *    -------------------------------
           05  IRECDTEL PIC S9(0004) COMP.
           05  IRECDTEF PIC  X(0001).
           05  FILLER REDEFINES IRECDTEF.
               10  IRECDTEA PIC  X(0001).
           05  IRECDTEI PIC  X(0008).
      *    -------------------------------
           05  IBYL PIC S9(0004) COMP.
           05  IBYF PIC  X(0001).
           05  FILLER REDEFINES IBYF.
               10  IBYA PIC  X(0001).
           05  IBYI PIC  X(0004).
      *    -------------------------------
           05  ITLRTYPL PIC S9(0004) COMP.
           05  ITLRTYPF PIC  X(0001).
           05  FILLER REDEFINES ITLRTYPF.
               10  ITLRTYPA PIC  X(0001).
           05  ITLRTYPI PIC  X(0001).
      *    -------------------------------
           05  ISEQL PIC S9(0004) COMP.
           05  ISEQF PIC  X(0001).
           05  FILLER REDEFINES ISEQF.
               10  ISEQA PIC  X(0001).
           05  ISEQI PIC  X(0004).
      *    -------------------------------
           05  IINCDTL PIC S9(0004) COMP.
           05  IINCDTF PIC  X(0001).
           05  FILLER REDEFINES IINCDTF.
               10  IINCDTA PIC  X(0001).
           05  IINCDTI PIC  X(0008).
      *    -------------------------------
           05  ITAPDL PIC S9(0004) COMP.
           05  ITAPDF PIC  X(0001).
           05  FILLER REDEFINES ITAPDF.
               10  ITAPDA PIC  X(0001).
           05  ITAPDI PIC  X(0013).
      *    -------------------------------
           05  IREPDTL PIC S9(0004) COMP.
           05  IREPDTF PIC  X(0001).
           05  FILLER REDEFINES IREPDTF.
               10  IREPDTA PIC  X(0001).
           05  IREPDTI PIC  X(0008).
      *    -------------------------------
           05  ITDPDL PIC S9(0004) COMP.
           05  ITDPDF PIC  X(0001).
           05  FILLER REDEFINES ITDPDF.
               10  ITDPDA PIC  X(0001).
           05  ITDPDI PIC  X(0006).
      *    -------------------------------
           05  IESTDTL PIC S9(0004) COMP.
           05  IESTDTF PIC  X(0001).
           05  FILLER REDEFINES IESTDTF.
               10  IESTDTA PIC  X(0001).
           05  IESTDTI PIC  X(0008).
      *    -------------------------------
           05  INOPMTL PIC S9(0004) COMP.
           05  INOPMTF PIC  X(0001).
           05  FILLER REDEFINES INOPMTF.
               10  INOPMTA PIC  X(0001).
           05  INOPMTI PIC  X(0006).
      *    -------------------------------
           05  ITHRUHDL PIC S9(0004) COMP.
           05  ITHRUHDF PIC  X(0001).
           05  FILLER REDEFINES ITHRUHDF.
               10  ITHRUHDA PIC  X(0001).
           05  ITHRUHDI PIC  X(0014).
      *    -------------------------------
           05  IPDTHRUL PIC S9(0004) COMP.
           05  IPDTHRUF PIC  X(0001).
           05  FILLER REDEFINES IPDTHRUF.
               10  IPDTHRUA PIC  X(0001).
           05  IPDTHRUI PIC  X(0008).
      *    -------------------------------
           05  ITLRCNTL PIC S9(0004) COMP.
           05  ITLRCNTF PIC  X(0001).
           05  FILLER REDEFINES ITLRCNTF.
               10  ITLRCNTA PIC  X(0001).
           05  ITLRCNTI PIC  X(0004).
      *    -------------------------------
           05  ILSTPDTL PIC S9(0004) COMP.
           05  ILSTPDTF PIC  X(0001).
           05  FILLER REDEFINES ILSTPDTF.
               10  ILSTPDTA PIC  X(0001).
           05  ILSTPDTI PIC  X(0008).
      *    -------------------------------
           05  IMANRESL PIC S9(0004) COMP.
           05  IMANRESF PIC  X(0001).
           05  FILLER REDEFINES IMANRESF.
               10  IMANRESA PIC  X(0001).
           05  IMANRESI PIC  X(0013).
      *    -------------------------------
           05  ITEXPDL PIC S9(0004) COMP.
           05  ITEXPDF PIC  X(0001).
           05  FILLER REDEFINES ITEXPDF.
               10  ITEXPDA PIC  X(0001).
           05  ITEXPDI PIC  X(0010).
      *    -------------------------------
           05  ICURRESL PIC S9(0004) COMP.
           05  ICURRESF PIC  X(0001).
           05  FILLER REDEFINES ICURRESF.
               10  ICURRESA PIC  X(0001).
           05  ICURRESI PIC  X(0013).
      *    -------------------------------
           05  ICHGEXPL PIC S9(0004) COMP.
           05  ICHGEXPF PIC  X(0001).
           05  FILLER REDEFINES ICHGEXPF.
               10  ICHGEXPA PIC  X(0001).
           05  ICHGEXPI PIC  X(0010).
      *    -------------------------------
           05  IADDRESL PIC S9(0004) COMP.
           05  IADDRESF PIC  X(0001).
           05  FILLER REDEFINES IADDRESF.
               10  IADDRESA PIC  X(0001).
           05  IADDRESI PIC  X(0013).
      *    -------------------------------
           05  ICAUSCDL PIC S9(0004) COMP.
           05  ICAUSCDF PIC  X(0001).
           05  FILLER REDEFINES ICAUSCDF.
               10  ICAUSCDA PIC  X(0001).
           05  ICAUSCDI PIC  X(0006).
      *    -------------------------------
           05  IDIAGL PIC S9(0004) COMP.
           05  IDIAGF PIC  X(0001).
           05  FILLER REDEFINES IDIAGF.
               10  IDIAGA PIC  X(0001).
           05  IDIAGI PIC  X(0060).
      *    -------------------------------
           05  IEMSG1L PIC S9(0004) COMP.
           05  IEMSG1F PIC  X(0001).
           05  FILLER REDEFINES IEMSG1F.
               10  IEMSG1A PIC  X(0001).
           05  IEMSG1I PIC  X(0079).
      *    -------------------------------
           05  IPFKL PIC S9(0004) COMP.
           05  IPFKF PIC  X(0001).
           05  FILLER REDEFINES IPFKF.
               10  IPFKA PIC  X(0001).
           05  IPFKI PIC  9(2).
       01  EL142IO REDEFINES EL142BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  IDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ITIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ITYPEO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  IRECDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  IBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ITLRTYPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ISEQO PIC  9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  IINCDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ITAPDO PIC  Z,ZZZ,ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  IREPDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ITDPDO PIC  Z,ZZ9-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  IESTDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INOPMTO PIC  Z,ZZ9-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ITHRUHDO PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  IPDTHRUO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ITLRCNTO PIC  9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ILSTPDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  IMANRESO PIC  Z,ZZZ,ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ITEXPDO PIC  ZZ,ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ICURRESO PIC  Z,ZZZ,ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ICHGEXPO PIC  ZZ,ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  IADDRESO PIC  Z,ZZZ,ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ICAUSCDO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  IDIAGO PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  IEMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  IPFKO PIC  99.
      *    -------------------------------
00572  01  FILLER REDEFINES EL142HI.
00573
101807     05  FILLER                      PIC X(259).
00575
00576      05  FILLER  OCCURS 5  INDEXED BY EL142H-INDEX1.
00577          10  EL142H-MAP-LINE  OCCURS 2 INDEXED BY EL142H-INDEX2.
00578
00579              15  EL142H-DATE-LENGTH  PIC S9(4)  COMP.
00580              15  EL142H-DATE-ATTRB   PIC X.
00581              15  EL142H-DATE         PIC X(8).
00582
00583              15  EL142H-OC-LENGTH    PIC S9(4)  COMP.
00584              15  EL142H-OC-ATTRB     PIC X.
00585              15  EL142H-OC           PIC X.
00586
00587              15  EL142H-CAUSE-LENGTH PIC S9(4)  COMP.
00588              15  EL142H-CAUSE-ATTRB  PIC X.
00589              15  EL142H-CAUSE        PIC X(5).
00590
00591      EJECT
00592 *                                COPY ELCEMIB.
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
00593      EJECT
00594 *                                COPY ELCDATE.
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
00595      EJECT
00596 *                                COPY ELCLOGOF.
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
00597      EJECT
00598 *                                COPY ELCATTR.
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
00599      EJECT
00600 *                                COPY ELCAID.
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
00601  01  FILLER REDEFINES DFHAID.
00602      05  FILLER                      PIC X(8).
00603      05  PF-VALUES                   PIC X  OCCURS 24.
00604
00605      EJECT
00606
00607 *    COPY ELCDMO.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCDMO.                             *
00004 *                            VMOD=2.004                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = DLO025 (DMO FILE MAINTENANCE PGRM)        *
00007 *        COMMUNICATION AREA                                      *
00008 *   FILE TYPE = NA                                               *
00009 *   RECORD SIZE = 110    RECFORM = FIXED                         *
00010 *                                                                *
00011 ******************************************************************
00012  01  DMO-COMMUNICATION-AREA.
00013      12  DM-RECORD-TYPE                  PIC  X(02).
00014              88  DM-ISSUE-TRAN                VALUE 'CC'.
00015              88  DM-CLAIM-STATUS-CHANGE       VALUE 'CS'.
00016              88  DM-CLAIM-PAYMENT             VALUE 'DR'.
00017      12  DM-DIST-CODE                    PIC  X(04).
00018      12  DM-MAIL-CODE                    PIC  X(05).
00019      12  DM-CREDIT-CARD-NUMBER           PIC  X(16).
00020      12  DM-INSURED-NAME                 PIC  X(30).
00021      12  DM-CLAIM-NO                     PIC  X(07).
00022      12  DM-CLAIM-TYPE                   PIC  X.
00023
00024      12  DM-STATUS-DATA-AREA.
00025          16  DM-CLAIM-STATUS             PIC  X.
00026              88  DM-OPEN-NO-PAYMENTS              VALUE '1'.
00027              88  DM-OPEN-WITH-PAYMENTS            VALUE '2'.
00028              88  DM-CLOSED                        VALUE '3'.
00029              88  DM-CLOSE-SETTLE-FINAL            VALUE '4'.
00030              88  DM-DEFAULT                       VALUE '9'.
00031          16  DM-STATUS-DATE              PIC  X(08).
00032 ******YYYYMMDD
00033          16  DM-STAT-CHANGE-TYPE         PIC  X.
00034              88  DM-MANUAL-CLOSE                  VALUE 'C'.
00035              88  DM-CLAIM-DENIED                  VALUE 'D'.
00036              88  DM-FINAL-PAYMENT                 VALUE 'F'.
00037              88  DM-INITIAL-PAYMENT               VALUE 'I'.
00038              88  DM-AUTO-CLOSE                    VALUE 'Q'.
00039              88  DM-RE-OPENED                     VALUE 'R'.
00040              88  DM-NEW-CLAIM-SETUP               VALUE 'S'.
00041              88  DM-VOIDED-PAYMENT                VALUE 'V'.
00042              88  DM-CLAIM-DELETED                 VALUE 'X'.
00043          16  DM-STAT-CARRIER             PIC X.
00044
00045      12  DM-DRAFT-DATA-AREA.
00046          16  DM-PAYMENT-TYPE             PIC  X.
00047              88  DM-VALID-CLAIM-TYPES VALUES 'L' 'D' 'U' 'A'.
00048          16  DM-PAYMENT-AMT              PIC  9(05)V9(02).
00049          16  DM-PAYMENT-DATE             PIC  X(08).
00050 ******YYYYMMDD
00051          16  DM-CERT-NO                  PIC  X(11).
00052          16  DM-TRLR-SEQ-NO              PIC  9(04).
00053          16  DM-CARRIER                  PIC  X.
00054
00055      12  DM-RETURN-CODE                  PIC  XX.
00608
00609      EJECT
00610
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
00612
00613  01  DFHCOMMAREA                     PIC X(1024).
00614
00615 *01 DFHBLLDS                         COMP SYNC.
00616 *    05  BLLCBAR                     PIC S9(9).
00617 *    05  ELTRLR-POINTER              PIC S9(9).
00618 *    05  ELMSTR-POINTER              PIC S9(9).
00619 *    05  ERACCT-POINTER              PIC S9(9).
00620 *    05  ELARCH-POINTER              PIC S9(9).
00621 *    05  ELCHKQ-POINTER              PIC S9(9).
00622 *    05  ELCNTL-POINTER              PIC S9(9).
00623 *    05  EMPROD-POINTER              PIC S9(9).
00624
00625      EJECT
00626 *                                COPY ELCTRLR.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCTRLR.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.014                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = ACTIVITY TRAILER FILE                     *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 200    RECFORM = FIXED                         *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ELTRLR             RKP=2,LEN=22          *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
120503******************************************************************
120503*                   C H A N G E   L O G
120503*
120503* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
120503*-----------------------------------------------------------------
120503*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
120503* EFFECTIVE    NUMBER
120503*-----------------------------------------------------------------
120503* 120503    2003080800002  SMVA  INITIAL SECURE PAY CHANGES
022106* 022106    2004040700004  PEMA  ADD LIFE CLAIM INTEREST
050506* 050506    2006030600001  AJRA  ADD DENIAL PROOF DATE
062806* 062806    2006030600001  AJRA  ADD PAYMENT PROOF DATE
080106* 080106    2006052500001  AJRA  ADD N AND R NOTE TYPES
041807* 041807    2006032200004  AJRA  ADD APPROVED BY TO PAYMENT
082807* 082807    2007032100001  PEMA  ADD INT RATE TO PMT TRLR
101807* 101807  IR2007100100007  PEMA  EXPAND SIZE OF CLM RESERVE FLDS
070909* 070909    2009060400001  AJRA  ADD AUTO PAY END LETTER
040110* 040110  CR2009070600002  AJRA  ADD RESEND LETTER ID TO LETTER
071910* 071910  CR2009122800001  PEMA  ADD EOB SWITCHES
102610* 102610    2009122800001  AJRA  ADD STOP DATE TO LETTER
061511* 061511    2011042000002  AJRA  ADD VFY 2ND BENE TO ADDRESS TRAIL
020413* 020413    2012071700001  AJRA  PRINT SURVEY AND PRINT CLM FORM I
021213* 021213    2012092400007  AJRA  CAUSAL STATE SEQUENCE NO
061013* 061013  CR2012113000002  PEMA  SPP CLAIM RELATED CHANGES
102413* 102413  CR2013100800001  AJRA  ADD SPECIAL RELEASE IND
022614* 022614    2013050100003  AJRA  ADD CERT CANCELLED NOTE TYPE - T
040814* 040814    2014030500002  AJRA  ADD ICD CODES
052614* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
013017* 013017  CR2016053100001  PEMA  ACH PROCESSING
062217* 062217  CR2017050300002  TANA  ADD AUTH RCVD
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
102418* 102418  CR2018083000001  TANA  ADD ADD NEW CALL TYPE
00017 ******************************************************************
00018  01  ACTIVITY-TRAILERS.
00019      12  AT-RECORD-ID                    PIC XX.
00020          88  VALID-AT-ID                       VALUE 'AT'.
00021
00022      12  AT-CONTROL-PRIMARY.
00023          16  AT-COMPANY-CD               PIC X.
00024          16  AT-CARRIER                  PIC X.
00025          16  AT-CLAIM-NO                 PIC X(7).
00026          16  AT-CERT-NO.
00027              20  AT-CERT-PRIME           PIC X(10).
00028              20  AT-CERT-SFX             PIC X.
00029          16  AT-SEQUENCE-NO              PIC S9(4)     COMP.
00030              88  AT-1ST-TRL-AVAIL             VALUE +4095.
00031              88  AT-LAST-TRL-AVAIL            VALUE +100.
00032              88  AT-RESV-EXP-HIST-TRL         VALUE +0.
00033              88  AT-INSURED-ADDR-TRL          VALUE +1 THRU +9.
00034              88  AT-BENEFICIARY-ADDR-TRL      VALUE +11 THRU +19.
00035              88  AT-ACCOUNT-ADDR-TRL          VALUE +21 THRU +29.
00036              88  AT-PHYSICIAN-ADDR-TRL        VALUE +31 THRU +39.
00037              88  AT-EMPLOYERS-ADDR-TRL        VALUE +41 THRU +49.
00038              88  AT-OTHER-1-ADDR-TRL          VALUE +51 THRU +59.
00039              88  AT-OTHER-2-ADDR-TRL          VALUE +61 THRU +69.
00040              88  AT-DIAGNOSIS-TRL             VALUE +90.
022106             88  AT-BENEFICIARY-TRL           VALUE +91.
022106             88  AT-SPECIAL-REVIEW-TRL        VALUE +92.
061511             88  AT-VFY-2ND-BENE-NOTE-TRL     VALUE +93.
021213             88  AT-VFY-CAUSAL-STATE          VALUE +94.
                   88  AT-ERROR-MSGS-TRL            VALUE +95.
00041
00042      12  AT-TRAILER-TYPE                 PIC X.
00043          88  RESERVE-EXPENSE-TR               VALUE '1'.
00044          88  PAYMENT-TR                       VALUE '2'.
00045          88  AUTO-PAY-TR                      VALUE '3'.
00046          88  CORRESPONDENCE-TR                VALUE '4'.
00047          88  ADDRESS-TR                       VALUE '5'.
00048          88  GENERAL-INFO-TR                  VALUE '6'.
00049          88  AUTO-PROMPT-TR                   VALUE '7'.
00050          88  DENIAL-TR                        VALUE '8'.
00051          88  INCURRED-CHG-TR                  VALUE '9'.
00052          88  FORM-CONTROL-TR                  VALUE 'A'.
00053
00054      12  AT-RECORDED-DT                  PIC XX.
00055      12  AT-RECORDED-BY                  PIC X(4).
00056      12  AT-LAST-MAINT-HHMMSS            PIC S9(6)     COMP-3.
00057
00058      12  AT-TRAILER-BODY                 PIC X(165).
00059
00060      12  AT-RESERVE-EXPENSE-TR  REDEFINES  AT-TRAILER-BODY.
00061          16  AT-RESERVE-CONTROLS.
00062              20  AT-MANUAL-SW            PIC X.
00063                  88  AT-MANUAL-RESERVES-USED VALUE '1'.
00064              20  AT-FUTURE-SW            PIC X.
00065                  88  AT-FUTURE-RESERVES-USED VALUE '1'.
00066              20  AT-PTC-SW               PIC X.
00067                  88  AT-PAY-TO-CURRENT-USED  VALUE '1'.
00068              20  AT-IBNR-SW              PIC X.
00069                  88  AT-IBNR-RESERVES-USED   VALUE '1'.
00070              20  AT-PTC-LF-SW            PIC X.
00071                  88  AT-LF-PTC-USED          VALUE '1'.
00072              20  AT-CDT-ACCESS-METHOD    PIC X.
00073                  88  AT-CDT-ROUND-NEAR       VALUE '1'.
00074                  88  AT-CDT-ROUND-HIGH       VALUE '2'.
00075                  88  AT-CDT-INTERPOLATED     VALUE '3'.
00076              20  AT-PERCENT-OF-CDT       PIC S9(3)V99    COMP-3.
00077          16  AT-LAST-COMPUTED-DT         PIC XX.
101807         16  AT-FUTURE-RESERVE           PIC S9(7)V99    COMP-3.
101807         16  AT-PAY-CURRENT-RESERVE      PIC S9(7)V99    COMP-3.
101807         16  AT-IBNR-RESERVE             PIC S9(7)V99    COMP-3.
101807         16  AT-INITIAL-MANUAL-RESERVE   PIC S9(7)V99    COMP-3.
101807         16  AT-CURRENT-MANUAL-RESERVE   PIC S9(7)V99    COMP-3.
101807         16  AT-ITD-ADDITIONAL-RESERVE   PIC S9(7)V99    COMP-3.
00084          16  AT-EXPENSE-CONTROLS.
00085              20  AT-EXPENSE-METHOD       PIC X.
00086                  88  NO-EXPENSE-CALCULATED    VALUE '1'.
00087                  88  FLAT-DOLLAR-PER-PMT      VALUE '2'.
00088                  88  PERCENT-OF-PMT           VALUE '3'.
00089                  88  DOLLAR-PER-OPEN-MONTH    VALUE '4'.
00090              20  AT-EXPENSE-PERCENT      PIC S9(3)V99    COMP-3.
00091              20  AT-EXPENSE-DOLLAR       PIC S9(3)V99    COMP-3.
00092          16  AT-ITD-PAID-EXPENSES        PIC S9(5)V99    COMP-3.
00093          16  AT-ITD-CHARGEABLE-EXPENSE   PIC S9(5)V99    COMP-3.
00094
00095          16  AT-ITD-LIFE-REFUNDS         PIC S9(5)V99    COMP-3.
00096          16  AT-ITD-AH-REFUNDS           PIC S9(5)V99    COMP-3.
00097
101807*        16  FILLER                      PIC X(53).
101807         16  FILLER                      PIC X(47).
00099
00100          16  AT-RESERVES-LAST-MAINT-DT   PIC XX.
00101          16  AT-RESERVES-LAST-UPDATED-BY PIC X(4).
00102
00103          16  AT-OPEN-CLOSE-HISTORY OCCURS 6 TIMES.
00104              20  AT-OPEN-CLOSE-DATE      PIC XX.
00105              20  AT-OPEN-CLOSE-TYPE      PIC X.
00106 *                    C = CLOSED
00107 *                    O = OPEN
00108              20  AT-OPEN-CLOSE-REASON    PIC X(5).
00109 *                   REASONS = ALTER, AUTO, FINAL, NEW, FORCE
00110
00111      12  AT-PAYMENT-TR  REDEFINES  AT-TRAILER-BODY.
00112          16  AT-PAYMENT-TYPE             PIC X.
00113              88  PARTIAL-PAYMENT                VALUE '1'.
00114              88  FINAL-PAYMENT                  VALUE '2'.
00115              88  LUMP-SUM-PAYMENT               VALUE '3'.
00116              88  ADDITIONAL-PAYMENT             VALUE '4'.
00117              88  CHARGEABLE-EXPENSE             VALUE '5'.
00118              88  NON-CHARGEABLE-EXPENSE         VALUE '6'.
00119              88  VOIDED-PAYMENT                 VALUE '9'.
00120              88  TRANSFER                       VALUE 'T'.
022106             88  LIFE-INTEREST                  VALUE 'I'.
00121
00122          16  AT-CLAIM-TYPE               PIC X.
00123              88  PAID-FOR-AH                    VALUE 'A'.
00124              88  PAID-FOR-LIFE                  VALUE 'L'.
00124              88  PAID-FOR-IUI                   VALUE 'I'.
120503             88  PAID-FOR-GAP                   VALUE 'G'.
052614             88  PAID-FOR-FAM                   VALUE 'F'.
100518             88  PAID-FOR-OTH                   VALUE 'O'.
00125          16  AT-CLAIM-PREM-TYPE          PIC X.
00126              88  AT-SINGLE-PREMIUM              VALUE '1'.
00127              88  AT-O-B-COVERAGE                VALUE '2'.
00128              88  AT-OPEN-END-COVERAGE           VALUE '3'.
00129          16  AT-AMOUNT-PAID              PIC S9(7)V99  COMP-3.
00130          16  AT-CHECK-NO                 PIC X(7).
00131          16  AT-PAID-FROM-DT             PIC XX.
00132          16  AT-PAID-THRU-DT             PIC XX.
00133          16  AT-DAYS-IN-PERIOD           PIC S9(4)     COMP.
013017         16  AT-ACH-PAYMENT              PIC X.
013017*        16  FILLER                      PIC X.
00135          16  AT-PAYEES-NAME              PIC X(30).
00136          16  AT-PAYMENT-ORIGIN           PIC X.
00137              88  ONLINE-MANUAL-PMT              VALUE '1'.
00138              88  ONLINE-AUTO-PMT                VALUE '2'.
00139              88  OFFLINE-PMT                    VALUE '3'.
00140          16  AT-CHECK-WRITTEN-DT         PIC XX.
00141          16  AT-TO-BE-WRITTEN-DT         PIC XX.
00142          16  AT-VOID-DATA.
00143              20  AT-VOID-DT              PIC XX.
041807*00144       20  AT-VOID-REASON          PIC X(30).
041807             20  AT-VOID-REASON          PIC X(26).
041807         16  AT-PMT-APPROVED-BY          PIC X(04).
00145          16  AT-ADDL-RESERVE             PIC S9(5)V99  COMP-3.
00146          16  AT-EXPENSE-PER-PMT          PIC S9(5)V99  COMP-3.
082807         16  AT-INT-RATE REDEFINES AT-EXPENSE-PER-PMT
082807                                         PIC S99V9(5)  COMP-3.
00147          16  AT-CREDIT-INTERFACE.
00148              20  AT-PMT-SELECT-DT        PIC XX.
00149                  88  PAYMENT-NOT-SELECTED  VALUE LOW-VALUE.
00150              20  AT-PMT-ACCEPT-DT        PIC XX.
00151                  88  PAYMENT-NOT-ACCEPTED  VALUE LOW-VALUE.
00152              20  AT-VOID-SELECT-DT       PIC XX.
00153                  88  VOID-NOT-SELECTED     VALUE LOW-VALUE.
00154              20  AT-VOID-ACCEPT-DT       PIC XX.
00155                  88  VOID-NOT-ACCEPTED     VALUE LOW-VALUE.
00156
00157          16  AT-CHECK-QUE-CONTROL        PIC S9(8)     COMP.
00158                  88  PAYMENT-NOT-QUEUED           VALUE ZERO.
00159                  88  CONVERSION-PAYMENT           VALUE +99999999.
00160          16  AT-CHECK-QUE-SEQUENCE       PIC S9(4)     COMP.
00161
00162          16  AT-FORCE-CONTROL            PIC X.
00163              88  PAYMENT-WAS-FORCED           VALUE '1'.
00164          16  AT-PREV-LAST-PMT-DT         PIC XX.
00165          16  AT-PREV-PAID-THRU-DT        PIC XX.
00166          16  AT-PREV-LAST-PMT-AMT        PIC S9(7)V99  COMP-3.
00167          16  AT-ELIMINATION-DAYS         PIC S999      COMP-3.
00168          16  AT-DAILY-RATE               PIC S9(3)V99  COMP-3.
00169          16  AT-BENEFIT-TYPE             PIC X.
00170
00171          16  AT-EXPENSE-TYPE             PIC X.
00172          16  AT-PAYMENT-APPROVAL-SW      PIC X.
00173
00174          16  AT-PAYEE-TYPE-CD.
00175              20  AT-PAYEE-TYPE           PIC X.
00176                  88  INSURED-PAID           VALUE 'I'.
00177                  88  BENEFICIARY-PAID       VALUE 'B'.
00178                  88  ACCOUNT-PAID           VALUE 'A'.
00179                  88  OTHER-1-PAID           VALUE 'O'.
00180                  88  OTHER-2-PAID           VALUE 'Q'.
00181                  88  DOCTOR-PAID            VALUE 'P'.
00182                  88  EMPLOYER-PAID          VALUE 'E'.
00183              20  AT-PAYEE-SEQ            PIC X.
00184
00185          16  AT-CASH-PAYMENT             PIC X.
00186          16  AT-GROUPED-PAYMENT          PIC X.
00187          16  AT-PAYMENT-NOTE-SEQ-NO      PIC S9(4)       COMP.
00188          16  AT-APPROVAL-LEVEL-REQD      PIC X.
00189          16  AT-APPROVED-LEVEL           PIC X.
00190          16  AT-VOID-TYPE                PIC X.
00191              88  AT-PAYMENT-WAS-STOPPED     VALUE 'S'.
00192              88  AT-PAYMENT-WAS-VOIDED      VALUE 'V'.
00193          16  AT-AIG-UNEMP-IND            PIC X.
00194              88  AT-AIG-UNEMPLOYMENT-PMT    VALUE 'U'.
00195          16  AT-ASSOCIATES               PIC X.
00196              88  AT-AIG-INTERFACE           VALUE 'I' 'N'.
00197              88  AT-AIG-NON-INTERFACE       VALUE 'A' 'M'.
00198
00199          16  AT-FORM-CTL-SEQ-NO          PIC S9(4)       COMP.
00200          16  AT-CV-PMT-CODE              PIC X.
00201              88  FULL-DEATH-PAYMENT         VALUE '1'.
00202              88  HALF-DEATH-PAYMENT         VALUE '2'.
00203              88  FULL-ADD-PAYMENT           VALUE '3'.
00204              88  HALF-ADD-PAYMENT           VALUE '4'.
00205              88  FULL-RIDER-PAYMENT         VALUE '5'.
00206              88  HALF-RIDER-PAYMENT         VALUE '6'.
00207              88  NON-CHG-EXP-PAYMENT        VALUE '7'.
00208              88  ADDL-PAYMENT               VALUE '8'.
00209
00210          16  AT-EOB-CODE1                PIC XXX.
00211          16  AT-EOB-CODE2                PIC XXX.
00212          16  AT-EOB-CODE3                PIC XXX.
020413         16  FILLER REDEFINES AT-EOB-CODE3.
020413             20  AT-PRINT-CLM-FORM       PIC X.
020413             20  AT-PRINT-SURVEY         PIC X.
102413             20  AT-SPECIAL-RELEASE      PIC X.
00213          16  AT-EOB-CODE4                PIC XXX.
               16  FILLER REDEFINES AT-EOB-CODE4.
                   20  AT-INT-PMT-SELECT-DT    PIC XX.
                   20  FILLER                  PIC X.
00214          16  AT-EOB-CODE5                PIC XXX.
062806         16  FILLER REDEFINES AT-EOB-CODE5.
062806             20  AT-PMT-PROOF-DT         PIC XX.
062806             20  FILLER                  PIC X.
00215
071910         16  AT-PRINT-EOB-WITH-CHECK     PIC X.
071910             88  AT-PRINT-EOB            VALUE 'Y'.
00217
00218          16  AT-PAYMENT-LAST-MAINT-DT    PIC XX.
00219          16  AT-PAYMENT-LAST-UPDATED-BY  PIC X(4).
00220
00221      12  AT-AUTO-PAY-TR  REDEFINES  AT-TRAILER-BODY.
00222          16  AT-SCHEDULE-START-DT        PIC XX.
00223          16  AT-SCHEDULE-END-DT          PIC XX.
00224          16  AT-TERMINATED-DT            PIC XX.
00225          16  AT-LAST-PMT-TYPE            PIC X.
00226              88  LAST-PMT-IS-FINAL              VALUE 'F'.
00227              88  LAST-PMT-IS-PARTIAL            VALUE 'P'.
00228          16  AT-FIRST-PMT-DATA.
00229              20  AT-FIRST-PMT-AMT        PIC S9(7)V99  COMP-3.
00230              20  AT-DAYS-IN-1ST-PMT      PIC S9(4)     COMP.
00231              20  AT-1ST-PAY-THRU-DT      PIC XX.
00232          16  AT-REGULAR-PMT-DATA.
00233              20  AT-REGULAR-PMT-AMT      PIC S9(7)V99  COMP-3.
00234              20  AT-DAYS-IN-REG-PMT      PIC S9(4)     COMP.
00235              20  AT-INTERVAL-MONTHS      PIC S9(4)     COMP.
00236          16  AT-AUTO-PAYEE-CD.
00237              20  AT-AUTO-PAYEE-TYPE      PIC X.
00238                  88  INSURED-PAID-AUTO      VALUE 'I'.
00239                  88  BENEFICIARY-PAID-AUTO  VALUE 'B'.
00240                  88  ACCOUNT-PAID-AUTO      VALUE 'A'.
00241                  88  OTHER-1-PAID-AUTO      VALUE 'O'.
00242                  88  OTHER-2-PAID-AUTO      VALUE 'Q'.
00243              20  AT-AUTO-PAYEE-SEQ       PIC X.
00244          16  AT-AUTO-PAY-DAY             PIC 99.
00245          16  AT-AUTO-CASH                PIC X.
00246              88  AT-CASH                      VALUE 'Y'.
00247              88  AT-NON-CASH                  VALUE 'N'.
070909*        16  FILLER                      PIC X(129).
070909         16  AT-AUTO-END-LETTER          PIC X(4).
070909         16  FILLER                      PIC X(125).
00249
00250          16  AT-AUTO-PAY-LAST-MAINT-DT   PIC XX.
00251          16  AT-AUTO-PAY-LAST-UPDATED-BY PIC X(4).
00252
00253      12  AT-CORRESPONDENCE-TR  REDEFINES  AT-TRAILER-BODY.
00254          16  AT-LETTER-SENT-DT           PIC XX.
00255          16  AT-RECEIPT-FOLLOW-UP        PIC XX.
00256          16  AT-AUTO-RE-SEND-DT          PIC XX.
00257          16  AT-LETTER-ANSWERED-DT       PIC XX.
00258          16  AT-LETTER-ARCHIVE-NO        PIC S9(8)     COMP.
00259          16  AT-LETTER-ORIGIN            PIC X.
00260              88  ONLINE-CREATION              VALUE '1' '3'.
00261              88  OFFLINE-CREATION             VALUE '2' '4'.
                   88  NAPER-ONLINE-CREATION        VALUE '3'.
                   88  NAPER-OFFLINE-CREATION       VALUE '4'.
00262          16  AT-STD-LETTER-FORM          PIC X(4).
00263          16  AT-REASON-TEXT              PIC X(70).
00264          16  AT-ADDRESS-REC-SEQ-NO       PIC S9(4)     COMP.
00265          16  AT-ADDRESEE-TYPE            PIC X.
00266               88  INSURED-ADDRESEE            VALUE 'I'.
00267               88  BENEFICIARY-ADDRESEE        VALUE 'B'.
00268               88  ACCOUNT-ADDRESEE            VALUE 'A'.
00269               88  PHYSICIAN-ADDRESEE          VALUE 'P'.
00270               88  EMPLOYER-ADDRESEE           VALUE 'E'.
00271               88  OTHER-ADDRESEE-1            VALUE 'O'.
00272               88  OTHER-ADDRESEE-2            VALUE 'Q'.
00273          16  AT-ADDRESSEE-NAME           PIC X(30).
00274          16  AT-INITIAL-PRINT-DATE       PIC XX.
00275          16  AT-RESEND-PRINT-DATE        PIC XX.
00276          16  AT-CORR-SOL-UNSOL           PIC X.
00277          16  AT-LETTER-PURGED-DT         PIC XX.
CIDMOD*
CIDMOD*FOLLOWING CID CHGS REENTERED AS DMD CHGS OVERLAID THEM.
CIDMOD*
CIDMOD         16  AT-CSO-REDEFINITION.
040110             20  AT-RESEND-LETTER-FORM   PIC X(4).
040110             20  AT-AUTO-CLOSE-IND       PIC X(1).
040110             20  AT-LETTER-TO-BENE       PIC X(1).
102610             20  AT-STOP-LETTER-DT       PIC X(2).
062217             20  AT-AUTH-RCVD            PIC X(1).
062217             20  FILLER                  PIC X(18).
040110*             20  FILLER                  PIC X(27).
CIDMOD             20  AT-CSO-LETTER-STATUS    PIC X.
CIDMOD                 88  AT-CSO-LETTER-ONLINE    VALUE '1'.
CIDMOD                 88  AT-CSO-LETTER-PURGED    VALUE '2'.
CIDMOD                 88  AT-CSO-LETTER-RELOADED  VALUE '3'.
CIDMOD             20  AT-CSO-LETTER-PURGE-DATE   PIC XX.
CIDMOD             20  AT-CSO-LETTER-RELOAD-DATE  PIC XX.
CIDMOD*
CIDMOD*FOLLOWING DMD CHGS COMMENTED OUT AS THEY OVERLAY CID MODS NEEDED
CIDMOD*
CIDMOD*        16  FILLER                      PIC X(26).
CIDMOD*
CIDMOD*        16  AT-DMD-BSR-CODE             PIC X.
CIDMOD*            88  AT-AUTOMATED-BSR              VALUE 'A'.
CIDMOD*            88  AT-NON-AUTOMATED-BSR          VALUE 'B' ' '.
CIDMOD*
CIDMOD*        16  AT-DMD-LETTER-STATUS        PIC X.
CIDMOD*            88  AT-DMD-LETTER-ONLINE          VALUE '1'.
CIDMOD*            88  AT-DMD-LETTER-PURGED          VALUE '2'.
CIDMOD*            88  AT-DMD-LETTER-RELOADED        VALUE '3'.
CIDMOD*        16  AT-DMD-LETTER-PURGE-DT      PIC XX.
CIDMOD*        16  AT-DMD-LETTER-RELOAD-DT     PIC XX.
00290
00291          16  AT-CORR-LAST-MAINT-DT       PIC XX.
00292          16  AT-CORR-LAST-UPDATED-BY     PIC X(4).
00293
00294      12  AT-ADDRESS-TR  REDEFINES  AT-TRAILER-BODY.
00295          16  AT-ADDRESS-TYPE             PIC X.
00296              88  INSURED-ADDRESS               VALUE 'I'.
00297              88  BENEFICIARY-ADDRESS           VALUE 'B'.
00298              88  ACCOUNT-ADDRESS               VALUE 'A'.
00299              88  PHYSICIAN-ADDRESS             VALUE 'P'.
00300              88  EMPLOYER-ADDRESS              VALUE 'E'.
00301              88  OTHER-ADDRESS-1               VALUE 'O'.
00302              88  OTHER-ADDRESS-2               VALUE 'Q'.
00303          16  AT-MAIL-TO-NAME             PIC X(30).
00304          16  AT-ADDRESS-LINE-1           PIC X(30).
00305          16  AT-ADDRESS-LINE-2           PIC X(30).
00306          16  AT-CITY-STATE.
                   20  AT-CITY                 PIC X(28).
                   20  AT-STATE                PIC XX.
00307          16  AT-ZIP.
00308              20  AT-ZIP-CODE.
00309                  24  AT-ZIP-1ST          PIC X.
00310                      88  AT-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00311                  24  FILLER              PIC X(4).
00312              20  AT-ZIP-PLUS4            PIC X(4).
00313          16  AT-CANADIAN-POSTAL-CODE  REDEFINES  AT-ZIP.
00314              20  AT-CAN-POSTAL-1         PIC XXX.
00315              20  AT-CAN-POSTAL-2         PIC XXX.
00316              20  FILLER                  PIC XXX.
00317          16  AT-PHONE-NO                 PIC 9(11)     COMP-3.
061511*         16  FILLER                      PIC X(23).
061511         16  AT-VFY-2ND-BENE-SSN         PIC X(9).
061511         16  AT-VFY-2ND-BENE-VERIFIED    PIC X.
061511         16  FILLER                      PIC X(13).
00319          16  AT-ADDRESS-LAST-MAINT-DT    PIC XX.
00320          16  AT-ADDRESS-LAST-UPDATED-BY  PIC X(4).
00321
00322      12  AT-GENERAL-INFO-TR  REDEFINES  AT-TRAILER-BODY.
00323          16  AT-INFO-LINE-1              PIC X(60).
061013         16  FILLER REDEFINES AT-INFO-LINE-1.
061013             20  AT-NOTE-ERROR-NO OCCURS 15
061013                                         PIC X(4).
00324          16  AT-INFO-LINE-2              PIC X(60).
040814         16  FILLER REDEFINES AT-INFO-LINE-2.
040814             20  AT-ICD-CODE-1           PIC X(8).
040814             20  AT-ICD-CODE-2           PIC X(8).
040814             20  FILLER                  PIC X(44).
00325          16  AT-INFO-TRAILER-TYPE        PIC X.
061013             88  AT-ERRORS-NOTE          VALUE 'E'.
00326              88  AT-PAYMENT-NOTE         VALUE 'P'.
00327              88  AT-CALL-NOTE            VALUE 'C'.
00328              88  AT-MAINT-NOTE           VALUE 'M'.
00329              88  AT-CERT-CHANGE          VALUE 'X'.
080106             88  AT-APPROVAL-NOTE        VALUE 'R'.
080106             88  AT-NOTE-FILE-NOTE       VALUE 'N'.
022614             88  AT-CERT-CANCELLED       VALUE 'T'.
00330          16  AT-CALL-TYPE                PIC X.
00331              88  AT-PHONE-CALL-IN        VALUE 'I'.
102418             88  AT-PHONE-CALL-NEW       VALUE 'N'.
00332              88  AT-PHONE-CALL-OUT       VALUE 'O'.
00333          16  AT-NOTE-CONTINUATION        PIC X.
00334              88  AT-CONTINUED-NOTE       VALUE 'X'.
071910         16  AT-EOB-CODES-EXIST          PIC X.
071910             88  AT-EOB-CODES-PRESENT    VALUE 'Y'.
00335          16  FILLER                      PIC X(35).
00336          16  AT-GEN-INFO-LAST-MAINT-DT   PIC XX.
00337          16  AT-GEN-INFO-LAST-UPDATED-BY PIC X(4).
00338
00339      12  AT-AUTO-PROMPT-TR  REDEFINES  AT-TRAILER-BODY.
00340          16  AT-PROMPT-LINE-1            PIC X(60).
00341          16  AT-PROMPT-LINE-2            PIC X(60).
00342          16  AT-PROMPT-START-DT          PIC XX.
00343          16  AT-PROMPT-END-DT            PIC XX.
00344          16  FILLER                      PIC X(35).
00345          16  AT-PROMPT-LAST-MAINT-DT     PIC XX.
00346          16  AT-PROMPT-LAST-UPDATED-BY   PIC X(4).
00347
00348      12  AT-DENIAL-INFO-TR  REDEFINES  AT-TRAILER-BODY.
00349          16  AT-DENIAL-INFO-1            PIC X(60).
00350          16  AT-DENIAL-INFO-2            PIC X(60).
00351          16  AT-DENIAL-DT                PIC XX.
00352          16  AT-RETRACTION-DT            PIC XX.
00353          16  AT-DENIAL-REASON-CODE       PIC X(4).
050506*         16  FILLER                      PIC X(31).
050506         16  AT-DENIAL-PROOF-DT          PIC XX.
050506         16  FILLER                      PIC X(29).
00355          16  AT-DENIAL-LAST-MAINT-DT     PIC XX.
00356          16  AT-DENIAL-LAST-UPDATED-BY   PIC X(4).
00357
00358      12  AT-INCURRED-CHG-TR  REDEFINES  AT-TRAILER-BODY.
00359          16  AT-OLD-INCURRED-DT          PIC XX.
00360          16  AT-OLD-REPORTED-DT          PIC XX.
00361          16  AT-OLD-ESTABLISHED-DT       PIC XX.
00362          16  AT-OLD-TOTAL-PAID           PIC S9(7)V99     COMP-3.
00363          16  AT-OLD-DAYS-PAID            PIC S9(4)        COMP.
00364          16  AT-OLD-NO-OF-PMTS           PIC S9(3)        COMP-3.
00365          16  AT-OLD-PAID-THRU-DT         PIC XX.
00366          16  AT-LAST-PMT-MADE-DT         PIC XX.
00367          16  FILLER                      PIC X(26).
00368          16  AT-OLD-DIAG-CODE            PIC X(6).
00369          16  AT-TRAILER-CNT-AT-CHG       PIC S9(4)        COMP.
00370          16  AT-OLD-ITD-PAID-EXPENSE     PIC S9(5)V99     COMP-3.
00371          16  AT-OLD-CHARGABLE-EXPENSE    PIC S9(5)V99     COMP-3.
00372          16  AT-OLD-INIT-MAN-RESV        PIC S9(7)V99     COMP-3.
00373          16  AT-OLD-CURRENT-MAN-RESV     PIC S9(7)V99     COMP-3.
00374          16  AT-OLD-ADDL-MAN-RESV        PIC S9(7)V99     COMP-3.
00375          16  AT-OLD-DIAG-DESCRIP         PIC X(60).
040814         16  AT-OLD-ICD-CODE-1           PIC X(8).
040814         16  AT-OLD-ICD-CODE-2           PIC X(8).
040814         16  FILLER                      PIC X(9).
00377          16  AT-INCURRED-LAST-UPDATED-BY PIC X(4).
00378
00379      12  AT-FORM-CONTROL-TR  REDEFINES  AT-TRAILER-BODY.
00380          16  AT-FORM-SEND-ON-DT          PIC XX.
00381          16  AT-FORM-FOLLOW-UP-DT        PIC XX.
00382          16  AT-FORM-RE-SEND-DT          PIC XX.
00383          16  AT-FORM-ANSWERED-DT         PIC XX.
00384          16  AT-FORM-PRINTED-DT          PIC XX.
00385          16  AT-FORM-REPRINT-DT          PIC XX.
00386          16  AT-FORM-TYPE                PIC X.
00387              88  INITIAL-FORM                  VALUE '1'.
00388              88  PROGRESS-FORM                 VALUE '2'.
00389          16  AT-INSTRUCT-LN-1            PIC X(28).
00390          16  AT-INSTRUCT-LN-2            PIC X(28).
00391          16  AT-INSTRUCT-LN-3            PIC X(28).
00392          16  AT-FORM-ADDR-SEQ-NO         PIC S9(4)      COMP.
00393          16  AT-FORM-ADDRESS             PIC X.
00394              88  FORM-TO-INSURED              VALUE 'I'.
00395              88  FORM-TO-ACCOUNT              VALUE 'A'.
00396              88  FORM-TO-OTHER-1              VALUE 'O'.
00397              88  FORM-TO-OTHER-2              VALUE 'Q'.
00398          16  AT-RELATED-1.
00399              20 AT-REL-CARR-1            PIC X.
00400              20 AT-REL-CLAIM-1           PIC X(7).
00401              20 AT-REL-CERT-1            PIC X(11).
00402          16  AT-RELATED-2.
00403              20 AT-REL-CARR-2            PIC X.
00404              20 AT-REL-CLAIM-2           PIC X(7).
00405              20 AT-REL-CERT-2            PIC X(11).
00406          16  AT-EMP-FORM-SEND-ON-DT      PIC XX.
00407          16  AT-PHY-FORM-SEND-ON-DT      PIC XX.
00408          16  AT-EMP-FORM-ANSWERED-DT     PIC XX.
00409          16  AT-PHY-FORM-ANSWERED-DT     PIC XX.
00410          16  AT-FORM-REM-PRINT-DT        PIC XX.
102610         16  AT-STOP-FORM-DT             PIC X(2).
00411
102610         16  FILLER                      PIC X(09).
00413          16  AT-FORM-LAST-MAINT-DT       PIC XX.
00414          16  AT-FORM-LAST-UPDATED-BY     PIC X(4).
00415 ******************************************************************
00627      EJECT
00628 *                                COPY ELCMSTR.
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
00629      EJECT
00630 *                                COPY ERCACCT.
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
00631      EJECT
00632 *                                COPY ELCARCH.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCARCH.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.007                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = LETTERS SENT TO ARCHIVE FILE              *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 090  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELARCH                        RKP=2,LEN=8     *
00013 *       ALTERNATE PATH1 = ELARCH2 (RECORD TYPE)  RKP=10,LEN=8    *
00014 *                                                                *
00015 *   LOG = NO                                                     *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
CIDMOD*                                                                *
CIDMOD*  THERE ARE CID MODS IN COPYBOOK ELCARCH                        *
00017 ******************************************************************
00018  01  LETTER-ARCHIVE.
00019      12  LA-RECORD-ID                PIC XX.
00020          88  VALID-LA-ID                VALUE 'LA'.
00021
00022      12  LA-CONTROL-PRIMARY.
00023          16  LA-COMPANY-CD           PIC X.
00024          16  LA-ARCHIVE-NO           PIC S9(8)     COMP.
00025          16  LA-RECORD-TYPE          PIC X.
00026              88  LA-HEADER-DATA         VALUE '1'.
00027              88  LA-ADDRESS-DATA        VALUE '2'.
00028              88  LA-TEXT-DATA           VALUE '3'.
00029              88  LA-FORM-CONTROL-HDR    VALUE '4'.
00030          16  LA-LINE-SEQ-NO          PIC S9(4)     COMP.
00031
00032      12  LA-CONTROL-BY-TYPE.
00033          16  LA-COMPANY-CD-A1        PIC X.
00034          16  LA-RECORD-TYPE-A1       PIC X.
00035          16  LA-ARCHIVE-NO-A1        PIC S9(8)     COMP.
00036          16  LA-LINE-SEQ-NO-A1       PIC S9(4)     COMP.
00037
00038      12  LA-TEXT-RECORD.
00039          16  LA-SKIP-CONTROL         PIC XX.
00040              88  NO-LINES-SKIPPED       VALUE SPACES.
00041              88  SKIP-TO-NEXT-PAGE      VALUE '99'.
00042          16  LA-TEXT-LINE            PIC X(70).
00043
00044      12  LA-ADDRESS-RECORD  REDEFINES  LA-TEXT-RECORD.
00045          16  FILLER                  PIC XX.
00046          16  LA-ADDRESS-LINE         PIC X(30).
00047          16  FILLER                  PIC X(40).
00048
00049      12  LA-HEADER-RECORD  REDEFINES  LA-TEXT-RECORD.
00050          16  FILLER                  PIC XX.
00051          16  LA-CARRIER              PIC X.
00052          16  LA-CLAIM-NO             PIC X(7).
00053          16  LA-CERT-NO.
00054              20  LA-CERT-PRIME       PIC X(10).
00055              20  LA-CERT-SFX         PIC X.
00056          16  LA-NO-OF-COPIES         PIC S9.
00057          16  LA-RESEND-DATE          PIC XX.
00058          16  LA-PROCESSOR-CD         PIC X(4).
00059          16  LA-CREATION-DT          PIC XX.
00060          16  LA-INITIAL-PRINT-DATE   PIC XX.
00061          16  LA-RESEND-PRINT-DATE    PIC XX.
00062          16  LA-CORR-TRLR-SEQ        PIC S9(4)    COMP.
00063          16  LA-1ST-RESEND-PRINT-DT  PIC XX.
CIDMOD*
00064 * -----  16  LA-DMD-ADDITIONAL-FIELDS.
00065 *   I        20  LA-DMD-LETTER-FORM      PIC X(4).
00066 *   I        20  LA-DMD-PROD-CODE        PIC XX.
00067 *   I        20  LA-DMD-RES-ST           PIC XX.
00068 *   I        20  LA-DMD-CORR-TRLR-SEQ    PIC S9(4)    COMP.
00069 *   I        20  LA-DMD-LETTER-STATUS    PIC X.
00070 *  NEW           88  LA-DMD-LETTER-ONLINE   VALUE '1'.
00071 *  DMD           88  LA-DMD-LETTER-PURGED   VALUE '2'.
00072 *  CHGS          88  LA-DMD-LETTER-RELOADED VALUE '3'.
00073 *   I        20  LA-DMD-LETTER-PURGE-DT  PIC XX.
00074 *   I        20  LA-DMD-LETTER-RELOAD-DT PIC XX.
00075 *   I        20  LA-DMD-UND-CODE         PIC XX.
00076 *   I        20  LA-DMD-BEN-CODE         PIC XX.
00077 *   V    16  FILLER                  PIC X(15).
CIDMOD* -----
CIDMOD*
CIDMOD* REINSERTED  CSO  MODS
CIDMOD*
CIDMOD         16  FILLER.
CIDMOD             20  FILLER                  PIC X(29).
CIDMOD             20  LA-CSO-LETTER-STATUS    PIC X.
CIDMOD                 88  LA-CSO-LETTER-ONLINE   VALUE '1'.
CIDMOD                 88  LA-CSO-LETTER-PURGED   VALUE '2'.
CIDMOD                 88  LA-CSO-LETTER-RELOADED VALUE '3'.
CIDMOD             20  LA-CSO-LETTER-PURGE-DT  PIC XX.
CIDMOD             20  LA-CSO-LETTER-RELOAD-DT PIC XX.
CIDMOD*
00078
00079      12  LA-FORM-CONTROL-HEADER REDEFINES  LA-TEXT-RECORD.
00080          16  FILLER                  PIC XX.
00081          16  LA4-CARRIER             PIC X.
00082          16  LA4-CLAIM-NO            PIC X(7).
00083          16  LA4-CERT-NO.
00084              20  LA4-CERT-PRIME      PIC X(10).
00085              20  LA4-CERT-SFX        PIC X.
00086          16  LA4-NO-OF-COPIES        PIC S9.
00087          16  LA4-RESEND-DATE         PIC XX.
00088          16  LA4-PROCESSOR-CD        PIC X(4).
00089          16  LA4-CREATION-DT         PIC XX.
00090          16  LA4-INITIAL-PRINT-DATE  PIC XX.
00091          16  LA4-RESEND-PRINT-DATE   PIC XX.
00092          16  LA4-FORM-TRLR-SEQ       PIC S9(4)    COMP.
00093          16  LA4-FORM-TYPE           PIC X.
00094              88  LA4-INITIAL-FORM    VALUE '1'.
00095              88  LA4-PROGRESS-FORM   VALUE '2'.
00096          16  LA4-FORM-REM-PRINT-DT   PIC X(02).
00097          16  LA4-STATE               PIC X(02).
00098          16  FILLER                  PIC X(31).
00099 ******************************************************************
00633      EJECT
00634 *                                COPY ELCARCT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCARCT.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = TEMPORARY LETTER ARCHIVE FILE             *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 090  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELARCT                        RKP=2,LEN=8     *
00013 *       ALTERNATE PATH1 = ELARCT2  (RECORD TYPE) RKP=10,LEN=8    *
00014 *                                                                *
00015 *   LOG = NO                                                     *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
CIDMOD*                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCARCT                          *
00017 ******************************************************************
00018  01  LETTER-ARCHIVE-TEMP.
00019      12  LT-RECORD-ID                PIC XX.
00020          88  VALID-LT-ID                VALUE 'LT'.
00021
00022      12  LT-CONTROL-PRIMARY.
00023          16  LT-COMPANY-CD           PIC X.
00024          16  LT-ARCHIVE-NO           PIC S9(8)     COMP.
00025          16  LT-RECORD-TYPE          PIC X.
00026              88  LT-HEADER-DATA         VALUE '1'.
00027              88  LT-ADDRESS-DATA        VALUE '2'.
00028              88  LT-TEXT-DATA           VALUE '3'.
00029              88  LT-FORM-CONTROL-HDR    VALUE '4'.
00030          16  LT-LINE-SEQ-NO          PIC S9(4)     COMP.
00031
00032      12  LT-CONTROL-BY-TYPE.
00033          16  LT-COMPANY-CD-A1        PIC X.
00034          16  LT-RECORD-TYPE-A1       PIC X.
00035          16  LT-ARCHIVE-NO-A1        PIC S9(8)     COMP.
00036          16  LT-LINE-SEQ-NO-A1       PIC S9(4)     COMP.
00037
00038      12  LT-TEXT-RECORD.
00039          16  LT-SKIP-CONTROL         PIC XX.
00040          16  LT-TEXT-LINE            PIC X(70).
00041
00042      12  LT-ADDRESS-RECORD  REDEFINES  LT-TEXT-RECORD.
00043          16  FILLER                  PIC XX.
00044          16  LT-ADDRESS-LINE         PIC X(30).
00045          16  FILLER                  PIC X(40).
00046
00047      12  LT-HEADER-RECORD  REDEFINES  LT-TEXT-RECORD.
00048          16  FILLER                  PIC XX.
00049          16  LT-CARRIER              PIC X.
00050          16  LT-CLAIM-NO             PIC X(7).
00051          16  LT-CERT-NO.
00052              20  LT-CERT-PRIME       PIC X(10).
00053              20  LT-CERT-SFX         PIC X.
00054          16  LT-NO-OF-COPIES         PIC S9.
00055          16  LT-RESEND-DATE          PIC XX.
00056          16  LT-PROCESSOR-CD         PIC X(4).
00057          16  LT-CREATION-DT          PIC XX.
00058          16  LT-INITIAL-PRINT-DATE   PIC XX.
00059          16  LT-RESEND-PRINT-DATE    PIC XX.
00060          16  LT-CORR-TRLR-SEQ        PIC S9(4)    COMP.
00061          16  LT-1ST-RESEND-PRINT-DT  PIC XX.
00062          16  LT-DMD-ADDITIONAL-FIELDS.
00063              20  LT-DMD-LETTER-FORM      PIC X(4).
00064              20  LT-DMD-PROD-CODE        PIC XX.
00065              20  LT-DMD-RES-ST           PIC XX.
00066              20  LT-DMD-CORR-TRLR-SEQ    PIC S9(4)    COMP.
00067              20  LT-DMD-LETTER-STATUS    PIC X.
00068                  88  LT-DMD-LETTER-ONLINE   VALUE '1'.
00069                  88  LT-DMD-LETTER-PURGED   VALUE '2'.
00070                  88  LT-DMD-LETTER-RELOADED VALUE '3'.
00071              20  LT-DMD-LETTER-PURGE-DT  PIC XX.
00072              20  LT-DMD-LETTER-RELOAD-DT PIC XX.
00073              20  LT-DMD-UND-CODE         PIC XX.
00074              20  LT-DMD-BEN-CODE         PIC XX.
00075          16  FILLER                  PIC X(15).
00076
00077      12  LT-FORM-CONTROL-HEADER REDEFINES  LT-TEXT-RECORD.
00078          16  FILLER                  PIC XX.
00079          16  LT4-CARRIER             PIC X.
00080          16  LT4-CLAIM-NO            PIC X(7).
00081          16  LT4-CERT-NO.
00082              20  LT4-CERT-PRIME      PIC X(10).
00083              20  LT4-CERT-SFX        PIC X.
00084          16  LT4-NO-OF-COPIES        PIC S9.
00085          16  LT4-RESEND-DATE         PIC XX.
00086          16  LT4-PROCESSOR-CD        PIC X(4).
00087          16  LT4-CREATION-DT         PIC XX.
00088          16  LT4-INITIAL-PRINT-DATE  PIC XX.
00089          16  LT4-RESEND-PRINT-DATE   PIC XX.
00090          16  LT4-FORM-TRLR-SEQ       PIC S9(4)    COMP.
00091          16  LT4-FORM-TYPE           PIC X.
00092              88  LT4-INITIAL-FORM    VALUE '1'.
00093              88  LT4-PROGRESS-FORM   VALUE '2'.
00094          16  LT4-FORM-REM-PRINT-DT   PIC XX.
00095          16  LT4-STATE               PIC XX.
00096          16  FILLER                  PIC X(31).
00097 ******************************************************************
00635      EJECT
00636 *                                COPY ELCCHKQ.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCCHKQ.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.007                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CHECK QUE FILE                            *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 100  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCHKQ                         RKP=2,LEN=7    *
00013 *       ALTERNATE PATH1 = ELCHKQ2 (BY PAYEE)      RKP=9,LEN=26   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
00018  01  CHECK-QUE.
00019      12  CQ-RECORD-ID                PIC XX.
00020          88  VALID-CQ-ID         VALUE 'CQ'.
00021
00022      12  CQ-CONTROL-PRIMARY.
00023          16  CQ-COMPANY-CD           PIC X.
00024          16  CQ-CONTROL-NUMBER       PIC S9(8)       COMP.
00025          16  CQ-SEQUENCE-NUMBER      PIC S9(4)       COMP.
00026
00027      12  CQ-CONTROL-BY-PAYEE.
DJNA           16  CQ-CONTROL-BY-NUMBER.
DJNA               20  CQ-COMPANY-CD-A1     PIC X.
DJNA               20  CQ-CONTROL-NUMBER-A1 PIC S9(8)      COMP.
00030          16  CQ-PAYEE-CARRIER        PIC X.
00031          16  CQ-PAYEE-GROUPING       PIC X(6).
00032          16  CQ-PAYEE-STATE          PIC XX.
00033          16  CQ-PAYEE-BENE-ACCT      PIC X(10).
00034          16  CQ-SEQUENCE-NUMBER-A1   PIC S9(4)       COMP.
00035
00036      12  CQ-DMD-CONTROL  REDEFINES  CQ-CONTROL-BY-PAYEE.
00037          16  CQ-DMD-COMPANY-CD-A2    PIC X.
00038          16  CQ-DMD-PAYEE-TYPE-A2    PIC X.
00039          16  CQ-DMD-BENE-CODE-A2     PIC X(10).
00040          16  CQ-DMD-CLAIM-NO-A2      PIC X(7).
00041          16  CQ-DMD-TIME-SEQ-A2      PIC S9(7)       COMP.
00042          16  FILLER                  PIC X(3).
00043
00044      12  CQ-ENTRY-TYPE               PIC X.
00045              88  CHECK-ON-QUE           VALUE 'Q'.
00046              88  ALIGNMENT-CHECK        VALUE 'A'.
00047              88  SPOILED-CHECK          VALUE 'S'.
00048              88  PAYMENT-ABORTED        VALUE 'X'.
00049
00050      12  CQ-CLAIM-MAST-CNTL.
00051          16  CQ-CARRIER              PIC X.
00052          16  CQ-CLAIM-NO             PIC X(7).
00053          16  CQ-CERT-NO.
00054              20  CQ-CERT-PRIME       PIC X(10).
00055              20  CQ-CERT-SFX         PIC X.
00056          16  CQ-CLAIM-TYPE           PIC X.
00057              88  CQ-LIFE-CLAIM          VALUE 'L'.
00058              88  CQ-AH-CLAIM            VALUE 'A'.
00059          16  CQ-CLAIM-SUB-TYPE       PIC X.
00060              88  CQ-FIXED-COVERAGE      VALUE '1'.
00061              88  CQ-O-B-COVERAGE        VALUE '2'.
00062              88  CQ-OPEN-END-COVERAGE   VALUE '3'.
00063
00064      12  CQ-PMT-TRLR-SEQUENCE        PIC S9(4)       COMP.
00065      12  CQ-CHECK-NUMBER             PIC X(7).
00066      12  CQ-CHECK-AMOUNT             PIC S9(7)V99    COMP-3.
00067      12  CQ-PAYMENT-TYPE             PIC X.
00068              88  CQ-PARTIAL-PAYMENT        VALUE '1'.
00069              88  CQ-FINAL-PAYMENT          VALUE '2'.
00070              88  CQ-LUMP-SUM-PAYMENT       VALUE '3'.
00071              88  CQ-ADDITIONAL-PAYMENT     VALUE '4'.
00072              88  CQ-CHARGEABLE-EXPENSE     VALUE '5'.
00073              88  CQ-NON-CHARGEABLE-EXPENSE VALUE '6'.
00074              88  CQ-LIFE-PREMIUM-REFUND    VALUE '7'.
00075              88  CQ-AH-PREMIUM-REFUND      VALUE '8'.
00076      12  CQ-VOID-INDICATOR           PIC X.
00077              88  CHECK-IS-STOPPED          VALUE 'S'.
00078              88  CHECK-IS-VOID             VALUE 'V'.
00079      12  CQ-TIMES-PRINTED            PIC S9(4)       COMP.
00080      12  CQ-PRINT-AT-HHMM            PIC S9(4)       COMP.
00081      12  CQ-CHECK-BY-USER            PIC X(4).
00082      12  CQ-PRE-NUMBERING-SW         PIC X.
00083        88  CHECKS-WERE-NOT-PRE-NUMBERED    VALUE SPACE.
00084        88  CHECKS-WERE-PRE-NUMBERED        VALUE '1'.
00085
00086      12  CQ-CHECK-WRITTEN-DT         PIC XX.
00087      12  CQ-LAST-UPDATED-BY          PIC S9(4)       COMP.
00088      12  CQ-LEDGER-FLAG              PIC X(01).
00089      12  CQ-VOID-AFTER-LEDGER        PIC X(01).
00090      12  CQ-LAST-UPDATED-DT          PIC XX.
00091      12  CQ-LAST-UPDATED-HHMMSS      PIC S9(6)       COMP-3.
00092      12  CQ-APPLIED-TO-RCON-DT       PIC XX.
00093
00094      12  FILLER                      PIC X(04).
00095
00096 ******************************************************************
00637      EJECT
00638 *                                COPY ELCCNTL.
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
020810*                                COPY ELCDENY.
      ******************************************************************
      *                                                                *
      *                                                                *
      *                            ELCDENY                             *
      *                            VMOD=2.001                          *
      *                                                                *
      *   CLAIM SYSTEM DENIAL/RECESSION/REFORMATION TABLE              *
      *                                                                *
      *   THIS COPYBOOK IS USED FOR BOTH THE ONLINE AND BATCH          *
      *   VSAM DENIAL TABLE                                            *
      *                                                                *
      *   FILE DESCRIPTION = DENIAL CODE TABLE                         *
      *                                                                *
      *   FILE TYPE = VSAM,KSDS                                        *
      *   RECORD SIZE = 125   RECFORM = FIX                            *
      *                                                                *
      *   BASE CLUSTER NAME = ELCDENY                   RKP=2,LEN=15   *
      *       ALTERNATE PATH1 = ELDENY2 (ALT GROUPING) RKP=17,LEN=16   *
      *                                                                *
      *   LOG = NO                                                     *
      *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
      *                                                                *
      *                                                                *
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 120808    2008100900001  PEMA  NEW COPYBOOK/FILE
      ******************************************************************
       01  DENIAL-CODES.
           12  DN-RECORD-ID                      PIC XX.
               88  VALID-DN-ID                      VALUE 'DN'.
           12  DN-CONTROL-PRIMARY.
               16  DN-COMPANY-CD                 PIC X.
               16  DN-DENIAL-CODE                PIC X(4).
               16  FILLER                        PIC X(10).
           12  DN-CONTROL-BY-TYPE.
               16  DN-COMPANY-CD-A1              PIC X.
               16  DN-RECORD-TYPE                PIC X.
               16  DN-DENIAL-CODE-A1             PIC X(4).
               16  FILLER                        PIC X(10).
           12  DN-MAINT-INFORMATION.
               16  DN-LAST-MAINT-DT              PIC XX.
               16  DN-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
               16  DN-LAST-MAINT-USER            PIC X(4).
               16  FILLER                        PIC XX.
           12  DN-DESCRIPTION                    PIC X(50).
           12  FILLER                            PIC X(30).
      ******************************************************************
102510*                                COPY ELCNAPS.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCNAPS                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *        FILE DESCRIPTION = NAPERSOFT LETTER FILE                *
00008 *                                                                *
00009 *        FILE TYPE= VSAM,KSDS                                    *
00010 *        RECORD SIZE = 150    RECFORM = FIXED                    *
00011 *                                                                *
00012 *        BASE CLUSTER = ELNAPS        RKP=2,LEN=28               *
00013 *                                                                *
00014 *        LOG = YES                                               *
00015 *        SERVREQ = DELETE,UPDATE,NEWREC                          *
00016 *                                                                *
033110******************************************************************
033110*                   C H A N G E   L O G
033110*
033110* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
033110*-----------------------------------------------------------------
033110*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
033110* EFFECTIVE    NUMBER
033110*-----------------------------------------------------------------
033110* 033110  CR2009122800001  AJRA  NEW FILE FOR NAPERSOFT.
00017 ******************************************************************
00018
00019  01  NAPERSOFT-FILE.
00020      12  NA-RECORD-ID                PIC  XX.
00021          88  VALID-NA-ID                  VALUE 'NA'.
00022
00023      12  NA-CONTROL-PRIMARY.
00024          16  NA-COMPANY-CD           PIC X.
00025          16  NA-CARRIER              PIC X.
00026          16  NA-CLAIM-NO             PIC X(7).
00027          16  NA-CERT-NO.
00028              20  NA-CERT-PRIME       PIC X(10).
00029              20  NA-CERT-SFX         PIC X.
00030          16  NA-ARCHIVE-NO           PIC 9(8).
00031
00032      12  NA-LETTER-INFORMATION.
00033          16  NA-LETTER-ID            PIC X(4).
00034          16  NA-PROCESSOR-ID         PIC X(4).
00035          16  NA-CREATION-DT          PIC X(2).
00040          16  NA-INITIAL-PRINT-DT     PIC X(2).
00041          16  NA-FOLLOW-UP-DT         PIC X(2).
00042          16  NA-RESEND-DT            PIC X(2).
00043          16  NA-RESEND-LETTER-ID     PIC X(4).
00044          16  NA-NO-OF-COPIES         PIC 9(2).
00045          16  NA-ADDRESS-TYPE         PIC X(2).
               16  NA-CORR-TRLR-SEQ        PIC 9(4).
               16  NA-RESEND-PRINT-DT      PIC X(2).
               16  NA-1ST-LTR-PRINT-DT     PIC X(2).
               16  NA-NEXT-DUE-DT          PIC X(2).
               16  NA-AUTOPYDT             PIC X(2).
               16  NA-ENCLOSURE-CD         PIC X(3).
               16  NA-CREATED-IN-NAPERSOFT PIC X(1).
               16  NA-ORIG-ARCHIVE-NO      PIC 9(9).
               16  NA-RESEND-PROMPT-IND    PIC X(1).
               16  NA-ORIG-ENCLOSURE-CD    PIC X(3).
00047          16  FILLER                  PIC X(67).
00048 ******************************************************************
102510
041613*                                COPY ELCENCC.
      ******************************************************************
      *                                                                *
      *                                                                *
      *                            ELCENCC                             *
      *                            VMOD=2.001                          *
      *                                                                *
      *   CLAIM SYSTEM ENCLOSURE CODE TABLE                            *
      *                                                                *
      *   THIS COPYBOOK IS USED FOR THE ONLINE PROCESS OF CREATING     *
      *   A NAPERSOFT DOCUMENT                                         *
      *                                                                *
      *   FILE DESCRIPTION = ENCLOSURE CODE TABLE                      *
      *                                                                *
      *   FILE TYPE = VSAM,KSDS                                        *
      *   RECORD SIZE = 400   RECFORM = FIX                            *
      *                                                                *
      *   BASE CLUSTER NAME = ELENCC                    RKP=2,LEN=16   *
      *                                                                *
      *   LOG = NO                                                     *
      *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
      *                                                                *
      *                                                                *
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 082010    2008100900001  PEMA  NEW COPYBOOK/FILE
      * 061217    2017060900001  TANA  INCREASE ATTACHMENTS FIELD SIZE
      ******************************************************************
       01  ENCLOSURE-CODES.
           12  NC-RECORD-ID                      PIC XX.
               88  VALID-NC-ID                      VALUE 'NC'.
           12  NC-CONTROL-PRIMARY.
               16  NC-COMPANY-CD                 PIC X.
               16  NC-REC-TYPE                   PIC X.
                   88  NC-CLAIMS                   VALUE '1'.
                   88  NC-ADMIN                    VALUE '2'.
               16  NC-ENC-CODE                   PIC X(5).
               16  FILLER                        PIC X(09).
           12  NC-MAINT-INFORMATION.
               16  NC-LAST-MAINT-DT              PIC XX.
               16  NC-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
               16  NC-LAST-MAINT-USER            PIC X(4).
               16  FILLER                        PIC XX.
           12  NC-OUTPUT-STACK                   PIC XXX.
           12  NC-ENCLOSURE-LINE                 PIC X(100).
           12  NC-ATTACHMENTS                    PIC X(255).
           12  NC-FUTURE                         PIC X(12).
      ******************************************************************
041613
00640 *                                COPY MPCPROD.
00001 ******************************************************************
00002 *                                                                *
00003 *                            MPCPROD                             *
00004 *                            VMOD=1.010                          *
00005 *                                                                *
00006 *   MORTGAGE SYSTEM PRODUCER MASTER FILE                         *
00007 *                                                                *
00008 *   THIS COPYBOOK IS USED FOR THE ONLINE                         *
00009 *   VSAM PRODUCER MASTER FILE.                                   *
00010 *                                                                *
00011 *   FILE DESCRIPTION = PRODUCER MASTER FILE                      *
00012 *                                                                *
00013 *   FILE TYPE = VSAM,KSDS                                        *
00014 *   RECORD SIZE = 2000 RECFORM = FIXED                           *
00015 *                                                                *
00016 *   BASE CLUSTER NAME = MPPROD                    RKP=02,LEN=22  *
00017 *       ALTERNATE PATH1 = MPPROD2 (ALT GROUPING)  RKP=48,LEN=22  *
00018 *       ALTERNATE PATH2 = MPPROD3 (PRODUCER NAME) RKP=90,LEN=56  *
00019 *                                                                *
00020 *   LOG = NO                                                     *
00021 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00022 *                                                                *
00023 *                                                                *
00024 ******************************************************************
00025
00026  01  PRODUCER-MASTER.
00027      12  PD-RECORD-ID                 PIC  X(02).
00028          88  PD-VALID-ID                   VALUE 'PD'.
00029
00030 ******************************************************************
00031 *   BASE CLUSTER NAME = MPPROD                    RKP=2,LEN=22   *
00032 ******************************************************************
00033
00034      12  PD-CONTROL-PRIMARY-BATCH.
00035          16  FILLER                   PIC  X(20).
00036          16  PD-EXPIRE-DT.
00037              20  PD-EXPIRE-DT-YY      PIC  9(02).
00038              20  PD-EXPIRE-DT-MM      PIC  9(02).
00039              20  PD-EXPIRE-DT-DD      PIC  9(02).
00040      12  FILLER REDEFINES PD-CONTROL-PRIMARY-BATCH.
00041          16  PD-CONTROL-PRIMARY.
00042              20  PD-COMPANY-CD        PIC  X(01).
00043              20  PD-MSTR-CNTRL.
00044                  24  PD-CONTROL-A.
00045                      28  PD-CARRIER   PIC  X(01).
00046                      28  PD-GROUPING.
00047                          32 PD-GROUPING-PREFIX
00048                                       PIC  X(03).
00049                          32 PD-GROUPING-PRIME
00050                                       PIC  X(03).
00051                      28  PD-STATE     PIC  X(02).
00052                      28  PD-PRODUCER.
00053                          32  PD-PRODUCER-PREFIX
00054                                       PIC  X(04).
00055                          32  PD-PRODUCER-PRIME
00056                                       PIC  X(06).
00057                  24  PD-CNTRL-B.
00058                      28  PD-EXPIRE-DATE
00059                                       PIC  X(02).
00060          16  FILLER REDEFINES PD-CONTROL-PRIMARY.
00061              20  FILLER               PIC  X(01).
00062              20  PD-CGSPE-KEY         PIC  X(21).
00063          16  FILLER                   PIC  X(04).
00064      12  FILLER                       PIC  X(20).
00065
00066 ******************************************************************
00067 *      ALTERNATE PATH1 = MPPROD2 (ALT GROUPING) RKP=48,LEN=22    *
00068 ******************************************************************
00069
00070      12  PD-CONTROL-BY-VAR-GRP.
00071          16  PD-VG-CCGSP-KEYLET.
00072              20  PD-COMPANY-CD-A1     PIC  X(01).
00073              20  PD-VG-CARRIER        PIC  X(01).
00074              20  PD-VG-GROUPING       PIC  X(06).
00075              20  PD-VG-STATE          PIC  X(02).
00076              20  PD-VG-PRODUCER       PIC  X(10).
00077          16  PD-VG-DATE.
00078              24  PD-VG-EXPIRE-DATE    PIC  X(02).
00079      12  FILLER                       PIC  X(20).
00080
00081
00082 ******************************************************************
00083 *      ALTERNATE PATH2 = MPPROD3 (NAME)         RKP=90,LEN=56    *
00084 ******************************************************************
00085
00086      12  PD-CONTROL-BY-NAME.
00087          16  PD-COMPANY-CD-A2         PIC  X(01).
00088          16  PD-NAME-A2               PIC  X(30).
00089          16  PD-CGSPE-KEY-A2.
00090              20  PD-CARRIER-A2        PIC  X(01).
00091              20  PD-GROUPING-A2       PIC  X(06).
00092              20  PD-STATE-A2          PIC  X(02).
00093              20  PD-PRODUCER-A2       PIC  X(10).
00094              20  PD-EXPIRE-DATE-A2    PIC  X(02).
00095          16  PD-CURRENT-DATE-BIN-A2   PIC  X(02).
00096          16  PD-CURRENT-TIME-BIN-A2   PIC S9(04) COMP.
00097      12  FILLER                       PIC  X(20).
00098
00099 ******************************************************************
00100 *                FILE SYNCHRONIZATION DATA                       *
00101 ******************************************************************
00102
00103      12  PD-MAINT-INFORMATION.
00104          16  PD-LAST-MAINT-DATE       PIC  X(02).
00105          16  PD-LAST-MAINT-HHMMSS     PIC S9(07) COMP-3.
00106          16  PD-LAST-MAINT-USER       PIC  X(04).
00107
00108 ******************************************************************
00109 *                PRODUCER SECURITY DATA                          *
00110 ******************************************************************
00111
00112      12  PD-SECURITY-ACCESS-CODE      PIC  X(01).
00113
00114 ******************************************************************
00115 *                DATES                                           *
00116 ******************************************************************
00117
00118      12  PD-ANNIVERSARY-DATE          PIC  X(02).
00119
00120      12  PD-AR-HI-DATE.
00121          16  PD-AR-HI-POLICY-DATE     PIC  X(02).
00122          16  FILLER                   PIC  X(04).
00123      12  PD-AR-HI-POLICY-DT REDEFINES PD-AR-HI-DATE.
00124          16  PD-AR-HI-POLICY-DT-YY    PIC  9(02).
00125          16  PD-AR-HI-POLICY-DT-MM    PIC  9(02).
00126          16  PD-AR-HI-POLICY-DT-DD    PIC  9(02).
00127
00128      12  PD-ENTRY-DATE                PIC  X(02).
00129
00130      12  PD-EFFECT-DTE.
00131          16  PD-EFFECT-DATE           PIC  X(02).
00132          16  FILLER                   PIC  X(04).
00133      12  PD-EFFECT-DT REDEFINES PD-EFFECT-DTE.
00134          16  PD-EFFECT-DT-YY          PIC  9(02).
00135          16  PD-EFFECT-DT-MM          PIC  9(02).
00136          16  PD-EFFECT-DT-DD          PIC  9(02).
00137
00138      12  PD-HI-DATE.
00139          16  PD-HI-POLICY-DATE        PIC  X(02).
00140          16  FILLER                   PIC  X(04).
00141      12  PD-HI-POLICY-DT REDEFINES PD-HI-DATE.
00142          16  PD-HI-POLICY-DT-YY       PIC  9(02).
00143          16  PD-HI-POLICY-DT-MM       PIC  9(02).
00144          16  PD-HI-POLICY-DT-DD       PIC  9(02).
00145
00146      12  PD-INACTIVE-DATE             PIC  X(02).
00147
00148      12  PD-LO-DATE.
00149          16  PD-LO-POLICY-DATE        PIC  X(02).
00150          16  FILLER                   PIC  X(04).
00151      12  PD-LO-POLICY-DT REDEFINES PD-LO-DATE.
00152          16  PD-LO-POLICY-DT-YY       PIC  9(02).
00153          16  PD-LO-POLICY-DT-MM       PIC  9(02).
00154          16  PD-LO-POLICY-DT-DD       PIC  9(02).
00155
00156      12  PD-POLICIES-PURGED-DATE      PIC  X(02).
00157
00158      12  PD-PREV-DATES.
00159          16  PD-PREV-EFF-DATE         PIC  X(02).
00160          16  FILLER                   PIC  X(04).
00161          16  PD-PREV-EXP-DATE         PIC  X(02).
00162          16  FILLER                   PIC  X(04).
00163      12  PD-PREV-DTS REDEFINES PD-PREV-DATES.
00164          16  PD-PREV-EFF-DT.
00165              20  PD-PREV-EFF-DT-YY    PIC  9(02).
00166              20  PD-PREV-EFF-DT-MM    PIC  9(02).
00167              20  PD-PREV-EFF-DT-DD    PIC  9(02).
00168          16  PD-PREV-EXP-DT.
00169              20  PD-PREV-EXP-DT-YY    PIC  9(02).
00170              20  PD-PREV-EXP-DT-MM    PIC  9(02).
00171              20  PD-PREV-EXP-DT-DD    PIC  9(02).
00172
00173      12  PD-1ST-PROD-DATE             PIC  X(02).
00174
00175      12  FILLER                       PIC  X(20).
00176
00177 ******************************************************************
00178 *                MORTGAGE BILLING DATA                           *
00179 ******************************************************************
00180
00181      12  PD-CONTACT                   PIC  X(30).
00182      12  PD-BILLING-MONTHS.
00183          16  PD-BILLING-MONTH-ANNUAL  PIC  9(02).
00184          16  PD-BILLING-MONTH-SEMIANN PIC  9(02).
00185      12  PD-BILLING-ADVANCE-ARREARS   PIC  X(01).
00186          88  PD-BILL-ADVANCE              VALUE '1'.
00187          88  PD-BILL-ARREARS              VALUE '2'.
00188      12  PD-BILLING-MODE              PIC  X(01).
00189          88  PD-ANNUAL-BILL               VALUE '1'.
00190          88  PD-SEMI-ANNUAL-BILL          VALUE '2'.
00191          88  PD-QUARTERLY-BILL            VALUE '3'.
00192          88  PD-MONTHLY-BILL              VALUE '4'.
00193          88  PD-BI-MONTHLY-BILL           VALUE '5'.
00194          88  PD-SINGLE-PREM-BILL          VALUE '6'.
00195      12  PD-BILLING-GROUPING-CODE     PIC  X(06).
00196      12  PD-BILLING-SCHEDULE          PIC  X(01).
00197          88  PD-BILL-1ST-WEEK             VALUE '1'.
00198          88  PD-BILL-2ND-WEEK             VALUE '2'.
00199          88  PD-BILL-3RD-WEEK             VALUE '3'.
00200          88  PD-BILL-4TH-WEEK             VALUE '4'.
00201          88  PD-BILL-5TH-WEEK             VALUE '5'.
00202          88  PD-HOLD-BILL                 VALUE '6'.
00203          88  PD-NO-BILL                   VALUE '7'.
00204      12  PD-BILLING-SEQUENCE          PIC  X(01).
00205          88  PD-BILL-NAME-SEQU            VALUE '1'.
00206          88  PD-BILL-LOAN-SEQU            VALUE '2'.
00207          88  PD-BILL-PLCY-SEQU            VALUE '3'.
00208      12  PD-BILLING-TYPE              PIC  X(01).
00209          88  PD-LIST-BILL                 VALUE '1'.
00210          88  PD-TAPE-BILL                 VALUE '2'.
00211          88  PD-TAPE-LIST-BILL            VALUE '3'.
00212          88  PD-GROUP-BILL            VALUES ARE '1' '2' '3'.
00213          88  PD-DIRECT-BILL               VALUE '4'.
00214          88  PD-PAC                   VALUES ARE '5' 'C' 'S'.
00215          88  PD-CREDIT-CARD               VALUE '6'.
00216          88  PD-INDIV-BILL
00217                               VALUES ARE '4' '5' '6' 'C' 'S'.
00218          88  PD-GROUP-BY-POLICY           VALUE '7'.
00219          88  PD-GROUP-BY-POLICY-PAC       VALUE '8'.
00220          88  PD-GROUP-BY-POLICY-CRDC      VALUE '9'.
00221          88  PD-GROUP-BY-BILL             VALUE '7' '8' '9'.
00222          88  PD-GROUP-BY-PROD             VALUE 'A'.
00223          88  PD-EFT-CHECKING              VALUE 'C'.
00224          88  PD-EFT-SAVINGS               VALUE 'S'.
00225      12  PD-DATE-PAID                 PIC  X(02).
00226      12  PD-LAST-BILLING-DATE         PIC  X(02).
00227      12  PD-LAST-BILL-TO-DATE         PIC  X(02).
00228      12  PD-MAX-MONTHS-BILL           PIC S9(03)  COMP-3.
00229      12  PD-PAID-TO-DATE              PIC  X(02).
00230      12  PD-PREV-BILLING-DATE         PIC  X(02).
00231      12  PD-PREV-BILL-TO-DATE         PIC  X(02).
00232
00233      12  FILLER                       PIC  X(20).
00234
00235 ******************************************************************
00236 *                PERSONAL DATA                                   *
00237 ******************************************************************
00238
00239      12  PD-ADDRS                     PIC  X(30).
00240      12  PD-CITY                      PIC  X(30).
00241      12  PD-CITY-CODE                 PIC  X(04).
00242      12  PD-COUNTY-CODE               PIC  X(03).
00243      12  PD-NAME                      PIC  X(30).
00244      12  PD-PARRISH-CODE              PIC  X(03).
00245      12  PD-PERSON                    PIC  X(30).
00246      12  PD-TEL-NO.
00247          16  PD-AREA-CODE             PIC  9(03).
00248          16  PD-TEL-PRE               PIC  9(03).
00249          16  PD-TEL-NBR               PIC  9(04).
00250      12  PD-ZIP.
00251          16  PD-ZIP-PRIME             PIC  X(05).
00252          16  PD-ZIP-PLUS4             PIC  X(04).
00253      12  PD-LANGUAGE-IND              PIC  X(01).
00254          88  PD-ENGLISH                          VALUE 'E'.
00255          88  PD-FRENCH                           VALUE 'F'.
00256          88  PD-SPANISH                          VALUE 'S'.
00257
00258      12  FILLER                       PIC  X(19).
00259
00260 ******************************************************************
00261 *                REINSURANCE DATA                                *
00262 ******************************************************************
00263
00264      12  PD-REINS-TBL-CODE            PIC  X(03).
00265      12  PD-REIN-RECALC               PIC  X(01).
00266
00267      12  PD-REI-AH-FEE                PIC S9(01)V9(04) COMP-3.
00268      12  PD-REI-AH-PE                 PIC  X(01).
00269      12  PD-REI-AH-TAX                PIC S9(01)V9(04) COMP-3.
00270
00271      12  PD-REI-GROUP-A               PIC  X(06).
00272      12  PD-REI-GROUP-B               PIC  X(06).
00273
00274      12  PD-REI-LF-FEE                PIC S9(01)V9(04) COMP-3.
00275      12  PD-REI-LF-PE                 PIC  X(01).
00276      12  PD-REI-LF-TAX                PIC S9(01)V9(04) COMP-3.
00277
00278      12  PD-REI-MORT                  PIC  X(04).
00279      12  PD-REI-PRT-OW                PIC  X(01).
00280      12  PD-REI-PRT-ST                PIC  X(01).
00281
00282      12  PD-REI-ADD-FEE               PIC S9(01)V9(04) COMP-3.
00283      12  PD-REI-ADD-PE                PIC  X(01).
00284      12  PD-REI-ADD-TAX               PIC S9(01)V9(04) COMP-3.
00285
00286      12  PD-REI-DIS-FEE               PIC S9(01)V9(04) COMP-3.
00287      12  PD-REI-DIS-PE                PIC  X(01).
00288      12  PD-REI-DIS-TAX               PIC S9(01)V9(04) COMP-3.
00289
00290      12  FILLER                       PIC  X(10).
00291 ******************************************************************
00292 *                RETRO DATA                                      *
00293 ******************************************************************
00294
00295      12  PD-RET-AH                    PIC S9(01)V9(04) COMP-3.
00296      12  PD-RET-GRP                   PIC  X(06).
00297      12  PD-RET-LF                    PIC S9(01)V9(04) COMP-3.
00298      12  PD-RET-MIN-LOSS-A            PIC SV9(03)      COMP-3.
00299      12  PD-RET-MIN-LOSS-L            PIC SV9(03)      COMP-3.
00300      12  PD-RET-P-E                   PIC  X(01).
00301      12  PD-RET-ST-TAX-USE            PIC  X(01).
00302          88  PD-CHARGE-ST-TAXES-ON-RETRO      VALUE 'Y' 'E' 'P'.
00303          88  PD-TAXES-NOT-IN-RETRO            VALUE 'N' ' '.
00304      12  PD-RET-Y-N                   PIC  X(01).
00305      12  PD-RET-ADD                   PIC S9(01)V9(04) COMP-3.
00306      12  PD-RET-MIN-LOSS-ADD          PIC SV9(03)      COMP-3.
00307      12  PD-RET-DIS                   PIC S9(01)V9(04) COMP-3.
00308      12  PD-RET-MIN-LOSS-DIS          PIC SV9(03)      COMP-3.
00309
00310      12  FILLER                       PIC  X(10).
00311
00312 ******************************************************************
00313 *                     MANAGEMENT OPTIONS                         *
00314 ******************************************************************
00315
00316      12  PD-DEFAULT-UNWTR-CODE        PIC  X(03).
00317      12  PD-LAPSE-NOTICE-CNTL         PIC  X(01).
00318      12  PD-CORRESPONDENCE-CNTL       PIC  X(01).
00319      12  PD-RETAIN-BILLING-DATA-MTHS  PIC S9(03)  COMP-3.
00320      12  PD-RETAIN-CLAIM-DATA-MTHS    PIC S9(03)  COMP-3.
00321      12  PD-RETAIN-COMMISSION-MTHS    PIC S9(03)  COMP-3.
00322      12  PD-RETAIN-DELINQUENCY-MTHS   PIC S9(03)  COMP-3.
00323      12  PD-RETAIN-INSD-PROFILE-MTHS  PIC S9(03)  COMP-3.
00324      12  PD-RETAIN-INS-COVERAGE-MTHS  PIC S9(03)  COMP-3.
00325      12  PD-RETAIN-STATUS-DISP-MTHS   PIC S9(03)  COMP-3.
00326      12  PD-NUM-BILLING-CYCLES-RETAINED
00327                                       PIC S9(03)  COMP-3.
00328      12  PD-RETAIN-UNDERWRITER-HST-MTHS
00329                                       PIC S9(03)  COMP-3.
00330
00331      12  FILLER                       PIC X(098).
00332
00333
00334 ******************************************************************
00335 *                MISCELLANEOUS DATA                              *
00336 ******************************************************************
00337
00338      12  PD-AH-RPT021-EXP-PCT         PIC S9(03)V9(04) COMP-3.
00339      12  PD-AUTO-REFUND-SW            PIC  X(01).
00340          88  PD-AUTO-REFUNDS-USED             VALUE 'Y'.
00341          88  PD-AUTO-REFUNDS-NOT-USED         VALUE 'N' ' '.
00342      12  PD-BUSINESS-TYPE             PIC  9(02).
00343      12  PD-CAL-TABLE                 PIC  X(02).
00344      12  PD-COMMENTS.
00345          16  PD-COMMENT-LINE          PIC  X(50)
00346                                            OCCURS 5 TIMES.
00347      12  PD-EMPLOYER-STMT-USED        PIC  X(01).
00348      12  PD-GROUPED-CHECKS-Y-N        PIC  X(01).
00349      12  PD-IG                        PIC  X(01).
00350          88  PD-HAS-INDIVIDUAL                VALUE 'I'
00351                                                     '1'.
00352          88  PD-HAS-GROUP                     VALUE 'G'
00353                                                     '2'.
00354      12  PD-LF-RPT021-EXP-PCT         PIC S9(03)V9(04) COMP-3.
00355      12  PD-REPORT-CODE-1             PIC  X(10).
00356      12  PD-REPORT-CODE-2             PIC  X(10).
00357      12  PD-RPT045A-SWITCH            PIC  X(01).
00358          88  PD-RPT045A-OFF                VALUE 'N'.
00359      12  PD-SPECIAL-BILLING-FREQ      PIC  X(01).
00360          88  PD-HAS-SPECIAL-BILL-FREQ         VALUE 'Y'.
00361          88  PD-NO-SPECIAL-BILL-FREQ          VALUE 'N' ' '.
00362      12  PD-STATUS                    PIC  X(01).
00363          88  PD-STATUS-ACTIVE                 VALUE '0'.
00364          88  PD-STATUS-INACTIVE               VALUE '1'.
00365      12  PD-STD-AH-TYPE               PIC  X(02).
00366      12  PD-TAX-NUMBER                PIC  X(11).
00367      12  PD-TOL-CLM                   PIC S9(03)V9(02) COMP-3.
00368      12  PD-USER-FIELDS.
00369          16  PD-USER-FLD-1            PIC  X(02).
00370          16  PD-USER-FLD-2            PIC  X(02).
00371          16  PD-USER-FLD-3            PIC  X(02).
00372          16  PD-USER-FLD-4            PIC  X(02).
00373          16  PD-USER-FLD-5            PIC  X(02).
00374      12  PD-USER-SELECT-OPTIONS.
00375          16  PD-USER-SELECT-1         PIC  X(10).
00376          16  PD-USER-SELECT-2         PIC  X(10).
00377          16  PD-USER-SELECT-3         PIC  X(10).
00378          16  PD-USER-SELECT-4         PIC  X(10).
00379          16  PD-USER-SELECT-5         PIC  X(10).
00380      12  PD-DIS-RPT021-EXP-PCT        PIC S9(03)V9(04) COMP-3.
00381      12  PD-ADD-RPT021-EXP-PCT        PIC S9(03)V9(04) COMP-3.
00382      12  FILLER                       PIC  X(20).
00383
00384 ******************************************************************
00385 *                CLIENT USE AREAS                                *
00386 ******************************************************************
00387
00388      12  PD-CLIENT-USE-AREA-1         PIC  X(30).
00389      12  PD-CLIENT-USE-AREA-2         PIC  X(30).
00390      12  PD-CLIENT-USE-AREA-3         PIC  X(11).
00391      12  PD-CLIENT-USE-AREA-4         PIC  X(30).
00392      12  PD-CLIENT-USE-AREA-5         PIC  X(30).
00393      12  PD-CLIENT-USE-AREA-6         PIC  X(11).
00394      12  PD-CLIENT-USE-AREA-7         PIC  X(30).
00395      12  PD-CLIENT-USE-AREA-8         PIC  X(30).
00396      12  PD-CLIENT-USE-AREA-9         PIC  X(11).
00397
00398 ******************************************************************
00399 *                TRANSFER DATA                                   *
00400 ******************************************************************
00401      12  PD-TRANSFERRED-FROM.
00402          16  PD-TRNFROM-CARRIER       PIC  X(01).
00403          16  PD-TRNFROM-GROUPING.
00404              20  PD-TRNFROM-GRP-PREFIX
00405                                       PIC  X(03).
00406              20  PD-TRNFROM-GRP-PRIME PIC  X(03).
00407          16  PD-TRNFROM-STATE         PIC  X(02).
00408          16  PD-TRNFROM-PRODUCER.
00409              20  PD-TRNFROM-PROD-PREFIX
00410                                       PIC  X(04).
00411              20  PD-TRNFROM-PROD-PRIME
00412                                       PIC  X(06).
00413          16  PD-TRNFROM-DATE          PIC  X(02).
00414      12  PD-TRANSFERRED-TO.
00415          16  PD-TRNTO-CARRIER         PIC  X(01).
00416          16  PD-TRNTO-GROUPING.
00417              20  PD-TRNTO-GRP-PREFIX  PIC  X(03).
00418              20  PD-TRNTO-GRP-PRIME   PIC  X(03).
00419          16  PD-TRNTO-STATE           PIC  X(02).
00420          16  PD-TRNTO-PRODUCER.
00421              20  PD-TRNTO-PROD-PREFIX PIC  X(04).
00422              20  PD-TRNTO-PROD-PRIME  PIC  X(06).
00423          16  PD-TRNTO-DATE            PIC  X(02).
00424      12  FILLER                       PIC  X(20).
00425
00426 ******************************************************************
00427 *                MORTGAGE PLANS SOLD                             *
00428 ******************************************************************
00429
00430      12  PD-PLANS-SOLD.
00431          16  PD-PRODUCER-PLANS  OCCURS 40 TIMES
00432                                 INDEXED BY PD-PLAN-NDX
00433                                            PD-PLAN-NDX2.
00434              20  PD-INDIVIDUAL-PLAN.
00435                  24  PD-PLAN-CODE     PIC  X(02).
00436                  24  PD-PLAN-REVISION PIC  X(03).
00437              20  PD-IBNR-PERCENT      PIC S9(01)V9(04) COMP-3.
00438      12  FILLER                       PIC  X(54).
00439
00440 ******************************************************************
00441 *                 AGENT AND COMMISSION DATA                      *
00442 ******************************************************************
00443
00444      12  PD-COMMISSION-INFORMATION.
00445          16  PD-REMIT-TO              PIC S9(03)   COMP-3.
00446          16  PD-RECALCULATION-SW      PIC  X(01).
00447              88  PD-RECALC-DETAIL             VALUE 'Y'.
00448              88  PD-RECALC-NO-DETAIL          VALUE 'I'.
00449              88  PD-IGNORE-RECALC             VALUE 'N'.
00450              88  PD-VALID-RECALCULATION-SW    VALUE 'Y' 'I' 'N'.
00451          16  PD-AGENT-DATA.
00452              20  PD-AGENT-ENTRY       OCCURS 5 TIMES
00453                                     INDEXED BY PD-AGENT-NDX
00454                                                PD-AGENT-NDX2.
00455                  24  PD-AGENT-NUMBER  PIC  X(10).
00456                  24  PD-AGENT-TYPE    PIC  X(01).
00457                      88  PD-AGENT-TYPE-A      VALUE 'C' 'D'.
00458                      88  PD-AGENT-TYPE-G      VALUE 'O' 'R'
00459                                                     'P' 'T'
00460                                                     'W'.
00461                      88  PD-AGENT-GROSS       VALUE 'C'.
00462                      88  PD-AGENT-REINS       VALUE 'R'.
00463                      88  PD-AGENT-GROSS-REINS VALUE 'D'.
00464                      88  PD-OVERWRITE-GROSS   VALUE 'O'.
00465                      88  PD-OVERWRITE-GROSS-REINS
00466                                           VALUE 'P'.
00467                      88  PD-OVERWRITE-REINS   VALUE 'T'.
00468                      88  PD-REINS-ONLY        VALUE 'W'.
00469                      88  PD-VALID-AGENT-TYPE  VALUE 'C' 'R'
00470                                                 'D' 'O' 'P'
00471                                                 'T' 'W'.
00472                  24  PD-COMMISSION-BILLED-PAID
00473                                       PIC  X(01).
00474                      88  PD-AGENT-BILLED      VALUE 'B'.
00475                      88  PD-AGENT-PAID        VALUE 'P'.
00476                  24  PD-COMP-RECALC-FLAG
00477                                       PIC  X(01).
00478                      88  PD-BYPASS-RECALC     VALUE 'N'.
00479                      88  PD-VALID-RECALC-FLAG VALUE ' ' 'N'.
00480      12  FILLER                       PIC  X(55).
00481
00482 ******************************************************************
00483 *                BANK DATA                                       *
00484 ******************************************************************
00485
00486      12  PD-BANK-ACCOUNT-NUMBER       PIC  X(20).
00487      12  PD-BANK-TRANSIT-NUMBER.
00488          16  PD-FEDERAL-NUMBER        PIC  X(04).
00489          16  PD-BANK-NUMBER           PIC  X(04).
00490      12  PD-CHARGE-CARD-EXP-DT        PIC  X(02).
00491      12  PD-CHARGE-CARD-TYPE          PIC  X(02).
00492          88  PD-AMERICAN-EXPRESS                 VALUE 'AE'.
00493          88  PD-CARTE-BLANCHE                    VALUE 'CB'.
00494          88  PD-DINERS-CLUB                      VALUE 'DN'.
00495          88  PD-DISCOVER                         VALUE 'DS'.
00496          88  PD-MASTER-CARD                      VALUE 'MC'.
00497          88  PD-VISA                             VALUE 'VI'.
00498      12  PD-SIGNATURE-NAME            PIC  X(25).
00499      12  PD-AUTHORIZATION-SW          PIC  X(01).
00500 ******************************************************************
00501 *                GENERIC FILLER                                  *
00502 ******************************************************************
00503
00504      12  PD-DATE-TEST                 PIC S9(08) COMP.
00505      12  FILLER                       PIC  X(62).
00506
00507 ******************************************************************
00641      EJECT
00642 *                                COPY ERCDMDNT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ERCDMDNT                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.005                          *
00006 *                                                                *
00007 *        FILE DESCRIPTION = DMD CERTIFICATE NOTES                *
00008 *                                                                *
00009 *        THIS COPYBOOK IS A REDEFINES OF ERCNOTE -               *
00010 *                                                                *
00011 *        FILE TYPE= VSAM,KSDS                                    *
00012 *        RECORD SIZE = 825    RECFORM = FIXED                    *
00013 *                                                                *
00014 *        BASE CLUSTER = ERNOTE        RKP=2,LEN=33               *
00015 *                                                                *
00016 *        LOG = YES                                               *
00017 *        SERVREQ = DELETE,UPDATE,NEWREC                          *
00018 *                                                                *
00019 ******************************************************************
00020
00021  01  CERTIFICATE-NOTE.
00022      12  CN-RECORD-ID                     PIC  XX.
00023          88  VALID-CN-ID                      VALUE 'CN'.
00024
00025      12  CN-CONTROL-PRIMARY.
00026          16  CN-COMPANY-CD                PIC X.
00027          16  CN-CERT-KEY.
00028              20  CN-CARRIER               PIC X.
00029              20  CN-GROUPING.
00030                  24  CN-GROUPING-PREFIX   PIC XXX.
00031                  24  CN-GROUPING-PRIME    PIC XXX.
00032              20  CN-STATE                 PIC XX.
00033              20  CN-ACCOUNT.
00034                  24  CN-ACCOUNT-PREFIX    PIC X(4).
00035                  24  CN-ACCOUNT-PRIME     PIC X(6).
00036              20  CN-CERT-EFF-DT           PIC XX.
00037              20  CN-CERT-NO.
00038                  24  CN-CERT-PRIME        PIC X(10).
00039                  24  CN-CERT-SFX          PIC X.
00040
00041      12  CN-BILLING-START-LINE-NO         PIC 99.
00042      12  CN-BILLING-END-LINE-NO           PIC 99.
00043
00044      12  CN-LINES.
00045          16  CN-LINE                      PIC X(77)  OCCURS 10.
00046
00047      12  CN-CSI-NOTES REDEFINES CN-LINES.
00048          16  CN-CSI-TEXT-NOTES            PIC X(77)  OCCURS 6.
00049          16  CN-CSI-GENERAL-DATA-AREA.
00050              20  CN-CSI-GENERAL-DATA      PIC X(77)  OCCURS 2.
00051
00052          16  CN-CSI-GENERAL-DATA-R REDEFINES
00053              CN-CSI-GENERAL-DATA-AREA.
00054              20  CN-CSI-GEN-NOC-KEY           PIC X(11).
00055              20  CN-CSI-GEN-PRI-INSD-1ST-NAME PIC X(15).
00056              20  CN-CSI-GEN-SEC-INSD-1ST-NAME PIC X(15).
00057              20  CN-CSI-GEN-INSD-WORK-PHONE   PIC X(10).
00058              20  CN-CSI-GEN-INFRM-1ST-NAME    PIC X(15).
00059              20  CN-CSI-GEN-INFRM-LAST-NAME   PIC X(20).
00060              20  CN-CSI-GEN-INFRM-MI          PIC X.
00061              20  CN-CSI-GEN-INFRM-PHONE       PIC X(10).
00062              20  CN-CSI-GEN-INFRM-REL         PIC X(15).
00063              20  FILLER                       PIC XX.
00064              20  CN-CSI-GEN-DATA-SOURCE       PIC XX.
00065              20  FILLER                       PIC X(38).
00066
00067          16  CN-CSI-PRODUCT-DATA-AREA.
00068              20  CN-CSI-PRODUCT-DATA      PIC X(77)  OCCURS 2.
00069
00070          16  CN-CSI-CREDIT-CARD-DATA REDEFINES
00071              CN-CSI-PRODUCT-DATA-AREA.
00072              20  CN-CSI-CC-BILL-BANK-ID   PIC X(6).
00073              20  CN-CSI-CC-CANCEL-CD      PIC XX.
00074              20  CN-CSI-CC-CANCEL-DT      PIC X(8).
00075              20  CN-CSI-CC-CARD-TYPE      PIC XX.
00076              20  CN-CSI-CC-CHANGE-AGE     PIC 999.
00077              20  CN-CSI-CC-DIAGNOSIS-CD   PIC X(6).
00078              20  FILLER                   PIC XX.
00079              20  CN-CSI-CC-INSURED-BAL    PIC S9(5)V99  COMP-3.
00080              20  CN-CSI-CC-INTEREST-AMT   PIC S9(5)V99  COMP-3.
00081              20  CN-CSI-CC-INTEREST-PAID  PIC X.
00082              20  CN-CSI-CC-ISSUE-ST       PIC XX.
00083              20  CN-CSI-CC-MAX-BEN-LIMIT  PIC S9(5)V99  COMP-3.
00084              20  CN-CSI-CC-MAX-BENEFITS   PIC S9(5)V99  COMP-3.
00085              20  CN-CSI-CC-MIN-PAY-AMT    PIC S9(5)V99  COMP-3.
00086              20  CN-CSI-CC-MIN-PAY-PCT    PIC SV9(6)    COMP-3.
00087              20  CN-CSI-CC-OLD-ACCT-NO    PIC X(20).
00088              20  CN-CSI-CC-POLICY-TYPE    PIC XXX.
00089              20  CN-CSI-CC-PREMIUM-AMT    PIC S999V99   COMP-3.
00090              20  CN-CSI-CC-PREMIUM-RT     PIC S999V999  COMP-3.
00091              20  CN-CSI-CC-PREV-CLAIM-NO  PIC X(7).
00092              20  CN-CSI-CC-SIGNED-DT      PIC X(8).
00093              20  CN-CSI-CC-SPECIAL-TERM   PIC S999      COMP-3.
00094              20  CN-CSI-CC-STMNT-DT       PIC X(8).
00095              20  CN-CSI-CC-TERM-AGE       PIC 999.
00096              20  CN-CSI-CC-TOL-BALANCE    PIC S9(5)V99  COMP-3.
00097              20  CN-CSI-CC-WAIV-PREM-FLAG PIC X.
00098              20  CN-CSI-CC-ISSUE-DT       PIC X(8).
00099              20  CN-CSI-CC-BEN-CALC-SW    PIC X.
00100              20  CN-CSI-CC-TERM-ROUND-SW  PIC X.
00101              20  FILLER                   PIC X(25).
00102
00103          16  CN-CSI-FAMILY-LEAVE-DATA REDEFINES
00104              CN-CSI-CREDIT-CARD-DATA.
00105              20  CN-CSI-FL-BILL-BANK-ID   PIC X(6).
00106              20  CN-CSI-FL-CANCEL-CD      PIC XX.
00107              20  CN-CSI-FL-CANCEL-DT      PIC X(8).
00108              20  CN-CSI-FL-CARD-TYPE      PIC XX.
00109              20  CN-CSI-FL-CHANGE-AGE     PIC 999.
00110              20  CN-CSI-FL-DIAGNOSIS-CD   PIC X(6).
00111              20  FILLER                   PIC XX.
00112              20  CN-CSI-FL-INSURED-BAL    PIC S9(5)V99  COMP-3.
00113              20  CN-CSI-FL-INTEREST-AMT   PIC S9(5)V99  COMP-3.
00114              20  CN-CSI-FL-INTEREST-PAID  PIC X.
00115              20  CN-CSI-FL-ISSUE-ST       PIC XX.
00116              20  CN-CSI-FL-MAX-BEN-LIMIT  PIC S9(5)V99  COMP-3.
00117              20  CN-CSI-FL-MAX-BENEFITS   PIC S9(5)V99  COMP-3.
00118              20  CN-CSI-FL-MIN-PAY-AMT    PIC S9(5)V99  COMP-3.
00119              20  CN-CSI-FL-MIN-PAY-PCT    PIC SV9(6)    COMP-3.
00120              20  CN-CSI-FL-OLD-ACCT-NO    PIC X(20).
00121              20  CN-CSI-FL-POLICY-TYPE    PIC XXX.
00122              20  CN-CSI-FL-PREMIUM-AMT    PIC S999V99   COMP-3.
00123              20  CN-CSI-FL-PREMIUM-RT     PIC S999V999  COMP-3.
00124              20  CN-CSI-FL-PREV-CLAIM-NO  PIC X(7).
00125              20  CN-CSI-FL-SIGNED-DT      PIC X(8).
00126              20  CN-CSI-FL-SPECIAL-TERM   PIC S999      COMP-3.
00127              20  CN-CSI-FL-STMNT-DT       PIC X(8).
00128              20  CN-CSI-FL-TERM-AGE       PIC 999.
00129              20  CN-CSI-FL-TOL-BALANCE    PIC S9(5)V99  COMP-3.
00130              20  CN-CSI-FL-WAIV-PREM-FLAG PIC X.
00131              20  CN-CSI-FL-ISSUE-DT       PIC X(8).
00132              20  CN-CSI-FL-BEN-CALC-SW    PIC X.
00133              20  CN-CSI-FL-TERM-ROUND-SW  PIC X.
00134              20  CN-CSI-FL-LAST-DAY-WRKED PIC X(8).
00135              20  FILLER                   PIC X(17).
00136
00137          16  CN-CSI-SENIOR-LIFE-DATA REDEFINES
00138              CN-CSI-FAMILY-LEAVE-DATA.
00139              20  CN-CSI-SL-BENE-DOB       PIC X(8).
00140              20  CN-CSI-SL-BENE-NAME      PIC X(27).
00141              20  CN-CSI-SL-BENE-REL       PIC X(8).
00142              20  CN-CSI-SL-BENE-SSN       PIC S9(9)     COMP-3.
00143              20  CN-CSI-SL-BILL-BANK-ID   PIC X(6).
00144              20  CN-CSI-SL-CANCEL-DT      PIC X(8).
00145              20  CN-CSI-SL-DIAGNOSIS-CD   PIC X(6).
00146              20  CN-CSI-SL-INT-CHECK-DT   PIC X(8).
00147              20  CN-CSI-SL-INT-CHECK-NO   PIC S9(7)     COMP-3.
00148              20  CN-CSI-SL-INT-ON-PROCEEDS
00149                                           PIC S9(5)V99  COMP-3.
00150              20  CN-CSI-SL-ISSUE-DT       PIC X(8).
00151              20  CN-CSI-SL-ISSUE-ST       PIC XX.
00152              20  CN-CSI-SL-LIFE-PROCEEDS  PIC S9(5)V99  COMP-3.
00153              20  CN-CSI-SL-LOAN-INT-DUE   PIC S9(5)V99  COMP-3.
00154              20  CN-CSI-SL-POLICY-BENEFITS
00155                                           PIC S9(5)V99  COMP-3.
00156              20  CN-CSI-SL-POLICY-TYPE    PIC XXX.
00157              20  CN-CSI-SL-PREM-AMT       PIC S9(5)V99  COMP-3.
00158              20  CN-CSI-SL-PREM-CHECK-DT  PIC X(8).
00159              20  CN-CSI-SL-PREM-CHECK-NO  PIC S9(7)     COMP-3.
00160              20  CN-CSI-SL-PREM-DUE       PIC S9(5)V99  COMP-3.
00161              20  CN-CSI-SL-PREM-MODE      PIC 99.
00162              20  CN-CSI-SL-PREM-REFUND    PIC S9(5)V99  COMP-3.
00163              20  CN-CSI-SL-PREM-SUSP-DT   PIC X(8).
00164              20  CN-CSI-SL-SIGNED-DT      PIC X(8).
00165              20  CN-CSI-SL-STATE-NOT      PIC X.
00166              20  FILLER                   PIC XX.
00167
00168          16  CN-CSI-PURCH-PROP-DATA REDEFINES
00169              CN-CSI-SENIOR-LIFE-DATA.
00170              20  CN-CSI-PP-CARD-TYPE      PIC XX.
00171              20  CN-CSI-PP-CHANGE-AGE     PIC 999.
00172              20  CN-CSI-PP-BEN-PAID-TO-DATE
00173                                           PIC S9(5)V99  COMP-3.
00174              20  CN-CSI-PP-BILL-BANK-ID   PIC X(6).
00175              20  CN-CSI-PP-CANCEL-CD      PIC XX.
00176              20  CN-CSI-PP-CANCEL-DT      PIC X(8).
00177              20  CN-CSI-PP-DIAGNOSIS-CD   PIC X(6).
00178              20  CN-CSI-PP-ISSUE-DT       PIC X(8).
00179              20  CN-CSI-PP-ISSUE-ST       PIC XX.
00180              20  CN-CSI-PP-MANUFACTURER   PIC X(17).
00181              20  CN-CSI-PP-MODEL-NO       PIC X(8).
00182              20  CN-CSI-PP-OLD-ACCT-NO    PIC X(20).
00183              20  CN-CSI-PP-POLICY-TYPE    PIC XXX.
00184              20  CN-CSI-PP-PREMIUM-RT     PIC S999V999  COMP-3.
00185              20  CN-CSI-PP-PREV-CLAIM-NO  PIC X(7).
00186              20  CN-CSI-PP-PURCHASE-DT    PIC X(8).
00187              20  CN-CSI-PP-PURCHASE-PRICE PIC S9(5)V99  COMP-3.
00188              20  CN-CSI-PP-REPAIR         PIC X.
00189              20  CN-CSI-PP-REPLACE        PIC X.
00190              20  CN-CSI-PP-SERIAL-NO      PIC X(16).
00191              20  CN-CSI-PP-SIGNED-DT      PIC X(8).
00192              20  CN-CSI-PP-STMNT-DT       PIC X(8).
00193              20  CN-CSI-PP-TERM-AGE       PIC 999.
00194              20  FILLER                   PIC X(5).
00195
00196          16  CN-CSI-EXT-WARR-DATA REDEFINES
00197              CN-CSI-PURCH-PROP-DATA.
00198              20  CN-CSI-EW-CARD-TYPE      PIC XX.
00199              20  CN-CSI-EW-CHANGE-AGE     PIC 999.
00200              20  CN-CSI-EW-BILL-BANK-ID   PIC X(6).
00201              20  CN-CSI-EW-CANCEL-CD      PIC XX.
00202              20  CN-CSI-EW-CANCEL-DT      PIC X(8).
00203              20  CN-CSI-EW-DIAGNOSIS-CD   PIC X(6).
00204              20  CN-CSI-EW-ISSUE-DT       PIC X(8).
00205              20  CN-CSI-EW-ISSUE-ST       PIC XX.
00206              20  CN-CSI-EW-MANUFACTURER   PIC X(17).
00207              20  CN-CSI-EW-MODEL-NO       PIC X(8).
00208              20  CN-CSI-EW-OLD-ACCT-NO    PIC X(20).
00209              20  CN-CSI-EW-POLICY-TYPE    PIC XXX.
00210              20  CN-CSI-EW-PREMIUM-RT     PIC S999V999  COMP-3.
00211              20  CN-CSI-EW-PREV-CLAIM-NO  PIC X(7).
00212              20  CN-CSI-EW-PURCHASE-DT    PIC X(8).
00213              20  CN-CSI-EW-PURCHASE-PRICE PIC S9(5)V99  COMP-3.
00214              20  CN-CSI-EW-REPAIR-COST    PIC S9(5)V99  COMP-3.
00215              20  CN-CSI-EW-REPLACE        PIC X.
00216              20  CN-CSI-EW-SERIAL-NO      PIC X(16).
00217              20  CN-CSI-EW-SIGNED-DT      PIC X(8).
00218              20  CN-CSI-EW-STMNT-DT       PIC X(8).
00219              20  CN-CSI-EW-TERM-AGE       PIC 999.
00220              20  CN-CSI-EW-WARRANTY-NO    PIC 99.
00221              20  FILLER                   PIC X(4).
00222
00223      12  CN-LAST-MAINT-DT                 PIC XX.
00224      12  CN-LAST-MAINT-HHMMSS             PIC S9(7)     COMP-3.
00225      12  CN-LAST-MAINT-USER               PIC X(4).
00226      12  FILLER                           PIC X(6).
00227
00228 ******************************************************************
00643      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA ACTIVITY-TRAILERS
                                CLAIM-MASTER ACCOUNT-MASTER
                                LETTER-ARCHIVE LETTER-ARCHIVE-TEMP
                                CHECK-QUE CONTROL-FILE DENIAL-CODES
                                NAPERSOFT-FILE ENCLOSURE-CODES
                                PRODUCER-MASTER CERTIFICATE-NOTE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL142' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00645
00646      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00647
00648 *    NOTE *******************************************************
00649 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL *
00650 *         *  FROM ANOTHER MODULE.                               *
00651 *         *******************************************************.
00652
00653      MOVE PI-EL142-PRIORITY      TO WS-PI-EL142-PRIORITY.
00654
00655      IF EIBCALEN NOT GREATER ZERO
00656          MOVE UNACCESS-MSG       TO  LOGOFF-MSG
00657          GO TO 8300-SEND-TEXT.
00658
00659      
      * EXEC CICS HANDLE CONDITION
00660 *        PGMIDERR (9600-PGMIDERR)
00661 *        NOTFND   (0130-MAIN-LOGIC)
00662 *        ENDFILE  (6000-END-OF-FILE)
00663 *        ERROR    (9990-ERROR)
00664 *    END-EXEC.
      *    MOVE '"$LI''.                ! " #00009052' TO DFHEIV0
           MOVE X'22244C49272E202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303039303532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00665
00666      MOVE EIBDATE               TO  DC-JULIAN-YYDDD.
00667      MOVE '5'                   TO  DC-OPTION-CODE.
00668      PERFORM 8500-DATE-CONVERSION.
00669      MOVE DC-BIN-DATE-1         TO  WS-CURRENT-DATE.
00670      MOVE DC-GREG-DATE-1-YMD    TO  SAVE-DATE-YMD.
00671
00672      IF SAVE-DATE-YY GREATER 70
00673          MOVE 19                TO  SAVE-DATE-CC
00674        ELSE
00675          MOVE 20                TO  SAVE-DATE-CC.
00676
00677      EJECT
00678  0010-MAIN-LOGIC.
00679      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00680          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00681              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6
00682              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5
00683              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4
00684              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3
00685              MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2
00686              MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1
00687              MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM
00688              MOVE THIS-PGM             TO  PI-CALLING-PROGRAM
00689            ELSE
00690              MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM
00691              MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM
00692              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1
00693              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2
00694              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3
00695              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4
00696              MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5
00697              MOVE SPACES               TO  PI-SAVED-PROGRAM-6
00698        ELSE
00699          GO TO 0020-MAIN-LOGIC.
00700
00701  0015-MAIN-LOGIC.
00702 *    NOTE *******************************************************
00703 *         *      INITALIZE THE WORK FIELDS FOR THE PROGRAM      *
00704 *         *  INTERFACE BLOCK FOR THIS MODULE.                   *
00705 *         *******************************************************.
00706
00707      IF PI-RETURN-TO-PROGRAM = PGM-EL1501
00708          NEXT SENTENCE
00709      ELSE
00710          MOVE +1                 TO  PI-REMINDERS-SW
00711                                      PI-LETTERS-SW
00712                                      PI-PAYMENTS-SW
00713                                      PI-AUTO-PAY-SW
00714                                      PI-NOTES-SW
00715                                      PI-RES-EXP-SW
00716                                      PI-DENIALS-SW
00717                                      PI-INCURRED-DATE-SW
00718                                      PI-FORMS-SW.
00719
00720      MOVE ZERO                   TO  PI-END-OF-FILE
00721                                      PI-RECORD-COUNT
00722                                      PI-TRAILER-NUMBER
042110                                     PI-MAPG-DELETE-CNT
00723
00724      MOVE LOW-VALUES             TO  PI-AFTER-DATE
00725                                      PI-AFTER-DATE-2
00726                                      PI-AFTER-DATE-3.
00727
00728      MOVE DFHENTER               TO  PI-PREV-AID.
00729
020413     MOVE '1'                    TO PI-APPROVAL-LEVEL
020413     MOVE PI-COMPANY-ID          TO CNTL-CO
020413     MOVE '2'                    TO CNTL-RECORD-TYPE
020413     MOVE +0                     TO CNTL-SEQ
020413     MOVE PI-PROCESSOR-ID        TO CNTL-GENL
020413
020413     
      * EXEC CICS READ
020413*        DATASET  ('ELCNTL')
020413*        SET      (ADDRESS OF CONTROL-FILE)
020413*        RIDFLD   (WS-CONTROL-FILE-KEY)
020413*        RESP     (WS-RESPONSE)
020413*    END-EXEC
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00009130' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303039313330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
020413     IF RESP-NORMAL
020413        MOVE CF-APPROVAL-LEVEL TO PI-APPROVAL-LEVEL
020413     END-IF
020413
00730      IF FIRST-TIME
00731          MOVE 'N'                TO  PI-FIRST-TIME-SW
00732          IF PI-RETURN-TO-PROGRAM = PGM-EL1501
00733              GO TO 4000-READ-TRAILER-FILE.
00734
00735      MOVE LOW-VALUES             TO  EL142AO.
00736
00737      MOVE PI-TRAILER-NUMBER      TO  ASEQO.
00738      MOVE AL-UNNON               TO  ASEQA.
00739      MOVE -1                     TO  ARECDTEL.
00740
00741      MOVE EL142A                 TO  PI-MAP-NAME.
00742
00743      PERFORM 8100-SEND-INITIAL-MAP.
00744
00745      GO TO 9100-RETURN-TRAN.
00746
00747      EJECT
00748  0020-MAIN-LOGIC.
00749 *    NOTE *******************************************************
00750 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- *
00751 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    *
00752 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         *
00753 *         *******************************************************.
00754
00755      IF EIBAID = DFHCLEAR
00756          GO TO 9400-CLEAR.
00757
00758      IF PI-PROCESSOR-ID = 'LGXX'
00759          NEXT SENTENCE
00760      ELSE
00761          
      * EXEC CICS  READQ TS
00762 *            QUEUE   (PI-SECURITY-TEMP-STORE-ID)
00763 *            INTO    (SECURITY-CONTROL)
00764 *            LENGTH  (SC-COMM-LENGTH)
00765 *            ITEM    (SC-ITEM)
00766 *        END-EXEC
      *    MOVE '*$II   L              ''   #00009171' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303039313731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00767          MOVE SC-CLAIMS-DISPLAY (15)  TO  PI-DISPLAY-CAP
00768          MOVE SC-CLAIMS-UPDATE  (15)  TO  PI-MODIFY-CAP
00769          IF NOT DISPLAY-CAP
00770              MOVE 'READ'          TO SM-READ
00771              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00772              MOVE ER-0070         TO  EMI-ERROR
00773              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00774              GO TO 8100-SEND-INITIAL-MAP.
00775
00776      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00777          MOVE ER-0008            TO  EMI-ERROR
00778          IF PI-MAP-NAME = EL142A
00779              MOVE -1             TO  ARECDTEL
00780              PERFORM 8200-SEND-DATAONLY
00781            ELSE
00782          IF PI-MAP-NAME = EL142B
00783              MOVE -1             TO  BPFKL
00784              PERFORM 8200-SEND-DATAONLY
00785            ELSE
00786          IF PI-MAP-NAME = EL142B2
00787              MOVE -1             TO  KPFKL
00788              PERFORM 8200-SEND-DATAONLY
00789            ELSE
00790          IF PI-MAP-NAME = EL142C
00791              MOVE -1             TO  CPFKL
00792              PERFORM 8200-SEND-DATAONLY
00793            ELSE
00794          IF PI-MAP-NAME = EL142D
00795              MOVE -1             TO  DPFKL
00796              PERFORM 8200-SEND-DATAONLY
00797            ELSE
00798          IF PI-MAP-NAME = EL142D2
00799              MOVE -1             TO  LPFKL
00800              PERFORM 8200-SEND-DATAONLY
00801            ELSE
00802          IF PI-MAP-NAME = EL142E
00803              MOVE -1             TO  EPFKL
00804              PERFORM 8200-SEND-DATAONLY
00805            ELSE
00806          IF PI-MAP-NAME = EL142F
00807              MOVE -1             TO  FPFKL
00808              PERFORM 8200-SEND-DATAONLY
00809            ELSE
00810          IF PI-MAP-NAME = EL142G
00811              MOVE -1             TO  GPFKL
00812              PERFORM 8200-SEND-DATAONLY
00813            ELSE
00814          IF PI-MAP-NAME = EL142H
00815              MOVE -1             TO  HPFKL
00816              PERFORM 8200-SEND-DATAONLY
00817            ELSE
00818          IF PI-MAP-NAME = EL142I
00819              MOVE -1             TO  IPFKL
00820              PERFORM 8200-SEND-DATAONLY
00821            ELSE
00822          IF PI-MAP-NAME = EL142J
00823              MOVE -1             TO  JPFKL
00824              PERFORM 8200-SEND-DATAONLY
00825            ELSE
00826              PERFORM 8200-SEND-DATAONLY.
00827
00828      
      * EXEC CICS RECEIVE
00829 *        INTO   (EL142DI)
00830 *        MAPSET (WS-MAPSET-NAME)
00831 *        MAP    (PI-MAP-NAME)
00832 *    END-EXEC.
           MOVE LENGTH OF
            EL142DI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00009238' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303039323338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 EL142DI, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00833
00834      IF PI-MAP-NAME = EL142B
00835          IF BPFKL GREATER ZERO
00836              IF EIBAID NOT = DFHENTER
00837                  MOVE ER-0004    TO  EMI-ERROR
00838                  MOVE AL-UNBOF   TO  BPFKA
00839                  MOVE -1         TO  BPFKL
00840                  PERFORM 8200-SEND-DATAONLY
00841                ELSE
00842                  IF (BPFKO NUMERIC) AND
00843                     (BPFKO GREATER ZERO AND LESS 25)
00844                      MOVE PF-VALUES (BPFKI)  TO  EIBAID
00845                    ELSE
00846                      MOVE ER-0029  TO  EMI-ERROR
00847                      MOVE AL-UNBON TO  BPFKA
00848                      MOVE -1       TO  BPFKL
00849                      PERFORM 8200-SEND-DATAONLY
00850            ELSE
00851              NEXT SENTENCE
00852        ELSE
00853      IF PI-MAP-NAME = EL142B2
00854         IF KPFKL GREATER ZERO
00855            IF EIBAID NOT = DFHENTER
00856               MOVE ER-0004       TO  EMI-ERROR
00857               MOVE AL-UNBOF      TO  KPFKA
00858               MOVE -1            TO  KPFKL
00859               PERFORM 8200-SEND-DATAONLY
00860            ELSE
00861               IF (KPFKO NUMERIC) AND
00862                  (KPFKO GREATER ZERO AND LESS 25)
00863                   MOVE PF-VALUES (KPFKI)  TO  EIBAID
00864               ELSE
00865                   MOVE ER-0029    TO  EMI-ERROR
00866                   MOVE AL-UNBOF   TO  KPFKA
00867                   MOVE -1         TO  KPFKL
00868                   PERFORM 8200-SEND-DATAONLY
00869         ELSE
00870            NEXT SENTENCE
00871      ELSE
00872      IF PI-MAP-NAME = EL142C
00873          IF CPFKL GREATER ZERO
00874              IF EIBAID NOT = DFHENTER
00875                  MOVE ER-0004    TO  EMI-ERROR
00876                  MOVE AL-UNBOF   TO  CPFKA
00877                  MOVE -1         TO  CPFKL
00878                  PERFORM 8200-SEND-DATAONLY
00879                ELSE
00880                  IF (CPFKO NUMERIC) AND
00881                     (CPFKO GREATER ZERO AND LESS 25)
00882                      MOVE PF-VALUES (CPFKI)  TO  EIBAID
00883                    ELSE
00884                      MOVE ER-0029  TO  EMI-ERROR
00885                      MOVE AL-UNBOF TO  CPFKA
00886                      MOVE -1       TO  CPFKL
00887                      PERFORM 8200-SEND-DATAONLY
00888            ELSE
00889              NEXT SENTENCE
00890        ELSE
00891      IF PI-MAP-NAME = EL142D
00892          IF DPFKL GREATER ZERO
00893              IF EIBAID NOT = DFHENTER
00894                  MOVE ER-0004    TO  EMI-ERROR
00895                  MOVE AL-UNBOF   TO  DPFKA
00896                  MOVE -1         TO  DPFKL
00897                  PERFORM 8200-SEND-DATAONLY
00898              ELSE
00899                  IF (DPFKO NUMERIC) AND
00900                     (DPFKO GREATER ZERO AND LESS 25)
00901                      MOVE PF-VALUES (DPFKI)  TO  EIBAID
00902                  ELSE
00903                      MOVE ER-0029  TO  EMI-ERROR
00904                      MOVE AL-UNBOF TO  DPFKA
00905                      MOVE -1       TO  DPFKL
00906                      PERFORM 8200-SEND-DATAONLY
00907          ELSE
00908              NEXT SENTENCE
00909      ELSE
00910      IF PI-MAP-NAME = EL142D2
00911         IF LPFKL GREATER ZERO
00912            IF EIBAID NOT = DFHENTER
00913               MOVE ER-0004       TO  EMI-ERROR
00914               MOVE AL-UNBOF      TO  LPFKA
00915               MOVE -1            TO  LPFKL
00916               PERFORM 8200-SEND-DATAONLY
00917            ELSE
00918               IF (LPFKO NUMERIC) AND
00919                  (LPFKO GREATER ZERO AND LESS 25)
00920                  MOVE PF-VALUES (LPFKI)  TO  EIBAID
00921               ELSE
00922                  MOVE ER-0029    TO  EMI-ERROR
00923                  MOVE AL-UNBON   TO  LPFKA
00924                  MOVE -1         TO  LPFKL
00925                  PERFORM 8200-SEND-DATAONLY
00926         ELSE
00927            NEXT SENTENCE
00928      ELSE
00929      IF PI-MAP-NAME = EL142E
00930          IF EPFKL GREATER ZERO
00931              IF EIBAID NOT = DFHENTER
00932                  MOVE ER-0004    TO  EMI-ERROR
00933                  MOVE AL-UNBOF   TO  EPFKA
00934                  MOVE -1         TO  EPFKL
00935                  PERFORM 8200-SEND-DATAONLY
00936                ELSE
00937                  IF (EPFKO NUMERIC) AND
00938                     (EPFKO GREATER ZERO AND LESS 25)
00939                      MOVE PF-VALUES (EPFKI)  TO  EIBAID
00940                    ELSE
00941                      MOVE ER-0029  TO  EMI-ERROR
00942                      MOVE AL-UNBOF TO  EPFKA
00943                      MOVE -1       TO  EPFKL
00944                      PERFORM 8200-SEND-DATAONLY
00945            ELSE
00946              NEXT SENTENCE
00947        ELSE
00948      IF PI-MAP-NAME = EL142F
00949          IF FPFKL GREATER ZERO
00950              IF EIBAID NOT = DFHENTER
00951                  MOVE ER-0004    TO  EMI-ERROR
00952                  MOVE AL-UNBOF   TO  FPFKA
00953                  MOVE -1         TO  FPFKL
00954                  PERFORM 8200-SEND-DATAONLY
00955                ELSE
00956                  IF (FPFKO NUMERIC) AND
00957                     (FPFKO GREATER ZERO AND LESS 25)
00958                      MOVE PF-VALUES (FPFKI)  TO  EIBAID
00959                    ELSE
00960                      MOVE ER-0029 TO  EMI-ERROR
00961                      MOVE AL-UNBOF TO  FPFKA
00962                      MOVE -1      TO  FPFKL
00963                      PERFORM 8200-SEND-DATAONLY
00964            ELSE
00965              NEXT SENTENCE
00966        ELSE
00967      IF PI-MAP-NAME = EL142G
00968          IF GPFKL GREATER ZERO
00969              IF EIBAID NOT = DFHENTER
00970                  MOVE ER-0004    TO  EMI-ERROR
00971                  MOVE AL-UNBOF   TO  GPFKA
00972                  MOVE -1         TO  GPFKL
00973                  PERFORM 8200-SEND-DATAONLY
00974                ELSE
00975                  IF (GPFKO NUMERIC) AND
00976                     (GPFKO GREATER ZERO AND LESS 25)
00977                      MOVE PF-VALUES (GPFKI)  TO  EIBAID
00978                    ELSE
00979                      MOVE ER-0029  TO  EMI-ERROR
00980                      MOVE AL-UNBOF TO  GPFKA
00981                      MOVE -1       TO  GPFKL
00982                      PERFORM 8200-SEND-DATAONLY
00983            ELSE
00984              NEXT SENTENCE
00985        ELSE
00986      IF PI-MAP-NAME = EL142H
00987          IF HPFKL GREATER ZERO
00988              IF EIBAID NOT = DFHENTER
00989                  MOVE ER-0004    TO  EMI-ERROR
00990                  MOVE AL-UNBOF   TO  HPFKA
00991                  MOVE -1         TO  HPFKL
00992                  PERFORM 8200-SEND-DATAONLY
00993                ELSE
00994                  IF (HPFKO NUMERIC) AND
00995                     (HPFKO GREATER ZERO AND LESS 25)
00996                      MOVE PF-VALUES (HPFKI)  TO  EIBAID
00997                    ELSE
00998                      MOVE ER-0029  TO  EMI-ERROR
00999                      MOVE AL-UNBOF TO  HPFKA
01000                      MOVE -1       TO  HPFKL
01001                      PERFORM 8200-SEND-DATAONLY
01002            ELSE
01003              NEXT SENTENCE
01004        ELSE
01005      IF PI-MAP-NAME = EL142I
01006          IF IPFKL GREATER ZERO
01007              IF EIBAID NOT = DFHENTER
01008                  MOVE ER-0004    TO  EMI-ERROR
01009                  MOVE AL-UNBOF   TO  IPFKA
01010                  MOVE -1         TO  IPFKL
01011                  PERFORM 8200-SEND-DATAONLY
01012                ELSE
01013                  IF (IPFKO NUMERIC) AND
01014                     (IPFKO GREATER ZERO AND LESS 25)
01015                      MOVE PF-VALUES (IPFKI)  TO  EIBAID
01016                    ELSE
01017                      MOVE ER-0029  TO  EMI-ERROR
01018                      MOVE AL-UNBOF TO  IPFKA
01019                      MOVE -1       TO  IPFKL
01020                      PERFORM 8200-SEND-DATAONLY
01021            ELSE
01022              NEXT SENTENCE
01023        ELSE
01024      IF PI-MAP-NAME = EL142J
01025          IF JPFKL GREATER ZERO
01026              IF EIBAID NOT = DFHENTER
01027                  MOVE ER-0004    TO  EMI-ERROR
01028                  MOVE AL-UNBOF   TO  JPFKA
01029                  MOVE -1         TO  JPFKL
01030                  PERFORM 8200-SEND-DATAONLY
01031                ELSE
01032                  IF (JPFKO NUMERIC) AND
01033                     (JPFKO GREATER ZERO AND LESS 25)
01034                      MOVE PF-VALUES (JPFKI)  TO  EIBAID
01035                    ELSE
01036                      MOVE ER-0029  TO  EMI-ERROR
01037                      MOVE AL-UNBOF TO  JPFKA
01038                      MOVE -1       TO  JPFKL
01039                      PERFORM 8200-SEND-DATAONLY.
01040
01041      IF EIBAID = DFHPF12
01042          MOVE EL010              TO  XCTL-PGM
01043          GO TO 9300-XCTL.
01044
01045      IF EIBAID = DFHPF23
01046          GO TO 9000-RETURN-CICS.
01047
01048      IF EIBAID = DFHPF24
01049          MOVE EL126              TO  XCTL-PGM
01050          GO TO 9300-XCTL.
01051
01052      IF EIBAID = DFHENTER OR
01053        (EIBAID = DFHPF1 OR DFHPF2 OR DFHPF5) AND
01054        (PI-MAP-NAME NOT = EL142A AND EL142B2 AND EL142D2)
01055                    OR
01056        (EIBAID = DFHPF6 AND
01057         PI-PROCESSOR-ID = 'PEMA' AND
01058         PI-MAP-NAME = EL142B OR EL142D)
01059            GO TO 0040-MAIN-LOGIC.
01060
01061      MOVE ER-0008                TO  EMI-ERROR.
01062
01063      IF PI-MAP-NAME = EL142A
01064          MOVE -1                 TO  ARECDTEL
01065        ELSE
01066      IF PI-MAP-NAME = EL142B
01067          MOVE -1                 TO  BPFKL
01068        ELSE
01069      IF PI-MAP-NAME = EL142B2
01070          MOVE -1                 TO  KPFKL
01071        ELSE
01072      IF PI-MAP-NAME = EL142C
01073          MOVE -1                 TO  CPFKL
01074        ELSE
01075      IF PI-MAP-NAME = EL142D
01076          MOVE -1                 TO  DPFKL
01077        ELSE
01078      IF PI-MAP-NAME = EL142D2
01079          MOVE -1                 TO  LPFKL
01080        ELSE
01081      IF PI-MAP-NAME = EL142E
01082          MOVE -1                 TO  EPFKL
01083        ELSE
01084      IF PI-MAP-NAME = EL142F
01085          MOVE -1                 TO  FPFKL
01086        ELSE
01087      IF PI-MAP-NAME = EL142G
01088          MOVE -1                 TO  GPFKL
01089        ELSE
01090      IF PI-MAP-NAME = EL142H
01091          MOVE -1                 TO  HPFKL
01092        ELSE
01093      IF PI-MAP-NAME = EL142I
01094          MOVE -1                 TO  IPFKL
01095        ELSE
01096      IF PI-MAP-NAME = EL142J
01097          MOVE -1                 TO  JPFKL.
01098
01099      PERFORM 8200-SEND-DATAONLY.
01100
01101  0040-MAIN-LOGIC.
01102      IF EIBAID = DFHPF1 OR DFHPF2
01103          PERFORM 4000-READ-TRAILER-FILE.
01104
01105      IF EIBAID = DFHPF5
01106          GO TO 0015-MAIN-LOGIC.
01107
01108      EJECT
01109  0100-MAIN-LOGIC.
01110      IF PI-MAP-NAME NOT = EL142A
01111          GO TO 0200-MAIN-LOGIC.
01112
01113      MOVE +3                     TO  EMI-NUMBER-OF-LINES.
01114      MOVE +2                     TO  EMI-SWITCH2.
01115
01116      IF ARECDTEL NOT GREATER ZERO
01117          MOVE LOW-VALUES         TO  PI-AFTER-DATE
01118      ELSE
01119          MOVE ARECDTEI           TO  WS-DEEDIT-FIELD
01120          PERFORM 8600-DEEDIT
01121          MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY
01122          MOVE '4'                TO  DC-OPTION-CODE
01123          PERFORM 8500-DATE-CONVERSION
01124          IF DC-ERROR-CODE NOT = SPACE
01125              MOVE ER-0285        TO  EMI-ERROR
01126              PERFORM 9900-ERROR-FORMAT
01127              MOVE -1             TO  ARECDTEL
01128              MOVE AL-UNBON       TO  ARECDTEA
01129          ELSE
01130              MOVE +1             TO  WS-UPDATE-SW
01131              MOVE DC-BIN-DATE-1  TO  PI-AFTER-DATE
01132              MOVE AL-UNNON       TO  ARECDTEA
01133              MOVE WS-DEEDIT-FIELD-V0  TO  ARECDTEO
01134              INSPECT ARECDTEI CONVERTING SPACES TO SLASH.
01135
01136      IF AREMINDL GREATER ZERO
01137          IF AREMINDI = 'Y'
01138              MOVE +1             TO  PI-REMINDERS-SW
01139                                      WS-UPDATE-SW
01140              MOVE AL-UANON       TO  AREMINDA
01141          ELSE
01142              IF AREMINDI = 'N'
01143                  MOVE ZERO       TO  PI-REMINDERS-SW
01144                  MOVE AL-UANON   TO  AREMINDA
01145              ELSE
01146                  MOVE ER-0286    TO  EMI-ERROR
01147                  PERFORM 9900-ERROR-FORMAT
01148                  MOVE -1         TO  AREMINDL
01149                  MOVE AL-UABON   TO  AREMINDA
01150      ELSE
01151          MOVE ER-0286            TO  EMI-ERROR
01152          PERFORM 9900-ERROR-FORMAT
01153          MOVE -1                 TO  AREMINDL
01154          MOVE AL-UABON           TO  AREMINDA.
01155
01156      IF ALETTERL GREATER ZERO
01157          IF ALETTERI = 'Y'
01158              MOVE +1             TO  PI-LETTERS-SW
01159                                      WS-UPDATE-SW
01160              MOVE AL-UANON       TO  ALETTERA
01161          ELSE
01162              IF ALETTERI = 'N'
01163                  MOVE ZERO       TO  PI-LETTERS-SW
01164                  MOVE AL-UANON   TO  ALETTERA
01165              ELSE
01166                  MOVE ER-0287    TO  EMI-ERROR
01167                  PERFORM 9900-ERROR-FORMAT
01168                  MOVE -1         TO  ALETTERL
01169                  MOVE AL-UABON   TO  ALETTERA
01170      ELSE
01171          MOVE ER-0287            TO  EMI-ERROR
01172          PERFORM 9900-ERROR-FORMAT
01173          MOVE -1                 TO  ALETTERL
01174          MOVE AL-UABON           TO  ALETTERA.
01175
01176      IF APAYMNTL GREATER ZERO
01177          IF APAYMNTI = 'Y'
01178              MOVE +1             TO  PI-PAYMENTS-SW
01179                                      WS-UPDATE-SW
01180              MOVE AL-UANON       TO  APAYMNTA
01181          ELSE
01182              IF APAYMNTI = 'N'
01183                  MOVE ZERO       TO  PI-PAYMENTS-SW
01184                  MOVE AL-UANON   TO  APAYMNTA
01185              ELSE
01186                  MOVE ER-0288       TO  EMI-ERROR
01187                  PERFORM 9900-ERROR-FORMAT
01188                  MOVE -1         TO  APAYMNTL
01189                  MOVE AL-UABON   TO  APAYMNTA
01190      ELSE
01191          MOVE ER-0288            TO  EMI-ERROR
01192          PERFORM 9900-ERROR-FORMAT
01193          MOVE -1                 TO  APAYMNTL
01194          MOVE AL-UABON           TO  APAYMNTA.
01195
01196      IF AAUTOPAL GREATER ZERO
01197          IF AAUTOPAI = 'Y'
01198              MOVE +1             TO  PI-AUTO-PAY-SW
01199                                      WS-UPDATE-SW
01200              MOVE AL-UANON       TO  AAUTOPAA
01201          ELSE
01202              IF AAUTOPAI = 'N'
01203                  MOVE ZERO       TO  PI-AUTO-PAY-SW
01204                  MOVE AL-UANON   TO  AAUTOPAA
01205              ELSE
01206                  MOVE ER-0289       TO  EMI-ERROR
01207                  PERFORM 9900-ERROR-FORMAT
01208                  MOVE -1         TO  AAUTOPAL
01209                  MOVE AL-UABON   TO  AAUTOPAA
01210      ELSE
01211          MOVE ER-0289            TO  EMI-ERROR
01212          PERFORM 9900-ERROR-FORMAT
01213          MOVE -1                 TO  AAUTOPAL
01214          MOVE AL-UABON           TO  AAUTOPAA.
01215
01216      IF ANOTESL GREATER ZERO
01217          IF ANOTESI = 'Y'
01218              MOVE +1             TO  PI-NOTES-SW
01219                                      WS-UPDATE-SW
01220              MOVE AL-UANON       TO  ANOTESA
01221          ELSE
01222              IF ANOTESI = 'N'
01223                  MOVE ZERO       TO  PI-NOTES-SW
01224                  MOVE AL-UANON   TO  ANOTESA
01225              ELSE
01226                  MOVE ER-0290       TO  EMI-ERROR
01227                  PERFORM 9900-ERROR-FORMAT
01228                  MOVE -1         TO  ANOTESL
01229                  MOVE AL-UABON   TO  ANOTESA
01230      ELSE
01231          MOVE ER-0290            TO  EMI-ERROR
01232          PERFORM 9900-ERROR-FORMAT
01233          MOVE -1                 TO  ANOTESL
01234          MOVE AL-UABON           TO  ANOTESA.
01235
01236      IF ARESEXPL GREATER ZERO
01237          IF ARESEXPI = 'Y'
01238              MOVE +1             TO  PI-RES-EXP-SW
01239                                      WS-UPDATE-SW
01240              MOVE AL-UANON       TO  ARESEXPA
01241          ELSE
01242              IF ARESEXPI = 'N'
01243                  MOVE ZERO       TO  PI-RES-EXP-SW
01244                  MOVE AL-UANON   TO  ARESEXPA
01245              ELSE
01246                  MOVE ER-0291    TO  EMI-ERROR
01247                  PERFORM 9900-ERROR-FORMAT
01248                  MOVE -1         TO  ARESEXPL
01249                  MOVE AL-UABON   TO  ARESEXPA
01250      ELSE
01251          MOVE ER-0291            TO  EMI-ERROR
01252          PERFORM 9900-ERROR-FORMAT
01253          MOVE -1                 TO  ARESEXPL
01254          MOVE AL-UABON           TO  ARESEXPA.
01255
01256      IF ADENIALL GREATER ZERO
01257          IF ADENIALI = 'Y'
01258              MOVE +1             TO  PI-DENIALS-SW
01259                                      WS-UPDATE-SW
01260              MOVE AL-UANON       TO  ADENIALA
01261          ELSE
01262              IF ADENIALI = 'N'
01263                  MOVE ZERO       TO  PI-DENIALS-SW
01264              MOVE AL-UANON       TO  ADENIALA
01265              ELSE
01266                  MOVE ER-0292    TO  EMI-ERROR
01267                  PERFORM 9900-ERROR-FORMAT
01268                  MOVE -1         TO  ADENIALL
01269                  MOVE AL-UABON   TO  ADENIALA
01270      ELSE
01271          MOVE ER-0292            TO  EMI-ERROR
01272          PERFORM 9900-ERROR-FORMAT
01273          MOVE -1                 TO  ADENIALL
01274          MOVE AL-UABON           TO  ADENIALA.
01275
01276      IF AIDCL GREATER ZERO
01277          IF AIDCI = 'Y'
01278              MOVE +1             TO  PI-INCURRED-DATE-SW
01279                                      WS-UPDATE-SW
01280              MOVE AL-UANON       TO  AIDCA
01281          ELSE
01282              IF AIDCI = 'N'
01283                  MOVE ZERO       TO  PI-INCURRED-DATE-SW
01284                  MOVE AL-UANON   TO  AIDCA
01285              ELSE
01286                  MOVE ER-0341    TO  EMI-ERROR
01287                  PERFORM 9900-ERROR-FORMAT
01288                  MOVE -1         TO  AIDCL
01289                  MOVE AL-UABON   TO  AIDCA
01290      ELSE
01291          MOVE ER-0341            TO  EMI-ERROR
01292          PERFORM 9900-ERROR-FORMAT
01293          MOVE -1                 TO  AIDCL
01294          MOVE AL-UABON           TO  AIDCA.
01295
01296      IF AFORMSL GREATER ZERO
01297          IF AFORMSI = 'Y'
01298              MOVE +1             TO  PI-FORMS-SW
01299                                      WS-UPDATE-SW
01300              MOVE AL-UANON       TO  AFORMSA
01301          ELSE
01302              IF AFORMSI = 'N'
01303                  MOVE ZERO       TO  PI-FORMS-SW
01304                  MOVE AL-UANON   TO  AFORMSA
01305              ELSE
01306                  MOVE ER-0538    TO  EMI-ERROR
01307                  PERFORM 9900-ERROR-FORMAT
01308                  MOVE -1         TO  AFORMSL
01309                  MOVE AL-UABON   TO  AFORMSA
01310      ELSE
01311          MOVE ER-0538            TO  EMI-ERROR
01312          PERFORM 9900-ERROR-FORMAT
01313          MOVE -1                 TO  AFORMSL
01314          MOVE AL-UABON           TO  AFORMSA.
01315
01316      IF ASEQL NOT GREATER ZERO
01317          MOVE ZERO               TO  PI-TRAILER-NUMBER
01318      ELSE
01319          IF ASEQI IS NUMERIC
01320              MOVE ASEQI          TO  PI-TRAILER-NUMBER
01321          ELSE
01322              MOVE ER-0293        TO  EMI-ERROR
01323              PERFORM 9900-ERROR-FORMAT
01324              MOVE -1             TO  ASEQL
01325              MOVE AL-UNBON       TO  ASEQA.
01326
01327      IF WS-UPDATE-SW = ZERO
01328          MOVE ER-0329            TO  EMI-ERROR
01329          MOVE -1                 TO  AREMINDL
01330          PERFORM 9900-ERROR-FORMAT.
01331
01332      IF WS-ERROR-COUNT GREATER ZERO
01333          PERFORM 8200-SEND-DATAONLY.
01334
01335      MOVE SPACES                 TO  PI-ACTIVITY-TRAILERS-KEY.
01336
01337      MOVE PI-COMPANY-CD          TO  PI-ATK-COMPANY-CODE.
01338      MOVE PI-CARRIER             TO  PI-ATK-CARRIER.
01339      MOVE PI-CLAIM-NO            TO  PI-ATK-CLAIM-NO.
01340      MOVE PI-CERT-NO             TO  PI-ATK-CERT-NO.
01341      MOVE PI-TRAILER-NUMBER      TO  PI-ATK-SEQUENCE-NO.
01342
01343      
      * EXEC CICS READ
01344 *        DATASET   (WS-ACTIVITY-TRAILERS-DSID)
01345 *        RIDFLD    (PI-ACTIVITY-TRAILERS-KEY)
01346 *        SET       (ADDRESS OF ACTIVITY-TRAILERS)
01347 *    END-EXEC
      *    MOVE '&"S        E          (   #00009753' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039373533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ACTIVITY-TRAILERS-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01348
01349      PERFORM 4000-READ-TRAILER-FILE.
01350
01351  0130-MAIN-LOGIC.
01352      MOVE ER-0342                TO  EMI-ERROR.
01353      MOVE -1                     TO  ARECDTEL.
01354      PERFORM 8200-SEND-DATAONLY.
01355
01356      EJECT
01357  0200-MAIN-LOGIC.
01358      IF PI-MAP-NAME NOT = EL142B
01359          GO TO 0300-MAIN-LOGIC.
01360
01361      IF NOT MODIFY-CAP
01362          IF BMAINTI = 'S'
01363              NEXT SENTENCE
01364          ELSE
01365              MOVE 'UPDATE'       TO SM-READ
01366              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
01367              MOVE ER-0070        TO  EMI-ERROR
01368              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01369              GO TO 8100-SEND-INITIAL-MAP.
01370
01371      IF EIBAID = DFHPF6
01372          PERFORM 5000-DISPLAY-CHECK-QUEUE.
01373
01374      MOVE +2                     TO  EMI-NUMBER-OF-LINES
01375                                      EMI-SWITCH2.
01376
01377      IF BMAINTL NOT GREATER ZERO
01378        OR (BMAINTL GREATER ZERO AND
01379            BMAINTI = 'S')
01380              PERFORM 4000-READ-TRAILER-FILE.
01381
01382      IF BMAINTI NOT = 'C'
01383          MOVE ER-0023            TO  EMI-ERROR
01384          MOVE -1                 TO  BMAINTL
01385          MOVE AL-UABON           TO  BMAINTA
01386          PERFORM 8200-SEND-DATAONLY.
01387
020816     IF PI-COMPANY-ID = 'CID' OR 'DCC' OR 'AHL' OR 'VPP'
062121           or 'FNL'
062602        IF (PI-EL142-PRIORITY = '8')
120718           AND (PI-approval-level <> '5')
062602           MOVE ER-8003          TO EMI-ERROR
062602           MOVE -1               TO BMAINTL
062602           MOVE AL-UABON         TO BMAINTA
062602           GO TO 8200-SEND-DATAONLY.
062602
01388      MOVE AL-UANON               TO  BMAINTA.
01389
01390      IF BCKNOL GREATER ZERO
01391          MOVE +1                 TO  WS-UPDATE-SW.
052506
052506     IF  BPRFDTL > 0
052506         MOVE BPRFDTI             TO WS-DEEDIT-FIELD
052506         PERFORM 8600-DEEDIT
052506         MOVE WS-DEEDIT-FIELD-V0  TO DC-GREG-DATE-1-MDY
052506         MOVE '4'                 TO DC-OPTION-CODE
052506         PERFORM 8500-DATE-CONVERSION
052506         IF DC-ERROR-CODE NOT = SPACES
052506            MOVE ER-0021          TO EMI-ERROR
052506            MOVE -1               TO BPRFDTL
052506            MOVE AL-UABON         TO BPRFDTA
052506            PERFORM 9900-ERROR-FORMAT
052506         ELSE
052506            IF DC-BIN-DATE-1 > WS-CURRENT-DATE
052506                MOVE ER-0873      TO EMI-ERROR
052506                MOVE -1           TO BPRFDTL
052506                MOVE AL-UABON     TO BPRFDTA
052506                PERFORM 9900-ERROR-FORMAT
052506            ELSE
052506                MOVE AL-UANON       TO  BPRFDTA
052506                MOVE +1             TO  WS-UPDATE-SW
052506                MOVE DC-BIN-DATE-1    TO WS-PRF-DT
052506                MOVE WS-DEEDIT-FIELD-V0  TO BPRFDTO
052506                INSPECT BPRFDTI CONVERTING ' ' TO '/'.
052506
01392
01393      IF BPAYEEL GREATER ZERO
01394          IF BPAYEEI = ('I' OR 'B' OR 'A' OR 'O' OR 'Q' OR 'P' OR
01395                               'E'           OR
01396                               'INSURED'     OR
01397                               'BENEFICIARY' OR
01398                               'ACCOUNT'     OR
01399                               'OTHER 1'     OR
01400                               'REM BORR'    OR
01401                               'EMPLOYER')
01402              MOVE +1             TO  WS-UPDATE-SW
01403              MOVE AL-UANON       TO  BPAYEEA
01404          ELSE
01405              MOVE -1             TO  BPAYEEL
01406              MOVE AL-UABON       TO  BPAYEEA
01407              MOVE ER-0294        TO  EMI-ERROR
01408              PERFORM 9900-ERROR-FORMAT.
01409
01410      IF BEXPTYPL GREATER ZERO
01411          MOVE +1                 TO WS-UPDATE-SW
01412          IF BEXPTYPI GREATER 0
01413            OR BEXPTYPI = SPACE
01414              MOVE AL-UANON       TO BEXPTYPA
01415          ELSE
01416              MOVE ER-2466        TO EMI-ERROR
01417              MOVE -1             TO BEXPTYPL
01418              MOVE AL-UABON       TO BEXPTYPA
01419              PERFORM 9900-ERROR-FORMAT.
01420
           IF BEOBYNL > +0
              MOVE +1               TO WS-UPDATE-SW
020413        IF BEOBYNI NOT = 'Y' AND 'N' AND 'S'
                 MOVE ER-1561          TO EMI-ERROR
                 MOVE AL-UABON         TO BEOBYNA
                 MOVE -1               TO BEOBYNL
                 PERFORM 9900-ERROR-FORMAT
              END-IF
           END-IF
020413     IF BCLMYNL > +0
020413        MOVE +1               TO WS-UPDATE-SW
020413        IF BCLMYNI NOT = 'Y' AND 'N'
020413           MOVE ER-1566          TO EMI-ERROR
020413           MOVE AL-UABON         TO BCLMYNA
020413           MOVE -1               TO BCLMYNL
020413           PERFORM 9900-ERROR-FORMAT
020413        END-IF
020413     END-IF
020413
020413     IF BSRVYNL > +0
020413        MOVE +1               TO WS-UPDATE-SW
020413        IF BSRVYNI NOT = 'Y' AND 'N'
020413           MOVE ER-1567          TO EMI-ERROR
020413           MOVE AL-UABON         TO BSRVYNA
020413           MOVE -1               TO BSRVYNL
020413           PERFORM 9900-ERROR-FORMAT
020413        END-IF
020413     END-IF
020413
102413     IF BSPRELL > +0
102413        MOVE +1               TO WS-UPDATE-SW
102413        IF BSPRELI NOT = 'Y' AND 'N'
102413           MOVE ER-1569          TO EMI-ERROR
102413           MOVE AL-UABON         TO BSPRELA
102413           MOVE -1               TO BSPRELL
102413           PERFORM 9900-ERROR-FORMAT
102413        END-IF
102413     END-IF
102413
01421      IF BCRSELL GREATER ZERO
01422          IF BCRSELI = SPACES
01423              MOVE LOW-VALUES     TO  WS-CRSEL
01424              MOVE AL-UNNON       TO  BCRSELA
01425              MOVE +1             TO  WS-UPDATE-SW
01426          ELSE
01427              MOVE BCRSELI        TO  WS-DEEDIT-FIELD
01428              PERFORM 8600-DEEDIT
01429              MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY
01430              MOVE '4'            TO  DC-OPTION-CODE
01431              PERFORM 8500-DATE-CONVERSION
01432              IF DC-ERROR-CODE NOT = SPACE
01433                  MOVE ER-0021    TO  EMI-ERROR
01434                  PERFORM 9900-ERROR-FORMAT
01435                  MOVE -1         TO  BCRSELL
01436                  MOVE AL-UNBON   TO  BCRSELA
01437              ELSE
01438                  MOVE +1                  TO  WS-UPDATE-SW
01439                  MOVE DC-BIN-DATE-1       TO  WS-CRSEL
01440                  MOVE AL-UNNON            TO  BCRSELA
01441                  MOVE WS-DEEDIT-FIELD-V0  TO WS-TEMP-DT
01442                  INSPECT WS-TEMP-DT CONVERTING SPACES TO SLASH
01443                  MOVE WS-TEMP-DT          TO BCRSELO.
01444
01445      IF BVOIDSDL GREATER ZERO
01446          IF BVOIDSDI = SPACES
01447              MOVE LOW-VALUES     TO  WS-VOIDSD
01448              MOVE AL-UNNON       TO  BVOIDSDA
01449              MOVE +1             TO  WS-UPDATE-SW
01450          ELSE
01451              MOVE BVOIDSDI       TO  WS-DEEDIT-FIELD
01452              PERFORM 8600-DEEDIT
01453              MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY
01454              MOVE '4'            TO  DC-OPTION-CODE
01455              PERFORM 8500-DATE-CONVERSION
01456              IF DC-ERROR-CODE NOT = SPACE
01457                  MOVE ER-0021    TO  EMI-ERROR
01458                  PERFORM 9900-ERROR-FORMAT
01459                  MOVE -1         TO  BVOIDSDL
01460                  MOVE AL-UNBON   TO  BVOIDSDA
01461              ELSE
01462                  MOVE +1                  TO  WS-UPDATE-SW
01463                  MOVE DC-BIN-DATE-1       TO  WS-VOIDSD
01464                  MOVE AL-UNNON            TO  BVOIDSDA
01465                  MOVE WS-DEEDIT-FIELD-V0  TO WS-TEMP-DT
01466                  INSPECT WS-TEMP-DT CONVERTING SPACES TO SLASH
01467                  MOVE WS-TEMP-DT          TO BVOIDSDO.
01468
01469      IF BCRACPL GREATER ZERO
01470          IF BCRACPI = SPACES
01471              MOVE LOW-VALUES     TO  PI-AFTER-DATE-2
01472              MOVE AL-UNNON       TO  BCRACPA
01473              MOVE +1             TO  WS-UPDATE-SW
01474          ELSE
01475              MOVE BCRACPI        TO  WS-DEEDIT-FIELD
01476              PERFORM 8600-DEEDIT
01477              MOVE WS-DEEDIT-FIELD-V0     TO  DC-GREG-DATE-1-MDY
01478              MOVE '4'            TO  DC-OPTION-CODE
01479              PERFORM 8500-DATE-CONVERSION
01480              IF DC-ERROR-CODE NOT = SPACE
01481                  MOVE ER-0021    TO  EMI-ERROR
01482                  PERFORM 9900-ERROR-FORMAT
01483                  MOVE -1         TO  BCRACPL
01484                  MOVE AL-UNBON   TO  BCRACPA
01485              ELSE
01486                  MOVE +1                  TO WS-UPDATE-SW
01487                  MOVE DC-BIN-DATE-1       TO PI-AFTER-DATE-2
01488                  MOVE AL-UNNON            TO BCRACPA
01489                  MOVE WS-DEEDIT-FIELD-V0  TO WS-TEMP-DT
01490                  INSPECT WS-TEMP-DT CONVERTING SPACES TO SLASH
01491                  MOVE WS-TEMP-DT          TO BCRACPO.
01492
01493      IF BVOIDACL GREATER ZERO
01494          IF BVOIDACI = SPACES
01495              MOVE LOW-VALUES     TO  PI-AFTER-DATE-3
01496              MOVE +1             TO  WS-UPDATE-SW
01497              MOVE AL-UNNON       TO  BVOIDACA
01498          ELSE
01499              MOVE BVOIDACI       TO  WS-DEEDIT-FIELD
01500              PERFORM 8600-DEEDIT
01501              MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY
01502              MOVE '4'            TO  DC-OPTION-CODE
01503              PERFORM 8500-DATE-CONVERSION
01504              IF DC-ERROR-CODE NOT = SPACE
01505                  MOVE ER-0021    TO  EMI-ERROR
01506                  PERFORM 9900-ERROR-FORMAT
01507                  MOVE -1         TO  BVOIDACL
01508                  MOVE AL-UNBON   TO  BVOIDACA
01509              ELSE
01510                  MOVE +1                  TO WS-UPDATE-SW
01511                  MOVE DC-BIN-DATE-1       TO PI-AFTER-DATE-3
01512                  MOVE AL-UNNON            TO BVOIDACA
01513                  MOVE WS-DEEDIT-FIELD-V0  TO WS-TEMP-DT
01514                  INSPECT WS-TEMP-DT CONVERTING SPACES TO SLASH
01515                  MOVE WS-TEMP-DT          TO BVOIDACO.
01516
01517      IF BHOLDATL GREATER ZERO
01518          IF BHOLDATI = SPACES
01519              MOVE LOW-VALUES     TO  PI-HOLD-UNTIL-DATE
01520              MOVE +1             TO  WS-UPDATE-SW
01521              MOVE AL-UNNON       TO  BHOLDATA
01522          ELSE
01523              MOVE BHOLDATI       TO  WS-DEEDIT-FIELD
01524              PERFORM 8600-DEEDIT
01525              MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY
01526              MOVE '4'            TO  DC-OPTION-CODE
01527              PERFORM 8500-DATE-CONVERSION
01528              IF DC-ERROR-CODE NOT = SPACE
01529                  MOVE ER-0021    TO  EMI-ERROR
01530                  PERFORM 9900-ERROR-FORMAT
01531                  MOVE -1         TO  BHOLDATL
01532                  MOVE AL-UNBON   TO  BHOLDATA
01533              ELSE
01534                  MOVE +1                  TO WS-UPDATE-SW
01535                  MOVE DC-BIN-DATE-1       TO PI-HOLD-UNTIL-DATE
01536                  MOVE AL-UNNON            TO BHOLDATA
01537                  MOVE WS-DEEDIT-FIELD-V0  TO WS-TEMP-DT
01538                  INSPECT WS-TEMP-DT CONVERTING SPACES TO SLASH
01539                  MOVE WS-TEMP-DT          TO BHOLDATO.
01540
01541      IF BCKQUEL GREATER ZERO
01542          
      * EXEC CICS BIF DEEDIT
01543 *            FIELD   (BCKQUEI)
01544 *            LENGTH  (8)
01545 *        END-EXEC
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00010025' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303130303235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BCKQUEI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01546          IF BCKQUEI NOT NUMERIC
01547              MOVE ER-0570        TO  EMI-ERROR
01548              PERFORM 9900-ERROR-FORMAT
01549              MOVE -1             TO  BCKQUEL
01550              MOVE AL-UNBON       TO  BCKQUEA
01551          ELSE
01552              MOVE AL-UNNON       TO  BCKQUEA
01553              MOVE BCKQUEI        TO  WS-CHECK-QUE  BCKQUEO
01554              MOVE +1             TO  WS-UPDATE-SW.
01555
01556      IF BCKSEQL GREATER ZERO
01557          
      * EXEC CICS BIF DEEDIT
01558 *            FIELD   (BCKSEQI)
01559 *            LENGTH  (4)
01560 *        END-EXEC
           MOVE 4
             TO DFHEIV11
      *    MOVE '@"L                   #   #00010040' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303130303430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BCKSEQI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01561          IF BCKSEQI NOT NUMERIC
01562              MOVE ER-0571        TO  EMI-ERROR
01563              PERFORM 9900-ERROR-FORMAT
01564              MOVE -1             TO  BCKSEQL
01565              MOVE AL-UNBON       TO  BCKSEQA
01566          ELSE
01567              MOVE AL-UNNON       TO  BCKSEQA
01568              MOVE BCKSEQI        TO  WS-CHECK-QUE-SEQ  BCKSEQO
01569              MOVE +1             TO  WS-UPDATE-SW.
01570
01571      IF (PI-COMPANY-ID = 'RMC' OR 'LAP') OR
01572         (PI-PROCESSOR-ID = 'LGXX')
01573          IF BPMTORGL IS GREATER THAN +0
01574              IF BPMTORGI = '1' OR '2' OR '3'
01575                  MOVE AL-UANON   TO  BPMTORGA
01576                  MOVE +1         TO  WS-UPDATE-SW
01577              ELSE
01578                  MOVE ER-0830    TO  EMI-ERROR
01579                  PERFORM 9900-ERROR-FORMAT
01580                  MOVE AL-UANON   TO  BPMTORGA
01581                  MOVE -1         TO  BPMTORGL.
01582
01583      IF WS-ERROR-COUNT GREATER ZERO
01584          PERFORM 8200-SEND-DATAONLY.
01585
01586      IF WS-UPDATE-SW NOT GREATER ZERO
01587          IF (BNOTE1L IS GREATER THAN 0 OR
01588              BNOTE2L IS GREATER THAN 0)
01589                  GO TO 0200-UPDATE-PMNT-NOTE-TRLR
01590               ELSE
01591                  PERFORM 4000-READ-TRAILER-FILE.
01592
01593      PERFORM 3000-READ-FOR-UPDATE.
01594
01595      IF BPAYEEL GREATER ZERO
01596          IF BPAYEEI EQUAL 'I'
01597              MOVE 'I1'           TO  AT-PAYEE-TYPE-CD
01598          ELSE
01599          IF BPAYEEI EQUAL 'B'
01600              MOVE 'B1'           TO  AT-PAYEE-TYPE-CD
01601          ELSE
01602          IF BPAYEEI EQUAL 'A'
01603              MOVE 'A1'           TO  AT-PAYEE-TYPE-CD
01604          ELSE
01605          IF BPAYEEI EQUAL 'O'
01606              MOVE 'O1'           TO  AT-PAYEE-TYPE-CD
01607          ELSE
01608          IF BPAYEEI EQUAL 'Q' OR 'REM BORR'
01609              MOVE 'Q1'           TO  AT-PAYEE-TYPE-CD
01610            ELSE
01611          IF BPAYEEI = 'P' OR 'D'
01612              MOVE 'P1'           TO  AT-PAYEE-TYPE-CD
01613            ELSE
01614          IF BPAYEEI EQUAL 'E'
01615              MOVE 'E1'           TO  AT-PAYEE-TYPE-CD.
052506
052506     IF BPRFDTL GREATER ZERO
052506         MOVE WS-PRF-DT          TO  AT-PMT-PROOF-DT.
01616
01617      IF BCKNOL GREATER ZERO
01618          MOVE BCKNOI             TO  AT-CHECK-NO.
01619
01620      IF BCRSELL GREATER ZERO
01621         MOVE WS-CRSEL            TO  AT-PMT-SELECT-DT.
01622
01623      IF BVOIDSDL GREATER ZERO
01624         MOVE WS-VOIDSD           TO  AT-VOID-SELECT-DT.
01625
01626      IF BCRACPL GREATER ZERO
01627         MOVE PI-AFTER-DATE-2     TO  AT-PMT-ACCEPT-DT.
01628
           IF BEOBYNL > 0
              MOVE BEOBYNI             TO AT-PRINT-EOB-WITH-CHECK
           END-IF
020413     IF BCLMYNL > 0
020413        MOVE BCLMYNI             TO AT-PRINT-CLM-FORM
020413     END-IF
020413
020413     IF BSRVYNL > 0
020413        MOVE BSRVYNI             TO AT-PRINT-SURVEY
020413     END-IF
020413
102413     IF BSPRELL > 0
102413        MOVE BSPRELI             TO AT-SPECIAL-RELEASE
102413     END-IF
102413
01629      IF BVOIDACL GREATER ZERO
01630         MOVE PI-AFTER-DATE-3     TO  AT-VOID-ACCEPT-DT.
01631
01632      IF BEXPTYPL GREATER ZERO
01633          MOVE BEXPTYPI           TO  AT-EXPENSE-TYPE.
01634
01635      IF BHOLDATL GREATER ZERO
01636          MOVE PI-HOLD-UNTIL-DATE TO  AT-TO-BE-WRITTEN-DT.
01637
01638      IF BCKQUEL GREATER ZERO
01639          MOVE WS-CHECK-QUE       TO  AT-CHECK-QUE-CONTROL.
01640
01641      IF BCKSEQL GREATER ZERO
01642          MOVE WS-CHECK-QUE-SEQ   TO  AT-CHECK-QUE-SEQUENCE.
01643
01644      IF BPMTORGL GREATER ZERO
01645          MOVE BPMTORGI           TO  AT-PAYMENT-ORIGIN.
01646
01647      MOVE PI-PROCESSOR-ID        TO  AT-PAYMENT-LAST-UPDATED-BY.
01648
01649      MOVE WS-CURRENT-DATE        TO  AT-PAYMENT-LAST-MAINT-DT.
01650
01651      PERFORM 3100-REWRITE.
01652      MOVE +1                     TO  WS-COMPLETED-SUCCESSFUL.
01653
01654      MOVE 'S'                    TO  BMAINTO.
01655      MOVE -1                     TO  BMAINTL.
01656      MOVE AL-UANOF               TO  BMAINTA.
01657
01658      IF BNOTE1L IS GREATER THAN 0 OR
01659         BNOTE2L IS GREATER THAN 0
01660              GO TO 0200-UPDATE-PMNT-NOTE-TRLR
01661           ELSE
01662              PERFORM 8200-SEND-DATAONLY.
01663
01664  0200-UPDATE-PMNT-NOTE-TRLR.
01665
01666      
      * EXEC CICS READ
01667 *        DATASET   (WS-ACTIVITY-TRAILERS-DSID)
01668 *        RIDFLD    (PI-SAVE-KEY)
01669 *        SET       (ADDRESS OF ACTIVITY-TRAILERS)
01670 *    END-EXEC.
      *    MOVE '&"S        E          (   #00010167' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303130313637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-SAVE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01671
01672      IF AT-PAYMENT-NOTE-SEQ-NO = 0
01673          GO TO 0200-ADD-PMNT-NOTE-TRLR.
01674
01675      MOVE PI-SAVE-KEY            TO  WS-ACTIVITY-TRAILERS-KEY.
01676      MOVE AT-PAYMENT-NOTE-SEQ-NO TO  WS-ATK-SEQUENCE-NO.
01677
01678      
      * EXEC CICS HANDLE CONDITION
01679 *        NOTFND   (0200-ADD-PMNT-NOTE-TRLR)
01680 *    END-EXEC.
      *    MOVE '"$I                   ! # #00010179' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303130313739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01681
01682      
      * EXEC CICS READ
01683 *        DATASET   (WS-ACTIVITY-TRAILERS-DSID)
01684 *        RIDFLD    (WS-ACTIVITY-TRAILERS-KEY)
01685 *        SET       (ADDRESS OF ACTIVITY-TRAILERS)
01686 *        UPDATE
01687 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00010183' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303130313833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ACTIVITY-TRAILERS-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01688
01689      IF BNOTE1L IS GREATER THAN 0
01690          MOVE BNOTE1I            TO  AT-INFO-LINE-1.
01691
01692      IF PI-COMPANY-ID = 'HAN'  OR  'JHL'
01693        IF BNOTE1L IS GREATER THAN 0
01694          MOVE BNOTE1I            TO  WS-HAN-PAYMENT-NOTE
01695          IF WS-HAN-PMT-CODE = SPACE
01696              MOVE SPACES         TO  WS-HAN-PMT-TEXT
01697          ELSE
01698          IF WS-HAN-PMT-CODE = 'A'
01699              MOVE ':FINAL PAYMENT / MAXIMUM BENEFIT-PAID'
01700                                  TO  WS-HAN-PMT-TEXT
01701          ELSE
01702          IF WS-HAN-PMT-CODE = 'B'
01703              MOVE ':FINAL PAYMENT / DECEASED'
01704                                  TO  WS-HAN-PMT-TEXT
01705          ELSE
01706          IF WS-HAN-PMT-CODE = 'C'
01707              MOVE ':FINAL PAYMENT / NO LONGER DISABLED'
01708                                  TO  WS-HAN-PMT-TEXT
01709          ELSE
01710          IF WS-HAN-PMT-CODE = 'D'
01711              MOVE ':FINAL PAYMENT / ADDL INFO NOT PROVIDED'
01712                                  TO  WS-HAN-PMT-TEXT
01713          ELSE
01714          IF WS-HAN-PMT-CODE = 'E'
01715              MOVE ':FINAL PAYMENT / RETURNED TO WORK'
01716                                  TO  WS-HAN-PMT-TEXT
01717          ELSE
01718          IF WS-HAN-PMT-CODE = 'F'
01719              MOVE ':FINAL PAYMENT / PAID TO MATURITY DATE'
01720                                  TO  WS-HAN-PMT-TEXT
01721          ELSE
01722          IF WS-HAN-PMT-CODE = 'P'
01723              MOVE ':PARTIAL PAYMENT' TO  WS-HAN-PMT-TEXT.
01724
01725      IF PI-COMPANY-ID = 'HAN'  OR  'JHL'
01726        IF BNOTE1L IS GREATER THAN 0
01727          MOVE WS-HAN-PAYMENT-NOTE  TO  BNOTE1I
01728                                        AT-INFO-LINE-1.
01729
01730      IF BNOTE2L IS GREATER THAN 0
01731          MOVE BNOTE2I            TO  AT-INFO-LINE-2.
01732
01733      MOVE PI-PROCESSOR-ID        TO  AT-PAYMENT-LAST-UPDATED-BY.
01734
01735      MOVE WS-CURRENT-DATE        TO  AT-PAYMENT-LAST-MAINT-DT.
01736
01737      PERFORM 3100-REWRITE.
01738
01739      MOVE 'S'                    TO  BMAINTO.
01740      MOVE -1                     TO  BMAINTL.
01741      MOVE AL-UANOF               TO  BMAINTA.
01742
01743      PERFORM 8200-SEND-DATAONLY.
01744
01745  0200-ADD-PMNT-NOTE-TRLR.
01746
01747      PERFORM 3500-READ-ELMSTR-FOR-UPDATE.
01748
01749      PERFORM 3000-READ-FOR-UPDATE.
01750
01751      MOVE CL-TRAILER-SEQ-CNT     TO  AT-PAYMENT-NOTE-SEQ-NO.
01752      SUBTRACT +1 FROM AT-PAYMENT-NOTE-SEQ-NO.
01753
01754      
      * EXEC CICS REWRITE
01755 *        DATASET   (WS-ACTIVITY-TRAILERS-DSID)
01756 *        FROM      (ACTIVITY-TRAILERS)
01757 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00010255' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303130323535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01758
01759      
      * EXEC CICS GETMAIN
01760 *        LENGTH  (WS-ACTIVITY-TRAILERS-LENGTH)
01761 *        INITIMG (WS-SPACES)
01762 *        SET     (ADDRESS OF ACTIVITY-TRAILERS)
01763 *    END-EXEC.
      *    MOVE ',"IL                  $   #00010260' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303130323630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 WS-ACTIVITY-TRAILERS-LENGTH, 
                 WS-SPACES
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01764
01765      MOVE 'AT'                   TO  AT-RECORD-ID.
01766      MOVE PI-SAVE-KEY            TO  AT-CONTROL-PRIMARY.
01767      MOVE CL-TRAILER-SEQ-CNT     TO  AT-SEQUENCE-NO.
01768      SUBTRACT +1 FROM AT-SEQUENCE-NO.
01769
01770      MOVE '6'                    TO  AT-TRAILER-TYPE.
01771
01772      MOVE WS-CURRENT-DATE        TO  AT-RECORDED-DT
01773                                      AT-PAYMENT-LAST-MAINT-DT.
01774      MOVE PI-PROCESSOR-ID        TO  AT-RECORDED-BY
01775                                      AT-PAYMENT-LAST-UPDATED-BY.
01776      MOVE EIBTIME                TO  AT-LAST-MAINT-HHMMSS.
01777
01778      MOVE SPACES                 TO  WS-HAN-PAYMENT-NOTE.
01779
01780      IF BNOTE1L IS GREATER THAN 0
01781          MOVE BNOTE1I            TO  AT-INFO-LINE-1
01782                                      WS-HAN-PAYMENT-NOTE.
01783
01784      IF PI-COMPANY-ID = 'HAN'  OR  'JHL'
01785          IF WS-HAN-PMT-CODE = 'A'
01786              MOVE ':FINAL PAYMENT / MAXIMUM BENEFIT-PAID'
01787                                  TO  WS-HAN-PMT-TEXT
01788          ELSE
01789          IF WS-HAN-PMT-CODE = 'B'
01790              MOVE ':FINAL PAYMENT / DECEASED'
01791                                  TO  WS-HAN-PMT-TEXT
01792          ELSE
01793          IF WS-HAN-PMT-CODE = 'C'
01794              MOVE ':FINAL PAYMENT / NO LONGER DISABLED'
01795                                  TO  WS-HAN-PMT-TEXT
01796          ELSE
01797          IF WS-HAN-PMT-CODE = 'D'
01798              MOVE ':FINAL PAYMENT / ADDL INFO NOT PROVIDED'
01799                                  TO  WS-HAN-PMT-TEXT
01800          ELSE
01801          IF WS-HAN-PMT-CODE = 'E'
01802              MOVE ':FINAL PAYMENT / RETURNED TO WORK'
01803                                  TO  WS-HAN-PMT-TEXT
01804          ELSE
01805          IF WS-HAN-PMT-CODE = 'F'
01806              MOVE ':FINAL PAYMENT / PAID TO MATURITY DATE'
01807                                  TO  WS-HAN-PMT-TEXT
01808          ELSE
01809          IF WS-HAN-PMT-CODE = 'P'
01810              MOVE ':PARTIAL PAYMENT' TO  WS-HAN-PMT-TEXT.
01811
01812      IF PI-COMPANY-ID = 'HAN'  OR  'JHL'
01813          MOVE WS-HAN-PAYMENT-NOTE  TO  BNOTE1I
01814                                        AT-INFO-LINE-1.
01815
01816      IF BNOTE2L IS GREATER THAN 0
01817          MOVE BNOTE2I            TO  AT-INFO-LINE-2.
01818
01819      MOVE 'P'                    TO  AT-INFO-TRAILER-TYPE.
01820
01821      
      * EXEC CICS WRITE
01822 *        DATASET   (WS-ACTIVITY-TRAILERS-DSID)
01823 *        FROM      (ACTIVITY-TRAILERS)
01824 *        RIDFLD    (AT-CONTROL-PRIMARY)
01825 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00010322' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303130333232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01826
01827      SUBTRACT +1 FROM  CL-TRAILER-SEQ-CNT.
01828
01829      PERFORM 3600-REWRITE-ELMSTR.
01830
01831      MOVE 'S'                    TO  BMAINTO.
01832      MOVE -1                     TO  BMAINTL.
01833      MOVE AL-UANOF               TO  BMAINTA.
01834
01835      PERFORM 8200-SEND-DATAONLY.
01836
01837      EJECT
01838  0300-MAIN-LOGIC.
01839      IF PI-MAP-NAME NOT = EL142C
01840          GO TO 0400-MAIN-LOGIC.
01841
01842      PERFORM 4000-READ-TRAILER-FILE.
01843
01844      EJECT
01845  0400-MAIN-LOGIC.
01846      IF PI-MAP-NAME NOT = EL142D
01847          GO TO 0500-MAIN-LOGIC.
01848
01849      IF NOT MODIFY-CAP
01850          IF DMAINTI = 'S'
01851              NEXT SENTENCE
01852          ELSE
01853              MOVE 'UPDATE'       TO SM-READ
01854              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
01855              MOVE ER-0070        TO  EMI-ERROR
01856              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01857              GO TO 8100-SEND-INITIAL-MAP.
01858
01859      IF EIBAID = DFHPF6
102510         PERFORM 6000-DISPLAY-ELNAPS.
01861
01862      MOVE PI-COMPANY-ID          TO CNTL-CO.
01863      MOVE '1'                    TO CNTL-RECORD-TYPE.
01864      MOVE SPACES                 TO CNTL-GENL.
01865      MOVE ZEROS                  TO CNTL-SEQ.
01866
01867      
      * EXEC CICS READ
01868 *        DATASET(WS-CONTROL-FILE-DSID)
01869 *        SET    (ADDRESS OF CONTROL-FILE)
01870 *        RIDFLD (WS-CONTROL-FILE-KEY)
01871 *    END-EXEC.
      *    MOVE '&"S        E          (   #00010368' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303130333638' TO DFHEIV0(25:11)
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
           
01872
01873      MOVE +2                     TO  EMI-NUMBER-OF-LINES
01874                                      EMI-SWITCH2.
01875
01876      IF DMAINTL NOT GREATER ZERO
01877        OR (DMAINTL GREATER ZERO AND
01878            DMAINTI = 'S')
01879              PERFORM 4000-READ-TRAILER-FILE.
01880
01881      IF DMAINTI NOT = 'C'
01882          MOVE ER-0023            TO  EMI-ERROR
01883          MOVE -1                 TO  DMAINTL
01884          MOVE AL-UABON           TO  DMAINTA
01885          PERFORM 8200-SEND-DATAONLY.
01886
020816     if pi-company-id = 'CID' or 'DCC' OR 'AHL' OR 'VPP'
062121           OR 'FNL'
062602        if (pi-el142-priority = '8')
120718           AND (PI-approval-level <> '5')
062602           MOVE ER-8003          TO EMI-ERROR
062602           MOVE -1               TO dMAINTL
062602           MOVE AL-UABON         TO dMAINTA
062602           GO TO 8200-SEND-DATAONLY.
062602
01887      MOVE AL-UANON               TO  DMAINTA
01888
01889 *    NOTE *******************************************************
01890 *         *                                                     *
01891 *         *      THE FOLLOWING CHECK ALLOWS THE LGXX SIGNON     *
01892 *         *  TO UPDATE THE ARCHIVE NUMBER, THE DATE SENT FIELD, *
01893 *         *  THE INITIAL PRINT DATE FIELD, AND THE RESEND PRINT *
01894 *         *  DATE FIELD.  IF NOT LGXX SIGNON, THE FIELDS ARE    *
01895 *         *  SET TO ASKIP.                                      *
01896 *         *                                                     *
01897 *         *******************************************************.
01898
01899      IF PI-PROCESSOR-ID = 'LGXX'
01900          PERFORM 0450-MAIN-LOGIC
01901      ELSE
01902          MOVE AL-SANOF           TO  DARCHNOA
050110                                     DRESFRMA
050110                                     DAUTOCLA
01903                                      DDTSENTA
01904                                      DINPRNTA
01905                                      DREPRNTA.
01906
01907      IF DFORMNOL GREATER ZERO
01908         MOVE DFORMNOI            TO WS-FORM-NUMBER
01909         MOVE +1                  TO WS-UPDATE-SW.
01910
01911      IF DRESENDL GREATER ZERO
01912          IF DRESENDI = SPACES
01913              MOVE AL-UANON       TO  DRESENDA
01914              MOVE +1             TO  WS-UPDATE-SW
01915              MOVE LOW-VALUES     TO  WS-RESEND-DATE
01916          ELSE
01917              MOVE DRESENDI       TO  WS-DEEDIT-FIELD
01918              PERFORM 8600-DEEDIT
01919              IF WS-DEEDIT-FIELD-V0 IS NUMERIC
01920                  MOVE WS-DEEDIT-FIELD-V0  TO  DRESENDO
01921                  INSPECT DRESENDI CONVERTING SPACES TO SLASH
01922                  MOVE '4'        TO  DC-OPTION-CODE
01923                  MOVE WS-DEEDIT-FIELD-V0  TO  DC-GREG-DATE-1-MDY
01924                  PERFORM 8500-DATE-CONVERSION
01925                  IF DC-ERROR-CODE NOT = SPACES
01926                      MOVE ER-0295         TO  EMI-ERROR
01927                      MOVE -1              TO  DRESENDL
01928                      MOVE AL-UABON        TO  DRESENDA
01929                      PERFORM 9900-ERROR-FORMAT
01930                  ELSE
01931                      MOVE AL-UANON       TO  DRESENDA
01932                      MOVE +1             TO  WS-UPDATE-SW
01933                      MOVE DC-BIN-DATE-1  TO  WS-RESEND-DATE
01934              ELSE
01935                  MOVE ER-0295    TO  EMI-ERROR
01936                  MOVE -1         TO  DRESENDL
01937                  MOVE AL-UABON   TO  DRESENDA
01938                  PERFORM 9900-ERROR-FORMAT.
01939
01940      IF DREPLYL GREATER ZERO
01941          IF DREPLYI = SPACES
01942              MOVE AL-UANON       TO  DREPLYA
01943              MOVE +1             TO  WS-UPDATE-SW
01944              MOVE LOW-VALUES     TO  WS-FOLLOW-UP-DATE
01945          ELSE
01946              MOVE DREPLYI        TO  WS-DEEDIT-FIELD
01947              PERFORM 8600-DEEDIT
01948              IF WS-DEEDIT-FIELD-V0 IS NUMERIC
01949                  MOVE WS-DEEDIT-FIELD-V0  TO  DREPLYO
01950                  INSPECT DREPLYI CONVERTING SPACES TO SLASH
01951                  MOVE '4'                 TO  DC-OPTION-CODE
01952                  MOVE WS-DEEDIT-FIELD-V0  TO  DC-GREG-DATE-1-MDY
01953                  PERFORM 8500-DATE-CONVERSION
01954                  IF DC-ERROR-CODE NOT = SPACES
01955                      MOVE ER-0296           TO  EMI-ERROR
01956                      MOVE -1             TO  DREPLYL
01957                      MOVE AL-UABON       TO  DREPLYA
01958                      PERFORM 9900-ERROR-FORMAT
01959                  ELSE
01960                      MOVE AL-UANON       TO  DREPLYA
01961                      MOVE +1             TO  WS-UPDATE-SW
01962                      MOVE DC-BIN-DATE-1  TO  WS-FOLLOW-UP-DATE
01963              ELSE
01964                  MOVE ER-0296    TO  EMI-ERROR
01965                  MOVE -1         TO  DREPLYL
01966                  MOVE AL-UABON   TO  DREPLYA
01967                  PERFORM 9900-ERROR-FORMAT.
01968
01969      IF DRECEVEL GREATER ZERO
01970          IF DRECEVEI = SPACES
01971              MOVE AL-UANON       TO  DRECEVEA
01972              MOVE +1             TO  WS-UPDATE-SW
01973              MOVE LOW-VALUES     TO  WS-RECEIVED-DATE
01974          ELSE
01975              MOVE DRECEVEI       TO  WS-DEEDIT-FIELD
01976              PERFORM 8600-DEEDIT
01977              IF WS-DEEDIT-FIELD-V0 IS NUMERIC
01978                  MOVE WS-DEEDIT-FIELD-V0  TO  DRECEVEO
01979                  INSPECT DRECEVEI CONVERTING SPACES TO SLASH
01980                  MOVE '4'        TO  DC-OPTION-CODE
01981                  MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY
01982                  PERFORM 8500-DATE-CONVERSION
01983                  IF DC-ERROR-CODE NOT = SPACES
01984                      MOVE ER-0297        TO  EMI-ERROR
01985                      MOVE -1             TO  DRECEVEL
01986                      MOVE AL-UABON       TO  DRECEVEA
01987                      PERFORM 9900-ERROR-FORMAT
01988                  ELSE
01989                      MOVE AL-UANON       TO  DRECEVEA
01990                      MOVE +1             TO  WS-UPDATE-SW
01991                      MOVE DC-BIN-DATE-1  TO  WS-RECEIVED-DATE
01992              ELSE
01993                  MOVE ER-0297    TO  EMI-ERROR
01994                  MOVE -1         TO  DRECEVEL
01995                  MOVE AL-UABON   TO  DRECEVEA
01996                  PERFORM 9900-ERROR-FORMAT.
102610
102610     IF DSTOPLTL GREATER ZERO
102610         IF DSTOPLTI = SPACES
102610             MOVE AL-UANON       TO  DSTOPLTA
102610             MOVE +1             TO  WS-UPDATE-SW
102610             MOVE LOW-VALUES     TO  WS-STOP-LETTER-DATE
102610         ELSE
102610             MOVE DSTOPLTI       TO  WS-DEEDIT-FIELD
102610             PERFORM 8600-DEEDIT
102610             IF WS-DEEDIT-FIELD-V0 IS NUMERIC
102610                 MOVE WS-DEEDIT-FIELD-V0  TO  DSTOPLTO
102610                 INSPECT DSTOPLTI CONVERTING SPACES TO SLASH
102610                 MOVE '4'        TO  DC-OPTION-CODE
102610                 MOVE WS-DEEDIT-FIELD-V0  TO  DC-GREG-DATE-1-MDY
102610                 PERFORM 8500-DATE-CONVERSION
102610                 IF DC-ERROR-CODE NOT = SPACES
102610                     MOVE ER-0897         TO  EMI-ERROR
102610                     MOVE -1              TO  DSTOPLTL
102610                     MOVE AL-UABON        TO  DSTOPLTA
102610                     PERFORM 9900-ERROR-FORMAT
102610                 ELSE
102610                     MOVE AL-UANON       TO  DSTOPLTA
102610                     MOVE +1             TO  WS-UPDATE-SW
102610                     MOVE DC-BIN-DATE-1  TO  WS-STOP-LETTER-DATE
102610             ELSE
102610                 MOVE ER-0897    TO  EMI-ERROR
102610                 MOVE -1         TO  DSTOPLTL
102610                 MOVE AL-UABON   TO  DSTOPLTA
102610                 PERFORM 9900-ERROR-FORMAT.
01997
01998      IF DREASONL GREATER ZERO
01999          MOVE +1                 TO  WS-UPDATE-SW.
041613
041613     IF DENCCODL GREATER THAN ZERO
041613        IF PI-CREATED-IN-NAPERSOFT = 'Y'
041613            MOVE DENCCODI TO WS-TEMP-ENCCODE
041613            IF WS-TEMP-ENCCODE (3:1) = 'X'
102413                IF WS-TEMP-ENCCODE (1:2) = 'EN'
102413                   MOVE 'V' TO WS-TEMP-ENCCODE (3:1)
102413                ELSE
102413                   MOVE SPACES TO WS-TEMP-ENCCODE (3:1)
102413                END-IF
041613            END-IF
041613            IF WS-TEMP-ENCCODE (2:1) = 'X'
041613                MOVE SPACES TO WS-TEMP-ENCCODE (2:1)
041613            END-IF
041613            IF WS-TEMP-ENCCODE <> PI-ENC-CODE
041613               MOVE ER-1568          TO EMI-ERROR
041613               MOVE -1               TO DENCCODL
041613               MOVE AL-UABON         TO DENCCODA
041613               PERFORM 9900-ERROR-FORMAT
041613            END-IF
041613        END-IF
041613        MOVE SPACES             TO WS-ELENCC-KEY
041613        MOVE PI-COMPANY-CD      TO WS-ELENCC-COMPANY-CD
041613        MOVE '1'                TO WS-ELENCC-REC-TYPE
041613        MOVE DENCCODI           TO WS-ELENCC-ENC-CODE
041613
041613        
      * EXEC CICS READ
041613*           DATASET    (WS-ELENCC-FILE-DSID)
041613*           SET        (ADDRESS OF ENCLOSURE-CODES)
041613*           RIDFLD     (WS-ELENCC-KEY)
041613*           RESP       (WS-RESPONSE)
041613*       END-EXEC
      *    MOVE '&"S        E          (  N#00010567' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303130353637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ELENCC-FILE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ELENCC-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ENCLOSURE-CODES TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
041613
041613        IF RESP-NORMAL
041613           MOVE DENCCODI         TO WS-ENCLOSURE-CODE
041613           MOVE AL-UANON         TO DENCCODA
041613           MOVE +1               TO WS-UPDATE-SW
041613        ELSE
041613           MOVE ER-1560          TO EMI-ERROR
041613           MOVE -1               TO DENCCODL
041613           MOVE AL-UABON         TO DENCCODA
041613           PERFORM 9900-ERROR-FORMAT
041613        END-IF
041613     END-IF
041613
02000
02001      IF WS-ERROR-COUNT GREATER ZERO
02002          PERFORM 8200-SEND-DATAONLY.
02003
02004      IF WS-UPDATE-SW NOT GREATER ZERO
02005          PERFORM 4000-READ-TRAILER-FILE.
02006
02007      IF DREPLYL GREATER ZERO
02008          PERFORM 3300-UPDATE-CLAIM-MASTER.
02009
02010      PERFORM 3000-READ-FOR-UPDATE.
02011
02012      IF DRESENDL GREATER ZERO AND
02013         AT-LETTER-ARCHIVE-NO LESS THAN CF-STARTING-ARCH-NO
02014           
      * EXEC CICS READ
02015 *             UPDATE
02016 *             DATASET (WS-CONTROL-FILE-DSID)
02017 *             SET     (ADDRESS OF CONTROL-FILE)
02018 *             RIDFLD  (WS-CONTROL-FILE-KEY)
02019 *         END-EXEC
      *    MOVE '&"S        EU         (   #00010600' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303130363030' TO DFHEIV0(25:11)
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02020           MOVE AT-LETTER-ARCHIVE-NO TO CF-STARTING-ARCH-NO
02021           
      * EXEC CICS REWRITE
02022 *             DATASET (WS-CONTROL-FILE-DSID)
02023 *             FROM    (CONTROL-FILE)
02024 *         END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00010607' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303130363037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02025
02026      IF DRESENDL GREATER ZERO
02027          MOVE WS-RESEND-DATE     TO  AT-AUTO-RE-SEND-DT
102510     END-IF.
102510*02028          MOVE LOW-VALUES         TO  AT-RESEND-PRINT-DATE
102510*02029          MOVE PI-COMPANY-CD      TO  WS-LA-COMPANY-CD
102510*02030          MOVE AT-LETTER-ARCHIVE-NO  TO  WS-LA-ARCHIVE-NO
102510*02031          MOVE '1'                TO  WS-LA-RECORD-TYPE
102510*02032          MOVE ZERO               TO  WS-LA-LINE-SEQ-NO
102510*02033          EXEC CICS READ UPDATE
102510*02034              DATASET (WS-LETTER-ARCHIVE-DSID)
102510*02035              RIDFLD  (WS-LETTER-ARCHIVE-KEY)
102510*02036              SET     (ADDRESS OF LETTER-ARCHIVE)
102510*02037          END-EXEC
102510*02038          MOVE WS-RESEND-DATE     TO  LA-RESEND-DATE
102510*02039          MOVE LOW-VALUES         TO  LA-RESEND-PRINT-DATE
102510*02040          EXEC CICS REWRITE
102510*02041              DATASET (WS-LETTER-ARCHIVE-DSID)
102510*02042              FROM    (LETTER-ARCHIVE)
102510*02043          END-EXEC
102510*02044          IF PI-COMPANY-ID = 'DMD'
102510*02045              EXEC CICS READ UPDATE
102510*02046                  DATASET (WS-ELARCT-FILE-ID)
102510*02047                  RIDFLD  (WS-LETTER-ARCHIVE-KEY)
102510*02048                  SET     (ADDRESS OF LETTER-ARCHIVE-TEMP)
102510*02049              END-EXEC
102510*02050              MOVE WS-RESEND-DATE     TO  LT-RESEND-DATE
102510*02051              MOVE LOW-VALUES         TO  LT-RESEND-PRINT-DA
102510*02052              EXEC CICS REWRITE
102510*02053                  DATASET (WS-ELARCT-FILE-ID)
102510*02054                  FROM    (LETTER-ARCHIVE-TEMP)
102510*02055              END-EXEC.
02056
02057      IF DREPLYL GREATER ZERO
02058          MOVE WS-FOLLOW-UP-DATE  TO  AT-RECEIPT-FOLLOW-UP.
102510
102510     IF DRESENDL GREATER ZERO OR DREPLYL GREATER ZERO
041613        OR DENCCODL GREATER ZERO
102510         MOVE PI-COMPANY-CD      TO  WS-NA-COMPANY-CD
102510         MOVE PI-CARRIER         TO  WS-NA-CARRIER
102510         MOVE PI-CLAIM-NO        TO  WS-NA-CLAIM-NO
102510         MOVE PI-CERT-NO         TO  WS-NA-CERT-NO
102510         MOVE AT-LETTER-ARCHIVE-NO  TO  WS-NA-ARCHIVE-NO
102510         
      * EXEC CICS READ UPDATE
102510*            DATASET (WS-NAPERSOFT-DSID)
102510*            RIDFLD  (WS-NAPERSOFT-KEY)
102510*            SET     (ADDRESS OF NAPERSOFT-FILE)
102510*        END-EXEC
      *    MOVE '&"S        EU         (   #00010654' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303130363534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-NAPERSOFT-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-NAPERSOFT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF NAPERSOFT-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
102510         IF DRESENDL GREATER ZERO
102510             MOVE WS-RESEND-DATE    TO  NA-RESEND-DT
102510         END-IF
102510         IF DREPLYL GREATER ZERO
102510             MOVE WS-FOLLOW-UP-DATE TO  NA-FOLLOW-UP-DT
102510         END-IF
041613         IF DENCCODL GREATER ZERO
041613             MOVE WS-ENCLOSURE-CODE TO  NA-ENCLOSURE-CD
041613         END-IF
102510         
      * EXEC CICS REWRITE
102510*            DATASET (WS-NAPERSOFT-DSID)
102510*            FROM    (NAPERSOFT-FILE)
102510*        END-EXEC
           MOVE LENGTH OF
            NAPERSOFT-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00010668' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303130363638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-NAPERSOFT-DSID, 
                 NAPERSOFT-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
102510     END-IF.
02059
02060      IF DRECEVEL GREATER ZERO
02061          MOVE WS-RECEIVED-DATE   TO  AT-LETTER-ANSWERED-DT.
02062
02063      IF DFORMNOL GREATER ZEROS
02064         MOVE WS-FORM-NUMBER      TO AT-STD-LETTER-FORM.
02065
02066      IF DARCHNOL GREATER ZERO
02067         MOVE DARCHNOI            TO  AT-LETTER-ARCHIVE-NO.
02068
02069      IF DDTSENTL GREATER ZERO
02070         MOVE WS-DATE-SENT        TO  AT-LETTER-SENT-DT.
02071
02072      IF DINPRNTL GREATER ZERO
02073         MOVE WS-IN-PRINT-DATE    TO  AT-INITIAL-PRINT-DATE.
02074
02075      IF DREPRNTL GREATER ZERO
02076         MOVE WS-REPRINTED-DATE   TO  AT-RESEND-PRINT-DATE.
050110
050110     IF DRESFRML GREATER ZERO
050110        MOVE DRESFRMI            TO  AT-RESEND-LETTER-FORM.
050110
050110     IF DAUTOCLL GREATER ZERO
050110        MOVE DAUTOCLI            TO  AT-AUTO-CLOSE-IND.
102610
102610     IF DSTOPLTL GREATER ZERO
102610        MOVE WS-STOP-LETTER-DATE TO  AT-STOP-LETTER-DT
102610     END-IF.
02077
02078      IF DREASONL GREATER ZERO
02079         MOVE DREASONI            TO  AT-REASON-TEXT.
02080
02081      IF PI-COMPANY-ID = 'HAN'  OR  'JHL'
02082          MOVE AT-REASON-TEXT     TO  WS-REASON-TEXT
02083          IF WS-RE-NDX NUMERIC  AND
02084             WS-RE-NDX GREATER THAN ZERO  AND
02085             WS-RE-NDX LESS THAN 24
02086              MOVE HAN-REASON-TEXT (WS-RE-NDX)
02087                                  TO  AT-REASON-TEXT
02088                                      DREASONO.
02089
02090      MOVE PI-PROCESSOR-ID        TO  AT-CORR-LAST-UPDATED-BY.
02091
02092      MOVE WS-CURRENT-DATE        TO  AT-CORR-LAST-MAINT-DT.
02093
02094      PERFORM 3100-REWRITE.
02095      MOVE +1                     TO  WS-COMPLETED-SUCCESSFUL.
02096
02097      MOVE 'S'                    TO  DMAINTO.
02098      MOVE -1                     TO  DMAINTL.
02099      MOVE AL-UANOF               TO  DMAINTA.
02100      PERFORM 8200-SEND-DATAONLY.
02101
02102      EJECT
02103
02104  0450-MAIN-LOGIC.
02105      IF DARCHNOL GREATER ZERO
02106          MOVE DARCHNOI           TO  WS-DEEDIT-FIELD
02107          PERFORM 8600-DEEDIT
02108          IF WS-DEEDIT-FIELD-V0 IS NUMERIC
02109              MOVE WS-DEEDIT-FIELD-V0     TO  DARCHNOO
02110              MOVE AL-UNNON       TO  DARCHNOA
02111              MOVE +1             TO  WS-UPDATE-SW
02112          ELSE
02113              MOVE -1             TO  DARCHNOL
02114              MOVE AL-UNBON       TO  DARCHNOA
02115              MOVE ER-0175        TO  EMI-ERROR
02116              PERFORM 9900-ERROR-FORMAT.
02117
02118      IF DDTSENTL GREATER ZERO
02119          IF DDTSENTI = SPACES
02120              MOVE AL-UANON       TO  DDTSENTA
02121              MOVE +1             TO  WS-UPDATE-SW
02122              MOVE LOW-VALUES     TO  WS-DATE-SENT
02123          ELSE
02124              MOVE DDTSENTI       TO  WS-DEEDIT-FIELD
02125              PERFORM 8600-DEEDIT
02126              IF WS-DEEDIT-FIELD-V0 IS NUMERIC
02127                  MOVE WS-DEEDIT-FIELD-V0    TO  DDTSENTO
02128                  INSPECT DDTSENTI CONVERTING SPACES TO SLASH
02129                  MOVE '4'                   TO  DC-OPTION-CODE
02130                  MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY
02131                  PERFORM 8500-DATE-CONVERSION
02132                  IF DC-ERROR-CODE NOT = SPACES
02133                      MOVE ER-0641           TO  EMI-ERROR
02134                      MOVE -1                TO  DDTSENTL
02135                      MOVE AL-UABON          TO  DDTSENTA
02136                      PERFORM 9900-ERROR-FORMAT
02137                  ELSE
02138                      MOVE AL-UANON       TO  DDTSENTA
02139                      MOVE +1             TO  WS-UPDATE-SW
02140                      MOVE DC-BIN-DATE-1  TO  WS-DATE-SENT
02141              ELSE
02142                  MOVE ER-0641        TO  EMI-ERROR
02143                  MOVE -1             TO  DDTSENTL
02144                  MOVE AL-UABON       TO  DDTSENTA
02145                  PERFORM 9900-ERROR-FORMAT.
02146
02147      IF DINPRNTL GREATER ZERO
02148          IF DINPRNTI = SPACES
02149              MOVE AL-UANON       TO  DINPRNTA
02150              MOVE +1             TO  WS-UPDATE-SW
02151              MOVE LOW-VALUES     TO  WS-IN-PRINT-DATE
02152          ELSE
02153              MOVE DINPRNTI       TO  WS-DEEDIT-FIELD
02154              PERFORM 8600-DEEDIT
02155              IF WS-DEEDIT-FIELD-V0 IS NUMERIC
02156                  MOVE WS-DEEDIT-FIELD-V0  TO  DINPRNTO
02157                  INSPECT DINPRNTI CONVERTING SPACES TO SLASH
02158                  MOVE '4'                 TO  DC-OPTION-CODE
02159                  MOVE WS-DEEDIT-FIELD-V0  TO  DC-GREG-DATE-1-MDY
02160                  PERFORM 8500-DATE-CONVERSION
02161                  IF DC-ERROR-CODE NOT = SPACES
02162                      MOVE ER-0642           TO  EMI-ERROR
02163                      MOVE -1                TO  DINPRNTL
02164                      MOVE AL-UABON          TO  DINPRNTA
02165                      PERFORM 9900-ERROR-FORMAT
02166                  ELSE
02167                      MOVE AL-UANON       TO  DINPRNTA
02168                      MOVE +1             TO  WS-UPDATE-SW
02169                      MOVE DC-BIN-DATE-1  TO  WS-IN-PRINT-DATE
02170              ELSE
02171                  MOVE ER-0642    TO  EMI-ERROR
02172                  MOVE -1         TO  DINPRNTL
02173                  MOVE AL-UABON   TO  DINPRNTA
02174                  PERFORM 9900-ERROR-FORMAT.
02175
02176      IF DREPRNTL GREATER ZERO
02177          IF DREPRNTI = SPACES
02178              MOVE AL-UANON       TO  DREPRNTA
02179              MOVE +1             TO  WS-UPDATE-SW
02180              MOVE LOW-VALUES     TO  WS-REPRINTED-DATE
02181          ELSE
02182              MOVE DREPRNTI       TO  WS-DEEDIT-FIELD
02183              PERFORM 8600-DEEDIT
02184              IF WS-DEEDIT-FIELD-V0 IS NUMERIC
02185                  MOVE WS-DEEDIT-FIELD-V0  TO  DREPRNTO
02186                  INSPECT DREPRNTI CONVERTING SPACES TO SLASH
02187                  MOVE '4'                 TO  DC-OPTION-CODE
02188                  MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY
02189                  PERFORM 8500-DATE-CONVERSION
02190                  IF DC-ERROR-CODE NOT = SPACES
02191                      MOVE ER-0643        TO  EMI-ERROR
02192                      MOVE -1             TO  DREPRNTL
02193                      MOVE AL-UABON       TO  DREPRNTA
02194                      PERFORM 9900-ERROR-FORMAT
02195                  ELSE
02196                      MOVE AL-UANON       TO  DREPRNTA
02197                      MOVE +1             TO  WS-UPDATE-SW
02198                      MOVE DC-BIN-DATE-1  TO  WS-REPRINTED-DATE
02199              ELSE
02200                  MOVE ER-0643       TO  EMI-ERROR
02201                  MOVE -1             TO  DREPRNTL
02202                  MOVE AL-UABON       TO  DREPRNTA
02203                  PERFORM 9900-ERROR-FORMAT.
02204
02205      EJECT
02206  0500-MAIN-LOGIC.
02207      IF PI-MAP-NAME NOT = EL142E
02208          GO TO 0600-MAIN-LOGIC.
02209
02210      IF NOT MODIFY-CAP
02211          IF EMAINTI = 'S'
02212              NEXT SENTENCE
02213          ELSE
02214              MOVE 'UPDATE'       TO SM-READ
02215              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
02216              MOVE ER-0070        TO  EMI-ERROR
02217              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02218              GO TO 8100-SEND-INITIAL-MAP.
02219
02220      MOVE +2                     TO  EMI-NUMBER-OF-LINES
02221                                      EMI-SWITCH2
02222
02223      IF EMAINTL NOT GREATER ZERO
02224        OR (EMAINTL GREATER ZERO AND
02225            EMAINTI = 'S')
02226              PERFORM 4000-READ-TRAILER-FILE.
02227
02228      IF EMAINTI = 'C' OR 'D'
02229          NEXT SENTENCE
02230      ELSE
02231          MOVE ER-0023            TO  EMI-ERROR
02232          MOVE -1                 TO  EMAINTL
02233          MOVE AL-UABON           TO  EMAINTA
02234          PERFORM 8200-SEND-DATAONLY.
02235
020816     if pi-company-id = 'CID' or 'DCC' OR 'AHL' OR 'VPP'
062121           OR 'FNL'
062602        if (pi-el142-priority = '8')
120718           AND (PI-approval-level <> '5')
062602           MOVE ER-8003          TO EMI-ERROR
062602           MOVE -1               TO EMAINTL
062602           MOVE AL-UABON         TO EMAINTA
062602           GO TO 8200-SEND-DATAONLY.
062602
02236      IF PI-COMPANY-ID EQUAL 'DMD'
02237       IF SYSTEM-MODIFY-CAP OR
02238         PI-PROCESSOR-ID = 'LGXX'
02239          NEXT SENTENCE
02240        ELSE
02241          IF (PI-PROCESSOR-ID NOT = PI-SAVE-LAST-UPD-BY)
02242                                   OR
02243             (WS-CURRENT-DATE NOT = PI-SAVE-LAST-MAINT-DT)
02244               MOVE ER-8003        TO EMI-ERROR
02245               MOVE -1             TO EMAINTL
02246               MOVE AL-UABON       TO EMAINTA
02247               GO TO 8200-SEND-DATAONLY.
02248
02249      IF EMAINTI = 'D'
02250          PERFORM 3000-READ-FOR-UPDATE
02251          PERFORM 3200-DELETE
02252          MOVE +1                 TO  WS-COMPLETED-SUCCESSFUL
02253          MOVE -1                 TO  EPFKL
02254          MOVE 'S'                TO  EMAINTO
02255          MOVE AL-SANOF           TO  EMAINTA  ELINE1A  ELINE2A
02256          PERFORM 8200-SEND-DATAONLY.
02257
02258      MOVE AL-UANON               TO  EMAINTA.
02259
02260      IF ELINE1L GREATER ZERO OR
02261         ELINE2L GREATER ZERO
02262           NEXT SENTENCE
02263      ELSE
02264           PERFORM 4000-READ-TRAILER-FILE.
02265
02266      PERFORM 3000-READ-FOR-UPDATE.
02267
02268      IF ELINE1L GREATER ZERO
02269          MOVE ELINE1I            TO  AT-INFO-LINE-1.
02270
02271      IF ELINE2L GREATER ZERO
02272          MOVE ELINE2I            TO  AT-INFO-LINE-2.
02273
02274      MOVE PI-PROCESSOR-ID        TO  AT-GEN-INFO-LAST-UPDATED-BY.
02275
02276      MOVE WS-CURRENT-DATE        TO  AT-GEN-INFO-LAST-MAINT-DT.
02277
02278      PERFORM 3100-REWRITE.
02279      MOVE +1                     TO  WS-COMPLETED-SUCCESSFUL.
02280
02281      MOVE 'S'                    TO  EMAINTO.
02282      MOVE -1                     TO  EMAINTL.
02283      MOVE AL-UANOF               TO  EMAINTA.
02284      PERFORM 8200-SEND-DATAONLY.
02285
02286      EJECT
02287  0600-MAIN-LOGIC.
02288      IF PI-MAP-NAME NOT = EL142F
02289          GO TO 0700-MAIN-LOGIC.
02290
02291      IF NOT MODIFY-CAP
02292          IF FMAINTI = 'S'
02293              NEXT SENTENCE
02294          ELSE
02295              MOVE 'UPDATE'       TO SM-READ
02296              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
02297              MOVE ER-0070        TO  EMI-ERROR
02298              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02299              GO TO 8100-SEND-INITIAL-MAP.
02300
02301      MOVE +2                     TO  EMI-NUMBER-OF-LINES
02302                                      EMI-SWITCH2.
02303
02304      IF FMAINTL NOT GREATER ZERO
02305        OR (FMAINTL GREATER ZERO AND
02306            FMAINTI = 'S')
02307              PERFORM 4000-READ-TRAILER-FILE.
02308
02309      IF FMAINTI = 'C' OR 'D'
02310          NEXT SENTENCE
02311      ELSE
02312          MOVE ER-0023            TO  EMI-ERROR
02313          MOVE -1                 TO  FMAINTL
02314          MOVE AL-UABON           TO  FMAINTA
02315          PERFORM 8200-SEND-DATAONLY.
02316
020816     if pi-company-id = 'CID' or 'DCC' OR 'AHL' OR 'VPP'
062121           OR 'FNL'
062602        if (pi-el142-priority = '8')
120718           AND (PI-approval-level <> '5')
062602           MOVE ER-8003          TO EMI-ERROR
062602           MOVE -1               TO fMAINTL
062602           MOVE AL-UABON         TO fMAINTA
062602           GO TO 8200-SEND-DATAONLY.
062602
02317      IF FMAINTI = 'D'
02318          PERFORM 3000-READ-FOR-UPDATE
02319          PERFORM 3200-DELETE
02320          MOVE +1                 TO  WS-COMPLETED-SUCCESSFUL
02321          MOVE -1                 TO  FPFKL
02322          MOVE 'S'                TO  FMAINTO
02323          MOVE AL-SANOF           TO  FMAINTA  FSNOTIFA  FENOTIFA
02324                                      FLINE1A  FLINE2A
02325          PERFORM 8200-SEND-DATAONLY.
02326
02327      MOVE AL-UANON               TO  FMAINTA.
02328
02329      IF FSNOTIFL GREATER ZERO
02330          MOVE FSNOTIFI           TO  WS-DEEDIT-FIELD
02331          PERFORM 8600-DEEDIT
02332          IF WS-DEEDIT-FIELD-V0 IS NUMERIC
02333              MOVE WS-DEEDIT-FIELD-V0  TO  FSNOTIFO
02334              INSPECT FSNOTIFI CONVERTING SPACES TO SLASH
02335              MOVE '4'                 TO  DC-OPTION-CODE
02336              MOVE WS-DEEDIT-FIELD-V0  TO  DC-GREG-DATE-1-MDY
02337              PERFORM 8500-DATE-CONVERSION
02338              IF DC-ERROR-CODE NOT = SPACES
02339                  MOVE ER-0298        TO  EMI-ERROR
02340                  MOVE -1             TO  FSNOTIFL
02341                  MOVE AL-UABON       TO  FSNOTIFA
02342                  PERFORM 9900-ERROR-FORMAT
02343              ELSE
02344                  MOVE AL-UANON       TO  FSNOTIFA
02345                  MOVE +1             TO  WS-UPDATE-SW
02346                  MOVE DC-BIN-DATE-1  TO  WS-START-DATE
02347          ELSE
02348              MOVE ER-0298        TO  EMI-ERROR
02349              MOVE -1             TO  FSNOTIFL
02350              MOVE AL-UABON       TO  FSNOTIFA
02351              PERFORM 9900-ERROR-FORMAT.
02352
02353      IF FENOTIFL GREATER ZERO
02354          MOVE FENOTIFI           TO  WS-DEEDIT-FIELD
02355          PERFORM 8600-DEEDIT
02356          IF WS-DEEDIT-FIELD-V0 IS NUMERIC
02357              MOVE WS-DEEDIT-FIELD-V0  TO  FENOTIFO
02358              INSPECT FENOTIFI CONVERTING SPACES TO SLASH
02359              MOVE '4'                 TO  DC-OPTION-CODE
02360              MOVE WS-DEEDIT-FIELD-V0  TO  DC-GREG-DATE-1-MDY
02361              PERFORM 8500-DATE-CONVERSION
02362              IF DC-ERROR-CODE NOT = SPACES
02363                  MOVE ER-0299           TO  EMI-ERROR
02364                  MOVE -1                TO  FENOTIFL
02365                  MOVE AL-UABON          TO  FENOTIFA
02366                  PERFORM 9900-ERROR-FORMAT
02367              ELSE
02368                  MOVE AL-UANON       TO  FENOTIFA
02369                  MOVE +1             TO  WS-UPDATE-SW
02370                  MOVE DC-BIN-DATE-1  TO  WS-END-DATE
02371          ELSE
02372              MOVE ER-0299        TO  EMI-ERROR
02373              MOVE -1             TO  FENOTIFL
02374              MOVE AL-UABON       TO  FENOTIFA
02375              PERFORM 9900-ERROR-FORMAT.
02376
02377      IF FLINE1L GREATER ZERO
02378        OR FLINE2L GREATER ZERO
02379          MOVE +1                 TO  WS-UPDATE-SW.
02380
02381      IF WS-ERROR-COUNT GREATER ZERO
02382          PERFORM 8200-SEND-DATAONLY.
02383
02384      IF WS-UPDATE-SW NOT GREATER ZERO
02385          PERFORM 4000-READ-TRAILER-FILE.
02386
02387      IF FSNOTIFL GREATER ZERO
02388          PERFORM 3300-UPDATE-CLAIM-MASTER.
02389
02390      PERFORM 3000-READ-FOR-UPDATE.
02391
02392      IF FLINE1L GREATER ZERO
02393          MOVE FLINE1I            TO  AT-PROMPT-LINE-1.
02394
02395      IF FLINE2L GREATER ZERO
02396          MOVE FLINE2I            TO  AT-PROMPT-LINE-2.
02397
02398      IF FSNOTIFL GREATER ZERO
02399          MOVE WS-START-DATE      TO  AT-PROMPT-START-DT.
02400
02401      IF FENOTIFL GREATER ZERO
02402          MOVE WS-END-DATE        TO  AT-PROMPT-END-DT.
02403
02404      MOVE PI-PROCESSOR-ID        TO  AT-PROMPT-LAST-UPDATED-BY.
02405
02406      MOVE WS-CURRENT-DATE        TO  AT-PROMPT-LAST-MAINT-DT.
02407
02408      PERFORM 3100-REWRITE.
02409      MOVE +1                     TO  WS-COMPLETED-SUCCESSFUL.
02410
02411      MOVE 'S'                    TO  FMAINTO.
02412      MOVE -1                     TO  FMAINTL.
02413      MOVE AL-UANOF               TO  FMAINTA.
02414      PERFORM 8200-SEND-DATAONLY.
02415
02416      EJECT
02417  0700-MAIN-LOGIC.
02418      IF PI-MAP-NAME NOT = EL142G
02419          GO TO 0800-MAIN-LOGIC.
02420
02421      IF NOT MODIFY-CAP
02422          IF GMAINTI = 'S'
02423              NEXT SENTENCE
02424          ELSE
02425              MOVE 'UPDATE'       TO SM-READ
02426              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
02427              MOVE ER-0070        TO  EMI-ERROR
02428              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02429              GO TO 8100-SEND-INITIAL-MAP.
02430
02431      MOVE +2                     TO  EMI-NUMBER-OF-LINES
02432                                      EMI-SWITCH2.
02433
02434      IF GMAINTL NOT GREATER ZERO
02435        OR (GMAINTL GREATER ZERO AND
02436            GMAINTI = 'S')
02437              PERFORM 4000-READ-TRAILER-FILE.
02438
042110     IF GMAINTI NOT = 'C' AND 'D'
02440          MOVE ER-0023            TO  EMI-ERROR
02441          MOVE -1                 TO  GMAINTL
02442          MOVE AL-UABON           TO  GMAINTA
02443          PERFORM 8200-SEND-DATAONLY.
02444
020816     if pi-company-id = 'CID' or 'DCC' OR 'AHL' OR 'VPP'
062121           OR 'FNL'
062602        if (pi-el142-priority = '8')
120718           AND (PI-approval-level <> '5')
062602           MOVE ER-8003          TO EMI-ERROR
062602           MOVE -1               TO gMAINTL
062602           MOVE AL-UABON         TO gMAINTA
062602           GO TO 8200-SEND-DATAONLY.
062602
042110     IF GMAINTI = 'D'
120718        if pi-approval-level <> '5'
042110           MOVE ER-0310            TO  EMI-ERROR
042110           MOVE -1                 TO  GMAINTL
042110           MOVE AL-UABON           TO  GMAINTA
042110           PERFORM 8200-SEND-DATAONLY
042110        ELSE
042110           ADD 1 TO PI-MAPG-DELETE-CNT
042110           IF PI-MAPG-DELETE-CNT > 1
042110              PERFORM 3000-READ-FOR-UPDATE
042110              PERFORM 3200-DELETE
042110              PERFORM 3500-READ-ELMSTR-FOR-UPDATE
042110                                 THRU 3599-EXIT
042110              MOVE SPACES        TO CL-DENIAL-TYPE
042110              MOVE 'O'           TO CL-CLAIM-STATUS
042110              
      * EXEC CICS REWRITE
042110*                DATASET (WS-CLAIM-MASTER-DSID)
042110*                FROM    (CLAIM-MASTER)
042110*             END-EXEC
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00011111' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303131313131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CLAIM-MASTER-DSID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
042110              MOVE 'O'              TO WS-CLAIM-TYPE
042110              PERFORM 0710-UPDATE-ZERO-TRLR THRU 0730-MAIN-LOGIC
042110              MOVE +1               TO WS-COMPLETED-SUCCESSFUL
042110              MOVE -1               TO GPFKL
042110              MOVE 'S'              TO GMAINTO
042110              MOVE AL-SANOF         TO GMAINTA  GLINE1A  GLINE2A
042110              PERFORM 8200-SEND-DATAONLY
042110           ELSE
042110              MOVE ER-0755            TO  EMI-ERROR
042110              MOVE -1                 TO  GMAINTL
042110              MOVE AL-UABOF           TO  GMAINTA
042110              PERFORM 8200-SEND-DATAONLY
042110           END-IF
042110        END-IF
042110     END-IF
02445      MOVE AL-UANON               TO  GMAINTA.
02446
02447      IF GRECONSL GREATER ZERO
02448          IF GRECONSI = SPACES
02449              MOVE AL-UANON       TO  GRECONSA
02450              MOVE +1             TO  WS-UPDATE-SW
02451              MOVE LOW-VALUES     TO  WS-END-DATE
042110             MOVE +4             TO  GRSNCDL
02452          ELSE
042110             MOVE +0             TO  GRSNCDL
02453              MOVE GRECONSI           TO  WS-DEEDIT-FIELD
02454              PERFORM 8600-DEEDIT
02455              IF WS-DEEDIT-FIELD-V0 IS NUMERIC
02456                  MOVE WS-DEEDIT-FIELD-V0  TO  GRECONSO
02457                  INSPECT GRECONSI CONVERTING SPACES TO SLASH
02458                  MOVE '4'                 TO  DC-OPTION-CODE
02459                  MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY
02460                  PERFORM 8500-DATE-CONVERSION
02461                  IF DC-ERROR-CODE NOT = SPACES
02462                      MOVE ER-0300        TO  EMI-ERROR
02463                      MOVE -1             TO  GRECONSL
02464                      MOVE AL-UABON       TO  GRECONSA
02465                      PERFORM 9900-ERROR-FORMAT
02466                  ELSE
02467                      MOVE AL-UANON       TO  GRECONSA
02468                      MOVE +1             TO  WS-UPDATE-SW
02469                      MOVE DC-BIN-DATE-1  TO  WS-END-DATE
02470              ELSE
02471                  MOVE ER-0300        TO  EMI-ERROR
02472                  MOVE -1             TO  GRECONSL
02473                  MOVE AL-UNBON       TO  GRECONSA
02474                  PERFORM 9900-ERROR-FORMAT.
02475
02476      IF WS-END-DATE GREATER THAN WS-CURRENT-DATE
02477         MOVE ER-0300             TO  EMI-ERROR
02478         MOVE -1                  TO  GRECONSL
02479         MOVE AL-UABON            TO  GRECONSA
02480         PERFORM 9900-ERROR-FORMAT.
02481
052506     IF  GPRFDTL > 0
052506         MOVE GPRFDTI             TO WS-DEEDIT-FIELD
052506         PERFORM 8600-DEEDIT
052506         MOVE WS-DEEDIT-FIELD-V0  TO DC-GREG-DATE-1-MDY
052506         MOVE '4'                 TO DC-OPTION-CODE
052506         PERFORM 8500-DATE-CONVERSION
052506         IF DC-ERROR-CODE NOT = SPACES
052506            MOVE ER-0021          TO EMI-ERROR
052506            MOVE -1               TO GPRFDTL
052506            MOVE AL-UABON         TO GPRFDTA
052506            PERFORM 9900-ERROR-FORMAT
052506         ELSE
043019            IF (DC-BIN-DATE-1 > WS-CURRENT-DATE)
043019               or (dc-bin-date-1 > pi-den-recorded-dt)
043019               or (dc-bin-date-1 < pi-incurred-dt)
052506                MOVE ER-0873      TO EMI-ERROR
052506                MOVE -1           TO GPRFDTL
052506                MOVE AL-UABON     TO GPRFDTA
052506                PERFORM 9900-ERROR-FORMAT
052506            ELSE
052506                MOVE AL-UANON       TO  GPRFDTA
052506                MOVE +1             TO  WS-UPDATE-SW
052506                MOVE DC-BIN-DATE-1    TO WS-PRF-DT
052506                MOVE WS-DEEDIT-FIELD-V0  TO GPRFDTO
052506                INSPECT GPRFDTI CONVERTING ' ' TO '/'.
052506
02482      IF GLINE1L GREATER ZERO OR
02483         GLINE2L GREATER ZERO
02484            MOVE +1               TO  WS-UPDATE-SW.
02485
02486      IF WS-ERROR-COUNT GREATER ZERO
02487          PERFORM 8200-SEND-DATAONLY.
02488
02489      IF GRSNCDL GREATER ZERO
02490          MOVE +1 TO WS-UPDATE-SW.
02491
02492      IF WS-UPDATE-SW NOT GREATER ZERO
02493          PERFORM 4000-READ-TRAILER-FILE.
02494
02495      IF GRECONSL NOT GREATER ZERO
02496          GO TO 0740-MAIN-LOGIC.
02497
02498      MOVE PI-SAVE-KEY            TO PI-ACTIVITY-TRAILERS-KEY.
02499
02500      PERFORM 3500-READ-ELMSTR-FOR-UPDATE.
02501
02502      IF PI-COMPANY-ID = 'DMD'
02503          MOVE CL-CERT-KEY-DATA     TO WS-CL-CERT-KEY-DATA
02504          MOVE CL-CERT-NO           TO WS-CL-CERT-NO
02505          MOVE CL-BENEFICIARY       TO WS-CL-BENEFICIARY
02506          MOVE CL-CCN               TO WS-CL-CCN
02507          MOVE CL-CLAIM-NO          TO WS-CL-CLAIM-NO
02508          MOVE CL-CLAIM-TYPE        TO WS-CL-CLAIM-TYPE
02509          MOVE CL-INSURED-LAST-NAME TO WS-CL-INSURED-LAST-NAME
02510          MOVE CL-INSURED-1ST-NAME  TO WS-CL-INSURED-1ST-NAME
02511          MOVE CL-INSURED-MID-INIT  TO WS-CL-INSURED-MID-INIT
02512          MOVE CL-NO-OF-PMTS-MADE   TO WS-CL-NO-OF-PMTS-MADE.
02513
02514      IF WS-END-DATE = LOW-VALUES
02515         IF CLAIM-IS-CLOSED
02516             
      * EXEC CICS UNLOCK
02517 *                DATASET   (WS-CLAIM-MASTER-DSID)
02518 *           END-EXEC
      *    MOVE '&*                    #   #00011229' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303131323239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CLAIM-MASTER-DSID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02519             GO TO 0740-MAIN-LOGIC
02520         ELSE
02521            MOVE 'C'              TO CL-CLAIM-STATUS
02522            MOVE WS-CURRENT-DATE  TO CL-LAST-CLOSE-DT
02523      ELSE
02524         IF CLAIM-IS-OPEN
02525             
      * EXEC CICS UNLOCK
02526 *                DATASET   (WS-CLAIM-MASTER-DSID)
02527 *           END-EXEC
      *    MOVE '&*                    #   #00011238' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303131323338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CLAIM-MASTER-DSID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02528             GO TO 0740-MAIN-LOGIC
02529         ELSE
02530            MOVE 'O'              TO CL-CLAIM-STATUS
042110           MOVE '5'              TO CL-DENIAL-TYPE
02531            MOVE WS-CURRENT-DATE  TO CL-LAST-REOPEN-DT.
02532
02533      MOVE CL-CLAIM-STATUS        TO  WS-CLAIM-TYPE.
02534
02535      PERFORM 3600-REWRITE-ELMSTR.
02536
042110 0710-UPDATE-ZERO-TRLR.
042110     MOVE PI-SAVE-KEY              TO  WS-ACTIVITY-TRAILERS-KEY.
02538      MOVE ZERO                     TO  WS-ATK-SEQUENCE-NO.
02539
02540      
      * EXEC CICS READ UPDATE
02541 *        DATASET (WS-ACTIVITY-TRAILERS-DSID)
02542 *        RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)
02543 *        SET     (ADDRESS OF ACTIVITY-TRAILERS)
02544 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00011255' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303131323535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ACTIVITY-TRAILERS-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02545
02546      MOVE +1                     TO  WS-INDEX.
02547
02548  0720-MAIN-LOGIC.
02549      IF AT-OPEN-CLOSE-TYPE (WS-INDEX) = SPACES
02550         IF WS-INDEX GREATER +1
02551            SUBTRACT +1 FROM WS-INDEX
02552            IF AT-OPEN-CLOSE-TYPE (WS-INDEX) = WS-CLAIM-TYPE
02553               MOVE WS-CURRENT-DATE TO
02554                    AT-OPEN-CLOSE-DATE (WS-INDEX)
02555               GO TO 0730-MAIN-LOGIC
02556            ELSE
02557               ADD +1 TO WS-INDEX
02558               MOVE WS-CURRENT-DATE  TO
02559                      AT-OPEN-CLOSE-DATE (WS-INDEX)
02560               MOVE WS-CLAIM-TYPE    TO
02561                      AT-OPEN-CLOSE-TYPE (WS-INDEX)
02562               MOVE 'ALTER'          TO
02563                      AT-OPEN-CLOSE-REASON (WS-INDEX)
02564               GO TO 0730-MAIN-LOGIC.
02565
02566      IF WS-INDEX LESS THAN +6
02567          ADD +1                  TO  WS-INDEX
02568          GO TO 0720-MAIN-LOGIC.
02569
02570      IF AT-OPEN-CLOSE-TYPE (6) = WS-CLAIM-TYPE
02571         MOVE WS-CURRENT-DATE     TO AT-OPEN-CLOSE-DATE (6)
02572         GO TO 0730-MAIN-LOGIC.
02573
02574      MOVE AT-OPEN-CLOSE-HISTORY (2) TO AT-OPEN-CLOSE-HISTORY (1).
02575      MOVE AT-OPEN-CLOSE-HISTORY (3) TO AT-OPEN-CLOSE-HISTORY (2).
02576      MOVE AT-OPEN-CLOSE-HISTORY (4) TO AT-OPEN-CLOSE-HISTORY (3).
02577      MOVE AT-OPEN-CLOSE-HISTORY (5) TO AT-OPEN-CLOSE-HISTORY (4).
02578      MOVE AT-OPEN-CLOSE-HISTORY (6) TO AT-OPEN-CLOSE-HISTORY (5).
02579
02580      MOVE WS-CURRENT-DATE        TO  AT-OPEN-CLOSE-DATE (6).
02581      MOVE WS-CLAIM-TYPE          TO  AT-OPEN-CLOSE-TYPE (6).
02582      MOVE 'ALTER'                TO  AT-OPEN-CLOSE-REASON (6).
02583
02584  0730-MAIN-LOGIC.
02585
02586      MOVE PI-PROCESSOR-ID        TO  AT-RESERVES-LAST-UPDATED-BY.
02587      MOVE WS-CURRENT-DATE        TO  AT-RESERVES-LAST-MAINT-DT.
02588
02589      
      * EXEC CICS REWRITE
02590 *        DATASET (WS-ACTIVITY-TRAILERS-DSID)
02591 *        FROM    (ACTIVITY-TRAILERS)
02592 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00011304' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303131333034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02593
02594  0740-MAIN-LOGIC.
02595
02596      PERFORM 3000-READ-FOR-UPDATE.
02597
02598      IF GLINE1L GREATER ZERO
02599          MOVE GLINE1I            TO  AT-DENIAL-INFO-1.
02600
02601      IF GLINE2L GREATER ZERO
02602          MOVE GLINE2I            TO  AT-DENIAL-INFO-2.
033010     IF GRSNCDL > +0
033010        MOVE LOW-VALUES          TO WS-ELDENY-KEY
033010        MOVE PI-COMPANY-CD       TO ELDENY-COMPANY-CD
033010        MOVE GRSNCDI             TO ELDENY-DENIAL-CODE
033010        
      * EXEC CICS READ
033010*          DATASET('ELDENY')
033010*          SET    (ADDRESS OF DENIAL-CODES)
033010*          RIDFLD (WS-ELDENY-KEY)
033010*          RESP   (WS-RESPONSE)
033010*       END-EXEC
           MOVE 'ELDENY' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00011322' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303131333232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ELDENY-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF DENIAL-CODES TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
033010        IF RESP-NORMAL
033010           MOVE GRSNCDI          TO AT-DENIAL-REASON-CODE
042110           IF AT-DENIAL-REASON-CODE NOT = PI-DENIAL-REASON-CODE
033010              IF GLINE1L > +0
033010                 STRING DN-DESCRIPTION ' ' GLINE1I
033010                    DELIMITED BY '  ' INTO AT-DENIAL-INFO-1
033010                 END-STRING
033010              ELSE
033010                 STRING AT-DENIAL-INFO-1 ' ' DN-DESCRIPTION
033010                    DELIMITED BY '  ' INTO AT-DENIAL-INFO-1
033010                 END-STRING
033010              END-IF
                 END-IF
033010           PERFORM 3500-READ-ELMSTR-FOR-UPDATE
033010                                 THRU 3599-EXIT
033010           MOVE DN-RECORD-TYPE   TO CL-DENIAL-TYPE
033010           
      * EXEC CICS REWRITE
033010*             DATASET (WS-CLAIM-MASTER-DSID)
033010*             FROM    (CLAIM-MASTER)
033010*          END-EXEC
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00011344' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303131333434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CLAIM-MASTER-DSID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
033010           MOVE AT-DENIAL-INFO-1 TO GLINE1O
033010           MOVE +1               TO GLINE1L
033010           MOVE AL-UANON         TO GLINE1A
033010        ELSE
033010           MOVE AL-UABON         TO GRSNCDA
033010           MOVE -1               TO GRSNCDL
033010           MOVE ER-0884          TO EMI-ERROR
033010           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
033010           
      * EXEC CICS UNLOCK
033010*             DATASET (WS-ACTIVITY-TRAILERS-DSID)
033010*          END-EXEC
      *    MOVE '&*                    #   #00011356' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303131333536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
033010           GO TO 8200-SEND-DATAONLY
033010        END-IF
033010     END-IF
02608
02609      IF GRECONSL GREATER ZERO
02610          MOVE WS-END-DATE        TO  AT-RETRACTION-DT
02611          IF PI-COMPANY-ID = 'DMD'
02612              PERFORM 8000-CREATE-DMO-REC THRU 8000-EXIT.
052506
052506     IF GPRFDTL GREATER ZERO
052506         MOVE WS-PRF-DT          TO  AT-DENIAL-PROOF-DT.
02613
02614      MOVE PI-PROCESSOR-ID        TO  AT-DENIAL-LAST-UPDATED-BY.
02615
02616      MOVE WS-CURRENT-DATE        TO  AT-DENIAL-LAST-MAINT-DT.
02617
02618      PERFORM 3100-REWRITE.
02619      MOVE +1                     TO  WS-COMPLETED-SUCCESSFUL.
02620
02621      MOVE 'S'                    TO  GMAINTO.
02622      MOVE -1                     TO  GMAINTL.
02623      MOVE AL-UANOF               TO  GMAINTA.
02624      PERFORM 8200-SEND-DATAONLY.
02625
02626      EJECT
02627  0800-MAIN-LOGIC.
02628      IF PI-MAP-NAME NOT = EL142H
02629          GO TO 0900-MAIN-LOGIC.
02630
02631      IF NOT MODIFY-CAP
02632          IF HMAINTI = 'S'
02633              NEXT SENTENCE
02634          ELSE
02635              MOVE 'UPDATE'       TO SM-READ
02636              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
02637              MOVE ER-0070        TO  EMI-ERROR
02638              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02639              GO TO 8100-SEND-INITIAL-MAP.
02640
02641      MOVE +2                     TO  EMI-NUMBER-OF-LINES
02642                                      EMI-SWITCH2.
02643
02644      IF HMAINTL NOT GREATER ZERO
02645        OR (HMAINTL GREATER ZERO AND
02646            HMAINTI = 'S')
02647              PERFORM 4000-READ-TRAILER-FILE.
02648
02649      IF HMAINTI NOT = 'C'
02650          MOVE ER-0023            TO  EMI-ERROR
02651          MOVE -1                 TO  HMAINTL
02652          MOVE AL-UABON           TO  HMAINTA
02653          PERFORM 8200-SEND-DATAONLY.
02654
020816     if pi-company-id = 'CID' or 'DCC' OR 'AHL' OR 'VPP'
062121           OR 'FNL'
062602        if (pi-el142-priority = '8')
120718           AND (PI-approval-level <> '5')
062602           MOVE ER-8003          TO EMI-ERROR
062602           MOVE -1               TO hMAINTL
062602           MOVE AL-UABON         TO hMAINTA
062602           GO TO 8200-SEND-DATAONLY.
062602
02655      MOVE AL-UANON               TO  HMAINTA.
02656
02657      IF HRESMANL GREATER ZERO
02658          IF HRESMANI = 'Y' OR 'N'
02659              MOVE AL-UANON       TO  HRESMANA
02660          ELSE
02661              MOVE -1             TO  HRESMANL
02662              MOVE AL-UABON       TO  HRESMANA
02663              MOVE ER-0107        TO  EMI-ERROR
02664              PERFORM 9900-ERROR-FORMAT.
02665
02666      IF HRESFUTL GREATER ZERO
02667          IF HRESFUTI = 'Y' OR 'N'
02668              MOVE AL-UANON       TO  HRESFUTA
02669          ELSE
02670              MOVE -1             TO  HRESFUTL
02671              MOVE AL-UABON       TO  HRESFUTA
02672              MOVE ER-0109        TO  EMI-ERROR
02673              PERFORM 9900-ERROR-FORMAT.
02674
02675      IF HRESIBNL GREATER ZERO
02676          IF HRESIBNI = 'Y' OR 'N'
02677              MOVE AL-UANON       TO  HRESIBNA
02678          ELSE
02679              MOVE -1             TO  HRESIBNL
02680              MOVE AL-UABON       TO  HRESIBNA
02681              MOVE ER-0111        TO  EMI-ERROR
02682              PERFORM 9900-ERROR-FORMAT.
02683
02684      IF HRESLFPL GREATER ZERO
02685          IF HRESLFPI = 'Y' OR 'N'
02686              MOVE AL-UANON       TO  HRESLFPA
02687          ELSE
02688              MOVE -1             TO  HRESLFPL
02689              MOVE AL-UABON       TO  HRESLFPA
02690              MOVE ER-0324        TO  EMI-ERROR
02691              PERFORM 9900-ERROR-FORMAT.
02692
02693      IF HRESAHPL GREATER ZERO
02694          IF HRESAHPI = 'Y' OR 'N'
02695              MOVE AL-UANON       TO  HRESAHPA
02696          ELSE
02697              MOVE -1             TO  HRESAHPL
02698              MOVE AL-UABON       TO  HRESAHPA
02699              MOVE ER-0325        TO  EMI-ERROR
02700              PERFORM 9900-ERROR-FORMAT.
02701
02702      IF HCDTAML GREATER ZERO
02703          IF HCDTAMI = '1' OR '2' OR '3'
02704              MOVE AL-UNNON       TO  HCDTAMA
02705          ELSE
02706              MOVE AL-UNBON       TO  HCDTAMA
02707              MOVE -1             TO  HCDTAML
02708              MOVE ER-0105        TO  EMI-ERROR
02709              PERFORM 9900-ERROR-FORMAT.
02710
02711      IF HMANAMTL GREATER ZERO
02712          
      * EXEC CICS BIF DEEDIT
02713 *            FIELD  (HMANAMTI)
02714 *            LENGTH (WS-HMANAMT-LENGTH)
02715 *        END-EXEC
      *    MOVE '@"L                   #   #00011478' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303131343738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 HMANAMTI, 
                 WS-HMANAMT-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02716          IF HMANAMTI IS NUMERIC
02717              MOVE HMANAMTI       TO  WS-HMANAMTI
02718                                      HMANAMTO
02719              MOVE AL-UNNON       TO  HMANAMTA
02720            ELSE
02721              MOVE -1             TO  HMANAMTL
02722              MOVE AL-UNBON       TO  HMANAMTA
02723              MOVE ER-0107        TO  EMI-ERROR
02724              PERFORM 9900-ERROR-FORMAT.
02725
02726      IF HPCTCDTL GREATER ZERO
02727          
      * EXEC CICS BIF DEEDIT
02728 *            FIELD  (HPCTCDTI)
02729 *            LENGTH (WS-HPCTCDT-LENGTH)
02730 *        END-EXEC
      *    MOVE '@"L                   #   #00011493' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303131343933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 HPCTCDTI, 
                 WS-HPCTCDT-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02731          IF HPCTCDTI IS NUMERIC
02732              MOVE HPCTCDTI       TO  WS-HPCTCDTI
02733                                      HPCTCDTO
02734              MOVE AL-UNNON       TO  HPCTCDTA
02735          ELSE
02736              MOVE -1             TO  HPCTCDTL
02737              MOVE AL-UNBON       TO  HPCTCDTA
02738              MOVE ER-0106        TO  EMI-ERROR
02739              PERFORM 9900-ERROR-FORMAT.
02740
02741      IF HEXPL GREATER ZERO
02742          IF HEXPI = '1' OR '2' OR '3'
02743              MOVE AL-UNNON       TO  HEXPA
02744          ELSE
02745              MOVE -1             TO  HEXPL
02746              MOVE AL-UNBON       TO  HEXPA
02747              MOVE ER-0327        TO  EMI-ERROR
02748              PERFORM 9900-ERROR-FORMAT.
02749
02750      IF HEXPAMTL GREATER ZERO
02751          
      * EXEC CICS BIF DEEDIT
02752 *            FIELD  (HEXPAMTI)
02753 *            LENGTH (WS-HEXPAMT-LENGTH)
02754 *        END-EXEC
      *    MOVE '@"L                   #   #00011517' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303131353137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 HEXPAMTI, 
                 WS-HEXPAMT-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02755          IF HEXPAMTI IS NUMERIC
02756              MOVE HEXPAMTI       TO  WS-HEXPAMTI
02757                                      HEXPAMTO
02758              MOVE AL-UNNON       TO  HEXPAMTA
02759          ELSE
02760              MOVE -1             TO  HEXPAMTL
02761              MOVE AL-UNBON       TO  HEXPAMTA
02762              MOVE ER-0328        TO  EMI-ERROR
02763              PERFORM 9900-ERROR-FORMAT.
02764
02765      IF WS-ERROR-COUNT GREATER ZERO
02766          PERFORM 8200-SEND-DATAONLY.
02767
02768      PERFORM 3000-READ-FOR-UPDATE.
02769
02770      IF HRESMANL GREATER ZERO
02771          MOVE HRESMANI           TO  AT-MANUAL-SW
02772          INSPECT AT-MANUAL-SW CONVERTING 'NY' TO ' 1'.
02773
02774      IF HMANAMTL GREATER ZERO
02775          MOVE WS-HMANAMTI        TO  AT-CURRENT-MANUAL-RESERVE.
02776
02777      IF HRESFUTL GREATER ZERO
02778          MOVE HRESFUTI           TO  AT-FUTURE-SW
02779          INSPECT AT-FUTURE-SW CONVERTING 'NY' TO ' 1'.
02780
02781      IF HRESIBNL GREATER ZERO
02782          MOVE HRESIBNI           TO  AT-IBNR-SW
02783          INSPECT AT-IBNR-SW CONVERTING 'NY' TO ' 1'.
02784
02785      IF HRESLFPL GREATER ZERO
02786          MOVE HRESLFPI           TO  AT-PTC-LF-SW
02787          INSPECT AT-PTC-LF-SW CONVERTING 'NY' TO ' 1'.
02788
02789      IF HRESAHPL GREATER ZERO
02790          MOVE HRESAHPI           TO  AT-PTC-SW
02791          INSPECT AT-PTC-SW CONVERTING 'NY' TO ' 1'.
02792
02793      IF HCDTAML GREATER ZERO
02794          MOVE HCDTAMI            TO  AT-CDT-ACCESS-METHOD.
02795
02796      IF HMANAMTL GREATER ZERO
02797          MOVE WS-HMANAMTI        TO  AT-CURRENT-MANUAL-RESERVE.
02798
02799      IF HPCTCDTL GREATER ZERO
02800          MOVE WS-HPCTCDTI        TO  AT-PERCENT-OF-CDT.
02801
02802      IF HEXPL GREATER ZERO
02803          MOVE HEXPI              TO  AT-EXPENSE-METHOD.
02804
02805      IF HEXPAMTL GREATER ZERO
02806          IF AT-EXPENSE-METHOD = '2' OR '4'
02807              MOVE WS-HEXPAMTI    TO  AT-EXPENSE-DOLLAR
02808            ELSE
02809              IF AT-EXPENSE-METHOD = '3'
02810                  MOVE WS-HEXPAMTI TO AT-EXPENSE-PERCENT
02811                ELSE
02812                  MOVE ZERO       TO  AT-EXPENSE-DOLLAR
02813                                      AT-EXPENSE-PERCENT.
02814
02815      MOVE PI-PROCESSOR-ID        TO  AT-RESERVES-LAST-UPDATED-BY.
02816
02817      MOVE WS-CURRENT-DATE        TO  AT-RESERVES-LAST-MAINT-DT.
02818
02819      PERFORM 3100-REWRITE.
02820      MOVE +1                     TO  WS-COMPLETED-SUCCESSFUL.
02821
02822      MOVE 'S'                    TO  HMAINTO.
02823      MOVE -1                     TO  HMAINTL.
02824      MOVE AL-UANOF               TO  HMAINTA.
02825      PERFORM 8200-SEND-DATAONLY.
02826
02827      EJECT
02828  0900-MAIN-LOGIC.
02829      IF PI-MAP-NAME NOT = EL142I
02830          GO TO 1000-MAIN-LOGIC.
02831
02832      PERFORM 4000-READ-TRAILER-FILE.
02833
02834      EJECT
02835  1000-MAIN-LOGIC.
02836      IF PI-MAP-NAME NOT = EL142J
02837          GO TO 1100-MAIN-LOGIC.
02838
02839      IF NOT MODIFY-CAP
02840          IF JMAINTI = 'S'
02841              NEXT SENTENCE
02842          ELSE
02843              MOVE 'UPDATE'       TO SM-READ
02844              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
02845              MOVE ER-0070        TO  EMI-ERROR
02846              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02847              GO TO 8100-SEND-INITIAL-MAP.
02848
02849      MOVE +3                     TO  EMI-NUMBER-OF-LINES.
02850      MOVE +2                     TO  EMI-SWITCH2.
02851
02852      IF JMAINTL NOT GREATER ZERO
02853        OR (JMAINTL GREATER ZERO AND
02854            JMAINTI = 'S')
02855              PERFORM 4000-READ-TRAILER-FILE.
02856
02857      IF JMAINTI = 'C' OR 'D'
02858          NEXT SENTENCE
02859      ELSE
02860          MOVE ER-0023            TO  EMI-ERROR
02861          MOVE -1                 TO  JMAINTL
02862          MOVE AL-UABON           TO  JMAINTA
02863          PERFORM 8200-SEND-DATAONLY.
02864
020816     if pi-company-id = 'CID' or 'DCC' OR 'AHL' OR 'VPP'
062121           OR 'FNL'
062602        if (pi-el142-priority = '8')
120718           AND (PI-approval-level <> '5')
062602           MOVE ER-8003          TO EMI-ERROR
062602           MOVE -1               TO jMAINTL
062602           MOVE AL-UABON         TO jMAINTA
062602           GO TO 8200-SEND-DATAONLY.
062602
02865      IF FMAINTI NOT = 'D'
02866          GO TO 1050-MAIN-LOGIC.
02867
02868      
      * EXEC CICS READ
02869 *        DATASET (WS-ACTIVITY-TRAILERS-DSID)
02870 *        RIDFLD  (PI-SAVE-KEY)
02871 *        SET     (ADDRESS OF ACTIVITY-TRAILERS)
02872 *    END-EXEC.
      *    MOVE '&"S        E          (   #00011643' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303131363433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-SAVE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02873
02874      IF AT-FORM-PRINTED-DT NOT = LOW-VALUES
02875          MOVE ER-0564            TO  EMI-ERROR
02876          MOVE -1                 TO  JMAINTL
02877          MOVE AL-UABON           TO  JMAINTA
02878          PERFORM 8200-SEND-DATAONLY.
02879
02880      PERFORM 3000-READ-FOR-UPDATE.
02881
02882      PERFORM 3400-DELETE-FORM-ARCHIVE.
02883
02884      PERFORM 3200-DELETE.
02885
02886      MOVE +1                     TO  WS-COMPLETED-SUCCESSFUL.
02887      MOVE -1                     TO  JPFKL.
02888      MOVE 'S'                    TO  JMAINTO.
02889
02890      MOVE AL-SANOF               TO  JMAINTA  JDTSENTA  JRESENDA
02891                                      JSI1A    JREPLYA   JSI2A
02892                                      JRECEVEA JSI3A     JFORMA
02893                                      JCARR1A  JCLAIM1A  JCERT1A
02894                                      JCARR2A  JCLAIM2A  JCERT2A
02895                                      JPHYRECA JEMPRECA  JREMDTA.
02896
02897      PERFORM 8200-SEND-DATAONLY.
02898
02899  1050-MAIN-LOGIC.
02900      MOVE AL-UANON               TO  JMAINTA.
02901
02902      IF JDTSENTL GREATER ZERO
02903          MOVE JDTSENTI           TO  WS-DEEDIT-FIELD
02904          PERFORM 8600-DEEDIT
02905          IF WS-DEEDIT-FIELD-V0 IS NUMERIC
02906              MOVE WS-DEEDIT-FIELD-V0  TO  JDTSENTO
02907              INSPECT JDTSENTI CONVERTING SPACES TO SLASH
02908              MOVE '4'                 TO  DC-OPTION-CODE
02909              MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY
02910              PERFORM 8500-DATE-CONVERSION
02911              IF DC-ERROR-CODE NOT = SPACES
02912                  MOVE ER-0550    TO  EMI-ERROR
02913                  MOVE -1         TO  JDTSENTL
02914                  MOVE AL-UABON   TO  JDTSENTA
02915                  PERFORM 9900-ERROR-FORMAT
02916              ELSE
02917                  MOVE AL-UANON   TO  JDTSENTA
02918                  MOVE +1         TO  WS-UPDATE-SW
02919                  MOVE DC-BIN-DATE-1  TO  WS-SEND-ON-DATE
02920                  IF WS-SEND-ON-DATE LESS THAN WS-CURRENT-DATE
02921                      MOVE ER-0551 TO  EMI-ERROR
02922                      MOVE -1      TO  JDTSENTL
02923                      MOVE AL-UABON TO  JDTSENTA
02924                      PERFORM 9900-ERROR-FORMAT
02925                  ELSE
02926                      NEXT SENTENCE
02927          ELSE
02928              MOVE ER-0550        TO  EMI-ERROR
02929              MOVE -1             TO  JDTSENTL
02930              MOVE AL-UABON       TO  JDTSENTA
02931              PERFORM 9900-ERROR-FORMAT.
02932
02933      IF JRESENDL GREATER ZERO
02934          IF JRESENDI = SPACES
02935              MOVE AL-UANON       TO  JRESENDA
02936              MOVE +1             TO  WS-UPDATE-SW
02937              MOVE LOW-VALUES     TO  WS-RESEND-DATE
02938          ELSE
02939              MOVE JRESENDI       TO  WS-DEEDIT-FIELD
02940              PERFORM 8600-DEEDIT
02941              IF WS-DEEDIT-FIELD-V0 IS NUMERIC
02942                  MOVE WS-DEEDIT-FIELD-V0  TO  JRESENDO
02943                  INSPECT JRESENDI CONVERTING SPACES TO SLASH
02944                  MOVE '4'                 TO  DC-OPTION-CODE
02945                  MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY
02946                  PERFORM 8500-DATE-CONVERSION
02947                  IF DC-ERROR-CODE NOT = SPACES
02948                      MOVE ER-0295           TO  EMI-ERROR
02949                      MOVE -1             TO  JRESENDL
02950                      MOVE AL-UABON       TO  JRESENDA
02951                      PERFORM 9900-ERROR-FORMAT
02952                  ELSE
02953                      MOVE AL-UANON       TO  JRESENDA
02954                      MOVE +1             TO  WS-UPDATE-SW
02955                      MOVE DC-BIN-DATE-1  TO  WS-RESEND-DATE
02956              ELSE
02957                  MOVE ER-0295        TO  EMI-ERROR
02958                  MOVE -1             TO  JRESENDL
02959                  MOVE AL-UABON       TO  JRESENDA
02960                  PERFORM 9900-ERROR-FORMAT.
02961
02962      IF JREPLYL GREATER ZERO
02963          IF JREPLYI = SPACES
02964              MOVE AL-UANON       TO  JREPLYA
02965              MOVE +1             TO  WS-UPDATE-SW
02966              MOVE LOW-VALUES     TO  WS-FOLLOW-UP-DATE
02967          ELSE
02968              MOVE JREPLYI        TO  WS-DEEDIT-FIELD
02969              PERFORM 8600-DEEDIT
02970              IF WS-DEEDIT-FIELD-V0 IS NUMERIC
02971                  MOVE WS-DEEDIT-FIELD-V0  TO  JREPLYO
02972                  INSPECT JREPLYI CONVERTING SPACES TO SLASH
02973                  MOVE '4'                 TO  DC-OPTION-CODE
02974                  MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY
02975                  PERFORM 8500-DATE-CONVERSION
02976                  IF DC-ERROR-CODE NOT = SPACES
02977                      MOVE ER-0296           TO  EMI-ERROR
02978                      MOVE -1             TO  JREPLYL
02979                      MOVE AL-UABON       TO  JREPLYA
02980                      PERFORM 9900-ERROR-FORMAT
02981                  ELSE
02982                      MOVE AL-UANON       TO  JREPLYA
02983                      MOVE +1             TO  WS-UPDATE-SW
02984                      MOVE DC-BIN-DATE-1  TO  WS-FOLLOW-UP-DATE
02985              ELSE
02986                  MOVE ER-0296        TO  EMI-ERROR
02987                  MOVE -1             TO  JREPLYL
02988                  MOVE AL-UABON       TO  JREPLYA
02989                  PERFORM 9900-ERROR-FORMAT.
02990
02991      IF JRECEVEL GREATER ZERO
02992          IF JRECEVEI = SPACES
02993              MOVE AL-UANON       TO  JRECEVEA
02994              MOVE +1             TO  WS-UPDATE-SW
02995              MOVE LOW-VALUES     TO  WS-RECEIVED-DATE
02996          ELSE
02997              MOVE JRECEVEI       TO  WS-DEEDIT-FIELD
02998              PERFORM 8600-DEEDIT
02999              IF WS-DEEDIT-FIELD-V0 IS NUMERIC
03000                  MOVE WS-DEEDIT-FIELD-V0  TO  JRECEVEO
03001                  INSPECT JRECEVEI CONVERTING SPACES TO SLASH
03002                  MOVE '4'                TO  DC-OPTION-CODE
03003                  MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY
03004                  PERFORM 8500-DATE-CONVERSION
03005                  IF DC-ERROR-CODE NOT = SPACES
03006                      MOVE ER-0297        TO  EMI-ERROR
03007                      MOVE -1             TO  JRECEVEL
03008                      MOVE AL-UABON       TO  JRECEVEA
03009                      PERFORM 9900-ERROR-FORMAT
03010                  ELSE
03011                      MOVE AL-UANON       TO  JRECEVEA
03012                      MOVE +1             TO  WS-UPDATE-SW
03013                      MOVE DC-BIN-DATE-1  TO  WS-RECEIVED-DATE
03014              ELSE
03015                  MOVE ER-0297        TO  EMI-ERROR
03016                  MOVE -1             TO  JRECEVEL
03017                  MOVE AL-UABON       TO  JRECEVEA
03018                  PERFORM 9900-ERROR-FORMAT.
03019
03020      IF JPHYRECL GREATER ZERO
03021          IF JPHYRECI = SPACES
03022              MOVE AL-UANON       TO  JPHYRECA
03023              MOVE +1             TO  WS-UPDATE-SW
03024              MOVE LOW-VALUES     TO  WS-RECEIVED-PHY-DATE
03025          ELSE
03026              MOVE JPHYRECI       TO  WS-DEEDIT-FIELD
03027              PERFORM 8600-DEEDIT
03028              IF WS-DEEDIT-FIELD-V0 IS NUMERIC
03029                  MOVE WS-DEEDIT-FIELD-V0  TO  JPHYRECO
03030                  INSPECT JPHYRECI CONVERTING SPACES TO SLASH
03031                  MOVE '4'                TO  DC-OPTION-CODE
03032                  MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY
03033                  PERFORM 8500-DATE-CONVERSION
03034                  IF DC-ERROR-CODE NOT = SPACES
03035                      MOVE ER-0297        TO  EMI-ERROR
03036                      MOVE -1             TO  JPHYRECL
03037                      MOVE AL-UABON       TO  JPHYRECA
03038                      PERFORM 9900-ERROR-FORMAT
03039                  ELSE
03040                      MOVE AL-UANON       TO  JPHYRECA
03041                      MOVE +1             TO  WS-UPDATE-SW
03042                      MOVE DC-BIN-DATE-1  TO  WS-RECEIVED-PHY-DATE
03043              ELSE
03044                  MOVE ER-0297        TO  EMI-ERROR
03045                  MOVE -1             TO  JPHYRECL
03046                  MOVE AL-UABON       TO  JPHYRECA
03047                  PERFORM 9900-ERROR-FORMAT.
03048
03049      IF JEMPRECL GREATER ZERO
03050          IF JEMPRECI = SPACES
03051              MOVE AL-UANON       TO  JEMPRECA
03052              MOVE +1             TO  WS-UPDATE-SW
03053              MOVE LOW-VALUES     TO  WS-RECEIVED-EMP-DATE
03054          ELSE
03055              MOVE JEMPRECI       TO  WS-DEEDIT-FIELD
03056              PERFORM 8600-DEEDIT
03057              IF WS-DEEDIT-FIELD-V0 IS NUMERIC
03058                  MOVE WS-DEEDIT-FIELD-V0  TO  JEMPRECO
03059                  INSPECT JEMPRECI CONVERTING SPACES TO SLASH
03060                  MOVE '4'                TO  DC-OPTION-CODE
03061                  MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY
03062                  PERFORM 8500-DATE-CONVERSION
03063                  IF DC-ERROR-CODE NOT = SPACES
03064                      MOVE ER-0297        TO  EMI-ERROR
03065                      MOVE -1             TO  JEMPRECL
03066                      MOVE AL-UABON       TO  JEMPRECA
03067                      PERFORM 9900-ERROR-FORMAT
03068                  ELSE
03069                      MOVE AL-UANON       TO  JEMPRECA
03070                      MOVE +1             TO  WS-UPDATE-SW
03071                      MOVE DC-BIN-DATE-1  TO  WS-RECEIVED-EMP-DATE
03072              ELSE
03073                  MOVE ER-0297        TO  EMI-ERROR
03074                  MOVE -1             TO  JEMPRECL
03075                  MOVE AL-UABON       TO  JEMPRECA
03076                  PERFORM 9900-ERROR-FORMAT.
03077
03078      IF JFORML GREATER ZERO
03079          IF JFORMI = 'INITIAL'  OR 'I' OR
03080                      'PROGRESS' OR 'P'
03081              MOVE AL-UANON       TO  JFORMA
03082          ELSE
03083              MOVE ER-0532        TO  EMI-ERROR
03084              MOVE AL-UABON       TO  JFORMA
03085              MOVE -1             TO  JFORML
03086              PERFORM 9900-ERROR-FORMAT.
03087
03088      IF JSI1L GREATER ZERO         OR
03089         JSI2L GREATER ZERO         OR
03090         JSI3L GREATER ZERO         OR
03091         JCARR1L GREATER ZERO       OR
03092         JCLAIM1L GREATER ZERO      OR
03093         JCERT1L GREATER ZERO       OR
03094         JCARR2L GREATER ZERO       OR
03095         JCLAIM2L GREATER ZERO      OR
03096         JCERT2L GREATER ZERO
03097          MOVE +1                 TO  WS-UPDATE-SW.
03098
03099      IF WS-ERROR-COUNT GREATER ZERO
03100          PERFORM 8200-SEND-DATAONLY.
03101
03102      IF WS-UPDATE-SW NOT GREATER ZERO
03103          PERFORM 4000-READ-TRAILER-FILE.
03104
03105      IF JREPLYL GREATER ZERO
03106          PERFORM 3300-UPDATE-CLAIM-MASTER.
03107
03108      PERFORM 3000-READ-FOR-UPDATE.
03109
03110      IF JDTSENTL GREATER ZERO
03111          MOVE WS-SEND-ON-DATE    TO  AT-FORM-SEND-ON-DT
03112          MOVE LOW-VALUES         TO  AT-FORM-PRINTED-DT.
03113
03114      IF JRESENDL GREATER ZERO
03115          MOVE WS-RESEND-DATE     TO  AT-FORM-RE-SEND-DT.
03116
03117      IF JREPLYL GREATER ZERO
03118          MOVE WS-FOLLOW-UP-DATE  TO  AT-FORM-FOLLOW-UP-DT.
03119
03120      IF JRECEVEL GREATER ZERO
03121          MOVE WS-RECEIVED-DATE   TO  AT-FORM-ANSWERED-DT.
03122
03123      IF JPHYRECL GREATER ZERO
03124          MOVE WS-RECEIVED-PHY-DATE   TO  AT-PHY-FORM-ANSWERED-DT.
03125
03126      IF JEMPRECL GREATER ZERO
03127          MOVE WS-RECEIVED-EMP-DATE   TO  AT-EMP-FORM-ANSWERED-DT.
03128
03129      IF JFORML GREATER ZERO
03130          MOVE JFORMI             TO  AT-FORM-TYPE
03131          INSPECT AT-FORM-TYPE CONVERTING 'IP' TO '12'.
03132
03133      IF JSI1L GREATER ZERO
03134          MOVE JSI1I              TO  AT-INSTRUCT-LN-1.
03135
03136      IF JSI2L GREATER ZERO
03137          MOVE JSI2I              TO  AT-INSTRUCT-LN-2.
03138
03139      IF JSI3L GREATER ZERO
03140          MOVE JSI3I              TO  AT-INSTRUCT-LN-3.
03141
03142      IF AT-INSTRUCT-LN-2 = SPACES
03143          MOVE AT-INSTRUCT-LN-3   TO  AT-INSTRUCT-LN-2
03144          MOVE SPACES             TO  AT-INSTRUCT-LN-3.
03145
03146      IF AT-INSTRUCT-LN-1 = SPACES
03147          MOVE AT-INSTRUCT-LN-2   TO  AT-INSTRUCT-LN-1
03148          MOVE AT-INSTRUCT-LN-3   TO  AT-INSTRUCT-LN-2
03149          MOVE SPACES             TO  AT-INSTRUCT-LN-3.
03150
03151      IF JCARR1L GREATER ZERO
03152          MOVE JCARR1I            TO  AT-REL-CARR-1.
03153
03154      IF JCLAIM1L GREATER ZERO
03155          MOVE JCLAIM1I           TO  AT-REL-CLAIM-1.
03156
03157      IF JCERT1L GREATER ZERO
03158          MOVE JCERT1I            TO  AT-REL-CERT-1.
03159
03160      IF JCARR2L GREATER ZERO
03161          MOVE JCARR2I            TO  AT-REL-CARR-2.
03162
03163      IF JCLAIM2L GREATER ZERO
03164          MOVE JCLAIM2I           TO  AT-REL-CLAIM-2.
03165
03166      IF JCERT2L GREATER ZERO
03167          MOVE JCERT2I            TO  AT-REL-CERT-2.
03168
03169      MOVE PI-PROCESSOR-ID        TO  AT-FORM-LAST-UPDATED-BY.
03170
03171      MOVE WS-CURRENT-DATE        TO  AT-FORM-LAST-MAINT-DT.
03172
03173      PERFORM 3100-REWRITE.
03174      MOVE +1                     TO  WS-COMPLETED-SUCCESSFUL.
03175
03176      MOVE 'S'                    TO  JMAINTO.
03177      MOVE -1                     TO  JMAINTL.
03178      MOVE AL-UANOF               TO  JMAINTA.
03179      PERFORM 8200-SEND-DATAONLY.
03180
03181      EJECT
03182  1100-MAIN-LOGIC.
03183      IF PI-MAP-NAME NOT = EL142B2
03184          GO TO 1200-MAIN-LOGIC.
03185
03186      IF NOT MODIFY-CAP
03187          IF KMAINTI = 'S'
03188              NEXT SENTENCE
03189          ELSE
03190              MOVE 'UPDATE'       TO SM-READ
03191              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
03192              MOVE ER-0070        TO  EMI-ERROR
03193              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03194              GO TO 8100-SEND-INITIAL-MAP.
03195
03196      MOVE +3                     TO  EMI-NUMBER-OF-LINES.
03197      MOVE +2                     TO  EMI-SWITCH2.
03198
03199      IF KMAINTL NOT GREATER ZERO
03200        OR (KMAINTL GREATER ZERO AND
03201            KMAINTI = 'S')
03202              MOVE PI-SAVE-KEY    TO  PI-ACTIVITY-TRAILERS-KEY
03203              PERFORM 4000-READ-TRAILER-FILE.
03204
062602     if kmainti = 'C' or 'D' or 'A'
020816        if pi-company-id = 'CID' or 'DCC' OR 'AHL' OR 'VPP'
062121           OR 'FNL'
062602           if (pi-el142-priority = '8')
120718           AND (PI-approval-level <> '5')
062602              MOVE ER-8003       TO EMI-ERROR
062602              MOVE -1            TO kMAINTL
062602              MOVE AL-UABON      TO kMAINTA
062602              GO TO 8200-SEND-DATAONLY
062602           end-if
062602        end-if
062602     end-if
062602
03205      IF KMAINTI NOT = 'D'
03206          GO TO 1120-MAIN-LOGIC.
03207
03208      PERFORM 3000-READ-FOR-UPDATE.
03209
03210      MOVE PI-COMPANY-CD          TO  WS-CQ-COMPANY-CD.
03211      MOVE AT-CHECK-QUE-CONTROL   TO  WS-CQ-CONTROL-NUMBER.
03212      MOVE AT-CHECK-QUE-SEQUENCE  TO  WS-CQ-SEQUENCE-NUMBER.
03213
03214      
      * EXEC CICS READ UPDATE
03215 *        DATASET (WS-CHECK-QUEUE-DSID)
03216 *        RIDFLD  (WS-CHECK-QUEUE-KEY)
03217 *        SET     (ADDRESS OF CHECK-QUE)
03218 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00012002' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132303032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CHECK-QUEUE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03219
03220      
      * EXEC CICS DELETE
03221 *        DATASET (WS-CHECK-QUEUE-DSID)
03222 *    END-EXEC.
      *    MOVE '&(                    &   #00012008' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303132303038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03223
03224      MOVE +99999999              TO  AT-CHECK-QUE-CONTROL.
03225      MOVE ZERO                   TO  AT-CHECK-QUE-SEQUENCE.
03226
03227      PERFORM 3100-REWRITE.
03228
03229      MOVE ER-ZERO                TO  EMI-ERROR.
03230      PERFORM 9900-ERROR-FORMAT.
03231
03232      MOVE PI-SAVE-KEY  TO  PI-ACTIVITY-TRAILERS-KEY.
03233      PERFORM 4000-READ-TRAILER-FILE.
03234
03235  1120-MAIN-LOGIC.
03236      IF KMAINTI = 'A' OR 'C'
03237          NEXT SENTENCE
03238      ELSE
03239          MOVE ER-0023            TO  EMI-ERROR
03240          MOVE -1                 TO  KMAINTL
03241          MOVE AL-UABON           TO  KMAINTA
03242          PERFORM 8200-SEND-DATAONLY.
03243
03244      IF KMAINTI NOT = 'C'
03245          GO TO 1150-MAIN-LOGIC.
03246
03247      IF KTIMPRTL GREATER ZERO
03248          
      * EXEC CICS BIF DEEDIT
03249 *            FIELD  (KTIMPRTI)
03250 *            LENGTH (4)
03251 *        END-EXEC
           MOVE 4
             TO DFHEIV11
      *    MOVE '@"L                   #   #00012036' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303132303336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 KTIMPRTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
03252          IF KTIMPRTI NOT NUMERIC
03253              MOVE -1             TO  KTIMPRTL
03254              MOVE AL-UNBON       TO  KTIMPRTA
03255              MOVE ER-0579        TO  EMI-ERROR
03256              PERFORM 9900-ERROR-FORMAT
03257          ELSE
03258              MOVE KTIMPRTI       TO  KTIMPRTO
03259              MOVE +1             TO  WS-UPDATE-SW.
03260
03261      IF WS-ERROR-COUNT GREATER ZERO
03262          PERFORM 8200-SEND-DATAONLY.
03263
03264      
      * EXEC CICS READ
03265 *        DATASET (WS-ACTIVITY-TRAILERS-DSID)
03266 *        RIDFLD  (PI-SAVE-KEY)
03267 *        SET     (ADDRESS OF ACTIVITY-TRAILERS)
03268 *    END-EXEC.
      *    MOVE '&"S        E          (   #00012052' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132303532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-SAVE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03269
03270      MOVE PI-COMPANY-CD          TO  WS-CQ-COMPANY-CD.
03271      MOVE AT-CHECK-QUE-CONTROL   TO  WS-CQ-CONTROL-NUMBER.
03272      MOVE AT-CHECK-QUE-SEQUENCE  TO  WS-CQ-SEQUENCE-NUMBER.
03273
03274      
      * EXEC CICS READ UPDATE
03275 *        DATASET (WS-CHECK-QUEUE-DSID)
03276 *        RIDFLD  (WS-CHECK-QUEUE-KEY)
03277 *        SET     (ADDRESS OF CHECK-QUE)
03278 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00012062' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132303632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CHECK-QUEUE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03279
03280      IF KTIMPRTL GREATER ZERO
03281          
      * EXEC CICS BIF DEEDIT
03282 *            FIELD  (KTIMPRTI)
03283 *            LENGTH (4)
03284 *        END-EXEC
           MOVE 4
             TO DFHEIV11
      *    MOVE '@"L                   #   #00012069' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303132303639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 KTIMPRTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
03285          MOVE KTIMPRTI           TO  CQ-TIMES-PRINTED
03286                                      KTIMPRTO.
03287
03288      
      * EXEC CICS REWRITE
03289 *        DATASET (WS-CHECK-QUEUE-DSID)
03290 *        FROM    (CHECK-QUE)
03291 *    END-EXEC.
           MOVE LENGTH OF
            CHECK-QUE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00012076' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303132303736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 CHECK-QUE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03292
03293      MOVE ER-ZERO                TO  EMI-ERROR.
03294      PERFORM 9900-ERROR-FORMAT.
03295
03296      MOVE -1                     TO  KMAINTL.
03297
03298      PERFORM 8200-SEND-DATAONLY.
03299
03300  1150-MAIN-LOGIC.
03301      IF KCONTRLL GREATER ZERO
03302          
      * EXEC CICS BIF DEEDIT
03303 *            FIELD  (KCONTRLI)
03304 *            LENGTH (8)
03305 *        END-EXEC
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00012090' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303132303930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 KCONTRLI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
03306          IF KCONTRLI NOT NUMERIC
03307              MOVE -1             TO  KCONTRLL
03308              MOVE AL-UNBON       TO  KCONTRLA
03309              MOVE ER-0580        TO  EMI-ERROR
03310              PERFORM 9900-ERROR-FORMAT
03311          ELSE
03312              MOVE KCONTRLI       TO  KCONTRLO
03313              MOVE +1             TO  WS-UPDATE-SW.
03314
03315      IF KSEQL GREATER ZERO
03316          
      * EXEC CICS BIF DEEDIT
03317 *            FIELD  (KSEQI)
03318 *            LENGTH (4)
03319 *        END-EXEC
           MOVE 4
             TO DFHEIV11
      *    MOVE '@"L                   #   #00012104' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303132313034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 KSEQI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
03320          IF KSEQI NOT NUMERIC
03321              MOVE -1             TO  KSEQL
03322              MOVE AL-UNBON       TO  KSEQA
03323              MOVE ER-0581        TO  EMI-ERROR
03324              PERFORM 9900-ERROR-FORMAT
03325          ELSE
03326              MOVE KSEQI          TO  KSEQO
03327              MOVE +1             TO  WS-UPDATE-SW.
03328
03329      IF KTIMPRTL GREATER ZERO
03330          
      * EXEC CICS BIF DEEDIT
03331 *            FIELD  (KTIMPRTI)
03332 *            LENGTH (4)
03333 *        END-EXEC
           MOVE 4
             TO DFHEIV11
      *    MOVE '@"L                   #   #00012118' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303132313138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 KTIMPRTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
03334          IF KTIMPRTI NOT NUMERIC
03335              MOVE -1             TO  KTIMPRTL
03336              MOVE AL-UNBON       TO  KTIMPRTA
03337              MOVE ER-0579           TO  EMI-ERROR
03338              PERFORM 9900-ERROR-FORMAT
03339          ELSE
03340              MOVE KTIMPRTI       TO  KTIMPRTO
03341              MOVE +1             TO  WS-UPDATE-SW.
03342
03343      IF WS-ERROR-COUNT GREATER ZERO
03344          PERFORM 8200-SEND-DATAONLY.
03345
03346      IF KCONTRLL GREATER ZERO
03347        OR KSEQL GREATER ZERO
03348          PERFORM 3000-READ-FOR-UPDATE
03349      ELSE
03350          
      * EXEC CICS READ
03351 *            DATASET (WS-ACTIVITY-TRAILERS-DSID)
03352 *            RIDFLD  (PI-SAVE-KEY)
03353 *            SET     (ADDRESS OF ACTIVITY-TRAILERS)
03354 *        END-EXEC.
      *    MOVE '&"S        E          (   #00012138' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132313338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-SAVE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03355
03356      MOVE PI-COMPANY-CD          TO  WS-CQ-COMPANY-CD.
03357      MOVE AT-CHECK-QUE-CONTROL   TO  WS-CQ-CONTROL-NUMBER.
03358      MOVE AT-CHECK-QUE-SEQUENCE  TO  WS-CQ-SEQUENCE-NUMBER.
03359
03360      
      * EXEC CICS GETMAIN
03361 *        LENGTH  (WS-CHECK-QUEUE-LENGTH)
03362 *        INITIMG (WS-SPACES)
03363 *        SET     (ADDRESS OF CHECK-QUE)
03364 *    END-EXEC.
      *    MOVE ',"IL                  $   #00012148' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303132313438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 WS-CHECK-QUEUE-LENGTH, 
                 WS-SPACES
           SET ADDRESS OF CHECK-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03365
03366      MOVE 'CQ'                   TO  CQ-RECORD-ID.
03367      MOVE PI-COMPANY-CD          TO  CQ-COMPANY-CD.
03368      MOVE AT-CHECK-QUE-CONTROL   TO  CQ-CONTROL-NUMBER.
03369      MOVE AT-CHECK-QUE-SEQUENCE  TO  CQ-SEQUENCE-NUMBER.
03370
03371      MOVE 'Q'                    TO  CQ-ENTRY-TYPE.
03372      MOVE AT-CARRIER             TO  CQ-CARRIER.
03373      MOVE AT-CLAIM-NO            TO  CQ-CLAIM-NO.
03374      MOVE AT-CERT-NO             TO  CQ-CERT-NO.
03375      MOVE AT-CLAIM-TYPE          TO  CQ-CLAIM-TYPE.
03376      MOVE AT-CLAIM-PREM-TYPE     TO  CQ-CLAIM-SUB-TYPE.
03377
03378      MOVE AT-SEQUENCE-NO         TO  CQ-PMT-TRLR-SEQUENCE.
03379      MOVE AT-CHECK-NO            TO  CQ-CHECK-NUMBER.
03380      MOVE AT-AMOUNT-PAID         TO  CQ-CHECK-AMOUNT.
03381      MOVE AT-PAYMENT-TYPE        TO  CQ-PAYMENT-TYPE.
03382
03383      IF AT-VOID-DT NOT = LOW-VALUES
03384          MOVE 'V'                TO  CQ-VOID-INDICATOR.
03385
03386      MOVE ZERO                   TO  CQ-TIMES-PRINTED
03387                                      CQ-PRINT-AT-HHMM.
03388      MOVE AT-RECORDED-BY         TO  CQ-CHECK-BY-USER.
03389      MOVE AT-CHECK-WRITTEN-DT    TO  CQ-CHECK-WRITTEN-DT.
03390      MOVE +1420                  TO  CQ-LAST-UPDATED-BY.
03391
03392      IF KCONTRLL GREATER ZERO
03393          
      * EXEC CICS BIF DEEDIT
03394 *            FIELD  (KCONTRLI)
03395 *            LENGTH (8)
03396 *        END-EXEC
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00012181' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303132313831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 KCONTRLI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
03397          MOVE KCONTRLI           TO  CQ-CONTROL-NUMBER
03398                                      AT-CHECK-QUE-CONTROL
03399                                      KCONTRLO.
03400      IF KSEQL GREATER ZERO
03401          
      * EXEC CICS BIF DEEDIT
03402 *            FIELD  (KSEQI)
03403 *            LENGTH (4)
03404 *        END-EXEC
           MOVE 4
             TO DFHEIV11
      *    MOVE '@"L                   #   #00012189' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303132313839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 KSEQI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
03405          MOVE KSEQI              TO  CQ-SEQUENCE-NUMBER
03406                                      AT-CHECK-QUE-SEQUENCE
03407                                      KSEQO.
03408      IF KTIMPRTL GREATER ZERO
03409          
      * EXEC CICS BIF DEEDIT
03410 *            FIELD  (KTIMPRTI)
03411 *            LENGTH (4)
03412 *        END-EXEC
           MOVE 4
             TO DFHEIV11
      *    MOVE '@"L                   #   #00012197' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303132313937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 KTIMPRTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
03413          MOVE KTIMPRTI           TO  CQ-TIMES-PRINTED
03414                                      KTIMPRTO.
03415
03416      
      * EXEC CICS WRITE
03417 *        DATASET (WS-CHECK-QUEUE-DSID)
03418 *        FROM    (CHECK-QUE)
03419 *        RIDFLD  (CQ-CONTROL-PRIMARY)
03420 *    END-EXEC.
           MOVE LENGTH OF
            CHECK-QUE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00012204' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303132323034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 CHECK-QUE, 
                 DFHEIV11, 
                 CQ-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03421
03422      IF KCONTRLL GREATER ZERO OR
03423         KSEQL    GREATER ZERO
03424          PERFORM 3100-REWRITE.
03425
03426      MOVE ER-ZERO                TO  EMI-ERROR.
03427      PERFORM 9900-ERROR-FORMAT.
03428
03429      MOVE SPACES                 TO  KMAINTO.
03430      MOVE AL-UANOF               TO  KMAINTA.
03431      MOVE -1                     TO  KMAINTL.
03432
03433      PERFORM 8200-SEND-DATAONLY.
03434
03435      EJECT
03436  1200-MAIN-LOGIC.
03437      IF PI-MAP-NAME = EL142D2
102510*03438          GO TO 0015-MAIN-LOGIC.
102510         MOVE EL142D TO PI-MAP-NAME
102510         IF PI-END-OF-FILE = ZERO
102510             SUBTRACT +1 FROM PI-ATK-SEQUENCE-NO
102510             ADD +1 TO PI-PREV-ATK-SEQUENCE-NO
102510         END-IF
102510         MOVE ZERO TO PI-END-OF-FILE
102510         PERFORM 4000-READ-TRAILER-FILE
102510     END-IF.
03439
03440      MOVE '1200-MAIN-LOGIC REACHED'  TO  LOGOFF-MSG.
03441      PERFORM 8300-SEND-TEXT.
03442
03443      EJECT
03444  3000-READ-FOR-UPDATE SECTION.
03445
03446      
      * EXEC CICS READ UPDATE
03447 *        DATASET (WS-ACTIVITY-TRAILERS-DSID)
03448 *        RIDFLD  (PI-SAVE-KEY)
03449 *        SET     (ADDRESS OF ACTIVITY-TRAILERS)
03450 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00012242' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132323432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-SAVE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03451
03452      IF AT-INFO-TRAILER-TYPE = 'M'
03453          MOVE ER-0969        TO EMI-ERROR
03454          MOVE -1             TO EMAINTL
03455          MOVE AL-UABON       TO EMAINTA
03456          
      * EXEC CICS UNLOCK
03457 *            DATASET (WS-ACTIVITY-TRAILERS-DSID)
03458 *        END-EXEC
      *    MOVE '&*                    #   #00012252' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303132323532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
03459          GO TO 8200-SEND-DATAONLY.
03460
03461      IF AT-RECORDED-BY NOT = PI-UPDATE-BY
03462        OR AT-LAST-MAINT-HHMMSS NOT = PI-UPDATE-HHMMSS
03463          MOVE AL-UABON           TO  HMAINTA
03464          MOVE -1                 TO  HMAINTL
03465          MOVE ER-0068            TO  EMI-ERROR
03466          
      * EXEC CICS UNLOCK
03467 *            DATASET (WS-ACTIVITY-TRAILERS-DSID)
03468 *        END-EXEC
      *    MOVE '&*                    #   #00012262' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303132323632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
03469          PERFORM 8200-SEND-DATAONLY.
03470
03471  3000-EXIT.
03472      EXIT.
03473
03474      EJECT
03475  3100-REWRITE SECTION.
03476
03477      MOVE PI-PROCESSOR-ID        TO  PI-UPDATE-BY
03478                                      BMANTBYO.
03479
03480      MOVE AT-RECORDED-DT             TO  DC-BIN-DATE-1.
03481      MOVE ' '                        TO  DC-OPTION-CODE.
03482      PERFORM 8500-DATE-CONVERSION.
03483      IF NO-CONVERSION-ERROR
03484          MOVE DC-GREG-DATE-1-EDIT    TO  BRECDTEO
03485      ELSE
03486          MOVE SPACES                 TO  BRECDTEO.
03487
03488      MOVE EIBTIME                TO  AT-LAST-MAINT-HHMMSS
03489                                      PI-UPDATE-HHMMSS.
03490
03491      
      * EXEC CICS REWRITE
03492 *        DATASET (WS-ACTIVITY-TRAILERS-DSID)
03493 *        FROM    (ACTIVITY-TRAILERS)
03494 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00012287' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303132323837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03495
03496      MOVE PI-SAVE-KEY            TO  WS-CLAIM-KEY.
03497
03498      PERFORM 3500-READ-ELMSTR-FOR-UPDATE.
03499
03500      PERFORM 3600-REWRITE-ELMSTR.
03501
03502  3100-EXIT.
03503      EXIT.
03504
03505      EJECT
03506  3200-DELETE SECTION.
03507
03508      
      * EXEC CICS DELETE
03509 *        DATASET (WS-ACTIVITY-TRAILERS-DSID)
03510 *    END-EXEC.
      *    MOVE '&(                    &   #00012304' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303132333034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03511
03512      MOVE PI-SAVE-KEY            TO  WS-CLAIM-KEY.
03513
03514      
      * EXEC CICS READ
03515 *        DATASET (WS-CLAIM-MASTER-DSID)
03516 *        RIDFLD  (WS-CLAIM-KEY)
03517 *        SET     (ADDRESS OF CLAIM-MASTER)
03518 *    END-EXEC.
      *    MOVE '&"S        E          (   #00012310' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132333130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CLAIM-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CLAIM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03519
03520      IF CL-TRAILER-SEQ-CNT NOT = PI-SAVE-ATK-SEQUENCE-NO
03521          GO TO 3200-EXIT.
03522
03523      PERFORM 3500-READ-ELMSTR-FOR-UPDATE.
03524
03525      
      * EXEC CICS HANDLE CONDITION
03526 *        NOTFND (3200-NOT-FOUND)
03527 *    END-EXEC.
      *    MOVE '"$I                   ! $ #00012321' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303132333231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03528
03529      
      * EXEC CICS READ
03530 *        DATASET (WS-ACTIVITY-TRAILERS-DSID)
03531 *        RIDFLD  (PI-SAVE-KEY)
03532 *        SET     (ADDRESS OF ACTIVITY-TRAILERS)
03533 *        GTEQ
03534 *    END-EXEC.
      *    MOVE '&"S        G          (   #00012325' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132333235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-SAVE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03535
03536      IF PI-SAVE-ATK-COMPANY-CODE = AT-COMPANY-CD    AND
03537         PI-SAVE-ATK-CARRIER      = AT-CARRIER       AND
03538         PI-SAVE-ATK-CLAIM-NO     = AT-CLAIM-NO      AND
03539         PI-SAVE-ATK-CERT-NO      = AT-CERT-NO
03540          MOVE AT-SEQUENCE-NO           TO  CL-TRAILER-SEQ-CNT
03541          GO TO 3200-REWRITE-CLAIM-MASTER.
03542
03543  3200-NOT-FOUND.
03544      MOVE +4095                  TO  CL-TRAILER-SEQ-CNT.
03545
03546  3200-REWRITE-CLAIM-MASTER.
03547      PERFORM 3600-REWRITE-ELMSTR.
03548
03549  3200-EXIT.
03550      EXIT.
03551
03552      EJECT
03553  3300-UPDATE-CLAIM-MASTER SECTION.
03554      MOVE PI-SAVE-KEY            TO  WS-CLAIM-KEY.
03555
03556      
      * EXEC CICS READ
03557 *        DATASET (WS-CLAIM-MASTER-DSID)
03558 *        RIDFLD  (WS-CLAIM-KEY)
03559 *        SET     (ADDRESS OF CLAIM-MASTER)
03560 *    END-EXEC.
      *    MOVE '&"S        E          (   #00012352' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132333532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CLAIM-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CLAIM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03561
03562      IF WS-FOLLOW-UP-DATE NOT LESS THAN CL-NEXT-FOLLOWUP-DT
03563          GO TO 3300-EXIT.
03564
03565      PERFORM 3500-READ-ELMSTR-FOR-UPDATE.
03566
03567      MOVE WS-FOLLOW-UP-DATE      TO  CL-NEXT-FOLLOWUP-DT.
03568
03569      PERFORM 3600-REWRITE-ELMSTR.
03570
03571  3300-EXIT.
03572      EXIT.
03573
03574      EJECT
03575  3400-DELETE-FORM-ARCHIVE SECTION.
03576      
      * EXEC CICS HANDLE CONDITION
03577 *        ENDFILE (3490-DELETE-FORM-ARCHIVE)
03578 *        NOTFND  (3499-EXIT)
03579 *    END-EXEC
      *    MOVE '"$''I                  ! % #00012372' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303132333732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
03580
03581      MOVE LOW-VALUES             TO  WS-LETTER-ARCHIVE-ALT-KEY.
03582
03583      MOVE PI-COMPANY-CD          TO  WS-LA-ALT-COMPANY-CD.
03584      MOVE '4'                    TO  WS-LA-ALT-RECORD-TYPE.
03585
03586      
      * EXEC CICS STARTBR
03587 *        DATASET (WS-LETTER-ARCHIVE-DSID2)
03588 *        RIDFLD  (WS-LETTER-ARCHIVE-ALT-KEY)
03589 *        GTEQ
03590 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00012382' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303132333832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-LETTER-ARCHIVE-DSID2, 
                 WS-LETTER-ARCHIVE-ALT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03591
03592  3410-DELETE-FORM-ARCHIVE.
03593      
      * EXEC CICS READNEXT
03594 *        DATASET (WS-LETTER-ARCHIVE-DSID2)
03595 *        RIDFLD  (WS-LETTER-ARCHIVE-ALT-KEY)
03596 *        SET     (ADDRESS OF LETTER-ARCHIVE)
03597 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00012389' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303132333839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-LETTER-ARCHIVE-DSID2, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-LETTER-ARCHIVE-ALT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03598
03599      IF LA-COMPANY-CD NOT = PI-COMPANY-CD
03600          GO TO 3490-DELETE-FORM-ARCHIVE.
03601
03602      IF LA-RECORD-TYPE NOT = '4'
03603          GO TO 3490-DELETE-FORM-ARCHIVE.
03604
03605      IF LA-CARRIER  NOT = PI-SAVE-ATK-CARRIER   OR
03606         LA-CLAIM-NO NOT = PI-SAVE-ATK-CLAIM-NO  OR
03607         LA-CERT-NO  NOT = PI-SAVE-ATK-CERT-NO
03608          GO TO 3410-DELETE-FORM-ARCHIVE.
03609
03610      MOVE LA-COMPANY-CD          TO WS-LA-COMPANY-CD.
03611      MOVE LA-ARCHIVE-NO          TO WS-LA-ARCHIVE-NO.
03612      MOVE LA-RECORD-TYPE         TO WS-LA-RECORD-TYPE.
03613      MOVE LA-LINE-SEQ-NO         TO WS-LA-LINE-SEQ-NO.
03614
03615      
      * EXEC CICS ENDBR
03616 *        DATASET (WS-LETTER-ARCHIVE-DSID2)
03617 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00012411' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303132343131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-LETTER-ARCHIVE-DSID2, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03618
03619      
      * EXEC CICS READ UPDATE
03620 *        DATASET (WS-LETTER-ARCHIVE-DSID)
03621 *        RIDFLD  (WS-LETTER-ARCHIVE-KEY)
03622 *        SET     (ADDRESS OF LETTER-ARCHIVE)
03623 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00012415' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132343135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-LETTER-ARCHIVE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-LETTER-ARCHIVE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03624
03625      
      * EXEC CICS DELETE
03626 *        DATASET (WS-LETTER-ARCHIVE-DSID)
03627 *    END-EXEC.
      *    MOVE '&(                    &   #00012421' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303132343231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-LETTER-ARCHIVE-DSID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03628
03629      GO TO 3499-EXIT.
03630
03631  3490-DELETE-FORM-ARCHIVE.
03632      
      * EXEC CICS ENDBR
03633 *        DATASET (WS-LETTER-ARCHIVE-DSID2)
03634 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00012428' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303132343238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-LETTER-ARCHIVE-DSID2, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03635
03636  3499-EXIT.
03637      EXIT.
03638
03639      EJECT
03640  3500-READ-ELMSTR-FOR-UPDATE SECTION.
03641      MOVE PI-SAVE-KEY            TO  WS-CLAIM-KEY.
03642
03643      
      * EXEC CICS READ UPDATE
03644 *        DATASET (WS-CLAIM-MASTER-DSID)
03645 *        RIDFLD  (WS-CLAIM-KEY)
03646 *        SET     (ADDRESS OF CLAIM-MASTER)
03647 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00012439' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132343339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CLAIM-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CLAIM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03648
03649  3599-EXIT.
03650      EXIT.
03651
03652  3600-REWRITE-ELMSTR SECTION.
03653      
      * EXEC CICS HANDLE CONDITION
03654 *        DUPKEY (3699-EXIT)
03655 *    END-EXEC.
      *    MOVE '"$$                   ! & #00012449' TO DFHEIV0
           MOVE X'222424202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303132343439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03656
03657      MOVE WS-CURRENT-DATE        TO  CL-LAST-MAINT-DT.
03658      MOVE EIBTIME                TO  CL-LAST-MAINT-HHMMSS.
03659      MOVE PI-PROCESSOR-ID        TO  CL-LAST-MAINT-USER.
03660      MOVE '3'                    TO  CL-LAST-MAINT-TYPE.
03661
03662      
      * EXEC CICS REWRITE
03663 *        DATASET (WS-CLAIM-MASTER-DSID)
03664 *        FROM    (CLAIM-MASTER)
03665 *    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00012458' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303132343538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CLAIM-MASTER-DSID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03666
03667  3699-EXIT.
03668      EXIT.
03669
03670      EJECT
03671  4000-READ-TRAILER-FILE SECTION.
03672      IF ((EIBAID = (DFHENTER OR DFHPF1) AND
03673          PI-PREV-AID = (DFHENTER OR DFHPF1))
03674        OR
03675         (EIBAID = DFHPF2 AND
03676          PI-PREV-AID = DFHPF2)
03677        OR
03678          PI-RECORD-COUNT = +1)
03679        AND
03680          PI-END-OF-FILE NOT = ZERO
03681              GO TO 0015-MAIN-LOGIC.
03682
03683      IF PI-END-OF-FILE NOT = ZERO
03684          IF EIBAID = DFHPF2
03685              MOVE PI-SAVE-KEY    TO  PI-PREV-ACTIVITY-TRAILERS-KEY
03686          ELSE
03687              MOVE PI-SAVE-KEY    TO  PI-ACTIVITY-TRAILERS-KEY.
03688
03689      MOVE LOW-VALUES             TO  EL142DO.
03690
03691      IF EIBAID = DFHPF2
03692          
      * EXEC CICS STARTBR
03693 *            DATASET (WS-ACTIVITY-TRAILERS-DSID)
03694 *            RIDFLD  (PI-PREV-ACTIVITY-TRAILERS-KEY)
03695 *            EQUAL
03696 *        END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         E          &   #00012488' TO DFHEIV0
           MOVE X'262C20202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303132343838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 PI-PREV-ACTIVITY-TRAILERS-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
03697        ELSE
03698          
      * EXEC CICS STARTBR
03699 *            DATASET (WS-ACTIVITY-TRAILERS-DSID)
03700 *            RIDFLD  (PI-ACTIVITY-TRAILERS-KEY)
03701 *            GTEQ
03702 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00012494' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303132343934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 PI-ACTIVITY-TRAILERS-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03703
03704      EJECT
03705  4100-READNEXT.
03706      IF EIBAID = DFHPF2
03707          MOVE PI-PREV-ACTIVITY-TRAILERS-KEY
03708                                  TO  PI-ACTIVITY-TRAILERS-KEY
03709          
      * EXEC CICS READPREV
03710 *            DATASET (WS-ACTIVITY-TRAILERS-DSID)
03711 *            RIDFLD  (PI-PREV-ACTIVITY-TRAILERS-KEY)
03712 *            SET     (ADDRESS OF ACTIVITY-TRAILERS)
03713 *        END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00012505' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303132353035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-PREV-ACTIVITY-TRAILERS-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
03714      ELSE
03715          MOVE PI-ACTIVITY-TRAILERS-KEY
03716                                  TO  PI-PREV-ACTIVITY-TRAILERS-KEY
03717          
      * EXEC CICS READNEXT
03718 *            DATASET (WS-ACTIVITY-TRAILERS-DSID)
03719 *            RIDFLD  (PI-ACTIVITY-TRAILERS-KEY)
03720 *            SET     (ADDRESS OF ACTIVITY-TRAILERS)
03721 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00012513' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303132353133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ACTIVITY-TRAILERS-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03722
CIDMOD     IF AT-SEQUENCE-NO = +90 OR +91
03724          GO TO 4100-READNEXT.
03725
03726      IF AT-TRAILER-TYPE = '6'
03727          IF AT-PAYMENT-NOTE
03728              GO TO 4100-READNEXT.
03729
03730      ADD +1  TO  WS-RECORD-COUNT.
03731      IF WS-RECORD-COUNT GREATER +0
03732         DIVIDE WS-RECORD-COUNT BY +10 GIVING WS-RECORD-DIV
101807               REMAINDER WS-RECORD-REMAINDER
101807     END-IF
101807*       IF WS-RECORD-REMAINDER = +0
101807*          EXEC CICS DELAY
101807*               INTERVAL(00001)
101807*          END-EXEC.
03738      EJECT
03739      IF AT-COMPANY-CD NOT = PI-COMPANY-CD OR
03740         AT-CARRIER    NOT = PI-CARRIER    OR
03741         AT-CLAIM-NO   NOT = PI-CLAIM-NO   OR
03742         AT-CERT-NO    NOT = PI-CERT-NO
03743          GO TO 6000-END-OF-FILE.
03744
03745      IF AT-RECORDED-DT LESS THAN PI-AFTER-DATE
03746         GO TO 4100-READNEXT.
03747
03748      IF ((EIBAID NOT = DFHPF2 AND
03749           PI-PREV-AID = DFHPF2)
03750         OR
03751          (EIBAID = DFHPF2 AND
03752           PI-PREV-AID NOT = DFHPF2)
03753         OR
03754           PI-END-OF-FILE = +1)
03755        AND
03756          WS-RECORD-COUNT NOT GREATER +1
03757              GO TO 4100-READNEXT.
03758
03759      MOVE ZERO                   TO  PI-END-OF-FILE.
03760
03761      IF (AT-TRAILER-TYPE = '1' AND
03762          PI-RES-EXP-SW = +1)
03763        OR
03764         (AT-TRAILER-TYPE = '2' AND
03765          PI-PAYMENTS-SW = +1)
03766        OR
03767         (AT-TRAILER-TYPE = '3' AND
03768          PI-AUTO-PAY-SW = +1)
03769        OR
03770         (AT-TRAILER-TYPE = '4' AND
03771          PI-LETTERS-SW = +1)
03772        OR
03773         (AT-TRAILER-TYPE = '6' AND
03774          PI-NOTES-SW = +1)
03775        OR
03776         (AT-TRAILER-TYPE = '7' AND
03777          PI-REMINDERS-SW = +1)
03778        OR
03779         (AT-TRAILER-TYPE = '8' AND
03780          PI-DENIALS-SW = +1)
03781        OR
03782         (AT-TRAILER-TYPE = '9' AND
03783          PI-INCURRED-DATE-SW = +1)
03784        OR
03785         (AT-TRAILER-TYPE = 'A' AND
03786          PI-FORMS-SW = +1)
03787              NEXT SENTENCE
03788            ELSE
03789              GO TO 4100-READNEXT.
03790
03791      ADD 1 TO LCP-ONCTR-01
03792
03793      IF LCP-ONCTR-01 = 2
03794          GO TO 6000-ENDBROWSE.
03795
03796      MOVE AT-RECORDED-BY         TO  PI-UPDATE-BY.
03797      MOVE AT-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.
03798      MOVE AT-CONTROL-PRIMARY     TO  PI-SAVE-KEY.
03799
03800      ADD +1                      TO  PI-RECORD-COUNT.
03801
03802      EJECT
03803      IF AT-TRAILER-TYPE NOT = '1'
03804          GO TO 4200-PAYMENT-TRAILER.
03805
03806      MOVE AT-CONTROL-PRIMARY     TO WS-CLAIM-KEY.
03807
03808      
      * EXEC CICS READ
03809 *         DATASET (WS-CLAIM-MASTER-DSID)
03810 *         RIDFLD  (WS-CLAIM-KEY)
03811 *         SET     (ADDRESS OF CLAIM-MASTER)
03812 *    END-EXEC.
      *    MOVE '&"S        E          (   #00012605' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132363035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CLAIM-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CLAIM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03813
03814      IF CL-LAST-CLOSE-DT = LOW-VALUES
03815         MOVE SPACES              TO HLSTCLOO
03816      ELSE
03817         MOVE CL-LAST-CLOSE-DT    TO DC-BIN-DATE-1
03818         MOVE SPACES              TO  DC-OPTION-CODE
03819         PERFORM 8500-DATE-CONVERSION
03820         MOVE DC-GREG-DATE-1-EDIT TO HLSTCLOO.
03821
03822      IF CL-LAST-REOPEN-DT = LOW-VALUES
03823         MOVE SPACES TO HLSTOPEO
03824      ELSE
03825         MOVE CL-LAST-REOPEN-DT   TO DC-BIN-DATE-1
03826         MOVE SPACES              TO  DC-OPTION-CODE
03827         PERFORM 8500-DATE-CONVERSION
03828         MOVE DC-GREG-DATE-1-EDIT TO HLSTOPEO.
03829
03830      MOVE EL142H                 TO  PI-MAP-NAME.
03831
03832      MOVE AT-TRAILER-TYPE        TO  HTLRTYPO.
03833      MOVE AT-SEQUENCE-NO         TO  HSEQO.
03834
03835      MOVE SPACES                 TO  DC-OPTION-CODE.
03836      MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1.
03837      PERFORM 8500-DATE-CONVERSION.
03838      MOVE DC-GREG-DATE-1-EDIT    TO  HRECDTEO.
03839
03840      MOVE AT-RECORDED-BY         TO  HBYO.
03841
03842      MOVE AT-RESERVES-LAST-MAINT-DT   TO  DC-BIN-DATE-1.
03843      MOVE ' '                         TO  DC-OPTION-CODE.
03844      PERFORM 8500-DATE-CONVERSION.
03845      IF NO-CONVERSION-ERROR
03846          MOVE DC-GREG-DATE-1-EDIT     TO  HMANTONO
03847      ELSE
03848          MOVE SPACES                  TO  HMANTONO.
03849
03850      MOVE AT-RESERVES-LAST-UPDATED-BY TO  HMANTBYO.
03851
03852      MOVE AT-LAST-MAINT-HHMMSS        TO  TIME-IN.
03853      MOVE TIME-OUT                    TO  HMANTATO.
03854
03855      MOVE AT-MANUAL-SW           TO  HRESMANO.
03856      INSPECT HRESMANO CONVERTING ' 1' TO 'NY'.
03857
03858      MOVE AT-FUTURE-SW           TO  HRESFUTO.
03859      INSPECT HRESFUTO CONVERTING ' 1' TO 'NY'.
03860
03861      MOVE AT-PTC-SW              TO  HRESAHPO.
03862      INSPECT HRESAHPO CONVERTING ' 1' TO 'NY'.
03863
03864      MOVE AT-IBNR-SW             TO  HRESIBNO.
03865      INSPECT HRESIBNO CONVERTING ' 1' TO 'NY'.
03866
03867      MOVE AT-PTC-LF-SW           TO  HRESLFPO.
03868      INSPECT HRESLFPO CONVERTING ' 1' TO 'NY'.
03869
03870      MOVE AT-CURRENT-MANUAL-RESERVE  TO  HMANAMTO.
03871      MOVE AT-FUTURE-RESERVE      TO  HFUTAMTO.
03872      MOVE AT-IBNR-RESERVE        TO  HIBNAMTO.
03873      MOVE AT-PAY-CURRENT-RESERVE TO  HPTCAMTO.
03874
03875      MOVE AT-ITD-CHARGEABLE-EXPENSE TO HITDCO.
03876      MOVE AT-ITD-PAID-EXPENSES   TO  HITDNCO.
03877
03878      MOVE AT-CDT-ACCESS-METHOD   TO  HCDTAMO.
03879      MOVE AT-PERCENT-OF-CDT      TO  HPCTCDTO.
03880
03881      MOVE AT-EXPENSE-METHOD      TO  HEXPO.
03882
03883      IF AT-EXPENSE-METHOD = '2' OR '4'
03884          MOVE AT-EXPENSE-DOLLAR  TO  HEXPAMTO
03885      ELSE
03886          IF AT-EXPENSE-METHOD = '3'
03887              MOVE AT-EXPENSE-PERCENT  TO  HEXPAMTO
03888          ELSE
03889              MOVE ZERO           TO  HEXPAMTO.
03890
03891      MOVE -1                     TO  HMAINTL.
03892
03893      MOVE +1                     TO  WS-INDEX.
03894      SET EL142H-INDEX1
03895          EL142H-INDEX2  TO  +1.
03896
03897  4110-MOVE-CAUSE.
03898      MOVE AT-OPEN-CLOSE-DATE (WS-INDEX)  TO  DC-BIN-DATE-1.
03899
03900      IF DC-BIN-DATE-1 = LOW-VALUES OR SPACES
03901          GO TO 4120-BUMP-INDEX.
03902
03903      MOVE SPACES                 TO  DC-OPTION-CODE.
03904      PERFORM 8500-DATE-CONVERSION.
03905      MOVE DC-GREG-DATE-1-EDIT    TO EL142H-DATE
03906                                     (EL142H-INDEX1  EL142H-INDEX2)
03907
03908      MOVE AT-OPEN-CLOSE-TYPE (WS-INDEX) TO EL142H-OC
03909                                     (EL142H-INDEX1  EL142H-INDEX2)
03910
03911      MOVE AT-OPEN-CLOSE-REASON (WS-INDEX) TO EL142H-CAUSE
03912                                    (EL142H-INDEX1  EL142H-INDEX2).
03913
03914  4120-BUMP-INDEX.
03915      IF WS-INDEX LESS +6
03916          ADD +1                  TO  WS-INDEX
03917      ELSE
03918          GO TO 4100-READNEXT.
03919
03920      IF EL142H-INDEX1 LESS +5
03921          SET EL142H-INDEX1 UP BY +1
03922          GO TO 4110-MOVE-CAUSE.
03923
03924      IF EL142H-INDEX2 LESS +2
03925          SET EL142H-INDEX1 TO +1
03926          SET EL142H-INDEX2 UP BY +1
03927          GO TO 4110-MOVE-CAUSE.
03928
03929      GO TO 4100-READNEXT.
03930
03931      EJECT
03932  4200-PAYMENT-TRAILER.
03933      IF AT-TRAILER-TYPE NOT = '2'
03934          GO TO 4300-AUTO-PAY-TRAILER.
03935
03936      MOVE EL142B                 TO  PI-MAP-NAME.
03937
03938      MOVE AT-TRAILER-TYPE        TO  BTLRTYPO.
03939      MOVE AT-SEQUENCE-NO         TO  BSEQO.
03940
03941      MOVE SPACES                 TO  DC-OPTION-CODE.
03942      MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1.
03943      PERFORM 8500-DATE-CONVERSION.
03944      MOVE DC-GREG-DATE-1-EDIT    TO  BRECDTEO.
03945
03946      MOVE AT-RECORDED-BY         TO  BBYO.
03947
03948      MOVE AT-PAYMENT-LAST-UPDATED-BY TO  BMANTBYO.
03949      MOVE AT-PAYMENT-LAST-MAINT-DT   TO  DC-BIN-DATE-1.
03950      MOVE ' '                        TO  DC-OPTION-CODE.
03951      PERFORM 8500-DATE-CONVERSION.
03952      IF NO-CONVERSION-ERROR
03953          MOVE DC-GREG-DATE-1-EDIT    TO  BMANTONO
03954      ELSE
03955          MOVE SPACES                 TO  BMANTONO.
03956
03957      MOVE AT-LAST-MAINT-HHMMSS       TO  TIME-IN.
03958      MOVE TIME-OUT                   TO  BMANTATO.
03959
03960      MOVE AT-CHECK-NO            TO  BCKNOO.
013017     if at-ach-payment = 'Y'
013017        move 'YES'               to bachpmto
013017        go to 4200-ach-cashed-dt
013017     else
013017        move 'NO '               to bachpmto
013017     end-if
021114
021114**** This routine will connect to the Logic Database on SQL Server
021114**** and call a stored procedure to determine the check cashed dat
021114
021114     PERFORM 7000-CONNECT-TO-DB  THRU 7000-EXIT
021114     IF SQLCODE = 0
021114        PERFORM 7100-GET-CHK-CASHED-DT  THRU 7100-EXIT
111714        if sqlcode = zeros
111714           move ws-check-cashed-dt (3:2)
111714                                 to dc-ymd-year
111714           move ws-check-cashed-dt (6:2)
111714                                 to dc-ymd-month
111714           move ws-check-cashed-dt (9:2)
111714                                 to dc-ymd-day
091714        else
091714           perform 7110-check-manual thru 7110-exit
021114           IF SQLCODE = 0
081214              move ws-check-cashed-dt (7:2)
081214                                 to dc-ymd-year
081214              move ws-check-cashed-dt (1:2)
081214                                 to dc-ymd-month
081214              move ws-check-cashed-dt (4:2)
081214                                 to dc-ymd-day
091714           end-if
111714        end-if
111714        if sqlcode = zeros
081214           move '3' to dc-option-code
081214           perform 8500-date-conversion
081214           if (no-conversion-error)
081214              and (dc-bin-date-1 > at-check-written-dt)
                    move dc-greg-date-1-edit to bcashedo
021114*             MOVE WS-CHECK-CASHED-DT TO BCASHEDO
081214           else
081214              move spaces        to bcashedo
081214           end-if
021114        END-IF
021114     END-IF
021114     PERFORM 7200-DISCONNECT THRU 7200-EXIT
           go to 4200-carry-on-my-waward-son
013017     .
       4200-ach-cashed-dt.
           MOVE AT-CONTROL-PRIMARY     TO WS-CLAIM-KEY
           
      * EXEC CICS READ
      *         DATASET (WS-CLAIM-MASTER-DSID)
      *         RIDFLD  (WS-CLAIM-KEY)
      *         SET     (ADDRESS OF CLAIM-MASTER)
      *    END-EXEC
      *    MOVE '&"S        E          (   #00012806' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132383036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CLAIM-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CLAIM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           move spaces                 to ws-check-cashed-dt
           move zeros                  to sqlcode
           PERFORM 7000-CONNECT-TO-DB  THRU 7000-EXIT
           IF SQLCODE <> 0
              display ' bad connect ' sqlcode
              go to 4200-carry-on-my-waward-son
           end-if
           move at-carrier             to ws-carrier
           move cl-cert-state          to ws-state
           move cl-cert-account        to ws-account-no
           move at-cert-no             to ws-cert-no
           move at-claim-no            to ws-claim-no
           move zeros                  to ws-check-no
           move at-check-no            to ws-check-no (4:7)
           if pi-company-id = 'CID'
              exec sql
                 SELECT
                    CASHED_DATE
                 INTO
                    :ws-check-cashed-dt
                 FROM
                    CLM_PMTS_ACH
                 WHERE
                        CARRIER   = :ws-carrier
                    and STATE     = :ws-state
                    and ACCOUNT   = :ws-account-no
                    and CERT_NO   = :ws-cert-no
                    and CLAIM_NO  = :ws-claim-no
                    and CHECK_NO  = :ws-check-no
              end-exec
           else
              exec sql
                 SELECT
                    CASHED_DATE
                 INTO
                    :ws-check-cashed-dt
                 FROM
                    DCC_CLM_PMTS_ACH
                 WHERE
                        CARRIER   = :ws-carrier
                    and STATE     = :ws-state
                    and ACCOUNT   = :ws-account-no
                    and CERT_NO   = :ws-cert-no
                    and CLAIM_NO  = :ws-claim-no
                    and CHECK_NO  = :ws-check-no
              end-exec
           end-if
           if sqlcode not = 0 and 1
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' dis sql code ' ws-dis-sql-code
              display "Error: cannot read row "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           else
              move ws-check-cashed-dt (3:2)
                                       to dc-ymd-year
              move ws-check-cashed-dt (6:2)
                                       to dc-ymd-month
              move ws-check-cashed-dt (9:2)
                                       to dc-ymd-day
              move '3'                 to dc-option-code
              perform 8500-date-conversion
              if (no-conversion-error)
                 and (dc-bin-date-1 > at-check-written-dt)
                 move dc-greg-date-1-edit
                                       to bcashedo
              else
                 move spaces           to bcashedo
              end-if
           end-if
           PERFORM 7200-DISCONNECT THRU 7200-EXIT
           .
013017 4200-carry-on-my-waward-son.
03962      IF OFFLINE-PMT
03963         MOVE AL-UANOF            TO BCKNOA
03964        ELSE
03965         MOVE AL-SANOF            TO BCKNOA.
03966
03967      IF AT-CHECK-WRITTEN-DT NOT = LOW-VALUES
03968          MOVE SPACES             TO  DC-OPTION-CODE
03969          MOVE AT-CHECK-WRITTEN-DT TO  DC-BIN-DATE-1
03970          PERFORM 8500-DATE-CONVERSION
03971          MOVE DC-GREG-DATE-1-EDIT TO  BDTWRITO.
03972
03973      MOVE AT-RECORDED-BY         TO  BWRITBYO.
031808     MOVE AT-PMT-APPROVED-BY     TO  BAPPVBYO.
03974      MOVE AT-AMOUNT-PAID         TO  BAMTO.
03975
03976      IF AT-PAID-THRU-DT NOT = LOW-VALUES
03977         MOVE SPACES             TO  DC-OPTION-CODE
03978         MOVE AT-PAID-THRU-DT    TO  DC-BIN-DATE-1
03979         PERFORM 8500-DATE-CONVERSION
03980         MOVE DC-GREG-DATE-1-EDIT TO  BPDTHRUO
03981         IF PI-USES-PAID-TO
03982            MOVE '6'                TO  DC-OPTION-CODE
03983            MOVE AT-PAID-THRU-DT    TO  DC-BIN-DATE-1
03984            MOVE +1                 TO  DC-ELAPSED-DAYS
03985            MOVE +0                 TO  DC-ELAPSED-MONTHS
03986            PERFORM 8500-DATE-CONVERSION
03987            MOVE DC-GREG-DATE-1-EDIT TO  BPDTHRUO.
03988
03989      IF AT-TO-BE-WRITTEN-DT NOT = LOW-VALUES AND SPACES
03990          MOVE SPACES             TO  DC-OPTION-CODE
03991          MOVE AT-TO-BE-WRITTEN-DT TO  DC-BIN-DATE-1
03992          PERFORM 8500-DATE-CONVERSION
03993          MOVE DC-GREG-DATE-1-EDIT TO  BHOLDATO.
03994
03995      MOVE AT-DAYS-IN-PERIOD      TO  BDAYSPDO
020413     IF AT-PRINT-EOB-WITH-CHECK = 'Y' OR 'S'
020413        MOVE AT-PRINT-EOB-WITH-CHECK TO BEOBYNO
           ELSE
              MOVE 'N'                 TO BEOBYNO
           END-IF
020413     IF AT-PRINT-CLM-FORM = 'N'
020413         MOVE 'N'                TO BCLMYNO
020413     ELSE
020413         MOVE 'Y'                TO BCLMYNO
020413     END-IF
020413     IF AT-PRINT-SURVEY = 'N'
020413         MOVE 'N'                TO BSRVYNO
020413     ELSE
020413         MOVE 'Y'                TO BSRVYNO
020413     END-IF
102413     IF AT-SPECIAL-RELEASE = 'Y'
102413         MOVE 'Y'                TO BSPRELO
102413     ELSE
102413         MOVE 'N'                TO BSPRELO
102413     END-IF
           IF AT-CHECK-WRITTEN-DT = LOW-VALUES
020413      AND AT-VOID-DT = LOW-VALUES
              MOVE AL-UANON            TO BEOBYNA
020413        MOVE AL-UANON            TO BCLMYNA
102413        MOVE AL-UANON            TO BSPRELA
111113        IF PI-APPROVAL-LEVEL = '4' OR '5'
020413           MOVE AL-UANON         TO BSRVYNA
020413        END-IF
           END-IF
082807     IF AT-PAYMENT-TYPE = 'I'
082807        IF AT-INT-RATE NOT NUMERIC
082807           MOVE ZEROS            TO AT-INT-RATE
082807        END-IF
082807        MOVE AT-INT-RATE         TO  BDAYRATO
082807        MOVE 'INTEREST RT  -'    TO  BRATHDO
082807     ELSE
03996         MOVE AT-DAILY-RATE       TO  BDAYRATO
082807     END-IF
03997
03998      IF AT-CV-PMT-CODE = ' '
03999          GO TO 4200-DISPLAY-PMT-DESC.
04000
04001      IF AT-CV-PMT-CODE = '1'
04002          MOVE 'FULL DEATH'               TO  BPAYTYPO.
04003
04004      IF AT-CV-PMT-CODE = '2'
04005          MOVE 'HALF DEATH'               TO  BPAYTYPO.
04006
04007      IF AT-CV-PMT-CODE = '3'
04008          MOVE 'FULL AD&D'                TO  BPAYTYPO.
04009
04010      IF AT-CV-PMT-CODE = '4'
04011          MOVE 'HALF AD&D'                TO  BPAYTYPO.
04012
04013      IF AT-CV-PMT-CODE = '5'
04014          MOVE 'FULL RIDER'               TO  BPAYTYPO.
04015
04016      IF AT-CV-PMT-CODE = '6'
04017          MOVE 'HALF RIDER'               TO  BPAYTYPO.
04018
04019      IF AT-CV-PMT-CODE = '7'
04020          MOVE 'NON-CHG EXP'              TO  BPAYTYPO.
04021
04022      IF AT-CV-PMT-CODE = '8'
04023          MOVE 'ADDITIONAL'               TO  BPAYTYPO.
04024
04025      GO TO 4200-PAYMENT-TRAILER-CONT.
04026
04027  4200-DISPLAY-PMT-DESC.
04028
022106     EVALUATE AT-PAYMENT-TYPE
022106        WHEN '1'
022106           MOVE 'PARTIAL PAYMENT'        TO BPAYTYPO
022106        WHEN '2'
022106           MOVE 'FINAL PAYMENT'          TO BPAYTYPO
022106        WHEN '3'
022106           MOVE 'LUMP SUM PAYMENT'       TO BPAYTYPO
022106        WHEN '4'
022106           MOVE 'ADDITIONAL PAYMENT'     TO BPAYTYPO
022106        WHEN '5'
022106           MOVE 'CHARGEABLE PAYMENT'     TO BPAYTYPO
022106        WHEN '6'
022106           MOVE 'NON-CHARGEABLE PAYMENT' TO BPAYTYPO
022106        WHEN '7'
022106           MOVE 'LIFE PREMIUM REFUND'    TO BPAYTYPO
022106        WHEN '8'
022106           MOVE 'A & H PREMIUM REFUND'   TO BPAYTYPO
022106        WHEN 'I'
022106           MOVE 'INTEREST PAYMENT   '    TO BPAYTYPO
022106        WHEN OTHER
022106           MOVE 'ENTRY CORRECTION'       TO BPAYTYPO
022106     END-EVALUATE
           .
04055  4200-PAYMENT-TRAILER-CONT.
04056
04057      MOVE AT-FORCE-CONTROL       TO  BFORCEDO.
04058      INSPECT BFORCEDO CONVERTING ' 1' TO 'NY'.
04059
04060      IF AT-VOID-DT NOT = LOW-VALUES
04061          MOVE SPACES              TO  DC-OPTION-CODE
04062          MOVE AT-VOID-DT          TO  DC-BIN-DATE-1
04063          PERFORM 8500-DATE-CONVERSION
04064          MOVE DC-GREG-DATE-1-EDIT TO  BVOIDDTO.
04065
052506     IF AT-PMT-PROOF-DT NOT = LOW-VALUES
052506         MOVE SPACES              TO  DC-OPTION-CODE
052506         MOVE AT-PMT-PROOF-DT     TO  WS-PRF-DT
052506         MOVE AT-PMT-PROOF-DT     TO  DC-BIN-DATE-1
052506         PERFORM 8500-DATE-CONVERSION
052506         MOVE DC-GREG-DATE-1-EDIT TO  BPRFDTI.
052506
04066      IF AT-PAYEE-TYPE EQUAL 'I'
04067          MOVE 'INSURED'          TO  BPAYEEO
04068        ELSE
04069      IF AT-PAYEE-TYPE EQUAL 'B'
04070          MOVE 'BENEFICIARY'      TO  BPAYEEO
04071        ELSE
04072      IF AT-PAYEE-TYPE EQUAL 'A'
04073          MOVE 'ACCOUNT'          TO  BPAYEEO
04074        ELSE
04075      IF AT-PAYEE-TYPE EQUAL 'O'
04076          MOVE 'OTHER 1'          TO  BPAYEEO
04077        ELSE
04078      IF AT-PAYEE-TYPE EQUAL 'Q'
04079          MOVE 'REM BORR   '      TO  BPAYEEO
04080        ELSE
04081      IF AT-PAYEE-TYPE EQUAL 'P'
04082          MOVE 'DOCTOR'           TO  BPAYEEO
04083        ELSE
04084      IF AT-PAYEE-TYPE EQUAL 'E'
04085          MOVE 'EMPLOYER'         TO  BPAYEEO.
04086
04087      MOVE AT-PAYEES-NAME         TO  BPNAMEO.
04088
04089      MOVE AT-ADDL-RESERVE        TO  BRESERVO.
082807     IF AT-PAYMENT-TYPE NOT = 'I'
082807        MOVE AT-EXPENSE-PER-PMT  TO  BEXPO
082807     END-IF
04091
04092      IF AT-PMT-SELECT-DT NOT = LOW-VALUES
04093          MOVE SPACES             TO  DC-OPTION-CODE
04094          MOVE AT-PMT-SELECT-DT   TO  DC-BIN-DATE-1
04095          PERFORM 8500-DATE-CONVERSION
04096          MOVE DC-GREG-DATE-1-EDIT TO  BCRSELO.
04097
04098      IF AT-PMT-ACCEPT-DT NOT = LOW-VALUES
04099          MOVE SPACES             TO  DC-OPTION-CODE
04100          MOVE AT-PMT-ACCEPT-DT   TO  DC-BIN-DATE-1
04101          PERFORM 8500-DATE-CONVERSION
04102          MOVE DC-GREG-DATE-1-EDIT TO  BCRACPO.
04103
04104      IF AT-VOID-SELECT-DT NOT = LOW-VALUES
04105          MOVE SPACES             TO  DC-OPTION-CODE
04106          MOVE AT-VOID-SELECT-DT  TO  DC-BIN-DATE-1
04107          PERFORM 8500-DATE-CONVERSION
04108          MOVE DC-GREG-DATE-1-EDIT TO  BVOIDSDO.
04109
04110      IF AT-VOID-ACCEPT-DT NOT = LOW-VALUES
04111          MOVE SPACES             TO  DC-OPTION-CODE
04112          MOVE AT-VOID-ACCEPT-DT  TO  DC-BIN-DATE-1
04113          PERFORM 8500-DATE-CONVERSION
04114          MOVE DC-GREG-DATE-1-EDIT TO  BVOIDACO.
04115
04116      IF AT-PAYMENT-ORIGIN = '1'
04117          MOVE 'ON-LINE'          TO  BORIGINO
04118        ELSE
04119      IF AT-PAYMENT-ORIGIN = '2'
04120          MOVE 'AUTO PAY'         TO  BORIGINO
04121        ELSE
04122      IF AT-PAYMENT-ORIGIN = '3'
04123          MOVE 'MANUAL'           TO  BORIGINO
04124        ELSE
04125          MOVE AT-PAYMENT-ORIGIN  TO  BORIGINO.
04126
04127      IF AT-EXPENSE-TYPE = SPACE
04128          MOVE AL-SADOF           TO BEXPHDGA
04129          MOVE AL-SANOF           TO BEXPTYPA
04130      ELSE
04131          MOVE AT-EXPENSE-TYPE    TO BEXPTYPO
04132          MOVE AL-SANOF           TO BEXPHDGA
04133          MOVE AL-UANOF           TO BEXPTYPA.
04134
04135      MOVE AT-CHECK-QUE-CONTROL   TO  BCKQUEO.
04136      MOVE AT-CHECK-QUE-SEQUENCE  TO  BCKSEQO.
04137
04138      IF PI-PROCESSOR-ID = 'LGXX'
04139          MOVE AL-UNNOF           TO  BCKQUEA  BCKSEQA
04140                                      BCRSELA  BVOIDSDA
04141          MOVE AL-UANOF           TO  BPAYEEA  BEXPTYPA
04142                                      BCRACPA  BHOLDATA
04143                                      BVOIDACA
04144      ELSE
040819         MOVE AL-SANOF           TO  BHOLDATA
04145          IF AT-CHECK-WRITTEN-DT NOT = LOW-VALUES
020413          OR AT-VOID-DT NOT = LOW-VALUES
040819             MOVE AL-SANOF       TO  BPAYEEA
04147          ELSE
040819             MOVE AL-UANOF       TO  BPAYEEA.
04149
04150      IF (PI-COMPANY-ID = 'LAP' OR 'RMC') OR
04151         (PI-PROCESSOR-ID = 'LGXX')
04152          MOVE AL-UANOF           TO  BPMTORGA.
04153
04154      MOVE -1                     TO  BMAINTL.
04155
04156      IF AT-PAYMENT-NOTE-SEQ-NO = 0
04157          GO TO 4100-READNEXT.
04158
04159  4200-READ-PAYMENT-NOTE-TRLR.
04160
04161      
      * EXEC CICS HANDLE CONDITION
04162 *        NOTFND   (4200-NOTE-TRLR-NOTFND)
04163 *    END-EXEC.
      *    MOVE '"$I                   ! '' #00013130' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303133313330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04164
04165      MOVE PI-ACTIVITY-TRAILERS-KEY   TO
04166                                 PI-PREV-ACTIVITY-TRAILERS-KEY.
04167
04168      MOVE AT-PAYMENT-NOTE-SEQ-NO TO  PI-ATK-SEQUENCE-NO.
04169
04170      
      * EXEC CICS READ
04171 *        DATASET   (WS-ACTIVITY-TRAILERS-DSID)
04172 *        RIDFLD    (PI-ACTIVITY-TRAILERS-KEY)
04173 *        SET       (ADDRESS OF ACTIVITY-TRAILERS)
04174 *    END-EXEC.
      *    MOVE '&"S        E          (   #00013139' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303133313339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ACTIVITY-TRAILERS-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04175
04176      IF AT-PAYMENT-NOTE
04177          MOVE AT-INFO-LINE-1     TO  BNOTE1O
04178          MOVE AT-INFO-LINE-2     TO  BNOTE2O
               IF AT-EOB-CODES-PRESENT
                  MOVE AL-SANOF        TO  BNOTE2A
               END-IF
04179      ELSE
04180          MOVE SPACES             TO  BNOTE1O
04181                                      BNOTE2O.
04182
04183      MOVE PI-PREV-ACTIVITY-TRAILERS-KEY  TO
04184                                      PI-ACTIVITY-TRAILERS-KEY.
04185
04186      GO TO 4100-READNEXT.
04187
04188  4200-NOTE-TRLR-NOTFND.
04189
04190      MOVE PI-PREV-ACTIVITY-TRAILERS-KEY  TO
04191                                      PI-ACTIVITY-TRAILERS-KEY.
04192
04193      GO TO 4100-READNEXT.
04194
04195      EJECT
04196  4300-AUTO-PAY-TRAILER.
04197      IF AT-TRAILER-TYPE NOT = '3'
04198          GO TO 4400-CORRESPONDENCE-TRAILER.
04199
04200      MOVE EL142C                 TO  PI-MAP-NAME.
04201
04202      MOVE AT-TRAILER-TYPE        TO  CTLRTYPO.
04203      MOVE AT-SEQUENCE-NO         TO  CSEQO.
04204
04205      MOVE SPACES                 TO  DC-OPTION-CODE.
04206      MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1.
04207      PERFORM 8500-DATE-CONVERSION.
04208      MOVE DC-GREG-DATE-1-EDIT    TO  CRECDTEO.
04209
04210      MOVE AT-RECORDED-BY         TO  CBYO.
04211
04212      MOVE AT-AUTO-PAY-LAST-MAINT-DT  TO  DC-BIN-DATE-1.
04213      MOVE ' '                        TO  DC-OPTION-CODE.
04214      PERFORM 8500-DATE-CONVERSION.
04215      IF NO-CONVERSION-ERROR
04216          MOVE DC-GREG-DATE-1-EDIT    TO  CMANTONO
04217      ELSE
04218          MOVE SPACES                 TO  CMANTONO.
04219
04220      MOVE AT-AUTO-PAY-LAST-UPDATED-BY    TO  CMANTBYO.
04221
04222      MOVE AT-LAST-MAINT-HHMMSS       TO  TIME-IN.
04223      MOVE TIME-OUT                   TO  CMANTATO.
04224
04225      IF AT-SCHEDULE-START-DT NOT = LOW-VALUES
04226          MOVE SPACES             TO  DC-OPTION-CODE
04227          MOVE AT-SCHEDULE-START-DT   TO  DC-BIN-DATE-1
04228          PERFORM 8500-DATE-CONVERSION
04229          MOVE DC-GREG-DATE-1-EDIT    TO  CEFFDTEO.
04230
04231      IF AT-TERMINATED-DT NOT = LOW-VALUES
04232          MOVE SPACES             TO  DC-OPTION-CODE
04233          MOVE AT-TERMINATED-DT   TO  DC-BIN-DATE-1
04234          PERFORM 8500-DATE-CONVERSION
04235          MOVE DC-GREG-DATE-1-EDIT TO  CREPDTEO.
04236
04237      MOVE AT-FIRST-PMT-AMT       TO  C1STPAO.
04238      MOVE AT-REGULAR-PMT-AMT     TO  CREGPAO.
04239
04240      IF AT-1ST-PAY-THRU-DT NOT = LOW-VALUES
04241          IF PI-USES-PAID-TO
04242              MOVE AT-1ST-PAY-THRU-DT     TO  DC-BIN-DATE-1
04243              MOVE '6'                    TO  DC-OPTION-CODE
04244              MOVE +1                     TO  DC-ELAPSED-DAYS
04245              MOVE +0                     TO  DC-ELAPSED-MONTHS
04246              PERFORM 8500-DATE-CONVERSION
04247              IF NO-CONVERSION-ERROR
04248                  MOVE DC-GREG-DATE-1-EDIT TO C1STPSO
04249              ELSE
04250                  MOVE LOW-VALUES         TO  C1STPSO
04251          ELSE
04252              MOVE SPACES                 TO  DC-OPTION-CODE
04253              MOVE AT-1ST-PAY-THRU-DT     TO  DC-BIN-DATE-1
04254              MOVE +0                     TO  DC-ELAPSED-DAYS
04255                                              DC-ELAPSED-MONTHS
04256              PERFORM 8500-DATE-CONVERSION
04257              IF NO-CONVERSION-ERROR
04258                  MOVE DC-GREG-DATE-1-EDIT TO C1STPSO
04259              ELSE
04260                  MOVE SPACES             TO  C1STPSO.
04261
04262      IF AT-SCHEDULE-END-DT NOT = LOW-VALUES
04263          IF PI-USES-PAID-TO
04264              MOVE AT-SCHEDULE-END-DT     TO  DC-BIN-DATE-1
04265              MOVE '6'                    TO  DC-OPTION-CODE
04266              MOVE +1                     TO  DC-ELAPSED-DAYS
04267              MOVE +0                     TO  DC-ELAPSED-MONTHS
04268              PERFORM 8500-DATE-CONVERSION
04269              IF NO-CONVERSION-ERROR
04270                  MOVE DC-GREG-DATE-1-EDIT TO CLSTPSO
04271              ELSE
04272                  MOVE LOW-VALUES         TO  CLSTPSO
04273          ELSE
04274              MOVE SPACES                 TO  DC-OPTION-CODE
04275              MOVE AT-SCHEDULE-END-DT     TO  DC-BIN-DATE-1
04276              MOVE +0                     TO  DC-ELAPSED-DAYS
04277                                              DC-ELAPSED-MONTHS
04278              PERFORM 8500-DATE-CONVERSION
04279              IF NO-CONVERSION-ERROR
04280                  MOVE DC-GREG-DATE-1-EDIT TO CLSTPSO
04281              ELSE
04282                  MOVE LOW-VALUES         TO  CLSTPSO.
04283
04284      MOVE AT-DAYS-IN-1ST-PMT     TO  CDIFPO.
04285      MOVE AT-INTERVAL-MONTHS     TO  CMBPAYO.
04286
04287      IF AT-LAST-PMT-TYPE = 'P'
04288          MOVE 'PARTIAL'          TO  CLSTPATO
04289      ELSE
04290          IF AT-LAST-PMT-TYPE = 'F'
04291              MOVE 'FINAL  '      TO  CLSTPATO.
04292
04293      IF AT-AUTO-PAYEE-TYPE EQUAL 'I'
04294          MOVE 'INSURED'          TO  CPAYEEO
04295        ELSE
04296      IF AT-AUTO-PAYEE-TYPE EQUAL 'B'
04297          MOVE 'BENEFICIARY'      TO  CPAYEEO
04298        ELSE
04299      IF AT-AUTO-PAYEE-TYPE EQUAL 'A'
04300          MOVE 'ACCOUNT'          TO  CPAYEEO
04301        ELSE
04302      IF AT-AUTO-PAYEE-TYPE EQUAL 'O'
04303          MOVE 'OTHER 1'          TO  CPAYEEO
04304        ELSE
04305          MOVE 'REM BORR'         TO  CPAYEEO.
070909
070909     MOVE AT-AUTO-END-LETTER     TO  CENDLETO.
04306
04307      MOVE -1                     TO  CPFKL.
04308
04309      GO TO 4100-READNEXT.
04310
04311      EJECT
04312  4400-CORRESPONDENCE-TRAILER.
04313      IF AT-TRAILER-TYPE NOT = '4'
04314          GO TO 4500-GENERAL-INFO-TRAILER.
041613
041613     MOVE PI-COMPANY-CD          TO  WS-NA-COMPANY-CD.
041613     MOVE PI-SAVE-ATK-CARRIER    TO  WS-NA-CARRIER.
041613     MOVE PI-SAVE-ATK-CLAIM-NO   TO  WS-NA-CLAIM-NO.
041613     MOVE PI-SAVE-ATK-CERT-NO    TO  WS-NA-CERT-NO.
041613     MOVE AT-LETTER-ARCHIVE-NO   TO  WS-NA-ARCHIVE-NO.
041613
041613     
      * EXEC CICS READ
041613*         DATASET (WS-NAPERSOFT-DSID)
041613*         RIDFLD  (WS-NAPERSOFT-KEY)
041613*         SET     (ADDRESS OF NAPERSOFT-FILE)
041613*         RESP    (WS-RESPONSE)
041613*    END-EXEC.
      *    MOVE '&"S        E          (  N#00013296' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303133323936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-NAPERSOFT-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-NAPERSOFT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF NAPERSOFT-FILE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
041613
041613     IF RESP-NORMAL
041613          MOVE NA-ENCLOSURE-CD TO DENCCODO
041613                                  PI-ENC-CODE
041613          MOVE NA-CREATED-IN-NAPERSOFT TO
041613                                  PI-CREATED-IN-NAPERSOFT
041613     ELSE
041613          MOVE SPACES TO DENCCODO
041613                         PI-ENC-CODE
041613                         PI-CREATED-IN-NAPERSOFT
041613     END-IF.
04315
04316      MOVE AT-CONTROL-PRIMARY     TO  WS-CLAIM-KEY.
04317
04318      
      * EXEC CICS READ
04319 *        DATASET   (WS-CLAIM-MASTER-DSID)
04320 *        RIDFLD    (WS-CLAIM-KEY)
04321 *        SET       (ADDRESS OF CLAIM-MASTER)
04322 *    END-EXEC.
      *    MOVE '&"S        E          (   #00013316' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303133333136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CLAIM-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CLAIM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04323
04324      
      * EXEC CICS HANDLE CONDITION
04325 *        NOTFND (4410-ADDRESS-NOT-FOUND)
04326 *    END-EXEC.
      *    MOVE '"$I                   ! ( #00013322' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303133333232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04327
04328      MOVE EL142D                 TO  PI-MAP-NAME.
04329
04330      MOVE +2                     TO  EMI-NUMBER-OF-LINES
04331                                      EMI-SWITCH2.
04332
04333      MOVE AT-TRAILER-TYPE        TO  DTLRTYPO.
04334      MOVE AT-SEQUENCE-NO         TO  DSEQO.
04335
04336      MOVE SPACES                 TO  DC-OPTION-CODE.
04337      MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1.
04338      PERFORM 8500-DATE-CONVERSION.
04339      MOVE DC-GREG-DATE-1-EDIT    TO  DRECDTEO.
04340
04341      MOVE AT-RECORDED-BY         TO  DBYO.
04342
04343      MOVE AT-CORR-LAST-MAINT-DT      TO  DC-BIN-DATE-1.
04344      MOVE ' '                        TO  DC-OPTION-CODE.
04345      PERFORM 8500-DATE-CONVERSION.
04346      IF NO-CONVERSION-ERROR
04347          MOVE DC-GREG-DATE-1-EDIT    TO  DMANTONO
04348      ELSE
04349          MOVE SPACES                 TO  DMANTONO.
04350
04351      MOVE AT-CORR-LAST-UPDATED-BY    TO  DMANTBYO.
04352
04353      MOVE AT-LAST-MAINT-HHMMSS   TO  TIME-IN.
04354      MOVE TIME-OUT               TO  DMANTATO.
04355
04356      MOVE AT-STD-LETTER-FORM     TO  DFORMNOO.
04357      MOVE AT-LETTER-ARCHIVE-NO   TO  DARCHNOO.
050110     MOVE AT-RESEND-LETTER-FORM  TO  DRESFRMO.
050110     MOVE AT-AUTO-CLOSE-IND      TO  DAUTOCLO.
050110     IF AT-LETTER-TO-BENE EQUAL 'Y'
050110         MOVE 'LETTER SENT TO BENEFICIARY' TO DBENLETO
050110     ELSE
050110         MOVE LOW-VALUES TO DBENLETO
050110     END-IF.
04358
04359      IF AT-LETTER-ARCHIVE-NO = ZEROS
04360         MOVE AL-UANON TO DFORMNOA.
04361
04362      IF AT-LETTER-SENT-DT NOT = LOW-VALUES
04363          MOVE SPACES             TO  DC-OPTION-CODE
04364          MOVE AT-LETTER-SENT-DT  TO  DC-BIN-DATE-1
04365          PERFORM 8500-DATE-CONVERSION
04366          MOVE DC-GREG-DATE-1-EDIT TO  DDTSENTI.
04367
04368      IF AT-AUTO-RE-SEND-DT NOT = LOW-VALUES
04369          MOVE SPACES             TO  DC-OPTION-CODE
04370          MOVE AT-AUTO-RE-SEND-DT TO  DC-BIN-DATE-1
04371          PERFORM 8500-DATE-CONVERSION
04372          MOVE DC-GREG-DATE-1-EDIT TO  DRESENDI.
04373
04374      IF AT-RECEIPT-FOLLOW-UP NOT = LOW-VALUES
04375          MOVE SPACES             TO  DC-OPTION-CODE
04376          MOVE AT-RECEIPT-FOLLOW-UP TO  DC-BIN-DATE-1
04377          PERFORM 8500-DATE-CONVERSION
04378          MOVE DC-GREG-DATE-1-EDIT TO  DREPLYI.
04379
04380      IF AT-LETTER-ANSWERED-DT NOT = LOW-VALUES
04381          MOVE SPACES             TO  DC-OPTION-CODE
04382          MOVE AT-LETTER-ANSWERED-DT TO  DC-BIN-DATE-1
04383          PERFORM 8500-DATE-CONVERSION
04384          MOVE DC-GREG-DATE-1-EDIT TO  DRECEVEI.
04385
04386      IF AT-INITIAL-PRINT-DATE NOT = LOW-VALUES
04387          MOVE SPACES             TO  DC-OPTION-CODE
04388          MOVE AT-INITIAL-PRINT-DATE  TO  DC-BIN-DATE-1
04389          PERFORM 8500-DATE-CONVERSION
04390          MOVE DC-GREG-DATE-1-EDIT TO  DINPRNTI.
04391
04392      IF AT-RESEND-PRINT-DATE  NOT = LOW-VALUES
04393          MOVE SPACES             TO  DC-OPTION-CODE
04394          MOVE AT-RESEND-PRINT-DATE   TO  DC-BIN-DATE-1
04395          PERFORM 8500-DATE-CONVERSION
04396          MOVE DC-GREG-DATE-1-EDIT TO  DREPRNTI.
041613
041613     IF AT-INITIAL-PRINT-DATE NOT = LOW-VALUES AND SPACES
041613         MOVE AL-SANOF               TO DENCCODA
041613     END-IF.
100610
100610     IF AT-RESEND-PRINT-DATE NOT = LOW-VALUES
100610         MOVE AL-SANOF               TO DRECEVEA
102610                                        DRESENDA
102610                                        DREPLYA
102610                                        DSTOPLTA
100610     END-IF.
102610
102610     IF AT-STOP-LETTER-DT NOT = LOW-VALUES AND SPACES
102610         MOVE SPACES           TO DC-OPTION-CODE
102610         MOVE AT-STOP-LETTER-DT TO DC-BIN-DATE-1
102610         PERFORM 8500-DATE-CONVERSION
102610         MOVE DC-GREG-DATE-1-EDIT TO DSTOPLTI
102610         MOVE AL-SANOF            TO DRECEVEA
102610                                     DRESENDA
102610                                     DREPLYA
041613                                     DENCCODA
102610     END-IF.
04397
04398      IF PI-COMPANY-ID NOT = 'DMD'
04399          GO TO 4400-CONTINUE.
04400
04401 ** DMD CODE START *****************
04402 *    MOVE AT-DMD-LETTER-STATUS       TO DMDLETSI.
04403 *
04404 *    IF AT-DMD-LETTER-ONLINE
04405 *        MOVE 'ONLINE'               TO DMDLETDO
04406 *      ELSE
04407 *    IF AT-DMD-LETTER-PURGED
04408 *        MOVE 'PURGED'               TO DMDLETDO
04409 *      ELSE
04410 *    IF AT-DMD-LETTER-RELOADED
04411 *        MOVE 'RELOADED'             TO DMDLETDO.
04412 *
04413 *    IF AT-DMD-LETTER-PURGE-DT NOT = LOW-VALUES AND SPACES
04414 *        MOVE SPACES                 TO DC-OPTION-CODE
04415 *        MOVE AT-DMD-LETTER-PURGE-DT TO DC-BIN-DATE-1
04416 *        PERFORM 8500-DATE-CONVERSION
04417 *        MOVE DC-GREG-DATE-1-EDIT    TO DMDPURDI.
04418 *
04419 *    IF AT-DMD-LETTER-RELOAD-DT NOT = LOW-VALUES AND SPACES
04420 *        MOVE SPACES                  TO DC-OPTION-CODE
04421 *        MOVE AT-DMD-LETTER-RELOAD-DT TO DC-BIN-DATE-1
04422 *        PERFORM 8500-DATE-CONVERSION
04423 *        MOVE DC-GREG-DATE-1-EDIT     TO DMDRELDI.
04424 ** DMD CODE END *******************
04425
04426  4400-CONTINUE.
04427      IF AT-LETTER-PURGED-DT NOT = LOW-VALUES AND SPACES
04428          MOVE ' '                    TO  DC-OPTION-CODE
04429          MOVE AT-LETTER-PURGED-DT    TO  DC-BIN-DATE-1
04430          PERFORM 8500-DATE-CONVERSION
04431          MOVE DC-GREG-DATE-1-EDIT    TO  DPURGDTI
04432          MOVE AL-SABON               TO  DPURGHDA
04433      ELSE
04434          MOVE AL-SANON               TO  DPURGHDA.
04435
04436      MOVE AT-LETTER-ORIGIN       TO  DWRITENO.
04437      INSPECT DWRITENO CONVERTING '1234' TO 'NYNY'.
04438
04439      MOVE AT-REASON-TEXT         TO  DREASONO.
04440
04441      MOVE -1                     TO  DMAINTL.
04442
04443      MOVE AT-ADDRESSEE-NAME      TO  DMAILTOO.
04444
04445      IF AT-ADDRESEE-TYPE = SPACE
04446          NEXT SENTENCE
04447       ELSE
04448      IF AT-ADDRESEE-TYPE      = '3'   AND
04449         AT-ADDRESS-REC-SEQ-NO = ZERO  AND
04450         CL-SYSTEM-IDENTIFIER  = 'CR'
04451          MOVE SPACES             TO  WS-ACCOUNT-MASTER-KEY
04452          MOVE PI-COMPANY-CD      TO  WS-AM-COMPANY-CD
04453          MOVE PI-CARRIER         TO  WS-AM-CARRIER
04454          MOVE PI-GROUPING        TO  WS-AM-GROUPING
04455          MOVE PI-STATE           TO  WS-AM-STATE
04456          MOVE PI-ACCOUNT         TO  WS-AM-ACCOUNT
04457          MOVE PI-CERT-EFF-DT     TO  WS-AM-EXPIRATION-DT
04458          PERFORM 4420-FIND-ACCOUNT-MASTER THRU 4449-EXIT
04459       ELSE
04460      IF AT-ADDRESEE-TYPE      = '3'   AND
04461         AT-ADDRESS-REC-SEQ-NO = ZERO  AND
04462         CL-SYSTEM-IDENTIFIER  = 'CV'
04463          MOVE SPACES             TO  WS-PRODUCER-MASTER-KEY
04464          MOVE PI-COMPANY-CD      TO  WS-PD-COMPANY-CD
04465          MOVE PI-CARRIER         TO  WS-PD-CARRIER
04466          MOVE PI-GROUPING        TO  WS-PD-GROUPING
04467          MOVE PI-STATE           TO  WS-PD-STATE
04468          MOVE PI-PRODUCER        TO  WS-PD-PRODUCER
04469          MOVE PI-CERT-EFF-DT     TO  WS-PD-EXPIRATION-DT
04470          PERFORM 4450-FIND-PRODUCER-MASTER THRU 4499-EXIT
04471       ELSE
04472      IF AT-ADDRESS-REC-SEQ-NO NOT = ZERO
04473          MOVE PI-ACTIVITY-TRAILERS-KEY TO WS-ACTIVITY-TRAILERS-KEY
04474          MOVE AT-ADDRESS-REC-SEQ-NO    TO  WS-ATK-SEQUENCE-NO
04475          
      * EXEC CICS READ
04476 *            DATASET (WS-ACTIVITY-TRAILERS-DSID)
04477 *            RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)
04478 *            SET     (ADDRESS OF ACTIVITY-TRAILERS)
04479 *        END-EXEC
      *    MOVE '&"S        E          (   #00013502' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303133353032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ACTIVITY-TRAILERS-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
04480          IF AT-MAIL-TO-NAME NOT = DMAILTOO
04481              MOVE ER-0384           TO  EMI-ERROR
04482              PERFORM 9900-ERROR-FORMAT
04483          ELSE
04484              MOVE AT-MAIL-TO-NAME        TO  DMAILTOO
04485              MOVE AT-ADDRESS-LINE-1      TO  DADDR1O
04486              MOVE AT-ADDRESS-LINE-2      TO  DADDR2O
04487 *            MOVE AT-CITY-STATE          TO  DCITYSTO
                   STRING AT-CITY ' ' AT-STATE
                      DELIMITED BY '  ' INTO DCITYSTO
                   END-STRING
04488              MOVE AT-PHONE-NO            TO  DPHONEO
04489              INSPECT DPHONEI CONVERTING SPACES TO '-'
04490              IF AT-CANADIAN-POST-CODE
04491                  MOVE AT-CAN-POSTAL-1    TO WS-CAN-POSTAL-CD-1
04492                  MOVE AT-CAN-POSTAL-2    TO WS-CAN-POSTAL-CD-2
04493                  MOVE SPACES             TO WS-DASH-CAN
04494                                             WS-CAN-FILLER
04495                  MOVE WS-CANADIAN-POSTAL-CODES
04496                                          TO DZIPO
04497              ELSE
04498                  MOVE AT-ZIP-CODE        TO WS-ZIP-CODE
04499                  IF AT-ZIP-PLUS4 = SPACES OR ZEROS
04500                      MOVE SPACES         TO WS-ZIP-PLUS4
04501                                             WS-DASH
04502                      MOVE WS-ZIP         TO DZIPO
04503                  ELSE
04504                      MOVE AT-ZIP-PLUS4   TO WS-ZIP-PLUS4
04505                      MOVE '-'            TO WS-DASH
04506                      MOVE WS-ZIP         TO DZIPO.
04507
04508      GO TO 4100-READNEXT.
04509
04510  4410-ADDRESS-NOT-FOUND.
04511      MOVE ER-0388                TO  EMI-ERROR.
04512      PERFORM 9900-ERROR-FORMAT.
04513
04514      GO TO 4100-READNEXT.
04515
04516      EJECT
04517  4420-FIND-ACCOUNT-MASTER.
04518      MOVE ZERO                   TO  WS-NOT-FOUND.
04519
04520      
      * EXEC CICS HANDLE CONDITION
04521 *        NOTFND (4440-NOT-FOUND)
04522 *    END-EXEC.
      *    MOVE '"$I                   ! ) #00013550' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303133353530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04523
04524      
      * EXEC CICS STARTBR
04525 *        DATASET   (WS-ACCOUNT-MASTER-DSID)
04526 *        RIDFLD    (WS-ACCOUNT-MASTER-KEY)
04527 *        GENERIC   EQUAL
04528 *        KEYLENGTH (13)
04529 *    END-EXEC.
           MOVE 13
             TO DFHEIV11
           MOVE 0
             TO DFHEIV12
      *    MOVE '&,   KG    E          &   #00013554' TO DFHEIV0
           MOVE X'262C2020204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303133353534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACCOUNT-MASTER-DSID, 
                 WS-ACCOUNT-MASTER-KEY, 
                 DFHEIV11, 
                 DFHEIV12, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04530
04531      MOVE +1                     TO  WS-READNEXT-SW.
04532
04533  4425-READNEXT.
04534      
      * EXEC CICS READNEXT
04535 *        DATASET   (WS-ACCOUNT-MASTER-DSID)
04536 *        RIDFLD    (WS-ACCOUNT-MASTER-KEY)
04537 *        SET       (ADDRESS OF ACCOUNT-MASTER)
04538 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00013564' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303133353634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACCOUNT-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ACCOUNT-MASTER-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04539
04540      IF PI-COMPANY-CD = WS-AM-COMPANY-CD AND
04541         PI-CARRIER    = WS-AM-CARRIER    AND
04542         PI-GROUPING   = WS-AM-GROUPING   AND
04543         PI-STATE      = WS-AM-STATE      AND
04544         PI-ACCOUNT    = WS-AM-ACCOUNT
04545          NEXT SENTENCE
04546        ELSE
04547          GO TO 4440-NOT-FOUND.
04548
04549      IF (PI-CERT-EFF-DT NOT LESS THAN AM-EFFECTIVE-DT
04550        AND PI-CERT-EFF-DT LESS THAN AM-EXPIRATION-DT)
04551          IF AT-TRAILER-TYPE = '4'
04552              MOVE AM-NAME        TO  DMAILTOO
04553              MOVE AM-PERSON      TO  DADDR1O
04554              MOVE AM-ADDRS       TO  DADDR2O
04555 *            MOVE AM-CITY        TO  DCITYSTO
                   STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
                      DELIMITED BY '  ' INTO DCITYSTO
                   END-STRING
04556              MOVE AM-TEL-NO      TO WS-WORK-PHONE
04557              INSPECT WS-WORK-PHONE CONVERTING SPACES TO '0'
04558              MOVE WS-NUMERIC-PHONE TO DPHONEO
04559              INSPECT DPHONEI CONVERTING SPACES TO '-'
04560              IF  AM-CANADIAN-POST-CODE
04561                  MOVE AM-CAN-POSTAL-1    TO WS-CAN-POSTAL-CD-1
04562                  MOVE AM-CAN-POSTAL-2    TO WS-CAN-POSTAL-CD-2
04563                  MOVE SPACES             TO WS-DASH-CAN
04564                                             WS-CAN-FILLER
04565                  MOVE WS-CANADIAN-POSTAL-CODES
04566                                          TO DZIPO
04567                  GO TO 4445-END-OF-BROWSE
04568              ELSE
04569                  MOVE AM-ZIP-PRIME       TO WS-ZIP-CODE
04570                  IF AM-ZIP-PLUS4 = SPACES OR ZEROS
04571                      MOVE SPACES         TO WS-ZIP-PLUS4 WS-DASH
04572                      MOVE WS-ZIP         TO DZIPO
04573                      GO TO 4445-END-OF-BROWSE
04574                  ELSE
04575                      MOVE AM-ZIP-PLUS4   TO WS-ZIP-PLUS4
04576                      MOVE '-'            TO WS-DASH
04577                      MOVE WS-ZIP         TO DZIPO
04578                      GO TO 4445-END-OF-BROWSE
04579          ELSE
04580              MOVE AM-NAME        TO  JMAILTOO
04581              MOVE AM-PERSON      TO  JADDR1O
04582              MOVE AM-ADDRS       TO  JADDR2O
04583 *            MOVE AM-CITY        TO  JCITYSTO
                   STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
                      DELIMITED BY '  ' INTO JCITYSTO
                   END-STRING
04584              MOVE AM-TEL-NO      TO WS-WORK-PHONE
04585              INSPECT WS-WORK-PHONE CONVERTING SPACES TO '0'
04586              MOVE WS-NUMERIC-PHONE TO JPHONEO
04587              INSPECT JPHONEI CONVERTING SPACES TO '-'
04588              IF  AM-CANADIAN-POST-CODE
04589                  MOVE AM-CAN-POSTAL-1    TO WS-CAN-POSTAL-CD-1
04590                  MOVE AM-CAN-POSTAL-2    TO WS-CAN-POSTAL-CD-2
04591                  MOVE SPACES             TO WS-DASH-CAN
04592                                             WS-CAN-FILLER
04593                  MOVE WS-CANADIAN-POSTAL-CODES
04594                                          TO JZIPO
04595                  GO TO 4445-END-OF-BROWSE
04596              ELSE
04597                  MOVE AM-ZIP-PRIME       TO WS-ZIP-CODE
04598                  IF AM-ZIP-PLUS4 = SPACES OR ZEROS
04599                      MOVE SPACES         TO WS-ZIP-PLUS4 WS-DASH
04600                      MOVE WS-ZIP         TO JZIPO
04601                      GO TO 4445-END-OF-BROWSE
04602                  ELSE
04603                      MOVE AM-ZIP-PLUS4   TO WS-ZIP-PLUS4
04604                      MOVE '-'            TO WS-DASH
04605                      MOVE WS-ZIP         TO JZIPO
04606                      GO TO 4445-END-OF-BROWSE.
04607
04608      GO TO 4425-READNEXT.
04609
04610  4440-NOT-FOUND.
04611      MOVE ER-0198                TO  EMI-ERROR.
04612      PERFORM 9900-ERROR-FORMAT.
04613
04614  4445-END-OF-BROWSE.
04615      IF WS-READNEXT-SW = +1
04616          
      * EXEC CICS ENDBR
04617 *            DATASET (WS-ACCOUNT-MASTER-DSID)
04618 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00013652' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303133363532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACCOUNT-MASTER-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04619
04620  4449-EXIT.
04621      EXIT.
04622
04623      EJECT
04624  4450-FIND-PRODUCER-MASTER.
04625      MOVE ZERO                   TO  WS-NOT-FOUND.
04626
04627      
      * EXEC CICS HANDLE CONDITION
04628 *        NOTFND (4490-NOT-FOUND)
04629 *    END-EXEC.
      *    MOVE '"$I                   ! * #00013663' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303133363633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04630
04631      
      * EXEC CICS STARTBR
04632 *        DATASET   (WS-PRODUCER-MASTER-DSID)
04633 *        RIDFLD    (WS-PRODUCER-MASTER-KEY)
04634 *        GENERIC   EQUAL
04635 *        KEYLENGTH (13)
04636 *    END-EXEC.
           MOVE 13
             TO DFHEIV11
           MOVE 0
             TO DFHEIV12
      *    MOVE '&,   KG    E          &   #00013667' TO DFHEIV0
           MOVE X'262C2020204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303133363637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-PRODUCER-MASTER-DSID, 
                 WS-PRODUCER-MASTER-KEY, 
                 DFHEIV11, 
                 DFHEIV12, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04637
04638      MOVE +1                     TO  WS-READNEXT-SW.
04639
04640  4455-READNEXT.
04641      
      * EXEC CICS READNEXT
04642 *        DATASET   (WS-PRODUCER-MASTER-DSID)
04643 *        RIDFLD    (WS-PRODUCER-MASTER-KEY)
04644 *        SET       (ADDRESS OF PRODUCER-MASTER)
04645 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00013677' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303133363737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-PRODUCER-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-PRODUCER-MASTER-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PRODUCER-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04646
04647      IF PI-COMPANY-CD = WS-PD-COMPANY-CD AND
04648         PI-CARRIER    = WS-PD-CARRIER    AND
04649         PI-GROUPING   = WS-PD-GROUPING   AND
04650         PI-STATE      = WS-PD-STATE      AND
04651         PI-PRODUCER   = WS-PD-PRODUCER
04652          NEXT SENTENCE
04653        ELSE
04654          GO TO 4490-NOT-FOUND.
04655
04656      IF (PI-CERT-EFF-DT NOT LESS THAN PD-EFFECT-DT
04657        AND PI-CERT-EFF-DT LESS THAN PD-EXPIRE-DATE)
04658          IF AT-TRAILER-TYPE = '4'
04659              MOVE PD-NAME        TO  DMAILTOO
04660              MOVE PD-PERSON      TO  DADDR1O
04661              MOVE PD-ADDRS       TO  DADDR2O
04662              MOVE PD-CITY        TO  DCITYSTO
04663              MOVE PD-TEL-NO      TO WS-WORK-PHONE
04664              INSPECT WS-WORK-PHONE CONVERTING SPACES TO '0'
04665              MOVE WS-NUMERIC-PHONE TO DPHONEO
04666              INSPECT DPHONEI CONVERTING SPACES TO '-'
04667              MOVE PD-ZIP-PRIME       TO WS-ZIP-CODE
04668              IF PD-ZIP-PLUS4 = SPACES OR ZEROS
04669                  MOVE SPACES         TO WS-ZIP-PLUS4 WS-DASH
04670                  MOVE WS-ZIP         TO DZIPO
04671                  GO TO 4495-END-OF-BROWSE
04672              ELSE
04673                  MOVE PD-ZIP-PLUS4   TO WS-ZIP-PLUS4
04674                  MOVE '-'            TO WS-DASH
04675                  MOVE WS-ZIP         TO DZIPO
04676                  GO TO 4495-END-OF-BROWSE
04677          ELSE
04678              MOVE PD-NAME        TO  JMAILTOO
04679              MOVE PD-PERSON      TO  JADDR1O
04680              MOVE PD-ADDRS       TO  JADDR2O
04681              MOVE PD-CITY        TO  JCITYSTO
04682              MOVE PD-TEL-NO      TO WS-WORK-PHONE
04683              INSPECT WS-WORK-PHONE CONVERTING SPACES TO '0'
04684              MOVE WS-NUMERIC-PHONE TO JPHONEO
04685              INSPECT JPHONEI CONVERTING SPACES TO '-'
04686              IF PD-ZIP-PLUS4 = SPACES OR ZEROS
04687                  MOVE SPACES         TO WS-ZIP-PLUS4 WS-DASH
04688                  MOVE WS-ZIP         TO JZIPO
04689                  GO TO 4495-END-OF-BROWSE
04690              ELSE
04691                  MOVE PD-ZIP-PLUS4   TO WS-ZIP-PLUS4
04692                  MOVE '-'            TO WS-DASH
04693                  MOVE WS-ZIP         TO JZIPO
04694                  GO TO 4495-END-OF-BROWSE.
04695
04696      GO TO 4455-READNEXT.
04697
04698  4490-NOT-FOUND.
04699      MOVE ER-9616                TO  EMI-ERROR.
04700      PERFORM 9900-ERROR-FORMAT.
04701
04702  4495-END-OF-BROWSE.
04703      IF WS-READNEXT-SW = +1
04704          
      * EXEC CICS ENDBR
04705 *            DATASET (WS-PRODUCER-MASTER-DSID)
04706 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00013740' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303133373430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-PRODUCER-MASTER-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04707
04708  4499-EXIT.
04709      EXIT.
04710
04711      EJECT
04712  4500-GENERAL-INFO-TRAILER.
04713      IF AT-TRAILER-TYPE NOT = '6'
04714          GO TO 4600-AUTO-PROMPT-TRAILER.
04715
04716      MOVE EL142E                 TO  PI-MAP-NAME.
04717
04718      MOVE AT-TRAILER-TYPE        TO  ETLRTYPO.
04719      MOVE AT-SEQUENCE-NO         TO  ESEQO.
04720
04721      MOVE SPACES                 TO  DC-OPTION-CODE.
04722      MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1.
04723      PERFORM 8500-DATE-CONVERSION.
04724      MOVE DC-GREG-DATE-1-EDIT    TO  ERECDTEO.
04725
04726      MOVE AT-RECORDED-BY         TO  EBYO.
04727
04728      MOVE AT-GEN-INFO-LAST-MAINT-DT    TO  DC-BIN-DATE-1
04729                                            PI-SAVE-LAST-MAINT-DT.
04730      MOVE ' '                          TO  DC-OPTION-CODE.
04731      PERFORM 8500-DATE-CONVERSION.
04732      IF NO-CONVERSION-ERROR
04733          MOVE DC-GREG-DATE-1-EDIT      TO  EMANTONO
04734      ELSE
04735          MOVE SPACES                   TO  EMANTONO.
04736
04737      MOVE AT-GEN-INFO-LAST-UPDATED-BY  TO  EMANTBYO
04738                                            PI-SAVE-LAST-UPD-BY.
04739
04740      MOVE AT-LAST-MAINT-HHMMSS   TO  TIME-IN.
04741      MOVE TIME-OUT               TO  EMANTATO.
04742
04743      MOVE AT-INFO-TRAILER-TYPE   TO  ETYPENO.
04744
04745      IF AT-INFO-TRAILER-TYPE = 'C'
04746          MOVE AT-CALL-TYPE       TO  ECALLO
04747        ELSE
04748          MOVE SPACES             TO  ECALLO
04749                                      ECALLTO.
04750
04751      MOVE AT-INFO-LINE-1         TO  ELINE1O.
04752      MOVE AT-INFO-LINE-2         TO  ELINE2O.
04753
04754      MOVE -1                     TO  EMAINTL.
04755
04756      GO TO 4100-READNEXT.
04757
04758      EJECT
04759  4600-AUTO-PROMPT-TRAILER.
04760      IF AT-TRAILER-TYPE NOT = '7'
04761          GO TO 4700-DENIAL-TRAILER.
04762
04763      MOVE EL142F                 TO  PI-MAP-NAME.
04764
04765      MOVE AT-TRAILER-TYPE        TO  FTLRTYPO.
04766      MOVE AT-SEQUENCE-NO         TO  FSEQO.
04767
04768      MOVE SPACES                 TO  DC-OPTION-CODE.
04769      MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1.
04770      PERFORM 8500-DATE-CONVERSION.
04771      MOVE DC-GREG-DATE-1-EDIT    TO  FRECDTEO.
04772
04773      MOVE AT-RECORDED-BY         TO  FBYO.
04774
04775      MOVE AT-PROMPT-LAST-MAINT-DT    TO  DC-BIN-DATE-1.
04776      MOVE ' '                        TO  DC-OPTION-CODE.
04777      PERFORM 8500-DATE-CONVERSION.
04778      IF NO-CONVERSION-ERROR
04779          MOVE DC-GREG-DATE-1-EDIT    TO  FMANTONO
04780      ELSE
04781          MOVE SPACES                 TO  FMANTONO.
04782
04783      MOVE AT-PROMPT-LAST-UPDATED-BY  TO  FMANTBYO.
04784
04785      MOVE AT-LAST-MAINT-HHMMSS       TO  TIME-IN.
04786      MOVE TIME-OUT                   TO  FMANTATO.
04787
04788      IF AT-PROMPT-START-DT NOT = LOW-VALUES
04789          MOVE SPACES                 TO  DC-OPTION-CODE
04790          MOVE AT-PROMPT-START-DT     TO  DC-BIN-DATE-1
04791          PERFORM 8500-DATE-CONVERSION
04792          MOVE DC-GREG-DATE-1-EDIT    TO  FSNOTIFI.
04793
04794      IF AT-PROMPT-END-DT NOT = LOW-VALUES
04795          MOVE SPACES                 TO  DC-OPTION-CODE
04796          MOVE AT-PROMPT-END-DT       TO  DC-BIN-DATE-1
04797          PERFORM 8500-DATE-CONVERSION
04798          MOVE DC-GREG-DATE-1-EDIT    TO  FENOTIFI.
04799
04800      MOVE AT-PROMPT-LINE-1       TO  FLINE1O.
04801      MOVE AT-PROMPT-LINE-2       TO  FLINE2O.
04802
04803      MOVE -1                     TO  FMAINTL.
04804
04805      GO TO 4100-READNEXT.
04806
04807      EJECT
04808  4700-DENIAL-TRAILER.
04809      IF AT-TRAILER-TYPE NOT = '8'
04810          GO TO 4800-INCURRED-CHANGE-TRAILER.
043019     MOVE AT-CONTROL-PRIMARY     TO WS-CLAIM-KEY
043019
043019     
      * EXEC CICS READ
043019*       DATASET   (WS-CLAIM-MASTER-DSID)
043019*       RIDFLD    (WS-CLAIM-KEY)
043019*       SET       (ADDRESS OF CLAIM-MASTER)
043019*       resp      (ws-response)
043019*    END-EXEC
      *    MOVE '&"S        E          (  N#00013849' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303133383439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CLAIM-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CLAIM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
043019     if resp-normal
043019        move cl-incurred-dt      to pi-incurred-dt
043019     else
043019        move low-values          to pi-incurred-dt
043019     end-if
04812      MOVE EL142G                 TO  PI-MAP-NAME.
04813
04814      MOVE AT-TRAILER-TYPE        TO  GTLRTYPO.
04815      MOVE AT-SEQUENCE-NO         TO  GSEQO.
04816
04817      MOVE SPACES                 TO  DC-OPTION-CODE.
04818      MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1
043019                                     pi-den-recorded-dt
04819      PERFORM 8500-DATE-CONVERSION.
04820      MOVE DC-GREG-DATE-1-EDIT    TO  GRECDTEO.
04821
04822      MOVE AT-RECORDED-BY         TO  GBYO.
04823
04824      MOVE AT-DENIAL-LAST-MAINT-DT    TO  DC-BIN-DATE-1.
04825      MOVE ' '                        TO  DC-OPTION-CODE.
04826      PERFORM 8500-DATE-CONVERSION.
04827      IF NO-CONVERSION-ERROR
04828          MOVE DC-GREG-DATE-1-EDIT    TO  GMANTONO
04829      ELSE
04830          MOVE SPACES                 TO  GMANTONO.
04831
04832      MOVE AT-DENIAL-LAST-UPDATED-BY  TO  GMANTBYO.
04833
04834      MOVE AT-LAST-MAINT-HHMMSS   TO  TIME-IN.
04835      MOVE TIME-OUT               TO  GMANTATO.
04836
04837      MOVE AT-DENIAL-INFO-1       TO  GLINE1O.
04838      MOVE AT-DENIAL-INFO-2       TO  GLINE2O.
04839
04840      IF AT-RETRACTION-DT NOT = LOW-VALUES
04841          MOVE SPACES             TO  DC-OPTION-CODE
04842          MOVE AT-RETRACTION-DT   TO  DC-BIN-DATE-1
04843          PERFORM 8500-DATE-CONVERSION
04844          MOVE DC-GREG-DATE-1-EDIT TO  GRECONSI.
04845
052506     IF AT-DENIAL-PROOF-DT NOT = LOW-VALUES
052506         MOVE SPACES             TO DC-OPTION-CODE
052506         MOVE AT-DENIAL-PROOF-DT TO DC-BIN-DATE-1
052506         PERFORM 8500-DATE-CONVERSION
052506         MOVE DC-GREG-DATE-1-EDIT TO GPRFDTI.
052506
04846      MOVE AT-DENIAL-REASON-CODE  TO  GRSNCDI
042110                                     PI-DENIAL-REASON-CODE
04847
04848      MOVE -1                     TO  BMAINTL.
04849
04850      GO TO 4100-READNEXT.
04851
04852      EJECT
04853  4800-INCURRED-CHANGE-TRAILER.
04854      IF AT-TRAILER-TYPE NOT = '9'
04855          GO TO 4900-FORMS-TRAILER.
04856
04857      MOVE EL142I                 TO  PI-MAP-NAME.
04858
04859      MOVE AT-TRAILER-TYPE        TO  ITLRTYPO.
04860      MOVE AT-SEQUENCE-NO         TO  ISEQO.
04861
04862      MOVE SPACES                 TO  DC-OPTION-CODE.
04863      MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1.
04864      PERFORM 8500-DATE-CONVERSION.
04865      MOVE DC-GREG-DATE-1-EDIT    TO  IRECDTEO.
04866
04867      MOVE AT-RECORDED-BY         TO  IBYO.
04868
04869      IF AT-OLD-INCURRED-DT NOT = LOW-VALUES
04870          MOVE SPACES                 TO  DC-OPTION-CODE
04871          MOVE AT-OLD-INCURRED-DT     TO  DC-BIN-DATE-1
04872          PERFORM 8500-DATE-CONVERSION
04873          MOVE DC-GREG-DATE-1-EDIT    TO  IINCDTO.
04874
04875      IF AT-OLD-REPORTED-DT NOT = LOW-VALUES
04876          MOVE SPACES                 TO  DC-OPTION-CODE
04877          MOVE AT-OLD-REPORTED-DT     TO  DC-BIN-DATE-1
04878          PERFORM 8500-DATE-CONVERSION
04879          MOVE DC-GREG-DATE-1-EDIT    TO  IREPDTO.
04880
04881      IF AT-OLD-ESTABLISHED-DT NOT = LOW-VALUES
04882          MOVE SPACES                 TO  DC-OPTION-CODE
04883          MOVE AT-OLD-ESTABLISHED-DT  TO  DC-BIN-DATE-1
04884          PERFORM 8500-DATE-CONVERSION
04885          MOVE DC-GREG-DATE-1-EDIT    TO  IESTDTO.
04886
04887      MOVE AT-TRAILER-CNT-AT-CHG  TO  ITLRCNTO.
04888      MOVE AT-OLD-TOTAL-PAID      TO  ITAPDO.
04889      MOVE AT-OLD-DAYS-PAID       TO  ITDPDO.
04890      MOVE AT-OLD-NO-OF-PMTS      TO  INOPMTO.
04891
04892      IF AT-OLD-PAID-THRU-DT NOT = LOW-VALUES
04893         MOVE SPACES                 TO  DC-OPTION-CODE
04894         MOVE AT-OLD-PAID-THRU-DT    TO  DC-BIN-DATE-1
04895         PERFORM 8500-DATE-CONVERSION
04896         MOVE DC-GREG-DATE-1-EDIT    TO  IPDTHRUO
04897         IF PI-USES-PAID-TO
04898            MOVE '6'                TO  DC-OPTION-CODE
04899            MOVE AT-OLD-PAID-THRU-DT TO  DC-BIN-DATE-1
04900            MOVE +1                 TO  DC-ELAPSED-DAYS
04901            MOVE +0                 TO  DC-ELAPSED-MONTHS
04902            PERFORM 8500-DATE-CONVERSION
04903            MOVE DC-GREG-DATE-1-EDIT TO  IPDTHRUO.
04904
04905      IF AT-LAST-PMT-MADE-DT NOT = LOW-VALUES
04906          MOVE SPACES                 TO  DC-OPTION-CODE
04907          MOVE AT-LAST-PMT-MADE-DT    TO  DC-BIN-DATE-1
04908          PERFORM 8500-DATE-CONVERSION
04909          MOVE DC-GREG-DATE-1-EDIT    TO  ILSTPDTO.
04910
04911      MOVE AT-OLD-INIT-MAN-RESV     TO  IMANRESO.
04912      MOVE AT-OLD-CURRENT-MAN-RESV  TO  ICURRESO.
04913      MOVE AT-OLD-ADDL-MAN-RESV     TO  IADDRESO.
04914
04915      MOVE AT-OLD-ITD-PAID-EXPENSE  TO  ITEXPDO.
04916      MOVE AT-OLD-CHARGABLE-EXPENSE TO  ICHGEXPO.
04917
04918      MOVE AT-OLD-DIAG-CODE         TO  ICAUSCDO.
04919      MOVE AT-OLD-DIAG-DESCRIP      TO  IDIAGO.
04920
04921      MOVE -1                     TO  IPFKL.
04922
04923      GO TO 4100-READNEXT.
04924
04925      EJECT
04926  4900-FORMS-TRAILER.
04927      IF AT-TRAILER-TYPE NOT = 'A'
04928          GO TO 6000-BAD-RECORD-TYPE.
04929
04930      MOVE AT-CONTROL-PRIMARY     TO  WS-CLAIM-KEY.
04931
04932      
      * EXEC CICS READ
04933 *        DATASET   (WS-CLAIM-MASTER-DSID)
04934 *        RIDFLD    (WS-CLAIM-KEY)
04935 *        SET        (ADDRESS OF CLAIM-MASTER)
04936 *    END-EXEC.
      *    MOVE '&"S        E          (   #00013988' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303133393838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CLAIM-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CLAIM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04937
04938      
      * EXEC CICS HANDLE CONDITION
04939 *        NOTFND (4910-ADDRESS-NOT-FOUND)
04940 *    END-EXEC.
      *    MOVE '"$I                   ! + #00013994' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303133393934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04941
04942      MOVE EL142J                 TO  PI-MAP-NAME.
04943
04944      MOVE +1                     TO  EMI-NUMBER-OF-LINES.
04945      MOVE +2                     TO  EMI-SWITCH2.
04946
04947      MOVE AT-TRAILER-TYPE        TO  JTLRTYPO.
04948      MOVE AT-SEQUENCE-NO         TO  JSEQO.
04949
04950      MOVE SPACES                 TO  DC-OPTION-CODE.
04951      MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1.
04952      PERFORM 8500-DATE-CONVERSION.
04953      MOVE DC-GREG-DATE-1-EDIT    TO  JRECDTEO.
04954
04955      MOVE AT-RECORDED-BY         TO  JBYO.
04956
04957      MOVE AT-FORM-LAST-MAINT-DT      TO  DC-BIN-DATE-1.
04958      MOVE ' '                        TO  DC-OPTION-CODE.
04959      PERFORM 8500-DATE-CONVERSION.
04960      IF NO-CONVERSION-ERROR
04961          MOVE DC-GREG-DATE-1-EDIT    TO  JMANTONO
04962      ELSE
04963          MOVE SPACES                 TO  JMANTONO.
04964
04965      MOVE AT-FORM-LAST-UPDATED-BY    TO  JMANTBYO.
04966
04967      MOVE AT-LAST-MAINT-HHMMSS       TO  TIME-IN.
04968      MOVE TIME-OUT                   TO  JMANTATO.
04969
04970      IF AT-FORM-SEND-ON-DT NOT = LOW-VALUES
04971          MOVE SPACES             TO  DC-OPTION-CODE
04972          MOVE AT-FORM-SEND-ON-DT  TO  DC-BIN-DATE-1
04973          PERFORM 8500-DATE-CONVERSION
04974          MOVE DC-GREG-DATE-1-EDIT TO  JDTSENTI.
04975
04976      IF AT-FORM-RE-SEND-DT NOT = LOW-VALUES
04977          MOVE SPACES              TO  DC-OPTION-CODE
04978          MOVE AT-FORM-RE-SEND-DT  TO  DC-BIN-DATE-1
04979          PERFORM 8500-DATE-CONVERSION
04980          MOVE DC-GREG-DATE-1-EDIT    TO  JRESENDI
04981          IF AT-FORM-REPRINT-DT NOT = LOW-VALUES
04982              MOVE AL-SANOF       TO  JRESENDA
04983            ELSE
04984              NEXT SENTENCE
04985      ELSE
04986          MOVE AL-SANOF           TO  JRESENDA.
04987
04988      IF AT-FORM-FOLLOW-UP-DT NOT = LOW-VALUES
04989          MOVE SPACES             TO  DC-OPTION-CODE
04990          MOVE AT-FORM-FOLLOW-UP-DT TO DC-BIN-DATE-1
04991          PERFORM 8500-DATE-CONVERSION
04992          MOVE DC-GREG-DATE-1-EDIT TO  JREPLYI.
04993
04994      IF AT-FORM-ANSWERED-DT NOT = LOW-VALUES
04995          MOVE SPACES             TO  DC-OPTION-CODE
04996          MOVE AT-FORM-ANSWERED-DT TO DC-BIN-DATE-1
04997          PERFORM 8500-DATE-CONVERSION
04998          MOVE DC-GREG-DATE-1-EDIT TO JRECEVEI.
04999
05000      IF AT-PHY-FORM-ANSWERED-DT NOT = LOW-VALUES AND SPACES
05001          MOVE SPACES                  TO  DC-OPTION-CODE
05002          MOVE AT-PHY-FORM-ANSWERED-DT TO  DC-BIN-DATE-1
05003          PERFORM 8500-DATE-CONVERSION
05004          MOVE DC-GREG-DATE-1-EDIT     TO JPHYRECI.
05005
05006      IF AT-EMP-FORM-ANSWERED-DT NOT = LOW-VALUES AND SPACES
05007          MOVE SPACES                  TO  DC-OPTION-CODE
05008          MOVE AT-EMP-FORM-ANSWERED-DT TO  DC-BIN-DATE-1
05009          PERFORM 8500-DATE-CONVERSION
05010          MOVE DC-GREG-DATE-1-EDIT     TO JEMPRECI.
05011
05012      IF AT-FORM-REM-PRINT-DT NOT = LOW-VALUES AND SPACES
05013          MOVE SPACES                  TO  DC-OPTION-CODE
05014          MOVE AT-FORM-REM-PRINT-DT    TO  DC-BIN-DATE-1
05015          PERFORM 8500-DATE-CONVERSION
05016          MOVE DC-GREG-DATE-1-EDIT     TO JREMDTI.
05017
05018      IF AT-FORM-TYPE = '1'
05019          MOVE 'INITIAL'          TO  JFORMO
05020      ELSE
05021          IF AT-FORM-TYPE = '2'
05022              MOVE 'PROGRESS'     TO  JFORMO
05023          ELSE
05024              MOVE AT-FORM-TYPE   TO  JFORMO.
05025
05026      MOVE AT-INSTRUCT-LN-1       TO  JSI1O.
05027      MOVE AT-INSTRUCT-LN-2       TO  JSI2O.
05028      MOVE AT-INSTRUCT-LN-3       TO  JSI3O.
05029
05030      MOVE AT-REL-CARR-1          TO  JCARR1O.
05031      MOVE AT-REL-CLAIM-1         TO  JCLAIM1O.
05032      MOVE AT-REL-CERT-1          TO  JCERT1O.
05033
05034      MOVE AT-REL-CARR-2          TO  JCARR2O.
05035      MOVE AT-REL-CLAIM-2         TO  JCLAIM2O.
05036      MOVE AT-REL-CERT-2          TO  JCERT2O.
05037
05038      MOVE -1                     TO  JMAINTL.
05039
05040      IF AT-FORM-ADDRESS = SPACE
05041          NEXT SENTENCE
05042        ELSE
05043      IF AT-FORM-ADDRESS       = '3'    AND
05044         AT-FORM-ADDR-SEQ-NO   = ZERO   AND
05045         CL-SYSTEM-IDENTIFIER  = 'CR'
05046          MOVE SPACES             TO  WS-ACCOUNT-MASTER-KEY
05047          MOVE PI-COMPANY-CD      TO  WS-AM-COMPANY-CD
05048          MOVE PI-CARRIER         TO  WS-AM-CARRIER
05049          MOVE PI-GROUPING        TO  WS-AM-GROUPING
05050          MOVE PI-STATE           TO  WS-AM-STATE
05051          MOVE PI-ACCOUNT         TO  WS-AM-ACCOUNT
05052          MOVE PI-CERT-EFF-DT     TO  WS-AM-EXPIRATION-DT
05053          PERFORM 4420-FIND-ACCOUNT-MASTER THRU 4449-EXIT
05054        ELSE
05055      IF AT-FORM-ADDRESS       = '3'    AND
05056         AT-FORM-ADDR-SEQ-NO   = ZERO   AND
05057         CL-SYSTEM-IDENTIFIER  = 'CV'
05058          MOVE SPACES             TO  WS-PRODUCER-MASTER-KEY
05059          MOVE PI-COMPANY-CD      TO  WS-PD-COMPANY-CD
05060          MOVE PI-CARRIER         TO  WS-PD-CARRIER
05061          MOVE PI-GROUPING        TO  WS-PD-GROUPING
05062          MOVE PI-STATE           TO  WS-PD-STATE
05063          MOVE PI-PRODUCER        TO  WS-PD-PRODUCER
05064          MOVE PI-CERT-EFF-DT     TO  WS-PD-EXPIRATION-DT
05065          PERFORM 4450-FIND-PRODUCER-MASTER THRU 4499-EXIT
05066        ELSE
05067      IF AT-FORM-ADDR-SEQ-NO NOT = ZERO
05068          MOVE PI-ACTIVITY-TRAILERS-KEY TO WS-ACTIVITY-TRAILERS-KEY
05069          MOVE AT-FORM-ADDR-SEQ-NO    TO  WS-ATK-SEQUENCE-NO
05070          
      * EXEC CICS READ
05071 *            DATASET (WS-ACTIVITY-TRAILERS-DSID)
05072 *            RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)
05073 *            SET     (ADDRESS OF ACTIVITY-TRAILERS)
05074 *        END-EXEC
      *    MOVE '&"S        E          (   #00014126' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303134313236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ACTIVITY-TRAILERS-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
05075          MOVE AT-MAIL-TO-NAME    TO  JMAILTOO
05076          MOVE AT-ADDRESS-LINE-1  TO  JADDR1O
05077          MOVE AT-ADDRESS-LINE-2  TO  JADDR2O
05078          MOVE AT-CITY-STATE      TO  JCITYSTO
               STRING AT-CITY ' ' AT-STATE
                  DELIMITED BY '  ' INTO JCITYSTO
               END-STRING
05079          MOVE AT-PHONE-NO        TO  JPHONEO
05080          INSPECT JPHONEI CONVERTING SPACES TO '-'
05081          IF  AT-CANADIAN-POST-CODE
05082              MOVE AT-CAN-POSTAL-1    TO WS-CAN-POSTAL-CD-1
05083              MOVE AT-CAN-POSTAL-2    TO WS-CAN-POSTAL-CD-2
05084              MOVE SPACES             TO WS-DASH-CAN
05085                                         WS-CAN-FILLER
05086              MOVE WS-CANADIAN-POSTAL-CODES
05087                                      TO JZIPO
05088          ELSE
05089              MOVE AT-ZIP-CODE        TO WS-ZIP-CODE
05090              IF  AT-ZIP-PLUS4 = SPACES OR ZEROS
05091                  MOVE SPACES         TO WS-ZIP-PLUS4 WS-DASH
05092                  MOVE WS-ZIP         TO JZIPO
05093              ELSE
05094                  MOVE AT-ZIP-PLUS4   TO WS-ZIP-PLUS4
05095                  MOVE '-'            TO WS-DASH
05096                  MOVE WS-ZIP         TO JZIPO.
05097
05098      GO TO 4100-READNEXT.
05099
05100  4910-ADDRESS-NOT-FOUND.
05101      MOVE ER-0388                TO  EMI-ERROR.
05102      PERFORM 9900-ERROR-FORMAT.
05103
05104      GO TO 4100-READNEXT.
05105
05106      EJECT
05107  6000-BAD-RECORD-TYPE.
05108      MOVE 'BAD TRAILER RECORD TYPE - PROBABLE LOGIC ERROR'
05109                                  TO  LOGOFF-MSG.
05110
05111      PERFORM 8300-SEND-TEXT.
05112
05113  6000-END-OF-FILE.
05114      IF PI-MAP-NAME = EL142A
05115          MOVE ER-0344            TO  EMI-ERROR
05116          MOVE -1                 TO  ARECDTEL
05117          
      * EXEC CICS ENDBR
05118 *            DATASET (WS-ACTIVITY-TRAILERS-DSID)
05119 *        END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00014176' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303134313736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
05120          PERFORM 8200-SEND-DATAONLY.
05121
05122      MOVE ER-0303                TO  EMI-ERROR.
05123      MOVE +1                     TO  PI-END-OF-FILE.
05124
05125  6000-ENDBROWSE.
05126      
      * EXEC CICS ENDBR
05127 *        DATASET (WS-ACTIVITY-TRAILERS-DSID)
05128 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00014185' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303134313835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05129
05130      MOVE EIBAID                 TO  PI-PREV-AID.
05131
05132      PERFORM 8100-SEND-INITIAL-MAP.
05133      PERFORM 9100-RETURN-TRAN.
05134
05135  6990-EXIT.
05136      EXIT.
05137
05138      EJECT
05139  5000-DISPLAY-CHECK-QUEUE SECTION.
05140      MOVE +3                     TO  EMI-NUMBER-OF-LINES.
05141      MOVE +2                     TO  EMI-SWITCH2.
05142
05143      MOVE EL142B2                TO  PI-MAP-NAME.
05144
05145      MOVE LOW-VALUES             TO  EL142B2O.
05146
05147      MOVE -1                     TO  KMAINTL.
05148
05149      
      * EXEC CICS READ
05150 *        DATASET  (WS-ACTIVITY-TRAILERS-DSID)
05151 *        RIDFLD   (PI-SAVE-KEY)
05152 *        SET      (ADDRESS OF ACTIVITY-TRAILERS)
05153 *    END-EXEC.
      *    MOVE '&"S        E          (   #00014208' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303134323038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-SAVE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05154
05155      MOVE AT-CLAIM-TYPE          TO  KCTYPEO.
05156
05157      MOVE AT-CARRIER             TO  KCARRO.
05158      MOVE AT-CLAIM-NO            TO  KCLMNOO.
05159      MOVE AT-CERT-NO             TO  KCERTNOO.
05160
05161      MOVE AT-SEQUENCE-NO         TO  KTSEQO.
05162
05163      IF AT-CLAIM-PREM-TYPE = '1'
05164          MOVE 'SP'               TO  KCOVERO
05165        ELSE
05166      IF AT-CLAIM-PREM-TYPE = '2'
05167          MOVE 'OB'               TO  KCOVERO
05168        ELSE
05169      IF AT-CLAIM-PREM-TYPE = '3'
05170          MOVE 'OE'               TO  KCOVERO
05171        ELSE
05172          MOVE AT-CLAIM-PREM-TYPE TO  KCOVERO.
05173
05174      MOVE PI-COMPANY-CD          TO  WS-CQ-COMPANY-CD.
05175      MOVE AT-CHECK-QUE-CONTROL   TO  WS-CQ-CONTROL-NUMBER
05176                                      KCONTRLO.
05177      MOVE AT-CHECK-QUE-SEQUENCE  TO  WS-CQ-SEQUENCE-NUMBER
05178                                      KSEQO.
05179
05180      MOVE AT-CHECK-NO            TO  KCKNOO.
05181      MOVE AT-AMOUNT-PAID         TO  KCKAMTO.
05182
05183      IF AT-CHECK-WRITTEN-DT NOT = LOW-VALUES
05184          MOVE AT-CHECK-WRITTEN-DT TO  DC-BIN-DATE-1
05185          MOVE SPACES              TO  DC-OPTION-CODE
05186          PERFORM 8500-DATE-CONVERSION
05187          MOVE DC-GREG-DATE-1-EDIT TO  KCKDATEO.
05188
05189      MOVE AT-RECORDED-BY         TO  KBYO.
05190
05191      IF AT-CV-PMT-CODE = ' '
05192          GO TO 5000-DISPLAY-CHECK-DESC.
05193
05194      IF AT-CV-PMT-CODE = '1'
05195          MOVE 'FULL DEATH     '         TO  KPAYTYPO.
05196
05197      IF AT-CV-PMT-CODE = '2'
05198          MOVE 'HALF DEATH     '         TO  KPAYTYPO.
05199
05200      IF AT-CV-PMT-CODE = '3'
05201          MOVE 'FULL AD&D      '         TO  KPAYTYPO.
05202
05203      IF AT-CV-PMT-CODE = '4'
05204          MOVE 'HALF AD&D      '         TO  KPAYTYPO.
05205
05206      IF AT-CV-PMT-CODE = '5'
05207          MOVE 'FULL RIDER     '         TO  KPAYTYPO.
05208
05209      IF AT-CV-PMT-CODE = '6'
05210          MOVE 'HALF RIDER     '         TO  KPAYTYPO.
05211
05212      IF AT-CV-PMT-CODE = '7'
05213          MOVE 'NON-CHG EXP    '         TO  KPAYTYPO.
05214
05215      IF AT-CV-PMT-CODE = '8'
05216          MOVE 'ADDITIONAL     '         TO  KPAYTYPO.
05217
05218      GO TO 5000-DISPLAY-CHECK-QUE-CONT.
05219
05220  5000-DISPLAY-CHECK-DESC.
05221
05222      IF AT-PAYMENT-TYPE = '1'
05223          MOVE 'PARTIAL PAYMENT'         TO  KPAYTYPO
05224        ELSE
05225      IF AT-PAYMENT-TYPE = '2'
05226          MOVE 'FINAL PAYMENT'           TO  KPAYTYPO
05227        ELSE
05228      IF AT-PAYMENT-TYPE = '3'
05229          MOVE 'LUMP SUM PAYMENT'        TO  KPAYTYPO
05230        ELSE
05231      IF AT-PAYMENT-TYPE = '4'
05232          MOVE 'ADDITIONAL PAYMENT'      TO  KPAYTYPO
05233        ELSE
05234      IF AT-PAYMENT-TYPE = '5'
05235          MOVE 'CHARGEABLE PAYMENT'      TO  KPAYTYPO
05236        ELSE
05237      IF AT-PAYMENT-TYPE = '6'
05238          MOVE 'NON-CHARGEABLE PAYMENT'  TO  KPAYTYPO
05239        ELSE
05240      IF AT-PAYMENT-TYPE = '7'
05241          MOVE 'LIFE PREMIUM REFUND'     TO  KPAYTYPO
05242        ELSE
05243      IF AT-PAYMENT-TYPE = '8'
05244          MOVE 'A & H PREMIUM REFUND'    TO  KPAYTYPO
05245        ELSE
05246          MOVE 'ENTRY CORRECTION'        TO  KPAYTYPO.
05247
05248  5000-DISPLAY-CHECK-QUE-CONT.
05249
05250      IF AT-VOID-DT NOT = LOW-VALUES
05251          MOVE 'YES'              TO  KVOIDO
05252        ELSE
05253          MOVE 'NO'               TO  KVOIDO.
05254
05255      
      * EXEC CICS HANDLE CONDITION
05256 *        NOTFND (5020-DISPLAY-CHECK-QUEUE)
05257 *    END-EXEC.
      *    MOVE '"$I                   ! , #00014314' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303134333134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05258
05259      
      * EXEC CICS READ
05260 *        DATASET (WS-CHECK-QUEUE-DSID)
05261 *        RIDFLD  (WS-CHECK-QUEUE-KEY)
05262 *        SET     (ADDRESS OF CHECK-QUE)
05263 *    END-EXEC.
      *    MOVE '&"S        E          (   #00014318' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303134333138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CHECK-QUEUE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05264
05265      MOVE CQ-ENTRY-TYPE          TO  KTYPEO.
05266
05267      IF CQ-CHECK-NUMBER NOT = AT-CHECK-NO
05268          MOVE CQ-CHECK-NUMBER    TO  KCKNO2O
05269          MOVE ER-0574            TO  EMI-ERROR
05270          PERFORM 9900-ERROR-FORMAT.
05271
05272      IF CQ-PMT-TRLR-SEQUENCE NOT = AT-SEQUENCE-NO
05273          MOVE ER-0575            TO  EMI-ERROR
05274          PERFORM 9900-ERROR-FORMAT.
05275
05276      IF CQ-CHECK-AMOUNT NOT = AT-AMOUNT-PAID
05277          MOVE ER-0576            TO  EMI-ERROR
05278          PERFORM 9900-ERROR-FORMAT.
05279
05280      IF CQ-CHECK-WRITTEN-DT NOT = AT-CHECK-WRITTEN-DT
05281          MOVE ER-0577            TO  EMI-ERROR
05282          PERFORM 9900-ERROR-FORMAT.
05283
05284      MOVE CQ-TIMES-PRINTED       TO  KTIMPRTO.
05285
05286      IF CHECKS-WERE-PRE-NUMBERED
05287          MOVE 'YES'              TO  KPRENOO
05288      ELSE
05289          MOVE 'NO '              TO  KPRENOO.
05290
05291      GO TO 5080-DISPLAY-CHECK-QUEUE.
05292
05293  5020-DISPLAY-CHECK-QUEUE.
05294      MOVE 'A'                    TO  KMAINTO.
05295      MOVE AL-SANON               TO  KMAINTA.
05296
05297      MOVE -1                     TO  KCONTRLO.
05298
05299      MOVE ER-0578                TO  EMI-ERROR.
05300      PERFORM 9900-ERROR-FORMAT.
05301
05302  5080-DISPLAY-CHECK-QUEUE.
05303      PERFORM 8100-SEND-INITIAL-MAP.
05304
05305  5090-EXIT.
05306      EXIT.
05307
05308      EJECT
05309
102510 6000-DISPLAY-ELNAPS SECTION.
05311      MOVE +3                     TO  EMI-NUMBER-OF-LINES.
05312      MOVE +2                     TO  EMI-SWITCH2.
05313
05314      MOVE EL142D2                TO  PI-MAP-NAME.
05315
05316      MOVE LOW-VALUES             TO  EL142D2O.
05317
05318      MOVE -1                     TO LPFKL.
05319
05320      
      * EXEC CICS READ
05321 *        DATASET  (WS-ACTIVITY-TRAILERS-DSID)
05322 *        RIDFLD   (PI-SAVE-KEY)
05323 *        SET      (ADDRESS OF ACTIVITY-TRAILERS)
05324 *    END-EXEC.
      *    MOVE '&"S        E          (   #00014379' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303134333739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-SAVE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05325
102510     MOVE PI-COMPANY-CD          TO  WS-NA-COMPANY-CD.
102510     MOVE PI-SAVE-ATK-CARRIER    TO  WS-NA-CARRIER.
102510     MOVE PI-SAVE-ATK-CLAIM-NO   TO  WS-NA-CLAIM-NO.
102510     MOVE PI-SAVE-ATK-CERT-NO    TO  WS-NA-CERT-NO.
102510     MOVE AT-LETTER-ARCHIVE-NO   TO  WS-NA-ARCHIVE-NO.
05330
05331      
      * EXEC CICS HANDLE CONDITION
102510*        ENDFILE (6020-ELNAPS-NOTFND)
102510*        NOTFND  (6020-ELNAPS-NOTFND)
05334 *    END-EXEC.
      *    MOVE '"$''I                  ! - #00014391' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2D20233030303134333931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05335
05336      
      * EXEC CICS READ
102510*         DATASET (WS-NAPERSOFT-DSID)
102510*         RIDFLD  (WS-NAPERSOFT-KEY)
102510*         SET     (ADDRESS OF NAPERSOFT-FILE)
05340 *    END-EXEC.
      *    MOVE '&"S        E          (   #00014396' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303134333936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-NAPERSOFT-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-NAPERSOFT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF NAPERSOFT-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05341
102510     MOVE NA-CARRIER             TO LCARRO.
102510     MOVE NA-CLAIM-NO            TO LCLMNOO.
102510     MOVE NA-CERT-NO             TO LCRTNOO.
102510     MOVE NA-ARCHIVE-NO          TO LARCHNOO.
102510     MOVE NA-LETTER-ID           TO LLETRIDO.
102510     MOVE NA-PROCESSOR-ID        TO LPROCO.
102510     MOVE NA-RESEND-LETTER-ID    TO LRSLTIDO.
102510     MOVE NA-NO-OF-COPIES        TO LNOCPYSO.
102510     MOVE NA-ADDRESS-TYPE        TO LADDTYPO.
102510     MOVE NA-CORR-TRLR-SEQ       TO LCORSEQO.
102510     MOVE NA-ENCLOSURE-CD        TO LENCCODO.
102510     MOVE NA-CREATED-IN-NAPERSOFT TO LCREATNO.
102510     MOVE NA-ORIG-ARCHIVE-NO     TO LORIGARO.
102510     MOVE NA-RESEND-PROMPT-IND   TO LPROMPTO.
102510     MOVE NA-ORIG-ENCLOSURE-CD   TO LORIGENO.
05347
102510     IF NA-CREATION-DT NOT = LOW-VALUES
102510         MOVE NA-CREATION-DT     TO  DC-BIN-DATE-1
05358          MOVE SPACES             TO  DC-OPTION-CODE
05359          PERFORM 8500-DATE-CONVERSION
05360          MOVE DC-GREG-DATE-1-EDIT TO LCREDTEO.
05361
102510     IF NA-INITIAL-PRINT-DT NOT = LOW-VALUES
102510         MOVE NA-INITIAL-PRINT-DT TO  DC-BIN-DATE-1
05364          MOVE SPACES             TO  DC-OPTION-CODE
05365          PERFORM 8500-DATE-CONVERSION
05366          MOVE DC-GREG-DATE-1-EDIT TO LINPRNTO.
05367
102510     IF NA-FOLLOW-UP-DT NOT = LOW-VALUES
102510         MOVE NA-FOLLOW-UP-DT    TO  DC-BIN-DATE-1
102510         MOVE SPACES             TO  DC-OPTION-CODE
102510         PERFORM 8500-DATE-CONVERSION
102510         MOVE DC-GREG-DATE-1-EDIT TO LFUPDTEO.
102510
102510     IF NA-RESEND-DT NOT = LOW-VALUES
102510         MOVE NA-RESEND-DT       TO  DC-BIN-DATE-1
102510         MOVE SPACES             TO  DC-OPTION-CODE
102510         PERFORM 8500-DATE-CONVERSION
102510         MOVE DC-GREG-DATE-1-EDIT TO LRESDTEO.
102510
102510     IF NA-RESEND-PRINT-DT NOT = LOW-VALUES
102510         MOVE NA-RESEND-PRINT-DT TO  DC-BIN-DATE-1
05370          MOVE SPACES             TO  DC-OPTION-CODE
05371          PERFORM 8500-DATE-CONVERSION
05372          MOVE DC-GREG-DATE-1-EDIT  TO LREPRNTO.
05373
102510     IF NA-1ST-LTR-PRINT-DT NOT = LOW-VALUES
102510         MOVE NA-1ST-LTR-PRINT-DT TO  DC-BIN-DATE-1
102510         MOVE SPACES             TO  DC-OPTION-CODE
102510         PERFORM 8500-DATE-CONVERSION
102510         MOVE DC-GREG-DATE-1-EDIT TO L1STPRTO.
102510
102510     IF NA-NEXT-DUE-DT NOT = LOW-VALUES
102510         MOVE NA-NEXT-DUE-DT     TO  DC-BIN-DATE-1
102510         MOVE SPACES             TO  DC-OPTION-CODE
102510         PERFORM 8500-DATE-CONVERSION
102510         MOVE DC-GREG-DATE-1-EDIT TO LNXTDUEO.
102510
102510     IF NA-AUTOPYDT NOT = LOW-VALUES
102510         MOVE NA-AUTOPYDT        TO  DC-BIN-DATE-1
102510         MOVE SPACES             TO  DC-OPTION-CODE
102510         PERFORM 8500-DATE-CONVERSION
102510         MOVE DC-GREG-DATE-1-EDIT TO LAUTOPYO.
05375
05376      GO TO 8100-SEND-INITIAL-MAP.
05377
102510 6020-ELNAPS-NOTFND.
05379      MOVE ER-0006                TO  EMI-ERROR.
05380      PERFORM 9900-ERROR-FORMAT.
05381
05382      GO TO 8100-SEND-INITIAL-MAP.
05383      EJECT
021114
021114
021114 7000-CONNECT-TO-DB.
021114
011822     MOVE 'SDVDB01_PdBnkInfo'     TO SVR
011822     MOVE 'appuser'              TO USR
011822     MOVE 'appuser@cso'          TO PASS
021114
021114     STRING
021114         USR DELIMITED SPACE
021114         "." DELIMITED SIZE
021114         PASS DELIMITED SPACE INTO USR-PASS
021114     END-STRING
021114
021114     EXEC SQL
021114        CONNECT TO :SVR USER :USR-PASS
021114     END-EXEC
021114
021114     IF SQLCODE NOT = 0
021114        DISPLAY "ERROR: CANNOT CONNECT "
021114        DISPLAY SQLCODE
021114        DISPLAY SQLERRMC
021114        GO TO 7000-EXIT
021114     END-IF
021114
021114     .
021114 7000-EXIT.
021114     EXIT.
021114
021114 7100-GET-CHK-CASHED-DT.
021114
051215     move spaces                 to ws-check-cashed-dt
051215     move zeros                  to sqlcode
111714     move '2'                    to ws-draft-or-check  *>  draft
111714     if at-check-written-dt not = low-values and spaces
111714        evaluate true
060315           when (at-check-written-dt > X'ACFE')    *> 04/30/2015
090415              and (pi-company-id not = 'DCC')
111714*          when at-check-written-dt >= X'AC93'    *> 01/19/2015
111714              move '1'           to ws-draft-or-check  *>  check
111714              move '000'         to ws-check-number (1:3)
111714              move at-check-no   to ws-check-number (4:7)
013017           when (at-check-written-dt > X'B016')    *> 05/22/2017
013017              and (pi-company-id = 'DCC')
013017              move '1'           to ws-draft-or-check  *>  check
013017              move '000'         to ws-check-number (1:3)
013017              move at-check-no   to ws-check-number (4:7)
111714           when at-payment-type = 'I'
111714              move '000'         to ws-check-number (1:3)
111714              move at-check-no   to ws-check-number (4:7)
111714           when other
111714              move '0'           to ws-check-number (1:1)
111714              move at-check-no (1:1)
111714                                 to ws-check-number (2:1)
111714              move '00'          to ws-check-number (3:2)
111714              move at-check-no (2:6)
111714                                 to ws-check-number (5:6)
111714        end-evaluate
051215     else   *>  no check written date yet
051215        go to 7100-exit
111714     end-if
111714     evaluate true
111714        when pi-company-id = 'AHL'
111714           move '%AHL%'          to ws-check-company
111714        when pi-company-id = 'DCC'
111714           move '%DCC%'          to ws-check-company
020816        when pi-company-id = 'VPP'
020816           move '%VPP%'          to ws-check-company
062121        when pi-company-id = 'FNL'
062121           move '%FNL%'          to ws-check-company
111714        when other   *>   CID s/b   CSO
111714           move '%CSO%'          to ws-check-company
111714     end-evaluate
021114     MOVE AT-PAYMENT-TYPE        TO WS-CHECK-TYPE
111714     move at-claim-no            to ws-claim-number
021114     MOVE AT-AMOUNT-PAID         TO WS-CHECK-AMT-TMP
021114     MOVE WS-CHECK-AMT-TMPX      TO WS-CHECK-AMOUNT
021114     MOVE SPACES                 TO WS-CHECK-CASHED-DT
021114
021114     EXEC SQL
021114        CALL LogicPaidBankChkCashedDt
111714           @draftorcheck    = :WS-DRAFT-OR-CHECK,
021114           @checktype       = :WS-CHECK-TYPE,
111714           @claimnumber     = :WS-CLAIM-NUMBER,
021114           @checknumber     = :WS-CHECK-NUMBER,
021114           @checkamount     = :WS-CHECK-AMOUNT,
021114           @checkcompany    = :WS-CHECK-COMPANY,
021114           @checkcasheddate = :WS-CHECK-CASHED-DT OUT
021114     END-EXEC
021114
021114     IF SQLCODE NOT = 0
021114        MOVE SPACES TO WS-CHECK-CASHED-DT
021114*        DISPLAY "ERROR: DID NOT RETURN CHK CASHED DT "
021114*        DISPLAY ' SQL RETURN CODE ' SQLCODE
021114*        DISPLAY ' SQL ERR MESS    ' SQLERRMC
021114        GO TO 7100-EXIT
021114     END-IF
021114
021114     .
021114 7100-EXIT.
021114     EXIT.
021114
091714 7110-CHECK-MANUAL.
091714
091714     MOVE AT-PAYMENT-TYPE        TO WS-CHECK-TYPE
111714     MOVE AT-CHECK-NO            TO WS-CHECK-NUMBER-man
091714     MOVE AT-AMOUNT-PAID         TO WS-CHECK-AMT-TMP
091714     MOVE WS-CHECK-AMT-TMPX      TO WS-CHECK-AMOUNT
111714     MOVE PI-COMPANY-ID          TO WS-CHECK-COMPANY-man
091714     MOVE SPACES                 TO WS-CHECK-CASHED-DT
091714
091714     EXEC SQL
091714       CALL LogicPaidBankChkCashedDtManual
091714                 @checktype = :WS-CHECK-TYPE,
111714                 @checknumber = :WS-CHECK-NUMBER-man,
091714                 @checkamount = :WS-CHECK-AMOUNT,
111714                 @checkcompany = :WS-CHECK-COMPANY-man,
091714                 @checkcasheddate = :WS-CHECK-CASHED-DT OUT
091714     END-EXEC
091714
091714     IF SQLCODE NOT = 0
091714        MOVE SPACES TO WS-CHECK-CASHED-DT
091714*        DISPLAY "ERROR: DID NOT RETURN CHK CASHED DT "
091714*        DISPLAY ' SQL RETURN CODE ' SQLCODE
091714*        DISPLAY ' SQL ERR MESS    ' SQLERRMC
091714*       GO TO 7100-EXIT
091714     END-IF
091714
091714     .
091714 7110-EXIT.
091714     EXIT.
021114
021114 7200-DISCONNECT.
021114
021114     EXEC SQL
021114        DISCONNECT
021114     END-EXEC
021114     .
021114 7200-EXIT.
021114     EXIT.
021114
05384
05385  8000-CREATE-DMO-REC.
05386      MOVE PI-COMPANY-CD          TO NOTE-COMP-CD.
05387      MOVE WS-CL-CERT-KEY-DATA    TO NOTE-CERT-KEY.
05388      MOVE WS-CL-CERT-NO          TO NOTE-CERT-NO.
05389
05390      
      * EXEC CICS HANDLE CONDITION
05391 *         NOTFND   (8000-NOTE-NOT-FOUND)
05392 *    END-EXEC.
      *    MOVE '"$I                   ! . #00014621' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2E20233030303134363231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05393
05394      
      * EXEC CICS READ
05395 *         DATASET(WS-NOTE-FILE-DSID)
05396 *         SET    (ADDRESS OF CERTIFICATE-NOTE)
05397 *         RIDFLD (NOTE-KEY)
05398 *    END-EXEC.
      *    MOVE '&"S        E          (   #00014625' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303134363235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-NOTE-FILE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 NOTE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-NOTE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05399
05400      MOVE SPACES                 TO DCT-COMMUNICATION-AREA.
05401      MOVE WS-CL-BENEFICIARY      TO DCT-LOGIC-BENEFICIARY-ID.
05402      MOVE WS-CL-CCN              TO DCT-CREDIT-CARD-NUMBER.
05403
05404      IF PI-GROUPING (5:2) = ZEROS OR SPACES
05405          MOVE 'CC'               TO DCT-PRODUCT-CODE
05406      ELSE
05407          MOVE PI-GROUPING (5:2)  TO DCT-PRODUCT-CODE.
05408
05409      MOVE CN-CSI-CC-BILL-BANK-ID TO DCT-BILLING-BANK-ID.
05410      MOVE '02'                   TO DCT-COLUMN-ID-REQUESTED.
05411
05412      
      * EXEC CICS LINK
05413 *        PROGRAM    ('DLO006')
05414 *        COMMAREA   (DCT-COMMUNICATION-AREA)
05415 *        LENGTH     (WS-DCT-LENGTH)
05416 *    END-EXEC.
           MOVE 'DLO006' TO DFHEIV1
      *    MOVE '."C                   (   #00014643' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303134363433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DCT-COMMUNICATION-AREA, 
                 WS-DCT-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05417
05418      IF DCT-RETURN-CODE = 'OK'
05419          GO TO 8000-CONT.
05420
05421      IF DCT-RETURN-CODE = '01' OR '02'
05422          GO TO 8000-EXIT.
05423
05424      IF DCT-RETURN-CODE = '03'
05425          MOVE ER-0951            TO EMI-ERROR
05426          MOVE -1                 TO GMAINTL
05427          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05428          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
05429          GO TO 8200-SEND-DATAONLY.
05430
05431      IF DCT-RETURN-CODE = '06'
05432          MOVE ER-0921            TO EMI-ERROR
05433          MOVE -1                 TO GMAINTL
05434          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05435          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
05436          GO TO 8200-SEND-DATAONLY.
05437
05438      IF DCT-RETURN-CODE = '07'
05439          MOVE ER-0919            TO EMI-ERROR
05440          MOVE -1                 TO GMAINTL
05441          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05442          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
05443          GO TO 8200-SEND-DATAONLY.
05444
05445      IF DCT-RETURN-CODE = '04'
05446          MOVE ER-0946            TO EMI-ERROR
05447          MOVE -1                 TO GMAINTL
05448          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05449          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
05450          GO TO 8200-SEND-DATAONLY.
05451
05452      IF DCT-RETURN-CODE = '05'
05453          MOVE ER-0947            TO EMI-ERROR
05454          MOVE -1                 TO GMAINTL
05455          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05456          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
05457          GO TO 8200-SEND-DATAONLY.
05458
05459      IF DCT-RETURN-CODE = '08'
05460          MOVE ER-0948            TO EMI-ERROR
05461          MOVE -1                 TO GMAINTL
05462          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05463          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
05464          GO TO 8200-SEND-DATAONLY.
05465
05466      IF DCT-RETURN-CODE = 'N1'
05467          MOVE ER-0950            TO EMI-ERROR
05468          MOVE -1                 TO GMAINTL
05469          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05470          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
05471          GO TO 8200-SEND-DATAONLY.
05472
05473      IF DCT-RETURN-CODE = 'E1'
05474          MOVE ER-0974            TO EMI-ERROR
05475          MOVE -1                 TO GMAINTL
05476          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05477          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
05478          GO TO 8200-SEND-DATAONLY.
05479
05480      IF DCT-RETURN-CODE = 'E2'
05481          MOVE ER-0975            TO EMI-ERROR
05482          MOVE -1                 TO GMAINTL
05483          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05484          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
05485          GO TO 8200-SEND-DATAONLY.
05486
05487      IF DCT-RETURN-CODE NOT = 'OK'
05488           MOVE ER-0949            TO EMI-ERROR
05489           MOVE -1                 TO GMAINTL
05490           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05491           PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
05492           GO TO 8200-SEND-DATAONLY.
05493
05494  8000-CONT.
05495
05496      MOVE SPACES                 TO DMO-COMMUNICATION-AREA.
05497      MOVE 'CS'                   TO DM-RECORD-TYPE.
05498      MOVE DCT-DISTRIBUTION-CODE  TO DM-DIST-CODE.
05499      MOVE DCT-MAIL-CODE          TO DM-MAIL-CODE.
05500      MOVE WS-CL-CLAIM-NO         TO DM-CLAIM-NO.
05501      MOVE WS-CL-CERT-NO (4:1)    TO DM-CLAIM-TYPE.
05502      MOVE WS-CL-CCN              TO DM-CREDIT-CARD-NUMBER.
05503      MOVE SAVE-DATE-CCYYMMDD     TO DM-STATUS-DATE.
05504
05505      MOVE WS-CL-INSURED-LAST-NAME TO W-NAME-LAST.
05506      MOVE WS-CL-INSURED-1ST-NAME  TO W-NAME-FIRST.
05507      MOVE WS-CL-INSURED-MID-INIT  TO W-NAME-MIDDLE.
05508      PERFORM 8050-FORMAT-LAST-NAME-1ST THRU 8050-EXIT.
05509      MOVE WS-NAME-WORK           TO DM-INSURED-NAME.
05510
05511      MOVE 'R'                    TO DM-STAT-CHANGE-TYPE.
05512
05513      IF WS-CL-NO-OF-PMTS-MADE = 0
05514          MOVE '1'                TO DM-CLAIM-STATUS
05515       ELSE
05516          MOVE '2'                TO DM-CLAIM-STATUS.
05517
05518      MOVE WS-CL-CERT-CARRIER     TO DM-STAT-CARRIER.
05519
05520      
      * EXEC CICS LINK
05521 *        PROGRAM    ('DLO025')
05522 *        COMMAREA   (DMO-COMMUNICATION-AREA)
05523 *        LENGTH     (WS-DMO-LENGTH)
05524 *    END-EXEC.
           MOVE 'DLO025' TO DFHEIV1
      *    MOVE '."C                   (   #00014751' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303134373531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DMO-COMMUNICATION-AREA, 
                 WS-DMO-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05525
05526      IF DM-RETURN-CODE = 'OK'
05527          GO TO 8000-EXIT.
05528
05529      IF DM-RETURN-CODE = '01'
05530          MOVE ER-8051            TO EMI-ERROR
05531          MOVE -1                 TO GMAINTL
05532          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05533          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
05534          GO TO 8200-SEND-DATAONLY.
05535
05536      IF DM-RETURN-CODE = '02'
05537          MOVE ER-8052            TO EMI-ERROR
05538          MOVE -1                 TO GMAINTL
05539          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05540          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
05541          GO TO 8200-SEND-DATAONLY.
05542
05543      IF DM-RETURN-CODE = '03'
05544          MOVE ER-8053            TO EMI-ERROR
05545          MOVE -1                 TO GMAINTL
05546          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05547          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
05548          GO TO 8200-SEND-DATAONLY.
05549
05550      IF DM-RETURN-CODE = '04'
05551          MOVE ER-8054            TO EMI-ERROR
05552          MOVE -1                 TO GMAINTL
05553          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05554          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
05555          GO TO 8200-SEND-DATAONLY.
05556
05557      IF DM-RETURN-CODE = '05'
05558          MOVE ER-8055            TO EMI-ERROR
05559          MOVE -1                 TO GMAINTL
05560          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05561          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
05562          GO TO 8200-SEND-DATAONLY.
05563
05564      IF DM-RETURN-CODE = '06'
05565          MOVE ER-8056            TO EMI-ERROR
05566          MOVE -1                 TO GMAINTL
05567          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05568          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
05569          GO TO 8200-SEND-DATAONLY.
05570
05571      IF DM-RETURN-CODE = '07'
05572          MOVE ER-8057            TO EMI-ERROR
05573          MOVE -1                 TO GMAINTL
05574          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05575          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
05576          GO TO 8200-SEND-DATAONLY.
05577
05578      IF DM-RETURN-CODE = '08'
05579          MOVE ER-8058            TO EMI-ERROR
05580          MOVE -1                 TO GMAINTL
05581          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05582          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
05583          GO TO 8200-SEND-DATAONLY.
05584
05585      IF DM-RETURN-CODE = '09'
05586          MOVE ER-8059            TO EMI-ERROR
05587          MOVE -1                 TO GMAINTL
05588          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05589          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
05590          GO TO 8200-SEND-DATAONLY.
05591
05592      IF DM-RETURN-CODE = '10'
05593          MOVE ER-8060            TO EMI-ERROR
05594          MOVE -1                 TO GMAINTL
05595          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05596          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
05597          GO TO 8200-SEND-DATAONLY.
05598
05599      IF DM-RETURN-CODE = '11'
05600          MOVE ER-8061            TO EMI-ERROR
05601          MOVE -1                 TO GMAINTL
05602          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05603          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
05604          GO TO 8200-SEND-DATAONLY.
05605
05606      IF DM-RETURN-CODE = '12'
05607          MOVE ER-8062            TO EMI-ERROR
05608          MOVE -1                 TO GMAINTL
05609          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05610          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
05611          GO TO 8200-SEND-DATAONLY.
05612
05613      IF DM-RETURN-CODE = '13'
05614          MOVE ER-8063            TO EMI-ERROR
05615          MOVE -1                 TO GMAINTL
05616          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05617          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
05618          GO TO 8200-SEND-DATAONLY.
05619
05620      IF DM-RETURN-CODE = '14'
05621          MOVE ER-8064            TO EMI-ERROR
05622          MOVE -1                 TO GMAINTL
05623          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05624          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
05625          GO TO 8200-SEND-DATAONLY.
05626
05627      IF DM-RETURN-CODE = '15'
05628          MOVE ER-8065            TO EMI-ERROR
05629          MOVE -1                 TO GMAINTL
05630          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05631          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
05632          GO TO 8200-SEND-DATAONLY.
05633
05634      IF DM-RETURN-CODE = '16'
05635          MOVE ER-8154            TO EMI-ERROR
05636          MOVE -1                 TO GMAINTL
05637          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05638          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
05639          GO TO 8200-SEND-DATAONLY.
05640
05641      IF DM-RETURN-CODE = '17'
05642          MOVE ER-8155            TO EMI-ERROR
05643          MOVE -1                 TO GMAINTL
05644          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05645          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
05646          GO TO 8200-SEND-DATAONLY.
05647
05648      IF DM-RETURN-CODE = 'N1'
05649          MOVE ER-8152            TO EMI-ERROR
05650          MOVE -1                 TO GMAINTL
05651          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05652          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
05653          GO TO 8200-SEND-DATAONLY.
05654
05655      IF DM-RETURN-CODE = 'E1'
05656          MOVE ER-8153            TO EMI-ERROR
05657          MOVE -1                 TO GMAINTL
05658          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05659          PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT
05660          GO TO 8200-SEND-DATAONLY.
05661
05662      MOVE ER-8066                TO EMI-ERROR.
05663      MOVE -1                     TO GMAINTL.
05664      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
05665      PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT.
05666      GO TO 8200-SEND-DATAONLY.
05667
05668  8000-NOTE-NOT-FOUND.
05669      MOVE ER-0954                TO EMI-ERROR.
05670      MOVE -1                     TO GMAINTL.
05671      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
05672      PERFORM 8070-UNLOCK-TRLR THRU 8070-EXIT.
05673      GO TO 8200-SEND-DATAONLY.
05674
05675  8000-EXIT.
05676      EXIT.
05677  EJECT
05678  8050-FORMAT-LAST-NAME-1ST.
05679 *****************************************************************
05680 *             M O V E   N A M E   R O U T I N E                 *
05681 *                                                               *
05682 *     THE FOLLOWING ROUTINE REARRANGES A GIVEN NAME SO          *
05683 *     THAT IT READS LAST, FIRST, MIDDLE.  PLACE NAME            *
05684 *     FIELDS IN THE FOLLOWING WORKING-STORAGE FIELDS.           *
05685 *                                                               *
05686 *                  FIELD               VALUE                    *
05687 *                  -----               -----                    *
05688 *           W-NAME-LAST    (CL15)      SMITH                    *
05689 *           W-NAME-FIRST   (CL15)      JOHN                     *
05690 *           W-NAME-MIDDLE  (CL15)      ALLEN/A                  *
05691 *                                                               *
05692 *     AFTER NAME HAS BEEN MOVED WS-NAME-WORK WILL CONTAIN       *
05693 *                SMITH, JOHN ALLEN                              *
05694 *                     OR                                        *
05695 *                SMITH, JOHN A.                                 *
05696 *                                                               *
05697 *     TO USE THIS ROUTINE YOU NEED THE WORKING-STORAGE          *
05698 *     COPYBOOK, ELCNWA.                                         *
05699 *****************************************************************.
05700
05701      MOVE SPACES                 TO WS-NAME-WORK-AREA.
05702      MOVE ZERO                   TO WS-NAME-SW.
05703      SET NWA-INDEX               TO +1.
05704
05705      IF W-NAME-LAST   = SPACES  AND
05706         W-NAME-MIDDLE = SPACES
05707           MOVE +1                TO WS-NAME-SW.
05708
05709      MOVE W-NAME-LAST            TO WS-NAME-WORK2.
05710      PERFORM 8060-MOVE-NAME THRU 8060-EXIT.
05711
05712      MOVE W-NAME-FIRST           TO WS-NAME-WORK2.
05713      PERFORM 8060-MOVE-NAME THRU 8060-EXIT.
05714
05715      SET NWA-INDEX UP BY +1.
05716
05717      IF W-NAME-MIDDLE NOT = SPACES
05718          IF W-NAME-MIDDLE-2 = SPACES
05719              MOVE W-NAME-MIDDLE  TO WS-NW (NWA-INDEX)
05720              SET NWA-INDEX UP BY +1
05721              MOVE '.'            TO WS-NW (NWA-INDEX)
05722              SET NWA-INDEX UP BY +2
05723          ELSE
05724              MOVE W-NAME-MIDDLE  TO WS-NAME-WORK2
05725              PERFORM 8060-MOVE-NAME THRU 8060-EXIT.
05726
05727  8050-EXIT.
05728      EXIT.
05729
05730  EJECT
05731  8060-MOVE-NAME.
05732      IF WS-NAME-SW GREATER THAN +1
05733          GO TO 8060-EXIT.
05734
05735      IF WS-NAME-WORK2 = SPACES
05736          GO TO 8060-EXIT.
05737
05738      SET NWA-INDEX2            TO +1.
05739      SET NWA-INDEX3            TO +2.
05740
05741  8060-MOVE-NAME-CYCLE.
05742      MOVE WS-NW2 (NWA-INDEX2)  TO WS-NW (NWA-INDEX).
05743
05744      IF NWA-INDEX LESS THAN +30
05745          SET NWA-INDEX UP BY +1
05746      ELSE
05747          ADD +2 TO  WS-NAME-SW
05748          GO TO 8060-EXIT.
05749
05750      IF NWA-INDEX2 LESS THAN +20
05751          SET NWA-INDEX3 UP BY +1
05752          SET NWA-INDEX2 UP BY +1.
05753
05754      IF WS-NW2 (NWA-INDEX2) = SPACES  AND
05755         WS-NW2 (NWA-INDEX3) = SPACES
05756          IF WS-NAME-SW = ZERO
05757              MOVE ','            TO WS-NW (NWA-INDEX)
05758              SET NWA-INDEX UP BY +2
05759              MOVE +1             TO WS-NAME-SW
05760              GO TO 8060-EXIT
05761          ELSE
05762              GO TO 8060-EXIT.
05763
05764      GO TO 8060-MOVE-NAME-CYCLE.
05765
05766  8060-EXIT.
05767      EXIT.
05768
05769      EJECT
05770
05771  8070-UNLOCK-TRLR.
05772      
      * EXEC CICS UNLOCK
05773 *        DATASET   (WS-ACTIVITY-TRAILERS-DSID)
05774 *    END-EXEC.
      *    MOVE '&*                    #   #00015003' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303135303033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05775
05776  8070-EXIT.
05777       EXIT.
05778
05779      EJECT
05780  8100-SEND-INITIAL-MAP SECTION.
05781      IF EMI-ERROR NOT = ZERO
05782          PERFORM 9900-ERROR-FORMAT
05783        ELSE
05784          IF TRANSACTION-SUCCESSFUL
05785              PERFORM 9900-ERROR-FORMAT.
05786
05787      MOVE EIBTIME                TO  TIME-IN.
05788
05789      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
05790      MOVE '5'                    TO  DC-OPTION-CODE.
05791      PERFORM 8500-DATE-CONVERSION.
05792
05793      IF PI-MAP-NAME = EL142A
05794          MOVE DC-GREG-DATE-1-EDIT  TO ADATEO
05795          MOVE TIME-OUT             TO ATIMEO
05796          MOVE EMI-MESSAGE-AREA (1) TO AEMSG1O
05797          MOVE EMI-MESSAGE-AREA (2) TO AEMSG2O
05798          MOVE EMI-MESSAGE-AREA (3) TO AEMSG3O
05799          GO TO 8110-SEND-MAP.
05800
05801      IF PI-MAP-NAME = EL142B
05802          MOVE DC-GREG-DATE-1-EDIT  TO BDATEO
05803          MOVE TIME-OUT             TO BTIMEO
05804          MOVE EMI-MESSAGE-AREA (1) TO BEMSG1O
05805          GO TO 8110-SEND-MAP.
05806
05807      IF PI-MAP-NAME = EL142B2
05808          MOVE DC-GREG-DATE-1-EDIT  TO KDATEO
05809          MOVE TIME-OUT             TO KTIMEO
05810          MOVE EMI-MESSAGE-AREA (1) TO KEMSG1O
05811          MOVE EMI-MESSAGE-AREA (2) TO KEMSG2O
05812          MOVE EMI-MESSAGE-AREA (3) TO KEMSG3O
05813          GO TO 8110-SEND-MAP.
05814
05815      IF PI-MAP-NAME = EL142C
05816          MOVE DC-GREG-DATE-1-EDIT  TO CDATEO
05817          MOVE TIME-OUT             TO CTIMEO
05818          MOVE EMI-MESSAGE-AREA (1) TO CEMSG1O
05819          MOVE EMI-MESSAGE-AREA (2) TO CEMSG2O
05820          GO TO 8110-SEND-MAP.
05821
05822      IF PI-MAP-NAME = EL142D
05823          IF PI-PROCESSOR-ID = 'LGXX'
05824              MOVE AL-UNNON       TO  DARCHNOA
050110                                     DRESFRMA
050110                                     DAUTOCLA
05825              MOVE AL-UANON       TO  DDTSENTA
05826                                      DINPRNTA
05827                                      DREPRNTA
05828      ELSE
05829          MOVE AL-SANOF           TO  DARCHNOA
050110                                     DRESFRMA
050110                                     DAUTOCLA
05830                                      DDTSENTA
05831                                      DINPRNTA
05832                                      DREPRNTA.
05833
050110*05834      IF PI-MAP-NAME = EL142D
050110*05835          IF PI-COMPANY-ID = 'DMD'
050110*05836              MOVE AL-SANON       TO  DMDLETPA
050110*05837                                      DMDPURPA
050110*05838                                      DMDRELPA.
05839
05840      IF PI-MAP-NAME = EL142D
05841          MOVE DC-GREG-DATE-1-EDIT  TO DDATEO
05842          MOVE TIME-OUT             TO DTIMEO
05843          MOVE EMI-MESSAGE-AREA (1) TO DEMSG1O
05844          GO TO 8110-SEND-MAP.
05845
05846      IF PI-MAP-NAME = EL142D2
05847          MOVE DC-GREG-DATE-1-EDIT  TO LDATEO
05848          MOVE TIME-OUT             TO LTIMEO
05849          MOVE EMI-MESSAGE-AREA (1) TO LEMSG1O
05850          MOVE EMI-MESSAGE-AREA (2) TO LEMSG2O
05851          MOVE EMI-MESSAGE-AREA (3) TO LEMSG3O
05852          GO TO 8110-SEND-MAP.
05853
05854      IF PI-MAP-NAME = EL142E
05855          MOVE DC-GREG-DATE-1-EDIT  TO EDATEO
05856          MOVE TIME-OUT             TO ETIMEO
05857          MOVE EMI-MESSAGE-AREA (1) TO EEMSG1O
05858          MOVE EMI-MESSAGE-AREA (2) TO EEMSG2O
05859          GO TO 8110-SEND-MAP.
05860
05861      IF PI-MAP-NAME = EL142F
05862          MOVE DC-GREG-DATE-1-EDIT  TO FDATEO
05863          MOVE TIME-OUT             TO FTIMEO
05864          MOVE EMI-MESSAGE-AREA (1) TO FEMSG1O
05865          MOVE EMI-MESSAGE-AREA (2) TO FEMSG2O
05866          GO TO 8110-SEND-MAP.
05867
05868      IF PI-MAP-NAME = EL142G
05869          MOVE DC-GREG-DATE-1-EDIT  TO GDATEO
05870          MOVE TIME-OUT             TO GTIMEO
05871          MOVE EMI-MESSAGE-AREA (1) TO GEMSG1O
05872          MOVE EMI-MESSAGE-AREA (2) TO GEMSG2O
042110         MOVE AL-UANON             TO GRSNCDA
05873          GO TO 8110-SEND-MAP.
05874
05875      IF PI-MAP-NAME = EL142H
05876          MOVE DC-GREG-DATE-1-EDIT  TO HDATEO
05877          MOVE TIME-OUT             TO HTIMEO
05878          MOVE EMI-MESSAGE-AREA (1) TO HEMSG1O
05879          GO TO 8110-SEND-MAP.
05880
05881      IF PI-MAP-NAME = EL142I
05882          MOVE DC-GREG-DATE-1-EDIT  TO IDATEO
05883          MOVE TIME-OUT             TO ITIMEO
05884          MOVE EMI-MESSAGE-AREA (1) TO IEMSG1O
05885          GO TO 8110-SEND-MAP.
05886
05887      IF PI-MAP-NAME = EL142J
05888          MOVE DC-GREG-DATE-1-EDIT  TO JDATEO
05889          MOVE TIME-OUT             TO JTIMEO
05890          MOVE EMI-MESSAGE-AREA (1) TO JEMSG1O
05891          GO TO 8110-SEND-MAP.
05892
05893  8110-SEND-MAP.
05894      IF PI-USES-PAID-TO
05895         IF PI-MAP-NAME = EL142B
05896            MOVE 'PAID  TO     -'    TO BTHRUHDO
05897            ELSE
05898            IF PI-MAP-NAME = EL142I
05899               MOVE 'PAID  TO  DATE' TO ITHRUHDO.
05900
05901      IF PI-COMPANY-ID = 'DMD'
05902          PERFORM  8400-DMD-NOTES-ONLY.
05903
05904      
      * EXEC CICS SEND
05905 *        FROM   (EL142DI)
05906 *        MAPSET (WS-MAPSET-NAME)
05907 *        MAP    (PI-MAP-NAME)
05908 *        CURSOR
05909 *        ERASE
05910 *    END-EXEC.
           MOVE LENGTH OF
            EL142DI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00015140' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303135313430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 EL142DI, 
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
           
05911
05912      PERFORM 9100-RETURN-TRAN.
05913
05914  8100-EXIT.
05915      EXIT.
05916
05917      EJECT
05918  8200-SEND-DATAONLY SECTION.
05919      IF EMI-ERROR NOT = ZERO
05920          PERFORM 9900-ERROR-FORMAT
05921        ELSE
05922          IF TRANSACTION-SUCCESSFUL
05923              PERFORM 9900-ERROR-FORMAT.
05924
05925      MOVE EIBTIME                TO  TIME-IN.
05926
05927      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
05928      MOVE '5'                    TO  DC-OPTION-CODE.
05929      PERFORM 8500-DATE-CONVERSION.
05930
05931      IF PI-MAP-NAME = EL142A
05932          MOVE DC-GREG-DATE-1-EDIT  TO ADATEO
05933          MOVE TIME-OUT             TO ATIMEO
05934          MOVE EMI-MESSAGE-AREA (1) TO AEMSG1O
05935          MOVE EMI-MESSAGE-AREA (2) TO AEMSG2O
05936          MOVE EMI-MESSAGE-AREA (3) TO AEMSG3O
05937          GO TO 8210-SEND-MAP.
05938
05939      IF PI-MAP-NAME = EL142B
05940          MOVE DC-GREG-DATE-1-EDIT  TO BDATEO
05941          MOVE TIME-OUT             TO BTIMEO
05942          MOVE EMI-MESSAGE-AREA (1) TO BEMSG1O
05943          GO TO 8210-SEND-MAP.
05944
05945      IF PI-MAP-NAME = EL142B2
05946          MOVE DC-GREG-DATE-1-EDIT  TO KDATEO
05947          MOVE TIME-OUT             TO KTIMEO
05948          MOVE EMI-MESSAGE-AREA (1) TO KEMSG1O
05949          MOVE EMI-MESSAGE-AREA (2) TO KEMSG2O
05950          MOVE EMI-MESSAGE-AREA (3) TO KEMSG3O
05951          GO TO 8210-SEND-MAP.
05952
05953      IF PI-MAP-NAME = EL142C
05954          MOVE DC-GREG-DATE-1-EDIT  TO CDATEO
05955          MOVE TIME-OUT             TO CTIMEO
05956          MOVE EMI-MESSAGE-AREA (1) TO CEMSG1O
05957          MOVE EMI-MESSAGE-AREA (2) TO CEMSG2O
05958          GO TO 8210-SEND-MAP.
05959
05960      IF PI-MAP-NAME = EL142D
05961          MOVE DC-GREG-DATE-1-EDIT  TO DDATEO
05962          MOVE TIME-OUT             TO DTIMEO
05963          MOVE EMI-MESSAGE-AREA (1) TO DEMSG1O
05964          GO TO 8210-SEND-MAP.
05965
05966      IF PI-MAP-NAME = EL142D2
05967          MOVE DC-GREG-DATE-1-EDIT  TO LDATEO
05968          MOVE TIME-OUT             TO LTIMEO
05969          MOVE EMI-MESSAGE-AREA (1) TO LEMSG1O
05970          MOVE EMI-MESSAGE-AREA (2) TO LEMSG2O
05971          MOVE EMI-MESSAGE-AREA (3) TO LEMSG3O
05972          GO TO 8210-SEND-MAP.
05973
05974      IF PI-MAP-NAME = EL142E
05975          MOVE DC-GREG-DATE-1-EDIT  TO EDATEO
05976          MOVE TIME-OUT             TO ETIMEO
05977          MOVE EMI-MESSAGE-AREA (1) TO EEMSG1O
05978          MOVE EMI-MESSAGE-AREA (2) TO EEMSG2O
05979          GO TO 8210-SEND-MAP.
05980
05981      IF PI-MAP-NAME = EL142F
05982          MOVE DC-GREG-DATE-1-EDIT  TO FDATEO
05983          MOVE TIME-OUT             TO FTIMEO
05984          MOVE EMI-MESSAGE-AREA (1) TO FEMSG1O
05985          MOVE EMI-MESSAGE-AREA (2) TO FEMSG2O
05986          GO TO 8210-SEND-MAP.
05987
05988      IF PI-MAP-NAME = EL142G
05989          MOVE DC-GREG-DATE-1-EDIT  TO GDATEO
05990          MOVE TIME-OUT             TO GTIMEO
05991          MOVE EMI-MESSAGE-AREA (1) TO GEMSG1O
05992          MOVE EMI-MESSAGE-AREA (2) TO GEMSG2O
05993          GO TO 8210-SEND-MAP.
05994
05995      IF PI-MAP-NAME = EL142H
05996          MOVE DC-GREG-DATE-1-EDIT  TO HDATEO
05997          MOVE TIME-OUT             TO HTIMEO
05998          MOVE EMI-MESSAGE-AREA (1) TO HEMSG1O
05999          GO TO 8210-SEND-MAP.
06000
06001      IF PI-MAP-NAME = EL142I
06002          MOVE DC-GREG-DATE-1-EDIT  TO IDATEO
06003          MOVE TIME-OUT             TO ITIMEO
06004          MOVE EMI-MESSAGE-AREA (1) TO IEMSG1O
06005          GO TO 8210-SEND-MAP.
06006
06007      IF PI-MAP-NAME = EL142J
06008          
      * EXEC CICS ASKTIME ABSTIME(LCP-CICS-DATE)
06009 *        END-EXEC
      *    MOVE '0"A                   "   #00015244' TO DFHEIV0
           MOVE X'302241202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303135323434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-DATE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
06010          
      * EXEC CICS FORMATTIME
06011 *                  ABSTIME(LCP-CICS-DATE)
06012 *                  YYMMDD(LCP-CURRENT-DATE-68)
06013 *                  DATESEP('/')
06014 *        END-EXEC
           MOVE '/' TO DFHEIV9
      *    MOVE 'j$((   "              $   #00015246' TO DFHEIV0
           MOVE X'6A2428282020202220202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303135323436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-DATE, 
                 LCP-CURRENT-DATE-68, 
                 DFHEIV9
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
06015          MOVE LCP-CURRENT-DATE-68  TO JDATEO
06016          MOVE TIME-OUT             TO JTIMEO
06017          MOVE EMI-MESSAGE-AREA (1) TO JEMSG1O
06018          GO TO 8210-SEND-MAP.
06019
06020  8210-SEND-MAP.
06021      IF PI-USES-PAID-TO
06022         IF PI-MAP-NAME = EL142B
06023            MOVE 'PAID  TO     -'    TO BTHRUHDO
06024           ELSE
06025            IF PI-MAP-NAME = EL142I
06026               MOVE 'PAID  TO  DATE' TO ITHRUHDO.
06027
06028      IF PI-COMPANY-ID = 'DMD'
06029          PERFORM  8400-DMD-NOTES-ONLY.
06030
06031      
      * EXEC CICS SEND DATAONLY
06032 *        FROM   (EL142DI)
06033 *        MAPSET (WS-MAPSET-NAME)
06034 *        MAP    (PI-MAP-NAME)
06035 *        CURSOR
06036 *    END-EXEC.
           MOVE LENGTH OF
            EL142DI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00015267' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303135323637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 EL142DI, 
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
           
06037
06038      PERFORM 9100-RETURN-TRAN.
06039
06040  8290-EXIT.
06041      EXIT.
06042
06043      EJECT
06044  8300-SEND-TEXT SECTION.
06045
06046      
      * EXEC CICS SEND TEXT
06047 *        FROM   (LOGOFF-TEXT)
06048 *        LENGTH (LOGOFF-LENGTH)
06049 *        ERASE  FREEKB
06050 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00015282' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303135323832' TO DFHEIV0(25:11)
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
           
06051
06052      
      * EXEC CICS RETURN
06053 *    END-EXEC.
      *    MOVE '.(                    ''   #00015288' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303135323838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06054
06055  8300-EXIT.
06056      EXIT.
06057
06058  8400-DMD-NOTES-ONLY SECTION.
06059
06060      IF NOT SYSTEM-MODIFY-CAP
06061         IF PI-MAP-NAME = EL142A
06062            IF WS-PI-EL142-PRIORITY = '9'
06063              MOVE AL-SANON   TO AREMINDA  ALETTERA  APAYMNTA
06064                                 AAUTOPAA  ARESEXPA  ADENIALA
06065                                 AIDCA     AFORMSA
06066              MOVE -1         TO ANOTESL.
06067
06068  8400-EXIT.
06069      EXIT.
06070
06071      EJECT
06072  8500-DATE-CONVERSION SECTION.
06073      
      * EXEC CICS LINK
06074 *        PROGRAM  (ELDATCV)
06075 *        COMMAREA (DATE-CONVERSION-DATA)
06076 *        LENGTH   (DC-COMM-LENGTH)
06077 *    END-EXEC.
      *    MOVE '."C                   (   #00015309' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303135333039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELDATCV, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06078
06079
06080  8500-EXIT.
06081      EXIT.
06082
06083  8600-DEEDIT SECTION.
06084      
      * EXEC CICS BIF DEEDIT
06085 *        FIELD  (WS-DEEDIT-FIELD)
06086 *        LENGTH (WS-DEEDIT-LENGTH)
06087 *    END-EXEC.
      *    MOVE '@"L                   #   #00015320' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303135333230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-DEEDIT-FIELD, 
                 WS-DEEDIT-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06088
06089  8600-EXIT.
06090      EXIT.
06091
06092      EJECT
06093  9000-RETURN-CICS SECTION.
06094      MOVE EL005                  TO  XCTL-PGM.
06095      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
06096      PERFORM 9300-XCTL.
06097
06098  9000-EXIT.
06099      EXIT.
06100
06101  9100-RETURN-TRAN SECTION.
06102      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
06103      MOVE PI-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.
06104
06105      
      * EXEC CICS RETURN
06106 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
06107 *        LENGTH   (PI-COMM-LENGTH)
06108 *        TRANSID  (WS-TRANS-ID)
06109 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00015341' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303135333431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06110
06111  9100-EXIT.
06112      EXIT.
06113
06114  9300-XCTL SECTION.
06115      MOVE DFHENTER               TO  EIBAID.
06116
06117      
      * EXEC CICS XCTL
06118 *        PROGRAM  (XCTL-PGM)
06119 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
06120 *        LENGTH   (PI-COMM-LENGTH)
06121 *    END-EXEC.
      *    MOVE '.$C                   %   #00015353' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303135333533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 XCTL-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06122
06123  9300-EXIT.
06124      EXIT.
06125
06126      EJECT
06127  9400-CLEAR SECTION.
041613     IF PI-MAP-NAME = EL142D2
041613         GO TO 1200-MAIN-LOGIC
041613     END-IF
041613
06128      MOVE PI-RETURN-TO-PROGRAM  TO  XCTL-PGM.
06129      PERFORM 9300-XCTL.
06130
06131  9400-EXIT.
06132      EXIT.
06133
06134  9600-PGMIDERR SECTION.
06135      
      * EXEC CICS HANDLE CONDITION
06136 *        PGMIDERR (8300-SEND-TEXT)
06137 *    END-EXEC.
      *    MOVE '"$L                   ! / #00015375' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2F20233030303135333735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06138
06139      MOVE THIS-PGM               TO  PI-CALLING-PROGRAM.
06140
06141      MOVE EL005                  TO  XCTL-PGM
06142                                      LOGOFF-PGM.
06143      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
06144      MOVE SPACES                 TO  PI-ENTRY-CD-1.
06145      PERFORM 9300-XCTL.
06146
06147  9600-EXIT.
06148      EXIT.
06149
06150      EJECT
06151  9900-ERROR-FORMAT SECTION.
06152      ADD +1  TO  WS-ERROR-COUNT.
06153
06154      IF EMI-ERRORS-COMPLETE
06155          MOVE ER-ZERO            TO  EMI-ERROR
06156          GO TO 9900-EXIT.
06157
06158      
      * EXEC CICS LINK
06159 *        PROGRAM  (EL001)
06160 *        COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)
06161 *        LENGTH   (EMI-COMM-LENGTH)
06162 *    END-EXEC.
      *    MOVE '."C                   (   #00015398' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303135333938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EL001, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06163
06164      MOVE ER-ZERO                TO  EMI-ERROR.
06165
06166  9900-EXIT.
06167      EXIT.
06168
06169      EJECT
06170  9990-ERROR SECTION.
06171      MOVE DFHEIBLK               TO EMI-LINE1.
06172      
      * EXEC CICS LINK
06173 *        PROGRAM  (EL004)
06174 *        COMMAREA (EMI-LINE1)
06175 *        LENGTH   (72)
06176 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00015412' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303135343132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EL004, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06177
06178      PERFORM 8200-SEND-DATAONLY.
06179      GO TO 9100-RETURN-TRAN.
06180
06181  9990-EXIT.
06182      EXIT.
06183
06184  9995-SECURITY-VIOLATION.
06185 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00015442' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303135343432' TO DFHEIV0(25:11)
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
06186
06187  9995-EXIT.
06188       EXIT.
06189
06190  9999-LAST-PARAGRAPH SECTION.
06191
06192      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL142' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL142' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9600-PGMIDERR,
                     0130-MAIN-LOGIC,
                     6000-END-OF-FILE,
                     9990-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 0200-ADD-PMNT-NOTE-TRLR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 3200-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 3490-DELETE-FORM-ARCHIVE,
                     3499-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 3699-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 4200-NOTE-TRLR-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 4410-ADDRESS-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 4440-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 4490-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 4910-ADDRESS-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 5020-DISPLAY-CHECK-QUEUE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 6020-ELNAPS-NOTFND,
                     6020-ELNAPS-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 8000-NOTE-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL142' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
