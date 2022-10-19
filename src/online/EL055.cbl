       identification division.
       program-id. EL055.
052020******************************************************************
052020*                   C H A N G E   L O G
052020*
052020* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
052020*-----------------------------------------------------------------
052020*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
052020* EFFECTIVE    NUMBER
052020*-----------------------------------------------------------------
052020* 052020  CR2019111300001  PEMA  New program for 'K' transaction
072320* 072320  IR2020072100002  PEMA  Allow deletion of dup pend issue
072920* 072920  IR2020072900001  PEMA  Skip err if cert notfnd
052020******************************************************************
       environment division.
       data division.
       working-storage section.
       01  DFH-START PIC X(04).
       77  s1                          pic s999 comp-3 value +0.
       77  ns1                         pic s9(5) comp-3 value +0.
       77  nscntr                      pic s999 comp-3 value +0.
       77  ws-eof-sw                   pic x  value spaces.
           88  end-of-input                  value 'Y'.
       77  ws-error-sw                 pic x  value spaces.
           88  error-found               value 'Y'.
       77  ws-string-len               pic s999 comp-3 value zeros.
       77  ws-startbr-ind              pic x value spaces.
       77  ws-NS-startbr-ind           pic x value spaces.
072320 77  ws-bypass-ind               pic x value spaces.
072320     88  bypass-copy-files         value 'Y'.
       01  P pointer.
       01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
       01  var-ptr pointer.
       01  env-var-len                 pic 9(4)  binary.
       01  rc                          pic 9(9)  binary.
       01  ws-new-greg-eff-date        pic x(10).
       01  ws-old-greg-eff-date        pic x(10).
       01  WS-KIXSYS.
           05  WS-KIX-FIL1             PIC X(10).
           05  WS-KIX-APPS             PIC X(10).
           05  WS-KIX-ENV              PIC X(10).
           05  WS-KIX-MYENV            PIC X(10).
           05  WS-KIX-SYS              PIC X(10).
      *                                copy ERCPNDB.
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
      *                                copy ELCCERT.
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
      *                                copy ELCCRTO.
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
      *                                copy ELCCRTT.
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
      *                                copy ERCARCH.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ERCARCH.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = LETTERS SENT TO ARCHIVE FILE              *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 250  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ERARCH                        RKP=2,LEN=5     *
00013 *     ALTERNATE PATH1 = ERARCH2 (CERT/RESP)      RKP=07,LEN=35   *
00014 *     ALTERNATE PATH2 = ERARCH3 (FORM NUMBER)    RKP=44,LEN=28   *
00015 *     ALTERNATE PATH3 = ERARCH4 (PROCCESSOR ID)  RKP=73,LEN=28   *
00016 *     ALTERNATE PATH4 = ERARCH5 (ACCOUNT KEY)    RKP=100,LEN=24  *
00017 *     ALTERNATE PATH5 = ERARCH6 (BTCH/CHK KEY)   RKP=124,LEN=11  *
00018 *                                                                *
00019 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00020 ******************************************************************
031011*                   C H A N G E   L O G
031011*
031011* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
031011*-----------------------------------------------------------------
031011*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
031011* EFFECTIVE    NUMBER
031011*-----------------------------------------------------------------
031011* 031011  CR2007070900001  PEMA  ADD FOLLOW-UP LETTER PROCESSING
070711* 070711  CR2011022800001  AJRA  NAPERSOFT CHANGES
110612* 110612    2012101700002  AJRA  ADD NEW FIELDS
102918* 102918  CR2018080300002  PEMA  ADD ONBASE STUFF
031011******************************************************************
00021  01  LETTER-ARCHIVE.
00022      12  LA-RECORD-ID                PIC  X(02).
00023          88  LA-VALID-ID                VALUE 'LA'.
00024
00025      12  LA-CONTROL-PRIMARY.
00026          16  LA-COMPANY-CD           PIC  X(01).
00027          16  LA-ARCHIVE-NO           PIC S9(08)    COMP.
00028
00029      12  LA-CONTROL-BY-CERT-RESP.
00030          16  LA-COMPANY-CD-A2        PIC  X(01).
00031          16  LA-CERT-NO-A2.
00032              20  LA-CERT-PRIME-A2    PIC  X(10).
00033              20  LA-CERT-SUFFIX-A2   PIC  X(01).
00034          16  LA-RSP-PERSON-A2 REDEFINES LA-CERT-NO-A2.
00035              20  LA-RESP-PERSON-A2   PIC  X(10).
00036              20  LA-TYPE-A2          PIC  X(01).
00037          16  LA-CARRIER-A2           PIC  X(01).
00038          16  LA-GROUPING-A2          PIC  X(06).
00039          16  LA-STATE-A2             PIC  X(02).
00040          16  LA-ACCOUNT-A2           PIC  X(10).
00041          16  LA-EFFECT-DATE-A2       PIC  X(02).
00042          16  LA-ARCHIVE-NO-A2        PIC S9(08)    COMP.
00043
00044      12  LA-CONTROL-BY-FORM.
00045          16  LA-COMPANY-CD-A3        PIC  X(01).
00046          16  LA-FORM-A3              PIC  X(04).
00047          16  LA-CARRIER-A3           PIC  X(01).
00048          16  LA-GROUPING-A3          PIC  X(06).
00049          16  LA-STATE-A3             PIC  X(02).
00050          16  LA-ACCOUNT-A3           PIC  X(10).
00051          16  LA-ARCHIVE-NO-A3        PIC S9(08)    COMP.
00052
00053      12  LA-CONTROL-BY-PROCESSOR.
00054          16  LA-COMPANY-CD-A4        PIC  X(01).
00055          16  LA-PROCESSOR-CD         PIC  X(04).
00056          16  LA-CARRIER-A4           PIC  X(01).
00057          16  LA-GROUPING-A4          PIC  X(06).
00058          16  LA-STATE-A4             PIC  X(02).
00059          16  LA-ACCOUNT-A4           PIC  X(10).
00060          16  LA-ARCHIVE-NO-A4        PIC S9(08)    COMP.
00061
00062      12  LA-CONTROL-BY-KEY-FIELDS.
00063          16  LA-COMPANY-CD-A5        PIC  X(01).
00064          16  LA-CARRIER-A5           PIC  X(01).
00065          16  LA-GROUPING-A5          PIC  X(06).
00066          16  LA-STATE-A5             PIC  X(02).
00067          16  LA-ACCOUNT-A5           PIC  X(10).
00068          16  LA-ARCHIVE-NO-A5        PIC S9(08)    COMP.
00069
00070      12  LA-CONTROL-BY-GROUP-CODE.
00071          16  LA-COMPANY-CD-A6        PIC  X(01).
00072          16  LA-ENTRY-A6.
00073              20  LA-FILLER           PIC  X(02).
00074              20  LA-QUE-CONTROL-A6   PIC S9(08)    COMP.
00075          16  LA-ARCHIVE-NO-A6        PIC S9(08)    COMP.
00076
00077      12  FILLER                      PIC  X(09).
00078
00079      12  LA-HEADER-RECORD.
00080          16  LA-NUMBER-LABEL-LINES   PIC S9(04)    COMP.
00081          16  LA-CREATION-DATE        PIC  X(02).
00082          16  LA-FOLLOW-UP-DATE       PIC  X(02).
070711         16  LA-FINAL-ACT-DATE       REDEFINES
070711               LA-FOLLOW-UP-DATE     PIC  X(02).
00083          16  LA-INITIAL-PRINT-DATE   PIC  X(02).
00084          16  LA-NO-OF-COPIES         PIC S9(01).
00085          16  LA-NO-OF-TEXT-RECORDS   PIC S9(04)    COMP.
00086          16  LA-REPLY-DATE           PIC  X(02).
00087          16  LA-RESEND-DATES.
00090              20  LA-RESEND-DATE      PIC  X(02).
00091              20  LA-SENT-DATE        PIC  X(02).
                   20  FILLER              PIC  X(08).
00099          16  LA-SOURCE-INFORMATION.
00100              20  LA-DATA-SOURCE      PIC  X(01).
00101              20  LA-ADDR-SOURCE      PIC  X(01).
00102          16  LA-STATUS               PIC  X(01).
00103              88  LA-STATUS-ACTIVE         VALUE 'A'.
00104              88  LA-STATUS-COMPLETED      VALUE 'C'.
00105              88  LA-STATUS-ON-HOLD        VALUE 'H'.
00106              88  LA-STATUS-TO-BE-PURGED   VALUE 'X'.
00107              88  LA-STATUS-PURGED         VALUE 'P'.
00108              88  LA-STATUS-VOIDED         VALUE 'V'.
00109          16  LA-LAST-RESENT-PRINT-DATE
00110                                      PIC  X(02).
00111          16  LA-PRINT-RESTRICTION    PIC  X(01).
00112              88  LA-PRINT-ONLY-WHEN-CNTL-GIVEN
00113                                           VALUE 'C'.
00114              88  LA-PRINT-ONLY-WHEN-FORM-GIVEN
00115                                           VALUE 'F'.
00116              88  LA-PRINT-ONLY-WHEN-PROC-GIVEN
00117                                           VALUE 'P'.
00118          16  LA-PURGED-DATE          PIC  X(02).
00119          16  LA-VOIDED-DATE          PIC  X(02).
101705         16  LA-RESEND-LETR          PIC  X(4).
102918         16  LA-VOID-ONBASE-YN       PIC  X.
102918         16  LA-ONBASE-UNIQUE-ID     PIC S9(5) COMP-3.
102918         16  FILLER                  PIC  X(04).
101705*        16  LA-RESEND-LETR-2        PIC  X(4).
101705*        16  LA-RESEND-LETR-3        PIC  X(4).
070711*        16  FILLER                  PIC  X(59).
               16  LA-ARCHIVE-STATUS       PIC  X.
                   88  LA-TEMP                VALUE 'T'.
                   88  LA-QWS                 VALUE 'Q'.
                   88  LA-BATCH               VALUE 'B'.
070711         16  LA-FINAL-ACT-IND        PIC  X(1).
070711         16  LA-VA-DISCLOSURE-IND    PIC  X(1).
110612         16  LA-ENDT-ARCH-NO         PIC S9(8) COMP.
110612         16  LA-ENDT-ARCH-NO-X REDEFINES LA-ENDT-ARCH-NO
110612                                     PIC X(4).
110612         16  FILLER                  PIC  X(42).
00120 *        16  FILLER                  PIC  X(71).
070711         16  LA-LAST-MAINT-DATE      PIC  X(2).
070711         16  LA-LAST-MAINT-TIME      PIC S9(6) COMP-3.
070711         16  LA-LAST-MAINT-TIMEX  REDEFINES LA-LAST-MAINT-TIME
070711                                     PIC  X(4).
070711         16  LA-LAST-UPDATED-BY      PIC  X(4).
00121
00122 ******************************************************************
      *                                copy NSCASEXTR.
      ******************************************************************
      *                                                                *
      *                                                                *
      *                            NSCASEXTR.                          *
      *                            VMOD=2.001                          *
      *                                                                *
      *   FILE DESCRIPTION = NAPERSOFT ADMIN SERVICES EXTRACT FILE     *
      *                                                                *
      *   FILE TYPE = VSAM,KSDS                                        *
      *   RECORD SIZE = 4500 RECFORM = FIXED                           *
      *                                                                *
      *   BASE CLUSTER = NSASEXTR                       RKP=0,LEN=07   *
      *                                                                *
      *   LOG = NO                                                     *
      *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
      ******************************************************************
      *-----------------------------------------------------------------
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 072211    2011022800001  PEMA  NEW FILE
      *-----------------------------------------------------------------
       01 NSAS-EXTRACT-RECORD.
          05  NSAS-CONTROL-PRIMARY.
              10  NSAS-COMPANY-CD      PIC X.
              10  NSAS-ARCHIVE-NO      PIC 9(8) BINARY.
              10  NSAS-SEQ-NO          PIC 9(4) BINARY.
          05  NSAS-LETTER-VARIABLES    PIC X(4350).
          05  FILLER                   PIC X(143).
      *                                copy ERCCHEK.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCHEK                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.008                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CHECK RECORDS                             *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 600    RECFORM = FIXED                         *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ERCHEK             RKP=2,LEN=35          *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
021414*                   C H A N G E   L O G
021414*
021414* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
021414*-----------------------------------------------------------------
021414*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
021414* EFFECTIVE    NUMBER
021414*-----------------------------------------------------------------
021414* 021414    2003053000001  PEMA  changes for auto chk request
021414******************************************************************
00018  01  CHECK-RECORDS.
00019      12  CH-RECORD-ID                      PIC XX.
00020          88  VALID-CH-ID                      VALUE 'CH'.
00021
00022      12  CH-CONTROL-PRIMARY.
00023          16  CH-COMPANY-CD                 PIC X.
00024          16  CH-CARRIER                    PIC X.
00025          16  CH-GROUPING                   PIC X(6).
00026          16  CH-STATE                      PIC XX.
00027          16  CH-ACCOUNT                    PIC X(10).
00028          16  CH-CERT-EFF-DT                PIC XX.
00029          16  CH-CERT-NO.
00030              20  CH-CERT-PRIME             PIC X(10).
00031              20  CH-CERT-SFX               PIC X.
00032          16  CH-SEQUENCE-NO                PIC S9(4)     COMP.
00033
00034      12  CH-RECORDED-DT                    PIC XX.
00035      12  CH-RECORDED-BY                    PIC X(4).
00036      12  CH-LAST-MAINT-HHMMSS              PIC S9(6)     COMP-3.
00037
00038      12  CH-AMOUNT-PAID                    PIC S9(7)V99  COMP-3.
00039      12  CH-CHECK-NO                       PIC X(7).
00040      12  CH-REASON-FOR-CHECK               PIC X(25).
00041      12  CH-CHECK-WRITTEN-DT               PIC XX.
00042      12  FILLER                            PIC X.
00044
00045      12  CH-PAYEE-INFO.
00046          16  CH-PAYEE-NAME-1               PIC X(30).
00047          16  CH-PAYEE-NAME-2               PIC X(30).
00048          16  CH-PAYEE-ADDRESS-1            PIC X(30).
00049          16  CH-PAYEE-ADDRESS-2            PIC X(30).
00050          16  CH-PAYEE-CITY-ST.
021414             20  CH-PAYEE-CITY             PIC X(28).
021414             20  CH-PAYEE-STATE            PIC XX.
00051          16  CH-PAYEE-ZIP-CODE.
00052              20  CH-PAYEE-ZIP.
00053                  24  CH-ZIP-PRI-1ST        PIC X.
00054                      88  CH-CANADIAN-POST-CODE
00055                                            VALUES 'A' THRU 'Z'.
00056                  24  FILLER                PIC X(4).
00057              20  CH-PAYEE-ZIP-EXT          PIC X(4).
00058          16  CH-CANADIAN-POSTAL-CODE REDEFINES CH-PAYEE-ZIP-CODE.
00059              20  CH-CAN-POSTAL-1           PIC XXX.
00060              20  CH-CAN-POSTAL-2           PIC XXX.
00061              20  FILLER                    PIC XXX.
00062
00063      12  CH-CHECK-STUB-TEXT.
00064          16  CH-STUB-LINE-1                PIC X(30).
00065          16  CH-TEXT-LINE-1                PIC X(50).
00066          16  CH-TEXT-LINE-2                PIC X(50).
00067          16  CH-TEXT-LINE-3                PIC X(40).
021414     12  CH-RETURN-TO                      PIC X(30).
00070
00071      12  CH-COMPENSATION-CONTROL.
00072          16  CH-COMP-CARRIER               PIC X.
00073          16  CH-COMP-GROUPING              PIC X(6).
00074          16  CH-COMP-FIN-RESP              PIC X(10).
00075          16  CH-COMP-ACCOUNT               PIC X(10).
00076
00077      12  CH-CREDIT-SELECT-DT               PIC XX.
00078      12  CH-CREDIT-ACCEPT-DT               PIC XX.
00079      12  CH-PAYEE-CODE                     PIC X(6).
00080
00081      12  CH-VOID-DATA.
00082          20  CH-VOID-DT                    PIC XX.
00083          20  CH-VOID-BY                    PIC X(4).
00084          20  CH-VOID-REASON                PIC X(25).
00085
021414     12  CH-APPROVAL-DATA.
021414         20  CH-APPROVAL-DT                PIC XX.
021414         20  CH-APPROVAL-STATUS            PIC X.
021414             88  CH-IN-LIMBO                  VALUE ' '.
021414             88  CH-APPROV-PENDING            VALUE 'P' '2'.
021414             88  CH-APPROVED                  VALUE 'A'.
021414             88  CH-DENIED                    VALUE 'D'.
021414         20  CH-APPROVED-BY                PIC XXXX.
00086      12  CH-CHECK-QUE-CONTROL              PIC S9(8)     COMP.
00087              88  PAYMENT-NOT-QUEUED           VALUE ZERO.
00088      12  CH-CHECK-QUE-SEQUENCE             PIC S9(4)     COMP.
00089
021414     12  ch-released-dt                    pic xx.
021414     12  ch-check-cashed-dt                pic xx.
           12  FILLER                            PIC X.
00090 *    12  CH-CHECK-REFERENCE                PIC X(12).
00091      12  CH-CHECK-ORIGIN-SW                PIC X.
00092              88  CH-REFUND-CHECK              VALUE 'R'.
00093              88  CH-MAINT-CHECK               VALUE 'M'.
00094
00095      12  CH-CANC-DT                        PIC XX.
00096      12  CH-LF-REFUND                      PIC S9(7)V99  COMP-3.
00097      12  CH-AH-REFUND                      PIC S9(7)V99  COMP-3.
00098
00099      12  CH-INSURED-NAME                   PIC X(28).
00100
021414     12  ch-released-by                    pic x(4).
021414     12  ch-csr                            pic x(4).
021414     12  ch-deduct-commission              pic x.
021414         88  ch-deduct-comm                  value 'Y'.
021414         88  ch-do-not-deduct-comm           value 'N'.
00103
           12  FILLER                            PIC X(11).
021414*    12  CH-LETTER-TABLE.
021414*        16  CH-LETTERS OCCURS 3 TIMES
021414*                       INDEXED BY CH-LT-NDX
021414*                                          PIC X(04).
00108
00109      12  FILLER                            PIC X(07).
00110
      *                                copy ERCCNOT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ERCCNOT                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *        FILE DESCRIPTION = CERTIFICATE NOTES                    *
00008 *                                                                *
00009 *        FILE TYPE= VSAM,KSDS                                    *
00010 *        RECORD SIZE = 150    RECFORM = FIXED                    *
00011 *                                                                *
00012 *        BASE CLUSTER = ERCNOT        RKP=2,LEN=36               *
00013 *                                                                *
00014 *        LOG = YES                                               *
00015 *        SERVREQ = DELETE,UPDATE,NEWREC                          *
00016 *                                                                *
091509******************************************************************
091509*                   C H A N G E   L O G
091509*
091509* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
091509*-----------------------------------------------------------------
091509*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
091509* EFFECTIVE    NUMBER
091509*-----------------------------------------------------------------
091509* 091509  CR2008100900003  AJRA  NEW FILE FOR CERT NOTES.
00017 ******************************************************************
00018
00019  01  CERT-NOTE-FILE.
00020      12  CZ-RECORD-ID                PIC  XX.
00021          88  VALID-CZ-ID                  VALUE 'CZ'.
00022
00023      12  CZ-CONTROL-PRIMARY.
00024          16  CZ-COMPANY-CD           PIC X.
00025          16  CZ-CARRIER              PIC X.
00026          16  CZ-GROUPING.
00027              20 CZ-GROUPING-PREFIX   PIC XXX.
00028              20 CZ-GROUPING-PRIME    PIC XXX.
00029          16  CZ-STATE                PIC XX.
00030          16  CZ-ACCOUNT.
00031              20 CZ-ACCOUNT-PREFIX    PIC X(4).
00032              20 CZ-ACCOUNT-PRIME     PIC X(6).
00033          16  CZ-CERT-EFF-DT          PIC XX.
00034          16  CZ-CERT-NO.
00035              20  CZ-CERT-PRIME       PIC X(10).
00036              20  CZ-CERT-SFX         PIC X.
00037          16  CZ-RECORD-TYPE          PIC X.
00038              88  CERT-NOTE           VALUE '1'.
                   88  CLAIM-CERT-NOTE     VALUE '2'.
00039          16  CZ-NOTE-SEQUENCE        PIC S9(4)     COMP.
00040
00041      12  CZ-LAST-MAINT-DT            PIC XX.
00042      12  CZ-LAST-MAINT-HHMMSS        PIC S9(7)   COMP-3.
00043      12  CZ-LAST-MAINT-USER          PIC X(4).
00044
00045      12  CZ-NOTE-INFORMATION.
00046          16  CZ-NOTE                 PIC X(63).
00047          16  FILLER                  PIC X(39).
00048 ******************************************************************
      *                                copy ERCNOTE.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCNOTE                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *        FILE DESCRIPTION = CERTIFICATE AND BILLING NOTES        *
00008 *                                                                *
00009 *        FILE TYPE= VSAM,KSDS                                    *
00010 *        RECORD SIZE = 825    RECFORM = FIXED                    *
00011 *                                                                *
00012 *        BASE CLUSTER = ERNOTE        RKP=2,LEN=33               *
00013 *                                                                *
00014 *        LOG = YES                                               *
00015 *        SERVREQ = DELETE,UPDATE,NEWREC                          *
00016 *                                                                *
091509******************************************************************
091509*                   C H A N G E   L O G
091509*
091509* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
091509*-----------------------------------------------------------------
091509*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
091509* EFFECTIVE    NUMBER
091509*-----------------------------------------------------------------
091509* 091509  CR2008100900003  AJRA  CERT NOTES MOVED TO NEW FILE. THI
091509*                                FILE WILL CONTAIN BILLING NOTES O
00017 ******************************************************************
00018
00019  01  CERTIFICATE-NOTE.
00020      12  CN-RECORD-ID                PIC  XX.
00021          88  VALID-CN-ID                  VALUE 'CN'.
00022
00023      12  CN-CONTROL-PRIMARY.
00024          16  CN-COMPANY-CD           PIC X.
00025          16  CN-CARRIER              PIC X.
00026          16  CN-GROUPING.
00027              20 CN-GROUPING-PREFIX   PIC XXX.
00028              20 CN-GROUPING-PRIME    PIC XXX.
00029          16  CN-STATE                PIC XX.
00030          16  CN-ACCOUNT.
00031              20 CN-ACCOUNT-PREFIX    PIC X(4).
00032              20 CN-ACCOUNT-PRIME     PIC X(6).
00033          16  CN-CERT-EFF-DT          PIC XX.
00034          16  CN-CERT-NO.
00035              20  CN-CERT-PRIME       PIC X(10).
00036              20  CN-CERT-SFX         PIC X.
00037
00038      12  CN-BILLING-START-LINE-NO    PIC 99.
00039      12  CN-BILLING-END-LINE-NO      PIC 99.
00040
00041      12  CN-LINES.
00042          16  CN-LINE OCCURS 10       PIC X(77).
00043
00044      12  CN-LAST-MAINT-DT            PIC XX.
00045      12  CN-LAST-MAINT-HHMMSS        PIC S9(7)   COMP-3.
00046      12  CN-LAST-MAINT-USER          PIC X(4).
00047      12  FILLER                      PIC X(6).
00048 ******************************************************************
      *                                copy ERCMAIL.
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
      *                                copy ERCENDR.
      ******************************************************************
      *                                                                *
      *                                                                *
      *                            ERCENDR.                            *
      *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
      *                            VMOD=2.001                          *
      *                                                                *
      *   FILE DESCRIPTION = ENDORSEMENT SUMMARY BY ACCOUNT            *
      *                                                                *
      *                                                                *
      *                                                                *
      *   FILE TYPE = VSAM,KSDS                                        *
PEMMOD*   RECORD SIZE = 513  RECFORM = FIXED                           *
      *                                                                *
      *   BASE CLUSTER = ERENDR                         RKP=2,LEN=36   *
      *                                                                *
      *   LOG = NO                                                     *
      *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
      ******************************************************************
052307*-----------------------------------------------------------------
052307*                   C H A N G E   L O G
052307*
052307* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
052307*-----------------------------------------------------------------
052307*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
052307* EFFECTIVE    NUMBER
052307*-----------------------------------------------------------------
052307* 052307    2006052600001  AJRA  ADDED FLAG FOR CANCELS ON CERTS
052307*                                WITH OPEN CLAIMS
052307*-----------------------------------------------------------------
       01  ENDORSEMENT-RECORD.
           12  EN-RECORD-ID            PIC XX.
               88  VALID-EN-ID             VALUE 'EN'.
           12  EN-CONTROL-PRIMARY.
               16  EN-COMPANY-CD       PIC X.
               16  EN-CARRIER          PIC X.
               16  EN-GROUPING         PIC X(6).
               16  EN-STATE            PIC XX.
               16  EN-ACCOUNT          PIC X(10).
               16  EN-CERT-EFF-DT      PIC XX.
               16  EN-CERT-NO.
                   20  EN-CERT-PRIME   PIC X(10).
                   20  EN-CERT-SFX     PIC X.
               16  EN-SEQ-NO           PIC S9(04) COMP.
               16  EN-REC-TYPE         PIC X.
                   88  EN-ISSUE             VALUE '1'.
                   88  EN-CANCELLATION      VALUE '2'.
           12  EN-LAST-MAINT-DT        PIC XX.
           12  EN-LAST-MAINT-BY        PIC X(4).
           12  EN-LAST-MAINT-HHMMSS    PIC S9(6)     COMP-3.
           12  EN-LAST-NAME            PIC X(15).
           12  EN-FIRST-NAME           PIC X(10).
           12  EN-MIDDLE-INIT          PIC X.
           12  EN-JOINT-LAST-NAME      PIC X(15).
           12  EN-JOINT-FIRST-NAME     PIC X(10).
           12  EN-JOINT-MIDDLE-INIT    PIC X.
           12  EN-LF-TERM              PIC S999      COMP-3.
           12  EN-AH-TERM              PIC S999      COMP-3.
           12  EN-LF-BEN-ENT-AMT       PIC S9(9)V99  COMP-3.
           12  EN-AH-BEN-ENT-AMT       PIC S9(7)V99  COMP-3.
           12  EN-LF-BEN-CAL-AMT       PIC S9(9)V99  COMP-3.
           12  EN-AH-BEN-CAL-AMT       PIC S9(7)V99  COMP-3.
           12  EN-LF-PREM-ENT-AMT      PIC S9(7)V99  COMP-3.
           12  EN-AH-PREM-ENT-AMT      PIC S9(7)V99  COMP-3.
           12  EN-LF-CANC-ENT-AMT      PIC S9(7)V99  COMP-3.
           12  EN-AH-CANC-ENT-AMT      PIC S9(7)V99  COMP-3.
           12  EN-LF-PREM-CAL-AMT      PIC S9(7)V99  COMP-3.
           12  EN-AH-PREM-CAL-AMT      PIC S9(7)V99  COMP-3.
           12  EN-LF-CANC-CAL-AMT      PIC S9(7)V99  COMP-3.
           12  EN-AH-CANC-CAL-AMT      PIC S9(7)V99  COMP-3.
           12  EN-LF-CANC-DT           PIC XX.
           12  EN-AH-CANC-DT           PIC XX.
           12  EN-LF-COMMISSION        PIC SV9(5)    COMP-3.
           12  EN-AH-COMMISSION        PIC SV9(5)    COMP-3.
           12  EN-LF-BEN-CD            PIC XX.
           12  EN-AH-BEN-CD            PIC XX.
052307*           12  FILLER                  PIC X(18).
052307     12  FILLER                  PIC X(17).
052307     12  EN-FLAG-CERT            PIC X.
           12  EN-INPUT-DT             PIC XX.
           12  EN-PRINT-DT             PIC XX.
           12  EN-SIG-SW               PIC X.
           12  EN-COMMENTS1            PIC X(70).
           12  EN-COMMENTS2            PIC X(70).
PEMMOD     12  EN-COMMENTS3            PIC X(70).
PEMMOD     12  EN-COMMENTS4            PIC X(70).
           12  FILLER                  PIC X(30).
      ******************************************************************
      *                                copy ERCENDT replacing
      *          ENDORSEMENT-RECORD by ENDT-RECORD
      *          LEADING ==EN-== BY ==ET-==.
      ******************************************************************
      *                                                                *
      *                                                                *
      *                            ERCENDT.                            *
      *                            VMOD=2.001                          *
      *                                                                *
      *   FILE DESCRIPTION = ENDORSEMENT FILE                          *
      *                                                                *
      *   FILE TYPE = VSAM,KSDS                                        *
PEMMOD*   RECORD SIZE = 579  RECFORM = FIXED                           *
      *                                                                *
      *   BASE CLUSTER = ERENDT                         RKP=02,LEN=36  *
      *       ALTERNATE PATH1 = ERENDT2 (BY ARCH NO)    RKP=38,LEN=05  *
      *                                                                *
      *   LOG = NO                                                     *
      *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
      ******************************************************************
052307*-----------------------------------------------------------------
052307*                   C H A N G E   L O G
052307*
052307* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
052307*-----------------------------------------------------------------
052307*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
052307* EFFECTIVE    NUMBER
052307*-----------------------------------------------------------------
052307* 052307    2006052600001  AJRA  ADDED FLAG FOR CANCELS ON CERTS
052307*                                WITH OPEN CLAIMS
072312* 072312    2011022800001  AJRA  ADDED BATCH NUMBER
110612* 110612    2012101700002  AJRA  ADD NEW FIELDS
121812* 121812  CR2012101700002  AJRA  ADD DEFAULT AGE FLAG
010616* 010616  CR2015082000001  PEMA  ADD ENDORSEMENT CHECK PROCESSING
052307*-----------------------------------------------------------------
       01  ENDT-RECORD.
           12  ET-RECORD-ID                PIC XX.
               88  VALID-EN-ID                VALUE 'EN'.
           12  ET-CONTROL-PRIMARY.
               16  ET-COMPANY-CD           PIC X.
               16  ET-CARRIER              PIC X.
               16  ET-GROUPING             PIC X(6).
               16  ET-STATE                PIC XX.
               16  ET-ACCOUNT              PIC X(10).
               16  ET-CERT-EFF-DT          PIC XX.
               16  ET-CERT-NO.
                   20  ET-CERT-PRIME       PIC X(10).
                   20  ET-CERT-SFX         PIC X.
               16  ET-REC-TYPE             PIC X.
                   88  ET-ISSUE               VALUE 'I'.
                   88  ET-CANCELLATION        VALUE 'C'.
               16  ET-SEQ-NO               PIC 9(04) BINARY.
           12  ET-CONTROL-BY-ARCH-NO.
               16  ET-COMPANY-CD-A1              PIC X.
               16  ET-ARCHIVE-NO                 PIC 9(8) BINARY.
           12  ET-LAST-MAINT-DT            PIC XX.
           12  ET-LAST-MAINT-BY            PIC X(4).
           12  ET-LAST-MAINT-HHMMSS        PIC S9(6)     COMP-3.
           12  ET-ENDORSEMENT-RECORD       PIC X(329).
           12  ET-ISSUE-RECORD REDEFINES ET-ENDORSEMENT-RECORD.
               16  ET-INS-ORIG-LAST-NAME   PIC X(15).
               16  ET-INS-ORIG-FIRST-NAME  PIC X(10).
               16  ET-INS-ORIG-MIDDLE-INIT PIC X.
               16  ET-INS-ORIG-AGE         PIC S999     COMP-3.
               16  ET-JNT-ORIG-LAST-NAME   PIC X(15).
               16  ET-JNT-ORIG-FIRST-NAME  PIC X(10).
               16  ET-JNT-ORIG-MIDDLE-INIT PIC X.
               16  ET-JNT-ORIG-AGE         PIC S999     COMP-3.
               16  ET-LF-ORIG-BENCD        PIC XX.
               16  ET-LF-ORIG-TERM         PIC S999      COMP-3.
               16  ET-LF-ORIG-BEN-AMT      PIC S9(9)V99  COMP-3.
               16  ET-LF-ORIG-PRM-AMT      PIC S9(7)V99  COMP-3.
               16  ET-LF-ORIG-ALT-BEN-AMT  PIC S9(9)V99  COMP-3.
               16  ET-LF-ORIG-ALT-PRM-AMT  PIC S9(7)V99  COMP-3.
               16  ET-LF-ORIG-EXP-DT       PIC XX.
      *        16  EN-LF-ORIG-COV-TYPE     PIC X(10).
               16  ET-ORIG-CRED-BENE       PIC X(25).
               16  ET-LF-ORIG-COMM-PCT     PIC SV9(5)    COMP-3.
               16  FILLER                  PIC X.
               16  ET-AH-ORIG-BENCD        PIC XX.
               16  ET-AH-ORIG-TERM         PIC S999      COMP-3.
               16  ET-AH-ORIG-BEN-AMT      PIC S9(9)V99  COMP-3.
               16  ET-AH-ORIG-PRM-AMT      PIC S9(7)V99  COMP-3.
               16  ET-AH-ORIG-EXP-DT       PIC XX.
      *        16  EN-AH-ORIG-COV-TYPE     PIC X(10).
      *        16  EN-AH-ORIG-WAIT-PER     PIC 99.
               16  ET-AH-ORIG-COMM-PCT     PIC SV9(5)    COMP-3.
               16  F                       PIC X(09).
               16  ET-AH-ORIG-CP           PIC 99.
               16  ET-INS-NEW-LAST-NAME    PIC X(15).
               16  ET-INS-NEW-FIRST-NAME   PIC X(10).
               16  ET-INS-NEW-MIDDLE-INIT  PIC X.
               16  ET-INS-NEW-AGE          PIC S999     COMP-3.
               16  ET-JNT-NEW-LAST-NAME    PIC X(15).
               16  ET-JNT-NEW-FIRST-NAME   PIC X(10).
               16  ET-JNT-NEW-MIDDLE-INIT  PIC X.
               16  ET-JNT-NEW-AGE          PIC S999     COMP-3.
               16  ET-LF-NEW-BENCD         PIC XX.
               16  ET-LF-NEW-TERM          PIC S999      COMP-3.
               16  ET-LF-NEW-BEN-AMT       PIC S9(9)V99  COMP-3.
               16  ET-LF-NEW-PRM-AMT       PIC S9(7)V99  COMP-3.
               16  ET-LF-NEW-ALT-BEN-AMT   PIC S9(9)V99  COMP-3.
               16  ET-LF-NEW-ALT-PRM-AMT   PIC S9(7)V99  COMP-3.
               16  ET-LF-NEW-EXP-DT        PIC XX.
               16  ET-NEW-CRED-BENE        PIC X(25).
               16  ET-LF-NEW-COMM-PCT      PIC SV9(5)    COMP-3.
               16  FILLER                  PIC X.
               16  ET-AH-NEW-BENCD         PIC XX.
               16  ET-AH-NEW-TERM          PIC S999      COMP-3.
               16  ET-AH-NEW-BEN-AMT       PIC S9(9)V99  COMP-3.
               16  ET-AH-NEW-PRM-AMT       PIC S9(7)V99  COMP-3.
               16  ET-AH-NEW-EXP-DT        PIC XX.
      *        16  EN-AH-NEW-COV-TYPE      PIC X(10).
      *        16  EN-AH-NEW-WAIT-PER      PIC 99.
      *        16  F                       PIC X(12).
               16  ET-AH-NEW-CP            PIC 99.
               16  ET-SIG-SW               PIC X.
               16  ET-AH-NEW-COMM-PCT      PIC SV9(5)    COMP-3.
121812         16  ET-INS-ORIG-AGE-DEF-FLAG PIC X.
121812         16  ET-JNT-ORIG-AGE-DEF-FLAG PIC X.
121812         16  ET-INS-NEW-AGE-DEF-FLAG PIC X.
121812         16  ET-JNT-NEW-AGE-DEF-FLAG PIC X.
121812         16  FILLER                  PIC X(33).
           12  ET-CANCEL-RECORD REDEFINES ET-ENDORSEMENT-RECORD.
               16  ET-LF-ORIG-REF-DT       PIC XX.
               16  ET-LF-ORIG-REF-AMT      PIC S9(7)V99  COMP-3.
               16  ET-AH-ORIG-REF-DT       PIC XX.
               16  ET-AH-ORIG-REF-AMT      PIC S9(7)V99  COMP-3.
               16  ET-LF-NEW-REF-DT        PIC XX.
               16  ET-LF-NEW-REF-AMT       PIC S9(7)V99  COMP-3.
               16  ET-AH-NEW-REF-DT        PIC XX.
               16  ET-AH-NEW-REF-AMT       PIC S9(7)V99  COMP-3.
               16  ET-FLAG-CERT            PIC X.
               16  ET-INS-LAST-NAME        PIC X(15).
               16  ET-INS-FIRST-NAME       PIC X(10).
               16  ET-INS-MIDDLE-INIT      PIC X.
110612         16  ET-LF-ORIG-REF-COMM-PCT PIC SV9(5)    COMP-3.
110612         16  ET-AH-ORIG-REF-COMM-PCT PIC SV9(5)    COMP-3.
110612         16  ET-LF-NEW-REF-COMM-PCT  PIC SV9(5)    COMP-3.
110612         16  ET-AH-NEW-REF-COMM-PCT  PIC SV9(5)    COMP-3.
110612         16  FILLER                  PIC X(262).
           12  ET-MONEY-SW             PIC X.
           12  ET-HEALTH-APP           PIC X.
           12  ET-VOUCHER-SW           PIC X.
           12  ET-PAYEE                PIC X(14).
           12  ET-INPUT-DT             PIC XX.
           12  ET-PROCESS-DT           PIC XX.
           12  ET-LF-COMMISSION        PIC SV9(5)    COMP-3.
           12  ET-AH-COMMISSION        PIC SV9(5)    COMP-3.
           12  ET-REASON-CODES.
               16  F OCCURS 12.
                   20  ET-REASON-CODE  PIC X(4).
           12  ET-TEMPLATE-USED        PIC X(8).
           12  ET-DOCU-TYPE            PIC X.
               88  ET-VERI-DOCU          VALUE 'V'.
               88  ET-GCE-DOCU           VALUE 'G'.
               88  ET-CANC-DOCU          VALUE 'C'.
           12  ET-COMMENTS1            PIC X(13).
           12  ET-COMMENTS2            PIC X(70).
           12  ET-COMM-CHGBK           PIC X.
               88  ET-DO-NOT-CHG-ACCT    VALUE 'N'.
               88  ET-CHG-ACCT           VALUE 'Y'.
           12  ET-CSO-PORTION          PIC S9(5)V99  COMP-3.
           12  ET-ACCT-PORTION         PIC S9(5)V99  COMP-3.
072312     12  ET-BATCH-NUMBER         PIC X(6).
072312     12  ET-ACCT-SUMM            PIC X.
072312     12  ET-CSO-SUMM             PIC X.
010616     12  en-check-type           pic x.
072312     12  FILLER                  PIC X(12).
      ******************************************************************
       01  ws-dummy-area               pic x(1000).
       01  ws-misc.
           05  ws-old-pndb-key.
               10  opk-company-cd      pic x.
               10  opk-carrier         pic x.
               10  opk-group           pic x(6).
               10  opk-state           pic xx.
               10  opk-account         pic x(10).
               10  opk-eff-dt          pic xx.
               10  opk-cert-no         pic x(11).
               10  opk-seq-no          pic xx.
               10  opk-rec-type        pic x.
       01  ws-disp-code                pic s9(11).
       01  WS-RESPONSE2                PIC S9(8) COMP VALUE +0.
       01  WS-RESPONSE                 PIC S9(8) COMP VALUE +0.
           88  RESP-NORMAL                    VALUE +0.
           88  resp-file-notfnd               value +12.
           88  RESP-NOTFND                    VALUE +13.
           88  resp-duprec                    value +14.
           88  resp-dupkey                    value +15.
           88  resp-invreq                    value +16.
           88  RESP-NOTOPEN                   VALUE +19.
           88  RESP-ENDFILE                   VALUE +20.
           88  resp-lengtherr                 value +22.
       01  erpndb-ridfld               pic x(11).
       01  erpndb2-ridfld              pic x(36).
       01  elcert-ridfld               pic x(33).
       01  elcrto-ridfld.
           05  filler                  pic x(33).
           05  elcrto-rec-type         pic x.
           05  elcrto-seq-no           pic s9(4) comp.
       01  nsasextr-ridfld.
           05  filler                  pic x(5).
           05  nsasextr-seq-no         pic s9(4) comp.
       01  elcrtt-ridfld.
           05  filler                  pic x(33).
           05  elcrtt-rec-type         pic x.
       01  erarch-ridfld               pic x(5).
       01  erarch2-ridfld.  *>  pic x(37).
           05  erarch2-company-cd      pic x.
           05  erarch2-cert-no         pic x(11).
           05  erarch2-carrier         pic x.
           05  erarch2-group           pic x(6).
           05  erarch2-state           pic xx.
           05  erarch2-account         pic x(10).
           05  erarch2-eff-dt          pic xx.
           05  erarch2-arch-no         pic s9(8) comp.
       01  erchek-ridfld.
           05  filler                  pic x(33).
           05  erchek-seq-no           pic s9(4) comp.
       01  ercnot-ridfld.
           05  filler                  pic x(33).
           05  ercnot-rec-type         pic x.
           05  ercnot-seq-no           pic s9(4) comp.
       01  ernote-ridfld               pic x(33).
       01  ermail-ridfld               pic x(33).
       01  erendr-ridfld.
           05  filler                  pic x(33).
           05  erendr-seq-no           pic s9(4) comp.
           05  erendr-rec-type         pic x.
       01  erendt-ridfld.
           05  filler                  pic x(33).
           05  erendt-rec-type         pic x.
           05  erendt-seq-no           pic s9(4) comp.
       01  msg-error.
           05  filler                  pic x(08) value ' error- '.
           05  me-response             pic 9(5)  value zeros.
           05  filler                  pic xx value spaces.
           05  me-response2            pic 9(5)  value zeros.
           05  filler                  pic x(5)  value spaces.
           05  me-command              pic x(20) value spaces.
       01  WS-PASS-AREA.
           05  PA-ERPNDB-KEY           PIC X(11).
           05  pa-new-pndb-key.
               10  filler              pic x(20).
               10  pa-new-eff-dt       pic xx.
               10  pa-new-cert-no      pic x(11).
               10  filler              pic xxx.
           05  pa-return-area.
               10  ra-status-code      pic 9(4).
               10  ra-message          pic x(205).
      *                                COPY ELCDATE.
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
       01  DFHCOMMAREA                 PIC X(256).
       01  var  pic x(30).
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA VAR.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL055' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
           display ' entering program EL055'
           perform 0010-init           thru 0010-exit
           perform 0100-check-validity thru 0100-exit
           perform 0200-convert-key    thru 0200-exit
           perform 0050-bld-return-area thru 0050-exit
           .
       0000-return.
           move ws-pass-area           to dfhcommarea
           
      * exec cics return
      *    end-exec
      *    MOVE '.(                    ''   #00002437' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           
      * GOBACK

           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL055' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK
           .
       0010-init.
           move dfhcommarea            to ws-pass-AREA
           set P to address of KIXSYS
           CALL "getenv" using by value P returning var-ptr
           if var-ptr = null then
              display ' kixsys not set '
           else
              set address of var to var-ptr
              move 0 to env-var-len
              inspect var tallying env-var-len
                for characters before X'00'
              unstring var (1:env-var-len) delimited by '/'
                 into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
                    WS-KIX-SYS
              end-unstring
           end-if
           .
       0010-exit.
           exit.
       0050-bld-return-area.
           move zeros                  to ra-status-code
           move 'SUCCESS'              to ra-message
           .
       0050-exit.
           exit.
       0100-check-validity.
      **** ======================================================== ****
      ****                                                          ****
      ****    Make sure Key to change exists on ERPNDB              ****
      ****                                                          ****
      **** ======================================================== ****
           move PA-ERPNDB-KEY          to erpndb-ridfld
           
      * exec cics read
      *       dataset    ('ERPNDB')
      *       ridfld     (erpndb-ridfld)
      *       into       (pending-business)
      *       resp       (ws-response)
      *    end-exec
           MOVE LENGTH OF
            pending-business
             TO DFHEIV11
           MOVE 'ERPNDB' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00002473' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303032343733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 pending-business, 
                 DFHEIV11, 
                 erpndb-ridfld, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if not resp-normal
              move ws-response         to ra-status-code
              move ' erpndb read error '
                                       to ra-message
              display 'error-erpndb-read ' ws-response
              go to 0000-return
           end-if
           move pb-control-by-account  to ws-old-pndb-key
           move pb-cert-eff-dt         to dc-bin-date-1
           move ' '                    to dc-option-code
           perform 8500-date-convert   thru 8500-exit
           if no-conversion-error
              string
                 dc-cymd-cen
                 dc-cymd-year   '-'
                 dc-cymd-month  '-'
                 dc-cymd-day delimited by size
                    into ws-old-greg-eff-date
              end-string
           else
              display ' Bad eff date convert ' pb-cert-no
              go to 0000-return
           end-if
      ***  Might as well convert the new effective dt while we're at it
           move pa-new-eff-dt          to dc-bin-date-1
           move ' '                    to dc-option-code
           perform 8500-date-convert   thru 8500-exit
           if no-conversion-error
              string
                 dc-cymd-cen
                 dc-cymd-year   '-'
                 dc-cymd-month  '-'
                 dc-cymd-day delimited by size
                    into ws-new-greg-eff-date
              end-string
           else
              display ' Bad new eff date convert ' pb-cert-no
              go to 0000-return
           end-if
      **** ======================================================== ****
      **** ======================================================== ****
      ****                                                          ****
      ****    Make sure Key to change exists on ELCERT              ****
      ****                                                          ****
      **** ======================================================== ****
072320     move spaces                 to ws-bypass-ind
           move ws-old-pndb-key(1:33)  to elcert-ridfld
           
      * exec cics read
      *       dataset    ('ELCERT')
      *       ridfld     (elcert-ridfld)
      *       into       (certificate-master)
      *       resp       (ws-response)
      *    end-exec
           MOVE LENGTH OF
            certificate-master
             TO DFHEIV11
           MOVE 'ELCERT' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00002526' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303032353236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 certificate-master, 
                 DFHEIV11, 
                 elcert-ridfld, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
072920     if ws-response = 13
072920        set bypass-copy-files to true
072920        go to 0100-check-erpndb2
072920     end-if
           if not resp-normal
              move ws-response         to ra-status-code
              move ' elcert read error '
                                       to ra-message
              display 'error-elcert-read ' ws-response
              go to 0000-return
           end-if
072320     if cert-added-batch
072320        set bypass-copy-files to true
072320     end-if
           if (cert-and-claim-online)
              or (cert-was-created-for-claim)
072320        or (cm-claim-attached-count > zeros)
              move +99                 to ra-status-code
              move 'Claim Exists '     to ra-message
              go to 0000-return
           end-if
      **** ======================================================== ****
072920     .
072920 0100-check-erpndb2.
      **** ======================================================== ****
      ****                                                          ****
      ****    Make sure new key does NOT exist on ERPNDB2           ****
      ****                                                          ****
      **** ======================================================== ****
           move PA-new-pndb-key        to erpndb2-ridfld
           
      * exec cics read
      *       dataset    ('ERPNDB2')
      *       ridfld     (erpndb2-ridfld)
      *       into       (ws-dummy-area)
      *       resp       (ws-response)
      *    end-exec
           MOVE LENGTH OF
            ws-dummy-area
             TO DFHEIV11
           MOVE 'ERPNDB2' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00002562' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303032353632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ws-dummy-area, 
                 DFHEIV11, 
                 erpndb2-ridfld, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if (not RESP-NOTFND)
              and (not resp-endfile)
              move +88                 to ra-status-code
              move ' pndb already exists '
                                       to ra-message
              display 'error-erpndb2-read ' ws-response
              go to 0000-return
           end-if
      **** ======================================================== ****
      **** ======================================================== ****
      ****                                                          ****
      ****    Make sure new key does NOT exist on ELCERT            ****
      ****                                                          ****
      **** ======================================================== ****
           move PA-new-pndb-key(1:33)  to elcert-ridfld
           
      * exec cics read
      *       dataset    ('ELCERT')
      *       ridfld     (elcert-ridfld)
      *       into       (ws-dummy-area)
      *       resp       (ws-response)
      *    end-exec
           MOVE LENGTH OF
            ws-dummy-area
             TO DFHEIV11
           MOVE 'ELCERT' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00002583' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303032353833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ws-dummy-area, 
                 DFHEIV11, 
                 elcert-ridfld, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if (not RESP-NOTFND)
              and (not resp-endfile)
              display ' setting status to 88 '
              move +88                 to ra-status-code
              move ' elcert already exist '
                                       to ra-message
              display 'error-elcert-read-exists ' ws-response
              go to 0000-return
           end-if
      **** ======================================================== ****
      **** ======================================================== ****
      ****                                                          ****
      ****    Right here, 1)rewrite erpndb with new key, 2)delete   ****
      ****  elcert.                                                 ****
      ****                                                          ****
      **** ======================================================== ****
      ** 1)  Rewrite erpndb with new key
           move PA-ERPNDB-KEY          to erpndb-ridfld
           
      * exec cics read update
      *       dataset    ('ERPNDB')
      *       ridfld     (erpndb-ridfld)
      *       into       (pending-business)
      *       resp       (ws-response)
      *    end-exec
           MOVE LENGTH OF
            pending-business
             TO DFHEIV11
           MOVE 'ERPNDB' TO DFHEIV1
      *    MOVE '&"IL       EU         (  N#00002607' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303032363037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 pending-business, 
                 DFHEIV11, 
                 erpndb-ridfld, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if not resp-normal
              move ws-response         to ra-status-code
              move ' erpndb read update error '
                                       to ra-message
              display 'error-erpndb-read update ' ws-response
              go to 0000-return
           end-if
           move pa-new-eff-dt          to pb-cert-eff-dt
           move pa-new-cert-no         to pb-cert-no
           
      * exec cics rewrite
      *       dataset  ('ERPNDB')
      *       from     (pending-business)
      *       resp     (ws-response)
      *    end-exec
           MOVE LENGTH OF
            pending-business
             TO DFHEIV11
           MOVE 'ERPNDB' TO DFHEIV1
      *    MOVE '&& L                  %  N#00002622' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'204E233030303032363232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 pending-business, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if not resp-normal
              move ws-response         to ra-status-code
              move ' erpndb rewrite error '
                                       to ra-message
              display 'error-erpndb-rewrite ' ws-response
              go to 0000-return
           end-if
           .
       0100-check-for-claim.
072320     if bypass-copy-files
072320        go to 0000-return
072320     end-if
           move ws-old-pndb-key(1:33)  to elcert-ridfld
           
      * exec cics read update
      *       dataset    ('ELCERT')
      *       ridfld     (elcert-ridfld)
      *       into       (certificate-master)
      *       resp       (ws-response)
      *    end-exec
           MOVE LENGTH OF
            certificate-master
             TO DFHEIV11
           MOVE 'ELCERT' TO DFHEIV1
      *    MOVE '&"IL       EU         (  N#00002640' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303032363430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 certificate-master, 
                 DFHEIV11, 
                 elcert-ridfld, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if (cert-added-batch)
              or (cert-and-claim-online)
              or (cert-was-created-for-claim)
              continue
           else
              go to 0100-delete-elcert
           end-if
           if (no-claim-attached)
              or (cert-added-batch)
072320        
      * exec cics unlock
072320*          dataset   ('ELCERT')
072320*          resp      (ws-response)
072320*       end-exec
           MOVE 'ELCERT' TO DFHEIV1
      *    MOVE '&*                    #  N#00002655' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'204E233030303032363535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           else
              move spaces              to cm-credit-interface-sw-1
              move '2'                 to cm-claim-interface-sw
              
      * exec cics rewrite
      *          dataset   ('ELCERT')
      *          from      (certificate-master)
      *          resp      (ws-response)
      *       end-exec
           MOVE LENGTH OF
            certificate-master
             TO DFHEIV11
           MOVE 'ELCERT' TO DFHEIV1
      *    MOVE '&& L                  %  N#00002662' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'204E233030303032363632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 certificate-master, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           end-if
           if not resp-normal
              move ws-response         to ra-status-code
              move ' elcert rewrite error clm '
                                       to ra-message
              display 'error-elcert-rewrite ' ws-response
              go to 0000-return
           end-if
           go to 0100-write-new-elcert
           .
       0100-delete-elcert.
      ** 2)  Delete ELCERT (old key)
           move ws-old-pndb-key(1:33)  to elcert-ridfld
           
      * exec cics delete
      *       dataset  ('ELCERT')
      *       resp     (ws-response)
      *    end-exec
           MOVE 'ELCERT' TO DFHEIV1
      *    MOVE '&(                    &  N#00002680' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303032363830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if not resp-normal
              move ws-response         to ra-status-code
              move ' elcert delete error '
                                       to ra-message
              display 'error-elcert-delete  ' ws-response
              go to 0000-return
           end-if
           .
       0100-write-new-elcert.
      **  This is commented out because I want EL6311 to write
      **    the new ELCERT record.
      ** 3)  Write new ELCERT record
      **  Not now. Let el6311 write the new one??
      **   move pa-new-eff-dt          to cm-cert-eff-dt
      **   move pa-new-cert-no         to cm-cert-no
      **
      **   exec cics write
      **      dataset  ('ELCERT')
      **      from     (certificate-master)
      **      ridfld   (cm-control-primary)
      **      resp     (ws-response)
      **   end-exec
      **
      **   if not resp-normal
      **      move ws-response         to ra-status-code
      **                               to ra-message
      **      display 'error-elcert-write   ' ws-response
      **      go to 0000-return
      **   end-if
      **** ======================================================== ****
      ****                                                          ****
      ****    Check for ELCRTO records                              ****
      ****                                                          ****
      **** ======================================================== ****
           move ' '                    to ws-startbr-ind
           move ws-old-pndb-key(1:33)  to elcrto-ridfld(1:33)
           move opk-rec-type           to elcrto-rec-type
           move +0                     to elcrto-seq-no
           
      * exec cics startbr
      *       dataset    ('ELCRTO')
      *       ridfld     (elcrto-ridfld)
      *       gteq
      *       resp       (ws-response)
      *    end-exec
           MOVE 'ELCRTO' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00002723' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303032373233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 elcrto-ridfld, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if RESP-NOTFND or resp-endfile
              go to 0100-end-elcrto
           else
              if not resp-normal
                 move ws-response      to ra-status-code
                 move ' elcrto error startbr '
                                       to ra-message
                 display 'error-elcrto-startbr ' ws-response
                 go to 0000-return
              end-if
           end-if
           move 'Y'                    to ws-startbr-ind
           .
       0100-elcrto-loop.
           
      * exec cics readnext
      *       dataset    ('ELCRTO')
      *       ridfld     (elcrto-ridfld)
      *       into       (original-certificate)
      *       resp       (ws-response)
      *    end-exec
           MOVE LENGTH OF
            original-certificate
             TO DFHEIV12
           MOVE 'ELCRTO' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00002743' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303032373433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 original-certificate, 
                 DFHEIV12, 
                 elcrto-ridfld, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if (RESP-NOTFND)
              or (resp-endfile)
              or (elcrto-ridfld(1:33) <> ws-old-pndb-key(1:33))
              go to 0100-end-elcrto
           else
              if not resp-normal
                 move ws-response      to ra-status-code
                 move ' elcrto error readnext '
                                       to ra-message
                 display 'error-elcrto-readnext ' ws-response
                 go to 0000-return
              end-if
           end-if
           if opk-rec-type <> oc-record-type
              go to 0100-elcrto-loop
           end-if
      ***  delete and add with new key right here!!!
           
      * exec cics delete
      *       dataset  ('ELCRTO')
      *       ridfld   (elcrto-ridfld)
      *       resp     (ws-response)
      *    end-exec
           MOVE 'ELCRTO' TO DFHEIV1
      *    MOVE '&(  R                 &  N#00002766' TO DFHEIV0
           MOVE X'262820205220202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303032373636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 elcrto-ridfld, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if not resp-normal
              move ws-response         to ra-status-code
              move ' elcrto delete error '
                                       to ra-message
              display 'error-elcrto-delete  ' ws-response
              go to 0000-return
           end-if
           move pa-new-eff-dt          to oc-cert-eff-dt
           move pa-new-cert-no         to oc-cert-no
           
      * exec cics write
      *       dataset  ('ELCRTO')
      *       from     (original-certificate)
      *       ridfld   (oc-control-primary)
      *       resp     (ws-response)
      *    end-exec
           MOVE LENGTH OF
            original-certificate
             TO DFHEIV11
           MOVE 'ELCRTO' TO DFHEIV1
      *    MOVE '&$ L                  ''  N#00002780' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303032373830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 original-certificate, 
                 DFHEIV11, 
                 oc-control-primary, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if not resp-normal
              move ws-response         to ra-status-code
              move ' elcrto write  error '
                                       to ra-message
              display 'error-elcrto-write   ' ws-response
              go to 0000-return
           end-if
           go to 0100-elcrto-loop
           .
       0100-end-elcrto.
           if ws-startbr-ind = 'Y'
              
      * exec cics endbr
      *          dataset     ('ELCRTO')
      *       end-exec
           MOVE 'ELCRTO' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00002797' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           end-if
            .
       0100-elcrtt.
      **** ======================================================== ****
      ****                                                          ****
      ****    Check for ELCRTT records                              ****
      ****                                                          ****
      **** ======================================================== ****
           move ' '                    to ws-startbr-ind
           move ws-old-pndb-key(1:33)  to elcrtt-ridfld(1:33)
           move ' '                    to elcrtt-rec-type
           
      * exec cics startbr
      *       dataset    ('ELCRTT')
      *       ridfld     (elcrtt-ridfld)
      *       gteq
      *       resp       (ws-response)
      *    end-exec
           MOVE 'ELCRTT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00002811' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303032383131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 elcrtt-ridfld, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if RESP-NOTFND or resp-endfile
              go to 0100-end-elcrtt
           else
              if not resp-normal
                 move ws-response      to ra-status-code
                 move ' elcrtt error startbr '
                                       to ra-message
                 display 'error-elcrtt-startbr ' ws-response
                 go to 0000-return
              end-if
           end-if
           move 'Y'                    to ws-startbr-ind
           .
       0100-elcrtt-loop.
           
      * exec cics readnext
      *       dataset    ('ELCRTT')
      *       ridfld     (elcrtt-ridfld)
      *       into       (certificate-trailers)
      *       resp       (ws-response)
      *    end-exec
           MOVE LENGTH OF
            certificate-trailers
             TO DFHEIV12
           MOVE 'ELCRTT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00002831' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303032383331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 certificate-trailers, 
                 DFHEIV12, 
                 elcrtt-ridfld, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if (RESP-NOTFND)
              or (resp-endfile)
              or (elcrtt-ridfld(1:33) <> ws-old-pndb-key(1:33))
              go to 0100-end-elcrtt
           else
              if not resp-normal
                 move ws-response      to ra-status-code
                 move ' elcrtt error readnext '
                                       to ra-message
                 display 'error-elcrtt-readnext ' ws-response
                 go to 0000-return
              end-if
           end-if
      ***  delete and add with new key right here!!!
           
      * exec cics delete
      *       dataset  ('ELCRTT')
      *       ridfld   (elcrtt-ridfld)
      *       resp     (ws-response)
      *    end-exec
           MOVE 'ELCRTT' TO DFHEIV1
      *    MOVE '&(  R                 &  N#00002851' TO DFHEIV0
           MOVE X'262820205220202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303032383531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 elcrtt-ridfld, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if not resp-normal
              move ws-response         to ra-status-code
              move ' elcrtt delete error '
                                       to ra-message
              display 'error-elcrtt-delete  ' ws-response
              go to 0000-return
           end-if
           move pa-new-eff-dt          to cs-cert-eff-dt
           move pa-new-cert-no         to cs-cert-no
           
      * exec cics write
      *       dataset  ('ELCRTT')
      *       from     (certificate-trailers)
      *       ridfld   (cs-control-primary)
      *       resp     (ws-response)
      *    end-exec
           MOVE LENGTH OF
            certificate-trailers
             TO DFHEIV11
           MOVE 'ELCRTT' TO DFHEIV1
      *    MOVE '&$ L                  ''  N#00002865' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303032383635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 certificate-trailers, 
                 DFHEIV11, 
                 cs-control-primary, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if not resp-normal
              move ws-response         to ra-status-code
              move ' elcrtt write  error '
                                       to ra-message
              display 'error-elcrtt-write   ' ws-response
              go to 0000-return
           end-if
           go to 0100-elcrtt-loop
           .
       0100-end-elcrtt.
           if ws-startbr-ind = 'Y'
              
      * exec cics endbr
      *          dataset     ('ELCRTT')
      *       end-exec
           MOVE 'ELCRTT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00002882' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           end-if
           .
       0100-erarch.
      **** ======================================================== ****
      ****                                                          ****
      ****    Check for ERARCH records                              ****
      ****                                                          ****
      **** ======================================================== ****
           move ' '                    to ws-startbr-ind
           move opk-company-cd         to erarch2-company-cd
           move opk-cert-no            to erarch2-cert-no
           move ws-old-pndb-key(1:21)  to erarch2-ridfld(13:21)
           move +0                     to erarch2-arch-no
           
      * exec cics startbr
      *       dataset    ('ERARCH2')
      *       ridfld     (erarch2-ridfld)
      *       gteq
      *       resp       (ws-response)
      *    end-exec
           MOVE 'ERARCH2' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00002898' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303032383938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 erarch2-ridfld, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if RESP-NOTFND or resp-endfile
              go to 0100-end-erarch
           else
              if not resp-normal
                 move ws-response      to ra-status-code
                 move ' erarch2 error startbr '
                                       to ra-message
                 display 'error-erarch2-startbr ' ws-response
                 go to 0000-return
              end-if
           end-if
           move 'Y'                    to ws-startbr-ind
           .
       0100-erarch-loop.
           
      * exec cics readnext
      *       dataset    ('ERARCH2')
      *       ridfld     (erarch2-ridfld)
      *       into       (letter-archive)
      *       resp       (ws-response)
      *    end-exec
           MOVE LENGTH OF
            letter-archive
             TO DFHEIV12
           MOVE 'ERARCH2' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00002918' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303032393138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 letter-archive, 
                 DFHEIV12, 
                 erarch2-ridfld, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if (RESP-NOTFND)
              or (resp-endfile)
              or (erarch2-company-cd <> opk-company-cd)
              or (erarch2-cert-no <> opk-cert-no)
              go to 0100-end-erarch
           else
              if not resp-normal
                 move ws-response      to ra-status-code
                 move ' erarch2 error readnext '
                                       to ra-message
                 display 'error-erarch2-readnext ' ws-response
                 go to 0000-return
              end-if
           end-if
           if (opk-carrier <> la-carrier-a2)
              or (opk-group <> la-grouping-a2)
              or (opk-state <> la-state-a2)
              or (opk-account <> la-account-a2)
              or (opk-eff-dt <> la-effect-date-a2)
              go to 0100-erarch-loop
           end-if
      ***  Do rewrites to base cluster right here!!!
           move la-control-primary     to erarch-ridfld
                                          nsasextr-ridfld
           
      * exec cics read update
      *       dataset  ('ERARCH')
      *       into     (letter-archive)
      *       ridfld   (erarch-ridfld)
      *       resp     (ws-response)
      *    end-exec
           MOVE LENGTH OF
            letter-archive
             TO DFHEIV11
           MOVE 'ERARCH' TO DFHEIV1
      *    MOVE '&"IL       EU         (  N#00002948' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303032393438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 letter-archive, 
                 DFHEIV11, 
                 erarch-ridfld, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if not resp-normal
              move ws-response         to ra-status-code
              move ' erarch read update error '
                                       to ra-message
              display 'error-erarch-read update ' ws-response
              go to 0000-return
           end-if
           move pa-new-eff-dt          to la-effect-date-a2
           move pa-new-cert-no         to la-cert-no-a2
           
      * exec cics rewrite
      *       dataset  ('ERARCH')
      *       from     (letter-archive)
      *       resp     (ws-response)
      *    end-exec
           MOVE LENGTH OF
            letter-archive
             TO DFHEIV11
           MOVE 'ERARCH' TO DFHEIV1
      *    MOVE '&& L                  %  N#00002963' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'204E233030303032393633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 letter-archive, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if not resp-normal
              move ws-response         to ra-status-code
              move ' erarch write  error '
                                       to ra-message
              display 'error-erarch-write   ' ws-response
              go to 0000-return
           end-if
           perform 0500-process-nsasextr thru 0500-exit
           go to 0100-erarch-loop
           .
       0100-end-erarch.
           if ws-startbr-ind = 'Y'
              
      * exec cics endbr
      *          dataset     ('ERARCH2')
      *       end-exec
           MOVE 'ERARCH2' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00002980' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           end-if
           .
       0100-erchek.
      **** ======================================================== ****
      ****                                                          ****
      ****    Check for ERCHEK records                              ****
      ****                                                          ****
      **** ======================================================== ****
           move ' '                    to ws-startbr-ind
           move ws-old-pndb-key(1:33)  to erchek-ridfld(1:33)
           move +0                     to erchek-seq-no
           
      * exec cics startbr
      *       dataset    ('ERCHEK')
      *       ridfld     (erchek-ridfld)
      *       gteq
      *       resp       (ws-response)
      *    end-exec
           MOVE 'ERCHEK' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00002994' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303032393934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 erchek-ridfld, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if RESP-NOTFND or resp-endfile
              go to 0100-end-erchek
           else
              if not resp-normal
                 move ws-response      to ra-status-code
                 move ' erchek error startbr '
                                       to ra-message
                 display 'error-erchek-startbr ' ws-response
                 go to 0000-return
              end-if
           end-if
           move 'Y'                    to ws-startbr-ind
           .
       0100-erchek-loop.
           
      * exec cics readnext
      *       dataset    ('ERCHEK')
      *       ridfld     (erchek-ridfld)
      *       into       (check-records)
      *       resp       (ws-response)
      *    end-exec
           MOVE LENGTH OF
            check-records
             TO DFHEIV12
           MOVE 'ERCHEK' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00003014' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303033303134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 check-records, 
                 DFHEIV12, 
                 erchek-ridfld, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if (RESP-NOTFND)
              or (resp-endfile)
              or (erchek-ridfld(1:33) <> ws-old-pndb-key(1:33))
              go to 0100-end-erchek
           else
              if not resp-normal
                 move ws-response      to ra-status-code
                 move ' erchek error readnext '
                                       to ra-message
                 display 'error-erchek-readnext ' ws-response
                 go to 0000-return
              end-if
           end-if
      ***  delete and add with new key right here!!!
           
      * exec cics delete
      *       dataset  ('ERCHEK')
      *       ridfld   (erchek-ridfld)
      *       resp     (ws-response)
      *    end-exec
           MOVE 'ERCHEK' TO DFHEIV1
      *    MOVE '&(  R                 &  N#00003034' TO DFHEIV0
           MOVE X'262820205220202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303033303334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 erchek-ridfld, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if not resp-normal
              move ws-response         to ra-status-code
              move ' erchek delete error '
                                       to ra-message
              display 'error-erchek-delete  ' ws-response
              go to 0000-return
           end-if
           move pa-new-eff-dt          to ch-cert-eff-dt
           move pa-new-cert-no         to ch-cert-no
           
      * exec cics write
      *       dataset  ('ERCHEK')
      *       from     (check-records)
      *       ridfld   (ch-control-primary)
      *       resp     (ws-response)
      *    end-exec
           MOVE LENGTH OF
            check-records
             TO DFHEIV11
           MOVE 'ERCHEK' TO DFHEIV1
      *    MOVE '&$ L                  ''  N#00003048' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303033303438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 check-records, 
                 DFHEIV11, 
                 ch-control-primary, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if not resp-normal
              move ws-response         to ra-status-code
              move ' erchek write  error '
                                       to ra-message
              display 'error-erchek-write   ' ws-response
              go to 0000-return
           end-if
           go to 0100-erchek-loop
           .
       0100-end-erchek.
           if ws-startbr-ind = 'Y'
              
      * exec cics endbr
      *          dataset     ('ERCHEK')
      *       end-exec
           MOVE 'ERCHEK' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00003065' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033303635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           end-if
           .
       0100-ercnot.
      **** ======================================================== ****
      ****                                                          ****
      ****    Check for ERCNOT records                              ****
      ****                                                          ****
      **** ======================================================== ****
           move ' '                    to ws-startbr-ind
           move ws-old-pndb-key(1:33)  to ercnot-ridfld(1:33)
           move opk-rec-type           to ercnot-rec-type
           move +0                     to ercnot-seq-no
           
      * exec cics startbr
      *       dataset    ('ERCNOT')
      *       ridfld     (ercnot-ridfld)
      *       gteq
      *       resp       (ws-response)
      *    end-exec
           MOVE 'ERCNOT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00003080' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303033303830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ercnot-ridfld, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if RESP-NOTFND or resp-endfile
              go to 0100-end-ercnot
           else
              if not resp-normal
                 move ws-response      to ra-status-code
                 move ' ercnot error startbr '
                                       to ra-message
                 display 'error-ercnot-startbr ' ws-response
                 go to 0000-return
              end-if
           end-if
           move 'Y'                    to ws-startbr-ind
           .
       0100-ercnot-loop.
           
      * exec cics readnext
      *       dataset    ('ERCNOT')
      *       ridfld     (ercnot-ridfld)
      *       into       (cert-note-file)
      *       resp       (ws-response)
      *    end-exec
           MOVE LENGTH OF
            cert-note-file
             TO DFHEIV12
           MOVE 'ERCNOT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00003100' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303033313030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 cert-note-file, 
                 DFHEIV12, 
                 ercnot-ridfld, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if (RESP-NOTFND)
              or (resp-endfile)
              or (ercnot-ridfld(1:33) <> ws-old-pndb-key(1:33))
              go to 0100-end-ercnot
           else
              if not resp-normal
                 move ws-response      to ra-status-code
                 move ' ercnot error readnext '
                                       to ra-message
                 display 'error-ercnot-readnext ' ws-response
                 go to 0000-return
              end-if
           end-if
           if cz-record-type <> '1'  *>  cert note
              go to 0100-ercnot-loop
           end-if
      ***  delete and add with new key right here!!!
           
      * exec cics delete
      *       dataset  ('ERCNOT')
      *       ridfld   (ercnot-ridfld)
      *       resp     (ws-response)
      *    end-exec
           MOVE 'ERCNOT' TO DFHEIV1
      *    MOVE '&(  R                 &  N#00003123' TO DFHEIV0
           MOVE X'262820205220202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303033313233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ercnot-ridfld, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if not resp-normal
              move ws-response         to ra-status-code
              move ' ercnot delete error '
                                       to ra-message
              display 'error-ercnot-delete  ' ws-response
              go to 0000-return
           end-if
           move pa-new-eff-dt          to cz-cert-eff-dt
           move pa-new-cert-no         to cz-cert-no
           
      * exec cics write
      *       dataset  ('ERCNOT')
      *       from     (cert-note-file)
      *       ridfld   (cz-control-primary)
      *       resp     (ws-response)
      *    end-exec
           MOVE LENGTH OF
            cert-note-file
             TO DFHEIV11
           MOVE 'ERCNOT' TO DFHEIV1
      *    MOVE '&$ L                  ''  N#00003137' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303033313337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 cert-note-file, 
                 DFHEIV11, 
                 cz-control-primary, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if not resp-normal
              move ws-response         to ra-status-code
              move ' ercnot write  error '
                                       to ra-message
              display 'error-ercnot-write   ' ws-response
              go to 0000-return
           end-if
           go to 0100-ercnot-loop
           .
       0100-end-ercnot.
           if ws-startbr-ind = 'Y'
              
      * exec cics endbr
      *          dataset     ('ERCNOT')
      *       end-exec
           MOVE 'ERCNOT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00003154' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033313534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           end-if
            .
       0100-ernote.
      **** ======================================================== ****
      ****                                                          ****
      ****    Check for ERNOTE record.                              ****
      ****                                                          ****
      **** ======================================================== ****
           move ws-old-pndb-key(1:33)  to ernote-ridfld
           
      * exec cics read
      *       dataset    ('ERNOTE')
      *       ridfld     (ernote-ridfld)
      *       into       (certificate-note)
      *       resp       (ws-response)
      *    end-exec
           MOVE LENGTH OF
            certificate-note
             TO DFHEIV11
           MOVE 'ERNOTE' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00003166' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303033313636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 certificate-note, 
                 DFHEIV11, 
                 ernote-ridfld, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if (RESP-NOTFND)
              or (resp-endfile)
              go to 0100-end-ernote
           else
              if not resp-normal
                 move ws-response      to ra-status-code
                 move ' ernote error readnext '
                                       to ra-message
                 display 'error-ernote-readnext ' ws-response
                 go to 0000-return
              end-if
           end-if
      ***  delete and add with new key right here!!!
      ** 1)  Delete ERNOTE (old key)
           
      * exec cics delete
      *       dataset  ('ERNOTE')
      *       ridfld   (ernote-ridfld)
      *       resp     (ws-response)
      *    end-exec
           MOVE 'ERNOTE' TO DFHEIV1
      *    MOVE '&(  R                 &  N#00003186' TO DFHEIV0
           MOVE X'262820205220202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303033313836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ernote-ridfld, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if not resp-normal
              move ws-response         to ra-status-code
              move ' ernote delete error '
                                       to ra-message
              display 'error-ernote-delete  ' ws-response
              go to 0000-return
           end-if
      ** 2)  Write new ERNOTE record
           move pa-new-eff-dt          to cn-cert-eff-dt
           move pa-new-cert-no         to cn-cert-no
           
      * exec cics write
      *       dataset  ('ERNOTE')
      *       from     (certificate-note)
      *       ridfld   (cn-control-primary)
      *       resp     (ws-response)
      *    end-exec
           MOVE LENGTH OF
            certificate-note
             TO DFHEIV11
           MOVE 'ERNOTE' TO DFHEIV1
      *    MOVE '&$ L                  ''  N#00003201' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303033323031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 certificate-note, 
                 DFHEIV11, 
                 cn-control-primary, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if not resp-normal
              move ws-response         to ra-status-code
              move ' ernote write  error '
                                       to ra-message
              display 'error-ernote-write   ' ws-response
              go to 0000-return
           end-if
           .
       0100-end-ernote.
           .
       0100-ermail.
      **** ======================================================== ****
      ****                                                          ****
      ****    Check for ERMAIL record.                              ****
      ****                                                          ****
      **** ======================================================== ****
           move ws-old-pndb-key(1:33)  to ermail-ridfld
           
      * exec cics read
      *       dataset    ('ERMAIL')
      *       ridfld     (ermail-ridfld)
      *       into       (mailing-data)
      *       resp       (ws-response)
      *    end-exec
           MOVE LENGTH OF
            mailing-data
             TO DFHEIV11
           MOVE 'ERMAIL' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00003224' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303033323234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 mailing-data, 
                 DFHEIV11, 
                 ermail-ridfld, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if (RESP-NOTFND)
              or (resp-endfile)
              go to 0100-end-ermail
           else
              if not resp-normal
                 move ws-response      to ra-status-code
                 move ' ermail error readnext '
                                       to ra-message
                 display 'error-ermail-readnext ' ws-response
                 go to 0000-return
              end-if
           end-if
      ***  delete and add with new key right here!!!
      ** 1)  Delete ERMAIL (old key)
           
      * exec cics delete
      *       dataset  ('ERMAIL')
      *       ridfld   (ermail-ridfld)
      *       resp     (ws-response)
      *    end-exec
           MOVE 'ERMAIL' TO DFHEIV1
      *    MOVE '&(  R                 &  N#00003244' TO DFHEIV0
           MOVE X'262820205220202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303033323434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ermail-ridfld, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if not resp-normal
              move ws-response         to ra-status-code
              move ' ermail delete error '
                                       to ra-message
              display 'error-ermail-delete  ' ws-response
              go to 0000-return
           end-if
      ** 2)  Write new ERMAIL record
           move pa-new-eff-dt          to ma-cert-eff-dt
           move pa-new-cert-no         to ma-cert-no
           
      * exec cics write
      *       dataset  ('ERMAIL')
      *       from     (mailing-data)
      *       ridfld   (ma-control-primary)
      *       resp     (ws-response)
      *    end-exec
           MOVE LENGTH OF
            mailing-data
             TO DFHEIV11
           MOVE 'ERMAIL' TO DFHEIV1
      *    MOVE '&$ L                  ''  N#00003259' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303033323539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 mailing-data, 
                 DFHEIV11, 
                 ma-control-primary, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if not resp-normal
              move ws-response         to ra-status-code
              move ' ermail write  error '
                                       to ra-message
              display 'error-ermail-write   ' ws-response
              go to 0000-return
           end-if
           .
       0100-end-ermail.
           .
       0100-erendr.
      **** ======================================================== ****
      ****                                                          ****
      ****    Check for ERENDR records                              ****
      ****                                                          ****
      **** ======================================================== ****
           move ' '                    to ws-startbr-ind
           move ws-old-pndb-key(1:33)  to erendr-ridfld(1:33)
           move +0                     to erendr-seq-no
           move ' '                    to erendr-rec-type
           
      * exec cics startbr
      *       dataset    ('ERENDR')
      *       ridfld     (erendr-ridfld)
      *       gteq
      *       resp       (ws-response)
      *    end-exec
           MOVE 'ERENDR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00003285' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303033323835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 erendr-ridfld, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if RESP-NOTFND or resp-endfile
              go to 0100-end-erendr
           else
              if not resp-normal
                 move ws-response      to ra-status-code
                 move ' erendr error startbr '
                                       to ra-message
                 display 'error-erendr-startbr ' ws-response
                 go to 0000-return
              end-if
           end-if
           move 'Y'                    to ws-startbr-ind
           .
       0100-erendr-loop.
           
      * exec cics readnext
      *       dataset    ('ERENDR')
      *       ridfld     (erendr-ridfld)
      *       into       (endorsement-record)
      *       resp       (ws-response)
      *    end-exec
           MOVE LENGTH OF
            endorsement-record
             TO DFHEIV12
           MOVE 'ERENDR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00003305' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303033333035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 endorsement-record, 
                 DFHEIV12, 
                 erendr-ridfld, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if (RESP-NOTFND)
              or (resp-endfile)
              or (erendr-ridfld(1:33) <> ws-old-pndb-key(1:33))
              go to 0100-end-erendr
           else
              if not resp-normal
                 move ws-response      to ra-status-code
                 move ' erendr error readnext '
                                       to ra-message
                 display 'error-erendr-readnext ' ws-response
                 go to 0000-return
              end-if
           end-if
           if en-rec-type <> '1'  *>  Issue record
              go to 0100-erendr-loop
           end-if
      ***  delete and add with new key right here!!!
           
      * exec cics delete
      *       dataset  ('ERENDR')
      *       ridfld   (erendr-ridfld)
      *       resp     (ws-response)
      *    end-exec
           MOVE 'ERENDR' TO DFHEIV1
      *    MOVE '&(  R                 &  N#00003328' TO DFHEIV0
           MOVE X'262820205220202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303033333238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 erendr-ridfld, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if not resp-normal
              move ws-response         to ra-status-code
              move ' erendr delete error '
                                       to ra-message
              display 'error-erendr-delete  ' ws-response
              go to 0000-return
           end-if
           move pa-new-eff-dt          to en-cert-eff-dt
           move pa-new-cert-no         to en-cert-no
           
      * exec cics write
      *       dataset  ('ERENDR')
      *       from     (ENDORSEMENT-RECORD)
      *       ridfld   (en-control-primary)
      *       resp     (ws-response)
      *    end-exec
           MOVE LENGTH OF
            ENDORSEMENT-RECORD
             TO DFHEIV11
           MOVE 'ERENDR' TO DFHEIV1
      *    MOVE '&$ L                  ''  N#00003342' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303033333432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ENDORSEMENT-RECORD, 
                 DFHEIV11, 
                 en-control-primary, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if not resp-normal
              move ws-response         to ra-status-code
              move ' erendr write  error '
                                       to ra-message
              display 'error-erendr-write   ' ws-response
              go to 0000-return
           end-if
           go to 0100-erendr-loop
           .
       0100-end-erendr.
           if ws-startbr-ind = 'Y'
              
      * exec cics endbr
      *          dataset     ('ERENDR')
      *       end-exec
           MOVE 'ERENDR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00003359' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033333539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           end-if
           .
       0100-erendt.
      **** ======================================================== ****
      ****                                                          ****
      ****    Check for ERENDT records                              ****
      ****                                                          ****
      **** ======================================================== ****
           move ' '                    to ws-startbr-ind
           move ws-old-pndb-key(1:33)  to erendt-ridfld(1:33)
           move ' '                    to erendt-rec-type
           move +0                     to erendt-seq-no
           
      * exec cics startbr
      *       dataset    ('ERENDT')
      *       ridfld     (erendt-ridfld)
      *       gteq
      *       resp       (ws-response)
      *    end-exec
           MOVE 'ERENDT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00003374' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303033333734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 erendt-ridfld, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if RESP-NOTFND or resp-endfile
              go to 0100-end-erendt
           else
              if not resp-normal
                 move ws-response      to ra-status-code
                 move ' erendt error startbr '
                                       to ra-message
                 display 'error-erendt-startbr ' ws-response
                 go to 0000-return
              end-if
           end-if
           move 'Y'                    to ws-startbr-ind
           .
       0100-erendt-loop.
           
      * exec cics readnext
      *       dataset    ('ERENDT')
      *       ridfld     (erendt-ridfld)
      *       into       (endt-record)
      *       resp       (ws-response)
      *    end-exec
           MOVE LENGTH OF
            endt-record
             TO DFHEIV12
           MOVE 'ERENDT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00003394' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303033333934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 endt-record, 
                 DFHEIV12, 
                 erendt-ridfld, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if (RESP-NOTFND)
              or (resp-endfile)
              or (erendt-ridfld(1:33) <> ws-old-pndb-key(1:33))
              go to 0100-end-erendt
           else
              if not resp-normal
                 move ws-response      to ra-status-code
                 move ' erendt error readnext '
                                       to ra-message
                 display 'error-erendt-readnext ' ws-response
                 go to 0000-return
              end-if
           end-if
           if et-rec-type <> 'I'  *>  Issue record
              go to 0100-erendt-loop
           end-if
      ***  delete and add with new key right here!!!
           
      * exec cics delete
      *       dataset  ('ERENDT')
      *       ridfld   (erendt-ridfld)
      *       resp     (ws-response)
      *    end-exec
           MOVE 'ERENDT' TO DFHEIV1
      *    MOVE '&(  R                 &  N#00003417' TO DFHEIV0
           MOVE X'262820205220202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303033343137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 erendt-ridfld, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if not resp-normal
              move ws-response         to ra-status-code
              move ' erendt delete error '
                                       to ra-message
              display 'error-erendt-delete  ' ws-response
              go to 0000-return
           end-if
           move pa-new-eff-dt          to et-cert-eff-dt
           move pa-new-cert-no         to et-cert-no
           
      * exec cics write
      *       dataset  ('ERENDT')
      *       from     (ENDT-RECORD)
      *       ridfld   (et-control-primary)
      *       resp     (ws-response)
      *    end-exec
           MOVE LENGTH OF
            ENDT-RECORD
             TO DFHEIV11
           MOVE 'ERENDT' TO DFHEIV1
      *    MOVE '&$ L                  ''  N#00003431' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303033343331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ENDT-RECORD, 
                 DFHEIV11, 
                 et-control-primary, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if not resp-normal
              move ws-response         to ra-status-code
              move ' erendt write  error '
                                       to ra-message
              display 'error-erendt-write   ' ws-response
              go to 0000-return
           end-if
           go to 0100-erendt-loop
           .
       0100-end-erendt.
           if ws-startbr-ind = 'Y'
              
      * exec cics endbr
      *          dataset     ('ERENDT')
      *       end-exec
           MOVE 'ERENDT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00003448' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033343438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           end-if
           .
       0100-exit.
           exit.
       0200-convert-key.
           .
       0200-exit.
           exit.
       0500-process-nsasextr.
      **** ======================================================== ****
      ****                                                          ****
      ****    Check for NSASEXTR records                            ****
      ****                                                          ****
      **** ======================================================== ****
           move ' '                    to ws-NS-startbr-ind
           move +0                     to nsasextr-seq-no
           
      * exec cics startbr
      *       dataset    ('NSASEXTR')
      *       ridfld     (nsasextr-ridfld)
      *       gteq
      *       resp       (ws-response)
      *    end-exec
           MOVE 'NSASEXTR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00003467' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303033343637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 nsasextr-ridfld, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if RESP-NOTFND or resp-endfile
              go to 0500-end-nsasextr
           else
              if not resp-normal
                 move ws-response      to ra-status-code
                 move ' nsasextr error startbr '
                                       to ra-message
                 display 'error-nsasextr-startbr ' ws-response
                 go to 0000-return
              end-if
           end-if
           move 'Y'                    to ws-NS-startbr-ind
           .
       0500-nsasextr-loop.
           
      * exec cics readnext
      *       dataset    ('NSASEXTR')
      *       ridfld     (nsasextr-ridfld)
      *       into       (nsas-extract-record)
      *       resp       (ws-response)
      *    end-exec
           MOVE LENGTH OF
            nsas-extract-record
             TO DFHEIV12
           MOVE 'NSASEXTR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00003487' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303033343837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 nsas-extract-record, 
                 DFHEIV12, 
                 nsasextr-ridfld, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if (RESP-NOTFND)
              or (resp-endfile)
              or (nsas-company-cd <> opk-company-cd)
              or (nsas-archive-no <> la-archive-no)
              go to 0500-end-nsasextr
           else
              if not resp-normal
                 move ws-response      to ra-status-code
                 move ' nsasextr error readnext '
                                       to ra-message
                 display 'error-nsasextr-readnext ' ws-response
                 go to 0000-return
              end-if
           end-if
           move nsas-control-primary   to nsasextr-ridfld
           
      * exec cics read update
      *       dataset  ('NSASEXTR')
      *       into     (nsas-extract-record)
      *       ridfld   (nsasextr-ridfld)
      *       resp     (ws-response)
      *    end-exec
           MOVE LENGTH OF
            nsas-extract-record
             TO DFHEIV11
           MOVE 'NSASEXTR' TO DFHEIV1
      *    MOVE '&"IL       EU         (  N#00003508' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303033353038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 nsas-extract-record, 
                 DFHEIV11, 
                 nsasextr-ridfld, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if not resp-normal
              move ws-response         to ra-status-code
              move ' nsasextr read update error '
                                       to ra-message
              display 'error-nsasextr-read update ' ws-response
              go to 0000-return
           end-if
           move zeros                  to nscntr
           perform varying ns1 from +1 by +1 until
              nscntr = +10
              if nsas-letter-variables(ns1:1) = '~'
                 compute nscntr = nscntr + 1
              end-if
           end-perform
           if nscntr = +10
              if (nsas-letter-variables(ns1:10) =
                    ws-old-greg-eff-date)
                 and (nsas-letter-variables(ns1:10) <>
                    ws-new-greg-eff-date)
                 move ws-new-greg-eff-date
                                       to nsas-letter-variables(ns1:10)
              end-if
              perform varying ns1 from ns1 by +1
                 until (nsas-letter-variables(ns1:1) = '~')
                    or (ns1 > +4000)
              end-perform
              compute ns1 = ns1 + 1
              if ns1 < +4000
                 if (nsas-letter-variables(ns1:11) =
                       opk-cert-no)
                    and (nsas-letter-variables(ns1:11) <>
                       pa-new-cert-no)
                    move pa-new-cert-no to
                       nsas-letter-variables(ns1:11)
                 end-if
              end-if
           end-if
           
      * exec cics rewrite
      *       dataset  ('NSASEXTR')
      *       from     (nsas-extract-record)
      *       resp     (ws-response)
      *    end-exec
           MOVE LENGTH OF
            nsas-extract-record
             TO DFHEIV11
           MOVE 'NSASEXTR' TO DFHEIV1
      *    MOVE '&& L                  %  N#00003551' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'204E233030303033353531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 nsas-extract-record, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if not resp-normal
              move ws-response         to ra-status-code
              move ' nsasextr write  error '
                                       to ra-message
              display 'error-nsasextr-write   ' ws-response
              go to 0000-return
           end-if
           go to 0500-nsasextr-loop
           .
       0500-end-nsasextr.
           if ws-NS-startbr-ind = 'Y'
              
      * exec cics endbr
      *          dataset     ('NSASEXTR')
      *       end-exec
           MOVE 'NSASEXTR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00003567' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           end-if
           .
       0500-exit.
           exit.
       8500-DATE-CONVERT.
           
      * EXEC CICS LINK
      *        PROGRAM  ('ELDATCV')
      *        COMMAREA (DATE-CONVERSION-DATA)
      *        LENGTH   (DC-COMM-LENGTH)
      *    END-EXEC
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00003575' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       8500-EXIT.
           EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL055' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL055' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
