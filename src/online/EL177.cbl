       IDENTIFICATION DIVISION.
LGC007 PROGRAM-ID.                 EL177.
      *                            VMOD=1.7.
       AUTHOR.    LOGIC, INC.
                  DALLAS, TEXAS.
       DATE-WRITTEN.  AUGUST, 1981.
CIDMOD*SECURITY.   *****************************************************
CIDMOD*            **                                                  *
CIDMOD*            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
CIDMOD*            *                                                   *
CIDMOD*            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
CIDMOD*            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
CIDMOD*            *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *
CIDMOD*            *                                                   *
CIDMOD*            *****************************************************
CIDMOD*
CIDMOD*
CIDMOD*REMARKS.
092602******************************************************************
092602*                   C H A N G E   L O G
092602*
092602* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
092602*-----------------------------------------------------------------
092602*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
092602* EFFECTIVE    NUMBER
092602*-----------------------------------------------------------------
LGC007****************************************************************
LGC007*****     THIS PROGRAM WAS DIRECTLY COPIED FROM CL177.     *****
LGC007*****     ALL CHANGES HAVE BEEN FLAGGED WITH LGC007 AT     *****
LGC007*****     THE FRONT OF THE STATEMENT.                      *****
LGC007****************************************************************
CSOCAS* CASB  02/02/94 - ADDED CODE SO THAT WHEN A DRAFT AMOUNT IS
CSOCAS*                  OVER $10,000.00 IT WILL NOT PRINT BILL
CSOCAS*                  KIZER'S SIGNATURE. THIS IS ACCOMPLISHED
CSOCAS*                  MY PLACING A VALUE OF 'C' IN THE
CSOCAS*                  M420C-SIGNATURE FIELD.
LGC121* JWBA  03/02/94 - CHG  CODE SO THAT IF CID DRAFT AMOUNT IS
LGC121*                  OVER $100,000.00 THEN THE CHECK MUST BE
LGC121*                  SIGNED BY W.M. KIZER.
CSODJN* DJNA  04/01/00 - CR#2000030100009 DRAFT NUMBER EXPANSION.
CSODAN* DANA  05/12/00 - CR#2000021500004 NEW MESSAGES ON CHECK
102902* 102902    2001061800003  PEMA  ADD DCC TO MICR PROCESSING
121903* 121903    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYP I
121903*           2003080800002  SMVA  ADD PROCESSING FOR NEW CLM TYP G
121903*           2002100700001  SMVA  ADD ACCT STATE AND CLM TYPE FOR P
121903*                                STATE SPEC PROGRESS RPTS
011105* 011105    2004071200002  PEMA  ADD ONE COMMENT LINE
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
080322* 080322  CR2021100800003  TANA  Add B and H claim types
121903****************************************************************
CIDMOD*        THIS FUNCTION READS THE CHECK QUEUE FILE AND PRINTS
CIDMOD*    CHECKS FROM THOSE ENTRIES QUEUED.  QUEUEING IS DONE USING
LGC007*    TIME (HH.MM) BY THE CHECK PRINT RELEASE PROGRAM (EL176).
CIDMOD*
CIDMOD*    SCREENS     - NONE - USERS PRINTED OUTPUT (CHECKS)
CIDMOD*
LGC007*    ENTERED BY  - EL176  - CHECK WRITER - VIA START
CIDMOD*
CIDMOD*    EXIT TO     - CICS
CIDMOD*
CIDMOD*    INPUT FILES - NONE
CIDMOD*
CIDMOD*    OUTPUT FILES - NONE
CIDMOD*
CIDMOD*    COMMAREA    - PASSED.
CIDMOD*
           EJECT
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
       77  FILLER       PIC X(32)  VALUE '**************************'.
LGC007 77  FILLER       PIC X(32)  VALUE '*   EL177 WORKING STORAGE '.
       77  FILLER       PIC X(32)  VALUE '*********** V/M 1.7 ******'.
LGC120*
LGC120 77  PI-PRINT-REPORT     PIC X(08)  VALUE SPACES.
LGC120 77  PI-PRINT-ID         PIC X(04)  VALUE SPACES.
CIDMOD 77  WS-FIRST-TIME-SWA   PIC X VALUE 'Y'.
CIDMOD 77  WS-FIRST-TIME-SWB   PIC X VALUE 'Y'.
LGC120
LGC007
LGC007 01  WS-COMMON-PRINT.
LGC007     05  THIS-PGM                   PIC X(8)   VALUE 'EL177   '.
LGC007     05  WS-BLANK                   PIC X      VALUE ' '.
LGC007     05  WS-I                       PIC X      VALUE 'I'.
LGC007     05  WS-F                       PIC X      VALUE 'F'.
LGC007     05  WS-R                       PIC X      VALUE 'R'.
LGC007     05  WS-L                       PIC X      VALUE 'L'.
       01  WS-FINAL-MESS-CONSTANT.
           05  WS-LINE-9                  PIC X(17)  VALUE
               'IF YOU HAVE ANY  '.
           05  WS-LINE-10                 PIC X(17)  VALUE
               'QUESTIONS, PLEASE'.
           05  WS-LINE-11                 PIC X(17)  VALUE
               'LET US KNOW.     '.
           05  WS-TP-RECORD-AREA.
               10  WS-TP-ACCOUNT-NO        PIC X(6).
               10  WS-TP-REFENCE-NO        PIC X(6).
           05  WS-TP-ADDRESS-AREA.
               10  WS-TP-REFNCE-NO         PIC X(6).
               10  WS-TP-ADD-LINE1         PIC X(40).
               10  WS-TP-ADD-LINE2         PIC X(40).
               10  WS-TP-ADD-LINE3         PIC X(40).
       01  FILLER                          COMPUTATIONAL-3.
           05  WS-RECORD-COUNT             PICTURE S9(5)   VALUE ZERO.
           05  WS-TIME-WORK                PICTURE S9(7)   VALUE ZERO.
           05  WS-TIME                     REDEFINES
               WS-TIME-WORK                PICTURE S9(3)V9(4).
           05  WS-HHMM                     REDEFINES
               WS-TIME-WORK                PICTURE S9(5)V99.
           05  WS-DELAY-INTERVAL           PICTURE S9(7)   VALUE +10.
           05  WS-DATA-SENT-SW             PICTURE S9      VALUE ZERO.
           05  WS-SW                       PICTURE S9      VALUE ZERO.
       01  FILLER                          COMPUTATIONAL
                                           SYNCHRONIZED.
LGC007     05  WS-TS-LENGTH                PICTURE S9(4)   VALUE +1158.
LGC007     05  WS-COMM-LENGTH              PICTURE S9(4)   VALUE +1024.
           05  WS-CHECK-LINES-LENGTH       PICTURE S9(4)   VALUE +904.
           05  WS-FIA-CHECK-LINES-LENGTH   PICTURE S9(4)   VALUE +1344.
           05  WS-CSO-CHECK-LINES-LENGTH   PICTURE S9(4)   VALUE +855.
           05  WS-OLI-CHECK-LINES-LENGTH   PICTURE S9(4)   VALUE +1230.
           05  WS-POS-CHECK-LINES-LENGTH   PICTURE S9(4)   VALUE +1163.
           05  WS-TCL-CHECK-LINES-LENGTH   PICTURE S9(4)   VALUE +666.
       01  CSO-PRINT-STARTED-SW        PIC X VALUE 'Y'.
       01  WS-SOC-SEC-NO               PICTURE 9(18)   VALUE ZEROS.
       01  WS-CSO-VOID-LINE-1          PICTURE X(11)
           VALUE '** VOID ** '.
       01  WS-CSO-VOID-LINE-2          PICTURE X(30)
           VALUE '** VOID **** VOID **** VOID **'.
LGC101 01  WS-DRAFT-ORDER              PIC 9(5)        VALUE ZEROS.
       01  WS-PLAN-CODE-AREA.
           05  WS-IND-GRP-TYPE         PICTURE X       VALUE SPACE.
           05  WS-BENEFIT-TYPE         PICTURE 99      VALUE ZEROS.
       01  WS-CHECK-AREA.
           05  COMPANY-CHECK           PICTURE X(2)    VALUE SPACE.
           05  FILLER                  PICTURE X(6)    VALUE SPACE.
       01  WS-COMPANY-NAME             PICTURE X(43)   VALUE SPACE.
       01  WS-COMPANY-NAME2            PICTURE X(43)   VALUE SPACE.
       01  CSO-COMPANY-HEADINGS.
           05  01-HEADING              PICTURE X(43)
LGC101         VALUE ' CENTRAL STATES HEALTH & LIFE CO. OF OMAHA '.
           05  02-HEADING              PICTURE X(43)
               VALUE '   CENTRAL STATES INDEMNITY CO. OF OMAHA   '.
           05  03-HEADING              PICTURE X(43)
               VALUE '        NATIONAL INDEMNITY COMPANY         '.
           05  04-HEADING              PICTURE X(43)
               VALUE ' MASSACHUSETTS INDEMNITY AND LIFE COMPANY  '.
           05  05-HEADING              PICTURE X(43)
               VALUE '        COLUMBIA INSURANCE COMPANY         '.
CSO685     05  06-HEADING              PICTURE X(43)
CSO685         VALUE '  NATIONAL FIRE AND MARINE INSURANCE CO.   '.
       01  COMPANY-NAMES.
           05  01-COMP-NAME            PICTURE X(43)
LGC101         VALUE ' CENTRAL STATES HEALTH & LIFE CO. OF OMAHA '.
           05  02-COMP-NAME            PICTURE X(43)
               VALUE 'CENTRAL STATES INDEMNITY CO. OF OMAHA      '.
           05  03-COMP-NAME            PICTURE X(43)
               VALUE 'NATIONAL INDEMNITY COMPANY                 '.
           05  04-COMP-NAME            PICTURE X(43)
               VALUE 'MASSACHUSETTS INDEMNITY AND LIFE COMPANY   '.
           05  05-COMP-NAME            PICTURE X(43)
               VALUE 'COLUMBIA INSURANCE COMPANY                 '.
CSO685     05  06-COMP-NAME            PICTURE X(43)
CSO685         VALUE 'NATIONAL FIRE AND MARINE INSURANCE CO.     '.
       01  FILLER.
           05  S-MSG                       PICTURE X(37)  VALUE
                    'FULL SETTLEMENT OF ANY AND ALL CLAIMS'.
           05  M-MSG                       PICTURE X(37)  VALUE
                    '  MEDICAL EXPENSE                    '.
           05  L-MSG                       PICTURE X(37)  VALUE
                    '  LEGAL EXPENSE                      '.
           05  I-MSG                       PICTURE X(37)  VALUE
                    '  INVESTIGATION EXPENSE              '.
           05  WS-BIN-CURRENT-DT           PICTURE XX     VALUE SPACES.
           05  WS-EDT-CURRENT-DT           PICTURE X(8)   VALUE SPACES.
LGC007     05  WS-PROGRAM-ID               PICTURE X(8)   VALUE 'EL177'.
           05  WS-TEXT-MESSAGE-LENGTH      PICTURE S9(4)   VALUE +70
                                           COMPUTATIONAL
                                           SYNCHRONIZED.
           05  WS-TEXT-MESSAGE             PICTURE X(70)   VALUE SPACES.
           05  WS-ZIP-CODE-LINE                            VALUE SPACES.
               10  WS-ZIP-CHAR             PICTURE X
                   OCCURS 133 TIMES        INDEXED BY ZIP-INDEX1
                                                      ZIP-INDEX2
                                                      ZIP-INDEX3.
           05  WS-DUMP-CODE.
               10  FILLER                  PICTURE X       VALUE 'S'.
               10  WS-DUMP-COUNT           PICTURE 999     VALUE ZERO.
LGC007     05  WS-CPA-COMMENT.
LGC007         10  WS-CPA-COMMENT-P1       PIC X(29).
LGC007         10  WS-CPA-VOID-INDICATOR   PIC X.
           EJECT
CSODAN 01  420C-MSG1.
           05  PIC X(35) VALUE 'A payment for benefits was made to '.
           05  PIC X(35) VALUE 'your account today.  Since more    '.
           05  PIC X(35) VALUE 'benefits may be payable in the futu'.
           05  PIC X(35) VALUE 're, we are enclosing another claim '.
           05  PIC X(35) VALUE 'form.  This form must be completed '.
           05  PIC X(35) VALUE 'IN FULL and sent to us when you    '.
           05  PIC X(35) VALUE 'return to work or on       whicheve'.
           05  PIC X(35) VALUE 'r is sooner.  Thank you for your   '.
           05  PIC X(35) VALUE 'cooperation.                       '.
           05  PIC X(35) VALUE '                                   '.
       01  REDEFINES 420C-MSG1.
           05  420C-MSG1-LINE OCCURS 5 TIMES PIC X(70).
       01  420C-MSG2.
           05  PIC X(35) VALUE 'This is the last benefit payment fo'.
           05  PIC X(35) VALUE 'r this period of disability under  '.
           05  PIC X(35) VALUE 'this claim on your credit insurance'.
           05  PIC X(35) VALUE ' policy.  Please check with your   '.
           05  PIC X(35) VALUE 'financial institution within the ne'.
           05  PIC X(35) VALUE 'xt week to make sure it was        '.
           05  PIC X(35) VALUE 'credited to your account.          '.
           05  PIC X(35) VALUE '                                   '.
       01  REDEFINES 420C-MSG2.
           05  420C-MSG2-LINE OCCURS 4 TIMES PIC X(70).
       01  420C-MSG3.
           05  PIC X(35) VALUE 'A claim payment was made on your ac'.
           05  PIC X(35) VALUE 'count today.  Please check with    '.
           05  PIC X(35) VALUE 'your financial institution within t'.
           05  PIC X(35) VALUE 'he next week to make sure it was   '.
           05  PIC X(35) VALUE 'credited to your account.          '.
           05  PIC X(35) VALUE '                                   '.
       01  REDEFINES 420C-MSG3.
           05  420C-MSG3-LINE OCCURS 3 TIMES PIC X(70).
       01  420C-MSG4.
           05  PIC X(35) VALUE 'This payment represents the total b'.
           05  PIC X(35) VALUE 'enefits payable under this credit  '.
           05  PIC X(35) VALUE 'life insurance policy.  This claim '.
           05  PIC X(35) VALUE 'has been closed.                   '.
       01  REDEFINES 420C-MSG4.
           05  420C-MSG4-LINE OCCURS 2 TIMES PIC X(70).
       01  420C-MSG5.
           05  PIC X(35) VALUE 'We are sorry for your loss.  Becaus'.
           05  PIC X(35) VALUE 'e credit life insurance was        '.
           05  PIC X(35) VALUE 'purchased to protect this loan, we '.
           05  PIC X(35) VALUE 'have paid the loan off at the      '.
           05  PIC X(35) VALUE 'financial institution.  This paymen'.
           05  PIC X(35) VALUE 't to the estate represents the     '.
           05  PIC X(35) VALUE 'remaining benefit amount available '.
           05  PIC X(35) VALUE 'under this policy.  This claim has '.
           05  PIC X(35) VALUE 'been closed.                       '.
           05  PIC X(35) VALUE '                                   '.
       01  REDEFINES 420C-MSG5.
CSODAN     05  420C-MSG5-LINE OCCURS 5 TIMES PIC X(70).
011105 01  DCC1-MSG1.
011105     05  PIC X(35) VALUE 'A payment was made to your account '.
011105     05  PIC X(35) VALUE 'today.  Please check with your     '.
011105     05  PIC X(35) VALUE 'financial institution within the ne'.
011105     05  PIC X(35) VALUE 'xt week to make sure it was        '.
011105     05  PIC X(35) VALUE 'credited to your account.  Since mo'.
011105     05  PIC X(35) VALUE 're benefits may be payable in the  '.
011105     05  PIC X(35) VALUE 'future, we are enclosing another fo'.
011105     05  PIC X(35) VALUE 'rm.  This form must be completed   '.
011105     05  PIC X(35) VALUE 'IN FULL and sent to us when you ret'.
011105     05  PIC X(35) VALUE 'urn to work or on       whichever  '.
011105     05  PIC X(35) VALUE 'is sooner.  Thank you for your coop'.
011105     05  PIC X(35) VALUE 'eration.                           '.
011105 01  REDEFINES DCC1-MSG1.
011105     05  DCC1-MSG1-LINE OCCURS 6 TIMES PIC X(70).
011105 01  DCC1-MSG2.
011105     05  PIC X(35) VALUE 'This is the last benefit payment fo'.
011105     05  PIC X(35) VALUE 'r this period of disability under  '.
011105     05  PIC X(35) VALUE 'this file on your debt protection a'.
011105     05  PIC X(35) VALUE 'ddendum.  Please check with your   '.
011105     05  PIC X(35) VALUE 'financial institution within the ne'.
011105     05  PIC X(35) VALUE 'xt week to make sure it was        '.
011105     05  PIC X(35) VALUE 'credited to your account properly. '.
011105     05  PIC X(35) VALUE '                                   '.
011105 01  REDEFINES DCC1-MSG2.
011105     05  DCC1-MSG2-LINE OCCURS 4 TIMES PIC X(70).
011105 01  DCC1-MSG3.
011105     05  PIC X(35) VALUE 'A payment was made on your account '.
011105     05  PIC X(35) VALUE 'today. Please check with your      '.
011105     05  PIC X(35) VALUE 'financial institution within the ne'.
011105     05  PIC X(35) VALUE 'xt week to make sure it was        '.
011105     05  PIC X(35) VALUE 'credited to your account properly. '.
011105     05  PIC X(35) VALUE '                                   '.
011105 01  REDEFINES DCC1-MSG3.
011105     05  DCC1-MSG3-LINE OCCURS 3 TIMES PIC X(70).
011105 01  DCC1-MSG4.
011105     05  PIC X(35) VALUE 'This payment represents the total b'.
011105     05  PIC X(35) VALUE 'enefits payable under your debt    '.
011105     05  PIC X(35) VALUE 'protection addendum.  This file has'.
011105     05  PIC X(35) VALUE ' been closed.                      '.
011105 01  REDEFINES DCC1-MSG4.
011105     05  DCC1-MSG4-LINE OCCURS 2 TIMES PIC X(70).
011105 01  DCC1-MSG5.
011105     05  PIC X(35) VALUE 'We are sorry for your loss.  Becaus'.
011105     05  PIC X(35) VALUE 'e debt protection coverage was     '.
011105     05  PIC X(35) VALUE 'purchased to protect this loan, we '.
011105     05  PIC X(35) VALUE 'have made a payment to the         '.
011105     05  PIC X(35) VALUE 'financial institution.  The file ha'.
011105     05  PIC X(35) VALUE 'been closed.                       '.
011105 01  REDEFINES DCC1-MSG5.
011105     05  DCC1-MSG5-LINE OCCURS 3 TIMES PIC X(70).
           EJECT
LGC007*                            COPY ELCINTF.
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
LGC007*           COPY ELC176PI.
00001 *****************************************************************
00002 *                                                               *
00002 *                                                               *
00003 *                            ELC176PI.                          *
00004 *                            VMOD=2.005                         *
00005 *****************************************************************.
00006
00007      12  FILLER                      REDEFINES
00008          PI-PROGRAM-WORK-AREA.
00009
00010          16  PI-TEMP-STORAGE-KEY.
00011              20  PI-TSK-TERM-ID      PIC X(4).
00012              20  PI-TSK-TIME         PIC S9(7)     COMP-3.
00013
00014          16  PI-PROCESSING-SW        PIC S9        COMP-3.
00015
00016          16  PI-NUMBER-OF-ALIGNMENT-CHECKS
00017                                      PIC S9        COMP-3.
00018          16  PI-ALIGNMENT-CONTROL-GROUP
00019                                      PIC S9(8)     COMP-3.
00020          16  PI-ALIGNMENT-SEQUENCE-NO
00021                                      PIC S9(4)     COMP-3.
00022          16  PI-NUMBER-OF-CONTROL-GROUPS
00023                                      PIC S9(4)     COMP-3.
00024
00025          16  PI-CONTROL-GROUPS                     COMP
00026              OCCURS 4 TIMES          INDEXED BY PI-INDEX.
00027              20  PI-CONTROL-GROUP    PIC S9(8).
00028              20  PI-HIGH-SEQUENCE    PIC S9(4).
00029
00030          16  PI-CHECK-PRINTER-ID     PIC X(4).
00031
00032          16  PI-PRINTER-STARTED-SW   PIC S9        COMP-3.
00033
00034          16  PI-ASSIGN-CHECK-NUMBERS PIC X.
00035
00036          16  PI-COMPANY-ADDRESS.
00037              20  PI-COMPANY-NAME             PIC X(30).
00038              20  PI-COMPANY-ADDRESS-LINE1    PIC X(30).
00039              20  PI-COMPANY-ADDRESS-LINE2    PIC X(30).
00040              20  PI-COMPANY-ADDRESS-LINE3    PIC X(30).
00041              20  PI-COMPANY-CITY-ST          PIC X(30).
00042              20  PI-COMPANY-ZIP.
00043                  24  PI-COMPANY-ZIP-CODE.
00044                      28  PI-COMPANY-ZIP-1ST  PIC X.
00045                          88  PI-COMPANY-CAN-POST-CODE
00046                                              VALUE 'A' THRU 'Z'.
00047                      28  FILLER              PIC X(4).
00048                  24  PI-COMPANY-ZIP-PLUS4    PIC X(4).
00049              20  PI-CO-CANADIAN-POSTAL-CODE
00050                      REDEFINES PI-COMPANY-ZIP.
00051                  24  PI-CO-CAN-POSTAL-1      PIC XXX.
00052                  24  PI-CO-CAN-POSTAL-2      PIC XXX.
00053                  24  FILLER                  PIC XXX.
00054              20  PI-COMPANY-PHONE-NUMBER     PIC S9(11) COMP-3.
00055
00056          16  PI-MONTH-END-SAVE               PIC XX.
00057
00058          16  PI-VALID-RCD-SW                 PIC X.
00059
00060          16  FILLER                          PIC X(419).
00061
               16  PI-TEMP-STORAGE-ITEM    PICTURE S9(4)
                                           COMPUTATIONAL
                                           SYNCHRONIZED.
           EJECT
LGC007*                           COPY ELC176W2.
00001 *****************************************************************
00002 *                                                               *
00003 *                            ELC176W2.                          *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                         *
00006 *****************************************************************.
00007
00008  01  SPELL-DOLLAR-PASS-AREA.
00009
00010      05  SD-PASS-AMOUNT              PIC S9(9)V99
00011                                      COMP-3        VALUE ZEROS.
00012
00013      05  SD-PASS-SPELLED-AMOUNT      PIC X(200)    VALUE SPACES.
00014
00015      05  FILLER.
00016          10  SD-PSA-LINE1            PIC X(100)    VALUE SPACES.
00017          10  SD-PSA-LINE2            PIC X(100)    VALUE SPACES.
00018
           EJECT
LGC007*                           COPY ELC176W1.
00001 *****************************************************************
00002 *                                                               *
00003 *                            ELC176W1.                          *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.004                         *
00006 *****************************************************************.
00007
00008  01  FILLER.
00009      05  WS-1ST-LINE-LENGTH          PIC S9(4)   VALUE +77
00010                                      COMP
00011                                      SYNC.
00012
00013      05  WS-1ST-LINE-LENGTH-PLUS-1   PIC S9(4)   VALUE +78
00014                                      COMP
00015                                      SYNC.
00016
00017      05  WS-1ST-LINE-LENGTH-PLUS-2   PIC S9(4)   VALUE +79
00018                                      COMP
00019                                      SYNC.
00020
00021      05  WS-1ST-LINE-LENGTH-MINUS-1  PIC S9(4)   VALUE +76
00022                                      COMP
00023                                      SYNC.
00024
00025      05  WS-2ND-LINE-LENGTH          PIC S9(4)   VALUE +48
00026                                      COMP
00027                                      SYNC.
00028
00029      05  WS-AMOUNT                   PIC 9(9)V99 VALUE ZEROS.
00030
00031      05  FILLER                      REDEFINES
00032          WS-AMOUNT.
00033          10  WS-MILLIONS             PIC 9(3).
00034          10  WS-THOUSANDS            PIC 9(3).
00035          10  WS-HUNDREDS             PIC 9(3).
00036
00037          10  WS-CENTS                PIC 99.
00038          10  WS-CENTS-X              REDEFINES
00039              WS-CENTS                PIC XX.
00040
00041      05  WS-AMOUNT-WORK              PIC 9(3)    VALUE ZEROS.
00042
00043      05  FILLER                      REDEFINES
00044          WS-AMOUNT-WORK.
00045          10  WS-HUNDRED              PIC 9.
00046          10  WS-TEEN                 PIC 99.
00047
00048          10  FILLER                  REDEFINES
00049              WS-TEEN.
00050
00051              15  WS-TEN              PIC 9.
00052              15  WS-ONE              PIC 9.
00053
00054      05  WS-SPELLED-AMOUNT           PIC X(200)  VALUE SPACES.
00055
00056      05  WS-CHAR                     REDEFINES
00057          WS-SPELLED-AMOUNT           PIC X
00058          OCCURS 200 TIMES            INDEXED BY SA-INDEX
00059                                                 SA-INDEX2.
00060
00061      05  WS-SPELLED-LINE1            PIC X(100)  VALUE SPACES.
00062
00063      05  WS-SL1                      REDEFINES
00064          WS-SPELLED-LINE1            PIC X
00065          OCCURS 100 TIMES            INDEXED BY SL1-INDEX.
00066
00067      05  WS-SPELLED-LINE2            PIC X(100)  VALUE SPACES.
00068
00069      05  WS-SL2                      REDEFINES
00070          WS-SPELLED-LINE2            PIC X
00071          OCCURS 100 TIMES            INDEXED BY SL2-INDEX.
00072
00073      05  WS-WORD                     PIC X(21)   VALUE SPACES.
00074
00075      05  WS-CHAR2                    REDEFINES
00076          WS-WORD                     PIC X
00077          OCCURS 21 TIMES             INDEXED BY CHAR-INDEX.
00078
00079      EJECT
00080      05  WS-SINGLE-AREA.
00081          10  FILLER    PIC X(21)  VALUE 'ONE'.
00082          10  FILLER    PIC X(21)  VALUE 'TWO'.
00083          10  FILLER    PIC X(21)  VALUE 'THREE'.
00084          10  FILLER    PIC X(21)  VALUE 'FOUR'.
00085          10  FILLER    PIC X(21)  VALUE 'FIVE'.
00086          10  FILLER    PIC X(21)  VALUE 'SIX'.
00087          10  FILLER    PIC X(21)  VALUE 'SEVEN'.
00088          10  FILLER    PIC X(21)  VALUE 'EIGHT'.
00089          10  FILLER    PIC X(21)  VALUE 'NINE'.
00090          10  FILLER    PIC X(21)  VALUE 'TEN'.
00091          10  FILLER    PIC X(21)  VALUE 'ELEVEN'.
00092          10  FILLER    PIC X(21)  VALUE 'TWELVE'.
00093          10  FILLER    PIC X(21)  VALUE 'THIRTEEN'.
00094          10  FILLER    PIC X(21)  VALUE 'FOURTEEN'.
00095          10  FILLER    PIC X(21)  VALUE 'FIFTEEN'.
00096          10  FILLER    PIC X(21)  VALUE 'SIXTEEN'.
00097          10  FILLER    PIC X(21)  VALUE 'SEVENTEEN'.
00098          10  FILLER    PIC X(21)  VALUE 'EIGHTEEN'.
00099          10  FILLER    PIC X(21)  VALUE 'NINETEEN'.
00100
00101      05  WS-SINGLE-DESC              REDEFINES
00102          WS-SINGLE-AREA              PIC X(21)
00103          OCCURS 19 TIMES             INDEXED BY SINGLE-INDEX.
00104
00105      05  WS-UPPER-AREA.
00106          10  FILLER    PIC X(21)  VALUE SPACES.
00107          10  FILLER    PIC X(21)  VALUE 'TWENTY'.
00108          10  FILLER    PIC X(21)  VALUE 'THIRTY'.
00109          10  FILLER    PIC X(21)  VALUE 'FORTY'.
00110          10  FILLER    PIC X(21)  VALUE 'FIFTY'.
00111          10  FILLER    PIC X(21)  VALUE 'SIXTY'.
00112          10  FILLER    PIC X(21)  VALUE 'SEVENTY'.
00113          10  FILLER    PIC X(21)  VALUE 'EIGHTY'.
00114          10  FILLER    PIC X(21)  VALUE 'NINETY'.
00115
00116      05  WS-UPPER-DESC               REDEFINES
00117          WS-UPPER-AREA               PIC X(21)
00118          OCCURS 9 TIMES              INDEXED BY UPPER-INDEX.
00119
00120      05  WS-DOLLARS-AND-CENTS        PIC X(21)   VALUE
00121          'DOLLARS-AND-XX-CENTS'.
00122
00123      05  FILLER                      REDEFINES
00124          WS-DOLLARS-AND-CENTS.
00125
00126          10  FILLER                  PIC X(12).
00127          10  WS-PENNEYS              PIC XX.
00128          10  FILLER                  PIC X(07).
00129
           EJECT
LGC007*                           COPY ELC176W3.
00001 *****************************************************************
00002 *                                                               *
00003 *                            ELC176W3.                          *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                         *
00006 *****************************************************************.
00007
00008  01  CHECK-PRINT-LINES.
00009
00010      05  CHECK-PRINT-LINE-1.
00011          10  FILLER                     PIC X       VALUE '1'.
00012
00013      05  CHECK-PRINT-LINE-3.
00014          10  CPL3-CARRIAGE-CONTROL      PIC X       VALUE '-'.
00015          10  FILLER                     PIC X(50)   VALUE SPACES.
00016          10  CPL3-CHECK-NUMBER-DESC     PIC X(10)
00017                                             VALUE 'CHECK NO.'.
00018          10  CPL3-CHECK-NUMBER          PIC X(7)    VALUE ALL 'X'.
00019
00020      05  CHECK-PRINT-LINE-6.
00021          10  FILLER                     PIC X       VALUE '-'.
00022
00023      05  CHECK-PRINT-LINE-9.
00024          10  FILLER                     PIC X(9)    VALUE '-'.
00025          10  CPL9-SPELLED-AMOUNT-LINE1  PIC X(77)   VALUE ALL 'X'.
00026
00027      05  CHECK-PRINT-LINE-10.
00028          10  FILLER                     PIC X(9)    VALUE SPACES.
00029          10  CPL10-SPELLED-AMOUNT-LINE2 PIC X(45)   VALUE ALL 'X'.
00030          10  FILLER                     PIC X(4)    VALUE SPACES.
00031          10  CPL10-CLAIM-NO             PIC X(7)    VALUE ALL 'X'.
00032          10  FILLER                     PIC X(9)    VALUE SPACES.
00033          10  CPL10-CERT-NO              PIC X(11)   VALUE ALL 'X'.
00034
00035      05  CHECK-PRINT-LINE-13.
00036          10  FILLER                     PIC X(12)   VALUE '-'.
00037          10  CPL13-DATA.
00038              15  CPL13-NAME             PIC X(30)   VALUE ALL 'X'.
00039              15  FILLER                 PIC X(15)   VALUE SPACES.
00040              15  CPL13-CHECK-DATE       PIC X(8)
00041                                             VALUE 'XX/XX/XX'.
00042              15  FILLER                 PIC X(2)    VALUE SPACES.
00043              15  CPL13-CHECK-AMOUNT     PIC $$,$$$,$$9.99-.
00044
00045      05  CHECK-PRINT-LINE-14.
00046          10  FILLER                     PIC X(12)   VALUE SPACES.
00047          10  CPL14-ADDRESS-LINE1        PIC X(30)   VALUE ALL 'X'.
00048
00049      05  CHECK-PRINT-LINE-15.
00050          10  FILLER                     PIC X(12)   VALUE SPACES.
00051          10  CPL15-ADDRESS-LINE2        PIC X(30)   VALUE ALL 'X'.
00052          10  FILLER                     PIC X(12)   VALUE SPACES.
00053          10  CPL15-VOID-MESSAGE1        PIC X(30)   VALUE
00054              '*** VOID *** VOID *** VOID ***'.
00055
00056      05  CHECK-PRINT-LINE-16.
00057          10  FILLER                     PIC X(12)   VALUE SPACES.
00058          10  CPL16-ADDRESS-LINE3        PIC X(30)   VALUE ALL 'X'.
00059          10  FILLER                     PIC X(12)   VALUE SPACES.
00060          10  CPL16-VOID-MESSAGE2        PIC X(30)   VALUE
00061              '*** VOID *** VOID *** VOID ***'.
00062
00063      05  CHECK-PRINT-LINE-17.
00064          10  FILLER                     PIC X(12)   VALUE SPACES.
00065          10  CPL17-ADDRESS-LINE4        PIC X(30)   VALUE ALL 'X'.
00066          10  FILLER                     PIC X(12)   VALUE SPACES.
00067          10  CPL17-VOID-MESSAGE3        PIC X(30)   VALUE
00068              '*** VOID *** VOID *** VOID ***'.
00069
00070      05  CHECK-PRINT-LINE-18.
00071          10  FILLER                     PIC X(11)   VALUE SPACES.
00072          10  CPL18-ADDRESS-LINE5.
00073              15  FILLER                 PIC X(21)   VALUE SPACES.
00074              15  CPL18-ZIP-CODE         PIC X(10)   VALUE SPACES.
00075
00076      05  CHECK-PRINT-LINE-21            PIC X       VALUE '-'.
00077
00078      05  CHECK-PRINT-LINE-23            PIC X       VALUE '0'.
00079
00080      05  CHECK-PRINT-LINE-25.
00081          10  FILLER                     PIC X(6)    VALUE '0'.
00082          10  CPL25-INSURED-NAME         PIC X(30)   VALUE ALL 'X'.
00083          10  FILLER                     PIC X(3)    VALUE SPACES.
00084          10  CPL25-CLAIM-TYPE           PIC X(10)   VALUE ALL 'X'.
00085          10  FILLER                     PIC X       VALUE SPACES.
00086          10  CPL25-PAYMENT-TYPE         PIC X(15)   VALUE ALL 'X'.
00087          10  FILLER                     PIC X(2)    VALUE SPACES.
00088          10  CPL25-BY                   PIC X(4)    VALUE ALL 'X'.
00089          10  FILLER                     PIC X(2)    VALUE SPACES.
00090          10  CPL25-PAID-TO-DATE         PIC Z,ZZZ,ZZ9.99-.
00091
00092      05  CHECK-PRINT-LINE-28.
00093          10  FILLER                     PIC X(5)    VALUE '-'.
00094          10  CPL28-STATE                PIC X(2)    VALUE ALL 'X'.
00095          10  FILLER                     PIC X(6)    VALUE SPACES.
00096          10  CPL28-ACCOUNT              PIC X(10)   VALUE ALL 'X'.
00097          10  FILLER                     PIC X       VALUE SPACES.
00098          10  CPL28-CERT-NO              PIC X(11)   VALUE ALL 'X'.
00099          10  FILLER                     PIC XX      VALUE SPACES.
00100          10  CPL28-CERT-EFF-DATE        PIC X(8)
00101                                             VALUE 'XX/XX/XX'.
00102          10  FILLER                     PIC X(5)    VALUE SPACES.
00103          10  CPL28-CLAIM-CODE           PIC X       VALUE ALL 'X'.
00104          10  FILLER                     PIC X(2)    VALUE SPACES.
00105          10  CPL28-INCURRED-DATE        PIC X(8)
00106                                             VALUE 'XX/XX/XX'.
00107          10  FILLER              REDEFINES
00108              CPL28-INCURRED-DATE.
00109              15  FILLER                 PIC X(6).
00110              15  CPL28-ID-YR1           PIC X.
00111              15  CPL28-ID-YR2           PIC X.
00112          10  CPL28-REPORTED-DATE        PIC X(8)
00113                                             VALUE 'XX/XX/XX'.
00114          10  FILLER              REDEFINES
00115              CPL28-REPORTED-DATE.
00116              15  FILLER                 PIC X(6).
00117              15  CPL28-RD-YR1           PIC X.
00118              15  CPL28-RD-YR2           PIC X.
00119          10  FILLER                     PIC X(1)    VALUE SPACES.
00120          10  CPL28-DATE-PAID            PIC X(8)
00121                                             VALUE 'XX/XX/XX'.
00122          10  FILLER              REDEFINES
00123              CPL28-DATE-PAID.
00124              15  FILLER                 PIC X(6).
00125              15  CPL28-DP-YR1           PIC X.
00126              15  CPL28-DP-YR2           PIC X.
00127          10  FILLER                     PIC X(1)    VALUE SPACES.
00128          10  CPL28-PAID-THRU            PIC X(8)
00129                                             VALUE 'XX/XX/XX'.
00130          10  FILLER              REDEFINES
00131              CPL28-PAID-THRU.
00132              15  FILLER                 PIC X(6).
00133              15  CPL28-PT-YR1           PIC X.
00134              15  CPL28-PT-YR2           PIC X.
00135
00136      05  ABL-PRINT-LINE-28.
00137          10  FILLER                     PIC X(5)    VALUE '-'.
00138          10  ABL28-STATE                PIC X(2)    VALUE ALL 'X'.
00139          10  FILLER                     PIC X(6)    VALUE SPACES.
00140          10  ABL28-ACCOUNT              PIC X(10)   VALUE ALL 'X'.
00141          10  FILLER                     PIC X       VALUE SPACES.
00142          10  ABL28-CERT-NO              PIC X(11)   VALUE ALL 'X'.
00143          10  FILLER                     PIC X       VALUE SPACES.
00144          10  ABL28-CERT-EFF-DATE        PIC X(8)
00145                                             VALUE 'XX/XX/XX'.
00146          10  FILLER                     PIC X(4)    VALUE SPACES.
00147          10  ABL28-CLAIM-CODE           PIC X       VALUE ALL 'X'.
00148          10  FILLER                     PIC X(2)    VALUE SPACES.
00149          10  ABL28-INCURRED-DATE        PIC X(8)
00150                                             VALUE 'XX/XX/XX'.
00151          10  FILLER              REDEFINES
00152              ABL28-INCURRED-DATE.
00153              15  FILLER                 PIC X(6).
00154              15  ABL28-ID-YR1           PIC X.
00155              15  ABL28-ID-YR2           PIC X.
00156          10  FILLER                     PIC X       VALUE SPACES.
00157          10  ABL28-REPORTED-DATE        PIC X(8)
00158                                             VALUE 'XX/XX/XX'.
00159          10  FILLER              REDEFINES
00160              ABL28-REPORTED-DATE.
00161              15  FILLER                 PIC X(6).
00162              15  ABL28-RD-YR1           PIC X.
00163              15  ABL28-RD-YR2           PIC X.
00164          10  FILLER                     PIC X(1)    VALUE SPACES.
00165          10  ABL28-DATE-PAID            PIC X(8)
00166                                             VALUE 'XX/XX/XX'.
00167          10  FILLER              REDEFINES
00168              ABL28-DATE-PAID.
00169              15  FILLER                 PIC X(6).
00170              15  ABL28-DP-YR1           PIC X.
00171              15  ABL28-DP-YR2           PIC X.
00172          10  FILLER                     PIC X(1)    VALUE SPACES.
00173          10  ABL28-PAID-THRU            PIC X(8)
00174                                             VALUE 'XX/XX/XX'.
00175          10  FILLER              REDEFINES
00176              ABL28-PAID-THRU.
00177              15  FILLER                 PIC X(6).
00178              15  ABL28-PT-YR1           PIC X.
00179              15  ABL28-PT-YR2           PIC X.
00180          10  FILLER                     PIC X(1)    VALUE SPACES.
00181
00182      05  CHECK-PRINT-LINE-31.
00183          10  FILLER                     PIC X(5)    VALUE '-'.
00184          10  CPL31-CHECK-AMOUNT         PIC Z,ZZZ,ZZ9.99-.
00185          10  FILLER                     PIC X(3)    VALUE SPACES.
00186          10  CPL31-CLAIM-NO             PIC X(7)    VALUE ALL 'X'.
00187          10  FILLER                     PIC X(4)    VALUE SPACES.
00188          10  CPL31-CHECK-NO             PIC X(7)    VALUE ALL 'X'.
00189          10  FILLER                     PIC X(4)    VALUE SPACES.
00190          10  CPL31-DAYS-PAID            PIC Z,ZZ9-.
00191          10  FILLER                     PIC X(2)    VALUE SPACES.
00192          10  CPL31-AGE                  PIC 99-.
00193          10  FILLER                     PIC X(4)    VALUE SPACES.
00194          10  CPL31-PAY-CODE             PIC X       VALUE ALL 'X'.
00195          10  FILLER                     PIC X(6)    VALUE SPACES.
00196          10  CPL31-CARRIER              PIC X       VALUE ALL 'X'.
00197          10  FILLER                     PIC X(6)    VALUE SPACES.
00198          10  CPL31-GROUP                PIC X(6)    VALUE ALL 'X'.
00199          10  FILLER                     PIC XX      VALUE SPACES.
00200          10  CPL31-ABL-ACCT             PIC X(3)    VALUE SPACES.
00201
00202      05  CHECK-PRINT-LINE-34.
00203          10  FILLER                     PIC X(5)    VALUE '0'.
00204          10  CPL34-COMMENT              PIC X(30)   VALUE SPACES.
00205
00206  01  CHECK-PRINT-LINES-SAVE-AREA        PIC X(1500) VALUE SPACES.
00207
           EJECT
LGC007*                           COPY ELCCSOCL.
       01  CSO-CHECK-PRINT-LINES.
           05  CSO-CHECK-PRINT-LINE-1.
               10  FILLER                  PIC X         VALUE '1'.
           05  CSO-CHECK-PRINT-LINE-2.
               10  FILLER                  PIC X(13)     VALUE ' '.
               10  CSO2-COMPANY-NAME       PIC X(43)     VALUE ALL 'X'.
               10  FILLER                  PIC X(7)      VALUE ' '.
               10  CSO2-CHECK-NUMBER       PIC X(7)      VALUE ALL 'X'.
           05  CSO-CHECK-PRINT-LINE-3.
               10  FILLER                  PIC X(18)     VALUE SPACES.
               10  FILLER                  PIC X(33)
                   VALUE 'P.O. BOX 34350   OMAHA, NE  68134'.
           05  CSO-CHECK-PRINT-LINE-4.
               10  FILLER                  PIC X         VALUE ' '.
           05  CSO-CHECK-PRINT-LINE-5.
               10  FILLER                  PIC X(2)      VALUE ' '.
               10  CSO5-CLAIM-NO           PIC X(7)      VALUE ALL 'X'.
LGC007         10  FILLER                  PIC X(3)      VALUE SPACES.
LGC007         10  CSO5-CERT-NO            PIC X(11)     VALUE ALL 'X'.
LGC007         10  FILLER                  PIC X(2)      VALUE SPACES.
               10  CSO5-PLAN-CODE          PIC X(4)      VALUE ALL 'X'.
               10  FILLER                  PIC X(3)      VALUE SPACES.
               10  CSO5-PAID-FROM-DATE     PIC X(8)   VALUE 'XX/XX/XX'.
               10  FILLER                  PIC X(2)      VALUE SPACES.
               10  CSO5-PAID-THRU-DATE     PIC X(8)   VALUE 'XX/XX/XX'.
               10  FILLER                  PIC X         VALUE SPACE.
               10  CSO5-AMOUNT-PAID        PIC ZZZ,ZZ9.99-.
               10  FILLER                  PIC X         VALUE SPACE.
               10  CSO5-PAYMENT-TYPE       PIC X      VALUE 'X'.
LGC045         10  FILLER                  PIC X         VALUE SPACE.
LGC045         10  CSO5-ACCT-NO            PIC X(10)  VALUE SPACES.
           05  CSO-CHECK-PRINT-LINE-6.
               10  FILLER                  PIC X         VALUE ' '.
           05  CSO-CHECK-PRINT-LINE-7.
               10  FILLER                  PIC X(5)      VALUE SPACES.
               10  CSO7-CC-ACCT            PIC X(7)      VALUE SPACES.
               10  CSO7-CC-ACCT-NUMBER     PIC X(18)     VALUE SPACES.
               10  FILLER                  PIC X(21)     VALUE SPACES.
               10  CSO7-TYPE-MESSAGE       PIC X(15)     VALUE ALL 'X'.
           05  CSO-CHECK-PRINT-LINE-8.
               10  FILLER                  PIC X         VALUE ' '.
           05  CSO-CHECK-PRINT-LINE-9.
               10  FILLER                  PIC X(50)     VALUE SPACES.
               10  CSO9-FINAL-MESS         PIC X(17)     VALUE ALL 'X'.
           05  CSO-CHECK-PRINT-LINE-10.
               10  FILLER                  PIC X(50)     VALUE ' '.
               10  CSO10-FINAL-MESS        PIC X(17)     VALUE ALL 'X'.
           05  CSO-CHECK-PRINT-LINE-11.
               10  FILLER                  PIC X(5)      VALUE ' '.
               10  CSO11-MEMBER-NAME       PIC X(30)     VALUE ALL 'X'.
               10  FILLER                  PIC X(15)     VALUE SPACES.
               10  CSO11-FINAL-MESS        PIC X(17)     VALUE ALL 'X'.
           05  CSO-CHECK-PRINT-LINE-12.
               10  FILLER                  PIC X(5)      VALUE SPACES.
               10  CSO12-MEMBER-ADDRESS1   PIC X(30)     VALUE ALL 'X'.
           05  CSO-CHECK-PRINT-LINE-13.
               10  FILLER                  PIC X(5)      VALUE SPACES.
               10  CSO13-MEMBER-ADDRESS2   PIC X(30)     VALUE ALL 'X'.
           05  CSO-CHECK-PRINT-LINE-14.
               10  FILLER                  PIC X(5)      VALUE SPACES.
               10  CSO14-MEMBER-ADDRESS3   PIC X(30)     VALUE ALL 'X'.
           05  CSO-CHECK-PRINT-LINE-15.
               10  FILLER                  PIC X(5)      VALUE SPACES.
               10  CSO15-MEMBER-ADDRESS4   PIC X(30)     VALUE ALL 'X'.
           05  CSO-CHECK-PRINT-LINE-16.
               10  FILLER                  PIC X(5)      VALUE ' '.
               10  CSO16-MEMBER-ADDRESS5.
                   15  FILLER                 PIC X(21).
                   15  CSO16-MEMBER-ZIP-CODE  PIC ZZZZ99999.
               10  FILLER                  PIC X(9)      VALUE SPACES.
LGC007     05  CSO-CHECK-PRINT-LINE-17.
LGC007         10  FILLER                  PIC X(5)      VALUE SPACES.
LGC007         10  CSO17-3RD-NAME          PIC X(40)     VALUE ALL 'X'.
           05  CSO-CHECK-PRINT-LINE-18.
               10  FILLER                  PIC X(5)      VALUE SPACES.
               10  CSO18-3RDADD-LINE1      PIC X(40)     VALUE ALL 'X'.
           05  CSO-CHECK-PRINT-LINE-19.
               10  FILLER                  PIC X(5)      VALUE SPACES.
               10  CSO19-3RDADD-LINE2      PIC X(40)     VALUE ALL 'X'.
           05  CSO-CHECK-PRINT-LINE-20.
               10  FILLER                  PIC X(5)      VALUE SPACES.
LGC007         10  CSO20-3RD-CITY-STATE    PIC X(40)     VALUE ALL 'X'.
           05  CSO-CHECK-PRINT-LINE-21.
LGC007         10  FILLER                  PIC X(26)     VALUE ' '.
LGC007         10  CSO21-3RD-ZIP           PIC ZZZZ99999.
LGC007*        10  FILLER                  PIC X(9)      VALUE SPACES.
LGC100         10  FILLER                  PIC X(5)      VALUE SPACES.
LGC100         10  CSO-NOTE-LINE1          PIC X(40)     VALUE SPACES.
           05  CSO-CHECK-PRINT-LINE-22.
LGC100*        10  FILLER                  PIC X         VALUE ' '.
LGC100         10  FILLER                  PIC X(40)     VALUE ' '.
LGC100         10  CSO-NOTE-LINE2          PIC X(40)     VALUE SPACES.
           05  CSO-CHECK-PRINT-LINE-23.
               10  FILLER                  PIC X(22)     VALUE SPACES.
               10  CSO23-REPLY-DT          PIC X(5)      VALUE ALL 'X'.
           05  CSO-CHECK-PRINT-LINE-24.
               10  FILLER                  PIC X(64)     VALUE ' '.
               10  CSO24-CHECK-NUMBER      PIC X(7)      VALUE ALL 'X'.
           05  CSO-CHECK-PRINT-LINE-25.
               10  FILLER                  PIC X         VALUE ' '.
           05  CSO-CHECK-PRINT-LINE-26.
               10  FILLER                  PIC X(3)      VALUE SPACES.
               10  CSO26-COMPANY-NAME      PIC X(43)     VALUE ALL 'X'.
           05  CSO-CHECK-PRINT-LINE-27.
               10  FILLER                  PIC X         VALUE ' '.
           05  CSO-CHECK-PRINT-LINE-28.
               10  FILLER                  PIC X         VALUE ' '.
           05  CSO-CHECK-PRINT-LINE-29.
               10  FILLER                  PIC X         VALUE ' '.
           05  CSO-CHECK-PRINT-LINE-30.
               10  FILLER                  PIC X         VALUE ' '.
           05  CSO-CHECK-PRINT-LINE-31.
               10  FILLER                  PIC X(3)      VALUE ' '.
               10  CSO31-CHECK-DATE        PIC X(8) VALUE 'XX/XX/XX'.
               10  FILLER                  PIC X(6)      VALUE SPACES.
               10  CSO31-CLAIM-NO          PIC X(7)      VALUE ALL 'X'.
               10  FILLER                  PIC XX        VALUE SPACES.
               10  CSO31-CC-ACCT           PIC X(7)      VALUE SPACES.
               10  CSO31-CC-ACCT-NUMBER    PIC X(18)     VALUE SPACES.
               10  FILLER                  PIC X(10)     VALUE SPACES.
               10  CSO31-CHECK-AMOUNT      PIC ***,**9.99-.
               10  CSO31-CHECK-AMOUNT-R REDEFINES CSO31-CHECK-AMOUNT
                                           PIC X(11).
           05  CSO-CHECK-PRINT-LINE-32.
               10  FILLER                  PIC X(17)     VALUE ' '.
               10  FILLER                  PIC X(9) VALUE 'INSURED: '.
               10  CSO32-MEMBER-NAME       PIC X(30)     VALUE ALL 'X'.
           05  CSO-CHECK-PRINT-LINE-33.
LGC107         10  FILLER                  PIC X(40)     VALUE ' '.
           05  CSO-CHECK-PRINT-LINE-34.
LGC107         10  FILLER                  PIC X(40)     VALUE ' '.
           05  CSO-CHECK-PRINT-LINE-35.
               10  FILLER                  PIC X(4)      VALUE SPACES.
               10  CSO35-PAYEE-NAME        PIC X(30)     VALUE ALL 'X'.
           05  CSO-CHECK-PRINT-LINE-36.
               10  FILLER                  PIC X(4)      VALUE SPACES.
               10  CSO36-PAYEE-ADDRESS1    PIC X(30)     VALUE ALL 'X'.
           05  CSO-CHECK-PRINT-LINE-37.
               10  FILLER                  PIC X(4)      VALUE SPACES.
               10  CSO37-PAYEE-ADDRESS2    PIC X(30)     VALUE ALL 'X'.
           05  CSO-CHECK-PRINT-LINE-38.
               10  FILLER                  PIC X(4)      VALUE SPACES.
               10  CSO38-PAYEE-ADDRESS3    PIC X(30)     VALUE ALL 'X'.
           05  CSO-CHECK-PRINT-LINE-39.
               10  FILLER                  PIC X(4)      VALUE SPACES.
               10  CSO39-PAYEE-ADDRESS4    PIC X(30)     VALUE ALL 'X'.
           05  CSO-CHECK-PRINT-LINE-40.
               10  FILLER                  PIC X(4)      VALUE SPACES.
               10  CSO40-PAYEE-ADDRESS5.
                   15  FILLER              PIC X(21)     VALUE SPACES.
                   15  CSO40-PAYEE-ZIP-CODE  PIC ZZZZ99999 VALUE ZEROS.
           05  CSO-CHECK-PRINT-LINE-41.
               10  FILLER                  PIC X         VALUE ' '.
           05  CSO-CHECK-PRINT-LINE-42.
               10  FILLER                  PIC X         VALUE ' '.
           EJECT
LGC007*                           COPY ELCDATE.
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
LGC007*01  S-WORK-AREA COPY ELPRTCVD.
LGC007*                       COPY ELPRTCVD.
CSOMOD 01  CSO-DRAFT-420C.
CSODJN     05  CSO-DRAFT-KEY           PIC X(19).
011105     05  FILLER                  PIC X(1235).
CSOMOD 01  CSO-DRAFT-420C-RED REDEFINES CSO-DRAFT-420C.
CSOMOD*    COPY MICR420C.
120803******************************************************************
120803*                   C H A N G E   L O G
120803*
120803* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
120803*-----------------------------------------------------------------
120803*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
120803* EFFECTIVE    NUMBER
120803*-----------------------------------------------------------------
120803* 120803    2002100700001  SMVA  ADD ACCT STATE & CLM TYPE FOR
120803*                                PRINTING STATE SPEC PROGRESS RPTS
011105* 011105    2004072100002  PEMA  ADD ONE COMMENT LINE
013017* 013017  CR2016053100001  PEMA  ACH PROCESSING
120803******************************************************************
120803*
011105* CURRENT LENGTH OF LAYOUT IS 1254
120803*
120803******************************************************************
DJNA         10 MICR-KEY.
000001           15 M420C-FORM         PIC X(4).
000002           15 M420C-DRAFT-ORDER  PIC 9(5).
CSODJN           15 M420C-DRAFT        PIC X(10).
000004       10 M420C-PRINT-COUNT      PIC S9(4) COMP.
000005       10 M420C-SEQ-NUMBER       PIC S9(4) COMP.
000006       10 M420C-AMOUNT-PAID      PIC S9(9)V9(2) COMP-3.
000007       10 M420C-COMPANY-NAME     PIC X(43).
000008       10 M420C-CSO-ADDRESS      PIC X(33).
000009       10 M420C-CLAIM-NO         PIC X(7).
000010       10 M420C-CERT-NO          PIC X(11).
000011       10 M420C-ACCT-NO          PIC X(10).
000012       10 M420C-PLAN-CODE        PIC X(4).
000013       10 M420C-PAID-FROM-DATE   PIC X(8).
000014       10 M420C-PAID-THRU-DATE   PIC X(8).
000015       10 M420C-PAYMENT-TYPE     PIC X.
000016       10 M420C-CC-ACCT          PIC X(7).
000017       10 M420C-CC-ACCT-NUMBER   PIC X(18).
000018       10 M420C-TYPE-MESSAGE     PIC X(15).
000019       10 M420C-FINAL-MESS9      PIC X(17).
000020       10 M420C-FINAL-MESS10     PIC X(17).
000021       10 M420C-FINAL-MESS11     PIC X(17).
000022       10 M420C-MEMBER-NAME      PIC X(30).
000023       10 M420C-MEMBER-ADDRESS1  PIC X(30).
000024       10 M420C-MEMBER-ADDRESS2  PIC X(30).
000025       10 M420C-MEMBER-ADDRESS3  PIC X(30).
000026       10 M420C-MEMBER-ADDRESS4  PIC X(30).
000027       10 M420C-MEMBER-ZIP-CODE  PIC X(9).
000028       10 M420C-3RDADD-NAME      PIC X(30).
000029       10 M420C-3RDADD-LINE1     PIC X(30).
000030       10 M420C-3RDADD-LINE2     PIC X(40).
000031       10 M420C-3RDADD-LINE3     PIC X(40).
000032       10 M420C-3RDADD-ZIP       PIC X(9).
000033       10 M420C-CHECK-DATE       PIC X(8).
000034       10 M420C-DFT-NOTES1       PIC X(40).
000035       10 M420C-DFT-NOTES2       PIC X(39).
013017       10 M420C-ACH-PAYMENT      PIC X.
000036       10 M420C-PAYEE-NAME       PIC X(30).
000037       10 M420C-PAYEE-ADDRESS1   PIC X(30).
000038       10 M420C-PAYEE-ADDRESS2   PIC X(30).
000039       10 M420C-PAYEE-ADDRESS3   PIC X(30).
000040       10 M420C-PAYEE-ADDRESS4   PIC X(30).
000041       10 M420C-PAYEE-ZIP-CODE   PIC X(9).
000042       10 M420C-REPLY-DATE       PIC X(5).
000043       10 M420C-SIGNATURE        PIC X.
CIDMOD       10 M420C-LOAN-NUMBER      PIC X(25).
CIDMOD       10 M420C-DRAFT-MESSAGES.
011105          15 M420C-DRAFT-MESSAGE OCCURS 6 TIMES  PIC X(70).
120803       10 M420C-ACCT-STATE       PIC X(02).
120803       10 M420C-CLAIM-TYPE       PIC X(01).
CSOMOD 01  CSO-RESP                    PIC S9(5) COMP.
CSOMOD 01  CSO-ZIP                     PIC Z(9).
LGC007     EJECT
DMBMOD*                    COPY ELCCPA.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCCPA.                             *
00004 *                            VMOD=2.013                          *
00005 *                                                                *
00006 *   DESCRIPTION:  DATA TO BE PASSED TO CHECK WRITER ROUTINE.     *
00007 ******************************************************************
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
072110* 072110    2009122800001  AJRA  CHANGE COMMENT,COMMENT-2 FROM 40
072110*                                SEPARATE CITY AND STATE
013017* 013017  CR2016053100001  PEMA  ACH PROCESSING
      ******************************************************************
00008
00009  01  CHECK-PASS-AREA.
00010      12  CPA-ALIGNMENT               PIC S9(3)    COMP-3.
00011      12  CPA-CARRIER                 PIC X.
00012      12  CPA-GROUP                   PIC X(6).
00013      12  CPA-ACCOUNT                 PIC X(10).
00014      12  CPA-STATE                   PIC XX.
00015      12  CPA-CLAIM-NO                PIC X(7).
00016      12  CPA-CERT-NO                 PIC X(11).
00017      12  CPA-CERT-EFF-DT             PIC XX.
00018      12  CPA-CLAIM-TYPE              PIC X.
00019      12  CPA-PAYEE-TYPE-CD           PIC X.
00020      12  CPA-IND-GRP-TYPE            PIC X.
00021          88  CPA-INDIVIDUAL                     VALUE 'I'.
00022          88  CPA-GROUP-POLICY                   VALUE 'G'.
00023      12  CPA-PAYEE-NAME              PIC X(30).
00024      12  CPA-PAYEE-ADDRESS-LINE1     PIC X(30).
00025      12  CPA-PAYEE-ADDRESS-LINE2     PIC X(30).
00026      12  CPA-PAYEE-ADDRESS-LINE3     PIC X(30).
072110     12  CPA-PAYEE-CITY-STATE.
072110         16  CPA-PAYEE-CITY          PIC X(28).
072110         16  CPA-PAYEE-STATE         PIC X(2).
00028      12  CPA-PAYEE-ZIP.
00029          16  CPA-PAYEE-ZIP-CODE.
00030              24  CPA-ZIP-1ST-PAYEE   PIC X.
00031                  88  CPA-CAN-POST-CODE-PAYEE  VALUE 'A' THRU 'Z'.
00032              24  FILLER              PIC X(4).
00033          16  CPA-PAYEE-ZIP-PLUS4     PIC X(4).
00034      12  CPA-CANADIAN-POSTAL-CODE-PAYEE REDEFINES CPA-PAYEE-ZIP.
00035          16  CPA-CAN-POSTAL-1-PAYEE  PIC XXX.
00036          16  CPA-CAN-POSTAL-2-PAYEE  PIC XXX.
00037          16  FILLER                  PIC XXX.
00038      12  CPA-INSURED-NAME            PIC X(30).
00039      12  CPA-INSURED-ADDRESS-LINE1   PIC X(30).
00040      12  CPA-INSURED-ADDRESS-LINE2   PIC X(30).
00041      12  CPA-INSURED-ADDRESS-LINE3   PIC X(30).
072110     12  CPA-INSURED-CITY-STATE.
072110         16  CPA-INSURED-CITY        PIC X(28).
072110         16  CPA-INSURED-STATE       PIC X(2).
00043      12  CPA-INSURED-ZIP.
00044          16  CPA-INSURED-ZIP-CODE.
00045              24  CPA-ZIP-1ST-INS     PIC X.
00046                  88  CPA-CAN-POST-CODE-INS    VALUE 'A' THRU 'Z'.
00047              24  FILLER              PIC X(4).
00048          16  CPA-INSURED-ZIP-PLUS4   PIC X(4).
00049      12  CPA-CANADIAN-POSTAL-CODE-INS REDEFINES CPA-INSURED-ZIP.
00050          16  CPA-CAN-POSTAL-1-INS    PIC XXX.
00051          16  CPA-CAN-POSTAL-2-INS    PIC XXX.
00052          16  FILLER                  PIC XXX.
00053      12  CPA-INSURED-AGE             PIC 99.
00054      12  CPA-PAYMENT-TYPE            PIC X.
00055      12  CPA-PAYMENT-BY              PIC X(4).
00056      12  CPA-CHECK-DATE              PIC X(2).
00057      12  CPA-CHECK-NUMBER            PIC X(7).
00058      12  CPA-AMOUNT-PAID             PIC S9(7)V99    COMP-3.
00059      12  CPA-AMOUNT-PAID-TO-DATE     PIC S9(7)V99    COMP-3.
00060      12  CPA-DAYS-PAID               PIC S9(5)       COMP-3.
00061      12  CPA-DAILY-RATE              PIC S9(3)V99    COMP-3.
00062      12  CPA-ELIMINATION-DAYS        PIC S9(3)       COMP-3.
00063      12  CPA-CLAIM-CODE              PIC X.
00064      12  CPA-PAY-CODE                PIC X.
00065      12  CPA-INCURRED-DT             PIC XX.
00066      12  CPA-REPORTED-DT             PIC XX.
00067      12  CPA-PAID-THRU-DT            PIC XX.
00068      12  CPA-PAID-FROM-DT            PIC XX.
00069      12  CPA-PAID-DT                 PIC XX.
00070
00071      12  CPA-ACCOUNT-NAME            PIC X(30).
00072      12  CPA-ACCOUNT-IN-CARE-OF      PIC X(30).
00073      12  CPA-ACCOUNT-ADDRESS-LINE1   PIC X(30).
00074      12  CPA-ACCOUNT-ADDRESS-LINE2   PIC X(30).
072110     12  CPA-ACCOUNT-CITY-ST.
072110         16  CPA-ACCOUNT-CITY        PIC X(28).
072110         16  CPA-ACCOUNT-STATE       PIC X(2).
00076      12  CPA-ACCOUNT-ZIP-CODE.
00077          16  CPA-ACCOUNT-ZIP.
00078              24  CPA-ZIP-1ST-ACCT    PIC X.
00079                  88  CPA-CAN-POST-CODE-ACCT VALUE 'A' THRU 'Z'.
00080              24  FILLER              PIC X(4).
00081          16  CPA-ACCOUNT-ZIP-PLUS4   PIC X(4).
00082      12  CPA-CANADIAN-POSTAL-CODE-ACCT
00083              REDEFINES CPA-ACCOUNT-ZIP-CODE.
00084          16  CPA-CAN-POSTAL-1-ACCT   PIC XXX.
00085          16  CPA-CAN-POSTAL-2-ACCT   PIC XXX.
00086          16  FILLER                  PIC XXX.
00087      12  CPA-ACCOUNT-PHONE-NO        PIC S9(11)     COMP-3.
00088
00089      12  CPA-SOC-SEC-NO              PIC X(11).
00090      12  CPA-MEMBER-NUMBER           PIC X(12).
00091      12  CPA-LOAN-NUMBER             PIC X(8).
00092
00093      12  CPA-BENEFIT-TYPE            PIC X.
00094
00095      12  CPA-NO-OF-PMTS-MADE         PIC S9(3)       COMP-3.
00096
00097      12  CPA-EXPIRE-DT               PIC XX.
00098
00099      12  CPA-MONTHLY-BENEFIT         PIC S9(7)V99    COMP-3.
00100
072110     12  CPA-COMMENT                 PIC X(60).
00102
00103      12  CPA-CLAIM-STATUS            PIC X.
00104 *      88  CLAIM-IS-OPEN                             VALUE 'O'.
00105 *      88  CLAIM-IS-CLOSED                           VALUE 'C'.
00106
00107      12  CPA-LAST-CLOSE-REASON       PIC X.
00108 *      88  FINAL-PAID                                VALUE '1'.
00109 *      88  CLAIM-DENIED                              VALUE '2'.
00110 *      88  AUTO-CLOSE                                VALUE '3'.
00111
00112      12  CPA-INSURED-ADDR-TRLR-NAME  PIC X(30).
00113
00114      12  CPA-EXAMINER                PIC X(4).
00115      12  CPA-EXPENSE-TYPE            PIC X.
00116
00117      12  CPA-CARRIER-ADDRESS-DATA.
00118          16  CPA-CARRIER-NAME            PIC X(30).
00119          16  CPA-CARRIER-ADDRESS-LINE1   PIC X(30).
00120          16  CPA-CARRIER-ADDRESS-LINE2   PIC X(30).
00121          16  CPA-CARRIER-ADDRESS-LINE3   PIC X(30).
072110         16  CPA-CARRIER-CITY-STATE.
072110             24  CPA-CARRIER-CITY        PIC X(28).
072110             24  CPA-CARRIER-STATE       PIC X(2).
00123          16  CPA-CARRIER-ZIP.
00124              24  CPA-CARRIER-ZIP-CODE.
00125                  28  CPA-ZIP-1ST-CARRIER PIC X.
00126                      88  CPA-CAN-POST-CODE-CARR
00127                                          VALUE 'A' THRU 'Z'.
00128                  28  FILLER              PIC X(4).
00129              24  CPA-CARRIER-ZIP-PLUS4   PIC X(4).
00130          16  CPA-CANADIAN-POSTAL-CODE-CARR
00131                            REDEFINES CPA-CARRIER-ZIP.
00132              24  CPA-CAN-POSTAL-1-CARR   PIC XXX.
00133              24  CPA-CAN-POSTAL-2-CARR   PIC XXX.
00134              24  FILLER                  PIC XXX.
00135
00136      12  CPA-PAYMENT-ORIGIN              PIC X.
00137      12  CPA-BENEFIT-CD                  PIC XX.
072110     12  CPA-COMMENT-2                   PIC X(60).
00139
00140      12  CPA-NOTIFY-ADDRESS-DATA.
00141          16  CPA-NOTIFY-NAME             PIC X(30).
00142          16  CPA-NOTIFY-ADDRESS-LINE1    PIC X(30).
00143          16  CPA-NOTIFY-ADDRESS-LINE2    PIC X(30).
00144          16  CPA-NOTIFY-ADDRESS-LINE3    PIC X(30).
072110         16  CPA-NOTIFY-CITY-STATE.
072110             20  CPA-NOTIFY-CITY         PIC X(28).
072110             20  CPA-NOTIFY-STATE        PIC X(2).
00146          16  CPA-NOTIFY-ZIP.
00147              20  CPA-NOTIFY-ZIP-CODE     PIC X(5).
00148              20  CPA-NOTIFY-ZIP-PLUS4    PIC X(4).
00149      12  CPA-COVERAGE-TYPE               PIC X.
00150          88  CPA-LEVEL-COV                 VALUE 'L'.
00151          88  CPA-REDUCE-COV                VALUE 'R'.
00152          88  CPA-PROP-COV                  VALUE 'P'.
00153          88  CPA-DISAB-COV                 VALUE 'A'.
00154
00155      12  CPA-AUTO-PAY-END-DT             PIC X(02).
00156      12  CPA-COMPANY-NAME                PIC X(30).
00157      12  CPA-TOTAL-BENEFIT               PIC S9(09)V99  COMP-3.
00158      12  CPA-FORM-CTL-SEQ-NO             PIC S9(4)      COMP.
00159      12  CPA-BENEFICIARY                 PIC X(25).
00160      12  CPA-REFERENCE-NO.
013017         16  CPA-REFERENCE-PRIME         PIC X(17).
00162          16  CPA-REFERENCE-SFX           PIC X(02).
013017     12  CPA-ACH-PAYMENT                 PIC X.
00163      12  CPA-CV-LOAN-NUMBER              PIC X(20).
00164      12  CPA-CREDIT-CARD-NO REDEFINES
00165          CPA-CV-LOAN-NUMBER              PIC X(20).
           EJECT
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
LGC007 01  DFHCOMMAREA                 PIC X(1024).
           EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL177' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
           
      * EXEC CICS HANDLE CONDITION
      *        QIDERR  (5000-MAIN-LOGIC)
      *        ITEMERR (5000-MAIN-LOGIC)
      *        ERROR   (9990-ERROR) END-EXEC.
      *    MOVE '"$N<.                 ! " #00001736' TO DFHEIV0
           MOVE X'22244E3C2E20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303031373336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           EJECT
       0010-MAIN-LOGIC.
           
      * EXEC CICS RETRIEVE
      *        INTO   (PROGRAM-INTERFACE-BLOCK)
      *        LENGTH (PI-COMM-LENGTH) END-EXEC
      *    MOVE '0*I L                 &   #00001742' TO DFHEIV0
           MOVE X'302A49204C20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           MOVE +1                     TO  PI-TEMP-STORAGE-ITEM.
       0100-MAIN-LOGIC.
           
      * EXEC CICS READQ TS
      *        INTO   (CHECK-PASS-AREA)
      *        QUEUE  (PI-TEMP-STORAGE-KEY)
      *        ITEM   (PI-TEMP-STORAGE-ITEM)
      *        LENGTH (WS-TS-LENGTH)
CIDMOD*    END-EXEC
      *    MOVE '*$II   L              ''   #00001747' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-TEMP-STORAGE-KEY, 
                 CHECK-PASS-AREA, 
                 WS-TS-LENGTH, 
                 PI-TEMP-STORAGE-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF WS-TS-LENGTH NOT GREATER THAN +1
               
      * EXEC CICS DELETEQ TS
      *            QUEUE (PI-TEMP-STORAGE-KEY) END-EXEC
      *    MOVE '*&                    #   #00001754' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-TEMP-STORAGE-KEY, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
               
      * EXEC CICS RETURN
      *            END-EXEC.
      *    MOVE '.(                    ''   #00001756' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
030612     IF PI-COMPANY-ID = ('CID' OR 'DCC' OR 'AHL')
              GO TO 0600-MAIN-LOGIC.
       0600-MAIN-LOGIC.
           IF CPA-ALIGNMENT NOT EQUAL TO ZERO
               MOVE +999999.99         TO  CSO31-CHECK-AMOUNT
               MOVE +999999.99         TO  CSO5-AMOUNT-PAID
               GO TO 0650-MAIN-LOGIC.
      *    MOVE CSO-CHECK-PRINT-LINES  TO  CHECK-PRINT-LINES-SAVE-AREA.
121903     MOVE CHECK-PRINT-LINES-SAVE-AREA  TO  CSO-CHECK-PRINT-LINES
      * THE FOLLOWING IF IS USED TO PRINT OR SPACE THE FINAL MESSAGE
121903     IF (CPA-PAYMENT-TYPE = '2')
052614        AND (CPA-CLAIM-TYPE = 'A' OR 'I' OR 'G' OR 'F'
080322                                  OR 'B' OR 'H' )
                MOVE WS-LINE-9  TO CSO9-FINAL-MESS
                MOVE WS-LINE-10 TO CSO10-FINAL-MESS
                MOVE WS-LINE-11 TO CSO11-FINAL-MESS
           ELSE
                MOVE SPACES TO CSO9-FINAL-MESS
                               CSO10-FINAL-MESS
                               CSO11-FINAL-MESS
           END-IF
           MOVE EIBDATE           TO DC-JULIAN-YYDDD.
           MOVE '5'               TO DC-OPTION-CODE.
           PERFORM 8500-DATE-CONVERSION.
           MOVE DC-BIN-DATE-1       TO WS-BIN-CURRENT-DT.
CSODAN     MOVE DC-GREG-DATE-1-EDIT TO WS-EDT-CURRENT-DT
052614     IF (CPA-CLAIM-TYPE = 'A' OR 'I' OR 'G' OR 'F'
080322                                  OR 'B' OR 'H' )
CSODAN         AND (CPA-PAYMENT-TYPE = '1' OR '2')
                   MOVE CPA-PAID-THRU-DT TO DC-BIN-DATE-1
                   MOVE +1               TO DC-ELAPSED-MONTHS
                                            DC-ELAPSED-DAYS
                   MOVE '6' TO DC-OPTION-CODE
                   PERFORM 8500-DATE-CONVERSION
                   IF DC-BIN-DATE-2 > WS-BIN-CURRENT-DT
                      MOVE DC-GREG-DATE-1-EDIT TO CSO23-REPLY-DT
                   ELSE
CSODAN                MOVE WS-EDT-CURRENT-DT   TO CSO23-REPLY-DT
CSODAN***             MOVE ' NOW ' TO CSO23-REPLY-DT
           ELSE
               MOVE SPACES TO CSO23-REPLY-DT.
           MOVE CPA-SOC-SEC-NO TO WS-SOC-SEC-NO.
030612     IF PI-COMPANY-ID = 'CID' OR 'DCC' OR 'AHL'
               MOVE SPACES TO CSO31-CC-ACCT
                              CSO31-CC-ACCT-NUMBER
                              CSO7-CC-ACCT
                              CSO7-CC-ACCT-NUMBER
           ELSE
               MOVE 'ACCT # ' TO CSO31-CC-ACCT, CSO7-CC-ACCT
               MOVE WS-SOC-SEC-NO TO CSO31-CC-ACCT-NUMBER
                                     CSO7-CC-ACCT-NUMBER.
100518     IF (CPA-CLAIM-TYPE = 'L' OR 'O') AND (CPA-PAYMENT-TYPE = '4')
CSODAN        MOVE '  FINAL PAYMENT'  TO  CSO7-TYPE-MESSAGE
CSODAN        MOVE 'F'                TO  CSO5-PAYMENT-TYPE
CSODAN     ELSE
              IF CPA-PAYMENT-TYPE EQUAL TO '2'
                 MOVE '  FINAL PAYMENT'  TO  CSO7-TYPE-MESSAGE
                 MOVE 'F'                TO  CSO5-PAYMENT-TYPE
              ELSE
                 MOVE 'PARTIAL PAYMENT'  TO  CSO7-TYPE-MESSAGE
                 MOVE 'P'                TO  CSO5-PAYMENT-TYPE
              END-IF
           END-IF
           MOVE CPA-INSURED-ADDR-TRLR-NAME TO CSO11-MEMBER-NAME.
           MOVE CPA-INSURED-NAME TO CSO32-MEMBER-NAME.
           MOVE CPA-INSURED-ADDRESS-LINE1  TO  CSO12-MEMBER-ADDRESS1
           MOVE SPACES            TO CSO5-PLAN-CODE.
           MOVE CPA-INSURED-ADDRESS-LINE2  TO  CSO13-MEMBER-ADDRESS2
           MOVE CPA-INSURED-ADDRESS-LINE3  TO  CSO14-MEMBER-ADDRESS3
           MOVE CPA-CLAIM-NO           TO  CSO5-CLAIM-NO
                                           CSO31-CLAIM-NO
           MOVE CPA-CERT-NO            TO  CSO5-CERT-NO
LGC045     MOVE CPA-ACCOUNT            TO  CSO5-ACCT-NO.
LGC045
           MOVE CPA-INSURED-CITY-STATE  TO  CSO15-MEMBER-ADDRESS4
           IF CPA-INSURED-ZIP-CODE NOT EQUAL TO ZERO
               MOVE CPA-INSURED-ZIP-CODE  TO  CSO16-MEMBER-ZIP-CODE.
           IF CSO15-MEMBER-ADDRESS4 EQUAL TO SPACES
               MOVE CSO16-MEMBER-ADDRESS5 TO CSO15-MEMBER-ADDRESS4
               MOVE SPACES             TO  CSO16-MEMBER-ADDRESS5.
           IF CSO14-MEMBER-ADDRESS3 EQUAL TO SPACES
               MOVE CSO15-MEMBER-ADDRESS4 TO CSO14-MEMBER-ADDRESS3
               MOVE CSO16-MEMBER-ADDRESS5 TO CSO15-MEMBER-ADDRESS4
               MOVE SPACES             TO  CSO16-MEMBER-ADDRESS5.
           IF CSO13-MEMBER-ADDRESS2 EQUAL TO SPACES
               MOVE CSO14-MEMBER-ADDRESS3 TO CSO13-MEMBER-ADDRESS2
               MOVE CSO15-MEMBER-ADDRESS4 TO CSO14-MEMBER-ADDRESS3
               MOVE CSO16-MEMBER-ADDRESS5 TO CSO15-MEMBER-ADDRESS4
               MOVE SPACES             TO  CSO16-MEMBER-ADDRESS5.
           IF CSO12-MEMBER-ADDRESS1 EQUAL TO SPACES
               MOVE CSO13-MEMBER-ADDRESS2 TO CSO12-MEMBER-ADDRESS1
               MOVE CSO14-MEMBER-ADDRESS3 TO CSO13-MEMBER-ADDRESS2
               MOVE CSO15-MEMBER-ADDRESS4 TO CSO14-MEMBER-ADDRESS3
               MOVE CSO16-MEMBER-ADDRESS5 TO CSO15-MEMBER-ADDRESS4
               MOVE SPACES             TO  CSO16-MEMBER-ADDRESS5.
           IF CPA-PAID-FROM-DT NOT EQUAL TO LOW-VALUES
               MOVE CPA-PAID-FROM-DT   TO  DC-BIN-DATE-1
               MOVE SPACES             TO  DC-OPTION-CODE
               PERFORM 8500-DATE-CONVERSION
               IF DC-ERROR-CODE EQUAL TO SPACES
                   MOVE DC-GREG-DATE-1-EDIT  TO  CSO5-PAID-FROM-DATE.
           IF CPA-PAID-THRU-DT NOT EQUAL TO LOW-VALUES
               MOVE CPA-PAID-THRU-DT   TO  DC-BIN-DATE-1
               MOVE SPACES             TO  DC-OPTION-CODE
               PERFORM 8500-DATE-CONVERSION
               IF DC-ERROR-CODE EQUAL TO SPACES
                   MOVE DC-GREG-DATE-1-EDIT  TO  CSO5-PAID-THRU-DATE.
LGC007     MOVE CPA-CERT-NO            TO WS-CHECK-AREA.
LGC007
030612     IF PI-COMPANY-ID EQUAL TO 'CID' OR 'DCC' OR 'AHL'
LGC007       MOVE 01-HEADING TO WS-COMPANY-NAME
LGC007       MOVE 01-COMP-NAME TO WS-COMPANY-NAME2
121903     END-IF.
           MOVE WS-COMPANY-NAME TO CSO2-COMPANY-NAME
           MOVE WS-COMPANY-NAME2 TO CSO26-COMPANY-NAME
052614     IF CPA-CLAIM-TYPE = 'A' OR 'I' OR 'G' OR 'F'
080322                                  OR 'B' OR 'H'
               MOVE 'A&H ' TO CSO5-PLAN-CODE
           ELSE
               MOVE 'LIFE' TO CSO5-PLAN-CODE.
LGC007*****CLAIM TYPE P WAS ADDED TO FLAG THOSE LIFE CLAIMS WHICH
LGC007*****ARE IN REALITY PROPERTY CLAIMS BEING PROCESSED THRU THE
LGC007*****LOGIC SYSTEM LIFE SECTIONS.  FOR CID CHANGE COMPANY NAME.
LGC007     IF CPA-COVERAGE-TYPE = 'P'
LGC007         MOVE 'PROP' TO CSO5-PLAN-CODE
030612         IF PI-COMPANY-ID EQUAL TO 'CID' OR 'DCC' OR 'AHL'
LGC007            MOVE 02-HEADING TO CSO2-COMPANY-NAME
LGC007            MOVE 02-COMP-NAME TO CSO26-COMPANY-NAME.
           MOVE CPA-AMOUNT-PAID        TO  CSO5-AMOUNT-PAID
                                           CSO31-CHECK-AMOUNT
           MOVE CPA-CHECK-NUMBER       TO  CSO2-CHECK-NUMBER
                                           CSO24-CHECK-NUMBER
           IF CPA-CHECK-DATE NOT EQUAL TO LOW-VALUES
               MOVE CPA-CHECK-DATE     TO  DC-BIN-DATE-1
               MOVE SPACES             TO  DC-OPTION-CODE
               PERFORM 8500-DATE-CONVERSION
               IF DC-ERROR-CODE EQUAL TO SPACES
                   MOVE DC-GREG-DATE-1-EDIT TO  CSO31-CHECK-DATE.
           MOVE CPA-COMMENT TO WS-CPA-COMMENT.
LGC107     MOVE CPA-COMMENT              TO  CSO-CHECK-PRINT-LINE-33
LGC107     MOVE CPA-COMMENT-2            TO  CSO-CHECK-PRINT-LINE-34
           MOVE CPA-PAYEE-NAME           TO  CSO35-PAYEE-NAME
           MOVE CPA-PAYEE-ADDRESS-LINE1  TO  CSO36-PAYEE-ADDRESS1
           MOVE CPA-PAYEE-ADDRESS-LINE2  TO  CSO37-PAYEE-ADDRESS2
           MOVE CPA-PAYEE-ADDRESS-LINE3  TO  CSO38-PAYEE-ADDRESS3
           MOVE CPA-PAYEE-CITY-STATE     TO  CSO39-PAYEE-ADDRESS4
           IF CPA-PAYEE-ZIP-CODE NOT EQUAL TO ZERO
               MOVE CPA-PAYEE-ZIP-CODE  TO  CSO40-PAYEE-ZIP-CODE.
           IF CSO39-PAYEE-ADDRESS4 EQUAL TO SPACES
               MOVE CSO40-PAYEE-ADDRESS5 TO CSO39-PAYEE-ADDRESS4
               MOVE SPACES             TO  CSO40-PAYEE-ADDRESS5.
           IF CSO38-PAYEE-ADDRESS3 EQUAL TO SPACES
               MOVE CSO39-PAYEE-ADDRESS4 TO CSO38-PAYEE-ADDRESS3
               MOVE CSO40-PAYEE-ADDRESS5 TO CSO39-PAYEE-ADDRESS4
               MOVE SPACES             TO  CSO40-PAYEE-ADDRESS5.
           IF CSO37-PAYEE-ADDRESS2 EQUAL TO SPACES
               MOVE CSO38-PAYEE-ADDRESS3 TO CSO37-PAYEE-ADDRESS2
               MOVE CSO39-PAYEE-ADDRESS4 TO CSO38-PAYEE-ADDRESS3
               MOVE CSO40-PAYEE-ADDRESS5 TO CSO39-PAYEE-ADDRESS4
               MOVE SPACES             TO  CSO40-PAYEE-ADDRESS5.
           IF CSO36-PAYEE-ADDRESS1 EQUAL TO SPACES
               MOVE CSO37-PAYEE-ADDRESS2 TO CSO36-PAYEE-ADDRESS1
               MOVE CSO38-PAYEE-ADDRESS3 TO CSO37-PAYEE-ADDRESS2
               MOVE CSO39-PAYEE-ADDRESS4 TO CSO38-PAYEE-ADDRESS3
               MOVE CSO40-PAYEE-ADDRESS5 TO CSO39-PAYEE-ADDRESS4
               MOVE SPACES             TO  CSO40-PAYEE-ADDRESS5.
       0600-MAIN-LOGIC-CONTINUE.
LGC007     MOVE CPA-NOTIFY-NAME                TO CSO17-3RD-NAME.
LGC007     MOVE CPA-NOTIFY-ADDRESS-LINE1       TO CSO18-3RDADD-LINE1.
LGC007     MOVE CPA-NOTIFY-ADDRESS-LINE2       TO CSO19-3RDADD-LINE2.
LGC007     MOVE CPA-NOTIFY-CITY-STATE          TO CSO20-3RD-CITY-STATE.
LGC007     MOVE CPA-NOTIFY-ZIP                 TO CSO21-3RD-ZIP.
           EJECT
       0650-MAIN-LOGIC.
           ADD +1  TO  PI-TEMP-STORAGE-ITEM
CSOMOD     IF CPA-ALIGNMENT NOT EQUAL   TO ZERO THEN
              GO TO 0100-MAIN-LOGIC.
CSOMOD     MOVE LOW-VALUES              TO CSO-DRAFT-420C.
103002     IF PI-COMPANY-ID = 'DCC'
103002        MOVE 'DCC1'               TO M420C-FORM
103002     ELSE
103002        MOVE '420C'               TO M420C-FORM
103002     END-IF
CSODJN     MOVE '0'                     TO M420C-DRAFT(1:1).
CSODJN     MOVE CSO2-CHECK-NUMBER(1:1)  TO M420C-DRAFT(2:1).
CSODJN     MOVE '00'                    TO M420C-DRAFT(3:2).
CSODJN     MOVE CSO2-CHECK-NUMBER(2:6)  TO M420C-DRAFT(5:6).
LGC101     IF WS-DRAFT-ORDER = 99999
LGC101         MOVE ZEROS               TO WS-DRAFT-ORDER.
LGC101     ADD 1 TO WS-DRAFT-ORDER.
LGC101     MOVE WS-DRAFT-ORDER          TO M420C-DRAFT-ORDER.
CIDMOD     MOVE CPA-BENEFICIARY         TO M420C-LOAN-NUMBER
CSOMOD     MOVE CPA-AMOUNT-PAID         TO M420C-AMOUNT-PAID.
CSOMOD     MOVE CSO2-COMPANY-NAME       TO M420C-COMPANY-NAME.
CSOMOD     MOVE 'P.O. BOX 34350   OMAHA, NE  68134'
CSOMOD                                  TO M420C-CSO-ADDRESS.
CSOMOD     MOVE CSO5-CLAIM-NO           TO M420C-CLAIM-NO.
CSOMOD     MOVE CSO5-CERT-NO            TO M420C-CERT-NO.
CSOMOD     MOVE CSO5-PLAN-CODE          TO M420C-PLAN-CODE.
CSOMOD     MOVE CSO5-PAID-FROM-DATE     TO M420C-PAID-FROM-DATE.
CSOMOD     MOVE CSO5-PAID-THRU-DATE     TO M420C-PAID-THRU-DATE.
CSOMOD     MOVE CSO5-PAYMENT-TYPE       TO M420C-PAYMENT-TYPE.
CSOMOD     MOVE CSO5-ACCT-NO            TO M420C-ACCT-NO.
CSOMOD     MOVE CSO7-CC-ACCT            TO M420C-CC-ACCT.
CSOMOD     MOVE CSO7-CC-ACCT-NUMBER     TO M420C-CC-ACCT-NUMBER.
CSOMOD     MOVE CSO7-TYPE-MESSAGE       TO M420C-TYPE-MESSAGE.
052614     IF CPA-CLAIM-TYPE = 'A' OR 'I' OR 'G' OR 'F'
080322                                  OR 'B' OR 'H'
CSODAN        MOVE 'NOTICE TO INSURED:' TO M420C-FINAL-MESS9
CSODAN     ELSE
CSODAN        MOVE SPACES               TO M420C-FINAL-MESS9
CSODAN     END-IF
CSODAN     MOVE SPACES                  TO M420C-FINAL-MESS10
CSODAN     MOVE SPACES                  TO M420C-FINAL-MESS11
CSOMOD     MOVE CSO11-MEMBER-NAME       TO M420C-MEMBER-NAME.
CSOMOD     MOVE CPA-INSURED-ADDRESS-LINE1 TO M420C-MEMBER-ADDRESS1.
CSOMOD     MOVE CPA-INSURED-ADDRESS-LINE2 TO M420C-MEMBER-ADDRESS2.
CSOMOD     MOVE CPA-INSURED-ADDRESS-LINE3 TO M420C-MEMBER-ADDRESS3.
CSOMOD     MOVE CPA-INSURED-CITY-STATE    TO M420C-MEMBER-ADDRESS4.
CSOMOD     MOVE CPA-INSURED-ZIP-CODE      TO CSO-ZIP.
CSOMOD     MOVE CSO-ZIP                   TO M420C-MEMBER-ZIP-CODE.
CSOMOD     MOVE CSO17-3RD-NAME          TO M420C-3RDADD-NAME.
CSOMOD     MOVE CSO18-3RDADD-LINE1      TO M420C-3RDADD-LINE1.
CSOMOD     MOVE CSO19-3RDADD-LINE2      TO M420C-3RDADD-LINE2.
CSOMOD     MOVE CSO20-3RD-CITY-STATE    TO M420C-3RDADD-LINE3.
CSOMOD     MOVE CSO21-3RD-ZIP           TO M420C-3RDADD-ZIP.
CSOMOD     MOVE CSO31-CHECK-DATE        TO M420C-CHECK-DATE.
LGC107     MOVE CSO-CHECK-PRINT-LINE-33 TO M420C-DFT-NOTES1.
LGC107     MOVE CSO-CHECK-PRINT-LINE-34 TO M420C-DFT-NOTES2.
CSOMOD     MOVE CSO35-PAYEE-NAME        TO M420C-PAYEE-NAME.
CSOMOD     MOVE CPA-PAYEE-ADDRESS-LINE1 TO M420C-PAYEE-ADDRESS1.
CSOMOD     MOVE CPA-PAYEE-ADDRESS-LINE2 TO M420C-PAYEE-ADDRESS2.
CSOMOD     MOVE CPA-PAYEE-ADDRESS-LINE3 TO M420C-PAYEE-ADDRESS3.
CSOMOD     MOVE CPA-PAYEE-CITY-STATE    TO M420C-PAYEE-ADDRESS4.
CSOMOD     MOVE CPA-PAYEE-ZIP-CODE      TO CSO-ZIP.
CSOMOD     MOVE CSO-ZIP                 TO M420C-PAYEE-ZIP-CODE.
CSOMOD     MOVE CSO23-REPLY-DT          TO M420C-REPLY-DATE.
CSOMOD********
CSOMOD* USE A IF YOU WANT BILL KIZER, AUTHORIZED SIGNATURE OR
CSOMOD* B IF YOU WANT BILL KIZER, PRESIDENT
CSOMOD********
LGC121     IF M420C-AMOUNT-PAID > 100000.00
CSOCAS       MOVE 'C'                   TO M420C-SIGNATURE
CSOCAS     ELSE
CSOCAS       MOVE 'B'                   TO M420C-SIGNATURE.
CSODAN     PERFORM FORMAT-DRAFT-MESSAGE
CSODAN        THRU FORMAT-DRAFT-MESSAGE-EXIT
121903     MOVE CPA-STATE               TO M420C-ACCT-STATE.
121903     MOVE CPA-CLAIM-TYPE          TO M420C-CLAIM-TYPE.
CSOMOD     
      * EXEC CICS WRITE FILE('MICRDRFT') FROM(CSO-DRAFT-420C)
121903*       LENGTH(1254) RESP(CSO-RESP) RIDFLD(CSO-DRAFT-KEY)
CSOMOD*       END-EXEC.
           MOVE 'MICRDRFT' TO DFHEIV1
           MOVE 1254
             TO DFHEIV11
      *    MOVE '&$ L                  ''  N#00002007' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303032303037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CSO-DRAFT-420C, 
                 DFHEIV11, 
                 CSO-DRAFT-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO CSO-RESP
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
CSOMOD     GO TO 0100-MAIN-LOGIC.
           EJECT
       5000-MAIN-LOGIC.
           
      * EXEC CICS DELAY
      *        INTERVAL (WS-DELAY-INTERVAL)
      *    END-EXEC
      *    MOVE '0$I                   &   #00002013' TO DFHEIV0
           MOVE X'302449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032303133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-DELAY-INTERVAL, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           GO TO 0100-MAIN-LOGIC.
       8500-DATE-CONVERSION SECTION.
           
      * EXEC CICS LINK
LGC007*        PROGRAM  ('ELDATCV')
      *        COMMAREA (DATE-CONVERSION-DATA)
      *        LENGTH   (DC-COMM-LENGTH) END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00002018' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032303138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       8500-EXIT.
           EXIT.
           EJECT
CSODAN FORMAT-DRAFT-MESSAGE SECTION.
           MOVE SPACE TO M420C-DRAFT-MESSAGES
030612     IF PI-COMPANY-ID = 'CID' OR 'AHL'
100518     IF CPA-CLAIM-TYPE = ('L' OR 'O') AND CPA-PAYMENT-TYPE = '2'
              MOVE 420C-MSG4-LINE(1) TO M420C-DRAFT-MESSAGE(1)
              MOVE 420C-MSG4-LINE(2) TO M420C-DRAFT-MESSAGE(2)
              GO TO FORMAT-DRAFT-MESSAGE-EXIT
           END-IF
100518     IF CPA-CLAIM-TYPE = ('L' OR 'O') AND CPA-PAYMENT-TYPE = '4'
              MOVE 420C-MSG5-LINE(1) TO M420C-DRAFT-MESSAGE(1)
              MOVE 420C-MSG5-LINE(2) TO M420C-DRAFT-MESSAGE(2)
              MOVE 420C-MSG5-LINE(3) TO M420C-DRAFT-MESSAGE(3)
              MOVE 420C-MSG5-LINE(4) TO M420C-DRAFT-MESSAGE(4)
              MOVE 420C-MSG5-LINE(5) TO M420C-DRAFT-MESSAGE(5)
              GO TO FORMAT-DRAFT-MESSAGE-EXIT
           END-IF
052614     IF (CPA-CLAIM-TYPE = 'A' OR 'I' OR 'G' OR 'F'
080322                                  OR 'B' OR 'H' )
              AND CPA-PAYMENT-ORIGIN = '2'
              AND CPA-PAYMENT-TYPE = '1'
              MOVE 420C-MSG3-LINE(1) TO M420C-DRAFT-MESSAGE(1)
              MOVE 420C-MSG3-LINE(2) TO M420C-DRAFT-MESSAGE(2)
              MOVE 420C-MSG3-LINE(3) TO M420C-DRAFT-MESSAGE(3)
              GO TO FORMAT-DRAFT-MESSAGE-EXIT
           END-IF
052614     IF (CPA-CLAIM-TYPE = 'A' OR 'I' OR 'G' OR 'F'
080322                                  OR 'B' OR 'H' )
              AND CPA-PAYMENT-ORIGIN = '2'
              AND CPA-PAYMENT-TYPE = '2'
              MOVE 420C-MSG1-LINE(1) TO M420C-DRAFT-MESSAGE(1)
              MOVE 420C-MSG1-LINE(2) TO M420C-DRAFT-MESSAGE(2)
              MOVE 420C-MSG1-LINE(3) TO M420C-DRAFT-MESSAGE(3)
              MOVE CSO23-REPLY-DT TO 420C-MSG1-LINE(4)(22:5)
              MOVE 420C-MSG1-LINE(4) TO M420C-DRAFT-MESSAGE(4)
              MOVE 420C-MSG1-LINE(5) TO M420C-DRAFT-MESSAGE(5)
              GO TO FORMAT-DRAFT-MESSAGE-EXIT
           END-IF
052614     IF (CPA-CLAIM-TYPE = 'A' OR 'I' OR 'G' OR 'F'
080322                              OR 'B' OR 'H' )
              AND CPA-PAYMENT-TYPE = '1'
              MOVE 420C-MSG1-LINE(1) TO M420C-DRAFT-MESSAGE(1)
              MOVE 420C-MSG1-LINE(2) TO M420C-DRAFT-MESSAGE(2)
              MOVE 420C-MSG1-LINE(3) TO M420C-DRAFT-MESSAGE(3)
              MOVE CSO23-REPLY-DT TO 420C-MSG1-LINE(4)(22:5)
              MOVE 420C-MSG1-LINE(4) TO M420C-DRAFT-MESSAGE(4)
              MOVE 420C-MSG1-LINE(5) TO M420C-DRAFT-MESSAGE(5)
              GO TO FORMAT-DRAFT-MESSAGE-EXIT
           END-IF
052614     IF (CPA-CLAIM-TYPE = 'A' OR 'I' OR 'G' OR 'F'
080322                              OR 'B' OR 'H' )
              AND CPA-PAYMENT-TYPE = '2'
              MOVE 420C-MSG2-LINE(1) TO M420C-DRAFT-MESSAGE(1)
              MOVE 420C-MSG2-LINE(2) TO M420C-DRAFT-MESSAGE(2)
              MOVE 420C-MSG2-LINE(3) TO M420C-DRAFT-MESSAGE(3)
              MOVE 420C-MSG2-LINE(4) TO M420C-DRAFT-MESSAGE(4)
              GO TO FORMAT-DRAFT-MESSAGE-EXIT
           END-IF
           ELSE
           IF CPA-CLAIM-TYPE = 'L' AND CPA-PAYMENT-TYPE = '2'
              MOVE DCC1-MSG4-LINE(1) TO M420C-DRAFT-MESSAGE(1)
              MOVE DCC1-MSG4-LINE(2) TO M420C-DRAFT-MESSAGE(2)
              GO TO FORMAT-DRAFT-MESSAGE-EXIT
           END-IF
           IF CPA-CLAIM-TYPE = 'L' AND CPA-PAYMENT-TYPE = '4'
              MOVE DCC1-MSG5-LINE(1) TO M420C-DRAFT-MESSAGE(1)
              MOVE DCC1-MSG5-LINE(2) TO M420C-DRAFT-MESSAGE(2)
              MOVE DCC1-MSG5-LINE(3) TO M420C-DRAFT-MESSAGE(3)
              GO TO FORMAT-DRAFT-MESSAGE-EXIT
           END-IF
052614     IF (CPA-CLAIM-TYPE = 'A' OR 'I' OR 'G' OR 'F'
080322                              OR 'B' OR 'H' )
              AND CPA-PAYMENT-ORIGIN = '2'
              AND CPA-PAYMENT-TYPE = '1'
              MOVE DCC1-MSG3-LINE(1) TO M420C-DRAFT-MESSAGE(1)
              MOVE DCC1-MSG3-LINE(2) TO M420C-DRAFT-MESSAGE(2)
              MOVE DCC1-MSG3-LINE(3) TO M420C-DRAFT-MESSAGE(3)
              GO TO FORMAT-DRAFT-MESSAGE-EXIT
           END-IF
052614     IF (CPA-CLAIM-TYPE = 'A' OR 'I' OR 'G' OR 'F'
080322                              OR 'B' OR 'H' )
              AND CPA-PAYMENT-ORIGIN = '2'
              AND CPA-PAYMENT-TYPE = '2'
              MOVE DCC1-MSG1-LINE(1) TO M420C-DRAFT-MESSAGE(1)
              MOVE DCC1-MSG1-LINE(2) TO M420C-DRAFT-MESSAGE(2)
              MOVE DCC1-MSG1-LINE(3) TO M420C-DRAFT-MESSAGE(3)
              MOVE CSO23-REPLY-DT TO DCC1-MSG1-LINE(5)(54:5)
              MOVE DCC1-MSG1-LINE(4) TO M420C-DRAFT-MESSAGE(4)
              MOVE DCC1-MSG1-LINE(5) TO M420C-DRAFT-MESSAGE(5)
              MOVE DCC1-MSG1-LINE(6) TO M420C-DRAFT-MESSAGE(6)
              GO TO FORMAT-DRAFT-MESSAGE-EXIT
           END-IF
052614     IF (CPA-CLAIM-TYPE = 'A' OR 'I' OR 'G' OR 'F'
080322                              OR 'B' OR 'H' )
              AND CPA-PAYMENT-TYPE = '1'
              MOVE DCC1-MSG1-LINE(1) TO M420C-DRAFT-MESSAGE(1)
              MOVE DCC1-MSG1-LINE(2) TO M420C-DRAFT-MESSAGE(2)
              MOVE DCC1-MSG1-LINE(3) TO M420C-DRAFT-MESSAGE(3)
              MOVE CSO23-REPLY-DT TO DCC1-MSG1-LINE(5)(54:5)
              MOVE DCC1-MSG1-LINE(4) TO M420C-DRAFT-MESSAGE(4)
              MOVE DCC1-MSG1-LINE(5) TO M420C-DRAFT-MESSAGE(5)
              MOVE DCC1-MSG1-LINE(6) TO M420C-DRAFT-MESSAGE(6)
              GO TO FORMAT-DRAFT-MESSAGE-EXIT
           END-IF
052614     IF (CPA-CLAIM-TYPE = 'A' OR 'I' OR 'G' OR 'F'
080322                              OR 'B' OR 'H' )
              AND CPA-PAYMENT-TYPE = '2'
              MOVE DCC1-MSG2-LINE(1) TO M420C-DRAFT-MESSAGE(1)
              MOVE DCC1-MSG2-LINE(2) TO M420C-DRAFT-MESSAGE(2)
              MOVE DCC1-MSG2-LINE(3) TO M420C-DRAFT-MESSAGE(3)
              MOVE DCC1-MSG2-LINE(4) TO M420C-DRAFT-MESSAGE(4)
              GO TO FORMAT-DRAFT-MESSAGE-EXIT
           END-IF
           END-IF
           .
CSODAN FORMAT-DRAFT-MESSAGE-EXIT.
CSODAN     EXIT.
           EJECT
LGC007 SPELL-DOLLAR SECTION. 
      *                      COPY ELC176P1.
00001 *****************************************************************
00002 *                                                               *
00003 *                            ELC176P1.                          *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                         *
00006 *                                                               *
00007 * THIS SECTION CONVERTS A DOLLAR FIGURE INTO A SPELLED OUT AMT. *
00002 *                                                               *
00008 *****************************************************************
00009
00010  SDS-010.
00011      MOVE SPACES                 TO  WS-SPELLED-AMOUNT
00012                                      SD-PASS-SPELLED-AMOUNT
00013                                      WS-SPELLED-LINE1
00014                                      WS-SPELLED-LINE2.
00015
00016      SET SA-INDEX TO +1.
00017
00018      MOVE SD-PASS-AMOUNT         TO  WS-AMOUNT.
00019
00020      IF WS-MILLIONS GREATER ZERO
00021          MOVE WS-MILLIONS        TO  WS-AMOUNT-WORK
00022          PERFORM SPELL-AMOUNT
00023          MOVE 'MILLION'          TO  WS-WORD
00024          PERFORM MOVE-WORD.
00025
00026      IF WS-THOUSANDS GREATER ZERO
00027          MOVE WS-THOUSANDS       TO  WS-AMOUNT-WORK
00028          PERFORM SPELL-AMOUNT
00029          MOVE 'THOUSAND'         TO  WS-WORD
00030          PERFORM MOVE-WORD.
00031
00032      IF WS-HUNDREDS GREATER ZERO
00033          MOVE WS-HUNDREDS        TO  WS-AMOUNT-WORK
00034          PERFORM SPELL-AMOUNT.
00035
00036      IF WS-AMOUNT LESS +1.00
00037          MOVE 'NO'               TO  WS-WORD
00038          PERFORM MOVE-WORD.
00039
00040      IF WS-CENTS NOT GREATER ZERO
00041          MOVE 'NO'               TO  WS-CENTS-X.
00042
00043      MOVE WS-CENTS-X             TO  WS-PENNEYS.
00044
00045      MOVE WS-DOLLARS-AND-CENTS   TO  WS-WORD.
00046      PERFORM MOVE-WORD.
00047
00048      INSPECT WS-SPELLED-AMOUNT REPLACING ALL '-' BY SPACES.
00049
00050      MOVE WS-SPELLED-AMOUNT      TO  SD-PASS-SPELLED-AMOUNT.
00051
00052      PERFORM MOVE-SPELLED-AMOUNT.
00053
00054  SDS-EXIT.
00055      EXIT.
00056
00057      EJECT
00058  SPELL-AMOUNT SECTION.
00059
00060 *SAS-NOTE.
00061 *
00062 *    NOTE *******************************************************
00063 *         *      THIS SECTION CONVERTS A THREE DIGIT NUMBER     *
00064 *         *  INTO A SPELLED AMOUNT.                             *
00065 *         *******************************************************
00066
00067  SAS-010.
00068      IF WS-HUNDRED GREATER ZERO
00069          SET SINGLE-INDEX        TO  WS-HUNDRED
00070          MOVE WS-SINGLE-DESC (SINGLE-INDEX)  TO  WS-WORD
00071          PERFORM MOVE-WORD
00072          MOVE 'HUNDRED'          TO  WS-WORD
00073          PERFORM MOVE-WORD.
00074
00075      IF WS-TEEN GREATER ZERO
00076          IF WS-TEEN LESS +20
00077              SET SINGLE-INDEX TO WS-TEEN
00078              MOVE WS-SINGLE-DESC (SINGLE-INDEX)  TO  WS-WORD
00079              PERFORM MOVE-WORD
00080            ELSE
00081              SET UPPER-INDEX TO WS-TEN
00082              MOVE WS-UPPER-DESC (UPPER-INDEX)  TO  WS-WORD
00083              PERFORM MOVE-WORD
00084              IF WS-ONE GREATER ZERO
00085                  SET SINGLE-INDEX TO WS-ONE
00086                  MOVE WS-SINGLE-DESC (SINGLE-INDEX)  TO  WS-WORD
00087                  PERFORM MOVE-WORD.
00088
00089  SAS-EXIT.
00090      EXIT.
00091
00092      EJECT
00093  MOVE-WORD SECTION.
00094
00095 *MWS-NOTE.
00096 *
00097 *    NOTE *******************************************************
00098 *         *      THIS SECTION MOVES ONE WORD TO THE SPELLED     *
00099 *         *  AMOUNT OUTPUT LINE.                                *
00100 *         *******************************************************.
00101
00102  MWD-010.
00103      PERFORM MOVE-CHARACTERS
00104          VARYING CHAR-INDEX FROM +1 BY +1
00105              UNTIL WS-CHAR2 (CHAR-INDEX) = SPACES.
00106
00107      SET SA-INDEX UP BY +1.
00108
00109  MWD-EXIT.
00110      EXIT.
00111
00112  MOVE-CHARACTERS SECTION.
00113
00114 *MCS-NOTE.
00115 *
00116 *    NOTE *******************************************************
00117 *         *      THIS SECTION MOVES ONE CHARACTER TO THE SPELLED*
00118 *         *  AMOUNT OUTPUT LINE.                                *
00119 *         *******************************************************.
00120
00121  MCD-010.
00122      MOVE WS-CHAR2 (CHAR-INDEX)  TO  WS-CHAR (SA-INDEX).
00123
00124      SET SA-INDEX UP BY +1.
00125
00126  MCD-EXIT.
00127      EXIT.
00128
00129      EJECT
00130  MOVE-SPELLED-AMOUNT SECTION.
00131
00132 *MSA-NOTE.
00133 *
00134 *    NOTE *******************************************************
00135 *         *      THIS SECTION MOVES THE SPELLED DOLLAR AMOUNT   *
00136 *         *  TO TWO LINES IF NECESSARY.                         *
00137 *         *******************************************************.
00138
00139  MSA-010.
00140      ADD WS-1ST-LINE-LENGTH +1 GIVING WS-1ST-LINE-LENGTH-PLUS-1.
00141      ADD WS-1ST-LINE-LENGTH +2 GIVING WS-1ST-LINE-LENGTH-PLUS-2.
00142      ADD WS-1ST-LINE-LENGTH -1 GIVING WS-1ST-LINE-LENGTH-MINUS-1.
00143
00144      MOVE WS-SPELLED-AMOUNT  TO  WS-SPELLED-LINE1.
00145
00146      IF SA-INDEX GREATER WS-1ST-LINE-LENGTH-PLUS-1
00147          SET SL2-INDEX TO +1
00148          IF WS-CHAR (WS-1ST-LINE-LENGTH-PLUS-1) = SPACES
00149              PERFORM MOVE-LINE2 VARYING SA-INDEX2
00150                FROM WS-1ST-LINE-LENGTH-PLUS-2 BY +1
00151                  UNTIL SL2-INDEX GREATER WS-2ND-LINE-LENGTH
00152            ELSE
00153              PERFORM CLEAR-LINE1 VARYING SL1-INDEX
00154                  FROM WS-1ST-LINE-LENGTH BY -1
00155                      UNTIL WS-SL1 (SL1-INDEX) = SPACES
00156              SET SL1-INDEX UP BY +1
00157              PERFORM MOVE-LINE2
00158                VARYING SA-INDEX2 FROM SL1-INDEX BY +1
00159                  UNTIL SL2-INDEX GREATER WS-2ND-LINE-LENGTH.
00160
00161      MOVE WS-SPELLED-LINE1       TO  SD-PSA-LINE1.
00162      MOVE WS-SPELLED-LINE2       TO  SD-PSA-LINE2.
00163
00164  MSA-EXIT.
00165      EXIT.
00166
00167      EJECT
00168  CLEAR-LINE1 SECTION.
00169
00170 *CLS-NOTE.
00171 *
00172 *    NOTE *******************************************************
00173 *         *      THIS SECTION CLEARS THE TRAILING WORD IN THE   *
00174 *         *  SPELLED LINE 1 IF THE AMOUNT IS GREATER THAN 78.   *
00175 *         *******************************************************.
00176
00177  CLS-010.
00178      MOVE SPACES                 TO  WS-SL1 (SL1-INDEX).
00179
00180  CLS-EXIT.
00181      EXIT.
00182
00183  MOVE-LINE2 SECTION.
00184
00185 *MLS-NOTE.
00186 *
00187 *    NOTE *******************************************************
00188 *         *      THIS SECTION MOVES ONE CHARACTER TO THE SPELLED*
00189 *         *  AMOUNT OUTPUT LINE.                                *
00190 *         *******************************************************.
00191
00192  MLS-010.
00193      MOVE WS-CHAR (SA-INDEX2)    TO  WS-SL2 (SL2-INDEX).
00194
00195      SET SL2-INDEX UP BY +1.
00196
00197      IF WS-CHAR (SA-INDEX2)     = SPACES   AND
00198         WS-CHAR (SA-INDEX2 + 1) = SPACES
00199          SET SL2-INDEX TO +99.
00200
00201  MLS-EXIT.
00202      EXIT.
00203
           EJECT
       POS-SPELL-DOLLAR SECTION.
      *SDS-NOTE.
      *
      *    NOTE *******************************************************
      *         *                                                     *
      *         *      THIS SECTION CONVERTS A DOLLAR FIGURE INTO A   *
      *         *  SPELLED OUT AMOUNT.                                *
      *         *                                                     *
      *         *******************************************************.
       SDS-010.
           MOVE SPACES                 TO  WS-SPELLED-AMOUNT
                                           SD-PASS-SPELLED-AMOUNT
                                           WS-SPELLED-LINE1
                                           WS-SPELLED-LINE2
           MOVE ZERO                   TO  WS-SW
           SET SA-INDEX TO +1
           MOVE SD-PASS-AMOUNT         TO  WS-AMOUNT
           IF WS-MILLIONS IS GREATER THAN ZERO
               MOVE WS-MILLIONS        TO  WS-AMOUNT-WORK
               PERFORM POS-SPELL-AMOUNT
               MOVE +1                 TO  WS-SW.
           IF WS-THOUSANDS IS GREATER THAN ZERO
               MOVE WS-THOUSANDS       TO  WS-AMOUNT-WORK
               PERFORM POS-SPELL-AMOUNT
               MOVE +1                 TO  WS-SW
             ELSE
               IF WS-MILLIONS IS GREATER THAN ZERO
                   MOVE 'ZERO'         TO  WS-WORD
                   PERFORM MOVE-WORD 3 TIMES.
           IF WS-HUNDREDS IS GREATER THAN ZERO
               MOVE WS-HUNDREDS        TO  WS-AMOUNT-WORK
               PERFORM POS-SPELL-AMOUNT
               MOVE +1                 TO  WS-SW
             ELSE
               IF WS-MILLIONS IS GREATER THAN ZERO
                 OR WS-THOUSANDS IS GREATER THAN ZERO
                   MOVE 'ZERO'         TO  WS-WORD
                   PERFORM MOVE-WORD 3 TIMES.
           IF WS-AMOUNT IS LESS THAN +1.00
               MOVE 'NO'               TO  WS-WORD
               PERFORM MOVE-WORD.
           IF WS-CENTS IS NOT GREATER THAN ZERO
               MOVE 'NO'               TO  WS-CENTS-X.
           MOVE WS-CENTS-X             TO  WS-PENNEYS
           MOVE WS-DOLLARS-AND-CENTS   TO  WS-WORD
           PERFORM MOVE-WORD
CIDMOD*    TRANSFORM WS-SPELLED-AMOUNT FROM '-' TO SPACES
           INSPECT WS-SPELLED-AMOUNT REPLACING ALL '-' BY ' '.
           MOVE WS-SPELLED-AMOUNT      TO  SD-PASS-SPELLED-AMOUNT
           PERFORM MOVE-SPELLED-AMOUNT.
       SDS-EXIT.
           EXIT.
           EJECT
       POS-SPELL-AMOUNT SECTION.
      *SAS-NOTE.
      *
      *    NOTE *******************************************************
      *         *                                                     *
      *         *      THIS SECTION CONVERTS A THREE DIGIT NUMBER     *
      *         *  INTO A SPELLED AMOUNT.                             *
      *         *                                                     *
      *         *                                                     *
      *         *******************************************************.
       SAS-010.
           IF WS-HUNDRED IS GREATER THAN ZERO
               SET SINGLE-INDEX        TO  WS-HUNDRED
               MOVE WS-SINGLE-DESC (SINGLE-INDEX)  TO  WS-WORD
               PERFORM MOVE-WORD
             ELSE
               IF WS-SW NOT EQUAL TO ZERO
                   MOVE 'ZERO'         TO  WS-WORD
                   PERFORM MOVE-WORD.
           IF WS-TEN IS GREATER THAN ZERO
               SET SINGLE-INDEX TO WS-TEN
               MOVE WS-SINGLE-DESC (SINGLE-INDEX)  TO  WS-WORD
               PERFORM MOVE-WORD
             ELSE
               IF WS-HUNDRED IS GREATER THAN ZERO
                 OR WS-SW NOT EQUAL TO ZERO
                   MOVE 'ZERO'         TO  WS-WORD
                   PERFORM MOVE-WORD.
           IF WS-ONE IS GREATER THAN ZERO
               SET SINGLE-INDEX TO WS-ONE
               MOVE WS-SINGLE-DESC (SINGLE-INDEX)  TO  WS-WORD
               PERFORM MOVE-WORD
             ELSE
               IF WS-HUNDRED IS GREATER THAN ZERO
                 OR WS-TEN IS GREATER THAN ZERO
                 OR WS-SW NOT EQUAL TO ZERO
                   MOVE 'ZERO'         TO  WS-WORD
                   PERFORM MOVE-WORD.
       SAS-EXIT.
           EXIT.
JGEND      EJECT
JGEND  9990-ERROR SECTION.
JGEND
JGEND
JGEND      
      * EXEC CICS LINK
LGC007*        PROGRAM  ('EL004')
JGEND *        COMMAREA (DFHEIBLK)
JGEND *        LENGTH   (64) END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 64
             TO DFHEIV11
      *    MOVE '."C                   (   #00002446' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIBLK, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
JGEND
JGEND
JGEND  9990-EXIT.
JGEND
JGEND      EXIT.
JGEND
JGEND      SKIP3
JGEND  9999-LAST-PARAGRAPH SECTION.
JGEND
JGEND      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL177' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
JGEND

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL177' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 5000-MAIN-LOGIC,
                     5000-MAIN-LOGIC,
                     9990-ERROR
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL177' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
