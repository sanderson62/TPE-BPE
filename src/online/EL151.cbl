00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL151 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 06/06/95 13:08:11.
00007 *                            VMOD=2.012.
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
050506******************************************************************
050506*                   C H A N G E   L O G
050506*
050506* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
050506*-----------------------------------------------------------------
050506*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
050506* EFFECTIVE    NUMBER
050506*-----------------------------------------------------------------
050506* 050506    2006030600001  AJRA  ADD PROOF DATE TO DENIAL SCREEN
120808* 120808    2008100900001  PEMA  ADD FOR DENIAL CODE
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
043019* 043019  IR2019042300001  PEMA  DISALLOW PROOF DT > REC DT OR < I
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
050506******************************************************************
00024 *REMARKS.
00025
00026 *    SCREENS     - EL151S - DENIALS
00027
00028 *    ENTERED BY  - EL150 - STATUS AND DISPOSITION
00029
00030 *    EXIT TO     - EL150 - CALLING PROGRAM
00031
00032 *    INPUT FILE  - ELMSTR - CLAIM MASTER
00033 *                - ELTRLR - ACTIVITY TRAILERS
00034 *                - ERNOTE - CERTIFICATE NOTES
00035
00036 *    OUTPUT FILE - ELMSTR - CLAIM MASTER
00037 *                - ELTRLR - ACTIVITY TRAILERS
00038
00039 *    COMMAREA    - PASSED
00040
00041 *    ERROR-CODES ACCESSED - 132,  133, 270, 137, 29, 315, 008, 50
00042
00043 *    NARRATIVE   - 1- PROVIDE CREATION OF DENIAL TRAILERS
00044 *                  2- UPDATE CLAIM STATUS TYPE TO CLOSED (ELMSTR)
00045 *                  3- UPDATE RESERVE-EXPENSE TRAILER STATUS TO
00046 *                     CLOSED, REASON DENIAL
00047
00048      EJECT
00049  ENVIRONMENT DIVISION.
00050
00051  DATA DIVISION.
00052
00053  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00054
00055  77  FILLER  PIC X(32)  VALUE '********************************'.
00056  77  FILLER  PIC X(32)  VALUE '*   EL151  WORKING STORAGE     *'.
00057  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.012 ********'.
00058
00059 *                            COPY ELCSCTM.
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
00060
00061 *                            COPY ELCSCRTY.
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
00062
00063 *                            COPY ELCNWA.
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
00064
00065  01  WS-DATE-AREA.
00066      05  SAVE-DATE           PIC X(8)    VALUE SPACES.
00067      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.
CIDMOD     05  WS-BLANK            PIC X       VALUE ' '.
00068
00069  01  WS-SCRATCH-AREA.
00070      05  GETMAIN-SPACE               PIC X     VALUE SPACE.
00071      05  WS-TRLR-LENGTH              PIC S9(4) VALUE +200  COMP.
00072      05  WS-DMO-LENGTH               PIC S9(4) VALUE +108  COMP.
00073      05  WS-DCT-LENGTH               PIC S9(4) VALUE +53   COMP.
00074      05  SC-ITEM                     PIC S9(4) VALUE +0001 COMP.
00075
00076      05  WS-TRANS-ID                 PIC X(4)   VALUE 'EX32'.
00077      05  WS-SEQ-NO-SAVED             PIC S9(4)   VALUE ZEROS.
00078
00079      05  WS-ORIG-SEQ-CNT             PIC S9(4)   VALUE ZEROS.
00080
00081      05  TR-SUB                      PIC S9(4)   VALUE ZERO  COMP.
00082      05  TR-SUB-2                    PIC S9(4)   VALUE ZERO  COMP.
00083      05  MISC-SUB                    PIC S9(03)  COMP-3  VALUE +0.
00084
00085      05  MAP-NAME                    PIC X(08)   VALUE 'EL151A'.
00086      05  MAPSET-NAME                 PIC X(08)   VALUE 'EL151S'.
00087
00088      05  ELTRLR-FILE-ID              PIC X(08)   VALUE 'ELTRLR'.
00089      05  ELMSTR-FILE-ID              PIC X(08)   VALUE 'ELMSTR'.
00090      05  ELCNTL-FILE-ID              PIC X(08)   VALUE 'ELCNTL'.
00091      05  ELARCH-FILE-ID              PIC X(08)   VALUE 'ELARCH'.
00092
00093      05  THIS-PGM                    PIC X(08)   VALUE  'EL151'.
00094
00095      05  WS-OC-LOAD-SW               PIC X(01)   VALUE SPACE.
00096          88  OC-HISTORY-LOADED       VALUE 'X'.
00097
00098      05  WS-CLAIM-SEQ-SW             PIC X(01)   VALUE SPACE.
00099          88  NO-SEQ-NUMBER           VALUE 'X'.
00100          88  SEQ-NUMBER-EXIST        VALUE ' '.
00101          88  NO-ACCT-TRLR            VALUE 'Y'.
00102
00103      05  WS-RESET-SW                 PIC X(01)   VALUE 'N'.
00104      05  WS-REC-FOUND-SW             PIC X(01)   VALUE 'N'.
00105      05  WS-BROWSE-SW                PIC X(01)   VALUE 'N'.
00106      05  WS-UPDATE-SW                PIC X(01)   VALUE 'N'.
00107
00108      05  TIME-IN                     PIC S9(7).
00109      05  TIME-OUT-R REDEFINES TIME-IN.
00110          10  FILLER                  PIC X(01).
00111          10  TIME-OUT                PIC 99V99.
00112          10  FILLER                  PIC X(02).
050506
050505     05  DEEDIT-FIELD                PIC X(15).
050506     05  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD   PIC S9(15).
050506
050506     05  WS-PRF-DT                   PIC X(2).
050506     05  WS-MAX-LETTER-ANSWER-DT     PIC X(2)  VALUE LOW-VALUES.
00113
00114      EJECT
00115      05  DENIAL-TABLE.
00116          10  FILLER                  PIC X(44)    VALUE
00117              'DPE  PRE-EXISTING DISABILITY                '.
00118          10  FILLER                  PIC X(44)    VALUE
00119              'DDN  DISABILITY NOT COVERED DUE TO NATURE   '.
00120          10  FILLER                  PIC X(44)    VALUE
00121              'DIR  INSURED RECOVERED FROM DISABILITY      '.
00122          10  FILLER                  PIC X(44)    VALUE
00123              'DEM  EMPLOYMENT INDICATES OCCUPATION NOT MET'.
00124          10  FILLER                  PIC X(44)    VALUE
00125              'DWP  DISABILITY WAITING PERIOD NOT SATISFIED'.
00126          10  FILLER                  PIC X(44)    VALUE
00127              'DNC  NO COVERAGE                            '.
00128          10  FILLER                  PIC X(44)    VALUE
00129              'DAP  APPLICATION MISREPRESENTATION          '.
00130          10  FILLER                  PIC X(44)    VALUE
00131              'DMA  MIS-STATEMENT OF AGE                   '.
00132          10  FILLER                  PIC X(44)    VALUE
00133              'DSU  SUICIDE OR SELF-INFLICTED INJURY       '.
00134          10  FILLER                  PIC X(44)    VALUE
00135              'DOT  OTHER                                  '.
00136          10  FILLER                  PIC X(44)    VALUE
00137              'DAR  AWARE OF UNEMPLOY./RED. AT INCEPTION   '.
00138          10  FILLER                  PIC X(44)    VALUE
00139              'DNE  NOT EMPLOYED SIX MONTHS PRIOR          '.
00140          10  FILLER                  PIC X(44)    VALUE
00141              'DNR  NOT MADE REDUNDANT/NOT UNEMPLOYED      '.
00142          10  FILLER                  PIC X(44)    VALUE
00143              'DSE  SELF EMPLOYED                          '.
00144          10  FILLER                  PIC X(44)    VALUE
00145              'DTW  TEMPORARY WORK                         '.
00146          10  FILLER                  PIC X(44)    VALUE
00147              'LAP  APPLICATION MISREPRESENTATION          '.
00148          10  FILLER                  PIC X(44)    VALUE
00149              'LMA  MIS-STATEMENT OF AGE                   '.
00150          10  FILLER                  PIC X(44)    VALUE
00151              'LNC  NO COVERAGE                            '.
00152          10  FILLER                  PIC X(44)    VALUE
00153              'LOT  OTHER                                  '.
00154      05  DEN-TABLE REDEFINES DENIAL-TABLE OCCURS 19
00155          INDEXED BY DEN-INDEX.
00156          10  DENIAL-CODE.
00157              15  DENIAL-1-1          PIC X(01).
00158              15  DENIAL-2-3          PIC X(02).
00159              15  DENIAL-4-4          PIC X(01).
00160          10  FILLER                  PIC X(01).
00161          10  DENIAL-DESC             PIC X(39).
00162
00163      EJECT
00164  01  ACCESS-KEYS.
00165      05  WS-TRAILER-KEY.
00166          10  WS-CLAIM-KEY.
00167              15  WS-KEY-COMPANY-CD       PIC X.
00168              15  WS-KEY-CARRIER          PIC X.
00169              15  WS-KEY-CLAIM-NO         PIC X(7).
00170              15  WS-KEY-CERT-NO.
00171                  20  WS-KEY-CERT-PRIME   PIC X(10).
00172                  20  WS-KEY-CERT-SFX     PIC X.
00173          10  WS-KEY-SEQUENCE-NO      PIC   S9(4) COMP.
00174
00175      05  ELARCH-KEY.
00176          10  ELARCH-COMPANY-CD   PIC X(01).
00177          10  ELARCH-ARCHIVE-NO   PIC S9(08)  COMP.
00178          10  ELARCH-RECORD-TYPE  PIC X(01).
00179          10  ELARCH-SEQ-NO       PIC S9(04)  COMP.
00180
120808     05  WS-ELDENY-KEY.
               10  ELDENY-COMPANY-CD   PIC X.
               10  ELDENY-DENIAL-CODE  PIC X(4).
120808         10  FILLER              PIC X(10).
00181      05  ELCNTL-KEY.
00182          10  ELCNTL-COMPANY-ID   PIC X(03).
00183          10  ELCNTL-RECORD-TYPE  PIC X(01).
00184          10  ELCNTL-ACCESS       PIC X(04).
00185          10  ELCNTL-SEQ-NO       PIC S9(04)  COMP.
00186
00187      05  W-NOTE-KEY.
00188          10  W-NOTE-COMP-CD      PIC X.
00189          10  W-NOTE-CERT-KEY.
00190              16  W-NOTE-CARRIER  PIC X.
00191              16  W-NOTE-GROUPING PIC X(6).
00192              16  W-NOTE-STATE    PIC XX.
00193              16  W-NOTE-ACCOUNT  PIC X(10).
00194              16  W-NOTE-EFF-DT   PIC XX.
00195              16  W-NOTE-CERT-NO  PIC X(11).
00196
120808 01  WS-RESPONSE             PIC S9(8)   COMP.
           88  RESP-NORMAL              VALUE +00.
           88  RESP-ERROR               VALUE +01.
           88  RESP-NOTFND              VALUE +13.
           88  RESP-NOTOPEN             VALUE +19.
120808     88  RESP-ENDFILE             VALUE +20.
00197  01  ERROR-MESSAGES.
00198      12  ER-0000                 PIC X(4)  VALUE '0000'.
00199      12  ER-0004                 PIC X(4)  VALUE '0004'.
00200      12  ER-0008                 PIC X(4)  VALUE '0008'.
050506     12  ER-0021                 PIC X(4)  VALUE '0021'.
00201      12  ER-0029                 PIC X(4)  VALUE '0029'.
00202      12  ER-0050                 PIC X(4)  VALUE '0050'.
00203      12  ER-0070                 PIC X(4)  VALUE '0070'.
00204      12  ER-0132                 PIC X(4)  VALUE '0132'.
00205      12  ER-0133                 PIC X(4)  VALUE '0133'.
00206      12  ER-0137                 PIC X(4)  VALUE '0137'.
00207      12  ER-0154                 PIC X(4)  VALUE '0154'.
00208      12  ER-0172                 PIC X(4)  VALUE '0172'.
00209      12  ER-0270                 PIC X(4)  VALUE '0270'.
00210      12  ER-0483                 PIC X(4)  VALUE '0483'.
050506     12  ER-0872                 PIC X(4)  VALUE '0872'.
050506     12  ER-0873                 PIC X(4)  VALUE '0873'.
00211      12  ER-0884                 PIC X(4)  VALUE '0884'.
00212      12  ER-0919                 PIC X(4)  VALUE '0919'.
00213      12  ER-0921                 PIC X(4)  VALUE '0921'.
00214      12  ER-0946                 PIC X(4)  VALUE '0946'.
00215      12  ER-0947                 PIC X(4)  VALUE '0947'.
00216      12  ER-0948                 PIC X(4)  VALUE '0948'.
00217      12  ER-0949                 PIC X(4)  VALUE '0949'.
00218      12  ER-0950                 PIC X(4)  VALUE '0950'.
00219      12  ER-0951                 PIC X(4)  VALUE '0951'.
00220      12  ER-0954                 PIC X(4)  VALUE '0954'.
00221      12  ER-0974                 PIC X(4)  VALUE '0974'.
00222      12  ER-0975                 PIC X(4)  VALUE '0975'.
00223      12  ER-8051                 PIC X(4)  VALUE '8051'.
00224      12  ER-8052                 PIC X(4)  VALUE '8052'.
00225      12  ER-8053                 PIC X(4)  VALUE '8053'.
00226      12  ER-8054                 PIC X(4)  VALUE '8054'.
00227      12  ER-8055                 PIC X(4)  VALUE '8055'.
00228      12  ER-8056                 PIC X(4)  VALUE '8056'.
00229      12  ER-8057                 PIC X(4)  VALUE '8057'.
00230      12  ER-8058                 PIC X(4)  VALUE '8058'.
00231      12  ER-8059                 PIC X(4)  VALUE '8059'.
00232      12  ER-8060                 PIC X(4)  VALUE '8060'.
00233      12  ER-8061                 PIC X(4)  VALUE '8061'.
00234      12  ER-8062                 PIC X(4)  VALUE '8062'.
00235      12  ER-8063                 PIC X(4)  VALUE '8063'.
00236      12  ER-8064                 PIC X(4)  VALUE '8064'.
00237      12  ER-8065                 PIC X(4)  VALUE '8065'.
00238      12  ER-8066                 PIC X(4)  VALUE '8066'.
00239      12  ER-8152                 PIC X(4)  VALUE '8152'.
00240      12  ER-8153                 PIC X(4)  VALUE '8153'.
00241      12  ER-8154                 PIC X(4)  VALUE '8154'.
00242      12  ER-8155                 PIC X(4)  VALUE '8155'.
00243      EJECT
00244 *                                    COPY ELCAID.
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
00245  01  PF-AID REDEFINES DFHAID.
00246      05  FILLER                      PIC X(8).
00247      05  PF-VALUES  OCCURS 24        PIC X.
00248      EJECT
00249 *                                    COPY ELCINTF.
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
00250      12  PI-REDEFINES REDEFINES PI-PROGRAM-WORK-AREA.
00251          16  PI-PREV-TRLR-KEY        PIC X(22).
050506*00252          16  FILLER                  PIC X(618).
050506         16  PI-PROOF-DATE           PIC 9(6).
050506         16  FILLER                  PIC X(612).
00253
00254 * DLO023
00255  01  DL23-COMM-LENGTH              PIC S9(4) COMP VALUE +132.
00256  01  WS-DLO-CODES-TABLE.
00257      12  DL23-SYSTEM-ID            PIC XX.
00258      12  DL23-RECORD-TYPE          PIC XX.
00259      12  DL23-RECORD-KEY           PIC X(6).
00260      12  DL23-RETURN-CODE          PIC XX.
00261      12  DL23-CODE-DESC.
00262          16  DL23-CODE-TEXT        PIC X(20).
00263          16  DL23-RET-CODE         PIC X(40).
00264      12  DL23-GEN-DESC-1           PIC X(20).
00265      12  DL23-GEN-DESC-2           PIC X(20).
00266      12  DL23-GEN-DESC-3           PIC X(20).
00267
00268  01  DMD-DATE-YYYYMMDD.
00269      12  DMD-DECADE          PIC XX      VALUE SPACES.
00270      12  DMD-YYMMDD.
00271          16  DMD-YY          PIC XX      VALUE SPACES.
00272          16  DMD-MM          PIC XX      VALUE SPACES.
00273          16  DMD-DD          PIC XX      VALUE SPACES.
00274
00275      12  W-NAME-LAST             PIC  X(15).
00276      12  W-NAME-FIRST            PIC  X(15).
00277      12  W-NAME-MIDDLE.
00278          16  FILLER              PIC  X(01).
00279          16  W-NAME-MIDDLE-2     PIC  X(01).
00280          16  FILLER              PIC  X(13).
00281
00282 *                                    COPY ELCDCTB.
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
00283      EJECT
00284 *                                    COPY ELCATTR.
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
00285      EJECT
00286 *                                    COPY ELCLOGOF.
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
00287      EJECT
00288 *                                    COPY ELCDATE.
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
00289      EJECT
00290 *                                    COPY ELCEMIB.
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
00291      EJECT
00292 *                                    COPY EL151S.
       01  EL151AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  MRNDATEL PIC S9(0004) COMP.
           05  MRNDATEF PIC  X(0001).
           05  FILLER REDEFINES MRNDATEF.
               10  MRNDATEA PIC  X(0001).
           05  MRNDATEI PIC  X(0008).
      *    -------------------------------
           05  MRNTIMEL PIC S9(0004) COMP.
           05  MRNTIMEF PIC  X(0001).
           05  FILLER REDEFINES MRNTIMEF.
               10  MRNTIMEA PIC  X(0001).
           05  MRNTIMEI PIC  X(0005).
      *    -------------------------------
           05  MLINE1L PIC S9(0004) COMP.
           05  MLINE1F PIC  X(0001).
           05  FILLER REDEFINES MLINE1F.
               10  MLINE1A PIC  X(0001).
           05  MLINE1I PIC  X(0060).
      *    -------------------------------
           05  MLINE2L PIC S9(0004) COMP.
           05  MLINE2F PIC  X(0001).
           05  FILLER REDEFINES MLINE2F.
               10  MLINE2A PIC  X(0001).
           05  MLINE2I PIC  X(0060).
      *    -------------------------------
           05  MDENCDL PIC S9(0004) COMP.
           05  MDENCDF PIC  X(0001).
           05  FILLER REDEFINES MDENCDF.
               10  MDENCDA PIC  X(0001).
           05  MDENCDI PIC  X(0004).
      *    -------------------------------
           05  MPRFDTL PIC S9(0004) COMP.
           05  MPRFDTF PIC  X(0001).
           05  FILLER REDEFINES MPRFDTF.
               10  MPRFDTA PIC  X(0001).
           05  MPRFDTI PIC  X(0008).
      *    -------------------------------
           05  MERMSG1L PIC S9(0004) COMP.
           05  MERMSG1F PIC  X(0001).
           05  FILLER REDEFINES MERMSG1F.
               10  MERMSG1A PIC  X(0001).
           05  MERMSG1I PIC  X(0070).
      *    -------------------------------
           05  MERMSG2L PIC S9(0004) COMP.
           05  MERMSG2F PIC  X(0001).
           05  FILLER REDEFINES MERMSG2F.
               10  MERMSG2A PIC  X(0001).
           05  MERMSG2I PIC  X(0070).
      *    -------------------------------
           05  MPFNUMBL PIC S9(0004) COMP.
           05  MPFNUMBF PIC  X(0001).
           05  FILLER REDEFINES MPFNUMBF.
               10  MPFNUMBA PIC  X(0001).
           05  MPFNUMBI PIC  99.
       01  EL151AO REDEFINES EL151AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MRNDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MRNTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MLINE1O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MLINE2O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MDENCDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MPRFDTO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MERMSG1O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MERMSG2O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MPFNUMBO PIC  99.
      *    -------------------------------
00293      EJECT
00294 *                                    COPY ELCDMO.
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
00295      EJECT
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
00297  01  DFHCOMMAREA                     PICTURE X(1024).
00298      EJECT
00299 *                                    COPY ELCMSTR.
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
00300      EJECT
00301 *                                    COPY ELCTRLR.
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
00302      EJECT
00303 *                                    COPY ELCARCH.
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
00304      EJECT
00305 *                                    COPY ELCCNTL.
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
120808*                                    COPY ELCDENY.
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
CIDMOD*                                    COPY ELCDAR.
00001 ******************************************************************
00002 *                                                                *
00003 *   FILE DESC. = DAILY ACTIVITY FILE, FOR PROCESSING NITELY      *
00004 *   FILE TYPE = VSAM,KSDS                                        *
00005 *   RECORD SIZE = 25   RECFORM = FIXED                           *
00006 *   BASE CLUSTER = DLYACTV                                       *
00007 *   LOG = YES                                                    *
00008 *   NARRATIVE - FILE IS BUILT DURING DAYTIME CICS PROCESSING AND *
00009 *               IS THEN PROCESSED BY CYCLE PROCESSING AT NIGHT.  *
00010 *               THIS IS USED TO BUILD THE LOGIC "F" EXTRACT      *
00011 *               RECORDS FOR THOSE CLAIMS WHICH HAVE HAD ACTIVITY *
00012 *               DURING THE DAY. THE EXTRACTS THEN GET READ IN    *
00013 *               BY PROGRAM "LGINFCE".                            *
00014 *                                                                *
00015 ******************************************************************
00016  01  DAILY-ACTIVITY-RECORD.
00017      05  DA-KEY.
00018          10  DA-COMP-CD          PIC X.
00019          10  DA-CARRIER          PIC X.
00020          10  DA-CLAIM-NO         PIC X(7).
00021          10  DA-CERT-NO.
00022              15  DA-CERT-PRIME   PIC X(10).
00023              15  DA-CERT-SFX     PIC X.
00024      05  DA-TRAILER-SEQ-NO       PIC S9(4)  COMP.
00025      05  DA-RECORD-TYPE          PIC X.
00026      05  FILLER                  PIC X(2).
00027 ******************************************************************
CIDMOD     EJECT
00307 *                                    COPY ERCDMDNT.
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
00308      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CLAIM-MASTER
                                ACTIVITY-TRAILERS LETTER-ARCHIVE
                                CONTROL-FILE DENIAL-CODES
                                DAILY-ACTIVITY-RECORD
                                CERTIFICATE-NOTE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL151' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00310
00311      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00312      MOVE '5'                    TO DC-OPTION-CODE.
00313      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
00314      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00315      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00316
00317  0001-PROCESSING-EXITS.
00318      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
00319      IF EIBCALEN NOT GREATER THAN ZEROS
00320        GO TO 8800-UNAUTHORIZED-ACCESS.
00321
00322      
      * EXEC CICS  HANDLE CONDITION
00323 *           ERROR    (9990-ABEND)
00324 *           PGMIDERR (9600-PGMID-ERROR)
00325 *    END-EXEC.
      *    MOVE '"$.L                  ! " #00004118' TO DFHEIV0
           MOVE X'22242E4C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303034313138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00326
00327      IF PI-CALLING-PROGRAM  NOT  = THIS-PGM
00328          IF PI-RETURN-TO-PROGRAM NOT  =  THIS-PGM
00329              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00330              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00331              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00332              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00333              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00334              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00335              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00336              MOVE THIS-PGM             TO PI-CALLING-PROGRAM
00337          ELSE
00338              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00339              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00340              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00341              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00342              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00343              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00344              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00345              MOVE SPACES               TO PI-SAVED-PROGRAM-6.
00346
00347      IF EIBAID IS EQUAL TO DFHCLEAR
00348          GO TO 9400-CLEAR.
00349
00350      IF PI-PROCESSOR-ID = 'LGXX'
00351          NEXT SENTENCE
00352      ELSE
00353          
      * EXEC CICS READQ TS
00354 *            QUEUE   (PI-SECURITY-TEMP-STORE-ID)
00355 *            INTO    (SECURITY-CONTROL)
00356 *            LENGTH  (SC-COMM-LENGTH)
00357 *            ITEM    (SC-ITEM)
00358 *        END-EXEC
      *    MOVE '*$II   L              ''   #00004149' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00359          MOVE SC-CLAIMS-DISPLAY (6)    TO  PI-DISPLAY-CAP
00360          MOVE SC-CLAIMS-UPDATE  (6)    TO  PI-MODIFY-CAP.
00361
00362      IF EIBTRNID IS EQUAL TO WS-TRANS-ID
00363          GO TO 0200-RECEIVE.
00364
00365  0100-FIRST-TIME-IN.
00366
00367      MOVE LOW-VALUES                 TO  EL151AO
00368      MOVE -1                         TO  MLINE1L
00369      MOVE 1                          TO  EMI-NUMBER-OF-LINES.
00370
00371      MOVE SPACE                      TO  DC-ERROR-CODE
00372                                          WS-CLAIM-SEQ-SW
00373                                          MERMSG2O.
00374
00375      MOVE '153A'                     TO  PI-CURRENT-SCREEN-NO.
050506
050506     MOVE 0                          TO  PI-PROOF-DATE.
050506     PERFORM 3550-FIND-LETTER-TRLR THRU 3550-EXIT.
00376
00377      GO TO 8100-SEND-INITIAL-MAP.
00378
00379  0200-RECEIVE.
00380
00381      MOVE LOW-VALUES              TO  EL151AI.
00382
00383      IF (EIBAID IS EQUAL TO DFHPA1 OR DFHPA2 OR DFHPA3)
00384          MOVE ER-0008             TO  EMI-ERROR
00385          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00386          MOVE -1                  TO  MPFNUMBL
00387          GO TO 8200-SEND-DATAONLY.
00388
00389      
      * EXEC CICS RECEIVE
00390 *        MAP      (MAP-NAME)
00391 *        MAPSET   (MAPSET-NAME)
00392 *        INTO     (EL151AI)
00393 *    END-EXEC.
           MOVE LENGTH OF
            EL151AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00004188' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL151AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00394
00395      IF MPFNUMBL IS EQUAL TO +0
00396          GO TO 0300-CHECK-PFKEYS.
00397
00398      IF EIBAID IS NOT EQUAL TO DFHENTER
00399          MOVE ER-0004             TO  EMI-ERROR
00400          GO TO 0320-INPUT-ERROR.
00401
00402      IF (MPFNUMBI NUMERIC) AND (MPFNUMBI GREATER 0 AND LESS 25)
00403          MOVE PF-VALUES (MPFNUMBI)    TO  EIBAID
00404      ELSE
00405          MOVE ER-0029                 TO  EMI-ERROR
00406          GO TO 0320-INPUT-ERROR.
00407
00408  0300-CHECK-PFKEYS.
00409
00410      IF EIBAID IS EQUAL TO DFHPF12
00411         MOVE  'EL010'            TO  THIS-PGM
00412         GO TO 9300-XCTL.
00413
00414      IF EIBAID IS EQUAL TO DFHPF23
00415         MOVE EIBAID              TO  PI-ENTRY-CD-1
00416         MOVE 'EL005'             TO  THIS-PGM
00417         GO TO 9300-XCTL.
00418
00419      IF EIBAID IS EQUAL TO DFHPF24
00420         MOVE EIBAID              TO  PI-ENTRY-CD-1
00421         MOVE  'EL126'            TO  THIS-PGM
00422         GO TO 9300-XCTL.
00423
00424      IF EIBAID IS EQUAL TO DFHENTER
00425         GO TO 0330-EDIT-DATA.
00426
00427      MOVE ER-0029                TO  EMI-ERROR.
00428
00429  0320-INPUT-ERROR.
00430      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00431
00432      MOVE -1                     TO  MPFNUMBL.
00433      GO TO 8200-SEND-DATAONLY.
00434
00435      EJECT
00436  0330-EDIT-DATA.
00437      IF NOT MODIFY-CAP
00438         MOVE 'UPDATE'             TO  SM-READ
00439         PERFORM 9995-SECURITY-VIOLATION
00440         MOVE ER-0070              TO  EMI-ERROR
00441         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00442         GO TO 8100-SEND-INITIAL-MAP.
00443
030612     IF PI-COMPANY-ID = 'AIG' OR 'AUK' OR 'CID' OR 'DCC'
062121                     OR 'AHL' or 'VPP' or 'FNL'
00445          IF MLINE1I = (LOW-VALUES OR SPACES) AND
00446             MLINE2I = (LOW-VALUES OR SPACES) AND
00447             MDENCDI = (LOW-VALUES OR SPACES)
00448              MOVE ER-0483        TO  EMI-ERROR
00449              MOVE -1             TO  MLINE1L
00450              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00451              GO TO 8100-SEND-INITIAL-MAP
00452          ELSE
00453              NEXT SENTENCE
00454      ELSE
00455          IF MLINE1I  = (LOW-VALUES OR SPACES)  AND
00456             MLINE2I  = (LOW-VALUES OR SPACES)
00457              MOVE ER-0483        TO  EMI-ERROR
00458              MOVE -1             TO  MLINE1L
00459              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00460              GO TO 8100-SEND-INITIAL-MAP.
050506
050506     IF MPRFDTL > 0
050506         MOVE MPRFDTI TO DEEDIT-FIELD
050506         PERFORM 9650-DEEDIT      THRU 9650-EXIT
050506         MOVE DEEDIT-FIELD-V0  TO PI-PROOF-DATE
050506         MOVE PI-PROOF-DATE    TO MPRFDTO
050506         INSPECT MPRFDTI REPLACING ALL ' ' BY '/'
050506     END-IF.
050506
050506     IF PI-PROOF-DATE = ZEROS OR LOW-VALUES
050506*     IF  MPRFDTI  = LOW-VALUES OR SPACES
050506         MOVE ER-0872             TO EMI-ERROR
050506         MOVE -1                  TO MPRFDTL
050506         MOVE AL-UABON            TO MPRFDTA
050506         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
050506         GO TO 8200-SEND-DATAONLY
050506     END-IF.
050506
050506     IF  PI-PROOF-DATE > 0
050506         MOVE PI-PROOF-DATE       TO DEEDIT-FIELD
050506         PERFORM 9650-DEEDIT      THRU 9650-EXIT
050506         MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY
050506         MOVE '4'                 TO DC-OPTION-CODE
050506         PERFORM 9700-LINK-DATE-CONVERT THRU  9700-EXIT
050506         IF DATE-CONVERSION-ERROR
050506            MOVE ER-0021          TO EMI-ERROR
050506            MOVE -1               TO MPRFDTL
050506            MOVE AL-UABON         TO MPRFDTA
050506            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
050506            GO TO 8200-SEND-DATAONLY
050506         ELSE
050506            IF DC-BIN-DATE-1 > SAVE-BIN-DATE
050506                MOVE ER-0873      TO EMI-ERROR
050506                MOVE -1           TO MPRFDTL
050506                MOVE AL-UABON     TO MPRFDTA
050506                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
050506                GO TO 8200-SEND-DATAONLY
050506            ELSE
050506                MOVE DC-BIN-DATE-1    TO WS-PRF-DT
050506                MOVE DEEDIT-FIELD-V0  TO PI-PROOF-DATE
050506                MOVE PI-PROOF-DATE    TO MPRFDTO
050506                INSPECT MPRFDTI REPLACING ALL ' ' BY '/'
050506            END-IF
050506         END-IF
050506     END-IF.
00461
00462      MOVE LOW-VALUES             TO  WS-TRAILER-KEY.
00463      MOVE PI-COMPANY-CD          TO  WS-KEY-COMPANY-CD.
00464      MOVE PI-CARRIER             TO  WS-KEY-CARRIER.
00465      MOVE PI-CLAIM-NO            TO  WS-KEY-CLAIM-NO.
00466      MOVE PI-CERT-NO             TO  WS-KEY-CERT-NO.
00467
00468      PERFORM 7000-READ-ELMSTR-UPDATE THRU 7000-EXIT.
00469
00470      IF CL-LAST-TRL-AVAIL
00471          MOVE ER-0137            TO  EMI-ERROR
00472          MOVE -1                 TO  MLINE1L
00473          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00474          GO TO 8200-SEND-DATAONLY.
043019     if ws-prf-dt < cl-incurred-dt
043019        MOVE ER-0873             TO EMI-ERROR
043019        MOVE -1                  TO MPRFDTL
043019        MOVE AL-UABON            TO MPRFDTA
043019        PERFORM 9900-ERROR-FORMAT
043019                                 THRU 9900-EXIT
043019        GO TO 8200-SEND-DATAONLY
043019     end-if
00476      MOVE CL-TRAILER-SEQ-CNT     TO  WS-ORIG-SEQ-CNT.
00477      SUBTRACT 1 FROM CL-TRAILER-SEQ-CNT.
00478      MOVE CL-TRAILER-SEQ-CNT     TO  WS-SEQ-NO-SAVED.
00479      MOVE 'C'                    TO  CL-CLAIM-STATUS.
00480      MOVE PI-PROCESSOR-ID        TO  CL-LAST-MAINT-USER.
00481      MOVE '3'                    TO  CL-LAST-MAINT-TYPE.
00482      MOVE '2'                    TO  CL-LAST-CLOSE-REASON.
00483      MOVE EIBTIME                TO  CL-LAST-MAINT-HHMMSS.
00484      MOVE SAVE-BIN-DATE          TO  CL-LAST-MAINT-DT
00485                                      CL-LAST-CLOSE-DT.
00486
00487      IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')
00488          IF MDENCDL IS GREATER THAN +0
00489              MOVE MDENCDI        TO  CL-FILE-LOCATION.
00490
CIDMOD*************************************************************
CIDMOD*****         START BUILDING ACTIVITY RECORD HERE          **
CIDMOD*************************************************************
062121     IF PI-COMPANY-ID EQUAL 'CID' OR 'AHL' OR 'FNL'
CIDMOD         PERFORM 2300-GETMAIN-DLYACTV THRU
CIDMOD                 2300-EXIT
CIDMOD         PERFORM 3500-BUILD-ACTIVITY-RECORD THRU
CIDMOD                 3500-EXIT
CIDMOD         PERFORM 2350-WRITE-DLYACTV THRU
CIDMOD                 2350-EXIT
CIDMOD     END-IF.
CIDMOD
CIDMOD*************************************************************
CIDMOD*****            END OF BUILDING ACTIVITY RECORD           **
CIDMOD*************************************************************
CIDMOD
00491      PERFORM 7120-READ-ELTRLR-UPDATE THRU 7120-EXIT.
00492
00493      MOVE +1                     TO  MISC-SUB.
00494      MOVE SPACES                 TO  WS-OC-LOAD-SW.
00495      PERFORM 1000-SET-OPEN-CLOSE-HIST THRU 1000-EXIT.
00496
00497      MOVE +01                    TO  TR-SUB
00498                                      TR-SUB-2.
00499      PERFORM 1010-UPDATE-TR1-OC   THRU 1010-EXIT
00500              UNTIL  OC-HISTORY-LOADED.
00501
00502      MOVE PI-PROCESSOR-ID        TO AT-RESERVES-LAST-UPDATED-BY
00503      MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS
00504      MOVE SAVE-BIN-DATE          TO AT-RESERVES-LAST-MAINT-DT.
00505
00506      IF PI-COMPANY-ID = 'DMD'
00507          PERFORM 2000-CREATE-DMD-DMO  THRU  2000-EXIT.
00508
00509      PERFORM 7130-REWRITE-ELTRLR THRU 7130-EXIT.
00510
00511      PERFORM 7100-GETMAIN-ELTRLR  THRU 7100-EXIT.
00512      PERFORM 1050-BUILD-DENIAL-TRLR THRU 1090-EXIT.
00513
00514      PERFORM 7110-WRITE-ELTRLR    THRU 7110-EXIT.
00515
00516      PERFORM 6100-CHECK-AUTO-ACTIVITY THRU 6100-EXIT.
00517
00518      IF WS-REC-FOUND-SW IS EQUAL TO 'N'
00519          GO TO 0340-FINISH-EDIT-DATA.
00520
00521      IF CL-ACTIVITY-CODE IS EQUAL TO 09
00522          GO TO 0340-FINISH-EDIT-DATA.
00523
00524      IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK') AND
00525         (CL-ACTIVITY-CODE IS EQUAL TO 11)
00526          GO TO 0340-FINISH-EDIT-DATA.
00527
00528      IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')
00529          PERFORM 6000-RESET-AUTO-ACTIVITY THRU 6000-EXIT
00530          IF WS-UPDATE-SW IS EQUAL TO 'Y'
00531              MOVE LOW-VALUES         TO  CL-NEXT-RESEND-DT
00532                                          CL-NEXT-FOLLOWUP-DT.
00533
00534      IF WS-RESET-SW IS EQUAL TO 'Y'
00535          MOVE ZEROS                  TO  CL-ACTIVITY-CODE
00536          MOVE SAVE-BIN-DATE          TO  CL-ACTIVITY-MAINT-DT
00537          MOVE 'DENY'                 TO  CL-ACTIVITY-MAINT-TYPE.
00538
00539  0340-FINISH-EDIT-DATA.
00540
00541      PERFORM 7010-REWRITE-ELMSTR  THRU 7010-EXIT.
00542
00543      MOVE ER-0000                TO EMI-ERROR.
00544      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
PEMTST     MOVE -1                     TO MLINE1L.
PEMTST*    MOVE LOW-VALUES             TO MLINE1O
PEMTST*                                   MLINE2O
PEMTST*                                   MDENCDO.
00549      GO TO 8200-SEND-DATAONLY.
00550
00551      EJECT
00552  1000-SET-OPEN-CLOSE-HIST.
00553      PERFORM 1000-BUMP-OPEN-CLOSE-HIST UNTIL
00554         (MISC-SUB GREATER THAN +6 OR
00555         AT-OPEN-CLOSE-TYPE (MISC-SUB) = SPACES).
00556
00557      IF MISC-SUB GREATER THAN +1
00558         SUBTRACT +1 FROM MISC-SUB
00559         IF AT-OPEN-CLOSE-TYPE (MISC-SUB) = 'C'
00560            MOVE SAVE-BIN-DATE   TO AT-OPEN-CLOSE-DATE (MISC-SUB)
00561            MOVE 'C'             TO AT-OPEN-CLOSE-TYPE (MISC-SUB)
00562            MOVE 'DENIED'        TO AT-OPEN-CLOSE-REASON (MISC-SUB)
00563            MOVE 'X'             TO WS-OC-LOAD-SW.
00564
00565      GO TO 1000-EXIT.
00566
00567  1000-BUMP-OPEN-CLOSE-HIST.
00568      ADD +1 TO MISC-SUB.
00569
00570  1000-EXIT.
00571      EXIT.
00572
00573  1010-UPDATE-TR1-OC.
00574      IF  AT-OPEN-CLOSE-DATE (TR-SUB) = (LOW-VALUE OR SPACES)
00575          MOVE SAVE-BIN-DATE      TO AT-OPEN-CLOSE-DATE (TR-SUB)
00576          MOVE 'C'                TO AT-OPEN-CLOSE-TYPE (TR-SUB)
00577          MOVE 'DENIED'           TO AT-OPEN-CLOSE-REASON (TR-SUB)
00578          MOVE 'X'                TO WS-OC-LOAD-SW
00579          GO TO 1010-EXIT.
00580
00581      ADD 1  TO TR-SUB.
00582      IF  TR-SUB  GREATER THAN 6
00583          MOVE +01                TO TR-SUB
00584          MOVE +02                TO TR-SUB-2
00585          PERFORM 1020-BUMP    THRU 1020-EXIT
00586           UNTIL TR-SUB-2 GREATER THAN 6
00587          MOVE SPACES             TO AT-OPEN-CLOSE-DATE (TR-SUB).
00588
00589  1010-EXIT.
00590       EXIT.
00591
00592  1020-BUMP.
00593      MOVE AT-OPEN-CLOSE-DATE (TR-SUB-2) TO
00594                            AT-OPEN-CLOSE-DATE (TR-SUB).
00595      MOVE AT-OPEN-CLOSE-TYPE (TR-SUB-2) TO
00596                            AT-OPEN-CLOSE-TYPE (TR-SUB).
00597      MOVE AT-OPEN-CLOSE-REASON (TR-SUB-2) TO
00598                            AT-OPEN-CLOSE-REASON (TR-SUB).
00599
00600      ADD +1  TO TR-SUB
00601                 TR-SUB-2.
00602
00603  1020-EXIT.
00604      EXIT.
00605
00606      EJECT
00607  1050-BUILD-DENIAL-TRLR.
00608
00609      MOVE 'AT'                   TO AT-RECORD-ID.
00610      MOVE '8'                    TO AT-TRAILER-TYPE.
00611
00612 *DLO023
00613      IF PI-COMPANY-ID NOT = 'DMD'
00614          GO TO 1055-EDIT-CID-CODE
           END-IF
00615
00616      IF MDENCDL GREATER +0
00617          MOVE 'CL'               TO DL23-SYSTEM-ID
00618          MOVE 'DN'               TO DL23-RECORD-TYPE
00619          MOVE MDENCDI            TO DL23-RECORD-KEY
00620          
      * EXEC CICS LINK
00621 *            PROGRAM    ('DLO023')
00622 *            COMMAREA   (WS-DLO-CODES-TABLE)
00623 *            LENGTH     (DL23-COMM-LENGTH)
00624 *        END-EXEC
           MOVE 'DLO023' TO DFHEIV1
      *    MOVE '."C                   (   #00004489' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-DLO-CODES-TABLE, 
                 DL23-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00625          IF DL23-RETURN-CODE = 'OK'
00626              MOVE AL-UANON       TO MDENCDA
00627           ELSE
00628              MOVE AL-UABON       TO MDENCDA
00629              MOVE -1             TO MDENCDL
00630              MOVE ER-0884        TO EMI-ERROR
00631              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00632              PERFORM 2250-UNLOCK-CLAIM-MSTR THRU 2250-EXIT
00633              PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
00634              GO TO 8200-SEND-DATAONLY.
00635
00636      IF MLINE1I = LOW-VALUES OR SPACES
00637          MOVE SPACES         TO AT-DENIAL-INFO-1
00638       ELSE
00639          MOVE MLINE1I        TO AT-DENIAL-INFO-1.
00640
00641      GO TO 1070-CONTINUE.
00642
       1055-EDIT-CID-CODE.
           IF MDENCDL > +0
              MOVE LOW-VALUES          TO WS-ELDENY-KEY
              MOVE PI-COMPANY-CD       TO ELDENY-COMPANY-CD
              MOVE MDENCDI             TO ELDENY-DENIAL-CODE
              
      * EXEC CICS READ
      *          DATASET('ELDENY')
      *          SET    (ADDRESS OF DENIAL-CODES)
      *          RIDFLD (WS-ELDENY-KEY)
      *          RESP   (WS-RESPONSE)
      *       END-EXEC
           MOVE 'ELDENY' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00004517' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303034353137' TO DFHEIV0(25:11)
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
              IF RESP-NORMAL
                 IF MLINE1L > +0
                    STRING DN-DESCRIPTION ' ' MLINE1I
                       DELIMITED BY '  ' INTO AT-DENIAL-INFO-1
                    END-STRING
                 ELSE
                    MOVE DN-DESCRIPTION
                                       TO AT-DENIAL-INFO-1
                 END-IF
                 MOVE DN-RECORD-TYPE   TO CL-DENIAL-TYPE
                 MOVE AT-DENIAL-INFO-1 TO MLINE1O
                 MOVE +1               TO MLINE1L
                 MOVE AL-UANON         TO MLINE1A
                 MOVE DN-RECORD-TYPE
                                       TO CL-DENIAL-TYPE
              ELSE
                 MOVE AL-UABON         TO MDENCDA
                 MOVE -1               TO MDENCDL
                 MOVE ER-0884          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                 PERFORM 2250-UNLOCK-CLAIM-MSTR THRU 2250-EXIT
                 PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
                 GO TO 8200-SEND-DATAONLY
              END-IF
           ELSE
              MOVE AL-UABON            TO MDENCDA
              MOVE -1                  TO MDENCDL
              MOVE ER-0884             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              PERFORM 2250-UNLOCK-CLAIM-MSTR
                                       THRU 2250-EXIT
              PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
              GO TO 8200-SEND-DATAONLY
           END-IF
           GO TO 1070-CONTINUE
           .
00643  1060-CONTINUE.
00644      IF MDENCDL IS GREATER THAN +0
00645          SET DEN-INDEX TO +1
00646          SEARCH DEN-TABLE VARYING DEN-INDEX
00647              AT END
00648                  MOVE MLINE1I                TO  AT-DENIAL-INFO-1
00649          WHEN MDENCDI IS EQUAL TO DENIAL-CODE (DEN-INDEX)
00650              MOVE DENIAL-DESC (DEN-INDEX)    TO  AT-DENIAL-INFO-1
                                                       MLINE1O
00651      ELSE
00652          IF MLINE1I = LOW-VALUES OR SPACES
00653              MOVE SPACES         TO AT-DENIAL-INFO-1
00654          ELSE
00655              MOVE MLINE1I        TO AT-DENIAL-INFO-1.
00656
00657  1070-CONTINUE.
00658      IF  MLINE2I  = LOW-VALUES OR SPACES
00659          MOVE  SPACES            TO AT-DENIAL-INFO-2
00660      ELSE
00661          MOVE MLINE2I            TO AT-DENIAL-INFO-2.
00662
00663      IF (AT-DENIAL-INFO-1 IS EQUAL TO SPACES OR LOW-VALUES)
00664          MOVE AT-DENIAL-INFO-2   TO AT-DENIAL-INFO-1
00665          MOVE SPACES             TO AT-DENIAL-INFO-2.
00666
050506     MOVE WS-PRF-DT              TO AT-DENIAL-PROOF-DT.
00667      MOVE WS-SEQ-NO-SAVED        TO WS-KEY-SEQUENCE-NO.
00668
00669      MOVE WS-TRAILER-KEY         TO AT-CONTROL-PRIMARY.
00670      MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY
00671                                     AT-DENIAL-LAST-UPDATED-BY.
00672      MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.
00673      MOVE SAVE-BIN-DATE          TO AT-RECORDED-DT
00674                                     AT-DENIAL-DT
00675                                     AT-DENIAL-LAST-MAINT-DT.
00676      MOVE MDENCDI                TO AT-DENIAL-REASON-CODE.
00677      MOVE LOW-VALUES             TO AT-RETRACTION-DT.
00678
00679  1090-EXIT.
00680       EXIT.
00681       EJECT
00682 ******************************************************************
00683 *    DMD ONLY  -  CREATE THE DIRECT MARKETING OUTPUT FILE
00684 ******************************************************************
00685  2000-CREATE-DMD-DMO.
00686
00687      MOVE CL-CERT-KEY-DATA       TO W-NOTE-CERT-KEY.
00688      MOVE PI-COMPANY-CD          TO W-NOTE-COMP-CD.
00689      MOVE CL-CERT-NO             TO W-NOTE-CERT-NO.
00690
00691      
      * EXEC CICS HANDLE CONDITION
00692 *         NOTFND   (2000-NOTE-NOT-FOUND)
00693 *         END-EXEC.
      *    MOVE '"$I                   ! # #00004610' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303034363130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00694
00695      
      * EXEC CICS READ
00696 *         DATASET('ERNOTE')
00697 *         SET    (ADDRESS OF CERTIFICATE-NOTE)
00698 *         RIDFLD (W-NOTE-KEY)
00699 *         END-EXEC.
           MOVE 'ERNOTE' TO DFHEIV1
      *    MOVE '&"S        E          (   #00004614' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-NOTE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-NOTE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00700
00701      MOVE SPACES                 TO DCT-COMMUNICATION-AREA.
00702      MOVE CL-BENEFICIARY         TO DCT-LOGIC-BENEFICIARY-ID.
00703      MOVE CL-CCN                 TO DCT-CREDIT-CARD-NUMBER.
00704
00705      IF CL-CERT-GROUPING (5:2) = ZEROS OR SPACES
00706          MOVE 'CC'               TO DCT-PRODUCT-CODE
00707      ELSE
00708          MOVE CL-CERT-GROUPING (5:2) TO DCT-PRODUCT-CODE.
00709
00710      MOVE CN-CSI-CC-BILL-BANK-ID TO DCT-BILLING-BANK-ID.
00711      MOVE '02'                   TO DCT-COLUMN-ID-REQUESTED.
00712
00713      
      * EXEC CICS LINK
00714 *        PROGRAM    ('DLO006')
00715 *        COMMAREA   (DCT-COMMUNICATION-AREA)
00716 *        LENGTH     (WS-DCT-LENGTH)
00717 *    END-EXEC.
           MOVE 'DLO006' TO DFHEIV1
      *    MOVE '."C                   (   #00004632' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DCT-COMMUNICATION-AREA, 
                 WS-DCT-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00718
00719      IF  DCT-RETURN-CODE = 'OK'
00720          GO TO 2000-CONT.
00721
00722      IF  DCT-RETURN-CODE = '01' OR '02'
00723          GO TO 2000-EXIT.
00724
00725      IF  DCT-RETURN-CODE = '03'
00726          MOVE ER-0951            TO EMI-ERROR
00727          MOVE -1                 TO MLINE1L
00728          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00729          PERFORM 2250-UNLOCK-CLAIM-MSTR
00730              THRU 2250-EXIT
00731          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
00732          GO TO 8200-SEND-DATAONLY.
00733
00734      IF  DCT-RETURN-CODE = '06'
00735          MOVE ER-0921            TO EMI-ERROR
00736          MOVE -1                 TO MLINE1L
00737          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00738          PERFORM 2250-UNLOCK-CLAIM-MSTR
00739              THRU 2250-EXIT
00740          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
00741          GO TO 8200-SEND-DATAONLY.
00742
00743      IF  DCT-RETURN-CODE = '07'
00744          MOVE ER-0919            TO EMI-ERROR
00745          MOVE -1                 TO MLINE1L
00746          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00747          PERFORM 2250-UNLOCK-CLAIM-MSTR
00748              THRU 2250-EXIT
00749          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
00750          GO TO 8200-SEND-DATAONLY.
00751
00752      IF  DCT-RETURN-CODE = '04'
00753          MOVE ER-0946            TO EMI-ERROR
00754          MOVE -1                 TO MLINE1L
00755          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00756          PERFORM 2250-UNLOCK-CLAIM-MSTR
00757              THRU 2250-EXIT
00758          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
00759          GO TO 8200-SEND-DATAONLY.
00760
00761      IF  DCT-RETURN-CODE = '05'
00762          MOVE ER-0947            TO EMI-ERROR
00763          MOVE -1                 TO MLINE1L
00764          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00765          PERFORM 2250-UNLOCK-CLAIM-MSTR
00766              THRU 2250-EXIT
00767          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
00768          GO TO 8200-SEND-DATAONLY.
00769
00770      IF  DCT-RETURN-CODE = '08'
00771          MOVE ER-0948            TO EMI-ERROR
00772          MOVE -1                 TO MLINE1L
00773          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00774          PERFORM 2250-UNLOCK-CLAIM-MSTR
00775              THRU 2250-EXIT
00776          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
00777          GO TO 8200-SEND-DATAONLY.
00778
00779      IF  DCT-RETURN-CODE = 'N1'
00780          MOVE ER-0950            TO EMI-ERROR
00781          MOVE -1                 TO MLINE1L
00782          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00783          PERFORM 2250-UNLOCK-CLAIM-MSTR
00784              THRU 2250-EXIT
00785          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
00786          GO TO 8200-SEND-DATAONLY.
00787
00788      IF DCT-RETURN-CODE = 'E1'
00789          MOVE ER-0974            TO EMI-ERROR
00790          MOVE -1                 TO MLINE1L
00791          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00792          PERFORM 2250-UNLOCK-CLAIM-MSTR
00793              THRU 2250-EXIT
00794          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
00795          GO TO 8200-SEND-DATAONLY.
00796
00797      IF DCT-RETURN-CODE = 'E2'
00798          MOVE ER-0975            TO EMI-ERROR
00799          MOVE -1                 TO MLINE1L
00800          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00801          PERFORM 2250-UNLOCK-CLAIM-MSTR
00802              THRU 2250-EXIT
00803          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
00804          GO TO 8200-SEND-DATAONLY.
00805
00806      IF DCT-RETURN-CODE NOT = 'OK'
00807          MOVE ER-0949            TO EMI-ERROR
00808          MOVE -1                 TO MLINE1L
00809          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00810          PERFORM 2250-UNLOCK-CLAIM-MSTR
00811              THRU 2250-EXIT
00812          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
00813          GO TO 8200-SEND-DATAONLY.
00814
00815  2000-CONT.
00816
00817      MOVE SPACES                 TO DMO-COMMUNICATION-AREA.
00818      MOVE 'CS'                   TO DM-RECORD-TYPE.
00819      MOVE DCT-DISTRIBUTION-CODE  TO DM-DIST-CODE.
00820      MOVE DCT-MAIL-CODE          TO DM-MAIL-CODE.
00821      MOVE CL-CLAIM-NO            TO DM-CLAIM-NO.
00822      MOVE CL-CERT-NO (4:1)       TO DM-CLAIM-TYPE.
00823      MOVE CL-CCN                 TO DM-CREDIT-CARD-NUMBER.
00824
00825      MOVE SAVE-BIN-DATE          TO  DC-BIN-DATE-1.
00826      MOVE ' '                    TO  DC-OPTION-CODE.
00827      PERFORM 9700-LINK-DATE-CONVERT  THRU  9700-EXIT.
00828      MOVE DC-GREG-DATE-1-YMD     TO  DMD-YYMMDD.
00829
00830      IF DMD-YY GREATER THAN 70
00831          MOVE '19'               TO  DMD-DECADE
00832      ELSE
00833          MOVE '20'               TO  DMD-DECADE.
00834
00835      MOVE DMD-DATE-YYYYMMDD      TO  DM-STATUS-DATE.
00836      MOVE CL-INSURED-LAST-NAME   TO W-NAME-LAST.
00837      MOVE CL-INSURED-1ST-NAME    TO W-NAME-FIRST.
00838      MOVE CL-INSURED-MID-INIT    TO W-NAME-MIDDLE.
00839      PERFORM 2100-FORMAT-LAST-NAME-1ST THRU 2100-EXIT.
00840      MOVE WS-NAME-WORK           TO DM-INSURED-NAME.
00841
00842      MOVE 'D'                    TO DM-STAT-CHANGE-TYPE.
00843      MOVE CL-CARRIER             TO DM-STAT-CARRIER.
00844      MOVE '3'                    TO DM-CLAIM-STATUS.
00845
00846      
      * EXEC CICS LINK
00847 *        PROGRAM    ('DLO025')
00848 *        COMMAREA   (DMO-COMMUNICATION-AREA)
00849 *        LENGTH     (WS-DMO-LENGTH)
00850 *    END-EXEC.
           MOVE 'DLO025' TO DFHEIV1
      *    MOVE '."C                   (   #00004765' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DMO-COMMUNICATION-AREA, 
                 WS-DMO-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00851
00852      IF  DM-RETURN-CODE = 'OK'
00853          GO TO 2000-EXIT.
00854
00855      IF  DM-RETURN-CODE = '01'
00856          MOVE ER-8051            TO EMI-ERROR
00857          MOVE -1                 TO MLINE1L
00858          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00859          PERFORM 2250-UNLOCK-CLAIM-MSTR
00860              THRU 2250-EXIT
00861          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
00862          GO TO 8200-SEND-DATAONLY.
00863
00864      IF  DM-RETURN-CODE = '02'
00865          MOVE ER-8052            TO EMI-ERROR
00866          MOVE -1                 TO MLINE1L
00867          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00868          PERFORM 2250-UNLOCK-CLAIM-MSTR
00869              THRU 2250-EXIT
00870          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
00871          GO TO 8200-SEND-DATAONLY.
00872
00873      IF  DM-RETURN-CODE = '03'
00874          MOVE ER-8053            TO EMI-ERROR
00875          MOVE -1                 TO MLINE1L
00876          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00877          PERFORM 2250-UNLOCK-CLAIM-MSTR
00878              THRU 2250-EXIT
00879          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
00880          GO TO 8200-SEND-DATAONLY.
00881
00882      IF  DM-RETURN-CODE = '04'
00883          MOVE ER-8054            TO EMI-ERROR
00884          MOVE -1                 TO MLINE1L
00885          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00886          PERFORM 2250-UNLOCK-CLAIM-MSTR
00887              THRU 2250-EXIT
00888          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
00889          GO TO 8200-SEND-DATAONLY.
00890
00891      IF  DM-RETURN-CODE = '05'
00892          MOVE ER-8055            TO EMI-ERROR
00893          MOVE -1                 TO MLINE1L
00894          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00895          PERFORM 2250-UNLOCK-CLAIM-MSTR
00896              THRU 2250-EXIT
00897          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
00898          GO TO 8200-SEND-DATAONLY.
00899
00900      IF  DM-RETURN-CODE = '06'
00901          MOVE ER-8056            TO EMI-ERROR
00902          MOVE -1                 TO MLINE1L
00903          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00904          PERFORM 2250-UNLOCK-CLAIM-MSTR
00905              THRU 2250-EXIT
00906          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
00907          GO TO 8200-SEND-DATAONLY.
00908
00909      IF  DM-RETURN-CODE = '07'
00910          MOVE ER-8057            TO EMI-ERROR
00911          MOVE -1                 TO MLINE1L
00912          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00913          PERFORM 2250-UNLOCK-CLAIM-MSTR
00914              THRU 2250-EXIT
00915          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
00916          GO TO 8200-SEND-DATAONLY.
00917
00918      IF  DM-RETURN-CODE = '08'
00919          MOVE ER-8058            TO EMI-ERROR
00920          MOVE -1                 TO MLINE1L
00921          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00922          PERFORM 2250-UNLOCK-CLAIM-MSTR
00923              THRU 2250-EXIT
00924          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
00925          GO TO 8200-SEND-DATAONLY.
00926
00927      IF  DM-RETURN-CODE = '09'
00928          MOVE ER-8059            TO EMI-ERROR
00929          MOVE -1                 TO MLINE1L
00930          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00931          PERFORM 2250-UNLOCK-CLAIM-MSTR
00932              THRU 2250-EXIT
00933          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
00934          GO TO 8200-SEND-DATAONLY.
00935
00936      IF  DM-RETURN-CODE = '10'
00937          MOVE ER-8060            TO EMI-ERROR
00938          MOVE -1                 TO MLINE1L
00939          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00940          PERFORM 2250-UNLOCK-CLAIM-MSTR
00941              THRU 2250-EXIT
00942          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
00943          GO TO 8200-SEND-DATAONLY.
00944
00945      IF  DM-RETURN-CODE = '11'
00946          MOVE ER-8061            TO EMI-ERROR
00947          MOVE -1                 TO MLINE1L
00948          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00949          PERFORM 2250-UNLOCK-CLAIM-MSTR
00950              THRU 2250-EXIT
00951          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
00952          GO TO 8200-SEND-DATAONLY.
00953
00954      IF  DM-RETURN-CODE = '12'
00955          MOVE ER-8062            TO EMI-ERROR
00956          MOVE -1                 TO MLINE1L
00957          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00958          PERFORM 2250-UNLOCK-CLAIM-MSTR
00959              THRU 2250-EXIT
00960          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
00961          GO TO 8200-SEND-DATAONLY.
00962
00963      IF  DM-RETURN-CODE = '13'
00964          MOVE ER-8063            TO EMI-ERROR
00965          MOVE -1                 TO MLINE1L
00966          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00967          PERFORM 2250-UNLOCK-CLAIM-MSTR
00968              THRU 2250-EXIT
00969          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
00970          GO TO 8200-SEND-DATAONLY.
00971
00972      IF  DM-RETURN-CODE = '14'
00973          MOVE ER-8064            TO EMI-ERROR
00974          MOVE -1                 TO MLINE1L
00975          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00976          PERFORM 2250-UNLOCK-CLAIM-MSTR
00977              THRU 2250-EXIT
00978          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
00979          GO TO 8200-SEND-DATAONLY.
00980
00981      IF  DM-RETURN-CODE = '15'
00982          MOVE ER-8065            TO EMI-ERROR
00983          MOVE -1                 TO MLINE1L
00984          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00985          PERFORM 2250-UNLOCK-CLAIM-MSTR
00986              THRU 2250-EXIT
00987          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
00988          GO TO 8200-SEND-DATAONLY.
00989
00990      IF DM-RETURN-CODE = '16'
00991          MOVE ER-8154            TO EMI-ERROR
00992          MOVE -1                 TO MLINE1L
00993          PERFORM 9900-ERROR-FORMAT      THRU 9900-EXIT
00994          PERFORM 2250-UNLOCK-CLAIM-MSTR THRU 2250-EXIT
00995          PERFORM 2255-UNLOCK-TRLR       THRU 2255-EXIT
00996          GO TO 8200-SEND-DATAONLY.
00997
00998      IF DM-RETURN-CODE = '17'
00999          MOVE ER-8155            TO EMI-ERROR
01000          MOVE -1                 TO MLINE1L
01001          PERFORM 9900-ERROR-FORMAT      THRU 9900-EXIT
01002          PERFORM 2250-UNLOCK-CLAIM-MSTR THRU 2250-EXIT
01003          PERFORM 2255-UNLOCK-TRLR       THRU 2255-EXIT
01004          GO TO 8200-SEND-DATAONLY.
01005
01006      IF DM-RETURN-CODE = 'N1'
01007          MOVE ER-8152            TO EMI-ERROR
01008          MOVE -1                 TO MLINE1L
01009          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01010          PERFORM 2250-UNLOCK-CLAIM-MSTR
01011              THRU 2250-EXIT
01012          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
01013          GO TO 8200-SEND-DATAONLY.
01014
01015      IF DM-RETURN-CODE = 'E1'
01016          MOVE ER-8153            TO EMI-ERROR
01017          MOVE -1                 TO MLINE1L
01018          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01019          PERFORM 2250-UNLOCK-CLAIM-MSTR
01020              THRU 2250-EXIT
01021          PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT
01022          GO TO 8200-SEND-DATAONLY.
01023
01024      MOVE ER-8066                TO EMI-ERROR.
01025      MOVE -1                     TO MLINE1L.
01026      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01027      PERFORM 2250-UNLOCK-CLAIM-MSTR
01028          THRU 2250-EXIT.
01029      PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT.
01030      GO TO 8200-SEND-DATAONLY.
01031
01032  2000-NOTE-NOT-FOUND.
01033
01034      MOVE ER-0954                TO EMI-ERROR.
01035      MOVE -1                     TO MLINE1L.
01036      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01037      PERFORM 2250-UNLOCK-CLAIM-MSTR
01038          THRU 2250-EXIT.
01039      PERFORM 2255-UNLOCK-TRLR THRU 2255-EXIT.
01040      GO TO 8200-SEND-DATAONLY.
01041
01042  2000-EXIT.
01043      EXIT.
01044       EJECT
01045
01046  2100-FORMAT-LAST-NAME-1ST.
01047 *****************************************************************
01048 *             M O V E   N A M E   R O U T I N E                 *
01049 *     THE FOLLOWING ROUTINE REARRANGES A GIVEN NAME SO          *
01050 *     THAT IT READS LAST, FIRST, MIDDLE.  PLACE NAME            *
01051 *     FIELDS IN THE FOLLOWING WORKING STORAGE FIELDS.           *
01052 *                  FIELD                   VALUE                *
01053 *           W-NAME-LAST    (CL15)      SMITH                    *
01054 *           W-NAME-FIRST   (CL15)      JOHN                     *
01055 *           W-NAME-MIDDLE  (CL15)      ALLEN/A                  *
01056 *     AFTER NAME HAS BEEN MOVED WS-NAME-WORK (CL30) WILL        *
01057 *     CONTAIN                                                   *
01058 *                SMITH, JOHN ALLEN                              *
01059 *     OR                                                        *
01060 *                SMITH, JOHN A.                                 *
01061 *     TO USE THIS ROUTINE YOU NEED THE WORKING STORAGE          *
01062 *     COPYBOOK, ELCNWA.                                         *
01063 *****************************************************************.
01064
01065      MOVE SPACES                 TO  WS-NAME-WORK-AREA.
01066      MOVE ZERO                   TO  WS-NAME-SW.
01067      SET NWA-INDEX               TO +1.
01068
01069      IF  W-NAME-LAST EQUAL SPACES
01070              AND
01071          W-NAME-MIDDLE EQUAL SPACES
01072          MOVE +1                 TO WS-NAME-SW.
01073
01074      MOVE W-NAME-LAST            TO WS-NAME-WORK2.
01075      PERFORM 2150-MOVE-NAME THRU 2150-EXIT.
01076
01077      MOVE W-NAME-FIRST           TO WS-NAME-WORK2.
01078      PERFORM 2150-MOVE-NAME THRU 2150-EXIT.
01079
01080      SET NWA-INDEX UP BY +1.
01081
01082      IF  W-NAME-MIDDLE NOT EQUAL SPACES
01083
01084          IF  W-NAME-MIDDLE-2 EQUAL SPACES
01085              MOVE W-NAME-MIDDLE  TO WS-NW (NWA-INDEX)
01086              SET NWA-INDEX UP BY +1
01087              MOVE '.'            TO WS-NW (NWA-INDEX)
01088              SET NWA-INDEX UP BY +2
01089
01090          ELSE
01091              MOVE W-NAME-MIDDLE  TO WS-NAME-WORK2
01092              PERFORM 2150-MOVE-NAME THRU 2150-EXIT.
01093
01094  2100-EXIT.
01095      EXIT.
01096                                  EJECT
01097  2150-MOVE-NAME.
01098
01099      IF  WS-NAME-SW GREATER THAN +1
01100          GO TO 2150-EXIT.
01101
01102      IF  WS-NAME-WORK2 = SPACES
01103          GO TO 2150-EXIT.
01104
01105      SET NWA-INDEX2            TO +1.
01106      SET NWA-INDEX3            TO +2.
01107
01108  2150-MOVE-NAME-CYCLE.
01109
01110      MOVE WS-NW2 (NWA-INDEX2)  TO  WS-NW (NWA-INDEX).
01111
01112      IF  NWA-INDEX LESS THAN +30
01113          SET NWA-INDEX UP BY +1
01114
01115      ELSE
01116          ADD +2                TO  WS-NAME-SW
01117          GO TO 2150-EXIT.
01118
01119      IF  NWA-INDEX2 LESS THAN +20
01120          SET NWA-INDEX3 UP BY +1
01121          SET NWA-INDEX2 UP BY +1.
01122
01123      IF  WS-NW2 (NWA-INDEX2) EQUAL SPACES
01124              AND
01125          WS-NW2 (NWA-INDEX3) EQUAL SPACES
01126
01127          IF  WS-NAME-SW EQUAL ZERO
01128              MOVE ','            TO  WS-NW (NWA-INDEX)
01129              SET NWA-INDEX UP BY +2
01130              MOVE +1             TO  WS-NAME-SW
01131              GO TO 2150-EXIT
01132
01133          ELSE
01134              GO TO 2150-EXIT.
01135
01136      GO TO 2150-MOVE-NAME-CYCLE.
01137
01138  2150-EXIT.
01139      EXIT.
01140                                  EJECT
01141  2250-UNLOCK-CLAIM-MSTR.
01142       
      * EXEC CICS UNLOCK
01143 *          DATASET  (ELMSTR-FILE-ID)
01144 *          END-EXEC.
      *    MOVE '&*                    #   #00005061' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01145  2250-EXIT.
01146      EXIT.
01147  2255-UNLOCK-TRLR.
01148       
      * EXEC CICS UNLOCK
01149 *          DATASET (ELTRLR-FILE-ID)
01150 *          END-EXEC.
      *    MOVE '&*                    #   #00005067' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01151  2255-EXIT.
01152      EXIT.
CIDMOD/
CIDMOD*************************************************************
CIDMOD*****      CSO ADDITION - FOR BUILDING ACTIVITY FILE      ***
CIDMOD*************************************************************
CIDMOD 2300-GETMAIN-DLYACTV.
CIDMOD     
      * EXEC CICS GETMAIN
CIDMOD*        SET (ADDRESS OF DAILY-ACTIVITY-RECORD)
CIDMOD*        LENGTH (25)
CIDMOD*        INITIMG (WS-BLANK)
CIDMOD*    END-EXEC.
           MOVE 25
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00005077' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 WS-BLANK
           SET ADDRESS OF DAILY-ACTIVITY-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
CIDMOD 2300-EXIT. EXIT.
CIDMOD/
CIDMOD*************************************************************
CIDMOD*****      CSO ADDITION - FOR BUILDING ACTIVITY FILE      ***
CIDMOD*************************************************************
CIDMOD 2350-WRITE-DLYACTV.
CIDMOD     
      * EXEC CICS HANDLE CONDITION
CIDMOD*        NOTOPEN (9986-NOTOPEN-DLYACTV)
CIDMOD*        DUPREC (2350-EXIT)
CIDMOD*    END-EXEC.
      *    MOVE '"$J%                  ! $ #00005088' TO DFHEIV0
           MOVE X'22244A252020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303035303838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
CIDMOD
CIDMOD     
      * EXEC CICS WRITE
CIDMOD*        DATASET ('DLYACTV')
CIDMOD*        RIDFLD (DA-KEY)
CIDMOD*        FROM (DAILY-ACTIVITY-RECORD)
CIDMOD*    END-EXEC.
           MOVE LENGTH OF
            DAILY-ACTIVITY-RECORD
             TO DFHEIV11
           MOVE 'DLYACTV' TO DFHEIV1
      *    MOVE '&$ L                  ''   #00005093' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DAILY-ACTIVITY-RECORD, 
                 DFHEIV11, 
                 DA-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
CIDMOD
CIDMOD 2350-EXIT.
CIDMOD     EXIT.
CIDMOD
CIDMOD/
CIDMOD*************************************************************
CIDMOD*****      CSO ADDITION - FOR BUILDING ACTIVITY FILE      ***
CIDMOD*************************************************************
CIDMOD
CIDMOD 3500-BUILD-ACTIVITY-RECORD.
CIDMOD
CIDMOD     MOVE SPACES                 TO DAILY-ACTIVITY-RECORD.
CIDMOD     MOVE WS-CLAIM-KEY           TO DA-KEY.
CIDMOD     SUBTRACT 1 FROM CL-TRAILER-SEQ-CNT
CIDMOD                                 GIVING DA-TRAILER-SEQ-NO.
CIDMOD     MOVE 'D'                    TO DA-RECORD-TYPE.
CIDMOD
CIDMOD 3500-EXIT.
CIDMOD     EXIT.
CIDMOD/
050506 3550-FIND-LETTER-TRLR.
050506      MOVE PI-COMPANY-CD             TO  WS-KEY-COMPANY-CD.
050506      MOVE PI-CARRIER                TO  WS-KEY-CARRIER.
050506      MOVE PI-CLAIM-NO               TO  WS-KEY-CLAIM-NO.
050506      MOVE PI-CERT-NO                TO  WS-KEY-CERT-NO.
050506      MOVE +100                      TO  WS-KEY-SEQUENCE-NO.
050506
050506 3550-STARTBR-ELTRLR.
050506
050506      
      * EXEC CICS HANDLE CONDITION
050506*         ENDFILE   (3550-END)
050506*         NOTFND    (3550-END)
050506*     END-EXEC.
      *    MOVE '"$''I                  ! % #00005127' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303035313237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
050506
050506      
      * EXEC CICS STARTBR
050506*         DATASET   (ELTRLR-FILE-ID)
050506*         RIDFLD    (WS-TRAILER-KEY)
050506*         GTEQ
050506*     END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005132' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 WS-TRAILER-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
050506
050506      MOVE LOW-VALUES TO WS-MAX-LETTER-ANSWER-DT.
050506
050506 3550-READNEXT-ELTRLR.
050506
050506      
      * EXEC CICS READNEXT
050506*         DATASET   (ELTRLR-FILE-ID)
050506*         RIDFLD    (WS-TRAILER-KEY)
050506*         SET       (ADDRESS OF ACTIVITY-TRAILERS)
050506*     END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005142' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-TRAILER-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
050506
050506      IF (PI-COMPANY-CD IS NOT EQUAL TO  WS-KEY-COMPANY-CD) OR
050506         (PI-CARRIER    IS NOT EQUAL TO  WS-KEY-CARRIER)    OR
050506         (PI-CLAIM-NO   IS NOT EQUAL TO  WS-KEY-CLAIM-NO)   OR
050506         (PI-CERT-NO    IS NOT EQUAL TO  WS-KEY-CERT-NO)
050506          GO TO 3550-END.
050506
050506      IF AT-TRAILER-TYPE IS NOT EQUAL TO '4'
050506          GO TO 3550-READNEXT-ELTRLR.
050506
050506     IF AT-LETTER-ANSWERED-DT GREATER THAN WS-MAX-LETTER-ANSWER-DT
050506         MOVE AT-LETTER-ANSWERED-DT TO WS-MAX-LETTER-ANSWER-DT
050506     END-IF.
050506
050506     GO TO 3550-READNEXT-ELTRLR.
050506
050506 3550-END.
050506
050506     IF WS-MAX-LETTER-ANSWER-DT NOT EQUAL LOW-VALUES AND SPACES
050506         MOVE SPACES             TO  DC-OPTION-CODE
050506         MOVE WS-MAX-LETTER-ANSWER-DT TO  DC-BIN-DATE-1
050506         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
050506         MOVE DC-GREG-DATE-1-MDY TO PI-PROOF-DATE
050506         MOVE PI-PROOF-DATE TO  MPRFDTO
050506         INSPECT MPRFDTI REPLACING ALL ' ' BY '/'
050506     END-IF.
050506
050506      
      * EXEC CICS ENDBR
050506*         DATASET   (ELTRLR-FILE-ID)
050506*     END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005174' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
050506
050506 3550-EXIT.
050506     EXIT.
050506/
01154  6000-RESET-AUTO-ACTIVITY.
01155 ******************************************************************
01156 *    RESET ALL FUTURE LETTER ACTIVITY AWAITING FURTHER ACTION    *
01157 *    (RESEND, FOLLOW-UP, ETC) BY MOVING LOW-VALUES TO THE        *
01158 *    FOLLOW-UP OR RESEND DATES IN THE ACTIVITY TRAILER AND THE   *
01159 *    RESEND DATE IS THE LETTER ARCHIVE, IF NECESSARY.            *
01160 ******************************************************************
01161       MOVE PI-COMPANY-CD             TO  WS-KEY-COMPANY-CD.
01162       MOVE PI-CARRIER                TO  WS-KEY-CARRIER.
01163       MOVE PI-CLAIM-NO               TO  WS-KEY-CLAIM-NO.
01164       MOVE PI-CERT-NO                TO  WS-KEY-CERT-NO.
01165       MOVE +100                      TO  WS-KEY-SEQUENCE-NO.
01166
01167  6000-STARTBR-ELTRLR.
01168
01169       
      * EXEC CICS HANDLE CONDITION
01170 *         ENDFILE   (6000-END-RESET)
01171 *         NOTFND    (6000-END-RESET)
01172 *     END-EXEC.
      *    MOVE '"$''I                  ! & #00005196' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303035313936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01173
01174       
      * EXEC CICS STARTBR
01175 *         DATASET   (ELTRLR-FILE-ID)
01176 *         RIDFLD    (WS-TRAILER-KEY)
01177 *         GTEQ
01178 *     END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005201' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 WS-TRAILER-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01179
01180       MOVE 'Y'                       TO  WS-BROWSE-SW.
01181
01182  6000-READNEXT-ELTRLR.
01183
01184       
      * EXEC CICS READNEXT
01185 *         DATASET   (ELTRLR-FILE-ID)
01186 *         RIDFLD    (WS-TRAILER-KEY)
01187 *         SET       (ADDRESS OF ACTIVITY-TRAILERS)
01188 *     END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005211' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-TRAILER-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01189
01190       IF (PI-COMPANY-CD IS NOT EQUAL TO  WS-KEY-COMPANY-CD) OR
01191          (PI-CARRIER    IS NOT EQUAL TO  WS-KEY-CARRIER)    OR
01192          (PI-CLAIM-NO   IS NOT EQUAL TO  WS-KEY-CLAIM-NO)   OR
01193          (PI-CERT-NO    IS NOT EQUAL TO  WS-KEY-CERT-NO)
01194           GO TO 6000-END-RESET.
01195
01196       IF WS-TRAILER-KEY IS EQUAL TO PI-PREV-TRLR-KEY
01197           GO TO 6000-READNEXT-ELTRLR.
01198
01199       IF AT-TRAILER-TYPE IS NOT EQUAL TO '4'
01200           GO TO 6000-READNEXT-ELTRLR.
01201
01202       IF (AT-LETTER-ANSWERED-DT NOT EQUAL LOW-VALUES AND SPACES)
01203           GO TO 6000-READNEXT-ELTRLR.
01204
01205       IF (AT-AUTO-RE-SEND-DT EQUAL LOW-VALUES OR SPACES)
01206         AND
01207          (AT-RECEIPT-FOLLOW-UP EQUAL LOW-VALUES OR SPACES)
01208           GO TO 6000-READNEXT-ELTRLR.
01209
01210       IF AT-RECEIPT-FOLLOW-UP IS LESS THAN SAVE-BIN-DATE AND
01211          AT-AUTO-RE-SEND-DT IS LESS THAN SAVE-BIN-DATE   AND
01212          AT-RESEND-PRINT-DATE IS LESS THAN SAVE-BIN-DATE
01213           GO TO 6000-READNEXT-ELTRLR.
01214
01215       IF (AT-AUTO-RE-SEND-DT NOT EQUAL LOW-VALUES AND SPACES)
01216         AND
01217          (AT-RESEND-PRINT-DATE NOT EQUAL LOW-VALUES AND SPACES)
01218         AND
01219          (AT-RECEIPT-FOLLOW-UP EQUAL LOW-VALUES)
01220           GO TO 6000-READNEXT-ELTRLR.
01221
01222  6000-END-BROWSE.
01223
01224       MOVE WS-TRAILER-KEY            TO  PI-PREV-TRLR-KEY.
01225
01226       
      * EXEC CICS ENDBR
01227 *         DATASET   (ELTRLR-FILE-ID)
01228 *     END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005253' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01229
01230       MOVE 'N'                       TO  WS-BROWSE-SW.
01231
01232  6000-READ-ELTRLR-UPDATE.
01233
01234       
      * EXEC CICS READ
01235 *         DATASET   (ELTRLR-FILE-ID)
01236 *         RIDFLD    (WS-TRAILER-KEY)
01237 *         SET       (ADDRESS OF ACTIVITY-TRAILERS)
01238 *         UPDATE
01239 *     END-EXEC.
      *    MOVE '&"S        EU         (   #00005261' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-TRAILER-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01240
01241       MOVE 'Y'                       TO  WS-UPDATE-SW.
01242
01243       IF AT-AUTO-RE-SEND-DT IS NOT LESS THAN SAVE-BIN-DATE
01244           MOVE LOW-VALUES            TO  AT-AUTO-RE-SEND-DT
01245           MOVE PI-COMPANY-CD         TO  ELARCH-COMPANY-CD
01246           MOVE AT-LETTER-ARCHIVE-NO  TO  ELARCH-ARCHIVE-NO
01247           MOVE '1'                   TO  ELARCH-RECORD-TYPE
01248           MOVE +0                    TO  ELARCH-SEQ-NO
01249           PERFORM 6200-READ-ELARCH-UPDATE THRU 6200-EXIT.
01250
01251       IF AT-RECEIPT-FOLLOW-UP IS NOT LESS THAN SAVE-BIN-DATE
01252           MOVE LOW-VALUES            TO  AT-RECEIPT-FOLLOW-UP.
01253
01254  6000-REWRITE-ELTRLR.
01255
01256       
      * EXEC CICS REWRITE
01257 *         DATASET   (ELTRLR-FILE-ID)
01258 *         FROM      (ACTIVITY-TRAILERS)
01259 *     END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005283' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01260
01261       GO TO 6000-STARTBR-ELTRLR.
01262
01263  6000-END-RESET.
01264
01265       IF WS-BROWSE-SW IS EQUAL TO 'Y'
01266           MOVE 'N'                   TO  WS-BROWSE-SW
01267           
      * EXEC CICS ENDBR
01268 *             DATASET   (ELTRLR-FILE-ID)
01269 *         END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005294' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01270
01271  6000-EXIT.
01272      EXIT.
01273
01274       EJECT
01275  6100-CHECK-AUTO-ACTIVITY.
01276 ******************************************************************
01277 *    READ THE AUTOMATIC ACTIVITY RECORD TO DETERMINE IF THE      *
01278 *    AUTOMATIC ACTIVITY CODE IN THE CLAIM MASTER IS TO BE RESET. *
01279 ******************************************************************
01280
01281       
      * EXEC CICS HANDLE CONDITION
01282 *         NOTFND   (6100-NOT-FOUND)
01283 *     END-EXEC.
      *    MOVE '"$I                   ! '' #00005308' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303035333038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01284
01285       MOVE PI-COMPANY-ID             TO  ELCNTL-COMPANY-ID.
01286       MOVE 'T'                       TO  ELCNTL-RECORD-TYPE.
01287       MOVE SPACES                    TO  ELCNTL-ACCESS.
01288       MOVE +0                        TO  ELCNTL-SEQ-NO.
01289
01290       
      * EXEC CICS READ
01291 *         DATASET   (ELCNTL-FILE-ID)
01292 *         RIDFLD    (ELCNTL-KEY)
01293 *         SET       (ADDRESS OF CONTROL-FILE)
01294 *     END-EXEC.
      *    MOVE '&"S        E          (   #00005317' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01295
01296       IF CL-ACTIVITY-CODE IS NOT EQUAL TO ZEROS
01297           MOVE CL-ACTIVITY-CODE      TO  MISC-SUB
01298           IF MISC-SUB IS GREATER THAN +9
01299               SUBTRACT +9 FROM MISC-SUB
01300               MOVE CF-USER-RESET-SW (MISC-SUB) TO  WS-RESET-SW
01301           ELSE
01302               MOVE CF-SYS-RESET-SW  (MISC-SUB) TO  WS-RESET-SW.
01303
01304       MOVE 'Y'                       TO  WS-REC-FOUND-SW.
01305       GO TO 6100-EXIT.
01306
01307  6100-NOT-FOUND.
01308
01309      MOVE 'N'                        TO  WS-RESET-SW
01310                                          WS-REC-FOUND-SW.
01311
01312  6100-EXIT.
01313      EXIT.
01314
01315       EJECT
01316  6200-READ-ELARCH-UPDATE.
01317 ******************************************************************
01318 *  READ AND UPDATE THE RESEND DATE IN THE LETTER ARCHIVE RECORD  *
01319 ******************************************************************
01320
01321       
      * EXEC CICS HANDLE CONDITION
01322 *         NOTFND   (6200-EXIT)
01323 *     END-EXEC.
      *    MOVE '"$I                   ! ( #00005348' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303035333438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01324
01325       
      * EXEC CICS READ
01326 *         DATASET   (ELARCH-FILE-ID)
01327 *         RIDFLD    (ELARCH-KEY)
01328 *         SET       (ADDRESS OF LETTER-ARCHIVE)
01329 *         UPDATE
01330 *     END-EXEC.
      *    MOVE '&"S        EU         (   #00005352' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELARCH-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELARCH-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01331
01332       MOVE LOW-VALUES                TO  LA-RESEND-DATE.
01333
01334  6200-REWRITE-ELARCH.
01335
01336       
      * EXEC CICS REWRITE
01337 *         DATASET   (ELARCH-FILE-ID)
01338 *         FROM      (LETTER-ARCHIVE)
01339 *     END-EXEC.
           MOVE LENGTH OF
            LETTER-ARCHIVE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005363' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELARCH-FILE-ID, 
                 LETTER-ARCHIVE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01340
01341  6200-EXIT.
01342      EXIT.
01343
01344      EJECT
01345  7000-READ-ELMSTR-UPDATE.
01346
01347      
      * EXEC CICS  HANDLE CONDITION
01348 *           NOTFND  (7000-NOTFND)
01349 *           NOTOPEN (9981-NOTOPEN-MSTR)
01350 *    END-EXEC.
      *    MOVE '"$IJ                  ! ) #00005374' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303035333734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01351
01352      
      * EXEC CICS  READ
01353 *        DATASET  (ELMSTR-FILE-ID)
01354 *        RIDFLD   (WS-CLAIM-KEY)
01355 *        SET      (ADDRESS OF CLAIM-MASTER)
01356 *        UPDATE
01357 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00005379' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CLAIM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01358
01359      GO TO 7000-EXIT.
01360
01361  7000-NOTFND.
01362
01363      MOVE ER-0133                TO  EMI-ERROR.
01364      MOVE -1                     TO  MLINE1L.
01365      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01366      GO TO 8200-SEND-DATAONLY.
01367
01368  7000-EXIT.
01369       EXIT.
01370
01371  7010-REWRITE-ELMSTR.
01372
01373      
      * EXEC CICS HANDLE CONDITION
01374 *        DUPKEY (7010-EXIT)
01375 *        END-EXEC.
      *    MOVE '"$$                   ! * #00005400' TO DFHEIV0
           MOVE X'222424202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303035343030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01376
01377      
      * EXEC CICS  REWRITE
01378 *        DATASET  (ELMSTR-FILE-ID)
01379 *        FROM     (CLAIM-MASTER)
01380 *    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005404' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-FILE-ID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01381
01382  7010-EXIT.
01383       EXIT.
01384
01385      EJECT
01386  7100-GETMAIN-ELTRLR.
01387
01388      
      * EXEC CICS GETMAIN
01389 *        SET     (ADDRESS OF ACTIVITY-TRAILERS)
01390 *        INITIMG (GETMAIN-SPACE)
01391 *        LENGTH  (WS-TRLR-LENGTH)
01392 *    END-EXEC.
      *    MOVE ',"IL                  $   #00005415' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 WS-TRLR-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01393
01394  7100-EXIT.
01395      EXIT.
01396
01397  7110-WRITE-ELTRLR.
01398      
      * EXEC CICS  HANDLE CONDITION
01399 *           DUPREC   (7110-DUPREC)
01400 *    END-EXEC.
      *    MOVE '"$%                   ! + #00005425' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303035343235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01401
01402      
      * EXEC CICS  WRITE
01403 *           DATASET  (ELTRLR-FILE-ID)
01404 *           RIDFLD   (WS-TRAILER-KEY)
01405 *           FROM     (ACTIVITY-TRAILERS)
01406 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00005429' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 WS-TRAILER-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01407
01408      GO TO 7110-EXIT.
01409
01410  7110-DUPREC.
01411
01412      MOVE ER-0132                TO  EMI-ERROR.
01413      MOVE -1                     TO  MLINE1L.
01414      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01415      GO TO 8200-SEND-DATAONLY.
01416
01417  7110-EXIT.
01418       EXIT.
01419
01420  7120-READ-ELTRLR-UPDATE.
01421
01422      
      * EXEC CICS  HANDLE CONDITION
01423 *           NOTFND  (9965-NO-TRLR-ERROR)
01424 *           NOTOPEN (9982-NOTOPEN-TRLR)
01425 *    END-EXEC.
      *    MOVE '"$IJ                  ! , #00005449' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303035343439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01426
01427      
      * EXEC CICS  READ
01428 *        DATASET  (ELTRLR-FILE-ID)
01429 *        RIDFLD   (WS-TRAILER-KEY)
01430 *        SET      (ADDRESS OF ACTIVITY-TRAILERS)
01431 *        UPDATE
01432 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00005454' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-TRAILER-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01433
01434  7120-EXIT.
01435       EXIT.
01436
01437  7130-REWRITE-ELTRLR.
01438
01439      
      * EXEC CICS  REWRITE
01440 *        DATASET  (ELTRLR-FILE-ID)
01441 *        FROM     (ACTIVITY-TRAILERS)
01442 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005466' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01443
01444  7130-EXIT.
01445       EXIT.
01446
01447      EJECT
01448  8100-SEND-INITIAL-MAP.
01449
01450      MOVE SAVE-DATE                  TO  MRNDATEO.
01451      MOVE EIBTIME                    TO  TIME-IN.
01452      MOVE TIME-OUT                   TO  MRNTIMEO.
01453
01454      
      * EXEC CICS  SEND
01455 *         MAP      (MAP-NAME)
01456 *         MAPSET   (MAPSET-NAME)
01457 *         FROM     (EL151AO)
01458 *         ERASE
01459 *         CURSOR
01460 *    END-EXEC.
           MOVE LENGTH OF
            EL151AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00005481' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL151AO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01461
01462      GO TO 9100-RETURN-TRAN.
01463
01464  8200-SEND-DATAONLY.
01465
01466      MOVE SAVE-DATE                  TO  MRNDATEO.
01467      MOVE EIBTIME                    TO  TIME-IN.
01468      MOVE TIME-OUT                   TO  MRNTIMEO.
01469
01470      
      * EXEC CICS  SEND
01471 *        MAP       (MAP-NAME)
01472 *        MAPSET    (MAPSET-NAME)
01473 *        FROM      (EL151AO)
01474 *        DATAONLY
01475 *        CURSOR
01476 *    END-EXEC.
           MOVE LENGTH OF
            EL151AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00005497' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL151AO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01477
01478      GO TO 9100-RETURN-TRAN.
01479
01480  8300-SEND-TEXT.
01481      
      * EXEC CICS SEND TEXT
01482 *        FROM     (LOGOFF-TEXT)
01483 *        LENGTH   (LOGOFF-LENGTH)
01484 *        ERASE
01485 *        FREEKB
01486 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00005508' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353038' TO DFHEIV0(25:11)
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
           
01487
01488      
      * EXEC CICS RETURN
01489 *        END-EXEC.
      *    MOVE '.(                    ''   #00005515' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01490
01491      EJECT
01492  9600-PGMID-ERROR.
01493      
      * EXEC CICS HANDLE CONDITION
01494 *        PGMIDERR (8300-SEND-TEXT)
01495 *    END-EXEC.
      *    MOVE '"$L                   ! - #00005520' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2D20233030303035353230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01496
01497      MOVE THIS-PGM               TO LOGOFF-PGM
01498                                     PI-CALLING-PROGRAM.
01499      MOVE SPACES                 TO PI-ENTRY-CD-1.
01500      MOVE 'EL005'                TO THIS-PGM.
01501      MOVE THIS-PGM               TO LOGOFF-PGM.
01502      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
01503      GO TO 9300-XCTL.
01504
01505  8800-UNAUTHORIZED-ACCESS.
01506
01507      MOVE UNACCESS-MSG           TO LOGOFF-MSG.
01508      GO TO 8300-SEND-TEXT.
01509
01510  9100-RETURN-TRAN.
01511      MOVE EMI-ERROR-NUMBER (1)    TO  PI-LAST-ERROR-NO.
01512      MOVE '151A'                  TO  PI-CURRENT-SCREEN-NO.
01513
01514      
      * EXEC CICS  RETURN
01515 *        TRANSID  (WS-TRANS-ID)
01516 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
01517 *        LENGTH   (PI-COMM-LENGTH)
01518 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00005541' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01519
01520  9300-XCTL.
01521      
      * EXEC CICS XCTL
01522 *        PROGRAM  (THIS-PGM)
01523 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
01524 *        LENGTH   (PI-COMM-LENGTH)
01525 *    END-EXEC.
      *    MOVE '.$C                   %   #00005548' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 THIS-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01526
01527  9400-CLEAR.
01528      MOVE PI-RETURN-TO-PROGRAM       TO  THIS-PGM.
01529      GO TO 9300-XCTL.
01530
050506 9650-DEEDIT.
050506     
      * EXEC CICS BIF DEEDIT
050506*        FIELD (DEEDIT-FIELD)
050506*        LENGTH(15)
050506*    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005559' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
050506
050506 9650-EXIT.
050506      EXIT.
050506
01531  9700-LINK-DATE-CONVERT.
01532      
      * EXEC CICS LINK
01533 *           PROGRAM  ('ELDATCV')
01534 *           COMMAREA (DATE-CONVERSION-DATA)
01535 *           LENGTH   (DC-COMM-LENGTH)
01536 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00005568' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01537
01538  9700-EXIT.
01539       EXIT.
01540
01541  9900-ERROR-FORMAT.
01542
01543      
      * EXEC CICS LINK
01544 *        PROGRAM  ('EL001')
01545 *        COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)
01546 *        LENGTH   (EMI-COMM-LENGTH)
01547 *    END-EXEC.
           MOVE 'EL001' TO DFHEIV1
      *    MOVE '."C                   (   #00005579' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01548
01549      MOVE EMI-LINE1              TO  MERMSG1O.
01550
01551  9900-EXIT.
01552       EXIT.
01553
01554  9965-NO-TRLR-ERROR.
01555      MOVE ER-0270                TO  EMI-ERROR.
01556      MOVE -1                     TO  MLINE1L.
01557      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01558      GO TO 8200-SEND-DATAONLY.
01559
01560  9981-NOTOPEN-MSTR.
01561      MOVE ER-0154                TO EMI-ERROR
01562      MOVE -1                     TO  MLINE1L.
01563      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01564      GO TO 8200-SEND-DATAONLY.
01565
01566  9982-NOTOPEN-TRLR.
01567      MOVE ER-0172                TO EMI-ERROR
01568      MOVE -1                     TO  MLINE1L.
01569      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01570      GO TO 8200-SEND-DATAONLY.
01571
CIDMOD 9986-NOTOPEN-DLYACTV.
CIDMOD     MOVE '2955'                 TO EMI-ERROR.
CIDMOD     MOVE -1                     TO  MLINE1L.
CIDMOD     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
CIDMOD     GO TO 8200-SEND-DATAONLY.
CIDMOD
01572  9995-SECURITY-VIOLATION.
01573 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00005632' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363332' TO DFHEIV0(25:11)
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
01574
01575  9995-EXIT.
01576      EXIT.
01577
01578  9990-ABEND.
01579      MOVE -1                      TO  MPFNUMBL.
01580      MOVE DFHEIBLK                TO  EMI-LINE1.
01581
01582      
      * EXEC CICS LINK
01583 *          PROGRAM  ('EL004')
01584 *          COMMAREA (EMI-LINE1)
01585 *          LENGTH   (72)
01586 *    END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00005648' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01587
01588      MOVE EMI-LINE1               TO  MERMSG1O.
01589      GO TO 8200-SEND-DATAONLY.
01590

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL151' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9990-ABEND,
                     9600-PGMID-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 2000-NOTE-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 9986-NOTOPEN-DLYACTV,
                     2350-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 3550-END,
                     3550-END
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 6000-END-RESET,
                     6000-END-RESET
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 6100-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 6200-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 7000-NOTFND,
                     9981-NOTOPEN-MSTR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 7010-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 7110-DUPREC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 9965-NO-TRLR-ERROR,
                     9982-NOTOPEN-TRLR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL151' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
