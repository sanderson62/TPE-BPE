00001  ID DIVISION.                                                     
00002                                                                   
00003  PROGRAM-ID.                 VP6311.
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 04/20/94 16:50:32.                 
00007 *                            VMOD=2.089.                          
00008 *                                                                 
00009 *AUTHOR.     LOGIC,INC.                                           
00010 *            DALLAS, TEXAS.                                       
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
00025 *        TRANSACTION - VPB1 - NEW BUSINESS, REVIEW AND CORRECTION 
00026 *                             ISSUES, CANCELS, AND BATCH HEADERS  
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING  
100703* 100703    2003080800002  PEMA  ADD SUPER GAP PROCESSING
011904* 011904                   PEMA  ADD TOTAL FEE PROCESSING
110105* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
081606* 081606  CR2006051800002  PEMA  ADD POST CARD PROCESSING
101706* 101706  CR2006080800002  PEMA  ADD VIN NUMBER
073107* 073107  CR2006051600002  PEMA  ADD OVERCHARGE PROCESSING
090607* 090607  CR2007010300001  PEMA  ADD CO TYPE TO OVERCHARGE
030708* 030708  CR2007092500001  PEMA  ADD VA DISCLOSURE PROCESSING
072308* 072308  CR2007110500003  PEMA  ADD NH REFUND INTEREST PROCESSING
090408* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
102408* 102408  CR2008080500002  PEMA  ADD BILLING NOTE FOR EC OF P
072009* 072009  IR2009062300002  AJRA  RESET CALC FLAG ON CHANGE
072209* 072209  CR2008101500003  AJRA  PRINT BORROWER FIRST NAME
111109* 111109  CR2008100900003  AJRA  CHANGE PF8 TO NEW CERT NOTE SCRN
032210* 032210  IR2010022400001  PEMA  CORRECT PF5 LOGIC, ADD APR PROC
101110* 101110  CR2010091000002  PEMA  ENHANCE PREMIUM EDIT
031011* 031011  CR2007070900001  PEMA  ADD FOLLOW-UP LETTER PROCESSING
100111* 100111  CR2011022800001  AJRA  NAPERSOFT VA DISC
010412* 010412  CR2011022800001  AJRA  NAPERSOFT
032612* 032612  CR2011110200001  PEMA  AHL CHANGES
062712* 062712  CR2011022800001  AJRA  REDEFINE ORIG DATA
072312* 072312  CR2011022800001  AJRA  NAPERSOFT MISC
091112* 091112  IR2012091100002  AJRA  FIX AKA
091312* 091312  IR2012091100004  AJRA  FIX ADDRESS CHECK
091712* 091712  IR2012091400003  AJRA  CHANGE PF6 TO GO TO CANCEL TRANS
100412* 100412  IR2012100300002  AJRA  FIX VA DISC FOR DISAB ONLY CERT
121712* 121712  CR2012101700002  AJRA  ADD DEFAULT AGE FLAG
011413* 011413  IR2012122700003  AJRA  ADD CRTO ISSUE/CANCEL INDICATOR
080713* 080713  CR2012032000002  AJRA  HIGHLIGHT AGE ERRORS
050713* 050713  CR2008042200001  PEMA  ADD CODE FOR ZERO APR
102813* 102813  IR2013102100001  PEMA  CORRECT CANCEL VOID STATUS
111913* 111913  CR2008042200001  PEMA  ADDITIONAL 0 % APR CHANGES
100213* 100213  CR2013090300001  AJRA  NAPERSOFT PHASE 2
111513* 111513  CR2013053000001  PEMA  DAILY CHECK REQUEST CHANGES
120513* 120513  CR2013090300001  AJRA  CHG ALL NINES IN PREM PROCESS
030314* 030314  IR2014030300003  PEMA  ADD CHK FOR STARTBR
031114* 031114  IR2014031100001  PEMA  CHECK FOR 0 CHECK AMOUNT
041514* 041514  CR2014032600001  PEMA  ALLOW DEL CNC IF VOID CHKS
051914* 051914  IR2014050500003  PEMA  ALLOW DEL CNC IF NO CHK ATTACHED
052814* 052814  CR2014012300001  PEMA  DCC CREDIT UNION CHANGES
091615* 091615  CR2015082000001  PEMA  add endorsement check processing
101615* 101615  CR2015080300002  PEMA  ALLOW VIN FOR CID
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
062017* 062017  CR2015091000001  PEMA  ADD PROCESSING FOR TN REF INTEREST
031620* 031620  CR2019092700001  PEMA  Remove post card indicator
122002******************************************************************
00027                                                                   
00028  ENVIRONMENT DIVISION.                                            
00029                                                                   
00030      EJECT                                                        
00031  DATA DIVISION.                                                   
00032  WORKING-STORAGE SECTION.                                         
00033  77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.   
00034  77  LCP-ONCTR-02                  PIC S9(8) COMP-3 VALUE ZERO.   
00035  77  FILLER  PIC X(32)  VALUE '********************************'. 
00036  77  FILLER  PIC X(32)  VALUE '*    VP6311 WORKING STORAGE    *'. 
00037  77  FILLER  PIC X(32)  VALUE '************VMOD=2.089**********'. 
101110 77  A1                          PIC S999 COMP-3 VALUE +0.
073107 77  N1                          PIC S999 COMP-3 VALUE +0.
073107 77  N2                          PIC S999 COMP-3 VALUE +0.
073107 77  N3                          PIC S999 COMP-3 VALUE +0.
073107 77  WS-OC-AMT                   PIC S9(9)V99 COMP-3 VALUE +0.
073107 77  WS-SAVE-ERPYAJ-KEY          PIC X(33) VALUE SPACES.
073107 77  WS-REVERSE-SW               PIC X VALUE 'N'.
           88  REVERSAL                    VALUE 'Y'.
       77  WS-NOTE-SW                  PIC X  VALUE ' '.
032210 77  WS-BEG                      PIC S999 COMP-3 VALUE +0.
032210 77  WS-END                      PIC S999 COMP-3 VALUE +0.
101110 77  WS-TALLY                    PIC 99  VALUE ZEROS.
       77  WS-ERMAIL-SW                PIC X  VALUE ' '.
           88  ERMAIL-FOUND                 VALUE 'Y'.
100111 77  A2                          PIC S9(5) COMP-3 VALUE +0.
100111 77  M1                          PIC S9(5) COMP-3 VALUE +0.
100111 77  WS-WORK-FIELD               PIC X(80)    VALUE SPACES.
121712 77  WS-CERT-TRL-REC-NOT-FOUND   PIC S9       VALUE +0.
121712     88  CERT-TRL-REC-NOT-FOUND     VALUE +1.
111513 77  ws-stop-sw                  pic x value ' '.
111513     88  i-say-stop                 value 'Y'.
111513 77  ws-check-sw                 pic x value ' '.
111513     88  i-have-checks              value 'Y'.
111513 77  ws-proc-check-sw            pic x value ' '.
111513     88  i-have-processed-checks    value 'Y'.
111513 77  ws-chek-browse-sw           pic x value ' '.
111513 77  ws-lf-diff                  pic s9(7)v99 comp-3 value +0.
111513 77  ws-ah-diff                  pic s9(7)v99 comp-3 value +0.
111513 77  ws-new-ref-total            pic s9(7)v99 comp-3 value +0.
091615 77  ws-new-iss-total            pic s9(7)v99 comp-3 value +0.
111513 77  ws-check-amts               pic s9(7)v99 comp-3 value +0.
091615 77  ws-tot-orig-prem            pic s9(7)v99 comp-3 value +0.

       01  P pointer.
       01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
       01  var-ptr pointer.
       01  env-var-len                 pic 9(4)  binary.
       01  rc                          pic 9(9)  binary.

       01  WS-KIXSYS.
           05  WS-KIX-FIL1             PIC X(10).
           05  WS-KIX-APPS             PIC X(10).
           05  WS-KIX-ENV              PIC X(10).
           05  WS-KIX-MYENV            PIC X(10).
           05  WS-KIX-SYS              PIC X(10).

00039      COPY ELCSCTM.                                                
00040                                                                   
00041      COPY ELCSCRTY.                                               
00042                                                                   
102408 01  WS-VA-NO-COMM-MESS.
102408     12  FILLER                  PIC X(41) VALUE
102408        'MAXIMUM AGE EXCEEDED - NO COMMISSION PAID'.
073107 01  WS-OC-MESSAGE.
           12  FILLER                  PIC X(43) VALUE
              'OVERCHARGE AUTO CORRECTED IN THE AMOUNT OF '.
           12  WS-OC-DIS-AMT           PIC -----9.99.
           12  FILLER                  PIC X(4) VALUE ' ON '.
           12  WS-OC-DATE              PIC X(10).
00043  01  WS-DATE-AREA.                                                
00044      12  WS-TEMP-EFF-DT          PIC XX     VALUE LOW-VALUES.     
00045      12  WS-CURRENT-DT.                                           
00046          16  FILLER              PIC X(6)    VALUE SPACES.        
00047          16  WS-CURRENT-YR       PIC XX      VALUE SPACES.        
00048      12  WS-CURRENT-BIN-DT       PIC XX      VALUE SPACES.        
00049      12  WS-CREDIT-SELECT-DT     PIC XX      VALUE SPACES.        
00050      12  WS-MONTH-END-DT         PIC X(8)    VALUE SPACES.        
00051                                                                   
00052  01  STANDARD-AREAS.                                              
00053      12  GETMAIN-SPACE           PIC X       VALUE SPACE.         
00054      12  VP631B                  PIC X(8)    VALUE 'VP631B'.      
00055      12  VP631C                  PIC X(8)    VALUE 'VP631C'.      
00056      12  VP631D                  PIC X(8)    VALUE 'VP631D'.      
00057      12  MAPSET-VP6311S          PIC X(8)    VALUE 'VP6311S'.     
00058      12  SCREEN-NUMBER           PIC X(6)    VALUE 'VP631B'.      
00059      12  TRANS-EXB1              PIC X(4)    VALUE 'VPB1'.        
00060      12  THIS-PGM                PIC X(8)    VALUE 'VP6311'.      
00061      12  PGM-NAME                PIC X(8).                        
00062      12  TIME-IN                 PIC S9(7).                       
00063      12  TIME-OUT-R  REDEFINES TIME-IN.                           
00064          16  FILLER              PIC X.                           
00065          16  TIME-OUT            PIC 99V99.                       
00066          16  FILLER              PIC XX.                          
00067      12  XCTL-EL001              PIC X(8)    VALUE 'EL001'.       
00068      12  XCTL-EL005              PIC X(8)    VALUE 'EL005'.       
00069      12  XCTL-EL010              PIC X(8)    VALUE 'EL010'.       
00070      12  XCTL-EL127              PIC X(8)    VALUE 'EL127'.       
00071      12  XCTL-EL1272             PIC X(8)    VALUE 'EL1272'.      
00072      12  XCTL-EL1273             PIC X(8)    VALUE 'EL1273'.      
00073      12  XCTL-EL1276             PIC X(8)    VALUE 'EL1276'.      
111109     12  XCTL-EL1279             PIC X(8)    VALUE 'EL1279'.
00074      12  XCTL-EL401DMD           PIC X(8)    VALUE 'EL401DMD'.    
00075      12  XCTL-EL626              PIC X(8)    VALUE 'EL626'.       
00076      12  XCTL-EL631              PIC X(8)    VALUE 'EL631'.       
00077      12  XCTL-EL6312             PIC X(8)    VALUE 'EL6312'.      
00078      12  XCTL-EL6313             PIC X(8)    VALUE 'EL6313'.      
00078      12  XCTL-EL6314             PIC X(8)    VALUE 'EL6314'.      
           12  XCTL-EL6315             PIC X(8)    VALUE 'EL6315'.
           12  XCTL-EL6316             PIC X(8)    VALUE 'EL6316'.
00079      12  XCTL-EL640              PIC X(8)    VALUE 'EL640'.       
00080      12  XCTL-EL650              PIC X(8)    VALUE 'EL650'.       
00081      12  XCTL-EL677              PIC X(8)    VALUE 'EL677'.       
00082      12  XCTL-EL680              PIC X(8)    VALUE 'EL680'.       
00083      12  XCTL-EL689              PIC X(8)    VALUE 'EL689'.       
00084      12  XCTL-EL690              PIC X(8)    VALUE 'EL690'.       
00085      12  LINK-EL001              PIC X(8)    VALUE 'EL001'.       
00086      12  LINK-EL004              PIC X(8)    VALUE 'EL004'.       
00087      12  LINK-EL050              PIC X(8)    VALUE 'EL050'.       
00088      12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.     
100111     12  LINK-ELVADS             PIC X(8)    VALUE 'ELVADS'.
00089      12  FILE-ID                 PIC X(8).                        
00090      12  FILE-ID-ELCERT          PIC X(8)    VALUE 'ELCERT'.      
00091      12  FILE-ID-ERPNDB          PIC X(8)    VALUE 'ERPNDB'.      
00092      12  FILE-ID-ERPNDB2         PIC X(8)    VALUE 'ERPNDB2'.     
00093      12  FILE-ID-ERPNDB3         PIC X(8)    VALUE 'ERPNDB3'.     
00094      12  FILE-ID-ERPNDB4         PIC X(8)    VALUE 'ERPNDB4'.     
00095      12  FILE-ID-ERPNDM          PIC X(8)    VALUE 'ERPNDM'.      
00096      12  FILE-ID-ERNOTE          PIC X(8)    VALUE 'ERNOTE'.      
111109     12  FILE-ID-ERCNOT          PIC X(8)    VALUE 'ERCNOT'.
073107     12  FILE-ID-ERPYAJ          PIC X(8)    VALUE 'ERPYAJ'.      
00097      12  FILE-ID-ERMAIL          PIC X(8)    VALUE 'ERMAIL'.      
00098      12  FILE-ID-ERRQST          PIC X(8)    VALUE 'ERRQST'.      
00099      12  FILE-ID-ERLOFC          PIC X(8)    VALUE 'ERLOFC'.      
062712     12  CLMS-ID                 PIC X(8)    VALUE 'ELMSTR5'. 
072312     12  FILE-ID-ERENDT          PIC X(8)    VALUE 'ERENDT'.
121712     12  CRTT-ID                 PIC X(8)    VALUE 'ELCRTT'.
00100                                                                   
       01  WS-PASS-AREA.
100111     05  WS-PASS-AREA-LENGTH PIC S9(4) COMP VALUE +352.
100111     05  WS-PASS-VADS-REC    PIC X(350).


      *** Z CONTROL LAYOUT MOVED TO COPYBOOK ELCZREC
                                       COPY ELCZREC.

00103 ******************************************************************
00104 *                                                                *
00105 *              A C C E S S   K E Y S                             *
00106 *                                                                *
00107 ******************************************************************
00108                                                                   
00109  01  ACCESS-KEYS.                                                 
073107     12  ERPYAJ-KEY.
073107         16  ERPYAJ-COMPANY-CD       PIC X.
073107         16  ERPYAJ-CARRIER          PIC X.
073107         16  ERPYAJ-GROUPING         PIC X(6).
073107         16  ERPYAJ-FIN-RESP         PIC X(10).
073107         16  ERPYAJ-ACCOUNT          PIC X(10).
073107         16  ERPYAJ-SEQ-NO           PIC S9(8) COMP.
073107         16  ERPYAJ-TYPE             PIC X.
00121      12  ERPYAJ-RECORD-LENGTH        PIC S9(4) COMP VALUE +200.

00110      12  ELCERT-KEY.                                              
00111          16  ELCERT-COMPANY-CD       PIC X.                       
00112          16  ELCERT-CARRIER          PIC X.                       
00113          16  ELCERT-GROUPING         PIC X(6).                    
00114          16  ELCERT-STATE            PIC XX.                      
00115          16  ELCERT-ACCOUNT          PIC X(10).                   
00116          16  ELCERT-CERT-EFF-DT      PIC XX.                      
00117          16  ELCERT-CERT-NO.                                      
00118              20  ELCERT-CERT-PRIME   PIC X(10).                   
00119              20  ELCERT-CERT-SFX     PIC X.                       
00120                                                                   
00121      12  ELCERT-RECORD-LENGTH        PIC S9(4) COMP VALUE +450.   
00122      12  ELCERT-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +473.   

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
121712
121712     12  ELCRTT-KEY.
121712         16  ELCRTT-PRIMARY          PIC X(33).
121712         16  ELCRTT-REC-TYPE         PIC X(1).
121712     12  ELCRTT-RECORD-LENGTH        PIC S9(4) COMP VALUE +552.
121712     12  ELCRTT-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +575.
121712

00124      12  ERPNDB-KEY.                                              
00125          16  ERPNDB-COMPANY-CD       PIC X        VALUE SPACE.    
00126          16  ERPNDB-ENTRY-BATCH      PIC X(6)     VALUE SPACES.   
00127          16  ERPNDB-BATCH-SEQ-NO     PIC S9(4)    VALUE +0 COMP.  
00128          16  ERPNDB-BATCH-SEQ-ALPHA  REDEFINES                    
00129              ERPNDB-BATCH-SEQ-NO     PIC XX.                      
00130          16  ERPNDB-BATCH-CHG-SEQ-NO PIC S9(4)    VALUE +0 COMP.  
00131                                                                   
00132      12  ERPNDB-RECORD-LENGTH        PIC S9(4) COMP VALUE +585.   
00133      12  ERPNDB-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +608.   

111513     12  ERCHEK-KEY.                                              
111513         16  CHEK-COMPANY-CD         PIC X       VALUE SPACE.     
111513         16  CHEK-CARRIER            PIC X       VALUE SPACE.     
111513         16  CHEK-GROUPING           PIC X(6)    VALUE SPACES.    
111513         16  CHEK-STATE              PIC XX      VALUE SPACES.    
111513         16  CHEK-ACCOUNT            PIC X(10)   VALUE SPACES.    
111513         16  CHEK-EFF-DT             PIC XX      VALUE SPACES.    
111513         16  CHEK-CERT-NO            PIC X(10)   VALUE SPACES.    
111513         16  CHEK-SUF-NO             PIC X       VALUE SPACES.    
111513         16  CHEK-RECORD-SEQ         PIC S9(4)   VALUE ZEROS COMP.

           12  ELLETR-KEY.
               16  LETR-COMPANY-CD     PIC X.
               16  LETR-LETTER-ID      PIC X(4).
               16  LETR-FILLER         PIC X(8).
               16  LETR-SEQ-NO         PIC 9(4) BINARY.

00135      12  ERPNDM-KEY.                                              
00136          16  ERPNDM-COMPANY-CD       PIC X        VALUE SPACE.    
00137          16  ERPNDM-ENTRY-BATCH      PIC X(6)     VALUE SPACES.   
00138          16  ERPNDM-BATCH-SEQ        PIC S9(4)    VALUE +1 COMP.  
00139          16  ERPNDM-BATCH-CHG-SEQ    PIC S9(4)    VALUE +0 COMP.  
00140                                                                   
CIDMOD*    12  ERPNDM-RECORD-LENGTH        PIC S9(4) COMP VALUE +250.   
CIDMOD     12  ERPNDM-RECORD-LENGTH        PIC S9(4) COMP VALUE +374.   
CIDMOD*    12  ERPNDM-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +273.   
CIDMOD     12  ERPNDM-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +397.   
00143                                                                   
00144      12  ERPNDB-ALT-KEY.                                          
00145          16  ERPNDB-COMPANY-CD-A1    PIC X        VALUE SPACE.    
00146          16  ERPNDB-CARRIER          PIC X        VALUE SPACES.   
00147          16  ERPNDB-GROUPING         PIC X(6)     VALUE SPACES.   
00148          16  ERPNDB-STATE            PIC XX       VALUE SPACES.   
00149          16  ERPNDB-ACCOUNT          PIC X(10)    VALUE SPACES.   
00150          16  ERPNDB-CERT-EFF-DT      PIC XX       VALUE SPACES.   
00151          16  ERPNDB-CERT-NO.                                      
00152              20  ERPNDB-CERT-PRIME   PIC X(10)    VALUE SPACES.   
00153              20  ERPNDB-CERT-SFX     PIC X        VALUE SPACE.    
00154          16  ERPNDB-ALT-CHG-SEQ-NO   PIC S9(4)    VALUE +0 COMP.  
00155          16  ERPNDB-RECORD-TYPE      PIC X.                       
00156                                                                   
00157       12  ERPNDB-CSR-KEY.                                         
00158           20  ERPNDB-CSR-COMPANY-CD-A2     PIC X.                 
00159           20  ERPNDB-CSR-ID                PIC X(4).              
00160           20  ERPNDB-CSR-ENTRY-BATCH       PIC X(6).              
00161           20  ERPNDB-CSR-BTCH-SEQ-NO       PIC S9(4)     COMP.    
00162           20  ERPNDB-CSR-BTCH-CHG-SEQ      PIC S9(4)     COMP.    
00163                                                                   
00164      12  ERNOTE-KEY.                                              
00165          16  ERNOTE-COMPANY-CD       PIC X        VALUE SPACE.    
00166          16  ERNOTE-CARRIER          PIC X        VALUE SPACES.   
00167          16  ERNOTE-GROUPING         PIC X(6)     VALUE SPACES.   
00168          16  ERNOTE-STATE            PIC XX       VALUE SPACES.   
00169          16  ERNOTE-ACCOUNT          PIC X(10)    VALUE SPACES.   
00170          16  ERNOTE-CERT-EFF-DT      PIC XX       VALUE SPACES.   
00171          16  ERNOTE-CERT-NO.                                      
00172              20  ERNOTE-CERT-PRIME   PIC X(10)    VALUE SPACES.   
00173              20  ERNOTE-CERT-SFX     PIC X        VALUE SPACES.   
00174                                                                   
00175      12  ERNOTE-RECORD-LENGTH        PIC S9(4) COMP VALUE +825.   
00176      12  ERNOTE-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +848.   
111109
111109     12  ERCNOT-KEY. 
111109         16  ERCNOT-PART-KEY.
111109             20  ERCNOT-COMPANY-CD   PIC X        VALUE SPACE.    
111109             20  ERCNOT-CARRIER      PIC X        VALUE SPACES.   
111109             20  ERCNOT-GROUPING     PIC X(6)     VALUE SPACES.   
111109             20  ERCNOT-STATE        PIC XX       VALUE SPACES.   
111109             20  ERCNOT-ACCOUNT      PIC X(10)    VALUE SPACES.   
111109             20  ERCNOT-CERT-EFF-DT  PIC XX       VALUE SPACES.   
111109             20  ERCNOT-CERT-NO.                                  
111109                 25  ERCNOT-CERT-PRIME PIC X(10)  VALUE SPACES.   
111109                 25  ERCNOT-CERT-SFX PIC X        VALUE SPACES.   
111109         16  ERCNOT-REC-TYPE         PIC X        VALUE SPACES.
111109         16  ERCNOT-SEQUENCE         PIC S9(4) COMP VALUE +0.
111109                                                                  
111109     12  ERCNOT-RECORD-LENGTH        PIC S9(4) COMP VALUE +150.   
111109     12  ERCNOT-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +173.   
00177                                                                   
00178      12  ERMAIL-KEY.                                              
00179          16  ERMAIL-COMPANY-CD       PIC X        VALUE SPACE.    
00180          16  ERMAIL-CARRIER          PIC X        VALUE SPACES.   
00181          16  ERMAIL-GROUPING         PIC X(6)     VALUE SPACES.   
00182          16  ERMAIL-STATE            PIC XX       VALUE SPACES.   
00183          16  ERMAIL-ACCOUNT          PIC X(10)    VALUE SPACES.   
00184          16  ERMAIL-CERT-EFF-DT      PIC XX       VALUE SPACES.   
00185          16  ERMAIL-CERT-NO.                                      
00186              20  ERMAIL-CERT-PRIME   PIC X(10)    VALUE SPACES.   
00187              20  ERMAIL-CERT-SFX     PIC X        VALUE SPACES.   
00188                                                                   
00189      12  ERMAIL-RECORD-LENGTH        PIC S9(4) COMP VALUE +374.   
00190      12  ERMAIL-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +393.   
00191                                                                   
00192      12  ERRQST-KEY.                                              
00193          16  ERRQST-COMPANY-CD       PIC X        VALUE SPACE.    
00194          16  ERRQST-ENTRY-BATCH      PIC X(6)     VALUE SPACES.   
00195                                                                   
00196      12  ERRQST-RECORD-LENGTH        PIC S9(4) COMP VALUE +200.   
00197      12  ERRQST-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +223.   
00198                                                                   
00199      12  WS-JOURNAL-RECORD-LENGTH    PIC S9(4) COMP VALUE +0000.  
00200                                                                   
00201      12  ERLOFC-KEY.                                              
00202          16  ERLOFC-COMPANY-CD          PIC  X.                   
00203          16  ERLOFC-CARRIER             PIC  X.                   
00204          16  ERLOFC-GROUPING            PIC  X(6).                
00205          16  ERLOFC-STATE               PIC  XX.                  
00206          16  ERLOFC-ACCOUNT             PIC  X(10).               
110105         16  ERLOFC-OFFICER-CODE        PIC  X(5).
00207 *        16  ERLOFC-OFFICER-CODE        PIC  XXX.                 
00208                                                                   
           12  ERACCT-KEY.                                              
               16  ERACCT-COMPANY-CD          PIC  X.                   
               16  ERACCT-CARRIER             PIC  X.                   
               16  ERACCT-GROUPING            PIC  X(6).                
               16  ERACCT-STATE               PIC  XX.                  
               16  ERACCT-ACCOUNT             PIC  X(10).               
               16  ERACCT-EXP-DT.
                   20  ERACCT-DT              PIC  XX.
                   20  ERACCT-FILL            PIC  X(4).
072312
072312     12  ERENDT-KEY.
072312         16  ENDT-COMPANY-CD       PIC X.
072312         16  ENDT-CARRIER          PIC X.
072312         16  ENDT-GROUPING         PIC X(6).
072312         16  ENDT-STATE            PIC XX.
072312         16  ENDT-ACCOUNT          PIC X(10).
072312         16  ENDT-CERT-EFF-DT      PIC XX.
072312         16  ENDT-CERT-PRIME       PIC X(10).
072312         16  ENDT-CERT-SFX         PIC X.
072312         16  ENDT-RECORD-TYPE      PIC X.
072312         16  ENDT-SEQ-NO           PIC 9(4) BINARY.
062712
062712 01  WS-ELMSTR-WORK.
062712     12  ELMSTR-KEY.                                              
062712         16  MSTR-COMP-CD         PIC X       VALUE LOW-VALUES.   
062712         16  MSTR-CERT-NO.
062712             20  MSTR-CERT-PRIME  PIC X(10)   VALUE SPACES.       
062712             20  MSTR-CERT-SFX    PIC X(1)    VALUE SPACES.       
062712     12  W-CLAIM-KEY.                                             
062712         16  W-CL-COMP-CD         PIC X       VALUE LOW-VALUES.   
062712         16  W-CL-CERT-NO.
062712             20  W-CL-CERT-PRIME  PIC X(10)   VALUE SPACES.       
062712             20  W-CL-CERT-SFX    PIC X(1)    VALUE SPACES.       
062712      12  ELMSTR-LENGTH           PIC S9999 COMP  VALUE +11.      

       01  CANCEL-GEN-PASS-AREA.
           05  CG-OPTION-CODE          PIC X.
               88  CG-VALID-OPTION       VALUE '1' '2' '3' '4'.
               88  CG-FLAT-CANCEL        VALUE '1'.
               88  CG-CANCEL             VALUE '2'.
               88  CG-CANCEL-REISSUE     VALUE '3'.
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
072312     05  FILLER                  PIC X(306).



00211 ******************************************************************
00212 *                                                                *
00213 *            G E N E R A L   W O R K   A R E A                   *
00214 *                                                                *
00215 ******************************************************************
00216                                                                   
00217  01  WORK-AREA.                                                   
111513     12  WS-RESPONSE             PIC S9(8)   COMP.
111513         88  RESP-NORMAL           VALUE +0.
111513         88  resp-file-notfnd      value +12.
111513         88  RESP-NOTFND           VALUE +13.
111513         88  resp-duprec           value +14.
111513         88  resp-dupkey           value +15.
111513         88  resp-invreq           value +16.
111513         88  RESP-NOTOPEN          VALUE +19.
111513         88  RESP-ENDFILE          VALUE +20.
111513         88  resp-lengtherr        value +22.

00218      12  DEEDIT-FIELD            PIC X(15).                       
00219      12  FILLER          REDEFINES DEEDIT-FIELD.                  
00220          16  FILLER              PIC X(4).                        
00221          16  DEEDIT-FIELD-X11    PIC X(11).                       
00222      12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD                   
00223                                  PIC S9(15).                      
CIDMOD     12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD                   
CIDMOD                                 PIC S9(13)V99.                   
CIDMOD     12  DEEDIT-FIELD-V4 REDEFINES DEEDIT-FIELD                   
CIDMOD                                 PIC S9(11)V9999.                 
CIDMOD     12  W-ONE                   PIC S9(4) COMP VALUE +1.         
00225      12  WS-CALC-TERM            PIC S999V9(5) VALUE ZEROS.       
00226      12  WS-CALC-TERM-R REDEFINES WS-CALC-TERM.                   
00227          16  WS-CALC-TERM-WHOLE  PIC S999.                        
00228          16  WS-CALC-TERM-REMAIN PIC SV9(5).                      
00229                                                                   
00230      12  WS-TIME                 PIC 9(6)   VALUE ZEROS.          
00231      12  WS-HR-MINS-SECS REDEFINES WS-TIME.                       
00232          16  WS-HR-MINS          PIC 99V99.                       
00233          16  FILLER              PIC XX.                          
00234                                                                   
00235      12  WS-EDIT-PASS-LENGTH     PIC S9(4)  VALUE +1036 COMP.     
00236      12  WS-COMM-LENGTH          PIC S9(4)  VALUE +1306 COMP.     
00237                                                                   
00238      12  W-CANCEL-FEE-LONG       PIC S9(3)V9(2)  VALUE +0  COMP-3.
00239      12  W-CANCEL-FEE            PIC S99         VALUE +0  COMP-3.
00240                                                                   
00241      12  WS-SUB                  PIC S9(4) VALUE +0  COMP-3.      
00242      12  WS-SUB1                 PIC S9(4) VALUE +0  COMP.        
00243      12  WS-SUB2                 PIC S9(4) VALUE +0  COMP.        
00244      12  WS-SUB3                 PIC S9(4) VALUE +0  COMP.        
00245      12  WS-SUB4                 PIC S9(4) VALUE +0  COMP.        
00246      12  WS-COV-SUB              PIC S9(4) VALUE +0  COMP.        
00247      12  WS-CSO-SUB              PIC S9(4) VALUE +0  COMP.        
00248                                                                   
00249      12  QID.                                                     
00250          16  QID-TERM            PIC X(4).                        
00251          16  FILLER              PIC X(4)    VALUE '607B'.        
00252                                                                   
00253      12  WS-DISPLAY-SW           PIC X     VALUE SPACE.           
00254          88  WS-DISPLAY-SCREEN       VALUE 'Y'.                   
00255                                                                   
00256      12  WS-LOAN-OFFICER         PIC X     VALUE SPACE.           
00257          88  LOAN-OFFICER-FOUND      VALUE SPACE.                 
00258                                                                   
00259      12  WS-MODE-CODE            PIC X     VALUE SPACE.           
00260          88 WS-MODE-CODE-VALID     VALUE ' ' 'M' 'W' 'S' 'B' 'T'
                                               'A'.
00261                                                                   
00262      12  WS-ALL-NINES            PIC S9(7)V99 VALUE +9999999.99.  
00263                                                                   
00264      12  WS-NUMERIC-SW           PIC X.                           
00265         88  CSO-NUMERIC-FIELD   VALUE 'Y'.                        
00266                                                                   
00267      12  WS-EDIT-CSO-FIELD.                                       
00268          16  WS-EDIT-BYTE  OCCURS 9 TIMES                         
00269                                  PIC X.                           
00270                                                                   
00271      12  WS-SAVE-INPUT-FIELDS.                                    
00272          16  WS-BAGE             PIC 99       VALUE ZERO.         
00273 *        16  WS-BAPR             PIC S99V9(4) VALUE +0 COMP-3.    

032210         16  WS-BAPR                 PIC 99V9(4) VALUE ZEROS.
032210         16  FILLER REDEFINES WS-BAPR.
032210             20  WS-APR-WHOLE-NUM    PIC 99.
032210             20  WS-APR-DEC          PIC 9999.


00274          16  WS-BLIVES           PIC S9(7)    VALUE +0 COMP-3.    
00275          16  WS-BILLED           PIC S9(7)    VALUE +0 COMP-3.    
00276                                                                   
00277          16  WS-B-COVERAGE OCCURS 2 TIMES.                        
00278              20  WS-TERM-IN-DAYS-SW  PIC X.                       
00279                  88  WS-TERM-IN-DAYS-FOUND        VALUE 'Y'.      
00280              20  WS-BTERM        PIC 999       COMP-3.            
00281              20  WS-BALT-TERM    PIC 999       COMP-3.            
00282              20  WS-BCRIT-PERD   PIC 99        COMP-3.            
00283              20  WS-BBEN         PIC S9(10)V99 COMP-3.            
00284              20  WS-BALT-BEN     PIC S9(10)V99 COMP-3.            
00285              20  WS-BPREM        PIC S9(7)V99  COMP-3.            
00286              20  WS-BALT-PREM    PIC S9(7)V99  COMP-3.            
00287              20  WS-BCPREM       PIC S9(7)V99  COMP-3.            
00288                                                                   
00289          16  WS-C-FIELDS COMP-3.                                  
00290              20  WS-CLIVES       PIC S9(3)     VALUE +0.          
00291              20  WS-CREFND1      PIC S9(7)V99  VALUE +0.          
00292              20  WS-CREFND2      PIC S9(7)V99  VALUE +0.          
00293                                                                   
00294          16  WS-D-FIELDS COMP-3.                                  
00295              20  WS-DRISS        PIC S9(5)    VALUE +0.           
00296              20  WS-DRLFPRM      PIC S9(7)V99 VALUE +0.           
00297              20  WS-DRAHPRM      PIC S9(7)V99 VALUE +0.           
00298              20  WS-DRCANC       PIC S9(5)    VALUE +0.           
00299              20  WS-DRLFRFD      PIC S9(7)V99 VALUE +0.           
00300              20  WS-DRAHRFD      PIC S9(7)V99 VALUE +0.           
00301                                                                   
00302      12  WS-SAVE-BIRTH-DATE.                                      
00303          16  FILLER              PIC X(4)   VALUE SPACES.         
00304          16  WS-SAVE-BIRTH-YR    PIC X(2)   VALUE SPACES.         
00305                                                                   
00306      12  CENTURY-ADJ             PIC S9(08) VALUE +38400 COMP.    
00307      12  WS-WORK-BIN-RED         PIC S9(08) VALUE +0 COMP.        
00308      12  FILLER REDEFINES WS-WORK-BIN-RED.                        
00309          16  FILLER              PIC X(02).                       
00310          16  WS-WORK-BIN-DT      PIC X(02).                       
00311      12  WS-SW-1                 PIC X     VALUE SPACE.           
00312      12  WS-SW-2                 PIC X     VALUE SPACE.           
00313      12  WS-REMOVE-AH-SW         PIC X     VALUE SPACE.           
00314          88  REMOVE-AH-COVERAGE  VALUE 'Y'.                       
00315      12  WS-REMOVE-LF-SW         PIC X     VALUE SPACE.           
00316          88  REMOVE-LF-COVERAGE  VALUE 'Y'.                       
00317                                                                   
00318      12  WS-CONVERTED-BIRTH      PIC XX    VALUE SPACE.           
00319      12  WS-CONVERTED-1ST-PMT-DT PIC XX    VALUE SPACE.           
00320      12  WS-CONVERTED-EFFDT      PIC XX    VALUE SPACES.          
00321      12  WS-CONVERTED-EXPIRDT      OCCURS 2 TIMES PIC XX.         
00322      12  WS-CONVERTED-CANCEL-DATES.                               
00323          16  WS-CONVERTED-CANDT1                  PIC XX.         
00324          16  WS-CONVERTED-CANDT2                  PIC XX.         
00325                                                                   
00326      12  WS-EXP-DT-EDIT          PIC 99B99B99.                    
00327                                                                   
00328      12  WS-REC-FOUND-SW         PIC X.                           
00329          88  RECORD-NOT-FOUND    VALUE 'N'.                       
00330                                                                   
00331      12  WS-PB-RECORDS-FOUND-SW  PIC X     VALUE 'N'.             
00332          88  PENDING-BUS-RECS-NOT-FOUND    VALUE 'N'.             
00333          88  PENDING-BUS-RECS-FOUND        VALUE 'Y'.             
00334                                                                   
00335      12  WS-ERRORS-PRESENT-SW    PIC X     VALUE 'N'.             
00336          88  WS-ERRORS-PRESENT             VALUE 'Y'.             
00337          88  WS-ERRORS-NOT-PRESENT         VALUE 'N'.             
00338                                                                   
00339      12  WS-BROWSE-STARTED-SW    PIC X            VALUE ' '.      
00340          88  BROWSE-STARTED      VALUE 'Y'.                       
00341                                                                   
00342      12  WS-FIRST-TIME-SW        PIC X            VALUE ' '.      
00343          88  FIRST-TIME          VALUE 'Y'.                       
00344                                                                   
00345      12  WS-FIRST-ENTRY-SW       PIC X            VALUE ' '.      
00346          88  FIRST-ENTRY         VALUE 'Y'.                       
00347                                                                   
00348      12  WS-REC-BILLED-SW        PIC X            VALUE ' '.      
00349          88  REC-BILLED          VALUE 'Y'.                       
00350                                                                   
00351      12  SET-CURSOR-SW           PIC S9  COMP-3   VALUE +0.       
00352          88  NO-CURSOR-SET       VALUE +0.                        
00353          88  CURSOR-SET          VALUE +1.                        
00354                                                                   
00355      12  WS-ERR-SW               PIC X.                           
00356          88  STD-ERRORS          VALUE 'S'.                       
00357          88  TRN-ERRORS          VALUE 'T'.                       
00358                                                                   
00359      12  WS-CERT-NOTE-SW         PIC X            VALUE ' '.      
00360          88 CERT-NOTES-ARE-PRESENT   VALUE 'Y'.                   
00361                                                                   
00362      12  WS-CERT-ADDRESS-SW      PIC X            VALUE ' '.      
00363          88 CERT-ADDRESS-PRESENT VALUE 'Y'.                       
010412
010412     12  WS-CANCEL-LETTER-SW     PIC X            VALUE ' '.
010412         88 CREATE-CANCEL-LETTER VALUE 'Y'.
00364                                                                   
00365      12  WS-ERR-CODE.                                             
00366          16  FILLER              PIC 99.                          
00367          16  WS-ERROR-SUB        PIC 99.                          
00368                                                                   
00369      12  WS-DATE-WORK.                                            
00370          16  WS-WORK-MM          PIC XX.                          
00371          16  WS-WORK-DD          PIC XX.                          
00372          16  WS-WORK-YY          PIC XX.                          
00373                                                                   
00374      12  WS-MM-YY.                                                
00375          16  WS-MM               PIC XX.                          
00376          16  FILLER              PIC X     VALUE '/'.             
00377          16  WS-YY               PIC XX.                          
00378                                                                   
00379      12  WS-CERT-EFF-DT          PIC XX.                          
00380                                                                   
00381      12  WS-MEMBER-NO            PIC X(12).                       
00382      12  FILLER  REDEFINES  WS-MEMBER-NO.                         
00383          16  WS-MEMBER-NO-1-8    PIC 9(8).                        
00384          16  FILLER              PIC X(4).                        
00385                                                                   
00386      12  WS-I-MICRO-NO           PIC S9(9)        COMP-3.         
00387                                                                   
00388      12  WS-ERPNDB-ALT-KEY.                                       
00389          16  WS-PB-COMPANY-CD-A1         PIC X.                   
00390          16  WS-PB-CARRIER               PIC X.                   
00391          16  WS-PB-GROUPING              PIC X(6).                
00392          16  WS-PB-STATE                 PIC XX.                  
00393          16  WS-PB-ACCOUNT               PIC X(10).               
00394          16  WS-PB-CERT-EFF-DT           PIC XX.                  
00395          16  WS-PB-CERT-NO.                                       
00396              20  WS-PB-CERT-PRIME        PIC X(10).               
00397              20  WS-PB-CERT-SFX          PIC X.                   
00398          16  WS-PB-ALT-CHG-SEQ-NO        PIC S9(4)     COMP.      
00399          16  WS-PB-RECORD-TYPE           PIC X.                   
00400                                                                   
00401      12  WS-SHOW-SAVE-AREAS.                                      
00402          16  WS-NEW-CERT-NO.                                      
00403              20  WS-NEW-PRIME    PIC X(10).                       
00404              20  WS-NEW-SUFFIX   PIC X.                           
00405          16  WS-NEW-CERT-EFF-DT  PIC XX.                          
00406          16  WS-NEW-RECORD-TYPE  PIC X.                           
00407          16  WS-SHOW-SW          PIC X   VALUE 'N'.               
00408              88   SHOW-RECORD            VALUE 'Y'.               
00409                                                                   
00410      12  WS-SEQ-SAVE             PIC S9(4) COMP.                  
00411                                                                   
00412      12  WS-BATCH-NO.                                             
00413          16  WS-BATCH-PREFIX     PIC XXX  VALUE SPACES.           
00414          16  FILLER              PIC X(5) VALUE SPACES.           
00415                                                                   
00416      12  WS-FIRST-NAME.                                           
00417          16  WS-1ST-INIT         PIC X.                           
00418          16  FILLER              PIC X(9).                        
00419                                                                   
00420      12  WS-INITIALS.                                             
00421          16  WS-INITIAL-1        PIC X.                           
00422          16  WS-INITIAL-2        PIC X.                           
00423                                                                   
00424      12  WS-C-HDG.                                                
00425          16  WS-REF-HDG-OVERRIDE PIC XX   VALUE SPACES.           
00426          16  FILLER              PIC X(4) VALUE '-REF'.           
00427                                                                   
00428      12  WS-D-PREM-HDG.                                           
00429          16  WS-PREM-HDG-OVERRIDE    PIC XX   VALUE SPACES.       
00430          16  FILLER                  PIC X(8) VALUE '-PREMIUM'.   
00431                                                                   
00432      12  WS-D-REFUND-HDG.                                         
00433          16  WS-REFUND-HDG-OVERRIDE  PIC XX   VALUE SPACES.       
00434          16  FILLER                  PIC X(7) VALUE '-REFUND'.    
00435                                                                   
00436      12  WS-BATCH-CSR-ID     PIC X(4)    VALUE SPACE.             
00437                                                                   
00438      12  CLIENT-ACE          PIC X(3)    VALUE 'ACE'.             
00439      12  CLIENT-CRI          PIC X(3)    VALUE 'CRI'.             
00440      12  CLIENT-CSO          PIC X(3)    VALUE 'CSO'.             
00441      12  CLIENT-GIC          PIC X(3)    VALUE 'GIC'.             
00442      12  CLIENT-LGX          PIC X(3)    VALUE 'LGX'.             
00443      12  CLIENT-MIC          PIC X(3)    VALUE 'MIC'.             
00444      12  CLIENT-PEM          PIC X(3)    VALUE 'PEM'.             
010412
010412     12  WS-01-CANCEL-ERR        PIC X(25)
010412         VALUE 'CANCEL DATE ERROR'.
010412     12  WS-02-CANCEL-ERR        PIC X(25)
010412         VALUE 'CERT NOT FOUND'.
010412     12  WS-04-CANCEL-ERR        PIC X(25)
010412         VALUE 'CANCEL AMOUNT ERROR'.
010412     12  WS-05-CANCEL-ERR        PIC X(25)
010412         VALUE 'CANCEL OPTION ERROR'.
010412     12  WS-06-CANCEL-ERR        PIC X(25)
010412         VALUE 'PREVIOUSLY CANCELLED'.
010412     12  WS-07-CANCEL-ERR        PIC X(25)
010412         VALUE 'INVALID DATA'.
010412     12  WS-08-CANCEL-ERR        PIC X(25)
010412         VALUE 'NO ACCOUNT MASTER'.
010412     12  WS-09-CANCEL-ERR        PIC X(25)
010412         VALUE 'SUFFIX ALREADY EXISTS'.
010412     12  WS-99-CANCEL-ERR        PIC X(25)
010412         VALUE 'MISC CANCEL ERROR'.
010412
010412     12  ARCH-SUPPRESS           PIC ZZZZZZZZ99.                  
010412     12  ARCH-EDIT REDEFINES ARCH-SUPPRESS    PIC X(10).          
121712
121712     12  WS-AGE-FIELDS.
121712         16  WS-INS-AGE-SET             PIC X VALUE 'N'.
121712         16  WS-JNT-AGE-SET             PIC X VALUE 'N'.
121712         16  WS-INS-AGE-DEFAULTED       PIC X VALUE 'N'.
121712         16  WS-JNT-AGE-DEFAULTED       PIC X VALUE 'N'.

00445                                                                   
00446      EJECT                                                        
00447                                                                   
00448 ******************************************************************
00449 *                                                                *
00450 *                 B A T C H   T O T A L S                        *
00451 *                                                                *
00452 ******************************************************************
00453                                                                   
00454      12  WS-BATCH-RECORD.                                         
00455          16  FILLER                       PIC X(10).              
00456          16  WS-B-LF-ISS-PRM-REMITTED     PIC S9(9)V99 COMP-3.    
00457          16  WS-B-LF-ISS-PRM-ENTERED      PIC S9(9)V99 COMP-3.    
00458          16  WS-B-LF-ISS-PRM-COMPUTED     PIC S9(9)V99 COMP-3.    
00459          16  WS-B-LF-CAN-PRM-REMITTED     PIC S9(9)V99 COMP-3.    
00460          16  WS-B-LF-CAN-PRM-ENTERED      PIC S9(9)V99 COMP-3.    
00461          16  WS-B-LF-CAN-PRM-COMPUTED     PIC S9(9)V99 COMP-3.    
00462          16  WS-B-AH-ISS-PRM-REMITTED     PIC S9(9)V99 COMP-3.    
00463          16  WS-B-AH-ISS-PRM-ENTERED      PIC S9(9)V99 COMP-3.    
00464          16  WS-B-AH-ISS-PRM-COMPUTED     PIC S9(9)V99 COMP-3.    
00465          16  WS-B-AH-CAN-PRM-REMITTED     PIC S9(9)V99 COMP-3.    
00466          16  WS-B-AH-CAN-PRM-ENTERED      PIC S9(9)V99 COMP-3.    
00467          16  WS-B-AH-CAN-PRM-COMPUTED     PIC S9(9)V99 COMP-3.    
00468          16  WS-B-ISSUE-CNT-REMITTED      PIC S9(5)    COMP-3.    
00469          16  WS-B-ISSUE-CNT-ENTERED       PIC S9(5)    COMP-3.    
00470          16  WS-B-CANCEL-CNT-REMITTED     PIC S9(5)    COMP-3.    
00471          16  WS-B-CANCEL-CNT-ENTERED      PIC S9(5)    COMP-3.    
00472          16  WS-B-HIGHEST-SEQ-NO          PIC S9(4)    COMP.      
00473          16  WS-ACCOUNT-NAME              PIC X(30).              
00474          16  FILLER                       PIC X.                  
00475          16  WS-REFERENCE                 PIC X(12).              
00476          16  FILLER                       PIC X(197).             
00477                                                                   
00478      12  WS-BATCH-TOTALS.                                         
00479          16  WS-ISS-TOTAL-CNT    PIC S9(5)     COMP-3 VALUE ZEROS.
00480          16  WS-ISS-FATAL-CNT    PIC S9(5)     COMP-3 VALUE ZEROS.
00481          16  WS-ISS-FORCE-CNT    PIC S9(5)     COMP-3 VALUE ZEROS.
00482          16  WS-ISS-HOLD-CNT     PIC S9(5)     COMP-3 VALUE ZEROS.
00483          16  WS-ISS-GOOD-PREM    PIC S9(7)V99  COMP-3 VALUE ZEROS.
00484          16  WS-ISS-BAD-PREM     PIC S9(7)V99  COMP-3 VALUE ZEROS.
00485          16  WS-CAN-TOTAL-CNT    PIC S9(5)     COMP-3 VALUE ZEROS.
00486          16  WS-CAN-FATAL-CNT    PIC S9(5)     COMP-3 VALUE ZEROS.
00487          16  WS-CAN-FORCE-CNT    PIC S9(5)     COMP-3 VALUE ZEROS.
00488          16  WS-CAN-HOLD-CNT     PIC S9(5)     COMP-3 VALUE ZEROS.
00489          16  WS-CAN-GOOD-PREM    PIC S9(7)V99  COMP-3 VALUE ZEROS.
00490          16  WS-CAN-BAD-PREM     PIC S9(7)V99  COMP-3 VALUE ZEROS.
00491          16  WS-NET-TOTAL-CNT    PIC S9(5)     COMP-3 VALUE ZEROS.
00492          16  WS-NET-FATAL-CNT    PIC S9(5)     COMP-3 VALUE ZEROS.
00493          16  WS-NET-FORCE-CNT    PIC S9(5)     COMP-3 VALUE ZEROS.
00494          16  WS-NET-HOLD-CNT     PIC S9(5)     COMP-3 VALUE ZEROS.
00495          16  WS-NET-AVAIL-CNT    PIC S9(5)     COMP-3 VALUE ZEROS.
00496          16  WS-NET-GOOD-PREM    PIC S9(7)V99  COMP-3 VALUE ZEROS.
00497          16  WS-NET-BAD-PREM     PIC S9(7)V99  COMP-3 VALUE ZEROS.
00498                                                                   
00499      12  WS-SEQ-NO-COUNTER       PIC S9(3)     COMP-3 VALUE ZEROS.
00500                                                                   
00501      EJECT                                                        
00502                                                                   
00503 ******************************************************************
00504 *                                                                *
00505 *                E R R O R   M E S S A G E S                     *
00506 *                                                                *
00507 ******************************************************************
00508                                                                   
00509  01  ERROR-MESSAGES.                                              
00510      12  ER-0004                 PIC  9(4) VALUE 0004.            
00511      12  ER-0008                 PIC  9(4) VALUE 0008.            
00512      12  ER-0023                 PIC  9(4) VALUE 0023.            
00513      12  ER-0029                 PIC  9(4) VALUE 0029.            
00514      12  ER-0033                 PIC  9(4) VALUE 0033.            
00515      12  ER-0070                 PIC  9(4) VALUE 0070.            
00516      12  ER-0220                 PIC  9(4) VALUE 0220.            
030708     12  ER-0248                 PIC  9(4) VALUE 0248.
010412     12  ER-0280                 PIC  9(4) VALUE 0280.
00517      12  ER-0348                 PIC  9(4) VALUE 0348.            
00518      12  ER-0582                 PIC  9(4) VALUE 0582.
030708     12  ER-0877                 PIC  9(4) VALUE 0877.
           12  ER-1236                 PIC  9(4) VALUE 1236.
020711     12  ER-1570                 PIC  9(4) VALUE 1570.
020711     12  ER-1575                 PIC  9(4) VALUE 1575.
072312     12  ER-1609                 PIC  9(4) VALUE 1609.
           12  ER-1819                 PIC  9(4) VALUE 1819.
080713     12  ER-2077                 PIC  9(4) VALUE 2077.
00519      12  ER-2129                 PIC  9(4) VALUE 2129.            
00520      12  ER-2188                 PIC  9(4) VALUE 2188.            
00521      12  ER-2192                 PIC  9(4) VALUE 2192.            
00522      12  ER-2196                 PIC  9(4) VALUE 2196.            
00523      12  ER-2197                 PIC  9(4) VALUE 2197.            
00524      12  ER-2198                 PIC  9(4) VALUE 2198.            
00525      12  ER-2200                 PIC  9(4) VALUE 2200.            
00526      12  ER-2212                 PIC  9(4) VALUE 2212.            
00527      12  ER-2218                 PIC  9(4) VALUE 2218.            
00528      12  ER-2220                 PIC  9(4) VALUE 2220.            
00529      12  ER-2222                 PIC  9(4) VALUE 2222.            
00530      12  ER-2223                 PIC  9(4) VALUE 2223.            
00531      12  ER-2224                 PIC  9(4) VALUE 2224.            
00532      12  ER-2225                 PIC  9(4) VALUE 2225.            
00533      12  ER-2226                 PIC  9(4) VALUE 2226.            
00534      12  ER-2227                 PIC  9(4) VALUE 2227.            
00535      12  ER-2228                 PIC  9(4) VALUE 2228.            
00536      12  ER-2240                 PIC  9(4) VALUE 2240.            
00537      12  ER-2241                 PIC  9(4) VALUE 2241.            
00538      12  ER-2242                 PIC  9(4) VALUE 2242.            
00539      12  ER-2243                 PIC  9(4) VALUE 2243.            
00540      12  ER-2244                 PIC  9(4) VALUE 2244.            
00541      12  ER-2245                 PIC  9(4) VALUE 2245.            
00542      12  ER-2246                 PIC  9(4) VALUE 2246.            
00543      12  ER-2247                 PIC  9(4) VALUE 2247.            
00544      12  ER-2251                 PIC  9(4) VALUE 2251.            
00545      12  ER-2252                 PIC  9(4) VALUE 2252.            
00546      12  ER-2253                 PIC  9(4) VALUE 2253.            
00547      12  ER-2254                 PIC  9(4) VALUE 2254.            
00548      12  ER-2255                 PIC  9(4) VALUE 2255.            
00549      12  ER-2256                 PIC  9(4) VALUE 2256.            
00550      12  ER-2257                 PIC  9(4) VALUE 2257.            
00551      12  ER-2258                 PIC  9(4) VALUE 2258.            
00552      12  ER-2259                 PIC  9(4) VALUE 2259.            
00553      12  ER-2260                 PIC  9(4) VALUE 2260.            
00554      12  ER-2261                 PIC  9(4) VALUE 2261.            
00555      12  ER-2287                 PIC  9(4) VALUE 2287.            
00556      12  ER-2336                 PIC  9(4) VALUE 2336.            
00557      12  ER-2337                 PIC  9(4) VALUE 2337.            
00558      12  ER-2437                 PIC  9(4) VALUE 2437.            
032210     12  ER-2471                 PIC  9(4) VALUE 2471.
00559      12  ER-2530                 PIC  9(4) VALUE 2530.            
00560      12  ER-2531                 PIC  9(4) VALUE 2531.            
00561      12  ER-2532                 PIC  9(4) VALUE 2532.            
00562      12  ER-2533                 PIC  9(4) VALUE 2533.            
00563      12  ER-2547                 PIC  9(4) VALUE 2547.            
00564      12  ER-2591                 PIC  9(4) VALUE 2591.            
00565      12  ER-2592                 PIC  9(4) VALUE 2592.            
00566      12  ER-2593                 PIC  9(4) VALUE 2593.            
00567      12  ER-2600                 PIC  9(4) VALUE 2600.            
00568      12  ER-2601                 PIC  9(4) VALUE 2601.            
00569      12  ER-2604                 PIC  9(4) VALUE 2604.            
00570      12  ER-2605                 PIC  9(4) VALUE 2605.            
00571      12  ER-2606                 PIC  9(4) VALUE 2606.            
00572      12  ER-2607                 PIC  9(4) VALUE 2607.            
00573      12  ER-2622                 PIC  9(4) VALUE 2622.            
00574      12  ER-2625                 PIC  9(4) VALUE 2625.            
00575      12  ER-2626                 PIC  9(4) VALUE 2626.            
00576      12  ER-2627                 PIC  9(4) VALUE 2627.            
00577      12  ER-2628                 PIC  9(4) VALUE 2628.            
00578      12  ER-2629                 PIC  9(4) VALUE 2629.            
00579      12  ER-2630                 PIC  9(4) VALUE 2630.            
00580      12  ER-2631                 PIC  9(4) VALUE 2631.            
00581      12  ER-2632                 PIC  9(4) VALUE 2632.            
00582      12  ER-2633                 PIC  9(4) VALUE 2633.            
00583      12  ER-2634                 PIC  9(4) VALUE 2634.            
00584      12  ER-2635                 PIC  9(4) VALUE 2635.            
00585      12  ER-2636                 PIC  9(4) VALUE 2636.            
00586      12  ER-2639                 PIC  9(4) VALUE 2639.            
00587      12  ER-2640                 PIC  9(4) VALUE 2640.            
00588      12  ER-2647                 PIC  9(4) VALUE 2647.            
00589      12  ER-2648                 PIC  9(4) VALUE 2648.            
00590      12  ER-2649                 PIC  9(4) VALUE 2649.            
00591      12  ER-2650                 PIC  9(4) VALUE 2650.            
00592      12  ER-2653                 PIC  9(4) VALUE 2653.            
00593      12  ER-2654                 PIC  9(4) VALUE 2654.            
00594      12  ER-2655                 PIC  9(4) VALUE 2655.            
00595      12  ER-2656                 PIC  9(4) VALUE 2656.            
00596      12  ER-2657                 PIC  9(4) VALUE 2657.            
00597      12  ER-2660                 PIC  9(4) VALUE 2660.            
080713     12  ER-2661                 PIC  9(4) VALUE 2661.
00598      12  ER-2662                 PIC  9(4) VALUE 2662.            
00599      12  ER-2663                 PIC  9(4) VALUE 2663.            
00600      12  ER-2664                 PIC  9(4) VALUE 2664.            
00601      12  ER-2667                 PIC  9(4) VALUE 2667.            
00602      12  ER-2668                 PIC  9(4) VALUE 2668.            
00603      12  ER-2669                 PIC  9(4) VALUE 2669.            
00604      12  ER-2670                 PIC  9(4) VALUE 2670.            
00605      12  ER-2671                 PIC  9(4) VALUE 2671.            
00606      12  ER-2672                 PIC  9(4) VALUE 2672.            
00607      12  ER-2673                 PIC  9(4) VALUE 2673.            
00608      12  ER-2677                 PIC  9(4) VALUE 2677.            
00609      12  ER-2678                 PIC  9(4) VALUE 2678.            
00610      12  ER-2679                 PIC  9(4) VALUE 2679.            
00611      12  ER-2680                 PIC  9(4) VALUE 2680.            
00612      12  ER-2681                 PIC  9(4) VALUE 2681.            
00613      12  ER-2682                 PIC  9(4) VALUE 2682.            
00614      12  ER-2683                 PIC  9(4) VALUE 2683.            
00615      12  ER-2684                 PIC  9(4) VALUE 2684.            
00616      12  ER-2686                 PIC  9(4) VALUE 2686.            
00617      12  ER-2689                 PIC  9(4) VALUE 2689.            
00618      12  ER-2690                 PIC  9(4) VALUE 2690.            
00619      12  ER-2693                 PIC  9(4) VALUE 2693.            
00620      12  ER-2695                 PIC  9(4) VALUE 2695.            
00621      12  ER-2701                 PIC  9(4) VALUE 2701.            
00621      12  ER-2720                 PIC  9(4) VALUE 2720.            
00621      12  ER-2709                 PIC  9(4) VALUE 2709.            
00622      12  ER-2704                 PIC  9(4) VALUE 2704.            
00623      12  ER-2725                 PIC  9(4) VALUE 2725.            
00624      12  ER-2726                 PIC  9(4) VALUE 2726.            
00625      12  ER-2728                 PIC  9(4) VALUE 2728.            
00626      12  ER-2729                 PIC  9(4) VALUE 2729.            
00627      12  ER-2731                 PIC  9(4) VALUE 2731.            
00628      12  ER-2732                 PIC  9(4) VALUE 2732.            
00629      12  ER-2733                 PIC  9(4) VALUE 2733.            
00630      12  ER-2735                 PIC  9(4) VALUE 2735.            
00631      12  ER-2738                 PIC  9(4) VALUE 2738.            
00632      12  ER-2742                 PIC  9(4) VALUE 2742.            
00633      12  ER-2743                 PIC  9(4) VALUE 2743.            
00634      12  ER-2744                 PIC  9(4) VALUE 2744.            
00635      12  ER-2747                 PIC  9(4) VALUE 2747.            
00636      12  ER-2749                 PIC  9(4) VALUE 2749.            
00637      12  ER-2753                 PIC  9(4) VALUE 2753.            
00638      12  ER-2755                 PIC  9(4) VALUE 2755.            
00639      12  ER-2757                 PIC  9(4) VALUE 2757.            
00640      12  ER-2771                 PIC  9(4) VALUE 2771.            
00641      12  ER-2774                 PIC  9(4) VALUE 2774.            
00642      12  ER-2775                 PIC  9(4) VALUE 2775.            
00643      12  ER-2776                 PIC  9(4) VALUE 2776.            
00644      12  ER-2777                 PIC  9(4) VALUE 2777.            
00645      12  ER-2794                 PIC  9(4) VALUE 2794.            
00646      12  ER-2799                 PIC  9(4) VALUE 2799.            
00647      12  ER-2900                 PIC  9(4) VALUE 2900.            
00648      12  ER-2902                 PIC  9(4) VALUE 2902.            
00649      12  ER-2945                 PIC  9(4) VALUE 2945.            
072312     12  ER-3000                 PIC  9(4) VALUE 3000.
00650      12  ER-3006                 PIC  9(4) VALUE 3006.            
080713     12  ER-3054                 PIC  9(4) VALUE 3054.
00651      12  ER-3224                 PIC  9(4) VALUE 3224.            
100213     12  ER-3269                 PIC  9(4) VALUE 3269.
111513     12  er-3444                 pic  9(4) value 3444.
111513     12  er-3446                 pic  9(4) value 3446.
111513     12  er-3447                 pic  9(4) value 3447.
111513     12  er-3448                 pic  9(4) value 3448.
111513     12  er-3449                 pic  9(4) value 3449.
091615     12  ER-3456                 PIC  9(4) VALUE 3456.
091615     12  ER-3457                 PIC  9(4) VALUE 3457.
091615     12  ER-3458                 PIC  9(4) VALUE 3458.
080713     12  ER-3504                 PIC  9(4) VALUE 3504.
080713     12  ER-3513                 PIC  9(4) VALUE 3513.
062712     12  ER-3831                 PIC  9(4) VALUE 3831.
072312     12  ER-3834                 PIC  9(4) VALUE 3834.
072312     12  ER-3835                 PIC  9(4) VALUE 3835.
00652      12  ER-4005                 PIC  9(4) VALUE 4005.            
080713     12  ER-7144                 PIC  9(4) VALUE 7144.
00653      12  ER-7401                 PIC  9(4) VALUE 7401.            
00654      12  ER-7402                 PIC  9(4) VALUE 7402.            
00655      12  ER-7403                 PIC  9(4) VALUE 7403.            
00656      12  ER-7404                 PIC  9(4) VALUE 7404.            
00657      12  ER-7423                 PIC  9(4) VALUE 7423.            
00658      12  ER-7424                 PIC  9(4) VALUE 7424.            
00659      12  ER-7433                 PIC  9(4) VALUE 7433.            
00660      12  ER-7450                 PIC  9(4) VALUE 7450.            
00661      12  ER-7451                 PIC  9(4) VALUE 7451.            
00662      12  ER-7530                 PIC  9(4) VALUE 7530.            
00663      12  ER-7573                 PIC  9(4) VALUE 7573.            
00664      12  ER-7631                 PIC  9(4) VALUE 7631.            
00665      12  ER-7632                 PIC  9(4) VALUE 7632.            
00666      12  ER-7633                 PIC  9(4) VALUE 7633.            
           12  ER-7537                 PIC  9(4) VALUE 7537.
           12  ER-7538                 PIC  9(4) VALUE 7538.
00667      12  ER-7574                 PIC  9(4) VALUE 7574.            
00668      12  ER-7997                 PIC  9(4) VALUE 7997.            
00669      12  ER-7998                 PIC  9(4) VALUE 7998.            
00670      12  ER-8148                 PIC  9(4) VALUE 8148.
080713     12  ER-9063                 PIC  9(4) VALUE 9063.
080713     12  ER-9076                 PIC  9(4) VALUE 9076.
080713     12  ER-9618                 PIC  9(4) VALUE 9618.
101010     12  ER-9841                 PIC  9(4) VALUE 9841.
00671                                                                   

                                       COPY ELCTEXT.
                                       COPY ELCCRTO.
030708                                 COPY ERCACCT.
100111                                 COPY ELCVADS.

00672      EJECT                                                        
00673      COPY ELCDATE.                                                
00674                                                                   
00675      EJECT                                                        
00676      COPY ELCLOGOF.                                               
00677                                                                   
00678      EJECT                                                        
00679      COPY ELCATTR.                                                
00680                                                                   
00681      EJECT                                                        
00682      COPY ELCEMIB.                                                
00683                                                                   
00684      EJECT                                                        
00685      COPY ELCINTF.                                                
00686                                                                   
00687      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.                   
00688      COPY ELC631PI.                                               
00689         16  FILLER             PIC X(94).                        
00690                                                                   
00691  01  W-LETTER-COMMUNICATION-AREAS.                                
00692      COPY ELC1042.                                                
00693      COPY ELC689PI.                                               
00694      EJECT                                                        
062712
062712 01  WS-EL6315-PASS-AREA.
062712        16  PI-6315-ISS-CAN-SW      PIC X.
062712        16  FILLER                  PIC X(120).
062712        16  PI-RES-REF-CLM-TYPE     PIC X.
062712        16  FILLER                  PIC X.
072312        16  PI-6315-PASS-AREA.
072312            20 FILLER               PIC X(236).
072312            20 PI-6315-FINALIZED    PIC X.
072312            20 FILLER               PIC X(280).
062712
062712 01  WS-EL6316-PASS-AREA.
062712        16  PI-ISS-CAN-SW           PIC X.
062712        16  FILLER                  PIC X(121).
062712        16  PI-CLM-RESC-IND         PIC X.
072312        16  PI-6316-PASS-AREA.
072312            20 FILLER               PIC X(236).
072312            20 PI-6316-FINALIZED    PIC X.
072312            20 FILLER               PIC X(280).
00695                                                                   
00696 ******************************************************************
00697 *                                                                *
00698 *         P A S S   A R E A   F O R   E D I T                    *
00699 *                                                                *
00700 ******************************************************************
00701                                                                   
00702  01  PASSED-RECORD.                                               
00703      12  RECORD-SAVE                  PIC X(585).                 
00704 * COPYBOOK FOR ADDITIONAL DFHCOMMAREA WK-WORK-AREA.               
00705      COPY ELC50W1 REPLACING WK-CANCEL-EXIT-DT                     
00706                      BY  WK-CURRENT-DATE.                         
00707      EJECT                                                        
00708                                                                   
00709      COPY ELCEDITC.                                               
00710                                                                   
00711 ******************************************************************
00712                                                                   
00713      EJECT                                                        
00714                                                                   
00715      COPY ELCJPFX.                                                
092315                             PIC X(825).                          
00717      EJECT                                                        
00718      COPY ELCAID.                                                 
00719                                                                   
00720  01  FILLER    REDEFINES DFHAID.                                  
00721      12  FILLER              PIC X(8).                            
00722      12  PF-VALUES           PIC X       OCCURS 24 TIMES.         
00723                                                                   

00725                                  COPY VP6311S.

00727  01  MAP-B REDEFINES VP631BI.                                     
121712     12  FILLER                  PIC X(569).
110105*    12  FILLER                  PIC X(512).                      
00728 *    12  FILLER                  PIC X(510).                      
00729      12  MAP-B-COVERAGE OCCURS 2 TIMES.                           
00730          16  BKIND-LEN               PIC S9(4)  COMP.             
00731          16  BKIND-ATTRB             PIC X.                       
00732          16  BKIND                   PIC XX.                      
00733          16  BTYPE-LEN               PIC S9(4)  COMP.             
00734          16  BTYPE-ATTRB             PIC X.                       
00735          16  BTYPE                   PIC X(3).                    
00736          16  BTERM-LEN               PIC S9(4)  COMP.             
00737          16  BTERM-ATTRB             PIC X.                       
00738          16  BTERMI                  PIC 999.                     
00739          16  BTERMO REDEFINES BTERMI PIC ZZZ.                     
00740          16  BBEN-LEN                PIC S9(4)  COMP.             
00741          16  BBEN-ATTRB              PIC X.                       
CIDMOD         16  BBENI                   PIC 9(10)V99.                
CIDMOD*        16  BBENI                   PIC 9(12).                   
00743          16  BBENO REDEFINES BBENI   PIC Z(9).99.                 
00744          16  BPREM-LEN               PIC S9(4)  COMP.             
00745          16  BPREM-ATTRB             PIC X.                       
CIDMOD         16  BPREMI                  PIC S9(9)V99.                
CIDMOD*        16  BPREMI                  PIC 9(11).                   
00747          16  BPREMO REDEFINES BPREMI PIC Z(7).99-.
101110         16  BPREM-ALPHA REDEFINES BPREMI PIC X(11).
00748          16  BEXPIRE-LEN             PIC S9(4)  COMP.             
00749          16  BEXPIRE-ATTRB           PIC X.                       
00750          16  BEXPIRE                 PIC X(8).                    
00751          16  BCRIT-PERD-LEN          PIC S9(4)  COMP.             
00752          16  BCRIT-PERD-ATTRB        PIC X.                       
00753          16  BCRIT-PERDI             PIC 99.                      
00754          16  BCRIT-PERDO REDEFINES                                
00755                          BCRIT-PERDI PIC ZZ.                      
00756          16  BALT-BEN-LEN            PIC S9(4)  COMP.             
00757          16  BALT-BEN-ATTRB          PIC X.                       
CIDMOD         16  BALT-BENI               PIC 9(10)V99.                
CIDMOD*        16  BALT-BENI               PIC 9(12).                   
00759          16  BALT-BENO REDEFINES                                  
00760                            BALT-BENI PIC Z(9).ZZ.                 
00761          16  BALT-PREM-LEN           PIC S9(4)  COMP.             
00762          16  BALT-PREM-ATTRB         PIC X.                       
CIDMOD         16  BALT-PREMI              PIC 9(9)V99.                 
CIDMOD*        16  BALT-PREMI              PIC 9(11).                   
00764          16  BALT-PREMO REDEFINES                                 
00765                           BALT-PREMI PIC Z(7).ZZ-.                
00766          16  BBEN-CD-LEN             PIC S9(4)  COMP.             
00767          16  BBEN-CD-ATTRB           PIC X.                       
00768          16  BBEN-CD                 PIC XXX.                     
00769          16  BCPREM-LEN              PIC S9(4)  COMP.             
00770          16  BCPREM-ATTRB            PIC X.                       
CIDMOD         16  BCPREMI                 PIC S9(9)V99.                
CIDMOD*        16  BCPREMI                 PIC X(11).                   
00772          16  BCPREMO REDEFINES BCPREMI                            
00773                                      PIC Z(7).99-.                
00774          16  BCALT-PREM-LEN          PIC S9(4)  COMP.             
00775          16  BCALT-PREM-ATTRB        PIC X.                       
00776          16  BCALT-PREM              PIC Z(7).99-.                
00777          16  BBEN-ABBR-LEN           PIC S9(4)  COMP.             
00778          16  BBEN-ABBR-ATTRB         PIC X.                       
00779          16  BBEN-ABBR               PIC X(5).                    
00780          16  BDIFF-LEN               PIC S9(4)  COMP.             
00781          16  BDIFF-ATTRB             PIC X.                       
00782          16  BDIFFO                  PIC Z(7).99-.                
00783          16  BBILL-LIT-LEN           PIC S9(4)  COMP.             
00784          16  BBILL-LIT-ATTRB         PIC X.                       
00785          16  BBILL-LIT               PIC X(8).                    
00786          16  BDALT-PREM-LEN          PIC S9(4)  COMP.             
00787          16  BDALT-PREM-ATTRB        PIC X.                       
00788          16  BDALT-PREM              PIC Z(7).99-.                
00789                                                                   
00790      EJECT                                                        
00791                                                                   
00792  LINKAGE SECTION.                                                 
00793  01  DFHCOMMAREA                 PIC X(1300).                     

00796                                  COPY ERCPNDB.
00799                                  COPY ELCCERT.
00802                                  COPY ERCNOTE.
00805                                  COPY ERCPNDM.
00808                                  COPY ERCMAIL.
00811                                  COPY ERCRQST.
00814                                  COPY ERCLOFC.
073107                                 COPY ERCPYAJ.
111109                                 COPY ERCCNOT.
062712                                 COPY ELCMSTR.
072312                                 COPY ERCENDT.
121712                                 COPY ELCCRTT.
                                       COPY ERCCHEK.
       01  var                         pic x(30).

00818  PROCEDURE DIVISION.                                              
00819                                                                   
00820      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      
00821      MOVE EIBTRMID               TO QID-TERM.                     
00822      MOVE 3                      TO EMI-NUMBER-OF-LINES.          
00823      MOVE PI-LIFE-OVERRIDE-L6    TO EMI-LIFE-OVERRIDE-L6.         
00824      MOVE PI-AH-OVERRIDE-L6      TO EMI-AH-OVERRIDE-L6.           
00825      MOVE 2                      TO EMI-SWITCH2.                  
00826                                                                   
           move spaces to ws-kix-myenv
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
           perform varying a1 from +1 by +1 until a1 > +10
              if ws-kix-myenv (a1:1) = low-values or high-values
                 display ' found low or hi val '
                 move spaces to ws-kix-myenv (a1:1)
              end-if
           end-perform

      *    display ' env *' ws-kix-myenv '*'

00827      IF EIBCALEN = 0                                              
00828          GO TO 8800-UNAUTHORIZED-ACCESS.                          
00829                                                                   
00830      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               
00831      MOVE '5'                   TO DC-OPTION-CODE.                
00832      PERFORM 9700-DATE-LINK.                                      
00833      MOVE DC-GREG-DATE-1-EDIT   TO  WS-CURRENT-DT.                
00834      MOVE DC-BIN-DATE-1         TO  WS-CURRENT-BIN-DT.            
00835                                                                   
00836      MOVE PI-CR-MONTH-END-DT     TO DC-BIN-DATE-1.                
00837      MOVE SPACE                  TO DC-OPTION-CODE.               
00838      PERFORM 9700-DATE-LINK.                                      
00839      MOVE DC-GREG-DATE-1-EDIT    TO WS-MONTH-END-DT.              
00840      MOVE LOW-VALUES             TO VP631BI.                      
00841                                                                   
00842      EXEC CICS HANDLE CONDITION                                   
00843          PGMIDERR  (9600-PGMID-ERROR)                             
00844          ERROR     (9990-ABEND)                                   
00845          INVREQ    (9990-ABEND)                                   
00846          QIDERR    (0010-MAIN-LOGIC)                              
00847      END-EXEC.                                                    
00848                                                                   
00849      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         
00850          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   
00851              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      
00852              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      
00853              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      
00854              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      
00855              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      
00856              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      
00857              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    
00858              MOVE THIS-PGM             TO PI-CALLING-PROGRAM      
00859                                                                   
00860              IF  PI-RETURN-TO-PROGRAM EQUAL 'EL631'               
00861                  PERFORM 6050-DELETE-TEMP-STORAGE THRU 6050-EXIT  
00862                                                                   
00863              ELSE                                                 
00864                  NEXT SENTENCE                                    
00865                                                                   
00866          ELSE                                                     
00867              MOVE PI-CALLING-PROGRAM   TO PGM-NAME                
00868              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      
00869              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    
00870              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      
00871              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      
00872              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      
00873              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      
00874              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      
00875              MOVE SPACES               TO PI-SAVED-PROGRAM-6      
00876              PERFORM 6050-RECOVER-TEMP-STORAGE THRU 6050-EXIT     
00877              MOVE LOW-VALUES TO EC-NEW-CERT EC-NEW-STATE          
00878              GO TO 3900-RE-DISPLAY-RECORD                         
00879      ELSE                                                         
00880          GO TO 0020-MAIN-LOGIC.                                   
00881                                                                   
00882  0010-MAIN-LOGIC.                                                 
00883      MOVE 'Y'                    TO WS-FIRST-TIME-SW              
00884                                     WS-FIRST-ENTRY-SW.            
010412
010412     MOVE ' '                    TO WS-CANCEL-LETTER-SW.
00885                                                                   
00886      MOVE LOW-VALUES             TO EC-NEW-CERT                   
00887                                     EC-NEW-STATE.                 
00888                                                                   
00889      IF PI-DISPLAY-ORIGINAL-BATCH                                 
00890          NEXT SENTENCE                                            
00891      ELSE                                                         
00892          IF ALL-OUT-OF-BAL   OR  ONLY-BATCH-HEADERS               
00893              GO TO 5000-BROWSE-BATCH-HEADERS.                     
00894                                                                   
00895      IF PI-PRIMARY-BROWSE OR PI-PRIMARY-WITH-SELECT               
00896          GO TO 4100-BROWSE-PRIMARY-FORWARD.                       
00897                                                                   
00898      IF PI-CSR-BROWSE                                             
00899         GO TO 4200-BROWSE-CSR-FORWARD.                            
00900                                                                   
00901      IF PI-FILE-BROWSE                                            
00902          GO TO 4300-BROWSE-FILE-FORWARD.                          
00903                                                                   
00904      GO TO 4000-BROWSE-DETAIL-FORWARD.                            
00905                                                                   
00906  0020-MAIN-LOGIC.                                                 
00907      IF EIBAID = DFHCLEAR                                         
00908          GO TO 9400-CLEAR.                                        
00909                                                                   
00910      MOVE LOW-VALUES             TO EC-NEW-CERT                   
00911                                     EC-NEW-STATE.                 
00912                                                                   
00913      EJECT                                                        
00914                                                                   
00915 ******************************************************************
00916 *                                                                *
00917 *              R E C E I V E   M A P                             *
00918 *                                                                *
00919 ******************************************************************
00920                                                                   
00921  0200-RECEIVE.                                                    
00922      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       
00923          MOVE ER-0008            TO EMI-ERROR                     
00924          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
00925          IF PI-MAP-NAME = VP631B                                  
00926              MOVE -1                 TO BMAINTL                   
00927              GO TO 8200-SEND-DATAONLY                             
00928          ELSE                                                     
00929              IF PI-MAP-NAME = VP631C                              
00930                  MOVE -1                 TO CMAINTL               
00931                  GO TO 8200-SEND-DATAONLY                         
00932              ELSE                                                 
00933                  IF PI-MAP-NAME = VP631D                          
00934                      MOVE -1                 TO DPFENTRL          
00935                      GO TO 8200-SEND-DATAONLY.                    
00936                                                                   
00937      EXEC CICS RECEIVE                                            
00938          MAP      (PI-MAP-NAME)                                   
00939          MAPSET   (MAPSET-VP6311S)                                
00940          INTO     (VP631BI)                                       
00941      END-EXEC.                                                    
00942                                                                   
00943 ******************************************************************
00944 *        TRANSFORM  UNDERLINE CHARACTERS TO SPACES               *
00945 ******************************************************************
00946                                                                   
00947      IF BTYPE-LEN (1) EQUAL +3                                    
00948         IF BTYPE (1) EQUAL ALL ' '                                
00949             MOVE 'Y' TO WS-REMOVE-LF-SW.                          
00950                                                                   
00951      IF BTYPE-LEN (2) EQUAL +3                                    
00952         IF BTYPE (2) EQUAL ALL ' '                                
00953             MOVE 'Y' TO WS-REMOVE-AH-SW.                          
00954                                                                   
00955      INSPECT VP631BI CONVERTING '_' TO ' '.                       
00956                                                                   
00957      IF PI-MAP-NAME = VP631B                                      
00958          IF BPFENTRL GREATER THAN ZEROS                           
00959              IF EIBAID NOT = DFHENTER                             
00960                  MOVE ER-0004       TO EMI-ERROR                  
00961                  MOVE -1            TO BPFENTRL                   
00962                  MOVE AL-UNBOF      TO BPFENTRA                   
00963                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         
00964                  GO TO 8200-SEND-DATAONLY                         
00965              ELSE                                                 
00966                  IF BPFENTRI NUMERIC AND                          
00967                    (BPFENTRI GREATER 0 AND LESS 25)               
00968                      MOVE PF-VALUES (BPFENTRI) TO EIBAID          
00969                  ELSE                                             
00970                      MOVE ER-0029  TO EMI-ERROR                   
00971                      MOVE AL-UNBON TO BPFENTRA                    
00972                      MOVE -1       TO BPFENTRL                    
00973                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     
00974                      GO TO 8200-SEND-DATAONLY                     
00975          ELSE                                                     
00976              NEXT SENTENCE                                        
00977      ELSE                                                         
00978          IF PI-MAP-NAME = VP631C                                  
00979             IF CPFENTRL GREATER THAN ZEROS                        
00980                 IF EIBAID NOT = DFHENTER                          
00981                  MOVE ER-0004         TO EMI-ERROR                
00982                  MOVE -1              TO CPFENTRL                 
00983                  MOVE AL-UNBOF        TO CPFENTRA                 
00984                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         
00985                  GO TO 8200-SEND-DATAONLY                         
00986              ELSE                                                 
00987                  IF CPFENTRI NUMERIC AND                          
00988                    (CPFENTRI GREATER 0 AND LESS 25)               
00989                      MOVE PF-VALUES (CPFENTRI) TO EIBAID          
00990                  ELSE                                             
00991                      MOVE ER-0029      TO EMI-ERROR               
00992                      MOVE AL-UNBON     TO CPFENTRA                
00993                      MOVE -1           TO CPFENTRL                
00994                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     
00995                      GO TO 8200-SEND-DATAONLY                     
00996          ELSE                                                     
00997              NEXT SENTENCE                                        
00998      ELSE                                                         
00999      IF PI-MAP-NAME = VP631D                                      
01000          IF DPFENTRL GREATER THAN ZEROS                           
01001              IF EIBAID NOT = DFHENTER                             
01002                  MOVE ER-0004         TO EMI-ERROR                
01003                  MOVE -1              TO DPFENTRL                 
01004                  MOVE AL-UNBOF        TO DPFENTRA                 
01005                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         
01006                  GO TO 8200-SEND-DATAONLY                         
01007              ELSE                                                 
01008                  IF DPFENTRI NUMERIC AND                          
01009                    (DPFENTRI GREATER 0 AND LESS 25)               
01010                      MOVE PF-VALUES (DPFENTRI) TO EIBAID          
01011                  ELSE                                             
01012                      MOVE ER-0029      TO EMI-ERROR               
01013                      MOVE AL-UNBON     TO DPFENTRA                
01014                      MOVE -1           TO DPFENTRL                
01015                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     
01016                      GO TO 8200-SEND-DATAONLY.                    
01017                                                                   
01018      IF EIBAID = DFHPF23                                          
01019          GO TO 8810-PF23.                                         
01020                                                                   
01021      IF EIBAID = DFHPF24                                          
01022          GO TO 9200-RETURN-MAIN-MENU.                             
01023                                                                   
01024      IF EIBAID = DFHPF12                                          
01025          GO TO 9500-PF12.                                         
01026                                                                   
01027      IF PI-MAP-NAME = VP631B                                      
01028         GO TO 0300-CHECK-ISSUE-PF-KEYS.                           
01029                                                                   
01030      IF PI-MAP-NAME = VP631C                                      
01031         GO TO 0400-CHECK-CANCEL-PF-KEYS.                          
01032                                                                   
01033      IF PI-MAP-NAME = VP631D                                      
01034         GO TO 0500-CHECK-BATCH-PF-KEYS.                           
01035                                                                   
01036      EJECT                                                        
01037                                                                   
01038 ******************************************************************
01039 *                                                                *
01040 *      ISSUE SCREEN PF KEY CHECKING                              *
01041 *                                                                *
01042 ******************************************************************
01043                                                                   
01044 ******************************************************************
01045 *                                                                *
01046 *      PF KEY FUNCTIONS:                                         *
01047 *                                                                *
01048 *      PF1  = BROWSE FORWARD                                     *
01049 *      PF2  = BROWSE BACKWARD                                    *
01050 *      PF3  = CERTIFICATE LOOKUP                                 *
01051 *      PF4  = DISPLAY BATCH HEADER                               *
01052 *      PF5  = ADD ISSUE  SCREEN                                  *
01053 *      PF6  = ADD CANCEL SCREEN                                  *
01054 *      PF7  = CERTIFICATE DISPLAY                                *
01055 *      PF8  = CERTIFICATE NOTES                                  *
01056 *      PF9  = EDIT CONTROL                                       *
01057 *      PF10 = DISPLAY LETTER SCREEN                              *
01058 *      PF11 = DISPLAY EXPANDED LIST OF ERRORS                    *
01059 *      PF12 = HELP SCREEN                                        *
01060 *      PF13 = CONTINUE ISSUE DISPLAY (SECOND SCREEN)             *
01061 *      PF14 = ACCOUNT MASTER                                     *
01062 *      PF15 = PROCESS CERT VERIFICATION OR GENERAL CHG ENDORSEMNT*
01063 *      PF16 = DISPLAY LETTER REVIEW                              *
01064 *                                                                *
01065 ******************************************************************
01066                                                                   
01067  0300-CHECK-ISSUE-PF-KEYS.                                        
01068      IF EIBAID = DFHPF9 AND PI-EDIT-SW NOT = '1'                  
01069         MOVE ER-2337            TO EMI-ERROR                      
01070         MOVE -1                  TO BMAINTL                       
01071         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
01072         GO TO 8200-SEND-DATAONLY.                                 
01073                                                                   
01074      IF EIBAID = DFHPF1                                           
01075         MOVE '0'                TO PI-EDIT-SW                     
01076         IF PI-PRIMARY-BROWSE                                      
01077            GO TO 4100-BROWSE-PRIMARY-FORWARD                      
01078         ELSE                                                      
01079            IF PI-PRIMARY-WITH-SELECT                              
01080               GO TO 4100-BROWSE-PRIMARY-FORWARD                   
01081            ELSE                                                   
01082               IF PI-FILE-BROWSE                                   
01083                  GO TO 4300-BROWSE-FILE-FORWARD                   
01084               ELSE                                                
01085                  IF PI-CSR-BROWSE                                 
01086                     GO TO 4200-BROWSE-CSR-FORWARD                 
01087                  ELSE                                             
01088                     GO TO 4000-BROWSE-DETAIL-FORWARD.             
01089                                                                   
01090      IF EIBAID = DFHPF2                                           
01091         MOVE '0'                TO PI-EDIT-SW                     
01092         IF PI-PRIMARY-BROWSE                                      
01093            GO TO 4600-BROWSE-PRIMARY-BACKWARD                     
01094         ELSE                                                      
01095            IF PI-PRIMARY-WITH-SELECT                              
01096               GO TO 4600-BROWSE-PRIMARY-BACKWARD                  
01097            ELSE                                                   
01098               IF PI-FILE-BROWSE                                   
01099                  GO TO 4800-BROWSE-FILE-BACKWARD                  
01100               ELSE                                                
01101                  IF PI-CSR-BROWSE                                 
01102                     GO TO 4900-BROWSE-CSR-BACKWARD                
01103                  ELSE                                             
01104                     GO TO 4500-BROWSE-DETAIL-BACKWARD.            
01105                                                                   
01106      IF EIBAID = DFHPF3                                           
01107         PERFORM 6000-CREATE-TEMP-STORAGE THRU 6000-EXIT           
01108         MOVE XCTL-EL127         TO PGM-NAME                       
01109         GO TO 9300-XCTL.                                          
01110                                                                   
           IF (EIBAID = DFHPF15 OR DFHPF6)
              AND (CSR-EDIT-SESSION)
062712*       AND (FATAL-OR-UNFORCED)
062712        AND (FATAL-ERRORS)
              MOVE ER-1819             TO EMI-ERROR
              MOVE -1                  TO BMAINTL
              MOVE AL-UANON            TO BMAINTA
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              GO TO 8200-SEND-DATAONLY
           END-IF
           
091312     IF (EIBAID = DFHPF15 AND CSR-EDIT-SESSION)
091312      OR (EIBAID = DFHPF10)
072312        MOVE PI-PREV-ALT-KEY     TO PB-CONTROL-BY-ACCOUNT
072312        PERFORM 6210-READ-ERACCT THRU 6210-EXIT
072312        IF (RESP-NORMAL)
072312           AND (PB-CONTROL-BY-ACCOUNT (1:20) =
072312              AM-CONTROL-PRIMARY (1:20))
072312            IF AM-NAME NOT GREATER THAN SPACES OR
072312               AM-ADDRS NOT GREATER THAN SPACES  OR
072312               AM-ADDR-CITY NOT GREATER THAN SPACES OR
072312               AM-ADDR-STATE NOT GREATER THAN SPACES OR
072312               AM-ZIP NOT GREATER THAN SPACES
072312               MOVE ER-3834             TO EMI-ERROR
072312               MOVE -1                  TO BMAINTL
072312               MOVE AL-UANON            TO BMAINTA
072312               PERFORM 9900-ERROR-FORMAT
072312                                 THRU 9900-EXIT
072312               GO TO 8200-SEND-DATAONLY
072312            END-IF
072312        ELSE
072312           MOVE ER-1609             TO EMI-ERROR
072312           MOVE -1                  TO BMAINTL
072312           MOVE AL-UANON            TO BMAINTA
072312           PERFORM 9900-ERROR-FORMAT
072312                                THRU 9900-EXIT
072312           GO TO 8200-SEND-DATAONLY
072312        END-IF
072312
072312        MOVE PI-PREV-ALT-KEY (1:33) TO ELCERT-KEY
072312        PERFORM 6220-READ-ERMAIL THRU 6220-EXIT
072312        IF (ERMAIL-FOUND)
072312            IF (MA-ADDRESS-LINE-1 NOT GREATER THAN SPACES AND
072312               MA-ADDRESS-LINE-2 NOT GREATER THAN SPACES)  OR
072312               MA-CITY NOT GREATER THAN SPACES OR
072312               MA-ADDR-STATE NOT GREATER THAN SPACES OR
072312               MA-ZIP-CODE NOT GREATER THAN SPACES
072312               MOVE ER-3835             TO EMI-ERROR
072312               MOVE -1                  TO BMAINTL
072312               MOVE AL-UANON            TO BMAINTA
072312               PERFORM 9900-ERROR-FORMAT
072312                                 THRU 9900-EXIT
072312               GO TO 8200-SEND-DATAONLY
072312            END-IF
072312        ELSE
072312           MOVE ER-3000             TO EMI-ERROR
072312           MOVE -1                  TO BMAINTL
072312           MOVE AL-UANON            TO BMAINTA
072312           PERFORM 9900-ERROR-FORMAT
072312                                THRU 9900-EXIT
072312           GO TO 8200-SEND-DATAONLY
072312        END-IF
072312
072312     END-IF

CIDMOD     IF (EIBAID = DFHPF15)
              AND (CSR-EDIT-SESSION)
CIDMOD        MOVE PI-PREV-ALT-KEY     TO ERPNDB-ALT-KEY                
CIDMOD        MOVE PI-SV-CARRIER       TO PI-CARRIER                    
CIDMOD        MOVE PI-SV-GROUPING      TO PI-GROUPING                   
CIDMOD        MOVE PI-SV-STATE         TO PI-STATE                      
CIDMOD        MOVE ERPNDB-ACCOUNT      TO PI-ACCOUNT                    
CIDMOD        MOVE BCERTNOI            TO PI-CERT-PRIME                 
CIDMOD        MOVE BSUFIXI             TO PI-CERT-SFX                   
CIDMOD        MOVE BEFFDTI             TO DEEDIT-FIELD                  
CIDMOD        PERFORM 8600-DEEDIT                                       
CIDMOD        MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY            
CIDMOD        MOVE '4'                 TO DC-OPTION-CODE                
CIDMOD        PERFORM 9700-DATE-LINK                                    
CIDMOD        IF DATE-CONVERSION-ERROR                                  
CIDMOD           MOVE ER-0348          TO EMI-ERROR                     
CIDMOD           MOVE -1               TO BEFFDTL                       
CIDMOD           MOVE AL-UABON         TO BEFFDTA                       
CIDMOD           PERFORM 9900-ERROR-FORMAT                              
CIDMOD                                 THRU 9900-EXIT                   
CIDMOD           GO TO 8200-SEND-DATAONLY                               
CIDMOD        ELSE                                                      
CIDMOD           MOVE DC-BIN-DATE-1    TO PI-CERT-EFF-DT                
CIDMOD           PERFORM 6000-CREATE-TEMP-STORAGE THRU 6000-EXIT        
CIDMOD           MOVE XCTL-EL6315      TO PGM-NAME                      
062712           MOVE '1'              TO PI-6315-ISS-CAN-SW
062712           MOVE LOW-VALUES       TO PI-6315-PASS-AREA
062712           PERFORM 6400-CHECK-FOR-RES-REF-CLAIM THRU 6400-EXIT
062712           MOVE WS-EL6315-PASS-AREA TO PI-PROGRAM-WORK-AREA
CIDMOD           GO TO 9300-XCTL                                        
CIDMOD        END-IF
CIDMOD     END-IF
01110                                                                   
01111      IF EIBAID = DFHPF4                                           
01112         MOVE '0'                TO PI-EDIT-SW                     
01113         IF PI-DISPLAY-ORIGINAL-BATCH                              
01114            NEXT SENTENCE                                          
01115         ELSE                                                      
01116            GO TO 7200-DISPLAY-BATCHES.                            
01117                                                                   
01118      IF EIBAID = DFHPF5                                           
090612        MOVE ER-0029                TO EMI-ERROR                     
090612        MOVE -1                     TO BPFENTRL                      
090612        MOVE AL-UNBON               TO BPFENTRA                      
090612        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     
090612        GO TO 8200-SEND-DATAONLY.                                    
090612*        MOVE '0'                TO PI-EDIT-SW                     
090612*        IF PI-DISPLAY-ORIGINAL-BATCH                              
090612*           NEXT SENTENCE                                          
090612*        ELSE                                                      
090612*           MOVE VP631B              TO PI-MAP-NAME                
090612*           MOVE LOW-VALUES          TO VP631BI                    
090612*           MOVE 'A'                 TO BMAINTO                    
090612*           MOVE AL-UANON            TO BMAINTA                    
090612*           MOVE -1                  TO BMAINTL                    
090612*           MOVE '5'                 TO PI-MAINT-FUNCTION          
090612*           GO TO 8100-SEND-INITIAL-MAP.                           
01130                                                                   
01131      IF EIBAID = DFHPF6                                           
              IF NOT CSR-EDIT-SESSION
090612           MOVE ER-0029                TO EMI-ERROR                     
090612           MOVE -1                     TO BPFENTRL                      
090612           MOVE AL-UNBON               TO BPFENTRA                      
090612           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     
090612           GO TO 8200-SEND-DATAONLY 
090612*           MOVE '0'              TO PI-EDIT-SW                     
090612*           IF PI-DISPLAY-ORIGINAL-BATCH                              
090612*              NEXT SENTENCE                                          
090612*           ELSE                                                      
090612*              MOVE VP631C        TO PI-MAP-NAME                
090612*              MOVE LOW-VALUES    TO VP631CI                    
090612*              MOVE 'A'           TO CMAINTO                    
090612*              MOVE AL-UANON      TO CMAINTA                    
090612*              MOVE -1            TO CMAINTL                    
090612*              MOVE '6'           TO PI-MAINT-FUNCTION
090612*              GO TO 8100-SEND-INITIAL-MAP
090612*           END-IF
              ELSE
                 PERFORM 5100-PROCESS-POL-RESC
                                       THRU 5100-EXIT
010412           MOVE CG-BATCH-NO      TO PI-PB-ENTRY-BATCH
010412           MOVE CG-BATCH-SEQ-NO  TO PI-PB-BATCH-SEQ-NO
010412                                    PI-PREV-SEQ-NO
010412           MOVE 'Y'              TO WS-FIRST-TIME-SW
010412*                                    WS-CANCEL-LETTER-SW
010412                                    PI-ALL-ISSUES-SW
010412                                    PI-ALL-CANCELS-SW
010412           GO TO 4100-BROWSE-PRIMARY-FORWARD
010412        END-IF
           END-IF

01144      IF EIBAID = DFHPF7                                           
01145         MOVE PI-PREV-ALT-KEY        TO ERPNDB-ALT-KEY             
01146         MOVE PI-SV-CARRIER          TO PI-CARRIER                 
01147         MOVE PI-SV-GROUPING         TO PI-GROUPING                
01148         MOVE PI-SV-STATE            TO PI-STATE                   
01149         MOVE ERPNDB-ACCOUNT         TO PI-ACCOUNT                 
01150         MOVE BCERTNOI               TO PI-CERT-PRIME              
01151         MOVE BSUFIXI                TO PI-CERT-SFX                
01152         MOVE BEFFDTI                TO DEEDIT-FIELD               
01153         PERFORM 8600-DEEDIT                                       
01154         MOVE DEEDIT-FIELD-V0        TO DC-GREG-DATE-1-MDY         
01155         MOVE '4'                    TO DC-OPTION-CODE             
01156         PERFORM 9700-DATE-LINK                                    
01157         IF DATE-CONVERSION-ERROR                                  
01158            MOVE ER-0348            TO EMI-ERROR                   
01159            MOVE -1                  TO BEFFDTL                    
01160            MOVE AL-UABON            TO BEFFDTA                    
01161            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
01162            GO TO 8200-SEND-DATAONLY                               
01163         ELSE                                                      
01164            MOVE DC-BIN-DATE-1       TO PI-CERT-EFF-DT             
01165            PERFORM 6000-CREATE-TEMP-STORAGE THRU 6000-EXIT        
01166            MOVE XCTL-EL1273        TO PGM-NAME                    
01167            GO TO 9300-XCTL.                                       
01168                                                                   
01169      IF EIBAID = DFHPF8                                           
01170         MOVE PI-PREV-ALT-KEY        TO ERPNDB-ALT-KEY             
01171         MOVE PI-SV-CARRIER          TO PI-CARRIER                 
01172         MOVE PI-SV-GROUPING         TO PI-GROUPING                
01173         MOVE PI-SV-STATE            TO PI-STATE                   
01174         MOVE ERPNDB-ACCOUNT         TO PI-ACCOUNT                 
01175         MOVE BCERTNOI               TO PI-CERT-PRIME              
01176         MOVE BSUFIXI                TO PI-CERT-SFX                
01177         MOVE BEFFDTI                TO DEEDIT-FIELD               
01178         PERFORM 8600-DEEDIT                                       
01179         MOVE DEEDIT-FIELD-V0        TO DC-GREG-DATE-1-MDY         
01180         MOVE '4'                    TO DC-OPTION-CODE             
01181         PERFORM 9700-DATE-LINK                                    
01182         IF DATE-CONVERSION-ERROR                                  
01183            MOVE ER-0348            TO EMI-ERROR                   
01184            MOVE -1                  TO BEFFDTL                    
01185            MOVE AL-UABON            TO BEFFDTA                    
01186            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
01187            GO TO 8200-SEND-DATAONLY                               
01188         ELSE                                                      
01189            MOVE DC-BIN-DATE-1       TO PI-CERT-EFF-DT             
01190            PERFORM 6000-CREATE-TEMP-STORAGE THRU 6000-EXIT        
01191            IF PI-COMPANY-ID = 'DMD'                               
01192                MOVE XCTL-EL401DMD      TO PGM-NAME                
01193              ELSE                                                 
111109               MOVE XCTL-EL1279        TO PGM-NAME                
01195            END-IF                                                 
01196            GO TO 9300-XCTL.                                       
01197                                                                   
01198      IF EIBAID = DFHPF9 AND PI-EDIT-SW = '1'                      
01199         MOVE XCTL-EL6312         TO PGM-NAME                      
01200         EXEC CICS XCTL                                            
01201             PROGRAM   (PGM-NAME)                                  
01202             COMMAREA  (PROGRAM-INTERFACE-BLOCK)                   
01203             LENGTH    (WS-COMM-LENGTH)                            
01204         END-EXEC                                                  
01205      ELSE                                                         
01206         MOVE '0'                 TO  PI-EDIT-SW.                  
01207                                                                   
01208      IF  EIBAID = DFHPF10                                         
01209          PERFORM 6000-CREATE-TEMP-STORAGE THRU 6000-EXIT          
01210          MOVE PI-PREV-ALT-KEY    TO PI-ERPNDB-ALT-KEY             
01211          MOVE LOW-VALUES         TO PI-689-WORK-AREA              
01212                                     PI-1042-WA                    
01213          MOVE 4                  TO PI-689-DATA-SOURCE            
01214          MOVE PI-SV-CARRIER      TO PI-689-CARRIER                
01215          MOVE PI-SV-GROUPING     TO PI-689-GROUPING               
01216          MOVE PI-SV-STATE        TO PI-689-STATE                  
01217          MOVE PI-PB-ACCOUNT      TO PI-689-ACCOUNT                
01218          MOVE PI-PB-CERT-EFF-DT  TO PI-689-EFF-DATE               
01219          MOVE PI-PB-CERT-NO      TO PI-689-CERT-NO                
01220          MOVE PI-PB-ALT-CHG-SEQ-NO                                
01221                                  TO PI-689-ALT-SEQ-NO             
01222          MOVE PI-PREV-BATCH      TO PI-689-ENTRY-BATCH            
01223          MOVE PI-PREV-SEQ-NO     TO PI-689-SEQ-NO                 
01224          MOVE PI-PREV-CHG-SEQ-NO TO PI-689-CHG-SEQ-NO             
01225          MOVE +0                 TO PI-689-CONTROL                
01226          MOVE LOW-VALUES         TO PI-689-DATE-EDIT              
01227                                     PI-689-EXP-DATE               
01228                                     PI-689-RESP-PERSON            
01229                                     PI-689-TYPE                   
01230                                     PI-689-FOLLOW-UP-EDIT         
01231                                     PI-689-RESEND1-EDIT           
01234                                     PI-689-SEQ-EDIT               
01235                                     PI-689-BCSEQ-EDIT             
01236                                     PI-689-LBL-OVERRIDE           
01237          MOVE W-LETTER-COMMUNICATION-AREAS                        
01238                                  TO PI-PROGRAM-WORK-AREA          
01239          MOVE XCTL-EL689         TO PGM-NAME                      
01240          GO TO 9300-XCTL.                                         
01241                                                                   
01242      IF  EIBAID = DFHPF16                                         
01243          PERFORM 6000-CREATE-TEMP-STORAGE THRU 6000-EXIT          
01244          MOVE PI-PREV-ALT-KEY    TO PI-ERPNDB-ALT-KEY             
01245          MOVE LOW-VALUES         TO PI-689-WORK-AREA              
01246                                     PI-1042-WA                    
01247          MOVE 4                  TO PI-689-DATA-SOURCE            
01248          MOVE PI-SV-CARRIER      TO PI-689-CARRIER                
01249          MOVE PI-SV-GROUPING     TO PI-689-GROUPING               
01250          MOVE PI-SV-STATE        TO PI-689-STATE                  
01251          MOVE PI-PB-ACCOUNT      TO PI-689-ACCOUNT                
01252          MOVE PI-PB-CERT-EFF-DT  TO PI-689-EFF-DATE               
01253          MOVE PI-PB-CERT-NO      TO PI-689-CERT-NO                
01254          MOVE PI-PB-ALT-CHG-SEQ-NO                                
01255                                  TO PI-689-ALT-SEQ-NO             
01256          MOVE PI-PB-RECORD-TYPE  TO PI-689-CHG-SEQ-NO             
01257          MOVE PI-PREV-BATCH      TO PI-689-ENTRY-BATCH            
01258          MOVE PI-PREV-SEQ-NO     TO PI-689-SEQ-NO                 
01259          MOVE PI-PREV-CHG-SEQ-NO TO PI-689-CHG-SEQ-NO             
01260          MOVE W-LETTER-COMMUNICATION-AREAS                        
01261                                  TO PI-PROGRAM-WORK-AREA          
01262          MOVE XCTL-EL690         TO PGM-NAME                      
01263          GO TO 9300-XCTL.                                         
01264                                                                   
01265      IF EIBAID = DFHPF13                                          
01266          MOVE XCTL-EL6313        TO PGM-NAME                      
01267          GO TO 9300-XCTL.                                         
01268                                                                   
030708     IF EIBAID = DFHPF17
030708        PERFORM 6200-CREATE-VADS THRU 6200-EXIT
032715        IF (PB-I-LOAN-APR numeric)
050713           and (pb-i-loan-apr < 99.9999)
030708           AND (PB-I-BENEFICIARY-NAME NOT
030708              = SPACES AND LOW-VALUES)
100111
100111           MOVE LINK-ELVADS          TO PGM-NAME
100111           EXEC CICS LINK
100111               PROGRAM    (PGM-NAME)
100111               COMMAREA   (WS-PASS-AREA)
100111               LENGTH     (WS-PASS-AREA-LENGTH)
100111           END-EXEC
100111
030708           MOVE 0000             TO EMI-ERROR                     
030708           MOVE -1               TO BMAINTL
030708           PERFORM 9900-ERROR-FORMAT
030708                                 THRU 9900-EXIT                  
030708           GO TO 8200-SEND-DATAONLY
030708        ELSE
030708           MOVE ER-0877            TO EMI-ERROR                   
030708           MOVE -1                  TO BEFFDTL                    
030708           MOVE AL-UABON            TO BEFFDTA                    
030708           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
030708           GO TO 8200-SEND-DATAONLY
030708        END-IF
030708     END-IF

01269      IF EIBAID = DFHPF14                                          
01270         MOVE PI-PREV-ALT-KEY        TO ERPNDB-ALT-KEY             
01271         MOVE PI-SV-CARRIER          TO PI-CR-CARRIER              
01272         MOVE PI-SV-GROUPING         TO PI-CR-GROUPING             
01273         MOVE PI-SV-STATE            TO PI-CR-STATE                
01274         MOVE ERPNDB-ACCOUNT         TO PI-CR-ACCOUNT              
01275         MOVE SPACE                  TO PI-EDIT-SW                 
01276         PERFORM 6000-CREATE-TEMP-STORAGE THRU 6000-EXIT           
01277         MOVE XCTL-EL650             TO PGM-NAME                   
01278         GO TO 9300-XCTL.                                          
01279                                                                   
01280      IF EIBAID = DFHPF15                                          
01281         IF PI-COMPANY-ID  =  'HER'                                
01282             PERFORM 6000-CREATE-TEMP-STORAGE THRU 6000-EXIT       
01283             MOVE XCTL-EL680         TO PGM-NAME                   
01284             GO TO 9300-XCTL.                                      
01285                                                                   
01286      IF EIBAID = DFHENTER OR DFHPF11                              
01287         GO TO 1000-ISSUE-MAINLINE.                                
01288                                                                   
01289      MOVE ER-0029                TO EMI-ERROR                     
01290      MOVE -1                     TO BPFENTRL                      
01291      MOVE AL-UNBON               TO BPFENTRA                      
01292      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     
01293      GO TO 8200-SEND-DATAONLY.                                    
01294                                                                   
01295      EJECT                                                        
01296                                                                   
01297 ******************************************************************
01298 *                                                                *
01299 *      CANCEL SCREEN PF KEY CHECKING                             *
01300 *                                                                *
01301 ******************************************************************
01302                                                                   
01303 ******************************************************************
01304 *                                                                *
01305 *      PF KEY FUNCTIONS:                                         *
01306 *                                                                *
01307 *      PF1  = BROWSE FORWARD                                     *
01308 *      PF2  = BROWSE BACKWARD                                    *
01309 *      PF3  = CERTIFICATE LOOKUP                                 *
01310 *      PF4  = DISPLAY BATCH HEADER                               *
01311 *      PF5  = ADD ISSUE SCREEN                                   *
01312 *      PF6  = ADD CANCEL SCREEN                                  *
01313 *      PF7  = CERTIFICATE DISPLAY                                *
01314 *      PF8  = CERTIFICATE NOTES                                  *
01315 *      PF9  = EDIT CONTROL                                       *
01316 *      PF10 = DISPLAY LETTER SCREEN                              *
01317 *      PF11 = DISPLAY EXPANDED LIST OF ERRORS                    *
01318 *      PF12 = DISPLAY HELP SCREEN                                *
01319 *      PF13 = CHECK WRITER FOR CANCEL SETTLEMENTS                *
01320 *      PF14 = ACCOUNT MASTER                                     *
01321 *      PF16 = LETTER REVIEW                                      *
01322 *                                                                *
01323 ******************************************************************
01324                                                                   
01325  0400-CHECK-CANCEL-PF-KEYS.                                       
01326      IF EIBAID = DFHPF9 AND PI-EDIT-SW NOT = '1'                  
01327         MOVE ER-2337             TO EMI-ERROR                     
01328         MOVE -1                  TO CMAINTL                       
01329         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
01330         GO TO 8200-SEND-DATAONLY.                                 
01331                                                                   
01332      IF EIBAID = DFHPF1                                           
01333         MOVE '0'                TO PI-EDIT-SW                     
01334         IF PI-PRIMARY-BROWSE                                      
01335            GO TO 4100-BROWSE-PRIMARY-FORWARD                      
01336         ELSE                                                      
01337            IF PI-PRIMARY-WITH-SELECT                              
01338               GO TO 4100-BROWSE-PRIMARY-FORWARD                   
01339            ELSE                                                   
01340               IF PI-FILE-BROWSE                                   
01341                  GO TO 4300-BROWSE-FILE-FORWARD                   
01342               ELSE                                                
01343                  IF PI-CSR-BROWSE                                 
01344                     GO TO 4200-BROWSE-CSR-FORWARD                 
01345                  ELSE                                             
01346                     GO TO 4000-BROWSE-DETAIL-FORWARD.             
01347                                                                   
01348      IF EIBAID = DFHPF2                                           
01349         MOVE '0'                TO PI-EDIT-SW                     
01350         IF PI-PRIMARY-BROWSE                                      
01351            GO TO 4600-BROWSE-PRIMARY-BACKWARD                     
01352         ELSE                                                      
01353            IF PI-PRIMARY-WITH-SELECT                              
01354               GO TO 4600-BROWSE-PRIMARY-BACKWARD                  
01355            ELSE                                                   
01356               IF PI-FILE-BROWSE                                   
01357                  GO TO 4800-BROWSE-FILE-BACKWARD                  
01358               ELSE                                                
01359                  IF PI-CSR-BROWSE                                 
01360                     GO TO 4900-BROWSE-CSR-BACKWARD                
01361                  ELSE                                             
01362                     GO TO 4500-BROWSE-DETAIL-BACKWARD.            
01363                                                                   
01364      IF EIBAID = DFHPF3                                           
01365         PERFORM 6000-CREATE-TEMP-STORAGE THRU 6000-EXIT           
01366         MOVE XCTL-EL127         TO PGM-NAME                       
01367         GO TO 9300-XCTL.                                          
01368                                                                   
           IF (EIBAID = DFHPF15 OR DFHPF17)
              AND (CSR-EDIT-SESSION)
062712*       AND (FATAL-OR-UNFORCED)
062712        AND (FATAL-ERRORS)
              MOVE ER-1819             TO EMI-ERROR
              MOVE -1                  TO CMAINTL
              MOVE AL-UANON            TO CMAINTA
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              GO TO 8200-SEND-DATAONLY
           END-IF

072312     IF (EIBAID = DFHPF15 OR DFHPF17 OR DFHPF10)
072312       AND (CSR-EDIT-SESSION)
072312        MOVE PI-PREV-ALT-KEY     TO PB-CONTROL-BY-ACCOUNT
072312        PERFORM 6210-READ-ERACCT THRU 6210-EXIT
072312        IF (RESP-NORMAL)
072312           AND (PB-CONTROL-BY-ACCOUNT (1:20) =
072312              AM-CONTROL-PRIMARY (1:20))
072312            IF AM-NAME NOT GREATER THAN SPACES OR
072312               AM-ADDRS NOT GREATER THAN SPACES  OR
072312               AM-ADDR-CITY NOT GREATER THAN SPACES OR
072312               AM-ADDR-STATE NOT GREATER THAN SPACES OR
072312               AM-ZIP NOT GREATER THAN SPACES
072312               MOVE ER-3834             TO EMI-ERROR
072312               MOVE -1                  TO BMAINTL
072312               MOVE AL-UANON            TO BMAINTA
072312               PERFORM 9900-ERROR-FORMAT
072312                                 THRU 9900-EXIT
072312               GO TO 8200-SEND-DATAONLY
072312            END-IF
072312        ELSE
072312           MOVE ER-1609             TO EMI-ERROR
072312           MOVE -1                  TO BMAINTL
072312           MOVE AL-UANON            TO BMAINTA
072312           PERFORM 9900-ERROR-FORMAT
072312                                THRU 9900-EXIT
072312           GO TO 8200-SEND-DATAONLY
072312        END-IF
072312
072312        MOVE PI-PREV-ALT-KEY (1:33) TO ELCERT-KEY
072312        PERFORM 6220-READ-ERMAIL THRU 6220-EXIT
072312        IF (ERMAIL-FOUND)
072312            IF (MA-ADDRESS-LINE-1 NOT GREATER THAN SPACES AND
072312               MA-ADDRESS-LINE-2 NOT GREATER THAN SPACES)  OR
072312               MA-CITY NOT GREATER THAN SPACES OR
072312               MA-ADDR-STATE NOT GREATER THAN SPACES OR
072312               MA-ZIP-CODE NOT GREATER THAN SPACES
072312               MOVE ER-3835             TO EMI-ERROR
072312               MOVE -1                  TO BMAINTL
072312               MOVE AL-UANON            TO BMAINTA
072312               PERFORM 9900-ERROR-FORMAT
072312                                 THRU 9900-EXIT
072312               GO TO 8200-SEND-DATAONLY
072312            END-IF
072312        ELSE
072312           MOVE ER-3000             TO EMI-ERROR
072312           MOVE -1                  TO BMAINTL
072312           MOVE AL-UANON            TO BMAINTA
072312           PERFORM 9900-ERROR-FORMAT
072312                                THRU 9900-EXIT
072312           GO TO 8200-SEND-DATAONLY
072312        END-IF
072312
072312     END-IF

CIDMOD     IF (EIBAID = DFHPF15 OR DFHPF17)
              AND (CSR-EDIT-SESSION)
CIDMOD        MOVE PI-PREV-ALT-KEY     TO ERPNDB-ALT-KEY                
CIDMOD        MOVE PI-SV-CARRIER       TO PI-CARRIER                    
CIDMOD        MOVE PI-SV-GROUPING      TO PI-GROUPING                   
CIDMOD        MOVE PI-SV-STATE         TO PI-STATE                      
CIDMOD        MOVE ERPNDB-ACCOUNT      TO PI-ACCOUNT                    
CIDMOD        MOVE CCERTNOI            TO PI-CERT-PRIME                 
CIDMOD        MOVE CSUFIXI             TO PI-CERT-SFX                   
CIDMOD        MOVE CEFFDTI             TO DEEDIT-FIELD                  
CIDMOD        PERFORM 8600-DEEDIT                                       
CIDMOD        MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY            
CIDMOD        MOVE '4'                 TO DC-OPTION-CODE                
CIDMOD        PERFORM 9700-DATE-LINK                                    
CIDMOD        IF DATE-CONVERSION-ERROR                                  
CIDMOD           MOVE ER-0348          TO EMI-ERROR                     
CIDMOD           MOVE -1               TO CEFFDTL                       
CIDMOD           MOVE AL-UABON         TO CEFFDTA                       
CIDMOD           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
CIDMOD           GO TO 8200-SEND-DATAONLY                               
CIDMOD        ELSE                                                      
CIDMOD           MOVE DC-BIN-DATE-1    TO PI-CERT-EFF-DT                
CIDMOD           PERFORM 6000-CREATE-TEMP-STORAGE                       
CIDMOD                                 THRU 6000-EXIT                   
062712           IF EIBAID = DFHPF17
061712               PERFORM 6400-CHECK-FOR-RES-REF-CLAIM 
062712                                  THRU 6400-EXIT
062712               IF PI-RES-REF-CLM-TYPE NOT EQUAL 
062712                                 '2' AND '3' AND '4'
062712                  MOVE ER-3831     TO  EMI-ERROR
062712                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
062712                  GO TO 8200-SEND-DATAONLY
010412               END-IF
062712               MOVE 'Y'          TO PI-CLM-RESC-IND
062712           ELSE
062712               MOVE 'N'          TO PI-CLM-RESC-IND
062712           END-IF
CIDMOD           MOVE XCTL-EL6316      TO PGM-NAME                      
062712           MOVE '2'              TO PI-ISS-CAN-SW
062712           MOVE LOW-VALUES       TO PI-6316-PASS-AREA
062712           MOVE WS-EL6316-PASS-AREA TO PI-PROGRAM-WORK-AREA
CIDMOD           GO TO 9300-XCTL                                        
CIDMOD        END-IF
CIDMOD     END-IF
01368                                                                   
01369      IF EIBAID = DFHPF4                                           
01370         MOVE '0'                TO PI-EDIT-SW                     
01371         IF PI-DISPLAY-ORIGINAL-BATCH                              
01372            NEXT SENTENCE                                          
01373         ELSE                                                      
01374            GO TO 7200-DISPLAY-BATCHES.                            
01375                                                                   
01376      IF EIBAID = DFHPF5                                           
090612        MOVE ER-0029                TO EMI-ERROR                     
090612        MOVE -1                     TO BPFENTRL                      
090612        MOVE AL-UNBON               TO BPFENTRA                      
090612        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     
090612        GO TO 8200-SEND-DATAONLY.                                    
090612*        MOVE '0'                TO PI-EDIT-SW                     
090612*        IF PI-DISPLAY-ORIGINAL-BATCH                              
090612*           NEXT SENTENCE                                          
090612*        ELSE                                                      
090612*           MOVE VP631B              TO PI-MAP-NAME                
090612*           MOVE LOW-VALUES          TO VP631BI                    
090612*           MOVE 'A'                 TO BMAINTO                    
090612*           MOVE AL-UANON            TO BMAINTA                    
090612*           MOVE -1                  TO BMAINTL                    
090612*           MOVE '5'                 TO PI-MAINT-FUNCTION          
090612*           GO TO 8100-SEND-INITIAL-MAP.                           
01388                                                                   
01389      IF EIBAID = DFHPF6                                           
090612        MOVE ER-0029                TO EMI-ERROR                     
090612        MOVE -1                     TO BPFENTRL                      
090612        MOVE AL-UNBON               TO BPFENTRA                      
090612        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     
090612        GO TO 8200-SEND-DATAONLY.                                    
090612*        MOVE '0'                TO PI-EDIT-SW                     
090612*        IF PI-DISPLAY-ORIGINAL-BATCH                              
090612*           NEXT SENTENCE                                          
090612*        ELSE                                                      
090612*           MOVE VP631C              TO PI-MAP-NAME                
090612*           MOVE LOW-VALUES          TO VP631CI                    
090612*           MOVE 'A'                 TO CMAINTO                    
090612*           MOVE AL-UANON            TO CMAINTA                    
090612*           MOVE -1                  TO CMAINTL                    
090612*           MOVE '6'                 TO PI-MAINT-FUNCTION          
090612*           GO TO 8100-SEND-INITIAL-MAP.                           
01401                                                                   
01402      IF EIBAID = DFHPF7                                           
01403         MOVE PI-PREV-ALT-KEY        TO ERPNDB-ALT-KEY             
01404         MOVE PI-SV-CARRIER          TO PI-CARRIER                 
01405         MOVE PI-SV-GROUPING         TO PI-GROUPING                
01406         MOVE PI-SV-STATE            TO PI-STATE                   
01407         MOVE ERPNDB-ACCOUNT         TO PI-ACCOUNT                 
01408         MOVE CCERTNOI               TO PI-CERT-PRIME              
01409         MOVE CSUFIXI                TO PI-CERT-SFX                
01410         MOVE CEFFDTI                TO DEEDIT-FIELD               
01411         PERFORM 8600-DEEDIT                                       
01412         MOVE DEEDIT-FIELD-V0        TO DC-GREG-DATE-1-MDY         
01413         MOVE '4'                    TO DC-OPTION-CODE             
01414         PERFORM 9700-DATE-LINK                                    
01415         IF DATE-CONVERSION-ERROR                                  
01416            MOVE ER-0348            TO EMI-ERROR                   
01417            MOVE -1                  TO BEFFDTL                    
01418            MOVE AL-UABON            TO BEFFDTA                    
01419            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
01420            GO TO 8200-SEND-DATAONLY                               
01421         ELSE                                                      
01422            MOVE DC-BIN-DATE-1       TO PI-CERT-EFF-DT             
01423            PERFORM 6000-CREATE-TEMP-STORAGE THRU 6000-EXIT        
01424            MOVE XCTL-EL1273        TO PGM-NAME                    
01425            GO TO 9300-XCTL.                                       
01426                                                                   
01427      IF EIBAID = DFHPF8                                           
01428         MOVE PI-PREV-ALT-KEY        TO ERPNDB-ALT-KEY             
01429         MOVE PI-SV-CARRIER          TO PI-CARRIER                 
01430         MOVE PI-SV-GROUPING         TO PI-GROUPING                
01431         MOVE PI-SV-STATE            TO PI-STATE                   
01432         MOVE ERPNDB-ACCOUNT         TO PI-ACCOUNT                 
01433         MOVE CCERTNOI               TO PI-CERT-PRIME              
01434         MOVE CSUFIXI                TO PI-CERT-SFX                
01435         MOVE CEFFDTI                TO DEEDIT-FIELD               
01436         PERFORM 8600-DEEDIT                                       
01437         MOVE DEEDIT-FIELD-V0        TO DC-GREG-DATE-1-MDY         
01438         MOVE '4'                    TO DC-OPTION-CODE             
01439         PERFORM 9700-DATE-LINK                                    
01440         IF DATE-CONVERSION-ERROR                                  
01441            MOVE ER-0348             TO EMI-ERROR                  
01442            MOVE -1                  TO BEFFDTL                    
01443            MOVE AL-UABON            TO BEFFDTA                    
01444            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
01445            GO TO 8200-SEND-DATAONLY                               
01446         ELSE                                                      
01447            MOVE DC-BIN-DATE-1       TO PI-CERT-EFF-DT             
01448            PERFORM 6000-CREATE-TEMP-STORAGE THRU 6000-EXIT        
01449            IF PI-COMPANY-ID = 'DMD'                               
01450                MOVE XCTL-EL401DMD      TO PGM-NAME                
01451              ELSE                                                 
111109               MOVE XCTL-EL1279        TO PGM-NAME                
01453            END-IF                                                 
01454            GO TO 9300-XCTL.                                       
01455                                                                   
01456      IF EIBAID = DFHPF9 AND PI-EDIT-SW = '1'                      
01457         MOVE XCTL-EL6312     TO PGM-NAME                          
01458         EXEC CICS XCTL                                            
01459             PROGRAM  (PGM-NAME)                                   
01460             COMMAREA (PROGRAM-INTERFACE-BLOCK)                    
01461             LENGTH   (WS-COMM-LENGTH)                             
01462         END-EXEC                                                  
01463      ELSE                                                         
01464         MOVE '0'                 TO  PI-EDIT-SW.                  
01465                                                                   
01466      IF EIBAID = DFHPF10                                          
01467          PERFORM 6000-CREATE-TEMP-STORAGE THRU 6000-EXIT          
01468          MOVE PI-PREV-KEY        TO PI-ERPNDB-KEY                 
01469          MOVE PI-PREV-ALT-KEY    TO PI-ERPNDB-ALT-KEY             
01470          MOVE LOW-VALUES         TO PI-689-WORK-AREA              
01471                                     PI-1042-WA                    
01472          MOVE 4                  TO PI-689-DATA-SOURCE            
01473          MOVE PI-SV-CARRIER      TO PI-689-CARRIER                
01474          MOVE PI-SV-GROUPING     TO PI-689-GROUPING               
01475          MOVE PI-SV-STATE        TO PI-689-STATE                  
01476          MOVE PI-PB-ACCOUNT      TO PI-689-ACCOUNT                
01477          MOVE PI-PB-CERT-EFF-DT  TO PI-689-EFF-DATE               
01478          MOVE PI-PB-CERT-NO      TO PI-689-CERT-NO                
01479          MOVE PI-PB-ALT-CHG-SEQ-NO                                
01480                                  TO PI-689-ALT-SEQ-NO             
01481          MOVE PI-PREV-BATCH      TO PI-689-ENTRY-BATCH            
01482          MOVE PI-PREV-SEQ-NO     TO PI-689-SEQ-NO                 
01483          MOVE PI-PREV-CHG-SEQ-NO TO PI-689-CHG-SEQ-NO             
01484          MOVE +0                 TO PI-689-CONTROL                
01485          MOVE LOW-VALUES         TO PI-689-DATE-EDIT              
01486                                     PI-689-EXP-DATE               
01487                                     PI-689-RESP-PERSON            
01488                                     PI-689-TYPE                   
01489                                     PI-689-FOLLOW-UP-EDIT         
01490                                     PI-689-RESEND1-EDIT           
01493                                     PI-689-SEQ-EDIT               
01494                                     PI-689-BCSEQ-EDIT             
01495                                     PI-689-LBL-OVERRIDE           
01496          MOVE W-LETTER-COMMUNICATION-AREAS                        
01497                                  TO PI-PROGRAM-WORK-AREA          
01498          MOVE XCTL-EL689         TO PGM-NAME                      
01499          GO TO 9300-XCTL.                                         
01500                                                                   
01501      IF  EIBAID = DFHPF16                                         
01502          PERFORM 6000-CREATE-TEMP-STORAGE THRU 6000-EXIT          
01503          MOVE PI-PREV-ALT-KEY    TO PI-ERPNDB-ALT-KEY             
01504          MOVE LOW-VALUES         TO PI-689-WORK-AREA              
01505                                     PI-1042-WA                    
01506          MOVE 4                  TO PI-689-DATA-SOURCE            
01507          MOVE PI-SV-CARRIER      TO PI-689-CARRIER                
01508          MOVE PI-SV-GROUPING     TO PI-689-GROUPING               
01509          MOVE PI-SV-STATE        TO PI-689-STATE                  
01510          MOVE PI-PB-ACCOUNT      TO PI-689-ACCOUNT                
01511          MOVE PI-PB-CERT-EFF-DT  TO PI-689-EFF-DATE               
01512          MOVE PI-PB-CERT-NO      TO PI-689-CERT-NO                
01513          MOVE PI-PB-ALT-CHG-SEQ-NO                                
01514                                  TO PI-689-ALT-SEQ-NO             
01515          MOVE PI-PB-RECORD-TYPE  TO PI-689-CHG-SEQ-NO             
01516          MOVE PI-PREV-BATCH      TO PI-689-ENTRY-BATCH            
01517          MOVE PI-PREV-SEQ-NO     TO PI-689-SEQ-NO                 
01518          MOVE PI-PREV-CHG-SEQ-NO TO PI-689-CHG-SEQ-NO             
01519          MOVE W-LETTER-COMMUNICATION-AREAS                        
01520                                  TO PI-PROGRAM-WORK-AREA          
01521          MOVE XCTL-EL690         TO PGM-NAME                      
01522          GO TO 9300-XCTL.                                         

01524      IF (EIBAID = DFHPF13)
111513        if (not fatal-errors)
111513           and (not unforced-errors)
01525            MOVE PI-PREV-ALT-KEY  TO ERPNDB-ALT-KEY             
01526            MOVE PI-SV-CARRIER    TO PI-CARRIER                 
01527            MOVE PI-SV-GROUPING   TO PI-GROUPING                
01528            MOVE PI-SV-STATE      TO PI-STATE                   
01529            MOVE ERPNDB-ACCOUNT   TO PI-ACCOUNT                 
01530            MOVE CCERTNOI         TO PI-CERT-PRIME              
01531            MOVE CSUFIXI          TO PI-CERT-SFX                
01532            MOVE CEFFDTI          TO DEEDIT-FIELD               
01533            PERFORM 8600-DEEDIT
01534            MOVE DEEDIT-FIELD-V0  TO DC-GREG-DATE-1-MDY         
01535            MOVE '4'              TO DC-OPTION-CODE             
01536            PERFORM 9700-DATE-LINK
01537            IF DATE-CONVERSION-ERROR                                  
01538               MOVE ER-0348       TO EMI-ERROR                   
01539               MOVE -1            TO CEFFDTL                    
01540               MOVE AL-UABON      TO CEFFDTA                    
01541               PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT               
01542               GO TO 8200-SEND-DATAONLY                               
01543            ELSE                                                      
01544               MOVE DC-GREG-DATE-1-EDIT TO CEFFDTI                    
01545               MOVE AL-UANON      TO CEFFDTA                    
01546               MOVE DC-BIN-DATE-1 TO PI-CERT-EFF-DT             
01547               PERFORM 6000-CREATE-TEMP-STORAGE
                                       THRU 6000-EXIT        
01548               MOVE XCTL-EL677    TO PGM-NAME                    
01549               GO TO 9300-XCTL
111513           end-if
111513        else
111513           MOVE ER-3444          TO EMI-ERROR
111513           MOVE -1               TO CPFENTRL
111513           MOVE AL-UNBON         TO CPFENTRA
111513           PERFORM 9900-ERROR-FORMAT
111513                                 THRU 9900-EXIT
111513           GO TO 8200-SEND-DATAONLY
111513        end-if
111513     end-if
01550                                                                   
01551      IF EIBAID = DFHPF14                                          
01552         MOVE PI-PREV-ALT-KEY        TO ERPNDB-ALT-KEY             
01553         MOVE PI-SV-CARRIER          TO PI-CR-CARRIER              
01554         MOVE PI-SV-GROUPING         TO PI-CR-GROUPING             
01555         MOVE PI-SV-STATE            TO PI-CR-STATE                
01556         MOVE ERPNDB-ACCOUNT         TO PI-CR-ACCOUNT              
01557         MOVE SPACE                  TO PI-EDIT-SW                 
01558         PERFORM 6000-CREATE-TEMP-STORAGE THRU 6000-EXIT           
01559         MOVE XCTL-EL650             TO PGM-NAME                   
01560         GO TO 9300-XCTL.                                          
01561                                                                   
01562      IF EIBAID = DFHENTER OR DFHPF11                              
01563         GO TO 2000-CANCEL-MAINLINE.                               
01564                                                                   
01565      MOVE ER-0029                TO EMI-ERROR.                    
01566      MOVE -1                     TO CPFENTRL.                     
01567      MOVE AL-UNBON               TO CPFENTRA.                     
01568      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01569      GO TO 8200-SEND-DATAONLY.                                    
01570                                                                   
01571      EJECT                                                        
01572                                                                   
01573 ******************************************************************
01574 *                                                                *
01575 *      BATCH SCREEN PF KEY CHECKING                              *
01576 *                                                                *
01577 ******************************************************************
01578                                                                   
01579 ******************************************************************
01580 *                                                                *
01581 *      PF KEY FUNCTIONS:                                         *
01582 *                                                                *
01583 *      PF1 = BROWSE FORWARD                                      *
01584 *      PF2 = BROWSE BACKWARD                                     *
01585 *      PF3 = DISPLAY PREMIUM TOTALS                              *
01586 *      PF4 = TRANSFER TO ACCOUNT BILLING                         *
01587 *                                                                *
01588 *                                                                *
01589 ******************************************************************
01590                                                                   
01591  0500-CHECK-BATCH-PF-KEYS.                                        
01592      IF EIBAID = DFHPF1                                           
01593         MOVE '0'                TO PI-EDIT-SW                     
01594         IF ONLY-BATCH-HEADERS  OR ALL-OUT-OF-BAL                  
01595            GO TO 5000-BROWSE-BATCH-HEADERS                        
01596         ELSE                                                      
01597            IF PI-GOOD-BROWSE                                      
01598               IF PI-PRIMARY-BROWSE OR PI-PRIMARY-WITH-SELECT      
01599                  GO TO 4100-BROWSE-PRIMARY-FORWARD                
01600               ELSE                                                
01601                  IF PI-FILE-BROWSE                                
01602                     GO TO 4300-BROWSE-FILE-FORWARD                
01603                  ELSE                                             
01604                     IF PI-CSR-BROWSE                              
01605                        GO TO 4200-BROWSE-CSR-FORWARD              
01606                     ELSE                                          
01607                        GO TO 4000-BROWSE-DETAIL-FORWARD           
01608            ELSE                                                   
01609               GO TO 0800-BROWSE-ERROR.                            
01610                                                                   
01611      IF EIBAID = DFHPF2                                           
01612         MOVE '0'                TO PI-EDIT-SW                     
01613         IF ONLY-BATCH-HEADERS  OR ALL-OUT-OF-BAL                  
01614            GO TO 5500-BROWSE-BATCH-BACKWARD                       
01615         ELSE                                                      
01616            IF PI-GOOD-BROWSE                                      
01617               IF PI-PRIMARY-BROWSE OR PI-PRIMARY-WITH-SELECT      
01618                  GO TO 4600-BROWSE-PRIMARY-BACKWARD               
01619               ELSE                                                
01620                  IF PI-FILE-BROWSE                                
01621                     GO TO 4800-BROWSE-FILE-BACKWARD               
01622                  ELSE                                             
01623                     IF PI-CSR-BROWSE                              
01624                        GO TO 4900-BROWSE-CSR-BACKWARD             
01625                     ELSE                                          
01626                        GO TO 4500-BROWSE-DETAIL-BACKWARD          
01627            ELSE                                                   
01628               GO TO 0800-BROWSE-ERROR.                            
01629                                                                   
01630      IF EIBAID = DFHPF3                                           
01631         GO TO 3000-BATCH-MAINLINE.                                
01632                                                                   
01633      IF  EIBAID = DFHPF4                                          
01634         MOVE PI-PREV-ALT-KEY        TO ERPNDB-ALT-KEY             
01635         MOVE PI-SV-CARRIER          TO PI-CR-CARRIER              
01636         MOVE PI-SV-GROUPING         TO PI-CR-GROUPING             
01637         MOVE PI-SV-STATE            TO PI-CR-STATE                
01638         MOVE ERPNDB-ACCOUNT         TO PI-CR-ACCOUNT              
01639                                        PI-CR-FIN-RESP             
01640         MOVE 'A'                    TO PI-CR-TYPE                 
01641         MOVE SPACE                  TO PI-EDIT-SW                 
01642         PERFORM 6000-CREATE-TEMP-STORAGE THRU 6000-EXIT           
01643         MOVE XCTL-EL640             TO PGM-NAME                   
01644         GO TO 9300-XCTL.                                          
01645                                                                   
01646      IF (ONLY-BATCH-HEADERS OR ALL-OUT-OF-BAL)                    
01647        AND                                                        
01648         (EIBAID = DFHENTER)                                       
01649            GO TO 3500-EDIT-BATCH-DATA.                            
01650                                                                   
01651      IF EIBAID = DFHENTER                                         
01652         GO TO 3500-EDIT-BATCH-DATA.                               
01653                                                                   
01654      MOVE ER-0029             TO EMI-ERROR.                       
01655      MOVE -1                  TO DPFENTRL.                        
01656      MOVE AL-UNBON            TO DPFENTRA.                        
01657      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01658      GO TO 8200-SEND-DATAONLY.                                    
01659                                                                   
01660  0800-BROWSE-ERROR.                                               
01661      MOVE ER-2258                TO EMI-ERROR.                    
01662      MOVE -1                     TO DPFENTRL.                     
01663      MOVE AL-UNBON               TO DPFENTRA.                     
01664      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01665      GO TO 8200-SEND-DATAONLY.                                    
01666                                                                   
01667      EJECT                                                        
01668                                                                   
01669 ******************************************************************
01670 *                                                                *
01671 *                I S S U E   M A I N L I N E                     *
01672 *                                                                *
01673 ******************************************************************
01674                                                                   
01675  1000-ISSUE-MAINLINE.                                             
01676      IF PI-DISPLAY-ORIGINAL-BATCH                                 
01677         IF BMAINTI NOT = 'C' AND 'D' AND 'S' AND 'K'              
01678            MOVE ER-0023             TO EMI-ERROR                  
01679            MOVE -1                  TO BMAINTL                    
01680            MOVE AL-UABON            TO BMAINTA                    
01681            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
01682            GO TO 8200-SEND-DATAONLY                               
01683         ELSE                                                      
01684            NEXT SENTENCE                                          
01685      ELSE                                                         
01686         IF BMAINTI NOT = 'A' AND 'C' AND 'D' AND 'S' AND 'K'      
01687            MOVE ER-0023             TO EMI-ERROR                  
01688            MOVE -1                  TO BMAINTL                    
01689            MOVE AL-UABON            TO BMAINTA                    
01690            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
01691            GO TO 8200-SEND-DATAONLY.                              
01692                                                                   
01693      IF BMAINTI = 'S'                                             
01694         GO TO 1100-SHOW-REQUEST.                                  
01695                                                                   
01696      IF NOT MODIFY-CAP                                            
01697          MOVE 'UPDATE'       TO SM-READ                           
01698          PERFORM 9995-SECURITY-VIOLATION                          
01699          MOVE ER-0070        TO EMI-ERROR                         
01700          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
01701          GO TO 8100-SEND-INITIAL-MAP.                             
01702                                                                   
01703      IF BMAINTL GREATER THAN ZEROS                                
01704         IF PI-PF5-FUNCTION                                        
01705            IF BMAINTI NOT = 'A'                                   
01706               MOVE AL-UABON  TO BMAINTA                           
01707               MOVE -1        TO BMAINTL                           
01708               MOVE ER-2260   TO EMI-ERROR                         
01709               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            
01710               GO TO 8200-SEND-DATAONLY.                           
01711                                                                   
01712      IF BCERTNOL = ZEROS                                          
01713        AND (BMAINTI = 'C' OR 'K' OR 'D')                          
01714          NEXT SENTENCE                                            
01715      ELSE                                                         
01716          IF BCERTNOL NOT = ZEROS                                  
01717              MOVE AL-UANON       TO BCERTNOA                      
01718          ELSE                                                     
01719              MOVE -1             TO BCERTNOL                      
01720              MOVE  1             TO SET-CURSOR-SW                 
01721              MOVE ER-2218        TO EMI-ERROR                     
01722              MOVE AL-UABON       TO BCERTNOA                      
01723              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
01724                                                                   
01725      IF BSUFIXL NOT = ZEROS                                       
01726          MOVE AL-UANON       TO BSUFIXA                           
01727      ELSE                                                         
01728          MOVE AL-SANOF       TO BSUFIXA.                          
01729                                                                   
01730      IF BEFFDTL = ZEROS AND (BMAINTI = 'C' OR 'K' OR 'D')         
01731         NEXT SENTENCE                                             
01732      ELSE                                                         
01733         IF BEFFDTL NOT = ZEROS                                    
01734            MOVE AL-UNNON             TO BEFFDTA                   
01735            MOVE BEFFDTI              TO DEEDIT-FIELD              
01736            PERFORM 8600-DEEDIT                                    
01737            MOVE DEEDIT-FIELD-V0      TO DC-GREG-DATE-1-MDY        
01738            MOVE 4                    TO DC-OPTION-CODE            
01739            PERFORM 9700-DATE-LINK                                 
01740            IF NO-CONVERSION-ERROR                                 
01741               MOVE DC-BIN-DATE-1     TO WS-CONVERTED-EFFDT        
01742               ELSE                                                
01743               MOVE -1                TO BEFFDTL                   
01744               MOVE  1                TO SET-CURSOR-SW             
01745               MOVE ER-2226           TO EMI-ERROR                 
01746               MOVE AL-UNBON          TO BEFFDTA                   
01747               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            
01748            ELSE                                                   
01749               MOVE -1                TO BEFFDTL                   
01750               MOVE  1                TO SET-CURSOR-SW             
01751               MOVE ER-2220           TO EMI-ERROR                 
01752               MOVE AL-UNBON          TO BEFFDTA                   
01753               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.           
01754                                                                   
01755      MOVE PI-PREV-ALT-KEY            TO WS-ERPNDB-ALT-KEY.        
01756                                                                   
01757      IF BCERTNOL NOT = ZEROS                                      
01758         MOVE BCERTNOI                TO WS-NEW-PRIME              
01759      ELSE                                                         
01760         MOVE SPACES                  TO WS-NEW-PRIME.             
01761                                                                   
01762      IF BSUFIXL NOT = ZEROS                                       
01763         MOVE BSUFIXI                 TO WS-NEW-SUFFIX             
01764      ELSE                                                         
01765         MOVE SPACE                   TO WS-NEW-SUFFIX.            
01766                                                                   
01767      IF BEFFDTL NOT = ZEROS                                       
01768         MOVE WS-CONVERTED-EFFDT      TO WS-NEW-CERT-EFF-DT        
01769      ELSE                                                         
01770         MOVE LOW-VALUES              TO WS-NEW-CERT-EFF-DT.       
01771                                                                   
01772      IF BMAINTI = 'A' OR 'K' OR 'S'                               
01773         NEXT SENTENCE                                             
01774      ELSE                                                         
01775         MOVE PI-PREV-ALT-KEY     TO WS-ERPNDB-ALT-KEY             
01776         IF WS-PB-CERT-NO     NOT = WS-NEW-CERT-NO      OR         
01777            WS-PB-CERT-EFF-DT NOT = WS-NEW-CERT-EFF-DT  OR         
01778            WS-PB-RECORD-TYPE NOT = '1'                            
01779            IF BMAINTI = 'C'                                       
01780                MOVE ER-2287          TO EMI-ERROR                 
01781                MOVE -1               TO BCERTNOL                  
01782                MOVE AL-UABON         TO BCERTNOA BEFFDTA          
01783                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT           
01784                GO TO 8200-SEND-DATAONLY                           
01785              ELSE                                                 
01786                MOVE ER-2260          TO EMI-ERROR                 
01787                MOVE -1               TO BCERTNOL                  
01788                MOVE AL-UABON         TO BCERTNOA                  
01789                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT           
01790                GO TO 8200-SEND-DATAONLY.                          
01791                                                                   
01792      IF BMAINTI = 'D'                                             
01793          GO TO 1400-DELETE-ISSUE-ROUTINE.                         
01794                                                                   
01795 *    IF BMICRNOL  NOT =  ZEROS                                    
01796 *        IF BMICRNOI  NUMERIC                                     
01797 *            MOVE AL-UNNON       TO  BMICRNOA                     
01798 *        ELSE                                                     
01799 *            MOVE -1             TO  BMICRNOL                     
01800 *            MOVE 1              TO  SET-CURSOR-SW                
01801 *            MOVE AL-UNBON       TO  BMICRNOA                     
01802 *            MOVE ER-2701        TO  EMI-ERROR                    
01803 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            

101706     IF BVINL NOT = ZEROS
101706        MOVE AL-UANON            TO BVINA
101706     END-IF

01805      IF B1STNML NOT = ZEROS                                       
01806          MOVE AL-UANON                TO B1STNMA.                 
01807                                                                   
01808      IF BINITL  NOT = ZEROS                                       
01809         MOVE AL-UANON                TO BINITA.                   
01810                                                                   
01811      IF BSEXL NOT = ZEROS                                         
01812         IF BSEXI = 'M' OR 'F'                                     
01813            MOVE AL-UANON             TO BSEXA                     
01814         ELSE                                                      
01815            MOVE -1               TO BSEXL                         
01816            MOVE ER-2629          TO EMI-ERROR                     
01817            MOVE AL-UABON         TO BSEXA                         
01818            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              
01819                                                                   
01820      IF BAGEL NOT = ZEROS                                         
01821          IF BAGEI NUMERIC                                         
01822              MOVE BAGEI              TO WS-BAGE                   
01823              MOVE AL-UNNON           TO BAGEA                     
01824          ELSE                                                     
01825              MOVE -1                 TO BAGEL                     
01826              MOVE  1                 TO SET-CURSOR-SW             
01827              MOVE ER-2223            TO EMI-ERROR                 
01828              MOVE AL-UNBON           TO BAGEA                     
01829              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
100213
100213     IF BLSTNML NOT = ZERO  OR
100213        B1STNML NOT = ZERO  OR
100213        BINITL NOT = ZERO
100213         IF BAGEL = ZEROS
100213             MOVE -1                 TO BAGEL
100213             MOVE  1                 TO SET-CURSOR-SW
100213             MOVE ER-3269            TO EMI-ERROR
100213             MOVE AL-UNBON           TO BAGEA
100213             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
100213         END-IF
100213     END-IF.
01830                                                                   
01831      MOVE LOW-VALUES             TO WS-CONVERTED-BIRTH            
01832                                                                   
01833      IF (BBIRTHL GREATER THAN +0) AND                             
01834         (BBIRTHI NOT EQUAL SPACES)                                
01835         NEXT SENTENCE                                             
01836      ELSE                                                         
01837         GO TO 1010-CHECK-AGE.                                     
01838                                                                   
01839      MOVE BBIRTHI                TO DEEDIT-FIELD.                 
01840      PERFORM 8600-DEEDIT.                                         
01841      MOVE DEEDIT-FIELD-V0        TO DC-GREG-DATE-1-MDY.           
01842      MOVE DC-GREG-DATE-1-MDY     TO WS-SAVE-BIRTH-DATE            
01843      MOVE AL-UNNON               TO BBIRTHA.                      
01844      MOVE '4'                    TO DC-OPTION-CODE.               
01845      PERFORM 9700-DATE-LINK.                                      
CIDMOD     IF DATE-CONVERSION-ERROR                                     
01847         MOVE -1                  TO BBIRTHL                       
01848         MOVE  1                  TO SET-CURSOR-SW                 
CIDMOD        MOVE ER-2228             TO EMI-ERROR                     
01850         MOVE AL-UNBON            TO BBIRTHA                       
01851         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
01852         GO TO 1010-CHECK-AGE.                                     
01853                                                                   
CIDMOD     IF (NO-CONVERSION-ERROR) AND                                 
CIDMOD        (DC-BIN-DATE-1 GREATER THAN WS-CONVERTED-EFFDT)           
01855         MOVE DC-BIN-DATE-1       TO WS-WORK-BIN-DT                
01856         SUBTRACT CENTURY-ADJ FROM WS-WORK-BIN-RED                 
01857         MOVE WS-WORK-BIN-DT      TO DC-BIN-DATE-1                 
01858                                     WS-CONVERTED-BIRTH            
01859         MOVE AL-UANON            TO BBIRTHA                       
01860      ELSE                                                         
01861         MOVE DC-BIN-DATE-1       TO WS-CONVERTED-BIRTH            
01862         MOVE AL-UANON            TO BBIRTHA.                      
01863                                                                   
01864  1010-CHECK-AGE.                                                  
01865                                                                   
01866 *    IF BAGEL = ZERO                                              
01867 *       IF BBIRTHL GREATER THAN ZERO                              
01868 *          PERFORM 1095-CALC-AGE THRU 1099-EXIT.                  
01869                                                                   
01889                                                                   
01898                                                                   
01907                                                                   
01908 **   IF PI-COMPANY-ID = 'DMD'                                     
032612     IF PI-COMPANY-ID = 'DMD' OR 'CID' or 'AHL'                   
01909         NEXT SENTENCE                                             
01910      ELSE                                                         
01911          IF BLNOFCRL NOT = ZEROS                                  
01912             PERFORM 7500-READ-ERLOFC THRU 7599-EXIT               
01913             IF LOAN-OFFICER-FOUND                                 
01914                 MOVE AL-UANON        TO BLNOFCRA                  
01915              ELSE                                                 
01916                  MOVE SPACE          TO WS-LOAN-OFFICER           
01917                  MOVE -1             TO BLNOFCRL                  
01918                  MOVE  1             TO SET-CURSOR-SW             
01919                  MOVE ER-3006        TO EMI-ERROR                 
01920                  MOVE AL-UNBON       TO BLNOFCRA                  
01921                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.        
02015                                                                   
02016  1010-EDIT-CONT.                                                  
02017                                                                   
02018      IF BFORCEL  NOT = ZEROS                                      
02019          IF FORCE-CAP                                             
02020             MOVE AL-UANON            TO BFORCEA                   
02021          ELSE                                                     
02022             MOVE ER-2945             TO EMI-ERROR                 
02023             MOVE AL-UABON            TO BFORCEA                   
02024             MOVE -1                  TO BFORCEL                   
02025             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.             
02026                                                                   
02027      IF BENTRYL NOT = ZEROS                                       
02028         IF BENTRYI NOT = ' ' AND 'E' AND 'R' AND 'P' AND          
052814                 'C' and 'D' AND 'V' AND 'U' AND 'M'            
02030            MOVE ER-2224              TO EMI-ERROR                 
02031            MOVE AL-UABON             TO BENTRYA                   
02032            MOVE -1                   TO BENTRYL                   
02033            MOVE  1                   TO SET-CURSOR-SW             
02034            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
02035         ELSE                                                      
02036            MOVE AL-UANON             TO BENTRYA.                  
02037                                                                   
02038      IF BUNWRITL NOT = ZEROS                                      
02039         IF BUNWRITI NOT = ' ' AND 'A' AND 'D' AND 'U' AND 'N'     
02040            MOVE ER-7574              TO EMI-ERROR                 
02041            MOVE AL-UABON             TO BUNWRITA                  
02042            MOVE -1                   TO BUNWRITL                  
02043            MOVE  1                   TO SET-CURSOR-SW             
02044            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
02045         ELSE                                                      
02046            MOVE AL-UANON             TO BUNWRITA.                 
02047                                                                   
02048      IF BREINL  NOT = ZEROS                                       
02049          MOVE AL-UANON               TO BREINA.                   
02050                                                                   
02051      IF BBILCDL NOT = ZEROS                                       
02052         IF BBILCDI NOT = ' ' AND 'H' AND 'R' AND 'L'              
02053                              AND 'A' AND 'B' AND 'E'              
02054            MOVE ER-2225              TO EMI-ERROR                 
02055            MOVE AL-UABON             TO BBILCDA                   
02056            MOVE -1                   TO BBILCDL                   
02057            MOVE  1                   TO SET-CURSOR-SW             
02058            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
02059         ELSE                                                      
02060            MOVE AL-UANON             TO BBILCDA.                  
02061                                                                   
02062      IF BSKPCDL NOT = ZEROS                                       
02063          MOVE AL-UANON               TO BSKPCDA.                  
02064                                                                   
02065      IF BINDGRPL NOT = ZEROS                                      
02066          MOVE AL-UANON               TO BINDGRPA.                 
02067                                                                   
02068      IF BSIGL   NOT = ZEROS                                       
02069          MOVE AL-UANON               TO BSIGA.                    
02070                                                                   
02071      IF BRTCLSL NOT = ZEROS                                       
02072          MOVE AL-UANON               TO BRTCLSA.                  
02073                                                                   
02074      IF BMEMBERL NOT = ZEROS                                      
02075          MOVE AL-UANON               TO BMEMBERA.                 
02076                                                                   
02077      MOVE +0                         TO WS-SUB1.                  
02078                                                                   
02079      EJECT                                                        
02080                                                                   
02081 ******************************************************************
02082 *                                                                *
02083 *           E D I T   I S S U E   C O V E R A G E S              *
02084 *                                                                *
02085 ******************************************************************
02086                                                                   
02087  1020-EDIT-COVERAGES.                                             
02088      ADD +1                      TO WS-SUB1.                      
02089                                                                   
02090      IF WS-SUB1 GREATER +2                                        
091615        GO TO 1025-EDIT-CONTINUE
           END-IF
02092                                                                   
02100                                                                   
02101      IF BTYPE-LEN      (WS-SUB1) GREATER THAN ZEROS OR            
02102         BTERM-LEN      (WS-SUB1) GREATER THAN ZEROS OR            
02103         BBEN-LEN       (WS-SUB1) GREATER THAN ZEROS OR            
02104         BPREM-LEN      (WS-SUB1) GREATER THAN ZEROS OR            
02105         BCRIT-PERD-LEN (WS-SUB1) GREATER THAN ZEROS OR            
02106         BEXPIRE-LEN    (WS-SUB1) GREATER THAN ZEROS OR            
02107         BALT-PREM-LEN  (WS-SUB1) GREATER THAN ZEROS OR            
02108         BALT-BEN-LEN   (WS-SUB1) GREATER THAN ZEROS               
02109         NEXT SENTENCE                                             
02110          ELSE                                                     
02111             GO TO 1020-EDIT-COVERAGES.                            
02112                                                                   
02113  1021-CONT-EDIT.                                                  
02114                                                                   
02115      IF BMAINTI = 'A'                                             
02116         IF BTYPE-LEN  (WS-SUB1) GREATER THAN ZEROS                
02117            MOVE AL-UANON         TO BTYPE-ATTRB       (WS-SUB1)   
02118         ELSE                                                      
02119            MOVE  -1              TO BTYPE-LEN         (WS-SUB1)   
02120            MOVE ER-7404          TO EMI-ERROR                     
02121            MOVE AL-SABON         TO BKIND-ATTRB       (WS-SUB1)   
02122            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
02123      ELSE                                                         
02124         IF BTYPE-LEN  (WS-SUB1) GREATER THAN ZEROS                
02125            MOVE AL-UANON         TO BTYPE-ATTRB       (WS-SUB1).  
02126                                                                   
02127      IF BTERM-LEN (WS-SUB1) NOT EQUAL +0                          
02128         MOVE BTERMI (WS-SUB1)    TO DEEDIT-FIELD                  
02129         PERFORM 8600-DEEDIT                                       
02130         IF DEEDIT-FIELD-V0 NUMERIC                                
02131            MOVE DEEDIT-FIELD-V0  TO WS-BTERM (WS-SUB1)            
02132         ELSE                                                      
02133            MOVE ZEROS            TO WS-BTERM (WS-SUB1).           
02134                                                                   
02146                                                                   
02147      IF BMAINTI = ('C' OR 'K')                                    
02148         IF BTERM-LEN (WS-SUB1)  = ZEROS                           
02149            NEXT SENTENCE                                          
02150         ELSE                                                      
02151            MOVE BTERMI    (WS-SUB1)   TO DEEDIT-FIELD             
02152            PERFORM 8600-DEEDIT                                    
02153            IF DEEDIT-FIELD-V0 NUMERIC                             
02154               MOVE DEEDIT-FIELD-V0    TO WS-BTERM    (WS-SUB1)    
02155               IF WS-BTERM (WS-SUB1)  GREATER THAN ZERO            
02156                     MOVE AL-UNNON     TO BTERM-ATTRB (WS-SUB1)    
02157               ELSE                                                
02158                     MOVE ER-2241      TO EMI-ERROR                
02159                     MOVE -1           TO BTERM-LEN   (WS-SUB1)    
02160                     MOVE AL-UNBON     TO BTERM-ATTRB (WS-SUB1)    
02161                     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT      
02162            ELSE                                                   
02163               MOVE ER-2223          TO EMI-ERROR                  
02164               MOVE -1               TO BTERM-LEN     (WS-SUB1)    
02165               MOVE AL-UNBON         TO BTERM-ATTRB   (WS-SUB1)    
02166               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            
02167      ELSE                                                         
02168         IF  BTERM-LEN      (WS-SUB1)   GREATER THAN ZEROS         
02169             MOVE BTERMI    (WS-SUB1)   TO DEEDIT-FIELD            
02170             PERFORM 8600-DEEDIT                                   
02171             IF DEEDIT-FIELD-V0      NUMERIC                       
02172                IF DEEDIT-FIELD-V0     GREATER THAN ZERO           
02173                   MOVE DEEDIT-FIELD-V0 TO WS-BTERM    (WS-SUB1)   
02174                   MOVE AL-UNNON        TO BTERM-ATTRB (WS-SUB1)   
02175                ELSE                                               
02176                   MOVE ER-2241         TO EMI-ERROR               
02177                   MOVE -1              TO BTERM-LEN   (WS-SUB1)   
02178                   MOVE AL-UNBON        TO BTERM-ATTRB (WS-SUB1)   
02179                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT        
02180             ELSE                                                  
02181               MOVE ER-2223             TO EMI-ERROR               
02182               MOVE -1                  TO BTERM-LEN   (WS-SUB1)   
02183               MOVE AL-UNBON            TO BTERM-ATTRB (WS-SUB1)   
02184               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            
02185         ELSE                                                      
02186             MOVE ER-2240               TO EMI-ERROR               
02187             MOVE -1                    TO BTERM-LEN   (WS-SUB1)   
02188             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.             
02193                                                                   
02194      IF BBEN-LEN (WS-SUB1) = ZEROS                                
02195         AND BMAINTI = ('C' OR 'K')                                
02196            NEXT SENTENCE                                          
02197      ELSE                                                         
02198         IF BBEN-LEN (WS-SUB1) GREATER THAN ZEROS                  
02199            MOVE AL-UNNON           TO BBEN-ATTRB (WS-SUB1)        
02200            EXEC CICS BIF DEEDIT
02201                FIELD  (BBENI (WS-SUB1))
02202                LENGTH (12)
02203            END-EXEC
02204            IF BBENI (WS-SUB1)  NUMERIC
02205               IF BBENI (WS-SUB1) GREATER THAN ZEROS
02206                  MOVE BBENI (WS-SUB1) TO WS-BBEN    (WS-SUB1)
CIDMOD*          MOVE BBENI (WS-SUB1)    TO DEEDIT-FIELD                
CIDMOD*          PERFORM 8600-DEEDIT                                    
CIDMOD*          IF DEEDIT-FIELD-V2  NUMERIC                            
CIDMOD*             IF DEEDIT-FIELD-V2 GREATER THAN ZEROS               
CIDMOD*                MOVE DEEDIT-FIELD-V2 TO WS-BBEN    (WS-SUB1)     
02207               ELSE                                                
02208                  MOVE ER-7632    TO EMI-ERROR                     
02209                  MOVE -1         TO BBEN-LEN   (WS-SUB1)          
02210                  MOVE AL-UNBON   TO BBEN-ATTRB (WS-SUB1)          
02211                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         
02212            ELSE                                                   
02213               MOVE ER-2223         TO EMI-ERROR                   
02214               MOVE -1              TO BBEN-LEN   (WS-SUB1)        
02215               MOVE AL-UNBON        TO BBEN-ATTRB (WS-SUB1)        
02216               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            
02217         ELSE                                                      
02218            MOVE ER-7632    TO EMI-ERROR                           
02219            MOVE -1         TO BBEN-LEN   (WS-SUB1)                
02220            MOVE AL-UNBON   TO BBEN-ATTRB (WS-SUB1)                
02221            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              
02222                                                                   
02223      IF (BPREM-LEN    (WS-SUB1) = ZEROS)
02224         AND (BMAINTI = ('C' OR 'K'))
02225         CONTINUE
02226      ELSE                                                         
02227         IF BPREM-LEN   (WS-SUB1) GREATER THAN ZEROS               
02228            MOVE AL-UNNON           TO BPREM-ATTRB (WS-SUB1)       
101110           INSPECT BPREM-ALPHA (WS-SUB1) TALLYING WS-TALLY
101110              FOR ALL '.'
101110           IF WS-TALLY > 1
101110              PERFORM VARYING A1 FROM 1 BY 1 UNTIL
101110                 BPREM-ALPHA (WS-SUB1) (A1:1) = '.'
101110              END-PERFORM
101110              IF A1 < 9
101110                 ADD 3 TO A1
101110                 MOVE SPACES TO BPREM-ALPHA (WS-SUB1) (A1:12 - A1)
101110              END-IF
101110           ELSE
101110              PERFORM VARYING A1 FROM 1 BY 1 UNTIL
101110                (A1 > 11)
101110                OR (BPREM-ALPHA (WS-SUB1) (A1:1) =
101110                  LOW-VALUES OR SPACES)
101110              END-PERFORM
101110              IF A1 > 1 AND < 12
101110                 MOVE SPACES
101110                           TO BPREM-ALPHA (WS-SUB1) (A1:12 - A1)
101110              END-IF
101110           END-IF

02229            EXEC CICS BIF DEEDIT
02230                FIELD  (BPREMI (WS-SUB1))
02231                LENGTH (11)
02232            END-EXEC
02233            IF BPREMI (WS-SUB1) NUMERIC
02234               IF BPREMI (WS-SUB1) GREATER THAN ZEROS
02235                  MOVE BPREMI (WS-SUB1) TO WS-BPREM   (WS-SUB1)
CIDMOD*          MOVE BPREMI (WS-SUB1)   TO DEEDIT-FIELD                
CIDMOD*          PERFORM 8600-DEEDIT                                    
CIDMOD*          IF DEEDIT-FIELD-V2  NUMERIC                            
CIDMOD*             IF DEEDIT-FIELD-V2 GREATER THAN ZEROS               
CIDMOD*                MOVE DEEDIT-FIELD-V2 TO WS-BPREM   (WS-SUB1)     
02236               ELSE                                                
                       IF PI-COMPANY-ID NOT = 'DCC' and 'VPP'
02237                     MOVE ER-7633    TO EMI-ERROR                     
02238                     MOVE -1         TO BPREM-LEN   (WS-SUB1)
02239                     MOVE AL-UNBON   TO BPREM-ATTRB (WS-SUB1)
02240                     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
100703                 END-IF
100703              END-IF
02241            ELSE
02242               MOVE ER-2223         TO EMI-ERROR                   
02243               MOVE -1              TO BPREM-LEN   (WS-SUB1)       
02244               MOVE AL-UNBON        TO BPREM-ATTRB (WS-SUB1)       
02245               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
100703           END-IF
02246         ELSE                                                      
100703           IF PI-COMPANY-ID NOT = 'DCC' and 'VPP'
02247               MOVE ER-7633         TO EMI-ERROR
02248               MOVE -1              TO BPREM-LEN   (WS-SUB1)
02249               MOVE AL-UNBON        TO BPREM-ATTRB (WS-SUB1)
02250               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
100703           END-IF
100703        END-IF
100703     END-IF
02251                                                                   
02252      IF BCPREM-LEN   (WS-SUB1) = ZEROS                            
02253         NEXT SENTENCE                                             
02254      ELSE                                                         
02255         MOVE AL-UNNON           TO BCPREM-ATTRB (WS-SUB1)         
02256         EXEC CICS BIF DEEDIT
02257             FIELD  (BCPREMI (WS-SUB1))
02258             LENGTH (11)
02259         END-EXEC
02260         IF BCPREMI (WS-SUB1) NUMERIC
02261            MOVE BCPREMI (WS-SUB1) TO WS-BCPREM  (WS-SUB1)
CIDMOD*       MOVE BCPREMI (WS-SUB1)  TO DEEDIT-FIELD-X11               
CIDMOD*       PERFORM 8600-DEEDIT                                       
CIDMOD*       IF DEEDIT-FIELD-V2  NUMERIC                               
CIDMOD*          MOVE DEEDIT-FIELD-V2 TO WS-BCPREM  (WS-SUB1)           
02262         ELSE                                                      
02263            MOVE ER-2223         TO EMI-ERROR                      
02264            MOVE -1              TO BCPREM-LEN   (WS-SUB1)         
02265            MOVE AL-UNBON        TO BCPREM-ATTRB (WS-SUB1)         
02266            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              
02267                                                                   
02268      IF BCRIT-PERD-LEN  (WS-SUB1)    GREATER THAN ZEROS           
02269         MOVE BCRIT-PERDI (WS-SUB1)   TO DEEDIT-FIELD              
02270         PERFORM 8600-DEEDIT                                       
02271         IF DEEDIT-FIELD-V0 NUMERIC                                
02272            MOVE DEEDIT-FIELD-V0      TO WS-BCRIT-PERD    (WS-SUB1)
02273            MOVE AL-UNNON             TO BCRIT-PERD-ATTRB (WS-SUB1)
02274          ELSE                                                     
02275            MOVE -1                   TO BCRIT-PERD-LEN   (WS-SUB1)
02276            MOVE AL-UNBON             TO BCRIT-PERD-ATTRB (WS-SUB1)
02277            MOVE ER-2223              TO EMI-ERROR                 
02278            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              
02279                                                                   
02280      IF BEXPIRE-LEN  (WS-SUB1)    GREATER THAN ZEROS              
02281         IF BEXPIRE   (WS-SUB1) = SPACES                           
02282            MOVE LOW-VALUES       TO WS-CONVERTED-EXPIRDT (WS-SUB1)
02283         ELSE                                                      
02284            MOVE BEXPIRE (WS-SUB1)   TO DEEDIT-FIELD               
02285            PERFORM 8600-DEEDIT                                    
02286            IF DEEDIT-FIELD-V0        NUMERIC                      
02287               MOVE AL-UNNON         TO BEXPIRE-ATTRB (WS-SUB1)    
02288               MOVE 4                TO DC-OPTION-CODE             
02289               MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY      
02290               PERFORM 9700-DATE-LINK                              
02291               MOVE DC-BIN-DATE-1    TO WS-CONVERTED-EXPIRDT       
02292                                                         (WS-SUB1) 
02293               IF NO-CONVERSION-ERROR                              
02294                  NEXT SENTENCE                                    
02295               ELSE                                                
02296                  MOVE -1         TO BEXPIRE-LEN   (WS-SUB1)       
02297                  MOVE ER-2531    TO EMI-ERROR                     
02298                  MOVE AL-UNBON   TO BEXPIRE-ATTRB (WS-SUB1)       
02299                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         
02300            ELSE                                                   
02301              MOVE -1             TO BEXPIRE-LEN   (WS-SUB1)       
02302              MOVE ER-2532        TO EMI-ERROR                     
02303              MOVE AL-UNBON       TO BEXPIRE-ATTRB (WS-SUB1)       
02304              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
02305                                                                   
02306      IF BALT-BEN-LEN   (WS-SUB1)  = ZEROS                         
02307         NEXT SENTENCE                                             
02308      ELSE                                                         
02309         EXEC CICS BIF DEEDIT
02310             FIELD  (BALT-BENI (WS-SUB1))
02311             LENGTH (12)
02312         END-EXEC
02313         IF BALT-BENI (WS-SUB1) NUMERIC
02314            MOVE BALT-BENI (WS-SUB1) TO WS-BALT-BEN    (WS-SUB1)
CIDMOD*       MOVE BALT-BENI (WS-SUB1) TO DEEDIT-FIELD                  
CIDMOD*       PERFORM 8600-DEEDIT                                       
CIDMOD*       IF DEEDIT-FIELD-V0 NUMERIC                                
CIDMOD*          MOVE DEEDIT-FIELD-V2 TO WS-BALT-BEN        (WS-SUB1)   
02315         ELSE                                                      
02316            MOVE ER-2223         TO EMI-ERROR                      
02317            MOVE -1              TO BALT-BEN-LEN       (WS-SUB1)   
02318            MOVE AL-UNBON        TO BALT-BEN-ATTRB     (WS-SUB1)   
02319            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              
02320                                                                   
02321      IF BALT-PREM-LEN (WS-SUB1)  = ZEROS                          
02322         NEXT SENTENCE                                             
02323      ELSE                                                         
02324         EXEC CICS BIF DEEDIT
02325             FIELD  (BALT-PREMI (WS-SUB1))
02326             LENGTH (11)
02327         END-EXEC
02328         IF BALT-PREMI (WS-SUB1) NUMERIC
02329            MOVE BALT-PREMI (WS-SUB1) TO WS-BALT-PREM  (WS-SUB1)
CIDMOD*       MOVE BALT-PREMI    (WS-SUB1)   TO DEEDIT-FIELD            
CIDMOD*       PERFORM 8600-DEEDIT                                       
CIDMOD*       IF DEEDIT-FIELD-V0 NUMERIC                                
CIDMOD*          MOVE DEEDIT-FIELD-V2 TO WS-BALT-PREM       (WS-SUB1)   
02330         ELSE                                                      
02331            MOVE ER-2223         TO EMI-ERROR                      
02332            MOVE -1              TO BALT-PREM-LEN      (WS-SUB1)   
02333            MOVE AL-UNBON        TO BALT-PREM-ATTRB    (WS-SUB1)   
02334            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              
02335                                                                   
02336       GO TO 1020-EDIT-COVERAGES.                                  

           .
091615 1025-edit-continue.

02339      IF PI-COMPANY-ID  =  'DMD'                                   
02340         MOVE LOW-VALUES             TO BNCERTI                    
02341                                        BNSTATI.                   
02342                                                                   
02343      IF EMI-ERROR = ZEROS                                         
02344          NEXT SENTENCE                                            
02345      ELSE                                                         
02346          GO TO 8200-SEND-DATAONLY.                                
02347                                                                   
02348      IF BMAINTI = 'A'                                             
02349          GO TO 1200-ADD-ISSUE-ROUTINE.                            
02350                                                                   
02351      IF BMAINTI = 'C'                                             
02352          GO TO 1300-CHANGE-ISSUE-ROUTINE.                         
02353                                                                   
02354      GO TO 2800-REWRITE-ISSUE-CERT-KEY.                           
02355                                                                   
02356      EJECT                                                        
02357                                                                   
02358  1030-EDIT-FOR-NUMERICS.                                          
02359      IF WS-CSO-SUB EQUAL 10                                       
02360         GO TO 1030-EDIT-EXIT.                                     
02361                                                                   
02362      IF (WS-EDIT-BYTE (WS-CSO-SUB) NUMERIC OR                     
02363         WS-EDIT-BYTE (WS-CSO-SUB) EQUAL ',' OR                    
02364         WS-EDIT-BYTE (WS-CSO-SUB) EQUAL LOW-VALUES)               
02365            ADD +1               TO WS-CSO-SUB                     
02366            GO TO 1030-EDIT-FOR-NUMERICS.                          
02367                                                                   
02368      MOVE 'N'                   TO WS-NUMERIC-SW.                 
02369                                                                   
02370  1030-EDIT-EXIT.                                                  
02371 ******************************************************************
02372 *                                                                *
02373 *                 C A L C U L A T E   A G E                      *
02374 *                                                                *
02375 ******************************************************************
02376                                                                   
02377                                                                   
02378  1095-CALC-AGE.                                                   
02379                                                                   
02380      MOVE WS-CONVERTED-BIRTH TO DC-BIN-DATE-1.                    
02381      MOVE WS-CURRENT-BIN-DT  TO DC-BIN-DATE-2.                    
02382      MOVE 1                  TO DC-OPTION-CODE.                   
02383      PERFORM 9700-DATE-LINK                                       
02384      IF NO-CONVERSION-ERROR                                       
02385          COMPUTE WS-BAGE = DC-ELAPSED-MONTHS / 12                 
02386          MOVE WS-BAGE            TO BAGEI                         
02387          MOVE +2                 TO BAGEL                         
02388          MOVE AL-UNNON TO BAGEA.                                  
02389                                                                   
02390  1099-EXIT.                                                       
02391      EXIT.                                                        
02392                                                                   
02393      EJECT                                                        
02394                                                                   
02395 ******************************************************************
02396 *                                                                *
02397 *                   S H O W   I S S U E                          *
02398 *                                                                *
02399 ******************************************************************
02400                                                                   
02401  1100-SHOW-REQUEST.                                               
02402      MOVE 'S'                    TO PI-MAINT-FUNCTION.            
02403                                                                   
02404      IF BCERTNOL = ZEROS AND                                      
02405         BEFFDTL  = ZEROS                                          
02406         MOVE -1                  TO BMAINTL                       
02407         GO TO 8200-SEND-DATAONLY.                                 
02408                                                                   
02409      MOVE BCERTNOI               TO WS-NEW-PRIME.                 
02410                                                                   
02411      IF BSUFIXL NOT = ZEROS                                       
02412         MOVE BSUFIXI             TO WS-NEW-SUFFIX                 
02413      ELSE                                                         
02414         MOVE SPACE               TO WS-NEW-SUFFIX.                
02415                                                                   
02416      MOVE BEFFDTI                TO DEEDIT-FIELD.                 
02417      PERFORM 8600-DEEDIT.                                         
02418      MOVE DEEDIT-FIELD-V0        TO DC-GREG-DATE-1-MDY.           
02419      MOVE '4'                    TO DC-OPTION-CODE.               
02420      PERFORM 9700-DATE-LINK.                                      
02421      IF DATE-CONVERSION-ERROR                                     
02422         MOVE ER-0348             TO EMI-ERROR                     
02423         MOVE -1                  TO BEFFDTL                       
02424         MOVE AL-UABON            TO BEFFDTA                       
02425         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
02426         GO TO 8200-SEND-DATAONLY                                  
02427      ELSE                                                         
02428         MOVE DC-GREG-DATE-1-EDIT TO BEFFDTI                       
02429         MOVE AL-UANON            TO BEFFDTA                       
02430         MOVE DC-BIN-DATE-1       TO WS-NEW-CERT-EFF-DT.           
02431                                                                   
02432      MOVE PI-PREV-ALT-KEY     TO ERPNDB-ALT-KEY                   
02433                                                                   
02434      IF ERPNDB-CERT-NO     NOT = WS-NEW-CERT-NO   OR              
02435         ERPNDB-CERT-EFF-DT NOT = WS-NEW-CERT-EFF-DT               
02436           MOVE 'Y'                    TO WS-SHOW-SW               
02437           IF PI-PRIMARY-BROWSE OR PI-PRIMARY-WITH-SELECT          
02438              GO TO 4100-BROWSE-PRIMARY-FORWARD                    
02439            ELSE                                                   
02440              IF PI-FILE-BROWSE                                    
02441                 GO TO 4300-BROWSE-FILE-FORWARD                    
02442               ELSE                                                
02443                 IF PI-CSR-BROWSE                                  
02444                    GO TO 4200-BROWSE-CSR-FORWARD                  
02445                 ELSE                                              
02446                    GO TO 4000-BROWSE-DETAIL-FORWARD               
02447         ELSE                                                      
02448            GO TO 3900-RE-DISPLAY-RECORD.                          
02449                                                                   
02450      EJECT                                                        
02451                                                                   
02452 ******************************************************************
02453 *                                                                *
02454 *            A D D   I S S U E   R O U T I N E                   *
02455 *                                                                *
02456 ******************************************************************
02457                                                                   
02458 ***************************************************************   
02459 *     1. GETMAIN FOR PENDING BUSINESS RECORDS.                *   
02460 *     2. SAVE THE BATCH TOTAL DATA FOR UPDATING WITH NEW DATA.*   
02461 *     3. INITIALIZE RECORD WITH INITIAL DATA FROM SCREEN.     *   
02462 *     4. LINK TO THE EDIT ROUTINE.                            *   
02463 *     5. FLAG ALL FIELDS SHOWN IN ERROR BY THE EDIT ROUTINE.  *   
02464 *     6. WRITE THE NEW ISSUE RECORD.                          *   
02465 *     7. UPDATE THE BATCH TOTAL RECORD WITH NEW TOTALS.       *   
02466 *     8. IF ANY ERRORS ARE DETECTED, THE ISSUE SCREEN IS      *   
02467 *           DISPLAYED WITH ANY ASSOCIATED ERRORS,             *   
02468 *        ELSE                                                 *   
02469 *           A NEW INITIALIZED SCREEN WILL BE DISPLAYED.       *   
02470 ***************************************************************   
02471                                                                   
02472  1200-ADD-ISSUE-ROUTINE.                                          
02473      MOVE 'A'                    TO PI-MAINT-FUNCTION.            
02474                                                                   
02475      EXEC CICS GETMAIN                                            
02476          SET     (ADDRESS OF PENDING-BUSINESS)                    
02477          LENGTH  (ERPNDB-RECORD-LENGTH)                           
02478      END-EXEC.                                                    
02479                                                                   
02480      EXEC CICS HANDLE CONDITION                                   
02481           NOTFND    (1280-BATCH-HDR-NOT-FOUND)                    
02482      END-EXEC.                                                    
02483                                                                   
02484      MOVE PI-PREV-KEY            TO ERPNDB-KEY.                   
02485      MOVE +9999                  TO ERPNDB-BATCH-SEQ-NO.          
02486      MOVE ZEROS                  TO ERPNDB-BATCH-CHG-SEQ-NO.      
02487                                                                   
02488      EXEC CICS READ                                               
02489          INTO    (PENDING-BUSINESS)                               
02490          DATASET (FILE-ID-ERPNDB)                                 
02491          RIDFLD  (ERPNDB-KEY)                                     
02492      END-EXEC.                                                    
02493                                                                   
02494      MOVE PB-CSR-ID              TO WS-BATCH-CSR-ID.              
02495                                                                   
02496      MOVE PB-BATCH-RECORD        TO WS-BATCH-RECORD.              
02497      ADD +1                      TO WS-B-HIGHEST-SEQ-NO.          
02498      MOVE PB-CREDIT-SELECT-DT    TO WS-CREDIT-SELECT-DT.          
02499      MOVE SPACES                 TO PENDING-BUSINESS.             
02500      MOVE 'PB'                   TO PB-RECORD-ID.                 
02501      MOVE PI-PREV-KEY            TO PB-CONTROL-PRIMARY.           
02502      MOVE PI-PREV-ALT-KEY        TO PB-CONTROL-BY-ACCOUNT.        
02503      MOVE SPACES                 TO PB-CERT-SFX.                  
02504      MOVE WS-B-HIGHEST-SEQ-NO    TO PB-BATCH-SEQ-NO.              
02505      MOVE ZEROS                  TO PB-BATCH-CHG-SEQ-NO           
02506                                     PB-ALT-CHG-SEQ-NO.            
02507                                                                   
02508      MOVE BCERTNOI               TO PB-CERT-PRIME.                
02509      MOVE '1'                    TO PB-RECORD-TYPE.               
02510                                                                   
02511      IF BSUFIXL NOT = ZEROS                                       
02512         MOVE BSUFIXI             TO PB-CERT-SFX.                  
02513                                                                   
02514      MOVE WS-CONVERTED-EFFDT     TO PB-CERT-EFF-DT.               
02515                                                                   
02516      MOVE PI-COMPANY-CD          TO PB-COMPANY-CD.                
02517      MOVE PI-COMPANY-ID          TO PB-COMPANY-ID.                
02518                                                                   
02519      MOVE ZEROS                  TO PB-I-LOAN-TERM                
02520                                     PB-I-LF-PREM-CALC             
02521                                     PB-I-LF-ALT-PREM-CALC         
02522                                     PB-I-LF-RATE                  
02523                                     PB-I-LF-ALT-RATE              
02524                                     PB-I-LF-REI-RATE              
02525                                     PB-I-LF-ALT-REI-RATE          
02526                                     PB-I-RATE-DEV-PCT-LF          
02527                                     PB-I-AH-PREM-CALC             
02528                                     PB-I-AH-RATE                  
02529                                     PB-I-AH-REI-RATE              
02530                                     PB-I-AH-RATE-TRM              
02531                                     PB-I-RATE-DEV-PCT-AH          
02532                                     PB-I-BUSINESS-TYPE            
02533                                     PB-I-LIFE-COMMISSION          
02534                                     PB-I-JOINT-COMMISSION         
02535                                     PB-I-JOINT-AGE                
02536                                     PB-I-AH-COMMISSION            
02537                                     PB-I-CURR-SEQ                 
02538                                     PB-CHG-COUNT                  
02539                                     PB-LF-BILLED-AMTS             
02540                                     PB-AH-BILLED-AMTS             
02541                                     PB-CALC-TOLERANCE             
02542                                     PB-I-EXTENTION-DAYS           
032210*                                   PB-I-MICROFILM-NO             
02544                                     PB-I-TERM-IN-DAYS.            
02545                                                                   
02546      MOVE LOW-VALUES             TO PB-CREDIT-ACCEPT-DT           
02547                                     PB-I-LF-EXPIRE-DT             
02548                                     PB-I-AH-EXPIRE-DT             
02549                                     PB-BILLED-DT                  
02550                                     PB-ACCT-EFF-DT                
02551                                     PB-ACCT-EXP-DT                
02552                                     PB-I-1ST-PMT-DT.              
02553                                                                   
02554      MOVE 'X'                    TO PB-FATAL-FLAG.                
02555                                                                   
02556      MOVE WS-CREDIT-SELECT-DT    TO PB-CREDIT-SELECT-DT.          
02557                                                                   
02558      IF BLSTNML GREATER THAN ZEROS                                
02559          MOVE BLSTNMI            TO PB-I-INSURED-LAST-NAME.       
02560                                                                   
032210*    IF BMICRNOL  GREATER  ZEROS                                  
032210*        MOVE BMICRNOI           TO  PB-I-MICROFILM-NO            
032210*    ELSE                                                         
032210*        MOVE ZEROS              TO  PB-I-MICROFILM-NO.           

101706     IF BVINL > ZEROS
101706        MOVE BVINI               TO PB-I-VIN
101706     END-IF

02566      IF B1STNML GREATER THAN ZEROS                                
02567          MOVE B1STNMI            TO PB-I-INSURED-FIRST-NAME.      
02568                                                                   
02569      IF BINITL  GREATER THAN ZEROS                                
02570          MOVE BINITI             TO PB-I-INSURED-MIDDLE-INIT.     
02571                                                                   
02572      IF BBIRTHL GREATER THAN ZERO                                 
02573          MOVE WS-CONVERTED-BIRTH TO PB-I-BIRTHDAY                 
02574      ELSE                                                         
02575          MOVE LOW-VALUES         TO PB-I-BIRTHDAY.                
02576                                                                   
02581                                                                   
02582      IF BAGEL    GREATER THAN ZEROS                               
02583          MOVE BAGEI              TO PB-I-AGE                      
02584         ELSE                                                      
02585          MOVE ZEROS              TO PB-I-AGE.                     
02586                                                                   
02587      IF BSEXL    GREATER THAN ZEROS                               
02588          MOVE BSEXI              TO PB-I-INSURED-SEX.             
02589                                                                   
02590      IF BTERM-LEN (1) GREATER THAN ZEROS                          
02591         MOVE WS-BTERM (1)        TO PB-I-LF-TERM                  
02592      ELSE                                                         
02593         MOVE ZEROS               TO PB-I-LF-TERM.                 
02594                                                                   
02595      IF BTERM-LEN (2) GREATER THAN ZEROS                          
02596         MOVE WS-BTERM (2)        TO PB-I-AH-TERM                  
02597      ELSE                                                         
02598         MOVE ZEROS               TO PB-I-AH-TERM.                 
02599                                                                   
02604                                                                   
02609                                                                   
02610      IF BSKPCDL       GREATER THAN ZEROS                          
02611          MOVE BSKPCDI            TO PB-I-SKIP-CODE.               
02612                                                                   
02616                                                                   
02621          MOVE ZEROS              TO PB-I-NO-OF-PAYMENTS           
02622                                     PI-BPMTS.                     
02623                                                                   
02627          MOVE ZEROS              TO PB-I-PAYMENT-AMOUNT.          
02628                                                                   
02629      IF BTYPE-LEN (1)   GREATER THAN ZEROS                        
02630         IF BTYPE  (1)   NOT = ZEROS                               
02631            MOVE BTYPE (1)        TO PB-I-LF-INPUT-CD              
02632                                     PI-BTYPE (1)                  
02633            MOVE ZEROS            TO PB-I-LF-BENEFIT-CD            
02634            MOVE SPACES           TO PB-I-LF-ABBR                  
02635         ELSE                                                      
02636            MOVE SPACES           TO PB-I-LF-INPUT-CD              
02637                                     PB-I-LF-ABBR                  
02638            MOVE ZEROS            TO PB-I-LF-BENEFIT-CD            
02639      ELSE                                                         
02640         MOVE SPACES              TO PB-I-LF-INPUT-CD              
02641                                     PB-I-LF-ABBR                  
02642         MOVE ZEROS               TO PB-I-LF-BENEFIT-CD.           
02643                                                                   
02644      IF  BBEN-LEN      (1)  GREATER THAN ZEROS                    
02645          MOVE WS-BBEN  (1)       TO PB-I-LF-BENEFIT-AMT           
02646      ELSE                                                         
02647          MOVE ZEROS              TO PB-I-LF-BENEFIT-AMT.          
02648                                                                   
02649      IF  BALT-BEN-LEN     (1)  GREATER THAN ZEROS                 
02650          MOVE WS-BALT-BEN (1)    TO PB-I-LF-ALT-BENEFIT-AMT       
02651      ELSE                                                         
02652          MOVE ZEROS              TO PB-I-LF-ALT-BENEFIT-AMT.      
02653                                                                   
02654      IF  BPREM-LEN     (1)  GREATER THAN ZEROS                    
02655          IF WS-BPREM   (1)  NOT = WS-ALL-NINES                    
02656             ADD  WS-BPREM (1)    TO WS-B-LF-ISS-PRM-ENTERED       
02657             MOVE WS-BPREM (1)    TO PB-I-LF-PREMIUM-AMT           
02658          ELSE                                                     
120513            MOVE WS-BPREM (1)    TO PB-I-LF-PREMIUM-AMT
120513*           MOVE ZEROS           TO PB-I-LF-PREMIUM-AMT           
120513*           MOVE '?'             TO PB-I-LF-CALC-FLAG             
120513         END-IF
02661      ELSE                                                         
02662          MOVE ZEROS              TO PB-I-LF-PREMIUM-AMT.          
02663                                                                   
02664      IF  BCPREM-LEN     (1)  GREATER THAN ZEROS                   
02665          MOVE WS-BCPREM (1)      TO PB-I-LF-PREM-CALC.            
02666                                                                   
02667      IF  BALT-PREM-LEN     (1)  GREATER THAN ZEROS                
02668          ADD  WS-BALT-PREM (1) TO WS-B-LF-ISS-PRM-ENTERED         
02669          MOVE WS-BALT-PREM (1) TO PB-I-LF-ALT-PREMIUM-AMT         
02670      ELSE                                                         
02671          MOVE ZEROS            TO PB-I-LF-ALT-PREMIUM-AMT.        
02672                                                                   
02667      IF  BALT-PREM-LEN     (2) > ZEROS
02669          MOVE WS-BALT-PREM (2) TO PB-I-TOT-FEES
02670      ELSE                                                         
02671          MOVE ZEROS            TO PB-I-TOT-FEES.
02672                                                                   
02673      IF BTYPE-LEN     (2)   GREATER THAN ZEROS                    
02674         IF BTYPE      (2)   NOT = ZEROS                           
02675            MOVE BTYPE (2)      TO PB-I-AH-INPUT-CD                
02676                                   PI-BTYPE (2)                    
02677            MOVE ZEROS          TO PB-I-AH-BENEFIT-CD              
02678            MOVE SPACES         TO PB-I-AH-ABBR                    
02679         ELSE                                                      
02680            MOVE SPACES         TO PB-I-AH-INPUT-CD                
02681                                   PB-I-AH-ABBR                    
02682            MOVE ZEROS          TO PB-I-AH-BENEFIT-CD              
02683      ELSE                                                         
02684         MOVE SPACES            TO PB-I-AH-INPUT-CD                
02685                                   PB-I-AH-ABBR                    
02686         MOVE ZEROS             TO PB-I-AH-BENEFIT-CD.             
02687                                                                   
02688      IF  BBEN-LEN         (2)  GREATER THAN ZEROS                 
02689          MOVE WS-BBEN     (2)  TO PB-I-AH-BENEFIT-AMT             
02690      ELSE                                                         
02691          MOVE ZEROS            TO PB-I-AH-BENEFIT-AMT.            
02692                                                                   
02693      IF  BPREM-LEN     (2)  GREATER THAN ZEROS                    
02694          IF WS-BPREM   (2)  NOT = WS-ALL-NINES                    
02695             ADD  WS-BPREM (2)    TO WS-B-AH-ISS-PRM-ENTERED       
02696             MOVE WS-BPREM (2)    TO PB-I-AH-PREMIUM-AMT           
02697          ELSE                                                     
120513            MOVE WS-BPREM (2)    TO PB-I-AH-PREMIUM-AMT
120513*           MOVE ZEROS           TO PB-I-AH-PREMIUM-AMT           
120513*           MOVE '?'             TO PB-I-AH-CALC-FLAG             
120513         END-IF
02700      ELSE                                                         
02701          MOVE ZEROS              TO PB-I-AH-PREMIUM-AMT.          
02702                                                                   
02703      IF  BCPREM-LEN     (2)  GREATER THAN ZEROS                   
02704          MOVE WS-BCPREM (2)      TO PB-I-AH-PREM-CALC.            
02705                                                                   
02706      IF BCRIT-PERD-LEN      (1)   GREATER THAN ZEROS              
02707         MOVE WS-BCRIT-PERD  (1)   TO PB-I-LF-CRIT-PER             
02708      ELSE                                                         
02709         MOVE ZEROS                TO PB-I-LF-CRIT-PER.            
02710                                                                   
02711      IF BCRIT-PERD-LEN      (2)   GREATER THAN ZEROS              
02712         MOVE WS-BCRIT-PERD  (2)   TO PB-I-AH-CRIT-PER             
02713      ELSE                                                         
02714         MOVE ZEROS                TO PB-I-AH-CRIT-PER.            
02715                                                                   
02716      IF BINDGRPL  GREATER THAN ZEROS                              
02717          MOVE BINDGRPI           TO PB-I-INDV-GRP-OVRD.           
02718                                                                   
02719      IF BRTCLSL   GREATER THAN ZEROS                              
02720          MOVE BRTCLSI            TO PB-I-RATE-CLASS-OVRD.         
02721                                                                   
02722      IF BSIGL     GREATER THAN ZEROS                              
02723          MOVE BSIGI              TO PB-I-SIG-SW.                  


02730      IF BMEMBERL  GREATER THAN ZEROS                              
02731          MOVE BMEMBERI       TO PB-I-MEMBER-NO.                   
02732                                                                   
02733      IF BLNOFCRL  GREATER THAN ZEROS                              
02734          MOVE BLNOFCRI           TO PB-I-LOAN-OFFICER.            
02735                                                                   
02736      IF BEXPIRE-LEN    (1)  GREATER THAN ZEROS                    
02737         MOVE WS-CONVERTED-EXPIRDT (1) TO PB-I-LF-EXPIRE-DT        
02738      ELSE                                                         
02739         MOVE LOW-VALUES               TO PB-I-LF-EXPIRE-DT.       
02740                                                                   
02741      IF BEXPIRE-LEN    (2)  GREATER THAN ZEROS                    
02742         MOVE WS-CONVERTED-EXPIRDT (2) TO PB-I-AH-EXPIRE-DT        
02743      ELSE                                                         
02744         MOVE LOW-VALUES               TO PB-I-AH-EXPIRE-DT.       
02745                                                                   
02755                                                                   
02756      IF BEXPIRE-LEN    (1)  GREATER THAN ZEROS                    
02757         IF WS-CONVERTED-EXPIRDT   (1) GREATER THAN LOW-VALUES     
02758            MOVE '3'              TO PB-I-DATA-ENTRY-SW.           
02759                                                                   
02760      IF BEXPIRE-LEN    (2)  GREATER THAN ZEROS                    
02761         IF WS-CONVERTED-EXPIRDT   (2) GREATER THAN LOW-VALUES     
02762            MOVE '3'              TO PB-I-DATA-ENTRY-SW.           
02763                                                                   
02766                                                                   
02767      IF PB-EXT-DAYS-PROCESSING                                    
02768         IF PB-I-EXTENTION-DAYS = ZEROS                            
02769            MOVE '1'              TO PB-I-DATA-ENTRY-SW.           
02770                                                                   
02771      IF PB-EXPIRE-DT-PROCESSING                                   
02772         IF PB-I-LF-EXPIRE-DT = LOW-VALUES AND                     
02773            PB-I-AH-EXPIRE-DT = LOW-VALUES                         
02774              MOVE '1'            TO PB-I-DATA-ENTRY-SW.           
02775                                                                   
02776      IF PB-1ST-PMT-DT-PROCESSING                                  
02777         IF PB-I-1ST-PMT-DT = LOW-VALUES                           
02778            MOVE '1'              TO PB-I-DATA-ENTRY-SW.           
02779                                                                   
02780      IF BREINL GREATER THAN ZEROS                                 
02781         MOVE BREINI              TO PB-I-SPECIAL-REIN-CODE.       
02782                                                                   
02783      IF BENTRYL NOT = ZEROS                                       
02784         MOVE BENTRYI             TO PB-BATCH-ENTRY
102408        IF PI-COMPANY-ID = 'CID'
102408           AND (PB-BATCH-ENTRY = 'P')
102408           AND (PB-STATE = 'VA')
102408           PERFORM 1500-UPDATE-ERNOTE
102408                                 THRU 1500-EXIT
102408        END-IF
102408     END-IF

02786      IF BUNWRITL NOT = ZEROS                                      
02787         MOVE BUNWRITI            TO PB-I-UNDERWRITING-STATUS.     
02788                                                                   
02789      IF BFORCEL NOT = ZEROS                                       
02790         MOVE BFORCEI             TO PB-FORCE-CODE.                
02791                                                                   
02792      IF BBILCDL NOT = ZEROS                                       
02793         MOVE BBILCDI             TO PB-RECORD-BILL.               
02794                                                                   
02799                                                                   
02806                                                                   
02807  1260-WRITE-PB-RECORD.                                            
02808      MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY              
02809                                     PB-INPUT-BY.                  
02810      MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.         
02811      MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT              
02812                                     PB-INPUT-DT.                  
02813                                                                   
02814      MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY.              
02815      MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY                   
02816                                     PB-CONTROL-BY-ORIG-BATCH.     
02817                                                                   
02818                                                                   
02819      MOVE PB-COMPANY-CD          TO PB-CSR-COMPANY-CD.            
02820      MOVE WS-BATCH-CSR-ID        TO PB-CSR-ID.                    
02821      MOVE PB-ENTRY-BATCH         TO PB-CSR-ENTRY-BATCH.           
02822      MOVE PB-BATCH-SEQ-NO        TO PB-CSR-BATCH-SEQ-NO.          
02823      MOVE PB-BATCH-CHG-SEQ-NO    TO PB-CSR-BATCH-CHG-SEQ-NO.      
02824                                                                   
02825      PERFORM 9800-LINK-PENDING-EDIT THRU 9800-EXIT.               
02826                                                                   
02827      PERFORM 7300-FORMAT-ERRORS  THRU 7399-EXIT.                  
02828                                                                   
02829      MOVE PB-SV-CARRIER          TO PI-SV-CARRIER.                
02830      MOVE PB-SV-GROUPING         TO PI-SV-GROUPING.               
02831      MOVE PB-SV-STATE            TO PI-SV-STATE.                  
02832                                                                   
02833      ADD PB-I-LF-PREM-CALC       TO WS-B-LF-ISS-PRM-COMPUTED.     
02834      ADD PB-I-AH-PREM-CALC       TO WS-B-AH-ISS-PRM-COMPUTED.     
02835                                                                   
02836  1275-WRITE-PENDING-BUSINESS.                                     
02837      MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
02838      MOVE FILE-ID-ERPNDB         TO JP-FILE-ID.                   
02839      MOVE 'A'                    TO JP-RECORD-TYPE.               
02840      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
02841                                                                   
02842      EXEC CICS HANDLE CONDITION                                   
02843          DUPREC (1290-DUPLICATE-ALT-INDEX)                        
02844      END-EXEC.                                                    
02845                                                                   
02846      EXEC CICS WRITE                                              
02847          DATASET (FILE-ID-ERPNDB)                                 
02848          FROM    (PENDING-BUSINESS)                               
02849          RIDFLD  (PB-CONTROL-PRIMARY)                             
02850      END-EXEC.                                                    
02851                                                                   
02852      ADD +1                      TO  WS-B-ISSUE-CNT-ENTERED.      
02853                                                                   
02854      PERFORM 8400-LOG-JOURNAL-RECORD.                             
02855                                                                   
      ******************************************************************
      *       A D D  O R I G I N A L  C E R T  I N F O                 *
      ******************************************************************

      *    display ' made it to add orig cert '

           MOVE ELCERT-KEY             TO ERMAIL-KEY
           MOVE ' '                    TO WS-ERMAIL-SW

           EXEC CICS READ
              DATASET   (FILE-ID-ERMAIL)
              SET       (ADDRESS OF MAILING-DATA)
              RIDFLD    (ERMAIL-KEY)
              RESP      (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
              SET ERMAIL-FOUND TO TRUE
           END-IF
121712
121712     PERFORM 1295-READ-CERT-TRAILER THRU 1295-EXIT

           MOVE SPACES                 TO ORIGINAL-CERTIFICATE
           MOVE 'OC'                   TO OC-RECORD-ID
           MOVE PB-CONTROL-BY-ACCOUNT (1:33)
                                       TO OC-CONTROL-PRIMARY (1:33)
           MOVE 'I'                    TO OC-RECORD-TYPE
           MOVE +4096                  TO OC-KEY-SEQ-NO
           MOVE PI-PROCESSOR-ID        TO OC-LAST-MAINT-BY
           MOVE EIBTIME                TO OC-LAST-MAINT-HHMMSS
           MOVE WS-CURRENT-BIN-DT      TO OC-LAST-MAINT-DT

062712     MOVE PB-I-INSURED-LAST-NAME TO OC-INS-LAST-NAME   
062712     MOVE PB-I-INSURED-FIRST-NAME TO OC-INS-FIRST-NAME  
062712     MOVE PB-I-INSURED-MIDDLE-INIT TO OC-INS-MIDDLE-INIT 
062712     MOVE PB-I-AGE               TO OC-INS-AGE         
062712     MOVE PB-I-JOINT-LAST-NAME   TO OC-JNT-LAST-NAME   
062712     MOVE PB-I-JOINT-FIRST-NAME  TO OC-JNT-FIRST-NAME  
062712     MOVE PB-I-JOINT-MIDDLE-INIT TO OC-JNT-MIDDLE-INIT 
062712     MOVE PB-I-JOINT-AGE         TO OC-JNT-AGE         
062712     MOVE PB-I-LF-BENEFIT-CD     TO OC-LF-BENCD        
062712     MOVE PB-I-LF-TERM           TO OC-LF-TERM         
062712     MOVE PB-I-LF-BENEFIT-AMT    TO OC-LF-BEN-AMT      
062712     MOVE PB-I-LF-PREMIUM-AMT    TO OC-LF-PRM-AMT      
062712     MOVE PB-I-LF-ALT-BENEFIT-AMT TO OC-LF-ALT-BEN-AMT  
062712     MOVE PB-I-LF-ALT-PREMIUM-AMT TO OC-LF-ALT-PRM-AMT  
062712     MOVE PB-I-LF-EXPIRE-DT      TO OC-LF-EXP-DT       
062712     IF PB-I-JOINT-COMMISSION > +0
062712         MOVE PB-I-JOINT-COMMISSION TO OC-LF-COMM-PCT
062712     ELSE
062712         MOVE PB-I-LIFE-COMMISSION  TO OC-LF-COMM-PCT
062712     END-IF
062712     MOVE LOW-VALUES             TO OC-LF-CANCEL-DT    
062712     MOVE +0                     TO OC-LF-CANCEL-AMT
071712                                    OC-LF-ITD-CANCEL-AMT
062712     MOVE PB-I-AH-BENEFIT-CD     TO OC-AH-BENCD        
062712     MOVE PB-I-AH-TERM           TO OC-AH-TERM         
062712     MOVE PB-I-AH-BENEFIT-AMT    TO OC-AH-BEN-AMT      
062712     MOVE PB-I-AH-PREMIUM-AMT    TO OC-AH-PRM-AMT      
062712     MOVE PB-I-AH-EXPIRE-DT      TO OC-AH-EXP-DT       
062712     MOVE PB-I-AH-COMMISSION     TO OC-AH-COMM-PCT     
062712     MOVE PB-I-AH-CRIT-PER       TO OC-AH-CP           
062712     MOVE LOW-VALUES             TO OC-AH-CANCEL-DT    
062712     MOVE +0                     TO OC-AH-CANCEL-AMT
071712                                    OC-AH-ITD-CANCEL-AMT
062712     MOVE PB-I-1ST-PMT-DT        TO OC-1ST-PMT-DT
011413     MOVE 'Y'                    TO OC-ISSUE-TRAN-IND
011413     MOVE 'N'                    TO OC-CANCEL-TRAN-IND
           
           IF ERMAIL-FOUND
              MOVE MA-CRED-BENE-NAME   TO OC-CRED-BENE-NAME
           END-IF
121712     IF NOT CERT-TRL-REC-NOT-FOUND
121712        MOVE CS-INS-AGE-DEFAULT-FLAG TO 
121712                           OC-INS-AGE-DEFAULT-FLAG
121712        MOVE CS-JNT-AGE-DEFAULT-FLAG TO 
121712                           OC-JNT-AGE-DEFAULT-FLAG
121712     END-IF
           MOVE LOW-VALUES             TO OC-ENDORSEMENT-PROCESSED-DT
           .
       1275-WRITE-ELCRTO.

           EXEC CICS WRITE
              DATASET   ('ELCRTO')
              FROM      (ORIGINAL-CERTIFICATE)
              RIDFLD    (OC-CONTROL-PRIMARY)
              RESP      (WS-RESPONSE)
           END-EXEC

      *    display ' just wrote 1275 ' ws-response
           IF RESP-DUPKEY
              SUBTRACT +1              FROM OC-KEY-SEQ-NO
              GO TO 1275-WRITE-ELCRTO
           ELSE
              IF NOT RESP-NORMAL
                 MOVE -1               TO BMAINTL
                 MOVE ER-7450          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 GO TO 8200-SEND-DATAONLY
              END-IF
           END-IF

02856 ******************************************************************
02857 *       U P D A T E   T H E   B A T C H   H E A D E R            *
02858 ******************************************************************
02859                                                                   
02860      EXEC CICS READ                                               
02861          INTO    (PENDING-BUSINESS)                               
02862          DATASET (FILE-ID-ERPNDB)                                 
02863          RIDFLD  (ERPNDB-KEY)                                     
02864          UPDATE                                                   
02865      END-EXEC.                                                    
02866                                                                   
02867      MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
02868      MOVE FILE-ID-ERPNDB         TO JP-FILE-ID.                   
02869      MOVE 'B'                    TO JP-RECORD-TYPE.               
02870      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
02871                                                                   
02872      PERFORM 8400-LOG-JOURNAL-RECORD.                             
02873                                                                   
02874      MOVE WS-BATCH-RECORD        TO PB-BATCH-RECORD.              
02875                                                                   
02876      EXEC CICS REWRITE                                            
02877          DATASET (FILE-ID-ERPNDB)                                 
02878          FROM    (PENDING-BUSINESS)                               
02879      END-EXEC.                                                    
02880                                                                   
02881      MOVE 'C'                    TO JP-RECORD-TYPE.               
02882      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
02883                                                                   
02884      PERFORM 8400-LOG-JOURNAL-RECORD.                             
02885                                                                   
02886      IF WS-ERRORS-PRESENT                                         
02887          GO TO 3900-RE-DISPLAY-RECORD.                            
02888                                                                   
02889      MOVE ZEROS                  TO EMI-ERROR.                    
02890      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
02891      MOVE LOW-VALUES             TO VP631BI.                      
02892      MOVE  -1                    TO BMAINTL.                      
02893      GO TO 8100-SEND-INITIAL-MAP.                                 
02894                                                                   
02895  1280-BATCH-HDR-NOT-FOUND.                                        
02896      MOVE -1                     TO BMAINTL.                      
02897      MOVE ER-7450                TO EMI-ERROR.                    
02898      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
02899      GO TO 8200-SEND-DATAONLY.                                    
02900                                                                   
02901  1290-DUPLICATE-ALT-INDEX.                                        
02902                                                                   
02903 ******************************************************************
02904 *                                                                *
02905 *      IF THE PB-B-HIGHEST-SEQ-NO WAS NOT UPDATED IN THE DATA    *
02906 *         ENTRY SESSION, ADD +1 TO THE HIGHEST SEQUENCE NUMBER   *
02907 *         AND ATTEMPT TO WRITE THE RECORD AGAIN. REPEAT THIS     *
02908 *         50 TIMES.  IF DUPREC CONDITION IS STILL ENCOUNTERED    *
02909 *         AFTER 50 TIMES, ASSUME THE DUPLICATE RECORD WAS        *
02910 *         CAUSED BY A DUPLICATE ALTERNATE KEY.                   *
02911 *                                                                *
02912 ******************************************************************
02913                                                                   
02914      IF LCP-ONCTR-01 =  0                                         
02915          ADD 1 TO LCP-ONCTR-01                                    
02916         MOVE +0                  TO WS-SEQ-NO-COUNTER.            
02917                                                                   
02918      ADD +1                      TO WS-SEQ-NO-COUNTER.            
02919                                                                   
02920      IF  WS-SEQ-NO-COUNTER GREATER THAN +50                       
02921          NEXT SENTENCE                                            
02922      ELSE                                                         
02923          ADD +1 TO PB-BATCH-SEQ-NO                                
02924                    PB-CSR-BATCH-SEQ-NO                            
02925          MOVE PB-BATCH-SEQ-NO TO WS-B-HIGHEST-SEQ-NO              
02926          GO TO 1275-WRITE-PENDING-BUSINESS.                       
02927                                                                   
02928      MOVE -1                     TO BCERTNOL.                     
02929      MOVE AL-UABON               TO BCERTNOA.                     
02930      MOVE AL-UNBON               TO BEFFDTA.                      
02931                                                                   
02932      MOVE 1                      TO EMI-SUB                       
02933                                     EMI-SWITCH1                   
02934                                     EMI-SWITCH-AREA-1             
02935                                     EMI-SWITCH-AREA-2.            
02936                                                                   
02937      MOVE SPACES                 TO EMI-ERROR-LINES.              
02938                                                                   
02939      EXEC CICS SYNCPOINT ROLLBACK                                 
02940      END-EXEC.                                                    
02941                                                                   
02942      MOVE ER-2247                TO EMI-ERROR.                    
02943      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
02944      GO TO 8200-SEND-DATAONLY.                                    
02945                                                                   
02946      EJECT                                                        
02947                                                                   
121712
121712 1295-READ-CERT-TRAILER.
121712
121712     EXEC CICS GETMAIN
121712         SET     (ADDRESS OF CERTIFICATE-TRAILERS)
121712         LENGTH  (ELCRTT-RECORD-LENGTH)
121712         INITIMG (GETMAIN-SPACE)                                 
121712     END-EXEC
121712
121712     MOVE +0                     TO  WS-CERT-TRL-REC-NOT-FOUND.
121712     MOVE ELCERT-KEY             TO  ELCRTT-PRIMARY.
121712     MOVE 'C'                    TO  ELCRTT-REC-TYPE.
121712
121712     EXEC CICS HANDLE CONDITION
121712         NOTFND (1295-CERT-TRL-REC-NOTFND)
121712     END-EXEC.
121712
121712     EXEC CICS READ
121712         DATASET  (CRTT-ID)
121712         RIDFLD   (ELCRTT-KEY)
121712         INTO     (CERTIFICATE-TRAILERS)
121712     END-EXEC.
121712
121712     GO TO 1295-EXIT.
121712
121712 1295-CERT-TRL-REC-NOTFND.
121712     MOVE +1                     TO WS-CERT-TRL-REC-NOT-FOUND.
121712
121712 1295-EXIT.
121712     EXIT.
121712
02948 ******************************************************************
02949 *                                                                *
02950 *          C H A N G E   I S S U E   R O U T I N E               *
02951 *                                                                *
02952 ******************************************************************
02953                                                                   
02954 ***************************************************************   
02955 *                                                             *   
02956 *     1. GETMAIN FOR PENDING BUSINESS RECORD.                 *   
02957 *     2. SAVE THE BATCH TOTAL DATA FOR UPDATING WITH NEW DATA.*   
02958 *     3. GET THE RECORD TO BE UPDATED AND SEE IF IT HAS BEEN  *   
02959 *              BILLED.  IF IT HAS THEN SAVE A -COPY- OF THE   *   
02960 *              ZERO RECORD FOR CREATING REVERSAL ENTRIES.     *   
02961 *     4. UPDATE THE RECORD WITH THE SCREEN DATA               *   
02962 *     5. LINK TO THE EDIT ROUTINE.                            *   
02963 *     6. FLAG ALL FIELDS SHOWN IN ERROR BY THE EDIT ROUTINE.  *   
02964 *     7. REDISPLAY SCREEN DATA WITH NEW DATA RETURNED FROM    *   
02965 *              THE EDIT ROUTINE.                              *   
02966 *     7. REWRITE THE NEW ISSUE                                *   
02967 *     8. UPDATE THE BATCH TOTAL RECORD WITH NEW TOTALS.       *   
02968 *                                                             *   
02969 ***************************************************************   
02970                                                                   
02971  1300-CHANGE-ISSUE-ROUTINE.                                       
02972      MOVE 'C'                    TO PI-MAINT-FUNCTION.            
02973                                                                   
02974      IF BCHGL NOT = ZEROS                                         
02975         MOVE ER-2196             TO EMI-ERROR                     
02976         MOVE AL-UABON            TO BMAINTA                       
02977         MOVE -1                  TO BMAINTL                       
02978         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
02979         GO TO 8200-SEND-DATAONLY.                                 
02980                                                                   
02981      MOVE BCERTNOI               TO WS-NEW-PRIME.                 
02982                                                                   
02983      IF BSUFIXL NOT = ZEROS                                       
02984         MOVE BSUFIXI             TO WS-NEW-SUFFIX                 
02985      ELSE                                                         
02986         MOVE SPACE               TO WS-NEW-SUFFIX.                
02987                                                                   
02988      MOVE BEFFDTI                TO DEEDIT-FIELD.                 
02989      PERFORM 8600-DEEDIT.                                         
02990      MOVE DEEDIT-FIELD-V0        TO DC-GREG-DATE-1-MDY.           
02991      MOVE '4'                    TO DC-OPTION-CODE.               
02992      PERFORM 9700-DATE-LINK.                                      
02993      IF DATE-CONVERSION-ERROR                                     
02994         MOVE ER-0348             TO EMI-ERROR                     
02995         MOVE -1                  TO BEFFDTL                       
02996         MOVE AL-UABON            TO BEFFDTA                       
02997         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
02998         GO TO 8200-SEND-DATAONLY                                  
02999      ELSE                                                         
03000         MOVE DC-GREG-DATE-1-EDIT TO BEFFDTI                       
03001         MOVE AL-UANON            TO BEFFDTA                       
03002         MOVE DC-BIN-DATE-1       TO WS-NEW-CERT-EFF-DT.           
03003                                                                   
03004  1325-PROCESS-ISSUE-CHANGES.                                      
03005      EXEC CICS HANDLE CONDITION                                   
03006           NOTFND    (1380-BATCH-HDR-NOT-FOUND)                    
03007      END-EXEC.                                                    
03008                                                                   
03009      MOVE PI-PREV-CONTROL-PRIMARY TO ERPNDB-KEY.                  
03010      MOVE +9999                   TO ERPNDB-BATCH-SEQ-NO.         
03011      MOVE ZEROS                   TO ERPNDB-BATCH-CHG-SEQ-NO.     
03012                                                                   
03013      EXEC CICS GETMAIN                                            
03014          SET      (ADDRESS OF PENDING-BUSINESS)                   
03015          LENGTH   (ERPNDB-RECORD-LENGTH)                          
03016          INITIMG  (GETMAIN-SPACE)                                 
03017       END-EXEC.                                                   
03018                                                                   
03019      EXEC CICS READ                                               
03020          INTO    (PENDING-BUSINESS)                               
03021          DATASET (FILE-ID-ERPNDB)                                 
03022          RIDFLD  (ERPNDB-KEY)                                     
03023      END-EXEC.                                                    
03024                                                                   
03025      MOVE PB-BATCH-RECORD        TO WS-BATCH-RECORD.              
03026                                                                   
03027      EXEC CICS HANDLE CONDITION                                   
03028           NOTFND     (1385-REC-NOT-FOUND)                         
03029      END-EXEC.                                                    
03030                                                                   
03031 **********************************************************        
03032 *      DONT READ THE RECORD FOR UPDATE UNTIL             *        
03033 *      AFTER THE CHANGE RECORD HAS BEEN CREATED.         *        
03034 **********************************************************        
03035                                                                   
03036      EXEC CICS READ                                               
03037          DATASET (FILE-ID-ERPNDB)                                 
03038          INTO    (PENDING-BUSINESS)                               
03039          RIDFLD  (PI-PREV-CONTROL-PRIMARY)                        
03040      END-EXEC.                                                    
03041                                                                   
03042 ********** DO NOT ALLOW CHANGES IF THRU A/R                       
03043      IF BMAINTI = 'C'                                             
03044          IF PB-EL860-INTERNAL-PROCESS                             
03045              MOVE ER-4005          TO EMI-ERROR                   
03046              MOVE -1               TO BMAINTL                     
03047              MOVE AL-UABON         TO BMAINTA                     
03048              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
03049              GO TO 8200-SEND-DATAONLY.                            
03050 **********                                                        
03051                                                                   
03052      IF PB-CREDIT-ACCEPT-DT NOT = LOW-VALUES                      
03053         MOVE -1                 TO BMAINTL                        
03054         MOVE ER-2197            TO EMI-ERROR                      
03055         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
03056         GO TO 8200-SEND-DATAONLY.                                 
03057                                                                   
03058      IF PB-BATCH-CHG-SEQ-NO NOT = ZEROS                           
03059         MOVE ER-2196             TO EMI-ERROR                     
03060         MOVE AL-UABON            TO BMAINTA                       
03061         MOVE -1                  TO BMAINTL                       
03062         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
03063         GO TO 8200-SEND-DATAONLY.                                 
03064                                                                   
03065      MOVE ZEROS                  TO WS-SEQ-SAVE.                  
03066                                                                   
03067      IF PB-BILLED-DT = LOW-VALUES AND                             
03068         PB-CHG-COUNT = ZEROS                                      
03069         NEXT SENTENCE                                             
03070      ELSE                                                         
03071         IF (BPREM-LEN (1) GREATER THAN ZEROS AND                  
03072             WS-BPREM  (1) NOT = PB-I-LF-PREMIUM-AMT)              
03073            OR                                                     
03074            (BPREM-LEN (2) GREATER THAN ZEROS AND                  
03075             WS-BPREM  (2) NOT = PB-I-AH-PREMIUM-AMT)              
03076            PERFORM 2700-BUILD-CORRECTION-RECORD THRU 2799-EXIT.   

091615     move zeros                  to ws-check-amts
091615     move ' '                    to ws-stop-sw
091615                                    ws-check-sw
091615                                    ws-proc-check-sw
091615                                    ws-chek-browse-sw
091615
091615     perform 2500-browse-checks  thru 2500-exit
091615
091615     if i-have-processed-checks
091615        or i-have-checks
091615        continue
091615     else
091615        go to 1330-continue
091615     end-if
091615
091615     compute ws-tot-orig-prem =
091615        pb-i-lf-premium-amt + pb-i-ah-premium-amt +
091615        pb-i-lf-alt-premium-amt
091615
091615     if bprem1l <> zeros
091615        MOVE AL-UNNON            TO bprem1a
091615        move ws-bprem (1)        to ws-new-iss-total
091615     else
091615        move pb-i-lf-premium-amt to ws-new-iss-total
091615     end-if
091615     display ' new iss totala ' ws-new-iss-total
091615
091615     if baltpr1l <> zeros
091615        MOVE AL-UNNON            TO baltpr1a
091615        compute ws-new-iss-total = 
091615           ws-new-iss-total + ws-balt-prem(1)
091615     else
091615        compute ws-new-iss-total = 
091615           ws-new-iss-total + pb-i-lf-alt-premium-amt
091615     end-if
091615
091615     if bprem2l <> zeros
091615        MOVE AL-UNNON            TO bprem2a
091615        compute ws-new-iss-total = ws-new-iss-total +
091615           ws-bprem (2)
091615     else
091615        compute ws-new-iss-total = ws-new-iss-total +
091615           pb-i-ah-premium-amt
091615     end-if
091615        
091615     if (ws-new-iss-total + ws-check-amts) = ws-tot-orig-prem
091615        if ((bprem1l <> zeros)
091615           and (ws-bprem (1) <> pb-i-lf-premium-amt))
091615                      or
091615           ((bprem2l <> zeros)
091615           and (ws-bprem (2) <> pb-i-ah-premium-amt))
091615           move er-3456          to emi-error
091615           perform 9900-error-format
091615                                 thru 9900-exit
091615           go to 1330-continue
091615        end-if
091615     end-if
091615        
091615     if ws-new-iss-total > ws-tot-orig-prem
091615        move er-3458             to emi-error
091615        perform 9900-error-format thru 9900-exit
091615        move al-uabon            to bmainta
091615        move -1                  to bmaintl
091615        go to 8200-send-dataonly
091615     end-if
091615     
091615     if ws-new-iss-total < ws-tot-orig-prem
091615        move er-3457             to emi-error
091615        move al-uabon            to bprem1a
091615                                    bprem2a
091615        move +0 to bprem1l bprem2l
091615        perform 9900-error-format thru 9900-exit
091615        move -1                  to bmaintl
091615        go to 1330-continue
091615     end-if
091615
091615     .
091615 1330-continue.
03077                                                                   
03078      EXEC CICS READ                                               
03079          DATASET (FILE-ID-ERPNDB)                                 
03080          INTO    (PENDING-BUSINESS)                               
03081          RIDFLD  (PI-PREV-CONTROL-PRIMARY)                        
03082          UPDATE                                                   
03083      END-EXEC.                                                    
03084                                                                   
03085      MOVE FILE-ID-ERPNDB         TO JP-FILE-ID.                   
03086      MOVE 'B'                    TO JP-RECORD-TYPE                
03087      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
03088      MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
03089                                                                   
03090      PERFORM 8400-LOG-JOURNAL-RECORD.                             
03091                                                                   
03092      IF PB-BILLED-DT NOT = LOW-VALUES OR                          
03093         PB-CHG-COUNT NOT = ZEROS                                  
03094         IF (BPREM-LEN (1) GREATER THAN ZEROS AND                  
03095             WS-BPREM  (1) NOT = PB-I-LF-PREMIUM-AMT)              
03096            OR                                                     
03097            (BPREM-LEN (2) GREATER THAN ZEROS AND                  
03098             WS-BPREM  (2) NOT = PB-I-AH-PREMIUM-AMT)              
03099             MOVE LOW-VALUES             TO PB-BILLED-DT           
03100             ADD +1                      TO PB-CHG-COUNT.          
03101                                                                   
03102      MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY.             
03103      MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.         
03104      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              
03105      MOVE '5'                    TO DC-OPTION-CODE.               
03106      PERFORM 9700-DATE-LINK                                       
03107      MOVE DC-BIN-DATE-1          TO PB-LAST-MAINT-DT.             
03108                                                                   
032210*    IF BMICRNOL  GREATER  ZEROS                                  
032210*        MOVE BMICRNOI           TO  PB-I-MICROFILM-NO.           
03111                                                                   

101706     IF BVINL > ZEROS
101706        MOVE BVINI               TO PB-I-VIN
101706     END-IF

073107     IF (BFORCEL NOT = ZEROS)
073107        AND (PB-COMPANY-ID = 'DCC' or 'VPP')
073107        AND (BFORCEI = ' ')
073107        AND (PB-FORCE-CODE = 'O')
              COMPUTE PB-I-AH-PREMIUM-AMT =
                 PB-I-DCC-OVER-CHG-AMT + PB-I-AH-PREM-CALC
073107        PERFORM 1800-REMOVE-CERT-NOTES
                                       THRU 1800-EXIT
073107        PERFORM 1900-DELETE-ERPYAJ
                                       THRU 1900-EXIT
              MOVE +0                  TO PB-I-DCC-OVER-CHG-AMT
073107     END-IF

073107     IF (BFORCEL NOT = ZEROS)
073107        AND (PB-COMPANY-ID = 'DCC' or 'VPP')
073107        AND (BFORCEI = 'O')
073107        AND (PB-FORCE-CODE NOT = 'O')
              COMPUTE WS-OC-AMT = PB-I-AH-PREMIUM-AMT -
                 PB-I-AH-PREM-CALC
              MOVE WS-CURRENT-BIN-DT   TO DC-BIN-DATE-1
              MOVE SPACE               TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK
              MOVE DC-GREG-DATE-A-EDIT TO WS-OC-DATE
              MOVE WS-OC-AMT           TO PB-I-DCC-OVER-CHG-AMT
              MOVE PB-I-AH-PREM-CALC   TO PB-I-AH-PREMIUM-AMT
              MOVE WS-OC-AMT           TO WS-OC-DIS-AMT
073107        PERFORM 1500-UPDATE-ERNOTE
                                       THRU 1500-EXIT
073107        PERFORM 1600-UPDATE-ERPYAJ
                                       THRU 1600-EXIT
073107     END-IF

03112      IF BLSTNML  GREATER THAN ZEROS                               
03113          MOVE BLSTNMI            TO PB-I-INSURED-LAST-NAME.       
03114                                                                   
03115      IF B1STNML  GREATER THAN ZEROS                               
03116          MOVE B1STNMI            TO PB-I-INSURED-FIRST-NAME.      
03117                                                                   
03118      IF BINITL   GREATER THAN ZEROS                               
03119          MOVE BINITI             TO PB-I-INSURED-MIDDLE-INIT.     
03120                                                                   
03121      IF BBIRTHL  GREATER THAN ZERO                                
03122          MOVE WS-CONVERTED-BIRTH TO PB-I-BIRTHDAY.                
03123                                                                   
03126                                                                   
03127      IF BAGEL    GREATER THAN ZEROS                               
03128          MOVE BAGEI              TO PB-I-AGE.                     
121712
121712     MOVE 'N'                    TO WS-INS-AGE-SET
121712                                    WS-JNT-AGE-SET.
121712     IF BAGEL    GREATER THAN ZEROS
121712        MOVE 'Y'                TO WS-INS-AGE-SET
121712        IF BAGEI = ZERO AND 
121712             (PB-I-BIRTHDAY = LOW-VALUES OR SPACES)
121712           MOVE 'Y'             TO WS-INS-AGE-DEFAULTED
121712           MOVE 'AGE*'          TO BAGEDEFO
121712        ELSE
121712           MOVE 'N'             TO WS-INS-AGE-DEFAULTED
121712           MOVE 'AGE '          TO BAGEDEFO
121712        END-IF
121712        PERFORM 1386-UPDATE-AGE-FLAGS THRU 1386-EXIT
121712     END-IF.

           if (byearl      <> zeros)
              or (bmakel   <> zeros)
              or (bmodell  <> zeros)
              or (bometerl <> zeros)
              MOVE PB-CONTROL-BY-ACCOUNT (1:33)
                                       TO ELCRTT-PRIMARY
              MOVE 'C'                 TO ELCRTT-REC-TYPE
              perform 1390-read-elcrtt-update
                                       thru 1390-exit
              if resp-normal
                 perform 1391-rewrite-elcrtt
                                       thru 1391-exit
              else
                 if resp-notfnd
                    perform 1392-write-elcrtt
                                       thru 1392-exit
                 end-if
              end-if
           end-if

03130      IF BSEXL    GREATER THAN ZEROS                               
03131          MOVE BSEXI              TO PB-I-INSURED-SEX.             
03132                                                                   
03133      IF BTERM-LEN (1)   GREATER THAN ZEROS                        
03134         MOVE WS-BTERM (1)        TO PB-I-LF-TERM.                 
03135                                                                   
03136      IF BTERM-LEN (2)   GREATER THAN ZEROS                        
03137         MOVE WS-BTERM (2)        TO PB-I-AH-TERM.                 
03138                                                                   
03141                                                                   
03144                                                                   
03145      IF BSKPCDL  GREATER THAN ZEROS                               
03146          MOVE BSKPCDI            TO PB-I-SKIP-CODE.               
03147                                                                   
03151                                                                   
03155                                                                   
03158                                                                   
03159      IF  BBEN-LEN      (1)  GREATER THAN ZEROS                    
03160          MOVE WS-BBEN  (1)       TO PB-I-LF-BENEFIT-AMT.          
03161                                                                   
03162      IF  BALT-BEN-LEN     (1)  GREATER THAN ZEROS                 
03163          MOVE WS-BALT-BEN (1)    TO PB-I-LF-ALT-BENEFIT-AMT.      
03164                                                                   
072009     MOVE SPACE TO PB-I-LF-CALC-FLAG.
072009                                                 
03165      IF  BPREM-LEN     (1)  GREATER THAN ZEROS                    
03166          IF WS-BPREM   (1)  NOT = WS-ALL-NINES                    
03167             SUBTRACT PB-I-LF-PREMIUM-AMT                          
03168                      FROM WS-B-LF-ISS-PRM-ENTERED                 
03169             ADD  WS-BPREM (1)    TO WS-B-LF-ISS-PRM-ENTERED       
03170             MOVE WS-BPREM (1)    TO PB-I-LF-PREMIUM-AMT           
03171             MOVE SPACE           TO PB-I-LF-CALC-FLAG             
03172          ELSE                                                     
120513            MOVE WS-BPREM (1)    TO PB-I-LF-PREMIUM-AMT
120513*           MOVE ZEROS           TO PB-I-LF-PREMIUM-AMT           
120513*           MOVE '?'             TO PB-I-LF-CALC-FLAG.            
120513         END-IF
120513     END-IF.
03175                                                                   
03176      IF  BALT-PREM-LEN     (1)  GREATER THAN ZEROS                
03177             SUBTRACT PB-I-LF-ALT-PREMIUM-AMT                      
03178                      FROM WS-B-LF-ISS-PRM-ENTERED                 
03179          ADD  WS-BALT-PREM (1) TO WS-B-LF-ISS-PRM-ENTERED         
03180          MOVE WS-BALT-PREM (1) TO PB-I-LF-ALT-PREMIUM-AMT.        
03181                                                                   
011904     IF  BALT-PREM-LEN     (2)  > ZEROS
011904         MOVE WS-BALT-PREM (2) TO PB-I-TOT-FEES.
011904                                                                  
03182      IF  BBEN-LEN (2)  GREATER THAN ZEROS                         
03183          MOVE WS-BBEN (2)  TO PB-I-AH-BENEFIT-AMT.                
03184                                                                   
03185      IF  BPREM-LEN     (2)  GREATER THAN ZEROS                    
03186          IF WS-BPREM   (2)  NOT = WS-ALL-NINES                    
03187             SUBTRACT PB-I-AH-PREMIUM-AMT                          
03188                      FROM WS-B-AH-ISS-PRM-ENTERED                 
03189             ADD  WS-BPREM (2)    TO WS-B-AH-ISS-PRM-ENTERED       
03190             MOVE WS-BPREM (2)    TO PB-I-AH-PREMIUM-AMT           
03191             MOVE SPACE           TO PB-I-AH-CALC-FLAG             
03192          ELSE                                                     
120513            MOVE WS-BPREM (2)    TO PB-I-AH-PREMIUM-AMT
120513*           MOVE ZEROS           TO PB-I-AH-PREMIUM-AMT           
120513*           MOVE '?'             TO PB-I-AH-CALC-FLAG.            
120513         END-IF
120513     END-IF
03195                                                                   
03196      IF BCRIT-PERD-LEN      (1)   GREATER THAN ZEROS              
03197         MOVE WS-BCRIT-PERD  (1)   TO PB-I-LF-CRIT-PER.            
03198                                                                   
03199      IF BCRIT-PERD-LEN      (2)   GREATER THAN ZEROS              
03200         MOVE WS-BCRIT-PERD  (2)   TO PB-I-AH-CRIT-PER.            
03201                                                                   
03202      IF BINDGRPL            GREATER THAN ZEROS                    
03203          MOVE BINDGRPI           TO PB-I-INDV-GRP-OVRD.           
03204                                                                   
03205      IF BRTCLSL             GREATER THAN ZEROS                    
03206          MOVE BRTCLSI            TO PB-I-RATE-CLASS-OVRD.         
03207                                                                   
03208      IF BSIGL               GREATER THAN ZEROS                    
03209          MOVE BSIGI              TO PB-I-SIG-SW.                  
03210                                                                   
03213                                                                   
03214      IF BMEMBERL            GREATER THAN ZEROS                    
03215          MOVE BMEMBERI       TO PB-I-MEMBER-NO.                   
03216                                                                   
03217      IF BLNOFCRL            GREATER THAN ZEROS                    
03218          MOVE BLNOFCRI           TO PB-I-LOAN-OFFICER.            
03219                                                                   
03220      IF BEXPIRE-LEN    (1)  GREATER THAN ZEROS                    
03221         MOVE WS-CONVERTED-EXPIRDT (1) TO PB-I-LF-EXPIRE-DT.       
03222                                                                   
03223      IF BEXPIRE-LEN    (2)  GREATER THAN ZEROS                    
03224         MOVE WS-CONVERTED-EXPIRDT (2) TO PB-I-AH-EXPIRE-DT.       
03225                                                                   
03232                                                                   
03233      IF BEXPIRE-LEN    (1)  GREATER THAN ZEROS                    
03234         IF WS-CONVERTED-EXPIRDT   (1) GREATER THAN LOW-VALUES     
03235            MOVE '3'              TO PB-I-DATA-ENTRY-SW.           
03236                                                                   
03237      IF BEXPIRE-LEN    (2)  GREATER THAN ZEROS                    
03238         IF WS-CONVERTED-EXPIRDT   (2) GREATER THAN LOW-VALUES     
03239            MOVE '3'              TO PB-I-DATA-ENTRY-SW.           
03240                                                                   
03243                                                                   
03244      IF PB-EXT-DAYS-PROCESSING                                    
03245         IF PB-I-EXTENTION-DAYS = ZEROS                            
03246            MOVE '1'              TO PB-I-DATA-ENTRY-SW.           
03247                                                                   
03248      IF PB-EXPIRE-DT-PROCESSING                                   
03249         IF PB-I-LF-EXPIRE-DT = LOW-VALUES AND                     
03250            PB-I-AH-EXPIRE-DT = LOW-VALUES                         
03251              MOVE '1'            TO PB-I-DATA-ENTRY-SW.           
03252                                                                   
03253      IF PB-1ST-PMT-DT-PROCESSING                                  
03254         IF PB-I-1ST-PMT-DT = LOW-VALUES                           
03255            MOVE '1'              TO PB-I-DATA-ENTRY-SW.           
03256                                                                   
03257      IF BREINL GREATER THAN ZEROS                                 
03258         MOVE BREINI              TO PB-I-SPECIAL-REIN-CODE.       
03259                                                                   
03260      IF BFORCEL NOT = ZEROS                                       
03261         MOVE BFORCEI             TO PB-FORCE-CODE.                

102408     IF (BENTRYL NOT = ZEROS)
102408        AND (PB-COMPANY-ID = 'CID')
102408        AND (PB-STATE = 'VA')
102408        AND (BENTRYI NOT = 'P')
102408        AND (PB-BATCH-ENTRY = 'P')
102408        PERFORM 1800-REMOVE-CERT-NOTES
102408                                 THRU 1800-EXIT
102408     END-IF

03263      IF BENTRYL NOT = ZEROS                                       
03264         MOVE BENTRYI             TO PB-BATCH-ENTRY
102408        IF PI-COMPANY-ID = 'CID'
102408           AND (PB-BATCH-ENTRY = 'P')
102408           AND (PB-STATE = 'VA')
102408           PERFORM 1500-UPDATE-ERNOTE
102408                                 THRU 1500-EXIT
102408        END-IF
102408     END-IF
03265                                                                   
03266      IF BUNWRITL NOT = ZEROS                                      
03267         MOVE BUNWRITI            TO PB-I-UNDERWRITING-STATUS.     
03268                                                                   
03269      IF BBILCDL NOT = ZEROS                                       
03270         MOVE BBILCDI             TO PB-RECORD-BILL.               
03271                                                                   
03274                                                                   
03275                                                                   
03283                                                                   
03284                                                                   
03285 ***************************************************************** 
03286 *        IF BTYPE = ZEROS OR SPACES DELETE COVERAGE             * 
03287 ***************************************************************** 
03288                                                                   
03289      IF BTYPE-LEN (1)   GREATER THAN ZEROS                        
03290         IF BTYPE  (1)   = ZEROS OR SPACES                         
03291            IF REMOVE-LF-COVERAGE                                  
03292                MOVE SPACES           TO PB-I-LF-INPUT-CD          
03293                                         PI-BTYPE (1)              
03294                SUBTRACT PB-I-LF-PREMIUM-AMT FROM                  
03295                                         WS-B-LF-ISS-PRM-ENTERED   
03296                SUBTRACT PB-I-LF-ALT-PREMIUM-AMT FROM              
03297                                         WS-B-LF-ISS-PRM-ENTERED   
03298                SUBTRACT PB-I-LF-PREM-CALC FROM                    
03299                                         WS-B-LF-ISS-PRM-COMPUTED  
03300                MOVE ZEROS            TO PB-I-LF-TERM              
03301                                         PB-I-LF-BENEFIT-AMT       
03302                                         PB-I-LF-PREMIUM-AMT       
03303                                         PB-I-LF-BENEFIT-CD        
03304                                         PB-I-LF-PREM-CALC         
03305                                         PB-I-LF-CRIT-PER          
03306                                         PB-I-LF-ALT-BENEFIT-AMT   
03307                                         PB-I-LF-ALT-PREMIUM-AMT   
03308                MOVE LOW-VALUES       TO PB-I-LF-EXPIRE-DT         
03309            ELSE                                                   
03310                NEXT SENTENCE                                      
03311         ELSE                                                      
03312            MOVE BTYPE (1)        TO PB-I-LF-INPUT-CD              
03313                                     PI-BTYPE (1).                 
03314      IF BTYPE-LEN (2)   GREATER THAN ZEROS                        
03315         IF BTYPE  (2)   = ZEROS OR SPACES                         
03316            IF  REMOVE-AH-COVERAGE                                 
03317                MOVE SPACES           TO PB-I-AH-INPUT-CD          
03318                                         PI-BTYPE (2)              
03319                SUBTRACT PB-I-AH-PREMIUM-AMT FROM                  
03320                                         WS-B-AH-ISS-PRM-ENTERED   
03321                SUBTRACT PB-I-AH-PREM-CALC FROM                    
03322                                         WS-B-AH-ISS-PRM-COMPUTED  
03323                MOVE ZEROS            TO PB-I-AH-TERM              
03324                                         PB-I-AH-BENEFIT-AMT       
03325                                         PB-I-AH-PREMIUM-AMT       
03326                                         PB-I-AH-BENEFIT-CD        
03327                                         PB-I-AH-PREM-CALC         
03328                                         PB-I-AH-CRIT-PER          
03329                MOVE LOW-VALUES       TO PB-I-AH-EXPIRE-DT         
03330            ELSE                                                   
03331                NEXT SENTENCE                                      
03332         ELSE                                                      
03333            MOVE BTYPE (2)        TO PB-I-AH-INPUT-CD              
03334                                     PI-BTYPE (2).                 
03335  1360-REWRITE-PB-RECORD.                                          
03336      MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY.             
03337                                                                   
03338      MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.         
03339      MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT.             
03340                                                                   
03341      MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY.              
03342                                                                   
03343      SUBTRACT PB-I-LF-PREM-CALC  FROM WS-B-LF-ISS-PRM-COMPUTED.   
03344      SUBTRACT PB-I-AH-PREM-CALC  FROM WS-B-AH-ISS-PRM-COMPUTED.   
03345                                                                   
03346      IF BCPREM-LEN (1)  GREATER THAN ZEROS                        
03347          MOVE WS-BCPREM (1)      TO PB-I-LF-PREM-CALC.            
03348                                                                   
03349      IF BCPREM-LEN (2)  GREATER THAN ZEROS                        
03350          MOVE WS-BCPREM (2)      TO PB-I-AH-PREM-CALC.            
03351                                                                   
03352      PERFORM 1398-UPDATE-ERPNDM THRU 1398-EXIT.                   
03353                                                                   
03354      PERFORM 9800-LINK-PENDING-EDIT THRU 9800-EXIT.               
03355                                                                   
03356      PERFORM 7300-FORMAT-ERRORS  THRU 7399-EXIT.                  
03357                                                                   
03358      MOVE PB-SV-CARRIER          TO PI-SV-CARRIER.                
03359      MOVE PB-SV-GROUPING         TO PI-SV-GROUPING.               
03360      MOVE PB-SV-STATE            TO PI-SV-STATE.                  
03361                                                                   
03362      ADD  PB-I-LF-PREM-CALC      TO WS-B-LF-ISS-PRM-COMPUTED.     
03363      ADD  PB-I-AH-PREM-CALC      TO WS-B-AH-ISS-PRM-COMPUTED.     
03364                                                                   
03365  1375-REWRITE-PB-RECORD.                                          
03366      MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
03367      MOVE FILE-ID-ERPNDB         TO JP-FILE-ID.                   
03368      MOVE 'C'                    TO JP-RECORD-TYPE.               
03369      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
03370                                                                   
03371      EXEC CICS REWRITE                                            
03372          DATASET (FILE-ID-ERPNDB)                                 
03373          FROM    (PENDING-BUSINESS)                               
03374      END-EXEC.                                                    
03375                                                                   
03376      PERFORM 8400-LOG-JOURNAL-RECORD.                             

03378 ******************************************************************
03379 *       U P D A T E   T H E   O R I G   C E R T   I N F O        *
03380 ******************************************************************

           MOVE ELCERT-KEY             TO ERMAIL-KEY
           MOVE ' '                    TO WS-ERMAIL-SW

           EXEC CICS READ
              DATASET   (FILE-ID-ERMAIL)
              SET       (ADDRESS OF MAILING-DATA)
              RIDFLD    (ERMAIL-KEY)
              RESP      (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
              SET ERMAIL-FOUND TO TRUE
           END-IF
121712
121712     PERFORM 1295-READ-CERT-TRAILER THRU 1295-EXIT

           MOVE PB-CONTROL-BY-ACCOUNT (1:33)
                                       TO ELCRTO-KEY
           MOVE 'I'                    TO ELCRTO-RECORD-TYPE
           MOVE +0                     TO ELCRTO-SEQ-NO

           EXEC CICS READ
              DATASET   ('ELCRTO')
              INTO      (ORIGINAL-CERTIFICATE)
              RIDFLD    (ELCRTO-KEY)
              GTEQ
              RESP      (WS-RESPONSE)
           END-EXEC
           
      *    display ' just read gteq ' ws-response
           IF RESP-NORMAL
              AND (OC-CONTROL-PRIMARY (1:33) =
                       PB-CONTROL-BY-ACCOUNT (1:33))
              AND (OC-RECORD-TYPE = 'I')
      *       display ' resp norm, key =, type i '
              IF (OC-ENDORSEMENT-PROCESSED-DT = LOW-VALUES)
      *          display ' end proc blank '
                 display ' csr edit session ' PI-CSR-SESSION-SW
                 IF NOT CSR-EDIT-SESSION
                    EXEC CICS READ
                       DATASET   ('ELCRTO')
                       INTO      (ORIGINAL-CERTIFICATE)
                       RIDFLD    (OC-CONTROL-PRIMARY)
                       UPDATE
                       RESP      (WS-RESPONSE)
                    END-EXEC
                    DISPLAY ' JUST DID READ UPD ' WS-RESPONSE
                    IF RESP-NORMAL
                       display ' good read upd '
062712                 MOVE PB-I-INSURED-LAST-NAME TO 
062712                         OC-INS-LAST-NAME   
062712                 MOVE PB-I-INSURED-FIRST-NAME TO 
062712                         OC-INS-FIRST-NAME  
062712                 MOVE PB-I-INSURED-MIDDLE-INIT TO 
062712                         OC-INS-MIDDLE-INIT 
062712                 MOVE PB-I-AGE         TO OC-INS-AGE
062712                 MOVE PB-I-JOINT-LAST-NAME   TO 
062712                         OC-JNT-LAST-NAME
062712                 MOVE PB-I-JOINT-FIRST-NAME  TO 
062712                         OC-JNT-FIRST-NAME  
062712                 MOVE PB-I-JOINT-MIDDLE-INIT TO 
062712                         OC-JNT-MIDDLE-INIT 
062712                 MOVE PB-I-JOINT-AGE   TO OC-JNT-AGE
062712                 MOVE PB-I-LF-BENEFIT-CD TO OC-LF-BENCD
062712                 MOVE PB-I-LF-TERM     TO OC-LF-TERM
062712                 MOVE PB-I-LF-BENEFIT-AMT TO OC-LF-BEN-AMT
062712                 MOVE PB-I-LF-PREMIUM-AMT TO OC-LF-PRM-AMT
062712                 MOVE PB-I-LF-ALT-BENEFIT-AMT TO 
062712                         OC-LF-ALT-BEN-AMT
062712                 MOVE PB-I-LF-ALT-PREMIUM-AMT TO 
062712                         OC-LF-ALT-PRM-AMT  
062712                 MOVE PB-I-LF-EXPIRE-DT TO OC-LF-EXP-DT
062712                 MOVE PB-I-LIFE-COMMISSION TO OC-LF-COMM-PCT
062712                 MOVE PB-I-AH-BENEFIT-CD TO OC-AH-BENCD
062712                 MOVE PB-I-AH-TERM     TO OC-AH-TERM
062712                 MOVE PB-I-AH-BENEFIT-AMT TO OC-AH-BEN-AMT
062712                 MOVE PB-I-AH-PREMIUM-AMT TO OC-AH-PRM-AMT
062712                 MOVE PB-I-AH-EXPIRE-DT TO OC-AH-EXP-DT
062712                 MOVE PB-I-AH-COMMISSION TO OC-AH-COMM-PCT
062712                 MOVE PB-I-AH-CRIT-PER TO OC-AH-CP
062712                 MOVE PB-I-1ST-PMT-DT  TO OC-1ST-PMT-DT
011413                 IF OC-CANCEL-TRAN-IND = 'N'
011413                    MOVE LOW-VALUES    TO OC-LF-CANCEL-DT
011413                    MOVE +0            TO OC-LF-CANCEL-AMT
011413                                          OC-LF-ITD-CANCEL-AMT
011413                    MOVE LOW-VALUES    TO OC-AH-CANCEL-DT
011413                    MOVE +0            TO OC-AH-CANCEL-AMT
011413                                          OC-AH-ITD-CANCEL-AMT
011413                 END-IF
011413                 MOVE 'Y'              TO OC-ISSUE-TRAN-IND

                       MOVE PI-PROCESSOR-ID  TO OC-LAST-MAINT-BY
                       MOVE EIBTIME          TO OC-LAST-MAINT-HHMMSS
                       MOVE WS-CURRENT-BIN-DT TO OC-LAST-MAINT-DT
                       IF ERMAIL-FOUND
                          MOVE MA-CRED-BENE-NAME
                                       TO OC-CRED-BENE-NAME
                       END-IF
121712                 IF NOT CERT-TRL-REC-NOT-FOUND
121712                    MOVE CS-INS-AGE-DEFAULT-FLAG TO 
121712                           OC-INS-AGE-DEFAULT-FLAG
121712                    MOVE CS-JNT-AGE-DEFAULT-FLAG TO 
121712                           OC-JNT-AGE-DEFAULT-FLAG
121712                 END-IF
                       EXEC CICS REWRITE
                          DATASET   ('ELCRTO')
                          FROM      (ORIGINAL-CERTIFICATE)
                          RESP      (WS-RESPONSE)
                       END-EXEC
                       display ' just rewrote ' ws-response
                    END-IF
                 END-IF
              ELSE
                 SUBTRACT +1 FROM OC-KEY-SEQ-NO
                 MOVE 'OC'             TO OC-RECORD-ID
                 MOVE PI-PROCESSOR-ID  TO OC-LAST-MAINT-BY
                 MOVE EIBTIME          TO OC-LAST-MAINT-HHMMSS
                 MOVE WS-CURRENT-BIN-DT TO OC-LAST-MAINT-DT
062712           MOVE PB-I-INSURED-LAST-NAME TO OC-INS-LAST-NAME
062712           MOVE PB-I-INSURED-FIRST-NAME TO OC-INS-FIRST-NAME
062712           MOVE PB-I-INSURED-MIDDLE-INIT TO OC-INS-MIDDLE-INIT
062712           MOVE PB-I-AGE         TO OC-INS-AGE
062712           MOVE PB-I-JOINT-LAST-NAME TO OC-JNT-LAST-NAME
062712           MOVE PB-I-JOINT-FIRST-NAME TO OC-JNT-FIRST-NAME  
062712           MOVE PB-I-JOINT-MIDDLE-INIT TO OC-JNT-MIDDLE-INIT 
062712           MOVE PB-I-JOINT-AGE   TO OC-JNT-AGE
062712           MOVE PB-I-LF-BENEFIT-CD TO OC-LF-BENCD
062712           MOVE PB-I-LF-TERM     TO OC-LF-TERM
062712           MOVE PB-I-LF-BENEFIT-AMT TO OC-LF-BEN-AMT
062712           MOVE PB-I-LF-PREMIUM-AMT TO OC-LF-PRM-AMT
062712           MOVE PB-I-LF-ALT-BENEFIT-AMT TO OC-LF-ALT-BEN-AMT
062712           MOVE PB-I-LF-ALT-PREMIUM-AMT TO OC-LF-ALT-PRM-AMT
062712           MOVE PB-I-LF-EXPIRE-DT TO OC-LF-EXP-DT
062712           MOVE PB-I-LIFE-COMMISSION TO OC-LF-COMM-PCT
062712           MOVE LOW-VALUES       TO OC-LF-CANCEL-DT
062712           MOVE +0               TO OC-LF-CANCEL-AMT
071712                                    OC-LF-ITD-CANCEL-AMT
062712           MOVE PB-I-AH-BENEFIT-CD TO OC-AH-BENCD
062712           MOVE PB-I-AH-TERM     TO OC-AH-TERM
062712           MOVE PB-I-AH-BENEFIT-AMT TO OC-AH-BEN-AMT
062712           MOVE PB-I-AH-PREMIUM-AMT TO OC-AH-PRM-AMT
062712           MOVE PB-I-AH-EXPIRE-DT TO OC-AH-EXP-DT
062712           MOVE PB-I-AH-COMMISSION TO OC-AH-COMM-PCT
062712           MOVE PB-I-AH-CRIT-PER TO OC-AH-CP
062712           MOVE LOW-VALUES       TO OC-AH-CANCEL-DT
062712           MOVE +0               TO OC-AH-CANCEL-AMT
071712                                    OC-AH-ITD-CANCEL-AMT
062712           MOVE PB-I-1ST-PMT-DT  TO OC-1ST-PMT-DT
011413           MOVE 'Y'              TO OC-ISSUE-TRAN-IND
011413           MOVE 'N'              TO OC-CANCEL-TRAN-IND
                 IF ERMAIL-FOUND
                    MOVE MA-CRED-BENE-NAME 
                                       TO OC-CRED-BENE-NAME
                 END-IF
121712           IF NOT CERT-TRL-REC-NOT-FOUND
121712              MOVE CS-INS-AGE-DEFAULT-FLAG TO 
121712                           OC-INS-AGE-DEFAULT-FLAG
121712              MOVE CS-JNT-AGE-DEFAULT-FLAG TO 
121712                           OC-JNT-AGE-DEFAULT-FLAG
121712           END-IF
                 MOVE LOW-VALUES       TO OC-ENDORSEMENT-PROCESSED-DT
                 EXEC CICS WRITE
                    DATASET   ('ELCRTO')
                    FROM      (ORIGINAL-CERTIFICATE)
                    RIDFLD    (OC-CONTROL-PRIMARY)
                    RESP      (WS-RESPONSE)
                 END-EXEC
              END-IF
           ELSE
              MOVE SPACES              TO ORIGINAL-CERTIFICATE
              MOVE 'OC'                TO OC-RECORD-ID
              MOVE PB-CONTROL-BY-ACCOUNT (1:33)
                                       TO OC-CONTROL-PRIMARY (1:33)
              MOVE 'I'                 TO OC-RECORD-TYPE
              MOVE +4096               TO OC-KEY-SEQ-NO
              MOVE PI-PROCESSOR-ID     TO OC-LAST-MAINT-BY
              MOVE EIBTIME             TO OC-LAST-MAINT-HHMMSS
              MOVE WS-CURRENT-BIN-DT   TO OC-LAST-MAINT-DT
062712        MOVE PB-I-INSURED-LAST-NAME TO OC-INS-LAST-NAME
062712        MOVE PB-I-INSURED-FIRST-NAME TO OC-INS-FIRST-NAME
062712        MOVE PB-I-INSURED-MIDDLE-INIT TO OC-INS-MIDDLE-INIT
062712        MOVE PB-I-AGE            TO OC-INS-AGE
062712        MOVE PB-I-JOINT-LAST-NAME TO OC-JNT-LAST-NAME
062712        MOVE PB-I-JOINT-FIRST-NAME TO OC-JNT-FIRST-NAME  
062712        MOVE PB-I-JOINT-MIDDLE-INIT TO OC-JNT-MIDDLE-INIT 
062712        MOVE PB-I-JOINT-AGE      TO OC-JNT-AGE
062712        MOVE PB-I-LF-BENEFIT-CD  TO OC-LF-BENCD
062712        MOVE PB-I-LF-TERM        TO OC-LF-TERM
062712        MOVE PB-I-LF-BENEFIT-AMT TO OC-LF-BEN-AMT
062712        MOVE PB-I-LF-PREMIUM-AMT TO OC-LF-PRM-AMT
062712        MOVE PB-I-LF-ALT-BENEFIT-AMT TO OC-LF-ALT-BEN-AMT
062712        MOVE PB-I-LF-ALT-PREMIUM-AMT TO OC-LF-ALT-PRM-AMT
062712        MOVE PB-I-LF-EXPIRE-DT   TO OC-LF-EXP-DT
062712        MOVE PB-I-LIFE-COMMISSION TO OC-LF-COMM-PCT
062712        MOVE LOW-VALUES          TO OC-LF-CANCEL-DT
062712        MOVE +0                  TO OC-LF-CANCEL-AMT
071712                                    OC-LF-ITD-CANCEL-AMT
062712        MOVE PB-I-AH-BENEFIT-CD  TO OC-AH-BENCD
062712        MOVE PB-I-AH-TERM        TO OC-AH-TERM
062712        MOVE PB-I-AH-BENEFIT-AMT TO OC-AH-BEN-AMT
062712        MOVE PB-I-AH-PREMIUM-AMT TO OC-AH-PRM-AMT
062712        MOVE PB-I-AH-EXPIRE-DT   TO OC-AH-EXP-DT
062712        MOVE PB-I-AH-COMMISSION  TO OC-AH-COMM-PCT
062712        MOVE PB-I-AH-CRIT-PER    TO OC-AH-CP
062712        MOVE LOW-VALUES          TO OC-AH-CANCEL-DT
062712        MOVE +0                  TO OC-AH-CANCEL-AMT
071712                                    OC-AH-ITD-CANCEL-AMT
062712        MOVE PB-I-1ST-PMT-DT     TO OC-1ST-PMT-DT
011413        MOVE 'Y'                 TO OC-ISSUE-TRAN-IND
011413        MOVE 'N'                 TO OC-CANCEL-TRAN-IND
              IF ERMAIL-FOUND
                 MOVE MA-CRED-BENE-NAME 
                                       TO OC-CRED-BENE-NAME
              END-IF
121712        IF NOT CERT-TRL-REC-NOT-FOUND
121712           MOVE CS-INS-AGE-DEFAULT-FLAG TO 
121712                           OC-INS-AGE-DEFAULT-FLAG
121712           MOVE CS-JNT-AGE-DEFAULT-FLAG TO 
121712                           OC-JNT-AGE-DEFAULT-FLAG
121712        END-IF
              MOVE LOW-VALUES          TO OC-ENDORSEMENT-PROCESSED-DT

              EXEC CICS WRITE
                 DATASET   ('ELCRTO')
                 FROM      (ORIGINAL-CERTIFICATE)
                 RIDFLD    (OC-CONTROL-PRIMARY)
                 RESP      (WS-RESPONSE)
              END-EXEC
           END-IF

03378 ******************************************************************
03379 *       U P D A T E   T H E   B A T C H   H E A D E R            *
03380 ******************************************************************
03381                                                                   
03382      MOVE +9999                  TO ERPNDB-BATCH-SEQ-NO.          
03383      MOVE ZEROS                  TO ERPNDB-BATCH-CHG-SEQ-NO.      
03384                                                                   
03385      EXEC CICS READ                                               
03386          INTO    (PENDING-BUSINESS)                               
03387          DATASET (FILE-ID-ERPNDB)                                 
03388          RIDFLD  (ERPNDB-KEY)                                     
03389          UPDATE                                                   
03390      END-EXEC.                                                    
03391                                                                   
03392      MOVE  FILE-ID-ERPNDB        TO JP-FILE-ID.                   
03393      MOVE 'B'                    TO JP-RECORD-TYPE.               
03394      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
03395                                                                   
03396      MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
03397                                                                   
03398      PERFORM 8400-LOG-JOURNAL-RECORD.                             
03399                                                                   
03400      MOVE WS-BATCH-RECORD        TO PB-BATCH-RECORD               
03401                                                                   
03402      MOVE 'C'                    TO JP-RECORD-TYPE.               
03403      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
03404                                                                   
03405      EXEC CICS REWRITE                                            
03406          DATASET    (FILE-ID-ERPNDB)                              
03407          FROM       (PENDING-BUSINESS)                            
03408      END-EXEC.                                                    
03409                                                                   
03410      PERFORM 8400-LOG-JOURNAL-RECORD.                             
03411                                                                   
03412      IF WS-ERRORS-PRESENT                                         
03413         GO TO 3900-RE-DISPLAY-RECORD.                             
03414                                                                   
03415      MOVE ZEROS                  TO EMI-ERROR.                    
03416      MOVE -1                     TO BMAINTL.                      
03417      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
03418      GO TO 3900-RE-DISPLAY-RECORD.                                
03419                                                                   
03420  1380-BATCH-HDR-NOT-FOUND.                                        
03421      MOVE -1                     TO BMAINTL.                      
03422      MOVE ER-7450                TO EMI-ERROR.                    
03423      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
03424      GO TO 8200-SEND-DATAONLY.                                    
03425                                                                   
03426  1385-REC-NOT-FOUND.                                              
03427      MOVE -1                     TO BMAINTL.                      
03428      MOVE ER-7451                TO EMI-ERROR.                    
03429      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
03430      GO TO 8200-SEND-DATAONLY.                                    
03431                                                                   
03432      EJECT                                                        
03433                                                                   






121712
121712 1386-UPDATE-AGE-FLAGS.
121712
121712     EXEC CICS GETMAIN
121712         SET     (ADDRESS OF CERTIFICATE-TRAILERS)
121712         LENGTH  (ELCRTT-RECORD-LENGTH)
121712         INITIMG (GETMAIN-SPACE)                                 
121712     END-EXEC
121712
121712     EXEC CICS HANDLE CONDITION
121712         NOTFND   (1386-NOTFND)
121712     END-EXEC.
121712
121712     MOVE PB-CONTROL-BY-ACCOUNT (1:33) TO ELCRTT-PRIMARY
121712     MOVE 'C'                TO ELCRTT-REC-TYPE
121712
121712     EXEC CICS READ
121712         UPDATE
121712         DATASET  (CRTT-ID)
121712         RIDFLD   (ELCRTT-KEY)
121712         INTO     (CERTIFICATE-TRAILERS)
121712     END-EXEC.
121712
121712     IF WS-INS-AGE-SET = 'Y'
121712        MOVE WS-INS-AGE-DEFAULTED TO CS-INS-AGE-DEFAULT-FLAG
121712     END-IF
121712     IF WS-JNT-AGE-SET = 'Y'
121712        MOVE WS-JNT-AGE-DEFAULTED TO CS-JNT-AGE-DEFAULT-FLAG
121712     END-IF
121712
121712     EXEC CICS REWRITE
121712        DATASET  (CRTT-ID)
121712        FROM     (CERTIFICATE-TRAILERS)
121712     END-EXEC.
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
121712     EXEC CICS WRITE
121712        DATASET  (CRTT-ID)
121712        RIDFLD   (ELCRTT-KEY)
121712        FROM     (CERTIFICATE-TRAILERS)
121712     END-EXEC.
121712
121712 1386-EXIT.                                                       
121712     EXIT.                                                        

       1390-read-elcrtt-update.

           EXEC CICS GETMAIN
               SET     (ADDRESS OF CERTIFICATE-TRAILERS)
               LENGTH  (ELCRTT-RECORD-LENGTH)
               INITIMG (GETMAIN-SPACE)
           END-EXEC

           EXEC CICS READ
               UPDATE
               DATASET  (CRTT-ID)
               RIDFLD   (ELCRTT-KEY)
               INTO     (CERTIFICATE-TRAILERS)
               resp     (ws-response)
           END-EXEC

           .
       1390-exit.
           exit.

       1391-rewrite-elcrtt.

           perform 1393-update-elcrtt  thru 1393-exit
           EXEC CICS REWRITE
              DATASET  (CRTT-ID)
              FROM     (CERTIFICATE-TRAILERS)
              resp     (ws-response)
           END-EXEC

           .
       1391-exit.
           exit.

       1392-write-elcrtt.

           move 'CS'                   to certificate-trailers
           move elcrtt-key             to cs-control-primary
           move 'C'                    to cs-trailer-type
           perform 1393-update-elcrtt  thru 1393-exit

           EXEC CICS WRITE
              DATASET  (CRTT-ID)
              ridfld   (elcrtt-key)
              FROM     (CERTIFICATE-TRAILERS)
              resp     (ws-response)
           END-EXEC

           .
       1392-exit.
           exit.

       1393-update-elcrtt.

           if byearl <> zeros
              move byeari              to cs-year
           end-if
           if bmakel <> zeros
              move bmakei              to cs-make
           end-if
           if bmodell <> zeros
              move bmodeli             to cs-model
           end-if

           if bometerl <> zeros
              MOVE Bometeri            TO DEEDIT-FIELD
              PERFORM 8600-DEEDIT                                    
              IF DEEDIT-FIELD-V0 NUMERIC                             
                 MOVE DEEDIT-FIELD-V0  TO cs-vehicle-odometer
              end-if
           end-if

           .
       1393-exit.
           exit.

03434  1398-UPDATE-ERPNDM.                                              
03435                                                                   
03436      EXEC CICS HANDLE CONDITION                                   
03437          NOTFND (1398-EXIT)                                       
03438      END-EXEC.                                                    
03439                                                                   
03440      MOVE PB-CONTROL-PRIMARY     TO  ERPNDM-KEY.                  
03441                                                                   
03442      EXEC CICS READ                                               
03443          SET     (ADDRESS OF PENDING-MAILING-DATA)                
03444          DATASET (FILE-ID-ERPNDM)                                 
03445          RIDFLD  (ERPNDM-KEY)                                     
03446          UPDATE                                                   
03447      END-EXEC.                                                    
03448                                                                   
03449      MOVE PI-PROCESSOR-ID        TO PM-LAST-MAINT-BY.             
03450      MOVE EIBTIME                TO PM-LAST-MAINT-HHMMSS.         
03451      MOVE WS-CURRENT-BIN-DT      TO PM-LAST-MAINT-DT.             
03452                                                                   
03453      IF BLSTNML GREATER THAN ZEROS                                
03454          MOVE BLSTNMI            TO PM-INSURED-LAST-NAME.         
03455                                                                   
03456      IF B1STNML GREATER  THAN ZEROS                               
03457          MOVE B1STNMI            TO PM-INSURED-FIRST-NAME.        
03458                                                                   
03459      IF BINITL GREATER THAN ZEROS                                 
03460          MOVE BINITI             TO PM-INSURED-MIDDLE-INIT.       
03461                                                                   
03462      IF BAGEL GREATER THAN ZEROS                                  
03463          MOVE WS-BAGE            TO PM-INSURED-ISSUE-AGE.         
03464                                                                   
03465      IF BBIRTHL GREATER THAN ZEROS                                
03466          MOVE  WS-CONVERTED-BIRTH TO PM-INSURED-BIRTH-DT.         
03467                                                                   
03468      IF BSEXL GREATER THAN ZEROS                                  
03469          MOVE  BSEXI             TO PM-INSURED-SEX.               
03470                                                                   
03471      EXEC CICS REWRITE                                            
03472          DATASET (FILE-ID-ERPNDM)                                 
03473          FROM    (PENDING-MAILING-DATA)                           
03474      END-EXEC.                                                    
03475                                                                   
03476  1398-EXIT.                                                       
03477      EXIT.                                                        
03478                                                                   
03479 ******************************************************************
03480 *                                                                *
03481 *          D E L E T E   I S S U E   R O U T I N E               *
03482 *                                                                *
03483 ******************************************************************
03484                                                                   
03485  1400-DELETE-ISSUE-ROUTINE.                                       
03486      MOVE 'D'                    TO PI-MAINT-FUNCTION.            
03487                                                                   
03488      IF BCHGL NOT = ZEROS                                         
03489         MOVE ER-2196             TO EMI-ERROR                     
03490         MOVE AL-UABON            TO BMAINTA                       
03491         MOVE -1                  TO BMAINTL                       
03492         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
03493         GO TO 8200-SEND-DATAONLY.                                 
03494                                                                   
03495      EXEC CICS HANDLE CONDITION                                   
03496           NOTFND    (1480-BATCH-HDR-NOT-FOUND)                    
03497      END-EXEC.                                                    
03498                                                                   
03499      MOVE PI-PREV-CONTROL-PRIMARY TO ERPNDB-KEY.                  
03500      MOVE +9999                  TO ERPNDB-BATCH-SEQ-NO.          
03501      MOVE ZEROS                  TO ERPNDB-BATCH-CHG-SEQ-NO.      
03502                                                                   
03503      EXEC CICS READ                                               
03504          SET     (ADDRESS OF PENDING-BUSINESS)                    
03505          DATASET (FILE-ID-ERPNDB)                                 
03506          RIDFLD  (ERPNDB-KEY)                                     
03507      END-EXEC.                                                    
03508                                                                   
03509      MOVE PB-BATCH-RECORD        TO WS-BATCH-RECORD.              
03510                                                                   
03511      MOVE BCERTNOI               TO WS-NEW-PRIME.                 
03512                                                                   
03513      IF BSUFIXL NOT = ZEROS                                       
03514         MOVE BSUFIXI             TO WS-NEW-SUFFIX                 
03515      ELSE                                                         
03516         MOVE SPACE               TO WS-NEW-SUFFIX.                
03517                                                                   
03518      MOVE BEFFDTI                TO DEEDIT-FIELD.                 
03519      PERFORM 8600-DEEDIT.                                         
03520      MOVE DEEDIT-FIELD-V0        TO DC-GREG-DATE-1-MDY.           
03521      MOVE '4'                    TO DC-OPTION-CODE.               
03522      PERFORM 9700-DATE-LINK.                                      
03523      IF DATE-CONVERSION-ERROR                                     
03524         MOVE ER-0348             TO EMI-ERROR                     
03525         MOVE -1                  TO BEFFDTL                       
03526         MOVE AL-UABON            TO BEFFDTA                       
03527         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
03528         GO TO 8200-SEND-DATAONLY                                  
03529      ELSE                                                         
03530         MOVE DC-GREG-DATE-1-EDIT TO BEFFDTI                       
03531         MOVE AL-UANON            TO BEFFDTA                       
03532         MOVE DC-BIN-DATE-1       TO WS-NEW-CERT-EFF-DT.           
03533                                                                   
03534      EXEC CICS HANDLE CONDITION                                   
03535           NOTFND    (1490-RECORD-NOT-FOUND)                       
03536      END-EXEC.                                                    
03537                                                                   
03538      EXEC CICS READ                                               
03539          SET     (ADDRESS OF PENDING-BUSINESS)                    
03540          DATASET (FILE-ID-ERPNDB)                                 
03541          RIDFLD  (PI-PREV-CONTROL-PRIMARY)                        
03542          UPDATE                                                   
03543      END-EXEC.                                                    
03544                                                                   
03545      IF PB-CREDIT-ACCEPT-DT NOT = LOW-VALUES                      
03546         MOVE -1                 TO BMAINTL                        
03547         MOVE ER-2197            TO EMI-ERROR                      
03548         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
03549         GO TO 8200-SEND-DATAONLY.                                 
03550                                                                   
03551      IF PB-BILLED-DT NOT = LOW-VALUES                             
03552         MOVE -1                 TO BMAINTL                        
03553         MOVE ER-2129            TO EMI-ERROR                      
03554         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
03555         GO TO 8200-SEND-DATAONLY.                                 
03556                                                                   
03557      IF PB-CHG-COUNT NOT = ZEROS                                  
03558         MOVE ER-2336            TO EMI-ERROR                      
03559         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
03560         MOVE AL-UABON            TO BMAINTA                       
03561         MOVE -1                  TO BMAINTL                       
03562         GO TO 8200-SEND-DATAONLY.                                 
03563                                                                   
03564      IF PB-BATCH-CHG-SEQ-NO NOT = ZEROS OR                        
03565         PB-CHG-COUNT NOT = ZEROS                                  
03566         MOVE ER-2196             TO EMI-ERROR                     
03567         MOVE AL-UABON            TO BMAINTA                       
03568         MOVE -1                  TO BMAINTL                       
03569         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
03570         GO TO 8200-SEND-DATAONLY.                                 

      ******************************************************************
      *                                                                *
      *       DELETE ORIGINAL CERTIFICATE INFO                         *
      *                                                                *
      ******************************************************************

           MOVE PI-PREV-ALT-KEY (1:33) TO ELCRTO-KEY (1:33)
           MOVE 'I'                    TO ELCRTO-RECORD-TYPE
           MOVE +0                     TO ELCRTO-SEQ-NO

           EXEC CICS READ
              DATASET   ('ELCRTO')
              INTO      (ORIGINAL-CERTIFICATE)
              RIDFLD    (ELCRTO-KEY)
              GTEQ
              RESP      (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
              AND (OC-CONTROL-PRIMARY (1:33) =
                       PB-CONTROL-BY-ACCOUNT (1:33))
              AND (OC-RECORD-TYPE = 'I')
              EXEC CICS READ
                 DATASET   ('ELCRTO')
                 INTO      (ORIGINAL-CERTIFICATE)
                 RIDFLD    (ELCRTO-KEY)
                 UPDATE
                 RESP      (WS-RESPONSE)
              END-EXEC
              IF RESP-NORMAL
                 EXEC CICS DELETE
                    DATASET   ('ELCRTO')
                 END-EXEC
              END-IF
           END-IF

03572 ******************************************************************
03573 *                                                                *
03574 *       DELETE PENDING MAILING ADDRESS RECORDS                   *
03575 *                                                                *
03576 ******************************************************************

03578      EXEC CICS HANDLE CONDITION                                   
03579          NOTFND    (1405-DELETE-CERTIFICATE)                      
03580      END-EXEC.                                                    
03581                                                                   
03582      EXEC CICS READ                                               
03583          EQUAL                                                    
03584          DATASET   (FILE-ID-ERPNDM)                               
03585          SET       (ADDRESS OF PENDING-MAILING-DATA)              
03586          RIDFLD    (PI-PREV-CONTROL-PRIMARY)                      
03587          UPDATE                                                   
03588      END-EXEC.                                                    
03589                                                                   
03590      MOVE FILE-ID-ERPNDM         TO  JP-FILE-ID.                  
03591      MOVE 'D'                    TO  JP-RECORD-TYPE.              
03592      MOVE PENDING-MAILING-DATA   TO  JP-RECORD-AREA.              
03593                                                                   
03594      MOVE ERPNDM-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
03595                                                                   
03596      EXEC CICS DELETE                                             
03597          DATASET   (FILE-ID-ERPNDM)                               
03598      END-EXEC.                                                    
03599                                                                   
03600 ******************************************************************
03601 *                PROCESS   CERTIFICATE                           *
03602 ******************************************************************
03603                                                                   
03604  1405-DELETE-CERTIFICATE.                                         
03605      EXEC CICS HANDLE CONDITION                                   
03606           NOTFND  (1420-CONTINUE-DELETE)                          
03607      END-EXEC.                                                    
03608                                                                   
03609      MOVE PB-CONTROL-BY-ACCOUNT  TO ELCERT-KEY.                   
03610      MOVE PB-SV-CARRIER          TO ELCERT-CARRIER.               
03611      MOVE PB-SV-GROUPING         TO ELCERT-GROUPING.              
03612      MOVE PB-SV-STATE            TO ELCERT-STATE.                 
03613                                                                   
03614      EXEC CICS READ                                               
03615          SET     (ADDRESS OF CERTIFICATE-MASTER)                  
03616          DATASET (FILE-ID-ELCERT)                                 
03617          RIDFLD  (ELCERT-KEY)                                     
03618          UPDATE                                                   
03619      END-EXEC.                                                    
03620                                                                   
03621      MOVE ELCERT-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
03622                                                                   
03623      MOVE FILE-ID-ELCERT         TO JP-FILE-ID.                   
03624      MOVE 'B'                    TO JP-RECORD-TYPE                
03625      MOVE CERTIFICATE-MASTER     TO JP-RECORD-AREA.               
03626                                                                   
03627      PERFORM 8400-LOG-JOURNAL-RECORD.                             
03628                                                                   
03629      IF CERT-NOTES-ARE-NOT-PRESENT                                
03630         MOVE ' '                 TO WS-CERT-NOTE-SW               
03631      ELSE                                                         
03632         MOVE 'Y'                 TO WS-CERT-NOTE-SW.              
03633                                                                   
03634      IF INSURED-ADDR-PRESENT                                      
03635         MOVE 'Y'                 TO WS-CERT-ADDRESS-SW.           
03636                                                                   
03637 ******************************************************************
03638 ******* THIS CODE SHOULD BYPASS SETTING THE CLAIM-INTERFACE-SW  **
03639 ******* IF THE CERTIFICATE HAS ALREADY GONE THROUGH A MONTH END **
03640 ******************************************************************
03641 ******************************************************************
03642                                                                   
03643      IF CERT-ADDED-BATCH AND                                      
03644         (NOT NO-CLAIM-ATTACHED                                    
03645          OR                                                       
03646          CM-CLAIM-ATTACHED-COUNT GREATER THAN +0)                 
03647         GO TO 1410-REWRITE-CERT.                                  
03648                                                                   
03649 ******************************************************************
03650 ******************************************************************
03651                                                                   
03652      IF CERT-ADDED-BATCH  AND                                     
03653         (NO-CLAIM-ATTACHED OR CM-CLAIM-ATTACHED-COUNT = ZEROS)    
03654         GO TO 1410-REWRITE-CERT.                                  
03655                                                                   
03656      IF NO-CLAIM-ATTACHED                                         
03657         NEXT SENTENCE                                             
03658      ELSE                                                         
03659         MOVE SPACE                  TO CM-CREDIT-INTERFACE-SW-1   
03660         MOVE '2'                    TO CM-CLAIM-INTERFACE-SW      
03661         GO TO 1410-REWRITE-CERT.                                  
03662                                                                   
03663      MOVE  FILE-ID-ELCERT          TO JP-FILE-ID.                 
03664      MOVE 'D'                      TO JP-RECORD-TYPE.             
03665      MOVE CERTIFICATE-MASTER       TO JP-RECORD-AREA.             
03666                                                                   
03667      EXEC CICS DELETE                                             
03668           DATASET    (FILE-ID-ELCERT)                             
03669      END-EXEC.                                                    
03670                                                                   
03671      PERFORM 8400-LOG-JOURNAL-RECORD.                             
03672                                                                   
03673 ******************************************************************
03674 *                                                                *
03675 *       DELETE CERTIFICATE NOTES FOR ALL DELETED CERTIFICATES    *
03676 *                                                                *
03677 ******************************************************************
03678                                                                   
03679      IF  CERT-NOTES-ARE-PRESENT                                   
03680          NEXT SENTENCE                                            
03681      ELSE                                                         
03682          GO TO 1407-DELETE-MAILING.                               
03683                                                                  
111109     EXEC CICS HANDLE CONDITION                                   
111109          NOTFND  (1406-DELETE-ERNOTE)                           
111109     END-EXEC.                                                    
111109                         
111109     MOVE ELCERT-KEY            TO ERCNOT-KEY.
111109     MOVE '0'                   TO ERCNOT-REC-TYPE.
111109     MOVE 0                     TO ERCNOT-SEQUENCE.
111109  
111109 1406-DELETE-ERCNOT-LOOP.
111109
111109     EXEC CICS READ                                               
111109         GTEQ                                                    
111109         DATASET   (FILE-ID-ERCNOT)                               
111109         SET       (ADDRESS OF CERT-NOTE-FILE)                  
111109         RIDFLD    (ERCNOT-KEY)                                   
111109     END-EXEC.                                                    
111109                             
111109     MOVE CZ-CONTROL-PRIMARY      TO ERCNOT-KEY
111109     IF ERCNOT-PART-KEY NOT EQUAL ELCERT-KEY
111109         GO TO 1406-DELETE-ERNOTE
111109     END-IF.
111109                                     
111109     MOVE FILE-ID-ERCNOT         TO  JP-FILE-ID.                  
111109     MOVE 'D'                    TO  JP-RECORD-TYPE.              
111109     MOVE CERT-NOTE-FILE         TO  JP-RECORD-AREA.              
111109                                                                  
111109     MOVE ERCNOT-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
111109                                                                  
111109     EXEC CICS DELETE                                             
111109         DATASET   (FILE-ID-ERCNOT) 
111109         RIDFLD    (ERCNOT-KEY)                              
111109         END-EXEC.                                                
111109                                                                  
111109     PERFORM 8400-LOG-JOURNAL-RECORD.  
111109
111109     GO TO 1406-DELETE-ERCNOT-LOOP.
111109
111109 1406-DELETE-ERNOTE.
111109
03684      EXEC CICS HANDLE CONDITION                                   
03685           NOTFND  (1407-DELETE-MAILING)                           
03686      END-EXEC.                                                    
03687                                                                   
03688      EXEC CICS READ                                               
03689          EQUAL                                                    
03690          DATASET   (FILE-ID-ERNOTE)                               
03691          SET       (ADDRESS OF CERTIFICATE-NOTE)                  
03692          RIDFLD    (ELCERT-KEY)                                   
03693          UPDATE                                                   
03694      END-EXEC.                                                    
03695                                                                   
03696      MOVE FILE-ID-ERNOTE         TO  JP-FILE-ID.                  
03697      MOVE 'D'                    TO  JP-RECORD-TYPE.              
03698      MOVE CERTIFICATE-NOTE       TO  JP-RECORD-AREA.              
03699                                                                   
03700      MOVE ERNOTE-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
03701                                                                   
03702      EXEC CICS DELETE                                             
03703          DATASET   (FILE-ID-ERNOTE)                               
03704          END-EXEC.                                                
03705                                                                   
03706      PERFORM 8400-LOG-JOURNAL-RECORD.                             
03707                                                                   
03708 ******************************************************************
03709 *                                                                *
03710 *                DELETE MAILING ADDRESS RECORDS                  *
03711 *                                                                *
03712 ******************************************************************
03713                                                                   
03714  1407-DELETE-MAILING.                                             
03715      IF CERT-ADDRESS-PRESENT                                      
03716         NEXT SENTENCE                                             
03717      ELSE                                                         
03718         GO TO 1420-CONTINUE-DELETE.                               
03719                                                                   
03720      EXEC CICS HANDLE CONDITION                                   
03721           NOTFND  (1420-CONTINUE-DELETE)                          
03722      END-EXEC.                                                    
03723                                                                   
03724      EXEC CICS READ                                               
03725          EQUAL                                                    
03726          DATASET   (FILE-ID-ERMAIL)                               
03727          SET       (ADDRESS OF MAILING-DATA)                      
03728          RIDFLD    (ELCERT-KEY)                                   
03729          UPDATE                                                   
03730      END-EXEC.                                                    
03731                                                                   
03732      MOVE FILE-ID-ERMAIL         TO  JP-FILE-ID.                  
03733      MOVE 'D'                    TO  JP-RECORD-TYPE.              
03734      MOVE MAILING-DATA           TO  JP-RECORD-AREA.              
03735                                                                   
03736      MOVE ERMAIL-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
03737                                                                   
03738      EXEC CICS DELETE                                             
03739          DATASET   (FILE-ID-ERMAIL)                               
03740      END-EXEC.                                                    
03741                                                                   
03742      GO TO 1420-CONTINUE-DELETE.                                  
03743                                                                   
03744  1410-REWRITE-CERT.                                               
03745      MOVE  FILE-ID-ELCERT          TO JP-FILE-ID.                 
03746      MOVE 'C'                      TO JP-RECORD-TYPE.             
03747      MOVE CERTIFICATE-MASTER       TO JP-RECORD-AREA.             
03748      MOVE ELCERT-JOURNAL-LENGTH    TO WS-JOURNAL-RECORD-LENGTH.   
03749                                                                   
03750      EXEC CICS REWRITE                                            
03751           DATASET    (FILE-ID-ELCERT)                             
03752           FROM       (CERTIFICATE-MASTER)                         
03753           END-EXEC.                                               
03754                                                                   
03755      PERFORM 8400-LOG-JOURNAL-RECORD.                             
03756                                                                   
03757  1420-CONTINUE-DELETE.                                            
03758      SUBTRACT PB-I-LF-PREMIUM-AMT FROM WS-B-LF-ISS-PRM-ENTERED.   
03759      SUBTRACT PB-I-LF-PREM-CALC   FROM WS-B-LF-ISS-PRM-COMPUTED.  
03760                                                                   
03761      SUBTRACT PB-I-AH-PREMIUM-AMT FROM WS-B-AH-ISS-PRM-ENTERED.   
03762      SUBTRACT PB-I-AH-PREM-CALC   FROM WS-B-AH-ISS-PRM-COMPUTED.  
03763                                                                   
03764      SUBTRACT +1 FROM WS-B-ISSUE-CNT-ENTERED.                     
03765                                                                   
03766  1450-DELETE-PB-RECORD.                                           
03767      MOVE FILE-ID-ERPNDB           TO JP-FILE-ID                  
03768      MOVE 'D'                      TO JP-RECORD-TYPE              
03769      MOVE PENDING-BUSINESS         TO JP-RECORD-AREA.             
03770                                                                   
03771      EXEC CICS DELETE                                             
03772          DATASET    (FILE-ID-ERPNDB)                              
03773      END-EXEC.                                                    
03774                                                                   
03775      MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
03776                                                                   
03777      PERFORM 8400-LOG-JOURNAL-RECORD.                             
03778                                                                   
03779      MOVE PI-PREV-CONTROL-PRIMARY TO ERPNDB-KEY.                  
03780      MOVE +9999                  TO ERPNDB-BATCH-SEQ-NO.          
03781      MOVE ZEROS                  TO ERPNDB-BATCH-CHG-SEQ-NO.      
03782                                                                   
03783      EXEC CICS HANDLE CONDITION                                   
03784           NOTFND    (1480-BATCH-HDR-NOT-FOUND)                    
03785      END-EXEC.                                                    
03786                                                                   
03787      EXEC CICS READ                                               
03788          SET     (ADDRESS OF PENDING-BUSINESS)                    
03789          DATASET (FILE-ID-ERPNDB)                                 
03790          RIDFLD  (ERPNDB-KEY)                                     
03791          UPDATE                                                   
03792      END-EXEC.                                                    
03793                                                                   
03794      MOVE ERPNDB-JOURNAL-LENGTH    TO WS-JOURNAL-RECORD-LENGTH.   
03795                                                                   
03796      MOVE  FILE-ID-ERPNDB          TO JP-FILE-ID.                 
03797                                                                   
03798      IF WS-B-ISSUE-CNT-ENTERED  = ZEROS AND                       
03799         WS-B-CANCEL-CNT-ENTERED = ZEROS                           
03800         MOVE 'D'                   TO JP-RECORD-TYPE              
03801         MOVE PENDING-BUSINESS      TO JP-RECORD-AREA              
03802         EXEC CICS DELETE                                          
03803              DATASET    (FILE-ID-ERPNDB)                          
03804         END-EXEC                                                  
03805         PERFORM 8400-LOG-JOURNAL-RECORD                           
03806      ELSE                                                         
03807         MOVE 'B'                 TO JP-RECORD-TYPE                
03808         MOVE PENDING-BUSINESS    TO JP-RECORD-AREA                
03809         PERFORM 8400-LOG-JOURNAL-RECORD                           
03810         MOVE WS-BATCH-RECORD     TO PB-BATCH-RECORD               
03811         MOVE 'C'                 TO JP-RECORD-TYPE                
03812         MOVE PENDING-BUSINESS    TO JP-RECORD-AREA                
03813         EXEC CICS REWRITE                                         
03814              DATASET    (FILE-ID-ERPNDB)                          
03815              FROM       (PENDING-BUSINESS)                        
03816         END-EXEC                                                  
03817         PERFORM 8400-LOG-JOURNAL-RECORD.                          
03818                                                                   
03819 ******************************************************************
03820 *                                                                *
03821 *       DELETE THE A/R REQUEST RECORD IF THE COMPANY USES        *
03822 *       THE ACCOUNTS RECEIVABLE SYSTEM.                          *
03823 *                                                                *
03824 ******************************************************************
03825                                                                   
03826      IF NOT PI-AR-PROCESSING                                      
03827         GO TO 1475-DELETE-FINISHED.                               
03828                                                                   
03829      IF WS-B-ISSUE-CNT-ENTERED  = ZEROS AND                       
03830         WS-B-CANCEL-CNT-ENTERED = ZEROS                           
03831         NEXT SENTENCE                                             
03832      ELSE                                                         
03833         GO TO 1475-DELETE-FINISHED.                               
03834                                                                   
03835      EXEC CICS HANDLE CONDITION                                   
03836           NOTFND    (1475-DELETE-FINISHED)                        
03837           END-EXEC.                                               
03838                                                                   
03839      MOVE ERPNDB-COMPANY-CD      TO ERRQST-COMPANY-CD.            
03840      MOVE ERPNDB-ENTRY-BATCH     TO ERRQST-ENTRY-BATCH.           
03841                                                                   
03842      EXEC CICS READ                                               
03843          SET     (ADDRESS OF AR-REQUEST-RECORD)                   
03844          DATASET (FILE-ID-ERRQST)                                 
03845          RIDFLD  (ERRQST-KEY)                                     
03846          UPDATE                                                   
03847      END-EXEC.                                                    
03848                                                                   
03849      MOVE ERRQST-JOURNAL-LENGTH    TO WS-JOURNAL-RECORD-LENGTH.   
03850                                                                   
03851      MOVE  FILE-ID-ERRQST          TO JP-FILE-ID.                 
03852                                                                   
03853      MOVE 'D'                      TO JP-RECORD-TYPE.             
03854      MOVE AR-REQUEST-RECORD        TO JP-RECORD-AREA.             
03855                                                                   
03856      EXEC CICS DELETE                                             
03857           DATASET    (FILE-ID-ERRQST)                             
03858      END-EXEC.                                                    
03859                                                                   
03860      PERFORM 8400-LOG-JOURNAL-RECORD.                             
03861                                                                   
03862  1475-DELETE-FINISHED.                                            
03863      MOVE ZEROS                  TO EMI-ERROR.                    
03864      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
03865      MOVE LOW-VALUES             TO VP631BI.                      
03866      MOVE -1                     TO BMAINTL.                      
03867      GO TO 8100-SEND-INITIAL-MAP.                                 
03868                                                                   
03869  1480-BATCH-HDR-NOT-FOUND.                                        
03870      MOVE -1                     TO BMAINTL.                      
03871      MOVE ER-7450                TO EMI-ERROR.                    
03872      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
03873      GO TO 8200-SEND-DATAONLY.                                    
03874                                                                   
03875  1490-RECORD-NOT-FOUND.                                           
03876      MOVE -1                     TO BMAINTL.                      
03877      MOVE ER-7451                TO EMI-ERROR.                    
03878      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
03879      GO TO 8200-SEND-DATAONLY.                                    
03880                                                                   
03883 ******************************************************************
03884 *            O V E R   C H A R G E   P R O C E S S I N G         *
03885 ******************************************************************
       1500-UPDATE-ERNOTE.

           PERFORM 1510-ADD-ERNOTE-REC THRU 1510-EXIT
111109     MOVE 'A'                    TO WS-NOTE-SW
           PERFORM 1520-UPDATE-ELCERT  THRU 1520-EXIT

           .
       1500-EXIT.
           EXIT.

       1510-ADD-ERNOTE-REC.

           EXEC CICS GETMAIN
               SET     (ADDRESS OF CERTIFICATE-NOTE)
               LENGTH  (ERNOTE-RECORD-LENGTH)
               INITIMG (GETMAIN-SPACE)                                 
           END-EXEC

           MOVE PB-CONTROL-BY-ACCOUNT (1:33)
                                       TO ERNOTE-KEY

           EXEC CICS READ
              DATASET    (FILE-ID-ERNOTE)
              RIDFLD     (ERNOTE-KEY)
              INTO       (CERTIFICATE-NOTE)
              RESP       (WS-RESPONSE)
              UPDATE
           END-EXEC

           IF RESP-NORMAL
111109        IF CN-BILLING-START-LINE-NO NOT NUMERIC
111109           MOVE ZEROS            TO CN-BILLING-START-LINE-NO
111109        END-IF
111109        IF CN-BILLING-END-LINE-NO NOT NUMERIC
111109           MOVE ZEROS            TO CN-BILLING-END-LINE-NO
111109        END-IF
111109        PERFORM VARYING N1 FROM +1 BY +1 UNTIL
111109           (N1 > +10)
CIDMOD           OR (CN-LINE (N1) (1:10) = 'OVERCHARGE')
102408           OR (CN-LINE (N1) (1:20) = 'MAXIMUM AGE EXCEEDED')
              END-PERFORM
              IF (CN-LINE (N1) (1:10) = 'OVERCHARGE')
102408          OR (CN-LINE (N1) (1:20) = 'MAXIMUM AGE EXCEEDED')
CIDMOD           EXEC CICS UNLOCK
CIDMOD             DATASET    (FILE-ID-ERNOTE)
CIDMOD           END-EXEC
              ELSE
111109          PERFORM VARYING N1 FROM +1 BY +1 
111109           UNTIL (N1 > +10)
111109           OR (CN-LINE (N1) = SPACES OR LOW-VALUES)
111109          END-PERFORM
111109          IF (N1 < +11) 
111109            IF N1 >= CN-BILLING-START-LINE-NO AND
111109              N1 <= CN-BILLING-END-LINE-NO
111109              IF PI-COMPANY-ID = 'DCC' or 'VPP'
111109                 MOVE WS-OC-MESSAGE TO CN-LINE (N1)
111109              ELSE
111109                 MOVE WS-VA-NO-COMM-MESS
111109                                    TO CN-LINE (N1)
111109              END-IF
111109            ELSE
111109              IF (CN-BILLING-END-LINE-NO NOT = ZEROS) AND
111109               (N1 = (CN-BILLING-END-LINE-NO + +1))
111109                IF PI-COMPANY-ID = 'DCC' or 'VPP'
111109                  MOVE WS-OC-MESSAGE TO CN-LINE (N1)
111109                ELSE
111109                  MOVE WS-VA-NO-COMM-MESS
111109                                    TO CN-LINE (N1)
111109                END-IF
111109                MOVE N1             TO CN-BILLING-END-LINE-NO
111109              ELSE
111109                IF (CN-BILLING-END-LINE-NO = ZEROS)
111109                  IF PI-COMPANY-ID = 'DCC' or 'VPP'
111109                     MOVE WS-OC-MESSAGE TO CN-LINE (N1)
111109                  ELSE
111109                     MOVE WS-VA-NO-COMM-MESS
111109                                    TO CN-LINE (N1)
111109                  END-IF
111109                  MOVE N1            TO CN-BILLING-END-LINE-NO
111109                                      CN-BILLING-START-LINE-NO
111109                ELSE
111109                  IF (CN-BILLING-START-LINE-NO NOT = ZEROS) AND
111109                    (N1 = (CN-BILLING-START-LINE-NO - +1))
111109                     IF PI-COMPANY-ID = 'DCC' or 'VPP'
111109                       MOVE WS-OC-MESSAGE TO CN-LINE (N1)
111109                     ELSE
111109                       MOVE WS-VA-NO-COMM-MESS
111109                                    TO CN-LINE (N1)
111109                     END-IF
111109                     MOVE N1         TO CN-BILLING-START-LINE-NO
111109                  ELSE
111109                     PERFORM 1700-SQUEEZE-IT-IN
111109                                 THRU 1700-EXIT
111109                  END-IF
111109                END-IF
111109              END-IF
111109            END-IF
111109            MOVE 'E631'              TO CN-LAST-MAINT-USER
111109            MOVE WS-CURRENT-BIN-DT   TO CN-LAST-MAINT-DT
111109            MOVE EIBTIME             TO CN-LAST-MAINT-HHMMSS
111109            EXEC CICS REWRITE
111109               DATASET    (FILE-ID-ERNOTE)
111109               FROM       (CERTIFICATE-NOTE)
111109               RESP       (WS-RESPONSE)
111109            END-EXEC
111109          END-IF
111109        END-IF
           ELSE
              MOVE SPACES              TO CERTIFICATE-NOTE
              MOVE 'CN'                TO CN-RECORD-ID
              MOVE PB-CONTROL-BY-ACCOUNT (1:33)
                                       TO CN-CONTROL-PRIMARY
                                          ERNOTE-KEY
              MOVE 01                  TO CN-BILLING-START-LINE-NO
                                          CN-BILLING-END-LINE-NO
102408        IF PI-COMPANY-ID = 'DCC' or 'VPP'
                 MOVE WS-OC-MESSAGE    TO CN-LINE (1)
102408        ELSE
102408           MOVE WS-VA-NO-COMM-MESS
102408                                 TO CN-LINE (1)
102408        END-IF
              MOVE 'E631'              TO CN-LAST-MAINT-USER
              MOVE WS-CURRENT-BIN-DT   TO CN-LAST-MAINT-DT
              MOVE EIBTIME             TO CN-LAST-MAINT-HHMMSS
              EXEC CICS WRITE
                 DATASET    (FILE-ID-ERNOTE)
                 FROM       (CERTIFICATE-NOTE)
                 RIDFLD     (ERNOTE-KEY)
                 RESP       (WS-RESPONSE)
              END-EXEC
           END-IF

           .
       1510-EXIT.
           EXIT.

       1520-UPDATE-ELCERT.

           MOVE PB-CONTROL-BY-ACCOUNT (1:33)
                                       TO ELCERT-KEY
           EXEC CICS READ
               SET     (ADDRESS OF CERTIFICATE-MASTER)
               DATASET (FILE-ID-ELCERT)
               RIDFLD  (ELCERT-KEY)
               UPDATE
               RESP    (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
111109        IF WS-NOTE-SW = 'A'
111109           IF CM-NOTE-SW = ' '
111109              MOVE '2'             TO CM-NOTE-SW
111109           ELSE
111109             IF CM-NOTE-SW = '1'
111109                MOVE '3'           TO CM-NOTE-SW
111109             ELSE
111109               IF CM-NOTE-SW = '4'
111109                  MOVE '6'         TO CM-NOTE-SW
111109               ELSE
111109                 IF CM-NOTE-SW = '5'
111109                    MOVE '7'       TO CM-NOTE-SW
111109                 END-IF
111109               END-IF
111109             END-IF
111109           END-IF
111109        ELSE
111109           IF CM-NOTE-SW = '2'
111109              MOVE ' '             TO CM-NOTE-SW
111109           ELSE
111109             IF CM-NOTE-SW = '3'
111109                MOVE '1'           TO CM-NOTE-SW
111109             ELSE
111109               IF CM-NOTE-SW = '6'
111109                  MOVE '4'         TO CM-NOTE-SW
111109               ELSE
111109                 IF CM-NOTE-SW = '7'
111109                    MOVE '5'       TO CM-NOTE-SW
111109                 END-IF
111109               END-IF
111109             END-IF
111109           END-IF
111109        END-IF
              EXEC CICS REWRITE
                 DATASET    (FILE-ID-ELCERT)
                 FROM       (CERTIFICATE-MASTER)
                 RESP       (WS-RESPONSE)
              END-EXEC
              IF RESP-NORMAL
                 CONTINUE
              ELSE
                 MOVE WS-RESPONSE TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 GO TO 8200-SEND-DATAONLY
              END-IF
           ELSE
              MOVE -1                  TO BMAINTL
              MOVE 0249                TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              GO TO 8200-SEND-DATAONLY
           END-IF

           .
       1520-EXIT.
           EXIT.

       1600-UPDATE-ERPYAJ.

           PERFORM 1610-ADD-ERPYAJ-BANK-REC
                                       THRU 1610-EXIT
           PERFORM 1620-ADD-ERPYAJ-DLR-REC
                                       THRU 1620-EXIT
           
           .
       1600-EXIT.
           EXIT.

       1610-ADD-ERPYAJ-BANK-REC.

           EXEC CICS GETMAIN
               SET     (ADDRESS OF PENDING-PAY-ADJ)
               LENGTH  (ERPYAJ-RECORD-LENGTH)
               INITIMG (GETMAIN-SPACE)                                 
           END-EXEC

           MOVE PB-COMPANY-CD          TO ERPYAJ-COMPANY-CD
           MOVE PB-CARRIER             TO ERPYAJ-CARRIER
           MOVE PB-GROUPING            TO ERPYAJ-GROUPING
           MOVE PB-I-BANK-NUMBER       TO ERPYAJ-FIN-RESP
           MOVE LOW-VALUES             TO ERPYAJ-ACCOUNT
           MOVE +10                    TO ERPYAJ-SEQ-NO
           MOVE 'R'                    TO ERPYAJ-TYPE
           IF REVERSAL
              MOVE 'C'                 TO ERPYAJ-TYPE
           END-IF
           MOVE SPACES                 TO PENDING-PAY-ADJ
           MOVE 'PY'                   TO PY-RECORD-ID
           MOVE ERPYAJ-KEY             TO PY-CONTROL-PRIMARY
           MOVE WS-OC-AMT              TO PY-ENTRY-AMT
           IF REVERSAL
              MOVE PB-I-DCC-OVER-CHG-AMT
                                       TO PY-ENTRY-AMT
           END-IF
           MOVE WS-CURRENT-BIN-DT      TO PY-LAST-MAINT-DT
                                          PY-INPUT-DT
      *    MOVE X'A15F'                TO PY-LAST-MAINT-DT
      *                                   PY-INPUT-DT
           MOVE PI-PROCESSOR-ID        TO PY-LAST-MAINT-BY
           MOVE PI-CR-MONTH-END-DT     TO PY-CREDIT-SELECT-DT
           MOVE 'OVERCHARGE'           TO PY-GL-COMMENT
090607     MOVE 'B'                    TO PY-ERCOMP-TYPE
           IF PB-CARRIER = '2'
              MOVE '2725040330'        TO PY-GL-ACCOUNT
           ELSE
              MOVE '2725040130'        TO PY-GL-ACCOUNT
           END-IF
           MOVE LOW-VALUES             TO PY-CREDIT-ACCEPT-DT
                                          PY-BILLED-DATE
                                          PY-AR-DATE
                                          PY-REPORTED-DT
                                          PY-CHECK-WRITTEN-DT
           MOVE ZEROS                  TO PY-CHECK-QUE-CONTROL
                                          PY-CHECK-QUE-SEQUENCE
           MOVE EIBTIME                TO PY-LAST-MAINT-HHMMSS
           PERFORM WITH TEST AFTER
              UNTIL (NOT RESP-DUPKEY)
                 AND (NOT RESP-DUPREC)
              EXEC CICS WRITE
                 DATASET    (FILE-ID-ERPYAJ)
                 FROM       (PENDING-PAY-ADJ)
                 RIDFLD     (ERPYAJ-KEY)
                 RESP       (WS-RESPONSE)
              END-EXEC
              IF RESP-DUPKEY OR RESP-DUPREC
                 ADD +1                TO ERPYAJ-SEQ-NO
                 MOVE ERPYAJ-SEQ-NO    TO PY-FILE-SEQ-NO
              END-IF
           END-PERFORM

           .
       1610-EXIT.
           EXIT.

       1620-ADD-ERPYAJ-DLR-REC.

           MOVE 'C'                    TO ERPYAJ-TYPE
           IF REVERSAL
              MOVE 'R'                 TO ERPYAJ-TYPE
           END-IF
           MOVE PB-ACCOUNT             TO ERPYAJ-FIN-RESP
                                          ERPYAJ-ACCOUNT
           ADD +1                      TO ERPYAJ-SEQ-NO
           MOVE ERPYAJ-KEY             TO PY-CONTROL-PRIMARY
090607     MOVE 'A'                    TO PY-ERCOMP-TYPE
           IF PB-CARRIER = '2'
              MOVE '2725040330'        TO PY-GL-ACCOUNT
           ELSE
              MOVE '2725040130'        TO PY-GL-ACCOUNT
           END-IF

           PERFORM WITH TEST AFTER
              UNTIL (NOT RESP-DUPKEY)
                 AND (NOT RESP-DUPREC)
              EXEC CICS WRITE
                 DATASET    (FILE-ID-ERPYAJ)
                 FROM       (PENDING-PAY-ADJ)
                 RIDFLD     (ERPYAJ-KEY)
                 RESP       (WS-RESPONSE)
              END-EXEC
              IF RESP-DUPKEY OR RESP-DUPREC
                 ADD +1                TO ERPYAJ-SEQ-NO
                 MOVE ERPYAJ-SEQ-NO    TO PY-FILE-SEQ-NO
              END-IF
           END-PERFORM

           .
       1620-EXIT.
           EXIT.

       1700-SQUEEZE-IT-IN.

111109     IF (N1 > CN-BILLING-END-LINE-NO)
111109        PERFORM VARYING N1 FROM N1 BY -1 UNTIL
111109           N1 = +1
111109           MOVE CN-LINE (N1 - 1)    TO CN-LINE (N1)
111109           IF (N1 - 1) = (CN-BILLING-END-LINE-NO + 1)
                    IF PI-COMPANY-ID = 'DCC' or 'VPP'
111109                 MOVE WS-OC-MESSAGE TO CN-LINE (N1 - 1)
                    ELSE
111109                 MOVE WS-VA-NO-COMM-MESS TO CN-LINE (N1 - 1)
                    END-IF
111109              COMPUTE CN-BILLING-END-LINE-NO = N1 - 1
111109              MOVE +2 TO N1
                 END-IF
              END-PERFORM
           ELSE
111109        IF (N1 < CN-BILLING-START-LINE-NO)
                 PERFORM VARYING N1 FROM N1 BY +1 UNTIL
                    N1 = +10
                    MOVE CN-LINE (N1 + 1) TO CN-LINE (N1)
                    IF (N1 + 1) = (CN-BILLING-START-LINE-NO - 1)
                       IF PI-COMPANY-ID = 'DCC' or 'VPP'
                          MOVE WS-OC-MESSAGE TO CN-LINE (N1 + 1)
                       ELSE
                          MOVE WS-VA-NO-COMM-MESS TO CN-LINE (N1 + 1)
                       END-IF
                       COMPUTE CN-BILLING-START-LINE-NO = N1 + 1
                       MOVE +9 TO N1
                    END-IF
                 END-PERFORM
              END-IF
           END-IF

           .
       1700-EXIT.
           EXIT.

       1800-REMOVE-CERT-NOTES.

           MOVE PB-CONTROL-BY-ACCOUNT (1:33)
                                       TO ERNOTE-KEY

           EXEC CICS READ
              SET        (ADDRESS OF CERTIFICATE-NOTE)
              DATASET    (FILE-ID-ERNOTE)
              RIDFLD     (ERNOTE-KEY)
              RESP       (WS-RESPONSE)
              UPDATE
           END-EXEC

           IF RESP-NORMAL
              PERFORM VARYING N1 FROM +1 BY +1 UNTIL
                 (N1 > +10)
                 OR (CN-LINE (N1) (1:10) = 'OVERCHARGE')
102408           OR (CN-LINE (N1) (1:20) = 'MAXIMUM AGE EXCEEDED')
              END-PERFORM
              IF N1 < +11
                 MOVE SPACES              TO CN-LINE (N1)
111109           IF N1 >= CN-BILLING-START-LINE-NO  AND
111109              N1 <= CN-BILLING-END-LINE-NO
111109              SUBTRACT 1 FROM CN-BILLING-END-LINE-NO
111109              IF CN-BILLING-END-LINE-NO < 
111109                               CN-BILLING-START-LINE-NO
111109                 MOVE ZEROS         TO CN-BILLING-END-LINE-NO
111109                                     CN-BILLING-START-LINE-NO
111109              END-IF
111109              PERFORM 1820-SQUEEZE-ERNOTE
111109                                  THRU 1820-EXIT
111109           END-IF
                 IF CN-LINES = SPACES
                    EXEC CICS DELETE
                       DATASET    (FILE-ID-ERNOTE)
                       RESP       (WS-RESPONSE)
                    END-EXEC
                    IF RESP-NORMAL
111109                 MOVE 'D'        TO WS-NOTE-SW
                       PERFORM 1520-UPDATE-ELCERT
                                       THRU 1520-EXIT
                    END-IF
                 ELSE
                    EXEC CICS REWRITE
                       DATASET    (FILE-ID-ERNOTE)
                       FROM       (CERTIFICATE-NOTE)
                       RESP       (WS-RESPONSE)
                    END-EXEC
                 END-IF
              END-IF
           END-IF

           .
       1800-EXIT.
           EXIT.

       1820-SQUEEZE-ERNOTE.

111109     MOVE CN-BILLING-START-LINE-NO TO N2
111109     IF N2 = 0
111109         MOVE +1                 TO N2
111109     END-IF
111109     PERFORM VARYING N1 FROM N2 BY +1 UNTIL
              (N1 > +9)
              IF CN-LINE (N1) = SPACES
                 MOVE CN-LINE (N1 + +1)
                                       TO CN-LINE (N1)
                 MOVE SPACES           TO CN-LINE (N1 + +1)
              END-IF
           END-PERFORM

           .
       1820-EXIT.
           EXIT.

       1900-DELETE-ERPYAJ.

           MOVE PB-COMPANY-CD          TO ERPYAJ-COMPANY-CD
           MOVE PB-CARRIER             TO ERPYAJ-CARRIER
           MOVE PB-GROUPING            TO ERPYAJ-GROUPING
           MOVE PB-I-BANK-NUMBER       TO ERPYAJ-FIN-RESP
           MOVE LOW-VALUES             TO ERPYAJ-ACCOUNT
           MOVE +10                    TO ERPYAJ-SEQ-NO
           MOVE 'R'                    TO ERPYAJ-TYPE

           MOVE ERPYAJ-KEY             TO WS-SAVE-ERPYAJ-KEY

           EXEC CICS STARTBR
              DATASET    (FILE-ID-ERPYAJ)
              RIDFLD     (ERPYAJ-KEY)
              RESP       (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
              CONTINUE
           ELSE
              GO TO 1900-GET-DEALER
           END-IF   

           .
       1900-READNEXT-ERPYAJ-B.

      *  GET THE BANK ERPYAJ RECORD
           EXEC CICS READNEXT
              DATASET    (FILE-ID-ERPYAJ)
              RIDFLD     (ERPYAJ-KEY)
              SET        (ADDRESS OF PENDING-PAY-ADJ)
              RESP       (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
              IF PY-CONTROL-PRIMARY (1:28) = WS-SAVE-ERPYAJ-KEY (1:28)
                 IF (PB-I-DCC-OVER-CHG-AMT = PY-ENTRY-AMT)
                    IF (PY-INPUT-DT = WS-CURRENT-BIN-DT)
                       AND (PY-LAST-MAINT-BY = PI-PROCESSOR-ID)
                       EXEC CICS READ
                          DATASET    (FILE-ID-ERPYAJ)
                          RIDFLD     (ERPYAJ-KEY)
                          SET        (ADDRESS OF PENDING-PAY-ADJ)
                          RESP       (WS-RESPONSE)
                          UPDATE
                       END-EXEC
                       IF RESP-NORMAL
                          EXEC CICS DELETE
                             DATASET   (FILE-ID-ERPYAJ)
                             RESP      (WS-RESPONSE)
                          END-EXEC
                       END-IF
                    ELSE
                       SET REVERSAL    TO TRUE
                       PERFORM 1600-UPDATE-ERPYAJ
                                       THRU 1600-EXIT
                       GO TO 1900-EXIT
                    END-IF
                 ELSE
                    GO TO 1900-READNEXT-ERPYAJ-B
                 END-IF
              END-IF
              EXEC CICS ENDBR
                 DATASET   (FILE-ID-ERPYAJ)
              END-EXEC
           END-IF

           .
       1900-GET-DEALER.

           MOVE PB-COMPANY-CD          TO ERPYAJ-COMPANY-CD
           MOVE PB-CARRIER             TO ERPYAJ-CARRIER
           MOVE PB-GROUPING            TO ERPYAJ-GROUPING
           MOVE PB-ACCOUNT             TO ERPYAJ-FIN-RESP
           MOVE PB-ACCOUNT             TO ERPYAJ-ACCOUNT
           MOVE +10                    TO ERPYAJ-SEQ-NO
           MOVE 'C'                    TO ERPYAJ-TYPE

           MOVE ERPYAJ-KEY             TO WS-SAVE-ERPYAJ-KEY

           EXEC CICS STARTBR
              DATASET    (FILE-ID-ERPYAJ)
              RIDFLD     (ERPYAJ-KEY)
              RESP       (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
              CONTINUE
           ELSE
              GO TO 1900-EXIT
           END-IF

           .
       1900-READNEXT-ERPYAJ-D.

      *  GET THE DEALER ERPYAJ RECORD
           EXEC CICS READNEXT
              DATASET    (FILE-ID-ERPYAJ)
              RIDFLD     (ERPYAJ-KEY)
              SET        (ADDRESS OF PENDING-PAY-ADJ)
              RESP       (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
              IF PY-CONTROL-PRIMARY (1:28) = WS-SAVE-ERPYAJ-KEY (1:28)
                 IF (PB-I-DCC-OVER-CHG-AMT = PY-ENTRY-AMT)
                    IF (PY-INPUT-DT = WS-CURRENT-BIN-DT)
                       AND (PY-LAST-MAINT-BY = PI-PROCESSOR-ID)
                       EXEC CICS READ
                          DATASET    (FILE-ID-ERPYAJ)
                          RIDFLD     (ERPYAJ-KEY)
                          SET        (ADDRESS OF PENDING-PAY-ADJ)
                          RESP       (WS-RESPONSE)
                          UPDATE
                       END-EXEC
                       IF RESP-NORMAL
                          EXEC CICS DELETE
                             DATASET   (FILE-ID-ERPYAJ)
                             RESP      (WS-RESPONSE)
                          END-EXEC
                       END-IF
                    ELSE
                       SET REVERSAL    TO TRUE
                    END-IF
                 ELSE
                    GO TO 1900-READNEXT-ERPYAJ-D
                 END-IF
              END-IF
              EXEC CICS ENDBR
                 DATASET   (FILE-ID-ERPYAJ)
              END-EXEC
           END-IF

           .
       1900-EXIT.
           EXIT.

03883 ******************************************************************
03884 *            C A N C E L   M A I N L I N E                       *
03885 ******************************************************************
03886                                                                   
03887  2000-CANCEL-MAINLINE.  
03888      IF PI-DISPLAY-ORIGINAL-BATCH                                 
03889         IF CMAINTI NOT = 'C' AND 'D' AND 'S' AND 'K'              
03890            MOVE ER-0023             TO EMI-ERROR                  
03891            MOVE -1                  TO CMAINTL                    
03892            MOVE AL-UABON            TO CMAINTA                    
03893            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
03894            GO TO 8200-SEND-DATAONLY                               
03895         ELSE                                                      
03896            NEXT SENTENCE                                          
03897      ELSE                                                         
03898         IF CMAINTI NOT = 'A' AND 'C' AND 'D' AND 'S' AND 'K'      
03899            MOVE ER-0023             TO EMI-ERROR                  
03900            MOVE -1                  TO CMAINTL                    
03901            MOVE AL-UABON            TO CMAINTA                    
03902            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
03903            GO TO 8200-SEND-DATAONLY.                              
03904                                                                   
03905      IF CMAINTI = 'S'                                             
03906         GO TO 2100-SHOW-REQUEST.                                  
03907                                                                   
03908      IF NOT MODIFY-CAP                                            
03909          MOVE 'UPDATE'       TO SM-READ                           
03910          PERFORM 9995-SECURITY-VIOLATION                          
03911          MOVE ER-0070        TO EMI-ERROR                         
03912          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
03913          GO TO 8100-SEND-INITIAL-MAP.                             
03914                                                                   
03915      IF CMAINTL GREATER THAN ZEROS                                
03916         IF PI-PF6-FUNCTION                                        
03917            IF CMAINTI NOT = 'A'                                   
03918               MOVE AL-UABON  TO CMAINTA                           
03919               MOVE -1        TO CMAINTL                           
03920               MOVE ER-2260   TO EMI-ERROR                         
03921               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            
03922               GO TO 8200-SEND-DATAONLY.                           
03923                                                                   
03924      IF CCERTNOL = ZEROS                                          
03925        AND (CMAINTI = 'C' OR 'K' OR 'D')                          
03926          NEXT SENTENCE                                            
03927      ELSE                                                         
03928          IF CCERTNOL NOT = ZEROS                                  
03929              MOVE AL-UANON       TO CCERTNOA                      
03930          ELSE                                                     
03931              MOVE -1             TO CCERTNOL                      
03932              MOVE  1             TO SET-CURSOR-SW                 
03933              MOVE ER-2218       TO EMI-ERROR                      
03934              MOVE AL-UABON       TO CCERTNOA                      
03935              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
03936                                                                   
03937      IF CEFFDTL = ZEROS AND (CMAINTI = 'C' OR 'K' OR 'D')         
03938         NEXT SENTENCE                                             
03939      ELSE                                                         
03940         IF CEFFDTL NOT = ZEROS                                    
03941            MOVE AL-UNNON         TO CEFFDTA                       
03942            MOVE CEFFDTI TO DEEDIT-FIELD                           
03943            PERFORM 8600-DEEDIT                                    
03944            MOVE 4                TO DC-OPTION-CODE                
03945            MOVE DEEDIT-FIELD-V0  TO DC-GREG-DATE-1-MDY            
03946            PERFORM 9700-DATE-LINK                                 
03947            IF NO-CONVERSION-ERROR                                 
03948               MOVE DC-BIN-DATE-1 TO WS-CONVERTED-EFFDT            
03949               ELSE                                                
03950               MOVE -1            TO CEFFDTL                       
03951               MOVE  1            TO SET-CURSOR-SW                 
03952               MOVE ER-2226      TO EMI-ERROR                      
03953               MOVE AL-UNBON      TO CEFFDTA                       
03954               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            
03955           ELSE                                                    
03956           MOVE -1                 TO CEFFDTL                      
03957           MOVE  1                 TO SET-CURSOR-SW                
03958           MOVE ER-2220            TO EMI-ERROR                    
03959           MOVE AL-UNBON           TO CEFFDTA                      
03960           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               
03961                                                                   
03962      MOVE PI-PREV-ALT-KEY            TO WS-ERPNDB-ALT-KEY.        
03963                                                                   
03964      IF CCERTNOL NOT = ZEROS                                      
03965         MOVE CCERTNOI                TO WS-NEW-PRIME              
03966      ELSE                                                         
03967         MOVE SPACES                  TO WS-NEW-PRIME.             
03968                                                                   
03969      IF CSUFIXL NOT = ZEROS                                       
03970         MOVE CSUFIXI                 TO WS-NEW-SUFFIX             
03971      ELSE                                                         
03972         MOVE SPACE                   TO WS-NEW-SUFFIX.            
03973                                                                   
03974      IF CEFFDTL NOT = ZEROS                                       
03975         MOVE WS-CONVERTED-EFFDT      TO WS-NEW-CERT-EFF-DT        
03976      ELSE                                                         
03977         MOVE LOW-VALUES              TO WS-NEW-CERT-EFF-DT.       
03978                                                                   
03979      IF CMAINTI = 'A' OR 'K' OR 'S'                               
03980         NEXT SENTENCE                                             
03981      ELSE                                                         
03982         MOVE PI-PREV-ALT-KEY     TO WS-ERPNDB-ALT-KEY             
03983         IF WS-PB-CERT-NO     NOT = WS-NEW-CERT-NO     OR          
03984            WS-PB-CERT-EFF-DT NOT = WS-NEW-CERT-EFF-DT OR          
03985            WS-PB-RECORD-TYPE NOT = '2'                            
03986            IF CMAINTI = 'C'                                       
03987               MOVE ER-2287          TO EMI-ERROR                  
03988               MOVE -1               TO CCERTNOL                   
03989               MOVE AL-UABON         TO CCERTNOA CEFFDTA           
03990               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            
03991               GO TO 8200-SEND-DATAONLY                            
03992              ELSE                                                 
03993               MOVE ER-2260          TO EMI-ERROR                  
03994               MOVE -1               TO CCERTNOL                   
03995               MOVE AL-UABON         TO CCERTNOA                   
03996               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            
03997               GO TO 8200-SEND-DATAONLY.                           
03998                                                                   
03999      IF CMAINTI = 'D'                                             
04000         GO TO 2400-DELETE-CANCEL-ROUTINE.                         
04001                                                                   
04002      IF CLSTNML GREATER THAN ZEROS                                
04003          MOVE AL-UANON           TO CLSTNMA.                      
04004                                                                   
04005      EJECT                                                        
04006                                                                   
04007 ******************************************************************
04008 *                                                                *
04009 *           E D I T   C A N C E L   C O V E R A G E S            *
04010 *                                                                *
04011 ******************************************************************
04012                                                                   
04013  2015-EDIT-COVERAGES.  
04014      IF CCANDT1L      GREATER THAN ZEROS OR                       
04015         CREFND1L      GREATER THAN ZEROS OR                       
04016         CCANDT2L      GREATER THAN ZEROS OR                       
04017         CREFND2L      GREATER THAN ZEROS OR                       
04018         CMTHD1L       GREATER THAN ZEROS OR                       
04019         CMTHD2L       GREATER THAN ZEROS OR                       
04020         CLIVESL       GREATER THAN ZEROS OR                       
04021         CPAYEEL       GREATER THAN ZEROS OR                       
04022         CFORCEL       GREATER THAN ZEROS OR                       
04023         CBILCDL       GREATER THAN ZEROS OR
              CCANREAL      GREATER THAN ZEROS
04024             NEXT SENTENCE                                         
04025          ELSE                                                     
04026             GO TO 2030-EDIT-COMPLETE.                             
04027                                                                   
04028      IF CCANDT1L = ZEROS                                          
04029         AND (CMAINTI = 'C' OR 'K')                                
04030         NEXT SENTENCE                                             
04031      ELSE                                                         
04032         IF CCANDT1L      GREATER THAN ZEROS                       
04033             MOVE AL-UNNON            TO CCANDT1A                  
04034             IF CCANDT1I = SPACES                                  
04035                MOVE LOW-VALUES       TO WS-CONVERTED-CANDT1       
04036             ELSE                                                  
04037                MOVE CCANDT1I            TO DEEDIT-FIELD           
04038                PERFORM 8600-DEEDIT                                
04039                IF DEEDIT-FIELD-V0   NUMERIC                       
04040                   MOVE DEEDIT-FIELD-V0  TO DC-GREG-DATE-1-MDY     
04041                   MOVE 4                TO DC-OPTION-CODE         
04042                   PERFORM 9700-DATE-LINK                          
04043                   IF NO-CONVERSION-ERROR                          
04044                      MOVE DC-BIN-DATE-1    TO WS-CONVERTED-CANDT1 
04045                   ELSE                                            
04046                       MOVE -1        TO CCANDT1L                  
04047                       MOVE ER-2227   TO EMI-ERROR                 
04048                       MOVE AL-UNBON  TO CCANDT1A                  
04049                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT    
04050                ELSE                                               
04051                   MOVE -1         TO CCANDT1L                     
04052                   MOVE ER-2223    TO EMI-ERROR                    
04053                   MOVE AL-UNBON   TO CCANDT1A                     
04054                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT        
04055         ELSE                                                      
04056             IF CREFND1L GREATER THAN ZEROS                        
04057                MOVE -1             TO CCANDT1L                    
04058                MOVE ER-2222        TO EMI-ERROR                   
04059                MOVE AL-UNBOF       TO CCANDT1A                    
04060                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.          
04061                                                                   
04062      IF CREFND1L = ZEROS                                          
04063         AND (CMAINTI = 'C' OR 'K')                                
04064          NEXT SENTENCE                                            
04065      ELSE                                                         
04066          IF CREFND1L NOT = ZEROS                                  
04067             MOVE AL-UNNON        TO CREFND1A                      
04068             EXEC CICS BIF DEEDIT
04069                 FIELD  (CREFND1I)
04070                 LENGTH (11)
04071             END-EXEC
04072             MOVE CREFND1I        TO WS-CREFND1
CIDMOD*           MOVE CREFND1I        TO DEEDIT-FIELD-X11              
CIDMOD*           PERFORM 8600-DEEDIT                                   
CIDMOD*           MOVE DEEDIT-FIELD-V2 TO WS-CREFND1                    
04073             IF WS-CREFND1  = ZEROS                                
04074                AND CMAINTI = 'A'                                  
04075              MOVE ER-2437        TO EMI-ERROR                     
04076              MOVE -1             TO CREFND1L                      
04077              MOVE AL-UNBON       TO CREFND1A                      
04078              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
04079                                                                   
           IF CCANREAL > +0
              MOVE AL-UANON            TO CCANREAA
              IF CCANREAI = 'R' OR ' '
                 CONTINUE
              ELSE
                 MOVE ER-9841        TO EMI-ERROR
                 MOVE -1             TO CCANREAL
                 MOVE AL-UABON       TO CCANREAA
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              END-IF
           END-IF

04080      IF CMTHD1L EQUAL +0                                          
04081         NEXT SENTENCE                                             
04082      ELSE                                                         
04083         IF CMTHD1L NOT = +0                                       
04084            MOVE AL-UNNON        TO CMTHD1A                        
04085            IF CMTHD1I EQUAL '1' OR '2' OR '3' OR '4'              
04086                          OR '5' OR '6' OR '8' OR '9'              
04087                          OR 'R' OR ' '                            
04088               NEXT SENTENCE                                       
04089            ELSE                                                   
04090               MOVE ER-0582        TO EMI-ERROR                    
04091               MOVE -1             TO CMTHD1L                      
04092               MOVE AL-UNBON       TO CMTHD1A                      
04093               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.           
04094                                                                   
04095      IF CCANDT2L = ZEROS                                          
04096         AND (CMAINTI = 'C' OR 'K')                                
04097          NEXT SENTENCE                                            
04098      ELSE                                                         
04099          IF CCANDT2L  GREATER THAN ZEROS                          
04100              MOVE AL-UNNON       TO CCANDT2A                      
04101              IF CCANDT2I = SPACES                                 
04102                 MOVE LOW-VALUES  TO WS-CONVERTED-CANDT2           
04103              ELSE                                                 
04104                 MOVE CCANDT2I       TO DEEDIT-FIELD               
04105                 PERFORM 8600-DEEDIT                               
04106                 IF DEEDIT-FIELD-V0   NUMERIC                      
04107                    MOVE DEEDIT-FIELD-V0  TO DC-GREG-DATE-1-MDY    
04108                    MOVE 4                TO DC-OPTION-CODE        
04109                    PERFORM 9700-DATE-LINK                         
04110                    IF NO-CONVERSION-ERROR                         
04111                       MOVE DC-BIN-DATE-1    TO WS-CONVERTED-CANDT2
04112                    ELSE                                           
04113                       MOVE -1       TO CCANDT2L                   
04114                       MOVE ER-2227  TO EMI-ERROR                  
04115                       MOVE AL-UNBON TO CCANDT2A                   
04116                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT    
04117                 ELSE                                              
04118                    MOVE -1         TO CCANDT2L                    
04119                    MOVE ER-2223    TO EMI-ERROR                   
04120                    MOVE AL-UNBON   TO CCANDT2A                    
04121                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT       
04122          ELSE                                                     
04123             IF CREFND2L GREATER THAN ZEROS                        
04124                MOVE -1             TO CCANDT2L                    
04125                MOVE ER-2222        TO EMI-ERROR                   
04126                MOVE AL-UNBOF       TO CCANDT2A                    
04127                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.          
04128                                                                   
04129      IF CREFND2L = ZEROS                                          
04130         AND (CMAINTI = 'C' OR 'K')                                
04131          NEXT SENTENCE                                            
04132      ELSE                                                         
04133          IF CREFND2L NOT = ZEROS                                  
04134             MOVE AL-UNNON        TO CREFND2A                      
04135             EXEC CICS BIF DEEDIT
04136                 FIELD  (CREFND2I)
04137                 LENGTH (11)
04138             END-EXEC
04139             MOVE CREFND2I        TO WS-CREFND2
CIDMOD*           MOVE CREFND2I        TO DEEDIT-FIELD-X11              
CIDMOD*           PERFORM 8600-DEEDIT                                   
CIDMOD*           MOVE DEEDIT-FIELD-V2 TO WS-CREFND2                    
04140             IF WS-CREFND2  = ZEROS                                
04141                AND CMAINTI = 'A'                                  
04142              MOVE ER-2437        TO EMI-ERROR                     
04143              MOVE -1             TO CREFND2L                      
04144              MOVE AL-UNBON       TO CREFND2A                      
04145              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
04146                                                                   
04147      IF CMTHD2L EQUAL +0                                          
04148         NEXT SENTENCE                                             
04149      ELSE                                                         
04150         IF CMTHD2L NOT = +0                                       
04151            MOVE AL-UNNON        TO CMTHD2A                        
04152            IF CMTHD2I EQUAL '1' OR '2' OR '3' OR '4'              
04153                          OR '5' OR '6' OR '8' OR '9'              
04154                          OR ' ' OR 'R'                            
04155               NEXT SENTENCE                                       
04156            ELSE                                                   
04157               MOVE ER-0582        TO EMI-ERROR                    
04158               MOVE -1             TO CMTHD2L                      
04159               MOVE AL-UNBON       TO CMTHD2A                      
04160               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.           
04161                                                                   
04162      IF CLIVESL GREATER THAN ZEROS                                
04163          MOVE CLIVESI            TO DEEDIT-FIELD                  
04164          PERFORM 8600-DEEDIT                                      
04165          IF DEEDIT-FIELD-V0 NUMERIC                               
04166              MOVE DEEDIT-FIELD-V0 TO WS-CLIVES                    
04167              MOVE AL-UNNON       TO CLIVESA                       
04168          ELSE                                                     
04169              MOVE -1             TO CLIVESL                       
04170              MOVE AL-UNBON       TO CLIVESA                       
04171              MOVE ER-2223        TO EMI-ERROR                     
04172              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
04173                                                                   
04174      IF CFORCEL  NOT = ZEROS                                      
04175          IF FORCE-CAP                                             
04176             MOVE AL-UANON            TO CFORCEA                   
04177          ELSE                                                     
04178             MOVE ER-2945             TO EMI-ERROR                 
04179             MOVE AL-UABON            TO CFORCEA                   
04180             MOVE -1                  TO CFORCEL                   
04181             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.             
04182                                                                   
04183      IF CPAYEEL GREATER THAN ZEROS                                
04184         MOVE AL-UANON            TO CPAYEEA.                      
04185                                                                   
04186      IF CBILCDL NOT = ZEROS                                       
04187         IF CBILCDI NOT = ' ' AND 'H' AND 'R' AND 'L'              
04188                              AND 'A' AND 'B' AND 'E'              
04189            MOVE ER-2225              TO EMI-ERROR                 
04190            MOVE AL-UABON             TO CBILCDA                   
04191            MOVE -1                   TO CBILCDL                   
04192            MOVE  1                   TO SET-CURSOR-SW             
04193            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
04194         ELSE                                                      
04195            MOVE AL-UANON             TO CBILCDA.                  
04196                                                                   
04197  2030-EDIT-COMPLETE.    
04198      IF EMI-ERROR = ZEROS                                         
04199          NEXT SENTENCE                                            
04200      ELSE                                                         
04201          GO TO 8200-SEND-DATAONLY.                                
04202                                                                   
04203      IF CMAINTI = 'A'                                             
04204         GO TO 2200-ADD-CANCEL-ROUTINE.                            
04205                                                                   
04206      IF CMAINTI = 'C'                                             
04207         GO TO 2300-CHANGE-CANCEL-ROUTINE.                         
04208                                                                   
04209      GO TO 2900-REWRITE-CAN-CERT-KEY.                             
04210                                                                   
04211      EJECT                                                        
04212                                                                   
04213 ******************************************************************
04214 *                                                                *
04215 *                   S H O W   C A N C E L                        *
04216 *                                                                *
04217 ******************************************************************
04218                                                                   
04219  2100-SHOW-REQUEST.    
04220      MOVE 'S'                    TO PI-MAINT-FUNCTION.            
04221                                                                   
04222      IF CCERTNOL = ZEROS AND CEFFDTL = ZEROS                      
04223         MOVE -1                  TO CMAINTL                       
04224         GO TO 8200-SEND-DATAONLY.                                 
04225                                                                   
04226      MOVE CCERTNOI            TO WS-NEW-PRIME.                    
04227                                                                   
04228      IF CSUFIXL NOT = ZEROS                                       
04229         MOVE CSUFIXI          TO WS-NEW-SUFFIX                    
04230        ELSE                                                       
04231         MOVE SPACE            TO WS-NEW-SUFFIX.                   
04232                                                                   
04233      MOVE CEFFDTI                TO DEEDIT-FIELD.                 
04234      PERFORM 8600-DEEDIT.                                         
04235      MOVE DEEDIT-FIELD-V0        TO DC-GREG-DATE-1-MDY.           
04236      MOVE '4'                    TO DC-OPTION-CODE.               
04237      PERFORM 9700-DATE-LINK.                                      
04238      IF DATE-CONVERSION-ERROR                                     
04239         MOVE ER-0348             TO EMI-ERROR                     
04240         MOVE -1                  TO CEFFDTL                       
04241         MOVE AL-UABON            TO CEFFDTA                       
04242         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
04243         GO TO 8200-SEND-DATAONLY                                  
04244      ELSE                                                         
04245         MOVE DC-GREG-DATE-1-EDIT TO CEFFDTI                       
04246         MOVE AL-UANON            TO CEFFDTA                       
04247         MOVE DC-BIN-DATE-1       TO WS-NEW-CERT-EFF-DT.           
04248                                                                   
04249      MOVE PI-PREV-ALT-KEY        TO ERPNDB-ALT-KEY                
04250                                                                   
04251      IF (ERPNDB-CERT-NO     NOT = WS-NEW-CERT-NO)      OR         
04252         (ERPNDB-CERT-EFF-DT NOT = WS-NEW-CERT-EFF-DT)             
04253         MOVE 'Y'                    TO WS-SHOW-SW                 
04254         IF PI-PRIMARY-BROWSE OR PI-PRIMARY-WITH-SELECT            
04255            GO TO 4100-BROWSE-PRIMARY-FORWARD                      
04256         ELSE                                                      
04257            IF PI-FILE-BROWSE                                      
04258               GO TO 4300-BROWSE-FILE-FORWARD                      
04259            ELSE                                                   
04260               IF PI-CSR-BROWSE                                    
04261                  GO TO 4200-BROWSE-CSR-FORWARD                    
04262               ELSE                                                
04263                  GO TO 4000-BROWSE-DETAIL-FORWARD                 
04264      ELSE                                                         
04265         GO TO 3900-RE-DISPLAY-RECORD.                             
04266                                                                   
04267      EJECT                                                        
04268                                                                   
04269                                                                   
04270 ******************************************************************
04271 *                                                                *
04272 *            A D D   C A N C E L   R O U T I N E                 *
04273 *                                                                *
04274 ******************************************************************
04275                                                                   
04276  2200-ADD-CANCEL-ROUTINE.            
04277      MOVE 'A'                    TO PI-MAINT-FUNCTION.            
04278                                                                   
04279      EXEC CICS GETMAIN                                            
04280          SET     (ADDRESS OF PENDING-BUSINESS)                    
04281          LENGTH  (ERPNDB-RECORD-LENGTH)                           
04282      END-EXEC.                                                    
04283                                                                   
04284      EXEC CICS HANDLE CONDITION                                   
04285           NOTFND    (2280-BATCH-HDR-NOT-FOUND)                    
04286      END-EXEC.                                                    
04287                                                                   
04288      MOVE PI-PREV-KEY            TO ERPNDB-KEY                    
04289      MOVE +9999                  TO ERPNDB-BATCH-SEQ-NO.          
04290      MOVE ZEROS                  TO ERPNDB-BATCH-CHG-SEQ-NO.      
04291                                                                   
04292      EXEC CICS READ                                               
04293          INTO    (PENDING-BUSINESS)                               
04294          DATASET (FILE-ID-ERPNDB)                                 
04295          RIDFLD  (ERPNDB-KEY)                                     
04296      END-EXEC.                                                    
04297                                                                   
04298      MOVE PB-CSR-ID              TO WS-BATCH-CSR-ID.              
04299                                                                   
04300      MOVE PB-BATCH-RECORD        TO WS-BATCH-RECORD.              
04301      ADD +1                      TO WS-B-HIGHEST-SEQ-NO.          
04302      MOVE PB-CREDIT-SELECT-DT    TO WS-CREDIT-SELECT-DT.          
04303      MOVE SPACES                 TO PENDING-BUSINESS.             
04304      MOVE 'PB'                   TO PB-RECORD-ID.                 
04305      MOVE PI-PREV-KEY            TO PB-CONTROL-PRIMARY.           
04306      MOVE PI-PREV-ALT-KEY        TO PB-CONTROL-BY-ACCOUNT.        
04307      MOVE SPACES                 TO PB-CERT-SFX.                  
04308      MOVE WS-B-HIGHEST-SEQ-NO    TO PB-BATCH-SEQ-NO.              
04309      MOVE ZEROS                  TO PB-BATCH-CHG-SEQ-NO           
04310                                     PB-ALT-CHG-SEQ-NO.            
04311                                                                   
04312      MOVE WS-CONVERTED-EFFDT     TO PB-CERT-EFF-DT.               
04313      MOVE CCERTNOI               TO PB-CERT-PRIME.                
04314      MOVE '2'                    TO PB-RECORD-TYPE.               
04315                                                                   
04316      MOVE PI-COMPANY-CD          TO PB-COMPANY-CD.                
04317      MOVE PI-COMPANY-ID          TO PB-COMPANY-ID.                
04318                                                                   
04319                                                                   
04320      MOVE ZEROS                  TO PB-BATCH-CHG-SEQ-NO           
04321                                     PB-ALT-CHG-SEQ-NO             
04322                                     PB-C-LF-REF-CALC              
04323                                     PB-C-AH-REF-CALC              
                                          PB-C-LF-RFND-CLP
                                          PB-C-AH-RFND-CLP
04324                                     PB-CI-INSURED-AGE             
04325                                     PB-CI-LF-TERM                 
04326                                     PB-CI-AH-TERM                 
04327                                     PB-CI-LF-BENEFIT-CD           
04328                                     PB-CI-LF-BENEFIT-AMT          
04329                                     PB-CI-LF-ALT-BENEFIT-AMT      
04330                                     PB-CI-LF-PREMIUM-AMT          
04331                                     PB-CI-LF-ALT-PREMIUM-AMT      
04332                                     PB-CI-AH-BENEFIT-CD           
04333                                     PB-CI-AH-BENEFIT-AMT          
04334                                     PB-CI-AH-PREMIUM-AMT          
04335                                     PB-CI-PAY-FREQUENCY           
04336                                     PB-CI-LOAN-APR                
04337                                     PB-CI-LOAN-TERM               
04338                                     PB-CI-LIFE-COMMISSION         
04339                                     PB-CI-AH-COMMISSION           
04340                                     PB-CI-CURR-SEQ                
04341                                     PB-CI-AH-CANCEL-AMT           
04342                                     PB-CI-LF-CANCEL-AMT           
04343                                     PB-CI-RATE-DEV-PCT-LF         
04344                                     PB-CI-RATE-DEV-PCT-AH         
04345                                     PB-CI-EXTENTION-DAYS          
04346                                     PB-CI-TERM-IN-DAYS            
04347                                     PB-CI-LIVES                   
04348                                     PB-CI-LF-CRIT-PER             
04349                                     PB-CI-AH-CRIT-PER             
04350                                     PB-C-LF-REM-TERM              
04351                                     PB-C-AH-REM-TERM              
04352                                     PB-CHG-COUNT                  
04353                                     PB-LF-BILLED-AMTS             
04354                                     PB-AH-BILLED-AMTS             
062017                                    PB-C-INT-ON-REFS                       
04355 *                                   PB-C-MICROFILM-NO             
04356                                     PB-CALC-TOLERANCE.            
04357                                                                   
04358      MOVE LOW-VALUES             TO PB-CI-AH-PAID-THRU-DT         
04359                                     PB-CI-AH-SETTLEMENT-DT        
04360                                     PB-CI-DEATH-DT                
04361                                     PB-CI-LF-PRIOR-CANCEL-DT      
04362                                     PB-CI-AH-PRIOR-CANCEL-DT      
04363                                     PB-CI-ENTRY-DT                
04364                                     PB-CI-LF-EXPIRE-DT            
04365                                     PB-CI-AH-EXPIRE-DT            
04366                                     PB-CI-LOAN-1ST-PMT-DT         
04367                                     PB-C-LF-CANCEL-DT             
04368                                     PB-C-AH-CANCEL-DT             
04369                                     PB-CREDIT-ACCEPT-DT           
04370                                     PB-BILLED-DT                  
04371                                     PB-ACCT-EFF-DT                
04372                                     PB-ACCT-EXP-DT.               
04373                                                                   
04374      MOVE 'X'                    TO PB-FATAL-FLAG.                
04375                                                                   
04376      MOVE WS-CREDIT-SELECT-DT    TO PB-CREDIT-SELECT-DT.          
04377                                                                   
04378      IF CSUFIXL     GREATER THAN ZEROS                            
04379         MOVE CSUFIXI             TO PB-CERT-SFX.                  
04380                                                                   
072908*    IF CMICRNOL  GREATER  ZEROS                                  
072908*        IF CMICRNOI  NUMERIC                                     
072908*            MOVE CMICRNOI       TO  PB-C-MICROFILM-NO            
072908*            MOVE AL-UNNON       TO  CMICRNOA                     
072908*        ELSE                                                     
072908*            MOVE -1             TO  CMICRNOL                     
072908*            MOVE AL-UNBON       TO  CMICRNOA                     
072908*            MOVE ER-2701        TO  EMI-ERROR                    
072908*            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
04390                                                                   
04391      IF CLSTNML     GREATER THAN ZEROS                            
04392         MOVE CLSTNMI             TO PB-C-LAST-NAME.               
04393                                                                   

           IF CCANREAL > ZEROS
              MOVE CCANREAI            TO PB-C-CANCEL-REASON
           END-IF

04394      IF CCANDT1L    GREATER THAN ZEROS                            
04395         MOVE WS-CONVERTED-CANDT1 TO PB-C-LF-CANCEL-DT.            
04396                                                                   
04397      IF CCANDT2L    GREATER THAN ZEROS                            
04398         MOVE WS-CONVERTED-CANDT2 TO PB-C-AH-CANCEL-DT.            
04399                                                                   
04400      IF CMTHD1L    GREATER THAN +0                                
04401         MOVE CMTHD1I             TO PB-C-LF-REFUND-OVERRIDE.      
04402                                                                   
04403      IF CMTHD2L    GREATER THAN +0                                
04404         MOVE CMTHD2I             TO PB-C-AH-REFUND-OVERRIDE.      
04405                                                                   
04406      IF CREFND1L    GREATER THAN ZEROS                            
04407         ADD  WS-CREFND1          TO WS-B-LF-CAN-PRM-ENTERED       
04408         MOVE WS-CREFND1          TO PB-C-LF-CANCEL-AMT            
04409      ELSE                                                         
04410         MOVE ZEROS               TO PB-C-LF-CANCEL-AMT.           
04411                                                                   
04412      IF CREFND2L    GREATER THAN ZEROS                            
04413         ADD  WS-CREFND2          TO WS-B-AH-CAN-PRM-ENTERED       
04414         MOVE WS-CREFND2          TO PB-C-AH-CANCEL-AMT            
04415      ELSE                                                         
04416         MOVE ZEROS               TO PB-C-AH-CANCEL-AMT.           
04417                                                                   
04418      IF CLIVESL     GREATER THAN ZEROS                            
04419         MOVE WS-CLIVES           TO PB-C-LIVES                    
04420      ELSE                                                         
04421         MOVE ZEROS               TO PB-C-LIVES.                   
04422                                                                   
04423      IF CPAYEEL     GREATER THAN ZEROS                            
04424         MOVE CPAYEEI             TO PB-C-PAYEE-CODE.              
04425                                                                   
04426      IF CFORCEL     GREATER THAN ZEROS                            
04427         MOVE CFORCEI             TO PB-FORCE-CODE.                
04428                                                                   
04429      IF CBILCDL     GREATER THAN ZEROS                            
04430         MOVE CBILCDI             TO PB-RECORD-BILL.               
04431                                                                   
011904*    MOVE WS-REFERENCE           TO PB-C-REFERENCE.               
04433                                                                   
04434  2260-WRITE-PB-RECORD.                                            
04435      MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY              
04436                                     PB-INPUT-BY.                  
04437      MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.         
04438      MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT              
04439                                     PB-INPUT-DT.                  
04440                                                                   
04441      MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY.              
04442      MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY                   
04443                                     PB-CONTROL-BY-ORIG-BATCH.     
04444                                                                   
04445      MOVE PB-COMPANY-CD          TO PB-CSR-COMPANY-CD.            
04446      MOVE WS-BATCH-CSR-ID        TO PB-CSR-ID.                    
04447      MOVE PB-ENTRY-BATCH         TO PB-CSR-ENTRY-BATCH.           
04448      MOVE PB-BATCH-SEQ-NO        TO PB-CSR-BATCH-SEQ-NO.          
04449      MOVE PB-BATCH-CHG-SEQ-NO    TO PB-CSR-BATCH-CHG-SEQ-NO.      
04450                                                                   
04451      PERFORM 9800-LINK-PENDING-EDIT THRU 9800-EXIT.               
04452                                                                   
04453      PERFORM 7300-FORMAT-ERRORS  THRU 7399-EXIT.                  
04454                                                                   
04455      MOVE PB-SV-CARRIER          TO PI-SV-CARRIER.                
04456      MOVE PB-SV-GROUPING         TO PI-SV-GROUPING.               
04457      MOVE PB-SV-STATE            TO PI-SV-STATE.                  
04458                                                                   
04459      ADD PB-C-LF-REF-CALC        TO WS-B-LF-CAN-PRM-COMPUTED.     
04460      ADD PB-C-AH-REF-CALC        TO WS-B-AH-CAN-PRM-COMPUTED.     
04461                                                                   
04462  2275-WRITE-PENDING-BUSINESS.                                     
04463      MOVE 'A'                    TO JP-RECORD-TYPE.               
04464      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
04465      MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
04466      MOVE FILE-ID-ERPNDB         TO JP-FILE-ID.                   
04467                                                                   
04468      EXEC CICS HANDLE CONDITION                                   
04469          DUPREC (2290-DUPLICATE-ALT-INDEX)                        
04470      END-EXEC.                                                    
04471                                                                   
04472      EXEC CICS WRITE                                              
04473          DATASET (FILE-ID-ERPNDB)                                 
04474          FROM    (PENDING-BUSINESS)                               
04475          RIDFLD  (PB-CONTROL-PRIMARY)                             
04476      END-EXEC.                                                    
04477                                                                   
04478      ADD +1                      TO WS-B-CANCEL-CNT-ENTERED.      
04479                                                                   
04480      PERFORM 8400-LOG-JOURNAL-RECORD.                             
04481                                                                   
071712******************************************************************
071712*       A D D  O R I G I N A L  C E R T  I N F O                 *
071712******************************************************************
071712
071712     display ' made it to add orig CANCEL cert '
071712     MOVE PB-CONTROL-BY-ACCOUNT  TO ELCERT-KEY.                   
071712     MOVE PB-SV-CARRIER          TO ELCERT-CARRIER.               
071712     MOVE PB-SV-GROUPING         TO ELCERT-GROUPING.              
071712     MOVE PB-SV-STATE            TO ELCERT-STATE.                 
071712                                                                  
071712     EXEC CICS READ                                               
071712         SET     (ADDRESS OF CERTIFICATE-MASTER)                  
071712         DATASET (FILE-ID-ELCERT)                                 
071712         RIDFLD  (ELCERT-KEY)                                     
071712         RESP    (WS-RESPONSE)
071712     END-EXEC.                                                    
071712
071712     IF NOT RESP-NORMAL
071712        GO TO 2276-BYPASS-CRTO
071712     END-IF
071712
071712     MOVE ELCERT-KEY             TO ERMAIL-KEY
071712     MOVE ' '                    TO WS-ERMAIL-SW
071712
071712     EXEC CICS READ
071712        DATASET   (FILE-ID-ERMAIL)
071712        SET       (ADDRESS OF MAILING-DATA)
071712        RIDFLD    (ERMAIL-KEY)
071712        RESP      (WS-RESPONSE)
071712     END-EXEC
071712
071712     IF RESP-NORMAL
071712        SET ERMAIL-FOUND TO TRUE
071712     END-IF
121712
121712     PERFORM 1295-READ-CERT-TRAILER THRU 1295-EXIT
071712
071712     MOVE SPACES                 TO ORIGINAL-CERTIFICATE
071712     MOVE 'OC'                   TO OC-RECORD-ID
071712     MOVE PB-CONTROL-BY-ACCOUNT (1:33)
071712                                 TO OC-CONTROL-PRIMARY (1:33)
071712     MOVE 'I'                    TO OC-RECORD-TYPE
071712     MOVE +4096                  TO OC-KEY-SEQ-NO
071712     MOVE PI-PROCESSOR-ID        TO OC-LAST-MAINT-BY
071712     MOVE EIBTIME                TO OC-LAST-MAINT-HHMMSS
071712     MOVE WS-CURRENT-BIN-DT      TO OC-LAST-MAINT-DT
071712
071712     MOVE CM-INSURED-LAST-NAME   TO OC-INS-LAST-NAME   
071712     MOVE CM-INSURED-FIRST-NAME  TO OC-INS-FIRST-NAME  
071712     MOVE CM-INSURED-INITIAL2    TO OC-INS-MIDDLE-INIT 
071712     MOVE CM-INSURED-ISSUE-AGE   TO OC-INS-AGE         
071712     MOVE CM-JT-LAST-NAME        TO OC-JNT-LAST-NAME   
071712     MOVE CM-JT-FIRST-NAME       TO OC-JNT-FIRST-NAME  
071712     MOVE CM-JT-INITIAL          TO OC-JNT-MIDDLE-INIT 
071712     MOVE CM-INSURED-JOINT-AGE   TO OC-JNT-AGE         
071712     MOVE CM-LF-BENEFIT-CD       TO OC-LF-BENCD        
071712     MOVE CM-LF-ORIG-TERM        TO OC-LF-TERM         
071712     MOVE CM-LF-BENEFIT-AMT      TO OC-LF-BEN-AMT      
071712     MOVE CM-LF-PREMIUM-AMT      TO OC-LF-PRM-AMT      
071712     MOVE CM-LF-ALT-BENEFIT-AMT  TO OC-LF-ALT-BEN-AMT  
071712     MOVE CM-LF-ALT-PREMIUM-AMT  TO OC-LF-ALT-PRM-AMT  
071712     MOVE CM-LF-LOAN-EXPIRE-DT   TO OC-LF-EXP-DT       
071712     MOVE CM-LIFE-COMM-PCT       TO OC-LF-COMM-PCT     
071712     MOVE PB-C-LF-CANCEL-DT      TO OC-LF-CANCEL-DT
071712     MOVE PB-C-LF-CANCEL-AMT     TO OC-LF-CANCEL-AMT
071712     MOVE CM-LF-ITD-CANCEL-AMT   TO OC-LF-ITD-CANCEL-AMT
071712     MOVE CM-AH-BENEFIT-CD       TO OC-AH-BENCD        
071712     MOVE CM-AH-ORIG-TERM        TO OC-AH-TERM         
071712     MOVE CM-AH-BENEFIT-AMT      TO OC-AH-BEN-AMT      
071712     MOVE CM-AH-PREMIUM-AMT      TO OC-AH-PRM-AMT      
071712     MOVE CM-AH-LOAN-EXPIRE-DT   TO OC-AH-EXP-DT       
071712     MOVE CM-AH-COMM-PCT         TO OC-AH-COMM-PCT     
071712     MOVE CM-AH-CRITICAL-PERIOD  TO OC-AH-CP
071712     MOVE PB-C-AH-CANCEL-DT      TO OC-AH-CANCEL-DT
071712     MOVE PB-C-AH-CANCEL-AMT     TO OC-AH-CANCEL-AMT
071712     MOVE CM-AH-ITD-CANCEL-AMT   TO OC-AH-ITD-CANCEL-AMT
071712     MOVE CM-LOAN-1ST-PMT-DT     TO OC-1ST-PMT-DT
011413     MOVE 'N'                    TO OC-ISSUE-TRAN-IND
011413     MOVE 'Y'                    TO OC-CANCEL-TRAN-IND
071712     
071712     IF ERMAIL-FOUND
071712        MOVE MA-CRED-BENE-NAME   TO OC-CRED-BENE-NAME
071712     END-IF
121712     IF NOT CERT-TRL-REC-NOT-FOUND
121712        MOVE CS-INS-AGE-DEFAULT-FLAG TO 
121712                           OC-INS-AGE-DEFAULT-FLAG
121712        MOVE CS-JNT-AGE-DEFAULT-FLAG TO 
121712                           OC-JNT-AGE-DEFAULT-FLAG
121712     END-IF
071712     MOVE LOW-VALUES             TO OC-ENDORSEMENT-PROCESSED-DT
071712     .
071712 2275-WRITE-ELCRTO.
071712
071712     EXEC CICS WRITE
071712        DATASET   ('ELCRTO')
071712        FROM      (ORIGINAL-CERTIFICATE)
071712        RIDFLD    (OC-CONTROL-PRIMARY)
071712        RESP      (WS-RESPONSE)
071712     END-EXEC
071712
071712     display ' just wrote 2275 ' ws-response
071712     IF RESP-DUPKEY
071712        SUBTRACT +1              FROM OC-KEY-SEQ-NO
071712        GO TO 2275-WRITE-ELCRTO
071712     ELSE
071712        IF NOT RESP-NORMAL
071712           MOVE -1               TO BMAINTL
071712           MOVE ER-7450          TO EMI-ERROR
071712           PERFORM 9900-ERROR-FORMAT
071712                                 THRU 9900-EXIT
071712           GO TO 8200-SEND-DATAONLY
071712        END-IF
071712     END-IF.
071712     
071712 2276-BYPASS-CRTO.
04482 ******************************************************************
04483 *       U P D A T E   T H E   B A T C H   H E A D E R            *
04484 ******************************************************************
04485                                                                   
04486      EXEC CICS READ                                               
04487          INTO    (PENDING-BUSINESS)                               
04488          DATASET (FILE-ID-ERPNDB)                                 
04489          RIDFLD  (ERPNDB-KEY)                                     
04490          UPDATE                                                   
04491      END-EXEC.                                                    
04492                                                                   
04493      MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
04494      MOVE FILE-ID-ERPNDB         TO JP-FILE-ID.                   
04495      MOVE 'B'                    TO JP-RECORD-TYPE.               
04496      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
04497                                                                   
04498      PERFORM 8400-LOG-JOURNAL-RECORD.                             
04499                                                                   
04500      MOVE WS-BATCH-RECORD        TO PB-BATCH-RECORD.              
04501                                                                   
04502      EXEC CICS REWRITE                                            
04503          DATASET (FILE-ID-ERPNDB)                                 
04504          FROM    (PENDING-BUSINESS)                               
04505      END-EXEC.                                                    
04506                                                                   
04507      MOVE 'C'                    TO JP-RECORD-TYPE.               
04508      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
04509                                                                   
04510      PERFORM 8400-LOG-JOURNAL-RECORD.                             
04511                                                                   
04512      IF WS-ERRORS-PRESENT                                         
04513         GO TO 3900-RE-DISPLAY-RECORD.                             
04514                                                                   
04515      MOVE ZEROS                  TO EMI-ERROR.                    
04516      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
04517      MOVE LOW-VALUES             TO VP631CI.                      
04518      MOVE  -1                    TO CMAINTL.                      
04519      GO TO 8100-SEND-INITIAL-MAP.                                 
04520                                                                   
04521  2280-BATCH-HDR-NOT-FOUND.                                        
04522      MOVE -1                     TO BMAINTL.                      
04523      MOVE ER-7450                TO EMI-ERROR.                    
04524      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
04525      GO TO 8200-SEND-DATAONLY.                                    
04526                                                                   
04527  2290-DUPLICATE-ALT-INDEX.                                        
04528                                                                   
04529 ******************************************************************
04530 *                                                                *
04531 *      IF THE PB-B-HIGHEST-SEQ-NO WAS NOT UPDATED IN THE DATA    *
04532 *         ENTRY SESSION, ADD +1 TO THE HIGHEST SEQUENCE NUMBER   *
04533 *         AND ATTEMPT TO WRITE THE RECORD AGAIN. REPEAT THIS     *
04534 *         50 TIMES.  IF DUPREC CONDITION IS STILL ENCOUNTERED    *
04535 *         AFTER 50 TIMES, ASSUME THE DUPLICATE RECORD WAS        *
04536 *         CAUSED BY A DUPLICATE ALTERNATE KEY.                   *
04537 *                                                                *
04538 ******************************************************************
04539                                                                   
04540      IF LCP-ONCTR-02 =  0                                         
04541          ADD 1 TO LCP-ONCTR-02                                    
04542         MOVE +0                  TO WS-SEQ-NO-COUNTER.            
04543                                                                   
04544      ADD +1                      TO WS-SEQ-NO-COUNTER.            
04545                                                                   
04546      IF  WS-SEQ-NO-COUNTER GREATER THAN +50                       
04547          NEXT SENTENCE                                            
04548      ELSE                                                         
04549          ADD +1 TO PB-BATCH-SEQ-NO                                
04550                    PB-CSR-BATCH-SEQ-NO                            
04551          MOVE PB-BATCH-SEQ-NO TO WS-B-HIGHEST-SEQ-NO              
04552          GO TO 2275-WRITE-PENDING-BUSINESS.                       
04553                                                                   
04554      MOVE -1                     TO CCERTNOL.                     
04555      MOVE AL-UABON               TO CCERTNOA.                     
04556      MOVE AL-UNBON               TO CEFFDTA.                      
04557                                                                   
04558      MOVE 1                      TO EMI-SUB                       
04559                                     EMI-SWITCH1                   
04560                                     EMI-SWITCH-AREA-1             
04561                                     EMI-SWITCH-AREA-2.            
04562                                                                   
04563      MOVE SPACES                 TO EMI-ERROR-LINES.              
04564                                                                   
04565      EXEC CICS SYNCPOINT ROLLBACK                                 
04566      END-EXEC.                                                    
04567                                                                   
04568      MOVE ER-2247                TO EMI-ERROR.                    
04569      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
04570      GO TO 8200-SEND-DATAONLY.                                    
04571                                                                   
04572      EJECT                                                        
04573                                                                   
04574 ******************************************************************
04575 *                                                                *
04576 *          C H A N G E   C A N C E L   R O U T I N E             *
04577 *                                                                *
04578 ******************************************************************
04579                                                                   
04580 ***************************************************************   
04581 *                                                             *   
04582 *     1. GETMAIN FOR PENDING BUSINESS RECORD.                 *   
04583 *     2. SAVE THE BATCH TOTAL DATA FOR UPDATING WITH NEW DATA.*   
04584 *     3. GET THE RECORD TO BE UPDATED AND SEE IF IT HAS BEEN  *   
04585 *              BILLED.  IF IT HAS THEN SAVE A COPY OF THE     *   
04586 *              ZERO RECORD FOR CREATING REVERSAL ENTRIES.     *   
04587 *     4. UPDATE THE RECORD WITH THE SCREEN DATA               *   
04588 *     5. LINK TO THE EDIT ROUTINE.                            *   
04589 *     6. FLAG ALL FIELDS SHOWN IN ERROR BY THE EDIT ROUTINE.  *   
04590 *     7. REDISPLAY SCREEN DATA WITH NEW DATA RETURNED FROM    *   
04591 *              THE EDIT ROUTINE.                              *   
04592 *     7. REWRITE THE NEW ISSUE                                *   
04593 *     8. UPDATE THE BATCH TOTAL RECORD WITH NEW TOTALS.       *   
04594 *                                                             *   
04595 ***************************************************************   
04596  2300-CHANGE-CANCEL-ROUTINE.   
04597      MOVE 'C'                    TO PI-MAINT-FUNCTION.            
04598                                                                   
04599      IF CCHGL NOT = ZEROS                                         
04600         MOVE ER-2196             TO EMI-ERROR                     
04601         MOVE AL-UABON            TO CMAINTA                       
04602         MOVE -1                  TO CMAINTL                       
04603         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
04604         GO TO 8200-SEND-DATAONLY.                                 
04605                                                                   
04606      MOVE CCERTNOI               TO WS-NEW-PRIME.                 
04607                                                                   
04608      IF CSUFIXL NOT = ZEROS                                       
04609         MOVE CSUFIXI             TO WS-NEW-SUFFIX                 
04610      ELSE                                                         
04611         MOVE SPACE               TO WS-NEW-SUFFIX.                
04612                                                                   
04613      MOVE CEFFDTI                TO DEEDIT-FIELD.                 
04614      PERFORM 8600-DEEDIT.                                         
04615      MOVE DEEDIT-FIELD-V0        TO DC-GREG-DATE-1-MDY.           
04616      MOVE '4'                    TO DC-OPTION-CODE.               
04617                                                                   
04618      PERFORM 9700-DATE-LINK.                                      
04619                                                                   
04620      IF DATE-CONVERSION-ERROR                                     
04621         MOVE ER-0348             TO EMI-ERROR                     
04622         MOVE -1                  TO CEFFDTL                       
04623         MOVE AL-UABON            TO CEFFDTA                       
04624         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
04625         GO TO 8200-SEND-DATAONLY                                  
04626      ELSE                                                         
04627         MOVE DC-GREG-DATE-1-EDIT TO CEFFDTI                       
04628         MOVE AL-UANON            TO CEFFDTA                       
04629         MOVE DC-BIN-DATE-1       TO WS-NEW-CERT-EFF-DT.           
04630                                                                   
04631  2325-PROCESS-CANCEL-CHANGES.                                     
04632      EXEC CICS HANDLE CONDITION                                   
04633           NOTFND    (2380-BATCH-HDR-NOT-FOUND)                    
04634      END-EXEC.                                                    
04635                                                                   
04636      MOVE PI-PREV-CONTROL-PRIMARY TO ERPNDB-KEY.                  
04637      MOVE +9999                  TO ERPNDB-BATCH-SEQ-NO.          
04638      MOVE ZEROS                  TO ERPNDB-BATCH-CHG-SEQ-NO.      
04639                                                                   
04640      EXEC CICS GETMAIN                                            
04641          SET      (ADDRESS OF PENDING-BUSINESS)                   
04642          LENGTH   (ERPNDB-RECORD-LENGTH)                          
04643          INITIMG  (GETMAIN-SPACE)                                 
04644       END-EXEC.                                                   
04645                                                                   
04646      EXEC CICS READ                                               
04647          INTO    (PENDING-BUSINESS)                               
04648          DATASET (FILE-ID-ERPNDB)                                 
04649          RIDFLD  (ERPNDB-KEY)                                     
04650      END-EXEC.                                                    
04651                                                                   
04652      MOVE PB-BATCH-RECORD        TO WS-BATCH-RECORD.              
04653                                                                   
04654      EXEC CICS HANDLE CONDITION                                   
04655           NOTFND     (2385-REC-NOT-FOUND)                         
04656      END-EXEC.                                                    
04657                                                                   
04658 **********************************************************        
04659 *      DONT READ THE RECORD FOR UPDATE UNTIL             *        
04660 *      AFTER THE CHANGE RECORD HAS BEEN CREATED.         *        
04661 **********************************************************        
04662                                                                   
04663      EXEC CICS READ                                               
04664          DATASET (FILE-ID-ERPNDB)                                 
04665          INTO    (PENDING-BUSINESS)                               
04666          RIDFLD  (PI-PREV-CONTROL-PRIMARY)                        
04667      END-EXEC.                                                    
04668                                                                   
04669 ********** DO NOT ALLOW CHANGES IF THRU A/R                       
04670      IF BMAINTI = 'C'                                             
04671          IF PB-EL860-INTERNAL-PROCESS                             
04672              MOVE ER-4005          TO EMI-ERROR                   
04673              MOVE -1               TO BMAINTL                     
04674              MOVE AL-UABON         TO BMAINTA                     
04675              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
04676              GO TO 8200-SEND-DATAONLY.                            
04677 **********                                                        
04678                                                                   
04679      IF PB-CREDIT-ACCEPT-DT NOT = LOW-VALUES                      
04680         MOVE -1                 TO CMAINTL                        
04681         MOVE ER-2197            TO EMI-ERROR                      
04682         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
04683         GO TO 8200-SEND-DATAONLY.                                 
04684                                                                   
04685      IF PB-BATCH-CHG-SEQ-NO NOT = ZEROS                           
04686         MOVE ER-2196             TO EMI-ERROR                     
04687         MOVE AL-UABON            TO CMAINTA                       
04688         MOVE -1                  TO CMAINTL                       
04689         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
04690         GO TO 8200-SEND-DATAONLY.                                 

04692      MOVE ZEROS                  TO WS-SEQ-SAVE.                  
04693                                                                   
04694      IF PB-BILLED-DT = LOW-VALUES AND                             
04695         PB-CHG-COUNT = ZEROS                                      
04696         NEXT SENTENCE                                             
04697      ELSE                                                         
04698         IF (CREFND1L GREATER THAN ZEROS AND                       
04699             WS-CREFND1 NOT = PB-C-LF-CANCEL-AMT)                  
04700            OR                                                     
04701            (CREFND2L GREATER THAN ZEROS AND                       
04702             WS-CREFND2 NOT = PB-C-AH-CANCEL-AMT)                  
04703            PERFORM 2700-BUILD-CORRECTION-RECORD THRU 2799-EXIT.   

111513     if ((crefnd1l <> zeros)
111513        and (ws-crefnd1 <> pb-c-lf-cancel-amt))
111513                      or
111513        ((crefnd2l <> zeros)
111513        and (ws-crefnd2 <> pb-c-ah-cancel-amt))
111513        continue
111513     else
111513        go to 2330-continue-change
111513     end-if
111513
031114     move zeros                  to ws-check-amts
111513     move ' '                    to ws-stop-sw
111513                                    ws-check-sw
111513                                    ws-proc-check-sw
111513                                    ws-chek-browse-sw
111513
111513     perform 2500-browse-checks  thru 2500-exit
111513
111513     if i-have-processed-checks
111513        or i-have-checks
111513        continue
111513     else
111513        go to 2330-continue-change
111513     end-if
111513
111513     if crefnd1l <> zeros
111513        compute ws-lf-diff = ws-crefnd1 - pb-c-lf-cancel-amt
111513        move ws-crefnd1 to ws-new-ref-total
111513     else
111513        move pb-c-lf-cancel-amt  to ws-new-ref-total
111513     end-if
111513     if crefnd2l <> zeros
111513        compute ws-ah-diff = ws-crefnd2 - pb-c-ah-cancel-amt
111513        compute ws-new-ref-total = ws-new-ref-total +
111513           ws-crefnd2
111513     else
111513        compute ws-new-ref-total = ws-new-ref-total +
111513           pb-c-ah-cancel-amt
111513     end-if
111513
111513     if zeros = (ws-lf-diff + ws-ah-diff)
111513        move er-3449             to emi-error
111513        perform 9900-error-format thru 9900-exit
111513        go to 2330-continue-change
111513     end-if
111513
111513     if (ws-new-ref-total < ws-check-amts)
051914        and (pb-c-refund-sw = 'Y')
111513        move er-3447             to emi-error
111513        perform 9900-error-format thru 9900-exit
111513        move al-uabon            to cmainta
111513        move -1                  to cmaintl
111513        go to 8200-send-dataonly
111513     end-if
111513
031114     if (ws-new-ref-total > ws-check-amts)
031114        and (ws-check-amts not = zeros)
111513        move er-3448             to emi-error
111513        move al-uabon            to crefnd1a
111513                                    crefnd2a
111513        perform 9900-error-format thru 9900-exit
111513     end-if
111513
111513*     if (ws-lf-diff + ws-ah-diff) < zeros
111513*        move er-3447             to emi-error
111513*        perform 9900-error-format thru 9900-exit
111513*        move al-uabon            to cmainta
111513*        move -1                  to cmaintl
111513*        go to 8200-send-dataonly
111513*     end-if

           .
       2330-continue-change.
04704                                                                   
04705      EXEC CICS READ                                               
04706          DATASET (FILE-ID-ERPNDB)                                 
04707          INTO    (PENDING-BUSINESS)                               
04708          RIDFLD  (PI-PREV-CONTROL-PRIMARY)                        
04709          UPDATE                                                   
04710      END-EXEC.                                                    
04711                                                                   
04712      MOVE FILE-ID-ERPNDB         TO JP-FILE-ID.                   
04713      MOVE 'B'                    TO JP-RECORD-TYPE                
04714      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
04715      MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
04716                                                                   
04717      PERFORM 8400-LOG-JOURNAL-RECORD.                             
04718                                                                   
04719      IF PB-BILLED-DT NOT = LOW-VALUES OR                          
04720           PB-CHG-COUNT NOT = ZEROS                                
04721         IF (CREFND1L GREATER THAN ZEROS AND                       
04722             WS-CREFND1 NOT = PB-C-LF-CANCEL-AMT)                  
04723            OR                                                     
04724            (CREFND2L GREATER THAN ZEROS AND                       
04725             WS-CREFND2 NOT = PB-C-AH-CANCEL-AMT)                  
04726             ADD +1                      TO PB-CHG-COUNT           
04727             MOVE LOW-VALUES             TO PB-BILLED-DT.          
04728                                                                   
072908*    IF CMICRNOL  GREATER  ZEROS                                  
072908*        IF CMICRNOI  NUMERIC                                     
072908*            MOVE CMICRNOI       TO  PB-C-MICROFILM-NO            
072908*            MOVE AL-UNNON       TO  CMICRNOA                     
072908*        ELSE                                                     
072908*            MOVE -1             TO  CMICRNOL                     
072908*            MOVE AL-UNBON       TO  CMICRNOA                     
072908*            MOVE ER-2701        TO  EMI-ERROR                    
072908*            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
04738                                                                   
04739      IF CSUFIXL GREATER THAN ZEROS                                
04740         MOVE CSUFIXI             TO PB-CERT-SFX.                  
04741                                                                   
04742      IF CLSTNML GREATER THAN ZEROS                                
04743          MOVE CLSTNMI            TO PB-C-LAST-NAME.               
04744                                                                   

           IF CCANREAL > ZEROS
              MOVE CCANREAI            TO PB-C-CANCEL-REASON
           END-IF

04745      IF CCANDT1L    GREATER THAN ZEROS                            
04746         MOVE WS-CONVERTED-CANDT1 TO PB-C-LF-CANCEL-DT.            
04747                                                                   
04748      IF CCANDT2L    GREATER THAN ZEROS                            
04749         MOVE WS-CONVERTED-CANDT2 TO PB-C-AH-CANCEL-DT.            
04750                                                                   
04751      IF CMTHD1L    GREATER THAN +0                                
04752         MOVE CMTHD1I             TO PB-C-LF-REFUND-OVERRIDE.      
04753                                                                   
04754      IF CMTHD2L    GREATER THAN +0                                
04755         MOVE CMTHD2I             TO PB-C-AH-REFUND-OVERRIDE.      
04756                                                                   
04757      IF CREFND1L    GREATER THAN ZEROS                            
04758         SUBTRACT PB-C-LF-CANCEL-AMT FROM WS-B-LF-CAN-PRM-ENTERED  
04759         ADD  WS-CREFND1          TO WS-B-LF-CAN-PRM-ENTERED       
04760         MOVE WS-CREFND1          TO PB-C-LF-CANCEL-AMT            
04761         MOVE SPACE               TO PB-C-LF-CALC-REQ.             
04762                                                                   
04763      IF CREFND2L    GREATER THAN ZEROS                            
04764         SUBTRACT PB-C-AH-CANCEL-AMT FROM WS-B-AH-CAN-PRM-ENTERED  
04765         ADD  WS-CREFND2          TO WS-B-AH-CAN-PRM-ENTERED       
04766         MOVE WS-CREFND2          TO PB-C-AH-CANCEL-AMT            
04767         MOVE SPACE               TO PB-C-AH-CALC-REQ.             
04768                                                                   
04769      IF CLIVESL     GREATER THAN ZEROS                            
04770         MOVE WS-CLIVES           TO PB-C-LIVES.                   
04771                                                                   
04772      IF CPAYEEL     GREATER THAN ZEROS                            
04773         MOVE CPAYEEI             TO PB-C-PAYEE-CODE.              
04774                                                                   
04775      IF CFORCEL     GREATER THAN ZEROS                            
04776         MOVE CFORCEI             TO PB-FORCE-CODE.                
04777                                                                   
04778      IF CBILCDL     GREATER THAN ZEROS                            
04779         MOVE CBILCDI             TO PB-RECORD-BILL.               
04780                                                                   
04781  2360-REWRITE-PB-RECORD.                                          
04782      MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY.             
04783      MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.         
04784      MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT.             
04785      MOVE 'C'                    TO JP-RECORD-TYPE.               
04786      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
04787      MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
04788      MOVE FILE-ID-ERPNDB         TO JP-FILE-ID.                   
04789                                                                   
04790      MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY.              
04791                                                                   
04792      SUBTRACT PB-C-LF-REF-CALC   FROM WS-B-LF-CAN-PRM-COMPUTED.   
04793      SUBTRACT PB-C-AH-REF-CALC   FROM WS-B-AH-CAN-PRM-COMPUTED.   
04794                                                                   
04795      PERFORM 9800-LINK-PENDING-EDIT THRU 9800-EXIT.               
04796                                                                   
04797      PERFORM 7300-FORMAT-ERRORS  THRU 7399-EXIT.                  
04798                                                                   
04799      MOVE PB-SV-CARRIER          TO PI-SV-CARRIER.                
04800      MOVE PB-SV-GROUPING         TO PI-SV-GROUPING.               
04801      MOVE PB-SV-STATE            TO PI-SV-STATE.                  
04802                                                                   
04803      ADD PB-C-LF-REF-CALC        TO WS-B-LF-CAN-PRM-COMPUTED.     
04804      ADD PB-C-AH-REF-CALC        TO WS-B-AH-CAN-PRM-COMPUTED.     
04805                                                                   
04806  2375-REWRITE-PENDING-BUSINESS.                                   
04807      MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
04808      MOVE FILE-ID-ERPNDB         TO JP-FILE-ID.                   
04809      MOVE 'C'                    TO JP-RECORD-TYPE.               
04810      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
04811                                                                   
04812      EXEC CICS REWRITE                                            
04813          DATASET (FILE-ID-ERPNDB)                                 
04814          FROM    (PENDING-BUSINESS)                               
04815      END-EXEC.                                                    
04816                                                                   
04817      PERFORM 8400-LOG-JOURNAL-RECORD.                             
04818
071712******************************************************************
071712*       U P D A T E   T H E   O R I G   C E R T   I N F O        *
071712******************************************************************
071712
071712     MOVE PB-CONTROL-BY-ACCOUNT  TO ELCERT-KEY.                   
071712     MOVE PB-SV-CARRIER          TO ELCERT-CARRIER.               
071712     MOVE PB-SV-GROUPING         TO ELCERT-GROUPING.              
071712     MOVE PB-SV-STATE            TO ELCERT-STATE.                 
071712                                                                  
071712     EXEC CICS READ                                               
071712         SET     (ADDRESS OF CERTIFICATE-MASTER)                  
071712         DATASET (FILE-ID-ELCERT)                                 
071712         RIDFLD  (ELCERT-KEY)                                     
071712         RESP    (WS-RESPONSE)
071712     END-EXEC.                                                    
071712
071712     IF NOT RESP-NORMAL
071712        GO TO 2376-BYPASS-CRTO
071712     END-IF
071712
071712
071712     MOVE ELCERT-KEY             TO ERMAIL-KEY
071712     MOVE ' '                    TO WS-ERMAIL-SW
071712
071712     EXEC CICS READ
071712        DATASET   (FILE-ID-ERMAIL)
071712        SET       (ADDRESS OF MAILING-DATA)
071712        RIDFLD    (ERMAIL-KEY)
071712        RESP      (WS-RESPONSE)
071712     END-EXEC
071712
071712     IF RESP-NORMAL
071712        SET ERMAIL-FOUND TO TRUE
071712     END-IF
121712
121712     PERFORM 1295-READ-CERT-TRAILER THRU 1295-EXIT
071712
071712     MOVE PB-CONTROL-BY-ACCOUNT (1:33)
071712                                 TO ELCRTO-KEY
071712     MOVE 'I'                    TO ELCRTO-RECORD-TYPE
071712     MOVE +0                     TO ELCRTO-SEQ-NO
071712
071712     EXEC CICS READ
071712        DATASET   ('ELCRTO')
071712        INTO      (ORIGINAL-CERTIFICATE)
071712        RIDFLD    (ELCRTO-KEY)
071712        GTEQ
071712        RESP      (WS-RESPONSE)
071712     END-EXEC
071712     
071712     IF RESP-NORMAL
071712        AND (OC-CONTROL-PRIMARY (1:33) =
071712                 PB-CONTROL-BY-ACCOUNT (1:33))
071712        AND (OC-RECORD-TYPE = 'I')
071712        display ' resp norm, key =, type i '
071712        IF (OC-ENDORSEMENT-PROCESSED-DT = LOW-VALUES)
071712           display ' end proc blank '
071712           display ' csr edit session ' PI-CSR-SESSION-SW
071712           IF NOT CSR-EDIT-SESSION
071712              EXEC CICS READ
071712                 DATASET   ('ELCRTO')
071712                 INTO      (ORIGINAL-CERTIFICATE)
071712                 RIDFLD    (OC-CONTROL-PRIMARY)
071712                 UPDATE
071712                 RESP      (WS-RESPONSE)
071712              END-EXEC
071712              DISPLAY ' JUST DID READ UPD ' WS-RESPONSE
071712              IF RESP-NORMAL
071712                 display ' good read upd '
011413                 IF OC-ISSUE-TRAN-IND = 'N'
071712                    MOVE CM-INSURED-LAST-NAME   TO 
071712                         OC-INS-LAST-NAME   
071712                    MOVE CM-INSURED-FIRST-NAME  TO 
071712                         OC-INS-FIRST-NAME  
071712                    MOVE CM-INSURED-INITIAL2    TO 
071712                         OC-INS-MIDDLE-INIT 
071712                    MOVE CM-INSURED-ISSUE-AGE   TO 
071712                         OC-INS-AGE         
071712                    MOVE CM-JT-LAST-NAME        TO 
071712                         OC-JNT-LAST-NAME   
071712                    MOVE CM-JT-FIRST-NAME       TO 
071712                         OC-JNT-FIRST-NAME  
071712                    MOVE CM-JT-INITIAL          TO 
071712                         OC-JNT-MIDDLE-INIT 
071712                    MOVE CM-INSURED-JOINT-AGE   TO 
071712                         OC-JNT-AGE         
071712                    MOVE CM-LF-BENEFIT-CD       TO 
071712                         OC-LF-BENCD        
071712                    MOVE CM-LF-ORIG-TERM        TO 
071712                         OC-LF-TERM         
071712                    MOVE CM-LF-BENEFIT-AMT      TO 
071712                         OC-LF-BEN-AMT      
071712                    MOVE CM-LF-PREMIUM-AMT      TO 
071712                         OC-LF-PRM-AMT
071712                    MOVE CM-LF-ALT-BENEFIT-AMT  TO 
071712                         OC-LF-ALT-BEN-AMT  
071712                    MOVE CM-LF-ALT-PREMIUM-AMT  TO 
071712                         OC-LF-ALT-PRM-AMT  
071712                    MOVE CM-LF-LOAN-EXPIRE-DT   TO 
071712                         OC-LF-EXP-DT       
071712                    MOVE CM-LIFE-COMM-PCT       TO 
071712                         OC-LF-COMM-PCT     
071712                    MOVE CM-AH-BENEFIT-CD       TO 
071712                         OC-AH-BENCD        
071712                    MOVE CM-AH-ORIG-TERM        TO 
071712                         OC-AH-TERM         
071712                    MOVE CM-AH-BENEFIT-AMT      TO 
071712                         OC-AH-BEN-AMT      
071712                    MOVE CM-AH-PREMIUM-AMT      TO 
071712                         OC-AH-PRM-AMT      
071712                    MOVE CM-AH-LOAN-EXPIRE-DT   TO 
071712                         OC-AH-EXP-DT       
071712                    MOVE CM-AH-COMM-PCT         TO 
071712                         OC-AH-COMM-PCT     
071712                    MOVE CM-AH-CRITICAL-PERIOD  TO 
071712                         OC-AH-CP
071712                    MOVE CM-LOAN-1ST-PMT-DT     TO 
071712                         OC-1ST-PMT-DT
011413                 END-IF
071712                 MOVE PB-C-LF-CANCEL-DT      TO 
071712                         OC-LF-CANCEL-DT
071712                 MOVE PB-C-LF-CANCEL-AMT     TO 
071712                         OC-LF-CANCEL-AMT
071712                 MOVE CM-LF-ITD-CANCEL-AMT   TO 
071712                         OC-LF-ITD-CANCEL-AMT
071712                 MOVE PB-C-AH-CANCEL-DT      TO 
071712                         OC-AH-CANCEL-DT
071712                 MOVE PB-C-AH-CANCEL-AMT     TO 
071712                         OC-AH-CANCEL-AMT
071712                 MOVE CM-AH-ITD-CANCEL-AMT   TO 
071712                         OC-AH-ITD-CANCEL-AMT
011413                 MOVE 'Y'                    TO
011413                         OC-CANCEL-TRAN-IND
071712                 MOVE PI-PROCESSOR-ID  TO OC-LAST-MAINT-BY
071712                 MOVE EIBTIME          TO OC-LAST-MAINT-HHMMSS
071712                 MOVE WS-CURRENT-BIN-DT TO OC-LAST-MAINT-DT
071712                 IF ERMAIL-FOUND
071712                    MOVE MA-CRED-BENE-NAME
071712                                 TO OC-CRED-BENE-NAME
071712                 END-IF
121712                 IF NOT CERT-TRL-REC-NOT-FOUND
121712                    MOVE CS-INS-AGE-DEFAULT-FLAG TO 
121712                           OC-INS-AGE-DEFAULT-FLAG
121712                    MOVE CS-JNT-AGE-DEFAULT-FLAG TO 
121712                           OC-JNT-AGE-DEFAULT-FLAG
121712                 END-IF
071712                 EXEC CICS REWRITE
071712                    DATASET   ('ELCRTO')
071712                    FROM      (ORIGINAL-CERTIFICATE)
071712                    RESP      (WS-RESPONSE)
071712                 END-EXEC
071712                 display ' just rewrote ' ws-response
071712              END-IF
071712           END-IF
071712        ELSE
071712           SUBTRACT +1 FROM OC-KEY-SEQ-NO
071712           MOVE 'OC'             TO OC-RECORD-ID
071712           MOVE PI-PROCESSOR-ID  TO OC-LAST-MAINT-BY
071712           MOVE EIBTIME          TO OC-LAST-MAINT-HHMMSS
071712           MOVE WS-CURRENT-BIN-DT TO OC-LAST-MAINT-DT
071712           MOVE CM-INSURED-LAST-NAME   TO OC-INS-LAST-NAME   
071712           MOVE CM-INSURED-FIRST-NAME  TO OC-INS-FIRST-NAME  
071712           MOVE CM-INSURED-INITIAL2    TO OC-INS-MIDDLE-INIT 
071712           MOVE CM-INSURED-ISSUE-AGE   TO OC-INS-AGE         
071712           MOVE CM-JT-LAST-NAME        TO OC-JNT-LAST-NAME   
071712           MOVE CM-JT-FIRST-NAME       TO OC-JNT-FIRST-NAME  
071712           MOVE CM-JT-INITIAL          TO OC-JNT-MIDDLE-INIT 
071712           MOVE CM-INSURED-JOINT-AGE   TO OC-JNT-AGE         
071712           MOVE CM-LF-BENEFIT-CD       TO OC-LF-BENCD        
071712           MOVE CM-LF-ORIG-TERM        TO OC-LF-TERM         
071712           MOVE CM-LF-BENEFIT-AMT      TO OC-LF-BEN-AMT      
071712           MOVE CM-LF-PREMIUM-AMT      TO OC-LF-PRM-AMT      
071712           MOVE CM-LF-ALT-BENEFIT-AMT  TO OC-LF-ALT-BEN-AMT  
071712           MOVE CM-LF-ALT-PREMIUM-AMT  TO OC-LF-ALT-PRM-AMT  
071712           MOVE CM-LF-LOAN-EXPIRE-DT   TO OC-LF-EXP-DT       
071712           MOVE CM-LIFE-COMM-PCT       TO OC-LF-COMM-PCT     
071712           MOVE PB-C-LF-CANCEL-DT      TO OC-LF-CANCEL-DT
071712           MOVE PB-C-LF-CANCEL-AMT     TO OC-LF-CANCEL-AMT
071712           MOVE CM-LF-ITD-CANCEL-AMT   TO OC-LF-ITD-CANCEL-AMT
071712           MOVE CM-AH-BENEFIT-CD       TO OC-AH-BENCD        
071712           MOVE CM-AH-ORIG-TERM        TO OC-AH-TERM         
071712           MOVE CM-AH-BENEFIT-AMT      TO OC-AH-BEN-AMT      
071712           MOVE CM-AH-PREMIUM-AMT      TO OC-AH-PRM-AMT      
071712           MOVE CM-AH-LOAN-EXPIRE-DT   TO OC-AH-EXP-DT       
071712           MOVE CM-AH-COMM-PCT         TO OC-AH-COMM-PCT     
071712           MOVE CM-AH-CRITICAL-PERIOD  TO OC-AH-CP
071712           MOVE PB-C-AH-CANCEL-DT      TO OC-AH-CANCEL-DT
071712           MOVE PB-C-AH-CANCEL-AMT     TO OC-AH-CANCEL-AMT
071712           MOVE CM-AH-ITD-CANCEL-AMT   TO OC-AH-ITD-CANCEL-AMT
071712           MOVE CM-LOAN-1ST-PMT-DT     TO OC-1ST-PMT-DT
011413           MOVE 'N'                    TO OC-ISSUE-TRAN-IND
011413           MOVE 'Y'                    TO OC-CANCEL-TRAN-IND
071712           IF ERMAIL-FOUND
071712              MOVE MA-CRED-BENE-NAME 
071712                                 TO OC-CRED-BENE-NAME
071712           END-IF
121712           IF NOT CERT-TRL-REC-NOT-FOUND
121712              MOVE CS-INS-AGE-DEFAULT-FLAG TO 
121712                           OC-INS-AGE-DEFAULT-FLAG
121712              MOVE CS-JNT-AGE-DEFAULT-FLAG TO 
121712                           OC-JNT-AGE-DEFAULT-FLAG
121712           END-IF
071712           MOVE LOW-VALUES       TO OC-ENDORSEMENT-PROCESSED-DT
071712           EXEC CICS WRITE
071712              DATASET   ('ELCRTO')
071712              FROM      (ORIGINAL-CERTIFICATE)
071712              RIDFLD    (OC-CONTROL-PRIMARY)
071712              RESP      (WS-RESPONSE)
071712           END-EXEC
071712        END-IF
071712     ELSE
071712        MOVE SPACES              TO ORIGINAL-CERTIFICATE
071712        MOVE 'OC'                TO OC-RECORD-ID
071712        MOVE PB-CONTROL-BY-ACCOUNT (1:33)
071712                                 TO OC-CONTROL-PRIMARY (1:33)
071712        MOVE 'I'                 TO OC-RECORD-TYPE
071712        MOVE +4096               TO OC-KEY-SEQ-NO
071712        MOVE PI-PROCESSOR-ID     TO OC-LAST-MAINT-BY
071712        MOVE EIBTIME             TO OC-LAST-MAINT-HHMMSS
071712        MOVE WS-CURRENT-BIN-DT   TO OC-LAST-MAINT-DT
071712        MOVE CM-INSURED-LAST-NAME   TO OC-INS-LAST-NAME   
071712        MOVE CM-INSURED-FIRST-NAME  TO OC-INS-FIRST-NAME  
071712        MOVE CM-INSURED-INITIAL2    TO OC-INS-MIDDLE-INIT 
071712        MOVE CM-INSURED-ISSUE-AGE   TO OC-INS-AGE         
071712        MOVE CM-JT-LAST-NAME        TO OC-JNT-LAST-NAME   
071712        MOVE CM-JT-FIRST-NAME       TO OC-JNT-FIRST-NAME  
071712        MOVE CM-JT-INITIAL          TO OC-JNT-MIDDLE-INIT 
071712        MOVE CM-INSURED-JOINT-AGE   TO OC-JNT-AGE         
071712        MOVE CM-LF-BENEFIT-CD       TO OC-LF-BENCD        
071712        MOVE CM-LF-ORIG-TERM        TO OC-LF-TERM         
071712        MOVE CM-LF-BENEFIT-AMT      TO OC-LF-BEN-AMT      
071712        MOVE CM-LF-PREMIUM-AMT      TO OC-LF-PRM-AMT      
071712        MOVE CM-LF-ALT-BENEFIT-AMT  TO OC-LF-ALT-BEN-AMT  
071712        MOVE CM-LF-ALT-PREMIUM-AMT  TO OC-LF-ALT-PRM-AMT  
071712        MOVE CM-LF-LOAN-EXPIRE-DT   TO OC-LF-EXP-DT       
071712        MOVE CM-LIFE-COMM-PCT       TO OC-LF-COMM-PCT     
071712        MOVE PB-C-LF-CANCEL-DT      TO OC-LF-CANCEL-DT
071712        MOVE PB-C-LF-CANCEL-AMT     TO OC-LF-CANCEL-AMT
071712        MOVE CM-LF-ITD-CANCEL-AMT   TO OC-LF-ITD-CANCEL-AMT
071712        MOVE CM-AH-BENEFIT-CD       TO OC-AH-BENCD        
071712        MOVE CM-AH-ORIG-TERM        TO OC-AH-TERM         
071712        MOVE CM-AH-BENEFIT-AMT      TO OC-AH-BEN-AMT      
071712        MOVE CM-AH-PREMIUM-AMT      TO OC-AH-PRM-AMT      
071712        MOVE CM-AH-LOAN-EXPIRE-DT   TO OC-AH-EXP-DT       
071712        MOVE CM-AH-COMM-PCT         TO OC-AH-COMM-PCT     
071712        MOVE CM-AH-CRITICAL-PERIOD  TO OC-AH-CP
071712        MOVE PB-C-AH-CANCEL-DT      TO OC-AH-CANCEL-DT
071712        MOVE PB-C-AH-CANCEL-AMT     TO OC-AH-CANCEL-AMT
071712        MOVE CM-AH-ITD-CANCEL-AMT   TO OC-AH-ITD-CANCEL-AMT
071712        MOVE CM-LOAN-1ST-PMT-DT     TO OC-1ST-PMT-DT
011413        MOVE 'N'                    TO OC-ISSUE-TRAN-IND
011413        MOVE 'Y'                    TO OC-CANCEL-TRAN-IND
071712        IF ERMAIL-FOUND
071712           MOVE MA-CRED-BENE-NAME 
071712                                 TO OC-CRED-BENE-NAME
071712        END-IF
121712        IF NOT CERT-TRL-REC-NOT-FOUND
121712           MOVE CS-INS-AGE-DEFAULT-FLAG TO 
121712                           OC-INS-AGE-DEFAULT-FLAG
121712           MOVE CS-JNT-AGE-DEFAULT-FLAG TO 
121712                           OC-JNT-AGE-DEFAULT-FLAG
121712        END-IF
071712        MOVE LOW-VALUES          TO OC-ENDORSEMENT-PROCESSED-DT
071712
071712        EXEC CICS WRITE
071712           DATASET   ('ELCRTO')
071712           FROM      (ORIGINAL-CERTIFICATE)
071712           RIDFLD    (OC-CONTROL-PRIMARY)
071712           RESP      (WS-RESPONSE)
071712        END-EXEC
071712     END-IF.
071712     
071712 2376-BYPASS-CRTO.
04819 ******************************************************************
04820 *       U P D A T E   T H E   B A T C H   H E A D E R            *
04821 ******************************************************************
04822                                                                   
04823      EXEC CICS HANDLE CONDITION                                   
04824           NOTFND    (2380-BATCH-HDR-NOT-FOUND)                    
04825      END-EXEC.                                                    
04826                                                                   
04827      EXEC CICS READ                                               
04828          INTO    (PENDING-BUSINESS)                               
04829          DATASET (FILE-ID-ERPNDB)                                 
04830          RIDFLD  (ERPNDB-KEY)                                     
04831          UPDATE                                                   
04832      END-EXEC.                                                    
04833                                                                   
04834      MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
04835      MOVE FILE-ID-ERPNDB         TO JP-FILE-ID.                   
04836      MOVE 'B'                    TO JP-RECORD-TYPE.               
04837      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
04838                                                                   
04839      PERFORM 8400-LOG-JOURNAL-RECORD.                             
04840                                                                   
04841      MOVE WS-BATCH-RECORD        TO PB-BATCH-RECORD.              
04842                                                                   
04843      EXEC CICS REWRITE                                            
04844          DATASET (FILE-ID-ERPNDB)                                 
04845          FROM    (PENDING-BUSINESS)                               
04846      END-EXEC.                                                    
04847                                                                   
04848      MOVE 'C'                    TO JP-RECORD-TYPE.               
04849      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
04850                                                                   
04851      PERFORM 8400-LOG-JOURNAL-RECORD.                             
04852                                                                   
04853      IF WS-ERRORS-PRESENT                                         
04854          GO TO 3900-RE-DISPLAY-RECORD.                            
04855                                                                   
04856      MOVE ZEROS                  TO EMI-ERROR                     
04857      MOVE -1                     TO CMAINTL                       
04858      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     
04859      GO TO 3900-RE-DISPLAY-RECORD.                                
04860                                                                   
04861  2380-BATCH-HDR-NOT-FOUND.                                        
04862      MOVE -1                     TO BMAINTL.                      
04863      MOVE ER-7450                TO EMI-ERROR.                    
04864      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
04865      GO TO 8200-SEND-DATAONLY.                                    
04866                                                                   
04867  2385-REC-NOT-FOUND.                                              
04868      MOVE -1                     TO BMAINTL.                      
04869      MOVE ER-7451                TO EMI-ERROR.                    
04870      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
04871      GO TO 8200-SEND-DATAONLY.                                    
04872                                                                   
04873      EJECT                                                        
04874                                                                   
04875 ******************************************************************
04876 *                                                                *
04877 *          D E L E T E   C A N C E L   R O U T I N E             *
04878 *                                                                *
04879 ******************************************************************
04880                                                                   
04881  2400-DELETE-CANCEL-ROUTINE.                                      
04882      MOVE 'D'                    TO PI-MAINT-FUNCTION.            
04883                                                                   
04884      IF CCHGL NOT = ZEROS                                         
04885         MOVE ER-2196             TO EMI-ERROR                     
04886         MOVE AL-UABON            TO CMAINTA                       
04887         MOVE -1                  TO CMAINTL                       
04888         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
04889         GO TO 8200-SEND-DATAONLY.                                 
04890                                                                   
04891      EXEC CICS HANDLE CONDITION                                   
04892           NOTFND    (2485-BATCH-HDR-NOT-FOUND)                    
04893      END-EXEC.                                                    
04894                                                                   
04895      MOVE PI-PREV-CONTROL-PRIMARY TO ERPNDB-KEY.                  
04896      MOVE +9999                  TO ERPNDB-BATCH-SEQ-NO.          
04897      MOVE ZEROS                  TO ERPNDB-BATCH-CHG-SEQ-NO.      
04898                                                                   
04899      EXEC CICS READ                                               
04900          SET     (ADDRESS OF PENDING-BUSINESS)                    
04901          DATASET (FILE-ID-ERPNDB)                                 
04902          RIDFLD  (ERPNDB-KEY)                                     
04903      END-EXEC.                                                    
04904                                                                   
04905      MOVE PB-BATCH-RECORD        TO WS-BATCH-RECORD.              
04906                                                                   
04907      MOVE CCERTNOI               TO WS-NEW-PRIME.                 
04908                                                                   
04909      IF CSUFIXL NOT = ZEROS                                       
04910         MOVE CSUFIXI             TO WS-NEW-SUFFIX                 
04911      ELSE                                                         
04912         MOVE SPACE               TO WS-NEW-SUFFIX.                
04913                                                                   
04914      MOVE CEFFDTI                TO DEEDIT-FIELD.                 
04915                                                                   
04916      PERFORM 8600-DEEDIT.                                         
04917      MOVE DEEDIT-FIELD-V0        TO DC-GREG-DATE-1-MDY.           
04918      MOVE '4'                    TO DC-OPTION-CODE.               
04919      PERFORM 9700-DATE-LINK.                                      
04920                                                                   
04921      IF DATE-CONVERSION-ERROR                                     
04922         MOVE ER-0348             TO EMI-ERROR                     
04923         MOVE -1                  TO CEFFDTL                       
04924         MOVE AL-UABON            TO CEFFDTA                       
04925         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
04926         GO TO 8200-SEND-DATAONLY                                  
04927      ELSE                                                         
04928         MOVE DC-GREG-DATE-1-EDIT TO CEFFDTI                       
04929         MOVE AL-UANON            TO CEFFDTA                       
04930         MOVE DC-BIN-DATE-1       TO WS-NEW-CERT-EFF-DT.           
04931                                                                   
04932      EXEC CICS HANDLE CONDITION                                   
04933           NOTFND    (2490-RECORD-NOT-FOUND)                       
04934      END-EXEC.                                                    
04935                                                                   
04936      EXEC CICS READ                                               
04937          SET     (ADDRESS OF PENDING-BUSINESS)                    
04938          DATASET (FILE-ID-ERPNDB)                                 
04939          RIDFLD  (PI-PREV-CONTROL-PRIMARY)                        
04940          UPDATE                                                   
04941      END-EXEC.                                                    
04942                                                                   
04943      IF PB-CREDIT-ACCEPT-DT NOT = LOW-VALUES                      
04944         MOVE -1                 TO CMAINTL                        
04945         MOVE ER-2197            TO EMI-ERROR                      
04946         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
04947         GO TO 8200-SEND-DATAONLY.                                 
04948                                                                   
04949      IF PB-BILLED-DT NOT = LOW-VALUES                             
04950         MOVE -1                 TO CMAINTL                        
04951         MOVE ER-2129            TO EMI-ERROR                      
04952         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
04953         GO TO 8200-SEND-DATAONLY.                                 
04954                                                                   
04955      IF PB-CHG-COUNT NOT = ZEROS                                  
04956         MOVE ER-2336            TO EMI-ERROR                      
04957         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
04958         MOVE AL-UABON            TO CMAINTA                       
04959         MOVE -1                  TO CMAINTL                       
04960         GO TO 8200-SEND-DATAONLY.                                 
04961                                                                   
04962      IF PB-BATCH-CHG-SEQ-NO NOT = ZEROS OR                        
04963         PB-CHG-COUNT NOT = ZEROS                                  
04964         MOVE ER-2196             TO EMI-ERROR                     
04965         MOVE AL-UABON            TO CMAINTA                       
04966         MOVE -1                  TO CMAINTL                       
04967         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
04968         GO TO 8200-SEND-DATAONLY.                                 
04969                                                                   
04970      IF PB-CONFIRMATION-REPT-DT NOT = SPACES AND LOW-VALUES       
04971         MOVE ER-2900 TO EMI-ERROR                                 
04972         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
04973         MOVE AL-UABON         TO CMAINTA                          
04974         MOVE -1               TO CMAINTL                          
04975         GO TO 8200-SEND-DATAONLY.                                 

111513     move ' '                    to ws-stop-sw
111513                                    ws-check-sw
111513                                    ws-proc-check-sw
111513                                    ws-chek-browse-sw
111513
111513     perform 2500-browse-checks  thru 2500-exit
111513     if (i-have-processed-checks)
051914        and (pb-c-refund-sw = 'Y')
111513        move er-3446             to emi-error
111513        perform 9900-error-format thru 9900-exit
111513        move al-uabon            to cmainta
111513        move -1                  to cmaintl
111513        go to 8200-send-dataonly
111513     end-if

04977      EXEC CICS HANDLE CONDITION                                   
04978           NOTFND  (2475-CONTINUE-DELETE)                          
04979      END-EXEC                                                     
04980                                                                   
04981      MOVE PB-CONTROL-BY-ACCOUNT  TO ELCERT-KEY.                   
04982      MOVE PB-SV-CARRIER          TO ELCERT-CARRIER.               
04983      MOVE PB-SV-GROUPING         TO ELCERT-GROUPING.              
04984      MOVE PB-SV-STATE            TO ELCERT-STATE.                 
04985                                                                   
04986      EXEC CICS READ                                               
04987          SET     (ADDRESS OF CERTIFICATE-MASTER)                  
04988          DATASET (FILE-ID-ELCERT)                                 
04989          RIDFLD  (ELCERT-KEY)                                     
04990          UPDATE                                                   
04991      END-EXEC.                                                    
04992                                                                   
04993      MOVE ELCERT-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
04994                                                                   
04995      MOVE FILE-ID-ELCERT         TO JP-FILE-ID.                   
04996      MOVE 'B'                    TO JP-RECORD-TYPE.               
04997      MOVE CERTIFICATE-MASTER     TO JP-RECORD-AREA.               
04998      PERFORM 8400-LOG-JOURNAL-RECORD.                             
04999                                                                   
05000      IF PB-CI-LIVES NOT NUMERIC                                   
05001          MOVE ZERO                    TO PB-CI-LIVES.             
05002                                                                   
05003      IF PB-C-LF-CANCEL-DT GREATER THAN LOW-VALUES                 
05004         IF PB-CI-LF-PRIOR-CANCEL-DT = LOW-VALUES                  
05005            MOVE SPACE                  TO CM-CREDIT-INTERFACE-SW-2
05006            MOVE ZERO                     TO CM-LF-ITD-CANCEL-AMT  
05007            MOVE LOW-VALUES               TO CM-LF-CANCEL-EXIT-DT  
05008                                             CM-LF-CANCEL-DT       
05009            MOVE SPACE                    TO CM-LF-EXIT-BATCH      
05010         ELSE                                                      
05011            MOVE SPACE                  TO CM-CREDIT-INTERFACE-SW-2
05012            MOVE PB-CI-LF-CANCEL-AMT      TO CM-LF-ITD-CANCEL-AMT  
05013            MOVE PB-CI-LF-PRIOR-CANCEL-DT TO CM-LF-CANCEL-DT       
05014            MOVE PB-CI-LIVES              TO CM-LIVES.             
05015                                                                   
05016      IF PB-C-LF-CANCEL-DT GREATER THAN LOW-VALUES                 
05017         IF PB-CI-LF-PRIOR-CANCEL-DT = LOW-VALUES                  
05018            IF CM-LF-STATUS-AT-CANCEL NOT EQUAL TO SPACE           
05019                MOVE CM-LF-STATUS-AT-CANCEL                        
05020                                         TO CM-LF-CURRENT-STATUS   
05021                MOVE SPACE                                         
05022                                         TO CM-LF-STATUS-AT-CANCEL 
05023            ELSE                                                   
05024                MOVE PB-CI-LF-POLICY-STATUS                        
05025                                         TO CM-LF-CURRENT-STATUS   
05026         ELSE                                                      
05027            MOVE PB-CI-LF-POLICY-STATUS  TO CM-LF-CURRENT-STATUS.  
05028                                                                   
05029                                                                   
05030      IF PB-C-AH-CANCEL-DT GREATER THAN LOW-VALUES                 
05031         IF PB-CI-AH-PRIOR-CANCEL-DT = LOW-VALUES                  
05032            MOVE SPACE                  TO CM-CREDIT-INTERFACE-SW-2
05033            MOVE ZERO                     TO CM-AH-ITD-CANCEL-AMT  
05034            MOVE LOW-VALUES               TO CM-AH-CANCEL-EXIT-DT  
05035                                             CM-AH-CANCEL-DT       
05036            MOVE SPACE                    TO CM-AH-EXIT-BATCH      
05037         ELSE                                                      
05038            MOVE SPACE                  TO CM-CREDIT-INTERFACE-SW-2
05039            MOVE PB-CI-AH-CANCEL-AMT      TO CM-AH-ITD-CANCEL-AMT  
05040            MOVE PB-CI-AH-PRIOR-CANCEL-DT TO CM-AH-CANCEL-DT       
05041            MOVE PB-CI-LIVES              TO CM-LIVES.             
05042                                                                   
05043      IF PB-C-AH-CANCEL-DT GREATER THAN LOW-VALUES                 
05044         IF PB-CI-AH-PRIOR-CANCEL-DT = LOW-VALUES                  
05045            IF CM-AH-STATUS-AT-CANCEL NOT EQUAL TO SPACE           
05046                MOVE CM-AH-STATUS-AT-CANCEL                        
05047                                         TO CM-AH-CURRENT-STATUS   
05048                MOVE SPACE                                         
05049                                         TO CM-AH-STATUS-AT-CANCEL 
05050            ELSE                                                   
05051                MOVE PB-CI-AH-POLICY-STATUS                        
05052                                         TO CM-AH-CURRENT-STATUS   
05053         ELSE                                                      
05054            MOVE PB-CI-AH-POLICY-STATUS  TO CM-AH-CURRENT-STATUS.  
05055                                                                   
05056  2450-REWRITE-CERT.                                               
05057      MOVE  FILE-ID-ELCERT          TO JP-FILE-ID.                 
05058      MOVE 'C'                      TO JP-RECORD-TYPE.             
05059      MOVE CERTIFICATE-MASTER       TO JP-RECORD-AREA.             
05060                                                                   
05061      EXEC CICS REWRITE                                            
05062          DATASET  (FILE-ID-ELCERT)                                
05063          FROM     (CERTIFICATE-MASTER)                            
05064      END-EXEC.                                                    
05065                                                                   
05066      MOVE ELCERT-JOURNAL-LENGTH    TO WS-JOURNAL-RECORD-LENGTH.   
05067                                                                   
05068      PERFORM 8400-LOG-JOURNAL-RECORD.                             
05069                                                                   

111513     move ' '                    to ws-stop-sw
111513                                    ws-proc-check-sw
030314                                    ws-chek-browse-sw
111513
111513     if i-have-checks
111513        perform until i-say-stop
111513           perform 2510-startbr-erchek
111513                                 thru 2510-exit
111513           if resp-normal
111513              perform 2520-readnext-erchek
111513                                 thru 2520-exit
111513           end-if
111513           if (not resp-normal)
111513              or (pb-control-by-account (1:33) not =
111513                 ch-control-primary (1:33))
111513                 set i-say-stop to true
111513           else
111513              perform 2530-readupd-erchek
111513                                 thru 2530-exit
111513              if resp-normal
111513                 perform 2540-delete-erchek
111513                                 thru 2540-exit
111513              end-if
111513              if ws-chek-browse-sw = 'Y'
111513                 perform 2525-endbr-erchek
111513                                 thru 2525-exit
111513                 move ' '        to ws-chek-browse-sw
111513              end-if
111513           end-if
111513        end-perform
111513     end-if
111513
111513     if ws-chek-browse-sw = 'Y'
111513        perform 2525-endbr-erchek thru 2525-exit
111513     end-if

           .
05070  2475-CONTINUE-DELETE.                                            
05071      SUBTRACT PB-C-LF-CANCEL-AMT  FROM WS-B-LF-CAN-PRM-ENTERED.   
05072      SUBTRACT PB-C-AH-CANCEL-AMT  FROM WS-B-AH-CAN-PRM-ENTERED.   
05073                                                                   
05074      SUBTRACT PB-C-LF-REF-CALC    FROM WS-B-LF-CAN-PRM-COMPUTED.  
05075      SUBTRACT PB-C-AH-REF-CALC    FROM WS-B-AH-CAN-PRM-COMPUTED.  
05076                                                                   
05077      SUBTRACT +1 FROM WS-B-CANCEL-CNT-ENTERED.                    
05078                                                                   
05079      MOVE FILE-ID-ERPNDB         TO JP-FILE-ID                    
05080      MOVE 'D'                    TO JP-RECORD-TYPE                
05081      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
05082                                                                   
05083      EXEC CICS DELETE                                             
05084          DATASET    (FILE-ID-ERPNDB)                              
05085      END-EXEC.                                                    
05086                                                                   
05087      MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
05088                                                                   
05089      PERFORM 8400-LOG-JOURNAL-RECORD.                             
05090                                                                   
05091      MOVE +9999                  TO ERPNDB-BATCH-SEQ-NO.          
05092      MOVE ZEROS                  TO ERPNDB-BATCH-CHG-SEQ-NO.      
05093                                                                   
05094      EXEC CICS HANDLE CONDITION                                   
05095           NOTFND    (2485-BATCH-HDR-NOT-FOUND)                    
05096      END-EXEC.                                                    
05097                                                                   
05098      EXEC CICS READ                                               
05099          SET     (ADDRESS OF PENDING-BUSINESS)                    
05100          DATASET (FILE-ID-ERPNDB)                                 
05101          RIDFLD  (ERPNDB-KEY)                                     
05102          UPDATE                                                   
05103      END-EXEC.                                                    
05104                                                                   
05105      MOVE FILE-ID-ERPNDB           TO JP-FILE-ID.                 
05106      MOVE ERPNDB-JOURNAL-LENGTH    TO WS-JOURNAL-RECORD-LENGTH.   
05107                                                                   
05108      IF WS-B-ISSUE-CNT-ENTERED  = ZEROS AND                       
05109         WS-B-CANCEL-CNT-ENTERED = ZEROS                           
05110         MOVE 'D'                 TO JP-RECORD-TYPE                
05111         MOVE PENDING-BUSINESS    TO JP-RECORD-AREA                
05112         EXEC CICS DELETE                                          
05113              DATASET    (FILE-ID-ERPNDB)                          
05114         END-EXEC                                                  
05115         PERFORM 8400-LOG-JOURNAL-RECORD                           
05116      ELSE                                                         
05117         MOVE 'B'                 TO JP-RECORD-TYPE                
05118         MOVE PENDING-BUSINESS    TO JP-RECORD-AREA                
05119         PERFORM 8400-LOG-JOURNAL-RECORD                           
05120         MOVE WS-BATCH-RECORD     TO PB-BATCH-RECORD               
05121         MOVE 'C'                 TO JP-RECORD-TYPE                
05122         MOVE PENDING-BUSINESS    TO JP-RECORD-AREA                
05123         EXEC CICS REWRITE                                         
05124              DATASET    (FILE-ID-ERPNDB)                          
05125              FROM       (PENDING-BUSINESS)                        
05126         END-EXEC                                                  
05127         PERFORM 8400-LOG-JOURNAL-RECORD.                          
05128                                                                   
05129 ******************************************************************
05130 *                                                                *
05131 *       DELETE THE A/R REQUEST RECORD IF THE COMPANY USES        *
05132 *       THE ACCOUNTS RECEIVABLE SYSTEM.                          *
05133 *                                                                *
05134 ******************************************************************
05135                                                                   
05136      IF NOT PI-AR-PROCESSING                                      
05137         GO TO 2480-DELETE-FINISHED.                               
05138                                                                   
05139      IF WS-B-ISSUE-CNT-ENTERED  = ZEROS AND                       
05140         WS-B-CANCEL-CNT-ENTERED = ZEROS                           
05141         NEXT SENTENCE                                             
05142      ELSE                                                         
05143         GO TO 2480-DELETE-FINISHED.                               
05144                                                                   
05145      EXEC CICS HANDLE CONDITION                                   
05146           NOTFND    (2480-DELETE-FINISHED)                        
05147      END-EXEC.                                                    
05148                                                                   
05149      MOVE ERPNDB-COMPANY-CD      TO ERRQST-COMPANY-CD.            
05150      MOVE ERPNDB-ENTRY-BATCH     TO ERRQST-ENTRY-BATCH.           
05151                                                                   
05152      EXEC CICS READ                                               
05153          SET     (ADDRESS OF AR-REQUEST-RECORD)                   
05154          DATASET (FILE-ID-ERRQST)                                 
05155          RIDFLD  (ERRQST-KEY)                                     
05156          UPDATE                                                   
05157      END-EXEC.                                                    
05158                                                                   
05159      MOVE ERRQST-JOURNAL-LENGTH    TO WS-JOURNAL-RECORD-LENGTH.   
05160                                                                   
05161      MOVE  FILE-ID-ERRQST          TO JP-FILE-ID.                 
05162                                                                   
05163      MOVE 'D'                      TO JP-RECORD-TYPE.             
05164      MOVE AR-REQUEST-RECORD        TO JP-RECORD-AREA.             
05165                                                                   
05166      EXEC CICS DELETE                                             
05167           DATASET    (FILE-ID-ERRQST)                             
05168      END-EXEC.                                                    
05169                                                                   
05170      PERFORM 8400-LOG-JOURNAL-RECORD.                             
05171                                                                   
05172  2480-DELETE-FINISHED.                                            
05173      MOVE ZEROS                  TO EMI-ERROR.                    
05174      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
05175      MOVE LOW-VALUES             TO VP631CI.                      
05176      MOVE -1                     TO CMAINTL.                      
05177      GO TO 8100-SEND-INITIAL-MAP.                                 
05178                                                                   
05179  2485-BATCH-HDR-NOT-FOUND.                                        
05180      MOVE -1                     TO CMAINTL                       
05181      MOVE AL-UABON               TO CMAINTA.                      
05182      MOVE ER-7450                TO EMI-ERROR.                    
05183      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
05184      GO TO 8200-SEND-DATAONLY.                                    
05185                                                                   
05186  2490-RECORD-NOT-FOUND.                                           
05187      MOVE -1                     TO CMAINTL                       
05188      MOVE AL-UABON               TO CMAINTA.                      
05189      MOVE ER-7451                TO EMI-ERROR.                    
05190      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
05191      GO TO 8200-SEND-DATAONLY.                                    

111513***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
111513***                                                            ***
111513***     2500  B R O W S E   C H E C K S                        ***
111513***  We perform this routine from 2 different places.          ***
111513***  1. If the user changes the refund amount on a cancel, we  ***
111513***    want to.........                                        ***
111513***    a. determine if prior checks have been processed        ***
111513***    b. If the amt is larger, display message instructing    ***
111513***     them to create another check to make up the difference.***
111513***    c. If the amt is less, do not allow the change and      ***
111513***     display a FATAL error suggesting they deal with check  ***
111513***     records befor continuing.                              ***
111513***  2. If the user wants to delete a cancel record we......   ***
111513***    a. browse through all the checks and if there is a      ***
111513***     processed check, send FATAL message.                   ***
111513***    b. If there is a non-released check, then delete that   ***
111513***     check after you delete the pending cancel record.      ***
111513***                                                            ***
111513***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
111513
111513 2500-browse-checks.
111513
111513     perform 2510-startbr-erchek thru 2510-exit
111513
111513     if not resp-normal
111513        go to 2500-exit
111513     end-if
111513
111513     perform until i-say-stop
111513        perform 2520-readnext-erchek
111513                                 thru 2520-exit
111513        if (not resp-normal)
111513           or (pb-control-by-account (1:33) not =
111513              ch-control-primary (1:33))
111513              set i-say-stop to true
111513        else
111513           if (ch-approval-status) = ' '
111513              and (ch-void-dt = low-values)
111513              and (ch-released-dt = low-values)
111513              set i-have-checks to true
111513           else
041514              if ch-void-dt = low-values
111513                 set i-have-processed-checks to true
041514              end-if
111513           end-if
031114           if (ch-credit-accept-dt = low-values)
031114              and (ch-void-dt = low-values)
031114              and (ch-approval-status = ' ' OR 'P' OR 'A')
091615              if (ch-check-origin-sw not = 'R')
091615                 and (pb-record-type = '2')
091615                 continue
091615              else
091615                 compute ws-check-amts =
091615                    ws-check-amts + ch-amount-paid
091615              end-if
111513           end-if
111513        end-if
111513     end-perform
111513
111513     if ws-chek-browse-sw = 'Y'
111513        perform 2525-endbr-erchek thru 2525-exit
111513     end-if
111513
111513    .
111513 2500-exit.
111513     exit.
111513
111513 2510-startbr-erchek.
111513
111513     move pb-control-by-account  to erchek-key
111513     move +0                     to chek-record-seq
111513
111513     exec cics startbr
111513        dataset   ('ERCHEK')
111513        ridfld    (erchek-key)
111513        gteq
111513        resp      (ws-response)
111513     end-exec
111513
111513     IF RESP-NORMAL
111513        move 'Y'                 to ws-chek-browse-sw
111513     end-if
111513
111513     .
111513 2510-exit.
111513     exit.
111513
111513 2520-readnext-erchek.
111513
111513     EXEC CICS READNEXT
111513        SET     (ADDRESS OF CHECK-RECORDS)
111513        DATASET ('ERCHEK')
111513        RIDFLD  (ERCHEK-KEY)
111513        RESP    (WS-RESPONSE)
111513     END-EXEC
111513
111513     .
111513 2520-exit.
111513     exit.
111513
111513 2525-endbr-erchek.
111513
111513     exec cics endbr
111513        dataset  ('ERCHEK')
111513     end-exec
111513
111513     .
111513 2525-exit.
111513     exit.
111513
111513 2530-readupd-erchek.
111513
111513     EXEC CICS READ
111513        UPDATE
111513        SET     (ADDRESS OF CHECK-RECORDS)
111513        DATASET ('ERCHEK')
111513        RIDFLD  (ERCHEK-KEY)
111513        RESP    (WS-RESPONSE)
111513     END-EXEC
111513
111513     .
111513 2530-exit.
111513     exit.
111513
111513 2540-delete-erchek.
111513
111513     EXEC CICS DELETE
111513        DATASET ('ERCHEK')
111513        RESP    (WS-RESPONSE)
111513     END-EXEC
111513
111513     .
111513 2540-exit.
111513     exit.


05195 ***************************************************************   
05196 *                                                             *   
05197 *       B U I L D   C O R R E C T I O N   R E C O R D         *   
05198 *                                                             *   
05199 ***************************************************************   
05200                                                                   
05201 ***************************************************************   
05202 *     ROUTINE TO CREATE COPY OF ZERO RECORD WHEN CHANGING     *   
05203 *     AN ALREADY BILLED RECORD.                               *   
05204 ***************************************************************   
05205                                                                   
05206  2700-BUILD-CORRECTION-RECORD.                                    
05207      MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY.             
05208      MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.         
05209      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              
05210      MOVE '5'                    TO DC-OPTION-CODE.               
05211      PERFORM 9700-DATE-LINK.                                      
05212      MOVE DC-BIN-DATE-1          TO PB-LAST-MAINT-DT.             
05213                                                                   
05214      IF PB-CHG-COUNT = ZEROS                                      
05215         MOVE +1                  TO WS-SEQ-SAVE                   
05216      ELSE                                                         
05217         ADD +1  PB-CHG-COUNT     TO WS-SEQ-SAVE.                  
05218                                                                   
05219      COMPUTE PB-BATCH-CHG-SEQ-NO = 1000 - WS-SEQ-SAVE.            
05220      MOVE PB-BATCH-CHG-SEQ-NO    TO PB-ALT-CHG-SEQ-NO.            
05221                                                                   
05222      MOVE PB-CONTROL-PRIMARY     TO PB-CONTROL-BY-ORIG-BATCH.     
05223                                                                   
05224      MOVE  FILE-ID-ERPNDB        TO JP-FILE-ID.                   
05225      MOVE 'A'                    TO JP-RECORD-TYPE.               
05226      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
05227                                                                   
05228      EXEC CICS WRITE                                              
05229          DATASET    (FILE-ID-ERPNDB)                              
05230          FROM       (PENDING-BUSINESS)                            
05231          RIDFLD     (PB-CONTROL-PRIMARY)                          
05232      END-EXEC.                                                    
05233                                                                   
05234      MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
05235                                                                   
05236      PERFORM 8400-LOG-JOURNAL-RECORD.                             
05237                                                                   
05238  2799-EXIT.                                                       
05239       EXIT.                                                       
05240                                                                   
05241      EJECT                                                        
05242                                                                   
05243 ******************************************************************
05244 *                                                                *
05245 *        R E W R I T E   I S S U E   C E R T.   K E Y            *
05246 *                                                                *
05247 ******************************************************************
05248                                                                   
05249  2800-REWRITE-ISSUE-CERT-KEY.                                     
05250      MOVE  PI-PREV-ALT-KEY       TO  ERPNDB-ALT-KEY.              
05251                                                                   
05252      IF  BCERTNOL GREATER THAN ZEROS                              
05253          MOVE BCERTNOI           TO  ERPNDB-CERT-PRIME.           
05254                                                                   
05255      IF  BSUFIXL  GREATER THAN ZEROS                              
05256          MOVE BSUFIXI            TO  ERPNDB-CERT-SFX.             
05257                                                                   
05258      IF  BEFFDTL  GREATER THAN ZEROS                              
05259          MOVE WS-CONVERTED-EFFDT TO  ERPNDB-CERT-EFF-DT.          
05260                                                                   
05261  2810-READ-BY-ACCOUNT.                                            
05262      EXEC CICS HANDLE CONDITION                                   
05263           NOTFND    (2820-PROCESS-REWRITE)                        
05264      END-EXEC.                                                    
05265                                                                   
      ***  read pndb using new key

05266      EXEC CICS READ                                               
05267          SET     (ADDRESS OF PENDING-BUSINESS)                    
05268          DATASET (FILE-ID-ERPNDB2)                                
05269          RIDFLD  (ERPNDB-ALT-KEY)                                 
05270      END-EXEC.                                                    
05271                                                                   
05272      GO TO 2890-DUPLICATE-RECORD.                                 
05273                                                                   
05274   2820-PROCESS-REWRITE.                                           
05275      EXEC CICS HANDLE CONDITION                                   
05276           NOTFND    (2885-RECORD-MISSING)                         
05277      END-EXEC.                                                    
05278                                                                   
05279      MOVE  PI-PREV-CONTROL-PRIMARY TO ERPNDB-KEY.                 
05280                                                                   
      ***  read pndb using original key

05281      EXEC CICS READ                                               
05282          SET     (ADDRESS OF PENDING-BUSINESS)                    
05283          DATASET (FILE-ID-ERPNDB)                                 
05284          RIDFLD  (ERPNDB-KEY)                                     
05285          UPDATE                                                   
05286      END-EXEC.                                                    
05287                                                                   
05288      MOVE FILE-ID-ERPNDB         TO JP-FILE-ID.                   
05289      MOVE 'D'                    TO JP-RECORD-TYPE.               
05290      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
05291      MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
05292                                                                   
05293      MOVE PB-CONTROL-BY-ACCOUNT  TO ELCERT-KEY.                   
05294                                                                   
05295      MOVE PB-SV-CARRIER          TO ELCERT-CARRIER.               
05296      MOVE PB-SV-GROUPING         TO ELCERT-GROUPING.              
05297      MOVE PB-SV-STATE            TO ELCERT-STATE.                 
05298                                                                   
      ***  delete pndb using original key

05299      EXEC CICS DELETE                                             
05300          DATASET    (FILE-ID-ERPNDB)                              
05301      END-EXEC.                                                    
05302                                                                   
05303      PERFORM 8400-LOG-JOURNAL-RECORD.                             
05304                                                                   
05305      EXEC CICS GETMAIN                                            
05306          SET      (ADDRESS OF PENDING-BUSINESS)                   
05307          LENGTH   (ERPNDB-RECORD-LENGTH)                          
05308          INITIMG  (GETMAIN-SPACE)                                 
05309      END-EXEC.                                                    
05310                                                                   
      ***  pending-business now has original record

05311      MOVE JP-RECORD-AREA         TO PENDING-BUSINESS.             
05312                                                                   
05313      EXEC CICS HANDLE CONDITION                                   
05314          NOTFND   (2830-BUILD-NEW-ISSUE-RECORD)                   
05315      END-EXEC.                                                    
05316                                                                   
      ***  read elcert using original key

05317      EXEC CICS READ                                               
05318          SET     (ADDRESS OF CERTIFICATE-MASTER)                  
05319          DATASET (FILE-ID-ELCERT)                                 
05320          RIDFLD  (ELCERT-KEY)                                     
05321          UPDATE                                                   
05322      END-EXEC.                                                    
05323                                                                   
05324      MOVE  FILE-ID-ELCERT        TO JP-FILE-ID.                   
05325      MOVE 'B'                    TO JP-RECORD-TYPE                
05326      MOVE CERTIFICATE-MASTER     TO JP-RECORD-AREA.               
05327      MOVE ELCERT-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
05328                                                                   
05329      IF INSURED-ADDR-PRESENT                                      
05330         MOVE 'Y'                 TO WS-CERT-ADDRESS-SW.           
05331                                                                   
05332      IF CERT-NOTES-ARE-NOT-PRESENT                                
05333         MOVE SPACE               TO WS-CERT-NOTE-SW               
05334      ELSE                                                         
05335         MOVE 'Y'                 TO WS-CERT-NOTE-SW.              
05336                                                                   
05337      IF CERT-ADDED-BATCH      OR                                  
05338         CERT-AND-CLAIM-ONLINE OR                                  
05339         CERT-WAS-CREATED-FOR-CLAIM                                
05340         NEXT SENTENCE                                             
05341      ELSE                                                         
05342         GO TO 2825-DELETE-CERTIFICATE.                            
05343                                                                   
05344      PERFORM 8400-LOG-JOURNAL-RECORD.                             
05345                                                                   
05346 *********************************************************         
05347 ** DO NOT SET CERT-ADDED-BATCH AS CLAIM-CREATED CERT **           
05348      IF CERT-ADDED-BATCH                                          
05349          GO TO 2822-REWRITE-ELCERT.                               
05350 *********************************************************         
05351                                                                   
05352      IF NO-CLAIM-ATTACHED                                         
05353         NEXT SENTENCE                                             
05354      ELSE                                                         
05355         MOVE SPACE               TO CM-CREDIT-INTERFACE-SW-1      
05356         MOVE '2'                 TO CM-CLAIM-INTERFACE-SW.        
05357                                                                   
05358  2822-REWRITE-ELCERT.                                             
05359      MOVE 'C'                    TO JP-RECORD-TYPE                
05360      MOVE CERTIFICATE-MASTER     TO JP-RECORD-AREA.               
05361                                                                   
05362      MOVE ELCERT-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
05363                                                                   
05364      EXEC CICS REWRITE                                            
05365           DATASET    (FILE-ID-ELCERT)                             
05366           FROM       (CERTIFICATE-MASTER)                         
05367      END-EXEC.                                                    
05368                                                                   
05369      PERFORM 8400-LOG-JOURNAL-RECORD.                             
05370                                                                   
05371      GO TO 2830-BUILD-NEW-ISSUE-RECORD.                           
05372                                                                   
05373  2825-DELETE-CERTIFICATE.                                         
05374      MOVE 'D'                      TO JP-RECORD-TYPE.             
05375      MOVE CERTIFICATE-MASTER       TO JP-RECORD-AREA.             
05376                                                                   
05377      MOVE ELCERT-JOURNAL-LENGTH    TO WS-JOURNAL-RECORD-LENGTH.   
05378                                                                   
05379      EXEC CICS DELETE                                             
05380           DATASET    (FILE-ID-ELCERT)                             
05381      END-EXEC.                                                    
05382                                                                   
05383      PERFORM 8400-LOG-JOURNAL-RECORD.                             
05384                                                                   
05385 ******************************************************************
05386 *       DELETE CERTIFICATE NOTES FOR ALL DELETED CERTIFICATES    *
05387 ******************************************************************
05388                                                                   
05389      IF CERT-NOTES-ARE-PRESENT                                    
05390         NEXT SENTENCE                                             
05391      ELSE                                                         
05392         GO TO 2827-DELETE-MAILING.                                
05393                                                                   
05394      EXEC CICS HANDLE CONDITION                                   
05395           NOTFND  (2827-DELETE-MAILING)                           
05396      END-EXEC.                                                    
05397                                                                   
05398      EXEC CICS READ                                               
05399          EQUAL                                                    
05400          DATASET   (FILE-ID-ERNOTE)                               
05401          SET       (ADDRESS OF CERTIFICATE-NOTE)                  
05402          RIDFLD    (ELCERT-KEY)                                   
05403          UPDATE                                                   
05404      END-EXEC.                                                    
05405                                                                   
05406      MOVE  FILE-ID-ERNOTE        TO  JP-FILE-ID.                  
05407      MOVE 'D'                    TO  JP-RECORD-TYPE.              
05408      MOVE CERTIFICATE-NOTE       TO  JP-RECORD-AREA.              
05409                                                                   
05410      MOVE ERNOTE-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
05411                                                                   
05412      EXEC CICS DELETE                                             
05413          DATASET   (FILE-ID-ERNOTE)                               
05414      END-EXEC.                                                    
05415                                                                   
05416      PERFORM 8400-LOG-JOURNAL-RECORD.                             
05417                                                                   
05418 ******************************************************************
05419 *     DELETE MAILING ADDRESS RECORDS FOR DELETED CERTIFICATES    *
05420 ******************************************************************
05421                                                                   
05422  2827-DELETE-MAILING.                                             
05423      IF CERT-ADDRESS-PRESENT                                      
05424         NEXT SENTENCE                                             
05425      ELSE                                                         
05426         GO TO 2830-BUILD-NEW-ISSUE-RECORD.                        
05427                                                                   
05428      EXEC CICS HANDLE CONDITION                                   
05429           NOTFND  (2830-BUILD-NEW-ISSUE-RECORD)                   
05430      END-EXEC.                                                    
05431                                                                   
05432      MOVE ELCERT-KEY             TO ERMAIL-KEY.                   
05433                                                                   
05434      EXEC CICS READ                                               
05435          EQUAL                                                    
05436          DATASET   (FILE-ID-ERMAIL)                               
05437          SET       (ADDRESS OF MAILING-DATA)                      
05438          RIDFLD    (ERMAIL-KEY)                                   
05439          UPDATE                                                   
05440      END-EXEC.                                                    
05441                                                                   
05442      MOVE FILE-ID-ERMAIL         TO  JP-FILE-ID.                  
05443      MOVE 'D'                    TO  JP-RECORD-TYPE.              
05444      MOVE MAILING-DATA           TO  JP-RECORD-AREA.              
05445                                                                   
05446      MOVE ERMAIL-JOURNAL-LENGTH  TO  WS-JOURNAL-RECORD-LENGTH.    
05447                                                                   
05448      EXEC CICS DELETE                                             
05449          DATASET   (FILE-ID-ERMAIL)                               
05450      END-EXEC.                                                    
05451                                                                   
05452      PERFORM 8400-LOG-JOURNAL-RECORD.                             
05453                                                                   
05454  2830-BUILD-NEW-ISSUE-RECORD.                                     
05455      MOVE SPACES                 TO ELCERT-KEY.                   
05456                                                                   
05457      IF  BCERTNOL GREATER THAN ZEROS                              
05458          MOVE BCERTNOI           TO  PB-CERT-PRIME.               
05459                                                                   
05460      IF  BSUFIXL  GREATER THAN ZEROS                              
05461          MOVE BSUFIXI            TO  PB-CERT-SFX.                 
05462                                                                   
05463      IF BEFFDTL GREATER THAN ZEROS                                
05464         MOVE  WS-CONVERTED-EFFDT TO  PB-CERT-EFF-DT.              
05465                                                                   
05466  2870-WRITE-PENDING-BUSINESS.                                     
05467      MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY.             
05468      MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.         
05469      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              
05470      MOVE '5'                    TO DC-OPTION-CODE.               
05471      PERFORM 9700-DATE-LINK.                                      
05472      MOVE DC-BIN-DATE-1          TO PB-LAST-MAINT-DT.             
05473                                                                   
05474      MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY.              
05475                                                                   
05476      MOVE  FILE-ID-ERPNDB        TO JP-FILE-ID.                   
05477      MOVE 'A'                    TO JP-RECORD-TYPE.               
05478      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
05479                                                                   
05480      EXEC CICS WRITE                                              
05481          DATASET  (FILE-ID-ERPNDB)                                
05482          FROM     (PENDING-BUSINESS)                              
05483          RIDFLD   (PB-CONTROL-PRIMARY)                            
05484      END-EXEC.                                                    
05485                                                                   
05486      MOVE ERPNDB-JOURNAL-LENGTH TO WS-JOURNAL-RECORD-LENGTH.      
05487                                                                   
05488      PERFORM 8400-LOG-JOURNAL-RECORD.                             
05489                                                                   
05490      GO TO 1325-PROCESS-ISSUE-CHANGES.                            
05491                                                                   
05492  2885-RECORD-MISSING.                                             
05493      MOVE -1                     TO BMAINTL                       
05494      MOVE AL-UABON               TO BMAINTA                       
05495      MOVE ER-2902                TO EMI-ERROR.                    
05496      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
05497      GO TO 8200-SEND-DATAONLY.                                    
05498                                                                   
05499  2890-DUPLICATE-RECORD.                                           
05500      MOVE -1                     TO BMAINTL.                      
05501      MOVE AL-UABON               TO BCERTNOA.                     
05502      MOVE AL-UNBON               TO BEFFDTA.                      
05503      MOVE SPACE                  TO BMAINTI.                      
05504                                                                   
05505      MOVE 1                      TO EMI-SUB                       
05506                                     EMI-SWITCH1                   
05507                                     EMI-SWITCH-AREA-1             
05508                                     EMI-SWITCH-AREA-2.            
05509                                                                   
05510      MOVE SPACES                 TO EMI-ERROR-LINES.              
05511                                                                   
05512      EXEC CICS SYNCPOINT ROLLBACK                                 
05513      END-EXEC.                                                    
05514                                                                   
05515      MOVE ER-2247                TO EMI-ERROR.                    
05516      PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT.                  
05517      GO TO 8200-SEND-DATAONLY.                                    
05518                                                                   
05519      EJECT                                                        
05520                                                                   
05521 ******************************************************************
05522 *                                                                *
05523 *         R E W R I T E   C A N C E L   C E R T.   K E Y         *
05524 *                                                                *
05525 ******************************************************************
05526                                                                   
05527  2900-REWRITE-CAN-CERT-KEY.                                       
05528      MOVE  PI-PREV-ALT-KEY       TO  ERPNDB-ALT-KEY.              
05529                                                                   
05530      IF  CCERTNOL GREATER THAN ZEROS                              
05531          MOVE  CCERTNOI          TO  ERPNDB-CERT-PRIME.           
05532                                                                   
05533      IF  CSUFIXL  GREATER THAN ZEROS                              
05534          MOVE  CSUFIXI           TO  ERPNDB-CERT-SFX.             
05535                                                                   
05536      IF  CEFFDTL  GREATER THAN ZEROS                              
05537          MOVE WS-CONVERTED-EFFDT TO  ERPNDB-CERT-EFF-DT.          
05538                                                                   
05539  2910-READ-BY-ACCOUNT.                                            
05540      EXEC CICS HANDLE CONDITION                                   
05541           NOTFND    (2920-PROCESS-REWRITE)                        
05542      END-EXEC.                                                    
05543                                                                   
05544      EXEC CICS READ                                               
05545          SET     (ADDRESS OF PENDING-BUSINESS)                    
05546          DATASET (FILE-ID-ERPNDB2)                                
05547          RIDFLD  (ERPNDB-ALT-KEY)                                 
05548      END-EXEC.                                                    
05549                                                                   
05550      GO TO 2990-DUPLICATE-RECORD.                                 
05551                                                                   
05552   2920-PROCESS-REWRITE.                                           
05553      EXEC CICS HANDLE CONDITION                                   
05554           NOTFND    (2985-RECORD-MISSING)                         
05555      END-EXEC.                                                    
05556                                                                   
05557      MOVE  PI-PREV-CONTROL-PRIMARY TO ERPNDB-KEY.                 
05558                                                                   
05559      EXEC CICS READ                                               
05560          SET     (ADDRESS OF PENDING-BUSINESS)                    
05561          DATASET (FILE-ID-ERPNDB)                                 
05562          RIDFLD  (ERPNDB-KEY)                                     
05563          UPDATE                                                   
05564      END-EXEC.                                                    
05565                                                                   
05566      MOVE FILE-ID-ERPNDB         TO JP-FILE-ID.                   
05567      MOVE 'D'                    TO JP-RECORD-TYPE.               
05568      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
05569      MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
05570                                                                   
05571      MOVE PB-CONTROL-BY-ACCOUNT  TO ELCERT-KEY.                   
05572                                                                   
05573      MOVE PB-SV-CARRIER          TO ELCERT-CARRIER.               
05574      MOVE PB-SV-GROUPING         TO ELCERT-GROUPING.              
05575      MOVE PB-SV-STATE            TO ELCERT-STATE.                 
05576                                                                   
05577      EXEC CICS DELETE                                             
05578          DATASET    (FILE-ID-ERPNDB)                              
05579      END-EXEC.                                                    
05580                                                                   
05581      PERFORM 8400-LOG-JOURNAL-RECORD.                             
05582                                                                   
05583      EXEC CICS GETMAIN                                            
05584          SET      (ADDRESS OF PENDING-BUSINESS)                   
05585          LENGTH   (ERPNDB-RECORD-LENGTH)                          
05586          INITIMG  (GETMAIN-SPACE)                                 
05587      END-EXEC.                                                    
05588                                                                   
05589      MOVE JP-RECORD-AREA         TO PENDING-BUSINESS.             
05590                                                                   
05591  2950-PROCESS-CANCELS.                                            
05592                                                                   
05593      EXEC CICS HANDLE CONDITION                                   
05594           NOTFND  (2960-BUILD-NEW-CANCEL-RECORD)                  
05595      END-EXEC.                                                    
05596                                                                   
05597      EXEC CICS READ                                               
05598          SET     (ADDRESS OF CERTIFICATE-MASTER)                  
05599          DATASET (FILE-ID-ELCERT)                                 
05600          RIDFLD  (ELCERT-KEY)                                     
05601          UPDATE                                                   
05602      END-EXEC.                                                    
05603                                                                   
05604      MOVE ELCERT-JOURNAL-LENGTH    TO WS-JOURNAL-RECORD-LENGTH.   
05605                                                                   
05606      MOVE  FILE-ID-ELCERT          TO JP-FILE-ID.                 
05607      MOVE 'B'                      TO JP-RECORD-TYPE.             
05608      MOVE CERTIFICATE-MASTER       TO JP-RECORD-AREA.             
05609      PERFORM 8400-LOG-JOURNAL-RECORD.                             
05610                                                                   
05611      IF PB-CI-LIVES NOT NUMERIC                                   
05612          MOVE ZERO                    TO PB-CI-LIVES.             
05613                                                                   
05614      IF PB-CI-LF-PRIOR-CANCEL-DT = LOW-VALUES                     
05615         MOVE SPACE                    TO CM-CREDIT-INTERFACE-SW-2 
05616         MOVE ZERO                     TO CM-LF-ITD-CANCEL-AMT     
05617         MOVE LOW-VALUES               TO CM-LF-CANCEL-EXIT-DT     
05618                                          CM-LF-CANCEL-DT          
05619      ELSE                                                         
05620         MOVE SPACE                    TO CM-CREDIT-INTERFACE-SW-2 
05621         MOVE PB-CI-LF-CANCEL-AMT      TO CM-LF-ITD-CANCEL-AMT     
05622         MOVE PB-CI-LF-PRIOR-CANCEL-DT TO CM-LF-CANCEL-DT          
05623         MOVE PB-CI-LIVES              TO CM-LIVES.                
05624                                                                   
05625      IF PB-CI-LF-PRIOR-CANCEL-DT = LOW-VALUES                     
05626         IF CM-LF-STATUS-AT-CANCEL NOT EQUAL TO SPACE              
05627             MOVE CM-LF-STATUS-AT-CANCEL                           
05628                                      TO CM-LF-CURRENT-STATUS      
05629             MOVE SPACE                                            
05630                                      TO CM-LF-STATUS-AT-CANCEL    
05631         ELSE                                                      
05632             MOVE PB-CI-LF-POLICY-STATUS                           
05633                                      TO CM-LF-CURRENT-STATUS      
05634      ELSE                                                         
05635         MOVE PB-CI-LF-POLICY-STATUS  TO CM-LF-CURRENT-STATUS.     
05636                                                                   
05637      IF PB-CI-AH-PRIOR-CANCEL-DT = LOW-VALUES                     
05638         MOVE SPACE                    TO CM-CREDIT-INTERFACE-SW-2 
05639         MOVE ZERO                     TO CM-AH-ITD-CANCEL-AMT     
05640         MOVE LOW-VALUES               TO CM-AH-CANCEL-EXIT-DT     
05641                                          CM-AH-CANCEL-DT          
05642      ELSE                                                         
05643         MOVE SPACE                    TO CM-CREDIT-INTERFACE-SW-2 
05644         MOVE PB-CI-AH-CANCEL-AMT      TO CM-AH-ITD-CANCEL-AMT     
05645         MOVE PB-CI-AH-PRIOR-CANCEL-DT TO CM-AH-CANCEL-DT          
05646         MOVE PB-CI-LIVES              TO CM-LIVES.                
05647                                                                   
05648      IF PB-CI-AH-PRIOR-CANCEL-DT = LOW-VALUES                     
05649         IF CM-AH-STATUS-AT-CANCEL NOT EQUAL TO SPACE              
05650             MOVE CM-AH-STATUS-AT-CANCEL                           
05651                                      TO CM-AH-CURRENT-STATUS      
05652             MOVE SPACE                                            
05653                                      TO CM-AH-STATUS-AT-CANCEL    
05654         ELSE                                                      
05655             MOVE PB-CI-AH-POLICY-STATUS                           
05656                                      TO CM-AH-CURRENT-STATUS      
05657      ELSE                                                         
05658         MOVE PB-CI-AH-POLICY-STATUS  TO CM-AH-CURRENT-STATUS.     
05659                                                                   
05660  2975-REWRITE-CERT.                                               
05661      MOVE 'C'                      TO JP-RECORD-TYPE.             
05662      MOVE CERTIFICATE-MASTER       TO JP-RECORD-AREA.             
05663                                                                   
05664      EXEC CICS REWRITE                                            
05665          DATASET    (FILE-ID-ELCERT)                              
05666          FROM       (CERTIFICATE-MASTER)                          
05667      END-EXEC.                                                    
05668                                                                   
05669      PERFORM 8400-LOG-JOURNAL-RECORD.                             
05670                                                                   
05671  2960-BUILD-NEW-CANCEL-RECORD.                                    
05672      IF  CCERTNOL GREATER THAN ZEROS                              
05673          MOVE CCERTNOI            TO PB-CERT-PRIME.               
05674                                                                   
05675      IF  CSUFIXL  GREATER THAN ZEROS                              
05676          MOVE CSUFIXI             TO PB-CERT-SFX.                 
05677                                                                   
05678      IF  CEFFDTL  GREATER  THAN ZEROS                             
05679          MOVE WS-CONVERTED-EFFDT  TO PB-CERT-EFF-DT.              
05680                                                                   
05681  2970-WRITE-PENDING-BUSINESS.                                     
05682      MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY.             
05683      MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.         
05684      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              
05685      MOVE '5'                    TO DC-OPTION-CODE.               
05686      PERFORM 9700-DATE-LINK.                                      
05687      MOVE DC-BIN-DATE-1          TO PB-LAST-MAINT-DT.             
05688                                                                   
05689      MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY.              
05690                                                                   
05691      MOVE SPACES                 TO PB-CI-INSURED-NAME            
05692                                     PB-CI-INSURED-SEX             
05693                                     PB-CI-LF-BENEFIT-CD           
05694                                     PB-CI-AH-BENEFIT-CD           
05695                                     PB-CI-RATE-CLASS              
05696                                     PB-CI-RATE-DEV-LF             
05697                                     PB-CI-RATE-DEV-AH             
05698                                     PB-CI-LF-ABBR                 
05699                                     PB-CI-AH-ABBR                 
05700                                     PB-CI-OB-FLAG                 
05701                                     PB-CI-LF-POLICY-STATUS        
05702                                     PB-CI-AH-POLICY-STATUS        
05703                                     PB-CI-INDV-GRP-CD             
05704                                     PB-CI-BENEFICIARY-NAME        
05705                                     PB-CI-NOTE-SW                 
05706                                     PB-CI-SOC-SEC-NO              
05707                                     PB-CI-MEMBER-NO               
05708                                     PB-CI-INT-CODE                
05709                                     PB-CI-COMP-EXCP-SW            
05710                                     PB-CI-ENTRY-STATUS            
05711                                     PB-CI-CREDIT-INTERFACE-SW-1   
05712                                     PB-CI-CREDIT-INTERFACE-SW-2   
05713                                     PB-CI-ENTRY-BATCH.            
05714                                                                   
05715      MOVE LOW-VALUES             TO PB-CI-AH-PAID-THRU-DT         
05716                                     PB-CI-AH-SETTLEMENT-DT        
05717                                     PB-CI-DEATH-DT                
05718                                     PB-CI-LF-PRIOR-CANCEL-DT      
05719                                     PB-CI-AH-PRIOR-CANCEL-DT      
05720                                     PB-CI-LOAN-1ST-PMT-DT         
05721                                     PB-CI-ENTRY-DT                
05722                                     PB-CI-LF-EXPIRE-DT            
05723                                     PB-CI-AH-EXPIRE-DT.           
05724                                                                   
05725      MOVE ZEROS                  TO PB-CI-INSURED-AGE             
05726                                     PB-CI-LF-TERM                 
05727                                     PB-CI-LF-BENEFIT-AMT          
05728                                     PB-CI-LF-ALT-BENEFIT-AMT      
05729                                     PB-CI-LF-PREMIUM-AMT          
05730                                     PB-CI-LF-ALT-PREMIUM-AMT      
05731                                     PB-CI-AH-TERM                 
05732                                     PB-CI-AH-BENEFIT-AMT          
05733                                     PB-CI-AH-PREMIUM-AMT          
05734                                     PB-CI-RATE-DEV-PCT-LF         
05735                                     PB-CI-RATE-DEV-PCT-AH         
05736                                     PB-CI-LIFE-COMMISSION         
05737                                     PB-CI-AH-COMMISSION           
05738                                     PB-CI-PAY-FREQUENCY           
05739                                     PB-CI-LOAN-APR                
05740                                     PB-CI-LOAN-TERM               
05741                                     PB-CI-CURR-SEQ                
05742                                     PB-CI-AH-CANCEL-AMT           
05743                                     PB-CI-LF-CANCEL-AMT           
05744                                     PB-CI-EXTENTION-DAYS          
05745                                     PB-CI-TERM-IN-DAYS            
05746                                     PB-CI-LOAN-OFFICER            
05747                                     PB-CI-LIVES                   
05748                                     PB-CI-LF-CRIT-PER             
05749                                     PB-CI-AH-CRIT-PER             
05750                                     PB-C-LF-REM-TERM              
05751                                     PB-C-AH-REM-TERM.             
05752                                                                   
05753      MOVE  FILE-ID-ERPNDB        TO JP-FILE-ID.                   
05754      MOVE 'A'                    TO JP-RECORD-TYPE.               
05755      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
05756                                                                   
05757      EXEC CICS HANDLE CONDITION                                   
05758          DUPREC   (2990-DUPLICATE-RECORD)                         
05759      END-EXEC.                                                    
05760                                                                   
05761      EXEC CICS WRITE                                              
05762          DATASET  (FILE-ID-ERPNDB)                                
05763          FROM     (PENDING-BUSINESS)                              
05764          RIDFLD   (PB-CONTROL-PRIMARY)                            
05765      END-EXEC.                                                    
05766                                                                   
05767      MOVE ERPNDB-RECORD-LENGTH    TO WS-JOURNAL-RECORD-LENGTH.    
05768                                                                   
05769      PERFORM 8400-LOG-JOURNAL-RECORD.                             
05770                                                                   
05771      GO TO 2325-PROCESS-CANCEL-CHANGES.                           
05772                                                                   
05773  2985-RECORD-MISSING.                                             
05774      MOVE -1                     TO CMAINTL                       
05775      MOVE AL-UABON               TO CMAINTA.                      
05776                                                                   
05777      MOVE ER-2902                TO EMI-ERROR.                    
05778      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
05779      GO TO 8200-SEND-DATAONLY.                                    
05780                                                                   
05781  2990-DUPLICATE-RECORD.                                           
05782      MOVE SPACE                  TO CMAINTI.                      
05783      MOVE -1                     TO CMAINTL.                      
05784      MOVE AL-UABON               TO CCERTNOA.                     
05785      MOVE AL-UNBON               TO CEFFDTA.                      
05786                                                                   
05787      MOVE 1                      TO EMI-SUB                       
05788                                     EMI-SWITCH1                   
05789                                     EMI-SWITCH-AREA-1             
05790                                     EMI-SWITCH-AREA-2.            
05791      MOVE SPACES                 TO EMI-ERROR-LINES.              
05792                                                                   
05793      EXEC CICS SYNCPOINT ROLLBACK                                 
05794      END-EXEC.                                                    
05795                                                                   
05796      MOVE ER-2247                TO EMI-ERROR.                    
05797      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
05798      GO TO 8200-SEND-DATAONLY.                                    
05799                                                                   
05800      EJECT                                                        
05801                                                                   
05802 ***************************************************************   
05803 *     ROUTINE TO CALCULATE THE TOTAL OF ALL GOOD AND BAD      *   
05804 *     PREMIUM FOR A SINGLE BATCH.                             *   
05805 *         ACTIVATED VIA THE PF3 FROM THE BATCH TOTAL SCREEN.  *   
05806 ***************************************************************   
05807                                                                   
05808  3000-BATCH-MAINLINE.                                             
05809      IF PI-PREV-KEY NOT = SPACES                                  
05810          MOVE PI-PREV-KEY        TO ERPNDB-KEY                    
05811      ELSE                                                         
05812          MOVE PI-ERPNDB-KEY      TO ERPNDB-KEY.                   
05813                                                                   
05814      MOVE +1                     TO ERPNDB-BATCH-SEQ-NO.          
05815                                                                   
05816  3010-READNEXT.                                                   
05817      EXEC CICS READ                                               
05818           DATASET    (FILE-ID-ERPNDB)                             
05819           SET        (ADDRESS OF PENDING-BUSINESS)                
05820           RIDFLD     (ERPNDB-KEY)                                 
05821           GTEQ                                                    
05822      END-EXEC.                                                    
05823                                                                   
05824      IF PB-COMPANY-CD  NOT = ERPNDB-COMPANY-CD OR                 
05825         PB-ENTRY-BATCH NOT = ERPNDB-ENTRY-BATCH                   
05826           GO TO 3030-END-BATCH.                                   
05827                                                                   
05828      MOVE PB-CONTROL-PRIMARY     TO ERPNDB-KEY.                   
05829                                                                   
05830      IF PB-BATCH-CHG-SEQ-NO NOT = ZEROS OR                        
05831         PB-CREDIT-ACCEPT-DT NOT = LOW-VALUES                      
05832           GO TO 3020-BUMP-SEQ.                                    
05833                                                                   
05834      IF PB-ISSUE                                                  
05835         ADD +1                   TO WS-ISS-TOTAL-CNT              
05836         IF PB-FATAL-ERRORS                                        
05837            ADD +1                      TO WS-ISS-FATAL-CNT        
05838            PERFORM 3100-ADD-BAD-ISSUE-PREM  THRU  3199-EXIT       
05839            GO TO 3020-BUMP-SEQ                                    
05840         ELSE                                                      
05841            IF PB-UNFORCED-ERRORS                                  
05842               ADD +1                      TO WS-ISS-FORCE-CNT     
05843               PERFORM 3100-ADD-BAD-ISSUE-PREM  THRU  3199-EXIT    
05844               GO TO 3020-BUMP-SEQ                                 
05845            ELSE                                                   
05846               IF PB-RECORD-ON-HOLD OR PB-RECORD-RETURNED          
05847                  ADD +1                      TO WS-ISS-HOLD-CNT   
05848                  PERFORM 3100-ADD-BAD-ISSUE-PREM  THRU  3199-EXIT 
05849                  GO TO 3020-BUMP-SEQ                              
05850               ELSE                                                
05851                  PERFORM 3200-ADD-GOOD-ISSUE-PREM                 
05852                      THRU  3299-EXIT                              
05853                  GO TO 3020-BUMP-SEQ.                             
05854                                                                   
05855      IF PB-CANCELLATION                                           
05856         ADD +1                   TO WS-CAN-TOTAL-CNT              
05857         IF PB-FATAL-ERRORS                                        
05858            ADD +1                 TO WS-CAN-FATAL-CNT             
05859            PERFORM 3300-ADD-BAD-CANCEL-PREM  THRU  3399-EXIT      
05860            GO TO 3020-BUMP-SEQ                                    
05861         ELSE                                                      
05862            IF PB-UNFORCED-ERRORS                                  
05863               ADD +1                 TO WS-CAN-FORCE-CNT          
05864               PERFORM 3300-ADD-BAD-CANCEL-PREM  THRU  3399-EXIT   
05865               GO TO 3020-BUMP-SEQ                                 
05866            ELSE                                                   
05867               IF PB-RECORD-ON-HOLD OR PB-RECORD-RETURNED          
05868                  ADD +1                 TO WS-CAN-HOLD-CNT        
05869                  PERFORM 3300-ADD-BAD-CANCEL-PREM  THRU  3399-EXIT
05870                  GO TO 3020-BUMP-SEQ                              
05871               ELSE                                                
05872                  PERFORM 3400-ADD-GOOD-CANCEL-PREM                
05873                      THRU  3499-EXIT                              
05874                  GO TO 3020-BUMP-SEQ.                             
05875                                                                   
05876      IF PB-BATCH-TRAILER                                          
05877          GO TO 3030-END-BATCH.                                    
05878                                                                   
05879  3020-BUMP-SEQ.                                                   
05880      ADD +1                      TO ERPNDB-BATCH-SEQ-NO.          
05881      MOVE ZEROS                  TO ERPNDB-BATCH-CHG-SEQ-NO.      
05882      GO TO 3010-READNEXT.                                         
05883                                                                   
05884  3030-END-BATCH.                                                  
05885      MOVE WS-ISS-TOTAL-CNT       TO DITOTALO.                     
05886      MOVE WS-ISS-FATAL-CNT       TO DIFATALO.                     
05887      MOVE WS-ISS-FORCE-CNT       TO DIFORCEO.                     
05888      MOVE WS-ISS-HOLD-CNT        TO DIHOLDO.                      
05889                                                                   
05890      COMPUTE DIAVAILO = (WS-ISS-TOTAL-CNT - WS-ISS-FATAL-CNT      
05891                                           - WS-ISS-FORCE-CNT      
05892                                           - WS-ISS-HOLD-CNT).     
05893                                                                   
05894      MOVE WS-ISS-GOOD-PREM       TO DIGPREMO.                     
05895      MOVE WS-ISS-BAD-PREM        TO DIBPREMO.                     
05896                                                                   
05897      MOVE WS-CAN-TOTAL-CNT       TO DCTOTALO.                     
05898      MOVE WS-CAN-FATAL-CNT       TO DCFATALO.                     
05899      MOVE WS-CAN-FORCE-CNT       TO DCFORCEO.                     
05900      MOVE WS-CAN-HOLD-CNT        TO DCHOLDO.                      
05901      COMPUTE DCAVAILO = (WS-CAN-TOTAL-CNT - WS-CAN-FATAL-CNT      
05902                                           - WS-CAN-FORCE-CNT      
05903                                           - WS-CAN-HOLD-CNT).     
05904      MOVE WS-CAN-GOOD-PREM       TO DCGPREMO.                     
05905      MOVE WS-CAN-BAD-PREM        TO DCBPREMO.                     
05906                                                                   
05907      COMPUTE WS-NET-TOTAL-CNT                                     
05908          = WS-ISS-TOTAL-CNT - WS-CAN-TOTAL-CNT.                   
05909      MOVE WS-NET-TOTAL-CNT       TO DNTOTALO.                     
05910      COMPUTE WS-NET-FATAL-CNT                                     
05911          = WS-ISS-FATAL-CNT - WS-CAN-FATAL-CNT.                   
05912      MOVE WS-NET-FATAL-CNT       TO DNFATALO.                     
05913      COMPUTE WS-NET-FORCE-CNT                                     
05914          = WS-ISS-FORCE-CNT - WS-CAN-FORCE-CNT.                   
05915      MOVE WS-NET-FORCE-CNT       TO DNFORCEO.                     
05916      COMPUTE WS-NET-HOLD-CNT                                      
05917          = WS-ISS-HOLD-CNT - WS-CAN-HOLD-CNT.                     
05918      MOVE WS-NET-HOLD-CNT        TO DNHOLDO.                      
05919                                                                   
05920      COMPUTE WS-NET-AVAIL-CNT                                     
05921           = (WS-ISS-TOTAL-CNT - WS-ISS-FATAL-CNT                  
05922                               - WS-ISS-FORCE-CNT                  
05923                               - WS-ISS-HOLD-CNT)                  
05924           - (WS-CAN-TOTAL-CNT - WS-CAN-FATAL-CNT                  
05925                               - WS-CAN-FORCE-CNT                  
05926                               - WS-CAN-HOLD-CNT).                 
05927      MOVE WS-NET-AVAIL-CNT       TO DNAVAILO.                     
05928                                                                   
05929      COMPUTE WS-NET-GOOD-PREM                                     
05930          = WS-ISS-GOOD-PREM - WS-CAN-GOOD-PREM.                   
05931      MOVE WS-NET-GOOD-PREM       TO DNGPREMO.                     
05932      COMPUTE WS-NET-BAD-PREM                                      
05933          = WS-ISS-BAD-PREM - WS-CAN-BAD-PREM.                     
05934      MOVE WS-NET-BAD-PREM        TO DNBPREMO.                     
05935                                                                   
05936      MOVE 0000                   TO EMI-ERROR.                    
05937      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
05938      MOVE -1                     TO DPFENTRL.                     
05939      GO TO 8200-SEND-DATAONLY.                                    
05940                                                                   
05941      EJECT                                                        
05942                                                                   
05943 ******************************************************************
05944 *                                                                *
05945 *           A D D    B A D   I S S U E   P R E M I U M           *
05946 *                                                                *
05947 ******************************************************************
05948                                                                   
05949  3100-ADD-BAD-ISSUE-PREM.                                         
05950      IF PB-OVERRIDE-LIFE                                          
05951        OR PB-OVERRIDE-BOTH                                        
05952            ADD PB-I-LF-PREM-CALC       TO  WS-ISS-BAD-PREM        
05953            ADD PB-I-LF-ALT-PREM-CALC   TO  WS-ISS-BAD-PREM        
05954        ELSE                                                       
05955            ADD PB-I-LF-PREMIUM-AMT     TO  WS-ISS-BAD-PREM        
05956            ADD PB-I-LF-ALT-PREMIUM-AMT TO  WS-ISS-BAD-PREM.       
05957                                                                   
05958      IF PB-OVERRIDE-AH                                            
05959        OR PB-OVERRIDE-BOTH                                        
05960            ADD PB-I-AH-PREM-CALC       TO  WS-ISS-BAD-PREM        
05961        ELSE                                                       
05962            ADD PB-I-AH-PREMIUM-AMT     TO  WS-ISS-BAD-PREM.       
05963                                                                   
05964  3199-EXIT.                                                       
05965      EXIT.                                                        
05966                                                                   
05967      EJECT                                                        
05968                                                                   
05969 ******************************************************************
05970 *                                                                *
05971 *       A D D   G O O D   I S S U E   P R E M I U M              *
05972 *                                                                *
05973 ******************************************************************
05974                                                                   
05975  3200-ADD-GOOD-ISSUE-PREM.                                        
05976      IF PB-OVERRIDE-LIFE                                          
05977        OR PB-OVERRIDE-BOTH                                        
05978            ADD PB-I-LF-PREM-CALC       TO  WS-ISS-GOOD-PREM       
05979            ADD PB-I-LF-ALT-PREM-CALC   TO  WS-ISS-GOOD-PREM       
05980        ELSE                                                       
05981            ADD PB-I-LF-PREMIUM-AMT     TO  WS-ISS-GOOD-PREM       
05982            ADD PB-I-LF-ALT-PREMIUM-AMT TO  WS-ISS-GOOD-PREM.      
05983                                                                   
05984      IF PB-OVERRIDE-AH                                            
05985        OR PB-OVERRIDE-BOTH                                        
05986            ADD PB-I-AH-PREMIUM-AMT     TO  WS-ISS-GOOD-PREM       
05987        ELSE                                                       
05988            ADD PB-I-AH-PREMIUM-AMT     TO  WS-ISS-GOOD-PREM.      
05989                                                                   
05990  3299-EXIT.                                                       
05991      EXIT.                                                        
05992                                                                   
05993      EJECT                                                        
05994                                                                   
05995                                                                   
05996 ******************************************************************
05997 *                                                                *
05998 *        A D D   B A D   C A N C E L   P R E M I U M             *
05999 *                                                                *
06000 ******************************************************************
06001                                                                   
06002  3300-ADD-BAD-CANCEL-PREM.                                        
06003      IF PB-OVERRIDE-LIFE                                          
06004        OR PB-OVERRIDE-BOTH                                        
06005            ADD PB-C-LF-REF-CALC   TO  WS-CAN-BAD-PREM             
06006        ELSE                                                       
06007            ADD PB-C-LF-CANCEL-AMT TO  WS-CAN-BAD-PREM.            
06008                                                                   
06009      IF PB-OVERRIDE-AH                                            
06010        OR PB-OVERRIDE-BOTH                                        
06011            ADD PB-C-AH-REF-CALC   TO  WS-CAN-BAD-PREM             
06012        ELSE                                                       
06013            ADD PB-C-AH-CANCEL-AMT TO  WS-CAN-BAD-PREM.            
06014                                                                   
06015  3399-EXIT.                                                       
06016      EXIT.                                                        
06017                                                                   
06018      EJECT                                                        
06019                                                                   
06020 ******************************************************************
06021 *                                                                *
06022 *       A D D   G O O D   C A N C E L   P R E M I U M            *
06023 *                                                                *
06024 ******************************************************************
06025                                                                   
06026  3400-ADD-GOOD-CANCEL-PREM.                                       
06027      IF PB-OVERRIDE-LIFE                                          
06028        OR PB-OVERRIDE-BOTH                                        
06029            ADD PB-C-LF-REF-CALC   TO  WS-CAN-GOOD-PREM            
06030        ELSE                                                       
06031            ADD PB-C-LF-CANCEL-AMT TO  WS-CAN-GOOD-PREM.           
06032                                                                   
06033      IF PB-OVERRIDE-AH                                            
06034        OR PB-OVERRIDE-BOTH                                        
06035            ADD PB-C-AH-REF-CALC   TO  WS-CAN-GOOD-PREM            
06036        ELSE                                                       
06037            ADD PB-C-AH-CANCEL-AMT TO  WS-CAN-GOOD-PREM.           
06038                                                                   
06039  3499-EXIT.                                                       
06040      EXIT.                                                        
06041                                                                   
06042      EJECT                                                        
06043                                                                   
06044 ******************************************************************
06045 *                                                                *
06046 *           E D I T   B A T C H   D A T A                        *
06047 *                                                                *
06048 ******************************************************************
06049                                                                   
06050  3500-EDIT-BATCH-DATA.                                            
06051      IF PI-PREV-KEY NOT = SPACES                                  
06052         MOVE PI-PREV-KEY         TO ERPNDB-KEY                    
06053      ELSE                                                         
06054         MOVE PI-ERPNDB-KEY       TO ERPNDB-KEY.                   
06055                                                                   
06056      MOVE +9999                  TO ERPNDB-BATCH-SEQ-NO.          
06057      MOVE ZEROS                  TO ERPNDB-BATCH-CHG-SEQ-NO.      
06058                                                                   
06059      EXEC CICS HANDLE CONDITION                                   
06060           NOTFND      (3590-NOT-FOUND)                            
06061      END-EXEC.                                                    
06062                                                                   
06063      IF DRISSL   GREATER THAN ZEROS OR                            
06064         DRISSL   GREATER THAN ZEROS OR                            
06065         DRLFPRML GREATER THAN ZEROS OR                            
06066         DRAHPRML GREATER THAN ZEROS OR                            
06067         DRCANCL  GREATER THAN ZEROS OR                            
06068         DRLFRFDL GREATER THAN ZEROS OR                            
06069         DRAHRFDL GREATER THAN ZEROS                               
06070               NEXT SENTENCE                                       
06071           ELSE                                                    
06072               MOVE ER-3224    TO EMI-ERROR                        
06073               MOVE -1         TO DPFENTRL                         
06074               MOVE AL-UNBON   TO DPFENTRA                         
06075               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            
06076               GO TO 8200-SEND-DATAONLY.                           
06077                                                                   
06078      IF NOT MODIFY-CAP                                            
06079          MOVE 'UPDATE'       TO SM-READ                           
06080          PERFORM 9995-SECURITY-VIOLATION                          
06081          MOVE ER-0070        TO EMI-ERROR                         
06082          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
06083          GO TO 8100-SEND-INITIAL-MAP.                             
06084                                                                   
06085         IF DRISSL GREATER THAN ZEROS                              
06086            MOVE AL-UNNON         TO DRISSA                        
06087            MOVE DRISSI           TO DEEDIT-FIELD                  
06088            PERFORM 8600-DEEDIT                                    
06089            IF DEEDIT-FIELD-V0  NUMERIC                            
06090               MOVE DEEDIT-FIELD-V0 TO WS-DRISS                    
06091            ELSE                                                   
06092               MOVE ER-2223    TO EMI-ERROR                        
06093               MOVE -1         TO DRISSL                           
06094               MOVE AL-UNBON   TO DRISSA                           
06095               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.           
06096                                                                   
06097         IF DRLFPRML GREATER THAN ZEROS                            
06098            MOVE AL-UNNON         TO DRLFPRMA                      
06099            EXEC CICS BIF DEEDIT
06100                FIELD  (DRLFPRMI)
06101                LENGTH (11)
06102            END-EXEC
06103            IF DRLFPRMI NUMERIC
06104               MOVE DRLFPRMI   TO WS-DRLFPRM
CIDMOD*          MOVE DRLFPRMI         TO DEEDIT-FIELD                  
CIDMOD*          PERFORM 8600-DEEDIT                                    
CIDMOD*          IF DEEDIT-FIELD-V2  NUMERIC                            
CIDMOD*             MOVE DEEDIT-FIELD-V2 TO WS-DRLFPRM                  
06105            ELSE                                                   
06106               MOVE ER-2223    TO EMI-ERROR                        
06107               MOVE -1         TO DRLFPRML                         
06108               MOVE AL-UNBON   TO DRLFPRMA                         
06109               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.           
06110                                                                   
06111         IF DRAHPRML GREATER THAN ZEROS                            
06112            MOVE AL-UNNON         TO DRAHPRMA                      
06113            EXEC CICS BIF DEEDIT
06114                FIELD  (DRAHPRMI)
06115                LENGTH (11)
06116            END-EXEC
06117            IF DRAHPRMI NUMERIC
06118               MOVE DRAHPRMI   TO WS-DRAHPRM
CIDMOD*          MOVE DRAHPRMI         TO DEEDIT-FIELD                  
CIDMOD*          PERFORM 8600-DEEDIT                                    
CIDMOD*          IF DEEDIT-FIELD-V2  NUMERIC                            
CIDMOD*             MOVE DEEDIT-FIELD-V2 TO WS-DRAHPRM                  
06119            ELSE                                                   
06120               MOVE ER-2223    TO EMI-ERROR                        
06121               MOVE -1         TO DRAHPRML                         
06122               MOVE AL-UNBON   TO DRAHPRMA                         
06123               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.           
06124                                                                   
06125         IF DRCANCL GREATER THAN ZEROS                             
06126            MOVE AL-UNNON         TO DRCANCA                       
06127            MOVE DRCANCI          TO DEEDIT-FIELD                  
06128            PERFORM 8600-DEEDIT                                    
06129            IF DEEDIT-FIELD-V0  NUMERIC                            
06130               MOVE DEEDIT-FIELD-V0 TO WS-DRCANC                   
06131            ELSE                                                   
06132               MOVE ER-2223    TO EMI-ERROR                        
06133               MOVE -1         TO DRCANCL                          
06134               MOVE AL-UNBON   TO DRCANCL                          
06135               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.           
06136                                                                   
06137         IF DRLFRFDL GREATER THAN ZEROS                            
06138            MOVE AL-UNNON         TO DRLFRFDA                      
06139            EXEC CICS BIF DEEDIT
06140                FIELD  (DRLFRFDI)
06141                LENGTH (11)
06142            END-EXEC
06143            IF DRLFRFDI NUMERIC
06144               MOVE DRLFRFDI   TO WS-DRLFRFD
CIDMOD*          MOVE DRLFRFDI         TO DEEDIT-FIELD                  
CIDMOD*          PERFORM 8600-DEEDIT                                    
CIDMOD*          IF DEEDIT-FIELD-V2  NUMERIC                            
CIDMOD*             MOVE DEEDIT-FIELD-V2 TO WS-DRLFRFD                  
06145            ELSE                                                   
06146               MOVE ER-2223    TO EMI-ERROR                        
06147               MOVE -1         TO DRLFRFDL                         
06148               MOVE AL-UNBON   TO DRLFRFDA                         
06149               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.           
06150                                                                   
06151         IF DRAHRFDL GREATER THAN ZEROS                            
06152            MOVE AL-UNNON         TO DRAHRFDA                      
06153            EXEC CICS BIF DEEDIT
06154                FIELD  (DRAHRFDI)
06155                LENGTH (11)
06156            END-EXEC
06157            IF DRAHRFDI NUMERIC
06158               MOVE DRAHRFDI   TO WS-DRAHRFD
CIDMOD*          MOVE DRAHRFDI         TO DEEDIT-FIELD                  
CIDMOD*          PERFORM 8600-DEEDIT                                    
CIDMOD*          IF DEEDIT-FIELD-V2  NUMERIC                            
CIDMOD*             MOVE DEEDIT-FIELD-V2 TO WS-DRAHRFD                  
06159            ELSE                                                   
06160               MOVE ER-2223    TO EMI-ERROR                        
06161               MOVE -1         TO DRAHRFDL                         
06162               MOVE AL-UNBON   TO DRAHRFDA                         
06163               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.           
06164                                                                   
06165      IF EMI-ERROR = ZEROS                                         
06166         GO 3600-UPDATE-BATCH-HEADER.                              
06167                                                                   
06168      GO TO 8200-SEND-DATAONLY.                                    
06169                                                                   
06170  3590-NOT-FOUND.                                                  
06171      MOVE ER-2192                TO EMI-ERROR.                    
06172      MOVE -1                     TO DPFENTRL.                     
06173      MOVE AL-UNBON               TO DPFENTRA.                     
06174      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
06175      GO TO 8200-SEND-DATAONLY.                                    
06176                                                                   
06177      EJECT                                                        
06178                                                                   
06179 ******************************************************************
06180 *                                                                *
06181 *           U P D A T E   B A T C H   H E A D E R                *
06182 *                                                                *
06183 ******************************************************************
06184                                                                   
06185  3600-UPDATE-BATCH-HEADER.                                        
06186      EXEC CICS HANDLE CONDITION                                   
06187           NOTFND      (3690-NOT-FOUND)                            
06188      END-EXEC.                                                    
06189                                                                   
06190      EXEC CICS READ                                               
06191          DATASET (FILE-ID-ERPNDB)                                 
06192          RIDFLD  (ERPNDB-KEY)                                     
06193          SET     (ADDRESS OF PENDING-BUSINESS)                    
06194          UPDATE                                                   
06195      END-EXEC.                                                    
06196                                                                   
06197      MOVE FILE-ID-ERPNDB         TO JP-FILE-ID.                   
06198      MOVE 'B'                    TO JP-RECORD-TYPE.               
06199      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
06200                                                                   
06201      MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
06202                                                                   
06203      PERFORM 8400-LOG-JOURNAL-RECORD.                             
06204                                                                   
06205      IF DRISSL GREATER THAN ZEROS                                 
06206         MOVE WS-DRISS            TO PB-B-ISSUE-CNT-REMITTED.      
06207                                                                   
06208      IF DRCANCL GREATER THAN ZEROS                                
06209         MOVE WS-DRCANC           TO PB-B-CANCEL-CNT-REMITTED.     
06210                                                                   
06211                                                                   
06212      IF DRLFPRML GREATER THAN ZEROS                               
06213         MOVE WS-DRLFPRM       TO PB-B-LF-ISS-PRM-REMITTED.        
06214                                                                   
06215      IF DRAHPRML GREATER THAN ZEROS                               
06216         MOVE WS-DRAHPRM       TO PB-B-AH-ISS-PRM-REMITTED.        
06217                                                                   
06218      IF DRLFRFDL GREATER THAN ZEROS                               
06219         MOVE WS-DRLFRFD       TO PB-B-LF-CAN-PRM-REMITTED.        
06220                                                                   
06221      IF DRAHRFDL GREATER THAN ZEROS                               
06222         MOVE WS-DRAHRFD       TO PB-B-AH-CAN-PRM-REMITTED.        
06223                                                                   
06224  3670-REWRITE-BATCH-HEADER.                                       
06225      IF PB-B-ISSUE-CNT-REMITTED  =  PB-B-ISSUE-CNT-ENTERED   AND  
06226         PB-B-LF-ISS-PRM-ENTERED  =  PB-B-LF-ISS-PRM-REMITTED AND  
06227         PB-B-AH-ISS-PRM-ENTERED  =  PB-B-AH-ISS-PRM-REMITTED AND  
06228         PB-B-CANCEL-CNT-REMITTED =  PB-B-CANCEL-CNT-ENTERED  AND  
06229         PB-B-LF-CAN-PRM-ENTERED  =  PB-B-LF-CAN-PRM-REMITTED AND  
06230         PB-B-AH-CAN-PRM-ENTERED  =  PB-B-AH-CAN-PRM-REMITTED      
06231         MOVE SPACE               TO PB-OUT-BAL-CD                 
06232      ELSE                                                         
06233         MOVE 'O'                 TO PB-OUT-BAL-CD.                
06234                                                                   
06235      MOVE 'C'                    TO JP-RECORD-TYPE.               
06236      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
06237                                                                   
06238      EXEC CICS REWRITE                                            
06239           DATASET      (FILE-ID-ERPNDB)                           
06240           FROM         (PENDING-BUSINESS)                         
06241      END-EXEC.                                                    
06242                                                                   
06243      PERFORM 8400-LOG-JOURNAL-RECORD.                             
06244                                                                   
06245      GO TO 7200-DISPLAY-BATCHES.                                  
06246                                                                   
06247  3690-NOT-FOUND.                                                  
06248      MOVE ER-2192                TO EMI-ERROR.                    
06249      MOVE -1                     TO DPFENTRL.                     
06250      MOVE AL-UNBON               TO DPFENTRA.                     
06251      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
06252      GO TO 8200-SEND-DATAONLY.                                    
06253                                                                   
06254      EJECT                                                        
06255                                                                   
06256 ******************************************************************
06257 *                                                                *
06258 *           R E - D I S P L A Y   R E C O R D                    *
06259 *                                                                *
06260 ******************************************************************
06261                                                                   
06262  3900-RE-DISPLAY-RECORD.                                          
06263      EXEC CICS HANDLE CONDITION                                   
06264           NOTFND   (3990-NOT-FOUND)                               
06265      END-EXEC.                                                    
06266                                                                   
06267      IF PI-DISPLAY-ORIGINAL-BATCH                                 
06268          MOVE FILE-ID-ERPNDB3     TO FILE-ID                      
06269      ELSE                                                         
06270          MOVE FILE-ID-ERPNDB      TO FILE-ID.                     
06271                                                                   
06272      EXEC CICS READ                                               
06273           DATASET   (FILE-ID)                                     
06274           RIDFLD    (PI-PREV-KEY)                                 
06275           SET       (ADDRESS OF PENDING-BUSINESS)                 
06276      END-EXEC.                                                    
06277                                                                   
06278      MOVE PI-PREV-KEY            TO ERPNDB-KEY.                   
06279                                                                   
06280      IF PB-ISSUE                                                  
06281          GO TO 7000-DISPLAY-ISSUES.                               
06282                                                                   
06283      IF PB-CANCELLATION                                           
06284          GO TO 7100-DISPLAY-CANCELS.                              
06285                                                                   
06286      IF PB-BATCH-TRAILER                                          
06287          GO TO 7200-DISPLAY-BATCHES.                              
06288                                                                   
06289  3990-NOT-FOUND.    
06290      MOVE LOW-VALUES             TO VP631BI.                      
06291                                                                   
06292      IF PI-MAP-NAME = VP631B                                      
06293          MOVE -1                  TO BMAINTL                      
06294      ELSE                                                         
06295          MOVE -1                  TO CMAINTL.                     
06296                                                                   
06297      GO TO 8100-SEND-INITIAL-MAP.                                 
06298                                                                   
06299      EJECT                                                        
06300                                                                   
06301 ***************************************************************   
06302 *     FORWARD BROWSING ROUTINE   (BY ALTERNATE KEY)           *   
06303 *                                                             *   
06304 *      1. START BROWSE WITH                                   *   
06305 *         A. FIRST RECORD SPECIFIED FROM FIRST SCREEN         *   
06306 *         B. LAST RECORD DISPLAYED FROM PREVIOUS SCREEN       *   
06307 *         C. RECORD REQUESTED VIA A SHOW MAINT REQUEST.       *   
06308 *                                                             *   
06309 *      2. READ RECORDS AND DETERMINE IF THEY ARE TO BE        *   
06310 *         DISPLAYED BY USING THE FOLLOWING CHECKS.            *   
06311 *         A. IS IT WITHIN THE SAME KEY GROUP?                 *   
06312 *         B. IF IT IS A BILLED REC AND IS IT TO BE DISPLAYED. *   
06313 *         C. IF IT HAS BEEN PROCESSED THRU THE BATCH SYSTEM   *   
06314 *               AND IS IT TO BE DISPLAYED.                    *   
06315 *         D. IF IT IS AN ISSUE AND IF ERRORS ARE TO BE        *   
06316 *               DISPLAYED.                                    *   
06317 *         E. IF IT IS AN CANCEL AND IF ERRORS ARE TO BE       *   
06318 *               DISPLAYED.                                    *   
06319 *                                                             *   
06320 *      3. IF THE RECORD MEETS ALL REQUIREMENTS, SAVE THE      *   
06321 *               RECORD KEY AND GO TO THE DISPLAY ROUTINE.     *   
06322 ***************************************************************   
06323                                                                   
06324  4000-BROWSE-DETAIL-FORWARD.                                      
06325      IF FIRST-TIME                                                
06326         MOVE PI-ERPNDB-ALT-KEY      TO ERPNDB-ALT-KEY             
06327      ELSE                                                         
06328         IF SHOW-RECORD                                            
06329            MOVE PI-ERPNDB-ALT-KEY   TO ERPNDB-ALT-KEY             
06330            MOVE WS-NEW-CERT-NO      TO ERPNDB-CERT-NO             
06331            MOVE WS-NEW-CERT-EFF-DT  TO ERPNDB-CERT-EFF-DT         
06332            MOVE LOW-VALUES          TO ERPNDB-RECORD-TYPE         
06333            MOVE ZEROS               TO ERPNDB-ALT-CHG-SEQ-NO      
06334         ELSE                                                      
06335            MOVE PI-PREV-ALT-KEY     TO ERPNDB-ALT-KEY.            
06336                                                                   
06337      EXEC CICS HANDLE CONDITION                                   
06338           NOTFND   (4050-END-RECORDS)                             
06339           ENDFILE  (4050-END-RECORDS)                             
06340      END-EXEC.                                                    
06341                                                                   
06342      EXEC CICS STARTBR                                            
06343           DATASET   (FILE-ID-ERPNDB2)                             
06344           RIDFLD    (ERPNDB-ALT-KEY)                              
06345      END-EXEC.                                                    
06346                                                                   
06347      MOVE 'Y'                    TO WS-BROWSE-STARTED-SW.         
06348                                                                   
06349      IF FIRST-TIME OR SHOW-RECORD                                 
06350         MOVE SPACE               TO WS-FIRST-TIME-SW              
06351         GO TO 4005-READ-NEXT-RECORD.                              
06352                                                                   
06353      EXEC CICS READNEXT                                           
06354           DATASET    (FILE-ID-ERPNDB2)                            
06355           SET        (ADDRESS OF PENDING-BUSINESS)                
06356           RIDFLD     (ERPNDB-ALT-KEY)                             
06357      END-EXEC.                                                    
06358                                                                   
06359  4005-READ-NEXT-RECORD.                                           
06360      EXEC CICS READNEXT                                           
06361           DATASET    (FILE-ID-ERPNDB2)                            
06362           SET        (ADDRESS OF PENDING-BUSINESS)                
06363           RIDFLD     (ERPNDB-ALT-KEY)                             
06364      END-EXEC.                                                    
06365                                                                   
06366      MOVE PB-ENTRY-BATCH         TO WS-BATCH-NO.                  
06367                                                                   
06368      IF WS-BATCH-PREFIX = '#CL'                                   
06369         GO TO 4005-READ-NEXT-RECORD.                              
06370                                                                   
06371      IF SHOW-RECORD                                               
06372         IF PB-COMPANY-CD-A1 NOT = PI-COMPANY-CD        OR         
06373            PB-CARRIER       NOT = PI-PB-CARRIER        OR         
06374            PB-GROUPING      NOT = PI-PB-GROUPING       OR         
06375            PB-STATE         NOT = PI-PB-STATE          OR         
06376            PB-ACCOUNT       NOT = PI-PB-ACCOUNT        OR         
06377            PB-CERT-EFF-DT   NOT = WS-NEW-CERT-EFF-DT   OR         
06378            PB-CERT-NO       NOT = WS-NEW-CERT-NO                  
06379            GO TO 4050-END-RECORDS                                 
06380         ELSE                                                      
06381            GO TO 4010-CHECK-REC-TYPE.                             
06382                                                                   
06383      IF PB-COMPANY-CD-A1    NOT = PI-COMPANY-CD        OR         
06384         PB-CARRIER          NOT = PI-PB-CARRIER        OR         
06385         PB-GROUPING         NOT = PI-PB-GROUPING       OR         
06386         PB-STATE            NOT = PI-PB-STATE          OR         
06387         PB-ACCOUNT          NOT = PI-PB-ACCOUNT                   
06388         GO TO 4050-END-RECORDS.                                   
06389                                                                   
06390      IF PB-BATCH-TRAILER                                          
06391         GO TO 4005-READ-NEXT-RECORD.                              
06392                                                                   
06393      IF PI-PB-CERT-NO NOT = LOW-VALUES                            
06394         IF PI-PB-CERT-NO NOT = PB-CERT-NO                         
06395            GO TO 4005-READ-NEXT-RECORD                            
06396         ELSE                                                      
06397            IF PI-PB-CERT-EFF-DT = LOW-VALUES                      
06398               GO TO 4010-CHECK-REC-TYPE                           
06399            ELSE                                                   
06400               IF PI-PB-CERT-EFF-DT = PB-CERT-EFF-DT               
06401                  GO TO 4010-CHECK-REC-TYPE                        
06402               ELSE                                                
06403                  GO TO 4005-READ-NEXT-RECORD.                     
06404                                                                   
06405      IF PI-PB-CERT-EFF-DT NOT = LOW-VALUES                        
06406         IF PI-PB-CERT-EFF-DT NOT = PB-CERT-EFF-DT                 
06407            GO TO 4005-READ-NEXT-RECORD.                           
06408                                                                   
06409  4010-CHECK-REC-TYPE.                                             
06410      IF  PI-CARRIER-SECURITY GREATER THAN SPACES                  
06411          IF  PB-CARRIER = PI-CARRIER-SECURITY                     
06412              NEXT SENTENCE                                        
06413          ELSE                                                     
06414              GO TO 4005-READ-NEXT-RECORD.                         
06415                                                                   
06416      IF  PI-ACCOUNT-SECURITY GREATER THAN SPACES                  
06417          IF  PB-ACCOUNT = PI-ACCOUNT-SECURITY                     
06418              NEXT SENTENCE                                        
06419         ELSE                                                      
06420              GO TO 4005-READ-NEXT-RECORD.                         
06421                                                                   
06422      IF NOT DISPLAY-CHANGE-RECORDS AND                            
06423             PB-BATCH-CHG-SEQ-NO NOT = ZEROS                       
06424         GO TO 4005-READ-NEXT-RECORD.                              
06425                                                                   
06426      IF DISPLAY-HOLD-RECORDS AND                                  
06427         NOT PB-RECORD-ON-HOLD                                     
06428           GO TO 4005-READ-NEXT-RECORD.                            
06429                                                                   
06430      IF PB-ISSUE                                                  
06431         IF DISPLAY-CHK-REQ-RECORDS                                
06432             GO TO 4005-READ-NEXT-RECORD.                          
06433                                                                   
06434      IF PB-ISSUE                                                  
06435         IF ISSUE-WITH-WARNING                                     
06436            IF PB-WARNING-ERRORS                                   
06437               IF (PB-UNFORCED-ERRORS OR PB-FATAL-ERRORS)          
06438                   GO TO 4005-READ-NEXT-RECORD                     
06439               ELSE                                                
06440                  MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY   
06441                  MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY       
06442                  MOVE PB-SV-CARRIER          TO PI-SV-CARRIER     
06443                  MOVE PB-SV-GROUPING         TO PI-SV-GROUPING    
06444                  MOVE PB-SV-STATE            TO PI-SV-STATE       
06445                  MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW               
06446                  GO TO 7000-DISPLAY-ISSUES                        
06447            ELSE                                                   
06448               GO TO 4005-READ-NEXT-RECORD.                        
06449                                                                   
06450      IF PB-ISSUE                                                  
06451         IF (ISSUES-IN-ERROR AND                                   
06452             (PB-UNFORCED-ERRORS OR PB-FATAL-ERRORS)) OR           
06453             ALL-ISSUES                                            
06454             MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY        
06455             MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY            
06456             MOVE PB-SV-CARRIER          TO PI-SV-CARRIER          
06457             MOVE PB-SV-GROUPING         TO PI-SV-GROUPING         
06458             MOVE PB-SV-STATE            TO PI-SV-STATE            
06459             MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW                    
06460             GO TO 7000-DISPLAY-ISSUES                             
06461         ELSE                                                      
06462             GO TO 4005-READ-NEXT-RECORD.                          
06463                                                                   
06464      IF PB-CANCELLATION                                           
06465         IF DISPLAY-CHK-REQ-RECORDS                                
06466            IF PB-C-REFUND-REQUESTED                               
06467               MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY      
06468               MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY          
06469               MOVE PB-SV-CARRIER          TO PI-SV-CARRIER        
06470               MOVE PB-SV-GROUPING         TO PI-SV-GROUPING       
06471               MOVE PB-SV-STATE            TO PI-SV-STATE          
06472               MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW                  
06473               GO TO 7100-DISPLAY-CANCELS                          
06474            ELSE                                                   
06475                GO TO 4005-READ-NEXT-RECORD.                       
06476                                                                   
06477      IF PB-CANCELLATION                                           
06478         IF CANCEL-WITH-WARNING                                    
06479            IF PB-WARNING-ERRORS                                   
06480               IF (PB-UNFORCED-ERRORS OR PB-FATAL-ERRORS)          
06481                   GO TO 4005-READ-NEXT-RECORD                     
06482               ELSE                                                
06483                  MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY   
06484                  MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY       
06485                  MOVE PB-SV-CARRIER          TO PI-SV-CARRIER     
06486                  MOVE PB-SV-GROUPING         TO PI-SV-GROUPING    
06487                  MOVE PB-SV-STATE            TO PI-SV-STATE       
06488                  MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW               
06489                  GO TO 7100-DISPLAY-CANCELS                       
06490            ELSE                                                   
06491               GO TO 4005-READ-NEXT-RECORD.                        
06492                                                                   
06493      IF PB-CANCELLATION                                           
06494         IF (CANCEL-IN-ERROR  AND                                  
06495             (PB-UNFORCED-ERRORS OR PB-FATAL-ERRORS)) OR           
06496             ALL-CANCELS                                           
06497             MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY        
06498             MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY            
06499             MOVE PB-SV-CARRIER          TO PI-SV-CARRIER          
06500             MOVE PB-SV-GROUPING         TO PI-SV-GROUPING         
06501             MOVE PB-SV-STATE            TO PI-SV-STATE            
06502             MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW                    
06503             GO TO 7100-DISPLAY-CANCELS.                           
06504                                                                   
06505      GO TO 4005-READ-NEXT-RECORD.                                 
06506                                                                   
06507  4050-END-RECORDS.                                                
06508 ******************************************************************
06509 *                                                                *
06510 *         IF THERE ARE NO PENDING BUS. RECORDS FOUND DURING      *
06511 *         THE INITIAL ENTRY OF VP6311, RETURN TO THE CALLING      
06512 *         PROGRAM.                                               *
06513 *                                                                *
06514 ******************************************************************
06515                                                                   
06516      IF BROWSE-STARTED                                            
06517         MOVE SPACE               TO WS-BROWSE-STARTED-SW          
06518         EXEC CICS ENDBR                                           
06519              DATASET  (FILE-ID-ERPNDB2)                           
06520         END-EXEC.                                                 
06521                                                                   
06522      IF  EIBTRNID NOT = TRANS-EXB1                                
06523          IF PENDING-BUS-RECS-FOUND                                
06524              NEXT SENTENCE                                        
06525            ELSE                                                   
06526              MOVE '9'            TO PI-BROWSE-SW                  
06527              GO TO 9400-CLEAR.                                    
06528                                                                   
06529      IF SHOW-RECORD                                               
06530         MOVE ER-2253            TO EMI-ERROR                      
06531        ELSE                                                       
06532         MOVE ER-2254            TO EMI-ERROR.                     
06533                                                                   
06534      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
06535                                                                   
06536      IF PI-MAP-NAME = SPACES                                      
06537         MOVE -1                  TO DPFENTRL                      
06538         MOVE VP631D               TO PI-MAP-NAME                  
06539         GO TO 8100-SEND-INITIAL-MAP.                              
06540                                                                   
06541      IF PI-MAP-NAME = VP631B                                      
06542         MOVE -1                  TO BMAINTL                       
06543      ELSE                                                         
06544         IF PI-MAP-NAME = VP631C                                   
06545            MOVE -1               TO CMAINTL                       
06546         ELSE                                                      
06547            MOVE -1               TO DPFENTRL.                     
06548                                                                   
06549      GO TO 8200-SEND-DATAONLY.                                    
06550                                                                   
06551      EJECT                                                        
06552                                                                   
06553 ***************************************************************   
06554 *                                                             *   
06555 *     FORWARD BROWSING ROUTINE   (BY BATCH)                   *   
06556 *                                                             *   
06557 *       1. READ RECORDS WITHIN THE BATCH BASED ON             *   
06558 *          A. FIRST TIME INTO PROGRAM FROM FIRST SCREEN       *   
06559 *          B. LAST RECORD DISPLAYED FROM PREVIOUS SCREEN      *   
06560 *          C. RECORD REQUESTED VIA A SHOW MAINT REQUEST.      *   
06561 *                                                             *   
06562 *       2. READ RECORDS AND DETERMINE IF THEY ARE TO BE       *   
06563 *          DISPLAYED BY USING THE FOLLOWING CHECKS.           *   
06564 *          A. IS IT WITHIN THE SAME KEY GROUP?                *   
06565 *          B. IF IT IS A BILLED REC AND IS IT TO BE DISPLAYED.*   
06566 *          C. IF IT HAS BEEN PROCESSED THRU THE BATCH SYSTEM  *   
06567 *                AND IS IT TO BE DISPLAYED.                   *   
06568 *          D. IF IT IS AN ISSUE AND IF ERRORS ARE TO BE       *   
06569 *                DISPLAYED.                                   *   
06570 *          E. IF IT IS AN CANCEL AND IF ERRORS ARE TO BE      *   
06571 *                DISPLAYED.                                   *   
06572 *                                                             *   
06573 *       3. IF THE RECORD MEETS ALL REQUIREMENTS, SAVE THE     *   
06574 *                RECORD KEY AND GO TO THE DISPLAY ROUTINE.    *   
06575 *                                                             *   
06576 *       4. WHEN READING ADDITION RECORDS ADJUSTMENTS MUST BE  *   
06577 *                MADE TO BYPASS THE CORRECTION RECORDS IF     *   
06578 *                THEY ARE NOT TO BE DISPLAYED.                *   
06579 *                                                             *   
06580 ***************************************************************   
06581                                                                   
06582  4100-BROWSE-PRIMARY-FORWARD.                                     
06583      IF FIRST-TIME  OR SHOW-RECORD                                
06584         MOVE PI-ERPNDB-KEY         TO ERPNDB-KEY                  
06585         MOVE +1                    TO ERPNDB-BATCH-SEQ-NO         
06586         MOVE ZEROS                 TO ERPNDB-BATCH-CHG-SEQ-NO     
06587      ELSE                                                         
06588         MOVE PI-PREV-KEY           TO ERPNDB-KEY                  
06589         IF ERPNDB-BATCH-SEQ-NO = +9999                            
06590            GO TO 4150-END-BATCH                                   
06591         ELSE                                                      
06592            IF DISPLAY-CHANGE-RECORDS                              
06593               ADD +1             TO ERPNDB-BATCH-CHG-SEQ-NO       
06594            ELSE                                                   
06595               MOVE ZEROS         TO ERPNDB-BATCH-CHG-SEQ-NO       
06596               ADD +1             TO ERPNDB-BATCH-SEQ-NO.          
06597                                                                   
06598      IF FIRST-TIME    
06599          IF ERPNDB-BATCH-SEQ-NO = +1  AND                         
06600             PI-PB-BATCH-SEQ-NO NOT = ZERO                         
06601               MOVE PI-PB-BATCH-SEQ-NO TO ERPNDB-BATCH-SEQ-NO.     
06602                                                                   
06603      EXEC CICS HANDLE CONDITION                                   
06604           ENDFILE  (4150-END-BATCH)                               
06605      END-EXEC.                                                    
06606                     
06607  4105-READ-NEXT.                                                  
06608      EXEC CICS HANDLE CONDITION                                   
06609           NOTFND    (4150-END-BATCH)                              
06610      END-EXEC.                                                    
06611                                                                   
06612      MOVE SPACES                 TO WS-REC-FOUND-SW.              
06613                                                                   
06614      IF PI-DISPLAY-ORIGINAL-BATCH                                 
06615         MOVE FILE-ID-ERPNDB3     TO FILE-ID                       
06616      ELSE                                                         
06617         MOVE FILE-ID-ERPNDB      TO FILE-ID.                      
06618                                                                   
06619      EXEC CICS READ                                               
06620           DATASET   (FILE-ID)                                     
06621           SET       (ADDRESS OF PENDING-BUSINESS)                 
06622           RIDFLD    (ERPNDB-KEY)                                  
06623           GTEQ                                                    
06624      END-EXEC.                                                    
06625                                                                   
06626      IF PI-DISPLAY-ORIGINAL-BATCH                                 
06627         IF (PB-ORIGINAL-COMPANY-CD  NOT = PI-COMPANY-CD) OR       
06628            (PB-ORIGINAL-ENTRY-BATCH NOT = PI-PB-ENTRY-BATCH)      
06629            GO TO 4150-END-BATCH                                   
06630         ELSE                                                      
06631            IF PB-BATCH-TRAILER                                    
06632               MOVE HIGH-VALUES   TO ERPNDB-BATCH-SEQ-ALPHA        
06633               GO TO 4105-READ-NEXT                                
06634            ELSE                                                   
06635               NEXT SENTENCE                                       
06636      ELSE                                                         
06637         IF (PB-COMPANY-CD  NOT = PI-COMPANY-CD) OR                
06638            (PB-ENTRY-BATCH NOT = PI-PB-ENTRY-BATCH)               
06639            GO TO 4150-END-BATCH.                                  
06640                                                                   
06641      MOVE PB-ENTRY-BATCH         TO WS-BATCH-NO.                  
06642                                                                   
06643      IF WS-BATCH-PREFIX = '#CL'                                   
06644          MOVE HIGH-VALUES        TO ERPNDB-BATCH-SEQ-ALPHA        
06645          GO TO 4105-READ-NEXT.                                    
06646                                                                   
06647      MOVE PB-CARRIER             TO PI-PB-CARRIER.                
06648      MOVE PB-GROUPING            TO PI-PB-GROUPING.               
06649      MOVE PB-STATE               TO PI-PB-STATE.                  
06650      MOVE PB-ACCOUNT             TO PI-PB-ACCOUNT.                
06651                                                                   
06652      IF PB-BATCH-TRAILER                                          
06653         IF SHOW-RECORD                                            
06654            GO TO 4150-END-BATCH                                   
06655         ELSE                                                      
06656            MOVE ER-2251         TO EMI-ERROR                      
06657            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
06658            MOVE 1               TO PI-BROWSE-TYPE                 
06659            GO TO 7200-DISPLAY-BATCHES.                            
06660                                                                   
06661      IF SHOW-RECORD                                               
06662            IF WS-NEW-CERT-NO NOT = PB-CERT-NO                     
06663               GO TO 4110-BUMP-SEQ                                 
06664            ELSE                                                   
06665               IF WS-NEW-CERT-EFF-DT NOT = PB-CERT-EFF-DT          
06666                  GO TO 4110-BUMP-SEQ                              
06667               ELSE                                                
06668                  GO TO 4107-CHECK-REC-TYPE.                       
06669                                                                   
06670      IF NOT  PI-PRIMARY-WITH-SELECT                               
06671         GO TO 4107-CHECK-REC-TYPE.                                
06672                                                                   
06673      IF PI-PB-CERT-NO NOT = LOW-VALUES                            
06674         IF PI-PB-CERT-NO NOT = PB-CERT-NO                         
06675            GO TO 4110-BUMP-SEQ                                    
06676         ELSE                                                      
06677            IF PI-PB-CERT-EFF-DT = LOW-VALUES                      
06678               GO TO 4107-CHECK-REC-TYPE                           
06679            ELSE                                                   
06680               IF PI-PB-CERT-EFF-DT = PB-CERT-EFF-DT               
06681                  GO TO 4107-CHECK-REC-TYPE                        
06682               ELSE                                                
06683                  GO TO 4110-BUMP-SEQ.                             
06684                                                                   
06685      IF PI-PB-CERT-EFF-DT NOT = LOW-VALUES                        
06686         IF PI-PB-CERT-EFF-DT NOT = PB-CERT-EFF-DT                 
06687            GO TO 4110-BUMP-SEQ.                                   
06688                                                                   
06689  4107-CHECK-REC-TYPE.                                             
06690      IF  PI-CARRIER-SECURITY GREATER THAN SPACES                  
06691          IF  PB-CARRIER = PI-CARRIER-SECURITY                     
06692              NEXT SENTENCE                                        
06693            ELSE                                                   
06694              GO TO 4110-BUMP-SEQ.                                 
06695                                                                   
06696      IF  PI-ACCOUNT-SECURITY GREATER THAN SPACES                  
06697          IF  PB-ACCOUNT = PI-ACCOUNT-SECURITY                     
06698              NEXT SENTENCE                                        
06699            ELSE                                                   
06700              GO TO 4110-BUMP-SEQ.                                 
06701                                                                   
06702      IF NOT DISPLAY-CHANGE-RECORDS AND                            
06703         PB-BATCH-CHG-SEQ-NO NOT = ZEROS                           
06704            GO TO 4110-BUMP-SEQ.                                   
06705                                                                   
06706      IF DISPLAY-HOLD-RECORDS AND                                  
06707         NOT PB-RECORD-ON-HOLD                                     
06708           GO TO 4110-BUMP-SEQ.                                    
06709                                                                   
06710      IF PB-ISSUE                                                  
06711         IF DISPLAY-CHK-REQ-RECORDS                                
06712             GO TO 4110-BUMP-SEQ.                                  
06713                                                                   
06714      IF PB-ISSUE                                                  
06715         IF ISSUE-WITH-WARNING                                     
06716            IF PB-WARNING-ERRORS                                   
06717               IF (PB-UNFORCED-ERRORS OR PB-FATAL-ERRORS)          
06718                   GO TO 4110-BUMP-SEQ                             
06719               ELSE                                                
06720                  MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY   
06721                  MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY       
06722                  MOVE PB-SV-CARRIER          TO PI-SV-CARRIER     
06723                  MOVE PB-SV-GROUPING         TO PI-SV-GROUPING    
06724                  MOVE PB-SV-STATE            TO PI-SV-STATE       
06725                  MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW               
06726                  GO TO 7000-DISPLAY-ISSUES                        
06727         ELSE                                                      
06728             GO TO 4110-BUMP-SEQ.                                  
06729                                                                   
06730      IF PB-ISSUE                                                  
06731         IF (ISSUES-IN-ERROR AND                                   
06732             (PB-UNFORCED-ERRORS OR PB-FATAL-ERRORS)) OR           
06733             ALL-ISSUES                                            
06734             MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY        
06735             MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY            
06736             MOVE PB-SV-CARRIER          TO PI-SV-CARRIER          
06737             MOVE PB-SV-GROUPING         TO PI-SV-GROUPING         
06738             MOVE PB-SV-STATE            TO PI-SV-STATE            
06739             MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW                    
06740             GO TO 7000-DISPLAY-ISSUES                             
06741         ELSE                                                      
06742             GO TO 4110-BUMP-SEQ.                                  
06743                                                                   
06744      IF PB-CANCELLATION                                           
06745         IF DISPLAY-CHK-REQ-RECORDS                                
06746            IF PB-C-REFUND-REQUESTED                               
06747               MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY      
06748               MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY          
06749               MOVE PB-SV-CARRIER          TO PI-SV-CARRIER        
06750               MOVE PB-SV-GROUPING         TO PI-SV-GROUPING       
06751               MOVE PB-SV-STATE            TO PI-SV-STATE          
06752               MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW                  
06753               GO TO 7100-DISPLAY-CANCELS                          
06754            ELSE                                                   
06755               GO TO 4110-BUMP-SEQ.                                
06756                                                                   
06757      IF PB-CANCELLATION                                           
06758         IF CANCEL-WITH-WARNING                                    
06759            IF PB-WARNING-ERRORS                                   
06760               IF (PB-UNFORCED-ERRORS OR PB-FATAL-ERRORS)          
06761                   GO TO 4110-BUMP-SEQ                             
06762               ELSE                                                
06763                  MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY   
06764                  MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY       
06765                  MOVE PB-SV-CARRIER          TO PI-SV-CARRIER     
06766                  MOVE PB-SV-GROUPING         TO PI-SV-GROUPING    
06767                  MOVE PB-SV-STATE            TO PI-SV-STATE       
06768                  MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW               
06769                  GO TO 7100-DISPLAY-CANCELS                       
06770            ELSE                                                   
06771               GO TO 4110-BUMP-SEQ.                                
06772                                                                   
06773      IF PB-CANCELLATION                                           
06774         IF (CANCEL-IN-ERROR  AND                                  
06775             (PB-UNFORCED-ERRORS OR PB-FATAL-ERRORS)) OR           
06776             ALL-CANCELS                                           
06777             MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY        
06778             MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY            
06779             MOVE PB-SV-CARRIER          TO PI-SV-CARRIER          
06780             MOVE PB-SV-GROUPING         TO PI-SV-GROUPING         
06781             MOVE PB-SV-STATE            TO PI-SV-STATE            
06782             MOVE 'Y'                    TO WS-PB-RECORDS-FOUND-SW 
06783             GO TO 7100-DISPLAY-CANCELS.                           
06784                                                                   
06785  4110-BUMP-SEQ.                                                   
06786      IF NOT DISPLAY-CHANGE-RECORDS                                
06787         MOVE ZEROS               TO ERPNDB-BATCH-CHG-SEQ-NO       
06788         ADD +1                   TO ERPNDB-BATCH-SEQ-NO           
06789      ELSE                                                         
06790         IF PB-BATCH-SEQ-NO NOT = ERPNDB-BATCH-SEQ-NO              
06791            MOVE PB-BATCH-SEQ-NO  TO ERPNDB-BATCH-SEQ-NO           
06792            MOVE +999             TO ERPNDB-BATCH-CHG-SEQ-NO       
06793         ELSE                                                      
06794            MOVE PB-BATCH-CHG-SEQ-NO  TO ERPNDB-BATCH-CHG-SEQ-NO   
06795            ADD +1                    TO ERPNDB-BATCH-CHG-SEQ-NO.  
06796                                                                   
06797      GO TO 4105-READ-NEXT.                                        
06798                                                                   
06799  4150-END-BATCH.                                                  
06800 ******************************************************************
06801 *                                                                *
06802 *         IF THERE ARE NO PENDING BUS. RECORDS FOUND DURING      *
06803 *         THE INITIAL ENTRY OF VP6311, RETURN TO THE CALLING     *
06804 *         PROGRAM.                                               *
06805 *                                                                *
06806 ******************************************************************
06807                                                                   
06808      IF  EIBTRNID NOT = TRANS-EXB1                                
06809          IF PENDING-BUS-RECS-FOUND                                
06810              NEXT SENTENCE                                        
06811            ELSE                                                   
06812              MOVE '9'            TO PI-BROWSE-SW                  
06813              GO TO 9400-CLEAR.                                    
06814                                                                   
06815      IF SHOW-RECORD                                               
06816         MOVE ER-2253             TO EMI-ERROR                     
06817      ELSE                                                         
06818         MOVE ER-2251             TO EMI-ERROR.                    
06819                                                                   
06820      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
06821                                                                   
06822      IF PI-MAP-NAME = SPACES                                      
06823         MOVE -1                  TO BMAINTL                       
06824         MOVE VP631B              TO PI-MAP-NAME                   
06825         GO TO 8100-SEND-INITIAL-MAP.                              
06826                                                                   
06827      IF PI-MAP-NAME = VP631B                                      
06828         MOVE -1                  TO BMAINTL                       
06829      ELSE                                                         
06830         IF PI-MAP-NAME = VP631C                                   
06831            MOVE -1               TO CMAINTL                       
06832         ELSE                                                      
06833            MOVE -1               TO DPFENTRL.                     
06834                                                                   
06835      GO TO 8200-SEND-DATAONLY.                                    
06836                                                                   
06837      EJECT                                                        
06838                                                                   
06839 ***************************************************************   
06840 *     FORWARD BROWSING ROUTINE   (BY CSR)                     *   
06841 *                                                             *   
06842 *      1. START BROWSE WITH                                   *   
06843 *         A. FIRST RECORD SPECIFIED FROM FIRST SCREEN         *   
06844 *         B. LAST RECORD DISPLAYED FROM PREVIOUS SCREEN       *   
06845 *         C. RECORD REQUESTED VIA A SHOW MAINT REQUEST.       *   
06846 *                                                             *   
06847 *      2. READ RECORDS AND DETERMINE IF THEY ARE TO BE        *   
06848 *         DISPLAYED BY USING THE FOLLOWING CHECKS.            *   
06849 *         A. IS IT WITHIN THE SAME KEY GROUP?                 *   
06850 *         B. IF IT IS A BILLED REC AND IS IT TO BE DISPLAYED. *   
06851 *         C. IF IT HAS BEEN PROCESSED THRU THE BATCH SYSTEM   *   
06852 *               AND IS IT TO BE DISPLAYED.                    *   
06853 *         D. IF IT IS AN ISSUE AND IF ERRORS ARE TO BE        *   
06854 *               DISPLAYED.                                    *   
06855 *         E. IF IT IS AN CANCEL AND IF ERRORS ARE TO BE       *   
06856 *               DISPLAYED.                                    *   
06857 *                                                             *   
06858 *      3. IF THE RECORD MEETS ALL REQUIREMENTS, SAVE THE      *   
06859 *               RECORD KEY AND GO TO THE DISPLAY ROUTINE.     *   
06860 ***************************************************************   
06861                                                                   
06862  4200-BROWSE-CSR-FORWARD.                                         
06863                                                                   
06864      IF FIRST-TIME                                                
06865         MOVE PI-ERPNDB-CSR-KEY      TO ERPNDB-CSR-KEY             
06866      ELSE                                                         
06867         MOVE PI-PREV-CSR-KEY        TO ERPNDB-CSR-KEY.            
06868                                                                   
06869      EXEC CICS HANDLE CONDITION                                   
06870           NOTFND   (4250-END-RECORDS)                             
06871           ENDFILE  (4250-END-RECORDS)                             
06872      END-EXEC.                                                    
06873                                                                   
06874      EXEC CICS STARTBR                                            
06875           DATASET   (FILE-ID-ERPNDB4)                             
06876           RIDFLD    (ERPNDB-CSR-KEY)                              
06877      END-EXEC.                                                    
06878                                                                   
06879      MOVE 'Y'                    TO WS-BROWSE-STARTED-SW.         
06880                                                                   
06881      IF FIRST-TIME                                                
06882         MOVE SPACE               TO WS-FIRST-TIME-SW              
06883         GO TO 4205-READ-NEXT-RECORD.                              
06884                                                                   
06885      EXEC CICS READNEXT                                           
06886           DATASET    (FILE-ID-ERPNDB4)                            
06887           SET        (ADDRESS OF PENDING-BUSINESS)                
06888           RIDFLD     (ERPNDB-CSR-KEY)                             
06889      END-EXEC.                                                    
06890                                                                   
06891  4205-READ-NEXT-RECORD.                                           
06892                                                                   
06893      EXEC CICS READNEXT                                           
06894           DATASET    (FILE-ID-ERPNDB4)                            
06895           SET        (ADDRESS OF PENDING-BUSINESS)                
06896           RIDFLD     (ERPNDB-CSR-KEY)                             
06897      END-EXEC.                                                    
06898                                                                   
06899      MOVE PB-ENTRY-BATCH         TO WS-BATCH-NO.                  
06900                                                                   
06901      IF WS-BATCH-PREFIX = '#CL'                                   
06902         GO TO 4205-READ-NEXT-RECORD.                              
06903                                                                   
06904      IF PB-CSR-COMPANY-CD   NOT = PI-COMPANY-CD OR                
06905         PB-CSR-ID           NOT = PI-PB-CSR-ID                    
06906         GO TO 4250-END-RECORDS.                                   
06907                                                                   
06908      IF PB-BATCH-TRAILER                                          
06909         GO TO 4205-READ-NEXT-RECORD.                              
06910                                                                   
06911      IF PI-PB-CERT-NO NOT = LOW-VALUES                            
06912         IF PI-PB-CERT-NO NOT = PB-CERT-NO                         
06913            GO TO 4205-READ-NEXT-RECORD                            
06914         ELSE                                                      
06915            IF PI-PB-CERT-EFF-DT = LOW-VALUES                      
06916               GO TO 4210-CHECK-REC-TYPE                           
06917            ELSE                                                   
06918               IF PI-PB-CERT-EFF-DT = PB-CERT-EFF-DT               
06919                  GO TO 4210-CHECK-REC-TYPE                        
06920               ELSE                                                
06921                  GO TO 4205-READ-NEXT-RECORD.                     
06922                                                                   
06923      IF PI-PB-CERT-EFF-DT NOT = LOW-VALUES                        
06924         IF PI-PB-CERT-EFF-DT NOT = PB-CERT-EFF-DT                 
06925            GO TO 4205-READ-NEXT-RECORD.                           
06926                                                                   
06927  4210-CHECK-REC-TYPE.                                             
06928      IF  PI-CARRIER-SECURITY GREATER THAN SPACES                  
06929          IF  PB-CARRIER = PI-CARRIER-SECURITY                     
06930              NEXT SENTENCE                                        
06931          ELSE                                                     
06932              GO TO 4205-READ-NEXT-RECORD.                         
06933                                                                   
06934      IF  PI-ACCOUNT-SECURITY GREATER THAN SPACES                  
06935          IF  PB-ACCOUNT = PI-ACCOUNT-SECURITY                     
06936              NEXT SENTENCE                                        
06937         ELSE                                                      
06938              GO TO 4205-READ-NEXT-RECORD.                         
06939                                                                   
06940      IF NOT DISPLAY-CHANGE-RECORDS AND                            
06941             PB-BATCH-CHG-SEQ-NO NOT = ZEROS                       
06942         GO TO 4205-READ-NEXT-RECORD.                              
06943                                                                   
06944      IF DISPLAY-HOLD-RECORDS AND                                  
06945         NOT PB-RECORD-ON-HOLD                                     
06946           GO TO 4205-READ-NEXT-RECORD.                            
06947                                                                   
06948      IF PB-ISSUE                                                  
06949         IF DISPLAY-CHK-REQ-RECORDS                                
06950             GO TO 4205-READ-NEXT-RECORD.                          
06951                                                                   
06952      IF PB-ISSUE                                                  
06953         IF ISSUE-WITH-WARNING                                     
06954            IF PB-WARNING-ERRORS                                   
06955               IF (PB-UNFORCED-ERRORS OR PB-FATAL-ERRORS)          
06956                   GO TO 4205-READ-NEXT-RECORD                     
06957               ELSE                                                
06958                  MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY   
06959                  MOVE PB-CONTROL-BY-CSR      TO PI-PREV-CSR-KEY   
06960                  MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY       
06961                  MOVE PB-SV-CARRIER          TO PI-SV-CARRIER     
06962                  MOVE PB-SV-GROUPING         TO PI-SV-GROUPING    
06963                  MOVE PB-SV-STATE            TO PI-SV-STATE       
06964                  MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW               
06965                  GO TO 7000-DISPLAY-ISSUES                        
06966            ELSE                                                   
06967               GO TO 4205-READ-NEXT-RECORD.                        
06968                                                                   
06969      IF PB-ISSUE                                                  
06970         IF (ISSUES-IN-ERROR AND                                   
06971             (PB-UNFORCED-ERRORS OR PB-FATAL-ERRORS)) OR           
06972             ALL-ISSUES                                            
06973             MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY        
06974             MOVE PB-CONTROL-BY-CSR      TO PI-PREV-CSR-KEY        
06975             MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY            
06976             MOVE PB-SV-CARRIER          TO PI-SV-CARRIER          
06977             MOVE PB-SV-GROUPING         TO PI-SV-GROUPING         
06978             MOVE PB-SV-STATE            TO PI-SV-STATE            
06979             MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW                    
06980             GO TO 7000-DISPLAY-ISSUES                             
06981         ELSE                                                      
06982             GO TO 4205-READ-NEXT-RECORD.                          
06983                                                                   
06984      IF PB-CANCELLATION                                           
06985         IF DISPLAY-CHK-REQ-RECORDS                                
06986            IF PB-C-REFUND-REQUESTED                               
06987               MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY      
06988               MOVE PB-CONTROL-BY-CSR      TO PI-PREV-CSR-KEY      
06989               MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY          
06990               MOVE PB-SV-CARRIER          TO PI-SV-CARRIER        
06991               MOVE PB-SV-GROUPING         TO PI-SV-GROUPING       
06992               MOVE PB-SV-STATE            TO PI-SV-STATE          
06993               MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW                  
06994               GO TO 7100-DISPLAY-CANCELS                          
06995            ELSE                                                   
06996                GO TO 4205-READ-NEXT-RECORD.                       
06997                                                                   
06998      IF PB-CANCELLATION                                           
06999         IF CANCEL-WITH-WARNING                                    
07000            IF PB-WARNING-ERRORS                                   
07001               IF (PB-UNFORCED-ERRORS OR PB-FATAL-ERRORS)          
07002                   GO TO 4205-READ-NEXT-RECORD                     
07003               ELSE                                                
07004                  MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY   
07005                  MOVE PB-CONTROL-BY-CSR      TO PI-PREV-CSR-KEY   
07006                  MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY       
07007                  MOVE PB-SV-CARRIER          TO PI-SV-CARRIER     
07008                  MOVE PB-SV-GROUPING         TO PI-SV-GROUPING    
07009                  MOVE PB-SV-STATE            TO PI-SV-STATE       
07010                  MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW               
07011                  GO TO 7100-DISPLAY-CANCELS                       
07012            ELSE                                                   
07013               GO TO 4205-READ-NEXT-RECORD.                        
07014                                                                   
07015      IF PB-CANCELLATION                                           
07016         IF (CANCEL-IN-ERROR  AND                                  
07017             (PB-UNFORCED-ERRORS OR PB-FATAL-ERRORS)) OR           
07018             ALL-CANCELS                                           
07019             MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY        
07020             MOVE PB-CONTROL-BY-CSR      TO PI-PREV-CSR-KEY        
07021             MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY            
07022             MOVE PB-SV-CARRIER          TO PI-SV-CARRIER          
07023             MOVE PB-SV-GROUPING         TO PI-SV-GROUPING         
07024             MOVE PB-SV-STATE            TO PI-SV-STATE            
07025             MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW                    
07026             GO TO 7100-DISPLAY-CANCELS.                           
07027                                                                   
07028      GO TO 4205-READ-NEXT-RECORD.                                 
07029                                                                   
07030  4250-END-RECORDS.                                                
07031 ******************************************************************
07032 *                                                                *
07033 *         IF THERE ARE NO PENDING BUS. RECORDS FOUND DURING      *
07034 *         THE INITIAL ENTRY OF VP6311, RETURN TO THE CALLING      
07035 *         PROGRAM.                                               *
07036 *                                                                *
07037 ******************************************************************
07038                                                                   
07039      IF BROWSE-STARTED                                            
07040         MOVE SPACE               TO WS-BROWSE-STARTED-SW          
07041         EXEC CICS ENDBR                                           
07042              DATASET  (FILE-ID-ERPNDB4)                           
07043         END-EXEC.                                                 
07044                                                                   
07045      IF  EIBTRNID NOT = TRANS-EXB1                                
07046          IF PENDING-BUS-RECS-FOUND                                
07047              NEXT SENTENCE                                        
07048            ELSE                                                   
07049              MOVE '9'            TO PI-BROWSE-SW                  
07050              GO TO 9400-CLEAR.                                    
07051                                                                   
07052      IF SHOW-RECORD                                               
07053         MOVE ER-2253            TO EMI-ERROR                      
07054        ELSE                                                       
07055         MOVE ER-2254            TO EMI-ERROR.                     
07056                                                                   
07057      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
07058                                                                   
07059      IF PI-MAP-NAME = SPACES                                      
07060         MOVE -1                  TO DPFENTRL                      
07061         MOVE VP631D               TO PI-MAP-NAME                  
07062         GO TO 8100-SEND-INITIAL-MAP.                              
07063                                                                   
07064      IF PI-MAP-NAME = VP631B                                      
07065         MOVE -1                  TO BMAINTL                       
07066      ELSE                                                         
07067         IF PI-MAP-NAME = VP631C                                   
07068            MOVE -1               TO CMAINTL                       
07069         ELSE                                                      
07070            MOVE -1               TO DPFENTRL.                     
07071                                                                   
07072      GO TO 8200-SEND-DATAONLY.                                    
07073                                                                   
07074      EJECT                                                        
07075                                                                   
07076 ***************************************************************   
07077 *                                                             *   
07078 *     FORWARD BROWSING ROUTINE   (BY FILE)                    *   
07079 *                                                             *   
07080 *       1. READ RECORDS WITHIN THE FILE BY COMPANY            *   
07081 *          A. FIRST TIME INTO PROGRAM FROM FIRST SCREEN       *   
07082 *          B. LAST RECORD DISPLAYED FROM PREVIOUS SCREEN      *   
07083 *                                                             *   
07084 *       2. READ RECORDS AND DETERMINE IF THEY ARE TO BE       *   
07085 *          DISPLAYED BY USING THE FOLLOWING CHECKS.           *   
07086 *          A. IS IT WITHIN THE SAME COMPANY                   *   
07087 *          B. IF IT IS A BILLED REC AND IS IT TO BE DISPLAYED.*   
07088 *          C. IF IT HAS BEEN PROCESSED THRU THE BATCH SYSTEM  *   
07089 *                AND IS IT TO BE DISPLAYED.                   *   
07090 *          D. IF IT IS AN ISSUE AND IF ERRORS ARE TO BE       *   
07091 *                DISPLAYED.                                   *   
07092 *          E. IF IT IS AN CANCEL AND IF ERRORS ARE TO BE      *   
07093 *                DISPLAYED.                                   *   
07094 *                                                             *   
07095 *       3. IF THE RECORD MEETS ALL REQUIREMENTS, SAVE THE     *   
07096 *                RECORD KEY AND GO TO THE DISPLAY ROUTINE.    *   
07097 *                                                             *   
07098 *       4. WHEN READING ADDITION RECORDS ADJUSTMENTS MUST BE  *   
07099 *                MADE TO BYPASS THE CORRECTION RECORDS IF     *   
07100 *                THEY ARE NOT TO BE DISPLAYED.                *   
07101 *                                                             *   
07102 ***************************************************************   
07103                                                                   
07104  4300-BROWSE-FILE-FORWARD.                                        
07105      IF FIRST-TIME OR SHOW-RECORD                                 
07106         MOVE PI-ERPNDB-KEY       TO ERPNDB-KEY                    
07107         MOVE +1                  TO ERPNDB-BATCH-SEQ-NO           
07108         MOVE ZEROS               TO ERPNDB-BATCH-CHG-SEQ-NO       
07109      ELSE                                                         
07110         MOVE PI-PREV-KEY         TO ERPNDB-KEY                    
07111         IF ERPNDB-BATCH-SEQ-NO = +9999                            
07112            MOVE HIGH-VALUES      TO ERPNDB-BATCH-SEQ-ALPHA        
07113         ELSE                                                      
07114            IF DISPLAY-CHANGE-RECORDS                              
07115               ADD +1             TO ERPNDB-BATCH-CHG-SEQ-NO       
07116            ELSE                                                   
07117               MOVE ZEROS         TO ERPNDB-BATCH-CHG-SEQ-NO       
07118               ADD +1             TO ERPNDB-BATCH-SEQ-NO.          
07119                                                                   
07120      EXEC CICS HANDLE CONDITION                                   
07121           ENDFILE  (4350-END-BATCH)                               
07122           NOTFND   (4350-END-BATCH)                               
07123      END-EXEC.                                                    
07124                                                                   
07125  4305-READ-NEXT.                                                  
07126      IF PI-DISPLAY-ORIGINAL-BATCH                                 
07127         MOVE FILE-ID-ERPNDB3     TO FILE-ID                       
07128      ELSE                                                         
07129         MOVE FILE-ID-ERPNDB      TO FILE-ID.                      
07130                                                                   
07131      EXEC CICS READ                                               
07132           DATASET   (FILE-ID)                                     
07133           SET       (ADDRESS OF PENDING-BUSINESS)                 
07134           RIDFLD    (ERPNDB-KEY)                                  
07135           GTEQ                                                    
07136      END-EXEC.                                                    
07137                                                                   
07138      IF PB-COMPANY-CD NOT = PI-COMPANY-CD                         
07139         GO TO 4350-END-BATCH.                                     
07140                                                                   
07141      MOVE PB-ENTRY-BATCH         TO ERPNDB-ENTRY-BATCH            
07142                                     WS-BATCH-NO.                  
07143                                                                   
07144      IF  WS-BATCH-PREFIX = '#CL'                                  
07145          MOVE HIGH-VALUES        TO ERPNDB-BATCH-SEQ-ALPHA        
07146          GO TO 4305-READ-NEXT.                                    
07147                                                                   
07148      MOVE PB-CONTROL-PRIMARY     TO PI-ERPNDB-KEY.                
07149      MOVE PB-CARRIER             TO PI-PB-CARRIER.                
07150      MOVE PB-GROUPING            TO PI-PB-GROUPING.               
07151      MOVE PB-STATE               TO PI-PB-STATE.                  
07152      MOVE PB-ACCOUNT             TO PI-PB-ACCOUNT.                
07153                                                                   
07154      IF PB-BATCH-TRAILER                                          
07155         MOVE HIGH-VALUES         TO ERPNDB-BATCH-SEQ-ALPHA        
07156         GO TO 4305-READ-NEXT.                                     
07157                                                                   
07158      IF SHOW-RECORD                                               
07159            IF WS-NEW-CERT-NO NOT = PB-CERT-NO                     
07160               GO TO 4310-BUMP-SEQ                                 
07161            ELSE                                                   
07162               IF WS-NEW-CERT-EFF-DT NOT = PB-CERT-EFF-DT          
07163                  GO TO 4310-BUMP-SEQ                              
07164               ELSE                                                
07165                  GO TO 4307-CHECK-REC-TYPE.                       
07166                                                                   
07167      IF PI-PB-CERT-NO NOT = LOW-VALUES                            
07168         IF PI-PB-CERT-NO NOT = PB-CERT-NO                         
07169            GO TO 4310-BUMP-SEQ                                    
07170         ELSE                                                      
07171            IF PI-PB-CERT-EFF-DT = LOW-VALUES                      
07172               GO TO 4307-CHECK-REC-TYPE                           
07173            ELSE                                                   
07174               IF PI-PB-CERT-EFF-DT = PB-CERT-EFF-DT               
07175                  GO TO 4307-CHECK-REC-TYPE                        
07176               ELSE                                                
07177                  GO TO 4310-BUMP-SEQ.                             
07178                                                                   
07179      IF PI-PB-CERT-EFF-DT NOT = LOW-VALUES                        
07180         IF PI-PB-CERT-EFF-DT NOT = PB-CERT-EFF-DT                 
07181            GO TO 4310-BUMP-SEQ.                                   
07182                                                                   
07183  4307-CHECK-REC-TYPE.                                             
07184      IF  PI-CARRIER-SECURITY GREATER THAN SPACES                  
07185          IF  PB-CARRIER = PI-CARRIER-SECURITY                     
07186              NEXT SENTENCE                                        
07187          ELSE                                                     
07188              GO TO 4310-BUMP-SEQ.                                 
07189                                                                   
07190      IF  PI-ACCOUNT-SECURITY GREATER THAN SPACES                  
07191          IF  PB-ACCOUNT = PI-ACCOUNT-SECURITY                     
07192              NEXT SENTENCE                                        
07193         ELSE                                                      
07194              GO TO 4310-BUMP-SEQ.                                 
07195                                                                   
07196      IF NOT DISPLAY-CHANGE-RECORDS AND                            
07197             PB-BATCH-CHG-SEQ-NO NOT = ZEROS                       
07198         GO TO 4310-BUMP-SEQ.                                      
07199                                                                   
07200      IF DISPLAY-HOLD-RECORDS AND                                  
07201         NOT PB-RECORD-ON-HOLD                                     
07202           GO TO 4310-BUMP-SEQ.                                    
07203                                                                   
07204      IF PB-ISSUE                                                  
07205         IF DISPLAY-CHK-REQ-RECORDS                                
07206            GO TO 4310-BUMP-SEQ.                                   
07207                                                                   
07208      IF PB-ISSUE                                                  
07209         IF ISSUE-WITH-WARNING                                     
07210            IF PB-WARNING-ERRORS                                   
07211               IF (PB-UNFORCED-ERRORS OR PB-FATAL-ERRORS)          
07212                   GO TO 4310-BUMP-SEQ                             
07213               ELSE                                                
07214                  MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY   
07215                  MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY       
07216                  MOVE PB-SV-CARRIER          TO PI-SV-CARRIER     
07217                  MOVE PB-SV-GROUPING         TO PI-SV-GROUPING    
07218                  MOVE PB-SV-STATE            TO PI-SV-STATE       
07219                  MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW               
07220                  GO TO 7000-DISPLAY-ISSUES                        
07221            ELSE                                                   
07222               GO TO 4310-BUMP-SEQ.                                
07223                                                                   
07224      IF PB-ISSUE                                                  
07225         IF (ISSUES-IN-ERROR AND                                   
07226             (PB-UNFORCED-ERRORS OR PB-FATAL-ERRORS)) OR           
07227             ALL-ISSUES                                            
07228             MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY        
07229             MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY            
07230             MOVE PB-SV-CARRIER          TO PI-SV-CARRIER          
07231             MOVE PB-SV-GROUPING         TO PI-SV-GROUPING         
07232             MOVE PB-SV-STATE            TO PI-SV-STATE            
07233             MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW                    
07234             GO TO 7000-DISPLAY-ISSUES                             
07235         ELSE                                                      
07236             GO TO 4310-BUMP-SEQ.                                  
07237                                                                   
07238      IF PB-CANCELLATION                                           
07239         IF DISPLAY-CHK-REQ-RECORDS                                
07240            IF PB-C-REFUND-REQUESTED                               
07241               MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY      
07242               MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY          
07243               MOVE PB-SV-CARRIER          TO PI-SV-CARRIER        
07244               MOVE PB-SV-GROUPING         TO PI-SV-GROUPING       
07245               MOVE PB-SV-STATE            TO PI-SV-STATE          
07246               MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW                  
07247               GO TO 7100-DISPLAY-CANCELS                          
07248            ELSE                                                   
07249               GO TO 4310-BUMP-SEQ.                                
07250                                                                   
07251      IF PB-CANCELLATION                                           
07252         IF CANCEL-WITH-WARNING                                    
07253            IF PB-WARNING-ERRORS                                   
07254               IF (PB-UNFORCED-ERRORS OR PB-FATAL-ERRORS)          
07255                   GO TO 4310-BUMP-SEQ                             
07256               ELSE                                                
07257                  MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY   
07258                  MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY       
07259                  MOVE PB-SV-CARRIER          TO PI-SV-CARRIER     
07260                  MOVE PB-SV-GROUPING         TO PI-SV-GROUPING    
07261                  MOVE PB-SV-STATE            TO PI-SV-STATE       
07262                  MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW               
07263                  GO TO 7100-DISPLAY-CANCELS                       
07264            ELSE                                                   
07265               GO TO 4310-BUMP-SEQ.                                
07266                                                                   
07267      IF PB-CANCELLATION                                           
07268         IF (CANCEL-IN-ERROR  AND                                  
07269             (PB-UNFORCED-ERRORS OR PB-FATAL-ERRORS)) OR           
07270             ALL-CANCELS                                           
07271             MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY        
07272             MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY            
07273             MOVE PB-SV-CARRIER          TO PI-SV-CARRIER          
07274             MOVE PB-SV-GROUPING         TO PI-SV-GROUPING         
07275             MOVE PB-SV-STATE            TO PI-SV-STATE            
07276             MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW                    
07277             GO TO 7100-DISPLAY-CANCELS.                           
07278                                                                   
07279  4310-BUMP-SEQ.                                                   
07280      IF NOT DISPLAY-CHANGE-RECORDS                                
07281         MOVE ZEROS               TO ERPNDB-BATCH-CHG-SEQ-NO       
07282         ADD +1                   TO ERPNDB-BATCH-SEQ-NO           
07283      ELSE                                                         
07284         IF PB-BATCH-SEQ-NO NOT = ERPNDB-BATCH-SEQ-NO              
07285            MOVE PB-BATCH-SEQ-NO  TO ERPNDB-BATCH-SEQ-NO           
07286            MOVE +999             TO ERPNDB-BATCH-CHG-SEQ-NO       
07287         ELSE                                                      
07288            MOVE PB-BATCH-CHG-SEQ-NO  TO ERPNDB-BATCH-CHG-SEQ-NO   
07289            ADD +1                TO ERPNDB-BATCH-CHG-SEQ-NO.      
07290                                                                   
07291      GO TO 4305-READ-NEXT.                                        
07292                                                                   
07293  4350-END-BATCH.                                                  
07294                                                                   
07295 ******************************************************************
07296 *                                                                *
07297 *         IF THERE ARE NO PENDING BUS. RECORDS FOUND DURING      *
07298 *         THE INITIAL ENTRY OF VP6311, RETURN TO THE CALLING    * 
07299 *         PROGRAM.                                               *
07300 *                                                                *
07301 ******************************************************************
07302                                                                   
07303      IF  EIBTRNID NOT = TRANS-EXB1                                
07304          IF PENDING-BUS-RECS-FOUND                                
07305              NEXT SENTENCE                                        
07306            ELSE                                                   
07307              MOVE '9'            TO PI-BROWSE-SW                  
07308              GO TO 9400-CLEAR.                                    
07309                                                                   
07310      IF SHOW-RECORD                                               
07311         MOVE ER-2253            TO EMI-ERROR                      
07312      ELSE                                                         
07313         MOVE ER-2251            TO EMI-ERROR.                     
07314                                                                   
07315      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
07316                                                                   
07317      IF PI-MAP-NAME = SPACES                                      
07318         MOVE -1                  TO BMAINTL                       
07319         MOVE VP631B               TO PI-MAP-NAME                  
07320         GO TO 8100-SEND-INITIAL-MAP.                              
07321                                                                   
07322      IF PI-MAP-NAME = VP631B                                      
07323         MOVE -1                  TO BMAINTL                       
07324      ELSE                                                         
07325         IF PI-MAP-NAME = VP631C                                   
07326            MOVE -1               TO CMAINTL                       
07327         ELSE                                                      
07328            MOVE -1               TO DPFENTRL.                     
07329                                                                   
07330      GO TO 8200-SEND-DATAONLY.                                    
07331                                                                   
07332      EJECT                                                        
07333                                                                   
07334 ******************************************************************
07335 *                                                                *
07336 *        B R O W S E   D E T A I L   B A C K W A R D             *
07337 *                                                                *
07338 ******************************************************************
07339                                                                   
07340  4500-BROWSE-DETAIL-BACKWARD.                                     
07341                                                                   
07342      MOVE PI-PREV-ALT-KEY        TO ERPNDB-ALT-KEY.               
07343                                                                   
07344      EXEC CICS HANDLE CONDITION                                   
07345           NOTFND   (4570-NOT-FOUND)                               
07346           ENDFILE  (4550-END-RECORDS)                             
07347      END-EXEC                                                     
07348                                                                   
07349      EXEC CICS STARTBR                                            
07350           DATASET   (FILE-ID-ERPNDB2)                             
07351           RIDFLD    (ERPNDB-ALT-KEY)                              
07352      END-EXEC.                                                    
07353                                                                   
07354      MOVE 'Y'                    TO WS-BROWSE-STARTED-SW          
07355                                     WS-FIRST-TIME-SW.             
07356                                                                   
07357  4505-READ-PREV-RECORD.                                           
07358      EXEC CICS READPREV                                           
07359           DATASET    (FILE-ID-ERPNDB2)                            
07360           SET        (ADDRESS OF PENDING-BUSINESS)                
07361           RIDFLD     (ERPNDB-ALT-KEY)                             
07362      END-EXEC.                                                    
07363                                                                   
07364      MOVE PB-ENTRY-BATCH         TO WS-BATCH-NO.                  
07365                                                                   
07366      IF WS-BATCH-PREFIX = '#CL'                                   
07367          GO TO 4505-READ-PREV-RECORD.                             
07368                                                                   
07369      IF FIRST-TIME                                                
07370          MOVE SPACE               TO WS-FIRST-TIME-SW             
07371          GO TO 4505-READ-PREV-RECORD.                             
07372                                                                   
07373      IF PB-COMPANY-CD-A1 NOT = PI-COMPANY-CD  OR                  
07374         PB-CARRIER       NOT = PI-PB-CARRIER  OR                  
07375         PB-GROUPING      NOT = PI-PB-GROUPING OR                  
07376         PB-STATE         NOT = PI-PB-STATE    OR                  
07377         PB-ACCOUNT       NOT = PI-PB-ACCOUNT                      
07378           GO TO 4550-END-RECORDS.                                 
07379                                                                   
CIDMOD                                                                  
CIDMOD     MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY.                  
07380                                                                   
07381      IF PB-BATCH-TRAILER                                          
07382         GO TO 4505-READ-PREV-RECORD.                              
07383                                                                   
07384      MOVE PB-ENTRY-BATCH         TO WS-BATCH-NO.                  
07385                                                                   
07386      IF  WS-BATCH-PREFIX = '#CL'                                  
07387          GO TO 4505-READ-PREV-RECORD.                             
07388                                                                   
07389      IF PI-PB-CERT-NO NOT = LOW-VALUES                            
07390         IF PI-PB-CERT-NO NOT = PB-CERT-NO                         
07391            GO TO 4505-READ-PREV-RECORD                            
07392         ELSE                                                      
07393            IF PI-PB-CERT-EFF-DT = LOW-VALUES                      
07394               GO TO 4510-CHECK-REC-TYPE                           
07395            ELSE                                                   
07396               IF PI-PB-CERT-EFF-DT = PB-CERT-EFF-DT               
07397                  GO TO 4510-CHECK-REC-TYPE                        
07398               ELSE                                                
07399                  GO TO 4505-READ-PREV-RECORD.                     
07400                                                                   
07401      IF PI-PB-CERT-EFF-DT NOT = LOW-VALUES                        
07402         IF PI-PB-CERT-EFF-DT NOT = PB-CERT-EFF-DT                 
07403            GO TO 4505-READ-PREV-RECORD.                           
07404                                                                   
07405  4510-CHECK-REC-TYPE.                                             
07406      IF  PI-CARRIER-SECURITY GREATER THAN SPACES                  
07407          IF  PB-CARRIER = PI-CARRIER-SECURITY                     
07408              NEXT SENTENCE                                        
07409          ELSE                                                     
07410              GO TO 4505-READ-PREV-RECORD.                         
07411                                                                   
07412      IF  PI-ACCOUNT-SECURITY GREATER THAN SPACES                  
07413          IF  PB-ACCOUNT = PI-ACCOUNT-SECURITY                     
07414              NEXT SENTENCE                                        
07415         ELSE                                                      
07416              GO TO 4505-READ-PREV-RECORD.                         
07417                                                                   
07418      IF NOT DISPLAY-CHANGE-RECORDS AND                            
07419             PB-BATCH-CHG-SEQ-NO NOT = ZEROS                       
07420         GO TO 4505-READ-PREV-RECORD.                              
07421                                                                   
07422      IF DISPLAY-HOLD-RECORDS AND                                  
07423         NOT PB-RECORD-ON-HOLD                                     
07424           GO TO 4505-READ-PREV-RECORD.                            
07425                                                                   
07426      IF PB-ISSUE                                                  
07427         IF DISPLAY-CHK-REQ-RECORDS                                
07428            GO TO 4505-READ-PREV-RECORD.                           
07429                                                                   
07430      IF PB-ISSUE                                                  
07431         IF ISSUE-WITH-WARNING                                     
07432            IF PB-WARNING-ERRORS                                   
07433               IF (PB-UNFORCED-ERRORS OR PB-FATAL-ERRORS)          
07434                   GO TO 4505-READ-PREV-RECORD                     
07435               ELSE                                                
07436                  MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY   
07437                  MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY       
07438                  MOVE PB-SV-CARRIER          TO PI-SV-CARRIER     
07439                  MOVE PB-SV-GROUPING         TO PI-SV-GROUPING    
07440                  MOVE PB-SV-STATE            TO PI-SV-STATE       
07441                  MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW               
07442                  GO TO 7000-DISPLAY-ISSUES                        
07443            ELSE                                                   
07444               GO TO 4505-READ-PREV-RECORD.                        
07445                                                                   
07446      IF PB-ISSUE                                                  
07447         IF (ISSUES-IN-ERROR AND                                   
07448             (PB-UNFORCED-ERRORS OR PB-FATAL-ERRORS)) OR           
07449             ALL-ISSUES                                            
07450             MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY        
07451             MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY            
07452             MOVE PB-SV-CARRIER          TO PI-SV-CARRIER          
07453             MOVE PB-SV-GROUPING         TO PI-SV-GROUPING         
07454             MOVE PB-SV-STATE            TO PI-SV-STATE            
07455             MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW                    
07456             GO TO 7000-DISPLAY-ISSUES                             
07457         ELSE                                                      
07458             GO TO 4505-READ-PREV-RECORD.                          
07459                                                                   
07460      IF PB-CANCELLATION                                           
07461         IF DISPLAY-CHK-REQ-RECORDS                                
07462            IF PB-C-REFUND-REQUESTED                               
07463               MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY      
07464               MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY          
07465               MOVE PB-SV-CARRIER          TO PI-SV-CARRIER        
07466               MOVE PB-SV-GROUPING         TO PI-SV-GROUPING       
07467               MOVE PB-SV-STATE            TO PI-SV-STATE          
07468               MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW                  
07469               GO TO 7100-DISPLAY-CANCELS                          
07470            ELSE                                                   
07471               GO TO 4505-READ-PREV-RECORD.                        
07472                                                                   
07473      IF PB-CANCELLATION                                           
07474         IF CANCEL-WITH-WARNING                                    
07475            IF PB-WARNING-ERRORS                                   
07476               IF (PB-UNFORCED-ERRORS OR PB-FATAL-ERRORS)          
07477                   GO TO 4505-READ-PREV-RECORD                     
07478               ELSE                                                
07479                  MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY   
07480                  MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY       
07481                  MOVE PB-SV-CARRIER          TO PI-SV-CARRIER     
07482                  MOVE PB-SV-GROUPING         TO PI-SV-GROUPING    
07483                  MOVE PB-SV-STATE            TO PI-SV-STATE       
07484                  MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW               
07485                  GO TO 7100-DISPLAY-CANCELS                       
07486            ELSE                                                   
07487               GO TO 4505-READ-PREV-RECORD.                        
07488                                                                   
07489      IF PB-CANCELLATION                                           
07490         IF (CANCEL-IN-ERROR  AND                                  
07491             (PB-UNFORCED-ERRORS OR PB-FATAL-ERRORS)) OR           
07492             ALL-CANCELS                                           
07493             MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY        
07494             MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY            
07495             MOVE PB-SV-CARRIER          TO PI-SV-CARRIER          
07496             MOVE PB-SV-GROUPING         TO PI-SV-GROUPING         
07497             MOVE PB-SV-STATE            TO PI-SV-STATE            
07498             MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW                    
07499             GO TO 7100-DISPLAY-CANCELS.                           
07500                                                                   
07501      GO TO 4505-READ-PREV-RECORD.                                 
07502                                                                   
07503  4550-END-RECORDS.                                                
07504      IF PI-MAP-NAME = VP631B                                      
07505         MOVE -1                  TO BMAINTL                       
07506      ELSE                                                         
07507         IF PI-MAP-NAME = VP631C                                   
07508            MOVE -1               TO CMAINTL                       
07509         ELSE                                                      
07510            MOVE -1               TO DPFENTRL.                     
07511                                                                   
07512      MOVE ER-2255               TO EMI-ERROR.                     
07513      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
07514      GO TO 8200-SEND-DATAONLY.                                    
07515                                                                   
07516  4570-NOT-FOUND.                                                  
07517                                                                   
07518 ******************************************************************
07519 *      CONDITION MAY OCCUR IF PREV. RECORD HAS BEEN DELETED.     *
07520 ******************************************************************
07521                                                                   
07522      EXEC CICS RESETBR                                            
07523           DATASET    (FILE-ID-ERPNDB2)                            
07524           RIDFLD     (ERPNDB-ALT-KEY)                             
07525      END-EXEC.                                                    
07526                                                                   
07527      EXEC CICS READNEXT                                           
07528           DATASET    (FILE-ID-ERPNDB2)                            
07529           SET        (ADDRESS OF PENDING-BUSINESS)                
07530           RIDFLD     (ERPNDB-ALT-KEY)                             
07531      END-EXEC.                                                    
07532                                                                   
07533      EXEC CICS READPREV                                           
07534           DATASET    (FILE-ID-ERPNDB2)                            
07535           SET        (ADDRESS OF PENDING-BUSINESS)                
07536           RIDFLD     (ERPNDB-ALT-KEY)                             
07537      END-EXEC.                                                    
07538                                                                   
07539  4580-READ-PREV-RECORD.                                           
07540      EXEC CICS READPREV                                           
07541           DATASET    (FILE-ID-ERPNDB2)                            
07542           SET        (ADDRESS OF PENDING-BUSINESS)                
07543           RIDFLD     (ERPNDB-ALT-KEY)                             
07544      END-EXEC.                                                    
07545                                                                   
07546      MOVE PB-ENTRY-BATCH         TO WS-BATCH-NO.                  
07547                                                                   
07548      IF WS-BATCH-PREFIX = '#CL'                                   
07549         GO TO 4580-READ-PREV-RECORD.                              
07550                                                                   
07551      MOVE SPACE                  TO WS-FIRST-TIME-SW.             
07552      GO TO 4505-READ-PREV-RECORD.                                 
07553                                                                   
07554      EJECT                                                        
07555                                                                   
07556 ******************************************************************
07557 *                                                                *
07558 *        B R O W S E   P R I M A R Y   B A C K W A R D           *
07559 *                                                                *
07560 ******************************************************************
07561                                                                   
07562  4600-BROWSE-PRIMARY-BACKWARD.                                    
07563         MOVE PI-PREV-KEY         TO ERPNDB-KEY.                   
07564                                                                   
07565      IF ERPNDB-BATCH-SEQ-NO = +9999                               
07566         MOVE PI-HIGH-SEQ-NO      TO ERPNDB-BATCH-SEQ-NO.          
07567                                                                   
07568         IF ERPNDB-BATCH-SEQ-NO = +0000                            
07569            GO TO 4650-END-BATCH                                   
07570         ELSE                                                      
07571            IF PI-MAP-NAME NOT = VP631D                            
07572               IF ERPNDB-BATCH-CHG-SEQ-NO NOT = ZEROS              
07573                  SUBTRACT +1      FROM ERPNDB-BATCH-CHG-SEQ-NO    
07574               ELSE                                                
07575                  SUBTRACT +1      FROM ERPNDB-BATCH-SEQ-NO        
07576                  IF DISPLAY-CHANGE-RECORDS                        
07577                     MOVE +999     TO ERPNDB-BATCH-CHG-SEQ-NO      
07578                  ELSE                                             
07579                     MOVE ZEROS    TO ERPNDB-BATCH-CHG-SEQ-NO.     
07580                                                                   
07581      EXEC CICS HANDLE CONDITION                                   
07582           NOTFND    (4680-NOT-FOUND)                              
07583           ENDFILE   (4680-NOT-FOUND)                              
07584      END-EXEC.                                                    
07585                                                                   
07586  4605-READ-NEXT.                                                  
07587      IF ERPNDB-BATCH-SEQ-NO = +0000                               
07588         GO TO 4650-END-BATCH.                                     
07589                                                                   
07590      IF PI-DISPLAY-ORIGINAL-BATCH                                 
07591         MOVE FILE-ID-ERPNDB3     TO FILE-ID                       
07592      ELSE                                                         
07593         MOVE FILE-ID-ERPNDB      TO FILE-ID.                      
07594                                                                   
07595      EXEC CICS READ                                               
07596           DATASET   (FILE-ID)                                     
07597           SET       (ADDRESS OF PENDING-BUSINESS)                 
07598           RIDFLD    (ERPNDB-KEY)                                  
07599      END-EXEC.                                                    
07600                                                                   
07601      IF NOT  PI-PRIMARY-WITH-SELECT                               
07602         GO TO 4607-CHECK-REC-TYPE.                                
07603                                                                   
07604      MOVE PB-ENTRY-BATCH         TO WS-BATCH-NO.                  
07605                                                                   
07606      IF  WS-BATCH-PREFIX = '#CL'                                  
07607          GO TO 4610-BUMP-SEQ.                                     
07608                                                                   
07609      IF PI-PB-CERT-NO NOT = LOW-VALUES                            
07610         IF PI-PB-CERT-NO NOT = PB-CERT-NO                         
07611            GO TO 4610-BUMP-SEQ                                    
07612         ELSE                                                      
07613            IF PI-PB-CERT-EFF-DT = LOW-VALUES                      
07614               GO TO 4607-CHECK-REC-TYPE                           
07615            ELSE                                                   
07616               IF PI-PB-CERT-EFF-DT = PB-CERT-EFF-DT               
07617                  GO TO 4607-CHECK-REC-TYPE                        
07618               ELSE                                                
07619                  GO TO 4610-BUMP-SEQ.                             
07620                                                                   
07621      IF PI-PB-CERT-EFF-DT NOT = LOW-VALUES                        
07622         IF PI-PB-CERT-EFF-DT NOT = PB-CERT-EFF-DT                 
07623            GO TO 4610-BUMP-SEQ.                                   
07624                                                                   
07625  4607-CHECK-REC-TYPE.                                             
07626      IF  PI-CARRIER-SECURITY GREATER THAN SPACES                  
07627          IF  PB-CARRIER = PI-CARRIER-SECURITY                     
07628              NEXT SENTENCE                                        
07629          ELSE                                                     
07630              GO TO 4610-BUMP-SEQ.                                 
07631                                                                   
07632      IF  PI-ACCOUNT-SECURITY GREATER THAN SPACES                  
07633          IF  PB-ACCOUNT = PI-ACCOUNT-SECURITY                     
07634              NEXT SENTENCE                                        
07635         ELSE                                                      
07636              GO TO 4610-BUMP-SEQ.                                 
07637                                                                   
07638      IF NOT DISPLAY-CHANGE-RECORDS AND                            
07639             PB-BATCH-CHG-SEQ-NO NOT = ZEROS                       
07640         GO TO 4610-BUMP-SEQ.                                      
07641                                                                   
07642      IF DISPLAY-HOLD-RECORDS AND                                  
07643         NOT PB-RECORD-ON-HOLD                                     
07644           GO TO 4610-BUMP-SEQ.                                    
07645                                                                   
07646      IF PB-ISSUE                                                  
07647         IF DISPLAY-CHK-REQ-RECORDS                                
07648            GO TO 4610-BUMP-SEQ.                                   
07649                                                                   
07650      IF PB-ISSUE                                                  
07651         IF ISSUE-WITH-WARNING                                     
07652            IF PB-WARNING-ERRORS                                   
07653               IF (PB-UNFORCED-ERRORS OR PB-FATAL-ERRORS)          
07654                   GO TO 4610-BUMP-SEQ                             
07655               ELSE                                                
07656                  MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY   
07657                  MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY       
07658                  MOVE PB-SV-CARRIER          TO PI-SV-CARRIER     
07659                  MOVE PB-SV-GROUPING         TO PI-SV-GROUPING    
07660                  MOVE PB-SV-STATE            TO PI-SV-STATE       
07661                  MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW               
07662                  GO TO 7000-DISPLAY-ISSUES                        
07663            ELSE                                                   
07664               GO TO 4610-BUMP-SEQ.                                
07665                                                                   
07666      IF PB-ISSUE                                                  
07667         IF (ISSUES-IN-ERROR AND                                   
07668             (PB-UNFORCED-ERRORS OR PB-FATAL-ERRORS)) OR           
07669             ALL-ISSUES                                            
07670             MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY        
07671             MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY            
07672             MOVE PB-SV-CARRIER          TO PI-SV-CARRIER          
07673             MOVE PB-SV-GROUPING         TO PI-SV-GROUPING         
07674             MOVE PB-SV-STATE            TO PI-SV-STATE            
07675             MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW                    
07676             GO TO 7000-DISPLAY-ISSUES                             
07677         ELSE                                                      
07678             GO TO 4610-BUMP-SEQ.                                  
07679                                                                   
07680      IF PB-CANCELLATION                                           
07681         IF DISPLAY-CHK-REQ-RECORDS                                
07682            IF PB-C-REFUND-REQUESTED                               
07683               MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY      
07684               MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY          
07685               MOVE PB-SV-CARRIER          TO PI-SV-CARRIER        
07686               MOVE PB-SV-GROUPING         TO PI-SV-GROUPING       
07687               MOVE PB-SV-STATE            TO PI-SV-STATE          
07688               MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW                  
07689               GO TO 7100-DISPLAY-CANCELS                          
07690            ELSE                                                   
07691               GO TO 4610-BUMP-SEQ.                                
07692                                                                   
07693      IF PB-CANCELLATION                                           
07694         IF CANCEL-WITH-WARNING                                    
07695            IF PB-WARNING-ERRORS                                   
07696               IF (PB-UNFORCED-ERRORS OR PB-FATAL-ERRORS)          
07697                   GO TO 4610-BUMP-SEQ                             
07698               ELSE                                                
07699                  MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY   
07700                  MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY       
07701                  MOVE PB-SV-CARRIER          TO PI-SV-CARRIER     
07702                  MOVE PB-SV-GROUPING         TO PI-SV-GROUPING    
07703                  MOVE PB-SV-STATE            TO PI-SV-STATE       
07704                  MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW               
07705                  GO TO 7100-DISPLAY-CANCELS                       
07706            ELSE                                                   
07707               GO TO 4610-BUMP-SEQ.                                
07708                                                                   
07709      IF PB-CANCELLATION                                           
07710         IF (CANCEL-IN-ERROR  AND                                  
07711             (PB-UNFORCED-ERRORS OR PB-FATAL-ERRORS)) OR           
07712             ALL-CANCELS                                           
07713             MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY        
07714             MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY            
07715             MOVE PB-SV-CARRIER          TO PI-SV-CARRIER          
07716             MOVE PB-SV-GROUPING         TO PI-SV-GROUPING         
07717             MOVE PB-SV-STATE            TO PI-SV-STATE            
07718             MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW                    
07719             GO TO 7100-DISPLAY-CANCELS.                           
07720                                                                   
07721  4610-BUMP-SEQ.                                                   
07722      IF ERPNDB-BATCH-SEQ-NO = ZEROS                               
07723         GO TO 4650-END-BATCH.                                     
07724                                                                   
07725      IF DISPLAY-CHANGE-RECORDS                                    
07726         IF ERPNDB-BATCH-CHG-SEQ-NO NOT = ZEROS                    
07727            SUBTRACT +1           FROM ERPNDB-BATCH-CHG-SEQ-NO     
07728         ELSE                                                      
07729            MOVE +999             TO   ERPNDB-BATCH-CHG-SEQ-NO     
07730            SUBTRACT +1           FROM ERPNDB-BATCH-SEQ-NO         
07731         ELSE                                                      
07732            MOVE ZEROS            TO   ERPNDB-BATCH-CHG-SEQ-NO     
07733            SUBTRACT +1           FROM ERPNDB-BATCH-SEQ-NO.        
07734                                                                   
07735         GO TO 4605-READ-NEXT.                                     
07736                                                                   
07737  4650-END-BATCH.                                                  
07738      IF PI-MAP-NAME = VP631B                                      
07739         MOVE -1                  TO BMAINTL                       
07740      ELSE                                                         
07741         IF PI-MAP-NAME = VP631C                                   
07742            MOVE -1               TO CMAINTL                       
07743         ELSE                                                      
07744            MOVE -1               TO DPFENTRL.                     
07745                                                                   
07746      MOVE ER-2252               TO EMI-ERROR.                     
07747      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
07748      GO TO 8200-SEND-DATAONLY.                                    
07749                                                                   
07750  4680-NOT-FOUND.                                                  
07751      IF DISPLAY-CHANGE-RECORDS                                    
07752         IF ERPNDB-BATCH-CHG-SEQ-NO NOT = ZEROS                    
07753            MOVE ZEROS            TO   ERPNDB-BATCH-CHG-SEQ-NO     
07754         ELSE                                                      
07755            MOVE +999             TO   ERPNDB-BATCH-CHG-SEQ-NO     
07756            SUBTRACT +1           FROM ERPNDB-BATCH-SEQ-NO         
07757      ELSE                                                         
07758         SUBTRACT +1              FROM ERPNDB-BATCH-SEQ-NO.        
07759                                                                   
07760      GO TO 4605-READ-NEXT.                                        
07761                                                                   
07762      EJECT                                                        
07763                                                                   
07764 ******************************************************************
07765 *                                                                *
07766 *        B R O W S E   F I L E   B A C K W A R D S               *
07767 *                                                                *
07768 ******************************************************************
07769                                                                   
07770  4800-BROWSE-FILE-BACKWARD.                                       
07771      EXEC CICS HANDLE CONDITION                                   
07772           NOTFND   (4860-NOT-FOUND)                               
07773           ENDFILE  (4850-END-RECORDS)                             
07774      END-EXEC.                                                    
07775                                                                   
07776      MOVE PI-PREV-KEY            TO ERPNDB-KEY.                   
07777                                                                   
07778      IF PI-DISPLAY-ORIGINAL-BATCH                                 
07779         MOVE FILE-ID-ERPNDB3     TO FILE-ID                       
07780      ELSE                                                         
07781         MOVE FILE-ID-ERPNDB      TO FILE-ID.                      
07782                                                                   
07783      EXEC CICS STARTBR                                            
07784           DATASET   (FILE-ID)                                     
07785           RIDFLD    (ERPNDB-KEY)                                  
07786      END-EXEC.                                                    
07787                                                                   
07788      MOVE 'Y'                    TO WS-BROWSE-STARTED-SW.         
07789                                                                   
07790      EXEC CICS READPREV                                           
07791           DATASET    (FILE-ID-ERPNDB)                             
07792           SET        (ADDRESS OF PENDING-BUSINESS)                
07793           RIDFLD     (ERPNDB-KEY)                                 
07794      END-EXEC.                                                    
07795                                                                   
07796  4810-READ-PREV-RECORD.                                           
07797      EXEC CICS READPREV                                           
07798           DATASET    (FILE-ID-ERPNDB)                             
07799           SET        (ADDRESS OF PENDING-BUSINESS)                
07800           RIDFLD     (ERPNDB-KEY)                                 
07801      END-EXEC.                                                    
07802                                                                   
07803      IF PB-COMPANY-CD-A1 NOT = PI-COMPANY-CD                      
07804         GO TO 4850-END-RECORDS.                                   
07805                                                                   
07806      IF PB-BATCH-TRAILER                                          
07807         GO TO 4810-READ-PREV-RECORD.                              
07808                                                                   
07809      MOVE PB-ENTRY-BATCH         TO WS-BATCH-NO.                  
07810                                                                   
07811      IF  WS-BATCH-PREFIX = '#CL'                                  
07812          GO TO 4810-READ-PREV-RECORD.                             
07813                                                                   
07814      IF PI-PB-CERT-NO NOT = LOW-VALUES                            
07815         IF PI-PB-CERT-NO NOT = PB-CERT-NO                         
07816            GO TO 4810-READ-PREV-RECORD                            
07817         ELSE                                                      
07818            IF PI-PB-CERT-EFF-DT = LOW-VALUES                      
07819               GO TO 4820-CHECK-REC-TYPE                           
07820            ELSE                                                   
07821               IF PI-PB-CERT-EFF-DT = PB-CERT-EFF-DT               
07822                  GO TO 4820-CHECK-REC-TYPE                        
07823               ELSE                                                
07824                  GO TO 4810-READ-PREV-RECORD.                     
07825                                                                   
07826      IF PI-PB-CERT-EFF-DT NOT = LOW-VALUES                        
07827         IF PI-PB-CERT-EFF-DT NOT = PB-CERT-EFF-DT                 
07828            GO TO 4810-READ-PREV-RECORD.                           
07829                                                                   
07830  4820-CHECK-REC-TYPE.                                             
07831      IF  PI-CARRIER-SECURITY GREATER THAN SPACES                  
07832          IF  PB-CARRIER = PI-CARRIER-SECURITY                     
07833              NEXT SENTENCE                                        
07834          ELSE                                                     
07835              GO TO 4810-READ-PREV-RECORD.                         
07836                                                                   
07837      IF  PI-ACCOUNT-SECURITY GREATER THAN SPACES                  
07838          IF  PB-ACCOUNT = PI-ACCOUNT-SECURITY                     
07839              NEXT SENTENCE                                        
07840         ELSE                                                      
07841              GO TO 4810-READ-PREV-RECORD.                         
07842                                                                   
07843      IF NOT DISPLAY-CHANGE-RECORDS AND                            
07844             PB-BATCH-CHG-SEQ-NO NOT = ZEROS                       
07845         GO TO 4810-READ-PREV-RECORD.                              
07846                                                                   
07847      IF DISPLAY-HOLD-RECORDS AND                                  
07848         NOT PB-RECORD-ON-HOLD                                     
07849           GO TO 4810-READ-PREV-RECORD.                            
07850                                                                   
07851      IF PB-ISSUE                                                  
07852         IF DISPLAY-CHK-REQ-RECORDS                                
07853            GO TO 4810-READ-PREV-RECORD.                           
07854                                                                   
07855      IF PB-ISSUE                                                  
07856         IF ISSUE-WITH-WARNING                                     
07857            IF PB-WARNING-ERRORS                                   
07858               IF (PB-UNFORCED-ERRORS OR PB-FATAL-ERRORS)          
07859                   GO TO 4810-READ-PREV-RECORD                     
07860               ELSE                                                
07861                  MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY   
07862                  MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY       
07863                  MOVE PB-SV-CARRIER          TO PI-SV-CARRIER     
07864                  MOVE PB-SV-GROUPING         TO PI-SV-GROUPING    
07865                  MOVE PB-SV-STATE            TO PI-SV-STATE       
07866                  MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW               
07867                  MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY       
07868                  GO TO 7000-DISPLAY-ISSUES                        
07869            ELSE                                                   
07870               GO TO 4810-READ-PREV-RECORD.                        
07871                                                                   
07872      IF PB-ISSUE                                                  
07873         IF (ISSUES-IN-ERROR AND                                   
07874             (PB-UNFORCED-ERRORS OR PB-FATAL-ERRORS)) OR           
07875             ALL-ISSUES                                            
07876             MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY        
07877             MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY            
07878             MOVE PB-SV-CARRIER          TO PI-SV-CARRIER          
07879             MOVE PB-SV-GROUPING         TO PI-SV-GROUPING         
07880             MOVE PB-SV-STATE            TO PI-SV-STATE            
07881             MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW                    
07882             MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY            
07883             GO TO 7000-DISPLAY-ISSUES                             
07884         ELSE                                                      
07885             GO TO 4810-READ-PREV-RECORD.                          
07886                                                                   
07887      IF PB-CANCELLATION                                           
07888         IF DISPLAY-CHK-REQ-RECORDS                                
07889            IF PB-C-REFUND-REQUESTED                               
07890               MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY      
07891               MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY          
07892               MOVE PB-SV-CARRIER          TO PI-SV-CARRIER        
07893               MOVE PB-SV-GROUPING         TO PI-SV-GROUPING       
07894               MOVE PB-SV-STATE            TO PI-SV-STATE          
07895               MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW                  
07896               MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY          
07897               GO TO 7100-DISPLAY-CANCELS                          
07898            ELSE                                                   
07899               GO TO 4810-READ-PREV-RECORD.                        
07900                                                                   
07901      IF PB-CANCELLATION                                           
07902         IF CANCEL-WITH-WARNING                                    
07903            IF PB-WARNING-ERRORS                                   
07904               IF (PB-UNFORCED-ERRORS OR PB-FATAL-ERRORS)          
07905                   GO TO 4810-READ-PREV-RECORD                     
07906               ELSE                                                
07907                  MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY   
07908                  MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY       
07909                  MOVE PB-SV-CARRIER          TO PI-SV-CARRIER     
07910                  MOVE PB-SV-GROUPING         TO PI-SV-GROUPING    
07911                  MOVE PB-SV-STATE            TO PI-SV-STATE       
07912                  MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW               
07913                  MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY       
07914                  GO TO 7100-DISPLAY-CANCELS                       
07915            ELSE                                                   
07916               GO TO 4810-READ-PREV-RECORD.                        
07917                                                                   
07918      IF PB-CANCELLATION                                           
07919         IF (CANCEL-IN-ERROR  AND                                  
07920             (PB-UNFORCED-ERRORS OR PB-FATAL-ERRORS)) OR           
07921             ALL-CANCELS                                           
07922             MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY        
07923             MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY            
07924             MOVE PB-SV-CARRIER          TO PI-SV-CARRIER          
07925             MOVE PB-SV-GROUPING         TO PI-SV-GROUPING         
07926             MOVE PB-SV-STATE            TO PI-SV-STATE            
07927             MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW                    
07928             MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY            
07929             GO TO 7100-DISPLAY-CANCELS.                           
07930                                                                   
07931      GO TO 4810-READ-PREV-RECORD.                                 
07932                                                                   
07933  4850-END-RECORDS.                                                
07934      MOVE ER-2252                TO EMI-ERROR.                    
07935      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
07936                                                                   
07937      IF PI-MAP-NAME = VP631B                                      
07938         MOVE -1                  TO BPFENTRL                      
07939      ELSE                                                         
07940         MOVE -1                  TO CPFENTRL.                     
07941                                                                   
07942      GO TO 8200-SEND-DATAONLY.                                    
07943                                                                   
07944  4860-NOT-FOUND.                                                  
07945                                                                   
07946 ******************************************************************
07947 *       CONDITION MAY OCCUR IF PREV RECORD HAS BEEN DELETED.     *
07948 ******************************************************************
07949                                                                   
07950      EXEC CICS RESETBR                                            
07951           DATASET    (FILE-ID-ERPNDB)                             
07952           RIDFLD     (ERPNDB-KEY)                                 
07953       END-EXEC.                                                   
07954                                                                   
07955      EXEC CICS READNEXT                                           
07956           DATASET    (FILE-ID-ERPNDB)                             
07957           SET        (ADDRESS OF PENDING-BUSINESS)                
07958           RIDFLD     (ERPNDB-KEY)                                 
07959      END-EXEC.                                                    
07960                                                                   
07961      EXEC CICS READPREV                                           
07962           DATASET    (FILE-ID-ERPNDB)                             
07963           SET        (ADDRESS OF PENDING-BUSINESS)                
07964           RIDFLD     (ERPNDB-KEY)                                 
07965      END-EXEC.                                                    
07966                                                                   
07967      GO TO 4810-READ-PREV-RECORD.                                 
07968                                                                   
07969      EJECT                                                        
07970                                                                   
07971 ******************************************************************
07972 *                                                                *
07973 *           B R O W S E   C S R   B A C K W A R D                *
07974 *                                                                *
07975 ******************************************************************
07976                                                                   
07977  4900-BROWSE-CSR-BACKWARD.                                        
07978                                                                   
07979      MOVE PI-PREV-CSR-KEY        TO ERPNDB-CSR-KEY.               
07980                                                                   
07981      EXEC CICS HANDLE CONDITION                                   
07982           NOTFND   (4970-NOT-FOUND)                               
07983           ENDFILE  (4950-END-RECORDS)                             
07984      END-EXEC                                                     
07985                                                                   
07986      EXEC CICS STARTBR                                            
07987           DATASET   (FILE-ID-ERPNDB4)                             
07988           RIDFLD    (ERPNDB-CSR-KEY)                              
07989      END-EXEC.                                                    
07990                                                                   
07991      MOVE 'Y'                    TO WS-BROWSE-STARTED-SW          
07992                                     WS-FIRST-TIME-SW.             
07993                                                                   
07994  4905-READ-PREV-RECORD.                                           
07995      EXEC CICS READPREV                                           
07996           DATASET    (FILE-ID-ERPNDB4)                            
07997           SET        (ADDRESS OF PENDING-BUSINESS)                
07998           RIDFLD     (ERPNDB-CSR-KEY)                             
07999      END-EXEC.                                                    
08000                                                                   
08001      MOVE PB-ENTRY-BATCH         TO WS-BATCH-NO.                  
08002                                                                   
08003      IF WS-BATCH-PREFIX = '#CL'                                   
08004          GO TO 4905-READ-PREV-RECORD.                             
08005                                                                   
08006      IF FIRST-TIME                                                
08007          MOVE SPACE               TO WS-FIRST-TIME-SW             
08008          GO TO 4905-READ-PREV-RECORD.                             
08009                                                                   
08010      IF PB-CSR-COMPANY-CD   NOT = PI-COMPANY-CD OR                
08011         PB-CSR-ID           NOT = PI-PB-CSR-ID                    
08012         GO TO 4950-END-RECORDS.                                   
08013                                                                   
08014      IF PB-BATCH-TRAILER                                          
08015         GO TO 4905-READ-PREV-RECORD.                              
08016                                                                   
08017      MOVE PB-ENTRY-BATCH         TO WS-BATCH-NO.                  
08018                                                                   
08019      IF  WS-BATCH-PREFIX = '#CL'                                  
08020          GO TO 4905-READ-PREV-RECORD.                             
08021                                                                   
08022      IF PI-PB-CERT-NO NOT = LOW-VALUES                            
08023         IF PI-PB-CERT-NO NOT = PB-CERT-NO                         
08024            GO TO 4905-READ-PREV-RECORD                            
08025         ELSE                                                      
08026            IF PI-PB-CERT-EFF-DT = LOW-VALUES                      
08027               GO TO 4910-CHECK-REC-TYPE                           
08028            ELSE                                                   
08029               IF PI-PB-CERT-EFF-DT = PB-CERT-EFF-DT               
08030                  GO TO 4910-CHECK-REC-TYPE                        
08031               ELSE                                                
08032                  GO TO 4905-READ-PREV-RECORD.                     
08033                                                                   
08034      IF PI-PB-CERT-EFF-DT NOT = LOW-VALUES                        
08035         IF PI-PB-CERT-EFF-DT NOT = PB-CERT-EFF-DT                 
08036            GO TO 4905-READ-PREV-RECORD.                           
08037                                                                   
08038  4910-CHECK-REC-TYPE.                                             
08039      IF  PI-CARRIER-SECURITY GREATER THAN SPACES                  
08040          IF  PB-CARRIER = PI-CARRIER-SECURITY                     
08041              NEXT SENTENCE                                        
08042          ELSE                                                     
08043              GO TO 4905-READ-PREV-RECORD.                         
08044                                                                   
08045      IF  PI-ACCOUNT-SECURITY GREATER THAN SPACES                  
08046          IF  PB-ACCOUNT = PI-ACCOUNT-SECURITY                     
08047              NEXT SENTENCE                                        
08048         ELSE                                                      
08049              GO TO 4905-READ-PREV-RECORD.                         
08050                                                                   
08051      IF NOT DISPLAY-CHANGE-RECORDS AND                            
08052             PB-BATCH-CHG-SEQ-NO NOT = ZEROS                       
08053         GO TO 4905-READ-PREV-RECORD.                              
08054                                                                   
08055      IF DISPLAY-HOLD-RECORDS AND                                  
08056         NOT PB-RECORD-ON-HOLD                                     
08057           GO TO 4905-READ-PREV-RECORD.                            
08058                                                                   
08059      IF PB-ISSUE                                                  
08060         IF DISPLAY-CHK-REQ-RECORDS                                
08061            GO TO 4905-READ-PREV-RECORD.                           
08062                                                                   
08063      IF PB-ISSUE                                                  
08064         IF ISSUE-WITH-WARNING                                     
08065            IF PB-WARNING-ERRORS                                   
08066               IF (PB-UNFORCED-ERRORS OR PB-FATAL-ERRORS)          
08067                   GO TO 4505-READ-PREV-RECORD                     
08068               ELSE                                                
08069                  MOVE PB-CONTROL-BY-CSR      TO PI-PREV-CSR-KEY   
08070                  MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY   
08071                  MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY       
08072                  MOVE PB-SV-CARRIER          TO PI-SV-CARRIER     
08073                  MOVE PB-SV-GROUPING         TO PI-SV-GROUPING    
08074                  MOVE PB-SV-STATE            TO PI-SV-STATE       
08075                  MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW               
08076                  GO TO 7000-DISPLAY-ISSUES                        
08077            ELSE                                                   
08078               GO TO 4905-READ-PREV-RECORD.                        
08079                                                                   
08080      IF PB-ISSUE                                                  
08081         IF (ISSUES-IN-ERROR AND                                   
08082             (PB-UNFORCED-ERRORS OR PB-FATAL-ERRORS)) OR           
08083             ALL-ISSUES                                            
08084             MOVE PB-CONTROL-BY-CSR      TO PI-PREV-CSR-KEY        
08085             MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY        
08086             MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY            
08087             MOVE PB-SV-CARRIER          TO PI-SV-CARRIER          
08088             MOVE PB-SV-GROUPING         TO PI-SV-GROUPING         
08089             MOVE PB-SV-STATE            TO PI-SV-STATE            
08090             MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW                    
08091             GO TO 7000-DISPLAY-ISSUES                             
08092         ELSE                                                      
08093             GO TO 4905-READ-PREV-RECORD.                          
08094                                                                   
08095      IF PB-CANCELLATION                                           
08096         IF DISPLAY-CHK-REQ-RECORDS                                
08097            IF PB-C-REFUND-REQUESTED                               
08098               MOVE PB-CONTROL-BY-CSR      TO PI-PREV-CSR-KEY      
08099               MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY      
08100               MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY          
08101               MOVE PB-SV-CARRIER          TO PI-SV-CARRIER        
08102               MOVE PB-SV-GROUPING         TO PI-SV-GROUPING       
08103               MOVE PB-SV-STATE            TO PI-SV-STATE          
08104               MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW                  
08105               GO TO 7100-DISPLAY-CANCELS                          
08106            ELSE                                                   
08107               GO TO 4905-READ-PREV-RECORD.                        
08108                                                                   
08109      IF PB-CANCELLATION                                           
08110         IF CANCEL-WITH-WARNING                                    
08111            IF PB-WARNING-ERRORS                                   
08112               IF (PB-UNFORCED-ERRORS OR PB-FATAL-ERRORS)          
08113                   GO TO 4505-READ-PREV-RECORD                     
08114               ELSE                                                
08115                  MOVE PB-CONTROL-BY-CSR      TO PI-PREV-CSR-KEY   
08116                  MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY   
08117                  MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY       
08118                  MOVE PB-SV-CARRIER          TO PI-SV-CARRIER     
08119                  MOVE PB-SV-GROUPING         TO PI-SV-GROUPING    
08120                  MOVE PB-SV-STATE            TO PI-SV-STATE       
08121                  MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW               
08122                  GO TO 7100-DISPLAY-CANCELS                       
08123            ELSE                                                   
08124               GO TO 4905-READ-PREV-RECORD.                        
08125                                                                   
08126      IF PB-CANCELLATION                                           
08127         IF (CANCEL-IN-ERROR  AND                                  
08128             (PB-UNFORCED-ERRORS OR PB-FATAL-ERRORS)) OR           
08129             ALL-CANCELS                                           
08130             MOVE PB-CONTROL-BY-CSR      TO PI-PREV-CSR-KEY        
08131             MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY        
08132             MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY            
08133             MOVE PB-SV-CARRIER          TO PI-SV-CARRIER          
08134             MOVE PB-SV-GROUPING         TO PI-SV-GROUPING         
08135             MOVE PB-SV-STATE            TO PI-SV-STATE            
08136             MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW                    
08137             GO TO 7100-DISPLAY-CANCELS.                           
08138                                                                   
08139      GO TO 4905-READ-PREV-RECORD.                                 
08140                                                                   
08141  4950-END-RECORDS.                                                
08142      IF PI-MAP-NAME = VP631B                                      
08143         MOVE -1                  TO BMAINTL                       
08144      ELSE                                                         
08145         IF PI-MAP-NAME = VP631C                                   
08146            MOVE -1               TO CMAINTL                       
08147         ELSE                                                      
08148            MOVE -1               TO DPFENTRL.                     
08149                                                                   
08150      MOVE ER-2255               TO EMI-ERROR.                     
08151      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
08152      GO TO 8200-SEND-DATAONLY.                                    
08153                                                                   
08154  4970-NOT-FOUND.                                                  
08155                                                                   
08156 ******************************************************************
08157 *      CONDITION MAY OCCUR IF PREV. RECORD HAS BEEN DELETED.     *
08158 ******************************************************************
08159                                                                   
08160      EXEC CICS RESETBR                                            
08161           DATASET    (FILE-ID-ERPNDB4)                            
08162           RIDFLD     (ERPNDB-CSR-KEY)                             
08163      END-EXEC.                                                    
08164                                                                   
08165      EXEC CICS READNEXT                                           
08166           DATASET    (FILE-ID-ERPNDB4)                            
08167           SET        (ADDRESS OF PENDING-BUSINESS)                
08168           RIDFLD     (ERPNDB-CSR-KEY)                             
08169      END-EXEC.                                                    
08170                                                                   
08171      EXEC CICS READPREV                                           
08172           DATASET    (FILE-ID-ERPNDB4)                            
08173           SET        (ADDRESS OF PENDING-BUSINESS)                
08174           RIDFLD     (ERPNDB-CSR-KEY)                             
08175      END-EXEC.                                                    
08176                                                                   
08177  4980-READ-PREV-RECORD.                                           
08178      EXEC CICS READPREV                                           
08179           DATASET    (FILE-ID-ERPNDB4)                            
08180           SET        (ADDRESS OF PENDING-BUSINESS)                
08181           RIDFLD     (ERPNDB-CSR-KEY)                             
08182      END-EXEC.                                                    
08183                                                                   
08184      MOVE PB-ENTRY-BATCH         TO WS-BATCH-NO.                  
08185                                                                   
08186      IF WS-BATCH-PREFIX = '#CL'                                   
08187         GO TO 4980-READ-PREV-RECORD.                              
08188                                                                   
08189      MOVE SPACE                  TO WS-FIRST-TIME-SW.             
08190      GO TO 4905-READ-PREV-RECORD.                                 
08191                                                                   
08192      EJECT                                                        
08193                                                                   
08194                                                                   
08195 ******************************************************************
08196 *                                                                *
08197 *        B R O W S E   B A T C H   H E A D E R S                 *
08198 *                                                                *
08199 ******************************************************************
08200                                                                   
08201  5000-BROWSE-BATCH-HEADERS.                                       
08202                                                                   
08203      IF EIBTRNID NOT = TRANS-EXB1                                 
08204         MOVE PI-ERPNDB-KEY       TO ERPNDB-KEY                    
08205         MOVE +9999               TO ERPNDB-BATCH-SEQ-NO           
08206         MOVE ZEROS               TO ERPNDB-BATCH-CHG-SEQ-NO       
08207         GO TO 5010-READ-NEXT-RECORD.                              
08208                                                                   
08209      IF EIBAID = DFHPF4                                           
08210         MOVE '0'                TO PI-EDIT-SW                     
08211         MOVE PI-PREV-KEY         TO ERPNDB-KEY                    
08212         MOVE +9999               TO ERPNDB-BATCH-SEQ-NO           
08213         MOVE ZEROS               TO ERPNDB-BATCH-CHG-SEQ-NO       
08214         GO TO 5010-READ-NEXT-RECORD.                              
08215                                                                   
08216      MOVE PI-PREV-KEY            TO ERPNDB-KEY.                   
08217      MOVE HIGH-VALUES            TO ERPNDB-BATCH-SEQ-ALPHA.       
08218      MOVE ZEROS                  TO ERPNDB-BATCH-CHG-SEQ-NO.      
08219                                                                   
08220      EXEC CICS HANDLE CONDITION                                   
08221           ENDFILE  (5050-NOT-FOUND)                               
08222      END-EXEC.                                                    
08223                                                                   
08224  5010-READ-NEXT-RECORD.                                           
08225      EXEC CICS HANDLE CONDITION                                   
08226           NOTFND    (5050-NOT-FOUND)                              
08227      END-EXEC.                                                    
08228                                                                   
08229      EXEC CICS READ                                               
08230           DATASET   (FILE-ID-ERPNDB)                              
08231           SET       (ADDRESS OF PENDING-BUSINESS)                 
08232           RIDFLD    (ERPNDB-KEY)                                  
08233           GTEQ                                                    
08234      END-EXEC.                                                    
08235                                                                   
08236      IF PB-COMPANY-CD NOT = PI-COMPANY-CD                         
08237          GO TO 5050-NOT-FOUND.                                    
08238                                                                   
08239      MOVE PB-ENTRY-BATCH         TO WS-BATCH-NO.                  
08240                                                                   
08241      IF WS-BATCH-PREFIX = '#CL'                                   
08242          MOVE PB-ENTRY-BATCH   TO ERPNDB-ENTRY-BATCH              
08243          MOVE HIGH-VALUES      TO ERPNDB-BATCH-SEQ-ALPHA          
08244          GO TO 5010-READ-NEXT-RECORD.                             
08245                                                                   
08246      IF NOT PB-BATCH-TRAILER                                      
08247          MOVE PB-ENTRY-BATCH   TO ERPNDB-ENTRY-BATCH              
08248          MOVE +9999            TO ERPNDB-BATCH-SEQ-NO             
08249          GO TO 5010-READ-NEXT-RECORD.                             
08250                                                                   
08251      IF  PI-CARRIER-SECURITY GREATER THAN SPACES                  
08252          IF  PB-CARRIER = PI-CARRIER-SECURITY                     
08253              NEXT SENTENCE                                        
08254          ELSE                                                     
08255              MOVE PB-ENTRY-BATCH     TO ERPNDB-ENTRY-BATCH        
08256              MOVE HIGH-VALUES        TO ERPNDB-BATCH-SEQ-ALPHA    
08257              MOVE ZEROS              TO ERPNDB-BATCH-CHG-SEQ-NO   
08258              GO TO 5010-READ-NEXT-RECORD.                         
08259                                                                   
08260      IF  PI-ACCOUNT-SECURITY GREATER THAN SPACES                  
08261          IF  PB-ACCOUNT = PI-ACCOUNT-SECURITY                     
08262              NEXT SENTENCE                                        
08263         ELSE                                                      
08264              MOVE HIGH-VALUES        TO ERPNDB-BATCH-SEQ-ALPHA    
08265              MOVE ZEROS              TO ERPNDB-BATCH-CHG-SEQ-NO   
08266              GO TO 5010-READ-NEXT-RECORD.                         
08267                                                                   
08268      IF EIBAID = DFHPF4                                           
08269         MOVE '0'                TO PI-EDIT-SW                     
08270         GO TO 7200-DISPLAY-BATCHES.                               
08271                                                                   
08272      IF PI-CSR-BROWSE                                             
08273         IF PB-CSR-ID           NOT = PI-PB-CSR-ID                 
08274            MOVE PB-ENTRY-BATCH   TO ERPNDB-ENTRY-BATCH            
08275            MOVE HIGH-VALUES      TO ERPNDB-BATCH-SEQ-ALPHA        
08276            GO TO 5010-READ-NEXT-RECORD.                           
08277                                                                   
08278      IF ONLY-BATCH-HEADERS OR PB-OUT-OF-BAL                       
08279        IF PI-PB-CARRIER    = SPACES AND                           
08280           PI-PB-GROUPING   = SPACES AND                           
08281           PI-PB-STATE      = SPACES AND                           
08282           PI-PB-ACCOUNT    = SPACES                               
08283             MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY            
08284             MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW                    
08285             GO TO 7200-DISPLAY-BATCHES                            
08286          ELSE                                                     
08287            IF PI-PB-CARRIER    = PB-CARRIER  AND                  
08288               PI-PB-GROUPING   = PB-GROUPING AND                  
08289               PI-PB-STATE      = PB-STATE    AND                  
08290               PI-PB-ACCOUNT    = PB-ACCOUNT                       
08291               MOVE PB-CONTROL-PRIMARY  TO PI-PREV-KEY             
08292               MOVE 'Y' TO WS-PB-RECORDS-FOUND-SW                  
08293               GO TO 7200-DISPLAY-BATCHES.                         
08294                                                                   
08295      MOVE PB-CONTROL-PRIMARY        TO ERPNDB-KEY.                
08296      MOVE HIGH-VALUE                TO ERPNDB-BATCH-SEQ-ALPHA.    
08297                                                                   
08298      EXEC CICS HANDLE CONDITION                                   
08299           NOTFND    (5050-NOT-FOUND)                              
08300      END-EXEC.                                                    
08301                                                                   
08302      EXEC CICS READ                                               
08303           DATASET   (FILE-ID-ERPNDB)                              
08304           SET       (ADDRESS OF PENDING-BUSINESS)                 
08305           RIDFLD    (ERPNDB-KEY)                                  
08306           GTEQ                                                    
08307      END-EXEC.                                                    
08308                                                                   
08309      GO TO 5010-READ-NEXT-RECORD.                                 
08310                                                                   
08311  5050-NOT-FOUND.                                                  
08312                                                                   
08313 ******************************************************************
08314 *                                                                *
08315 *         IF THERE ARE NO PENDING BUS. RECORDS FOUND DURING      *
08316 *         THE INITIAL ENTRY OF VP6311, RETURN TO THE CALLING     *
08317 *         PROGRAM.                                               *
08318 *                                                                *
08319 ******************************************************************
08320                                                                   
08321      IF EIBTRNID NOT = TRANS-EXB1                                 
08322         IF PENDING-BUS-RECS-FOUND                                 
08323             NEXT SENTENCE                                         
08324           ELSE                                                    
08325             MOVE '9'            TO PI-BROWSE-SW                   
08326             GO TO 9400-CLEAR.                                     
08327                                                                   
08328      MOVE ER-2256                TO EMI-ERROR.                    
08329      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
08330                                                                   
08331      IF PI-MAP-NAME = SPACES                                      
08332         MOVE VP631D              TO PI-MAP-NAME                   
08333         MOVE -1                  TO DPFENTRL                      
08334         GO TO 8100-SEND-INITIAL-MAP.                              
08335                                                                   
08336      IF PI-MAP-NAME = VP631B                                      
08337         MOVE -1                  TO BMAINTL                       
08338      ELSE                                                         
08339         IF PI-MAP-NAME = VP631C                                   
08340            MOVE -1               TO CMAINTL                       
08341         ELSE                                                      
08342            MOVE -1               TO DPFENTRL                      
08343            GO TO 8200-SEND-DATAONLY.                              
08344                                                                   
08345      GO TO 8100-SEND-INITIAL-MAP.                                 
08346                                                                   
           .
       5100-PROCESS-POL-RESC.

           MOVE ZEROS                  TO CG-LF-BENCD
                                          CG-AH-BENCD
                                          CG-LF-CAN-AMT
                                          CG-AH-CAN-AMT
           MOVE LOW-VALUES             TO CG-LF-CAN-DT
                                          CG-AH-CAN-DT
           MOVE SPACES                 TO CG-BATCH-NO

           MOVE '4'                    TO CG-OPTION-CODE
           MOVE PI-COMPANY-ID          TO CG-COMPANY-ID
           MOVE PI-PROCESSOR-ID        TO CG-PROC-ID
           MOVE PI-CR-MONTH-END-DT     TO CG-MONTH-END-DT
           MOVE PI-COMPANY-CD          TO CG-CERT-COMPANY-CD
           MOVE PI-SV-CARRIER          TO CG-CERT-CARRIER
           MOVE PI-SV-GROUPING         TO CG-CERT-GROUP
           MOVE PI-SV-STATE            TO CG-CERT-STATE
           MOVE PI-PB-ACCOUNT          TO CG-CERT-ACCOUNT
           MOVE PI-PB-CERT-EFF-DT      TO CG-CERT-EFF-DT
           MOVE PI-PB-CERT-NO          TO CG-CERT-CERT-NO
           MOVE PI-PREV-ALT-KEY (1:33)
                                       TO CG-CERT-KEY
           MOVE WS-CURRENT-BIN-DT      TO CG-CURRENT-DT
           MOVE ZEROS                  TO CG-LF-CAN-AMT
                                          CG-AH-CAN-AMT
           MOVE LOW-VALUES             TO CG-LF-CAN-DT
                                          CG-AH-CAN-DT
010412     MOVE PI-PREV-BATCH          TO CG-BATCH-NO
010412     MOVE ZERO                   TO CG-BATCH-SEQ-NO

            display 'about to cancel ' cg-cert-key
           EXEC CICS LINK
               PROGRAM  ('ELCANC')
               COMMAREA (CANCEL-GEN-PASS-AREA)
           END-EXEC
           IF CG-SUCCESS
              CONTINUE
           ELSE
010412        MOVE SPACES              TO EMI-MESSAGE-AREA (1)
              MOVE '3'                 TO EMI-SWITCH1
              MOVE CG-ERROR-CODE TO EMI-TEXT-VARIABLE (1)
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
010412             MOVE ' ERROR - ELCANC - RETURN '
                                       TO EMI-ERROR-TEXT (1) (12:25)
010412        END-EVALUATE
              GO TO 8200-SEND-DATAONLY
           END-IF

           .
       5100-EXIT.
           EXIT.

08348                                                                   
08349 ******************************************************************
08350 *                                                                *
08351 *        B R O W S E   B A T C H   B A C K W A R D S             *
08352 *                                                                *
08353 ******************************************************************
08354                                                                   
08355  5500-BROWSE-BATCH-BACKWARD.                                      
08356      EXEC CICS HANDLE CONDITION                                   
08357           NOTFND   (5550-END-RECORDS)                             
08358           ENDFILE  (5550-END-RECORDS)                             
08359      END-EXEC.                                                    
08360                                                                   
08361  5505-START-BROWSE.                                               
08362      MOVE PI-PREV-KEY            TO ERPNDB-KEY.                   
08363      MOVE +1                     TO ERPNDB-BATCH-SEQ-NO.          
08364      MOVE ZEROS                  TO ERPNDB-BATCH-CHG-SEQ-NO.      
08365                                                                   
08366      EXEC CICS STARTBR                                            
08367           DATASET   (FILE-ID-ERPNDB)                              
08368           RIDFLD    (ERPNDB-ALT-KEY)                              
08369      END-EXEC.                                                    
08370                                                                   
08371      MOVE 'Y'                    TO WS-BROWSE-STARTED-SW.         
08372                                                                   
08373      EXEC CICS READNEXT                                           
08374           DATASET    (FILE-ID-ERPNDB)                             
08375           SET        (ADDRESS OF PENDING-BUSINESS)                
08376           RIDFLD     (ERPNDB-KEY)                                 
08377      END-EXEC.                                                    
08378                                                                   
08379      EXEC CICS READPREV                                           
08380           DATASET    (FILE-ID-ERPNDB)                             
08381           SET        (ADDRESS OF PENDING-BUSINESS)                
08382           RIDFLD     (ERPNDB-KEY)                                 
08383      END-EXEC.                                                    
08384                                                                   
08385      EXEC CICS READPREV                                           
08386           DATASET    (FILE-ID-ERPNDB)                             
08387           SET        (ADDRESS OF PENDING-BUSINESS)                
08388           RIDFLD     (ERPNDB-KEY)                                 
08389      END-EXEC.                                                    
08390                                                                   
08391      IF PB-COMPANY-CD-A1 NOT = PI-COMPANY-CD                      
08392         GO TO 5550-END-RECORDS.                                   
08393                                                                   
08394      MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY.                  
08395                                                                   
08396      MOVE PB-ENTRY-BATCH         TO WS-BATCH-NO.                  
08397                                                                   
08398      IF  WS-BATCH-PREFIX = '#CL'                                  
08399              MOVE SPACE               TO WS-BROWSE-STARTED-SW     
08400              EXEC CICS ENDBR                                      
08401                   DATASET  (FILE-ID-ERPNDB)                       
08402              END-EXEC                                             
08403          GO TO 5505-START-BROWSE.                                 
08404                                                                   
08405      IF  PI-CARRIER-SECURITY GREATER THAN SPACES                  
08406          IF  PB-CARRIER = PI-CARRIER-SECURITY                     
08407              NEXT SENTENCE                                        
08408          ELSE                                                     
08409              MOVE SPACE               TO WS-BROWSE-STARTED-SW     
08410              EXEC CICS ENDBR                                      
08411                   DATASET  (FILE-ID-ERPNDB)                       
08412              END-EXEC                                             
08413              GO TO 5505-START-BROWSE.                             
08414                                                                   
08415                                                                   
08416      IF  PI-ACCOUNT-SECURITY GREATER THAN SPACES                  
08417          IF  PB-ACCOUNT = PI-ACCOUNT-SECURITY                     
08418              NEXT SENTENCE                                        
08419         ELSE                                                      
08420              MOVE SPACE               TO WS-BROWSE-STARTED-SW     
08421              EXEC CICS ENDBR                                      
08422                   DATASET  (FILE-ID-ERPNDB)                       
08423              END-EXEC                                             
08424              GO TO 5505-START-BROWSE.                             
08425                                                                   
08426      IF NOT PB-BATCH-TRAILER                                      
08427         MOVE SPACE               TO WS-BROWSE-STARTED-SW          
08428         EXEC CICS ENDBR                                           
08429              DATASET  (FILE-ID-ERPNDB)                            
08430         END-EXEC                                                  
08431         GO TO 5505-START-BROWSE.                                  
08432                                                                   
08433      IF PI-CSR-BROWSE                                             
08434         IF PB-CSR-ID           NOT = PI-PB-CSR-ID                 
08435            MOVE SPACE            TO WS-BROWSE-STARTED-SW          
08436            EXEC CICS ENDBR                                        
08437                 DATASET  (FILE-ID-ERPNDB)                         
08438            END-EXEC                                               
08439            GO TO 5505-START-BROWSE.                               
08440                                                                   
08441      IF ONLY-BATCH-HEADERS OR PB-OUT-OF-BAL                       
08442         IF PI-PB-CARRIER    = SPACES AND                          
08443            PI-PB-GROUPING   = SPACES AND                          
08444            PI-PB-STATE      = SPACES AND                          
08445            PI-PB-ACCOUNT    = SPACES                              
08446              GO TO 7200-DISPLAY-BATCHES                           
08447           ELSE                                                    
08448             IF PI-PB-CARRIER    = PB-CARRIER  AND                 
08449                PI-PB-GROUPING   = PB-GROUPING AND                 
08450                PI-PB-STATE      = PB-STATE    AND                 
08451                PI-PB-ACCOUNT    = PB-ACCOUNT                      
08452                  GO TO 7200-DISPLAY-BATCHES.                      
08453                                                                   
08454      MOVE SPACE                  TO WS-BROWSE-STARTED-SW.         
08455                                                                   
08456      EXEC CICS ENDBR                                              
08457           DATASET  (FILE-ID-ERPNDB)                               
08458      END-EXEC.                                                    
08459                                                                   
08460      GO TO 5505-START-BROWSE.                                     
08461                                                                   
08462  5550-END-RECORDS.                                                
08463      MOVE ER-2257                TO EMI-ERROR.                    
08464      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
08465      MOVE -1                     TO DPFENTRL.                     
08466                                                                   
08467      GO TO 8200-SEND-DATAONLY.                                    
08468                                                                   
08469      EJECT                                                        
08470                                                                   
08471 ******************************************************************
08472 *                                                                *
08473 *        C R E A T E   T E M P O R A R Y   S T O R A G E         *
08474 *                                                                *
08475 ******************************************************************
08476                                                                   
08477  6000-CREATE-TEMP-STORAGE.                                        
08478                                                                   
08479      EXEC CICS HANDLE CONDITION                                   
08480          QIDERR  (6050-QID-ERROR)                                 
08481      END-EXEC.                                                    
08482                                                                   
08483      EXEC CICS WRITEQ TS                                          
08484          QUEUE    (QID)                                           
08485          FROM     (PROGRAM-INTERFACE-BLOCK)                       
08486          LENGTH   (WS-COMM-LENGTH)                                
08487          ITEM     (W-ONE)                                         
08488      END-EXEC.                                                    
08489                                                                   
08490  6000-EXIT.                                                       
08491       EXIT.                                                       
08492                                                                   
08493      EJECT                                                        
08494                                                                   
08495 ******************************************************************
08496 *                                                                *
08497 *        R E C O V E R   T E M P O R A R Y   S T O R A G E       *
08498 *                                                                *
08499 ******************************************************************
08500                                                                   
08501  6050-RECOVER-TEMP-STORAGE.                                       
08502      IF PGM-NAME = XCTL-EL6312                                    
08503         GO TO 6050-EXIT.                                          
08504                                                                   
08505      IF PGM-NAME = XCTL-EL6313                                    
08506         GO TO 6050-EXIT.                                          
072312
072312     IF PGM-NAME = XCTL-EL6315
072312         MOVE PI-PROGRAM-WORK-AREA TO WS-EL6315-PASS-AREA
072312     END-IF
072312
072312     IF PGM-NAME = XCTL-EL6316
072312         MOVE PI-PROGRAM-WORK-AREA TO WS-EL6316-PASS-AREA
072312     END-IF
08507                                                                   
08508      EXEC CICS HANDLE CONDITION                                   
08509          QIDERR  (6050-QID-ERROR)                                 
08510      END-EXEC.                                                    
08511                                                                   
08512      EXEC CICS READQ TS                                           
08513           QUEUE   (QID)                                           
08514           INTO    (PROGRAM-INTERFACE-BLOCK)                       
08515           LENGTH  (WS-COMM-LENGTH)                                
08516           ITEM    (W-ONE)                                         
08517       END-EXEC.                                                   
08518                                                                   
08519  6050-DELETE-TEMP-STORAGE.                                        
08520                                                                   
08521      EXEC CICS DELETEQ TS                                         
08522          QUEUE   (QID)                                            
08523      END-EXEC.                                                    
08524                                                                   
08525      GO TO 6050-EXIT.                                             
08526                                                                   
08527  6050-QID-ERROR.                                                  
08528      MOVE ER-0033                TO EMI-ERROR                     
08529      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
08530                                                                   
08531  6050-EXIT.                                                       
08532       EXIT.                                                       
08533                                                                   
08534      EJECT                                                        
08535                                                                   
08536 ******************************************************************
08537 *                                                                *
08538 *            T E R M   C O N V E R S I O N                       *
08539 *                                                                *
08540 ******************************************************************
08541                                                                   
08542  6100-TERM-CONVERSION.                                            
08543                                                                   
08544      IF PI-BPMTS = ZERO                                           
08545          GO TO 6199-EXIT.                                         
08546                                                                   
08565                                                                   
08566  6105-ROUND-TERM.                                                 
08567                                                                   
08568      IF WS-CALC-TERM-REMAIN GREATER THAN .00000                   
08569          ADD +1                  TO WS-CALC-TERM-WHOLE            
08570          MOVE ZEROS              TO WS-CALC-TERM-REMAIN.          
08571                                                                   
08572      IF PI-COMPANY-ID = CLIENT-CRI OR CLIENT-LGX                  
08573          GO TO 6125-COMPUTE-TERM-BENEFIT.                         
08574                                                                   
08575      IF BTERM-LEN (WS-SUB1) NOT GREATER THAN +0                   
08576          MOVE WS-CALC-TERM-WHOLE   TO BTERMO      (WS-SUB1)       
08577                                       WS-BTERM    (WS-SUB1)       
08578          MOVE +3                   TO BTERM-LEN   (WS-SUB1).      
08579                                                                   
08586                                                                   
08587  6124-CHECK-BENEFIT.                                              
08588                                                                   
08589      IF BBEN-LEN (WS-SUB1) NOT GREATER THAN +0                    
08590         GO TO 6126-COMPUTE-BENEFIT.                               
08591                                                                   
08592      GO TO 6199-EXIT.                                             
08593                                                                   
08594  EJECT                                                            
08595 ******************************************************************
08596 **         COMPUTE TERM AND BENEFIT                               
08597 ******************************************************************
08598  6125-COMPUTE-TERM-BENEFIT.                                       
08599                                                                   
08600      IF PI-BTYPE (WS-SUB1) = SPACES  OR                           
08601         PI-BPMTAMT = ZERO                                         
08602          GO TO 6199-EXIT.                                         
08603                                                                   
08604      MOVE WS-CALC-TERM-WHOLE     TO  BTERMI    (WS-SUB1)          
08605                                      WS-BTERM  (WS-SUB1).         
08606      MOVE +3                     TO  BTERM-LEN (WS-SUB1).         
08607                                                                   
08608  6126-COMPUTE-BENEFIT.                                            
08609                                                                   
08624                                                                   
08625      IF WS-SUB1 = +1                                              
08626         COMPUTE WS-BBEN (WS-SUB1) =                               
08627                 WS-BBEN (WS-SUB1) * WS-CALC-TERM.                 
08628                                                                   
08629      MOVE WS-BBEN (WS-SUB1)           TO BBENO       (WS-SUB1).   
08630      MOVE +12                         TO BBEN-LEN    (WS-SUB1).   
08631      MOVE AL-UNNON                    TO BBEN-ATTRB  (WS-SUB1).   
08632                                                                   
08633  6199-EXIT.                                                       
08634      EXIT.                                                        
08635      EJECT                                                        
08636                                                                   
08637 ******************************************************************
08638 *                                                                *
08639 *             D I S P L A Y   I S S U E S                        *
08640 *                                                                *
08641 ******************************************************************
08642                                                                   
030708 6200-CREATE-VADS.

030708     MOVE PI-PREV-KEY            TO PI-ERPNDB-KEY
030708     MOVE  PI-PREV-CONTROL-PRIMARY TO ERPNDB-KEY
100111     MOVE SPACES                 TO WS-PASS-VADS-REC

030708     EXEC CICS GETMAIN
030708        SET      (ADDRESS OF PENDING-BUSINESS)
030708        LENGTH   (ERPNDB-RECORD-LENGTH)
030708        INITIMG  (GETMAIN-SPACE)
030708        RESP     (WS-RESPONSE)
030708     END-EXEC

030708     EXEC CICS READ
030708        INTO    (PENDING-BUSINESS)
030708        DATASET (FILE-ID-ERPNDB)
030708        RIDFLD  (ERPNDB-KEY)
030708        RESP    (WS-RESPONSE)
030708     END-EXEC

030708     IF RESP-NORMAL
              PERFORM 6210-READ-ERACCT THRU 6210-EXIT
              IF (RESP-NORMAL)
                 AND (PB-CONTROL-BY-ACCOUNT (1:20) =
                    AM-CONTROL-PRIMARY (1:20))
100111           MOVE AM-NAME          TO VD-ACCOUNT-NAME
              ELSE
                 MOVE 'ACCOUNT NOT FOUND'
100111                                 TO VD-ACCOUNT-NAME

              END-IF
              
100111        MOVE FUNCTION LENGTH(VD-ACCOUNT-NAME)
100111                                    TO M1
100111        MOVE VD-ACCOUNT-NAME        TO WS-WORK-FIELD
100111        PERFORM VARYING A2 FROM +1 BY +1 UNTIL 
100111           (A2 > M1)
100111           OR (WS-WORK-FIELD (A2:1) = '*')
091112           OR (WS-WORK-FIELD (A2:4) = ' DBA' OR ' AKA')
100111           OR (WS-WORK-FIELD (A2:4) = '(DBA' OR '(AKA')
100111        END-PERFORM
100111        IF A2 > M1
100111           CONTINUE
100111        ELSE
100111           MOVE WS-WORK-FIELD (1:A2 - 1)
100111                                    TO VD-ACCOUNT-NAME
100111        END-IF
100111        MOVE PB-COMPANY-CD       TO VD-COMPANY-CD
100111        MOVE PB-CARRIER          TO VD-CARRIER
100111        MOVE PB-GROUPING         TO VD-GROUPING
100111        MOVE PB-STATE            TO VD-STATE
100111        MOVE PB-ACCOUNT          TO VD-ACCOUNT
100111        MOVE PB-CERT-EFF-DT      TO VD-CERT-EFF-DT
100111        MOVE PB-CERT-NO          TO VD-CERT-NO
100111        MOVE PB-I-INSURED-LAST-NAME TO VD-INSURED-LAST-NAME
100111        MOVE PB-I-INSURED-FIRST-NAME TO VD-INSURED-FIRST-NAME
100111        MOVE PB-I-INSURED-MIDDLE-INIT TO VD-INSURED-MIDDLE-INIT
100111        MOVE PB-I-JOINT-LAST-NAME TO VD-JOINT-LAST-NAME
100111        MOVE PB-I-JOINT-FIRST-NAME TO VD-JOINT-FIRST-NAME
100111        MOVE PB-I-JOINT-MIDDLE-INIT TO VD-JOINT-MIDDLE-INIT
100111        MOVE PB-I-BENEFICIARY-NAME TO VD-BENEFICIARY-NAME
100111        MOVE PB-ENTRY-BATCH      TO VD-ENTRY-BATCH
100111        MOVE PB-CSR-ID           TO VD-CSR-ID
100111        MOVE PB-I-LF-BENEFIT-CD  TO VD-LF-BENEFIT-CD
100111        MOVE PB-I-LF-PREMIUM-AMT TO VD-LF-PREMIUM-AMT
100111        MOVE PB-I-LF-BENEFIT-AMT TO VD-LF-BENEFIT-AMT
100111        MOVE PB-I-LF-TERM        TO VD-LF-TERM
100111        MOVE PB-I-LF-RATE        TO VD-LF-RATE
100111        MOVE PB-I-AH-BENEFIT-CD  TO VD-AH-BENEFIT-CD
100111        MOVE PB-I-AH-PREMIUM-AMT TO VD-AH-PREMIUM-AMT
100412        MOVE PB-I-AH-BENEFIT-AMT TO VD-AH-BENEFIT-AMT
100111        MOVE PB-I-AH-TERM        TO VD-AH-TERM
100111        MOVE PB-I-AH-RATE        TO VD-AH-RATE
100111        MOVE PB-I-LOAN-TERM      TO VD-LOAN-TERM
100111        MOVE PB-I-LOAN-APR       TO VD-LOAN-APR
100111        MOVE PB-I-1ST-PMT-DT     TO VD-1ST-PMT-DT
030708     END-IF

100111     MOVE PB-CONTROL-PRIMARY     TO  ERPNDM-KEY.
100111
100111     EXEC CICS GETMAIN
100111        SET      (ADDRESS OF PENDING-MAILING-DATA)
100111        LENGTH   (ERPNDM-RECORD-LENGTH)
100111        INITIMG  (GETMAIN-SPACE)
100111        RESP     (WS-RESPONSE)
100111     END-EXEC
100111
100111     EXEC CICS READ
100111        INTO    (PENDING-MAILING-DATA)
100111        DATASET (FILE-ID-ERPNDM)
100111        RIDFLD  (ERPNDM-KEY)
100111        RESP    (WS-RESPONSE)
100111     END-EXEC
100111
100111     IF RESP-NORMAL
100111        MOVE PM-ADDRESS-LINE-1    TO VD-INSURED-ADDRESS-1
100111        MOVE PM-ADDRESS-LINE-2    TO VD-INSURED-ADDRESS-2
100111        MOVE PM-CITY              TO VD-INSURED-CITY
100111        MOVE PM-STATE             TO VD-INSURED-STATE
100111        MOVE PM-ZIP               TO VD-INSURED-ZIP-CODE
100111     END-IF
100111
100111     MOVE 'VAC1'                  TO VD-LETTER-ID
100111     MOVE PI-PROCESSOR-ID         TO VD-PROC-ID
100111     MOVE PI-COMPANY-ID           TO VD-COMP-ID
100111     MOVE EIBTIME                 TO VD-CURRENT-TIME         
100111     MOVE WS-CURRENT-BIN-DT       TO VD-CURRENT-DATE              
100111     MOVE +0                      TO VD-ARCHIVE-NO
100111     MOVE VIRGINIA-DISCLOSURE     TO WS-PASS-VADS-REC

030708     .
030708 6200-EXIT.
030708     EXIT.

       6210-READ-ERACCT.

           MOVE PB-CONTROL-BY-ACCOUNT  TO ERACCT-KEY
           MOVE LOW-VALUES             TO ERACCT-FILL

           EXEC CICS READ
                DATASET   ('ERACCT')
                INTO      (ACCOUNT-MASTER)
                RIDFLD    (ERACCT-KEY)
                GTEQ
                RESP      (WS-RESPONSE)
           END-EXEC

           .
       6210-EXIT.
           EXIT.
072312
072312 6220-READ-ERMAIL.
072312
072312     MOVE ELCERT-KEY             TO ERMAIL-KEY
072312     MOVE ' '                    TO WS-ERMAIL-SW
072312
072312     EXEC CICS READ
072312        DATASET   (FILE-ID-ERMAIL)
072312        SET       (ADDRESS OF MAILING-DATA)
072312        RIDFLD    (ERMAIL-KEY)
072312        RESP      (WS-RESPONSE)
072312     END-EXEC
072312
072312     IF RESP-NORMAL
072312        SET ERMAIL-FOUND TO TRUE
072312     END-IF
072312     .
072312 6220-EXIT.
072312     EXIT.
062712
062712 6400-CHECK-FOR-RES-REF-CLAIM.
           display 'made it to 6400- for ' pi-cert-no
062712     MOVE LOW-VALUES             TO PI-RES-REF-CLM-TYPE
062712
062712     MOVE PI-COMPANY-CD          TO MSTR-COMP-CD
062712                                    W-CL-COMP-CD.
062712     MOVE PI-CERT-NO             TO MSTR-CERT-NO
062712                                    W-CL-CERT-NO.
062712     MOVE SPACES                 TO MSTR-CERT-SFX.
062712
062712     EXEC CICS HANDLE CONDITION
062712         NOTFND  (6400-CONTINUE)
062712         ENDFILE (6400-CONTINUE)
062712     END-EXEC.
062712
062712     EXEC CICS IGNORE CONDITION
062712         DUPKEY
062712     END-EXEC.
062712
062712     EXEC CICS STARTBR
062712         DATASET   (CLMS-ID)
062712         RIDFLD    (ELMSTR-KEY)
062712         GENERIC   EQUAL
062712         KEYLENGTH (ELMSTR-LENGTH)
062712     END-EXEC.
062712
           display 'browse started'
           
062712     MOVE 'Y'                    TO WS-BROWSE-STARTED-SW.
062712
062712 6400-NEXT-CLAIM.
062712
062712     EXEC CICS READNEXT
062712         DATASET   (CLMS-ID)
062712         RIDFLD    (ELMSTR-KEY)
062712         SET       (ADDRESS OF CLAIM-MASTER)
062712     END-EXEC.
062712
           display 'read claim for ' mstr-cert-no
           
062712     IF MSTR-COMP-CD NOT = W-CL-COMP-CD  OR
062712        MSTR-CERT-PRIME NOT = W-CL-CERT-PRIME
062712         GO TO 6400-CONTINUE
062712     END-IF.
062712
062712     IF PI-CARRIER     NOT = CL-CARRIER        OR
062712        PI-GROUPING    NOT = CL-CERT-GROUPING  OR
062712        PI-STATE       NOT = CL-CERT-STATE     OR
062712        PI-ACCOUNT     NOT = CL-CERT-ACCOUNT   OR
062712        PI-CERT-EFF-DT NOT = CL-CERT-EFF-DT
062712         GO TO 6400-NEXT-CLAIM
062712     END-IF.
062712
           display 'found claim ' cl-claim-no
              ' with denial type ' cl-denial-type
              
062712     IF CL-DENIAL-TYPE = '2' OR '3' OR '4'
062712         MOVE CL-DENIAL-TYPE TO PI-RES-REF-CLM-TYPE
062712         GO TO 6400-CONTINUE
062712     END-IF.
062712
062712     GO TO 6400-NEXT-CLAIM.
062712
062712 6400-CONTINUE.
062712
062712     IF WS-BROWSE-STARTED-SW = 'Y'
062712         EXEC CICS ENDBR
062712             DATASET   (CLMS-ID)
062712         END-EXEC
062712         MOVE 'N' TO WS-BROWSE-STARTED-SW
062712     END-IF.
062712
062712 6400-EXIT.
062712     EXIT.           
062712
08643  7000-DISPLAY-ISSUES.                                             
08644                                                                   
08645      IF PI-DISPLAY-ORIGINAL-BATCH                                 
08646         MOVE PB-CONTROL-BY-ORIG-BATCH TO PI-PREV-KEY              
08647         MOVE ZEROS                    TO PI-PREV-CHG-SEQ-NO       
08648      ELSE                                                         
08649         MOVE PB-CONTROL-PRIMARY      TO PI-PREV-KEY.              
08650                                                                   
08651      MOVE PB-CONTROL-PRIMARY         TO PI-PREV-CONTROL-PRIMARY.  
08652                                                                   
08653      MOVE 'Y'                        TO WS-DISPLAY-SW.            
08654      MOVE 'Y'                        TO PI-DISPLAY-SCREEN-SW.     
08655                                                                   
08656      MOVE LOW-VALUES                 TO VP631BI.                  
08657      MOVE ZEROS                      TO PI-BPMTAMT                
08658                                         PI-BPMTS.                 
08659      MOVE SPACE                      TO PI-BMODE                  
08660                                         PI-BTYPE (1)              
08661                                         PI-BTYPE (2).             
08662      MOVE VP631B                     TO PI-MAP-NAME.              
08663      MOVE 'Y'                        TO PI-BROWSE-SW.             
08664      MOVE PB-BATCH-SEQ-NO            TO BSEQO                     
08665                                         PI-HIGH-SEQ-NO.           
08666                                                                   
08667      IF PB-BATCH-CHG-SEQ-NO NOT = ZEROS                           
08668         MOVE AL-SABOF                TO BCHGTA                    
08669         SUBTRACT PB-BATCH-CHG-SEQ-NO FROM 1000 GIVING BCHGO       
08670         MOVE AL-SANON                TO BCHGA                     
08671      ELSE                                                         
08672         MOVE AL-SADOF                TO BCHGTA                    
08673                                         BCHGA                     
08674         MOVE PB-CHG-COUNT            TO BCHGO.                    
08675                                                                   
08676      MOVE PB-LAST-MAINT-BY           TO BMANTBYO.                 
08677      MOVE PB-LAST-MAINT-HHMMSS       TO WS-TIME.                  
08678      MOVE WS-HR-MINS                 TO BMANTATO.                 
08679                                                                   
08680      IF PB-CREDIT-ACCEPT-DT NOT = LOW-VALUES                      
08681         MOVE PB-CREDIT-ACCEPT-DT     TO DC-BIN-DATE-1             
08682         MOVE SPACE                   TO DC-OPTION-CODE            
08683         PERFORM 9700-DATE-LINK                                    
08684         MOVE DC-GREG-DATE-1-EDIT     TO BMANTDTO                  
08685         MOVE 'INACTIVE'              TO BMOENDTO                  
08686      ELSE                                                         
08687         MOVE PB-LAST-MAINT-DT        TO DC-BIN-DATE-1             
08688         MOVE SPACE                   TO DC-OPTION-CODE            
08689         PERFORM 9700-DATE-LINK                                    
08690         MOVE DC-GREG-DATE-1-EDIT     TO BMANTDTO                  
08691         MOVE PB-CREDIT-SELECT-DT     TO DC-BIN-DATE-1             
08692         MOVE SPACE                   TO DC-OPTION-CODE            
08693         PERFORM 9700-DATE-LINK                                    
08694         MOVE DC-GREG-DATE-1-EDIT     TO BMOENDTO.                 
08695                                                                   
011904*    MOVE PB-I-REFERENCE             TO BREFO.                    
08697                                                                   
08698 *    IF PI-COMPANY-ID EQUAL 'HER'                                 
08699 *        MOVE PB-I-MEMBER-NO     TO  WS-MEMBER-NO                 
08700 *        IF WS-MEMBER-NO-1-8  IS NUMERIC                          
08701 *            IF WS-MEMBER-NO-1-8  GREATER  ZEROS                  
08702 *                MOVE WS-MEMBER-NO-1-8                            
08703 *                                TO  BMICRNOO.                    
08704                                                                   
032210*    IF PB-I-MICROFILM-NO  IS NUMERIC                             
032210*        IF PB-I-MICROFILM-NO  GREATER  ZEROS                     
032210*            MOVE PB-I-MICROFILM-NO                               
032210*                                TO  BMICRNOO.                    

101706     IF PB-I-VIN NOT = SPACES
101706        MOVE PB-I-VIN            TO BVINO
101706     END-IF
08709                                                                   
08710      MOVE PB-CERT-PRIME              TO BCERTNOO.                 
08711      MOVE PB-CERT-SFX                TO BSUFIXO.                  
08712      MOVE PB-CERT-EFF-DT             TO DC-BIN-DATE-1.            
08713      MOVE SPACE                      TO DC-OPTION-CODE.           
08714      PERFORM 9700-DATE-LINK.                                      
08715      MOVE DC-GREG-DATE-1-EDIT        TO BEFFDTO                   
08716                                                                   
08717      MOVE PB-I-INSURED-LAST-NAME     TO BLSTNMO.                  
08718      MOVE PB-I-INSURED-FIRST-NAME    TO B1STNMO.                  
08719      MOVE PB-I-INSURED-MIDDLE-INIT   TO BINITO.                   
08720      MOVE PB-I-INSURED-SEX           TO BSEXO                     
08721                                                                   
08722      IF PB-I-AGE NOT = ZEROS                                      
08723          MOVE PB-I-AGE               TO BAGEO.                    
121712
121712     MOVE PB-CONTROL-BY-ACCOUNT (1:33)
121712                                 TO ELCERT-KEY
121712     PERFORM 1295-READ-CERT-TRAILER THRU 1295-EXIT
121712     IF NOT CERT-TRL-REC-NOT-FOUND
121712         IF CS-INS-AGE-DEFAULT-FLAG = 'Y'
121712             MOVE 'AGE*'             TO BAGEDEFO
121712         ELSE
121712             MOVE 'AGE '             TO BAGEDEFO
121712         END-IF
               if cs-year numeric
                  move cs-year         to byearo
               end-if
               move cs-make            to bmakeo
               move cs-model           to bmodelo
               move cs-future          to bfutureo
               if cs-vehicle-odometer numeric
                  move cs-vehicle-odometer
                                       to bometero
               end-if
121712     ELSE
121712         MOVE 'AGE '             TO BAGEDEFO
121712     END-IF
08724                                                                   
08725      IF PB-I-BIRTHDAY NOT = LOW-VALUES                            
08726          MOVE PB-I-BIRTHDAY          TO DC-BIN-DATE-1             
08727          MOVE SPACE                  TO DC-OPTION-CODE            
08728          PERFORM 9700-DATE-LINK                                   
08729          IF NO-CONVERSION-ERROR                                   
08730             MOVE DC-GREG-DATE-1-EDIT TO BBIRTHO.                  
08731                                                                   
08738                                                                   
08746                                                                   
08749                                                                   
08750      MOVE PB-FORCE-CODE              TO BFORCEO.                  
08753                                                                   
08756                                                                   
08760                                                                   
08765                                                                   
08766      MOVE PB-I-LOAN-OFFICER          TO BLNOFCRO.                 
08767      MOVE PB-BATCH-ENTRY             TO BENTRYO.                  
08768      MOVE PB-I-UNDERWRITING-STATUS   TO BUNWRITO.                 
08769      MOVE PB-I-SKIP-CODE             TO BSKPCDO.                  
08770      MOVE PB-I-INDV-GRP-CD           TO BINDGRPO.                 
08771      MOVE PB-I-SIG-SW                TO BSIGO.                    
08772      MOVE PB-I-RATE-CLASS-OVRD       TO BRTCLSO.                  
08773      MOVE PB-I-SPECIAL-REIN-CODE     TO BREINO.                   
08774      MOVE PB-RECORD-BILL             TO BBILCDO.                  
08775      MOVE PB-FORCE-CODE              TO BFORCEO.                  
08776                                                                   
08782                                                                   
08788                                                                   
08789      MOVE PB-CHG-COUNT               TO BCHGCNTO.                 
08790                                                                   
08793                                                                   
08794      IF PB-I-MEMBER-NO  GREATER THAN SPACES                       
08795         MOVE PB-I-MEMBER-NO          TO BMEMBERO.                 
08796                                                                   
08802                                                                   
08803      MOVE PI-LIFE-OVERRIDE-L2        TO BKIND (1).                
08804                                                                   
08805      IF PB-I-LF-CRIT-PER GREATER THAN ZEROS                       
08806         MOVE PB-I-LF-CRIT-PER        TO BCRIT-PERDO (1).          
08807                                                                   
08808      IF PB-I-LF-INPUT-CD GREATER THAN SPACES                      
08809         MOVE PB-I-LF-INPUT-CD        TO BTYPE (1)                 
08810                                         PI-BTYPE (1).             
08811                                                                   
08812      IF PB-I-LF-TERM     GREATER THAN ZEROS                       
08813         MOVE PB-I-LF-TERM            TO BTERMO (1).               
08814                                                                   
08815      IF PB-I-LF-BENEFIT-AMT GREATER THAN ZEROS                    
08816         MOVE PB-I-LF-BENEFIT-AMT     TO BBENO  (1).               
08817                                                                   
08818      IF PB-I-LF-ALT-BENEFIT-AMT GREATER THAN ZEROS                
08819         MOVE PB-I-LF-ALT-BENEFIT-AMT TO BALT-BENO   (1).          
08820                                                                   
08821      IF PB-I-LF-PREMIUM-AMT GREATER THAN ZEROS                    
08822         MOVE PB-I-LF-PREMIUM-AMT     TO BPREMO      (1).          
08823                                                                   
08824      IF PB-I-LF-ALT-PREMIUM-AMT GREATER THAN ZEROS                
08825         MOVE PB-I-LF-ALT-PREMIUM-AMT TO BALT-PREMO  (1).          
08826                                                                   
08827      IF PB-I-LF-EXPIRE-DT NOT = LOW-VALUES                        
08828          MOVE PB-I-LF-EXPIRE-DT      TO DC-BIN-DATE-1             
08829          MOVE SPACE                  TO DC-OPTION-CODE            
08830          PERFORM 9700-DATE-LINK                                   
08831          IF NO-CONVERSION-ERROR                                   
08832             MOVE DC-GREG-DATE-1-EDIT TO BEXPIRE   (1).            
08833                                                                   
08834      IF PB-I-LF-BENEFIT-CD NOT = ZEROS                            
08835         MOVE PB-I-LF-BENEFIT-CD      TO BBEN-CD     (1)           
08836         MOVE PB-I-LF-ABBR            TO BBEN-ABBR   (1)           
08837         MOVE PB-I-LF-PREM-CALC       TO BCPREMO     (1)           
08838         SUBTRACT PB-I-LF-PREMIUM-AMT FROM PB-I-LF-PREM-CALC       
08839                  GIVING BDIFFO (1)                                
08840      ELSE                                                         
08841         MOVE SPACES                  TO BBEN-CD     (1)           
08842                                         BBEN-ABBR   (1).          
08843                                                                   
08844      IF PB-I-LF-ALT-PREMIUM-AMT GREATER THAN ZEROS                
08845         MOVE PB-I-LF-ALT-PREM-CALC   TO BCALT-PREM  (1)           
08846         SUBTRACT PB-I-LF-ALT-PREMIUM-AMT FROM                     
08847                  PB-I-LF-ALT-PREM-CALC   GIVING BDALT-PREM (1).   
08848                                                                   
08849      IF PB-I-LF-BENEFIT-CD NOT = ZEROS                            
08850        IF PB-RECORD-ON-HOLD                                       
08851           MOVE 'ON HOLD'           TO BBILL-LIT     (1).          
08852                                                                   
08853      IF PB-I-LF-BENEFIT-CD NOT = ZEROS                            
08854        IF PB-RECORD-RETURNED                                      
08855           MOVE 'RETURNED'          TO BBILL-LIT     (1).          
08856                                                                   
08857      IF PB-I-LF-BENEFIT-CD NOT = ZEROS                            
08858         AND (PB-OVERRIDE-LIFE OR PB-OVERRIDE-BOTH)                
08859              MOVE 'OVERRIDE'          TO BBILL-LIT  (1).          
08860                                                                   
08861      MOVE PI-AH-OVERRIDE-L2          TO BKIND       (2).          
08862                                                                   
08863      IF PB-I-AH-CRIT-PER GREATER THAN ZEROS                       
08864         MOVE PB-I-AH-CRIT-PER        TO BCRIT-PERDO (2).          
08865                                                                   
08866      IF PB-I-AH-INPUT-CD GREATER THAN SPACES                      
08867         MOVE PB-I-AH-INPUT-CD        TO BTYPE       (2)           
08868                                         PI-BTYPE (2).             
08869                                                                   
08870      IF PB-I-AH-TERM     GREATER THAN ZEROS                       
08871         MOVE PB-I-AH-TERM            TO BTERMO      (2).          
08872                                                                   
08873      IF PB-I-AH-BENEFIT-AMT GREATER THAN ZEROS                    
08874         MOVE PB-I-AH-BENEFIT-AMT     TO BBENO       (2).          
08875                                                                   
08876      IF PB-I-AH-PREMIUM-AMT GREATER THAN ZEROS                    
08877         MOVE PB-I-AH-PREMIUM-AMT     TO BPREMO      (2).          
08878                                                                   
           IF PB-I-TOT-FEES NOT NUMERIC
              MOVE ZEROS               TO PB-I-TOT-FEES
           END-IF
           
011904     IF PB-I-TOT-FEES > ZEROS
011904        MOVE PB-I-TOT-FEES       TO BALT-PREMO  (2).          
011904                                                                  
08879      IF PB-I-AH-EXPIRE-DT NOT = LOW-VALUES                        
08880          MOVE PB-I-AH-EXPIRE-DT      TO DC-BIN-DATE-1             
08881          MOVE SPACE                  TO DC-OPTION-CODE            
08882          PERFORM 9700-DATE-LINK                                   
08883          IF NO-CONVERSION-ERROR                                   
08884             MOVE DC-GREG-DATE-1-EDIT TO BEXPIRE       (2).        
08885                                                                   
011904     IF PB-I-TOT-FEES > ZEROS
011904        MOVE PB-I-TOT-FEES-CALC      TO BCALT-PREM  (2)           
011904        SUBTRACT PB-I-TOT-FEES FROM                     
011904                 PB-I-TOT-FEES-CALC GIVING BDALT-PREM (2).   
011904                                                                  
08886      IF PB-I-AH-BENEFIT-CD NOT = ZEROS                            
08887         MOVE PB-I-AH-BENEFIT-CD      TO BBEN-CD     (2)           
08888         MOVE PB-I-AH-ABBR            TO BBEN-ABBR   (2)           
08889         MOVE PB-I-AH-PREM-CALC       TO BCPREMO     (2)           
08890         SUBTRACT PB-I-AH-PREMIUM-AMT FROM PB-I-AH-PREM-CALC       
08891                  GIVING BDIFFO (2)                                
08892      ELSE                                                         
08893         MOVE SPACES                  TO BBEN-CD     (2)           
08894                                         BBEN-ABBR   (2).          
08895                                                                   
08896      IF PB-I-AH-BENEFIT-CD NOT = ZEROS                            
08897        IF PB-RECORD-ON-HOLD                                       
08898           MOVE 'ON HOLD'           TO BBILL-LIT     (2).          
08899                                                                   
08900      IF PB-I-AH-BENEFIT-CD NOT = ZEROS                            
08901        IF PB-RECORD-RETURNED                                      
08902           MOVE 'RETURNED'          TO BBILL-LIT     (2).          
08903                                                                   
08904      IF PB-I-AH-BENEFIT-CD NOT = ZEROS                            
08905         AND (PB-OVERRIDE-AH OR PB-OVERRIDE-BOTH)                  
08906              MOVE 'OVERRIDE'          TO BBILL-LIT  (2).          
08907                                                                   
08908      MOVE PB-CSR-ID                  TO BCSRO.                    
08909                                                                   
08910      IF PI-COMPANY-ID  =  'DMD'                                   
08911         IF (EC-NEW-CERT = SPACES OR LOW-VALUES)                   
08912             NEXT SENTENCE                                         
08913         ELSE                                                      
08914             IF (PB-CERT-NO  NOT =  EC-NEW-CERT)                   
08915                MOVE 'NEW CERT'       TO BNCERTLI                  
08916                MOVE EC-NEW-CERT      TO BNCERTO.                  
08917                                                                   
08918      IF PI-COMPANY-ID  =  'DMD'                                   
08919         IF (EC-NEW-STATE = LOW-VALUES OR SPACES)                  
08920             NEXT SENTENCE                                         
08921         ELSE                                                      
08922            IF (PB-SV-STATE  NOT =  EC-NEW-STATE)                  
08923                MOVE 'NEW STATE'      TO BNSTATLI                  
08924                MOVE EC-NEW-STATE     TO BNSTATO.                  
08925                                                                   
08926  7000-CONTINUE.                                                   
08927      MOVE 'C'                        TO BMAINTI.                  
08928      MOVE AL-UANON                   TO BMAINTA.                  
08929                                                                   
08930      IF PB-BATCH-CHG-SEQ-NO = ZEROS                               
08931         PERFORM 7300-FORMAT-ERRORS THRU 7399-EXIT.                
072312
072312     IF PI-6315-FINALIZED = 'Y'   AND 
072312        PB-UNFORCED-ERRORS
072312          MOVE -1                TO BFORCEL
072312          MOVE  1                TO SET-CURSOR-SW
072312     END-IF
08932                                                                   
08933      IF NO-CURSOR-SET                                             
08934         MOVE -1                      TO BMAINTL.                  
08935                                                                   
08936      GO TO 8100-SEND-INITIAL-MAP.                                 
08937                                                                   
08938      EJECT                                                        
08939                                                                   
08940 ******************************************************************
08941 *                                                                *
08942 *             D I S P L A Y   C A N C E L S                      *
08943 *                                                                *
08944 ******************************************************************
08945                                                                   
08946  7100-DISPLAY-CANCELS.                                            
08947      IF PI-DISPLAY-ORIGINAL-BATCH                                 
08948         MOVE PB-CONTROL-BY-ORIG-BATCH TO PI-PREV-KEY              
08949         MOVE ZEROS                    TO PI-PREV-CHG-SEQ-NO       
08950      ELSE                                                         
08951         MOVE PB-CONTROL-PRIMARY      TO PI-PREV-KEY.              
08952                                                                   
08953      MOVE PB-CONTROL-PRIMARY         TO PI-PREV-CONTROL-PRIMARY.  
08954                                                                   
08955      MOVE 'Y'                        TO WS-DISPLAY-SW.            
08956      MOVE 'Y'                        TO PI-DISPLAY-SCREEN-SW.     
08957                                                                   
08958      MOVE LOW-VALUES                 TO VP631CI.                  
08959      MOVE VP631C                     TO PI-MAP-NAME.              
08960      MOVE 'Y'                        TO PI-BROWSE-SW.             
08961      MOVE PB-BATCH-SEQ-NO            TO CSEQO                     
08962                                         PI-HIGH-SEQ-NO.           
08963                                                                   
08964      IF PB-BATCH-CHG-SEQ-NO NOT = ZEROS                           
08965         MOVE AL-SABOF                TO CCHGTA                    
08966         SUBTRACT PB-BATCH-CHG-SEQ-NO FROM 1000 GIVING CCHGO       
08967         MOVE AL-SANON                TO CCHGA                     
08968       ELSE                                                        
08969         MOVE AL-SADOF                TO CCHGTA                    
08970                                         CCHGA                     
08971         MOVE PB-CHG-COUNT            TO CCHGCNTO.                 
08972                                                                   
011904*    MOVE PB-C-REFERENCE             TO CREFO.                    
08974                                                                   
072908*    IF PB-C-MICROFILM-NO IS NUMERIC
072908*       IF PB-C-MICROFILM-NO  NOT = ZEROS
072908*          MOVE PB-C-MICROFILM-NO
072908*                                TO CMICRNOO
072908*          MOVE AL-UNNON         TO CMICRNOA
072908*       END-IF
072908*    END-IF

062017     IF PB-C-INT-ON-REFS NUMERIC
062017        IF PB-C-INT-ON-REFS NOT = ZEROS
062017           MOVE PB-C-INT-ON-REFS
072908                                 TO CNHINTO
072908           MOVE AL-SANOF         TO CNHINTA
072908        END-IF
072908     END-IF

08981      MOVE PB-CERT-PRIME              TO CCERTNOO.                 
08982      MOVE PB-CERT-SFX                TO CSUFIXO.                  
08983      MOVE PB-CERT-EFF-DT             TO DC-BIN-DATE-1.            
08984      MOVE SPACE                      TO DC-OPTION-CODE.           
08985      PERFORM 9700-DATE-LINK.                                      
08986      MOVE DC-GREG-DATE-1-EDIT        TO CEFFDTO.                  
08987                                                                   
08988      IF PB-C-LF-CANCEL-DT NOT = LOW-VALUES                        
08989         MOVE PB-C-LF-CANCEL-DT       TO DC-BIN-DATE-1             
08990         MOVE SPACE                   TO DC-OPTION-CODE            
08991         PERFORM 9700-DATE-LINK                                    
08992         IF NO-CONVERSION-ERROR                                    
08993            MOVE DC-GREG-DATE-1-EDIT  TO CCANDT1O.                 
08994                                                                   
08995      IF PB-C-LF-REFUND-OVERRIDE EQUAL SPACES                      
08996         NEXT SENTENCE                                             
08997      ELSE                                                         
08998         MOVE PB-C-LF-REFUND-OVERRIDE TO CMTHD1I.                  
08999                                                                   
09000      IF PB-C-AH-REFUND-OVERRIDE EQUAL SPACES                      
09001         NEXT SENTENCE                                             
09002      ELSE                                                         
09003         MOVE PB-C-AH-REFUND-OVERRIDE TO CMTHD2I.                  
09004                                                                   
09005      IF PB-C-AH-CANCEL-DT   NOT = LOW-VALUES                      
09006         MOVE PB-C-AH-CANCEL-DT       TO DC-BIN-DATE-1             
09007         MOVE SPACE                   TO DC-OPTION-CODE            
09008         PERFORM 9700-DATE-LINK                                    
09009         IF NO-CONVERSION-ERROR                                    
09010            MOVE DC-GREG-DATE-1-EDIT  TO CCANDT2O.                 
09011                                                                   
09012      IF PB-C-LF-CANCEL-AMT NOT = ZEROS                            
09013          MOVE PB-C-LF-CANCEL-AMT     TO CREFND1O.                 
09014                                                                   
09015      IF PB-C-AH-CANCEL-AMT NOT = ZEROS                            
09016          MOVE PB-C-AH-CANCEL-AMT     TO CREFND2O.                 
09017                                                                   
09018      IF PB-C-LF-CANCEL-DT NOT = LOW-VALUES                        
09019         MOVE PB-C-LF-REF-CALC        TO CCRFND1O                  
09020         SUBTRACT PB-C-LF-CANCEL-AMT FROM PB-C-LF-REF-CALC         
09021                  GIVING CDRFND1O.                                 
09022                                                                   
09023      IF PB-C-AH-CANCEL-DT NOT = LOW-VALUES                        
09024         MOVE PB-C-AH-REF-CALC        TO CCRFND2O                  
09025         SUBTRACT PB-C-AH-CANCEL-AMT FROM PB-C-AH-REF-CALC         
09026                  GIVING CDRFND2O.                                 
09027                                                                   
09028      IF PB-C-LF-CANCEL-AMT NOT = ZEROS                            
09029        IF PB-RECORD-ON-HOLD                                       
09030           MOVE 'ON HOLD'             TO CBILIT1O.                 
09031                                                                   
09032      IF PB-C-LF-CANCEL-AMT NOT = ZEROS                            
09033        IF PB-RECORD-RETURNED                                      
09034           MOVE 'RETURNED'            TO CBILIT1O.                 
09035                                                                   
09036      IF PB-C-LF-CANCEL-AMT NOT = ZEROS                            
09037         AND (PB-OVERRIDE-LIFE OR PB-OVERRIDE-BOTH)                
09038              MOVE 'OVERRIDE'         TO CBILIT1O.                 
09039                                                                   
09040      IF PB-C-AH-CANCEL-AMT NOT = ZEROS                            
09041        IF PB-RECORD-ON-HOLD                                       
09042           MOVE 'ON HOLD'             TO CBILIT2O.                 
09043                                                                   
09044      IF PB-C-AH-CANCEL-AMT NOT = ZEROS                            
09045        IF PB-RECORD-RETURNED                                      
09046           MOVE 'RETURNED'            TO CBILIT2O.                 
09047                                                                   
09048      IF PB-C-AH-CANCEL-AMT NOT = ZEROS                            
09049         AND (PB-OVERRIDE-AH OR PB-OVERRIDE-BOTH)                  
09050              MOVE 'OVERRIDE'         TO CBILIT2O.                 
09051                                                                   
09052      MOVE PB-C-LAST-NAME             TO CLSTNMO.                  

           MOVE PB-C-CANCEL-REASON         TO CCANREAO

09053      MOVE PB-FORCE-CODE              TO CFORCEO.                  
09054      MOVE PB-RECORD-BILL             TO CBILCDO.                  
09055      MOVE PB-CI-LAST-NAME            TO CINAMEO.  
072209     MOVE SPACES                     TO CINITSO.
072209     IF PB-CI-FIRST-NAME GREATER THAN SPACES
072209         MOVE PB-CI-FIRST-NAME       TO CINITSO
072209     ELSE                
072209         MOVE PB-CI-INITIALS         TO CINITSO
07209      END-IF. 
09057                                                                   
09058      IF PB-CI-CANCEL-FEE NUMERIC                                  
09059         MOVE PB-CI-CANCEL-FEE        TO W-CANCEL-FEE-LONG         
09060         MOVE W-CANCEL-FEE-LONG       TO W-CANCEL-FEE              
09061      ELSE                                                         
09062         MOVE ZEROES                  TO W-CANCEL-FEE.             
09063                                                                   
09064      MOVE W-CANCEL-FEE               TO CCANFEEO.                 
09065                                                                   
09066      IF PB-CI-LF-PREMIUM-AMT NOT = ZEROS                          
09067         ADD PB-CI-LF-PREMIUM-AMT                                  
09068              PB-CI-LF-ALT-PREMIUM-AMT  GIVING  CPREM1O.           
09069                                                                   
09070      IF PB-CI-AH-PREMIUM-AMT NOT = ZEROS                          
09071         MOVE PB-CI-AH-PREMIUM-AMT       TO CPREM2O.               
09072                                                                   
09073      MOVE PB-CI-LF-TERM              TO CTERM1O.                  
09074      MOVE PB-C-LF-REM-TERM           TO CRTERM1O.                 
09075      MOVE PB-CI-AH-TERM              TO CTERM2O.                  
09076      MOVE PB-C-AH-REM-TERM           TO CRTERM2O.                 
09077                                                                   
09078      IF PB-C-LIVES NUMERIC                                        
09079         MOVE PB-C-LIVES              TO CLIVESO.                  
09080                                                                   
09081      IF PB-C-PAYEE-CODE GREATER THAN SPACES                       
09082         MOVE PB-C-PAYEE-CODE         TO CPAYEEO.                  
09083                                                                   
09084      IF PB-CI-ENTRY-DT NOT = LOW-VALUES                           
09085         MOVE PB-CI-ENTRY-DT          TO DC-BIN-DATE-1             
09086         MOVE SPACE                   TO DC-OPTION-CODE            
09087         PERFORM 9700-DATE-LINK                                    
09088         IF NO-CONVERSION-ERROR                                    
09089            MOVE DC-GREG-DATE-1-EDIT  TO CENTRDTO.                 
09090                                                                   
09091      IF  PB-C-REFUND-CREATED                                      
09092          MOVE 'YES'                  TO CRFNDSWO                  
09093      ELSE                                                         
09094          IF PB-C-REFUND-REQUESTED                                 
09095             MOVE 'REQ'               TO CRFNDSWO                  
09096          ELSE                                                     
09097             MOVE 'NO'                TO CRFNDSWO.                 
09098                                                                   
09099      MOVE PB-INPUT-DT                TO DC-BIN-DATE-1.            
09100      MOVE SPACE                      TO DC-OPTION-CODE.           
09101      PERFORM 9700-DATE-LINK.                                      
09102      MOVE DC-GREG-DATE-1-EDIT        TO CENTDTO.                  
09103      MOVE PB-INPUT-BY                TO CENTBYO.                  
09104                                                                   
09105      IF PB-CREDIT-ACCEPT-DT NOT = LOW-VALUES                      
09106         MOVE PB-CREDIT-ACCEPT-DT     TO DC-BIN-DATE-1             
09107         MOVE SPACE                   TO DC-OPTION-CODE            
09108         PERFORM 9700-DATE-LINK                                    
09109         MOVE DC-GREG-DATE-1-EDIT     TO CMANTDTO                  
09110         MOVE 'INACTIVE'              TO CMOENDTO                  
09111      ELSE                                                         
09112         MOVE PB-LAST-MAINT-DT        TO DC-BIN-DATE-1             
09113         MOVE SPACE                   TO DC-OPTION-CODE            
09114         PERFORM 9700-DATE-LINK                                    
09115         MOVE DC-GREG-DATE-1-EDIT     TO CMANTDTO                  
09116         MOVE PB-CREDIT-SELECT-DT     TO DC-BIN-DATE-1             
09117         MOVE SPACE                   TO DC-OPTION-CODE            
09118         PERFORM 9700-DATE-LINK                                    
09119         MOVE DC-GREG-DATE-1-EDIT     TO CMOENDTO.                 
09120                                                                   
09121      MOVE PB-LAST-MAINT-BY           TO CMANTBYO.                 
09122                                                                   
09123      MOVE PB-LAST-MAINT-HHMMSS       TO WS-TIME.                  
09124      MOVE WS-HR-MINS                 TO CMANTATO.                 
09125                                                                   
09126      EVALUATE PB-LF-REFUND-TYPE                                   
09127         WHEN '1'                                                  
09128            MOVE 'RULE78'            TO CLFREFNO                   
09129         WHEN '2'                                                  
09130            MOVE 'PRORAT'            TO CLFREFNO                   
09131         WHEN '3'                                                  
09132            MOVE 'CALIF'             TO CLFREFNO                   
09133         WHEN '4'                                                  
09134            MOVE 'IRREG'             TO CLFREFNO                   
09135         WHEN '5'                                                  
09136            MOVE 'NETPAY'            TO CLFREFNO                   
09137         WHEN '6'                                                  
09138            MOVE 'ATCP'              TO CLFREFNO                   
09139         WHEN '7'                                                  
09140            MOVE 'UTAH'              TO CLFREFNO                   
09141         WHEN '8'                                                  
09142            MOVE 'MEAN'              TO CLFREFNO                   
09143         WHEN '9'                                                  
09144            MOVE 'SUM '              TO CLFREFNO                   
09145         WHEN 'B'                                                  
09146            MOVE 'BALN'              TO CLFREFNO                   
09147      END-EVALUATE.                                                
09148                                                                   
09149      EVALUATE TRUE                                                
09150         WHEN PB-AH-REFUND-TYPE =  '1'                             
09151            MOVE 'RULE78'            TO CAHREFNO                   
09152         WHEN PB-AH-REFUND-TYPE =  '2'                             
09153            MOVE 'PRORAT'            TO CAHREFNO                   
09154         WHEN PB-AH-REFUND-TYPE =  ('3' OR 'S')                    
09155            MOVE 'CALIF'             TO CAHREFNO                   
09156         WHEN PB-AH-REFUND-TYPE =  '4'                             
09157            MOVE 'IRREG'             TO CAHREFNO                   
09158         WHEN PB-AH-REFUND-TYPE =  '5'                             
09159            MOVE 'NETPAY'            TO CAHREFNO                   
09160         WHEN PB-AH-REFUND-TYPE =  '6'                             
09161            MOVE 'ATCP'              TO CAHREFNO                   
09162         WHEN PB-AH-REFUND-TYPE =  '7'                             
09163            MOVE 'UTAH'              TO CAHREFNO                   
09164         WHEN PB-AH-REFUND-TYPE =  '8'                             
09165            MOVE 'MEAN'              TO CAHREFNO                   
09166         WHEN PB-AH-REFUND-TYPE =  '9'                             
09167            MOVE 'SUM '              TO CAHREFNO                   
09168         WHEN PB-AH-REFUND-TYPE =  'B'                             
09169            MOVE 'BALN'              TO CAHREFNO                   
09170      END-EVALUATE.                                                
09171                                                                   
09172      MOVE PB-REIN-CD                 TO CREINCDO.                 
09173                                                                   
09174      IF PB-ACCT-EFF-DT NOT = LOW-VALUES AND SPACES                
09175         MOVE PB-ACCT-EFF-DT          TO DC-BIN-DATE-1             
09176         MOVE SPACE                   TO DC-OPTION-CODE            
09177         PERFORM 9700-DATE-LINK                                    
09178         IF NO-CONVERSION-ERROR                                    
09179            MOVE DC-GREG-DATE-1-EDIT  TO CAEFFDTO.                 
09180                                                                   
09181      IF PB-ACCT-EXP-DT  = HIGH-VALUES                             
09182         MOVE 999999                  TO WS-EXP-DT-EDIT            
09183         INSPECT WS-EXP-DT-EDIT CONVERTING SPACES TO '/'           
09184         MOVE WS-EXP-DT-EDIT          TO CAEXPDTO                  
09185      ELSE                                                         
09186         IF PB-ACCT-EXP-DT NOT = LOW-VALUES                        
09187            MOVE PB-ACCT-EXP-DT          TO DC-BIN-DATE-1          
09188            MOVE SPACE                   TO DC-OPTION-CODE         
09189            PERFORM 9700-DATE-LINK                                 
09190            IF NO-CONVERSION-ERROR                                 
09191               MOVE DC-GREG-DATE-1-EDIT  TO CAEXPDTO.              
09192                                                                   
09193      IF PB-ORIGINAL-ENTRY-BATCH NOT = SPACES                      
09194         IF PB-ORIGINAL-ENTRY-BATCH NOT = PB-ENTRY-BATCH           
09195            MOVE AL-SANOF                TO CORIGHDA               
09196            MOVE PB-ORIGINAL-ENTRY-BATCH TO CORGNOO                
09197            MOVE PB-ORIGINAL-SEQ-NO      TO CORGSEQO.              
09198                                                                   
09199      MOVE PB-CSR-ID              TO CCSRO.                        
09200                                                                   
09201      MOVE PB-CI-LIFE-COMMISSION  TO COMMLFO.                      
09202      MOVE PB-CI-AH-COMMISSION    TO COMMAHO.                      
09203                                                                   
09204      MOVE 'C'                    TO CMAINTI.                      
09205      MOVE AL-UANON               TO CMAINTA.                      
072312
072312     IF PI-6316-FINALIZED = 'Y'  AND 
072312        PB-UNFORCED-ERRORS
072312          MOVE -1                TO CFORCEL
072312          MOVE  1                TO SET-CURSOR-SW
072312     END-IF
09206                                                                   
09207      IF PB-BATCH-CHG-SEQ-NO = ZEROS                               
09208         PERFORM 7300-FORMAT-ERRORS THRU 7399-EXIT.                
09209                                                                   
09210      IF NO-CURSOR-SET                                             
09211         MOVE -1                  TO CMAINTL.                      
091712
091712     GO TO 8100-SEND-INITIAL-MAP.
09212                                                                   
09214                                                                   
09215      EJECT                                                        
09216 ***************************************************************   
09217 *     ROUTINE THAT MOVES ALL DATA FROM THE BATCH TOTAL        *   
09218 *     RECORD TO THE BATCH TOTAL SCREEN.                       *   
09219 ***************************************************************   
09220                                                                   
09221  7200-DISPLAY-BATCHES.                                            
09222      MOVE VP631D                 TO PI-MAP-NAME.                  
09223      MOVE LOW-VALUES             TO VP631DI.                      
09224                                                                   
09225      MOVE 'Y'                    TO WS-DISPLAY-SW.                
09226                                                                   
09227      EXEC CICS HANDLE CONDITION                                   
09228           NOTFND   (3690-NOT-FOUND)                               
09229      END-EXEC.                                                    
09230                                                                   
09231      IF PI-DISPLAY-SCREEN                                         
09232         MOVE PI-PREV-KEY         TO ERPNDB-KEY                    
09233      ELSE                                                         
09234         MOVE PB-CONTROL-PRIMARY  TO ERPNDB-KEY.                   
09235                                                                   
09236      MOVE +9999                  TO ERPNDB-BATCH-SEQ-NO.          
09237      MOVE +0                     TO ERPNDB-BATCH-CHG-SEQ-NO.      
09238                                                                   
09239      EXEC CICS READ                                               
09240           DATASET   (FILE-ID-ERPNDB)                              
09241           RIDFLD    (ERPNDB-KEY)                                  
09242           SET       (ADDRESS OF PENDING-BUSINESS)                 
09243      END-EXEC.                                                    
09244                                                                   
09245      MOVE PB-CONTROL-BY-ACCOUNT  TO PI-PREV-ALT-KEY.              
09246      MOVE PB-CONTROL-PRIMARY     TO PI-PREV-KEY.                  
09247      MOVE PB-SV-CARRIER          TO PI-SV-CARRIER.                
09248      MOVE PB-SV-GROUPING         TO PI-SV-GROUPING.               
09249      MOVE PB-SV-STATE            TO PI-SV-STATE.                  
09250                                                                   
09251      MOVE VP631D                 TO PI-MAP-NAME.                  
09252      MOVE LOW-VALUES             TO VP631DI.                      
09253      MOVE PB-ENTRY-BATCH         TO DBATCHO.                      
09254                                                                   
011904*    IF PI-AR-PROCESSING                                          
011904*       MOVE PB-REFERENCE        TO DREFO.                        
09257                                                                   
09258      MOVE PB-CSR-ID              TO DCSRO.                        
09259                                                                   
09260      IF PB-CREDIT-ACCEPT-DT NOT = LOW-VALUES                      
09261         MOVE PB-CREDIT-ACCEPT-DT TO DC-BIN-DATE-1                 
09262         MOVE SPACE               TO DC-OPTION-CODE                
09263         PERFORM 9700-DATE-LINK                                    
09264         MOVE DC-GREG-DATE-1-EDIT TO DMAINDTO                      
09265         MOVE AL-SABOF            TO DLDATEA  DMAINDTA             
09266         MOVE 'INACTIVE'          TO DLDATEO                       
09267      ELSE                                                         
09268         MOVE PB-LAST-MAINT-DT    TO DC-BIN-DATE-1                 
09269         MOVE SPACE               TO DC-OPTION-CODE                
09270         PERFORM 9700-DATE-LINK                                    
09271         MOVE DC-GREG-DATE-1-EDIT TO DMAINDTO.                     
09272                                                                   
09273      MOVE PB-INPUT-DT            TO DC-BIN-DATE-1.                
09274      MOVE SPACE                  TO DC-OPTION-CODE.               
09275      PERFORM 9700-DATE-LINK.                                      
09276      MOVE DC-GREG-DATE-1-EDIT    TO DINPUTO.                      
09277                                                                   
09278      IF PB-B-RECEIVED-DT = SPACES OR LOW-VALUES                   
09279         MOVE DC-GREG-DATE-1-EDIT TO DRECVDTO                      
09280      ELSE                                                         
09281         MOVE PB-B-RECEIVED-DT    TO DC-BIN-DATE-1                 
09282         MOVE SPACE               TO DC-OPTION-CODE                
09283         PERFORM 9700-DATE-LINK                                    
09284         MOVE DC-GREG-DATE-1-EDIT TO DRECVDTO.                     
09285                                                                   
09286      MOVE PB-CREDIT-SELECT-DT    TO DC-BIN-DATE-1.                
09287      MOVE SPACE                  TO DC-OPTION-CODE.               
09288      PERFORM 9700-DATE-LINK.                                      
09289      MOVE DC-GREG-DATE-1-EDIT        TO DPROCDTO.                 
09290                                                                   
09291      MOVE PB-B-ISSUE-CNT-REMITTED    TO DRISSO.                   
09292      MOVE PB-B-ISSUE-CNT-ENTERED     TO DEISSO.                   
09293                                                                   
09294      IF PB-B-ISSUE-CNT-REMITTED NOT = PB-B-ISSUE-CNT-ENTERED      
09295         SUBTRACT PB-B-ISSUE-CNT-ENTERED FROM                      
09296                  PB-B-ISSUE-CNT-REMITTED GIVING DOISSO.           
09297                                                                   
09298      MOVE PB-B-LF-ISS-PRM-REMITTED   TO DRLFPRMO.                 
09299      MOVE PB-B-LF-ISS-PRM-ENTERED    TO DELFPRMO.                 
09300      MOVE PB-B-LF-ISS-PRM-COMPUTED   TO DCLFPRMO.                 
09301                                                                   
09302      IF PB-B-LF-ISS-PRM-ENTERED NOT = PB-B-LF-ISS-PRM-REMITTED    
09303         SUBTRACT PB-B-LF-ISS-PRM-ENTERED FROM                     
09304                  PB-B-LF-ISS-PRM-REMITTED  GIVING DOLFPRMO.       
09305                                                                   
09306      MOVE PB-B-AH-ISS-PRM-REMITTED   TO DRAHPRMO.                 
09307      MOVE PB-B-AH-ISS-PRM-ENTERED    TO DEAHPRMO.                 
09308      MOVE PB-B-AH-ISS-PRM-COMPUTED   TO DCAHPRMO.                 
09309                                                                   
09310      IF PB-B-AH-ISS-PRM-ENTERED NOT = PB-B-AH-ISS-PRM-REMITTED    
09311         SUBTRACT PB-B-AH-ISS-PRM-ENTERED FROM                     
09312                  PB-B-AH-ISS-PRM-REMITTED  GIVING DOAHPRMO.       
09313                                                                   
09314      MOVE PB-B-CANCEL-CNT-REMITTED     TO DRCANCO.                
09315      MOVE PB-B-CANCEL-CNT-ENTERED      TO DECANCO.                
09316                                                                   
09317      IF PB-B-CANCEL-CNT-REMITTED NOT = PB-B-CANCEL-CNT-ENTERED    
09318         SUBTRACT PB-B-CANCEL-CNT-ENTERED FROM                     
09319                  PB-B-CANCEL-CNT-REMITTED GIVING DOCANCO.         
09320                                                                   
09321      MOVE PB-B-LF-CAN-PRM-REMITTED   TO DRLFRFDO.                 
09322      MOVE PB-B-LF-CAN-PRM-ENTERED    TO DELFRFDO.                 
09323      MOVE PB-B-LF-CAN-PRM-COMPUTED   TO DCLFRFDO.                 
09324                                                                   
09325      IF PB-B-LF-CAN-PRM-ENTERED NOT = PB-B-LF-CAN-PRM-REMITTED    
09326         SUBTRACT PB-B-LF-CAN-PRM-ENTERED FROM                     
09327                  PB-B-LF-CAN-PRM-REMITTED  GIVING DOLFRFDO.       
09328                                                                   
09329      MOVE PB-B-AH-CAN-PRM-REMITTED   TO DRAHRFDO.                 
09330      MOVE PB-B-AH-CAN-PRM-ENTERED    TO DEAHRFDO.                 
09331      MOVE PB-B-AH-CAN-PRM-COMPUTED   TO DCAHRFDO.                 
09332                                                                   
09333      IF PB-B-AH-CAN-PRM-ENTERED NOT = PB-B-AH-CAN-PRM-REMITTED    
09334         SUBTRACT PB-B-AH-CAN-PRM-ENTERED FROM                     
09335                  PB-B-AH-CAN-PRM-REMITTED  GIVING DOAHRFDO.       
09336                                                                   
09337      MOVE -1                     TO DPFENTRL.                     
09338      GO TO 8100-SEND-INITIAL-MAP.                                 
09339                                                                   
09340      EJECT                                                        
09341                                                                   
09342 ***************************************************************   
09343 *     THIS ROUTINE DETERMINES IF EL050 ENCOUNTERED ANY        *   
09344 *     ERRORS.  IF ERRORS WERE DETECTED, THE ONLINE ERROR      *   
09345 *     PROCESSING ROUTINE IS CALLED TO FORMAT THE ERROR        *   
09346 *     MESSAGES AND ACCUMULATE TOTALS FOR THE DIFFERENT        *   
09347 *     SEVERITY LEVELS. THE SEVERITY FLAGS WITHIN THE RECORDS  *   
09348 *     ARE THEN SET ACCORDINGLY.                               *   
09349 *         ERROR NUMBERS FOR THE STANDARD ERRORS START AT 2600 *   
09350 *         ERROR NUMBERS FOR ISSUE TRANSACTIONS  START AT 2625 *   
09351 *         ERROR NUMBERS FOR CANCEL TRANSACTIONS START AT 2725 *   
09352 ***************************************************************   
09353                                                                   
09354 ******************************************************************
09355 *                                                                *
09356 *            E R R O R   A N A L Y S I S                         *
09357 *                                                                *
09358 ******************************************************************
09359                                                                   
09360 ******************************************************************
09361 *                                                                *
09362 *   THIS ROUTINE DETERMINES IF EL050 (EDIT PROGRAM) ENCOUNTERED  *
09363 *   ANY ERRORS.  IF ERRORS WERE DETECTED, THE ROUTINE READS THE  *
09364 *   ERROR FILE TO DETERMINE THE SEVERITY OF THE ERRORS.  ONCE    *
09365 *   THE SEVERITY OF ERRORS IS ESTABLISHED THE APPROPRIATE ERROR  *
09366 *   FLAGS ARE SET (FORCIBLE, UNFORCIBLE, WARNING, ETC.).         *
09367 *                                                                *
09368 *   IF ANY UNFORCED OR FATAL ERRORS, OR RECORD IS RETURNED       *
09369 *   OR PLACED ON HOLD UPDATE THE CERTIFICATE ACCORDINGLY.        *
09370 *                                                                *
09371 ******************************************************************
09372                                                                   
09373  7300-FORMAT-ERRORS.                                              

           MOVE ' '                    TO PI-ERRORS-SW

09375      IF PB-COMMON-ERRORS = LOW-VALUES                             
09376         IF PB-FATAL-ERRORS                                        
09377            MOVE ER-2695          TO EMI-ERROR                     
09378            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
09379            GO TO 7399-EXIT.                                       
09380                                                                   
09381      MOVE +0                     TO WS-SUB4.                      
09382                                                                   
09383  7310-ERROR-LOOP.                                                 
09384                                                                   
09385      ADD +1                      TO WS-SUB4.                      
09386                                                                   
09387      IF WS-SUB4 GREATER THAN PB-NO-OF-ERRORS                      
09388         GO TO 7350-SET-ERROR-FLAGS.                               
09389                                                                   
09390      IF PI-COMPANY-ID = 'UCL'                                     
09391          IF PB-COMMON-ERROR (WS-SUB4) = ER-2799                   
09392              MOVE WS-CERT-EFF-DT        TO DC-BIN-DATE-1          
09393              MOVE ' '                   TO DC-OPTION-CODE         
09394              PERFORM 9700-DATE-LINK                               
09395              MOVE DC-GREG-DATE-1-MDY    TO EMI-AH-OVERRIDE-L6.    
09396                                                                   
09397      MOVE PB-COMMON-ERROR (WS-SUB4) TO EMI-ERROR.                 
09398      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
09399      PERFORM 7400-SET-ATTRIBUTES THRU 7499-EXIT.                  
09400                                                                   
09401      GO TO 7310-ERROR-LOOP.                                       
09402                                                                   
09403                                                                   
09404  7350-SET-ERROR-FLAGS.                                            
09405                                                                   
09406      IF EMI-FATAL-CTR NOT = ZEROS 
09407         MOVE 'Y'                 TO WS-ERRORS-PRESENT-SW          
09408         MOVE 'X'                 TO PB-FATAL-FLAG
                                          PI-ERRORS-SW
09409         GO TO 7355-SET-ERROR-FLAGS.                               
09410                                                                   
111513     move ' ' to pi-unforced-sw
09411      IF EMI-FORCABLE-CTR NOT = ZEROS                              
09412         MOVE 'Y'                 TO WS-ERRORS-PRESENT-SW          
09413         IF PB-ISSUE                                               
09414            IF PB-ISSUE-FORCE                                      
09415               MOVE 'F'           TO PB-FORCE-ER-CD                
09416               MOVE ER-2600       TO EMI-ERROR                     
09417               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            
09418              ELSE                                                 
09419               MOVE 'X'           TO PB-FORCE-ER-CD
062712*                                   PI-ERRORS-SW
09420           ELSE                                                    
09421            IF PB-CANCEL-FORCE OR PB-ALL-CANCEL-FORCED-NO-FEE      
09422               MOVE 'F'           TO PB-FORCE-ER-CD
111513                                    pi-unforced-sw                
09423               MOVE ER-2600       TO EMI-ERROR                     
09424               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            
09425              ELSE                                                 
062712              MOVE 'X'           TO PB-FORCE-ER-CD
111513                                    pi-unforced-sw.
062712*                                   PI-ERRORS-SW.
072312
072312     MOVE PB-CONTROL-BY-ACCOUNT (1:33) TO ERENDT-KEY (1:33)
072312     IF PB-ISSUE
072312         MOVE 'I'                TO ENDT-RECORD-TYPE
072312     ELSE
072312         MOVE 'C'                TO ENDT-RECORD-TYPE
072312     END-IF
072312     MOVE +0                     TO ENDT-SEQ-NO
072312
072312     EXEC CICS READ
072312         DATASET   (FILE-ID-ERENDT)
072312         SET       (ADDRESS OF ENDORSEMENT-RECORD)
072312         RIDFLD    (ERENDT-KEY)
072312         RESP      (WS-RESPONSE)
072312         GTEQ
072312     END-EXEC
072312
072312     IF RESP-NORMAL
072312        AND (EN-CONTROL-PRIMARY (1:34) = ERENDT-KEY (1:34))
072312        IF EN-BATCH-NUMBER = PB-ENTRY-BATCH
072312            MOVE ER-0280          TO EMI-ERROR
072312            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
072312            MOVE EN-ARCHIVE-NO    TO ARCH-SUPPRESS
072312            EVALUATE TRUE
072312              WHEN EMI-ERROR-NUMBER (1) EQUAL ER-0280
072312                 MOVE ARCH-EDIT   TO EMI-TEXT-VARIABLE (1)
072312              WHEN EMI-ERROR-NUMBER (2) EQUAL ER-0280
072312                 MOVE ARCH-EDIT   TO EMI-TEXT-VARIABLE (2)
072312              WHEN EMI-ERROR-NUMBER (3) EQUAL ER-0280
072312                 MOVE ARCH-EDIT   TO EMI-TEXT-VARIABLE (3)
072312            END-EVALUATE
072312        END-IF
072312     END-IF
072312
072312
09427                                                                   
09428      IF FIRST-ENTRY OR                                            
09429         EIBAID = DFHPF1 OR DFHPF2                                 
09430         MOVE '0'                TO PI-EDIT-SW                     
09431         GO TO 7399-EXIT.                                          
09432                                                                   
09433  7355-SET-ERROR-FLAGS.                                            
09434                                                                   
09435      IF EMI-WARNING-CTR NOT = ZEROS                               
09436         MOVE 'Y'                 TO WS-ERRORS-PRESENT-SW          
09437         MOVE 'W'                 TO PB-WARN-ER-CD.                
09438                                                                   
09439      IF PB-UNFORCED-ERRORS OR                                     
09440         PB-FATAL-ERRORS    OR                                     
09441         PB-RECORD-ON-HOLD  OR                                     
09442         PB-RECORD-RETURNED OR                                     
09443         PB-CANCELLATION                                           
09444           NEXT SENTENCE                                           
09445         ELSE                                                      
09446           GO TO 7399-EXIT.                                        
09447                                                                   
09448      EXEC CICS  HANDLE CONDITION                                  
09449             NOTFND   (7399-EXIT)                                  
09450      END-EXEC.                                                    
09451                                                                   
09452      MOVE PB-CONTROL-BY-ACCOUNT  TO ELCERT-KEY.                   
09453      MOVE PB-SV-CARRIER          TO ELCERT-CARRIER.               
09454      MOVE PB-SV-GROUPING         TO ELCERT-GROUPING.              
09455      MOVE PB-SV-STATE            TO ELCERT-STATE.                 
09456                                                                   
09457      EXEC CICS READ                                               
09458          SET     (ADDRESS OF CERTIFICATE-MASTER)                  
09459          DATASET (FILE-ID-ELCERT)                                 
09460          RIDFLD  (ELCERT-KEY)                                     
09461          UPDATE                                                   
09462      END-EXEC.                                                    
09463                                                                   
09464                                                                   
09465      MOVE CM-CREDIT-INTERFACE-SW-1         TO WS-SW-1             
09466      MOVE CM-CREDIT-INTERFACE-SW-2         TO WS-SW-2             
09467                                                                   
09468      IF (CERT-ADDED-BATCH OR CERT-PURGED-OFFLINE) AND PB-ISSUE    
09469         GO TO 7360-REWRITE-CERT-MASTER.                           
09470                                                                   
09471      IF PB-ISSUE                                                  
09472         IF PB-RECORD-RETURNED                                     
09473             MOVE '4'  TO CM-CREDIT-INTERFACE-SW-1                 
09474             GO TO 7360-REWRITE-CERT-MASTER                        
09475         ELSE                                                      
09476             MOVE '2' TO CM-CREDIT-INTERFACE-SW-1                  
09477             GO TO 7360-REWRITE-CERT-MASTER.                       
09478                                                                   
09479      IF PB-RECORD-RETURNED                                        
09480          MOVE '7'      TO CM-CREDIT-INTERFACE-SW-2                
09481          GO TO 7360-REWRITE-CERT-MASTER.                          
09482                                                                   
09483      IF (PB-C-LF-CANCEL-VOIDED)
              or (pb-c-ah-cancel-voided)
09484         IF (PB-UNFORCED-ERRORS OR                                 
09485             PB-FATAL-ERRORS    OR                                 
09486             PB-RECORD-ON-HOLD)                                    
09487             MOVE '6'             TO CM-CREDIT-INTERFACE-SW-2      
09488             GO TO 7360-REWRITE-CERT-MASTER                        
09489         ELSE                                                      
09490             MOVE '5'             TO CM-CREDIT-INTERFACE-SW-2      
09491             GO TO 7360-REWRITE-CERT-MASTER.                       
09492                                                                   
09493      IF (PB-UNFORCED-ERRORS OR PB-FATAL-ERRORS                    
09494                             OR PB-RECORD-ON-HOLD)                 
09495         MOVE '4'                 TO CM-CREDIT-INTERFACE-SW-2      
09496      ELSE                                                         
09497         MOVE '1'                 TO CM-CREDIT-INTERFACE-SW-2.     
09498                                                                   
09499  7360-REWRITE-CERT-MASTER.                                        
09500                                                                   
09501      IF WS-SW-1 = CM-CREDIT-INTERFACE-SW-1 AND                    
09502         WS-SW-2 = CM-CREDIT-INTERFACE-SW-2                        
09503         EXEC CICS UNLOCK                                          
09504              DATASET    (FILE-ID-ELCERT)                          
09505         END-EXEC                                                  
09506         GO TO 7399-EXIT.                                          
09507                                                                   
09508      EXEC CICS REWRITE                                            
09509           DATASET    (FILE-ID-ELCERT)                             
09510           FROM       (CERTIFICATE-MASTER)                         
09511      END-EXEC.                                                    
09512                                                                   
09513  7399-EXIT.                                                       
09514       EXIT.                                                       
09515                                                                   
09516      EJECT                                                        
09517                                                                   
09518 *************************************************************     
09519 *                                                           *     
09520 *              S E T   A T T R I B U T E S                  *     
09521 *                                                           *     
09522 *************************************************************     
09523                                                                   
09524  7400-SET-ATTRIBUTES.                                             
09525                                                                   
09526      IF WS-SUB4 GREATER THAN PB-NO-OF-ERRORS                      
09527         GO TO 7499-EXIT.                                          
09528                                                                   
09529 **2601**                                                          
09530                                                                   
09531      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2601                    
09532         IF PB-ISSUE                                               
09533            MOVE AL-UABON         TO BEFFDTA                       
09534            GO TO 7499-EXIT                                        
09535          ELSE                                                     
09536            MOVE AL-UABON         TO CEFFDTA                       
09537            GO TO 7499-EXIT.                                       
09538                                                                   
09539 **2604**                                                          
09540                                                                   
09541      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2604                    
09542         IF PB-ISSUE                                               
09543            MOVE AL-UABON         TO BTYPE1A                       
09544            MOVE -1               TO BTYPE1L                       
09545            MOVE  1               TO SET-CURSOR-SW                 
09546            GO TO 7499-EXIT.                                       
09547                                                                   
09548 **2605**                                                          
09549                                                                   
09550      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2605                    
09551         IF PB-ISSUE                                               
09552            MOVE AL-UABON         TO BTYPE2A                       
09553            MOVE -1               TO BTYPE2L                       
09554            MOVE  1               TO SET-CURSOR-SW                 
09555            GO TO 7499-EXIT.                                       
09556                                                                   
09557 **2606**                                                          
09558                                                                   
09559      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2606                    
09560         IF PB-ISSUE                                               
09561            MOVE AL-UABON         TO BSIGA                         
09562            MOVE -1               TO BSIGL                         
09563            MOVE  1               TO SET-CURSOR-SW                 
09564            GO TO 7499-EXIT.                                       
09565                                                                   
09566 **2607**                                                          
09567                                                                   
09568      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2607                    
09569         IF PB-ISSUE                                               
09570            MOVE AL-UABON         TO BSIGA                         
09571            MOVE -1               TO BSIGL                         
09572            MOVE  1               TO SET-CURSOR-SW                 
09573            GO TO 7499-EXIT.                                       
09574                                                                   
09575 **2615**                                                          
09576                                                                   
09577      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2607                    
09578         IF PB-ISSUE                                               
09579            MOVE AL-UABOF         TO BREINA                        
09580            MOVE -1               TO BREINL                        
09581            GO TO 7499-EXIT.                                       
09582                                                                   
09583 **2622**                                                          
09584                                                                   
09585      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2622                    
09586         IF PB-ISSUE                                               
09587            MOVE AL-UABON         TO BEFFDTA                       
09588            GO TO 7499-EXIT                                        
09589         ELSE                                                      
09590            MOVE AL-UABON         TO CEFFDTA                       
09591            GO TO 7499-EXIT.                                       
09592                                                                   
09593      EJECT                                                        
09594                                                                   
09595 *************************************************************     
09596 *                                                           *     
09597 *              I S S U E   E R R O R S                      *     
09598 *                                                           *     
09599 *************************************************************     
09600                                                                   
09601  7420-ISSUE-ERRORS.                                               
09602                                                                   
09603 **2626**                                                          
09604                                                                   
09605      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2626                    
09606         MOVE AL-UABON            TO BCERTNOA                      
09607         GO TO 7499-EXIT.                                          
09608                                                                   
09609 **2627**                                                          
09610                                                                   
09611      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2627                    
09612         MOVE AL-UNBON            TO BAGEA                         
09613         MOVE -1                  TO BAGEL                         
09614         MOVE  1                  TO SET-CURSOR-SW                 
09615         GO TO 7499-EXIT.                                          
09616                                                                   
09617 **2628**                                                          
09618                                                                   
09619      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2628                    
09620         MOVE AL-UNBON            TO BAGEA                         
09621         MOVE -1                  TO BAGEL                         
09622         MOVE  1                  TO SET-CURSOR-SW                 
09623         GO TO 7499-EXIT.                                          
09624                                                                   
09625 **2629**                                                          
09626                                                                   
09627      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2629                    
09628         MOVE AL-UABON            TO BSEXA                         
09629         MOVE -1                  TO BSEXL                         
09630         MOVE  1                  TO SET-CURSOR-SW                 
09631         GO TO 7499-EXIT.                                          
09632                                                                   
09633 **2630**                                                          
09634                                                                   
09635      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2630                    
09636         MOVE -1                  TO BTYPE1L                       
09637         MOVE  1                  TO SET-CURSOR-SW                 
09638         GO TO 7499-EXIT.                                          
09639                                                                   
09640 **2631**                                                          
09641                                                                   
09642      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2631                    
09643         MOVE -1                  TO BTYPE1L                       
09644         MOVE  1                  TO SET-CURSOR-SW                 
09645         GO TO 7499-EXIT.                                          
09646                                                                   
09647 **2632**                                                          
09648                                                                   
09649      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2632                    
09650         MOVE AL-UNBON            TO BBEN1A                        
09651         MOVE -1                  TO BBEN1L                        
09652         MOVE  1                  TO SET-CURSOR-SW                 
09653         GO TO 7499-EXIT.                                          
09654                                                                   
09655 **2633**                                                          
09656                                                                   
09657      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2633                    
09658         MOVE AL-UNBON            TO BPREM1A                       
09659         MOVE -1                  TO BPREM1L                       
09660         MOVE  1                  TO SET-CURSOR-SW                 
09661         GO TO 7499-EXIT.                                          
09662                                                                   
09663 **2634**                                                          
09664                                                                   
09665      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2634                    
09666         MOVE -1                  TO BTYPE2L                       
09667         MOVE  1                  TO SET-CURSOR-SW                 
09668         GO TO 7499-EXIT.                                          
09669                                                                   
09670 **2635**                                                          
09671                                                                   
09672      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2635                    
09673         MOVE AL-UNBON            TO BBEN2A                        
09674         MOVE -1                  TO BBEN2L                        
09675         MOVE  1                  TO SET-CURSOR-SW                 
09676         GO TO 7499-EXIT.                                          
09677                                                                   
09678 **2636**                                                          
09679                                                                   
09680      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2636                    
09681         MOVE AL-UNBON            TO BPREM2A                       
09682         MOVE -1                  TO BPREM2L                       
09683         MOVE  1                  TO SET-CURSOR-SW                 
09684         GO TO 7499-EXIT.                                          
09685                                                                   
09686 **2639**                                                          
09687                                                                   
09688      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2639                    
09689         MOVE AL-UNBON            TO BBEN1A                        
09690         MOVE -1                  TO BBEN1L                        
09691         MOVE  1                  TO SET-CURSOR-SW                 
09692         GO TO 7499-EXIT.                                          
09693                                                                   
09694 **2640**                                                          
09695                                                                   
09696      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2640                    
09697         MOVE AL-UNBON            TO BBEN2A                        
09698         MOVE -1                  TO BBEN2L                        
09699         MOVE  1                  TO SET-CURSOR-SW                 
09700         GO TO 7499-EXIT.                                          
09701                                                                   
09702 **2647**                                                          
09703                                                                   
09704      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2647                    
09705         MOVE AL-UNBON            TO BTERM1A                       
09706         MOVE -1                  TO BTERM1L                       
09707         MOVE  1                  TO SET-CURSOR-SW                 
09708         GO TO 7499-EXIT.                                          
09709                                                                   
09717                                                                   
09718 **2649**                                                          
09719                                                                   
09720      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2649                    
09721         AND PB-I-LF-PREMIUM-AMT GREATER THAN ZEROS                
09722             MOVE AL-UNBON            TO BPREM1A                   
09723             MOVE -1                  TO BPREM1L                   
09724             MOVE  1                  TO SET-CURSOR-SW             
09725             GO TO 7499-EXIT                                       
09726      ELSE                                                         
09727         IF PB-COMMON-ERROR    (WS-SUB4) = ER-2649                 
09728            AND PB-I-AH-PREMIUM-AMT GREATER THAN ZEROS             
09729             MOVE AL-UNBON            TO BPREM2A                   
09730             MOVE -1                  TO BPREM2L                   
09731             MOVE  1                  TO SET-CURSOR-SW             
09732             GO TO 7499-EXIT.                                      
09733                                                                   
09734 **2650**                                                          
09735                                                                   
09736      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2650                    
09737         IF  PB-I-LF-PREMIUM-AMT GREATER THAN ZEROS                
09738             MOVE AL-UNBON        TO BPREM1A                       
09739             MOVE -1              TO BPREM1L                       
09740             MOVE  1              TO SET-CURSOR-SW                 
09741             IF  PB-I-AH-PREMIUM-AMT GREATER THAN ZEROS            
09742                 MOVE AL-UNBON        TO BPREM2A                   
09743                 MOVE -1              TO BPREM2L                   
09744                 MOVE  1              TO SET-CURSOR-SW             
09745                 GO TO 7499-EXIT                                   
09746             ELSE                                                  
09747                 GO TO 7499-EXIT                                   
09748         ELSE                                                      
09749             IF  PB-I-AH-PREMIUM-AMT GREATER THAN ZEROS            
09750                 MOVE AL-UNBON        TO BPREM2A                   
09751                 MOVE -1              TO BPREM2L                   
09752                 MOVE  1              TO SET-CURSOR-SW             
09753                 GO TO 7499-EXIT                                   
09754             ELSE                                                  
09755                 GO TO 7499-EXIT.                                  
09756                                                                   
09757 **2651**                                                          
09758                                                                   
09759      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2650                    
09760         MOVE AL-UABON            TO BSIGA                         
09761         MOVE -1                  TO BSIGL                         
09762         MOVE  1                  TO SET-CURSOR-SW                 
09763         GO TO 7499-EXIT.                                          
09764                                                                   
09772                                                                   
09788                                                                   
09804                                                                   
09805 **2658**                                                          
09806                                                                   
09807      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2657                    
09808         MOVE AL-UNBON            TO BTERM2A                       
09809         MOVE -1                  TO BTERM2L                       
09810         MOVE  1                  TO SET-CURSOR-SW                 
09811         GO TO 7499-EXIT.                                          
09812                                                                   
09813 **2660**                                                          
09814                                                                   
09815      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2660                    
09816         MOVE AL-UABON            TO BLSTNMA B1STNMA BINITA        
09817         MOVE -1                  TO BLSTNML                       
09818         MOVE  1                  TO SET-CURSOR-SW                 
09819         GO TO 7499-EXIT.                                          
09820                                                                   
09821                                                                   
09822 **2662**                                                          
09823                                                                   
09824      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2662                    
09825         MOVE AL-UNBON            TO BAGEA                         
09826         MOVE -1                  TO BAGEL                         
09827         MOVE  1                  TO SET-CURSOR-SW                 
09828         GO TO 7499-EXIT.                                          
09829                                                                   
09830 **2663**                                                          
09831                                                                   
09832      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2663                    
09833         MOVE AL-UNBON            TO BAGEA                         
09834         MOVE -1                  TO BAGEL                         
09835         MOVE  1                  TO SET-CURSOR-SW                 
09836         GO TO 7499-EXIT.                                          
09837                                                                   
09838 **2664**                                                          
09839                                                                   
09840      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2664                    
09841         MOVE AL-UABON            TO BREINA                        
09842         MOVE -1                  TO BREINL                        
09843         MOVE  1                  TO SET-CURSOR-SW                 
09844         GO TO 7499-EXIT.                                          
09845                                                                   
09846 **2667**                                                          
09847                                                                   
09848      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2667                    
09849         MOVE AL-UNBON            TO BBEN2A                        
09850         MOVE -1                  TO BBEN2L                        
09851         MOVE  1                  TO SET-CURSOR-SW                 
09852         GO TO 7499-EXIT.                                          
09853                                                                   
09854 **2668**                                                          
09855                                                                   
09856      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2668                    
09857         MOVE AL-UABON            TO BEFFDTA                       
09858         GO TO 7499-EXIT.                                          
09859                                                                   
09860 **2669**                                                          
09861                                                                   
020711     IF PB-COMMON-ERROR (WS-SUB4) = ER-2669 OR ER-1570
080713       OR ER-2077 OR ER-2628 OR ER-2661 OR ER-3054 OR ER-3504
080713       OR ER-3513 OR ER-7144 OR ER-9063 OR ER-9076 OR ER-9618
09863         MOVE AL-UNBON            TO BAGEA                         
09864         MOVE -1                  TO BAGEL                         
09865         MOVE  1                  TO SET-CURSOR-SW                 
09866         GO TO 7499-EXIT.                                          
09867                                                                   
09868 **2670**                                                          
09869                                                                   
09870      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2670                    
09871         MOVE AL-UABON            TO BFORCEA                       
09872         MOVE -1                  TO BFORCEL                       
09873         MOVE  1                  TO SET-CURSOR-SW                 
09874         GO TO 7499-EXIT.                                          
09875                                                                   
09876 **2671**                                                          
09877                                                                   
09878      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2671                    
09879         MOVE AL-UABON            TO BEFFDTA                       
09880         GO TO 7499-EXIT.                                          
09881                                                                   
09882 **2672**                                                          
09883                                                                   
09884      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2672                    
09885         MOVE AL-UABON            TO BEFFDTA                       
09886         GO TO 7499-EXIT.                                          
09887                                                                   
09888 **2673**                                                          
09889                                                                   
09890      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2673                    
09891         MOVE AL-UABON            TO BEFFDTA                       
09892         GO TO 7499-EXIT.                                          
09893                                                                   
09894 **2674**                                                          
09895                                                                   
09896      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2673                    
09897         MOVE AL-UABON            TO BTYPE1A BTYPE2A               
09898         MOVE -1                  TO BTYPE1L                       
09899         MOVE  1                  TO SET-CURSOR-SW                 
09900         GO TO 7499-EXIT.                                          
09901                                                                   
09902 **2677**                                                          
09903                                                                   
09904      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2677                    
09905         MOVE AL-UNBON            TO BTERM1A                       
09906         MOVE -1                  TO BTERM1L                       
09907         MOVE  1                  TO SET-CURSOR-SW                 
09908         GO TO 7499-EXIT.                                          
09909                                                                   
09910 **2678**                                                          
09911                                                                   
09912      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2678                    
09913         MOVE AL-UNBON            TO BTERM2A                       
09914         MOVE -1                  TO BTERM2L                       
09915         MOVE  1                  TO SET-CURSOR-SW                 
09916         GO TO 7499-EXIT.                                          
09917                                                                   
09918 **2679**                                                          
09919                                                                   
09920      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2679                    
09921         MOVE AL-UABON            TO BTYPE1A                       
09922         MOVE -1                  TO BTYPE1L                       
09923         MOVE  1                  TO SET-CURSOR-SW                 
09924         GO TO 7499-EXIT.                                          
09925                                                                   
09933                                                                   
09941                                                                   
09942 **2682**                                                          
09943                                                                   
09944      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2682                    
09945         MOVE AL-UABON            TO BTERM1A                       
09946         MOVE -1                  TO BTERM1L                       
09947         MOVE  1                  TO SET-CURSOR-SW                 
09948         GO TO 7499-EXIT.                                          
09949                                                                   
09950 **2683**                                                          
09951                                                                   
09952      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2683                    
09953         MOVE AL-UABON            TO BSKPCDA                       
09954         MOVE -1                  TO BSKPCDL                       
09955         MOVE  1                  TO SET-CURSOR-SW                 
09956         GO TO 7499-EXIT.                                          
09957                                                                   
09958 **2684**                                                          
09959                                                                   
09960      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2684                    
09961         MOVE AL-UABON            TO BTYPE2A                       
09962         MOVE -1                  TO BTYPE2L                       
09963         MOVE  1                  TO SET-CURSOR-SW                 
09964         GO TO 7499-EXIT.                                          
09965                                                                   
09966 **2686**                                                          
09967                                                                   
09968      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2686                    
09969         MOVE AL-UABON            TO BTYPE2A                       
09970         MOVE -1                  TO BTYPE2L                       
09971         MOVE  1                  TO SET-CURSOR-SW                 
09972         GO TO 7499-EXIT.                                          
09973                                                                   
09974 **2689**                                                          
09975                                                                   
09976      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2689                    
09977         MOVE AL-UABON            TO BSIGA                         
09978         MOVE -1                  TO BSIGL                         
09979         MOVE  1                  TO SET-CURSOR-SW                 
09980         GO TO 7499-EXIT.                                          
09981                                                                   
09982 **2690**                                                          
09983                                                                   
09984      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2690                    
09985         MOVE AL-UABOF            TO BLNOFCRA                      
09986         MOVE -1                  TO BLNOFCRL                      
09987         MOVE  1                  TO SET-CURSOR-SW                 
09988         GO TO 7499-EXIT.                                          
09989                                                                   
09990 **2693**                                                          
09991                                                                   
09992      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2693                    
09993         MOVE AL-UABOF            TO BLNOFCRA                      
09994         MOVE AL-UNBON            TO BBEN1A                        
09995         MOVE -1                  TO BBEN1L                        
09996         MOVE  1                  TO SET-CURSOR-SW                 
09997         GO TO 7499-EXIT.                                          
09998                                                                   
CIDMOD**2720**                                                          
CIDMOD                                                                  
CIDMOD     IF PB-COMMON-ERROR    (WS-SUB4) = ER-2720                    
CIDMOD        MOVE AL-UNBON            TO BBEN1A                        
CIDMOD        MOVE -1                  TO BBEN1L                        
CIDMOD        MOVE  1                  TO SET-CURSOR-SW                 
CIDMOD        GO TO 7499-EXIT.                                          
CIDMOD                                                                  
CIDMOD**2709**                                                          
CIDMOD                                                                  
CIDMOD*    IF PB-COMMON-ERROR    (WS-SUB4) = ER-2709                    
CIDMOD*       MOVE AL-UNBON            TO BBEN1A                        
CIDMOD*       MOVE -1                  TO BBEN1L                        
CIDMOD*       MOVE  1                  TO SET-CURSOR-SW                 
CIDMOD*       GO TO 7499-EXIT.
10000                                                                   
10001 *************************************************************     
10002 *                                                           *     
10003 *            C A N C E L   E R R O R S                      *     
10004 *                                                           *     
10005 *************************************************************     
10006                                                                   
10007 **2726**                                                          
10008                                                                   
10009      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2726                    
10010         MOVE AL-UABON            TO CCERTNOA                      
10011         GO TO 7499-EXIT.                                          
10012                                                                   
10013 **2728**                                                          
10014                                                                   
10015      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2728                    
10016         MOVE AL-UABON            TO CCERTNOA                      
10017         GO TO 7499-EXIT.                                          
10018                                                                   
10019 **2729**                                                          
10020                                                                   
10021      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2729                    
10022         MOVE AL-UABON            TO CCERTNOA                      
10023         GO TO 7499-EXIT.                                          
10024                                                                   
10025 **2730**                                                          
10026                                                                   
10027      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2729                    
10028         MOVE AL-UABON            TO CCERTNOA                      
10029         GO TO 7499-EXIT.                                          
10030                                                                   
10031 **2731**                                                          
10032                                                                   
10033      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2731                    
10034         MOVE AL-UABON            TO CCANDT1A                      
10035         MOVE -1                  TO CCANDT1L                      
10036         MOVE  1                  TO SET-CURSOR-SW                 
10037         GO TO 7499-EXIT.                                          
10038                                                                   
10039 **2732**                                                          
10040                                                                   
10041      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2732                    
10042         MOVE AL-UABON            TO CCANDT1A                      
10043         MOVE -1                  TO CCANDT1L                      
10044         MOVE  1                  TO SET-CURSOR-SW                 
10045         GO TO 7499-EXIT.                                          
10046                                                                   
10047 **2733**                                                          
10048                                                                   
10049      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2733                    
10050         MOVE AL-UABON            TO CCANDT1A                      
10051         MOVE -1                  TO CCANDT1L                      
10052         MOVE  1                  TO SET-CURSOR-SW                 
10053         GO TO 7499-EXIT.                                          
10054                                                                   
10055 **2734**                                                          
10056                                                                   
10057      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2733                    
10058         MOVE AL-UNBON            TO CREFND1A                      
10059         MOVE -1                  TO CREFND1L                      
10060         MOVE  1                  TO SET-CURSOR-SW                 
10061         GO TO 7499-EXIT.                                          
10062                                                                   
10063 **2735**                                                          
10064                                                                   
10065      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2735                    
10066         MOVE AL-UABON            TO CCANDT2A                      
10067         MOVE -1                  TO CCANDT2L                      
10068         MOVE  1                  TO SET-CURSOR-SW                 
10069         GO TO 7499-EXIT.                                          
10070                                                                   
10071 **2736**                                                          
10072                                                                   
10073      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2735                    
10074         IF  PB-C-LF-CANCEL-AMT GREATER THAN ZEROS                 
10075             MOVE AL-UNBON        TO CREFND1A                      
10076             MOVE -1              TO CREFND1L                      
10077             MOVE  1              TO SET-CURSOR-SW                 
10078             IF  PB-C-AH-CANCEL-AMT GREATER THAN ZEROS             
10079                 MOVE AL-UNBON        TO CREFND2A                  
10080                 MOVE -1              TO CREFND2L                  
10081                 MOVE  1              TO SET-CURSOR-SW             
10082                 GO TO 7499-EXIT                                   
10083             ELSE                                                  
10084                 GO TO 7499-EXIT                                   
10085         ELSE                                                      
10086             IF  PB-C-AH-CANCEL-AMT GREATER THAN ZEROS             
10087                 MOVE AL-UNBON        TO CREFND2A                  
10088                 MOVE -1              TO CREFND2L                  
10089                 MOVE  1              TO SET-CURSOR-SW             
10090                 GO TO 7499-EXIT                                   
10091             ELSE                                                  
10092                 GO TO 7499-EXIT.                                  
10093                                                                   
10094 **2738**                                                          
10095                                                                   
10096      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2738                    
10097         MOVE -1                  TO CREFND2L                      
10098         MOVE  1                  TO SET-CURSOR-SW                 
10099         GO TO 7499-EXIT.                                          
10100                                                                   
10101 **2739**                                                          
10102                                                                   
10103      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2738                    
10104         MOVE -1                  TO CREFND1L                      
10105         MOVE  1                  TO SET-CURSOR-SW                 
10106         GO TO 7499-EXIT.                                          
10107                                                                   
10108 **2741**                                                          
10109                                                                   
10110      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2738                    
10111         MOVE -1                  TO CREFND1L                      
10112         MOVE  1                  TO SET-CURSOR-SW                 
10113         GO TO 7499-EXIT.                                          
10114                                                                   
10115 **2743**                                                          
10116                                                                   
10117      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2743                    
10118         MOVE AL-UNBON            TO CCANDT1A                      
10119         MOVE -1                  TO CCANDT1L                      
10120         MOVE  1                  TO SET-CURSOR-SW                 
10121         GO TO 7499-EXIT.                                          
10122                                                                   
10123 **2744**                                                          
10124                                                                   
10125      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2744                    
10126         MOVE AL-UNBON            TO CCANDT2A                      
10127         MOVE -1                  TO CCANDT2L                      
10128         MOVE  1                  TO SET-CURSOR-SW                 
10129         GO TO 7499-EXIT.                                          
10130                                                                   
10131 **2747**                                                          
10132                                                                   
10133      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2747                    
10134         MOVE AL-UABON            TO CLSTNMA                       
10135         MOVE -1                  TO CLSTNML                       
10136         MOVE  1                  TO SET-CURSOR-SW                 
10137         GO TO 7499-EXIT.                                          
10138                                                                   
10139 **2749**                                                          
10140                                                                   
10141      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2749                    
10142         MOVE AL-UABON            TO CFORCEA                       
10143         MOVE -1                  TO CFORCEL                       
10144         MOVE  1                  TO SET-CURSOR-SW                 
10145         GO TO 7499-EXIT.                                          
10146                                                                   
10147 **2753**                                                          
10148                                                                   
10149      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2753                    
10150         MOVE AL-UNBON            TO CCANDT1A                      
10151         MOVE -1                  TO CCANDT1L                      
10152         MOVE  1                  TO SET-CURSOR-SW                 
10153         GO TO 7499-EXIT.                                          
10154                                                                   
10155 **2755**                                                          
10156                                                                   
10157      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2755                    
10158         IF  PB-C-LF-CANCEL-AMT GREATER THAN ZEROS                 
10159             MOVE AL-UNBON        TO CREFND1A                      
10160             MOVE -1              TO CREFND1L                      
10161             MOVE  1              TO SET-CURSOR-SW                 
10162             IF  PB-C-AH-CANCEL-AMT GREATER THAN ZEROS             
10163                 MOVE AL-UNBON        TO CREFND2A                  
10164                 MOVE -1              TO CREFND2L                  
10165                 MOVE  1              TO SET-CURSOR-SW             
10166                 GO TO 7499-EXIT                                   
10167             ELSE                                                  
10168                 GO TO 7499-EXIT                                   
10169         ELSE                                                      
10170             IF  PB-C-AH-CANCEL-AMT GREATER THAN ZEROS             
10171                 MOVE AL-UNBON        TO CREFND2A                  
10172                 MOVE -1              TO CREFND2L                  
10173                 MOVE  1              TO SET-CURSOR-SW             
10174             ELSE                                                  
10175                 GO TO 7499-EXIT.                                  
10176                                                                   
10177 **2757**                                                          
10178                                                                   
10179      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2757                    
10180         MOVE AL-UNBON            TO CLIVESA                       
10181         MOVE -1                  TO CLIVESL                       
10182         MOVE  1                  TO SET-CURSOR-SW                 
10183         GO TO 7499-EXIT.                                          
10184                                                                   
10185 **2771**                                                          
10186                                                                   
10187      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2771                    
10188         MOVE AL-UABON            TO CCANDT2A                      
10189         MOVE -1                  TO CCANDT2L                      
10190         MOVE  1                  TO SET-CURSOR-SW                 
10191         GO TO 7499-EXIT.                                          
10192                                                                   
10193 **2774**                                                          
10194                                                                   
10195      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2774                    
10196         MOVE AL-UABON            TO CCANDT1A                      
10197         MOVE -1                  TO CCANDT1L                      
10198         MOVE  1                  TO SET-CURSOR-SW                 
10199         GO TO 7499-EXIT.                                          
10200                                                                   
10201 **2775**                                                          
10202                                                                   
10203      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2775                    
10204         MOVE AL-UABON            TO CCANDT2A                      
10205         MOVE -1                  TO CCANDT2L                      
10206         MOVE  1                  TO SET-CURSOR-SW                 
10207         GO TO 7499-EXIT.                                          
10208                                                                   
10209 **2776**                                                          
10210                                                                   
10211      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2776                    
10212         MOVE AL-UABON            TO CCANDT1A                      
10213         MOVE -1                  TO CCANDT1L                      
10214         MOVE  1                  TO SET-CURSOR-SW                 
10215         GO TO 7499-EXIT.                                          
10216                                                                   
10217                                                                   
10218 **2777**                                                          
10219                                                                   
10220      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2777                    
10221         MOVE AL-UABON            TO CCANDT2A                      
10222         MOVE -1                  TO CCANDT2L                      
10223         MOVE  1                  TO SET-CURSOR-SW                 
10224         GO TO 7499-EXIT.                                          
10225                                                                   
10226 **2794**                                                          
10227                                                                   
10228      IF PB-COMMON-ERROR    (WS-SUB4) = ER-2794                    
10229         MOVE AL-UABON            TO CFORCEA                       
10230         MOVE -1                  TO CFORCEL                       
10231         MOVE  1                  TO SET-CURSOR-SW                 
10232         GO TO 7499-EXIT.                                          
10233                                                                   
10234 **2799**                                                          
10235                                                                   
10236      IF PI-COMPANY-ID = 'UCL'                                     
10237          IF PB-COMMON-ERROR    (WS-SUB4) = ER-2799                
10238              MOVE AL-UNBON            TO BEFFDTA                  
10239              MOVE -1                  TO BEFFDTL                  
10240              MOVE  1                  TO SET-CURSOR-SW            
10241              GO TO 7499-EXIT.                                     
10242                                                                   
10243  7499-EXIT.                                                       
10244       EXIT.                                                       
10245                                                                   
10246      EJECT                                                        
10247                                                                   
10248  7500-READ-ERLOFC.                                                
10249      MOVE SPACES                 TO  WS-LOAN-OFFICER              
10250                                      ERLOFC-KEY.                  
10251                                                                   
10252      MOVE PI-COMPANY-CD          TO  ERLOFC-COMPANY-CD.           
10253                                                                   
10254      IF ST-ACCNT-CNTL                                             
10255          MOVE PI-SV-STATE        TO  ERLOFC-STATE.                
10256                                                                   
10257      IF CARR-GROUP-ST-ACCNT-CNTL                                  
10258          MOVE PI-SV-CARRIER      TO  ERLOFC-CARRIER               
10259          MOVE PI-SV-STATE        TO  ERLOFC-STATE                 
10260          MOVE PI-SV-GROUPING     TO  ERLOFC-GROUPING.             
10261                                                                   
10262      IF CARR-ST-ACCNT-CNTL                                        
10263          MOVE PI-SV-CARRIER      TO  ERLOFC-CARRIER               
10264          MOVE PI-SV-STATE        TO  ERLOFC-STATE.                
10265                                                                   
10266      IF CARR-ACCNT-CNTL                                           
10267          MOVE PI-SV-CARRIER      TO  ERLOFC-CARRIER.              
10268                                                                   
10269      MOVE PI-PB-ACCOUNT          TO  ERLOFC-ACCOUNT.              
10270                                                                   
10271      MOVE BLNOFCRI               TO ERLOFC-OFFICER-CODE.          
10272                                                                   
10273      EXEC CICS HANDLE CONDITION                                   
10274          NOTFND   (7550-OFFICER-NOTFND)                           
10275      END-EXEC.                                                    
10276                                                                   
10277      EXEC CICS READ                                               
10278          DATASET   (FILE-ID-ERLOFC)                               
10279          SET       (ADDRESS OF LOAN-OFFICER-MASTER)               
10280          RIDFLD    (ERLOFC-KEY)                                   
10281          EQUAL                                                    
10282      END-EXEC.                                                    
10283                                                                   
10284      GO TO 7599-EXIT.                                             
10285                                                                   
10286  7550-OFFICER-NOTFND.                                             
10287      MOVE 'X'                    TO WS-LOAN-OFFICER.              
10288                                                                   
10289  7599-EXIT.                                                       
10290       EXIT.                                                       
10291      EJECT                                                        
10292 ******************************************************************
10293 *                                                                *
10294 *            S  E N D    I N I T I A L   M A P                   *
10295 *                                                                *
10296 ******************************************************************
10297                                                                   
10298  8100-SEND-INITIAL-MAP.                                           
10299      MOVE EIBTIME                TO TIME-IN.                      
10300                                                                   
10301      IF EIBAID = DFHPF11                                          
10302         MOVE 'Y'                 TO EMI-ROLL-SWITCH               
10303         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 
10304                                                                   
10305                                                                   
10306      IF PI-COMPANY-ID = CLIENT-GIC                                
10307         MOVE AL-UNNOF               TO BCPREM-ATTRB (1)           
10308                                        BCPREM-ATTRB (2).          
10309                                                                   
10310      IF PI-COMPANY-ID = (CLIENT-MIC OR                            
10311                          CLIENT-PEM OR                            
10312                          CLIENT-CSO)                              
10313         AND PI-DISPLAY-ORIGINAL-BATCH                             
10314           IF PI-MAP-NAME = VP631B                                 
10315              MOVE AL-SADOF               TO BPF5HDRA              
10316                                             BPF4HDRA              
10317           ELSE                                                    
10318              IF PI-MAP-NAME = VP631C                              
10319                 MOVE AL-SADOF            TO CPF5HDRA              
10320                                             CPF4HDRA.             
10321                                                                   
090612     IF PI-MAP-NAME = VP631B                                 
090612        MOVE AL-SADOF               TO BPF5HDRA
090612     ELSE
090612        MOVE AL-SADOF               TO CPF5HDRA
090612     END-IF
10322                                                                   
10329                                                                   
10330      IF PI-PROCESSOR-ID = 'LGXX'                                  
10331         IF PI-MAP-NAME = VP631B                                   
10332            MOVE AL-SABOF                    TO BREFHDGA           
10333            MOVE AL-SANOF                    TO BREFA              
10334         ELSE                                                      
10335            IF PI-MAP-NAME = VP631C                                
10336               MOVE AL-SABOF                 TO CREFHDGA           
10337               MOVE AL-SANOF                 TO CREFA.             
10338                                                                   
10340      IF PI-COMPANY-ID = 'ACE' or 'LGX' or 'DCC' or 'VPP'
10341         continue
10342      ELSE
10343         IF PI-MAP-NAME = VP631C
10344            MOVE AL-SADOF         TO CCANTXTA                   
10345                                     CCANFEEA
              end-if
           end-if

10347      IF PI-MAP-NAME = VP631D                                      
10348         IF PI-AR-PROCESSING                                       
10349            MOVE AL-SANOF            TO DREFHDGA                   
10350         ELSE                                                      
10351            MOVE AL-SADOF            TO DREFHDGA.                  
10352                                                                   
081606     IF (PI-COMPANY-ID NOT = 'DCC' and 'VPP' and 'CID')
081606        AND (PI-MAP-NAME = VP631B)
081606        MOVE AL-SADOF            TO BVINHA
081606                                    BVINA
081606     END-IF

10353      IF PI-MAP-NAME = VP631B                                      
              MOVE PI-COMPANY-ID          TO BCOMPIDO
              MOVE FUNCTION UPPER-CASE(WS-KIX-MYENV)
                                       TO BREGIONO
              MOVE PI-PROCESSOR-ID     TO BUSERO
              IF CSR-EDIT-SESSION
                 MOVE '- CUSTOMER SERVICE REVIEW/CORRECTION -'
                                       TO BHEADO
                 MOVE '4=B-HDR 6=RES'  TO BPF4HDRO
              ELSE
                 MOVE AL-SADOF         TO BF15HDRA
090612           MOVE '4=B-HDR      '  TO BPF4HDRO
              END-IF
10354         MOVE PI-MEMBER-CAPTION      TO BMEMCAPO                   
10355         MOVE PI-LIFE-OVERRIDE-L2    TO BKIND1O                    
10356         MOVE PI-AH-OVERRIDE-L2      TO BKIND2O                    
10357         MOVE WS-CURRENT-DT          TO BDATEO                     
10358         MOVE TIME-OUT               TO BTIMEO                     
10359         MOVE EMI-MESSAGE-AREA (1)   TO BERMSG1O                   
10360         MOVE EMI-MESSAGE-AREA (2)   TO BERMSG2O                   
10361         MOVE EMI-MESSAGE-AREA (3)   TO BERMSG3O                   
10362         IF   WS-DISPLAY-SCREEN                                    
10363              MOVE PB-ENTRY-BATCH    TO BBATCHO                    
10364              MOVE PB-SV-CARRIER     TO BCARRO                     
10365              MOVE PB-SV-GROUPING    TO BGROUPO                    
10366              MOVE PB-SV-STATE       TO BSTATEO                    
10367              MOVE PB-ACCOUNT        TO BACCTO                     
10368         ELSE                                                      
10369              MOVE PI-PB-ENTRY-BATCH TO BBATCHO                    
10370      ELSE                                                         
10371         IF PI-MAP-NAME = VP631C                                   
090612           MOVE '4=B-HDR      '     TO CPF4HDRO
                 IF CSR-EDIT-SESSION
                    MOVE '- CUSTOMER SERVICE REVIEW/CORRECTION -'
                                       TO CHEADO
                 ELSE
                    MOVE AL-SADOF      TO CF15HDRA
                 END-IF
10372            MOVE PI-PB-ENTRY-BATCH      TO CBATCHO                 
10373            MOVE WS-CURRENT-DT          TO CDATEO                  
10374            MOVE TIME-OUT               TO CTIMEO                  
10375            MOVE PI-LIFE-OVERRIDE-L2    TO CKIND1O                 
10376            MOVE PI-AH-OVERRIDE-L2      TO CKIND2O                 
10377            MOVE PI-LIFE-OVERRIDE-L2    TO WS-REF-HDG-OVERRIDE     
10378            MOVE WS-C-HDG               TO CLFHDGO                 
10379            MOVE PI-AH-OVERRIDE-L2      TO WS-REF-HDG-OVERRIDE     
10380            MOVE WS-C-HDG               TO CAHHDGO                 
10381            MOVE EMI-MESSAGE-AREA (1)   TO CERMSG1O                
10382            MOVE EMI-MESSAGE-AREA (2)   TO CERMSG2O                
10383            MOVE EMI-MESSAGE-AREA (3)   TO CERMSG3O                
10384            IF   WS-DISPLAY-SCREEN                                 
10385                 MOVE PB-ENTRY-BATCH    TO CBATCHO                 
10386                 MOVE PB-SV-CARRIER     TO CCARRO                  
10387                 MOVE PB-SV-GROUPING    TO CGROUPO                 
10388                 MOVE PB-SV-STATE       TO CSTATEO                 
10389                 MOVE PB-ACCOUNT        TO CACCTO                  
10390            ELSE                                                   
10391                 MOVE PI-PB-ENTRY-BATCH TO CBATCHO                 
10392         ELSE                                                      
10393            MOVE WS-CURRENT-DT          TO DDATEO                  
10394            MOVE TIME-OUT               TO DTIMEO                  
10395            MOVE PI-LIFE-OVERRIDE-L2    TO WS-PREM-HDG-OVERRIDE    
10396                                           WS-REFUND-HDG-OVERRIDE  
10397            MOVE WS-D-PREM-HDG          TO DLFPHDGO                
10398            MOVE WS-D-REFUND-HDG        TO DLFRHDGO                
10399            MOVE PI-AH-OVERRIDE-L2      TO WS-PREM-HDG-OVERRIDE    
10400                                           WS-REFUND-HDG-OVERRIDE  
10401            MOVE WS-D-PREM-HDG          TO DAHPHDGO                
10402            MOVE WS-D-REFUND-HDG        TO DAHRHDGO                
10403            MOVE EMI-MESSAGE-AREA (1)   TO DERMSGO                 
                 IF CSR-EDIT-SESSION
                    MOVE '- CUSTOMER SERVICE REVIEW/CORRECTION -'
                                       TO DHEADO
                 END-IF
10404            IF  WS-DISPLAY-SCREEN                                  
10405                MOVE PB-ENTRY-BATCH     TO DBATCHO                 
10406                MOVE PB-CARRIER         TO DCARRO                  
10407                MOVE PB-GROUPING        TO DGROUPO                 
10408                MOVE PB-STATE           TO DSTATEO                 
10409                MOVE PB-ACCOUNT         TO DACCTO                  
10410                MOVE PB-ACCOUNT-NAME    TO DACCTNMO                
10411            ELSE                                                   
10412                MOVE VP631D             TO PI-MAP-NAME             
10413                MOVE PI-PB-ENTRY-BATCH  TO DBATCHO                 
10414                MOVE PB-ACCOUNT-NAME TO DACCTNMO.                  
10415                                                                   
10416      MOVE SPACE                        TO WS-DISPLAY-SW.          
10417                                                                   
10418      EXEC CICS SEND                                               
10419          MAP      (PI-MAP-NAME)                                   
10420          MAPSET   (MAPSET-VP6311S)                                
10421          FROM     (VP631BI)                                       
10422          ERASE                                                    
10423          CURSOR                                                   
10424      END-EXEC.                                                    
10425                                                                   
10426      GO TO 9100-RETURN-TRAN.                                      
10427                                                                   
10428      EJECT                                                        
10429                                                                   
10430 ******************************************************************
10431 *                                                                *
10432 *              S E N D    D A T A O N L Y                        *
10433 *                                                                *
10434 ******************************************************************
10435                                                                   
10436  8200-SEND-DATAONLY.                                              
10437      MOVE EIBTIME                TO TIME-IN.                      
10438                                                                   
10439      IF PI-COMPANY-ID = CLIENT-GIC                                
10440         MOVE AL-UNNOF               TO BCPREM-ATTRB (1)           
10441                                        BCPREM-ATTRB (2).          
10442                                                                   
10443      IF PI-MAP-NAME = VP631B                                      
10444         MOVE PI-MEMBER-CAPTION      TO BMEMCAPO                   
10445         MOVE WS-CURRENT-DT          TO BDATEO                     
10446         MOVE TIME-OUT               TO BTIMEO                     
10447         MOVE EMI-MESSAGE-AREA (1)   TO BERMSG1O                   
10448         MOVE EMI-MESSAGE-AREA (2)   TO BERMSG2O                   
10449         MOVE EMI-MESSAGE-AREA (3)   TO BERMSG3O                   
10450         MOVE SPACES                 TO BPFENTRO                   
10451         MOVE AL-UANOF               TO BPFENTRA                   
10452                                                                   
10460      ELSE                                                         
10461         IF PI-MAP-NAME = VP631C                                   
10462            MOVE WS-CURRENT-DT          TO CDATEO                  
10463            MOVE TIME-OUT               TO CTIMEO                  
10464            MOVE EMI-MESSAGE-AREA (1)   TO CERMSG1O                
10465            MOVE EMI-MESSAGE-AREA (2)   TO CERMSG2O                
10466            MOVE EMI-MESSAGE-AREA (3)   TO CERMSG3O                
10467            MOVE SPACES                 TO CPFENTRO                
10468            MOVE AL-UANOF               TO CPFENTRA                
10469         ELSE                                                      
10470            MOVE WS-CURRENT-DT          TO DDATEO                  
10471            MOVE TIME-OUT               TO DTIMEO                  
10472            MOVE EMI-MESSAGE-AREA (1)   TO DERMSGO.                
10473                                                                   
10474      EXEC CICS SEND                                               
10475          MAP      (PI-MAP-NAME)                                   
10476          MAPSET   (MAPSET-VP6311S)                                
10477          FROM     (VP631BI)                                       
10478          DATAONLY                                                 
10479          CURSOR                                                   
10480      END-EXEC.                                                    
10481                                                                   
10482      GO TO 9100-RETURN-TRAN.                                      
10483                                                                   
10484      EJECT                                                        
10485                                                                   
10486 ******************************************************************
10487 *                                                                *
10488 *           U T I L I T Y   R O U T I N E S                      *
10489 *                                                                *
10490 ******************************************************************
10491                                                                   
10492  8300-SEND-TEXT.                                                  
10493      EXEC CICS SEND TEXT                                          
10494          FROM     (LOGOFF-TEXT)                                   
10495          LENGTH   (LOGOFF-LENGTH)                                 
10496          ERASE                                                    
10497          FREEKB                                                   
10498      END-EXEC.                                                    
10499                                                                   
10500      EXEC CICS RETURN                                             
10501      END-EXEC.                                                    
10502                                                                   
10503  8400-LOG-JOURNAL-RECORD.                                         
10504      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.                   
10505      MOVE THIS-PGM               TO JP-PROGRAM-ID.                
10506                                                                   
10507 *    EXEC CICS JOURNAL                                            
10508 *        JFILEID     (PI-JOURNAL-FILE-ID)                         
10509 *        JTYPEID     ('EL')                                       
10510 *        FROM        (JOURNAL-RECORD)                             
10511 *        LENGTH      (WS-JOURNAL-RECORD-LENGTH)                   
10512 *    END-EXEC.                                                    
10513                                                                   
10514  8600-DEEDIT.                                                     
10515      EXEC CICS BIF DEEDIT                                         
10516           FIELD   (DEEDIT-FIELD)                                  
10517           LENGTH  (15)                                            
10518      END-EXEC.                                                    
10519                                                                   
10520  8800-UNAUTHORIZED-ACCESS.                                        
10521      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   
10522      GO TO 8300-SEND-TEXT.                                        
10523                                                                   
10524  8810-PF23.                                                       
10525      MOVE EIBAID                 TO PI-ENTRY-CD-1.                
10526      MOVE XCTL-EL005             TO PGM-NAME.                     
10527      GO TO 9300-XCTL.                                             
10528                                                                   
10529  9100-RETURN-TRAN.                                                
10530      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             
10531                                                                   
10532      IF  PI-MAP-NAME =  VP631B                                    
10533          MOVE '631B'             TO PI-CURRENT-SCREEN-NO.         
10534                                                                   
10535      IF  PI-MAP-NAME =  VP631C                                    
10536          MOVE '631C'             TO PI-CURRENT-SCREEN-NO.         
10537                                                                   
10538      IF  PI-MAP-NAME =  VP631D                                    
10539          MOVE '631D'             TO PI-CURRENT-SCREEN-NO.         
10540                                                                   
10541      EXEC CICS RETURN                                             
10542          TRANSID    (TRANS-EXB1)                                  
10543          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     
10544          LENGTH     (1300)                                        
10545      END-EXEC.                                                    
10546                                                                   
10547  9200-RETURN-MAIN-MENU.                                           
10548      MOVE XCTL-EL626             TO PGM-NAME.                     
10549      GO TO 9300-XCTL.                                             
10550                                                                   
10551  9300-XCTL.                                                       
10552      EXEC CICS XCTL                                               
10553          PROGRAM    (PGM-NAME)                                    
10554          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     
10555          LENGTH     (PI-COMM-LENGTH)                              
10556      END-EXEC.                                                    
10557                                                                   
10558  9400-CLEAR.                                                      
10559      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.                     
10560      GO TO 9300-XCTL.                                             
10561                                                                   
10562  9500-PF12.                                                       
10563      MOVE XCTL-EL010             TO PGM-NAME.                     
10564      GO TO 9300-XCTL.                                             
10565                                                                   
10566  9600-PGMID-ERROR.                                                
10567      EXEC CICS HANDLE CONDITION                                   
10568          PGMIDERR    (8300-SEND-TEXT)                             
10569      END-EXEC.                                                    
10570                                                                   
10571      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.           
10572      MOVE ' '                    TO PI-ENTRY-CD-1.                
10573      MOVE XCTL-EL005             TO PGM-NAME.                     
10574      MOVE PGM-NAME               TO LOGOFF-PGM.                   
10575      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  
10576      GO TO 9300-XCTL.                                             
10577                                                                   
10578  9700-DATE-LINK.                                                  
10579      MOVE LINK-ELDATCV           TO PGM-NAME.                     
10580      EXEC CICS LINK                                               
10581          PROGRAM    (PGM-NAME)                                    
10582          COMMAREA   (DATE-CONVERSION-DATA)                        
10583          LENGTH     (DC-COMM-LENGTH)                              
10584      END-EXEC.                                                    
10585                                                                   
10586  9800-LINK-PENDING-EDIT.    
10587      MOVE PI-LIFE-OVERRIDE-L1    TO PB-LIFE-OVERRIDE-L1.          
10588      MOVE PI-AH-OVERRIDE-L1      TO PB-AH-OVERRIDE-L1.            
10589                                                                   
10590      MOVE LINK-EL050             TO PGM-NAME                      
10591      MOVE PENDING-BUSINESS       TO PASSED-RECORD                 
10592      MOVE ZEROS                  TO WK-CO-MAX-CAP                 
10593                                     WK-CO-TOL-CLAIM               
10594                                     WK-CO-TOL-PREM                
10595                                     WK-CO-TOL-REFUND              
10596                                     WK-CO-TOL-PREM-PCT            
10597                                     WK-CO-TOL-REFUND-PCT          
10598                                     WK-REFUND-OVS-AMT             
10599                                     WK-REFUND-OVS-PCT             
10600                                     WK-ACCT-ADDR                  
10601                                     WK-LIFE-EDIT-ADDR             
10602                                     WK-AH-EDIT-ADDR               
10603                                     WK-LIFE-BEN-ADDR              
10604                                     WK-AH-BEN-ADDR                
10605                                     WK-STATE-ADDR                 
10606                                     WK-PLAN-ADDR                  
10607                                     WK-FORM-ADDR
           MOVE PI-CSR-SESSION-SW      TO WK-CSR-SESSION-SW
10608                                                                   
10609      MOVE '6'                    TO WK-ENTRY-SW.                  
10610                                                                   
10611      MOVE SPACES                 TO EDIT-CRITERIA-DATA.           
10612                                                                   
10613      MOVE LOW-VALUES             TO EC-CO-MONTH-END-DT            
10614                                     EC-AM-EXPIRATION-DT           
10615                                     EC-AM-EFFECTIVE-DT            
10616                                     EC-CM-LF-CANCEL-DT            
10617                                     EC-CM-AH-CANCEL-DT            
10618                                     EC-CM-DEATH-DT.               
10619                                                                   
10620      MOVE ZEROS                  TO EC-CO-TOL-PREM                
10621                                     EC-CO-TOL-REFUND              
10622                                     EC-CO-MIN-AGE                 
10623                                     EC-CO-MIN-PREMIUM             
10624                                     EC-CO-MIN-TERM                
10625                                     EC-CO-MAX-TERM                
10626                                     EC-ST-TOL-PREM                
10627                                     EC-ST-TOL-REFUND              
10628                                     EC-AM-LF-TOL-PREM             
10629                                     EC-AM-AH-TOL-PREM             
10630                                     EC-AM-LF-MAX-ATT-AGE          
10631                                     EC-AM-LF-MAX-AGE              
10632                                     EC-AM-LF-MAX-TERM             
10633                                     EC-AM-LF-MAX-TOT-BEN          
10634                                     EC-AM-AH-MAX-ATT-AGE          
10635                                     EC-AM-AH-MAX-AGE              
10636                                     EC-AM-AH-MAX-TERM             
10637                                     EC-AM-AH-MAX-TOT-BEN          
10638                                     EC-AM-AH-MAX-MON-BEN          
10639                                     EC-RT-LF-MAX-ATT-AGE          
10640                                     EC-RT-LF-MAX-AGE              
10641                                     EC-RT-LF-MAX-TERM             
10642                                     EC-RT-LF-MAX-TOT-BEN          
10643                                     EC-RT-AH-MAX-ATT-AGE          
10644                                     EC-RT-AH-MAX-AGE              
10645                                     EC-RT-AH-MAX-TERM             
10646                                     EC-RT-AH-MAX-TOT-BEN          
10647                                     EC-RT-AH-MAX-MON-BEN          
10648                                     EC-RT-AH-RATE                 
10649                                     EC-RT-LF-RATE                 
10650                                     EC-RT-LF-NSP-RATE             
10651                                     EC-RT-AH-NSP-RATE             
10652                                     EC-CM-LF-PRIOR-REFUND         
10653                                     EC-CM-AH-PRIOR-REFUND.        
10654                                                                   
10655      MOVE ZEROS                  TO EC-CO-TOL-PREM-PCT            
10656                                     EC-ST-TOL-PREM-PCT            
10657                                     EC-AM-LF-TOL-REFUND           
10658                                     EC-AM-LF-DEV-PERCENT          
10659                                     EC-CO-TOL-REFUND-PCT          
10660                                     EC-ST-TOL-REFUND-PCT          
10661                                     EC-AM-AH-TOL-REFUND           
10662                                     EC-AM-AH-DEV-PERCENT.         
10663                                                                   
10664      EXEC CICS LINK                                               
10665           PROGRAM   (PGM-NAME)                                    
10666           COMMAREA  (PASSED-RECORD)                               
10667           LENGTH    (WS-EDIT-PASS-LENGTH)                         
10668      END-EXEC.                                                    
10669                                                                   
10670      MOVE '1'                    TO PI-EDIT-SW.                   
10671      MOVE EDIT-CRITERIA-DATA     TO PI-CRITERIA-DATA.             
10672                                                                   
10673      MOVE PASSED-RECORD          TO PENDING-BUSINESS.             
10674                                                                   
10675      IF PI-COMPANY-ID = 'UCL'                                     
10676          MOVE EC-AM-EFFECTIVE-DT TO WS-CERT-EFF-DT.               
10677                                                                   
10678      IF VALID-PB-ID                                               
10679         GO TO 9800-EXIT.                                          
10680                                                                   
10681      MOVE PENDING-BUSINESS       TO EMI-LINE1.                    
10682                                                                   
10683      IF PI-MAP-NAME = VP631B                                      
10684         MOVE -1                  TO BMAINTL                       
10685        ELSE                                                       
10686         MOVE -1                  TO CMAINTL.                      
10687                                                                   
10688      GO TO 8200-SEND-DATAONLY.                                    
10689                                                                   
10690  9800-EXIT.                                                       
10691       EXIT.                                                       
10692                                                                   
10693  9900-ERROR-FORMAT.                                               
10694                                                                   
10695      MOVE PI-COMPANY-ID TO EMI-CLIENT-ID.                         
10696                                                                   
10697      IF NOT EMI-ERRORS-COMPLETE                                   
10698          MOVE LINK-EL001         TO PGM-NAME                      
10699          EXEC CICS LINK                                           
10700              PROGRAM    (PGM-NAME)                                
10701              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           
10702              LENGTH     (EMI-COMM-LENGTH)                         
10703          END-EXEC.                                                
10704                                                                   
10705  9900-EXIT.                                                       
10706      EXIT.                                                        
10707                                                                   
10708  9990-ABEND.                                                      
10709      MOVE LINK-EL004             TO PGM-NAME.                     
10710      MOVE DFHEIBLK               TO EMI-LINE1.                    
10711      EXEC CICS LINK                                               
10712          PROGRAM   (PGM-NAME)                                     
10713          COMMAREA  (EMI-LINE1)                                    
10714          LENGTH    (72)                                           
10715      END-EXEC.                                                    
10716                                                                   
10717      IF PI-MAP-NAME = VP631B                                      
10718         MOVE -1                  TO BMAINTL                       
10719        ELSE                                                       
10720         MOVE -1                  TO CMAINTL.                      
10721                                                                   
10722      IF EIBTRNID NOT = TRANS-EXB1                                 
10723         GO TO 8100-SEND-INITIAL-MAP                               
10724        ELSE                                                       
10725         GO TO 8200-SEND-DATAONLY.                                 
10726                                                                   
10727  9995-SECURITY-VIOLATION.                                         
10728                              COPY ELCSCTP.                        
