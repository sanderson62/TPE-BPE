00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                 EL152 .                              
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 06/14/94 08:03:40.                 
00007 *                            VMOD=2.055.                          
00008 *                                                                 
00009 *AUTHOR.     LOGIC,INC.                                           
00010 *            DALLAS, TEXAS.                                       
00011                                                                   
00024 *REMARKS.    TRANSACTION - EX27 - CLAIMS LETTER WRITER.           
00025                                                                   
121802******************************************************************
121802*                   C H A N G E   L O G
121802*
121802* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
121802*-----------------------------------------------------------------
121802*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
121802* EFFECTIVE    NUMBER
121802*-----------------------------------------------------------------
121802* 121802    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYPE I
121802*                                REMOVE OBSOLETE CODE
102703* 102703                   SMVA  MOD COPY MEMBER ELCNAMET TO LEAVE
102703*                                CERTIFICATION DESIGNATION ON 
102703*                                PROCESSOR NAME LINE IN CAPS
121203* 121203                   SMVA  ADD PROCESSING FOR NEW CLM TYP G
081004* 081004                   PEMA  CONVERT TO PSUEDO CONVERSATIONAL
042604* 042605    2005042100002  PEMA  FIX TEMP STORAGE PROBLEMS
010407* 010407    2006111300003  PEMA  ADD PROCESSING FOR CARRIER 8
060109* 060109  CR2008102800002  PEMA  ADD VAR 16.1
033110* 033110  CR2009122800001  AJRA  NAPERSOFT
040110* 040110  CR2009070600002  AJRA  ADD RESEND LETTER ID TO LETTER
011212* 011212  IR2012011100002  AJRA  FIX DCC PROMPT LETTER 
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
041513* 041513  CR2013011500003  AJRA  VALIDATE ENC CODE AGAINST ELENCC TBL
052814* 052814  CR2014012300001  PEMA  DCC CREDIT UNION CHANGES
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
080322* 080322  CR2021100800003  TANA  Add B and H claim types
121802******************************************************************
00026  ENVIRONMENT DIVISION.                                            
00027                                                                   
00028      EJECT                                                        
00029  DATA DIVISION.                                                   
00030  WORKING-STORAGE SECTION.                                         
00031  77  FILLER  PIC X(32)  VALUE '********************************'. 
00032  77  FILLER  PIC X(32)  VALUE '*    EL152 WORKING STORAGE     *'. 
00033  77  FILLER  PIC X(32)  VALUE '********** VMOD=2.055 **********'. 
00034                                                                   
pemuni*77  LCP-WS-ADDR-COMP              PIC S9(8) COMP.
pemuni*77  LCP-WS-ADDR-PNTR REDEFINES LCP-WS-ADDR-COMP
pemuni*                                  USAGE POINTER.
pemuni 77  LCP-WS-ADDR-COMP              PIC x(4) comp-5 value 0.
pemuni 77  LCP-WS-ADDR-PNTR REDEFINES LCP-WS-ADDR-COMP
pemuni                                   USAGE POINTER.
       77  B1                          PIC S9(5) COMP-3 VALUE +0.
       77  S1                          PIC S9 VALUE +0.
       77  S2                          PIC S9 VALUE +0.


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

CIDMOD 01  WS-HOLDING-ZIPS.                                             
CIDMOD     05  FILLER              PIC X(10)  VALUE '--AM-ZIP--'.       
CIDMOD     05  WS-AM-ZIP           PIC X(9)   VALUE 'XXXXXXXXX'.        
CIDMOD     05  FILLER              PIC X(10)  VALUE '--AT-ZIP--'.       
CIDMOD     05  WS-AT-ZIP           PIC X(9)   VALUE 'XXXXXXXXX'.        
CIDMOD     05  FILLER              PIC X(10)  VALUE '--BE-ZIP--'.       
CIDMOD     05  WS-BE-ZIP-CODE      PIC X(9)   VALUE 'XXXXXXXXX'.        
CIDMOD     05  FILLER              PIC X(10)  VALUE '--CO-ZIP--'.       
CIDMOD     05  WS-CO-ZIP           PIC X(9)   VALUE 'XXXXXXXXX'.        
CIDMOD     05  FILLER              PIC X(17)  VALUE '--AM-ZIP-BEFORE--'.
CIDMOD     05  WS-AM-ZIP-BEFORE    PIC X(9)   VALUE 'XXXXXXXXX'.        
CIDMOD     05  FILLER              PIC X(17)  VALUE '--AT-ZIP-BEFORE--'.
CIDMOD     05  WS-AT-ZIP-BEFORE    PIC X(9)   VALUE 'XXXXXXXXX'.        
CIDMOD     05  FILLER              PIC X(17)  VALUE '--BE-ZIP-BEFORE--'.
CIDMOD     05  WS-BE-ZIP-BEFORE    PIC X(9)   VALUE 'XXXXXXXXX'.        
CIDMOD     05  FILLER              PIC X(17)  VALUE '--CO-ZIP-BEFORE--'.
CIDMOD     05  WS-CO-ZIP-BEFORE    PIC X(9)   VALUE 'XXXXXXXXX'.        
CIDMOD     05  FILLER              PIC X(17)  VALUE '-----------------'.
00038                                                                   
00039                              COPY ELCSCTM.                        
00040                                                                   
00041                              COPY ELCSCRTY.                       
00042                                                                   
00043  01  WS-DATE-AREA.                                                
00044      12  SAVE-DATE               PIC X(08)    VALUE SPACES.       
00045      12  SAVE-BIN-DATE           PIC XX       VALUE SPACES.       
00046      12  W-REVERSE-DATE-SW       PIC X        VALUE SPACES.       
00047          88  W-REVERSE-DATE                   VALUE 'Y'.          
00048      12  W-EDIT-DATE-1.                                           
00049          16  W-ED1-MM            PIC XX.                          
00050          16  FILLER              PIC X        VALUE '/'.          
00051          16  W-ED1-DD            PIC XX.                          
00052          16  FILLER              PIC X        VALUE '/'.          
00053          16  W-ED1-YY            PIC XX.                          
00054      12  W-EDIT-DATE-2.                                           
00055          16  W-ED2-DD            PIC XX.                          
00056          16  FILLER              PIC X        VALUE '/'.          
00057          16  W-ED2-MM            PIC XX.                          
00058          16  FILLER              PIC X        VALUE '/'.          
00059          16  W-ED2-YY            PIC XX.                          
       01  FILLER.
           05  WS-WORK-INT-RATE        PIC 99.999.
           05  WS-WORK-INT REDEFINES WS-WORK-INT-RATE
                                       PIC X(6).
       01  TRAN-DATA-LINE1             PIC X(80)    VALUE
           'BEGINJOB mode=''MVS'''.
       01  TRAN-DATA-LINE2.
033110*     05  FILLER                  PIC X(39)    VALUE
033110*     '"smtp -f EL152cl2 -t pema,kmsb,jmsb -s '.
033110     05  FILLER                  PIC X(29)    VALUE
033110     '"smtp -f EL152cl2 -t ajra -s '.
           05  TRAN-DETAIL.
               10  FILLER              PIC XX       VALUE
               ''' '.
               10  TRAN-DL2-USER       PIC X(05)    VALUE SPACES.
               10  TRAN-DL2-ARCHNO     PIC ZZZZ999  VALUE ZEROS.
               10  FILLER              PIC X        VALUE SPACES.
               10  TRAN-DL2-CCC        PIC X(22)    VALUE SPACES.
           05  FILLER                  PIC XX       VALUE '''"'.
           05  FILLER                  PIC X(30)    VALUE SPACES.
       01  TRAN-DATA-LINE3             PIC X(80)    VALUE
           'ENDJOB            '.
00060                                                                   
042605 01  WS-PRINTER-ID               PIC X(4)    VALUE SPACES.
00061  01  STANDARD-AREAS.                                              
00062      12  WS-ACCT-READ-SW         PIC X       VALUE ' '.           
00063      12  WS-COMP-READ-SW         PIC X       VALUE ' '.           
00064      12  WS-PROD-READ-SW         PIC X       VALUE ' '.           
00065      12  SC-ITEM                 PIC S9(4)   VALUE +1  COMP.      
00066      12  MAP-NAME.                                                
00067          16  MAP-PREFIX          PIC XX      VALUE 'EL'.          
00068          16  MAP-NUMBER          PIC X(4)    VALUE '152A'.        
00069          16  MAP-FILLER          PIC XX      VALUE SPACES.        
00070      12  GETMAIN-SPACE           PIC X       VALUE SPACE.         
00071      12  MAPSET-NAME             PIC X(8)    VALUE 'EL152S'.      
00072      12  TRANS-ID                PIC X(4)    VALUE 'EX27'.        
00073      12  PRINT-TRANS             PIC X(4)    VALUE 'EX57'.        
00074      12  LGXX-ID                 PIC X(4)    VALUE 'LGXX'.        
00075      12  PGM-NAME                PIC X(8).                        
00076      12  TIME-IN                 PIC S9(7).                       
00077      12  TIME-OUT-R  REDEFINES TIME-IN.                           
00078          16  FILLER              PIC X.                           
00079          16  TIME-OUT            PIC 99V99.                       
00080          16  FILLER              PIC XX.                          
00081      12  XCTL-005                PIC X(5)    VALUE 'EL005'.       
00082      12  XCTL-010                PIC X(5)    VALUE 'EL010'.       
00083      12  XCTL-126                PIC X(5)    VALUE 'EL126'.       
00084      12  LINK-001                PIC X(5)    VALUE 'EL001'.       
00085      12  LINK-004                PIC X(5)    VALUE 'EL004'.       
00086      12  LINK-ELDATCV            PIC X(7)    VALUE 'ELDATCV'.     
00087      12  THIS-PGM                PIC X(8)    VALUE 'EL152'.       
00088      12  LINK-EL1522             PIC X(8)    VALUE 'EL1522'.      
00089      12  PGM-EL126               PIC X(8)    VALUE 'EL126'.       
00090      12  PGM-EL150               PIC X(8)    VALUE 'EL150'.       
00091      12  PGM-EL141               PIC X(8)    VALUE 'EL141'.       
00092      12  PGM-EL1042              PIC X(8)    VALUE 'EL1042'.      
00093      12  SUB                     PIC 99.                          
00094      12  WS-LABELS-SW            PIC X       VALUE SPACE.         
00095                                                                   
00096      12  WS-PI-QID.                                               
00097          16  QID-TERM            PIC X(4)    VALUE SPACES.        
00098          16  FILLER              PIC X(4)    VALUE '152A'.        
00099                                                                   
00100      12  W-LETTER-ADDRESS-TYPE.                                   
00101          16  W-LETTER-ADDR-TYPE  PIC  X VALUE SPACES.             
00102          16  W-LETTER-ADDR-SEQ   PIC  9 VALUE ZEROS.              
00103                                                                   
00104      12  W-ADDRESS-SELECTION.                                     
00105          16  W-ACCOUNT           PIC  9 VALUE ZEROS.              
00106          16  W-BENEFICIARY       PIC  9 VALUE ZEROS.              
00107          16  W-EMPLOYER          PIC  9 VALUE ZEROS.              
00108          16  W-INSURED           PIC  9 VALUE ZEROS.              
00109          16  W-OTHER-1           PIC  9 VALUE ZEROS.              
00110          16  W-OTHER-2           PIC  9 VALUE ZEROS.              
00111          16  W-PHYSICIAN         PIC  9 VALUE ZEROS.              
00112                                                                   
00113      12  W-Z-CONTROL-DATA.                                        
00114          16  W-NUMBER-OF-COPIES  PIC  9.                          
00115          16  FILLER              PIC  X.                          
00116          16  W-DAYS-TO-FOLLOW-UP PIC  999.                        
00117          16  FILLER              PIC  X.                          
00118          16  W-DAYS-TO-RESEND-1  PIC  999.                        
00119          16  FILLER              PIC  X.                          
040110         16  W-FORM-TO-RESEND    PIC  X(4).
040110         16  FILLER              PIC  X(1).
040110         16  W-PROMPT-LETTER     PIC  X(1).
040110         16  FILLER              PIC  X(1).
040110         16  W-ENCLOSURE-CD      PIC  X(3).
040110         16  FILLER              PIC  X(1).
040110         16  W-AUTO-CLOSE-IND    PIC  X(1).
040110         16  FILLER              PIC  X(1).
040110         16  W-LETTER-TO-BENE    PIC  X(1).                        
00123                                                                   
00124      12  W-CREDIT-CARD-LOAN-NO.                                   
00125          16  W-LOAN-NO           PIC  X(08).                      
00126          16  W-CURRENT-LOAN-NO   PIC  X(12).                      
00127                                                                   
00128      12  W-GROUPING.                                              
00129          16  W-GROUP-3           PIC  XXX.                        
00130          16  FILLER              PIC  XXX.                        
00131                                                                   
00132      12  W-NAME.                                                  
00133          16  W-FIRST-NAME        PIC  X(12).                      
00134          16  W-MIDDLE-NAME       PIC  X(12).                      
00135          16  W-LAST-NAME         PIC  X(15).                      
00136                                                                   
00137      12  WS-PHONE-IN             PIC 9(11)   VALUE ZEROS.         
00138      12  WS-PHONE-IN-R  REDEFINES WS-PHONE-IN.                    
00139          16  FILLER              PIC 9.                           
00140          16  WSPI-AREA           PIC 9(3).                        
00141          16  WSPI-PFX            PIC 9(3).                        
00142          16  WSPI-SFX            PIC 9(4).                        
00143      12  WS-PHONE-OUT.                                            
00144          16  WSPO-AREA           PIC X(3).                        
00145          16  FILLER              PIC X       VALUE '-'.           
00146          16  WSPO-PFX            PIC X(3).                        
00147          16  FILLER              PIC X       VALUE '-'.           
00148          16  WSPO-SFX            PIC X(4).                        
00149                                                                   
00150      12  WS-ZIP-NUMERIC          PIC 9(9).                        
00151      12  WS-ZIP-NONNUM  REDEFINES  WS-ZIP-NUMERIC                 
00152                                  PIC X(9).                        
00153                                                                   
00154      12  WS-ZIP-CODE.                                             
00155          16  WS-AM-ZIP-CODE      PIC X(5).                        
00156          16  WS-AM-ZIP-DASH      PIC X.                           
00157          16  WS-AM-ZIP-PLUS4     PIC X(4).                        
00158      12  WS-ZIP-CODE-CANADIAN  REDEFINES  WS-ZIP-CODE.            
00159          16  WS-CAN-POSTAL-1     PIC XXX.                         
00160          16  FILLER              PIC X.                           
00161          16  WS-CAN-POSTAL-2     PIC XXX.                         
00162          16  FILLER              PIC XXX.                         
00163                                                                   
00164      12  WS-LABEL-HOLD-AREA.                                      
00165          16  WS-LABEL-LINES OCCURS 6 TIMES                        
00166                             INDEXED BY WS-NDX  WS-NDX2.           
00167              20  WS-LABEL-ZIP.                                    
00168                  24  WS-LABEL-1ST-ZIP  PIC X(5).                  
00169                  24  FILLER            PIC X.                     
00170                  24  WS-LABEL-2ND-ZIP  PIC X(4).                  
00171              20  FILLER                PIC X(9).                  
00172              20  WS-LAST-DIGIT         PIC X.                     
00173              20  WS-LAST-ZIP.                                     
00174                  24  WS-LAST-1ST-ZIP   PIC X(5).                  
00175                  24  FILLER            PIC X.                     
00176                  24  WS-LAST-2ND-ZIP   PIC X(4).                  
00177                                                                   
00178      12  WS-DATA-FOUND-SW            PIC X.                       
00179          88  NO-CHARACTERS-FOUND           VALUE 'N'.             
00180                                                                   
033110     12  WS-SKIP-EMAIL           PIC X       VALUE 'N'.
00181      12  WS-STATE-LINE           PIC X       VALUE 'N'.           
102703     12  WS-PROCESSOR-LINE       PIC X(01)   VALUE 'N'.           
102703     12  WS-CAPS-SW              PIC X(01)   VALUE 'N'.
102703         88  THE-REST-R-CAPS                 VALUE 'Y'.
00182      12  WS-POSITION2            PIC S9(4)   COMP.                
00183      12  WS-POSITION21           PIC S9(4)   COMP.                
00184      12  WS-WORD-LENGTH          PIC S9(4)   COMP-3.              
00185                                                                   
00186      12  WS-TEMP-AREA1.                                           
00187          16  WS-TEMP-1           PIC X OCCURS 29                  
00188                                        INDEXED BY TA1.            
00189      12  WS-TEMP-AREA2.                                           
00190          16  WS-TEMP-2           PIC X OCCURS 30                  
00191                                        INDEXED BY TA2             
00192                                                   TA21            
00193                                                   MOVE-INDX.      
00194                                                                   
00195      12  WS-SAVE-TEMP-AREA2      PIC X(30)   VALUE SPACES.        
00196                                                                   
00197      12  ACCT-BROWSE-STARTED     PIC X       VALUE 'N'.           
00198      12  TEXT-BROWSE-STARTED     PIC X       VALUE 'N'.           
00199      12  ARCH-BROWSE-STARTED     PIC X       VALUE 'N'.           
00200      12  ACTV-BROWSE-STARTED     PIC X       VALUE 'N'.           
00201      12  PROD-BROWSE-STARTED     PIC X       VALUE 'N'.           
00202      12  INDX-WORK               PIC 99.                          
00203      12  TEMP-CURR-LINE          PIC S9(3)    COMP-3.             
00204                                                                   
00205      12  DATE-WORK               PIC 9(7).                        
00206      12  DT-REDEF REDEFINES DATE-WORK.                            
00207          16  FILLER              PIC XX.                          
00208          16  DT-WORK             PIC 9(5).                        
00209                                                                   
00210      12  BEN-HOLD                PIC XX.                          
00211                                                                   
00212      12  BENEFIT-WORK            PIC XXX.                         
00213      12  BEN-R REDEFINES BENEFIT-WORK.                            
00214          16  ELIM-DAYS           PIC XX.                          
00215          16  FILLER              PIC X.                           
00216                                                                   
00217      12  GETMAIN-SWITCH          PIC 9        VALUE 0.            
00218          88  NO-GETMAIN-DONE-YET              VALUE 0.            
00219          88  REFRESH-GETMAIN-AREA             VALUE 1.            
00220                                                                   
00221      12  WORK-AMOUNT             PIC S9(9)V99 VALUE +0.           
00222      12  CURRENT-SAVE            PIC XX.                          
00223      12  CURRENT-PLUS3-SAVE      PIC XX.                          
00224      12  RESEND-SAVE             PIC XX    VALUE LOW-VALUE.       
00225      12  FOLLOW-UP-SAVE          PIC XX    VALUE LOW-VALUE.       
00226      12  SEQ-COUNTER             PIC S9(4) COMP.                  
00227      12  CORR-TRLR-SEQ           PIC S9(4) COMP.                  
00228      12  DEEDIT-FIELD            PIC X(15).                       
00229      12  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD   PIC S9(15).    
00230                                                                   
00231      12  ARCH-SUPPRESS           PIC ZZZZZZZZ99.                  
00232      12  ARCH-EDIT REDEFINES ARCH-SUPPRESS    PIC X(10).          
00233                                                                   
00234      12  LOWER-CASE PIC X(26) VALUE 'abcdefghijklmnopqrstuvwxyz'. 
00235      12  UPPER-CASE PIC X(26) VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'. 
00236                                                                   
00237      12  WS-CCN-12               PIC X(12).                       
00238                                                                   
00239      12  WS-ADDR-TYPE-CD.                                         
00240          16  WS-ADDR-TYPE        PIC X.                           
00241          16  WS-ADDR-SEQ         PIC X.                           
00242          16  WS-ADDR-SEQ-NUM REDEFINES                            
00243              WS-ADDR-SEQ         PIC 9.                           
00244                                                                   
00245      12  WS-DMD-CERT-STATE       PIC XX.                          
00246      12  WS-DMD-CERT-GROUPING    PIC XX.                          
00247      12  WS-DMD-BEN-CODE         PIC XX.                          
00248      12  WS-DMD-CORR-TRLR-SEQ    PIC S9(4) COMP.                  
00249      12  WS-DMD-LETTER-FORM      PIC X(4).                        
00250      12  WS-DMD-RES-ST           PIC XX.                          
00251                                                                   
00252      12  WS-DMD-UND-STATEMENT    PIC X(50)  VALUE                 
00253           'Central States provides plan administration for'.      
00254                                                                   
00255      12  WS-DMD-UND-COMPANYA     PIC X(50)  VALUE                 
00256           'Central States Health and Life Co.'.                   
00257                                                                   
00258      12  WS-DMD-UND-COMPANYB     PIC X(50)  VALUE                 
00259           'Central States Indemnity Co.'.                         
00260                                                                   
00261 * DLO023                                                          
00262  01  DL23-COMM-LENGTH            PIC S9(4) COMP VALUE +132.       
00263  01  WS-DLO-CODES-TABLE.                                          
00264      12  DL23-SYSTEM-ID          PIC XX.                          
00265      12  DL23-RECORD-TYPE        PIC XX.                          
00266      12  DL23-RECORD-KEY         PIC X(6).                        
00267      12  DL23-RETURN-CODE        PIC XX.                          
00268      12  DL23-CODE-DESC          PIC X(60).                       
00269      12  DL23-GEN-DESC-1         PIC X(20).                       
00270      12  DL23-GEN-DESC-2         PIC X(20).                       
00271      12  DL23-GEN-DESC-3         PIC X(20).                       
CIDMOD     12  WS-LETTER-STATUS        PIC X        VALUE ' '.          
CIDMOD     12  WS-BLANK                PIC X        VALUE ' '.          
00272                                                                   
00273  01  WS-SAVE-ACCT-RECORD         PIC X(2000)  VALUE SPACES.       
00274  01  WS-SAVE-PRODUCER-RECORD     PIC X(2000)  VALUE SPACES.       
00275                                                                   
00276      EJECT                                                        
00277  01  ACCESS-KEYS-AND-FILE-IDS.                                    
00278      12  CNTL-ID                  PIC X(8)    VALUE 'ELCNTL'.     
00279      12  ACCT-ID                  PIC X(8)    VALUE 'ERACCT'.     
00280      12  CERT-ID                  PIC X(8)    VALUE 'ELCERT'.     
00281      12  ARCH-ID                  PIC X(8)    VALUE 'ELARCH'.     
00282      12  ARCT-ID                  PIC X(8)    VALUE 'ELARCT'.     
00283      12  CLAM-ID                  PIC X(8)    VALUE 'ELMSTR'.     
00284      12  TEXT-ID                  PIC X(8)    VALUE 'ELLETR'.     
00285      12  ACTV-ID                  PIC X(8)    VALUE 'ELTRLR'.     
00286      12  BENE-ID                  PIC X(8)    VALUE 'ELBENE'.     
00287      12  PROD-ID                  PIC X(8)    VALUE 'MPPROD'.     
00288      12  PLCY-ID                  PIC X(8)    VALUE 'MPPLCY'.     
00289      12  PLAN-ID                  PIC X(8)    VALUE 'MPPLAN'.     
033110     12  NAPS-ID                  PIC X(8)    VALUE 'ELNAPS'.
041513     12  ENCC-ID                  PIC X(8)    VALUE 'ELENCC'.
00290                                                                   
00291      12  CNTL-KEY.                                                
00292          16  CNTL-CO              PIC X(3).                       
00293          16  CNTL-RECORD-TYPE     PIC X       VALUE '1'.          
00294          16  CNTL-GENL.                                           
00295            18 CNTL-GEN1           PIC XX      VALUE SPACES.       
00296            18 CNTL-GEN2.                                          
00297              20 CNTL-GEN3         PIC X       VALUE SPACES.       
00298              20 CNTL-GEN4         PIC X       VALUE SPACES.       
00299          16  CNTL-SEQ             PIC S9(4)   VALUE +0    COMP.   
00300                                                                   
00301       12  WS-ERCOMP-KEY.                                          
00302           16  WS-ERCOMP-COMPANY-CD PIC X.                         
00303           16  WS-ERCOMP-CARRIER    PIC X.                         
00304           16  WS-ERCOMP-GROUPING   PIC X(6).                      
00305           16  WS-ERCOMP-RESP-NO    PIC X(10).                     
00306           16  WS-ERCOMP-ACCOUNT    PIC X(10).                     
00307           16  WS-ERCOMP-TYPE       PIC X.                         
00308                                                                   
00309      12  CLAM-KEY.                                                
00310          16  CLAM-CO              PIC X.                          
00311          16  CLAM-CARRIER         PIC X.                          
00312          16  CLAM-CLAIM           PIC X(7).                       
00313          16  CLAM-CERT-NUM        PIC X(11).                      
00314                                                                   
00315      12  TEXT-KEY.                                                
00316          16  TEXT-PARTIAL-KEY.                                    
00317              20  TEXT-CO          PIC X.                          
00318              20  TEXT-LETTER      PIC X(4).                       
00319          16  TEXT-FILLER          PIC X(8)    VALUE SPACES.       
00320          16  TEXT-SEQ             PIC S9(4)   VALUE +0    COMP.   
00321                                                                   
00322      12  ACTV-KEY.                                                
00323          16  ACTV-PARTIAL-KEY.                                    
00324              20  ACTV-CO          PIC X.                          
00325              20  ACTV-CARRIER     PIC X.                          
00326              20  ACTV-CLAIM       PIC X(7).                       
00327              20  ACTV-CERT-NUM    PIC X(11).                      
00328          16  ACTV-SEQ             PIC S9(4)   VALUE +0    COMP.   
00329                                                                   
00330      12  CERT-KEY.                                                
00331          16  CERT-CO              PIC X.                          
00332          16  CERT-CARRIER         PIC X.                          
00333          16  CERT-GROUPING        PIC X(6).                       
00334          16  CERT-STATE           PIC XX.                         
00335          16  CERT-ACCOUNT         PIC X(10).                      
00336          16  CERT-EFF-DT          PIC XX.                         
00337          16  CERT-CERT-NUM        PIC X(11).                      
00338                                                                   
00339      12  ACCT-KEY.                                                
00340          16  ACCT-PARTIAL-KEY.                                    
00341              20  ACCT-CO              PIC X.                      
00342              20  ACCT-CARRIER         PIC X.                      
00343              20  ACCT-GROUPING        PIC X(6).                   
00344              20  ACCT-STATE           PIC XX.                     
00345              20  ACCT-ACCOUNT         PIC X(10).                  
00346          16  ACCT-EXP-DATE            PIC XX.                     
00347                                                                   
00348      12  ARCH-KEY.                                                
00349          16  ARCH-PARTIAL-KEY.                                    
00350              20  ARCH-CO          PIC X.                          
00351              20  ARCH-NUMBER      PIC S9(8)      COMP.            
00352          16  ARCH-REC-TYPE        PIC X.                          
00353          16  ARCH-SEQ             PIC S9(4)      COMP VALUE +0.   
00354                                                                   
00355      12  BENE-KEY.                                                
00356          16  BENE-COMP-CD         PIC X.                          
00357          16  BENE-REC-TYPE        PIC X.                          
00358          16  BENE-NUMBER.                                         
00359              20  BENE-CREDITOR    PIC XXX.                        
00360              20  FILLER           PIC X(7).                       
00361                                                                   
00362      12  PROD-KEY.                                                
00363          16  PROD-PARTIAL-KEY.                                    
00364              20  PROD-CO          PIC X.                          
00365              20  PROD-CARRIER     PIC X.                          
00366              20  PROD-GROUPING    PIC X(6).                       
00367              20  PROD-STATE       PIC XX.                         
00368              20  PROD-PRODUCER    PIC X(10).                      
00369          16  PROD-EXP-DATE        PIC XX.                         
00370                                                                   
00371      12  PLCY-KEY.                                                
00372          16  PLCY-CO              PIC X.                          
00373          16  PLCY-CARRIER         PIC X.                          
00374          16  PLCY-GROUPING        PIC X(06).                      
00375          16  PLCY-STATE           PIC XX.                         
00376          16  PLCY-PRODUCER        PIC X(10).                      
00377          16  PLCY-EFF-DT          PIC XX.                         
00378          16  PLCY-REFERENCE-NO    PIC X(20).                      
00379                                                                   
00380      12  PLAN-KEY.                                                
00381          16  PLAN-CO              PIC X.                          
00382          16  PLAN-CARRIER         PIC X.                          
00383          16  PLAN-GROUPING        PIC X(6).                       
00384          16  PLAN-STATE           PIC XX.                         
00385          16  PLAN-PRODUCER        PIC X(10).                      
00386          16  PLAN-CODE            PIC XX.                         
00387          16  PLAN-REV-NO          PIC 999.                        
041513
041513     12  ELENCC-KEY.
041513         16  ELENCC-COMPANY-CD    PIC X.
041513         16  ELENCC-REC-TYPE      PIC X.
041513         16  ELENCC-ENC-CODE      PIC X(5).
041513         16  F                    PIC X(09).
00388                                                                   
00389      EJECT                                                        
00390      12  ARCH-SAVE-KEY           PIC X(5).                        
00391      12  ACTV-SAVE-KEY           PIC X(20).                       
00392      12  ACCT-SAVE-KEY           PIC X(20).                       
00393      12  PROD-SAVE-KEY           PIC X(20).                       
00394      12  ACCT-LENGTH             PIC S9(4)  COMP  VALUE +2000.    
00395      12  ARCH-LENGTH             PIC S9(4)  COMP  VALUE +90.      
00396      12  ARCT-LENGTH             PIC S9(4)  COMP  VALUE +90.      
00397      12  ACTV-LENGTH             PIC S9(4)  COMP  VALUE +200.     
00398      12  PROD-LENGTH             PIC S9(4)  COMP  VALUE +2000.    
033110     12  NAPS-LENGTH             PIC S9(4)  COMP  VALUE +150.
041513     12  ENCC-LENGTH             PIC S9(4)  COMP  VALUE +400.
00399      12  TEXT-SAVE-KEY           PIC X(5).                        
00400      12  SAVE-VARIABLE-POINTER   PIC S9(8)   COMP.                
00401      12  VAR-HOLD.                                                
00402          16  V1                  PIC X.                           
00403          16  V2                  PIC X.                           
00404          16  V3                  PIC X.                           
00405          16  V4                  PIC X.                           
00406      12  V-HOLD REDEFINES VAR-HOLD.                               
00407          16  V-NUM               PIC 99.                          
00408          16  V-PERIOD            PIC X.                           
00409          16  V-DECIMAL           PIC 9.                           
00410                                                                   
00411      12  MAX-LINES               PIC 999     VALUE 300.           
00412      12  NUM-LINES-PER-SCREEN    PIC 99      VALUE 13.            
00413      12  TS-NUM-REC-IN-GROUP     PIC 99      VALUE 50.            
00414      12  TS-GROUP-WORK           PIC 9(5)    VALUE 0     COMP-3.  
00415      12  TS-LENGTH               PIC S9(4)   VALUE +3650 COMP.    
00416      12  TS-ITEM                 PIC S9(4)   VALUE +0    COMP.    
00417      12  TS-MAP-LENGTH           PIC S9(4)   VALUE +1343 COMP.    
00418      12  ROLL-COUNTER            PIC S999    VALUE +0    COMP-3.  
00419      12  TS-NAME-TEXT.                                            
00420          16  TS-ID-TEXT          PIC X(4)    VALUE '104A'.        
00421          16  TS-ID-TIME REDEFINES TS-ID-TEXT  PIC S9(7) COMP-3.   
00422          16  TS-TERM-TEXT.                                        
00423           17 TS-TERM-PREFIX      PIC XX.                          
00424           17 FILLER              PIC XX.                          
00425      12  TS-NAME-SCREEN.                                          
00426          16  FILLER              PIC X(4)    VALUE '152X'.        
00427          16  TS-TERM-SCREEN      PIC X(4).                        
00428                                                                   
00429      12  LINE-NUM.                                                
00430          16  LINE1               PIC X.                           
00431          16  LINE23              PIC 99.                          
00432      12  LIN-NUM REDEFINES LINE-NUM  PIC 999.                     
00433      12  TOP-FORM                PIC X(70)                        
00434          VALUE '*****TOP OF FORM *****'.                          
00435      12  SINGLE-LINE             PIC X(70).                       
00436      12  SINGLE-LINE-BY-1 REDEFINES SINGLE-LINE.                  
00437          16  ONE-CHAR OCCURS 70 TIMES INDEXED BY INDX1 INDX2      
00438                                  PIC X.                           
033110     12  NAPERSOFT-LETTER        PIC X(16)
033110         VALUE 'Napersoft letter'.
041513
041513     12  WS-RESPONSE             PIC S9(8)   COMP.
041513         88  RESP-NORMAL              VALUE +00.
041513         88  RESP-ERROR               VALUE +01.
041513         88  RESP-NOTFND              VALUE +13.
041513         88  RESP-NOTOPEN             VALUE +19.
041513         88  RESP-ENDFILE             VALUE +20.
00439                                                                   
00440  01  HAN-LETTER-REASON-DATA.                                      
00441      12  WS-REASON-TEXT.                                          
00442          16  WS-RE-NDX           PIC 99.                          
00443          16  FILLER              PIC X(68).                       
00444                                                                   
00445      12  HAN-REASON-TABLE.                                        
00446          16  FILLER              PIC X(50) VALUE                  
00447            'ADDITIONAL INFO REQUESTED FROM PHYSICIAN          '.  
00448          16  FILLER              PIC X(50) VALUE                  
00449            'CHECKING PRE-EXISTING CONDITION                   '.  
00450          16  FILLER              PIC X(50) VALUE                  
00451            'ADDITIONAL INFO RECEIVED / CLAIM REOPENED         '.  
00452          16  FILLER              PIC X(50) VALUE                  
00453            'LETTER TO INSURED                                 '.  
00454          16  FILLER              PIC X(50) VALUE                  
00455            'LETTER TO CREDITOR                                '.  
00456          16  FILLER              PIC X(50) VALUE                  
00457            'LETTER TO EMPLOYER                                '.  
00458          16  FILLER              PIC X(50) VALUE                  
00459            'LETTER TO INSURED / 2ND REQUEST                   '.  
00460          16  FILLER              PIC X(50) VALUE                  
00461            'LETTER TO CREDITOR / 2ND REQUEST                  '.  
00462          16  FILLER              PIC X(50) VALUE                  
00463            'LETTER TO EMPLOYER / 2ND REQUEST                  '.  
00464          16  FILLER              PIC X(50) VALUE                  
00465            'AWAITING INITIAL CLAIM FORM                       '.  
00466          16  FILLER              PIC X(50) VALUE                  
00467            'AWAITING SUPPLEMENTAL INFORMATION                 '.  
00468          16  FILLER              PIC X(50) VALUE                  
00469            'DENIED / PRE-EXISTING CONDITION                   '.  
00470          16  FILLER              PIC X(50) VALUE                  
00471            'DENIED / WAITING PERIOD NOT MET                   '.  
00472          16  FILLER              PIC X(50) VALUE                  
00473            'DENIED / NORMAL PREGNANCY                         '.  
00474          16  FILLER              PIC X(50) VALUE                  
00475            'DENIED / ACT OF WAR                               '.  
00476          16  FILLER              PIC X(50) VALUE                  
00477            'DENIED / NOT TOTALLY DISABLED                     '.  
00478          16  FILLER              PIC X(50) VALUE                  
00479            'DENIED / NOT UNDER CARE & TREATMENT OF PHYSICIAN  '.  
00480          16  FILLER              PIC X(50) VALUE                  
00481            'DENIED / NO COVERAGE INFORCE                      '.  
00482          16  FILLER              PIC X(50) VALUE                  
00483            'DENIED / DISABLED ON DATE OF LOAN                 '.  
00484          16  FILLER              PIC X(50) VALUE                  
00485            'DENIED / OVER MAXIMUM AGE                         '.  
00486          16  FILLER              PIC X(50) VALUE                  
00487            'CLOSED / CLAIM INFO NOT PROVIDED                  '.  
00488          16  FILLER              PIC X(50) VALUE                  
00489            'PHYSICIAN INFORMATION INCOMPLETE                  '.  
00490          16  FILLER              PIC X(50) VALUE                  
00491            'ACKNOWLEDGEMENT LETTER TO INSURED                 '.  
00492          16  FILLER              PIC X(50) VALUE                  
00493            'DENIED/SUICIDE EXCLUSION                          '.  
00494          16  FILLER              PIC X(50) VALUE                  
00495            'DENIED/LOAN EFFECTIVE BEFORE POLICY EFFECTIVE DATE'.  
00496          16  FILLER              PIC X(50) VALUE                  
00497            'DENIED/JOINT DEBTORS NOT COVERED                  '.  
00498          16  FILLER              PIC X(50) VALUE                  
00499            'DENIED/GROUP POLICY LAPSED                        '.  
00500          16  FILLER              PIC X(50) VALUE                  
00501            'DENIED/DECEASED PRIOR TO POLICY EFFECTIVE DATE    '.  
00502          16  FILLER              PIC X(50) VALUE                  
00503            'DENIED/LOAN TERM IN EXCESS OF MAXIMUM LOAN TERM   '.  
00504          16  FILLER              PIC X(50) VALUE                  
00505            'DENIED/INSURED NOT PERMANENTLY & TOTALLY DISABLED '.  
00506          16  FILLER              PIC X(50) VALUE                  
00507            'DENIED/IME EXAM DOES NOT SUPPORT CONT. DISABILITY '.  
00508          16  FILLER              PIC X(50) VALUE                  
00509            'DENIED/INSURED DID NOT APPEAR FOR IME EXAM        '.  
00510                                                                   
00511      12  HAN-LETTER-REASON-TABLE  REDEFINES  HAN-REASON-TABLE.    
00512          16  HAN-TABLE-ENTRIES  OCCURS  32  TIMES.                
00513              20  HAN-REASON-TEXT PIC X(50).                       
00514                                                                   
00515  01  ERROR-MESSAGES.                                              
00516      12  ER-0000                 PIC X(4)  VALUE '0000'.          
00517      12  ER-0004                 PIC X(4)  VALUE '0004'.          
00518      12  ER-0006                 PIC X(4)  VALUE '0006'.          
00519      12  ER-0008                 PIC X(4)  VALUE '0008'.          
00520      12  ER-0013                 PIC X(4)  VALUE '0013'.          
00521      12  ER-0023                 PIC X(4)  VALUE '0023'.          
00522      12  ER-0029                 PIC X(4)  VALUE '0029'.          
00523      12  ER-0033                 PIC X(4)  VALUE '0033'.          
00524      12  ER-0042                 PIC X(4)  VALUE '0042'.          
00525      12  ER-0047                 PIC X(4)  VALUE '0047'.          
00526      12  ER-0051                 PIC X(4)  VALUE '0051'.          
00527      12  ER-0066                 PIC X(4)  VALUE '0066'.          
00528      12  ER-0067                 PIC X(4)  VALUE '0067'.          
00529      12  ER-0070                 PIC X(4)  VALUE '0070'.          
00530      12  ER-0133                 PIC X(4)  VALUE '0133'.          
00531      12  ER-0154                 PIC X(4)  VALUE '0154'.          
00532      12  ER-0168                 PIC X(4)  VALUE '0168'.          
00533      12  ER-0169                 PIC X(4)  VALUE '0169'.          
00534      12  ER-0172                 PIC X(4)  VALUE '0172'.          
00535      12  ER-0174                 PIC X(4)  VALUE '0174'.          
00536      12  ER-0175                 PIC X(4)  VALUE '0175'.          
00537      12  ER-0176                 PIC X(4)  VALUE '0176'.          
00538      12  ER-0177                 PIC X(4)  VALUE '0177'.          
00539      12  ER-0178                 PIC X(4)  VALUE '0178'.          
00540      12  ER-0179                 PIC X(4)  VALUE '0179'.          
00541      12  ER-0180                 PIC X(4)  VALUE '0180'.          
00542      12  ER-0181                 PIC X(4)  VALUE '0181'.          
00543      12  ER-0182                 PIC X(4)  VALUE '0182'.          
00544      12  ER-0183                 PIC X(4)  VALUE '0183'.          
00545      12  ER-0184                 PIC X(4)  VALUE '0184'.          
00546      12  ER-0185                 PIC X(4)  VALUE '0185'.          
00547      12  ER-0186                 PIC X(4)  VALUE '0186'.          
00548      12  ER-0187                 PIC X(4)  VALUE '0187'.          
00549      12  ER-0188                 PIC X(4)  VALUE '0188'.          
00550      12  ER-0189                 PIC X(4)  VALUE '0189'.          
00551      12  ER-0190                 PIC X(4)  VALUE '0190'.          
00552      12  ER-0191                 PIC X(4)  VALUE '0191'.          
00553      12  ER-0206                 PIC X(4)  VALUE '0206'.          
00554      12  ER-0211                 PIC X(4)  VALUE '0211'.          
00555      12  ER-0279                 PIC X(4)  VALUE '0279'.          
00556      12  ER-0280                 PIC X(4)  VALUE '0280'.          
00557      12  ER-0281                 PIC X(4)  VALUE '0281'.          
00558      12  ER-0332                 PIC X(4)  VALUE '0332'.          
00559      12  ER-0343                 PIC X(4)  VALUE '0343'.          
00560      12  ER-0373                 PIC X(4)  VALUE '0373'.          
00561      12  ER-0374                 PIC X(4)  VALUE '0374'.          
00562      12  ER-0412                 PIC X(4)  VALUE '0412'.          
00563      12  ER-0413                 PIC X(4)  VALUE '0413'.          
00564      12  ER-0533                 PIC X(4)  VALUE '0533'.          
00565      12  ER-0537                 PIC X(4)  VALUE '0537'.          
00566      12  ER-0716                 PIC X(4)  VALUE '0716'.          
00567      12  ER-0861                 PIC X(4)  VALUE '0861'.          
040110     12  ER-0894                 PIC X(4)  VALUE '0894'.
00568      12  ER-0900                 PIC X(4)  VALUE '0900'.          
00569      12  ER-0909                 PIC X(4)  VALUE '0909'.          
00570      12  ER-0911                 PIC X(4)  VALUE '0911'.          
00571      12  ER-0912                 PIC X(4)  VALUE '0912'.
           12  ER-1236                 PIC X(4)  VALUE '1236'.
           12  ER-1560                 PIC X(4)  VALUE '1560'.
00572      12  ER-2055                 PIC X(4)  VALUE '2055'.          
00573      12  ER-2397                 PIC X(4)  VALUE '2397'.          
00574      12  ER-2398                 PIC X(4)  VALUE '2398'.          
00575      12  ER-3547                 PIC X(4)  VALUE '3547'.          
00576      12  ER-3770                 PIC X(4)  VALUE '3770'.          
00577      12  ER-3771                 PIC X(4)  VALUE '3771'.          
00578      12  ER-3772                 PIC X(4)  VALUE '3772'.          
00579      12  ER-7675                 PIC X(4)  VALUE '7675'.          
00580      12  ER-7840                 PIC X(4)  VALUE '7840'.          
00581      12  ER-7842                 PIC X(4)  VALUE '7842'.          
00582      12  ER-7843                 PIC X(4)  VALUE '7843'.          
00583      12  ER-8158                 PIC X(4)  VALUE '8158'.          
00584      12  ER-9106                 PIC X(4)  VALUE '9106'.          
00585      12  ER-9483                 PIC X(4)  VALUE '9483'.          
00586      12  ER-9808                 PIC X(4)  VALUE '9808'.          
00587      12  ER-9883                 PIC X(4)  VALUE '9883'.          
00588      12  ER-9887                 PIC X(4)  VALUE '9887'.          
00589                                                                   
00590      EJECT                                                        
00591 ****************************************************              
00592 *       WHEN ADDING OR DELETING ENTRIES TO         *              
00593 *       THE SYSTEM-SUPPORTED-VARIABLES THE         *              
00594 *       SS-NUM-ENTRIES FIELD MUST BE ALTERED       *              
00595 *       TO MATCH THE NUMBER OF ENTRIES IN THE      *              
00596 *       SYSTEM-SUPPORTED-VARIABLE TABLE.           *              
00597 *       ALSO YOU NEED TO INCREASE THE LENGTH OF    *              
00598 *       SS-WORK-AREA-LENGTH AND SYSTEM-VARIABLES   *              
00599 *                                                  *              
00600 *   3 FIELDS TO CHANGE:                            *              
00601 *      1 - SS-NUM-ENTRIES........(NO.OF ENTRIES)   *              
00602 *      2 - SS-WORK-AREA-LENGTH...(TOTAL LENGTH)    *              
00603 *      3 - SYSTEM-VARIABLES......( "      "   )    *              
00604 ****************************************************              
00605                                                                   
00606 *  THE SYSTEM-VARIABLES  FIELD LENGTH MUST MATCH THE LENGTH OF    
00607 *  THE SS-WORK-AREA-LENGTH FIELD FOR THE VARIABLE-WORK-AREA       
00608                                                                   
010407     12  SS-NUM-ENTRIES          PIC 999    VALUE 130      COMP-3.
00610      12  SS-COUNTER              PIC 999                   COMP-3.
010407     12  SS-WORK-AREA-LENGTH     PIC S9(4)  VALUE +3865    COMP.  
00612                                                                   
00613  01  VARIABLE-WORK-AREA.                                          
00614      12  VAR-CODE                PIC X(4).                        
00615      12  VAR-LEN                 PIC 99.                          
00616      12  VAR-DATA                PIC X(100).                      
00617      12  VAR-DATA-R REDEFINES VAR-DATA.                           
00618        16  VAR-ONE-CHAR OCCURS 100 TIMES INDEXED BY INDXV PIC X.  
00619                                                                   
00620  01  SYSTEM-SUPPORTED-VARIABLES.                                  
00621 *****COMPANY NAME                                                 
00622      12  SS01                    PIC X(4)  VALUE     '01.0'.      
00623      12  SS01L                   PIC 99    VALUE 36.              
00624      12  SS01D                   PIC X(30) VALUE ALL '*'.         
00625 *****FULL COMPANY ADDRESS                                         
00626      12  SS02-1                  PIC X(4)  VALUE     '02.1'.      
00627      12  SS02-1L                 PIC 99    VALUE 36.              
00628      12  SS02-1D                 PIC X(30) VALUE ALL '*'.         
00629      12  SS02-2                  PIC X(4)  VALUE     '02.2'.      
00630      12  SS02-2L                 PIC 99    VALUE 36.              
00631      12  SS02-2D                 PIC X(30) VALUE ALL '*'.         
00632      12  SS02-3                  PIC X(4)  VALUE     '02.3'.      
00633      12  SS02-3L                 PIC 99    VALUE 36.              
00634      12  SS02-3D                 PIC X(30) VALUE ALL '*'.         
00635      12  SS02-4                  PIC X(4)  VALUE     '02.4'.      
00636      12  SS02-4L                 PIC 99    VALUE 36.              
00637      12  SS02-4D                 PIC X(30) VALUE ALL '*'.         
00638      12  SS02-5                  PIC X(4)  VALUE     '02.5'.      
00639      12  SS02-5L                 PIC 99    VALUE 36.              
00640      12  SS02-5D                 PIC X(30) VALUE ALL '*'.         
00641 *****CARRIER NAME                                                 
00642      12  SS03                    PIC X(4)  VALUE     '03.0'.      
00643      12  SS03L                   PIC 99    VALUE 36.              
00644      12  SS03D                   PIC X(30) VALUE ALL '*'.         
00645 *****INVESTORS HERITAGE
010407* as Administrator for Investors Heritage Life Insurance Company
00646      12  SS03-1                  PIC X(4)  VALUE     '03.1'.      
00647      12  SS03-1L                 PIC 99    VALUE 68.              
00648      12  SS03-1D                 PIC X(62) VALUE ALL '*'.         
00645 *****FULL CARRIER ADDRESS                                         
00646      12  SS04-1                  PIC X(4)  VALUE     '04.1'.      
00647      12  SS04-1L                 PIC 99    VALUE 36.              
00648      12  SS04-1D                 PIC X(30) VALUE ALL '*'.         
00649      12  SS04-2                  PIC X(4)  VALUE     '04.2'.      
00650      12  SS04-2L                 PIC 99    VALUE 36.              
00651      12  SS04-2D                 PIC X(30) VALUE ALL '*'.         
00652      12  SS04-3                  PIC X(4)  VALUE     '04.3'.      
00653      12  SS04-3L                 PIC 99    VALUE 36.              
00654      12  SS04-3D                 PIC X(30) VALUE ALL '*'.         
00655      12  SS04-4                  PIC X(4)  VALUE     '04.4'.      
00656      12  SS04-4L                 PIC 99    VALUE 36.              
00657      12  SS04-4D                 PIC X(30) VALUE ALL '*'.         
00658      12  SS04-5                  PIC X(4)  VALUE     '04.5'.      
00659      12  SS04-5L                 PIC 99    VALUE 36.              
00660      12  SS04-5D                 PIC X(30) VALUE ALL '*'.         
00661 *****CARRIER PHONE NUMBER                                         
00662      12  SS04-6                  PIC X(4)  VALUE     '04.6'.      
00663      12  SS04-6L                 PIC 99    VALUE 18.              
00664      12  SS04-6D                 PIC X(12) VALUE ALL '*'.         
00665 *****FULL ADDRESEE LABEL                                          
00666      12  SS05-1                  PIC X(4)  VALUE     '05.1'.      
00667      12  SS05-1L                 PIC 99    VALUE 36.              
00668      12  SS05-1D                 PIC X(30) VALUE ALL '*'.         
00669      12  SS05-2                  PIC X(4)  VALUE     '05.2'.      
00670      12  SS05-2L                 PIC 99    VALUE 36.              
00671      12  SS05-2D                 PIC X(30) VALUE ALL '*'.         
00672      12  SS05-3                  PIC X(4)  VALUE     '05.3'.      
00673      12  SS05-3L                 PIC 99    VALUE 36.              
00674      12  SS05-3D                 PIC X(30) VALUE ALL '*'.         
00675      12  SS05-4                  PIC X(4)  VALUE     '05.4'.      
00676      12  SS05-4L                 PIC 99    VALUE 36.              
00677      12  SS05-4D                 PIC X(30) VALUE ALL '*'.         
00678      12  SS05-5                  PIC X(4)  VALUE     '05.5'.      
00679      12  SS05-5L                 PIC 99    VALUE 36.              
00680      12  SS05-5D                 PIC X(30) VALUE ALL '*'.         
00681      12  SS05-6                  PIC X(4)  VALUE     '05.6'.      
00682      12  SS05-6L                 PIC 99    VALUE 36.              
00683      12  SS05-6D                 PIC X(30) VALUE ALL '*'.         
00684 *****ACCOUNT NAME                                                 
00685      12  SS06                    PIC X(4)  VALUE     '06.0'.      
00686      12  SS06L                   PIC 99    VALUE 36.              
00687      12  SS06D                   PIC X(30) VALUE ALL '*'.         
00688 *****FULL ACCOUNT ADDRESS                                         
00689      12  SS07-1                  PIC X(4)  VALUE     '07.1'.      
00690      12  SS07-1L                 PIC 99    VALUE 36.              
00691      12  SS07-1D                 PIC X(30) VALUE ALL '*'.         
00692      12  SS07-2                  PIC X(4)  VALUE     '07.2'.      
00693      12  SS07-2L                 PIC 99    VALUE 36.              
00694      12  SS07-2D                 PIC X(30) VALUE ALL '*'.         
00695      12  SS07-3                  PIC X(4)  VALUE     '07.3'.      
00696      12  SS07-3L                 PIC 99    VALUE 36.              
00697      12  SS07-3D                 PIC X(30) VALUE ALL '*'.         
00698      12  SS07-4                  PIC X(4)  VALUE     '07.4'.      
00699      12  SS07-4L                 PIC 99    VALUE 36.              
00700      12  SS07-4D                 PIC X(30) VALUE ALL '*'.         
00701      12  SS07-5                  PIC X(4)  VALUE     '07.5'.      
00702      12  SS07-5L                 PIC 99    VALUE 36.              
00703      12  SS07-5D                 PIC X(30) VALUE ALL '*'.         
00704 *****ACCOUNT PHONE NUMBER                                         
00705      12  SS07-6                  PIC X(4)  VALUE     '07.6'.      
00706      12  SS07-6L                 PIC 99    VALUE 18.              
00707      12  SS07-6D                 PIC X(12) VALUE ALL '*'.         
00708 *****EXECUTING PROCESSOR NAME                                     
00709      12  SS08                    PIC X(4)  VALUE     '08.0'.      
00710      12  SS08L                   PIC 99    VALUE 36.              
00711      12  SS08D                   PIC X(30) VALUE ALL '*'.         
00712 *****PROCESSOR TITLE                                              
00713      12  SS09                    PIC X(4)  VALUE     '09.0'.      
00714      12  SS09L                   PIC 99    VALUE 32.              
00715      12  SS09D                   PIC X(26) VALUE ALL '*'.         
00716 *****INSUREDS NAME                                                
00717      12  SS10                    PIC X(4)  VALUE     '10.0'.      
00718      12  SS10L                   PIC 99    VALUE 36.              
00719      12  SS10D                   PIC X(30) VALUE ALL '*'.         
00720 *****INSUREDS ADDRESS                                             
00721      12  SS11-1                  PIC X(4)  VALUE     '11.1'.      
00722      12  SS11-1L                 PIC 99    VALUE 36.              
00723      12  SS11-1D                 PIC X(30) VALUE ALL '*'.         
00724      12  SS11-2                  PIC X(4)  VALUE     '11.2'.      
00725      12  SS11-2L                 PIC 99    VALUE 36.              
00726      12  SS11-2D                 PIC X(30) VALUE ALL '*'.         
00727      12  SS11-3                  PIC X(4)  VALUE     '11.3'.      
00728      12  SS11-3L                 PIC 99    VALUE 36.              
00729      12  SS11-3D                 PIC X(30) VALUE ALL '*'.         
00730      12  SS11-4                  PIC X(4)  VALUE     '11.4'.      
00731      12  SS11-4L                 PIC 99    VALUE 36.              
00732      12  SS11-4D                 PIC X(30) VALUE ALL '*'.         
00733 *****INSUREDS NAME FROM ADDR TRAILER                              
00734      12  SS11-5                  PIC X(4)  VALUE     '11.5'.      
00735      12  SS11-5L                 PIC 99    VALUE 36.              
00736      12  SS11-5D                 PIC X(30) VALUE ALL '*'.         
00737 *****INSUREDS PHONE NUMBER FROM ADDR TRAILER                      
00738      12  SS11-6                  PIC X(4)  VALUE     '11.6'.      
00739      12  SS11-6L                 PIC 99    VALUE 18.              
00740      12  SS11-6D                 PIC X(12) VALUE ALL '*'.         
00741 *****CLAIM TYPE NAME                                              
00742      12  SS12                    PIC X(4)  VALUE     '12.0'.      
00743      12  SS12L                   PIC 99    VALUE 12.              
00744      12  SS12D                   PIC X(6)  VALUE ALL '*'.         
00745 *****CLAIM INCURRED DATE                                          
00746      12  SS13                    PIC X(4)  VALUE     '13.0'.      
00747      12  SS13L                   PIC 99    VALUE 14.              
00748      12  SS13D                   PIC X(8)  VALUE ALL '*'.         
00749 *****CLAIM REPORTED DATE                                          
00750      12  SS14                    PIC X(4)  VALUE     '14.0'.      
00751      12  SS14L                   PIC 99    VALUE 14.              
00752      12  SS14D                   PIC X(8)  VALUE ALL '*'.         
00753 *****LAST PAYMENT DATE                                            
00754      12  SS15                    PIC X(4)  VALUE     '15.0'.      
00755      12  SS15L                   PIC 99    VALUE 14.              
00756      12  SS15D                   PIC X(8)  VALUE ALL '*'.         
00757 *****LAST PAYMENT AMOUNT                                          
00758      12  SS16                    PIC X(4)  VALUE     '16.0'.      
00759      12  SS16L                   PIC 99    VALUE 17.              
00760      12  SS16D                   PIC $$$$,$$$.99 VALUE ZEROS.     
060109*****CLAIM INTEREST RATE
060109     12  SS16-1                  PIC X(4)  VALUE     '16.1'.
060109     12  SS16-1L                 PIC 99    VALUE 13.
060109     12  SS16-1D                 PIC X(7)        VALUE SPACE.
00761 *****CLAIM PAID THRU/TO DATE                                      
00762      12  SS17                    PIC X(4)  VALUE     '17.0'.      
00763      12  SS17L                   PIC 99    VALUE 14.              
00764      12  SS17D                   PIC X(8)  VALUE ALL '*'.         
00765 *****TOTAL PAID TO DATE                                           
00766      12  SS18                    PIC X(4)  VALUE     '18.0'.      
00767      12  SS18L                   PIC 99    VALUE 17.              
00768      12  SS18D                   PIC $$$$,$$$.99 VALUE ZEROS.     
00769 *****DIAGNOSIS OR CAUSE                                           
00770      12  SS19                    PIC X(4)  VALUE     '19.0'.      
00771      12  SS19L                   PIC 99    VALUE 32.              
00772      12  SS19D                   PIC X(26) VALUE ALL '*'.         
00773 *****CAUSE CODE                                                   
00774      12  SS19-1                  PIC X(4)  VALUE     '19.1'.      
00775      12  SS19-1L                 PIC 99    VALUE 12.              
00776      12  SS19-1D                 PIC X(6)  VALUE ALL '*'.         
PEMMOD*****CID LOAN NUMBER                                              
PEMMOD     12  SS19-2                  PIC X(4)  VALUE     '19.2'.      
PEMMOD     12  SS19-2L                 PIC 99    VALUE 31.              
PEMMOD     12  SS19-2D                 PIC X(25) VALUE ALL '*'.         
00777 *****CURRENT DATE                                                 
00778      12  SS20                    PIC X(4)  VALUE     '20.0'.      
00779      12  SS20L                   PIC 99    VALUE 14.              
00780      12  SS20D                   PIC X(8)  VALUE ALL '*'.         
00781 *****FULL CURRENT DATE                                            
00782      12  SS21                    PIC X(4)  VALUE     '21.0'.      
00783      12  SS21L                   PIC 99    VALUE 24.              
00784      12  SS21D                   PIC X(18) VALUE ALL '*'.         
00785 *****BENEFIT DESCRIPTION                                          
00786      12  SS22                    PIC X(4)  VALUE     '22.0'.      
00787      12  SS22L                   PIC 99    VALUE 16.              
00788      12  SS22D                   PIC X(10) VALUE ALL '*'.         
00789 *****CARRIER CODE IN CERT                                         
00790      12  SS23                    PIC X(4)  VALUE     '23.0'.      
00791      12  SS23L                   PIC 99    VALUE 9.               
00792      12  SS23D                   PIC XXX   VALUE ALL '*'.         
00793 *****GROUPING CODE IN CERT                                        
00794      12  SS24                    PIC X(4)  VALUE     '24.0'.      
00795      12  SS24L                   PIC 99    VALUE 12.              
00796      12  SS24D                   PIC X(6)  VALUE ALL '*'.         
00797 *****ACCOUNT NUMBER IN CERT                                       
00798      12  SS25                    PIC X(4)  VALUE     '25.0'.      
00799      12  SS25L                   PIC 99    VALUE 16.              
00800      12  SS25D                   PIC X(10) VALUE ALL '*'.         
00801 *****CERTIFICATE NUMBER                                           
00802      12  SS26                    PIC X(4)  VALUE     '26.0'.      
00803      12  SS26L                   PIC 99    VALUE 17.              
00804      12  SS26D                   PIC X(11) VALUE ALL '*'.         
00805 *****CERT EFFECTIVE DATE                                          
00806      12  SS27                    PIC X(4)  VALUE     '27.0'.      
00807      12  SS27L                   PIC 99    VALUE 14.              
00808      12  SS27D                   PIC X(8)  VALUE ALL '*'.         
00809 *****CERT EXPIRATION DATE                                         
00810      12  SS28                    PIC X(4)  VALUE     '28.0'.      
00811      12  SS28L                   PIC 99    VALUE 14.              
00812      12  SS28D                   PIC X(8)  VALUE ALL '*'.         
00813 *****APPLICABLE COVERAGE TERM                                     
00814      12  SS29                    PIC X(4)  VALUE     '29.0'.      
00815      12  SS29L                   PIC 99    VALUE 9.               
00816      12  SS29D                   PIC XXX   VALUE ALL '*'.         
00817 *****APPLICABLE COVERAGE AMOUNT                                   
00818      12  SS30                    PIC X(4)  VALUE     '30.0'.      
00819      12  SS30L                   PIC 99    VALUE 18.              
00820      12  SS30D                   PIC $$$$$,$$$.99  VALUE ZEROS.   
00821 *****APPLICABLE COVERAGE CANCEL DATE                              
00822      12  SS31                    PIC X(4)  VALUE     '31.0'.      
00823      12  SS31L                   PIC 99    VALUE 14.              
00824      12  SS31D                   PIC X(8)  VALUE ALL '*'.         
00825 *****APPLICABLE COVERAGE FORM NUMBER                              
00826      12  SS32                    PIC X(4)  VALUE     '32.0'.      
00827      12  SS32L                   PIC 99    VALUE 18.              
00828      12  SS32D                   PIC X(12) VALUE ALL '*'.         
00829 *****INSURES AGE AT POLICY ISSUE                                  
00830      12  SS33                    PIC X(4)  VALUE     '33.0'.      
00831      12  SS33L                   PIC 99    VALUE 9.               
00832      12  SS33D                   PIC XXX   VALUE ALL '*'.         
00833 *****CLAIM NUMBER                                                 
00834      12  SS34                    PIC X(4)  VALUE     '34.0'.      
00835      12  SS34L                   PIC 99    VALUE 13.              
00836      12  SS34D                   PIC X(7)  VALUE ALL '*'.         
00837 *****LAST DENIAL TEXT                                             
00838      12  SS35-1                  PIC X(4)  VALUE     '35.1'.      
00839      12  SS35-1L                 PIC 99    VALUE 66.              
00840      12  SS35-1D                 PIC X(60) VALUE ALL '*'.         
00841      12  SS35-2                  PIC X(4)  VALUE     '35.2'.      
00842      12  SS35-2L                 PIC 99    VALUE 66.              
00843      12  SS35-2D                 PIC X(60) VALUE ALL '*'.         
00844 *****LOAN NUMBER                                                  
00845      12  SS36                    PIC X(4)  VALUE     '36.0'.      
00846      12  SS36L                   PIC 99    VALUE 14.              
00847      12  SS36D                   PIC X(8)  VALUE ALL '*'.         
00848 *****CURRENT LOAN NUMBER                                          
00849      12  SS36-1                  PIC X(4)  VALUE     '36.1'.      
00850      12  SS36-1L                 PIC 99    VALUE 26.              
00851      12  SS36-1D                 PIC X(20) VALUE ALL '*'.         
00852 *****LOAN BALANCE                                                 
00853      12  SS37                    PIC X(4)  VALUE     '37.0'.      
00854      12  SS37L                   PIC 99    VALUE 18.              
00855      12  SS37D                   PIC $$$$$,$$$.99  VALUE ZEROS.   
00856 *****MEMBER NUMBER                                                
00857      12  SS38                    PIC X(4)  VALUE     '38.0'.      
00858      12  SS38L                   PIC 99    VALUE 18.              
00859      12  SS38D                   PIC X(12) VALUE ALL '*'.         
00860 *****INSURED NAME (FIRST M LAST)                                  
00861      12  SS39                    PIC X(4)  VALUE     '39.0'.      
00862      12  SS39L                   PIC 99    VALUE 36.              
00863      12  SS39D                   PIC X(30) VALUE ALL '*'.         
00864 *****INSURED LAST NAME ONLY                                       
00865      12  SS40                    PIC X(4)  VALUE     '40.0'.      
00866      12  SS40L                   PIC 99    VALUE 21.              
00867      12  SS40D                   PIC X(15) VALUE ALL '*'.         
00868 *****TITLE (MR/MS)                                                
00869      12  SS41                    PIC X(4)  VALUE     '41.0'.      
00870      12  SS41L                   PIC 99    VALUE 9.               
00871      12  SS41D                   PIC X(3)  VALUE ALL '*'.         
00872 *****ELIMINATION PERIOD                                           
00873      12  SS42                    PIC X(4)  VALUE     '42.0'.      
00874      12  SS42L                   PIC 99    VALUE 9.               
00875      12  SS42D                   PIC X(3)  VALUE ALL '*'.         
00876 *****BENEFICIARY NAME                                             
00877      12  SS43                    PIC X(4)  VALUE     '43.0'.      
00878      12  SS43L                   PIC 99    VALUE 36.              
00879      12  SS43D                   PIC X(30) VALUE ALL '*'.         
00880 *****BENEFICIARY ADDRESS                                          
00881      12  SS44-1                  PIC X(4)  VALUE     '44.1'.      
00882      12  SS44-1L                 PIC 99    VALUE 36.              
00883      12  SS44-1D                 PIC X(30) VALUE ALL '*'.         
00884      12  SS44-2                  PIC X(4)  VALUE     '44.2'.      
00885      12  SS44-2L                 PIC 99    VALUE 36.              
00886      12  SS44-2D                 PIC X(30) VALUE ALL '*'.         
00887      12  SS44-3                  PIC X(4)  VALUE     '44.3'.      
00888      12  SS44-3L                 PIC 99    VALUE 36.              
00889      12  SS44-3D                 PIC X(30) VALUE ALL '*'.         
00890      12  SS44-4                  PIC X(4)  VALUE     '44.4'.      
00891      12  SS44-4L                 PIC 99    VALUE 36.              
00892      12  SS44-4D                 PIC X(30) VALUE ALL '*'.         
00893      12  SS44-5                  PIC X(4)  VALUE     '44.5'.      
00894      12  SS44-5L                 PIC 99    VALUE 18.              
00895      12  SS44-5D                 PIC X(12) VALUE ALL '*'.         
00896      12  SS44-6                  PIC X(4)  VALUE     '44.6'.      
00897      12  SS44-6L                 PIC 99    VALUE 36.              
00898      12  SS44-6D                 PIC X(30) VALUE ALL '*'.         
00899 *****INSUREDS DATE OF BIRTH                                       
00900      12  SS45                    PIC X(4)  VALUE     '45.0'.      
00901      12  SS45L                   PIC 99    VALUE 14.              
00902      12  SS45D                   PIC X(8)  VALUE ALL '*'.         
00903 *****INSUREDS SOC SEC NUMBER                                      
00904      12  SS46                    PIC X(4)  VALUE     '46.0'.      
00905      12  SS46L                   PIC 99    VALUE 17.              
00906      12  SS46D                   PIC X(11) VALUE ALL '*'.         
00907 *****PHYSICIANS  NAME                                             
00908      12  SS47                    PIC X(4)  VALUE     '47.0'.      
00909      12  SS47L                   PIC 99    VALUE 36.              
00910      12  SS47D                   PIC X(30) VALUE ALL '*'.         
00911 *****PHYSICIANS  ADDRESS                                          
00912      12  SS47-1                  PIC X(4)  VALUE     '47.1'.      
00913      12  SS47-1L                 PIC 99    VALUE 36.              
00914      12  SS47-1D                 PIC X(30) VALUE ALL '*'.         
00915      12  SS47-2                  PIC X(4)  VALUE     '47.2'.      
00916      12  SS47-2L                 PIC 99    VALUE 36.              
00917      12  SS47-2D                 PIC X(30) VALUE ALL '*'.         
00918      12  SS47-3                  PIC X(4)  VALUE     '47.3'.      
00919      12  SS47-3L                 PIC 99    VALUE 36.              
00920      12  SS47-3D                 PIC X(30) VALUE ALL '*'.         
00921      12  SS47-4                  PIC X(4)  VALUE     '47.4'.      
00922      12  SS47-4L                 PIC 99    VALUE 36.              
00923      12  SS47-4D                 PIC X(30) VALUE ALL '*'.         
00924      12  SS47-5                  PIC X(4)  VALUE     '47.5'.      
00925      12  SS47-5L                 PIC 99    VALUE 18.              
00926      12  SS47-5D                 PIC X(12) VALUE ALL '*'.         
00927 *****EMPLOYERS   NAME                                             
00928      12  SS48                    PIC X(4)  VALUE     '48.0'.      
00929      12  SS48L                   PIC 99    VALUE 36.              
00930      12  SS48D                   PIC X(30) VALUE ALL '*'.         
00931 *****EMPLOYERS   ADDRESS                                          
00932      12  SS48-1                  PIC X(4)  VALUE     '48.1'.      
00933      12  SS48-1L                 PIC 99    VALUE 36.              
00934      12  SS48-1D                 PIC X(30) VALUE ALL '*'.         
00935      12  SS48-2                  PIC X(4)  VALUE     '48.2'.      
00936      12  SS48-2L                 PIC 99    VALUE 36.              
00937      12  SS48-2D                 PIC X(30) VALUE ALL '*'.         
00938      12  SS48-3                  PIC X(4)  VALUE     '48.3'.      
00939      12  SS48-3L                 PIC 99    VALUE 36.              
00940      12  SS48-3D                 PIC X(30) VALUE ALL '*'.         
00941      12  SS48-4                  PIC X(4)  VALUE     '48.4'.      
00942      12  SS48-4L                 PIC 99    VALUE 36.              
00943      12  SS48-4D                 PIC X(30) VALUE ALL '*'.         
00944      12  SS48-5                  PIC X(4)  VALUE     '48.5'.      
00945      12  SS48-5L                 PIC 99    VALUE 18.              
00946      12  SS48-5D                 PIC X(12) VALUE ALL '*'.         
00947 *****OTHER1      NAME                                             
00948      12  SS49                    PIC X(4)  VALUE     '49.0'.      
00949      12  SS49L                   PIC 99    VALUE 36.              
00950      12  SS49D                   PIC X(30) VALUE ALL '*'.         
00951 *****OTHER1      ADDRESS                                          
00952      12  SS49-1                  PIC X(4)  VALUE     '49.1'.      
00953      12  SS49-1L                 PIC 99    VALUE 36.              
00954      12  SS49-1D                 PIC X(30) VALUE ALL '*'.         
00955      12  SS49-2                  PIC X(4)  VALUE     '49.2'.      
00956      12  SS49-2L                 PIC 99    VALUE 36.              
00957      12  SS49-2D                 PIC X(30) VALUE ALL '*'.         
00958      12  SS49-3                  PIC X(4)  VALUE     '49.3'.      
00959      12  SS49-3L                 PIC 99    VALUE 36.              
00960      12  SS49-3D                 PIC X(30) VALUE ALL '*'.         
00961      12  SS49-4                  PIC X(4)  VALUE     '49.4'.      
00962      12  SS49-4L                 PIC 99    VALUE 36.              
00963      12  SS49-4D                 PIC X(30) VALUE ALL '*'.         
00964      12  SS49-5                  PIC X(4)  VALUE     '49.5'.      
00965      12  SS49-5L                 PIC 99    VALUE 18.              
00966      12  SS49-5D                 PIC X(12) VALUE ALL '*'.         
00967 *****OTHER2      NAME                                             
00968      12  SS50                    PIC X(4)  VALUE     '50.0'.      
00969      12  SS50L                   PIC 99    VALUE 36.              
00970      12  SS50D                   PIC X(30) VALUE ALL '*'.         
00971 *****OTHER2      ADDRESS                                          
00972      12  SS50-1                  PIC X(4)  VALUE     '50.1'.      
00973      12  SS50-1L                 PIC 99    VALUE 36.              
00974      12  SS50-1D                 PIC X(30) VALUE ALL '*'.         
00975      12  SS50-2                  PIC X(4)  VALUE     '50.2'.      
00976      12  SS50-2L                 PIC 99    VALUE 36.              
00977      12  SS50-2D                 PIC X(30) VALUE ALL '*'.         
00978      12  SS50-3                  PIC X(4)  VALUE     '50.3'.      
00979      12  SS50-3L                 PIC 99    VALUE 36.              
00980      12  SS50-3D                 PIC X(30) VALUE ALL '*'.         
00981      12  SS50-4                  PIC X(4)  VALUE     '50.4'.      
00982      12  SS50-4L                 PIC 99    VALUE 36.              
00983      12  SS50-4D                 PIC X(30) VALUE ALL '*'.         
00984      12  SS50-5                  PIC X(4)  VALUE     '50.5'.      
00985      12  SS50-5L                 PIC 99    VALUE 18.              
00986      12  SS50-5D                 PIC X(12) VALUE ALL '*'.         
00987 *****A&H TERM TIMES MON. BEN.                                     
00988      12  SS51                    PIC X(4)  VALUE     '51.0'.      
00989      12  SS51L                   PIC 99    VALUE 17.              
00990      12  SS51D                   PIC $$$$,$$$.99 VALUE ZEROS.     
00991 *****THIRD PARTY NAME                                             
00992      12  SS52                    PIC X(4)  VALUE     '52.0'.      
00993      12  SS52L                   PIC 99    VALUE 36.              
00994      12  SS52D                   PIC X(30) VALUE ALL '*'.         
00995 *****THIRD PARTY ADDRESS                                          
00996      12  SS53-1                  PIC X(4)  VALUE     '53.1'.      
00997      12  SS53-1L                 PIC 99    VALUE 36.              
00998      12  SS53-1D                 PIC X(30) VALUE ALL '*'.         
00999      12  SS53-2                  PIC X(4)  VALUE     '53.2'.      
01000      12  SS53-2L                 PIC 99    VALUE 36.              
01001      12  SS53-2D                 PIC X(30) VALUE ALL '*'.         
01002      12  SS53-3                  PIC X(4)  VALUE     '53.3'.      
01003      12  SS53-3L                 PIC 99    VALUE 36.              
01004      12  SS53-3D                 PIC X(30) VALUE ALL '*'.         
01005      12  SS53-4                  PIC X(4)  VALUE     '53.4'.      
01006      12  SS53-4L                 PIC 99    VALUE 36.              
01007      12  SS53-4D                 PIC X(30) VALUE ALL '*'.         
01008      12  SS53-5                  PIC X(4)  VALUE     '53.5'.      
01009      12  SS53-5L                 PIC 99    VALUE 36.              
01010      12  SS53-5D                 PIC X(30) VALUE ALL '*'.         
01011 *****THIRD PARTY PHONE NUMBER                                     
01012      12  SS53-6                  PIC X(4)  VALUE     '53.6'.      
01013      12  SS53-6L                 PIC 99    VALUE 18.              
01014      12  SS53-6D                 PIC X(12) VALUE ALL '*'.         
01015 *****CERTIFICATE SEQUENCE                                         
01016      12  SS54                    PIC X(4)  VALUE     '54.0'.      
01017      12  SS54L                   PIC 99    VALUE 09.              
01018      12  SS54D                   PIC XXX VALUE ALL '*'.           
01019 *****CERTIFICATE TOTAL  E                                         
01020      12  SS55                    PIC X(4)  VALUE     '55.0'.      
01021      12  SS55L                   PIC 99    VALUE 09.              
01022      12  SS55D                   PIC XXX VALUE ALL '*'.           
01023 *****CREDITOR ID                                                  
01024      12  SS56                    PIC  X(04) VALUE    '56.0'.      
01025      12  SS56L                   PIC  99 VALUE 36.                
01026      12  SS56D                   PIC  X(30) VALUE ALL '*'.        
01027 *****INSUREDS NAME (CERTIFICATE)                                  
01028      12  SS57                    PIC X(4)  VALUE     '57.0'.      
01029      12  SS57L                   PIC 99    VALUE 36.              
01030      12  SS57D                   PIC X(30) VALUE ALL '*'.         
01031 *****JOINT NAME (CERTIFICATE)                                     
01032      12  SS58                    PIC X(4)  VALUE     '58.0'.      
01033      12  SS58L                   PIC 99    VALUE 36.              
01034      12  SS58D                   PIC X(30) VALUE ALL '*'.         
01035 *****POLICY REFERENCE NUMBER                                      
01036      12  SS59                    PIC X(4)  VALUE     '59.0'.      
01037      12  SS59L                   PIC 99    VALUE 26.              
01038      12  SS59D                   PIC X(20) VALUE ALL '*'.         
01039 *****CREDIT CARD NUMBER (CLAIMS)                                  
01040      12  SS60                    PIC X(4)  VALUE     '60.0'.      
01041      12  SS60L                   PIC 99    VALUE 22.              
01042      12  SS60D                   PIC X(16) VALUE ALL '*'.         
01043 *****CORRESPONDENCE BENEFICIARY NAME                              
01044      12  SS61                    PIC X(4)  VALUE     '61.0'.      
01045      12  SS61L                   PIC 99    VALUE 36.              
01046      12  SS61D                   PIC X(30) VALUE ALL '*'.         
01047 *****CORRESPONDENCE BENEFICIARY ADDRESS                           
01048      12  SS61-1                  PIC X(4)  VALUE     '61.1'.      
01049      12  SS61-1L                 PIC 99    VALUE 36.              
01050      12  SS61-1D                 PIC X(30) VALUE ALL '*'.         
01051      12  SS61-2                  PIC X(4)  VALUE     '61.2'.      
01052      12  SS61-2L                 PIC 99    VALUE 36.              
01053      12  SS61-2D                 PIC X(30) VALUE ALL '*'.         
01054      12  SS61-3                  PIC X(4)  VALUE     '61.3'.      
01055      12  SS61-3L                 PIC 99    VALUE 36.              
01056      12  SS61-3D                 PIC X(30) VALUE ALL '*'.         
01057      12  SS61-4                  PIC X(4)  VALUE     '61.4'.      
01058      12  SS61-4L                 PIC 99    VALUE 36.              
01059      12  SS61-4D                 PIC X(30) VALUE ALL '*'.         
01060      12  SS61-5                  PIC X(4)  VALUE     '61.5'.      
01061      12  SS61-5L                 PIC 99    VALUE 36.              
01062      12  SS61-5D                 PIC X(30) VALUE ALL '*'.         
01063      12  SS61-6                  PIC X(4)  VALUE     '61.6'.      
01064      12  SS61-6L                 PIC 99    VALUE 18.              
01065      12  SS61-6D                 PIC X(12) VALUE ALL '*'.         
01066                                                                   
01067 *****DMD UNDERWRITER STATEMENT                                    
01068      12  SS62                    PIC X(4)  VALUE     '62.0'.      
01069      12  SS62L                   PIC 99    VALUE 56.              
01070      12  SS62D                   PIC X(50) VALUE ALL '*'.         
01071                                                                   
01072 *****DMD UNDERWRITER NAME                                         
01073      12  SS63                    PIC X(4)  VALUE     '63.0'.      
01074      12  SS63L                   PIC 99    VALUE 66.              
01075      12  SS63D                   PIC X(60) VALUE ALL '*'.         
01076                                                                   
01077 *****DMD UNDERWRITER NAME                                         
01078      12  SS64                    PIC X(4)  VALUE     '64.0'.      
01079      12  SS64L                   PIC 99    VALUE 66.              
01080      12  SS64D                   PIC X(60) VALUE ALL '*'.         
01081                                                                   
01082 ****************************************************              
01083 *       WHEN ADDING OR DELETING ENTRIES TO         *              
01084 *       THE SYSTEM-SUPPORTED-VARIABLES THE         *              
01085 *       SS-NUM-ENTRIES FIELD MUST BE ALTERED       *              
01086 *       TO MATCH THE NUMBER OF ENTRIES IN THE      *              
01087 *       SYSTEM-SUPPORTED-VARIABLE TABLE.           *              
01088 *       ALSO YOU NEED TO INCREASE THE LENGTH OF    *              
01089 *       SS-WORK-AREA-LENGTH AND SYSTEM-VARIABLES   *              
01090 ****************************************************              


01208 *  THE SYSTEM-VARIABLES  FIELD LENGTH MUST MATCH THE LENGTH OF    
01209 *  THE SS-WORK-AREA-LENGTH FIELD FOR THE VARIABLE-WORK-AREA       
01210                                                                   
PEMMOD 01  SYSTEM-VARIABLES            PIC X(3865).
01212  01  SYS-VAR-ENTRY.
01213      12  SYS-VAR-CODE            PIC X(4).                        
01214      12  SYS-VAR-LEN             PIC 99.                          
01215      12  SYS-VAR-DATA            PIC X(100).                      
01216                                                                   


01092                                  COPY ELCDATE.                    
01093                                                                   

01095                                  COPY ELCLOGOF.                   
01096                                                                   
01097      EJECT                                                        
01098                                  COPY ELCNWA.                     
01099      EJECT                                                        
01100                                  COPY ELCATTR.                    
01101      EJECT                                                        
01102                                  COPY ELCEMIB.                    
01103  01  EMI-SAVE-AREA               PIC X(400).                      
01104      EJECT                                                        
01105                                  COPY ELCINTF.                    
01106      12  PI-WA REDEFINES PI-PROGRAM-WORK-AREA.                    
01107          COPY ELC1042.                                            
01108 **********************************************************        
01109 *    NOTE                                                *        
01110 *        THE WORK AREA IS USED BY EL152 AND EL1522       *        
01111 *        AND CANNOT BE REARRANGED WITHOUT COMPILING      *        
01112 *        BOTH PROGRAMS.                                  *        
01113 *                                                        *        
01114 **********************************************************        
01115          16  PI-EL152-WA.                                         
01116              20  PI-ALT-PRINTER-ID        PIC X(4).               
01117              20  PI-ARCHIVE-NUMBER        PIC 9(8).               
01118              20  PI-FORM-NUMBER           PIC X(4).               
01119              20  PI-ADDR-TYPE             PIC XX.                 
01120              20  PI-TEMP-STOR-ID          PIC X(8).               
01121              20  PI-NUM-PRINT-COPIES      PIC 9.                  
01122              20  PI-ADDR-SEQ              PIC S9(4)   COMP.       
01123              20  PI-PRINT-SW              PIC X.                  
01124                  88 PRINT-PERFORMED  VALUE '1'.                   
01125              20  PI-LETTER-ADDRESS-TYPE   PIC XX.                 
040110             20  PI-RESEND-FORM-NUMBER    PIC X(4).
040110             20  PI-PROMPT-LETTER         PIC X.
040110             20  PI-ENCLOSURE-CD          PIC X(3).
040110             20  PI-AUTO-CLOSE-IND        PIC X(1).
040110             20  PI-LETTER-TO-BENE        PIC X(1).
040110             20  PI-FILLER                PIC X(528).             
01127              20  PI-FORCE-7840            PIC X.                  
01128                  88 FORCE-7840       VALUE '1'.                   
01129                                                                   
01130              20  PI-BSR-LETTER-IND        PIC XX.                 
01131                  88  PI-BSR-AUTOMATED VALUE 'BA'.                 
01132              20  FILLER REDEFINES PI-BSR-LETTER-IND.              
01133                  22  PI-ELLETR-BSR        PIC X.                  
01134                  22  PI-ELBENE-BSR        PIC X.                  
01135                                                                   
01136      EJECT                                                        
                                   COPY DFHBMSCA.

01137                              COPY ELCAID.                         
01138  01  FILLER    REDEFINES DFHAID.                                  
01139      12  FILLER              PIC X(8).                            
01140      12  PF-VALUES           PIC X       OCCURS 2.                
01141      EJECT                                                        
01142                              COPY EL152S.                         
01143  01  MAP-REDEF REDEFINES EL152AI.                                 
           12  FILLER              PIC X(251).
01144 *    12  FILLER              PIC X(189).                          
01145      12  EL152RI.                                                 
01146        14  TEXT-LINES OCCURS 13 TIMES INDEXED BY SC-INDX.         
01147          16  SC-LINEL        PIC S9(4)  COMP.                     
01148          16  SC-LINEA        PIC X.                               
01149          16  SC-LINE         PIC XXX.                             
01150          16  SC-TEXTL        PIC S9(4)  COMP.                     
01151          16  SC-TEXTA        PIC X.                               
01152          16  SC-TEXT         PIC X(70).                           
01153      EJECT                                                        
01154  01  RECORD-TABLE            PIC X(21900) VALUE SPACES.           
01155                                                                   
01156  01  REC-TABLE REDEFINES RECORD-TABLE.                            
01157      12  TS-GROUP OCCURS 6 TIMES INDEXED BY TS-INDX               
01158                              PIC X(3650).                         
01159                                                                   
01160  01  REC-ENTRIES REDEFINES RECORD-TABLE.                          
01161      12  REC-ENT OCCURS 300 TIMES INDEXED BY TB-INDX TB-INDX1.    
01162          16  REC-TEXT        PIC X(70).                           
01163          16  REC-PC          PIC 99.                              
01164          16  FILLER          PIC X.                               
01165                                                                   
01166  01  TS-WORK-AREA            PIC X(3650).                         
01167      EJECT                                                        
01168  LINKAGE SECTION.                                                 

01169  01  DFHCOMMAREA                 PIC X(1024).                     
01170                                                                   

       01  var  pic x(30).


01171      EJECT                                                        
01172                                  COPY ELCMSTR.                    
01173                                                                   
01174      EJECT                                                        
01175                                  COPY ELCCNTL.                    
01176                                                                   
01177      EJECT                                                        
01178                                  COPY ELCARCH.                    
01179                                                                   
01180      EJECT                                                        
01181                                  COPY ELCARCT.                    
01182                                                                   
01183      EJECT                                                        
01184                                  COPY ELCCERT.                    
01185                                                                   
01186      EJECT                                                        
01187                                  COPY ERCACCT.                    
01188                                                                   
01189      EJECT                                                        
01190                                  COPY ELCTEXT.                    
01191                                                                   
01192      EJECT                                                        
01193                                  COPY ELCTRLR.                    
01194                                                                   
01195      EJECT                                                        
01196                                  COPY ELCBENE.                    
01197                                                                   
01198      EJECT                                                        
01199                                  COPY ERCCOMP.                    
01200                                                                   
01201      EJECT                                                        
121802*                                COPY MPCPROD.                    
121802*    EJECT                                                        
121802*                                COPY MPCPLCY.                    
121802*    EJECT                                                        
121802*                                COPY MPCPLAN.                    
01207      EJECT                                                        
033110                                 COPY ELCNAPS.
033110     EJECT
041513                                 COPY ELCENCC.
041513     EJECT
01208 *  THE SYSTEM-VARIABLES  FIELD LENGTH MUST MATCH THE LENGTH OF    
01209 *  THE SS-WORK-AREA-LENGTH FIELD FOR THE VARIABLE-WORK-AREA       
01210                                                                   
010407*01  SYSTEM-VARIABLES            PIC X(3851).                     
010407*01  SYS-VAR-ENTRY REDEFINES SYSTEM-VARIABLES.                    
010407*    12  SYS-VAR-CODE            PIC X(4).                        
010407*    12  SYS-VAR-LEN             PIC 99.                          
010407*    12  SYS-VAR-DATA            PIC X(100).                      
01216                                                                   
01217      EJECT                                                        
01218  PROCEDURE DIVISION.                                              
01219                                                                   
01220      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               
01221      MOVE '5'                   TO DC-OPTION-CODE.                
01222      PERFORM 9700-DATE-LINK THRU 9700-EXIT.                       
01223      MOVE DC-GREG-DATE-1-EDIT   TO SAVE-DATE.                     
01224      MOVE DC-BIN-DATE-1         TO SAVE-BIN-DATE.                 
01225                                                                   
01226      MOVE DFHCOMMAREA           TO PROGRAM-INTERFACE-BLOCK.       
01227      MOVE 1                     TO EMI-NUMBER-OF-LINES.           
01228      MOVE ERROR-MESSAGE-INTERFACE-BLOCK                           
01229                                 TO EMI-SAVE-AREA.                 
01230                                                                   
01231      MOVE EIBTRMID              TO TS-TERM-TEXT                   
01232                                    TS-TERM-SCREEN                 
01233                                    QID-TERM.                      
01234                                                                   
01235      MOVE SAVE-BIN-DATE       TO CURRENT-SAVE.                    
01236                                                                   
121802*    IF PI-COMPANY-ID = 'DMD'                                     
121802*       MOVE CURRENT-SAVE           TO DC-BIN-DATE-1              
121802*       MOVE '6'                    TO DC-OPTION-CODE             
121802*       MOVE +3                     TO DC-ELAPSED-MONTHS          
121802*       MOVE +0                     TO DC-ELAPSED-DAYS            
121802*       PERFORM 9700-DATE-LINK THRU 9700-EXIT                     
121802*       MOVE DC-BIN-DATE-2          TO CURRENT-PLUS3-SAVE.        
01244                                                                   
01245      EXEC CICS HANDLE CONDITION                                   
01246          PGMIDERR (9600-PGMID-ERROR)                              
01247          MAPFAIL  (0325-MAPFAIL)                                  
01248          ERROR    (9990-ABEND)                                    
01249      END-EXEC.                                                    
01250                                                                   
01251      IF PI-PROCESSOR-ID NOT = 'LGXX'                              
01252          EXEC CICS READQ TS                                       
01253              QUEUE   (PI-SECURITY-TEMP-STORE-ID)                  
01254              INTO    (SECURITY-CONTROL)                           
01255              LENGTH  (SC-COMM-LENGTH)                             
01256              ITEM    (SC-ITEM)                                    
01257          END-EXEC                                                 
01258          MOVE SC-CLAIMS-DISPLAY (7)  TO  PI-DISPLAY-CAP           
01259          MOVE SC-CLAIMS-UPDATE  (7)  TO  PI-MODIFY-CAP.           
01260                                                                   


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

01261      IF EIBCALEN = 0                                              
01262          GO TO 8800-UNAUTHORIZED-ACCESS.                          
01263                                                                   
01264      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         
01265          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   
01266              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      
01267              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      
01268              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      
01269              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      
01270              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      
01271              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      
01272              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    
01273              MOVE THIS-PGM             TO PI-CALLING-PROGRAM      
01274              PERFORM 7750-DELETE-TEMP-STOR         THRU 7750-EXIT 
01275              PERFORM 7760-DELETE-TEMP-STOR-SCREEN  THRU 7760-EXIT 
01276              PERFORM 7770-DELETE-TEMP-STOR-PI-AREA THRU 7770-EXIT 
01277              MOVE SPACES              TO PI-WA                    
01278                                          PI-FORCE-7840            
01279              MOVE ZEROS               TO PI-TOTAL-LINES           
01280                                          PI-CURRENT-LINE          
01281                                          PI-TEMP-STOR-ITEMS       
01282                                          PI-UPDATE-SW             
01283                                          PI-ADDR-SEQ              
01284              MOVE '2'                 TO PI-ACTION                
01285              MOVE LOW-VALUES          TO EL152AO                  
01286                                          PI-ADDR-TYPE             
01287              GO TO 8100-SEND-INITIAL-MAP                          
01288          ELSE                                                     
01289              MOVE PI-CALLING-PROGRAM   TO PGM-NAME                
01290              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      
01291              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    
01292              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      
01293              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      
01294              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      
01295              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      
01296              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      
01297              MOVE SPACES               TO PI-SAVED-PROGRAM-6      
01298              PERFORM 7500-READ-TS           THRU 7599-EXIT        
01299              PERFORM 7600-READ-SCREEN-TS    THRU 7699-EXIT        
01300              IF PGM-NAME = PGM-EL141                              
01301                 PERFORM 500-RECOVER-PI-TEMP-STORAGE THRU 599-EXIT 
01302                 SET TB-INDX TO PI-CURRENT-LINE                    
01303                 PERFORM 400-SET-CODES       THRU 499-EXIT         
01304                 PERFORM 7000-READ-ADDR      THRU 7099-EXIT        
01305                 PERFORM 7170-FORMAT-SCREEN  THRU 7170-EXIT        
01306                         VARYING SC-INDX FROM 1 BY 1 UNTIL         
01307                         SC-INDX > NUM-LINES-PER-SCREEN            
01308                 MOVE PI-ALT-PRINTER-ID  TO PRINTERO               
01309                 MOVE AL-UANON           TO PRINTERA               
01310                 GO TO 8100-SEND-INITIAL-MAP                       
01311              ELSE                                                 
01312                 SET TB-INDX TO PI-CURRENT-LINE                    
01313                 PERFORM 7170-FORMAT-SCREEN   THRU  7170-EXIT      
01314                         VARYING SC-INDX FROM 1 BY 1 UNTIL         
01315                         SC-INDX > NUM-LINES-PER-SCREEN            
01316                 MOVE PI-ALT-PRINTER-ID  TO PRINTERO               
01317                 MOVE AL-UANON           TO PRINTERA               
01318                 GO TO 8100-SEND-INITIAL-MAP.                      
01319                                                                   
01320 *0100-PA.                                                         
01321 *    MOVE ER-0008                TO EMI-ERROR.                    
01322 *    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01323 *    MOVE -1                     TO MAINTL.                       
01324 *    GO TO 8200-SEND-DATAONLY.                                    
01325                                                                   
01326      EJECT                                                        
01327  0200-RECEIVE.                                                    
01328      MOVE TRANS-ID         TO EIBTRNID.                           
01329      MOVE EMI-SAVE-AREA    TO ERROR-MESSAGE-INTERFACE-BLOCK.      
01330      MOVE LOW-VALUES       TO EL152AI.                            
01331      MOVE '104A'           TO TS-ID-TEXT.                         
01332      MOVE EIBTRMID         TO TS-TERM-TEXT                        
01333                               TS-TERM-SCREEN                      
01334                               QID-TERM.                           
01335                                                                   
01336      EXEC CICS HANDLE AID                                         
01337          CLEAR    (9400-CLEAR)                                    
01338          PA1      (0100-PA)                                       
01339          PA2      (0100-PA)                                       
01340          PA3      (0100-PA)                                       
01341      END-EXEC.                                                    
01342                                                                   
01343      EXEC CICS HANDLE CONDITION                                   
01344          MAPFAIL  (0325-MAPFAIL)                                  
01345      END-EXEC.                                                    
01346                                                                   
01347      EXEC CICS SYNCPOINT                                          
01348      END-EXEC.                                                    
01349                                                                   
01350      IF LOWER-CASE-LETTERS-USED                                   
01351         EXEC CICS RECEIVE                                         
01352              MAP      (MAP-NAME)                                  
01353              MAPSET   (MAPSET-NAME)                               
01354              INTO     (EL152AI)                                   
01355              ASIS                                                 
01356         END-EXEC                                                  
01357       ELSE                                                        
01358         EXEC CICS RECEIVE                                         
01359              MAP      (MAP-NAME)                                  
01360              MAPSET   (MAPSET-NAME)                               
01361              INTO     (EL152AI)                                   
01362         END-EXEC.                                                 
01363                                                                   
01364      IF NOT DISPLAY-CAP AND PI-PROCESSOR-ID NOT = 'LGXX'          
01365          MOVE 'READ'             TO  SM-READ                      
01366          PERFORM 9995-SECURITY-VIOLATION                          
01367          MOVE ER-0070            TO  EMI-ERROR                    
01368          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
01369          GO TO 8100-SEND-INITIAL-MAP.                             
01370                                                                   
01371      INSPECT MAINTI CONVERTING LOWER-CASE TO UPPER-CASE.          
           MOVE 'N'                    TO CLEANI
01372                                                                   
01373      IF ENTERPFL = 0                                              
01374          GO TO 0300-CHECK-PFKEYS.                                 
01375      IF EIBAID NOT = DFHENTER                                     
01376          MOVE ER-0004            TO EMI-ERROR                     
01377          GO TO 0320-INPUT-ERROR.                                  
01378      IF (ENTERPFI NUMERIC) AND (ENTERPFI > 0 AND < 25)            
01379          MOVE PF-VALUES (ENTERPFI) TO EIBAID                      
01380      ELSE                                                         
01381          MOVE ER-0029            TO EMI-ERROR                     
01382          GO TO 0320-INPUT-ERROR.                                  
01383                                                                   
01384  0300-CHECK-PFKEYS.                                               
01385      IF EIBAID = DFHPF23                                          
01386          GO TO 8810-PF23.                                         
01387      IF EIBAID = DFHPF24                                          
01388          GO TO 9200-RETURN-MAIN-MENU.                             
01389 *    IF EIBAID = DFHPF12                                          
01390 *        GO TO 9500-PF12.                                         
01391      IF EIBAID = DFHPF1                                           
01392          MOVE 7                  TO ROLL-COUNTER                  
01393          GO TO 7900-ROLL-PAGE.                                    
01394      IF EIBAID = DFHPF2                                           
01395          MOVE -7                 TO ROLL-COUNTER                  
01396          GO TO 7900-ROLL-PAGE.                                    
01397      IF EIBAID = DFHPF3                                           
01398          GO TO 6100-ADDR-MAINT.                                   
01399      IF EIBAID = DFHPF4                                           
01400          IF MODIFY-CAP                                            
01401              GO TO 6200-EDIT-MODE                                 
01402          ELSE                                                     
01403              MOVE 'UPDATE'       TO  SM-READ                      
01404              PERFORM 9995-SECURITY-VIOLATION                      
01405              MOVE ER-0070        TO  EMI-ERROR                    
01406              GO TO 0320-INPUT-ERROR.                              
01407                                                                   
01408      IF EIBAID = DFHPF5                                           
01409          GO TO 0330-FUNCTION-CHECK.                               
01410                                                                   
01411      IF EIBAID = DFHPF6                                           
01412          GO TO 0330-FUNCTION-CHECK.                               
01413                                                                   
01414      IF EIBAID = DFHPF7                                           
01415         COMPUTE ROLL-COUNTER = ((PI-TOTAL-LINES - 1) * -1)        
01416         GO TO 7900-ROLL-PAGE.                                     
01417                                                                   
01418      IF EIBAID = DFHPF8                                           
01419         MOVE PI-TOTAL-LINES TO ROLL-COUNTER                       
01420         GO TO 7900-ROLL-PAGE.                                     
01421                                                                   
121802*    IF EIBAID = DFHPF9 AND                                       
121802*       PI-COMPANY-ID = 'DMD'                                     
121802*        MOVE '1'           TO PI-FORCE-7840                      
121802*        GO TO 0330-FUNCTION-CHECK.                               
01426                                                                   
01427      IF EIBAID = DFHENTER                                         
01428          GO TO 0330-FUNCTION-CHECK.                               
01429                                                                   
030805     PERFORM 7500-READ-TS        THRU 7599-EXIT
030805
030805     PERFORM 7950-SET-INDX THRU 7950-EXIT.                        
030805     PERFORM 7960-UPDATE-TABLE-FROM-SCREEN THRU 7960-EXIT         
030805             VARYING SC-INDX FROM 1 BY 1                          
030805             UNTIL SC-INDX > NUM-LINES-PER-SCREEN.                
030805                                                                  
01430      MOVE ER-0029                TO EMI-ERROR.                    
01431  0320-INPUT-ERROR.                                                
01432      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01433      MOVE AL-UNBON               TO ENTERPFA.                     
01434      IF ENTERPFL = 0                                              
01435          MOVE -1                 TO MAINTL                        
01436      ELSE                                                         
01437          MOVE -1                 TO ENTERPFL.                     
01438                                                                   
01439      GO TO 8200-SEND-DATAONLY.                                    
01440                                                                   

081004 0100-PA.                                                         
081004     MOVE ER-0008                TO EMI-ERROR.                    
081004     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
081004     MOVE -1                     TO MAINTL.                       
081004     GO TO 8200-SEND-DATAONLY.                                    
081004                                                                  

01441  0325-MAPFAIL.                                                    
01442 ***********************************************************       
01443 *      ROUTINE SHOULD ONLY BE PERFORMED WHEN PRINTING     *       
01444 *      LETTERS ON A 3275 PRINTER.                         *       
01445 ***********************************************************       
01446                                                                   
01447      PERFORM 7600-READ-SCREEN-TS    THRU 7699-EXIT.               
01448                                                                   
01449      SET TB-INDX         TO PI-CURRENT-LINE.                      
01450      PERFORM 7170-FORMAT-SCREEN   THRU  7170-EXIT                 
01451                   VARYING SC-INDX FROM 1 BY 1 UNTIL               
01452                   SC-INDX > NUM-LINES-PER-SCREEN.                 
01453                                                                   
01454      GO TO 8100-SEND-INITIAL-MAP.                                 
01455                                                                   
01456      EJECT                                                        
01457  0330-FUNCTION-CHECK.                                             
01458      IF NOT MODIFY-CAP                                            
01459          IF MAINTI = 'S'                                          
01460              NEXT SENTENCE                                        
01461          ELSE                                                     
01462              MOVE 'UPDATE'           TO  SM-READ                  
01463              PERFORM 9995-SECURITY-VIOLATION                      
01464              MOVE ER-0070            TO  EMI-ERROR                
01465              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
01466              GO TO 8100-SEND-INITIAL-MAP.                         
01467                                                                   
01468      PERFORM 0350-EDIT-ROUTINE THRU 0350-EXIT.                    
01469                                                                   
01470      IF EIBAID = DFHPF5
011212      IF PI-PROMPT-LETTER = 'Y'
040110         MOVE ER-0894            TO EMI-ERROR
040110         MOVE -1             TO MAINTL                        
040110         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
040110         MOVE AL-UABON       TO MAINTA                        
040110         GO TO 8200-SEND-DATAONLY                             
040110      ELSE
01471          IF MAINTI NOT = 'C'                                      
01472              MOVE ER-0716        TO EMI-ERROR                     
01473              MOVE -1             TO MAINTL                        
01474              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
01475              MOVE AL-UABON       TO MAINTA                        
01476              GO TO 8200-SEND-DATAONLY                             
01477          ELSE                                                     
01478              IF MODIFY-CAP                                        
01479                  GO TO 6400-LETTER-RELEASE                        
01480              ELSE                                                 
01481                  IF PRINT-PERFORMED                               
01482                      GO TO 6400-LETTER-RELEASE                    
01483                  ELSE                                             
01484                      MOVE AL-UNBON TO ENTERPFA                    
01485                      MOVE -1       TO ENTERPFL                    
01486                      MOVE ER-2398 TO EMI-ERROR                    
01487                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     
01488                      GO TO 8200-SEND-DATAONLY.                    
01489                                                                   
01490      IF EIBAID = DFHPF6                                           
011212      IF PI-PROMPT-LETTER = 'Y'
040110         MOVE ER-0894            TO EMI-ERROR
040110         MOVE -1             TO MAINTL                        
040110         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
040110         MOVE AL-UABON       TO MAINTA                        
040110         GO TO 8200-SEND-DATAONLY                             
040110      ELSE
01491          GO TO 7800-PRINT-LETTER-NOW.                             
01492                                                                   
01493      IF MAINTI = 'S'                                              
01494          GO TO 1000-SHOW.                                         
01495                                                                   
01496      IF MAINTI = 'C'                                              
01497          GO TO 2000-CREATE.                                       
01498                                                                   
01499      IF MAINTI = 'R'                                              
01500         IF NOT MODIFY-CAP                                         
01501            MOVE ER-0070          TO EMI-ERROR                     
01502            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
01503            MOVE LOW-VALUES       TO EL152AO                       
01504            GO TO 8100-SEND-INITIAL-MAP                            
01505           ELSE                                                    
01506            GO TO 3000-RECORD.                                     
01507                                                                   
PEMTST     PERFORM 7500-READ-TS        THRU 7599-EXIT
PEMTST     PERFORM 7950-SET-INDX       THRU 7950-EXIT
PEMTST     PERFORM 7960-UPDATE-TABLE-FROM-SCREEN
                                       THRU 7960-EXIT
PEMTST        VARYING SC-INDX FROM 1 BY 1 UNTIL
PEMTST        SC-INDX > NUM-LINES-PER-SCREEN

01508      MOVE ER-0023                TO EMI-ERROR.                    
01509      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01510      MOVE -1                     TO MAINTL.                       
01511      MOVE AL-UABON               TO MAINTA.                       
01512      GO TO 8200-SEND-DATAONLY.                                    
01513                                                                   
01514      EJECT                                                        
01515  0350-EDIT-ROUTINE.                                               
01516      PERFORM 400-SET-CODES THRU 499-EXIT.                         
01517      MOVE PI-CLAIM-NO    TO CLAM-CLAIM                            
01518                             ACTV-CLAIM.                           
01519                                                                   
01520      IF ADDRL NOT = +0                                            
01521         INSPECT ADDRI CONVERTING LOWER-CASE TO UPPER-CASE         
01522         MOVE ADDRI           TO WS-ADDR-TYPE-CD                   
01523         IF WS-ADDR-TYPE = 'I' OR 'B' OR 'A' OR 'P' OR             
01524                           'O' OR 'Q' OR 'E'                       
01525            MOVE AL-UANON         TO ADDRA                         
01526         ELSE                                                      
01527            MOVE -1               TO ADDRL                         
01528            MOVE AL-UABON         TO ADDRA                         
01529            MOVE ER-0176          TO EMI-ERROR                     
01530            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              
121802*     ELSE                                                        
121802*       IF PI-COMPANY-ID = 'DMD'                                  
121802*        IF MAINTI = 'C' OR 'R'                                   
121802*          MOVE -1               TO ADDRL                         
121802*          MOVE AL-UABON         TO ADDRA                         
121802*          MOVE ER-0861          TO EMI-ERROR                     
121802*          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              
01538                                                                   
01539      IF ADDRL NOT = +0                                            
01540         IF WS-ADDR-SEQ NOT NUMERIC                                
01541            MOVE -1               TO ADDRL                         
01542            MOVE AL-UABON         TO ADDRA                         
01543            MOVE ER-0176          TO EMI-ERROR                     
01544            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              
01545                                                                   
121802*    IF ADDRL NOT = +0                                            
121802*       IF PI-COMPANY-ID = 'DMD'                                  
121802*          IF WS-ADDR-TYPE = 'B' AND                              
121802*             WS-ADDR-SEQ NOT = '9'                               
121802*                MOVE -1               TO ADDRL                   
121802*                MOVE AL-UABON         TO ADDRA                   
121802*                MOVE ER-7842          TO EMI-ERROR               
121802*                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.        
01554                                                                   
01555      IF FORML NOT = ZEROS                                         
01556         INSPECT FORMI CONVERTING LOWER-CASE TO UPPER-CASE.        
01557                                                                   
01558      IF MAINTI = 'C' AND FORML = ZEROS                            
01559         MOVE -1                  TO FORML                         
01560         MOVE ER-0177             TO EMI-ERROR                     
01561         MOVE AL-UABON            TO FORMA                         
01562         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 
01563                                                                   
01564      IF FOLLOWL NOT = ZEROS                                       
01565         MOVE FOLLOWI             TO DEEDIT-FIELD                  
01566         PERFORM 8600-DEEDIT                                       
01567         MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY            
01568         MOVE '4'                 TO DC-OPTION-CODE                
01569         PERFORM 9700-DATE-LINK  THRU  9700-EXIT                   
01570         IF DATE-CONVERSION-ERROR                                  
01571            MOVE ER-0182          TO EMI-ERROR                     
01572            MOVE -1               TO FOLLOWL                       
01573            MOVE AL-UABON         TO FOLLOWA                       
01574            PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT             
01575         ELSE                                                      
121802*          IF (PI-COMPANY-ID = 'DMD') AND                         
121802*             (NOT FORCE-7840)        AND                         
121802*             (DC-BIN-DATE-1 > CURRENT-PLUS3-SAVE)                
121802*              MOVE 'PF9=FORCE 7840' TO PFKEY9O                   
121802*              MOVE AL-SABON         TO PFKEY9A                   
121802*              MOVE ER-7840          TO EMI-ERROR                 
121802*              MOVE -1               TO FOLLOWL                   
121802*              MOVE AL-UABON         TO FOLLOWA                   
121802*              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT         
121802*          ELSE                                                   
01586              MOVE SPACES                TO PFKEY9O                
01587              MOVE AL-SADON              TO PFKEY9A                
01588              MOVE DC-GREG-DATE-1-EDIT   TO FOLLOWO                
01589              MOVE AL-UANON              TO FOLLOWA                
01590              MOVE DC-BIN-DATE-1         TO FOLLOW-UP-SAVE         
01591      ELSE                                                         
01592         MOVE LOW-VALUES                 TO FOLLOW-UP-SAVE.        
01593                                                                   
01594      IF FOLLOW-UP-SAVE NOT = LOW-VALUES                           
01595         IF FOLLOW-UP-SAVE NOT > CURRENT-SAVE                      
01596            MOVE ER-0533          TO EMI-ERROR                     
01597            MOVE AL-UABON         TO FOLLOWA                       
01598            MOVE -1               TO FOLLOWL                       
01599            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              
01600                                                                   
01601      IF RESENDL NOT = ZEROS                                       
01602         MOVE RESENDI             TO DEEDIT-FIELD                  
01603         PERFORM 8600-DEEDIT                                       
01604         MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY            
01605         MOVE '4'                 TO DC-OPTION-CODE                
01606         PERFORM 9700-DATE-LINK  THRU  9700-EXIT                   
01607         IF DATE-CONVERSION-ERROR                                  
01608            MOVE ER-0185          TO EMI-ERROR                     
01609            MOVE -1               TO RESENDL                       
01610            MOVE AL-UABON         TO RESENDA                       
01611            PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT             
01612          ELSE                                                     
121802*          IF (PI-COMPANY-ID = 'DMD') AND                         
121802*             (NOT FORCE-7840)        AND                         
121802*             (DC-BIN-DATE-1 > CURRENT-PLUS3-SAVE)                
121802*              MOVE 'PF9=FORCE 7840' TO PFKEY9O                   
121802*              MOVE AL-SABON         TO PFKEY9A                   
121802*              MOVE ER-7840          TO EMI-ERROR                 
121802*              MOVE -1               TO RESENDL                   
121802*              MOVE AL-UABON         TO RESENDA                   
121802*              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT         
121802*          ELSE                                                   
01623              MOVE SPACES                 TO PFKEY9O               
01624              MOVE AL-SADON               TO PFKEY9A               
01625              MOVE DC-GREG-DATE-1-EDIT    TO RESENDO               
01626              MOVE AL-UANON               TO RESENDA               
01627              MOVE DC-BIN-DATE-1          TO RESEND-SAVE           
01628      ELSE                                                         
01629         MOVE LOW-VALUE                   TO RESEND-SAVE.          
01630                                                                   
01631      IF RESEND-SAVE NOT = LOW-VALUES                              
01632         IF RESEND-SAVE NOT > CURRENT-SAVE                         
01633            MOVE ER-0537          TO EMI-ERROR                     
01634            MOVE AL-UABON         TO RESENDA                       
01635            MOVE -1               TO RESENDL                       
01636            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              
01637                                                                   
01638      IF PRINTL NOT = ZEROS                                        
01639         INSPECT PRINTI CONVERTING LOWER-CASE TO UPPER-CASE        
01640         IF PRINTI = 'Y' OR = ' '                                  
01641            MOVE AL-UANON         TO PRINTA                        
01642         ELSE                                                      
01643            MOVE ER-0183          TO EMI-ERROR                     
01644            MOVE -1               TO PRINTL                        
01645            MOVE AL-UABON         TO PRINTA                        
01646            PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.            
01647                                                                   
           MOVE 'Y'                    TO CLEANO
           IF ENCL <= ZEROS
              MOVE 'N'                 TO CLEANO
           END-IF

           IF ENCL > ZEROS
              MOVE FUNCTION UPPER-CASE(ENCI) TO ENCI
                                                PI-ENCLOSURE-CD
041513        MOVE SPACES             TO ELENCC-KEY
041513        MOVE PI-COMPANY-CD      TO ELENCC-COMPANY-CD
041513        MOVE '1'                TO ELENCC-REC-TYPE
041513        MOVE ENCI               TO ELENCC-ENC-CODE
041513
041513        EXEC CICS READ
041513            DATASET    (ENCC-ID)
041513            SET        (ADDRESS OF ENCLOSURE-CODES)
041513            RIDFLD     (ELENCC-KEY)
041513            RESP       (WS-RESPONSE)
041513        END-EXEC
041513
041513        IF RESP-NORMAL
                 MOVE ENCI             TO PI-ENCLOSURE-CD
                                          W-ENCLOSURE-CD
                 MOVE AL-UANON         TO ENCA
              ELSE
                 MOVE 'N'              TO CLEANO
                 MOVE ER-1560          TO EMI-ERROR
                 MOVE -1               TO ENCL
                 MOVE AL-UABON         TO ENCA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF

01648      IF COPIESL NOT = ZEROS                                       
01649         IF COPIESI NOT NUMERIC OR                                 
01650            COPIESI = '0'                                          
01651            MOVE ER-0184          TO EMI-ERROR                     
01652            MOVE -1               TO COPIESL                       
01653            MOVE AL-UABON         TO COPIESA                       
01654            PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT             
01655         ELSE                                                      
01656            MOVE AL-UANON         TO COPIESA.                      
01657                                                                   
01658      IF PI-PROCESSOR-PRINTER NOT = SPACES                         
01659          MOVE PI-PROCESSOR-PRINTER   TO  PI-ALT-PRINTER-ID.       
01660                                                                   
01661      MOVE SPACES                 TO PI-ALT-DMD-PRT-ID.            
01662                                                                   
01663      IF PRINTERL > ZERO                                           
01664         INSPECT PRINTERI CONVERTING LOWER-CASE TO UPPER-CASE      
01665         MOVE AL-UANON            TO PRINTERA                      
01666         MOVE PRINTERI            TO PI-ALT-PRINTER-ID             
01667                                     PI-ALT-DMD-PRT-ID             
01668      ELSE                                                         
01669         IF (PI-NO-CARRIER-SECURITY AND                            
01670             PI-NO-ACCOUNT-SECURITY) OR                            
01671             PI-PROCESSOR-PRINTER NOT = SPACES                     
01672              NEXT SENTENCE                                        
01673         ELSE                                                      
01674             MOVE AL-UABON       TO PRINTERA                       
01675             MOVE -1             TO PRINTERL                       
01676             MOVE ER-2397        TO EMI-ERROR                      
01677             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.             
01678                                                                   
01679      IF ACTL NOT = +0                                             
01680          MOVE ZEROS              TO W-ACCOUNT                     
01681          IF ACTI > SPACES                                         
01682              IF ACTI NUMERIC                                      
01683                      AND                                          
01684                 ACTI NOT < 1                                      
01685                      AND                                          
01686                 ACTI NOT > 9                                      
01687                  MOVE AL-UANON   TO ACTA                          
01688                  MOVE ACTI       TO W-ACCOUNT                     
01689                                     W-LETTER-ADDR-SEQ             
01690                  MOVE 'A'        TO W-LETTER-ADDR-TYPE            
01691              ELSE                                                 
01692                  MOVE -1         TO ACTL                          
01693                  MOVE AL-UABON   TO ACTA                          
01694                  MOVE ER-3547    TO EMI-ERROR                     
01695                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.        
01696                                                                   
01697      IF BENL NOT = +0                                             
01698          MOVE ZEROS              TO W-BENEFICIARY                 
01699          IF BENI > SPACES                                         
01700              IF BENI NUMERIC                                      
01701                      AND                                          
01702                 BENI NOT < 1                                      
01703                      AND                                          
01704                 BENI NOT > 9                                      
01705                  MOVE AL-UANON   TO BENA                          
01706                  MOVE BENI       TO W-BENEFICIARY                 
01707                                     W-LETTER-ADDR-SEQ             
01708                  MOVE 'B'        TO W-LETTER-ADDR-TYPE            
01709              ELSE                                                 
01710                  MOVE -1         TO BENL                          
01711                  MOVE AL-UABON   TO BENA                          
01712                  MOVE ER-3547    TO EMI-ERROR                     
01713                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.        
01714                                                                   
01715      IF EMPL NOT = +0                                             
01716          MOVE ZEROS              TO W-EMPLOYER                    
01717          IF EMPI > SPACES                                         
01718              IF EMPI NUMERIC                                      
01719                      AND                                          
01720                 EMPI NOT < 1                                      
01721                      AND                                          
01722                 EMPI NOT > 9                                      
01723                  MOVE AL-UANON   TO EMPA                          
01724                  MOVE EMPI       TO W-EMPLOYER                    
01725                                     W-LETTER-ADDR-SEQ             
01726                  MOVE 'E'        TO W-LETTER-ADDR-TYPE            
01727              ELSE                                                 
01728                  MOVE -1         TO EMPL                          
01729                  MOVE AL-UABON   TO EMPA                          
01730                  MOVE ER-3547    TO EMI-ERROR                     
01731                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.        
01732                                                                   
01733      IF INSL NOT = +0                                             
01734          MOVE ZEROS              TO W-INSURED                     
01735          IF INSI > SPACES                                         
01736              IF INSI NUMERIC                                      
01737                      AND                                          
01738                 INSI NOT < 1                                      
01739                      AND                                          
01740                 INSI NOT > 9                                      
01741                  MOVE AL-UANON   TO INSA                          
01742                  MOVE INSI       TO W-INSURED                     
01743                                     W-LETTER-ADDR-SEQ             
01744                  MOVE 'I'        TO W-LETTER-ADDR-TYPE            
01745              ELSE                                                 
01746                  MOVE -1         TO INSL                          
01747                  MOVE AL-UABON   TO INSA                          
01748                  MOVE ER-3547    TO EMI-ERROR                     
01749                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.        
01750                                                                   
01751      IF PHYSL NOT = +0                                            
01752          MOVE ZEROS              TO W-PHYSICIAN                   
01753          IF PHYSI > SPACES                                        
01754              IF PHYSI NUMERIC                                     
01755                      AND                                          
01756                 PHYSI NOT < 1                                     
01757                      AND                                          
01758                 PHYSI NOT > 9                                     
01759                  MOVE AL-UANON   TO PHYSA                         
01760                  MOVE PHYSI      TO W-PHYSICIAN                   
01761                                     W-LETTER-ADDR-SEQ             
01762                  MOVE 'P'        TO W-LETTER-ADDR-TYPE            
01763              ELSE                                                 
01764                  MOVE -1         TO PHYSL                         
01765                  MOVE AL-UABON   TO PHYSA                         
01766                  MOVE ER-3547    TO EMI-ERROR                     
01767                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.        
01768                                                                   
01769      IF OTHR1L NOT = +0                                           
01770          MOVE ZEROS              TO W-OTHER-1                     
01771          IF OTHR1I > SPACES                                       
01772              IF OTHR1I NUMERIC                                    
01773                      AND                                          
01774                 OTHR1I NOT < 1                                    
01775                      AND                                          
01776                 OTHR1I NOT > 9                                    
01777                  MOVE AL-UANON   TO OTHR1A                        
01778                  MOVE OTHR1I     TO W-OTHER-1                     
01779                                     W-LETTER-ADDR-SEQ             
01780                  MOVE 'O'        TO W-LETTER-ADDR-TYPE            
01781              ELSE                                                 
01782                  MOVE -1         TO OTHR1L                        
01783                  MOVE AL-UABON   TO OTHR1A                        
01784                  MOVE ER-3547    TO EMI-ERROR                     
01785                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.        
01786                                                                   
01787      IF OTHR2L NOT = +0                                           
01788          MOVE ZEROS              TO W-OTHER-2                     
01789          IF OTHR2I > SPACES                                       
01790              IF OTHR2I NUMERIC                                    
01791                      AND                                          
01792                 OTHR2I NOT < 1                                    
01793                      AND                                          
01794                 OTHR2I NOT > 9                                    
01795                  MOVE AL-UANON   TO OTHR2A                        
01796                  MOVE OTHR2I     TO W-OTHER-2                     
01797                                     W-LETTER-ADDR-SEQ             
01798                  MOVE 'Q'        TO W-LETTER-ADDR-TYPE            
01799              ELSE                                                 
01800                  MOVE -1         TO OTHR2L                        
01801                  MOVE AL-UABON   TO OTHR2A                        
01802                  MOVE ER-3547    TO EMI-ERROR                     
01803                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.        
01804                                                                   
01805      IF NOT EMI-NO-ERRORS                                         
01806         GO TO 8200-SEND-DATAONLY.                                 
01807                                                                   
01808  0350-EXIT.                                                       
01809       EXIT.                                                       
01810      EJECT                                                        
01811  400-SET-CODES.                                                   
01812      MOVE PI-COMPANY-ID          TO CNTL-CO.                      
01813      MOVE PI-COMPANY-CD          TO CLAM-CO 
01814                                     TEXT-CO                       
01815                                     ACTV-CO                       
01816                                     CERT-CO                       
01817                                     ACCT-CO                       
01818                                     ARCH-CO                       
01819                                     PROD-CO                       
01820                                     PLCY-CO                       
01821                                     PLAN-CO.                      
01822                                                                   
01823      MOVE PI-CARRIER             TO CLAM-CARRIER                  
01824                                     ACTV-CARRIER                  
01825                                     CERT-CARRIER                  
01826                                     ACCT-CARRIER                  
01827                                     PROD-CARRIER                  
01828                                     PLCY-CARRIER                  
01829                                     PLAN-CARRIER.                 
01830                                                                   
01831      MOVE PI-CERT-NO             TO CLAM-CERT-NUM                 
01832                                     ACTV-CERT-NUM                 
01833                                     CERT-CERT-NUM.                
01834  499-EXIT.                                                        
01835       EXIT.                                                       
01836                                                                   
01837  500-RECOVER-PI-TEMP-STORAGE.                                     
01838                                                                   
01839      EXEC CICS HANDLE CONDITION                                   
01840          QIDERR   (590-QIDERR)                                    
01841      END-EXEC.                                                    
01842                                                                   
01843      EXEC CICS READQ TS                                           
01844          QUEUE    (WS-PI-QID)                                     
01845          INTO     (PROGRAM-INTERFACE-BLOCK)                       
01846          LENGTH   (PI-COMM-LENGTH)                                
01847      END-EXEC.                                                    
01848                                                                   
01849      PERFORM 7770-DELETE-TEMP-STOR-PI-AREA THRU 7770-EXIT.        
01850                                                                   
01851      GO TO 599-EXIT.                                              
01852                                                                   
01853  590-QIDERR.                                                      
01854                                                                   
01855      IF EIBTRNID = TRANS-ID                                       
01856          MOVE ER-0033                   TO  EMI-ERROR             
01857          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
01858          GO TO 8200-SEND-DATAONLY.                                
01859                                                                   
01860  599-EXIT.                                                        
01861      EXIT.                                                        
01862                                                                   
01863      EJECT                                                        
01864  1000-SHOW.                                                       
01865 ***************************************************************   
01866 *     THIS ROUTINE WILL BROWSE THE ARCHIVE FILE WITH THE      *   
01867 *     ARCHIVE NUMBER SPECIFIED FROM THE SCREEN. THE TEXT      *   
01868 *     WILL BE INSERTED INTO THE TS-TABLE AND DISPLAYED TO     *   
01869 *     OPERATOR.                                               *   
01870 ***************************************************************   
01871                                                                   
01872      MOVE SPACES                 TO RECORD-TABLE.                 
01873      IF ARCHNUML = ZEROS                                          
01874         MOVE -1                  TO ARCHNUML                      
01875         MOVE AL-UNBON            TO ARCHNUMA                      
01876         MOVE ER-0174             TO EMI-ERROR                     
01877         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
01878         GO TO 8200-SEND-DATAONLY.                                 
01879                                                                   
01880      IF ARCHNUMI NOT NUMERIC                                      
01881         MOVE -1                  TO ARCHNUML                      
01882         MOVE AL-UNBON            TO ARCHNUMA                      
01883         MOVE ER-0175             TO EMI-ERROR                     
01884         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
01885         GO TO 8200-SEND-DATAONLY.                                 
01886                                                                   
01887      IF PI-SHOW-MODE                                              
042605*       IF PI-ARCHIVE-NUMBER = ARCHNUMI                           
042605*          MOVE -1               TO MAINTL                        
042605*          GO TO 8200-SEND-DATAONLY                               
042605*       ELSE                                                      
01892            PERFORM 7750-DELETE-TEMP-STOR THRU 7750-EXIT.          
01893                                                                   
01894      MOVE ARCHNUMI               TO ARCH-NUMBER                   
01895                                     PI-ARCHIVE-NUMBER.            
01896      MOVE ' '                    TO ARCH-REC-TYPE.                
01897      MOVE '1'                    TO PI-ACTION.                    
01898      MOVE ZEROS                  TO PI-ADDR-SEQ.                  
01899      MOVE LOW-VALUES             TO PI-ADDR-TYPE.                 
01900      MOVE SPACES                 TO PI-PRINT-SW                   
01901                                     PI-FORM-NUMBER.               
01902      SET TB-INDX TO 1.                                            
CIDMOD                                                                  
CIDMOD     MOVE SPACE                  TO WS-LETTER-STATUS.             
CIDMOD                                                                  
01903      EXEC CICS HANDLE CONDITION                                   
01904           NOTOPEN    (8850-ARCH-NOT-OPEN)                         
01905           NOTFND     (1020-ENDBR)                                 
01906           ENDFILE    (1020-ENDBR)                                 
01907      END-EXEC.                                                    
01908                                                                   
01909      MOVE ARCH-PARTIAL-KEY       TO ARCH-SAVE-KEY.                
01910                                                                   
01911      EXEC CICS STARTBR                                            
01912           DATASET    (ARCH-ID)                                    
01913           RIDFLD     (ARCH-KEY)                                   
01914           GTEQ                                                    
01915      END-EXEC                                                     
01916                                                                   
01917      MOVE 'Y'                    TO ARCH-BROWSE-STARTED.          
01918                                                                   
01919  1010-READ-NEXT.                                                  
01920      EXEC CICS READNEXT                                           
01921           SET       (ADDRESS OF LETTER-ARCHIVE)                   
01922           DATASET   (ARCH-ID)                                     
01923           RIDFLD    (ARCH-KEY)                                    
01924      END-EXEC.                                                    
01925                                                                   
01926      IF LA-FORM-CONTROL-HDR                                       
01927         GO TO 1020-ENDBR.                                         
01928                                                                   
CIDMOD                                                                  
CIDMOD     IF NOT LA-HEADER-DATA                                        
CIDMOD         NEXT SENTENCE                                            
CIDMOD     ELSE                                                         
CIDMOD         IF NOT LA-CSO-LETTER-PURGED                              
CIDMOD             NEXT SENTENCE                                        
CIDMOD         ELSE                                                     
CIDMOD             MOVE 'P'            TO WS-LETTER-STATUS              
CIDMOD             GO TO 1020-ENDBR.                                    
CIDMOD                                                                  
01929      IF ARCH-PARTIAL-KEY = ARCH-SAVE-KEY                          
01930         IF LA-HEADER-DATA                                         
01931            PERFORM 1040-FORMAT-RESEND                             
01932            MOVE LA-NO-OF-COPIES    TO COPIESO                     
01933            PERFORM 1050-GET-CORRESPOND THRU 1059-EXIT             
01934            GO TO 1010-READ-NEXT                                   
01935         ELSE                                                      
01936            IF LA-ADDRESS-DATA                                     
01937               MOVE LA-ADDRESS-LINE   TO REC-TEXT (TB-INDX)        
01938               MOVE ZEROS             TO REC-PC (TB-INDX)          
01939               SET TB-INDX UP BY 1                                 
01940               GO TO 1010-READ-NEXT                                
01941            ELSE                                                   
01942               PERFORM 1030-TEXT-BUILD                             
01943               GO TO 1010-READ-NEXT.                               
01944                                                                   
01945  1020-ENDBR.                                                      
01946      IF ARCH-BROWSE-STARTED = 'Y'                                 
01947         MOVE 'N'                TO ARCH-BROWSE-STARTED            
01948         EXEC CICS ENDBR                                           
01949              DATASET   (ARCH-ID)                                  
01950         END-EXEC                                                  
CIDMOD     END-IF.
CIDMOD                                                                  
121802*    IF PI-COMPANY-ID = 'CID'                                     
CIDMOD         IF TB-INDX = 1                                           
CIDMOD             IF WS-LETTER-STATUS EQUAL 'P'                        
CIDMOD                 MOVE 9021           TO EMI-ERROR                 
CIDMOD                 MOVE -1             TO ARCHNUML                  
CIDMOD                 MOVE AL-UNBON       TO ARCHNUMA                  
CIDMOD                 PERFORM 9900-ERROR-FORMAT THRU                   
CIDMOD                         9900-EXIT                                
CIDMOD                 GO TO 8200-SEND-DATAONLY                         
CIDMOD             ELSE                                                 
CIDMOD                 MOVE ER-0006             TO EMI-ERROR            
CIDMOD                 MOVE -1                  TO ARCHNUML             
CIDMOD                 MOVE AL-UNBON            TO ARCHNUMA             
CIDMOD                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         
CIDMOD                 GO TO 8200-SEND-DATAONLY                         
CIDMOD             END-IF
CIDMOD         END-IF
121802*    ELSE                                                         
121802*        IF TB-INDX = 1                                           
121802*            MOVE ER-0006             TO EMI-ERROR                
121802*            MOVE -1                  TO ARCHNUML                 
121802*            MOVE AL-UNBON            TO ARCHNUMA                 
121802*            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
121802*            GO TO 8200-SEND-DATAONLY                             
121802*        END-IF
121802*    END-IF
CIDMOD
01958                                                                   
01959      SET TB-INDX DOWN BY 1.                                       
01960      SET PI-TOTAL-LINES          TO TB-INDX.                      
01961      MOVE 1                      TO PI-CURRENT-LINE.              
01962      SET TB-INDX TO 1.                                            
01963                                                                   
01964      PERFORM 7170-FORMAT-SCREEN THRU 7170-EXIT                    
01965              VARYING SC-INDX FROM 1 BY 1                          
01966                UNTIL SC-INDX > NUM-LINES-PER-SCREEN.              
01967                                                                   
01968      MOVE SPACES                 TO PI-ADDR-TYPE                  
01969                                     MAINTI.                       
01970      MOVE -1                     TO MAINTL.                       
01971                                                                   
01972      GO TO 8100-SEND-INITIAL-MAP.                                 
01973                                                                   
01974  1030-TEXT-BUILD.                                                 
01975 ***** IF THERE ARE NO ADDRESS LINES OR LESS THAN 6 LINES, SET TO  
01976 ***** TOP OF FORM.                                                
01977      IF TB-INDX < 7                                               
01978         SET TB-INDX   TO 7.                                       
01979                                                                   
01980      IF TB-INDX = 7                                               
01981         MOVE TOP-FORM            TO REC-TEXT (TB-INDX)            
01982         SET TB-INDX UP BY 1.                                      
01983                                                                   
01984      MOVE LA-TEXT-LINE           TO REC-TEXT (TB-INDX).           
01985      MOVE LA-SKIP-CONTROL        TO REC-PC (TB-INDX)              
01986                                     INDX-WORK.                    
01987                                                                   
01988      IF INDX-WORK NOT = 99                                        
01989         SET TB-INDX UP BY 1                                       
01990         SET TB-INDX UP BY INDX-WORK                               
01991      ELSE                                                         
01992         SET TB-INDX UP BY 1                                       
01993         MOVE TOP-FORM            TO REC-TEXT (TB-INDX)            
01994         SET TB-INDX UP BY 1.                                      
01995                                                                   
01996  1040-FORMAT-RESEND.                                              
01997      IF LA-RESEND-DATE = LOW-VALUES                               
01998         MOVE SPACES                 TO RESENDI                    
01999        ELSE                                                       
02000         MOVE LA-RESEND-DATE         TO DC-BIN-DATE-1              
02001         MOVE ' '                    TO DC-OPTION-CODE             
02002         PERFORM 9700-DATE-LINK  THRU 9700-EXIT                    
02003         MOVE DC-GREG-DATE-1-EDIT    TO RESENDO.                   
02004                                                                   
02005  1050-GET-CORRESPOND.                                             
02006      MOVE LA-CARRIER             TO ACTV-CARRIER.                 
02007      MOVE LA-CLAIM-NO            TO ACTV-CLAIM.                   
02008      MOVE LA-CERT-NO             TO ACTV-CERT-NUM.                
02009      MOVE LA-CORR-TRLR-SEQ       TO ACTV-SEQ.                     
02010                                                                   
02011      EXEC CICS READ                                               
02012           DATASET     (ACTV-ID)                                   
02013           RIDFLD      (ACTV-KEY)                                  
02014           SET         (ADDRESS OF ACTIVITY-TRAILERS)              
02015      END-EXEC.                                                    
02016                                                                   
02017      IF AT-RECEIPT-FOLLOW-UP = LOW-VALUES                         
02018         MOVE SPACES                TO FOLLOWI                     
02019      ELSE                                                         
02020         MOVE SPACES                TO DC-OPTION-CODE              
02021         MOVE AT-RECEIPT-FOLLOW-UP  TO DC-BIN-DATE-1               
02022         PERFORM 9700-DATE-LINK THRU 9700-EXIT                     
02023         MOVE DC-GREG-DATE-1-EDIT   TO FOLLOWI.                    
02024                                                                   
02025      MOVE SPACES                 TO ADDRI                         
02026      MOVE AT-STD-LETTER-FORM     TO FORMI                         
02027      MOVE AT-REASON-TEXT         TO REI.                          
02028                                                                   
02029  1059-EXIT.                                                       
02030       EXIT.                                                       
02031      EJECT                                                        
02032  2000-CREATE.                                                     
02033 ***************************************************************   
02034 *    THIS ROUTINE WILL CREATE A NEW LETTER BY READING THE     *   
02035 *    TEXT FILE WITH THE FORM CODE SPECIFIED FROM THE SCREEN.  *   
02036 *    ALL VARIABLE SYMBOLS WILL BE RESOLVED AND THE LETTER     *   
02037 *    WILL BE DISPLAYED ONTO THE SCREEN.                       *   
02038 *                                                             *   
02039 ***************************************************************   
02040                                                                   
02041 ***************************************************************   
02042 *    CHECK TO SEE IF SAME REQUEST OR NOT.                     *   
02043 *    IF NEW REQUEST AND A LETTER WAS PRINTED, FORCE AN ERROR  *   
02044 ***************************************************************   
02045                                                                   
02046      IF PI-CREATE-MODE                                            
02047         IF PI-FORM-NUMBER = FORMI AND PI-ADDR-TYPE = ADDRI AND
02048            PI-LETTER-ADDRESS-TYPE = W-LETTER-ADDRESS-TYPE
110404           PERFORM 7500-READ-TS  THRU 7599-EXIT
110404           PERFORM 7950-SET-INDX THRU 7950-EXIT
110404           PERFORM 7960-UPDATE-TABLE-FROM-SCREEN THRU 7960-EXIT
110404             VARYING SC-INDX FROM 1 BY 1
110404             UNTIL SC-INDX > NUM-LINES-PER-SCREEN
02049            MOVE -1               TO MAINTL                        
      *          MOVE 'Y'              TO CLEANO
02050            GO TO 8200-SEND-DATAONLY                               
02051         ELSE                                                      
02052            IF PRINT-PERFORMED AND                                 
02053               PI-RETURN-TO-PROGRAM  =  PGM-EL150 AND              
02054               FORMI NOT = 9999                                    
02055               MOVE ER-0279           TO EMI-ERROR                 
02056               MOVE -1                TO MAINTL                    
02057               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            
02058               GO TO 8200-SEND-DATAONLY.                           
02059                                                                   
02060 ***************************************************************   
02061 *   IF THE FORM NUMBER IS THE SAME BUT THE ADDRESS TYPE       *   
02062 *   HAS CHANGED, THEN SAVE THE CURRENT SCREEN AND REBUILD     *   
02063 *   THE ADDRESS DATA.                                         *   
02064 ***************************************************************   
02065                                                                   
02066      IF PI-FORM-NUMBER = FORMI AND                                
02067         PI-ADDR-TYPE  =  ADDRI AND                                
02068         PI-LETTER-ADDRESS-TYPE = W-LETTER-ADDRESS-TYPE            
02069            MOVE -1                  TO MAINTL                     
02070            GO TO 8200-SEND-DATAONLY.                              
02071                                                                   
02072      IF PI-LETTER-ADDRESS-TYPE NOT = W-LETTER-ADDRESS-TYPE        
02073          MOVE 0                  TO GETMAIN-SWITCH                
02074          MOVE 'N'                TO WS-ACCT-READ-SW               
02075                                     WS-PROD-READ-SW               
02076                                     WS-COMP-READ-SW.              
02077                                                                   
02078      MOVE SPACES                 TO RECORD-TABLE.                 
02079                                                                   
PEMTST     PERFORM 7000-READ-ADDR THRU 7099-EXIT.                       
02081                                                                   
PEMTST     SET TB-INDX TO 1
02083      MOVE TOP-FORM               TO REC-TEXT (TB-INDX).           
02084      SET TB-INDX UP BY 1.                                         
02085                                                                   
02086 ***************************************************************   
02087 *    IF A NEW LETTER IS BEING CREATED FROM SCRATCH, SAVE      *   
02088 *    THE EXISTING SCREEN AND PASS CONTROL TO THE TEXT EDITOR  *   
02089 ***************************************************************   
02090                                                                   
02091      IF FORMI = '9999'                                            
02092         MOVE 16                  TO PI-TOTAL-LINES                
02093         MOVE 1                   TO PI-CURRENT-LINE               
02094         PERFORM 7700-PUT-TEMP-STOR   THRU 7749-EXIT               
02095         PERFORM 7790-WRITE-SCREEN-TS THRU 7790-EXIT               
02096         MOVE '3'                 TO PI-ACTION                     
02097         MOVE FORMI               TO PI-FORM-NUMBER                
02098                                     PI-COMM-CONTROL               
02099         MOVE ZEROS               TO PI-UPDATE-SW                  
02100         MOVE PGM-EL1042          TO PGM-NAME                      
02101         GO TO 9300-XCTL.                                          
02102                                                                   
02103      MOVE FORMI                  TO TEXT-LETTER.                  
02104      MOVE TEXT-PARTIAL-KEY       TO TEXT-SAVE-KEY.                
02105                                                                   
02106      EXEC CICS HANDLE CONDITION                                   
02107           NOTFND     (2120-ENDBR)                                 
02108           ENDFILE    (2120-ENDBR)                                 
02109           NOTOPEN    (8890-TEXT-NOT-OPEN)                         
02110      END-EXEC                                                     
02111                                                                   
02112      EXEC CICS STARTBR                                            
02113           DATASET    (TEXT-ID)                                    
02114           RIDFLD     (TEXT-KEY)                                   
02115           GTEQ                                                    
02116      END-EXEC.                                                    
02117                                                                   
02118      MOVE 'Y'                    TO TEXT-BROWSE-STARTED.          
02119                                                                   
02120  2110-READ-NEXT.                                                  
02121      IF TB-INDX > MAX-LINES                                       
02122         MOVE ER-0051             TO EMI-ERROR                     
02123         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
02124         GO TO 2120-ENDBR.                                         
02125                                                                   
02126      EXEC CICS READNEXT                                           
02127           DATASET    (TEXT-ID)                                    
02128           SET        (ADDRESS OF TEXT-FILES)                      
02129           RIDFLD     (TEXT-KEY)                                   
02130      END-EXEC.                                                    
02131                                                                   
02132      IF TEXT-PARTIAL-KEY NOT = TEXT-SAVE-KEY
              IF TB-INDX = +2
                 MOVE SPACES           TO PI-FORM-NUMBER
                 MOVE -1               TO FORML
                 MOVE AL-UABON         TO FORMA
                 MOVE ER-1236          TO EMI-ERROR
                 MOVE 'N'              TO CLEANO
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
02133         GO TO 2120-ENDBR
           END-IF
02134                                                                   

PEMTST     MOVE FORMI                  TO PI-FORM-NUMBER.               

02135      IF TX-LINE-SQUEEZE-CONTROL = 'Z'                             
02136          PERFORM 2800-PROCESS-Z-CONTROLS THRU 2800-EXIT           
02137          GO TO 2110-READ-NEXT.                                    
02138                                                                   
02139       MOVE TX-BSR-CODE           TO PI-ELLETR-BSR.                
02140                                                                   
030612      IF (PI-COMPANY-ID = 'CID' OR 'AHL')
010407         AND (PI-CARRIER NOT = '8')
010407         PERFORM VARYING B1 FROM +1 BY +1 UNTIL
010407            B1 > +20
010407            IF TX-TEXT-LINE (B1:5) = '@03.1'
010407               GO TO 2110-READ-NEXT
010407            END-IF
010407         END-PERFORM
010407      END-IF
02141       MOVE TX-TEXT-LINE          TO REC-TEXT (TB-INDX).           
02142       MOVE TX-PROCESS-CONTROL    TO REC-PC (TB-INDX)              
02143                                     INDX-WORK.                    
02144       SET TB-INDX UP BY 1.                                        
02145       IF INDX-WORK = 99                                           
02146          MOVE TOP-FORM           TO REC-TEXT (TB-INDX)            
02147          SET TB-INDX UP BY 1                                      
02148          GO TO 2110-READ-NEXT                                     
02149       ELSE                                                        
02150          SET TB-INDX UP BY INDX-WORK                              
02151          GO TO 2110-READ-NEXT.                                    
02152                                                                   
02153  2120-ENDBR.                                                      
02154      IF TEXT-BROWSE-STARTED = 'Y'                                 
02155         MOVE 'N'                 TO TEXT-BROWSE-STARTED           
02156         EXEC CICS ENDBR                                           
02157              DATASET     (TEXT-ID)                                
02158         END-EXEC.                                                 
02159                                                                   
033110*02160      IF TB-INDX = 8                                               
033110*02161         MOVE ER-0006             TO EMI-ERROR                     
033110*02162         MOVE -1                  TO FORML                         
033110*02163         MOVE AL-UABON            TO FORMA                         
033110*02164         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
033110*02165         GO TO 8200-SEND-DATAONLY-ERASEAUP.                        
02166                                                                   
02167      SET TB-INDX DOWN BY 1.                                       
02168      SET PI-TOTAL-LINES          TO TB-INDX.                      
02169      MOVE 1                      TO PI-CURRENT-LINE.              
02170                                                                   
02171 ***************************************************************   
02172 *    IF IT IS A DESIGNATED CLAIM, THEN RESOLVE ALL            *   
02173 *    VARIABLE SYMBOLS AND INSERT THEM INTO THE TEXT DATA.     *   
02174 ***************************************************************   
02175                                                                   
02176      IF PI-RETURN-TO-PROGRAM = PGM-EL150                          
02177         PERFORM 7200-RESOLVE-VARIABLES THRU 7269-EXIT             
02178         MOVE SPACES              TO W-REVERSE-DATE-SW             
02179         PERFORM 7300-VARIABLE-SEARCH   THRU 7399-EXIT             
02180                 VARYING TB-INDX FROM 7 BY 1 UNTIL                 
02181                 TB-INDX > PI-TOTAL-LINES                          
02182      ELSE                                                         
02183         MOVE ER-0373             TO EMI-ERROR                     
02184         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 
02185                                                                   
02186      SET TB-INDX TO 1.                                            
02187      PERFORM 7170-FORMAT-SCREEN THRU 7170-EXIT                    
02188              VARYING SC-INDX FROM 1 BY 1 UNTIL                    
02189              SC-INDX > NUM-LINES-PER-SCREEN.                      
02190                                                                   
02191      MOVE '3'                    TO PI-ACTION.                    
PEMTST*    MOVE FORMI                  TO PI-FORM-NUMBER.               
02193      MOVE W-LETTER-ADDRESS-TYPE  TO PI-LETTER-ADDRESS-TYPE.       
02194      MOVE -1                     TO MAINTL.                       
02195      GO TO 8200-SEND-DATAONLY-ERASEAUP.                           
02196                                  EJECT                            
02197  2800-PROCESS-Z-CONTROLS.                                         
02198                                                                   
02199      MOVE TX-TEXT-LINE           TO W-Z-CONTROL-DATA.             
02200                                                                   
02201      IF RESEND-SAVE = LOW-VALUES                                  
02202          IF W-DAYS-TO-RESEND-1 NUMERIC                            
02203              IF W-DAYS-TO-RESEND-1 > ZEROS                        
02204                  MOVE '6'                    TO DC-OPTION-CODE    
02205                  MOVE SAVE-BIN-DATE          TO DC-BIN-DATE-1     
02206                  MOVE ZEROS                  TO DC-ELAPSED-MONTHS 
02207                  MOVE W-DAYS-TO-RESEND-1     TO DC-ELAPSED-DAYS   
02208                  PERFORM 9700-DATE-LINK THRU 9700-EXIT            
02209                  IF NO-CONVERSION-ERROR                           
02210                      MOVE DC-BIN-DATE-2       TO RESEND-SAVE      
02211                      MOVE DC-GREG-DATE-1-EDIT TO RESENDO          
02212                      MOVE AL-UANON            TO RESENDA          
02213                      MOVE +8                  TO RESENDL          
02214                  ELSE                                             
02215                      MOVE ER-3770             TO EMI-ERROR        
02216                      MOVE -1                  TO MAINTL           
02217                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     
02218                      GO TO 8200-SEND-DATAONLY.                    
02219                                                                   
02220      IF FOLLOW-UP-SAVE = LOW-VALUES                               
02221          IF W-DAYS-TO-FOLLOW-UP NUMERIC                           
02222              IF W-DAYS-TO-FOLLOW-UP > ZEROS                       
02223                  MOVE '6'                 TO DC-OPTION-CODE       
02224                  MOVE SAVE-BIN-DATE       TO DC-BIN-DATE-1        
02225                  MOVE ZEROS               TO DC-ELAPSED-MONTHS    
02226                  MOVE W-DAYS-TO-FOLLOW-UP TO DC-ELAPSED-DAYS      
02227                  PERFORM 9700-DATE-LINK THRU 9700-EXIT            
02228                  IF NO-CONVERSION-ERROR                           
02229                      MOVE DC-BIN-DATE-2       TO FOLLOW-UP-SAVE   
02230                      MOVE DC-GREG-DATE-1-EDIT TO FOLLOWO          
02231                      MOVE AL-UANON            TO FOLLOWA          
02232                      MOVE +8                  TO FOLLOWL          
02233                  ELSE                                             
02234                      MOVE ER-3771             TO EMI-ERROR        
02235                      MOVE -1                  TO MAINTL           
02236                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     
02237                      GO TO 8200-SEND-DATAONLY.                    
02238                                                                   
02239      IF COPIESI NOT NUMERIC                                       
02240          IF W-NUMBER-OF-COPIES NUMERIC                            
02241              IF W-NUMBER-OF-COPIES > ZEROS                        
02242                  MOVE W-NUMBER-OF-COPIES  TO COPIESI              
02243                  MOVE AL-UNNON            TO COPIESA              
02244                  MOVE +1                  TO COPIESL              
02245              ELSE                                                 
02246                  MOVE +1                  TO COPIESI              
02247                  MOVE AL-UNNON            TO COPIESA              
02248                  MOVE +1                  TO COPIESL.             
02249                                                                   
040110     IF W-FORM-TO-RESEND > SPACES
040110         MOVE W-FORM-TO-RESEND          TO PI-RESEND-FORM-NUMBER
040110     ELSE
040110         MOVE LOW-VALUES                TO PI-RESEND-FORM-NUMBER
040110     END-IF.
040110
040110     IF W-PROMPT-LETTER = 'Y'
040110         MOVE W-PROMPT-LETTER           TO PI-PROMPT-LETTER
040110         MOVE ER-0894                   TO EMI-ERROR
040110         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
040110     ELSE
040110         MOVE 'N'                       TO PI-PROMPT-LETTER
040110     END-IF.
040110
040110     MOVE W-ENCLOSURE-CD                TO PI-ENCLOSURE-CD
                                                 ENCO
           MOVE AL-UANON                      TO ENCA
           MOVE +3                            TO ENCL
           MOVE 'N'                           TO CLEANO

040110     MOVE W-AUTO-CLOSE-IND              TO PI-AUTO-CLOSE-IND.
040110     MOVE W-LETTER-TO-BENE              TO PI-LETTER-TO-BENE.
040110
02250  2800-EXIT.                                                       
02251      EXIT.                                                        
02252      EJECT                                                        
02253  3000-RECORD.                                                     
02254 ***************************************************************   
02255 *    THIS ROUTINE WILL SAVE THE LETTER TEXT (IF ANY) AND      *   
02256 *    BUILD A CORRESPONDENCE TRAILER WITH THE DATA FROM        *   
02257 *    THE SCREEN.                                              *   
02258 ***************************************************************   
02259                                                                   
02260      IF PI-RETURN-TO-PROGRAM NOT = PGM-EL150                      
02261         MOVE ER-0211             TO EMI-ERROR                     
02262         MOVE -1                  TO MAINTL                        
02263         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
02264         GO TO 8200-SEND-DATAONLY.                                 
02265                                                                   
02266      EXEC CICS HANDLE CONDITION                                   
02267           NOTOPEN    (8860-CLAM-NOT-OPEN)                         
02268           NOTFND     (3010-NOT-FOUND)                             
02269      END-EXEC.                                                    
02270                                                                   
02271      PERFORM 0350-EDIT-ROUTINE THRU 0350-EXIT.                    
02272      PERFORM 7950-SET-INDX     THRU 7950-EXIT.                    
02273                                                                   
02274      PERFORM 7960-UPDATE-TABLE-FROM-SCREEN THRU 7960-EXIT         
02275              VARYING SC-INDX FROM 1 BY 1                          
02276              UNTIL SC-INDX > NUM-LINES-PER-SCREEN.                
02277                                                                   
02278      PERFORM 7000-READ-ADDR THRU 7099-EXIT.                       
02279                                                                   
02280      IF NOT EMI-NO-ERRORS                                         
02281         GO TO 8200-SEND-DATAONLY.                                 
02282                                                                   
02283      MOVE ZEROS               TO ARCH-NUMBER.                     
02284      PERFORM 6500-BUILD-CORRESPOND THRU 6599-EXIT.                
02285      MOVE ER-0000             TO EMI-ERROR.                       
02286      MOVE -1                  TO MAINTL.                          
02287      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
02288      GO TO 8200-SEND-DATAONLY.                                    
02289                                                                   
02290  3010-NOT-FOUND.                                                  
02291      MOVE ER-0133             TO EMI-ERROR.                       
02292      MOVE -1                  TO MAINTL.                          
02293      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
02294      GO TO 8200-SEND-DATAONLY.                                    
02295                                                                   
02296      EJECT                                                        
121802*4000-SET-BSR. Remove as dead code                                
121802*4001-READ-NEXT. Remove as dead code                              
121802*4002-ENDBR. Remove as dead code                                  
121802*4000-EXIT. Remove as dead code                                   
02357                                                                   
02358      EJECT                                                        
02359  5000-MOVE-NAME.                                                  
02360      COPY ELCMNS.                                                 
02361      EJECT                                                        
02362  5200-MOVE-NAME.                                                  
02363 *    NOTE ******************************************************* 
02364 *         *           M O V E   N A M E   R O U T I N E         * 
02365 *         *                                                     * 
02366 *         *      THE FOLLOWING ROUTINE MOVES THE INSURRED'S     * 
02367 *         *  NAME FROM THE CLAIM MASTER TO A WORK AREA WITH     * 
02368 *         *  NO EMBEDDED BLANKS.                                * 
02369 *         *                                                     * 
02370 *         *        FIELD               VALUE                    * 
02371 *         *                                                     * 
02372 *         *      LAST NAME (CL15)      SMITH                    * 
02373 *         *      1ST NAME  (CL12)      JOHN                     * 
02374 *         *      MID NAME  (CL1)       A                        * 
02375 *         *                                                     * 
02376 *         *      AFTER NAME HAS BEEN MOVED WS-NAME-WORK (CL30)  * 
02377 *         *                                                     * 
02378 *         *              JOHN A. SMITH                          * 
02379 *         *                                                     * 
02380 *         *      TO USE THIS ROUTINE YOU ALSO NEED A WORKING    * 
02381 *         *  STORAGE COPYBOOK:                                  * 
02382 *         *                                                     * 
02383 *         *      01  WS-NAME-WORK-AREA COPY ELCNWA.             * 
02384 *         *                                                     * 
02385 *         *******************************************************.
02386                                                                   
02387      MOVE SPACES                 TO  WS-NAME-WORK-AREA.           
02388      MOVE ZERO                   TO  WS-NAME-SW.                  
02389      SET NWA-INDEX TO +1.                                         
02390                                                                   
02391      IF W-FIRST-NAME = SPACES                                     
02392              AND                                                  
02393         W-MIDDLE-NAME = SPACES                                    
02394          MOVE W-LAST-NAME        TO WS-NAME-WORK                  
02395          GO TO 5200-EXIT.                                         
02396                                                                   
02397      MOVE W-FIRST-NAME           TO  WS-NAME-WORK2.               
02398      PERFORM 5300-MOVE-NAME THRU 5390-EXIT.                       
02399                                                                   
02400      SET NWA-INDEX UP BY +1.                                      
02401                                                                   
02402      IF W-MIDDLE-NAME NOT = SPACES                                
02403         MOVE W-MIDDLE-NAME       TO  WS-NW (NWA-INDEX)            
02404         SET NWA-INDEX UP BY +1                                    
02405         MOVE '.'                 TO  WS-NW (NWA-INDEX)            
02406         SET NWA-INDEX UP BY +2.                                   
02407                                                                   
02408      MOVE W-LAST-NAME            TO  WS-NAME-WORK2.               
02409      PERFORM 5300-MOVE-NAME THRU 5390-EXIT.                       
02410                                                                   
02411  5200-EXIT.                                                       
02412      EXIT.                                                        
02413                                                                   
02414      EJECT                                                        
02415  5300-MOVE-NAME.                                                  
02416      IF WS-NAME-SW > +1                                           
02417          GO TO 5390-EXIT.                                         
02418                                                                   
02419      IF WS-NAME-WORK2 = SPACES                                    
02420          GO TO 5390-EXIT.                                         
02421                                                                   
02422      SET NWA-INDEX2 TO +1.                                        
02423      SET NWA-INDEX3 TO +2.                                        
02424                                                                   
02425  5310-MOVE-NAME.                                                  
02426      MOVE WS-NW2 (NWA-INDEX2)  TO  WS-NW (NWA-INDEX).             
02427                                                                   
02428      IF NWA-INDEX < +30                                           
02429         SET NWA-INDEX UP BY +1                                    
02430      ELSE                                                         
02431         ADD +2  TO  WS-NAME-SW                                    
02432         GO TO 5390-EXIT.                                          
02433                                                                   
02434      IF NWA-INDEX2 < +20                                          
02435          SET NWA-INDEX2 UP BY +1                                  
02436          SET NWA-INDEX3 UP BY +1.                                 
02437                                                                   
02438      IF WS-NW2 (NWA-INDEX2) = SPACES AND                          
02439         WS-NW2 (NWA-INDEX3) = SPACES                              
02440         GO TO 5390-EXIT.                                          
02441                                                                   
02442      GO TO 5310-MOVE-NAME.                                        
02443                                                                   
02444  5390-EXIT.                                                       
02445      EXIT.                                                        
02446                                                                   
02447      EJECT                                                        
02448  6100-ADDR-MAINT.                                                 
02449 ***************************************************************   
02450 *    THIS ROUTINE WILL SAVE A COPY OF THE EXISTING SCREEN     *   
02451 *    AND THE TS-TABLE OF LETTER TEXT.                         *   
02452 *    IT WILL THEN XCTL TO THE ADDRESS MAINT PROGRAM.          *   
02453 ***************************************************************   
02454                                                                   

030805     PERFORM 7500-READ-TS        THRU 7599-EXIT
030805
030805     PERFORM 7950-SET-INDX THRU 7950-EXIT.                        
030805     PERFORM 7960-UPDATE-TABLE-FROM-SCREEN THRU 7960-EXIT         
030805             VARYING SC-INDX FROM 1 BY 1                          
030805             UNTIL SC-INDX > NUM-LINES-PER-SCREEN.                
030805                                                                  
02455      IF PI-RETURN-TO-PROGRAM NOT = PGM-EL150                      
02456         MOVE ER-0343             TO EMI-ERROR                     
02457         MOVE -1                  TO MAINTL                        
02458         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
02459         GO TO 8200-SEND-DATAONLY.                                 
02460                                                                   

081004*    PERFORM 7500-READ-TS        THRU 7599-EXIT

02461 *    PERFORM 7950-SET-INDX THRU 7950-EXIT.                        
02462 *    PERFORM 7960-UPDATE-TABLE-FROM-SCREEN THRU 7960-EXIT         
02463 *            VARYING SC-INDX FROM 1 BY 1                          
02464 *            UNTIL SC-INDX > NUM-LINES-PER-SCREEN.                
02465                                                                   
02466      PERFORM 7700-PUT-TEMP-STOR    THRU 7749-EXIT.                
02467      PERFORM 7790-WRITE-SCREEN-TS  THRU 7790-EXIT.                
02468      PERFORM 7795-WRITE-PI-AREA-TS THRU 7795-EXIT.                
02469      MOVE PGM-EL141 TO PGM-NAME.                                  
02470                                                                   
02471      GO TO 9300-XCTL.                                             
02472      EJECT                                                        
02473  6200-EDIT-MODE.                                                  
02474 ***************************************************************   
02475 *    THIS ROUTINE WILL SAVE A COPY OF THE EXISTING SCREEN     *   
02476 *    AND THE TS-TABLE OF LETTER TEXT.                         *   
02477 *    IT WILL THEN XCTL TO THE TEXT-EDITOR PROGRAM.            *   
02478 ***************************************************************   
PEMTST     IF PI-SHOW-MODE
PEMTST        MOVE ER-0188             TO EMI-ERROR
PEMTST        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
PEMTST        MOVE -1                  TO MAINTL
PEMTST        MOVE AL-UABON            TO MAINTA
PEMTST        GO TO 8200-SEND-DATAONLY
PEMTST     END-IF
02485                                                                   
121802*    IF PI-COMPANY-ID = 'DMD'                                     
121802*        IF PI-BSR-AUTOMATED                                      
121802*            MOVE ER-0912        TO EMI-ERROR                     
121802*            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
121802*            MOVE -1             TO MAINTL                        
121802*            MOVE AL-UABON       TO MAINTA                        
121802*            GO TO 8200-SEND-DATAONLY.                            
02493                                                                   
02494      IF PI-CURRENT-LINE = ZEROS                                   
02495         MOVE ER-0187             TO EMI-ERROR                     
02496         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
02497         MOVE -1                  TO MAINTL                        
02498         GO TO 8200-SEND-DATAONLY.                                 
02499                                                                   

081004     PERFORM 7500-READ-TS        THRU 7599-EXIT
081004     PERFORM 7750-DELETE-TEMP-STOR
                                       THRU 7750-EXIT 

02500      PERFORM 7950-SET-INDX THRU 7950-EXIT.                        
02501      PERFORM 7960-UPDATE-TABLE-FROM-SCREEN THRU 7960-EXIT         
02502              VARYING SC-INDX FROM 1 BY 1                          
02503              UNTIL SC-INDX > NUM-LINES-PER-SCREEN.                
02504                                                                   
02505      PERFORM 7700-PUT-TEMP-STOR   THRU 7749-EXIT.                 
02506      PERFORM 7790-WRITE-SCREEN-TS THRU 7790-EXIT.                 
02507                                                                   
02508      MOVE PI-FORM-NUMBER         TO PI-COMM-CONTROL.              
02509      MOVE ZEROS                  TO PI-UPDATE-SW.                 
02510      MOVE '1'                    TO PI-ENTRY-CD-1.                
02511      MOVE PGM-EL1042             TO PGM-NAME.                     
02512      GO TO 9300-XCTL.                                             
02513                                                                   
02514      EJECT                                                        
02515  6400-LETTER-RELEASE.                                             
02516 ***************************************************************   
02517 *    THIS ROUTINE WILL BE USED WHEN THE LETTER HAS BEEN       *   
02518 *    COMPLETED AND IS TO BE PUT AS PERMANENT RECORDS ONTO     *   
02519 *    THE ARCHIVE FILE.                                        *   
02520 *    THE FOLLOWING FUNCTIONS WILL BE PERFORMED                *   
02521 *        1. CHECK SECURITY AND IF IT IS A DESIGNATED LETTER.  *   
02522 *        2. RE-EDIT DATA AND UPDATE TS-TABLE WITH CHANGES     *   
02523 *        3. MAKE SURE THERE ARE NO UNRESOLVED SYMBOLS         *   
02524 *        4. GET THE ARCHIVE NUMBER FROM THE CONTROL FILE.     *   
02525 *        5. WRITE THE NEW ARCHIVE RECORDS FROM TS-TABLE.      *   
02526 *        7. BUILD A CORRESPONDENCE TRAILER                    *   
02527 *        8. BUILD OR UPDATE THE ACTIVITY QUE FILE WITH THE    *   
02528 *                 ARCHIVE NUMBER IF IT IS TO BE PRINTED LATER.*   
02529 *        4. RESET ALL CONTROL FIELDS AND RETURN THE           *   
02530 *                 ARCHIVE NUMBER USED TO FILE THE RECORDS.    *   
02531 ***************************************************************   
02532                                                                   

030805     PERFORM 7500-READ-TS        THRU 7599-EXIT
030805
030805     PERFORM 7950-SET-INDX THRU 7950-EXIT.                        
030805     PERFORM 7960-UPDATE-TABLE-FROM-SCREEN THRU 7960-EXIT         
030805             VARYING SC-INDX FROM 1 BY 1                          
030805             UNTIL SC-INDX > NUM-LINES-PER-SCREEN.                
030805                                                                  

02533      IF NOT MODIFY-CAP                                            
02534         MOVE ER-0070             TO EMI-ERROR                     
02535         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
02536         MOVE -1                  TO MAINTL                        
02537         GO TO 8200-SEND-DATAONLY.                                 
02538                                                                   
02539      IF PI-RETURN-TO-PROGRAM NOT = PGM-EL150                      
02540         MOVE ER-0211             TO EMI-ERROR                     
02541         MOVE -1                  TO MAINTL                        
02542         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
02543         GO TO 8200-SEND-DATAONLY.                                 
02544                                                                   

081004*    PERFORM 7500-READ-TS        THRU 7599-EXIT

02545 *    PERFORM 7950-SET-INDX THRU 7950-EXIT.                        
02546 *    PERFORM 7960-UPDATE-TABLE-FROM-SCREEN THRU 7960-EXIT         
02547 *            VARYING SC-INDX FROM 1 BY 1                          
02548 *            UNTIL SC-INDX > NUM-LINES-PER-SCREEN.                
02549                                                                   
02550      IF PI-ADDR-TYPE  NOT =  ADDRI                                
02551         PERFORM 7000-READ-ADDR THRU 7099-EXIT.                    
02552                                                                   
02553      IF NOT EMI-NO-ERRORS                                         
02554         GO TO 8200-SEND-DATAONLY.                                 
02555                                                                   
02556      MOVE +0                     TO TALLY.                        
02557      INSPECT RECORD-TABLE TALLYING TALLY                          
02558                                  FOR CHARACTERS BEFORE '@'.       
02559                                                                   
02560      IF TALLY < +21900                                            
02561         COMPUTE PI-CURRENT-LINE = TALLY / 73                      
02562         MOVE ZEROS               TO ROLL-COUNTER                  
02563         MOVE ER-0191             TO EMI-ERROR                     
02564         MOVE -1                  TO MAINTL                        
02565         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
02566         GO TO 7900-ROLL-PAGE.                                     
02567                                                                   
02568      MOVE '1'                    TO CNTL-RECORD-TYPE.             
02569      MOVE ZEROS                  TO CNTL-SEQ.                     
02570      MOVE SPACES                 TO CNTL-GENL.                    
02571      EXEC CICS READ                                               
02572           DATASET    (CNTL-ID)                                    
02573           SET        (ADDRESS OF CONTROL-FILE)                    
02574           RIDFLD     (CNTL-KEY)                                   
02575           UPDATE                                                  
02576      END-EXEC.                                                    
02577                                                                   
02578      ADD 1                       TO CF-CO-ARCHIVE-COUNTER.        
02579      MOVE CF-CO-ARCHIVE-COUNTER  TO ARCH-NUMBER.                  
02580      MOVE CF-PRINT-ADDRESS-LABELS   TO  WS-LABELS-SW.             
02581                                                                   
02582      EXEC CICS REWRITE                                            
02583           FROM      (CONTROL-FILE)                                
02584           DATASET   (CNTL-ID)                                     
02585      END-EXEC                                                     
02586                                                                   
02587      PERFORM 6500-BUILD-CORRESPOND THRU 6599-EXIT.                
02588                                                                   
02589      EXEC CICS HANDLE CONDITION                                   
02590           NOTOPEN   (9990-ABEND)                                  
02591      END-EXEC.                                                    
02592                                                                   
02593      EXEC CICS GETMAIN                                            
02594           SET      (ADDRESS OF LETTER-ARCHIVE)                    
02595           LENGTH   (ARCH-LENGTH)                                  
02596      END-EXEC.                                                    
02597                                                                   
02598      MOVE SPACES                 TO LETTER-ARCHIVE.               
02599      MOVE 'LA'                   TO LA-RECORD-ID.                 
02600      MOVE ARCH-NUMBER            TO LA-ARCHIVE-NO                 
02601                                     LA-ARCHIVE-NO-A1.             
02602      MOVE '1'                    TO LA-RECORD-TYPE                
02603                                     LA-RECORD-TYPE-A1.            
02604      MOVE ZEROS                  TO LA-LINE-SEQ-NO                
02605                                     LA-LINE-SEQ-NO-A1.            
02606      MOVE PI-COMPANY-CD          TO LA-COMPANY-CD                 
02607                                     LA-COMPANY-CD-A1.             
02608      MOVE CLAM-CARRIER           TO LA-CARRIER.                   
02609      MOVE PI-CLAIM-NO            TO LA-CLAIM-NO.                  
02610      MOVE PI-CERT-NO             TO LA-CERT-NO.                   
02611                                                                   
CIDMOD*    IF PI-COMPANY-ID = 'DMD'                                     
CIDMOD*        MOVE WS-DMD-CERT-STATE    TO LA-DMD-UND-CODE             
CIDMOD*        MOVE WS-DMD-CERT-GROUPING TO LA-DMD-PROD-CODE            
CIDMOD*        MOVE WS-DMD-BEN-CODE      TO LA-DMD-BEN-CODE             
CIDMOD*        MOVE WS-DMD-CORR-TRLR-SEQ TO LA-DMD-CORR-TRLR-SEQ        
CIDMOD*        MOVE WS-DMD-LETTER-FORM   TO LA-DMD-LETTER-FORM          
CIDMOD*        MOVE WS-DMD-RES-ST        TO LA-DMD-RES-ST               
CIDMOD*        MOVE LOW-VALUES           TO LA-DMD-LETTER-PURGE-DT      
CIDMOD*                                     LA-DMD-LETTER-RELOAD-DT     
CIDMOD*        MOVE '1'                  TO LA-DMD-LETTER-STATUS        
CIDMOD*        EXEC CICS GETMAIN                                        
CIDMOD*            SET    (ADDRESS OF LETTER-ARCHIVE-TEMP)              
CIDMOD*            LENGTH (ARCT-LENGTH)                                 
CIDMOD*        END-EXEC.                                                
02626                                                                   
02627      IF COPIESL NOT = ZEROS                                       
02628         MOVE COPIESI             TO LA-NO-OF-COPIES               
02629      ELSE                                                         
02630         MOVE  1                  TO LA-NO-OF-COPIES.              
02631                                                                   
02632      IF RESENDL NOT = ZEROS                                       
02633         MOVE RESEND-SAVE         TO LA-RESEND-DATE                
02634      ELSE                                                         
02635         MOVE LOW-VALUES          TO LA-RESEND-DATE.               
02636                                                                   
02637      MOVE PI-PROCESSOR-ID        TO LA-PROCESSOR-CD.              
02638      MOVE CURRENT-SAVE           TO LA-CREATION-DT.               
02639                                                                   
02640      IF PRINT-PERFORMED                                           
02641         MOVE CURRENT-SAVE        TO LA-INITIAL-PRINT-DATE         
02642      ELSE                                                         
02643         MOVE LOW-VALUES          TO LA-INITIAL-PRINT-DATE.        
02644                                                                   
02645      MOVE LOW-VALUES             TO LA-RESEND-PRINT-DATE.         
02646      MOVE CORR-TRLR-SEQ          TO LA-CORR-TRLR-SEQ.             
02647      PERFORM 6490-WRITE-ARCHIVE THRU 6499-EXIT.                   
033110
040110     IF PI-PROMPT-LETTER NOT EQUAL 'Y'
033110         PERFORM 6700-BUILD-NAPERSOFT THRU 6799-EXIT
040110     END-IF.
02648                                                                   
02649      IF WS-LABELS-SW = 'N'                                        
02650          NEXT SENTENCE                                            
02651      ELSE                                                         
02652          SET TB-INDX TO 1                                         
02653          MOVE ZEROS                  TO SEQ-COUNTER               
02654          PERFORM 6480-FORMAT-ADDRESS                              
02655                VARYING TB-INDX FROM 1 BY 1 UNTIL                  
02656                TB-INDX > 6.                                       
02657                                                                   
02658      MOVE ZEROS                  TO SEQ-COUNTER.                  
033110     MOVE 'N'                    TO WS-SKIP-EMAIL
02659      PERFORM 6470-FORMAT-TEXT THRU 6479-EXIT                      
02660              VARYING TB-INDX FROM 8 BY 1                          
02661              UNTIL TB-INDX > PI-TOTAL-LINES.                      
033110     MOVE 'Y'                    TO WS-SKIP-EMAIL.
PEMTST     IF SEQ-COUNTER < +15
033110      AND WS-SKIP-EMAIL EQUAL 'N'
              MOVE ARCH-NUMBER         TO TRAN-DL2-ARCHNO
              MOVE PI-PROCESSOR-ID     TO TRAN-DL2-USER
              MOVE SPACES              TO TRAN-DL2-CCC
              STRING PI-CARRIER ' ' PI-CLAIM-NO ' ' PI-CERT-NO
                 DELIMITED BY SIZE
                 INTO TRAN-DL2-CCC
              END-STRING
              EXEC CICS WRITEQ TD
                 QUEUE ('BTCH')
                 FROM (TRAN-DATA-LINE1)
                 LENGTH (80)
              END-EXEC
              EXEC CICS WRITEQ TD
                 QUEUE ('BTCH')
                 FROM (TRAN-DATA-LINE2)
                 LENGTH (80)
              END-EXEC
              EXEC CICS WRITEQ TD
                 QUEUE ('BTCH')
                 FROM (TRAN-DATA-LINE3)
                 LENGTH (80)
              END-EXEC
           END-IF

02662      MOVE ER-0280                TO EMI-ERROR.                    
02663                                                                   
02664      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     
02665      MOVE ARCH-NUMBER            TO ARCH-SUPPRESS                 
02666      MOVE ARCH-EDIT              TO EMI-TEXT-VARIABLE (1).        
02667                                                                   
02668      GO TO 9400-CLEAR.                                            
02669                                                                   
02670      EJECT                                                        
02671  6470-FORMAT-TEXT.                                                
033110
033110     IF REC-TEXT (TB-INDX) (1:16) = NAPERSOFT-LETTER
033110         MOVE 'Y'                TO WS-SKIP-EMAIL 
033110     END-IF.
033110
02672      MOVE SPACES                 TO LETTER-ARCHIVE.               
02673      MOVE '3'                    TO LA-RECORD-TYPE                
02674                                     LA-RECORD-TYPE-A1.            
02675      MOVE 'LA'                   TO LA-RECORD-ID.                 
02676      MOVE ARCH-NUMBER            TO LA-ARCHIVE-NO                 
02677                                     LA-ARCHIVE-NO-A1.             
02678      MOVE SEQ-COUNTER            TO LA-LINE-SEQ-NO                
02679                                     LA-LINE-SEQ-NO-A1.            
02680      MOVE PI-COMPANY-CD          TO LA-COMPANY-CD                 
02681                                     LA-COMPANY-CD-A1.             
02682      MOVE REC-TEXT (TB-INDX)     TO LA-TEXT-LINE.                 
02683      SET TB-INDX1 TO TB-INDX.                                     
02684      SET TB-INDX1 UP BY 1.                                        
02685      MOVE ZEROS                  TO INDX-WORK.                    
02686                                                                   
02687  6472-LOOP.                                                       
02688      IF TB-INDX1 < PI-TOTAL-LINES AND                             
02689         REC-TEXT (TB-INDX1) = SPACES                              
02690            SET TB-INDX1 UP BY 1                                   
02691            ADD 1                 TO INDX-WORK                     
02692            GO TO 6472-LOOP.                                       
02693                                                                   
02694      IF REC-TEXT (TB-INDX1) = TOP-FORM                            
02695         MOVE '99'                TO LA-SKIP-CONTROL               
02696         SET TB-INDX1 UP BY 1                                      
02697      ELSE                                                         
02698         MOVE INDX-WORK           TO LA-SKIP-CONTROL.              
02699                                                                   
02700      SET TB-INDX TO TB-INDX1.                                     
02701      SET TB-INDX DOWN BY 1.                                       
02702      PERFORM 6490-WRITE-ARCHIVE THRU 6499-EXIT.                   
02703      ADD 1 TO SEQ-COUNTER.                                        
02704                                                                   
02705  6479-EXIT.                                                       
02706       EXIT.                                                       
02707                                                                   
02708      EJECT                                                        
02709  6480-FORMAT-ADDRESS.                                             
02710      MOVE SPACES                 TO LETTER-ARCHIVE.               
02711      MOVE '2'                    TO LA-RECORD-TYPE                
02712                                     LA-RECORD-TYPE-A1.            
02713      MOVE 'LA'                   TO LA-RECORD-ID.                 
02714      MOVE ARCH-NUMBER            TO LA-ARCHIVE-NO                 
02715                                     LA-ARCHIVE-NO-A1.             
02716      MOVE SEQ-COUNTER            TO LA-LINE-SEQ-NO                
02717                                     LA-LINE-SEQ-NO-A1.            
02718      MOVE PI-COMPANY-CD          TO LA-COMPANY-CD                 
02719                                     LA-COMPANY-CD-A1.             
02720      MOVE REC-TEXT (TB-INDX)     TO LA-ADDRESS-LINE.              
02721                                                                   
02722      PERFORM 6490-WRITE-ARCHIVE THRU 6499-EXIT.                   
02723      ADD 1 TO SEQ-COUNTER.                                        
02724                                                                   
02725      EJECT                                                        
02726  6490-WRITE-ARCHIVE.                                              
02727      EXEC CICS HANDLE CONDITION                                   
02728          DUPKEY    (6499-EXIT)                                    
02729      END-EXEC.                                                    
02730                                                                   
121802*    IF PI-COMPANY-ID = 'DMD'                                     
121802*        MOVE LETTER-ARCHIVE     TO LETTER-ARCHIVE-TEMP           
121802*        MOVE 'LT'               TO LT-RECORD-ID                  
121802*        EXEC CICS WRITE                                          
121802*            DATASET   (ARCT-ID)                                  
121802*            FROM      (LETTER-ARCHIVE-TEMP)                      
121802*            RIDFLD    (LT-CONTROL-PRIMARY)                       
121802*        END-EXEC.                                                
02739                                                                   
02740      EXEC CICS WRITE                                              
02741           DATASET   (ARCH-ID)                                     
02742           FROM      (LETTER-ARCHIVE)                              
02743           RIDFLD    (LA-CONTROL-PRIMARY)                          
02744      END-EXEC.                                                    
02745                                                                   
02746  6499-EXIT.                                                       
02747      EXIT.                                                        
02748                                                                   
02749      EJECT                                                        
02750  6500-BUILD-CORRESPOND.                                           
02751 ***************************************************************   
02752 *    THIS ROUTINE WILL GET THE TRAILER SEQUENCE NUMBER FROM   *   
02753 *    THE CLAIM MASTER AND BUILD A CORRESPONDENCE TRAILER      *   
02754 *    USING THE NEW SEQUENCE NUMBER.                           *   
02755 *    INPUT DATA FROM THE SCREEN IS USED TO CREATE THE NEW     *   
02756 *    TRAILER RECORD.                                          *   
02757 ***************************************************************   
02758                                                                   
02759      MOVE PI-CLAIM-NO            TO CLAM-CLAIM.                   
02760                                                                   
02761      EXEC CICS READ                                               
02762           DATASET    (CLAM-ID)                                    
02763           SET        (ADDRESS OF CLAIM-MASTER)                    
02764           RIDFLD     (CLAM-KEY)                                   
02765           UPDATE                                                  
02766      END-EXEC.                                                    
02767                                                                   
02768      SUBTRACT 1 FROM CL-TRAILER-SEQ-CNT.                          
02769                                                                   
02770      IF FOLLOW-UP-SAVE > CL-NEXT-FOLLOWUP-DT                      
02771         MOVE FOLLOW-UP-SAVE      TO CL-NEXT-FOLLOWUP-DT.          
02772                                                                   
02773      IF RESEND-SAVE > CL-NEXT-FOLLOWUP-DT                         
02774         MOVE RESEND-SAVE         TO CL-NEXT-FOLLOWUP-DT.          
02775                                                                   
02776      IF MAINTI = 'C'                                              
02777         MOVE '2'                 TO CL-LAST-MAINT-TYPE.           
02778                                                                   
02779      IF MAINTI = 'R'                                              
02780         PERFORM 6600-SET-ADDR-SEQ THRU 6699-EXIT                  
02781         MOVE ACTV-SEQ            TO PI-ADDR-SEQ.                  
02782                                                                   
121802*    IF PI-COMPANY-ID = 'DMD'                                     
121802*        MOVE 04                 TO CL-ACTIVITY-CODE.             
02785                                                                   
02786      EXEC CICS GETMAIN                                            
02787           SET       (ADDRESS OF ACTIVITY-TRAILERS)                
02788           INITIMG   (GETMAIN-SPACE)                               
02789           LENGTH    (ACTV-LENGTH)                                 
02790      END-EXEC.                                                    
02791                                                                   
02792      MOVE 'AT'                   TO AT-RECORD-ID.                 
02793      MOVE  4                     TO AT-TRAILER-TYPE.              
02794      MOVE CURRENT-SAVE           TO AT-RECORDED-DT                
02795                                     CL-LAST-MAINT-DT              
02796                                     AT-CORR-LAST-MAINT-DT         
02797      MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY                
02798                                     CL-LAST-MAINT-USER            
02799                                     AT-CORR-LAST-UPDATED-BY       
02800      MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS          
02801                                     CL-LAST-MAINT-HHMMSS.         
02802      MOVE ACTV-KEY               TO AT-CONTROL-PRIMARY.           
02803      MOVE PI-CLAIM-NO            TO AT-CLAIM-NO.                  
02804      MOVE CL-TRAILER-SEQ-CNT     TO AT-SEQUENCE-NO                
02805                                     CORR-TRLR-SEQ.                
02806      MOVE CURRENT-SAVE           TO AT-LETTER-SENT-DT.            
02807      MOVE FOLLOW-UP-SAVE         TO AT-RECEIPT-FOLLOW-UP.         
02808      MOVE RESEND-SAVE            TO AT-AUTO-RE-SEND-DT.           
02809      MOVE LOW-VALUES             TO AT-LETTER-ANSWERED-DT         
02810                                     AT-LETTER-PURGED-DT.          
02811      MOVE ARCH-NUMBER            TO AT-LETTER-ARCHIVE-NO.         
02812      MOVE '1'                    TO AT-LETTER-ORIGIN.             
040110     MOVE PI-RESEND-FORM-NUMBER  TO AT-RESEND-LETTER-FORM.
040110     MOVE PI-AUTO-CLOSE-IND      TO AT-AUTO-CLOSE-IND.
040110     MOVE PI-LETTER-TO-BENE      TO AT-LETTER-TO-BENE.
02813                                                                   
CIDMOD*    IF PI-COMPANY-ID = 'DMD'                                     
CIDMOD*        MOVE '1'                TO AT-DMD-LETTER-STATUS          
CIDMOD*        IF PI-BSR-AUTOMATED                                      
CIDMOD*            MOVE 'A'            TO AT-DMD-BSR-CODE.              
CIDMOD*                                                                 
02819      IF FORML  NOT = ZEROS                                        
02820         MOVE FORMI               TO AT-STD-LETTER-FORM            
02821      ELSE                                                         
02822         MOVE SPACES              TO AT-STD-LETTER-FORM.           
02823                                                                   
02824      IF REL NOT = ZEROS                                           
02825         MOVE REI                 TO AT-REASON-TEXT                
02826      ELSE                                                         
02827         MOVE SPACES              TO AT-REASON-TEXT.               
02828                                                                   
121802*    IF PI-COMPANY-ID = 'HAN'  OR  'JHL'                          
121802*        MOVE AT-REASON-TEXT     TO WS-REASON-TEXT                
121802*        IF WS-RE-NDX NUMERIC  AND                                
121802*           WS-RE-NDX > ZERO   AND                                
121802*           WS-RE-NDX < 33                                        
121802*            MOVE HAN-REASON-TEXT (WS-RE-NDX)                     
121802*                                TO AT-REASON-TEXT.               
02836                                                                   
02837      MOVE PI-ADDR-SEQ            TO AT-ADDRESS-REC-SEQ-NO.        
02838                                                                   
02839      IF PI-ADDR-TYPE > SPACES                                     
02840          MOVE PI-ADDR-TYPE          TO AT-ADDRESEE-TYPE           
02841          MOVE REC-TEXT (1)          TO AT-ADDRESSEE-NAME          
02842      ELSE                                                         
02843      IF W-ACCOUNT > ZEROS                                         
02844          MOVE 'A'                   TO AT-ADDRESEE-TYPE           
02845          MOVE SS06D                 TO AT-ADDRESSEE-NAME          
02846      ELSE                                                         
02847      IF W-BENEFICIARY > ZEROS                                     
02848           MOVE 'B'                  TO AT-ADDRESEE-TYPE           
02849           IF W-BENEFICIARY = 9                                    
02850               MOVE SS61D            TO AT-ADDRESSEE-NAME          
02851             ELSE                                                  
02852               MOVE SS43D            TO AT-ADDRESSEE-NAME          
02853      ELSE                                                         
02854      IF W-EMPLOYER > ZEROS                                        
02855           MOVE 'E'                  TO AT-ADDRESEE-TYPE           
02856           MOVE SS48D                TO AT-ADDRESSEE-NAME          
02857      ELSE                                                         
02858      IF W-INSURED > ZEROS                                         
02859           MOVE 'I'                  TO AT-ADDRESEE-TYPE           
02860           MOVE SS57D                TO AT-ADDRESSEE-NAME          
02861      ELSE                                                         
02862      IF W-PHYSICIAN > ZEROS                                       
02863           MOVE 'P'                  TO AT-ADDRESEE-TYPE           
02864           MOVE SS47D                TO AT-ADDRESSEE-NAME          
02865      ELSE                                                         
02866      IF W-OTHER-1 > ZEROS                                         
02867           MOVE 'O'                  TO AT-ADDRESEE-TYPE           
02868           MOVE SS49D                TO AT-ADDRESSEE-NAME          
02869      ELSE                                                         
02870      IF W-OTHER-2 > ZEROS                                         
02871           MOVE 'Q'                  TO AT-ADDRESEE-TYPE           
02872           MOVE SS50D                TO AT-ADDRESSEE-NAME          
02873      ELSE                                                         
02874           MOVE SPACES               TO AT-ADDRESEE-TYPE           
02875                                        AT-ADDRESSEE-NAME.         
02876      IF PRINT-PERFORMED                                           
02877         MOVE CURRENT-SAVE        TO AT-INITIAL-PRINT-DATE         
02878      ELSE                                                         
02879         MOVE LOW-VALUES          TO AT-INITIAL-PRINT-DATE.        
02880                                                                   
02881      MOVE LOW-VALUES             TO AT-RESEND-PRINT-DATE.         
02882                                                                   
121802*    IF PI-COMPANY-ID = 'DMD'                                     
121802*        MOVE CL-CERT-STATE          TO WS-DMD-CERT-STATE         
121802*        MOVE CL-CERT-GROUPING (5:2) TO WS-DMD-CERT-GROUPING      
121802*        MOVE BEN-HOLD               TO WS-DMD-BEN-CODE           
121802*        MOVE AT-SEQUENCE-NO         TO WS-DMD-CORR-TRLR-SEQ      
121802*        MOVE AT-STD-LETTER-FORM     TO WS-DMD-LETTER-FORM.       
02889                                                                   
02890      EXEC CICS WRITE                                              
02891           DATASET    (ACTV-ID)                                    
02892           FROM       (ACTIVITY-TRAILERS)                          
02893           RIDFLD     (AT-CONTROL-PRIMARY)                         
02894      END-EXEC.                                                    
02895                                                                   
02896      EXEC CICS HANDLE CONDITION                                   
02897          DUPKEY      (6599-EXIT)                                  
02898      END-EXEC.                                                    
02899                                                                   
02900      EXEC CICS REWRITE                                            
02901           DATASET    (CLAM-ID)                                    
02902           FROM       (CLAIM-MASTER)                               
02903      END-EXEC.                                                    
02904                                                                   
02905  6599-EXIT.                                                       
02906       EXIT.                                                       
02907      EJECT                                                        
02908  6600-SET-ADDR-SEQ.                                               
02909      IF ADDRL = ZEROS                                             
02910         GO TO 6699-EXIT.                                          
02911                                                                   
02912      MOVE ADDRI                  TO PI-ADDR-TYPE                  
02913                                     WS-ADDR-TYPE-CD.              
02914      IF WS-ADDR-SEQ NOT NUMERIC                                   
02915         GO TO 6699-EXIT.                                          
02916                                                                   
02917      MOVE ZEROS                  TO PI-ADDR-SEQ.                  
02918                                                                   
02919      IF WS-ADDR-TYPE = 'I'                                        
02920         MOVE WS-ADDR-SEQ-NUM     TO ACTV-SEQ                      
02921      ELSE                                                         
02922      IF WS-ADDR-TYPE = 'A'                                        
02923         MOVE WS-ADDR-SEQ-NUM     TO ACTV-SEQ                      
02924         ADD +20                  TO ACTV-SEQ                      
02925      ELSE                                                         
02926      IF WS-ADDR-TYPE = 'B'                                        
02927         MOVE WS-ADDR-SEQ-NUM     TO ACTV-SEQ                      
02928         ADD +10                  TO ACTV-SEQ                      
02929      ELSE                                                         
02930      IF WS-ADDR-TYPE = 'P'                                        
02931         MOVE WS-ADDR-SEQ-NUM     TO ACTV-SEQ                      
02932         ADD +30                  TO ACTV-SEQ                      
02933      ELSE                                                         
02934      IF WS-ADDR-TYPE = 'E'                                        
02935         MOVE WS-ADDR-SEQ-NUM     TO ACTV-SEQ                      
02936         ADD +40                  TO ACTV-SEQ                      
02937      ELSE                                                         
02938      IF WS-ADDR-TYPE = 'O'                                        
02939         MOVE WS-ADDR-SEQ-NUM     TO ACTV-SEQ                      
02940         ADD +50                  TO ACTV-SEQ                      
02941      ELSE                                                         
02942      IF WS-ADDR-TYPE = 'Q'                                        
02943         MOVE WS-ADDR-SEQ-NUM     TO ACTV-SEQ                      
02944         ADD +60                  TO ACTV-SEQ.                     
02945                                                                   
02946  6699-EXIT.                                                       
02947       EXIT.                                                       
033110
033110 6700-BUILD-NAPERSOFT.
033110
033110     EXEC CICS GETMAIN                                            
033110          SET      (ADDRESS OF NAPERSOFT-FILE)                    
033110          LENGTH   (NAPS-LENGTH)                                  
033110     END-EXEC.                                                    
033110
033110     MOVE LOW-VALUES            TO  NAPERSOFT-FILE.
033110     MOVE 'NA'                  TO  NA-RECORD-ID.
033110     MOVE PI-COMPANY-CD         TO  NA-COMPANY-CD.
033110     MOVE PI-CARRIER            TO  NA-CARRIER.
033110     MOVE PI-CLAIM-NO           TO  NA-CLAIM-NO.
033110     MOVE PI-CERT-NO            TO  NA-CERT-NO.
033110     MOVE ARCH-NUMBER           TO  NA-ARCHIVE-NO.
033110     MOVE FORMI                 TO  NA-LETTER-ID.
033110     MOVE PI-PROCESSOR-ID       TO  NA-PROCESSOR-ID.
033110     MOVE CURRENT-SAVE          TO  NA-CREATION-DT.
033110     IF PRINT-PERFORMED
033110         MOVE CURRENT-SAVE      TO  NA-INITIAL-PRINT-DT
033110     ELSE
033110         MOVE LOW-VALUES        TO  NA-INITIAL-PRINT-DT
033110     END-IF.
033110     MOVE FOLLOW-UP-SAVE        TO  NA-FOLLOW-UP-DT.
033110     MOVE RESEND-SAVE           TO  NA-RESEND-DT
040110     MOVE PI-RESEND-FORM-NUMBER TO  NA-RESEND-LETTER-ID.
033110     IF COPIESL NOT = ZEROS
033110         MOVE COPIESI           TO  NA-NO-OF-COPIES
033110     ELSE
033110         MOVE 1                 TO  NA-NO-OF-COPIES
033110     END-IF.
033110     IF PI-LETTER-ADDRESS-TYPE = ' 0'
033110         MOVE LOW-VALUES        TO  NA-ADDRESS-TYPE
033110     ELSE
033110         MOVE PI-LETTER-ADDRESS-TYPE TO NA-ADDRESS-TYPE
033110     END-IF.
033110     MOVE CORR-TRLR-SEQ         TO  NA-CORR-TRLR-SEQ.
040110     MOVE PI-ENCLOSURE-CD       TO  NA-ENCLOSURE-CD.
033110
033110     EXEC CICS WRITE                                              
033110          DATASET    (NAPS-ID)                                    
033110          FROM       (NAPERSOFT-FILE)                          
033110          RIDFLD     (NA-CONTROL-PRIMARY)                         
033110     END-EXEC.                                                    
033110
033110 6799-EXIT.
033110      EXIT.
02948                                                                   
02949      EJECT                                                        
02950  7000-READ-ADDR.                                                  
02951      IF PI-RETURN-TO-PROGRAM NOT = PGM-EL150                      
02952         IF ADDRL = ZEROS                                          
02953            GO TO 7099-EXIT                                        
02954         ELSE                                                      
02955            MOVE ER-0374             TO EMI-ERROR                  
02956            MOVE -1                  TO ADDRL                      
02957            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
02958            GO TO 7099-EXIT.                                       
02959                                                                   
02960      MOVE PI-CLAIM-NO            TO CLAM-CLAIM                    
02961                                     ACTV-CLAIM.                   
02962                                                                   
02963      EXEC CICS HANDLE CONDITION                                   
02964           NOTOPEN    (8860-CLAM-NOT-OPEN)                         
02965           NOTFND     (7090-CLAIM-NOT-FOUND)                       
02966       END-EXEC.                                                   
02967                                                                   
02968      EXEC CICS READ                                               
02969           DATASET    (CLAM-ID)                                    
02970           SET        (ADDRESS OF CLAIM-MASTER)                    
02971           RIDFLD     (CLAM-KEY)                                   
02972      END-EXEC.                                                    
02973                                                                   
121802*    IF PI-COMPANY-ID = 'DMD'                                     
121802*            AND                                                  
121802*       NOT SYSTEM-MODIFY-CAP                                     
121802*            AND                                                  
121802*        HIGHEST-PRIORITY                                         
121802*        IF MAINTI = 'R'                                          
121802*            MOVE ER-0909        TO EMI-ERROR                     
121802*            MOVE -1             TO ADDRL                         
121802*            MOVE AL-UABON       TO ADDRA                         
121802*            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
121802*            GO TO 8200-SEND-DATAONLY                             
121802*        ELSE                                                     
121802*            MOVE ER-0900        TO EMI-ERROR                     
121802*            MOVE -1             TO ADDRL                         
121802*            MOVE AL-UABON       TO ADDRA                         
121802*            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
121802*            GO TO 8200-SEND-DATAONLY.                            
02991                                                                   
02992      IF ADDRL = ZEROS                                             
02993         GO TO 7099-EXIT.                                          
02994                                                                   
121802*    IF PI-COMPANY-ID = 'DMD'                                     
121802*        IF MAINTI = 'C'                                          
121802*           IF CL-BENEFICIARY = '0000B0CA10' OR '0000B0CAN0' OR   
121802*                               '0000B0CAS0' OR '0000B0CA50' OR   
121802*                               '0000B0CAA0' OR '0000B0CAB0' OR   
121802*                               '0000B0CAC0' OR '0000B0CAD0' OR   
121802*                               '0000B0CO20' OR '0000B0MN10' OR   
121802*                               '0000B0MN20' OR '0000B0MN30' OR   
121802*                               '0000B0MN40' OR '0000B0MN50' OR   
121802*                               '0000B0MN60' OR '0000B0MN70'      
121802*           IF ADDRI(1:1) = 'B'                                   
121802*              MOVE ER-8158     TO EMI-ERROR                      
121802*              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT           
121802*              MOVE -1          TO MAINTL                         
121802*              GO TO 8200-SEND-DATAONLY.                          
121802*                                                                 
121802*    IF PI-COMPANY-ID = 'DMD'                                     
121802*      PERFORM 4000-SET-BSR THRU 4000-EXIT                        
121802*      IF PI-BSR-AUTOMATED                                        
121802*        IF MAINTI = 'C'                                          
121802*           MOVE ER-7843          TO EMI-ERROR                    
121802*           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              
121802*           MOVE -1               TO MAINTL                       
121802*           GO TO 8200-SEND-DATAONLY.                             
03019                                                                   
03020      PERFORM 6600-SET-ADDR-SEQ THRU 6699-EXIT.                    
03021                                                                   
03022      MOVE ADDRI TO WS-ADDR-TYPE-CD.                               
03023                                                                   
121802*    IF CL-SYSTEM-IDENTIFIER = 'CV'                               
121802*        GO TO 7040-READ-PRODUCER.                                
03026                                                                   
03027  7010-READ-ACCT.                                                  
03028                                                                   
03029      MOVE CL-CERT-GROUPING       TO ACCT-GROUPING.                
03030      MOVE CL-CERT-STATE          TO ACCT-STATE.                   
03031      MOVE CL-CERT-ACCOUNT        TO ACCT-ACCOUNT.                 
03032      MOVE CL-CERT-EFF-DT         TO ACCT-EXP-DATE.                
03033                                                                   
03034      EXEC CICS HANDLE CONDITION                                   
03035           NOTOPEN    (8880-ACCT-NOT-OPEN)                         
03036           NOTFND     (7080-ACCT-NOT-FOUND)                        
03037      END-EXEC.                                                    
03038                                                                   
03039      PERFORM 8000-STARTBR-ERACCT THRU 8000-STARTBR-EXIT.          
03040                                                                   
03041      MOVE ACCT-PARTIAL-KEY       TO ACCT-SAVE-KEY.                
03042                                                                   
03043  7020-READNEXT.                                                   
03044                                                                   
03045      PERFORM 8000-READNEXT-ERACCT THRU 8000-READNEXT-EXIT.        
03046                                                                   
03047      IF ACCT-PARTIAL-KEY NOT = ACCT-SAVE-KEY                      
03048         IF WS-SAVE-ACCT-RECORD = SPACES                           
03049             GO TO 7080-ACCT-NOT-FOUND                             
03050         ELSE                                                      
03051             MOVE 'Y'                 TO  WS-ACCT-READ-SW          
03052             MOVE AM-CONTROL-PRIMARY  TO  ACCT-KEY                 
03053             MOVE WS-SAVE-ACCT-RECORD TO  ACCOUNT-MASTER           
03054             GO TO 7030-CONTINUE-BUILD-ADDR.                       
03055                                                                   
03056      IF AM-EXPIRATION-DT = HIGH-VALUES                            
03057          NEXT SENTENCE                                            
03058      ELSE                                                         
03059          MOVE ACCOUNT-MASTER         TO  WS-SAVE-ACCT-RECORD      
03060          GO TO 7020-READNEXT.                                     
03061                                                                   
03062      MOVE AM-CONTROL-PRIMARY TO ACCT-KEY.                         
03063      MOVE 'Y'                TO WS-ACCT-READ-SW.                  
03064                                                                   
03065  7030-CONTINUE-BUILD-ADDR.                                        
03066                                                                   
03067      MOVE SPACES             TO  WS-SAVE-ACCT-RECORD.             
03068                                                                   
03069      IF WS-ADDR-TYPE-CD = 'A0'                                    
03070         MOVE ZEROS               TO PI-ADDR-SEQ                   
03071      ELSE                                                         
03072         GO TO 7067-CHECK-BENE-ADDR.                               
03073                                                                   
03074      MOVE SPACES             TO               WS-LABEL-HOLD-AREA. 
03075      MOVE AM-NAME            TO  REC-TEXT (1) WS-LABEL-LINES (1). 
03076      MOVE AM-PERSON          TO  REC-TEXT (2) WS-LABEL-LINES (2). 
03077      MOVE AM-ADDRS           TO  REC-TEXT (3) WS-LABEL-LINES (3). 
03078 *    MOVE AM-CITY            TO  REC-TEXT (4) WS-LABEL-LINES (4). 
           STRING AM-ADDR-CITY ' ' AM-ADDR-STATE DELIMITED BY '  '
              INTO WS-LABEL-LINES (4)
           END-STRING
           MOVE WS-LABEL-LINES (4)     TO REC-TEXT (4)
03079      MOVE SPACES             TO  REC-TEXT (5) WS-LABEL-LINES (5). 
03080                                                                   
03081      MOVE SPACES             TO  WS-ZIP-CODE.                     
03082                                                                   
03083      IF AM-CANADIAN-POST-CODE                                     
03084          MOVE AM-CAN-POSTAL-1    TO  WS-CAN-POSTAL-1              
03085          MOVE AM-CAN-POSTAL-2    TO  WS-CAN-POSTAL-2              
03086      ELSE                                                         
03087          MOVE AM-ZIP-PRIME       TO  WS-AM-ZIP-CODE               
03088          IF AM-ZIP-PLUS4 NOT = SPACES  AND  ZEROS                 
03089              MOVE '-'            TO  WS-AM-ZIP-DASH               
03090              MOVE AM-ZIP-PLUS4   TO  WS-AM-ZIP-PLUS4.             
03091                                                                   
03092      MOVE WS-ZIP-CODE        TO  REC-TEXT (6) WS-LABEL-LINES (6). 
03093                                                                   
03094      PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.                      
03095                                                                   
03096      MOVE WS-LABEL-LINES (1) TO  REC-TEXT (1)    SS05-1D.         
03097      MOVE WS-LABEL-LINES (2) TO  REC-TEXT (2)    SS05-2D.         
03098      MOVE WS-LABEL-LINES (3) TO  REC-TEXT (3)    SS05-3D.         
03099      MOVE WS-LABEL-LINES (4) TO  REC-TEXT (4)    SS05-4D.         
03100      MOVE WS-LABEL-LINES (5) TO  REC-TEXT (5)    SS05-5D.         
03101      MOVE WS-LABEL-LINES (6) TO  REC-TEXT (6)    SS05-6D.         
03102                                                                   
03103      GO TO 7099-EXIT.                                             
03104                                                                   
03105      EJECT                                                        
121802*7040-READ-PRODUCER. Remove as dead code                          
121802*7050-READNEXT. Remove as dead code                               
121802*7060-CONTINUE-BUILD-ADDR. Remove as dead code                    

03181  7067-CHECK-BENE-ADDR.                                            
03182                                                                   
121802*    IF PI-COMPANY-ID = 'DMD'                                     
121802*        IF WS-ADDR-TYPE-CD = 'B9'                                
121802*            GO TO 7067-CHECK-BENE-ADDR-9.                        
03186                                                                   
03187      IF WS-ADDR-TYPE-CD NOT = 'B0'                                
03188          GO TO 7068-CONTINUE-BUILD-ADDR.                          
03189                                                                   
03190      IF CL-BENEFICIARY = SPACES                                   
03191          GO TO 7070-ACTV-NOT-FOUND.                               
03192                                                                   
03193      EXEC CICS HANDLE CONDITION                                   
03194           NOTFND     (7070-ACTV-NOT-FOUND)                        
03195      END-EXEC.                                                    
03196                                                                   
03197      MOVE PI-COMPANY-CD          TO BENE-COMP-CD.                 
03198      MOVE 'B'                    TO BENE-REC-TYPE.                
03199      MOVE CL-BENEFICIARY         TO BENE-NUMBER.                  
03200                                                                   
03201      EXEC CICS READ                                               
03202           DATASET    (BENE-ID)                                    
03203           SET        (ADDRESS OF BENEFICIARY-MASTER)              
03204           RIDFLD     (BENE-KEY)                                   
03205      END-EXEC.                                                    
03206                                                                   
03207      MOVE SPACES              TO  WS-LABEL-HOLD-AREA.             
03208      MOVE BE-MAIL-TO-NAME     TO  REC-TEXT (1) WS-LABEL-LINES (1).
03209      MOVE BE-ADDRESS-LINE-1   TO  REC-TEXT (2) WS-LABEL-LINES (2).
03210      MOVE BE-ADDRESS-LINE-2   TO  REC-TEXT (3) WS-LABEL-LINES (3).
03211      MOVE BE-ADDRESS-LINE-3   TO  REC-TEXT (4) WS-LABEL-LINES (4).
03212 *    MOVE BE-CITY-STATE       TO  REC-TEXT (5) WS-LABEL-LINES (5).
           STRING BE-CITY ' ' BE-STATE DELIMITED BY '  '
              INTO WS-LABEL-LINES (5)
           END-STRING
           MOVE WS-LABEL-LINES (5)     TO REC-TEXT (5)
03213                                                                   
03214      MOVE SPACES              TO  WS-ZIP-CODE.                    
03215      IF BE-CANADIAN-POST-CODE                                     
03216          MOVE BE-CAN-POSTAL-1    TO  WS-CAN-POSTAL-1              
03217          MOVE BE-CAN-POSTAL-2    TO  WS-CAN-POSTAL-2              
03218      ELSE                                                         
03219          MOVE BE-ZIP-PRIME       TO  WS-AM-ZIP-CODE               
03220          IF BE-ZIP-PLUS4 NOT = SPACES  AND  ZEROS                 
03221              MOVE '-'            TO  WS-AM-ZIP-DASH               
03222              MOVE BE-ZIP-PLUS4   TO  WS-AM-ZIP-PLUS4.             
03223                                                                   
03224      MOVE WS-ZIP-CODE         TO  REC-TEXT (6) WS-LABEL-LINES (6).
03225                                                                   
03226      GO TO 7069-SET-ADDR.                                         
03227                                                                   
03228  7067-CHECK-BENE-ADDR-9.                                          
03229                                                                   
03230      IF CL-BENEFICIARY = SPACES                                   
03231          GO TO 7070-ACTV-NOT-FOUND.                               
03232                                                                   
03233      EXEC CICS HANDLE CONDITION                                   
03234           NOTFND     (7070-ACTV-NOT-FOUND)                        
03235      END-EXEC.                                                    
03236                                                                   
03237      MOVE PI-COMPANY-CD          TO BENE-COMP-CD.                 
03238      MOVE 'B'                    TO BENE-REC-TYPE.                
03239      MOVE CL-BENEFICIARY         TO BENE-NUMBER.                  
03240                                                                   
03241      EXEC CICS READ                                               
03242           DATASET    (BENE-ID)                                    
03243           SET        (ADDRESS OF BENEFICIARY-MASTER)              
03244           RIDFLD     (BENE-KEY)                                   
03245      END-EXEC.                                                    
03246                                                                   
03247      MOVE SPACES                 TO WS-LABEL-HOLD-AREA.           
03248      MOVE BE-MAIL-TO-NAME2       TO REC-TEXT (1)                  
03249                                     WS-LABEL-LINES (1).           
03250      MOVE BE-ADDRESS-LINE-12     TO REC-TEXT (2)                  
03251                                     WS-LABEL-LINES (2).           
03252      MOVE BE-ADDRESS-LINE-22     TO REC-TEXT (3)                  
03253                                     WS-LABEL-LINES (3).           
03254      MOVE BE-ADDRESS-LINE-32     TO REC-TEXT (4)                  
03255                                     WS-LABEL-LINES (4).           
03256 *    MOVE BE-CITY-STATE2         TO REC-TEXT (5)                  
03257 *                                   WS-LABEL-LINES (5).           
           STRING BE-CITY2 ' ' BE-STATE2 DELIMITED BY '  '
              INTO WS-LABEL-LINES (5)
           END-STRING
           MOVE WS-LABEL-LINES (5)     TO REC-TEXT (5)
03258                                                                   
03259      MOVE SPACES                 TO WS-ZIP-CODE.                  
03260                                                                   
03261      IF BE-CANADIAN-POST-CODE2                                    
03262          MOVE BE-CAN-POSTAL-12   TO WS-CAN-POSTAL-1               
03263          MOVE BE-CAN-POSTAL-22   TO WS-CAN-POSTAL-2               
03264      ELSE                                                         
03265          MOVE BE-ZIP-PRIME2      TO WS-AM-ZIP-CODE                
03266          IF BE-ZIP-PLUS42 NOT = SPACES AND ZEROS                  
03267              MOVE '-'            TO WS-AM-ZIP-DASH                
03268              MOVE BE-ZIP-PLUS42  TO WS-AM-ZIP-PLUS4.              
03269                                                                   
03270      MOVE WS-ZIP-CODE            TO REC-TEXT (6)                  
03271                                     WS-LABEL-LINES (6).           
03272                                                                   
03273      GO TO 7069-SET-ADDR.                                         
03274                                                                   
03275  7068-CONTINUE-BUILD-ADDR.                                        
03276                                                                   
03277      IF ACTV-SEQ = ZEROS                                          
03278         GO TO 7070-ACTV-NOT-FOUND.                                
03279                                                                   
03280      MOVE ACTV-SEQ               TO PI-ADDR-SEQ.                  
03281                                                                   
03282      EXEC CICS HANDLE CONDITION                                   
03283           NOTOPEN    (8870-ACTV-NOT-OPEN)                         
03284           NOTFND     (7070-ACTV-NOT-FOUND)                        
03285      END-EXEC.                                                    
03286                                                                   
03287      EXEC CICS READ                                               
03288           DATASET    (ACTV-ID)                                    
03289           SET        (ADDRESS OF ACTIVITY-TRAILERS)               
03290           RIDFLD     (ACTV-KEY)                                   
03291      END-EXEC.                                                    
03292                                                                   
03293      MOVE SPACES            TO              WS-LABEL-HOLD-AREA.   
03294      MOVE AT-MAIL-TO-NAME   TO REC-TEXT (1) WS-LABEL-LINES (1).   
03295      MOVE AT-ADDRESS-LINE-1 TO REC-TEXT (2) WS-LABEL-LINES (2).   
03296      MOVE AT-ADDRESS-LINE-2 TO REC-TEXT (3) WS-LABEL-LINES (3).   
03297 *    MOVE AT-CITY-STATE     TO REC-TEXT (4) WS-LABEL-LINES (4).   
           STRING AT-CITY ' ' AT-STATE DELIMITED BY '  '
              INTO WS-LABEL-LINES (4)
           END-STRING
           MOVE WS-LABEL-LINES (4)     TO REC-TEXT (4)
03298                                                                   
03299      MOVE SPACES            TO  WS-ZIP-CODE.                      
03300      IF AT-CANADIAN-POST-CODE                                     
03301          MOVE AT-CAN-POSTAL-1    TO  WS-CAN-POSTAL-1              
03302          MOVE AT-CAN-POSTAL-2    TO  WS-CAN-POSTAL-2              
03303      ELSE                                                         
03304          MOVE AT-ZIP-CODE        TO  WS-AM-ZIP-CODE               
03305          IF AT-ZIP-PLUS4 NOT = SPACES  AND  ZEROS                 
03306              MOVE '-'            TO  WS-AM-ZIP-DASH               
03307              MOVE AT-ZIP-PLUS4   TO  WS-AM-ZIP-PLUS4.             
03308                                                                   
03309      MOVE WS-ZIP-CODE       TO  REC-TEXT (5) WS-LABEL-LINES (5).  
03310                                                                   
03311      MOVE SPACES            TO  REC-TEXT (6).                     
03312                                                                   
03313  7069-SET-ADDR.                                                   
03314      PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.                      
03315                                                                   
03316      MOVE WS-LABEL-LINES (1) TO  REC-TEXT (1)    SS05-1D.         
03317      MOVE WS-LABEL-LINES (2) TO  REC-TEXT (2)    SS05-2D.         
03318      MOVE WS-LABEL-LINES (3) TO  REC-TEXT (3)    SS05-3D.         
03319      MOVE WS-LABEL-LINES (4) TO  REC-TEXT (4)    SS05-4D.         
03320      MOVE WS-LABEL-LINES (5) TO  REC-TEXT (5)    SS05-5D.         
03321      MOVE SPACES             TO  REC-TEXT (6)    SS05-6D.         
03322                                                                   
03323      GO TO 7099-EXIT.                                             
03324                                                                   
03325  7070-ACTV-NOT-FOUND.                                             
03326                                                                   
03327      IF ACTV-SEQ NOT = +29                                        
03328         GO TO 7075-CONTINUE-ACTV-ERROR.                           
03329                                                                   
121802*    IF CL-SYSTEM-IDENTIFIER = 'CV'                               
121802*        GO TO 7099-EXIT.                                         
03332                                                                   
03333      IF AM-3RD-PARTY-NOTIF-LEVEL NOT NUMERIC                      
03334         MOVE ZEROS         TO AM-3RD-PARTY-NOTIF-LEVEL            
03335         GO TO 7075-CONTINUE-ACTV-ERROR.                           
03336                                                                   
03337      IF AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL) = SPACES OR ZEROS       
03338         GO TO 7075-CONTINUE-ACTV-ERROR.                           
03339                                                                   
03340      MOVE PI-COMPANY-CD   TO WS-ERCOMP-COMPANY-CD                 
03341      MOVE AM-CARRIER      TO WS-ERCOMP-CARRIER                    
03342      MOVE AM-GROUPING     TO WS-ERCOMP-GROUPING                   
03343      MOVE 'A'             TO WS-ERCOMP-TYPE                       
03344      MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)                       
03345                           TO WS-ERCOMP-RESP-NO.                   
03346                                                                   
03347      IF AM-3RD-PARTY-NOTIF-LEVEL = AM-REMIT-TO                    
03348          IF AM-COM-TYP (AM-REMIT-TO) = 'O' OR 'P' OR              
052814                                       'G' OR 'B' or 'S'
03350              MOVE 'G'            TO WS-ERCOMP-TYPE                
03351              MOVE LOW-VALUES     TO WS-ERCOMP-ACCOUNT             
03352          ELSE                                                     
03353              MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)               
03354                                  TO WS-ERCOMP-ACCOUNT             
03355      ELSE                                                         
03356          MOVE 'G'                TO WS-ERCOMP-TYPE                
03357          MOVE LOW-VALUES         TO WS-ERCOMP-ACCOUNT.            
03358                                                                   
03359      IF PI-ZERO-CARRIER OR PI-ZERO-CAR-GROUP                      
03360         MOVE ZEROS TO WS-ERCOMP-CARRIER.                          
03361                                                                   
03362      IF PI-ZERO-GROUPING OR PI-ZERO-CAR-GROUP                     
03363         MOVE ZEROS TO WS-ERCOMP-GROUPING.                         
03364                                                                   
03365      EXEC CICS HANDLE CONDITION                                   
03366           NOTFND    (7075-CONTINUE-ACTV-ERROR)                    
03367      END-EXEC.                                                    
03368                                                                   
03369      EXEC CICS  READ                                              
03370           SET      (ADDRESS OF COMPENSATION-MASTER)               
03371           DATASET  ('ERCOMP')                                     
03372           RIDFLD   (WS-ERCOMP-KEY)                                
03373      END-EXEC.                                                    
03374                                                                   
03375      MOVE 'Y'                   TO WS-COMP-READ-SW.               
03376      MOVE SPACES                TO WS-LABEL-HOLD-AREA.            
03377      MOVE CO-ACCT-NAME          TO WS-LABEL-LINES (1).            
03378                                                                   
03379      IF CO-ACCT-NAME = SPACES                                     
03380         MOVE CO-MAIL-NAME       TO WS-LABEL-LINES (1).            
03381                                                                   
03382      MOVE CO-ADDR-1             TO WS-LABEL-LINES (2).            
03383      MOVE CO-ADDR-2             TO WS-LABEL-LINES (3).            
03384      MOVE CO-ADDR-3             TO WS-LABEL-LINES (4).            
03385                                                                   
03386      MOVE SPACES                TO  WS-ZIP-CODE.                  
03387      IF CO-CANADIAN-POST-CODE                                     
03388          MOVE CO-CAN-POSTAL-1   TO  WS-CAN-POSTAL-1               
03389          MOVE CO-CAN-POSTAL-2   TO  WS-CAN-POSTAL-2               
03390      ELSE                                                         
03391          MOVE CO-ZIP-PRIME      TO  WS-AM-ZIP-CODE                
03392          IF CO-ZIP-PLUS4 NOT = SPACES  AND  ZEROS                 
03393              MOVE '-'           TO  WS-AM-ZIP-DASH                
03394              MOVE CO-ZIP-PLUS4  TO  WS-AM-ZIP-PLUS4.              
03395                                                                   
03396      MOVE WS-ZIP-CODE           TO  WS-LABEL-LINES (5).           
03397                                                                   
03398      MOVE ZEROS                 TO WS-PHONE-IN.                   
03399      MOVE CO-AREA-CODE          TO WSPO-AREA.                     
03400      MOVE CO-PREFIX             TO WSPO-PFX.                      
03401      MOVE CO-PHONE              TO WSPO-SFX.                      
03402      MOVE WS-PHONE-OUT          TO SS53-6D.                       
03403                                                                   
03404      PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.                      
03405                                                                   
03406      MOVE WS-LABEL-LINES (1)     TO REC-TEXT (1)    SS52D.        
03407      MOVE WS-LABEL-LINES (2)     TO REC-TEXT (2)    SS53-1D.      
03408      MOVE WS-LABEL-LINES (3)     TO REC-TEXT (3)    SS53-2D.      
03409      MOVE WS-LABEL-LINES (4)     TO REC-TEXT (4)    SS53-3D.      
03410      MOVE WS-LABEL-LINES (5)     TO REC-TEXT (5)    SS53-4D.      
03411      MOVE WS-LABEL-LINES (6)     TO REC-TEXT (6)    SS53-5D.      
03412                                                                   
03413      GO TO 7099-EXIT.                                             
03414      EJECT                                                        
03415                                                                   
03416  7075-CONTINUE-ACTV-ERROR.                                        
03417      MOVE ER-0178                TO EMI-ERROR.                    
03418      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
03419      MOVE -1                     TO ADDRL.                        
03420      MOVE AL-UABON               TO ADDRA.                        
03421      MOVE SPACES                 TO PI-ADDR-TYPE.                 
03422      MOVE SPACES                 TO REC-TEXT (1)                  
03423                                     REC-TEXT (2)                  
03424                                     REC-TEXT (3)                  
03425                                     REC-TEXT (4)                  
03426                                     REC-TEXT (5)                  
03427                                     REC-TEXT (6).                 
03428      GO TO 7099-EXIT.                                             
03429                                                                   
03430  7080-ACCT-NOT-FOUND.                                             
03431      MOVE ER-0179                TO EMI-ERROR.                    
03432      MOVE -1                     TO ADDRL.                        
03433      MOVE AL-UABON               TO ADDRA.                        
03434      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
03435      GO TO 8200-SEND-DATAONLY.                                    
03436                                                                   
03437  7090-CLAIM-NOT-FOUND.                                            
03438      MOVE ER-0186                TO EMI-ERROR.                    
03439      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
03440      GO TO 8200-SEND-DATAONLY.                                    
03441                                                                   
121802*7095-PROD-NOT-FOUND. Remove as dead code                         
03448                                                                   
03449  7099-EXIT.                                                       
03450       EXIT.                                                       
03451      EJECT                                                        
03452                                                                   
03453  7170-FORMAT-SCREEN.                                              
03454      IF MAINTI = 'S'                                              
03455         MOVE AL-PANOF            TO SC-TEXTA (SC-INDX)            
03456        ELSE                                                       
03457         MOVE AL-UANOF            TO SC-TEXTA (SC-INDX).           
03458                                                                   
03459      IF TB-INDX < 7                                               
03460         SET LINE23 TO TB-INDX                                     
03461         MOVE 'A'                 TO LINE1                         
03462        ELSE                                                       
03463         IF TB-INDX = 7                                            
03464            MOVE ZEROS            TO LINE-NUM                      
03465         ELSE                                                      
03466            SET LIN-NUM TO TB-INDX                                 
03467            SUBTRACT 7            FROM LIN-NUM.                    
03468                                                                   
03469      MOVE LINE-NUM               TO SC-LINE (SC-INDX).            
03470      MOVE REC-TEXT (TB-INDX)     TO SC-TEXT (SC-INDX).            
03471      SET TB-INDX UP BY 1.                                         
03472                                                                   
03473  7170-EXIT.                                                       
03474       EXIT.                                                       
03475                                                                   
03476      EJECT                                                        
03477  7200-RESOLVE-VARIABLES.                                          
03478 ***************************************************************   
03479 *    THIS ROUTINE WILL FORMAT THE SYSTEM-DEFINED SYMBOLS      *   
03480 *    WITH DATA PERTAINING TO THE DESIGNATED CLAIM.            *   
03481 *    THIS ROUTINE IS PERFORMED THRU 7269-EXIT TO              *   
03482 *    RESOLVE ALL OF THE SYMBOLS.                              *   
03483 ***************************************************************   
03484                                                                   
121802*    IF PI-COMPANY-ID = 'AUK'                                     
121802*        MOVE 'Y'                TO W-REVERSE-DATE-SW.            
03487                                                                   
03488      MOVE PI-CLAIM-NO            TO SS34D.                        
03489                                                                   
100518     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
03491         MOVE PI-LIFE-OVERRIDE-L6 TO SS12D                         
03492      ELSE                                                         
03493         MOVE PI-AH-OVERRIDE-L6   TO SS12D.                        
03494                                                                   
03495      MOVE CL-LAST-PMT-AMT        TO SS16D.                        
03496      MOVE CL-TOTAL-PAID-AMT      TO SS18D.                        
03497      MOVE CL-CAUSE-CD            TO SS19-1D.                      
03498      MOVE CL-ASSOC-CERT-SEQU     TO SS54D.                        
03499      MOVE CL-ASSOC-CERT-TOTAL    TO SS55D.                        
03500                                                                   
121802*    IF PI-COMPANY-ID NOT = 'DMD'                                 
121802*        GO TO 7200-CONTINUE.                                     
03503                                                                   
03504      IF CL-CCN-PREFIX-A5 = '1111' OR '3333' OR '6666' OR          
03505                            '8888' OR '9876'                       
03506          MOVE CL-CCN-PRIME-A5    TO WS-CCN-12                     
03507          MOVE WS-CCN-12          TO CL-CCN.                       
03508                                                                   
03509      IF CL-CERT-STATE NOT = '08' AND '09'                         
03510          MOVE SPACES             TO SS62D                         
03511                                     SS63D                         
03512          GO TO 7200-CONTINUE.                                     
03513                                                                   
03514      MOVE WS-DMD-UND-STATEMENT   TO SS62D.                        
03515                                                                   
03516      MOVE 'CL'                   TO DL23-SYSTEM-ID.               
03517      MOVE 'UN'                   TO DL23-RECORD-TYPE.             
03518      MOVE CL-CERT-STATE          TO DL23-RECORD-KEY.              
03519                                                                   
03520      EXEC CICS LINK                                               
03521          PROGRAM    ('DLO023')                                    
03522          COMMAREA   (WS-DLO-CODES-TABLE)                          
03523          LENGTH     (DL23-COMM-LENGTH)                            
03524      END-EXEC.                                                    
03525                                                                   
03526      IF DL23-RETURN-CODE = 'OK'                                   
03527          MOVE DL23-CODE-DESC     TO SS63D                         
03528        ELSE                                                       
03529          MOVE SPACES             TO SS63D.                        
03530                                                                   
03531  7200-CONTINUE.                                                   
03532      MOVE CL-CCN                 TO SS60D.                        
03533                                                                   
03534      MOVE CL-INCURRED-DT         TO DC-BIN-DATE-1.                
03535      MOVE SPACES                 TO DC-OPTION-CODE.               
03536      PERFORM 9700-DATE-LINK THRU 9700-EXIT.                       
03537      IF NO-CONVERSION-ERROR                                       
03538         MOVE DC-GREG-DATE-1-EDIT TO SS13D.                        
03539                                                                   
03540      MOVE CL-REPORTED-DT         TO DC-BIN-DATE-1.                
03541      MOVE SPACES                 TO DC-OPTION-CODE.               
03542      PERFORM 9700-DATE-LINK THRU 9700-EXIT.                       
03543      IF NO-CONVERSION-ERROR                                       
03544         MOVE DC-GREG-DATE-1-EDIT TO SS14D.                        
03545                                                                   
03546      MOVE CL-LAST-PMT-DT         TO DC-BIN-DATE-1.                
03547      MOVE SPACES                 TO DC-OPTION-CODE.               
03548      PERFORM 9700-DATE-LINK THRU 9700-EXIT.                       
03549      IF NO-CONVERSION-ERROR                                       
03550         MOVE DC-GREG-DATE-1-EDIT TO SS15D.                        
03551                                                                   
03552      IF NOT PI-USES-PAID-TO                                       
03553         MOVE CL-PAID-THRU-DT        TO DC-BIN-DATE-1              
03554         MOVE SPACES                 TO DC-OPTION-CODE             
03555         PERFORM 9700-DATE-LINK THRU 9700-EXIT                     
03556         IF NO-CONVERSION-ERROR                                    
03557            MOVE DC-GREG-DATE-1-EDIT TO SS17D                      
03558         ELSE                                                      
03559            MOVE SPACES TO SS17D                                   
03560      ELSE                                                         
03561         MOVE CL-PAID-THRU-DT        TO DC-BIN-DATE-1              
03562         MOVE '6'                    TO DC-OPTION-CODE             
03563         MOVE +1                     TO DC-ELAPSED-DAYS            
03564         MOVE +0                     TO DC-ELAPSED-MONTHS          
03565         PERFORM 9700-DATE-LINK THRU 9700-EXIT                     
03566         IF NO-CONVERSION-ERROR                                    
03567            MOVE DC-GREG-DATE-1-EDIT TO SS17D.                     
03568                                                                   
03569      MOVE CL-INSURED-BIRTH-DT    TO DC-BIN-DATE-1.                
03570      MOVE SPACES                 TO DC-OPTION-CODE.               
03571      PERFORM 9700-DATE-LINK THRU 9700-EXIT.                       
03572      IF NO-CONVERSION-ERROR                                       
03573         MOVE DC-GREG-DATE-1-EDIT TO SS45D                         
03574      ELSE                                                         
03575         MOVE '@@DOB'             TO SS45D.                        
03576                                                                   
03577      IF CL-SSN-STATE   = CL-CERT-STATE  AND                       
03578         CL-SSN-ACCOUNT = CL-CERT-ACCOUNT-PRIME                    
03579              NEXT SENTENCE                                        
03580          ELSE                                                     
03581              MOVE CL-SOC-SEC-NO  TO SS46D.                        
03582                                                                   
03583      MOVE CL-CERT-GROUPING       TO CERT-GROUPING                 
03584                                     ACCT-GROUPING                 
03585                                     PROD-GROUPING                 
03586                                     PLCY-GROUPING                 
03587                                     PLAN-GROUPING.                
03588      MOVE CL-CERT-STATE          TO CERT-STATE                    
03589                                     ACCT-STATE                    
03590                                     PROD-STATE                    
03591                                     PLCY-STATE                    
03592                                     PLAN-STATE.                   
03593      MOVE CL-CERT-ACCOUNT        TO CERT-ACCOUNT                  
03594                                     ACCT-ACCOUNT                  
03595                                     PROD-PRODUCER                 
03596                                     PLCY-PRODUCER                 
03597                                     PLAN-PRODUCER.                
03598      MOVE CL-CERT-EFF-DT         TO CERT-EFF-DT                   
03599                                     PLCY-EFF-DT.                  
03600      MOVE CL-CV-REFERENCE-NO     TO PLCY-REFERENCE-NO.            
03601                                                                   
03602      PERFORM 5000-MOVE-NAME THRU 5000-EXIT.                       
03603                                                                   
121802*    IF PI-COMPANY-ID = 'DMD'                                     
121802*       MOVE WS-NAME-WORK           TO SS10D                      
121802*       GO TO 7200-CONTINUE-2.                                    
03607                                                                   
03608      IF LOWER-CASE-LETTERS-USED                                   
03609         MOVE 'N'                    TO WS-STATE-LINE              
03610         MOVE WS-NAME-WORK           TO WS-TEMP-AREA2              
03611         PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT          
03612         MOVE WS-TEMP-AREA2          TO SS10D                      
03613      ELSE                                                         
03614         MOVE WS-NAME-WORK           TO SS10D.                     
03615                                                                   
03616  7200-CONTINUE-2.                                                 
03617      MOVE CL-INSURED-1ST-NAME     TO W-FIRST-NAME.                
03618      MOVE CL-INSURED-LAST-NAME    TO W-LAST-NAME.                 
03619      MOVE CL-INSURED-MID-INIT     TO W-MIDDLE-NAME.               
03620      PERFORM 5200-MOVE-NAME THRU 5200-EXIT.                       
03621                                                                   
03622      IF LOWER-CASE-LETTERS-USED                                   
03623         MOVE 'N'                    TO WS-STATE-LINE              
03624         MOVE WS-NAME-WORK           TO WS-TEMP-AREA2              
03625         PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT          
03626         MOVE WS-TEMP-AREA2          TO SS39D                      
03627      ELSE                                                         
03628         MOVE WS-NAME-WORK           TO SS39D.                     
03629                                                                   
03630      MOVE CL-INSURED-LAST-NAME   TO SS40D.                        
03631                                                                   
03632      IF INSURED-IS-FEMALE                                         
03633         MOVE 'MS.'               TO SS41D                         
03634        ELSE                                                       
03635         MOVE 'MR.'               TO SS41D.                        
03636                                                                   
121802*    IF PI-COMPANY-ID = 'AIG' OR 'AUK'                            
121802*        PERFORM 7290-RESOLVE-CREDITOR THRU 7290-EXIT.            
03639                                                                   
121802*    IF CL-SYSTEM-IDENTIFIER = 'CV'                               
121802*        GO TO 7200-READ-EMPLCY.                                  
03642      EJECT                                                        
03643      EXEC CICS HANDLE CONDITION                                   
03644           NOTOPEN   (8900-CERT-NOT-OPEN)                          
03645           NOTFND    (8910-CERT-NOT-FOUND)                         
03646      END-EXEC.                                                    
03647                                                                   
03648      EXEC CICS READ                                               
03649           DATASET   (CERT-ID)                                     
03650           SET       (ADDRESS OF CERTIFICATE-MASTER)               
03651           RIDFLD    (CERT-KEY)                                    
03652      END-EXEC.                                                    
03653                                                                   
121802*    IF PI-COMPANY-ID = 'DMD'                                     
121802*        MOVE CM-RESIDENT-STATE  TO WS-DMD-RES-ST                 
121802*        IF CM-POLICY-FORM-NO(6:1) = '1'                          
121802*            MOVE WS-DMD-UND-COMPANYA    TO SS64D                 
121802*          ELSE                                                   
121802*        IF CM-POLICY-FORM-NO(6:1) = '2'                          
121802*            MOVE WS-DMD-UND-COMPANYB    TO SS64D                 
121802*          ELSE                                                   
121802*            MOVE SPACES                 TO SS64D.                
03663                                                                   
03664      MOVE CM-CERT-NO             TO SS26D.                        
03665      MOVE CM-INSURED-ISSUE-AGE   TO SS33D.                        
03666                                                                   
121802*    IF PI-COMPANY-ID = 'AIG' OR 'AUK'                            
121802*        MOVE CL-CURRENT-CARRIER TO SS23D                         
121802*        MOVE CL-CURRENT-GROUPING                                 
121802*                                TO SS24D                         
121802*        MOVE CL-CURRENT-ACCOUNT TO SS25D                         
121802*        MOVE CM-MEMBER-NO       TO SS36D                         
121802*        MOVE CM-GROUPING        TO W-GROUPING                    
121802*        IF W-GROUP-3 = 'C01' OR 'C02'                            
121802*            MOVE CM-MEMBER-NO   TO W-CURRENT-LOAN-NO             
121802*            MOVE CM-LOAN-NUMBER TO W-LOAN-NO                     
121802*            MOVE W-CREDIT-CARD-LOAN-NO                           
121802*                                TO SS36-1D                       
121802*        ELSE                                                     
121802*            MOVE CM-MEMBER-NO   TO SS36-1D                       
121802*    ELSE                                                         
03682          MOVE CM-CARRIER         TO SS23D.                        
03683          MOVE CM-GROUPING        TO SS24D.                        
03684          MOVE CM-ACCOUNT         TO SS25D.                        
03685          MOVE CM-LOAN-NUMBER     TO SS36D.                        
03686                                                                   
03687      MOVE CM-LOAN-BALANCE        TO SS37D.                        
03688      MOVE CM-MEMBER-NO           TO SS38D.                        
03689                                                                   
03690      MOVE CM-CERT-EFF-DT         TO DC-BIN-DATE-1.                
03691      MOVE SPACES                 TO DC-OPTION-CODE.               
03692      PERFORM 9700-DATE-LINK THRU 9700-EXIT.                       
03693      IF NO-CONVERSION-ERROR                                       
03694         MOVE DC-GREG-DATE-1-EDIT TO SS27D.                        
03695                                                                   
03696      MOVE CM-INSURED-FIRST-NAME  TO W-FIRST-NAME.                 
03697      MOVE CM-INSURED-LAST-NAME   TO W-LAST-NAME.                  
03698      MOVE CM-INSURED-INITIAL2    TO W-MIDDLE-NAME.                
03699      PERFORM 5200-MOVE-NAME THRU 5200-EXIT.                       
03700                                                                   
03701      IF LOWER-CASE-LETTERS-USED                                   
03702          MOVE 'N'                TO WS-STATE-LINE                 
03703          MOVE WS-NAME-WORK       TO WS-TEMP-AREA2                 
03704          PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT         
03705          MOVE WS-TEMP-AREA2      TO SS57D                         
03706      ELSE                                                         
03707          MOVE WS-NAME-WORK       TO SS57D.                        
03708                                                                   
03709      MOVE CM-JT-FIRST-NAME       TO W-FIRST-NAME.                 
03710      MOVE CM-JT-LAST-NAME        TO W-LAST-NAME.                  
03711      MOVE CM-JT-INITIAL          TO W-MIDDLE-NAME.                
03712      PERFORM 5200-MOVE-NAME THRU 5200-EXIT.                       
03713                                                                   
03714      IF LOWER-CASE-LETTERS-USED                                   
03715          MOVE 'N'                TO WS-STATE-LINE                 
03716          MOVE WS-NAME-WORK       TO WS-TEMP-AREA2                 
03717          PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT         
03718          MOVE WS-TEMP-AREA2      TO SS58D                         
03719      ELSE                                                         
03720          MOVE WS-NAME-WORK       TO SS58D.                        
03721                                                                   
100518     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
03723         MOVE CM-LF-BENEFIT-CD    TO BEN-HOLD                      
03724         MOVE CM-LF-ORIG-TERM     TO SS29D                         
03725         MOVE CM-LF-BENEFIT-AMT   TO SS30D                         
03726         MOVE CM-POLICY-FORM-NO   TO SS32D                         
03727         MOVE CM-LF-CANCEL-DT     TO DC-BIN-DATE-1                 
03728      ELSE                                                         
03729         MOVE CM-AH-BENEFIT-CD    TO BEN-HOLD                      
03730         MOVE CM-AH-ORIG-TERM     TO SS29D                         
03731         MOVE CM-AH-BENEFIT-AMT   TO SS30D                         
03732         MOVE CM-POLICY-FORM-NO   TO SS32D                         
03733         MOVE CM-AH-CANCEL-DT     TO DC-BIN-DATE-1.                
03734                                                                   
03735      COMPUTE WORK-AMOUNT = CM-AH-ORIG-TERM * CM-AH-BENEFIT-AMT.   
03736                                                                   
03737      MOVE WORK-AMOUNT            TO SS51D.                        
03738                                                                   
03739      MOVE SPACES                 TO DC-OPTION-CODE.               
03740      PERFORM 9700-DATE-LINK THRU 9700-EXIT.                       
03741      IF NO-CONVERSION-ERROR                                       
03742         MOVE DC-GREG-DATE-1-EDIT TO SS31D.                        
03743                                                                   
03744      MOVE ' '                         TO  DC-OPTION-CODE.         
03745      MOVE +0                          TO  DC-ELAPSED-MONTHS       
03746                                           DC-ELAPSED-DAYS.        
03747                                                                   
100518     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
03749          MOVE CM-LF-LOAN-EXPIRE-DT    TO  DC-BIN-DATE-1           
03750          PERFORM 9700-DATE-LINK THRU 9700-EXIT                    
03751          IF NO-CONVERSION-ERROR                                   
03752              MOVE DC-GREG-DATE-1-EDIT TO  SS28D                   
03753          ELSE                                                     
03754              NEXT SENTENCE                                        
03755      ELSE                                                         
03756          MOVE CM-AH-LOAN-EXPIRE-DT    TO  DC-BIN-DATE-1           
03757          PERFORM 9700-DATE-LINK THRU 9700-EXIT                    
03758          IF NO-CONVERSION-ERROR                                   
03759              MOVE DC-GREG-DATE-1-EDIT TO  SS28D.                  
03760                                                                   
121802*    GO TO 7200-READ-DIAGNOSIS.                                   
03762                                                                   
03763      EJECT                                                        
121802*7200-READ-EMPLCY. Remove as dead code                            
03844  7200-READ-DIAGNOSIS.                                             
03845      EXEC CICS HANDLE CONDITION                                   
03846           NOTOPEN    (8870-ACTV-NOT-OPEN)                         
PEMMOD          NOTFND     (7200-READ-LOAN-NUMBER)                      
03848      END-EXEC.                                                    
03849                                                                   
03850      MOVE CL-CONTROL-PRIMARY    TO ACTV-KEY.                      
03851      MOVE +90                   TO ACTV-SEQ.                      
03852                                                                   
03853      EXEC CICS READ                                               
03854           DATASET  (ACTV-ID)                                      
03855           SET      (ADDRESS OF ACTIVITY-TRAILERS)                 
03856           RIDFLD   (ACTV-KEY)                                     
03857      END-EXEC.                                                    
03858                                                                   
03859      IF AT-TRAILER-TYPE = '6'                                     
03860         MOVE AT-INFO-LINE-1         TO SS19D.                     
03861                                                                   
PEMMOD 7200-READ-LOAN-NUMBER.                                           
PEMMOD
PEMMOD     EXEC CICS HANDLE CONDITION                                   
PEMMOD          NOTOPEN    (8870-ACTV-NOT-OPEN)                         
PEMMOD          NOTFND     (7201-READ-BENEFICIARY)                      
PEMMOD     END-EXEC.                                                    
PEMMOD                                                                  
PEMMOD     MOVE CL-CONTROL-PRIMARY    TO ACTV-KEY.                      
PEMMOD     MOVE +91                   TO ACTV-SEQ.                      
PEMMOD                                                                  
PEMMOD     EXEC CICS READ                                               
PEMMOD          DATASET  (ACTV-ID)                                      
PEMMOD          SET      (ADDRESS OF ACTIVITY-TRAILERS)                 
PEMMOD          RIDFLD   (ACTV-KEY)                                     
PEMMOD     END-EXEC.                                                    
PEMMOD                                                                  
PEMMOD     IF AT-TRAILER-TYPE = '6'                                     
PEMMOD        MOVE AT-INFO-LINE-1         TO SS19-2D.                   
PEMMOD                                                                  
03862  7201-READ-BENEFICIARY.                                           
03863      IF CL-BENIF-ADDR-CNT = +0  AND                               
03864         CL-BENEFICIARY = SPACES                                   
03865           GO TO 7205-READ-PHYSICIAN-ADDR.                         
03866                                                                   
03867      IF W-BENEFICIARY > ZERO                                      
03868          COMPUTE ACTV-SEQ = W-BENEFICIARY + 10                    
03869      ELSE                                                         
03870          MOVE CL-BENIF-ADDR-CNT  TO ACTV-SEQ                      
03871          ADD +10                 TO ACTV-SEQ.                     
03872                                                                   
03873      EXEC CICS HANDLE CONDITION                                   
03874           NOTOPEN    (7290-BENE-NOT-OPEN)                         
03875           NOTFND     (7205-READ-PHYSICIAN-ADDR)                   
03876      END-EXEC.                                                    
03877                                                                   
03878      MOVE SPACES                 TO WS-LABEL-HOLD-AREA.           
03879                                                                   
03880      IF ACTV-SEQ = +19                                            
03881          NEXT SENTENCE                                            
03882      ELSE                                                         
03883          IF ACTV-SEQ NOT = +10                                    
03884              GO TO 7202-GET-FROM-ACTIVITY.                        
03885                                                                   
03886      MOVE PI-COMPANY-CD        TO BENE-COMP-CD.                   
03887      MOVE 'B'                  TO BENE-REC-TYPE.                  
03888      MOVE CL-BENEFICIARY       TO BENE-NUMBER.                    
03889                                                                   
03890      EXEC CICS READ                                               
03891           DATASET    (BENE-ID)                                    
03892           SET        (ADDRESS OF BENEFICIARY-MASTER)              
03893           RIDFLD     (BENE-KEY)                                   
03894      END-EXEC.                                                    
03895                                                                   
121802*    IF PI-COMPANY-ID = 'DMD'                                     
121802*        MOVE BE-BSR             TO PI-ELBENE-BSR.                
03898                                                                   
03899  7201-CONT.                                                       
03900      MOVE BE-MAIL-TO-NAME2       TO WS-LABEL-LINES (1).           
03901      MOVE BE-ADDRESS-LINE-12     TO WS-LABEL-LINES (2).           
03902      MOVE BE-ADDRESS-LINE-22     TO WS-LABEL-LINES (3).           
03903      MOVE BE-ADDRESS-LINE-32     TO WS-LABEL-LINES (4).           
03904 *    MOVE BE-CITY-STATE2         TO WS-LABEL-LINES (5).           
           STRING BE-CITY2 ' ' BE-STATE2 DELIMITED BY '  '
              INTO WS-LABEL-LINES (5)
           END-STRING
03905                                                                   
03906      MOVE SPACES                 TO WS-ZIP-CODE.                  
03907                                                                   
03908      IF BE-CANADIAN-POST-CODE2                                    
03909          MOVE BE-CAN-POSTAL-12   TO WS-CAN-POSTAL-1               
03910          MOVE BE-CAN-POSTAL-22   TO WS-CAN-POSTAL-2               
03911      ELSE                                                         
03912          MOVE BE-ZIP-PRIME2      TO WS-AM-ZIP-CODE                
03913          IF BE-ZIP-PLUS42 NOT = SPACES AND ZEROS                  
03914              MOVE '-'            TO WS-AM-ZIP-DASH                
03915              MOVE BE-ZIP-PLUS42  TO WS-AM-ZIP-PLUS4.              
03916                                                                   
03917      MOVE WS-ZIP-CODE            TO WS-LABEL-LINES (6).           
03918                                                                   
03919      PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.                      
03920                                                                   
03921      MOVE WS-LABEL-LINES (1)     TO SS61D.                        
03922      MOVE WS-LABEL-LINES (2)     TO SS61-1D.                      
03923      MOVE WS-LABEL-LINES (3)     TO SS61-2D.                      
03924      MOVE WS-LABEL-LINES (4)     TO SS61-3D.                      
03925      MOVE WS-LABEL-LINES (5)     TO SS61-4D.                      
03926      MOVE WS-LABEL-LINES (6)     TO SS61-5D.                      
03927                                                                   
03928      MOVE BE-PHONE-NO2           TO WS-PHONE-IN.                  
03929      MOVE WSPI-AREA              TO WSPO-AREA.                    
03930      MOVE WSPI-PFX               TO WSPO-PFX.                     
03931      MOVE WSPI-SFX               TO WSPO-SFX.                     
03932      MOVE WS-PHONE-OUT           TO SS61-6D.                      
03933                                                                   
03934  7201-CONT-PROCESS.                                               
03935      MOVE BE-MAIL-TO-NAME      TO  WS-LABEL-LINES (1).            
03936      MOVE BE-ADDRESS-LINE-1    TO  WS-LABEL-LINES (2).            
03937      MOVE BE-ADDRESS-LINE-2    TO  WS-LABEL-LINES (3).            
03938      MOVE BE-ADDRESS-LINE-3    TO  WS-LABEL-LINES (4).            
03939 *    MOVE BE-CITY-STATE        TO  WS-LABEL-LINES (5).            
           STRING BE-CITY ' ' BE-STATE DELIMITED BY '  '
              INTO WS-LABEL-LINES (5)
           END-STRING
03940                                                                   
03941      MOVE SPACES               TO  WS-ZIP-CODE.                   
03942      IF BE-CANADIAN-POST-CODE                                     
03943          MOVE BE-CAN-POSTAL-1  TO  WS-CAN-POSTAL-1                
03944          MOVE BE-CAN-POSTAL-2  TO  WS-CAN-POSTAL-2                
03945      ELSE                                                         
03946          MOVE BE-ZIP-PRIME     TO  WS-AM-ZIP-CODE                 
03947          IF BE-ZIP-PLUS4 NOT = SPACES  AND  ZEROS                 
03948              MOVE '-'          TO  WS-AM-ZIP-DASH                 
03949              MOVE BE-ZIP-PLUS4 TO  WS-AM-ZIP-PLUS4.               
03950                                                                   
03951      MOVE WS-ZIP-CODE          TO  WS-LABEL-LINES (6).            
03952                                                                   
03953      MOVE BE-PHONE-NO          TO  WS-PHONE-IN.                   
03954                                                                   
03955      GO TO 7204-SET-PHONE.                                        
03956                                                                   
03957  7202-GET-FROM-ACTIVITY.                                          
03958      EXEC CICS READ                                               
03959           DATASET  (ACTV-ID)                                      
03960           SET      (ADDRESS OF ACTIVITY-TRAILERS)                 
03961           RIDFLD   (ACTV-KEY)                                     
03962      END-EXEC.                                                    
03963                                                                   
03964      MOVE AT-MAIL-TO-NAME        TO WS-LABEL-LINES (1).           
03965      MOVE AT-ADDRESS-LINE-1      TO WS-LABEL-LINES (2).           
03966      MOVE AT-ADDRESS-LINE-2      TO WS-LABEL-LINES (3).           
03967 *    MOVE AT-CITY-STATE          TO WS-LABEL-LINES (4).           
           STRING AT-CITY ' ' AT-STATE DELIMITED BY '  '
              INTO WS-LABEL-LINES (4)
           END-STRING
03968                                                                   
03969      MOVE SPACES                 TO  WS-ZIP-CODE.                 
03970      IF AT-CANADIAN-POST-CODE                                     
03971          MOVE AT-CAN-POSTAL-1    TO  WS-CAN-POSTAL-1              
03972          MOVE AT-CAN-POSTAL-2    TO  WS-CAN-POSTAL-2              
03973      ELSE                                                         
03974          MOVE AT-ZIP-CODE        TO  WS-AM-ZIP-CODE               
03975          IF AT-ZIP-PLUS4 NOT = SPACES  AND  ZEROS                 
03976              MOVE '-'            TO  WS-AM-ZIP-DASH               
03977              MOVE AT-ZIP-PLUS4   TO  WS-AM-ZIP-PLUS4.             
03978                                                                   
03979      MOVE WS-ZIP-CODE            TO  WS-LABEL-LINES (5).          
03980                                                                   
03981      MOVE AT-PHONE-NO            TO WS-PHONE-IN.                  
03982                                                                   
03983  7204-SET-PHONE.                                                  
03984      MOVE WSPI-AREA              TO WSPO-AREA.                    
03985      MOVE WSPI-PFX               TO WSPO-PFX.                     
03986      MOVE WSPI-SFX               TO WSPO-SFX.                     
03987      MOVE WS-PHONE-OUT           TO SS44-5D.                      
03988                                                                   
03989      PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.                      
03990                                                                   
03991      MOVE WS-LABEL-LINES (1)     TO SS43D.                        
03992      MOVE WS-LABEL-LINES (2)     TO SS44-1D.                      
03993      MOVE WS-LABEL-LINES (3)     TO SS44-2D.                      
03994      MOVE WS-LABEL-LINES (4)     TO SS44-3D.                      
03995      MOVE WS-LABEL-LINES (5)     TO SS44-4D.                      
03996      MOVE WS-LABEL-LINES (6)     TO SS44-6D.                      
03997                                                                   
03998      EJECT                                                        
03999  7205-READ-PHYSICIAN-ADDR.                                        
04000      IF W-PHYSICIAN > ZERO                                        
04001          COMPUTE ACTV-SEQ = W-PHYSICIAN + 30                      
04002      ELSE                                                         
04003          MOVE CL-DOCTOR-ADDR-CNT TO ACTV-SEQ                      
04004          ADD +30                 TO ACTV-SEQ.                     
04005                                                                   
04006      IF ACTV-SEQ = +30                                            
04007         GO TO 7210-READ-EMPLOYER-ADDR.                            
04008                                                                   
04009      EXEC CICS HANDLE CONDITION                                   
04010           NOTOPEN    (8870-ACTV-NOT-OPEN)                         
04011           NOTFND     (7210-READ-EMPLOYER-ADDR)                    
04012      END-EXEC.                                                    
04013                                                                   
04014      EXEC CICS READ                                               
04015           DATASET    (ACTV-ID)                                    
04016           SET        (ADDRESS OF ACTIVITY-TRAILERS)               
04017           RIDFLD     (ACTV-KEY)                                   
04018      END-EXEC.                                                    
04019                                                                   
04020      MOVE SPACES                 TO WS-LABEL-HOLD-AREA.           
04021      MOVE AT-MAIL-TO-NAME        TO WS-LABEL-LINES (1).           
04022      MOVE AT-ADDRESS-LINE-1      TO WS-LABEL-LINES (2).           
04023      MOVE AT-ADDRESS-LINE-2      TO WS-LABEL-LINES (3).           
04024 *    MOVE AT-CITY-STATE          TO WS-LABEL-LINES (4).           
           STRING AT-CITY ' ' AT-STATE DELIMITED BY '  '
              INTO WS-LABEL-LINES (4)
           END-STRING
04025                                                                   
04026      MOVE SPACES                 TO  WS-ZIP-CODE.                 
04027      IF AT-CANADIAN-POST-CODE                                     
04028          MOVE AT-CAN-POSTAL-1    TO  WS-CAN-POSTAL-1              
04029          MOVE AT-CAN-POSTAL-2    TO  WS-CAN-POSTAL-2              
04030      ELSE                                                         
04031          MOVE AT-ZIP-CODE        TO  WS-AM-ZIP-CODE               
04032          IF AT-ZIP-PLUS4 NOT = SPACES  AND  ZEROS                 
04033              MOVE '-'            TO  WS-AM-ZIP-DASH               
04034              MOVE AT-ZIP-PLUS4   TO  WS-AM-ZIP-PLUS4.             
04035                                                                   
04036      MOVE WS-ZIP-CODE            TO  WS-LABEL-LINES (5).          
04037                                                                   
04038      MOVE AT-PHONE-NO            TO WS-PHONE-IN.                  
04039      MOVE WSPI-AREA              TO WSPO-AREA.                    
04040      MOVE WSPI-PFX               TO WSPO-PFX.                     
04041      MOVE WSPI-SFX               TO WSPO-SFX.                     
04042      MOVE WS-PHONE-OUT           TO SS47-5D.                      
04043                                                                   
04044      PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.                      
04045                                                                   
04046      MOVE WS-LABEL-LINES (1)     TO SS47D.                        
04047      MOVE WS-LABEL-LINES (2)     TO SS47-1D.                      
04048      MOVE WS-LABEL-LINES (3)     TO SS47-2D.                      
04049      MOVE WS-LABEL-LINES (4)     TO SS47-3D.                      
04050      MOVE WS-LABEL-LINES (5)     TO SS47-4D.                      
04051                                                                   
04052      EJECT                                                        
04053                                                                   
04054  7210-READ-EMPLOYER-ADDR.                                         
04055      IF W-EMPLOYER > ZERO                                         
04056          COMPUTE ACTV-SEQ = W-EMPLOYER + 40                       
04057      ELSE                                                         
04058          MOVE CL-EMPLOYER-ADDR-CNT                                
04059                                  TO ACTV-SEQ                      
04060          ADD +40                 TO ACTV-SEQ.                     
04061                                                                   
04062      IF ACTV-SEQ = +40                                            
04063         GO TO 7220-READ-INSURED-ADDR.                             
04064                                                                   
04065      EXEC CICS HANDLE CONDITION                                   
04066           NOTOPEN    (8870-ACTV-NOT-OPEN)                         
04067           NOTFND     (7220-READ-INSURED-ADDR)                     
04068      END-EXEC.                                                    
04069                                                                   
04070      EXEC CICS READ                                               
04071           DATASET    (ACTV-ID)                                    
04072           SET        (ADDRESS OF ACTIVITY-TRAILERS)               
04073           RIDFLD     (ACTV-KEY)                                   
04074      END-EXEC.                                                    
04075                                                                   
04076      MOVE SPACES                 TO WS-LABEL-HOLD-AREA.           
04077      MOVE AT-MAIL-TO-NAME        TO WS-LABEL-LINES (1).           
04078      MOVE AT-ADDRESS-LINE-1      TO WS-LABEL-LINES (2).           
04079      MOVE AT-ADDRESS-LINE-2      TO WS-LABEL-LINES (3).           
04080 *    MOVE AT-CITY-STATE          TO WS-LABEL-LINES (4).           
           STRING AT-CITY ' ' AT-STATE DELIMITED BY '  '
              INTO WS-LABEL-LINES (4)
           END-STRING
04081                                                                   
04082      MOVE SPACES                 TO  WS-ZIP-CODE.                 
04083      IF AT-CANADIAN-POST-CODE                                     
04084          MOVE AT-CAN-POSTAL-1    TO  WS-CAN-POSTAL-1              
04085          MOVE AT-CAN-POSTAL-2    TO  WS-CAN-POSTAL-2              
04086      ELSE                                                         
04087          MOVE AT-ZIP-CODE        TO  WS-AM-ZIP-CODE               
04088          IF AT-ZIP-PLUS4 NOT = SPACES  AND  ZEROS                 
04089              MOVE '-'            TO  WS-AM-ZIP-DASH               
04090              MOVE AT-ZIP-PLUS4   TO  WS-AM-ZIP-PLUS4.             
04091                                                                   
04092      MOVE WS-ZIP-CODE            TO  WS-LABEL-LINES (5).          
04093                                                                   
04094      MOVE AT-PHONE-NO            TO WS-PHONE-IN.                  
04095      MOVE WSPI-AREA              TO WSPO-AREA.                    
04096      MOVE WSPI-PFX               TO WSPO-PFX.                     
04097      MOVE WSPI-SFX               TO WSPO-SFX.                     
04098      MOVE WS-PHONE-OUT           TO SS48-5D.                      
04099                                                                   
04100      PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.                      
04101                                                                   
04102      MOVE WS-LABEL-LINES (1)     TO SS48D.                        
04103      MOVE WS-LABEL-LINES (2)     TO SS48-1D.                      
04104      MOVE WS-LABEL-LINES (3)     TO SS48-2D.                      
04105      MOVE WS-LABEL-LINES (4)     TO SS48-3D.                      
04106      MOVE WS-LABEL-LINES (5)     TO SS48-4D.                      
04107                                                                   
04108      EJECT                                                        
04109  7220-READ-INSURED-ADDR.                                          
04110                                                                   
04111      IF W-INSURED > ZERO                                          
04112          COMPUTE ACTV-SEQ = W-INSURED                             
04113      ELSE                                                         
04114          MOVE CL-INSURED-ADDR-CNT TO ACTV-SEQ.                    
04115                                                                   
04116      IF ACTV-SEQ = +0                                             
04117         GO TO 7225-READ-OTHER1-ADDR.                              
04118                                                                   
04119      EXEC CICS HANDLE CONDITION                                   
04120           NOTOPEN   (8870-ACTV-NOT-OPEN)                          
04121           NOTFND    (7225-READ-OTHER1-ADDR)                       
04122      END-EXEC.                                                    
04123                                                                   
04124      EXEC CICS READ                                               
04125           DATASET   (ACTV-ID)                                     
04126           SET       (ADDRESS OF ACTIVITY-TRAILERS)                
04127           RIDFLD    (ACTV-KEY)                                    
04128      END-EXEC.                                                    
04129                                                                   
04130      MOVE SPACES                 TO WS-LABEL-HOLD-AREA.           
04131      MOVE SS10D                  TO WS-LABEL-LINES (1).           
04132      MOVE AT-ADDRESS-LINE-1      TO WS-LABEL-LINES (2).           
04133      MOVE AT-ADDRESS-LINE-2      TO WS-LABEL-LINES (3).           
04134 *    MOVE AT-CITY-STATE          TO WS-LABEL-LINES (4).           
           STRING AT-CITY ' ' AT-STATE DELIMITED BY '  '
              INTO WS-LABEL-LINES (4)
           END-STRING
04135                                                                   
04136      MOVE SPACES                 TO  WS-ZIP-CODE.                 
04137      IF AT-CANADIAN-POST-CODE                                     
04138          MOVE AT-CAN-POSTAL-1    TO  WS-CAN-POSTAL-1              
04139          MOVE AT-CAN-POSTAL-2    TO  WS-CAN-POSTAL-2              
04140      ELSE                                                         
04141          MOVE AT-ZIP-CODE        TO  WS-AM-ZIP-CODE               
04142          IF AT-ZIP-PLUS4 NOT = SPACES  AND  ZEROS                 
04143              MOVE '-'            TO  WS-AM-ZIP-DASH               
04144              MOVE AT-ZIP-PLUS4   TO  WS-AM-ZIP-PLUS4.             
04145                                                                   
04146      MOVE WS-ZIP-CODE            TO  WS-LABEL-LINES (5).          
04147                                                                   
04148      PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.                      
04149                                                                   
04150      MOVE WS-LABEL-LINES (1)     TO SS10D.                        
04151      MOVE WS-LABEL-LINES (2)     TO SS11-1D.                      
04152      MOVE WS-LABEL-LINES (3)     TO SS11-2D.                      
04153      MOVE WS-LABEL-LINES (4)     TO SS11-3D.                      
04154      MOVE WS-LABEL-LINES (5)     TO SS11-4D.                      
04155      MOVE AT-MAIL-TO-NAME        TO SS11-5D.                      
04156                                                                   
04157      MOVE AT-PHONE-NO            TO WS-PHONE-IN.                  
04158      MOVE WSPI-AREA              TO WSPO-AREA.                    
04159      MOVE WSPI-PFX               TO WSPO-PFX.                     
04160      MOVE WSPI-SFX               TO WSPO-SFX.                     
04161      MOVE WS-PHONE-OUT           TO SS11-6D.                      
04162                                                                   
04163      EJECT                                                        
04164                                                                   
04165  7225-READ-OTHER1-ADDR.                                           
04166                                                                   
04167      IF W-OTHER-1 > ZERO                                          
04168          COMPUTE ACTV-SEQ = W-OTHER-1 + 50                        
04169      ELSE                                                         
04170          MOVE CL-OTHER-1-ADDR-CNT TO ACTV-SEQ                     
04171          ADD +50                  TO ACTV-SEQ.                    
04172                                                                   
04173      IF ACTV-SEQ = +50                                            
04174         GO TO 7230-READ-OTHER2-ADDR.                              
04175                                                                   
04176      EXEC CICS HANDLE CONDITION                                   
04177           NOTOPEN    (8870-ACTV-NOT-OPEN)                         
04178           NOTFND     (7230-READ-OTHER2-ADDR)                      
04179      END-EXEC.                                                    
04180                                                                   
04181      EXEC CICS READ                                               
04182           DATASET    (ACTV-ID)                                    
04183           SET        (ADDRESS OF ACTIVITY-TRAILERS)               
04184           RIDFLD     (ACTV-KEY)                                   
04185      END-EXEC.                                                    
04186                                                                   
04187      MOVE SPACES                 TO WS-LABEL-HOLD-AREA.           
04188      MOVE AT-MAIL-TO-NAME        TO WS-LABEL-LINES (1).           
04189      MOVE AT-ADDRESS-LINE-1      TO WS-LABEL-LINES (2).           
04190      MOVE AT-ADDRESS-LINE-2      TO WS-LABEL-LINES (3).           
04191 *    MOVE AT-CITY-STATE          TO WS-LABEL-LINES (4).           
           STRING AT-CITY ' ' AT-STATE DELIMITED BY '  '
              INTO WS-LABEL-LINES (4)
           END-STRING
04192                                                                   
04193      MOVE SPACES                 TO  WS-ZIP-CODE.                 
04194      IF AT-CANADIAN-POST-CODE                                     
04195          MOVE AT-CAN-POSTAL-1    TO  WS-CAN-POSTAL-1              
04196          MOVE AT-CAN-POSTAL-2    TO  WS-CAN-POSTAL-2              
04197      ELSE                                                         
04198          MOVE AT-ZIP-CODE        TO  WS-AM-ZIP-CODE               
04199          IF AT-ZIP-PLUS4 NOT = SPACES  AND  ZEROS                 
04200              MOVE '-'            TO  WS-AM-ZIP-DASH               
04201              MOVE AT-ZIP-PLUS4   TO  WS-AM-ZIP-PLUS4.             
04202                                                                   
04203      MOVE WS-ZIP-CODE            TO  WS-LABEL-LINES (5).          
04204                                                                   
04205      MOVE AT-PHONE-NO            TO WS-PHONE-IN.                  
04206      MOVE WSPI-AREA              TO WSPO-AREA.                    
04207      MOVE WSPI-PFX               TO WSPO-PFX.                     
04208      MOVE WSPI-SFX               TO WSPO-SFX.                     
04209      MOVE WS-PHONE-OUT           TO SS49-5D.                      
04210                                                                   
04211      PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.                      
04212                                                                   
04213      MOVE WS-LABEL-LINES (1)     TO SS49D.                        
04214      MOVE WS-LABEL-LINES (2)     TO SS49-1D.                      
04215      MOVE WS-LABEL-LINES (3)     TO SS49-2D.                      
04216      MOVE WS-LABEL-LINES (4)     TO SS49-3D.                      
04217      MOVE WS-LABEL-LINES (5)     TO SS49-4D.                      
04218                                                                   
04219      EJECT                                                        
04220  7230-READ-OTHER2-ADDR.                                           
04221                                                                   
04222      IF W-OTHER-2 > ZERO                                          
04223          COMPUTE ACTV-SEQ = W-OTHER-2 + 60                        
04224      ELSE                                                         
04225          MOVE CL-OTHER-2-ADDR-CNT TO ACTV-SEQ                     
04226          ADD +60                  TO ACTV-SEQ.                    
04227                                                                   
04228      IF ACTV-SEQ = +60                                            
04229         GO TO 7240-NOT-FOUND.                                     
04230                                                                   
04231      EXEC CICS HANDLE CONDITION                                   
04232           NOTOPEN    (8870-ACTV-NOT-OPEN)                         
04233           NOTFND     (7240-NOT-FOUND)                             
04234      END-EXEC.                                                    
04235                                                                   
04236      EXEC CICS READ                                               
04237           DATASET    (ACTV-ID)                                    
04238           SET        (ADDRESS OF ACTIVITY-TRAILERS)               
04239           RIDFLD     (ACTV-KEY)                                   
04240      END-EXEC.                                                    
04241                                                                   
04242      MOVE SPACES                 TO WS-LABEL-HOLD-AREA.           
04243      MOVE AT-MAIL-TO-NAME        TO WS-LABEL-LINES (1).           
04244      MOVE AT-ADDRESS-LINE-1      TO WS-LABEL-LINES (2).           
04245      MOVE AT-ADDRESS-LINE-2      TO WS-LABEL-LINES (3).           
04246 *    MOVE AT-CITY-STATE          TO WS-LABEL-LINES (4).           
           STRING AT-CITY ' ' AT-STATE DELIMITED BY '  '
              INTO WS-LABEL-LINES (4)
           END-STRING
04247                                                                   
04248      MOVE SPACES                 TO  WS-ZIP-CODE.                 
04249      IF AT-CANADIAN-POST-CODE                                     
04250          MOVE AT-CAN-POSTAL-1    TO  WS-CAN-POSTAL-1              
04251          MOVE AT-CAN-POSTAL-2    TO  WS-CAN-POSTAL-2              
04252      ELSE                                                         
04253          MOVE AT-ZIP-CODE        TO  WS-AM-ZIP-CODE               
04254          IF AT-ZIP-PLUS4 NOT = SPACES  AND  ZEROS                 
04255              MOVE '-'            TO  WS-AM-ZIP-DASH               
04256              MOVE AT-ZIP-PLUS4   TO  WS-AM-ZIP-PLUS4.             
04257                                                                   
04258      MOVE WS-ZIP-CODE            TO  WS-LABEL-LINES (5).          
04259                                                                   
04260      MOVE AT-PHONE-NO            TO WS-PHONE-IN.                  
04261      MOVE WSPI-AREA              TO WSPO-AREA.                    
04262      MOVE WSPI-PFX               TO WSPO-PFX.                     
04263      MOVE WSPI-SFX               TO WSPO-SFX.                     
04264      MOVE WS-PHONE-OUT           TO SS50-5D.                      
04265                                                                   
04266      PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.                      
04267                                                                   
04268      MOVE WS-LABEL-LINES (1)     TO SS50D.                        
04269      MOVE WS-LABEL-LINES (2)     TO SS50-1D.                      
04270      MOVE WS-LABEL-LINES (3)     TO SS50-2D.                      
04271      MOVE WS-LABEL-LINES (4)     TO SS50-3D.                      
04272      MOVE WS-LABEL-LINES (5)     TO SS50-4D.                      
04273                                                                   
04274      EJECT                                                        
04275  7240-NOT-FOUND.                                                  
04276      IF ACCOUNT-IS-ONLINE                                         
04277         GO TO 7250-READ-ACCOUNT.                                  
04278                                                                   
04279      IF W-ACCOUNT > ZERO                                          
04280          COMPUTE ACTV-SEQ = W-ACCOUNT + 20                        
04281      ELSE                                                         
04282          MOVE CL-ACCOUNT-ADDR-CNT TO ACTV-SEQ                     
04283          ADD +20                  TO ACTV-SEQ.                    
04284                                                                   
04285      IF ACTV-SEQ = +20                                            
04286         GO TO 7250-READ-ACCOUNT.                                  
04287                                                                   
04288      EXEC CICS HANDLE CONDITION                                   
04289           NOTOPEN    (8870-ACTV-NOT-OPEN)                         
04290           NOTFND     (7250-READ-ACCOUNT)                          
04291      END-EXEC.                                                    
04292                                                                   
04293      EXEC CICS READ                                               
04294           DATASET    (ACTV-ID)                                    
04295           SET        (ADDRESS OF ACTIVITY-TRAILERS)               
04296           RIDFLD     (ACTV-KEY)                                   
04297      END-EXEC.                                                    
04298                                                                   
04299      MOVE SPACES                 TO WS-LABEL-HOLD-AREA.           
04300      MOVE AT-MAIL-TO-NAME        TO WS-LABEL-LINES (1).           
04301      MOVE AT-ADDRESS-LINE-1      TO WS-LABEL-LINES (2).           
04302      MOVE AT-ADDRESS-LINE-2      TO WS-LABEL-LINES (3).           
04303 *    MOVE AT-CITY-STATE          TO WS-LABEL-LINES (4).           
           STRING AT-CITY ' ' AT-STATE DELIMITED BY '  '
              INTO WS-LABEL-LINES (4)
           END-STRING
04304                                                                   
04305      MOVE SPACES                 TO  WS-ZIP-CODE.                 
04306      IF AT-CANADIAN-POST-CODE                                     
04307          MOVE AT-CAN-POSTAL-1    TO  WS-CAN-POSTAL-1              
04308          MOVE AT-CAN-POSTAL-2    TO  WS-CAN-POSTAL-2              
04309      ELSE                                                         
04310          MOVE AT-ZIP-CODE        TO  WS-AM-ZIP-CODE               
04311          IF AT-ZIP-PLUS4 NOT = SPACES  AND  ZEROS                 
04312              MOVE '-'            TO  WS-AM-ZIP-DASH               
04313              MOVE AT-ZIP-PLUS4   TO  WS-AM-ZIP-PLUS4.             
04314                                                                   
04315      MOVE WS-ZIP-CODE            TO  WS-LABEL-LINES (5).          
04316                                                                   
04317      MOVE AT-PHONE-NO            TO WS-PHONE-IN.                  
04318      MOVE WSPI-AREA              TO WSPO-AREA.                    
04319      MOVE WSPI-PFX               TO WSPO-PFX.                     
04320      MOVE WSPI-SFX               TO WSPO-SFX.                     
04321      MOVE WS-PHONE-OUT           TO SS07-6D.                      
04322                                                                   
121802*    IF PI-COMPANY-ID NOT = 'FLA'                                 
121802*        PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.                  
04325                                                                   
04326      MOVE WS-LABEL-LINES (1)     TO SS06D.                        
04327      MOVE WS-LABEL-LINES (2)     TO SS07-1D.                      
04328      MOVE WS-LABEL-LINES (3)     TO SS07-2D.                      
04329      MOVE WS-LABEL-LINES (4)     TO SS07-3D.                      
04330      MOVE WS-LABEL-LINES (5)     TO SS07-4D.                      
04331      MOVE WS-LABEL-LINES (6)     TO SS07-5D.                      
04332                                                                   
04333      EJECT                                                        
04334  7250-READ-ACCOUNT.                                               
121802*    IF CL-SYSTEM-IDENTIFIER = 'CV'                               
121802*        GO TO 7250-READ-PRODUCER.                                
04337                                                                   
04338      IF WS-ACCT-READ-SW = 'Y'                                     
04339         GO TO 7250-BUILD-ACCT-ADDR.                               
04340                                                                   
04341      MOVE CM-CERT-EFF-DT TO ACCT-EXP-DATE.                        
04342                                                                   
04343      EXEC CICS HANDLE CONDITION                                   
04344           NOTOPEN   (8880-ACCT-NOT-OPEN)                          
04345           NOTFND    (7251-READ-3RD-PARTY)                         
04346      END-EXEC.                                                    
04347                                                                   
04348  7250-STARTBR-ACCOUNT.                                            
04349      MOVE ACCT-PARTIAL-KEY       TO  ACCT-SAVE-KEY.               
04350      PERFORM 8000-STARTBR-ERACCT THRU 8000-STARTBR-EXIT.          
04351                                                                   
04352  7250-READNEXT-ACCOUNT.                                           
04353      PERFORM 8000-READNEXT-ERACCT THRU 8000-READNEXT-EXIT.        
04354                                                                   
04355      IF ACCT-PARTIAL-KEY NOT = ACCT-SAVE-KEY                      
04356          IF WS-SAVE-ACCT-RECORD = SPACES                          
04357              GO TO 7251-READ-3RD-PARTY                            
04358          ELSE                                                     
04359              MOVE AM-CONTROL-PRIMARY  TO  ACCT-KEY                
04360              MOVE WS-SAVE-ACCT-RECORD TO  ACCOUNT-MASTER          
04361              GO TO 7250-BUILD-ACCT-ADDR.                          
04362                                                                   
04363      IF AM-EXPIRATION-DT = HIGH-VALUES                            
04364          MOVE AM-CONTROL-PRIMARY      TO  ACCT-KEY                
04365      ELSE                                                         
04366          MOVE ACCOUNT-MASTER          TO  WS-SAVE-ACCT-RECORD     
04367          GO TO 7250-READNEXT-ACCOUNT.                             
04368                                                                   
04369  7250-BUILD-ACCT-ADDR.                                            
04370      MOVE SPACES                TO  WS-SAVE-ACCT-RECORD.          
04371                                                                   
04372      IF NOT ACCOUNT-IS-ONLINE                                     
04373         GO TO 7251-READ-3RD-PARTY.                                
04374                                                                   
04375      MOVE SPACES                TO WS-LABEL-HOLD-AREA.            
04376      MOVE AM-NAME               TO WS-LABEL-LINES (1).            
04377      MOVE AM-PERSON             TO WS-LABEL-LINES (2).            
04378      MOVE AM-ADDRS              TO WS-LABEL-LINES (3).            
04379 *    MOVE AM-CITY               TO WS-LABEL-LINES (4).            
           STRING AM-ADDR-CITY ' ' AM-ADDR-STATE DELIMITED BY '  '
              INTO WS-LABEL-LINES (4)
           END-STRING
04380                                                                   
04381      MOVE SPACES                TO  WS-ZIP-CODE.                  
04382      IF AM-CANADIAN-POST-CODE                                     
04383          MOVE AM-CAN-POSTAL-1   TO  WS-CAN-POSTAL-1               
04384          MOVE AM-CAN-POSTAL-2   TO  WS-CAN-POSTAL-2               
04385      ELSE                                                         
04386          MOVE AM-ZIP-PRIME      TO  WS-AM-ZIP-CODE                
04387          IF AM-ZIP-PLUS4 NOT = SPACES  AND  ZEROS                 
04388              MOVE '-'           TO  WS-AM-ZIP-DASH                
04389              MOVE AM-ZIP-PLUS4  TO  WS-AM-ZIP-PLUS4.              
04390                                                                   
04391      MOVE WS-ZIP-CODE           TO  WS-LABEL-LINES (5).           
04392                                                                   
04393      MOVE ZEROS                 TO WS-PHONE-IN.                   
04394      MOVE AM-AREA-CODE          TO WSPO-AREA.                     
04395      MOVE AM-TEL-PRE            TO WSPO-PFX.                      
04396      MOVE AM-TEL-NBR            TO WSPO-SFX.                      
04397      MOVE WS-PHONE-OUT          TO SS07-6D.                       
04398                                                                   
121802*    IF PI-COMPANY-ID NOT = 'FLA'                                 
121802*        PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.                  
04401                                                                   
04402      MOVE WS-LABEL-LINES (1)     TO SS06D.                        
04403      MOVE WS-LABEL-LINES (2)     TO SS07-1D.                      
04404      MOVE WS-LABEL-LINES (3)     TO SS07-2D.                      
04405      MOVE WS-LABEL-LINES (4)     TO SS07-3D.                      
04406      MOVE WS-LABEL-LINES (5)     TO SS07-4D.                      
04407      MOVE WS-LABEL-LINES (6)     TO SS07-5D.                      
04408                                                                   
121802*    GO TO 7251-READ-3RD-PARTY.                                   
04410                                                                   
04411      EJECT                                                        
121802*7250-READ-PRODUCER. Remove as dead code                          
121802*7250-STARTBR-PRODUCER. Remove as dead code                       
121802*7250-READNEXT-PRODUCER. Remove as dead code                      
121802*7250-BUILD-PROD-ADDR. Remove as dead code                        

04488  7251-READ-3RD-PARTY.                                             
04489                                                                   
04490      MOVE +29                    TO ACTV-SEQ.                     
04491                                                                   
04492      EXEC CICS HANDLE CONDITION                                   
04493           NOTOPEN    (8870-ACTV-NOT-OPEN)                         
04494           NOTFND     (7252-READ-COMP)                             
04495      END-EXEC.                                                    
04496                                                                   
04497      EXEC CICS READ                                               
04498           DATASET    (ACTV-ID)                                    
04499           SET        (ADDRESS OF ACTIVITY-TRAILERS)               
04500           RIDFLD     (ACTV-KEY)                                   
04501      END-EXEC.                                                    
04502                                                                   
04503      MOVE SPACES                 TO WS-LABEL-HOLD-AREA.           
04504      MOVE AT-MAIL-TO-NAME        TO WS-LABEL-LINES (1).           
04505      MOVE AT-ADDRESS-LINE-1      TO WS-LABEL-LINES (2).           
04506      MOVE AT-ADDRESS-LINE-2      TO WS-LABEL-LINES (3).           
04507 *    MOVE AT-CITY-STATE          TO WS-LABEL-LINES (4).           
           STRING AT-CITY ' ' AT-STATE DELIMITED BY '  '
              INTO WS-LABEL-LINES (4)
           END-STRING
04508                                                                   
04509      MOVE SPACES                 TO  WS-ZIP-CODE.                 
04510      IF AT-CANADIAN-POST-CODE                                     
04511          MOVE AT-CAN-POSTAL-1    TO  WS-CAN-POSTAL-1              
04512          MOVE AT-CAN-POSTAL-2    TO  WS-CAN-POSTAL-2              
04513      ELSE                                                         
04514          MOVE AT-ZIP-CODE        TO  WS-AM-ZIP-CODE               
04515          IF AT-ZIP-PLUS4 NOT = SPACES  AND  ZEROS                 
04516              MOVE '-'            TO  WS-AM-ZIP-DASH               
04517              MOVE AT-ZIP-PLUS4   TO  WS-AM-ZIP-PLUS4.             
04518                                                                   
04519      MOVE WS-ZIP-CODE            TO  WS-LABEL-LINES (5).          
04520                                                                   
04521      MOVE AT-PHONE-NO            TO WS-PHONE-IN.                  
04522      MOVE WSPI-AREA              TO WSPO-AREA.                    
04523      MOVE WSPI-PFX               TO WSPO-PFX.                     
04524      MOVE WSPI-SFX               TO WSPO-SFX.                     
04525      MOVE WS-PHONE-OUT           TO SS53-6D.                      
04526                                                                   
04527      PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.                      
04528                                                                   
04529      MOVE WS-LABEL-LINES (1)     TO SS52D.                        
04530      MOVE WS-LABEL-LINES (2)     TO SS53-1D.                      
04531      MOVE WS-LABEL-LINES (3)     TO SS53-2D.                      
04532      MOVE WS-LABEL-LINES (4)     TO SS53-3D.                      
04533      MOVE WS-LABEL-LINES (5)     TO SS53-4D.                      
04534      MOVE WS-LABEL-LINES (6)     TO SS53-5D.                      
04535                                                                   
04536      GO TO 7260-READ-DENIAL.                                      
04537      EJECT                                                        
04538  7252-READ-COMP.                                                  
04539                                                                   
04540      IF WS-COMP-READ-SW = 'Y'                                     
04541         GO TO 7255-BUILD-COMP-ADDR.                               
04542                                                                   
04543      IF AM-3RD-PARTY-NOTIF-LEVEL NOT NUMERIC                      
04544         MOVE ZEROS         TO AM-3RD-PARTY-NOTIF-LEVEL            
04545         GO TO 7260-READ-DENIAL.                                   
04546                                                                   
04547      IF AM-3RD-PARTY-NOTIF-LEVEL > 00 AND < 11                    
04548         NEXT SENTENCE                                             
04549      ELSE                                                         
04550         GO TO 7260-READ-DENIAL.                                   
04551                                                                   
04552      IF AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL) = SPACES OR ZEROS       
04553         GO TO 7260-READ-DENIAL.                                   
04554                                                                   
04555      MOVE PI-COMPANY-CD   TO WS-ERCOMP-COMPANY-CD.                
04556      MOVE AM-CARRIER      TO WS-ERCOMP-CARRIER.                   
04557      MOVE AM-GROUPING     TO WS-ERCOMP-GROUPING.                  
04558      MOVE 'A'             TO WS-ERCOMP-TYPE.                      
04559      MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)                       
04560                           TO WS-ERCOMP-RESP-NO.                   
04561                                                                   
04562      IF AM-3RD-PARTY-NOTIF-LEVEL = AM-REMIT-TO                    
04563          IF AM-COM-TYP (AM-REMIT-TO) = 'O' OR 'P' OR              
04564                                        'G' OR 'B' or 'S'          
04565              MOVE 'G'            TO WS-ERCOMP-TYPE                
04566              MOVE LOW-VALUES     TO WS-ERCOMP-ACCOUNT             
04567          ELSE                                                     
04568              MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)               
04569                                  TO WS-ERCOMP-ACCOUNT             
04570      ELSE                                                         
04571          MOVE 'G'                TO WS-ERCOMP-TYPE                
04572          MOVE LOW-VALUES         TO WS-ERCOMP-ACCOUNT.            
04573                                                                   
04574      IF PI-ZERO-CARRIER OR PI-ZERO-CAR-GROUP                      
04575         MOVE ZEROS TO WS-ERCOMP-CARRIER.                          
04576                                                                   
04577      IF PI-ZERO-GROUPING OR PI-ZERO-CAR-GROUP                     
04578         MOVE ZEROS TO WS-ERCOMP-GROUPING.                         
04579                                                                   
04580      EXEC CICS HANDLE CONDITION                                   
04581           NOTFND    (7260-READ-DENIAL)                            
04582      END-EXEC.                                                    
04583                                                                   
04584      EXEC CICS  READ                                              
04585           SET      (ADDRESS OF COMPENSATION-MASTER)               
04586           DATASET  ('ERCOMP')                                     
04587           RIDFLD   (WS-ERCOMP-KEY)                                
04588      END-EXEC.                                                    
04589                                                                   
04590  7255-BUILD-COMP-ADDR.                                            
04591                                                                   
04592      MOVE SPACES                TO WS-LABEL-HOLD-AREA.            
04593      MOVE CO-ACCT-NAME          TO WS-LABEL-LINES (1).            
04594      IF CO-ACCT-NAME = SPACES                                     
04595         MOVE CO-MAIL-NAME       TO WS-LABEL-LINES (1).            
04596      MOVE CO-ADDR-1             TO WS-LABEL-LINES (2).            
04597      MOVE CO-ADDR-2             TO WS-LABEL-LINES (3).            
04598      MOVE CO-ADDR-3             TO WS-LABEL-LINES (4).            
04599                                                                   
04600      MOVE SPACES                TO  WS-ZIP-CODE.                  
04601      IF CO-CANADIAN-POST-CODE                                     
04602          MOVE CO-CAN-POSTAL-1   TO  WS-CAN-POSTAL-1               
04603          MOVE CO-CAN-POSTAL-2   TO  WS-CAN-POSTAL-2               
04604      ELSE                                                         
04605          MOVE CO-ZIP-PRIME      TO  WS-AM-ZIP-CODE                
04606          IF CO-ZIP-PLUS4 NOT = SPACES  AND  ZEROS                 
04607              MOVE '-'           TO  WS-AM-ZIP-DASH                
04608              MOVE CO-ZIP-PLUS4  TO  WS-AM-ZIP-PLUS4.              
04609                                                                   
04610      MOVE WS-ZIP-CODE           TO  WS-LABEL-LINES (5).           
04611                                                                   
04612      MOVE ZEROS                 TO WS-PHONE-IN.                   
04613      MOVE CO-AREA-CODE          TO WSPO-AREA.                     
04614      MOVE CO-PREFIX             TO WSPO-PFX.                      
04615      MOVE CO-PHONE              TO WSPO-SFX.                      
04616      MOVE WS-PHONE-OUT          TO SS53-6D.                       
04617                                                                   
04618      PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.                      
04619                                                                   
04620      MOVE WS-LABEL-LINES (1)     TO SS52D.                        
04621      MOVE WS-LABEL-LINES (2)     TO SS53-1D.                      
04622      MOVE WS-LABEL-LINES (3)     TO SS53-2D.                      
04623      MOVE WS-LABEL-LINES (4)     TO SS53-3D.                      
04624      MOVE WS-LABEL-LINES (5)     TO SS53-4D.                      
04625      MOVE WS-LABEL-LINES (6)     TO SS53-5D.                      
04626                                                                   
04627      EJECT                                                        
04628                                                                   
04629  7260-READ-DENIAL.                                                
04630                                                                   
04631      MOVE +93                    TO ACTV-SEQ
04632                                                                   
04633      EXEC CICS HANDLE CONDITION                                   
04634           NOTOPEN    (8870-ACTV-NOT-OPEN)                         
04635           NOTFND     (7265-READ-CNTL1)                            
04636           ENDFILE    (7265-READ-CNTL1)                            
04637      END-EXEC.                                                    
04638                                                                   
04639      EXEC CICS STARTBR                                            
04640           DATASET    (ACTV-ID)                                    
04641           RIDFLD     (ACTV-KEY)                                   
04642           GTEQ                                                    
04643      END-EXEC.                                                    
04644                                                                   
04645      MOVE 'Y'                    TO ACTV-BROWSE-STARTED.          
04646      MOVE ACTV-PARTIAL-KEY       TO ACTV-SAVE-KEY.                
04647                                                                   
04648  7262-READ-NEXT.                                                  
04649                                                                   
04650      EXEC CICS READNEXT                                           
04651           SET     (ADDRESS OF ACTIVITY-TRAILERS)                  
04652           RIDFLD  (ACTV-KEY)                                      
04653           DATASET (ACTV-ID)                                       
04654      END-EXEC.                                                    
04655                                                                   
04656      IF ACTV-PARTIAL-KEY  NOT = ACTV-SAVE-KEY                     
04657         GO TO 7265-READ-CNTL1.                                    

060109     IF AT-TRAILER-TYPE = '2'
060109        AND AT-PAYMENT-TYPE = 'I'
060109        AND AT-INT-RATE NUMERIC
060109        AND AT-INT-RATE NOT = ZEROS
060109        COMPUTE WS-WORK-INT-RATE = AT-INT-RATE * 100
060109        MOVE +1                  TO S2
060109        PERFORM VARYING S1 FROM +1 BY +1 UNTIL
060109           (S1 > +6)
060109           IF WS-WORK-INT (S1:1) NOT = '0'
060109              MOVE WS-WORK-INT (S1:1)
060109                                 TO SS16-1D (S2:1)
060109              ADD +1             TO S2
060109           END-IF
060109        END-PERFORM
060109        MOVE '%'                 TO SS16-1D (S2:1)
060109        GO TO 7262-READ-NEXT
060109     END-IF

04659      IF AT-TRAILER-TYPE = '8'                                 
04662         MOVE AT-DENIAL-INFO-1    TO SS35-1D
04663         MOVE AT-DENIAL-INFO-2    TO SS35-2D
060109        IF (CL-TOTAL-INT-PAID = ZEROS)
060109           OR (CL-CLAIM-TYPE NOT = (PI-LIFE-OVERRIDE-L1 OR 'O'))
060109           GO TO 7265-READ-CNTL1
              END-IF
           END-IF

           GO TO 7262-READ-NEXT

           .
04666  7265-READ-CNTL1.                                                 
04667      IF ACTV-BROWSE-STARTED = 'Y'                                 
04668         MOVE 'N'                  TO ACTV-BROWSE-STARTED          
04669         EXEC CICS ENDBR                                           
04670              DATASET    (ACTV-ID)                                 
04671         END-EXEC.                                                 
04672                                                                   
04673      IF SS35-1D = ALL '*'                                         
04674          MOVE '@@DENIAL1'        TO SS35-1D.                      
04675                                                                   
04676      IF SS35-2D = ALL '*'                                         
04677          MOVE '@@DENIAL2'        TO SS35-2D.                      
04678                                                                   
04679      MOVE '1'                    TO CNTL-RECORD-TYPE.             
04680      MOVE ZEROS                  TO CNTL-SEQ.                     
04681      EXEC CICS HANDLE CONDITION                                   
04682           NOTOPEN   (8840-CNTL-NOT-OPEN)                          
04683           NOTFND    (7266-READ-CNTL2)                             
04684      END-EXEC.                                                    
04685                                                                   
04686      EXEC CICS READ                                               
04687           DATASET   (CNTL-ID)                                     
04688           SET       (ADDRESS OF CONTROL-FILE)                     
04689           RIDFLD    (CNTL-KEY)                                    
04690      END-EXEC.                                                    
04691                                                                   
04692      MOVE SPACES                 TO WS-LABEL-HOLD-AREA.           
04693      MOVE CF-CL-MAIL-TO-NAME     TO WS-LABEL-LINES (1).           
04694      MOVE CF-CL-IN-CARE-OF       TO WS-LABEL-LINES (2).           
04695      MOVE CF-CL-ADDR-LINE-1      TO WS-LABEL-LINES (3).           
04696      MOVE CF-CL-ADDR-LINE-2      TO WS-LABEL-LINES (4).           
04697      MOVE CF-CL-CITY-STATE       TO WS-LABEL-LINES (5).           
04698                                                                   
04699      IF CF-CL-ZIP-CODE-NUM NOT NUMERIC                            
04700          MOVE ZEROS              TO  CF-CL-ZIP-CODE-NUM.          
04701      IF CF-CL-ZIP-CODE-NUM NOT = ZEROS                            
04702          MOVE CF-CL-ZIP-CODE-NUM TO  WS-ZIP-NUMERIC               
04703          MOVE WS-ZIP-NONNUM      TO  CF-CL-ZIP-CODE.              
04704                                                                   
04705      MOVE SPACES                 TO  WS-ZIP-CODE.                 
04706      IF CF-CL-CAN-POST-CODE                                       
04707          MOVE CF-CL-CAN-POSTAL-1   TO  WS-CAN-POSTAL-1            
04708          MOVE CF-CL-CAN-POSTAL-2   TO  WS-CAN-POSTAL-2            
04709      ELSE                                                         
04710          MOVE CF-CL-ZIP-PRIME      TO  WS-AM-ZIP-CODE             
04711          IF CF-CL-ZIP-PLUS4 NOT = SPACES  AND  ZEROS              
04712              MOVE '-'              TO  WS-AM-ZIP-DASH             
04713              MOVE CF-CL-ZIP-PLUS4  TO  WS-AM-ZIP-PLUS4.           
04714                                                                   
04715      MOVE WS-ZIP-CODE            TO  WS-LABEL-LINES (6).          
04716                                                                   
04717      PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.                      
04718                                                                   
04719      MOVE WS-LABEL-LINES (1)     TO SS01D.                        
04720      MOVE WS-LABEL-LINES (2)     TO SS02-1D.                      
04721      MOVE WS-LABEL-LINES (3)     TO SS02-2D.                      
04722      MOVE WS-LABEL-LINES (4)     TO SS02-3D.                      
04723      MOVE WS-LABEL-LINES (5)     TO SS02-4D.                      
04724      MOVE WS-LABEL-LINES (6)     TO SS02-5D.                      
04725                                                                   
04726  7266-READ-CNTL2.                                                 
04727      IF PI-PROCESSOR-ID  =  LGXX-ID                               
04728         GO TO 7267-READ-CNTL4.                                    
04729                                                                   
04730      MOVE '2'                    TO CNTL-RECORD-TYPE.             
04731      MOVE PI-PROCESSOR-ID        TO CNTL-GENL.                    
04732      MOVE ZEROS                  TO CNTL-SEQ.                     
04733                                                                   
04734      EXEC CICS HANDLE CONDITION                                   
04735           NOTOPEN    (8840-CNTL-NOT-OPEN)                         
04736           NOTFND     (7267-READ-CNTL4)                            
04737      END-EXEC.                                                    
04738                                                                   
04739      EXEC CICS READ                                               
04740           DATASET    (CNTL-ID)                                    
04741           SET        (ADDRESS OF CONTROL-FILE)                    
04742           RIDFLD     (CNTL-KEY)                                   
04743      END-EXEC.                                                    
04744                                                                   
04745      MOVE CF-PROCESSOR-NAME      TO SS08D.                        
04746      MOVE CF-PROCESSOR-TITLE     TO SS09D.                        
04747                                                                   
04748      EJECT                                                        
04749  7267-READ-CNTL4.                                                 
121802*    IF CL-SYSTEM-IDENTIFIER = 'CV'                               
121802*        GO TO 7267-READ-EMPLAN.                                  
04752                                                                   
04753      IF BEN-HOLD = ZEROS                                          
04754         GO TO 7267-READ-CNTL6.                                    
04755                                                                   
04756      MOVE BEN-HOLD               TO CNTL-GEN2.                    
04757      MOVE SPACES                 TO CNTL-GEN1.                    
04758                                                                   
04759      IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
04760         MOVE '4'                 TO CNTL-RECORD-TYPE              
04761      ELSE                                                         
04762         MOVE '5'                 TO CNTL-RECORD-TYPE.             
04763                                                                   
04764      MOVE ZEROS                  TO CNTL-SEQ.                     
04765                                                                   
04766      EXEC CICS HANDLE CONDITION                                   
04767           NOTOPEN    (8840-CNTL-NOT-OPEN)                         
04768           NOTFND     (7267-READ-CNTL6)                            
04769      END-EXEC.                                                    
04770                                                                   
04771      EXEC CICS READ                                               
04772           DATASET    (CNTL-ID)                                    
04773           SET        (ADDRESS OF CONTROL-FILE)                    
04774           RIDFLD     (CNTL-KEY)                                   
04775           GTEQ                                                    
04776      END-EXEC.                                                    
04777                                                                   
04778      MOVE 1                      TO SUB.                          
04779  7267-LOOP.                                                       
04780      IF SUB = 9                                                   
04781         GO TO 7267-READ-CNTL6.                                    
04782                                                                   
04783      IF CF-BENEFIT-CODE (SUB) < BEN-HOLD                          
04784         ADD 1 TO SUB                                              
04785         GO TO 7267-LOOP.                                          
04786                                                                   
04787      IF BEN-HOLD = CF-BENEFIT-CODE (SUB)                          
04788         MOVE CF-BENEFIT-DESCRIP (SUB) TO SS22D                    
121802        IF CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1
121802           OR CL-CLAIM-TYPE = 'I'
121203           OR CL-CLAIM-TYPE = 'G'
052614           OR CL-CLAIM-TYPE = 'F'
080322           OR CL-CLAIM-TYPE = 'B'
080322           OR CL-CLAIM-TYPE = 'H'
04790            MOVE CF-BENEFIT-ALPHA (SUB)   TO BENEFIT-WORK          
04791            MOVE ELIM-DAYS                TO SS42D.                
04792                                                                   
04793      GO TO 7267-READ-CNTL6.                                       
04794                                                                   
04795      EJECT                                                        
121802*7267-READ-EMPLAN. Remove as dead code                            

04820  7267-READ-CNTL6.                                                 
04821      MOVE '6'                    TO CNTL-RECORD-TYPE.             
04822      MOVE SPACES                 TO CNTL-GENL.                    
04823                                                                   
121802*    IF PI-COMPANY-ID = 'AIG' OR 'AUK'                            
121802*        MOVE CL-CURRENT-CARRIER TO CNTL-GEN4                     
121802*    ELSE                                                         
04827          MOVE PI-CARRIER         TO CNTL-GEN4.                    
04828                                                                   
04829      MOVE ZEROS                  TO CNTL-SEQ.                     
04830                                                                   
04831      EXEC CICS HANDLE CONDITION                                   
04832           NOTOPEN    (8840-CNTL-NOT-OPEN)                         
04833           NOTFND     (7268-SET-DATE)                              
04834      END-EXEC.                                                    
04835                                                                   
04836      EXEC CICS READ                                               
04837           DATASET    (CNTL-ID)                                    
04838           SET        (ADDRESS OF CONTROL-FILE)                    
04839           RIDFLD     (CNTL-KEY)                                   
04840      END-EXEC.                                                    
04841                                                                   
04842      MOVE SPACES              TO WS-LABEL-HOLD-AREA.              
04843      MOVE CF-MAIL-TO-NAME     TO WS-LABEL-LINES (1).              
04844      MOVE CF-IN-CARE-OF       TO WS-LABEL-LINES (2).              
04845      MOVE CF-ADDRESS-LINE-1   TO WS-LABEL-LINES (3).              
04846      MOVE CF-ADDRESS-LINE-2   TO WS-LABEL-LINES (4).              
04847      MOVE CF-CITY-STATE       TO WS-LABEL-LINES (5).              
04848                                                                   
04849      IF CF-ZIP-CODE-NUM NOT NUMERIC                               
04850          MOVE ZEROS              TO  CF-ZIP-CODE-NUM.             
04851      IF CF-ZIP-CODE-NUM NOT = ZEROS                               
04852          MOVE CF-ZIP-CODE-NUM    TO  WS-ZIP-NUMERIC               
04853          MOVE WS-ZIP-NONNUM      TO  CF-ZIP-CODE.                 
04854                                                                   
04855      MOVE SPACES              TO  WS-ZIP-CODE.                    
04856      IF CF-CANADIAN-POST-CODE                                     
04857          MOVE CF-CAN-POSTAL-1  TO  WS-CAN-POSTAL-1                
04858          MOVE CF-CAN-POSTAL-2  TO  WS-CAN-POSTAL-2                
04859      ELSE                                                         
04860          MOVE CF-ZIP-PRIME     TO  WS-AM-ZIP-CODE                 
04861          IF CF-ZIP-PLUS4 NOT = SPACES  AND  ZEROS                 
04862              MOVE '-'          TO  WS-AM-ZIP-DASH                 
04863              MOVE CF-ZIP-PLUS4 TO  WS-AM-ZIP-PLUS4.               
04864                                                                   
04865      MOVE WS-ZIP-CODE         TO  WS-LABEL-LINES (6).             
04866                                                                   
04867      MOVE CF-PHONE-NO         TO WS-PHONE-IN.                     
04868      MOVE WSPI-AREA           TO WSPO-AREA.                       
04869      MOVE WSPI-PFX            TO WSPO-PFX.                        
04870      MOVE WSPI-SFX            TO WSPO-SFX.                        
04871      MOVE WS-PHONE-OUT        TO SS04-6D.                         
04872                                                                   
04873      PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.                      
04874                                                                   
04875      MOVE WS-LABEL-LINES (1)  TO SS03D
010407     IF PI-CARRIER = '8'
010407        MOVE 'as Administrator for Investors Heritage Life Insuran
010407-     'ce Company'               TO SS03-1D
010407     END-IF
04876      MOVE WS-LABEL-LINES (2)  TO SS04-1D.                         
04877      MOVE WS-LABEL-LINES (3)  TO SS04-2D.                         
04878      MOVE WS-LABEL-LINES (4)  TO SS04-3D.                         
04879      MOVE WS-LABEL-LINES (5)  TO SS04-4D.                         
04880      MOVE WS-LABEL-LINES (6)  TO SS04-5D.                         
04881                                                                   
04882      EJECT                                                        
04883  7268-SET-DATE.                                                   
04884      MOVE EIBDATE                TO DATE-WORK.                    
04885      MOVE DT-WORK                TO DC-JULIAN-YYDDD.              
04886      MOVE '5'                    TO DC-OPTION-CODE.               
04887      PERFORM 9700-DATE-LINK  THRU  9700-EXIT.                     
04888      MOVE DC-GREG-DATE-1-EDIT    TO SS20D.                        
04889      MOVE DC-GREG-DATE-1-ALPHA   TO SS21D.                        
04890                                                                   
04891      IF NOT LOWER-CASE-LETTERS-USED                               
04892         GO TO 7269-EXIT.                                          
04893                                                                   
121802*    IF PI-COMPANY-ID = 'DMD'                                     
121802*        MOVE SS21D              TO WS-TEMP-AREA2                 
121802*        PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT         
121802*        IF WS-TEMP-AREA2 (1:1) = SPACE                           
121802*           MOVE WS-TEMP-AREA2 (2:29) TO WS-SAVE-TEMP-AREA2       
121802*           MOVE WS-SAVE-TEMP-AREA2   TO WS-TEMP-AREA2            
121802*        END-IF                                                   
121802*        IF WS-TEMP-AREA2 (1:1) = SPACE                           
121802*           MOVE WS-TEMP-AREA2 (2:29) TO WS-SAVE-TEMP-AREA2       
121802*           MOVE WS-SAVE-TEMP-AREA2   TO WS-TEMP-AREA2            
121802*        END-IF                                                   
121802*        IF WS-TEMP-AREA2 (1:1) = SPACE                           
121802*           MOVE WS-TEMP-AREA2 (2:29) TO WS-SAVE-TEMP-AREA2       
121802*           MOVE WS-SAVE-TEMP-AREA2   TO WS-TEMP-AREA2            
121802*        END-IF                                                   
121802*        IF WS-TEMP-AREA2 (1:1) = SPACE                           
121802*           MOVE WS-TEMP-AREA2 (2:29) TO WS-SAVE-TEMP-AREA2       
121802*           MOVE WS-SAVE-TEMP-AREA2   TO WS-TEMP-AREA2            
121802*        END-IF                                                   
121802*        IF WS-TEMP-AREA2 (1:1) = SPACE                           
121802*           MOVE WS-TEMP-AREA2 (2:29) TO WS-SAVE-TEMP-AREA2       
121802*           MOVE WS-SAVE-TEMP-AREA2   TO WS-TEMP-AREA2            
121802*        END-IF                                                   
121802*        MOVE WS-TEMP-AREA2      TO SS21D                         
121802*        GO TO 7269-EXIT.                                         
121802*                                                                 
04920      INSPECT SS19D   CONVERTING UPPER-CASE TO LOWER-CASE.         
04921      INSPECT SS35-1D CONVERTING UPPER-CASE TO LOWER-CASE.         
04922      INSPECT SS35-2D CONVERTING UPPER-CASE TO LOWER-CASE.         
04923      INSPECT SS22D   CONVERTING UPPER-CASE TO LOWER-CASE.         
04924                                                                   
04925      MOVE 'N'                    TO WS-STATE-LINE.                
04926                                                                   
102703     MOVE 'Y'                    TO WS-PROCESSOR-LINE.
04927      MOVE SS08D                  TO WS-TEMP-AREA2.                
04928      PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT.            
04929      MOVE WS-TEMP-AREA2          TO SS08D.                        
102703     MOVE 'N'                    TO WS-PROCESSOR-LINE.
102703     MOVE 'N'                    TO WS-CAPS-SW.
04930                                                                   
04931      MOVE SS09D                  TO WS-TEMP-AREA2.                
04932      PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT.            
04933      MOVE WS-TEMP-AREA2          TO SS09D.                        
04934                                                                   
04935      MOVE SS11-5D                TO WS-TEMP-AREA2.                
04936      PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT.            
04937      MOVE WS-TEMP-AREA2          TO SS11-5D.                      
04938                                                                   
04939      MOVE SS21D                  TO WS-TEMP-AREA2.                
04940      PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT.            
04941      MOVE WS-TEMP-AREA2          TO SS21D.                        
04942                                                                   
04943      MOVE SS40D                  TO WS-TEMP-AREA2.                
04944      PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT.            
04945      MOVE WS-TEMP-AREA2          TO SS40D.                        
04946                                                                   
04947      MOVE SS41D                  TO WS-TEMP-AREA2.                
04948      PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT.            
04949      MOVE WS-TEMP-AREA2          TO SS41D.                        
04950                                                                   
04951  7269-EXIT.                                                       
04952      EXIT.                                                        
04953                                                                   
04954      EJECT                                                        
04955              COPY ELCNAMET.                                       
04956                                  EJECT                            
04957  7290-RESOLVE-CREDITOR.                                           
04958                                                                   
04959      EXEC CICS HANDLE CONDITION                                   
04960           NOTOPEN    (7290-BENE-NOT-OPEN)                         
04961           NOTFND     (7290-EXIT)                                  
04962      END-EXEC.                                                    
04963                                                                   
04964      MOVE SPACES                 TO WS-LABEL-HOLD-AREA.           
04965                                                                   
04966      MOVE PI-COMPANY-CD          TO BENE-COMP-CD.                 
04967      MOVE 'B'                    TO BENE-REC-TYPE.                
04968      MOVE SPACES                 TO BENE-NUMBER.                  
04969      MOVE CL-CURRENT-GROUPING    TO BENE-CREDITOR.                
04970                                                                   
04971      EXEC CICS READ                                               
04972           DATASET    (BENE-ID)                                    
04973           SET        (ADDRESS OF BENEFICIARY-MASTER)              
04974           RIDFLD     (BENE-KEY)                                   
04975      END-EXEC.                                                    
04976                                                                   
04977      IF LOWER-CASE-LETTERS-USED                                   
04978          MOVE BE-MAIL-TO-NAME    TO WS-TEMP-AREA2                 
04979          PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT         
04980          MOVE WS-TEMP-AREA2      TO SS56D                         
04981      ELSE                                                         
04982          MOVE BE-MAIL-TO-NAME    TO SS56D.                        
04983                                                                   
04984      GO TO 7290-EXIT.                                             
04985                                                                   
04986  7290-BENE-NOT-OPEN.                                              
04987                                                                   
04988      MOVE ER-7675                TO EMI-ERROR.                    
04989      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
04990      MOVE -1                     TO MAINTL.                       
04991      GO TO 8200-SEND-DATAONLY.                                    
04992                                                                   
04993  7290-EXIT.                                                       
04994      EXIT.                                                        
04995                                                                   
04996      EJECT                                                        
04997  7300-VARIABLE-SEARCH.                                            
04998 ***************************************************************   
04999 *    THIS ROUTINE SEARCHES THE NEWLY CREATED LETTER FOR ANY   *   
05000 *    VARIABLE SYMBOL AND WILL REPLACE THE VARIABLE SYMBOL     *   
05001 *    WITH THE CORRESPONDING DATA FROM THE SYSTEM DEFINED      *   
05002 *    DATA THAT WAS GENERATED IN PARAGRAPHS 7200-7299.         *   
05003 *                                                             *   
05004 *    THE ADDRESSING OF THE VARIABLE TABLE IS ACCOMPLISHED     *   
05005 *    BY DOING A GETMAIN, MOVING THE TABLE TO THE NEW STORAGE  *   
05006 *    AND BY ADJUSTING THE BLL POINTER TO POINT AT THE DATA.   *   
05007 ***************************************************************   
05008                                                                   
05009      MOVE REC-TEXT (TB-INDX)     TO SINGLE-LINE.                  
05010                                                                   
05011      SET INDX1 TO 1.                                              
05012  7301-LOOP.                                                       
05013      IF INDX1 > 70                                                
05014         MOVE SINGLE-LINE         TO REC-TEXT (TB-INDX)            
05015         GO TO 7399-EXIT.                                          
05016                                                                   
05017      IF ONE-CHAR (INDX1) NOT = '@'                                
05018         SET INDX1 UP BY 1                                         
05019         GO TO 7301-LOOP.                                          
05020                                                                   
05021      SET INDX2 TO INDX1.                                          
05022      SET INDX2 UP BY 1.                                           
05023                                                                   
05024      IF ONE-CHAR (INDX2) = '@'                                    
05025         SET INDX1 UP BY 2                                         
05026         GO TO 7301-LOOP.                                          
05027                                                                   
05028      MOVE ONE-CHAR (INDX2)       TO V1.                           
05029      SET INDX2 UP BY 1.                                           
05030      MOVE ONE-CHAR (INDX2)       TO V2.                           
05031      SET INDX2 UP BY 1.                                           
05032      MOVE ONE-CHAR (INDX2)       TO V3.                           
05033      SET INDX2 UP BY 1.                                           
05034      MOVE ONE-CHAR (INDX2)       TO V4.                           
05035                                                                   
05036      IF V-NUM NOT NUMERIC                                         
05037         GO TO 7330-VAR-ERROR.                                     
05038                                                                   
05039      IF V-PERIOD NOT = '.'                                        
05040         MOVE '.'                 TO V-PERIOD                      
05041         MOVE ZERO                TO V-DECIMAL                     
05042         GO TO 7340-TABLE-SEARCH.                                  
05043                                                                   
05044      IF V-DECIMAL NUMERIC                                         
05045         GO TO 7340-TABLE-SEARCH.                                  
05046                                                                   
05047  7330-VAR-ERROR.                                                  
05048      MOVE ER-0180                TO EMI-ERROR.                    
05049      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
05050      SET INDX1 UP BY 1.                                           
05051      GO TO 7301-LOOP.                                             
05052                                                                   
05053  7340-TABLE-SEARCH.                                               
05054                                                                   
05055      IF REFRESH-GETMAIN-AREA                                      
05056 *        MOVE SAVE-VARIABLE-POINTER      TO LCP-WS-ADDR-COMP      
05057 *        SET ADDRESS OF SYSTEM-VARIABLES TO LCP-WS-ADDR-PNTR      
05058          MOVE SYSTEM-SUPPORTED-VARIABLES TO SYSTEM-VARIABLES      
05059          MOVE 2                          TO GETMAIN-SWITCH        
05060          MOVE 1                          TO SS-COUNTER
                                                  B1
05061      ELSE                                                         
05062          IF NO-GETMAIN-DONE-YET                                   
05063 *            EXEC CICS GETMAIN                                    
05064 *                 SET       (ADDRESS OF SYSTEM-VARIABLES)         
05065 *                 LENGTH    (SS-WORK-AREA-LENGTH)                 
05066 *            END-EXEC                                             
05067              MOVE 2              TO GETMAIN-SWITCH                
05068 *            SET LCP-WS-ADDR-PNTR TO ADDRESS OF SYSTEM-VARIABLES  
05069 *            MOVE LCP-WS-ADDR-COMP                                
05070 *                                TO SAVE-VARIABLE-POINTER         
05071              MOVE SYSTEM-SUPPORTED-VARIABLES                      
05072                                  TO SYSTEM-VARIABLES              
05073              MOVE 1              TO SS-COUNTER
                                          B1                    
05074          ELSE                                                     
05075              MOVE 1              TO SS-COUNTER
                                          B1
05076 *            MOVE SAVE-VARIABLE-POINTER                           
05077 *                                TO LCP-WS-ADDR-COMP              
05078 *            SET ADDRESS OF SYSTEM-VARIABLES TO LCP-WS-ADDR-PNTR
               END-IF
05079      END-IF
           .                                                             
05080  7350-TABLE-LOOP.                                                 
05081                                                                   
05082      IF SS-COUNTER  >  SS-NUM-ENTRIES                             
05083         GO TO 7330-VAR-ERROR.                                     
05084                                                                   
           MOVE SYSTEM-VARIABLES (B1:106)
                                       TO SYS-VAR-ENTRY
05085      IF SYS-VAR-CODE NOT = VAR-HOLD
              ADD SYS-VAR-LEN          TO B1
05086 *       SET LCP-WS-ADDR-PNTR TO ADDRESS OF SYSTEM-VARIABLES       
05087 *       ADD SYS-VAR-LEN          TO LCP-WS-ADDR-COMP              
05088 *       SET ADDRESS OF SYSTEM-VARIABLES TO LCP-WS-ADDR-PNTR       
05089         ADD 1                    TO SS-COUNTER                    
05090         GO TO 7350-TABLE-LOOP
           END-IF
05091                                                                   
05092      MOVE SYS-VAR-ENTRY          TO VARIABLE-WORK-AREA.           
05093      SET INDXV TO 1.                                              
05094      SUBTRACT 6                  FROM VAR-LEN.                    
05095      PERFORM 7400-MOVE-VAR-DATA VAR-LEN TIMES.                    
05096      GO TO 7301-LOOP.                                             
05097                                                                   
05098  7399-EXIT.                                                       
05099       EXIT.                                                       
05100                                                                   
05101  7400-MOVE-VAR-DATA.                                              
05102      IF INDX1 > 70                                                
05103         MOVE ER-0181             TO EMI-ERROR                     
05104         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
05105         GO TO 7301-LOOP.                                          
05106                                                                   
05107      MOVE VAR-ONE-CHAR (INDXV)   TO ONE-CHAR (INDX1).             
05108      SET INDXV UP BY 1.                                           
05109      SET INDX1 UP BY 1.                                           
05110                                                                   
05111      EJECT                                                        
05112  7500-READ-TS.                                                    
05113      EXEC CICS HANDLE CONDITION                                   
05114           QIDERR     (7590-TS-QIDERR)                             
05115           ITEMERR    (7585-TS-ITEMERR)                            
05116      END-EXEC.                                                    
05117                                                                   
05118      SET TS-INDX TO 1.                                            
05119      MOVE 1                      TO TS-ITEM.                      
05120                                                                   
05121  7501-LOOP.                                                       
05122      EXEC CICS READQ TS                                           
05123           INTO     (TS-WORK-AREA)                                 
05124           QUEUE    (TS-NAME-TEXT)                                 
05125           LENGTH   (TS-LENGTH)                                    
05126           ITEM     (TS-ITEM)                                      
05127      END-EXEC.                                                    
05128                                                                   
05129      MOVE TS-WORK-AREA           TO TS-GROUP (TS-INDX).           
05130      SET TS-INDX UP BY 1.                                         
05131      ADD 1                       TO TS-ITEM.                      
05132      GO TO 7501-LOOP.                                             
05133                                                                   
05134  7585-TS-ITEMERR.                                                 
05135      IF EIBTRNID NOT = TRANS-ID                                   
05136         SUBTRACT 1               FROM TS-ITEM                     
05137         MOVE TS-ITEM             TO PI-TEMP-STOR-ITEMS.           
05138                                                                   
05139      GO TO 7599-EXIT.                                             
05140                                                                   
05141  7590-TS-QIDERR.                                                  
05142      IF EIBTRNID = TRANS-ID                                       
05143         MOVE ER-0033             TO EMI-ERROR                     
05144         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
05145         GO TO 8200-SEND-DATAONLY.                                 
05146                                                                   
05147  7599-EXIT.                                                       
05148       EXIT.                                                       
05149                                                                   
05150      EJECT                                                        
05151  7600-READ-SCREEN-TS.                                             
05152      EXEC CICS HANDLE CONDITION                                   
05153           QIDERR     (7690-TS-QIDERR)                             
05154           ITEMERR    (7685-TS-ITEMERR)                            
05155      END-EXEC.                                                    
05156                                                                   
05157      MOVE 1                      TO TS-ITEM.                      
05158                                                                   
05159      EXEC CICS READQ TS                                           
05160           INTO    (EL152AO)                                       
05161           QUEUE   (TS-NAME-SCREEN)                                
05162           LENGTH  (TS-MAP-LENGTH)                                 
05163           ITEM    (TS-ITEM)                                       
05164      END-EXEC.                                                    
05165                                                                   
05166      IF MAINTL NOT = ZEROS                                        
05167         MOVE AL-UANON            TO MAINTA.                       
05168      IF ARCHNUML NOT = ZEROS                                      
05169         MOVE AL-UNNON            TO ARCHNUMA.                     
05170      IF FORML NOT = ZEROS                                         
05171         MOVE AL-UANON            TO FORMA.                        
05172      IF FOLLOWL NOT = ZEROS                                       
05173         MOVE AL-UANON            TO FOLLOWA.                      
05174      IF RESENDL NOT = ZEROS                                       
05175         MOVE AL-UANON            TO RESENDA.                      
05176      IF PRINTL NOT = ZEROS                                        
05177         MOVE AL-UANON            TO PRINTA.                       
05178      IF COPIESL NOT = ZEROS                                       
05179         MOVE AL-UNNON            TO COPIESA.                      
05180      IF ADDRL NOT = ZEROS                                         
05181         MOVE AL-UANON            TO ADDRA.                        
05182      IF REL NOT = ZEROS                                           
05183         MOVE AL-UANON            TO REA.                          
05184                                                                   
05185      GO TO 7699-EXIT.                                             
05186                                                                   
05187  7685-TS-ITEMERR.                                                 
05188      GO TO 7699-EXIT.                                             
05189                                                                   
05190  7690-TS-QIDERR.                                                  
05191      IF EIBTRNID = TRANS-ID                                       
05192         MOVE ER-0033             TO EMI-ERROR                     
05193         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
05194         GO TO 8200-SEND-DATAONLY.                                 
05195                                                                   
05196  7699-EXIT.                                                       
05197       EXIT.                                                       
05198      EJECT                                                        
05199                                                                   
05200  7700-PUT-TEMP-STOR.                                              
05201      PERFORM 7750-DELETE-TEMP-STOR THRU 7750-EXIT.                
05202      PERFORM 7760-DELETE-TEMP-STOR-SCREEN THRU 7760-EXIT.         
05203      SET TS-INDX TO 1.                                            
05204      MOVE 0                      TO PI-TEMP-STOR-ITEMS.           
05205      PERFORM 7780-WRITE-TS THRU 7780-EXIT                         
05206              VARYING TS-GROUP-WORK FROM 0 BY TS-NUM-REC-IN-GROUP  
05207              UNTIL TS-GROUP-WORK NOT < PI-TOTAL-LINES.            
05208                                                                   
05209  7749-EXIT.                                                       
05210       EXIT.                                                       
05211                                                                   
05212  7750-DELETE-TEMP-STOR.                                           
05213      EXEC CICS HANDLE CONDITION                                   
05214           QIDERR      (7750-EXIT)                                 
05215      END-EXEC.                                                    
05216                                                                   
05217      EXEC CICS DELETEQ TS                                         
05218           QUEUE       (TS-NAME-TEXT)                              
05219      END-EXEC.                                                    
05220                                                                   
05221  7750-EXIT.                                                       
05222      EXIT.                                                        
05223                                                                   
05224  7760-DELETE-TEMP-STOR-SCREEN.                                    
05225      EXEC CICS HANDLE CONDITION                                   
05226           QIDERR     (7760-EXIT)                                  
05227      END-EXEC.                                                    
05228                                                                   
05229      EXEC CICS DELETEQ TS                                         
05230           QUEUE      (TS-NAME-SCREEN)                             
05231      END-EXEC.                                                    
05232                                                                   
05233  7760-EXIT.                                                       
05234      EXIT.                                                        
05235                                                                   
05236  7770-DELETE-TEMP-STOR-PI-AREA.                                   
05237      EXEC CICS HANDLE CONDITION                                   
05238          QIDERR   (7770-EXIT)                                     
05239      END-EXEC.                                                    
05240                                                                   
05241      EXEC CICS DELETEQ TS                                         
05242          QUEUE    (WS-PI-QID)                                     
05243      END-EXEC.                                                    
05244                                                                   
05245  7770-EXIT.                                                       
05246      EXIT.                                                        
05247                                                                   
05248      EJECT                                                        
05249  7780-WRITE-TS.                                                   
05250      MOVE TS-GROUP (TS-INDX)     TO TS-WORK-AREA.                 
05251      SET TS-INDX UP BY 1.                                         
05252      ADD 1                       TO PI-TEMP-STOR-ITEMS.           
05253                                                                   
05254      EXEC CICS WRITEQ TS                                          
05255           FROM    (TS-WORK-AREA)                                  
05256           QUEUE   (TS-NAME-TEXT)                                  
05257           LENGTH  (TS-LENGTH)                                     
05258           ITEM    (PI-TEMP-STOR-ITEMS)                            
05259      END-EXEC.                                                    
05260                                                                   
05261  7780-EXIT.                                                       
05262      EXIT.                                                        
05263                                                                   
05264      EJECT                                                        
05265  7790-WRITE-SCREEN-TS.                                            
05266      MOVE 1                      TO TS-ITEM.                      
05267                                                                   
05268      EXEC CICS WRITEQ TS                                          
05269           FROM    (EL152AI)                                       
05270           QUEUE   (TS-NAME-SCREEN)                                
05271           LENGTH  (TS-MAP-LENGTH)                                 
05272           ITEM    (TS-ITEM)                                       
05273      END-EXEC.                                                    
05274                                                                   
05275  7790-EXIT.                                                       
05276      EXIT.                                                        
05277                                                                   
05278  7795-WRITE-PI-AREA-TS.                                           
05279      EXEC CICS WRITEQ TS                                          
05280          QUEUE    (WS-PI-QID)                                     
05281          FROM     (PROGRAM-INTERFACE-BLOCK)                       
05282          LENGTH   (PI-COMM-LENGTH)                                
05283      END-EXEC.                                                    
05284                                                                   
05285  7795-EXIT.                                                       
05286      EXIT.                                                        
05287                                                                   
05288      EJECT                                                        
05289  7800-PRINT-LETTER-NOW.                                           
05290 ***************************************************************   
05291 *     THIS ROUTINE WILL CAUSE THE CURRENTLY CREATED LETTER    *   
05292 *     TO BE PRINTED ON A HARDCOPY PRINTER.                    *   
05293 *        THE TEXT IS EDITED FOR ANY UNRESOLVED SYMBOLS,       *   
05294 *        THE PRINTER ID IS OBTAINED FROM THE CONTROL FILE,    *   
05295 *        THE LETTER IS SAVED IN TEMP-STORAGE,                 *   
05296 *        AND A START IS ISSUED FOR THE PRINT TRANSACTION.     *   
05297 ***************************************************************   
05298                                                                   
05299      PERFORM 400-SET-CODES THRU 499-EXIT.                         
05300                                                                   
081004     PERFORM 7500-READ-TS        THRU 7599-EXIT

030805     PERFORM 7950-SET-INDX THRU 7950-EXIT.                        
030805     PERFORM 7960-UPDATE-TABLE-FROM-SCREEN THRU 7960-EXIT         
030805             VARYING SC-INDX FROM 1 BY 1                          
030805             UNTIL SC-INDX > NUM-LINES-PER-SCREEN.                
05324                                                                   
05301      IF PI-TOTAL-LINES = 0                                        
05302         MOVE ER-0187             TO EMI-ERROR                     
05303         MOVE -1                  TO MAINTL                        
05304         MOVE AL-UABON            TO MAINTA                        
05305         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
05306         GO TO 8200-SEND-DATAONLY.                                 
05307                                                                   
05308      IF COPIESL NOT = ZEROS                                       
05309         IF COPIESI NOT NUMERIC                                    
05310            MOVE ER-0184          TO EMI-ERROR                     
05311            MOVE -1               TO COPIESL                       
05312            MOVE AL-UNBON         TO COPIESA                       
05313            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
05314            GO TO 8200-SEND-DATAONLY                               
05315          ELSE                                                     
05316            MOVE COPIESI          TO PI-NUM-PRINT-COPIES           
05317        ELSE                                                       
05318         MOVE 1                   TO PI-NUM-PRINT-COPIES.          
05319                                                                   
081004*    PERFORM 7500-READ-TS        THRU 7599-EXIT

05320 *    PERFORM 7950-SET-INDX THRU 7950-EXIT.                        
05321 *    PERFORM 7960-UPDATE-TABLE-FROM-SCREEN THRU 7960-EXIT         
05322 *            VARYING SC-INDX FROM 1 BY 1                          
05323 *            UNTIL SC-INDX > NUM-LINES-PER-SCREEN.                
05324                                                                   
05325      MOVE +0                     TO TALLY.                        
05326      INSPECT RECORD-TABLE TALLYING TALLY                          
05327                                  FOR CHARACTERS BEFORE '@'.       
05328                                                                   
05329      IF TALLY < +21900                                            
05330         COMPUTE PI-CURRENT-LINE = TALLY / 73                      
05331         MOVE ZEROS               TO ROLL-COUNTER                  
05332         MOVE ER-0191             TO EMI-ERROR                     
05333         MOVE -1                  TO MAINTL                        
05334         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
05335         GO TO 7900-ROLL-PAGE.                                     
05336                                                                   
05337  7800-UPDATE-CLAIM.                                               
05338                                                                   
05339      IF PI-RETURN-TO-PROGRAM = PGM-EL126                          
05340          GO TO 7800-GET-PRINTER.                                  
05341                                                                   
05342      MOVE PI-COMPANY-CD          TO  CLAM-CO.                     
05343      MOVE PI-CARRIER             TO  CLAM-CARRIER.                
05344      MOVE PI-CLAIM-NO            TO  CLAM-CLAIM.                  
05345      MOVE PI-CERT-NO             TO  CLAM-CERT-NUM.               
05346                                                                   
05347      EXEC CICS HANDLE CONDITION                                   
05348          NOTOPEN   (8860-CLAM-NOT-OPEN)                           
05349          NOTFND    (7800-CLAIM-NOT-FOUND)                         
05350      END-EXEC.                                                    
05351                                                                   
05352      EXEC CICS READ                                               
05353          DATASET   (CLAM-ID)                                      
05354          RIDFLD    (CLAM-KEY)                                     
05355          SET       (ADDRESS OF CLAIM-MASTER)                      
05356          UPDATE                                                   
05357      END-EXEC.                                                    
05358                                                                   
05359      MOVE PI-PROCESSOR-ID        TO  CL-LAST-MAINT-USER.          
05360      MOVE EIBTIME                TO  CL-LAST-MAINT-HHMMSS.        
05361      MOVE CURRENT-SAVE           TO  CL-LAST-MAINT-DT.            
05362      MOVE '2'                    TO  CL-LAST-MAINT-TYPE.          
05363                                                                   
05364      EXEC CICS HANDLE CONDITION                                   
05365          DUPKEY    (7800-GET-PRINTER)                             
05366      END-EXEC.                                                    
05367                                                                   
05368      EXEC CICS REWRITE                                            
05369          DATASET   (CLAM-ID)                                      
05370          FROM      (CLAIM-MASTER)                                 
05371      END-EXEC.                                                    
05372                                                                   
05373      GO TO 7800-GET-PRINTER.                                      
05374                                                                   
05375  7800-CLAIM-NOT-FOUND.                                            
05376                                                                   
05377      MOVE ER-0133                TO  EMI-ERROR.                   
05378      MOVE -1                     TO  MAINTL.                      
05379      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
05380      GO TO 8200-SEND-DATAONLY.                                    
05381                                                                   
05382  7800-GET-PRINTER.                                                
05383                                                                   
05384      IF PI-ALT-PRINTER-ID NOT = SPACES                            
05385         GO TO 7800-CNTL-BYPASS.                                   
05386                                                                   
05387      MOVE '1'                    TO CNTL-RECORD-TYPE.             
05388      MOVE SPACES                 TO CNTL-GENL.                    
05389      MOVE ZEROS                  TO CNTL-SEQ.                     
05390                                                                   
05391      EXEC CICS HANDLE CONDITION                                   
05392           NOTOPEN    (8840-CNTL-NOT-OPEN)                         
05393           NOTFND     (7890-NOT-FOUND)                             
05394      END-EXEC.                                                    
05395                                                                   
05396      EXEC CICS READ                                               
05397           DATASET    (CNTL-ID)                                    
05398           SET        (ADDRESS OF CONTROL-FILE)                    
05399           RIDFLD     (CNTL-KEY)                                   
05400      END-EXEC.                                                    
05401                                                                   
042605*    MOVE CF-FORMS-PRINTER-ID       TO TS-TERM-TEXT.              
042605     MOVE CF-FORMS-PRINTER-ID    TO WS-PRINTER-ID              
05403      GO TO 7800-SET-UP-TS.                                        
05404                                                                   
05405  7800-CNTL-BYPASS.                                                
042605*    MOVE PI-ALT-PRINTER-ID         TO TS-TERM-TEXT.              
042605     MOVE PI-ALT-PRINTER-ID      TO WS-PRINTER-ID.
05407                                                                   
05408  7800-SET-UP-TS.                                                  
05409                                                                   
05410 ***********************************************************       
05411 *      CHECK TO SEE IF IT IS A PRINT REQUEST FOR PRINTING *       
05412 *      LETTERS ON A 3275 PRINTER. IF SO, SAVE THE SCREEN  *       
05413 ***********************************************************       
05414                                                                   
05415      IF TS-TERM-PREFIX = 'DU'                                     
05416         PERFORM 7760-DELETE-TEMP-STOR-SCREEN THRU 7760-EXIT       
05417         PERFORM 7790-WRITE-SCREEN-TS THRU 7790-EXIT.              
05418                                                                   
05419      SET TS-INDX TO 1.                                            
05420                                                                   
05421      EXEC CICS ASKTIME                                            
05422      END-EXEC.                                                    
05423                                                                   
05424      MOVE EIBTIME                TO TS-ID-TIME.                   
042605*    MOVE '152A'                 TO TS-ID-TEXT
05425      MOVE TS-NAME-TEXT           TO PI-TEMP-STOR-ID.              
05426      MOVE 0                      TO PI-TEMP-STOR-ITEMS.           
05427                                                                   
05428      PERFORM 7780-WRITE-TS THRU 7780-EXIT                         
05429              VARYING TS-GROUP-WORK FROM 0 BY TS-NUM-REC-IN-GROUP  
05430              UNTIL TS-GROUP-WORK NOT < PI-TOTAL-LINES             
05431                                                                   
05432      IF NOT PI-SHOW-MODE                                          
05433         MOVE '1'                 TO PI-PRINT-SW.                  
05434                                                                   
05435      EXEC CICS HANDLE CONDITION                                   
05436           TERMIDERR  (8820-TERM-ERROR)                            
05437           TRANSIDERR (8830-TRAN-ERROR)                            
05438      END-EXEC.                                                    
05439                                                                   
121802*    IF PI-COMPANY-ID = 'DMD' OR 'XXX'
05441 *        MOVE EIBTRMID    TO TS-TERM-TEXT                         
121802*        EXEC CICS START                                          
121802*             INTERVAL    (0)                                     
121802*             TRANSID     (PRINT-TRANS)                           
121802*             FROM        (PROGRAM-INTERFACE-BLOCK)               
121802*             LENGTH      (PI-COMM-LENGTH)                        
05447 *             TERMID      (TS-TERM-TEXT)                          
121802*        END-EXEC                                                 
121802*    ELSE                                                         
05450          EXEC CICS START                                          
05451               INTERVAL    (0)                                     
05452               TRANSID     (PRINT-TRANS)                           
05453               FROM        (PROGRAM-INTERFACE-BLOCK)               
05454               LENGTH      (PI-COMM-LENGTH)                        
042605*             TERMID      (TS-TERM-TEXT)                          
042605              TERMID      (WS-PRINTER-ID)                         
05456          END-EXEC.                                                
05457                                                                   
050505     MOVE '104A'                 TO TS-ID-TEXT
05458      MOVE ER-0189                TO EMI-ERROR.                    
05459      MOVE -1                     TO MAINTL.                       
05460      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  
042605     MOVE ' PRINT TRANS STARTED ' TO EMI-MESSAGE-AREA (1)
042605     MOVE PI-TEMP-STOR-ID TO EMI-MESSAGE-AREA (1) (25:10)
05461                                                                   
05462      GO TO 8200-SEND-DATAONLY.                                    
05463                                                                   
05464  7890-NOT-FOUND.                                                  
05465      MOVE ER-0190                TO EMI-ERROR.                    
05466      MOVE -1                     TO MAINTL.                       
05467      PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.                   
05468      GO TO 8200-SEND-DATAONLY.                                    
05469                                                                   
05470      EJECT                                                        
05471  7900-ROLL-PAGE.                                                  
05472      IF ENTERPFL NOT = ZEROS                                      
05473         MOVE -1                  TO ENTERPFL                      
05474        ELSE                                                       
05475         MOVE -1                  TO MAINTL.                       
05476                                                                   
110404     PERFORM 7500-READ-TS     THRU 7599-EXIT
05499      PERFORM 7950-SET-INDX    THRU 7950-EXIT                     
05500      PERFORM 7960-UPDATE-TABLE-FROM-SCREEN THRU 7960-EXIT     
05501         VARYING SC-INDX FROM 1 BY 1 UNTIL                
05502         SC-INDX > NUM-LINES-PER-SCREEN

05477      IF PI-TOTAL-LINES = 0                                        
05478         MOVE ER-0047             TO EMI-ERROR                     
05479         MOVE -1                  TO MAINTL                        
05480         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
05481         GO TO 8200-SEND-DATAONLY.                                 
05482                                                                   
05483      COMPUTE TEMP-CURR-LINE = PI-CURRENT-LINE + ROLL-COUNTER.     
05484                                                                   
05485      IF TEMP-CURR-LINE NEGATIVE OR TEMP-CURR-LINE = ZEROS         
05486         MOVE ER-0067             TO EMI-ERROR                     
05487         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
05488         MOVE 1                   TO TEMP-CURR-LINE.               
05489                                                                   
05490      IF TEMP-CURR-LINE > PI-TOTAL-LINES                           
05491         MOVE ER-0066             TO EMI-ERROR                     
05492         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
05493         COMPUTE TEMP-CURR-LINE                                    
05494             = PI-TOTAL-LINES + 1 - NUM-LINES-PER-SCREEN           
05495         IF TEMP-CURR-LINE NEGATIVE OR TEMP-CURR-LINE = ZEROS      
05496             MOVE 1 TO TEMP-CURR-LINE.                             
05497                                                                   
05498 *    IF EMI-ERROR NOT = ER-0191                                   
110404*       PERFORM 7500-READ-TS     THRU 7599-EXIT
05499 *       PERFORM 7950-SET-INDX    THRU 7950-EXIT                     
05500 *       PERFORM 7960-UPDATE-TABLE-FROM-SCREEN THRU 7960-EXIT     
05501 *                VARYING SC-INDX FROM 1 BY 1 UNTIL                
05502 *                SC-INDX > NUM-LINES-PER-SCREEN
      *    END-IF
05503                                                                   
05504      IF EMI-ERROR = ER-0066 OR ER-0067 OR ER-0000 OR ER-0191      
05505         NEXT SENTENCE                                             
05506      ELSE                                                         
05507         GO TO 8200-SEND-DATAONLY.                                 
05508                                                                   

110404*    PERFORM 7700-PUT-TEMP-STOR   THRU 7749-EXIT

05509      MOVE TEMP-CURR-LINE         TO PI-CURRENT-LINE.              
05510      SET TB-INDX                 TO PI-CURRENT-LINE.              
05511      MOVE LOW-VALUES             TO EL152RI.                      
05512                                                                   
081004*    PERFORM 7500-READ-TS        THRU 7599-EXIT

05513      PERFORM 7170-FORMAT-SCREEN THRU 7170-EXIT                    
05514              VARYING SC-INDX FROM 1 BY 1                          
05515              UNTIL SC-INDX > NUM-LINES-PER-SCREEN.                
05516                                                                   
05517      GO TO 8200-SEND-DATAONLY.                                    
05518                                                                   
05519      EJECT                                                        
05520  7950-SET-INDX.                                                   
05521      IF PI-CURRENT-LINE = 0                                       
05522         SET TB-INDX TO 1                                          
05523        ELSE                                                       
05524         SET TB-INDX              TO PI-CURRENT-LINE.              
05525                                                                   
05526  7950-EXIT.                                                       
05527       EXIT.                                                       
05528                                                                   
05529      EJECT                                                        
05530  7960-UPDATE-TABLE-FROM-SCREEN.                                   
05531      IF SC-TEXTL (SC-INDX) NOT = ZEROS                            
05532         IF TB-INDX > PI-TOTAL-LINES                               
05533            IF PI-TOTAL-LINES = MAX-LINES                          
05534               MOVE ER-0051       TO EMI-ERROR                     
05535               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            
05536               GO TO 8200-SEND-DATAONLY                            
05537            ELSE                                                   
05538               MOVE SC-TEXT (SC-INDX) TO REC-TEXT (TB-INDX)        
05539               ADD 1              TO PI-TOTAL-LINES                
05540         ELSE                                                      
05541            MOVE SC-TEXT (SC-INDX) TO REC-TEXT (TB-INDX).          
05542                                                                   
05543      SET TB-INDX UP BY 1.                                         
05544                                                                   
05545  7960-EXIT.                                                       
05546       EXIT.                                                       
05547                                                                   
05548      EJECT                                                        
05549  8000-STARTBR-ERACCT.                                             
05550                                                                   
05551      IF WS-ACCT-READ-SW = 'Y'                                     
05552          NEXT SENTENCE                                            
05553      ELSE                                                         
05554          EXEC CICS GETMAIN                                        
05555               SET       (ADDRESS OF ACCOUNT-MASTER)               
05556               LENGTH    (ACCT-LENGTH)                             
05557          END-EXEC.                                                
05558                                                                   
05559      EXEC CICS STARTBR                                            
05560          RIDFLD      (ACCT-KEY)                                   
05561          DATASET     (ACCT-ID)                                    
05562          KEYLENGTH   (20)                                         
05563          GENERIC                                                  
05564      END-EXEC.                                                    
05565                                                                   
05566      MOVE 'Y'         TO ACCT-BROWSE-STARTED.                     
05567  8000-STARTBR-EXIT.                                               
05568      EXIT.                                                        
05569                                                                   
05570  8000-READNEXT-ERACCT.                                            
05571                                                                   
05572      EXEC CICS READNEXT                                           
05573          DATASET   (ACCT-ID)                                      
05574          INTO      (ACCOUNT-MASTER)                               
05575          RIDFLD    (ACCT-KEY)                                     
05576      END-EXEC.                                                    
05577                                                                   
05578  8000-READNEXT-EXIT.                                              
05579      EXIT.                                                        
05580                                                                   
05581      EJECT                                                        
121802*8050-STARTBR-EMPROD. Remove as dead code                         
121802*8050-EXIT. Remove as dead code                                   
121802*8060-READNEXT-EMPROD. Remove as dead code                        
121802*8060-EXIT. Remove as dead code                                   
05614                                                                   
05615      EJECT                                                        
05616  8100-SEND-INITIAL-MAP.                                           

081004     PERFORM 7700-PUT-TEMP-STOR  THRU 7749-EXIT

05617      MOVE SAVE-DATE              TO DATEAO.                       
05618      MOVE EIBTIME                TO TIME-IN.                      
05619      MOVE TIME-OUT               TO TIMEAO.                       
05620                                                                   
           MOVE PI-CARRIER             TO CARRO
           MOVE PI-CLAIM-NO            TO CLMNOO
           MOVE PI-CERT-NO             TO CRTNOO
           MOVE PI-PROCESSOR-ID        TO PROCO
           MOVE PI-COMPANY-ID          TO COMPIDO
           MOVE FUNCTION UPPER-CASE(WS-KIX-MYENV)
                                       TO SYSO

           MOVE PI-ENCLOSURE-CD        TO ENCO

05621      IF NOT EMI-NO-ERRORS                                         
05622          SET EMI-INDX TO 1                                        
05623          MOVE EMI-MESSAGE-AREA (EMI-INDX) TO ERRMSGO              
05624      ELSE                                                         
05625          MOVE SPACES             TO ERRMSGO.                      
05626                                                                   
05627      MOVE -1                     TO MAINTL.                       
05628                                                                   
05629      EXEC CICS SEND                                               
05630          MAP      (MAP-NAME)                                      
05631          MAPSET   (MAPSET-NAME)                                   
05632          FROM     (EL152AO)                                       
05633          ERASE                                                    
05634          CURSOR                                                   
05635      END-EXEC.                                                    
05636                                                                   
05637      PERFORM 8210-ENDBR  THRU 8210-EXIT.                          
05638      PERFORM 8220-ENDBR-EMPROD THRU 8220-EXIT.                    
05639                                                                   
081004     GO TO 9000-RETURN-TRANS.
081004*    GO TO 0200-RECEIVE.
05641                                                                   
05642  8200-SEND-DATAONLY.                                              

081004     PERFORM 7700-PUT-TEMP-STOR   THRU 7749-EXIT

           MOVE PI-ENCLOSURE-CD        TO ENCO

05643      MOVE SAVE-DATE              TO DATEAO.                       
05644      MOVE EIBTIME                TO TIME-IN.                      
05645      MOVE TIME-OUT               TO TIMEAO.                       
05646      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO                       
05647                                                                   
121802*    IF PI-COMPANY-ID = 'DMD'                                     
121802*       IF FORCE-7840                                             
121802*          MOVE '7840 FORCED' TO PFKEY9O                          
121802*          MOVE AL-SABON      TO PFKEY9A.                         
05652                                                                   
05653      EXEC CICS SEND                                               
05654          MAP      (MAP-NAME)                                      
05655          MAPSET   (MAPSET-NAME)                                   
05656          FROM     (EL152AO)                                       
05657          DATAONLY                                                 
05658          CURSOR                                                   
05659      END-EXEC.                                                    
05660                                                                   
05661      PERFORM 8210-ENDBR  THRU 8210-EXIT.                          
05662      PERFORM 8220-ENDBR-EMPROD THRU 8220-EXIT.                    
05663                                                                   
081004     GO TO 9000-RETURN-TRANS.
081004*    GO TO 0200-RECEIVE.
05665                                                                   
05666  8200-SEND-DATAONLY-ERASEAUP.                                     

081004     PERFORM 7700-PUT-TEMP-STOR   THRU 7749-EXIT

           MOVE PI-ENCLOSURE-CD        TO ENCO

05667      MOVE SAVE-DATE              TO DATEAO.                       
05668      MOVE EIBTIME                TO TIME-IN.                      
05669      MOVE TIME-OUT               TO TIMEAO.                       
05670      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO                       
05671      EXEC CICS SEND                                               
05672          MAP      (MAP-NAME)                                      
05673          MAPSET   (MAPSET-NAME)                                   
05674          FROM     (EL152AO)                                       
05675          DATAONLY                                                 
05676          ERASEAUP                                                 
05677          CURSOR                                                   
05678      END-EXEC.                                                    
05679                                                                   
05680      PERFORM 8210-ENDBR  THRU 8210-EXIT.                          
05681      PERFORM 8220-ENDBR-EMPROD THRU 8220-EXIT.                    
05682                                                                   
081004     GO TO 9000-RETURN-TRANS.
081004*    GO TO 0200-RECEIVE.
05684                                                                   
05685  8210-ENDBR.                                                      
05686      IF ACCT-BROWSE-STARTED = 'Y'                                 
05687         MOVE 'N'                  TO ACCT-BROWSE-STARTED          
05688         EXEC CICS ENDBR                                           
05689              DATASET    (ACCT-ID)                                 
05690         END-EXEC.                                                 
05691                                                                   
05692  8210-EXIT.                                                       
05693      EXIT.                                                        
05694                                                                   
05695  8220-ENDBR-EMPROD.                                               
05696      IF PROD-BROWSE-STARTED = 'Y'                                 
05697         MOVE 'N'                  TO PROD-BROWSE-STARTED          
05698         EXEC CICS ENDBR                                           
05699              DATASET    (PROD-ID)                                 
05700         END-EXEC.                                                 
05701                                                                   
05702  8220-EXIT.                                                       
05703      EXIT.                                                        
05704                                                                   
05705  8300-SEND-TEXT.                                                  
05706      EXEC CICS SEND TEXT                                          
05707          FROM    (LOGOFF-TEXT)                                    
05708          LENGTH  (LOGOFF-LENGTH)                                  
05709          ERASE                                                    
05710          FREEKB                                                   
05711      END-EXEC.                                                    
05712                                                                   
05713      EXEC CICS RETURN                                             
05714      END-EXEC.                                                    
05715                                                                   
05716  8600-DEEDIT.                                                     
05717      EXEC CICS BIF DEEDIT                                         
05718           FIELD    (DEEDIT-FIELD)                                 
05719           LENGTH   (15)                                           
05720      END-EXEC.                                                    
05721                                                                   
05722      EJECT                                                        
05723  8800-UNAUTHORIZED-ACCESS.                                        
05724      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   
05725      GO TO 8300-SEND-TEXT.                                        
05726                                                                   
05727  8810-PF23.                                                       
05728      PERFORM 7750-DELETE-TEMP-STOR THRU 7750-EXIT.                
05729      MOVE ZEROS                  TO PI-TOTAL-LINES.               
05730      MOVE ZEROS                  TO PI-CURRENT-LINE.              
05731      MOVE EIBAID                 TO PI-ENTRY-CD-1.                
05732      MOVE XCTL-005               TO PGM-NAME.                     
05733      GO TO 9300-XCTL.                                             
05734                                                                   
05735  8820-TERM-ERROR.                                                 
05736      MOVE ER-0412                TO EMI-ERROR.                    
05737      MOVE SPACES                 TO PI-PRINT-SW.                  
05738      GO TO 8999-OPEN-ERROR.                                       
05739                                                                   
05740  8830-TRAN-ERROR.                                                 
05741      MOVE ER-0413                TO EMI-ERROR.                    
05742      MOVE SPACES                 TO PI-PRINT-SW.                  
05743      GO TO 8999-OPEN-ERROR.                                       
05744                                                                   
05745  8840-CNTL-NOT-OPEN.                                              
05746      MOVE ER-0042                TO EMI-ERROR.                    
05747      GO TO 8999-OPEN-ERROR.                                       
05748                                                                   
05749  8850-ARCH-NOT-OPEN.                                              
05750      MOVE ER-0332                TO EMI-ERROR.                    
05751      GO TO 8999-OPEN-ERROR.                                       
05752                                                                   
05753  8860-CLAM-NOT-OPEN.                                              
05754      MOVE ER-0154                TO EMI-ERROR.                    
05755      GO TO 8999-OPEN-ERROR.                                       
05756                                                                   
05757  8870-ACTV-NOT-OPEN.                                              
05758      MOVE ER-0172                TO EMI-ERROR.                    
05759      GO TO 8999-OPEN-ERROR.                                       
05760                                                                   
05761  8880-ACCT-NOT-OPEN.                                              
05762      MOVE ER-0168                TO EMI-ERROR.                    
05763      GO TO 8999-OPEN-ERROR.                                       
05764                                                                   
05765  8890-TEXT-NOT-OPEN.                                              
05766      MOVE ER-0013                TO EMI-ERROR.                    
05767      GO TO 8999-OPEN-ERROR.                                       
05768                                                                   
05769  8900-CERT-NOT-OPEN.                                              
05770      MOVE ER-0169                TO EMI-ERROR.                    
05771      GO TO 8999-OPEN-ERROR.                                       
05772                                                                   
05773  8910-CERT-NOT-FOUND.                                             
05774      MOVE ER-0206                TO EMI-ERROR.                    
05775      GO TO 8999-OPEN-ERROR.                                       
05776                                                                   
05777  8915-PROD-NOT-OPEN.                                              
05778      MOVE ER-9106                TO EMI-ERROR.                    
05779      GO TO 8999-OPEN-ERROR.                                       
05780                                                                   
05781  8920-PLCY-NOT-OPEN.                                              
05782      MOVE ER-9883                TO EMI-ERROR.                    
05783      GO TO 8999-OPEN-ERROR.                                       
05784                                                                   
05785  8925-PLAN-NOT-OPEN.                                              
05786      MOVE ER-9808                TO EMI-ERROR.                    
05787      GO TO 8999-OPEN-ERROR.                                       
05788                                                                   
05789  8930-PLCY-NOT-FOUND.                                             
05790      MOVE ER-9483                TO EMI-ERROR.                    
05791      GO TO 8999-OPEN-ERROR.                                       
05792                                                                   
05793  8999-OPEN-ERROR.                                                 
05794      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
05795      MOVE -1                     TO MAINTL.                       
05796      GO TO 8200-SEND-DATAONLY.                                    
05797                                                                   
081004 9000-RETURN-TRANS.
081004
081004     MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO

081004     EXEC CICS RETURN
081004         TRANSID    (TRANS-ID)
081004         COMMAREA   (PROGRAM-INTERFACE-BLOCK)
081004         LENGTH     (PI-COMM-LENGTH)
081004     END-EXEC.
081004
081004
081004 9000-EXIT.
081004     EXIT.

05798  9200-RETURN-MAIN-MENU.                                           
05799      PERFORM 7750-DELETE-TEMP-STOR THRU 7750-EXIT.                
05800                                                                   
05801      MOVE ZEROS                  TO PI-TOTAL-LINES                
05802                                     PI-CURRENT-LINE.              
05803      MOVE XCTL-126               TO PGM-NAME.                     
05804                                                                   
05805      GO TO 9300-XCTL.                                             
05806                                                                   
05807  9300-XCTL.                                                       
05808      EXEC CICS XCTL                                               
05809          PROGRAM    (PGM-NAME)                                    
05810          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     
05811          LENGTH     (PI-COMM-LENGTH)                              
05812      END-EXEC.                                                    
05813                                                                   
05814  9400-CLEAR.                                                      
05815      IF PI-CLEAR-MODE                                             
05816          MOVE PI-RETURN-TO-PROGRAM TO PGM-NAME                    
05817          GO TO 9300-XCTL                                          
05818      ELSE                                                         
05819          PERFORM 7750-DELETE-TEMP-STOR         THRU 7750-EXIT     
05820          PERFORM 7760-DELETE-TEMP-STOR-SCREEN  THRU 7760-EXIT     
05821          PERFORM 7770-DELETE-TEMP-STOR-PI-AREA THRU 7770-EXIT     
05822          MOVE SPACES             TO PI-WA                         
05823                                     RECORD-TABLE                  
05824                                     TS-WORK-AREA                  
05825          MOVE ZEROS              TO PI-TOTAL-LINES                
05826                                     PI-CURRENT-LINE               
05827                                     PI-TEMP-STOR-ITEMS            
05828                                     PI-UPDATE-SW                  
05829                                     PI-ADDR-SEQ                   
05830          MOVE '2'                TO PI-ACTION                     
05831          MOVE LOW-VALUES         TO EL152AO                       
05832                                     PI-ADDR-TYPE                  
05833          SET TS-INDX                                              
05834              TA1                                                  
05835              TA2                                                  
05836              TA21                                                 
05837              TB-INDX                                              
05838              TB-INDX1                                             
05839              MOVE-INDX                                            
05840              SC-INDX             TO +1                            
05841          IF GETMAIN-SWITCH = '2'                                  
05842              MOVE 1              TO GETMAIN-SWITCH                
05843              GO TO 8100-SEND-INITIAL-MAP                          
05844          ELSE                                                     
05845              GO TO 8100-SEND-INITIAL-MAP.                         
05846                                                                   
05847  9600-PGMID-ERROR.                                                
05848      EXEC CICS HANDLE CONDITION                                   
05849          PGMIDERR    (8300-SEND-TEXT)                             
05850      END-EXEC.                                                    
05851                                                                   
05852      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.           
05853      MOVE ' '                    TO PI-ENTRY-CD-1.                
05854      MOVE XCTL-005               TO PGM-NAME.                     
05855      MOVE PGM-NAME               TO LOGOFF-PGM.                   
05856      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  
05857      GO TO 9300-XCTL.                                             
05858                                                                   
05859  9700-DATE-LINK.                                                  
05860      MOVE LINK-ELDATCV           TO PGM-NAME.                     
05861      EXEC CICS LINK                                               
05862          PROGRAM   (PGM-NAME)                                     
05863          COMMAREA  (DATE-CONVERSION-DATA)                         
05864          LENGTH    (DC-COMM-LENGTH)                               
05865      END-EXEC.                                                    
05866                                                                   
121802*    IF NO-CONVERSION-ERROR                                       
121802*            AND                                                  
121802*       W-REVERSE-DATE                                            
121802*            AND                                                  
121802*       PI-COMPANY-ID = 'AUK'                                     
121802*           MOVE DC-GREG-DATE-1-EDIT TO W-EDIT-DATE-1             
121802*           MOVE W-ED1-MM            TO W-ED2-MM                  
121802*           MOVE W-ED1-DD            TO W-ED2-DD                  
121802*           MOVE W-ED1-YY            TO W-ED2-YY                  
121802*           MOVE W-EDIT-DATE-2       TO DC-GREG-DATE-1-EDIT.      
05877                                                                   
05878  9700-EXIT.                                                       
05879       EXIT.                                                       
05880                                                                   
05881  9900-ERROR-FORMAT.                                               
05882      IF NOT EMI-ERRORS-COMPLETE                                   
05883          MOVE LINK-001           TO PGM-NAME                      
05884          EXEC CICS LINK                                           
05885              PROGRAM   (PGM-NAME)                                 
05886              COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)            
05887              LENGTH    (EMI-COMM-LENGTH)                          
05888          END-EXEC.                                                
05889                                                                   
05890  9900-EXIT.                                                       
05891      EXIT.                                                        
05892                                                                   
05893  9990-ABEND.                                                      
05894      MOVE LINK-004               TO PGM-NAME.                     
05895      MOVE DFHEIBLK               TO EMI-LINE1.                    
05896      EXEC CICS LINK                                               
05897          PROGRAM   (PGM-NAME)                                     
05898          COMMAREA  (EMI-LINE1)                                    
05899          LENGTH    (72)                                           
05900      END-EXEC.                                                    
05901                                                                   
05902      GO TO 8200-SEND-DATAONLY.                                    
05903                                                                   
05904  9995-SECURITY-VIOLATION.                                         
05905                              COPY ELCSCTP.                        
05906                                                                   
