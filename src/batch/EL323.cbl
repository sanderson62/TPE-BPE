00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                 EL323 .                              
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 06/20/95 13:43:16.                 
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            
00008 *                            VMOD=2.021.                          
00009                                                                   
00009                                                                   
00010 *AUTHOR.     LOGIC,INC.                                           
00011 *            DALLAS, TEXAS.                                       
00012                                                                   
00013 *DATE-COMPILED.                                                   
00014                                                                   
00015 *SECURITY.   *****************************************************
00016 *            *                                                   *
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00018 *            *                                                   *
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00022 *            *                                                   *
00023 *            *****************************************************
00024                                                                   
00025 *REMARKS.                                                         
00026 *        THIS REPORT IS PRODUCED DAILY TO RECORD THE AUTOMATIC    
00027 *    ACTIVITY PERFORMED BY THE SYSTEM AND TO PROVIDE A CURRENT    
00028 *    DOCKET FOR THE CLAIMS PROCESSOR'S.  THE REPORT CONSISTS OF   
00029 *    EIGHT SECTIONS:                                              
00030                                                                   
00031 *        1.  AUTOMATIC PAYMENTS GENERATED                         
00032 *        2.  CORRESPONDENCE TO BE RE-SENT                         
00033 *        3.  CLAIMS WITH NO ACTIVITY FOR 30 DAYS                  
00034 *        4.  AUTOMATIC CLAIM CLOSINGS                             
00035 *        5.  UNPROCESSED CHECKS                                   
00036 *        6.  CLAIMS WITH UNRESOLVED ERRORS                        
00037 *        7.  AUTOMATIC PROMPT REQUESTS                            
00038 *        8.  UNAPPROVED CLAIM PAYMENTS                            
00039 *        9.  LAG REPORT                                           
00040 *       10.  LAPSE IN ACTIVITY REPORT                             
00041 *       11.  INCOMPLETE CERTIFICATE REPORT                        
080510******************************************************************
080510*                   C H A N G E   L O G
080510*-----------------------------------------------------------------
080510*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
080510* EFFECTIVE    NUMBER
080510*-----------------------------------------------------------------
080510* 080510  CR2009122800001  AJRA  NAPERSOFT AUTO CLOSE ON FOLLOW UP
021113* 021113  CR2012072400001  AJRA  ADD 323GA RPT 'AUTO' PAYS BY USER
091914* 091914  IR2014091800004  PEMA  CORRECT 323GA REPORT
050619* 051619  CR2019042600001  PEMA  Remove 323GA report
080510******************************************************************
00042                                                                   
00043      EJECT                                                        
00044  ENVIRONMENT DIVISION.                                            
00045  CONFIGURATION SECTION.                                           
00046                                                                   
00047  INPUT-OUTPUT SECTION.                                            
00048                                                                   
00049  FILE-CONTROL.                                                    
00050                                                                   
00051      SELECT REPORTS-EXTRACT-FILE                                  
00052          ASSIGN TO SYS010-UT-FBA1-S-SYS010.                       
00053                                                                   
00054      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   
00055                                                                   
00056      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   
00057                                                                   
00058      SELECT ELREPT           ASSIGN TO SYS018-FBA1-ELREPT         
00059                              ORGANIZATION IS INDEXED              
00060                              ACCESS IS DYNAMIC                    
00061                              RECORD KEY IS RF-CONTROL-PRIMARY     
00062                              FILE STATUS IS ELREPT-FILE-STATUS.   
021113
021113     SELECT RPT-G-AUTO       ASSIGN TO SYS012.
021113
021113     SELECT SORT-G-AUTO       ASSIGN TO SORT.
00063                                                                   
00064      EJECT                                                        
00065  DATA DIVISION.                                                   
00066                                                                   
00067  FILE SECTION.                                                    
00068                                                                   
00069  FD  REPORTS-EXTRACT-FILE        COPY ELCEXTFD.                   
00070                                                                   
00071                                  COPY ELCEXTR.                    
00072                                                                   
00073  FD  PRNTR                       COPY ELCPRTFD.                   
00074                                                                   
00075  FD  FICH                        COPY ELCFCHFD.                   
00076                                                                   
00077  FD  ELREPT                      COPY ELCRPTFD.                   
00078                                                                   
00079                                  COPY ELCREPT.                    
00080                                                                   
021113 FD  RPT-G-AUTO.
021113 01  RPT-G-RECORD.
021113     10   FILLER                 PIC X(2).
021113     10   RPT-G-REC-KEY          PIC X(40).
021113     10   FILLER                 PIC X(25).
021113     10   RPT-G-REC-USER         PIC X(4).
021113     10   FILLER                 PIC X(248).
021113
021113 SD  SORT-G-AUTO.
021113 01  SORT-G-RECORD.
021113     10   FILLER                 PIC X(2).
021113     10   SORT-G-REC-KEY         PIC X(40).
021113     10   FILLER                 PIC X(25).
021113     10   SORT-G-REC-USER        PIC X(4).
021113     10   FILLER                 PIC X(248).
021113
00081      EJECT                                                        
00082  WORKING-STORAGE SECTION.                                         
00083                                                                   
00084  77  FILLER  PIC X(32)   VALUE '********************************'.
00085  77  FILLER  PIC X(32)   VALUE '*     EL323  WORKING STORAGE   *'.
00086  77  FILLER  PIC X(32)   VALUE '******** VMOD=2.021 ************'.
00087                                                                   
00088  01  FILLER       COMP-3.                                         
00089      12  WS-LINE-COUNT           PIC S9(3)   VALUE +99.           
00090      12  WS-LINE-COUNT-MAX       PIC S9(3)   VALUE +53.           
00091      12  WS-PAGE                 PIC S9(5)   VALUE ZERO.          
00092      12  WS-REPORT-SW            PIC S9      VALUE ZERO.          
00093      12  WS-PRINT-SW             PIC S9      VALUE ZERO.          
00094      12  WS-RECORD-COUNT         PIC S9(9)   VALUE ZERO.          
00095      12  WS-D-RECORD-COUNT       PIC S9(9)   VALUE ZERO.          
00096      12  WS-RETURN-CODE          PIC S9(3)   VALUE ZERO.          
00097      12  WS-ZERO                 PIC S9      VALUE ZERO.          
00098      12  WS-START-SW             PIC S9      VALUE ZERO.          
00099                                                                   
00100      12  WS-FICHE-OPEN           PIC S9      VALUE ZERO.          
00101      12  WS-ONLINE-REPORT-FILE-OPEN  PIC S9      VALUE ZERO.      
021113     12  WS-RPT-G-AUTO-OPEN      PIC S9      VALUE ZERO.
00102                                                                   
00103      12  WS-PAYMENT-COUNT        PIC S9(5)   VALUE ZERO.          
00104      12  WS-PAYMENT-AMOUNT       PIC S9(7)V99 VALUE ZERO.         
00105                                                                   
00106      12  WS-LETTER-RESENT-COUNT  PIC S9(5)   VALUE ZERO.          
00107                                                                   
00108      12  WS-INACTIVE-CLAIM-COUNT PIC S9(5)   VALUE ZERO.          
00109      12  WS-CLAIMS-CLOSED-COUNT  PIC S9(5)   VALUE ZERO.          
00110      12  WS-CLAIMS-PURGED-COUNT  PIC S9(5)   VALUE ZERO.          
00111      12  WS-CLAIMS-WITH-ERRORS   PIC S9(5)   VALUE ZERO.          
00112      12  WS-CLAIM-ERROR-COUNT    PIC S9(5)   VALUE ZERO.          
00113      12  WS-LETTER-REC-COUNT     PIC S9(5)   VALUE ZERO.          
00114      12  WS-LAG-REC-COUNT        PIC S9(5)   VALUE ZERO.          
00115      12  WS-LAPSE-REC-COUNT      PIC S9(5)   VALUE ZERO.          
00116      12  WS-INCOMPLETE-REC-COUNT PIC S9(5)   VALUE ZERO.          
00117                                                                   
00118      12  WS-CHECK-COUNT          PIC S9(5)   VALUE ZERO.          
00119      12  WS-CHECK-AMOUNT         PIC S9(7)V99 VALUE ZERO.         
00120                                                                   
00121      12  WS-UNAPPROVE-COUNT      PIC S9(5)   VALUE ZERO.          
00122      12  WS-AMOUNT-PAID          PIC S9(7)V99 VALUE ZERO.         
00123      EJECT                                                        
00124                                                                   
00125  01  WS-DATE-AND-TIME.                                            
00126      12  WS-ACCEPT-DATE.                                          
00127          16  WS-AD-YY                PIC  99.                     
00128          16  WS-AD-MM                PIC  99.                     
00129          16  WS-AD-DD                PIC  99.                     
00130      12  WS-CURRENT-DATE.                                         
00131          16  WS-CD-MM                PIC  99.                     
00132          16  FILLER                  PIC  X          VALUE '/'.   
00133          16  WS-CD-DD                PIC  99.                     
00134          16  FILLER                  PIC  X          VALUE '/'.   
00135          16  WS-CD-YY                PIC  99.                     
00136      12  WS-REGULAR-ALPHA            PIC  X(18).                  
00137      12  WS-DMD-ALPHA                PIC  X(18).                  
00138      12  WS-USER-DATE-INPUT          PIC  X  VALUE 'N'.           
00139      12  WS-TIME-OF-DAY.                                          
00140          16  WS-TIME                 PIC  9(6).                   
00141          16  WS-HUN-SEC              PIC  99.                     
00142                                                                   
00143  01  FILLER      COMP SYNC.                                       
00144      12  PGM-SUB                 PIC S9(4)   VALUE +323.          
00145      12  WS-INDEX                PIC S9(4)   VALUE ZERO.          
00146      12  WS-LINE-NUMBER          PIC S9(8)   VALUE ZERO.
021113     12  SUB1                    PIC S9(4)   VALUE +0.
021113     12  WS-AUTO-RECS            PIC S9(4)   VALUE +0.
00147                                                                   
00148  01  FILLER.                                                      
00149      12  WS-HOLD-K-PROCESSOR     PIC X(4) VALUE LOW-VALUES.       
00150      12  WS-SAVE-PRINT-RECORD    PIC X(133)  VALUE SPACES.        
00151                                                                   
00152      12  WS-LAST-COMPANY-CD      PIC X       VALUE SPACES.        
00153      12  WS-LAST-RECORD-TYPE     PIC X       VALUE SPACES.        
021113     12  WS-LAST-RECORDED-BY     PIC X(4)    VALUE SPACES.
00154                                                                   
00155      12  ELREPT-FILE-STATUS      PIC XX      VALUE ZERO.          
00156                                                                   
00157      12  X                       PIC X.                           
00158      12  WS-PRINT-OPTION         PIC X       VALUE 'P'.           
00159      12  WS-FORMAT-OPTION        PIC X       VALUE SPACES.        
00160      12  WS-PROCESS-OPTION       PIC X       VALUE SPACES.        
00161      12  WS-TOTAL-OPTION         PIC X       VALUE SPACES.        
00162                                                                   
00163      12  WS-PAID-THRU-TO-OPTION  PIC X       VALUE SPACES.        
00164      12  WS-ABEND-MESSAGE        PIC X(80)   VALUE SPACES.        
00165                                                                   
00166      12  WS-ABEND-FILE-STATUS    PIC XX      VALUE ZERO.          
00167                                                                   
00168      12  WS-FILE-ERROR-MESSAGE.                                   
00169          16  FILLER              PIC X(24)   VALUE                
00170              'ERROR OCCURED OPENING - '.                          
00171          16  WS-FEM-FILE-NAME    PIC X(8).                        
00172                                                                   
00173      12  WS-DATE-WORK.                                            
00174          16  FILLER              PIC X(6).                        
00175          16  WS-DW-YEAR          PIC 99.                          
00176                                                                   
00177      EJECT                                                        
00178      12  WS-ADDRESSEE-TYPE-AREA.                                  
00179          16  FILLER              PIC X(11) VALUE 'IINSURED'.      
00180          16  FILLER              PIC X(11) VALUE 'BBENEFICARY'.   
00181          16  FILLER              PIC X(11) VALUE 'AACCOUNT'.      
00182          16  FILLER              PIC X(11) VALUE 'PPHYSICIAN'.    
00183          16  FILLER              PIC X(11) VALUE 'EEMPLOYER'.     
00184          16  FILLER              PIC X(11) VALUE 'OOTHER 1'.      
00185          16  FILLER              PIC X(11) VALUE 'QOTHER 2'.      
00186                                                                   
00187      12  WS-ADDRESSEE-TYPE-TABLE     REDEFINES                    
00188          WS-ADDRESSEE-TYPE-AREA      OCCURS 7 TIMES               
00189          ASCENDING KEY IS WS-ADDRESSEE-TYPE                       
00190          INDEXED BY AI.                                           
00191                                                                   
00192          16  WS-ADDRESSEE-TYPE   PIC X.                           
00193          16  WS-ADDRESSEE-DESC   PIC X(10).                       
00194                                                                   
00195      12  WS-PAYMENT-TYPE-AREA.                                    
00196          16  FILLER              PIC X(11) VALUE '1PARTIAL'.      
00197          16  FILLER              PIC X(11) VALUE '2FINAL'.        
00198          16  FILLER              PIC X(11) VALUE '3LUMP SUM'.     
00199          16  FILLER              PIC X(11) VALUE '4ADDL'.         
00200          16  FILLER              PIC X(11) VALUE '5CHG EXP'.      
00201          16  FILLER              PIC X(11) VALUE '6OTH EXP'.      
00202          16  FILLER              PIC X(11) VALUE '7REFUND'.       
00203          16  FILLER              PIC X(11) VALUE '8REFUND'.       
00204                                                                   
00205      12  WS-PAYMENT-TYPE-TABLE       REDEFINES                    
00206          WS-PAYMENT-TYPE-AREA        OCCURS 8 TIMES               
00207          ASCENDING KEY IS WS-PAYMENT-TYPE                         
00208          INDEXED BY PI.                                           
00209                                                                   
00210          16  WS-PAYMENT-TYPE     PIC X.                           
00211          16  WS-PAYMENT-DESC     PIC X(10).                       
00212                                                                   
00213      12  WS-MAINTENANCE-TYPE-AREA.                                
00214          16  FILLER              PIC X(11) VALUE ' SET UP'.       
00215          16  FILLER              PIC X(11) VALUE '1PAYMENT'.      
00216          16  FILLER              PIC X(11) VALUE '2LETTER'.       
00217          16  FILLER              PIC X(11) VALUE '3ALTERED'.      
00218          16  FILLER              PIC X(11) VALUE '4RESTORED'.     
00219          16  FILLER              PIC X(11) VALUE '5INCURED DT'.   
00220          16  FILLER              PIC X(11) VALUE '6CONVERSION'.   
00221                                                                   
00222      12  WS-MAINTENANCE-TYPE-TABLE   REDEFINES                    
00223          WS-MAINTENANCE-TYPE-AREA    OCCURS 7 TIMES               
00224          ASCENDING KEY IS WS-MAINTENANCE-TYPE                     
00225          INDEXED BY MI.                                           
00226                                                                   
00227          16  WS-MAINTENANCE-TYPE PIC X.                           
00228          16  WS-MAINTENANCE-DESC PIC X(10).                       
00229                                                                   
00230      12  FIRST-TIME-SW           PIC X     VALUE 'Y'.             
00231          88  FIRST-TIME                    VALUE 'Y'.             
00232      12  FIRST-EXTRACT-C-SW      PIC X     VALUE 'Y'.             
00233          88  FIRST-EXTRACT-C               VALUE 'Y'.             
00234                                                                   
00235      12  WS-PREV-PROCESSOR       PIC X(4).                        
00236                                                                   
00237      EJECT                                                        
021113 01  WS-G-EXTRACT-REC.
021113     12  WS-G-RECORD-ID                      PIC XX.              
021113     12  WS-G-SORT-KEY-AREAS.                                       
021113         16  WS-G-POSITIONING-CODE           PIC X.               
021113         16  WS-G-EXTRACT-CODE               PIC X.               
021113         16  WS-G-COMPANY-CD                 PIC X.               
021113         16  WS-G-COMPANY-ID                 PIC XXX.             
021113         16  WS-G-RECORD-TYPE                PIC X.               
021113         16  WS-G-SORT-KEY.
021113             20  WS-G-SE3-SORT-CODE          PIC X.               
021113             20  WS-G-SE3-ACTION-DATE        PIC XX.              
021113             20  WS-G-SE3-PROCESSOR          PIC X(4).            
021113             20  WS-G-SE3-CARRIER            PIC X.               
021113             20  WS-G-SE3-CLAIM-NO           PIC X(7).            
021113             20  WS-G-SE3-CERT-NO            PIC X(11).           
021113             20  FILLER                      PIC X(7).            
021113     12  WS-G-DATA-AREA.
021113         16  WS-G-DG-CLAIM-TYPE              PIC X.               
021113         16  WS-G-DG-INSURED-LAST-NAME       PIC X(18).           
021113         16  WS-G-DG-FILE-LOCATION           PIC X(4).            
021113         16  WS-G-DG-INCURRED-DT             PIC XX.              
021113         16  WS-G-DG-RECORDED-BY             PIC X(4).            
021113         16  WS-G-DG-TEXT-LINE-1.                                   
021113             20  FILLER                      PIC X(12).           
021113             20  WS-G-DG-LETTER-SENT-DT      PIC X(8).            
021113             20  FILLER                      PIC X(4).            
021113             20  WS-G-DG-ADRESSEE-NAME       PIC X(30).           
021113             20  FILLER                      PIC X.               
021113             20  WS-G-DG-ADRESSEE-TYPE       PIC X(11).           
021113             20  FILLER                      PIC X(4).            
021113                                                                  
021113         16  WS-G-DG-TEXT-LINE-2             PIC X(80).           
021113         16  WS-G-DG-OVERRIDE-L2             PIC X(2).            
021113         16  WS-G-DG-DROP-CLAIM-SW           PIC X(1).
021113         16  FILLER                          PIC X(95).           
021113
00238  01  WS-HEADING1.                                                 
00239      12  FILLER                  PIC X(21)   VALUE '1'.           
00240      12  WS-H1-TITLE             PIC X(40)   VALUE                
00241          'AUTOMATIC ACTIVITY AND FOLLOW-UP REPORT'.               
00242      12  FILLER                  PIC X(11)   VALUE SPACES.        
00243      12  WS-H1-REPORT-NUMBER     PIC X(7) VALUE 'EL -323'.        
00244      12  WS-H1-REPORT-TYPE       PIC X       VALUE 'A'.           
021113     12  WS-HI-AUDITOR-RPT       PIC X       VALUE SPACES.
021113     12  FILLER                  PIC X(52)   VALUE SPACES.        
00246                                                                   
00247  01  WS-HEADING2.                                                 
00248      12  FILLER                  PIC X(26)   VALUE SPACES.        
00249      12  WS-H2-CLIENT-NAME       PIC X(30)   VALUE SPACES.        
00250      12  FILLER                  PIC X(16)   VALUE SPACES.        
00251      12  WS-H2-DATE              PIC X(8)    VALUE SPACES.        
00252      12  FILLER                  PIC X(53)   VALUE SPACES.        
00253                                                                   
00254  01  WS-HEADING3.                                                 
00255      12  FILLER                  PIC X(33)   VALUE SPACES.        
00256      12  WS-H3-DATE              PIC X(20)   VALUE SPACES.        
00257      12  FILLER                  PIC X(12)   VALUE SPACES.        
00258      12  FILLER                  PIC X(5)    VALUE 'PAGE'.        
00259      12  WS-H3-PAGE              PIC ZZ,ZZ9.                      
00260      12  FILLER                  PIC X(57)   VALUE SPACES.        
00261                                                                   
00262  01  WS-EL323A-HEADING4.                                          
00263      12  FILLER                  PIC X(25)   VALUE '-'.           
00264      12  FILLER                  PIC X(108)  VALUE                
00265          '- AUTOMATIC PAYMENTS GENERATED -'.                      
00266                                                                   
00267  01  WS-EL323A-HEADING5.                                          
00268      12  PIC X(38) VALUE '0    CLAIM     CERT      ACCOUNT      '.
00269      12  PIC X(38) VALUE '             INSURED                  '.
00270      12  PIC X(38) VALUE '                        PAID      CHEC'.
00271      12  PIC X(19) VALUE 'K   LST PMT    PMT '.                   
00272                                                                   
00273  01  WS-EL323A-HEADING6.                                          
00274      12  PIC X(38) VALUE ' CAR NUMBER   NUMBER     NAME         '.
00275      12  PIC X(38) VALUE '             NAME                     '.
00276      12  PIC X(38) VALUE '  PAYMENT     TYPE      THRU     NUMBE'.
00277      12  PIC X(19) VALUE 'R   SCHEDULED  INTV'.                   
00278                                                                   
00279  01  REDEFINES WS-EL323A-HEADING6.                                
00279      12  FILLER       PIC X(99).                                  
00279      12  EL323A-HDG   PIC X(04).                                  
00279      12  FILLER       PIC X(30).                                  
00279                                                                   
00280      EJECT                                                        
00281  01  WS-EL323B-HEADING4.                                          
00282      12  FILLER                  PIC X(25)   VALUE '-'.           
00283      12  FILLER                  PIC X(108)  VALUE                
00284          '- CORRESPONDENCE TO BE RE-SENT -'.                      
00285                                                                   
00286  01  WS-EL323B-HEADING5.                                          
00287      12  FILLER                  PIC X(133)  VALUE                
00288          '0     CLAIM   CERT            FORM  ARCHIVE   ORIGINAL  
00289 -        ' SCHEDULED       ADDRESSEE'.                            
00290                                                                   
00291  01  WS-EL323B-HEADING6.                                          
00292      12  FILLER                  PIC X(133)  VALUE                
00293          ' CAR NUMBER  NUMBER     TYPE  TYPE  NUMBER    SEND DATE 
00294 -        ' RE-SEND    BY   TYPE'.                                 
00295                                                                   
00296  01  WS-EL323C-HEADING4.                                          
00297      12  FILLER                  PIC X(21)   VALUE '-'.           
00298      12  FILLER                  PIC X(112)  VALUE                
00299          '- CLAIMS WITH NO ACTIVITY FOR 45 DAYS -'.               
00300                                                                   
00301  01  WS-EL323C-HEADING5.                                          
00302      12  FILLER                  PIC X(50)   VALUE                
00303          '0     CLAIM  CERT  '.                                   
00304      12  FILLER                  PIC X(23)   VALUE                
00305          'PAID     LAST     MAINT'.                               
00306      12  WS-EL323C-DMD5          PIC X(60)   VALUE                
00307          '                CREDIT CARD      CLAIMANT      '.       
00308                                                                   
00309  01  WS-EL323C-HEADING6.                                          
00310      12  FILLER                  PIC X(50)  VALUE                 
00311          ' CAR NUMBER  NUMBER     TYPE  INCURRED ESTABLISH  '.    
00312      12  EL323C-HDG              PIC X(4)   VALUE  'THRU'.        
00313      12  FILLER                  PIC X(26)  VALUE                 
00314          '    MAINT     TYPE      BY'.                            
00315      12  WS-EL323C-DMD6          PIC X(53)   VALUE                
00316          '         NUMBER           NAME        '.                
00317                                                                   
00318  01  WS-EL323D-HEADING4.                                          
00319      12  FILLER                  PIC X(27)   VALUE '-'.           
00320      12  FILLER                  PIC X(106)  VALUE                
00321          '- AUTOMATIC CLAIM CLOSINGS -'.                          
00322                                                                   
00323  01  WS-EL323D-HEADING5.                                          
00324      12  FILLER                  PIC X(52)   VALUE                
00325          '0     CLAIM   CERT  '.                                  
00326      12  FILLER                  PIC X(10)   VALUE 'PAID'.        
00327      12  FIA-HD5                 PIC X(10)   VALUE 'LAST'.        
00328      12  FILLER                  PIC X(05)   VALUE 'MAINT'.       
00329      12  FILLER                  PIC X(56)   VALUE SPACES.        
00330                                                                   
00331  01  WS-EL323D-HEADING6.                                          
00332      12  FILLER                  PIC X(40)  VALUE                 
00333          ' CAR NUMBER  NUMBER     TYPE   INCURRED '.              
00334      12  FIA-HD6                 PIC X(12)  VALUE                 
00335          ' ESTABLISH'.                                            
00336      12  EL323D-HDG              PIC X(4)   VALUE 'THRU'.         
00337      12  FILLER                  PIC X(06)  VALUE SPACES.         
00338      12  FIA-HDG6A               PIC X(10)  VALUE                 
00339          'MAINT'.                                                 
00340      12  FILLER                  PIC X(04)  VALUE                 
00341          'TYPE'.                                                  
00342                                                                   
00343      EJECT                                                        
00344  01  WS-EL323E-HEADING4.                                          
00345      12  FILLER                  PIC X(30)   VALUE '-'.           
00346      12  FILLER                  PIC X(103)  VALUE                
00347          '- UNPROCESSED CHECKS -'.                                
00348                                                                   
00349  01  WS-EL323E-HEADING5.                                          
00350      12  FILLER                  PIC X(133)  VALUE                
00351          '0  CHECK     CLAIM  CERT        CONTROL  PAYMENT        
00352 -        'PAID'.                                                  
00353                                                                   
00354  01  WS-EL323E-HEADING6.                                          
00355      12  FILLER                  PIC X(133)  VALUE                
00356          '  NUMBER CAR NUMBER  NUMBER      GROUP    TYPE          
00357 -        'DATE       STATUS'.                                     
00358                                                                   
00359  01  WS-EL323F-HEADING4.                                          
00360      12  FILLER                  PIC X(25)   VALUE '-'.           
00361      12  FILLER                  PIC X(108)  VALUE                
00362          '- CLAIMS WITH UNRESOLVED ERRORS -'.                     
00363                                                                   
00364  01  WS-EL323F-HEADING5.                                          
00365      12  FILLER                  PIC X(42)   VALUE                
00366          '0     CLAIM   CERT  '.                                  
00367      12  FILLER                  PIC X(91)   VALUE                
00368          'LAST     MAINT           FATAL  FORCE'.                 
00369                                                                   
00370  01  WS-EL323F-HEADING6.                                          
00371      12  FILLER                  PIC X(133)  VALUE                
00372          ' CAR NUMBER  NUMBER     TYPE  ESTABLISH   MAINT    TYPE 
00373 -        '      BY   ERROR  ERROR'.                               
00374                                                                   
00375  01  WS-EL323G-HEADING4.                                          
00376      12  FILLER                  PIC X(26)   VALUE '-'.           
00377      12  FILLER                  PIC X(107)  VALUE                
00378          '- AUTOMATIC PROMPT REQUESTS -'.                         
021113
021113 01  WS-EL323G-HEADING4-AUTO.                                          
021113     12  FILLER                  PIC X(23)   VALUE '-'.           
021113     12  FILLER                  PIC X(110)  VALUE                
021113         '- AUTO PROMPT REQUESTS BY AUDITOR -'.                         
00379                                                                   
00380  01  WS-EL323G-HEADING5.                                          
00381      12  FILLER                  PIC X(21)   VALUE                
00382          '0  ACTION'.                                             
00383      12  FILLER                  PIC X(50)   VALUE                
00384          'CLAIM   CERT '.                                         
00385      12  FILLER                  PIC X(60)   VALUE 'FILE'.        
00386                                                                   
00387  01  WS-EL323G-HEADING6.                                          
00388      12  FILLER                  PIC X(133)  VALUE                
00389          '  DATE     BY   CAR NUMBER  NUMBER     TYPE  INCURRED  I
00390 -        'NSURED           AT'.                                   
00391                                                                   
00392      EJECT                                                        
00393  01  WS-EL323H-HEADING4.                                          
00394      05  FILLER                      PIC X(30)       VALUE '-'.   
00395      05  FILLER                      PIC X(103)      VALUE        
00396          '- UNAPPROVED PAYMENTS -'.                               
00397                                                                   
00398  01  WS-EL323H-HEADING5.                                          
00399      05  FILLER                      PIC X(133)      VALUE        
00400          '0        CLAIM    CERT     TRLR    DATE      PAYMENT    
00401 -        '   AMOUNT'.                                             
00402                                                                   
00403  01  WS-EL323H-HEADING6.                                          
00404      05  FILLER                      PIC X(133)      VALUE        
00405          ' CARR    NUMBER   NUMBER   SEQ     PAID       TYPE      
00406 -        '    PAID      USER'.                                    
00407  01  WS-EL323I-HEADING4.                                          
00408      05  FILLER                      PIC X(30)       VALUE '-'.   
00409      05  FILLER                      PIC X(103)      VALUE        
00410          '- MAIL RECEIVED AND RECORDED -'.                        
00411                                                                   
00412  01  WS-EL323I-HEADING5.                                          
00413      05  FILLER                      PIC X(133)      VALUE        
00414          '0    CLAIM      CERT     INSURED               ARCHIVE  
00415 -        '  SEND   INITIAL   ANSWER'.                             
00416                                                                   
00417  01  WS-EL323I-HEADING6.                                          
00418      05  FILLER                      PIC X(133)      VALUE        
00419          ' CAR NUMBER    NUMBER    LAST NAME       FORM  NUMBER   
00420 -        '  DATE     DATE     DATE'.                              
00421                                                                   
00422      EJECT                                                        
00423  01  WS-EL323J-HEADING4.                                          
00424      05  FILLER                      PIC X(30)       VALUE '-'.   
00425      05  FILLER                      PIC X(103)      VALUE        
00426          '        - LAG REPORT -        '.                        
00427                                                                   
00428  01  WS-EL323J-HEADING5.                                          
00429      05  FILLER                      PIC X(133)      VALUE        
00430          '0    ACTION                        CLAIM        CERT    
00431 -        '                    LAST '.                             
00432                                                                   
00433  01  WS-EL323J-HEADING6.                                          
00434      05  FILLER                      PIC X(133)      VALUE        
00435          '      TYPE    DAYS    CAR  STATE   NUMBER      NUMBER   
00436 -        '  NAME              DATE'.                              
00437                                                                   
00438      EJECT                                                        
00439  01  WS-EL323K-HEADING4.                                          
00440      05  FILLER                      PIC X(30)       VALUE '-'.   
00441      05  FILLER                      PIC X(103)      VALUE        
00442          '-  LAPSE IN ACTIVITY REPORT  -'.                        
00443                                                                   
00444  01  WS-EL323K-HEADING5.                                          
00445      05  FILLER                      PIC X(20)       VALUE        
00446          '0    EXAMINER -     '.                                  
00447      05  WS-EL323K-PROCESSOR         PIC X(4).                    
00448                                                                   
00449  01  WS-EL323K-HEADING6.                                          
00450      05  FILLER                      PIC X(133)      VALUE        
00451          '0    ACTION                        CLAIM        CERT    
00452 -        '                    LAST '.                             
00453                                                                   
00454  01  WS-EL323K-HEADING7.                                          
00455      05  FILLER                      PIC X(133)      VALUE        
00456          '      TYPE    DAYS    CAR  STATE   NUMBER      NUMBER   
00457 -        '  NAME              DATE'.                              
00458                                                                   
00459  01  WS-EL323L-HEADING4.                                          
00460      05  FILLER                      PIC X(30)       VALUE '-'.   
00461      05  FILLER                      PIC X(103)      VALUE        
00462          '-  CLAIMS ATTACHED TO INCOMPLETE CERTIFICATES -'.       
00463                                                                   
00464  01  WS-EL323L-HEADING5.                                          
00465      05  FILLER                      PIC X(20)       VALUE        
00466          '0   CERT NUMBER -   '.                                  
00467      05  WS-EL323L-CERT-NO           PIC X(10) VALUE              
00468      'INCOMPLETE'.                                                
00469                                                                   
00470  01  WS-EL323L-HEADING6.                                          
00471      05  FILLER                      PIC X(133)      VALUE        
00472          '0         CLAIM            NAME                  ESTABLI
00473 -        'SH  PROC   LAST      MAINT'.                            
00474                                                                   
00475  01  WS-EL323L-HEADING7.                                          
00476      05  FILLER                      PIC X(133)      VALUE        
00477          '0   CAR   NUMBER   LAST             FIRST           DATE
00478 -        '     ID    MAINT     TYPE '.                            
00479                                                                   
00480      EJECT                                                        
00481  01  WS-DETAIL1                  PIC X(133)  VALUE SPACES.        
00482                                                                   
00483  01  WS-EL323A-DETAIL1               REDEFINES                    
00484      WS-DETAIL1.                                                  
00485      12  FILLER                  PIC XX.                          
00486      12  WS-A1-CARRIER           PIC X.                           
00487      12  FILLER                  PIC X.                           
00488      12  WS-A1-CLAIM-NO          PIC X(7).                        
00489      12  FILLER                  PIC X.                           
00490      12  WS-A1-CERT-NO           PIC X(11).                       
DAN        12  FILLER                  PIC X.
DAN        12  WS-A1-ACCT-NAME         PIC X(25).
DAN        12  FILLER                  PIC X.
DAN        12  WS-A1-INSURED-NAME      PIC X(25).
00491      12  FILLER                  PIC X.                           
00492      12  WS-A1-PAYMENT-AMOUNT    PIC Z,ZZZ,ZZ9.99-.               
00493      12  FILLER                  PIC X.                           
00494      12  WS-A1-PAYMENT-TYPE      PIC X(7).                        
00495      12  FILLER                  PIC XX.                          
00496      12  WS-A1-PAID-THRU         PIC X(8).                        
00497      12  FILLER                  PIC XX.                          
00498      12  WS-A1-CHECK-NO          PIC X(7).                        
00499      12  FILLER                  PIC XX.                          
00500      12  WS-A1-LAST-PMT-SCHEDULED PIC X(8).                       
00501      12  FILLER                  PIC X(4).                        
00502      12  WS-A1-PAYMENT-INTERVAL  PIC 99.                          
DAN        12  FILLER                  PIC X.                           
DAN*****   12  FILLER                  PIC X(53).                       
00504                                                                   
00505  01  FILLER                          REDEFINES                    
00506      WS-DETAIL1.                                                  
00507      12  FILLER                  PIC X(23).                       
00508      12  WS-A2-MESSAGE.                                           
00509          16  FILLER              PIC X(29).                       
00510          16  WS-A2-DAYS          PIC 9.                           
00511          16  FILLER              PIC X(10).                       
00512      12  WS-A2-INSURED           PIC X(30).                       
00513      EJECT                                                        
00514  01  WS-EL323B-DETAIL1               REDEFINES                    
00515      WS-DETAIL1.                                                  
00516      12  FILLER                  PIC XX.                          
00517      12  WS-B1-CARRIER           PIC X.                           
00518      12  FILLER                  PIC X.                           
00519      12  WS-B1-CLAIM-NO          PIC X(7).                        
00520      12  FILLER                  PIC X.                           
00521      12  WS-B1-CERT-NO           PIC X(11).                       
00522      12  FILLER                  PIC XX.                          
00523      12  WS-B1-CLAIM-TYPE        PIC X(4).                        
00524      12  FILLER                  PIC XX.                          
00525      12  WS-B1-FORM-TYPE         PIC X(4).                        
00526      12  FILLER                  PIC X.                           
00527      12  WS-B1-ARCHIVE-NO        PIC Z(7)9.                       
00528      12  WS-B1-ARCHIVE-NO-X          REDEFINES                    
00529          WS-B1-ARCHIVE-NO        PIC X(8).                        
00530      12  FILLER                  PIC XX.                          
00531      12  WS-B1-ORIG-SEND-DATE    PIC X(8).                        
00532      12  WS-B1-NOT-PRINTED-FLAG  PIC X.                           
00533      12  FILLER                  PIC XX.                          
00534      12  WS-B1-SCHEDULED-RESEND-DATE PIC X(8).                    
00535      12  FILLER                  PIC XX.                          
00536      12  WS-B1-BY                PIC X(4).                        
00537      12  FILLER                  PIC XX.                          
00538      12  WS-B1-ADDRESSEE-TYPE    PIC X(10).                       
00539      12  FILLER                  PIC X(50).                       
00540                                                                   
00541  01  WS-EL323B-DETAIL2               REDEFINES                    
00542      WS-DETAIL1.                                                  
00543      12  FILLER                  PIC X(8).                        
00544      12  WS-B2-DESCRIPTION       PIC X(3).                        
00545      12  WS-B2-REASON            PIC X(70).                       
00546      12  FILLER                  PIC X(52).                       
00547                                                                   
00548      EJECT                                                        
00549  01  WS-EL323C-DETAIL1               REDEFINES                    
00550      WS-DETAIL1.                                                  
00551      12  FILLER                  PIC X.                           
00552      12  WS-C1-CARRIER           PIC X.                           
00553      12  FILLER                  PIC X.                           
00554      12  WS-C1-CLAIM-NO          PIC X(7).                        
00555      12  FILLER                  PIC X.                           
00556      12  WS-C1-CERT-NO           PIC X(11).                       
00557      12  FILLER                  PIC XX.                          
00558      12  WS-C1-CLAIM-TYPE        PIC X(4).                        
00559      12  FILLER                  PIC XX.                          
00560      12  WS-C1-INCURRED-DATE     PIC X(8).                        
00561      12  FILLER                  PIC X.                           
00562      12  WS-C1-ESTABLISHED-DATE  PIC X(8).                        
00563      12  FILLER                  PIC X.                           
00564      12  WS-C1-PAID-THRU-DATE    PIC X(8).                        
00565      12  FILLER                  PIC X.                           
00566      12  WS-C1-LAST-MAINT-DATE   PIC X(8).                        
00567      12  FILLER                  PIC X.                           
00568      12  WS-C1-MAINT-TYPE        PIC X(10).                       
00569      12  FILLER                  PIC X.                           
00570      12  WS-C1-BY                PIC X(4).                        
00571      12  FILLER                  PIC X.                           
00572      12  WS-C1-CCN               PIC X(20).                       
00573      12  FILLER                  PIC X.                           
00574      12  WS-C1-LAST              PIC X(15).                       
00575      12  FILLER                  PIC X.                           
00576      12  WS-C1-FIRST             PIC X(12).                       
00577      12  FILLER                  PIC X.                           
00578      12  WS-C1-MID               PIC X(12).                       
00579                                                                   
00580      EJECT                                                        
00581  01  WS-EL323D-DETAIL1               REDEFINES                    
00582      WS-DETAIL1.                                                  
00583      12  FILLER                  PIC XX.                          
00584      12  WS-D1-CARRIER           PIC X.                           
00585      12  FILLER                  PIC X.                           
00586      12  WS-D1-CLAIM-NO          PIC X(7).                        
00587      12  FILLER                  PIC X.                           
00588      12  WS-D1-CERT-NO           PIC X(11).                       
00589      12  FILLER                  PIC XX.                          
00590      12  WS-D1-CLAIM-TYPE        PIC X(4).                        
00591      12  FILLER                  PIC XX.                          
00592      12  WS-D1-INCURRED-DATE     PIC X(8).                        
00593      12  FILLER                  PIC X(3).                        
00594      12  WS-D1-ESTABLISHED-DATE  PIC X(8).                        
00595      12  FILLER                  PIC XX.                          
00596      12  WS-D1-PAID-THRU-DATE    PIC X(8).                        
00597      12  FILLER                  PIC XX.                          
00598      12  WS-D1-LAST-MAINT-DATE   PIC X(8).                        
00599      12  FILLER                  PIC XX.                          
00600      12  WS-D1-MAINT-TYPE        PIC X(10).                       
00601      12  FILLER                  PIC XX.                          
00602      12  WS-D1-PURGED-MESSAGE    PIC X(14).                       
00603      12  FILLER                  PIC X(35).                       
00604                                                                   
00605      EJECT                                                        
00606  01  WS-EL323E-DETAIL1               REDEFINES                    
00607      WS-DETAIL1.                                                  
00608      12  FILLER                  PIC X.                           
00609      12  WS-E1-CHECK-NO          PIC X(7).                        
00610      12  FILLER                  PIC XX.                          
00611      12  WS-E1-CARRIER           PIC X.                           
00612      12  FILLER                  PIC X.                           
00613      12  WS-E1-CLAIM-NO          PIC X(7).                        
00614      12  FILLER                  PIC X.                           
00615      12  WS-E1-CERT-NO           PIC X(11).                       
00616      12  FILLER                  PIC XX.                          
00617      12  WS-E1-CONTROL-GROUP     PIC Z(5)9                        
00618                                      BLANK WHEN ZERO.             
00619      12  FILLER                  PIC X(3).                        
00620      12  WS-E1-PAYMENT-TYPE      PIC X(10).                       
00621      12  FILLER                  PIC X(3).                        
00622      12  WS-E1-DATE-PAID         PIC X(8).                        
00623      12  FILLER                  PIC X(3).                        
00624      12  WS-E1-STATUS            PIC X(16).                       
00625      12  FILLER                  PIC X(51).                       
00626                                                                   
00627  01  WS-EL323E-TOT-LINE  REDEFINES                                
00628         WS-DETAIL1.                                               
00629      12  FILLER                  PIC X.                           
00630      12  WS-E1-COUNT             PIC ZZ,ZZ9-.                     
00631      12  WS-E1-DESCRIPT1         PIC X(23).                       
00632      12  WS-E1-DESCRIPT2         PIC X(19).                       
00633      12  WS-E1-AMOUNT            PIC Z,ZZZ,ZZ9.99-.               
00634      12  FILLER                  PIC X(70).                       
00635      EJECT                                                        
00636  01  WS-EL323F-DETAIL1               REDEFINES                    
00637      WS-DETAIL1.                                                  
00638      12  FILLER                  PIC XX.                          
00639      12  WS-F1-CARRIER           PIC X.                           
00640      12  FILLER                  PIC X.                           
00641      12  WS-F1-CLAIM-NO          PIC X(7).                        
00642      12  FILLER                  PIC X.                           
00643      12  WS-F1-CERT-NO           PIC X(11).                       
00644      12  FILLER                  PIC XX.                          
00645      12  WS-F1-CLAIM-TYPE        PIC X(4).                        
00646      12  FILLER                  PIC XX.                          
00647      12  WS-F1-ESTABLISHED-DATE  PIC X(8).                        
00648      12  FILLER                  PIC XX.                          
00649      12  WS-F1-LAST-MAINT-DATE   PIC X(8).                        
00650      12  FILLER                  PIC XX.                          
00651      12  WS-F1-MAINT-TYPE        PIC X(10).                       
00652      12  FILLER                  PIC X.                           
00653      12  WS-F1-BY                PIC X(4).                        
00654      12  FILLER                  PIC X(3).                        
00655      12  WS-F1-FATAL-ERROR       PIC ZZ9-.                        
00656      12  FILLER                  PIC X(3).                        
00657      12  WS-F1-FORCE-ERROR       PIC ZZ9-.                        
00658      12  FILLER                  PIC X(53).                       
00659                                                                   
00660      EJECT                                                        
00661  01  WS-EL323G-DETAIL1               REDEFINES                    
00662      WS-DETAIL1.                                                  
00663      12  FILLER                  PIC X.                           
00664      12  WS-G1-ACTION-DATE       PIC X(8).                        
00665      12  FILLER                  PIC XX.                          
00666      12  WS-G1-BY                PIC X(4).                        
00667      12  FILLER                  PIC X(3).                        
00668      12  WS-G1-CARRIER           PIC X.                           
00669      12  FILLER                  PIC X.                           
00670      12  WS-G1-CLAIM-NO          PIC X(7).                        
00671      12  FILLER                  PIC X.                           
00672      12  WS-G1-CERT-NO           PIC X(11).                       
00673      12  FILLER                  PIC XX.                          
00674      12  WS-G1-CLAIM-TYPE        PIC X(4).                        
00675      12  FILLER                  PIC XX.                          
00676      12  WS-G1-INCURRED-DATE     PIC X(8).                        
00677      12  FILLER                  PIC XX.                          
00678      12  WS-G1-INSURED-NAME      PIC X(15).                       
00679      12  FILLER                  PIC XX.                          
00680      12  WS-G1-FILE-AT           PIC X(4).                        
00681      12  FILLER                  PIC X(4).                        
00682      12  FILLER                  PIC X(51).                       
00683                                                                   
00684  01  WS-EL323G-DETAIL2               REDEFINES                    
00685      WS-DETAIL1.                                                  
00686      12  FILLER                  PIC X(11).                       
00687      12  WS-G2-MESSAGE           PIC X(70).                       
00688      12  FILLER                  PIC X(52).                       
00689                                                                   
00690      EJECT                                                        
00691  01  WS-EL323H-DETAIL1               REDEFINES                    
00692      WS-DETAIL1.                                                  
00693      05  FILLER                      PIC XX.                      
00694      05  WS-H1-CARRIER               PIC X.                       
00695      05  FILLER                      PIC X(5).                    
00696      05  WS-H1-CLAIM-NO              PIC X(7).                    
00697      05  FILLER                      PIC XX.                      
00698      05  WS-H1-CERT-NO               PIC X(11).                   
00699      05  FILLER                      PIC XX.                      
00700      05  WS-H1-TRLR-SEQ-NO           PIC ZZZ9.                    
00701      05  FILLER                      PIC XX.                      
00702      05  WS-H1-DATE-PAID             PIC X(8).                    
00703      05  FILLER                      PIC X(3).                    
00704      05  WS-H1-PAYMENT-TYPE          PIC X(10).                   
00705      05  FILLER                      PIC X(3).                    
00706      05  WS-H1-AMOUNT-PAID           PIC ZZZZ,ZZZ.99.             
00707      05  FILLER                      PIC XX.                      
00708      05  WS-H1-USER                  PIC X(4).                    
00709      05  FILLER                      PIC X(56).                   
00710                                                                   
00711  01  WS-EL323H-TOT-LINE  REDEFINES                                
00712          WS-DETAIL1.                                              
00713      12  FILLER                  PIC X.                           
00714      12  WS-H1-COUNT             PIC ZZ,ZZ9-.                     
00715      12  WS-H1-DESCRIPT1         PIC X(20).                       
00716      12  WS-H1-DESCRIPT2         PIC X(19).                       
00717      12  WS-H1-AMOUNT            PIC Z,ZZZ,ZZ9.99-.               
00718      12  FILLER                  PIC X(73).                       
00719      EJECT                                                        
00720  01  WS-EL323I-DETAIL1 REDEFINES                                  
00721          WS-DETAIL1.                                              
00722      05  FILLER                      PIC XX.                      
00723      05  WS-I1-CARRIER               PIC X.                       
00724      05  FILLER                      PIC X.                       
00725      05  WS-I1-CLAIM-NO              PIC X(7).                    
00726      05  FILLER                      PIC X.                       
00727      05  WS-I1-CERT-NO               PIC X(11).                   
00728      05  FILLER                      PIC X.                       
00729      05  WS-I1-INSURED-LAST-NAME     PIC X(15).                   
00730      05  FILLER                      PIC XX.                      
00731      05  WS-I1-FORM                  PIC X(4).                    
00732      05  FILLER                      PIC X.                       
00733      05  WS-I1-ARCHIVE-NUMBER        PIC Z(7)9.                   
00734                                                                   
00735      05  WS-I1-ARCHIVE-NUMBER-X      REDEFINES                    
00736          WS-I1-ARCHIVE-NUMBER        PIC X(8).                    
00737      05  FILLER                      PIC XX.                      
00738      05  WS-I1-SEND-DATE             PIC X(8).                    
00739      05  FILLER                      PIC X.                       
00740      05  WS-I1-INITIAL-PRINT         PIC X(8).                    
00741      05  FILLER                      PIC X.                       
00742      05  WS-I1-ANSWER-RECEIVED       PIC X(8).                    
00743      05  FILLER                      PIC X(42).                   
00744                                                                   
00745  01  WS-EL323J-DETAIL1 REDEFINES                                  
00746          WS-DETAIL1.                                              
00747      05  FILLER                      PIC X(6).                    
00748      05  WS-J1-ACTION                PIC XX.                      
00749      05  FILLER                      PIC X(6).                    
00750      05  WS-J1-LAG-DAYS              PIC 9(4).                    
00751      05  FILLER                      PIC X(5).                    
00752      05  WS-J1-CARRIER               PIC X.                       
00753      05  FILLER                      PIC X(4).                    
00754      05  WS-J1-STATE                 PIC XX.                      
00755      05  FILLER                      PIC X(5).                    
00756      05  WS-J1-CLAIM-NO              PIC X(7).                    
00757      05  FILLER                      PIC XXX.                     
00758      05  WS-J1-CERT-NO               PIC X(11).                   
00759      05  FILLER                      PIC X.                       
00760      05  WS-J1-INSURED-LAST-NAME     PIC X(15).                   
00761      05  FILLER                      PIC XXX.                     
00762      05  WS-J1-LAST-DATE             PIC X(8).                    
00763                                                                   
00764  01  WS-EL323L-DETAIL1 REDEFINES                                  
00765          WS-DETAIL1.                                              
00766      05  FILLER                      PIC X(5).                    
00767      05  WS-L1-CARRIER               PIC X(1).                    
00768      05  FILLER                      PIC X(4).                    
00769      05  WS-L1-CLAIM-NO              PIC X(7).                    
00770      05  FILLER                      PIC X(2).                    
00771      05  WS-L1-LAST-NAME             PIC X(15).                   
00772      05  FILLER                      PIC X(2).                    
00773      05  WS-L1-FIRST-NAME            PIC X(12).                   
00774      05  FILLER                      PIC X(2).                    
00775      05  WS-L1-EST-DATE              PIC X(8).                    
00776      05  FILLER                      PIC X(2).                    
00777      05  WS-L1-PROCESSOR-ID          PIC X(4).                    
00778      05  FILLER                      PIC X(2).                    
00779      05  WS-L1-MAINT-DATE            PIC X(8).                    
00780      05  FILLER                      PIC X.                       
00781      05  WS-L1-MAINT-TYPE            PIC X(9).                    
00782                                                                   
00783  01  WS-TOTAL-LINE1                  REDEFINES                    
00784      WS-DETAIL1.                                                  
00785      12  FILLER                  PIC X.                           
00786      12  WS-T1-COUNT             PIC ZZ,ZZ9-.                     
00787      12  WS-T1-DESCRIPTION       PIC X(73).                       
00788                                                                   
00789      12  FILLER                      REDEFINES                    
00790          WS-T1-DESCRIPTION.                                       
00791          16  FILLER              PIC X(15).                       
00792          16  WS-T1-AMOUNT        PIC Z,ZZZ,ZZ9.99-.               
00793          16  FILLER              PIC X(45).                       
00794                                                                   
00795      12  FILLER                      REDEFINES                    
00796          WS-T1-DESCRIPTION.                                       
00797          16  FILLER              PIC X(12).                       
00798          16  WS-T1-ERRORS        PIC ZZ,ZZ9-.                     
00799          16  FILLER              PIC X(54).                       
00800                                                                   
00801      12  FILLER                  PIC X(52).                       
00802                                                                   
00803      EJECT                                                        
00804                                  COPY ELCDATE.                    
00805                                                                   
00806      EJECT                                                        
00807  PROCEDURE DIVISION.                                              
00808                                                                   
00809  1000-MAIN-LOGIC SECTION.                                         
00810                                                                   
00811      ACCEPT WS-ACCEPT-DATE.                                       
00812                                                                   
00813      IF WS-ACCEPT-DATE GREATER THAN SPACES                        
00814          MOVE WS-ACCEPT-DATE     TO  DC-GREG-DATE-1-YMD           
00815          MOVE '3'                TO  DC-OPTION-CODE               
00816          PERFORM 8500-DATE-CONVERSION                             
00817          IF NO-CONVERSION-ERROR                                   
00818              MOVE DC-GREG-DATE-1-ALPHA                            
00819                                  TO  WS-DMD-ALPHA                 
00820              MOVE 'Y'            TO  WS-USER-DATE-INPUT.          
00821                                                                   
00822      ACCEPT WS-ACCEPT-DATE       FROM  DATE.                      
00823                                                                   
00824      MOVE WS-AD-YY               TO  WS-CD-YY.                    
00825      MOVE WS-AD-MM               TO  WS-CD-MM.                    
00826      MOVE WS-AD-DD               TO  WS-CD-DD.                    
00827                                                                   
00828      ACCEPT WS-TIME-OF-DAY       FROM  TIME.                      
00829                                                                   
00830      PERFORM OPEN-FILES.                                          
00831                                                                   
00832      PERFORM 3000-PRINT-REPORT.                                   
00833                                                                   
00834      PERFORM CLOSE-FILES.                                         
00835                                                                   
00836      GOBACK.                                                      
00837                                                                   
00838      EJECT                                                        
00839  3000-PRINT-REPORT SECTION.                                       
00840                                                                   
00841  3100-PRINT-REPORT.                                               
00842      READ REPORTS-EXTRACT-FILE                                    
00843          AT END                                                   
00844              MOVE HIGH-VALUES    TO  EX-COMPANY-CD                
00845              GO TO 3200-PRINT-REPORT.                             
00846                                                                   
00847      ADD +1  TO  WS-RECORD-COUNT.                                 
00848                                                                   
00849      IF EX-POSITIONING-CODE LESS THAN '2'                         
00850          GO TO 3100-PRINT-REPORT.                                 
00851                                                                   
00852      IF EX-POSITIONING-CODE GREATER THAN '2'                      
00853          MOVE HIGH-VALUES        TO  EX-COMPANY-CD                
00854          GO TO 3200-PRINT-REPORT.                                 
00855                                                                   
00856      IF EX-EXTRACT-CODE LESS THAN 'D'                             
00857          GO TO 3100-PRINT-REPORT.                                 
00858                                                                   
00859      IF EX-EXTRACT-CODE GREATER THAN 'D'                          
00860          MOVE HIGH-VALUES        TO  EX-COMPANY-CD                
00861          GO TO 3200-PRINT-REPORT.                                 
00862                                                                   
00863  3200-PRINT-REPORT.                                               
00864      IF WS-D-RECORD-COUNT NOT GREATER THAN ZERO                   
00865        AND EX-COMPANY-CD = HIGH-VALUES                            
00866          MOVE '-***** NO REPORT RECORDS FOR THIS RUN *****'       
00867                                  TO  PRT                          
00868          PERFORM WRITE-A-LINE                                     
00869          GO TO 4900-EXIT.                                         
00870                                                                   
00871      ADD +1  TO  WS-D-RECORD-COUNT.                               
00872                                                                   
00873      IF FIRST-TIME                                                
00874          MOVE WS-CURRENT-DATE    TO  WS-H2-DATE                   
00875          PERFORM 5000-START-OF-REPORT                             
00876          MOVE SPACES             TO  WS-LAST-RECORD-TYPE          
00877          MOVE 'N'                TO  FIRST-TIME-SW                
00878          GO TO 3100-PRINT-REPORT.                                 
00879                                                                   
00880      IF WS-LAST-RECORD-TYPE = SPACES                              
00881          MOVE EX-RECORD-TYPE  TO  WS-LAST-RECORD-TYPE             
00882                                   WS-H1-REPORT-TYPE.              
00883                                                                   
00884      IF WS-LAST-COMPANY-CD  = EX-COMPANY-CD  AND                  
00885         WS-LAST-RECORD-TYPE = EX-RECORD-TYPE                      
00886           GO TO 3400-PRINT-REPORT.                                
00887                                                                   
00888      MOVE '-'                    TO  WS-TOTAL-LINE1.              
00889                                                                   
00890      IF WS-LAST-RECORD-TYPE = 'A'                                 
00891          MOVE 'PAYMENTS  FOR'    TO  WS-T1-DESCRIPTION            
00892          MOVE WS-PAYMENT-COUNT   TO  WS-T1-COUNT                  
00893          MOVE WS-PAYMENT-AMOUNT  TO  WS-T1-AMOUNT                 
00894          MOVE ZERO               TO  WS-PAYMENT-COUNT             
00895                                      WS-PAYMENT-AMOUNT            
00896        ELSE                                                       
00897      IF WS-LAST-RECORD-TYPE = 'B'                                 
00898          MOVE WS-LETTER-RESENT-COUNT  TO   WS-T1-COUNT            
00899          MOVE 'LETTERS ARE SCHEDULED TO BE RE-SENT'               
00900                                  TO  WS-T1-DESCRIPTION            
00901          MOVE ZERO               TO  WS-LETTER-RESENT-COUNT       
00902        ELSE                                                       
00903      IF WS-LAST-RECORD-TYPE = 'C'                                 
00904          MOVE WS-INACTIVE-CLAIM-COUNT TO  WS-T1-COUNT             
00905          MOVE 'CLAIMS HAVE BEEN INACTIVE FOR 45 DAYS OR MORE'     
00906                                  TO  WS-T1-DESCRIPTION            
00907          MOVE ZERO               TO  WS-INACTIVE-CLAIM-COUNT      
00908        ELSE                                                       
00909      IF WS-LAST-RECORD-TYPE = 'D'                                 
00910          MOVE WS-CLAIMS-CLOSED-COUNT  TO  WS-T1-COUNT             
00911          MOVE 'CLAIMS WERE FORCED CLOSED'                         
00912                                  TO  WS-T1-DESCRIPTION            
00913          MOVE WS-TOTAL-LINE1     TO  PRT                          
00914          PERFORM WRITE-A-LINE                                     
00915          MOVE WS-CLAIMS-PURGED-COUNT  TO  WS-T1-COUNT             
00916          MOVE 'CLAIMS WERE PURGED'                                
00917                                  TO  WS-T1-DESCRIPTION            
00918          MOVE ZERO               TO  WS-CLAIMS-CLOSED-COUNT       
00919                                      WS-CLAIMS-PURGED-COUNT       
00920        ELSE                                                       
00921      IF WS-LAST-RECORD-TYPE = 'E'                                 
00922          MOVE WS-CHECK-COUNT     TO  WS-E1-COUNT                  
00923          MOVE 'CHECKS ARE UNPROCESSED '                           
00924                                  TO  WS-E1-DESCRIPT1              
00925          MOVE 'FOR THE AMOUNT OF  '                               
00926                                  TO  WS-E1-DESCRIPT2              
00927          MOVE WS-CHECK-AMOUNT    TO  WS-E1-AMOUNT                 
00928          MOVE ZERO               TO  WS-CHECK-COUNT               
00929                                      WS-CHECK-AMOUNT              
00930        ELSE                                                       
00931      IF WS-LAST-RECORD-TYPE = 'F'                                 
00932          MOVE WS-CLAIMS-WITH-ERRORS TO  WS-T1-COUNT               
00933          MOVE 'CLAIMS HAVE XX,XXX- UNRESOLVED ERRORS'             
00934                                  TO  WS-T1-DESCRIPTION            
00935          MOVE WS-CLAIM-ERROR-COUNT  TO  WS-T1-ERRORS              
00936          MOVE ZERO               TO  WS-CLAIMS-WITH-ERRORS        
00937                                      WS-CLAIM-ERROR-COUNT         
00938        ELSE                                                       
091914     if ws-last-record-type = 'G'
091914        perform 4050-PRINT-G-AUTO-REPORT
091914                                 thru 4070-end-of-ga-report
091914        go to 3200-continue
091914       else
00939      IF WS-LAST-RECORD-TYPE = 'H'                                 
00940          MOVE WS-UNAPPROVE-COUNT TO  WS-H1-COUNT                  
00941          MOVE 'UNAPPROVED PAYMENTS '                              
00942                                  TO  WS-H1-DESCRIPT1              
00943          MOVE 'FOR THE AMOUNT OF  '                               
00944                                  TO  WS-H1-DESCRIPT2              
00945          MOVE WS-AMOUNT-PAID     TO  WS-H1-AMOUNT                 
00946          MOVE +0                 TO  WS-UNAPPROVE-COUNT           
00947                                      WS-AMOUNT-PAID               
00948        ELSE                                                       
00949      IF WS-LAST-RECORD-TYPE = 'I'                                 
00950          MOVE WS-LETTER-REC-COUNT TO  WS-T1-COUNT                 
00951          MOVE 'LETTERS RECEIVED      '                            
00952                                  TO  WS-T1-DESCRIPTION            
00953          MOVE +0                 TO  WS-LETTER-REC-COUNT          
00954        ELSE                                                       
00955      IF WS-LAST-RECORD-TYPE = 'J'                                 
00956          MOVE WS-LAG-REC-COUNT   TO  WS-T1-COUNT                  
00957          MOVE 'LAG RECORDS           '                            
00958                                  TO  WS-T1-DESCRIPTION            
00959          MOVE +0                 TO  WS-LAG-REC-COUNT             
00960                                                                   
00961        ELSE                                                       
00962      IF WS-LAST-RECORD-TYPE = 'K'                                 
00963          MOVE WS-LAPSE-REC-COUNT TO  WS-T1-COUNT                  
00964          MOVE 'LAPSE RECORDS         '                            
00965                                  TO  WS-T1-DESCRIPTION            
00966          MOVE +0                 TO  WS-LAPSE-REC-COUNT           
00967                                                                   
00968        ELSE                                                       
00969      IF WS-LAST-RECORD-TYPE = 'L'                                 
00970          MOVE WS-INCOMPLETE-REC-COUNT TO  WS-T1-COUNT             
00971          MOVE 'INCOMPLETE RECORDS    '                            
00972                                  TO  WS-T1-DESCRIPTION            
00973          MOVE +0                 TO  WS-INCOMPLETE-REC-COUNT.     
00974                                                                   
00975      IF WS-LAST-RECORD-TYPE = 'E'                                 
00976          MOVE WS-EL323E-TOT-LINE      TO  PRT                     
00977          PERFORM WRITE-A-LINE                                     
00978      ELSE                                                         
00979          IF WS-LAST-RECORD-TYPE = 'H'                             
00980              MOVE WS-EL323H-TOT-LINE  TO  PRT                     
00981              PERFORM WRITE-A-LINE                                 
00982          ELSE                                                     
00983              MOVE WS-TOTAL-LINE1     TO  PRT                      
00984              PERFORM WRITE-A-LINE.                                
091914 3200-continue.
00985                                                                   
00986      MOVE +99                    TO  WS-LINE-COUNT.               
00987      MOVE ZERO                   TO  WS-PAGE.                     
00988                                                                   
00989  3300-PRINT-REPORT.                                               
00990      MOVE EX-RECORD-TYPE         TO  WS-LAST-RECORD-TYPE          
00991                                      WS-H1-REPORT-TYPE.           
00992                                                                   
00993      EJECT                                                        
00994      IF EX-COMPANY-CD = WS-LAST-COMPANY-CD                        
00995          GO TO 3400-PRINT-REPORT.                                 
00996                                                                   
00997      IF WS-PRINT-OPTION = ('S' OR 'T')                            
00998        AND WS-LINE-NUMBER GREATER THAN ZERO                       
00999          NEXT SENTENCE                                            
01000        ELSE                                                       
01001          GO TO 3310-PRINT-REPORT.                                 
01002                                                                   
01003      MOVE WS-LAST-COMPANY-CD     TO  RF-COMPANY-CD.               
01004      MOVE '2'                    TO  RF-RECORD-TYPE.              
01005      MOVE 'EL323'                TO  RF-REPORT-ID.                
01006      ADD +1  TO  WS-LINE-NUMBER.                                  
01007      MOVE WS-LINE-NUMBER         TO  RF-LINE-NUMBER.              
01008                                                                   
01009      MOVE SPACES                 TO  RF-TRAILER-RECORD.           
01010      ACCEPT WS-TIME-OF-DAY FROM TIME.                             
01011      MOVE WS-TIME                TO  RF-PRINT-HH-MM-SS.           
01012      MOVE WS-CURRENT-DATE        TO  RF-CURRENT-DATE.             
01013                                                                   
01014      WRITE REPORT-SAVE-FILE.                                      
01015                                                                   
01016      IF ELREPT-FILE-STATUS NOT = ZERO                             
01017          MOVE 'ERROR OCCURED WRITE ELREPT TRAILER RECORD'         
01018                                  TO  WS-ABEND-MESSAGE             
01019          MOVE ELREPT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
01020          GO TO ABEND-PGM.                                         
01021                                                                   
01022  3310-PRINT-REPORT.                                               
01023      IF EX-COMPANY-CD = HIGH-VALUES                               
01024          GO TO 4900-EXIT.                                         
01025                                                                   
01026      PERFORM 5000-START-OF-REPORT.                                
01027                                                                   
01028      IF EX-RECORD-TYPE = SPACES                                   
01029          GO TO 3100-PRINT-REPORT.                                 
01030                                                                   
01031      EJECT                                                        
01032  3400-PRINT-REPORT.                                               
01033 *    NOTE ******************************************************* 
01034 *         *      PROCESS THE DETAIL RECORDS FOR THE AUTOMATIC   * 
01035 *         *  PAYMENTS GENERATED REPORT (EL323A).                * 
01036 *         *******************************************************.
01037                                                                   
01038      IF EX-RECORD-TYPE NOT = 'A'                                  
01039          GO TO 3500-PRINT-REPORT.                                 
01040                                                                   
01041      MOVE SPACES                 TO  WS-EL323A-DETAIL1.           
01042                                                                   
01043      MOVE EX-SF-CARRIER          TO  WS-A1-CARRIER.               
01044      MOVE EX-SF-CLAIM-NO         TO  WS-A1-CLAIM-NO.              
01045      MOVE EX-SF-CERT-NO          TO  WS-A1-CERT-NO.               
DAN        MOVE EX-DA-ACCT-NAME        TO  WS-A1-ACCT-NAME
DAN        MOVE EX-DA-INSURED-NAME     TO  WS-A1-INSURED-NAME
01046                                                                   
01047      IF EX-DA-ERROR-FLAG (4) = '*'                                
01048         MOVE 'TRYING TO PAY PAST CANCELLATION' TO                 
01049                      WS-A2-MESSAGE                                
01050         GO TO 3490-PRINT-REPORT.                                  
01051                                                                   
01052      IF EX-SF-DAYS-TO-PAYMENT GREATER THAN ZERO                   
01053          MOVE 'PAYMENT WILL BE GENERATED IN X DAYS FOR '          
01054                                     TO WS-A2-MESSAGE              
01055          MOVE EX-SF-DAYS-TO-PAYMENT TO WS-A2-DAYS                 
01056          MOVE EX-DA-INSURED-NAME    TO WS-A2-INSURED              
01057          GO TO 3490-PRINT-REPORT.                                 
01058                                                                   
01059      ADD +1  TO  WS-PAYMENT-COUNT.                                
01060                                                                   
01061      MOVE EX-DA-PAYMENT-AMOUNT   TO  WS-A1-PAYMENT-AMOUNT.        
01062      ADD EX-DA-PAYMENT-AMOUNT    TO  WS-PAYMENT-AMOUNT.           
01063                                                                   
01064      SET PI TO +1.                                                
01065                                                                   
01066      SEARCH WS-PAYMENT-TYPE-TABLE                                 
01067          AT END                                                   
01068              MOVE EX-DA-PAYMENT-TYPE  TO  WS-A1-PAYMENT-TYPE      
01069                                                                   
01070          WHEN EX-DA-PAYMENT-TYPE = WS-PAYMENT-TYPE (PI)           
01071              MOVE WS-PAYMENT-DESC (PI) TO WS-A1-PAYMENT-TYPE.     
01072                                                                   
01073      IF EX-DA-PAID-THRU-DT NOT = LOW-VALUES                       
01074         IF WS-PAID-THRU-TO-OPTION = ' '                           
01075            MOVE EX-DA-PAID-THRU-DT     TO  DC-BIN-DATE-1          
01076            MOVE SPACES                 TO  DC-OPTION-CODE         
01077            PERFORM 8500-DATE-CONVERSION                           
01078            MOVE DC-GREG-DATE-1-EDIT    TO  WS-A1-PAID-THRU        
01079         ELSE                                                      
01080            MOVE EX-DA-PAID-THRU-DT     TO  DC-BIN-DATE-1          
01081            MOVE '6'                    TO  DC-OPTION-CODE         
01082            MOVE +1                     TO  DC-ELAPSED-DAYS        
01083            MOVE +0                     TO  DC-ELAPSED-MONTHS      
01084            PERFORM 8500-DATE-CONVERSION                           
01085            MOVE DC-GREG-DATE-1-EDIT    TO  WS-A1-PAID-THRU.       
01086                                                                   
01087      MOVE EX-DA-CHECK-NUMBER     TO  WS-A1-CHECK-NO.              
01088                                                                   
01089      IF EX-DA-SCHEDULE-END-DT NOT = LOW-VALUES                    
01090        IF WS-PAID-THRU-TO-OPTION IS EQUAL TO ' '                  
01091          MOVE EX-DA-SCHEDULE-END-DT  TO  DC-BIN-DATE-1            
01092          MOVE SPACES                 TO  DC-OPTION-CODE           
01093          MOVE +0                     TO  DC-ELAPSED-DAYS          
01094                                          DC-ELAPSED-MONTHS        
01095          PERFORM 8500-DATE-CONVERSION                             
01096          IF NO-CONVERSION-ERROR                                   
01097            MOVE DC-GREG-DATE-1-EDIT  TO  WS-A1-LAST-PMT-SCHEDULED 
01098          ELSE                                                     
01099            MOVE SPACES               TO  WS-A1-LAST-PMT-SCHEDULED 
01100        ELSE                                                       
01101          MOVE EX-DA-SCHEDULE-END-DT  TO  DC-BIN-DATE-1            
01102          MOVE '6'                    TO  DC-OPTION-CODE           
01103          MOVE +1                     TO  DC-ELAPSED-DAYS          
01104          MOVE +0                     TO  DC-ELAPSED-MONTHS        
01105          PERFORM 8500-DATE-CONVERSION                             
01106          IF NO-CONVERSION-ERROR                                   
01107            MOVE DC-GREG-DATE-1-EDIT  TO  WS-A1-LAST-PMT-SCHEDULED 
01108          ELSE                                                     
01109            MOVE SPACES               TO  WS-A1-LAST-PMT-SCHEDULED.
01110                                                                   
01111      MOVE EX-DA-INTERVAL-MONTHS  TO  WS-A1-PAYMENT-INTERVAL.      
01112                                                                   
01113  3490-PRINT-REPORT.                                               
01114      MOVE WS-EL323A-DETAIL1      TO  PRT.                         
01115      PERFORM WRITE-A-LINE.                                        
01116                                                                   
01117      GO TO 3100-PRINT-REPORT.                                     
01118                                                                   
01119      EJECT                                                        
01120  3500-PRINT-REPORT.                                               
01121 *    NOTE ********************************************************
01122 *         * PROCESS THE DETAIL RECORDS FOR THE CORRESPONDENCE TO *
01123 *         * BE RE-SENT REPORT (EL323B).                          *
01124 *         ********************************************************
01125                                                                   
01126      IF EX-RECORD-TYPE NOT = 'B'                                  
01127          GO TO 3600-PRINT-REPORT.                                 
01128                                                                   
01129      ADD +1  TO  WS-LETTER-RESENT-COUNT.                          
01130                                                                   
01131      MOVE '0'                    TO  WS-EL323B-DETAIL1.           
01132                                                                   
01133      MOVE EX-SE-CARRIER          TO  WS-B1-CARRIER.               
01134      MOVE EX-SE-CLAIM-NO         TO  WS-B1-CLAIM-NO.              
01135      MOVE EX-SE-CERT-NO          TO  WS-B1-CERT-NO.               
01136                                                                   
01137      MOVE EX-DB-OVERRIDE-L2      TO WS-B1-CLAIM-TYPE.             
01138                                                                   
01139      IF EX-DB-LETTER-ARCHIVE-NO GREATER THAN ZERO                 
01140          MOVE EX-DB-STD-LETTER-FORM   TO WS-B1-FORM-TYPE          
01141          MOVE EX-DB-LETTER-ARCHIVE-NO TO WS-B1-ARCHIVE-NO         
01142        ELSE                                                       
01143          MOVE 'FORM'                  TO WS-B1-ARCHIVE-NO-X       
01144          IF EX-DB-STD-LETTER-FORM = '1'                           
01145              MOVE 'INIT'              TO WS-B1-FORM-TYPE          
01146            ELSE                                                   
01147              IF EX-DB-STD-LETTER-FORM = '2'                       
01148                  MOVE 'PROG'          TO WS-B1-FORM-TYPE          
01149                ELSE                                               
01150                  MOVE EX-DB-STD-LETTER-FORM TO WS-B1-FORM-TYPE.   
01151                                                                   
01152      IF EX-DB-LETTER-SENT-DT NOT = LOW-VALUES                     
01153          MOVE EX-DB-LETTER-SENT-DT   TO  DC-BIN-DATE-1            
01154          MOVE SPACES                 TO  DC-OPTION-CODE           
01155          PERFORM 8500-DATE-CONVERSION                             
01156          MOVE DC-GREG-DATE-1-EDIT    TO  WS-B1-ORIG-SEND-DATE     
01157      ELSE                                                         
01158         MOVE '00/00/00'          TO WS-B1-ORIG-SEND-DATE.         
01159                                                                   
01160      IF EX-DB-AUTO-RE-SEND-DT NOT = LOW-VALUES                    
01161          MOVE EX-DB-AUTO-RE-SEND-DT  TO  DC-BIN-DATE-1            
01162          MOVE SPACES                 TO  DC-OPTION-CODE           
01163          PERFORM 8500-DATE-CONVERSION                             
01164          MOVE DC-GREG-DATE-1-EDIT TO WS-B1-SCHEDULED-RESEND-DATE. 
01165                                                                   
01166      IF EX-DB-INITIAL-PRINT-DT = LOW-VALUES                       
01167         MOVE '*'                 TO WS-B1-NOT-PRINTED-FLAG.       
01168                                                                   
01169      MOVE EX-DB-RECORDED-BY      TO  WS-B1-BY.                    
01170                                                                   
01171      SET AI TO +1.                                                
01172                                                                   
01173      SEARCH WS-ADDRESSEE-TYPE-TABLE                               
01174          AT END                                                   
01175              MOVE EX-DB-ADDRESEE-TYPE  TO  WS-B1-ADDRESSEE-TYPE   
01176                                                                   
01177          WHEN EX-DB-ADDRESEE-TYPE = WS-ADDRESSEE-TYPE (AI)        
01178              MOVE WS-ADDRESSEE-DESC (AI) TO WS-B1-ADDRESSEE-TYPE. 
01179                                                                   
01180      MOVE WS-EL323B-DETAIL1      TO  PRT.                         
01181      PERFORM WRITE-A-LINE.                                        
01182                                                                   
01183      IF EX-DB-REASON = SPACES                                     
01184          GO TO 3100-PRINT-REPORT.                                 
01185                                                                   
01186      MOVE SPACES                 TO  WS-EL323B-DETAIL2.           
01187      MOVE 'RE-'                  TO  WS-B2-DESCRIPTION.           
01188      MOVE EX-DB-REASON           TO  WS-B2-REASON.                
01189                                                                   
01190      MOVE WS-EL323B-DETAIL2      TO  PRT.                         
01191      PERFORM WRITE-A-LINE.                                        
01192                                                                   
01193      GO TO 3100-PRINT-REPORT.                                     
01194                                                                   
01195      EJECT                                                        
01196  3600-PRINT-REPORT.                                               
01197 *    NOTE ******************************************************* 
01198 *         *      PROCESS THE DETAIL RECORDS FOR THE CLAIMS WITH * 
01199 *         *  NO ACTIVITY FOR 30 DAYS OR MORE REPORT (EL323C)    * 
01200 *         *******************************************************.
01201                                                                   
01202      IF EX-RECORD-TYPE NOT = 'C'                                  
01203          GO TO 3700-PRINT-REPORT.                                 
01204                                                                   
01205      ADD +1  TO  WS-INACTIVE-CLAIM-COUNT.                         
01206                                                                   
01207      MOVE SPACES                 TO  WS-EL323C-DETAIL1.           
01208                                                                   
01209      IF EX-COMPANY-ID = 'DMD'                                     
01210          IF FIRST-EXTRACT-C                                       
01211              MOVE 'N'            TO  FIRST-EXTRACT-C-SW           
01212              MOVE EX-DC-RECORDED-BY                               
01213                                  TO  WS-PREV-PROCESSOR            
01214          ELSE                                                     
01215              IF EX-DC-RECORDED-BY NOT = WS-PREV-PROCESSOR         
01216                  PERFORM WRITE-HEADINGS                           
01217                  MOVE EX-DC-RECORDED-BY                           
01218                                  TO  WS-PREV-PROCESSOR.           
01219                                                                   
01220      IF EX-COMPANY-ID = 'DMD'                                     
01221          MOVE EX-SC2-CARRIER     TO  WS-C1-CARRIER                
01222          MOVE EX-SC2-CLAIM-NO    TO  WS-C1-CLAIM-NO               
01223          MOVE EX-SC2-CERT-NO     TO  WS-C1-CERT-NO                
01224          MOVE EX-DC-CREDIT-CARD-NO                                
01225                                  TO  WS-C1-CCN                    
01226          MOVE EX-DC-INSURED-LAST-NAME                             
01227                                  TO  WS-C1-LAST                   
01228          MOVE EX-DC-INSURED-FIRST-NAME                            
01229                                  TO  WS-C1-FIRST                  
01230          MOVE EX-DC-INSURED-MID-INIT                              
01231                                  TO  WS-C1-MID                    
01232      ELSE                                                         
01233          MOVE EX-SE-CARRIER      TO  WS-C1-CARRIER                
01234          MOVE EX-SE-CLAIM-NO     TO  WS-C1-CLAIM-NO               
01235          MOVE EX-SE-CERT-NO      TO  WS-C1-CERT-NO.               
01236                                                                   
01237      MOVE EX-DC-OVERRIDE-L2      TO WS-C1-CLAIM-TYPE.             
01238                                                                   
01239      IF EX-DC-INCURRED-DT NOT = LOW-VALUES                        
01240          MOVE EX-DC-INCURRED-DT      TO  DC-BIN-DATE-1            
01241          MOVE SPACES                 TO  DC-OPTION-CODE           
01242          PERFORM 8500-DATE-CONVERSION                             
01243          MOVE DC-GREG-DATE-1-EDIT    TO  WS-C1-INCURRED-DATE.     
01244                                                                   
01245      IF EX-DC-FILE-ESTABLISH-DT NOT = LOW-VALUES                  
01246          MOVE EX-DC-FILE-ESTABLISH-DT TO DC-BIN-DATE-1            
01247          MOVE SPACES                 TO  DC-OPTION-CODE           
01248          PERFORM 8500-DATE-CONVERSION                             
01249          MOVE DC-GREG-DATE-1-EDIT    TO  WS-C1-ESTABLISHED-DATE.  
01250                                                                   
01251      IF EX-DC-PAID-THRU-DT NOT = LOW-VALUES                       
01252         IF WS-PAID-THRU-TO-OPTION = ' '                           
01253            MOVE EX-DC-PAID-THRU-DT     TO  DC-BIN-DATE-1          
01254            MOVE SPACES                 TO  DC-OPTION-CODE         
01255            PERFORM 8500-DATE-CONVERSION                           
01256            MOVE DC-GREG-DATE-1-EDIT    TO  WS-C1-PAID-THRU-DATE   
01257         ELSE                                                      
01258            MOVE EX-DC-PAID-THRU-DT     TO  DC-BIN-DATE-1          
01259            MOVE '6'                    TO  DC-OPTION-CODE         
01260            MOVE +1                     TO  DC-ELAPSED-DAYS        
01261            MOVE +0                     TO  DC-ELAPSED-MONTHS      
01262            PERFORM 8500-DATE-CONVERSION                           
01263            MOVE DC-GREG-DATE-1-EDIT    TO  WS-C1-PAID-THRU-DATE.  
01264                                                                   
01265      IF EX-DC-LAST-MAINT-DT NOT = LOW-VALUES                      
01266          MOVE EX-DC-LAST-MAINT-DT    TO  DC-BIN-DATE-1            
01267          MOVE SPACES                 TO  DC-OPTION-CODE           
01268          MOVE +0                     TO  DC-ELAPSED-DAYS          
01269                                          DC-ELAPSED-MONTHS        
01270          PERFORM 8500-DATE-CONVERSION                             
01271          MOVE DC-GREG-DATE-1-EDIT    TO  WS-C1-LAST-MAINT-DATE.   
01272                                                                   
01273      SET MI TO +1.                                                
01274                                                                   
01275      SEARCH WS-MAINTENANCE-TYPE-TABLE                             
01276          AT END                                                   
01277              MOVE EX-DC-MAINT-TYPE  TO  WS-C1-MAINT-TYPE          
01278                                                                   
01279          WHEN EX-DC-MAINT-TYPE = WS-MAINTENANCE-TYPE (MI)         
01280              MOVE WS-MAINTENANCE-DESC (MI) TO WS-C1-MAINT-TYPE.   
01281                                                                   
01282      MOVE EX-DC-RECORDED-BY      TO  WS-C1-BY.                    
01283                                                                   
01284      MOVE WS-EL323C-DETAIL1      TO  PRT.                         
01285      PERFORM WRITE-A-LINE.                                        
01286                                                                   
01287      GO TO 3100-PRINT-REPORT.                                     
01288                                                                   
01289      EJECT                                                        
01290  3700-PRINT-REPORT.                                               
01291 *    NOTE ******************************************************* 
01292 *         *      PROCESS THE DETAIL RECORDS FOR THE AUTOMATIC   * 
01293 *         *  CLAIM CLOSINGS REPORT (EL323D).                    * 
01294 *         *******************************************************.
01295                                                                   
01296      MOVE WS-REGULAR-ALPHA       TO  WS-H3-DATE.                  
01297                                                                   
01298      IF EX-RECORD-TYPE NOT = 'D'                                  
01299          GO TO 3800-PRINT-REPORT.                                 
01300                                                                   
01301      IF EX-DD-CLAIM-PURGED-SW = '1'                               
01302          ADD +1  TO  WS-CLAIMS-PURGED-COUNT                       
01303        ELSE                                                       
01304          ADD +1  TO  WS-CLAIMS-CLOSED-COUNT.                      
01305                                                                   
01306      MOVE SPACES                 TO  WS-EL323D-DETAIL1.           
01307                                                                   
01308      MOVE EX-SA-CARRIER          TO  WS-D1-CARRIER.               
01309      MOVE EX-SA-CLAIM-NO         TO  WS-D1-CLAIM-NO.              
01310      MOVE EX-SA-CERT-NO          TO  WS-D1-CERT-NO.               
01311                                                                   
01312      MOVE EX-DD-OVERRIDE-L2      TO WS-D1-CLAIM-TYPE.             
01313                                                                   
01314      IF EX-DD-INCURRED-DT NOT = LOW-VALUES                        
01315          MOVE EX-DD-INCURRED-DT      TO  DC-BIN-DATE-1            
01316          MOVE SPACES                 TO  DC-OPTION-CODE           
01317          PERFORM 8500-DATE-CONVERSION                             
01318          MOVE DC-GREG-DATE-1-EDIT    TO  WS-D1-INCURRED-DATE.     
01319                                                                   
01320      IF EX-DD-FILE-ESTABLISH-DT NOT = LOW-VALUES                  
01321          MOVE EX-DD-FILE-ESTABLISH-DT TO DC-BIN-DATE-1            
01322          MOVE SPACES                 TO  DC-OPTION-CODE           
01323          PERFORM 8500-DATE-CONVERSION                             
01324          MOVE DC-GREG-DATE-1-EDIT    TO  WS-D1-ESTABLISHED-DATE.  
01325                                                                   
01326      IF EX-COMPANY-ID = 'FIA'                                     
01327          MOVE '  INSURED'                TO FIA-HD6               
01328          MOVE EX-DD-INSURED-LAST-NAME    TO WS-D1-ESTABLISHED-DATE
01329      ELSE                                                         
01330          MOVE ' ESTABLISH'               TO FIA-HD6.              
01331                                                                   
01332      IF EX-DD-PAID-THRU-DT NOT = LOW-VALUES                       
01333         IF WS-PAID-THRU-TO-OPTION = ' '                           
01334            MOVE EX-DD-PAID-THRU-DT     TO  DC-BIN-DATE-1          
01335            MOVE SPACES                 TO  DC-OPTION-CODE         
01336            PERFORM 8500-DATE-CONVERSION                           
01337            MOVE DC-GREG-DATE-1-EDIT    TO  WS-D1-PAID-THRU-DATE   
01338         ELSE                                                      
01339            MOVE EX-DD-PAID-THRU-DT     TO  DC-BIN-DATE-1          
01340            MOVE '6'                    TO  DC-OPTION-CODE         
01341            MOVE +1                     TO DC-ELAPSED-DAYS         
01342            MOVE +0                     TO DC-ELAPSED-MONTHS       
01343            PERFORM 8500-DATE-CONVERSION                           
01344            MOVE DC-GREG-DATE-1-EDIT    TO  WS-D1-PAID-THRU-DATE.  
01345                                                                   
01346      IF EX-COMPANY-ID = 'FIA'                                     
01347          MOVE 'ACCOUNT'                 TO  FIA-HDG6A             
01348          MOVE SPACES                    TO  FIA-HD5               
01349          MOVE EX-DD-ACCOUNT-PRIME       TO  WS-D1-LAST-MAINT-DATE 
01350      ELSE                                                         
01351          MOVE 'LAST'                    TO  FIA-HD5               
01352          MOVE 'MAINT'                   TO  FIA-HDG6A             
01353          IF EX-DD-LAST-MAINT-DT NOT = LOW-VALUES                  
01354              MOVE EX-DD-LAST-MAINT-DT   TO  DC-BIN-DATE-1         
01355              MOVE SPACES                TO  DC-OPTION-CODE        
01356              MOVE +0                    TO  DC-ELAPSED-DAYS       
01357                                             DC-ELAPSED-MONTHS     
01358              PERFORM 8500-DATE-CONVERSION                         
01359              MOVE DC-GREG-DATE-1-EDIT   TO  WS-D1-LAST-MAINT-DATE.
01360                                                                   
01361      SET MI TO +1.                                                
01362                                                                   
01363      SEARCH WS-MAINTENANCE-TYPE-TABLE                             
01364          AT END                                                   
01365              MOVE EX-DD-MAINT-TYPE  TO  WS-D1-MAINT-TYPE          
01366                                                                   
01367          WHEN EX-DD-MAINT-TYPE = WS-MAINTENANCE-TYPE (MI)         
01368              MOVE WS-MAINTENANCE-DESC (MI) TO WS-D1-MAINT-TYPE.   
01369                                                                   
080510     IF EX-DD-DROP-CLAIM-SW = 'Y'
080510         MOVE '** DROP **' TO WS-D1-MAINT-TYPE
080510     END-IF.
080510
01370      IF EX-DD-CLAIM-PURGED-SW = '1'                               
01371          MOVE '*** PURGED ***'  TO  WS-D1-PURGED-MESSAGE.         
01372                                                                   
01373      MOVE WS-EL323D-DETAIL1      TO  PRT.                         
01374      PERFORM WRITE-A-LINE.                                        
01375                                                                   
01376      GO TO 3100-PRINT-REPORT.                                     
01377                                                                   
01378      EJECT                                                        
01379  3800-PRINT-REPORT.                                               
01380 *    NOTE ******************************************************* 
01381 *         *      PROCESS THE DETAIL RECORDS FOR THE UNPROCESSED * 
01382 *         *  CHECKS REPORT (EL323E).                            * 
01383 *         *******************************************************.
01384                                                                   
01385      IF EX-RECORD-TYPE NOT = 'E'                                  
01386          GO TO 3900-PRINT-REPORT.                                 
01387                                                                   
01388      ADD +1  TO  WS-CHECK-COUNT.                                  
01389                                                                   
01390      MOVE SPACES                 TO  WS-EL323E-DETAIL1.           
01391                                                                   
01392      MOVE EX-DE-CHECK-NO         TO  WS-E1-CHECK-NO.              
01393      MOVE EX-SA-CARRIER          TO  WS-E1-CARRIER.               
01394      MOVE EX-DE-CLAIM-NO         TO  WS-E1-CLAIM-NO.              
01395      MOVE EX-DE-CERT-NO          TO  WS-E1-CERT-NO.               
01396      MOVE EX-DE-CONTROL-NUMBER   TO  WS-E1-CONTROL-GROUP.         
01397      ADD  EX-DE-PAYMENT-AMOUNT   TO  WS-CHECK-AMOUNT.             
01398                                                                   
01399      SET PI TO +1.                                                
01400                                                                   
01401      SEARCH WS-PAYMENT-TYPE-TABLE                                 
01402          AT END                                                   
01403              MOVE EX-DE-PAYMENT-TYPE  TO  WS-E1-PAYMENT-TYPE      
01404                                                                   
01405          WHEN EX-DE-PAYMENT-TYPE = WS-PAYMENT-TYPE (PI)           
01406              MOVE WS-PAYMENT-DESC (PI) TO WS-E1-PAYMENT-TYPE.     
01407                                                                   
01408      IF EX-DE-CHECK-WRITTEN-DATE NOT = LOW-VALUES                 
01409          MOVE EX-DE-CHECK-WRITTEN-DATE TO  DC-BIN-DATE-1          
01410          MOVE SPACES                 TO  DC-OPTION-CODE           
01411          PERFORM 8500-DATE-CONVERSION                             
01412          MOVE DC-GREG-DATE-1-EDIT    TO  WS-E1-DATE-PAID.         
01413                                                                   
01414      IF EX-DE-CONTROL-NUMBER = ZERO                               
01415          MOVE 'NOT RELEASED'     TO  WS-E1-STATUS                 
01416        ELSE                                                       
01417      IF EX-DE-TIMES-PRINTED NOT GREATER THAN ZERO                 
01418          MOVE 'NOT PRINTED'      TO  WS-E1-STATUS.                
01419                                                                   
01420      MOVE WS-EL323E-DETAIL1      TO  PRT.                         
01421      PERFORM WRITE-A-LINE.                                        
01422                                                                   
01423      GO TO 3100-PRINT-REPORT.                                     
01424                                                                   
01425      EJECT                                                        
01426  3900-PRINT-REPORT.                                               
01427 *    NOTE ******************************************************* 
01428 *         *      PROCESS THE DETAIL RECORDS FOR THE CLAIMS WITH * 
01429 *         *  UNRESOLVED ERRORS REPORT (EL323F).                 * 
01430 *         *******************************************************.
01431                                                                   
01432      IF EX-RECORD-TYPE NOT = 'F'                                  
01433          GO TO 4000-PRINT-REPORT.                                 
01434                                                                   
01435      ADD +1  TO  WS-CLAIMS-WITH-ERRORS.                           
01436                                                                   
01437      MOVE SPACES                 TO  WS-EL323F-DETAIL1.           
01438                                                                   
01439      MOVE EX-SA-CARRIER          TO  WS-F1-CARRIER.               
01440      MOVE EX-SA-CLAIM-NO         TO  WS-F1-CLAIM-NO.              
01441      MOVE EX-SA-CERT-NO          TO  WS-F1-CERT-NO.               
01442                                                                   
01443      MOVE EX-DF-OVERRIDE-L2      TO WS-F1-CLAIM-TYPE.             
01444                                                                   
01445      IF EX-DF-FILE-ESTABLISH-DT NOT = LOW-VALUES                  
01446          MOVE EX-DF-FILE-ESTABLISH-DT TO DC-BIN-DATE-1            
01447          MOVE SPACES                 TO  DC-OPTION-CODE           
01448          PERFORM 8500-DATE-CONVERSION                             
01449          MOVE DC-GREG-DATE-1-EDIT    TO  WS-F1-ESTABLISHED-DATE.  
01450                                                                   
01451      IF EX-DF-LAST-MAINT-DT NOT = LOW-VALUES                      
01452          MOVE EX-DF-LAST-MAINT-DT    TO  DC-BIN-DATE-1            
01453          MOVE SPACES                 TO  DC-OPTION-CODE           
01454          PERFORM 8500-DATE-CONVERSION                             
01455          MOVE DC-GREG-DATE-1-EDIT    TO  WS-F1-LAST-MAINT-DATE.   
01456                                                                   
01457      SET MI TO +1.                                                
01458                                                                   
01459      SEARCH WS-MAINTENANCE-TYPE-TABLE                             
01460          AT END                                                   
01461              MOVE EX-DF-MAINT-TYPE  TO  WS-F1-MAINT-TYPE          
01462                                                                   
01463          WHEN EX-DF-MAINT-TYPE = WS-MAINTENANCE-TYPE (MI)         
01464              MOVE WS-MAINTENANCE-DESC (MI) TO WS-F1-MAINT-TYPE.   
01465                                                                   
01466      MOVE EX-DF-RECORDED-BY      TO  WS-F1-BY.                    
01467                                                                   
01468      MOVE EX-DF-FATAL-ERROR-CNT  TO  WS-F1-FATAL-ERROR.           
01469      ADD EX-DF-FATAL-ERROR-CNT  TO  WS-CLAIM-ERROR-COUNT.         
01470                                                                   
01471      MOVE EX-DF-FORCABLE-ERROR-CNT  TO  WS-F1-FORCE-ERROR.        
01472      ADD EX-DF-FORCABLE-ERROR-CNT  TO  WS-CLAIM-ERROR-COUNT.      
01473                                                                   
01474      MOVE WS-EL323F-DETAIL1      TO  PRT.                         
01475      PERFORM WRITE-A-LINE.                                        
01476                                                                   
01477      GO TO 3100-PRINT-REPORT.                                     
01478                                                                   
01479      EJECT                                                        
01480  4000-PRINT-REPORT.                                               
01481 *    NOTE ******************************************************* 
01482 *         *      PROCESS THE DETAIL RECORDS FOR THE AUTOMATIC   * 
01483 *         *  PROMPT REQUESTS REPORT (EL323G).                   * 
01484 *         *******************************************************.
01485                                                                   
01486      IF EX-RECORD-TYPE NOT = 'G'                                  
021113*        GO TO 4100-PRINT-REPORT.                                 
021113         GO TO 4050-PRINT-G-AUTO-REPORT
021113     END-IF.
01488                                                                   
01489      MOVE '0'                    TO  WS-EL323G-DETAIL1.           
01490                                                                   
01491      IF EX-COMPANY-ID = 'UFL' OR 'UFR' OR 'WFL' OR 'WSL'          
01492          MOVE EX-SE2-ACTION-DATE     TO  DC-BIN-DATE-1            
01493          MOVE SPACES                 TO  DC-OPTION-CODE           
01494          PERFORM 8500-DATE-CONVERSION                             
01495          MOVE DC-GREG-DATE-1-EDIT    TO  WS-G1-ACTION-DATE        
01496          MOVE EX-DG-RECORDED-BY      TO  WS-G1-BY                 
01497          MOVE EX-SE2-CARRIER         TO  WS-G1-CARRIER            
01498          MOVE EX-SE2-CLAIM-NO        TO  WS-G1-CLAIM-NO           
01499          MOVE EX-SE2-CERT-NO         TO  WS-G1-CERT-NO            
01500        ELSE                                                       
01501          MOVE EX-SE3-ACTION-DATE      TO  DC-BIN-DATE-1           
01502          MOVE SPACES                  TO  DC-OPTION-CODE          
01503          PERFORM 8500-DATE-CONVERSION                             
01504          MOVE DC-GREG-DATE-1-EDIT     TO  WS-G1-ACTION-DATE       
01505          MOVE EX-DG-RECORDED-BY       TO  WS-G1-BY                
01506          MOVE EX-SE3-CARRIER          TO  WS-G1-CARRIER           
01507          MOVE EX-SE3-CLAIM-NO         TO  WS-G1-CLAIM-NO          
01508          MOVE EX-SE3-CERT-NO          TO  WS-G1-CERT-NO.          
01509                                                                   
01510      MOVE EX-DG-OVERRIDE-L2      TO WS-G1-CLAIM-TYPE.             
01511                                                                   
01512      IF EX-DG-INCURRED-DT NOT = LOW-VALUES                        
01513          MOVE EX-DG-INCURRED-DT      TO  DC-BIN-DATE-1            
01514          MOVE SPACES                 TO  DC-OPTION-CODE           
01515          PERFORM 8500-DATE-CONVERSION                             
01516          MOVE DC-GREG-DATE-1-EDIT    TO  WS-G1-INCURRED-DATE.     
01517                                                                   
01518      MOVE EX-DG-INSURED-LAST-NAME TO  WS-G1-INSURED-NAME.         
01519      MOVE EX-DG-FILE-LOCATION    TO  WS-G1-FILE-AT.               
01520                                                                   
01521      MOVE WS-EL323G-DETAIL1      TO  PRT.                         
01522      PERFORM WRITE-A-LINE.                                        
01523                                                                   
080510     IF EX-DG-DROP-CLAIM-SW = 'Y'
080510         MOVE SPACES             TO  WS-EL323G-DETAIL2
080510         MOVE '*** DROP ***'     TO  WS-G2-MESSAGE
080510         MOVE WS-EL323G-DETAIL2  TO  PRT
080510         PERFORM WRITE-A-LINE
080510     ELSE
01524      IF EX-DG-TEXT-LINE-1 NOT = SPACES                            
01525          MOVE SPACES             TO  WS-EL323G-DETAIL2            
01526          MOVE EX-DG-TEXT-LINE-1  TO  WS-G2-MESSAGE                
01527          MOVE WS-EL323G-DETAIL2  TO  PRT                          
01528          PERFORM WRITE-A-LINE.                                    
01529                                                                   
01530      IF EX-DG-TEXT-LINE-2 NOT = SPACES                            
01531          MOVE SPACES             TO  WS-EL323G-DETAIL2            
01532          MOVE EX-DG-TEXT-LINE-2  TO  WS-G2-MESSAGE                
01533          MOVE WS-EL323G-DETAIL2  TO  PRT                          
01534          PERFORM WRITE-A-LINE.                                    
021113
021113     IF WS-RPT-G-AUTO-OPEN = ZERO
021113         OPEN OUTPUT RPT-G-AUTO
021113         MOVE +1    TO WS-RPT-G-AUTO-OPEN
021113     END-IF
021113
050619*    IF EX-DG-TEXT-LINE-1 NOT = SPACES
050619*       PERFORM VARYING SUB1 FROM 1 BY 1
050619*                UNTIL SUB1 > 67 
050619*           IF EX-DG-TEXT-LINE-1 (SUB1:4) = 'AUTO' OR 'auto'
050619*               WRITE RPT-G-RECORD FROM REPORTS-EXTRACT-RECORD
050619*               ADD +1 TO WS-AUTO-RECS
050619*               MOVE +78 TO SUB1
050619*           END-IF
050619*       END-PERFORM
050619*    END-IF.
050619*
050619*    IF EX-DG-TEXT-LINE-2 NOT = SPACES
050619*       PERFORM VARYING SUB1 FROM 1 BY 1
050619*                UNTIL SUB1 > 77 
050619*           IF EX-DG-TEXT-LINE-2 (SUB1:4) = 'AUTO' OR 'auto'
050619*               WRITE RPT-G-RECORD FROM REPORTS-EXTRACT-RECORD
050619*               ADD +1 TO WS-AUTO-RECS
050619*               MOVE +78 TO SUB1
050619*           END-IF
050619*       END-PERFORM
050619*    END-IF.
01535                                                                   
01536      GO TO 3100-PRINT-REPORT.                                     
01537                                                                   
01538      EJECT                                                        
021113 4050-PRINT-G-AUTO-REPORT.
021113     IF WS-AUTO-RECS = 0
091914*        GO TO 4100-PRINT-REPORT
091914         GO TO 4070-end-of-ga-report
021113     END-IF.
021113
021113     MOVE 'G'                    TO  WS-H1-REPORT-TYPE.
021113     MOVE 'A'                    TO  WS-HI-AUDITOR-RPT.
021113     MOVE +99                    TO  WS-LINE-COUNT.
021113     MOVE ZERO                   TO  WS-PAGE.
021113
021113     CLOSE RPT-G-AUTO.
021113     SORT SORT-G-AUTO ON ASCENDING SORT-G-REC-USER
021113                                   SORT-G-REC-KEY
021113          USING RPT-G-AUTO GIVING RPT-G-AUTO.
021113     OPEN INPUT RPT-G-AUTO.
021113
021113 4050-RPT-LOOP.
021113
021113     READ RPT-G-AUTO INTO WS-G-EXTRACT-REC
021113          AT END
021113              MOVE +0 TO WS-AUTO-RECS 
021113              MOVE EX-RECORD-TYPE TO  WS-H1-REPORT-TYPE
021113              MOVE SPACES         TO  WS-HI-AUDITOR-RPT
021113              MOVE +99            TO  WS-LINE-COUNT
021113              MOVE ZERO           TO  WS-PAGE
021113              CLOSE RPT-G-AUTO
021113              MOVE ZERO           TO  WS-RPT-G-AUTO-OPEN
091914              go to 4070-end-of-ga-report
091914*             GO TO 4100-PRINT-REPORT.
021113
021113     IF WS-G-DG-RECORDED-BY NOT = WS-LAST-RECORDED-BY  AND
021113        WS-LAST-RECORDED-BY NOT = SPACES
021113          MOVE +99                TO  WS-LINE-COUNT
021113          MOVE ZERO               TO  WS-PAGE
021113     END-IF.
021113
021113     MOVE '0'                     TO  WS-EL323G-DETAIL1
021113
021113     MOVE WS-G-SE3-ACTION-DATE    TO  DC-BIN-DATE-1
021113     MOVE SPACES                  TO  DC-OPTION-CODE
021113     PERFORM 8500-DATE-CONVERSION
021113     MOVE DC-GREG-DATE-1-EDIT     TO  WS-G1-ACTION-DATE
021113     MOVE WS-G-DG-RECORDED-BY     TO  WS-G1-BY
021113                                      WS-LAST-RECORDED-BY
021113     MOVE WS-G-SE3-CARRIER        TO  WS-G1-CARRIER
021113     MOVE WS-G-SE3-CLAIM-NO       TO  WS-G1-CLAIM-NO
021113     MOVE WS-G-SE3-CERT-NO        TO  WS-G1-CERT-NO
021113
021113     MOVE WS-G-DG-OVERRIDE-L2     TO WS-G1-CLAIM-TYPE.
021113
021113     IF WS-G-DG-INCURRED-DT NOT = LOW-VALUES
021113         MOVE WS-G-DG-INCURRED-DT    TO  DC-BIN-DATE-1
021113         MOVE SPACES                 TO  DC-OPTION-CODE
021113         PERFORM 8500-DATE-CONVERSION
021113         MOVE DC-GREG-DATE-1-EDIT    TO  WS-G1-INCURRED-DATE.
021113
021113     MOVE WS-G-DG-INSURED-LAST-NAME TO  WS-G1-INSURED-NAME.
021113     MOVE WS-G-DG-FILE-LOCATION  TO  WS-G1-FILE-AT.
021113
021113     MOVE WS-EL323G-DETAIL1    TO  PRT.
021113     PERFORM WRITE-A-LINE.
021113
021113     IF WS-G-DG-DROP-CLAIM-SW = 'Y'
021113         MOVE SPACES             TO  WS-EL323G-DETAIL2
021113         MOVE '*** DROP ***'     TO  WS-G2-MESSAGE
021113         MOVE WS-EL323G-DETAIL2  TO  PRT
021113         PERFORM WRITE-A-LINE
021113     ELSE
021113     IF WS-G-DG-TEXT-LINE-1 NOT = SPACES
021113         MOVE SPACES             TO  WS-EL323G-DETAIL2
021113         MOVE WS-G-DG-TEXT-LINE-1 TO  WS-G2-MESSAGE
021113         MOVE WS-EL323G-DETAIL2  TO  PRT
021113         PERFORM WRITE-A-LINE.
021113
021113     IF WS-G-DG-TEXT-LINE-2 NOT = SPACES
021113         MOVE SPACES             TO  WS-EL323G-DETAIL2
021113         MOVE WS-G-DG-TEXT-LINE-2 TO  WS-G2-MESSAGE
021113         MOVE WS-EL323G-DETAIL2  TO  PRT
021113         PERFORM WRITE-A-LINE.
021113
021113     GO TO 4050-RPT-LOOP.
021113
091914 4070-end-of-ga-report.

01539  4100-PRINT-REPORT.                                               
01540 *    NOTE ******************************************************* 
01541 *         *      PROCESS THE DETAIL RECORDS FOR THE UNAPPROVED  * 
01542 *         *  PAYMNENT REPORT (EL323H)                           * 
01543 *         *******************************************************.
01544                                                                   
01545      IF EX-RECORD-TYPE NOT = 'H'                                  
01546          GO TO 4150-PRINT-REPORT.                                 
01547                                                                   
01548      ADD +1  TO  WS-UNAPPROVE-COUNT.                              
01549                                                                   
01550      MOVE SPACES                 TO  WS-EL323H-DETAIL1.           
01551                                                                   
01552      MOVE EX-SA-CARRIER          TO  WS-H1-CARRIER.               
01553      MOVE EX-SA-CLAIM-NO         TO  WS-H1-CLAIM-NO.              
01554      MOVE EX-SA-CERT-NO          TO  WS-H1-CERT-NO.               
01555      MOVE EX-DH-TRLR-SEQ-NO      TO  WS-H1-TRLR-SEQ-NO.           
01556      MOVE EX-DH-PAYMENT-AMOUNT   TO  WS-H1-AMOUNT-PAID.           
01557      ADD  EX-DH-PAYMENT-AMOUNT   TO  WS-AMOUNT-PAID.              
01558      MOVE EX-DH-CHECK-BY-USER    TO  WS-H1-USER.                  
01559                                                                   
01560      SET PI TO +1.                                                
01561                                                                   
01562      SEARCH WS-PAYMENT-TYPE-TABLE                                 
01563          AT END                                                   
01564              MOVE EX-DH-PAYMENT-TYPE  TO  WS-H1-PAYMENT-TYPE      
01565                                                                   
01566          WHEN EX-DH-PAYMENT-TYPE = WS-PAYMENT-TYPE (PI)           
01567              MOVE WS-PAYMENT-DESC (PI) TO WS-H1-PAYMENT-TYPE.     
01568                                                                   
01569      IF EX-DH-CHECK-WRITTEN-DATE NOT = LOW-VALUES                 
01570          MOVE EX-DH-CHECK-WRITTEN-DATE TO  DC-BIN-DATE-1          
01571          MOVE SPACES                 TO  DC-OPTION-CODE           
01572          PERFORM 8500-DATE-CONVERSION                             
01573          MOVE DC-GREG-DATE-1-EDIT    TO  WS-H1-DATE-PAID.         
01574                                                                   
01575      MOVE WS-EL323H-DETAIL1      TO  PRT.                         
01576      PERFORM WRITE-A-LINE.                                        
01577                                                                   
01578      GO TO 3100-PRINT-REPORT.                                     
01579                                                                   
01580      EJECT                                                        
01581  4150-PRINT-REPORT.                                               
01582 *    NOTE ******************************************************* 
01583 *         *      PROCESS THE DETAIL RECORDS FOR THE UNAPPROVED  * 
01584 *         *  PAYMNENT REPORT (EL323I)                           * 
01585 *         *******************************************************.
01586                                                                   
01587      IF EX-RECORD-TYPE NOT = 'I'                                  
01588          GO TO 4200-PRINT-REPORT.                                 
01589                                                                   
01590      ADD +1  TO WS-LETTER-REC-COUNT.                              
01591                                                                   
01592      MOVE SPACES                 TO  WS-EL323I-DETAIL1.           
01593                                                                   
01594      MOVE EX-SE-CARRIER          TO  WS-I1-CARRIER.               
01595      MOVE EX-SE-CLAIM-NO         TO  WS-I1-CLAIM-NO.              
01596      MOVE EX-SE-CERT-NO          TO  WS-I1-CERT-NO.               
01597      MOVE EX-DI-INSURED-LAST-NAME TO WS-I1-INSURED-LAST-NAME.     
01598      MOVE EX-DI-STD-LETTER-FORM  TO  WS-I1-FORM.                  
01599                                                                   
01600      IF EX-DI-LETTER-ARCHIVE-NO LESS THAN +0                      
01601         IF EX-DI-STD-LETTER-FORM = '1'                            
01602            MOVE 'INITIAL'        TO WS-I1-ARCHIVE-NUMBER-X        
01603         ELSE                                                      
01604            MOVE 'PROGRESS'       TO WS-I1-ARCHIVE-NUMBER-X        
01605      ELSE                                                         
01606         MOVE EX-DI-LETTER-ARCHIVE-NO TO WS-I1-ARCHIVE-NUMBER.     
01607                                                                   
01608      IF EX-DI-LETTER-ARCHIVE-NO = +0                              
01609         IF EX-DI-CORR-SOL-UNSOL = 'U'                             
01610            MOVE 'UNSOLICT'          TO WS-I1-ARCHIVE-NUMBER-X.    
01611                                                                   
01612      IF EX-DI-LETTER-SENT-DT NOT = LOW-VALUES                     
01613         MOVE EX-DI-LETTER-SENT-DT   TO  DC-BIN-DATE-1             
01614         MOVE SPACES                 TO  DC-OPTION-CODE            
01615         PERFORM 8500-DATE-CONVERSION                              
01616         MOVE DC-GREG-DATE-1-EDIT    TO  WS-I1-SEND-DATE.          
01617                                                                   
01618      IF EX-DI-INITIAL-PRINT-DT NOT = LOW-VALUES                   
01619         MOVE EX-DI-INITIAL-PRINT-DT TO  DC-BIN-DATE-1             
01620         MOVE SPACES                 TO  DC-OPTION-CODE            
01621         PERFORM 8500-DATE-CONVERSION                              
01622         MOVE DC-GREG-DATE-1-EDIT    TO  WS-I1-INITIAL-PRINT.      
01623                                                                   
01624      IF EX-DI-LETTER-ANSWERED-DT NOT = LOW-VALUES                 
01625         MOVE EX-DI-LETTER-ANSWERED-DT TO  DC-BIN-DATE-1           
01626         MOVE SPACES                 TO  DC-OPTION-CODE            
01627         PERFORM 8500-DATE-CONVERSION                              
01628         MOVE DC-GREG-DATE-1-EDIT    TO  WS-I1-ANSWER-RECEIVED.    
01629                                                                   
01630      MOVE WS-EL323I-DETAIL1      TO  PRT.                         
01631      PERFORM WRITE-A-LINE.                                        
01632                                                                   
01633      GO TO 3100-PRINT-REPORT.                                     
01634                                                                   
01635      EJECT                                                        
01636  4200-PRINT-REPORT.                                               
01637 *    NOTE ******************************************************* 
01638 *         *      PROCESS THE DETAIL RECORDS FOR THE LAG REPORT  * 
01639 *         *      (EL323J)                                       * 
01640 *         *******************************************************.
01641                                                                   
01642      IF EX-RECORD-TYPE NOT = 'J'                                  
01643          GO TO 4250-PRINT-REPORT.                                 
01644                                                                   
01645      ADD +1  TO WS-LAG-REC-COUNT.                                 
01646                                                                   
01647      MOVE SPACES                 TO  WS-EL323J-DETAIL1.           
01648                                                                   
01649      MOVE EX-DJ-ACTIVITY-CODE    TO  WS-J1-ACTION                 
01650      MOVE EX-SH-LAG-DAYS         TO  WS-J1-LAG-DAYS               
01651      MOVE EX-DJ-CARRIER          TO  WS-J1-CARRIER                
01652      MOVE EX-DJ-STATE            TO  WS-J1-STATE                  
01653      MOVE EX-SH-CLAIM-NO         TO  WS-J1-CLAIM-NO.              
01654      MOVE EX-DJ-CERT-NO          TO  WS-J1-CERT-NO.               
01655      MOVE EX-DJ-INSURED-NAME     TO WS-J1-INSURED-LAST-NAME.      
01656                                                                   
01657                                                                   
01658      IF EX-DJ-LAST-DATE NOT EQUAL LOW-VALUES                      
01659         MOVE EX-DJ-LAST-DATE        TO  DC-BIN-DATE-1             
01660         MOVE SPACES                 TO  DC-OPTION-CODE            
01661         PERFORM 8500-DATE-CONVERSION                              
01662         MOVE DC-GREG-DATE-1-EDIT    TO  WS-J1-LAST-DATE.          
01663                                                                   
01664      MOVE WS-EL323J-DETAIL1      TO  PRT.                         
01665      PERFORM WRITE-A-LINE.                                        
01666                                                                   
01667      GO TO 3100-PRINT-REPORT.                                     
01668                                                                   
01669      EJECT                                                        
01670  4250-PRINT-REPORT.                                               
01671 *    NOTE ******************************************************* 
01672 *         *     PROCESS THE DETAIL RECORDS FOR THE LAPSE REPORT * 
01673 *         *     (EL323K)                                        * 
01674 *         *******************************************************.
01675                                                                   
01676      IF EX-RECORD-TYPE NOT EQUAL 'K'                              
01677          GO TO 4275-PRINT-REPORT.                                 
01678                                                                   
01679      ADD +1  TO WS-LAPSE-REC-COUNT.                               
01680                                                                   
01681      MOVE SPACES                 TO  WS-EL323J-DETAIL1.           
01682                                                                   
01683      IF EX-SI-PROCESSOR EQUAL WS-HOLD-K-PROCESSOR                 
01684         NEXT SENTENCE                                             
01685      ELSE                                                         
01686         MOVE EX-SI-PROCESSOR     TO WS-EL323K-PROCESSOR           
01687         PERFORM WRITE-HEADINGS.                                   
01688                                                                   
01689      MOVE EX-SI-PROCESSOR        TO  WS-HOLD-K-PROCESSOR          
01690      MOVE EX-SI-ACTIVITY-CODE    TO  WS-J1-ACTION                 
01691      MOVE EX-SI-DAYS             TO  WS-J1-LAG-DAYS               
01692      MOVE EX-DK-CARRIER          TO  WS-J1-CARRIER                
01693      MOVE EX-DK-STATE            TO  WS-J1-STATE                  
01694      MOVE EX-DK-CLAIM-NO         TO  WS-J1-CLAIM-NO.              
01695      MOVE EX-DK-CERT-NO          TO  WS-J1-CERT-NO.               
01696      MOVE EX-DK-INSURED-NAME     TO WS-J1-INSURED-LAST-NAME.      
01697                                                                   
01698                                                                   
01699      IF EX-DK-LAST-DATE NOT EQUAL LOW-VALUES                      
01700         MOVE EX-DK-LAST-DATE        TO  DC-BIN-DATE-1             
01701         MOVE SPACES                 TO  DC-OPTION-CODE            
01702         PERFORM 8500-DATE-CONVERSION                              
01703         MOVE DC-GREG-DATE-1-EDIT    TO  WS-J1-LAST-DATE.          
01704                                                                   
01705      MOVE WS-EL323J-DETAIL1      TO  PRT.                         
01706      PERFORM WRITE-A-LINE.                                        
01707                                                                   
01708      GO TO 3100-PRINT-REPORT.                                     
01709                                                                   
01710      EJECT                                                        
01711  4275-PRINT-REPORT.                                               
01712 *    NOTE ******************************************************* 
01713 *         *     PROCESS THE DETAIL RECORDS FOR THE INCOMPLETE   * 
01714 *         *     REPORT (EL323L)                                 * 
01715 *         *******************************************************.
01716                                                                   
01717      IF EX-RECORD-TYPE NOT EQUAL 'L'                              
01718          GO TO 4300-PRINT-REPORT.                                 
01719                                                                   
01720      ADD +1  TO WS-INCOMPLETE-REC-COUNT.                          
01721                                                                   
01722      MOVE SPACES                 TO  WS-EL323L-DETAIL1.           
01723                                                                   
01724      MOVE EX-DL-CARRIER          TO WS-L1-CARRIER                 
01725      MOVE EX-DL-CLAIM-NO         TO WS-L1-CLAIM-NO                
01726      MOVE EX-DL-FIRST-NAME       TO WS-L1-LAST-NAME               
01727                                                                   
01728      IF EX-DL-ESTABLISH-DT NOT EQUAL LOW-VALUES                   
01729         MOVE EX-DL-ESTABLISH-DT  TO DC-BIN-DATE-1                 
01730         MOVE ' '                 TO DC-OPTION-CODE                
01731         PERFORM 8500-DATE-CONVERSION                              
01732         MOVE DC-GREG-DATE-1-EDIT TO WS-L1-EST-DATE.               
01733                                                                   
01734      IF EX-DL-LAST-MAINT-DT NOT EQUAL LOW-VALUES                  
01735         MOVE EX-DL-LAST-MAINT-DT TO DC-BIN-DATE-1                 
01736         MOVE ' '                 TO DC-OPTION-CODE                
01737         PERFORM 8500-DATE-CONVERSION                              
01738         MOVE DC-GREG-DATE-1-EDIT TO WS-L1-MAINT-DATE.             
01739                                                                   
01740      MOVE EX-DL-PROCESSOR-ID     TO WS-L1-PROCESSOR-ID            
01741      IF EX-DL-LAST-MAINT-TYPE EQUAL ' '                           
01742         MOVE 'SET-UP'            TO WS-L1-MAINT-TYPE              
01743      ELSE                                                         
01744      IF EX-DL-LAST-MAINT-TYPE EQUAL '1'                           
01745         MOVE 'PAYMENT'           TO WS-L1-MAINT-TYPE              
01746      ELSE                                                         
01747      IF EX-DL-LAST-MAINT-TYPE EQUAL '2'                           
01748         MOVE 'LETTER'            TO WS-L1-MAINT-TYPE              
01749      ELSE                                                         
01750      IF EX-DL-LAST-MAINT-TYPE EQUAL '3'                           
01751         MOVE 'ALTERED'           TO WS-L1-MAINT-TYPE              
01752      ELSE                                                         
01753      IF EX-DL-LAST-MAINT-TYPE EQUAL '4'                           
01754         MOVE 'RESTORED'          TO WS-L1-MAINT-TYPE              
01755      ELSE                                                         
01756      IF EX-DL-LAST-MAINT-TYPE EQUAL '5'                           
01757         MOVE 'INC DATE'          TO WS-L1-MAINT-TYPE              
01758      ELSE                                                         
01759      IF EX-DL-LAST-MAINT-TYPE EQUAL '6'                           
01760         MOVE 'CONVERTED'         TO WS-L1-MAINT-TYPE.             
01761                                                                   
01762      MOVE WS-EL323L-DETAIL1      TO  PRT.                         
01763      PERFORM WRITE-A-LINE.                                        
01764                                                                   
01765      GO TO 3100-PRINT-REPORT.                                     
01766                                                                   
01767      EJECT                                                        
01768  4300-PRINT-REPORT.                                               
01769 *    NOTE ******************************************************* 
01770 *         *      IF YOU REACHED HERE YOU HAVE AN INVALID RECORD * 
01771 *         *  TYPE (NOT A-K)                                     * 
01772 *         *******************************************************.
01773                                                                   
01774      MOVE 'INVALID RECORD TYPE'  TO  WS-ABEND-MESSAGE.            
01775      GO TO ABEND-PGM.                                             
01776                                                                   
01777  4900-EXIT.                                                       
01778      EXIT.                                                        
01779                                                                   
01780      EJECT                                                        
01781  5000-START-OF-REPORT SECTION.                                    
01782      MOVE EX-COMPANY-CD          TO  WS-LAST-COMPANY-CD.          
01783                                                                   
01784      MOVE EX-DA-ALPHA-DATE       TO  WS-H3-DATE                   
01785                                      WS-REGULAR-ALPHA.            
01786      MOVE EX-DA-COMPANY-NAME     TO  WS-H2-CLIENT-NAME.           
01787                                                                   
01788      MOVE EX-DA-PRINT-OPTION     TO  WS-PRINT-OPTION.             
01789      MOVE EX-DA-FORMAT-OPTION    TO  WS-FORMAT-OPTION.            
01790      MOVE EX-DA-PROCESS-OPTION   TO  WS-PROCESS-OPTION.           
01791      MOVE EX-DA-TOTAL-OPTION     TO  WS-TOTAL-OPTION.             
01792                                                                   
01793      MOVE EX-DA-PAID-THRU-TO     TO  WS-PAID-THRU-TO-OPTION.      
01794                                                                   
01795      IF EX-DA-PAID-THRU-TO = '1'                                  
01796         MOVE ' TO'       TO EL323A-HDG                            
01797                             EL323C-HDG                            
01798                             EL323D-HDG.                           
01799                                                                   
01800      IF EX-COMPANY-ID NOT = 'DMD'                                 
01801         MOVE SPACES              TO  WS-EL323C-DMD5               
01802                                      WS-EL323C-DMD6.              
01803                                                                   
01804      IF WS-PRINT-OPTION = 'F' OR 'B'                              
01805        AND WS-FICHE-OPEN = ZERO                                   
01806          OPEN OUTPUT FICH                                         
01807          MOVE +1                 TO  WS-FICHE-OPEN.               
01808                                                                   
01809      IF WS-PRINT-OPTION = 'S' OR 'T'                              
01810          NEXT SENTENCE                                            
01811        ELSE                                                       
01812          GO TO 5099-EXIT.                                         
01813                                                                   
01814      MOVE ZERO                   TO  WS-LINE-NUMBER.              
01815                                                                   
01816      IF WS-ONLINE-REPORT-FILE-OPEN = ZERO                         
01817          OPEN I-O ELREPT                                          
01818          MOVE +1                 TO  WS-ONLINE-REPORT-FILE-OPEN   
01819          IF ELREPT-FILE-STATUS NOT = ZERO AND '97'                
01820              MOVE 'ELREPT'       TO  WS-FEM-FILE-NAME             
01821              MOVE WS-FILE-ERROR-MESSAGE  TO  WS-ABEND-MESSAGE     
01822              MOVE ELREPT-FILE-STATUS  TO  WS-ABEND-FILE-STATUS    
01823              GO TO ABEND-PGM.                                     
01824                                                                   
01825      MOVE ZERO                   TO  WS-START-SW.                 
01826      MOVE WS-LAST-COMPANY-CD     TO  RF-COMPANY-CD.               
01827      MOVE '1'                    TO  RF-RECORD-TYPE.              
01828      MOVE 'EL323'                TO  RF-REPORT-ID.                
01829      MOVE ZERO                   TO  RF-LINE-NUMBER.              
01830                                                                   
01831  5010-PRINT-REPORT.                                               
01832      START ELREPT                                                 
01833          KEY NOT LESS RF-CONTROL-PRIMARY                          
01834                                                                   
01835      IF ELREPT-FILE-STATUS = '10' OR '23'                         
01836          GO TO 5090-PRINT-REPORT.                                 
01837                                                                   
01838      IF ELREPT-FILE-STATUS NOT = ZERO                             
01839          MOVE 'ERROR OCCURED START ELREPT'  TO  WS-ABEND-MESSAGE  
01840          MOVE ELREPT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
01841          GO TO ABEND-PGM.                                         
01842                                                                   
01843  5020-PRINT-REPORT.                                               
01844      READ ELREPT NEXT.                                            
01845                                                                   
01846      IF ELREPT-FILE-STATUS = '10'                                 
01847          GO TO 5090-PRINT-REPORT.                                 
01848                                                                   
01849      IF ELREPT-FILE-STATUS NOT = ZERO                             
01850          MOVE 'ERROR OCCURED READNEXT ELREPT' TO WS-ABEND-MESSAGE 
01851          MOVE ELREPT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
01852          GO TO ABEND-PGM.                                         
01853                                                                   
01854      IF RF-COMPANY-CD NOT = WS-LAST-COMPANY-CD                    
01855          GO TO 5090-PRINT-REPORT.                                 
01856                                                                   
01857      IF WS-START-SW = ZERO                                        
01858          IF RF-RECORD-TYPE = '1'                                  
01859              NEXT SENTENCE                                        
01860            ELSE                                                   
01861              GO TO 5090-PRINT-REPORT                              
01862        ELSE                                                       
01863          IF RF-RECORD-TYPE = '2'                                  
01864              NEXT SENTENCE                                        
01865            ELSE                                                   
01866              GO TO 5090-PRINT-REPORT.                             
01867                                                                   
01868      IF RF-REPORT-ID NOT = 'EL323'                                
01869          GO TO 5090-PRINT-REPORT.                                 
01870                                                                   
01871      DELETE ELREPT RECORD.                                        
01872                                                                   
01873      IF ELREPT-FILE-STATUS NOT = ZERO                             
01874          MOVE 'ERROR OCCURED DELETE ELREPT' TO WS-ABEND-MESSAGE   
01875          MOVE ELREPT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
01876          GO TO ABEND-PGM.                                         
01877                                                                   
01878      GO TO 5020-PRINT-REPORT.                                     
01879                                                                   
01880  5090-PRINT-REPORT.                                               
01881      IF WS-START-SW = ZERO                                        
01882          MOVE WS-LAST-COMPANY-CD TO  RF-COMPANY-CD                
01883          MOVE '2'                TO  RF-RECORD-TYPE               
01884          MOVE 'EL323'            TO  RF-REPORT-ID                 
01885          MOVE ZERO               TO  RF-LINE-NUMBER               
01886          MOVE +1                 TO  WS-START-SW                  
01887          GO TO 5010-PRINT-REPORT.                                 
01888                                                                   
01889      MOVE WS-LAST-COMPANY-CD     TO  RF-COMPANY-CD.               
01890      MOVE '1'                    TO  RF-RECORD-TYPE.              
01891      MOVE 'EL323'                TO  RF-REPORT-ID.                
01892      MOVE ZERO                   TO  RF-LINE-NUMBER.              
01893                                                                   
01894      MOVE SPACES                 TO  RF-REPORT-LINE-133.          
01895                                                                   
01896  5099-EXIT.                                                       
01897      EXIT.                                                        
01898                                                                   
01899      EJECT                                                        
01900  8500-DATE-CONVERSION SECTION. COPY ELCDCS.                       
01901                                                                   
01902  WRITE-A-LINE SECTION. COPY ELCWAL.                               
01903                                                                   
01904  WRITE-HEADINGS SECTION.                                          
01905                                                                   
01906  WHS-010.                                                         
01907                                                                   
01908      ADD +1  TO  WS-PAGE.                                         
01909      MOVE WS-PAGE                TO  WS-H3-PAGE.                  
01910                                                                   
01911      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        
01912                                                                   
01913      IF EX-COMPANY-ID = 'DMD' AND                                 
01914         WS-H1-REPORT-TYPE = 'C' AND                               
01915         WS-USER-DATE-INPUT = 'Y'                                  
01916          MOVE WS-DMD-ALPHA       TO  WS-H3-DATE                   
01917          MOVE 'N'                TO  WS-USER-DATE-INPUT.          
01918                                                                   
01919      MOVE WS-HEADING1            TO  PRT.                         
01920      PERFORM WRITE-PRINTER.                                       
01921                                                                   
01922      MOVE WS-HEADING2            TO  PRT.                         
01923      PERFORM WRITE-PRINTER.                                       
01924                                                                   
01925      MOVE WS-HEADING3            TO  PRT.                         
01926      PERFORM WRITE-PRINTER.                                       
01927                                                                   
01928      IF WS-H1-REPORT-TYPE = 'A'                                   
01929          MOVE WS-EL323A-HEADING4 TO  PRT                          
01930          PERFORM WRITE-PRINTER                                    
01931          MOVE WS-EL323A-HEADING5 TO  PRT                          
01932          PERFORM WRITE-PRINTER                                    
01933          MOVE WS-EL323A-HEADING6 TO  PRT                          
01934          PERFORM WRITE-PRINTER                                    
01935        ELSE                                                       
01936      IF WS-H1-REPORT-TYPE = 'B'                                   
01937          MOVE WS-EL323B-HEADING4 TO  PRT                          
01938          PERFORM WRITE-PRINTER                                    
01939          MOVE WS-EL323B-HEADING5 TO  PRT                          
01940          PERFORM WRITE-PRINTER                                    
01941          MOVE WS-EL323B-HEADING6 TO  PRT                          
01942          PERFORM WRITE-PRINTER                                    
01943        ELSE                                                       
01944      IF WS-H1-REPORT-TYPE = 'C'                                   
01945          MOVE WS-EL323C-HEADING4 TO  PRT                          
01946          PERFORM WRITE-PRINTER                                    
01947          MOVE WS-EL323C-HEADING5 TO  PRT                          
01948          PERFORM WRITE-PRINTER                                    
01949          MOVE WS-EL323C-HEADING6 TO  PRT                          
01950          PERFORM WRITE-PRINTER                                    
01951        ELSE                                                       
01952      IF WS-H1-REPORT-TYPE = 'D'                                   
01953          MOVE WS-EL323D-HEADING4 TO  PRT                          
01954          PERFORM WRITE-PRINTER                                    
01955          MOVE WS-EL323D-HEADING5 TO  PRT                          
01956          PERFORM WRITE-PRINTER                                    
01957          MOVE WS-EL323D-HEADING6 TO  PRT                          
01958          PERFORM WRITE-PRINTER                                    
01959        ELSE                                                       
01960      IF WS-H1-REPORT-TYPE = 'E'                                   
01961          MOVE WS-EL323E-HEADING4 TO  PRT                          
01962          PERFORM WRITE-PRINTER                                    
01963          MOVE WS-EL323E-HEADING5 TO  PRT                          
01964          PERFORM WRITE-PRINTER                                    
01965          MOVE WS-EL323E-HEADING6 TO  PRT                          
01966          PERFORM WRITE-PRINTER                                    
01967        ELSE                                                       
01968      IF WS-H1-REPORT-TYPE = 'F'                                   
01969          MOVE WS-EL323F-HEADING4 TO  PRT                          
01970          PERFORM WRITE-PRINTER                                    
01971          MOVE WS-EL323F-HEADING5 TO  PRT                          
01972          PERFORM WRITE-PRINTER                                    
01973          MOVE WS-EL323F-HEADING6 TO  PRT                          
01974          PERFORM WRITE-PRINTER                                    
01975        ELSE                                                       
01976      IF WS-H1-REPORT-TYPE = 'G'                                   
021113         IF WS-HI-AUDITOR-RPT = 'A'
021113           MOVE WS-EL323G-HEADING4-AUTO TO PRT
021113         ELSE
021113           MOVE WS-EL323G-HEADING4 TO  PRT                          
021113         END-IF
01978          PERFORM WRITE-PRINTER                                    
01979          MOVE WS-EL323G-HEADING5 TO  PRT                          
01980          PERFORM WRITE-PRINTER                                    
01981          MOVE WS-EL323G-HEADING6 TO  PRT                          
01982          PERFORM WRITE-PRINTER                                    
01983        ELSE                                                       
01984      IF WS-H1-REPORT-TYPE = 'H'                                   
01985          MOVE WS-EL323H-HEADING4 TO  PRT                          
01986          PERFORM WRITE-PRINTER                                    
01987          MOVE WS-EL323H-HEADING5 TO  PRT                          
01988          PERFORM WRITE-PRINTER                                    
01989          MOVE WS-EL323H-HEADING6 TO  PRT                          
01990          PERFORM WRITE-PRINTER                                    
01991        ELSE                                                       
01992      IF WS-H1-REPORT-TYPE = 'I'                                   
01993          MOVE WS-EL323I-HEADING4 TO  PRT                          
01994          PERFORM WRITE-PRINTER                                    
01995          MOVE WS-EL323I-HEADING5 TO  PRT                          
01996          PERFORM WRITE-PRINTER                                    
01997          MOVE WS-EL323I-HEADING6 TO  PRT                          
01998          PERFORM WRITE-PRINTER                                    
01999        ELSE                                                       
02000      IF WS-H1-REPORT-TYPE = 'J'                                   
02001          MOVE WS-EL323J-HEADING4 TO  PRT                          
02002          PERFORM WRITE-PRINTER                                    
02003          MOVE WS-EL323J-HEADING5 TO  PRT                          
02004          PERFORM WRITE-PRINTER                                    
02005          MOVE WS-EL323J-HEADING6 TO  PRT                          
02006          PERFORM WRITE-PRINTER                                    
02007        ELSE                                                       
02008      IF WS-H1-REPORT-TYPE = 'K'                                   
02009          MOVE WS-EL323K-HEADING4 TO  PRT                          
02010          PERFORM WRITE-PRINTER                                    
02011          MOVE WS-EL323K-HEADING5 TO  PRT                          
02012          PERFORM WRITE-PRINTER                                    
02013          MOVE WS-EL323K-HEADING6 TO  PRT                          
02014          PERFORM WRITE-PRINTER                                    
02015          MOVE WS-EL323K-HEADING7 TO  PRT                          
02016          PERFORM WRITE-PRINTER                                    
02017        ELSE                                                       
02018      IF WS-H1-REPORT-TYPE = 'L'                                   
02019          MOVE WS-EL323L-HEADING4 TO  PRT                          
02020          PERFORM WRITE-PRINTER                                    
02021          MOVE WS-EL323L-HEADING5 TO  PRT                          
02022          PERFORM WRITE-PRINTER                                    
02023          MOVE WS-EL323L-HEADING6 TO  PRT                          
02024          PERFORM WRITE-PRINTER                                    
02025          MOVE WS-EL323L-HEADING7 TO  PRT                          
02026          PERFORM WRITE-PRINTER.                                   
02027                                                                   
02028      MOVE +12                    TO  WS-LINE-COUNT.               
02029                                                                   
02030  WHS-020.                                                         
02031      MOVE WS-SAVE-PRINT-RECORD   TO  PRT.                         
02032      MOVE '-'                    TO  P-CTL.                       
02033                                                                   
02034  WHS-EXIT.                                                        
02035      EXIT.                                                        
02036                                                                   
02037                                                                   
02038      EJECT                                                        
02039  WRITE-PRINTER SECTION.                                           
02040                                                                   
02041  WPS-010.                                                         
02042      IF WS-PRINT-OPTION = 'S' OR 'T'                              
02043          NEXT SENTENCE                                            
02044        ELSE                                                       
02045          GO TO WPS-020.                                           
02046                                                                   
02047      MOVE WS-LAST-COMPANY-CD     TO  RF-COMPANY-CD.               
02048      MOVE '1'                    TO  RF-RECORD-TYPE.              
02049      MOVE 'EL323'                TO  RF-REPORT-ID.                
02050      ADD +1  TO  WS-LINE-NUMBER.                                  
02051      MOVE WS-LINE-NUMBER         TO  RF-LINE-NUMBER.              
02052      MOVE PRT                    TO  RF-REPORT-LINE-133.          
02053                                                                   
02054      WRITE REPORT-SAVE-FILE.                                      
02055                                                                   
02056      IF ELREPT-FILE-STATUS NOT = ZERO                             
02057          MOVE 'ERROR OCCURED WRITE ELREPT'  TO  WS-ABEND-MESSAGE  
02058          MOVE ELREPT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
02059          GO TO ABEND-PGM.                                         
02060                                                                   
02061  WPS-020.                                                         
02062      IF WS-PRINT-OPTION = 'F' OR 'B'                              
02063          WRITE FICH-REC FROM PRT.                                 
02064                                                                   
02065      IF WS-PRINT-OPTION NOT = 'S' AND 'F'                         
02066          IF P-CTL = ' '                                           
02067              WRITE PRT AFTER ADVANCING 1 LINE                     
02068          ELSE                                                     
02069          IF P-CTL = '0'                                           
02070              WRITE PRT AFTER ADVANCING 2 LINES                    
02071          ELSE                                                     
02072          IF P-CTL = '-'                                           
02073              WRITE PRT AFTER ADVANCING 3 LINES                    
02074          ELSE                                                     
02075              WRITE PRT AFTER ADVANCING PAGE.                      
02076                                                                   
02077  WPS-EXIT.                                                        
02078      EXIT.                                                        
02079                                                                   
02080      EJECT                                                        
02081  OPEN-FILES SECTION.                                              
02082                                                                   
02083  OFS-010.                                                         
02084      OPEN INPUT REPORTS-EXTRACT-FILE                              
02085           OUTPUT PRNTR.                                           
02086                                                                   
02087  OFS-EXIT.                                                        
02088      EXIT.                                                        
02089                                                                   
02090      EJECT                                                        
02091  CLOSE-FILES SECTION.                                             
02092                                                                   
02093  CFS-010.                                                         
02094      CLOSE REPORTS-EXTRACT-FILE                                   
02095            PRNTR.                                                 
02096                                                                   
02097      IF WS-FICHE-OPEN NOT = ZERO                                  
02098          CLOSE FICH.                                              
02099                                                                   
02100      IF WS-ONLINE-REPORT-FILE-OPEN NOT = ZERO                     
02101          CLOSE ELREPT                                             
02102          IF ELREPT-FILE-STATUS NOT = ZERO                         
02103              MOVE 'ERROR OCCURED CLOSING -'                       
02104                                  TO  WS-FILE-ERROR-MESSAGE        
02105              MOVE 'ELREPT'       TO  WS-FEM-FILE-NAME             
02106              MOVE WS-FILE-ERROR-MESSAGE  TO  WS-ABEND-MESSAGE     
02107              MOVE ELREPT-FILE-STATUS  TO  WS-ABEND-FILE-STATUS    
02108              GO TO ABEND-PGM.                                     
02109                                                                   
02110  CFS-EXIT.                                                        
02111      EXIT.                                                        
02112                                                                   
02113  ABEND-PGM SECTION. COPY ELCABEND SUPPRESS.                       
02114                                                                   
